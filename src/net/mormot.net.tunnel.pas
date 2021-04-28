/// Asynchronous Network Port Forwarding / Tunnelling
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.tunnel;

{
  *****************************************************************************

   TCP/UDP Port Forwarding and Tunnelling
   - Abstract Definitions for Port Forwarding
   - Local NAT Client/Server to Tunnel TCP Streams

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.text,
  mormot.core.json,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.threads,
  mormot.core.log,
  mormot.core.interfaces,
  mormot.soa.core,
  mormot.soa.server,
  mormot.crypt.core,
  mormot.crypt.ecc256r1,
  mormot.net.sock,
  mormot.net.http,
  mormot.net.client,
  mormot.net.server;



{ ******************** Abstract Definitions for Port Forwarding }

type
  ETunnel = class(ESynException);

  /// options for TTunnelLocal process
  // - toEcdhe will compute an ephemeral secret to encrypt the link
  TTunnelOption = (
    toEcdhe);
  TTunnelOptions = set of TTunnelOption;
  PTunnelOptions = ^TTunnelOptions;

  /// abstract transmission layer with the central relay server
  // - may be implemented as raw sockets or over a mORMot SOA WebSockets link
  // - if toEcdhe option was set, the frames are encrypted
  ITunnelTransmit = interface(IInvokable)
    ['{F481A93C-1321-49A6-9801-CCCF065F3973}']
    /// should send Frame to the relay server
    // - no result so that the frames could be gathered on the wire
    procedure Send(const Frame: RawByteString);
  end;

  /// abstract tunneling service implementation
  ITunnelLocal = interface(IServiceWithCallbackReleased)
    ['{201150B4-6E28-47A3-AAE5-1335C82B060A}']
    /// this is the main method binding to a local ephemeral port and tunneling
    // to the Transmit interface
    function BindLocalPort(TransmitOptions: TTunnelOptions; TimeOut: integer;
      const Transmit: ITunnelTransmit): TNetPort;
  end;


  TTunnelLocal = class;

  /// background thread bound to a local port process
  TTunnelLocalThread = class(TSynThread)
  protected
    fState: (stCreated, stAccepting, stProcessing, stTerminated);
    fOwner: TTunnelLocal;
    fTransmit: ITunnelTransmit;
    fAes: array[{send:}boolean] of TAesAbstract;
    fServerSock, fClientSock: TNetSocket;
    fClientAddr: TNetAddr;
    fPort: TNetPort;
    /// accept a single incoming connection, then crypt/redirect to fTransmit
    procedure Execute; override;
  public
    /// initialize the thread - called from BindLocalPort()
    constructor Create(owner: TTunnelLocal; const transmit: ITunnelTransmit;
      const enc, dec: THash128; sock: TNetSocket); reintroduce;
    /// release all sockets and encryption state
    destructor Destroy; override;
    /// redirected from TTunnelLocal.Send
    procedure OnReceived(Frame: RawByteString);
  end;

  /// abstract tunneling service implementation
  // - also implement ITunnelTransmit so that could be used as callback
  // from the other side
  TTunnelLocal = class(TInterfacedObjectWithCustomCreate,
    ITunnelLocal, ITunnelTransmit)
  protected
    fOptions: TTunnelOptions;
    fPort: TNetPort;
    fThread: TTunnelLocalThread;
    fHandshake: TSynQueue;
  public
    /// called e.g. by CallbackReleased
    procedure ClosePort;
    /// finalize the server
    destructor Destroy; override;
  public
    // ITunnelTransmit methods
    /// called when a Frame is received from the relay server
    procedure Send(const Frame: RawByteString);
    // ITunnelLocal methods
    /// initialize a local forwarding port
    // - will call Transmit.Send for proper handshaking
    function BindLocalPort(TransmitOptions: TTunnelOptions; TimeOut: integer;
      const Transmit: ITunnelTransmit): TNetPort; virtual; abstract;
    /// called when a ITunnelTransmit callback is not used any more
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
  published
    property Port: TNetPort
      read fPort;
    property Options: TTunnelOptions
      read fOptions;
  end;



{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

type
  /// implement a server tunneling service
  TTunnelLocalServer = class(TTunnelLocal)
  public
    /// initialize a local forwarding server port
    function BindLocalPort(TransmitOptions: TTunnelOptions; TimeOut: integer;
      const Transmit: ITunnelTransmit): TNetPort; override;
  end;


type
  /// implement a client tunneling service
  TTunnelLocalClient = class(TTunnelLocal)
  public
    /// initialize a local forwarding client port
    function BindLocalPort(TransmitOptions: TTunnelOptions; TimeOut: integer;
      const Transmit: ITunnelTransmit): TNetPort; override;
  end;


implementation


{ ******************** Abstract Definitions for Port Forwarding }

{ TTunnelLocalThread }

constructor TTunnelLocalThread.Create(owner: TTunnelLocal;
  const transmit: ITunnelTransmit; const enc, dec: THash128; sock: TNetSocket);
begin
  fOwner := owner;
  fPort := owner.Port;
  fTransmit := transmit;
  if toEcdhe in owner.Options then
  begin
    // ecc256r1 secret has 128-bit resolution -> 128-bit AES
    fAes[{send:}false] := TAesFast[mCtr].Create(enc);
    fAes[{send:}true]  := TAesFast[mCtr].Create(dec);
  end;
  fServerSock := sock;
  FreeOnTerminate := true;
  inherited Create({suspended=}false);
end;

destructor TTunnelLocalThread.Destroy;
var
  callback: TNetSocket; // touch-and-go to the server to release main Accept()
begin
  fOwner.ClosePort; // also calls Terminate
  fServerSock.ShutdownAndClose({rdwr=}true);
  fServerSock := nil;
  fClientSock.ShutdownAndClose({rdwr=}true);
  fClientSock := nil;
  if fState = stAccepting then
    if NewSocket(cLocalhost, UInt32ToUtf8(fPort), nlTCP,
       {dobind=}false, 10, 10, 10, 0, callback) = nrOK then
      // Windows TCP/UDP socket may not release Accept() until connected
      callback.ShutdownAndClose({rdwr=}false);
  inherited Destroy;
  if fAes[false] <> fAes[true] then // CloneEncryptDecrypt may return self
    FreeAndNil(fAes[true]);
  FreeAndNil(fAes[false]);
end;

procedure TTunnelLocalThread.OnReceived(Frame: RawByteString);
var
  res: TNetResult;
begin
  if Terminated or
     (fTransmit = nil) or
     (Frame = '') or
     (fClientSock = nil) then
    exit;
  if fAes[{send:}false] <> nil then
  begin
    Frame := fAes[false].DecryptPkcs7(Frame, {ivatbeg=}true, {raise=}false);
    if Frame = '' then
    begin
      Terminate;
      raise ETunnel.CreateUtf8('%.OnReceived(%): invalid content', [self, fPort]);
    end;
  end;
  res := fClientSock.SendAll(pointer(Frame), length(Frame), @Terminated);
  if (res = nrOk) or
     Terminated then
    exit;
  Terminate;
  raise ETunnel.CreateUtf8('%.OnReceived(%): error % when retransmitting',
    [self, fPort, ToText(res)^]);
end;

procedure TTunnelLocalThread.Execute;
var
  tmp: RawByteString;
  res: TNetResult;
begin
  try
    fState := stAccepting;
    ENetSock.Check(fServerSock.Accept(fClientSock, fClientAddr),
      'TTunnelLocalThread.Execute');
    fState := stProcessing;
    while not Terminated do
    begin
      res := fClientSock.RecvWait(100, tmp, @Terminated);
      if res = nrRetry then
        continue;
      if res <> nrOK then
        raise ETunnel.CreateUtf8('%.Execute(%): error % at receiving',
          [self, fPort, ToText(res)^]);
      if (tmp <> '') and
         not Terminated then
      begin
        if fAes[{send:}true] <> nil then
          tmp := fAes[true].EncryptPkcs7(tmp, {ivatbeg=}true);
        fTransmit.Send(tmp);
      end;
    end;
    fOwner.ClosePort;
  except
    fOwner.ClosePort;
  end;
  fState := stTerminated;
end;


{ TTunnelLocal }

procedure TTunnelLocal.ClosePort;
begin
  if self = nil then
    exit;
  if fThread <> nil then
  begin
    fThread.fOwner := nil;
    fThread.Terminate;
  end;
  fPort := 0;
end;

destructor TTunnelLocal.Destroy;
begin
  if fThread <> nil then
  begin
    ClosePort;
    Sleep(200);
  end;
  inherited Destroy;
end;

procedure TTunnelLocal.Send(const Frame: RawByteString);
begin
  if fHandshake <> nil then
    fHandshake.Push(Frame)
  else if fThread <> nil then
    fThread.OnReceived(Frame)
  else
    raise ETunnel.CreateUtf8('%.Send: out of context call', [self]);
end;

procedure TTunnelLocal.CallbackReleased(const callback: IInvokable;
  const interfaceName: RawUtf8);
begin
  if IdemPropNameU(interfaceName, 'ITunnelTransmit') then
    ClosePort;
end;


{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

{ TTunnelLocalServer }

function TTunnelLocalServer.BindLocalPort(TransmitOptions: TTunnelOptions;
  TimeOut: integer; const Transmit: ITunnelTransmit): TNetPort;
var
  sock: TNetSocket;
  addr: TNetAddr;
  priv: TEccPrivateKey;
  frame, remote: RawByteString;
  rnd: PAesBlock;
  secret: TEccSecretKey;
  key: THash256Rec;
begin
  if (fPort <> 0) or
     not Assigned(Transmit) then
    raise ETunnel.CreateUtf8('%.BindLocalPort invalid call', [self]);
  // bind to a local ephemeral port
  ClosePort;
  ENetSock.Check(NewSocket(cLocalhost, {port=}'0', nlTCP, {bind=}true,
    TimeOut, TimeOut, TimeOut, {retry=}0, sock, @addr), 'BindLocalPort');
  result := addr.Port; // bind on port='0' = ephemeral port
  fOptions := TransmitOptions;
  fHandshake := TSynQueue.Create(TypeInfo(TRawByteStringDynArray));
  try
    // initial handshake: TTunnelOptions should match on both sides
    SetLength(frame, SizeOf(fOptions));
    PTunnelOptions(frame)^ := fOptions;
    Transmit.Send(frame);
    if Transmit = nil then
      raise ETunnel.CreateUtf8('%.BindLocalPort protocol issue', [self]);
    if not fHandshake.WaitPop(TimeOut, nil, remote) or
       (remote <> frame) then
      raise ETunnel.CreateUtf8('%.BindLocalPort protocol handshake', [self]);
    if toEcdhe in fOptions then
    try
      // EDCHE handshake with perfect forward security
      frame := TAesPrng.Main.FillRandom(SizeOf(TEccPublicKey) + SizeOf(TAesBlock));
      rnd := @PByteArray(frame)[SizeOf(TEccPublicKey)]; // frame=pub+rnd
      if not Ecc256r1MakeKey(PEccPublicKey(frame)^, priv) then
        raise ETunnel.CreateUtf8('%.BindLocalPort ECDHE init failure', [self]);
      Transmit.Send(frame);
      if not fHandshake.WaitPop(TimeOut, nil, remote) or
         (length(remote) <> SizeOf(TEccPublicKey)) or
         not Ecc256r1SharedSecret(PEccPublicKey(remote)^, priv, secret) then
        raise ETunnel.CreateUtf8('%.BindLocalPort ECDHE handshake', [self]);
      HmacSha256(rnd, @secret, SizeOf(rnd^), SizeOf(secret), key.b);
    finally
      FillZero(priv);
      FillZero(secret);
    end;
    // launch the background processing thread
    fThread := TTunnelLocalThread.Create(self, Transmit, key.Lo, key.hi, sock);
    FillZero(key.b);
  except
    sock.ShutdownAndClose(true);
  end;
  FreeAndNil(fHandshake);
end;


{ TTunnelLocalClient }

function TTunnelLocalClient.BindLocalPort(TransmitOptions: TTunnelOptions;
  TimeOut: integer; const Transmit: ITunnelTransmit): TNetPort;
var
  sock: TNetSocket;
  addr: TNetAddr;
  priv: TEccPrivateKey;
  frame, remote: RawByteString;
  rnd: PAesBlock;
  secret: TEccSecretKey;
  key: THash256Rec;
begin
  if (fPort <> 0) or
     not Assigned(Transmit) then
    raise ETunnel.CreateUtf8('%.BindLocalPort invalid call', [self]);
  ClosePort;
  // bind to a local ephemeral port
  ENetSock.Check(NewSocket(cLocalhost, {port=}'0', nlTCP, {bind=}true,
    TimeOut, TimeOut, TimeOut, {retry=}0, sock, @addr), 'BindLocalPort');
  result := addr.Port; // bind on port='0' = ephemeral port
  fOptions := TransmitOptions;
  fHandshake := TSynQueue.Create(TypeInfo(TRawByteStringDynArray));
  try
    // initial handshake: TTunnelOptions should match on both sides
    SetLength(frame, SizeOf(fOptions));
    PTunnelOptions(frame)^ := fOptions;
    if not fHandshake.WaitPop(TimeOut, nil, remote) or
       (remote <> frame) then
      raise ETunnel.CreateUtf8('%.BindLocalPort protocol handshake', [self]);
    Transmit.Send(frame);
    if toEcdhe in fOptions then
    try
      // EDCHE handshake with perfect forward security
      SetString(frame, nil, SizeOf(TEccPublicKey));
      if not fHandshake.WaitPop(TimeOut, nil, remote) or // remote=pub+rnd
         (length(remote) <> SizeOf(TEccPublicKey) + SizeOf(TAesBlock)) or
         not Ecc256r1MakeKey(PEccPublicKey(frame)^, priv) then
        raise ETunnel.CreateUtf8('%.BindLocalPort ECDHE handshake', [self]);
      Transmit.Send(frame);
      if not Ecc256r1SharedSecret(PEccPublicKey(remote)^, priv, secret) then
        raise ETunnel.CreateUtf8('%.BindLocalPort ECDHE init failure', [self]);
      rnd := @PByteArray(remote)[SizeOf(TEccPublicKey)];
      HmacSha256(rnd, @secret, SizeOf(rnd^), SizeOf(secret), key.b);
    finally
      FillZero(priv);
      FillZero(secret);
    end;
    // launch the background processing thread
    fThread := TTunnelLocalThread.Create(self, Transmit, key.Hi, key.Lo, sock);
    FillZero(key.b);
  except
    sock.ShutdownAndClose(true);
  end;
  FreeAndNil(fHandshake);
end;



end.

