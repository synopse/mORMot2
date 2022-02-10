/// Network Port Forwarding / Tunnelling
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.tunnel;

{
  *****************************************************************************

   TCP/UDP Port Forwarding and Tunnelling
   - Abstract Definitions for Port Forwarding
   - Local NAT Client/Server to Tunnel TCP Streams
   - WebSockets stand-alone Relay Server

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
  mormot.core.unicode,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.log,
  mormot.soa.core,
  mormot.soa.server,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.ecc256r1, // for ECDHE encryption
  mormot.net.sock,
  mormot.net.ws.core,
  mormot.net.ws.client,
  mormot.net.ws.server;


{ ******************** Abstract Definitions for Port Forwarding }

type
  ETunnel = class(ESynException);

  /// each option available for TTunnelLocal process
  // - toEcdhe will compute an ephemeral secret to encrypt the link
  TTunnelOption = (
    toEcdhe);

  /// options for TTunnelLocal process
  TTunnelOptions = set of TTunnelOption;
  PTunnelOptions = ^TTunnelOptions;

  /// an opaque value matching e.g. a TBinaryCookieGeneratorSessionID
  TTunnelSession = Int64;
  PTunnelSession = ^TTunnelSession;

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
    // - returns the port number to connect to, over the 127.0.0.1 loopback
    function BindLocalPort(Session: TTunnelSession; TransmitOptions: TTunnelOptions;
      TimeOutMS: integer; const Transmit: ITunnelTransmit): TNetPort;
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

  TTunnelLocalHeader = packed record
    options: TTunnelOptions;
    session: TTunnelSession;
  end;
  PTunnelLocalHeader = ^TTunnelLocalHeader;

  TTunnelEcdheContext = packed record
    rnd: TAesBlock;
    pub: TEccPublicKey;
    priv: TEccPrivateKey;
  end;
  PTunnelEcdheContext = ^TTunnelEcdheContext;

  /// abstract tunneling service implementation
  // - also implement ITunnelTransmit so that could be used as callback
  // from the other side
  // - if you release this instance, the tunnel will end
  TTunnelLocal = class(TInterfacedObjectWithCustomCreate,
    ITunnelLocal, ITunnelTransmit)
  protected
    fOptions: TTunnelOptions;
    fPort: TNetPort;
    fThread: TTunnelLocalThread;
    fHandshake: TSynQueue;
    fContext: TTunnelEcdheContext;
    function EcdheHandshake(TimeOutMS: integer; const Transmit: ITunnelTransmit;
      out ecdhe: TTunnelEcdheContext): boolean; virtual; abstract;
  public
    /// called e.g. by CallbackReleased
    procedure ClosePort;
    /// initialize the class, especially the Context values
    constructor Create; override;
    /// finalize the server
    destructor Destroy; override;
    /// Create will initialize this with some random values
    // - is publicated here to allow putting the public key into some message
    // before calling BindLocalPort()
    property Context: TTunnelEcdheContext
      read fContext;
  public
    /// ITunnelTransmit method: when a Frame is received from the relay server
    procedure Send(const Frame: RawByteString);
    /// ITunnelLocal method: initialize a local forwarding port
    // - returns 0 on failure, an ephemeral port on 127.0.0.1 on success
    function BindLocalPort(Session: TTunnelSession; TransmitOptions: TTunnelOptions;
      TimeOutMS: integer; const Transmit: ITunnelTransmit): TNetPort; virtual;
    /// ITunnelLocal method: when a ITunnelTransmit remote callback is finished
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
  protected
    /// initialize a local forwarding server port
    function EcdheHandshake(TimeOutMS: integer; const Transmit: ITunnelTransmit;
      out ecdhe: TTunnelEcdheContext): boolean; override;
  end;


type
  /// implement a client tunneling service
  TTunnelLocalClient = class(TTunnelLocal)
  protected
    /// initialize a local forwarding client port
    function EcdheHandshake(TimeOutMS: integer; const Transmit: ITunnelTransmit;
      out ecdhe: TTunnelEcdheContext): boolean; override;
  end;


{ ******************** WebSockets stand-alone Relay Server }

type
  TTunnelRelayIDs = array of TBinaryCookieGeneratorSessionID;

  TTunnelRelayLink = record
    // order doesn't matter -> just a link between two clients
    ProcessA, ProcessB: TWebSocketProcess;
  end;
  TTunnelRelayLinks = array of TTunnelRelayLink;

  TTunnelRelayServer = class(TWebSocketServer)
  protected
    fMainProtocol: TWebSocketProtocolUri;
    fLinks: TSynDictionary; // TTunnelRelayIDs/TTunnelRelayLinks
  public
    /// initialize the Relay Server
    // - if publicUri is not set, '127.0.0.1:localPort' is used, but you can
    // use a reverse proxy URI like 'publicdomain.com/websockgateway'
    constructor Create(const localPort: RawUtf8; const publicUri: RawUtf8 = '';
      expirationMinutes: integer = 1); reintroduce;
    /// finalize this Relay Server
    destructor Destroy; override;
    /// generate a new WebSockets connection URI and its associated session ID
    // - will be valid for expirationMinutes time as specified to Create()
    function NewUri(out SessionID: TTunnelSession): RawUtf8;
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
  Terminate;
  if fOwner <> nil then
    fOwner.ClosePort;
  fServerSock.ShutdownAndClose({rdwr=}true);
  fServerSock := nil;
  fClientSock.ShutdownAndClose({rdwr=}true);
  fClientSock := nil;
  if fState = stAccepting then
    if NewSocket(cLocalhost, UInt32ToUtf8(fPort), nlTcp,
       {dobind=}false, 10, 0, 0, 0, callback) = nrOK then
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
    ENetSock.Check(fServerSock.Accept(fClientSock, fClientAddr, {async=}false),
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
  except
    fOwner.ClosePort;
  end;
  fState := stTerminated;
end;


{ TTunnelLocal }

procedure TTunnelLocal.ClosePort;
var
  thread: TTunnelLocalThread;
begin
  if self = nil then
    exit;
  thread := fThread;
  if thread <> nil then
  begin
    fThread := nil;
    thread.fOwner := nil;
    thread.Terminate;
  end;
  fPort := 0;
end;

constructor TTunnelLocal.Create;
begin
  inherited Create;
  TAesPrng.Main.FillRandom(fContext.rnd);
  if not Ecc256r1MakeKey(fContext.pub, fContext.priv) then
    raise ETunnel.CreateUtf8('%.Create: no ECC engine available', [self]);
end;

destructor TTunnelLocal.Destroy;
begin
  if fThread <> nil then
    ClosePort; // calls Terminate
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

{ TTunnelLocal }

function TTunnelLocal.BindLocalPort(Session: TTunnelSession;
  TransmitOptions: TTunnelOptions; TimeOutMS: integer;
  const Transmit: ITunnelTransmit): TNetPort;
var
  sock: TNetSocket;
  addr: TNetAddr;
  frame, remote: RawByteString;
  header: TTunnelLocalHeader;
  secret: TEccSecretKey;
  ecdhe: TTunnelEcdheContext;
  key: THash256Rec;
begin
  if (fPort <> 0) or
     (not Assigned(Transmit)) then
    raise ETunnel.CreateUtf8('%.BindLocalPort invalid call', [self]);
  // bind to a local ephemeral port
  ClosePort;
  ENetSock.Check(NewSocket(cLocalhost, {port=}'0', nlTcp, {bind=}true,
    TimeOutMS, TimeOutMS, TimeOutMS, {retry=}0, sock, @addr), 'BindLocalPort');
  result := addr.Port; // bind on port='0' = ephemeral port
  fOptions := TransmitOptions;
  fHandshake := TSynQueue.Create(TypeInfo(TRawByteStringDynArray));
  try
    // initial handshake: TTunnelOptions+TTunnelSession
    header.options := fOptions;
    header.session := Session;
    FastSetRawByteString(frame, @header, SizeOf(header));
    Transmit.Send(frame);
    // server will wait until both sides are connected
    if not fHandshake.WaitPop(TimeOutMS, nil, remote) or
       (remote <> frame) then
      raise ETunnel.CreateUtf8('%.BindLocalPort handshake failed', [self]);
    // optional ECDHE ephemeral encryption
    if toEcdhe in fOptions then
    begin
      if not EcdheHandshake(TimeOutMS, Transmit, ecdhe) or
         not Ecc256r1SharedSecret(ecdhe.pub, ecdhe.priv, secret) then
        raise ETunnel.CreateUtf8('%.BindLocalPort ECDHE handshake', [self]);
      HmacSha256(@ecdhe.rnd, @secret, SizeOf(ecdhe.rnd), SizeOf(secret), key.b);
    end;
    // launch the background processing thread
    fThread := TTunnelLocalThread.Create(self, Transmit, key.Lo, key.hi, sock);
  except
    sock.ShutdownAndClose(true); // any error would abort and return 0
    result := 0;
  end;
  FreeAndNil(fHandshake);
  FillZero(ecdhe.priv);
  FillZero(secret);
  FillZero(key.b);
end;


{ TTunnelLocalServer }

function TTunnelLocalServer.EcdheHandshake(TimeOutMS: integer;
  const Transmit: ITunnelTransmit; out ecdhe: TTunnelEcdheContext): boolean;
var
  frame, remote: RawByteString;
begin
  result := false;
  // EDCHE handshake with perfect forward security - server side
  ecdhe := fContext; // pre-computed by overriden Create
  FastSetRawByteString(frame, @ecdhe, SizeOf(ecdhe.rnd) + SizeOf(ecdhe.pub));
  Transmit.Send(frame); // frame = rnd+pub
  if not fHandshake.WaitPop(TimeOutMS, nil, remote) or
     (length({%H-}remote) <> SizeOf(TEccPublicKey)) then
    exit;
  ecdhe.pub := PEccPublicKey(remote)^; // remote = pub
  result := true;
end;


{ TTunnelLocalClient }

function TTunnelLocalClient.EcdheHandshake(TimeOutMS: integer;
  const Transmit: ITunnelTransmit; out ecdhe: TTunnelEcdheContext): boolean;
var
  frame, remote: RawByteString;
begin
  result := false;
  // EDCHE handshake with perfect forward security - client side
  if not fHandshake.WaitPop(TimeOutMS, nil, remote) or // remote = rnd+pub
     (length({%H-}remote) <> SizeOf(ecdhe.rnd) + SizeOf(ecdhe.pub)) then
    exit;
  FastSetRawByteString(frame, nil, SizeOf(TEccPublicKey));
  PEccPublicKey(frame)^ := fContext.pub;
  ecdhe.priv := fContext.priv;
  Transmit.Send(frame); // frame = pub
  with PTunnelEcdheContext(remote)^ do
  begin
    ecdhe.rnd := rnd;
    ecdhe.pub := pub;
  end;
  result := true;
end;


{ ******************** WebSockets stand-alone Relay Server }

{ TTunnelRelayServerProtocol }

type
  TTunnelRelayServerProtocol = class(TWebSocketProtocolUri)
  protected
    fReverse: TWebSocketProcess;
    fOptions: TTunnelOptions;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var request: TWebSocketFrame; const info: RawUtf8); override;
  end;

procedure AsynchSend(process: TWebSocketProcess; const msg: RawByteString);
var
  frame: TWebSocketFrame;
begin
  frame.opcode := focBinary;
  frame.content := [fopAlreadyCompressed]; // it is probably encrypted
  frame.tix := 0;
  frame.payload := msg;
  process.Outgoing.Push(frame, 0);
end;

procedure TTunnelRelayServerProtocol.ProcessIncomingFrame(
  Sender: TWebSocketProcess; var request: TWebSocketFrame; const info: RawUtf8);
var
  added: boolean;
  link: ^TTunnelRelayLink;
  connections: TSynDictionary;
  session: TTunnelSession;
  reversed: TTunnelRelayServerProtocol;
  head: PTunnelLocalHeader;
begin
  if (Sender <> nil) and
     (Sender.Protocol <> nil) and
     (Sender.Protocol.ClassType = TTunnelRelayServerProtocol) then
  case request.opcode of
    // focBinary or focContinuation/focConnectionClose
    focBinary:
      if fReverse = nil then
      begin
        // initialization of this connection
        session := TTunnelRelayServerProtocol(Sender.Protocol).Session;
        head := pointer(request.payload);
        if (length(request.payload) <> SizeOf(head^)) or
           (head^.session <> session) then
          raise ETunnel.CreateUtf8('%.ProcessIncomingFrame: bad handshake %.%.%',
            [self, length(request.payload), head^.session, session]);
        fOptions := head^.options;
        connections := (((Sender as TWebSocketProcessServer).
                          Socket as TWebSocketServerSocket).
                          Server as TTunnelRelayServer).fLinks;
        connections.Safe.Lock;
        try
          link := connections.FindValueOrAdd(session, added);
          if link^.ProcessA = nil then
            // first end connected for this Session
            link^.ProcessA := Sender
          else
          begin
            // ensure only one link with matching options per URI/session
            reversed := link^.ProcessA.Protocol as TTunnelRelayServerProtocol;
            if (link^.ProcessB <> nil) or
               (reversed.fOptions <> fOptions) then
              raise ETunnel.CreateUtf8('%.ProcessIncomingFrame: abusive', [self]);
            // now both sides are properly connected
            link^.ProcessB := Sender;
            fReverse := link^.ProcessA;
            reversed.fReverse := Sender;
            // unblock both ends to begin normal relay
            AsynchSend(fReverse, request.PayLoad);
            AsynchSend(Sender, request.PayLoad);
            // no connections.DeleteAt(ndx) to ensure any other link creation
            connections.DeleteDeprecated;
          end;
        finally
          connections.Safe.UnLock;
        end;
      end
      else
        // normal process: asynch relaying to the other side
        AsynchSend(fReverse, request.payload);
    focConnectionClose:
       if fReverse <> nil then
         fReverse.Shutdown({waitforpong=}true);
  end;
end;


{ TTunnelRelayServer }

constructor TTunnelRelayServer.Create(const localPort, publicUri: RawUtf8;
  expirationMinutes: integer);
var
  uri: RawUtf8;
begin
  fLinks := TSynDictionary.Create(TypeInfo(TTunnelRelayIDs),
    TypeInfo(TTunnelRelayLinks), false, {timeout=} expirationMinutes * 60);
  inherited Create(localPort, nil, nil, 'relaysrv');
  if publicUri = '' then
    uri := '127.0.0.1:' + localPort
  else
    uri := publicUri;
  fMainProtocol := TTunnelRelayServerProtocol.Create(
    'mrmtproxy', uri, expirationMinutes, nil);
  fProtocols.Add(fMainProtocol); // will be cloned for each URI
end;

destructor TTunnelRelayServer.Destroy;
begin
  inherited Destroy;
  fLinks.Free;
end;

function TTunnelRelayServer.NewUri(out SessionID: TTunnelSession): RawUtf8;
var
  session: TBinaryCookieGeneratorSessionID;
begin
  result := fMainProtocol.NewUri(session);
  SessionID := session;
end;




end.

