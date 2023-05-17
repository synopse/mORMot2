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
  // - toClientSigned/toServerSigned will be set by BindLocalPort according to
  // the actual certificates available, to ensure an authenticated handshake
  TTunnelOption = (
    toEcdhe,
    toClientSigned,
    toServerSigned);

  /// options for TTunnelLocal process
  TTunnelOptions = set of TTunnelOption;
  PTunnelOptions = ^TTunnelOptions;

  /// a session identifier which should match on both sides of the tunnel
  // - typically a Random64 or a TBinaryCookieGeneratorSessionID value
  TTunnelSession = Int64;
  PTunnelSession = ^TTunnelSession;

  /// abstract transmission layer with the central relay server
  // - may be implemented as raw sockets or over a mORMot SOA WebSockets link
  // - if toEcdhe option was set, the frames are already encrypted
  ITunnelTransmit = interface(IInvokable)
    ['{F481A93C-1321-49A6-9801-CCCF065F3973}']
    /// should send Frame to the relay server
    // - no result so that the frames could be gathered e.g. over WebSockets
    procedure Send(const Frame: RawByteString);
  end;

  /// abstract tunneling service implementation
  ITunnelLocal = interface(IServiceWithCallbackReleased)
    ['{201150B4-6E28-47A3-AAE5-1335C82B060A}']
    /// to be called before BindLocalPort() for proper handshake process
    procedure SetTransmit(const Transmit: ITunnelTransmit);
    /// this is the main method binding to a local ephemeral port and tunneling
    // to the Transmit interface
    // - TransmitOptions should match on both sides
    // - returns the port number to connect to, over the 127.0.0.1 loopback
    function BindLocalPort(Session: TTunnelSession; TransmitOptions: TTunnelOptions;
      TimeOutMS: integer; const AppSecret: RawByteString = ''): TNetPort;
    /// the bound 127.0.0.1:LocalPort to be used for the tunnel local process
    function LocalPort: RawUtf8;
    /// check if the background processing thread is using encrypted frames
    function Encrypted: boolean;
  end;

  TTunnelLocal = class;

  /// background thread bound to a local port process
  TTunnelLocalThread = class(TSynThread)
  protected
    fState: (stCreated, stAccepting, stProcessing, stTerminated);
    fStarted: boolean;
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
      const key, iv: THash128; sock: TNetSocket); reintroduce;
    /// release all sockets and encryption state
    destructor Destroy; override;
    /// redirected from TTunnelLocal.Send
    procedure OnReceived(const Frame: RawByteString);
  end;

  /// identify the TTunnelLocal handshake protocol version
  TTunnelLocalMagic = (
    tlmNone,
    tlmVersion1);

  /// define the inital frame exchanged between both TTunnelLocal ends
  TTunnelLocalHeader = packed record
    /// protocol version
    magic: TTunnelLocalMagic;
    /// define how this link should be setup
    options: TTunnelOptions;
    /// a genuine integer ID
    session: TTunnelSession;
    /// SHA3 checksum/padding of the previous fields with app specific salt
    crc: THash128;
  end;
  PTunnelLocalHeader = ^TTunnelLocalHeader;

  /// store the cryptographic context for TTunnelLocal optional ECDHE handshake
  // - all fields are computed at startup by the TTunnelLocal.Create constructor
  // - two first fields are rnd + pub so that they will be sent over the wire
  TTunnelEcdheContext = packed record
    /// 128-bit random salt to ensure ECDHE features perfect forward security
    rnd: TAesBlock;
    /// the ECC secp256r1 public key, as transmitted to the other end
    pub: TEccPublicKey;
    /// the ECC secp256r1 private key, kept in local memory for safety
    priv: TEccPrivateKey;
  end;
  PTunnelEcdheContext = ^TTunnelEcdheContext;

  /// abstract tunneling service implementation
  // - is properly implemented by TTunnelLocalServer/TTunnelLocalClient classes
  // - published ITunnelTransmit so that could be used as receival callback
  // - if you release this instance, the tunnel will end
  TTunnelLocal = class(TInterfacedObjectWithCustomCreate,
    ITunnelLocal, ITunnelTransmit)
  protected
    fOptions: TTunnelOptions;
    fPort: TNetPort;
    fThread: TTunnelLocalThread;
    fHandshake: TSynQueue;
    fContext: TTunnelEcdheContext;
    fTransmit: ITunnelTransmit;
    fSignCert, fVerifyCert: ICryptCert;
    // two methods to be overriden according to the client/server side
    function ComputeOptionsFromCert: TTunnelOptions; virtual; abstract;
    function EcdheHandshake(TimeOutMS: integer; const Transmit: ITunnelTransmit;
      out ecdhe: TTunnelEcdheContext): boolean; virtual; abstract;
    // can optionally add a signature to the main handshake frame
    procedure FrameSign(var frame: RawByteString); virtual;
    function FrameVerify(const frame: RawByteString;
      payloadlen: PtrInt): boolean; virtual;
  public
    /// initialize the instance for process
    // - if no Context value is supplied, will compute an ephemeral key pair
    constructor Create(SpecificContext: PTunnelEcdheContext = nil); reintroduce;
    /// finalize the server
    destructor Destroy; override;
    /// called e.g. by CallbackReleased
    procedure ClosePort;
    /// Create will initialize this with some random or specific values
    property Context: TTunnelEcdheContext
      read fContext;
  public
    /// ITunnelTransmit method: when a Frame is received from the relay server
    procedure Send(const Frame: RawByteString);
    /// ITunnelLocal method: to be called before BindLocalPort()
    procedure SetTransmit(const Transmit: ITunnelTransmit);
    /// ITunnelLocal method: initialize a local forwarding port
    // - TransmitOptions will be amended to follow SignCert/VerifyCert properties
    // - returns 0 on failure, or an ephemeral port on 127.0.0.1 on success
    function BindLocalPort(Session: TTunnelSession; TransmitOptions: TTunnelOptions;
      TimeOutMS: integer; const AppSecret: RawByteString = ''): TNetPort; virtual;
    /// ITunnelLocal method: return the localport needed
    function LocalPort: RawUtf8;
    /// ITunnelLocal method: check if the background thread uses encrypted frames
    function Encrypted: boolean;
    /// ITunnelLocal method: when a ITunnelTransmit remote callback is finished
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
    /// optional Certificate with private key to sign the output handshake frame
    // - certificate should have [cuDigitalSignature] usage
    // - should match other side's VerifyCert public key property
    property SignCert: ICryptCert
      read fSignCert write fSignCert;
    /// optional Certificate with public key to verify the input handshake frame
    // - certificate should have [cuDigitalSignature] usage
    // - should match other side's SignCert private key property
    property VerifyCert: ICryptCert
      read fVerifyCert write fVerifyCert;
  published
    /// the ephemeral port on the loopback as returned by BindLocalPort()
    property Port: TNetPort
      read fPort;
    /// the connection specifications, as used by BindLocalPort()
    property Options: TTunnelOptions
      read fOptions;
  end;



{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

type
  /// implements server-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel
  TTunnelLocalServer = class(TTunnelLocal)
  protected
    /// returns toClientSigned/toServerSigned from SignCert/VerifyCert
    function ComputeOptionsFromCert: TTunnelOptions; override;
    /// initialize a local forwarding server port
    // - could be overriden to ensure ecdhe.pub matches the expected value
    function EcdheHandshake(TimeOutMS: integer; const Transmit: ITunnelTransmit;
      out ecdhe: TTunnelEcdheContext): boolean; override;
  end;


type
  /// implements client-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel
  TTunnelLocalClient = class(TTunnelLocal)
  protected
    /// returns toClientSigned/toServerSigned from SignCert/VerifyCert
    function ComputeOptionsFromCert: TTunnelOptions; override;
    /// initialize a local forwarding client port
    // - could be overriden to ensure ecdhe.pub matches the expected value
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
  const transmit: ITunnelTransmit; const key, iv: THash128; sock: TNetSocket);
begin
  fOwner := owner;
  fPort := owner.Port;
  fTransmit := transmit;
  if toEcdhe in owner.Options then
  begin
    // ecc256r1 shared secret has 128-bit resolution -> 128-bit AES-CTR
    fAes[{send:}false] := AesIvUpdatedCreate(mCtr, key, 128);
    fAes[{send:}true]  := AesIvUpdatedCreate(mCtr, key, 128);
    // we don't send an IV with each frame, but update it from ecdhe derivation
    fAes[false].IV := iv;
    fAes[true].IV := iv;
  end;
  fServerSock := sock;
  FreeOnTerminate := true;
  inherited Create({suspended=}false);
end;

destructor TTunnelLocalThread.Destroy;
begin
  Terminate;
  if fOwner <> nil then
    fOwner.ClosePort;
  fServerSock.ShutdownAndClose({rdwr=}true);
  fClientSock.ShutdownAndClose({rdwr=}true);
  inherited Destroy;
  FreeAndNil(fAes[true]);
  FreeAndNil(fAes[false]);
end;

procedure TTunnelLocalThread.OnReceived(const Frame: RawByteString);
var
  res: TNetResult;
  data: RawByteString;
begin
  // validate and optionally decrypt the input frame
  if Terminated or
     (fTransmit = nil) or
     (Frame = '') then
    exit;
  if fClientSock = nil then // may occur with direct calls
  begin
    SleepHiRes(10); // let the socket be accepted()
    if Terminated or
       (fClientSock = nil) then
      exit;
  end;
  if fAes[{send:}false] = nil then
    data := Frame
  else
  begin
    data := fAes[false].DecryptPkcs7(Frame, {ivatbeg=}false, {raise=}false);
    if data = '' then
    begin
      Terminate;
      raise ETunnel.CreateUtf8('%.OnReceived(%): decrypt error', [self, fPort]);
    end;
  end;
  // relay the (decrypted) data to the local loopback
  if Terminated then
    exit;
  res := fClientSock.SendAll(pointer(data), length(data), @Terminated);
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
  fStarted := true;
  try
    fState := stAccepting;
    res := fServerSock.Accept(fClientSock, fClientAddr, {async=}false);
    if (res = nrOk) and
       not Terminated then
    begin
      fState := stProcessing;
      while not Terminated do
      begin
        // wait for some data on the local loopback
        res := fClientSock.RecvWait(100, tmp, @Terminated);
        case res of
          nrRetry:
            continue;
          nrClosed:
            break;
          nrOK:
            if (tmp <> '') and
               not Terminated then
            begin
              // send the data (optionally encrypted) to the other side
              if fAes[{send:}true] <> nil then
                tmp := fAes[true].EncryptPkcs7(tmp, {ivatbeg=}false);
              fTransmit.Send(tmp);
            end;
        else
          raise ETunnel.CreateUtf8('%.Execute(%): error % at receiving',
            [self, fPort, ToText(res)^]);
        end;
      end;
    end;
  except
    fOwner.ClosePort;
  end;
  fState := stTerminated;
end;


{ TTunnelLocal }

constructor TTunnelLocal.Create(SpecificContext: PTunnelEcdheContext);
begin
  inherited Create;
  if SpecificContext <> nil then
  begin
    fContext := SpecificContext^;
    if IsZero(fContext.pub) or
       IsZero(fContext.priv) then
      raise ETunnel.CreateUtf8('%.Create: void supplied ECC key', [self]);
  end
  else if not Ecc256r1MakeKey(fContext.pub, fContext.priv) then
    raise ETunnel.CreateUtf8('%.Create: no ECC engine available', [self]);
  TAesPrng.Main.FillRandom(fContext.rnd); // overwrite any previous random
  fHandshake := TSynQueue.Create(TypeInfo(TRawByteStringDynArray));
end;

destructor TTunnelLocal.Destroy;
begin
  if fThread <> nil then
    ClosePort; // calls Terminate
  inherited Destroy;
  FreeAndNil(fHandshake);
end;

procedure TTunnelLocal.ClosePort;
var
  thread: TTunnelLocalThread;
  callback: TNetSocket; // touch-and-go to the server to release main Accept()
begin
  if self = nil then
    exit;
  thread := fThread;
  if thread <> nil then
  begin
    fThread := nil;
    thread.fOwner := nil;
    thread.Terminate;
    if thread.fState = stAccepting then
      if NewSocket(cLocalhost, UInt32ToUtf8(fPort), nlTcp,
         {dobind=}false, 10, 0, 0, 0, callback) = nrOK then
        // Windows socket may not release Accept() until connected
        callback.ShutdownAndClose({rdwr=}false);
  end;
  fPort := 0;
end;

procedure TTunnelLocal.Send(const Frame: RawByteString);
begin
  // ITunnelTransmit method: when a Frame is received from the relay server
  if fHandshake <> nil then
    fHandshake.Push(Frame)    // during the handshake phase
  else if fThread <> nil then
    fThread.OnReceived(Frame); // regular tunelling process
end;

procedure TTunnelLocal.CallbackReleased(const callback: IInvokable;
  const interfaceName: RawUtf8);
begin
  if PropNameEquals(interfaceName, 'ITunnelTransmit') then
    ClosePort;
end;

procedure TTunnelLocal.FrameSign(var frame: RawByteString);
begin
  if fSignCert <> nil then
    Append(frame, fSignCert.Sign(frame));
end;

function TTunnelLocal.FrameVerify(const frame: RawByteString;
  payloadlen: PtrInt): boolean;
var
  f: PAnsiChar absolute frame;
begin
  result := (length(Frame) >= payloadlen) and
            ((fVerifyCert = nil) or
             (fVerifyCert.Verify(f + payloadlen, f,
               length(frame) - payloadlen, payloadlen) in CV_VALIDSIGN));
end;

procedure TTunnelLocal.SetTransmit(const Transmit: ITunnelTransmit);
begin
  fTransmit := Transmit;
  if fThread <> nil then
    fThread.fTransmit := Transmit;
end;

const
  HEADER_DEFAULT_SALT: TGuid = '{AB15C52F-754F-49CB-9B23-CF88735E39C8}';

function TTunnelLocal.BindLocalPort(Session: TTunnelSession;
  TransmitOptions: TTunnelOptions; TimeOutMS: integer;
  const AppSecret: RawByteString): TNetPort;
var
  sock: TNetSocket;
  addr: TNetAddr;
  frame, remote: RawByteString;
  header: TTunnelLocalHeader;
  secret: TEccSecretKey;
  ecdhe: TTunnelEcdheContext;
  key: THash256Rec;
  sha3: TSha3;
begin
  // validate input parameters
  if (fPort <> 0) or
     (not Assigned(fTransmit)) then
    raise ETunnel.CreateUtf8('%.BindLocalPort invalid call', [self]);
  TransmitOptions := TransmitOptions - [toClientSigned, toServerSigned];
  TransmitOptions := TransmitOptions + ComputeOptionsFromCert;
  // bind to a local ephemeral port
  ClosePort;
  ENetSock.Check(NewSocket(cLocalhost, {port=}'0', nlTcp, {bind=}true,
    TimeOutMS, TimeOutMS, TimeOutMS, {retry=}0, sock, @addr), 'BindLocalPort');
  result := addr.Port; // bind on port='0' = ephemeral port
  fOptions := TransmitOptions;
  try
    // initial handshake: same TTunnelOptions + TTunnelSession from both sides
    header.magic := tlmVersion1;
    header.options := fOptions;
    header.session := Session;
    sha3.Init(SHA3_224);
    if AppSecret = '' then
      sha3.Update(@HEADER_DEFAULT_SALT, SizeOf(HEADER_DEFAULT_SALT))
    else
      sha3.Update(AppSecret); // custom symmetric application-specific secret
    sha3.Update(@header, SizeOf(header) - SizeOf(header.crc));
    sha3.Final(@header.crc, SizeOf(header.crc) shl 3);
    FastSetRawByteString(frame, @header, SizeOf(header));
    FrameSign(frame); // optional digital signature
    fTransmit.Send(frame);
    // server will wait until both sides sent an identical (signed) header
    if not fHandshake.WaitPop(TimeOutMS, nil, remote) or
       not FrameVerify(remote, SizeOf(header)) or
       not CompareMem(pointer(remote), @header, SizeOf(header)) then
      raise ETunnel.CreateUtf8('%.BindLocalPort handshake failed', [self]);
    // optional ECDHE ephemeral encryption
    FillZero(key.b);
    if toEcdhe in fOptions then
    begin
      if not EcdheHandshake(TimeOutMS, fTransmit, ecdhe) or
         not Ecc256r1SharedSecret(ecdhe.pub, ecdhe.priv, secret) then
        raise ETunnel.CreateUtf8('%.BindLocalPort ECDHE failed', [self]);
      sha3.Init(SHA3_256);
      sha3.Update(@ecdhe.rnd, SizeOf(ecdhe.rnd));    // random server salt
      sha3.Update(@header.crc, SizeOf(header.crc));  // avoid session replay
      sha3.Update(@secret, SizeOf(secret));          // ephemeral secret
      sha3.Final(key.b); // key.Lo/Hi = AES-128-CTR key/iv
    end;
    // launch the background processing thread
    FreeAndNil(fHandshake); // ends the handshaking phase
    fThread := TTunnelLocalThread.Create(self, fTransmit, key.Lo, key.Hi, sock);
    SleepHiRes(100, fThread.fStarted);
  except
    sock.ShutdownAndClose(true); // any error would abort and return 0
    result := 0;
  end;
  FreeAndNil(fHandshake);
  FillZero(ecdhe.priv);
  FillZero(secret);
  FillZero(key.b);
  fPort := result;
end;

function TTunnelLocal.LocalPort: RawUtf8;
begin
  if (self = nil) or
     (fPort = 0) then
    result := ''
  else
    UInt32ToUtf8(fPort, result);
end;

function TTunnelLocal.Encrypted: boolean;
begin
  result := (self <> nil) and
            (fThread <> nil) and
            (fThread.fAes[false] <> nil);
end;


{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

{ TTunnelLocalServer }

function TTunnelLocalServer.ComputeOptionsFromCert: TTunnelOptions;
begin
  result := [];
  if Assigned(fSignCert) then
    include(result, toServerSigned);
  if Assigned(fVerifyCert) then
    include(result, toClientSigned);
end;

function TTunnelLocalServer.EcdheHandshake(TimeOutMS: integer;
  const Transmit: ITunnelTransmit; out ecdhe: TTunnelEcdheContext): boolean;
var
  frame, remote: RawByteString;
begin
  result := false;
  // EDCHE handshake with perfect forward security - server side
  ecdhe := fContext; // pre-computed/specified by overriden Create
  FastSetRawByteString(frame, @ecdhe, SizeOf(ecdhe.rnd) + SizeOf(ecdhe.pub));
  Transmit.Send(frame); // frame = rnd+pub
  if not fHandshake.WaitPop(TimeOutMS, nil, remote) or
     (length(remote) <> SizeOf(TEccPublicKey)) then
    exit;
  ecdhe.pub := PEccPublicKey(remote)^; // remote = pub
  result := true;
end;


{ TTunnelLocalClient }

function TTunnelLocalClient.ComputeOptionsFromCert: TTunnelOptions;
begin
  result := [];
  if Assigned(fSignCert) then
    include(result, toClientSigned);
  if Assigned(fVerifyCert) then
    include(result, toServerSigned);
end;

function TTunnelLocalClient.EcdheHandshake(TimeOutMS: integer;
  const Transmit: ITunnelTransmit; out ecdhe: TTunnelEcdheContext): boolean;
var
  frame, remote: RawByteString;
begin
  result := false;
  // EDCHE handshake with perfect forward security - client side
  if not fHandshake.WaitPop(TimeOutMS, nil, remote) or // remote = rnd+pub
     (length(remote) <> SizeOf(ecdhe.rnd) + SizeOf(ecdhe.pub)) then
    exit;
  FastSetRawByteString(frame, @fContext.pub, SizeOf(TEccPublicKey));
  Transmit.Send(frame); // frame = pub
  ecdhe.priv := fContext.priv;
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

