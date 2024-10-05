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
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.rtti,
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
  // - if toEcdhe is not set, toEncrypt will setup a symmetric encryption
  // - only localhost clients are accepted, unless toAcceptNonLocal is set
  // - toClientSigned/toServerSigned will be set by Open() according to
  // the actual certificates available, to ensure an authenticated handshake
  TTunnelOption = (
    toEcdhe,
    toEncrypt,
    toAcceptNonLocal,
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
  // - if toEcdhe or toEncrypt option is set, the frames are already encrypted
  ITunnelTransmit = interface(IInvokable)
    ['{F481A93C-1321-49A6-9801-CCCF065F3973}']
    /// should send Frame to the relay server
    // - no result so that the frames could be gathered e.g. over WebSockets
    procedure Send(const Frame: RawByteString);
  end;

  /// abstract tunneling service implementation
  ITunnelLocal = interface(IServiceWithCallbackReleased)
    ['{201150B4-6E28-47A3-AAE5-1335C82B060A}']
    /// to be called before Open() for proper handshake process
    procedure SetTransmit(const Transmit: ITunnelTransmit);
    /// this is the main method to start tunneling to the Transmit interface
    // - Session, TransmitOptions and AppSecret should match on both sides
    // - if Address has a port, will connect a socket to this address:port
    // - if Address has no port, will bound its address an an ephemeral port,
    // which is returned as result for proper client connection
    function Open(Session: TTunnelSession; TransmitOptions: TTunnelOptions;
      TimeOutMS: integer; AppSecret: RawByteString; const Address: RawUtf8;
      out RemotePort: TNetPort): TNetPort;
    /// the local port used for the tunnel local process
    function LocalPort: RawUtf8;
    /// check if the background processing thread is using encrypted frames
    function Encrypted: boolean;
  end;

  TTunnelLocal = class;

  /// background thread bound or connected to a local port process
  TTunnelLocalThread = class(TLoggedThread)
  protected
    fState: (stCreated, stAccepting, stProcessing, stTerminated);
    fStarted: boolean;
    fOwner: TTunnelLocal;
    fTransmit: ITunnelTransmit;
    fAes: array[{send:}boolean] of TAesAbstract;
    fServerSock, fClientSock: TNetSocket;
    fClientAddr: TNetAddr;
    fPort: TNetPort;
    fReceived, fSent: Int64;
    /// accept/connect the connection, then crypt/redirect to fTransmit
    procedure DoExecute; override;
  public
    // how much data has been processed by this background thread
    /// initialize the thread - called from Open()
    constructor Create(owner: TTunnelLocal; const transmit: ITunnelTransmit;
      const key, iv: THash128; sock: TNetSocket); reintroduce;
    /// release all sockets and encryption state
    destructor Destroy; override;
    /// redirected from TTunnelLocal.Send
    procedure OnReceived(const Frame: RawByteString);
  published
    property Received: Int64
      read fReceived;
    property Sent: Int64
      read fSent;
  end;

  /// identify the TTunnelLocal handshake protocol version
  TTunnelLocalMagic = (
    tlmNone,
    tlmVersion1);

  /// define the inital frame exchanged between both TTunnelLocal ends
  // - full frame content (therefore the execution context) should match
  // - its content is signed if toClientSigned/toServerSigned are in options
  TTunnelLocalHeader = packed record
    /// protocol version
    magic: TTunnelLocalMagic;
    /// define how this link should be setup
    options: TTunnelOptions;
    /// a genuine integer ID
    session: TTunnelSession;
    /// SHA3 checksum/padding of the previous fields with app specific salt
    crc: THash128;
    /// the local port used for communication - may be an ephemeral bound port
    port: word;
  end;
  PTunnelLocalHeader = ^TTunnelLocalHeader;

  /// define the wire frame layout for TTunnelLocal optional ECDHE handshake
  TTunnelEcdhFrame = packed record
    /// the 128-bit client or server random
    rnd: TAesBlock;
    /// the public key of this side
    pub: TEccPublicKey;
  end;
  PTunnelEcdhFrame = ^TTunnelEcdhFrame;

  /// abstract tunneling service implementation
  // - is properly implemented by TTunnelLocalServer/TTunnelLocalClient classes
  // - published ITunnelTransmit so that could be used as receival callback
  // - if you release this instance, the tunnel will end
  TTunnelLocal = class(TInterfacedPersistent,
    ITunnelLocal, ITunnelTransmit)
  protected
    fOptions: TTunnelOptions;
    fPort: TNetPort;
    fThread: TTunnelLocalThread;
    fHandshake: TSynQueue;
    fEcdhe: TEccKeyPair;
    fTransmit: ITunnelTransmit;
    fSignCert, fVerifyCert: ICryptCert;
    fSession: TTunnelSession;
    fOpenBind: boolean;
    // methods to be overriden according to the client/server side
    function ComputeOptionsFromCert: TTunnelOptions; virtual; abstract;
    procedure EcdheHashRandom(var sha3: TSha3;
      const local, remote: TTunnelEcdhFrame); virtual; abstract;
    // can optionally add a signature to the main handshake frame
    procedure FrameSign(var frame: RawByteString); virtual;
    function FrameVerify(const frame: RawByteString;
      payloadlen: PtrInt): boolean; virtual;
  public
    /// initialize the instance for process
    // - if no Context value is supplied, will compute an ephemeral key pair
    constructor Create(SpecificKey: PEccKeyPair = nil); reintroduce;
    /// finalize the server
    destructor Destroy; override;
    /// called e.g. by CallbackReleased
    procedure ClosePort;
  public
    /// ITunnelTransmit method: when a Frame is received from the relay server
    procedure Send(const Frame: RawByteString);
    /// ITunnelLocal method: to be called before Open()
    procedure SetTransmit(const Transmit: ITunnelTransmit);
    /// ITunnelLocal method: initialize tunnelling process
    // - TransmitOptions will be amended to follow SignCert/VerifyCert properties
    // - if Address has a port, will connect a socket to this address:port
    // - if Address has no port, will bound its address an an ephemeral port,
    // which is returned as result for proper client connection
    function Open(Sess: TTunnelSession; TransmitOptions: TTunnelOptions;
      TimeOutMS: integer; AppSecret: RawByteString; const Address: RawUtf8;
      out RemotePort: TNetPort): TNetPort;
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
    /// the ephemeral port on the loopback as returned by Open()
    property Port: TNetPort
      read fPort;
    /// the connection specifications, as used by Open()
    property Options: TTunnelOptions
      read fOptions;
    /// the TTunnelSession as supplied to Open()
    property Session: TTunnelSession
      read fSession;
    /// access to the associated background thread processing the data
    property Thread: TTunnelLocalThread
      read fThread;
  end;

const
  toEncrypted = [toEcdhe, toEncrypt];

function ToText(opt: TTunnelOptions): shortstring; overload;


{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

type
  /// implements server-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel
  TTunnelLocalServer = class(TTunnelLocal)
  protected
    function ComputeOptionsFromCert: TTunnelOptions; override;
    procedure EcdheHashRandom(var sha3: TSha3;
      const local, remote: TTunnelEcdhFrame); override;
  end;


type
  /// implements client-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel
  TTunnelLocalClient = class(TTunnelLocal)
  protected
    function ComputeOptionsFromCert: TTunnelOptions; override;
    procedure EcdheHashRandom(var sha3: TSha3;
      const local, remote: TTunnelEcdhFrame); override;
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
  if not IsZero(key) then
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
  inherited Create({suspended=}false, TSynLog, FormatUtf8('tun %', [fPort]));
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
      ETunnel.RaiseUtf8('%.OnReceived(%): decrypt error', [self, fPort]);
    end;
  end;
  // relay the (decrypted) data to the local loopback
  if Terminated then
    exit;
  inc(fReceived, length(data));
  res := fClientSock.SendAll(pointer(data), length(data), @Terminated);
  if (res = nrOk) or
     Terminated then
    exit;
  Terminate;
  ETunnel.RaiseUtf8('%.OnReceived(%): error % when retransmitting',
    [self, fPort, ToText(res)^]);
end;

procedure TTunnelLocalThread.DoExecute;
var
  tmp: RawByteString;
  res: TNetResult;
begin
  fStarted := true;
  try
    if fOwner.fOpenBind then
    begin
      // newsocket() was done in the main thread: blocking accept() now
      fState := stAccepting;
      fLog.Log(sllTrace,
        'DoExecute: waiting for accept on port %', [fPort], self);
      res := fServerSock.Accept(fClientSock, fClientAddr, {async=}false);
      if (res = nrOk) and
         not Terminated then
      begin
        fLog.Log(sllTrace,
          'DoExecute: accepted %', [fClientAddr.IPWithPort], self);
        if (toAcceptNonLocal in fOwner.Options) or
           (fClientAddr.IP4 = cLocalhost32) then
         fState := stProcessing // start background process
        else
          fLog.Log(sllWarning, 'DoExecute: rejected non local client', self);
      end;
    end
    else
      // newsocket() with connect() was done in the main thread
      fState := stProcessing;
    if fState = stProcessing then
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
              inc(fSent, length(tmp));
              if fAes[{send:}true] <> nil then
                tmp := fAes[true].EncryptPkcs7(tmp, {ivatbeg=}false);
              if (fTransmit <> nil) and
                 not Terminated then
                fTransmit.Send(tmp);
            end;
        else
          ETunnel.RaiseUtf8('%.Execute(%): error % at receiving',
            [self, fPort, ToText(res)^]);
        end;
      end;
    fLog.Log(sllTrace, 'DoExecute: ending %', [self]);
  except
    fLog.Log(sllWarning, 'DoExecute: aborted %', [self]);
    fOwner.ClosePort;
  end;
  fState := stTerminated;
end;


{ TTunnelLocal }

constructor TTunnelLocal.Create(SpecificKey: PEccKeyPair);
begin
  inherited Create;
  if SpecificKey <> nil then
    fEcdhe := SpecificKey^;
  fHandshake := TSynQueue.Create(TypeInfo(TRawByteStringDynArray));
end;

destructor TTunnelLocal.Destroy;
begin
  if fThread <> nil then
    ClosePort; // calls Terminate
  inherited Destroy;
  FillCharFast(fEcdhe, SizeOf(fEcdhe), 0);
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
    fThread.fTransmit := Transmit; // could be refreshed during process
end;

function TTunnelLocal.Open(Sess: TTunnelSession;
  TransmitOptions: TTunnelOptions; TimeOutMS: integer; AppSecret: RawByteString;
  const Address: RawUtf8; out RemotePort: TNetPort): TNetPort;
var
  uri: TUri;
  sock: TNetSocket;
  addr: TNetAddr;
  frame, remote: RawByteString;
  header: TTunnelLocalHeader;
  secret: TEccSecretKey;
  key: THash256Rec;
  sha3: TSha3;
  log: ISynLog;
begin
  log := TSynLog.Enter('Open(%)', [Session], self);
  // validate input parameters
  if (fPort <> 0) or
     (not Assigned(fTransmit)) then
    ETunnel.RaiseUtf8('%.Open invalid call', [self]);
  if not uri.From(Address, '0') then
    ETunnel.RaiseUtf8('%.Open invalid %', [self, Address]);
  RemotePort := 0;
  fSession := Sess;
  TransmitOptions := TransmitOptions - [toClientSigned, toServerSigned];
  TransmitOptions := TransmitOptions + ComputeOptionsFromCert;
  // bind to a local ephemeral port
  ClosePort;
  result := uri.PortInt;
  if result = 0 then
  begin
    // bind on port='0' = ephemeral port
    ENetSock.Check(NewSocket(uri.Server, uri.Port, nlTcp, {bind=}true,
      TimeOutMS, TimeOutMS, TimeOutMS, {retry=}0, sock, @addr), 'Open');
    result := addr.Port;
    if Assigned(log) then
      log.Log(sllTrace, 'Open: bound to %', [addr.IPWithPort], self);
    fOpenBind := true;
  end
  else
  begin
    // connect to a local socket on address:port
    ENetSock.Check(NewSocket(uri.Server, uri.Port, nlTcp, {bind=}false,
      TimeOutMS, TimeOutMS, TimeOutMS, {retry=}0, sock, @addr), 'Open');
    if Assigned(log) then
      log.Log(sllTrace, 'Open: connected to %:%', [uri.Server, uri.Port], self);
  end;
  fOptions := TransmitOptions;
  try
    // initial handshake: same TTunnelOptions + TTunnelSession from both sides
    if AppSecret = '' then
      AppSecret := 'AB15C52F754F49CB9B23CF88735E39C8'; // some default random
    header.magic := tlmVersion1;
    header.options := fOptions;
    header.session := fSession;
    sha3.Init(SHA3_224);
    sha3.Update(AppSecret); // custom symmetric application-specific secret
    sha3.Update(@header, SizeOf(header) - SizeOf(header.crc) - SizeOf(header.port));
    sha3.Final(@header.crc, SizeOf(header.crc) shl 3);
    header.port := result; // port is asymmetrical so not part of the crc
    FastSetRawByteString(frame, @header, SizeOf(header));
    FrameSign(frame); // optional digital signature
    fTransmit.Send(frame);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: after Send1 len=', [length(frame)], self);
    // server will wait until both sides sent an identical (signed) header
    if not fHandshake.WaitPop(TimeOutMS, nil, remote) then
      ETunnel.RaiseUtf8('Open handshake timeout on port %', [result]);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: after WaitPop1 len=', [length(remote)], self);
    if not FrameVerify(remote, SizeOf(header)) or // also checks length(remote)
       not CompareMem(pointer(remote), @header,
             SizeOf(header) - SizeOf(header.port)) then
      ETunnel.RaiseUtf8('Open handshake failed on port %', [result]);
    RemotePort := PTunnelLocalHeader(remote)^.port;
    // optional encryption
    FillZero(key.b);
    if toEncrypted * fOptions <> [] then
    begin
      sha3.Init(SHA3_256);
      sha3.Update(@header, SizeOf(header) - SizeOf(header.port)); // no replay
      if toEcdhe in fOptions then
      begin
        // optional ECDHE ephemeral encryption
        FastNewRawByteString(frame, SizeOf(TTunnelEcdhFrame));
        with PTunnelEcdhFrame(frame)^ do
          RandomBytes(@rnd, SizeOf(rnd)); // Lecuyer is enough for public random
        if IsZero(fEcdhe.pub) then // ephemeral key was not specified at Create
          if not Ecc256r1MakeKey(fEcdhe.pub, fEcdhe.priv) then
            ETunnel.RaiseUtf8('%.Open: no ECC engine available', [self]);
        PTunnelEcdhFrame(frame)^.pub := fEcdhe.pub;
        fTransmit.Send(frame);
        if Assigned(log) then
          log.Log(sllTrace, 'Open: after Send2 len=', [length(frame)], self);
        if not fHandshake.WaitPop(TimeOutMS, nil, remote) then
          exit;
        if Assigned(log) then
          log.Log(sllTrace, 'Open: after WaitPop2 len=', [length(remote)], self);
        if (length(remote) <> SizeOf(TTunnelEcdhFrame)) or
           not Ecc256r1SharedSecret(
             PTunnelEcdhFrame(remote)^.pub, fEcdhe.priv, secret) then
          exit;
        EcdheHashRandom(sha3, PTunnelEcdhFrame(frame)^, PTunnelEcdhFrame(remote)^);
        sha3.Update(@secret, SizeOf(secret)); // ephemeral secret
      end
      else
        // optional encryption using symmetric secret
        sha3.Update(AppSecret);
      sha3.Final(key.b); // key.Lo/Hi = AES-128-CTR key/iv
    end;
    // launch the background processing thread
    if Assigned(log) then
      log.Log(sllTrace, 'Open: % success', [ToText(fOptions)], self);
    FreeAndNil(fHandshake); // ends the handshaking phase
    fPort := result;
    fThread := TTunnelLocalThread.Create(self, fTransmit, key.Lo, key.Hi, sock);
    SleepHiRes(100, fThread.fStarted);
  except
    sock.ShutdownAndClose(true); // any error would abort and return 0
    result := 0;
  end;
  FreeAndNil(fHandshake);
  FillZero(secret);
  FillZero(key.b);
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


function ToText(opt: TTunnelOptions): shortstring;
begin
  GetSetNameShort(TypeInfo(TTunnelOptions), opt, result, {trim=}true);
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

procedure TTunnelLocalServer.EcdheHashRandom(var sha3: TSha3;
  const local, remote: TTunnelEcdhFrame);
begin
  sha3.Update(@remote.rnd, SizeOf(remote.rnd)); // client random
  sha3.Update(@local.rnd, SizeOf(local.rnd));   // server random
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

procedure TTunnelLocalClient.EcdheHashRandom(var sha3: TSha3;
  const local, remote: TTunnelEcdhFrame);
begin
  sha3.Update(@local.rnd, SizeOf(local.rnd));   // client random
  sha3.Update(@remote.rnd, SizeOf(remote.rnd)); // server random
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
     (PClass(Sender.Protocol)^ = TTunnelRelayServerProtocol) then
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
          ETunnel.RaiseUtf8('%.ProcessIncomingFrame: bad handshake %.%.%',
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
              ETunnel.RaiseUtf8('%.ProcessIncomingFrame: abusive', [self]);
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

