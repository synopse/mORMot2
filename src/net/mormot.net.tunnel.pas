/// Network Port Forwarding / Tunnelling
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
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.json,
  mormot.core.threads,
  mormot.core.rtti,
  mormot.core.log,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.crypt.core,
  mormot.crypt.secure,   // for ICryptCert
  mormot.crypt.ecc256r1, // for ECDHE encryption
  mormot.net.sock;


{ ******************** Abstract Definitions for Port Forwarding }

type
  ETunnel = class(ESynException);

  /// each option available for TTunnelLocal process
  // - toEcdhe will compute an ephemeral secret to encrypt the link
  // - if toEcdhe is not set, toEncrypt will ensure a symmetric encryption
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
  // - typically a Random32 or a TBinaryCookieGeneratorSessionID value
  TTunnelSession = Int64;
  PTunnelSession = ^TTunnelSession;

  /// abstract transmission layer with the central relay server
  // - may be implemented as raw sockets or over a mORMot SOA WebSockets link
  // - if toEcdhe or toEncrypt option is set, the frames are already encrypted
  // - named as Tunnel*() methods to be joined as a single service interface,
  // to leverage a single WebSockets callback
  ITunnelTransmit = interface(IInvokable)
    ['{F481A93C-1321-49A6-9801-CCCF065F3973}']
    /// main method to emit the supplied binary Frame to the relay server
    // - the raw binary frame always end with 8 bytes of 64-bit TTunnelSession
    // - no result so that the frames could be gathered e.g. over WebSockets
    // - single binary parameter so that could be transmitted as
    // BINARY_CONTENT_TYPE without any base-64 encoding (to be done at WS level)
    procedure TunnelSend(const Frame: RawByteString);
    /// return some information about this connection(s)
    // - as a TDocVariant object for a single connection, or array for a node
    function TunnelInfo: variant;
  end;

  /// abstract tunneling service implementation
  ITunnelLocal = interface(ITunnelTransmit)
    ['{201150B4-6E28-47A3-AAE5-1335C82B060A}']
    /// match mormot.soa.core IServiceWithCallbackReleased definition
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
    /// to be called before Open() for proper handshake process
    procedure SetTransmit(const Transmit: ITunnelTransmit);
    /// the associated tunnel session ID
    function TunnelSession: TTunnelSession;
    /// the local port used for the tunnel local process
    function LocalPort: RawUtf8;
    /// the remote port used for the tunnel local process
    function RemotePort: cardinal;
    /// check if the background processing thread is using encrypted frames
    function Encrypted: boolean;
  end;
  PITunnelLocal = ^ITunnelLocal;

  TTunnelLocal = class;

  /// background thread bound or connected to a local port process
  TTunnelLocalThread = class(TLoggedThread)
  protected
    fState: (stCreated, stAccepting, stProcessing, stTerminated);
    fStarted: boolean;
    fOwner: TTunnelLocal;
    fTransmit: ITunnelTransmit;
    fSession: TTunnelSession;
    fAes: array[{sending:}boolean] of TAesAbstract;
    fServerSock, fClientSock: TNetSocket;
    fClientAddr: TNetAddr;
    fPort: TNetPort;
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
  end;

  /// define the wire frame layout for TTunnelLocal optional ECDHE handshake
  TTunnelEcdhFrame = packed record
    /// the 128-bit client or server random nonce
    rnd: TAesBlock;
    /// the public key of this side (may be just random if toEcdhe is not set)
    pub: TEccPublicKey;
  end;

  /// identify the TTunnelLocal handshake protocol version
  TTunnelLocalMagic = (
    tlmNone,
    tlmVersion1);

  /// the header of the inital frame exchanged between both TTunnelLocal ends
  // - full header content (therefore the execution context) should match
  // - should be at the end of the handshake, and ending with crc+port
  TTunnelLocalInfo = packed record
    /// protocol version
    magic: TTunnelLocalMagic;
    /// define how this link should be setup
    options: TTunnelOptions;
    /// a genuine integer ID
    session: TTunnelSession;
    /// the local port used for communication - may be an ephemeral bound port
    port: word;
    /// SHAKE128 checksum/padding of all previous TTunnelLocalHandshake fields
    // - should be the very last field
    crc: THash128;
  end;

  /// define the inital frame exchanged between both TTunnelLocal ends
  // - its content is signed if toClientSigned/toServerSigned are in options
  TTunnelLocalHandshake = packed record
    /// optional ECDHE information - used if toEcdhe is in Info.Options
    Ecdh: TTunnelEcdhFrame;
    /// a digitally signed header with shared information
    Info: TTunnelLocalInfo;
  end;
  PTunnelLocalHandshake = ^TTunnelLocalHandshake;

  /// abstract tunneling service implementation
  // - is properly implemented by TTunnelLocalServer/TTunnelLocalClient classes
  // - published ITunnelTransmit so that could be used as receival callback
  // - if you release this instance, the tunnel will end
  TTunnelLocal = class(TInterfacedPersistent,
    ITunnelLocal, ITunnelTransmit)
  protected
    fSession: TTunnelSession;
    fSendSafe: TMultiLightLock;
    fPort, fRemotePort: TNetPort;
    fOptions: TTunnelOptions;
    fOpenBind, fClosePortNotified: boolean;
    fThread: TTunnelLocalThread;
    fHandshake: TSynQueue;
    fEcdhe: TEccKeyPair;
    fTransmit: ITunnelTransmit;
    fSignCert, fVerifyCert: ICryptCert;
    fReceived, fSent: Int64;
    fLogClass: TSynLogClass;
    fStartTicks: cardinal;
    fInfo: TDocVariantData;
    // methods to be overriden according to the client/server side
    function ComputeOptionsFromCert: TTunnelOptions; virtual; abstract;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); virtual; abstract;
    // can optionally add a signature to the main handshake frame
    procedure FrameSign(var frame: RawByteString); virtual;
    function FrameVerify(frame: PAnsiChar; framelen, payloadlen: PtrInt): boolean; virtual;
  public
    /// initialize the instance for process
    // - if no Context value is supplied, will compute an ephemeral key pair
    constructor Create(Logger: TSynLogClass = nil;
      SpecificKey: PEccKeyPair = nil); reintroduce;
    /// main method to initialize tunnelling process
    // - TransmitOptions will be amended to follow SignCert/VerifyCert properties
    // - if Address has a port, will connect a socket to this address:port
    // - if Address has no port, will bound its address an an ephemeral port,
    // which is returned as result for proper client connection
    function Open(Sess: TTunnelSession; TransmitOptions: TTunnelOptions;
      TimeOutMS: integer; const AppSecret, Address: RawUtf8;
      const InfoNameValue: array of const): TNetPort;
    /// finalize this instance, and its local TCP server
    destructor Destroy; override;
    /// called e.g. by CallbackReleased() or by Destroy
    procedure ClosePort;
  public
    /// ITunnelTransmit method: when a Frame is received from the relay server
    procedure TunnelSend(const aFrame: RawByteString);
    /// ITunnelTransmit method: return some information about this connection
    function TunnelInfo: variant;
    /// ITunnelLocal method: to be called before Open()
    procedure SetTransmit(const Transmit: ITunnelTransmit);
    /// ITunnelLocal method: return the associated tunnel session ID
    function TunnelSession: TTunnelSession;
    /// ITunnelLocal method: return the local port
    function LocalPort: RawUtf8;
    /// ITunnelLocal method: return the remote port
    function RemotePort: cardinal;
    /// ITunnelLocal method: check if the background thread uses encrypted frames
    function Encrypted: boolean;
    /// ITunnelLocal method: when a ITunnelTransmit remote callback is finished
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
    /// access the logging features of this class
    property LogClass: TSynLogClass
      read fLogClass write fLogClass;
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
    /// the ephemeral port on the remote loopback as returned by Open()
    // on the other side
    property Remote: TNetPort
      read fRemotePort;
    /// the connection specifications, as used by Open()
    property Options: TTunnelOptions
      read fOptions;
    /// the TTunnelSession as supplied to Open()
    property Session: TTunnelSession
      read fSession;
    /// access to the associated background thread processing the data
    property Thread: TTunnelLocalThread
      read fThread;
    /// input TCP frames bytes
    property Received: Int64
      read fReceived;
    /// output TCP frames bytes
    property Sent: Int64
      read fSent;
  end;

function ToText(opt: TTunnelOptions): ShortString; overload;

/// extract the 64-bit session trailer from a ITunnelTransmit.TunnelSend() frame
function FrameSession(const Frame: RawByteString): TTunnelSession;
  {$ifdef HASINLINE} inline; {$endif}

const
  toEncrypted = [toEcdhe, toEncrypt];


{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

type
  /// implements server-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel
  TTunnelLocalServer = class(TTunnelLocal)
  protected
    function ComputeOptionsFromCert: TTunnelOptions; override;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); override;
  end;

  /// implements client-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel
  TTunnelLocalClient = class(TTunnelLocal)
  protected
    function ComputeOptionsFromCert: TTunnelOptions; override;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); override;
  end;

  /// maintain a list of ITunnelLocal instances
  // - with proper redirection of ITunnelTransmit.TunnelSend() frames
  TTunnelList = class(TInterfacedPersistent,
    ITunnelTransmit)
  protected
    fSafe: TRWLightLock;
    fItem: array of ITunnelLocal;
  public
    /// append one ITunnelLocal to the list
    function Add(const aInstance: ITunnelLocal): boolean;
    /// remove one ITunnelLocal from its session ID
    function Delete(aSession: TTunnelSession): boolean;
    /// search if one ITunnelLocal matches a session ID
    function Exists(aSession: TTunnelSession): boolean;
    /// search the ITunnelLocal matching a session ID
    function Get(aSession: TTunnelSession; var aInstance: ITunnelLocal): boolean;
  public
    /// ITunnelTransmit method which will redirect the given frame to the
    // expected registered TTunnelLocal instance
    procedure TunnelSend(const Frame: RawByteString);
    /// ITunnelTransmit method: return some information about these connections
    function TunnelInfo: variant;
  end;



implementation


{ ******************** Abstract Definitions for Port Forwarding }

{ TTunnelLocalThread }

constructor TTunnelLocalThread.Create(owner: TTunnelLocal;
  const transmit: ITunnelTransmit; const key, iv: THash128; sock: TNetSocket);
begin
  fOwner := owner;
  fPort := owner.Port;
  fSession := owner.Session;
  fTransmit := transmit;
  if not IsZero(key) then
  begin
    // ecc256r1 shared secret has 128-bit resolution -> 128-bit AES-CTR
    fAes[{sending:}false] := AesIvUpdatedCreate(mCtr, key, 128);
    fAes[{sending:}true]  := AesIvUpdatedCreate(mCtr, key, 128);
    // won't include an IV with each frame, but update it from (ecdhe) KDF
    fAes[false].IV := iv;
    fAes[true].IV := iv;
  end;
  fServerSock := sock;
  FreeOnTerminate := true;
  inherited Create({suspended=}false, nil, nil, fOwner.fLogClass, Make(['tun ', fPort]));
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
  if fAes[{sending:}false] = nil then
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
  if fOwner <> nil then
    inc(fOwner.fReceived, length(data));
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
    if (fOwner <> nil) and
       fOwner.fOpenBind then
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
          'DoExecute: accepted %', [fClientAddr.IPShort({port=}true)], self);
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
              // emit the (encrypted) data with a 64-bit TTunnelSession trailer
              if fOwner <> nil then
                inc(fOwner.fSent, length(tmp)); // Sent/Received are plain sizes in bytes
              if fAes[{send:}true] <> nil then
                tmp := fAes[true].EncryptPkcs7(tmp, {ivatbeg=}false, {trailer=}8)
              else
                SetLength(tmp, length(tmp) + 8);
              PInt64(@PByteArray(tmp)[length(tmp) - 8])^ := fSession;
              if (fTransmit <> nil) and
                 not Terminated then
                fTransmit.TunnelSend(tmp);
            end;
        else
          ETunnel.RaiseUtf8('%.Execute(%): error % at receiving',
            [self, fPort, ToText(res)^]);
        end;
      end;
    fLog.Log(sllTrace, 'DoExecute: ending %', [self]);
  except
    fLog.Log(sllWarning, 'DoExecute: aborted %', [self]);
    if fOwner <> nil then
      fOwner.ClosePort;
  end;
  fState := stTerminated;
end;


{ TTunnelLocal }

constructor TTunnelLocal.Create(Logger: TSynLogClass; SpecificKey: PEccKeyPair);
begin
  fLogClass := Logger;
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
  notifycloseport: RawByteString;
  callback: TNetSocket; // touch-and-go to the server to release main Accept()
begin
  if self = nil then
    exit;
  if not fClosePortNotified then
    try
      fClosePortNotified := true;
      PInt64(FastNewRawByteString(notifycloseport, 8))^ := fSession;
      TunnelSend(notifycloseport);
    except
    end;
  thread := fThread;
  if thread <> nil then
    try
      fThread := nil;
      thread.fOwner := nil;
      thread.Terminate;
      if thread.fState = stAccepting then
        if NewSocket(cLocalhost, UInt32ToUtf8(fPort), nlTcp,
           {dobind=}false, 10, 0, 0, 0, callback) = nrOK then
          // Windows socket may not release Accept() until connected
          callback.ShutdownAndClose({rdwr=}false);
    except
    end;
  fPort := 0;
end;

procedure TTunnelLocal.TunnelSend(const aFrame: RawByteString);
var
  l: PtrInt;
  p: PAnsiChar;
begin
  // ITunnelTransmit method: when a Frame is received from the relay server
  l := length(aFrame) - 8;
  if l < 0 then
    ETunnel.RaiseUtf8('%.Send: unexpected size=%', [self, l]);
  fSendSafe.Lock;
  try
    if fHandshake <> nil then
    begin
      fHandshake.Push(aFrame); // during the handshake phase - maybe before Open
      exit;
    end;
    p := pointer(aFrame);
    if PInt64(p + l)^ <> fSession then
      ETunnel.RaiseUtf8('%.Send: session mismatch', [self]);
    if l = 0 then
    begin
      fClosePortNotified := true; // the other party notified end of process
      ClosePort;
    end
    else if fThread <> nil then // = nil after ClosePort (too late)
    begin
      PStrLen(p - _STRLEN)^ := l; // trim 64-bit session trailer
      fThread.OnReceived(aFrame); // regular tunelling process
    end;
  finally
    fSendSafe.UnLock;
  end;
end;

procedure TTunnelLocal.CallbackReleased(const callback: IInvokable;
  const interfaceName: RawUtf8);
begin
  if not IdemPChar(pointer(interfaceName), 'ITUNNEL') then
    exit; // should be ITunnelLocal or ITunnelTransmit
  fClosePortNotified := true; // no need to notify the remote end
  ClosePort;
end;

procedure TTunnelLocal.FrameSign(var frame: RawByteString);
begin
  if fSignCert <> nil then
    Append(frame, fSignCert.Sign(frame));
end;

function TTunnelLocal.FrameVerify(frame: PAnsiChar; framelen, payloadlen: PtrInt): boolean;
begin
  dec(framelen, payloadlen);
  result := (framelen >= 0) and
            ((fVerifyCert = nil) or
             (fVerifyCert.Verify(frame + payloadlen, frame, framelen, payloadlen)
               in CV_VALIDSIGN));
end;

procedure TTunnelLocal.SetTransmit(const Transmit: ITunnelTransmit);
begin
  fTransmit := Transmit;
  if fThread <> nil then
    fThread.fTransmit := Transmit; // could be refreshed during process
end;

procedure TunnelHandshakeCrc(const Handshake: TTunnelLocalHandshake;
  const appsecret: RawUtf8; out crc: THash128);
var
  sha3: TSha3;
begin
  sha3.Init(SHAKE_128); // 128-bit digital signature of Handshake.Ecdh+Info
  if appsecret = '' then
    sha3.Update('AB15C52F754F49CB9B23CF88735E39C8') // some default secret
  else
    sha3.Update(appsecret); // custom symmetric application-specific secret
  sha3.Update(@Handshake, SizeOf(Handshake) - SizeOf(Handshake.Info.crc));
  sha3.Final(@crc, 128);
end;

function TTunnelLocal.Open(Sess: TTunnelSession; TransmitOptions: TTunnelOptions;
  TimeOutMS: integer; const AppSecret, Address: RawUtf8;
  const InfoNameValue: array of const): TNetPort;
var
  uri: TUri;
  sock: TNetSocket;
  addr: TNetAddr;
  l, li: PtrInt;
  frame, remote, info: RawByteString;
  infoaes: TAesCtr;
  rem: PTunnelLocalHandshake absolute remote;
  loc: TTunnelLocalHandshake;
  key, iv: THash256Rec;
  hmackey, hmaciv: THmacSha256;
  hqueue: TSynQueue;
  log: ISynLog;
const // port is asymmetrical so not included to the KDF - nor the crc
  KDF_SIZE = SizeOf(loc.Info) - (SizeOf(loc.Info.port) + SizeOf(loc.Info.crc));
begin
  if fLogClass <> nil then
    fLogClass.EnterLocal(log, 'Open(%)', [Session], self);
  // validate input parameters
  if (fPort <> 0) or
     (not Assigned(fTransmit)) then
    ETunnel.RaiseUtf8('%.Open invalid call', [self]);
  if not uri.From(Address, '0') then
    ETunnel.RaiseUtf8('%.Open invalid %', [self, Address]);
  fRemotePort := 0;
  fInfo.Clear;
  fSession := Sess;
  TransmitOptions := (TransmitOptions - [toClientSigned, toServerSigned]) +
                     ComputeOptionsFromCert;
  // bind to a local (ephemeral) port
  if fThread <> nil then
  begin
    fClosePortNotified := true; // emulate a clean remote closing
    ClosePort;                  // close any previous Open()
  end;
  fPort := 0;
  fClosePortNotified := false;
  result := uri.PortInt;
  if result = 0 then
  begin
    // bind on port='0' = ephemeral port
    ENetSock.Check(NewSocket(uri.Server, uri.Port, nlTcp, {bind=}true,
      TimeOutMS, TimeOutMS, TimeOutMS, {retry=}0, sock, @addr), 'Open');
    result := addr.Port;
    if Assigned(log) then
      log.Log(sllTrace, 'Open: bound to %', [addr.IPShort(true)], self);
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
  // initial single round trip handshake
  infoaes := nil;
  fOptions := TransmitOptions;
  try
    // header with optional ECDHE
    loc.Info.magic   := tlmVersion1;
    loc.Info.options := fOptions;
    loc.Info.session := fSession; // is typically an increasing sequence number
    loc.Info.port    := result;
    Random128(@loc.Ecdh.rnd);   // unpredictable
    if toEcdhe in fOptions then
    begin
      if IsZero(fEcdhe.pub) then // ephemeral key was not specified at Create
      begin
        if Assigned(log) then
          log.Log(sllTrace, 'Open: generate ECHDE key', self);
        if not Ecc256r1MakeKey(fEcdhe.pub, fEcdhe.priv) then
          ETunnel.RaiseUtf8('%.Open: no ECC engine available', [self]);
      end;
      loc.Ecdh.pub := fEcdhe.pub;
    end
    else
      SharedRandom.Fill(@loc.Ecdh.pub, SizeOf(loc.Ecdh.pub)); // some padding
    TunnelHandshakeCrc(loc, AppSecret, loc.Info.crc);
    FastSetRawByteString(frame, @loc, SizeOf(loc));
    FrameSign(frame); // optional digital signature
    // append (and potentially encrypt) the info JSON payload
    info := JsonEncode(InfoNameValue);
    if toEncrypted * fOptions <> [] then // toEncrypt or toEcdhe
    begin
      if AppSecret = '' then
        hmackey.Init('705FC9676148405B91A66FFE7C3B54AA') // some minimal key
      else
        hmackey.Init(AppSecret);
      hmackey.Update(@loc.Info, KDF_SIZE); // no replay (sequential session)
      hmaciv := hmackey; // use hmaciv for info KDF
      hmaciv.Done(key.b);
      infoaes := TAesCtr.Create(key.Lo); // simple symmetrical encryption
      infoaes.IV := key.Hi;
      info := infoaes.EncryptPkcs7(info, {ivatbeg=}false);
      infoaes.IV := key.Hi; // use the same IV for decoding "remote" info below
    end;
    li := length(info);
    if li > 65535 then
      ETunnel.RaiseUtf8('Open: too much info (len=%)', [li]);
    // actually send the frame ending with its session ID
    Append(frame, [info, '0101234567']); // li:W sess:I64
    l := length(frame);
    PWord(@PByteArray(frame)^[l - 10])^ := li;
    PInt64(@PByteArray(frame)^[l - 8])^ := fSession;
    fTransmit.TunnelSend(frame);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: after Send1 len=', [length(frame)], self);
    // this method will wait until both sides sent a valid signed header
    if not fHandshake.WaitPop(TimeOutMS, nil, remote) then
      ETunnel.RaiseUtf8('Open: handshake timeout on port %', [result]);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: received len=', [length(remote)], self);
    // ensure the returned frame is for this session
    if FrameSession(remote) <> fSession then
      ETunnel.RaiseUtf8('Open: wrong handshake trailer on port %', [result]);
    // extract (and potentially decrypt) the associated JSON info payload
    l := length(remote) - 10;
    li := PWord(@PByteArray(remote)^[l])^;
    FastSetRawByteString(info, @PByteArray(remote)^[l - li], li);
    if infoaes <> nil then
      info := infoaes.DecryptPkcs7(info, {ivatbeg=}false);
    if fInfo.InitJsonInPlace(pointer(info), JSON_FAST) = nil then
      ETunnel.RaiseUtf8('Open: invalid JSON info on port %', [result]);
    // check the digital signature, maybe using the certificate
    dec(l, li);
    if l < SizeOf(loc) then // may have a signature trailer
      ETunnel.RaiseUtf8('Open: wrong handshake size=% on port %', [l, result]);
    if not CompareMemSmall(@rem.Info, @loc.Info, KDF_SIZE) then
      ETunnel.RaiseUtf8('Open: unexpected handshake on port %', [result]);
    TunnelHandshakeCrc(rem^, AppSecret, key.Lo);
    if not IsEqual(rem^.Info.crc, key.Lo) then
      ETunnel.RaiseUtf8('Open: invalid handshake signature on port %', [result]);
    if not FrameVerify(pointer(remote), l, SizeOf(loc)) then
      ETunnel.RaiseUtf8('Open: handshake failed on port %', [result]);
    fRemotePort := rem^.Info.port;
    // optional encryption - maybe with ECDHE ephemeral keys
    FillZero(key.b);
    if toEncrypted * fOptions <> [] then // toEncrypt or toEcdhe
    begin
      // hmackey has been pre-computed above with loc.Info and AppSecret
      EcdheHashRandom(hmackey, loc.Ecdh, rem^.Ecdh); // rnd+pub in same order
      if toEcdhe in fOptions then
      begin
        if Assigned(log) then
          log.Log(sllTrace, 'Open: ECDHE shared secret', self);
        if not Ecc256r1SharedSecret(rem^.Ecdh.pub, fEcdhe.priv, key.b) then
          exit;
        hmackey.Update(key.b); // prime256v1 shared secret
      end;
      hmaciv := hmackey;     // two labeled hmacs - see NIST SP 800-108
      hmackey.Update('AES key'#0);
      hmackey.Done(key.b);   // AES-128-CTR key
      hmaciv.Update('IV'#1);
      hmaciv.Done(iv.b);     // AES-128-CTR iv
    end;
    // launch the background processing thread
    if Assigned(log) then
      log.Log(sllTrace, 'Open: % success', [ToText(fOptions)], self);
    fPort := result;
    fThread := TTunnelLocalThread.Create(self, fTransmit, key.Lo, iv.Lo, sock);
    SleepHiRes(100, fThread.fStarted);
    fStartTicks := GetUptimeSec; // wall clock
    fInfo.AddNameValuesToObject([
      'remotePort', fRemotePort,
      'localPort',  fPort,
      'started',    NowUtcToString,
      'session',    fSession,
      'encrypted',  Encrypted,
      'options',    ToText(fOptions)]);
    hqueue := fHandshake;
    fSendSafe.Lock; // re-entrant for TunnelSend()
    try
      fHandshake := nil; // ends the handshaking phase
      while hqueue.Pop(frame) do
        TunnelSend(frame); // paranoid
    finally
      fSendSafe.UnLock;
      hqueue.Free;
    end;
  except
    sock.ShutdownAndClose(true); // any error would abort and return 0
    result := 0;
  end;
  infoaes.Free;
  FreeAndNil(fHandshake);
  FillZero(key.b);
  FillZero(iv.b);
end;

function TTunnelLocal.TunnelSession: TTunnelSession;
begin
  result := fSession;
end;

function TTunnelLocal.LocalPort: RawUtf8;
begin
  if (self = nil) or
     (fPort = 0) then
    result := ''
  else
    UInt32ToUtf8(fPort, result);
end;

function TTunnelLocal.RemotePort: cardinal;
begin
  result := fRemotePort;
end;

function TTunnelLocal.Encrypted: boolean;
begin
  result := (self <> nil) and
            (fThread <> nil) and
            (fThread.fAes[false] <> nil);
end;

function TTunnelLocal.TunnelInfo: variant;
var
  dv: TDocVariantData absolute result;
begin
  VarClear(result);
  if fPort = 0 then
    exit;
  dv.InitFast(fInfo.Count + 3, dvObject);
  dv.AddFrom(fInfo);
  dv.AddNameValuesToObject([
    'elapsed',  GetUptimeSec - fStartTicks,
    'bytesIn',  fReceived,
    'bytesOut', fSent]);
end;


function ToText(opt: TTunnelOptions): ShortString;
begin
  GetSetNameShort(TypeInfo(TTunnelOptions), opt, result, {trim=}true);
  LowerCaseShort(result);
end;

function FrameSession(const Frame: RawByteString): TTunnelSession;
var
  l: PtrInt;
begin
  l := length(Frame) - 8;
  if l >= 0 then
    result := PInt64(@PByteArray(Frame)[l])^
  else
    result := 0;
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

procedure TTunnelLocalServer.EcdheHashRandom(var hmac: THmacSha256;
  const local, remote: TTunnelEcdhFrame);
begin
  hmac.Update(@remote.rnd, SizeOf(remote.rnd)); // client random
  hmac.Update(@local. rnd, SizeOf(local.rnd));  // server random
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

procedure TTunnelLocalClient.EcdheHashRandom(var hmac: THmacSha256;
  const local, remote: TTunnelEcdhFrame);
begin
  hmac.Update(@local. rnd, SizeOf(local.rnd));  // client random
  hmac.Update(@remote.rnd, SizeOf(remote.rnd)); // server random
end;


{ TTunnelList }

function FindIndexLocked(p: PITunnelLocal; s: TTunnelSession): PtrInt;
var
  n: PtrInt;
begin
  if (s <> 0) and
     (p <> nil) then
  begin
    result := 0;
    n := PDALen(PAnsiChar(p) - _DALEN)^ + (_DAOFF - 1);
    repeat
      if p^.TunnelSession = s then // fast TTunnelLocal.TunnelSession method
        exit;
      if result = n then
        break;
      inc(p);
      inc(result);
    until false;
  end;
  result := -1; // not found
end;

function TTunnelList.Exists(aSession: TTunnelSession): boolean;
begin
  fSafe.ReadLock;
  try
    result := FindIndexLocked(pointer(fItem), aSession) >= 0;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TTunnelList.Get(aSession: TTunnelSession; var aInstance: ITunnelLocal): boolean;
var
  ndx: PtrInt;
begin
  result := false;
  if aSession = 0 then
    exit;
  fSafe.ReadLock;
  try
    ndx := FindIndexLocked(pointer(fItem), aSession);
    if ndx < 0 then
      exit;
    aInstance := fItem[ndx]; // fast ref counted assignment
    result := true;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TTunnelList.Add(const aInstance: ITunnelLocal): boolean;
begin
  result := false;
  if aInstance = nil then
    exit;
  fSafe.WriteLock;
  try
    if FindIndexLocked(pointer(fItem), aInstance.TunnelSession) >= 0 then
      exit;
    InterfaceArrayAdd(fItem, aInstance);
  finally
    fSafe.WriteUnLock;
  end;
  result := true;
end;

function TTunnelList.Delete(aSession: TTunnelSession): boolean;
var
  ndx: PtrInt;
  instance: ITunnelLocal;
begin
  result := false;
  if aSession = 0 then
    exit;
  fSafe.WriteLock;
  try
    ndx := FindIndexLocked(pointer(fItem), aSession);
    if (ndx < 0) or
       not InterfaceArrayExtract(fItem, ndx, instance) then // weak copy
      exit;
  finally
    fSafe.WriteUnLock;
  end;
  instance := nil; // release outside of the blocking Write lock
  result := true;
end;

procedure TTunnelList.TunnelSend(const Frame: RawByteString);
var
  s: TTunnelSession;
  ndx: PtrInt;
begin
  s := FrameSession(Frame);
  if s = 0 then
    exit; // invalid frame for sure
  fSafe.ReadLock; // non-blocking Read lock
  try
    ndx := FindIndexLocked(pointer(fItem), s);
    if ndx < 0 then
      exit;
    fItem[ndx].TunnelSend(frame); // call ITunnelTransmit method within Read lock
  finally
    fSafe.ReadUnLock;
  end;
  if length(Frame) = 8 then // notified end of process from the other party
    Delete(s); // remove this instance (Send did already make ClosePort)
end;

function TTunnelList.TunnelInfo: variant;
var
  dv: TDocVariantData absolute result;
  n, i: PtrInt;
begin
  VarClear(result);
  dv.InitFast(dvArray);
  if fItem = nil  then
    exit;
  fSafe.ReadLock; // non-blocking Read lock
  try
    n := length(fItem);
    dv.Capacity := n;
    for i := 0 to n - 1 do
      dv.AddItem(fItem[i].TunnelInfo);
  finally
    fSafe.ReadUnLock;
  end;
end;


end.

