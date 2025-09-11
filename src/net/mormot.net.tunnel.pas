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
  // - toEcdhe computes an ephemeral secret to encrypt the link: combined with
  // toClientSigned/toServerSigned, it offers perfect End-to-end encryption
  // - toClientSigned/toServerSigned will be set by Open() according to the
  // actual certificates available, to ensure endpoint authenticated handshake
  // - if toEcdhe is not set, toEncrypt will ensure a symmetric encryption
  // - only localhost clients are accepted, unless toAcceptNonLocal is set
  TTunnelOption = (
    toEcdhe,
    toClientSigned,
    toServerSigned,
    toEncrypt,
    toAcceptNonLocal);

  /// options for TTunnelLocal process
  TTunnelOptions = set of TTunnelOption;
  PTunnelOptions = ^TTunnelOptions;

  /// a session identifier which should match on both sides of the tunnel
  // - typically a Random32 or a TBinaryCookieGeneratorSessionID value
  TTunnelSession = cardinal;
  PTunnelSession = ^TTunnelSession;

  /// abstract transmission layer with the central relay server
  // - may be implemented as raw sockets or over a mORMot SOA WebSockets link
  // - if toEcdhe or toEncrypt option is set, the frames are already encrypted
  // - named as Tunnel*() methods to be joined as a single service interface,
  // to leverage a single WebSockets callback
  ITunnelTransmit = interface(IInvokable)
    ['{F481A93C-1321-49A6-9801-CCCF065F3973}']
    /// main method to emit the supplied binary Frame to the relay server
    // - the raw binary frame always end with 4 bytes of 32-bit TTunnelSession
    // - no result so that the frames could be gathered e.g. over WebSockets
    // - single binary parameter so that could be transmitted as
    // BINARY_CONTENT_TYPE without any base-64 encoding (to be done at WS level)
    procedure TunnelSend(const Frame: RawByteString);
    /// return some information about this connection(s)
    // - as a TDocVariant object for a single connection, or null from the relay
    function TunnelInfo: variant;
  end;
  PITunnelTransmit = ^ITunnelTransmit;

  /// abstract tunneling service implementation
  ITunnelLocal = interface(ITunnelTransmit)
    ['{201150B4-6E28-47A3-AAE5-1335C82B060A}']
    /// match mormot.soa.core IServiceWithCallbackReleased definition
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
    /// the local port used for the tunnel local process
    function LocalPort: RawUtf8;
    /// the remote port used for the tunnel local process
    function RemotePort: cardinal;
    /// check if the background processing thread is using End-to-end encryption
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
    /// the public key of this side (33 random bytes if toEcdhe is not set)
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
    /// a genuine 32-bit integer ID
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
    fSendSafe: TMultiLightLock; // protect fHandshake+fThread
    fPort, fRemotePort: TNetPort;
    fOptions: TTunnelOptions;
    fFlags: set of (fBound, fClosePortNotified);
    fClosed, fVerboseLog: boolean;
    fThread: TTunnelLocalThread;
    fHandshake: TSynQueue;
    fEcdhe: TEccKeyPair;
    fTransmit: ITunnelTransmit;
    fSignCert, fVerifyCert: ICryptCert;
    fBytesIn, fBytesOut, fFramesIn, fFramesOut: Int64;
    fLogClass: TSynLogClass;
    fStartTicks: cardinal;
    fInfo: TDocVariantData;
    // methods to be overriden according to the client/server side
    procedure IncludeOptionsFromCert; virtual; abstract;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); virtual; abstract;
    // can optionally add a signature to the main handshake frame
    procedure FrameSign(var frame: RawByteString); virtual;
    function FrameVerify(frame: PAnsiChar; framelen, payloadlen: PtrInt): boolean; virtual;
    function GetElapsed: cardinal;
    // can be overriden to customize this class process
    procedure AfterHandshake; virtual;
    procedure OnTunnelInfo(var Info: TDocVariantData); virtual;
  public
    /// initialize the instance for process
    // - if no Context value is supplied, will compute an ephemeral key pair
    // - call Open() to perform actual handshaking and start the background
    // tunnelling thread
    constructor Create(Logger: TSynLogClass = nil;
      SpecificKey: PEccKeyPair = nil); reintroduce;
    /// main method to initialize tunnelling process
    // - Sess genuine integer identifier should match on both sides
    // - Transmit.TunnelSend will be used for sending raw data to the other end
    // - TransmitOptions will be amended to follow SignCert/VerifyCert properties
    // - AppSecret is used during handshake (and toEncrypt with no toEcdhe), and
    // should match on both sides
    // - if Address has a port, will connect a socket to this address:port
    // - if Address has no port, will bound its address an an ephemeral port,
    // which is returned as result for proper client connection
    // - InfoNameValue are name/value pairs of some JSON fields which will be
    // included to ITunnelTransmit.TunnelInfo returned object (e.g. Host name)
    // - SignCert/VerifyCert should have [cuDigitalSignature] usage, and match
    // VerifyCert/SignCert corresponding certificate on other side
    // - should be called only once per TTunnelLocal instance
    function Open(Sess: TTunnelSession; const Transmit: ITunnelTransmit;
      TransmitOptions: TTunnelOptions; TimeOutMS: integer; const AppSecret, Address: RawUtf8;
      const InfoNameValue: array of const; const SignCert: ICryptCert = nil;
      const VerifyCert: ICryptCert = nil): TNetPort;
    /// finalize this instance, and its local TCP server
    destructor Destroy; override;
    /// called e.g. by CallbackReleased() or by Destroy
    procedure ClosePort;
  public
    /// ITunnelTransmit method: when a Frame is received from the relay server
    procedure TunnelSend(const aFrame: RawByteString);
    /// ITunnelTransmit method: return a TDocVariant object about this connection
    function TunnelInfo: variant;
    /// ITunnelLocal method: return the local port
    function LocalPort: RawUtf8;
    /// ITunnelLocal method: return the remote port
    function RemotePort: cardinal;
    /// ITunnelLocal method: check if the background thread uses E2EE
    function Encrypted: boolean;
    /// ITunnelLocal method: when a ITunnelTransmit remote callback is finished
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
    /// access the logging features of this class
    property LogClass: TSynLogClass
      read fLogClass write fLogClass;
    /// log each received frame length for raw debugging
    property VerboseLog: boolean
      read fVerboseLog write fVerboseLog;
    /// raw access to the transmission method - used during testing
    property RawTransmit: ITunnelTransmit
      read fTransmit write fTransmit;
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
    /// how many bytes have been received
    property BytesIn: Int64
      read fBytesIn;
    /// how many bytes have been sent
    property BytesOut: Int64
      read fBytesOut;
    /// how many frames have been received
    property FramesIn: Int64
      read fFramesIn;
    /// how many frames have been sent
    property FramesOut: Int64
      read fFramesOut;
    /// number of seconds elapsed since Open()
    property Elapsed: cardinal
      read GetElapsed;
    /// equals true after ClosePort
    property Closed: boolean
      read fClosed;
  end;

function ToText(opt: TTunnelOptions): ShortString; overload;

/// extract the 32-bit session trailer from a ITunnelTransmit.TunnelSend() frame
function FrameSession(const Frame: RawByteString): TTunnelSession;
  {$ifdef HASINLINE} inline; {$endif}

const
  toEncrypted = [toEcdhe, toEncrypt];


{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

type
  /// implements server-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel - but they must appear in a
  // coherent order during the handshake phase by using those overriden methods
  TTunnelLocalServer = class(TTunnelLocal)
  protected
    procedure IncludeOptionsFromCert; override;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); override;
  end;

  /// implements client-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel - but they must appear in a
  // coherent order during the handshake phase by using those overriden methods
  TTunnelLocalClient = class(TTunnelLocal)
  protected
    procedure IncludeOptionsFromCert; override;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); override;
  end;

  /// maintain a list of ITunnelTransmit instances
  // - is itself a ITunnelTransmit instance, to redirect TunnelSend() frames
  TTunnelList = class(TInterfacedPersistent,
    ITunnelTransmit)
  protected
    fSafe: TRWLightLock;
    fInfoCacheSafe: TLightLock;
    fItem: array of ITunnelTransmit;
    fSession: TIntegerDynArray; // store TTunnelSession (=cardinal) values
    fCount: integer;
    fInfoCacheTix32: cardinal;
    fInfoCache: variant;
  public
    /// append one ITunnelTransmit callback to the list
    function Add(aSession: TTunnelSession;
      const aInstance: ITunnelTransmit): boolean;
    /// remove one ITunnelTransmit from its session ID
    function Delete(aSession: TTunnelSession): boolean;
    /// search if one ITunnelTransmit matches a session ID
    function Exists(aSession: TTunnelSession): boolean;
    /// ask the TunnelInfo of a given session ID as TDocVariant object
    procedure GetInfo(aSession: TTunnelSession; out aInfo: variant);
    /// ask all TunnelInfo of all opended sessions as TDocVariant array
    // - with a one second cache
    // - not published by default over ITunnelTransmit.TunnelInfo for safety
    procedure GetAllInfo(out aInfo: variant);
  public
    /// ITunnelTransmit method which will redirect the given frame to the
    // expected registered ITunnelTransmit instance
    // - if the Frame does not match any known session, just do nothing
    procedure TunnelSend(const Frame: RawByteString); virtual;
    /// ITunnelTransmit method: return null for safety
    // - may be overriden to return GetAllInfo() result
    function TunnelInfo: variant; virtual;
  end;



implementation


{ ******************** Abstract Definitions for Port Forwarding }

const
  TRAIL_SIZE = SizeOf(TTunnelSession);

function ToText(opt: TTunnelOptions): ShortString;
begin
  GetSetNameShort(TypeInfo(TTunnelOptions), opt, result, {trim=}true);
  LowerCaseShort(result);
end;

function FrameSession(const Frame: RawByteString): TTunnelSession;
var
  l: PtrInt;
begin
  l := length(Frame) - TRAIL_SIZE;
  if l >= 0 then
    result := PTunnelSession(@PByteArray(Frame)[l])^
  else
    result := 0;
end;


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
    fAes[{sending:}false] := AesIvUpdatedCreate(mCtr, key, 128, @iv);
    fAes[{sending:}true]  := AesIvUpdatedCreate(mCtr, key, 128, @iv);
    // won't include an IV with each frame, but update it after each frame
  end;
  fServerSock := sock;
  FreeOnTerminate := true;
  inherited Create({susp=}false, nil, nil, fOwner.fLogClass, Make(['tun', fPort]));
end;

destructor TTunnelLocalThread.Destroy;
begin
  Terminate;
  if fOwner <> nil then
    fOwner.fThread := nil;
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
     (Frame = '') then
    exit;
  if fClientSock = nil then // may occur with direct calls
  begin
    if SleepOrTerminated(100) or // let the socket be accepted()
       (fClientSock = nil) then
    begin
      fLog.Log(sllDebug, 'OnReceived: no ClientSock', self);
      exit;
    end;
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
    inc(fOwner.fBytesIn, length(data));
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
       (fBound in fOwner.fFlags) then
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
              // emit the (encrypted) data with a 32-bit TTunnelSession trailer
              if fOwner <> nil then
                inc(fOwner.fBytesOut, length(tmp)); // size before encryption
              if fAes[{send:}true] <> nil then
                tmp := fAes[true].EncryptPkcs7(tmp, {ivatbeg=}false, TRAIL_SIZE)
              else
                SetLength(tmp, length(tmp) + TRAIL_SIZE);
              PTunnelSession(@PByteArray(tmp)[length(tmp) - TRAIL_SIZE])^ := fSession;
              if (fTransmit <> nil) and
                 not Terminated then
              begin
                if fOwner <> nil then
                  inc(fOwner.fFramesOut);
                fTransmit.TunnelSend(tmp);
              end;
            end;
        else
          ETunnel.RaiseUtf8('%.Execute(%): error % at receiving',
            [self, fPort, ToText(res)^]);
        end;
      end;
  except
    on E: Exception do
    try
      fLog.Log(sllWarning, 'DoExecute: aborted due to %', [self, PClass(E)^], self);
      if fOwner <> nil then
        fOwner.ClosePort;
    except
      on E2: Exception do
        fLog.Log(sllWarning, 'DoExecute: nested %', [PClass(E2)^], self);
    end;
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
  FreeAndNil(fHandshake); // if Open() was not called
end;

procedure TTunnelLocal.ClosePort;
var
  thread: TTunnelLocalThread;
  frame: RawByteString; // notification frame to unregister to the other side
  callback: TNetSocket; // touch-and-go to the server to release main Accept()
  log: ISynLog;
begin
  if self = nil then
    exit;
  fLogClass.EnterLocal(log, 'ClosePort %', [fPort], self);
  fSendSafe.Lock; // protect fHandshake+fThread
  try
    if not (fClosePortNotified in fFlags) then
      try
        include(fFlags, fClosePortNotified);
        if Assigned(log) then
          log.Log(sllTrace, 'ClosePort: notify other end', self);
        PTunnelSession(FastNewRawByteString(frame, TRAIL_SIZE))^ := fSession;
        if Assigned(fTransmit) then
        begin
          inc(fFramesOut);
          fTransmit.TunnelSend(frame);
        end;
      except
      end;
    thread := fThread;
    if thread <> nil then
      try
        fThread := nil;
        thread.fOwner := nil;
        thread.Terminate;
        if thread.fState = stAccepting then
        begin
          if Assigned(log) then
            log.Log(sllDebug, 'ClosePort: release accept', self);
          if NewSocket(cLocalhost, UInt32ToUtf8(fPort), nlTcp,
             {dobind=}false, 10, 0, 0, 0, callback) = nrOK then
            // Windows socket may not release Accept() until connected
            callback.ShutdownAndClose({rdwr=}false);
        end;
      except
      end;
  finally
    fSendSafe.UnLock;
  end;
  if Assigned(log) then
    log.Log(sllTrace, 'ClosePort: %', [self]); // final statistics
  fPort := 0;
  fClosed := true;
end;

procedure TTunnelLocal.TunnelSend(const aFrame: RawByteString);
var
  l: PtrInt;
  p: PAnsiChar;
begin
  // ITunnelTransmit method: when a Frame is received from the relay server
  l := length(aFrame);
  if fVerboseLog then
    fLogClass.Add.Log(sllTrace, 'TunnelSend=%', [l]);
  dec(l, TRAIL_SIZE);
  if l < 0 then
    ETunnel.RaiseUtf8('%.Send: unexpected size=%', [self, l]);
  fSendSafe.Lock; // protect fHandshake+fThread
  try
    inc(fFramesIn);
    if fHandshake <> nil then
    begin
      fLogClass.Add.Log(sllTrace, 'TunnelSend: into fHandshake', self);
      fHandshake.Push(aFrame); // during the handshake phase - maybe before Open
      exit;
    end;
    p := pointer(aFrame);
    if PTunnelSession(p + l)^ <> fSession then
      ETunnel.RaiseUtf8('%.Send: session mismatch', [self]);
    if l = 0 then
    begin
      include(fFlags, fClosePortNotified); // notified by the other end
      ClosePort;
    end
    else if fThread <> nil then // = nil after ClosePort (too late)
    begin
      PStrLen(p - _STRLEN)^ := l; // trim 32-bit session trailer
      fThread.OnReceived(aFrame); // regular tunelling process
    end
    else
      fLogClass.Add.Log(sllDebug, 'TunnelSend: Thread=nil', self); // unlikely
  finally
    fSendSafe.UnLock;
  end;
end;

procedure TTunnelLocal.CallbackReleased(const callback: IInvokable;
  const interfaceName: RawUtf8);
begin
  if not IdemPChar(pointer(interfaceName), 'ITUNNEL') then
    exit; // should be ITunnelLocal or ITunnelTransmit
  include(fFlags, fClosePortNotified); // no need to notify the remote end
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

function TTunnelLocal.GetElapsed: cardinal;
begin
  if (self = nil) or
     (fStartTicks = 0) then
    result := 0
  else
    result := GetUptimeSec - fStartTicks; // in seconds
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

function TTunnelLocal.Open(Sess: TTunnelSession; const Transmit: ITunnelTransmit;
  TransmitOptions: TTunnelOptions; TimeOutMS: integer; const AppSecret, Address: RawUtf8;
  const InfoNameValue: array of const; const SignCert, VerifyCert: ICryptCert): TNetPort;
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
  hmac, hmac2: THmacSha256;
  hqueue: TSynQueue;
  thread: TTunnelLocalThread;
  log: ISynLog;
const // port is asymmetrical so not included to the KDF - nor the crc
  KDF_SIZE = SizeOf(loc.Info) - (SizeOf(loc.Info.port) + SizeOf(loc.Info.crc));
  SUFFIX_SIZE = TRAIL_SIZE + SizeOf(word);
begin
  // validate input parameters
  fRemotePort := 0;
  fInfo.Clear;
  fSession := Sess;
  fSignCert := SignCert;
  fVerifyCert := VerifyCert;
  fOptions := TransmitOptions - [toClientSigned, toServerSigned];
  IncludeOptionsFromCert; // adjust from fSignCert/fVerifyCert
  if fLogClass <> nil then
    fLogClass.EnterLocal(log, 'Open(%,[%])',
      [Sess, ToText(fOptions)], self);
  if (fPort <> 0) or
     (not Assigned(Transmit)) then
    ETunnel.RaiseUtf8('%.Open invalid call', [self]);
  if not uri.From(Address, '0') then
    ETunnel.RaiseUtf8('%.Open invalid %', [self, Address]);
  fTransmit := Transmit;
  // bind to a local (ephemeral) port
  if (fThread <> nil) or
     (fHandshake = nil) then
    ETunnel.RaiseUtf8('%.Open called twice', [self]);
  fPort := 0;
  fFlags := [];
  result := uri.PortInt;
  if result = 0 then
  begin
    // bind on port='0' = ephemeral port
    ENetSock.Check(NewSocket(uri.Server, uri.Port, nlTcp, {bind=}true,
      TimeOutMS, TimeOutMS, TimeOutMS, {retry=}0, sock, @addr), 'Open');
    result := addr.Port;
    if Assigned(log) then
      log.Log(sllTrace, 'Open: bound to %', [addr.IPShort(true)], self);
    include(fFlags, fBound);
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
  thread := nil;
  infoaes := nil;
  try
    // header with optional ECDHE
    loc.Info.magic   := tlmVersion1;
    loc.Info.options := fOptions;
    loc.Info.session := fSession; // is typically an increasing sequence number
    loc.Info.port    := result;
    Random128(@loc.Ecdh.rnd);     // unpredictable
    if toEcdhe in fOptions then
    begin
      if IsZero(fEcdhe.pub) then  // ephemeral key was not specified at Create
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
        hmac.Init('705FC9676148405B91A66FFE7C3B54AA') // some minimal key
      else
        hmac.Init(AppSecret);
      hmac.Update(@loc.Info, KDF_SIZE); // no replay (sequential session)
      hmac2 := hmac; // use hmac2 to compute info KDF
      hmac2.Done(key.b);
      infoaes := TAesCtr.Create(key.Lo, 128, @key.Hi); // simple encryption
      info := infoaes.EncryptPkcs7(info, {ivatbeg=}false);
      infoaes.IV := key.Hi; // use the same IV for decoding "remote" info below
    end;
    li := length(info);
    if li > 65535 then
      ETunnel.RaiseUtf8('Open: too much info (len=%)', [li]);
    // actually send the frame ending with its session ID
    Append(frame, [info, '010123']); // li:W sess:U32 = SUFFIX_SIZE
    l := length(frame);
    PWord(@PByteArray(frame)^[l - SUFFIX_SIZE])^ := li;
    PTunnelSession(@PByteArray(frame)^[l - TRAIL_SIZE])^ := fSession;
    inc(fFramesOut);
    fTransmit.TunnelSend(frame);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: sent % - wait for answer', [length(frame)], self);
    // this method will wait until both sides sent a valid signed header
    if not fHandshake.WaitPop(TimeOutMS, nil, remote) then
      ETunnel.RaiseUtf8('Open: handshake %ms timeout on port %', [TimeOutMS, result]);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: received len=%', [length(remote)], self);
    // ensure the returned frame is for this session
    if FrameSession(remote) <> fSession then
      ETunnel.RaiseUtf8('Open: wrong handshake trailer on port %', [result]);
    // extract (and potentially decrypt) the associated JSON info payload
    l := length(remote) - SUFFIX_SIZE;
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
      // hmac has been pre-computed above with loc.Info and AppSecret
      EcdheHashRandom(hmac, loc.Ecdh, rem^.Ecdh); // rnd+pub in same order
      if toEcdhe in fOptions then
      begin
        if Assigned(log) then
          log.Log(sllTrace, 'Open: compute ECDHE shared secret', self);
        if not Ecc256r1SharedSecret(rem^.Ecdh.pub, fEcdhe.priv, key.b) then
          exit;
        hmac.Update(key.b); // prime256v1 shared secret
      end;
      hmac2 := hmac;     // two labeled hmacs - see NIST SP 800-108
      hmac.Update('AES key'#0);
      hmac.Done(key.b);   // AES-128-CTR key
      hmac2.Update('IV'#1);
      hmac2.Done(iv.b);     // AES-128-CTR iv
    end;
    // launch the background processing thread
    fPort := result;
    thread := TTunnelLocalThread.Create(self, fTransmit, key.Lo, iv.Lo, sock);
    SleepHiRes(100, thread.fStarted);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: started=% %',
        [BOOL_STR[thread.fStarted], thread], self);
    fStartTicks := GetUptimeSec; // wall clock
    hqueue := fHandshake;
    fSendSafe.Lock; // re-entrant for TunnelSend()
    try
      fThread := thread;   // starts the normal tunnelling phase
      fHandshake := nil;   // ends the handshaking phase
      while hqueue.Pop(frame) do
      begin
        if Assigned(log) then
          log.Log(sllDebug, 'Open: delayed frame len=%', [length(frame)], self);
        TunnelSend(frame); // paranoid: redirect to this instance
      end;
    finally
      fSendSafe.UnLock;
      hqueue.Free;
    end;
    // now everything is running and we can finish by preparing the fixed info
    fInfo.AddNameValuesToObject([
      'remotePort', fRemotePort,
      'localPort',  fPort,
      'started',    NowUtcToString,
      'session',    Int64(fSession),
      'encrypted',  Encrypted,
      'options',    ToText(fOptions)]);
    AfterHandshake;
  except
    sock.ShutdownAndClose(true); // any error would abort and return 0
    result := 0;
  end;
  infoaes.Free;
  FillZero(key.b);
  FillZero(iv.b);
end;

procedure TTunnelLocal.AfterHandshake;
begin
  // do nothing by default, but could perform some custom process e.g. on fInfo
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

procedure TTunnelLocal.OnTunnelInfo(var Info: TDocVariantData);
begin
  // do nothing by default, but could remove or add some fields
end;

function TTunnelLocal.TunnelInfo: variant;
var
  dv: TDocVariantData absolute result;
begin
  VarClear(result);
  if fPort = 0 then
    exit;
  dv.InitFast(fInfo.Count + 7, dvObject);
  dv.AddFrom(fInfo);         // fixed values
  dv.AddNameValuesToObject([ // evolving values
    'elapsed',   GetElapsed,
    'bytesIn',   fBytesIn,
    'bytesOut',  fBytesOut,
    'framesIn',  fFramesIn,
    'framesOut', fFramesOut]);
  OnTunnelInfo(dv);
end;


{ ******************** Local NAT Client/Server to Tunnel TCP Streams }

{ TTunnelLocalServer }

procedure TTunnelLocalServer.IncludeOptionsFromCert;
begin
  if Assigned(fVerifyCert) then
    include(fOptions, toClientSigned); // client signature
  if Assigned(fSignCert) then
    include(fOptions, toServerSigned); // server signature
end;

procedure TTunnelLocalServer.EcdheHashRandom(var hmac: THmacSha256;
  const local, remote: TTunnelEcdhFrame);
begin
  hmac.Update(@remote.rnd, SizeOf(remote.rnd)); // client random
  hmac.Update(@local. rnd, SizeOf(local.rnd));  // server random
end;


{ TTunnelLocalClient }

procedure TTunnelLocalClient.IncludeOptionsFromCert;
begin
  if Assigned(fSignCert) then
    include(fOptions, toClientSigned); // client signature
  if Assigned(fVerifyCert) then
    include(fOptions, toServerSigned); // server signature
end;

procedure TTunnelLocalClient.EcdheHashRandom(var hmac: THmacSha256;
  const local, remote: TTunnelEcdhFrame);
begin
  hmac.Update(@local. rnd, SizeOf(local.rnd));  // client random
  hmac.Update(@remote.rnd, SizeOf(remote.rnd)); // server random
end;


{ TTunnelList }

function TTunnelList.Exists(aSession: TTunnelSession): boolean;
begin
  fSafe.ReadLock;
  try
    result := IntegerScanExists(pointer(fSession), fCount, aSession);
  finally
    fSafe.ReadUnLock;
  end;
end;

function TTunnelList.Add(aSession: TTunnelSession;
  const aInstance: ITunnelTransmit): boolean;
begin
  result := false;
  if (aInstance = nil) or
     (aSession = 0) then
    exit;
  fSafe.WriteLock;
  try
    if IntegerScanExists(pointer(fSession), fCount, aSession) then
      exit;
    AddInteger(fSession, fCount, aSession);
    InterfaceArrayAdd(fItem, aInstance);
  finally
    fSafe.WriteUnLock;
  end;
  result := true;
end;

function TTunnelList.Delete(aSession: TTunnelSession): boolean;
var
  ndx: PtrInt;
  instance: ITunnelTransmit;
begin
  result := false;
  if (aSession = 0) or
     (fCount = 0) then
    exit;
  fSafe.WriteLock;
  try
    ndx := IntegerScanIndex(pointer(fSession), fCount, aSession);
    if (ndx < 0) or
       not InterfaceArrayExtract(fItem, ndx, instance) then // weak copy
      exit;
    DeleteInteger(fSession, fCount, ndx);
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
  if (s = 0) or   // invalid frame
     (fCount = 0) then
    exit;
  fSafe.ReadLock; // non-blocking Read lock
  try
    ndx := IntegerScanIndex(pointer(fSession), fCount, s); // use SSE2 asm
    if ndx < 0 then
      exit; // just skip the frame if the session does not exist (anti-fuzzing)
    fItem[ndx].TunnelSend(frame); // call ITunnelTransmit method within ReadLock
  finally
    fSafe.ReadUnLock;
  end;
  // handle end of process notification from the other side
  if length(Frame) = TRAIL_SIZE then
    Delete(s); // remove this instance (Send did already make ClosePort)
end;

procedure TTunnelList.GetInfo(aSession: TTunnelSession; out aInfo: variant);
var
  ndx: PtrInt;
begin
  if (aSession = 0) or
     (fCount = 0) then
    exit;
  fSafe.ReadLock;
  try
    ndx := IntegerScanIndex(pointer(fSession), fCount, aSession);
    if ndx >= 0 then
      aInfo := fItem[ndx].TunnelInfo; // ask the remote endpoint
  finally
    fSafe.ReadUnLock;
  end;
end;

function TTunnelList.TunnelInfo: variant;
begin
  VarClear(result); // return nothing by default for safety - see GetAllInfo()
end;

procedure TTunnelList.GetAllInfo(out aInfo: variant);
var
  dv: TDocVariantData absolute aInfo;
  n, i: PtrInt;
  tix32: cardinal;
begin
  if fCount = 0  then
    exit;
  tix32 := GetTickSec;
  fInfoCacheSafe.Lock;
  if tix32 = fInfoCacheTix32 then // cache last resultset for one second
    aInfo := fInfoCache;
  fInfoCacheSafe.UnLock;
  if dv.Count <> 0 then
    exit;
  dv.InitFast(dvArray);
  fSafe.ReadLock; // non-blocking Read lock
  try
    n := length(fItem);
    dv.Capacity := n;
    for i := 0 to n - 1 do
      dv.AddItem(fItem[i].TunnelInfo); // call all remote endpoints
  finally
    fSafe.ReadUnLock;
  end;
  fInfoCacheSafe.Lock;
  fInfoCacheTix32 := tix32;
  fInfoCache := aInfo;
  fInfoCacheSafe.UnLock;
end;

end.

