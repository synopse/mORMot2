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
  mormot.crypt.core,
  mormot.crypt.secure,   // for ICryptCert
  mormot.crypt.ecc256r1, // for ECDHE encryption
  mormot.net.sock;


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
    procedure Send(Session: TTunnelSession; const Frame: RawByteString);
  end;

  /// abstract tunneling service implementation
  ITunnelLocal = interface(IInvokable)
    ['{201150B4-6E28-47A3-AAE5-1335C82B060A}']
    /// match mormot.soa.core IServiceWithCallbackReleased definition
    procedure CallbackReleased(const callback: IInvokable;
      const interfaceName: RawUtf8);
    /// to be called before Open() for proper handshake process
    procedure SetTransmit(const Transmit: ITunnelTransmit);
    /// this is the main method to start tunneling to the Transmit interface
    // - Session, TransmitOptions and AppSecret should match on both sides
    // - if Address has a port, will connect a client socket to this address:port
    // - if Address has no port, will bound its address as an ephemeral port,
    // which is returned as result for proper client connection
    function Open(Session: TTunnelSession; TransmitOptions: TTunnelOptions;
      TimeOutMS: integer; const AppSecret, Address: RawUtf8;
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

  /// define the wire frame layout for TTunnelLocal optional ECDHE handshake
  TTunnelEcdhFrame = packed record
    /// the 128-bit client or server random nonce
    rnd: TAesBlock;
    /// the public key of this side
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
    procedure EcdheHashRandom(var hmac: THmacSha256;
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
    procedure Send(aSession: TTunnelSession; const aFrame: RawByteString);
    /// ITunnelLocal method: to be called before Open()
    procedure SetTransmit(const Transmit: ITunnelTransmit);
    /// ITunnelLocal method: initialize tunnelling process
    // - TransmitOptions will be amended to follow SignCert/VerifyCert properties
    // - if Address has a port, will connect a socket to this address:port
    // - if Address has no port, will bound its address an an ephemeral port,
    // which is returned as result for proper client connection
    function Open(Sess: TTunnelSession; TransmitOptions: TTunnelOptions;
      TimeOutMS: integer; const AppSecret, Address: RawUtf8;
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

function ToText(opt: TTunnelOptions): ShortString; overload;


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


type
  /// implements client-side tunneling service
  // - here 'server' or 'client' side does not have any specific meaning - one
  // should just be at either end of the tunnel
  TTunnelLocalClient = class(TTunnelLocal)
  protected
    function ComputeOptionsFromCert: TTunnelOptions; override;
    procedure EcdheHashRandom(var hmac: THmacSha256;
      const local, remote: TTunnelEcdhFrame); override;
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
  inherited Create({suspended=}false, nil, nil, TSynLog, Make(['tun ', fPort]));
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
              // send the data (optionally encrypted) to the other side
              inc(fSent, length(tmp));
              if fAes[{send:}true] <> nil then
                tmp := fAes[true].EncryptPkcs7(tmp, {ivatbeg=}false);
              if (fTransmit <> nil) and
                 not Terminated then
                fTransmit.Send(fOwner.fSession, tmp);
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

procedure TTunnelLocal.Send(aSession: TTunnelSession; const aFrame: RawByteString);
begin
  // ITunnelTransmit method: when a Frame is received from the relay server
  if fHandshake <> nil then
    fHandshake.Push(aFrame) // during the handshake phase - maybe before Open
  else if fThread <> nil then
    if aSession <> fSession then
      ETunnel.RaiseUtf8('%.Send: session mismatch', [self])
    else
      fThread.OnReceived(aFrame); // regular tunelling process
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

function TTunnelLocal.Open(Sess: TTunnelSession;
  TransmitOptions: TTunnelOptions; TimeOutMS: integer;
  const AppSecret, Address: RawUtf8; out RemotePort: TNetPort): TNetPort;
var
  uri: TUri;
  sock: TNetSocket;
  addr: TNetAddr;
  frame, remote: RawByteString;
  rem: PTunnelLocalHandshake absolute remote;
  loc: TTunnelLocalHandshake;
  key, iv: THash256Rec;
  hmackey, hmaciv: THmacSha256;
  log: ISynLog;
const // port is asymmetrical so not included to the KDF - nor the crc
  KDF_SIZE = SizeOf(loc.Info) - (SizeOf(loc.Info.port) + SizeOf(loc.Info.crc));
begin
  TSynLog.EnterLocal(log, 'Open(%)', [Session], self);
  // validate input parameters
  if (fPort <> 0) or
     (not Assigned(fTransmit)) then
    ETunnel.RaiseUtf8('%.Open invalid call', [self]);
  if not uri.From(Address, '0') then
    ETunnel.RaiseUtf8('%.Open invalid %', [self, Address]);
  RemotePort := 0;
  fSession := Sess;
  TransmitOptions := (TransmitOptions - [toClientSigned, toServerSigned]) +
                     ComputeOptionsFromCert;
  // bind to a local (ephemeral) port
  ClosePort;
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
  fOptions := TransmitOptions;
  try
    // initial handshake: same TTunnelOptions + TTunnelSession from both sides
    loc.Info.magic   := tlmVersion1;
    loc.Info.options := fOptions;
    loc.Info.session := fSession; // is typically an increasing sequence number
    loc.Info.port    := result;
    Random128(@loc.Ecdh.rnd);   // unpredictable
    if toEcdhe in fOptions then // ECDHE in a single round trip
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
    fTransmit.Send(fSession, frame);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: after Send1 len=', [length(frame)], self);
    // server will wait until both sides sent an identical (signed) header
    if not fHandshake.WaitPop(TimeOutMS, nil, remote) then
      ETunnel.RaiseUtf8('Open: handshake timeout on port %', [result]);
    if Assigned(log) then
      log.Log(sllTrace, 'Open: received len=', [length(remote)], self);
    // check the returned header, maybe using the certificate
    if length(remote) < SizeOf(loc) then // may have a signature trailer
      ETunnel.RaiseUtf8('Open: wrong handshake size=% on port %',
        [length(remote), result]);
    if not CompareMemSmall(@rem.Info, @loc.Info, KDF_SIZE) then
      ETunnel.RaiseUtf8('Open: unexpected handshake on port %',
        [length(remote), result]);
    TunnelHandshakeCrc(rem^, AppSecret, key.Lo);
    if not IsEqual(rem^.Info.crc, key.Lo) then
      ETunnel.RaiseUtf8('Open: invalid handshake signature on port %', [result]);
    if not FrameVerify(remote, SizeOf(loc)) then
      ETunnel.RaiseUtf8('Open: handshake failed on port %', [result]);
    RemotePort := rem^.Info.port;
    // optional ephemeral encryption
    FillZero(key.b);
    if toEncrypted * fOptions <> [] then
    begin
      if AppSecret = '' then
        hmackey.Init('705FC9676148405B91A66FFE7C3B54AA') // some minimal key
      else
        hmackey.Init(AppSecret);
      hmackey.Update(@loc.Info, KDF_SIZE); // no replay (sequential session)
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
    FreeAndNil(fHandshake); // ends the handshaking phase
    fPort := result;
    fThread := TTunnelLocalThread.Create(self, fTransmit, key.Lo, iv.Lo, sock);
    SleepHiRes(100, fThread.fStarted);
  except
    sock.ShutdownAndClose(true); // any error would abort and return 0
    result := 0;
  end;
  FreeAndNil(fHandshake);
  FillZero(key.b);
  FillZero(iv.b);
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


function ToText(opt: TTunnelOptions): ShortString;
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



end.

