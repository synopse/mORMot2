/// Main Process of the Command Line "mORMot GET" tool
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.tools.mget;


interface

{$I ..\..\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.buffers,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.log,
  mormot.core.data,
  mormot.crypt.secure,
  mormot.crypt.core,
  mormot.net.sock,
  mormot.net.client,
  mormot.net.server;


type
  TMGetProcessHash = (
    gphAutoDetect,
    gphMd5,
    gphSha1,
    gphSha256,
    gphSha384,
    gphSha512,
    gphSha3_256,
    gphSha3_512);

  /// state engine for mget processing
  // - just a wrapper around THttpClientSocket and THttpPeerCache
  // - published properties will be included as command line switches, using RTTI
  // - could be reused between the mget command line tool and an eventual GUI
  TMGetProcess = class(TPersistentAutoCreateFields)
  protected
    fPeerSettings: THttpPeerCacheSettings;
    fHashAlgo: TMGetProcessHash;
    fPeerRequest: TWGetAlternateOptions;
    fLimitBandwidthMB, fWholeRequestTimeoutSec: integer;
    fHeader, fHashValue, fPeerCacheInterface: RawUtf8;
    fPeerSecret, fPeerSecretHexa: SpiUtf8;
    fClient: THttpClientSocket;
    fOnProgress: TOnStreamProgress;
    fOnPeerCacheDirectOptions: TOnHttpPeerCacheDirectOptions;
    fOnStep: TOnWGetStep;
    fOutSteps: TWGetSteps;
    fPeerCache: IWGetAlternate;
    function GetTcpTimeoutSec: integer;
    procedure SetTcpTimeoutSec(Seconds: integer);
    // could be overriden to change the behavior of this class
    procedure PeerCacheStarted({%H-}PeerInstance: THttpPeerCache); virtual;
    procedure PeerCacheStopping; virtual;
    procedure BeforeClientConnect(var {%H-}Uri: TUri); virtual;
    procedure AfterClientConnect; virtual;
    procedure BeforeClientGet(var {%H-}Uri: TUri; var {%H-}WGet: THttpClientSocketWGet); virtual;
    procedure AfterClientGet(var {%H-}Uri: TUri; var {%H-}WGet: THttpClientSocketWGet); virtual;
  public
    // input parameters (e.g. from command line) for the MGet process
    Silent, NoResume, Cache, Peer, LogSteps, TrackNetwork: boolean;
    CacheFolder, DestFile: TFileName;
    Options: THttpRequestExtendedOptions;
    Log: TSynLogClass;
    ServerTls, ClientTls: TNetTlsContext;
    /// initialize this instance with the default values
    constructor Create; override;
    /// finalize this instance
    destructor Destroy; override;
    /// could be run once input parameters are set, before Execute() is called
    // - will launch THttpPeerCache background process, and re-create it if
    // the network layout did change (if TrackNetwork is true)
    // - do nothing if Peer is false, or if the THttpPeerCache instance is fine
    procedure StartPeerCache;
    /// this is the main processing method
    function Execute(const Url: RawUtf8): TFileName;
    /// write some message to the console, if Silent flag is false
    procedure ToConsole(const Fmt: RawUtf8; const Args: array of const);
    /// encode a remote URI for pcoHttpDirect download at localhost
    // - returns aDirectUri e.g. as 'http://1.2.3.4:8099/https/microsoft.com/...'
    // (if peer cache runs on 1.2.3.4:8099) and its associated aDirectHeaderBearer
    function HttpDirectUri(const aRemoteUri, aRemoteHash: RawUtf8;
      out aDirectUri, aDirectHeaderBearer: RawUtf8; aPermanent: boolean = false;
      aOptions: PHttpRequestExtendedOptions = nil): boolean;
    /// access to the associated THttpPeerCache instance
    // - a single peer-cache is run in the background between Execute() calls
    // - equals nil if this instance Peer property is false
    property PeerCache: IWGetAlternate
      read fPeerCache;
    /// the 'ip:port' of the running THttpPeerCache instance, '' if none
    property PeerCacheInterface: RawUtf8
      read fPeerCacheInterface;
    /// optional callback event called during download process
    property OnProgress: TOnStreamProgress
      read fOnProgress write fOnProgress;
    /// optional event to customize the access of a given URI in pcoHttpDirect mode
    property OnPeerCacheDirectOptions: TOnHttpPeerCacheDirectOptions
      read fOnPeerCacheDirectOptions write fOnPeerCacheDirectOptions;
    /// optional callback event raised during WGet() process
    // - if OutSteps: TWGetSteps field and LogSteps boolean flag are not enough
    // - alternative for business logic tracking: the OnProgress callback is
    // more about periodic human interaction in GUI or console
    property OnStep: TOnWGetStep
      read fOnStep write fOnStep;
    /// after Execute(), contains a set of all processed steps
    property OutSteps: TWGetSteps
      read fOutSteps;
  published
    /// the settings used if Peer is true
    property PeerSettings: THttpPeerCacheSettings
      read fPeerSettings write fPeerSettings;
    // following properties will be published as command line switches
    property customHttpHeader: RawUtf8
      read fHeader write fHeader;
    property proxyUri: RawUtf8
      read Options.Proxy write Options.Proxy;
    property redirectMax: integer
      read Options.RedirectMax write Options.RedirectMax;
    property hashAlgo: TMGetProcessHash
      read fHashAlgo write fHashAlgo;
    property hashValue: RawUtf8
      read fHashValue write fHashValue;
    property limitBandwidthMB: integer
      read fLimitBandwidthMB write fLimitBandwidthMB;
    property tcpTimeoutSec: integer
      read GetTcpTimeoutSec write SetTcpTimeoutSec;
    property wholeRequestTimeoutSec: integer
      read fWholeRequestTimeoutSec write fWholeRequestTimeoutSec;
    property peerSecret: SpiUtf8
      read fPeerSecret write fPeerSecret;
    property peerSecretHexa: SpiUtf8
      read fPeerSecretHexa write fPeerSecretHexa;
    property peerRequest: TWGetAlternateOptions
      read fPeerRequest write fPeerRequest;
  end;


implementation

const
  HASH_ALGO: array[gphMd5 .. high(TMGetProcessHash)] of THashAlgo = (
    hfMd5,
    hfSha1,
    hfSha256,
    hfSha384,
    hfSha512,
    hfSha3_256,
    hfSha3_512);

function GuessAlgo(const HashHexa: RawUtf8): TMGetProcessHash;
var
  l: integer;
begin
  l := length(HashHexa) shr 1; // from hexa to bytes
  for result := low(HASH_ALGO) to high(HASH_ALGO) do
    if HASH_SIZE[HASH_ALGO[result]] = l then
      exit; // detect first exact matching size (not SHA-3)
  result := gphAutoDetect;
end;


{ TMGetProcess }

function TMGetProcess.GetTcpTimeoutSec: integer;
begin
  result := Options.CreateTimeoutMS div 1000;
end;

procedure TMGetProcess.SetTcpTimeoutSec(Seconds: integer);
begin
  Options.CreateTimeoutMS := Seconds * 1000;
end;

constructor TMGetProcess.Create;
begin
  inherited Create;
  Options.RedirectMax := 5;
end;

destructor TMGetProcess.Destroy;
begin
  inherited Destroy;
  fClient.Free;
  FillZero(fPeerSecret);
  FillZero(fPeerSecretHexa);
  if fPeerCache <> nil then
    PeerCacheStopping;
end;

procedure TMGetProcess.StartPeerCache;
var
  l: ISynLog;
  peerinstance: THttpPeerCache;
begin
  if not Peer then
    exit;
  // first check if the network interface changed
  if fPeerCache = nil then
    MacIPAddressFlush // force reload network interfaces from OS API at startup
  else if TrackNetwork and
          fPeerCache.NetworkInterfaceChanged then
  begin
    Log.EnterLocal(l, self, 'StartPeerCache: NetworkInterfaceChanged');
    PeerCacheStopping;
    fPeerCache := nil; // release IWGetAlternate to force re-create just below
    fPeerCacheInterface := '';
    l := nil;
  end;
  // (re)create the peer-cache background process if necessary
  if fPeerCache <> nil then
    exit;
  Log.EnterLocal(l, self, 'StartPeerCache: THttpPeerCache.Create');
  if (fPeerSecret = '') and
     (fPeerSecretHexa <> '') then
    fPeerSecret := HexToBin(fPeerSecretHexa);
  try
    peerinstance := THttpPeerCache.Create(fPeerSettings, fPeerSecret,
      nil, 2, self.Log, @ServerTls, @ClientTls);
    fPeerCache := peerinstance; // stored as IWGetAlternate
    fPeerCacheInterface := peerinstance.IpPort;
    peerinstance.OnDirectOptions := fOnPeerCacheDirectOptions;
    // THttpAsyncServer could also be tried with rfProgressiveStatic
    PeerCacheStarted(peerinstance); // may be overriden
  except
    // don't disable Peer: we would try on next Execute()
    on E: Exception do
      Log.Add.Log(sllDebug,
        'StartPeerCache raised %: will retry next time', [PClass(E)^]);
  end;
end;

procedure TMGetProcess.PeerCacheStarted(PeerInstance: THttpPeerCache);
begin
  // do nothing
end;

procedure TMGetProcess.PeerCacheStopping;
begin
  // do nothing
end;

procedure TMGetProcess.BeforeClientConnect(var Uri: TUri);
begin
  // do nothing
end;

procedure TMGetProcess.AfterClientConnect;
begin
  // do nothing
end;

procedure TMGetProcess.BeforeClientGet(var Uri: TUri; var WGet: THttpClientSocketWGet);
begin
  // do nothing
end;

procedure TMGetProcess.AfterClientGet(var Uri: TUri; var WGet: THttpClientSocketWGet);
begin
  // do nothing
end;

function TMGetProcess.Execute(const Url: RawUtf8): TFileName;
var
  wget: THttpClientSocketWGet;
  u, h: RawUtf8;
  algo: TMGetProcessHash; // may change with next Url
  uri: TUri;
  l: ISynLog;
begin
  // prepare the process
  Log.EnterLocal(l, 'Execute %', [Url], self);
  // (re)start background THttpPeerCache process if needed
  StartPeerCache;
  // identify e.g. 'xxxxxxxxxxxxxxxxxxxx@http://toto.com/res'
  if not Split(Url, '@', h, u) or
     (GuessAlgo(h) = gphAutoDetect) or
     (HexToBin(h) = '') then // ignore https://user:password@server:port/addr
  begin
    u := Url;
    h := hashValue;
  end;
  // guess the hash algorithm from its hexadecimal value size
  algo := hashAlgo;
  if algo = gphAutoDetect then
    if h <> '' then
      algo := GuessAlgo(h)
    else if Peer then
      algo := gphSha256;
  // set the WGet additional parameters
  fOutSteps := [];
  wget.Clear;
  wget.KeepAlive := 30000;
  wget.Resume := not NoResume;
  wget.Header := fHeader;
  wget.HashFromServer := (h = '') and
                         (algo <> gphAutoDetect);
  if Assigned(fOnProgress) then
    wget.OnProgress := fOnProgress; // periodic human friendly state change
  if Assigned(fOnStep) then
    wget.OnStep := fOnStep;         // logical state change
  if LogSteps and
     (Log <> nil) then
    wget.LogSteps := Log.DoLog; // may be in complement to OnStep
  if algo <> gphAutoDetect then
  begin
    wget.Hasher := HASH_STREAMREDIRECT[HASH_ALGO[algo]];
    wget.Hash := h;
    if not Silent then
      if not Assigned(wget.OnProgress) then
        wget.OnProgress := TStreamRedirect.ProgressStreamToConsole;
  end;
  wget.LimitBandwidth := fLimitBandwidthMB shl 20;
  wget.TimeOutSec := fWholeRequestTimeoutSec;
  // (peer) cache support
  if Cache then
    wget.HashCacheDir := EnsureDirectoryExists(CacheFolder);
  if Peer then
  begin
    wget.Alternate := fPeerCache; // reuse background THttpPeerCache
    wget.AlternateOptions := fPeerRequest;
  end;
  // make the actual request
  result := '';
  if not uri.From(u) then
    exit;
  if fClient <> nil then
    if not fClient.SameOpenOptions(uri, Options) then // need a new connection
      FreeAndNil(fClient);
  if fClient = nil then  // if we can't reuse the existing connection
  begin
    BeforeClientConnect(uri);
    fClient := THttpClientSocket.OpenOptions(uri, Options);
    if Log <> nil then
      fClient.OnLog := Log.DoLog;
    AfterClientConnect;
  end;
  BeforeClientGet(uri, wget);
  result := fClient.WGet(uri.Address, DestFile, wget);
  AfterClientGet(uri, wget);
  fOutSteps := wget.OutSteps;
  if Assigned(l) then
    l.Log(sllTrace, 'Execute: WGet=% [%]',
      [result, GetSetName(TypeInfo(TWGetSteps), fOutSteps, {trim=}true)], self);
end;

procedure TMGetProcess.ToConsole(const Fmt: RawUtf8;
  const Args: array of const);
begin
  if not Silent then
    ConsoleWrite(Fmt, Args);
end;

function TMGetProcess.HttpDirectUri(const aRemoteUri, aRemoteHash: RawUtf8;
  out aDirectUri, aDirectHeaderBearer: RawUtf8; aPermanent: boolean;
  aOptions: PHttpRequestExtendedOptions): boolean;
var
  secret: RawUtf8;
begin
  result := false;
  if self = nil then
    exit;
  secret := fPeerSecret;
  if secret = '' then
    if fPeerSecretHexa = '' then
      exit
    else
      secret := HexToBin(fPeerSecretHexa);
  result := fPeerSettings.HttpDirectUri(secret, aRemoteUri, aRemoteHash,
    aDirectUri, aDirectHeaderBearer, ServerTls.Enabled, aPermanent, aOptions);
  FillZero(secret);
end;


initialization

end.
