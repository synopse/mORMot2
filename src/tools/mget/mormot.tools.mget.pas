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
    fLimitBandwidthMB, fWholeRequestTimeoutSec, fTcpTimeoutSec: integer;
    fHeader, fHashValue: RawUtf8;
    fPeerSecret, fPeerSecretHexa: SpiUtf8;
    fClient: THttpClientSocket;
    fPeerCache: IWGetAlternate;
  public
    // input parameters (e.g. from command line) for the MGet process
    Silent, NoResume, TlsIgnoreErrors, Cache, Peer: boolean;
    CacheFolder, TlsCertFile, DestFile: TFileName;
    Log: TSynLogClass;
    /// could be run once input parameters are set, before Execute() is called
    // - will launch THttpPeerCache background process, for instance
    // - do nothing if already already called
    procedure Start;
    /// this is the main processing method
    function Execute(const Url: RawUtf8): TFileName;
    /// finalize this instance
    destructor Destroy; override;
    /// write some message to the console, if Silent flag is false
    procedure ToConsole(const Fmt: RawUtf8; const Args: array of const);
  published
    /// the settings used if Peer is true
    property PeerSettings: THttpPeerCacheSettings
      read fPeerSettings write fPeerSettings;
    // following properties will be published as command line switches
    property customHttpHeader: RawUtf8
      read fHeader write fHeader;
    property hashAlgo: TMGetProcessHash
      read fHashAlgo write fHashAlgo;
    property hashValue: RawUtf8
      read fHashValue write fHashValue;
    property limitBandwidthMB: integer
      read fLimitBandwidthMB write fLimitBandwidthMB;
    property tcpTimeoutSec: integer
      read fTcpTimeoutSec write fTcpTimeoutSec;
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

procedure TMGetProcess.Start;
begin
  if Peer and
     (fPeerCache = nil) then // reuse THttpPeerCache instance between calls
    with Log.Enter(self, 'Start: THttpPeerCache') do
    begin
      if (fPeerSecret = '') and
         (fPeerSecretHexa <> '') then
        fPeerSecret := HexToBin(fPeerSecretHexa);
      try
        fPeerCache := THttpPeerCache.Create(fPeerSettings, fPeerSecret);
        // by now, THttpAsyncServer is incompatible with rfProgressiveStatic
      except
        Peer := false; // disable --peer if something is wrong
      end;
    end;
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
  l := Log.Enter('Execute %', [Url], self);
  Start; // start e.g. background THttpPeerCache process
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
  wget.Clear;
  wget.KeepAlive := 30000;
  wget.Resume := not NoResume;
  wget.Header := fHeader;
  wget.HashFromServer := (h = '') and
                         (algo <> gphAutoDetect);
  if algo <> gphAutoDetect then
  begin
    wget.Hasher := HASH_STREAMREDIRECT[HASH_ALGO[algo]];
    wget.Hash := h;
    if not Silent then
      wget.OnProgress := TStreamRedirect.ProgressStreamToConsole;
  end;
  wget.LimitBandwidth := fLimitBandwidthMB shl 20;
  wget.TimeOutSec := fWholeRequestTimeoutSec;
  // (peer) cache support
  if Cache then
    wget.HashCacheDir := EnsureDirectoryExists(CacheFolder);
  if Peer then
  begin
    wget.Alternate := fPeerCache; // reuse THttpPeerCache on background
    wget.AlternateOptions := fPeerRequest;
  end;
  // make the actual request
  result := '';
  if not uri.From(u) then
    exit;
  if fClient <> nil then
    if (fClient.TLS.Enabled <> uri.Https) or
       (fClient.Server <> uri.Server) or
       (fClient.Port <> uri.Port) then // need a new connection
      FreeAndNil(fClient);
  if fClient = nil then  // try to reuse an existing connection
  begin
    fClient := THttpClientSocket.Create(fTcpTimeoutSec * 1000);
    if Log <> nil then
      fClient.OnLog := Log.DoLog;
    fClient.TLS.IgnoreCertificateErrors := TlsIgnoreErrors;
    fClient.TLS.CertificateFile := TlsCertFile;
    fClient.OpenBind(uri.Server, uri.Port, {bind=}false, uri.Https);
  end;
  result := fClient.WGet(uri.Address, DestFile, wget);
  if Assigned(l) then
    l.Log(sllTrace, 'Execute: WGet=%', [result], self);
end;

destructor TMGetProcess.Destroy;
begin
  inherited Destroy;
  fClient.Free;
end;

procedure TMGetProcess.ToConsole(const Fmt: RawUtf8;
  const Args: array of const);
begin
  if not Silent then
    ConsoleWrite(Fmt, Args);
end;


initialization

end.
