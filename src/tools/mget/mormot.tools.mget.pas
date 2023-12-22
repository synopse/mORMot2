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
    gphAutoDetect, gphMd5, gphSha1, gphSha256, gphSha384, gphSha512, gphSha3_256);

  /// state engine for mget processing
  // - could be reused between the mget command line tool and an eventual GUI
  TMGetProcess = class(TPersistentAutoCreateFields)
  protected
    fPeerSettings: THttpPeerCacheSettings;
    fHashAlgo: TMGetProcessHash;
    fPeerRequest: TWGetAlternateOptions;
    fLimitBandwidthMB, fWholeRequestTimeoutSec, fTcpTimeoutSec: integer;
    fHashValue: RawUtf8;
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
    property hashAlgo: TMGetProcessHash
      read fHashAlgo write fHashAlgo;
    property hashValue: RawUtf8
      read fHashValue write fHashValue;
    property limitBandwithMB: integer
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

{ TMGetProcess }

const
  HASH_ALGO: array[gphMd5 .. high(TMGetProcessHash)] of THashAlgo = (
    hfMd5,
    hfSha1,
    hfSha256,
    hfSha384,
    hfSha512,
    hfSha3_256);

function GuessAlgo(const Hash: RawUtf8): TMGetProcessHash;
begin
  case length(Hash) shr 1 of // from hexa to bytes
    SizeOf(TMd5Digest):
      result := gphMd5;
    SizeOf(TSha1Digest):
      result := gphSha1;
    SizeOf(TSha256Digest):
      result := gphSha256;
    SizeOf(TSha384Digest):
      result := gphSha384;
    SizeOf(TSha512Digest):
      result := gphSha512;
  else
    result := gphAutoDetect;
  end;
end;

procedure TMGetProcess.Start;
var
  {%H-}l: ISynLog;
begin
  if Peer and
     (fPeerCache = nil) then // reuse THttpPeerCache instance between calls
  begin
    l := Log.Enter(self, 'Start');
    if (fPeerSecret = '') and
       (fPeerSecretHexa <> '') then
      fPeerSecret := HexToBin(fPeerSecretHexa);
    fPeerCache := THttpPeerCache.Create(fPeerSettings, fPeerSecret);
  end;
end;

function TMGetProcess.Execute(const Url: RawUtf8): TFileName;
var
  wget: THttpClientSocketWGet;
  u, h: RawUtf8;
  uri: TUri;
  l: ISynLog;
begin
  // prepare the process
  l := Log.Enter('Execute %', [Url], self);
  Start; // start e.g. background THttpPeerCache process
  // identify e.g. 'xxxxxxxxxxxxxxxxxxxx@http://toto.com/res'
  if Split(Url, '@', h, u) and
     (GuessAlgo(h) <> gphAutoDetect) and
     (HexToBin(h) <> '') then
    hashValue := h // this is a real hash value
  else
    u := Url;
  // guess the hash algorithm from its hexadecimal value size
  if hashAlgo = gphAutoDetect then
    if hashValue <> '' then
      hashAlgo := GuessAlgo(hashValue)
    else if Peer then
      hashAlgo := gphSha256;
  // set the WGet additional parameters
  wget.Clear;
  wget.KeepAlive := 30000;
  wget.Resume := not NoResume;
  wget.HashFromServer := (hashValue = '') and
                         (hashAlgo <> gphAutoDetect);
  if hashAlgo <> gphAutoDetect then
  begin
    wget.Hasher := HASH_STREAMREDIRECT[HASH_ALGO[hashAlgo]];
    wget.Hash := hashValue;
    if not Silent then
      wget.OnProgress := TStreamRedirect.ProgressStreamToConsole;
  end;
  wget.LimitBandwith := fLimitBandwidthMB shl 20;
  wget.TimeOutSec := fWholeRequestTimeoutSec;
  // (peer) cache support
  if Cache then
    wget.HashCacheDir := EnsureDirectoryExists(CacheFolder);
  if Peer then
  begin
    wget.Alternate := fPeerCache; // reuse THttpPeerCache on background
    wget.AlternateOptions := fPeerRequest;
  end;
  // make the request
  result := '';
  if not uri.From(u) then
    exit;
  if (fClient <> nil) and
     ((fClient.Server <> uri.Server) or
      (fClient.Port <> uri.Port)) then
    FreeAndNil(fClient); // need a new connection
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
