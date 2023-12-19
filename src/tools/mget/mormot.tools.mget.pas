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
  /// state engine for mget processing
  // - could be reused between the mget command line tool and an eventual GUI
  TMGetProcess = class(TPersistentAutoCreateFields)
  protected
    fPeerSettings: THttpPeerCacheSettings;
    fHashAlgo: THashAlgo;
    fPeerRequest: TWGetAlternateOptions;
    fLimitBandwidthMB, fWholeRequestTimeoutSec, fTcpTimeoutSec: integer;
    fHashValue: RawUtf8;
    fPeerSecret, fPeerSecretHexa: SpiUtf8;
  public
    // input parameters (e.g. from command line) for the MGet process
    Verbose, NoResume, TlsIgnoreErrors, Hash, Cache, Peer: boolean;
    CacheFolder, TlsCertFile, DestFile: TFileName;
    Log: TSynLogClass;
    /// this is the main processing method
    function Execute(const Url: RawUtf8): TFileName;
  published
    /// the settings used if Peer is true
    property PeerSettings: THttpPeerCacheSettings
      read fPeerSettings write fPeerSettings;
    // following properties will be published as command line switches
    property hashAlgo: THashAlgo
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

function TMGetProcess.Execute(const Url: RawUtf8): TFileName;
var
  client: THttpClientSocket;
  wget: THttpClientSocketWGet;
  u: RawUtf8;
begin
  // set the WGet additional parameters
  wget.Clear;
  if Verbose then
    wget.OnProgress := TStreamRedirect.ProgressStreamToConsole;
  wget.Resume := not NoResume;
  wget.HashFromServer := (hashValue = '') and (Hash or Peer);
  wget.Hasher := HASH_STREAMREDIRECT[fHashAlgo];
  wget.Hash := hashValue;
  wget.LimitBandwith := fLimitBandwidthMB shl 20;
  wget.TimeOutSec := fWholeRequestTimeoutSec;
  if Cache then
    wget.HashCacheDir := EnsureDirectoryExists(CacheFolder);
  if Peer then
  begin
    if (fPeerSecret = '') and
       (fPeerSecretHexa <> '') then
      fPeerSecret := HexToBin(fPeerSecretHexa);
    wget.Alternate := THttpPeerCache.Create(fPeerSettings, fPeerSecret);
    wget.AlternateOptions := fPeerRequest;
  end;
  // make the request
  client := THttpClientSocket.Create(fTcpTimeoutSec * 1000);
  try
    if Log <> nil then
      client.OnLog := Log.DoLog;
    client.TLS.IgnoreCertificateErrors := TlsIgnoreErrors;
    client.TLS.CertificateFile := TlsCertFile;
    client.OpenUri(Url, u);
    result := client.WGet(u, DestFile, wget);
  finally
    client.Free;
  end;
end;


initialization

end.
