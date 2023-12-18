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
    fHash: THashAlgo;
  public
    // input parameters (e.g. from command line) for the MGet process
    Verbose, NoResume, TlsIgnoreErrors, Cache, Peer: boolean;
    CacheFolder, TlsCertFile, DestFile: TFileName;
    Log: TSynLogClass;
    HttpTimeoutSec: integer;
    /// this is the main processing method
    function Execute(const Url: RawUtf8): TFileName;
  published
    /// the settings used if Peer is true
    property PeerSettings: THttpPeerCacheSettings
      read fPeerSettings write fPeerSettings;
    /// following properties could be published as command line switches
    property Hash: THashAlgo
      read fHash write fHash;
  end;


implementation

{ TMGetProcess }

function TMGetProcess.Execute(const Url: RawUtf8): TFileName;
var
  client: THttpClientSocket;
  wget: THttpClientSocketWGet;
begin
  // set the WGet additional parameters
  wget.Clear;
  if Verbose then
    wget.OnProgress := TStreamRedirect.ProgressStreamToConsole;
  wget.Resume := not NoResume;
  wget.Hash := copy(HASH_EXT[fHash], 2, 10);
  if cache then
    wget.HashCacheDir := CacheFolder;
  // make the request
  client := THttpClientSocket.Create(HttpTimeoutSec * 1000);
  try
    if Log <> nil then
      client.OnLog := Log.DoLog;
    client.TLS.IgnoreCertificateErrors := TlsIgnoreErrors;
    client.TLS.CertificateFile := TlsCertFile;
    result := client.WGet(Url, DestFile, wget);
  finally
    client.Free;
  end;
end;


initialization

end.
