/// HTTP Local Peer-to-peer Cache
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.peercache;

{
  *****************************************************************************

   HTTP Local Peer-to-peer Cache
   - THttpPeerCache Local Peer-to-peer Cache

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.os.security,
  mormot.core.data,
  mormot.core.threads,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.buffers,
  mormot.core.datetime,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.fmt,
  mormot.core.variants,
  mormot.core.zip,
  mormot.core.log,
  mormot.core.search,
  mormot.net.sock,
  mormot.net.http,
  mormot.lib.sspi,   // void unit on POSIX
  mormot.lib.gssapi, // void unit on Windows
  mormot.net.client,
  mormot.crypt.core,
  mormot.crypt.secure,
  mormot.crypt.factory,
  mormot.net.server;


{ ******************** THttpPeerCache Local Peer-to-peer Cache }

{
  TODO:
  - Daemon/Service background mode for the mget tool
  - Asymmetric security using ECDH shared secret? use HTTPS instead?
  - Frame signature using ECDHE with ephemeral keys? use HTTPS instead?
  - Allow binding to several network interfaces? (e.g. wifi to/from ethernet)
}

type
  /// the content of a binary THttpPeerCacheMessage
  // - could eventually be extended in the future for frame versioning
  // - pcfBearerDirect* are used for pcoHttpDirect mode: /https/microsoft.com/...
  THttpPeerCacheMessageKind = (
    pcfPing,
    pcfPong,
    pcfRequest,
    pcfResponseNone,
    pcfResponseOverloaded,
    pcfResponsePartial,
    pcfResponseFull,
    pcfBearer,
    pcfBearerDirect,
    pcfBearerDirectPermanent);

  /// one UDP request frame used during THttpPeerCache discovery
  // - requests and responses have the same binary layout
  // - some fields may be void or irrelevant, and the structure is padded
  // with random up to 192 bytes
  // - over the wire, packets are encrypted and authenticated via AES-GCM-128
  // with an ending salted checksum for quick anti-fuzzing (as 212 bytes blocks)
  THttpPeerCacheMessage = packed record
    /// the content of this binary frame
    Kind: THttpPeerCacheMessageKind;
    /// 32-bit sequence number
    Seq: cardinal;
    /// the UUID of the Sender
    Uuid: TGuid;
    /// the Operating System of the Sender
    Os: TOperatingSystemVersion;
    /// the local IPv4 which sent this frame
    // - e.g. 192.168.1.1
    IP4: cardinal;
    /// the destination IPv4 of this frame
    // - contains 0 for a broadcast
    // - allows to filter response frames when broadcasted on POSIX
    DestIP4: cardinal;
    /// the IPv4 network mask of the local network interface
    // - e.g. 255.255.255.0
    MaskIP4: cardinal;
    /// the IPv4 broadcast address the local network interface
    // - e.g. 192.168.1.255
    BroadcastIP4: cardinal;
    /// the link speed (in Mbits per second) of the local network interface
    Speed: cardinal;
    /// the hardware model of this network interface
    Hardware: TMacAddressKind;
    /// the local UnixTimeMinimalUtc value
    Timestamp: TUnixTimeMinimal;
    /// number of background download connections currently on this server
    Connections: word;
    /// up to 512-bit of binary Hash (and algo) of the requested file content
    Hash: THashDigest;
    /// the known full size of this file
    Size: Int64;
    /// the Range offset of the requested file content
    RangeStart: Int64;
    /// the Range ending position of the file content (included)
    RangeEnd: Int64;
    /// some internal state representation, e.g. sent back as pcfBearer
    Opaque: QWord;
    /// define the kind of content in the padding block - always 0 by now
    PaddingVersion: byte;
    /// some random padding up to 192 bytes, used for future content revisions
    // - e.g. for a TEccPublicKey (ECDHE) and additional fields
    // - using random enhances AES-GCM obfuscation by making it unpredictable
    Padding: array[0 .. 41] of byte;
  end;
  PHttpPeerCacheMessage = ^THttpPeerCacheMessage;
  THttpPeerCacheMessageDynArray = array of THttpPeerCacheMessage;

  /// AES-GCM encoded binary frame of THttpPeerCacheMessage
  THttpPeerCacheMessageEncoded = packed record
    iv: THash128;
    msg: THttpPeerCacheMessage;
    padding, mac: THash128;
    crc: cardinal;
  end;
  PHttpPeerCacheMessageEncoded = ^THttpPeerCacheMessageEncoded;

  /// each THttpPeerCacheSettings.Options item
  // - pcoCacheTempSubFolders will create 16 sub-folders (from first 0-9/a-f
  // hash nibble) within CacheTempPath to reduce filesystem fragmentation
  // - pcoCacheTempNoCheckSize will ignore checking CacheTempMaxMB ratio on disk
  // - pcoUseFirstResponse will accept the first positive response, and don't
  // wait for the BroadcastTimeoutMS delay for all responses to be received
  // - pcoTryLastPeer will first check the latest peer with HTTP/TCP before
  // making any broadcast - to be used if the files are likely to come in batch;
  // can be forced by TWGetAlternateOptions from a given WGet() call
  // - pcoTryAllPeers will try up to the best 10 braodcast responses, before
  // falling back to the main server
  // - pcoBroadcastNotAlone will disable broadcasting for up to one second if
  // no response at all was received within BroadcastTimeoutMS delay
  // - pcoNoServer disable the local UDP/HTTP servers and acts as a pure client
  // - pcoNoBanIP disable the 4 seconds IP banishment mechanism at HTTP level;
  // set RejectInstablePeersMin = 0 to disable banishment at UDP level
  // - pcoSelfSignedHttps enables HTTPS communication with a self-signed server
  // (warning: this option should be set on all peers, clients and servers) -
  // as an alternative, you could set THttpPeerCache.ServerTls/ClientTls props
  // - pcoVerboseLog will log all details, e.g. raw UDP frames
  // - pcoHttpDirect extends the HTTP endpoint to initiate a download process
  // from localhost, and return the cached content (used e.g. as proxy + cache)
  // requiring URI + Bearer as returned by HttpDirectUri() overloaded methods
  // - pcoHttpDirectNoBroadcast would disable UDP peer search for pcoHttpDirect
  // - pcoHttpDirectTryLastPeer could make pcoTryLastPeer for pcoHttpDirect
  // - pcoHttpReprDigest would include a 'Repr-Digest:' header as per RFC 9530
  THttpPeerCacheOption = (
    pcoCacheTempSubFolders,
    pcoCacheTempNoCheckSize,
    pcoUseFirstResponse,
    pcoTryLastPeer,
    pcoTryAllPeers,
    pcoBroadcastNotAlone,
    pcoNoServer,
    pcoNoBanIP,
    pcoSelfSignedHttps,
    pcoVerboseLog,
    pcoHttpDirect,
    pcoHttpDirectNoBroadcast,
    pcoHttpDirectTryLastPeer,
    pcoHttpReprDigest);

  /// THttpPeerCacheSettings.Options values
  THttpPeerCacheOptions = set of THttpPeerCacheOption;

  /// define how THttpPeerCache handles its process
  THttpPeerCacheSettings = class(TSynPersistent)
  protected
    fPort: TNetPort;
    fInterfaceFilter: TMacAddressFilter;
    fOptions: THttpPeerCacheOptions;
    fLimitMBPerSec, fLimitClientCount,
    fBroadcastTimeoutMS, fBroadcastMaxResponses, fBroadCastDirectMinBytes,
    fTryAllPeersCount, fHttpTimeoutMS, fRejectInstablePeersMin: integer;
    fCacheTempMaxMB, fCacheTempMaxMin,
    fCacheTempMinBytes, fCachePermMinBytes: integer;
    fCacheTempPath, fCachePermPath: TFileName;
    fInterfaceName, fUuid: RawUtf8;
  public
    /// set the default settings
    // - i.e. Port=8099, LimitMBPerSec=10, LimitClientCount=32,
    // RejectInstablePeersMin=4, CacheTempMaxMB=1000, CacheTempMaxMin=60,
    // CacheTempMinBytes=CachePermMinBytes=2048, BroadCastDirectMinBytes=65536,
    // BroadcastTimeoutMS=10 HttpTimeoutMS=500 and BroadcastMaxResponses=24
    constructor Create; override;
    /// retrieve the network interface fulfilling these settings
    // - network layout may change in real time: this method allows to renew
    // the peer cache instance when a better interface is available
    // - returns '' on success, or an error message
    function GuessInterface(out Mac: TMacAddress): RawUtf8; virtual;
    /// encode a remote URI for pcoHttpDirect download at localhost
    // - returns aDirectUri e.g. as 'http://1.2.3.4:8099/https/microsoft.com/...'
    // (if port is 8099 and Mac.IP is 1.2.3.4) and its aDirectHeaderBearer
    // - aForceTls should map ServerTls.Enabled
    function HttpDirectUri(const aSharedSecret: RawByteString;
      const aRemoteUri, aRemoteHash: RawUtf8;
      out aDirectUri, aDirectHeaderBearer: RawUtf8;
      aForceTls: boolean = false; aPermanent: boolean = false;
      aOptions: PHttpRequestExtendedOptions = nil): boolean;
  published
    /// the local port used for UDP and TCP process
    // - value should match on all peers for proper discovery
    // - UDP for discovery, TCP for HTTP/HTTPS content delivery
    // - is 8099 by default, which is unassigned by IANA
    property Port: TNetPort
      read fPort write fPort;
    /// allow to customize the process
    property Options: THttpPeerCacheOptions
      read fOptions write fOptions;
    /// local TMacAddress.Name, Address or IP to be used for UDP and TCP
    // - Name and Address will be searched case-insensitive
    // - IP could be exact or eventually a bitmask pattern (e.g. 192.168.1.255)
    // - if not set, will fallback to the best local makEthernet/makWifi network
    // with broadcasting abilities
    // - matching TMacAddress.IP will be used with the Port property value to
    // bind the TCP/HTTP server and broadcast the UDP discovery packets, so that
    // only this network interface will be involved to find cache peers
    property InterfaceName: RawUtf8
      read fInterfaceName write fInterfaceName;
    /// how GetMacAddress() should find the network, if InterfaceName is not set
    property InterfaceFilter: TMacAddressFilter
      read fInterfaceFilter write fInterfaceFilter;
    /// can limit the peer bandwidth used, in data MegaBytes per second
    // - will be assigned to each TStreamRedirect.LimitPerSecond instance
    // - default is 10 MB/s of data, i.e. aroung 100-125 MBit/s on network
    // - you may set 0 to disable any bandwidth limitation
    // - you may set -1 to use the default TStreamRedirect.LimitPerSecond value
    property LimitMBPerSec: integer
      read fLimitMBPerSec write fLimitMBPerSec;
    /// can limit how many peer clients can be served content at the same time
    // - would prevent any overload, to avoid Denial of Service
    // - default is 32, which means 32 threads with the default THttpServer
    property LimitClientCount: integer
      read fLimitClientCount write fLimitClientCount;
    /// RejectInstablePeersMin will set a delay (in minutes) to ignore any peer
    // which sent invalid UDP frames or HTTP/HTTPS requests
    // - should be a positive small power of two <= 128
    // - default is 4, for a 4 minutes time-to-live of IP banishments
    // - you may set 0 to disable the whole IP ban safety mechanism at UDP level
    // - use pcoNoBanIP option to disable the IP ban mechanism at HTTP level
    property RejectInstablePeersMin: integer
      read fRejectInstablePeersMin write fRejectInstablePeersMin;
    /// how many milliseconds UDP broadcast should wait for a response
    // - default is 10 ms which seems enough on a local network
    // - on Windows, this value is indicative, likely to have 15ms resolution
    property BroadcastTimeoutMS: integer
      read fBroadcastTimeoutMS write fBroadcastTimeoutMS;
    /// how many responses UDP broadcast should take into account
    // - default is 24
    property BroadcastMaxResponses: integer
      read fBroadcastMaxResponses write fBroadcastMaxResponses;
    /// above how many bytes the peer network should be asked for a pcoHttpDirect
    // - default is 65536 bytes, i.e. 64KB
    property BroadCastDirectMinBytes: integer
      read fBroadCastDirectMinBytes write fBroadCastDirectMinBytes;
    /// how many of the best responses should pcoTryAllPeers also try
    // - default is 10
    property TryAllPeersCount: integer
      read fTryAllPeersCount write fTryAllPeersCount;
    /// the socket level timeout for HTTP requests
    // - default to low 500 ms because should be local
    property HttpTimeoutMS: integer
      read fHttpTimeoutMS write fHttpTimeoutMS;
    /// location of the temporary cached files, available for remote requests
    // - the files are cached using their THashDigest values as filename
    // - this folder will be purged according to CacheTempMaxMB/CacheTempMaxMin
    // - if this value equals '', or pcoNoServer is defined in Options,
    // temporary caching would be disabled
    property CacheTempPath: TFileName
      read fCacheTempPath write fCacheTempPath;
    /// above how many bytes the peer network should be asked for a temporary file
    // - there is no size limitation if the file is already in the temporary
    // cache, or if the waoNoMinimalSize option is specified by WGet()
    // - default is 2048 bytes, i.e. 2KB
    property CacheTempMinBytes: integer
      read fCacheTempMinBytes  write fCacheTempMinBytes;
    /// after how many MB in CacheTempPath the folder should be cleaned
    // - default is 1000, i.e. just below 1 GB
    // - THttpPeerCache.Create will also always ensure that this value won't
    // take more than 25% of the CacheTempPath folder available space
    property CacheTempMaxMB: integer
      read fCacheTempMaxMB write fCacheTempMaxMB;
    /// after how many minutes files in CacheTempPath could be cleaned
    // - i.e. the Time-To-Live (TTL) of temporary files
    // - default is 60 minutes, i.e. 1 hour
    property CacheTempMaxMin: integer
      read fCacheTempMaxMin write fCacheTempMaxMin;
    /// location of the permanent cached files, available for remote requests
    // - in respect to CacheTempPath, this folder won't be purged
    // - the files are cached using their THashDigest values as filename
    // - if this value equals '', or pcoNoServer is defined in Options,
    // permanent caching would be disabled
    property CachePermPath: TFileName
      read fCachePermPath write fCachePermPath;
    /// above how many bytes the peer network should be asked for a permanent file
    // - there is no size limitation if the file is already in the permanent
    // cache, or if the waoNoMinimalSize option is specified by WGet()
    // - default is 2048 bytes, i.e. 2KB, which is just two network MTU trips
    property CachePermMinBytes: integer
      read fCachePermMinBytes  write fCachePermMinBytes;
    /// allow to customize the UUID used to identify this node
    // - instead of the default GetComputerUuid() from SMBios
    // - should be a valid UUID/GUID text as recognized by RawUtf8ToGuid()
    property Uuid: RawUtf8
      read fUuid write fUuid;
  end;

  /// information about THttpPeerCrypt.MessageDecode() success
  THttpPeerCryptMessageDecode = (
    mdOk,
    mdBParam,
    mdBLen,
    mdB64,
    mdBearer,
    mdLen,
    mdCrc,
    mdAes,
    mdSeq,
    mdKind,
    mdHw,
    mdAlgo);

  /// abstract parent to THttpPeerCache for its cryptographic core
  THttpPeerCrypt = class(TInterfacedPersistent)
  protected
    fAesSafe: TLightLock; // topmost to ensure proper aarch64 alignment
    fClientSafe: TOSLightLock; // if try to download with background direct mode
    fSettings: THttpPeerCacheSettings;
    fSharedMagic, fFrameSeqLow: cardinal;
    fFrameSeq: integer;
    fIP4, fMaskIP4, fBroadcastIP4, fClientIP4, fLastNetworkTix: cardinal;
    fAesEnc, fAesDec: TAesGcmAbstract;
    fLog: TSynLogClass;
    fPort, fIpPort, fDirectSecret: RawUtf8;
    fClient: THttpClientSocket;
    fInstable: THttpAcceptBan; // from Settings.RejectInstablePeersMin
    fMac: TMacAddress;
    fSettingsOwned, fVerboseLog, fShutdown: boolean;
    fUuid: TGuid;
    fServerTls, fClientTls: TNetTlsContext;
    procedure AfterSettings; virtual;
    function CurrentConnections: integer; virtual;
    procedure MessageInit(aKind: THttpPeerCacheMessageKind; aSeq: cardinal;
      out aMsg: THttpPeerCacheMessage); virtual;
    procedure MessageEncode(const aMsg: THttpPeerCacheMessage;
      out aEncoded: THttpPeerCacheMessageEncoded);
    procedure MessageEncodeBearer(const aMsg: THttpPeerCacheMessage;
      out aBearer: RawUtf8);
    function MessageDecode(aFrame: PHttpPeerCacheMessageEncoded; aFrameLen: PtrInt;
      out aMsg: THttpPeerCacheMessage): THttpPeerCryptMessageDecode;
    function BearerDecode(const aBearerToken: RawUtf8;
      aExpected: THttpPeerCacheMessageKind; out aMsg: THttpPeerCacheMessage;
      aParams: PRawUtf8 = nil): THttpPeerCryptMessageDecode; virtual;
    procedure LocalPeerClientSetup(const aIp: RawUtf8;
      aClient: THttpClientSocket; aRecvTimeout: integer);
    function LocalPeerRequest(const aRequest: THttpPeerCacheMessage;
      var aResp : THttpPeerCacheMessage; const aUrl: RawUtf8;
      aOutStream: TStreamRedirect; aBanInstable: boolean): integer;
    function GetUuidText: RawUtf8;
  public
    /// initialize the cryptography of this peer-to-peer node instance
    // - warning: inherited class should also call AfterSettings once
    // fSettings is defined
    constructor Create(const aSharedSecret: RawByteString;
      aServerTls, aClientTls: PNetTlsContext); reintroduce;
    /// finalize this class instance
    destructor Destroy; override;
    /// check if the network interface defined in Settings did actually change
    // - you may want to recreate a peer-cache to track the new network layout
    function NetworkInterfaceChanged: boolean;
    /// encode a remote URI for pcoHttpDirect download at localhost
    // - aSharedSecret should match the Create() value
    // - returns aDirectUri as '/https/microsoft.com/...' and aDirectHeaderBearer
    class function HttpDirectUri(const aSharedSecret: RawByteString;
      const aRemoteUri, aRemoteHash: RawUtf8;
      out aDirectUri, aDirectHeaderBearer: RawUtf8; aPermanent: boolean = false;
      aOptions: PHttpRequestExtendedOptions = nil): boolean;
    /// decode a remote URI for pcoHttpDirect download at localhost
    // - as previously encoded by HttpDirectUri() class function
    class function HttpDirectUriReconstruct(P: PUtf8Char;
      out Decoded: TUri): boolean;
    /// optional TLS options for the peer HTTPS server
    // - e.g. to set a custom certificate for this peer
    // - when ServerTls.Enabled is set, ClientTls.Enabled and other params should match
    property ServerTls: TNetTlsContext
      read fServerTls write fServerTls;
    /// optional TLS options for the peer HTTPS client
    // - e.g. set ClientTls.OnPeerValidate to verify a peer ServerTls certificate
    // - when ClientTls.Enabled is set, ServerTls.Enabled and other params should match
    property ClientTls: TNetTlsContext
      read fClientTls write fClientTls;
    /// the network interface used for UDP and TCP process
    // - the main fields are published below as Network* properties
    property Mac: TMacAddress
      read fMac;
    /// the 'ip:port' value used for UDP and TCP process
    property IpPort: RawUtf8
      read fIpPort;
    /// the raw 32-bit IPv4 value used for UDP and TCP process
    property Ip4: cardinal
      read fIp4;
  published
    /// define how this instance handles its process
    property Settings: THttpPeerCacheSettings
      read fSettings;
    /// the UUID used to identify this PeerCache node
    property Uuid: RawUtf8
      read GetUuidText;
    /// which network interface is used for UDP and TCP process
    property NetworkInterface: RawUtf8
      read fMac.Name;
    /// the local IP address used for UDP and TCP process
    property NetworkIP: RawUtf8
      read fMac.IP;
    /// the port value used for UDP and TCP process
    property NetworkPort: RawUtf8
      read fPort;
    /// the IP used for UDP and TCP process broadcast
    property NetworkBroadcast: RawUtf8
      read fMac.Broadcast;
  end;

  /// exception class raised on THttpPeerCache issues
  EHttpPeerCache = class(ESynException);

  THttpPeerCache = class;

  /// background UDP server thread, associated to a THttpPeerCache instance
  THttpPeerCacheThread = class(TUdpServerThread)
  protected
    fOwner: THttpPeerCache;
    fSent, fResponses: integer;
    fRespSafe: TLightLock;
    fResp: THttpPeerCacheMessageDynArray;
    fRespCount: integer;
    fBroadcastCurrentSeq: cardinal;
    fBroadcastEvent: TSynEvent;   // e.g. for pcoUseFirstResponse
    fBroadcastAddr: TNetAddr;     // from fBroadcastIP4 + fSettings.Port
    fBroadcastSafe: TOSLightLock; // non-reentrant, to serialize Broadcast()
    fBroadcastIpPort: RawUtf8;
    procedure OnFrameReceived(len: integer; var remote: TNetAddr); override;
    procedure OnIdle(tix64: Int64); override; // called every second
    procedure OnShutdown; override; // = Destroy
    function Broadcast(const aReq: THttpPeerCacheMessage;
      out aAlone: boolean): THttpPeerCacheMessageDynArray;
    function AddResponseAndDone(const aMessage: THttpPeerCacheMessage): boolean;
    function GetResponses(aSeq: cardinal): THttpPeerCacheMessageDynArray;
  public
    /// initialize the background UDP server thread
    constructor Create(Owner: THttpPeerCache); reintroduce;
    /// finalize this instance
    destructor Destroy; override;
  published
    property Sent: integer
      read fSent;
  end;

  THttpPeerCacheLocalFileName = set of (
    lfnSetDate,
    lfnEnsureDirectoryExists);

  /// callback to optimize HTTP/HTTPS remote access a given URI with pcoHttpDirect
  // - as defined in THttpPeerCache.OnDirectOptions property
  // - should override proper options (and/or header/URI) and return HTTP_SUCCESS
  TOnHttpPeerCacheDirectOptions = function(var aUri: TUri; var aHeader: RawUtf8;
    var aOptions: THttpRequestExtendedOptions): integer of object;

  EHttpServerCache = class(ESynException);

  /// implement a local peer-to-peer download cache via UDP and TCP
  // - UDP broadcasting is used for local peers discovery
  // - TCP is bound to a local THttpServer/THttpAsyncServer content delivery
  // - will maintain its own local folders of cached files, stored by hash
  THttpPeerCache = class(THttpPeerCrypt, IWGetAlternate)
  protected
    fHttpServer: THttpServerGeneric;
    fUdpServer: THttpPeerCacheThread;
    fPermFilesPath, fTempFilesPath: TFileName;
    fTempFilesMaxSize: Int64; // from Settings.CacheTempMaxMB
    fTempCurrentSize: Int64;
    fBroadcastStart: Int64;   // QueryPerformanceMicroSeconds()
    fTempFilesTix, fInstableTix, fBroadcastTix: cardinal;
    fPartials: THttpPartials;
    fFilesSafe: TOSLock; // concurrent cached files access
    fOnDirectOptions: TOnHttpPeerCacheDirectOptions;
    // most of these internal methods are virtual for proper customization
    procedure StartHttpServer(aHttpServerClass: THttpServerSocketGenericClass;
      aHttpServerThreadCount: integer; const aIP: RawUtf8); virtual;
    function CurrentConnections: integer; override;
    function ComputeFileName(const aHash: THashDigest): TFileName; virtual;
    function PermFileName(const aFileName: TFileName;
      aFlags: THttpPeerCacheLocalFileName): TFileName; virtual;
    function LocalFileName(const aMessage: THttpPeerCacheMessage;
      aFlags: THttpPeerCacheLocalFileName;
      aFileName: PFileName; aSize: PInt64): integer;
    function CachedFileName(const aParams: THttpClientSocketWGet;
      aFlags: THttpPeerCacheLocalFileName;
      out aLocal: TFileName; out isTemp: boolean): boolean;
    function DirectFileName(Ctxt: THttpServerRequestAbstract;
      const aMessage: THttpPeerCacheMessage; aHttp: PHttpRequestContext;
      out aFileName: TFileName; out aSize: Int64; const aParams: RawUtf8): integer;
    procedure DirectFileNameBackgroundGet(Sender: TObject);
    function DirectFileNameHead(Ctxt: THttpServerRequestAbstract;
      const aHash: THashDigest; const aParams: RawUtf8): cardinal;
    function TooSmallFile(const aParams: THttpClientSocketWGet;
      aSize: Int64; const aCaller: ShortString): boolean;
    function PartialFileName(const aMessage: THttpPeerCacheMessage;
      aHttp: PHttpRequestContext; aFileName: PFileName; aSize: PInt64): integer;
    function TempFolderEstimateNewSize(aAddingSize: Int64): Int64;
    function Check(Status: THttpPeerCryptMessageDecode;
      const Ctxt: ShortString; const Msg: THttpPeerCacheMessage): boolean;
  public
    /// initialize this peer-to-peer cache instance
    // - any supplied aSettings should be owned by the caller (e.g from a main
    // settings class instance)
    // - aSharedSecret is used to cipher and authenticate each UDP frame between
    // all peer nodes, and also generate HTTP authentication bearers
    // - if aSettings = nil, default values will be used by this instance
    // - you can supply THttpAsyncServer class to replace default THttpServer
    // - may raise some exceptions if the HTTP server cannot be started
    constructor Create(aSettings: THttpPeerCacheSettings;
      const aSharedSecret: RawByteString;
      aHttpServerClass: THttpServerSocketGenericClass = nil;
      aHttpServerThreadCount: integer = 2; aLogClass: TSynLogClass = nil;
      aServerTls: PNetTlsContext = nil; aClientTls: PNetTlsContext = nil); reintroduce;
    /// finalize this peer-to-peer cache instance
    destructor Destroy; override;
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    // - will transfer Sender.Server/Port/RangeStart/RangeEnd into OutStream
    // - OutStream.LimitPerSecond will be overriden during the call
    // - could return 0 to fallback to a regular GET (e.g. not cached)
    function OnDownload(Sender: THttpClientSocket;
      var Params: THttpClientSocketWGet; const Url: RawUtf8;
      ExpectedFullSize: Int64; OutStream: TStreamRedirect): integer; virtual;
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    // - if a file has been downloaded from the main repository, this method
    // should be called to copy the content into this instance files cache
    // - also makes Rename(Partial, ToRename) with proper progressive support
    procedure OnDownloaded(var Params: THttpClientSocketWGet;
      const Partial, ToRename: TFileName; PartialID: integer); virtual;
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    // - OnDownload() may have returned corrupted data: local cache file is
    // likely to be deleted, for safety
    procedure OnDownloadFailed(const Params: THttpClientSocketWGet);
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    // - make this .part file available as pcfResponsePartial
    // - returns PartialID > 0 sequence
    function OnDownloading(const Params: THttpClientSocketWGet;
      const Partial: TFileName; ExpectedFullSize: Int64): THttpPartialID;
    /// IWGetAlternate main processing method, as used by THttpClientSocketWGet
    /// notify the alternate download implementation that OnDownloading() failed
    // - e.g. THttpPeerCache is likely to abort publishing this partial file
    procedure OnDownloadingFailed(ID: THttpPartialID);
    /// broadcast a pcfPing on the network interface and return the responses
    function Ping: THttpPeerCacheMessageDynArray;
    /// method called by the HttpServer before any request is processed
    // - will reject anything but a GET with a proper bearer, from the right IP
    function OnBeforeBody(var aUrl, aMethod, aInHeaders,
      aInContentType, aRemoteIP, aBearerToken: RawUtf8; aContentLength: Int64;
      aFlags: THttpServerRequestFlags): cardinal; virtual;
    /// method called by the HttpServer to process a request
    // - statically serve a local file from decoded bearer hash
    function OnRequest(Ctxt: THttpServerRequestAbstract): cardinal; virtual;
    /// is called on a regular basis for background regular process
    // - is called from THttpPeerCacheThread.OnIdle
    // - e.g. to implement optional CacheTempMaxMin disk space release,
    // actually reading and purging the CacheTempPath folder every minute
    // - could call Instable.DoRotate every minute to refresh IP banishments
    procedure OnIdle(tix64: Int64);
    /// returns the current state of this PeerCache instance
    function State: TWGetAlternateState;
    /// event to customize the access of a given URI in pcoHttpDirect mode
    property OnDirectOptions: TOnHttpPeerCacheDirectOptions
      read fOnDirectOptions write fOnDirectOptions;
    /// access to the current directory storing temporary cache files
    property TempFilesPath: TFileName
      read fTempFilesPath;
    /// access to the current directory storing permanent cache files
    property PermFilesPath: TFileName
      read fPermFilesPath;
  published
    /// the associated HTTP/HTTPS server delivering cached context
    property HttpServer: THttpServerGeneric
      read fHttpServer;
    /// the current state of banned IP from incorrect UDP/HTTP requests
    // - follow RejectInstablePeersMin settings
    property Instable: THttpAcceptBan
      read fInstable;
  end;

  /// one THttpPeerCache.OnDownload instance
  THttpPeerCacheProcess = class(TSynPersistent)
  protected
    fOwner: THttpPeerCache;
  public
  published
    property Owner: THttpPeerCache
      read fOwner;
  end;

const
  PCF_RESPONSE = [
    pcfPong,
    pcfResponseNone,
    pcfResponseOverloaded,
    pcfResponsePartial,
    pcfResponseFull];

  PEER_CACHE_PATTERN = '*.cache';

  PEER_CACHE_MESSAGELEN = SizeOf(THttpPeerCacheMessageEncoded);
  PEER_CACHE_AESLEN = PEER_CACHE_MESSAGELEN - SizeOf(cardinal); // length - crc
  /// base-64 HttpDirectUri() bearer size in chars, from PEER_CACHE_MESSAGELEN
  PEER_CACHE_BEARERLEN = 326;

function ToText(pcf: THttpPeerCacheMessageKind): PShortString; overload;
function ToText(md: THttpPeerCryptMessageDecode): PShortString; overload;
function ToText(const msg: THttpPeerCacheMessage): ShortString; overload;
  {$ifdef HASINLINE} inline; {$endif}

procedure MsgToShort(const msg: THttpPeerCacheMessage; var result: ShortString);


implementation


{ ******************** THttpPeerCache Local Peer-to-peer Cache }

{ THttpPeerCrypt }

procedure THttpPeerCrypt.AfterSettings;
var
  err: RawUtf8;
begin
  if fSettings = nil then
    EHttpPeerCache.RaiseUtf8('%.AfterSettings(nil)', [self]);
  fLog.Add.Log(sllTrace, 'Create: with %', [fSettings], self);
  err := fSettings.GuessInterface(fMac);
  if err <> '' then
    EHttpPeerCache.RaiseUtf8('%.Create: %', [self, err]);
  IPToCardinal(fMac.IP, fIP4);
  IPToCardinal(fMac.NetMask, fMaskIP4);
  IPToCardinal(fMac.Broadcast, fBroadcastIP4);
  UInt32ToUtf8(fSettings.Port, fPort);
  FormatUtf8('%:%', [fMac.IP, fPort], fIpPort); // UDP/TCP bound to this network
  if fSettings.RejectInstablePeersMin > 0 then
  begin
    fInstable := THttpAcceptBan.Create(fSettings.RejectInstablePeersMin);
    fInstable.WhiteIP := fIP4; // no UDP ban from localhost
  end;
  fLog.Add.Log(sllDebug, 'Create: network="%" as % (broadcast=%) %',
    [fMac.Name, fIpPort, fMac.Broadcast, fMac.Address], self);
end;

function THttpPeerCrypt.GetUuidText: RawUtf8;
begin
  ToUtf8(fUuid, result);
end;

function THttpPeerCrypt.CurrentConnections: integer;
begin
  result := 0; // to be properly overriden with the HTTP server information
end;

procedure THttpPeerCrypt.MessageInit(aKind: THttpPeerCacheMessageKind;
  aSeq: cardinal; out aMsg: THttpPeerCacheMessage);
var
  n: cardinal;
begin
  FillCharFast(aMsg, SizeOf(aMsg) - SizeOf(aMsg.Padding), 0);
  SharedRandom.Fill(@aMsg.Padding, SizeOf(aMsg.Padding)); // TLecuyer is enough
  if aSeq = 0 then
    aSeq := InterlockedIncrement(fFrameSeq);
  aMsg.Seq := aSeq;
  aMsg.Kind := aKind;
  aMsg.Uuid := fUuid;
  aMsg.Os := OSVersion32;
  aMsg.IP4 := fIP4;
  aMsg.DestIP4 := fBroadcastIP4; // overriden in DoSendResponse()
  aMsg.MaskIP4 := fMaskIP4;
  aMsg.BroadcastIP4 := fBroadcastIP4;
  aMsg.Speed := fMac.Speed;
  aMsg.Hardware := fMac.Kind;
  aMsg.Timestamp := UnixTimeMinimalUtc;
  n := CurrentConnections; // virtual method
  if n > 65535 then
    n := 65535;
  aMsg.Connections := n;
end;

// UDP frames are AES-GCM encrypted and signed, ending with a 32-bit crc, fixed
// to crc32c(): md5/sha (without SHA-NI) are slower than AES-GCM-128 itself ;)
// - on x86_64
//    2000 messages in 275us i.e. 6.9M/s, aver. 137ns, 1.3 GB/s   = AES-GCM-128
//    10000 altered in 126us i.e. 75.6M/s, aver. 12ns, 14.1 GB/s  = crc32c()
// - on i386
//    2000 messages in 673us i.e. 2.8M/s, aver. 336ns, 544.1 MB/s
//    10000 altered in 310us i.e. 30.7M/s, aver. 31ns, 5.7 GB/s

procedure THttpPeerCrypt.MessageEncode(const aMsg: THttpPeerCacheMessage;
  out aEncoded: THttpPeerCacheMessageEncoded);
begin
  // AES-GCM-128 encoding and authentication
  fAesSafe.Lock;
  try
    if fAesEnc.AesGcmBuffer(@aMsg, @aEncoded, SizeOf(aMsg), SizeOf(aEncoded),
          true, true) <> PEER_CACHE_AESLEN then
      exit;
  finally
    fAesSafe.UnLock;
  end;
  // append salted checksum to quickly reject any fuzzing attempt (endsize=4)
  aEncoded.crc := crc32c(fSharedMagic, @aEncoded, PEER_CACHE_AESLEN);
end;

procedure THttpPeerCrypt.MessageEncodeBearer(const aMsg: THttpPeerCacheMessage;
  out aBearer: RawUtf8);
var
  bin: THttpPeerCacheMessageEncoded;
begin
  MessageEncode(aMsg, bin);
  aBearer := AuthorizationBearer(BinToBase64uri(@bin, SizeOf(bin)));
end;

function THttpPeerCrypt.MessageDecode(
  aFrame: PHttpPeerCacheMessageEncoded; aFrameLen: PtrInt;
  out aMsg: THttpPeerCacheMessage): THttpPeerCryptMessageDecode;
var
  tmp: THttpPeerCacheMessageEncoded; // need more space to decode the padding
begin
  // quickly reject any naive fuzzing attempt against length and 32-bit checksum
  result := mdLen;
  PByte(@aMsg.Kind)^ := 255; // so that ToText(aMsg) = ToText(aMsg.Kind)^ = ''
  if aFrameLen <> PEER_CACHE_MESSAGELEN then
    exit;
  result := mdCrc;
  if crc32c(fSharedMagic, pointer(aFrame), PEER_CACHE_AESLEN) <> aFrame^.crc then
     exit;
  // AES-GCM-128 decoding and authentication
  result := mdAes;
  fAesSafe.Lock;
  try
    if fAesDec.AesGcmBuffer(aFrame, @tmp, PEER_CACHE_AESLEN, SizeOf(tmp),
         {enc=}false, {iv=}true) <> SizeOf(aMsg) then
      exit;
  finally
    fAesSafe.UnLock;
  end;
  MoveFast(tmp, aMsg, SizeOf(aMsg));
  // check consistency of the decoded THttpPeerCacheMessage value
  if (aMsg.Kind in PCF_RESPONSE) and  // responses are broadcasted on POSIX
     (aMsg.DestIP4 = fIP4) and // validate against local sequence
     ((aMsg.Seq < fFrameSeqLow) or
       (aMsg.Seq > cardinal(fFrameSeq))) then
    result := mdSeq
  else if ord(aMsg.Kind) > ord(high(aMsg.Kind)) then
    result := mdKind
  else if ord(aMsg.Hardware) > ord(high(aMsg.Hardware)) then
    result := mdHw
  else if ord(aMsg.Hash.Algo) > ord(high(aMsg.Hash.Algo)) then
    result := mdAlgo
  else
    result := mdOk;
end;

function THttpPeerCrypt.BearerDecode(
  const aBearerToken: RawUtf8; aExpected: THttpPeerCacheMessageKind;
  out aMsg: THttpPeerCacheMessage; aParams: PRawUtf8): THttpPeerCryptMessageDecode;
var
  tok: THttpPeerCacheMessageEncoded; // no memory allocation
  bearerlen: PtrInt;
begin
  result := mdBLen;
  bearerlen := length(aBearerToken) - PEER_CACHE_BEARERLEN;
  if bearerlen <> 0 then
    if bearerlen > 0 then
    begin
      result := mdBParam;
      if aBearerToken[PEER_CACHE_BEARERLEN + 1] <> '?' then
        exit;
      if aParams <> nil then // e.g. URI-encoded THttpRequestExtendedOptions
        FastSetString(aParams^,
          @PByteArray(aBearerToken)[PEER_CACHE_BEARERLEN + 1], bearerlen - 1);
    end
    else
      exit; // not enough input
  result := mdB64;
  if not Base64uriToBin(pointer(aBearerToken), @tok, PEER_CACHE_BEARERLEN, SizeOf(tok)) then
    exit;
  result := MessageDecode(@tok, SizeOf(tok), aMsg);
  if (result = mdOk) and
     (aExpected >= pcfBearer) and
     (aMsg.Kind <> aExpected) then
    result := mdBearer;
end;

procedure THttpPeerCrypt.LocalPeerClientSetup(const aIp: RawUtf8;
  aClient: THttpClientSocket; aRecvTimeout: integer);
var
  tls: boolean;
begin
  aClient.ResetExtendedOptions;
  if fVerboseLog then
    aClient.OnLog := fLog.DoLog;
  tls := true;
  if fClientTls.Enabled then
    aClient.TLS := fClientTls
  else if pcoSelfSignedHttps in fSettings.Options then
    aClient.TLS.IgnoreCertificateErrors := true // self-signed
  else
    tls := false;
  aClient.OpenBind(aIp, fPort, {bind=}false, tls); // try to connect
  aClient.ReceiveTimeout := aRecvTimeout; // socket timeout once connected
end;

function THttpPeerCrypt.LocalPeerRequest(const aRequest: THttpPeerCacheMessage;
  var aResp : THttpPeerCacheMessage; const aUrl: RawUtf8;
  aOutStream: TStreamRedirect; aBanInstable: boolean): integer;
var
  head, ip, method: RawUtf8;

  procedure LocalPeerRequestFailed(E: TClass);
  begin
    fLog.Add.Log(sllWarning, 'OnDownload: % %:% % failed as % %',
      [method, ip, fPort, aUrl, StatusCodeToText(result)^, E], self);
    if (fInstable <> nil) and // add to RejectInstablePeersMin list
       (E <> nil) and         // on OpenBind() error
       aBanInstable then      // not from partial request before broadcast
      fInstable.BanIP(aResp.IP4); // this peer may have a HTTP firewall issue
    FreeAndNil(fClient);
    fClientIP4 := 0;
    result := 0; // will fallback to regular GET on the main repository
  end;

begin
  result := 0;
  try
    // compute the call parameters and the request bearer
    IP4Text(@aResp.IP4, ip);
    fLog.Add.Log(LOG_DEBUGERROR[ip = ''], 'OnDownload: request %:% %',
      [ip, fPort, aUrl], self);
   if ip = '' then
      exit; // clearly invalid (sllError logged above)
    aResp.Kind := pcfBearer; // authorize OnBeforeBody with response message
    MessageEncodeBearer(aResp, head);
    // ensure we have the right peer
    if (fClient <> nil) and
       ((fClientIP4 <> aResp.IP4) or
        not fClient.SockConnected) then
      FreeAndNil(fClient);
    // ensure we have the expected HTTP/HTTPS connection
    if fClient = nil then
    begin
      fClient := THttpClientSocket.Create(fSettings.HttpTimeoutMS);
      LocalPeerClientSetup(ip, fClient, 5000);
    end;
    // makes the HEAD/GET request, optionally with the needed range bytes
    fClient.RangeStart := aRequest.RangeStart;
    fClient.RangeEnd   := aRequest.RangeEnd;
    if aOutStream = nil then
      method := 'HEAD'
    else
    begin
      if fSettings.LimitMBPerSec >= 0 then // -1 to keep original value
        aOutStream.LimitPerSecond := fSettings.LimitMBPerSec shl 20; // bytes/sec
      method := 'GET';
    end;
    result := fClient.Request(
      aUrl, method, 30000, head, '',  '', {AsRetry=}false, nil, aOutStream);
    fLog.Add.Log(sllTrace, 'OnDownload: request=%', [result], self);
    if result in HTTP_GET_OK then
      fClientIP4 := aResp.IP4 // success or not found (HTTP_NOCONTENT)
    else
      LocalPeerRequestFailed(nil); // error downloading from local peer
  except
    on E: Exception do
      LocalPeerRequestFailed(PClass(E)^);
  end;
end;

constructor THttpPeerCrypt.Create(const aSharedSecret: RawByteString;
  aServerTls, aClientTls: PNetTlsContext);
var
  key: THash256Rec;
begin
  // setup internal processing status
  fClientSafe.Init; // mandatory for TOSLightLock
  fFrameSeqLow := Random31Not0; // 31-bit random start value set at startup
  fFrameSeq := fFrameSeqLow;
  // setup internal cryptography
  if aSharedSecret = '' then
    EHttpPeerCache.RaiseUtf8('%.Create without aSharedSecret', [self]);
  HmacSha256('4b0fb62af680447c9d0604fc74b908fa', aSharedSecret, key.b);
  // favor our TAesGcm for small messages, instead of OpenSSL TAesFast[mGCM]
  fAesEnc := TAesGcm.Create(key.Lo) as TAesGcmAbstract; // lower 128-bit
  fAesDec := fAesEnc.Clone as TAesGcmAbstract; // two AES-GCM-128 instances
  HmacSha256(key.b, '2b6f48c3ffe847b9beb6d8de602c9f25', key.b); // paranoid
  fSharedMagic := key.h.c3; // 32-bit derivation for anti-fuzzing checksum
  FastSetString(fDirectSecret, @key, SizeOf(key)); // for HttpDirectUri()
  if Assigned(fLog) then
    // log includes safe 16-bit key.w[0] fingerprint
    fLog.Add.Log(sllTrace, 'Create: Uuid=% SecretFingerPrint=%, Seq=#%',
      [UuidToShort(fUuid), key.w[0], CardinalToHexShort(fFrameSeq)], self);
  FillZero(key.b);
  if aServerTls <> nil then
    fServerTls := aServerTls^;
  if aClientTls <> nil then
    fClientTls := aClientTls^;
end;

destructor THttpPeerCrypt.Destroy;
begin
  FreeAndNilSafe(fClient);
  FreeAndNil(fInstable);
  FreeAndNil(fAesEnc);
  FreeAndNil(fAesDec);
  fSharedMagic := 0;
  inherited Destroy;
  FillZero(fDirectSecret);
  fClientSafe.Done; // mandatory for TOSLightLock
end;

function THttpPeerCrypt.NetworkInterfaceChanged: boolean;
var
  newmac: TMacAddress;
  err: RawUtf8;
  tix: cardinal;
begin
  result := false;
  if self = nil then
    exit;
  tix := GetTickSec;
  if tix = fLastNetworkTix then
    exit;
  fLastNetworkTix := tix;
  MacIPAddressFlush; // thread-safe flush mormot.net.sock cache
  err := fSettings.GuessInterface(newmac);
  result := (err = '') and
            ((fMac.Name <> newmac.Name) or
             (fMac.IP <> newmac.IP) or
             (fMac.Broadcast <> newmac.Broadcast) or
             (fMac.NetMask <> newmac.NetMask));
  if Assigned(fLog) then
    fLog.Add.Log(sllTrace, 'NetworkInterfaceChanged=% [% % % %] %',
      [BOOL_STR[result], newmac.Name, newmac.IP, newmac.Broadcast, newmac.NetMask, err], self);
end;

const // URI start for pcfBearerDirect* peer requests
  DIRECTURI_32 = ord('/') + ord('h') shl 8 + ord('t') shl 16 + ord('t') shl 24;

class function THttpPeerCrypt.HttpDirectUri(const aSharedSecret: RawByteString;
  const aRemoteUri, aRemoteHash: RawUtf8;
  out aDirectUri, aDirectHeaderBearer: RawUtf8; aPermanent: boolean;
  aOptions: PHttpRequestExtendedOptions): boolean;
var
  c: THttpPeerCrypt;
  msg: THttpPeerCacheMessage;
  p: RawUtf8;
  uri: TUri;
begin
  result := false;
  if (aSharedSecret = '') or
     (aRemoteHash = '') or
     not uri.From(aRemoteUri) then
    exit;
  c := THttpPeerCrypt.Create(aSharedSecret, nil, nil);
  try
    c.MessageInit(pcfBearerDirect, 0, msg);
    if aPermanent then
      msg.Kind := pcfBearerDirectPermanent;
    if not HashDetect(aRemoteHash, msg.Hash) then
      exit;
    if uri.Port <> DEFAULT_PORT[uri.Https] then
      Join(['_', uri.Port], p); // '_' is ok for URI, but not for domain
    FormatUtf8('/%/%%/%', [uri.Scheme, uri.Server, p, uri.Address], aDirectUri);
    msg.Opaque := crc63c(pointer(aDirectUri), length(aDirectUri)); // no replay
    c.MessageEncodeBearer(msg, aDirectHeaderBearer);
    if aOptions <> nil then // extended options are URI-encoded to the bearer
      aDirectHeaderBearer :=
        aOptions^.ToUrlEncode(aDirectHeaderBearer, c.fDirectSecret);
    result := true;
  finally
    c.Free;
  end;
end;

class function THttpPeerCrypt.HttpDirectUriReconstruct(P: PUtf8Char;
  out Decoded: TUri): boolean;
var
  scheme, domain: RawUtf8;
begin
  result := false;
  if (P = nil) or
     (PCardinal(P)^ <> DIRECTURI_32) then
    exit;
  inc(P); // http/... or https/...
  GetNextItem(P, '/', scheme);
  GetNextItem(P, '/', domain); // domain/... or domain_port/...
  domain := StringReplaceChars(domain, '_', ':');
  if (domain <> '') and
     (P <> nil) then
    result := Decoded.From(Make([scheme, '://', domain, '/', P]));
end;


{ THttpPeerCacheSettings }

constructor THttpPeerCacheSettings.Create;
begin
  inherited Create;
  fPort := 8099;
  fLimitMBPerSec := 10;
  fLimitClientCount := 32;
  fRejectInstablePeersMin := 4;
  fCacheTempPath := '*';
  fCacheTempMinBytes := 2048;
  fCacheTempMaxMB := 1000;
  fCacheTempMaxMin := 60;
  fCachePermPath := '*';
  fCachePermMinBytes := 2048;
  fBroadcastTimeoutMS := 10;
  fBroadcastMaxResponses := 24;
  fBroadCastDirectMinBytes := 64 shl 10;
  fTryAllPeersCount := 10;
  fHttpTimeoutMS := 500;
end;

function THttpPeerCacheSettings.GuessInterface(out Mac: TMacAddress): RawUtf8;
begin
  FastAssignNew(result); // success
  if fInterfaceName <> '' then
  begin
    if not GetMainMacAddress(Mac, fInterfaceName, {UpAndDown=}true) then
      // an explicitly requested interface may be down
      FormatUtf8('impossible to find the [%] network interface', [fInterfaceName], result);
  end
  else if not GetMainMacAddress(Mac, [mafLocalOnly, mafUpOnly, mafRequireBroadcast]) then
    // automatic PeerCache selection requires an "up" interface
    result := 'impossible to find a local active network interface';
end;

function THttpPeerCacheSettings.HttpDirectUri(
  const aSharedSecret: RawByteString; const aRemoteUri, aRemoteHash: RawUtf8;
  out aDirectUri, aDirectHeaderBearer: RawUtf8; aForceTls, aPermanent: boolean;
  aOptions: PHttpRequestExtendedOptions): boolean;
var
  mac: TMacAddress;
begin
  result := false;
  if (self = nil) or
     (aSharedSecret = '') or
     (pcoNoServer in fOptions) or
     (GuessInterface(mac) <> '') or
     not THttpPeerCrypt.HttpDirectUri(aSharedSecret, aRemoteUri, aRemoteHash,
           aDirectUri, aDirectHeaderBearer, aPermanent, aOptions) then
    exit;
  aForceTls := aForceTls or (pcoSelfSignedHttps in fOptions);
  aDirectUri := Make([HTTPS_TEXT[aForceTls], mac.IP, ':', fPort, aDirectUri]);
  result := true;
end;


{ THttpPeerCacheThread }

constructor THttpPeerCacheThread.Create(Owner: THttpPeerCache);
begin
  fOwner := Owner;
  fBroadcastSafe.Init;
  fBroadcastAddr.SetIP4Port(fOwner.fBroadcastIP4, fOwner.Settings.Port);
  fBroadcastAddr.IPWithPort(fBroadcastIpPort);
  fBroadcastEvent := TSynEvent.Create;
  // POSIX requires to bind to the broadcast address to receive brodcasted frames
  inherited Create(fOwner.fLog,
    fOwner.fMac.{$ifdef OSPOSIX}Broadcast{$else}IP{$endif}, // OS-specific
    fOwner.fPort, 'udp-PeerCache', 100);
end;

destructor THttpPeerCacheThread.Destroy;
begin
  if Assigned(fBroadcastEvent) and
     (fBroadcastCurrentSeq <> 0) then
    fBroadcastEvent.SetEvent;
  inherited Destroy;
  fBroadcastEvent.Free;
  fBroadcastSafe.Done;
end;

const
  _LATE: array[boolean] of TShort7 = ('', 'late ');

procedure THttpPeerCacheThread.OnFrameReceived(len: integer;
  var remote: TNetAddr);
var
  msg, resp: THttpPeerCacheMessage;

  procedure DoLog(const Fmt: RawUtf8; const Args: array of const);
  var
    txt: ShortString;
    us: TShort16;
    stop: Int64;
  begin
    if not fOwner.fLog.HasLevel([sllTrace]) then
      exit;
    FormatShort(Fmt, Args, txt);
    us[0] := #0;
    if (fOwner.fBroadcastStart <> 0) and
       (msg.Kind in PCF_RESPONSE) then
    begin
      QueryPerformanceMicroSeconds(stop);
      dec(stop, fOwner.fBroadcastStart);
      if stop > 0 then
        MicroSecToStringVar(stop, us);
    end;
    fOwner.fLog.Add.Log(sllTrace, 'OnFrameReceived: % % %',
      [remote.IP4Short, txt, us], self);
  end;

  procedure DoSendResponse;
  var
    res: TNetResult;
    sock: TNetSocket;
    frame: THttpPeerCacheMessageEncoded;
  begin
    // compute PCF_RESPONSE frame
    resp.DestIP4 := remote.IP4; // notify actual source IP (over broadcast)
    fOwner.MessageEncode(resp, frame);
    // respond on main UDP port and on broadcast (POSIX) or local (Windows) IP
    if msg.Os.os = osWindows then
    begin // local IP is good enough on Windows
      remote.SetPort(fBroadcastAddr.Port);
      sock := remote.NewSocket(nlUdp);
    end
    else
    begin // need to broadcast
      remote.SetIP4Port(fOwner.fBroadcastIP4, fBroadcastAddr.Port);
      sock := remote.NewSocket(nlUdp);
      sock.SetBroadcast(true);
    end;
    res := sock.SendTo(@frame, SizeOf(frame), remote);
    sock.Close;
    if fOwner.fVerboseLog then
      DoLog('send %=%', [ToText(resp.Kind)^, _NR[res]]);
    inc(fSent);
  end;

var
  ok: THttpPeerCryptMessageDecode;
  late: boolean;
begin
  // quick return if this frame is not worth decoding
  if (fOwner = nil) or
     fOwner.fShutdown or             // avoid random GPF at shutdown
     (remote.IP4 = 0) or
     (remote.IP4 = fOwner.fIP4) then // Windows broadcasts to self :)
    exit;
  // RejectInstablePeersMin option: validate the input frame IP
  PByte(@msg.Kind)^ := 255; // ToText(msg.Kind)^ = ''
  if fOwner.fInstable.IsBanned(remote) then
  begin
    if fOwner.fVerboseLog then
      DoLog('banned /%', [fOwner.fInstable.Count]);
    exit;
  end;
  // validate the input frame content
  ok := fOwner.MessageDecode(pointer(fFrame), len, msg);
  if fOwner.Check(ok, 'OnFrameReceived', msg) then
    if (msg.Kind in PCF_RESPONSE) and    // responses are broadcasted on POSIX
       (msg.DestIP4 <> fOwner.fIP4) then // will also detect any unexpected NAT
     begin
       if fOwner.fVerboseLog then
         DoLog('ignored % %<>%', [ToText(msg.Kind)^,
           IP4ToShort(@msg.DestIP4), fOwner.fIpPort]);
       exit;
     end;
  late := (msg.Kind in PCF_RESPONSE) and
          (msg.Seq <> fBroadcastCurrentSeq);
  if fOwner.fVerboseLog then
    if ok = mdOk then
      DoLog('%%', [_LATE[late], ToText(msg)])
    else if ok <= mdAes then // decoding error
      DoLog('unexpected % len=% [%]',
        [ToText(ok)^, len, EscapeToShort(pointer(fFrame), len)]);
  // process the frame message
  if ok = mdOk then
    case msg.Kind of
      pcfPing:
        begin
          fOwner.MessageInit(pcfPong, msg.Seq, resp);
          DoSendResponse;
        end;
      pcfRequest:
        begin
          fOwner.MessageInit(pcfResponseNone, msg.Seq, resp);
          resp.Hash := msg.Hash;
          if not (pcoNoServer in fOwner.Settings.Options) then
            if integer(fOwner.fHttpServer.ConnectionsActive) >
                        fOwner.Settings.LimitClientCount then
              resp.Kind := pcfResponseOverloaded
            else if fOwner.LocalFileName(
                             msg, [], nil, @resp.Size) = HTTP_SUCCESS then
              resp.Kind := pcfResponseFull
            else if fOwner.PartialFileName(
                             msg, nil, nil, @resp.Size) = HTTP_SUCCESS then
              resp.Kind := pcfResponsePartial;
          DoSendResponse;
        end;
      pcfPong,
      pcfResponsePartial,
      pcfResponseFull:
        if not late then
        begin
          inc(fResponses);
          if AddResponseAndDone(msg) then
          begin
            fBroadcastCurrentSeq := 0;   // ignore next responses
            fBroadcastEvent.SetEvent;    // notify MessageBroadcast
          end;
        end;
      pcfResponseNone,
      pcfResponseOverloaded:
        if not late then
          inc(fResponses);
    end
  else // not ok = this UDP packet is invalid
    if fOwner.fInstable <> nil then // RejectInstablePeersMin
      if fOwner.fInstable.BanIP(remote.IP4) then
        DoLog('ban IP after %', [ToText(ok)^]);
end;

function SortMessagePerPriority(const VA, VB): integer;
var
  a: THttpPeerCacheMessage absolute VA;
  b: THttpPeerCacheMessage absolute VB;
begin
  result := CompareCardinal(ord(b.Kind), ord(a.Kind));
  if result <> 0 then // pcfResponseFull first
    exit;
  result := CompareCardinal(NETHW_ORDER[a.Hardware], NETHW_ORDER[b.Hardware]);
  if result <> 0 then // ethernet first
    exit;
  result := CompareCardinal(b.Speed, a.Speed);
  if result <> 0 then // highest speed first
    exit;
  result := CompareCardinal(a.Connections, b.Connections);
  if result <> 0 then // less active
    exit;
  result := ComparePointer(@a, @b); // by pointer in a dynamic array = received first
end;

function THttpPeerCacheThread.Broadcast(const aReq: THttpPeerCacheMessage;
  out aAlone: boolean): THttpPeerCacheMessageDynArray;
var
  sock: TNetSocket;
  res: TNetResult;
  frame: THttpPeerCacheMessageEncoded;
  start, stop: Int64;
begin
  result := nil;
  if self = nil then
    exit;
  QueryPerformanceMicroSeconds(start);
  fOwner.MessageEncode(aReq, frame);
  fBroadcastSafe.Lock; // serialize OnDownload() or Ping() calls
  try
    // setup this broadcasting sequence
    QueryPerformanceMicroSeconds(fOwner.fBroadcastStart);
    fBroadcastEvent.ResetEvent;
    fBroadcastCurrentSeq := aReq.Seq; // ignore any other responses
    fResponses := 0; // reset counter for this broadcast (not late)
    // broadcast request over the UDP sub-net of the selected network interface
    sock := fBroadcastAddr.NewSocket(nlUdp);
    if sock = nil then
      exit;
    try
      sock.SetBroadcast(true);
      res := sock.SendTo(@frame, SizeOf(frame), fBroadcastAddr);
      if fOwner.fVerboseLog then
        fOwner.fLog.Add.Log(sllTrace, 'Broadcast: % % = %',
          [fBroadcastIpPort, ToText(aReq), _NR[res]], self);
      if res <> nrOk then
        exit;
    finally
      sock.Close;
    end;
    // wait for the (first) response(s)
    fBroadcastEvent.WaitFor(fOwner.Settings.BroadcastTimeoutMS);
    result := GetResponses(aReq.Seq);
  finally
    fBroadcastCurrentSeq := 0; // ignore any late responses
    fOwner.fBroadcastStart := 0;
    aAlone := (fResponses = 0);
    fBroadcastSafe.UnLock;
  end;
  // select the best responses
  if length(result) > 1 then
    DynArray(TypeInfo(THttpPeerCacheMessageDynArray), result).
      Sort(SortMessagePerPriority);
  QueryPerformanceMicroSeconds(stop);
  fOwner.fLog.Add.Log(sllTrace, 'Broadcast: %=%/% in %', [ToText(aReq.Kind)^,
    length(result), fResponses, MicroSecToString(stop - start)], self);
end;

function THttpPeerCacheThread.AddResponseAndDone(
  const aMessage: THttpPeerCacheMessage): boolean;
begin
  if fRespCount < fOwner.Settings.BroadcastMaxResponses then
  begin
    fRespSafe.Lock;
    try
      if fRespCount = length(fResp) then
        SetLength(fResp, NextGrow(fRespCount));
      fResp[fRespCount] := aMessage;
      inc(fRespCount);
    finally
      fRespSafe.UnLock;
    end;
  end;
  result := (pcoUseFirstResponse in fOwner.Settings.Options) or
            (fRespCount >= fOwner.Settings.BroadcastMaxResponses);
end;

function THttpPeerCacheThread.GetResponses(
  aSeq: cardinal): THttpPeerCacheMessageDynArray;
var
  i, c, n: PtrInt;
begin
  result := nil;
  // retrieve the pending responses
  fBroadcastCurrentSeq := 0; // no more reponse from now on
  fRespSafe.Lock;
  try
    if fRespCount = 0 then
      exit;
    pointer(result) := pointer(fResp); // assign with no refcount
    pointer(fResp) := nil;
    c := fRespCount;
    fRespCount := 0;
  finally
    fRespSafe.UnLock;
  end;
  // filter the responses matching aSeq (paranoid)
  n := 0;
  for i := 0 to c - 1 do
    if result[i].Seq = aSeq then
    begin
      if i <> n then
        result[n] := result[i];
      inc(n);
    end;
  if n = 0 then
    result := nil
  else
    DynArrayFakeLength(result, n);
end;

procedure THttpPeerCacheThread.OnIdle(tix64: Int64);
begin
  fOwner.OnIdle(tix64); // do nothing but once every minute
end;

procedure THttpPeerCacheThread.OnShutdown;
begin
  // nothing to be done in our case
end;


{ THttpPeerCache }

constructor THttpPeerCache.Create(aSettings: THttpPeerCacheSettings;
  const aSharedSecret: RawByteString;
  aHttpServerClass: THttpServerSocketGenericClass;
  aHttpServerThreadCount: integer; aLogClass: TSynLogClass;
  aServerTls, aClientTls: PNetTlsContext);
var
  ilog: ISynLog;
  avail, existing: Int64;
begin
  fLog := aLogClass;
  if fLog = nil then
    fLog := TSynLog;
  fLog.EnterLocal(ilog, 'Create threads=% %', [aHttpServerThreadCount, aLogClass], self);
  fFilesSafe.Init;
  // initialize the cryptographic state in inherited THttpPeerCrypt.Create
  if (fSettings = nil) or
     (fSettings.Uuid = '') then // allow UUID customization
    GetComputerUuid(fUuid)
  else if not RawUtf8ToGuid(fSettings.Uuid, fUuid) then
    EHttpPeerCache.RaiseUtf8('Invalid %.Create(uuid=%)', [self, fSettings.Uuid]);
  inherited Create(aSharedSecret, aServerTls, aClientTls);
  // setup the processing options
  if aSettings = nil then
  begin
    fSettings := THttpPeerCacheSettings.Create;
    fSettingsOwned := true;
  end
  else
    fSettings := aSettings;
  fVerboseLog := (pcoVerboseLog in fSettings.Options) and
                 fLog.HasLevel([sllTrace]);
  // check the temporary files cache folder and its maximum allowed size
  if fSettings.CacheTempPath = '*' then // not customized
    fSettings.CacheTempPath := TemporaryFileName;
  fTempCurrentSize := -1; // will force compute current folder size
  fTempFilesPath := EnsureDirectoryExists(fSettings.CacheTempPath);
  if fTempFilesPath <> '' then
  begin
    OnIdle(0); // initial clean-up
    fTempFilesMaxSize := Int64(fSettings.CacheTempMaxMB) shl 20;
    avail := GetDiskAvailable(fTempFilesPath);
    existing := DirectorySize(fTempFilesPath, false, PEER_CACHE_PATTERN);
    if Assigned(ilog) then
      ilog.Log(sllDebug, 'Create: % folder has % available, with % existing cache',
        [fTempFilesPath, KB(avail), KB(existing)], self);
    if (avail <> 0) and
       not (pcoCacheTempNoCheckSize in fSettings.Options) then
    begin
      avail := (avail + existing) shr 2;
      if fTempFilesMaxSize > avail then
      begin
        fTempFilesMaxSize := avail; // allow up to 25% of the folder capacity
        if Assigned(ilog) then
          ilog.Log(sllDebug, 'Create: trim CacheTempMax=%', [KB(avail)], self);
      end;
    end;
  end;
  // ensure we have somewhere to cache
  if fSettings.CachePermPath = '*' then // not customized
    fSettings.CachePermPath := MakePath(
      [GetSystemPath(spCommonData), Executable.ProgramName, 'permcache']);
  fPermFilesPath := EnsureDirectoryExists(fSettings.CachePermPath);
  if (fTempFilesPath = '') and
     (fPermFilesPath = '') then
    EHttpPeerCache.RaiseUtf8('%.Create: no cache defined', [self]);
  // retrieve the local network interface (in inherited THttpPeerCrypt)
  AfterSettings; // fSettings should have been defined
  // start the local UDP server on this interface
  fUdpServer := THttpPeerCacheThread.Create(self);
  if Assigned(ilog) then
    ilog.Log(sllTrace, 'Create: started %', [fUdpServer], self);
  // start the local HTTP/HTTPS server on this interface
  if not (pcoNoServer in fSettings.Options) then
  begin
    StartHttpServer(aHttpServerClass, aHttpServerThreadCount, fIpPort);
    fHttpServer.ServerName := Executable.ProgramName;
    fHttpServer.OnBeforeBody := OnBeforeBody;
    fHttpServer.OnRequest := OnRequest;
    if Assigned(ilog) then
      ilog.Log(sllDebug, 'Create: started %', [fHttpServer], self);
  end;
end;

procedure THttpPeerCache.StartHttpServer(
  aHttpServerClass: THttpServerSocketGenericClass;
  aHttpServerThreadCount: integer; const aIP: RawUtf8);
var
  opt: THttpServerOptions;
  srv: THttpServerSocketGeneric;
begin
  if fClientTls.Enabled <> fServerTls.Enabled then
    EHttpPeerCache.RaiseUtf8(
      '%.StartHttpServer: inconsistent ClientTls=% ServerTls=%',
      [self, fClientTls.Enabled, fServerTls.Enabled]);
  if aHttpServerClass = nil then
    aHttpServerClass := THttpServer; // classic per-client thread server is enough
  opt := [hsoNoXPoweredHeader, hsoThreadSmooting];
  if not (pcoNoBanIP in fSettings.Options) then // RejectInstablePeersMin = UDP
    include(opt, hsoBan40xIP);
  if fVerboseLog then
    include(opt, hsoLogVerbose);
  if fServerTls.Enabled or
     (pcoSelfSignedHttps in fSettings.Options) then
    include(opt, hsoEnableTls);
  fHttpServer := aHttpServerClass.Create(aIP, nil, nil, 'PeerCache',
    aHttpServerThreadCount, 30000, opt, fLog);
  if aHttpServerClass.InheritsFrom(THttpServerSocketGeneric) then
  begin
    srv := fHttpServer as THttpServerSocketGeneric;
    // no TCP/HTTP ban from localhost
    if Assigned(srv.Banned) then
      srv.Banned.WhiteIP := fIP4;
    // note: both THttpServer and THttpAsyncServer support rfProgressiveStatic
    fPartials := THttpPartials.Create;
    if fVerboseLog then
      fPartials.OnLog := fLog.DoLog;
    srv.SetProgressiveRequests(fPartials);
    // actually start and wait for the local HTTP(S) server to be available
    if fServerTls.Enabled then
    begin
      fLog.Add.Log(sllTrace, 'StartHttpServer: HTTPS from ServerTls using %',
        [fServerTls.PrivateKeyFile], self);
      srv.WaitStarted(10, @fServerTls);
    end
    else if pcoSelfSignedHttps in fSettings.Options then
    begin
      fLog.Add.Log(sllTrace, 'StartHttpServer: self-signed HTTPS', self);
      srv.WaitStartedHttps(10);
    end
    else
    begin
      fLog.Add.Log(sllTrace, 'StartHttpServer: plain HTTP', self);
      srv.WaitStarted(10);
    end;
  end;
end;

function THttpPeerCache.CurrentConnections: integer;
begin
  if Assigned(fSettings) and
     (pcoNoServer in fSettings.Options) then
    result := 0
  else
    result := fHttpServer.ConnectionsActive;
end;

destructor THttpPeerCache.Destroy;
var
  ilog: ISynLog;
begin
  fLog.EnterLocal(ilog, 'Destroy % %', [fUdpServer, fHttpServer], self);
  fShutdown := true;          // set internal flag for background process
  FreeAndNilSafe(fUdpServer); // stop servers first
  FreeAndNilSafe(fHttpServer);
  FreeAndNilSafe(fPartials);
  if fSettingsOwned then
    fSettings.Free;
  fFilesSafe.Done;
  inherited Destroy;
end;

function THttpPeerCache.Check(Status: THttpPeerCryptMessageDecode;
  const Ctxt: ShortString; const Msg: THttpPeerCacheMessage): boolean;
var
  msgtxt: ShortString;
begin
  result := (Status = mdOk);
  if result or
     not fLog.HasLevel([sllTrace]) then
    exit;
  msgtxt[0] := #0;
  if Status > mdAes then // decrypt ok but wrong content: log msg
    MsgToShort(Msg, msgtxt);
  fLog.Add.Log(sllTrace, '%: decode=% %', [Ctxt, ToText(Status)^, msgtxt], self);
end;

function THttpPeerCache.State: TWGetAlternateState;
begin
  result := [];
  if (self = nil) or
     not Assigned(fSettings) then // nil at shutdown
    exit;
  if Assigned(fHttpServer) and
     (fHttpServer.CurrentProcess > 0) then
    include(result, gasProcessing);
  if not fPartials.IsVoid then
    include(result, gasPartials);
end;

function THttpPeerCache.ComputeFileName(const aHash: THashDigest): TFileName;
begin
  // filename is binary algo + hash encoded as hexadecimal up to 520-bit
  result := FormatString('%.cache',
    [BinToHexLower(@aHash, SizeOf(aHash.Algo) + HASH_SIZE[aHash.Algo])]);
  // note: it does not make sense to obfuscate this file name because the file
  // content itself is unencrypted on disk, so the hash can be recomputed anyway
end;

function THttpPeerCache.PermFileName(const aFileName: TFileName;
  aFlags: THttpPeerCacheLocalFileName): TFileName;
begin
  if Assigned(fSettings) and
     (pcoCacheTempSubFolders in fSettings.Options) then
  begin
    // create sub-folders using the first hash nibble (0..9/a..z), in a way
    // similar to git - aFileName[1..2] is the algorithm, so hash starts at [3]
    MakePath([fPermFilesPath, aFileName[3]], result);
    if lfnEnsureDirectoryExists in aFlags then
      result := EnsureDirectoryExistsNoExpand(result);
    result := result + aFileName;
  end
  else
    result := fPermFilesPath + aFileName;
end;

function THttpPeerCache.LocalFileName(const aMessage: THttpPeerCacheMessage;
  aFlags: THttpPeerCacheLocalFileName; aFileName: PFileName;
  aSize: PInt64): integer;
var
  perm, temp, name, fn: TFileName;
  size: Int64;
begin
  name := ComputeFileName(aMessage.Hash);
  if fPermFilesPath <> '' then
    perm := PermFileName(name, aFlags); // with pcoCacheTempSubFolders support
  if fTempFilesPath <> '' then
    temp := fTempFilesPath + name;
  fFilesSafe.Lock; // disable any concurrent file access
  try
    size := FileSize(perm); // fast syscall on all platforms
    if size <> 0 then // found in permanent cache folder
      fn := perm
    else
    begin
      size := FileSize(temp);
      if size <> 0 then // found in temporary cache folder
      begin
        fn := temp;
        if lfnSetDate in aFlags then
          FileSetDateFromUnixUtc(temp, UnixTimeUtc); // renew TTL
      end;
    end;
  finally
    fFilesSafe.UnLock;
  end;
  if fVerboseLog then
    if fn = '' then
      fLog.Add.Log(sllTrace, 'LocalFileName: none', self)
    else
      fLog.Add.Log(sllTrace, 'LocalFileName: % size=% msg: size=% start=% end=%',
        [fn, size, aMessage.Size, aMessage.RangeStart, aMessage.RangeEnd], self);
  result := HTTP_NOTFOUND;
  if size = 0 then
    exit; // not existing
  result := HTTP_NOTACCEPTABLE; // 406
  if (aMessage.Size <> 0) and
     // ExpectedSize may be 0 if waoNoHeadFirst was set, or for pcfBearerDirect*
     (size <> aMessage.Size) then
    exit; // invalid file
  result := HTTP_SUCCESS;
  if aFileName <> nil then
    aFileName^ := fn;
  if aSize <> nil then
    aSize^ := size;
end;

function WGetToHash(const Params: THttpClientSocketWGet;
  out Hash: THashDigest): boolean;
begin
  result := (Params.Hash <> '') and
            (Params.Hasher <> nil) and
            Params.Hasher.InheritsFrom(TStreamRedirectSynHasher) and
       TStreamRedirectSynHasherClass(Params.Hasher).GetHashDigest(Params.Hash, Hash);
end;

function THttpPeerCache.CachedFileName(const aParams: THttpClientSocketWGet;
  aFlags: THttpPeerCacheLocalFileName;
  out aLocal: TFileName; out isTemp: boolean): boolean;
var
  hash: THashDigest;
begin
  if not WGetToHash(aParams, hash) then
  begin
    result := false;
    exit;
  end;
  aLocal := ComputeFileName(hash);
  isTemp := (fPermFilesPath = '') or
            not (waoPermanentCache in aParams.AlternateOptions);
  if isTemp then
    aLocal := fTempFilesPath + aLocal
  else
    aLocal := PermFileName(aLocal, aFlags); // with sub-folder
  result := true;
end;

function THttpPeerCache.TempFolderEstimateNewSize(aAddingSize: Int64): Int64;
var
  i: integer;
  deleted: Int64;
  dir: TFindFilesDynArray;
  d: PFindFiles;
  fn: TFileName;
begin
  // check if the file is clearly too big to be cached
  result := aAddingSize;
  if result > fTempFilesMaxSize then
    exit;
  // compute the current folder cache size
  result := fTempCurrentSize;
  if result < 0 then // first time, or after OnIdle DirectoryDeleteOlderFiles()
  begin
    dir := FindFiles(fTempFilesPath, PEER_CACHE_PATTERN, '', [ffoExcludesDir]);
    result := FindFilesSize(dir);
    fTempCurrentSize := result; // may be 0
  end;
  // enough space to write this file?
  inc(result, aAddingSize); // simulate adding this file
  if result < fTempFilesMaxSize then
    exit;
  // delete oldest files in cache up to CacheTempMaxMB
  if dir = nil then
    dir := FindFiles(fTempFilesPath, PEER_CACHE_PATTERN, '', [ffoExcludesDir]);
  FindFilesSortByTimestamp(dir); // only sort if needed
  deleted := 0;
  d := pointer(dir);
  for i := 1 to length(dir) do
  begin
    fn := fTempFilesPath + d^.Name;
    if not fPartials.Find(fn) then // if not currently downloading
      if DeleteFile(fn) then
      begin
        dec(result, d^.Size);
        inc(deleted, d^.Size);
        if result < fTempFilesMaxSize then
          break; // we have deleted enough old files
      end;
    inc(d);
  end;
  fLog.Add.Log(sllTrace, 'OnDownloaded: deleted %', [KB(deleted)], self);
  dec(fTempCurrentSize, deleted);
end;

function THttpPeerCache.TooSmallFile(const aParams: THttpClientSocketWGet;
  aSize: Int64; const aCaller: ShortString): boolean;
var
  minsize: Int64;
begin
  result := false; // continue
  if fShutdown or
     (waoNoMinimalSize in aParams.AlternateOptions) then
    exit;
  if (waoPermanentCache in aParams.AlternateOptions) and
     (fPermFilesPath <> '') then
    minsize := fSettings.CachePermMinBytes
  else
    minsize := fSettings.CacheTempMinBytes;
  if aSize >= minsize then
    exit; // big enough
  if fVerboseLog then
    fLog.Add.Log(sllTrace, '%: size < minsize=%', [aCaller, KB(minsize)], self);
  result := true; // too small
end;

function THttpPeerCache.OnDownload(Sender: THttpClientSocket;
  var Params: THttpClientSocketWGet; const Url: RawUtf8;
  ExpectedFullSize: Int64; OutStream: TStreamRedirect): integer;
var
  req: THttpPeerCacheMessage;
  resp : THttpPeerCacheMessageDynArray;
  fn: TFileName;
  u: RawUtf8;
  local: TFileStreamEx;
  i: PtrInt;
  tix: cardinal;
  outStreamInitialPos: Int64;
  alone: boolean;
  log: ISynLog;
  l: TSynLog;

  function ResetOutStreamPosition: boolean;
  begin
    // not moved (e.g. first request, or connection issue) returns true
    result := (OutStream.Position = outStreamInitialPos) and
              not fShutdown;
    if not result then
      // TStreamRedirect requires full rewind for full content re-hash
      if outStreamInitialPos = 0 then
        result := OutStream.Seek(0, soBeginning) = 0; // will call ResetHash
      // TODO: fix range support - TStreamRedirect.Seek/ResetHash after Append()
  end;

begin
  result := 0;
  // validate WGet caller context
  if (self = nil) or
     fShutdown or
     (Sender = nil) or
     (Params.Hash = '') or
     (Url = '') or
     (OutStream = nil) then
    exit;
  if not Params.Hasher.InheritsFrom(TStreamRedirectSynHasher) then
    EHttpPeerCache.RaiseUtf8('%.OnDownload: unexpected %', [Params.Hasher]);
  outStreamInitialPos := OutStream.Position;
  // prepare a request frame
  l := nil;
  fLog.EnterLocal(log, 'OnDownload % % % %', [KBNoSpace(ExpectedFullSize),
    Params.Hasher.GetHashName, Params.Hash, Url], self);
  if Assigned(log) then // log=nil if fLog=nil or sllEnter is not enabled
    l := log.Instance;
  MessageInit(pcfRequest, 0, req);
  if not WGetToHash(Params, req.Hash) then
  begin
    l.Log(sllWarning, 'OnDownload: invalid hash=%', [Params.Hash], self);
    exit;
  end;
  req.Size := ExpectedFullSize; // may be 0 if waoNoHeadFirst
  req.RangeStart := Sender.RangeStart;
  req.RangeEnd := Sender.RangeEnd;
  // always check if we don't already have this file cached locally
  if not (pcoNoServer in fSettings.Options) and
     (LocalFileName(req, [lfnSetDate], @fn, @req.Size) = HTTP_SUCCESS) then
  begin
    l.Log(sllDebug, 'OnDownload: from local %', [fn], self);
    local := TFileStreamEx.Create(fn, fmOpenReadShared);
    try
      // range support
      if req.RangeStart > 0 then
        req.RangeStart := local.Seek(req.RangeStart, soBeginning);
      if (req.RangeEnd <= 0) or
         (req.RangeEnd >= req.Size) then
        req.RangeEnd := req.Size - 1;
      req.Size := req.RangeEnd - req.RangeStart + 1;
      // fetch the data
      if req.Size > 0 then
      begin
        OutStream.LimitPerSecond := 0; // not relevant within the same process
        OutStream.CopyFrom(local, req.Size);
      end;
    finally
      local.Free;
    end;
    if req.RangeStart > 0 then
      result := HTTP_PARTIALCONTENT
    else
      result := HTTP_SUCCESS;
    Params.SetStep(wgsAlternateFromCache, [fn]);
    exit;
  end;
  // ensure the file is big enough for broadcasting
  if (ExpectedFullSize <> 0) and
     TooSmallFile(Params, ExpectedFullSize, 'OnDownload') then
    exit; // you are too small, buddy
  // try first the current/last HTTP peer client (if any)
  FormatUtf8('?%=%', [Sender.Server, Url], u); // just <> DIRECTURI_32 (for log)
  if (fClient <> nil) and
     fClient.SockConnected and
     (fClientIP4 <> 0) and
     ((pcoTryLastPeer in fSettings.Options) or
      (waoTryLastPeer in Params.AlternateOptions)) and
     fClientSafe.TryLock then
    try
      SetLength(resp, 1); // create a "fake" response to reuse this connection
      resp[0] := req;
      FillZero(resp[0].Uuid);
      // OnRequest() returns HTTP_NOCONTENT (204) - and not 404 - if not found
      result := LocalPeerRequest(req, resp[0], u, OutStream, {BanInstable=}false);
      if result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT] then
      begin
        Params.SetStep(wgsAlternateLastPeer, [fClient.Server]);
        exit; // successful direct downloading from last peer
      end;
      result := 0; // may be HTTP_NOCONTENT (204) if not found on this peer
    finally
      fClientSafe.UnLock;
    end;
  // broadcast the request over UDP
  if not Assigned(fSettings) then
    exit;
  tix := 0;
  if (pcoBroadcastNotAlone in fSettings.Options) or
     (waoBroadcastNotAlone in Params.AlternateOptions) then
  begin
    tix := GetTickSec + 1;
    if fBroadcastTix = tix then  // disable broadcasting within up to 1s delay
      exit;
  end;
  Params.SetStep(wgsAlternateBroadcast, [fUdpServer.fBroadcastIpPort]);
  resp := fUdpServer.Broadcast(req, alone);
  if (resp = nil) or
     not Assigned(fSettings) then
  begin
    if (tix <> 0) and // pcoBroadcastNotAlone
       alone then
      fBroadcastTix := tix; // no broadcast within the next second
    exit; // no match
  end;
  fBroadcastTix := 0; // resp<>nil -> broadcasting seems fine
  // HTTP/HTTPS request over the best peer corresponding to this response
  Params.SetStep(wgsAlternateGet, [IP4ToShort(@resp[0].IP4)]);
  if not ResetOutStreamPosition then
    exit; // partial download would fail the hash anyway
  fClientSafe.Lock;
  try
    result := LocalPeerRequest(req, resp[0], u, OutStream, {BanInstable=}true);
  finally
    fClientSafe.UnLock;
  end;
  if (result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT]) or
     not ResetOutStreamPosition or // rewind OutStream for main server fallback
     not ((pcoTryAllPeers in fSettings.Options) or
          (waoTryAllPeers in Params.AlternateOptions)) then
    exit;
  // try up to the best TryAllPeersCount peers of our broadcast response
  for i := 1 to MinPtrInt(length(resp), fSettings.TryAllPeersCount) - 1 do
    if not fInstable.IsBanned(resp[i].IP4) then // banned in-between (unlikely)
      if fClientSafe.TryLock then
      try
        Params.SetStep(wgsAlternateGetNext, [IP4ToShort(@resp[i].IP4)]);
        result := LocalPeerRequest(req, resp[i], u, OutStream, {Ban=}true);
        if (result in [HTTP_SUCCESS, HTTP_PARTIALCONTENT]) or
           not ResetOutStreamPosition then
          exit; // success
      finally
        fClientSafe.UnLock;
      end;
end;

function THttpPeerCache.Ping: THttpPeerCacheMessageDynArray;
var
  req: THttpPeerCacheMessage;
  alone: boolean;
begin
  MessageInit(pcfPing, 0, req);
  result := fUdpServer.Broadcast(req, alone);
end;

type
  TOnBeforeBodyErr = set of (
    eShutdown, eBearer, eNoGetHead, eUrl, eIp1, eBanned, eDecode, eIp2, eUuid,
    eDirectIp, eDirectDecode, eDirectKind, eDirectOpaque, aDirectDisabled);

function THttpPeerCache.OnBeforeBody(var aUrl, aMethod, aInHeaders,
  aInContentType, aRemoteIP, aBearerToken: RawUtf8; aContentLength: Int64;
  aFlags: THttpServerRequestFlags): cardinal;
var
  msg: THttpPeerCacheMessage;
  ip4: cardinal;
  err: TOnBeforeBodyErr;
begin
  // should return HTTP_SUCCESS=200 to continue the process, or an HTTP
  // error code to reject the request immediately as a "TeaPot", close the
  // socket and ban this IP for a few seconds at accept() level
  PByte(@msg.Kind)^ := 255; // ToText(msg.Kind)^ = ''
  err := [];
  if fShutdown then
    include(err, eShutdown); // avoid GPF at shutdown
  if length(aBearerToken) < PEER_CACHE_BEARERLEN then // base64uri length
    include(err, eBearer);
  if (aMethod = '') or
     not (IsGet(aMethod) or
          IsHead(aMethod)) then
    include(err, eNoGetHead);
  if aUrl = '' then // URI is just ignored but something should be specified
    include(err, eUrl)
  else if err = [] then
    if PCardinal(aUrl)^ = DIRECTURI_32 then // start with '/htt'
    begin
      // pcfBearerDirect* for pcoHttpDirect mode: /https/microsoft.com/...
      if (aRemoteIp <> '') and
         (PCardinal(aRemoteIP)^ <> HOST_127) and
         (aRemoteIP <> fMac.IP) then
        include(err, eDirectIp); // only accepted from local
      if not Check(BearerDecode(aBearerToken, pcfRequest, msg),
               'OnBeforeBody Direct', msg) then
        include(err, eDirectDecode)
      else
      begin
        if not (msg.Kind >= pcfBearerDirect) then
          include(err, eDirectKind);
        if Int64(msg.Opaque) <> crc63c(pointer(aUrl), length(aUrl)) then
          include(err, eDirectOpaque); // see THttpPeerCrypt.HttpDirectUri()
      end;
      if not (pcoHttpDirect in fSettings.Options) then
        include(err, aDirectDisabled);
    end
    else
    begin
      // pcfBearer for regular request in broadcasting mode
      if not IPToCardinal(aRemoteIP, ip4) then
        include(err, eIp1)
      else if fInstable.IsBanned(ip4) then // banned from RejectInstablePeersMin
        include(err, eBanned);
      if err = [] then
        if Check(BearerDecode(aBearerToken, pcfBearer, msg), 'OnBeforeBody', msg) then
        begin
          if msg.IP4 <> fIP4 then
            include(err, eIp2);
          if not ((IsZero(THash128(msg.Uuid)) or // IsZero for "fake" response bearer
                 IsEqualGuid(msg.Uuid, fUuid))) then
            include(err, eUuid);
        end
        else
          include(err, eDecode);
    end;
  result := HTTP_SUCCESS;
  if err <> [] then
    result := HTTP_FORBIDDEN;
  if fVerboseLog or
     (err <> []) then
    fLog.Add.Log(sllTrace, 'OnBeforeBody %=% % % % [%]',
      [aMethod, result, aRemoteIP, ToText(msg.Kind)^, aUrl,
       GetSetNameShort(TypeInfo(TOnBeforeBodyErr), @err)], self);
end;

procedure THttpPeerCache.OnIdle(tix64: Int64);
var
  tix: cardinal;
  size: Int64;
begin
  // avoid GPF at shutdown
  if fShutdown then
    exit;
  // check state every minute (65,536 seconds)
  if tix64 = 0 then
    tix64 := mormot.core.os.GetTickCount64;
  tix := (tix64 shr 16) + 1;
  // renew banned peer IPs TTL to implement RejectInstablePeersMin
  if (fInstable <> nil) and
     (fInstableTix <> tix) then
  begin
    fInstableTix := tix;
    if fInstable.Count <> 0 then
      if fInstable.DoRotate <> 0 then
        fLog.Add.Log(sllTrace, 'OnIdle: %', [fInstable], self);
  end;
  // check the temp folder every minute
  if fTempFilesTix <> tix then
    if fFilesSafe.TryLock then // within the lock
    try
      fTempFilesTix := tix;
      fTempCurrentSize := -1; // call FindFiles() again every minute
      // handle temporary cache folder deprecation
      if (fSettings.CacheTempMaxMin <= 0) or
         (fTempFilesPath = '') then
        exit;
      size := 0;
      DirectoryDeleteOlderFiles(fTempFilesPath,
        fSettings.CacheTempMaxMin / MinsPerDay, PEER_CACHE_PATTERN, false, @size);
      if size <> 0 then // something changed on disk
        fLog.Add.Log(sllTrace, 'OnIdle: deleted %', [KBNoSpace(size)], self);
    finally
      fFilesSafe.UnLock; // re-allow background file access
    end;
end;

procedure THttpPeerCache.OnDownloaded(var Params: THttpClientSocketWGet;
  const Partial, ToRename: TFileName; PartialID: integer);
var
  local: TFileName;
  localsize, sourcesize, tot, start, stop: Int64;
  localok, istemp: boolean;

  procedure HandleCleanup; // sub-function for FPC Win64-aarch64 compilation
  begin
    fPartials.Safe.WriteLock; // safely move file without background access
    try
      if ToRename <> '' then
      begin
        // optional rename *.partial to final WGet() file name
        if RenameFile(Partial, ToRename) then
        begin
          Params.SetStep(wgsAlternateRename, [ToRename]);
          if not localok then
            // if we can't use the cached local file, switch to renamed file
            // - it will work as long as it can (usually just fine)
            if fPartials.DoneLocked(Partial, ToRename) then
              PartialID := 0;
        end
        else
          Params.SetStep(wgsAlternateFailedRename,
            [Partial, ' as ', ToRename, ' ', OsErrorShort]);
      end;
      if localok then
        fPartials.DoneLocked(Partial, local)
      else if (PartialID <> 0) and
              (sourcesize <> 0) then
          fPartials.DoneLocked(PartialID);
    finally
      fPartials.Safe.WriteUnLock;
    end;
  end;

begin
  // avoid GPF at shutdown or in case of unexpected method call
  if fShutdown or
     (pcoNoServer in fSettings.Options) then
    exit;
  sourcesize := FileSize(Partial);
  localok := false; // for proper THttpPartials cleanup on abort
  try
    // the supplied downloaded source file should be big enough
    if (sourcesize = 0) or // paranoid
       TooSmallFile(Params, sourcesize, 'OnDownloaded') then
      exit;
    // compute the local cache file name from the known file hash
    if not CachedFileName(Params, [lfnEnsureDirectoryExists], local, istemp) then
    begin
      fLog.Add.Log(sllWarning,
        'OnDownloaded: no hash specified for %', [Partial], self);
      exit;
    end;
    // check if this file was not already in the cache folder
    // - outside fFilesSafe.Lock because happens just after OnDownload from cache
    localsize := FileSize(local);
    if localsize <> 0 then
    begin
      fLog.Add.Log(LOG_TRACEWARNING[localsize <> sourcesize],
        'OnDownloaded: % already in cache', [Partial], self);
      // size mismatch may happen on race condition (hash collision is unlikely)
      if localsize = sourcesize then
      begin
        Params.SetStep(wgsAlternateAlreadyInCache, [local]);
        localok := true;
      end
      else
        Params.SetStep(wgsAlternateWrongSizeInCache,
          [local, ' ', localsize, '<>', sourcesize]); // paranoid
      exit;
    end;
    QueryPerformanceMicroSeconds(start);
    fFilesSafe.Lock; // disable any concurrent file access
    try
      // ensure adding this file won't trigger the maximum cache size limit
      if (fTempFilesMaxSize > 0) and
         istemp then
      begin
        tot := TempFolderEstimateNewSize(sourcesize);
        if tot >= fTempFilesMaxSize then
        begin
          fLog.Add.Log(sllDebug, 'OnDownloaded: % is too big (%) for tot=%',
            [Partial, KBNoSpace(sourcesize), KBNoSpace(tot)], self);
          exit;
        end;
      end;
      // actually copy the source file into the local cache folder
      localok := CopyFile(Partial, local, {failsifexists=}false);
      if localok then
      begin
        Params.SetStep(wgsAlternateCopiedInCache, [local]);
        if istemp then
        begin
          // force timestamp = now within the temporary folder
          FileSetDateFromUnixUtc(local, UnixTimeUtc);
          inc(fTempCurrentSize, localsize);
        end;
      end
      else
        Params.SetStep(wgsAlternateFailedCopyInCache,
          [Partial, ' into ', local, ' ', OsErrorShort]);
    finally
      fFilesSafe.UnLock;
    end;
    QueryPerformanceMicroSeconds(stop);
    fLog.Add.Log(LOG_TRACEWARNING[not localok],
      'OnDownloaded: % copied % into % in %', [KBNoSpace(sourcesize),
      Partial, local, MicroSecToString(stop - start)], self);
  finally
    HandleCleanup;
    if (PartialID <> 0) and
       (sourcesize = 0) then
      OnDownloadingFailed(PartialID); // clearly something went wrong
  end;
end;

procedure THttpPeerCache.OnDownloadFailed(const Params: THttpClientSocketWGet);
var
  local: TFileName;
  istemp: boolean;
begin
  // compute the local cache file name from the known file hash
  if not CachedFileName(Params, [lfnEnsureDirectoryExists], local, istemp) then
    fLog.Add.Log(sllWarning, 'OnDowloadFailed: missing hash', self)
  // actually delete the local (may be corrupted) file
  else if DeleteFile(local) then
    fLog.Add.Log(sllTrace, 'OnDowloadFailed: deleted %', [local], self)
  else
    fLog.Add.Log(sllLastError, 'OnDowloadFailed: error deleting %', [local], self);
end;

function THttpPeerCache.OnDownloading(const Params: THttpClientSocketWGet;
  const Partial: TFileName; ExpectedFullSize: Int64): THttpPartialID;
var
  h: THashDigest;
begin
  if (fPartials = nil) or // not supported by this fHttpServer class
     (waoNoProgressiveDownloading in Params.AlternateOptions) or
     not WGetToHash(Params, h) or
     TooSmallFile(Params, ExpectedFullSize, 'OnDownloading') then
    result := 0
  else
    result := fPartials.Add(Partial, ExpectedFullSize, @h, {http=}nil);
end;

function THttpPeerCache.PartialFileName(const aMessage: THttpPeerCacheMessage;
  aHttp: PHttpRequestContext; aFileName: PFileName; aSize: PInt64): integer;
var
  fn: TFileName;
  size: Int64;
  id: THttpPartialID;
begin
  result := HTTP_NOTFOUND;
  if fPartials.IsVoid then // not supported or not used yet
    exit;
  fn := fPartials.Find(aMessage.Hash, size, @id);
  if fVerboseLog then
    if fn = '' then
      fLog.Add.Log(sllTrace, 'PartialFileName: none', self)
    else
      fLog.Add.Log(sllTrace, 'PartialFileName: % id=% size=% msg: size=% start=% end=%',
        [fn, id, size, aMessage.Size, aMessage.RangeStart, aMessage.RangeEnd], self);
  if size = 0 then
    exit; // not existing
  result := HTTP_NOTACCEPTABLE; // 406
  if (aMessage.Size <> 0) and // ExpectedSize may be 0 if waoNoHeadFirst was set
     (size <> aMessage.Size) then
    exit; // invalid file
  if (aHttp <> nil) and // register the partial for this HTTP request
     not fPartials.Associate(aMessage.Hash, aHttp) then
  begin
    fLog.Add.Log(sllWarning, 'PartialFileName: % race condition', [fn], self);
    exit; // race condition with Find() - paranoid
  end;
  result := HTTP_SUCCESS;
  if aFileName <> nil then
    aFileName^ := fn;
  if aSize <> nil then
    aSize^ := size;
end;

procedure THttpPeerCache.OnDownloadingFailed(ID: THttpPartialID);
begin
  // unregister and abort any partial downloading process
  if fPartials.Abort(ID) <> 0 then
    SleepHiRes(100); // wait for THttpServer.Process abort
end;

type
  THttpClientSocketPeerCache = class(THttpClientSocket)
  public
    // some additional internal parameters and methods for proper threading
    DestFileName: TFileName;
    DestStream: TStreamRedirect;
    RemoteUri: RawUtf8;
    RemoteHeaders: RawUtf8;
    ExpectedHash: RawUtf8;
    PartialID: THttpPartialID;
    function ExpectedHashOrRaiseEHttpPeerCache: Int64;
    procedure AbortDownload(Sender: THttpPeerCache; E: Exception);
    destructor Destroy; override;
  end;

function THttpClientSocketPeerCache.ExpectedHashOrRaiseEHttpPeerCache: Int64;
var
  done: RawUtf8;
begin
  done := DestStream.GetHash;
  if not PropNameEquals(done, ExpectedHash) then
    EHttpPeerCache.RaiseUtf8('GET % hash % failed: %<>%',
      [RemoteUri, DestStream, done, ExpectedHash]);
  result := DestStream.Size;
end;

procedure THttpClientSocketPeerCache.AbortDownload(Sender: THttpPeerCache; E: Exception);
begin
  Sender.fLog.Add.Log(sllDebug, 'DirectFileName(%) raised %',
    [DestFileName, E], Sender);
  Sender.OnDownloadingFailed(PartialID); // abort progressive HTTP requests
  Sender.fFilesSafe.Lock; // disable any concurrent file access
  try
    FreeAndNil(DestStream);   // close incorrect stream before deleting file
    DeleteFile(DestFileName); // remove any incompleted/invalid file
  finally
    Sender.fFilesSafe.UnLock;
  end;
end;

destructor THttpClientSocketPeerCache.Destroy;
begin
  FreeAndNil(DestStream);
  inherited Destroy;
end;

function DirectConnectAndHead(Sender: THttpPeerCache;
  Ctxt: THttpServerRequestAbstract; redirmax: integer; const aParams: RawUtf8;
  out cs: THttpClientSocketPeerCache): integer;
var
  cslog: TSynLogProc;
  uri: TUri;
  hdr: RawUtf8;
  opt: THttpRequestExtendedOptions;
begin
  cs := nil;
  // compute the remote URI and its associated options/header
  result := HTTP_BADREQUEST;
  if not Sender.HttpDirectUriReconstruct(pointer(Ctxt.Url), uri) then
    exit;
  if aParams = '' then
    opt.Init
  else if not opt.InitFromUrl(aParams, Sender.fDirectSecret) then
    exit; // e.g. 'ti=1&as=3'
  opt.CreateTimeoutMS := 1000;
  opt.RedirectMax := redirmax;
  if Assigned(Sender.fOnDirectOptions) then
  begin
    result := Sender.fOnDirectOptions(uri, hdr, opt); // customizable
    if result <> HTTP_SUCCESS then
      exit;
  end;
  // HEAD to the original server to connect, retrieving size and redirection
  cslog := nil;
  if Sender.fVerboseLog then
    cslog := Sender.fLog.DoLog;
  cs := THttpClientSocketPeerCache.OpenOptions(uri, opt, cslog);
  if redirmax = 0 then // from DirectFileNameHead()
    include(cs.Http.Options, hroHeadersUnfiltered);
  result := cs.Head(uri.Address, 30000, hdr);
  Ctxt.OutCustomHeaders := cs.Headers; // include e.g. Content-Type: or Location:
  if not (result in HTTP_GET_OK) then
  begin
    FreeAndNil(cs);
    exit;
  end;
  if cs.Redirected <> '' then
    uri.Address := cs.Redirected; // follow 3xx redirection
  cs.RemoteUri := uri.Address;
  cs.RemoteHeaders := hdr;
end;

function THttpPeerCache.DirectFileName(Ctxt: THttpServerRequestAbstract;
  const aMessage: THttpPeerCacheMessage; aHttp: PHttpRequestContext;
  out aFileName: TFileName; out aSize: Int64; const aParams: RawUtf8): integer;
var
  cs: THttpClientSocketPeerCache;
  err, ip: RawUtf8;
  i: PtrInt;
  localsize: Int64;
  alone: boolean;
  req: THttpPeerCacheMessage;
  peers: THttpPeerCacheMessageDynArray;
begin
  cs := nil;
  result := HTTP_BADREQUEST;
  try
    if aMessage.Kind >= pcfBearerDirect then
    try
      // HEAD to the original server to connect and retrieve size + redirection
      err := 'head';
      result := DirectConnectAndHead(self, Ctxt, {redir=}3, aParams, cs);
      if not (result in HTTP_GET_OK) then
        exit;
      result := HTTP_LENGTHREQUIRED;
      aSize := cs.Http.ContentLength;
      Int32ToUtf8(aSize, err);
      if (aSize <= 0) or  // progressive request requires a final size
         fShutdown then // shutdown in progress
        exit;
      // try from cached files
      err := 'from partial';
      if fPartials <> nil then // check partial first (local may not be finished)
        result := PartialFileName(aMessage, aHttp, @aFileName, @localsize);
      if result <> HTTP_SUCCESS then
      begin
        err := 'from local';
        result := LocalFileName(aMessage, [lfnSetDate], @aFileName, @localsize);
      end;
      if result = HTTP_SUCCESS then
        if localsize <> aSize then // paranoid
        begin
          fLog.Add.Log(sllWarning, 'DirectFileName delete % size: %=% remote=%',
            [aFileName, err, localsize, aSize], self);
          fFilesSafe.Lock;
          try
            DeleteFile(aFileName); // this file seems invalid: clean from cache
            fTempCurrentSize := -1;
          finally
            fFilesSafe.UnLock;
          end;
        end
        else
        begin
          if err = 'from local' then
            aSize := 0; // not progressive
          exit;
        end;
      // prepare direct download into the final cache file as in LocalFileName()
      aFileName := ComputeFileName(aMessage.Hash);
      fFilesSafe.Lock; // disable any concurrent file access
      try
        case aMessage.Kind of
          pcfBearerDirect:
            begin
              result := HTTP_PAYLOADTOOLARGE;
              err := 'temp';
              if TempFolderEstimateNewSize(aSize) >= fTempFilesMaxSize then
                exit;
              aFileName := fTempFilesPath + aFileName;
            end;
          pcfBearerDirectPermanent:
            aFileName := PermFileName(aFileName, [lfnEnsureDirectoryExists]);
        end;
        // create the proper TStreamRedirect to store and hash
        err := 'file';
        result := HTTP_CONFLICT;
        if (FileSize(aFileName) > 0) or
           fShutdown then
          exit; // paranoid: existing cached files should have been checked
        cs.DestStream := HASH_STREAMREDIRECT[aMessage.Hash.Algo].Create(
          TFileStreamEx.Create(aFileName, fmCreate or fmShareRead));
        // mimics THttpPeerCache.OnDownloading() for progressive mode
        cs.DestFileName := aFileName;
        cs.PartialID := fPartials.Add(aFileName, aSize, @aMessage.Hash, aHttp);
      finally
        fFilesSafe.UnLock;
      end;
      // ask the local peers for this resource (enabled by default above 64KB)
      cs.ExpectedHash := ToText(aMessage.Hash);
      if (aSize > fSettings.BroadCastDirectMinBytes) and
         not (pcoHttpDirectNoBroadcast in fSettings.Options) then
      begin
        MessageInit(pcfRequest, 0, req);
        req.Hash := aMessage.Hash;
        req.Size := aSize;
        // try first the current/last HTTP peer client (disabled by default)
        if (pcoHttpDirectTryLastPeer in fSettings.Options) and
           (fClient <> nil) and
           (fClientIP4 <> 0) and
           fClient.SockConnected and
           fClientSafe.TryLock then
        try
          SetLength(peers, 1);
          peers[0] := req;
          FillZero(peers[0].Uuid); // "fake" HEAD request to check this peer
          result := LocalPeerRequest(req, peers[0], Ctxt.Url, nil, {Ban=}false);
          if fShutdown then
            exit;
          if result <> HTTP_SUCCESS then // returns HTTP_NOCONTENT if not found
            peers := nil; // make full GET in background thread
        finally
          fClientSafe.UnLock;
        end;
        // make an actual UDP broadcast about this file hash
        if peers = nil then
          peers := fUdpServer.Broadcast(req, alone);
        if peers <> nil then
          for i := 0 to high(peers) do
            if peers[i].IP4 <> fIP4 then
            begin
              // switch to this best local peer instead of main server
              IP4Text(@peers[i].IP4, ip);
              fLog.Add.Log(sllDebug, 'DirectFileName(%) switch to %:% peer',
                [Ctxt.Url, ip, fPort], self);
              cs.Close;
              LocalPeerClientSetup(ip, cs, 5000); // local cs, not fClient
              // local peer requires a new bearer
              peers[i].Kind := pcfBearer;
              MessageEncodeBearer(peers[i], cs.RemoteHeaders);
              FormatUtf8('?%', [Ctxt.Url], cs.RemoteUri); // <> DIRECTURI_32
              // make a quick HEAD on this new socket to verify the peer
              result := cs.Request(cs.RemoteUri, 'HEAD', 30000, cs.RemoteHeaders);
              if fShutdown then
                exit;
              if result in HTTP_GET_OK then
                break; // this peer seems just fine: background GET
            end;
      end;
      // start the GET request to the remote URI into aFileName via a sub-thread
      Make(['direct-', cs.PartialID], err);
      TLoggedWorkThread.Create(fLog, err, cs, DirectFileNameBackgroundGet);
      cs := nil; // will be owned by TLoggedWorkThread from now on
      result := HTTP_SUCCESS;
      // caller OnRequest() will return aFileName in progressive mode
    except
      on E: Exception do
      begin
        ClassToText(PClass(E)^, err);
        if (cs <> nil) and
           (cs.DestStream <> nil) then
          cs.AbortDownload(self, E)
        else
          fLog.Add.Log(sllDebug, 'DirectFileName(% %)=%',
            [Ctxt.Method, Ctxt.Url, PClass(E)^], self);
        result := HTTP_NOTFOUND;
      end;
    end;
  finally
    if (err <> '') and
       fVerboseLog then
      fLog.Add.Log(sllTrace, 'DirectFileName=% % (%)', [result, aFileName, err], self);
    cs.Free;
  end;
end;

procedure THttpPeerCache.DirectFileNameBackgroundGet(Sender: TObject);
var
  cs: THttpClientSocketPeerCache absolute Sender;
  status: integer;
  endsize: Int64;
begin
  // remote HTTP/HTTPS GET request executed in its own TLoggedWorkThread thread
  try
    try
      // make the actual blocking GET request in this background thread
      status := cs.Request(cs.RemoteUri, 'GET', 30000, cs.RemoteHeaders, '', '',
        {AsRetry=}false, {instream=}nil, {outstream=}cs.DestStream);
      if fShutdown then
        exit;
      if not (status in HTTP_GET_OK) then
        EHttpPeerCache.RaiseUtf8('GET % failed as %', [cs.RemoteUri, status]);
      endsize := cs.ExpectedHashOrRaiseEHttpPeerCache;
      fLog.Add.Log(sllTrace, 'DirectFileNameBackgroundGet(%)=% size=%',
        [cs.DestFileName, status, endsize], self);
    except
      on E: Exception do
        cs.AbortDownload(self, E);
    end;
  finally
    cs.Free; // overriden Destroy will make cs.DestStream.Free
  end;
end;

function THttpPeerCache.DirectFileNameHead(Ctxt: THttpServerRequestAbstract;
  const aHash: THashDigest; const aParams: RawUtf8): cardinal;
var
  cs: THttpClientSocketPeerCache;
begin
  try
    cs := nil;
    try
      // direct HEAD to the remote server, with no redirection
      result := DirectConnectAndHead(self, Ctxt, {redir=}0, aParams, cs);
      if pcoHttpReprDigest in fSettings.Options then
        (Ctxt as THttpServerRequest).SetOutReprDigest(aHash);
    finally
      cs.Free;
    end;
  except
    on E: Exception do
    begin
      fLog.Add.Log(sllDebug, 'DirectFileNameHead(%)=%', [Ctxt.Url, PClass(E)^], self);
      result := HTTP_BADGATEWAY;
    end;
  end;
end;

type
  TOnRequestError = (
    oreOK,
    oreNoLocalFile,
    oreLocalFileNotAcceptable,
    oreDirectRemoteUriFailed,
    oreShutdown);

function THttpPeerCache.OnRequest(Ctxt: THttpServerRequestAbstract): cardinal;
var
  msg: THttpPeerCacheMessage;
  fn: TFileName;
  progsize: Int64; // expected progressive file size, to be supplied as header
  err: TOnRequestError;
  errtxt, opt: RawUtf8;
begin
  // avoid GPF at shutdown
  result := HTTP_BADREQUEST;
  if fShutdown then
    exit;
  // retrieve context - already checked by OnBeforeBody
  err := oreOK;
  if Check(BearerDecode(Ctxt.AuthBearer, pcfRequest, msg, @opt), 'OnRequest', msg) then
  try
    // resource will always be identified by decoded bearer hash
    progsize := 0;
    case msg.Kind of
      pcfBearerDirect,
      pcfBearerDirectPermanent:
        if IsHead(Ctxt.Method) then
        begin
          // proxy the HEAD request to the final server
          result := DirectFileNameHead(Ctxt, msg.Hash, opt);
          if not (result in HTTP_GET_OK) then
            err := oreDirectRemoteUriFailed;
          exit;
        end
        else
        begin
          // remote HEAD + GET new partial file in a background thread
          result := DirectFileName(Ctxt, msg, Ctxt.ConnectionHttp, fn, progsize, opt);
          if result <> HTTP_SUCCESS then
          begin
            err := oreDirectRemoteUriFailed;
            exit;
          end;
        end;
    else // pcfBearer
      begin
        // try first from PartialFileName() then LocalFileName()
        if fPartials <> nil then // check partial first (local may not be finished)
          result := PartialFileName(msg, Ctxt.ConnectionHttp, @fn, @progsize);
        if result <> HTTP_SUCCESS then
          result := LocalFileName(msg, [lfnSetDate], @fn, nil);
      end;
    end;
    if fShutdown then
    begin
      err := oreShutdown;
      result := HTTP_BADGATEWAY; // 502
    end
    else if result <> HTTP_SUCCESS then
    begin
      // no local/partial known file: nothing to return
      if result = HTTP_NOTACCEPTABLE then // 406
        err := oreLocalFileNotAcceptable
      else
        err := oreNoLocalFile;
      if IsZero(THash128(msg.Uuid)) then // from "fake" response bearer
        result := HTTP_NOCONTENT; // OnDownload should make a broadcast
    end
    else
    begin
      // HTTP_SUCCESS: return the (partial) file as requested
      Ctxt.OutContent := StringToUtf8(fn);
      Ctxt.OutContentType := STATICFILE_CONTENT_TYPE;
      if progsize <> 0 then // append header for rfProgressiveStatic mode
        Ctxt.SetOutCustomHeader([STATICFILE_PROGSIZE + ' ', progsize]);
      if pcoHttpReprDigest in fSettings.Options then
        THttpServerRequest(Ctxt).SetOutReprDigest(msg.Hash);
    end;
  finally
    if err <> oreOK then
      errtxt := GetEnumNameUnCamelCase(TypeInfo(TOnRequestError), ord(err));
    if not StatusCodeIsSuccess(result) then
      Ctxt.OutContent := Join([StatusCodeToErrorMsg(result), ' - ', errtxt]);
    fLog.Add.Log(sllDebug, 'OnRequest=% % % % fn=% progsiz=% progid=% %',
      [result, Ctxt.Method, Ctxt.RemoteIP, Ctxt.Url, fn, progsize,
       Ctxt.ConnectionHttp^.ProgressiveID, errtxt], self);
  end;
end;


function ToText(pcf: THttpPeerCacheMessageKind): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpPeerCacheMessageKind), ord(pcf));
end;

function ToText(md: THttpPeerCryptMessageDecode): PShortString;
begin
  result := GetEnumName(TypeInfo(THttpPeerCryptMessageDecode), ord(md));
end;

function ToText(const msg: THttpPeerCacheMessage): ShortString;
begin
  MsgToShort(msg, result);
end;

procedure MsgToShort(const msg: THttpPeerCacheMessage; var result: ShortString);
begin
  result[0] := #0;
  if msg.Kind > high(msg.Kind) then
    exit; // clearly invalid message
  result := ToText(msg.Kind)^;
  AppendShortTwoChars(ord(' ') + ord('#') shl 8, @result);
  AppendShortIntHex(msg.Seq, result);
  AppendShortChar(' ', @result);
  AppendShortChar(OS_INITIAL[msg.Os.os], @result);
  AppendShortChar(' ', @result);
  AppendShort(OsvToShort(msg.Os)^, result);
  AppendOsBuild(msg.Os, @result, ' ');
  AppendShortChar(' ', @result);
  AppendShortChar(MAK_TXT[msg.Hardware], @result);
  AppendShortChar(' ', @result);
  AppendShortIp4(@msg.IP4, @result, ' ');
  AppendShort('to ', result);
  AppendShortIp4(@msg.DestIP4, @result, ' ');
  AppendShortIp4(@msg.MaskIP4, @result, ' ');
  AppendShortIp4(@msg.BroadcastIP4, @result, ' ');
  AppendShortCardinal(msg.Speed, result);
  AppendShort('Mb/s ', result);
  AppendShort(UnixTimeToFileShort(QWord(msg.Timestamp) + UNIXTIME_MINIMAL), result);
  if (msg.Hash.Algo <= high(msg.Hash.Algo)) and
     not IsZero(@msg.Hash.Bin.b, HASH_SIZE[msg.Hash.Algo]) then
  begin
    AppendShortChar(' ', @result); // append e.g. ' xxxHexaHashxxx.sha256'
    AppendShortHex(@msg.Hash.Bin, HASH_SIZE[msg.Hash.Algo], result);
    AppendShortAnsi7String(HASH_EXT[msg.Hash.Algo], result);
  end; // IsZero(Hash.Bin) = no hash known = no hash computed nor verified
  AppendShort(' siz=', result);
  AppendShortCardinal(msg.Size, result);
  AppendShort(' con=', result);
  AppendShortCardinal(msg.Connections, result);
  AppendShortCharSafe(' ', result);
  AppendShortUuid(msg.Uuid, result);
end;


initialization
  assert(SizeOf(THttpPeerCacheMessage) = 192);

end.
