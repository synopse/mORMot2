/// WebSockets Shared Process Classes and Definitions
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.ws.core;

{
  *****************************************************************************

   WebSockets Abstract Processing for Client and Server
   - WebSockets Frames Definitions
   - WebSockets Protocols Implementation
   - WebSockets Asynchronous Frames Parsing
   - WebSockets Client and Server Shared Process
   - TWebSocketProtocolChat Simple Protocol
   - Socket.IO / Engine.IO Raw Protocols

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.variants,
  mormot.core.datetime,
  mormot.core.data,
  mormot.core.log,
  mormot.core.threads,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.buffers,
  mormot.crypt.core,
  mormot.crypt.ecc,
  mormot.crypt.jwt,
  mormot.crypt.secure, // IProtocol definition
  mormot.net.sock,
  mormot.net.http;


{ ******************** WebSockets Frames Definitions }

type
  /// Exception raised when processing WebSockets
  EWebSockets = class(ESynException);

  /// defines the interpretation of the WebSockets frame data
  // - match order expected by the WebSockets RFC
  TWebSocketFrameOpCode = (
    focContinuation,
    focText,
    focBinary,
    focReserved3,
    focReserved4,
    focReserved5,
    focReserved6,
    focReserved7,
    focConnectionClose,
    focPing,
    focPong,
    focReservedB,
    focReservedC,
    focReservedD,
    focReservedE,
    focReservedF);

  /// set of WebSockets frame interpretation
  TWebSocketFrameOpCodes = set of TWebSocketFrameOpCode;

  /// define one attribute of a WebSockets frame data
  TWebSocketFramePayload = (
    fopAlreadyCompressed);
  /// define the attributes of a WebSockets frame data

  TWebSocketFramePayloads = set of TWebSocketFramePayload;

  /// stores a WebSockets frame
  // - see @http://tools.ietf.org/html/rfc6455 for reference
  TWebSocketFrame = record
    /// the content of the frame data, typically focText or focBinary
    opcode: TWebSocketFrameOpCode;
    /// what is stored in the frame data, i.e. in payload field
    content: TWebSocketFramePayloads;
    /// equals GetTickSec, as used for TWebSocketFrameList timeout
    tix: cardinal;
    /// the frame data itself
    // - is plain UTF-8 for focText kind of frame
    // - is raw binary for focBinary or any other frames
    payload: RawByteString;
  end;

  /// points to a WebSockets frame
  PWebSocketFrame = ^TWebSocketFrame;

  /// a dynamic list of WebSockets frames
  TWebSocketFrameDynArray = array of TWebSocketFrame;


const
  FRAME_OPCODE_FIN = 128;
  // https://tools.ietf.org/html/rfc6455#section-10.3
  // client-to-server masking is mandatory (but not from server to client)
  FRAME_LEN_MASK = 128;
  FRAME_LEN_2BYTES = 126;
  FRAME_LEN_8BYTES = 127;


/// used to return the text corresponding to a specified WebSockets frame type
function ToText(opcode: TWebSocketFrameOpCode): PShortString; overload;


/// low-level intitialization of a TWebSocketFrame for proper REST content
procedure FrameInit(opcode: TWebSocketFrameOpCode;
  const Content, ContentType: RawByteString; out frame: TWebSocketFrame);

/// low-level encoding of an output frame
// - don't make any fProtocol.BeforeSendFrame virtual encoding (like
// binary compression or encryption), but prepare the outgoing frame, ready
// to be sent over the socket
// - encoded buffer is a TSynTempBuffer to avoid most memory allocations
// - caller should always finally perform an eventual ToSend.Done
procedure FrameSendEncode(const Frame: TWebSocketFrame;
  MaskSentFrames: cardinal; var ToSend: TSynTempBuffer);

/// compute the SHA-1 signature of the given WebSockets upgrade challenge
procedure ComputeChallenge(const Base64: RawByteString; out Digest: TSha1Digest);



{ ******************** WebSockets Protocols Implementation }

type
  {$M+}
  TWebSocketProcess = class;
  {$M-}

  /// used by TWebSocketProcessSettings for WebSockets process logging settings
  TWebSocketProcessSettingsLogDetails = set of (
    logHeartbeat,
    logTextFrameContent,
    logBinaryFrameContent,
    logCallback);

  /// points to parameters to be used for WebSockets process
  // - using a pointer/reference type will allow in-place modification of
  // any TWebSocketProcess.Settings, TWebSocketServer.Settings or
  // THttpClientWebSockets.Settings property
  PWebSocketProcessSettings = ^TWebSocketProcessSettings;

  /// parameters to be used for WebSockets processing
  // - those settings are used by all protocols running on a given
  // TWebSocketServer or a THttpClientWebSockets
  {$ifdef USERECORDWITHMETHODS}
  TWebSocketProcessSettings = record
  {$else}
  TWebSocketProcessSettings = object
  {$endif USERECORDWITHMETHODS}
  public
    /// time in milliseconds between each focPing commands sent to the other end
    // - default is 0, i.e. no automatic ping sending on client side, and
    // 20000, i.e. 20 seconds, on server side
    HeartbeatDelay: cardinal;
    /// maximum period time in milliseconds when ProcessLoop thread will stay
    // idle before checking for the next pending requests
    // - default is 500 ms, but you may put a lower value, if you expects e.g.
    // REST commands or NotifyCallback(wscNonBlockWithoutAnswer) to be processed
    // with a lower delay
    LoopDelay: cardinal;
    /// milliseconds delay between sending pending frames
    // - allow to gather output frames in ProcessLoopStepSend
    // - GetTickCount64 resolution is around 16ms on Windows and 4ms on Linux,
    // so default 10 (ms) value seems fine for a cross-platform similar behavior
    // (resulting in a <16ms period on Windows, and <12ms period on Linux)
    SendDelay: cardinal;
    /// will close the connection after a given number of invalid Heartbeat sent
    // - when a Hearbeat is failed to be transmitted, the class will start
    // counting how many ping/pong did fail: when this property value is
    // reached, it will release and close the connection
    // - client could then try to reestablish the weak connection, e.g. if a
    // mobile connection reconnects after a white zone and may change its IP
    // - default value is 5
    DisconnectAfterInvalidHeartbeatCount: cardinal;
    /// how many milliseconds the callback notification should wait acquiring
    // the connection before failing
    // - defaut is 5000, i.e. 5 seconds
    CallbackAcquireTimeOutMS: cardinal;
    /// how many milliseconds the callback notification should wait for the
    // client to return its answer
    // - defaut is 30000, i.e. 30 seconds
    CallbackAnswerTimeOutMS: cardinal;
    /// callback run when a WebSockets client is just connected
    // - triggerred by TWebSocketProcess.ProcessStart
    OnClientConnected: TNotifyEvent;
    /// callback run when a WebSockets client is just disconnected
    // - triggerred by TWebSocketProcess.ProcessStop
    OnClientDisconnected: TNotifyEvent;
    /// if the WebSockets Client should be upgraded after socket reconnection
    // - default is TRUE
    ClientAutoUpgrade: boolean;
    /// notify the server to move any callbacks to the renewed connection
    // - default is FALSE
    ClientRestoreCallbacks: boolean;
    /// by default, contains [] to minimize the logged information
    // - set logHeartbeat if you want the ping/pong frames to be logged
    // - set logTextFrameContent if you want the text frame content to be logged
    // - set logBinaryFrameContent if you want the binary frame content to be logged
    // - set logCallback for each TWebSocketAsyncServerRest callback notification
    // - used only if WebSocketLog global variable is set to a TSynLog class
    LogDetails: TWebSocketProcessSettingsLogDetails;
    /// TWebSocketProtocol.SetEncryptKey PBKDF2-SHA-3 salt for TProtocolAes
    // - default is some fixed value - you may customize it for a project
    AesSalt: RawUtf8;
    /// TWebSocketProtocol.SetEncryptKey PBKDF2-SHA-3 rounds for TProtocolAes
    // - default is 1024 which takes around 0.5 ms to compute
    // - 0 would use Sha256Weak() derivation function, as mORMot 1.18
    AesRounds: integer;
    /// TWebSocketProtocol.SetEncryptKey AES class for TProtocolAes
    // - default is TAesFast[mCtr]
    AesCipher: TAesAbstractClass;
    /// TWebSocketProtocol.SetEncryptKey AES key size in bits, for TProtocolAes
    // - default is 128 for efficient 'aes-128-ctr' at 2.5GB/s
    // - for mORMot 1.18 compatibility, you could override default globals as such:
    // ! AesClass := TAesCfb;
    // ! AesBits := 256;
    // ! AesRounds := 0; // Sha256Weak() deprecated function
    AesBits: integer;
    /// TWebSocketProtocol.SetEncryptKey 'password#xxxxxx.private' ECDHE algo
    // - default is efAesCtr128 as set to TEcdheProtocol.FromPasswordSecureFile
    EcdheCipher: TEcdheEF;
    /// TWebSocketProtocol.SetEncryptKey 'password#xxxxxx.private' ECDHE auth
    // - default is the safest authMutual
    EcdheAuth: TEcdheAuth;
    /// TWebSocketProtocol.SetEncryptKey 'password#xxxxxx.private' password rounds
    // - default is 60000, i.e. DEFAULT_ECCROUNDS
    EcdheRounds: integer;
    /// will set the default values
    // - will also reset the HeartbeatDelay to 0, as expected on client side
    procedure SetDefaults;
    /// will set LogDetails to its highest level of verbosity
    // - used only if WebSocketLog global variable is set
    function SetFullLog: PWebSocketProcessSettings;
  end;

  /// callback event triggered by TWebSocketProtocol for any incoming message
  // - called before TWebSocketProtocol.ProcessIncomingFrame for incoming
  // focText/focBinary frames
  // - should return true if the frame has been handled, or false if the
  // regular processing should take place
  TOnWebSocketProtocolIncomingFrame = function(Sender: TWebSocketProcess;
    var Frame: TWebSocketFrame): boolean of object;

  /// one instance implementing application-level WebSockets protocol
  // - shared by TWebSocketServer and TWebSocketClient classes
  // - once upgraded to WebSockets, a HTTP link could be used e.g. to transmit our
  // proprietary 'synopsejson' or 'synopsebin' application content, as stated
  // by this typical handshake:
  // $ GET /myservice HTTP/1.1
  // $ Host: server.example.com
  // $ Upgrade: websocket
  // $ Connection: Upgrade
  // $ Sec-WebSocket-Key: x3JJHMbDL1EzLkh9GBhXDw==
  // $ Sec-WebSocket-Protocol: synopsejson
  // $ Sec-WebSocket-Version: 13
  // $ Origin: http://example.com
  // $
  // $ HTTP/1.1 101 Switching Protocols
  // $ Upgrade: websocket
  // $ Connection: Upgrade
  // $ Sec-WebSocket-Accept: HSmrc0sMlYUkAGmm5OPpG2HaGWk=
  // $ Sec-WebSocket-Protocol: synopsejson
  // - the TWebSocketProtocolJson inherited class will implement
  // $ Sec-WebSocket-Protocol: synopsejson
  // - the TWebSocketProtocolBinary inherited class will implement
  // $ Sec-WebSocket-Protocol: synopsebin
  TWebSocketProtocol = class(TSynPersistent)
  protected
    fConnectionID: THttpServerConnectionID;
    fFramesInCount: integer;
    fFramesOutCount: integer;
    fFramesInBytes: QWord;
    fFramesOutBytes: QWord;
    fOnBeforeIncomingFrame: TOnWebSocketProtocolIncomingFrame;
    fRemoteLocalhost: boolean;
    fConnectionFlags: THttpServerRequestFlags;
    fConnectionOpaque: PHttpServerConnectionOpaque;
    fRemoteIP: RawUtf8;
    fUpgradeUri: RawUtf8;
    fUpgradeBearerToken: RawUtf8;
    fName: RawUtf8;
    fUri: RawUtf8;
    fLastError: string;
    fEncryption: IProtocol;
    procedure AfterUpgrade(aProcess: TWebSocketProcess); virtual;
    // focText/focBinary or focContinuation/focConnectionClose from ProcessStart/Stop
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Request: TWebSocketFrame; const Info: RawUtf8); virtual; abstract;
    function SendFrames(Owner: TWebSocketProcess;
      var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean; virtual;
    procedure AfterGetFrame(var frame: TWebSocketFrame); virtual;
    procedure BeforeSendFrame(var frame: TWebSocketFrame); virtual;
    function FrameData(const frame: TWebSocketFrame; const Head: RawUtf8;
      HeadFound: PRawUtf8 = nil; PMax: PPByte = nil): pointer; virtual;
    function FrameType(const frame: TWebSocketFrame): TShort31; virtual;
    function GetRemoteIP: RawUtf8;
    function GetEncrypted: boolean;
      {$ifdef HASINLINE}inline;{$endif}
  public
    /// abstract constructor to initialize the protocol
    // - the protocol should be named, so that the client may be able to request
    // for a given protocol
    // - if aUri is '', any URI would potentially upgrade to this protocol; you can
    // specify an URI to limit the protocol upgrade to a single resource
    constructor Create(const aName, aUri: RawUtf8); reintroduce;
    /// compute a new instance of the WebSockets protocol, with same parameters
    // - by default, will return nil, as expected for Client-side only
    function Clone(const aClientUri: RawUtf8): TWebSocketProtocol; virtual;
    /// reuse an existing protocol instance on a new connection
    // - called e.g. by THttpClientWebSockets.WebSocketsUpgrade(aReconnect=true)
    procedure Reset; virtual;
    /// the sub-protocols supported by this client (not used on server side)
    // - as transmitted in the 'Sec-WebSocket-Protocol:' header during upgrade
    // - returns Name by default, but could be e.g. 'synopsebin, synopsebinary'
    // - some protocols have no sub-protocol, so would return '' here
    function GetSubprotocols: RawUtf8; virtual;
    /// recognize a supported sub-protocol (on both client and server sides)
    // - should return true on success, i.e. if aProtocolName has been recognized
    // - check against Name by default, but could be e.g. 'synopsebin, synopsebinary'
    function SetSubprotocol(const aProtocolName: RawUtf8): boolean; virtual;
    /// create the internal Encryption: IProtocol according to the supplied key
    // - any asymmetric algorithm needs to know its side, i.e. client or server
    // - use aKey='password#xxxxxx.private' for efAesCtr128 calling
    // TEcdheProtocol.FromPasswordSecureFile() - FromKeySetCA() should have been
    // called to set the global PKI
    // - use aKey='a=mutual;e=aesctc128;p=34a2;pw=password;ca=..' full
    // TEcdheProtocol.FromKey(aKey) format
    // - or aKey will be derivated using aSettings to call
    // SetEncryptKeyAes - default as 1024 PBKDF2-SHA-3 rounds into aes-128-ctr
    // - you can disable encryption by setting aKey=''
    procedure SetEncryptKey(aServer: boolean; const aKey: RawUtf8;
      aSettings: PWebSocketProcessSettings);
    /// set the fEncryption: IProtocol from TProtocolAes.Create()
    // - if aClass is nil, TAesFast[mCtr] will be used as default
    // - AEAD Cfc,mOfc,mCtc,mGcm modes will be rejected since unsupported
    procedure SetEncryptKeyAes(aCipher: TAesAbstractClass;
      const aKey; aKeySize: cardinal);
    /// set the fEncryption: IProtocol from TEcdheProtocol.Create()
    // - as default, we use efAesCtr128 which is the fastest on x86_64 (2.5GB/s)
    procedure SetEncryptKeyEcdhe(aAuth: TEcdheAuth; aPKI: TEccCertificateChain;
      aPrivate: TEccCertificateSecret; aServer: boolean;
      aEF: TEcdheEF = efAesCtr128; aPrivateOwned: boolean = false);
    /// redirect to Encryption.ProcessHandshake, if defined
    function ProcessHandshake(const ExtIn: TRawUtf8DynArray;
      out ExtOut: RawUtf8; ErrorMsg: PRawUtf8): boolean; virtual;
    /// called e.g. for authentication during the WebSockets handshake
    function ProcessHandshakeUri(const aClientUri: RawUtf8): boolean; virtual;
    /// allow low-level interception before ProcessIncomingFrame is done
    property OnBeforeIncomingFrame: TOnWebSocketProtocolIncomingFrame
      read fOnBeforeIncomingFrame write fOnBeforeIncomingFrame;
    /// access low-level frame encryption
    property Encryption: IProtocol
      read fEncryption;
    /// contains either [hsrSecured, hsrWebsockets] or [hsrWebsockets]
    property ConnectionFlags: THttpServerRequestFlags
      read fConnectionFlags;
    /// the associated low-level WebSocket connection numerical identifier
    property ConnectionID: THttpServerConnectionID
      read fConnectionID;
    /// associated low-level opaque pointer maintained during the connection
    property ConnectionOpaque: PHttpServerConnectionOpaque
      read fConnectionOpaque;
    /// quickly check if the known remote IP maps the local host
    property RemoteLocalhost: boolean
      read fRemoteLocalhost write fRemoteLocalhost;
  published
    /// the Sec-WebSocket-Protocol application name currently involved
    // - e.g. 'synopsejson', 'synopsebin' or 'synopsebinary'
    property Name: RawUtf8
      read fName write fName;
    /// the optional URI on which this protocol would be enabled
    // - leave to '' if any URI should match
    property URI: RawUtf8
      read fUri;
    /// the associated Remote IP as set by the raw socket layer
    // - returns '' if self=nil or RemoteLocalhost=true on localhost
    property RemoteIP: RawUtf8
      read GetRemoteIP write fRemoteIP;
    /// the URI on which this protocol has been upgraded
    property UpgradeUri: RawUtf8
      read fUpgradeUri write fUpgradeUri;
    /// the "Bearer" HTTP header value on which this protocol has been upgraded
    property UpgradeBearerToken: RawUtf8
      read fUpgradeBearerToken write fUpgradeBearerToken;
    /// the last error message, during frame processing
    property LastError: string
      read fLastError;
    /// returns TRUE if encryption is enabled during the transmission
    // - is currently only available for TWebSocketProtocolBinary
    property Encrypted: boolean
      read GetEncrypted;
    /// how many frames have been received by this instance
    property FramesInCount: integer
      read fFramesInCount;
    /// how many frames have been sent by this instance
    property FramesOutCount: integer
      read fFramesOutCount;
    /// how many (uncompressed) bytes have been received by this instance
    property FramesInBytes: QWord
      read fFramesInBytes;
    /// how many (uncompressed) bytes have been sent by this instance
    property FramesOutBytes: QWord
      read fFramesOutBytes;
  end;


  /// handle a REST application-level bi-directional WebSockets protocol
  // - will emulate a bi-directional REST process, using THttpServerRequest to
  // store and handle the request parameters: clients would be able to send
  // regular REST requests to the server, but the server could use the same
  // communication channel to push REST requests to the client
  // - a local THttpServerRequest will be used on both client and server sides,
  // to store REST parameters and compute the corresponding WebSockets frames
  TWebSocketProtocolRest = class(TWebSocketProtocol)
  protected
    fSequencing: boolean;
    fSequence: integer;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
       var Request: TWebSocketFrame; const Info: RawUtf8); override;
    procedure FrameCompress(const Head: RawUtf8; const Values: array of const;
      const Content, ContentType: RawByteString; var frame: TWebSocketFrame);
        virtual; abstract;
    function FrameDecompress(const frame: TWebSocketFrame;
      const Head: RawUtf8; const values: array of PRawUtf8;
      var contentType, content: RawUtf8): boolean; virtual; abstract;
    /// convert the input information of REST request to a WebSocket frame
    procedure InputToFrame(Ctxt: THttpServerRequestAbstract; aNoAnswer: boolean;
      var request: TWebSocketFrame; out head: RawUtf8); virtual;
    /// convert a WebSocket frame to the input information of a REST request
    function FrameToInput(var request: TWebSocketFrame; out aNoAnswer: boolean;
      Ctxt: THttpServerRequestAbstract): boolean; virtual;
    /// convert a WebSocket frame to the output information of a REST request
    function FrameToOutput(var answer: TWebSocketFrame;
      Ctxt: THttpServerRequestAbstract): cardinal; virtual;
    /// convert the output information of REST request to a WebSocket frame
    procedure OutputToFrame(Ctxt: THttpServerRequestAbstract; Status: cardinal;
      var outhead: RawUtf8; var answer: TWebSocketFrame); virtual;
  end;

  /// used to store the class of a TWebSocketProtocol type
  TWebSocketProtocolClass = class of TWebSocketProtocol;

  /// handle a REST application-level WebSockets protocol using JSON for transmission
  // - could be used e.g. for AJAX or non Delphi remote access
  // - this class will implement then following application-level protocol:
  // $ Sec-WebSocket-Protocol: synopsejson
  TWebSocketProtocolJson = class(TWebSocketProtocolRest)
  protected
    procedure FrameCompress(const Head: RawUtf8; const Values: array of const;
      const Content, ContentType: RawByteString; var frame: TWebSocketFrame); override;
    function FrameDecompress(const frame: TWebSocketFrame; const Head: RawUtf8;
      const values: array of PRawUtf8;
      var contentType, content: RawUtf8): boolean; override;
    function FrameData(const frame: TWebSocketFrame; const Head: RawUtf8;
      HeadFound: PRawUtf8 = nil; PMax: PPByte = nil): pointer; override;
    function FrameType(const frame: TWebSocketFrame): TShort31; override;
  public
    /// initialize the WebSockets JSON protocol
    // - if aUri is '', any URI would potentially upgrade to this protocol; you can
    // specify an URI to limit the protocol upgrade to a single resource
    constructor Create(const aUri: RawUtf8); reintroduce;
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone(const aClientUri: RawUtf8): TWebSocketProtocol; override;
    /// overriden method to compute and send all output as a single buffer
    function SendFrames(Owner: TWebSocketProcess;
      var Frames: TWebSocketFrameDynArray;
      var FramesCount: integer): boolean; override;
  end;


  /// handle a REST application-level WebSockets protocol using compressed and
  // optionally AES-CTR encrypted binary
  // - this class will implement then following application-level protocol:
  // $ Sec-WebSocket-Protocol: synopsebin
  // or fallback to the previous subprotocol
  // $ Sec-WebSocket-Protocol: synopsebinary
  // - 'synopsebin' will expect requests sequenced as 'r000001','r000002',...
  // headers matching 'a000001','a000002',... instead of 'request'/'answer'
  TWebSocketProtocolBinary = class(TWebSocketProtocolRest)
  protected
    fFramesInBytesSocket: QWord;
    fFramesOutBytesSocket: QWord;
    fOptions: TWebSocketProtocolBinaryOptions;
    procedure FrameCompress(const Head: RawUtf8;
      const Values: array of const; const Content, ContentType: RawByteString;
      var frame: TWebSocketFrame); override;
    function FrameDecompress(const frame: TWebSocketFrame;
      const Head: RawUtf8; const values: array of PRawUtf8;
      var contentType, content: RawUtf8): boolean; override;
    procedure AfterGetFrame(var frame: TWebSocketFrame); override;
    procedure BeforeSendFrame(var frame: TWebSocketFrame); override;
    function FrameData(const frame: TWebSocketFrame; const Head: RawUtf8;
      HeadFound: PRawUtf8 = nil; PMax: PPByte = nil): pointer; override;
    function FrameType(const frame: TWebSocketFrame): TShort31; override;
    function SendFrames(Owner: TWebSocketProcess;
      var Frames: TWebSocketFrameDynArray;
      var FramesCount: integer): boolean; override;
    procedure ProcessIncomingFrames(Sender: TWebSocketProcess; P, PMax: PByte);
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Request: TWebSocketFrame; const Info: RawUtf8); override;
    function GetFramesInCompression: integer;
    function GetFramesOutCompression: integer;
  public
    /// initialize the WebSockets binary protocol with no encryption
    // - if aUri is '', any URI would potentially upgrade to this protocol; you
    // can specify an URI to limit the protocol upgrade to a single resource
    // - SynLZ compression is enabled by default, for all frames
    constructor Create(const aUri: RawUtf8;
      aOptions: TWebSocketProtocolBinaryOptions = [pboSynLzCompress]);
      reintroduce; overload; virtual;
    /// initialize the WebSockets binary protocol with a symmetric AES key
    // - if aUri is '', any URI would potentially upgrade to this protocol; you
    // can specify an URI to limit the protocol upgrade to a single resource
    // - if aKeySize if 128, 192 or 256, TProtocolAes (i.e. AES-CTR encryption)
    //  will be used to secure the transmission
    // - SynLZ compression is enabled by default, before encryption
    constructor Create(const aUri: RawUtf8; const aKey; aKeySize: cardinal;
      aOptions: TWebSocketProtocolBinaryOptions = [pboSynLzCompress];
      aCipher: TAesAbstractClass = nil);
        reintroduce; overload;
    /// initialize the WebSockets binary protocol from a textual key
    // - if aUri is '', any URI would potentially upgrade to this protocol; you
    // can specify an URI to limit the protocol upgrade to a single resource
    // - will create a TProtocolAes or TEcdheProtocol instance, corresponding to
    // the supplied aKey and aServer values, to secure the transmission using
    // a symmetric or assymmetric algorithm
    // - SynLZ compression is enabled by default, unless aCompressed is false
    constructor Create(const aUri: RawUtf8; aServer: boolean;
      const aKey: RawUtf8; aSettings: PWebSocketProcessSettings;
      aOptions: TWebSocketProtocolBinaryOptions = [pboSynLzCompress]);
        reintroduce; overload;
    /// compute a new instance of the WebSockets protocol, with same parameters
    function Clone(const aClientUri: RawUtf8): TWebSocketProtocol; override;
    /// overriden to return 'synopsebin, synopsebinary' sub-protocols
    function GetSubprotocols: RawUtf8; override;
    /// recognize our 'synopsebin, synopsebinary' sub-protocols
    function SetSubprotocol(const aProtocolName: RawUtf8): boolean; override;
  published
    /// how compression / encryption is implemented during the transmission
    // - is set to [pboSynLzCompress] by default
    property Options: TWebSocketProtocolBinaryOptions
      read fOptions write fOptions;
    /// how many bytes have been received by this instance from the wire
    property FramesInBytesSocket: QWord
      read fFramesInBytesSocket;
    /// how many bytes have been sent by this instance to the wire
    property FramesOutBytesSocket: QWord
      read fFramesOutBytesSocket;
    /// compression ratio of frames received by this instance
    property FramesInCompression: integer
      read GetFramesInCompression;
    /// compression ratio of frames Sent by this instance
    property FramesOutCompression: integer
      read GetFramesOutCompression;
  end;

  /// event signature trigerred from TWebSocketProtocolList.ServerUpgrade()
  // - allow e.g. to verify a JWT bearer before returning the WS 101 response
  // - Protocol.UpgradeUri/UpgradeBearerToken/RemoteIP/RemoteLocalhost and
  // ConnectionID/ConnectionOpaque fields have already been populated
  // - should return HTTP_SUCCESS to continue, or an error code to abort
  TOnWebSocketProtocolUpgraded =
    function(Protocol: TWebSocketProtocol): integer of object;
  /// event signature trigerred on WS connection close
  TOnWebSocketProtocolClosed =
    procedure(Protocol: TWebSocketProtocol) of object;

  /// used to maintain a list of websocket protocols (for the server side)
  TWebSocketProtocolList = class(TObjectRWLightLock)
  protected
    fProtocols: array of TWebSocketProtocol;
    fOnUpgraded: TOnWebSocketProtocolUpgraded;
    // caller should make fSafe.ReadOnlyLock/WriteLock
    function LockedFindIndex(const aName, aUri: RawUtf8): PtrInt;
  public
    /// add a protocol to the internal list
    // - returns TRUE on success
    // - if this protocol is already existing for this given name and URI,
    // returns FALSE: it is up to the caller to release aProtocol if needed
    function Add(aProtocol: TWebSocketProtocol): boolean;
    /// add once a protocol to the internal list
    // - if this protocol is already existing for this given name and URI, any
    // previous one will be released - so it may be confusing on a running server
    // - returns TRUE if the protocol was added for the first time, or FALSE
    // if the protocol has been replaced or is invalid (e.g. aProtocol=nil)
    function AddOnce(aProtocol: TWebSocketProtocol): boolean;
    /// erase a protocol from the internal list, specified by its name
    function Remove(const aProtocolName, aUri: RawUtf8): boolean;
    /// finalize the list storage
    destructor Destroy; override;
    /// create a new protocol instance, from the internal list
    function CloneByName(
      const aProtocolName, aClientUri: RawUtf8): TWebSocketProtocol;
    /// create a new protocol instance, from the internal list
    function CloneByUri(const aClientUri: RawUtf8): TWebSocketProtocol;
    /// how many protocols are stored
    function Count: integer;
    /// server-side HTTP Upgrade to one supported WebSockets protocols
    // - if returns, HTTP_SUCCESS caller should send the Response headers
    // and use the Protocol - or free it and close the connection
    function ServerUpgrade(const Http: THttpRequestContext;
      const RemoteIp: RawUtf8; ConnectionID: THttpServerConnectionID;
      ConnectionOpaque: PHttpServerConnectionOpaque;
      out Protocol: TWebSocketProtocol; out Response: RawUtf8): integer;
    /// callback event run from ServerUpgrade
    property OnUpgraded: TOnWebSocketProtocolUpgraded
      read fOnUpgraded write fOnUpgraded;
  end;

  /// indicates which kind of process did occur in the main WebSockets loop
  TWebSocketProcessOne = (
    wspNone,
    wspPing,
    wspDone,
    wspAnswer,
    wspError,
    wspClosed);

  /// indicates how TWebSocketProcess.NotifyCallback() will work
  TWebSocketProcessNotifyCallback = (
    wscBlockWithAnswer,
    wscBlockWithoutAnswer,
    wscNonBlockWithoutAnswer);

  /// used to manage a thread-safe list of WebSockets frames
  // - TSynLocked because SendPendingOutgoingFrames() locks it and may take time
  TWebSocketFrameList = class(TObjectOSLock)
  protected
    fTimeoutSec: cardinal;
    fAnswerToIgnore: integer;
    procedure Delete(i: PtrInt);
  public
    /// low-level access to the WebSocket frames list
    List: TWebSocketFrameDynArray;
    /// current number of WebSocket frames in the list
    Count: integer;
    /// initialize the list
    constructor Create(timeoutsec: cardinal); reintroduce;
    /// add a WebSocket frame in the list
    // - this method is thread-safe
    procedure Push(const frame: TWebSocketFrame; currentSec: cardinal);
    /// add a void WebSocket frame in the list
    // - this method is thread-safe
    procedure PushVoidFrame(opcode: TWebSocketFrameOpCode; currentSec: cardinal);
    /// retrieve a WebSocket frame from the list, oldest first
    // - you should specify a frame type to search for, according to the
    // specified WebSockets protocl
    // - this method is thread-safe
    function Pop(protocol: TWebSocketProtocol; const head: RawUtf8;
      out frame: TWebSocketFrame; currentSec: cardinal): boolean;
    /// how many 'answer' frames are to be ignored
    // - incdec should be either 0, -1 or +1
    // - this method is thread-safe
    function AnswerToIgnore(incdec: integer = 0): integer;
  end;

  /// a WebSocket protocol able to generate ephemeral connection URI
  // - on JavaScript, it is not possible to set a HTTP header bearer to
  // authenticate: so this class generates and recognizes formatted URIs
  // - inherited classes should override the ProcessIncomingFrame() method
  TWebSocketProtocolUri = class(TWebSocketProtocol)
  protected
    fSession: TBinaryCookieGeneratorSessionID;
    fCreated: cardinal;
    fGenerator: TBinaryCookieGenerator;
    fRecordTypeInfo: PRttiInfo;
    fRecordData: pointer;
    fPublicUri: RawUtf8;
    fGeneratorOwned: boolean;
  public
    /// initialize the protocol for a given Jwt
    // - if aExpirationMinutes is set, will own a new URI generator
    // - aPublicUri is mandatory and will be used by NewUri, typically equals
    // '127.0.0.1:888' or 'publicdomain.com/websockgateway'
    // - each time this protocol is setup, a random seed is used for NewUri
    // - aRecordTypeInfo can optionally associate a record to each URI
    constructor Create(const aName, aPublicUri: RawUtf8;
      aExpirationMinutes: integer; aRecordTypeInfo: PRttiInfo);
      reintroduce; virtual;
    /// finalize the protocol definition
    destructor Destroy; override;
    /// validate the URI supplied during connection upgrade on server side
    function ProcessHandshakeUri(const aClientUri: RawUtf8): boolean; override;
    /// called when a new connection upgrade attempt is received on server side
    function Clone(const aClientUri: RawUtf8): TWebSocketProtocol; override;
    /// high-level code should call this method to generate a valid URI
    // - WebSockets connection on this URI will be upgraded by this protocol
    // - URI are obfuscated and signed with an ephemeral TBinaryCookieGenerator,
    // so will have an unique Session ID, and can be associated with a record
    // - this method is thread-safe
    function NewUri(out SessionID: TBinaryCookieGeneratorSessionID;
      PRecordData: pointer = nil): RawUtf8; virtual;
    /// access to the low-level ephemeral URI generator
    property Generator: TBinaryCookieGenerator
      read fGenerator;
    /// optional associated record, as recognized by ProcessHandshakeUri()
    // - is a pointer to a RecordTypeInfo record, owned by this instance
    property RecordData: pointer
      read fRecordData;
    /// access to the associated record RTTI definition
    property RecordTypeInfo: PRttiInfo
      read fRecordTypeInfo;
  published
    /// the public Root URI as used by NewUri
    // - e.g. '127.0.0.1:888' or 'publicdomain.com/wsgateway'
    property PublicUri: RawUtf8
      read fPublicUri;
    /// the session 32-bit identifier, as generated by NewUri and recognized
    // by ProcessHandshakeUri() during WebSockets upgrade
    property Session: TBinaryCookieGeneratorSessionID
      read fSession;
    /// server-side UnixTimeUtc value when the recognized URI was generated
    property Created: cardinal
      read fCreated;
  end;

  TWebSocketProtocolUriClass = class of TWebSocketProtocolUri;


{ ******************** WebSockets Client and Server Shared Process }

  /// the current state of the WebSockets process
  TWebSocketProcessState = (
    wpsCreate,
    wpsRun,
    wpsClose,
    wpsDestroy);

  /// abstract WebSockets process, used on both client or server sides
  // - CanGetFrame/ReceiveBytes/SendBytes abstract methods should be overriden with
  // actual communication, and fState and ProcessStart/ProcessStop should be
  // updated from the actual processing thread (e.g. as in TWebCrtSocketProcess)
  TWebSocketProcess = class(TSynPersistent)
  protected
    fProtocol: TWebSocketProtocol;
    fConnectionID: THttpServerConnectionID;
    fIncoming: TWebSocketFrameList;
    fOutgoing: TWebSocketFrameList;
    fOwnerThread: TSynThread;
    fState: TWebSocketProcessState;
    fMaskSentFrames: byte;
    fProcessEnded: boolean;
    fConnectionCloseWasSent: boolean;
    fNoLastSocketTicks: boolean;
    fProcessCount: integer;
    fInvalidPingSendCount: cardinal;
    fSettings: PWebSocketProcessSettings;
    fSafeIn, fSafeOut: TOSLock;
    fLastSocketTicks: Int64;
    fProcessName: RawUtf8;
    procedure MarkAsInvalid;
    function LastPingDelay: Int64;
    procedure SetLastPingTicks;
    procedure SendPing;
    /// callback methods run by ProcessLoop
    procedure ProcessStart; virtual;
    procedure ProcessStop; virtual;
    // called by ProcessLoop - TRUE=continue, FALSE=ended
    // - caller may have checked that some data is pending to read
    function ProcessLoopStepReceive(FrameProcessed: PBoolean): boolean;
    procedure ProcessLoopReceived(var request: TWebSocketFrame);
    // called by ProcessLoop - TRUE=continue, FALSE=ended
    // - caller may check that LastPingDelay>fSettings.SendDelay and Socket is writable
    function ProcessLoopStepSend: boolean;
    // blocking process, for one thread handling all WebSocket connection process
    procedure ProcessLoop;
    function ComputeContext(
      out RequestProcess: TOnHttpServerRequest): THttpServerRequestAbstract;
        virtual; abstract;
    procedure Log(const frame: TWebSocketFrame; const aMethodName: ShortString;
      aEvent: TSynLogLevel = sllTrace; DisableRemoteLog: boolean = false); virtual;
    function SendPendingOutgoingFrames: integer;
    procedure WaitThreadStarted;
    function HiResDelay(var start: Int64): Int64;
  public
    /// initialize the WebSockets process on a given connection
    // - the supplied TWebSocketProtocol will be owned by this instance
    // - other parameters should reflect the client or server expectations
    constructor Create(aProtocol: TWebSocketProtocol; aOwnerThread: TSynThread;
      aSettings: PWebSocketProcessSettings;
      const aProcessName: RawUtf8); reintroduce;
    /// finalize the context
    // - if needed, will notify the other end with a focConnectionClose frame
    // - will release the TWebSocketProtocol associated instance
    destructor Destroy; override;
    /// reuse an existing process instance on a new connection
    // - called e.g. by THttpClientWebSockets.WebSocketsUpgrade(aReconnect=true)
    procedure Reset(aConnectionID: THttpServerConnectionID); virtual;
    /// abstract low-level method to retrieve pending input data
    // - should return the number of bytes (<=count) received and written to P
    // - is defined separated to allow multi-thread pooling
    function ReceiveBytes(P: PAnsiChar; count: PtrInt): integer; virtual; abstract;
    /// abstract low-level method to send pending output data
    // - returns false on any error, try on success
    // - is defined separated to allow multi-thread pooling
    function SendBytes(P: pointer; Len: PtrInt): boolean; virtual; abstract;
    /// abstract low-level method to check if there is some pending input data
    // in the input Socket ready for GetFrame/ReceiveBytes
    // - is defined separated to allow multi-thread pooling
    function CanGetFrame(TimeOut: cardinal;
      ErrorWithoutException: PInteger): boolean; virtual; abstract;
    /// (blocking) process incoming WebSockets framing protocol
    // - CanGetFrame should have been called and returned true before
    // - will call overriden ReceiveBytes() for the actual communication
    function GetFrame(out Frame: TWebSocketFrame; Blocking: boolean;
      ErrorWithoutException: PInteger): boolean;
    /// process outgoing WebSockets framing protocol
    // - will call overriden SendBytes() for immediate transmission
    // - use SendFrameAsync() to send frames asynchronously S(with optional
    // jumboframes gathering)
    function SendFrame(var Frame: TWebSocketFrame): boolean;
    /// delayed process of outgoing WebSockets framing protocol
    // - by default, store the frame in Outgoing.Push() internal list
    // - some protocols could implement optional jumboframe gathering
    procedure SendFrameAsync(const Frame: TWebSocketFrame); virtual;
    /// will push a request or notification to the other end of the connection
    // - caller should set the aRequest with the outgoing parameters, and
    // optionally receive a response from the other end
    // - the request may be sent in blocking or non blocking mode
    // - returns the HTTP Status code (e.g. HTTP_SUCCESS=200 for success)
    function NotifyCallback(aRequest: THttpServerRequestAbstract;
      aMode: TWebSocketProcessNotifyCallback): cardinal; virtual;
    /// send a focConnectionClose frame (if not already sent) and set wpsClose
    procedure Shutdown(waitForPong: boolean);
    /// returns the current state of the underlying connection
    function State: TWebSocketProcessState;
      {$ifdef HASINLINE}inline;{$endif}
      /// the associated Remote IP as set by the raw socket layer
    // - returns '' if Protocol=nil or Protocol.RemoteLocalhost=true on localhost
    function RemoteIP: RawUtf8;
      {$ifdef HASINLINE}inline;{$endif}
    /// the settings currently used during the WebSockets process
    // - points to the owner instance, e.g. TWebSocketServer.Settings or
    // THttpClientWebSockets.Settings field
    property Settings: PWebSocketProcessSettings
      read fSettings;
    /// direct access to the low-level incoming frame stack
    property Incoming: TWebSocketFrameList
      read fIncoming;
    /// direct access to the low-level outgoing frame stack
    // - you should not use this property, but SendFrameAsync() virtual method
    property Outgoing: TWebSocketFrameList
      read fOutgoing;
    /// the associated low-level processing thread
    property OwnerThread: TSynThread
      read fOwnerThread;
    /// how many frames are currently processed by this connection
    property ProcessCount: integer
      read fProcessCount;
    /// may be set to TRUE before Destroy to force raw socket disconnection
    property ConnectionCloseWasSent: boolean
      read fConnectionCloseWasSent write fConnectionCloseWasSent;
  published
    /// the Sec-WebSocket-Protocol application protocol currently involved
    // - TWebSocketProtocolJson or TWebSocketProtocolBinary in the mORMot context
    // - could be nil if the connection is in standard HTTP/1.1 mode
    property Protocol: TWebSocketProtocol
      read fProtocol;
    /// the associated process name
    property ProcessName: RawUtf8
      read fProcessName write fProcessName;
    /// how many invalid heartbeat frames have been sent
    // - a non 0 value indicates a connection problem
    property InvalidPingSendCount: cardinal
      read fInvalidPingSendCount;
  end;

  /// TCrtSocket-based WebSockets process, used on both client or server sides
  // - will use the socket in blocking mode, so expects its own processing thread
  TWebCrtSocketProcess = class(TWebSocketProcess)
  protected
    fSocket: TCrtSocket;
  public
    /// initialize the WebSockets process on a given TCrtSocket connection
    // - the supplied TWebSocketProtocol will be owned by this instance
    // - other parameters should reflect the client or server expectations
    constructor Create(aSocket: TCrtSocket; aProtocol: TWebSocketProtocol;
      aOwnerThread: TSynThread; aSettings: PWebSocketProcessSettings;
      const aProcessName: RawUtf8); reintroduce; virtual;
    /// first step of the low level incoming WebSockets framing protocol over TCrtSocket
    // - call fSocket.SockInPending to check for pending data
    function CanGetFrame(TimeOut: cardinal;
      ErrorWithoutException: PInteger): boolean; override;
    /// low level receive incoming WebSockets frame data over TCrtSocket
    // - call fSocket.SockInRead to check for pending data
    function ReceiveBytes(P: PAnsiChar; count: PtrInt): integer; override;
    /// low-level method to send pending output data over TCrtSocket
    // - call fSocket.TrySndLow to send pending data
    function SendBytes(P: pointer; Len: PtrInt): boolean; override;
    /// the associated communication socket
    // - on the server side, is a THttpServerSocket
    // - access to this instance is protected by Safe.Lock/Unlock
    property Socket: TCrtSocket
      read fSocket;
  end;

/// returns the text corresponding to a specified WebSockets sending mode
function ToText(mode: TWebSocketProcessNotifyCallback): PShortString; overload;

/// returns the text corresponding to a specified WebSockets state
function ToText(st: TWebSocketProcessState): PShortString; overload;



{ ******************** WebSockets Asynchronous Frames Parsing }

type
  /// define our work memory buffer for low-level WebSockets frame headers
  TFrameHeader = packed record
    first: byte;
    len8: byte;
    len32: cardinal;
    len64: cardinal;
    mask: cardinal; // mask=0 indicates no payload masking
  end;

  /// states of the WebSockets parsing asynchronous machine
  TWebProcessInFrameState = (
    pfsHeader1,
    pfsData1,
    pfsHeaderN,
    pfsDataN,
    pfsDone,
    pfsError);

  /// asynchronous state machine to process WebSockets incoming frames
  {$ifdef USERECORDWITHMETHODS}
  TWebProcessInFrame = record
  {$else}
  TWebProcessInFrame = object
  {$endif USERECORDWITHMETHODS}
  public
    hdr: TFrameHeader;
    opcode: TWebSocketFrameOpCode;
    masked: boolean;
    state: TWebProcessInFrameState;
    process: TWebSocketProcess;
    outputframe: PWebSocketFrame;
    len: integer;
    data: RawByteString; // will eventually be appended to outputframe.payload
    procedure Init(Owner: TWebSocketProcess; output: PWebSocketFrame);
    function HasBytes(P: PAnsiChar; count: integer): boolean;
      {$ifdef HASINLINE} inline; {$endif}
    function GetHeader: boolean;
    function GetData: boolean;
    function Step(ErrorWithoutException: PInteger): TWebProcessInFrameState;
  end;

  /// reusable encoder for WebSockets outgoing frames
  {$ifdef USERECORDWITHMETHODS}
  TWebSocketFrameEncoder = record
  {$else}
  TWebSocketFrameEncoder = object
  {$endif USERECORDWITHMETHODS}
  public
    hdr: TFrameHeader;
    hdrlen, len: cardinal;
    function Prepare(const Frame: TWebSocketFrame; MaskSentFrames: cardinal): integer;
    function Encode(const Frame: TWebSocketFrame; Dest: PAnsiChar): integer;
  end;


{ ******************** TWebSocketProtocolChat Simple Protocol }

type
  /// callback event triggered by TWebSocketProtocolChat for any incoming message
  // - a first call with frame.opcode=focContinuation will take place when
  // the connection will be upgraded to WebSockets
  // - then any incoming focText/focBinary events will trigger this callback
  // - eventually, a focConnectionClose will notify the connection ending
  TOnWebSocketProtocolChatIncomingFrame = procedure(
    Sender: TWebSocketProcess; const Frame: TWebSocketFrame) of object;

  /// simple chatting protocol, allowing to receive and send WebSocket frames
  // - you can use this protocol to implement simple asynchronous communication
  // with events expecting no answers, e.g. from or as AJAX applications
  // - as used e.g. by sample ex/rest-websockets/restws_simpleechoserver.dpr
  // - see TWebSocketProtocolRest for bi-directional events expecting answers,
  // as between mORMot client and server
  TWebSocketProtocolChat = class(TWebSocketProtocol)
  protected
    fOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Request: TWebSocketFrame; const Info: RawUtf8); override;
  public
    /// initialize the chat protocol with an incoming frame callback
    // - if you need no "Sec-WebSocket-Protocol:" header, specify aName = ''
    constructor Create(const aName, aUri: RawUtf8;
       const aOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame); overload;
    /// compute a new instance of this WebSockets protocol, with same parameters
    function Clone(const aClientUri: RawUtf8): TWebSocketProtocol; override;
    /// allows to send a message over the wire to a specified connection
    // - a temporary copy of the Frame content will be made for safety
    // - Sender identify the connection, typically from OnIncomingFrame callback
    function SendFrame(Sender: TWebSocketProcess;
       const Frame: TWebSocketFrame): boolean;
    /// allows to send a JSON message over the wire to a specified connection
    // - a temporary copy of the Json content will be made for safety
    function SendFrameJson(Sender: TWebSocketProcess;
       const Json: RawUtf8): boolean;
    /// you can assign an event to this property to be notified of incoming messages
    property OnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame
      read fOnIncomingFrame write fOnIncomingFrame;
  end;

var
  /// if set, will log all WebSockets raw information
  // - see also TWebSocketProcessSettings.LogDetails and
  // TWebSocketProcessSettings.SetFullLog to setup even more verbose information,
  // e.g. by setting HttpServerFullWebSocketsLog and HttpClientFullWebSocketsLog
  // global variables to true (as defined in mormot.rest.http.server/client)
  WebSocketLog: TSynLogClass;

  /// number of bytes above which SynLZ compression may be done
  // - when working with TWebSocketProtocolBinary
  // - it is useless to compress smallest frames, which fits in network MTU
  WebSocketsBinarySynLzThreshold: integer = 450;

  /// the allowed maximum size, in MB, of a WebSockets frame
  WebSocketsMaxFrameMB: cardinal = 256;


{ ****************** Socket.IO / Engine.IO Raw Protocols }

type
  /// define the Engine.IO available packet types
  // - defined in their numeric order, so ord() would give the proper ID number
  // - eioOpen (0) is used during the handshake
  // - eioClose (1) indicates that a transport can be closed
  // - eioPing (2) and eioPong (3) implement a heartbeat mechanism
  // - eioMessage (4) sends a payload to the other side
  // - eioUpgrade (5) and eioNoop (6) are used during the upgrade process
  TEngineIOPacket = (
   eioOpen,
   eioClose,
   eioPing,
   eioPong,
   eioMessage,
   eioUpgrade,
   eioNoop);
  PEngineIOPacket = ^TEngineIOPacket;

  /// define the Socket.IO available packet types
  // - defined in their numeric order, so ord() would give the proper ID number
  // - such packets are likely to be nested in a eioMessage frame
  // - sioConnect (0) and sioConnectError (4) are used during connection to a namespace
  // - sioDisconnect (1) is used when disconnecting from a namespace
  // - sioEvent (2) is used to send data to the other side
  // - sioAck (3) is used to acknowledge an event
  // - sioBinaryEvent (5) is used to send binary data to the other side
  // - sioBinaryAck (6) is used to acknowledge an event with a binary response
  TSocketIOPacket = (
    sioConnect,
    sioDisconnect,
    sioEvent,
    sioAck,
    sioConnectError,
    sioBinaryEvent,
    sioBinaryAck);
  PSocketIOPacket = ^TSocketIOPacket;

  /// Socket.IO sequence used for event acknowledgment
  TSocketIOAckID = integer;

  /// define a raw decoded Socket.IO message content
  {$ifdef USERECORDWITHMETHODS}
  TSocketIOMessage = record
  {$else}
  TSocketIOMessage = object
  {$endif USERECORDWITHMETHODS}
  private
    fSender: TWebSocketProcess;
    fNameSpace: pointer;
    fNameSpaceLen: PtrInt;
    fData: PUtf8Char;
    fDataLen: PtrInt;
    fPacketType: TSocketIOPacket;
    fDataBinary: boolean;
    fID: TSocketIOAckID;
    fBinaryAttachment: cardinal;
  public
    /// decode a Socket.IO raw text packet into its message fields
    // - mainly used for testing purposes
    function Init(const PayLoad: RawUtf8): boolean;
    /// decode a Socket.IO raw packet into its message fields
    // - returns true on success, false if the input PayLoad is incorrect
    function InitBuffer(PayLoad: PUtf8Char; PayLoadLen: PtrInt;
      PayLoadBinary: boolean; Process: TWebSocketProcess): boolean;
    /// quickly check if the NameSpace value does match (case sensitive)
    function NameSpaceIs(const Name: RawUtf8): boolean;
    /// retrieve the NameSpace value as a new RawUtf8
    procedure NameSpaceGet(out Dest: RawUtf8);
    /// retrieve the NameSpace value as a shortstring (used e.g. for RaiseESockIO)
    function NameSpaceShort: ShortString;
      {$ifdef HASINLINE} inline; {$endif}
    /// quickly check if the Data content does match (mainly used for testing)
    function DataIs(const Content: RawUtf8): boolean;
    /// decode the Data content JSON payload into a TDocVariant
    // - can optionally override the default JSON_SOCKETIO options
    // - warning: the Data/DataLen buffer will be decoded in-place, so modified
    function DataGet(out Dest: TDocVariantData;
      Options: PDocVariantOptions = nil): boolean;
    /// raise a ESockIO exception with the specified text context
    procedure RaiseESockIO(const ctx: RawUtf8);
    /// low-level kind of Socket.IO packet of this message
    property PacketType: TSocketIOPacket
      read fPacketType;
    /// optional low-level Socket.IO acknowledge ID of this message
    property ID: TSocketIOAckID
      read fID;
    /// optional low-level Socket.IO binary attachement ID of this message
    property BinaryAttachment: cardinal
      read fBinaryAttachment;
    /// access to the internal NameSpace text buffer - for internal use
    // - call NameSpaceIs() and NameSpaceGet() functions instead
    // - warning: this buffer is NOT #0 ended but follows NameSpaceLen
    property NameSpace: pointer
      read fNameSpace;
    /// access to the internal NameSpace text buffer length - for internal use
    property NameSpaceLen: PtrInt
      read fNameSpaceLen;
    /// low-level access to the WebSockets instance specified to InitBuffer
    property Sender: TWebSocketProcess
      read fSender;
  end;

const
  /// constant used if no TSocketIOAckID is necessary
  SIO_NO_ACK = 0;

function ToText(p: TEngineIOPacket): PShortString; overload;
function ToText(p: TSocketIOPacket): PShortString; overload;

var
  /// define how TSocketIOMessage.DataGet() returns its TDocVariant value
  // - equals JSON_[mNameValue] by default to include dvoNameCaseSensitive
  JSON_SOCKETIO: TDocVariantOptions =
    [dvoReturnNullForUnknownProperty,
     dvoValueCopiedByReference,
     dvoNameCaseSensitive];

type
  TSocketIOLocalNamespace = class;

  /// exception class raised during Engine.IO process
  EEngineIO = class(ESynException);
  /// exception class raised during Socket.IO process
  ESocketIO = class(ESynException);

  /// Socket.IO process Acknowledgment callback
  TOnSocketIOAck = procedure(const Message: TSocketIOMessage) of object;

  /// internal slot for one Socket.IO process Acknowledgment callback
  TSocketIOCallback = record
    Ack: TSocketIOAckID;
    OnAck: TOnSocketIOAck;
  end;
  PSocketIOCallback = ^TSocketIOCallback;

  /// Socket.IO process Event handler callback signature
  // - the associated JSON data is decoded and supplied as a TDocVariant dvArray
  // - if the result is not '', it is expected to be JSON array acknowledgment
  // payload, e.g. from JsonEncodeArray([])
  TOnSocketIOEvent = function(Sender: TSocketIOLocalNamespace;
    const EventName: RawUtf8; const Data: TDocVariantData): RawJson of object;
  /// Socket.IO process published methods handler signature
  // - the associated JSON data is decoded and supplied as a TDocVariant dvArray
  // - required signature of TSocketIOLocalNamespace.RegisterPublishedMethods()
  // - if the result is not '', it is expected to be JSON array acknowledgment
  // payload, e.g. from JsonEncodeArray([])
  TOnSocketIOMethod = function(const Data: TDocVariantData): RawJson of object;

  /// abstract parent for client side and server side Engine.IO sessions support
  // - several Socket.IO namespaces are maintained over this main Engine.IO session
  // - properties do contain the values of a typical connection response like
  // ${"sid":"dwy_mNoFzsrMlhINAAAA","upgrades":[],"pingTimeout":20000,"pingInterval":25000}
  // - since we only implement WebSocket, "upgrades" is expected to be always void
  TEngineIOAbstract = class(TSynPersistent)
  protected
    fWebSockets: TWebCrtSocketProcess;
    fEngineSid: RawUtf8;
    fPingInterval, fPingTimeout, fMaxPayload, fVersion: integer;
  public
    /// initialize this instance with its default values
    constructor Create; override;
    /// the associated Engine.IO Session ID
    // - as computed on the server side, and received on client side as "sid"
    // - is typically a base-64 encoded binary like 'dwy_mNoFzsrMlhINAAAA'
    property EngineSid: RawUtf8
      read fEngineSid;
    /// the protocol version used by this class, as used for EIO=? parameter
    // - is currently fixed to 4
    property Version: integer
      read fVersion;
    /// the ping timeout, used in Engine.IO heartbeat mechanism (in milliseconds)
    // - ping packets are now sent by the server, since protocol v4
    // - as computed on the server side, and received on client side as "pingTimeout"
    // - default value is 20000, i.e. 20 seconds
    property PingTimeout: integer
      read fPingTimeout write fPingTimeout;
    /// the ping interval, used in Engine.IO heartbeat mechanism (in milliseconds)
    // - ping packets are now sent by the server, since protocol v4
    // - as computed on the server side, and received on client side as "pingInterval"
    // - default value is 25000, i.e. 25 seconds
    property PingInterval: integer
      read fPingInterval write fPingInterval;
    /// optional number of bytes per chunk, used in Engine.IO payloads mechanism
    // - as computed on the server side, and received on client side as "maxPayload"
    // - default value is 0, meaning no limitation is requested
    property MaxPayload: integer
      read fMaxPayload write fMaxPayload;
  end;

  /// abstract parent for local and remote socketio namespaces
  // - each session (sid) has its own namespace
  TSocketIONamespace = class(TSynPersistent)
  protected
    fOwner: TEngineIOAbstract;
    fNameSpace: RawUtf8;
  public
    /// access to the associates Engine.IO main connection
    property Owner: TEngineIOAbstract
      read fOwner;
  published
    /// the associated namespace, as established between client and server
    // - default namespace is '/'
    property NameSpace: RawUtf8
      read fNameSpace;
  end;
  PSocketIONamespace = ^TSocketIONamespace;

  /// abstract parent class for remote namespace object
  // - emit events to remote namespace (ie client->server or server->client)
  // - dispatch received sioAck to effective handler
  TSocketIORemoteNamespace = class(TSocketIONamespace)
  protected
    fSid, fHandshakeData: RawUtf8;
    fAckIdCursor: TSocketIOAckID;
    fCallbacks: array of TSocketIOCallback;
    /// Generate a new event acknowledgment ID, incrementing the internal cursor
    function GenerateAckId(const aOnAck: TOnSocketIOAck): TSocketIOAckID;
  public
    /// initialize this instance
    constructor Create(aOwner: TEngineIOAbstract;
      const aNameSpace, aSid, aHandshakeData: RawUtf8); reintroduce; virtual;
    /// initialize a new instance from an incoming connection message
    constructor CreateFromConnectMessage(const aMessage: TSocketIOMessage;
      const aHandshakeData: RawUtf8; aOwner: TEngineIOAbstract);
    /// emit an event to Self.NameSpace, with an optional callback
    // - returns the packet ID if aCallback is assigned, SIO_NO_ACK otherwise
    // - aDataArray is an optional JSON array of values, without any [ ] chars
    function SendEvent(const aEventName: RawUtf8; const aDataArray: RawUtf8 = '';
      const aOnAck: TOnSocketIOAck = nil): TSocketIOAckID;
    /// handle an acknowledge message and call the associated callback
    // - will raise an ESocketIO if the packet is invalid or ID was not found
    procedure Acknowledge(const aMessage: TSocketIOMessage);
    /// low-level associated JSON array data supplied to Connect()
    property HandshakeData: RawUtf8
      read fHandshakeData write fHandshakeData;
  published
    /// the associated Socket.IO Session ID, as computed on the server side
    property Sid: RawUtf8
      read fSid;
  end;
  PSocketIORemoteNamespace = ^TSocketIORemoteNamespace;
  TSocketIORemoteNamespaces = array of TSocketIORemoteNamespace;

  /// a local Socket.IO namespace definition
  TEventHandler = record
    /// the event name (should be the first field for TDynArrayHashed)
    Name: RawUtf8;
    /// the event callback which will be executed for this event name
    OnEvent: TOnSocketIOEvent;
    /// the published method which will be executed for this event name
    OnMethod: TOnSocketIOMethod;
  end;
  PEventHandler = ^TEventHandler;
  TLocalNamespaceEventHandlers = array of TEventHandler;

  /// options to tune globally TSocketIOLocalNamespace behavior
  // - snoIgnoreUnknownEvent will let HandleEvent() raise no exception and
  // ignore if the received event name is unknown
  // - snoIgnoreIncorrectData will let HandleEvent raise no exception and
  // ignore if the received message has unexpected/incorrect associated JSON
  TSocketIOLocalNamespaceOption = (
    snoIgnoreUnknownEvent,
    snoIgnoreIncorrectData);
  TSocketIOLocalNamespaceOptions = set of TSocketIOLocalNamespaceOption;

  /// abstract parent class for local namespace object
  // - map namespace event to their handlers
  // - dispatch received sioEvent to effective handler
  TSocketIOLocalNamespace = class(TSocketIONamespace)
  protected
    fHandler: TLocalNamespaceEventHandlers;
    fHandlers: TDynArrayHashed;
    fOptions: TSocketIOLocalNamespaceOptions;
    // called by Create: can override this method to register some events
    procedure RegisterHandlers; virtual;
  public
    /// global callback triggerred when any event message is received and
    // decoded for this name space
    OnEventReceived: procedure(Sender: TSocketIOLocalNamespace;
      const EventName: RawUtf8; var Data: TDocVariantData) of object;
    /// initialize this instance
    constructor Create(aOwner: TEngineIOAbstract;
      const aNamespace: RawUtf8 = '/'); reintroduce;
    /// register an event with an associated callback
    // - returns self to be used as a fluid interface, e.g.
    // from TSocketsIOClient.Local()
    function RegisterEvent(const aEventName: RawUtf8;
      const aCallback: TOnSocketIOEvent): TSocketIOLocalNamespace;
    /// register all published methods of a class as event handlers
    // - published method names are case-sensitive Socket.IO event names
    // - the methods should follow the TOnSocketIOMethod exact signature, e.g.
    // ! function eventname(const Data: TDocVariantData): RawJson;
    procedure RegisterPublishedMethods(aInstance: TObject);
    /// register all handlers of another local namespace
    // - as used by TSocketsIOClient.OnReconnect()
    procedure RegisterFrom(aAnother: TSocketIOLocalNamespace);
    /// dispatch an event message to the appropriate handler
    procedure HandleEvent(const aMessage: TSocketIOMessage;
      aIgnoreUnknownEvent: boolean); virtual;
    /// raw access to the internal events list
    property Handler: TLocalNamespaceEventHandlers
      read fHandler;
    /// customize the process on this local namespace
    property Options: TSocketIOLocalNamespaceOptions
      read fOptions write fOptions;
  end;
  PSocketIOLocalNamespace = ^TSocketIOLocalNamespace;
  TSocketIOLocalNamespaces = array of TSocketIOLocalNamespace;

  /// meta-class of the TSocketIOLocalNamespace hierarchy
  TSocketIOLocalNamespaceClass = class of TSocketIOLocalNamespace;

  /// abstract parent for client or server Engine.IO protocol over WebSockets frames
  // - Engine.IO is the low-level connection/heartbeat protocol on which the
  // Socket.IO bidirectional multiplexed communication protocol is built
  // - this abstract class will redirect all incoming frames to FrameReceived(),
  // send an eioPong when an eioPing is received
  TWebSocketEngineIOProtocol = class(TWebSocketProtocol)
  protected
    fOpened: boolean;
    procedure ProcessIncomingFrame(Sender: TWebSocketProcess;
      var Request: TWebSocketFrame; const Info: RawUtf8); override;
    procedure EnginePacketReceived(Sender: TWebSocketProcess; PacketType: TEngineIOPacket;
      PayLoad: PUtf8Char; PayLoadLen: PtrInt; PayLoadBinary: boolean); virtual; abstract;
  public
    // overriden to return '' i.e. recognize by URI, not "Sec-WebSocket-Protocol:"
    function GetSubprotocols: RawUtf8; override;
    function SetSubprotocol(const aProtocolName: RawUtf8): boolean; override;
    /// true when Engine.IO messages can be processed
    // - i.e. after OPEN (eioOpen) and before CLOSE (eioClose)
    property Opened: boolean
      read fOpened;
  end;

  /// abstract parent for client or server Socket.IO protocol over WebSockets frames
  TWebSocketSocketIOProtocol = class(TWebSocketEngineIOProtocol)
  protected
    // this is the (abstract) main entry point for incoming Socket.IO messages
    procedure SocketPacketReceived(const Message: TSocketIOMessage); virtual; abstract;
  end;

/// compute the URI for a WebSocket-only Engine.IO upgrade
// - server should respond with a HTTP_SWITCHINGPROTOCOLS = 101 response,
// followed with a eioOpen response frame
// - PollingUpgradeSid can be used from an upgrade on a long-polling connection
function EngineIOHandshakeUri(const Root: RawUtf8 = '/socket.io/';
  const PollingUpgradeSid: RawUtf8 = ''): RawUtf8;

/// encode and send an Engine.IO message over the wire to a specified connection
// - Sender identifies the connection, typically from FrameReceived() method
function EngineIOSendPacket(Sender: TWebSocketProcess;
  Payload: pointer; PayloadLen: PtrInt; PayLoadBinary: boolean;
  PacketType: TEngineIOPacket = eioMessage): boolean;


/// Socket.IO event names 'connect', 'message' and 'disconnect' are reserved
function SocketIOReserved(const event: RawUtf8): boolean;

/// encode and send a Socket.IO packet to a given WebSockets connection
procedure SocketIOSendPacket(aWebSockets: TWebCrtSocketProcess;
  aOperation: TSocketIOPacket; const aNamespace: RawUtf8;
  aPayload: pointer = nil; aPayloadLen: PtrInt = 0;
  ackId: TSocketIOAckID = SIO_NO_ACK);

/// efficient case-sensitive search within an array of TSocketIONamespace
// - if name = '' (i.e. namelen is 0), will search for '/'
function SocketIOGetNameSpace(one: PSocketIONamespace; count: integer;
  name: PUtf8Char; namelen: TStrLen): TSocketIONamespace;

/// retrieve the NameSpace properties of an array of TSocketIONamespace
procedure SocketIOGetNameSpaces(one: PSocketIONamespace; count: integer;
  out Dest: TRawUtf8DynArray);


implementation


{ ******************** WebSockets Frames Definitions }

var
  _TWebSocketFrameOpCode:
    array[TWebSocketFrameOpCode] of PShortString;
  _TWebSocketProcessNotifyCallback:
    array[TWebSocketProcessNotifyCallback] of PShortString;

function ToText(opcode: TWebSocketFrameOpCode): PShortString;
begin
  result := _TWebSocketFrameOpCode[opcode];
end;

function ToText(mode: TWebSocketProcessNotifyCallback): PShortString;
begin
  result := _TWebSocketProcessNotifyCallback[mode];
end;

function ToText(st: TWebSocketProcessState): PShortString;
begin
  result := GetEnumName(TypeInfo(TWebSocketProcessState), ord(st));
end;

procedure ComputeChallenge(const Base64: RawByteString; out Digest: TSha1Digest);
const
  // see https://tools.ietf.org/html/rfc6455
  SALT: string[36] = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
var
  SHA: TSha1;
begin
  SHA.Init;
  SHA.Update(pointer(Base64), length(Base64));
  SHA.Update(@SALT[1], 36);
  SHA.Final(Digest);
end;

procedure ProcessMask(data: PCardinalArray; mask: PtrUInt; len: PtrInt);
var
  i: PtrInt;
begin
  i := len shr 2;
  if i <> 0 then
  begin
    data := @data[i];
    i := -i;
    repeat
      data^[i] := data^[i] xor mask;
      inc(i);
    until i = 0;
  end;
  for i := 0 to (len and 3) - 1 do
  begin
    PByteArray(data)^[i] := PByteArray(data)^[i] xor mask;
    mask := mask shr 8;
  end;
end;

const
  FRAME_HEAD_SEP = #1;

procedure FrameInit(opcode: TWebSocketFrameOpCode;
  const Content, ContentType: RawByteString; out frame: TWebSocketFrame);
begin
  frame.opcode := opcode;
  if (ContentType <> '') and
     (Content <> '') and
     not IsContentTypeTextU(ContentType) and
     IsContentCompressed(pointer(Content), length(Content)) then
    frame.content := [fopAlreadyCompressed]
  else
    frame.content := [];
end;

{ TWebSocketFrameEncoder }

function TWebSocketFrameEncoder.Prepare(const Frame: TWebSocketFrame;
  MaskSentFrames: cardinal): integer;
begin
  len := Length(Frame.payload);
  hdr.first := byte(Frame.opcode) or FRAME_OPCODE_FIN; // single frame
  if len < FRAME_LEN_2BYTES then
  begin
    hdr.len8 := len or MaskSentFrames;
    hdrlen := 2; // opcode+len8
  end
  else if len < 65536 then
  begin
    hdr.len8 := FRAME_LEN_2BYTES or MaskSentFrames;
    hdr.len32 := bswap16(len);
    hdrlen := 4; // opcode+len8+len32.low
  end
  else
  begin
    hdr.len8 := FRAME_LEN_8BYTES or MaskSentFrames;
    hdr.len64 := bswap32(len);
    hdr.len32 := 0;
    hdrlen := 10; // opcode+len8+len32+len64.low
  end;
  if MaskSentFrames <> 0 then
  begin
    // https://tools.ietf.org/html/rfc6455#section-10.3
    // client-to-server masking is mandatory (but not from server to client)
    hdr.mask := Random32Not0;
    inc(hdrlen, 4);
  end
  else
    hdr.mask := 0;
  result := hdrlen + len;
end;

function TWebSocketFrameEncoder.Encode(
  const Frame: TWebSocketFrame; Dest: PAnsiChar): integer;
begin
  MoveByOne(@hdr, Dest, hdrlen);  // 2/4 bytes for small/common frames
  inc(Dest, hdrlen);
  if hdr.mask <> 0 then
    // hdr.mask is not at the right position: append to actual end of header
    PInteger(Dest - 4)^ := hdr.mask;
  MoveFast(pointer(Frame.payload)^, Dest^, len);
  if hdr.mask <> 0 then
    ProcessMask(pointer(Dest), hdr.mask, len); // only on client side
  result := hdrlen + len;
end;

procedure FrameSendEncode(const Frame: TWebSocketFrame;
  MaskSentFrames: cardinal; var ToSend: TSynTempBuffer);
var
  encoder: TWebSocketFrameEncoder;
begin
  ToSend.Init(encoder.Prepare(Frame, MaskSentFrames));
  encoder.Encode(Frame, ToSend.buf);
end;



{ ******************** WebSockets Protocols Implementation }

{ TWebSocketProtocol }

constructor TWebSocketProtocol.Create(const aName, aUri: RawUtf8);
begin
  inherited Create; // may have been overriden
  fName := aName;
  fUri := aUri;
  fConnectionFlags := [hsrWebsockets];
end;

procedure TWebSocketProtocol.Reset;
begin
end;

function TWebSocketProtocol.Clone(const aClientUri: RawUtf8): TWebSocketProtocol;
begin
  result := nil; // no clone needed for a client-side protocol
end;

procedure TWebSocketProtocol.AfterUpgrade(aProcess: TWebSocketProcess);
begin
  // see e.g. TWebSocketSocketIOClientProtocol.AfterUpgrade override
end;

procedure TWebSocketProtocol.SetEncryptKey(aServer: boolean; const aKey: RawUtf8;
  aSettings: PWebSocketProcessSettings);
var
  key: THash256Rec;
begin
  // always first disable any previous encryption
  fEncryption := nil;
  fConnectionFlags := [hsrWebsockets];
  if (aKey = '') or
     (aSettings = nil) then
    exit;
  // 1. try asymmetric ES-256 ephemeral secret key and mutual authentication
  // check human-friendly format 'password#*.private' key file name
  with aSettings^ do
    fEncryption := TEcdheProtocol.FromPasswordSecureFile(
      aKey, aServer, EcdheAuth, EcdheCipher, EcdheRounds);
  if fEncryption = nil then
    // check 'a=mutual;e=aesctc128;p=34a2;pw=password;ca=..' full format
    fEncryption := TEcdheProtocol.FromKey(aKey, aServer);
  if fEncryption <> nil then
    include(fConnectionFlags, hsrSecured)
  else
  begin
    // 2. aKey no 'a=...'/'pw#xx.private' layout -> use symmetric TProtocolAes
    if aSettings^.AesRounds = 0 then
      // mORMot 1.18 deprecated password derivation
      Sha256Weak(aKey, key.b)
    else
      // new safer password derivation algorithm (rounds=1000 -> 1ms)
      Pbkdf2Sha3(SHA3_256, aKey, aSettings^.AesSalt, aSettings^.AesRounds,
        @key, SizeOf(key));
    SetEncryptKeyAes(aSettings^.AesCipher, key, aSettings^.AesBits);
  end;
end;

procedure TWebSocketProtocol.SetEncryptKeyAes(aCipher: TAesAbstractClass;
  const aKey; aKeySize: cardinal);
begin
  fEncryption := nil;
  fConnectionFlags := [hsrWebsockets];
  if aKeySize < 128 then
    exit;
  fEncryption := TProtocolAes.Create(aCipher, aKey, aKeySize);
  include(fConnectionFlags, hsrSecured)
end;

procedure TWebSocketProtocol.SetEncryptKeyEcdhe(aAuth: TEcdheAuth;
  aPKI: TEccCertificateChain; aPrivate: TEccCertificateSecret; aServer: boolean;
  aEF: TEcdheEF; aPrivateOwned: boolean);
begin
  fEncryption := nil;
  fConnectionFlags := [hsrWebsockets];
  fEncryption := ECDHEPROT_CLASS[aServer].Create(
    aAuth, aPKI, aPrivate, aEF, aPrivateOwned);
  include(fConnectionFlags, hsrSecured)
end;

procedure TWebSocketProtocol.AfterGetFrame(var frame: TWebSocketFrame);
begin
  inc(fFramesInCount);
  inc(fFramesInBytes, length(frame.payload) + 2);
end;

procedure TWebSocketProtocol.BeforeSendFrame(var frame: TWebSocketFrame);
begin
  inc(fFramesOutCount);
  inc(fFramesOutBytes, length(frame.payload) + 2);
end;

function TWebSocketProtocol.FrameData(const frame: TWebSocketFrame;
  const Head: RawUtf8; HeadFound: PRawUtf8; PMax: PPByte): pointer;
begin
  result := nil; // no frame type by default
end;

function TWebSocketProtocol.FrameType(const frame: TWebSocketFrame): TShort31;
begin
  result := '*'; // no frame URI by default
end;

function TWebSocketProtocol.ProcessHandshake(const ExtIn: TRawUtf8DynArray;
  out ExtOut: RawUtf8; ErrorMsg: PRawUtf8): boolean;
var
  res: TProtocolResult;
  msgin, msgout: RawUtf8;
  synhk: boolean;
  i: PtrInt;
begin
  result := fEncryption = nil;
  if result then
    exit; // nothing to handshake for -> try to continue
  synhk := false;
  if ExtIn <> nil then
  begin
    for i := 0 to length(ExtIn) - 1 do
      if PropNameEquals(ExtIn[i], 'synhk') then
        synhk := true
      else if synhk and
              IdemPChar(pointer(ExtIn[i]), 'HK=') then
      begin
        msgin := copy(ExtIn[i], 4, maxInt);
        break;
      end;
    if ({%H-}msgin = '') or
       not synhk then
      exit;
  end;
  res := fEncryption.ProcessHandshake(msgin, msgout); // e.g. TEcdheProtocol
  case res of
    sprSuccess:
      begin
        AddToCsv('synhk; hk=' + msgout, ExtOut{%H-}, '; ');
        result := true;
        exit;
      end;
    sprUnsupported:
      if not synhk then
      begin
        result := true; // try to continue execution
        exit;
      end;
  end;
  WebSocketLog.Add.Log(sllWarning, 'ProcessHandshake=% In=[%]',
    [ToText(res)^, msgin], self);
  if ErrorMsg <> nil then
    ErrorMsg^ := FormatUtf8('%: %', [ErrorMsg^,
      GetCaptionFromEnum(TypeInfo(TProtocolResult), ord(res))]);
end;

function TWebSocketProtocol.ProcessHandshakeUri(
  const aClientUri: RawUtf8): boolean;
begin
  result := true; // override and return false to return HTTP_UNAUTHORIZED
end;

function TWebSocketProtocol.SendFrames(Owner: TWebSocketProcess;
  var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean;
var
  i, n: PtrInt;
begin
  // this default implementation will send all frames one by one
  n := FramesCount;
  if (n > 0) and
     (Owner <> nil) then
  begin
    result := false;
    FramesCount := 0;
    for i := 0 to n - 1 do
      if Owner.SendFrame(Frames[i]) then // immediate frame sending
        Frames[i].payload := ''
      else
        exit;
    Frames := nil;
  end;
  result := true;
end;

function TWebSocketProtocol.GetEncrypted: boolean;
begin
  result := (self <> nil) and
            (fEncryption <> nil);
end;

function TWebSocketProtocol.GetSubprotocols: RawUtf8;
begin
  result := fName;
end;

function TWebSocketProtocol.SetSubprotocol(const aProtocolName: RawUtf8): boolean;
begin
  result := PropNameEquals(aProtocolName, fName);
end;

function TWebSocketProtocol.GetRemoteIP: RawUtf8;
begin
  if (self = nil) or
     fRemoteLocalhost then
    result := ''
  else
    result := fRemoteIP;
end;


{ TWebSocketFrameList }

constructor TWebSocketFrameList.Create(timeoutsec: cardinal);
begin
  inherited Create;
  fTimeoutSec := timeoutsec;
end;

function TWebSocketFrameList.AnswerToIgnore(incdec: integer): integer;
begin
  if incdec < 0 then
    LockedDec32(@fAnswerToIgnore)
  else if incdec > 0 then
    LockedInc32(@fAnswerToIgnore);
  result := fAnswerToIgnore;
end;

function TWebSocketFrameList.Pop(protocol: TWebSocketProtocol;
  const head: RawUtf8; out frame: TWebSocketFrame; currentSec: cardinal): boolean;
var
  i: PtrInt;
  item: PWebSocketFrame;
begin
  result := false;
  if (self = nil) or
     (Count = 0) or
     (head = '') or
     (protocol = nil) then
    exit;
  Safe.Lock;
  try
    for i := Count - 1 downto 0 do
    begin
      item := @List[i];
      if protocol.FrameData(item^, head) <> nil then
      begin
        result := true;
        frame := item^;
        Delete(i);
        exit;
      end;
      if fTimeoutSec = 0 then
        continue;
      if currentSec = 0 then
        currentSec := GetTickSec;
      if currentSec > item^.tix then
        Delete(i);
    end;
  finally
    Safe.UnLock;
  end;
end;

procedure TWebSocketFrameList.Push(
  const frame: TWebSocketFrame; currentSec: cardinal);
var
  n: PtrInt;
begin
  if self = nil then
    exit;
  if fTimeoutSec <= 0 then
    currentSec := 0
  else if currentSec = 0 then
    currentSec := GetTickSec;
  Safe.Lock;
  try
    n := Count;
    if n >= length(List) then
      SetLength(List, NextGrow(n));
    List[n] := frame;
    if currentSec > 0 then
      List[n].tix := currentSec + fTimeoutSec;
    inc(n);
    Count := n;
  finally
    Safe.UnLock;
  end;
end;

procedure TWebSocketFrameList.PushVoidFrame(
  opcode: TWebSocketFrameOpCode; currentSec: cardinal);
var
  frame: TWebSocketFrame;
begin
  frame.opcode := opcode;
  frame.content := [];
  Push(frame, currentSec);
end;

procedure TWebSocketFrameList.Delete(i: PtrInt);
begin
  // slightly faster than a TDynArray which would release the memory
  List[i].payload := '';
  dec(Count);
  if i >= Count then
    exit;
  MoveFast(List[i + 1], List[i], (Count - i) * SizeOf(List[i]));
  pointer(List[Count].payload) := nil;
end;



{ TWebSocketProtocolRest }

procedure TWebSocketProtocolRest.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var Request: TWebSocketFrame; const Info: RawUtf8);
var
  Ctxt: THttpServerRequestAbstract;
  onRequest: TOnHttpServerRequest;
  status: cardinal;
  noAnswer: boolean;
  answer: TWebSocketFrame;
  head: RawUtf8;
begin
  if not (Request.opcode in [focText, focBinary]) then
    exit; // ignore e.g. from ProcessStart/ProcessStop
  if FrameData(Request, 'r', @head) <> nil then
  try
    // regular HTTP-like Request from client
    Ctxt := Sender.ComputeContext(onRequest);
    try
      // prepare the HTTP Request from input frame
      if (Ctxt = nil) or
         (not Assigned(onRequest)) then
        EWebSockets.RaiseUtf8('%.ProcessOne: onRequest=nil', [self]);
      if (head = '') or
         not FrameToInput(Request, noAnswer, Ctxt) then
        EWebSockets.RaiseUtf8('%.ProcessOne: invalid frame', [self]);
      Request.payload := ''; // release memory ASAP
      if fUpgradeBearerToken <> '' then
        Ctxt.AuthBearer := fUpgradeBearerToken; // re-pass the HTTP bearer
      if Info <> '' then
        Ctxt.AddInHeader(Info);  // include JUMBO_INFO[] custom header
      // compute the HTTP answer from the main HTTP server
      status := onRequest(Ctxt);
      if noAnswer or
         (Ctxt.OutContentType = NORESPONSE_CONTENT_TYPE) then
        exit; // custom non-blocking process expects no answer
      // there is a response frame to send back
      OutputToFrame(Ctxt, status, head, answer);
      if not Sender.SendFrame(answer) then // immediate frame sending
        FormatString('%.SendFrame error', [Sender], fLastError);
    finally
      Ctxt.Free;
    end;
  except
    on E: Exception do
      FormatString('% [%]', [E, E.Message], fLastError);
  end
  else if (Sender.fIncoming.AnswerToIgnore > 0) and
          (FrameData(Request, 'answer') <> nil) then
  begin
    Sender.fIncoming.AnswerToIgnore(-1);
    Sender.Log(Request, 'Ignored answer after NotifyCallback TIMEOUT', sllWarning);
  end
  else
    // e.g. async 'answer' to store in the internal incoming frames list
    Sender.fIncoming.Push(Request, 0);
end;

procedure TWebSocketProtocolRest.InputToFrame(Ctxt: THttpServerRequestAbstract;
  aNoAnswer: boolean; var request: TWebSocketFrame; out head: RawUtf8);
var
  Method, InContentType: pointer; // weak RawUtf8 reference
  seq: integer;
  p: PAnsiChar;
begin
  // by convention, defaults are POST and JSON, to reduce frame size for SOA
  Method := nil;
  if not IsPost(Ctxt.Method) then
    Method := pointer(Ctxt.Method);
  InContentType := nil;
  if (Ctxt.InContent <> '') and
     not IsContentTypeJsonU(Ctxt.InContentType) then
    InContentType := pointer(Ctxt.InContentType);
  // compute the WebSockets frame and corresponding response header
  if fSequencing then
  begin // head = rxxxxxx = safe overlap after 16,777,216 frames
    seq := InterlockedIncrement(fSequence);
    p := FastSetString(head, 7);
    p^ := 'r';
    BinToHexDisplayLower(@seq, p + 1, 3);
  end
  else
    head := 'request';
  FrameCompress(head, [RawUtf8(Method), Ctxt.Url, Ctxt.InHeaders, ord(aNoAnswer)],
    Ctxt.InContent, RawUtf8(InContentType), request);
  if fSequencing then
    // 'r000001' -> 'a000001'
    PByte(head)^ := ord('a')
  else
    head := 'answer';
end;

function TWebSocketProtocolRest.FrameToInput(var request: TWebSocketFrame;
  out aNoAnswer: boolean; Ctxt: THttpServerRequestAbstract): boolean;
var
  URL, Method, InHeaders, NoAnswer, InContentType, InContent: RawUtf8;
begin
  result := FrameDecompress(request, 'r',
    [@Method, @URL, @InHeaders, @NoAnswer], InContentType, InContent);
  if result then
  begin
    // by convention, defaults are POST and JSON, to reduce frame size for SOA
    if (InContentType = '') and
       (InContent <> '') then
      InContentType := JSON_CONTENT_TYPE_VAR;
    if Method = '' then
      Method := 'POST';
    // return the decoded WebSockets frame as a regular HTTP request
    Ctxt.PrepareDirect(
      URL, Method, InHeaders, InContent, InContentType, fRemoteIP);
    aNoAnswer := NoAnswer = '1';
  end;
end;

procedure TWebSocketProtocolRest.OutputToFrame(Ctxt: THttpServerRequestAbstract;
  Status: cardinal; var outhead: RawUtf8; var answer: TWebSocketFrame);
var
  OutContentType: pointer; // weak RawUtf8
begin
  OutContentType := nil;
  if (Ctxt.OutContent <> '') and
     not IsContentTypeJsonU(Ctxt.OutContentType) then
    OutContentType := pointer(Ctxt.OutContentType);
  if NormToUpperAnsi7[outhead[3]] = 'Q' then
    // 'reQuest' -> 'answer'
    outhead := 'answer'
  else
    // 'r000001' -> 'a000001'
    PByte(outhead)^ := ord('a');
  FrameCompress(outhead, [Status, Ctxt.OutCustomHeaders], Ctxt.OutContent,
    RawUtf8(OutContentType), answer);
end;

function TWebSocketProtocolRest.FrameToOutput(var answer: TWebSocketFrame;
  Ctxt: THttpServerRequestAbstract): cardinal;
var
  status, outHeaders, outContentType, outContent: RawUtf8;
begin
  result := HTTP_NOTFOUND;
  if not FrameDecompress(answer, 'a',
     [@status, @outHeaders], outContentType, outContent) then
    exit;
  result := GetInteger(pointer(status));
  Ctxt.OutCustomHeaders := outHeaders;
  if outContentType <> '' then
    Ctxt.OutContentType := outContentType
  else if outContent <> '' then
    Ctxt.OutContentType := JSON_CONTENT_TYPE_VAR;
  Ctxt.OutContent := outContent;
end;


{ TWebSocketProtocolJson }

constructor TWebSocketProtocolJson.Create(const aUri: RawUtf8);
begin
  inherited Create('synopsejson', aUri);
end;

function TWebSocketProtocolJson.Clone(
  const aClientUri: RawUtf8): TWebSocketProtocol;
begin
  result := TWebSocketProtocolJson.Create(fUri);
end;

procedure TWebSocketProtocolJson.FrameCompress(const Head: RawUtf8;
  const Values: array of const; const Content, ContentType: RawByteString;
  var frame: TWebSocketFrame);
var
  WR: TJsonWriter;
  tmp: TTextWriterStackBuffer; // 8KB work buffer on stack
  i: PtrInt;
begin
  frame.opcode := focText;
  frame.content := [];
  frame.tix := 0;
  WR := TJsonWriter.CreateOwnedStream(tmp);
  try
    WR.AddDirect('{');
    WR.AddFieldName(Head);
    WR.AddDirect('[');
    for i := 0 to High(Values) do
    begin
      WR.AddJsonEscapeVarRec(@Values[i]);
      WR.AddComma;
    end;
    WR.AddDirect('"');
    WR.AddString(ContentType);
    WR.AddDirect('"', ',');
    if Content = '' then
      WR.AddDirect('"', '"')
    else if (ContentType = '') or
            IsContentTypeJsonU(ContentType) then
      WR.AddString(Content)
    else if IsValidUtf8NotVoid(Content) then
      WR.AddJsonString(Content)
    else
      WR.WrBase64(pointer(Content), length(Content), {withMagic=}true);
    WR.AddDirect(']', '}');
    WR.SetText(RawUtf8(frame.payload));
  finally
    WR.Free;
  end;
end;

function CompareMemFast(P1, P2: PUtf8Char; L: PtrInt): boolean;
  {$ifdef HASINLINE} inline; {$endif}
var
  c: AnsiChar;
begin
  result := false;
  inc(P1, L); // here L is expected to be <> 0
  inc(P2, L);
  L := -L;
  repeat
    c := P1[L];
    if c <> P2[L] then
      exit;
    inc(L);
  until L = 0;
  result := true;
end;

function TWebSocketProtocolJson.FrameData(const frame: TWebSocketFrame;
  const Head: RawUtf8; HeadFound: PRawUtf8; PMax: PPByte): pointer;
var
  P, txt: PUtf8Char;
  len: PtrInt;
begin
  result := nil;
  if (length(frame.payload) < 10) or
     (frame.opcode <> focText) then
    exit;
  P := pointer(frame.payload);
  if PMax <> nil then
    PMax^ := pointer(P + length(frame.payload));
  if not NextNotSpaceCharIs(P, '{') then
    exit;
  while P^ <> '"' do
  begin
    inc(P);
    if P^ = #0 then
      exit;
  end;
  txt := P + 1;
  P := GotoEndOfJsonString(P); // here P^ should be '"' and returns ending '"'
  len := length(Head);
  if (P^ <> #0) and
     (P - txt >= len) and
     CompareMemFast(pointer(Head), txt, len) then
  begin
    result := P + 1;
    if HeadFound <> nil then
      FastSetString(HeadFound^, txt, P - txt);
  end;
end;

function TWebSocketProtocolJson.FrameDecompress(const frame: TWebSocketFrame;
  const Head: RawUtf8; const values: array of PRawUtf8;
  var contentType, content: RawUtf8): boolean;
var
  i: PtrInt;
  info: TGetJsonField;
begin
  result := false;
  info.Json := FrameData(frame, Head);
  if info.Json = nil then
    exit;
  if not NextNotSpaceCharIs(info.Json, ':') or
     not NextNotSpaceCharIs(info.Json, '[') then
    exit;
  for i := 0 to high(values) do
    info.GetJsonValue(values[i]^);
  info.GetJsonValue(contentType);
  if info.Json = nil then
    exit;
  if (contentType = '') or
     IsContentTypeJsonU(contentType) then
    GetJsonItemAsRawJson(info.Json, RawJson(content))
  else
  begin
    info.GetJsonField;
    if not Base64MagicCheckAndDecode(info.Value, info.ValueLen,
        RawByteString(content)) then // e.g. IsValidUtf8() in FrameCompress()
      FastSetString(content, info.Value, info.ValueLen);
  end;
  result := true;
end;

function TWebSocketProtocolJson.FrameType(const frame: TWebSocketFrame): TShort31;
var
  P, txt: PUtf8Char;
begin
  result := '*';
  if (length(frame.payload) < 10) or
     (frame.opcode <> focText) then
    exit;
  P := pointer(frame.payload);
  if not NextNotSpaceCharIs(P, '{') or
     not NextNotSpaceCharIs(P, '"') then
    exit;
  txt := P;
  P := GotoEndOfJsonString(P);
  SetString(result, PAnsiChar(txt), P - txt);
end;

function TWebSocketProtocolJson.SendFrames(Owner: TWebSocketProcess;
  var Frames: TWebSocketFrameDynArray;
  var FramesCount: integer): boolean;
var
  enc: array of TWebSocketFrameEncoder;
  i, len: PtrInt;
  P: PAnsiChar;
  tmp: TSynTempBuffer; // to avoid most memory allocations
begin
  if (FramesCount = 0) or
     (Owner = nil) then
  begin
    result := true;
    exit;
  end;
  if FramesCount = 1 then
  begin
    result := Owner.SendFrame(Frames[0]); // single frame sending
    FramesCount := 0;
    exit;
  end;
  SetLength(enc, FramesCount);
  len := 0;
  for i := 0 to FramesCount - 1 do
    if Frames[i].opcode <> focText then
      EWebSockets.RaiseUtf8('%.SendFrames: unexpected %',
        [self, ToText(Frames[i].opcode)])
    else
      inc(len, enc[i].Prepare(Frames[i], Owner.fMaskSentFrames));
  P := tmp.Init(len);
  try
    for i := 0 to FramesCount - 1 do
      inc(P, enc[i].Encode(Frames[i], P));
    result := Owner.SendBytes(tmp.buf, len); // directly send at once
    if (WebSocketLog <> nil) and
       (logTextFrameContent in Owner.Settings.LogDetails) then
      with WebSocketLog.Family do
        if sllTrace in Level then
          Add.LogEscape(sllTrace, 'SendFrames=%',
            [FramesCount * ord(result)], tmp.buf, len, self);
  except
    result := false;
  end;
  tmp.Done;
  FramesCount := 0;
  Frames := nil;
end;


{ TWebSocketProtocolBinary }

constructor TWebSocketProtocolBinary.Create(
  const aUri: RawUtf8; aOptions: TWebSocketProtocolBinaryOptions);
begin
  inherited Create('synopsebin', aUri);
  fOptions := aOptions;
end;

constructor TWebSocketProtocolBinary.Create(const aUri: RawUtf8;
  const aKey; aKeySize: cardinal; aOptions: TWebSocketProtocolBinaryOptions;
  aCipher: TAesAbstractClass);
begin
  Create(aUri, aOptions);
  SetEncryptKeyAes(aCipher, aKey, aKeySize);
end;

constructor TWebSocketProtocolBinary.Create(const aUri: RawUtf8;
  aServer: boolean; const aKey: RawUtf8; aSettings: PWebSocketProcessSettings;
  aOptions: TWebSocketProtocolBinaryOptions);
begin
  Create(aUri, aOptions);
  SetEncryptKey(aServer, aKey, aSettings);
end;

function TWebSocketProtocolBinary.Clone(
  const aClientUri: RawUtf8): TWebSocketProtocol;
begin
  result := TWebSocketProtocolBinary.Create(
    fUri, {dummykey=}self, 0, fOptions);
  TWebSocketProtocolBinary(result).fSequencing := fSequencing;
  if fEncryption <> nil then
    result.fEncryption := fEncryption.Clone;
end;

procedure TWebSocketProtocolBinary.FrameCompress(const Head: RawUtf8;
  const Values: array of const; const Content, ContentType: RawByteString;
  var frame: TWebSocketFrame);
var
  item: array[0..5] of TTempUtf8; // no TempRawUtf8 memory allocation
  it: PTempUtf8;
  len, i: PtrInt;
  P: PUtf8Char;
begin
  FrameInit(focBinary, Content, ContentType, frame);
  len := length(Head) + {FRAME_HEAD_SEP=} 1 +
         PtrInt(ToVarUInt32LengthWithData(length(ContentType))) +
         length(Content);
  it := @item;
  for i := 0 to high(Values) do
  begin
    VarRecToTempUtf8(@Values[i], it^);
    inc(len, ToVarUInt32LengthWithData(it^.Len));
    inc(it);
  end;
  P := AppendRawUtf8ToBuffer(FastNewRawByteString(frame.payload, len), Head);
  P^ := FRAME_HEAD_SEP;
  inc(P);
  it := @item;
  for i := 0 to high(Values) do
  begin
    P := pointer(ToVarUInt32(it^.Len, pointer(P)));
    P := AppendBufferToBuffer(P, it^.Text, it^.Len);
    inc(it);
  end;
  AppendRawUtf8ToBuffer(pointer(ToVarString(ContentType, pointer(P))), Content);
end;

function TWebSocketProtocolBinary.FrameData(const frame: TWebSocketFrame;
  const Head: RawUtf8; HeadFound: PRawUtf8; PMax: PPByte): pointer;
var
  len: PtrInt;
  P: PUtf8Char;
begin
  result := nil;
  if frame.opcode <> focBinary then
    exit;
  len := length(Head);
  if length(frame.payload) < len + 6 then
    exit;
  P := pointer(frame.payload);
  if not CompareMemFast(pointer(Head), P, len) then
    exit;
  result := PosChar(P + len, FRAME_HEAD_SEP);
  if result = nil then
    exit;
  if PMax <> nil then
    PMax^ := pointer(P + PStrLen(P - _STRLEN)^);
  if HeadFound <> nil then
    FastSetString(HeadFound^, P, PAnsiChar(result) - P);
  inc(PByte(result));
end;

function TWebSocketProtocolBinary.FrameType(const frame: TWebSocketFrame): TShort31;
var
  i: PtrInt;
begin
  if (length(frame.payload) < 10) or
     (frame.opcode <> focBinary) then
    i := 0
  else
    i := PosExChar(FRAME_HEAD_SEP, frame.payload);
  if i = 0 then
    result := '*'
  else
    SetString(result, PAnsiChar(pointer(frame.payload)), i - 1);
end;

procedure TWebSocketProtocolBinary.BeforeSendFrame(var frame: TWebSocketFrame);
var
  value: RawByteString;
  threshold: integer;
begin
  inherited BeforeSendFrame(frame);
  if frame.opcode = focBinary then
  begin
    if pboSynLzCompress in fOptions then
    begin
      if (fopAlreadyCompressed in frame.content) or
         (fRemoteLocalhost and
          (pboNoLocalHostCompress in fOptions)) then
        // localhost or compressed -> no SynLZ
        threshold := maxInt
      else
        threshold := WebSocketsBinarySynLzThreshold;
      value := AlgoSynLZ.Compress(
        pointer(frame.payload), length(frame.payload), threshold);
    end
    else
      value := frame.payload;
    if (fEncryption <> nil) and
       not (fRemoteLocalhost and
            (pboNoLocalHostEncrypt in fOptions)) then
      fEncryption.Encrypt(value, frame.payload)
    else
      frame.payload := value;
  end;
  inc(fFramesOutBytesSocket, length(frame.payload) + 2);
end;

procedure TWebSocketProtocolBinary.AfterGetFrame(var frame: TWebSocketFrame);
var
  value: RawByteString;
  res: TProtocolResult;
begin
  inc(fFramesInBytesSocket, length(frame.payload) + 2);
  if frame.opcode = focBinary then
  begin
    if (fEncryption <> nil) and
       not (fRemoteLocalhost and
            (pboNoLocalHostEncrypt in fOptions)) then
    begin
      res := fEncryption.Decrypt(frame.payload, value);
      if res <> sprSuccess then
        EWebSockets.RaiseUtf8('%.AfterGetFrame: encryption error %',
          [self, ToText(res)^]);
    end
    else
      value := frame.payload;
    if pboSynLzCompress in fOptions then
      AlgoSynLZ.Decompress(pointer(value), length(value), frame.payload)
    else
      frame.payload := value;
  end;
  inherited AfterGetFrame(frame);
end;

function TWebSocketProtocolBinary.FrameDecompress(const frame: TWebSocketFrame;
  const Head: RawUtf8; const values: array of PRawUtf8;
  var contentType, content: RawUtF8): boolean;
var
  i: PtrInt;
  P: PByte;
begin
  result := false;
  P := FrameData(frame, Head);
  if P = nil then
    exit;
  for i := 0 to high(values) do
    FromVarString(P, values[i]^);
  FromVarString(P, contentType);
  i := length(frame.payload) - (PAnsiChar(P) - pointer(frame.payload));
  if i < 0 then
    exit;
  FastSetString(content, P, i);
  result := true;
end;

function TWebSocketProtocolBinary.SendFrames(Owner: TWebSocketProcess;
  var Frames: TWebSocketFrameDynArray; var FramesCount: integer): boolean;
const
  JUMBO_HEADER: array[0..6] of AnsiChar = 'frames' + FRAME_HEAD_SEP;
var
  jumboFrame: TWebSocketFrame;
  i, len: PtrInt;
  P: PByte;
begin
  if (FramesCount = 0) or
     (Owner = nil) then
  begin
    result := true;
    exit;
  end;
  dec(FramesCount);
  if FramesCount = 0 then
  begin
    result := Owner.SendFrame(Frames[0]); // immediate frame sending
    exit;
  end;
  jumboFrame.opcode := focBinary;
  jumboFrame.content := [];
  jumboFrame.tix := 0;
  len := SizeOf(JUMBO_HEADER) + ToVarUInt32Length(FramesCount);
  for i := 0 to FramesCount do
    if Frames[i].opcode = focBinary then
      inc(len, ToVarUInt32LengthWithData(length(Frames[i].payload)))
    else
      EWebSockets.RaiseUtf8('%.SendFrames[%]: Unexpected opcode=%',
        [self, i, ord(Frames[i].opcode)]);
  P := FastNewString(len);
  pointer(jumboFrame.payload) := P;
  MoveFast(JUMBO_HEADER, P^, SizeOf(JUMBO_HEADER));
  inc(P, SizeOf(JUMBO_HEADER));
  P := ToVarUInt32(FramesCount, P); // store max
  for i := 0 to FramesCount do
  begin
    len := length(Frames[i].payload);
    P := ToVarUInt32(len, P);
    MoveFast(pointer(Frames[i].payload)^, P^, len);
    inc(P, len);
  end;
  FramesCount := 0;
  Frames := nil;
  // resulting jumboFrame will be compressed+encrypted (if needed) then sent
  result := Owner.SendFrame(jumboFrame);
end;

const
  JUMBO_INFO: array[0..2] of RawUtf8 = (
    'Sec-WebSocket-Frame: [0]',
    'Sec-WebSocket-Frame: [1]',
    '');

procedure TWebSocketProtocolBinary.ProcessIncomingFrames(
  Sender: TWebSocketProcess; P, PMax: PByte);
var
  max, i, j: integer;
  frame: TWebSocketFrame;
  tmp: ShortString;
begin
  P := FromVarUInt32Safe(P, PMax, cardinal(max));
  if P <> nil then
    for i := 0 to max do
    begin
      frame.opcode := focBinary;
      frame.content := [];
      frame.tix := 0;
      FromVarString(P, PMax, frame.payload, CP_UTF8);
      FormatShort('GetSubFrame(%/%)', [i + 1, max + 1], tmp);
      Sender.Log(frame, tmp);
      if i = 0 then
        j := 0
      else if i = max then
        j := 1
      else
        j := 2;
      inherited ProcessIncomingFrame(Sender, frame, JUMBO_INFO[j]);
    end;
end;

procedure TWebSocketProtocolBinary.ProcessIncomingFrame(
  Sender: TWebSocketProcess; var Request: TWebSocketFrame; const Info: RawUtf8);
var
  P, PMax: PByte;
begin
  P := FrameData(Request, 'frames', nil, @PMax);
  if P <> nil then
    ProcessIncomingFrames(Sender, P, PMax)
  else
    inherited ProcessIncomingFrame(Sender, Request, Info);
end;

function TWebSocketProtocolBinary.GetFramesInCompression: integer;
begin
  if (self = nil) or
     (fFramesInBytes = 0) then
    result := 100
  else if (fFramesInBytesSocket < fFramesInBytes) or
          not (pboSynLzCompress in fOptions) then
    result := 0
  else
    result := 100 - (fFramesInBytesSocket * 100) div fFramesInBytes;
end;

function TWebSocketProtocolBinary.GetFramesOutCompression: integer;
begin
  if (self = nil) or
     (fFramesOutBytes = 0) then
    result := 100
  else if (fFramesOutBytesSocket <= fFramesOutBytes) or
          not (pboSynLzCompress in fOptions) then
    result := 0
  else
    result := 100 - (fFramesOutBytesSocket * 100) div fFramesOutBytes;
end;

function TWebSocketProtocolBinary.GetSubprotocols: RawUtf8;
begin
  result := 'synopsebin, synopsebinary';
end;

function TWebSocketProtocolBinary.SetSubprotocol(const aProtocolName: RawUtf8): boolean;
begin
  result := false;
  case FindPropName(['synopsebin', 'synopsebinary'], aProtocolName) of
    0:
      fSequencing := true;
    1:
      fSequencing := false;
  else
    exit;
  end;
  result := true;
end;



{ TWebSocketProtocolUri }

constructor TWebSocketProtocolUri.Create(const aName, aPublicUri: RawUtf8;
  aExpirationMinutes: integer; aRecordTypeInfo: PRttiInfo);
begin
  // validate and compute the NewUri prefix
  if aPublicUri = '' then
    EJwtException.RaiseUtf8('%.Create uri=''''', [self]);
  fPublicUri := aPublicUri;
  AppendCharOnceToRawUtf8(fPublicUri, '/');
  // initialize the generator and associated record RTTI
  if aExpirationMinutes <> 0 then
  begin
    fGenerator := TBinaryCookieGenerator.Create('uri', aExpirationMinutes);
    fGeneratorOwned := true;
  end;
  if (aRecordTypeInfo <> nil) and
     (aRecordTypeInfo^.Kind in rkRecordTypes) then
    fRecordTypeInfo := aRecordTypeInfo;
  inherited Create(aName, '');
end;

destructor TWebSocketProtocolUri.Destroy;
begin
  inherited Destroy;
  if fGeneratorOwned then
    FreeAndNil(fGenerator);
  if fRecordData <> nil then
  begin
    if fRecordTypeInfo <> nil then
      FastRecordClear(fRecordData, fRecordTypeInfo);
    FreeMem(fRecordData);
  end;
end;

function TWebSocketProtocolUri.Clone(
  const aClientUri: RawUtf8): TWebSocketProtocol;
begin
  result := TWebSocketProtocolUriClass(ClassType).Create(
    fName, fPublicUri, 0, fRecordTypeInfo);
  TWebSocketProtocolUri(result).fGenerator := fGenerator; // reuse main instance
end;

function TWebSocketProtocolUri.ProcessHandshakeUri(
  const aClientUri: RawUtf8): boolean;
var
  bearer: RawUtf8; // quick extraction of the trailing TBinaryCookieGenerator
begin
  bearer := ParseTrailingJwt(aClientUri, {nodotcheck=}true);
  if bearer <> '' then
  begin
    if (fRecordTypeInfo <> nil) and
       (fRecordData = nil) then
      fRecordData := AllocMem(fRecordTypeInfo.RecordSize);
    fSession := fGenerator.Validate(
      bearer, fRecordData, fRecordTypeInfo, @fCreated);
  end;
  result := fSession <> 0;
end;

function TWebSocketProtocolUri.NewUri(
  out SessionID: TBinaryCookieGeneratorSessionID;
  PRecordData: pointer): RawUtf8;
begin
  SessionID := fGenerator.Generate(result, 0, PRecordData, fRecordTypeInfo);
  result := fPublicUri + result;
end;



{ TWebSocketProtocolList }

function TWebSocketProtocolList.CloneByName(
  const aProtocolName, aClientUri: RawUtf8): TWebSocketProtocol;
var
  i: PtrInt;
  u: RawUtf8;
  p: TWebSocketProtocol;
begin
  result := nil;
  if self = nil then
    exit;
  u := Split(aClientUri, '?'); // ignore parameters
  fSafe.ReadLock;
  try
    for i := 0 to length(fProtocols) - 1 do
    begin
      p := fProtocols[i];
      if ((p.fUri = '') or
          PropNameEquals(p.fUri, u)) and
         p.SetSubprotocol(aProtocolName) then
      begin
        result := p.Clone(u);
        result.fName := aProtocolName;
        exit;
      end;
    end;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TWebSocketProtocolList.CloneByUri(
  const aClientUri: RawUtf8): TWebSocketProtocol;
var
  i: PtrInt;
  u: RawUtf8;
begin
  result := nil;
  u := Split(aClientUri, '?');
  if (self = nil) or
     (u = '') then
    exit;
  fSafe.ReadLock;
  try
    for i := 0 to length(fProtocols) - 1 do
      if PropNameEquals(fProtocols[i].fUri, u) then
      begin
        result := fProtocols[i].Clone(u);
        exit;
      end;
  finally
    fSafe.ReadUnLock;
  end;
end;

function TWebSocketProtocolList.Count: integer;
begin
  if self = nil then
    result := 0
  else
    result := length(fProtocols);
end;

destructor TWebSocketProtocolList.Destroy;
begin
  ObjArrayClear(fProtocols);
  inherited;
end;

function TWebSocketProtocolList.LockedFindIndex(const aName, aUri: RawUtf8): PtrInt;
begin
  if aName <> '' then
    for result := 0 to length(fProtocols) - 1 do
      with fProtocols[result] do
        if PropNameEquals(fName, aName) and
           ((fUri = '') or
            PropNameEquals(fUri, aUri)) then
          exit;
  result := -1;
end;

function TWebSocketProtocolList.Add(aProtocol: TWebSocketProtocol): boolean;
begin
  result := false;
  if aProtocol = nil then
    exit;
  fSafe.WriteLock;
  try
    if LockedFindIndex(aProtocol.Name, aProtocol.Uri) < 0 then
    begin
      ObjArrayAdd(fProtocols, aProtocol);
      result := true;
    end;
  finally
    fSafe.WriteUnLock;
  end;
end;

function TWebSocketProtocolList.AddOnce(aProtocol: TWebSocketProtocol): boolean;
var
  i: PtrInt;
begin
  result := false;
  if aProtocol = nil then
    exit;
  fSafe.WriteLock;
  try
    i := LockedFindIndex(aProtocol.Name, aProtocol.Uri);
    if i < 0 then
    begin
      ObjArrayAdd(fProtocols, aProtocol);
      result := true;
    end
    else
    begin
      fProtocols[i].Free;
      fProtocols[i] := aProtocol;
    end;
  finally
    fSafe.WriteUnLock;
  end;
end;

function TWebSocketProtocolList.Remove(const aProtocolName, aUri: RawUtf8): boolean;
var
  i: PtrInt;
begin
  fSafe.WriteLock;
  try
    i := LockedFindIndex(aProtocolName, aUri);
    if i >= 0 then
    begin
      ObjArrayDelete(fProtocols, i);
      result := true;
    end
    else
      result := false;
  finally
    fSafe.WriteUnLock;
  end;
end;

function TWebSocketProtocolList.ServerUpgrade(
  const Http: THttpRequestContext; const RemoteIp: RawUtf8;
  ConnectionID: THttpServerConnectionID;
  ConnectionOpaque: PHttpServerConnectionOpaque;
  out Protocol: TWebSocketProtocol; out Response: RawUtf8): integer;
var
  uri, version, prot, subprot, key, extin, extout, protout: RawUtf8;
  extins: TRawUtf8DynArray;
  P: PUtf8Char;
  Digest: TSha1Digest;
begin
  // validate WebSockets protocol upgrade request
  Protocol := nil;
  result := HTTP_BADREQUEST;
  if not IsGet(Http.CommandMethod) or
     not PropNameEquals(Http.Upgrade, 'websocket') then
    exit;
  version := Http.HeaderGetValue('SEC-WEBSOCKET-VERSION');
  if GetInteger(pointer(version)) < 13 then
    exit; // we expect WebSockets protocol version 13 at least
  key := Http.HeaderGetValue('SEC-WEBSOCKET-KEY');
  if Base64ToBinLengthSafe(pointer(key), length(key)) <> 16 then
    exit; // WS nonce must be a Base64-encoded value of 16 bytes
  uri := TrimU(Http.CommandUri);
  if (uri <> '') and
     (uri[1] = '/') then
    Delete(uri, 1, 1);
  // identify the Websockets protocol
  prot := Http.HeaderGetValue('SEC-WEBSOCKET-PROTOCOL');
  P := pointer(prot);
  if P <> nil then
  begin
    repeat
      GetNextItemTrimed(P, ',', subprot);
      Protocol := CloneByName(subprot, uri);
    until (P = nil) or
          (Protocol <> nil);
    if Protocol <> nil then
      if (Protocol.Uri = '') and
         not Protocol.ProcessHandshakeUri(uri) then
      begin
        Protocol.Free;
        result := HTTP_NOTFOUND;
        exit;
      end
      else
        FormatUtf8('Sec-WebSocket-Protocol: %'#13#10, [Protocol.Name], protout);
  end
  else
    // if no protocol is specified, try to match by URI
    Protocol := CloneByUri(uri);
  if Protocol = nil then
    exit;
  // setup the raw connection context
  Protocol.fUpgradeUri := uri;
  Protocol.fUpgradeBearerToken := Http.BearerToken;
  Protocol.fConnectionID := ConnectionID;
  Protocol.fConnectionOpaque := ConnectionOpaque;
  Protocol.fRemoteIP := Http.HeaderGetValue('SEC-WEBSOCKET-REMOTEIP');
  if Protocol.RemoteIP = '' then
  begin
    Protocol.RemoteIP := RemoteIP;
    Protocol.RemoteLocalhost := (RemoteIP = '127.0.0.1') or
                                 (RemoteIPLocalHostAsVoidInServers and
                                  (RemoteIP = ''));
  end
  else
    Protocol.RemoteLocalhost := Protocol.RemoteIP = '127.0.0.1';
  // call OnUpgraded callback for request custom validation (e.g. bearer)
  if Assigned(fOnUpgraded) then
  begin
    result := fOnUpgraded(Protocol);
    if result <> HTTP_SUCCESS then
    begin
      Protocol.Free; // upgrade was refused by the callback
      exit;
    end;
  end;
  // process any additional protocol extension (e.g. TEcdheProtocol handshake)
  extin := Http.HeaderGetValue('SEC-WEBSOCKET-EXTENSIONS');
  if extin <> '' then
  begin
    CsvToRawUtf8DynArray(pointer(extin), extins, ';', true);
    if not Protocol.ProcessHandshake(extins, extout, nil) then
    begin
      Protocol.Free;
      result := HTTP_NOTACCEPTABLE;
      exit;
    end;
  end;
  // return the 101 header and switch protocols
  ComputeChallenge(key, Digest);
  if {%H-}extout <> '' then
    extout := Join(['Sec-WebSocket-Extensions: ', extout, #13#10]);
  FormatUtf8('HTTP/1.1 101 Switching Protocols'#13#10 +
             'Upgrade: websocket'#13#10 +
             'Connection: Upgrade'#13#10 +
             'Sec-WebSocket-Connection-ID: %'#13#10 +
             '%' +
             '%Sec-WebSocket-Accept: %'#13#10#13#10,
    [ConnectionID,
     protout,
     extout,
     BinToBase64Short(@Digest, SizeOf(Digest))], Response);
  result := HTTP_SUCCESS;
  // on connection upgrade, will never be back to plain HTTP/1.1
end;


{ ******************** WebSockets Client and Server Shared Process }

{ TWebSocketProcessSettings }

procedure TWebSocketProcessSettings.SetDefaults;
begin
  HeartbeatDelay := 0;
  LoopDelay := 500;
  SendDelay := 10;
  DisconnectAfterInvalidHeartbeatCount := 5;
  CallbackAcquireTimeOutMS := 5000;
  CallbackAnswerTimeOutMS := 30000;
  LogDetails := [];
  OnClientConnected := nil;
  OnClientDisconnected := nil;
  ClientAutoUpgrade := true;
  ClientRestoreCallbacks := false;
  AesSalt := 'E750ACCA-2C6F-4B0E-999B-D31C9A14EFAB';
  AesRounds := 1024;
  AesCipher := TAesFast[mCtr];
  AesBits := 128;
  EcdheCipher := efAesCtr128;
  EcdheAuth := authMutual;
  EcdheRounds := DEFAULT_ECCROUNDS;
end;

function TWebSocketProcessSettings.SetFullLog: PWebSocketProcessSettings;
begin
  LogDetails := [logHeartbeat, logTextFrameContent, logBinaryFrameContent];
  // logCallback is debug-focused and for TWebSocketAsyncServerRest only
  result := @self;
end;


{ TWebSocketProcess }

constructor TWebSocketProcess.Create(aProtocol: TWebSocketProtocol;
  aOwnerThread: TSynThread; aSettings: PWebSocketProcessSettings;
  const aProcessName: RawUtf8);
begin
  inherited Create; // may have been overriden
  fProcessName := aProcessName;
  fProtocol := aProtocol;
  fConnectionID := aProtocol.ConnectionID;
  fOwnerThread := aOwnerThread;
  fSettings := aSettings;
  fIncoming := TWebSocketFrameList.Create(30 * 60);
  fOutgoing := TWebSocketFrameList.Create(0);
  fSafeIn.Init;
  fSafeOut.Init;
  fProtocol.AfterUpgrade(self); // e.g. for TWebSocketSocketIOClientProtocol
end;

procedure TWebSocketProcess.Shutdown(waitForPong: boolean);
var
  frame: TWebSocketFrame;
  error: integer;
begin
  if self = nil then
    exit;
  fSafeOut.Lock;
  try
    if fConnectionCloseWasSent then
      exit;
    fConnectionCloseWasSent := true;
  finally
    fSafeOut.UnLock;
  end;
  LockedInc32(@fProcessCount);
  try
    if fOutgoing.Count > 0 then
      SendPendingOutgoingFrames;
    fState := wpsClose; // the connection is inactive from now on
    // send and acknowledge a focConnectionClose frame to notify the other end
    frame.opcode := focConnectionClose;
    frame.content := [];
    frame.tix := 0;
    error := 0;
    if not SendFrame(frame) then // immediate frame sending + wait 1s for ACK
       WebSocketLog.Add.Log(sllWarning,
         'Destroy: no focConnectionClose SendFrame', self)
    else if waitForPong then
      if not CanGetFrame(1000, @error) or
         not GetFrame(frame, {blocking=}false, @error) then
        WebSocketLog.Add.Log(sllWarning,
          'Destroy: no focConnectionClose ACK %', [error], self);
  finally
    LockedDec32(@fProcessCount);
  end;
end;

destructor TWebSocketProcess.Destroy;
var
  endtix: cardinal;
  log: ISynLog;
begin
  if fState = wpsCreate then
    fProcessEnded := true
  else if not fConnectionCloseWasSent then
  begin
    if log = nil then
      WebSocketLog.EnterLocal(log, 'Destroy %', [ToText(fState)^], self);
    if log <> nil then
      log.Log(sllTrace, 'Destroy: send focConnectionClose', self);
    Shutdown({waitforpong=}true);
  end;
  fState := wpsDestroy;
  if (fProcessCount > 0) or
     not fProcessEnded then
  begin
    if log = nil then
      WebSocketLog.EnterLocal(log, 'Destroy %', [ToText(fState)^], self);
    if log <> nil then
      log.Log(sllDebug, 'Destroy: wait for fProcessCount=% fProcessEnded=%',
        [fProcessCount, fProcessEnded], self);
    endtix := GetTickSec + 5;
    repeat
      SleepHiRes(1);
    until ((fProcessCount = 0) and fProcessEnded) or
          (GetTickSec > endtix);
    if log <> nil then
      log.Log(sllDebug,
        'Destroy: waited fProcessCount=%', [fProcessCount], self);
  end;
  FreeAndNil(fProtocol);
  fOutgoing.Free;
  fIncoming.Free;
  fSafeIn.Done; // to be done lately to avoid GPF
  fSafeOut.Done;
  inherited Destroy;
end;

procedure TWebSocketProcess.Reset(aConnectionID: THttpServerConnectionID);
begin
  fConnectionID := 0;
  fState := wpsCreate;
  fProcessEnded := false;
  fProtocol.Reset;
end;

procedure TWebSocketProcess.ProcessStart;
var
  frame: TWebSocketFrame; // notify e.g. TOnWebSocketProtocolChatIncomingFrame
begin
  if Assigned(fSettings.OnClientConnected) then
  try
    WebSocketLog.Add.Log(sllTrace, 'ProcessStart: OnClientConnected', self);
    fSettings.OnClientConnected(Self);
  except
  end;
  //WebSocketLog.Add.Log(sllTrace, 'ProcessStart: callbacks', self);
  frame.opcode := focContinuation;
  frame.content := [];
  frame.tix := 0;
  if (not Assigned(fProtocol.fOnBeforeIncomingFrame)) or
     (not fProtocol.fOnBeforeIncomingFrame(self, frame)) then
    fProtocol.ProcessIncomingFrame(self, frame, ''); // any exception would abort
  WebSocketLog.Add.Log(sllDebug, 'ProcessStart %', [fProtocol], self);
end;

procedure TWebSocketProcess.ProcessStop;
var
  frame: TWebSocketFrame; // notify e.g. TOnWebSocketProtocolChatIncomingFrame
begin
  try
    WebSocketLog.Add.Log(sllTrace, 'ProcessStop: callbacks', self);
    frame.opcode := focConnectionClose;
    frame.content := [];
    frame.tix := 0;
    if Assigned(fProtocol) then
      if (not Assigned(fProtocol.fOnBeforeIncomingFrame)) or
         (not fProtocol.fOnBeforeIncomingFrame(self, frame)) then
        fProtocol.ProcessIncomingFrame(self, frame, '');
    if Assigned(fSettings.OnClientDisconnected) then
    begin
      WebSocketLog.Add.Log(sllTrace, 'ProcessStop: OnClientDisconnected', self);
      fSettings.OnClientDisconnected(Self);
    end;
  except // exceptions are just ignored at shutdown
  end;
  fProcessEnded := true;
  WebSocketLog.Add.Log(sllDebug, 'ProcessStop %', [fProtocol], self);
end;

procedure TWebSocketProcess.MarkAsInvalid;
begin
  inc(fInvalidPingSendCount);
  fSafeOut.Lock;
  fConnectionCloseWasSent := true;
  fSafeOut.UnLock;
end;

procedure TWebSocketProcess.SetLastPingTicks;
begin
  if fNoLastSocketTicks then
    EWebSockets.RaiseUtf8('Unexpected %.LastPingDelay', [self]);
  fLastSocketTicks := GetTickCount64;
  fInvalidPingSendCount := 0;
end;

function TWebSocketProcess.LastPingDelay: Int64;
begin
  if fNoLastSocketTicks then
    EWebSockets.RaiseUtf8('Unexpected %.LastPingDelay', [self]);
  result := GetTickCount64 - fLastSocketTicks;
end;

procedure TWebSocketProcess.ProcessLoopReceived(var request: TWebSocketFrame);
begin
  case request.opcode of
    focPing:
      begin
        request.opcode := focPong;
        SendFrame(request); // immediate pong frame sending
      end;
    focPong:
      ; // nothing to do
    focText,
    focBinary:
      if Assigned(fProtocol) then
        if (not Assigned(fProtocol.fOnBeforeIncomingFrame)) or
           (not fProtocol.fOnBeforeIncomingFrame(self, request)) then
          fProtocol.ProcessIncomingFrame(self, request, '');
    focConnectionClose:
      begin
        if (fState = wpsRun) and
           not fConnectionCloseWasSent then
        begin
          fState := wpsClose; // will close the connection
          SendFrame(request); // immediate send back the frame as ACK
        end;
      end;
  end;
  request.payload := ''; // release memory ASAP
end;

function TWebSocketProcess.ProcessLoopStepReceive(FrameProcessed: PBoolean): boolean;
var
  request: TWebSocketFrame;
  error: integer;
begin
  if FrameProcessed <> nil then
    FrameProcessed^ := false;
  if fState = wpsRun then
  begin
    LockedInc32(@fProcessCount); // flag currently processing
    try
      if CanGetFrame({timeout=}1, @error) and
         GetFrame(request, {blocking=}FrameProcessed = nil, @error) then
      begin
        // we received a full frame
        if FrameProcessed <> nil then
          FrameProcessed^ := true;
        ProcessLoopReceived(request);
      end
      else if (fOwnerThread <> nil) and
              fOwnerThread.Terminated then
        fState := wpsClose
      else if error <> 0 then
      begin
        WebSocketLog.Add.Log(sllInfo, 'GetFrame error % on %',
          [error, fProtocol], self);
        fState := wpsClose;
      end;
    finally
      LockedDec32(@fProcessCount); // release flag
    end;
  end;
  result := (fState = wpsRun);
end;

procedure TWebSocketProcess.SendPing;
var
  request: TWebSocketFrame;
begin
  if (self = nil) or
     (fState <> wpsRun) then
    exit;
  LockedInc32(@fProcessCount); // flag currently processing
  try
    request.opcode := focPing;
    request.content := [];
    request.tix := 0;
    if not SendFrame(request) then // immediate frame sending
      if (fSettings.DisconnectAfterInvalidHeartbeatCount <> 0) and
         (fInvalidPingSendCount >=
           fSettings.DisconnectAfterInvalidHeartbeatCount) then
        fState := wpsClose
      else
        MarkAsInvalid;
  finally
    LockedDec32(@fProcessCount); // release flag
  end;
end;

function TWebSocketProcess.ProcessLoopStepSend: boolean;
var
  elapsed: cardinal;
begin
  if fState = wpsRun then
  begin
    LockedInc32(@fProcessCount); // flag currently processing
    try
      elapsed := LastPingDelay;
      if elapsed > fSettings.SendDelay then
        if (fOutgoing.Count > 0) and
           (SendPendingOutgoingFrames < 0) then
          fState := wpsClose // SendFrames() failed
        else if (fSettings.HeartbeatDelay <> 0) and
                (elapsed > fSettings.HeartbeatDelay) then
          SendPing;
    finally
      LockedDec32(@fProcessCount); // release flag
    end;
  end;
  result := (fState = wpsRun);
end;

procedure TWebSocketProcess.ProcessLoop;
begin
  if fProtocol = nil then
    exit;
  try
    ProcessStart; // any exception will close the socket
    try
      SetLastPingTicks;
      fState := wpsRun;
      while (fOwnerThread = nil) or
            (fProtocol = nil) or
            not fOwnerThread.Terminated do
        if ProcessLoopStepReceive({nonblockingflag=}nil) and
           ProcessLoopStepSend then
          HiResDelay(fLastSocketTicks) // 0/1/5/50/120-250 ms steps
        else
          break; // connection ended
    finally
      ProcessStop;
    end;
  except // don't be optimistic: abort and close connection
    fState := wpsClose;
  end;
end;

procedure TWebSocketProcess.WaitThreadStarted;
var
  endtix: cardinal;
begin
  endtix := GetTickSec + 5;
  repeat
    SleepHiRes(0);
  until fProcessEnded or
        (fState <> wpsCreate) or
        (GetTickSec > endtix);
end;

function TWebSocketProcess.HiResDelay(var start: Int64): Int64;
var
  delay: cardinal;
begin
  delay := SleepStepTime(start, result); // efficient 0/1/5/50/120-250 ms steps
  if (fSettings.LoopDelay <> 0) and
     (delay > fSettings.LoopDelay) then
    delay := fSettings.LoopDelay;
  SleepHiRes(delay);
end;

function TWebSocketProcess.State: TWebSocketProcessState;
begin
  if self = nil then
    result := wpsCreate
  else
    result := fState;
end;

function TWebSocketProcess.RemoteIP: RawUtf8;
begin
  if (self = nil) or
     (fProtocol = nil) or
     fProtocol.fRemoteLocalhost then
    result := ''
  else
    result := fProtocol.fRemoteIP;
end;

function TWebSocketProcess.NotifyCallback(aRequest: THttpServerRequestAbstract;
  aMode: TWebSocketProcessNotifyCallback): cardinal;
var
  request, answer: TWebSocketFrame;
  i: integer;
  start, max, tix: Int64;
  head: RawUtf8;
begin
  result := HTTP_NOTFOUND;
  if (fProtocol = nil) or
     (aRequest = nil) or
     not fProtocol.InheritsFrom(TWebSocketProtocolRest) then
    exit;
  if WebSocketLog <> nil then
    WebSocketLog.Add.Log(sllTrace, 'NotifyCallback(%,%)',
      [aRequest.Url, _TWebSocketProcessNotifyCallback[aMode]^], self);
  TWebSocketProtocolRest(fProtocol).InputToFrame(aRequest,
    aMode in [wscBlockWithoutAnswer, wscNonBlockWithoutAnswer], request, head);
  case aMode of
    wscNonBlockWithoutAnswer:
      begin
        // add to the internal sending list for asynchronous sending
        SendFrameAsync(request); // with potential jumboframes gathering
        result := HTTP_SUCCESS;
        exit;
      end;
    wscBlockWithAnswer:
      // need to block until all previous answers are received
      if fIncoming.AnswerToIgnore > 0 then
      begin
        WebSocketLog.Add.Log(sllDebug,
          'NotifyCallback: Waiting for AnswerToIgnore=%',
          [fIncoming.AnswerToIgnore], self);
        start := GetTickCount64; // HiResDelay() requires ms resolution
        max := start + 30000; // never wait forever
        repeat
          tix := HiResDelay(start); // 0/1/5/50/120-250 ms steps
          if fState in [wpsDestroy, wpsClose] then
          begin
            WebSocketLog.Add.Log(sllError,
              'NotifyCallback wait previous on closed connection', self);
            exit;
          end;
          if fIncoming.AnswerToIgnore = 0 then
            break; // it is now safe to send a new 'request'
          if tix < max then
            continue;
          self.Log(request, 'NotifyCallback AnswerToIgnore TIMEOUT -> ' +
            'abort connection', sllInfo);
          result := HTTP_CLIENTERROR; // to force recreate connection
          exit;
        until false;
      end;
  end;
  // wscBlockWithoutAnswer or wscBlockWithAnswer
  i := InterlockedIncrement(fProcessCount);
  try
    if (i > 2) and
       (WebSocketLog <> nil) then
      WebSocketLog.Add.Log(sllWarning,
        'NotifyCallback with fProcessCount=%', [i], self);
    if not SendFrame(request) then // immediate frame sending
      exit;
    if aMode = wscBlockWithoutAnswer then
    begin
      result := HTTP_SUCCESS;
      exit;
    end;
    tix := GetTickCount64;
    start := tix;
    max := fSettings.CallbackAnswerTimeOutMS;
    if max = 0 then
      // never wait for ever: 30 seconds is the absolute maximum delay
      max := 30000
    else if max < 2000 then
      // 2 seconds minimal wait
      max := 2000;
    inc(max, start);
    while not fIncoming.Pop(fProtocol, head, answer, tix div MilliSecsPerSec) do
      if fState in [wpsDestroy, wpsClose] then
      begin
        WebSocketLog.Add.Log(sllError,
          'NotifyCallback answer abort on closed connection', self);
        exit;
      end
      else if tix > max then
      begin
        WebSocketLog.Add.Log(sllWarning,
          'NotifyCallback TIMEOUT %', [head], self);
        if head = 'answer' then
          fIncoming.AnswerToIgnore(1); // ignore next 'answer'
        exit; // returns HTTP_NOTFOUND
      end
      else
        tix := HiResDelay(start); // 0/1/5/50/120-250 ms steps
  finally
    LockedDec32(@fProcessCount);
  end;
  result := TWebSocketProtocolRest(fProtocol).FrameToOutput(answer, aRequest);
end;

function TWebSocketProcess.SendPendingOutgoingFrames: integer;
begin
  fOutgoing.Safe.Lock;
  try
    result := fOutgoing.Count;
    if not fProtocol.SendFrames(self, fOutgoing.List, fOutgoing.Count) then
    begin
      WebSocketLog.Add.Log(sllInfo,
        'SendPendingOutgoingFrames: SendFrames failed', self);
      result := -1; // indicates error
    end;
  finally
    fOutgoing.Safe.UnLock;
  end;
end;

procedure TWebSocketProcess.Log(const frame: TWebSocketFrame;
  const aMethodName: ShortString; aEvent: TSynLogLevel; DisableRemoteLog: boolean);

  procedure DoLog(log: TSynLog);
  var
    tmp: TLogEscape; // 512 bytes of temp buffer
    len: integer;
  begin
    log.DisableRemoteLog(DisableRemoteLog);
    try
      if (frame.opcode = focText) and
         (logTextFrameContent in fSettings.LogDetails) then
        log.Log(aEvent, '% % % focText %',
          [aMethodName, fProtocol.GetRemoteIP,
           fProtocol.FrameType(frame), frame.PayLoad], self)
      else
      begin
        len := length(frame.PayLoad);
        log.Log(aEvent, '% % % % len=%%',
         [aMethodName, fProtocol.GetRemoteIP, fProtocol.FrameType(frame),
          _TWebSocketFrameOpCode[frame.opcode]^, len,
          LogEscape(pointer(frame.PayLoad), len, tmp,
            logBinaryFrameContent in fSettings.LogDetails)], self);
      end;
    finally
      log.DisableRemoteLog(false);
    end;
  end;

begin
  if WebSocketLog <> nil then
    with WebSocketLog.Family do
      if aEvent in Level then
        if (logHeartbeat in fSettings.LogDetails) or
           not (frame.opcode in [focPing, focPong]) then
          DoLog(Add);
end;

function TWebSocketProcess.GetFrame(out Frame: TWebSocketFrame;
  Blocking: boolean; ErrorWithoutException: PInteger): boolean;
var
  f: TWebProcessInFrame;
begin
  f.Init(self, @Frame);
  fSafeIn.Lock;
  try
    if Blocking then
      repeat
        // blocking processing loop to perform all steps
      until f.Step(ErrorWithoutException) in [pfsDone, pfsError]
    else
      // not blocking process
      f.Step(ErrorWithoutException);
    result := f.state = pfsDone;
  finally
    fSafeIn.UnLock;
  end;
end;

function TWebSocketProcess.SendFrame(var Frame: TWebSocketFrame): boolean;
var
  tmp: TSynTempBuffer;
begin
  fSafeOut.Lock;
  try
    Log(Frame, 'SendFrame', sllTrace, true);
    try
      if Frame.opcode = focConnectionClose then
        fConnectionCloseWasSent := true; // to be done once on each end
      if (fProtocol <> nil) and
         (Frame.payload <> '') then
        fProtocol.BeforeSendFrame(Frame); // may encrypt/compress in-place
      FrameSendEncode(Frame, fMaskSentFrames, tmp);
      try
        result := SendBytes(tmp.buf, tmp.len); // immediate frame sending
      finally
        tmp.Done;
      end;
    except
      result := false;
    end;
    if not result then
      MarkAsInvalid
    else if not fNoLastSocketTicks then
      SetLastPingTicks;
  finally
    fSafeOut.UnLock;
  end;
end;

procedure TWebSocketProcess.SendFrameAsync(const Frame: TWebSocketFrame);
begin
  fOutgoing.Push(frame, 0);
end;


{ TWebCrtSocketProcess }

constructor TWebCrtSocketProcess.Create(aSocket: TCrtSocket;
  aProtocol: TWebSocketProtocol; aOwnerThread: TSynThread;
  aSettings: PWebSocketProcessSettings; const aProcessName: RawUtf8);
begin
  inherited Create(aProtocol, aOwnerThread, aSettings, aProcessName);
  fSocket := aSocket;
end;

function TWebCrtSocketProcess.CanGetFrame(TimeOut: cardinal;
  ErrorWithoutException: PInteger): boolean;
var
  pending: integer;
begin
  if ErrorWithoutException <> nil then
    ErrorWithoutException^ := 0;
  pending := fSocket.SockInPending(TimeOut);
  if pending < 0 then // socket error
    if ErrorWithoutException <> nil then
    begin
      ErrorWithoutException^ := pending; // -1 or -2
      result := false;
      exit;
    end
    else
      EWebSockets.RaiseUtf8('SockInPending() Error % on %:% - from %',
        [pending, fSocket.Server, fSocket.Port, fProtocol.fRemoteIP]);
  result := (pending > 0); // assume if we got 1 byte, we are likely to have two
end;

function TWebCrtSocketProcess.ReceiveBytes(P: PAnsiChar; count: PtrInt): integer;
begin
  result := fSocket.SockInRead(P, count, {useonlysockin=}false);
end;

function TWebCrtSocketProcess.SendBytes(P: pointer; Len: PtrInt): boolean;
begin
  result := fSocket.TrySndLow(P, Len);
end;


{ ******************** WebSockets Asynchronous Frames Parsing }

{ TWebProcessInFrame }

procedure TWebProcessInFrame.Init(owner: TWebSocketProcess; output: PWebSocketFrame);
begin
  process := owner;
  outputframe := output;
  state := pfsHeader1;
  len := 0;
end;

function TWebProcessInFrame.HasBytes(P: PAnsiChar; count: integer): boolean;
begin
  if len > count then
    // we already got that much input data
    result := true
  else
  begin
    // TWebCrtSocketProcess SockInRead() would raise a ENetSock error on failure
    inc(len, process.ReceiveBytes(P + len, count - len));
    result := len = count;
  end;
end;

function TWebProcessInFrame.GetHeader: boolean;
begin
  result := false;
  if len = 0 then
  begin
    data := '';
    FillCharFast(hdr, SizeOf(hdr), 0);
  end;
  if not HasBytes(@hdr, 2) then // first+len8
    exit;
  opcode := TWebSocketFrameOpCode(hdr.first and 15);
  masked := hdr.len8 and FRAME_LEN_MASK <> 0;
  if masked then
    hdr.len8 := hdr.len8 and (FRAME_LEN_MASK - 1);
  if hdr.len8 < FRAME_LEN_2BYTES then
    hdr.len32 := hdr.len8
  else if hdr.len8 = FRAME_LEN_2BYTES then
  begin
    if not HasBytes(@hdr, 4) then // first+len8+len32.low
      exit;
    hdr.len32 := bswap16(hdr.len32);
  end
  else if hdr.len8 = FRAME_LEN_8BYTES then
  begin
    if not HasBytes(@hdr, 10) then // first+len8+len32+len64.low
      exit;
    if hdr.len32 <> 0 then // size is more than 32 bits (4GB) -> reject
      hdr.len32 := maxInt
    else
      hdr.len32 := bswap32(hdr.len64);
    if hdr.len32 > WebSocketsMaxFrameMB shl 20 then
      EWebSockets.RaiseUtf8('%.GetFrame: length = % should be < % MB',
        [process, KB(hdr.len32), WebSocketsMaxFrameMB]);
  end;
  if masked then
  begin
    len := 0; // not appended to hdr
    if not HasBytes(@hdr.mask, 4) then
      EWebSockets.RaiseUtf8('%.GetFrame: truncated mask', [process]);
  end;
  len := 0; // prepare upcoming GetData
  result := true;
end;

function TWebProcessInFrame.GetData: boolean;
begin
  if length(data) <> integer(hdr.len32) then
    FastNewRawByteString(data, hdr.len32);
  result := HasBytes(pointer(data), hdr.len32);
  if result then
  begin
    if hdr.mask <> 0 then
      // client-to-server masking is mandatory (but not from server to client)
      ProcessMask(pointer(data), hdr.mask, hdr.len32);
    len := 0; // prepare next upcoming GetHeader
  end;
end;

function TWebProcessInFrame.Step(ErrorWithoutException: PInteger): TWebProcessInFrameState;
begin
  while true do // process as much incoming data as possible
    case state of
      pfsHeader1:
        if GetHeader then
        begin
          outputframe.opcode := opcode;
          outputframe.content := [];
          state := pfsData1;
        end
        else
          break; // quit when not enough data is available from input
      pfsData1:
        if GetData then
        begin
          outputframe.payload := data;
          if hdr.first and FRAME_OPCODE_FIN = 0 then
            state := pfsHeaderN
          else
            state := pfsDone;
        end
        else
          break; // not enough input yet
      pfsHeaderN:
        if GetHeader then
          if (opcode <> focContinuation) and
             (opcode <> outputframe.opcode) then
          begin
            state := pfsError; // logic error in the websockets state machine
            if ErrorWithoutException <> nil then
            begin
              WebSocketLog.Add.Log(sllDebug, 'GetFrame: received %, expected %',
                [_TWebSocketFrameOpCode[opcode]^,
                 _TWebSocketFrameOpCode[outputframe.opcode]^], process);
              break;
            end
            else
              EWebSockets.RaiseUtf8('%.GetFrame: received %, expected %',
                [process, _TWebSocketFrameOpCode[opcode]^,
                 _TWebSocketFrameOpCode[outputframe.opcode]^])
          end
          else
            state := pfsDataN
        else
          break; // not enough input yet
      pfsDataN:
        if GetData then
        begin
          outputframe.payload := outputframe.payload + data;
          if hdr.first and FRAME_OPCODE_FIN = 0 then
            state := pfsHeaderN
          else
            state := pfsDone;
        end
        else
          break; // not enough input yet
      pfsDone:
        begin
          data := '';
          if opcode = focText then
            // identify text content as UTF-8 - is likely to be JSON anyway
            FakeCodePage(outputframe.payload, CP_UTF8);
          if (process.fProtocol <> nil) and
             (outputframe.payload <> '') then
            process.fProtocol.AfterGetFrame(outputframe^);
          process.Log(outputframe^, 'GetFrame');
          if not process.fNoLastSocketTicks then
            process.SetLastPingTicks;
          break;
        end;
    else
      break; // e.g. pfsError
    end;
  if state = pfsError then
    if (ErrorWithoutException <> nil) and
       (ErrorWithoutException^ = 0) then
      ErrorWithoutException^ := -3; // logic error should close the socket
  result := state;
end;


{ ******************** TWebSocketProtocolChat Simple Protocol }

{ TWebSocketProtocolChat }

constructor TWebSocketProtocolChat.Create(const aName, aUri: RawUtf8;
  const aOnIncomingFrame: TOnWebSocketProtocolChatIncomingFrame);
begin
  inherited Create(aName, aUri);
  fOnIncomingFrame := aOnIncomingFrame;
end;

function TWebSocketProtocolChat.Clone(const aClientUri: RawUtf8): TWebSocketProtocol;
begin
  result := TWebSocketProtocolChat.Create(fName, fUri);
  if fEncryption <> nil then
    TWebSocketProtocolChat(result).fEncryption := fEncryption.Clone;
  TWebSocketProtocolChat(result).OnIncomingFrame := OnIncomingFrame;
end;

procedure TWebSocketProtocolChat.ProcessIncomingFrame(Sender: TWebSocketProcess;
  var Request: TWebSocketFrame; const Info: RawUtf8);
begin
  if Assigned(OnInComingFrame) then
  try
    OnIncomingFrame(Sender, Request);
  except
    // ignore any exception in the callback
  end;
end;

function TWebSocketProtocolChat.SendFrame(Sender: TWebSocketProcess;
  const frame: TWebSocketFrame): boolean;
var
  tmp: TWebSocketFrame; // SendFrame() may change frame content (e.g. mask)
begin
  result := false;
  if (self = nil) or
     (Sender = nil) or
     (Sender.State <> wpsRun) or
     not (frame.opcode in [focText, focBinary])  then
    exit;
  tmp.opcode := frame.opcode;
  tmp.content := frame.content;
  FastSetRawByteString(tmp.payload, pointer(frame.payload), length(frame.payload));
  result := Sender.SendFrame(tmp)
end;

function TWebSocketProtocolChat.SendFrameJson(Sender: TWebSocketProcess;
  const Json: RawUtf8): boolean;
var
  frame: TWebSocketFrame;
begin
  result := false;
  if (self = nil) or
     (Sender = nil) or
     (Sender.State <> wpsRun) then
    exit;
  frame.opcode := focText;
  frame.content := [];
  frame.tix := 0;
  FastSetRawByteString(frame.payload, pointer(Json), length(Json)); // temp copy
  result := Sender.SendFrame(frame)
end;


{ ****************** Socket.IO / Engine.IO Raw Protocols }

// reference: https://sockjs.com/docs/v4/socket-io-protocol/

const
  /// the default Socket.IO remote name space if none ('') is supplied
  DefaultSocketIONameSpace: RawUtf8 = '/';

function ToText(p: TEngineIOPacket): PShortString;
begin
  result := GetEnumName(TypeInfo(TEngineIOPacket), ord(p));
end;

function ToText(p: TSocketIOPacket): PShortString;
begin
  result := GetEnumName(TypeInfo(TSocketIOPacket), ord(p));
end;



{ TSocketIOMessage }

function TSocketIOMessage.InitBuffer(PayLoad: PUtf8Char; PayLoadLen: PtrInt;
  PayLoadBinary: boolean; Process: TWebSocketProcess): boolean;
var
  v: PtrUInt;
begin
  result := false;
  if (PayLoad = nil) or
     (PayLoadLen = 0) then
    exit;
  fPacketType := TSocketIOPacket(PByte(PayLoad)^ - ord('0'));
  if byte(fPacketType) > byte(high(fPacketType)) then
    exit;
  fSender := Process;
  fNameSpaceLen := 1; // '/' by default (if not specified)
  fNameSpace := pointer(DefaultSocketIONameSpace);
  fID := 0;
  fBinaryAttachment := 0;
  inc(PayLoad);
  dec(PayLoadLen);
  if PayLoadLen <> 0 then
  begin
    if PayLoad^ in ['1' ..'9'] then
    begin
      v := 0;
      repeat
        v := (v * 10) + PtrUInt(ord(PayLoad^) - ord('0'));
        inc(PayLoad);
        dec(PayLoadLen);
      until (PayLoadLen = 0) or
            not (PayLoad^ in ['0'..'9']);
      if PayLoad^ = '-' then
      begin
        fBinaryAttachment := v;
        inc(PayLoad);
        dec(PayLoadLen);
      end
      else
        fID := v;
    end;
    if (PayLoadLen <> 0) and
       (PayLoad^ = '/') then
    begin
      fNameSpace := PayLoad;
      repeat
        inc(PayLoad);
        dec(PayLoadLen);
      until (PayLoadLen = 0) or
            (PayLoad^ = ',');
      fNameSpacelen := PayLoad - fNameSpace;
      if (PayLoadLen <> 0) and
         (PayLoad^ = ',') then
      begin
        inc(PayLoad);
        dec(PayLoadLen);
      end;
    end;
    if fID = 0 then
      while (PayLoadLen <> 0) and
            (PayLoad^ in ['0'..'9']) do
      begin
        fID := (fID * 10) + (ord(PayLoad^) - ord('0'));
        inc(PayLoad);
        dec(PayLoadLen);
      end;
  end;
  if PayLoadLen = 0 then
    PayLoad := nil;
  fData := PayLoad;
  fDataLen := PayLoadLen;
  fDataBinary := PayLoadBinary;
  result := true;
end;

function TSocketIOMessage.Init(const PayLoad: RawUtf8): boolean;
begin
  result := InitBuffer(pointer(PayLoad), length(PayLoad), {binary=}false, nil);
end;

function TSocketIOMessage.NameSpaceIs(const Name: RawUtf8): boolean;
begin
  result := (length(Name) = fNameSpaceLen) and
            ((fNameSpaceLen = 0) or
             CompareMemFast(pointer(Name), fNameSpace, fNameSpaceLen));
end;

procedure TSocketIOMessage.NameSpaceGet(out Dest: RawUtf8);
begin
  FastSetString(Dest, fNameSpace, fNameSpaceLen);
end;

function TSocketIOMessage.NameSpaceShort: ShortString;
begin
  SetString(result, PAnsiChar(fNameSpace), fNameSpaceLen);
end;

function TSocketIOMessage.DataIs(const Content: RawUtf8): boolean;
begin
  result := (length(Content) = fDataLen) and
            ((fDataLen = 0) or
             CompareMemFast(pointer(Content), fData, fDataLen));
end;

function TSocketIOMessage.DataGet(out Dest: TDocVariantData;
  Options: PDocVariantOptions): boolean;
begin
  if Options = nil then
    Options := @JSON_SOCKETIO;
  result := Dest.InitJsonInPlace(fData, Options^) <> nil;
end;

procedure TSocketIOMessage.RaiseESockIO(const ctx: RawUtf8);
begin
  ESocketIO.RaiseUtf8('% NameSpace=% Data=%', [ctx, NameSpaceShort, fData]);
end;


{ TSocketIOLocalNamespace }

constructor TSocketIOLocalNamespace.Create(aOwner: TEngineIOAbstract;
  const aNamespace: RawUtf8);
begin
  inherited Create;
  fOwner := aOwner;
  fNameSpace := aNamespace;
  fHandlers.InitSpecific(
    TypeInfo(TLocalNamespaceEventHandlers), fHandler, ptRawUtf8, nil, false);
  RegisterHandlers;
end;

procedure TSocketIOLocalNamespace.RegisterHandlers;
begin
  // nothing to register by default
end;

function TSocketIOLocalNamespace.RegisterEvent(const aEventName: RawUtf8;
  const aCallback: TOnSocketIOEvent): TSocketIOLocalNamespace;
begin
  PEventHandler(fHandlers.AddUniqueName(aEventName,
     'Duplicated event name %', [aEventName]))^.OnEvent := aCallback;
  result := self;
end;

procedure TSocketIOLocalNamespace.RegisterPublishedMethods(aInstance: TObject);
var
  met: TPublishedMethodInfoDynArray;
  m: PtrInt;
begin
  for m := 0 to GetPublishedMethods(aInstance, met) - 1 do
    PEventHandler(fHandlers.AddUniqueName(met[m].Name,
       'Duplicated event name % on %', [met[m].Name, aInstance]))^.
      OnMethod := TOnSocketIOMethod(met[m].Method);
end;

procedure TSocketIOLocalNamespace.RegisterFrom(aAnother: TSocketIOLocalNamespace);
var
  i: integer;
  s: PEventHandler;
begin
  if aAnother = nil then
    exit;
  s := pointer(aAnother.fHandler);
  for i := 1 to length(aAnother.fHandler) do
  begin
    PEventHandler(fHandlers.AddUniqueName(s^.Name))^ := s^;
    inc(s);
  end;
end;

procedure TSocketIOLocalNamespace.HandleEvent(const aMessage: TSocketIOMessage;
  aIgnoreUnknownEvent: boolean);
var
  ndx: PtrInt;
  event, ack: RawUtf8;
  data: TDocVariantData;
  d: PDocVariantData;
begin
  // validate input context (paranoid checks)
  if (fNameSpace <> '*') and
     not aMessage.NameSpaceIs(fNameSpace) then
    ESocketIO.RaiseUtf8('%.HandleEvent: unexpected namespace ([%]<>[%])',
      [self, aMessage.NameSpaceShort, fNameSpace]);
  if aMessage.PacketType <> sioEvent then
    ESocketIO.RaiseUtf8('%.HandleEvent: unexpected % message for namespace %',
      [self, ToText(aMessage.PacketType)^, fNameSpace]);
  // decode the input JSON array
  if not aMessage.DataGet(data) or
     not data.IsArray or
     (data.Count = 0) then
    if snoIgnoreIncorrectData in fOptions then
      exit // ignore in silence
    else
      ESocketIO.RaiseUtf8('%.HandleEvent: message is not a JSON array', [self]);
  VariantToUtf8(data.Values[0], event);
  data.Delete(0); // trim the event name from the data array
  d := @data;
  if (d^.Count = 1) and
     _Safe(d^.Values[0])^.IsObject then
    d := _Safe(d^.Values[0]); // return a single object as root (common case)
  // optional callback
  if Assigned(OnEventReceived) then
    OnEventReceived(self, event, d^);
  // retrieve event name and search for associated handler
  ndx := fHandlers.FindHashed(event);
  if ndx < 0 then
    if aIgnoreUnknownEvent or
       (snoIgnoreUnknownEvent in fOptions) then
      exit // ignore in silence
    else
      ESocketIO.RaiseUtf8('%.HandleEvent: unknown event % for namespace %',
        [event, fNameSpace]);
  // call the handler
  with fHandler[ndx] do
    if Assigned(OnEvent) then
      ack := OnEvent(self, event, d^)
    else if Assigned(OnMethod) then
      ack := OnMethod(d^);
  // optionally call back the server with an ACK payload
  if (ack <> '') and
     (aMessage.ID <> SIO_NO_ACK) then
    SocketIOSendPacket(fOwner.fWebSockets, sioAck, fNameSpace,
      pointer(ack), length(ack), aMessage.ID);
end;


{ TSocketIORemoteNamespace }

constructor TSocketIORemoteNamespace.Create(aOwner: TEngineIOAbstract;
  const aNameSpace, aSid, aHandshakeData: RawUtf8);
begin
  inherited Create;
  fOwner := aOwner;
  if aNameSpace = '' then
    fNameSpace := DefaultSocketIONameSpace
  else
    fNameSpace := aNameSpace;
  fSid := aSid;
  fHandshakeData := aHandshakeData;
end;

constructor TSocketIORemoteNamespace.CreateFromConnectMessage(
  const aMessage: TSocketIOMessage; const aHandshakeData: RawUtf8; aOwner: TEngineIOAbstract);
var
  data: TDocVariantData;
  sid, namespace: RawUtf8;
begin
  if not aMessage.DataGet(data) or
     not data.GetAsRawUtf8('sid', sid) then
    EEngineIO.RaiseUtf8('%.Create: missing "sid" in message', [aOwner]);
  aMessage.NameSpaceGet(namespace);
  Create(aOwner, namespace, sid, aHandshakeData);
end;

function SocketIOCallbackSearch(cb: PSocketIOCallback; n: integer;
  id: TSocketIOAckID): PSocketIOCallback;
begin // fast O(n) CPU cache brute force search - only a few pending ACK anyway
  result := cb;
  if result <> nil then
    repeat
      if result^.Ack = id then
        exit;
      inc(result);
      dec(n);
    until n = 0;
  result := nil;
end;

function TSocketIORemoteNamespace.GenerateAckId(
  const aOnAck: TOnSocketIOAck): TSocketIOAckID;
var
  cb: PSocketIOCallback;
  n: PtrInt;
begin
  result := InterlockedIncrement(fAckIdCursor);
  n := Length(fCallbacks);
  cb := SocketIOCallbackSearch(pointer(fCallbacks), n, SIO_NO_ACK); // search any void
  if cb = nil then
  begin
    SetLength(fCallbacks, NextGrow(n)); // no void slot: allocate some new ones
    cb := @fCallbacks[n];
  end;
  cb^.Ack := result;
  cb^.OnAck := aOnAck;
  result := result;
end;

function TSocketIORemoteNamespace.SendEvent(const aEventName, aDataArray: RawUtf8;
  const aOnAck: TOnSocketIOAck): TSocketIOAckID;
var
  tmp: TSynTempAdder;
begin
  result := SIO_NO_ACK;
  if Assigned(aOnack) then
    result := GenerateAckId(aOnAck);
  tmp.Init(length(aEventName) + length(aDataArray) + 8); // pre-allocate
  try
    tmp.AddDirect('[', '"');
    tmp.Add(aEventName);
    tmp.AddDirect('"');
    if aDataArray <> '' then
    begin
      tmp.AddDirect(',');
      tmp.Add(aDataArray);
    end;
    tmp.AddDirect(']');
    SocketIOSendPacket(fOwner.fWebSockets,
      sioEvent, fNameSpace, tmp.Buffer, tmp.Size, result);
  finally
    tmp.Store.Done;
  end;
end;

procedure TSocketIORemoteNamespace.Acknowledge(const aMessage: TSocketIOMessage);
var
  cb: PSocketIOCallback;
begin
  // validate message
  if not aMessage.NameSpaceIs(fNameSpace) then
    ESocketIO.RaiseUtf8('%.Acknowledge: unexpected namespace ([%]<>[%])',
      [self, aMessage.NameSpaceShort, fNameSpace]);
  if (aMessage.PacketType <> sioAck) or
     (aMessage.ID = SIO_NO_ACK) then
    ESocketIO.RaiseUtf8('%.Acknowledge: message %#% is not a valid ' +
      'acknowledgment message for namespace %',
      [self, ToText(aMessage.PacketType)^, aMessage.ID, fNameSpace]);
  // search for the registered callback
  cb := SocketIOCallbackSearch(pointer(fCallbacks), length(fCallbacks), aMessage.ID);
  if cb = nil then
    ESocketIO.RaiseUtf8('%.Acknowledge: callback for message ID % not found ' +
      '(may already have been consumed) for namespace %',
        [self, aMessage.ID, fNameSpace]);
  // call the registered callback and remove it from the callback list
  cb^.OnAck(aMessage);
  cb^.Ack := SIO_NO_ACK; // O(1) void the slot - to be reused for the next ack
end;


{ TEngineIOSessionsAbstract }

constructor TEngineIOAbstract.Create;
begin
  // server-side initialization with default values
  inherited Create;
  fVersion := 4;
  fPingTimeout := 20000;
  fPingInterval := 25000;
end;


{ TWebSocketEngineIOProtocol }

function TWebSocketEngineIOProtocol.GetSubprotocols: RawUtf8;
begin
  result := ''; // no "Sec-WebSocket-Protocol:" header
end;

function TWebSocketEngineIOProtocol.SetSubprotocol(const aProtocolName: RawUtf8): boolean;
begin
  result := false; // should never be called
end;

procedure TWebSocketEngineIOProtocol.ProcessIncomingFrame(
  Sender: TWebSocketProcess; var Request: TWebSocketFrame; const Info: RawUtf8);
var
  p: TEngineIOPacket;
begin
  // focText/focBinary or focContinuation/focConnectionClose
  if not (Request.opcode in [focText, focBinary]) then
    exit;
  if Request.payload = '' then
    EEngineIO.RaiseUtf8('%.ProcessIncomingFrame with no Payload', [self]);
  p := TEngineIOPacket(PByte(Request.payload)^ - ord('0'));
  case p of
    eioOpen:
      if fOpened then
        EEngineIO.RaiseUtf8('%.ProcessIncomingFrame: OPEN twice', [self])
      else
        fOpened := true;
    eioClose:
      if fOpened then
        fOpened := false
      else
        EEngineIO.RaiseUtf8('%.ProcessIncomingFrame: unexpected CLOSE', [self]);
    eioPing:
      EngineIOSendPacket(Sender, nil, 0, {binary=}false, eioPong);
    eioPong:
      ; // process depends on the client or server side (mostly do nothing)
    eioMessage:
      if not fOpened then
        EEngineIO.RaiseUtf8('%.ProcessIncomingFrame: missing OPEN', [self]);
  else // eioUpgrade, eioNoop
    EEngineIO.RaiseUtf8('%.ProcessIncomingFrame: unexpected % (%)',
      [self, ToText(p)^, Request.payload[1]])
  end;
  // call virtual method for proper process of this incoming Engine.IO packet
  EnginePacketReceived(Sender, p, @PByteArray(Request.payload)[1],
    length(Request.payload) - 1, (Request.opcode = focBinary));
end;


function EngineIOHandshakeUri(const Root, PollingUpgradeSid: RawUtf8): RawUtf8;
var
  r: RawUtf8;
begin
  r := Root;
  if r = '' then
    r := '/socket.io/'
  else
  begin
    // normalize root
    if r[1] <> '/' then
      insert('/', r, 1);
    if r[length(r)] <> '/' then
      Append(r, '/');
  end;
  // EIO        4          Mandatory, the version of the protocol
  // transport  websocket  Mandatory, the name of the transport
  // sid        <sid>      None here - direct websockets, not from HTTP polling
  // t          <random>   Ensure that the request is not cached by the browser
  FormatUtf8('%?EIO=4&transport=websocket&t=%',
    [r, CardinalToHexShort(Random32Not0)], result);
  if PollingUpgradeSid <> '' then
    Append(result, '&sid=', PollingUpgradeSid);
end;

function EngineIOSendPacket(Sender: TWebSocketProcess;
  Payload: pointer; PayloadLen: PtrInt; PayLoadBinary: boolean;
  PacketType: TEngineIOPacket): boolean;
var
  tmp: TWebSocketFrame; // SendFrame() may change frame content (e.g. mask)
begin
  result := false;
  if (Sender = nil) or
     (Sender.State <> wpsRun)  then
    exit;
  // create Engine.IO packet within this WebSocket frame
  tmp.opcode := focText;
  if PayLoadBinary then
    tmp.opcode := focBinary;
  tmp.content := [];
  tmp.tix := 0;
  FastSetRawByteString(tmp.payload, nil, PayloadLen + 1);
  PByteArray(tmp.payload)[0] := ord(PacketType) + ord('0');
  if PayloadLen <> 0 then
    MoveFast(PayLoad^, PByteArray(tmp.payload)[1], PayloadLen);
  result := Sender.SendFrame(tmp);
end;


function SocketIOReserved(const event: RawUtf8): boolean;
begin
  result := (event = 'connect') or
            (event = 'message') or
            (event = 'disconnect'); // case sensitive
end;

function SocketIOGetNameSpace(one: PSocketIONamespace; count: integer;
  name: PUtf8Char; namelen: TStrLen): TSocketIONamespace;
begin
  if namelen = 0 then // name = '' will search for '/' = DefaultSocketIONameSpace
  begin
    name := pointer(DefaultSocketIONameSpace);
    namelen := length(DefaultSocketIONameSpace);
  end;
  if one <> nil then
    repeat // O(n) brute force search is fast enough
      result := one^;
      if (PStrLen(PAnsiChar(pointer(result.NameSpace)) - _STRLEN)^ = namelen) and
         CompareMemFast(name, pointer(result.NameSpace), namelen) then
        exit;
      inc(one);
      dec(count);
    until count = 0;
  result := nil;
end;

procedure SocketIOGetNameSpaces(one: PSocketIONamespace; count: integer;
  out Dest: TRawUtf8DynArray);
var
  p: PRawUtf8;
begin
  if one = nil then
    exit;
  SetLength(Dest , count);
  p := pointer(Dest);
  repeat
    p^ := one^.NameSpace;
    inc(one);
    inc(p);
    dec(count);
  until count = 0;
end;

procedure SocketIOSendPacket(aWebSockets: TWebCrtSocketProcess;
  aOperation: TSocketIOPacket; const aNamespace: RawUtf8;
  aPayload: pointer; aPayloadLen: PtrInt; ackId: TSocketIOAckID);
var
  tmp: TSynTempAdder;
begin
  if (aWebSockets = nil) or
     not aWebSockets.Protocol.InheritsFrom(TWebSocketEngineIOProtocol) then
    ESocketIO.RaiseUtf8('Unexpected SendSocketPacket(%)', [aWebSockets]);
  tmp.Init(length(aNameSpace) + aPayloadLen + 32); // pre-allocate (unlikely)
  try
    tmp.AddDirect(AnsiChar(ord(aOperation) + ord('0')));
    if (aNameSpace <> '') and
       (aNameSpace <> '/') then
    begin
      tmp.Add(aNameSpace);
      tmp.AddDirect(',');
    end;
    if ackId <> SIO_NO_ACK then
      tmp.AddU(ackID);
    if aPayloadLen <> 0 then
      tmp.Add(aPayload, aPayloadLen);
    EngineIOSendPacket(aWebSockets, tmp.Buffer, tmp.Size, {binary=}false);
  finally
    tmp.Store.Done;
  end;
end;



initialization
  GetEnumNames(TypeInfo(TWebSocketFrameOpCode),
    @_TWebSocketFrameOpCode);
  GetEnumNames(TypeInfo(TWebSocketProcessNotifyCallback),
    @_TWebSocketProcessNotifyCallback);

end.

