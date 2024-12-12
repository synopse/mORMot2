/// low-level access to the libcurl API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.lib.curl;


{
  *****************************************************************************

   Cross-Platform and Cross-Compiler libcurl API
   - CURL Low-Level Constants and Types
   - CURL Functions API

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

{.$define LIBCURLMULTI}
// to include the more advanced "multi session" API functions of libcurl
// see https://curl.se/libcurl/c/libcurl-multi.html interface

{.$define LIBCURLSTATIC}
// to use the static libcurl.a version of the library - mainly for Android

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.net.sock;


{ ************ CURL Low-Level Constants and Types }

{$Z4} // enums should be 32-bit integers here below

type
  /// low-level exception raised during libcurl library access
  ECurl = class(ExceptionWithProps);

const
  { actual libcurl options are a combination of the following
    constants as actual value type, and a sequence number }
  CURLOPTTYPE_LONG          = 0;
  CURLOPTTYPE_OBJECTPOINT   = 10000;
  CURLOPTTYPE_FUNCTIONPOINT = 20000;
  CURLOPTTYPE_OFF_T         = 30000;
  CURLOPTTYPE_BLOB          = 40000;
  CURLOPTTYPE_STRINGPOINT   = CURLOPTTYPE_OBJECTPOINT;
  CURLOPTTYPE_SLISTPOINT    = CURLOPTTYPE_OBJECTPOINT;
  CURLOPTTYPE_CBPOINT       = CURLOPTTYPE_OBJECTPOINT;
  CURLOPTTYPE_VALUES        = CURLOPTTYPE_LONG;

{$ifdef FPC}
  {$WARN 3031 off : Values in enumeration types have to be ascending }
{$endif FPC}

type
  /// low-level options for libcurl library API calls
  TCurlOption = (
    coWriteData              = CURLOPTTYPE_CBPOINT + 1,
    coUrl                    = CURLOPTTYPE_STRINGPOINT + 2,
    coPort                   = CURLOPTTYPE_LONG + 3,
    coProxy                  = CURLOPTTYPE_STRINGPOINT + 4,
    coUserPwd                = CURLOPTTYPE_STRINGPOINT + 5,
    coProxyUserPwd           = CURLOPTTYPE_STRINGPOINT + 6,
    coRange                  = CURLOPTTYPE_STRINGPOINT + 7,
    coReadData               = CURLOPTTYPE_CBPOINT + 9,
    coErrorBuffer            = CURLOPTTYPE_OBJECTPOINT + 10,
    coWriteFunction          = CURLOPTTYPE_FUNCTIONPOINT + 11,
    coReadFunction           = CURLOPTTYPE_FUNCTIONPOINT + 12,
    coTimeout                = CURLOPTTYPE_LONG + 13,
    coInFileSize             = CURLOPTTYPE_LONG + 14,
    coPostFields             = CURLOPTTYPE_OBJECTPOINT + 15,
    coReferer                = CURLOPTTYPE_STRINGPOINT + 16,
    coFtpPort                = CURLOPTTYPE_STRINGPOINT + 17,
    coUserAgent              = CURLOPTTYPE_STRINGPOINT + 18,
    coLowSpeedLimit          = CURLOPTTYPE_LONG + 19,
    coLowSpeedTime           = CURLOPTTYPE_LONG + 20,
    coResumeFrom             = CURLOPTTYPE_LONG + 21,
    coCookie                 = CURLOPTTYPE_STRINGPOINT + 22,
    coHttpHeader             = CURLOPTTYPE_SLISTPOINT + 23,
    coHttpPost               = CURLOPTTYPE_OBJECTPOINT + 24, // deprecated
    coSslCert                = CURLOPTTYPE_STRINGPOINT + 25,
    coKeyPasswd              = CURLOPTTYPE_STRINGPOINT + 26,
    coCRLF                   = CURLOPTTYPE_LONG + 27,
    coQuote                  = CURLOPTTYPE_SLISTPOINT + 28,
    coHeaderData             = CURLOPTTYPE_CBPOINT + 29,
    coCookieFile             = CURLOPTTYPE_STRINGPOINT + 31,
    coSslVersion             = CURLOPTTYPE_VALUES + 32,
    coTimeCondition          = CURLOPTTYPE_VALUES + 33,
    coTimeValue              = CURLOPTTYPE_LONG + 34,
    coCustomRequest          = CURLOPTTYPE_STRINGPOINT + 36,
    coStdErr                 = CURLOPTTYPE_OBJECTPOINT + 37,
    coPostQuote              = CURLOPTTYPE_SLISTPOINT + 39,
    coObsolete40             = CURLOPTTYPE_OBJECTPOINT + 40,
    coVerbose                = CURLOPTTYPE_LONG + 41,
    coHeader                 = CURLOPTTYPE_LONG + 42,
    coNoProgress             = CURLOPTTYPE_LONG + 43,
    coNobody                 = CURLOPTTYPE_LONG + 44,
    coFailOnError            = CURLOPTTYPE_LONG + 45,
    coUpload                 = CURLOPTTYPE_LONG + 46,
    coPost                   = CURLOPTTYPE_LONG + 47,
    coDirListOnly            = CURLOPTTYPE_LONG + 48,
    coAppend                 = CURLOPTTYPE_LONG + 50,
    coNetRC                  = CURLOPTTYPE_VALUES + 51,
    coFollowLocation         = CURLOPTTYPE_LONG + 52,
    coTransferText           = CURLOPTTYPE_LONG + 53,
    coPut                    = CURLOPTTYPE_LONG + 54, // deprecated
    coProgressFunction       = CURLOPTTYPE_FUNCTIONPOINT + 56, // deprecated
    coXferInfoData           = CURLOPTTYPE_CBPOINT + 57,
    coAutoReferer            = CURLOPTTYPE_LONG + 58,
    coProxyPort              = CURLOPTTYPE_LONG + 59,
    coPostFieldSize          = CURLOPTTYPE_LONG + 60,
    coHttpProxyTunnel        = CURLOPTTYPE_LONG + 61,
    coInterface              = CURLOPTTYPE_STRINGPOINT + 62,
    coKRBLevel               = CURLOPTTYPE_STRINGPOINT + 63,
    coSslVerifyPeer          = CURLOPTTYPE_LONG + 64,
    coCAInfo                 = CURLOPTTYPE_STRINGPOINT + 65,
    coMaxRedirs              = CURLOPTTYPE_LONG + 68,
    coFileTime               = CURLOPTTYPE_LONG + 69,
    coTelnetOptions          = CURLOPTTYPE_SLISTPOINT + 70,
    coMaxConnects            = CURLOPTTYPE_LONG + 71,
    coObsolete72             = CURLOPTTYPE_LONG + 72,
    coFreshConnect           = CURLOPTTYPE_LONG + 74,
    coForbidResue            = CURLOPTTYPE_LONG + 75,
    coRandomFile             = CURLOPTTYPE_STRINGPOINT + 76, // deprecated
    coEGDSocket              = CURLOPTTYPE_STRINGPOINT + 77, // deprecated
    coConnectTimeout         = CURLOPTTYPE_LONG + 78,
    coHeaderFunction         = CURLOPTTYPE_FUNCTIONPOINT + 79,
    coHttpGet                = CURLOPTTYPE_LONG + 80,
    coSslVerifyHost          = CURLOPTTYPE_LONG + 81,
    coCookieJar              = CURLOPTTYPE_STRINGPOINT + 82,
    coSslCipherList          = CURLOPTTYPE_STRINGPOINT + 83,
    coHttpVersion            = CURLOPTTYPE_VALUES + 84,
    coFtpUseEPSV             = CURLOPTTYPE_LONG + 85,
    coSslCertType            = CURLOPTTYPE_STRINGPOINT + 86,
    coSslKey                 = CURLOPTTYPE_STRINGPOINT + 87,
    coSslKeyType             = CURLOPTTYPE_STRINGPOINT + 88,
    coSslEngine              = CURLOPTTYPE_STRINGPOINT + 89,
    coSslEngineDefault       = CURLOPTTYPE_LONG + 90,
    coDnsCacheTimeout        = CURLOPTTYPE_LONG + 92,
    coPreQuote               = CURLOPTTYPE_SLISTPOINT + 93,
    coDebugFunction          = CURLOPTTYPE_FUNCTIONPOINT + 94,
    coDebugData              = CURLOPTTYPE_CBPOINT + 95,
    coCookieSession          = CURLOPTTYPE_LONG + 96,
    coCAPath                 = CURLOPTTYPE_STRINGPOINT + 97,
    coBufferSize             = CURLOPTTYPE_LONG + 98,
    coNoSignal               = CURLOPTTYPE_LONG + 99,
    coShare                  = CURLOPTTYPE_OBJECTPOINT + 100,
    coProxyType              = CURLOPTTYPE_LONG + 101,
    coAcceptEncoding         = CURLOPTTYPE_STRINGPOINT + 102,
    coPrivate                = CURLOPTTYPE_OBJECTPOINT + 103,
    coHttp200Aliases         = CURLOPTTYPE_SLISTPOINT + 104,
    coUnrestrictedAuth       = CURLOPTTYPE_LONG + 105,
    coFtpUseEPRT             = CURLOPTTYPE_LONG + 106,
    coHttpAuth               = CURLOPTTYPE_VALUES + 107,
    coSslCtxFunction         = CURLOPTTYPE_FUNCTIONPOINT + 108,
    coSslCtxData             = CURLOPTTYPE_CBPOINT + 109,
    coFtpCreateMissingDirs   = CURLOPTTYPE_LONG + 110,
    coProxyAuth              = CURLOPTTYPE_VALUES + 111,
    coServerResponseTimeout  = CURLOPTTYPE_LONG + 112,
    coIPResolve              = CURLOPTTYPE_VALUES + 113,
    coMaxFileSize            = CURLOPTTYPE_LONG + 114,
    coInFileSizeLarge        = CURLOPTTYPE_OFF_T + 115,
    coResumeFromLarge        = CURLOPTTYPE_OFF_T + 116,
    coMaxFileSizeLarge       = CURLOPTTYPE_OFF_T + 117,
    coNetRCFile              = CURLOPTTYPE_STRINGPOINT + 118,
    coUseSsl                 = CURLOPTTYPE_VALUES + 119,
    coPostFieldSizeLarge     = CURLOPTTYPE_OFF_T + 120,
    coTcpNoDelay             = CURLOPTTYPE_LONG + 121,
    coFtpSslAuth             = CURLOPTTYPE_VALUES + 129,
    coIOCTLFunction          = CURLOPTTYPE_FUNCTIONPOINT + 130, // deprecated
    coIOCTLData              = CURLOPTTYPE_CBPOINT + 131, // deprecated
    coFtpAccount             = CURLOPTTYPE_STRINGPOINT + 134,
    coCookieList             = CURLOPTTYPE_STRINGPOINT + 135,
    coIgnoreContentLength    = CURLOPTTYPE_LONG + 136,
    coFtpSkipPASVIp          = CURLOPTTYPE_LONG + 137,
    coFtpFileMethod          = CURLOPTTYPE_VALUES + 138,
    coLocalPort              = CURLOPTTYPE_LONG + 139,
    coLocalPortRange         = CURLOPTTYPE_LONG + 140,
    coConnectOnly            = CURLOPTTYPE_LONG + 141,
    coMaxSendSpeedLarge      = CURLOPTTYPE_OFF_T + 145,
    coMaxRecvSpeedLarge      = CURLOPTTYPE_OFF_T + 146,
    coFtpAlternativeToUser   = CURLOPTTYPE_STRINGPOINT + 147,
    coSockOptFunction        = CURLOPTTYPE_FUNCTIONPOINT + 148,
    coSockOptData            = CURLOPTTYPE_CBPOINT + 149,
    coSslSessionIDCache      = CURLOPTTYPE_LONG + 150,
    coSshAuthTypes           = CURLOPTTYPE_VALUES + 151,
    coSshPublicKeyfile       = CURLOPTTYPE_STRINGPOINT + 152,
    coSshPrivateKeyfile      = CURLOPTTYPE_STRINGPOINT + 153,
    coFtpSslCCC              = CURLOPTTYPE_LONG + 154,
    coTimeoutMs              = CURLOPTTYPE_LONG + 155,
    coConnectTimeoutMs       = CURLOPTTYPE_LONG + 156,
    coHttpTransferDecoding   = CURLOPTTYPE_LONG + 157,
    coHttpContentDecoding    = CURLOPTTYPE_LONG + 158,
    coNewFilePerms           = CURLOPTTYPE_LONG + 159,
    coNewDirectoryPerms      = CURLOPTTYPE_LONG + 160,
    coPostRedir              = CURLOPTTYPE_VALUES + 161,
    coSshHostPublicKeyMD5    = CURLOPTTYPE_STRINGPOINT + 162,
    coOpenSocketFunction     = CURLOPTTYPE_FUNCTIONPOINT + 163,
    coOpenSocketData         = CURLOPTTYPE_CBPOINT + 164,
    coCopyPostFields         = CURLOPTTYPE_OBJECTPOINT + 165,
    coProxyTransferMode      = CURLOPTTYPE_LONG + 166,
    coSeekFunction           = CURLOPTTYPE_FUNCTIONPOINT + 167,
    coSeekData               = CURLOPTTYPE_CBPOINT + 168,
    coCRLFile                = CURLOPTTYPE_STRINGPOINT + 169,
    coIssuerCert             = CURLOPTTYPE_STRINGPOINT + 170,
    coAddressScope           = CURLOPTTYPE_LONG + 171,
    coCertInfo               = CURLOPTTYPE_LONG + 172,
    coUserName               = CURLOPTTYPE_STRINGPOINT + 173,
    coPassword               = CURLOPTTYPE_STRINGPOINT + 174,
    coProxyUsername          = CURLOPTTYPE_STRINGPOINT + 175,
    coProxyPassword          = CURLOPTTYPE_STRINGPOINT + 176,
    coNoProxy                = CURLOPTTYPE_STRINGPOINT + 177,
    coTFtpBlkSize            = CURLOPTTYPE_LONG + 178,
    coSocks5GssApiNec        = CURLOPTTYPE_LONG + 180,
    coSshKnownHosts          = CURLOPTTYPE_STRINGPOINT + 183,
    coSshKeyFuntion          = CURLOPTTYPE_FUNCTIONPOINT + 184,
    coSshKeyData             = CURLOPTTYPE_CBPOINT + 185,
    coMailFrom               = CURLOPTTYPE_STRINGPOINT + 186,
    coMailRcpt               = CURLOPTTYPE_SLISTPOINT + 187,
    coFtpUsePRET             = CURLOPTTYPE_LONG + 188,
    coRtspRequest            = CURLOPTTYPE_VALUES + 189,
    coRtspSessionID          = CURLOPTTYPE_STRINGPOINT + 190,
    coRtspStreamURI          = CURLOPTTYPE_STRINGPOINT + 191,
    coRtspTransport          = CURLOPTTYPE_STRINGPOINT + 192,
    coRtspClientCSeq         = CURLOPTTYPE_LONG + 193,
    coRtspServerCSeq         = CURLOPTTYPE_LONG + 194,
    coInterleaveData         = CURLOPTTYPE_CBPOINT + 195,
    coInterleaveFunction     = CURLOPTTYPE_FUNCTIONPOINT + 196,
    coWildcardMatch          = CURLOPTTYPE_LONG + 197,
    coChunkBgnFunction       = CURLOPTTYPE_FUNCTIONPOINT + 198,
    coChunkEndFunction       = CURLOPTTYPE_FUNCTIONPOINT + 199,
    coFnMatchFunction        = CURLOPTTYPE_FUNCTIONPOINT + 200,
    coChunkData              = CURLOPTTYPE_CBPOINT + 201,
    coFnMatchData            = CURLOPTTYPE_CBPOINT + 202,
    coResolve                = CURLOPTTYPE_SLISTPOINT + 203,
    coTlsAuthUsername        = CURLOPTTYPE_STRINGPOINT + 204,
    coTlsAuthPassword        = CURLOPTTYPE_STRINGPOINT + 205,
    coTlsAuthType            = CURLOPTTYPE_STRINGPOINT + 206,
    coTransferEncoding       = CURLOPTTYPE_LONG + 207,
    coCloseSocketFunction    = CURLOPTTYPE_FUNCTIONPOINT + 208,
    coCloseSocketData        = CURLOPTTYPE_CBPOINT + 209,
    coGssApiDelegation       = CURLOPTTYPE_VALUES + 210,
    coDnsServers             = CURLOPTTYPE_STRINGPOINT + 211,
    coAcceptTimeoutMs        = CURLOPTTYPE_LONG + 212,
    coTcpKeepalive           = CURLOPTTYPE_LONG + 213,
    coTcpKeepIdle            = CURLOPTTYPE_LONG + 214,
    coTcpKeepIntvl           = CURLOPTTYPE_LONG + 215,
    coSslOptions             = CURLOPTTYPE_VALUES + 216,
    coMailAuth               = CURLOPTTYPE_STRINGPOINT + 217,
    coSaslIR                 = CURLOPTTYPE_LONG + 218,
    coXferInfoFunction       = CURLOPTTYPE_FUNCTIONPOINT + 219,
    coXOAuth2Bearer          = CURLOPTTYPE_STRINGPOINT + 220,
    coDnsInterface           = CURLOPTTYPE_STRINGPOINT + 221,
    coDnsLocalIP4            = CURLOPTTYPE_STRINGPOINT + 222,
    coDnsLocalIP6            = CURLOPTTYPE_STRINGPOINT + 223,
    coLoginOptions           = CURLOPTTYPE_STRINGPOINT + 224,
    coSslEnableAlpn          = CURLOPTTYPE_LONG + 226,
    coExpect100TimeoutMs     = CURLOPTTYPE_LONG + 227,
    coProxyHeader            = CURLOPTTYPE_SLISTPOINT + 228,
    coHeaderOpt              = CURLOPTTYPE_VALUES + 229,
    coPinnedPublicKey        = CURLOPTTYPE_STRINGPOINT + 230,
    coUnixSocketPath         = CURLOPTTYPE_STRINGPOINT + 231,
    coSslVerifyStatus        = CURLOPTTYPE_LONG + 232,
    coSslFalseStart          = CURLOPTTYPE_LONG + 233,
    coPathAsIs               = CURLOPTTYPE_LONG + 234,
    coProxyServiceName       = CURLOPTTYPE_STRINGPOINT + 235,
    coServiceName            = CURLOPTTYPE_STRINGPOINT + 236,
    coPipeWait               = CURLOPTTYPE_LONG + 237,
    coDefaultProtocol        = CURLOPTTYPE_STRINGPOINT + 238,
    coStreamWeight           = CURLOPTTYPE_LONG + 239,
    coStreamDepends          = CURLOPTTYPE_OBJECTPOINT + 240,
    coStreamDependsE         = CURLOPTTYPE_OBJECTPOINT + 241,
    coTFtpNoOptions          = CURLOPTTYPE_LONG + 242,
    coConnectTo              = CURLOPTTYPE_SLISTPOINT + 243,
    coTcpFastOpen            = CURLOPTTYPE_LONG + 244,
    coKeepSendingOnError     = CURLOPTTYPE_LONG + 245,
    coProxyCAInfo            = CURLOPTTYPE_STRINGPOINT + 246,
    coProxyYCAPath           = CURLOPTTYPE_STRINGPOINT + 247,
    coProxySslVerifyPeer     = CURLOPTTYPE_LONG + 248,
    coProxySslVeryfyHost     = CURLOPTTYPE_LONG + 249,
    coProxySslVersion        = CURLOPTTYPE_VALUES + 250,
    coProxyTlsAuthUsername   = CURLOPTTYPE_STRINGPOINT + 251,
    coProxyTlsAuthPassword   = CURLOPTTYPE_STRINGPOINT + 252,
    coProxyTlsAuthType       = CURLOPTTYPE_STRINGPOINT + 253,
    coProxySslCert           = CURLOPTTYPE_STRINGPOINT + 254,
    coProxySslCertType       = CURLOPTTYPE_STRINGPOINT + 255,
    coProxySslKey            = CURLOPTTYPE_STRINGPOINT + 256,
    coProxySslKeyType        = CURLOPTTYPE_STRINGPOINT + 257,
    coProxyKeyPasswd         = CURLOPTTYPE_STRINGPOINT + 258,
    coProxySslCipherList     = CURLOPTTYPE_STRINGPOINT + 259,
    coProxyCRLFile           = CURLOPTTYPE_STRINGPOINT + 260,
    coProxySslOptions        = CURLOPTTYPE_LONG + 261,
    coPreProxy               = CURLOPTTYPE_STRINGPOINT + 262,
    coProxyPinnedPublicKey   = CURLOPTTYPE_STRINGPOINT + 263,
    coAbstractUnixSocket     = CURLOPTTYPE_STRINGPOINT + 264,
    coSupressConnectHeaders  = CURLOPTTYPE_LONG + 265,
    coRequestTarget          = CURLOPTTYPE_STRINGPOINT + 266,
    coSocks5Auth             = CURLOPTTYPE_LONG + 267,
    coSshCompression         = CURLOPTTYPE_LONG + 268,
    coMimePost               = CURLOPTTYPE_OBJECTPOINT + 269,
    coTimeValueLarge         = CURLOPTTYPE_OFF_T + 270,
    coHappyEyeballsTimeoutMs = CURLOPTTYPE_LONG + 271,
    coResolverStartFunction  = CURLOPTTYPE_FUNCTIONPOINT + 272,
    coResolverStartData      = CURLOPTTYPE_CBPOINT + 273,
    coHAProxyProtocol        = CURLOPTTYPE_LONG + 274,
    coDnsShuffleAddresses    = CURLOPTTYPE_LONG + 275,
    coTls13Ciphers           = CURLOPTTYPE_STRINGPOINT + 276,
    coProxyTls13Ciphers      = CURLOPTTYPE_STRINGPOINT + 277,
    coDisallowUsernameInUrl  = CURLOPTTYPE_LONG + 278,
    coDohUR                  = CURLOPTTYPE_STRINGPOINT + 279,
    coUploadBufferSize       = CURLOPTTYPE_LONG + 280,
    coUpkeepIntervalMs       = CURLOPTTYPE_LONG + 281,
    coCurlLU                 = CURLOPTTYPE_OBJECTPOINT + 282,
    coTrailerFunction        = CURLOPTTYPE_FUNCTIONPOINT + 283,
    coTrailerData            = CURLOPTTYPE_CBPOINT + 284,
    coHttp09Allowed          = CURLOPTTYPE_LONG + 285,
    coAltSvcCtrl             = CURLOPTTYPE_LONG + 286,
    coAltSvc                 = CURLOPTTYPE_STRINGPOINT + 287,
    coMaxAgeConn             = CURLOPTTYPE_LONG + 288,
    coSaslAuthZID            = CURLOPTTYPE_STRINGPOINT + 289,
    coMailRcptAllowFails     = CURLOPTTYPE_LONG + 290,
    coSslCertBlob            = CURLOPTTYPE_BLOB + 291,
    coSslKeyBlob             = CURLOPTTYPE_BLOB + 292,
    coProxySslCertBlob       = CURLOPTTYPE_BLOB + 293,
    coProxySslKeyBlob        = CURLOPTTYPE_BLOB + 294,
    coIssuerCertBlob         = CURLOPTTYPE_BLOB + 295,
    coProxyIssuerCert        = CURLOPTTYPE_STRINGPOINT + 296,
    coProxyIssuerCertBlob    = CURLOPTTYPE_BLOB + 297,
    coSslECCurves            = CURLOPTTYPE_STRINGPOINT + 298,
    coHstsCtrl               = CURLOPTTYPE_LONG + 299,
    coHsts                   = CURLOPTTYPE_STRINGPOINT + 300,
    coHstsReadFuction        = CURLOPTTYPE_FUNCTIONPOINT + 301,
    coHstsReadData           = CURLOPTTYPE_CBPOINT + 302,
    coHstsWriteFunction      = CURLOPTTYPE_FUNCTIONPOINT + 303,
    coHstsWriteDate          = CURLOPTTYPE_CBPOINT + 304,
    coAwsSigV4               = CURLOPTTYPE_STRINGPOINT + 305,
    coDohSslVerifyPeer       = CURLOPTTYPE_LONG + 306,
    coDohSslVerifyHost       = CURLOPTTYPE_LONG + 307,
    coDohSslVerifyStatus     = CURLOPTTYPE_LONG + 308,
    coCAInfoBlob             = CURLOPTTYPE_BLOB + 309,
    coProxyCAInfoBlob        = CURLOPTTYPE_BLOB + 310,
    coSshHostPublicKeySHA256 = CURLOPTTYPE_STRINGPOINT + 311,
    coPreReqFunction         = CURLOPTTYPE_FUNCTIONPOINT + 312,
    coPreReqData             = CURLOPTTYPE_CBPOINT + 313,
    coMaxLifetimeConn        = CURLOPTTYPE_LONG + 314,
    coMimeOptions            = CURLOPTTYPE_LONG + 315,
    coSshHostKeyFunction     = CURLOPTTYPE_FUNCTIONPOINT + 316,
    coSshHostKeyData         = CURLOPTTYPE_CBPOINT + 317,
    coProtocolsStr           = CURLOPTTYPE_STRINGPOINT + 318,
    coRedirProtocolsStr      = CURLOPTTYPE_STRINGPOINT + 319,
    coWSOptions              = CURLOPTTYPE_LONG + 320,
    coCACacheTimeout         = CURLOPTTYPE_LONG + 321,
    coQuickExit              = CURLOPTTYPE_LONG + 322,
    coHAProxyClientIP        = CURLOPTTYPE_STRINGPOINT + 323
  );

  /// low-level result codes for libcurl library API calls
  TCurlResult = (
    crOK,
    crUnsupportedProtocol,
    crFailedInit,
    crURLMalformat,
    crNotBuiltIn,
    crCouldNotResolveProxy,
    crCouldNotResolveHost,
    crCouldNotConnect,
    crWeirdServerReply,
    crRemoteAccessDenied,
    crFtpAccessFailed,
    crFtpWeirdPassReply,
    crFtpAcceptTimeout,
    crFtpWeirdPasvReply,
    crFtpWeird227Format,
    crFtpCannotGetHost,
    crHttp2,
    crFtpCouldNotSetType,
    crPartialFile,
    crFtpCouldNotRetrFile,
    crObsolete20,
    crQuoteError,
    crHttpReturnedError,
    crWriteError,
    crObsolete24,
    crUploadFailed,
    crReadError,
    crOutOfMemory,
    crOperationTimeout,
    crObsolete29,
    crFtpPortFailed,
    crFtpCouldNotUseRest,
    crObsolete32,
    crRangeError,
    crHttpPostError,
    crSslConnectError,
    crBadDownloadResume,
    crFileCouldNotReadFile,
    crLdapCannotBind,
    crLdapSearchFailed,
    crObsolete40,
    crFunctionNotFound,
    crAbortedByCallback,
    crBadFunctionArgument,
    crObsolete44,
    crInterfaceFailed,
    crObsolete46,
    crTooManyRedirects,
    crUnknownOption,
    crSetOptOptionSyntax,
    crObsolete50,
    crObsolete51,
    crGotNothing,
    crSslEngineNotFound,
    crSslEngineSetFailed,
    crSendError,
    crRecvError,
    crObsolete57,
    crSslCertProblem,
    crSslCipher,
    crPeerFailedVerification,
    crBadContentEncoding,
    crObsolete62,
    crFileSizeExceeded,
    crUseSslFailed,
    crSendFailRewind,
    crSslEngineInitFailed,
    crLoginDenied,
    crTFtpNotFound,
    crTFtpPerm,
    crRemoteDiskFull,
    crTFtpIllegal,
    crTFtpUnknownID,
    crRemoteFileExists,
    crTFtpNoSuchUser,
    crObsolete75,
    crObsolete76,
    crSslCACertBadFile,
    crRemoteFileNotFound,
    crSsh,
    crSslShutdownFailed,
    crAgain,
    crSslCrlBadFile,
    crSslIssuerError,
    crFtpPRETFailed,
    crRtspCSeqError,
    crRtspSessionError,
    crFtpBadFileList,
    crChunkFailed,
    crNoConnectionAvailable,
    crSslPinnedPubKeyNotMatch,
    crSslInvalidCertStatus,
    crHttp2Stream,
    crResursiveApiCall,
    crAuthError,
    crHttp3,
    crQuicConnectError,
    crProxy,
    crSslClientCert,
    crUnrecoverablePoll
  );

  /// libcurl share interface result codes
  TCurlShareResult = (
    csrOk,
    csrBadOption,
    csrInUse,
    csrInvalid,
    csrNoMem,
    csrNotBuiltIn
  );

  /// libcurl share interface options
  TCurlShareOption = (
    csoNone,
    csoShare,
    csoUnShare,
    csoLockFunc,
    csoUnLockFunc,
    csoUserData
  );

const
  { actual libcurl infos are a combination of the following
    constants as actual value type, and a sequence number }
  CURLINFO_STRING = $100000;
  CURLINFO_LONG   = $200000;
  CURLINFO_DOUBLE = $300000;
  CURLINFO_SLIST  = $400000;
  CURLINFO_PTR    = $400000; // same as SLIST
  CURLINFO_SOCKET = $500000;
  CURLINFO_OFF_T  = $600000;

type
  TCurlInfo = (
    ciEffectiveURL           = CURLINFO_STRING + 1,
    ciResponseCode           = CURLINFO_LONG   + 2,
    ciTotalTime              = CURLINFO_DOUBLE + 3,
    ciNameLookupTime         = CURLINFO_DOUBLE + 4,
    ciConnectTime            = CURLINFO_DOUBLE + 5,
    ciPreTransferTime        = CURLINFO_DOUBLE + 6,
    ciSizeUpload             = CURLINFO_DOUBLE + 7,  // deprecated since v7.55.0, use ciSizeUploadT
    ciSizeUploadT            = CURLINFO_OFF_T  + 7,
    ciSizeDownload           = CURLINFO_DOUBLE + 8,  // deprecated since v7.55.0, use ciSizeDownloadT
    ciSizeDownloadT          = CURLINFO_OFF_T  + 8,
    ciSpeedDownload          = CURLINFO_DOUBLE + 9,  // deprecated since v7.55.0, use ciSpeedDownloadT
    ciSpeedDownloadT         = CURLINFO_OFF_T  + 9,
    ciSpeedUpload            = CURLINFO_DOUBLE + 10, // deprecated since v7.55.0, use ciSpeedUploadT
    ciSpeedUploadT           = CURLINFO_OFF_T  + 10,
    ciHeaderSize             = CURLINFO_LONG   + 11,
    ciRequestSize            = CURLINFO_LONG   + 12,
    ciSslVerifyResult        = CURLINFO_LONG   + 13,
    ciFileTime               = CURLINFO_LONG   + 14,
    ciFileTimeT              = CURLINFO_OFF_T  + 14,
    ciContentLengthDownload  = CURLINFO_DOUBLE + 15, // deprecated since v7.55.0, use ciContentLengthDownloadT
    ciContentLengthDownloadT = CURLINFO_OFF_T  + 15,
    ciContentLengthUpload    = CURLINFO_DOUBLE + 16, // deprecated since v7.55.0, use ciContentLengthUploadT
    ciContentLengthUploadT   = CURLINFO_OFF_T  + 16,
    ciStartTransferTime      = CURLINFO_DOUBLE + 17,
    ciContentType            = CURLINFO_STRING + 18,
    ciRedirectTime           = CURLINFO_DOUBLE + 19,
    ciRedirectCount          = CURLINFO_LONG   + 20,
    ciPrivate                = CURLINFO_STRING + 21,
    ciHttpConnectCode        = CURLINFO_LONG   + 22,
    ciHttpAuthAvail          = CURLINFO_LONG   + 23,
    ciProxyAuthAvail         = CURLINFO_LONG   + 24,
    ciOSErrno                = CURLINFO_LONG   + 25,
    ciNumConnects            = CURLINFO_LONG   + 26,
    ciSslEngines             = CURLINFO_SLIST  + 27,
    ciCookieList             = CURLINFO_SLIST  + 28,
    ciLastSocket             = CURLINFO_LONG   + 29, // deprecated since v7.45.0, use ciActiveSocket
    ciFtpEntryPath           = CURLINFO_STRING + 30,
    ciRedirectURL            = CURLINFO_STRING + 31,
    ciPrimaryIP              = CURLINFO_STRING + 32,
    ciAppConnectTime         = CURLINFO_DOUBLE + 33,
    ciCertInfo               = CURLINFO_PTR    + 34,
    ciConditionUnmet         = CURLINFO_LONG   + 35,
    ciRtspSessionID          = CURLINFO_STRING + 36,
    ciRtspClientCSeq         = CURLINFO_LONG   + 37,
    ciRtspServerCSeq         = CURLINFO_LONG   + 38,
    ciRtspCSeqRecv           = CURLINFO_LONG   + 39,
    ciPrimaryPort            = CURLINFO_LONG   + 40,
    ciLocalIP                = CURLINFO_STRING + 41,
    ciLocalPort              = CURLINFO_LONG   + 42,
    ciTlsSession             = CURLINFO_PTR    + 43, // deprecated since v7.48.0, use ciTlsSslPtr
    ciActiveSocket           = CURLINFO_SOCKET + 44,
    ciTlsSslPtr              = CURLINFO_PTR    + 45,
    ciHttpVersion            = CURLINFO_LONG   + 46,
    ciProxySslVerifyResult   = CURLINFO_LONG   + 47,
    ciProtocol               = CURLINFO_LONG   + 48, // deprecated since v7.85.0, use ciScheme
    ciScheme                 = CURLINFO_STRING + 49,
    ciTotalTimeT             = CURLINFO_OFF_T  + 50, // can be used for calculation "Content download time"
    ciNameLookupTimeT        = CURLINFO_OFF_T  + 51,
    ciConnectTimeT           = CURLINFO_OFF_T  + 52,
    ciPreTransferTimeT       = CURLINFO_OFF_T  + 53,
    ciStartTransferTimeT     = CURLINFO_OFF_T  + 54,
    ciRedirectTimeT          = CURLINFO_OFF_T  + 55,
    ciAppConnectTimeT        = CURLINFO_OFF_T  + 56, // TLS handshake
    ciRetryAfter             = CURLINFO_OFF_T  + 57,
    ciEffectiveMethod        = CURLINFO_STRING + 58,
    ciProxyError             = CURLINFO_LONG   + 59,
    ciReferer                = CURLINFO_STRING + 60,
    ciCAInfo                 = CURLINFO_STRING + 61,
    ciCAPath                 = CURLINFO_STRING + 62,
    ciXferID                 = CURLINFO_OFF_T  + 63,
    ciConnID                 = CURLINFO_OFF_T  + 64
  );

  /// low-level parameter for coHttpVersion
  TCurlHttpVersion = (
    chvNone,
    chv1_0,
    chv1_1,
    chv2,
    chv2Tls,
    chv2PriorKnowledge,
    chv3 = 30,
    chv3Only = 31
  );

  /// items of low-level parameter for coHttpAuth
  TCurlAuth = (
    cauBasic     = 0,
    cauDigest    = 1,
    cauNegotiate = 2,
    cauNtlm      = 3,
    cauDigest_IE = 4,
    cauNtlm_WB   = 5,
    cauBearer    = 6,
    cauAws_SigV4 = 7,
    cauOnly      = 31);

  /// low-level parameter for coHttpAuth
  TCurlAuths = set of TCurlAuth;

  /// low-level parameter for coUseSsl
  TCurlUseSsl = (
    cusNone,
    cusTry,
    cusControl,
    cusAll
  );

  /// low-level argument to the coDebugFunction callback
  TCurlDebug = (
    cuText,
    cuHeaderIn,
    cuHeaderOut,
    cuDataIn,
    cuDataOut,
    cuSslDataIn,
    cuSslDataOut
  );

  {$ifdef LIBCURLMULTI}

  /// low-level result codes for libcurl library API calls in "multi" mode
  TCurlMultiCode = (
    cmcCallMultiPerform = -1,
    cmcOK = 0,
    cmcBadHandle,
    cmcBadEasyHandle,
    cmcOutOfMemory,
    cmcInternalError,
    cmcBadSocket,
    cmcUnknownOption,
    cmcAddedAlready,
    cmcRecursiveApiCall,
    cmcWakeupFailure,
    cmcBadFunctionArgument,
    cmcAbortedByCallback,
    cmcUnrecoverablePoll
  );

  /// low-level options for libcurl library API calls in "multi" mode
  TCurlMultiOption = (
    cmoPipeLining               = 3,
    cmoMaxConnects              = 6,
    cmoMaxHostConnections       = 7,
    cmoMaxPipelineLength        = 8,
    cmoMaxTotalConnections      = 13,
    cmoSocketData               = 10002,
    cmoTimerData                = 10005,
    cmoPipeliningSiteBL         = 10011,
    cmoPipeliningServerBL       = 10012,
    cmoPushData                 = 10015,
    cmoSocketFunction           = 20001,
    cmoTimerFunction            = 20004,
    cmoPushFunction             = 20014,
    cmoContentLengthPenaltySize = 30009,
    cmoChunkLengthPenaltySize   = 30010
  );

  {$endif LIBCURLMULTI}

  /// low-level version identifier of the libcurl library
  TCurlVersion = (
    cv1,
    cv2,
    cv3,
    cv4,
    cv5,
    cv6,
    cv7,
    cv8,
    cv9,
    cv10,
    cv11
  );

  /// low-level initialization option for libcurl library API
  // - currently, only giSsl is set, since giWin32 is redundant with WinHttp
  TCurlGlobalInit = set of (
    giNone,
    giSsl,
    giWin32,
    giAll
  );

  /// low-level message state for libcurl library API
  TCurlMsg = (
    cmNone,
    cmDone
  );

  /// libcurl multipart/formdata HTTP POST result codes
  TCurlFormCode = (
    CURL_FORMADD_OK,
    CURL_FORMADD_MEMORY,
    CURL_FORMADD_OPTION_TWICE,
    CURL_FORMADD_NULL,
    CURL_FORMADD_UNKNOWN_OPTION,
    CURL_FORMADD_INCOMPLETE,
    CURL_FORMADD_ILLEGAL_ARRAY,
    CURL_FORMADD_DISABLED
  );

  /// libcurl multipart/formdata HTTP POST options
  TCurlFormOption = (
    CURLFORM_NOTHING,
    CURLFORM_COPYNAME, // followed by a string which will be copied by libcurl
    CURLFORM_PTRNAME,  // followed by a string which pointer will stay available
    CURLFORM_NAMELENGTH, // if CURLFORM_COPYNAME/PTRNAME is not #0 terminated
    CURLFORM_COPYCONTENTS, // followed by a pointer to the contents to be copied
    CURLFORM_PTRCONTENTS,  // followed by a persistent pointer to the contents
    CURLFORM_CONTENTSLENGTH, // deprecated - use CURLFORM_CONTENTLEN
    CURLFORM_FILECONTENT, // followed by a filename - may not be a file upload
    CURLFORM_ARRAY,
    CURLFORM_OBSOLETE,
    CURLFORM_FILE, // followed by a filename - will be a file upload part
    CURLFORM_BUFFER, // followed by a filename - file upload without CURLFORM_FILE
    CURLFORM_BUFFERPTR,     // CURLFORM_BUFFER persistent pointer
    CURLFORM_BUFFERLENGTH,  // CURLFORM_BUFFER persistent size
    CURLFORM_CONTENTTYPE, // followed by a pointer to a string, for CURLFORM_FILE
    CURLFORM_CONTENTHEADER, // followed by a curl_slist_append() list of headers
    CURLFORM_FILENAME, // followed by a pointer to a string, for CURLFORM_FILE
    CURLFORM_END,
    CURLFORM_OBSOLETE2,
    CURLFORM_STREAM,
    CURLFORM_CONTENTLEN // added in 7.46.0, provide a curl_off_t 64-bit size
  );

  /// low-level items for TCurlVersionInfo.features
  TCurlVersionFeature = (
    cvfIPv6,
    cvfKerberos4,
    cvfSsl,
    cvfLibz,
    cvfNtlm,
    cvfGssNegotiate,
    cvfDebug,
    cvfAsynchDns,
    cvfSpNego,
    cvfLargefile,
    cvfIDN,
    cvfSspi,
    cvfConv,
    cvfCurlDebug,
    cvfTlsAuthSrp,
    cvfNtlmWB,
    cvfHttp2,
    cvfGssApi,
    cvfKerberos5,
    cvfUnixSockets,
    cvfPsl,
    cvfHttpSProxy,
    cvfMultiSsl,
    cvfBrotli,
    cvfAltSvc,
    cvfHttp3,
    cvfZstd,
    cvfUnicode,
    cvfHsts,
    cvfGSasl,
    cvfThreadsafe
  );
  /// low-level set for TCurlVersionInfo.features
  TCurlVersionFeatures = set of TCurlVersionFeature;

  /// low-level version information for libcurl library (in its cv11 layout)
  // - you need to check the actual version before accessing any of its members
  TCurlVersionInfo = record
    age: TCurlVersion;
    version: PAnsiChar;
    version_num: cardinal;
    host: PAnsiChar;
    features: TCurlVersionFeatures;
    ssl_version: PAnsiChar;
    ssl_version_num: PAnsiChar;
    libz_version: PAnsiChar;
    protocols: PPAnsiCharArray;
    ares: PAnsiChar;
    ares_num: integer;
    libidn: PAnsiChar;
    iconv_ver_num: integer;
    libssh_version: PAnsiChar;
    brotli_ver_num: cardinal;
    brotli_version: PAnsiChar;
    nghttp2_ver_num: cardinal;
    nghttp2_version: PAnsiChar;
    quic_version: PAnsiChar;
    cainfo: PAnsiChar;
    capath: PAnsiChar;
    zstd_ver_num: integer;
    zstd_version: PAnsiChar;
    hyper_version: PAnsiChar;
    gsasl_version: PAnsiChar;
    feature_names: PPAnsiCharArray;
  end;
  PCurlVersionInfo = ^TCurlVersionInfo;

  /// low-level access to the libcurl library instance
  TCurl = type pointer;

  /// low-level string list type for libcurl library API
  TCurlSList = type pointer;
  PCurlSList = ^TCurlSList;
  PPCurlSListArray = ^PCurlSListArray;
  PCurlSListArray = array[0 .. (MaxInt div SizeOf(PCurlSList)) - 1] of PCurlSList;

  /// low-level access to the libcurl share interface
  TCurlShare = type pointer;

  /// low-level access to the libcurl mime interface
  TCurlMime = type pointer;

  /// low-level access to the libcurl mime part interface
  TCurlMimePart = type pointer;

  /// low-level access to the libcurl library instance in "multi" mode
  TCurlMulti = type pointer;

  /// low-level access to one libcurl library socket instance
  TCurlSocket = type TNetSocket;

  /// low-level certificate information for libcurl library API
  TCurlCertInfo = packed record
    /// count of items in certinfo[]
    num_of_certs: integer;
    {$ifdef CPUX64} _align: array[0..3] of byte; {$endif}
    /// actual list of certificates
    certinfo: PPCurlSListArray;
  end;
  PCurlCertInfo = ^TCurlCertInfo;

  /// pointer reference to the next multipart/formdata HTTP POST section
  PCurlHttpPost = ^TCurlHttpPost;

  /// defines a multipart/formdata HTTP POST section
  TCurlHttpPost = record
    /// next entry in the list
    next: PCurlHttpPost;
    /// pointer to allocated name
    name: PAnsiChar;
    /// length of name length
    namelength: integer;
    /// pointer to allocated data contents
    contents: PAnsiChar;
    /// length of contents field
    // - see also CURL_HTTPPOST_LARGE / 64-bit contentlen
    contentslength: integer;
    /// pointer to allocated buffer contents
    buffer: PAnsiChar;
    /// length of buffer field
    bufferlength: integer;
    /// Content-Type text
    contenttype: PAnsiChar;
    /// list of extra headers for this form
    contentheader: PPCurlSListArray;
    /// if one field name has more than one file, this link should link to following files
    more: PCurlHttpPost;
    /// some associated flags
    flags: integer;
    /// file name to show. If not set, the actual file name will be used (if this is a file part)
    showfilename: PAnsiChar;
    /// custom pointer used for HTTPPOST_CALLBACK posts
    userp: pointer;
    /// alternative length of contents field - used if CURL_HTTPPOST_LARGE is set
    // - added in 7.46.0 - is defined as POSIX off_t
    contentlen: Int64;
  end;

  /// low-level message information for libcurl library API
  TCurlMsgRec = packed record
    msg: TCurlMsg;
    {$ifdef CPUX64} _align: array[0..3] of byte; {$endif}
    easy_handle: TCurl;
    data: packed record case byte of
      0: (whatever: pointer);
      1: (result: TCurlResult);
    end;
  end;
  PCurlMsgRec = ^TCurlMsgRec;

  /// low-level file description event handler for libcurl library API
  TCurlWaitFD = packed record
    fd: TCurlSocket;
    events: SmallInt;
    revents: SmallInt;
    {$ifdef CPUX64} _align: array[0..3] of byte; {$endif}
  end;
  PCurlWaitFD = ^TCurlWaitFD;

  /// low-level write callback function signature for libcurl library API
  curl_write_callback = function (buffer: PAnsiChar; size,nitems: integer;
    outstream: pointer): integer; cdecl;

  /// low-level read callback function signature for libcurl library API
  curl_read_callback = function (buffer: PAnsiChar; size,nitems: integer;
    instream: pointer): integer; cdecl;

  curl_lock_data = (
    CURL_LOCK_DATA_NONE = 0,
    CURL_LOCK_DATA_SHARE,
    CURL_LOCK_DATA_COOKIE,
    CURL_LOCK_DATA_DNS,
    CURL_LOCK_DATA_SSL_SESSION,
    CURL_LOCK_DATA_CONNECT,
    CURL_LOCK_DATA_PSL,
    CURL_LOCK_DATA_HSTS
  );

  curl_lock_access = (
    CURL_LOCK_ACCESS_NONE = 0,
    CURL_LOCK_ACCESS_SHARED = 1,
    CURL_LOCK_ACCESS_SINGLE = 2
  );

  /// lock function signature for csoLockFunc
  curl_lock_function = procedure (handle: TCurl; data: curl_lock_data;
    locktype: curl_lock_access; userptr: pointer); cdecl;
  /// unlock function signature for csoUnLockFunc
  curl_unlock_function = procedure (handle: TCurl; data: curl_lock_data;
    userptr: pointer); cdecl;

{$Z1}

const
  // coErrorBuffer must be at least CURL_ERROR_SIZE bytes big
  CURL_ERROR_SIZE = 256;

  cauNone: TCurlAuths = [];
  cauAll = [low(TCurlAuth) .. high(TCurlAuth)];
  cauAny = cauAll - [cauDigest_IE];
  cauAnySafe = cauAll - [cauBasic, cauDigest_IE];

  // aliases for removed/repurposed/obsolete values
  crURLMalformatUser         = crNotBuiltIn;
  crFtpWeirdServerReply      = crWeirdServerReply;
  crFtpAccessDenied          = crRemoteAccessDenied;
  crFtpUserPasswordIncorrect = crFtpAccessFailed;
  crFtpWeirdUSERReply        = crFtpAcceptTimeout;
  crFtpCantGetHost           = crFtpCannotGetHost;
  crFtpCantReconnect         = crHttp2;
  crFtpCouldNotSetBinary     = crFtpCouldNotSetType;
  crFtpWriteError            = crObsolete20;
  crFtpQuoteError            = crQuoteError;
  crMalFormatUser            = crObsolete24;
  crFtpCouldNotStorFile      = crUploadFailed;
  crFtpCouldNotSetAscii      = crObsolete29;
  crFtpCouldNotGetSize       = crObsolete32;
  crHttpRangeError           = crRangeError;
  crLibraryNotFound          = crObsolete40;
  crBadCallingOrder          = crObsolete44;
  crBadPasswordEntered       = crObsolete46;
  crUnknownTelnetOption      = crUnknownOption;
  crTelnetOptionSyntax       = crSetOptOptionSyntax;
  crSslPeerCertificate       = crObsolete51;
  crShareInUse               = crObsolete57;
  crSslCACert                = crPeerFailedVerification;
  crLdapInvalidURL           = crObsolete62;
  crFtpSslFailed             = crUseSslFailed;
  crTFtpDiskFull             = crRemoteDiskFull;
  crTFtpExists               = crRemoteFileExists;

  coFile               = coWriteData;
  coInFile             = coReadData;
  coSslCertPasswd      = coKeyPasswd;
  coWriteHeader        = coHeaderData;
  coWriteInfo          = coObsolete40;
  coFtpListOnly        = coDirListOnly;
  coFtpAppend          = coAppend;
  coProgressData       = coXferInfoData;
  coKRB4Level          = coKRBLevel;
  coClosePolicy        = coObsolete72;
  coEncoding           = coAcceptEncoding;
  coFtpResponseTimeout = coServerResponseTimeout;
  coFtpSsl             = coUseSsl;

{ ************ CURL Functions API }

const
  /// low-level libcurl library file name, depending on the running OS
  LIBCURL_DLL =
    {$ifdef OSDARWIN}
      'libcurl.dylib';
    {$else} {$ifdef OSWINDOWS}
      {$ifdef CPU64}
        'libcurl-x64.dll';
      {$else}
        'libcurl.dll';
      {$endif CPU64}
    {$else}
      'libcurl.so';
    {$endif OSWINDOWS}
    {$endif OSDARWIN}

type
  /// low-level late binding functions access to the libcurl library API
  // - ensure you called LibCurlInitialize or CurlIsAvailable functions to
  // setup this global instance before using any of its internal functions
  // - see also https://curl.se/libcurl/c/libcurl-multi.html interface
  {$ifdef LIBCURLSTATIC}
  TLibCurl = record
  {$else}
  TLibCurl = class(TSynLibrary)
  {$endif LIBCURLSTATIC}
  public
    /// global TCurlShare object, created by CurlEnableGlobalShare
    globalShare: TCurlShare;
    /// hold CurlEnableGlobalShare mutexes
    share_cs: array[curl_lock_data] of TRTLCriticalSection;
    /// initialize the library
    global_init: function(flags: TCurlGlobalInit): TCurlResult; cdecl;
    /// initialize the library and specify a memory manager
    // - set malloc, free, realloc, strdup and calloc functions
    global_init_mem: function(flags: TCurlGlobalInit;
      m, f, r, s, c: pointer): TCurlResult; cdecl;
    /// finalize the library
    global_cleanup: procedure; cdecl;
    /// returns run-time libcurl version info
    version_info: function(age: TCurlVersion): PCurlVersionInfo; cdecl;
    // start a libcurl easy session
    easy_init: function: pointer; cdecl;
    /// set options for a curl easy handle
    easy_setopt: function(curl: TCurl; option: TCurlOption): TCurlResult; cdecl varargs;
    /// perform a blocking file transfer
    easy_perform: function(curl: TCurl): TCurlResult; cdecl;
    /// end a libcurl easy handle
    easy_cleanup: procedure(curl: TCurl); cdecl;
    /// extract information from a curl handle
    easy_getinfo: function(curl: TCurl; info: TCurlInfo; out value): TCurlResult; cdecl;
    /// clone a libcurl session handle
    easy_duphandle: function(curl: TCurl): pointer; cdecl;
    /// reset all options of a libcurl session handle
    easy_reset: procedure(curl: TCurl); cdecl;
    /// return string describing error code
    easy_strerror: function(code: TCurlResult): PAnsiChar; cdecl;
    /// add a string to an slist
    slist_append: function(list: TCurlSList; s: PAnsiChar): TCurlSList; cdecl;
    /// free an entire slist
    slist_free_all: procedure(list: TCurlSList); cdecl;
    /// add a section to a multipart/formdata HTTP POST request
    formadd: function(var first, last: PCurlHttpPost): TCurlFormCode; cdecl varargs;
    /// finalize the sections of a multipart/formdata HTTP POST request
    formfree: procedure(first: PCurlHttpPost); cdecl;

    /// create a shared object
    share_init: function: pointer; cdecl;
    /// clean up a shared object
    share_cleanup: function(share_handle: TCurlShare): TCurlShareResult; cdecl;
    /// set options for a shared object
    share_setopt: function(share: TCurlShare; option: TCurlShareOption): TCurlShareResult; cdecl varargs;
    /// return the text description of an error code
    share_strerror: function(code: TCurlShareResult): PAnsiChar; cdecl;

    /// create a mime context and return its handle
    mime_init: function(curl: TCurl): TCurlMime; cdecl;
    /// release a mime handle and its substructures
    mime_free: procedure(mime: TCurlMime); cdecl;
    /// append a new empty part to the given mime context and return a handle to the created part
    mime_addpart: function(mime: TCurlMime): TCurlMimePart; cdecl;
    /// set mime/form part data
    mime_data: function(part: TCurlMimePart; data: PAnsiChar; size: integer): TCurlResult; cdecl;
    /// sets a mime part name
    mime_name: function(part: TCurlMimePart; name: PAnsiChar): TCurlResult; cdecl;
    /// sets a mime part content type
    mime_type: function(part: TCurlMimePart; mimetype: PAnsiChar): TCurlResult; cdecl;

    {$ifdef LIBCURLMULTI}
    /// add an easy handle to a multi session
    multi_add_handle: function(mcurl: TCurlMulti; curl: TCurl): TCurlMultiCode; cdecl;
    /// set data to associate with an internal socket
    multi_assign: function(mcurl: TCurlMulti; socket: TCurlSocket; data: pointer): TCurlMultiCode; cdecl;
    /// close down a multi session
    multi_cleanup: function(mcurl: TCurlMulti): TCurlMultiCode; cdecl;
    /// extracts file descriptor information from a multi handle
    multi_fdset: function(mcurl: TCurlMulti; read, write, exec: pointer; out max: integer): TCurlMultiCode; cdecl;
    /// read multi stack informationals
    multi_info_read: function(mcurl: TCurlMulti; out msgsqueue: integer): PCurlMsgRec; cdecl;
    /// create a multi handle
    multi_init: function: TCurlMulti; cdecl;
    /// reads/writes available data from each easy handle
    multi_perform: function(mcurl: TCurlMulti; out runningh: integer): TCurlMultiCode; cdecl;
    /// remove an easy handle from a multi session
    multi_remove_handle: function(mcurl: TCurlMulti; curl: TCurl): TCurlMultiCode; cdecl;
    /// set options for a curl multi handle
    multi_setopt: function(mcurl: TCurlMulti; option: TCurlMultiOption): TCurlMultiCode; cdecl varargs;
    /// reads/writes available data given an action
    multi_socket_action: function(mcurl: TCurlMulti; socket: TCurlSocket; mask: integer; out runningh: integer): TCurlMultiCode; cdecl;
    /// reads/writes available data - deprecated call
    multi_socket_all: function(mcurl: TCurlMulti; out runningh: integer): TCurlMultiCode; cdecl;
    /// return string describing error code
    multi_strerror: function(code: TCurlMultiCode): PAnsiChar; cdecl;
    /// retrieve how long to wait for action before proceeding
    multi_timeout: function(mcurl: TCurlMulti; out ms: integer): TCurlMultiCode; cdecl;
    /// polls on all easy handles in a multi handle
    multi_wait: function(mcurl: TCurlMulti; fds: PCurlWaitFD; fdscount: cardinal; ms: integer; out ret: integer): TCurlMultiCode; cdecl;
    {$endif LIBCURLMULTI}

    /// contains numerical information about the initialized libcurl instance
    // - this is a direct access to the static struct returned by curl_version_info
    info: PCurlVersionInfo;
    /// contains textual information about the initialized libcurl instance
    infoText: string;

    {$ifndef LIBCURLSTATIC}
    /// finalize the library
    destructor Destroy; override;
    {$endif LIBCURLSTATIC}
  end;

var
  /// main access to the libcurl library API
  // - ensure you called LibCurlInitialize or CurlIsAvailable functions to
  // setup this global instance before using any of its internal functions
  curl: TLibCurl;

/// initialize the libcurl API, accessible via the curl global variable
// - do nothing if the library has already been loaded
// - will raise ECurl exception on any loading issue
// - you can specify the libcurl library name to load
procedure LibCurlInitialize(engines: TCurlGlobalInit = [giAll];
  const dllname: TFileName = LIBCURL_DLL);

/// return TRUE if a curl library is available
// - will load and initialize it, calling LibCurlInitialize if necessary,
// catching any exception during the process
function CurlIsAvailable: boolean;

/// Callback used by libcurl to write data, e.g. when downloading a resource
// - Usage:
// ! curl.easy_setopt(fHandle, coWriteFunction, @CurlWriteRawByteString);
// ! curl.easy_setopt(curlHandle, coWriteData, @curlRespBody);
// where curlRespBody should be a generic AnsiString/RawByteString, i.e.
// in practice a RawUtf8 or a RawByteString
function CurlWriteRawByteString(buffer: PAnsiChar; size, nitems: integer;
  opaque: pointer): integer; cdecl;

type
  /// structure which may be passed to CurlReadRawByteString()
  // - via curl.easy_setopt(hnd, coReadData, @Upload);
  TCurlReadState = record
    buf: PAnsiChar;
    size,
    uploaded: integer;
  end;
  PCurlReadState = ^TCurlReadState;

/// Callback used by libcurl to read data, e.g. when uploading a resource
function CurlReadRawByteString(buffer: PAnsiChar; size, nitems: integer;
  opaque: pointer): integer; cdecl;

/// enable libcurl multiple easy handles to share data
// - is called automatically during libcurl initialization
// - shared objects are: DNS cache, TLS session cache and connection cache
// - this way, each single transfer can take advantage of the context of the
// other transfer(s)
// - do nothing if the global share has already been enabled
// - see https://curl.se/libcurl/c/libcurl-share.html for details
function CurlEnableGlobalShare: boolean;

/// disable a global share for libcurl
// - is called automatically in finalization section
// - can be called on purpose, to ensure there is no active HTTP requests
// and prevent csrIN_USE error
// - you can re-enable the libcurl global share by CurlEnableGlobalShare
function CurlDisableGlobalShare: TCurlShareResult;


implementation 

{ ************ CURL Functions API }

{$ifdef LIBCURLSTATIC}

{$ifdef FPC}

  // compiled static library from https://github.com/gcesarmza/curl-android-ios
  {$ifdef OSANDROID}
    {$ifdef CPUAARCH64}
      {$linklib ..\..\static\aarch64-android\libcurl.a}
    {$endif CPUAARCH64}
    {$ifdef CPUARM}
      {$linklib ..\..\static\arm-android\libcurl.a}
    {$endif CPUARM}
    {$linklib libz.so}
  {$endif OSANDROID}

  /// initialize the library
  function curl_global_init(flags: TCurlGlobalInit): TCurlResult; cdecl; external;
  /// initialize the library with a custom memory manager
  function curl_global_init_mem(flags: TCurlGlobalInit;
    m, f, r, s, c: pointer): TCurlResult; cdecl; external;
  /// finalize the library
  procedure curl_global_cleanup cdecl; external;
  /// returns run-time libcurl version info
  function curl_version_info(age: TCurlVersion): PCurlVersionInfo; cdecl; external;
  // start a libcurl easy session
  function curl_easy_init: pointer; cdecl; external;
  /// set options for a curl easy handle
  function curl_easy_setopt(curl: TCurl; option: TCurlOption): TCurlResult; cdecl varargs; external;
  /// perform a blocking file transfer
  function curl_easy_perform(curl: TCurl): TCurlResult; cdecl; external;
  /// end a libcurl easy handle
  procedure curl_easy_cleanup(curl: TCurl); cdecl; external;
  /// extract information from a curl handle
  function curl_easy_getinfo(curl: TCurl; info: TCurlInfo; out value): TCurlResult; cdecl; external;
  /// clone a libcurl session handle
  function curl_easy_duphandle(curl: TCurl): pointer; cdecl; external;
  /// reset all options of a libcurl session handle
  procedure curl_easy_reset(curl: TCurl); cdecl; external;
  /// return string describing error code
  function curl_easy_strerror(code: TCurlResult): PAnsiChar; cdecl; external;
  /// add a string to an slist
  function curl_slist_append(list: TCurlSList; s: PAnsiChar): TCurlSList; cdecl; external;
  /// free an entire slist
  procedure curl_slist_free_all(list: TCurlSList); cdecl; external;
  /// add a section to a multipart/formdata HTTP POST
  function curl_formadd(var first, last: PCurlHttpPost): TCurlFormCode; cdecl varargs; external;
  /// finalize all sections of a multipart/formdata HTTP POST
  procedure curl_formfree(first: PCurlHttpPost); cdecl; external;

  /// create a shared object
  function curl_share_init: pointer; cdecl; external;
  /// clean up a shared object
  function curl_share_cleanup(share_handle: TCurlShare): TCurlShareResult; cdecl; external;
  /// set options for a shared object
  function curl_share_setopt(share: TCurlShare; option: TCurlShareOption): TCurlShareResult; cdecl varargs; external;
  /// return string describing error code
  function curl_share_strerror(code: TCurlShareResult): PAnsiChar;  cdecl; external;

  /// initializes a new mime structure
  function curl_mime_init(curl: TCurl): TCurlMime; cdecl; external;
  /// used to clean up data previously built/appended with curl_mime_addpart and other mime-handling functions
  procedure curl_mime_free(mime: TCurlMime); cdecl; external;
  /// creates and appends a new empty part to the given mime structure
  function curl_mime_addpart(mime: TCurlMime): TCurlMimePart; cdecl; external;
  /// sets a mime part's body content from memory data
  function curl_mime_data(part: TCurlMimePart; data: PAnsiChar; size: integer): TCurlResult; cdecl; external;
  /// sets a mime part's name
  function curl_mime_name(part: TCurlMimePart; name: PAnsiChar): TCurlResult; cdecl; external;
  /// sets a mime part's content type
  function curl_mime_type(part: TCurlMimePart; mimetype: PAnsiChar): TCurlResult; cdecl; external;

  {$ifdef LIBCURLMULTI}
  /// add an easy handle to a multi session
  function curl_multi_add_handle(mcurl: TCurlMulti; curl: TCurl): TCurlMultiCode; cdecl; external;
  /// set data to associate with an internal socket
  function curl_multi_assign(mcurl: TCurlMulti; socket: TCurlSocket; data: pointer): TCurlMultiCode; cdecl; external;
  /// close down a multi session
  function curl_multi_cleanup(mcurl: TCurlMulti): TCurlMultiCode; cdecl; external;
  /// extracts file descriptor information from a multi handle
  function curl_multi_fdset(mcurl: TCurlMulti; read, write, exec: pointer; out max: integer): TCurlMultiCode; cdecl; external;
  /// read multi stack informationals
  function curl_multi_info_read(mcurl: TCurlMulti; out msgsqueue: integer): PCurlMsgRec; cdecl; external;
  /// create a multi handle
  function curl_multi_init: TCurlMulti; cdecl; external;
  /// reads/writes available data from each easy handle
  function curl_multi_perform(mcurl: TCurlMulti; out runningh: integer): TCurlMultiCode; cdecl; external;
  /// remove an easy handle from a multi session
  function curl_multi_remove_handle(mcurl: TCurlMulti; curl: TCurl): TCurlMultiCode; cdecl; external;
  /// set options for a curl multi handle
  function curl_multi_setopt(mcurl: TCurlMulti; option: TCurlMultiOption): TCurlMultiCode; cdecl varargs; external;
  /// reads/writes available data given an action
  function curl_multi_socket_action(mcurl: TCurlMulti; socket: TCurlSocket; mask: integer; out runningh: integer): TCurlMultiCode; cdecl; external;
  /// reads/writes available data - deprecated call
  function curl_multi_socket_all(mcurl: TCurlMulti; out runningh: integer): TCurlMultiCode; cdecl; external;
  /// return string describing error code
  function curl_multi_strerror(code: TCurlMultiCode): PAnsiChar; cdecl; external;
  /// retrieve how long to wait for action before proceeding
  function curl_multi_timeout(mcurl: TCurlMulti; out ms: integer): TCurlMultiCode; cdecl; external;
  /// polls on all easy handles in a multi handle
  function curl_multi_wait(mcurl: TCurlMulti; fds: PCurlWaitFD; fdscount: cardinal; ms: integer; out ret: integer): TCurlMultiCode; cdecl; external;
  {$endif LIBCURLMULTI}

{$endif FPC}

{$endif LIBCURLSTATIC}

function CurlWriteRawByteString(buffer: PAnsiChar; size, nitems: integer;
  opaque: pointer): integer;
var
  storage: PRawByteString absolute opaque;
  n: integer;
begin
  if storage = nil then
    result := 0
  else
  begin
    n := length(storage^);
    result := size * nitems;
    SetLength(storage^, n + result);
    MoveFast(buffer^, PPAnsiChar(opaque)^[n], result);
  end;
end;

function CurlReadRawByteString(buffer: PAnsiChar; size, nitems: integer;
  opaque: pointer): integer;
var
  upload: PCurlReadState absolute opaque;
  available: integer;
begin
  available := upload.size - upload.uploaded;
  if available = 0 then
    result := 0
  else
  begin
    result := size * nitems;
    if result > available then
      result := available;
    MoveFast(upload.buf[upload.uploaded], buffer^, result);
    inc(upload.uploaded, result);
  end;
end;

var
  curl_initialized: boolean;

function CurlIsAvailable: boolean;
begin
  if not curl_initialized then
    // try to initialize with the default library name
    LibCurlInitialize;
  result := {$ifdef LIBCURLSTATIC} true {$else} curl <> nil {$endif};
end;

// ensure libcurl will call our RTL MM, not the libc heap

function curl_malloc_callback(size: PtrInt) : pointer; cdecl;
begin
  GetMem(result, size);
end;

procedure curl_free_callback(ptr: pointer); cdecl;
begin
  FreeMem(ptr);
end;

function curl_realloc_callback(ptr: pointer; size: PtrInt) : pointer; cdecl;
begin
  ReallocMem(ptr, size);
  result := ptr;
end;

function curl_strdup_callback(str: PAnsiChar): PAnsiChar; cdecl;
var
  len: PtrInt;
begin
  len := StrLen(str);
  GetMem(result, len + 1);
  result[len] := #0;
  MoveFast(str^, result^, len);
end;

function curl_calloc_callback(nmemb, size: PtrInt): pointer; cdecl;
begin
  result := AllocMem(size * nmemb);
end;


procedure LibCurlInitialize(engines: TCurlGlobalInit; const dllname: TFileName);

var
  res: TCurlResult;
{$ifndef LIBCURLSTATIC}
  P: PPointerArray;
  api: PtrInt;

const
  NAMES: array[0 .. {$ifdef LIBCURLMULTI} 39 {$else} 25 {$endif}] of RawUtf8 = (
    'global_init',
    'global_init_mem',
    'global_cleanup',
    'version_info',
    'easy_init',
    'easy_setopt',
    'easy_perform',
    'easy_cleanup',
    'easy_getinfo',
    'easy_duphandle',
    'easy_reset',
    'easy_strerror',
    'slist_append',
    'slist_free_all',
    'formadd',
    'formfree',
    'share_init',
    'share_cleanup',
    'share_setopt',
    'share_strerror',
    'mime_init',
    'mime_free',
    'mime_addpart',
    'mime_data',
    'mime_name',
    'mime_type'
    {$ifdef LIBCURLMULTI},
    'multi_add_handle',
    'multi_assign',
    'multi_cleanup',
    'multi_fdset',
    'multi_info_read',
    'multi_init',
    'multi_perform',
    'multi_remove_handle',
    'multi_setopt',
    'multi_socket_action',
    'multi_socket_all',
    'multi_strerror',
    'multi_timeout',
    'multi_wait'
    {$endif LIBCURLMULTI} );

{$endif LIBCURLSTATIC}

begin
  if curl_initialized
     {$ifndef LIBCURLSTATIC} and
     (curl <> nil)
     {$endif LIBCURLSTATIC} then
    exit; // set it once, but allow to retry a given dllname

  GlobalLock;
  try
    if curl_initialized then
      exit;

    {$ifdef LIBCURLSTATIC}

    curl.global_init := @curl_global_init;
    curl.global_init_mem := @curl_global_init_mem;
    curl.global_cleanup := @curl_global_cleanup;
    curl.version_info := @curl_version_info;
    curl.easy_init := @curl_easy_init;
    curl.easy_setopt := @curl_easy_setopt;
    curl.easy_perform := @curl_easy_perform;
    curl.easy_cleanup := @curl_easy_cleanup;
    curl.easy_getinfo := @curl_easy_getinfo;
    curl.easy_duphandle := @curl_easy_duphandle;
    curl.easy_reset := @curl_easy_reset;
    curl.easy_strerror := @curl_easy_strerror;
    curl.slist_append := @curl_slist_append;
    curl.slist_free_all := @curl_slist_free_all;
    curl.formadd := @curl_formadd;
    curl.formfree := @curl_formfree;
    curl.share_init := @curl_share_init;
    curl.share_cleanup := @curl_share_cleanup;
    curl.share_setopt := @curl_share_setopt;
    curl.share_strerror := @curl_share_strerror;
    curl.mime_init := @curl_mime_init;
    curl.mime_free := @curl_mime_free;
    curl.mime_addpart := @curl_mime_addpart;
    curl.mime_data := @curl_mime_data;
    curl.mime_name := @curl_mime_name;
    curl.mime_type := @curl_mime_type;
    {$ifdef LIBCURLMULTI}
    curl.multi_add_handle := @curl_multi_add_handle;
    curl.multi_assign := @curl_multi_assign;
    curl.multi_cleanup := @curl_multi_cleanup;
    curl.multi_fdset := @curl_multi_fdset;
    curl.multi_info_read := @curl_multi_info_read;
    curl.multi_init := @curl_multi_init;
    curl.multi_perform := @curl_multi_perform;
    curl.multi_remove_handle := @curl_multi_remove_handle;
    curl.multi_setopt := @curl_multi_setopt;
    curl.multi_socket_action := @curl_multi_socket_action;
    curl.multi_socket_all := @curl_multi_socket_all;
    curl.multi_strerror := @curl_multi_strerror;
    curl.multi_timeout := @curl_multi_timeout;
    curl.multi_wait := @curl_multi_wait;
    {$endif LIBCURLMULTI}

    {$else}

    curl := TLibCurl.Create;
    try
      curl.TryLoadLibrary([
      {$ifdef OSWINDOWS}
        // first try the libcurl.dll in the local executable folder
        Executable.ProgramFilePath + dllname,
      {$endif OSWINDOWS}
        // search standard library in path
        dllname
      {$ifdef OSDARWIN}
        // another common names on MacOS
        , 'libcurl.4.dylib', 'libcurl.3.dylib'
      {$else}
        {$ifdef OSPOSIX}
        // another common names on POSIX
        , 'libcurl.so.4', 'libcurl.so.3'
        // for latest Linux Mint and other similar distros using gnutls
        , 'libcurl-gnutls.so.4', 'libcurl-gnutls.so.3'
        {$endif OSPOSIX}
      {$endif OSDARWIN}
        ], ECurl);
      P := @@curl.global_init;
      for api := low(NAMES) to high(NAMES) do
        curl.Resolve('curl_', NAMES[api], @P[api], {onfailure=}ECurl);
    except
      FreeAndNil(curl); // ECurl raised during initialization above
      exit;
    end;

    {$endif LIBCURLSTATIC}

    // if we reached here, the library has been successfully loaded
    res := curl.global_init_mem(engines, @curl_malloc_callback, @curl_free_callback,
      @curl_realloc_callback, @curl_strdup_callback, @curl_calloc_callback);
    if res <> crOK then
      raise ECurl.CreateFmt('curl_global_init_mem() failed as %d', [ord(res)]);
    curl.info := curl.version_info(cv11); // direct static assign
    curl.infoText := format('%s version %s', [LIBCURL_DLL, curl.info^.version]);
    if curl.info^.ssl_version <> nil then
      curl.infoText := format('%s using %s', [curl.infoText, curl.info^.ssl_version]);
    curl_initialized := true; // should be set last but before CurlEnableGlobalShare

    curl.globalShare := nil;
    CurlEnableGlobalShare; // won't hurt, and may benefit even for the OS
    // api := 0; with curl.info^ do while protocols[api]<>nil do
    // begin write(protocols[api], ' '); inc(api); end; writeln(#13#10,curl.infoText);
  finally
    GlobalUnLock;
  end;
end;

procedure curlShareLock(handle: TCurl; data: curl_lock_data;
  locktype: curl_lock_access; userptr: pointer); cdecl;
begin
  EnterCriticalSection(curl.share_cs[data]);
end;

procedure curlShareUnLock(handle: TCurl; data: curl_lock_data;
  userptr: pointer); cdecl;
begin
  LeaveCriticalSection(curl.share_cs[data]);
end;

function CurlEnableGlobalShare: boolean;
var
  d: curl_lock_data;
begin
  result := false;
  if not CurlIsAvailable or 
     (curl.globalShare <> nil) then
    exit; // not available, or already shared
  curl.globalShare := curl.share_init;
  if curl.globalShare = nil then
    // something went wrong (out of memory, etc.) and therefore
    // the share object was not created
    exit;
  for d := low(d) to high(d) do
    InitializeCriticalSection(curl.share_cs[d]);
  curl.share_setopt(curl.globalShare, csoLockFunc, @curlShareLock);
  curl.share_setopt(curl.globalShare, csoUnLockFunc, @curlShareUnLock);
  // share and cache DNS + TLS sessions (but not Connections)
  curl.share_setopt(curl.globalShare, csoShare, CURL_LOCK_DATA_DNS);
  curl.share_setopt(curl.globalShare, csoShare, CURL_LOCK_DATA_SSL_SESSION);
  // CURL_LOCK_DATA_CONNECT triggers GPF e.g. on Debian Burster 10
  if curl.info^.version_num >= $00074400 then // seems to be fixed in 7.68
    // see https://github.com/curl/curl/issues/4544
    curl.share_setopt(curl.globalShare, csoShare, CURL_LOCK_DATA_CONNECT);
  result := true;
end;

function CurlDisableGlobalShare: TCurlShareResult;
var
  d: curl_lock_data;
begin
  result := csrOK;
  if {$ifndef LIBCURLSTATIC}(curl = nil) or{$endif}
     (curl.globalShare = nil) then
    exit; // already disabled
  result := curl.share_cleanup(curl.globalShare);
  if result = csrOK then
    curl.globalShare := nil;
  for d := low(d) to high(d) do
    DeleteCriticalSection(curl.share_cs[d]);
end;

{$ifndef LIBCURLSTATIC}
destructor TLibCurl.Destroy;
begin
  CurlDisableGlobalShare;
  if curl <> nil then
    curl.global_cleanup;
end;
{$endif LIBCURLSTATIC}


initialization
  assert(ord(crUnrecoverablePoll) = 99); // CURLE_UNRECOVERABLE_POLL
  assert(ord(cvfThreadsafe) = 30);

finalization
  {$ifdef LIBCURLSTATIC}
  if curl_initialized then
  begin
    CurlDisableGlobalShare;
    curl.global_cleanup;
  end;
  {$else}
  curl.Free;
  {$endif LIBCURLSTATIC}

end.

