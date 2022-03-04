/// TFTP Protocol and Client-Side Process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.tftp.client;

{
  *****************************************************************************

    TFTP Protocol and Client with RFC 1350/2347/2348/2349/7440 Support
    - TFTP Protocol Definitions
    - TClientTftp Client Connection Class

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode, // for efficient UTF-8 text process within HTTP
  mormot.core.text,
  mormot.core.data,
  mormot.core.log,
  mormot.core.threads,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.buffers,
  mormot.net.sock;




{ ******************** TFTP Protocol Definitions }

type
  /// the TFTP frame content, matching RFC1350/2347 definition
  // - toOck replaces toAck when RFC2347 Option Extensions are negotiated
  TTftpOpcode = (
    toUndefined,
    toRrq,
    toWrq,
    toDat,
    toAck,
    toErr,
    toOck);

  /// the TFTP frame error codes, matching RFC1350/2347 definition
  // - teInvalidOptionNegotiation is used on RFC2347 Option Extensions
  TTftpError = (
    teUndefined,
    teFileNotFound,
    teAccessViolation,
    teDiskFull,
    teIllegalOperation,
    teUnknownTransferID,
    teFileAlreadyExists,
    teNoSuchUser,
    teInvalidOptionNegotiation); // RFC2347 Option Extension

const
  TFTP_RRQ = 1;
  TFTP_WRQ = 2;
  TFTP_DAT = 3;
  TFTP_ACK = 4;
  TFTP_ERR = 5;
  TFTP_OACK = 6; // RFC2347

  toLast = toErr;
  teLast = teInvalidOptionNegotiation;

  /// the TFTP frame content, as text
  TFTP_OPCODE: array[TTftpOpcode] of string[5] = (
    '??? ',
    'RRQ ',
    'WRQ ',
    'DAT ',
    'ACK ',
    'ERR ',
    'OACK ');

  /// RFC2348 minimum supported TFTP block size
  // - original RFC1350 defines 512 bytes, but we support RFC2348 blksize option
  TFTP_MINSIZE = 8;

  /// RFC2348 maximum supported TFTP block size
  // - original RFC1350 defines 512 bytes, but we support RFC2348 blksize option
  // - in theory, blksize option allows up to 64 KB of data
  // - but TFTP_MTUSIZE is recommended, to avoid IP fragmentation
  TFTP_MAXSIZE = 65464;

  /// RFC1350 default TFTP block size
  TFTP_DEFAULTSIZE = 512;

  /// RFC2348 optimal supported TFTP maximum block size
  // - original RFC1350 defines 512 bytes, but we support RFC2348 blksize option
  // - best is up to 1428 bytes, i.e. default Ethernet MTU, minus the TFTP, UDP
  // and IP headers to avoid IP fragmentation at transmission level
  TFTP_MTUSIZE = 1428;

  /// default TTftpContext.TransferSize value, if no "tsize" option was defined
  TFTP_NOTRANSFERSIZE = cardinal(-1);

  /// RFC1350 default TTftpContext.TimeoutSec value
  TFTP_DEFAULTTIMEOUT = 5;

  /// RFC1350 default TTftpContext.WindowSize value
  TFTP_DEFAULTWINDOWSIZE = 1;


type
  {$A-}
  /// map a TFTP frame as transmitted over UDP, including the Data block
  TTftpFrame = packed record
    case
      /// the frame operation code
      // - encoded as big-endian on the wire
      // - swap(Opcode) can be TFTP_RRQ, TFTP_WRQ, TFTP_DAT, TFTP_ACK, TFTP_OACK
      // and TFTP_ERR: use ToOpcode(frame) to decode it as a TTftpOpcode
      Opcode: word of
        TFTP_RRQ,
        TFTP_WRQ,
        TFTP_OACK:
          (
            /// the #0 terminated FileName, Mode and options fields
            Header: array[0 .. TFTP_MAXSIZE + 1] of byte;
          );
        TFTP_DAT,
        TFTP_ACK,
        TFTP_ERR:
          (
            /// the block sequence number
            Sequence: word;
            /// data block content - up to 64KB of data is theoritically possible
            Data: array[0 .. TFTP_MAXSIZE - 1] of byte;
          );
  end;
  {$A+}

  /// points to a TTftpFrame memory buffer
  PTftpFrame = ^TTftpFrame;

  /// the TFTP Context, following RFC 1350/2347/2348/2349/7440 specifications
  // - stores its own 64KB processing buffer
  TTftpContext = class
  public
    /// the request opcode, as filled by SetFromFrame or set before AsFrame
    OpCode: TTftpOpcode;
    /// the RFC 2349 "timeout" option in seconds, in range 1..255
    // - equals 5 by default if no option was specified
    TimeoutSec: cardinal;
    /// the RFC 2348 "blksize" option, in range TFTP_MINSIZE..TFTP_MAXSIZE
    // - equals TFTP_DEFAULTSIZE = 512 if no option was specified
    BlockSize: cardinal;
    /// the RFC 2349 "tsize" option, as positive cardinal
    // - equals TFTP_NOTRANSFERSIZE = -1 if no option was specified
    TransferSize: cardinal;
    /// the RFC 7440 "windowsize" option, in range 1..65535
    // - equals 1 (which is RFC1350 compatible) if no option was specified
    WindowSize: cardinal;
    /// current sequence number
    CurrentSequence: cardinal;
    /// current processed size
    CurrentSize: cardinal;
    /// size of the processing frame
    FrameLen: integer;
    /// the transmitted file name, UTF-8 encoded
    FileName: RawUtf8;
    /// the processing frame - with up to 64KB of content
    Frame: TTftpFrame;
    /// set the default TimeoutSec/BlockSize/TransferSize/WindowSize values
    procedure SetDefaultOptions(op: TTftpOpcode);
    /// append some text to Frame/FrameLen
    procedure AppendTextToFrame(const Text: RawUtf8);
    /// parse the initial RRQ/WRQ/OACK Frame/FrameLen on server side
    // - return false on error
    // - for RRQ/WRQ, return true and set OpCode/FileName and the toAck/toOck
    // response within the Frame/FrameLen buffer
    // - for OACK, return true and set OpCode and override options values
    function ParseFrame: boolean;
    /// generate the initial RRQ/WRQ request Frame/FrameLen on client side
    // - following current OpCode/FileName and the current options
    procedure GenerateRequestFrame;
    /// generate an ERR packet in Frame/FrameLen
    procedure GenerateErrorFrame(err: TTftpError; const msg: RawUtf8);
  end;


/// decode the TFTP frame type as TTftpOpcode
function ToOpcode(const frame: TTftpFrame): TTftpOpcode;
  {$ifdef HASINLINE} inline; {$endif}

/// convert TFTP frame Opcode + Sequence to text, ready for logging
function ToText(const frame: TTftpFrame; len: integer = 0): shortstring; overload;

function ToText(e: TTftpError): PShortString; overload;


{ ******************** TClientTftp Client Connection Class }

implementation


function ToOpcode(const frame: TTftpFrame): TTftpOpcode;
begin
  result := TTftpOpcode(Swap(frame.Opcode));
  if result > toLast then
    result := toUndefined;
end;

function ToText(const frame: TTftpFrame; len: integer): shortstring;
var
  c: TTftpOpcode;
  seq: integer;
begin
  c := ToOpcode(frame);
  result := TFTP_OPCODE[c];
  if c = toUndefined then
  begin
    AppendShortInteger(frame.Opcode, result);
    exit;
  end;
  dec(len, SizeOf(Frame.Opcode));
  seq := swap(frame.Sequence);
  case c of
    toRrq,
    toWrq:
      if len <= 0 then
        /// 'RRQ filename' / 'WRQ filename'
        AppendShortBuffer(@frame.Header, StrLen(@frame.Header), result)
      else
        // all options will be included with #0 terminated (logged as space)
        AppendShortBuffer(@frame.Header, len, result);
    toDat,
    toAck:
      begin
        /// 'DAT 123,len' / 'ACK 123'
        AppendShortInteger(seq, result);
        dec(len, SizeOf(Frame.Sequence));
        if (len >= 0) and
           (c = toDat) then
        begin
          AppendShortChar(',', result);
          AppendShortInteger(len, result);
        end;
      end;
    toErr:
      begin
        AppendShortInteger(seq, result);
        if seq <= ord(teLast) then
        begin
          AppendShort(' (', result);
          AppendShort(GetEnumName(TypeInfo(TTftpError), seq)^, result);
          AppendShort(') ', result);
          AppendShortBuffer(@frame.Header, StrLen(@frame.Header), result)
        end;
      end;
  end;
end;

function ToText(e: TTftpError): PShortString;
begin
  result := GetEnumName(TypeInfo(TTftpError), ord(e));
end;



{ TTftpContext }

procedure TTftpContext.SetDefaultOptions(op: TTftpOpcode);
begin
  OpCode := op;
  TimeoutSec := TFTP_DEFAULTTIMEOUT;
  BlockSize := TFTP_DEFAULTSIZE; // = 512
  TransferSize := TFTP_NOTRANSFERSIZE; // = -1
  WindowSize := TFTP_DEFAULTWINDOWSIZE; // as before RFC7440
end;

procedure TTftpContext.AppendTextToFrame(const Text: RawUtf8);
var
  l: PtrInt;
begin
  l := length(Text) + 1; // include trailing #0
  MoveFast(pointer(Text)^, PByteArray(@Frame)[FrameLen], l);
  inc(FrameLen, l);
end;

const
  TFTP_OPTIONS: array[0.. 4] of PAnsiChar = (
    'timeout',
    'blksize',
    'tsize',
    'windowsize',
    nil);

function TTftpContext.ParseFrame: boolean;
var
  len: integer;
  P: PUtf8Char;
  v: RawUtf8;

  function GetNext: boolean;
  var
    vlen: PtrInt;
  begin
    vlen := StrLen(P);
    dec(len, vlen);
    if len <= 0 then
      result := false
    else
    begin
      FastSetString(v, P, vlen);
      inc(P, vlen);
      result := true;
    end;
  end;

  function GetNextCardinal(min, max: cardinal; out c: cardinal): boolean;
  begin
    result := GetNext and
              ToCardinal(v, c, min) and
              ({%H-}c <= max);
  end;

begin
  result := false; // error if exit
  SetDefaultOptions(ToOpcode(Frame));
  dec(FrameLen, SizeOf(Frame.Opcode));
  if (FrameLen <= 0) or
     (Frame.Header[FrameLen] <> 0) or // should end with a trailing #0
     not (OpCode in [toRrq, toWrq, toOck]) then
    exit;
  P := @Frame.Header;
  len := FrameLen;
  if not GetNext then
    exit;
  FileName := v;
  if OpCode <> toOck then
    if not GetNext or
       not IdemPropNameU(v, 'octet') then
      exit;
  FrameLen := SizeOf(Frame.Opcode) + SizeOf(Frame.Sequence);
  if GetNext then
  begin
    // RFC 2347 Option Extension with its OACK
    if OpCode <> toOck then
      Frame.Opcode := swap(word(TFTP_OACK));
    repeat
      if OpCode <> toOck then
        AppendTextToFrame(v); // option name
      case IdemPPChar(pointer(v), @TFTP_OPTIONS) of
        0:
          if not GetNextCardinal(1, 255, TimeoutSec) then
            exit;
        1:
          if not GetNextCardinal(TFTP_MINSIZE, TFTP_MAXSIZE, BlockSize) then
            exit;
        2:
          if not GetNextCardinal(0, MaxInt, TransferSize) then
            exit;
        3:
          if not GetNextCardinal(1, 65535, WindowSize) then
            exit;
      else
        exit; // unsupported option
      end;
      if OpCode <> toOck then
        AppendTextToFrame(v); // option value
    until not GetNext;
  end
  else if OpCode <> toOck then
    // RFC 1350 regular ACK
    Frame.Opcode := swap(word(TFTP_ACK));
  Frame.Sequence := 0;
  result := true; // success
end;

procedure TTftpContext.GenerateRequestFrame;

  procedure Append(const name: RawUtf8; value, default: cardinal);
  begin
    if value = default then
      exit;
    AppendTextToFrame(name);
    AppendTextToFrame(UInt32ToUtf8(value));
  end;

begin
  Frame.Opcode := swap(word(ord(OpCode)));
  FrameLen := SizeOf(Frame.OpCode);
  AppendTextToFrame(FileName);
  AppendTextToFrame('octet');
  Append('timeout',    TimeoutSec,   TFTP_DEFAULTTIMEOUT);
  Append('blksize',    BlockSize,    TFTP_DEFAULTSIZE);
  Append('tsize',      TransferSize, TFTP_NOTRANSFERSIZE);
  Append('windowsize', WindowSize,   TFTP_DEFAULTWINDOWSIZE);
end;

procedure TTftpContext.GenerateErrorFrame(err: TTftpError; const msg: RawUtf8);
begin
  Frame.Opcode := TFTP_ERR;
  Frame.Sequence := CurrentSequence;
  FrameLen := SizeOf(Frame.Opcode) + SizeOf(Frame.Sequence);
  if msg <> '' then
    AppendTextToFrame(msg)
  else
    // if no error message is set, use RTTI to generate something in English
    AppendTextToFrame(GetEnumNameUnCamelCase(TypeInfo(TTftpError), ord(err)));
end;



initialization
  assert(SizeOf(TTftpFrame) = 4 + TFTP_MAXSIZE);

end.

