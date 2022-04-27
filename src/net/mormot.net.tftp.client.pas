/// TFTP Protocol and Client-Side Process
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.tftp.client;

{
  *****************************************************************************

    TFTP Protocol and Client with RFC 1350/2347/2348/2349/7440 Support
    - TFTP Protocol Definitions
    - TClientTftp Client Connection Class (not yet implemented)

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
  mormot.core.rtti,
  mormot.core.buffers,
  mormot.core.log,
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
  // - teFinished is an internal success flag for "Normal Termination" as
  // detailed in RFC1350 #6, and never transmitted
  TTftpError = (
    teNoError,
    teFileNotFound,
    teAccessViolation,
    teDiskFull,
    teIllegalOperation,
    teUnknownTransferID,
    teFileAlreadyExists,
    teNoSuchUser,
    teInvalidOptionNegotiation,
    teFinished);

const
  TFTP_RRQ  = 1;
  TFTP_WRQ  = 2;
  TFTP_DAT  = 3;
  TFTP_ACK  = 4;
  TFTP_ERR  = 5;
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

  /// RFC2348 maximum supported TFTP block size
  // - original RFC1350 defines 512 bytes, but we support RFC2348 blksize option
  // - in theory, blksize option allows up to 64 KB of data
  // - but TFTP_MTUSIZE is recommended, to avoid IP fragmentation
  TFTP_MAXSIZE = 65464;

  /// RFC1350 default TFTP block size
  TFTP_DEFAULTSIZE = 512;

  /// RFC2348 optimal supported TFTP maximum block size
  // - original RFC1350 defines 512 bytes, but we support RFC2348 blksize option
  // - best is up to 1468 bytes, i.e. default Ethernet MTU (1500), minus the
  // TFTP, UDP and IP headers to avoid IP fragmentation at transmission level
  TFTP_MTUSIZE = 1468;

  /// default TTftpContext.TransferSize value, if no "tsize" option was defined
  TFTP_NOTRANSFERSIZE = cardinal(-1);

  /// RFC1350 default TTftpContext.TimeoutSec value
  TFTP_DEFAULTTIMEOUT = 5;

  /// RFC1350 default TTftpContext.WindowSize value
  TFTP_DEFAULTWINDOWSIZE = 1;


type
  /// the TFTP sequence number
  // - we use a 16-bit word to allow overflow, as most implementations do
  TTftpSequence = type word;

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
        TFTP_ACK:
          (
            /// the block sequence number
            Sequence: TTftpSequence;
            /// data block content - up to 64KB of data is theoritically possible
            Data: array[0 .. TFTP_MAXSIZE - 1] of byte;
          );
        TFTP_ERR:
          (
            /// the TTftpError value, netword-ordered
            ErrorCode: word;
            /// the #0 terminated Error message
            ErrorMsg: array[0 .. 511] of byte;
          );
  end;
  {$A+}

  /// points to a TTftpFrame memory buffer
  PTftpFrame = ^TTftpFrame;

  /// points to a TTftpContext processing buffer
  PTftpContext = ^TTftpContext;

  /// the TFTP Context, following RFC 1350/2347/2348/2349/7440 specifications
  // - access an external 64KB processing buffer
  {$ifdef USERECORDWITHMETHODS}
  TTftpContext = record
  {$else}
  TTftpContext = object
  {$endif USERECORDWITHMETHODS}
  private
    Parse: PUtf8Char;
    ParseLen: PtrInt;
    Parsed: RawUtf8;
    function GetNext: boolean;
    function GetNextCardinal(min, max: cardinal; out c: cardinal): boolean;
  public
    /// the toRrq/toWrq request opcode
    // - as filled by ParseRequest()
    OpCode: TTftpOpcode;
    /// the expected toDat/toAck input opcode, as filled after ParseRequest
    OpDataAck: TTftpOpcode;
    /// ParseRequest will identify RFC 2347 option extensions
    HasExtendedOptions: boolean;
    /// the number of retries after a timeout
    RetryCount: byte;
    /// the RFC 2349 "timeout" option in seconds, in range 1..255
    // - equals 5 by default if no option was specified
    TimeoutSec: cardinal;
    /// the RFC 2348 "blksize" option, in range TFTP_MINSIZE..TFTP_MAXSIZE
    // - equals TFTP_DEFAULTSIZE = 512 if no option was specified
    BlockSize: cardinal;
    /// the RFC 2349 "tsize" option, as positive cardinal
    // - equals TFTP_NOTRANSFERSIZE = -1 if no option was specified
    // - for RRQ, contains the expected final CurrentSize value
    TransferSize: cardinal;
    /// the RFC 7440 "windowsize" option, in range 1..65535
    // - equals 1 (which is RFC1350 compatible) if no option was specified
    WindowSize: cardinal;
    /// current processed size
    CurrentSize: cardinal;
    /// the sequence number of the last DAT fragment which has been received
    // - as transmitted over the wire
    LastReceivedSequence: TTftpSequence;
    /// the sequence number of the last DAT fragment which has been acknowledged
    // - may be not in synch with LastReceivedSequence if WindowSize > 1
    // - server should retry after TimeoutTix
    LastAcknowledgedSequence: TTftpSequence;
    /// RFC 7440 "windowsize" counter, set to WindowSize at each ACK
    LastReceivedSequenceWindowCounter: cardinal;
    /// upper 16-bit mask of the sequence number of the last DAT fragment received
    // - used to compute the correct processed size after ACK on block overflow
    LastReceivedSequenceHi: cardinal;
    /// the GetTickCount64 value which made a timeout - as set by SetTimeoutTix
    TimeoutTix: Int64;
    /// the transmitted file name, UTF-8 encoded
    FileName: RawUtf8;
    /// the transmitted file content, as a TStream
    FileStream: TStream;
    /// the UDP/IP connection to be used by SendFrame() method
    Sock: TNetSocket;
    /// size of the processing frame
    FrameLen: integer;
    /// the processing frame - with up to 64KB of content
    // - should have been set before calling SetDefaultOptions
    Frame: PTftpFrame;
    /// the remote host address which initiated the request
    // - this big field is included last, for better code generation
    // - includes the ephemeral port, so allow multiple connections per host
    Remote: TNetAddr;
    /// set the default TimeoutSec/BlockSize/TransferSize/WindowSize values
    procedure SetDefaultOptions(op: TTftpOpcode);
    /// append some text to Frame/FrameLen
    procedure AppendTextToFrame(const Text: RawUtf8);
    /// parse the initial RRQ/WRQ/OACK frame buffer/len
    // - Remote and Socket should have been set before calling this method
    // - first call for server side process, to initialize the state machine
    // - returns teIllegalOperation on parsing error
    // - for RRQ/WRQ, return teNoError and set OpCode/FileName fields
    // - for OACK, return teNoError and set OpCode
    // - after teNoError, caller should then call ParseRequestOptions()
    function ParseRequestFileName(len: integer): TTftpError;
    /// parse the options of RRQ/WRQ/OACK frame buffer/len
    // - caller should have called ParseRequestFileName() then set FileStream
    // - for RRQ/WRQ, the toAck/toOck response within the Frame/FrameLen buffer,
    // reading the first RRQ data packet from the associated FileStream
    // - for OACK, set OpCode and override options values
    // - return teInvalidOptionNegotiation if options are out of range or unknown
    function ParseRequestOptions: TTftpError;
    /// parse a DAT/ACK Frame/FrameLen then generate the ACK/DAT answer
    // - for DAT (writing request), return teNoError and call GenerateAckFrame
    // - for ACK (reading request), return teNoError and call GenerateNextDataFrame
    // or return
    function ParseData(len: integer): TTftpError;
    /// generate an ACK datagram in Frame/FrameLen
    procedure GenerateAckFrame;
    /// generate a DAT datagram in Frame/FrameLen, calling OnFileData
    procedure GenerateNextDataFrame;
    /// generate the initial RRQ/WRQ request Frame/FrameLen on client side
    // - following current OpCode/FileName and the current options
    procedure GenerateRequestFrame;
    /// generate an ERR packet in Frame/FrameLen
    procedure GenerateErrorFrame(err: TTftpError; const msg: RawUtf8);
    /// compute TimeOutTix from TimeoutSec
    procedure SetTimeoutTix;
    /// send Frame/FrameLen to Remote address over Sock, and set TimeoutTix
    function SendFrame: TNetResult;
    /// generate and send an ERR packet, then close Sock and FileStream
    procedure SendErrorAndShutdown(err: TTftpError; log: TSynLog;
      obj: TObject; const caller: shortstring);
    /// close Sock and FileStream
    procedure Shutdown;
  end;

  TTftpContextDynArray = array of TTftpContext;


/// decode the TFTP frame type as TTftpOpcode
function ToOpcode(const frame: TTftpFrame): TTftpOpcode;
  {$ifdef HASINLINE} inline; {$endif}

/// convert TFTP frame Opcode + Sequence to text, ready for logging
function ToText(const frame: TTftpFrame; len: integer = 0): shortstring; overload;

function ToText(e: TTftpError): PShortString; overload;


{ ******************** TClientTftp Client Connection Class }


implementation


{ ******************** TFTP Protocol Definitions }

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
    toWrq,
    toOck:
      if len <= 0 then
        // 'RRQ filename' / 'WRQ filename' / 'OACK option'
        AppendShortBuffer(@frame.Header, StrLen(@frame.Header), result)
      else
      begin
        // all options will be included with #0 terminated (logged as space)
        if len > 240 then
          len := 240; // ensure at least beginning of frame is logged
        AppendShortBuffer(@frame.Header, len, result);
      end;
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
  HasExtendedOptions := false;
  RetryCount := 0;
  TimeoutSec := TFTP_DEFAULTTIMEOUT;    // = 5
  BlockSize := TFTP_DEFAULTSIZE;        // = 512
  TransferSize := TFTP_NOTRANSFERSIZE;  // = -1
  WindowSize := TFTP_DEFAULTWINDOWSIZE; // = 1 as before RFC7440
  LastReceivedSequenceWindowCounter := WindowSize;
  LastReceivedSequence := 0;
  LastReceivedSequenceHi := 0;
  LastAcknowledgedSequence := 0;
  CurrentSize := 0;
end;

procedure TTftpContext.AppendTextToFrame(const Text: RawUtf8);
var
  len: PtrInt;
begin
  len := length(Text) + 1; // include trailing #0
  if FrameLen + len > SizeOf(Frame^) then
    exit; // paranoid
  MoveFast(pointer(Text)^, PByteArray(Frame)[FrameLen], len);
  inc(FrameLen, len);
end;

function TTftpContext.GetNext: boolean;
var
  vlen: PtrInt;
begin
  vlen := StrLen(Parse);
  dec(ParseLen, vlen);
  if ParseLen <= 0 then
    result := false
  else
  begin
    FastSetString(Parsed, Parse, vlen);
    inc(Parse, vlen + 1);
    dec(ParseLen);
    result := true;
  end;
end;

function TTftpContext.GetNextCardinal(min, max: cardinal; out c: cardinal): boolean;
begin
  result := GetNext and
            ToCardinal(Parsed, c, min) and
            ({%H-}c <= max);
end;

function TTftpContext.ParseRequestFileName(len: integer): TTftpError;
begin
  result := teIllegalOperation; // error on exit exit
  if (@self = nil) or
     (Frame = nil) or
     (len < 4) then
    exit;
  SetDefaultOptions(ToOpcode(Frame^));
  FrameLen := len;
  ParseLen := len - SizeOf(Frame^.Opcode);
  if (ParseLen <= 0) or
     (ParseLen > SizeOf(Frame.Header)) or
     (Frame^.Header[ParseLen - 1] <> 0) or // should end with a trailing #0
     not (OpCode in [toRrq, toWrq, toOck]) then
    exit;
  Parse := @Frame^.Header;
  if not GetNext then
    exit;
  FileName := Parsed;
  if OpCode <> toOck then
    if not GetNext or
       not IdemPropNameU(Parsed, 'octet') then // supports only 8-bit transfer
      exit;
  result := teNoError;
  // caller should now set the FileStream field, and call ParseRequestOptions
end;

const
  TFTP_OPTIONS: array[0.. 4] of PAnsiChar = (
    'TIMEOUT',
    'BLKSIZE',
    'TSIZE',
    'WINDOWSIZE',
    nil);

function TTftpContext.ParseRequestOptions: TTftpError;
begin
  // caller should have set the FileStream from FileName parsed field
  if GetNext then
  begin
    // RFC 2347 Option Extension with its OACK
    result := teInvalidOptionNegotiation;
    HasExtendedOptions := true;
    FrameLen := SizeOf(Frame^.Opcode);
    repeat
      if OpCode <> toOck then
        AppendTextToFrame(Parsed); // include option name to OACK answer
      case IdemPPChar(pointer(Parsed), @TFTP_OPTIONS) of
        0:
          if not GetNextCardinal(1, 255, TimeoutSec) then
            exit;
        1:
          if not GetNextCardinal(8, TFTP_MAXSIZE, BlockSize) then
            exit;
        2:
          if not GetNextCardinal(0, MaxInt, TransferSize) then // up to 2GB
            exit
          else if OpCode = toRrq then
          begin
            // compute and send back the actual RRQ file size in OACK
            TransferSize := FileStream.Size;
            Parsed := 'tsize'#0 + UInt32ToUtf8(TransferSize); // no #0 yet
          end;
        3:
          if not GetNextCardinal(1, 65535, WindowSize) then
            exit
          else
            LastReceivedSequenceWindowCounter := WindowSize;
      else
        exit; // unsupported option
      end;
      if OpCode <> toOck then
        AppendTextToFrame(Parsed); // include option value to OACK answer
    until not GetNext;
    if OpCode <> toOck then
      //        2 bytes  string   1b   string  1b   string  1b   string  1b
      //       +-------+---~~---+---+---~~---+---+---~~---+---+---~~---+---+
      // OACK |   6   |  opt1  | 0 | value1 | 0 |  optN  | 0 | valueN | 0 |
      //      +-------+---~~---+---+---~~---+---+---~~---+---+---~~---+---+
      Frame^.Opcode := swap(word(TFTP_OACK));
  end
  else
    // RFC 1350 regular response
    case OpCode of
      toWrq:
        // RFC 1350 regular ACK
        GenerateAckFrame;
      toRrq:
        GenerateNextDataFrame;
    end;
  // request success
  result := teNoError;
  case OpCode of
    toWrq:
      OpDataAck := toDat;
    toRrq:
      begin
        if TransferSize = TFTP_NOTRANSFERSIZE then
          TransferSize := FileStream.Size;
        OpDataAck := toAck;
      end;
  end;
end;

function TTftpContext.ParseData(len: integer): TTftpError;
var
  op: TTftpOpcode;
begin
  result := teIllegalOperation;
  op := ToOpCode(Frame^);
  dec(len, 4);
  if (len < 0) or
     (op <> OpDataAck) or
     (FileStream = nil) then
    exit;
  Frame^.Sequence := swap(Frame^.Sequence);
  CurrentSize := (cardinal(Frame^.Sequence) * BlockSize) +
                   LastReceivedSequenceHi; // allow retry from other side
  case op of
    toDat: // during WRQ request
      begin
        if LastReceivedSequence <> Frame^.Sequence then
        begin
          LastReceivedSequence := Frame^.Sequence;
          if Frame^.Sequence = 0 then
            inc(LastReceivedSequenceHi, 1 shl 16); // 16-bit overflow
        end;
        FileStream.WriteBuffer(Frame^.Data, len);
        if len < integer(BlockSize) then
        begin
          if len < 0 then
            result := teDiskFull
          else
            result := teFinished; // Normal Termination
          exit;
        end;
        dec(LastReceivedSequenceWindowCounter);
        if LastReceivedSequenceWindowCounter = 0 then
        begin
          LastReceivedSequenceWindowCounter := WindowSize;
          GenerateAckFrame;
        end
        else
          FrameLen := 0; // no ACK to follow RFC7440 "windowsize" option
      end;
    toAck: // during RRQ request
      begin
        LastAcknowledgedSequence := Frame^.Sequence;
        LastReceivedSequenceWindowCounter := WindowSize;
        if CurrentSize > TransferSize then // >= would miss last block w/ len=0
        begin
          result := teFinished; // Normal Termination
          exit;
        end;
        GenerateNextDataFrame;
      end;
  else
    exit;
  end;
  result := teNoError;
end;

procedure TTftpContext.GenerateAckFrame;
begin
  //        2 bytes   2 bytes
  //        ------------------
  //  ACK  | 04    |  seq    |
  //       ------------------
  Frame^.Opcode := swap(word(TFTP_ACK));
  Frame^.Sequence := swap(LastReceivedSequence);
  FrameLen := SizeOf(Frame^.Opcode) + SizeOf(Frame^.Sequence);
end;

procedure TTftpContext.GenerateNextDataFrame;
begin
  //      2 bytes     2 bytes      n bytes
  //      ----------------------------------
  // DAT | 03    |   Block #  |   Data     |
  //     ----------------------------------
  inc(LastReceivedSequence);
  if LastReceivedSequence = 0 then
    inc(LastReceivedSequenceHi, 1 shl 16);
  Frame^.Opcode := swap(word(TFTP_DAT));
  Frame^.Sequence := swap(LastReceivedSequence);
  if CurrentSize <> FileStream.Position then
    FileStream.Seek(Int64(CurrentSize), soBeginning);
  FrameLen := FileStream.Read(Frame^.Data,  BlockSize);
  // FrameLen=0 is possible for last block
  inc(FrameLen, SizeOf(Frame^.Opcode) + SizeOf(Frame^.Sequence));
  inc(CurrentSize, BlockSize);
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
  //    2 bytes  string   1b  string   1b  string  1b   string  1b
  //   +-------+---~~---+---+---~~---+---+---~~---+---+---~~---+---+-->
  //   | 01/02 |filename| 0 |  mode  | 0 |  opt1  | 0 | value1 | 0 | <
  //   +-------+---~~---+---+---~~---+---+---~~---+---+---~~---+---+-->
  //      string  1b   string  1b
  //    >-------+---+---~~---+---+
  //   <  optN  | 0 | valueN | 0 |
  //   >-------+---+---~~---+---+
  Frame^.Opcode := swap(word(ord(OpCode)));
  FrameLen := SizeOf(Frame^.OpCode);
  AppendTextToFrame(FileName);
  AppendTextToFrame('octet');
  Append('timeout',    TimeoutSec,   TFTP_DEFAULTTIMEOUT);
  Append('blksize',    BlockSize,    TFTP_DEFAULTSIZE);
  Append('tsize',      TransferSize, TFTP_NOTRANSFERSIZE);
  Append('windowsize', WindowSize,   TFTP_DEFAULTWINDOWSIZE);
end;

procedure TTftpContext.GenerateErrorFrame(
  err: TTftpError; const msg: RawUtf8);
begin
  //        2 bytes  2 bytes        string    1 byte
  //        -----------------------------------------
  // ERROR | 05    |  ErrorCode |   ErrMsg   |   0  |
  //       -----------------------------------------
  Frame^.Opcode := swap(word(TFTP_ERR));
  if err > teLast then
    Frame^.ErrorCode := 0
  else
    Frame^.ErrorCode := swap(word(ord(err)));
  FrameLen := SizeOf(Frame^.Opcode) + SizeOf(Frame^.ErrorCode);
  if msg <> '' then
    AppendTextToFrame(msg)
  else
    // if no error message is set, use RTTI to generate something in English
    AppendTextToFrame(GetEnumNameUnCamelCase(TypeInfo(TTftpError), ord(err)));
end;

procedure TTftpContext.SetTimeoutTix;
begin
  TimeoutTix := GetTickCount64 + TimeoutSec * 1000;
end;

function TTftpContext.SendFrame: TNetResult;
begin
  if Sock = nil then
    result := nrNotImplemented
  else if FrameLen = 0 then
    result := nrUnknownError // paranoid
  else
    result := Sock.SendTo(Frame, FrameLen, Remote);
  if (result = nrOk) and
     (ToOpCode(Frame^) <> toErr) then
    SetTimeoutTix;
end;

procedure TTftpContext.SendErrorAndShutdown(err: TTftpError; log: TSynLog;
  obj: TObject; const caller: shortstring);
begin
  GenerateErrorFrame(err, '');
  log.Log(sllTrace, '%: % % failed as %',
    [caller, TFTP_OPCODE[OpCode], FileName, ToText(Frame^, FrameLen)], obj);
  SendFrame;
  Shutdown;
end;

procedure TTftpContext.Shutdown;
begin
  FreeAndNil(FileStream);
  Sock.ShutdownAndClose({rwdr=}true);
  Sock := nil; // make it reentrant
end;



{ ******************** TClientTftp Client Connection Class }



initialization
  assert(SizeOf(TTftpFrame) = 4 + TFTP_MAXSIZE);

end.

