unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.interfaces;

type
  TMessageID = Int64;

  /// Message DTO (Data Transfer Object)
  TMessageDTO = packed record
    ID: TMessageID;
    Content: RawUtf8;
    Timestamp: TUnixMSTime;
  end;
  TMessageDTOs = array of TMessageDTO;

  /// HelloWorld response DTO
  THelloWorldResponse = packed record
    Message: RawUtf8;
    Time: RawUtf8;
  end;

  /// POST request DTO
  TPostRequestDTO = packed record
    Data: RawUtf8;
    Modified: RawUtf8;
  end;

  /// Division result DTO
  TDivisionResult = packed record
    Result: Double;
  end;

  /// Basic Demo API interface
  // Ports the DMVC basicdemo_server endpoints to mORMot2
  IBasicDemoApi = interface(IInvokable)
    ['{B1A2C3D4-5E6F-4890-ABCD-EF1234567890}']

    /// GET /hello - Returns a simple hello world message with current time
    function HelloWorld: THelloWorldResponse;

    /// POST /hello - Echoes back the posted JSON with a modification
    function HelloWorldPost(const data: RawUtf8): TPostRequestDTO;

    /// GET /div/{par1}/{par2} - Divides two numbers
    function Divide(par1, par2: integer): TDivisionResult;
  end;


implementation


initialization
  // customize the DTO fields (as shorter lowercase)
  Rtti.RegisterFromText(TypeInfo(TMessageDTO),
    'id:Int64 content:RawUtf8 timestamp:Int64');
  Rtti.RegisterFromText(TypeInfo(THelloWorldResponse),
    'message:RawUtf8 time:RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TPostRequestDTO),
    'data:RawUtf8 modified:RawUtf8');
  Rtti.RegisterFromText(TypeInfo(TDivisionResult),
    'result:Double');

  // allow to use directly IBasicDemoApi type/guid for TypeInfo(IBasicDemoApi)
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IBasicDemoApi)]);

end.
