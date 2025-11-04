unit api.mobile;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.interfaces;

type
  TSourceID = Int64;
  TEventID = Int64;

  // event DTO for the mobile platform
  TEvent = packed record
    ID: TEventID;
    Description: RawUtf8;
    TimeStamp: TUnixMSTime;
  end;
  TEvents = array of TEvent;

  // mobile API as used on both client and server sides
  IApiMobile = interface(IInvokable)
    ['{068173CE-E307-4166-AE3F-60B7F0EF42F8}']
    function Login(id: TSourceID): boolean;
    function Register(const descr: RawUtf8): TSourceID;
    function UnRegister(id: TSourceID): boolean;
    function NewEvent(id: TSourceID; const eventinfo: RawUtf8): TEventID;
    function LastEvent(id: TSourceID; max: integer): TEvents;
  end;


implementation


initialization
  // customize the DTO fields (as shorter lowercase)
  Rtti.RegisterFromText(TypeInfo(TEvent),
    'id:Int64 desc:RawUtf8 time:Int64');
  // allow to use directly IApiMobile type/guid for TypeInfo(IApiMobile)
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IApiMobile)]);

end.
