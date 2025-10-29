unit api.mobile;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.os,
  mormot.core.interfaces;

type
  TSourceID = Int64;
  TEventID = Int64;

  // mobile API as used on both client and server side
  IApiMobile = interface(IInvokable)
    ['{068173CE-E307-4166-AE3F-60B7F0EF42F8}']
    function Login(id: TSourceID): boolean;
    function Register(const descr: RawUtf8): TSourceID;
    function UnRegister(id: TSourceID): boolean;
    function NewEvent(id: TSourceID; const eventinfo: RawUtf8): TEventID;
  end;


implementation


initialization
  // allow to use directly IApiMobile type/guid for TypeInfo(IApiMobile)
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IApiMobile)]);

end.
