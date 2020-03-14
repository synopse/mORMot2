{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   Delphi specific definitions used by mormot.core.rtti.pas implementation
}


const
  NO_INDEX = longint($80000000);

  ptField = $ff;
  ptVirtual = $fe;

type
  /// used to map a TPropInfo.GetProc/SetProc and retrieve its kind
  // - defined here for proper Delphi inlining
  PropWrap = packed record
    FillBytes: array [0 .. SizeOf(Pointer) - 2] of byte;
    /// =$ff for a ptField address, or =$fe for a ptVirtual method
    Kind: byte;
  end;

  PPropData = ^TPropData; // not defined e.g. in Delphi 7

  AlignToPtr = Pointer;

  {$ifdef ISDELPHI2010}
  /// map the Delphi record field enhanced RTTI (available since Delphi 2010)
  TEnhancedRecordInfo = packed record
    TypeInfo: PPTypeInfo;
    Offset: PtrUInt;
    Flags: byte;
    NameLen: byte; // = Name[0] = length(Name)
  end;
  PEnhancedRecordInfo = ^TEnhancedRecordInfo;
  {$endif ISDELPHI2010}

  TRecordInfo = packed record // not defined e.g. in Delphi 7
    RecSize: cardinal;
    ManagedCount: integer;
    ManagedFields: array[0..0] of TRecordField;
    {$ifdef ISDELPHI2010} // enhanced RTTI containing info about all fields
    NumOps: Byte;
    //RecOps: array[1..NumOps] of Pointer;
    AllCount: Integer; // !!!! may need $RTTI EXPLICIT FIELDS([vcPublic])
    AllFields: array[0..0] of TEnhancedRecordInfo;
    {$endif ISDELPHI2010}
  end;
  PRecordInfo = ^TRecordInfo;


function TTypeInfo.ClassType: PClassType; // for proper inlining below
begin
  result := pointer(GetTypeData(@self));
end;

function TClassType.PropCount: integer;
begin
  result := PTypeData(@self)^.PropCount;
end;

function TClassType.ParentInfo: PTypeInfo;
begin
  result := pointer(PTypeData(@self)^.ParentInfo^);
end;

function TClassType.ClassProp: PClassProp;
begin
  result := @self;
  if result <> nil then
    with PTypeData(result)^ do
      result := @UnitName[ord(UnitName[0]) + 1];
end;

function InternalClassProp(ClassType: TClass): PClassProp;
var
  p: TypInfo.PTypeInfo;
begin // code is a bit abstract, but compiles very well
  p := PPointer(PtrInt(ClassType) + vmtTypeInfo)^;
  if p <> nil then // avoid GPF if no RTTI available for this class
    with PTypeData(@p^.Name[ord(p^.Name[0]) + 1])^ do
      result := @UnitName[ord(UnitName[0]) + 1]
    else
      result := nil;
end;

function TClassProp.PropCount: integer;
begin
  result := PPropData(@self)^.PropCount;
end;

function TClassProp.PropList: PPropInfo;
begin
  result := pointer(@PPropData(@self)^.PropList);
end;

function InternalClassPropInfo(ClassType: TClass; out PropInfo: PPropInfo): integer;
var
  p: TypInfo.PTypeInfo;
begin
  if ClassType<>nil then
  begin
    p := PPointer(PtrInt(ClassType) + vmtTypeInfo)^;
    if p <> nil then // avoid GPF if no RTTI available
      with PTypeData(@p^.Name[ord(p^.Name[0]) + 1])^,
           PPropData(@UnitName[ord(UnitName[0]) + 1])^ do
      begin
        PropInfo := @PropList;
        result := PropCount;
        exit;
      end;
  end;
  result := 0;
end;

function TEnumType.EnumBaseType: PEnumType;
begin
  with PTypeData(@self).BaseType^^ do
    result := @Name[ord(Name[0]) + 1];
end;

function TEnumType.SetBaseType: PEnumType;
begin
  with PTypeData(@self).CompType^^ do
    result := @Name[ord(Name[0]) + 1];
end;


function TInterfaceTypeData.IntfParent: PTypeInfo;
begin
  result := Pointer(PTypeData(@self)^.IntfParent^);
end;

function TPropInfo.TypeInfo: PTypeInfo;
begin
  result := pointer(TypInfo.PPropInfo(@self)^.PropType^);
end;

function TPropInfo.GetterIsField: boolean;
begin
  with TypInfo.PPropInfo(@self)^ do
    result := PropWrap(GetProc).Kind = ptField;
end;

function TPropInfo.SetterIsField: boolean;
begin
  with TypInfo.PPropInfo(@self)^ do
    result := PropWrap(SetProc).Kind = ptField;
end;


function TTypeInfo.RecordSize: integer;
begin
  result := PRecordInfo(GetTypeData(@self))^.RecSize;
end;

function TTypeInfo.RecordManagedFields(out Fields: PRecordField): integer;
begin
  with PRecordInfo(GetTypeData(@self))^ do
  begin
    Fields := @ManagedFields;
    result := ManagedCount;
  end;
end;

function TTypeInfo.IsQWord: boolean;
begin
  if @self = TypeInfo(QWord) then
    result := true
  else
    {$ifdef UINICODE}
    if Kind = tkInt64 then
      with PHash128Rec(PAnsiChar(@Name[1]) + ord(Name[0]))^ do
        result := Lo > Hi // check MinInt64Value>MaxInt64Value
    else
    {$endif UNICODE}
      result := false;
end;

function TTypeInfo.EnumBaseType: PEnumType;
begin
  result := pointer(GetTypeData(@self));
  result := result^.EnumBaseType;
end;

