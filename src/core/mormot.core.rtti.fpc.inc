{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   FPC specific definitions used by mormot.core.rtti.pas implementation
}

const
  NO_INDEX = 0;

{$ifdef HASDIRECTTYPEINFO}

  'older FPC RTTI is not supported' // e.g. ParentInfoRef: PPTypeInfo

{$else}

function Deref(Info: pointer): pointer; inline;
begin
  result := Info;
  if result <> nil then
    result := PPointer(result)^;
end;

{$endif HASDIRECTTYPEINFO}

{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}

// will use complex AlignTypeData() inlined function from typinfo.pp
// = AlignToQword() as defined in system.inc and used in rtti.inc

function AlignToPtr(p: pointer): pointer; inline; // not exported by typinfo.pp
begin
  result := align(p, sizeof(p));
end;

{$else}

type
  AlignTypeData = pointer;
  AlignToPtr = pointer;

{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

Function GetTypeData(TypeInfo: pointer): PTypeData; inline;
begin // (aligned) jump over Kind + Name ShortString
  result := AlignTypeData(TypeInfo + 2 + PByte(TypeInfo + 1)^);
end;

function TTypeInfo.ClassType: PClassType; // for proper inlining below
begin
  result := pointer(GetTypeData(@self));
end;

function TClassType.ParentInfo: PTypeInfo;
begin
  result := pointer(PTypeData(@self)^.ParentInfoRef^);
end;

function TClassType.ClassProp: PClassProp;
begin
  result := @self;
  if result <> nil then
    with PTypeData(result)^ do
      {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      result := pointer(GetPropertyTable)
      {$else}
      result := @UnitName[ord(UnitName[0]) + 1];
      {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
end;

function TClassType.PropCount: integer;
begin
  result := PTypeData(@self)^.PropCount;
end;

function InternalClassProp(ClassType: TClass): PClassProp;
begin
  result := PTypeInfo(PPointer(PtrUInt(ClassType) + vmtTypeInfo)^)^.
    ClassType^.ClassProp;
end;


function TClassProp.PropCount: integer;
begin
  result := PPropData(@self)^.PropCount;
end;

function TClassProp.PropList: PPropInfo;
begin // see TPropData.GetProp(0)
  result := AlignToPtr(@PPropData(@self)^.PropList);
end;

function InternalClassPropInfo(ClassType: TClass; out PropInfo: PPropInfo): integer;
var CP: mormot.core.rtti.PClassProp;
begin
  if ClassType<>nil then begin
    CP := InternalClassProp(ClassType);
    if CP<>nil then
    begin // no more RTTI information available
      PropInfo := CP^.PropList;
      result := CP^.PropCount;
      exit;
    end;
  end;
  result := 0;
end;


function TEnumType.EnumBaseType: PEnumType;
var
  base: mormot.core.rtti.PTypeInfo;
begin
  result := @self;
  base := pointer(PTypeData(result)^.BaseType);
  if base <> nil then
    result := pointer(GetTypeData(base));
end;

function TEnumType.SetBaseType: PEnumType;
var
  base: mormot.core.rtti.PTypeInfo;
begin
  result := @self;
  base := pointer(PTypeData(result)^.CompType);
  if base <> nil then
    result := pointer(GetTypeData(base));
end;


function TInterfaceTypeData.IntfParent: PTypeInfo;
begin
  result := Pointer(PTypeData(@self)^.IntfParentRef^);
end;


function TPropInfo.TypeInfo: PTypeInfo;
begin
  result := pointer(TypInfo.PPropInfo(@self)^.PropTypeRef^);
end;

function TPropInfo.GetterIsField: boolean;
begin
  with TypInfo.PPropInfo(@self)^ do
    result := integer(PropProcs) and 3 = ptField;
end;

function TPropInfo.SetterIsField: boolean;
begin
  with TypInfo.PPropInfo(@self)^ do
    result := integer(PropProcs shr 2) and 3 = ptField;
end;


function TTypeInfo.RecordSize: integer;
begin
  result := GetTypeData(@self)^.RecSize;
end;

{$ifdef FPC_NEWRTTI}

function TTypeInfo.RecordManagedFields(out Fields: PRecordField): integer;
var
  d: PRecInitData; // = PRecordInfoInit in rtti.inc
begin
  d := pointer(GetTypeData(@self));
  if Assigned(d^.Terminator) then // see RTTIRecordOp() in rtti.inc
    d := pointer(GetTypeData(d^.Terminator));
  result := d^.ManagedFieldCount;
  inc(PByte(d), SizeOf(d^.ManagedFieldCount));
  Fields := AlignToPtr(d);
end;

{$else}

function TTypeInfo.RecordManagedFields(out Fields: PRecordField): integer;
begin
  with GetTypeData(@self)^ do
  begin
    Fields := @ManagedFldCount;
    inc(Fields); // ManagedFields aligned just after ManagedFldCount: integer
    result := ManagedFldCount;
  end;
end;

{$endif FPC_NEWRTTI}

function TTypeInfo.IsQWord: boolean;
begin
  result := (Kind = tkQWord);
end;

function TTypeInfo.EnumBaseType: PEnumType;
begin
  result := pointer(GetTypeData(@self));
  if Kind <> tkBool then
    result := result^.EnumBaseType;
end;


