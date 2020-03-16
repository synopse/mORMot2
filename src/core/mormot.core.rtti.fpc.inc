{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   FPC specific definitions used by mormot.core.rtti.pas implementation
}

const
  NO_INDEX = 0;

{$ifdef HASDIRECTTYPEINFO}

type
  DeRef = pointer;

{$else}

function Deref(Info: pointer): pointer; inline; // e.g. ParentInfoRef: PPTypeInfo
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

function GetTypeData(TypeInfo: pointer): PTypeData; inline;
begin // (aligned) jump over Kind + Name ShortString - compiles into 2 asm opcodes
  result := AlignTypeData(@PByteArray(TypeInfo)[PByte(TypeInfo + 1)^ + 2]);
end;

function TTypeInfo.ClassType: PClassType; // for proper inlining below
begin
  result := pointer(GetTypeData(@self));
end;

function TClassType.ParentInfo: PTypeInfo;
begin
  result := DeRef(PTypeData(@self)^.ParentInfoRef);
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
var CP: PClassProp;
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
  base: PTypeInfo;
begin
  result := @self;
  base := pointer(PTypeData(result)^.BaseType);
  if base <> nil then
    result := pointer(GetTypeData(base));
end;

function TEnumType.SetBaseType: PEnumType;
var
  base: PTypeInfo;
begin
  result := @self;
  base := pointer(PTypeData(result)^.CompType);
  if base <> nil then
    result := pointer(GetTypeData(base));
end;


function TInterfaceTypeData.IntfGuid: PGUID;
begin
  result := @PTypeData(@self)^.Guid;
end;

function TInterfaceTypeData.IntfParent: PTypeInfo;
begin
  result := DeRef(PTypeData(@self)^.IntfParentRef);
end;


function TPropInfo.TypeInfo: PTypeInfo;
begin
  result := DeRef(TypInfo.PPropInfo(@self)^.PropTypeRef);
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

procedure TTypeInfo.RecordManagedFields(out Fields: TRecordManagedFields);
var
  info: PTypeData;
  d: PRecInitData absolute info; // = PRecordInfoInit in rtti.inc
begin
  info := GetTypeData(@self);
  Fields.Size := info^.RecSize;
  if Assigned(info^.RecInitInfo) then // see RTTIRecordOp() in rtti.inc
    d := pointer(GetTypeData(info^.RecInitInfo));
  Fields.Count := d^.ManagedFieldCount;
  inc(d); // ManagedFields[] just follow TRecInitData/TRecordInfoInit
  Fields.Fields := AlignToPtr(d);
end;

{$else}

function TTypeInfo.RecordManagedFields(out Fields: PRecordField): integer;
begin
  with GetTypeData(@self)^ do
  begin
    Fields.Size := RecSize;
    Fields.Count := ManagedFldCount;
    Fields.Fields := AlignToPtr(PByte(@ManagedFldCount) + SizeOf(integer));
  end; // warning: older FPC RTTI includes all fields, not only managed fields
end;

{$endif FPC_NEWRTTI}

function TTypeInfo.RecordAllFields(out RecSize: integer): TRecordAllFieldDynArray;
begin
  RecSize := self.RecordSize;
  Finalize(result); // enhanced RTTI not available yet on FPC
end;

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


