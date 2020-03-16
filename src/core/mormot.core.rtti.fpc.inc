{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   FPC specific definitions used by mormot.core.rtti.pas implementation
}

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

function TRttiInfo.RttiClass: PRttiClass; // for proper inlining below
begin
  result := pointer(GetTypeData(@self));
end;

function TRttiClass.ParentInfo: PRttiInfo;
begin
  result := DeRef(PTypeData(@self)^.ParentInfoRef);
end;

function TRttiClass.RttiProps: PRttiProps;
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

function TRttiClass.PropCount: integer;
begin
  result := PTypeData(@self)^.PropCount;
end;

function GetRttiProps(RttiClass: TClass): PRttiProps;
begin
  result := PRttiInfo(PPointer(PtrUInt(RttiClass) + vmtTypeInfo)^)^.
    RttiClass^.RttiProps;
end;


function TRttiProps.PropCount: integer;
begin
  result := PPropData(@self)^.PropCount;
end;

function TRttiProps.PropList: PRttiProp;
begin // see TPropData.GetProp(0)
  result := AlignToPtr(@PPropData(@self)^.PropList);
end;

function GetRttiProp(C: TClass; out PropInfo: PRttiProp): integer;
var CP: PRttiProps;
begin
  if C<>nil then begin
    CP := GetRttiProps(C);
    if CP <> nil then
    begin // no more RTTI information available
      PropInfo := CP^.PropList;
      exit(CP^.PropCount);
    end;
  end;
  result := 0;
end;


function TRttiEnumType.EnumBaseType: PRttiEnumType;
var
  base: PTypeInfo;
begin
  result := @self;
  base := DeRef(PTypeData(@self)^.BaseTypeRef);
  if base <> nil then
    result := pointer(GetTypeData(base));
end;

function TRttiEnumType.SetBaseType: PRttiEnumType;
var
  base: PTypeInfo;
begin
  result := @self;
  base := DeRef(PTypeData(@self)^.CompTypeRef);
  if base <> nil then
    result := pointer(GetTypeData(base));
end;

function TRttiEnumType.GetEnumNameOrd(Value: cardinal): PShortString;
var
  by4: integer;
begin // this code is very aggressively inlined on FPC
  if Value <= cardinal(PTypeData(@self).MaxValue) then
  begin
    result := @PTypeData(@self).NameList;
    by4 := Value shr 2;
    if by4 <> 0 then
      repeat
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        dec(by4);
      until by4 = 0;
    Value := Value and 3;
    if Value <> 0 then
      repeat
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        dec(Value);
      until Value = 0;
  end
  else
    result := @NULL_SHORTSTRING;
end;

function TRttiInfo.EnumBaseType: PRttiEnumType; // moved here for proper inlining
begin
  result := pointer(GetTypeData(@self));
  if Kind <> rkBool then
    result := result^.EnumBaseType;
end;

function GetEnumName(aTypeInfo: pointer; aIndex: integer): PShortString;
begin
  result := PRttiInfo(aTypeInfo)^.EnumBaseType.GetEnumNameOrd(aIndex);
end;


function TRttiInterfaceTypeData.IntfGuid: PGUID;
begin
  result := @PTypeData(@self)^.Guid;
end;

function TRttiInterfaceTypeData.IntfParent: PRttiInfo;
begin
  result := DeRef(PTypeData(@self)^.IntfParentRef);
end;


function TRttiProp.TypeInfo: PRttiInfo;
begin
  result := DeRef(PPropInfo(@self)^.PropTypeRef);
end;

function TRttiProp.GetterIsField: boolean;
begin
  with PPropInfo(@self)^ do
    result := integer(PropProcs) and 3 = ptField;
end;

function TRttiProp.SetterIsField: boolean;
begin
  with PPropInfo(@self)^ do
    result := integer(PropProcs shr 2) and 3 = ptField;
end;

function TRttiProp.WriteIsDefined: boolean;
begin
  with PPropInfo(@self)^ do // see typinfo.IsWriteableProp
    result := (SetProc <> nil) and
      ((integer(PropProcs) shr 2) and 3 in [ptField..ptVirtual]);
end;

function TRttiProp.IsStored(Instance: TObject): boolean;
type
  TGetProc = function: boolean of object;
  TGetIndexed = function(Index: integer): boolean of object;
var
  call: TMethod;
begin
  with PPropInfo(@self)^ do
  begin
    case (integer(PropProcs) shr 4) and 3 of // bits 4..5 = StoredProc
      ptConst:
        exit(boolean(PtrUInt(StoredProc)));
      ptField:
        exit(PBoolean(PtrUInt(Instance) + PtrUInt(StoredProc))^);
      ptVirtual:
        call.Code := PPointer(PPtrUInt(Instance)^ + PtrUInt(StoredProc))^;
      else
        call.Code := pointer(StoredProc);
    end;
    call.Data := Instance;
    if integer(PropProcs) and (1 shl 6) <> 0 then
      result := TGetIndexed(call)(Index)
    else
      result := TGetProc(call);
  end;
end;

function TRttiProp.Getter(Instance: TObject; Call: PMethod): TRttiPropCall;
begin
  with PPropInfo(@self)^ do
  begin
    if GetProc = nil then
      // no 'read' was defined -> try from 'write' field
      if (SetProc <> nil) and ((integer(PropProcs) shr 2) and 3 = ptField) then
      begin // bits 2..3 = SetProc
        Call.Data := pointer(PtrUInt(Instance) + PtrUInt(SetProc));
        exit(rpcField);
      end
      else
        exit(rpcNone)
    else
    case integer(PropProcs) and 3 of // bits 0..1 = GetProc
      ptField:
        begin  // GetProc is an offset to the instance fields
          Call.Data := pointer(PtrUInt(Instance) + PtrUInt(GetProc));
          exit(rpcField);
        end;
      ptVirtual: // GetProc is an offset to the class VMT
        Call.Code := PPointer(PPtrUInt(Instance)^ + PtrUInt(GetProc))^;
      ptConst:
        exit(rpcNone);
    else // ptStatic: GetProc is the method code itself
      Call.Code := GetProc;
    end;
    Call.Data := Instance;
    result := rpcMethod;
    if integer(PropProcs) and (1 shl 6) <> 0 then // bit 6 = constant index property
      result := rpcIndexed;
  end;
end;

function TRttiProp.Setter(Instance: TObject; Call: PMethod): TRttiPropCall;
begin
  with PPropInfo(@self)^ do
  begin
    if SetProc = nil then
      // no 'write' was defined -> try from 'read' field
      if (GetProc <> nil) and (integer(PropProcs) and 3 = ptField) then
      begin // bits 0..1 = GetProc
        Call.Data := pointer(PtrUInt(Instance) + PtrUInt(GetProc));
        exit(rpcField);
      end
      else
        exit(rpcNone)
    else
    case (integer(PropProcs) shr 2) and 3 of // bits 2..3 = SetProc
      ptField:
        begin  // SetProc is an offset to the instance fields
          Call.Data := pointer(PtrUInt(Instance) + PtrUInt(SetProc));
          exit(rpcField);
        end;
      ptVirtual: // SetProc is an offset to the class VMT
        Call.Code := PPointer(PPtrUInt(Instance)^ + PtrUInt(SetProc))^;
      ptConst:
        exit(rpcNone);
    else // ptStatic: SetProc is the method code itself
      Call.Code := SetProc;
    end;
    Call.Data := Instance;
    result := rpcMethod;
    if integer(PropProcs) and (1 shl 6) <> 0 then // bit 6 = constant index property
      result := rpcIndexed;
  end;
end;


function TRttiInfo.RecordSize: integer;
begin
  result := GetTypeData(@self)^.RecSize;
end;

{$ifdef FPC_NEWRTTI}

procedure TRttiInfo.RecordManagedFields(out Fields: TRttiRecordManagedFields);
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

procedure TRttiInfo.RecordManagedFields(out Fields: TRttiRecordManagedFields);
begin
  with GetTypeData(@self)^ do
  begin
    Fields.Size := RecSize;
    Fields.Count := ManagedFldCount;
    Fields.Fields := AlignToPtr(PByte(@ManagedFldCount) + SizeOf(integer));
  end; // warning: older FPC RTTI includes all fields, not only managed fields
end;

{$endif FPC_NEWRTTI}

function TRttiInfo.RecordAllFields(out RecSize: integer): TRttiRecordAllFields;
begin
  RecSize := RecordSize;
  Finalize(result); // enhanced RTTI not available yet on FPC
end;

function TRttiInfo.IsQWord: boolean;
begin
  result := (Kind = rkQWord);
end;

function TRttiInfo.DynArrayItemType(aDataSize: PInteger): PRttiInfo;
begin
  result := nil;
  if (@self <> nil) and (Kind = rkDynArray) then
  begin
    result := pointer(GetTypeData(@self));
    if aDataSize <> nil then
      aDataSize^ := PTypeData(result)^.elSize and $7FFFFFFF;
    result := DeRef(PTypeData(result)^.elTypeRef);
  end;
end;

function GetPublishedMethods(Instance: TObject; out Methods: TPublishedMethodInfoDynArray;
  aClass: TClass): integer;

  procedure AddParentsFirst(C: TClass);
  type
    TMethodInfo = packed record
      Name: PShortString;
      Addr: Pointer;
    end;
  var
    Table: PCardinalArray;
    M: ^TMethodInfo;
    i: integer;
  begin
    if C = nil then
      exit;
    AddParentsFirst(GetClassParent(C)); // put children published methods afterward
    Table := PPointer(PtrUInt(C) + PtrUInt(vmtMethodTable))^;
    if Table = nil then
      exit;
    SetLength(Methods, result + Table^[0]);
    M := @Table^[1];
    for i := 1 to Table^[0] do  // Table^[0] = methods count
      with Methods[result] do
      begin
        ShortStringToAnsi7String(M^.Name^, Name);
        Method.Data := Instance;
        Method.Code := M^.Addr;
        inc(M);
        inc(result);
      end;
  end;

begin
  result := 0;
  if aClass <> nil then
    AddParentsFirst(aClass)
  else if Instance <> nil then
    AddParentsFirst(PPointer(Instance)^); // use recursion for adding
end;


