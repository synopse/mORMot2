{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   FPC specific definitions used by mormot.core.rtti.pas implementation
}

{$ifdef HASDIRECTTYPEINFO}

type
  DeRef = pointer;

{$else}

function Deref(Info: pointer): pointer; // e.g. ParentInfoRef: PPTypeInfo
begin
  result := Info;
  if result <> nil then
    result := PPointer(result)^;
end;

{$endif HASDIRECTTYPEINFO}

{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}

function AlignToPtr(p: pointer): pointer; // not exported by typinfo.pp
begin
  result := align(p, sizeof(p));
end;

// use complex AlignTypeData() inlined function from typinfo.pp
// = AlignToQword() as defined in system.inc and used in rtti.inc
function GetTypeData(TypeInfo: pointer): PTypeData;
begin // qword aligned jump over Kind+Name - compiles as 2 x86_64 asm opcodes
  result := AlignTypeData(@PByteArray(TypeInfo)[PByte(TypeInfo + 1)^ + 2]);
end;

{$else}

type
  AlignToPtr = pointer;

function GetTypeData(TypeInfo: pointer): PTypeData;
begin // jump over Kind+Name - compiles as 2 x86_64 asm opcodes
  result := @PByteArray(TypeInfo)[PByte(TypeInfo + 1)^ + 2];
end;

{$endif FPC_REQUIRES_PROPER_ALIGNMENT}


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
    {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    result := pointer(PClassData(result)^.PropertyTable)
    {$else}
    with PTypeData(result)^ do
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
  if C<>nil then
  begin
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
    result := @NULCHAR;
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


const
  INTEGER_NAME:  string[7] = 'Integer';   // FPC names integer  as "longint"
  CARDINAL_NAME: string[8] = 'Cardinal';  // FPC names cardinal as "longword"

function TRttiInfo.Name: PShortString;
begin // TypeInfo(Integer/Cardinal)=TypeInfo(LongInt/LongWord)
  result := pointer(@self);
  if result <> TypeInfo(integer) then
    if result <> TypeInfo(cardinal) then
      result := @RawName
    else
      result := @CARDINAL_NAME
  else
    result := @INTEGER_NAME;
end;

function TRttiInfo.RecordSize: PtrInt;
begin
  if (Kind <> rkRecord) and (Kind <> rkObject) then
    result := 0
  else
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

function TRttiInfo.RecordAllFields(out RecSize: PtrInt): TRttiRecordAllFields;
begin
  RecSize := RecordSize;
  Finalize(result{%H-}); // enhanced RTTI not available yet on FPC
end;

function TRttiInfo.IsQWord: boolean;
begin
  result := (Kind = rkQWord);
end;

function TRttiInfo.DynArrayItemType(out aDataSize: PtrInt): PRttiInfo;
begin
  result := pointer(GetTypeData(@self));
  aDataSize := PTypeData(result)^.elSize and $7FFFFFFF;
  result := DeRef(PTypeData(result)^.elTypeRef);
end;

function TRttiInfo.DynArrayItemType: PRttiInfo;
begin
  result := DeRef(GetTypeData(@self)^.elTypeRef);
end;

function TRttiInfo.ArrayItemType(out aDataCount, aDataSize: PtrInt): PRttiInfo;
var
  info: ^TArrayTypeData;
begin
  info := @GetTypeData(@self)^.ArrayData;
  aDataCount := info^.ElCount;
  aDataSize := info^.Size {$ifdef VER2_6} * info^.ElCount {$endif};
  result := DeRef(info^.ElTypeRef);
  if (info^.DimCount <> 1) or (result = nil) or not (result^.Kind in rkManagedTypes) then
    result := nil;
end;

function TRttiInfo.ArrayItemSize: PtrInt;
var
  info: ^TArrayTypeData;
begin
  info := @GetTypeData(@self)^.ArrayData;
  result := info^.Size {$ifdef VER2_6} * info^.ElCount {$endif};
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
    AddParentsFirst(GetClassParent(C)); // put children methods afterwards
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


const
  {$if defined(CPUI386) or defined(CPUI8086) or defined(CPUX86_64) or defined(CPUM68K)}
  DEFCC = ccReg;
  {$else}
  DEFCC = ccStdCall;
  {$ifend}

procedure TGetRttiInterface.AddMethodsFromTypeInfo(aInterface: PTypeInfo);
var
  info: PInterfaceData;
  ancestor: PTypeInfo;
  methods: PIntfMethodTable;
  m: PIntfMethodEntry;
  a: PVmtMethodParam;
  res: PRttiInfo;
  mn, an: integer;
begin
  info := pointer(GetTypeData(aInterface));
  if IdemPropName(info^.UnitName, 'System') then
    exit;
  if Definition.Name = '' then
  begin
    ShortStringToAnsi7String(aInterface^.Name, Definition.Name);
    ShortStringToAnsi7String(info^.UnitName, Definition.UnitName);
    Definition.GUID := info^.GUID;
  end;
  ancestor := DeRef(info^.Parent);
  if ancestor <> nil then
  begin
    AddMethodsFromTypeInfo(ancestor);
    inc(Level);
  end;
  methods := info^.MethodTable;
  m := methods^.Method[0];
  if m = nil then
    exit;
  SetLength(Definition.Methods, MethodCount + methods^.Count);
  mn :=  methods^.Count;
  repeat
    AddMethod(m^.Name, m^.ParamCount, m^.Kind);
    if m^.CC <> DEFCC then
      RaiseError('unsupported %', [GetEnumName(TypeInfo(TCallConv), ord(m^.CC))^]);
    a := m^.Param[0];
    if a <> nil then
    begin
      an := m^.ParamCount;
      repeat
        if not (pfResult in a^.Flags) then // result may not be the last on ARM
          AddArgument( {$ifdef VER3_1} @a^.Name {$else} a^.NamePtr {$endif},
          { since https://svn.freepascal.org/cgi-bin/viewvc.cgi?view=revision&revision=39684
            TVmtMethodParam.Name is a local stack copy -> direct NamePtr use }
            nil, DeRef(a^.ParamType), a^.Flags);
        dec(an);
        if an = 0 then
          break;
        a := a^.Next;
      until false;
    end;
    res := DeRef(m^.ResultType);
    if res <> nil then
      AddArgument(nil, nil, res, []);
    if ArgCount <> length(CurrentMethod^.Args) then
      SetLength(CurrentMethod^.Args, ArgCount); // only on ARM
    dec(mn);
    if mn = 0 then
      break;
    m := m^.Next;
  until false;
  CurrentMethod := nil;
end;



