{
  This file is a part of the freeware Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

   Delphi specific definitions used by mormot.core.rtti.pas implementation
}


type
  AlignToPtr = Pointer;

{$ifdef HASINLINE} // Delphi RTL TypInfo.GetTypeData() is awful on x86_64

function GetTypeData(TypeInfo: pointer): PTypeData; inline;
begin // weird code which compiles and inlines best on Delphi Win32 and Win64
  {$ifdef CPU64}
  result := pointer(PtrInt(TypeInfo) + ord(TypInfo.PTypeInfo(result)^.Name[0]) + 2);
  {$else}
  result := TypeInfo;
  inc(PByte(result), ord(TypInfo.PTypeInfo(result)^.Name[0]) + 2);
  {$endif CPU64}
end;

{$else}

function GetTypeData(TypeInfo: pointer): PTypeData;
asm // faster code for oldest Delphi
        movzx   edx, byte ptr[eax].TTypeInfo.Name
        lea     eax, [eax + edx].TTypeInfo.Name[1]
end;

{$endif HASINLINE}

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


function TInterfaceTypeData.IntfGuid: PGUID;
begin
  {$ifdef ISDELPHI102} // adapt to latest TypInfo.pas changes
  result := @PTypeData(@self)^.IntfGuid;
  {$else}
  result := @PTypeData(@self)^.Guid;
  {$endif ISDELPHI102}
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

procedure TTypeInfo.RecordManagedFields(out Fields: TRecordManagedFields);
begin
  with PRecordInfo(GetTypeData(@self))^ do
  begin
    Fields.Size := RecSize;
    Fields.Count := ManagedFldCount;
    Fields.Fields := @PIntegerArray(@ManagedFldCount)[1];
  end;
end;

{$ifdef ISDELPHI2010} // read enhanced RTTI available since Delphi 2010

type
  /// map Delphi tkRecord TypeInfo with enhanced RTTI
  TRecordEnhancedTypeData = packed record
    RecSize: cardinal;
    ManagedCount: integer;
    // ManagedFields: array[0..0] of TManagedField;
    NumOps: Byte;
    // RecOps: array[0..0] of pointer;
    AllCount: Integer; // !!!! may need $RTTI EXPLICIT FIELDS([vcPublic])
    AllFields: array[0..0] of TRecordTypeField; // as defined in TypInfo.pas
  end;

function TTypeInfo.RecordAllFields(out RecSize: integer): TRecordAllFieldDynArray;
var
  info: ^TRecordEnhancedTypeData;
  p: PRecordTypeField;
  f: PtrInt;
begin
  info := pointer(GetTypeData(@self));
  RecSize := info^.RecSize;
  inc(PByte(info), info^.ManagedCount * SizeOf(TManagedField));
  inc(PByte(info), info^.NumOps * SizeOf(pointer));
  result := nil; // don't reallocate previous answer
  SetLength(result, info^.AllCount);
  p := @info^.AllFields[0];
  for f := 0 to info^.AllCount - 1 do
    begin
      with result[f] do
      begin
        TypeInfo := PPointer(p^.Field.TypeRef)^;
        Offset := p^.Field.FldOffset;
        Name := @p^.Name;
      end;
      p := pointer(PtrInt(@p^.Name[1]) + ord(p^.Name[0]));
      inc(PByte(p), PWord(p)^); // jump attributes
    end;
end;

{$else}

function TTypeInfo.RecordAllFields(out RecSize: integer): TRecordAllFieldDynArray;
begin
  RecSize := self.RecordSize;
  result := nil; // not available until Delphi 2010
end;

{$endif ISDELPHI2010}

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

