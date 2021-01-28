{
  This file is a part of the Open Source Synopse mORMot framework 2,
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
begin
  // qword aligned jump over Kind+Name - compiles as 2 x86_64 asm opcodes
  result := AlignTypeData(@PByteArray(TypeInfo)[PByte(TypeInfo + 1)^ + 2]);
end;

{$else}

type
  AlignToPtr = pointer;

function GetTypeData(TypeInfo: pointer): PTypeData;
begin
  // jump over Kind+Name - compiles as 2 x86_64 asm opcodes
  result := @PByteArray(TypeInfo)[PByte(TypeInfo + 1)^ + 2];
end;

{$endif FPC_REQUIRES_PROPER_ALIGNMENT}

type
  PRecordInfo = PTypeData; // oldest Delphi PTypeData doesn't have RecordSize

function TRttiInfo.RttiClass: PRttiClass; // for proper inlining below
begin
  if @self <> nil then
    result := pointer(GetTypeData(@self))
  else
    result := nil;
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
      result := AlignToPtr(@UnitName[ord(UnitName[0]) + 1]);
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
begin
  // see TPropData.GetProp(0)
  result := AlignToPtr(@PPropData(@self)^.PropList);
end;

function GetRttiProp(C: TClass; out PropInfo: PRttiProp): integer;
var CP: PRttiProps;
begin
  if C <> nil then
  begin
    CP := GetRttiProps(C);
    if CP <> nil then
    begin
      // no more RTTI information available
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

const
  NULCHAR: AnsiChar = #0; // local to use RIP and not REL

function TRttiEnumType.GetEnumNameOrd(Value: cardinal): PShortString;
var
  n: integer;
begin
  // this code is very aggressively inlined on FPC
  if Value <= cardinal(PTypeData(@self).MaxValue) then
  begin
    result := @PTypeData(@self).NameList;
    n := Value shr 2; // move forward by four items at once
    if n <> 0 then
      repeat
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        dec(n);
      until n = 0;
    n := Value and 3; // last 1..3 moves
    if n <> 0 then
      repeat
        result := @PByteArray(result)^[ord(result^[0]) + 1];
        dec(n);
      until n = 0;
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

function GetEnumName(aTypeInfo: PRttiInfo; aIndex: integer): PShortString;
begin
  result := aTypeInfo^.EnumBaseType.GetEnumNameOrd(aIndex);
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
begin
  if (integer(PPropInfo(@self)^.PropProcs) shr 4) and 3 = ptConst then
    result := boolean(PtrUInt(PPropInfo(@self)^.StoredProc))
  else
    result := GetIsStored(Instance);
end;

function TRttiProp.GetIsStored(Instance: TObject): boolean;
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
      if (SetProc <> nil) and
         ((integer(PropProcs) shr 2) and 3 = ptField) then
      begin
        // bits 2..3 = SetProc
        Call.Data := pointer(PtrUInt(Instance) + PtrUInt(SetProc));
        exit(rpcField);
      end
      else
        exit(rpcNone)
    else
    case integer(PropProcs) and 3 of // bits 0..1 = GetProc
      ptField:
        begin
          // GetProc is an offset to the instance fields
          Call.Data := pointer(PtrUInt(Instance) + PtrUInt(GetProc));
          exit(rpcField);
        end;
      ptVirtual:
        // GetProc is an offset to the class VMT
        if Instance <> nil then // e.g. from GetterCall()
          Call.Code := PPointer(PPtrUInt(Instance)^ + PtrUInt(GetProc))^;
      ptConst:
        exit(rpcNone);
    else
      // ptStatic: GetProc is the method code itself
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
      if (GetProc <> nil) and
         (integer(PropProcs) and 3 = ptField) then
      begin
        // bits 0..1 = GetProc
        Call.Data := pointer(PtrUInt(Instance) + PtrUInt(GetProc));
        exit(rpcField);
      end
      else
        exit(rpcNone)
    else
    case (integer(PropProcs) shr 2) and 3 of // bits 2..3 = SetProc
      ptField:
        begin
          // SetProc is an offset to the instance fields
          Call.Data := pointer(PtrUInt(Instance) + PtrUInt(SetProc));
          exit(rpcField);
        end;
      ptVirtual:
        // SetProc is an offset to the class VMT
        if Instance <> nil then // e.g. from SetterCall()
          Call.Code := PPointer(PPtrUInt(Instance)^ + PtrUInt(SetProc))^;
      ptConst:
        exit(rpcNone);
    else
      // ptStatic: SetProc is the method code itself
      Call.Code := SetProc;
    end;
    Call.Data := Instance;
    result := rpcMethod;
    if integer(PropProcs) and (1 shl 6) <> 0 then // bit 6 = constant index property
      result := rpcIndexed;
  end;
end;


const
  // FPC RTTI names integer type as "longint"
  INTEGER_NAME:  string[7] = 'Integer';
  // FPC RTTI names cardinal type as "longword"
  CARDINAL_NAME: string[8] = 'Cardinal';

function TRttiInfo.Name: PShortString;
begin
  // recognize TypeInfo(integer/cardinal)=TypeInfo(LongInt/LongWord) on FPC
  result := pointer(@self);
  if result <> nil then
    if result <> TypeInfo(integer) then
      if result <> TypeInfo(cardinal) then
        result := @RawName
      else
        result := @CARDINAL_NAME
    else
      result := @INTEGER_NAME
  else
    result := @NULCHAR;
end;

function TRttiInfo.RecordSize: PtrInt;
begin
  if (Kind <> rkRecord) and
     (Kind <> rkObject) then
    result := 0
  else
    result := GetTypeData(@self)^.RecSize;
end;

{$ifdef ISFPC32}
function TRttiInfo.SetEnumSize: PtrInt;
begin
  result := GetTypeData(@self)^.SetSize; // newer FPC have direct size field :)
end;
{$endif ISFPC32}

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

function TRttiInfo.RecordManagedFieldsCount: integer;
var
  info: PTypeData;
  d: PRecInitData absolute info; // = PRecordInfoInit in rtti.inc
begin
  info := GetTypeData(@self);
  if Assigned(info^.RecInitInfo) then
    d := pointer(GetTypeData(info^.RecInitInfo));
  result := d^.ManagedFieldCount;
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


function TRttiInfo.RecordManagedFieldsCount: integer;
var
  fields: TRttiRecordManagedFields;
begin
  result := 0;
  RecordManagedFields(fields);
  while fields.Count > 0 do
  begin
    if PRttiInfo(DeRef(fields.Fields^.{$ifdef HASDIRECTTYPEINFO}TypeInfo
         {$else}TypeInfoRef^{$endif}))^.IsManaged then
      inc(result);
    inc(fields.Fields);
    dec(fields.Count);
  end;
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

function TRttiInfo.IsBoolean: boolean;
begin
  result := (Kind = rkBool);
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

function TRttiInfo.DynArrayItemTypeAny: PRttiInfo;
begin
  with GetTypeData(@self)^ do
  begin
    result := DeRef(elTypeRef); // only managed fields by default
    if result = nil then
      // try the second RTTI slot, which is set even for unmanaged types
      result := DeRef(elType2Ref);
  end;
end;

function TRttiInfo.ArrayItemType(out aDataCount, aDataSize: PtrInt): PRttiInfo;
var
  info: ^TArrayTypeData;
begin
  info := @GetTypeData(@self)^.ArrayData;
  aDataCount := info^.ElCount;
  aDataSize := info^.Size {$ifdef VER2_6} * info^.ElCount {$endif};
  result := DeRef(info^.ElTypeRef);
  if (info^.DimCount <> 1) or
     (result = nil) or
     not (result^.Kind in rkManagedTypes) then
    result := nil;
end;

function TRttiInfo.ArraySize: PtrInt;
var
  info: ^TArrayTypeData;
begin
  info := @GetTypeData(@self)^.ArrayData;
  result := info^.Size {$ifdef VER2_6} * info^.ElCount {$endif};
end;

function GetPublishedMethods(Instance: TObject;
  out Methods: TPublishedMethodInfoDynArray; aClass: TClass): integer;

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
    Definition.Guid := info^.GUID;
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


{ ********* Redirect Most Used FPC RTL Functions to Optimized x86_64 Assembly }

{$ifdef FPC_CPUX64}

// access to lowest level FPC RTL functions by their raw linking name
procedure fpc_ansistr_decr_ref;      external name 'FPC_ANSISTR_DECR_REF';
procedure fpc_ansistr_incr_ref;      external name 'FPC_ANSISTR_INCR_REF';
procedure fpc_ansistr_assign;        external name 'FPC_ANSISTR_ASSIGN';
procedure fpc_ansistr_setlength;     external name 'FPC_ANSISTR_SETLENGTH';
procedure fpc_ansistr_compare;       external name 'FPC_ANSISTR_COMPARE';
procedure fpc_ansistr_compare_equal; external name 'FPC_ANSISTR_COMPARE_EQUAL';
procedure fpc_unicodestr_decr_ref;   external name 'FPC_UNICODESTR_DECR_REF';
procedure fpc_unicodestr_incr_ref;   external name 'FPC_UNICODESTR_INCR_REF';
procedure fpc_unicodestr_assign;     external name 'FPC_UNICODESTR_ASSIGN';
procedure fpc_dynarray_incr_ref;     external name 'FPC_DYNARRAY_INCR_REF';
procedure fpc_dynarray_decr_ref;     external name 'FPC_DYNARRAY_DECR_REF';
procedure fpc_dynarray_clear;        external name 'FPC_DYNARRAY_CLEAR';
{$ifdef FPC_X64MM}
procedure fpc_getmem;                external name 'FPC_GETMEM';
procedure fpc_freemem;               external name 'FPC_FREEMEM';
{$else}
procedure _Getmem;                   external name 'FPC_GETMEM';
procedure _Freemem(p: pointer);      external name 'FPC_FREEMEM';
{$endif FPC_X64MM}

procedure PatchJmp(old, new: PByteArray; size: PtrInt; jmp: PtrUInt = 0);
var
  rel: PCardinal;
begin
  PatchCode(old, new, size, nil, {unprotected=}true);
  if jmp = 0 then
    jmp := PtrUInt(@_Freemem);
  repeat // search and fix "jmp rel _Freemem"
    dec(size);
    if size = 0 then
      exit;
    rel := @old[size + 1];
  until (old[size] = $e9) and
        (rel^ = cardinal(jmp - PtrUInt(@new[size]) - 5));
  rel^ := jmp - PtrUInt(rel) - 4;
end;

procedure _ansistr_decr_ref(var p: Pointer); nostackframe; assembler;
asm
        mov     rax, qword ptr [p]
        xor     edx, edx
        test    rax, rax
        jz      @z
        mov     qword ptr [p], rdx
        mov     p, rax
        cmp     qword ptr [rax - _STRREFCNT], rdx
        jl      @z
lock    dec     qword ptr [rax - _STRREFCNT]
        jbe     @free
@z:     ret
@free:  sub     p, _STRRECSIZE
        jmp     _Freemem
end;

procedure _ansistr_incr_ref(p: pointer); nostackframe; assembler;
asm
        test    p, p
        jz      @z
        cmp     qword ptr [p - _STRREFCNT], 0
        jl      @z
lock    inc     qword ptr [p - _STRREFCNT]
@z:
end;

procedure _ansistr_assign(var d: pointer; s: pointer); nostackframe; assembler;
asm
        mov     rax, qword ptr [d]
        cmp     rax, s
        jz      @eq
        test    s, s
        jz      @ns
        cmp     qword ptr [s - _STRREFCNT], 0
        jl      @ns
lock    inc     qword ptr [s - _STRREFCNT]
@ns:    mov     qword ptr [d], s
        test    rax, rax
        jnz     @z
@eq:    ret
@z:     mov     d, rax
        cmp     qword ptr [rax - _STRREFCNT], 0
        jl      @n
 lock   dec     qword ptr [rax - _STRREFCNT]
        ja      @n
@free:  sub     d, _STRRECSIZE
        jmp     _Freemem
@n:
end;

{ note: fpc_ansistr_compare/_equal do check the codepage and make a UTF-8
  conversion if necessary, whereas Delphi _LStrCmp/_LStrEqual don't;
  involving codepage is safer, but paranoid, and 1. is (much) slower,
  and 2. is not Delphi compatible -> we rather follow the Delphi's way }

function _ansistr_compare(s1, s2: pointer): SizeInt; nostackframe; assembler;
asm
        xor     eax, eax
        cmp     s1, s2
        je      @0
        test    s1, s2
        jz      @maybe0
@first: mov     al, byte ptr [s1] // we can check the first char (for quicksort)
        sub     al, byte ptr [s2]
        jne     @ne
        mov     r8, qword ptr [s1 - _STRLEN]
        mov     r11, r8
        sub     r8, qword ptr [s2 - _STRLEN] // r8 = length(s1)-length(s2)
        adc     rax, -1
        and     rax, r8  // rax = -min(length(s1),length(s2))
        sub     rax, r11
        sub     s1, rax
        sub     s2, rax
        align   16
@s:     mov     r10, qword ptr [s1 + rax] // compare by 8 bytes (may include len)
        xor     r10, qword ptr [s2 + rax]
        jnz     @d
        add     rax, 8
        js      @s
@e:     mov     rax, r8 // all equal -> return difflen
@0:     ret
@ne:    movsx   rax, al
        ret
@d:     bsf     r10, r10 // compute s1^-s2^
        shr     r10, 3
        add     rax, r10
        jns     @e
        movzx   edx, byte ptr [s2 + rax]
        movzx   eax, byte ptr [s1 + rax]
        sub     rax, rdx
        ret
@maybe0:test    s2, s2
        jz      @1
        test    s1, s1
        jnz     @first
        dec     rax
        ret
@1:     inc     eax
end;

function _ansistr_compare_equal(s1, s2: pointer): SizeInt; nostackframe; assembler;
asm
        xor     eax, eax
        cmp     s1, s2
        je      @q
        test    s1, s2
        jz      @maybe0
@ok:    mov     rax, qword ptr [s1 - _STRLEN] // len must match
        cmp     rax, qword ptr [s2 - _STRLEN]
        jne     @q
        lea     s1, qword ptr [s1 + rax - 8]
        lea     s2, qword ptr [s2 + rax - 8]
        neg     rax
        mov     r8, qword ptr [s1] // compare last 8 bytes (may include len)
        cmp     r8, qword ptr [s2]
        jne     @q
        align 16
@s:     add     rax, 8 // compare remaining 8 bytes per iteration
        jns     @0
        mov     r8, qword ptr [s1 + rax]
        cmp     r8, qword ptr [s2 + rax]
        je      @s
        mov     eax, 1
        ret
@0:     xor     eax, eax
@q:     ret
@maybe0:test    s2, s2
        jz      @1
        test    s1, s1
        jnz     @ok
@1:     inc     eax // not zero is enough
end;

procedure _dynarray_incr_ref(p: pointer); nostackframe; assembler;
asm
        test    p, p
        jz      @z
        cmp     qword ptr [p - _DAREFCNT], 0
        jle     @z
lock    inc     qword ptr [p - _DAREFCNT]
@z:
end;

procedure _dynarray_decr_ref_free(p: PDynArrayRec; Info: pointer);
begin
  Info := GetTypeData(Info)^.elTypeRef;
  if Info <> nil then
  begin
    Info := PPointer(Info)^;
    if Info <> nil then
      FastFinalizeArray(pointer(PAnsiChar(p) + SizeOf(p^)), Info, p^.high + 1);
  end;
  _Freemem(p);
end;

procedure _dynarray_decr_ref(var p: Pointer; info: pointer); nostackframe; assembler;
asm
        mov     rax, qword ptr [p]
        test    rax, rax
        jz      @z
        mov     qword ptr [p], 0
        mov     p, rax
        sub     p, SizeOf(TDynArrayRec)
        cmp     qword ptr [rax - _DAREFCNT], 0
        jle     @z
lock    dec     qword ptr [p]
        jbe     @free
@z:     ret
@free:  jmp     _dynarray_decr_ref_free
end;


{$ifdef FPC_HAS_CPSTRING} // optimized for systemcodepage=CP_UTF8

{$ifdef FPC_X64MM}
procedure _ansistr_setlength_new(var s: RawByteString; len: PtrInt; cp: cardinal);
var
  p, new: PAnsiChar;
  l: PtrInt;
begin
  if cp <= CP_OEMCP then // TranslatePlaceholderCP logic
    cp := DefaultSystemCodePage;
  new := FastNewString(len, cp);
  p := pointer(s);
  if p <> nil then
  begin
    l := PStrLen(p - _STRLEN)^ + 1;
    if l > len then
      l := len;
    MoveFast(p^, new^, l);
  end;
  FastAssignNew(s, new);
end;

procedure _ansistr_setlength(var s: RawByteString; len: PtrInt; cp: cardinal);
  nostackframe; assembler;
asm
        mov     rax, qword ptr [s]
        test    len, len
        jle     _ansistr_decr_ref
        test    rax, rax
        jz      _ansistr_setlength_new
        cmp     qword ptr [rax - _STRREFCNT], 1
        jne     _ansistr_setlength_new
        push    len
        push    s
        sub     qword ptr [s], _STRRECSIZE
        add     len, _STRRECSIZE + 1
        call    _reallocmem // rely on MM in-place detection
        pop     s
        pop     len
        add     qword ptr [s], _STRRECSIZE
        mov     qword ptr [rax].TStrRec.length, len
        mov     byte ptr [rax + len + _STRRECSIZE], 0
end;
{$endif FPC_X64MM}

function ToTempUtf8(var temp: TSynTempBuffer; p: pointer; len, cp: cardinal): pointer;
begin
  if (len = 0) or
     (cp = CP_UTF8) or
     (cp >= CP_RAWBLOB) or
     IsAnsiCompatible(p, len) then
  begin
    temp.buf := nil;
    temp.len := len;
    result := p;
  end
  else
  begin
    temp.Init(len * 3);
    p := TSynAnsiConvert.Engine(cp).AnsiBufferToUtf8(temp.buf, p, len);
    temp.len := PAnsiChar(p) - PAnsiChar(temp.buf);
    result := temp.buf;
  end;
end;

procedure _ansistr_concat_convert(var dest: RawByteString;
  const s1, s2: RawByteString; cp, cp1, cp2: cardinal);
var
  t1, t2, t: TSynTempBuffer; // avoid most memory allocation
  p1, p2, p: PAnsiChar;
  eng: TSynAnsiConvert;
begin
  p1 := ToTempUtf8(t1, pointer(s1), length(s1), cp1);
  p2 := ToTempUtf8(t2, pointer(s2), length(s2), cp2);
  if (cp = CP_UTF8) or
     (cp >= CP_RAWBLOB) or
     ((t1.buf = nil) and
      (t2.buf = nil)) then
  begin
    p := FastNewString(t1.len + t2.len, cp);
    MoveFast(p1^, p[0], t1.len);
    MoveFast(p2^, p[t1.len], t2.len);
    FastAssignNew(dest, p);
  end
  else
  begin
    eng := TSynAnsiConvert.Engine(cp);
    t.Init((t1.len + t2.len) shl eng.AnsiCharShift);
    p := eng.Utf8BufferToAnsi(eng.Utf8BufferToAnsi(t.buf, p1, t1.len), p2, t2.len);
    FastSetStringCP(dest, t.buf, p - t.buf, cp);
    t.Done;
  end;
  t2.Done;
  t1.Done;
end;

function _lstrlen(const s: RawByteString): TStrLen; inline;
begin
  result := PStrLen(PtrUInt(s) - _STRLEN)^;
end;

function _lstrcp(const s: RawByteString; cp: integer): integer; inline;
begin
  result := cp;
  if s <> '' then
  begin
    result := PStrRec(PtrUInt(s) - _STRRECSIZE)^.codePage;
    if result <= CP_OEMCP then
      result := CP_UTF8;
  end;
end;

procedure _ansistr_concat_utf8(var dest: RawByteString;
  const s1, s2: RawByteString; cp: cardinal);
var
  cp1, cp2: cardinal;
  new: PAnsiChar;
  l1: PtrInt;
begin
  if cp <= CP_OEMCP then // TranslatePlaceholderCP logic
    cp := CP_UTF8;
  cp1 := _lstrcp(s1, cp);
  cp2 := _lstrcp(s2, cp1);
  if (cp1 = cp2) and
     ((cp >= CP_RAWBLOB) or
      (cp = cp1)) then
    cp := cp1
  else if ((cp1 <> cp) and
           (cp1 < CP_RAWBLOB)) or
          ((cp2 <> cp) and
           (cp2 < CP_RAWBLOB)) then
  begin
    // we need to ensure that codepage handling is performed as expected
    _ansistr_concat_convert(dest, s1, s2, cp, cp1, cp2);
    exit;
  end;
  // we can safely concatenate the input with no conversion
  if s1 = '' then
    dest := s2
  else if s2 = '' then
    dest := s1
  else
  begin
    l1 := _lstrlen(s1);
    if pointer(s1) = pointer(dest) then
    begin
      // dest := dest+s2 -> self-resize dest
      SetLength(dest, l1 + _lstrlen(s2));
      PStrRec(PtrUInt(dest) - _STRRECSIZE)^.codepage := cp;
      MoveFast(pointer(s2)^, PByteArray(dest)[l1], _lstrlen(s2));
    end
    else
    begin
      new := FastNewString(l1 + _lstrlen(s2), cp);
      MoveFast(pointer(s1)^, new[0], l1);
      MoveFast(pointer(s2)^, new[l1], _lstrlen(s2));
      FastAssignNew(dest, new);
    end;
  end;
end;

procedure _ansistr_concat_multi_convert(var dest: RawByteString;
  const s: array of RawByteString; cp: cardinal);
var
  t: TBaseWriter;
  i: PtrInt;
  p: pointer;
  u: RawUtf8;
  tmp: TTextWriterStackBuffer;
begin
  t := TBaseWriter.CreateOwnedStream(tmp);
  try
    for i := 0 to high(s) do
    begin
      p := pointer(s[i]);
      if p <> nil then
        // use mormot.core.unicode efficient conversion
        t.AddNoJsonEscape(p, _lstrlen(RawByteString(p)), _lstrcp(RawByteString(p), cp));
    end;
    t.SetText(u);
  finally
    t.Free;
  end;
  if (cp = CP_UTF8) or
     (cp >= CP_RAWBLOB) then
    dest := u
  else
    TSynAnsiConvert.Engine(cp).Utf8BufferToAnsi(pointer(u), length(u), dest);
end;

procedure _ansistr_concat_multi_utf8(var dest: RawByteString;
  const s: array of RawByteString; cp: cardinal);
var
  first, len, l, i: TStrLen;
  cpf, cpi: cardinal;
  p: pointer;
  new: PAnsiChar;
begin
  if cp <= CP_OEMCP then
    cp := CP_UTF8;
  first := 0;
  repeat
    if first > high(s) then
    begin
      _ansistr_decr_ref(pointer(dest));
      exit;
    end;
    p := pointer(s[first]);
    if p <> nil then
      break;
    inc(first);
  until false;
  len := _lstrlen(RawByteString(p));
  cpf := _lstrcp(RawByteString(p), cp);
  if (cpf <> cp) and
     (cpf < CP_RAWBLOB) then
    cpf := 0
  else
    for i := first + 1 to high(s) do
    begin
      p := pointer(s[i]);
      if p <> nil then
      begin
        inc(len, _lstrlen(RawByteString(p)));
        cpi := PStrRec(PtrUInt(p) - _STRRECSIZE)^.codePage;
        if cpi <= CP_OEMCP then
          cpi := CP_UTF8;
        if (cpi <> cpf) and
           (cpi < CP_RAWBLOB) then
        begin
          cpf := 0;
          break;
        end;
      end;
    end;
  if cpf = 0 then
    // we need to ensure that codepage handling is performed as expected
    _ansistr_concat_multi_convert(dest, s, cp)
  else
  begin
    // we can safely concatenate the input with no conversion
    p := pointer(s[first]);
    l := _lstrlen(RawByteString(p));
    if p = pointer(dest) then
    begin
      // dest := dest+s... -> self-resize
      SetLength(dest, len);
      new := pointer(dest);
      PStrRec(PtrUInt(dest) - _STRRECSIZE)^.codepage := cp;
      cp := 0; // no FastAssignNew() below
    end
    else
    begin
      new := FastNewString(len, cp);
      MoveFast(p^, new[0], l);
    end;
    for i := first + 1 to high(s) do
    begin
      p := pointer(s[i]);
      if p <> nil then
      begin
        MoveFast(p^, new[l], _lstrlen(RawByteString(p)));
        inc(l, _lstrlen(RawByteString(p)));
      end;
    end;
    if cp <> 0 then
      FastAssignNew(dest, new);
  end;
end;

procedure _fpc_ansistr_concat(var a: RawUtf8);
begin
  a := a + a; // to generate "call fpc_ansistr_concat" opcode
end;

procedure _fpc_ansistr_concat_multi(var a: RawUtf8);
begin
  a := a + a + a; // to generate "call fpc_ansistr_concat_multi" opcode
end;

procedure RedirectRtl(dummy, dest: PByteArray);
begin
  // POSIX only ABI: Windows is never natively UTF-8
  repeat
    if (dummy[0] = $b9) and
       (PCardinal(@dummy[1])^ = CP_UTF8) then
      case dummy[5] of
        $e8:
          begin
            // found "mov ecx,65001; call fpc_ansistr_concat" opcodes
            RedirectCode(@dummy[PInteger(@dummy[6])^ + 10], dest);
            exit;
          end;
        $ba:
          if (PCardinal(@dummy[6])^ = 2) and
             (dummy[10] = $e8) then
          begin
            // found "mov ecx,65001; mov edx,2; call fpc_ansistr_concat_multi"
            RedirectCode(@dummy[PInteger(@dummy[11])^ + 15], dest);
            exit;
          end;
      end;
    inc(PByte(dummy));
  until PInt64(dummy)^ = 0;
end;

{$endif FPC_HAS_CPSTRING}

{$endif FPC_CPUX64}

