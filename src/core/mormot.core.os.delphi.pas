/// Framework Core POSIX API Wrappers for Delphi
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.os.delphi;

{
  *****************************************************************************

   Map FPC cross-platform API types and functions into Delphi POSIX units
   - Core POSIX Operating Systems API for Delphi
   - Network POSIX Operating Systems API for Delphi
  
   This unit is called by mormot.core.os.posix.inc and mormot.net.sock.posix.inc
   as a compatibility layer with the FPC POSIX units.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

{$ifdef POSIXDELPHI} // do-nothing unit on FPC or Delphi for Windows
uses
  classes,
  types,
  sysutils,
  mormot.core.base,
  // the Delphi System and POSIX units exposed by this unit
  System.IOUtils,
  System.SyncObjs,
  Posix.Dlfcn;


{ ****************** Core POSIX Operating Systems API for Delphi }

// in the code below, PChar = PWideChar so those wrapper functions could make the
// proper temporary conversion from UTF-16 to UTF-8 before calling the POSIX API

function dlopen(Name: PWideChar; Flags: integer): pointer;
function dlsym(Lib: pointer; Name: PAnsiChar): pointer;
function dlclose(Lib: pointer) : integer;
function dlerror: UnicodeString;

const
  RTLD_LAZY = Posix.Dlfcn.RTLD_LAZY;


{ ****************** Network POSIX Operating Systems API for Delphi }


implementation

uses
  mormot.core.os;


{ ****************** Core POSIX Operating Systems API for Delphi }

function dlopen(Name: PWideChar; Flags: integer): pointer;
var
  tmp: TSynTempBuffer;
begin
  result := pointer(Posix.Dlfcn.dlopen(Unicode_ToUtf8(Name, tmp), Flags));
  tmp.Done;
end;

function dlsym(Lib: pointer; Name: PAnsiChar): pointer;
begin
  result := Posix.Dlfcn.dlsym(PtrUInt(Lib), Name);
end;

function dlclose(Lib: pointer) : integer;
begin
  result := Posix.Dlfcn.dlclose(PtrUInt(Lib));
end;

function dlerror: UnicodeString;
begin
  result := UnicodeString(Posix.Dlfcn.dlerror);
end;

{ ****************** Network POSIX Operating Systems API for Delphi }

initialization

{$else}
implementation
{$endif POSIXDELPHI}

end.

