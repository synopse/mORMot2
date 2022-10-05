/// Framework Core Wrappers to High-Level MacOS API
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.os.mac;

{
  *****************************************************************************

   MacOS API calls for FPC, as injected to mormot.core.os.pas
  - Gather MacOS Specific Operating System Information
  
   This unit uses MacOSAll and link several toolkits, so is not included
   in mormot.core.os.pas to reduce executable size, but inject this methods
   at runtime: just include "uses mormot.core.os.mac" in programs needing it.

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

{$ifdef OSDARWIN} // do-nothing unit on other systems

{$modeswitch cvar}
{$linkframework IOKit}

uses
  classes,
  types,
  sysutils,
  ctypes,
  MacOSAll,
  mormot.core.base,
  mormot.core.os;


{ ****************** Gather MacOS Specific Operating System Information }

implementation


{ ****************** Gather MacOS Specific Operating System Information }

type
  kern_return_t = cint;
  natural_t = UInt32;
  mach_port_t = natural_t;
  io_object_t = mach_port_t;
  io_registry_entry_t = io_object_t;
  IOOptionBits = UInt32;
 
var
  kIOMasterPortDefault: mach_port_t; cvar; external;
 
function IORegistryEntryFromPath(
  masterPort: mach_port_t; const path: ): io_registry_entry_t; cdecl; external;
function IORegistryEntryCreateCFProperty(entry: io_registry_entry_t; key: CFStringRef;
  allocator: CFAllocatorRef; options: IOOptionBits): CFTypeRef; cdecl; external;
function IOObjectRelease(entry: io_registry_entry_t): kern_return_t; cdecl; external;

// see https://forum.lazarus.freepascal.org/index.php?topic=49358#msg357641
procedure _Get(id: PChar; var result: RawUtf8);
var
  uuidCf: CFStringRef;
  ioRegistryRoot: io_registry_entry_t;
begin
  ioRegistryRoot := IORegistryEntryFromPath(kIOMasterPortDefault, 'IOService:/');
  uuidCf := CFStringRef(IORegistryEntryCreateCFProperty(
    ioRegistryRoot, CFSTR(id), kCFAllocatorDefault, 0));
  IOObjectRelease(ioRegistryRoot);
  FastSetString(result, nil, 1024);
  CFStringGetCString(uuidCf,
    pointer(result), length(result), kCFStringEncodingMacRoman);
  CFRelease(uuidCf);
  SetLength(result, StrLen(pointer(result)));
  if result <> '' then
    writeln(id,' = ', result);
end;

function _GetSmbios(info: TSmbiosBasicInfo): RawUtf8;
begin
  case info of
    sbiUuid:
      begin
        _Get('IOPlatformUUID', result);
        if PCardinal(result)^ =
             ord('F') + ord('F') shl 8 + ord('F') shl 16 + ord('F') shl 24 then
          result := '' // fake ID e.g. on iOS 7+
      end;
    sbiSerial:
      _Get('IOPlatformSerialNumber', result);
    sbiBoardSerial:
      _Get('board-id', result);
    sbiManufacturer:
      _Get('manufacturer', result);
    sbiProductName:
      _Get('product-name', result);
  else
    result := '';
  end;
end;


initialization
  PosixInject.GetSmbios := _GetSmbios;

{$else}
implementation
{$endif OSDARWIN}

end.

