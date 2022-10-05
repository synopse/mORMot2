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
  io_service_t = io_object_t;
  io_string_t = array[0..511] of AnsiChar;
  IOOptionBits = UInt32;
 
var
  kIOMasterPortDefault: mach_port_t; cvar; external;
 
function IORegistryEntryFromPath(
  masterPort: mach_port_t; const path: io_string_t): io_registry_entry_t; cdecl; external;
function IORegistryEntryCreateCFProperty(entry: io_registry_entry_t; key: CFStringRef;
  allocator: CFAllocatorRef; options: IOOptionBits): CFTypeRef; cdecl; external;
function IOObjectRelease(entry: io_registry_entry_t): kern_return_t; cdecl; external;
function IOServiceMatching(name: PAnsiChar): CFMutableDictionaryRef; cdecl; external;
function IOServiceGetMatchingService(masterPort: mach_port_t;
  matching: CFMutableDictionaryRef): io_service_t; cdecl; external;
function IORegistryEntryCreateCFProperties(entry: io_registry_entry_t;
  var properties: CFMutableDictionaryRef; allocator: CFAllocatorRef;
  options: IOOptionBits): integer; cdecl; external;

// see https://forum.lazarus.freepascal.org/index.php?topic=49358#msg357641
procedure _Get(id: PChar; out result: RawUtf8);
var
  ref, str: pointer;
  root: io_registry_entry_t;
  typeID: CFTypeID;
begin
  try
    root := IORegistryEntryFromPath(kIOMasterPortDefault, 'IOService:/');
    if root = 0 then
      exit;
    ref := IORegistryEntryCreateCFProperty(root, CFSTR(id), kCFAllocatorDefault, 0);
    IOObjectRelease(root);
    if ref = nil then
      exit;
    typeID := CFGetTypeID(ref);
    if typeID = CFStringGetTypeID then
    begin
      FastSetString(result, nil, 1024);
      CFStringGetCString(ref, pointer(result), length(result), kCFStringEncodingMacRoman);
      SetLength(result, StrLen(pointer(result)));
    end
    else if typeID = CFDataGetTypeID then
    begin
      str := CFDataGetBytePtr(ref);
      FastSetString(result, str, StrLen(str));
    end;
    CFRelease(ref);
    writeln(id,' = ',result);
  except
  end;
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
    sbiVersion:
      _Get('model', result);
    sbiFamily:
      _Get('target-type', result);
  else
    result := '';
  end;
end;

function _GetSmbiosData: RawByteString;
var
  ref: pointer;
  root: io_registry_entry_t;
  serv: io_service_t;
  prop: CFMutableDictionaryRef;
begin
  result := '';
  try
    // first try the IORegistryEntryFromPath() way
    // - see https://github.com/acidanthera/dmidecode
    root := IORegistryEntryFromPath(kIOMasterPortDefault,
      'IOService:/AppleACPIPlatformExpert/bios/AppleSMBIOS');
    if root <> 0 then
    begin
      ref := IORegistryEntryCreateCFProperty(
        root, CFSTR('SMBIOS'), kCFAllocatorDefault, 0);
      IOObjectRelease(root);
      if (ref <> nil) and
         (CFGetTypeID(ref) = CFDataGetTypeID) then
        FastSetRawByteString(result, CFDataGetBytePtr(ref), CFDataGetLength(ref));
      if ref <> nil then
        CFRelease(ref);
      if result <> '' then
        exit;
    end;
  except
  end;
  try
    // then try the IOServiceGetMatchingService() way
    // - see https://github.com/cavaliercoder/dmidecode-osx
    serv := IOServiceGetMatchingService(kIOMasterPortDefault,
              IOServiceMatching('AppleSMBIOS'));
    if serv = 0 then
      exit;
    prop := nil;
    IORegistryEntryCreateCFProperties(serv, prop, kCFAllocatorDefault, 0);
    IOObjectRelease(serv);
    if prop = nil then
      exit;
    ref := nil;
    CFDictionaryGetValueIfPresent(prop, CFSTR('SMBIOS'), @ref);
    try
      CFRelease(prop);
    except
      // throws 'Segmentation fault: 11' since macOS 10.12, if the compiled
      // binary is not signed with an Apple developer profile :(
    end;
    if ref = nil then
      exit;
    if CFGetTypeID(ref) = CFDataGetTypeID then
      FastSetRawByteString(result, CFDataGetBytePtr(ref), CFDataGetLength(ref));
    try
      CFRelease(ref);
    except
      // buggy Apple
    end;
  except
    result := '';
  end;
end;


initialization
  PosixInject.GetSmbios := _GetSmbios;
  PosixInject.GetSmbiosData := _GetSmbiosData;

{$else}
implementation
{$endif OSDARWIN}

end.

