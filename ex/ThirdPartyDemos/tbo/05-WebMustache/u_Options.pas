// Author: Thomas Bogenrieder
// The example is a proof of concept for working with the mORMot library. Source code is neither tested nor optimized.

unit u_Options;

{$I mormot.defines.inc}

interface

uses
  System.SysUtils,
  mormot.core.base,
  mormot.core.data,
  mormot.core.json,
  mormot.core.os;

type
  TServerOptions = class(TSynPersistent)
  strict private
    FPort: RawUtf8;
    function GetPort: RawUtf8;
  published
    property Port: RawUtf8
      read GetPort write FPort;
  end;

  TOptions = class(TSynAutoCreateFields)
  strict private  
    FServer: TServerOptions;
    FLastProjectFileName: TFileName;
  private
    const
      MAIN_SECTION = 'Options';
  protected
    procedure Load;
  public
    procedure Save;
  published
    property Server: TServerOptions
      read FServer;
    property LastProjectFileName: TFileName
      read FLastProjectFileName write FLastProjectFileName;
  end;

function Options: TOptions;


implementation

var
  __Options: TOptions;

//==============================================================================

function Options: TOptions;
begin
  if __Options = Nil then
  begin
    __Options := TOptions.Create;
    __Options.Load;
  end;
  
  Result := __Options;
end;


//==============================================================================
// TServerOptions
//==============================================================================

function TServerOptions.GetPort: RawUtf8;
begin
  Result := FPort;
  if Result = '' then
    Result := '8089';
end;


//==============================================================================
// TOptions
//==============================================================================

procedure TOptions.Load;
begin
  IniToObject(StringFromFile(ChangeFileExt(Executable.ProgramFileName, '.ini')), Self, MAIN_SECTION);
end;


procedure TOptions.Save;
begin
  FileFromString(ObjectToIni(Self, MAIN_SECTION), ChangeFileExt(Executable.ProgramFileName, '.ini'), {FlushOnDisk=} True);
end;


//==============================================================================

initialization

finalization
  FreeAndNil(__Options);

end.