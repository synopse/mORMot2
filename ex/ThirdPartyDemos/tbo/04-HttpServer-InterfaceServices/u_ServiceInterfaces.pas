// Author: Thomas Bogenrieder
// The example is a proof of concept for creating interface-based services with mORMot, the source code is neither tested nor optimized.

unit u_ServiceInterfaces;

{$I mormot.defines.inc}

interface

uses
  System.SysUtils,
  mormot.core.base,
  mormot.core.rtti,
  mormot.core.interfaces;

const
  ROOT_NAME_FILE = 'File';

type
  IDocument = interface(IInvokable)
  ['{691473CE-1695-440A-A212-F334C466974A}']
    function Load(const pmcName: TFileName; out pmoData: RawBlob): Boolean;
    function Save(const pmcName: TFileName; const pmcData: RawBlob): Boolean;
    procedure GetAllNames(out pmoFileNames: TFileNameDynArray);
  end;


implementation

//==============================================================================

initialization

  TInterfaceFactory.RegisterInterfaces([
    TypeInfo(IDocument)]);

end.