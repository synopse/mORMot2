{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit mormot2ui;

{$warn 5023 off : no warning about unused units}

interface

uses
  mormot.ui.grid.orm, mormot.ui.controls, mormot.ui.core, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('mormot2ui', @Register);
end.
