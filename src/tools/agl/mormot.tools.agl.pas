/// Main Process of the Command Line agl Tool
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.tools.agl;

{
  *****************************************************************************

  Angelize (`agl`) tool is able to run one or several executables as daemon/services
  - 

  *****************************************************************************
}

interface

{$I ..\..\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.text,
  mormot.core.unicode,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.net.client, // for http: requests
  mormot.app.console,
  mormot.app.daemon,
  mormot.app.agl;


{ ****************  }

implementation

end.
