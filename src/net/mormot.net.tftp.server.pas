/// TFTP Server-Side Process 
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.net.tftp.server;

{
  *****************************************************************************

    TFTP Server Processing with RFC 1350/2347/2348/2349/7440 Support
    - TServerTftp Asynchronous Server Class

  *****************************************************************************

}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode, // for efficient UTF-8 text process within HTTP
  mormot.core.text,
  mormot.core.data,
  mormot.core.log,
  mormot.core.threads,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.buffers,
  mormot.net.sock,
  mormot.net.server;




{ ******************** TServerTftp Asynchronous Server Class }

implementation

end.

