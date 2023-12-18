/// Main Process of the Command Line "mORMot GET" tool
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.tools.mget;


interface

{$I ..\..\mormot.defines.inc}

uses
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.core.log,
  mormot.core.data,
  mormot.crypt.secure,
  mormot.crypt.core,
  mormot.net.sock,
  mormot.net.client,
  mormot.net.server;


{ ****************  }

type
  /// state engine for mget processing
  TMGetProcess = class(TPersistentAutoCreateFields)
  protected
    fPeerSettings: THttpPeerCacheSettings;
  public
    /// input parameters for the MGet process
    Verbose, Peer: boolean;
    Url: RawUtf8;
  published
    property PeerSettings: THttpPeerCacheSettings
      read fPeerSettings write fPeerSettings;
  end;


implementation

{ TMGetProcess }


initialization

end.
