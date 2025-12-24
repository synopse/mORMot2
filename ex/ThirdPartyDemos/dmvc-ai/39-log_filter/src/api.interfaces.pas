unit api.interfaces;

interface

uses
  mormot.core.base,
  mormot.rest.core;

type
  ILogFilterApi = interface(IInvokable)
    ['{F8E3A1B2-C4D5-4E6F-A7B8-C9D0E1F2A3B4}']
    
    /// Returns HTML welcome page - gets logged
    function Index: RawUtf8;
    
    /// Returns current timestamp - filtered from logs
    function NotLogged: RawUtf8;
    
    /// Demonstrates verbose logging
    function VerboseOnly: RawUtf8;
    
    /// Demonstrates all log levels
    function TestLevels: RawUtf8;
  end;

implementation

end.
