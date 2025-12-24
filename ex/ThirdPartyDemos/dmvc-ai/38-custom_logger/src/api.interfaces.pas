unit api.interfaces;

interface

uses
  mormot.core.interfaces;

type
  IApiService = interface(IInvokable)
    ['{CCCCCCCC-3838-3838-3838-383838383838}']
    function Index: RawUtf8;
    function Error: RawUtf8;
  end;

implementation

end.
