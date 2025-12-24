unit api.interfaces;

{$I mormot.defines.inc}

interface

uses
  mormot.core.base,
  mormot.core.interfaces;

type
  /// Profiling API interface
  // Demonstrates performance monitoring in mORMot2 REST services
  IProfilingApi = interface(IInvokable)
    ['{E3F7C8A1-9B2D-4E5F-A6C7-D8E9F0A1B2C3}']

    /// Index endpoint with recursive call profiling
    function Index: RawUtf8;

    /// Demonstrates nested profiling with DoSomething/DoSomethingElse
    function ProfilerSample1: RawUtf8;

    /// Simple profiling demo with sleep
    function ProfilerSample2: RawUtf8;
  end;


implementation

uses
  mormot.core.rtti;

initialization
  // Register the interface for mORMot2 service resolution
  TInterfaceFactory.RegisterInterfaces([TypeInfo(IProfilingApi)]);

end.
