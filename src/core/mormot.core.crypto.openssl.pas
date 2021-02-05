/// Framework Core Cryptographic Process using OpenSSL
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.core.crypto.openssl;

{
  *****************************************************************************

   High-Performance Cryptographic Features using OpenSSL 1.1.1
    - OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG)

  *****************************************************************************

  Our mormot.core.crypto.pas unit is stand-alone, and pretty fast.
  But using OpenSSL 1.1.1 libcrypto could induce even better performance,
  especially on systems where OpenSSL is already available, like most POSIX.

}

interface

{$I ..\mormot.defines.inc}

uses
  classes,
  sysutils,
  mormot.core.base,
  mormot.core.os,
  mormot.core.rtti,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.crypto,
  mormot.lib.openssl11;


{ ************** OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG) }

type
  /// TAesPrng-compatible class using OpenSSL 1.1.1
  // - may be used instead of TAesPrng if a "proven" generator is required -
  // you could override MainAesPrng global variable
  // - mormot.core.crypto TAesPrng is faster, especially for small output:
  // $ OpenSSL Random32 in 313.10ms i.e. 319,378/s, aver. 3us, 1.2 MB/s
  // $ OpenSSL FillRandom in 334us, 285.5 MB/s
  // $ mORMot Random32 in 4.76ms i.e. 21,003,990/s, aver. 0us, 80.1 MB/s
  // $ mORMot FillRandom in 212us, 449.8 MB/s
  TAesPrngOpenSsl = class(TAesPrngAbstract)
  public
    /// initialize the CSPRNG using OpenSSL 1.1.1
    // - if the library is not available, will raise an Exception
    constructor Create; override;
    /// fill a binary buffer with some pseudorandom data
    // - this method is thread-safe
    // - is just a wrapper around RAND_bytes() API call
    procedure FillRandom(Buffer: pointer; Len: PtrInt); override;
    /// returns the single system-wide instance of TAesPrngOpenSsl
    // - if you need to generate some random content, just call the
    // TAesPrngOpenSsl.Main.FillRandom() overloaded methods, or directly
    // TAesPrngOpenSsl.Fill() class methods
    class function Main: TAesPrngAbstract; override;
  end;


implementation


{ ************** OpenSSL Cryptographic Pseudorandom Number Generator (CSPRNG) }

{ TAesPrngOpenSsl }

constructor TAesPrngOpenSsl.Create;
var
  test: shortstring;
begin
  inherited Create;
  if not OpenSslIsAvailable or
     (RAND_bytes(@test, SizeOf(test)) <> 1) then
    raise ESynCrypto.CreateUtf8('%.Create: OpenSSL 1.1.1 not available', [self]);
end;

procedure TAesPrngOpenSsl.FillRandom(Buffer: pointer; Len: PtrInt);
begin
  inc(fTotalBytes, Len);
  RAND_bytes(Buffer, Len);
end;

var
  MainAesPrngOpenSsl: TAesPrngOpenSsl;

class function TAesPrngOpenSsl.Main: TAesPrngAbstract;
begin
  result := MainAesPrngOpenSsl;
  if result = nil then
  begin
    if not OpenSslIsAvailable then
      raise ESynCrypto.CreateUtf8('%.Main: OpenSSL 1.1.1 not available', [self]);
    GlobalLock;
    try
      if MainAesPrngOpenSsl = nil then
        MainAesPrngOpenSsl := TAesPrngOpenSsl.Create;
    finally
      GlobalUnLock;
    end;
    result := MainAesPrngOpenSsl;
  end;
end;


initialization

finalization
  FreeAndNil(MainAesPrngOpenSsl);

end.
