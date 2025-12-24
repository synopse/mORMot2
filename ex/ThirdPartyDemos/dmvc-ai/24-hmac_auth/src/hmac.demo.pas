unit hmac.demo;

{$I mormot.defines.inc}

interface

uses
  SysUtils,
  mormot.core.base,
  mormot.core.text,
  mormot.crypt.core,
  mormot.crypt.secure;

/// Run the HMAC demonstration - test all supported algorithms
procedure RunHmacDemo;

implementation

type
  /// Test vector for HMAC validation
  THmacTestVector = record
    Algorithm: string;
    Expected: string;
  end;

const
  /// Test input data (same as DMVC sample)
  TEST_INPUT = 'Daniele Teti';
  TEST_KEY = 'daniele';

  /// Expected HMAC signatures (from DMVC sample)
  TEST_VECTORS: array[0..4] of THmacTestVector = (
    (Algorithm: 'MD5';    Expected: '5256311089fa9c80f735fb8cc28bf4fe'),
    (Algorithm: 'SHA1';   Expected: '323ff5f4e53c43f2d9342952299a9d35f9ee5dc2'),
    (Algorithm: 'SHA224'; Expected: '2f42e18342d2d35afc9942364caec009e1ace1d1695c3e9178e65e35'),
    (Algorithm: 'SHA256'; Expected: '1f75a969e2b9c43e6d06969dfad2088f9aab68d3aa440904d2ed8710e2f8e38b'),
    (Algorithm: 'SHA512'; Expected: '22465b5f4138ab80801ff8eca8dd99a56844dd7dc54f76d38bb02bdd815596fc5859709ba4f7130c299a626864a84a4a79401f529d44c85a894fcd7e6192eee9')
  );

/// Compute HMAC signature using specified algorithm
function ComputeHmac(const Algorithm, Input, Key: RawUtf8): RawUtf8;
var
  digest: THash512Rec;
  digestLen: integer;
begin
  // Use mormot.crypt.secure HMAC functions
  // Note: mORMot2 doesn't provide HMAC-MD5 as MD5 is deprecated
  if SameText(Algorithm, 'MD5') then
  begin
    // MD5 not supported in mORMot2 HMAC - would need manual implementation
    raise Exception.Create('HMAC-MD5 not supported (MD5 is deprecated)');
  end
  else if SameText(Algorithm, 'SHA1') then
  begin
    mormot.crypt.secure.Hmac(saSha1, pointer(Key), pointer(Input), length(Key), length(Input), @digest);
    digestLen := SizeOf(TSha1Digest);
  end
  else if SameText(Algorithm, 'SHA224') then
  begin
    mormot.crypt.secure.Hmac(saSha224, pointer(Key), pointer(Input), length(Key), length(Input), @digest);
    digestLen := SizeOf(THash224);
  end
  else if SameText(Algorithm, 'SHA256') then
  begin
    mormot.crypt.secure.Hmac(saSha256, pointer(Key), pointer(Input), length(Key), length(Input), @digest);
    digestLen := SizeOf(TSha256Digest);
  end
  else if SameText(Algorithm, 'SHA384') then
  begin
    mormot.crypt.secure.Hmac(saSha384, pointer(Key), pointer(Input), length(Key), length(Input), @digest);
    digestLen := SizeOf(TSha384Digest);
  end
  else if SameText(Algorithm, 'SHA512') then
  begin
    mormot.crypt.secure.Hmac(saSha512, pointer(Key), pointer(Input), length(Key), length(Input), @digest);
    digestLen := SizeOf(TSha512Digest);
  end
  else
    raise Exception.CreateFmt('Unknown HMAC algorithm: %s', [Algorithm]);

  // Convert to hex string
  Result := LowerCase(BinToHex(@digest, digestLen));
end;

procedure RunHmacDemo;
var
  i: integer;
  computed: RawUtf8;
  expected: RawUtf8;
  success: boolean;
  successCount: integer;
begin
  WriteLn('Running HMAC cryptographic tests...');
  WriteLn('Input:  "', TEST_INPUT, '"');
  WriteLn('Key:    "', TEST_KEY, '"');
  WriteLn;

  successCount := 0;

  for i := Low(TEST_VECTORS) to High(TEST_VECTORS) do
  begin
    computed := ComputeHmac(TEST_VECTORS[i].Algorithm, TEST_INPUT, TEST_KEY);
    expected := TEST_VECTORS[i].Expected;
    success := SameText(computed, expected);

    if success then
      Inc(successCount);

    if success then
      WriteLn(Format('%-10s [OK]', [TEST_VECTORS[i].Algorithm + ':']))
    else
      WriteLn(Format('%-10s [FAIL]', [TEST_VECTORS[i].Algorithm + ':']));
    WriteLn('  Expected: ', expected);
    WriteLn('  Computed: ', computed);
    if not success then
      WriteLn('  *** MISMATCH ***');
    WriteLn;
  end;

  WriteLn('=======================================');
  WriteLn(Format('Results: %d/%d tests passed',
    [successCount, Length(TEST_VECTORS)]));
  WriteLn('=======================================');
end;

end.
