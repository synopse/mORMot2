/// Main Process of the Command Line Public Key Cryptography ecc Tool
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.tools.ecc;

{
  *****************************************************************************

  Manages certificate-based public-key cryptography using ECC-secp256r1
  - Implement End-User Commands
  - High-Level Command-Line Process 

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
  mormot.core.variants,
  mormot.core.rtti,
  mormot.core.json,
  mormot.crypt.secure,
  mormot.core.search,
  mormot.crypt.ecc,
  mormot.crypt.ecc256r1,
  mormot.crypt.core,
  mormot.app.console;
  

{ **************** Implement End-User Commands }

/// end-user command to create a new private/public key file
// - as used in the ecc.dpr command-line tool
function EccCommandNew(const AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const Issuer: RawUtf8; StartDate: TDateTime; ExpirationDays: integer;
  const SavePassword: RawUtf8; SavePasswordRounds, SplitFiles: integer): TFileName;

/// end-user command to create a renew a .private file password
// - as used in the ecc.dpr command-line tool
function EccCommandRekey(const AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const SavePassword: RawUtf8; SavePasswordRounds: integer): TFileName;

/// end-user command to sign a file using a private key file
// - as used in the ecc.dpr command-line tool
function EccCommandSignFile(const FileToSign, AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const MetaNameValuePairs: array of const): TFileName;

/// end-user command to verify a file signature
// - as used in the ecc.dpr command-line tool
function EccCommandVerifyFile(const FileToVerify, AuthPubKey: TFileName;
  const AuthBase64: RawUtf8): TEccValidity;

/// end-user command to create a .inc pascal source file from a private key file
// - ready to be included within the executable binary as private secret
// - as used in the ecc.dpr command-line tool
function EccCommandSourceFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const ConstName, Comment, PassWord: RawUtf8): TFileName;

/// end-user command to create a .json base-64 text array from a set of public key files
// - ready to be included e.g. as settings of any server
// - EccCommandChainCertificates(['*']) will create a 'chain.ca' of all
// public key files in the current folder
// - as used in the ecc.dpr command-line tool
function EccCommandChainCertificates(
  const CertFiles: array of RawUtf8): TFileName;

/// end-user command to display the json information from a .private file
// - as used in the ecc.dpr command-line tool
function EccCommandInfoPrivFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer): RawUtf8;

/// end-user command to encrypt a file with the .synecc format
// - as used in the ecc.dpr command-line tool
procedure EccCommandCryptFile(
  const FileToCrypt, DestFile, AuthPubKey: TFileName;
  const AuthBase64, AuthSerial, Password: RawUtf8; PasswordRounds: integer;
  Algo: TEciesAlgo = ecaUnknown);

/// end-user command to decrypt a .synecc file
// - as used in the ecc.dpr command-line tool
// - if AuthPrivKey is not set, it will search for the stored TEccCertificate.Serial
function EccCommandDecryptFile(
  const FileToDecrypt, DestFile, AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const DecryptPassword: RawUtf8; DecryptPasswordRounds: integer;
  Signature: PEccSignatureCertifiedContent; MetaData: PRawJson): TEccDecrypt;

/// end-user command to verify a .synecc file signature, after decryption
// - as used in the ecc.dpr command-line tool
// - the supplied signature can be retrieved from EccCommandDecryptFile()
function EccCommandVerifyDecryptedFile(
  const FileToVerify: TFileName;
  const Signature: TEccSignatureCertifiedContent): TEccValidity;

/// end-user command to initialize local cheat.private/.public files
// - as used in the ecc.dpr command-line tool
// - those files will let new/rekey commands create a .cheat file, encrypted
// using the master cheat.public key, so that the password of the generated
// .private key file could be retrieved using the cheat.private key and its
// password/round, via EccCommandCheat (ECC cheat)
// - it may be convenient to remember a single password instead of several,
// and security will be enhanced by the fact that cheat.private stays hidden
function EccCommandCheatInit(const Issuer: RawUtf8;
  const CheatPassword: RawUtf8; CheatRounds: integer): TFileName;

/// end-user command to display a .private password as stored in its .cheat file
// - as used in the ecc.dpr command-line tool
// - using the master password/rounds of the local cheat.private key, as
// generated by EccCommandCheatInit (ECC cheatinit)
function EccCommandCheat(const PrivateFile: TFileName;
  const CheatPassword: RawUtf8; CheatRounds: integer;
  out authpass: RawUtf8; out authround: integer): RawUtf8;

/// end-user command to encrypt a file with the symetric .synaead format
// - will use symetric encryption via AES-256-CFB/PKCS7 over Pbkdf2HmacSha256
// - as used in the ecc.dpr command-line tool
procedure AeadCommandCryptFile(const FileToCrypt, DestFile: TFileName;
  const Password, PasswordSalt: RawUtf8; PasswordRounds: integer);

/// end-user command to decrypt a symetric .synaead file
// - will use symetric encryption via AES-256-CFB/PKCS7 over Pbkdf2HmacSha256
// - as used in the ecc.dpr command-line tool
procedure AeadCommandDecryptFile(const FileToDecrypt, DestFile: TFileName;
  const Password, PasswordSalt: RawUtf8; PasswordRounds: integer);

type
  /// the actions implemented by EccCommand()
  // - as used in the ecc.dpr command-line tool
  // - retrieved from the command line as first parameter
  TEccCommand = (
    ecHelp,
    ecNew,
    ecRekey,
    ecSign,
    ecVerify,
    ecSource,
    ecInfoPriv,
    ecChain,
    ecChainAll,
    ecCrypt,
    ecDecrypt,
    ecInfoCrypt,
    ecAeadCrypt,
    ecAeadDecrypt,
    ecCheatInit,
    ecCheat);

  /// the result code returned by EccCommand()
  // - as used in the ecc.dpr command-line tool
  TEccCommandError = (
    eccSuccess,
    eccUnknownCommand,
    eccValidationError,
    eccError,
    eccException);


{ **************** High-Level Command-Line Process }

/// execute the encryption process corresponding to the command line options
// - as used in the ecc.dpr tool in this folder
// - returns the ExitCode expected value (0=eccSuccess)
function EccCommand(cmd: TEccCommand; const sw: ICommandLine): TEccCommandError;

const
  CHEAT_FILEEXT = '.cheat';
  CHEAT_FILEMASTER = 'cheat';
  CHEAT_ROUNDS = 100000;
  CHEAT_SPLIT = 100;

  AEAD_FILEEXT = '.synaead';
  DEFAULT_AEADROUNDS = 60000;


implementation


{ **************** Implement End-User Commands }

procedure CreateCheatFile(secret: TEccCertificateSecret;
  const SavePassword: RawUtf8; SavePasswordRounds: integer);
var
  json, bin: RawByteString;
  master: TEccCertificate;
  fn: TFileName;
begin
  master := TEccCertificate.Create;
  try
    if master.FromFile(CHEAT_FILEMASTER) then
    try
      if SavePasswordRounds = DEFAULT_ECCROUNDS then
        json := SavePassword
      else
        json := JsonEncode(['pass', SavePassword, 'rounds', SavePasswordRounds]);
      bin := TAesPrng.Main.AFSplit(pointer(json)^, length(json), CHEAT_SPLIT);
      fn := Utf8ToString(secret.Serial) + CHEAT_FILEEXT;
      FileFromString(master.Encrypt(bin), fn);
    finally
      FillZero(json);
      FillZero(bin);
    end;
  finally
    master.Free;
  end;
end;

function EccCommandNew(const AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const Issuer: RawUtf8; StartDate: TDateTime; ExpirationDays: integer;
  const SavePassword: RawUtf8; SavePasswordRounds: integer;
  SplitFiles: integer): TFileName;
var
  auth, new: TEccCertificateSecret;
begin
  if AuthPrivKey = '' then
    auth := nil
  else
    auth := TEccCertificateSecret.CreateFromSecureFile(
      AuthPrivKey, AuthPassword, AuthPasswordRounds);
  try
    // generate pair
    new := TEccCertificateSecret.CreateNew(
      auth, Issuer, ExpirationDays, StartDate);
    try
      // save private key as .private password-protected binary file
      new.SaveToSecureFiles(
        SavePassword, '.', SplitFiles, 64, SavePasswordRounds);
      CreateCheatFile(new, SavePassword, SavePasswordRounds);
      // save public key as .public JSON file
      result := ChangeFileExt(
        new.SaveToSecureFileName, ECCCERTIFICATEPUBLIC_FILEEXT);
      new.ToFile(result);
    finally
      new.Free;
    end;
  finally
    auth.Free;
  end;
end;

function EccCommandRekey(const AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const SavePassword: RawUtf8; SavePasswordRounds: integer): TFileName;
var
  auth: TEccCertificateSecret;
begin
  auth := TEccCertificateSecret.CreateFromSecureFile(
    AuthPrivKey, AuthPassword, AuthPasswordRounds);
  try
    auth.SaveToSecureFile(SavePassword, '.', 64, SavePasswordRounds);
    CreateCheatFile(auth, SavePassword, SavePasswordRounds);
    result := auth.SaveToSecureFileName;
  finally
    auth.Free;
  end;
end;

function EccCommandSignFile(const FileToSign, AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const MetaNameValuePairs: array of const): TFileName;
var
  auth: TEccCertificateSecret;
begin
  auth := TEccCertificateSecret.CreateFromSecureFile(
    AuthPrivKey, AuthPassword, AuthPasswordRounds);
  try
    result := auth.SignFile(FileToSign, MetaNameValuePairs);
  finally
    auth.Free;
  end;
end;

function EccCommandSourceFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const ConstName, Comment, PassWord: RawUtf8): TFileName;
var
  auth: TEccCertificateSecret;
begin
  auth := TEccCertificateSecret.CreateFromSecureFile(
    AuthPrivKey, AuthPassword, AuthPasswordRounds);
  try
    result := AuthPrivKey + '.inc';
    FileFromString(auth.SaveToSource(ConstName, Comment, PassWord), result);
  finally
    auth.Free;
  end;
end;

function EccCommandVerifyFile(const FileToVerify, AuthPubKey: TFileName;
  const AuthBase64: RawUtf8): TEccValidity;
var
  content: RawByteString;
  auth: TEccCertificate;
  cert: TEccSignatureCertified;
begin
  content := StringFromFile(FileToVerify);
  if content = '' then
    raise EECCException.CreateUtf8('File not found: %', [FileToVerify]);
  cert := TEccSignatureCertified.CreateFromFile(FileToVerify);
  try
    if not cert.Check then
    begin
      result := ecvInvalidSignature;
      exit;
    end;
    auth := TEccCertificate.Create;
    try
      if auth.FromAuth(AuthPubKey, AuthBase64, cert.AuthoritySerial) then
        result := cert.Verify(auth, pointer(content), length(content))
      else
        result := ecvUnknownAuthority;
    finally
      auth.Free;
    end;
  finally
    cert.Free;
  end;
end;

function EccCommandVerifyDecryptedFile(const FileToVerify: TFileName;
  const Signature: TEccSignatureCertifiedContent): TEccValidity;
var
  content: RawByteString;
  auth: TEccCertificate;
  cert: TEccSignatureCertified;
begin
  content := StringFromFile(FileToVerify);
  if content = '' then
    raise EECCException.CreateUtf8('File not found: %', [FileToVerify]);
  cert := TEccSignatureCertified.CreateFrom(Signature);
  try
    auth := TEccCertificate.Create;
    try
      result := ecvUnknownAuthority;
      if auth.FromAuth('', '', cert.AuthoritySerial) then
        result := cert.Verify(auth, pointer(content), length(content));
    finally
      auth.Free;
    end;
  finally
    cert.Free;
  end;
end;

procedure EccCommandCryptFile(
  const FileToCrypt, DestFile, AuthPubKey: TFileName;
  const AuthBase64, AuthSerial, Password: RawUtf8; PasswordRounds: integer;
  Algo: TEciesAlgo);
var
  content: RawByteString;
  auth: TEccCertificate;
begin
  content := StringFromFile(FileToCrypt);
  if content = '' then
    raise EECCException.CreateUtf8('File not found: %', [FileToCrypt]);
  auth := TEccCertificate.Create;
  try
    if not auth.FromAuth(AuthPubKey, AuthBase64, AuthSerial) then
      raise EECCException.Create('No public key');
    if not auth.EncryptFile(
       FileToCrypt, DestFile, Password, PasswordRounds, Algo, true) then
      raise EECCException.CreateUtf8('EncryptFile failed for %', [FileToCrypt]);
  finally
    auth.Free;
    FillZero(content);
  end;
end;

function EccCommandDecryptFile(
  const FileToDecrypt, DestFile, AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer;
  const DecryptPassword: RawUtf8; DecryptPasswordRounds: integer;
  Signature: PEccSignatureCertifiedContent; MetaData: PRawJson): TEccDecrypt;
var
  auth: TEccCertificateSecret;
  head: TEciesHeader;
  priv: TFileName;
begin
  auth := TEccCertificateSecret.Create;
  try
    result := ecdNoPrivateKey;
    if FileExists(AuthPrivKey) then
      priv := AuthPrivKey
    else
    begin
      if not EciesHeaderFile(FileToDecrypt, head) then
        exit;
      priv := Utf8ToString(EccText(head.recid));
      if not EccKeyFileFind(priv, true) then
        exit; // not found local .private from header
    end;
    if not auth.LoadFromSecureFile(priv, AuthPassword, AuthPasswordRounds) then
      exit;
    result := auth.DecryptFile(FileToDecrypt, DestFile, DecryptPassword,
      DecryptPasswordRounds, Signature, MetaData);
  finally
    auth.Free;
  end;
end;

function EccCommandChainCertificates(
  const CertFiles: array of RawUtf8): TFileName;
var
  n, i: PtrInt;
  files: TFileNameDynArray;
begin
  result := '';
  n := length(CertFiles);
  if n = 0 then
    exit;
  if (n = 1) and (CertFiles[0] = '*') then
  begin
    files := FindFilesDynArrayToFileNames(FindFiles(
      '.', '*' + ECCCERTIFICATEPUBLIC_FILEEXT));
    result := 'chain' + ECCCERTIFICATES_FILEEXT;
  end
  else
  begin
    SetLength(files, n);
    for i := 0 to n - 1 do
    begin
      files[i] := Utf8ToString(CertFiles[i]);
      if not EccKeyFileFind(files[i], false) then
        exit;
    end;
    result := format('chain%d' + ECCCERTIFICATES_FILEEXT, [UnixTimeUtc]);
  end;
  with TEccCertificateChain.CreateFromFiles(files) do
  try
    if ValidateItems <> nil then
    begin
      result := '';
      raise EECCException.Create('Some of the certificates are invalid');
    end;
    SaveToFile(result);
  finally
    Free;
  end;
end;

function EccCommandInfoPrivFile(const AuthPrivKey: TFileName;
  const AuthPassword: RawUtf8; AuthPasswordRounds: integer): RawUtf8;
var
  auth: TEccCertificateSecret;
begin
  auth := TEccCertificateSecret.CreateFromSecureFile(
    AuthPrivKey, AuthPassword, AuthPasswordRounds);
  try
    result := JsonReformat(VariantSaveJson(auth.ToVariant))
  finally
    auth.Free;
  end;
end;

function EccCommandCheatInit(const Issuer, CheatPassword: RawUtf8;
  CheatRounds: integer): TFileName;
var
  new: TEccCertificateSecret;
  priv: RawByteString;
begin
  if FileExists(CHEAT_FILEMASTER + ECCCERTIFICATEPUBLIC_FILEEXT) or
     FileExists(CHEAT_FILEMASTER + ECCCERTIFICATESECRET_FILEEXT) then
    raise EECCException.Create(CHEAT_FILEMASTER + ' file already exist');
  // generate pair
  new := TEccCertificateSecret.CreateNew(nil, Issuer);
  try
    // save private key as cheat.private password-protected binary file
    priv := new.SaveToSecureBinary(CheatPassword, 128, CheatRounds);
    FileFromString(priv, CHEAT_FILEMASTER + ECCCERTIFICATESECRET_FILEEXT);
    // save public key as mastercheat.public JSON file
    result := CHEAT_FILEMASTER + ECCCERTIFICATEPUBLIC_FILEEXT;
    JsonReformatToFile(VariantSaveJson(new.ToVariant), result);
  finally
    new.Free;
    FillZero(priv);
  end;
end;

function EccCommandCheat(const PrivateFile: TFileName;
  const CheatPassword: RawUtf8; CheatRounds: integer;
  out authpass: RawUtf8; out authround: integer): RawUtf8;
var
  bin, split, json: RawByteString;
  master: TEccCertificateSecret;
  doc: TDocVariantData;
  fn: TFileName;
  res: TEccDecrypt;
begin
  fn := ChangeFileExt(PrivateFile, CHEAT_FILEEXT);
  bin := StringFromFile(fn);
  if bin = '' then
    raise EECCException.CreateUtf8('Unknown file %', [fn]);
  master := TEccCertificateSecret.CreateFromSecureFile(
    CHEAT_FILEMASTER, CheatPassword, CheatRounds);
  try
    res := master.Decrypt(bin, split);
    if res <> ecdDecrypted then
      raise EECCException.CreateUtf8('% on %', [ToText(res)^, fn]);
    json := TAesPrng.AFUnsplit(split, CHEAT_SPLIT);
    if json = '' then
      raise EECCException.CreateUtf8('Incorrect file %', [fn]);
    if not doc.InitJson(json) then
      doc.InitObject(['pass', json,
                      'rounds', DEFAULT_ECCROUNDS], JSON_OPTIONS_FAST);
    authpass := doc.U['pass'];
    authround := doc.I['rounds'];
    result := doc.ToJson('', '', jsonHumanReadable);
  finally
    master.Free;
    FillZero(json);
  end;
end;

procedure AeadProcess(Encrypt: boolean; var Source: RawByteString;
  const DestFileName: TFileName;
  const Password, PasswordSalt: RawUtf8; PasswordRounds: integer);
var
  aeskey: THash256;
  dst: RawByteString;
begin
  try
    Pbkdf2HmacSha256(
      Password, PasswordSalt, PasswordRounds, aeskey, 'salt');
    try
      dst := TAesCfc.MacEncrypt(Source, aeskey, Encrypt);
      try
        if dst = '' then
          raise EECCException.CreateUtf8(
            'MacEncrypt failed for %', [DestFileName]);
        if not FileFromString(dst, DestFileName) then
          raise EECCException.CreateUtf8(
            'FileFromString failed for %', [DestFileName]);
      finally
        FillZero(dst);
      end;
    finally
      FillZero(aeskey);
    end;
  finally
    FillZero(Source);
  end;
end;

procedure AeadCommandCryptFile(const FileToCrypt, DestFile: TFileName;
  const Password, PasswordSalt: RawUtf8; PasswordRounds: integer);
var
  plain: RawByteString;
  dest: TFileName;
begin
  plain := StringFromFile(FileToCrypt);
  if plain = '' then
    raise EECCException.CreateUtf8('File not found: %', [FileToCrypt]);
  if DestFile = '' then
    dest := FileToCrypt + AEAD_FILEEXT
  else
    dest := DestFile;
  AeadProcess({encrypt=}true,
    plain, dest, Password, PasswordSalt, PasswordRounds);
end;

procedure AeadCommandDecryptFile(const FileToDecrypt, DestFile: TFileName;
  const Password, PasswordSalt: RawUtf8; PasswordRounds: integer);
var
  encrypted: RawByteString;
  dest: TFileName;
begin
  encrypted := StringFromFile(FileToDecrypt);
  if encrypted = '' then
    raise EECCException.CreateUtf8('File not found: %', [FileToDecrypt]);
  if DestFile = '' then
    dest := GetFileNameWithoutExt(FileToDecrypt)
  else
    dest := DestFile;
  AeadProcess({encrypt=}false,
    encrypted, dest, Password, PasswordSalt, PasswordRounds);
end;



{ **************** High-Level Command-Line Process }

function EccCommand(cmd: TEccCommand; const sw: ICommandLine): TEccCommandError;

  procedure WriteVerif(verif: TEccValidity; const filename: TFileName;
    const sw: ICommandLine);
  var
    res: string;
  begin
    res := SysUtils.LowerCase(
      GetCaptionFromEnum(TypeInfo(TEccValidity), ord(verif)));
    if verif in ECC_VALIDSIGN then
      sw.Text(' % file verified as %.', [filename, res], ccLightGreen)
    else
    begin
      sw.Text(' % file verification failure: % (%).',
        [filename, res, ord(verif)], ccLightRed);
      result := eccValidationError;
    end;
  end;

  procedure WritePassword(const privfile: TFileName;
    const pass: RawUtf8; rounds: integer);
  var
    a: TEcdheAuth;
    privkey: RawUtf8;
  begin
    if privfile = '' then
      exit;
    sw.Text('Corresponding TSynPersistentWithPassword.ComputePassword:', []);
    sw.Text(' encryption %',
      [TSynPersistentWithPassword.ComputePassword(pass)], ccLightBlue);
    privkey := StringToUtf8(copy(GetFileNameWithoutExt(privfile), 1, 8));
    for a := low(a) to high(a) do
      sw.Text(' % %', [ToText(a)^,
        TEcdheProtocol.FromKeyCompute(privkey, pass, rounds, '', a)],
        ccLightBlue);
    sw.Text('', []);
  end;

var
  issuer, authpass, savepass, constname, comment, json: RawUtf8;
  meta: RawJson;
  start: TDateTime;
  authrounds, days, saverounds, splitfiles: integer;
  msg: string;
  origfile, auth, newfile, jsonfile: TFileName;
  algo: TEciesAlgo;
  decrypt: TEccDecrypt;
  decryptsign: TEccSignatureCertifiedContent;
begin
  result := eccSuccess;
  if sw = nil then
    raise EECCException.Create('EccCommand(nil)');
  try
    try
      case cmd of
        ecNew:
          begin
            repeat
              auth := sw.AsString('Auth', '',
                'Enter the first chars of the .private file name of the signing authority.'#13#10 +
                'Will create a self-signed certificate if left void.');
            until (auth = '') or
                  EccKeyFileFind(auth, true) or
                  sw.NoPrompt;
            if auth <> '' then
            begin
              sw.Text('Will use: %'#13#10, [ExtractFileName(auth)]);
              authpass := sw.AsUtf8('AuthPass', '',
                'Enter the PassPhrase of this .private file.');
              authrounds := sw.AsInt('AuthRounds', DEFAULT_ECCROUNDS,
                'Enter the PassPhrase iteration rounds of this .private file.');
            end
            else
              authrounds := 0;
            issuer := sw.AsUtf8('Issuer', Executable.User,
              'Enter Issuer identifier text.'#13#10'Will be truncated to 15-20 ascii-7 chars.');
            start := sw.AsDate('Start', NowUtc,
              'Enter the YYYY-MM-DD start date of its validity.'#13#10 +
              '0 will create a never-expiring certificate.');
            if start <= 0 then
              days := 0
            else
              days := sw.AsInt('Days', 365, 'Enter the number of days of its validity.');
            repeat
              savepass := sw.AsUtf8('NewPass', TAesPrng.Main.RandomPassword(12),
                'Enter a private PassPhrase for the new key (at least 8 chars long).'#13#10 +
                'Save this in a safe place: if you forget it, the key will be useless!');
            until (length(savepass) >= 8) or
                  sw.NoPrompt;
            repeat
              saverounds := sw.AsInt('NewRounds', DEFAULT_ECCROUNDS,
                'Enter the PassPhrase iteration rounds for the new key (at least 1000).'#13#10 +
                'The higher, the safer, but will demand more computation time.');
            until (saverounds >= 1000) or
                  sw.NoPrompt;
            splitfiles := 1;
      {repeat
        splitfiles := sw.AsInt('SplitFiles',1,
          'Into how many files the private key should be parceled out.');
      until (splitfiles>0) or sw.NoPrompt;}
            newfile := EccCommandNew(auth, authpass, authrounds, issuer, start,
              days, savepass, saverounds, splitfiles);
            WritePassword(newfile, savepass, saverounds);
            if newfile <> '' then
            begin
              newfile := newfile + '/.private';
              if FileExists(ChangeFileExt(newfile, CHEAT_FILEEXT)) then
                newfile := newfile + ('/' + CHEAT_FILEEXT);
            end;
          end;
        ecRekey:
          begin
            repeat
              auth := sw.AsString('Auth', '',
                'Enter the first chars of the .private certificate file name.');
            until EccKeyFileFind(auth, true) or
                  sw.NoPrompt;
            sw.Text('Will use: %'#13#10, [ExtractFileName(auth)]);
            authpass := sw.AsUtf8('AuthPass', '',
              'Enter the PassPhrase of this .private file.');
            authrounds := sw.AsInt('AuthRounds', DEFAULT_ECCROUNDS,
              'Enter the PassPhrase iteration rounds of this .private file.');
            repeat
              savepass := sw.AsUtf8('NewPass', TAesPrng.Main.RandomPassword(12),
                'Enter a NEW private PassPhrase for the key (at least 8 chars long).'#13#10 +
                'Save this in a safe place: if you forget it, the key will be useless!');
            until (length(savepass) >= 8) or
                  sw.NoPrompt;
            repeat
              saverounds := sw.AsInt('NewRounds', DEFAULT_ECCROUNDS,
                'Enter the NEW PassPhrase iteration rounds for the key (at least 1000).'#13#10 +
                'The higher, the safer, but will demand more computation time.');
            until (saverounds >= 1000) or
                  sw.NoPrompt;
            newfile := EccCommandRekey(
              auth, authpass, authrounds, savepass, saverounds);
            WritePassword(newfile, savepass, saverounds);
            if FileExists(ChangeFileExt(newfile, CHEAT_FILEEXT)) then
              newfile := newfile + ('/' + CHEAT_FILEEXT);
          end;
        ecSign:
          begin
            repeat
              origfile := sw.AsString('File', '',
                'Enter the name of the file to be signed.');
            until FileExists(origfile) or
                  sw.NoPrompt;
            repeat
              auth := sw.AsString('Auth', '',
                'Enter the first chars of the .private file name of the signing authority.');
            until EccKeyFileFind(auth, true) or
                  sw.NoPrompt;
            sw.Text('Will use: %'#13#10, [ExtractFileName(auth)]);
            authpass := sw.AsUtf8('Pass', '',
              'Enter the PassPhrase of this .private file.');
            authrounds := sw.AsInt('Rounds', DEFAULT_ECCROUNDS,
              'Enter the PassPhrase iteration rounds of this .private file.');
            newfile := EccCommandSignFile(
              origfile, auth, authpass, authrounds, []);
          end;
        ecVerify:
          begin
            repeat
              origfile := sw.AsString('File', '',
                'Enter the name of the file to be verified.');
            until sw.NoPrompt or
                  (FileExists(origfile) and
                   FileExists(origfile + ECCCERTIFICATESIGN_FILEEXT));
            WriteVerif(EccCommandVerifyFile(
              origfile, sw.AsString('auth', '', ''), ''), origfile, sw);
          end;
        ecSource:
          begin
            repeat
              auth := sw.AsString('Auth', '',
                'Enter the first chars of the .private certificate file name.');
            until EccKeyFileFind(auth, true) or
                  sw.NoPrompt;
            sw.Text('Will use: %'#13#10, [ExtractFileName(auth)]);
            authpass := sw.AsUtf8('Pass', '',
              'Enter the PassPhrase of this .private file.');
            authrounds := sw.AsInt('Rounds', DEFAULT_ECCROUNDS,
              'Enter the PassPhrase iteration rounds of this .private file.');
            constname := sw.AsUtf8('Const', '',
              'Enter the variable name to define the const in source.');
            comment := sw.AsUtf8('Comment', '',
              'Enter some optional comment to identify this private key.');
            newfile := EccCommandSourceFile(auth, authpass, authrounds,
              constname, comment, TAesPrng.Main.RandomPassword(24));
          end;
        ecInfoPriv:
          begin
            repeat
              auth := sw.AsString('Auth', '',
                'Enter the first chars of the .private certificate file name.');
            until EccKeyFileFind(auth, true) or
                  sw.NoPrompt;
            if not sw.NoPrompt then
              sw.Text('Will use: %'#13#10, [ExtractFileName(auth)]);
            authpass := sw.AsUtf8('Pass', '',
              'Enter the PassPhrase of this .private file.');
            authrounds := sw.AsInt('Rounds', DEFAULT_ECCROUNDS,
              'Enter the PassPhrase iteration rounds of this .private file.');
            json := EccCommandInfoPrivFile(auth, authpass, authrounds);
            sw.Text('%', [json]);
            jsonfile := sw.AsString('Json', '', '');
            if jsonfile <> '' then
              FileFromString(json, jsonfile);
            if not sw.NoPrompt then
              WritePassword(auth, authpass, authrounds);
          end;
        ecChain:
          newfile := EccCommandChainCertificates(sw.AsArray);
        ecChainAll:
          newfile := EccCommandChainCertificates(['*']);
        ecCrypt:
          begin
            repeat
              origfile := sw.AsString('File', '',
                'Enter the name of the file to be encrypted.');
            until FileExists(origfile) or
                  sw.NoPrompt;
            repeat
              newfile := SysUtils.Trim(sw.AsString(
                'Out', origfile + ENCRYPTED_FILEEXT,
                'Enter the name of the encrypted file'));
            until (newfile <> '') or
            sw.NoPrompt;
            repeat
              auth := sw.AsString('Auth', '',
                'Enter the first chars of the .public file name of the encryption authority.');
            until EccKeyFileFind(auth, false) or
                  sw.NoPrompt;
            sw.Text('Will use: %'#13#10, [ExtractFileName(auth)]);
            authpass := sw.AsUtf8('SaltPass', 'salt',
              'Enter the optional PassPhrase to be used for encryption.');
            authrounds := sw.AsInt('SaltRounds', DEFAULT_ECCROUNDS,
              'Enter the PassPhrase iteration rounds.');
            algo := TEciesAlgo(sw.AsEnum('Algo', '0', TypeInfo(TEciesAlgo), ''));
            EccCommandCryptFile(
              origfile, newfile, auth, '', '', authpass, authrounds, algo);
          end;
        ecInfoCrypt:
          begin
            repeat
              origfile := sw.AsString('File', '',
                'Enter the name of the encrypted file.');
            until FileExists(origfile) or
                  sw.NoPrompt;
            newfile := sw.AsString('RawFile', '', '');
            json := JsonReformat(EciesHeaderText(origfile, newfile));
            sw.Text('%', [json]);
            jsonfile := sw.AsString('Json', '', '');
            if jsonfile <> '' then
              FileFromString(json, jsonfile);
          end;
        ecDecrypt:
          begin
            repeat
              origfile := sw.AsString('File', '',
                'Enter the name of the file to be decrypted.');
            until FileExists(origfile) or
                  sw.NoPrompt;
            repeat
              newfile := SysUtils.Trim(sw.AsString(
                'Out', GetFileNameWithoutExt(origfile) + '.2',
                'Enter the name of the decrypted file'));
            until (newfile <> '') or
                  sw.NoPrompt;
            authpass := sw.AsUtf8('AuthPass', '',
              'Enter the PassPhrase of the associated .private file.');
            authrounds := sw.AsInt('AuthRounds', DEFAULT_ECCROUNDS,
              'Enter the PassPhrase iteration rounds of this .private file.');
            savepass := sw.AsUtf8('SaltPass', 'salt',
              'Enter the optional PassPhrase to be used for decryption.');
            saverounds := sw.AsInt('SaltRounds', DEFAULT_ECCROUNDS,
              'Enter the PassPhrase iteration rounds.');
            decrypt := EccCommandDecryptFile(origfile, newfile,
              sw.AsString('Auth', '', ''), authpass, authrounds,
              savepass, saverounds, @decryptsign, @meta);
            msg := SysUtils.LowerCase(
              GetCaptionFromEnum(TypeInfo(TEccDecrypt), ord(decrypt)));
            if decrypt in ECC_VALIDDECRYPT then
            begin
              if decrypt = ecdDecryptedWithSignature then
                WriteVerif(EccCommandVerifyDecryptedFile(
                  newfile, decryptsign), newfile, sw);
              if meta <> '' then
                sw.Text(' % file meta = %', [origfile, meta], ccGreen);
              sw.Text(' % file %.', [origfile, msg], ccLightGreen);
            end
            else
            begin
              sw.Text(' % file decryption failure: % (%).',
                [origfile, msg, ord(decrypt)], ccLightRed);
              result := eccError;
            end;
          end;
        ecAeadCrypt:
          begin
            repeat
              origfile := sw.AsString('File', '',
                'Enter the name of the file to be encrypted.');
            until FileExists(origfile) or
                  sw.NoPrompt;
            repeat
              newfile := SysUtils.Trim(sw.AsString(
                'Out', origfile + AEAD_FILEEXT,
                'Enter the name of the encrypted file'));
            until (newfile <> '') or
                  sw.NoPrompt;
            authpass := sw.AsUtf8('Pass', '',
              'Enter the PassPhrase to be used for encryption.');
            savepass := sw.AsUtf8('Salt', 'salt',
              'Enter the optional PassPhrase to be used for encryption.');
            authrounds := sw.AsInt('Rounds', DEFAULT_AEADROUNDS,
              'Enter the PassPhrase iteration rounds.');
            AeadCommandCryptFile(origfile, newfile, authpass, savepass, authrounds);
          end;
        ecAeadDecrypt:
          begin
            repeat
              origfile := sw.AsString('File', '',
                'Enter the name of the file to be decrypted.');
            until FileExists(origfile) or
                  sw.NoPrompt;
            repeat
              newfile := SysUtils.Trim(sw.AsString(
                'Out', GetFileNameWithoutExt(origfile) + '.2',
                'Enter the name of the decrypted file'));
            until (newfile <> '') or
                  sw.NoPrompt;
            authpass := sw.AsUtf8('Pass', '',
              'Enter the PassPhrase to be used for decryption.');
            savepass := sw.AsUtf8('Salt', 'salt',
              'Enter the optional PassPhrase to be used for decryption.');
            authrounds := sw.AsInt('Rounds', DEFAULT_AEADROUNDS,
              'Enter the PassPhrase iteration rounds.');
            AeadCommandDecryptFile(
              origfile, newfile, authpass, savepass, authrounds);
            sw.Text(' % file decrypted.', [origfile], ccLightGreen);
          end;
        ecCheatInit:
          begin
            issuer := sw.AsUtf8('Issuer', Executable.User,
              'Enter Issuer identifier text of the master cheat keys.'#13#10 +
              'Will be truncated to 15-20 ascii-7 chars.');
            repeat
              savepass := sw.AsUtf8('NewPass', TAesPrng.Main.RandomPassword(12),
                'Enter a private PassPhrase for the master cheat.private key (at least 8 chars).'#13#10 +
                'Save this in a safe place: if you forget it, the key will be useless!');
            until (length(savepass) >= 8) or
                  sw.NoPrompt;
            repeat
              saverounds := sw.AsInt('NewRounds', CHEAT_ROUNDS,
                'Enter iteration rounds for the mastercheat.private key (at least 100000).');
            until (saverounds >= CHEAT_ROUNDS) or
                  sw.NoPrompt;
            newfile := EccCommandCheatInit(issuer, savepass, saverounds);
            if newfile <> '' then
              newfile := newfile + '/.private';
          end;
        ecCheat:
          begin
            repeat
              auth := sw.AsString('Auth', '',
                'Enter the first chars of the .private certificate file name.');
            until EccKeyFileFind(auth, true) or
                  sw.NoPrompt;
            if not sw.NoPrompt then
              sw.Text('Will use: %'#13#10, [ExtractFileName(auth)]);
            savepass := sw.AsUtf8('AuthPass', '',
              'Enter the PassPhrase of the master cheat.private file.');
            saverounds := sw.AsInt('AuthRounds', CHEAT_ROUNDS,
              'Enter the PassPhrase iteration rounds of the cheat.private file.');
            sw.Text('%', [EccCommandCheat(
              {in:} auth, savepass, saverounds, {out:} authpass, authrounds)]);
            if not sw.NoPrompt then
              WritePassword(auth, authpass, authrounds);
          end;
      else
        result := eccUnknownCommand;
      end;
    except
      on E: Exception do
      begin
        if not sw.NoPrompt then
          ConsoleShowFatalException(E, false);
        newfile := '';
        result := eccException;
      end;
    end;
  finally
    if not sw.NoPrompt then
    begin
      FillZero(authpass);
      FillZero(savepass);
    end;
  end;
  if (newfile <> '') and
     (result = eccSuccess) and
     not (cmd in [ecInfoCrypt]) then
    sw.Text(' % file created.', [newfile], ccWhite);
  sw.TextColor(ccLightGray);
end;


end.
