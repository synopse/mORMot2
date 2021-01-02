/// Command Line Public Key Cryptography Tool
// - this program is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
program ecc;

{
  *****************************************************************************

  Manages certificate-based public-key cryptography using ECC-secp256r1
  - Public/private key pairs generation using new/rekey/source/infopriv
  - Safe key chaining using chain/chainall
  - ECDSA digital signature using sign/verify
  - ECIES encryption using crypt/decrypt/infocrypt
  - Symetric encryption via aeadcrypt/aeaddecrypt
  - Centralized passwords management via cheatinit/cheat

  *****************************************************************************
}

{$I ..\..\mormot.defines.inc}

{$ifdef MSWINDOWS}
  {$apptype console}
  {$R ..\..\mormot.win.default.manifest.res}
{$endif MSWINDOWS}

uses
  {$I ..\..\mormot.uses.inc}
  classes,
  sysutils,
  mormot.core.base         in '..\..\core\mormot.core.base.pas',
  mormot.core.os           in '..\..\core\mormot.core.os.pas',
  mormot.core.unicode      in '..\..\core\mormot.core.unicode.pas',
  mormot.core.text         in '..\..\core\mormot.core.text.pas',
  mormot.core.rtti         in '..\..\core\mormot.core.rtti.pas',
  mormot.core.datetime     in '..\..\core\mormot.core.datetime.pas',
  mormot.core.perf         in '..\..\core\mormot.core.perf.pas',
  mormot.core.buffers      in '..\..\core\mormot.core.buffers.pas',
  mormot.core.data         in '..\..\core\mormot.core.data.pas',
  mormot.core.variants     in '..\..\core\mormot.core.variants.pas',
  mormot.core.json         in '..\..\core\mormot.core.json.pas',
  mormot.core.log          in '..\..\core\mormot.core.log.pas',
  mormot.core.test         in '..\..\core\mormot.core.test.pas',
  mormot.core.crypto       in '..\..\core\mormot.core.crypto.pas',
  mormot.core.secure       in '..\..\core\mormot.core.secure.pas',
  mormot.core.ecc256r1     in '..\..\core\mormot.core.ecc256r1.pas',
  mormot.core.ecc          in '..\..\core\mormot.core.ecc.pas',
  mormot.app.console       in '..\..\app\mormot.app.console.pas',
  mormot.tools.ecc;


{$R *.res}

function ProcessCommandLine: TEccCommandError;
var
  cmd: RawUtf8;
  main: TEccCommand;
  sw: ICommandLine;
begin
  cmd := StringToUtf8(ParamStr(1));
  main := TEccCommand(
    GetEnumNameValueTrimmed(TypeInfo(TEccCommand), pointer(cmd), length(cmd)));
  if main = ecChain then
    sw := TCommandLine.CreateAsArray({firstparam=}2)
  else
    sw := TCommandLine.Create;
  result := EccCommand(main, sw);
  if result = eccUnknownCommand then
  begin
    TextColor(ccLightGreen);
    writeln(#13#10'Synopse ECC certificate-based public-key cryptography' + 
            #13#10'-----------------------------------------------------');
    TextColor(ccGreen);
    writeln('Using mormot.core.ecc '  + SYNOPSE_FRAMEWORK_VERSION +  #13#10);
    TextColor(ccLightGray);
    writeln(ExeVersion.ProgramName,
      ' help');
    writeln(ExeVersion.ProgramName,
      ' new -auth key.private -authpass authP@ssW0rd -authrounds 60000'#13#10 + 
      '      -issuer toto@toto.com -start 2016-10-30 -days 30'#13#10 + 
      '      -newpass P@ssw0RD@ -newrounds 60000'); // -splitfiles 1');
    writeln(ExeVersion.ProgramName,
      ' rekey -auth key.private -authpass P@ssW0rd -authrounds 60000'#13#10 + 
      '      -newpass newP@ssw0RD@ -newrounds 60000');
    writeln(ExeVersion.ProgramName,
      ' sign -file some.doc -auth key.private -pass P@ssW0rd -rounds 60000');
    writeln(ExeVersion.ProgramName,
      ' verify -file some.doc -auth key.public');
    writeln(ExeVersion.ProgramName,
      ' source -auth key.private -pass P@ssW0rd -rounds 60000'#13#10 + 
      '      -const MY_PRIVKEY -comment "My Private Key"');
    writeln(ExeVersion.ProgramName,
      ' infopriv -auth key.private -pass P@ssW0rd -rounds 60000 [-json key.json]');
    writeln(ExeVersion.ProgramName,
      ' chain file1.public file2.public file3.public ...');
    writeln(ExeVersion.ProgramName,
      ' chainall');
    writeln(ExeVersion.ProgramName,
      ' crypt -file some.doc -out some.doc.synecc -auth key.public'#13#10 + 
      '      -saltpass salt -saltrounds 60000 [-algo 0]');
    writeln(ExeVersion.ProgramName,
      ' decrypt -file some.doc.synecc -out some.doc -auth key.private'#13#10 + 
      '      -authpass P@ssW0rd -authrounds 60000 -saltpass salt -saltrounds 60000');
    writeln(ExeVersion.ProgramName,
      ' infocrypt -file some.doc.synecc [-rawfile some.raw][-json some.json]');
    writeln(ExeVersion.ProgramName,
      ' aeadcrypt -file some.doc -out some.doc.synaead -pass P@ssW0rd -salt salt'#13#10 + 
      '       -rounds 60000');
    writeln(ExeVersion.ProgramName,
      ' aeaddecrypt -file some.doc -out some.doc.synaead -pass P@ssW0rd -salt salt'#13#10 + 
      '       -rounds 60000');
    writeln(ExeVersion.ProgramName,
      ' cheatinit -newpass MasterP@ssw0RD@ -newrounds 100000');
    writeln(ExeVersion.ProgramName,
      ' cheat -auth key.private -authpass MasterP@ssw0RD@ -authrounds 100000');
    writeln(#10'Note that you can add the -noprompt switch for no console interactivity.');
  end;
end;

begin
  ExitCode := ord(ProcessCommandLine);
  
end.

