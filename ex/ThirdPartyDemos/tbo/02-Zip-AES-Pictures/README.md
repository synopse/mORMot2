# ZIP AES and Pictures

This is a sample extracted from a [Delphi Praxis Forum page](https://www.delphipraxis.net/210914-mormot-zip-datei-als-datenspeicher-mit-verschluesseltem-inhalt.html).

With the example code you get:
- A ZIP file as data storage, which can optionally store entries encrypted with AES.
- Connection of a progress indicator with the help of a mediator.
- Introducing a function that can AES encrypt and decrypt a file.
- Acceleration in saving and reading graphic formats JPEG, PNG, GIF, TIFF.
- And some little things more...

The interface is kept very simple and includes only a few functions. The source code is written in such a way that it invites you to try it out on your own.

## Source code

The Source code has been extracted from the original `.zip` attached with the article.

But a `TestImage.png` picture is expected to be available in the executable folder, for testing the PNG reading/writing.

# Translated Documentation

[Just copy and pasted from Google Translator, with a quick manual review and formatting](https://www-delphipraxis-net.translate.goog/210914-mormot-zip-datei-als-datenspeicher-mit-verschluesseltem-inhalt.html?_x_tr_sl=auto&_x_tr_tl=en&_x_tr_hl=de&_x_tr_pto=wapp)

## Presentation

The second mORMot presentation also has a question in the Synopse forum as a starting point. The sample application created can serve as a starting point for your own explorations. The aim is to present as much functionality as possible with just a few lines of code. This is about concepts, not about a finished copy-paste solution. mORMot2 is used for the example .

Attached is the source code and the executable program. Disclaimer: The source code is neither tested nor optimized. It should work with Delphi 10.2 or later. The use of the materials provided is at your own risk.

The created class `TImageResourceFile` manages images and uses a ZIP file as storage. The entries can optionally be stored with AES encryption. In the second part, the connection of a progress display with the help of a mediator is presented. At the end I show some practical functions and the results of a small write speed challenge between Delphi and mORMot ZIP, with an unexpected result for me.

The interface of the `TImageResourceFile` class is kept very simple and includes only a few functions:

```
TImageResourceFile = class (TObject)
private
  FPassword: RawUtf8;
  FFileName: TFileName;
  FIsWritable: Boolean;
protected
   function LoadStream(pmStream: TCustomMemoryStream; const pmcResName: TFileName; const pmcPassword: RawUtf8): Boolean;
   procedure SaveStream(pmStream: TCustomMemoryStream; const pmcResName: TFileName; const pmcPassword: RawUtf8; pmIsCompressed: Boolean = True);
public
   constructor Create( const pmcFileName: TFileName; const pmcPassword: RawUtf8);
   destructor Destroy; override ;
   function LoadImage(pmImage: TImage; const pmcImageName: String ): Boolean;
   procedure SaveImage(pmImage: TImage; const pmcImageName: String ); overload ;
   procedureSaveImage(pmImageData: TCustomMemoryStream; const pmcImageName: String ); overload ;
end ;
```

With the example source code you get:
- A ZIP file as data storage, which can optionally store entries encrypted with AES.
- Connection of a progress display with the help of a mediator.
- Introducing a function that can AES encrypt and decrypt a file.
- Accelerated saving and reading of the graphic formats JPEG, PNG, GIF, TIFF.

## Acceleration when saving and reading images

To deal with the last point straight away, here are a few benchmark values from the application determined with a 2MB PNG image:

```
SaveImage() Vcl.Imaging.pngimage  900ms   mormot.ui.gdiplus	25ms
LoadImage() Vcl.Imaging.pngimage 70 ms	mormot.ui.gdiplus 10ms
```

The numbers speak for themselves. If you have a speed problem, try the following: remove the `Vcl.Imaging.pngimage` unit, then include the `mormot.ui.gdiplus` unit and call the `RegisterSynPictures` function.

## ZIP file implementation 

By including the unit `mormot.core.zip` you get support for ZIP and GZ. Reading and writing a ZIP file are always treated separately. There is a `TZipRead` class for reading and a `TZipWrite` class for writing


The source can be a file or a stream. `TZipRead` can also read directly from a resource embedded in the executable. In addition, the unit includes some useful functions. Examples are:
- `ZipTest`: This function unpacks the contents of a ZIP file and checks the respective CRC.
- `CompressZLib`: Encrypts or decrypts the content using the ZIP algorithm.
- `CompressGZip`: Encrypts or decrypts the content using the GZ algorithm.
- `FileAppend`: Adds a ZIP file to an executable.

## Progress Bar with Mediator

*Mediators* are an elegant way to encapsulate functionality and achieve easy and safe use. Even if the following example is under-complex, the profit is recognizable. The function could also hide a semi-modal dialog with a progress bar. And who ever knows which fields exactly `PProgressInfo` contains. With the simple connection to a mediator, this takes care of itself. I just choose, the rest is done reliably. Developed once and used again and again.

Note: The term *mediator* is used here more generally than the context of the design pattern of the same name would suggest.

```
type
  TZipProgressBarGuiHelper = class (TCustomZipProgressGuiHelper)
   private
    FProgressBar: TProgressBar;
   protected
     procedure DoOnInfoProgress(pmSender: TObject; pmInfo: PProgressInfo); override ;
   public
     constructor Create(pmProgressBar: TProgressBar); introduce ;
     procedure Prepare(pmZip: TZipAbstract); override ;
   end ;
  
procedureTZipProgressBarGuiHelper.DoOnInfoProgress(pmSender: TObject; pmInfo: PProgressInfo);
begin
   if pmInfo.CurrentSize = 0 then
   begin
    FProgressBar.Min := 0;
    FProgressBar.Max := 100;
    FProgressBar.Position := 0
   end
   else if pmInfo.CurrentSize >= pmInfo.ExpectedSize then
    FProgressBar.Position := 0
   else
    FProgressBar.Position := pmInfo.Percent;
end ;

procedure TZipProgressBarGuiHelper.Prepare(pmZip: TZipAbstract);
begin
   if pmZip = Nil then Exit; //=>

  pmZip.ReportDelay := 50;
  pmZip.OnProgress := DoOnInfoProgress;
end;
```

Implementing the connection is a simple call to the Prepare() function:

```
var zipWrite: TZipWrite := TZipWrite.Create(MakePath([Executable.ProgramFilePath, ' TestFile.zip ']));
try
  FProgressHelper.Prepare(zipWrite);
  zipWrite.AddDeflated(fileName);
finally
  zipWrite.Free;
end ;
```

## Useful Cryptographic Functions

The `src\crypt` folder of the mORMot repository houses the cryptographic classes and functions of the framework. In total, it includes about 28K lines of source code. The pivotal point is the `mormot.crypt.core` unit.

A short excerpt from the description gives an insight into the content:
- AES Encoding/Decoding with optimized asm and AES-NI/CLMUL support
- AES-256 Cryptographic Pseudorandom Number Generator (CSPRNG)
- SHA-2 SHA-3 Secure Hashing
- HMAC Authentication over SHA and CRC32C
- PBKDF2 Key Derivation over SHA2 and SHA3

One of those many features is `AesPkcs7File`. It can be used to securely encrypt or decrypt a file. Encrypt once and back looks like this:

```
AesPkcs7File(' FileName.txt ', ' FileName.dat ', True, ' TopSecretPassword ');
AesPkcs7File(' FileName.dat ', ' FileName2.txt ', False, ' TopSecretPassword ');
if HashFileMd5(' FileName.txt ') = HashFileMd5(' FileName2.txt ') then
  ShowMessage(' Everything is fine! ');
```

The `CryptDataForCurrentUser()` function can protect data using AES-256-CFB and a secret known only to the current user. It uses Windows DPAPI to create it and stores the file in the user's local `AppData` folder. In addition, an application-specific `AppSecret` value can be specified in the application.

```
var
  plainText, secretText: RawByteString;
begin
  plainText := ' This is a secret text that only I should know. ';
  secretText := CryptDataForCurrentUser(plainText, ' AppSecret ', True);

  plainText := CryptDataForCurrentUser(secretText, ' AppSecret ', False);
   try
    ShowMessage(Utf8ToString(plainText));
   finally
    FillZero(plainText);
   end ;
end ;
```

Here is how to create a license handling with the combination of the classes `TAesPkcs7Reader TAesPkcs7Writer` and the functions `RecordLoadJson RecordSaveJson`:

```
type
  TLicenseData = record
    CustomerNum: Integer;
    CustomerName: RawUtf8;
    CustomerAddress: RawUtf8;
    LicenseDate: TDate;
    ProductName: RawUtf8;
    ProductVersion: record
      Major: Integer;
      minor: integer;
     end ;
   end ;

var
  licData: TLicenseData;
begin
  licData.CustomerNum := 1;
  licData.CustomerName := ' Thomas';
  licData.LicenceDate := Date;
  licData.ProductName := ' Delphi ';
  licData.ProductVersion.Major := 11;
  licData.ProductVersion.Minor := 1;

   var tmpStream: TMemoryStream := TMemoryStream.Create;
   try
     var licStream: TRawByteStringStream := TRawByteStringStream.Create(RecordSaveJson(licData, TypeInfo(TLicenseData)));
     try
       var aesWriter: TAesPkcs7Writer := TAesPkcs7Writer.Create(tmpStream, ' TopSecretPassword ');
       try
        StreamCopyUntilEnd(licStream, aesWriter);
        aesWriter.Finish;
       finally
        aesWriter.Free;
       end ;
     finally
      licStream.Free;
     end ;

    tmpStream.SaveToFile(' LicenseData.lic ');
   finally
    tmpStream.Free;
   end ;
```

The JSON deserialization is error tolerant ! This means you can add new fields to the `TLicenseData` record and the `RecordLoadJson()` function also reliably loads previous versions. This means you no longer have to maintain streaming versions.

## Write Speed Challenge

A few benchmark values was determined with the help of the example application.

mORMot's ZIP implementation is only marginally faster than Delphi's. Embarcadero did his homework there. mORMot supports Delphi 7 to 11.1, plus the FP compiler, but the same applies to Delphi: Better late than never.

*Arnaud's note: on FPC, mORMot uses `libdeflate` which is much faster than the regular `zlib`. So on FPC/Linux we could have much better performance when working with `.zip` than with Delphi.*

# Summary

mORMot is well documented. The help includes more than 2500 pages. The first 650 pages contain a very readable general part, the rest is API documentation. mORMot does not have to be installed in the IDE ! It is sufficient to paste the appropriate library paths. There are many examples and a friendly forumto disposal. If there is more interest in mORMot, I can also briefly introduce other parts in a similar way.

See you soon...
Thomas
