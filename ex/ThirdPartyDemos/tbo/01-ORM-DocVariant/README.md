# ORM and DocVariant

This is a sample extracted from a [Delphi Praxis Forum page](https://www.delphipraxis.net/210843-mormot-orm-und-docvariant-kurz-vorgestellt.html).

The example is the answer to a forum member's question and the implementation of one of two solution variants, all with mORMot. I wrote this example because I noticed that answering a question with the reference to mORMot and the necessary functions has no effect. I think without concrete introduction with full source code to play with, mentioning mORMot makes no sense.

The example shows a very simplified image database with the following technical details:
- An embedded SQLite database that can be optionally AES encrypted.
- Complete tasks such as storing and reading records with the built-in ORM.
- A full text search using the Title and Comment fields.
- A field for meta data that can contain different fields for each record and can be queried via SQL.
- Acceleration in saving and reading graphic formats JPEG, PNG, GIF, TIFF.

The interface is kept very simple and includes only a few functions. The source code is written in such a way that it invites you to try it out on your own.

## Source code

The Source code has been extracted from the original `.zip` attached with the article.

But a `TestImage.png` picture is expected to be available in the executable folder, for testing the PNG reading/writing.

# Translated Documentation

[Just copy and pasted from Google Translator, with a quick manual review and formatting](https://www-delphipraxis-net.translate.goog/210843-mormot-orm-und-docvariant-kurz-vorgestellt.html?_x_tr_sl=auto&_x_tr_tl=en&_x_tr_hl=en&_x_tr_pto=wapp)

## Presentation

Inspired by this question in the Synopse forum, I would like to take the opportunity to demonstrate some mORMot functions with an example. The aim is to present as much functionality as possible with just a few lines of code. This is about concepts, not about a finished copy-paste solution. The contained references to the documentation link to the currently available mORMot1 help.

mORMot2 is used for the example. Class and function names may differ slightly.

Attached is the source code and the executable program. Disclaimer: The source code is neither tested nor optimized. It should work with Delphi 10.2 or later. The use of the materials provided is at your own risk.

The created class `TImageResourceDB` manages images and uses an SQLite database as a data grab. The interface is kept very simple and includes only a few functions:

```
TImageResourceDB = class (TObject)
private
  FRestServer: TRestServerDB;
   function CreateModel: TOrmModel;
protected
   function LoadData(pmStream: TStream; const pmcRowID: TID): Boolean;
   function SaveData( const pmcImageData: RawBlob; const pmcTitle, pmcComment: String ; const pmcMetaData: Variant; var pmvRowID: TID): Boolean;
public
   constructor Create( const pmcFileName: TFileName; const pmcPassword: RawUtf8 = ' ');
   destructor Destroy; override ;
   class function InitDefaultMetaData( const pmcCreator, pmcLocation: RawUtf8; pmLatitude, pmLongitude: Double; pmDate: TDate; pmTime: TTime): Variant;
   function LoadImage(pmImage: TImage; const pmcRowID: TID): Boolean; overload ;
   function LoadImage(pmImage: TImage; const pmcSearchPhrase: String ; pmResultIDs: PIDDynArray = Nil): Boolean; overload ;
   function LoadImage(pmImage: TImage; const pmcMetaFieldName: String ; const pmcMetaFieldValue: Variant; pmResultIDs: PIDDynArray = Nil ): Boolean; overload ;
   function SaveImage(pmImage: TImage; const pmcTitle, pmcComment: String ; const pmcMetaData: Variant; pmRowID: PID = Nil ): Boolean; overload ;
   function SaveImage(pmStream: TStream; const pmcTitle, pmcComment: String ; const pmcMetaData: Variant; pmRowID: PID = Nil ): Boolean; overload ;
end;
```
Nothing special, one might think. With the few lines you get:
- An embedded SQLite database that can optionally be AES encrypted.
- Use the built-in ORM to perform tasks such as storing and reading records.
- A full-text search using the Title and Comment fields.
- A field for meta data that can contain different fields for each record and can be queried via SQL .
- Accelerated saving and reading of the graphic formats JPEG, PNG, GIF, TIFF.

## Acceleration when saving and reading images

To deal with the last point straight away, here are a few benchmark values, determined from the application with a 2MB PNG image:

```
SaveImage() Vcl.Imaging.pngimage  900ms   mormot.ui.gdiplus	25ms
LoadImage() Vcl.Imaging.pngimage 70 ms	mormot.ui.gdiplus 10ms
```

The numbers speak for themselves. If you have a speed problem, try the following: remove the `Vcl.Imaging.pngimage` unit, then include the `mormot.ui.gdiplus` unit and call the `RegisterSynPictures` function.

## Embedded SQLite database

In order to statically integrate an SQLite database into the program, only the `unit mormot.db.raw.sqlite3.static` is to be added. After that, access to the SQLite3 engine can be low-level, or better, via a TRest### class. Other databases can also be connected via connection classes. There are connections for the frameworks ZEOS, FireDac/AnyDac, UniDac, ODBC , OleDB API and direct connections for SQLite, PostgreSQL, Oracle OCI and MongoDB. The connection is made via the fastest possible variant. For example, ZEOS uses ZDBC directly instead of going through the Delphi DB classes. This results in a significant acceleration.

## Initialize the ORM

All classes for the ORM must descendant(s) of class `TOrm`. The table name in the database results from the class name. All `published` properties of an ORM class are represented as a field in the database. In addition, the ID/RowID field is created. The possible field types are described in the help. The ORM classes are combined in a model. The model is passed to a `TRest` class for access. An overview of the existing `TRest` classes and their purpose is listed in the help.

The actual source code is not much longer than the description:

```
type
  TOrmFile = class (TOrm)
  ...
   published
     property Title: RawUTF8
       read FTitle write FTitle;
     property Comment: RawUtf8
       read FComment write FComment;
     property MetaData: Variant
       read FMetaData write FMetaData;
    ...
   end ;

...
FRestServer := TRestServerDB.Create(TOrmModel.Create([TOrmFile, ...]), DBFileName, False, DBPassword);
FRestServer.Model.Owner := FRestServer;
FRestServer.DB.Synchronous := smFull;
FRestServer.DB.LockingMode := lmExclusive;
FRestServer.Server.CreateMissingTables(0, [itoNoAutoCreateGroups, itoNoAutoCreateUsers]);
```

Note: `smFull` is by far the slowest mode but ensures 100% ACID behavior. In practice, `smNormal` is a good compromise between security and speed. In the example, there is no reason for authentication, so the automatic creation of the tables required for this is suppressed.

## Completing tasks via the ORM

An overview of all ORM functions can be obtained by looking at the `IRestOrm` interface. There are a variety of options to choose from. The application is very simple. Adding a record is done as follows:

```
var
  ormFile: TOrmFile;
begin
  ormFile := TOrmFile.Create;
   try
    ormFile.Title := ' my first one ';
    ormFile.Comment := ' Arnaud is the best ';
    ...
    FRestServer.Server.Add(ormFile, True);
   finally
    ormFile.Free;
   end ;
```

## SQLite full-text search

In order to activate the full-text search, you create your own ORM class, which descends from a specialized base class (`TOrmFts5/TOrmFts5Porter/TOrmFts5Unicode61`) that is available in mORMot. The fields of the data class intended for searching are repeated in this class. The search is a simple SQL query:

```
var
  sqlWhere: RawUtf8;
  searchIDs: TIDDynArray;
begin
  sqlWhere := FormatUtf8(' % MATCH ? ORDER BY rank DESC ', [' SearchTable '], [SearchPhrase]);
   if FRestServer.Server.FTSMatch(TOrmFileSearch, sqlWhere, searchIDs) then
```
Note: The `FormatUtf8()` function is similar to the Delphi `Format()` function. An additional feature is that there are two sets of arguments. First inserted arguments are marked with `%` (and not `%s %d`...), and `?` is also used as place holder for SQL values, which will be enclosed with `:():` markers. This marker is detected by the ORM storage engines, and the values will be replaced with bound SQL parameters at runtime - for best performance, reuse of the prepared statements, and also good security - since such bound parameters are not subject to SQL injection.

## Field with meta data

A property field of type variant is defined, to store a `TDocVariant` in ORM as JSON. A `TDocVariant` is an arbitrarily complex data structure made up of object(s) and/or arrays, or a combination of both. WOW. The `TDocVariant` syntax takes some getting used to for Pascal developers because it is more reminiscent of a scripting language. Read more about this in the help . A DocVariant can be created in several ways. One possibility is the following:

```
var
  metaData: variant;
begin
  TDocVariant.New(metaData);
  metaData.Number := 10;
  metaData.Creator := ' Thomas ';
  metaData.Birthday := EncodeDate(' Top Secret! ');
```
  
In SQLite from version 3.38.0 this can be queried with the following SQL extended syntax:
```
Schema: SELECT * FROM File WHERE MetaData->>'$.Creator'='Thomas' ORDER BY ...
```

# Summary

mORMot is well documented. The help includes more than 2500 pages. The first 650 pages contain a very readable general part, the rest is API documentation. mORMot does not have to be installed in the IDE ! It is sufficient to paste the appropriate library paths. mORMot2 is recommended for new applications. Here is the link to the GitHub repro . Many examples and a friendly forum are available. If there is more interest in mORMot, I can also briefly introduce other parts in a similar way.

See you soon...

Thomas 