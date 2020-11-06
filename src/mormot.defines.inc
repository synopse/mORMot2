{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Define a Centralized Set of Conditional Defines, included in all our
  framework units, and could be used also for your own private units.
}


(********************** User-Trigerred Conditionals **********************)

{  Those conditionals below can be enabled in your project Options,
   to tune the compilation depending your setup or your project expectations. }

{.$define USEPACKAGES}
// define this if you compile the unit within a Delphi package
// - it will avoid error like "[DCC Error] E2201 Need imported data reference ($G)
// to access 'VarCopyProc'
// - shall be set at the package options level, and left untouched by default
// - note: you should probably also set "Generate DCUs only" in Project Options
// -> Delphi Compiler -> Output C/C++ -> C/C++ output file generation

{.$define PUREMORMOT2}
// if defined, no mORMot 1.18 compatilibity types nor functions would be enabled
// - by default, existing projects should compile with mORMot 2
// - you should eventually define this conditional to make a perfect code
// conversion to the new types and methods definitions

{.$define NEWRTTINOTUSED}
// if defined, the new RTTI (available since Delphi 2010) won't be linked to
// the executable: resulting file size will be much smaller, and mORMot won't
// be affected (unless you use the enhanced RTTI for record/dynamic array JSON
// serialization) - left undefined by default to ensure minimal impact

{.$define NOSETTHREADNAME}
// if defined, SetThreadName() would not raise the exception used to set the
// thread name: to be defined if you have issues when debugging your application

{.$define NOEXCEPTIONINTERCEPT}
// if defined, exceptions shall not be intercepted and logged

{.$define NOPATCHRTL}
// if defined, FPC RTL won't be patched on x86_64 with optimized asm
// - you can enable it if you find out some compatibility problem

{.$define USELOCKERDEBUG}
// by default, some IAutoLocker instances would use TAutoLocker, unless this
// conditional is defined to use more verbose TAutoLockerDebug
// (may be used for race condition debugging, in multi-threaded apps)

{.$define DISABLE_SSE42}
// if defined, no SSE4.2 nor AES-NI instruction will be used, i.e. disable
// all crc32c and aesenc/aesdec opcodes - is set for FPC DARWIN target

{.$define WITH_ERMS}
// you may define this to enable REP MOVSB/STOSB for Fillchar/Move if cfERMS
// flag is available in Intel's CpuFeatures
// -> disabled by default, since in practice it is (much) slower for small blocks

{.$define NOXPOWEREDNAME}
// define this to avoid sending "X-Powered-By: Synopse mORMot" HTTP header

{.$define SQLVIRTUALLOGS}
// enable low-level logging of SQlite3 virtual table query planner costs
// -> to be defined only for internal debugging

{.$define VOIDSQLZEOS}
// make mormot.db.sql.zeos.pas a "void" unit - defined for FPC/Lazarus packages only

{.$define DISABLEAPPSQL}
// mormot.db.sql external SQL DB won't be linked to the executable by mormot.app
{.$define DISABLEAPPNOSQL}
// Mongo DB client won't be linked to the executable by mormot.app


{$ifdef FPC}

(********************** FPC Conditionals **********************)

  {$WARN 5093 off : function result variable of a managed type does not seem to be initialized }

  {$ifndef FPC_DELPHI}
    {$MODE DELPHI} // e.g. for asm syntax - disabled for FPC 2.6 compatibility
  {$endif}

  {$INLINE ON}
  {$MINENUMSIZE 1}
  {$PACKRECORDS DEFAULT} // force normal alignment
  {$PACKSET 1}
  {$PACKENUM 1}
  {$CODEPAGE UTF8} // otherwise unexpected behavior occurs in most cases

  {$define HASINLINE}
  {$define NODELPHIASM} // avoid e.g. low-level System.@LStrFromPCharLen calls
  {$define HASTTHREADSTART}
  {$define HASINTERFACEASTOBJECT}
  {$define EXTENDEDTOSHORT_USESTR} // FloatToText uses str() in FPC
  {$define DOUBLETOSHORT_USEGRISU} // fast DoubleToAscii()
  {$define FPC_OR_UNICODE}
  {$define USERECORDWITHMETHODS}  // use "object" only for Delphi 7
  {$define FPC_ENUMHASINNER}

  // $if FPC_FULLVERSION>20700 breaks Delphi 6-7 and SynProject :(
  {$ifdef VER2_7}
    {$define ISFPC27}
  {$endif}
  {$ifdef VER3_0}
    {$define ISFPC27}
    {$define ISFPC30}
    {$define HASDIRECTTYPEINFO}
    // PTypeInfo would be stored with no pointer de-reference
    // => Delphi and newer FPC uses a pointer reference to ease exe linking
  {$endif}
  {$ifdef VER3_1} // trunk before 3.2
    {$define ISFPC27}
    {$define ISFPC30}
    {.$define HASDIRECTTYPEINFO}
    // define this for trunk revisions older than June 2016 - see
    // http://wiki.freepascal.org/User_Changes_Trunk#RTTI_Binary_format_change
  {$endif}
  {$ifdef VER3_1_1}
    {$define ISFPC32}
  {$endif}
  {$ifdef VER3_2}
    {$define ISFPC27}
    {$define ISFPC30}
    {$define ISFPC32}
  {$endif}
  {$ifdef VER3_3} // trunk before 3.4
    {$define ISFPC27}
    {$define ISFPC30}
    {$define ISFPC32}
  {$endif}
  {$ifdef VER3_4}
    {$define ISFPC27}
    {$define ISFPC30}
    {$define ISFPC32}
    {$define ISFPC34}
    {$define FPC_PROVIDE_ATTR_TABLE} // introducing TTypeData.AttributeTable
  {$endif}
  {$if not defined(VER3_0) and not defined(VER3_2) and not defined(VER2)}
    {.$define FPC_PROVIDE_ATTR_TABLE} // to be defined since SVN 42356-42411
    // on compilation error in mormot.core.Rtti, undefine the above conditional
    // see https://lists.freepascal.org/pipermail/fpc-announce/2019-July/000612.html
  {$ifend}

  {$ifdef ANDROID}
    {$define LINUX} // a Linux-based system
  {$endif}

  {$ifdef BSD}
    // LINUX conditional includes Darwin and BSD family like FreeBSD
    {$define LINUX} // not true, but a POSIX/BSD system - see LINUXNOTBSD
    {$ifdef DARWIN}
      {$define FPCSQLITE3STATIC} // we supply Darwin static libs
      {$ifdef CPUINTEL}
        {$define FPC_PIC} // may have not be defined by the compiler options
      {$endif}
    {$else}
      {$define BSDNOTDARWIN}   // OSX has some non-standard API calls
    {$endif}
  {$else}
    {$ifdef LINUX}
      {$define LINUXNOTBSD} // e.g. to disable epoll API
      {$define FPCLINUXNOTBSD}
    {$endif LINUX}
  {$endif BSD}

  {$ifdef LINUX}
     {$define FPCLINUX}
  {$endif LINUX}

  {$ifdef CPU64}
    {$define FPC_64}
    {$ifdef CPUX64}
      {$ASMMODE INTEL}       // as Delphi expects
      {$define CPUINTEL}
      {$define FPC_CPUINTEL}
      {$define FPC_CPUX64}
      {$ifndef DARWIN}
        {$define ASMX64}     // supports asm with globals
        {$define ASMINTEL}   // either ASMX86 or ASMX64
        {$define ASMX64AVX}  // supports AVX/AVX2/AVX512
        {$define CPUX64ASM}  // FPC has no problem (not Delphi prior XE7)
        {$define HASAESNI}   // mormot.core.crypto rejected by Darwin asm
        {$ifdef LINUX}
          {$define ASMX64LINUX}  // e.g. for tuned server-side asm
        {$endif LINUX}
      {$endif DARWIN}
    {$endif CPUX64}
    {$ifdef CPUAARCH64}
      {$define CPUARM3264}
    {$endif CPUAARCH64}
  {$else}
    {$define FPC_32}
    {$ifdef CPUX86}
      {$ASMMODE INTEL}       // as Delphi expects
      {$define CPUINTEL}
      {$define FPC_CPUINTEL}
      {$define FPC_X86}
      {$ifndef DARWIN}
        {$define ASMX86}     // supports asm with globals
        {$define ASMINTEL}   // either ASMX86 or ASMX64
        {$define CPUX86NOTPIC} // use "absolute" instead of local register
      {$endif DARWIN}
      {$define HASAESNI}
      {$define TSYNEXTENDED80} // only 32-bit has a true x87 extended type
    {$endif CPUX86}
    {$ifdef CPUARM}
      {$define CPUARM3264}
    {$endif CPUARM}
  {$endif CPU64}

  {$ifdef ISFPC30}
    {$ifndef MSWINDOWS}
      // define FPCUSEVERSIONINFO to link low-level executable file information
      // retrieval for ExeVersion from mormot.core.os.pas
      // => disabled by default, to reduce the executable overhead
      {.$define FPCUSEVERSIONINFO}
    {$endif MSWINDOWS}
  {$endif ISFPC30}

  {$ifdef ISFPC32}
    // FPC has its own RTTI layout only since late 3.x
    {$define FPC_NEWRTTI}
    // when http://bugs.freepascal.org/view.php?id=26774 has been fixed
    {$define HASINTERFACERTTI}
  {$endif}

  {$ifdef FPC_NEWRTTI}
    {$define ISDELPHI2010_OR_FPC_NEWRTTI}
  {$else}
    {$define DELPHI_OR_FPC_OLDRTTI}
    {$define FPC_OLDRTTI}
  {$endif}
  {$define ISDELPHI2010_OR_FPC} // eltype2 field

  {$ifdef FPC_HAS_CPSTRING}
    // see http://wiki.freepascal.org/FPC_Unicode_support
    {$define HASCODEPAGE} // UNICODE means {$mode delphiunicode}
  {$endif}
  {$ifdef ISFPC27}
    {$define ISFPC271}
    {$define HASVARUSTRING}
    {$define HASVARUSTRARG}
    // defined if the http://mantis.freepascal.org/view.php?id=26773 bug is fixed
    // you should use 2.7.1/trunk branch in revision 28995 from 2014-11-05T22:17:54
    // => this will change the TInvokeableVariantType.SetProperty() signature
    {$define FPC_VARIANTSETVAR}
  {$endif ISFPC27}
  {$ifdef FPC_PROVIDE_ATTR_TABLE}
    {$define HASALIGNTYPEDATA} // to ignore attributes RTTI table
  {$endif FPC_PROVIDE_ATTR_TABLE}
  {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    {$define FPC_ENUMHASINNER}
    {$define HASALIGNTYPEDATA} // to ensure proper RTTI alignment
  {$endif FPC_REQUIRES_PROPER_ALIGNMENT}

  // define FPCSQLITE3STATIC to enable static-linked SQLite3 engine for FPC
  // -> to be downloaded from https://github.com/synopse/mORMot2/releases
  // -> could be disabled to force external .so/.dll linking
  {$ifdef MSWINDOWS}
  {$ifdef CPUX86}
    {$define FPCSQLITE3STATIC}   // use static\i386-win32\sqlite3.o
  {$else}
    {$define FPCSQLITE3STATIC}   // use static\x86_64-win64\sqlite3.o
  {$endif}
  {$endif}
  {$ifdef LINUXNOTBSD}
    {$ifdef CPUX86}
      {$define FPCSQLITE3STATIC} // use static/i386-linux/sqlite3.o
    {$endif}
    {$ifdef CPUX64}
      {$define FPCSQLITE3STATIC} // use static/x86_64-linux/sqlite3.o
    {$endif}
    {$ifdef CPUARM}
      {$define FPCSQLITE3STATIC} // use static/arm-linux/sqlite3.o
    {$endif}
    {$ifdef CPUAARCH64}
      {$ifdef ANDROID}
        {$define FPCSQLITE3STATIC}
        // note: problems with current static/aarch64-linux/sqlite3.o
      {$endif}
    {$endif}
  {$endif}

{$else FPC}

(********************** Delphi Conditionals **********************)

  {$ifndef MSWINDOWS}
  'Kylix or Delphi for MacOS/Linux/Mobile are unsupported'
  '-> we recommend using FPC for POSIX platforms'
  {$endif MSWINDOWS}
  {$IFNDEF CONDITIONALEXPRESSIONS}
  'Delphi 2-5 are not supported'
  {$ENDIF CONDITIONALEXPRESSIONS}
  {$ifdef VER140}
  'Delphi 6 is not supported'
  {$endif}

  {$A+} // force normal alignment

  {$ifdef UNICODE}
    {$define HASVARUSTRING}
    {$define HASCODEPAGE}
    {$define FPC_OR_UNICODE}
    {$define USERECORDWITHMETHODS}
    { due to a bug in Delphi 2009+, we need to fake inheritance of record,
      since TDynArrayHashed = object(TDynArray) fails to initialize
      http://blog.synopse.info/post/2011/01/29/record-and-object-issue-in-Delphi-2010 }
    {$define UNDIRECTDYNARRAY}
  {$else}
    {$define HASNOSTATICRTTI} // Delphi 7/2007 has no TypeInfo(TGUID)
  {$endif}

  {$define ISDELPHI}
  {$define CPUINTEL}   // Delphi only for Intel by now
  {$define ASMINTEL}   // either ASMX86 or ASMX64
  {$ifdef CPUX64}
    {$undef CPU32}
    {$define CPU64}    // Delphi compiler for 64 bit CPU
    {$define CPU64DELPHI}
    {$define ASMX64}   // supports asm with globals
    {$define EXTENDEDTOSHORT_USESTR} // FloatToText() slower in Delphi Win64
    {$define DOUBLETOSHORT_USEGRISU} // fast DoubleToAscii() - not Delphi Win32
  {$else CPUX64}
    {$define CPU32}    // Delphi compiler for 32 bit CPU
    {$undef CPU64}
    {$define CPU32DELPHI}
    {$define CPUX86}   // for compatibility with older versions of Delphi
    {$define ASMX86}   // supports asm with globals
    {$define CPUX86NOTPIC} // use "absolute" instead of local register
    {$define TSYNEXTENDED80} // only 32-bit has a true x87 extended type
  {$endif CPUX64}

  {$define DELPHI_OR_FPC_OLDRTTI}
  {$define HASINTERFACERTTI} // interface RTTI (not oldest FPC)
  {$warn UNSAFE_CODE OFF}    // Delphi for .Net does not exist any more!
  {$warn UNSAFE_TYPE OFF}
  {$warn UNSAFE_CAST OFF}
  {$warn DUPLICATE_CTOR_DTOR OFF} // avoid W1029 unneeded hints
  {$warn SYMBOL_PLATFORM OFF}
  {$warn SYMBOL_DEPRECATED OFF} // for faVolumeID
  {$warn UNIT_PLATFORM OFF}

  {$if CompilerVersion >= 17}
    {$define ISDELPHI2005ANDUP} // Delphi 2005 or newer
    {$if CompilerVersion >= 18}
      {$define ISDELPHI2006ANDUP} // Delphi 2006 or newer
      {$define HASNEWFILEAGE}
      {$define HASINLINE}
      {$define HASINLINEDELPHI}
      {$define HASREGION}
      {$define HASFASTMM4}
      // you can define this so that GetMemoryInfo/TSynMonitorMemory returns
      // low-level FastMM4 information
      {.$define WITH_FASTMM4STATS}
    {$ifend}
    {$ifdef VER180}
      {$define ISDELPHI20062007} // to circumvent some specific bugs
    {$endif}
    {$ifdef VER185}
      {$define ISDELPHI20062007}
    {$endif}
    {$if CompilerVersion > 18}
      {$define ISDELPHI2007ANDUP} // Delphi 2007 or newer
    {$ifend}
    {$if CompilerVersion = 20}
      {$define ISDELPHI20092010} // specific compilation issues
    {$ifend}
    {$if CompilerVersion = 21}
      {$define ISDELPHI20092010} //specific compilation issues
    {$ifend}
    {$if CompilerVersion >= 21.0}
      {$define ISDELPHI2010}
      {$define ISDELPHI2010_OR_FPC} // eltype2 field
      {$define ISDELPHI2010_OR_FPC_NEWRTTI}
      {$define HASTTHREADSTART}
      {$define HASINTERFACEASTOBJECT}
      {$ifdef NEWRTTINOTUSED} // option reduce EXE size by disabling much RTTI
        {$WEAKLINKRTTI ON}
        {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
      {$endif NEWRTTINOTUSED}
    {$ifend}
    {$if CompilerVersion >= 22.0}
      {$define FPC_OR_DELPHIXE} // Delphi 2007/2009/2010 inlining bugs
      {$define ISDELPHIXE}
    {$ifend}
    {$if CompilerVersion >= 23.0}
      // Delphi XE2 has some cross-platform features
      // e.g. {$ifdef ISDELPHIXE2}VCL.Graphics{$else}Graphics{$endif}
      {$define ISDELPHIXE2}
      {$define HASVARUSTRARG}
      {$define HASTTHREADTERMINATESET} // introduced TThread.TerminateSet
    {$ifend}
    {$if CompilerVersion >= 24.0}
      {$define ISDELPHIXE3}
    {$ifend}
    {$if CompilerVersion >= 25.0}
      {$define ISDELPHIXE4}
      {$define FPC_OR_DELPHIXE4} // circumvent Internal Error: C2130 on XE3
      {$define HASAESNI}
    {$ifend}
    {$if CompilerVersion >= 26.0}
      {$define ISDELPHIXE5}
      {$define PUBLISHRECORD}
      // if defined, will handle RTTI available only since Delphi XE5 for
      // record published properties
    {$ifend}
    {$if CompilerVersion >= 27.0}
      {$define ISDELPHIXE6}
    {$ifend}
    {$if CompilerVersion >= 28.0}
      {$define ISDELPHIXE7}
      {$ifdef CPU64}
        {$define CPUX64ASM} // e.g. Delphi XE4 SSE asm is buggy :(
      {$endif}
    {$ifend}
    {$if CompilerVersion >= 29.0}
      {$define ISDELPHIXE8}
    {$ifend}
    {$if CompilerVersion >= 30.0}
      {$define ISDELPHI10}
    {$ifend}
    {$if CompilerVersion >= 31.0}
      {$define ISDELPHI101}
    {$ifend}
    {$if CompilerVersion >= 32.0}
      {$define ISDELPHI102}
      {$ifdef CPUX64}
      {$ifdef VER320withoutAprilUpdate}
        // circumvent early Delphi 10.2 Tokyo Win64 compiler bug
        {$undef HASINLINE}
      {$endif}
      {$endif}
    {$ifend}
    {$if CompilerVersion >= 33.0}
      {$define ISDELPHI103}
    {$ifend}
    {$if CompilerVersion >= 34.0}
      {$define ISDELPHI104}
    {$ifend}
  {$ifend CompilerVersion >= 17}

{$endif FPC}


(********************** Shared Conditionals **********************)

{$H+} // we use long strings
{$R-} // disable Range checking in our code
{$S-} // disable Stack checking in our code
{$X+} // expect extended syntax
{$W-} // disable stack frame generation
{$Q-} // disable overflow checking in our code
{$B-} // expect short circuit boolean
{$V-} // disable Var-String Checking
{$T-} // Typed @ operator
{$Z1} // enumerators stored as byte by default
{$P+} // Open string params

{$ifdef CPUINTEL}
  {$ifdef MSWINDOWS}
  {$ifdef CPUX86} // safest to reset x87 exceptions on Win32
    {$define RESETFPUEXCEPTION}
  {$endif CPUX86}
  {$endif MSWINDOWS}
{$else}
  {$undef HASAESNI} // AES-NI is an Intel-specific feature
{$endif CPUINTEL}

{$ifdef FPC}
  {$ifndef FPCSQLITE3STATIC} // see above about this FPC-specific conditional
    {$define NOSQLITE3STATIC}
  {$endif}
{$else}
  // there is a linking bug with Delphi XE4 on Win64
  {$ifdef CPUX64}
    {$if CompilerVersion = 25.0} // exactly XE4
      {$define NOSQLITE3STATIC}
    {$ifend}
  {$endif} // other Win32/Win64 Delphi platforms should work as expected
{$endif FPC}

{$ifdef NOSQLITE3STATIC}
  // our proprietary crypto expects a statically linked custom sqlite3.c
  {$define NOSQLITE3ENCRYPT}
{$endif NOSQLITE3STATIC}


{$ifdef MSWINDOWS}

  {$define USEWININET}         // publish TWinINet/TWinHttp/TWinHttpAPI classes
  {.$define ONLYUSEHTTPSOCKET} // for testing (no benefit vs http.sys)
  {.$define USELIBCURL}        // for testing (no benefit vs WinHTTP)

  {$define USE_WINIOCP} // I/O completion ports API is the best option under Windows
  // (under Linux/POSIX, we fallback to a classical event-driven pool)

{$else}

  {$define ONLYUSEHTTPSOCKET}  // http.sys server is Windows-specific
  // cross-platform libcurl for https -> TCurlHttp and TRestHttpClientCurl
  {$define USELIBCURL}
  {$ifdef ANDROID}
    // for Android, consider using https://github.com/gcesarmza/curl-android-ios
    // static libraries and force USELIBCURL in the project conditionals
    {$define LIBCURLSTATIC}
  {$endif ANDROID}

{$endif MSWINDOWS}

{$ifdef USELIBCURL}
  {.$define LIBCURLMULTI}
  // enable https://curl.haxx.se/libcurl/c/libcurl-multi.html interface
{$endif USELIBCURL}

