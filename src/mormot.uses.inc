{
  This file is a part of the Open Source Synopse mORMot framework 2,
  licensed under a MPL/GPL/LGPL three license - see LICENSE.md

  Content of this file should be part of the .dpr/.lpr main uses clauses,
  to ensure proper cross-platform process.
}


{$ifdef FPC} // we may be on Delphi for Linux

  // setup the expected memory manager
  {$ifdef CPUX64}         // our x86_64 asm MM
  {$ifdef FPC_X64MM}
    mormot.core.fpcx64mm, // redirect to our optimized x86_64 asm Memory Manager
    {$undef FPC_LIBCMM}   // no fpclibcmm
  {$endif FPC_X64MM}
  {$endif CPUX64}
  {$ifdef FPC_LIBCMM}     // libc MM
    {$ifdef LINUX}
    mormot.core.fpclibcmm, // redirect to libc memory manager on LINUX
    {$else}
    cmem,                  // missing malloc_usable_size(): need size prefix
    {$endif LINUX}
  {$endif FPC_LIBCMM}

  // setup the libc threads on POSIX
  {$ifdef UNIX} // we may also be on Darwin / OSX
    cthreads, // if you use threads: always needed for servers
  {$endif UNIX}

  // optional WideString manager: cwstring (or iosxwstr for Mac/Darwin)

{$else}

  // enable FastMM4 on older versions of Delphi
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$if CompilerVersion <= 17} // Delphi 2006 (and up) have FastMM4 integrated
      FastMM4,
    {$ifend}
    {$ifdef UNICODE}
      {$warn DUPLICATE_CTOR_DTOR OFF} // global at .dpr level - see QC 100815
    {$endif UNICODE}
  {$ELSE}
    FastMM4, // no conditional expressions -> versions older than Delphi 6
  {$ENDIF CONDITIONALEXPRESSIONS}

{$endif FPC}

