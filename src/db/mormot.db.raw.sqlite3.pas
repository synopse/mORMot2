/// Database Framework Low-Level SQLite3 Engine
// - this unit is a part of the Open Source Synopse mORMot framework 2,
// licensed under a MPL/GPL/LGPL three license - see LICENSE.md
unit mormot.db.raw.sqlite3;

{
  *****************************************************************************

   Direct Access to the SQLite3 Database Engine
    - Raw SQLite3 API Constants and Functions
    - High-Level Classes for SQlite3 Queries

  *****************************************************************************
}

interface

{$I ..\mormot.defines.inc}

uses
  sysutils,
  classes,
  mormot.core.base,
  mormot.core.os,
  mormot.core.data,
  mormot.core.unicode,
  mormot.core.text,
  mormot.core.datetime,
  mormot.core.json,
  mormot.core.rtti,
  mormot.core.perf,
  mormot.core.buffers,
  mormot.core.variants,
  mormot.core.search,
  mormot.core.log,
  mormot.db.core,
  mormot.lib.static;


{ ************ Raw SQLite3 API Constants and Functions }

{$ifdef FPC}
  {$packrecords C}
  {$packenum 4}
{$endif FPC}

type
  /// internally store the SQLite3 database handle
  TSqlite3DB = type PtrUInt;

  /// internally store the SQLite3 statement handle
  // - This object is variously known as a "prepared statement" or a "compiled
  // SQL statement" or simply as a "statement".
  // - Create the object using sqlite3.prepare_v2() or a related function.
  // - Bind values to host parameters using the sqlite3.bind_*() interfaces.
  // - Run the SQL by calling sqlite3.step() one or more times.
  // - Reset the statement using sqlite3.reset() then go back to "Bind" step.
  //  Do this zero or more times.
  // - Destroy the object using sqlite3.finalize().
  TSqlite3Statement = type PtrUInt;

  /// internally store the SQLite3 blob handle
  TSqlite3Blob = type PtrUInt;

  /// internally store a SQLite3 Dynamically Typed Value Object
  // - SQLite uses the sqlite3.value object to represent all values that
  // can be stored in a database table, which are mapped to this TSqlite3Value type
  // - SQLite uses dynamic typing for the values it stores
  // - Values stored in sqlite3.value objects can be integers, floating point
  // values, strings, BLOBs, or NULL
  TSqlite3Value = type PtrUInt;

  /// internal store a SQLite3 Function Context Object
  // - The context in which an SQL function executes is stored in an sqlite3.context
  // object, which is mapped to this TSqlite3FunctionContext type
  // - A pointer to an sqlite3.context object is always first parameter to
  // application-defined SQL functions, i.e. a TSqlFunctionFunc prototype
  TSqlite3FunctionContext = type PtrUInt;

  /// internally store a SQLite3 Backup process handle
  TSqlite3Backup = type PtrUInt;

  /// internally store of SQLite3 values, as used by TSqlFunctionFunc
  TSqlite3ValueArray = array[0..63] of TSqlite3Value;

  /// A pointer to the opaque TSqlite3APIRoutines structure is passed as the third
  // parameter to entry points of loadable extensions.
  TSqlite3APIRoutines = type PtrUInt;

const
  {$ifdef OSWINDOWS}
    {$ifdef CPU64}
      // see https://synopse.info/files/SQLite3-64.7z or static/delphi folder
      SQLITE_LIBRARY_DEFAULT_NAME = 'sqlite3-64.dll';
    {$else}
      SQLITE_LIBRARY_DEFAULT_NAME = 'sqlite3.dll';
    {$endif CPU64}
  {$else}
  {$ifdef OSPOSIX}
    {$ifdef OSANDROID}
      SQLITE_LIBRARY_DEFAULT_NAME = 'libsqlite.so';
    {$else}
      {$ifdef OSDARWIN}
      SQLITE_LIBRARY_DEFAULT_NAME = 'libsqlite3.dylib';
      {$else}
      SQLITE_LIBRARY_DEFAULT_NAME = 'libsqlite3.so.0';
      {$endif OSDARWIN}
    {$endif OSANDROID}
  {$endif OSPOSIX}
  {$endif OSWINDOWS}

  /// internal SQLite3 type as integer
  SQLITE_INTEGER = 1;
  /// internal SQLite3 type as Floating point value
  SQLITE_FLOAT = 2;
  /// internal SQLite3 type as Text
  SQLITE_TEXT = 3;
  /// internal SQLite3 type as Blob
  SQLITE_BLOB = 4;
  /// internal SQLite3 type as NULL
  SQLITE_NULL = 5;

  /// text is UTF-8 encoded
  SQLITE_UTF8 = 1;
  /// text is UTF-16 LE encoded
  SQLITE_UTF16LE = 2;
  /// text is UTF-16 BE encoded
  SQLITE_UTF16BE = 3;
  /// text is UTF-16 encoded, using the system native byte order
  SQLITE_UTF16 = 4;
  /// sqlite3.create_function don't care about text encoding
  SQLITE_ANY = 5;
  /// used by sqlite3.create_collation() only
  SQLITE_UTF16_ALIGNED = 8;


  /// Successful result. No error occured
  SQLITE_OK = 0;
  /// SQL error or missing database - legacy generic code
  // - Use Extended Result Codes for more detailed information about errors
  // - sqlite3.extended_result_codes() enables or disables the extended result codes
  // - Or, the extended code for the most recent error can be obtained using
  // sqlite3.extended_errcode()
  SQLITE_ERROR = 1;
  /// An internal logic error in SQLite
  SQLITE_INTERNAL = 2;
  /// Access permission denied
  SQLITE_PERM = 3;
  /// Callback routine requested an abort
  SQLITE_ABORT = 4;
  /// The database file is locked
  SQLITE_BUSY = 5;
  /// A table in the database is locked
  SQLITE_LOCKED = 6;
  /// A malloc() failed
  SQLITE_NOMEM = 7;
  /// Attempt to write a readonly database
  SQLITE_READONLY = 8;
  /// Operation terminated by sqlite3.interrupt()
  SQLITE_INTERRUPT = 9;
  /// Some kind of disk I/O error occurred
  SQLITE_IOERR = 10;
  /// The database disk image is malformed
  SQLITE_CORRUPT = 11;
  /// (Internal Only) Table or record not found
  SQLITE_NOTFOUND = 12;
  /// Insertion failed because database is full
  SQLITE_FULL = 13;
  /// Unable to open the database file
  SQLITE_CANTOPEN = 14;
  /// (Internal Only) Database lock protocol error
  SQLITE_PROTOCOL = 15;
  /// Database is empty
  SQLITE_EMPTY = 16;
  /// The database schema changed, and unable to be recompiled
  SQLITE_SCHEMA = 17;
  /// Too much data for one row of a table
  SQLITE_TOOBIG = 18;
  /// Abort due to contraint violation
  SQLITE_CONSTRAINT = 19;
  /// Data type mismatch
  SQLITE_MISMATCH = 20;
  /// Library used incorrectly
  SQLITE_MISUSE = 21;
  /// Uses OS features not supported on host
  SQLITE_NOLFS = 22;
  /// Authorization denied
  SQLITE_AUTH = 23;
  /// Auxiliary database format error
  SQLITE_FORMAT = 24;
  /// 2nd parameter to sqlite3.bind out of range
  SQLITE_RANGE = 25;
  /// File opened that is not a database file
  SQLITE_NOTADB = 26;

  /// sqlite3.step() return code: another result row is ready
  SQLITE_ROW = 100;
  /// sqlite3.step() return code: has finished executing
  SQLITE_DONE = 101;

  /// possible error codes for sqlite_exec() and sqlite3.step()
  // - as verified by sqlite3_check()
  SQLITE_ERRORS = [SQLITE_ERROR..SQLITE_ROW-1];


  /// The sqlite3.load_extension() interface loads an extension into a single database connection.
  // - The default behavior is for that extension to be automatically unloaded when the database
  // connection closes.
  // - However, if the extension entry point returns SQLITE_OK_LOAD_PERMANENTLY instead of
  // SQLITE_OK, then the extension remains loaded into the process address space after the
  // database connection closes.
  // - In other words, the xDlClose methods of the sqlite3_vfs object is not called for the
  // extension when the database connection closes.
  // - The SQLITE_OK_LOAD_PERMANENTLY return code is useful to loadable extensions that
  // register new VFSes, for example.
  SQLITE_OK_LOAD_PERMANENTLY = 256;
  /// The SQLITE_ERROR_MISSING_COLLSEQ result code means that an SQL statement could not be
  // prepared because a collating sequence named in that SQL statement could not be located.
  // - Sometimes when this error code is encountered, the sqlite3.prepare_v2() routine will
  // convert the error into SQLITE_ERROR_RETRY and try again to prepare the SQL statement
  // using a different query plan that does not require the use of the unknown collating sequence.
  SQLITE_ERROR_MISSING_COLLSEQ = 257;
  /// The SQLITE_BUSY_RECOVERY error code is an extended error code for SQLITE_BUSY that
  // indicates that an operation could not continue because another process is busy recovering
  // a WAL mode database file following a crash.
  // - The SQLITE_BUSY_RECOVERY error code only occurs on WAL mode databases.
  SQLITE_BUSY_RECOVERY = 261;
  /// The SQLITE_LOCKED_SHAREDCACHE result code indicates that access to an SQLite data
  // record is blocked by another database connection that is using the same record in
  // shared cache mode.
  // - When two or more database connections share the same cache and one of the connections
  // is in the middle of modifying a record in that cache, then other connections are blocked
  // from accessing that data while the modifications are on-going in order to prevent the
  // readers from seeing a corrupt or partially completed change.
  SQLITE_LOCKED_SHAREDCACHE = 262;
  /// The SQLITE_READONLY_RECOVERY error code is an extended error code for SQLITE_READONLY.
  // - The SQLITE_READONLY_RECOVERY error code indicates that a WAL mode database cannot be
  // opened because the database file needs to be recovered and recovery requires write access
  // but only read access is available.
  SQLITE_READONLY_RECOVERY = 264;
  /// The SQLITE_IOERR_READ error code is an extended error code for SQLITE_IOERR indicating
  // an I/O error in the VFS layer while trying to read from a file on disk.
  // - This error might result from a hardware malfunction or because a filesystem came
  // unmounted while the file was open.
  SQLITE_IOERR_READ = 266;
  /// The SQLITE_CORRUPT_VTAB error code is an extended error code for SQLITE_CORRUPT used
  // by virtual tables.
  // - A virtual table might return SQLITE_CORRUPT_VTAB to indicate that content in the
  // virtual table is corrupt.
  SQLITE_CORRUPT_VTAB = 267;
  /// The SQLITE_CONSTRAINT_CHECK error code is an extended error code for SQLITE_CONSTRAINT
  // indicating that a CHECK constraint failed.
  SQLITE_CONSTRAINT_CHECK = 275;
  /// The SQLITE_NOTICE_RECOVER_WAL result code is passed to the callback of sqlite3.log()
  // when a WAL mode database file is recovered.
  SQLITE_NOTICE_RECOVER_WAL = 283;
  /// The SQLITE_WARNING_AUTOINDEX result code is passed to the callback of sqlite3.log()
  // whenever automatic indexing is used.
  // - This can serve as a warning to application designers that the database might benefit
  // from additional indexes.
  SQLITE_WARNING_AUTOINDEX = 284;
  /// The SQLITE_ERROR_RETRY is used internally to provoke sqlite3.prepare_v2()(or one of its
  // sibling routines for creating prepared statements) to try again to prepare a statement
  // that failed with an error on the previous attempt.
  SQLITE_ERROR_RETRY = 513;
  /// The SQLITE_ABORT_ROLLBACK error code is an extended error code for SQLITE_ABORT indicating
  // that an SQL statement aborted because the transaction that was active when the SQL statement
  // first started was rolled back.
  // - Pending write operations always fail with this error when a rollback occurs.
  // - A ROLLBACK will cause a pending read operation to fail only if the schema was changed within
  // the transaction being rolled back.
  SQLITE_ABORT_ROLLBACK = 516;
  /// The SQLITE_BUSY_SNAPSHOT error code is an extended error code for SQLITE_BUSY that occurs
  // on WAL mode databases when a database connection tries to promote a read transaction into
  // a write transaction but finds that another database connection has already written to the
  // database and thus invalidated prior reads.
  // - The following scenario illustrates how an SQLITE_BUSY_SNAPSHOT error might arise:
  //  - Process A starts a read transaction on the database and does one or more SELECT statement.
  //  - Process A keeps the transaction open.
  //  - Process B updates the database, changing values previous read by process A.
  //  - Process A now tries to write to the database. But process A's view of the database content is
  // now obsolete because process B has modified the database file after process A read from it.
  // Hence process A gets an SQLITE_BUSY_SNAPSHOT error.
  SQLITE_BUSY_SNAPSHOT = 517;
  /// The SQLITE_LOCKED_VTAB result code is not used by the SQLite core, but it is available for
  // use by extensions.
  // - Virtual table implementations can return this result code to indicate that they cannot
  // complete the current operation because of locks held by other threads or processes.
  // - The R-Tree extension returns this result code when an attempt is made to update the R-Tree
  // while another prepared statement is actively reading the R-Tree.
  // - The update cannot proceed because any change to an R-Tree might involve reshuffling and
  // rebalancing of nodes, which would disrupt read cursors, causing some rows to be repeated and
  // other rows to be omitted.
  SQLITE_LOCKED_VTAB = 518;
   /// The SQLITE_READONLY_CANTLOCK error code is an extended error code for SQLITE_READONLY.
   // - The SQLITE_READONLY_CANTLOCK error code indicates that SQLite is unable to obtain a read
   // lock on a WAL mode database because the shared-memory file associated with that database
   // is read-only.
  SQLITE_READONLY_CANTLOCK = 520;
  /// The SQLITE_IOERR_SHORT_READ error code is an extended error code for SQLITE_IOERR indicating
  // that a read attempt in the VFS layer was unable to obtain as many bytes as was requested.
  // - This might be due to a truncated file.
  SQLITE_IOERR_SHORT_READ = 522;
  /// The SQLITE_CORRUPT_SEQUENCE result code means that the schema of the sqlite_sequence table
  //is corrupt.
  // - The sqlite_sequence table is used to help implement the AUTOINCREMENT feature.
  // - The sqlite_sequence table should have the following format:
  //   CREATE TABLE sqlite_sequence(name,seq);
  // - If SQLite discovers that the sqlite_sequence table has any other format, it returns the
  // SQLITE_CORRUPT_SEQUENCE error.
  SQLITE_CORRUPT_SEQUENCE = 523;
  /// The SQLITE_CANTOPEN_ISDIR error code is an extended error code for SQLITE_CANTOPEN indicating
  // that a file open operation failed because the file is really a directory.
  SQLITE_CANTOPEN_ISDIR = 526;
  /// The SQLITE_CONSTRAINT_COMMITHOOK error code is an extended error code for SQLITE_CONSTRAINT
  // indicating that a commit hook callback returned non-zero that thus caused the SQL statement
  // to be rolled back.
  SQLITE_CONSTRAINT_COMMITHOOK = 531;
  /// The SQLITE_NOTICE_RECOVER_ROLLBACK result code is passed to the callback of sqlite3.log() when
  // a hot journal is rolled back.
  SQLITE_NOTICE_RECOVER_ROLLBACK = 539;
  /// The SQLITE_ERROR_SNAPSHOT result code might be returned when attempting to start a read transaction
  // on an historical version of the database by using the sqlite3.snapshot_open() interface.
  // - If the historical snapshot is no longer available, then the read transaction will fail with the
  // SQLITE_ERROR_SNAPSHOT.
  SQLITE_ERROR_SNAPSHOT = 769;
  /// The SQLITE_BUSY_TIMEOUT error code indicates that a blocking Posix advisory file lock request in
  // the  VFS layer failed due to a timeout.
  // - Blocking Posix advisory locks are only available as a proprietary SQLite extension and even then
  // are only supported if SQLite is compiled with the SQLITE_EANBLE_SETLK_TIMEOUT compile-time option.
  SQLITE_BUSY_TIMEOUT = 773;
  /// The SQLITE_READONLY_ROLLBACK error code is an extended error code for SQLITE_READONLY.
  // The SQLITE_READONLY_ROLLBACK error code indicates that a database cannot be opened because it has
  // a hot journal that needs to be rolled back but cannot because the database is readonly.
  SQLITE_READONLY_ROLLBACK = 776;
  /// The SQLITE_IOERR_WRITE error code is an extended error code for SQLITE_IOERR indicating an I/O
  // error in the VFS layer while trying to write into a file on disk.
  // - This error might result from a hardware malfunction or because a filesystem came unmounted while
  // the file was open.
  // - This error should not occur if the filesystem is full as there is a separate error code
  // (SQLITE_FULL) for that purpose.
  SQLITE_IOERR_WRITE = 778;
  /// The SQLITE_CORRUPT_INDEX result code means that SQLite detected an entry is or was missing
  // from an index.
  // - This is a special case of the SQLITE_CORRUPT error code that suggests that the problem
  // might be resolved by running the REINDEX command, assuming no other problems exist elsewhere
  // in the database file.
  SQLITE_CORRUPT_INDEX = 779;
  /// The SQLITE_CANTOPEN_FULLPATH error code is an extended error code for SQLITE_CANTOPEN indicating
  // that a file open operation failed because the operating system was unable to convert the filename
  // into a full pathname.
  SQLITE_CANTOPEN_FULLPATH = 782;
  /// The SQLITE_CONSTRAINT_FOREIGNKEY error code is an extended error code for SQLITE_CONSTRAINT
  // indicating that a foreign key constraint failed.
  SQLITE_CONSTRAINT_FOREIGNKEY = 787;
  /// The SQLITE_READONLY_DBMOVED error code is an extended error code for SQLITE_READONLY.
  // - The SQLITE_READONLY_DBMOVED error code indicates that a database cannot be modified because the
  // database file has been moved since it was opened, and so any attempt to modify the database might
  // result in database corruption if the processes crashes because the rollback journal would not be
  // correctly named.
  SQLITE_READONLY_DBMOVED = 1032;
  /// The SQLITE_IOERR_FSYNC error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // in the VFS layer while trying to flush previously written content out of OS and/or disk-control
  // buffers and into persistent storage.
  // - In other words, this code indicates a problem with the fsync() system call in unix or the
  // FlushFileBuffers() system call in windows.
  SQLITE_IOERR_FSYNC = 1034;
  /// The SQLITE_CANTOPEN_CONVPATH error code is an extended error code for SQLITE_CANTOPEN used only by
  // Cygwin VFS and indicating that the cygwin_conv_path() system call failed while trying to open a file.
  // - See also: SQLITE_IOERR_CONVPATH
  SQLITE_CANTOPEN_CONVPATH = 1038;
  /// The SQLITE_CONSTRAINT_FUNCTION error code is not currently used by the SQLite core.
  // - However, this error code is available for use by extension functions.
  SQLITE_CONSTRAINT_FUNCTION = 1043;
  /// The SQLITE_READONLY_CANTINIT result code originates in the xShmMap method of a VFS to indicate that
  // the shared memory region used by WAL mode exists buts its content is unreliable and unusable by the
  // current process since the current process does not have write permission on the shared memory region.
  // - The shared memory region for WAL mode is normally a file with a "-wal" suffix that is mmapped into
  // the process space.
  // - If the current process does not have write permission on that file, then it cannot write into shared memory.
  // - Higher level logic within SQLite will normally intercept the error code and create a temporary in-memory
  // shared memory region so that the current process can at least read the content of the database.
  // - This result code should not reach the application interface layer.
  SQLITE_READONLY_CANTINIT = 1288;
  /// The SQLITE_IOERR_DIR_FSYNC error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // in the VFS layer while trying to invoke fsync() on a directory.
  // - The unix VFS attempts to fsync() directories after creating or deleting certain files to ensure that
  // those files will still appear in the filesystem following a power loss or system crash.
  // - This error code indicates a problem attempting to perform that fsync().
  SQLITE_IOERR_DIR_FSYNC = 1290;
  /// The SQLITE_CANTOPEN_DIRTYWAL result code is not used at this time.
  SQLITE_CANTOPEN_DIRTYWAL = 1294;
  /// The SQLITE_CONSTRAINT_NOTNULL error code is an extended error code for SQLITE_CONSTRAINT indicating that
  // a NOT NULL constraint failed.
  SQLITE_CONSTRAINT_NOTNULL = 1299;
  /// The SQLITE_READONLY_DIRECTORY result code indicates that the database is read-only because process does
  // not have permission to create a journal file in the same directory as the database and the creation of a
  // journal file is a prerequisite for writing.
  SQLITE_READONLY_DIRECTORY = 1544;
  /// The SQLITE_IOERR_TRUNCATE error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // in the VFS layer while trying to truncate a file to a smaller size.
  SQLITE_IOERR_TRUNCATE = 1546;
  /// The SQLITE_CANTOPEN_SYMLINK result code is returned by the sqlite3.open() interface and its siblings when
  // the SQLITE_OPEN_NOFOLLOW flag is used and the database file is a symbolic link.
  SQLITE_CANTOPEN_SYMLINK = 1550;
  /// The SQLITE_CONSTRAINT_PRIMARYKEY error code is an extended error code for SQLITE_CONSTRAINT indicating
  // that a PRIMARY KEY constraint failed.
  SQLITE_CONSTRAINT_PRIMARYKEY = 1555;
  /// The SQLITE_IOERR_FSTAT error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // in the VFS layer while trying to invoke fstat() (or the equivalent) on a file in order to determine
  // information such as the file size or access permissions.
  SQLITE_IOERR_FSTAT = 1802;
  /// The SQLITE_CONSTRAINT_TRIGGER error code is an extended error code for SQLITE_CONSTRAINT indicating
  // that a RAISE function within a trigger fired, causing the SQL statement to abort.
  SQLITE_CONSTRAINT_TRIGGER = 1811;
  /// The SQLITE_IOERR_UNLOCK error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // within xUnlock method on the sqlite3_io_methods object.
  SQLITE_IOERR_UNLOCK = 2058;
  /// The SQLITE_CONSTRAINT_UNIQUE error code is an extended error code for SQLITE_CONSTRAINT indicating
  // that a UNIQUE constraint failed.
  SQLITE_CONSTRAINT_UNIQUE = 2067;
  /// The SQLITE_IOERR_UNLOCK error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // within xLock method on the sqlite3_io_methods object while trying to obtain a read lock.
  SQLITE_IOERR_RDLOCK = 2314;
  /// The SQLITE_CONSTRAINT_VTAB error code is not currently used by the SQLite core. However, this error
  // code is available for use by application-defined virtual tables.
  SQLITE_CONSTRAINT_VTAB = 2323;
  /// The SQLITE_IOERR_UNLOCK error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // within xDelete method on the sqlite3_vfs object.
  SQLITE_IOERR_DELETE = 2570;
  /// The SQLITE_CONSTRAINT_ROWID error code is an extended error code for SQLITE_CONSTRAINT indicating
  // that a rowid is not unique.
  SQLITE_CONSTRAINT_ROWID = 2579;
  /// The SQLITE_CONSTRAINT_PINNED error code is an extended error code for SQLITE_CONSTRAINT indicating
  // that an UPDATE trigger attempted do delete the row that was being updated in the middle of the update.
  SQLITE_CONSTRAINT_PINNED = 2835;
  /// The SQLITE_IOERR_NOMEM error code is sometimes returned by the VFS layer to indicate that an operation
  // could not be completed due to the inability to allocate sufficient memory.
  // This error code is normally converted into SQLITE_NOMEM by the higher layers of SQLite before being
  // returned to the application.
  SQLITE_IOERR_NOMEM = 3082;
  /// The SQLITE_IOERR_ACCESS error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // within the xAccess method on the sqlite3_vfs object.
  SQLITE_IOERR_ACCESS = 3338;
  /// The SQLITE_IOERR_CHECKRESERVEDLOCK error code is an extended error code for SQLITE_IOERR indicating
  // an I/O error within the xCheckReservedLock method on the sqlite3_io_methods object.
  SQLITE_IOERR_CHECKRESERVEDLOCK = 3594;
  /// The SQLITE_IOERR_LOCK error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // in the advisory file locking logic.
  // - Usually an SQLITE_IOERR_LOCK error indicates a problem obtaining a PENDING lock.
  // - However it can also indicate miscellaneous locking errors on some of the specialized VFSes used on Macs.
  SQLITE_IOERR_LOCK = 3850;
  /// The SQLITE_IOERR_ACCESS error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // within the xClose method on the sqlite3_io_methods object.
  SQLITE_IOERR_DIR_CLOSE = 4362;
  /// The SQLITE_IOERR_SHMOPEN error code is an extended error code for SQLITE_IOERR indicating an I/O error
    // within the xShmMap method on the sqlite3_io_methods object while trying to open a new shared memory segment.
  SQLITE_IOERR_SHMOPEN = 4618;
  /// The SQLITE_IOERR_SHMSIZE error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // within the xShmMap method on the sqlite3_io_methods object while trying to enlarge a "shm" file as part
  // of WAL mode transaction processing. This error may indicate that the underlying filesystem volume is out of space.
  SQLITE_IOERR_SHMSIZE = 4874;
  /// The SQLITE_IOERR_SHMMAP error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // within the xShmMap method on the sqlite3_io_methods object while trying to map a shared memory segment
  // into the process address space.
  SQLITE_IOERR_SHMMAP = 5386;
  /// The SQLITE_IOERR_SEEK error code is an extended error code for SQLITE_IOERR indicating an I/O error
  // within the xRead or xWrite methods on the sqlite3_io_methods object while trying to seek a file descriptor
  // to the beginning point of the file where the read or write is to occur.
  SQLITE_IOERR_SEEK = 5642;
  /// The SQLITE_IOERR_DELETE_NOENT error code is an extended error code for SQLITE_IOERR indicating that the
  // xDelete method on the sqlite3_vfs object failed because the file being deleted does not exist.
  SQLITE_IOERR_DELETE_NOENT = 5898;
  /// The SQLITE_IOERR_MMAP error code is an extended error code for SQLITE_IOERR indicating an I/O error within
  // the xFetch or xUnfetch methods on the sqlite3_io_methods object while trying to map or unmap part of the
  // database file into the process address space.
  SQLITE_IOERR_MMAP = 6154;
  /// The SQLITE_IOERR_GETTEMPPATH error code is an extended error code for SQLITE_IOERR indicating that the VFS
  // is unable to determine a suitable directory in which to place temporary files.
  SQLITE_IOERR_GETTEMPPATH = 6410;
  /// The SQLITE_IOERR_CONVPATH error code is an extended error code for SQLITE_IOERR used only by Cygwin VFS
  // and indicating that the cygwin_conv_path() system call failed.
  // - See also: SQLITE_CANTOPEN_CONVPATH
  SQLITE_IOERR_CONVPATH = 6666;
  /// The SQLITE_IOERR_VNODE error code is a code reserved for use by extensions.
  SQLITE_IOERR_VNODE  = 6922;
  /// The SQLITE_IOERR_AUTH error code is a code reserved for use by extensions.
  SQLITE_IOERR_AUTH = 7178;
  /// The SQLITE_IOERR_BEGIN_ATOMIC error code indicates that the underlying operating system reported and error
  // on the SQLITE_FCNTL_BEGIN_ATOMIC_WRITE file-control.
  // - This only comes up when SQLITE_ENABLE_ATOMIC_WRITE is enabled and the database is hosted on a filesystem
  // that supports atomic writes.
  SQLITE_IOERR_BEGIN_ATOMIC = 7434;
  /// The SQLITE_IOERR_COMMIT_ATOMIC error code indicates that the underlying operating system reported and error
  // on the SQLITE_FCNTL_COMMIT_ATOMIC_WRITE file-control.
  // - This only comes up when SQLITE_ENABLE_ATOMIC_WRITE is enabled and the database is hosted on a filesystem
  // that supports atomic writes.
  SQLITE_IOERR_COMMIT_ATOMIC = 7690;
  /// The SQLITE_IOERR_ROLLBACK_ATOMIC error code indicates that the underlying operating system reported and error
  // on the SQLITE_FCNTL_ROLLBACK_ATOMIC_WRITE file-control. This only comes up when SQLITE_ENABLE_ATOMIC_WRITE is
  // enabled and the database is hosted on a filesystem that supports atomic writes.
  SQLITE_IOERR_ROLLBACK_ATOMIC = 7946;
  /// The SQLITE_IOERR_DATA error code is an extended error code for SQLITE_IOERR used only by checksum VFS shim to
  // indicate that the checksum on a page of the database file is incorrect.
  SQLITE_IOERR_DATA = 8202;


  /// The database is opened in read-only mode
  // - if the database does not already exist, an error is returned
  // - Ok for sqlite3.open_v2()
  SQLITE_OPEN_READONLY = $00000001;
  /// The database is opened for reading and writing if possible, or reading
  // only if the file is write protected by the operating system
  // - In either case the database must already exist, otherwise an error is
  // returned
  // - Ok for sqlite3.open_v2()
  SQLITE_OPEN_READWRITE = $00000002;
  /// In conjunction with SQLITE_OPEN_READWRITE, optionally create the database
  // file if it does not exist
  // - The database is opened for reading and writing if possible, or reading
  // only if the file is write protected by the operating system
  // - In either case the database must already exist, otherwise an error is returned
  SQLITE_OPEN_CREATE = $00000004;
  /// URI filename interpretation is enabled if the SQLITE_OPEN_URI flag is set
  // in the fourth argument to sqlite3.open_v2(), or if it has been enabled
  // globally using the SQLITE_CONFIG_URI option with the sqlite3.config() method
  // or by the SQLITE_USE_URI compile-time option.
  // - As of SQLite version 3.7.7, URI filename interpretation is turned off by
  // default, but future releases of SQLite might enable URI filename
  // interpretation by default
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_URI = $00000040;  // Ok for sqlite3_open_v2()
  /// If the SQLITE_OPEN_NOMUTEX flag is set, then the database will remain in
  // memory
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_MEMORY = $00000080;  // Ok for sqlite3_open_v2()
  /// If the SQLITE_OPEN_NOMUTEX flag is set, then the database connection opens
  // in the multi-thread threading mode as long as the single-thread mode has
  // not been set at compile-time or start-time
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_NOMUTEX = $00008000;  // Ok for sqlite3_open_v2()
  /// If the SQLITE_OPEN_FULLMUTEX flag is set then the database connection opens
  // in the serialized threading mode unless single-thread was previously selected
  // at compile-time or start-time
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_FULLMUTEX = $00010000;  // Ok for sqlite3_open_v2()
  /// The SQLITE_OPEN_SHAREDCACHE flag causes the database connection to be
  // eligible to use shared cache mode, regardless of whether or not shared
  // cache is enabled using sqlite3.enable_shared_cache()
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_SHAREDCACHE = $00020000;  // Ok for sqlite3_open_v2()
  /// The SQLITE_OPEN_PRIVATECACHE flag causes the database connection to not
  // participate in shared cache mode even if it is enabled
  // - Ok for sqlite3.open_v2(), in conjuction with SQLITE_OPEN_READONLY,
  // SQLITE_OPEN_READWRITE, (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
  SQLITE_OPEN_PRIVATECACHE = $00040000;

{
  SQLITE_OPEN_DELETEONCLOSE  = $00000008;  // VFS only
  SQLITE_OPEN_EXCLUSIVE      = $00000010;  // VFS only
  SQLITE_OPEN_AUTOPROXY      = $00000020;  // VFS only
  SQLITE_OPEN_MAIN_DB        = $00000100;  // VFS only
  SQLITE_OPEN_TEMP_DB        = $00000200;  // VFS only
  SQLITE_OPEN_TRANSIENT_DB   = $00000400;  // VFS only
  SQLITE_OPEN_MAIN_JOURNAL   = $00000800;  // VFS only
  SQLITE_OPEN_TEMP_JOURNAL   = $00001000;  // VFS only
  SQLITE_OPEN_SUBJOURNAL     = $00002000;  // VFS only
  SQLITE_OPEN_MASTER_JOURNAL = $00004000;  // VFS only
  SQLITE_OPEN_WAL            = $00080000;  // VFS only
}

  /// DestroyPtr set to SQLITE_STATIC if data is constant and will never change
  // - SQLite assumes that the text or BLOB result is in constant space and
  // does not copy the content of the parameter nor call a destructor on the
  // content when it has finished using that result
  SQLITE_STATIC = pointer(0);

  /// DestroyPtr set to SQLITE_TRANSIENT for SQLite3 to make a private copy of
  // the data into space obtained from from sqlite3.malloc() before it returns
  // - this is the default behavior in our framework
  // - note that we discovered that under Win64, sqlite3.result_text() expects
  // SQLITE_TRANSIENT_VIRTUALTABLE=pointer(integer(-1)) and not pointer(-1)
  SQLITE_TRANSIENT = pointer(-1);

  /// DestroyPtr set to SQLITE_TRANSIENT_VIRTUALTABLE for setting results to
  // SQlite3 virtual tables columns
  // - due to a bug of the SQlite3 engine under Win64
  SQLITE_TRANSIENT_VIRTUALTABLE = pointer(integer(-1));

  /// pseudo database file name used to create an in-memory database
  // - an SQLite database is normally stored in a single ordinary disk file -
  // however, in certain circumstances, the database might be stored in memory,
  // if you pass SQLITE_MEMORY_DATABASE_NAME to TSqlDatabase.Create() instead of
  // a real disk file name
  // - this instance will cease to exist as soon as the database connection
  // is closed, i.e. when calling TSqlDatabase.Free
  // - every ':memory:' database is distinct from every other - so, creating two
  // TSqlDatabase instances each with the filename SQLITE_MEMORY_DATABASE_NAME
  //  will create two independent in-memory databases
  SQLITE_MEMORY_DATABASE_NAME = ':memory:';


  /// This option sets the threading mode to Single-thread
  // - In other words, it disables all mutexing and puts SQLite into a mode where it
  // can only be used by a single thread. If SQLite is compiled with the
  // SQLITE_THREADSAFE=0 compile-time option then it is not possible to change the
  // threading mode from its default value of Single-thread and so sqlite3.config()
  // will return SQLITE_ERROR if called with the SQLITE_CONFIG_SINGLETHREAD
  // configuration option.
  // - There are no arguments to this option.
  SQLITE_CONFIG_SINGLETHREAD = 1;
  ///This option sets the threading mode to Multi-thread
  // - In other words, it disables mutexing on database connection and prepared
  // statement objects.
  // - The application is responsible for serializing access to database connections
  // and prepared statements.
  // - But other mutexes are enabled so that SQLite will be safe to use in a
  // multi-threaded environment as long as no two threads attempt to use the same
  // database connection at the same time.
  // - If SQLite is compiled with the SQLITE_THREADSAFE=0 compile-time option then
  // it is not possible to set the Multi-thread threading mode and sqlite3.config()
  // will return SQLITE_ERROR if called with the SQLITE_CONFIG_MULTITHREAD
  // configuration option.
  // - There are no arguments to this option.
  SQLITE_CONFIG_MULTITHREAD = 2;
  /// This option sets the threading mode to Serialized
  // - In other words, this option enables all mutexes including the recursive mutexes
  // on database connection and prepared statement objects.
  // - In this mode (which is the default when SQLite is compiled with SQLITE_THREADSAFE=1)
  // the SQLite library will itself serialize access to database connections and prepared
  // statements so that the application is free to use the same database connection
  // or the same prepared statement in different threads at the same time.
  // - If SQLite is compiled with the SQLITE_THREADSAFE=0 compile-time option then
  // it is not possible to set the Serialized threading mode and sqlite3.config()
  // will return SQLITE_ERROR if called with the SQLITE_CONFIG_SERIALIZED
  // configuration option.
  // - There are no arguments to this option.
  SQLITE_CONFIG_SERIALIZED = 3;
  /// The argument specifies alternative low-level memory allocation routines to
  // be used in place of the memory allocation routines built into SQLite
  // - SQLite makes its own private copy of the content of the TSqlite3MemMethods
  // structure before the sqlite3.config() call returns.
  // -  This option takes a single argument which is a pointer to an instance of
  // the TSqlite3MemMethods structure.
  SQLITE_CONFIG_MALLOC = 4;
  /// This option takes a single argument which is a pointer to an instance of
  // the TSqlite3MemMethods structure
  // - The TSqlite3MemMethods structure is filled with the currently defined memory
  // allocation routines.
  // - This option can be used to overload the default memory allocation routines
  // with a wrapper that simulations memory allocation failure or tracks memory
  // usage, for example.
  SQLITE_CONFIG_GETMALLOC = 5;
  /// This option is no longer used
  SQLITE_CONFIG_SCRATCH = 6;
  /// This option specifies a static memory buffer that SQLite can use for the database
  // page cache with the default page cache implementation
  // - This configuration should not be used if an application-define page cache
  // implementation is loaded using the SQLITE_CONFIG_PCACHE2 option.
  // - There are three arguments to this option: A pointer to 8-byte aligned memory,
  // the size of each page buffer (sz), and the number of pages (N).
  // - The sz argument should be the size of the largest database page (a power of
  // two between 512 and 32768) plus a little extra for each page header.
  // - The page header size is 20 to 40 bytes depending on the host architecture.
  // - It is harmless, apart from the wasted memory, to make sz a little too large.
  // - The first argument should point to an allocation of at least sz*N bytes of memory.
  // - SQLite will use the memory provided by the first argument to satisfy its memory
  // needs for the first N pages that it adds to cache. If additional page cache memory
  // is needed beyond what is provided by this option, then SQLite goes to sqlite3.malloc()
  // for the additional storage space.
  // - The pointer in the first argument must be aligned to an 8-byte boundary or subsequent
  // behavior of SQLite will be undefined.
  SQLITE_CONFIG_PAGECACHE = 7;
  /// This option specifies a static memory buffer that SQLite will use for all of
  // its dynamic memory allocation needs beyond those provided for by
  // SQLITE_CONFIG_SCRATCH and SQLITE_CONFIG_PAGECACHE
  // - There are three arguments: An 8-byte aligned pointer to the memory, the number
  // of bytes in the memory buffer, and the minimum allocation size.
  // - If the first pointer (the memory pointer) is NULL, then SQLite reverts
  // to using its default memory allocator (the system malloc() implementation),
  // undoing any prior invocation of SQLITE_CONFIG_MALLOC.
  // - If the memory pointer is not NULL and either SQLITE_ENABLE_MEMSYS3 or
  // SQLITE_ENABLE_MEMSYS5 are defined, then the alternative memory allocator is
  // engaged to handle all of SQLites memory allocation needs.
  // - The first pointer (the memory pointer) must be aligned to an 8-byte boundary
  // or subsequent behavior of SQLite will be undefined.
  // - The minimum allocation size is capped at 2**12. Reasonable values for the
  // minimum allocation size are 2**5 through 2**8.
  SQLITE_CONFIG_HEAP = 8;
  /// This option takes single argument of type int, interpreted as a boolean,
  // which enables or disables the collection of memory allocation statistics
  // - When memory allocation statistics are disabled, the following SQLite
  // interfaces become non-operational:
  // - sqlite3.memory_used()
  // - sqlite3.memory_highwater()
  // - sqlite3.soft_heap_limit64()
  // - sqlite3.status()
  // - Memory allocation statistics are enabled by default unless SQLite is compiled
  // with SQLITE_DEFAULT_MEMSTATUS=0 in which case memory allocation statistics
  // are disabled by default.
  SQLITE_CONFIG_MEMSTATUS = 9;
  /// This option takes a single argument which is a pointer to an instance of the
  // sqlite3_mutex_methods structure
  // - The argument specifies alternative low-level mutex routines to be used in
  // place the mutex routines built into SQLite. SQLite makes a copy of the content
  // of the sqlite3_mutex_methods structure before the call to sqlite3.config() returns.
  // - If SQLite is compiled with the SQLITE_THREADSAFE=0 compile-time option then
  // the entire mutexing subsystem is omitted from the build and hence calls to
  // sqlite3.config() with the SQLITE_CONFIG_MUTEX configuration option will
  // return SQLITE_ERROR.
  SQLITE_CONFIG_MUTEX = 10;
  /// This option takes a single argument which is a pointer to an instance of
  // the sqlite3_mutex_methods structure
  // - The sqlite3_mutex_methods structure is filled with the currently defined
  // mutex routines.
  // - This option can be used to overload the default mutex allocation routines
  // with a wrapper used to track mutex usage for performance profiling or testing,
  // for example.
  // - If SQLite is compiled with the SQLITE_THREADSAFE=0 compile-time option then
  // the entire mutexing subsystem is omitted from the build and hence calls to
  // sqlite3.config() with the SQLITE_CONFIG_GETMUTEX configuration option will
  // return SQLITE_ERROR.
  SQLITE_CONFIG_GETMUTEX = 11;
  /// This option takes two arguments that determine the default memory allocation
  // for the lookaside memory allocator on each database connection
  // - The first argument is the size of each lookaside buffer slot and the second
  // is the number of slots allocated to each database connection.
  // - This option sets the default lookaside size.
  // - The SQLITE_DBCONFIG_LOOKASIDE verb to sqlite3.db_config() can be used to
  // change the lookaside configuration on individual connections.
  SQLITE_CONFIG_LOOKASIDE = 13;
  /// These options are obsolete and should not be used by new code
  // - They are retained for backwards compatibility but are now no-ops.
  SQLITE_CONFIG_PCACHE = 14;
  /// These options are obsolete and should not be used by new code
  // - They are retained for backwards compatibility but are now no-ops.
  SQLITE_CONFIG_GETPCACHE = 15;
  /// This option takes two arguments: a pointer to a function with a call
  // signature of void(*)(void*,int,const char*), and a pointer to void
  // - If the function pointer is not NULL, it is invoked by sqlite3.log()
  // to process each logging event.
  // - If the function pointer is NULL, the sqlite3.log() interface becomes a no-op.
  // - The void pointer that is the second argument to SQLITE_CONFIG_LOG is passed
  // through as the first parameter to the application-defined logger function whenever
  // that function is invoked.
  // - The second parameter to the logger function is a copy of the first parameter
  // to the corresponding sqlite3.log() call and is intended to be a result code or
  // an extended result code.
  // - The third parameter passed to the logger is log message after formatting via
  // sqlite3.snprintf().
  // - The SQLite logging interface is not reentrant; the logger function supplied
  // by the application must not invoke any SQLite interface.
  // - In a multi-threaded application, the application-defined logger function must
  // be threadsafe.
  SQLITE_CONFIG_LOG = 16;
  /// This option takes a single argument of type int
  // - If non-zero, then URI handling is globally enabled.
  // - If the parameter is zero, then URI handling is globally disabled.
  // - If URI handling is globally enabled, all filenames passed to sqlite3_open(),
  // sqlite3.open_v2(), sqlite3.open16() or specified as part of ATTACH commands are
  // interpreted as URIs, regardless of whether or not the SQLITE_OPEN_URI flag is
  // set when the database connection is opened.
  // - If it is globally disabled, filenames are only interpreted as URIs if the
  // SQLITE_OPEN_URI flag is set when the database connection is opened.
  // - By default, URI handling is globally disabled.
  // - The default value may be changed by compiling with the SQLITE_USE_URI
  // symbol defined.
  SQLITE_CONFIG_URI = 17;
  /// This option takes a single argument which is a pointer to an
  // sqlite3_pcache_methods2 object
  // - This object specifies the interface to a custom page cache implementation.
  // - SQLite makes a copy of the object and uses it for page cache memory allocations.
  SQLITE_CONFIG_PCACHE2 = 18;
  /// This option takes a single argument which is a pointer to an
  // sqlite3_pcache_methods2 object
  // - SQLite copies of the current page cache implementation into that object.
  SQLITE_CONFIG_GETPCACHE2 = 19;
  /// This option takes a single integer argument which is interpreted
  // as a boolean in order to enable or disable the use of covering indices
  // for full table scans in the query optimizer.
  // - The default setting is determined by the SQLITE_ALLOW_COVERING_INDEX_SCAN
  // compile-time option, or is "on" if that compile-time option is omitted.
  // - The ability to disable the use of covering indices for full table scans
  // is because some incorrectly coded legacy applications might malfunction
  // when the optimization is enabled. Providing the ability to disable the
  // optimization allows the older, buggy application code to work without
  // change even with newer versions of SQLite.
  SQLITE_CONFIG_COVERING_INDEX_SCAN = 20;
  /// This option is used to configure the SQLite global error log.
  // - The SQLITE_CONFIG_LOG option takes two arguments: a pointer to a function
  // with a call signature of void(*)(void*,int,const char*), and a pointer
  // to void.
  // - If the function pointer is not NULL, it is invoked by sqlite3_log()
  // to process each logging event. If the function pointer is NULL, the
  // sqlite3_log() interface becomes a no-op.
  // - The void pointer that is the second argument to SQLITE_CONFIG_LOG is
  // passed through as the first parameter to the application-defined logger
  // function whenever that function is invoked. The second parameter to the
  // logger function is a copy of the first parameter to the corresponding
  // sqlite3_log() call and is intended to be a result code or an extended
  // result code. The third parameter passed to the logger is log message
  // after formatting via sqlite3_snprintf().
  // - The SQLite logging interface is not reentrant; the logger function
  // supplied by the application must not invoke any SQLite interface.
  // - In a multi-threaded application, the application-defined logger function
  // must be threadsafe.
  SQLITE_CONFIG_SQLLOG = 21;
  /// This option takes two 64-bit integer (sqlite3_int64) values that are the
  // default mmap size limit (the default setting for PRAGMA mmap_size) and
  // the maximum allowed mmap size limit.
  // - The default setting can be overridden by each database connection using
  // either the PRAGMA mmap_size command, or by using the SQLITE_FCNTL_MMAP_SIZE
  // file control. The maximum allowed mmap size will be silently truncated if
  // necessary so that it does not exceed the compile-time maximum mmap size
  // set by the SQLITE_MAX_MMAP_SIZE compile-time option.
  // - If either argument to this option is negative, then that argument is
  // changed to its compile-time default.
  SQLITE_CONFIG_MMAP_SIZE = 22;
  /// Available if SQLite is compiled for Windows with the SQLITE_WIN32_MALLOC.
  SQLITE_CONFIG_WIN32_HEAPSIZE = 23;
  /// This option takes a single parameter which is a pointer to an integer
  // and writes into that integer the number of extra bytes per page required
  // for each page in SQLITE_CONFIG_PAGECACHE.
  // - The amount of extra space required can change depending on the compiler,
  // target platform, and SQLite version.
  SQLITE_CONFIG_PCACHE_HDRSZ = 24;
  /// This option takes a single parameter which is an unsigned integer and
  // sets the "Minimum PMA Size" for the multithreaded sorter to that integer.
  // - The default minimum PMA Size is set by the SQLITE_SORTER_PMASZ
  // compile-time option. New threads are launched to help with sort operations
  // when multithreaded sorting is enabled (using the PRAGMA threads command)
  // and the amount of content to be sorted exceeds the page size times the
  // minimum of the PRAGMA cache_size setting and this value.
  SQLITE_CONFIG_PMASZ = 25;
  /// This option takes a single parameter which becomes the statement journal
  // spill-to-disk threshold.
  // - Statement journals are held in memory until their size (in bytes) exceeds
  // this threshold, at which point they are written to disk. Or if the
  // threshold is -1, statement journals are always held exclusively in memory.
  // - Since many statement journals never become large, setting the spill
  // threshold to a value such as 64KiB can greatly reduce the amount of
  // I/O required to support statement rollback.
  // - The default value for this setting is controlled by the
  // SQLITE_STMTJRNL_SPILL compile-time option.
  SQLITE_CONFIG_STMTJRNL_SPILL = 26;
  /// This option takes single argument of type int, interpreted as a boolean,
  // which if true provides a hint to SQLite that it should avoid large memory
  // allocations if possible.
  // - SQLite will run faster if it is free to make large memory allocations,
  // but some application might prefer to run slower in exchange for guarantees
  // about memory fragmentation that are possible if large allocations are avoided.
  // - This hint is normally off.
  SQLITE_CONFIG_SMALL_MALLOC = 27;
  /// This option accepts a single parameter of type (int) - the new value of
  // the sorter-reference size threshold.
  // - Usually, when SQLite uses an external sort to order records according to
  // an ORDER BY clause, all fields required by the caller are present in the
  // sorted records. However, if SQLite determines based on the declared type
  // of a table column that its values are likely to be very large - larger than
  // the configured sorter-reference size threshold - then a reference is stored
  // in each sorted record and the required column values loaded from the
  // database as records are returned in sorted order.
  // - The default value for this option is to never use this optimization.
  // - Specifying a negative value for this option restores the default behaviour.
  // - This option is only available if SQLite is compiled with the
  // SQLITE_ENABLE_SORTER_REFERENCES compile-time option.
  SQLITE_CONFIG_SORTERREF_SIZE = 28;
  /// This option accepts a single parameter sqlite3_int64 parameter which is
  // the default maximum size for an in-memory database created
  // using sqlite3_deserialize().
  // - This default maximum size can be adjusted up or down for individual
  // databases using the SQLITE_FCNTL_SIZE_LIMIT file-control.
  // - If this configuration setting is never used, then the default maximum is
  // determined by the SQLITE_MEMDB_DEFAULT_MAXSIZE compile-time option.
  // - If that compile-time option is not set, then the default maximum is
  // 1073741824.
  SQLITE_CONFIG_MEMDB_MAXSIZE = 29;

  /// This option takes three additional arguments that determine the lookaside
  // memory allocator configuration for the database connection.
  // - The first argument (the third parameter to sqlite3_db_config() is a
  // pointer to a memory buffer to use for lookaside memory.
  // - The first argument after the SQLITE_DBCONFIG_LOOKASIDE verb may be NULL
  // in which case SQLite will allocate the lookaside buffer itself using
  // sqlite3_malloc().
  // - The second argument is the size of each lookaside buffer slot.
  // - The third argument is the number of slots.
  // - The size of the buffer in the first argument must be greater than or
  // equal to the product of the second and third arguments.
  // - The buffer must be aligned to an 8-byte boundary.
  // - If the second argument to SQLITE_DBCONFIG_LOOKASIDE is not a multiple
  // of 8, it is internally rounded down to the next smaller multiple of 8.
  // - The lookaside memory configuration for a database connection can only be
  // changed when that connection is not currently using lookaside memory,
  // or in other words when the "current value" returned by
  // sqlite3_db_status(D,SQLITE_CONFIG_LOOKASIDE,...) is zero. Any attempt to
  // change the lookaside memory configuration when lookaside memory is in use
  // leaves the configuration unchanged and returns SQLITE_BUSY.
  SQLITE_DBCONFIG_LOOKASIDE = 1001;
  /// This option is used to enable or disable the enforcement of foreign key
  // constraints.
  // - There should be two additional arguments.
  // - The first argument is an integer which is 0 to disable FK enforcement,
  // positive to enable FK enforcement or negative to leave FK enforcement unchanged.
  // - The second parameter is a pointer to an integer into which is written
  // 0 or 1 to indicate whether FK enforcement is off or on following this call.
  // - The second parameter may be a NULL pointer, in which case the FK
  // enforcement setting is not reported back.
  SQLITE_DBCONFIG_ENABLE_FKEY = 1002;
  /// This option is used to enable or disable triggers.
  // - There should be two additional arguments.
  // - The first argument is an integer which is 0 to disable triggers,
  // positive to enable triggers or negative to leave the setting unchanged.
  // - The second parameter is a pointer to an integer into which is written
  // 0 or 1 to indicate whether triggers are disabled or enabled following
  // this call.
  // - The second parameter may be a NULL pointer, in which case the trigger
  // setting is not reported back.
  // - Originally this option disabled all triggers. However, since SQLite
  // version 3.35.0, TEMP triggers are still allowed even if this option is off.
  // So, in other words, this option now only disables triggers in the main
  // database schema or in the schemas of ATTACH-ed databases.
  SQLITE_DBCONFIG_ENABLE_TRIGGER = 1003;
  /// This option is used to enable or disable the fts3_tokenizer() function
  // which is part of the FTS3 full-text search engine extension.
  // - There should be two additional arguments.
  // - The first argument is an integer which is 0 to disable fts3_tokenizer()
  // or positive to enable fts3_tokenizer() or negative to leave the setting
  // unchanged.
  // - The second parameter is a pointer to an integer into which is
  // written 0 or 1 to indicate whether fts3_tokenizer is disabled or enabled
  // following this call. The second parameter may be a NULL pointer, in which
  // case the new setting is not reported back.
  SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER = 1004;
  /// This option is used to enable or disable the sqlite3_load_extension()
  // interface independently of the load_extension() SQL function.
  // - The sqlite3_enable_load_extension() API enables or disables both the
  // C-API sqlite3_load_extension() and the SQL function load_extension().
  // - There should be two additional arguments.
  // - When the first argument to this interface is 1, then only the C-API
  // is enabled and the SQL function remains disabled.
  // - If the first argument to this interface is 0, then both the C-API
  // and the SQL function are disabled.
  // - If the first argument is -1, then no changes are made to state of either
  // the C-API or the SQL function.
  // - The second parameter is a pointer to an integer into which is written
  // 0 or 1 to indicate whether sqlite3_load_extension() interface is disabled
  // or enabled following this call.
  // - The second parameter may be a NULL pointer, in which case the new
  // setting is not reported back.
  SQLITE_DBCONFIG_ENABLE_LOAD_EXTENSION = 1005;


  /// Calls of the form sqlite3.vtab_config(db,SQLITE_VTAB_CONSTRAINT_SUPPORT,X)
  // are supported, where X is an integer.
  // - If X is zero, then the virtual table whose xCreate or xConnect method invoked
  // sqlite3.vtab_config() does not support constraints.
  // - In this configuration (which is the default) if a call to the xUpdate method
  // returns SQLITE_CONSTRAINT, then the entire statement is rolled back as if OR ABORT
  // had been specified as part of the users SQL statement, regardless of the actual
  // ON CONFLICT mode specified.
  // - If X is non-zero, then the virtual table implementation guarantees that if
  // xUpdate returns SQLITE_CONSTRAINT, it will do so before any modifications to
  // internal or persistent data structures have been made.
  // - If the ON CONFLICT mode is ABORT, FAIL, IGNORE or ROLLBACK, SQLite is able
  // to roll back a statement or database transaction, and abandon or continue processing
  // the current SQL statement as appropriate.
  // - If the ON CONFLICT mode is REPLACE and the xUpdate method returns SQLITE_CONSTRAINT,
  // SQLite handles this as if the ON CONFLICT mode had been ABORT.
  // - Virtual table implementations that are required to handle OR REPLACE must do so within
  // the xUpdate method.
  // - If a call to the sqlite3_vtab_on_conflict() function indicates that the current
  // ON CONFLICT policy is REPLACE, the virtual table implementation should silently
  // replace the appropriate rows within the xUpdate callback and return SQLITE_OK.
  // Or, if this is not possible, it may return SQLITE_CONSTRAINT, in which case SQLite falls
  // back to OR ABORT constraint handling.
  SQLITE_VTAB_CONSTRAINT_SUPPORT = 1;
  /// Calls of the form sqlite3_vtab_config(db,SQLITE_VTAB_INNOCUOUS) from within the
  // the xConnect or xCreate methods of a virtual table implmentation identify that
  // virtual table as being safe to use from within triggers and views.
  // - Conceptually, the SQLITE_VTAB_INNOCUOUS tag means that the virtual table can do
  // no serious harm even if it is controlled by a malicious hacker.
  // - Developers should avoid setting the SQLITE_VTAB_INNOCUOUS flag unless absolutely necessary.
  SQLITE_VTAB_INNOCUOUS = 2;
  /// Calls of the form sqlite3.vtab_config(db,SQLITE_VTAB_DIRECTONLY) from within the
  // the xConnect or xCreate methods of a virtual table implmentation prohibits that
  // virtual table from being used from within triggers and views.
  SQLITE_VTAB_DIRECTONLY = 3;

  /// Current amount of memory checked out using sqlite3.malloc(), either directly
  // or indirectly.
  // - The figure includes calls made to sqlite3.malloc() by the application and
  // internal memory usage by the SQLite library.
  // - Auxiliary page-cache memory controlled by SQLITE_CONFIG_PAGECACHE is not
  // included in this parameter.
  // - The amount returned is the sum of the allocation sizes as reported by the
  // xSize method in TSqlite3MemMethods.
  // - Needs SQLITE_CONFIG_MEMSTATUS to be active by SQLITE_DEFAULT_MEMSTATUS at
  // compile time or using sqlite3.config() at runtime and before library intialization.
  SQLITE_STATUS_MEMORY_USED = 0;
  /// Number of pages used out of the pagecache memory allocator that was configured
  // using SQLITE_CONFIG_PAGECACHE.
  // - The value returned is in pages, not in bytes.
  SQLITE_STATUS_PAGECACHE_USED = 1;
  /// Number of bytes of page cache allocation which could not be satisfied by the
  // SQLITE_CONFIG_PAGECACHE buffer and where forced to overflow to sqlite3.malloc().
  // - The returned value includes allocations that overflowed because they where too
  // large (they were larger than the "sz" parameter to SQLITE_CONFIG_PAGECACHE)
  // and allocations that overflowed because no space was left in the page cache.
  SQLITE_STATUS_PAGECACHE_OVERFLOW = 2;
  /// Largest memory allocation request handed to sqlite3.malloc() or sqlite3.realloc()
  // (or their internal equivalents).
  // - Only the value returned in the pHighwater is of interest.
  // - The value written into the pCurrent is undefined.
  // - Needs SQLITE_CONFIG_MEMSTATUS to be active by SQLITE_DEFAULT_MEMSTATUS at
  // compile time or using sqlite3.config() at runtime and before library intialization.
  SQLITE_STATUS_MALLOC_SIZE = 5;
  /// The pHighwater parameter records the deepest parser stack.
  // - The pCurrent value is undefined.
  // - The pHighwater value is only meaningful if SQLite is compiled with YYTRACKMAXSTACKDEPTH.
  SQLITE_STATUS_PARSER_STACK = 6;
  /// Largest memory allocation request handed to the pagecache memory allocator.
  // - Only the value returned in the pHighwater is of interest.
  // - The value written into the pCurrent is undefined.
  SQLITE_STATUS_PAGECACHE_SIZE = 7;
  /// Number of separate memory allocations currently checked out.
  // - Needs SQLITE_CONFIG_MEMSTATUS to be active by SQLITE_DEFAULT_MEMSTATUS at
  // compile time or using sqlite3.config() at runtime and before library intialization.
  SQLITE_STATUS_MALLOC_COUNT = 9;


  /// Returns the number of lookaside memory slots currently checked out.
  SQLITE_DBSTATUS_LOOKASIDE_USED = 0;
  /// Returns the approximate number of bytes of heap memory used by all pager
  // caches associated with the database connection.
  // - The pHighwater value is always zero
  SQLITE_DBSTATUS_CACHE_USED = 1;
  /// This parameter returns the approximate number of bytes of heap memory used
  // to store the schema for all databases associated with the connection - main,
  // temp, and any ATTACH-ed databases.
  // - The full amount of memory used by the schemas is reported, even if the schema
  // memory is shared with other database connections due to shared cache mode being enabled.
  // - The pHighwater value is always zero
  SQLITE_DBSTATUS_SCHEMA_USED = 2;
  /// Returns the approximate number of bytes of heap and lookaside memory used by
  // all prepared statements associated with the database connection.
  // - The pHighwater value is always zero
  SQLITE_DBSTATUS_STMT_USED = 3;
  /// Returns the number of malloc attempts that were satisfied using lookaside memory.
  // - The pCurrent value is always zero
  SQLITE_DBSTATUS_LOOKASIDE_HIT = 4;
  /// Returns the number malloc attempts that might have been satisfied using
  // lookaside memory but failed due to the amount of memory requested being larger
  // than the lookaside slot size.
  // - The pCurrent value is always zero
  SQLITE_DBSTATUS_LOOKASIDE_MISS_SIZE = 5;
  /// Returns the number malloc attempts that might have been satisfied using
  // lookaside memory but failed due to all lookaside memory already being in use.
  // - The pCurrent value is always zero
  SQLITE_DBSTATUS_LOOKASIDE_MISS_FULL = 6;
  /// Returns the number of pager cache hits that have occurred.
  // - The pHighwater value is always zero
  SQLITE_DBSTATUS_CACHE_HIT = 7;
  /// Returns the number of pager cache misses that have occurred.
  // - The pHighwater value is always zero
  SQLITE_DBSTATUS_CACHE_MISS = 8;
  /// Returns the number of dirty cache entries that have been written to disk.
  // Specifically, the number of pages written to the wal file in wal mode databases,
  // or the number of pages written to the database file in rollback mode databases.
  // - Any pages written as part of transaction rollback or database recovery operations
  // are not included.
  // - If an IO or other error occurs while writing a page to disk, the effect on
  // subsequent SQLITE_DBSTATUS_CACHE_WRITE requests is undefined.
  // - The pHighwater value is always zero
  SQLITE_DBSTATUS_CACHE_WRITE = 9;
  /// Returns zero for the current value if and only if all foreign key constraints (deferred or immediate) have been resolved.
  // - The pHighwater value is always zero
  SQLITE_DBSTATUS_DEFERRED_FKS = 10;
  /// This parameter is similar to SQLITE_DBSTATUS_CACHE_USED, except that if a pager cache
  // is shared between two or more connections the bytes of heap memory used by that
  // pager cache is divided evenly between the attached connections.
  // - In other words, if none of the pager caches associated with the database connection
  // are shared, this request returns the same value as SQLITE_DBSTATUS_CACHE_USED.
  // - Or, if one or more or the pager caches are shared, the value returned by this
  // call will be smaller than that returned by SQLITE_DBSTATUS_CACHE_USED.
  // - The pHighwater value is always zero
  SQLITE_DBSTATUS_CACHE_USED_SHARED = 11;
  /// Returns the number of dirty cache entries that have been written to disk in
  // the middle of a transaction due to the page cache overflowing.
  // - Transactions are more efficient if they are written to disk all at once.
  // - When pages spill mid-transaction, that introduces additional overhead.
  // - This parameter can be used help identify inefficiencies that can be resolved
  // by increasing the cache size.
  SQLITE_DBSTATUS_CACHE_SPILL = 12;
  /// Largest defined DBSTATUS
  SQLITE_DBSTATUS_MAX = 12;


  /// Number of times that SQLite has stepped forward in a table as part of a full
  // table scan.
  // - Large numbers for this counter may indicate opportunities for performance
  // improvement through careful use of indices.
  SQLITE_STMTSTATUS_FULLSCAN_STEP = 1;
  /// Number of sort operations that have occurred.
  // - A non-zero value in this counter may indicate an opportunity to improvement
  // performance through careful use of indices.
  SQLITE_STMTSTATUS_SORT = 2;
  /// Number of rows inserted into transient indices that were created automatically
  // in order to help joins run faster.
  // - A non-zero value in this counter may indicate an opportunity to improvement
  // performance by adding permanent indices that do not need to be reinitialized
  // each time the statement is run.
  SQLITE_STMTSTATUS_AUTOINDEX = 3;
  /// Number of virtual machine operations executed by the prepared statement if
  // that number is less than or equal to 2147483647.
  // - The number of virtual machine operations can be used as a proxy for the total
  // work done by the prepared statement.
  // - If the number of virtual machine operations exceeds 2147483647 then the value
  // returned by this statement status code is undefined.
  SQLITE_STMTSTATUS_VM_STEP = 4;
  /// Number of times that the prepare statement has been automatically regenerated
  // due to schema changes or changes to bound parameters that might affect the query plan.
  SQLITE_STMTSTATUS_REPREPARE = 5;
  /// Number of times that the prepared statement has been run.
  // - A single "run" for the purposes of this counter is one or more calls to sqlite3.step()
  // followed by a call to sqlite3.reset().
  // - The counter is incremented on the first sqlite3.step() call of each cycle.
  SQLITE_STMTSTATUS_RUN = 6;
  /// Approximate number of bytes of heap memory used to store the prepared statement.
  // This value is not actually a counter, and so the resetFlg parameter to
  // sqlite3.stmt_status() is ignored when the opcode is SQLITE_STMTSTATUS_MEMUSED.
  SQLITE_STMTSTATUS_MEMUSED = 99;


  /// The Int64 variable pointed to by the pOut parameter will be set to the total
  // number of times that the X-th loop has run.
  SQLITE_SCANSTAT_NLOOP = 0;
  /// The Int64 variable pointed to by the pOut parameter will be set to the total number
  // of rows examined by all iterations of the X-th loop.
  SQLITE_SCANSTAT_NVISIT = 1;
  /// The double variable pointed to by the pOut parameter will be set to the query
  // planner's estimate for the average number of rows output from each iteration of
  // the X-th loop.
  // - If the query planner's estimates was accurate, then this value will approximate
  // the quotient NVISIT/NLOOP and the product of this value for all prior loops with
  // the same SELECTID will be the NLOOP value for the current loop.
  SQLITE_SCANSTAT_EST = 2;
  /// The PUtf8Char variable pointed to by the pOut parameter will be set to a
  // zero-terminated UTF-8 string containing the name of the index or table used
  // for the X-th loop.
  SQLITE_SCANSTAT_NAME = 3;
  /// The PUtf8Char variable pointed to by the V parameter will be set to a
  // zero-terminated UTF-8 string containing the EXPLAIN QUERY PLAN description
  // for the X-th loop.
  SQLITE_SCANSTAT_EXPLAIN = 4;
  /// The integer variable pointed to by the V parameter will be set to the "select-id"
  // for the X-th loop. The select-id identifies which query or subquery the loop is part of.
  // - The main query has a select-id of zero.
  // - The select-id is the same value as is output in the first column of an
  // EXPLAIN QUERY PLAN query.
  SQLITE_SCANSTAT_SELECTID = 5;

  /// A hint to the query planner that the prepared statement will be retained for
  // a long time and probably reused many times.
  // - Without this flag, sqlite3.prepare_v3() assumes that the prepared statement
  // will be used just once or at most a few times and then destroyed using
  // sqlite3.finalize() relatively soon.
  // - The current implementation acts on this hint by avoiding the use of lookaside
  // memory so as not to deplete the limited store of lookaside memory.
  // - Future versions of SQLite may act on this hint differently.
  SQLITE_PREPARE_PERSISTENT = 1;
  /// Causes the SQL compiler to return an error (error code SQLITE_ERROR) if
  // the statement uses any virtual tables.
  SQLITE_PREPARE_NO_VTAB = 4;

  /// The SQLITE_TXN_NONE state means that no transaction is currently pending.
  SQLITE_TXN_NONE = 1;
  /// The SQLITE_TXN_READ state means that the database is currently in a read transaction.
  // - Content has been read from the database file but nothing in the database file has changed.
  // - The transaction state will advanced to SQLITE_TXN_WRITE if any changes occur and there
  // are no other conflicting concurrent write transactions.
  // - The transaction state will revert to SQLITE_TXN_NONE following a ROLLBACK or COMMIT.
  SQLITE_TXN_READ = 2;
  /// The SQLITE_TXN_WRITE state means that the database is currently in a write transaction.
  // Content has been written to the database file but has not yet committed.
  // The transaction state will change to to SQLITE_TXN_NONE at the next ROLLBACK or COMMIT.
  SQLITE_TXN_WRITE = 3;


  /// Do as much as possible w/o blocking
  // - Checkpoint as many frames as possible without waiting for any database readers or writers
  // to finish, then sync the database file if all frames in the log were checkpointed.
  // - The busy-handler callback is never invoked in the SQLITE_CHECKPOINT_PASSIVE mode.
  // On the other hand, passive mode might leave the checkpoint unfinished if there are
  // concurrent readers or writers.
  SQLITE_CHECKPOINT_PASSIVE = 0;
  /// Wait for writers, then checkpoint
  // - This mode blocks (it invokes the busy-handler callback) until there is no database writer
  // and all readers are reading from the most recent database snapshot.
  // - It then checkpoints all frames in the log file and syncs the database file.
  // - This mode blocks new database writers while it is pending, but new database readers are
  // allowed to continue unimpeded.
  SQLITE_CHECKPOINT_FULL = 1;
  /// Like FULL but wait for for readers
  // - This mode works the same way as SQLITE_CHECKPOINT_FULL with the addition that after
  // checkpointing the log file it blocks (calls the busy-handler callback) until all readers
  // are reading from the database file only.
  // - This ensures that the next writer will restart the log file from the beginning.
  // - Like SQLITE_CHECKPOINT_FULL, this mode blocks new database writer attempts while it is
  // pending, but does not impede readers.
  SQLITE_CHECKPOINT_RESTART = 2;
  /// Like RESTART but also truncate WAL
  // - This mode works the same way as SQLITE_CHECKPOINT_RESTART with the addition that it also
  // truncates the log file to zero bytes just prior to a successful return.
  SQLITE_CHECKPOINT_TRUNCATE = 3;

type
  /// type for a custom destructor for the text or BLOB content
  // - set to @sqlite3InternalFree if a Value must be released via Freemem()
  // - set to @sqlite3InternalFreeObject if a Value must be released via
  // TObject(p).Free
  TSqlDestroyPtr = procedure(p: pointer); cdecl;

  /// SQLite3 collation (i.e. sort and comparison) function prototype
  // - this function MUST use s1Len and s2Len parameters during the comparison:
  // s1 and s2 are not zero-terminated
  // - used by sqlite3.create_collation low-level function
  TSqlCollateFunc = function(CollateParam: pointer; s1Len: integer; s1: pointer;
    s2Len: integer; s2: pointer) : integer; cdecl;

  /// SQLite3 user function or aggregate callback prototype
  // - argc is the number of supplied parameters, which are available in argv[]
  // (you can call ErrorWrongNumberOfArgs(Context) in case of unexpected number)
  // - use sqlite3.value_*(argv[*]) functions to retrieve a parameter value
  // - then set the result using sqlite3.result_*(Context,*) functions
  TSqlFunctionFunc = procedure(Context: TSqlite3FunctionContext;
    argc: integer; var argv: TSqlite3ValueArray); cdecl;

  /// SQLite3 user final aggregate callback prototype
  TSqlFunctionFinal = procedure(Context: TSqlite3FunctionContext); cdecl;

  /// SQLite3 callback prototype to handle SQLITE_BUSY errors
  // - The first argument to the busy handler is a copy of the user pointer which
  // is the third argument to sqlite3.busy_handler().
  // - The second argument to the busy handler callback is the number of times
  // that the busy handler has been invoked for this locking event.
  // - If the busy callback returns 0, then no additional attempts are made to
  // access the database and SQLITE_BUSY or SQLITE_IOERR_BLOCKED is returned.
  // - If the callback returns non-zero, then another attempt is made to open
  // the database for reading and the cycle repeats.
  TSqlBusyHandler = function(user: pointer; count: integer): integer; cdecl;

  PFTSMatchInfo = ^TFTSMatchInfo;
  /// map the matchinfo function returned BLOB value
  // - i.e. the default 'pcx' layout, for both FTS3 and FTS4
  // - see http://www.sqlite.org/fts3.html#matchinfo
  // - used for the FTS3/FTS4 ranking of results by TRest.FTSMatch method
  // and the internal RANK() function as proposed in
  // http://www.sqlite.org/fts3.html#appendix_a
  TFTSMatchInfo = packed record
    nPhrase: integer;
    nCol: integer;
    hits: array[1..9] of record
      this_row: integer;
      all_rows: integer;
      docs_with_hits: integer;
    end;
  end;

  PSqlite3Module = ^TSqlite3Module;
  PSqlite3VTab = ^TSqlite3VTab;
  PSqlite3VTabCursor = ^TSqlite3VTabCursor;

  /// records WHERE clause constraints of the form "column OP expr"
  // - Where "column" is a column in the virtual table, OP is an operator like
  // "=" or "<", and EXPR is an arbitrary expression
  // - So, for example, if the WHERE clause contained a term like this:
  // $ a = 5
  // Then one of the constraints would be on the "a" column with operator "="
  // and an expression of "5"
  // - For example, if the WHERE clause contained something like this:
  // $  x BETWEEN 10 AND 100 AND 999>y
  // The query optimizer might translate this into three separate constraints:
  // ! x >= 10
  // ! x <= 100
  // ! y < 999
  TSqlite3IndexConstraint = record
    /// Column on left-hand side of constraint
    // - The first column of the virtual table is column 0
    // - The ROWID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index.
    iColumn: integer;
    /// Constraint operator
    // - OP is =, <, <=, >, or >= using one of the SQLITE_INDEX_CONSTRAINT_* values
    op: byte;
    /// True if this constraint is usable
    // - The aConstraint[] array contains information about all constraints that
    // apply to the virtual table. But some of the constraints might not be usable
    // because of the way tables are ordered in a join. The xBestIndex method
    // must therefore only consider constraints that have a usable flag which is
    // true, and just ignore contraints with usable set to false
    usable: bytebool;
    /// Used internally - xBestIndex() should ignore this field
    iTermOffset: integer;
  end;

  PSqlite3IndexConstraintArray = ^TSqlite3IndexConstraintArray;
  TSqlite3IndexConstraintArray = array[0 ..
    MaxInt div SizeOf(TSqlite3IndexConstraint) - 1] of TSqlite3IndexConstraint;

  /// ORDER BY clause, one item per column
  TSqlite3IndexOrderBy = record
    /// Column number
    // - The first column of the virtual table is column 0
    // - The ROWID of the virtual table is column -1
    // - Hidden columns are counted when determining the column index.
    iColumn: integer;
    /// True for DESC.  False for ASC.
    desc: bytebool;
  end;
  PSqlite3IndexOrderByArray = ^TSqlite3IndexOrderByArray;
  TSqlite3IndexOrderByArray = array[0 ..
    MaxInt div SizeOf(TSqlite3IndexOrderBy) - 1] of TSqlite3IndexOrderBy;

  /// define what information is to be passed to xFilter() for a given WHERE
  // clause constraint of the form "column OP expr"
  TSqlite3IndexConstraintUsage = record
    /// If argvIndex>0 then the right-hand side of the corresponding
    // aConstraint[] is evaluated and becomes the argvIndex-th entry in argv
    // - Exactly one entry should be set to 1, another to 2, another to 3, and
    // so forth up to as many or as few as the xBestIndex() method wants.
    // - The EXPR of the corresponding constraints will then be passed in as
    // the argv[] parameters to xFilter()
    // - For example, if the aConstraint[3].argvIndex is set to 1, then when
    // xFilter() is called, the argv[0] passed to xFilter will have the EXPR
    // value of the aConstraint[3] constraint.
    argvIndex: integer;
    /// If omit is true, then the constraint is assumed to be fully handled
    // by the virtual table and is not checked again by SQLite
    // - By default, the SQLite core double checks all constraints on each
    // row of the virtual table that it receives. If such a check is redundant,
    // xBestFilter() method can suppress that double-check by setting this field
    omit: bytebool;
  end;
  PSqlite3IndexConstraintUsageArray = ^TSqlite3IndexConstraintUsageArray;
  TSqlite3IndexConstraintUsageArray = array[0 ..
    MaxInt div SizeOf(TSqlite3IndexConstraintUsage) - 1] of TSqlite3IndexConstraintUsage;

  /// Structure used as part of the virtual table interface to pass information
  // into and receive the reply from the xBestIndex() method of a virtual table module
  // - Outputs fields will be passed as parameter to the xFilter() method, and
  // will be initialized to zero by SQLite
  // - For instance, xBestIndex() method fills the idxNum and idxStr fields with
  // information that communicates an indexing strategy to the xFilter method.
  // The information in idxNum and idxStr is arbitrary as far as the SQLite core
  // is concerned. The SQLite core just copies the information through to the
  // xFilter() method. Any desired meaning can be assigned to idxNum and idxStr
  // as long as xBestIndex() and xFilter() agree on what that meaning is.
  // Use the SetInfo() method of this object in order to make a temporary copy
  // of any needed data.
  TSqlite3IndexInfo = record
    /// input: Number of entries in aConstraint array
    nConstraint: integer;
    /// input: List of WHERE clause constraints of the form "column OP expr"
    aConstraint: PSqlite3IndexConstraintArray;
    /// input: Number of terms in the aOrderBy array
    nOrderBy: integer;
    /// input: List of ORDER BY clause, one per column
    aOrderBy: PSqlite3IndexOrderByArray;
    /// output: filled by xBestIndex() method with information about what
    // parameters to pass to xFilter() method
    // - has the same number of items than the aConstraint[] array
    // - should set the aConstraintUsage[].argvIndex to have the corresponding
    // argument in xFilter() argc/argv[] expression list
    aConstraintUsage: PSqlite3IndexConstraintUsageArray;
    /// output: Number used to identify the index
    idxNum: integer;
    /// output: String, possibly obtained from sqlite3.malloc()
    // - may contain any variable-length data or class/record content, as
    // necessary
    idxStr: PUtf8Char;
    /// output: Free idxStr using sqlite3.free() if true (=1)
    needToFreeIdxStr: integer;
    /// output: True (=1) if output is already ordered
    // - i.e. if the virtual table will output rows in the order specified
    // by the ORDER BY clause
    // - if False (=0), will indicate to the SQLite core that it will need to
    // do a separate sorting pass over the data after it comes out
    // of the virtual table
    orderByConsumed: integer;
    /// output: Estimated cost of using this index
    // - Should be set to the estimated number of disk access operations
    // required to execute this query against the virtual table
    // - The SQLite core will often call xBestIndex() multiple times with
    // different constraints, obtain multiple cost estimates, then choose the
    // query plan that gives the lowest estimate
    estimatedCost: Double;
    /// output: Estimated number of rows returned  (since 3.8.2)
    // - may be set to an estimate of the number of rows returned by the
    // proposed query plan. If this value is not explicitly set, the default
    // estimate of 25 rows is used
    estimatedRows: Int64;
    /// output: Mask of SQLITE_INDEX_SCAN_* flags  (since 3.9.0)
    // - may be set to SQLITE_INDEX_SCAN_UNIQUE to indicate that the virtual
    // table will return only zero or one rows given the input constraints.
    // Additional bits of the idxFlags field might be understood in later
    // versions of SQLite
    idxFlags: integer;
    /// input: Mask of columns used by statement   (since 3.10.0)
    // - indicates which fields of the virtual table are actually used by the
    // statement being prepared. If the lowest bit of colUsed is set, that means
    // that the first column is used. The second lowest bit corresponds to the
    // second column. And so forth. If the most significant bit of colUsed is
    // set, that means that one or more columns other than the first 63 columns
    // are used.
    // - If column usage information is needed by the xFilter method, then the
    // required bits must be encoded into either the idxNum or idxStr output fields
    colUsed: UInt64;
  end;

  /// Virtual Table Instance Object
  // - Every virtual table module implementation uses a subclass of this object
  // to describe a particular instance of the virtual table.
  // - Each subclass will be tailored to the specific needs of the module
  // implementation. The purpose of this superclass is to define certain fields
  // that are common to all module implementations. This structure therefore
  // contains a pInstance field, which will be used to store a class instance
  // handling the virtual table as a pure class: the TOrmVirtualTableModule
  // class will use it internally
  TSqlite3VTab = record
    /// The module for this virtual table
    pModule: PSqlite3Module;
    /// no longer used
    nRef: integer;
    /// Error message from sqlite3.mprintf()
    // - Virtual tables methods can set an error message by assigning a string
    // obtained from sqlite3.mprintf() to zErrMsg.
    // - The method should take care that any prior string is freed by a call
    // to sqlite3.free() prior to assigning a new string to zErrMsg.
    // - After the error message is delivered up to the client application,
    // the string will be automatically freed by sqlite3.free() and the zErrMsg
    // field will be zeroed.
    zErrMsg: PUtf8Char;
    /// this will be used to store a class instance handling the Virtual Table
    pInstance: TObject;
  end;

  /// Virtual Table Cursor Object
  // - Every virtual table module implementation uses a subclass of the following
  // structure to describe cursors that point into the virtual table and are
  // used to loop through the virtual table.
  // - Cursors are created using the xOpen method of the module and are destroyed
  // by the xClose method. Cursors are used by the xFilter, xNext, xEof, xColumn,
  // and xRowid methods of the module.
  // - Each module implementation will define the content of a cursor structure
  // to suit its own needs.
  // - This superclass exists in order to define fields of the cursor that are
  // common to all implementations
  // - This structure therefore contains a pInstance field, which will be used
  // to store a class instance handling the virtual table as a TObject: the
  // TOrmVirtualTableModule class will use it internally
  TSqlite3VTabCursor = record
    /// Virtual table of this cursor
    pVtab: PSqlite3VTab;
    /// this will be used to store a class instance handling the cursor
    pInstance: TObject;
  end;

  /// defines a module object used to implement a virtual table.
  // - Think of a module as a class from which one can construct multiple virtual
  // tables having similar properties. For example, one might have a module that
  // provides read-only access to comma-separated-value (CSV) files on disk.
  // That one module can then be used to create several virtual tables where each
  // virtual table refers to a different CSV file.
  // - The module structure contains methods that are invoked by SQLite to perform
  // various actions on the virtual table such as creating new instances of a
  // virtual table or destroying old ones, reading and writing data, searching
  // for and deleting, updating, or inserting rows.
  TSqlite3Module = record
    /// defines the particular edition of the module table structure
    // - Currently, handled iVersion is 2, but in future releases of SQLite the
    // module structure definition might be extended with additional methods and
    // in that case the iVersion value will be increased
    iVersion: integer;
    /// called to create a new instance of a virtual table in response to a
    // CREATE VIRTUAL TABLE statement
    // - The job of this method is to construct the new virtual table object (an
    // PSqlite3VTab object) and return a pointer to it in ppVTab
    // - The DB parameter is a pointer to the SQLite database connection that is
    // executing the CREATE VIRTUAL TABLE statement
    // - The pAux argument is the copy of the client data pointer that was the
    // fourth argument to the sqlite3.create_module_v2() call that registered
    // the virtual table module
    // - The argv parameter is an array of argc pointers to null terminated strings
    // - The first string, argv[0], is the name of the module being invoked. The
    // module name is the name provided as the second argument to sqlite3.create_module()
    // and as the argument to the USING clause of the CREATE VIRTUAL TABLE
    // statement that is running.
    // - The second, argv[1], is the name of the database in which the new virtual
    // table is being created. The database name is "main" for the primary
    // database, or "temp" for TEMP database, or the name given at the end of
    // the ATTACH statement for attached databases.
    // - The third element of the array, argv[2], is the name of the new virtual
    // table, as specified following the TABLE keyword in the CREATE VIRTUAL
    // TABLE statement
    // - If present, the fourth and subsequent strings in the argv[] array report
    // the arguments to the module name in the CREATE VIRTUAL TABLE statement
    // - As part of the task of creating a new PSqlite3VTab structure, this method
    // must invoke sqlite3.declare_vtab() to tell the SQLite core about the
    // columns and datatypes in the virtual table
    xCreate: function(DB: TSqlite3DB; pAux: Pointer;
      argc: integer; const argv: PPUtf8CharArray;
      var ppVTab: PSqlite3VTab; var pzErr: PUtf8Char): integer; cdecl;
    /// xConnect is called to establish a new connection to an existing virtual table,
    // whereas xCreate is called to create a new virtual table from scratch
    // - It has the same parameters and constructs a new PSqlite3VTab structure
    // - xCreate and xConnect methods are only different when the virtual table
    // has some kind of backing store that must be initialized the first time the
    // virtual table is created. The xCreate method creates and initializes the
    // backing store. The xConnect method just connects to an existing backing store.
    xConnect: function(DB: TSqlite3DB; pAux: Pointer;
      argc: integer; const argv: PPUtf8CharArray;
      var ppVTab: PSqlite3VTab; var pzErr: PUtf8Char): integer; cdecl;
    /// Used to determine the best way to access the virtual table
    // - The pInfo parameter is used for input and output parameters
    // - The SQLite core calls the xBestIndex() method when it is compiling a query
    // that involves a virtual table. In other words, SQLite calls this method when
    // it is running sqlite3.prepare() or the equivalent.
    // - By calling this method, the SQLite core is saying to the virtual table
    // that it needs to access some subset of the rows in the virtual table and
    // it wants to know the most efficient way to do that access. The xBestIndex
    // method replies with information that the SQLite core can then use to
    // conduct an efficient search of the virtual table, via the xFilter() method.
    // - While compiling a single SQL query, the SQLite core might call xBestIndex
    // multiple times with different settings in pInfo. The SQLite
    // core will then select the combination that appears to give the best performance.
    // - The information in the pInfo structure is ephemeral and may be overwritten
    // or deallocated as soon as the xBestIndex() method returns. If the
    // xBestIndex() method needs to remember any part of the pInfo structure,
    // it should make a copy. Care must be taken to store the copy in a place
    // where it will be deallocated, such as in the idxStr field with
    // needToFreeIdxStr set to 1.
    xBestIndex: function(var pVTab: TSqlite3VTab;
      var pInfo: TSqlite3IndexInfo): integer; cdecl;
    /// Releases a connection to a virtual table
    // - Only the pVTab object is destroyed. The virtual table is not destroyed and
    // any backing store associated with the virtual table persists. This method
    // undoes the work of xConnect.
    xDisconnect: function(pVTab: PSqlite3VTab): integer; cdecl;
    /// Releases a connection to a virtual table, just like the xDisconnect method,
    // and it also destroys the underlying table implementation.
    // - This method undoes the work of xCreate
    // - The xDisconnect method is called whenever a database connection that uses
    // a virtual table is closed. The xDestroy method is only called when a
    // DROP TABLE statement is executed against the virtual table.
    xDestroy: function(pVTab: PSqlite3VTab): integer; cdecl;
    /// Creates a new cursor used for accessing (read and/or writing) a virtual table
    // - A successful invocation of this method will allocate the memory for the
    // TSqlite3VTabCursor (or a subclass), initialize the new object, and
    // make ppCursor point to the new object. The successful call then returns SQLITE_OK.
    // - For every successful call to this method, the SQLite core will later
    // invoke the xClose method to destroy the allocated cursor.
    // - The xOpen method need not initialize the pVtab field of the ppCursor structure.
    // The SQLite core will take care of that automatically.
    // - A virtual table implementation must be able to support an arbitrary number
    // of simultaneously open cursors.
    // - When initially opened, the cursor is in an undefined state. The SQLite core
    // will invoke the xFilter method on the cursor prior to any attempt to
    // position or read from the cursor.
    xOpen: function(var pVTab: TSqlite3VTab;
      var ppCursor: PSqlite3VTabCursor): integer; cdecl;
    /// Closes a cursor previously opened by xOpen
    // - The SQLite core will always call xClose once for each cursor opened using xOpen.
    // - This method must release all resources allocated by the corresponding xOpen call.
    // - The routine will not be called again even if it returns an error. The
    // SQLite core will not use the pVtabCursor again after it has been closed.
    xClose: function(pVtabCursor: PSqlite3VTabCursor): integer; cdecl;
    /// Begins a search of a virtual table
    // - The first argument is a cursor opened by xOpen.
    // - The next two arguments define a particular search index previously chosen
    // by xBestIndex(). The specific meanings of idxNum and idxStr are unimportant
    // as long as xFilter() and xBestIndex() agree on what that meaning is.
    // - The xBestIndex() function may have requested the values of certain
    // expressions using the aConstraintUsage[].argvIndex values of its pInfo
    // structure. Those values are passed to xFilter() using the argc and argv
    // parameters.
    // - If the virtual table contains one or more rows that match the search criteria,
    // then the cursor must be left point at the first row. Subsequent calls to
    // xEof must return false (zero). If there are no rows match, then the cursor
    // must be left in a state that will cause the xEof to return true (non-zero).
    // The SQLite engine will use the xColumn and xRowid methods to access that row content.
    // The xNext method will be used to advance to the next row.
    // - This method must return SQLITE_OK if successful, or an sqlite error code
    // if an error occurs.
    xFilter: function(var pVtabCursor: TSqlite3VTabCursor; idxNum: integer;
      const idxStr: PUtf8Char; argc: integer;
      var argv: TSqlite3ValueArray): integer; cdecl;
    /// Advances a virtual table cursor to the next row of a result set initiated by xFilter
    // - If the cursor is already pointing at the last row when this routine is called,
    // then the cursor no longer points to valid data and a subsequent call to the
    // xEof method must return true (non-zero).
    // - If the cursor is successfully advanced to another row of content, then
    // subsequent calls to xEof must return false (zero).
    // - This method must return SQLITE_OK if successful, or an sqlite error code
    // if an error occurs.
    xNext: function(var pVtabCursor: TSqlite3VTabCursor): integer; cdecl;
    /// Checks if cursor reached end of rows
    // - Must return false (zero) if the specified cursor currently points to a
    // valid row of data, or true (non-zero) otherwise
    xEof: function(var pVtabCursor: TSqlite3VTabCursor): integer; cdecl;
    /// The SQLite core invokes this method in order to find the value for the
    // N-th column of the current row
    // - N is zero-based so the first column is numbered 0.
    // - The xColumn method may return its result back to SQLite using one of the
    // standard sqlite3.result_*() functions with the specified sContext
    // - If the xColumn method implementation calls none of the sqlite3.result_*()
    // functions, then the value of the column defaults to an SQL NULL.
    // - The xColumn method must return SQLITE_OK on success.
    // - To raise an error, the xColumn method should use one of the result_text()
    // methods to set the error message text, then return an appropriate error code.
    xColumn: function(var pVtabCursor: TSqlite3VTabCursor;
      sContext: TSqlite3FunctionContext; N: integer): integer; cdecl;
    /// Should fill pRowid with the rowid of row that the virtual table cursor
    // pVtabCursor is currently pointing at
    xRowid: function(var pVtabCursor: TSqlite3VTabCursor;
      var pRowid: Int64): integer; cdecl;
    /// Makes a change to a virtual table content (insert/delete/update)
    // - The nArg parameter specifies the number of entries in the ppArg[] array
    // - The value of nArg will be 1 for a pure delete operation or N+2 for an
    // insert or replace or update where N is the number of columns in the table
    // (including any hidden columns)
    // - The ppArg[0] parameter is the rowid of a row in the virtual table to be deleted.
    // If ppArg[0] is an SQL NULL, then no deletion occurs
    // - The ppArg[1] parameter is the rowid of a new row to be inserted into the
    // virtual table. If ppArg[1] is an SQL NULL, then the implementation must
    // choose a rowid for the newly inserted row. Subsequent ppArg[] entries
    // contain values of the columns of the virtual table, in the order that
    // the columns were declared. The number of columns will match the table
    // declaration that the xConnect or xCreate method made using the
    // sqlite3.declare_vtab() call. All hidden columns are included.
    // - When doing an insert without a rowid (nArg>1, ppArg[1] is an SQL NULL),
    // the implementation must set pRowid to the rowid of the newly inserted row;
    // this will become the value returned by the sqlite3.last_insert_rowid()
    // function. Setting this value in all the other cases is a harmless no-op;
    // the SQLite engine ignores the pRowid return value if nArg=1 or ppArg[1]
    // is not an SQL NULL.
    // - Each call to xUpdate() will fall into one of cases shown below. Note
    // that references to ppArg[i] mean the SQL value held within the ppArg[i]
    // object, not the ppArg[i] object itself:
    // $ nArg = 1
    // The single row with rowid equal to ppArg[0] is deleted. No insert occurs.
    // $ nArg > 1
    // $ ppArg[0] = NULL
    // A new row is inserted with a rowid ppArg[1] and column values in ppArg[2]
    // and following. If ppArg[1] is an SQL NULL, the a new unique rowid is
    // generated automatically.
    // $ nArg > 1
    // $ ppArg[0] <> NULL
    // $ ppArg[0] = ppArg[1]
    // The row with rowid ppArg[0] is updated with new values in ppArg[2] and
    // following parameters.
    // $ nArg > 1
    // $ ppArg[0] <> NULL
    // $ ppArg[0] <> ppArg[1]
    // The row with rowid ppArg[0] is updated with rowid ppArg[1] and new values
    // in ppArg[2] and following parameters. This will occur when an SQL statement
    // updates a rowid, as in the statement:
    // $ UPDATE table SET rowid=rowid+1 WHERE ...;
    // - The xUpdate() method must return SQLITE_OK if and only if it is successful.
    // If a failure occurs, the xUpdate() must return an appropriate error code.
    // On a failure, the pVTab.zErrMsg element may optionally be replaced with
    // a custom error message text.
    // - If the xUpdate() method violates some constraint of the virtual table
    // (including, but not limited to, attempting to store a value of the
    // wrong datatype, attempting to store a value that is too large or too small,
    // or attempting to change a read-only value) then the xUpdate() must fail
    // with an appropriate error code.
    // - There might be one or more TSqlite3VTabCursor objects open and in use on
    // the virtual table instance and perhaps even on the row of the virtual
    // table when the xUpdate() method is invoked. The implementation of xUpdate()
    // must be prepared for attempts to delete or modify rows of the table out
    // from other existing cursors. If the virtual table cannot accommodate such
    // changes, the xUpdate() method must return an error code.
    xUpdate: function(var pVTab: TSqlite3VTab; nArg: integer;
      var ppArg: TSqlite3ValueArray; var pRowid: Int64): integer; cdecl;
    /// Begins a transaction on a virtual table
    // - This method is always followed by one call to either the xCommit or
    // xRollback method.
    // - Virtual table transactions do not nest, so the xBegin method will not be
    // invoked more than once on a single virtual table without an intervening
    // call to either xCommit or xRollback. For nested transactions, use
    // xSavepoint, xRelease and xRollBackTo methods.
    // - Multiple calls to other methods can and likely will occur in between the
    // xBegin and the corresponding xCommit or xRollback.
    xBegin: function(var pVTab: TSqlite3VTab): integer; cdecl;
    /// Signals the start of a two-phase commit on a virtual table
    // - This method is only invoked after call to the xBegin method and prior
    // to an xCommit or xRollback.
    // - In order to implement two-phase commit, the xSync method on all virtual
    // tables is invoked prior to invoking the xCommit method on any virtual table.
    // - If any of the xSync methods fail, the entire transaction is rolled back.
    xSync: function(var pVTab: TSqlite3VTab): integer; cdecl;
    /// Causes a virtual table transaction to commit
    xCommit: function(var pVTab: TSqlite3VTab): integer; cdecl;
    /// Causes a virtual table transaction to rollback
    xRollback: function(var pVTab: TSqlite3VTab): integer; cdecl;
    /// Called during sqlite3.prepare() to give the virtual table implementation
    // an opportunity to overload SQL functions
    // - When a function uses a column from a virtual table as its first argument,
    // this method is called to see if the virtual table would like to overload
    // the function. The first three parameters are inputs: the virtual table,
    // the number of arguments to the function, and the name of the function.
    // If no overloading is desired, this method returns 0. To overload the
    // function, this method writes the new function implementation into pxFunc
    // and writes user data into ppArg and returns 1.
    // - Note that infix functions (LIKE, GLOB, REGEXP, and MATCH) reverse the
    // order of their arguments. So "like(A,B)" is equivalent to "B like A".
    // For the form "B like A" the B term is considered the first argument to the
    // function. But for "like(A,B)" the A term is considered the first argument.
    // - The function pointer returned by this routine must be valid for the
    // lifetime of the pVTab object given in the first parameter.
    xFindFunction: function(var pVTab: TSqlite3VTab; nArg: integer;
      const zName: PUtf8Char; var pxFunc: TSqlFunctionFunc;
      var ppArg: Pointer): integer; cdecl;
    /// Provides notification that the virtual table implementation that the
    // virtual table will be given a new name
    // - If this method returns SQLITE_OK then SQLite renames the table.
    // - If this method returns an error code then the renaming is prevented.
    xRename: function(var pVTab: TSqlite3VTab;
      const zNew: PUtf8Char): integer; cdecl;
    /// Starts a new transaction with the virtual table
    // - SAVEPOINTs are a method of creating transactions, similar to BEGIN and
    // COMMIT, except that the SAVEPOINT and RELEASE commands are named and
    // may be nested. See @http://www.sqlite.org/lang_savepoint.html
    // - iSavepoint parameter indicates the unique name of the SAVEPOINT
    xSavepoint: function(var pVTab: TSqlite3VTab;
      iSavepoint: integer): integer; cdecl;
    /// Merges a transaction into its parent transaction, so that the specified
    // transaction and its parent become the same transaction
    // - Causes all savepoints back to and including the most recent savepoint
    // with a matching identifier to be removed from the transaction stack
    // - Some people view RELEASE as the equivalent of COMMIT for a SAVEPOINT.
    // This is an acceptable point of view as long as one remembers that the
    // changes committed by an inner transaction might later be undone by a
    // rollback in an outer transaction.
    // - iSavepoint parameter indicates the unique name of the SAVEPOINT
    xRelease: function(var pVTab: TSqlite3VTab;
      iSavepoint: integer): integer; cdecl;
    /// Reverts the state of the virtual table content back to what it was just
    // after the corresponding SAVEPOINT
    // - iSavepoint parameter indicates the unique name of the SAVEPOINT
    xRollbackTo: function(var pVTab: TSqlite3VTab;
      iSavepoint: integer): integer; cdecl;
  end;

  /// An instance of the snapshot object records the state of a WAL mode database for
  // some specific point in history.
  // - In WAL mode, multiple database connections that are open on the same database
  // file can each be reading a different historical version of the database file.
  // - When a database connection begins a read transaction, that connection sees an
  // unchanging copy of the database as it existed for the point in time when the
  // transaction first started. Subsequent changes to the database from other connections
  // are not seen by the reader until a new read transaction is started.
  // - The TSqlite3Snapshot object records state information about an historical
  // version of the database file so that it is possible to later open a new read
  // transaction that sees that historical version of the database rather than the
  // most recent version.
  TSqlite3Snapshot = record
    hidden: array[0..47] of Byte;
  end;

  PSqlite3Snapshot = ^TSqlite3Snapshot;

  /// Compile-Time Authorization Callback prototype
  // - The authorizer callback is invoked as SQL statements are being compiled by
  // sqlite3.prepare2() e.g.
  // - The authorizer callback should return SQLITE_OK to allow the action,
  // SQLITE_IGNORE to disallow the specific action but allow the SQL statement
  // to continue to be compiled, or SQLITE_DENY to cause the entire SQL statement
  // to be rejected with an error.
  // - If the authorizer callback returns any value other than SQLITE_IGNORE,
  // SQLITE_OK, or SQLITE_DENY then the sqlite3.prepare_v2() or equivalent call
  // that triggered the authorizer will fail with an error message.
  // - The first pUserData parameter to the authorizer callback is a copy of the
  // third parameter to the sqlite3.set_authorizer() interface
  // - The second parameter to the callback is an integer action code that
  // specifies the particular action to be authorized:
  // - The third through sixth parameters to the callback are zero-terminated
  // strings that contain additional details about the action to be authorized.
  // - Here is a list of handled code constant, and their associated zTab / zCol
  // parameters:
  // !    const                       zTab            zCol
  // $ SQLITE_CREATE_INDEX          Index Name      Table Name
  // $ SQLITE_CREATE_TABLE          Table Name      nil
  // $ SQLITE_CREATE_TEMP_INDEX     Index Name      Table Name
  // $ SQLITE_CREATE_TEMP_TABLE     Table Name      nil
  // $ SQLITE_CREATE_TEMP_TRIGGER   Trigger Name    Table Name
  // $ SQLITE_CREATE_TEMP_VIEW      View Name       nil
  // $ SQLITE_CREATE_TRIGGER        Trigger Name    Table Name
  // $ SQLITE_CREATE_VIEW           View Name       nil
  // $ SQLITE_DELETE                Table Name      nil
  // $ SQLITE_DROP_INDEX            Index Name      Table Name
  // $ SQLITE_DROP_TABLE            Table Name      nil
  // $ SQLITE_DROP_TEMP_INDEX       Index Name      Table Name
  // $ SQLITE_DROP_TEMP_TABLE       Table Name      nil
  // $ SQLITE_DROP_TEMP_TRIGGER     Trigger Name    Table Name
  // $ SQLITE_DROP_TEMP_VIEW        View Name       nil
  // $ SQLITE_DROP_TRIGGER          Trigger Name    Table Name
  // $ SQLITE_DROP_VIEW             View Name       nil
  // $ SQLITE_INSERT                Table Name      nil
  // $ SQLITE_PRAGMA                Pragma Name     1st arg or nil
  // $ SQLITE_READ                  Table Name      Column Name
  // $ SQLITE_SELECT                nil             nil
  // $ SQLITE_TRANSACTION           Operation       nil
  // $ SQLITE_UPDATE                Table Name      Column Name
  // $ SQLITE_ATTACH                Filename        nil
  // $ SQLITE_DETACH                Database Name   nil
  // $ SQLITE_ALTER_TABLE           Database Name   Table Name
  // $ SQLITE_REINDEX               Index Name      nil
  // $ SQLITE_ANALYZE               Table Name      nil
  // $ SQLITE_CREATE_VTABLE         Table Name      Module Name
  // $ SQLITE_DROP_VTABLE           Table Name      Module Name
  // $ SQLITE_FUNCTION              nil             Function Name
  // $ SQLITE_SAVEPOINT             Operation       Savepoint Name
  // - The 5th parameter to the authorizer callback is the name of the database
  // ('main', 'temp', etc.) if applicable.
  // - The 6th parameter to the authorizer callback is the name of the inner-most
  // trigger or view that is responsible for the access attempt or nil if this
  // access attempt is directly from top-level SQL code.
  TSqlAuthorizerCallback = function(pUserData: Pointer; code: integer;
    const zTab, zCol, zDb, zAuthContext: PUtf8Char): integer; cdecl;

  /// Callback function invoked for each new database connection that is created
  // after sqlite3.auto_extension() registration
  // - If this method encounters an error, it should make pzErrMsg point to an
  // appropriate error message and return an appropriate error code.
  // - SQLite ensures that pzErrMsg is nil before calling this method().
  // - SQLite will invoke sqlite3.free() on pzErrMsg after this returns.
  // - If TSqlEntryPointCallback returns an error, the sqlite3.open() or sqlite3_open_v2()
  // call that provoked the TSqlEntryPointCallback() will fail.
  TSqlEntryPointCallback = function(DB: TSqlite3DB; var pzErrMsg: PUtf8Char;
    pThunk: TSqlite3APIRoutines): integer; cdecl;

  /// Callback function invoked prior to each INSERT, UPDATE, and DELETE operation on
  // a database table after sqlite3.preupdate_hook() registration
  // - pArg is a copy of the third argument to sqlite3.preupdate_hook().
  // - op is one of SQLITE_INSERT, SQLITE_DELETE, or SQLITE_UPDATE,
  // depending on the operation that caused the callback to be invoked.
  // - zDb is the name of the database within the database connection that is being
  // modified. This will be "main" for the main database or "temp" for TEMP tables or
  // the name given after the AS keyword in the ATTACH statement for attached databases.
  // - zName is the name of the table that is being modified.
  // - For an UPDATE or DELETE operation on a rowid table, iKey1 is the initial rowid of
  // the row being modified or deleted.
  // - For an INSERT operation on a rowid table, or any operation on a WITHOUT ROWID table,
  // iKey1 is undefined.
  // - For an INSERT or UPDATE on a rowid table iKey2 is the final rowid value of the row
  // being inserted or updated.
  // - iKey2 is not defined for operations on WITHOUT ROWID tables, or for DELETE
  // operations on rowid tables.
  // - The sqlite3.preupdate_old(), sqlite3.preupdate_new(), sqlite3.preupdate_count(),
  // and sqlite3.preupdate_depth() interfaces provide additional information about a
  // preupdate event.
  // - These routines may only be called from within a preupdate callback.
  // - Invoking any of these routines from outside of a preupdate callback or with a database
  // connection pointer that is different from the one supplied to the preupdate callback
  // results in undefined and probably undesirable behavior.
  TSqlPreUpdateCallback = procedure(pArg: Pointer; DB: TSqlite3DB;
    op: integer; zDb, zName: PUtf8Char; iKey1, iKey2: Int64); cdecl;

  /// Callback function invoked wen running in shared-cache mode, a database operation
  // may fail with an SQLITE_LOCKED error after sqlite3.unlock_notify() registration
  // - When an unlock-notify callback is registered, the application provides a single Pointer
  // that is passed to the callback when it is invoked. However, the signature of the callback
  // function allows SQLite to pass it an array of context pointers.
  // The first argument passed to an unlock-notify callback is a pointer to an array of pointers,
  // and the second is the number of entries in the array.
  // - When a blocking connection's transaction is concluded, there may be more than one blocked
  // connection that has registered for an unlock-notify callback.
  // - If two or more such blocked connections have specified the same callback function, then
  // instead of invoking the callback function multiple times, it is invoked once with the set of
  // context pointers specified by the blocked connections bundled together into an array.
  // - This gives the application an opportunity to prioritize any actions related to the set of
  // unblocked database connections.
  TSqlUnlockNotify = procedure(apArg: PPointerArray; nArg: integer); cdecl;

  /// Callback function invoked when a row is updated, inserted or deleted,
  // after sqlite3.update_hook() registration
  // - The first pUpdateArg argument is a copy of the third argument to
  // sqlite3.update_hook().
  // - The second op argument is one of SQLITE_INSERT, SQLITE_DELETE, or SQLITE_UPDATE,
  // depending on the operation that caused the callback to be invoked.
  // - The third and fourth zDb / zTbl arguments contain pointers to the database
  // and table name containing the affected row.
  // - The final iRowID parameter is the rowid of the row. In the case of an update,
  // this is the rowid after the update takes place.
  // - The update hook implementation must not do anything that will modify the
  // database connection that invoked the update hook. Any actions to modify the
  // database connection must be deferred until after the completion of the
  // sqlite3.step() call that triggered the update hook. Note that
  // sqlite3.prepare_v2() and sqlite3.step() both modify their database
  // connections for the meaning of "modify" in this paragraph.
  TSqlUpdateCallback = procedure(pUpdateArg: Pointer; op: integer;
    const zDb, zTbl: PUtf8Char; iRowID: Int64); cdecl;

  /// Commit And Rollback Notification Callback function after
  // sqlite3.commit_hook() or sqlite3.rollback_hook() registration
  // - The callback implementation must not do anything that will modify the
  // database connection that invoked the callback. Any actions to modify the
  // database connection must be deferred until after the completion of the
  // sqlite3.step() call that triggered the commit or rollback hook in the
  // first place. Note that sqlite3.prepare_v2() and sqlite3.step() both modify
  // their database connections for the meaning of "modify" in this paragraph.
  // - When the commit hook callback routine returns zero, the COMMIT operation
  // is allowed to continue normally. If the commit hook returns non-zero, then
  // the COMMIT is converted into a ROLLBACK. The rollback hook is invoked on
  // a rollback that results from a commit hook returning non-zero, just as
  // it would be with any other rollback.
  // - For the purposes of this API, a transaction is said to have been rolled
  // back if an explicit "ROLLBACK" statement is executed, or an error or
  // constraint causes an implicit rollback to occur. The rollback callback
  // is not invoked if a transaction is automatically rolled back because the
  // database connection is closed.
  TSqlCommitCallback = function(pArg: Pointer): integer; cdecl;

  /// SQLite3 callback to handle sqlite3.progress_handler()
  // - UserData is a copy of the user pointer which is the forth argument to sqlite3.progress_handler().
  // - If returns non-zero, the operation is interrupted. This feature can be used to implement a
  // "Cancel" button on a GUI progress dialog box.
  // - Must not do anything that will modify the database connection that invoked the
  // progress handler.
  // Note that sqlite3.prepare_v2() and sqlite3.step() both modify their database connections
  // for the meaning of "modify" in this paragraph.
  TSqlProgressCallback = function(UserData: pointer): integer; cdecl;


  /// SQLite3 callback to handle sqlite3.collation_needed()
  // - CollateParam is a copy of the user pointer which is the second argument to
  // sqlite3.collation_needed().
  // - The StringEncoding is one of SQLITE_UTF8, SQLITE_UTF16BE, or SQLITE_UTF16LE,
  // indicating the most desirable form of the collation sequence function required.
  // The CollationName is the name of the required collation sequence.
  TSqlCollationNeededCallback = function(CollateParam: pointer; DB: TSqlite3DB;
    StringEncoding: integer; CollationName: PUtf8Char): integer; cdecl;

  /// events monitored by sqlite3.trace_v2() tracing logic
  // - stmStmt callback is invoked when a prepared statement first begins
  // running and possibly at other times during the execution of the prepared
  // statement, such as at the start of each trigger subprogram. The P argument
  // is a pointer to the prepared statement. The X argument is a pointer to a
  // string which is the unexpanded SQL text of the prepared statement or an
  // SQL comment that indicates the invocation of a trigger.
  // - stmProfile callback provides approximately the same information as was
  // provided by the deprecated sqlite3.profile() callback. The P argument is
  // a pointer to the prepared statement and the X argument points to a 64-bit
  // integer which is the estimated of the number of nanosecond that the
  // prepared statement took to run. The stmProfile callback is invoked when
  // the statement finishes.
  // - stmRow callback is invoked whenever a prepared statement generates
  // a single row of result. The P argument is a pointer to the prepared
  // statement and the X argument is unused.
  // - stmClose callback is invoked when a database connection closes. The
  // P argument is a pointer to the database connection object and the X
  // argument is unused.
  TSqlTraceMask = set of (
    stmStmt,
    stmProfile,
    stmRow,
    stmClose);


  TSqlPrepareFlags = set of (
    spfPersistent = SQLITE_PREPARE_PERSISTENT,
    spfNoVTab = SQLITE_PREPARE_NO_VTAB);

  /// Callback function registered by sqlite3.trace_v2()
  // - the Trace argument has one of the TSqlTraceMask items set, to indicate
  // why the callback was invoked
  // - UserData argument is a copy of the context pointer, as provided at
  // sqlite3.trace_v2() call
  // - P and X arguments are pointers whose meanings depend on Trace content:
  // see TSqlTraceMask for the various use cases
  TSqlTraceCallback = procedure(Trace: TSqlTraceMask;
    UserData, P, X: pointer); cdecl;

  /// Callback function registered by sqlite3.profile()
  // - This procedure will be invoked as each SQL statement finishes
  // - warning: sqlite3.profile() function is considered experimental and is
  // subject to change in future versions of SQLite
  TSqlProfileCallback = procedure(ProfileArg: Pointer; Profile: PUtf8Char;
    ProfileNanoSeconds: Int64); cdecl;

  /// Callback function registered by sqlite3.exec()
  // - This procedure will be invoked for each result row coming out of the evaluated SQL statements
  // - If returns non-zero, the sqlite3.exec() routine returns SQLITE_ABORT without invoking
  // the callback again and without running any subsequent SQL statements.
  // - UserData argument is a copy of the context pointer, as provided at sqlite3.exec() call
  // - NumCols is the number of columns in the result
  // - ColValues is an array of pointers to strings obtained as if from sqlite3.column_text(),
  // one for each column. If an element of a result row is NULL then the corresponding string
  // pointer is a NULL pointer.
  // - ColNames is an array of pointers to strings where each entry represents the name of
  // corresponding result column as obtained from sqlite3.column_name().
  TSqlExecCallback = function(UserData: pointer; NumCols: integer; ColValues:
    PPUtf8CharArray; ColNames: PPUtf8CharArray): integer; cdecl;

  /// Callback function registered by sqlite3.wal_hook()
  // - This procedure is invoked by SQLite after the commit has taken place and the
  // associated write-lock on the database released, so the implementation may read,
  // write or checkpoint the database as required.
  // - UserData argument is a copy of the context pointer, as provided at sqlite3.wal_hook() call
  // - DB is a copy of the database handle.
  // - DBName is the name of the database that was written to - either "main" or the name of
  // an ATTACH-ed database.
  // - PageCount is the number of pages currently in the write-ahead log file, including those
  // that were just committed.
  // - The callback function should normally return SQLITE_OK.
  // - If an error code is returned, that error will propagate back up through the SQLite code
  // base to cause the statement that provoked the callback to report an error, though the commit
  // will have still occurred.
  // - If the callback returns SQLITE_ROW or SQLITE_DONE, or if it returns a value that does not
  // correspond to any valid SQLite error code, the results are undefined.
  TSqlWalHookCallback = function(UserData: pointer; DB: TSqlite3DB;
    DBName: PUtf8Char; PageCount: integer): integer; cdecl;

  /// defines the interface between SQLite and low-level memory allocation routines
  // - as used by sqlite3.config(SQLITE_CONFIG_MALLOC,pMemMethods);
  TSqlite3MemMethods = record
    /// Memory allocation function
    xMalloc: function(size: integer): pointer; cdecl;
    /// Free a prior allocation
    xFree: procedure(ptr: pointer); cdecl;
    /// Resize an allocation
    xRealloc: function(ptr: pointer; size: integer): pointer; cdecl;
    /// Return the size of an allocation
    xSize: function(ptr: pointer): integer; cdecl;
    /// Round up request size to allocation size
    xRoundup: function(size: integer): integer; cdecl;
    /// Initialize the memory allocator
    xInit: function(appData: pointer): integer; cdecl;
    /// Deinitialize the memory allocator
    xShutdown: procedure(appData: pointer); cdecl;
    /// Argument to xInit() and xShutdown()
    pAppData: pointer;
  end;

  {$M+}
  /// wrapper around all SQLite3 library API calls
  // - abstract class allowing direct binding of static sqlite3.obj
  // (TSqlite3LibrayStatic) or with an external library (TSqlite3LibraryDynamic)
  // - a global sqlite3: TSqlite3Library will be defined in this unit, so
  // you should call sqlite3.open() instead of sqlite3_open() for instance
  // - if your project refers to mormot.db.raw.sqlite3.static unit, it will
  // initialize a TSqlite3LibrayStatic instance
  TSqlite3Library = class
  protected
    fUseInternalMM: boolean;
    fVersionNumber: cardinal;
    fVersionText: RawUtf8;
    function GetVersion: RawUtf8;
  public
    /// Initialize the SQLite3 database code
    // - automaticaly called by the initialization block of this unit
    // - so sqlite3.c is compiled with SQLITE_OMIT_AUTOINIT defined
    initialize: function: integer; cdecl;

    /// Shutdown the SQLite3 database core
    // - automaticaly called by the finalization block of this unit
    shutdown: function: integer; cdecl;

    /// Open a SQLite3 database filename, creating a DB handle
    // - filename must be UTF-8 encoded (filenames containing international
    // characters must be converted to UTF-8 prior to passing them)
    // - allocate a sqlite3 object, and return its handle in DB
    // - return SQLITE_OK on success
    // - an error code (see SQLITE_* const) is returned otherwise - sqlite3.errmsg()
    // can be used to obtain an English language description of the error
    // - Whatever or not an error occurs when it is opened, resources associated with
    // the database connection handle should be released by passing it to
    // sqlite3.close() when it is no longer required
    open: function(filename: PUtf8Char; var DB: TSqlite3DB): integer; cdecl;

    /// Open a SQLite3 database filename, creating a DB handle
    // - sqlite3.open_v2() interface works like sqlite3.open() except that it
    // accepts two additional parameters for additional control over the new
    // database connection.
    // - flags parameter to sqlite3.open_v2() can take one of SQLITE_OPEN_READONLY,
    // SQLITE_OPEN_READWRITE or (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)
    // values, optionally combined with the SQLITE_OPEN_NOMUTEX,
    // SQLITE_OPEN_FULLMUTEX, SQLITE_OPEN_SHAREDCACHE, SQLITE_OPEN_PRIVATECACHE,
    // and/or SQLITE_OPEN_URI flags
    // - If the flags parameter is not one of the combinations shown above optionally
    // combined with other SQLITE_OPEN_* bits then the behavior is undefined.
    // - The fourth parameter is the name of the sqlite3_vfs object that defines
    // the operating system interface that the new database connection should use.
    // If the fourth parameter is a nil pointer then the default sqlite3_vfs
    // object is used
    open_v2: function(filename: PUtf8Char; var DB: TSqlite3DB; flags: integer;
      zVfszVfs: PUtf8Char): integer; cdecl;

    /// Specify the encryption key on a newly opened database connection
    // - Assigned(key)=false if encryption is not available for this .dll
    // - mormot.db.raw.sqlite3.static will use its own internal encryption format
    // - key/keylen may be a JSON-serialized TSynSignerParams object, or will use
    // AES-OFB-128 after SHAKE_128 with rounds=1000 and a fixed salt on plain password text
    key: function(DB: TSqlite3DB; key: pointer; keyLen: integer): integer; cdecl;

    /// change the encryption key on a database connection that is already opened
    // -  can also decrypt a previously encrypted database (so that it is accessible
    // from any version of SQLite) by specifying a nil key
    // - Assigned(rekey)=false if encryption is not available, i.e. if
    // NOSQLITE3STATIC is defined
    // - also see ChangeSQLEncryptTablePassWord() procedure
    rekey: function(DB: TSqlite3DB; key: pointer; keyLen: integer): integer; cdecl;

    /// Destructor for the sqlite3 object, which handle is DB
    //  - Applications should finalize all prepared statements and close all BLOB handles
    // associated with the sqlite3 object prior to attempting to close the object
    // (sqlite3.next_stmt() interface can be used for this task)
    // - if invoked while a transaction is open, the transaction is automatically rolled back
    // - mormot.db.raw.sqlite3.static will use its own internal function for
    // handling properly its own encryption format
    close: function(DB: TSqlite3DB): integer; cdecl;

    /// Return the version of the SQLite database engine, in ascii format
    // - currently returns '3.36.0', when used in conjunction with our
    // mormot.db.raw.sqlite3.static unit
    // - if an external SQLite3 library is used, version may vary
    // - you may use the VersionText property (or Version for full details) instead
    libversion: function: PUtf8Char; cdecl;

    /// Returns the integer version of the SQLite database engine like 3034000
    libversion_number: function: integer; cdecl;

    /// Returns string containing the date and time of the check-in (UTC) and a SHA1
    // or SHA3-256 hash of the entire source tree.
    sourceid: function: PUtf8Char; cdecl;

    /// Returns zero if and only if SQLite was compiled with mutexing code omitted due
    // to the SQLITE_THREADSAFE compile-time option being set to 0.
    // - This interface can be used by an application to make sure that the version of
    // SQLite that it is linking against was compiled with the desired setting of the
    // SQLITE_THREADSAFE macro.
    // - This interface only reports on the compile-time mutex setting of the SQLITE_THREADSAFE flag.
    // - If SQLite is compiled with SQLITE_THREADSAFE=1 or =2 then mutexes are enabled by default
    // but can be fully or partially disabled using a call to sqlite3_config() with the verbs
    // SQLITE_CONFIG_SINGLETHREAD, SQLITE_CONFIG_MULTITHREAD, or SQLITE_CONFIG_SERIALIZED.
    // T- he return value of the sqlite3.threadsafe() function shows only the compile-time setting
    // of thread safety, not any run-time changes to that setting made by sqlite3.config().
    // - In other words, the return value from sqlite3.threadsafe() is unchanged by calls to sqlite3_config().
    threadsafe: function: integer; cdecl;

    /// Returns the numeric result code or extended result code for the most
    // recent failed sqlite3 API call associated with a database connection
    errcode: function(DB: TSqlite3DB): integer; cdecl;

    /// Returns the extended result code for the most recent failed sqlite3 API
    // call associated with a database connection
    // - Use sqlite3.extended_result_codes() to enabled or disabled on a per database connection basis
    extended_errcode: function(DB: TSqlite3DB): integer; cdecl;

    /// Returns English-language text that describes the most recent error,
    // using UTF-8 encoding (which, with English text, is the same as Ansi).
    // - Memory to hold the error message string is managed internally.
    // The application does not need to worry about freeing the result.
    // However, the error string might be overwritten or deallocated by
    // subsequent calls to other SQLite interface functions.
    errmsg: function(DB: TSqlite3DB): PUtf8Char; cdecl;

    /// Returns English-language text that describes an error,
    // using UTF-8 encoding (which, with English text, is the same as Ansi).
    // - Memory to hold the error message string is managed internally.
    // The application does not need to worry about freeing the result.
    // However, the error string might be overwritten or deallocated by
    // subsequent calls to other SQLite interface functions.
    errstr: function(Code: integer): PUtf8Char; cdecl;

    /// Attempt to return the underlying operating system error code or error number
    // that caused the most recent I/O error or failure to open a file.
    // - The return value is OS-dependent.
    // - For example, on unix systems, after sqlite3.open_v2() returns SQLITE_CANTOPEN,
    // this interface could be called to get back the underlying "errno" that caused the problem,
    // such as ENOSPC, EAUTH, EISDIR, and so forth.
    system_errno: function(DB: TSqlite3DB): integer; cdecl;

    /// Enables or disables the extended result codes feature of SQLite.
    // - The extended result codes are disabled by default for historical compatibility.
    // - The extended code for the most recent error can be obtained using sqlite3.extended_errcode()
    extended_result_codes: function(DB: TSqlite3DB; OnOff: integer): integer; cdecl;

    /// Determine if the currently entered text seems to form a complete SQL statement
    // or if additional input is needed before sending the text into SQLite for parsing.
    // - SQL must contains an UTF-8 encoded null-terminated string query
    // - This routine return 1 if the input string appears to be a complete SQL statement.
    // - A statement is judged to be complete if it ends with a semicolon token and is not
    // a prefix of a well-formed CREATE TRIGGER statement.
    // - Semicolons that are embedded within string literals or quoted identifier names
    // or comments are not independent tokens (they are part of the token in which they are
    // embedded) and thus do not count as a statement terminator.
    // - Whitespace and comments that follow the final semicolon are ignored.
    // - This routine return 0 if the statement is incomplete.
    // - If a memory allocation fails, then SQLITE_NOMEM is returned.
    // - This routine do not parse the SQL statements thus will not detect syntactically
    // incorrect SQL.
    complete: function(SQL: PUtf8Char): integer; cdecl;

    /// Returns the number of distinct keywords understood by SQLite.
    keyword_count: function: integer; cdecl;

    /// Finds the N-th keyword and makes Identifier point to that keyword expressed as UTF8
    // and writes the number of bytes in the keyword into L.
    // - The string that Identifier points to is NOT zero-terminated.
    // - Returns SQLITE_OK if N is within bounds and SQLITE_ERROR if not.
    // - If either Identifier or L are nil or invalid pointers then calls result in undefined behavior.
    keyword_name: function(Nth: integer; var Identifier: PUtf8Char; L: PInteger): integer; cdecl;

    /// Checks to see whether or not the L-byte UTF8 identifier that Identifier points to is a keyword
    // - Returning non-zero if it is and zero if not.
    keyword_check: function(Identifier: PUtf8Char; L: integer): integer; cdecl;

    /// Returns the current transaction state of schema SchemaName in database connection DB.
    // - If SchemaName is nil, then the highest transaction state of any schema on database connection DB
    // is returned.
    // - Transaction states are SQLITE_TXN_NONE, SQLITE_TXN_READ and SQLITE_TXN_WRITE.
    // - If SchemaName is not the name of a valid schema, then -1 is returned.
    txn_state: function(DB: TSqlite3DB; SchemaName: PUtf8Char): integer; cdecl;

    /// Convenience wrapper around sqlite3.prepare_v2(), sqlite3.step(), and sqlite3.finalize(),
    // that allows to run multiple statements of SQL without having to use a lot of code.
    // - Runs zero or more UTF-8 encoded, semicolon-separate SQL statements passed into SQL argument.
    // - Callback is invoked for each result row coming out of the evaluated SQL statements.
    // - UserData is relayed through to the 1st argument of each Callback invocation.
    // - If Callback is nil then no callback is ever invoked and result rows are ignored.
    // - If an error occurs while evaluating the SQL statements, then execution of the
    // current statement stops and subsequent statements are skipped.
    // - If Callback returns non-zero, the sqlite3.exec() routine returns SQLITE_ABORT without
    // invoking the callback again and without running any subsequent SQL statements.
    // - If ErrorMsg is not nil then any error message is written into memory obtained from
    // sqlite3.malloc() and passed back through.
    // - To avoid memory leaks, the application should invoke sqlite3.free() on error message
    // strings returned after it is no longer needed.
    // If ErrorMsg is not nil and no errors occur, then sqlite3.exec() sets it to nil
    // before returning.
    exec: function(DB: TSqlite3DB; SQL: PUtf8Char; Callback: TSqlExecCallback;
      UserData: Pointer; var ErrorMsg: PUtf8Char): integer; cdecl;

    /// This function causes any pending database operation to abort and return at its
    // earliest opportunity.
    // - This routine is typically called in response to a user action such as pressing
    // "Cancel" or Ctrl-C where the user wants a long query operation to halt immediately.
    // - It is safe to call this routine from a thread different from the thread that is
    // currently running the database operation.
    // - But it is not safe to call this routine with a database connection that is closed
    // or might close before sqlite3.interrupt() returns.
    // - If an SQL operation is very nearly finished at the time when sqlite3.interrupt() is
    // called, then it might not have an opportunity to be interrupted and might continue
    // to completion.
    // - An SQL operation that is interrupted will return SQLITE_INTERRUPT.
    // - If the interrupted SQL operation is an INSERT, UPDATE, or DELETE that is inside an
    // explicit transaction, then the entire transaction will be rolled back automatically.
    // - The sqlite3.interrupt(DB) call is in effect until all currently running SQL statements
    // on database connection DB complete.
    // - Any new SQL statements that are started after the sqlite3.interrupt() call and before
    // the running statement count reaches zero are interrupted as if they had been running prior
    // to the sqlite3.interrupt() call.
    // - New SQL statements that are started after the running statement count reaches zero are
    // not effected by the sqlite3.interrupt().
    // - A call to sqlite3.interrupt(DB) that occurs when there are no running SQL statements is
    // a no-op and has no effect on SQL statements that are started after the sqlite3.interrupt()
    // call returns.
    interrupt: procedure(DB: TSqlite3DB); cdecl;

    /// Returns the rowid of the most recent successful INSERT into the database
    last_insert_rowid: function(DB: TSqlite3DB): Int64; cdecl;

    /// Allows the application to set the value returned by calling sqlite3.last_insert_rowid()
    // to R without inserting a row into the database.
    set_last_insert_rowid: procedure(DB: TSqlite3DB; R: Int64); cdecl;

    /// Set A Busy Timeout
    // - This routine sets a busy handler that sleeps for a specified amount of time
    // when a table is locked. The handler will sleep multiple times until at least
    // "ms" milliseconds of sleeping have accumulated. After at least "ms" milliseconds
    // of sleeping, the handler returns 0 which causes sqlite3.step() to return
    // SQLITE_BUSY or SQLITE_IOERR_BLOCKED.
    // - Calling this routine with an argument less than or equal to zero turns off
    // all busy handlers.
    // - There can only be a single busy handler for a particular database connection
    // any given moment. If another busy handler was defined (using
    // sqlite3.busy_handler()) prior to calling this routine, that other busy handler
    // is cleared.
    busy_timeout: function(DB: TSqlite3DB; Milliseconds: integer): integer; cdecl;

    /// Register A Callback To Handle SQLITE_BUSY Errors
    // - This routine sets a callback function that might be invoked whenever an
    // attempt is made to open a database table that another thread or process has locked.
    // - If the busy callback is nil, then SQLITE_BUSY or SQLITE_IOERR_BLOCKED is
    // returned immediately upon encountering the lock. If the busy callback is not
    // nil, then the callback might be invoked with two arguments.
    // - The default busy callback is nil.
    busy_handler: function(DB: TSqlite3DB;
      CallbackPtr: TSqlBusyHandler; user: Pointer): integer;  cdecl;

    /// Causes the callback function X to be invoked periodically during long running calls to
    // sqlite3.exec() and sqlite3.step() for database connection DB.
    // - UserData is passed through as the only parameter to the Callback.
    // - Only a single progress handler may be defined at one time per database connection;
    // setting a new progress handler cancels the old one.
    // Setting Callback to nil disables the progress handler.
    // The progress handler is also disabled by setting N to a value less than 1.
    progress_handler: procedure(DB: TSqlite3DB; N: integer; Callback: TSqlProgressCallback;
      UserData: pointer); cdecl;

        /// Returns non-zero or zero if the given database connection is or is not in autocommit
    // mode, respectively.
    // - Autocommit mode is on by default.
    // - Autocommit mode is disabled by a BEGIN statement.
    // - Autocommit mode is re-enabled by a COMMIT or ROLLBACK.
    // - If certain kinds of errors occur on a statement within a multi-statement transaction
    // (errors including SQLITE_FULL, SQLITE_IOERR, SQLITE_NOMEM, SQLITE_BUSY, and SQLITE_INTERRUPT)
    // then the transaction might be rolled back automatically.
    // - The only way to find out whether SQLite automatically rolled back the transaction after
    // an error is to use this function.
    // - If another thread changes the autocommit status of the database connection while this
    // routine is running, then the return value is undefined.
    get_autocommit: function(DB: TSqlite3DB): integer; cdecl;

    /// Registers an authorizer callback to a specified DB connection
    // - Only a single authorizer can be in place on a database connection at a time
    // - Each call to sqlite3.set_authorizer overrides the previous call
    // - Disable the authorizer by installing a nil callback
    // - The authorizer is disabled by default
    set_authorizer: function(DB: TSqlite3DB; xAuth: TSqlAuthorizerCallback;
      pUserData: Pointer): integer; cdecl;

    /// Registers a callback function that is invoked prior to each INSERT, UPDATE,
    // and DELETE operation on a database table.
    // - At most one preupdate hook may be registered at a time on a single database connection
    // - Each call to sqlite3.preupdate_hook() overrides the previous setting.
    // - The preupdate hook is disabled by invoking sqlite3.preupdate_hook() with a nil pointer
    // as xCallback.
    // - pArg is passed through as the first parameter to callbacks.
    // - The preupdate hook only fires for changes to real database tables;
    // - The preupdate hook is not invoked for changes to virtual tables or to system tables
    // like sqlite_sequence or sqlite_stat1.
    // - The sqlite3.preupdate_old(), sqlite3.preupdate_new(), sqlite3.preupdate_count(),
    // and sqlite3.preupdate_depth() interfaces provide additional information about a
    // preupdate event.
    preupdate_hook: function(DB: TSqlite3DB; xCallback: TSqlPreUpdateCallback;
      pArg: pointer): pointer; cdecl;

    /// Writes into Value that contains the value of the Nth column of the table row
    // before it is updated.
    // - The N parameter must be between 0 and one less than the number of columns or the
    // behavior will be undefined.
    // - This must only be used within SQLITE_UPDATE and SQLITE_DELETE preupdate callbacks;
    // if it is used by an SQLITE_INSERT callback then the behavior is undefined.
    // - Value will be destroyed when the preupdate callback returns.
    // - This routine may only be called from within a preupdate callback.
    // - Invoking this routine from outside of a preupdate callback or with a database
    // connection pointer that is different from the one supplied to the preupdate callback
    // results in undefined and probably undesirable behavior.
    preupdate_old: function(DB: TSqlite3DB; N: integer; var Value: TSqlite3Value): integer; cdecl;

    /// Writes into Value that contains the value of the Nth column of the table row
    // before it is updated.
    // - The N parameter must be between 0 and one less than the number of columns or the
    // behavior will be undefined.
    // - This must only be used within SQLITE_INSERT and SQLITE_UPDATE preupdate callbacks;
    // if it is used by an SQLITE_INSERT callback then the behavior is undefined.
    // - Value will be destroyed when the preupdate callback returns.
    // - This routine may only be called from within a preupdate callback.
    // - Invoking this routine from outside of a preupdate callback or with a database
    // connection pointer that is different from the one supplied to the preupdate callback
    // results in undefined and probably undesirable behavior.
    preupdate_new: function(DB: TSqlite3DB; N: integer; var Value: TSqlite3Value): integer; cdecl;

    // Returns the number of columns in the row that is being inserted, updated, or deleted.
    // - This routine may only be called from within a preupdate callback.
    // - Invoking this routine from outside of a preupdate callback or with a database
    // connection pointer that is different from the one supplied to the preupdate callback
    // results in undefined and probably undesirable behavior.
    preupdate_count: function(DB: TSqlite3DB): integer; cdecl;

    /// Returns 0 if the preupdate callback was invoked as a result of a direct insert,
    // update, or delete operation; or 1 for inserts, updates, or deletes invoked by
    // top-level triggers; or 2 for changes resulting from triggers called by top-level
    // triggers; and so forth.
    // - This routine may only be called from within a preupdate callback.
    // - Invoking this routine from outside of a preupdate callback or with a database
    // connection pointer that is different from the one supplied to the preupdate callback
    // results in undefined and probably undesirable behavior.
    preupdate_depth: function(DB: TSqlite3DB): integer; cdecl;

    /// Register Unlock Notification
    // - When running in shared-cache mode, a database operation may fail with an SQLITE_LOCKED
    // error if the required locks on the shared-cache or individual tables within the shared-cache
    // cannot be obtained. See SQLite Shared-Cache Mode for a description of shared-cache locking.
    // - This API may be used to register a callback that SQLite will invoke when the connection
    // currently holding the required lock relinquishes it.
    // - Shared-cache locks are released when a database connection concludes its current transaction,
    // either by committing it or rolling it back.
    // - When a connection (known as the blocked connection) fails to obtain a shared-cache lock
    // and SQLITE_LOCKED is returned to the caller, the identity of the database connection
    // (the blocking connection) that has locked the required resource is stored internally.
    // - After an application receives an SQLITE_LOCKED error, it may call the sqlite3.unlock_notify()
    // method with the blocked connection handle as the first argument to register for a callback that
    // will be invoked when the blocking connections current transaction is concluded.
    // - The callback is invoked from within the sqlite3.step or sqlite3.close call that concludes the
    // blocking connection's transaction.
    // - If sqlite3.unlock_notify() is called in a multi-threaded application, there is a chance that
    // the blocking connection will have already concluded its transaction by the time
    // sqlite3.unlock_notify() is invoked.
    // - If this happens, then the specified callback is invoked immediately, from within the call to
    // sqlite3.unlock_notify().
    // - If the blocked connection is attempting to obtain a write-lock on a shared-cache table, and
    // more than one other connection currently holds a read-lock on the same table, then SQLite
    // arbitrarily selects one of the other connections to use as the blocking connection.
    // - There may be at most one unlock-notify callback registered by a blocked connection.
    // - If sqlite3.unlock_notify() is called when the blocked connection already has a registered
    // unlock-notify callback, then the new callback replaces the old. If sqlite3.unlock_notify()
    // is called with a nil pointer as its second argument, then any existing unlock-notify callback
    // is canceled.
    // - The blocked connections unlock-notify callback may also be canceled by closing the blocke
    // connection using sqlite3.close().
    // - The unlock-notify callback is not reentrant. If an application invokes any sqlite3.* API
    // functions from within an unlock-notify callback, a crash or deadlock may be the result.
    // - Unless deadlock is detected (see below), sqlite3.unlock_notify() always returns SQLITE_OK.
    // - Deadlock Detection:
    // - Assuming that after registering for an unlock-notify callback a database waits for the callback
    // to be issued before taking any further action (a reasonable assumption), then using this API may
    // cause the application to deadlock.
    // - For example, if connection X is waiting for connection Y's transaction to be concluded,
    // and similarly connection Y is waiting on connection X's transaction, then neither connection
    // will proceed and the system may remain deadlocked indefinitely.
    // - To avoid this scenario, the sqlite3.unlock_notify() performs deadlock detection.
    // - If a given call to sqlite3.unlock_notify() would put the system in a deadlocked state,
    // then SQLITE_LOCKED is returned and no unlock-notify callback is registered.
    // - The system is said to be in a deadlocked state if connection A has registered for an
    // unlock-notify callback on the conclusion of connection B's transaction, and connection B has
    // itself registered for an unlock-notify callback when connection A's transaction is concluded.
    // - Indirect deadlock is also detected, so the system is also considered to be deadlocked if
    // connection B has registered for an unlock-notify callback on the conclusion of connection
    // C's transaction, where connection C is waiting on connection A.
    // - Any number of levels of indirection are allowed.
    // - The "DROP TABLE" Exception:
    // - When a call to sqlite3.step() returns SQLITE_LOCKED, it is almost always appropriate to
    // call sqlite3.unlock_notify(). There is however, one exception:
    // - When executing a "DROP TABLE" or "DROP INDEX" statement, SQLite checks if there are any
    // currently executing SELECT statements that belong to the same connection.
    // - If there are, SQLITE_LOCKED is returned. In this case there is no "blocking connection",
    // so invoking sqlite3.unlock_notify() results in the unlock-notify callback being invoked immediately.
    // - If the application then re-attempts the "DROP TABLE" or "DROP INDEX" query, an infinite loop
    // might be the result.
    // - One way around this problem is to check the extended error code returned by an
    // sqlite3.step() call.
    // - If there is a blocking connection, then the extended error code is set to SQLITE_LOCKED_SHAREDCACHE.
    // - Otherwise, in the special "DROP TABLE/INDEX" case, the extended error code is just SQLITE_LOCKED.
    unlock_notify: function(pBlocked: TSqlite3DB; xNotify: TSqlUnlockNotify;
      pArg: Pointer): Pointer; cdecl;

    /// Register Data Change Notification Callbacks
    // - The sqlite3.update_hook() interface registers a callback function with
    // the database connection identified by the first argument to be invoked
    // whenever a row is updated, inserted or deleted.
    // - Any callback set by a previous call to this function for the same
    // database connection is overridden.
    // - sqlite3.update_hook(D,C,P) function returns the P argument from the
    // previous call on the same database connection D, or nil for the first
    // call on database connection D.
    // - The update hook is not invoked when internal system tables are modified
    // (i.e. sqlite_master and sqlite_sequence).
    // - In the current implementation, the update hook is not invoked when
    // duplication rows are deleted because of an ON CONFLICT REPLACE clause.
    // Nor is the update hook invoked when rows are deleted using the truncate
    // optimization. The exceptions defined in this paragraph might change in
    // a future release of SQLite.
    // - Note that you should also trace COMMIT and ROLLBACK commands (calling
    // sqlite3.commit_hook() and sqlite3.rollback_hook() functions) if you want to
    // ensure that the notified update was not canceled by a later Rollback.
    update_hook: function(DB: TSqlite3DB; xCallback: TSqlUpdateCallback;
      pArg: pointer): pointer; cdecl;

    /// Register Commit Notification Callbacks
    // - The sqlite3.commit_hook() interface registers a callback function to be
    // invoked whenever a transaction is committed.
    // - Any callback set by a previous call to sqlite3.commit_hook() for the same
    // database connection is overridden.
    // - Registering a nil function disables the Commit callback.
    // - The sqlite3.commit_hook(DB,C,P) function returns the P argument from the
    // previous call of the same function on the same database connection DB, or nil
    // for the first call for each function on DB.
    commit_hook: function(DB: TSqlite3DB; xCallback: TSqlCommitCallback;
      pArg: Pointer): Pointer; cdecl;

    // Register Rollback Notification Callbacks
    // - The sqlite3.rollback_hook() interface registers a callback function to be
    // invoked whenever a transaction is rolled back.
    // - Any callback set by a previous call to sqlite3.rollback_hook() for the same
    // database connection is overridden.
    // - Registering a nil function disables the Rollback callback.
    // - The sqlite3.rollback_hook(D,C,P) function returns the P argument from the
    // previous call of the same function on the same database connection D, or nil
    // for the first call for each function on D.
    rollback_hook: function(DB: TSqlite3DB;  xCallback: TSqlCommitCallback;
      pArg: Pointer): Pointer; cdecl;

    /// Count The Number Of Rows Modified
    // - This function returns the number of database rows that were changed or
    // inserted or deleted by the most recently completed SQL statement on the
    // database connection specified by the first parameter. Only changes that
    // are directly specified by the INSERT, UPDATE, or DELETE statement are counted.
    // Auxiliary changes caused by triggers or foreign key actions are not counted.
    // Use the sqlite3.total_changes() function to find the total number of changes
    // including changes caused by triggers and foreign key actions.
    // - If a separate thread makes changes on the same database connection while
    // sqlite3.changes() is running then the value returned is unpredictable and not
    // meaningful.
    changes: function(DB: TSqlite3DB): integer; cdecl;

    /// Total Number Of Rows Modified
    // - This function returns the number of row changes caused by INSERT, UPDATE or
    // DELETE statements since the database connection was opened. The count returned
    // by sqlite3.total_changes() includes all changes from all trigger contexts and
    // changes made by foreign key actions. However, the count does not include
    // changes used to implement REPLACE constraints, do rollbacks or ABORT
    // processing, or DROP TABLE processing. The count does not include rows of
    // views that fire an INSTEAD OF trigger, though if the INSTEAD OF trigger makes
    // changes of its own, those changes are counted. The sqlite3.total_changes()
    // function counts the changes as soon as the statement that makes them is
    // completed (when the statement handle is passed to sqlite3.reset()
    // or sqlite3.finalize()).
    // - If a separate thread makes changes on the same database connection while
    // sqlite3.total_changes() is running then the value returned is unpredictable
    // and not meaningful.
    total_changes: function(DB: TSqlite3DB): integer; cdecl;

    /// Compile a SQL query into byte-code
    // - SQL must contains an UTF-8 encoded null-terminated string query
    // - SQL_bytes contains -1 (to stop at the null char) or the number of bytes in
    // the input string, including the null terminator
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S will contain an handle of the resulting statement (an opaque sqlite3.stmt
    // object) on success, or will 0 on error - the calling procedure is responsible
    // for deleting the compiled SQL statement using sqlite3.finalize() after it has
    // finished with it
    // - in this "v2" interface, the prepared statement that is returned contains a
    // copy of the original SQL text
    // - this routine only compiles the first statement in SQL, so SQLtail is left pointing
    // to what remains uncompiled
    prepare_v2: function(DB: TSqlite3DB; SQL: PUtf8Char; SQL_bytes: integer;
      var S: TSqlite3Statement; var SQLtail: PUtf8Char): integer; cdecl;

    /// Compile a SQL query into byte-code
    // - SQL must contains an UTF-8 encoded null-terminated string query
    // - SQL_bytes contains -1 (to stop at the null char) or the number of bytes in
    // the input string, including the null terminator
    // - Return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S will contain an handle of the resulting statement (an opaque sqlite3.stmt
    // object) on success, or will 0 on error - the calling procedure is responsible
    // for deleting the compiled SQL statement using sqlite3.finalize() after it has
    // finished with it
    // - The prepared statement that is returned contains a copy of the original SQL text
    // - If the database schema changes, sqlite3.step() will automatically recompile the
    // SQL statement and try to run it again.
    // - As many as SQLITE_MAX_SCHEMA_RETRY retries will occur before sqlite3.step() gives
    // up and returns an error.
    // - If the specific value bound to a host parameter in the WHERE clause might influence
    // the choice of query plan for a statement, then the statement will be automatically
    // recompiled, as if there had been a schema change, on the first sqlite3.step() call
    // following any change to the bindings of that parameter.
    // - The specific value of a WHERE-clause parameter might influence the choice of query
    // plan if the parameter is the left-hand side of a LIKE or GLOB operator or if the parameter
    // is compared to an indexed column and the SQLITE_ENABLE_STAT4 compile-time option is enabled.
    // - This routine only compiles the first statement in SQL, so SQLtail is left pointing
    // to what remains uncompiled
    prepare_v3: function(DB: TSqlite3DB; SQL: PUtf8Char; SQL_bytes: integer;
      prepFlags: TSqlPrepareFlags; var S: TSqlite3Statement; var SQLtail: PUtf8Char): integer; cdecl;

    /// Delete a previously prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - this routine can be called at any point during the execution of the prepared
    //  statement. If the virtual machine has not completed execution when this routine
    //  is called, that is like encountering an error or an interrupt. Incomplete updates
    //  may be rolled back and transactions canceled, depending on the circumstances,
    //  and the error code returned will be SQLITE_ABORT
    finalize: function(S: TSqlite3Statement): integer; cdecl;

    /// Find the next prepared statement
    // - this interface returns a handle to the next prepared statement after S,
    // associated with the database connection DB.
    // - if S is 0 then this interface returns a pointer to the first prepared
    // statement associated with the database connection DB.
    // - if no prepared statement satisfies the conditions of this routine, it returns 0
    next_stmt: function(DB: TSqlite3DB; S: TSqlite3Statement): TSqlite3Statement; cdecl;

    /// Reset a prepared statement object back to its initial state, ready to be re-Prepared
    // - if the most recent call to sqlite3.step(S) returned SQLITE_ROW or SQLITE_DONE,
    // or if sqlite3.step(S) has never before been called with S, then sqlite3.reset(S)
    // returns SQLITE_OK.
    // - return an appropriate error code if the most recent call to sqlite3.step(S) failed
    // - any SQL statement variables that had values bound to them using the sqlite3.bind_*()
    // API retain their values. Use sqlite3.clear_bindings() to reset the bindings.
    reset: function(S: TSqlite3Statement): integer; cdecl;

    /// Returns true (non-zero) if the prepared statement S has been stepped at least once
    // using sqlite3.step(S) but has neither run to completion (returned SQLITE_DONE from
    // sqlite3.step(S)) nor been reset using sqlite3.reset(S).
    // - The sqlite3.stmt_busy(S) interface returns false if S is a nil pointer.
    // - If S is not a nil pointer and is not a pointer to a valid prepared statement object,
    // then the behavior is undefined and probably undesirable.
    // - This interface can be used in combination sqlite3.next_stmt() to locate all prepared
    // statements associated with a database connection that are in need of being reset.
    // - This can be used, for example, in diagnostic routines to search for prepared statements
    // that are holding a transaction open.
    stmt_busy: function(S: TSqlite3Statement): integer; cdecl;

    /// Returns 1 if the prepared statement S is an EXPLAIN statement,
    // or 2 if the statement S is an EXPLAIN QUERY PLAN,
    // or 0 if S is an ordinary statement or a nil pointer.
    stmt_isexplain: function(S: TSqlite3Statement): integer; cdecl;

    /// Returns true (non-zero) if and only if the prepared statement X
    // makes no direct changes to the content of the database file
    // - Transaction control statements such as BEGIN, COMMIT, ROLLBACK, SAVEPOINT,
    // and RELEASE cause sqlite3.stmt_readonly() to return true, since the statements
    // themselves do not actually modify the database but rather they control the
    // timing of when other statements modify the database. The ATTACH and DETACH
    // statements also cause sqlite3.stmt_readonly() to return true since, while
    // those statements change the configuration of a database connection, they
    // do not make changes to the content of the database files on disk.
    stmt_readonly: function(S: TSqlite3Statement): integer; cdecl;

    /// Returns information about the predicted and measured performance for pStmt.
    // - Advanced applications can use this interface to compare the predicted and
    // the measured performance and issue warnings and/or rerun ANALYZE if discrepancies
    // are found.
    // - idx identifies the specific loop to retrieve statistics for. Loops are numbered
    // starting from zero.
    // - iScanStatusOp determines which status information to return, and must be one of
    // the SQLITE_SCANSTAT_* options or the behavior of this interface is undefined
    // - The requested measurement is written into a variable pointed to by pOut.
    // - On invalid idx or not available statistics, returns non-zero and leave the variable
    // that pOut points to unchanged.
    // - When the value returned to pOut is a string, space to hold that string is managed
    // by the prepared statement S and will be automatically freed when S is finalized.
    stmt_scanstatus: function(S: TSqlite3Statement; idx: integer; iScanStatusOp: integer;
      pOut: pointer): integer; cdecl;

    /// Zero all sqlite3.stmt_scanstatus() related event counters.
    stmt_scanstatus_reset: procedure(S: TSqlite3Statement); cdecl;

    /// Retrieve and reset counter values from a prepared statement.
    // - Each prepared statement maintains various SQLITE_STMTSTATUS_* counters that
    // measure the number of times it has performed specific operations.
    // - These counters can be used to monitor the performance characteristics of
    // the prepared statements.
    // - For example, if the number of table steps greatly exceeds the number of table
    // searches or result rows, that would tend to indicate that the prepared statement
    // is using a full table scan rather than an index.
    // - Operation is an integer code for a specific SQLITE_STMTSTATUS_* counter to be interrogated.
    // - If the resetFlag is true, then the counter is reset to zero after this interface call returns.
    // - The result is the current value of the requested counter.
    stmt_status: function(S: TSqlite3Statement; Operation: integer; resetFlag: integer): integer; cdecl;

    /// Returns the database connection handle to which a prepared statement belongs.
    // - The database connection returned by sqlite3.db_handle is the same database connection
    // that was used to create the statement in the first place.
    db_handle: function(S: TSqlite3Statement): TSqlite3DB; cdecl;

    /// Returns a pointer to a copy of the UTF-8 SQL text used to create prepared statement P.
    // - The result is managed by SQLite and are automatically freed when the prepared statement is finalized.
    sql: function(S: TSqlite3Statement): PUtf8Char; cdecl;

    /// Returns a pointer to a UTF-8 string containing the SQL text of prepared statement P
    // with bound parameters expanded.
    // - Returns NULL if insufficient memory is available to hold the result, or if the result
    // would exceed the the maximum string length determined by the SQLITE_LIMIT_LENGTH.
    // - The result is obtained from sqlite3.malloc() and must be free by the application by
    // passing it to sqlite3.free_().
    expanded_sql: function(S: TSqlite3Statement): PUtf8Char; cdecl;

    /// Returns a pointer to a UTF-8 string containing the normalized SQL text of prepared statement P.
    // - The semantics used to normalize a SQL statement are unspecified and subject to change.
    // At a minimum, literal values will be replaced with suitable placeholders.
    // - The result is managed by SQLite and are automatically freed when the prepared statement is finalized.
    normalized_sql: function(S: TSqlite3Statement): PUtf8Char; cdecl;

    /// Evaluate An SQL Statement, returning a result status:
    // - SQLITE_BUSY means that the database engine was unable to acquire the database
    // locks it needs to do its job. If the statement is a COMMIT or occurs outside of
    // an explicit transaction, then you can retry the statement. If the statement
    // is not a COMMIT and occurs within a explicit transaction then you should
    // rollback the transaction before continuing.
    // - SQLITE_DONE means that the statement has finished executing successfully.
    // sqlite3.step() should not be called again on this virtual machine without
    // first calling sqlite3.reset() to reset the virtual machine state back.
    // - SQLITE_ROW is returned each time a new row of data is ready for processing by
    // the caller. The values may be accessed using the column access functions below.
    // sqlite3.step() has to be called again to retrieve the next row of data.
    // - SQLITE_MISUSE means that the this routine was called inappropriately. Perhaps
    // it was called on a prepared statement that has already been finalized or on
    // one that had previously returned SQLITE_ERROR or SQLITE_DONE. Or it could be
    // the case that the same database connection is being used by two or more threads
    // at the same moment in time.
    // - SQLITE_SCHEMA means that the database schema changes, and the SQL statement
    // has been recompiled and run again, but the schame changed in a way that makes
    // the statement no longer valid, as a fatal error.
    // - another specific error code is returned on fatal error
    step: function(S: TSqlite3Statement): integer; cdecl;

    /// Returns information about a column
    //- Returns SQLITE_OK and fills in the non-nil pointers in the final five arguments
    // with appropriate values if the specified column exists.
    // - Returns SQLITE_ERROR if the specified column does not exist.
    // - If the zColumnName is nil, then this routine simply checks for the existence of
    // the table and returns SQLITE_OK if the table exists and SQLITE_ERROR if it does not.
    // - If zTableName is nil then the result is undefined behavior.
    // - zDbName is either the name of the database (i.e. "main", "temp", or an
    // attached database) containing the specified table or NULL.
    // - If zDbName is nil, then all attached databases are searched for the table using the same
    // algorithm used by the database engine to resolve unqualified table references.
    // - The memory pointed to by the character pointers returned for the declaration type
    // and collation sequence is valid until the next call to any SQLite API function.
    // - If the specified table is actually a view, an error code is returned.
    // - If the specified column is "rowid", "oid" or "_rowid_" and the table is not a
    // WITHOUT ROWID table and an INTEGER PRIMARY KEY column has been explicitly declared,
    // then the output parameters are set for the explicitly declared column.
    // - If there is no INTEGER PRIMARY KEY column, then the outputs for the rowid are set as follows:
    //  data type: "INTEGER"
    //  collation sequence: "BINARY"
    //  not null: 0
    //  primary key: 1
    //  auto increment: 0
    // - This function causes all database schemas to be read from disk and parsed, if that has
    // not already been done, and returns an error if any errors are encountered while loading the schema.
    table_column_metadata: function(DB: TSqlite3DB; zDbName, zTableName,
      zColumnName: PUtf8Char; var pzDataType, pzCollSeq: PUtf8Char;
      var pNotNull, pPrimaryKey, pAutoinc: PInteger): integer; cdecl;

    /// Get the number of columns in the result set for the statement
    column_count: function(S: TSqlite3Statement): integer; cdecl;

    /// Datatype code for the initial data type of a result column
    // - returned value is one of SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT,
    // SQLITE_BLOB or SQLITE_NULL
    // - S is the SQL statement, after sqlite3.step(S) returned SQLITE_ROW
    // - Col is the column number, indexed from 0 to sqlite3.column_count(S)-1
    // - must be called before any sqlite3.column_*() statement, which may result in
    // an implicit type conversion: in this case, value is undefined
    column_type: function(S: TSqlite3Statement; Col: integer): integer; cdecl;

    /// Returns a zero-terminated UTF-8 string containing the declared datatype
    // of a result column
    column_decltype: function(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl;

    /// Returns the name of a result column as a zero-terminated UTF-8 string
    // - The returned string pointer is valid until either the prepared statement is
    // destroyed by sqlite3.finalize() or until the statement is automatically reprepared
    // by the first call to sqlite3.step() for a particular run or until the next call to
    // sqlite3.column_name() on the same column
    column_name: function(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl;

    /// Returns the original un-aliased database name that is the origin of a particular
    // result column in SELECT statement as a zero-terminated UTF-8 string.
    // - If the column returned by the statement is an expression or subquery and is
    // not a column value, then returns NULL.
    // Might also returns NULL if a memory allocation error occurs.
    // - The returned string pointer is valid until either the prepared statement is
    // destroyed by sqlite3.finalize() or until the statement is automatically reprepared
    // by the first call to sqlite3.step() for a particular run or until the same information
    // is requested again in a different encoding.
    column_database_name: function(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl;

    /// Returns the original un-aliased table name that is the origin of a particular
    // result column in SELECT statement as a zero-terminated UTF-8 string.
    // - If the column returned by the statement is an expression or subquery and is
    // not a column value, then returns NULL.
    // Might also returns NULL if a memory allocation error occurs.
    // - The returned string pointer is valid until either the prepared statement is
    // destroyed by sqlite3.finalize() or until the statement is automatically reprepared
    // by the first call to sqlite3.step() for a particular run or until the same information
    // is requested again in a different encoding.
    column_table_name: function(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl;

    /// Returns the original un-aliased origin name that is the origin of a particular
    // result column in SELECT statement as a zero-terminated UTF-8 string.
    // - If the column returned by the statement is an expression or subquery and is
    // not a column value, then returns NULL.
    // Might also returns NULL if a memory allocation error occurs.
    // - The returned string pointer is valid until either the prepared statement is
    // destroyed by sqlite3.finalize() or until the statement is automatically reprepared
    // by the first call to sqlite3.step() for a particular run or until the same information
    // is requested again in a different encoding.
    column_origin_name: function(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl;

    /// Number of bytes for a BLOB or UTF-8 string result
    // - S is the SQL statement, after sqlite3.step(S) returned SQLITE_ROW
    // - Col is the column number, indexed from 0 to sqlite3.column_count(S)-1
    // - an implicit conversion into UTF-8 text is made for a numeric value or
    // UTF-16 column: you must call sqlite3.column_text() or sqlite3.column_blob()
    // before calling sqlite3.column_bytes() to perform the conversion itself
    column_bytes: function(S: TSqlite3Statement; Col: integer): integer; cdecl;

    /// Get the value handle of the Col column in the current row of prepared statement S
    // - This handle represent a sqlite3.value object
    // - This handle can then be accessed with any sqlite3.value_*() function below
    // - This handle is unprotected and no mutex is held for it
    // - Unprotected TSqlite3Value objects may only be used as arguments to
    // sqlite3.result_value(), sqlite3.bind_value(), and sqlite3_value_dup()
    // - For other sqlite3.value_*() in a multithreaded use, protected values must be used
    // - The sqlite3.value_dup() interface can be used to construct a new protected value
    column_value: function(S: TSqlite3Statement; Col: integer): TSqlite3Value; cdecl;

    /// Converts the Col column in the current row prepared statement S
    // into a floating point value and returns a copy of that value
    // - NULL is converted into 0.0
    // - INTEGER is converted into corresponding floating point value
    // - TEXT or BLOB is converted from all correct ASCII numbers with 0.0 as default
    column_double: function(S: TSqlite3Statement; Col: integer): double; cdecl;

    /// Converts the Col column in the current row prepared statement S
    // into a 32 bit integer value and returns a copy of that value
    // - NULL is converted into 0
    // - FLOAT is truncated into corresponding integer value
    // - TEXT or BLOB is converted from all correct ASCII numbers with 0 as default
    column_int: function(S: TSqlite3Statement; Col: integer): integer; cdecl;

    /// Converts the Col column in the current row prepared statement S
    // into a 64 bit integer value and returns a copy of that value
    // - NULL is converted into 0
    // - FLOAT is truncated into corresponding integer value
    // - TEXT or BLOB is converted from all correct ASCII numbers with 0 as default
    column_int64: function(S: TSqlite3Statement; Col: integer): int64; cdecl;

    /// Converts the Col column in the current row prepared statement S
    // into a zero-terminated UTF-8 string and returns a pointer to that string
    // - NULL is converted into nil
    // - INTEGER or FLOAT are converted into ASCII rendering of the numerical value
    // - TEXT is returned directly (with UTF-16 -> UTF-8 encoding if necessary)
    // - BLOB add a zero terminator if needed
    column_text: function(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl;

    /// Converts the Col column in the current row prepared statement S
    // into a zero-terminated UTF-16 string and returns a pointer to that string
    // - NULL is converted into nil
    // - INTEGER or FLOAT are converted into ASCII rendering of the numerical value
    // - TEXT is returned directly (with UTF-8 -> UTF-16 encoding if necessary)
    // - BLOB add a zero terminator if needed
    column_text16: function(S: TSqlite3Statement; Col: integer): PWideChar; cdecl;

    /// Converts the Col column in the current row of prepared statement S
    // into a BLOB and then returns a pointer to the converted value
    // - NULL is converted into nil
    // - INTEGER or FLOAT are converted into ASCII rendering of the numerical value
    // - TEXT and BLOB are returned directly
    column_blob: function(S: TSqlite3Statement; Col: integer): PUtf8Char; cdecl;

    /// Datatype code for a sqlite3.value object, specified by its handle
    // - returned value is one of SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT,
    // SQLITE_BLOB or SQLITE_NULL
    // - must be called before any sqlite3.value_*() statement, which may result in
    // an implicit type conversion: in this case, value is undefined
    value_type: function(Value: TSqlite3Value): integer; cdecl;

    /// Returns the subtype for an application-defined SQL function argument Value.
    // - The subtype information can be used to pass a limited amount of context from
    // one SQL function to another. Use the sqlite3.result_subtype() routine to set
    // the subtype for the return value of an SQL function.
    value_subtype: function(Value: TSqlite3Value): cardinal; cdecl;

    /// Attempts to apply numeric affinity to the value
    // - This means that an attempt is made to convert the value to an integer or
    // floating point. If such a conversion is possible without loss of information
    // (in other words, if the value is a string that looks like a number) then the
    // conversion is performed. Otherwise no conversion occurs. The datatype after
    // conversion is returned.
    // - returned value is one of SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT,
    // SQLITE_BLOB or SQLITE_NULL
    value_numeric_type: function(Value: TSqlite3Value): integer; cdecl;

    /// Within the xUpdate method of a virtual table, returns true if and only if
    // the column corresponding to Value is unchanged by the UPDATE operation that
    // the xUpdate method call was invoked to implement and if and the prior xColumn
    // method call that was invoked to extracted the value for that column returned
    // without setting a result (probably because it queried sqlite3.vtab_nochange()
    // and found that the column was unchanging).
    // - Within an xUpdate method, any value for which sqlite3.value_nochange() is
    // true will in all other respects appear to be a NULL value. If is invoked
    // anywhere other than within an xUpdate method call for an UPDATE statement,
    // then the return value is arbitrary and meaningless.
    value_nochange: function(Value: TSqlite3Value): integer; cdecl;

    /// Returns non-zero if the Value originated from one of the sqlite3.bind() interfaces.
    // - If Value comes from an SQL literal value, or a table column, or an expression,
    // then returns zero.
    value_frombind: function(Value: TSqlite3Value): integer; cdecl;

    /// Number of bytes for a sqlite3.value object, specified by its handle
    // - used after a call to sqlite3.value_text() or sqlite3.value_blob()
    //  to determine buffer size (in bytes)
    value_bytes: function(Value: TSqlite3Value): integer; cdecl;

    /// Makes a copy of the sqlite3_value object D and returns a pointer to that copy.
    // - The result is a protected object even if the input is not.
    // - Returns NULL if V is NULL or if a memory allocation fails.
    value_dup: function(Value: TSqlite3Value): TSqlite3Value; cdecl;

    ///  Frees an sqlite3_value object previously obtained from sqlite3.value_dup().
    // - If V is a NULL pointer then sqlite3_value_free(V) is a harmless no-op.
    value_free: procedure(Value: TSqlite3Value); cdecl;

    /// If Value object V was initialized using sqlite3.bind_pointer(S,I,P,X,D)
    // or sqlite3.result_pointer(C,P,X,D) and if X and Y are strings that compare equal
    // according to strcmp(X,Y), then sqlite3.value_pointer(V,Y) will return the pointer P.
    // Otherwise, sqlite3.value_pointer(V,Y) returns a NULL.
    value_pointer: function(Value: TSqlite3Value; Typ: PUtf8Char): pointer; cdecl;

    /// Converts a sqlite3.value object, specified by its handle,
    // into a floating point value and returns a copy of that value
    value_double: function(Value: TSqlite3Value): double; cdecl;

    /// Converts a sqlite3.value object, specified by its handle,
    // into an integer value and returns a copy of that value
    value_int64: function(Value: TSqlite3Value): Int64; cdecl;

    /// Converts a sqlite3.value object, specified by its handle,
    // into an UTF-8 encoded string, and returns a copy of that value
    value_text: function(Value: TSqlite3Value): PUtf8Char; cdecl;

    /// Converts a sqlite3.value object, specified by its handle,
    // into a blob memory, and returns a copy of that value
    value_blob: function(Value: TSqlite3Value): pointer; cdecl;

    /// Add SQL functions or aggregates or to redefine the behavior of existing
    // SQL functions or aggregates
    // - The first parameter is the database connection to which the SQL function is
    // to be added. If an application uses more than one database connection then
    // application-defined SQL functions must be added to each database connection
    // separately.
    // - The second parameter is the name of the SQL function to be created or redefined.
    // The length of the name is limited to 255 bytes in a UTF-8 representation,
    // exclusive of the zero-terminator. Note that the name length limit is in
    // UTF-8 bytes, not characters nor UTF-16 bytes. Any attempt to create a
    // function with a longer name will result in SQLITE_MISUSE being returned.
    // - The third parameter (nArg) is the number of arguments that the SQL
    // function or aggregate takes. If this parameter is -1, then the SQL
    // function or aggregate may take any number of arguments between 0 and the
    // SQLITE_LIMIT_FUNCTION_ARG current limit. If the third parameter is less
    // than -1 or greater than 127 then the behavior is undefined.
    // - The fourth parameter, eTextRep, specifies what text encoding this SQL
    // function prefers for its parameters. Every SQL function implementation must
    // be able to work with UTF-8, UTF-16le, or UTF-16be. But some implementations
    // may be more efficient with one encoding than another. When multiple
    // implementations of the same function are available, SQLite will pick the one
    // that involves the least amount of data conversion. If there is only a single
    // implementation which does not care what text encoding is used, then the
    // fourth argument should be SQLITE_ANY.
    // - The fifth parameter, pApp, is an arbitrary pointer. The implementation
    // of the function can gain access to this pointer using sqlite3.user_data().
    // - The seventh, eighth and ninth parameters, xFunc, xStep and xFinal, are
    // pointers to C-language functions that implement the SQL function or aggregate.
    // A scalar SQL function requires an implementation of the xFunc callback only;
    // nil pointers must be passed as the xStep and xFinal parameters. An aggregate
    // SQL function requires an implementation of xStep and xFinal and nil pointer
    // must be passed for xFunc. To delete an existing SQL function or aggregate,
    // pass nil pointers for all three function callbacks.
    // - It is permitted to register multiple implementations of the same functions
    // with the same name but with either differing numbers of arguments or
    // differing preferred text encodings. SQLite will use the implementation
    // that most closely matches the way in which the SQL function is used.
    create_function: function(DB: TSqlite3DB; FunctionName: PUtf8Char;
      nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSqlFunctionFunc;
      xFinal: TSqlFunctionFinal): integer; cdecl;

    /// Add SQL functions or aggregates or to redefine the behavior of existing
    // SQL functions or aggregates, including destruction
    // - if the additinal xDestroy parameter is not nil, then it is invoked when
    // the function is deleted, either by being overloaded or when the database
    // connection closes.
    // - When the destructure callback of the tenth parameter is invoked, it is
    // passed a single argument which is a copy of the pointer which was the fifth
    // parameter to sqlite3.create_function_v2().
    // - this function is not available in older revisions - e.g. 3.6.*
    create_function_v2: function(DB: TSqlite3DB; FunctionName: PUtf8Char;
      nArg, eTextRep: integer; pApp: pointer; xFunc, xStep: TSqlFunctionFunc;
      xFinal: TSqlFunctionFinal; xDestroy: TSqlDestroyPtr): integer; cdecl;

    /// Add SQL functions or aggregates or to redefine the behavior of existing
    // SQL functions or aggregates, including  extra callback functions needed
    // by aggregate window functions
    // - see https://www.sqlite.org/windowfunctions.html#aggregate_window_functions
    // - sixth, seventh, eighth and ninth parameters (xStep, xFinal, xValue
    // and xInverse) passed to this function are pointers to callbacks that
    // implement the new aggregate window function. xStep and xFinal must both
    // be non-nil. xValue and xInverse may either both be nil, in which case a
    // regular aggregate function is created, or must both be non-nil, in which
    // case the new function may be used as either an aggregate or aggregate
    // window function
    // - this function is not available in older revisions, i.e. before 3.25.2
    create_window_function: function(DB: TSqlite3DB; FunctionName: PUtf8Char;
      nArg, eTextRep: integer; pApp: pointer; xStep: TSqlFunctionFunc;
      xFinal, xValue: TSqlFunctionFinal; xInverse: TSqlFunctionFunc;
      xDestroy: TSqlDestroyPtr): integer; cdecl;

    /// Saves Value as metadata for the N-th argument of the application-defined function.
    // This function may be used by (non-aggregate) SQL functions to associate metadata with argument values.
    // - If the same value is passed to multiple invocations of the same SQL function during query execution,
    // under some circumstances the associated metadata may be preserved.
    // - An example of where this might be useful is in a regular-expression matching function.
    // - The compiled version of the regular expression can be stored as metadata associated with the pattern
    // string. Then as long as the pattern string remains the same, the compiled regular expression can be
    // reused on multiple invocations of the same function.
    // - Subsequent calls to sqlite3.get_auxdata() return Value from the most recent
    // sqlite3.set_auxdata() call if the metadata is still valid or nil if the metadata has been discarded.
    // - DestroyPtr is either a nil pointer or a pointer to a destructor function for Value.
    // - After each call to sqlite3.set_auxdata where DestroyPtr is not nil, SQLite will invoke the
    // destructor function with parameter Value exactly once, when the metadata is discarded.
    // - SQLite is free to discard the metadata at any time, including:
    //  - when the corresponding function parameter changes, or
    //  - when sqlite3.reset() or sqlite3.finalize() is called for the SQL statement, or
    //  - when sqlite3.set_auxdata() is invoked again on the same parameter, or
    //  - during the original sqlite3.set_auxdata() call when a memory allocation error occurs.
    // - Note the last bullet in particular. The DestroyPtr might be called immediately, before
    // the sqlite3.set_auxdata() interface even returns.
    // - Hence sqlite3.set_auxdata() should be called near the end of the function implementation
    // and the function implementation should not make any use of Value after sqlite3.set_auxdata()
    // has been called.
    // In practice, metadata is preserved between function calls for function parameters that are
    // compile-time constants, including literal values and parameters and expressions composed
    // from the same.
    // - The value of the N parameter to these interfaces should be non-negative.
    // - Future enhancements may make use of negative N values to define new kinds of function
    // caching behavior.
    // - These routines must be called from the same thread in which the SQL function is running.
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    // or to @sqlite3InternalFreeObject if Value must be released via a Free method
    set_auxdata: procedure(Context: TSqlite3FunctionContext; N: integer;
      Value: pointer; DestroyPtr: TSqlDestroyPtr); cdecl;

    /// Returns a pointer to the metadata associated by the sqlite3.set_auxdata() function with
    // the Nth argument value to the application-defined function.
    // - N is zero for the left-most function argument.
    // - If there is no metadata associated with the function argument, the sqlite3.get_auxdata
    // interface returns a nil pointer.
    get_auxdata: function(Context: TSqlite3FunctionContext; N: integer): pointer; cdecl;

    /// Sets the result to an SQL NULL value, just like sqlite3.result_null,
    // except that it also associates the host-language pointer Value or type T
    // with that NULL value such that the pointer can be retrieved within an
    // application-defined SQL function using sqlite3.value_pointer().
    // - Typ parameter should be a static string, preferably a string literal
    // - DestroyPtr is either a nil pointer or a pointer to a destructor function for Value.
    // - SQLite will invoke the destructor DestroyPtr with a single argument of Value when it is finished using Value.
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    // or to @sqlite3InternalFreeObject if Value must be released via a Free method
    result_pointer: procedure(Context: TSqlite3FunctionContext;
      Value: pointer; Typ: PUtf8Char; DestroyPtr: TSqlDestroyPtr); cdecl;

    /// Sets the return value of the application-defined function to be NULL
    result_null: procedure(Context: TSqlite3FunctionContext); cdecl;

    /// Sets the return value of the application-defined function to be the 64-bit
    // signed integer value given in the 2nd argument
    result_int64: procedure(Context: TSqlite3FunctionContext; Value: Int64); cdecl;

    /// Sets the result from an application-defined function to be a floating point
    // value specified by its 2nd argument
    result_double: procedure(Context: TSqlite3FunctionContext; Value: double); cdecl;

    /// Sets the result from an application-defined function to be the BLOB
    // - content is pointed to by the Value and which is Value_bytes bytes long
    // - set DestroyPtr as SQLITE_STATIC (nil) for static binding
    // - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    // copy of the data (this is the prefered way in our Framework)
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    // or to @sqlite3InternalFreeObject if Value must be released via a Free method
    result_blob: procedure(Context: TSqlite3FunctionContext;
      Value: Pointer; Value_bytes: integer = 0;
      DestroyPtr: TSqlDestroyPtr = SQLITE_TRANSIENT); cdecl;

    /// Set the result of the application-defined function to be a BLOB containing all
    // zero bytes and Value_bytes in size.
    result_zeroblob: procedure(Context: TSqlite3FunctionContext; Value_bytes: integer); cdecl;

    /// Sets the return value of the application-defined function to be a text string
    // which is represented as UTF-8
    // - if Value_bytes is negative, then SQLite takes result text from the Value
    // parameter through the first zero character
    // - if Value_bytes is non-negative, then as many bytes (NOT characters: this
    // parameter must include the #0 terminator) of the text pointed to by the
    // Value parameter are taken as the application-defined function result
    // - set DestroyPtr as SQLITE_STATIC (nil) for static binding
    // - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    // copy of the data (this is the prefered way in our Framework)
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    // or to @sqlite3InternalFreeObject if Value must be released via a Free method
    result_text: procedure(Context: TSqlite3FunctionContext;
      Value: PUtf8Char; Value_bytes: integer = -1;
      DestroyPtr: TSqlDestroyPtr = SQLITE_TRANSIENT); cdecl;

    /// Sets the result of the application-defined function to be a copy the unprotected
    // sqlite3.value object specified by the 2nd parameter
    // - The sqlite3.result_value() interface makes a copy of the sqlite3.value so
    // that the sqlite3.value specified in the parameter may change or be deallocated
    // after sqlite3.result_value() returns without harm
    result_value: procedure(Context: TSqlite3FunctionContext; Value: TSqlite3Value); cdecl;

    /// Causes the subtype of the result from the application-defined SQL function with
    // Context to be the Value.
    // - Only the lower 8 bits of the subtype T are preserved in current versions of SQLite;
    // higher order bits are discarded.
    result_subtype: procedure(Context: TSqlite3FunctionContext; Value: cardinal); cdecl;

    /// Cause the implemented SQL function to throw an exception
    // - SQLite interprets the error message string from sqlite3.result_error() as UTF-8
    // - if MsgLen is negative, Msg must be #0 ended, or MsgLen must tell the numnber of
    // characters in the Msg UTF-8 buffer
    result_error: procedure(Context: TSqlite3FunctionContext;
      Msg: PUtf8Char; MsgLen: integer = -1); cdecl;

    /// Returns a copy of the pointer that was the pUserData parameter (the 5th
    // parameter) of the sqlite3.create_function() routine that originally
    // registered the application defined function
    // - This routine must be called from the same thread in which the
    // application-defined function is running
    user_data: function(Context: TSqlite3FunctionContext): pointer; cdecl;

    /// Returns a copy of the pointer to the database connection (the 1st parameter)
    // of the sqlite3.create_function() routine that originally registered the
    // application defined function
    context_db_handle: function(Context: TSqlite3FunctionContext): TSqlite3DB; cdecl;

    /// Implementations of aggregate SQL functions use this routine to allocate
    // memory for storing their state.
    // - The first time the sqlite3.aggregate_context(C,N) routine is called for a
    // particular aggregate function, SQLite allocates N of memory, zeroes out that
    // memory, and returns a pointer to the new memory. On second and subsequent calls
    // to sqlite3.aggregate_context() for the same aggregate function instance, the
    // same buffer is returned. sqlite3.aggregate_context() is normally called once
    // for each invocation of the xStep callback and then one last time when the
    // xFinal callback is invoked. When no rows match an aggregate query, the xStep()
    // callback of the aggregate function implementation is never called and xFinal()
    // is called exactly once. In those cases, sqlite3.aggregate_context() might be
    // called for the first time from within xFinal().
    // - The sqlite3.aggregate_context(C,N) routine returns a nil pointer if N is
    // less than or equal to zero or if a memory allocate error occurs.
    // - The amount of space allocated by sqlite3.aggregate_context(C,N) is
    // determined by the N parameter on first successful call. Changing the value
    // of N in subsequent call to sqlite3.aggregate_context() within the same
    // aggregate function instance will not resize the memory allocation.
    // - SQLite automatically frees the memory allocated by sqlite3.aggregate_context()
    // when the aggregate query concludes.
    aggregate_context: function(Context: TSqlite3FunctionContext;
       nBytes: integer): pointer; cdecl;

    /// Bind a Text Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set. The leftmost SQL parameter
    // has an index of 1.
    // - Text must contains an UTF-8 encoded null-terminated string query
    // - Text_bytes contains -1 (to stop at the null char) or the number of chars
    // in the input string, excluding the null terminator
    // - set DestroyPtr as SQLITE_STATIC (nil) for static binding
    // - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    // copy of the data (this is the prefered way in our Framework)
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    bind_text: function(S: TSqlite3Statement;
      Param: integer; Text: PUtf8Char; Text_bytes: integer = -1;
      DestroyPtr: TSqlDestroyPtr = SQLITE_TRANSIENT): integer; cdecl;
      // note that the official SQLite3 documentation could lead into misunderstanding:
      // Text_bytes must EXCLUDE the null terminator, otherwise a #0 is appended to
      // all column values

    /// Bind a Blob Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    // - Buf must point to a memory buffer of Buf_bytes bytes
    // - Buf_bytes contains the number of bytes in Buf
    // - set DestroyPtr as SQLITE_STATIC (nil) for static binding
    // - set DestroyPtr to SQLITE_TRANSIENT (-1) for SQLite to make its own private
    // copy of the data (this is the prefered way in our Framework)
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    bind_blob: function(S: TSqlite3Statement;
      Param: integer; Buf: pointer; Buf_bytes: integer;
      DestroyPtr: TSqlDestroyPtr = SQLITE_TRANSIENT): integer; cdecl;

    /// Bind a ZeroBlob buffer to a parameter
    // - uses a fixed amount of memory (just an integer to hold its size) while
    // it is being processed. Zeroblobs are intended to serve as placeholders
    // for BLOBs whose content is later written using incremental BLOB I/O routines.
    // - a negative value for the Size parameter results in a zero-length BLOB
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    bind_zeroblob: function(S: TSqlite3Statement;
      Param: integer; Size: integer): integer; cdecl;

    /// Bind a floating point Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    // - Value is the floating point number to bind
    bind_double: function(S: TSqlite3Statement;
      Param: integer; Value: double): integer; cdecl;

    /// Bind a 32 bits integer Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    // - Value is the 32 bits integer to bind
    bind_int: function(S: TSqlite3Statement;
      Param: integer; Value: integer): integer; cdecl;

    /// Bind a 64 bits integer Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    // - Value is the 64 bits integer to bind
    bind_int64: function(S: TSqlite3Statement;
      Param: integer; Value: Int64): integer; cdecl;

    /// Bind a NULL Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    bind_null: function(S: TSqlite3Statement;
      Param: integer): integer; cdecl;

    /// Bind a NULL Value to a parameter of a prepared statement but associate it with the pointer Value of type Typ
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    // - Typ parameter should be a static string, preferably a string literal
    // - DestroyPtr is either a nil pointer or a pointer to a destructor function for Value.
    // - SQLite will invoke the destructor DestroyPtr with a single argument of Value when it is finished using Value.
    // - set DestroyPtr to @sqlite3InternalFree if Value must be released via Freemem()
    // or to @sqlite3InternalFreeObject if Value must be released via a Free method
    bind_pointer: function(S: TSqlite3Statement;
      Param: integer; Value: pointer; Typ: PUtf8Char;
      DestroyPtr: TSqlDestroyPtr): integer; cdecl;

    /// Bind a TSqlite3Value Value to a parameter of a prepared statement
    // - return SQLITE_OK on success or an error code - see SQLITE_* and sqlite3.errmsg()
    // - S is a statement prepared by a previous call to sqlite3.prepare_v2()
    // - Param is the index of the SQL parameter to be set (leftmost=1)
    bind_value: function(S: TSqlite3Statement;
      Param: integer; Value: TSqlite3Value): integer; cdecl;

    /// Reset All Bindings On A Prepared Statement
    // - the reset() API doesn't clear the binding
    clear_bindings: function(S: TSqlite3Statement): integer; cdecl;

    /// Number Of SQL Parameters for a prepared statement
    // - returns the index of the largest (rightmost) parameter. For all forms
    // except ?NNN, this will correspond to the number of unique parameters.
    // - If parameters of the ?NNN type are used, there may be gaps in the list.
    bind_parameter_count: function(S: TSqlite3Statement): integer; cdecl;

    /// Returns the index of an SQL parameter given its name.
    // - The index value returned is suitable for use as the second parameter to sqlite3.bind()
    // - A zero is returned if no matching parameter is found
    bind_parameter_index: function(S: TSqlite3Statement; ParamName: PUtf8Char): integer; cdecl;

    /// Returns the name of the N-th SQL parameter in the prepared statement S.
    // - SQL parameters of the form "?NNN" or ":AAA" or "@AAA" or "$AAA" have a name which is
    // the string "?NNN" or ":AAA" or "@AAA" or "$AAA" respectively. In other words, the initial
    // ":" or "$" or "@" or "?" is included as part of the name. Parameters of the form "?" without
    // a following integer have no name and are referred to as "nameless" or "anonymous parameters".
    // If the value N is out of range or if the N-th parameter is nameless, then NULL is returned.
    bind_parameter_name: function(S: TSqlite3Statement; Param: integer): PUtf8Char; cdecl;

    /// Open a BLOB For Incremental I/O
    // - returns a BLOB handle for row RowID, column ColumnName, table TableName
    // in database DBName; in other words, the same BLOB that would be selected by:
    // ! SELECT ColumnName FROM DBName.TableName WHERE rowid = RowID;
    blob_open: function(DB: TSqlite3DB; DBName, TableName, ColumnName: PUtf8Char;
      RowID: Int64; Flags: integer; var Blob: TSqlite3Blob): integer; cdecl;

    /// Move a BLOB Handle to a New Row
    // - will point to a different row of the same database table
    // - this is faster than closing the existing handle and opening a new one
    blob_reopen: function(Blob: TSqlite3Blob; RowID: Int64): integer; cdecl;

    /// Close A BLOB Handle
    blob_close: function(Blob: TSqlite3Blob): integer; cdecl;

    /// Read Data From a BLOB Incrementally
    blob_read: function(Blob: TSqlite3Blob;
      const Data; Count, Offset: integer): integer; cdecl;

    /// Write Data To a BLOB Incrementally
    blob_write: function(Blob: TSqlite3Blob;
      const Data; Count, Offset: integer): integer; cdecl;

    /// Return The Size Of An Open BLOB
    blob_bytes: function(Blob: TSqlite3Blob): integer; cdecl;

    /// Define New Collating Sequences
    // - add new collation sequences to the database connection specified
    // - collation name is to be used in CREATE TABLE t1 (a COLLATE CollationName);
    // or in SELECT * FROM t1 ORDER BY c COLLATE CollationName;
    // - StringEncoding is either SQLITE_UTF8 either SQLITE_UTF16
    // - TSqlDataBase.Create add WIN32CASE, WIN32NOCASE and ISO8601 collations
    create_collation: function(DB: TSqlite3DB; CollationName: PUtf8Char;
      StringEncoding: integer; CollateParam: pointer;
      cmp: TSqlCollateFunc): integer; cdecl;

    /// Define New Collating Sequences
    // - add new collation sequences to the database connection specified
    // - collation name is to be used in CREATE TABLE t1 (a COLLATE CollationName);
    // or in SELECT * FROM t1 ORDER BY c COLLATE CollationName;
    // - StringEncoding is either SQLITE_UTF8 either SQLITE_UTF16
    // - TSqlDataBase.Create add WIN32CASE, WIN32NOCASE and ISO8601 collations
    // - set DestroyPtr to @sqlite3InternalFree if CollateParam must be released via Freemem()
    // or to @sqlite3InternalFreeObject if CollateParam must be released via a Free method
    // - The DestroyPtr callback is NOT called if the sqlite3.create_collation_v2() function
    // fails. Applications that invoke sqlite3.create_collation_v2() with a non-nil DestroyPtr
    // argument should check the return code and dispose of the application data pointer
    // themselves rather than expecting SQLite to deal with it for them.
    create_collation_v2: function(DB: TSqlite3DB; CollationName: PUtf8Char;
      StringEncoding: integer; CollateParam: pointer;
      cmp: TSqlCollateFunc; DestroyPtr: TSqlDestroyPtr): integer; cdecl;

    /// To avoid having to register all collation sequences before a database can be used,
    // a single callback function may be registered with the database connection to be
    // invoked whenever an undefined collation sequence is required.
    // - If the function is registered using the sqlite3.collation_needed() API, then it is
    // passed the names of undefined collation sequences as strings encoded in UTF-8.
    // - A call to the function replaces the existing collation-needed callback.
    // The callback function should register the desired collation using sqlite.create_collation()
    collation_needed: function(DB: TSqlite3DB; CollateParam: pointer;
      Callback: TSqlCollationNeededCallback): integer; cdecl;

    /// Used to register a new virtual table module name
    // - The module name is registered on the database connection specified by the
    // first DB parameter.
    // - The name of the module is given by the second parameter.
    // - The third parameter is a pointer to the implementation of the virtual table
    // module.
    // - The fourth parameter is an arbitrary client data pointer that is passed
    // through into the xCreate and xConnect methods of the virtual table module
    // when a new virtual table is be being created or reinitialized.
    // - The fifth parameter can be used to specify a custom destructor for the
    // pClientData buffer. SQLite will invoke the destructor function (if it is
    // not nil) when SQLite no longer needs the pClientData pointer. The
    // destructor will also be invoked if call to sqlite3.create_module_v2() fails.
    create_module_v2: function(DB: TSqlite3DB;
      const zName: PUtf8Char; var p: TSqlite3Module;
      pClientData: Pointer; xDestroy: TSqlDestroyPtr): integer; cdecl;

    /// Removes all virtual table modules from database connection DB except those named
    // on list azKeep.
    // - The azKeep parameter must be either nil or a pointer to an array of pointers
    // to strings where the array is terminated by a single nil pointer.
    // - If the azKeep parameter is nil, then all virtual table modules are removed.
    drop_modules: function(DB: TSqlite3DB; azKeep: PUtf8Char): integer; cdecl;

    /// Declare the Schema of a virtual table
    // - The xCreate() and xConnect() methods of a virtual table module call this
    // interface to declare the format (the names and datatypes of the columns) of
    // the virtual tables they implement. The string can be deallocated and/or reused
    // as soon as the sqlite3.declare_vtab() routine returns.
    // - If a column datatype contains the special keyword "HIDDEN" (in any
    // combination of upper and lower case letters) then that keyword it is omitted
    // from the column datatype name and the column is marked as a hidden column
    // internally. A hidden column differs from a normal column in three respects:
    // 1. Hidden columns are not listed in the dataset returned by "PRAGMA table_info",
    // 2. Hidden columns are not included in the expansion of a "*" expression in
    // the result set of a SELECT, and 3. Hidden columns are not included in the
    // implicit column-list used by an INSERT statement that lacks an explicit
    // column-list.
    declare_vtab: function(DB: TSqlite3DB; const zSQL: PUtf8Char): integer; cdecl;

    /// Determine The Collation For a Virtual Table Constraint
    // - This function may only be called from within a call to the xBestIndex method of
    // a virtual table.
    // - IndexInfo must be the TSqlite3IndexInfo object that is the first parameter
    // to the xBestIndex() method.
    // - Index be an index into the aConstraint[] array belonging to the
    // TSqlite3IndexInfo structure passed to xBestIndex.
    // - Returns a pointer to a buffer containing the name of the collation sequence
    // for the corresponding constraint.
    vtab_collation: function(var IndexInfo: TSqlite3IndexInfo;
      Index: integer): PUtf8Char; cdecl;

    /// Virtual Table Interface Configuration
    // - This function may be called by either the xConnect or xCreate method of a virtual
    // table implementation to configure various facets of the virtual table interface.
    // - If this interface is invoked outside the context of an xConnect or xCreate virtual
    // table method then the behavior is undefined.
    // - op is one of the virtual table configuration options.
    // - The presence and meaning of parameters after op depend on which virtual table
    // configuration option is used.
    vtab_config: function(DB: TSqlite3DB; op: integer): integer; cdecl;

    /// Determine If Virtual Table Column Access Is For UPDATE
    // - If the sqlite3.vtab_nochange() routine is called within the xColumn method of a
    // virtual table, then it might return true if the column is being fetched as part of
    // an UPDATE operation during which the column value will not change.
    // - The virtual table implementation can use this hint as permission to substitute a
    // return value that is less expensive to compute and that the corresponding xUpdate
    // method understands as a "no-change" value.
    // - If the xColumn method calls sqlite3.vtab_nochange() and finds that the column is
    // not changed by the UPDATE statement, then the xColumn method can optionally return
    // without setting a result, without calling any of the sqlite3.result_*() interfaces.
    // - In that case, sqlite3.value_nochange() will return true for the same column in
    // the xUpdate method.
    // - The sqlite3.vtab_nochange() routine is an optimization.
    // - Virtual table implementations should continue to give a correct answer even if
    // the sqlite3.vtab_nochange() interface were to always return false.
    // In the current implementation, the sqlite3.vtab_nochange() interface does always
    // returns false for the enhanced UPDATE FROM statement.
    vtab_nochange: function(Context: TSqlite3FunctionContext): integer; cdecl;

    /// Determine The Virtual Table Conflict Policy
    // - This function may only be called from within a call to the xUpdate method of a
    // virtual table implementation for an INSERT or UPDATE operation.
    // - The value returned is one of SQLITE_ROLLBACK, SQLITE_IGNORE, SQLITE_FAIL,
    // SQLITE_ABORT, or SQLITE_REPLACE, according to the ON CONFLICT mode of the SQL
    // statement that triggered the call to the xUpdate method of the virtual table.
    vtab_on_conflict: function(DB: TSqlite3DB): integer; cdecl;

    /// Overload A Function For A Virtual Table
    // - Virtual tables can provide alternative implementations of functions using the
    // xFindFunction method of the virtual table module.
    // - But global versions of those functions must exist in order to be overloaded.
    // - This API makes sure a global version of a function with a particular name and
    // number of parameters exists.
    // - If no such function exists before this API is called, a new function is created.
    // - The implementation of the new function always causes an exception to be thrown.
    // - So the new function is not good for anything by itself.
    // - Its only purpose is to be a placeholder function that can be overloaded by a
    // virtual table.
    overload_function: function(DB: TSqlite3DB; zFuncName: PUtf8Char;
      nArg: integer): integer; cdecl;

    /// Automatically Load Statically Linked Extensions
    // - Register the xEntryPoint() function to be invoked for each new database
    // connection that is created.
    // - The idea here is that xEntryPoint() is the entry point for a statically
    // linked SQLite extension that is to be automatically loaded into all new
    // database connections.
    // - Calling sqlite3.auto_extension(X) with an entry point X that is already
    // on the list of automatic extensions is a harmless no-op.
    // - No entry point will be called more than once for each database connection
    // that is opened.
    auto_extension: function(xEntryPoint: TSqlEntryPointCallback): integer; cdecl;

    /// Unregisters the initialization routine xEntryPoint that was registered using a prior
    // call to sqlite3.auto_extension().
    //- Returns 1 if initialization routine xEntryPoint was successfully unregistered and it
    // returns 0 if xEntryPoint was not on the list of initialization routines.
    cancel_auto_extension: function(xEntryPoint: TSqlEntryPointCallback): integer; cdecl;

    /// Disables all automatic extensions previously registered using sqlite3.auto_extension().
    reset_auto_extension: procedure; cdecl;

    /// Loads an SQLite extension library from the named file.
    // - Attempts to load an SQLite extension library contained in the file zFile.
    // - If the file cannot be loaded directly, attempts are made to load with various
    // operating-system specific extensions added.
    // - So for example, if "samplelib" cannot be loaded, then names like "samplelib.so"
    // or "samplelib.dylib" or "samplelib.dll" might be tried also.
    // - The entry point is zProc. zProc may be 0, in which case SQLite will try to come
    // up with an entry point name  on its own.
    //- It first tries "sqlite3_extension_init". If that does not work, it constructs a name
    // "sqlite3_X_init" where the X is consists of the lower-case equivalent of all ASCII
    // alphabetic characters in the filename from the last "/" to the first following "."
    // and omitting any initial "lib".
    // The sqlite3.load_extension() interface returns SQLITE_OK on success and SQLITE_ERROR
    // if something goes wrong.
    // - If an error occurs and pzErrMsg is not 0, then the sqlite3.load_extension() interface
    // shall attempt to fill pzErrMsg with error message text stored in memory obtained from
    // sqlite3.malloc().
    //The calling function should free this memory by calling sqlite3.free().
    load_extension: function(DB: TSqlite3DB; zFile, zProc: PUtf8Char;
      var pzErrMsg: PUtf8Char): integer; cdecl;

    /// Returns a pointer to a block of memory at least N bytes in length
    // - should call native malloc() function, i.e. GetMem() in this unit
    malloc: function(N: integer): Pointer; cdecl;

    /// Attempts to resize a prior memory allocation
    // - should call native realloc() function, i.e. ReallocMem() in this unit
    realloc: function(pOld: Pointer; N: integer): Pointer; cdecl;

    /// Releases memory previously returned by sqlite3.malloc() or sqlite3.realloc()
    // - should call native free() function, i.e. FreeMem() in this unit
    // - renamed free_ in order not to override TObject.Free method
    free_: procedure(p: Pointer); cdecl;

    /// Returns the size of a memory allocation in bytes.
    // - The returned value might be larger than the number of bytes requested when
    // P was allocated.
    // - If P is nil then returns zero.
    // - If P points to something that is not the beginning of memory allocation,
    // or if it points to a formerly valid memory allocation that has now been freed,
    // then the behavior is undefined and possibly harmful.
    msize: function(p: Pointer): Int64; cdecl;

    /// Attempts to free N bytes of heap memory by deallocating non-essential memory
    // allocations held by the database library.
    // - Memory used to cache database pages to improve performance is an example of
    // non-essential memory.
    // - Returns the number of bytes actually freed, which might be more or less than
    // the amount requested.
    release_memory: function(N: integer): integer; cdecl;

    /// Attempts to free as much heap memory as possible from database connection DB
    db_release_memory: function(DB: TSqlite3DB): integer; cdecl;

    /// Returns the number of bytes of memory currently outstanding (malloced but not freed)
    // - our SQlite3 static library is compiled with #define SQLITE_DEFAULT_MEMSTATUS 0
    // so this value is not available, unless you override the BeforeInitialization virtual
    // method and set the SQLITE_CONFIG_MEMSTATUS value to 1
    // - Needs SQLITE_CONFIG_MEMSTATUS to be active by SQLITE_DEFAULT_MEMSTATUS at
    // compile time or using sqlite3.config() at runtime and before library intialization.
    memory_used: function: Int64; cdecl;

    /// Returns the maximum value of sqlite3.memory_used() since the high-water mark
    // was last reset
    // - Needs SQLITE_CONFIG_MEMSTATUS to be active by SQLITE_DEFAULT_MEMSTATUS at
    // compile time or using sqlite3.config() at runtime and before library intialization.
    memory_highwater: function(resetFlag: integer): Int64; cdecl;

    /// Sets and/or queries the soft limit on the amount of heap memory
    // that may be allocated by SQLite
    // - SQLite strives to keep heap memory utilization below the soft heap limit
    // by reducing the number of pages held in the page cache as heap memory usages
    // approaches the limit. The soft heap limit is "soft" because even though
    // SQLite strives to stay below the limit, it will exceed the limit rather
    // than generate an SQLITE_NOMEM error. In other words, the soft heap limit
    // is advisory only
    // - The return value from soft_heap_limit64() is the size of the soft heap
    // limit prior to the call, or negative in the case of an error. If the
    // argument N is negative then no change is made to the soft heap limit.
    // Hence, the current size of the soft heap limit can be determined by
    // invoking soft_heap_limit64() with a negative argument
    // - This function is useful when you have many SQLite databases open at
    // the same time, as the cache-size setting is per-database (connection),
    // while this limit is global for the process, so this allows to limit the
    // total cache size
    // - Needs SQLITE_CONFIG_MEMSTATUS to be active by SQLITE_DEFAULT_MEMSTATUS at
    // compile time or using sqlite3.config() at runtime and before library intialization.
    soft_heap_limit64: function(N: Int64): Int64; cdecl;

    /// Used to make global configuration changes to current database
    // - May only be invoked prior to library initialization or after shutdown, so a
    // typical way is to override the BeforeInitialization virtual method to call this.
    // - Not threadsafe. The application must ensure that no other SQLite
    // interfaces are invoked by other threads while sqlite3.config() is running.
    // - Operation is an integer code for a specific SQLITE_CONFIG_* that determines
    // what property of SQLite is to be configured
    // - Subsequent arguments vary depending on the configuration option in the
    // first argument.
    // - Returns SQLITE_OK, when a configuration option is set.
    // - Returns SQLITE_MISUSE, if is called after sqlite3.initialize() and before
    // sqlite3.shutdown()
    // - Returns non-zero when the option is unknown or SQLite is unable to set it.
    config: function(Operation: integer): integer; cdecl varargs;

    /// Used to make global configuration changes to current database connection
    db_config: function(DestDB: TSqlite3DB; Operation: integer): integer; cdecl varargs;

    /// Retrieve runtime status information about the performance of SQLite, and
    // optionally to reset various highwater marks.
    // - Operation is an integer code for a specific SQLITE_STATUS_* counter to be measured.
    // - The current value of the parameter is returned into pCurrent.
    // - The highest recorded value is returned in pHighwater.
    // - If the resetFlag is true, then the highest record value is reset after
    // pHighwater is written.
    // - Some parameters do not record the highest value. For those parameters nothing
    // is written into pHighwater and the resetFlag is ignored.
    // - Some parameters record only the highwater mark and not the current value.
    // For these latter parameters nothing is written into pCurrent.
    // - Returns SQLITE_OK if successful, or an sqlite error code if an error occurs.
    status64: function(Operation: integer; pCurrent, pHighwater: PInt64;
      resetFlag: integer): Integer; cdecl;

    /// Retrieve runtime status information about a single database connection, and
    // optionally to reset various highwater marks.
    // - Operation is an integer code for a specific SQLITE_DBSTATUS_* counter to be measured.
    // - The current value of the parameter is returned into pCurrent.
    // - The highest recorded value is returned in pHighwater.
    // - If the resetFlag is true, then the highest record value is reset after
    // pHighwater is written.
    db_status: function(DB: TSqlite3DB; Operation: integer; pCurrent, pHighwater: PInteger;
      resetFlag: integer): Integer; cdecl;

    /// If a write-transaction is open on database connection D when the sqlite3.db_cacheflush(DB)
    // interface invoked, any dirty pages in the pager-cache that are not currently in use are
    // written out to disk.
    // - A dirty page may be in use if a database cursor created by an active SQL statement is
    // reading from it, or if it is page 1 of a database file (page 1 is always "in use").
    // The sqlite3.db_cacheflush(DB) interface flushes caches for all schemas - "main", "temp",
    // and any attached databases.
    // If this function needs to obtain extra database locks before dirty pages can be flushed to disk,
    // it does so.
    // - If those locks cannot be obtained immediately and there is a busy-handler callback configured,
    // it is invoked in the usual manner.
    // - If the required lock still cannot be obtained, then the database is skipped and an attempt made
    // to flush any dirty pages belonging to the next (if any) database. If any databases are skipped
    // because locks cannot be obtained, but no other error occurs, this function returns SQLITE_BUSY.
    // - If any other error occurs while flushing dirty pages to disk (for example an IO error or
    // out-of-memory condition), then processing is abandoned and an SQLite error code is returned to the
    // caller immediately.
    // - Otherwise, if no error occurs, sqlite3.db_cacheflush() returns SQLITE_OK.
    // This function does not set the database handle error code or message returned by the
    // sqlite3.errcode() and sqlite3.errmsg() functions.
    db_cacheflush: function(DB: TSqlite3DB): integer; cdecl;

    /// Returns a pointer to the filename associated with database DBName of connection DB.
    // - If there is no attached database N on the database connection DB, or if database DBName
    // is a temporary or in-memory database, then this function will return either a NULL
    // pointer or an empty string.
    // - The string value returned by this routine is owned and managed by the database connection.
    // - The value will be valid until the database DBName is DETACH-ed or until the database connection closes.
    // - The filename returned by this function is the output of the xFullPathname method of the VFS.
    // - In other words, the filename will be an absolute pathname, even if the filename used to open
    // the database originally was a URI or relative pathname.
    db_filename: function(DB: TSqlite3DB; DBName: PUtf8Char): PUtf8Char; cdecl;

    /// Returns 1 if the database DBName of connection DB is read-only, 0 if it is read/write,
    // or -1 if DBName is not the name of a database on connection DB.
    db_readonly: function(DB: TSqlite3DB; DBName: PUtf8Char): integer; cdecl;

    /// Register callback function that can be used for tracing the execution of
    // SQL statements
    // - registers a trace callback function Callback against database connection
    // DB, using property mask TSqlTraceMask and context pointer UserData
    // - if the Callback parameter is nil or if the TSqlTraceMask mask is zero,
    // then tracing is disabled
    // - parameters of the Callback functions depend of the TSqlTraceMask involved
    trace_v2: function(DB: TSqlite3DB; Mask: TSqlTraceMask;
      Callback: TSqlTraceCallback; UserData: Pointer): Pointer; cdecl;

    /// Allows the size of various constructs to be limited on a connection
    // by connection basis
    // - The first parameter is the database connection whose limit is to be
    // set or queried
    // - The second parameter is one of the limit categories that define a
    // class of constructs to be size limited - see TSqlLimitCategory enumerate
    // - The third parameter is the new limit for that construct. If the new
    // limit is a negative number, the limit is unchanged.
    // - Regardless of whether or not the limit was changed, the sqlite3.limit()
    // interface returns the prior value of the limit. Hence, to find the current
    // value of a limit without changing it, simply invoke this interface with
    // the third parameter set to -1.
    limit: function(DB: TSqlite3DB; id, newValue: integer): integer; cdecl;

    /// Initialize a backup process of a given SQLite3 database instance
    // - The DestDB and DestDatabaseName arguments are the database connection
    // associated with the destination database and the database name,
    // respectively.  The database name is "main" for the main database,
    // "temp" for the temporary database, or the name specified after
    // the AS keyword in an ATTACH statement for an attached database.
    // - The SourceDB and SourceDatabaseName arguments identify the database
    // connection and database name of the source database, respectively.
    // - The source and destination database connections (parameters SourceDB and
    // DestDB) must be different or else function will fail with an error.
    // - If an error occurs within backup_init(), then nil is returned and an
    // error code and error message are stored in the destination database
    // connection DestDB. The error code and message for the failed call to
    // backup_init() can be retrieved using the errcode() or errmsg() functions.
    // - A successful call to backup_init() returns a pointer to an TSqlite3Backup
    // object. The TSqlite3Backup object may be used with the backup_step() and
    // backup_finish() functions to perform the specified backup operation.
    backup_init: function(DestDB: TSqlite3DB; DestDatabaseName: PUtf8Char;
      SourceDB: TSqlite3DB; SourceDatabaseName: PUtf8Char): TSqlite3Backup; cdecl;

    /// Perform a backup step to transfer the data between the two databases
    // - backup_step() will copy up to nPages pages between the source and
    // destination databases specified by TSqlite3Backup object Backup.
    // - If nPages is negative, all remaining source pages are copied.
    // - If backup_step() successfully copies nPages pages and there are still more
    // pages to be copied, then the function returns SQLITE_OK.
    // - If backup_step() successfully finishes copying all pages from source to
    // destination, then it returns SQLITE_DONE.
    // - If an error occurs while running backup_step(), an error code is returned.
    // - As well as SQLITE_OK and SQLITE_DONE, a call to backup_step() may return
    // SQLITE_READONLY, SQLITE_NOMEM, SQLITE_BUSY, SQLITE_LOCKED, or an
    // SQLITE_IOERR_XXX extended error code. The function might return
    // SQLITE_READONLY if the destination database was opened read-only, or is
    // using WAL journaling and the destination and source page sizes differ, or
    // the destination database is an in-memory database and the destination and
    // source page sizes differ. SQLITE_BUSY indicates that the file-system lock
    // did not succeed: in this case the call to backup_step() can be retried later.
    // If the source database connection is being used to write to the source
    // database when backup_step() is called, then SQLITE_LOCKED is returned
    // immediately. Again, in this case the call to backup_step() can be retried
    // later on. If SQLITE_IOERR_XXX, SQLITE_NOMEM, or SQLITE_READONLY is returned,
    // then there is no point in retrying the call to backup_step(). These errors
    // are considered fatal. The application must accept that the backup operation
    // has failed and pass the backup operation handle to the backup_finish() to
    // release associated resources.
    // - The first call to sqlite3_backup_step() obtains an exclusive lock on the
    // destination file. The exclusive lock is not released until either
    // backup_finish() is called or the backup operation is complete and
    // backup_step() returns SQLITE_DONE. Every call to backup_step() obtains a
    // shared lock on the source database that lasts for the duration of the
    // backup_step() call.
    // - Because the source database is not locked between calls to backup_step(),
    // the source database may be modified mid-way through the backup process.
    // If the source database is modified by an external process or via a database
    // connection other than the one being used by the backup operation, then the
    // backup will be automatically restarted by the next call to backup_step().
    // If the source database is modified by the using the same database connection
    // as is used by the backup operation (which is the case in the
    // mormot.db.raw.sqlite3 and  mORMoTSqlite3 units), then the backup database
    // is automatically updated at the same time, so you won't loose any data.
    backup_step: function(Backup: TSqlite3Backup; nPages: integer): integer; cdecl;

    /// Finalize a Backup process on a given database
    // - When backup_step() has returned SQLITE_DONE, or when the application
    // wishes to abandon the backup operation, the application should destroy the
    // TSqlite3Backup by passing it to backup_finish().
    // - The backup_finish() interfaces releases all resources associated with the
    // TSqlite3Backup object. If backup_step() has not yet returned SQLITE_DONE,
    // then any active write-transaction on the destination database is rolled back.
    // - The TSqlite3Backup object is invalid and may not be used following a call
    // to backup_finish().
    // - The value returned by backup_finish is SQLITE_OK if no backup_step() errors
    // occurred, regardless or whether or not backup_step() completed. If an
    // out-of-memory condition or IO error occurred during any prior backup_step()
    // call on the same TSqlite3Backup object, then backup_finish() returns the
    // corresponding error code.
    // - A return of SQLITE_BUSY or SQLITE_LOCKED from backup_step() is not a
    // permanent error and does not affect the return value of backup_finish().
    backup_finish: function(Backup: TSqlite3Backup): integer; cdecl;

    /// Returns the number of pages still to be backed up for a given Backup
    // - The values returned by this function is only updated by backup_step().
    // If the source database is modified during a backup operation, then the
    // value is not updated to account for any extra pages that need to be
    // updated or the size of the source database file changing.
    backup_remaining: function(Backup: TSqlite3Backup): integer; cdecl;

    /// Returns the the total number of pages in the source database file
    // for a given Backup process
    // - The values returned by this function is only updated by backup_step().
    // If the source database is modified during a backup operation, then the
    // value is not updated to account for any extra pages that need to be
    // updated or the size of the source database file changing.
    backup_pagecount: function(Backup: TSqlite3Backup): integer; cdecl;

    /// Serialize a database
    // - returns a pointer to memory that is a serialization of the Schema
    // database on database connection DB
    // - if Size is not nil, then the size of the database in bytes is written into Size^
    // - for an ordinary on-disk database file, the serialization is just a copy
    // of the disk file; for an in-memory database or a "TEMP" database, the
    // serialization is the same sequence of bytes which would be written to disk
    // if that database where backed up to disk
    // - caller is responsible for freeing the returned value (using free_)
    // to avoid a memory leak
    serialize: function(DB: TSqlite3DB; Schema: PUtf8Char; Size: PInt64;
      Flags: cardinal): pointer; cdecl;

    /// Deserialize a database
    // - causes the database connection DB to disconnect from database Schema
    // and then reopen Schema as an in-memory database based on the serialization
    // contained in Data; the serialized database Data is DBSize bytes in size
    // - BufSize is the size of the buffer Data, which might be larger than DBSize
    deserialize: function(DB: TSqlite3DB; Schema: PUtf8Char; Data: pointer;
      DBSize, BufSize: Int64; Flags: cardinal): pointer; cdecl;

    // Register a callback that is invoked each time data is committed to a database in wal mode.
    //  - The callback is invoked by SQLite after the commit has taken place and the associated
    // write-lock on the database released, so the implementation may read, write or checkpoint
    // the database as required.
    // - A single database handle may have at most a single write-ahead log callback registered at one time.
    // Calling sqlite3.wal_hook() replaces any previously registered write-ahead log callback.
    // - Note that the sqlite3.wal_autocheckpoint() interface and the wal_autocheckpoint pragma both
    // invoke sqlite3.wal_hook() and will overwrite any prior sqlite3.wal_hook() settings.
    wal_hook: function(DB: TSqlite3DB; Callback: TSqlWalHookCallback;
      UserData: pointer): pointer; cdecl;

    /// A wrapper around sqlite3.wal_hook() that causes any database on database connection DB
    // to automatically checkpoint after committing a transaction if there are N or more frames
    // in the write-ahead log file.
    // - Passing zero or a negative value as the nFrame parameter disables automatic checkpoints entirely.
    // - The callback registered by this function replaces any existing callback registered using
    // sqlite3.wal_hook(). Likewise, registering a callback using sqlite3.wal_hook() disables the
    // automatic checkpoint mechanism configured by this function.
    // - The wal_autocheckpoint pragma can be used to invoke this interface from SQL.
    // - Checkpoints initiated by this mechanism are PASSIVE.
    // - Every new database connection defaults to having the auto-checkpoint enabled with a threshold
    // of 1000 or SQLITE_DEFAULT_WAL_AUTOCHECKPOINT pages.
    // The use of this interface is only necessary if the default setting is found to be suboptimal
    // for a particular application.
    wal_autocheckpoint: function(DB: TSqlite3DB; N: integer): integer; cdecl;

    /// Runs a checkpoint operation on database zDb of database connection DB in mode eMode.
    // - Status information is written back into integers pointed to by pnLog and pnLog.
    // - If pnLog is not nil, then pnLog is set to the total number of frames in the log file
    // or to -1 if the checkpoint could not run because of an error or because the database is
    // not in WAL mode.
    // - If pnCkpt is not nil,then pnCkpt is set to the total number of checkpointed frames in
    // the log file (including any that were already checkpointed before the function was called)
    // or to -1 if the checkpoint could not run due to an error or because the database is not
    // in WAL mode.
    // - Note that upon successful completion of an SQLITE_CHECKPOINT_TRUNCATE, the log file will
    // have been truncated to zero bytes and so both pnLog and pnCkpt will be set to zero.
    // - All calls obtain an exclusive "checkpoint" lock on the database file.
    // - If any other process is running a checkpoint operation at the same time, the lock cannot
    // be obtained and SQLITE_BUSY is returned.
    // - Even if there is a busy-handler configured, it will not be invoked in this case.
    // - The SQLITE_CHECKPOINT_FULL, RESTART and TRUNCATE modes also obtain the exclusive "writer"
    // lock on the database file.
    // - If the writer lock cannot be obtained immediately, and a busy-handler is configured,
    // it is invoked and the writer lock retried until either the busy-handler returns 0 or the lock
    // is successfully obtained.
    // - The busy-handler is also invoked while waiting for database readers as described above.
    // - If the busy-handler returns 0 before the writer lock is obtained or while waiting for database
    // readers, the checkpoint operation proceeds from that point in the same way as
    // - SQLITE_CHECKPOINT_PASSIVE - checkpointing as many frames as possible without blocking any further.
    // SQLITE_BUSY is returned in this case.
    // - If parameter zDb is nil or points to a zero length string, then the specified operation is attempted
    // on all WAL databases attached to database connection db.
    // - In this case the values written to output parameters pnLog and pnCkpt are undefined.
    // - If an SQLITE_BUSY error is encountered when processing one or more of the attached WAL databases,
    // the operation is still attempted on any remaining attached databases and SQLITE_BUSY is returned at
    // the end.
    // - If any other error occurs while processing an attached database, processing is abandoned and the
    // error code is returned to the caller immediately.
    // - If no error (SQLITE_BUSY or otherwise) is encountered while processing the attached databases,
    // SQLITE_OK is returned.
    // - If database zDb is the name of an attached database that is not in WAL mode, SQLITE_OK is returned
    // and both pnLog and pnCkpt set to -1. If zDb is not nil (or a zero length string) and is not the
    // name of any attached database, SQLITE_ERROR is returned to the caller.
    // - Unless it returns SQLITE_MISUSE, the sqlite3.wal_checkpoint_v2() interface sets the error
    // information that is queried by sqlite3.errcode() and sqlite3.errmsg().
    // - The PRAGMA wal_checkpoint command can be used to invoke this interface from SQL.
    wal_checkpoint_v2: function(DB: TSqlite3DB; zDb: PUtf8Char;
      eMode: integer; var pnLog, pnCkpt: integer): integer; cdecl;

    /// Attempts to make a new TSqlite3Snapshot object that records the current state of schema zSchema
    // in database connection DB.
    // - On success, the sqlite3.snapshot_get() interface writes a pointer to the newly created
    // TSqlite3Snapshot object into Snapshot and returns SQLITE_OK.
    // - If there is not already a read-transaction open on schema zSchema when this function is called,
    // one is opened automatically.
    // - The following must be true for this function to succeed.
    // - If any of the following statements are false when sqlite3.snapshot_get() is called,
    // SQLITE_ERROR is returned. The final value of *P is undefined in this case.
    // - The database handle must not be in autocommit mode.
    // - Schema zSchema of database connection DB must be a WAL mode database.
    // - There must not be a write transaction open on schema zSchema of database connection DB.
    // - One or more transactions must have been written to the current wal file since it was created
    // on disk (by any connection).
    // - This means that a snapshot cannot be taken on a wal mode database with no wal file immediately
    // after it is first opened. At least one transaction must be written to it first.
    // - This function may also return SQLITE_NOMEM. If it is called with the database handle in autocommit
    // mode but fails for some other reason, whether or not a read transaction is opened on schema
    // zSchema is undefined.
    // - The TSqlite3Snapshot object returned from a successful call to sqlite3.snapshot_get() must be
    // freed using sqlite3.snapshot_free() to avoid a memory leak.
    snapshot_get: function(DB: TSqlite3DB; zSchema: PUtf8Char;
      var Snapshot: PSqlite3Snapshot): integer; cdecl;

    /// Either starts a new read transaction or upgrades an existing one for schema zSchema of database
    // connection DB such that the read transaction refers to historical snapshot Snapshot, rather than
    // the most recent change to the database.
    // - Returns SQLITE_OK on success or an appropriate error code if it fails.
    // - In order to succeed, the database connection must not be in autocommit mode when
    // sqlite3.snapshot_open() is called. If there is already a read transaction open on schema zSchema,
    // then the database handle must have no active statements (SELECT statements that have been passed
    // to sqlite3.step() but not sqlite3.reset() or sqlite3.finalize()).
    // - SQLITE_ERROR is returned if either of these conditions is violated, or if schema zSchema does not exist,
    // or if the snapshot object is invalid.
    // - A call to sqlite3.snapshot_open() will fail to open if the specified snapshot has been overwritten
    // by a checkpoint. In this case SQLITE_ERROR_SNAPSHOT is returned.
    // - If there is already a read transaction open when this function is invoked, then the same read
    // transaction remains open (on the same database snapshot) if SQLITE_ERROR, SQLITE_BUSY or
    // SQLITE_ERROR_SNAPSHOT is returned. If another error code - for example SQLITE_PROTOCOL or an
    // SQLITE_IOERR error code - is returned, then the final state of the read transaction is undefined.
    // - If SQLITE_OK is returned, then the read transaction is now open on database snapshot Snapshot.
    // - A call to sqlite3.snapshot_open() will fail if the database connection DB does not know
    // that the database file for schema zSchema is in WAL mode. A database connection might not know that the
    // database file is in WAL mode if there has been no prior I/O on that database connection, or if the
    // database entered WAL mode after the most recent I/O on the database connection.
    // - Run "PRAGMA application_id" against a newly opened database connection in order to make it ready
    // to use snapshots.
    snapshot_open: function(DB: TSqlite3DB; zSchema: PUtf8Char;
      Snapshot: PSqlite3Snapshot): integer; cdecl;

    /// Recover snapshots from a wal file
    // - If a WAL file remains on disk after all database connections close (either through the use
    // of the SQLITE_FCNTL_PERSIST_WAL file control or because the last process to have the database
    // opened exited without calling sqlite3.close()) and a new connection is subsequently opened on
    // that database and WAL file, the sqlite3.snapshot_open() interface will only be able to open the
    // last transaction added to the WAL file even though the WAL file contains other valid transactions.
    // - This function attempts to scan the WAL file associated with database zDb of database handle db
    // and make all valid snapshots available to sqlite3.snapshot_open().
    // - It is an error if there is already a read transaction open on the database, or if the database
    // is not a WAL mode database.
    // - SQLITE_OK is returned if successful, or an SQLite error code otherwise.
    snapshot_recover: function(DB: TSqlite3DB; zDB: PUtf8Char): integer; cdecl;

    /// Compare the ages of two valid snapshot handles
    //- If the two snapshot handles are not associated with the same database file, the result of
    // the comparison is undefined.
    // - Additionally, the result of the comparison is only valid if both of the snapshot handles
    // were obtained by calling sqlite3.snapshot_get() since the last time the wal file was deleted.
    // - The wal file is deleted when the database is changed back to rollback mode or when the number
    // of database clients drops to zero.
    // - If either snapshot handle was obtained before the wal file was last deleted, the value returned
    // by this function is undefined.
    // - Otherwise, this API returns a negative value if P1 refers to an older snapshot than P2, zero
    // if the two handles refer to the same database snapshot, and a positive value if P1 is a newer
    // snapshot than P2.
    snapshot_cmp: function(DB: TSqlite3DB; P1, P2: PSqlite3Snapshot): integer; cdecl;

    /// Destroys a TSqlite3Snapshot
    // - The application must eventually free every TSqlite3Snapshot object using this routine
    // to avoid a memory leak.
    snapshot_free: function(DB: TSqlite3DB; Snapshot: PSqlite3Snapshot): integer; cdecl;

    /// Initialize the internal version numbers and call AfterInitialization
    constructor Create; virtual;
    /// this method is called by Create after SQlite3 is loaded, but before
    // sqlite3_initialize is called
    // - do nothing by default, but you may override it
    procedure BeforeInitialization; virtual;
    /// this method is called by Create after SQlite3 is loaded, and after
    // sqlite3_initialize is called
    // - do nothing by default, but you may override it
    procedure AfterInitialization; virtual;
    /// Will change the SQLite3 configuration to use Delphi/FPC memory manager
    // - this will reduce memory fragmentation, and enhance speed, especially
    // under multi-process activity
    // - this method should be called before sqlite3.initialize(), e.g. by
    // overriding the BeforeInitialization virtual method - as does the
    // TSqlite3LibraryStatic class
    procedure ForceToUseSharedMemoryManager; virtual;
    /// Returns the current version number as a plain integer
    // - equals e.g. 3008003001 for '3.8.3.1'
    property VersionNumber: cardinal
      read fVersionNumber;
    /// Returns the current version number as a text
    // - equals e.g. '3.8.3.1'
    // - use the Version property for the full information about this instance
    property VersionText: RawUtf8
      read fVersionText;
  published
    /// Will return the class name and SQLite3 version number
    // - if self (e.g. global sqlite3) is nil, will return ''
    property Version: RawUtf8
      read GetVersion;
  end;
  {$M-}

  /// allow access to an exernal SQLite3 library engine
  // - you can e.g. replace the main sqlite3 engine with any external library:
  // ! FreeAndNil(sqlite3); // release any previous instance (e.g. static)
  // ! sqlite3 := TSqlite3LibraryDynamic.Create;
  TSqlite3LibraryDynamic = class(TSqlite3Library)
  protected
    fLoader: TSynLibrary;
    function GetLibraryName: TFileName;
  public
    /// initialize the specified external library
    // - raise an ESqlite3Exception on error
    constructor Create(
      const LibraryName: TFileName = SQLITE_LIBRARY_DEFAULT_NAME); reintroduce;
    /// unload the external library
    destructor Destroy; override;
  published
    property LibraryName: TFileName
      read GetLibraryName;
  end;


/// an internal function which calls Freemem(p)
// - can be used to free some PUtf8Char pointer allocated by the RTL Getmem()
procedure sqlite3InternalFree(p: pointer); cdecl;

/// an internal function which calls TObject(p).Free
// - can be used to free some class instance
procedure sqlite3InternalFreeObject(p: pointer); cdecl;

/// an internal function which calls RawByteString(p) := ''
// - can be used to free some class instance
// - use a local tmp: pointer variable to prepare the reference count, e.g.
// !  tmp := nil;
// !  RawUtf8(tmp) := Text; // fast COW assignment
// !  sqlite3.result_text(Context,tmp,length(Text)+1,sqlite3InternalFreeRawByteString);
procedure sqlite3InternalFreeRawByteString({%H-}p: pointer); cdecl;

/// wrapper around sqlite3.result_error() to be called if wrong number of arguments
procedure ErrorWrongNumberOfArgs(Context: TSqlite3FunctionContext);

/// wrapper around sqlite3.result_error() validating the expected number of arguments
function CheckNumberOfArgs(Context: TSqlite3FunctionContext;
  expected, sent: integer): boolean;

/// create a TSqlite3Module.pzErr UTF-8 text buffer according to the given
// Exception class
procedure ExceptionToSqlite3Err(E: Exception; var pzErr: PUtf8Char);

/// set a TSqlVar into a SQlite3 result context
// - will call the corresponding sqlite3.result_*() function and return true,
// or will return false if the TSqlVar type is not handled
function SqlVarToSQlite3Context(const Res: TSqlVar;
  Context: TSqlite3FunctionContext): boolean;

/// set a UTF-8 string into a SQlite3 result context
// - this function will use copy-on-write assignment of Text, with no memory
// allocation, then let sqlite3InternalFreeRawByteString release its reference count
// - ForcedLen can be used if the UTF-8 text is smaller than length(Text)
procedure RawUtf8ToSQlite3Context(const Text: RawUtf8;
  Context: TSqlite3FunctionContext; VoidTextAsNull: boolean;
  ForcedLen: integer = -1);

/// set a variant value into a SQlite3 result context
// - will call the corresponding sqlite3.result_*() function, using
// SqlVarToSQlite3Context() after a call to VariantToSqlVar()
procedure VariantToSQlite3Context(const Value: Variant;
  Context: TSqlite3FunctionContext);

/// set a JSON value into a SQlite3 result context
// - a JSON object or array would be returned at plain TEXT, or other simple
// JSON text or number would be returned as the corresponding SQLite3 value
procedure JsonToSQlite3Context(json: PUtf8Char;
  Context: TSqlite3FunctionContext);

/// set a SQLite3 value into a TSqlVar
// - will call the corresponding sqlite3.value_*() function to retrieve the
// data with the less overhead (e.g. memory allocation or copy) as possible
procedure SQlite3ValueToSqlVar(Value: TSqlite3Value; var Res: TSqlVar);


const
  SQLITE_INDEX_CONSTRAINT_EQ    = 2;
  SQLITE_INDEX_CONSTRAINT_GT    = 4;
  SQLITE_INDEX_CONSTRAINT_LE    = 8;
  SQLITE_INDEX_CONSTRAINT_LT    = 16;
  SQLITE_INDEX_CONSTRAINT_GE    = 32;
  SQLITE_INDEX_CONSTRAINT_MATCH = 64;
  SQLITE_INDEX_CONSTRAINT_LIKE  = 65;
  SQLITE_INDEX_CONSTRAINT_GLOB  = 66;
  SQLITE_INDEX_CONSTRAINT_REGEXP = 67;

  SQLITE_DENY   = 1;
  SQLITE_IGNORE = 2;

  SQLITE_CREATE_INDEX        = 1;
  SQLITE_CREATE_TABLE        = 2;
  SQLITE_CREATE_TEMP_INDEX   = 3;
  SQLITE_CREATE_TEMP_TABLE   = 4;
  SQLITE_CREATE_TEMP_TRIGGER = 5;
  SQLITE_CREATE_TEMP_VIEW    = 6;
  SQLITE_CREATE_TRIGGER      = 7;
  SQLITE_CREATE_VIEW         = 8;
  SQLITE_DELETE              = 9;
  SQLITE_DROP_INDEX          = 10;
  SQLITE_DROP_TABLE          = 11;
  SQLITE_DROP_TEMP_INDEX     = 12;
  SQLITE_DROP_TEMP_TABLE     = 13;
  SQLITE_DROP_TEMP_TRIGGER   = 14;
  SQLITE_DROP_TEMP_VIEW      = 15;
  SQLITE_DROP_TRIGGER        = 16;
  SQLITE_DROP_VIEW           = 17;
  SQLITE_INSERT              = 18;
  SQLITE_PRAGMA              = 19;
  SQLITE_READ                = 20;
  SQLITE_SELECT              = 21;
  SQLITE_TRANSACTION         = 22;
  SQLITE_UPDATE              = 23;
  SQLITE_ATTACH              = 24;
  SQLITE_DETACH              = 25;
  SQLITE_ALTER_TABLE         = 26;
  SQLITE_REINDEX             = 27;
  SQLITE_ANALYZE             = 28;
  SQLITE_CREATE_VTABLE       = 29;
  SQLITE_DROP_VTABLE         = 30;
  SQLITE_FUNCTION            = 31;
  SQLITE_SAVEPOINT           = 32;
  SQLITE_COPY                = 0;


  /// SQL statement to get all tables names in the current database file
  // (taken from official SQLite3 documentation)
  SQL_GET_TABLE_NAMES =
    'SELECT name FROM sqlite_master WHERE type=''table'' AND name NOT LIKE ''sqlite_%'';';

type
  /// the main possible return codes, including error codes
  TSqlite3ErrorCode = (
    secUnknown,
    secOK,
    secERROR,
    secINTERNAL,
    secPERM,
    secABORT,
    secBUSY,
    secLOCKED,
    secNOMEM,
    secREADONLY,
    secINTERRUPT,
    secIOERR,
    secCORRUPT,
    secNOTFOUND,
    secFULL,
    secCANTOPEN,
    secPROTOCOL,
    secEMPTY,
    secSCHEMA,
    secTOOBIG,
    secCONSTRAINT,
    secMISMATCH,
    secMISUSE,
    secNOLFS,
    secAUTH,
    secFORMAT,
    secRANGE,
    secNOTADB,
    secROW,
    secDONE);

  /// custom SQLite3 dedicated Exception type
  ESqlite3Exception = class(ESynException)
  protected
    fErrorCode: integer;
    fSQLite3ErrorCode: TSqlite3ErrorCode;
  public
    /// the DB which raised this exception
    DB: TSqlite3DB;
    /// create the exception, getting the message from DB
    constructor Create(aDB: TSqlite3DB; aErrorCode: integer;
      const aSql: RawUtf8); reintroduce; overload;
  published
    /// the corresponding error code, e.g. 21 (for SQLITE_MISUSE)
    property ErrorCode: integer
      read fErrorCode;
    /// the corresponding error code, e.g. secMISUSE
    property SQLite3ErrorCode: TSqlite3ErrorCode
      read fSQLite3ErrorCode;
  end;

/// convert a SQLite3 result code into a TSqlite3ErrorCode item
function sqlite3_resultToErrorCode(aResult: integer): TSqlite3ErrorCode;

/// convert a SQLite3 result code into the corresponding SQLite constant name
// - e.g. sqlite3_resultToErrorText(SQLITE_OK)='SQLITE_OK'
function sqlite3_resultToErrorText(aResult: integer): RawUtf8;

/// convert a TSqlite3ErrorCode item into the corresponding SQLite constant name
// - e.g. ErrorCodeToText(secOK)='SQLITE_OK'
function ErrorCodeToText(err: TSqlite3ErrorCode): RawUtf8;

/// test the result state of a sqlite3.*() function
// - raise a ESqlite3Exception if the result state is within SQLITE_ERRORS
// - return the result state otherwise (SQLITE_OK,SQLITE_ROW,SQLITE_DONE e.g.)
function sqlite3_check(DB: TSqlite3DB; aResult: integer;
  const SQL: RawUtf8=''): integer;

var
  /// global access to linked SQLite3 library API calls
  // - you should call sqlite3.open() instead of sqlite3_open() for instance
  // - points either to the statically linked sqlite3.obj, or to an external
  // library (e.g. sqlite3.dll under Windows)
  // - your project should use EITHER mormot.db.raw.sqlite3.static unit
  // OR create a TSqlite3LibraryDynamic instance:
  // ! FreeAndNil(sqlite3); // release any previous instance
  // ! sqlite3 := TSqlite3LibraryDynamic.Create;
  // - caller should free the sqlite3 instance only with
  // ! FreeAndNil(sqlite3);
  // to avoid issues with the automatic freeing in finalization section
  sqlite3: TSqlite3Library;


{ ************ High-Level Classes for SQlite3 Queries }

type
  /// available file-level write access wait mode of the SQLite3 engine
  // - when synchronous is smFull (which is the default setting), the SQLite
  // database engine will use the xSync method of the VFS to ensure that all
  // content is safely written to the disk surface prior to continuing. This
  // ensures that an operating system crash or power failure will not corrupt
  // the database. FULL synchronous is very safe, but it is also slower.
  // - when synchronous is smNormal, the SQLite database engine will still
  // sync at the most critical moments, but less often than in FULL mode. There
  // is a very small (though non-zero) chance that a power failure at just the
  // wrong time could corrupt the database in NORMAL mode. But in practice,
  // you are more likely to suffer a catastrophic disk failure or some other
  // unrecoverable hardware fault.
  // - when synchronous is smOff, SQLite continues without syncing as soon as
  // it has handed data off to the operating system. If the application running
  // SQLite crashes, the data will be safe, but the database might become
  // corrupted if the operating system crashes or the computer loses power
  // before that data has been written to the disk surface. On the other hand,
  // some operations are as much as 50 or more times faster with synchronous OFF.
  TSqlSynchronousMode = (
    smOff,
    smNormal,
    smFull);

  /// available file-level database connection locking-mode
  // - lmNormal locking-mode (the default unless overridden at compile-time using
  // SQLITE_DEFAULT_LOCKING_MODE), a database connection unlocks the database
  // file at the conclusion of each read or write transaction.
  // - when the locking-mode is set to lmExclusive, the database connection
  // never releases file-locks. The first time the database is read in
  // lmExclusive mode, a shared lock is obtained and held. The first time the
  // database is written, an exclusive lock is obtained and held. Database locks
  // obtained by a connection in lmExclusive mode may be released either by
  // closing the database connection, or by setting the locking-mode back to
  // lmNormal using this pragma and then accessing the database file (for read
  // or write). Simply setting the locking-mode to lmNormal is not enough - locks
  // are not released until the next time the database file is accessed.
  // - lmExclusive gives much better write performance, and could be used when
  // needed, in case of a heavy loaded mORMot server
  TSqlLockingMode = (
    lmNormal,
    lmExclusive);

  /// available Run-Time limit categories
  // - as expected by sqlite3.limit() function and TSqlDatabase.Limit property
  // - lcLength The maximum size of any string or BLOB or table row, in bytes.
  // - lcSQLLength The maximum length of an SQL statement, in bytes.
  // - lcColumn The maximum number of columns in a table definition or in the
  // result set of a SELECT or the maximum number of columns in an index or in
  // an ORDER BY or GROUP BY clause.
  // - lcExprDepth The maximum depth of the parse tree on any expression.
  // - lcCompoundSelect The maximum number of terms in a compound SELECT statement.
  // - lcVDBEop The maximum number of instructions in a virtual machine program
  // used to implement an SQL statement. This limit is not currently enforced,
  // though that might be added in some future release of SQLite.
  // - lcFunctionArg The maximum number of arguments on a function.
  // - lcAttached The maximum number of attached databases.
  // - lcLikePatternLength The maximum length of the pattern argument to the
  // LIKE or GLOB operators.
  // - lcVariableNumber The maximum number of parameters in an SQL statement.
  // - lcTriggerDepth The maximum depth of recursion for triggers.
  TSqlLimitCategory = (
    lcLength,
    lcSQLLength,
    lcColumn,
    lcExprDepth,
    lcCompoundSelect,
    lcVDBEop,
    lcFunctionArg,
    lcAttached,
    lcLikePatternLength,
    lcVariableNumber,
    lcTriggerDepth);

  {$M+}
  TSqlDatabase = class;
  {$M-}

  TSqlBlobStream = class;

  PSqlRequest = ^TSqlRequest;

  /// wrapper to a SQLite3 request
  // - defined as a record, so that it may be allocated on the stack
  // - do not forget to call the Close method to release the request resources
  {$ifdef USERECORDWITHMETHODS}
  TSqlRequest = record
  {$else}
  TSqlRequest = object
  {$endif USERECORDWITHMETHODS}
  private
    fDB: TSqlite3DB;
    fRequest: TSqlite3Statement;
    fNextSQL: PUtf8Char;
    fFieldCount: integer;
    fResetDone: boolean; // to make Reset re-entrant
    function GetReadOnly: boolean;
    function GetParamCount: integer;

  // 1. general request process
  public
    /// Prepare a UTF-8 encoded SQL statement
    // - compile the SQL into byte-code
    // - parameters ? ?NNN :VV @VV $VV can be bound with Bind*() functions below
    // - raise an ESqlite3Exception on any error, unless NoExcept is TRUE
    function Prepare(DB: TSqlite3DB; const SQL: RawUtf8;
      NoExcept: boolean = false): integer;
    /// Prepare a WinAnsi SQL statement
    // - behave the same as Prepare()
    function PrepareAnsi(DB: TSqlite3DB; const SQL: WinAnsiString): integer;
    /// Prepare the next SQL command initialized in previous Prepare()
    // - raise an ESqlite3Exception on any error
    function PrepareNext: integer;
    /// Evaluate An SQL Statement, returning the sqlite3.step() result status:
    // - return SQLITE_ROW on success, with data ready to be retrieved via the
    // Field*() methods
    // - return SQLITE_DONE if the SQL commands were executed
    // - raise an ESqlite3Exception on any error
    function Step: integer;
    /// Reset A Prepared Statement Object
    // - reset a prepared statement object back to its initial state,
    // ready to be re-executed.
    // - any SQL statement variables that had values bound to them using the Bind*()
    // function below retain their values. Use BindReset() to reset the bindings
    // - return SQLITE_OK on success, or the previous Step error code
    function Reset: integer;
    /// Execute all SQL statements already prepared by a call to Prepare()
    // - Close is always called internally
    // - raise an ESqlite3Exception on any error
    procedure ExecuteAll; overload;
    /// Execute all SQL statements in the aSql UTF-8 encoded string
    // - internally call Prepare() then Step then PrepareNext until end of aSql
    // - Close is always called internally
    // - raise an ESqlite3Exception on any error
    procedure ExecuteAll(aDB: TSqlite3DB; const aSql: RawUtf8); overload;
    /// Execute one SQL statement already prepared by a call to Prepare()
    // - Close is always called internally
    // - raise an ESqlite3Exception on any error
    procedure Execute; overload;
    /// Execute one SQL statement in the aSql UTF-8 encoded string
    // - Execute the first statement in aSql: call Prepare() then Step once
    // - Close is always called internally
    // - raise an ESqlite3Exception on any error
    procedure Execute(aDB: TSqlite3DB; const aSql: RawUtf8); overload;
    /// Execute one SQL statement in the aSql UTF-8 encoded string
    // - Execute the first statement in aSql: call Prepare() then Step once
    // - Close is always called internally
    // - returns TRUE on success, and raise no ESqlite3Exception on error, but returns FALSE
    function ExecuteNoException(aDB: TSqlite3DB; const aSql: RawUtf8): boolean;
    /// Execute a SQL statement which return integers from the aSql UTF-8 encoded string
    // - Execute the first statement in aSql
    // - this statement must get (at least) one field/column result of INTEGER
    // - return result as a dynamic array of Int64 in aValues[]
    // - return count of row in integer function result (may be < length(aValues))
    // - Close is always called internally
    // - raise an ESqlite3Exception on any error
    function Execute(aDB: TSqlite3DB; const aSql: RawUtf8;
      var aValues: TInt64DynArray): integer; overload;
    /// Execute a SQL statement which return one integer from the aSql UTF-8 encoded string
    // - Execute the first statement in aSql
    // - this statement must get (at least) one field/column result of INTEGER
    // - return result as an unique Int64 in aValue
    // - Close is always called internally
    // - raise an ESqlite3Exception on any error
    procedure Execute(aDB: TSqlite3DB; const aSql: RawUtf8;
      out aValue: Int64); overload;
    /// Execute a SQL statement which return one TEXT value from the aSql UTF-8 encoded string
    // - Execute the first statement in aSql
    // - this statement must get (at least) one field/column result of TEXT
    // - Close is always called internally
    // - raise an ESqlite3Exception on any error
    procedure Execute(aDB: TSqlite3DB; const aSql: RawUtf8;
      out aValue: RawUtf8); overload;
    /// Execute a SQL statement which return TEXT from the aSql UTF-8 encoded string
    // - Execute the first statement in aSql
    // - this statement must get (at least) one field/column result of TEXT
    // - return result as a dynamic array of RawUtf8 in aValues[]
    // - return count of row in integer function result (may be < length(aValues))
    // - Close is always called internally
    // - raise an ESqlite3Exception on any error
    function Execute(aDB: TSqlite3DB; const aSql: RawUtf8;
      var aValues: TRawUtf8DynArray): integer; overload;
    /// Execute one SQL statement which return the results in JSON format
    // - JSON format is more compact than XML and well supported
    // - Execute the first statement in aSql
    // - if SQL is '', the statement should have been prepared, reset and bound
    // if necessary - if SQL <> '' then the statement would be closed internally
    // - raise an ESqlite3Exception on any error
    // - JSON data is added to TStream, with UTF-8 encoding
    // - if Expand is true, JSON data is an array of objects, for direct use
    // with any Ajax or .NET client:
    // & [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
    // - if Expand is false, JSON data is serialized (used in TOrmTableJson)
    // & { "FieldCount":1,"Values":["col1","col2",val11,"val12",val21,..] }
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"'
    // format and contains true BLOB data (no conversion into TEXT, as with
    // TOrmTableDB) - so will work for sftBlob, sftBlobDynArray and sftBlobRecord
    // - by default, won't write more than 512MB of JSON, to avoid OutOfMemory
    // - returns the number of data rows added to JSON (excluding the headers)
    function Execute(aDB: TSqlite3DB; const aSql: RawUtf8; Json: TStream;
      Expand: boolean = false; MaxMemory: PtrUInt = 512 shl 20;
      Options: TTextWriterOptions = []): PtrInt; overload;
    /// Execute one SQL statement which return the results in JSON format
    // - use internally Execute() above with a TRawByteStringStream, and return a string
    // - returns the number of data rows added to JSON (excluding the headers)
    // in the integer variable mapped by aResultCount (if any)
    // - by default, won't write more than 512MB of JSON, to avoid EOutOfMemory
    // - if any error occurs, the ESqlite3Exception is handled and '' is returned
    function ExecuteJson(aDB: TSqlite3DB; const aSql: RawUtf8;
      Expand: boolean = false; aResultCount: PPtrInt = nil;
      MaxMemory: PtrUInt = 512 shl 20; Options: TTextWriterOptions = []): RawUtf8;
    /// Execute all SQL statements in the aSql UTF-8 encoded string, results will
    // be written as ANSI text in OutFile
    procedure ExecuteDebug(aDB: TSqlite3DB; const aSql: RawUtf8;
      var OutFile: Text);
    /// close the Request handle
    // - call it even if an ESqlite3Exception has been raised
    procedure Close;

    /// read-only access to the Request (SQLite3 statement) handle
    property Request: TSqlite3Statement
      read fRequest;
    /// read-only access to the SQLite3 database handle
    property RequestDB: TSqlite3DB
      read fDB;
    /// returns true if the current prepared statement makes no direct changes
    // to the content of the database file
    // - Transaction control statements such as BEGIN, COMMIT, ROLLBACK, SAVEPOINT,
    // and RELEASE cause this property to return true, since the statements
    // themselves do not actually modify the database but rather they control the
    // timing of when other statements modify the database. The ATTACH and DETACH
    // statements also cause this property to return true since, while
    // those statements change the configuration of a database connection, they
    // do not make changes to the content of the database files on disk.
    property IsReadOnly: boolean
      read GetReadOnly;

  // 2. Bind parameters to a SQL query (for the last prepared statement)
  public
    /// Reset All Bindings On A Prepared Statement
    // - Contrary to the intuition of many, Reset() does not reset the bindings
    // on a prepared statement. Use this routine to reset all host parameter
    procedure BindReset;
    /// bind a NULL value to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESqlite3Exception on any error
    procedure BindNull(Param: integer);
    /// bind an integer value to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESqlite3Exception on any error
    procedure Bind(Param: integer; Value: Int64); overload;
    /// bind a double value to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESqlite3Exception on any error
    procedure Bind(Param: integer; Value: double); overload;
    /// bind a UTF-8 encoded string to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESqlite3Exception on any error
    // - this function will use copy-on-write assignment of Value, with no memory
    // allocation, then let sqlite3InternalFreeRawByteString release the variable
    procedure Bind(Param: integer; const Value: RawUtf8); overload;
    /// bind a generic VCL string to a parameter
    // - with versions prior to Delphi 2009, you may loose some content here:
    // Bind(Param: integer; Value: RawUtf8) is the prefered method
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESqlite3Exception on any error
    procedure BindS(Param: integer; const Value: string);
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESqlite3Exception on any error
    procedure Bind(Param: integer; Data: pointer; Size: integer); overload;
    /// bind a Blob buffer to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESqlite3Exception on any error
    // - this function will use copy-on-write assignment of Data, with no memory
    // allocation, then let sqlite3InternalFreeRawByteString release the variable
    procedure BindBlob(Param: integer; const Data: RawByteString);
    /// bind a Blob TCustomMemoryStream buffer to a parameter
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESqlite3Exception on any error
    procedure Bind(Param: integer; Data: TCustomMemoryStream); overload;
    /// bind a ZeroBlob buffer to a parameter
    // - uses a fixed amount of memory (just an integer to hold its size) while
    // it is being processed. Zeroblobs are intended to serve as placeholders
    // for BLOBs whose content is later written using incremental BLOB I/O routines
    // (as with TSqlBlobStream created from TSqlDataBase.Blob() e.g.).
    // - a negative value for the Size parameter results in a zero-length BLOB
    // - the leftmost SQL parameter has an index of 1, but ?NNN may override it
    // - raise an ESqlite3Exception on any error
    procedure BindZero(Param: integer; Size: integer);

  // 3. Field attributes after a sucessfull Step() (returned SQLITE_ROW)
  public
    /// the field name of the current ROW
    function FieldName(Col: integer): RawUtf8;
    /// the field index matching this name
    // - return -1 if not found
    function FieldIndex(const aColumnName: RawUtf8): integer;
    /// return the field as a sqlite3.value object handle, first Col is 0
    function FieldValue(Col: integer): TSqlite3Value;
    /// return a field integer value, first Col is 0
    function FieldInt(Col: integer): Int64;
    /// return a field floating point value, first Col is 0
    function FieldDouble(Col: integer): double;
    /// return a field UTF-8 encoded text value, first Col is 0
    function FieldUtf8(Col: integer): RawUtf8;
    /// return a text value value as generic VCL string, first Col is 0
    // - note that prior to Delphi 2009, you may loose content during conversion
    function FieldS(Col: integer): string;
    /// return a field as Win-Ansi (i.e. code page 1252) encoded text value, first Col is 0
    function FieldA(Col: integer): WinAnsiString;
    /// return a field RawUnicode encoded text value, first Col is 0
    function FieldW(Col: integer): RawUnicode;
    /// return a field as a blob value (RawByteString/RawBlob is an AnsiString),
    // first Col is 0
    function FieldBlob(Col: integer): RawByteString;
    /// return a field as a TStream blob value, first Col is 0
    // - caller shall release the returned TStream instance
    function FieldBlobToStream(Col: integer): TStream;
    /// return TRUE if the column value is NULL, first Col is 0
    function FieldNull(Col: integer): boolean;
    /// return the field type of this column
    // - retrieve the "SQLite3" column type as returned by sqlite3.column_type -
    //  i.e. SQLITE_NULL, SQLITE_INTEGER, SQLITE_FLOAT, SQLITE_TEXT, or SQLITE_BLOB
    function FieldType(Col: integer): integer;
    /// return the type of this column, as declared at creation
    // - textual type used for CREATE TABLE of the corresponding column, as
    // returned by sqlite3.column_decltype()
    function FieldDeclaredType(Col: integer): RawUtf8;
    /// return the generic VCL string type of this column, as declared at creation
    // - textual type used for CREATE TABLE of corresponding column, as
    // returned by sqlite3.column_decltype()
    // - note that prior to Delphi 2009, you may loose content during conversion
    function FieldDeclaredTypeS(Col: integer): string;
    /// append all columns values of the current Row to a JSON stream
    // - will use WR.Expand to guess the expected output format
    // - BLOB field value is saved as Base64, in the '"\uFFF0base64encodedbinary"
    // format and contains true BLOB data
    procedure FieldsToJson(WR: TJsonWriter; DoNotFetchBlobs: boolean = false);
    /// the column/field count of the current ROW
    // - fields numerotation starts with 0
    property FieldCount: integer
      read fFieldCount;
    /// the bound parameters count
    property ParamCount: integer
      read GetParamCount;
  end;

  /// used to retrieve a SQLite3 prepared statement
  TSqlStatementCache = record
    /// associated SQL statement
    // - should be the first field for propery Caches: TDynArrayHashed search
    StatementSql: RawUtf8;
    /// associated prepared statement, ready to be executed after binding
    Statement: TSqlRequest;
    /// used to monitor execution time
    Timer: TSynMonitor;
  end;
  /// used to store all prepared statement
  TSqlStatementCacheDynArray = array of TSqlStatementCache;

  /// handle a cache of SQLite3 prepared statements
  // - is defined either as an object either as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  {$ifdef USERECORDWITHMETHODS}
  TSqlStatementCached = record
  {$else}
  TSqlStatementCached = object
  {$endif USERECORDWITHMETHODS}
    /// prepared statements with parameters for faster SQLite3 execution
    // - works for SQL code with ? internal parameters
    Cache: TSqlStatementCacheDynArray;
    /// current number of items in the Cache[] array
    Count: integer;
    /// index in Cache[] of the last Prepare() call
    LastPrepared: integer;
    /// hashing wrapper associated to the Cache[] array
    Caches: TDynArrayHashed;
    /// the associated SQLite3 database instance
    // - any direct access to this cache list should be protected via DB.Lock
    DB: TSqlite3DB;
    /// intialize the cache
    procedure Init(aDB: TSqlite3DB);
    /// add or retrieve a generic SQL (with ? parameters) statement from cache
    function Prepare(const GenericSql: RawUtf8; WasPrepared: PBoolean = nil;
      ExecutionTimer: PPPrecisionTimer = nil;
      ExecutionMonitor: PSynMonitor = nil): PSqlRequest;
    /// used internally to release all prepared statements from Cache[]
    procedure ReleaseAllDBStatements;
    /// could be used e.g. for statistics
    // - will use internally the function StatementCacheTotalTimeCompare()
    procedure SortCacheByTotalTime(var aIndex: TIntegerDynArray);
  end;

  /// those classes can be used to define custom SQL functions inside a TSqlDataBase
  TSqlDataBaseSQLFunction = class
  protected
    fInternalFunction: TSqlFunctionFunc;
    fSqlName: RawUtf8;
    fFunctionParametersCount: integer;
    function CreateFunction(DB: TSqlite3DB): integer; virtual;
  public
    /// initialize the corresponding SQL function
    // - expects at least the low-level TSqlFunctionFunc implementation (in
    // sqlite3.create_function() format) and the number of expected parameters
    // - if the function name is not specified, it will be retrieved from the type
    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
    constructor Create(aFunction: TSqlFunctionFunc;
      aFunctionParametersCount: integer;
      const aFunctionName: RawUtf8=''); reintroduce;
    /// the internal function prototype
    // - ready to be assigned to sqlite3.create_function() xFunc parameter
    property InternalFunction: TSqlFunctionFunc
      read fInternalFunction;
    /// the SQL function name, as called from the SQL statement
    // - the same function name may be registered several times with a diverse
    // number of parameters (e.g. to implement optional parameters)
    property FunctionName: RawUtf8
      read fSqlName;
    /// the number of parameters expected by the SQL function
    property FunctionParametersCount: integer
      read fFunctionParametersCount;
  end;

  /// to be used to define custom SQL functions for dynamic arrays BLOB search
  TSqlDataBaseSQLFunctionDynArray = class(TSqlDataBaseSQLFunction)
  protected
    fDummyDynArray: TDynArray;
    fDummyDynArrayValue: pointer;
  public
    /// initialize the corresponding SQL function
    // - if the function name is not specified, it will be retrieved from the type
    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
    // - the SQL function will expect two parameters: the first is the BLOB
    // field content, and the 2nd is the array element to search (set with
    // TDynArray.ElemSave() or with BinToBase64WithMagic(aDynArray.ElemSave())
    // if called via a Client and a JSON prepared parameter)
    // - you should better use the already existing faster SQL functions
    // Byte/Word/Integer/Cardinal/Int64/CurrencyDynArrayContains() if possible
    // (this implementation will allocate each dynamic array into memory before
    // comparison, and will be therefore slower than those optimized versions)
    constructor Create(aTypeInfo: PRttiInfo; aCompare: TDynArraySortCompare;
      const aFunctionName: RawUtf8 = ''); reintroduce;
  end;

  /// Stored Procedure prototype, used by TSqlDataBase.Execute() below
  // - called for every row of a Statement
  // - the implementation may update the database directly by using a
  // local or shared TSqlRequest
  // - the TSqlRequest may be shared and prepared before the call for even
  // faster access than with a local TSqlRequest
  // - no TSqlDataBase or higher levels objects can be used inside this method,
  // since all locking and try..finally protection is outside it
  // - can optionnaly trigger a ESqlite3Exception on any error
  TOnSqlStoredProc = procedure(const Statement: TSqlRequest) of object;

  /// TSqlDataBase.TransactionBegin can be deferred, immediate, or exclusive
  // - tbDeferred means that no locks are acquired on the database until the
  // database is first accessed. Thus with a deferred transaction, the BEGIN
  // statement itself does nothing to the filesystem. Locks are not acquired
  // until the first read or write operation. The first read operation against
  // a database creates a SHARED lock and the first write operation creates a
  // RESERVED lock. Because the acquisition of locks is deferred until they are
  // needed, it is possible that another thread or process could create a
  // separate transaction and write to the database after the BEGIN on the
  // current thread has executed.
  // - If the transaction is tbImmediate, then RESERVED locks are acquired
  // on all databases as soon as the BEGIN command is executed, without waiting
  // for the database to be used. After a BEGIN IMMEDIATE, no other database
  // connection will be able to write to the database or do a BEGIN IMMEDIATE
  // or BEGIN EXCLUSIVE. Other processes can continue to read from the database,
  // however.
  // - A tbExclusive transaction causes EXCLUSIVE locks to be acquired on all
  // databases. After a BEGIN EXCLUSIVE, no other database connection except
  // for read_uncommitted connections will be able to read the database and
  // no other connection without exception will be able to write the database
  // until the transaction is complete.
  TSqlDataBaseTransactionBehaviour = (
    tbDeferred,
    tbImmediate,
    tbExclusive);

  {$M+}
  TSqlDatabaseBackupThread = class;
  {$M-}

  /// callback called asynchronously during TSqlDatabase.BackupBackground()
  // - implementation should return TRUE to continue the process: if the method
  // returns FALSE, backup will be aborted, and destination file deleted
  // - this method allows to monitor the backup process, thanks to
  // TSqlDatabaseBackupThread properties (especialy the Step property)
  // - this method will be executed in the context of the associated
  // TSqlDatabaseBackupThread: so you should use Synchronize() to update the UI
  TOnSqlDatabaseBackup =
    function(Sender: TSqlDatabaseBackupThread): boolean of object;

  /// simple wrapper for direct SQLite3 database manipulation
  // - embed the SQLite3 database calls into a common object
  // - thread-safe call of all SQLite3 queries (SQLITE_THREADSAFE 0 in sqlite.c)
  // - can cache last results for SELECT statements, if property UseCache is true:
  //  this can speed up most read queries, for web server or client UI e.g.
  TSqlDataBase = class(TSynPersistentLock)
  protected
    fDB: TSqlite3DB;
    fFileName: TFileName;
    fFileNameWithoutPath: TFileName;
    fPageSize, fFileDefaultPageSize: cardinal;
    fFileDefaultCacheSize: integer;
    fIsMemory: boolean;
    fPassword: RawUtf8;
    fTransactionActive: boolean;
    /// if not nil, cache is used - see UseCache property
    fCache: TSynCache;
    fInternalState: PCardinal;
    fBusyTimeout: integer;
    fOpenV2Flags: integer;
    fBackupBackgroundInProcess: TSqlDatabaseBackupThread;
    fBackupBackgroundLastTime: RawUtf8;
    fBackupBackgroundLastFileName: TFileName;
    fUseCacheSize: integer;
    fStatementMaxMemory: PtrUInt;
    fLogResultMaximumSize: integer;
    fLog: TSynLogClass;
    /// store TSqlDataBaseSQLFunction instances
    fSqlFunctions: TSynObjectList;
    function GetUseCache: boolean;
    procedure SetUseCache(const Value: boolean);
    procedure SetBusyTimeout(const ms: integer);
    function GetUserVersion: cardinal;
    procedure SetUserVersion(const Value: cardinal);
    procedure SetWALMode(Value: boolean);
    function GetWALMode: boolean;
    procedure SetSynchronous(const Value: TSqlSynchronousMode);
    function GetSynchronous: TSqlSynchronousMode;
    procedure SetLockingMode(const Value: TSqlLockingMode);
    function GetLockingMode: TSqlLockingMode;
    function GetCacheSize: cardinal;
    procedure SetCacheSize(const Value: cardinal);
    function GetPageSize: cardinal;
    procedure SetPageSize(const Value: cardinal);
    function GetPageCount: cardinal;
    function GetFileSize: Int64;
    function GetMemoryMappedMB: cardinal;
    procedure SetMemoryMappedMB(const Value: cardinal);
    function GetLimit(Category: TSqlLimitCategory): integer;
    procedure SetLimit(Category: TSqlLimitCategory; Value: integer);
    function GetBackupBackgroundInProcess: boolean;
    function SQLShouldBeLogged(const aSql: RawUtf8): boolean;
    function GetSqlite3Library: TSqlite3Library; // class function = bug in D2005
  public
    /// enter the internal mutex: called before any DB access
    // - provide the SQL statement about to be executed: handle proper caching
    // - if the SQL statement is void, assume a SELECT statement (no cache flush)
    procedure Lock(const aSql: RawUtf8); overload;
    /// enter the internal mutex without any cache flush
    // - same as Lock('');
    procedure Lock; overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// flush the internal statement cache, and enter the internal mutex
    // - same as Lock('ALTER');
    procedure LockAndFlushCache;
    /// leave the internal mutex: called after any DB access
    procedure UnLock;
      {$ifdef HASINLINE}inline;{$endif}
    /// enter the internal mutex: called before any DB access
    // - provide the SQL statement about to be executed: handle proper caching
    // - if this SQL statement has an already cached JSON response, return it and
    // don't enter the internal mutex: no UnLockJson() call is necessary
    // - if this SQL statement is not a SELECT, cache is flushed and
    // the next call to UnLockJson() won't add any value to the cache since
    // this statement is not a SELECT and doesn't have to be cached!
    // - if aResultCount does map to an integer variable, it will be filled
    // with the returned row count of data (excluding field names) in the result
    function LockJson(const aSql: RawUtf8; aResultCount: PPtrInt): RawUtf8;
    /// leave the internal mutex: called after any DB access
    // - caller must provide the JSON result for the SQL statement previously set
    //  by LockJson()
    // - do proper caching of the JSON response for this SQL statement
    procedure UnLockJson(const aJsonResult: RawUtf8; aResultCount: PtrInt);
    /// (re)open the database from file fFileName
    // - TSqlDatabase.Create already opens the database: this method is to be
    // used only on particular cases, e.g. to close temporary a DB file and
    // allow making a backup on its content
    // - returns the SQLITE_* status code, as retrieved from sqlite3.open()
    // so that it should be SQLITE_OK on success
    function DBOpen: integer; virtual;
    /// close the opened database
    // - TSqlDatabase.Destroy already closes the database: this method is to be
    // used only on particular cases, e.g. to close temporary a DB file and
    // allow making a backup on its content
    // - returns the SQLITE_* status code, as retrieved from sqlite3.close(fDB)
    // so that it should be SQLITE_OK on success
    function DBClose: integer;
    /// for SQLite >= 3.11 - enable registation of a custom tokenizer
    // - see details at http://sqlite.org/fts3.html#f3tknzr
    function EnableCustomTokenizer: integer;
  public
    /// open a SQLite3 database file
    // - open an existing database file or create a new one if no file exists
    // - if specified, the password will be used to cypher this file on disk
    // (the main SQLite3 database file is encrypted, not the wal file during run);
    // the password may be a JSON-serialized TSynSignerParams object, or will use
    // AES-OFB-128 after SHAKE_128 with rounds=1000 and a fixed salt on plain
    // password text; note that our custom encryption is not compatible with the
    // official SQLite Encryption Extension module
    // - you can specify some optional flags for sqlite3.open_v2() as
    // SQLITE_OPEN_READONLY or SQLITE_OPEN_READWRITE instead of supplied default
    // value (which corresponds to the sqlite3.open() behavior)
    // - by default, 10000 pages are used to cache data in memory (using around
    // 40 MB of RAM), but you may specify another value for performance tuning
    // - SYSTEMNOCASE collation is registered, for WinAnsi CP-1252 comparison
    // via the Utf8ILComp() function from mormot.core.unicode.pas
    // - UNICODENOCASE collation is registered, for Unicode 10.0 comparison as
    // implemented by the Utf8ILCompReference() from mormot.core.unicode.pas
    // - WIN32CASE and WIN32NOCASE collations are registered, calling the
    // Operating System (Win32 API or ICU) - so may not be consistent when you
    // move the SQLite3 database file: consider SYSTEMNOCASE or UNICODENOCASE
    // - ISO8601 collation is added (TDateTime stored as ISO-8601 encoded TEXT)
    // - some additional SQL functions are registered: MOD, SOUNDEX/SOUNDEXFR/SOUNDEXES,
    // RANK, CONCAT, TIMELOG, TIMELOGUNIX, JSONGET/JSONHAS/JSONSET and TDynArray-Blob
    // Byte/Word/Integer/Cardinal/Int64/Currency/RawUtf8DynArrayContains
    // - initialize a internal mutex to ensure that all access to the database is atomic
    // - raise an ESqlite3Exception on any error
    constructor Create(const aFileName: TFileName; const aPassword: RawUtf8 = '';
      aOpenV2Flags: integer = 0; aDefaultCacheSize: integer = 10000;
      aDefaultPageSize: integer = 4096); reintroduce;
    /// close a database and free its memory and context
    //- if TransactionBegin was called but not commited, a RollBack is performed
    destructor Destroy; override;
    /// Execute all SQL statements in aSql UTF-8 encoded string
    // - can be prepared with TransactionBegin()
    // - raise an ESqlite3Exception on any error
    procedure ExecuteAll(const aSql: RawUtf8);
    /// Execute one SQL statements in aSql UTF-8 encoded string
    // - can be prepared with TransactionBegin()
    // - raise an ESqlite3Exception on any error
    procedure Execute(const aSql: RawUtf8); overload;
    /// Execute one SQL statement which return integers from the aSql UTF-8 encoded string
    // - Execute the first statement in aSql
    // - this statement must get a one field/column result of INTEGER
    // - return result as a dynamic array of Int64 in aValues[]
    // - return count of row in integer function result (may be < length(ID))
    // - raise an ESqlite3Exception on any error
    function Execute(const aSql: RawUtf8;
      var aValues: TInt64DynArray): integer; overload;
    /// Execute one SQL statement returning TEXT from the aSql UTF-8 encoded string
    // - Execute the first statement in aSql
    // - this statement must get (at least) one field/column result of TEXT
    // - return result as a dynamic array of RawUtf8 in aValues[]
    // - return count of row in integer function result (may be < length(ID))
    // - raise an ESqlite3Exception on any error
    function Execute(const aSql: RawUtf8;
      var aValues: TRawUtf8DynArray): integer; overload;
    /// Execute one SQL statement which returns one integer from the aSql UTF-8 encoded string
    // - Execute the first statement in aSql
    // - this statement must get a one field/column result of INTEGER in aValue
    // - raise an ESqlite3Exception on any error
    procedure Execute(const aSql: RawUtf8;
      out aValue: Int64; NoLog: boolean = false); overload;
    /// Execute one SQL statement which returns one UTF-8 encoded string value
    // - Execute the first statement in aSql
    // - this statement must get a one field/column result of TEXT in aValue
    // - raise an ESqlite3Exception on any error
    procedure Execute(const aSql: RawUtf8;
      out aValue: RawUtf8; NoLog: boolean = false); overload;
    /// Execute one SQL statements in aSql UTF-8 encoded string
    // - can be prepared with TransactionBegin()
    // - raise no Exception on error, but returns FALSE in such case
    function ExecuteNoException(const aSql: RawUtf8): boolean;
    /// Seamless execution of a SQL statement which returns one integer
    // - Execute the first statement in aSql
    // - this statement must get a one field/column result of INTEGER
    // - returns 0 on any error
    function ExecuteNoExceptionInt64(const aSql: RawUtf8): Int64;
    /// Seamless execution of a SQL statement which returns one UTF-8 encoded string
    // - Execute the first statement in aSql
    // - this statement must get a one field/column result of TEXT
    // - returns '' on any error
    function ExecuteNoExceptionUtf8(const aSql: RawUtf8): RawUtf8;
    /// Execute one SQL statement returning its results in JSON format
    // - the BLOB data is encoded as '"\uFFF0base64encodedbinary"'
    function ExecuteJson(const aSql: RawUtf8; Expand: boolean = false;
      aResultCount: PPtrInt = nil): RawUtf8;
    /// returns the EXPLAIN QUERY PLAN raw text of a given SQL statement
    // - the result layout is not fixed, and subject to change from one release
    // of SQLite to the next: so we expand it as extended JSON
    function ExplainQueryPlan(const aSql: RawUtf8): RawUtf8;
    /// begin a transaction
    // - Execute SQL statements with Execute() procedure below
    // - must be ended with Commit on success
    // - must be aborted with Rollback after an ESqlite3Exception raised
    // - The default transaction behavior is tbDeferred
    procedure TransactionBegin(
      aBehavior: TSqlDataBaseTransactionBehaviour = tbDeferred);
    /// end a transaction: write all Execute() statements to the disk
    procedure Commit;
    /// abort a transaction: restore the previous state of the database
    procedure RollBack;
    /// return the last Insert Rowid
    function LastInsertRowID: Int64;
    /// count the number of rows modified by the last SQL statement
    // - this method returns the number of database rows that were changed or
    // inserted or deleted by the most recently completed SQL statement on the
    // database connection specified by the first parameter. Only changes that
    // are directly specified by the INSERT, UPDATE, or DELETE statement are counted.
    // - wrapper around the sqlite3.changes() low-level function
    function LastChangeCount: integer;
    /// return the number of row changes caused by INSERT, UPDATE or
    // DELETE statements since the database connection was opened
    // - wrapper around the sqlite3.total_changes() low-level function
    function TotalChangeCount: integer;

    /// get all table names contained in this database file
    procedure GetTableNames(var Names: TRawUtf8DynArray);
    /// get all field names for a specified Table
    procedure GetFieldNames(var Names: TRawUtf8DynArray;
      const TableName: RawUtf8);
    /// check if the given table do exist
    function HasTable(const Name: RawUtf8): boolean;
    /// add a SQL custom function to the SQLite3 database engine
    // - the supplied aFunction instance will be used globally and freed
    // by TSqlDataBase.Destroy destructor
    // - will do nothing if the same function name and parameters count have
    // already been registered (you can register then same function name with
    // several numbers of parameters)
    // - you may use the overloaded function, which is a wrapper around:
    // ! Demo.RegisterSQLFunction(
    // !   TSqlDataBaseSQLFunction.Create(InternalSQLFunctionCharIndex,2,'CharIndex'));
    procedure RegisterSQLFunction(aFunction: TSqlDataBaseSQLFunction); overload;
    /// add a SQL custom function to the SQLite3 database engine
    // - will do nothing if the same function name and parameters count have
    // already been registered (you can register then same function name with
    // several numbers of parameters)
    // - typical use may be:
    // ! Demo.RegisterSQLFunction(InternalSQLFunctionCharIndex,2,'CharIndex');
    procedure RegisterSQLFunction(aFunction: TSqlFunctionFunc;
      aFunctionParametersCount: integer; const aFunctionName: RawUtf8); overload;
    /// add a SQL custom function for a dynamic array to the database
    // - the resulting SQL function will expect two parameters: the first is the
    // BLOB field content, and the 2nd is the array element to search (as set with
    // TDynArray.ElemSave() or with BinToBase64WithMagic(aDynArray.ElemSave())
    // if called via a Client and a JSON prepared parameter)
    // - if the function name is not specified, it will be retrieved from the type
    // information (e.g. TReferenceDynArray will declare 'ReferenceDynArray')
    // - you should better use the already existing faster SQL functions
    // Byte/Word/Integer/Cardinal/Int64/CurrencyDynArrayContains() if possible
    // (this implementation will allocate each dynamic array into memory before
    // comparison, and will be therefore slower than those optimized versions -
    // but it will be always faster than Client-Server query, in all cases)
    procedure RegisterSQLFunction(aDynArrayTypeInfo: PRttiInfo;
      aCompare: TDynArraySortCompare;
      const aFunctionName: RawUtf8 = ''); overload;

    /// open a BLOB incrementally for read[/write] access
    // - find a BLOB located in row RowID, column ColumnName, table TableName
    // in database DBName; in other words, the same BLOB that would be selected by:
    // ! SELECT ColumnName FROM DBName.TableName WHERE rowid = RowID;
    // - use after a TSqlRequest.BindZero() to reserve Blob memory
    // - if RowID=0, then the last inserted RowID is used (beware that this
    // value won't be thread-safe, if another thread run another INSERT)
    // - will raise an ESqlite3Exception on any error
    function Blob(const DBName, TableName, ColumnName: RawUtf8;
      RowID: Int64; ReadWrite: boolean = false): TSqlBlobStream;
    /// backup of the opened Database into an external file name
    // - warning: this method won't use the SQLite Online Backup API
    // - database is closed, VACCUUMed, copied, then reopened: it's very fast for
    // small databases, but is blocking and should be an issue
    // - if you use some virtual tables, they won't be restored after backup:
    // this method would probably fail e.g. in the context of our ORM
    function Backup(const BackupFileName: TFileName): boolean;
    /// backup of the opened Database into an external file name
    // - this method will use the SQLite Online Backup API and a dedicated
    // background thread for the process
    // - this will be asynchronous, and would block the main database process
    // only when copying the StepPageNumber numer of pages for each step,
    // waiting StepSleepMS milliseconds before performing the next step:
    // as a result, the copy operation can be done incrementally, by blocks of
    // StepPageNumber pages, in which case the source database does not need
    // to be locked for the duration of the copy, only for the brief periods of
    // time when it is actually being read from: this allows other database
    // users to continue uninterrupted (at least during StepSleepMS
    // milliseconds) while a backup is running
    // - if StepPageNumber is -1, the whole DB will be copied in a single step,
    // therefore in blocking mode, e.g. with BackupBackgroundWaitUntilFinished
    // - if SynLzCompress is TRUE, the backup file would be compressed using
    // FileSynLZ() function - you may use BackupUnSynLZ() class method to
    // uncompress the .dbsynlz file into a proper SQLite3 file
    // - the supplied OnProgress event handler will be called at each step, in
    // the context of the background thread
    // - the background thread will be released when the process is finished
    // - if only one connection to the database does exist (e.g. if you use only
    // one TSqlDataBase instance on the same database file), any modification
    // to the source database during the background process will be included in
    // the backup - so this method will work perfectly e.g. for our ORM
    // - if specified, a password will be used to cypher BackupFileName on disk
    // (it will work only with mormot.db.raw.sqlite3.static) - you can uncypher
    // the resulting encrypted database file via ChangeSQLEncryptTablePassWord()
    // - returns TRUE if backup started as expected, or FALSE in case of error
    // (e.g. if there is already another backup started, if the source or
    // destination databases are locked or invalid, or if the sqlite3.dll is too
    // old and does not support the Online Backup API)
    // - you can also use this method to save an SQLite3 ':memory:' database,
    // perhaps in conjunction with the BackupBackgroundWaitUntilFinished method
    function BackupBackground(const BackupFileName: TFileName;
      StepPageNumber, StepSleepMS: integer;
      const OnProgress: TOnSqlDatabaseBackup;
      SynLzCompress: boolean = false; const aPassword: RawUtf8 = ''): boolean;
    /// background backup to another opened database instance
    // - in respect to BackupBackground method, it will use an existing database
    // the actual process
    // - by design, SynLZCompress or aPassword parameters are unavailable
    function BackupBackgroundToDB(BackupDB: TSqlDatabase;
      StepPageNumber, StepSleepMS: integer;
      const OnProgress: TOnSqlDatabaseBackup): boolean;
    /// wait until any previous BackupBackground() is finished
    // - warning: this method won't call the Windows message loop, so should not
    // be called from main thread, unless the UI may become unresponsive: you
    // should better rely on OnProgress() callback for any GUI application
    // - by default, it will wait forever so that process is finished, but you
    // can set a time out (in seconds) after which the process will be aborted
    // - could be used with BackupBackground() and StepPageNumber=-1 to perform
    // a whole copy of a database in one shot:
    // ! if aDB.BackupBackground('backup.db3',-1,0,nil) then
    // !   aDB.BackupBackgroundWaitUntilFinished;
    procedure BackupBackgroundWaitUntilFinished(TimeOutSeconds: integer = -1);
    /// uncompress a .dbsynlz backup file as previously compressed with BackupSynLZ()
    // or if SynLZCompress parameter is TRUE for BackupBackground() method
    // - any DestDB file name would be overwritten
    // - returns TRUE on success, FALSE on failure
    class function BackupUnSynLZ(const SourceSynLZ, DestDB: TFileName): boolean;
    /// compress a SQlite3 file into a proprietary but efficient .dbsynlz layout
    // - same format than BackupUnSynLZ() class method or if SynLZCompress
    // parameter is TRUE for BackupBackground() method
    // - the SourceDB file should not be active (e.g. be a backup file), i.e.
    // not currently opened by the SQlite3 engine, otherwise behavior is unknown
    // - returns TRUE on success, FALSE on failure
    class function BackupSynLZ(const SourceDB, DestSynLZ: TFileName;
      EraseSourceDB: boolean): boolean;
    /// returns TRUE if the supplied name is a SQlite3 .dbsynlz compressed file
    // - i.e. on the format generated by the BackupUnSynLZ() class method or
    // if SynLZCompress parameter is TRUE for BackupBackground() method
    class function IsBackupSynLZFile(const SynLZFile: TFileName): boolean;
    /// flush the internal SQL-based JSON cache content
    // - to be called when the regular Lock/LockJson methods are not called,
    // e.g. with external tables as defined in SQLite3DB unit
    // - will also increment the global InternalState property value (if set)
    procedure CacheFlush;

    /// read-only access to the SQLite3 database handle
    property DB: TSqlite3DB
      read fDB;
    /// read-only access to the SQlite3 password used for encryption
    // - may be a JSON-serialized TSynSignerParams object, or will use AES-OFB-128
    // after SHAKE_128 with rounds=1000 and a fixed salt on plain password text
    property Password: RawUtf8
      read fPassword;
    /// read-only access to the SQLite3 database filename opened without its path
    property FileNameWithoutPath: TFileName
      read fFileNameWithoutPath;
    /// access to the internal JSON cache, used by ExecuteJson() method
    // - see UseCache property and CacheFlush method
    property Cache: TSynCache
      read fCache;
    /// retrieve of define a limit on the current database connection
    // - see TSqlLimitCategory for a details of all available limits
    // - see @http://www.sqlite.org/c3ref/limit.html
    property Limit[Category: TSqlLimitCategory]: integer
      read GetLimit write SetLimit;
    /// maximum bytes allowed for FetchAllToJSON/FetchAllToBinary methods
    // - if a result set exceeds this limit, an ESQLDBException is raised
    // - default is 512 shl 20, i.e. 512MB which is very high
    // - avoid unexpected OutOfMemory errors when incorrect statement is run
    property StatementMaxMemory: PtrUint
      read fStatementMaxMemory write fStatementMaxMemory;
    /// access to the log class associated with this SQLite3 database engine
    // - can be customized, e.g. by overriden TRestServerDB.SetLogClass()
    property Log: TSynLogClass
      read fLog write fLog;
    /// sets a maximum size (in bytes) to be logged as sllResult rows
    // - by default, is set to 512 bytes, which sounds a good compromise
    // since it does not make sense to log all the JSON content retrieved from
    // the database engine, when a huge SELECT is executed
    property LogResultMaximumSize: integer
      read fLogResultMaximumSize write fLogResultMaximumSize;
    /// this integer pointer (if not nil) is incremented when any SQL statement
    // changes the database contents (i.e. any not SELECT statement)
    // - this pointer is thread-safe updated, inside a critical section
    property InternalState: PCardinal
      read fInternalState write fInternalState;
  published
    /// read-only access to the SQLite3 database filename opened
    property FileName: TFileName
      read fFileName;
    /// equals TRUE if the SQLite3 database was created as ':memory:'
    // (i.e. SQLITE_MEMORY_DATABASE_NAME)
    property IsMemory: boolean
      read fIsMemory;
    /// if this property is set, all ExecuteJson() responses will be cached
    // - cache is flushed on any write access to the DB (any not SELECT statement)
    // - cache is consistent only if ExecuteJson() Expand parameter is constant
    // - cache is used by TSqlDataBase.ExecuteJson() and TOrmTableDB.Create()
    property UseCache: boolean
      read GetUseCache write SetUseCache;
    /// cache size in JSON bytes, to be set before UseCache is set to true
    // - default is 16MB
    property UseCacheSize: integer
      read fUseCacheSize write fUseCacheSize;
    /// return TRUE if a Transaction begun
    property TransactionActive: boolean
      read fTransactionActive;
    /// sets a busy handler that sleeps for a specified amount of time
    // (in milliseconds) when a table is locked, before returning an error
    property BusyTimeout: integer
      read fBusyTimeout write SetBusyTimeout;
    /// query or change the suggested maximum number of database disk pages
    // that SQLite will hold in memory at once per open database file
    // - DBOpen method will set this cache size to a big 10000 default, which
    // sounds reasonnable in the context of a server application (will use
    // up to 40 MB of memory cache, with the default PageSize of 4096 bytes)
    // - when you change the cache size using the cache_size pragma, the change
    // only endures for the current session. The cache size reverts to the
    // default value when the database is closed and reopened
    // - we do not handle negative values here (i.e. KB of RAM), since it won't
    // work if the linked SQLite3 library is version 3.7.9 and earlier
    property CacheSize: cardinal
      read GetCacheSize write SetCacheSize;
    /// query or change the page size of the database
    // - the page size must be a power of two between 512 and 65536 inclusive
    // - DBOpen method will set the PageSize to 4096 (if the database is not
    // encrypted), which sounds better than the default 1024 value - you should
    // not have to set this property usually
    // - setting this property will only cause an immediate change in the page
    // size if it is issued while the database is still empty, prior to the
    // first CREATE TABLE statement; if this property is used to specify a new
    // page size just prior to running the VACUUM command and if the database
    // is not in WAL journal mode then VACUUM will change the page size to the
    // new value for the newly created database file
    property PageSize: cardinal
      read GetPageSize write SetPageSize;
    /// return the total number of pages in the database file
    property PageCount: cardinal
      read GetPageCount;
    /// return the total number of bytes in the database file
    // - computes PageSize*PageCount
    property FileSize: Int64
      read GetFileSize;
    /// query or change the Write-Ahead Logging mode for the database
    // - beginning with version 3.7 of the SQLite3 engine, a new "Write-Ahead Log"
    // option (hereafter referred to as "WAL") is optionaly available
    // - WAL might be very slightly slower (perhaps 1% or 2% slower) than the
    // traditional rollback-journal approach in applications that do mostly reads
    // and seldom write; but WAL provides more concurrency as readers do not block
    // writers and a writer does not block readers. Reading and writing can
    // proceed concurrently. With our SQLite3 framework, it's not needed.
    // - by default, this option is not set: only implement if you really need it,
    // but our SQlite3 framework use locked access to the databse, so there
    // should be no benefit of WAL for the framework; but if you call
    // directly TSqlDatabase instances in your code, it may be useful to you
    property WALMode: boolean
      read GetWALMode write SetWALMode;
    /// query or change the SQlite3 file-based syncrhonization mode, i.e. the
    // way it waits for the data to be flushed on hard drive
    // - default smFull is very slow, but achieve 100% ACID behavior
    // - smNormal is faster, and safe until a catastrophic hardware failure occurs
    // - smOff is the fastest, data should be safe if the application crashes,
    // but database file may be corrupted in case of failure at the wrong time
    property Synchronous: TSqlSynchronousMode
      read GetSynchronous write SetSynchronous;
    /// query or change the SQlite3 file-based locking mode, i.e. the
    // way it locks the file
    // - default lmNormal is ACID and safe
    // - lmExclusive gives better performance in case of a number of write
    // transactions, so can be used to release a mORMot server power: but you
    // won't be able to access the database file from outside the process (like
    // a "normal" database engine)
    property LockingMode: TSqlLockingMode
      read GetLockingMode write SetLockingMode;
    /// enables or disables disk content access using memory-mapped I/O
    // - 0 to disable it (the default, because of potential disadvantages)
    // - set to a number of Mega Bytes value of memory for the mapping
    // - expects a SQLite3 engine version >= 3.7.17
    // - Memory-Mapped I/O is NOT compatible with password encryption as
    // implemented in our mormot.db.raw.sqlite3.static unit
    property MemoryMappedMB: cardinal
      read GetMemoryMappedMB write SetMemoryMappedMB;
    /// retrieve or set the user_version stored in the SQLite3 database file
    // - user-version is a 32-bit signed integer stored in the database header
    //- it can be used to change the database in case of format upgrade (e.g.
    // refresh some hand-made triggers)
    property user_version: cardinal
      read GetUserVersion write SetUserVersion;
    /// reflects how the database connection was created in the constructor
    property OpenV2Flags: integer
      read fOpenV2Flags;
    /// is set to TRUE while a BackupBackground() process is still running
    // - see also BackupBackgroundWaitUntilFinished() method
    property BackupBackgroundInProcess: boolean
      read GetBackupBackgroundInProcess;
    /// how much time did the latest BackupBackground() finished process take
    property BackupBackgroundLastTime: RawUtf8
      read fBackupBackgroundLastTime;
    /// the latest BackupBackground() process file name
    property BackupBackgroundLastFileName: TFileName
      read fBackupBackgroundLastFileName;
    /// the SQLite3 library which is currently running
    // - part of TSqlDatabase published properties, to publish e.g. Version
    property SQLite3Library: TSqlite3Library
      read GetSqlite3Library;
  end;

  /// used to read or write a BLOB Incrementaly
  // - data is read/written directly from/to the SQLite3 BTree
  // - data can be written after a TSqlRequest.BindZero() call to reserve memory
  // - this TStream has a fixed size, but Position property can be used to rewind
  TSqlBlobStream = class(TStream)
  protected
    fBlob: TSqlite3Blob;
    fDB: TSqlite3DB;
    fSize, fPosition: Int64;
    fWritable: boolean;
  public
    /// Opens a BLOB located in row RowID, column ColumnName, table TableName
    // in database DBName; in other words, the same BLOB that would be selected by:
    // ! SELECT ColumnName FROM DBName.TableName WHERE rowid = RowID;
    constructor Create(aDB: TSqlite3DB; const DBName, TableName,
      ColumnName: RawUtf8; RowID: Int64; ReadWrite: boolean);
    /// release the BLOB object
    destructor Destroy; override;
    /// read Count bytes from the opened BLOB in Buffer
    function Read(var Buffer; Count: Longint): Longint; override;
    /// write is allowed for in-place replacement (resizing is not allowed)
    // - Create() must have been called with ReadWrite=true
    function Write(const Buffer; Count: Longint): Longint; override;
    /// change the current read position
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    /// change the current read position
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    /// reuse this class instance with another row of the same table
    // - will update the stream size, and also rewind position to the beginning
    // - it is actually faster than creating a new TSqlBlobStream instance
    procedure ChangeRow(RowID: Int64);
    /// read-only access to the BLOB object handle
    property Handle: TSqlite3Blob
      read fBlob;
  end;

  /// kind of event triggerred during TSqlDatabase.BackupBackground() process
  // - you can use (Sender.Step in backupAnyStep), to check for normal step,
  // or (Sender.Step in backupFinished) to check for process end
  TOnSqlDatabaseBackupStep = (
    backupNone,
    backupStart,
    backupSuccess,
    backupFailure,
    backupStepOk,
    backupStepBusy,
    backupStepLocked,
    backupStepSynLz);

  /// background thread used for TSqlDatabase.BackupBackground() process
  TSqlDatabaseBackupThread = class(TThread)
  protected
    fBackupDestFile: TFileName;
    fSourceDB: TSqlDatabase;
    fDestDB: TSqlDatabase;
    fStepPageNumber, fStepSleepMS: integer;
    fBackup: TSqlite3Backup;
    fStep: TOnSqlDatabaseBackupStep;
    fStepNumberToFinish, fStepNumberTotal: integer;
    fStepSynLzCompress: boolean;
    fOnProgress: TOnSqlDatabaseBackup;
    fError: Exception;
    fTimer: TPrecisionTimer;
    fOwnerDest: boolean;
    /// main process
    procedure Execute; override;
  public
    /// initialize the background thread
    // - execution is started immediately - caller may call the WaitFor
    // inherited method to run the process in blocking mode
    constructor Create(Backup: TSqlite3Backup; Source, Dest: TSqlDatabase;
      StepPageNumber,StepSleepMS: integer; SynLzCompress: boolean;
      const OnProgress: TOnSqlDatabaseBackup; OwnerDest: boolean = true); reintroduce;
    /// the source database of the backup process
    property SourceDB: TSqlDatabase
      read fSourceDB;
    /// the destination database of the backup process
    property DestDB: TSqlDatabase
      read fDestDB;
    /// the raised exception in case of backupFailure notification
    property FailureError: Exception
      read fError;
    /// the backup target database file name
    property BackupDestFile: TFileName
      read fBackupDestFile;
  published
    /// the current state of the backup process
    // - only set before a call to TOnSqlDatabaseBackup
    property Step: TOnSqlDatabaseBackupStep
      read fStep;
    /// the number of pages which remain before end of backup
    // - only set before a call to TOnSqlDatabaseBackup with backupStep* event
    property StepNumberToFinish: integer
      read fStepNumberToFinish;
    /// the number of pages for the whole database
    // - only set before a call to TOnSqlDatabaseBackup with backupStep* event
    property StepNumberTotal: integer
      read fStepNumberTotal;
    /// if .dbsynlz compression would be done on the backup file
    // - would use FileSynLZ(), so compress in chunks of 128 MB
    property StepSynLzCompress: boolean
      read fStepSynLzCompress;
  end;

const
  /// identify the iterative step events during TSqlDatabase.BackupBackground()
  // - you can use (Sender.Step in backupAnyStep), to check for normal step
  backupAnyStep =
      [backupStepOk, backupStepBusy, backupStepLocked, backupStepSynLz];
  /// identify the end step events during TSqlDatabase.BackupBackground()
  // - you can use (Sender.Step in backupFinished) to check for process end
  backupFinished =
    [backupSuccess, backupFailure];


var
  /// the TSynLog class used for logging our mormot.db.raw.sqlite3 functions
  // - since not all exceptions are handled specificaly by this unit, you
  // may better use a common TSynLog class for the whole application or module
  SQLite3Log: TSynLogClass = TSynLog;


/// check from the file beginning if sounds like a valid SQLite3 file
// - returns true if a database file is encrypted or not
// - optional retrieve the file page size from header
function IsSQLite3File(const FileName: TFileName;
  PageSize: PInteger = nil): boolean;

/// check if sounds like an encrypted SQLite3 file
function IsSQLite3FileEncrypted(const FileName: TFileName): boolean;

/// comparison function using TSqlStatementCache.Timer.TimeInMicroSec
function StatementCacheTotalTimeCompare(const A, B): integer;


const
  /// a magic text constant which will prevent any JSON result to be cached
  // in TSqlDataBase, if present in the SQL statement
  // - to be used e.g. when you put some pointers as bound parameters
  SQLDATABASE_NOCACHE: RawUtf8 = '/*nocache*/';

  /// the "magic" number used to identify .dbsynlz compressed files, as
  // created by TSqlDataBase.BackupSynLZ() or if SynLZCompress parameter is TRUE
  // for the TSqlDataBase.BackupBackground() method
  // - note that the SynDBExplorer tool is able to recognize such files, and
  // open them directly - or use the DBSynLZ.dpr command-line sample tool
  SQLITE3_MAGIC = $ABA5A5AB;

  /// the "magic" 16 bytes header stored at the begining of every SQlite3 file
  SQLITE_FILE_HEADER: array[0..15] of AnsiChar = 'SQLite format 3';

var
  SQLITE_FILE_HEADER128: THash128Rec absolute SQLITE_FILE_HEADER;


implementation


{ ************ Raw SQLite3 API Constants and Functions }

function SqlVarToSQlite3Context(const Res: TSqlVar;
  Context: TSqlite3FunctionContext): boolean;
var
  tmp: array[0..31] of AnsiChar;
begin
  case Res.VType of
    ftNull:
      sqlite3.result_null(Context);
    ftInt64:
      sqlite3.result_int64(Context, Res.VInt64);
    ftDouble:
      sqlite3.result_double(Context, Res.VDouble);
    ftCurrency:
      sqlite3.result_double(Context, Res.VCurrency);
    ftDate:
      begin
        DateTimeToIso8601ExpandedPChar(Res.VDateTime, tmp{%H-}, 'T',
          svoDateWithMS in Res.Options);
        sqlite3.result_text(Context, tmp, -1, SQLITE_TRANSIENT_VIRTUALTABLE);
      end;
    // WARNING! use pointer(integer(-1)) instead of SQLITE_TRANSIENT=pointer(-1)
    // due to a bug in SQLite3 current implementation of virtual tables in Win64
    ftUtf8:
      if Res.VText = nil then
        sqlite3.result_text(Context, @NULCHAR, 0, SQLITE_STATIC)
      else
        sqlite3.result_text(Context, Res.VText, -1, SQLITE_TRANSIENT_VIRTUALTABLE);
    ftBlob:
      sqlite3.result_blob(Context, Res.VBlob, Res.VBlobLen,
        SQLITE_TRANSIENT_VIRTUALTABLE);
  else
    begin
      sqlite3.result_null(Context);
      SQLite3Log.DebuggerNotify(sllWarning, 'SqlVarToSQlite3Context(%)',
        [ord(Res.VType)]);
      result := false; // not handled type (will set null value)
      exit;
    end;
  end;
  result := true;
end;

procedure SQlite3ValueToSqlVar(Value: TSqlite3Value; var Res: TSqlVar);
var
  ValueType: integer;
begin
  Res.Options := [];
  ValueType := sqlite3.value_type(Value);
  case ValueType of
    SQLITE_NULL:
      Res.VType := ftNull;
    SQLITE_INTEGER:
      begin
        Res.VType := ftInt64;
        Res.VInt64 := sqlite3.value_int64(Value);
      end;
    SQLITE_FLOAT:
      begin
        Res.VType := ftDouble;
        Res.VDouble := sqlite3.value_double(Value);
      end;
    SQLITE_TEXT:
      begin
        Res.VType := ftUtf8;
        Res.VText := sqlite3.value_text(Value);
      end;
    SQLITE_BLOB:
      begin
        Res.VType := ftBlob;
        Res.VBlobLen := sqlite3.value_bytes(Value);
        Res.VBlob := sqlite3.value_blob(Value);
      end;
  else
    begin
      SQLite3Log.DebuggerNotify(
        sllWarning, 'SQlite3ValueToSqlVar(%)', [ValueType]);
      Res.VType := ftUnknown;
    end;
  end;
end;

procedure sqlite3InternalFree(p: pointer); cdecl;
begin
  Freemem(p);
end;

procedure sqlite3InternalFreeObject(p: pointer); cdecl;
begin
  TObject(p).Free;
end;

procedure sqlite3InternalFreeRawByteString(p: pointer); cdecl;
begin
  RawByteString(p) := '';
end;

procedure ErrorWrongNumberOfArgs(Context: TSqlite3FunctionContext);
begin
  sqlite3.result_error(Context, 'wrong number of arguments');
end;

function CheckNumberOfArgs(Context: TSqlite3FunctionContext;
  expected, sent: integer): boolean;
var
  msg: ShortString;
begin
  if sent <> expected then
  begin
    FormatShort('wrong number of arguments: expected %, got %',
      [expected, sent], msg);
    sqlite3.result_error(Context, @msg[1], ord(msg[0]));
    result := false;
  end
  else
    result := true;
end;

procedure ExceptionToSqlite3Err(E: Exception; var pzErr: PUtf8Char);
var
  U: RawUtf8;
begin
  U := StringToUtf8(E.Message);
  pzErr := sqlite3.malloc(length(U));
  MoveFast(pointer(U)^, pzErr^, length(U));
end;

procedure RawUtf8ToSQlite3Context(const Text: RawUtf8;
  Context: TSqlite3FunctionContext; VoidTextAsNull: boolean; ForcedLen: integer);
var
  tmp: pointer;
begin
  if Text = '' then
    if VoidTextAsNull then
      sqlite3.result_null(Context)
    else
      sqlite3.result_text(Context, @NULCHAR, 0, SQLITE_STATIC)
  else
  begin
    tmp := nil;
    RawUtf8(tmp) := Text; // fast COW assignment
    if ForcedLen < 0 then
      // exact RawUtf8 content
      ForcedLen := length(Text)
    else
      // ensure ASCIIZ even when truncated
      PUtf8Char(tmp)[ForcedLen] := #0;
    // set Value_bytes=..+1 to include the ending #0
    sqlite3.result_text(
      Context, tmp, ForcedLen + 1, sqlite3InternalFreeRawByteString);
  end;
end;

procedure VariantToSQlite3Context(const Value: Variant;
  Context: TSqlite3FunctionContext);
var
  res: TSqlVar;
  tmp: RawByteString;
begin
  VariantToSqlVar(Value, tmp, res);
  SqlVarToSQlite3Context(res, Context);
end;

procedure JsonToSQlite3Context(json: PUtf8Char;
  Context: TSqlite3FunctionContext);
var
  tmp: Variant;
  start: PUtf8Char;
begin
  if json = nil then
    sqlite3.result_null(Context)
  else
  begin
    start := GotoNextNotSpace(json);
    if start^ in ['[', '{'] then
    begin
      // JSON object or array is returned as plain TEXT
      json := GotoEndJsonItem(start);
      if json = nil then
        sqlite3.result_null(Context)
      else
      begin
        json^ := #0; // truncate to the matching object or array end
        sqlite3.result_text(Context, start, json - start + 1, SQLITE_TRANSIENT);
      end;
    end
    else
      // JSON simple types (text, numbers) would be converted via a variant
      if VariantLoadJson(tmp, start, nil, nil) = nil then
        sqlite3.result_null(Context)
      else
        VariantToSQlite3Context(tmp, Context);
  end;
end;


{ ESqlite3Exception }

constructor ESqlite3Exception.Create(aDB: TSqlite3DB; aErrorCode: integer;
  const aSql: RawUtf8);
var
  msg: RawUtf8;
begin
  fErrorCode := aErrorCode;
  fSQLite3ErrorCode := sqlite3_resultToErrorCode(aErrorCode);
  FormatUtf8('Error % (%) [%] using %', [ErrorCodeToText(SQLite3ErrorCode),
    aErrorCode, aSql, sqlite3.VersionText], msg);
  if aDB = 0 then
    msg := msg + ' with aDB=nil'
  else
  begin
    msg := FormatUtf8('% - %', [msg, sqlite3.errmsg(aDB)]);
    if Assigned(sqlite3.extended_errcode) then
      msg := FormatUtf8('%, extended_errcode=%',
        [msg, sqlite3.extended_errcode(aDB)]);
  end;
  DB := aDB;
  inherited Create(Utf8ToString(msg));
end;

function sqlite3_check(DB: TSqlite3DB; aResult: integer;
  const SQL: RawUtf8): integer;
begin
  if (DB = 0) or
     (aResult in SQLITE_ERRORS) then // possible error codes
    raise ESqlite3Exception.Create(DB, aResult, SQL);
  result := aResult;
end;

function sqlite3_resultToErrorCode(aResult: integer): TSqlite3ErrorCode;
begin
  case aResult of
    SQLITE_OK..SQLITE_NOTADB:
      result := TSqlite3ErrorCode(aResult + ord(secOK));
    SQLITE_ROW..SQLITE_DONE:
      result := TSqlite3ErrorCode(aResult + ord(secROW));
  else
    result := secUnknown;
  end;
end;

function ErrorCodeToText(err: TSqlite3ErrorCode): RawUtf8;
begin
  result := 'SQLITE_' +
    TrimLeftLowerCaseShort(GetEnumName(TypeInfo(TSqlite3ErrorCode), ord(err)));
end;

function sqlite3_resultToErrorText(aResult: integer): RawUtf8;
begin
  result := ErrorCodeToText(sqlite3_resultToErrorCode(aResult));
end;



{ TSqlite3Library }

constructor TSqlite3Library.Create;
var
  V: PUtf8Char;
begin
  if Assigned(libversion) then
  begin
    V := libversion;
    fVersionText := RawUtf8(V);
    // convert into e.g. 3008003001
    fVersionNumber := GetNextItemCardinal(V, '.') * 1000000000 +
                      GetNextItemCardinal(V, '.') * 1000000 +
                      GetNextItemCardinal(V, '.') * 1000 +
                      GetNextItemCardinal(V, '.');
  end;
  AfterInitialization;
end;

procedure TSqlite3Library.BeforeInitialization;
begin
end;

procedure TSqlite3Library.AfterInitialization;
begin
end;

// under FPC, MemSize() returns the value expected by xSize()
// under Delphi, we need to store the size as 4 bytes header for xSize()

{$ifdef FPC}

function xMalloc(size: integer): pointer; cdecl;
begin
  result := GetMem(size);
end;

procedure xFree(ptr: pointer); cdecl;
begin
  FreeMem(ptr);
end;

function xRealloc(ptr: pointer; size: integer): pointer; cdecl;
begin
  result := ReAllocMem(ptr, size);
end;

function xSize(ptr: pointer): integer; cdecl;
begin
  result := MemSize(ptr);
end;

{$else}

function xMalloc(size: integer): pointer; cdecl;
begin
  GetMem(result, size + 4);
  PInteger(result)^ := size;
  inc(PInteger(result));
end;

procedure xFree(ptr: pointer); cdecl;
begin
  dec(PInteger(ptr));
  FreeMem(ptr);
end;

function xRealloc(ptr: pointer; size: integer): pointer; cdecl;
begin
  dec(PInteger(ptr));
  ReallocMem(ptr, size + 4);
  PInteger(ptr)^ := size;
  inc(PInteger(ptr));
  result := ptr;
end;

function xSize(ptr: pointer): integer; cdecl;
begin
  if ptr = nil then
    result := 0
  else
  begin
    dec(PInteger(ptr));
    result := PInteger(ptr)^;
  end;
end;

{$endif FPC}

function xRoundup(size: integer): integer; cdecl;
begin
  result := size;
end;

function xInit(appData: pointer): integer; cdecl;
begin
  result := SQLITE_OK;
end;

procedure xShutdown(appData: pointer); cdecl;
begin
end;

procedure TSqlite3Library.ForceToUseSharedMemoryManager;
// due to FPC's linker limitation, all wrapper functions should be defined outside
var
  mem: TSqlite3MemMethods;
  res: integer;
  {$ifdef FPC_X64}
  mm: TMemoryManager;
  {$endif FPC_X64}
begin
  if not Assigned(config) then
    exit;
  {$ifdef FPC_X64} // SQLite3 prototypes match FPC RTL functions on x86_64 ABI
  GetMemoryManager(mm);
  mem.xMalloc := @mm.Getmem;
  mem.xFree := @mm.Freemem;
  mem.xSize := @mm.MemSize;
  {$else}
  mem.xMalloc := @xMalloc;
  mem.xFree := @xFree;
  mem.xSize := @xSize;
  {$endif FPC_X64}
  mem.xRealloc := @xRealloc;
  mem.xRoundup := @xRoundup;
  mem.xInit := @xInit;
  mem.xShutdown := @xShutdown;
  mem.pAppData := nil;
  try
    res := config(SQLITE_CONFIG_MALLOC, @mem);
  except
    res := SQLITE_INTERNAL;
  end;
  if res <> SQLITE_OK then
    SQLite3Log.Add.Log(sllError, 'SQLITE_CONFIG_MALLOC failed as %', [res])
  else
    fUseInternalMM := true;
end;

function TSqlite3Library.GetVersion: RawUtf8;
const
  mm: array[boolean] of string[2] = ('ex', 'in');
begin
  if self = nil then
    result := 'No TSqlite3Library available'
  else
    FormatUtf8('% % with %ternal MM', [self, fVersionText, mm[fUseInternalMM]], result);
end;


{ TSqlite3LibraryDynamic }

const
  // warning: those entry should follow EXACTLY the order in TSqlite3Library
  // methods, from @initialize() to the last one
  SQLITE3_ENTRIES: array[0..171] of PAnsiChar = (
    'sqlite3_initialize',
    'sqlite3_shutdown',
    'sqlite3_open',
    'sqlite3_open_v2',
    'sqlite3_key',
    'sqlite3_rekey',
    'sqlite3_close',
    'sqlite3_libversion',
    'sqlite3_libversion_number',
    'sqlite3_sourceid',
    'sqlite3_threadsafe',
    'sqlite3_errcode',
    'sqlite3_extended_errcode',
    'sqlite3_errmsg',
    'sqlite3_errstr',
    'sqlite3_system_errno',
    'sqlite3_extended_result_codes',
    'sqlite3_complete',
    'sqlite3_keyword_count',
    'sqlite3_keyword_name',
    'sqlite3_keyword_check',
    'sqlite3_txn_state',
    'sqlite3_exec',
    'sqlite3_interrupt',
    'sqlite3_last_insert_rowid',
    'sqlite3_set_last_insert_rowid',
    'sqlite3_busy_timeout',
    'sqlite3_busy_handler',
    'sqlite3_progress_handler',
    'sqlite3_get_autocommit',
    'sqlite3_set_authorizer',
    'sqlite3_preupdate_hook',
    'sqlite3_preupdate_old',
    'sqlite3_preupdate_new',
    'sqlite3_preupdate_count',
    'sqlite3_preupdate_depth',
    'sqlite3_unlock_notify',
    'sqlite3_update_hook',
    'sqlite3_commit_hook',
    'sqlite3_rollback_hook',
    'sqlite3_changes',
    'sqlite3_total_changes',
    'sqlite3_prepare_v2',
    'sqlite3_prepare_v3',
    'sqlite3_finalize',
    'sqlite3_next_stmt',
    'sqlite3_reset',
    'sqlite3_stmt_busy',
    'sqlite3_stmt_isexplain',
    'sqlite3_stmt_readonly',
    'sqlite3_stmt_scanstatus',
    'sqlite3_stmt_scanstatus_reset',
    'sqlite3_stmt_status',
    'sqlite3_db_handle',
    'sqlite3_sql',
    'sqlite3_expanded_sql',
    'sqlite3_normalized_sql',
    'sqlite3_step',
    'sqlite3_table_column_metadata',
    'sqlite3_column_count',
    'sqlite3_column_type',
    'sqlite3_column_decltype',
    'sqlite3_column_name',
    'sqlite3_column_database_name',
    'sqlite3_column_table_name',
    'sqlite3_column_origin_name',
    'sqlite3_column_bytes',
    'sqlite3_column_value',
    'sqlite3_column_double',
    'sqlite3_column_int',
    'sqlite3_column_int64',
    'sqlite3_column_text',
    'sqlite3_column_text16',
    'sqlite3_column_blob',
    'sqlite3_value_type',
    'sqlite3_value_subtype',
    'sqlite3_value_numeric_type',
    'sqlite3_value_nochange',
    'sqlite3_value_frombind',
    'sqlite3_value_bytes',
    'sqlite3_value_dup',
    'sqlite3_value_free',
    'sqlite3_value_pointer',
    'sqlite3_value_double',
    'sqlite3_value_int64',
    'sqlite3_value_text',
    'sqlite3_value_blob',
    'sqlite3_create_function',
    'sqlite3_create_function_v2',
    'sqlite3_create_window_function',
    'sqlite3_set_auxdata',
    'sqlite3_get_auxdata',
    'sqlite3_result_pointer',
    'sqlite3_result_null',
    'sqlite3_result_int64',
    'sqlite3_result_double',
    'sqlite3_result_blob',
    'sqlite3_result_zeroblob',
    'sqlite3_result_text',
    'sqlite3_result_value',
    'sqlite3_result_subtype',
    'sqlite3_result_error',
    'sqlite3_user_data',
    'sqlite3_context_db_handle',
    'sqlite3_aggregate_context',
    'sqlite3_bind_text',
    'sqlite3_bind_blob',
    'sqlite3_bind_zeroblob',
    'sqlite3_bind_double',
    'sqlite3_bind_int',
    'sqlite3_bind_int64',
    'sqlite3_bind_null',
    'sqlite3_bind_pointer',
    'sqlite3_bind_value',
    'sqlite3_clear_bindings',
    'sqlite3_bind_parameter_count',
    'sqlite3_bind_parameter_index',
    'sqlite3_bind_parameter_name',
    'sqlite3_blob_open',
    'sqlite3_blob_reopen',
    'sqlite3_blob_close',
    'sqlite3_blob_read',
    'sqlite3_blob_write',
    'sqlite3_blob_bytes',
    'sqlite3_create_collation',
    'sqlite3_create_collation_v2',
    'sqlite3_collation_needed',
    'sqlite3_create_module_v2',
    'sqlite3_drop_modules',
    'sqlite3_declare_vtab',
    'sqlite3_vtab_collation',
    'sqlite3_vtab_config',
    'sqlite3_vtab_nochange',
    'sqlite3_vtab_on_conflict',
    'sqlite3_overload_function',
    'sqlite3_auto_extension',
    'sqlite3_cancel_auto_extension',
    'sqlite3_reset_auto_extension',
    'sqlite3_load_extension',
    'sqlite3_malloc',
    'sqlite3_realloc',
    'sqlite3_free',
    'sqlite3_msize',
    'sqlite3_release_memory',
    'sqlite3_db_release_memory',
    'sqlite3_memory_used',
    'sqlite3_memory_highwater',
    'sqlite3_soft_heap_limit64',
    'sqlite3_config',
    'sqlite3_db_config',
    'sqlite3_status64',
    'sqlite3_db_status',
    'sqlite3_db_cacheflush',
    'sqlite3_db_filename',
    'sqlite3_db_readonly',
    'sqlite3_trace_v2',
    'sqlite3_limit',
    'sqlite3_backup_init',
    'sqlite3_backup_step',
    'sqlite3_backup_finish',
    'sqlite3_backup_remaining',
    'sqlite3_backup_pagecount',
    'sqlite3_serialize',
    'sqlite3_deserialize',
    'sqlite3_wal_hook',
    'sqlite3_wal_autocheckpoint',
    'sqlite3_wal_checkpoint_v2',
    'sqlite3_snapshot_get',
    'sqlite3_snapshot_open',
    'sqlite3_snapshot_recover',
    'sqlite3_snapshot_cmp',
    'sqlite3_snapshot_free');


function TSqlite3LibraryDynamic.GetLibraryName: TFileName;
begin
  if (self = nil) or
     (fLoader = nil) then
    result := ''
  else
    result := fLoader.LibraryPath;
end;

constructor TSqlite3LibraryDynamic.Create(const LibraryName: TFileName);
var
  P: PPointerArray;
  i: PtrInt;
  l1: TFileName;
  vers: PUtf8Char;
begin
  fLoader := TSynLibrary.Create;
  if LibraryName = SQLITE_LIBRARY_DEFAULT_NAME then
    // first search for the standard library in the executable folder
    l1 := Executable.ProgramFilePath + LibraryName;
  fLoader.TryLoadLibrary([{%H-}l1, LibraryName], ESqlite3Exception);
  P := @@initialize;
  for i := 0 to High(SQLITE3_ENTRIES) do
    fLoader.Resolve(SQLITE3_ENTRIES[i], @P^[i]); // no exception, but set nil
  if (Assigned(limit) and
      (LibraryResolve(fLoader.Handle, 'sqlite3_limit') <> @limit)) or
     (Assigned(snapshot_free) and
      (LibraryResolve(fLoader.Handle, SQLITE3_ENTRIES[171]) <> @snapshot_free)) then
    raise ESqlite3Exception.CreateUtf8( // paranoid check
      '%.Create: please check SQLITE3_ENTRIES[] order for %', [self, LibraryName]);
  if not Assigned(initialize) or
     not Assigned(libversion) or
     not Assigned(open) or
     not Assigned(close) or
     not Assigned(create_function) or
     not Assigned(prepare_v2) or
     not Assigned(create_module_v2) then
     // note: some APIs like config() key() or trace() may not be available
     // -> use the "if Assigned(xxx) then xxx(...)" pattern for safety
  begin
    if Assigned(libversion) then
      vers := libversion
    else
      vers := 'unknown';
    FreeAndNil(fLoader);
    raise ESqlite3Exception.CreateUtf8(
      '%.Create: TOO OLD % % - need 3.7 at least', [self, LibraryName, vers]);
  end;
  BeforeInitialization;
  inherited Create; // set fVersionNumber/fVersionText
  SQLite3Log.Add.Log(sllInfo,
    'Loaded external % version %', [LibraryName, Version]);
end;

destructor TSqlite3LibraryDynamic.Destroy;
begin
  FreeAndNil(fLoader);
  inherited;
end;


{ ************ High-Level Classes for SQlite3 Queries }

{ Some remarks about our custom SQLite3 functions:

  1. From WladiD: if a field is empty '' (not NULL), SQLite calls the registered
     collate function with s1len=0 or s2len=0, but the pointers s1 or s2 map to
     the string of the previous call - so s1len/s2len should be first checked.

  2. Some collations (WIN32CASE/WIN32NOCASE) may not be consistent depenging
     on the system/libray they run on: if you expect to move the SQLite3 file,
     consider SYSTEMNOCASE or UNICODENOCASE safer (and faster) functions.
  }

function Utf16_WIN32CASE(CollateParam: pointer; s1Len: integer; S1: pointer;
  s2Len: integer; S2: pointer): integer; cdecl;
begin
  if s1Len <= 0 then
    if s2Len <= 0 then
      result := 0
    else
      result := -1
  else if s2Len <= 0 then
    result := 1
  else
    // Windows / ICU comparison - warning: may vary on systems
    result := Unicode_CompareString(S1, S2, s1Len shr 1, s2Len shr 1,
      {igncase=} false) - 2;
end;

function Utf16_WIN32NOCASE(CollateParam: pointer; s1Len: integer; s1: pointer;
  s2Len: integer; s2: pointer): integer; cdecl;
begin
  if s1Len <= 0 then
    if s2Len <= 0 then
      result := 0
    else
      result := -1
  else if s2Len <= 0 then
    result := 1
  else
    // Windows / ICU case folding - warning: may vary on systems
    result := Unicode_CompareString(s1, s2, s1Len shr 1, s2Len shr 1,
      {igncase=} true) - 2;
end;

function Utf8_SYSTEMNOCASE(CollateParam: pointer; s1Len: integer; s1: pointer;
  s2Len: integer; s2: pointer): integer; cdecl;
begin
  if s1Len <= 0 then
    if s2Len <= 0 then
      result := 0
    else
      result := -1
  else if s2Len <= 0 then
    result := 1
  else
    // WinAnsi CP-1252 case folding
    result := Utf8ILComp(s1, s2, s1Len, s2Len);
end;

function Utf8_UNICODENOCASE(CollateParam: pointer; s1Len: integer; s1: pointer;
  s2Len: integer; s2: pointer): integer; cdecl;
begin
  if s1Len <= 0 then
    if s2Len <= 0 then
      result := 0
    else
      result := -1
  else if s2Len <= 0 then
    result := 1
  else
    // case folding using our Unicode 10.0 tables - will remain stable
    result := Utf8ILCompReference(s1, s2, s1Len, s2Len);
end;

function Utf8_ISO8601(CollateParam: pointer; s1Len: integer; s1: pointer;
  s2Len: integer; s2: pointer): integer; cdecl;
var
  V1, V2: TDateTime; // will handle up to .sss milliseconds resolution
begin
  if s1Len <= 0 then
    s1 := nil;
  if s2Len <= 0 then
    s2 := nil;
  if s1 = s2 then
    result := 0
  else
  begin
    Iso8601ToDateTimePUtf8CharVar(s1, s1Len, V1);
    Iso8601ToDateTimePUtf8CharVar(s2, s2Len, V2);
    if (V1 = 0) or
       (V2 = 0) then
      // any invalid date -> compare as UTF-8 strings
      result := Utf8ILComp(s1, s2, s1Len, s2Len)
    else if SameValue(V1, V2, 1 / MSecsPerDay) then
      result := 0
    else if V1 < V2 then
      result := -1
    else
      result := +1;
  end;
end;

procedure InternalSoundex(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
begin
  if CheckNumberOfArgs(Context, 1, argc) then
    sqlite3.result_int64(Context, SoundExUtf8(sqlite3.value_text(argv[0])));
end;

procedure InternalSoundexFr(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
begin
  if CheckNumberOfArgs(Context, 1, argc) then
    sqlite3.result_int64(Context, SoundExUtf8(sqlite3.value_text(argv[0]), nil,
      sndxFrench));
end;

procedure InternalSoundexEs(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
begin
  if CheckNumberOfArgs(Context, 1, argc) then
    sqlite3.result_int64(Context, SoundExUtf8(sqlite3.value_text(argv[0]), nil,
      sndxSpanish));
end;

procedure InternalMod(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
var
  A1, A2: Int64;
begin
  // implements the MOD() function, just like Oracle and others
  if argc <> 2 then
  begin
    ErrorWrongNumberOfArgs(Context);
    exit; // two parameters expected
  end;
  A1 := sqlite3.value_int64(argv[0]);
  A2 := sqlite3.value_int64(argv[1]);
  if A2 = 0 then // avoid computation exception, returns NULL
    sqlite3.result_null(Context)
  else
    sqlite3.result_int64(Context, A1 mod A2);
end;

procedure InternalTimeLog(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
var
  TimeLog: TTimeLogBits;
begin
  if argc <> 1 then
  begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  TimeLog.Value := sqlite3.value_int64(argv[0]);
  RawUtf8ToSQlite3Context(TimeLog.Text(True, 'T'), Context, false);
end;

procedure InternalTimeLogUnix(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
var
  TimeLog: TTimeLogBits;
begin
  if argc <> 1 then
  begin
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  TimeLog.Value := sqlite3.value_int64(argv[0]);
  sqlite3.result_int64(Context, TimeLog.ToUnixTime);
end;

procedure InternalRank(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
// supplies the same "RANK" internal function as proposed in
// http://www.sqlite.org/fts3.html#appendix_a
var
  MI: PFTSMatchInfo;
  p, c: integer;
  score: Double;
begin
  if argc >= 1 then
  begin
    MI := sqlite3.value_blob(argv[0]);
    // rank(nil) for example select rank(matchinfo(tabName)) without corresponding MATCH clause
    if MI = nil then
    begin
      sqlite3.result_double(Context, 0);
      exit;
    end;
    if argc = MI^.nCol + 1 then
    begin
      score := 0;
      for p := 1 to MI^.nPhrase do
        for c := 1 to MI^.nCol do
          with MI^.hits[c] do
            if this_row > 0 then
              score := score + (this_row / all_rows) * sqlite3.value_double(argv[c]);
      sqlite3.result_double(Context, score);
      exit; // success: don't call sqlite3.result_error()
    end;
  end;
  ErrorWrongNumberOfArgs(Context);
end;

// supplies a CONCAT() function to process fast string concatenation
type
  TConcatRec = record
    result: PUtf8Char;
    resultlen: PtrInt;
  end;
  PConcatRec = ^TConcatRec;

procedure InternalConcatStep(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
var
  sep, txt: PUtf8Char;
  seplen, txtlen: PtrInt;
begin
  if argc = 2 then
    with PConcatRec(sqlite3.aggregate_context(Context, sizeof(TConcatRec)))^ do
    begin
      // +1 below for adding a final #0
      txt := sqlite3.value_text(argv[0]);
      txtlen := StrLen(txt);
      if result = nil then
        GetMem(result, txtlen + 1)
      else
      begin
        sep := sqlite3.value_text(argv[1]);
        seplen := StrLen(sep);
        ReallocMem(result, resultlen + txtlen + seplen + 1);
        MoveFast(sep^, result[resultlen], seplen);
        inc(resultlen, seplen);
      end;
      MoveFast(txt^, result[resultlen], txtlen + 1);
      inc(resultlen, txtlen);
    end
  else
    ErrorWrongNumberOfArgs(Context);
end;

procedure InternalConcatFinal(Context: TSqlite3FunctionContext); cdecl;
begin
  with PConcatRec(sqlite3.aggregate_context(Context, sizeof(TConcatRec)))^ do
    // sqlite3InternalFree will call Freemem(PConcatRec()^.result)
    sqlite3.result_text(Context, result, resultlen + 1, sqlite3InternalFree);
end;

procedure InternalIntegerDynArray(Context: TSqlite3FunctionContext;
  argc: integer; var argv: TSqlite3ValueArray); cdecl;
var
  Blob: pointer;
  PI: PIntegerArray;
  Count: integer;
begin
  // SQL function: IntegerDynArrayContains(BlobField,10) returning a boolean
  if not CheckNumberOfArgs(Context, 2, argc) then
    exit;
  Blob := sqlite3.value_blob(argv[0]);
  if Blob <> nil then
  begin
    PI := IntegerDynArrayLoadFrom(Blob, Count); // fast map into in-memory array
    if not IntegerScanExists(pointer(PI), Count, sqlite3.value_int64(argv[1])) then
      Blob := nil;
  end;
  sqlite3.result_int64(Context, Int64(Blob <> nil));
end;

procedure InternalSimpleInt64DynArray(Context: TSqlite3FunctionContext;
  argc: integer; var argv: TSqlite3ValueArray); cdecl;
var
  Blob: pointer;
  Count, ElemSize: PtrInt;
  V: Int64;
begin
  // Byte/Word/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
  // for currency, expect I64 value = aCurrency*10000 = PInt64(@aCurrency)^
  if not CheckNumberOfArgs(Context, 2, argc) then
    exit;
  Blob := sqlite3.value_blob(argv[0]);
  if Blob <> nil then
  begin
    // search into direct in-memory mapping (no allocation)
    Blob := SimpleDynArrayLoadFrom(
      Blob, sqlite3.user_data(Context), Count, ElemSize);
    if Blob <> nil then
    begin
      V := sqlite3.value_int64(argv[1]);
      sqlite3.result_int64(Context, Int64(true)); // exit will report value found
      if AnyScanExists(Blob, @V, Count, ElemSize) then
        exit;
    end;
  end;
  sqlite3.result_int64(Context, Int64(false)); // not found
end;

procedure InternalRawUtf8DynArray(Context: TSqlite3FunctionContext;
  argc: integer; var argv: TSqlite3ValueArray); cdecl;
var
  Blob: pointer;
  Value: PUtf8Char;
begin
  // SQL function: RawUtf8DynArrayContainsCase/NoCase(BlobField,'Text'): boolean
  if not CheckNumberOfArgs(Context, 2, argc) then
    exit;
  Blob := sqlite3.value_blob(argv[0]);
  if Blob <> nil then
  begin
    Value := sqlite3.value_text(argv[1]);
    if RawUtf8DynArrayLoadFromContains(Blob, Value, StrLen(Value), sqlite3.user_data
      (Context) = nil) < 0 then
      Blob := nil;
  end;
  sqlite3.result_int64(Context, Int64(Blob <> nil));
end;

procedure InternalJsonGet(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;

  function returnObject(w: PUtf8Char): boolean;
  begin
    if w <> nil then
      repeat
        case w^ of
          #0:
            break;
          ',', '*':
            begin
              result := true;
              exit;
            end;
        end;
        inc(w);
      until false;
    result := false;
  end;

var
  where, json: PUtf8Char;
begin
  // JsonGet(VariantField,'PropName') returns the value of a JSON object
  // JsonGet(VariantField,'Obj1.Obj2.PropName') to search by path
  // JsonGet(VariantField,0) returns the 1st item in the JSON array
  // JsonGet(VariantField,'Prop1,Prop2') returns the values as a JSON object
  // JsonGet(VariantField,'Prop*') returns the values as a JSON object
  // JsonGet(VariantField,'Obj1.Obj2.Prop1,Obj1.Prop2') to search by path
  // JsonGet(VariantField,'Obj1.Obj2.Prop*,Obj1.Prop2') to search by path
  if not CheckNumberOfArgs(Context, 2, argc) then
    exit;
  if sqlite3.value_type(argv[0]) <> SQLITE_TEXT then
    sqlite3.result_null(Context)
  else
    case sqlite3.value_type(argv[1]) of
      // fast SAX search (no memory allocation)
      SQLITE_TEXT:
        begin
          json := sqlite3.value_text(argv[0]);
          where := sqlite3.value_text(argv[1]);
          if returnObject(where) then
            RawUtf8ToSQlite3Context(JsonObjectsByPath(json, where), Context, true)
          else
            JsonToSQlite3Context(JsonObjectByPath(json, where), Context);
        end;
      SQLITE_INTEGER:
        begin
          json := JsonArrayItem(
            sqlite3.value_text(argv[0]), sqlite3.value_int64(argv[1]));
          JsonToSQlite3Context(json, Context);
        end;
    else
      sqlite3.result_null(Context);
    end;
end;

procedure InternalJsonHas(Context: TSqlite3FunctionContext;
  argc: integer; var argv: TSqlite3ValueArray); cdecl;
begin
  // JsonHas(VariantField,'PropName') returns TRUE if matches a JSON object property
  // JsonHas(VariantField,'Obj1.Obj2.PropName') to search by path
  // JsonHas(VariantField,0) returns TRUE if the JSON array has at least one item
  if not CheckNumberOfArgs(Context, 2, argc) then
    exit;
  if sqlite3.value_type(argv[0]) <> SQLITE_TEXT then
    sqlite3.result_int64(Context, Int64(false))
  else
    case sqlite3.value_type(argv[1]) of // fast SAX search (no memory allocation)
      SQLITE_TEXT:
        sqlite3.result_int64(Context,ord(JsonObjectByPath(
          sqlite3.value_text(argv[0]), sqlite3.value_text(argv[1])) <> nil));
      SQLITE_INTEGER:
        sqlite3.result_int64(Context,ord(JsonArrayItem(
          sqlite3.value_text(argv[0]), sqlite3.value_int64(argv[1])) <> nil));
    else
      sqlite3.result_int64(Context, Int64(false));
    end;
end;

procedure InternalJsonSet(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
var
  doc: TDocVariantData;
  json: PUtf8Char;
  tmp: RawUtf8;
  v: PVariant;
begin
  // JsonSet(VariantField,'PropName','abc') to set a value
  // JsonSet(VariantField,'Obj1.Obj2.PropName','def') to set by path
  if not CheckNumberOfArgs(Context, 3, argc) then
    exit;
  if sqlite3.value_type(argv[0]) <> SQLITE_TEXT then
    sqlite3.result_null(Context)
  else
  begin
    json := sqlite3.value_text(argv[0]);
    FastSetString(tmp, json, StrLen(json));
    doc.InitJsonInPlace(pointer(tmp), JSON_FAST);
    v := doc.GetPVariantByPath(sqlite3.value_text(argv[1]));
    if v <> nil then
    begin
      json := sqlite3.value_text(argv[2]);
      FastSetString(tmp, json, StrLen(json));
      VariantLoadJson(v^, pointer(tmp), nil, @JSON_[mFast]);
      RawUtf8ToSQlite3Context(doc.ToJson, Context, false);
    end
    else
    begin
      FastSetString(tmp, json, StrLen(json));
      RawUtf8ToSQlite3Context(tmp, Context, false);
    end;
  end;
end;

procedure InternalUnicodeUpper(Context: TSqlite3FunctionContext; argc: integer;
  var argv: TSqlite3ValueArray); cdecl;
var
  input: PUtf8Char;
  len: integer;
  tmp: RawUtf8;
begin
  if not CheckNumberOfArgs(Context, 1, argc) then
    exit;
  input := sqlite3.value_text(argv[0]);
  len := StrLen(input);
  if len <> 0 then
  begin
    FastSetString(tmp, nil, len * 2); // Unicode Upper may enhance input length
    len := Utf8UpperReference(input, pointer(tmp), len) - PUtf8Char(pointer(tmp));
  end;
  // don't call SetLength() but set forcedlen to truncate the value
  RawUtf8ToSQlite3Context(tmp, Context, false, {forced=}len);
end;


{ TSqlDataBase }

function TSqlDataBase.Blob(const DBName, TableName, ColumnName: RawUtf8;
  RowID: Int64; ReadWrite: boolean): TSqlBlobStream;
begin
  if self = nil then
  begin
    result := nil;
    exit; // avoid GPF in case of call from a static-only server
  end;
  Lock;
  try
    if RowID = 0 then
      RowID := LastInsertRowID; // warning: won't work on multi-thread process
    result := TSqlBlobStream.Create(DB, DBName, TableName, ColumnName, RowID, ReadWrite);
  finally
    UnLock;
  end;
end;

procedure TSqlDataBase.Rollback;
begin
  if (self = nil) or
     not fTransactionActive then
    exit;
  Execute('ROLLBACK TRANSACTION;');
  fTransactionActive := false;
end;

procedure TSqlDataBase.TransactionBegin(aBehavior: TSqlDataBaseTransactionBehaviour);
const
  TBTOKENS: array[TSqlDataBaseTransactionBehaviour] of RawUtf8 = (
    // see http://www.sqlite.org/lang_transaction.html
    '', 'IMMEDIATE ', 'EXCLUSIVE ');
begin
  if self = nil then
    exit; // avoid GPF in case of call from a static-only server
  if fTransactionActive then
  try
    Execute('ROLLBACK TRANSACTION;');
  finally
    fTransactionActive := false;
  end;
  Execute('BEGIN ' + TBTOKENS[aBehavior] + 'TRANSACTION;');
  fTransactionActive := true;
end;

procedure TSqlDataBase.Commit;
begin
  if (Self <> nil) and
     fTransactionActive then
  try
    Execute('COMMIT TRANSACTION;');
  finally
    fTransactionActive := false;
  end;
end;

constructor TSqlDataBase.Create(
  const aFileName: TFileName; const aPassword: RawUtf8;
  aOpenV2Flags, aDefaultCacheSize, aDefaultPageSize: integer);
var
  result: integer;
begin
  inherited Create; // initialize fSafe
  if sqlite3 = nil then
    raise ESqlite3Exception.CreateUtf8('%.Create: No SQLite3 libray available' +
      ' - you shall either add mormot.db.raw.sqlite3.static to your project uses clause, ' +
      'or run sqlite3 := TSqlite3LibraryDynamic.Create(..)', [self]);
  fLog := SQLite3Log; // leave fLog=nil if no Logging wanted
  fLogResultMaximumSize := 512;
  fStatementMaxMemory := 512 shl 20;
  if SysUtils.Trim(aFileName) = '' then
    raise ESqlite3Exception.CreateUtf8('%.Create('''')', [self]);
  if aOpenV2Flags = 0 then
    fOpenV2Flags := SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE
  else
    fOpenV2Flags := aOpenV2Flags;
  fFileDefaultPageSize := aDefaultPageSize;
  fFileDefaultCacheSize := aDefaultCacheSize;
  if (fOpenV2Flags <> (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE)) and
     not Assigned(sqlite3.open_v2) then
    raise ESqlite3Exception.CreateUtf8(
      'Your % version of SQLite3 does not support custom OpenV2Flags=%',
      [sqlite3.libversion, fOpenV2Flags]);
  fFileName := aFileName;
  if fFileName = SQLITE_MEMORY_DATABASE_NAME then
    fIsMemory := true
  else
    fFileNameWithoutPath := ExtractFileName(fFileName);
  fPassword := aPassword;
  fUseCacheSize := 16384 * 1024;
  fSqlFunctions := TSynObjectList.Create;
  result := DBOpen;
  if result <> SQLITE_OK then
    raise ESqlite3Exception.Create(fDB, result, 'DBOpen');
end;

destructor TSqlDataBase.Destroy;
var
  {%H-}log: ISynLog;
begin
  log := fLog.Enter('Destroy %', [fFileNameWithoutPath], self);
  if DB <> 0 then
  try
    Rollback; // any unfinished transaction is rollbacked
  finally
  (*
  { Applications should finalize all prepared statements and close all BLOB handles
    associated with the sqlite3 object prior to attempting to close the object }
  repeat
    S := sqlite3.next_stmt(DB,0); // 0: get first prepared statement for DB
    if S=0 then
      break;
    // if code was correctly protected with try/finally, as in
    // TSqlDataBase.Execute() and TSqlRequest.Execute(), we should never go here
    // -> BUT it seems that the FTS3 leaves some statements open at closing
    // assert(false,FileName); // debug purpose, but not FTS3 ready
  until not (sqlite3.finalize(S) in [SQLITE_OK,SQLITE_ABORT]);
  { BUT the problem is that if you use FTS3, the statements will be released
    twice (i.e. one time above and next time in sqlite3.close below),
    so some GPF will occur :(
  -> we don't release any statement in case of FTS3 usage, and rely on our
    framework, which protects all SQL statements with try..finally clauses }
  *)
    DBClose;
  end;
  FillZero(fPassword);
  fCache.Free;
  fSqlFunctions.Free;
  inherited Destroy;
end;

function TSqlDataBase.SQLShouldBeLogged(const aSql: RawUtf8): boolean;
begin
  result := false;
  if (self = nil) or
     (fLog = nil) or
     not (sllSQL in fLog.Family.Level) then
    exit;
  if not IdemPChar(pointer(aSql), 'PRAGMA ') or
     (PosEx('=', aSql) > 0) then
    result := true;
end;

procedure TSqlDataBase.ExecuteAll(const aSql: RawUtf8);
var
  R: TSqlRequest;
  log: ISynLog;
begin
  if self = nil then
    exit; // avoid GPF in case of call from a static-only server
  if SQLShouldBeLogged(aSql) then
  begin
    log := fLog.Enter(self, 'ExecuteAll');
    if log <> nil then
      log.Log(sllSQL, aSql, self, 4096);
  end;
  LockAndFlushCache; // don't trust aSql -> assume modify -> inc(InternalState^)
  try
    R.ExecuteAll(DB, aSql);
  finally
    UnLock;
  end;
end;

procedure TSqlDataBase.Execute(const aSql: RawUtf8);
var
  R: TSqlRequest;
  Timer: TPrecisionTimer;
begin
  if self = nil then
    exit; // avoid GPF in case of call from a static-only server
  Timer.Start;
  Lock(aSql); // run one statement -> we can trust IsSelect()
  try
    R.Execute(DB, aSql);
  finally
    UnLock;
    fLog.Add.Log(sllSQL, '% % %', [Timer.Stop, FileNameWithoutPath, aSql], self);
  end;
end;

function TSqlDataBase.Execute(const aSql: RawUtf8;
  var aValues: TInt64DynArray): integer;
var
  R: TSqlRequest;
  log: ISynLog;
begin
  if self = nil then
  begin
    result := 0;
    exit; // avoid GPF in case of call from a static-only server
  end;
  if SQLShouldBeLogged(aSql) then
  begin
    log := fLog.Enter(self, 'Execute');
    if log <> nil then
      log.Log(sllSQL, aSql, self, 2048);
  end;
  Lock(aSql);
  try
    result := R.Execute(DB, aSql, aValues);
  finally
    UnLock;
  end;
end;

procedure TSqlDataBase.Execute(const aSql: RawUtf8;
  out aValue: Int64; NoLog: boolean);
var
  R: TSqlRequest;
  Timer: TPrecisionTimer;
begin
  if self = nil then
    exit; // avoid GPF in case of call from a static-only server
  if not NoLog then
    Timer.Start;
  Lock(aSql);
  try
    R.Execute(DB, aSql, aValue);
  finally
    UnLock;
    if not NoLog then
      fLog.Add.Log(sllSQL, '% % returned % for %', [Timer.Stop,
        FileNameWithoutPath, aValue, aSql], self);
  end;
end;

procedure TSqlDataBase.Execute(const aSql: RawUtf8;
  out aValue: RawUtf8; NoLog: boolean);
var
  R: TSqlRequest;
  Timer: TPrecisionTimer;
begin
  if self = nil then
    exit; // avoid GPF in case of call from a static-only server
  if not NoLog then
    Timer.Start;
  Lock(aSql);
  try
    R.Execute(DB, aSql, aValue);
  finally
    UnLock;
    if not NoLog then
      fLog.Add.Log(sllSQL, '% % returned [%] for %',
        [Timer.Stop, FileNameWithoutPath, aValue, aSql], self);
  end;
end;

function TSqlDataBase.ExecuteNoException(const aSql: RawUtf8): boolean;
var
  R: TSqlRequest;
  Timer: TPrecisionTimer;
begin
  result := false;
  if (self = nil) or
     (DB = 0) then
    exit; // avoid GPF in case of call from a static-only server
  Timer.Start;
  Lock(aSql); // run one statement -> we can trust IsCacheable()
  try
    result := R.ExecuteNoException(DB, aSql);
  finally
    UnLock;
    fLog.Add.Log(sllSQL, '% % % = %', [Timer.Stop, FileNameWithoutPath, aSql,
      BOOL_STR[result]], self);
  end;
end;

function TSqlDataBase.ExecuteNoExceptionInt64(const aSql: RawUtf8): Int64;
begin
  if (self = nil) or
     (DB = 0) then
    result := 0
  else
  try
    Execute(aSql, result, true);
  except
    result := 0;
  end;
end;

function TSqlDataBase.ExecuteNoExceptionUtf8(const aSql: RawUtf8): RawUtf8;
begin
  if (self = nil) or
     (DB = 0) then
    result := ''
  else
  try
    Execute(aSql, result, true);
  except
    result := '';
  end;
end;

function TSqlDataBase.ExecuteJson(const aSql: RawUtf8; Expand: boolean;
  aResultCount: PPtrInt): RawUtf8;
var
  R: TSqlRequest;
  Count: PtrInt;
  Timer: TPrecisionTimer;
begin
  if self = nil then
  begin
    result := '';
    exit; // avoid GPF in case of call from a static-only server
  end;
  Timer.Start;
  result := LockJson(aSql, aResultCount); // lock and try getting the request from the cache
  if result = '' then
  // only Execute the DB request if not got from cache
  try
    result := R.ExecuteJson(DB, aSql, Expand, @Count, StatementMaxMemory);
    if aResultCount <> nil then
      aResultCount^ := Count;
  finally
    UnLockJson(result, Count);
    fLog.Add.Log(sllSQL, '% % returned % bytes %', [Timer.Stop,
      FileNameWithoutPath, length(result), aSql], self);
  end;
end;

function TSqlDataBase.ExplainQueryPlan(const aSql: RawUtf8): RawUtf8;
var
  R: TSqlRequest;
  cnt: integer;
begin
  Lock; // don't cache the result since usually called once
  try
    result := R.ExecuteJson(DB, 'explain query plan ' + aSql, true, @cnt, 4096,
      [twoForceJsonExtended, twoIgnoreDefaultInRecord]);
    if cnt = 0 then
      result := ''; // no query plan
  finally
    UnLock;
  end;
end;

function TSqlDataBase.Execute(const aSql: RawUtf8;
  var aValues: TRawUtf8DynArray): integer;
var
  R: TSqlRequest;
  Timer: TPrecisionTimer;
begin
  result := 0;
  if self = nil then
    exit; // avoid GPF in case of call from a static-only server
  Timer.Start;
  Lock(aSql);
  try
    result := R.Execute(DB, aSql, aValues);
  finally
    UnLock;
    fLog.Add.Log(sllSQL, '% % returned % rows %', [Timer.Stop,
      FileNameWithoutPath, result, aSql], self);
  end;
end;

function TSqlDataBase.LastInsertRowID: Int64;
begin
  if (self = nil) or
     (DB = 0) then
    result := 0
  else
  try
    Lock;
    result := sqlite3.last_insert_rowid(DB);
  finally
    UnLock;
  end;
end;

function TSqlDataBase.LastChangeCount: integer;
begin
  if (self = nil) or
     (DB = 0) then
    result := 0
  else
  try
    Lock;
    result := sqlite3.changes(DB);
  finally
    UnLock;
  end;
end;

function TSqlDataBase.TotalChangeCount: integer;
begin
  if (self = nil) or
     (DB = 0) or
     not Assigned(sqlite3.total_changes) then
    result := 0
  else
  try
    Lock;
    result := sqlite3.total_changes(DB);
  finally
    UnLock;
  end;
end;

procedure TSqlDataBase.GetTableNames(var Names: TRawUtf8DynArray);
begin
  // SQL statement taken from official SQLite3 FAQ
  SetLength(Names, Execute(SQL_GET_TABLE_NAMES, Names));
end;

function TSqlDataBase.HasTable(const Name: RawUtf8): boolean;
var
  names: TRawUtf8DynArray;
begin
  GetTableNames(names);
  result := FindPropName(names, Name) >= 0;
end;

procedure TSqlDataBase.GetFieldNames(var Names: TRawUtf8DynArray;
  const TableName: RawUtf8);
var
  R: TSqlRequest;
  n: integer;
begin
  if (self = nil) or
     (fDB = 0) then
    exit; // avoid GPF in case of call from a static-only server
  Lock;
  try
    try
      R.Prepare(fDB, 'PRAGMA table_info(' + TableName + ');'); // ESqlite3Exception
      n := 0;
      while R.Step = SQLITE_ROW do
        // cid,name,type,notnull,dflt_value,pk
        AddRawUtf8(Names, n, sqlite3.column_text(R.Request, 1));
      SetLength(Names, n);
    finally
      R.Close;
    end;
  finally
    UnLock;
  end;
end;

function TSqlDataBase.GetUseCache: boolean;
begin
  result := (self <> nil) and
            (fCache <> nil);
end;

procedure TSqlDataBase.SetUseCache(const Value: boolean);
begin
  if self <> nil then
    if Value <> UseCache then
      if Value and
         (fUseCacheSize > 0) then
        fCache := TSynCache.Create(fUseCacheSize, true)
      else
        FreeAndNil(fCache);
end;

function IsCacheable(const aSql: RawUtf8): boolean;
begin
  result := IsSelect(pointer(aSql)) and
            (PosEx(SQLDATABASE_NOCACHE, aSql) = 0);
end;

procedure TSqlDataBase.Lock(const aSql: RawUtf8);
begin
  if self = nil then
    exit; // avoid GPF in case of call from a static-only server
  if (aSql = '') or
     IsCacheable(aSql) then
    fSafe.Lock         // on non-concurent calls, is very fast
  else
    LockAndFlushCache; // INSERT UPDATE DELETE statements need to flush cache
end;

procedure TSqlDataBase.LockAndFlushCache;
begin
  if self = nil then
    exit; // avoid GPF in case of call from a static-only server
  fSafe.Lock; // on non-concurent calls, this API is very fast
  try
    CacheFlush;
  except
    on Exception do
    begin
      // ensure critical section is left even on error
      fSafe.UnLock;
      raise;
    end;
  end;
end;

procedure TSqlDataBase.Lock;
begin
  if self <> nil then
    fSafe.Lock; // on non-concurent calls, this API is very fast
end;

procedure TSqlDataBase.UnLock;
begin
  if self <> nil then
    fSafe.UnLock; // on non-concurent calls, this API is very fast
end;

function TSqlDataBase.LockJson(const aSql: RawUtf8;
  aResultCount: PPtrInt): RawUtf8;
begin
  if self = nil then
  begin
    result := '';
    exit; // avoid GPF in case of call from a static-only server
  end;
  fSafe.Lock; // cache access is also protected by fSafe
  try
    if IsCacheable(aSql) then
    begin
      result := fCache.Find(aSql, aResultCount); // try to get JSON result from cache
      if result <> '' then
      begin
        if fLog <> nil then
        begin
          fLog.Add.Log(sllSQL, 'from cache % %', [FileNameWithoutPath, aSql], self);
          fLog.Add.Log(sllResult, result, self, fLogResultMaximumSize);
        end;
        fSafe.UnLock; // found in cache -> leave critical section
      end;
    end
    else
    begin
      // UPDATE, INSERT or any non SELECT statement
      CacheFlush;
      result := '';
    end;
  except
    on Exception do
    begin
      // ensure critical section is left even on error
      fSafe.UnLock;
      raise;
    end;
  end;
end;

procedure TSqlDataBase.UnLockJson(const aJsonResult: RawUtf8;
  aResultCount: PtrInt);
begin
  if self <> nil then
  try
    if fLog <> nil then
      fLog.Add.Log(sllResult, aJsonResult, self, fLogResultMaximumSize);
    fCache.Add(aJsonResult, aResultCount); // no-op if Reset was made just before
  finally
    fSafe.UnLock; // on non-concurent calls, this API is very fast
  end;
end;

function TSqlDataBase.Backup(const BackupFileName: TFileName): boolean;
var
  log: ISynLog;
begin
  log := fLog.Enter('Backup % -> %',
    [fFileNameWithoutPath, BackupFileName], self);
  if self = nil then
  begin
    result := false;
    exit; // avoid GPF in case of call from a static-only server
  end;
  Rollback; // any unfinished transaction is rollbacked
  Execute('VACUUM;');
  LockAndFlushCache;
  try
    try
      if log <> nil then
        log.Log(sllTrace, 'close', self);
      DBClose;
      if log <> nil then
        log.Log(sllTrace, 'copy file', self);
      result := CopyFile(fFileName, BackupFileName, false);
    finally
      if log <> nil then
        log.Log(sllTrace, 'reopen', self);
      DBOpen;
    end;
  finally
    UnLock;
  end;
end;

function TSqlDataBase.GetBackupBackgroundInProcess: boolean;
begin
  result := (self <> nil) and
            (fBackupBackgroundInProcess <> nil);
end;

function TSqlDataBase.GetSqlite3Library: TSqlite3Library;
begin
  // class function may be better, but fails on Delphi 2005
  result := sqlite3;
end;

function TSqlDataBase.BackupBackground(const BackupFileName: TFileName;
  StepPageNumber, StepSleepMS: integer; const OnProgress: TOnSqlDatabaseBackup;
  SynLzCompress: boolean; const aPassword: RawUtf8): boolean;
var
  Dest: TSqlDatabase;
  Backup: TSqlite3Backup;
begin
  result := false;
  if (self = nil) or
     (BackupFileName = '') or
     not Assigned(sqlite3.backup_init) or
     (fBackupBackgroundInProcess <> nil) then
    exit;
  fLog.Add.Log(sllDB,'BackupBackground("%") started on %',
    [BackupFileName, FileNameWithoutPath],self);
  if FileExists(BackupFileName) then
    if not DeleteFile(BackupFileName) then
      exit;
  // see https://bitbucket.org/egrange/sql3bak for proper parameters
  Dest := TSqlDatabase.Create(BackupFileName,aPassword, 0, 1);
  Dest.SetLockingMode(lmExclusive);
  Dest.SetSynchronous(smOff);
  Dest.ExecuteNoException('PRAGMA journal_mode=MEMORY');
  Dest.ExecuteNoException('PRAGMA temp_store=MEMORY');
  Backup := sqlite3.backup_init(Dest.DB, 'main', DB, 'main');
  if Backup = 0 then
  begin
    Dest.Free;
    exit;
  end;
  fBackupBackgroundInProcess := TSqlDatabaseBackupThread.Create(Backup,
    self, Dest, StepPageNumber, StepSleepMS, SynLzCompress, OnProgress);
  repeat
    Lock;
    try
      if (fBackupBackgroundInProcess = nil) or
         (fBackupBackgroundInProcess.Step > backupNone) then
        break;
    finally
      UnLock;
    end;
    SleepHiRes(0);
  until false; // wait for the background thread to be actually started
  result := true;
end;

function TSqlDataBase.BackupBackgroundToDB(BackupDB: TSqlDatabase;
  StepPageNumber, StepSleepMS: integer;
  const OnProgress: TOnSqlDatabaseBackup): boolean;
var Backup: TSqlite3Backup;
begin
  result := false;
  if (self = nil) or
     (BackupDB = nil) or
     not Assigned(sqlite3.backup_init) or
     (fBackupBackgroundInProcess <> nil) then
    exit;
  fLog.Add.Log(sllDB,'BackupBackgroundToDB("%") started on %',
    [BackupDB.FileName, FileNameWithoutPath], self);
  Backup := sqlite3.backup_init(BackupDB.DB, 'main', DB, 'main');
  if Backup = 0 then
    exit;
  fBackupBackgroundInProcess := TSqlDatabaseBackupThread.Create(Backup,
    self, BackupDB, StepPageNumber, StepSleepMS, false, OnProgress, false);
  result := true;
end;

procedure TSqlDataBase.BackupBackgroundWaitUntilFinished(
  TimeOutSeconds: integer);

  function StepAsText: shortstring;
  begin
    Lock;
    if fBackupBackgroundInProcess = nil then
      result := 'backupFinished'
    else
      result := GetEnumName(TypeInfo(TOnSqlDatabaseBackupStep),
        ord(fBackupBackgroundInProcess.Step))^;
    UnLock;
  end;

var
  endtix: Int64;
begin
  if fBackupBackgroundInProcess = nil then
    exit;
  if TimeOutSeconds < 0 then
    // TimeOutSeconds=-1 for infinite wait (unsafe!) -> 1 minute seems enough
    TimeOutSeconds := 60;
  fLog.Add.Log(sllDB,'BackupBackgroundWaitUntilFinished(%) wait on % - %',
    [TimeOutSeconds, FileNameWithoutPath, StepAsText], self);
  endtix := GetTickCount64 + TimeOutSeconds * 1000;
  repeat
    // wait for "natural" process ending
    SleepHiRes(10);
    if fBackupBackgroundInProcess = nil then
      exit;
  until GetTickCount64 > endtix;
  fLog.Add.Log(sllDB,'BackupBackgroundWaitUntilFinished force abort on % - %',
    [FileNameWithoutPath, StepAsText], self);
  Lock;
  if fBackupBackgroundInProcess <> nil then
    // notify Execute to force loop abortion
    fBackupBackgroundInProcess.Terminate;
  UnLock;
  endtix := GetTickCount64 + TimeOutSeconds * 1000;
  repeat
    // wait for the background process to be actually aborted
    SleepHiRes(10);
  until (fBackupBackgroundInProcess = nil) or
        (GetTickCount64 > endtix);
  fLog.Add.Log(sllError,'BackupBackgroundWaitUntilFinished(%) ended on % - %',
    [TimeOutSeconds, FileNameWithoutPath, StepAsText],self);
end;

class function TSqlDataBase.BackupSynLZ(const SourceDB, DestSynLZ: TFileName;
  EraseSourceDB: boolean): boolean;
begin
  result := AlgoSynLZ.FileCompress(
    SourceDB, DestSynLZ, SQLITE3_MAGIC, {hash32=}true);
  if result and
     EraseSourceDB then
    result := DeleteFile(SourceDB);
end;

class function TSqlDataBase.BackupUnSynLZ(
  const SourceSynLZ, DestDB: TFileName): boolean;
begin
  result := AlgoSynLZ.FileUnCompress(
    SourceSynLZ, DestDB, SQLITE3_MAGIC, {hash32=}true);
end;

class function TSqlDataBase.IsBackupSynLZFile(
  const SynLZFile: TFileName): boolean;
begin
  result := AlgoSynLZ.FileIsCompressed(SynLZFile, SQLITE3_MAGIC);
end;

function TSqlDataBase.DBClose: integer;
var
  log: ISynLog;
begin
  result := SQLITE_OK;
  if (self = nil) or
     (fDB = 0) then
    exit;
  log := fLog.Enter(self, 'DBClose');
  if log <> nil then
    log.Log(sllDB,'closing [%] %', [FileName, KB(GetFileSize)], self);
  if (sqlite3 = nil) or
     not Assigned(sqlite3.close) then
    raise ESqlite3Exception.CreateUtf8(
      '%.DBClose called with no sqlite3 global', [self]);
  if fBackupBackgroundInProcess <> nil then
    BackupBackgroundWaitUntilFinished;
  result := sqlite3.close(fDB);
  fDB := 0;
  fPageSize := 0;
end;

function TSqlDataBase.EnableCustomTokenizer: integer;
var
  log: ISynLog;
begin
  result := SQLITE_OK;
  if (self = nil) or
     (fDB = 0) then
    exit;
  log := fLog.Enter;
  if log <> nil then
    log.Log(sllDB, 'Enable custom tokenizer for [%]', [FileName], self);
  if (sqlite3 = nil) or
     not Assigned(sqlite3.db_config) then
    raise ESqlite3Exception.CreateUtf8(
      '%.EnableCustomTokenizer called with no sqlite3 engine', [self]);
  result := sqlite3.db_config(fDB, SQLITE_DBCONFIG_ENABLE_FTS3_TOKENIZER, 1);
end;

function TSqlDataBase.DBOpen: integer;
var
  u: RawUtf8;
  i: integer;
  log: ISynLog;
begin
  log := fLog.Enter('DBOpen %', [fFileNameWithoutPath], self);
  if fDB <> 0 then
    raise ESqlite3Exception.Create('DBOpen called twice');
  // open the database with the proper API call
  if (sqlite3 = nil) or
     not Assigned(sqlite3.open) then
    raise ESqlite3Exception.Create('DBOpen called with no sqlite3 global');
  StringToUtf8(fFileName, u);
  {$ifdef OSPOSIX}
  // for WAL to work under Linux - see http://www.sqlite.org/vfs.html
  if assigned(sqlite3.open_v2) and
     (fPassword = '') then
  begin
    result := sqlite3.open_v2(pointer(u), fDB, fOpenV2Flags, 'unix-excl');
    if result <> SQLITE_OK then // may be 'unix-excl' is not supported by the library
      result := sqlite3.open_v2(pointer(u), fDB, fOpenV2Flags, nil);
  end
  else
  {$else}
  if fOpenV2Flags <> (SQLITE_OPEN_READWRITE or SQLITE_OPEN_CREATE) then
    result := sqlite3.open_v2(pointer(u), fDB, fOpenV2Flags, nil)
  else
  {$endif OSPOSIX}
    result := sqlite3.open(pointer(u), fDB);
  if result <> SQLITE_OK then
  begin
    if log <> nil then
      log.Log(sllError, 'sqlite3_open(%) failed with error % (%): %',
        [u, sqlite3_resultToErrorText(result), result, sqlite3.errmsg(fDB)]);
    sqlite3.close(fDB); // should always be closed, even on failure
    fDB := 0;
    exit;
  end;
  // initialize optional encryption (if supported by the compiled engine)
  if Assigned(sqlite3.key) and
     (fPassword <> '') and
     (fFileName <> '') and
     (fFileName <> SQLITE_MEMORY_DATABASE_NAME) then
    sqlite3.key(fDB, pointer(fPassword), length(fPassword));
  // tune up execution context (before accessing the database)
  if not fIsMemory then
  begin
    if (fOpenV2Flags and SQLITE_OPEN_CREATE <> 0) and
       (fFileDefaultPageSize <> 0) then
      PageSize := fFileDefaultPageSize;
    if fFileDefaultCacheSize <> 0 then
      CacheSize := fFileDefaultCacheSize; // 10000 by default (i.e. 40 MB)
  end;
  // always try to check for proper database content (and password)
  if not ExecuteNoException('select count(*) from sqlite_master') then
  begin
    result := SQLITE_NOTADB; // likely a password error
    sqlite3.close(fDB); // should always be closed, even on failure
    fDB := 0;
    exit;
  end;
  // custom fast UTF-8 WinAnsi case insensitive comparison, using NormToUpper[]
  sqlite3.create_collation(DB, 'SYSTEMNOCASE', SQLITE_UTF8, nil,
    Utf8_SYSTEMNOCASE);
  // custom fast UTF-8 Unicode 10.0 case insensitive comparison
  sqlite3.create_collation(DB, 'UNICODENOCASE', SQLITE_UTF8, nil,
    Utf8_UNICODENOCASE);
  // custom fast ISO-8601 date time encoded
  sqlite3.create_collation(DB, 'ISO8601', SQLITE_UTF8, nil,
    Utf8_ISO8601);
  // two slow but always accurate comparers, using the Win32/ICU UTF-16 API
  sqlite3.create_collation(DB, 'WIN32CASE', SQLITE_UTF16, nil,
    Utf16_WIN32CASE);
  sqlite3.create_collation(DB, 'WIN32NOCASE', SQLITE_UTF16, nil,
    Utf16_WIN32NOCASE);
  // note: standard SQLite3 NOCASE collation is used for AnsiString
  // register the MOD() user function, similar to the standard % operator
  sqlite3.create_function(DB, 'MOD', 2, SQLITE_ANY, nil,
    InternalMod, nil, nil);
  // register TIMELOG(), returning a ISO-8601 date/time from TTimeLog value
  sqlite3.create_function(DB, 'TIMELOG', 1, SQLITE_ANY, nil,
    InternalTimeLog, nil, nil);
  // register TIMELOGUNIX(), returning Unix Epoch seconds from TTimeLog value
  sqlite3.create_function(DB, 'TIMELOGUNIX', 1, SQLITE_ANY, nil,
    InternalTimeLogUnix, nil, nil);
  // register SOUNDEX() SOUNDEXFR() SOUNDEXES() functions
  sqlite3.create_function(DB, 'SOUNDEX', 1, SQLITE_UTF8, nil,
    InternalSoundex, nil, nil);
  sqlite3.create_function(DB, 'SOUNDEXFR', 1, SQLITE_UTF8, nil,
    InternalSoundexFr, nil, nil);
  sqlite3.create_function(DB, 'SOUNDEXES', 1, SQLITE_UTF8, nil,
    InternalSoundexEs, nil, nil);
  // rank() function as proposed in http://www.sqlite.org/fts3.html#appendix_a
  sqlite3.create_function(DB, 'RANK', -1, SQLITE_ANY, nil,
    InternalRank, nil, nil);
  // register CONCAT() function to process fast string concatenation
  sqlite3.create_function(DB, 'CONCAT', 2, SQLITE_UTF8, nil, nil,
    InternalConcatStep, InternalConcatFinal);
  // functions to handle some standard dynamic array BLOB content in SQL
  // IntegerDynArrayContains(BlobField,10) returning a boolean
  sqlite3.create_function(DB, 'INTEGERDYNARRAYCONTAINS', 2, SQLITE_ANY, nil,
    InternalIntegerDynArray, nil, nil);
  // Byte/Word/Cardinal/Int64/CurrencyDynArrayContains(BlobField,I64)
  sqlite3.create_function(DB, 'BYTEDYNARRAYCONTAINS', 2, SQLITE_ANY,
    TypeInfo(TByteDynArray), InternalSimpleInt64DynArray, nil, nil);
  sqlite3.create_function(DB, 'WORDDYNARRAYCONTAINS', 2, SQLITE_ANY,
    TypeInfo(TWordDynArray), InternalSimpleInt64DynArray, nil, nil);
  sqlite3.create_function(DB, 'CARDINALDYNARRAYCONTAINS', 2, SQLITE_ANY,
    TypeInfo(TCardinalDynArray), InternalSimpleInt64DynArray, nil, nil);
  sqlite3.create_function(DB, 'INT64DYNARRAYCONTAINS', 2, SQLITE_ANY,
    TypeInfo(TInt64DynArray), InternalSimpleInt64DynArray, nil, nil);
  sqlite3.create_function(DB, 'CURRENCYDYNARRAYCONTAINS', 2, SQLITE_ANY,
    TypeInfo(TInt64DynArray), InternalSimpleInt64DynArray, nil, nil);
  // RawUtf8DynArrayContainsCase/NoCase(BlobField,'Text') returning a boolean
  sqlite3.create_function(DB, 'RAWUTF8DYNARRAYCONTAINSCASE', 2, SQLITE_ANY,
    nil, InternalRawUtf8DynArray, nil, nil);
  sqlite3.create_function(DB, 'RAWUTF8DYNARRAYCONTAINSNOCASE', 2, SQLITE_ANY,
    @Utf8ILComp, InternalRawUtf8DynArray, nil, nil);
  // JSON related functions (e.g. for ORM storing variants as JSON UTF-8 text)
  sqlite3.create_function(DB, 'JSONGET', 2, SQLITE_ANY, nil,
    InternalJsonGet, nil, nil);
  sqlite3.create_function(DB, 'JSONHAS', 2, SQLITE_ANY, nil,
    InternalJsonHas, nil, nil);
  sqlite3.create_function(DB, 'JSONSET', 3, SQLITE_ANY, nil,
    InternalJsonSet, nil, nil);
  // register UNICODEUPPER() function using Unicode 10.0 uppercase folding
  sqlite3.create_function(DB, 'UNICODEUPPER', 1, SQLITE_UTF8, nil,
    InternalUnicodeUpper, nil, nil);
  // reallocate all TSqlDataBaseSQLFunction for re-Open (TRestServerDB.Backup)
  for i := 0 to fSqlFunctions.Count - 1 do
    TSqlDataBaseSQLFunction(fSqlFunctions.List[i]).CreateFunction(DB);
  i := CacheSize;
  if i < 0 then
    i := (-i) shr 10
  else
    i := PageSize * CacheSize;
  if log <> nil then
    log.Log(sllDB, '"%" database file (%) opened with PageSize=% CacheSize=% (%)',
      [FileName, KB(GetFileSize), PageSize, CacheSize, KB(i)], self);
end;

function TSqlDataBase.GetUserVersion: cardinal;
begin
  result := ExecuteNoExceptionInt64('PRAGMA user_version');
end;

procedure TSqlDataBase.SetUserVersion(const Value: cardinal);
begin
  ExecuteNoException('PRAGMA user_version=' + Int32ToUtf8(Value));
end;

function TSqlDataBase.GetCacheSize: cardinal;
begin
  result := ExecuteNoExceptionInt64('PRAGMA cache_size');
end;

procedure TSqlDataBase.SetCacheSize(const Value: cardinal);
begin
  ExecuteNoException('PRAGMA cache_size=' + UInt32ToUtf8(Value));
end;

function TSqlDataBase.GetPageSize: cardinal;
begin
  if fPageSize = 0 then
    // can be cached, since not change once opened
    fPageSize := ExecuteNoExceptionInt64('PRAGMA page_size');
  result := fPageSize;
end;

procedure TSqlDataBase.SetPageSize(const Value: cardinal);
begin
  if ExecuteNoException('PRAGMA page_size=' + UInt32ToUtf8(Value)) then
    fPageSize := Value;
end;

function TSqlDataBase.GetPageCount: cardinal;
begin
  result := ExecuteNoExceptionInt64('PRAGMA page_count');
end;

function TSqlDataBase.GetFileSize: Int64;
begin
  result := GetPageCount;
  result := result * GetPageSize;
end;

procedure TSqlDataBase.SetSynchronous(const Value: TSqlSynchronousMode);
begin
  ExecuteNoException('PRAGMA synchronous=' + UInt32ToUtf8(ord(Value)));
end;

procedure TSqlDataBase.SetMemoryMappedMB(const Value: cardinal);
begin
  ExecuteNoException('PRAGMA mmap_size=' + Int64ToUtf8(Value shl 20));
end;

function TSqlDataBase.GetMemoryMappedMB: cardinal;
begin
  result := ExecuteNoExceptionInt64('PRAGMA mmap_size') shr 20;
end;

function TSqlDataBase.GetSynchronous: TSqlSynchronousMode;
begin
  result := TSqlSynchronousMode(ExecuteNoExceptionInt64('PRAGMA synchronous'));
end;

procedure TSqlDataBase.SetLockingMode(const Value: TSqlLockingMode);
const
  CMD: array[TSqlLockingMode] of RawUtf8 = (
    'NORMAL;', 'EXCLUSIVE;');
begin
  ExecuteNoException('PRAGMA locking_mode=' + CMD[Value]);
end;

function TSqlDataBase.GetLockingMode: TSqlLockingMode;
var
  tmp: RawUtf8;
begin
  tmp := ExecuteNoExceptionUtf8('PRAGMA locking_mode');
  if IdemPropNameU(tmp, 'EXCLUSIVE') then
    result := lmExclusive
  else
    result := lmNormal;
end;

procedure TSqlDataBase.SetWALMode(Value: boolean);
const
  CMD: array[boolean] of RawUtf8 = (
    'DELETE;', 'WAL;');
begin
  ExecuteNoException('PRAGMA journal_mode=' + CMD[Value]);
end;

function TSqlDataBase.GetWALMode: boolean;
begin
  result := IdemPropNameU(ExecuteNoExceptionUtf8('PRAGMA journal_mode'), 'wal');
end;

procedure TSqlDataBase.SetBusyTimeout(const ms: integer);
begin
  if (self = nil) or
     (fDB = 0) then
    exit;
  sqlite3.busy_timeout(DB, ms);
  fBusyTimeout := ms;
end;

function TSqlDataBase.GetLimit(Category: TSqlLimitCategory): integer;
begin
  if (self = nil) or
     (fDB = 0) or
     not Assigned(sqlite3.limit) then
    result := 0
  else
    result := sqlite3.limit(fDB, ord(Category), -1);
end;

procedure TSqlDataBase.SetLimit(Category: TSqlLimitCategory; Value: integer);
begin
  if (self <> nil) and
     Assigned(sqlite3.limit) then
    sqlite3.limit(fDB, ord(Category), Value);
end;

procedure TSqlDataBase.CacheFlush;
begin
  if self = nil then
    exit;
  if InternalState <> nil then
    inc(InternalState^);
  if fCache.Reset then
    if fLog <> nil then
      fLog.Add.Log(sllCache, '% cache flushed', [FileNameWithoutPath], self);
end;

procedure TSqlDataBase.RegisterSQLFunction(aFunction: TSqlDataBaseSQLFunction);
var
  i: PtrInt;
begin
  if (self = nil) or
     (aFunction = nil) then
    exit;
  for i := 0 to fSqlFunctions.Count - 1 do
    with TSqlDataBaseSQLFunction(fSqlFunctions.List[i]) do
      if (FunctionParametersCount = aFunction.FunctionParametersCount) and
        IdemPropNameU(FunctionName, aFunction.FunctionName) then
      begin
        aFunction.Free;
        exit; // already registered with the same name and parameters count
      end;
  if fLog <> nil then
    fLog.Add.Log(sllDB, '% RegisterSQLFunction("%") %',
      [FileNameWithoutPath, aFunction.FunctionName], self);
  fSqlFunctions.Add(aFunction);
  if DB <> 0 then
    // DB already opened -> register this custom function
    aFunction.CreateFunction(DB);
end;

procedure TSqlDataBase.RegisterSQLFunction(aDynArrayTypeInfo: PRttiInfo;
  aCompare: TDynArraySortCompare; const aFunctionName: RawUtf8);
begin
  RegisterSQLFunction(TSqlDataBaseSQLFunctionDynArray.Create(aDynArrayTypeInfo,
    aCompare, aFunctionName));
end;

procedure TSqlDataBase.RegisterSQLFunction(aFunction: TSqlFunctionFunc;
  aFunctionParametersCount: integer; const aFunctionName: RawUtf8);
begin
  RegisterSQLFunction(TSqlDataBaseSQLFunction.Create(aFunction,
    aFunctionParametersCount, aFunctionName));
end;



{ TSqlRequest }

procedure TSqlRequest.Bind(Param: integer; Value: Int64);
begin
  sqlite3_check(RequestDB,
    sqlite3.bind_int64(Request, Param, Value), 'bind_int64');
end;

procedure TSqlRequest.Bind(Param: integer; Value: double);
begin
  sqlite3_check(RequestDB,
    sqlite3.bind_double(Request, Param, Value), 'bind_double');
end;

procedure TSqlRequest.Bind(Param: integer; const Value: RawUtf8);
var
  tmp: pointer;
begin
  // note that the official SQLite3 documentation is missleading:
  // sqlite3.bind_text(Text_bytes) must EXCLUDE the null terminator, otherwise a
  // #0 is appended to all column values -> so length(Value) is needed below
  if pointer(Value) = nil then
    // avoid to bind '' as null
    sqlite3_check(RequestDB,
      sqlite3.bind_text(Request, Param, @NULCHAR, 0, SQLITE_STATIC))
  else
  begin
    // assign RawUtf8 value by reference, to avoid memory allocation
    tmp := nil;
    RawByteString(tmp) := Value;
    // sqlite3InternalFreeRawByteString will decrease RefCount
    sqlite3_check(RequestDB,
      sqlite3.bind_text(Request, Param, tmp, length(Value),
        sqlite3InternalFreeRawByteString), 'bind_text');
  end;
end;

procedure TSqlRequest.BindS(Param: integer; const Value: string);
var
  P: PUtf8Char;
  len: integer;
begin
  if pointer(Value) = nil then
  begin
    // avoid to bind '' as null
    sqlite3_check(RequestDB,
      sqlite3.bind_text(Request, Param, @NULCHAR, 0, SQLITE_STATIC));
    exit;
  end;
  len := length(Value);
  GetMem(P, len * 3 + 1);
  {$ifdef UNICODE}
  len := RawUnicodeToUtf8(P, len * 3, pointer(Value), len, []);
  {$else}
  len := CurrentAnsiConvert.AnsiBufferToUtf8(P, pointer(Value), len) - P;
  {$endif UNICODE}
  sqlite3_check(RequestDB,
    sqlite3.bind_text(Request, Param, P, len, @sqlite3InternalFree), 'bind_text');
end;

procedure TSqlRequest.Bind(Param: integer; Data: pointer; Size: integer);
begin
  sqlite3_check(RequestDB,
    sqlite3.bind_blob(Request, Param, Data, Size, SQLITE_TRANSIENT),
      'bind_blob'); // SQLITE_TRANSIENT = make private copy of the data
end;

procedure TSqlRequest.BindBlob(Param: integer; const Data: RawByteString);
var
  tmp: pointer;
begin
  // assign RawByteString value by reference, to avoid memory allocation
  tmp := nil;
  RawByteString(tmp) := Data;
  // sqlite3InternalFreeRawByteString will decrease RefCount
  sqlite3_check(RequestDB,
    sqlite3.bind_blob(Request, Param, tmp, length(Data),
      sqlite3InternalFreeRawByteString), 'bind_blob');
end;

procedure TSqlRequest.Bind(Param: integer; Data: TCustomMemoryStream);
begin
  Bind(Param, Data.Memory, Data.Size);
end;

procedure TSqlRequest.BindNull(Param: integer);
begin
  sqlite3_check(RequestDB, sqlite3.bind_null(Request, Param));
end;

procedure TSqlRequest.BindReset;
begin
  if Request <> 0 then
    sqlite3.clear_bindings(Request);
end;

procedure TSqlRequest.BindZero(Param, Size: integer);
begin
  sqlite3_check(RequestDB, sqlite3.bind_zeroblob(Request, Param, Size));
end;

procedure TSqlRequest.Close;
var
  saved: cardinal;
begin
  if Request = 0 then
    exit;
  saved := SetFpuFlags(ffLibrary);
  sqlite3.finalize(Request);
  ResetFpuFlags(saved);
  fRequest := 0;
  fFieldCount := 0;
end;

procedure TSqlRequest.ExecuteAll;
begin
  if RequestDB = 0 then
    raise ESqlite3Exception.Create(0, SQLITE_CANTOPEN, 'ExecuteAll');
  try
    repeat
      repeat
      until Step <> SQLITE_ROW; // all steps of this statement
    until PrepareNext = SQLITE_DONE; // all statements
  finally
    Close; // always release statement
  end;
end;

procedure TSqlRequest.Execute;
begin
  if RequestDB = 0 then
    raise ESqlite3Exception.Create(0, SQLITE_CANTOPEN, 'Execute');
  try
    repeat
    until Step <> SQLITE_ROW; // Execute all steps of the first statement
  finally
    Close; // always release statement
  end;
end;

procedure TSqlRequest.ExecuteAll(aDB: TSqlite3DB; const aSql: RawUtf8);
begin
  try
    Prepare(aDB, aSql); // will raise an ESqlite3Exception on error
    ExecuteAll;
  finally
    Close; // always release statement, even if done normally in EngineExecuteAll
  end;
end;

procedure TSqlRequest.Execute(aDB: TSqlite3DB; const aSql: RawUtf8);
begin
  try
    Prepare(aDB, aSql); // will raise an ESqlite3Exception on error
    Execute;
  finally
    Close; // always release statement, even if done normally in Execute
  end;
end;

function TSqlRequest.ExecuteNoException(aDB: TSqlite3DB;
  const aSql: RawUtf8): boolean;
begin
  // avoid sqlite3_check() calls for no ESqlite3Exception
  result := false;
  if (aDB <> 0) and
     (aSql <> '') then
  try
    if not (Prepare(aDB, aSql, {noexcept=}true) in SQLITE_ERRORS) and
       (Request <> 0) and
       not (sqlite3.step(Request) in SQLITE_ERRORS) then
      result := true;
  finally
    Close; // always release statement, even if done normally in Execute
  end;
end;

function TSqlRequest.Execute(aDB: TSqlite3DB; const aSql: RawUtf8;
  var aValues: TInt64DynArray): integer;
var
  Res: integer;
begin
  result := 0;
  try
    Prepare(aDB, aSql); // will raise an ESqlite3Exception on error
    if FieldCount > 0 then
      repeat
        Res := Step;
        if Res = SQLITE_ROW then
          // retrieve first column values
          AddInt64(aValues, result, sqlite3.column_int64(Request, 0));
      until Res = SQLITE_DONE;
  finally
    Close; // always release statement
  end;
end;

procedure TSqlRequest.Execute(aDB: TSqlite3DB; const aSql: RawUtf8;
  out aValue: Int64);
begin
  aValue := 0;
  try
    Prepare(aDB, aSql); // will raise an ESqlite3Exception on error
    if FieldCount > 0 then
      if Step = SQLITE_ROW then
        // retrieve first column value
        aValue := sqlite3.column_int64(Request, 0);
  finally
    Close; // always release statement
  end;
end;

procedure TSqlRequest.Execute(aDB: TSqlite3DB; const aSql: RawUtf8;
  out aValue: RawUtf8);
begin
  try
    Prepare(aDB, aSql); // will raise an ESqlite3Exception on error
    if FieldCount > 0 then
      if Step = SQLITE_ROW then
        // retrieve first column value
        aValue := sqlite3.column_text(Request, 0);
  finally
    Close; // always release statement
  end;
end;

function TSqlRequest.Execute(aDB: TSqlite3DB; const aSql: RawUtf8;
  var aValues: TRawUtf8DynArray): integer;
var
  Res: integer;
begin
  result := 0;
  try
    Prepare(aDB, aSql); // will raise an ESqlite3Exception on error
    if FieldCount > 0 then
      repeat
        Res := Step;
        if Res = SQLITE_ROW then
          // retrieve first column values
          AddRawUtf8(aValues, result, sqlite3.column_text(Request, 0));
      until Res = SQLITE_DONE;
  finally
    Close; // always release statement
  end;
end;

function TSqlRequest.Execute(aDB: TSqlite3DB; const aSql: RawUtf8;
  Json: TStream; Expand: boolean; MaxMemory: PtrUInt;
  Options: TTextWriterOptions): PtrInt;
// expand=true: [ {"col1":val11,"col2":"val12"},{"col1":val21,... ]
// expand=false: { "FieldCount":2,"Values":["col1","col2",val11,"val12",val21,..] }
var
  i: PtrInt;
  W: TJsonWriter;
  tmp: TTextWriterStackBuffer;
begin
  result := 0;
  W := TJsonWriter.Create(Json, Expand, false, nil, 0, @tmp);
  try
    W.CustomOptions := W.CustomOptions + Options;
    // prepare the SQL request
    if aSql <> '' then // if not already prepared, reset and bound by caller
      Prepare(aDB, aSql); // will raise an ESqlite3Exception on error
    if FieldCount <= 0 then
    begin
      W.CancelAllVoid;
      exit;
    end;
    // get col names and types
    SetLength(W.ColNames, FieldCount);
    for i := 0 to FieldCount - 1 do
      W.ColNames[i] := sqlite3.column_name(Request, i);
    W.AddColumns; // write or init field names for appropriate Json Expand
    if Expand then
      W.Add('[');
    // write rows data
    repeat
      case Step of
        SQLITE_ROW:
          begin
            FieldsToJson(W);
            W.AddComma;
            inc(result);
            if W.WrittenBytes > MaxMemory then // TextLength is slower
              raise ESqlite3Exception.CreateUTF8(
                'TSqlRequest.Execute: output overflow after % for [%]',
                [KB(MaxMemory), aSql]);
          end;
        SQLITE_DONE:
          break;
      end;
    until false;
    if (result = 0) and
       W.Expand then
    begin
      // we want the field names at least, even with no data: allow RowCount=0
      W.Expand := false; //  {"FieldCount":2,"Values":["col1","col2"]}
      W.CancelAll;
      for i := 0 to FieldCount - 1 do
        W.ColNames[i] := sqlite3.column_name(Request, i);
      W.AddColumns;
    end;
    W.EndJsonObject(0, result);
  finally
    try
      if aSql <> '' then
        Close; // always release statement (if not prepared and closed by caller)
    finally
      W.Free;
    end;
  end;
end;

{$I-}
procedure TSqlRequest.ExecuteDebug(aDB: TSqlite3DB; const aSql: RawUtf8;
  var OutFile: Text);
var
  Res, i, n: integer;
begin
  writeln;
  try
    Prepare(aDB, aSql); // will raise an ESqlite3Exception on error
    repeat
      repeat
        Res := Step;
        if Res = SQLITE_ROW then
        begin
          n := FieldCount - 1;
          for i := 0 to n do
          begin
            write(OutFile,
              {$ifdef OSWINDOWS} FieldA {$else} FieldUtf8 {$endif}(i));
            if i < n then
              write(OutFile, '|');
          end;
          writeln(OutFile);
        end;
      until Res = SQLITE_DONE;
    until PrepareNext = SQLITE_DONE;
  finally
    ioresult;
    Close; // always release statement
  end;
end;
{$I+}

function TSqlRequest.ExecuteJson(aDB: TSqlite3DB; const aSql: RawUtf8;
  Expand: boolean; aResultCount: PPtrInt; MaxMemory: PtrUInt;
  Options: TTextWriterOptions): RawUtf8;
var
  Stream: TRawByteStringStream;
  RowCount: PtrInt;
begin
  Stream := TRawByteStringStream.Create;
  try
    try
      // create JSON data in Stream
      RowCount := Execute(aDB, aSql, Stream, Expand, MaxMemory, Options);
      if aResultCount <> nil then
        aResultCount^ := RowCount;
      result := Stream.DataString;
    except
      on ESqlite3Exception do
        result := '';
    end;
    // Close has been called in Execute() above since aSql<>''
  finally
    Stream.Free;
  end;
end;

function TSqlRequest.FieldA(Col: integer): WinAnsiString;
var
  P: PUtf8Char;
  L, L2: integer;
begin
  result := '';
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldA');
  P := sqlite3.column_text(Request, Col);
  L := StrLen(P); // faster than sqlite3.column_bytes(Request,Col)
  if L > 0 then
  begin
    SetLength(result, L);
    L2 := Utf8ToWinPChar(pointer(result), P, L);
    if L2 <> L then
      SetLength(result, L2);
  end;
end;

function TSqlRequest.FieldBlob(Col: integer): RawByteString;
var
  P: PUtf8Char;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldBlob');
  P := sqlite3.column_blob(Request, Col);
  SetString(result, P, sqlite3.column_bytes(Request, Col));
end;

function TSqlRequest.FieldBlobToStream(Col: integer): TStream;
begin
  result := TRawByteStringStream.Create(FieldBlob(Col));
end;

function TSqlRequest.FieldDouble(Col: integer): double;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldDouble');
  result := sqlite3.column_double(Request, Col);
end;

function TSqlRequest.FieldInt(Col: integer): Int64;
begin
  // internally, SQLite always uses Int64 -> pure integer function is useless
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldInt');
  result := sqlite3.column_int64(Request, Col);
end;

function TSqlRequest.FieldName(Col: integer): RawUtf8;
var
  P: PUtf8Char;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldName');
  P := sqlite3.column_name(Request, Col);
  FastSetString(result, P, StrLen(P));
end;

function TSqlRequest.FieldIndex(const aColumnName: RawUtf8): integer;
begin
  if Request = 0 then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_MISUSE, 'FieldIndex');
  for result := 0 to FieldCount - 1 do
    if StrIComp(pointer(aColumnName), sqlite3.column_name(Request, result)) = 0 then
      exit;
  result := -1; // not found
end;

function TSqlRequest.FieldNull(Col: integer): boolean;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldNull');
  result := sqlite3.column_type(Request, Col) = SQLITE_NULL;
end;

function TSqlRequest.FieldType(Col: integer): integer;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldType');
  result := sqlite3.column_type(Request, Col);
end;

function TSqlRequest.FieldDeclaredType(Col: integer): RawUtf8;
var
  P: PUtf8Char;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldDeclaredType');
  P := pointer(sqlite3.column_decltype(Request, Col));
  FastSetString(result, P, StrLen(P));
end;

function TSqlRequest.FieldDeclaredTypeS(Col: integer): string;
var
  P: PUtf8Char;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldDeclaredTypeS');
  P := pointer(sqlite3.column_decltype(Request, Col));
  result := Utf8DecodeToString(P, StrLen(P));
end;

function TSqlRequest.FieldUtf8(Col: integer): RawUtf8;
var
  P: PUtf8Char;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldUTF8');
  P := pointer(sqlite3.column_text(Request, Col));
  FastSetString(result, P, StrLen(P));
end;

{$ifdef UNICODE}

function TSqlRequest.FieldS(Col: integer): string;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldS');
  result := sqlite3.column_text16(Request, Col);
end;

{$else}

function TSqlRequest.FieldS(Col: integer): string;
var
  P: PUtf8Char;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldS');
  P := pointer(sqlite3.column_text(Request, Col));
  CurrentAnsiConvert.Utf8BufferToAnsi(P, StrLen(P), RawByteString(result));
end;

{$endif UNICODE}

function TSqlRequest.FieldValue(Col: integer): TSqlite3Value;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldValue');
  result := sqlite3.column_value(Request, Col);
end;

function TSqlRequest.FieldW(Col: integer): RawUnicode;
var
  P: PWideChar;
begin
  if cardinal(Col) >= cardinal(FieldCount) then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_RANGE, 'FieldW');
  P := sqlite3.column_text16(Request, Col);
  SetString(result, PUtf8Char(pointer(P)), StrLenW(P) * 2 + 1);
end;

function TSqlRequest.Prepare(DB: TSqlite3DB; const SQL: RawUtf8;
  NoExcept: boolean): integer;
var
  saved: cardinal;
begin
  fDB := DB;
  fRequest := 0;
  fResetDone := false;
  if DB = 0 then
    raise ESqlite3Exception.Create(DB, SQLITE_CANTOPEN, SQL);
  saved := SetFpuFlags(ffLibrary);
  try
    result := sqlite3.prepare_v2(RequestDB, pointer(SQL), length(SQL) + 1,
      fRequest, fNextSQL);
    while (result = SQLITE_OK) and
          (Request = 0) do
    begin
      // loop handling comment or white-space
      result := sqlite3.prepare_v2(RequestDB,
        fNextSQL, -1, fRequest, fNextSQL);
      if fNextSQL^ = #0 then
        // statement contains only comment
       raise ESqlite3Exception.Create(DB, SQLITE_EMPTY, SQL);
    end;
    fFieldCount := sqlite3.column_count(fRequest);
    if not NoExcept then
      sqlite3_check(RequestDB, result, SQL);
  finally
    ResetFpuFlags(saved);
  end;
end;

function TSqlRequest.PrepareAnsi(DB: TSqlite3DB;
  const SQL: WinAnsiString): integer;
begin
  result := Prepare(DB, WinAnsiToUtf8(SQL));
end;

function TSqlRequest.PrepareNext: integer;
begin
  if (Request = 0) or
     (fNextSQL^ = #0) then
    result := SQLITE_DONE
  else
  begin
    Close; // free any previous statement
    result := sqlite3.prepare_v2(RequestDB, fNextSQL, -1, fRequest, fNextSQL);
    while (result = SQLITE_OK) and
          (Request = 0) and
          (fNextSQL^ <> #0) do
      // comment or white-space -> ignore
      result := sqlite3.prepare_v2(RequestDB, fNextSQL, -1, fRequest, fNextSQL);
    fFieldCount := sqlite3.column_count(fRequest);
    sqlite3_check(RequestDB, result, 'PrepareNext');
    if Request = 0 then
      result := SQLITE_DONE; // nothing more to add
  end;
end;

function TSqlRequest.Reset: integer;
var
  saved: cardinal;
begin
  if Request = 0 then
    raise ESqlite3Exception.Create(
      'TSqlRequest.Reset called with no previous Request');
  if fResetDone then
  begin
    result := SQLITE_OK;
    exit;
  end;
  saved := SetFpuFlags(ffLibrary);
  // no check here since it is in PREVIOUS execution error state
  result := sqlite3.reset(Request);
  ResetFpuFlags(saved);
  fResetDone := true;
end;

function TSqlRequest.Step: integer;
var
  saved: cardinal;
begin
  if Request = 0 then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_MISUSE, 'Step');
  fResetDone := false;
  saved := SetFpuFlags(ffLibrary);
  try
    result := sqlite3_check(RequestDB, sqlite3.step(Request), 'Step');
  finally
    ResetFpuFlags(saved);
  end;
end;

function TSqlRequest.GetReadOnly: boolean;
begin
  if Request = 0 then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_MISUSE, 'IsReadOnly');
  result := sqlite3.stmt_readonly(Request) <> 0;
end;

procedure TSqlRequest.FieldsToJson(WR: TJsonWriter; DoNotFetchBlobs: boolean);
var
  i: PtrInt;
  P: PUtf8Char;
  typ: integer;
begin
  if Request = 0 then
    raise ESqlite3Exception.Create(RequestDB, SQLITE_MISUSE, 'FieldsToJson');
  if WR.Expand then
    WR.Add('{');
  for i := 0 to FieldCount - 1 do
  begin
    typ := sqlite3.column_type(Request, i); // fast evaluation: type may vary
    P := nil;
    if twoIgnoreDefaultInRecord in WR.CustomOptions then
    begin
      case typ of
        SQLITE_BLOB:
          if DoNotFetchBlobs or
             (sqlite3.column_bytes(Request, i) = 0) then
            continue;
        SQLITE_NULL:
          continue;
        SQLITE_INTEGER:
          if sqlite3.column_int64(Request, i) = 0 then
            continue;
        SQLITE_FLOAT:
          if sqlite3.column_double(Request, i) = 0 then
            continue;
        SQLITE_TEXT:
          begin
            P := sqlite3.column_text(Request, i);
            if P = nil then
              continue;
          end;
      end;
    end;
    if WR.Expand then
      WR.AddString(WR.ColNames[i]); // '"'+ColNames[]+'":'
    case typ of
      SQLITE_BLOB:
        if DoNotFetchBlobs then
          WR.AddShort('null')
        else
          WR.WrBase64(pointer(sqlite3.column_blob(Request, i)),
            sqlite3.column_bytes(Request, i), {withMagic=}true);
      SQLITE_NULL:
        WR.AddShort('null'); // returned also for ""
      SQLITE_INTEGER:
        WR.Add(sqlite3.column_int64(Request, i));
      SQLITE_FLOAT:
        WR.AddDouble(sqlite3.column_double(Request, i));
      SQLITE_TEXT:
        begin
          WR.Add('"');
          if P = nil then
            P := sqlite3.column_text(Request, i);
          WR.AddJsonEscape(P, 0);
          WR.Add('"');
        end;
    end; // case ColTypes[]
    WR.AddComma;
  end;
  WR.CancelLastComma; // cancel last ','
  if WR.Expand then
    WR.Add('}');
end;

function TSqlRequest.GetParamCount: integer;
begin
  if Request = 0 then
    result := 0
  else
    result := sqlite3.bind_parameter_count(Request);
end;



{ TSqlBlobStream }

constructor TSqlBlobStream.Create(aDB: TSqlite3DB; const DBName, TableName,
  ColumnName: RawUtf8; RowID: Int64; ReadWrite: boolean);
begin
  fDB := aDB;
  fWritable := ReadWrite;
  sqlite3_check(aDB, sqlite3.blob_open(aDB, pointer(DBName), pointer(TableName),
    pointer(ColumnName), RowID, integer(ReadWrite), fBlob), 'blob_open');
  fSize := sqlite3.blob_bytes(fBlob);
end;

destructor TSqlBlobStream.Destroy;
begin
  sqlite3.blob_close(fBlob);
  inherited;
end;

function TSqlBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  result := fSize - fPosition; // bytes available left
  if Count < result then // read only inside the Blob size
    result := Count;
  if result <> 0 then
  begin
    // warning: sqlite3.blob_read() seems to work with 32-bit position only
    sqlite3_check(fDB, sqlite3.blob_read(fBlob, Buffer, result, fPosition));
    inc(fPosition, result);
  end;
end;

function TSqlBlobStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  result := Seek(Offset, TSeekOrigin(Origin));
end;

function TSqlBlobStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning:
      fPosition := Offset;
    soCurrent:
      Inc(fPosition, Offset);
    soEnd:
      fPosition := fSize + Offset;
  end;
  if fPosition > fSize then
    fPosition := fSize;
  result := fPosition;
end;

procedure TSqlBlobStream.ChangeRow(RowID: Int64);
begin
  if not Assigned(sqlite3.blob_reopen) then
    raise ESqlite3Exception.Create('blob_reopen API not available');
  sqlite3_check(fDB, sqlite3.blob_reopen(fBlob, RowID), 'blob_reopen');
  fPosition := 0;
  fSize := sqlite3.blob_bytes(fBlob);
end;

function TSqlBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  result := fSize - fPosition; // bytes available left
  if Count < result then
    result := Count; // write only inside the Blob size
  if result <> 0 then
  begin
    sqlite3_check(fDB, sqlite3.blob_write(fBlob, Buffer, result, fPosition));
    inc(fPosition, result);
  end;
end;


{ TSqlDataBaseSQLFunction }

constructor TSqlDataBaseSQLFunction.Create(aFunction: TSqlFunctionFunc;
  aFunctionParametersCount: integer; const aFunctionName: RawUtf8);
begin
  fInternalFunction := aFunction;
  fFunctionParametersCount := aFunctionParametersCount;
  if aFunctionName = '' then
    fSqlName := RawUtf8(copy(ClassName, 2, maxInt))
  else
    fSqlName := aFunctionName;
end;

function TSqlDataBaseSQLFunction.CreateFunction(DB: TSqlite3DB): integer;
begin
  if self <> nil then
    result := sqlite3.create_function(DB, pointer(fSqlName),
      FunctionParametersCount, SQLITE_ANY, self, fInternalFunction, nil, nil)
  else
    result := SQLITE_ERROR;
end;


{ TSqlDataBaseSQLFunctionDynArray }

procedure InternalSQLFunctionDynArrayBlob(Context: TSqlite3FunctionContext;
  argc: integer; var argv: TSqlite3ValueArray); cdecl;
var
  P, item: PAnsiChar;
  PLen, itemLen: PtrInt;
  caller: TSqlDataBaseSQLFunctionDynArray;
begin
  if argc <> 2 then
  begin
    // two parameters expected
    ErrorWrongNumberOfArgs(Context);
    exit;
  end;
  P := sqlite3.value_blob(argv[0]);
  PLen := sqlite3.value_bytes(argv[0]);
  item := sqlite3.value_blob(argv[1]);
  itemLen := sqlite3.value_bytes(argv[1]);
  caller := sqlite3.user_data(Context);
  if (P <> nil) and
     (PLen > 0) and
     (item <> nil) and
     (itemLen > 0) and
     (caller <> nil) then
    with caller.fDummyDynArray do
    try
      // temporary allocate all dynamic array content
      try
        if (LoadFrom(P, P + PLen) = nil) or
           (ItemLoadFind(item, item + itemLen) < 0) then
          P := nil; // not found
      finally
        Clear; // always release temporary array content
      end;
    except
      on Exception do
      begin
        sqlite3.result_error(Context, 'Invalid BLOB content');
        exit;
      end;
    end
  else
    P := nil;
  sqlite3.result_int64(Context, Int64(P <> nil));
end;

constructor TSqlDataBaseSQLFunctionDynArray.Create(aTypeInfo: PRttiInfo;
  aCompare: TDynArraySortCompare; const aFunctionName: RawUtf8);
begin
  fDummyDynArray.Init(aTypeInfo, fDummyDynArrayValue);
  fDummyDynArray.Compare := aCompare;
  inherited Create(InternalSQLFunctionDynArrayBlob, 2, aFunctionName);
end;


{ TSqlStatementCached }

procedure TSqlStatementCached.Init(aDB: TSqlite3DB);
begin
  Caches.InitSpecific(
    TypeInfo(TSqlStatementCacheDynArray), Cache, ptRawUtf8, @Count);
  DB := aDB;
end;

function TSqlStatementCached.Prepare(const GenericSql: RawUtf8;
  WasPrepared: PBoolean; ExecutionTimer: PPPrecisionTimer;
  ExecutionMonitor: PSynMonitor): PSqlRequest;
var
  added: boolean;
  c: ^TSqlStatementCache;
begin
  if (LastPrepared >= Count) or
     (Cache[LastPrepared].StatementSql <> GenericSql) then
    LastPrepared := Caches.FindHashedForAdding(GenericSql, added)
  else
    added := false; // occurs e.g. on multiple insert
  c := @Cache[LastPrepared];
  if added then
  begin
    c^.StatementSql := GenericSql;
    c^.Statement.Prepare(DB, GenericSql);
    c^.Timer := TSynMonitor.Create;
    if WasPrepared <> nil then
      WasPrepared^ := true;
  end
  else
  begin
    if c^.Timer = nil then
      // there was a Statement.Prepare exception on previous call
      raise ESqlite3Exception.CreateUtf8(
        'TSqlStatementCached.Prepare failed [%]', [GenericSql]);
    if c^.Statement.Request <> 0 then
      c^.Statement.Reset;
    if WasPrepared <> nil then
      WasPrepared^ := false;
  end;
  if ExecutionTimer <> nil then
  begin
    c^.Timer.ProcessStartTask;
    ExecutionTimer^ := @c^.Timer.InternalTimer;
    if ExecutionMonitor <> nil then
      ExecutionMonitor^ := c^.Timer;
  end;
  result := @c^.Statement;
end;

procedure TSqlStatementCached.ReleaseAllDBStatements;
var
  i: PtrInt;
begin
  for i := 0 to Count - 1 do
  begin
    Cache[i].Statement.Close; // close prepared statement
    Cache[i].Timer.Free;
  end;
  Caches.Clear;
  Caches.ReHash; // need to refresh all hashs
end;

function StatementCacheTotalTimeCompare(const A, B): integer;
var
  i64: Int64;
begin
  i64 := TSqlStatementCache(A).Timer.InternalTimer.TimeInMicroSec -
         TSqlStatementCache(B).Timer.InternalTimer.TimeInMicroSec;
  if i64 < 0 then
    result := -1
  else if i64 > 0 then
    result := 1
  else
    result := 0;
end;

procedure TSqlStatementCached.SortCacheByTotalTime(var aIndex: TIntegerDynArray);
begin
  Caches.{$ifdef UNDIRECTDYNARRAY}InternalDynArray.{$endif}
    CreateOrderedIndex(aIndex, StatementCacheTotalTimeCompare);
end;


{ TSqlDatabaseBackupThread }

constructor TSqlDatabaseBackupThread.Create(Backup: TSqlite3Backup;
  Source, Dest: TSqlDatabase; StepPageNumber, StepSleepMS: integer;
  SynLzCompress: boolean; const OnProgress: TOnSqlDatabaseBackup; OwnerDest: boolean);
begin
  fTimer.Start;
  fBackup := Backup;
  fSourceDB := Source;
  fDestDB := Dest;
  fBackupDestFile := Dest.fFileName;
  if StepPageNumber = 0 then
    fStepPageNumber := 1
  else
    fStepPageNumber := StepPageNumber;
  if (cardinal(StepSleepMS) <= 1000) and
     (StepPageNumber > 0) then
    fStepSleepMS := StepSleepMS;
  fOnProgress := OnProgress;
  fStepSynLzCompress := SynLzCompress;
  fOwnerDest := OwnerDest;
  FreeOnTerminate := true;
  inherited Create(false);
end;

procedure TSqlDatabaseBackupThread.Execute;
var
  log: ISynLog;

  procedure NotifyProgressAndContinue(aStep: TOnSqlDatabaseBackupStep);
  begin
    fStep := aStep;
    if Assigned(log) then
      log.Log(sllTrace, '%', [self]);
    if Assigned(fOnProgress) then
      if not fOnProgress(self) then
        raise ESqlite3Exception.CreateUtf8(
          '%.Execute aborted by OnProgress=false', [self]);
  end;

var
  res: integer;
  fn, fn2: TFileName;
begin
  fn := fDestDB.FileName;
  SetCurrentThreadName('% [%] [%]', [self, fSourceDB.FileName, fn]);
  log := SQLite3Log.Enter(self, 'Execute');
  try
    try
      try
        NotifyProgressAndContinue(backupStart);
        repeat
          fSourceDB.Lock; // naive multi-thread protection of main process
          res := sqlite3.backup_step(fBackup, fStepPageNumber);
          fSourceDB.UnLock;
          fStepNumberToFinish := sqlite3.backup_remaining(fBackup);
          fStepNumberTotal := sqlite3.backup_pagecount(fBackup);
          case res of
            SQLITE_OK:
              NotifyProgressAndContinue(backupStepOk);
            SQLITE_BUSY:
              begin
                NotifyProgressAndContinue(backupStepBusy);
                if fStepSleepMS = 0 then
                  SleepHiRes(1);
              end;
            SQLITE_LOCKED:
              NotifyProgressAndContinue(backupStepLocked);
            SQLITE_DONE:
              break;
          else
            raise ESqlite3Exception.Create(fDestDB.DB, res, 'Backup');
          end;
          if Terminated then
            raise ESqlite3Exception.Create('Backup process forced to terminate');
          SleepHiRes(fStepSleepMS);
        until false;
        if fDestDB <> nil then
        begin
          sqlite3.backup_finish(fBackup);
          // close destination backup database
          if fOwnerDest then
            FreeAndNil(fDestDB);
          fDestDB := nil; // no abort below
        end;
        if not IdemPChar(pointer(fn), SQLITE_MEMORY_DATABASE_NAME) then
        begin
          if fStepSynLzCompress then
          begin
            NotifyProgressAndContinue(backupStepSynLz);
            fn2 := ChangeFileExt(fn, '.db.tmp');
            DeleteFile(fn2);
            if not RenameFile(fn, fn2) then
              raise ESqlite3Exception.CreateUtf8(
                '%.Execute: RenameFile(%,%) failed', [self, fn, fn2]);
            if not TSqlDatabase.BackupSynLZ(fn2, fn, true) then
              raise ESqlite3Exception.CreateUtf8(
                '%.Execute: BackupSynLZ(%,%) failed', [self, fn, fn2]);
            if Assigned(log) then
              log.Log(sllTrace, 'TSqlDatabase.BackupSynLZ into % %',
                [KB(FileSize(fn)), fn], self);
          end;
          fSourceDB.fBackupBackgroundLastFileName := ExtractFileName(fn);
        end;
        NotifyProgressAndContinue(backupSuccess);
      finally
        if fDestDB <> nil then
        begin
          if Assigned(log) then
            log.Log(sllWarning, 'Execute Aborted', self);
          sqlite3.backup_finish(fBackup);
          if fOwnerDest then
            // close destination backup database if not already
            fDestDB.Free;
        end;
      end;
    except
      on E: Exception do
      begin
        fError := E;
        fStep := backupFailure;
        if Assigned(fOnProgress) then
          fOnProgress(self);
      end;
    end;
  finally
    fSourceDB.fBackupBackgroundLastTime := fTimer.Stop;
    fSourceDB.Lock;
    fSourceDB.fBackupBackgroundInProcess := nil;
    fSourceDB.Unlock;
    if Assigned(log) then
      log.Log(sllTrace, 'Execute Finished', self);
    log := nil;
    SQLite3Log.Add.NotifyThreadEnded;
  end;
end;

function IsSQLite3File(const FileName: TFileName; PageSize: PInteger): boolean;
var
  F: THandle;
  Header: THash256Rec;
begin
  F := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if not ValidHandle(F) then
    result := false
  else
  begin
    result := (FileRead(F, Header, sizeof(Header)) = SizeOf(Header)) and
              (Header.d0 = SQLITE_FILE_HEADER128.Lo) and
              // don't check header 8..15 (may equal encrypted bytes 16..23)
              (Header.b[21] = 64) and
              (Header.b[22] = 32) and
              (Header.b[23] = 32);
    if result and
       (PageSize <> nil) then
      // header bytes 16..23 are always stored unencrypted
      PageSize^ := integer(Header.b[16]) shl 8 + Header.b[17];
    FileClose(F);
  end;
end;

function IsSQLite3FileEncrypted(const FileName: TFileName): boolean;
var
  F: THandle;
  Header: THash256Rec;
begin
  // see CodecEncrypt/CodecDecrypt in mormot.db.raw.sqlite3.static
  result := false;
  F := FileOpen(FileName, fmOpenRead or fmShareDenyNone);
  if not ValidHandle(F) then
    exit;
  if (FileRead(F, Header, SizeOf(Header)) = SizeOf(Header)) and
     // header bytes 8..15 are encrypted bytes 16..23
     // header bytes 16..23 are stored unencrypted
     (Header.d0 = SQLITE_FILE_HEADER128.Lo) and
     (Header.d1 <> SQLITE_FILE_HEADER128.Hi) and
     (Header.b[21] = 64) and
     (Header.b[22] = 32) and
     (Header.b[23] = 32) then
    result := true;
  FileClose(F);
end;


initialization

finalization
  FreeAndNil(sqlite3); // sqlite3.Free is not reintrant e.g. as .bpl in IDE

end.
