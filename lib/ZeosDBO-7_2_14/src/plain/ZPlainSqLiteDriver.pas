{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for SQLite             }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainSqLiteDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_SQLITE}

uses SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFDEF OLDFPC}ZClasses,{$ENDIF} ZCompatibility, ZPlainDriver;

const
  WINDOWS_DLL_LOCATION = 'sqlite.dll';
  WINDOWS_DLL3_LOCATION = 'sqlite3.dll';
  LINUX_DLL_LOCATION = 'libsqlite'+SharedSuffix;
  LINUX_DLL3_LOCATION = 'libsqlite3'+SharedSuffix;

  SQLITE_ISO8859   = 1;
  MASTER_NAME      = 'sqlite_master';
  TEMP_MASTER_NAME = 'sqlite_temp_master';

  { Return values for sqlite3_exec() and sqlite3_step() }
  SQLITE_OK           = 0;   // Successful result
  SQLITE_ERROR        = 1;   // SQL error or missing database
  SQLITE_INTERNAL     = 2;   // An internal logic error in SQLite
  SQLITE_PERM         = 3;   // Access permission denied
  SQLITE_ABORT        = 4;   // Callback routine requested an abort
  SQLITE_BUSY         = 5;   // The database file is locked
  SQLITE_LOCKED       = 6;   // A table in the database is locked
  SQLITE_NOMEM        = 7;   // A malloc() failed
  SQLITE_READONLY     = 8;   // Attempt to write a readonly database
  SQLITE_INTERRUPT    = 9;   // Operation terminated by sqlite3_interrupt()
  SQLITE_IOERR        = 10;  // Some kind of disk I/O error occurred
  SQLITE_CORRUPT      = 11;  // The database disk image is malformed
  SQLITE_NOTFOUND     = 12;  // (Internal Only) Table or record not found
  SQLITE_FULL         = 13;  // Insertion failed because database is full
  SQLITE_CANTOPEN     = 14;  // Unable to open the database file
  SQLITE_PROTOCOL     = 15;  // Database lock protocol error
  SQLITE_EMPTY        = 16;  // (Internal Only) Database table is empty
  SQLITE_SCHEMA       = 17;  // The database schema changed
  SQLITE_TOOBIG       = 18;  // Too much data for one row of a table
  SQLITE_CONSTRAINT   = 19;  // Abort due to contraint violation
  SQLITE_MISMATCH     = 20;  // Data type mismatch
  SQLITE_MISUSE       = 21;  // Library used incorrectly
  SQLITE_NOLFS        = 22;  // Uses OS features not supported on host
  SQLITE_AUTH         = 23;  // Authorization denied
  SQLITE_FORMAT       = 24;  // Auxiliary database format error
  SQLITE_RANGE        = 25;  // 2nd parameter to sqlite_bind out of range
  SQLITE_NOTADB       = 26;  // File opened that is not a database file
  SQLITE_ROW          = 100;  // sqlite3_step() has another row ready
  SQLITE_DONE         = 101;  // sqlite3_step() has finished executing

  SQLITE_NUMERIC      = -1;
  SQLITE_TEXT         = -2;
  SQLITE_ARGS         = -3;

  {
    The second parameter to the access authorization function above will
    be one of the values below.  These values signify what kind of operation
    is to be authorized.  The 3rd and 4th parameters to the authorization
    function will be parameters or NULL depending on which of the following
    codes is used as the second parameter.  The 5th parameter is the name
    of the database ("main", "temp", etc.) if applicable.  The 6th parameter
    is the name of the inner-most trigger or view that is responsible for
    the access attempt or NULL if this access attempt is directly from
    input SQL code.

                                             Arg-3           Arg-4
  }
  SQLITE_COPY                  = 0;  // Table Name      File Name
  SQLITE_CREATE_INDEX          = 1;  // Index Name      Table Name
  SQLITE_CREATE_TABLE          = 2;  // Table Name      NULL
  SQLITE_CREATE_TEMP_INDEX     = 3;  // Index Name      Table Name
  SQLITE_CREATE_TEMP_TABLE     = 4;  // Table Name      NULL
  SQLITE_CREATE_TEMP_TRIGGER   = 5;  // Trigger Name    Table Name
  SQLITE_CREATE_TEMP_VIEW      = 6;  // View Name       NULL
  SQLITE_CREATE_TRIGGER        = 7;  // Trigger Name    Table Name
  SQLITE_CREATE_VIEW           = 8;  // View Name       NULL
  SQLITE_DELETE                = 9;  // Table Name      NULL
  SQLITE_DROP_INDEX            = 10; // Index Name      Table Name
  SQLITE_DROP_TABLE            = 11; // Table Name      NULL
  SQLITE_DROP_TEMP_INDEX       = 12; // Index Name      Table Name
  SQLITE_DROP_TEMP_TABLE       = 13; // Table Name      NULL
  SQLITE_DROP_TEMP_TRIGGER     = 14; // Trigger Name    Table Name
  SQLITE_DROP_TEMP_VIEW        = 15; // View Name       NULL
  SQLITE_DROP_TRIGGER          = 16; // Trigger Name    Table Name
  SQLITE_DROP_VIEW             = 17; // View Name       NULL
  SQLITE_INSERT                = 18; // Table Name      NULL
  SQLITE_PRAGMA                = 19; // Pragma Name     1st arg or NULL
  SQLITE_READ                  = 20; // Table Name      Column Name
  SQLITE_SELECT                = 21; // NULL            NULL
  SQLITE_TRANSACTION           = 22; // NULL            NULL
  SQLITE_UPDATE                = 23; // Table Name      Column Name
  SQLITE_ATTACH                = 24; // Filename        NULL
  SQLITE_DETACH                = 25; // Database Name   NULL

  { The return value of the authorization function should be one of the
    following constants: }
  SQLITE_DENY    = 1;   // Abort the SQL statement with an error
  SQLITE_IGNORE = 2;   // Don't allow access, but don't generate an error

  SQLITE_INTEGER = 1;
  SQLITE_FLOAT   = 2;
  SQLITE3_TEXT   = 3;
  SQLITE_BLOB    = 4;
  SQLITE_NULL    = 5;

  //flags for sqlite3_open_v2
  SQLITE_OPEN_READONLY        = $00000001; //Ok for sqlite3_open_v2()
  SQLITE_OPEN_READWRITE       = $00000002; //Ok for sqlite3_open_v2()
  SQLITE_OPEN_CREATE          = $00000004; //Ok for sqlite3_open_v2()
  SQLITE_OPEN_DELETEONCLOSE   = $00000008; //VFS only
  SQLITE_OPEN_EXCLUSIVE       = $00000010; //VFS only
  SQLITE_OPEN_AUTOPROXY       = $00000020; //VFS only
  SQLITE_OPEN_URI             = $00000040; //Ok for sqlite3_open_v2()
  SQLITE_OPEN_MEMORY          = $00000080; //Ok for sqlite3_open_v2()
  SQLITE_OPEN_MAIN_DB         = $00000100; //VFS only
  SQLITE_OPEN_TEMP_DB         = $00000200; //VFS only
  SQLITE_OPEN_TRANSIENT_DB    = $00000400; //VFS only
  SQLITE_OPEN_MAIN_JOURNAL    = $00000800; //VFS only
  SQLITE_OPEN_TEMP_JOURNAL    = $00001000; //VFS only
  SQLITE_OPEN_SUBJOURNAL      = $00002000; //VFS only
  SQLITE_OPEN_SUPER_JOURNAL   = $00004000; //VFS only
  SQLITE_OPEN_NOMUTEX         = $00008000; //Ok for sqlite3_open_v2()
  SQLITE_OPEN_FULLMUTEX       = $00010000; //Ok for sqlite3_open_v2()
  SQLITE_OPEN_SHAREDCACHE     = $00020000; //Ok for sqlite3_open_v2()
  SQLITE_OPEN_PRIVATECACHE    = $00040000; //Ok for sqlite3_open_v2()
  SQLITE_OPEN_WAL             = $00080000; //VFS only
  SQLITE_OPEN_NOFOLLOW        = $01000000; //Ok for sqlite3_open_v2()
  Reserved                    = $00F00000;
  //Legacy compatibility
  SQLITE_OPEN_MASTER_JOURNAL  = $00004000; //VFS only
type
  Psqlite = Pointer;
  Psqlite_func = Pointer;
  Psqlite_vm = Pointer;
  Psqlite3_stmt = Pointer;
  Psqlite3_value = Pointer;

  Tsqlite3_destructor_type = procedure(user: pointer); cdecl;


  SQLITE_STATIC = procedure(User: Pointer = Nil); cdecl;
  SQLITE_TRANSIENT = procedure(User: pointer = Pointer(-1)); cdecl;

type
{ ************** Plain API Function types definition ************* }

  Tsqlite_callback = function(p1: Pointer; p2: Integer; var p3: PAnsiChar;
    var p4: PAnsiChar): Integer; cdecl;
  Tsqlite_simple_callback = function(p1: Pointer): Integer; cdecl;
  Tsqlite_busy_callback = function(p1: Pointer; const p2: PAnsiChar;
    p3: Integer): Integer; cdecl;

  Tsqlite_function_callback = procedure(p1: Psqlite_func; p2: Integer;
    const p3: PPAnsiChar); cdecl;
  Tsqlite_finalize_callback = procedure(p1: Psqlite_func); cdecl;
  Tsqlite_auth_callback = function(p1: Pointer; p2: Integer; const p3: PAnsiChar;
    const p4: PAnsiChar; const p5: PAnsiChar; const p6: PAnsiChar): Integer; cdecl;
  Tsqlite_trace_callback = procedure(p1: Pointer; const p2: PAnsiChar); cdecl;

{ ************* Plain API Function variables definition ************ }
type
  {** Represents a generic interface to SQLite native API. }
  IZSQLitePlainDriver = interface (IZPlainDriver)
    ['{B931C952-3076-4ECB-9630-D900E8DB9869}']

    function CompiledWith_SQLITE_ENABLE_COLUMN_METADATA: Boolean;
    function Open(const filename: PAnsiChar; var Handle: Psqlite): Integer;
    function open_v2(const filename: PAnsiChar; var sqlite: Psqlite; flags: integer; zVfs: PAnsiChar): Integer;
    function Close(db: Psqlite): Integer;
    function Execute(db: Psqlite; const sql: PAnsiChar;
      sqlite_callback: Tsqlite_callback; arg: Pointer;
      var errmsg: PAnsiChar): Integer;
    function LastInsertRowId(db: Psqlite): Int64;
    function Changes(db: Psqlite): Integer;
    function LastStatementChanges(db: Psqlite): Integer;
    function ErrorString(db: Psqlite; code: Integer): RawByteString;
    function ErrorMessage(db: Psqlite): RawByteString;
    procedure Interrupt(db: Psqlite);
    function Complete(const sql: PAnsiChar): Integer;
    function Has_sqlite3_column_table_name: Boolean;

    function enable_load_extension(db: Psqlite; OnOff: Integer): Integer;
    function load_extension(db: Psqlite; const zFile: PAnsiChar; zProc: Pointer; var pzErrMsg: PAnsiChar): Integer;

    procedure BusyHandler(db: Psqlite; callback: Tsqlite_busy_callback;
      ptr: Pointer);
    procedure BusyTimeout(db: Psqlite; ms: Integer);

    procedure FreeMem(ptr: Pointer);
    function LibVersion: PAnsiChar;

    function FunctionType(db: Psqlite; const zName: PAnsiChar;
      datatype: Integer): Integer;
    function SetResultText(func: Psqlite_func; const arg: PAnsiChar;
      len: Integer): PAnsiChar;
    procedure SetResultInt(func: Psqlite_func; arg: Integer);
    procedure SetResultDouble(func: Psqlite_func; arg: Double);
    procedure SetResultError(func: Psqlite_func; const arg: PAnsiChar; len: Integer);
    function UserData(func: Psqlite_func): Pointer;
    function AggregateContext(func: Psqlite_func; nBytes: Integer): Pointer;
    function AggregateCount(func: Psqlite_func): Integer;

    function SetAuthorizer(db: Psqlite; callback: Tsqlite_auth_callback;
      pUserData: Pointer): Integer;
    function Trace(db: Psqlite; callback: Tsqlite_trace_callback;
      ptr: Pointer): Pointer;

    { Prepared statmenet api }
    function Prepare(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; var pzTail: PAnsichar): Integer;
    function Prepare_v2(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; var pzTail: PAnsichar): Integer;
    function Prepare16(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; var pzTail: PWideChar): Integer;
    function Prepare16_v2(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; var pzTail: PWideChar): Integer;

    function bind_parameter_count(pStmt: Psqlite3_stmt): Integer;
    function bind_parameter_name(pStmt: Psqlite3_stmt; ParamIndex: Integer): PAnsichar;
    function bind_parameter_index(pStmt: Psqlite3_stmt; const zName: PAnsiChar): Integer;

    function clear_bindings(pStmt: Psqlite3_stmt): Integer;
    function column_count(pStmt: Psqlite3_stmt): Integer;
    function column_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;

    function column_database_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_table_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_origin_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_decltype(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;

    function Step(Stmt: Psqlite3_stmt): Integer;
    function data_count(pStmt: Psqlite3_stmt): Integer;

    function bind_blob(pStmt: Psqlite3_stmt; ParamIndex: Integer;
      const Buffer: Pointer; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_double(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Double): Integer;
    function bind_int(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Integer): Integer;
    function bind_int64(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Int64): Integer;
    function bind_null(pStmt: Psqlite3_stmt; ParamIndex: Integer): Integer;
    function bind_text(pStmt: Psqlite3_stmt; ParamIndex: Integer;
      const Text: PAnsiChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_text16(pStmt: Psqlite3_stmt; ParamIndex: Integer;
      const Text: PWideChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_value(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Value: Psqlite3_value): Integer;
    function bind_zeroblob(pStmt: Psqlite3_stmt; ParamIndex: Integer; N: Integer): Integer;

    function finalize(pStmt: Psqlite3_stmt): Integer;
    function reset(pStmt: Psqlite3_stmt): Integer;

    function column_blob(Stmt: Psqlite3_stmt; iCol:integer): Pointer;
    function column_bytes(Stmt: Psqlite3_stmt; iCol: Integer): integer;
    function column_bytes16(Stmt: Psqlite3_stmt; iCol: Integer): integer;
    function column_double(Stmt: Psqlite3_stmt; iCol: Integer): Double;
    function column_int(Stmt: Psqlite3_stmt; iCol: Integer): Integer;
    function column_int64(Stmt: Psqlite3_stmt; iCol: Integer): Int64;
    function column_text(Stmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_text16(Stmt: Psqlite3_stmt; iCol: Integer): PWideChar;
    function column_type(Stmt: Psqlite3_stmt; iCol: Integer): Integer;
    function column_value(Stmt: Psqlite3_stmt; iCol: Integer): Psqlite3_value;

    procedure ProgressHandler(db: Psqlite; p1: Integer;
      callback: Tsqlite_simple_callback; ptr: Pointer);
    function CommitHook(db: Psqlite; callback: Tsqlite_simple_callback;
      ptr: Pointer): Pointer;

    function ReKey(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
    function Key(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;

    function backup_init(pDest: Psqlite; const zDestName: PAnsiChar; pSource: Psqlite; const zSourceName: PAnsiChar):Pointer;
    function backup_step(p: Psqlite; nPage: Integer): Integer;
    function backup_finish(p: Psqlite): Integer;
    function backup_remaining(p: Psqlite): Integer;
    function backup_pagecount(p: Psqlite): Integer;
  end;

  {** Implements a base driver for SQLite}
  TZSQLiteBaseDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZSQLitePlainDriver)
  private
    sqlite3_open: function(const filename: PAnsiChar;var Qsqlite: Psqlite): Integer; cdecl;
    sqlite3_open_v2: function(const filename: PAnsiChar; var sqlite: Psqlite; flags: integer; zVfs: PAnsiChar): Integer; cdecl;
    sqlite3_close: function(db: Psqlite): Integer; cdecl;

    { prepared statement api }
    sqlite3_prepare: function(
        db: Psqlite;                // Database handle
        const zSql: PAnsiChar;      // SQL statement, UTF-8 encoded
        nBytes: Integer;            // Maximum length of zSql in bytes. -1 = null terminated
        out ppStmt: Psqlite3_stmt;  // OUT: Statement handle
        var pzTail: PAnsichar      // OUT: Pointer to unused portion of zSql
      ): Integer; cdecl;
    sqlite3_prepare_v2: function(
        db: Psqlite;                // Database handle
        const zSql: PAnsiChar;      // SQL statement, UTF-8 encoded
        nBytes: Integer;            // Maximum length of zSql in bytes. -1 = null terminated
        out ppStmt: Psqlite3_stmt;  // OUT: Statement handle
        var pzTail: PAnsichar      // OUT: Pointer to unused portion of zSql
      ): Integer; cdecl;
    sqlite3_prepare16: function(
        db: Psqlite;                // Database handle
        const zSql: PWideChar;      // SQL statement, UTF-16 encoded
        nBytes: Integer;            // Maximum length of zSql in bytes. -1 = null terminated
        out ppStmt: Psqlite3_stmt;  // OUT: Statement handle
        var pzTail: PWideChar      // OUT: Pointer to unused portion of zSql
      ): Integer; cdecl;
    sqlite3_prepare16_v2: function(
        db: Psqlite;                // Database handle
        const zSql: PWideChar;      // SQL statement, UTF-16 encoded
        nBytes: Integer;            // Maximum length of zSql in bytes. -1 = null terminated
        out ppStmt: Psqlite3_stmt;  // OUT: Statement handle
        var pzTail: PWideChar      // OUT: Pointer to unused portion of zSql
      ): Integer; cdecl;

    sqlite3_bind_parameter_count: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_bind_parameter_name: function(pStmt: Psqlite3_stmt; ParamIndex: Integer): PAnsichar; cdecl;
    sqlite3_bind_parameter_index: function(pStmt: Psqlite3_stmt; const zName: PAnsiChar): Integer; cdecl;

    sqlite3_clear_bindings: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_column_count: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_column_name: function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_name16: function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;

    sqlite3_column_database_name: function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_database_name16: function(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;
    sqlite3_column_table_name: function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_table_name16: function(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;
    sqlite3_column_origin_name: function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_origin_name16: function(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;

    sqlite3_column_decltype: function(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_decltype16: function(pStmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;

    sqlite3_step: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_data_count: function (pStmt: Psqlite3_stmt): Integer; cdecl;

    sqlite3_bind_blob: function(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Buffer: Pointer; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer; cdecl;
    sqlite3_bind_double: function(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Double): Integer; cdecl;
    sqlite3_bind_int: function(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Integer): Integer; cdecl;
    sqlite3_bind_int64: function(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Int64): Integer; cdecl;
    sqlite3_bind_null: function(pStmt: Psqlite3_stmt; ParamIndex: Integer): Integer; cdecl;
    sqlite3_bind_text: function(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Text: PAnsiChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer; cdecl;
    sqlite3_bind_text16: function(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Text: PWideChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer; cdecl;
    sqlite3_bind_value: function(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Value: Psqlite3_value): Integer; cdecl;
    sqlite3_bind_zeroblob: function(pStmt: Psqlite3_stmt; ParamIndex: Integer; N: Integer): Integer; cdecl;

    sqlite3_finalize: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_reset: function(pStmt: Psqlite3_stmt): Integer; cdecl;
    sqlite3_enable_load_extension: function(db: Psqlite; OnOff: Integer): Integer; cdecl;
    sqlite3_load_extension: function(db: Psqlite; const zFile: PAnsiChar; zProc: Pointer; var pzErrMsg: PAnsiChar): Integer; cdecl;


    sqlite3_column_blob: function(Stmt: Psqlite3_stmt; iCol:integer): Pointer; cdecl;
    sqlite3_column_bytes: function(Stmt: Psqlite3_stmt; iCol: Integer): integer; cdecl;
    sqlite3_column_bytes16: function(Stmt: Psqlite3_stmt; iCol: Integer): integer; cdecl;
    sqlite3_column_double: function(Stmt: Psqlite3_stmt; iCol: Integer): Double; cdecl;
    sqlite3_column_int: function(Stmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
    sqlite3_column_int64: function(Stmt: Psqlite3_stmt; iCol: Integer): Int64; cdecl;
    sqlite3_column_text: function(Stmt: Psqlite3_stmt; iCol: Integer): PAnsiChar; cdecl;
    sqlite3_column_text16: function(Stmt: Psqlite3_stmt; iCol: Integer): PWideChar; cdecl;
    sqlite3_column_type: function(Stmt: Psqlite3_stmt; iCol: Integer): Integer; cdecl;
    sqlite3_column_value: function(Stmt: Psqlite3_stmt; iCol: Integer): Psqlite3_value; cdecl;

    sqlite3_exec: function(db: Psqlite; const sql: PAnsiChar; sqlite_callback: Tsqlite_callback;
      arg: Pointer; var errmsg: PAnsiChar): Integer; cdecl;

    sqlite3_errmsg: function(db: Psqlite): PAnsiChar; cdecl;
    sqlite3_errstr: function(code: Integer): PAnsiChar; cdecl;
    sqlite3_extended_errcode: function(db: Psqlite): integer; cdecl;

    sqlite3_last_insert_rowid: function(db: Psqlite): Int64; cdecl;
    sqlite3_changes: function(db: Psqlite): Integer; cdecl;
    sqlite3_interrupt: procedure(db: Psqlite); cdecl;
    sqlite3_complete: function(const sql: PAnsiChar): Integer; cdecl;
    sqlite3_busy_handler: procedure(db: Psqlite; callback: Tsqlite_busy_callback; ptr: Pointer); cdecl;
    sqlite3_busy_timeout: procedure(db: Psqlite; ms: Integer); cdecl;
    sqlite3_free: procedure(ptr: Pointer); cdecl;
    sqlite3_libversion: function: PAnsiChar; cdecl;
    sqlite3_function_type: function(db: Psqlite; const zName: PAnsiChar; datatype: Integer): Integer; cdecl;
    sqlite3_result_text: function(func: Psqlite_func; const arg: PAnsiChar;
      len: Integer; UN: Tsqlite_simple_callback): PAnsiChar; cdecl;
    sqlite3_result_int: procedure(func: Psqlite_func; arg: Integer); cdecl;
    sqlite3_result_double: procedure(func: Psqlite_func; arg: Double); cdecl;
    sqlite3_result_error: procedure(func: Psqlite_func; const arg: PAnsiChar; len: Integer); cdecl;
    sqlite3_user_data: function(func: Psqlite_func): Pointer; cdecl;
    sqlite3_aggregate_context: function(func: Psqlite_func; nBytes: Integer): Pointer; cdecl;
    sqlite3_aggregate_count: function(func: Psqlite_func): Integer; cdecl;
    sqlite3_set_authorizer: function(db: Psqlite; callback: Tsqlite_auth_callback; pUserData: Pointer): Integer; cdecl;
    sqlite3_trace: function(db: Psqlite; callback: Tsqlite_trace_callback; ptr: Pointer): Pointer; cdecl;
    sqlite3_progress_handler: procedure(db: Psqlite; p1: Integer; callback: Tsqlite_simple_callback; ptr: Pointer); cdecl;
    sqlite3_commit_hook: function(db: Psqlite; callback: Tsqlite_simple_callback; ptr: Pointer): Pointer; cdecl;
    sqlite3_rekey: function(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer; cdecl;
    sqlite3_key: function(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer; cdecl;

    sqlite3_backup_init: function(pDest: Psqlite; const zDestName: PAnsiChar; pSource: Psqlite; const zSourceName: PAnsiChar):Pointer; cdecl;
    sqlite3_backup_step: function(p: Psqlite; nPage: Integer): Integer; cdecl;
    sqlite3_backup_finish: function(p: Psqlite): Integer; cdecl;
    sqlite3_backup_remaining: function(p: Psqlite): Integer; cdecl;
    sqlite3_backup_pagecount: function(p: Psqlite): Integer; cdecl;
  protected
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
  public
    function CompiledWith_SQLITE_ENABLE_COLUMN_METADATA: Boolean;
    function Open(const filename: PAnsiChar; var Handle: Psqlite): Integer;
    function open_v2(const filename: PAnsiChar; var sqlite: Psqlite; flags: integer; zVfs: PAnsiChar): Integer;
    function Close(db: Psqlite): Integer;
    function Execute(db: Psqlite; const sql: PAnsiChar;
      sqlite_callback: Tsqlite_callback; arg: Pointer;
      var errmsg: PAnsiChar): Integer;
    function LastInsertRowId(db: Psqlite): Int64;
    function Changes(db: Psqlite): Integer;
    function LastStatementChanges({%H-}db: Psqlite): Integer;
    function ErrorString(db: Psqlite; code: Integer): RawByteString;
    function ErrorMessage(db: Psqlite): RawByteString;
    procedure Interrupt(db: Psqlite);
    function Complete(const sql: PAnsiChar): Integer;

    procedure BusyHandler(db: Psqlite; callback: Tsqlite_busy_callback;
      ptr: Pointer);
    procedure BusyTimeout(db: Psqlite; ms: Integer);

    procedure FreeMem(ptr: Pointer);
    function LibVersion: PAnsiChar;
    function Has_sqlite3_column_table_name: Boolean;

    function enable_load_extension(db: Psqlite; OnOff: Integer): Integer;
    function load_extension(db: Psqlite; const zFile: PAnsiChar; zProc: Pointer; var pzErrMsg: PAnsiChar): Integer;

    function FunctionType({%H-}db: Psqlite; const {%H-}zName: PAnsiChar;
      {%H-}datatype: Integer): Integer;
    function SetResultText(func: Psqlite_func; const arg: PAnsiChar;
      len: Integer): PAnsiChar;
    procedure SetResultInt(func: Psqlite_func; arg: Integer);
    procedure SetResultDouble(func: Psqlite_func; arg: Double);
    procedure SetResultError(func: Psqlite_func; const arg: PAnsiChar; len: Integer);
    function UserData(func: Psqlite_func): Pointer;
    function AggregateContext(func: Psqlite_func; nBytes: Integer): Pointer;
    function AggregateCount(func: Psqlite_func): Integer;

    function SetAuthorizer(db: Psqlite; callback: Tsqlite_auth_callback;
      pUserData: Pointer): Integer;
    function Trace(db: Psqlite; callback: Tsqlite_trace_callback;
      ptr: Pointer): Pointer;

    { Prepared statmenet api }
    function Prepare(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; var pzTail: PAnsichar): Integer;
    function Prepare_v2(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; var pzTail: PAnsichar): Integer;
    function Prepare16(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; var pzTail: PWideChar): Integer;
    function Prepare16_v2(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
      out ppStmt: Psqlite3_stmt; var pzTail: PWideChar): Integer;

    function bind_parameter_count(pStmt: Psqlite3_stmt): Integer;
    function bind_parameter_name(pStmt: Psqlite3_stmt; ParamIndex: Integer): PAnsichar;
    function bind_parameter_index(pStmt: Psqlite3_stmt; const zName: PAnsiChar): Integer;

    function clear_bindings(pStmt: Psqlite3_stmt): Integer;
    function column_count(pStmt: Psqlite3_stmt): Integer;
    function column_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;

    function column_database_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_table_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_origin_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_decltype(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;

    function Step(Stmt: Psqlite3_stmt): Integer;
    function data_count(pStmt: Psqlite3_stmt): Integer;

    function bind_blob(pStmt: Psqlite3_stmt; ParamIndex: Integer;
      const Buffer: Pointer; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_double(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Double): Integer;
    function bind_int(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Integer): Integer;
    function bind_int64(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Int64): Integer;
    function bind_null(pStmt: Psqlite3_stmt; ParamIndex: Integer): Integer;
    function bind_text(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Text: PAnsiChar; N: Integer;
      ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_text16(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Text: PWideChar; N: Integer;
      ValDestructor: Tsqlite3_destructor_type): Integer;
    function bind_value(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Value: Psqlite3_value): Integer;
    function bind_zeroblob(pStmt: Psqlite3_stmt; ParamIndex: Integer; N: Integer): Integer;

    function finalize(pStmt: Psqlite3_stmt): Integer;
    function reset(pStmt: Psqlite3_stmt): Integer;

    function column_blob(Stmt: Psqlite3_stmt; iCol: integer): Pointer;
    function column_bytes(Stmt: Psqlite3_stmt; iCol: Integer): integer;
    function column_bytes16(Stmt: Psqlite3_stmt; iCol: Integer): integer;
    function column_double(Stmt: Psqlite3_stmt; iCol: Integer): Double;
    function column_int(Stmt: Psqlite3_stmt; iCol: Integer): Integer;
    function column_int64(Stmt: Psqlite3_stmt; iCol: Integer): Int64;
    function column_text(Stmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
    function column_text16(Stmt: Psqlite3_stmt; iCol: Integer): PWideChar;
    function column_type(Stmt: Psqlite3_stmt; iCol: Integer): Integer;
    function column_value(Stmt: Psqlite3_stmt; iCol: Integer): Psqlite3_value;

    function backup_init(pDest: Psqlite; const zDestName: PAnsiChar; pSource: Psqlite; const zSourceName: PAnsiChar):Pointer;
    function backup_step(p: Psqlite; nPage: Integer): Integer;
    function backup_finish(p: Psqlite): Integer;
    function backup_remaining(p: Psqlite): Integer;
    function backup_pagecount(p: Psqlite): Integer;

    procedure ProgressHandler(db: Psqlite; p1: Integer;
      callback: Tsqlite_simple_callback; ptr: Pointer);
    function CommitHook(db: Psqlite; callback: Tsqlite_simple_callback;
      ptr: Pointer): Pointer;

    function ReKey(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
    function Key(db: Psqlite; const pKey: Pointer; nKey: Integer): Integer;
  public
    constructor Create;
  end;

  {** Implements a driver for SQLite 3 }
  TZSQLite3PlainDriver = class (TZSQLiteBaseDriver, IZPlainDriver, IZSQLitePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadApi; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

{$ENDIF ZEOS_DISABLE_SQLITE}

implementation

{$IFNDEF ZEOS_DISABLE_SQLITE}

uses ZPlainLoader, ZEncoding, ZClasses, ZMessages, ZFastCode, ZSysUtils;

{ TZSQLiteBaseDriver }

function TZSQLiteBaseDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF-8'
end;

function TZSQLiteBaseDriver.Has_sqlite3_column_table_name: Boolean;
begin
  Result := Assigned(sqlite3_column_table_name);
end;

procedure TZSQLiteBaseDriver.LoadCodePages;  //Egonhugeist
begin
  { MultiByte }
  AddCodePage('UTF-8', 1, ceUTF8, zCP_UTF8, '', 4);
  AddCodePage('UTF-16le', 2, ceUTF16, zCP_UTF16, 'UTF-8'); //Setting this will be ignored by actual Excute of Plaindriver
  AddCodePage('UTF-16be', 3, ceUTF16, zCP_UTF16BE, 'UTF-8'); //Setting this will be ignored by actual Excute of Plaindriver
  AddCodePage('UTF-16', 4, ceUTF16, zCP_UTF16, 'UTF-8'); //Setting this will be ignored by actual Excute of Plaindriver
end;

function TZSQLiteBaseDriver.load_extension(db: Psqlite; const zFile: PAnsiChar;
  zProc: Pointer; var pzErrMsg: PAnsiChar): Integer;
begin
  Result := sqlite3_load_extension(db, zFile, zProc, pzErrMsg);
end;

constructor TZSQLiteBaseDriver.Create;
begin
   inherited create;
   FLoader := TZNativeLibraryLoader.Create([]);
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
    FLoader.AddLocation(LINUX_DLL_LOCATION+'.0');
  {$ENDIF}
end;

function TZSQLiteBaseDriver.AggregateContext(func: Psqlite_func;
  nBytes: Integer): Pointer;
begin
  Result := sqlite3_aggregate_context(func, nBytes);
end;

function TZSQLiteBaseDriver.AggregateCount(func: Psqlite_func): Integer;
begin
  Result := sqlite3_aggregate_count(func);
end;

procedure TZSQLiteBaseDriver.BusyHandler(db: Psqlite;
  callback: Tsqlite_busy_callback; ptr: Pointer);
begin
  sqlite3_busy_handler(db, callback, ptr);
end;

procedure TZSQLiteBaseDriver.BusyTimeout(db: Psqlite; ms: Integer);
begin
  sqlite3_busy_timeout(db, ms);
end;

function TZSQLiteBaseDriver.Changes(db: Psqlite): Integer;
begin
  Result := sqlite3_changes(db);
end;

function TZSQLiteBaseDriver.CommitHook(db: Psqlite;
  callback: Tsqlite_simple_callback; ptr: Pointer): Pointer;
begin
  Result := sqlite3_commit_hook(db, callback, ptr);
end;

function TZSQLiteBaseDriver.CompiledWith_SQLITE_ENABLE_COLUMN_METADATA: Boolean;
begin
  Result := Assigned(sqlite3_column_name) and Assigned(sqlite3_column_table_name);
end;

function TZSQLiteBaseDriver.Complete(const sql: PAnsiChar): Integer;
begin
  Result := sqlite3_complete(sql);
end;

function TZSQLiteBaseDriver.ErrorMessage(db: Psqlite): RawByteString;
var P: PAnsiChar;
    L: LengthInt;
begin
  if Assigned(sqlite3_errmsg) and Assigned(db) then begin
    P := sqlite3_errmsg(db);
    if P = nil
    then L := 0
    else L := ZFastCode.StrLen(P);
    Result := ZSysUtils.Trim(P,L);
  end else Result := '';
end;

function TZSQLiteBaseDriver.ErrorString(db: Psqlite; code: Integer): RawByteString;
var P: PAnsiChar;
    L: LengthInt;
begin
  if code = SQLITE_OK then
  begin
    Result := 'not an error';
    Exit;
  end;

  if code = SQLITE_NOMEM then
  begin
    Result := 'out of memory';
    Exit;
  end;

  if ( db = nil ) or ( @sqlite3_errstr = nil ) then
    case code of
      SQLITE_OK:         Result := 'not an error';
      SQLITE_ERROR:      Result := 'SQL logic error or missing database';
      SQLITE_INTERNAL:   Result := 'internal SQLite implementation flaw';
      SQLITE_PERM:       Result := 'access permission denied';
      SQLITE_ABORT:      Result := 'callback requested query abort';
      SQLITE_BUSY:       Result := 'database is locked';
      SQLITE_LOCKED:     Result := 'database table is locked';
      SQLITE_NOMEM:      Result := 'out of memory';
      SQLITE_READONLY:   Result := 'attempt to write a readonly database';
      SQLITE_INTERRUPT:  Result := 'interrupted';
      SQLITE_IOERR:      Result := 'disk I/O error';
      SQLITE_CORRUPT:    Result := 'database disk image is malformed';
      SQLITE_NOTFOUND:   Result := 'table or record not found';
      SQLITE_FULL:       Result := 'database is full';
      SQLITE_CANTOPEN:   Result := 'unable to open database file';
      SQLITE_PROTOCOL:   Result := 'database locking protocol failure';
      SQLITE_EMPTY:      Result := 'table contains no data';
      SQLITE_SCHEMA:     Result := 'database schema has changed';
      SQLITE_TOOBIG:     Result := 'too much data for one table row';
      SQLITE_CONSTRAINT: Result := 'constraint failed';
      SQLITE_MISMATCH:   Result := 'datatype mismatch';
      SQLITE_MISUSE:     Result := 'library routine called out of sequence';
      SQLITE_NOLFS:      Result := 'kernel lacks large file support';
      SQLITE_AUTH:       Result := 'authorization denied';
      SQLITE_FORMAT:     Result := 'auxiliary database format error';
      SQLITE_RANGE:      Result := 'bind index out of range';
      SQLITE_NOTADB:     Result := 'file is encrypted or is not a database';
    else
      Result := 'unknown error';
    end
  else begin
    P := sqlite3_errstr(code);
    if P = nil
    then L := 0
    else L := ZFastCode.StrLen(P);
    Result := ZSysUtils.Trim(P,L);
  end;
end;

function TZSQLiteBaseDriver.Execute(db: Psqlite; const sql: PAnsiChar;
  sqlite_callback: Tsqlite_callback; arg: Pointer;
  var errmsg: PAnsiChar): Integer;
begin
  errmsg:= nil;
  Result := sqlite3_exec(db, sql, sqlite_callback, arg, errmsg);
end;

procedure TZSQLiteBaseDriver.FreeMem(ptr: Pointer);
begin
  sqlite3_free(ptr);
end;

function TZSQLiteBaseDriver.FunctionType(db: Psqlite;
  const zName: PAnsiChar; datatype: Integer): Integer;
begin
  Result := SQLITE_MISUSE;
end;

procedure TZSQLiteBaseDriver.Interrupt(db: Psqlite);
begin
  sqlite3_interrupt(db);
end;

function TZSQLiteBaseDriver.LastInsertRowId(db: Psqlite): Int64;
begin
  Result := sqlite3_last_insert_rowid(db);
end;

function TZSQLiteBaseDriver.LastStatementChanges(db: Psqlite): Integer;
begin
  Result := SQLITE_MISUSE;
end;

function TZSQLiteBaseDriver.LibVersion: PAnsiChar;
begin
  Result := sqlite3_libversion;
end;

function TZSQLiteBaseDriver.Open(const filename: PAnsiChar; var Handle: Psqlite): Integer;
{$IFNDEF UNICODE}
var
  Version: string;
  FileNameString: String;
{$ENDIF}
begin
  Handle := nil;
  (*Note to Windows users: The encoding used for the filename argument of
    sqlite3_open() and sqlite3_open_v2() must be UTF-8, not whatever codepage
    is currently defined. Filenames containing international characters must
    be converted to UTF-8 prior to passing them into sqlite3_open() or
    sqlite3_open_v2(). *)

{$IFDEF UNICODE}
  Result := sqlite3_open(filename, Handle);
{$ELSE}
  Version := LibVersion;
  FileNameString := filename;
  if (Version > '3.2.5') then
    {$IFDEF FPC}
      Result := sqlite3_open(PAnsiChar(FileNameString), Handle)
    {$ELSE}
      Result := sqlite3_open(PAnsiChar(AnsiToUTF8(FileNameString)), Handle)
    {$ENDIF}
  else
    Result := sqlite3_open(filename, Handle);
{$ENDIF}
end;

function TZSQLiteBaseDriver.open_v2(const filename: PAnsiChar;
  var sqlite: Psqlite; flags: integer; zVfs: PAnsiChar): Integer;
begin
  if Assigned(sqlite3_open_v2) and ((flags <> 0) or (zVfs <> nil))
  then Result := sqlite3_open_v2(filename, sqlite, flags, zVfs)
  else Result := sqlite3_open(filename, sqlite);
end;

procedure TZSQLiteBaseDriver.ProgressHandler(db: Psqlite; p1: Integer;
  callback: Tsqlite_simple_callback; ptr: Pointer);
begin
  sqlite3_progress_handler(db, p1, callback, ptr);
end;

function TZSQLiteBaseDriver.ReKey(db: Psqlite; const pKey: Pointer;
  nKey: Integer): Integer;
begin
  if @sqlite3_rekey = nil then
  begin
    Result := SQLITE_OK;
  end
  else
  begin
    Result := sqlite3_rekey(db, pKey, nKey);
  end;
end;

function TZSQLiteBaseDriver.Key(db: Psqlite; const pKey: Pointer;
  nKey: Integer): Integer;
begin
  if @sqlite3_key = nil then
  begin
    Result := SQLITE_OK;
  end
  else
  begin
    Result := sqlite3_key(db, pKey, nKey);
  end;
end;

function TZSQLiteBaseDriver.SetAuthorizer(db: Psqlite;
  callback: Tsqlite_auth_callback; pUserData: Pointer): Integer;
begin
  Result := sqlite3_set_authorizer(db, callback, pUserData);
end;

procedure TZSQLiteBaseDriver.SetResultDouble(func: Psqlite_func;
  arg: Double);
begin
  sqlite3_result_double(func, arg);
end;

procedure TZSQLiteBaseDriver.SetResultError(func: Psqlite_func;
  const arg: PAnsiChar; len: Integer);
begin
  sqlite3_result_error(func, arg, len);
end;

procedure TZSQLiteBaseDriver.SetResultInt(func: Psqlite_func;
  arg: Integer);
begin
  sqlite3_result_int(func, arg);
end;

function TZSQLiteBaseDriver.SetResultText(func: Psqlite_func;
  const arg: PAnsiChar; len: Integer): PAnsiChar;
begin
  Result := sqlite3_result_text(func, arg, len, nil);
end;

{ Prepared statmenet api }
function TZSQLiteBaseDriver.Prepare(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
  out ppStmt: Psqlite3_stmt; var pzTail: PAnsichar): Integer;
begin
  Result := sqlite3_prepare(db, zSql, nBytes, ppStmt, pzTail);
end;

function TZSQLiteBaseDriver.Prepare_v2(db: Psqlite; const zSql: PAnsiChar; nBytes: Integer;
  out ppStmt: Psqlite3_stmt; var pzTail: PAnsichar): Integer;
begin
  Result := sqlite3_prepare_v2(db, zSql, nBytes, ppStmt, pzTail);
end;

function TZSQLiteBaseDriver.Prepare16(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
  out ppStmt: Psqlite3_stmt; var pzTail: PWideChar): Integer;
begin
  Result := sqlite3_prepare16(db, zSql, nBytes, ppStmt, pzTail);
end;

function TZSQLiteBaseDriver.Prepare16_v2(db: Psqlite; const zSql: PWideChar; nBytes: Integer;
  out ppStmt: Psqlite3_stmt; var pzTail: PWideChar): Integer;
begin
  Result := sqlite3_prepare16_v2(db, zSql, nBytes, ppStmt, pzTail);
end;

function TZSQLiteBaseDriver.bind_parameter_count(pStmt: Psqlite3_stmt): Integer;
begin
  Result := sqlite3_bind_parameter_count(pStmt);
end;

function TZSQLiteBaseDriver.bind_parameter_name(pStmt: Psqlite3_stmt; ParamIndex: Integer): PAnsichar;
begin
  Result := sqlite3_bind_parameter_name(pStmt, ParamIndex);
end;

function TZSQLiteBaseDriver.bind_parameter_index(pStmt: Psqlite3_stmt; const zName: PAnsiChar): Integer;
begin
  Result := sqlite3_bind_parameter_index(pStmt, ZName);
end;

function TZSQLiteBaseDriver.clear_bindings(pStmt: Psqlite3_stmt): Integer;
begin
  Result := sqlite3_clear_bindings(pStmt);
end;

function TZSQLiteBaseDriver.column_count(pStmt: Psqlite3_stmt): Integer;
begin
  Result := sqlite3_column_count(pStmt);
end;

function TZSQLiteBaseDriver.column_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  Result := sqlite3_column_name(pStmt, iCol);
end;

function TZSQLiteBaseDriver.column_database_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  if Assigned(sqlite3_column_database_name) then
    Result := sqlite3_column_database_name(pStmt, iCol)
  else
    Result := nil;
end;

function TZSQLiteBaseDriver.column_table_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  if Assigned(sqlite3_column_table_name) then
    Result := sqlite3_column_table_name(pStmt, iCol)
  else
    Result := nil;
end;

function TZSQLiteBaseDriver.column_origin_name(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  if Assigned(sqlite3_column_origin_name) then
    Result := sqlite3_column_origin_name(pStmt, iCol)
  else
    Result := nil;
end;

function TZSQLiteBaseDriver.column_decltype(pStmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  Result := sqlite3_column_decltype(pStmt, iCol);
end;

function TZSQLiteBaseDriver.Step(Stmt: Psqlite3_stmt): Integer;
begin
  Result := sqlite3_step(Stmt);
end;

function TZSQLiteBaseDriver.data_count(pStmt: Psqlite3_stmt): Integer;
begin
  Result := sqlite3_data_count(pStmt);
end;

function TZSQLiteBaseDriver.enable_load_extension(db: Psqlite;
  OnOff: Integer): Integer;
begin
  if not Assigned(sqlite3_enable_load_extension) then
    raise EZSQLException.Create(SUnsupportedOperation);

  Result := sqlite3_enable_load_extension(db, OnOff);
end;

function TZSQLiteBaseDriver.backup_finish(p: Psqlite): Integer;
begin
  Result := sqlite3_backup_finish(p);
end;

function TZSQLiteBaseDriver.backup_init(pDest: Psqlite;
  const zDestName: PAnsiChar; pSource: Psqlite;
  const zSourceName: PAnsiChar): Pointer;
begin
  Result := sqlite3_backup_init(pDest, zDestName, pSource, zSourceName);
end;

function TZSQLiteBaseDriver.backup_pagecount(p: Psqlite): Integer;
begin
  Result := sqlite3_backup_pagecount(p);
end;

function TZSQLiteBaseDriver.backup_remaining(p: Psqlite): Integer;
begin
  Result := sqlite3_backup_remaining(p);
end;

function TZSQLiteBaseDriver.backup_step(p: Psqlite; nPage: Integer): Integer;
begin
  Result := sqlite3_backup_step(p, nPage);
end;

function TZSQLiteBaseDriver.bind_blob(pStmt: Psqlite3_stmt; ParamIndex: Integer;
  const Buffer: Pointer; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
begin
  Result := sqlite3_bind_blob(pStmt, ParamIndex, Buffer, N, ValDestructor);
end;

function TZSQLiteBaseDriver.bind_double(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Double): Integer;
begin
  Result := sqlite3_bind_double(pStmt, ParamIndex, Value);
end;

function TZSQLiteBaseDriver.bind_int(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Integer): Integer;
begin
  Result := sqlite3_bind_int(pStmt, ParamIndex, Value);
end;

function TZSQLiteBaseDriver.bind_int64(pStmt: Psqlite3_stmt; ParamIndex: Integer; Value: Int64): Integer;
begin
  Result := sqlite3_bind_int64(pStmt, ParamIndex, Value);
end;

function TZSQLiteBaseDriver.bind_null(pStmt: Psqlite3_stmt; ParamIndex: Integer): Integer;
begin
  Result := sqlite3_bind_null(pStmt, ParamIndex);
end;

function TZSQLiteBaseDriver.bind_text(pStmt: Psqlite3_stmt; ParamIndex: Integer;
  const Text: PAnsiChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
begin
  Result := sqlite3_bind_text(pStmt, ParamIndex, Text, N, ValDestructor);
end;

function TZSQLiteBaseDriver.bind_text16(pStmt: Psqlite3_stmt; ParamIndex: Integer;
  const Text: PWideChar; N: Integer; ValDestructor: Tsqlite3_destructor_type): Integer;
begin
  Result := sqlite3_bind_text16(pStmt, ParamIndex, Text, N, ValDestructor);
end;

function TZSQLiteBaseDriver.bind_value(pStmt: Psqlite3_stmt; ParamIndex: Integer; const Value: Psqlite3_value): Integer;
begin
  Result := sqlite3_bind_value(pStmt, ParamIndex, Value);
end;

function TZSQLiteBaseDriver.bind_zeroblob(pStmt: Psqlite3_stmt; ParamIndex: Integer; N: Integer): Integer;
begin
  Result := sqlite3_bind_zeroblob(pStmt, ParamIndex, N);
end;

function TZSQLiteBaseDriver.finalize(pStmt: Psqlite3_stmt): Integer;
begin
  Result := sqlite3_finalize(pStmt);
end;

function TZSQLiteBaseDriver.reset(pStmt: Psqlite3_stmt): Integer;
begin
  Result := sqlite3_reset(pStmt);
end;

function TZSQLiteBaseDriver.column_blob(Stmt: Psqlite3_stmt; iCol:integer): Pointer;
begin
  Result := sqlite3_column_blob(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_bytes(Stmt: Psqlite3_stmt; iCol: Integer): integer;
begin
  Result := sqlite3_column_bytes(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_bytes16(Stmt: Psqlite3_stmt; iCol: Integer): integer;
begin
  Result := sqlite3_column_bytes16(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_double(Stmt: Psqlite3_stmt; iCol: Integer): Double;
begin
  Result := sqlite3_column_double(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_int(Stmt: Psqlite3_stmt; iCol: Integer): Integer;
begin
  Result := sqlite3_column_int(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_int64(Stmt: Psqlite3_stmt; iCol: Integer): Int64;
begin
  Result := sqlite3_column_int64(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_text(Stmt: Psqlite3_stmt; iCol: Integer): PAnsiChar;
begin
  Result := sqlite3_column_text(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_text16(Stmt: Psqlite3_stmt; iCol: Integer): PWideChar;
begin
  Result := sqlite3_column_text16(Stmt, iCol);
end;

function TZSQLiteBaseDriver.column_type(Stmt: Psqlite3_stmt; iCol: Integer): Integer;
begin
  Result := sqlite3_column_type(stmt, iCol);
end;

function TZSQLiteBaseDriver.column_value(Stmt: Psqlite3_stmt; iCol: Integer): Psqlite3_value;
begin
  Result := sqlite3_column_value(stmt, iCol);
end;

function TZSQLiteBaseDriver.Trace(db: Psqlite;
  callback: Tsqlite_trace_callback; ptr: Pointer): Pointer;
begin
  Result := sqlite3_trace(db, callback, ptr);
end;

function TZSQLiteBaseDriver.UserData(func: Psqlite_func): Pointer;
begin
  Result := sqlite3_user_data(func);
end;

function TZSQLiteBaseDriver.Close(db: Psqlite): Integer;
begin
  Result := sqlite3_close(db);
end;

{ TZSQLite3PlainDriver }

function TZSQLite3PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZSQLite3PlainDriver.Create;
end;

procedure TZSQLite3PlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do begin
  @sqlite3_open                   := GetAddress('sqlite3_open');
  @sqlite3_open_v2                := GetAddress('sqlite3_open_v2');
  @sqlite3_close                  := GetAddress('sqlite3_close');

  { prepared Statment api }
  @sqlite3_prepare                := GetAddress('sqlite3_prepare');
  @sqlite3_prepare_v2             := GetAddress('sqlite3_prepare_v2');
  @sqlite3_prepare16              := GetAddress('sqlite3_prepare16');
  @sqlite3_prepare16_v2           := GetAddress('sqlite3_prepare16_v2');

  @sqlite3_bind_parameter_count   := GetAddress('sqlite3_bind_parameter_count');
  @sqlite3_bind_parameter_name    := GetAddress('sqlite3_bind_parameter_name');
  @sqlite3_bind_parameter_index   := GetAddress('sqlite3_bind_parameter_index');

  @sqlite3_clear_bindings         := GetAddress('sqlite3_clear_bindings');

  @sqlite3_column_count           := GetAddress('sqlite3_column_count');
  @sqlite3_column_bytes           := GetAddress('sqlite3_column_bytes');
  @sqlite3_column_bytes16         := GetAddress('sqlite3_column_bytes16');
  @sqlite3_column_blob            := GetAddress('sqlite3_column_blob');
  @sqlite3_column_double          := GetAddress('sqlite3_column_double');
  @sqlite3_column_int             := GetAddress('sqlite3_column_int');
  @sqlite3_column_int64           := GetAddress('sqlite3_column_int64');
  @sqlite3_column_text            := GetAddress('sqlite3_column_text');
  @sqlite3_column_text16          := GetAddress('sqlite3_column_text16');
  @sqlite3_column_type            := GetAddress('sqlite3_column_type');
  @sqlite3_column_value           := GetAddress('sqlite3_column_value');
  @sqlite3_column_name            := GetAddress('sqlite3_column_name');
  @sqlite3_column_name16          := GetAddress('sqlite3_column_name16');
  @sqlite3_column_database_name   := GetAddress('sqlite3_column_database_name');
  @sqlite3_column_database_name16 := GetAddress('sqlite3_column_database_name16');
  @sqlite3_column_table_name      := GetAddress('sqlite3_column_table_name');
  @sqlite3_column_table_name16    := GetAddress('sqlite3_column_table_name16');
  @sqlite3_column_origin_name     := GetAddress('sqlite3_column_origin_name');
  @sqlite3_column_origin_name16   := GetAddress('sqlite3_column_origin_name16');
  @sqlite3_column_decltype        := GetAddress('sqlite3_column_decltype');
  @sqlite3_column_decltype16      := GetAddress('sqlite3_column_decltype16');

  @sqlite3_step                   := GetAddress('sqlite3_step');
  @sqlite3_data_count             := GetAddress('sqlite3_data_count');

  @sqlite3_bind_blob              := GetAddress('sqlite3_bind_blob');
  @sqlite3_bind_double            := GetAddress('sqlite3_bind_double');
  @sqlite3_bind_int               := GetAddress('sqlite3_bind_int');
  @sqlite3_bind_int64             := GetAddress('sqlite3_bind_int64');
  @sqlite3_bind_null              := GetAddress('sqlite3_bind_null');
  @sqlite3_bind_text              := GetAddress('sqlite3_bind_text');
  @sqlite3_bind_text16            := GetAddress('sqlite3_bind_text16');
  @sqlite3_bind_value             := GetAddress('sqlite3_bind_value');
  @sqlite3_bind_zeroblob          := GetAddress('sqlite3_bind_zeroblob');

  @sqlite3_finalize               := GetAddress('sqlite3_finalize');
  @sqlite3_reset                  := GetAddress('sqlite3_reset');
  @sqlite3_enable_load_extension  := GetAddress('sqlite3_enable_load_extension');
  @sqlite3_load_extension         := GetAddress('sqlite3_load_extension');

  @sqlite3_exec                   := GetAddress('sqlite3_exec');
  @sqlite3_last_insert_rowid      := GetAddress('sqlite3_last_insert_rowid');
  @sqlite3_changes                := GetAddress('sqlite3_changes');

  @sqlite3_errmsg                 := GetAddress('sqlite3_errmsg');
  @sqlite3_errstr                 := GetAddress('sqlite3_errstr');
  @sqlite3_extended_errcode       := GetAddress('sqlite3_extended_errcode');

  @sqlite3_interrupt              := GetAddress('sqlite3_interrupt');
  @sqlite3_complete               := GetAddress('sqlite3_complete');
  @sqlite3_busy_handler           := GetAddress('sqlite3_busy_handler');
  @sqlite3_busy_timeout           := GetAddress('sqlite3_busy_timeout');
  @sqlite3_free                   := GetAddress('sqlite3_free');
  @sqlite3_libversion             := GetAddress('sqlite3_libversion');
  @sqlite3_function_type          := GetAddress('sqlite3_function_type');
  @sqlite3_result_text            := GetAddress('sqlite3_result_text');
  @sqlite3_result_int             := GetAddress('sqlite3_result_int');
  @sqlite3_result_double          := GetAddress('sqlite3_result_double');
  @sqlite3_result_error           := GetAddress('sqlite3_result_error');
  @sqlite3_user_data              := GetAddress('sqlite3_user_data');
  @sqlite3_aggregate_context      := GetAddress('sqlite3_aggregate_context');
  @sqlite3_aggregate_count        := GetAddress('sqlite3_aggregate_count');
  @sqlite3_set_authorizer         := GetAddress('sqlite3_set_authorizer');
  @sqlite3_trace                  := GetAddress('sqlite3_trace');
  @sqlite3_progress_handler       := GetAddress('sqlite3_progress_handler');
  @sqlite3_commit_hook            := GetAddress('sqlite3_commit_hook');
  @sqlite3_rekey                  := GetAddress('sqlite3_rekey');
  @sqlite3_key                    := GetAddress('sqlite3_key');
  { backup api }
  @sqlite3_backup_init := GetAddress('sqlite3_backup_init');
  @sqlite3_backup_step := GetAddress('sqlite3_backup_step');
  @sqlite3_backup_finish := GetAddress('sqlite3_backup_finish');
  @sqlite3_backup_remaining := GetAddress('sqlite3_backup_remaining');
  @sqlite3_backup_pagecount := GetAddress('sqlite3_backup_pagecount');
  end;
end;

constructor TZSQLite3PlainDriver.Create;
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(WINDOWS_DLL3_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL3_LOCATION);
    FLoader.AddLocation(LINUX_DLL3_LOCATION+'.0');
  {$ENDIF}
  LoadCodePages;
end;

function TZSQLite3PlainDriver.GetProtocol: string;
begin
  Result := 'sqlite-3';
end;

function TZSQLite3PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for SQLite 3';
end;

{$ENDIF ZEOS_DISABLE_SQLITE}

end.

