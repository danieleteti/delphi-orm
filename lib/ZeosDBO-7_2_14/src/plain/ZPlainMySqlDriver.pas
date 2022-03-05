{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for MySQL              }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{    Thanks to :                                          }
{               Pascal Data Objects Library               }
{               John Marino, www.synsport.com             }
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

unit ZPlainMySqlDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL}

uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZPlainDriver, ZCompatibility, ZPlainMySqlConstants;

const
  MARIADB_LOCATION = 'libmariadb'+ SharedSuffix;
{$IFNDEF UNIX}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  WINDOWS_DLL_LOCATION = 'libmysql.dll';
  WINDOWS_DLL_LOCATION_EMBEDDED = 'libmysqld.dll';
  {$ENDIF}
  WINDOWS_DLL41_LOCATION = 'libmysql41.dll';
  WINDOWS_DLL41_LOCATION_EMBEDDED = 'libmysqld41.dll';
  WINDOWS_DLL50_LOCATION = 'libmysql50.dll';
  WINDOWS_DLL50_LOCATION_EMBEDDED = 'libmysqld50.dll';
  WINDOWS_DLL51_LOCATION = 'libmysql51.dll';
  WINDOWS_DLL51_LOCATION_EMBEDDED = 'libmysqld51.dll';
  WINDOWS_DLL55_LOCATION = 'libmysql55.dll';
  WINDOWS_DLL55_LOCATION_EMBEDDED = 'libmysqld55.dll';
  WINDOWS_DLL56_LOCATION = 'libmysql56.dll';
  WINDOWS_DLL56_LOCATION_EMBEDDED = 'libmysqld56.dll';
  WINDOWS_DLL57_LOCATION = 'libmysql57.dll';
  WINDOWS_DLL57_LOCATION_EMBEDDED = 'libmysqld57.dll';
{$ELSE}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  LINUX_DLL_LOCATION = 'libmysqlclient'+SharedSuffix;
  LINUX_DLL_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix;
  {$ENDIF}
  LINUX_DLL41_LOCATION = 'libmysqlclient'+SharedSuffix+'.14';
  LINUX_DLL41_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.14';
  LINUX_DLL50_LOCATION = 'libmysqlclient'+SharedSuffix+'.15';
  LINUX_DLL50_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.15';
  LINUX_DLL51_LOCATION = 'libmysqlclient'+SharedSuffix+'.16';
  LINUX_DLL51_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.16';
  LINUX_DLL55_LOCATION = 'libmysqlclient'+SharedSuffix+'.18';
  LINUX_DLL55_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.18';
  LINUX_DLL56_LOCATION = 'libmysqlclient'+SharedSuffix+'.19';
  LINUX_DLL56_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.19';
  LINUX_DLL57_LOCATION = 'libmysqlclient'+SharedSuffix+'.20';
  LINUX_DLL57_LOCATION_EMBEDDED = 'libmysqld'+SharedSuffix+'.20';
{$ENDIF}

type
  {** Represents a generic interface to MySQL native API. }
  IZMySQLPlainDriver = interface (IZPlainDriver)
    ['{D1CB3F6C-72A1-4125-873F-791202ACC5F0}']
    function IsMariaDBDriver: Boolean;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(mysql: PMYSQL): Integer;
    {END ADDED by fduenas 15-06-2006}

    function GetAffectedRows(mysql: PMYSQL): Int64;
    function character_set_name(mysql: PMYSQL): PAnsiChar;// char_set_name
    procedure Close(mysql: PMYSQL);
    function Connect(mysql: PMYSQL; const Host, User, Password: PAnsiChar): PMYSQL;
    function CreateDatabase(mysql: PMYSQL; const Database: PAnsiChar): Integer;
    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    procedure Debug(Debug: PAnsiChar);
    function DropDatabase(mysql: PMYSQL; const Database: PAnsiChar): Integer;
    function DumpDebugInfo(mysql: PMYSQL): Integer;
    // eof
    function GetLastErrorCode(mysql: PMYSQL): Integer;
    function GetLastError(mysql: PMYSQL): PAnsiChar;
    function FetchField(Res: PZMySQLResult): PMYSQL_FIELD;
    // fetch_field_direct
    // fetch_fields
    function FetchLengths(Res: PZMySQLResult): PULongArray;
    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;
    // field_tell
    procedure FreeResult(Res: PZMySQLResult);
    function GetClientInfo: PAnsiChar;
    function GetHostInfo(mysql: PMYSQL): PAnsiChar;
    function GetProtoInfo(mysql: PMYSQL): Cardinal;
    function GetServerInfo(mysql: PMYSQL): PAnsiChar;
    // info
    function Init(const mysql: PMYSQL): PMYSQL;
    function GetLastInsertID (mysql: PMYSQL): Int64;
    function Kill(mysql: PMYSQL; Pid: LongInt): Integer;
    function GetListDatabases(mysql: PMYSQL; Wild: PAnsiChar): PZMySQLResult;
    function GetListFields(mysql: PMYSQL; const Table, Wild: PAnsiChar): PZMySQLResult;
    function GetListProcesses(mysql: PMYSQL): PZMySQLResult;
    function GetListTables(mysql: PMYSQL; const Wild: PAnsiChar): PZMySQLResult;
    function GetNumRows(Res: PZMySQLResult): Int64;
    function SetOptions(mysql: PMYSQL; Option: TMySQLOption; const Arg: Pointer): Integer;
    function Ping(mysql: PMYSQL): Integer;
    function ExecQuery(mysql: PMYSQL; const Query: PAnsiChar): Integer; overload;
    function RealConnect(mysql: PMYSQL; const Host, User, Password, Db: PAnsiChar; Port: Cardinal; UnixSocket: PAnsiChar; ClientFlag: Cardinal): PMYSQL;
    function ExecRealQuery(mysql: PMYSQL; const Query: PAnsiChar; Length: Integer): Integer;
    function Refresh(mysql: PMYSQL; Options: Cardinal): Integer;
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    // row_tell
    function SelectDatabase(mysql: PMYSQL; const Database: PAnsiChar): Integer;
    function SslSet(mysql: PMYSQL; const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
    function GetStatInfo(mysql: PMYSQL): PAnsiChar;
    function StoreResult(mysql: PMYSQL): PZMySQLResult;
    function GetThreadId(mysql: PMYSQL): Cardinal;
    function use_result(mysql: PMYSQL): PZMySQLResult;

    // thread_init
    // thread_end
    // thread_safe

    // server_init
    // server_end

    // change_user
    // function GetClientVersion: AnsiString;

    function Shutdown(mysql: PMYSQL; shutdown_level: TMysqlShutdownLevel = ZPlainMySqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!

    function SetAutocommit (mysql: PMYSQL; mode: Boolean): Boolean;
    function Commit (mysql: PMYSQL): Boolean;
    //function GetServerVersion (mysql: PMYSQL): AnsiString;
    // hex_string
    function CheckAnotherRowset   (mysql: PMYSQL): Boolean;
    function RetrieveNextRowset   (mysql: PMYSQL): Integer;
    function Rollback (mysql: PMYSQL): Boolean;
    {ADDED by EgonHugeist}
    function set_character_set(mysql: PMYSQL; const csname: PAnsiChar): Integer; // set_character_set returns 0 if valid
    // set_server_option
    function GetSQLState (mysql: PMYSQL): {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
    // warning_count

    function EscapeString(mysql: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: ULong): ULong;
    function stmt_affected_rows(stmt: PMYSQL_STMT): Int64;
    // stmt_attr_get
    function stmt_attr_set(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; arg: Pointer): Byte;
    function stmt_bind_param(stmt: PMYSQL_STMT; bindArray: PZMysqlBindArray): Byte;
    function stmt_bind_result(stmt: PMYSQL_STMT;  bindArray: PZMysqlBindArray): Byte;
    function stmt_close(stmt: PMYSQL_STMT): Byte;
    procedure stmt_data_seek(stmt: PMYSQL_STMT; Offset: Cardinal);
    function stmt_errno(stmt: PMYSQL_STMT): Integer;
    function stmt_error(stmt: PMYSQL_STMT): {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
    function stmt_execute(stmt: PMYSQL_STMT): Integer;
    function stmt_fetch(stmt: PMYSQL_STMT): Integer;
    function stmt_fetch_column(stmt: PMYSQL_STMT; bind: Pointer{BIND record}; column: UInt; offset: ULong): Integer;
    function stmt_field_count(stmt: PMYSQL_STMT): UInt;
    function stmt_free_result(stmt: PMYSQL_STMT): Byte;
    function stmt_init(mysql: PMYSQL): PMYSQL_STMT;
    function stmt_insert_id(stmt: PMYSQL_STMT): Int64;
    function stmt_next_result(stmt: PMYSQL_STMT): Integer;
    function stmt_num_rows(stmt: PMYSQL_STMT): Int64;
    function stmt_param_count(stmt: PMYSQL_STMT): Cardinal; // param_count
    function stmt_execute_direct(stmt: PMYSQL_STMT; query: PAnsiChar; Length: ULong): Integer;

    function stmt_param_metadata(stmt: PMYSQL_STMT): PZMySQLResult;
    function stmt_prepare(stmt: PMYSQL_STMT; const Query: PAnsiChar; Length: Integer): Integer;
    function stmt_reset(stmt: PMYSQL_STMT): Byte;
    function stmt_result_metadata(stmt: PMYSQL_STMT): PZMySQLResult;
    function stmt_row_seek(stmt: PMYSQL_STMT; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    // stmt_row_tell
    function stmt_send_long_data(stmt: PMYSQL_STMT; parameter_number: Cardinal; const data: PAnsiChar; length: Cardinal): Byte;
    function stmt_sqlstate(stmt: PMYSQL_STMT): PAnsiChar;
    function stmt_store_result(stmt: PMYSQL_STMT): Integer;

    procedure GetCharacterSetInfo(mysql: PMYSQL; CharSetInfo: PMY_CHARSET_INFO);// get_character_set_info since 5.0.10

    {non API functions}
    function field_count(mysql: PMYSQL): UInt;
    function GetRowCount(Res: PZMySQLResult): Int64;
    function num_fields(Res: PZMySQLResult): UInt;
    procedure SetDriverOptions(Options: TStrings); // changed by tohenk, 2009-10-11
  end;

  {** Implements a base driver for MySQL}

  { TZMySQLPlainDriver }

  TZMySQLPlainDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZMySQLPlainDriver)
  public
    FIsMariaDBDriver: Boolean;
    { ************** Plain API Function types definition ************* }
    { Functions to get information from the MYSQL and MYSQL_RES structures
      Should definitely be used if one uses shared libraries. }
    mysql_get_character_set_info: procedure(mysql: PMYSQL; cs: PMY_CHARSET_INFO); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_affected_rows:          function( mysql: PMYSQL): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_character_set_name:     function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_close:                  procedure(mysql: PMYSQL); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_connect:                function(mysql: PMYSQL; const Host, User, Passwd: PAnsiChar): PMYSQL;   {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_create_db:              function(mysql: PMYSQL; const Db: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_data_seek:              procedure(Result: PMYSQL_RES; Offset: ULongLong); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_debug:                  procedure(Debug: PAnsiChar); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_drop_db:                function(mysql: PMYSQL; const Db: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_dump_debug_info:        function(mysql: PMYSQL): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_eof:                    function(Result: PMYSQL_RES): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_errno:                  function(mysql: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_error:                  function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_escape_string:          function(PTo, PFrom: PAnsiChar; Len: ULong): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_field:            function(Result: PMYSQL_RES): PMYSQL_FIELD; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_field_direct:     function(Result: PMYSQL_RES; FieldNo: UInt): PMYSQL_FIELD; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_fields:           function(Result: PMYSQL_RES): PMYSQL_FIELD; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_lengths:          function(Result: PMYSQL_RES): PULongArray; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_fetch_row:              function(Result: PMYSQL_RES): PMYSQL_ROW; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_field_seek:             function(Result: PMYSQL_RES; Offset: MYSQL_FIELD_OFFSET): MYSQL_FIELD_OFFSET; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_field_tell:             function(Result: PMYSQL_RES): MYSQL_FIELD_OFFSET; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_free_result:            procedure(Result: PMYSQL_RES); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_client_info:        function: PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_host_info:          function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_proto_info:         function(mysql: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_server_info:        function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_info:                   function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_init:                   function(mysql: PMYSQL): PMYSQL; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_insert_id:              function(mysql: PMYSQL): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_kill:                   function(mysql: PMYSQL; Pid: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_list_dbs:               function(mysql: PMYSQL; Wild: PAnsiChar): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_list_fields:            function(mysql: PMYSQL; const Table, Wild: PAnsiChar): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_list_processes:         function(mysql: PMYSQL): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_list_tables:            function(mysql: PMYSQL; const Wild: PAnsiChar): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_num_fields:             function(Result: PMYSQL_RES): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_num_rows:               function(Result: PMYSQL_RES): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_options:                function(mysql: PMYSQL; Option: TMySqlOption; const Arg: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_ping:                   function(mysql: PMYSQL): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_query:                  function(mysql: PMYSQL; const Query: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_real_connect:           function(mysql: PMYSQL; const Host, User, Passwd, Db: PAnsiChar; Port: UInt; const UnixSocket: PAnsiChar; ClientFlag: ULong): PMYSQL; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_real_escape_string:     function(mysql: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: ULong): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_real_query:             function(mysql: PMYSQL; const Query: PAnsiChar; Length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_refresh:                function(mysql: PMYSQL; Options: UInt): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_row_seek:               function(Result: PMYSQL_RES; Offset: PMYSQL_ROWS): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_row_tell:               function(Result: PMYSQL_RES): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_select_db:              function(mysql: PMYSQL; const Db: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_ssl_set:                function(mysql: PMYSQL; const key, cert, CA, CApath, cipher: PAnsiChar): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stat:                   function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_store_result:           function(mysql: PMYSQL): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_thread_id:              function(mysql: PMYSQL): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_use_result:             function(mysql: PMYSQL): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Set up and bring down a thread; these function should be called for each thread in an application which
      opens at least one MySQL connection.  All uses of the connection(s) should be between these function calls. }
    my_init:                      procedure; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_thread_init:            function: Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_thread_end:             procedure; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_thread_safe:            function: UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    { Set up and bring down the server; to ensure that applications will work when linked against either the
      standard client library or the embedded server library, these functions should be called. }
    mysql_server_init:            function(Argc: Integer; Argv, Groups: Pointer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF}; //deprecated
    mysql_library_init:           function(Argc: Integer; Argv, Groups: Pointer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_server_end:             procedure; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF}; //deprecated
    mysql_library_end:            procedure; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mysql_change_user:            function(mysql: PMYSQL; const user: PAnsiChar; const passwd: PAnsiChar; const db: PAnsiChar): Byte;
    mysql_field_count:            function(mysql: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_client_version:     function: ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mysql_send_query:             function(mysql: PMYSQL; const query: PAnsiChar; length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_read_query_result:      function(mysql: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mysql_autocommit:             function(mysql: PMYSQL; const mode: Byte): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_commit:                 function(mysql: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_get_server_version:     function(mysql: PMYSQL): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_hex_string:             function(PTo, PFrom: PAnsiChar; Len: ULong): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_more_results:           function(mysql: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_next_result:            function(mysql: PMYSQL): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_rollback:               function(mysql: PMYSQL): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_set_character_set:      function(mysql: PMYSQL; const csname: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_set_server_option:      function(mysql: PMYSQL; Option: TMysqlSetOption): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_shutdown:               function(mysql: PMYSQL; shutdown_level: TMysqlShutdownLevel): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_sqlstate:               function(mysql: PMYSQL): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_warning_count:          function(mysql: PMYSQL): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    {BELOW are new PREPARED STATEMENTS}
    mysql_stmt_affected_rows:     function(stmt: PMYSQL_STMT): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_attr_get:          function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; arg: PAnsiChar): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_attr_set517UP:     function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; const arg: Pointer): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_attr_set:          function(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType; const arg: Pointer): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_bind_param:        function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_bind_result:       function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_close:             function(stmt: PMYSQL_STMT): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_data_seek:         procedure(stmt: PMYSQL_STMT; offset: ULongLong); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_errno:             function(stmt: PMYSQL_STMT): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_error:             function(stmt: PMYSQL_STMT): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_execute:           function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_fetch:             function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_fetch_column:      function(stmt: PMYSQL_STMT; bind: Pointer{BIND record}; column: UInt; offset: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_field_count:       function(stmt: PMYSQL_STMT): UInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_free_result:       function(stmt: PMYSQL_STMT): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_init:              function(mysql: PMYSQL): PMYSQL_STMT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_insert_id:         function(stmt: PMYSQL_STMT): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_next_result:       function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_num_rows:          function(stmt: PMYSQL_STMT): ULongLong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_param_count:       function(stmt: PMYSQL_STMT): ULong; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_param_metadata:    function(stmt: PMYSQL_STMT): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_prepare:           function(stmt: PMYSQL_STMT; const query: PAnsiChar; length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_reset:             function(stmt: PMYSQL_STMT): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_result_metadata:   function(stmt: PMYSQL_STMT): PMYSQL_RES; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_row_seek:          function(stmt: PMYSQL_STMT; offset: PMYSQL_ROWS): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_row_tell:          function(stmt: PMYSQL_STMT): PMYSQL_ROWS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_send_long_data:    function(stmt: PMYSQL_STMT; parameter_number: UInt; const data: PAnsiChar; length: ULong): Byte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_sqlstate:          function(stmt: PMYSQL_STMT): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    mysql_stmt_store_result:      function(stmt: PMYSQL_STMT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    mariadb_stmt_execute_direct:  function(stmt: PMYSQL_STMT; query: PAnsiChar; Length: ULong): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    /// <summary>From MariaDB-Docs:
    ///  Immediately aborts a connection by making all subsequent
    ///  read/write operations fail. mariadb_cancel() does not invalidate memory
    ///  used for mysql structure, nor close any communication channels. To free
    ///  the memory, mysql_close() must be called. mariadb_cancel() is useful to
    ///  break long queries in situations where sending KILL is not possible.
    /// </summary>
    /// <param>"mysql" mysql handle, which was previously allocated
    ///  by mysql_init() or mysql_real_connect().</param>
    /// <returns>???</returns>
    mariadb_cancel: function(mysql: PMYSQL): integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    /// <summary>From MariaDB-Docs:
    ///  mariadb_reconnect() tries to reconnect to a server in case the
    ///  connection died due to timeout or other errors. It uses the same
    ///  credentials which were specified in mysql_real_connect().
    /// </summary>
    /// <param>"mysql" mysql handle, which was previously allocated
    ///  by mysql_init() or mysql_real_connect().</param>
    /// <returns>0 on success; an error, if the option MYSQL_OPT_RECONNECT
    ///  wasn't specified before.</returns>
    mariadb_reconnect: function(mysql: PMYSQL): my_bool;

  protected
    ServerArgs: array of PAnsiChar;
    ServerArgsRaw: array of RawByteString;
    IsEmbeddedDriver: Boolean;
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    procedure LoadApi; override;
    procedure BuildServerArguments(const Options: TStrings);
  public
    constructor Create;
    destructor Destroy; override;

    function IsMariaDBDriver: Boolean;
    procedure Debug(Debug: PAnsiChar);
    function DumpDebugInfo(mysql: PMYSQL): Integer;
    function GetLastError(mysql: PMYSQL): PAnsiChar;
    function GetLastErrorCode(mysql: PMYSQL): Integer;
    function Init(const mysql: PMYSQL): PMYSQL; virtual;
    function GetLastInsertID (mysql: PMYSQL): Int64;

    function Connect(mysql: PMYSQL;
      const Host, User, Password: PAnsiChar): PMYSQL;
    function RealConnect(mysql: PMYSQL;
      const Host, User, Password, Db: PAnsiChar; Port: Cardinal;
      UnixSocket: PAnsiChar; ClientFlag: Cardinal): PMYSQL;
    procedure Close(mysql: PMYSQL);

    function ExecQuery(mysql: PMYSQL; const Query: PAnsiChar): Integer; overload;
    function ExecRealQuery(mysql: PMYSQL; const Query: PAnsiChar;
      Length: Integer): Integer;

    function SelectDatabase(mysql: PMYSQL;
      const Database: PAnsiChar): Integer;
    function SslSet(mysql: PMYSQL; const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
    function CreateDatabase(mysql: PMYSQL;
      const Database: PAnsiChar): Integer;
    function DropDatabase(mysql: PMYSQL;
      const Database: PAnsiChar): Integer;

    function Shutdown(mysql: PMYSQL; shutdown_level: TMysqlShutdownLevel = ZPlainMySqlConstants.SHUTDOWN_DEFAULT): Integer; // 2 versions!!
    function SetAutocommit (mysql: PMYSQL; mode: Boolean): Boolean;
    function Commit (mysql: PMYSQL): Boolean;
    function CheckAnotherRowset   (mysql: PMYSQL): Boolean;
    function RetrieveNextRowset   (mysql: PMYSQL): Integer;
    function Rollback (mysql: PMYSQL): Boolean;
    function GetSQLState (mysql: PMYSQL): {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};

    function stmt_attr_set(stmt: PMYSQL_STMT; option: TMysqlStmtAttrType;
                                  arg: Pointer): Byte;
    function stmt_affected_rows(stmt: PMYSQL_STMT): Int64;
    function stmt_bind_param(stmt: PMYSQL_STMT; bindArray: PZMysqlBindArray): Byte;
    function stmt_bind_result(stmt: PMYSQL_STMT;  bindArray: PZMysqlBindArray): Byte;
    function stmt_close(stmt: PMYSQL_STMT): Byte;
    procedure stmt_data_seek(stmt: PMYSQL_STMT; Offset: Cardinal);
    function stmt_errno(stmt: PMYSQL_STMT): Integer;
    function stmt_error(stmt: PMYSQL_STMT): {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
    function stmt_execute(stmt: PMYSQL_STMT): Integer;
    function stmt_fetch(stmt: PMYSQL_STMT): Integer;
    function stmt_fetch_column(stmt: PMYSQL_STMT; bind: Pointer{BIND record};
      column: UInt; offset: ULong): Integer;
    function stmt_field_count(stmt: PMYSQL_STMT): UInt;
    function stmt_free_result(stmt: PMYSQL_STMT): Byte;
    function stmt_init(mysql: PMYSQL): PMYSQL_STMT;
    function stmt_insert_id(stmt: PMYSQL_STMT): Int64;
    function stmt_next_result(stmt: PMYSQL_STMT): Integer;
    function stmt_num_rows(stmt: PMYSQL_STMT): Int64;
    function stmt_param_count(stmt: PMYSQL_STMT): Cardinal;
    function stmt_param_metadata(stmt: PMYSQL_STMT): PZMySQLResult;
    function stmt_prepare(stmt: PMYSQL_STMT; const Query: PAnsiChar; Length: Integer): Integer;
    function stmt_reset(stmt: PMYSQL_STMT): Byte;
    function stmt_result_metadata(stmt: PMYSQL_STMT): PZMySQLResult;
    function stmt_row_seek(stmt: PMYSQL_STMT; Row: PZMySQLRowOffset): PZMySQLRowOffset;
    function stmt_send_long_data(stmt: PMYSQL_STMT; parameter_number: Cardinal; const data: PAnsiChar; length: Cardinal): Byte;
    function stmt_sqlstate(stmt: PMYSQL_STMT): PAnsiChar;
    function stmt_store_result(stmt: PMYSQL_STMT): Integer;
    function stmt_execute_direct(stmt: PMYSQL_STMT; query: PAnsiChar; Length: ULong): Integer;
    procedure GetCharacterSetInfo(mysql: PMYSQL; CharSetInfo: PMY_CHARSET_INFO);

    function Refresh(mysql: PMYSQL; Options: Cardinal): Integer;
    function Kill(mysql: PMYSQL; Pid: LongInt): Integer;
    function Ping(mysql: PMYSQL): Integer;

    function GetStatInfo(mysql: PMYSQL): PAnsiChar;
    function SetOptions(mysql: PMYSQL; Option: TMySQLOption;
      const Arg: Pointer): Integer;
    function EscapeString(mysql: PMYSQL; PTo: PAnsiChar; const PFrom: PAnsiChar; length: ULong): ULong;
    function GetServerInfo(mysql: PMYSQL): PAnsiChar;
    function GetClientInfo: PAnsiChar;
    function GetHostInfo(mysql: PMYSQL): PAnsiChar;
    function GetProtoInfo(mysql: PMYSQL): Cardinal;
    function GetThreadId(mysql: PMYSQL): Cardinal;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer;
    function GetServerVersion(mysql: PMYSQL): Integer;
    {END ADDED by fduenas 15-06-2006}
    function GetListDatabases(mysql: PMYSQL;
      Wild: PAnsiChar): PZMySQLResult;
    function GetListTables(mysql: PMYSQL;
      const Wild: PAnsiChar): PZMySQLResult;
    function GetNumRows(Res: PZMySQLResult): Int64;
    function GetListFields(mysql: PMYSQL;
      const Table, Wild: PAnsiChar): PZMySQLResult;
    function GetListProcesses(mysql: PMYSQL): PZMySQLResult;

    function StoreResult(mysql: PMYSQL): PZMySQLResult;
    function use_result(mysql: PMYSQL): PZMySQLResult;
    procedure FreeResult(Res: PZMySQLResult);
    function GetAffectedRows(mysql: PMYSQL): Int64;
    {ADDED by EgonHugeist}
    function character_set_name(mysql: PMYSQL): PAnsiChar;// char_set_name
    function set_character_set(mysql: PMYSQL; const csname: PAnsiChar): Integer; // set_character_set Returns 0 if valid

    function FetchRow(Res: PZMySQLResult): PZMySQLRow;
    function FetchLengths(Res: PZMySQLResult): PULongArray;
    function FetchField(Res: PZMySQLResult): PMYSQL_FIELD;

    procedure SeekData(Res: PZMySQLResult; Offset: Cardinal);
    function SeekRow(Res: PZMySQLResult; Row: PZMySQLRowOffset):
      PZMySQLRowOffset;
    function SeekField(Res: PZMySQLResult; Offset: Cardinal): Cardinal;
    function num_fields(Res: PZMySQLResult): UInt;

    function field_count(mysql: PMYSQL): UInt;
    function GetRowCount(Res: PZMySQLResult): Int64;
    procedure SetDriverOptions(Options: TStrings); virtual; // changed by tohenk, 2009-10-11
  end;

  {** Implements a driver for MySQL 4.1 }

  { TZNewMySQL41PlainDriver }

  TZMySQL41PlainDriver = class (TZMySQLPlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Implements a driver for MySQL 4.1 }

  { TZNewMySQLD41PlainDriver }

  TZMySQLD41PlainDriver = class (TZMySQL41PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZNewMySQL5PlainDriver }

  TZMySQL5PlainDriver = class (TZMySQLPlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  protected
    procedure LoadApi; override;
    procedure LoadCodePages; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZNewMySQLD5PlainDriver }

  TZMySQLD5PlainDriver = class (TZMySQL5PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZMariaDB5PlainDriver }

  TZMariaDB5PlainDriver = class (TZMySQL5PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZMariaDB10PlainDriver }
  TZMariaDB10PlainDriver = class (TZMySQL5PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

implementation

uses SysUtils, ZPlainLoader, ZEncoding, ZFastCode
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{$IFOPT R+}
  {$DEFINE RangeCheckEnabled}
{$ENDIF}
{ TZMySQLPlainBaseDriver }
function TZMySQLPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'utf8';
end;

procedure TZMySQLPlainDriver.LoadCodePages;
begin
  {MySQL 3.23-4.1}
  { MultiByte }
  AddCodePage('big5', 1, ceAnsi, zCP_Big5, '', 2); {Big5 Traditional Chinese}
  AddCodePage('ujis', 10, ceAnsi, zCP_EUC_JP, '', 3); {EUC-JP Japanese}
  AddCodePage('sjis', 11, ceAnsi, zCP_SHIFTJS, '', 2); {Shift-JIS Japanese}
  AddCodePage('gbk', 19, ceAnsi, zCP_GB2312, '', 2); {GBK Simplified Chinese}
  AddCodePage('utf8', 22, ceUTF8, zCP_UTF8, '', 3); {UTF-8 Unicode}
  AddCodePage('ucs2', 23, ceUTF16, zCP_UTF16, 'utf8', 2); {UCS-2 Unicode}
  AddCodePage('euckr', 14, ceAnsi, zCP_EUCKR, '', 2); {EUC-KR Korean}
  AddCodePage('gb2312', 16, ceAnsi, zCP_GB2312, '', 2); {GB2312 Simplified Chinese}
  AddCodePage('cp932', 35, ceAnsi, zCP_SHIFTJS, '', 2); {SJIS for Windows Japanese}
  AddCodePage('eucjpms', 36, ceAnsi, $ffff, '', 3); {UJIS for Windows Japanese}
  { SingleChar }
  AddCodePage('dec8', 2); {DEC West European}
  AddCodePage('cp850', 3, ceAnsi, zCP_DOS850); {DOS West European}
  AddCodePage('hp8', 4); {HP West European}
  AddCodePage('koi8r', 5, ceAnsi, zCP_KOI8R); {KOI8-R Relcom Russian}
  AddCodePage('latin1', 6, ceAnsi, zCP_WIN1252); {cp1252 West European}
  AddCodePage('latin2', 7, ceAnsi, zCP_L2_ISO_8859_2); {ISO 8859-2 Central European}
  AddCodePage('swe7', 8, ceAnsi, zCP_x_IA5_Swedish); {7bit Swedish}
  AddCodePage('ascii', 9, ceAnsi, zCP_us_ascii); {US ASCII}
  AddCodePage('hebrew', 12, ceAnsi, zCP_L8_ISO_8859_8); {ISO 8859-8 Hebrew}
  AddCodePage('tis620', 13, ceAnsi, zCP_WIN874); {TIS620 Thai}
  AddCodePage('koi8u', 15, ceAnsi, zCP_KOI8U); {KOI8-U Ukrainian}
  AddCodePage('greek', 17, ceAnsi, zCP_L7_ISO_8859_7); {ISO 8859-7 Greek}
  AddCodePage('cp1250', 18, ceAnsi, zCP_WIN1250); {Windows Central European}
  AddCodePage('latin5', 20, ceAnsi, zCP_L5_ISO_8859_9); {ISO 8859-9 Turkish}
  AddCodePage('armscii8', 21, ceAnsi, zCP_us_ascii); {ARMSCII-8 Armenian}
  AddCodePage('cp866', 24, ceAnsi, zCP_DOS866); {DOS Russian}
  AddCodePage('keybcs2', 25); {DOS Kamenicky Czech-Slovak}
  AddCodePage('macce', 26, ceAnsi, zCP_x_mac_ce); {Mac Central European}
  AddCodePage('macroman', 27, ceAnsi, zCP_macintosh); {Mac West European}
  AddCodePage('cp852', 28, ceAnsi, zCP_DOS852); {DOS Central European}
  AddCodePage('latin7', 29, ceAnsi, zCP_L7_ISO_8859_13); {ISO 8859-13 Baltic}
  AddCodePage('cp1251', 30, ceAnsi, zCP_WIN1251); {Windows Cyrillic}
  AddCodePage('cp1256', 31, ceAnsi, zCP_WIN1256); {Windows Arabic}
  AddCodePage('cp1257', 32, ceAnsi, zCP_WIN1257); {Windows Baltic}
  AddCodePage('binary', 33); {Binary pseudo charset}
  AddCodePage('geostd8', 34); {GEOSTD8 Georgian}
end;

procedure TZMySQLPlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do begin
  @mysql_affected_rows          := GetAddress('mysql_affected_rows');
  @mysql_character_set_name     := GetAddress('mysql_character_set_name');
  @mysql_close                  := GetAddress('mysql_close');
  @mysql_connect                := GetAddress('mysql_connect');
  @mysql_create_db              := GetAddress('mysql_create_db');
  @mysql_data_seek              := GetAddress('mysql_data_seek');
  @mysql_debug                  := GetAddress('mysql_debug');
  @mysql_drop_db                := GetAddress('mysql_drop_db');
  @mysql_dump_debug_info        := GetAddress('mysql_dump_debug_info');
  @mysql_eof                    := GetAddress('mysql_eof');
  @mysql_errno                  := GetAddress('mysql_errno');
  @mysql_error                  := GetAddress('mysql_error');
  @mysql_escape_string          := GetAddress('mysql_escape_string');
  @mysql_fetch_field            := GetAddress('mysql_fetch_field');
  @mysql_fetch_field_direct     := GetAddress('mysql_fetch_field_direct');
  @mysql_fetch_fields           := GetAddress('mysql_fetch_fields');
  @mysql_fetch_lengths          := GetAddress('mysql_fetch_lengths');
  @mysql_fetch_row              := GetAddress('mysql_fetch_row');
  @mysql_field_seek             := GetAddress('mysql_field_seek');
  @mysql_field_tell             := GetAddress('mysql_field_tell');
  @mysql_free_result            := GetAddress('mysql_free_result');
  @mysql_get_client_info        := GetAddress('mysql_get_client_info');
  @mysql_get_host_info          := GetAddress('mysql_get_host_info');
  @mysql_get_proto_info         := GetAddress('mysql_get_proto_info');
  @mysql_get_server_info        := GetAddress('mysql_get_server_info');
  @mysql_info                   := GetAddress('mysql_info');
  @mysql_init                   := GetAddress('mysql_init');
  @mysql_insert_id              := GetAddress('mysql_insert_id');
  @mysql_kill                   := GetAddress('mysql_kill');
  @mysql_list_dbs               := GetAddress('mysql_list_dbs');
  @mysql_list_fields            := GetAddress('mysql_list_fields');
  @mysql_list_processes         := GetAddress('mysql_list_processes');
  @mysql_list_tables            := GetAddress('mysql_list_tables');
  @mysql_num_fields             := GetAddress('mysql_num_fields');
  @mysql_num_rows               := GetAddress('mysql_num_rows');
  @mysql_options                := GetAddress('mysql_options');
  @mysql_ping                   := GetAddress('mysql_ping');
  @mysql_query                  := GetAddress('mysql_query');
  @mysql_real_connect           := GetAddress('mysql_real_connect');
  @mysql_real_escape_string     := GetAddress('mysql_real_escape_string');
  @mysql_real_query             := GetAddress('mysql_real_query');
  @mysql_refresh                := GetAddress('mysql_refresh');
  @mysql_row_seek               := GetAddress('mysql_row_seek');
  @mysql_row_tell               := GetAddress('mysql_row_tell');
  @mysql_select_db              := GetAddress('mysql_select_db');
  @mysql_shutdown               := GetAddress('mysql_shutdown');
  @mysql_ssl_set                := GetAddress('mysql_ssl_set');
  @mysql_stat                   := GetAddress('mysql_stat');
  @mysql_store_result           := GetAddress('mysql_store_result');
  @mysql_thread_id              := GetAddress('mysql_thread_id');
  @mysql_use_result             := GetAddress('mysql_use_result');

  @my_init                      := GetAddress('my_init');
  @mysql_thread_init            := GetAddress('mysql_thread_init');
  @mysql_thread_end             := GetAddress('mysql_thread_end');
  @mysql_thread_safe            := GetAddress('mysql_thread_safe');

  @mysql_server_init            := GetAddress('mysql_server_init'); //deprecated
  @mysql_library_init           := GetAddress('mysql_library_init');
  @mysql_server_end             := GetAddress('mysql_server_end');  //deprecated
  @mysql_library_end            := GetAddress('mysql_library_end');

  @mysql_change_user            := GetAddress('mysql_change_user');
  @mysql_field_count            := GetAddress('mysql_field_count');

  @mysql_get_client_version     := GetAddress('mysql_get_client_version');

  @mysql_send_query             := GetAddress('mysql_send_query');
  @mysql_read_query_result      := GetAddress('mysql_read_query_result');

  @mysql_autocommit             := GetAddress('mysql_autocommit');
  @mysql_commit                 := GetAddress('mysql_commit');
  @mysql_get_server_version     := GetAddress('mysql_get_server_version');
  @mysql_hex_string             := GetAddress('mysql_hex_string');
  @mysql_more_results           := GetAddress('mysql_more_results');
  @mysql_next_result            := GetAddress('mysql_next_result');
  @mysql_rollback               := GetAddress('mysql_rollback');
  @mysql_set_character_set      := GetAddress('mysql_set_character_set');
  @mysql_set_server_option      := GetAddress('mysql_set_server_option');
  @mysql_sqlstate               := GetAddress('mysql_sqlstate');
  @mysql_warning_count          := GetAddress('mysql_warning_count');
  {API for PREPARED STATEMENTS}
  @mysql_stmt_affected_rows     := GetAddress('mysql_stmt_affected_rows');
  @mysql_stmt_attr_get          := GetAddress('mysql_stmt_attr_get');
  @mysql_stmt_attr_set          := GetAddress('mysql_stmt_attr_set'); //uses ulong
  @mysql_stmt_attr_set517UP     := GetAddress('mysql_stmt_attr_set'); //uses mybool
  @mysql_stmt_bind_param        := GetAddress('mysql_stmt_bind_param');
  @mysql_stmt_bind_result       := GetAddress('mysql_stmt_bind_result');
  @mysql_stmt_close             := GetAddress('mysql_stmt_close');
  @mysql_stmt_data_seek         := GetAddress('mysql_stmt_data_seek');
  @mysql_stmt_errno             := GetAddress('mysql_stmt_errno');
  @mysql_stmt_error             := GetAddress('mysql_stmt_error');
  @mysql_stmt_execute           := GetAddress('mysql_stmt_execute');
  @mysql_stmt_fetch             := GetAddress('mysql_stmt_fetch');
  @mysql_stmt_fetch_column      := GetAddress('mysql_stmt_fetch_column');
  @mysql_stmt_field_count       := GetAddress('mysql_stmt_field_count');
  @mysql_stmt_free_result       := GetAddress('mysql_stmt_free_result');
  @mysql_stmt_init              := GetAddress('mysql_stmt_init');
  @mysql_stmt_insert_id         := GetAddress('mysql_stmt_insert_id');
  @mysql_stmt_num_rows          := GetAddress('mysql_stmt_num_rows');
  @mysql_stmt_param_count       := GetAddress('mysql_stmt_param_count');
  @mysql_stmt_param_metadata    := GetAddress('mysql_stmt_param_metadata');
  @mysql_stmt_prepare           := GetAddress('mysql_stmt_prepare');
  @mysql_stmt_reset             := GetAddress('mysql_stmt_reset');
  @mysql_stmt_result_metadata   := GetAddress('mysql_stmt_result_metadata');
  @mysql_stmt_row_seek          := GetAddress('mysql_stmt_row_seek');
  @mysql_stmt_row_tell          := GetAddress('mysql_stmt_row_tell');
  @mysql_stmt_send_long_data    := GetAddress('mysql_stmt_send_long_data');
  @mysql_stmt_sqlstate          := GetAddress('mysql_stmt_sqlstate');
  @mysql_stmt_store_result      := GetAddress('mysql_stmt_store_result');
  @mariadb_stmt_execute_direct  := GetAddress('mariadb_stmt_execute_direct');
  end;
end;

procedure TZMySQLPlainDriver.BuildServerArguments(const Options: TStrings);
var
  TmpList: TStringList;
  i: Integer;
begin
  TmpList := TStringList.Create;
  try
    TmpList.Add(ParamStr(0));
    for i := 0 to Options.Count - 1 do
      if SameText(SERVER_ARGUMENTS_KEY_PREFIX,
                  Copy(Options.Names[i], 1,
                       Length(SERVER_ARGUMENTS_KEY_PREFIX))) then
        TmpList.Add(Options.ValueFromIndex[i]);
    //Check if DataDir is specified, if not, then add it to the Arguments List
    if TmpList.Values['--datadir'] = '' then
       TmpList.Add('--datadir='+EMBEDDED_DEFAULT_DATA_DIR);

    SetLength(ServerArgs, TmpList.Count);
    SetLength(ServerArgsRaw, TmpList.Count);
    for i := 0 to TmpList.Count - 1 do begin
      {$IFDEF UNICODE}
      ServerArgsRaw[i] := ZUnicodeToRaw(TmpList[i], ZOSCodePage);
      {$ELSE}
      ServerArgsRaw[i] := TmpList[i];
      {$ENDIF}
      ServerArgs[i] :=  Pointer(TmpList[i]);
    end;
  finally
    {$IFDEF AUTOREFCOUNT}
    TmpList := nil;
    {$ELSE}
    TmpList.Free;
    {$ENDIF}
  end;
end;

constructor TZMySQLPlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
{$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
  {$ENDIF}
{$ENDIF}
  IsEmbeddedDriver := False;
  LoadCodePages;
end;

destructor TZMySQLPlainDriver.Destroy;
begin
  SetLength(ServerArgs, 0);
  SetLength(ServerArgsRaw, 0);

  if (FLoader.Loaded) then
    if Assigned(mysql_library_end) then
      mysql_library_end //since 5.0.3
    else
      if Assigned(mysql_server_end) then
        mysql_server_end; //deprected since 5.0.3
  inherited Destroy;
end;

function TZMySQLPlainDriver.IsMariaDBDriver: Boolean;
begin
  Result := FIsMariaDBDriver;
end;

procedure TZMySQLPlainDriver.Close(mysql: PMYSQL);
begin
  mysql_close(mysql);
end;

function TZMySQLPlainDriver.Connect(mysql: PMYSQL; const Host,
  User, Password: PAnsiChar): PMYSQL;
begin
  Result := mysql_connect(mysql, Host, User, Password);
end;

function TZMySQLPlainDriver.SslSet(mysql: PMYSQL;
  const Key, Cert, Ca, Capath, Cipher: PAnsiChar): Integer;
begin
  Result := mysql_ssl_set(mysql, Key, Cert, Ca, Capath, Cipher);
end;

function TZMySQLPlainDriver.CreateDatabase(mysql: PMYSQL;
  const Database: PAnsiChar): Integer;
begin
  Result := mysql_create_db(mysql, Database);
end;

procedure TZMySQLPlainDriver.Debug(Debug: PAnsiChar);
begin
  mysql_debug(Debug);
end;

function TZMySQLPlainDriver.DropDatabase(mysql: PMYSQL;
  const Database: PAnsiChar): Integer;
begin
  Result := mysql_drop_db(mysql, Database);
end;

function TZMySQLPlainDriver.DumpDebugInfo(mysql: PMYSQL): Integer;
begin
  Result := mysql_dump_debug_info(mysql);
end;

function TZMySQLPlainDriver.ExecQuery(mysql: PMYSQL;
  const Query: PAnsiChar): Integer;
begin
  Result := mysql_query(mysql, Query);
end;

function TZMySQLPlainDriver.ExecRealQuery(mysql: PMYSQL;
  const Query: PAnsiChar; Length: Integer): Integer;
begin
  Result := mysql_real_query(mysql, Query, Length);
end;

function TZMySQLPlainDriver.FetchField(Res: PZMySQLResult): PMYSQL_FIELD;
begin
  Result := mysql_fetch_field(Res);
end;

function TZMySQLPlainDriver.FetchLengths(Res: PZMySQLResult): PULongArray;
begin
  Result := mysql_fetch_lengths(Res);
end;

function TZMySQLPlainDriver.FetchRow(Res: PZMySQLResult): PZMySQLRow;
begin
  Result := mysql_fetch_row(Res);
end;

procedure TZMySQLPlainDriver.FreeResult(Res: PZMySQLResult);
begin
  mysql_free_result(Res);
end;

function TZMySQLPlainDriver.GetAffectedRows(mysql: PMYSQL): Int64;
begin
  Result := mysql_affected_rows(mysql);
end;

{**
  EgonHugeist: Get CharacterSet of current Connection
  Returns the default character set name for the current connection.
}
function TZMySQLPlainDriver.character_set_name(mysql: PMYSQL): PAnsiChar;// char_set_name
begin
  if Assigned(mysql_character_set_name)
  then Result := mysql_character_set_name(mysql)
  else Result := nil;
end;

{**
  EgonHugeist: This function is used to set the default character set for the
  current connection. The string csname specifies a valid character set name.
  The connection collation becomes the default collation of the character set.
  This function works like the SET NAMES statement, but also sets the value
  of mysql->charset, and thus affects the character set
  used by mysql_real_escape_string()
}
function TZMySQLPlainDriver.set_character_set(mysql: PMYSQL;
  const csname: PAnsiChar): Integer; // set_character_set Returns 0 if valid
begin
  if Assigned(mysql_set_character_set)
  then Result := mysql_set_character_set(mysql, csName)
  else Result := 1;
end;

function TZMySQLPlainDriver.GetClientInfo: PAnsiChar;
begin
  Result := mysql_get_client_info;
end;

function TZMySQLPlainDriver.EscapeString(mysql: PMYSQL; PTo: PAnsiChar;
  const PFrom: PAnsiChar; length: ULong): ULong;
begin
  if mysql = nil
  then Result := mysql_escape_string(PTo, PFrom, Length)
  else Result := mysql_real_escape_string(mysql, PTo, PFrom, Length);
end;

function TZMySQLPlainDriver.GetHostInfo(mysql: PMYSQL): PAnsiChar;
begin
  Result := mysql_get_host_info(mysql);
end;

function TZMySQLPlainDriver.GetListDatabases(mysql: PMYSQL;
  Wild: PAnsiChar): PZMySQLResult;
begin
  Result := mysql_list_dbs(mysql, Wild);
end;

function TZMySQLPlainDriver.GetListFields(mysql: PMYSQL;
  const Table, Wild: PAnsiChar): PZMySQLResult;
begin
  Result := mysql_list_fields(mysql, Table, Wild);
end;

function TZMySQLPlainDriver.GetListProcesses(
  mysql: PMYSQL): PZMySQLResult;
begin
  Result := mysql_list_processes(mysql);
end;

function TZMySQLPlainDriver.GetListTables(mysql: PMYSQL;
  const Wild: PAnsiChar): PZMySQLResult;
begin
  Result := mysql_list_tables(mysql, Wild);
end;

function TZMySQLPlainDriver.GetNumRows(Res: PZMySQLResult): Int64;
begin
    if (Res = nil) then
        Result := 0
    else
        Result :=  mysql_num_rows (Res);
end;

function TZMySQLPlainDriver.GetProtoInfo(mysql: PMYSQL): Cardinal;
begin
  Result := mysql_get_proto_info(mysql);
end;

function TZMySQLPlainDriver.GetServerInfo(mysql: PMYSQL): PAnsiChar;
begin
  Result := mysql_get_server_info(mysql);
end;

function TZMySQLPlainDriver.GetStatInfo(mysql: PMYSQL): PAnsiChar;
begin
  Result := mysql_stat(mysql);
end;

function TZMySQLPlainDriver.GetThreadId(mysql: PMYSQL): Cardinal;
begin
  Result := mysql_thread_id(mysql);
end;

function TZMySQLPlainDriver.Init(const mysql: PMYSQL): PMYSQL;
var
  ClientInfo: PAnsiChar;
  L: LengthInt;
  ErrorNo: Integer;
begin
  if (Assigned(mysql_server_init) or Assigned(mysql_library_init)){ and (ServerArgsLen > 0) }then begin
    ErrorNo := Length(ServerArgs);
    if Assigned(mysql_library_init) then //http://dev.mysql.com/doc/refman/5.7/en/mysql-library-init.html
      ErrorNo := mysql_library_init(ErrorNo, ServerArgs, @SERVER_GROUPS) //<<<-- Isn't threadsafe
    else //http://dev.mysql.com/doc/refman/5.7/en/mysql-server-init.html
      ErrorNo := mysql_server_init(ErrorNo, ServerArgs, @SERVER_GROUPS); //<<<-- Isn't threadsafe
    if ErrorNo <> 0 then
      raise Exception.Create('Could not initialize the MySQL / MariaDB client library. Error No: ' + ZFastCode.IntToStr(ErrorNo));  // The manual says nothing else can be called until this call succeeds. So lets just throw the error number...
  end;
  Result := mysql_init(mysql);
  if not Assigned(Result) then
    raise Exception.Create('Could not finish the call to mysql_init. Not enough memory?');
  ClientInfo := GetClientInfo;
  L := ZFastCode.StrLen(ClientInfo);
  FIsMariaDBDriver := Assigned(mariadb_stmt_execute_direct) or CompareMem(ClientInfo+L-7, PAnsiChar('MariaDB'), 7);
end;

function TZMySQLPlainDriver.GetLastInsertID(mysql: PMYSQL): Int64;
begin
  Result := mysql_insert_id(mysql);
end;

function TZMySQLPlainDriver.Kill(mysql: PMYSQL; Pid: LongInt): Integer;
begin
  Result := mysql_kill(mysql, Pid);
end;

function TZMySQLPlainDriver.Ping(mysql: PMYSQL): Integer;
begin
  Result := mysql_ping(mysql);
end;

function TZMySQLPlainDriver.RealConnect(mysql: PMYSQL;
  const Host, User, Password, Db: PAnsiChar; Port: Cardinal; UnixSocket: PAnsiChar;
  ClientFlag: Cardinal): PMYSQL;
begin
  Result := mysql_real_connect(mysql, Host, User, Password, Db,
    Port, UnixSocket, ClientFlag);
end;

function TZMySQLPlainDriver.Refresh(mysql: PMYSQL;
  Options: Cardinal): Integer;
begin
  Result := mysql_refresh(mysql, Options);
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZMySQLPlainDriver.SeekData(Res: PZMySQLResult;
  Offset: Cardinal);
begin
  mysql_data_seek(Res, Offset);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

function TZMySQLPlainDriver.SeekField(Res: PZMySQLResult;
  Offset: Cardinal): Cardinal;
begin
  Result := mysql_field_seek(Res, Offset);
end;

function TZMySQLPlainDriver.SeekRow(Res: PZMySQLResult;
  Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
  Result := mysql_row_seek(Res, Row);
end;

function TZMySQLPlainDriver.SelectDatabase(mysql: PMYSQL;
  const Database: PAnsiChar): Integer;
begin
  Result := mysql_select_db(mysql, Database);
end;

function TZMySQLPlainDriver.SetOptions(mysql: PMYSQL;
  Option: TMySQLOption; const Arg: Pointer): Integer;
begin
  Result := mysql_options(mysql, Option, Arg);
end;

function TZMySQLPlainDriver.Shutdown(mysql: PMYSQL; shutdown_level: TMysqlShutdownLevel = ZPlainMySqlConstants.SHUTDOWN_DEFAULT): Integer;
begin
  Result := mysql_shutdown(mysql,shutdown_level);
end;

function TZMySQLPlainDriver.SetAutocommit(mysql: PMYSQL; mode: Boolean): Boolean;
begin
  Result := mysql_autocommit(mysql, Byte(Ord(Mode))) = 0;
end;

function TZMySQLPlainDriver.Commit(mysql: PMYSQL): Boolean;
begin
  Result := mysql_commit(mysql) = 0;
end;

function TZMySQLPlainDriver.CheckAnotherRowset(mysql: PMYSQL): Boolean;
begin
  Result := mysql_more_results (mysql) <> 0;
end;

function TZMySQLPlainDriver.RetrieveNextRowset(mysql: PMYSQL): Integer;
begin
  if Assigned(mysql_next_result)
  then Result := mysql_next_result (mysql)
  else Result := -1;
end;

function TZMySQLPlainDriver.Rollback (mysql: PMYSQL): Boolean;
begin
  Result := mysql_rollback(mysql) = 0;
end;

function TZMySQLPlainDriver.GetSQLState(mysql: PMYSQL): {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
begin
  Result := mysql_sqlstate (mysql);
end;

function TZMySQLPlainDriver.stmt_attr_set(stmt: PMYSQL_STMT;
  option: TMysqlStmtAttrType; arg: Pointer): Byte;
begin
  //http://dev.mysql.com/doc/refman/4.1/en/mysql-stmt-attr-set.html
  //http://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-attr-set.html
  if mysql_get_client_version >= 50107 //avoid stack crashs !
  then Result :=  mysql_stmt_attr_set517up(PMYSQL_STMT(stmt),option,arg)
  else Result := mysql_stmt_attr_set(PMYSQL_STMT(stmt),option,arg);
end;

function TZMySQLPlainDriver.stmt_affected_rows(stmt: PMYSQL_STMT): Int64;
begin
  Result :=  mysql_stmt_affected_rows(stmt);
end;

function TZMySQLPlainDriver.stmt_bind_param(stmt: PMYSQL_STMT; bindArray: PZMysqlBindArray): Byte;
begin
    Result := mysql_stmt_bind_param(stmt, pointer(bindArray));
end;

function TZMySQLPlainDriver.stmt_bind_result(stmt: PMYSQL_STMT; bindArray: PZMysqlBindArray): Byte;
begin
  Result := mysql_stmt_bind_result(stmt, pointer(bindArray));
end;

function TZMySQLPlainDriver.stmt_close(stmt: PMYSQL_STMT): Byte;
begin
  Result := mysql_stmt_close(stmt);
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
procedure TZMySQLPlainDriver.stmt_data_seek(stmt: PMYSQL_STMT; Offset: Cardinal);
begin
  mysql_stmt_data_seek(stmt, Offset);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

function TZMySQLPlainDriver.stmt_errno(stmt: PMYSQL_STMT):Integer;
begin
    Result := mysql_stmt_errno(stmt);
end;

function TZMySQLPlainDriver.stmt_error(stmt: PMYSQL_STMT):{$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
begin
    Result := mysql_stmt_error(stmt);
end;

function TZMySQLPlainDriver.stmt_execute(stmt: PMYSQL_STMT): Integer;
begin
    Result := mysql_stmt_execute(stmt);
end;

function TZMySQLPlainDriver.stmt_execute_direct(stmt: PMYSQL_STMT;
  query: PAnsiChar; Length: ULong): Integer;
begin
  if @mariadb_stmt_execute_direct <> nil
  then Result := mariadb_stmt_execute_direct(stmt, query, Length)
  else Result := -1; //indicate we fail
end;

function TZMySQLPlainDriver.stmt_fetch(stmt: PMYSQL_STMT): Integer;
begin
    Result := mysql_stmt_fetch(stmt);
end;

function TZMySQLPlainDriver.stmt_fetch_column(stmt: PMYSQL_STMT;
  bind: Pointer{BIND record}; column: UInt; offset: ULong): Integer;
begin
  if (@mysql_stmt_fetch_column <> nil) then
    Result := mysql_stmt_fetch_column(stmt, bind, column, offset)
  else
    Result := -1; //indicate an error: http://dev.mysql.com/doc/refman/4.1/en/mysql-stmt-fetch-column.html
end;

function TZMySQLPlainDriver.stmt_field_count(stmt: PMYSQL_STMT): UInt;
begin
  Result := mysql_stmt_field_count(stmt);
end;

function TZMySQLPlainDriver.stmt_free_result(stmt: PMYSQL_STMT): Byte;
begin
   Result := mysql_stmt_free_result(stmt);
end;

function TZMySQLPlainDriver.stmt_init(mysql: PMYSQL): PMYSQL_STMT;
begin
    Result := mysql_stmt_init(PMYSQL(mysql));
end;

function TZMySQLPlainDriver.stmt_insert_id(stmt: PMYSQL_STMT): Int64;
begin
    Result := mysql_stmt_insert_id(stmt);
end;

function TZMySQLPlainDriver.stmt_next_result(stmt: PMYSQL_STMT): Integer;
begin
  if (@mysql_stmt_next_result = nil)
  then Result := -1  // Successful and there are no more results
  else Result :=  mysql_stmt_next_result(stmt);
end;

function TZMySQLPlainDriver.stmt_num_rows(stmt: PMYSQL_STMT): Int64;
begin
  Result :=  mysql_stmt_num_rows(stmt);
end;

function TZMySQLPlainDriver.stmt_param_count(stmt: PMYSQL_STMT): Cardinal;
begin
    Result := mysql_stmt_param_count(stmt);
end;

function TZMySQLPlainDriver.stmt_param_metadata(stmt: PMYSQL_STMT): PZMySQLResult;
begin
  Result := mysql_stmt_param_metadata(stmt);
end;

function TZMySQLPlainDriver.stmt_prepare(stmt: PMYSQL_STMT; const Query: PAnsiChar; Length: Integer): Integer;
begin
    Result := mysql_stmt_prepare(stmt, Query, Length);
end;

function TZMySQLPlainDriver.stmt_reset(stmt: PMYSQL_STMT): Byte;
begin
  Result := mysql_stmt_reset(stmt);
end;

function TZMySQLPlainDriver.stmt_result_metadata(stmt: PMYSQL_STMT): PZMySQLResult;
begin
    Result := mysql_stmt_result_metadata(stmt);
end;

function TZMySQLPlainDriver.stmt_row_seek(stmt: PMYSQL_STMT; Row: PZMySQLRowOffset): PZMySQLRowOffset;
begin
    Result := mysql_stmt_row_seek(stmt, Row);
end;

function TZMySQLPlainDriver.stmt_send_long_data(stmt: PMYSQL_STMT;
  parameter_number: Cardinal; const data: PAnsiChar; length: Cardinal): Byte;
begin
  Result := mysql_stmt_send_long_data(stmt, parameter_number, data, length);
end;

function TZMySQLPlainDriver.stmt_sqlstate(stmt: PMYSQL_STMT): PAnsiChar;
begin
  Result := mysql_stmt_sqlstate(stmt);
end;

function TZMySQLPlainDriver.stmt_store_result(stmt: PMYSQL_STMT): Integer;
begin
  Result := mysql_stmt_store_result(stmt);
end;

procedure TZMySQLPlainDriver.GetCharacterSetInfo(mysql: PMYSQL; CharSetInfo: PMY_CHARSET_INFO);
begin
    mysql_get_character_set_info(mysql, CharSetInfo);
end;

function TZMySQLPlainDriver.StoreResult(
  mysql: PMYSQL): PZMySQLResult;
begin
  Result := mysql_store_result(mysql);
end;

function TZMySQLPlainDriver.use_result(mysql: PMYSQL): PZMySQLResult;
begin
  Result := mysql_use_result(mysql);
end;

function TZMySQLPlainDriver.GetLastError(mysql: PMYSQL): PAnsiChar;
begin
  Result := mysql_error(mysql);
end;

function TZMySQLPlainDriver.GetRowCount(Res: PZMySQLResult): Int64;
begin
  Result := mysql_num_rows(Res);
end;

function TZMySQLPlainDriver.field_count(mysql: PMYSQL): UInt;
begin
 result := mysql_field_count(mysql);
end;

function TZMySQLPlainDriver.num_fields(Res: PZMySQLResult): UInt;
begin
  Result := mysql_num_fields(Res);
end;

function TZMySQLPlainDriver.GetLastErrorCode(mysql: PMYSQL): Integer;
begin
  Result := mysql_errno(mysql);
end;

function TZMySQLPlainDriver.GetClientVersion: Integer;
begin
 Result := mysql_get_client_version;
end;

function TZMySQLPlainDriver.GetServerVersion(mysql: PMYSQL): Integer;
begin
 Result := mysql_get_server_version(mysql);
end;

procedure TZMySQLPlainDriver.SetDriverOptions(Options: TStrings);
var
  PreferedLibrary: String;
begin
  PreferedLibrary := Options.Values['Library'];
  if PreferedLibrary <> '' then
    Loader.AddLocation(PreferedLibrary);
  if IsEmbeddedDriver then
    BuildServerArguments(Options);
end;

{ TZMySQL41PlainDriver }

function TZMySQL41PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMySQL41PlainDriver.Create;
end;

constructor TZMySQL41PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL41_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL41_LOCATION);
  {$ENDIF}
end;

function TZMySQL41PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-4.1';
end;

function TZMySQL41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 4.1+';
end;

{ TZMySQLD41PlainDriver }

function TZMySQLD41PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMySQLD41PlainDriver.Create;
end;

constructor TZMySQLD41PlainDriver.Create;
begin
  inherited Create;
  // only include embedded library
  FLoader.ClearLocations;
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION_EMBEDDED);
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL41_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL41_LOCATION_EMBEDDED);
  {$ENDIF}
  IsEmbeddedDriver := True;
end;

function TZMySQLD41PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-4.1';
end;

function TZMySQLD41PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 4.1+';
end;

{ TZMySQL5PlainDriver }

function TZMySQL5PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMySQL5PlainDriver.Create;
end;

procedure TZMySQL5PlainDriver.LoadApi;
begin
  inherited LoadApi;

  with Loader do
  begin
    @mysql_get_character_set_info := GetAddress('mysql_get_character_set_info');
    @mysql_stmt_next_result       := GetAddress('mysql_stmt_next_result');
  end;
end;

procedure TZMySQL5PlainDriver.LoadCodePages;
begin
  inherited LoadCodePages;
  {MySQL 4.1-5.5}
  { MultiByte }
  AddCodePage('utf8mb4', 37, ceUTF8, zCP_UTF8, '', 4); {UTF-8 Unicode}
  AddCodePage('utf16', 38, ceUTF16, zCP_UTF16, 'utf8', 4); {UTF-16 Unicode}
  AddCodePage('utf32', 39, ceUTF16, zCP_utf32, 'utf8', 4); {UTF-32 Unicode} //Egonhugeist improved
end;

constructor TZMySQL5PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
    FLoader.AddLocation(MARIADB_LOCATION);
  {$ENDIF}
    FLoader.AddLocation(WINDOWS_DLL50_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL51_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL55_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL56_LOCATION);
    FLoader.AddLocation(WINDOWS_DLL57_LOCATION);
  {$ELSE}
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
    FLoader.AddLocation(MARIADB_LOCATION);
  {$ENDIF}
    FLoader.AddLocation(LINUX_DLL50_LOCATION);
    FLoader.AddLocation(LINUX_DLL51_LOCATION);
    FLoader.AddLocation(LINUX_DLL55_LOCATION);
    FLoader.AddLocation(LINUX_DLL56_LOCATION);
    FLoader.AddLocation(LINUX_DLL57_LOCATION);
  {$ENDIF}
end;

function TZMySQL5PlainDriver.GetProtocol: string;
begin
  Result := 'mysql-5';
end;

function TZMySQL5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MySQL 5.0+';
end;

{ TZMySQLD5PlainDriver }

function TZMySQLD5PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMySQLD5PlainDriver.Create
end;

constructor TZMySQLD5PlainDriver.Create;
begin
  inherited Create;
  // only include embedded library
  FLoader.ClearLocations;
  {$IFNDEF MYSQL_STRICT_DLL_LOADING}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION_EMBEDDED);
  {$ENDIF}
  {$ENDIF}
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL50_LOCATION_EMBEDDED);
    FLoader.AddLocation(WINDOWS_DLL51_LOCATION_EMBEDDED);
    FLoader.AddLocation(WINDOWS_DLL55_LOCATION_EMBEDDED);
    FLoader.AddLocation(WINDOWS_DLL56_LOCATION_EMBEDDED);
    FLoader.AddLocation(WINDOWS_DLL57_LOCATION_EMBEDDED);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL50_LOCATION_EMBEDDED);
    FLoader.AddLocation(LINUX_DLL51_LOCATION_EMBEDDED);
    FLoader.AddLocation(LINUX_DLL55_LOCATION_EMBEDDED);
    FLoader.AddLocation(LINUX_DLL56_LOCATION_EMBEDDED);
    FLoader.AddLocation(LINUX_DLL57_LOCATION_EMBEDDED);
  {$ENDIF}
  IsEmbeddedDriver := True;
end;

function TZMySQLD5PlainDriver.GetProtocol: string;
begin
  Result := 'mysqld-5';
end;

function TZMySQLD5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Embedded MySQL 5+';
end;

{ TZMariaDB5PlainDriver }
function TZMariaDB5PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMariaDB5PlainDriver.Create
end;

constructor TZMariaDB5PlainDriver.Create;
begin
  inherited Create;
  FLoader.ClearLocations;
  FLoader.AddLocation(MARIADB_LOCATION);
end;

function TZMariaDB5PlainDriver.GetProtocol: string;
begin
  Result := 'MariaDB-5';
end;

function TZMariaDB5PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MariaDB-5.x';
end;

{ TZMariaDB5PlainDriver }
function TZMariaDB10PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZMariaDB10PlainDriver.Create
end;

function TZMariaDB10PlainDriver.GetProtocol: string;
begin
  Result := 'MariaDB-10';
end;

function TZMariaDB10PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for MariaDB-10';
end;
{$ENDIF ZEOS_DISABLE_MYSQL}
end.


