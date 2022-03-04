{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi plain interface to libmysql.dll           }
{                     Version 4.1                         }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{    Thanks to :                                          }
{               Pascal Data Objects Library               }
{                                                         }
{    Copyright (c) 2006 John Marino, www.synsport.com     }
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

unit ZPlainMySqlConstants;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL}

//{$A-} //pack the records!   EH: nope this is wrong!
{$Z+} //enum to DWORD
uses
   ZCompatibility;

const
{ General Declarations }
  MYSQL_ERRMSG_SIZE    = 512;
  SQLSTATE_LENGTH      = 5;

  MYSQL_PORT           = 3306;
  LOCAL_HOST           = 'localhost';

  { Field's flags }
  NOT_NULL_FLAG          = 1;     { Field can't be NULL }
  PRI_KEY_FLAG           = 2;     { Field is part of a primary key }
  UNIQUE_KEY_FLAG        = 4;     { Field is part of a unique key }
  MULTIPLE_KEY_FLAG      = 8;     { Field is part of a key }
  BLOB_FLAG              = 16;    { Field is a blob }
  UNSIGNED_FLAG          = 32;    { Field is unsigned }
  ZEROFILL_FLAG          = 64;    { Field is zerofill }
  BINARY_FLAG            = 128;   { Field is binary }
  ENUM_FLAG              = 256;   { Field is an enum }
  AUTO_INCREMENT_FLAG    = 512;   { Field is a autoincrement field }
  TIMESTAMP_FLAG         = 1024;  { Field is a timestamp }
  SET_FLAG               = 2048;  { Field is a set }
  NUM_FLAG               = 32768; { Field is num (for clients) }
  PART_KEY_FLAG	         = 16384; { Intern; Part of some key }
  GROUP_FLAG	           = 32768; { Intern: Group field }
  UNIQUE_FLAG            = 65536; { Intern: Used by sql_yacc }
  BINCMP_FLAG            = $20000; { Intern: Used by sql_yacc }
  GET_FIXED_FIELDS_FLAG  = $40000; { Used to get fields in item tree }
  FIELD_IN_PART_FUNC_FLAG= $80000; { Field part of partition func }
  FIELD_IN_ADD_INDEX     = $100000; { Intern: Field used in ADD INDEX }
  FIELD_IS_RENAMED       = $200000; { Intern: Field is being renamed}

{THD: Killable}
  MYSQL_SHUTDOWN_KILLABLE_CONNECT    = 1;
  MYSQL_SHUTDOWN_KILLABLE_TRANS      = 2;
  MYSQL_SHUTDOWN_KILLABLE_LOCK_TABLE = 4;
  MYSQL_SHUTDOWN_KILLABLE_UPDATE     = 8;

{prepared fetch results}
  STMT_FETCH_OK         = 0;
  STMT_FETCH_ERROR      = 1;
  STMT_FETCH_NO_DATA    = 100;
  STMT_FETCH_DATA_TRUNC = 101;

  {status codes}
const
   MYSQL_NO_DATA = 100;
   MYSQL_DATA_TRUNCATED  = 101;

{$MINENUMSIZE 4}
type
  TMySqlOption = (
    MYSQL_OPT_CONNECT_TIMEOUT, MYSQL_OPT_COMPRESS, MYSQL_OPT_NAMED_PIPE,
    MYSQL_INIT_COMMAND, MYSQL_READ_DEFAULT_FILE, MYSQL_READ_DEFAULT_GROUP,
    MYSQL_SET_CHARSET_DIR, MYSQL_SET_CHARSET_NAME, MYSQL_OPT_LOCAL_INFILE,
    MYSQL_OPT_PROTOCOL, MYSQL_SHARED_MEMORY_BASE_NAME, MYSQL_OPT_READ_TIMEOUT,
    MYSQL_OPT_WRITE_TIMEOUT, MYSQL_OPT_USE_RESULT,
    MYSQL_OPT_USE_REMOTE_CONNECTION, MYSQL_OPT_USE_EMBEDDED_CONNECTION,
    MYSQL_OPT_GUESS_CONNECTION, MYSQL_SET_CLIENT_IP, MYSQL_SECURE_AUTH,
    MYSQL_REPORT_DATA_TRUNCATION, MYSQL_OPT_RECONNECT,
    MYSQL_OPT_SSL_VERIFY_SERVER_CERT, MYSQL_PLUGIN_DIR, MYSQL_DEFAULT_AUTH,
    MYSQL_OPT_BIND,
    MYSQL_OPT_SSL_KEY, MYSQL_OPT_SSL_CERT,
    MYSQL_OPT_SSL_CA, MYSQL_OPT_SSL_CAPATH, MYSQL_OPT_SSL_CIPHER,
    MYSQL_OPT_SSL_CRL, MYSQL_OPT_SSL_CRLPATH,
    MYSQL_OPT_CONNECT_ATTR_RESET, MYSQL_OPT_CONNECT_ATTR_ADD,
    MYSQL_OPT_CONNECT_ATTR_DELETE,
    MYSQL_SERVER_PUBLIC_KEY,
    MYSQL_ENABLE_CLEARTEXT_PLUGIN,
    MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS,
    MYSQL_OPT_SSL_ENFORCE,
    MYSQL_OPT_MAX_ALLOWED_PACKET, MYSQL_OPT_NET_BUFFER_LENGTH,
    MYSQL_OPT_TLS_VERSION,
    MYSQL_OPT_SSL_MODE,
    {MySQL 8:}
    MYSQL_OPT_GET_SERVER_PUBLIC_KEY,
    MYSQL_OPT_RETRY_COUNT,
    MYSQL_OPT_OPTIONAL_RESULTSET_METADATA,
    MYSQL_OPT_SSL_FIPS_MODE,
    MYSQL_OPT_TLS_CIPHERSUITES,
    MYSQL_OPT_COMPRESSION_ALGORITHMS,
    MYSQL_OPT_ZSTD_COMPRESSION_LEVEL
  );
  TMariaDBOption = (
    { MariaDB specific }
    MYSQL_PROGRESS_CALLBACK=5999,
    MYSQL_OPT_NONBLOCK);
  TMariaDBConnectorOption = (
    { MariaDB Connector/C specific }
    MYSQL_DATABASE_DRIVER=7000,
    MARIADB_OPT_SSL_FP,             // deprecated, use MARIADB_OPT_TLS_PEER_FP instead
    MARIADB_OPT_SSL_FP_LIST,        // deprecated, use MARIADB_OPT_TLS_PEER_FP_LIST instead
    MARIADB_OPT_TLS_PASSPHRASE,     // passphrase for encrypted certificates
    MARIADB_OPT_TLS_CIPHER_STRENGTH,
    MARIADB_OPT_TLS_VERSION,
    MARIADB_OPT_TLS_PEER_FP,            // single finger print for server certificate verification
    MARIADB_OPT_TLS_PEER_FP_LIST,       // finger print white list for server certificate verification
    MARIADB_OPT_CONNECTION_READ_ONLY,
    MYSQL_OPT_CONNECT_ATTRS,        // for mysql_get_optionv
    MARIADB_OPT_USERDATA,
    MARIADB_OPT_CONNECTION_HANDLER,
    MARIADB_OPT_PORT,
    MARIADB_OPT_UNIXSOCKET,
    MARIADB_OPT_PASSWORD,
    MARIADB_OPT_HOST,
    MARIADB_OPT_USER,
    MARIADB_OPT_SCHEMA,
    MARIADB_OPT_DEBUG,
    MARIADB_OPT_FOUND_ROWS,
    MARIADB_OPT_MULTI_RESULTS,
    MARIADB_OPT_MULTI_STATEMENTS,
    MARIADB_OPT_INTERACTIVE,
    MARIADB_OPT_PROXY_HEADER,
    MARIADB_OPT_IO_WAIT
  );
const
  TMySqlOptionMinimumVersion: array[TMySqlOption] of Integer =
    (
      {MYSQL_OPT_CONNECT_TIMEOUT}               0,
      {MYSQL_OPT_COMPRESS}                      0,
      {MYSQL_OPT_NAMED_PIPE}                    0,
      {MYSQL_INIT_COMMAND}                      0,
      {MYSQL_READ_DEFAULT_FILE}                 0,
      {MYSQL_READ_DEFAULT_GROUP}                0,
      {MYSQL_SET_CHARSET_DIR}                   0,
      {MYSQL_SET_CHARSET_NAME}                  0,
      {MYSQL_OPT_LOCAL_INFILE}                  0,
      {MYSQL_OPT_PROTOCOL}                      40100,
      {MYSQL_SHARED_MEMORY_BASE_NAME}           40100,
      {MYSQL_OPT_READ_TIMEOUT}                  40101,
      {MYSQL_OPT_WRITE_TIMEOUT}                 40101,
      {MYSQL_OPT_USE_RESULT}                    40101,
      {MYSQL_OPT_USE_REMOTE_CONNECTION}         40101,
      {MYSQL_OPT_USE_EMBEDDED_CONNECTION}       40101,
      {MYSQL_OPT_GUESS_CONNECTION}              40101,
      {MYSQL_SET_CLIENT_IP}                     40101,
      {MYSQL_SECURE_AUTH}                       40101,
      {MYSQL_REPORT_DATA_TRUNCATION}            40101,
      {MYSQL_OPT_RECONNECT}                     50013,
      {MYSQL_OPT_SSL_VERIFY_SERVER_CERT}        50023,
      {MYSQL_PLUGIN_DIR}                        50507,
      {MYSQL_DEFAULT_AUTH}                      50507,
      {MYSQL_OPT_BIND}                          50601,
      {MYSQL_OPT_SSL_KEY}                       50603,
      {MYSQL_OPT_SSL_CERT}                      50603,
      {MYSQL_OPT_SSL_CA}                        50603,
      {MYSQL_OPT_SSL_CAPATH}                    50603,
      {MYSQL_OPT_SSL_CIPHER}                    50603,
      {MYSQL_OPT_SSL_CRL}                       50603,
      {MYSQL_OPT_SSL_CRLPATH}                   50603,
      {MYSQL_OPT_CONNECT_ATTR_RESET}            50606,
      {MYSQL_OPT_CONNECT_ATTR_ADD}              50606,
      {MYSQL_OPT_CONNECT_ATTR_DELETE}           50606,
      {MYSQL_SERVER_PUBLIC_KEY}                 50606,
      {MYSQL_ENABLE_CLEARTEXT_PLUGIN}           50607,
      {MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS}  50610,
      {MYSQL_OPT_SSL_ENFORCE}                   50703,
      {MYSQL_OPT_MAX_ALLOWED_PACKET}            60111,
      {MYSQL_OPT_NET_BUFFER_LENGTH}             60111,
      {MYSQL_OPT_SSL_MODE}                      60111,
      {MYSQL_OPT_TLS_VERSION}                   60111,
      {MYSQL_OPT_GET_SERVER_PUBLIC_KEY}         60111,
      {MYSQL_OPT_RETRY_COUNT}                   60111,
      {MYSQL_OPT_OPTIONAL_RESULTSET_METADATA}   60111,
      {MYSQL_OPT_SSL_FIPS_MODE}                 60111,
      {MYSQL_OPT_TLS_CIPHERSUITES}              60111,
      {MYSQL_OPT_COMPRESSION_ALGORITHMS}        60111,
      {MYSQL_OPT_ZSTD_COMPRESSION_LEVEL}        60111
    );
type
  Tmysql_protocol_type = ( MYSQL_PROTOCOL_DEFAULT, MYSQL_PROTOCOL_TCP, MYSQL_PROTOCOL_SOCKET,
    MYSQL_PROTOCOL_PIPE, MYSQL_PROTOCOL_MEMORY);
  // EgonHugeist: Use always a 4Byte unsigned Integer for Windows otherwise MySQL64 has problems on Win64!
  // don't know anything about reported issues on other OS's
  ULong                 = {$IFDEF MSWINDOWS}LongWord{$ELSE}NativeUInt{$ENDIF};
  ULongLong             = UInt64;
  PULong                = ^ULong;
  PULongLong            = ^ULongLong;

  Pmy_bool = ^my_bool;
  my_bool = byte;

  PUSED_MEM=^USED_MEM;
  USED_MEM = record
    next:       PUSED_MEM;
    left:       Integer;
    size:       Integer;
  end;

  PERR_PROC = ^ERR_PROC;
  ERR_PROC = procedure;

  PMEM_ROOT = ^MEM_ROOT;
  MEM_ROOT = record
    free:          PUSED_MEM;
    used:          PUSED_MEM;
    pre_alloc:     PUSED_MEM;
    min_malloc:    Integer;
    block_size:    Integer;
    block_num:     Integer;
    first_block_usage: Integer;
    error_handler: PERR_PROC;
  end;

  MYSQL_ROW = array[00..$ff] of PAnsiChar;
  PMYSQL_ROW = ^MYSQL_ROW;

  PMYSQL_ROWS = ^MYSQL_ROWS;
  MYSQL_ROWS = record
    next:       PMYSQL_ROWS;
    data:       PMYSQL_ROW;
  end;

  MYSQL_ROW_OFFSET = PMYSQL_ROWS;

  MYSQL_DATA = record
    Rows:       Int64;
    Fields:     Cardinal;
    Data:       PMYSQL_ROWS;
    Alloc:      MEM_ROOT;
  end;
  PMYSQL_DATA = ^MYSQL_DATA;

  MYSQL_FIELD_OFFSET = UInt;

  PMYSQL_OPTIONS   = ^TMYSQL_OPTIONS;
  TMYSQL_OPTIONS = record
    connect_timeout:          UInt;
    read_timeout:             UInt;
    write_timeout:            UInt;
    port:                     UInt;
    protocol:                 UInt;
    client_flag:              ULong;
    host:                     PAnsiChar;
    user:                     PAnsiChar;
    password:                 PAnsiChar;
    unix_socket:              PAnsiChar;
    db:                       PAnsiChar;
    init_commands:            Pointer;
    my_cnf_file:              PAnsiChar;
    my_cnf_group:             PAnsiChar;
    charset_dir:              PAnsiChar;
    charset_name:             PAnsiChar;
    ssl_key:                  PAnsiChar;
    ssl_cert:                 PAnsiChar;
    ssl_ca:                   PAnsiChar;
    ssl_capath:               PAnsiChar;
    ssl_cipher:               PAnsiChar;
    shared_memory_base_name:  PAnsiChar;
    max_allowed_packet:       ULong;
    use_ssl:                  Byte;
    compress:                 Byte;
    named_pipe:               Byte;
    unused1:                  Byte;
    unused2:                  Byte;
    unused3:                  Byte;
    unused4:                  Byte;
    methods_to_use:           TMySqlOption;
    client_ip:                PAnsiChar;
    secure_auth:              Byte;
    local_infile_init:        Pointer;
    local_infile_read:        Pointer;
    local_infile_end:         Pointer;
    local_infile_error:       Pointer;
    local_infile_userdata:    Pointer;
  end;

  PZMySQLResult = Pointer;
  PZMySQLRow = Pointer;
  PZMySQLRowOffset = Pointer;
  PZMysqlBindArray = Pointer;

{ Enum Field Types from binary_log_types.h }
  PMysqlFieldType = ^TMysqlFieldType;
  TMysqlFieldType = (
    FIELD_TYPE_DECIMAL   = 0,
    FIELD_TYPE_TINY      = 1,
    FIELD_TYPE_SHORT     = 2,
    FIELD_TYPE_LONG      = 3,
    FIELD_TYPE_FLOAT     = 4,
    FIELD_TYPE_DOUBLE    = 5,
    FIELD_TYPE_NULL      = 6,
    FIELD_TYPE_TIMESTAMP = 7,
    FIELD_TYPE_LONGLONG  = 8,
    FIELD_TYPE_INT24     = 9,
    FIELD_TYPE_DATE      = 10,
    FIELD_TYPE_TIME      = 11,
    FIELD_TYPE_DATETIME  = 12,
    FIELD_TYPE_YEAR      = 13,
    FIELD_TYPE_NEWDATE   = 14,
    FIELD_TYPE_VARCHAR   = 15, //<--ADDED by fduenas 20-06-2006
    FIELD_TYPE_BIT       = 16, //<--ADDED by fduenas 20-06-2006
    MYSQL_TYPE_JSON      = 245,
    FIELD_TYPE_NEWDECIMAL = 246, //<--ADDED by fduenas 20-06-2006
    FIELD_TYPE_ENUM      = 247,
    FIELD_TYPE_SET       = 248,
    FIELD_TYPE_TINY_BLOB = 249,
    FIELD_TYPE_MEDIUM_BLOB = 250,
    FIELD_TYPE_LONG_BLOB = 251,
    FIELD_TYPE_BLOB      = 252,
    FIELD_TYPE_VAR_STRING = 253,
    FIELD_TYPE_STRING    = 254,
    FIELD_TYPE_GEOMETRY  = 255);

  { Options for mysql_set_option }
  TMySqlSetOption = (
    MYSQL_OPTION_MULTI_STATEMENTS_ON,
    MYSQL_OPTION_MULTI_STATEMENTS_OFF
  );

  TMysqlStmtAttrType = (
    STMT_ATTR_UPDATE_MAX_LENGTH,
    STMT_ATTR_CURSOR_TYPE,
    STMT_ATTR_PREFETCH_ROWS,
    { mariadb 10.2.7up}
    STMT_ATTR_PREBIND_PARAMS=200,
    STMT_ATTR_ARRAY_SIZE,
    STMT_ATTR_ROW_SIZE
  );

  //http://eclipseclp.org/doc/bips/lib/dbi/cursor_next_execute-3.html
  //"Only one active cursor of type no_cursor is allowed per session,
  //and this active cursor must be closed before another query can be issued to the DBMS server.
  //read_only cursor does not have this restriction,
  //and several such cursors can be active at the same time "
  Tenum_cursor_type = (
    CURSOR_TYPE_NO_CURSOR   = 0,
    CURSOR_TYPE_READ_ONLY   = 1,
    CURSOR_TYPE_FOR_UPDATE  = 2,
    CURSOR_TYPE_SCROLLABLE  = 4
  );
  TMysqlShutdownLevel = (
    SHUTDOWN_DEFAULT = 0,
    SHUTDOWN_WAIT_CONNECTIONS = MYSQL_SHUTDOWN_KILLABLE_CONNECT,
    SHUTDOWN_WAIT_TRANSACTIONS = MYSQL_SHUTDOWN_KILLABLE_TRANS,
    SHUTDOWN_WAIT_UPDATES = MYSQL_SHUTDOWN_KILLABLE_UPDATE,
    SHUTDOWN_WAIT_ALL_BUFFERS = (MYSQL_SHUTDOWN_KILLABLE_UPDATE shl 1),
    SHUTDOWN_WAIT_CRITICAL_BUFFERS,
    KILL_QUERY = 254,
    KILL_CONNECTION = 255
  );

TMYSQL_CLIENT_OPTIONS =
( CLIENT_LONG_PASSWORD,	{  = 1;	   new more secure passwords }
  CLIENT_FOUND_ROWS ,	{	  = 2;	   Found instead of affected rows }
  CLIENT_LONG_FLAG	 ,	{ = 4;	   Get all column flags }
  CLIENT_CONNECT_WITH_DB ,	{ = 8;	   One can specify db on connect }
  CLIENT_NO_SCHEMA	 ,	{  = 16;	   Don't allow database.table.column }
  CLIENT_COMPRESS	 ,	{  = 32;	   Can use compression protcol }
  CLIENT_ODBC		 ,	{  = 64;	   Odbc client }
  CLIENT_LOCAL_FILES	  ,	{ = 128;   Can use LOAD DATA LOCAL }
  CLIENT_IGNORE_SPACE	 ,	{  = 256;   Ignore spaces before '(' }
  CLIENT_CHANGE_USER    ,	{  = 512;   Support the mysql_change_user() }
  CLIENT_INTERACTIVE    ,	{  = 1024;  This is an interactive client }
  CLIENT_SSL     ,	{         = 2048;  Switch to SSL after handshake }
  CLIENT_IGNORE_SIGPIPE  ,	{ = 4096;  IGNORE sigpipes }
  CLIENT_TRANSACTIONS    ,	{ = 8196;  Client knows about transactions }
  CLIENT_RESERVED     ,	{    = 16384;  Old flag for 4.1 protocol  }
  CLIENT_SECURE_CONNECTION  ,	{= 32768;  New 4.1 authentication }
  CLIENT_MULTI_STATEMENTS  ,	{= 65536;  Enable/disable multi-stmt support }
  CLIENT_MULTI_RESULTS  ,	{  = 131072;  Enable/disable multi-results }
  CLIENT_PS_MULTI_RESULTS,  {2^18 = 262144; Enable Multi-results in PS-protocol}
  CLIENT_PLUGIN_AUTH,{2^19 = 524288}
  CLIENT_OPT_20,  {2^20 = 1048576}
  CLIENT_OPT_21,   {2^21 = 2097152 }
  CLIENT_OPT_22,  {2^22 = 4194304}
  CLIENT_OPT_23,  {2^23 = 8388608 }
  CLIENT_OPT_24,   {2^24 = 16777216 }
  CLIENT_OPT_25,   {2^25 = 33554432}
  CLIENT_OPT_26,    {2^26 = 67108864}
  CLIENT_OPT_27,    {2^27 = 134217728}
  CLIENT_OPT_28,    {2^28 = 268435456}
  CLIENT_OPT_29,    {2^29 = 536870912}
  CLIENT_SSL_VERIFY_SERVER_CERT,    {2^30 = 1073741824}
  CLIENT_REMEMBER_OPTIONS	{ = 2147483648; Enable/disable multi-results });

  TMysqlStmtState = (
    MYSQL_STMT_INIT_DONE = 1,
    MYSQL_STMT_PREPARE_DONE,
    MYSQL_STMT_EXECUTE_DONE,
    MYSQL_STMT_FETCH_DONE
  );

  mysql_timestamp_type = (
    MYSQL_TIMESTAMP_NONE = -2,
    MYSQL_TIMESTAMP_ERROR = -1,
    MYSQL_TIMESTAMP_DATE = 0,
    MYSQL_TIMESTAMP_DATETIME = 1,
    MYSQL_TIMESTAMP_TIME = 2
  );

  TMYSQL_TIME = record
    year:                UInt;
    month:               UInt;
    day:                 UInt;
    hour:                UInt;
    minute:              UInt;
    second:              UInt;
    second_part:         ULong;
    neg:                 my_bool;
    time_type:           mysql_timestamp_type;
  end;
  PMYSQL_TIME = ^TMYSQL_TIME;

  PLIST = ^LIST;
  LIST = record
    prev:       PLIST;
    next:       PLIST;
    data:       Pointer;
  end;

  PMYSQL_FIELD = Pointer;

  PMYSQL_FIELD51 = ^TMYSQL_FIELD51;
  TMYSQL_FIELD51 = record
    name:             PAnsiChar;   // Name of column
    org_name:         PAnsiChar;   // Original column name, if an alias
    table:            PAnsiChar;   // Table of column if column was a field
    org_table:        PAnsiChar;   // Org table name if table was an alias
    db:               PAnsiChar;   // Database for table
    catalog:          PAnsiChar;   // Catalog for table
    def:              PAnsiChar;   // Default value (set by mysql_list_fields)
    length:           ULong; // Width of column
    max_length:       ULong; // Max width of selected set
    name_length:      UInt;
    org_name_length:  UInt;
    table_length:     UInt;
    org_table_length: UInt;
    db_length:        UInt;
    catalog_length:   UInt;
    def_length:       UInt;
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
    charsetnr:        UInt; // Character set
    _type:            TMysqlFieldType; // Type of field. Se mysql_com.h for types
    extension:        Pointer //added in 4.1
  end;

  PMYSQL_FIELD41 = ^MYSQL_FIELD41;
  MYSQL_FIELD41 = record
    name:             PAnsiChar; // Name of column
    org_name:         PAnsiChar; // Original column name, if an alias
    table:            PAnsiChar; // Table of column if column was a field
    org_table:        PAnsiChar; // Org table name if table was an alias
    db:               PAnsiChar; // Database for table
    catalog:          PAnsiChar; // Catalog for table
    def:              PAnsiChar; // Default value (set by mysql_list_fields)
    length:           ULong; // Width of column
    max_length:       ULong; // Max width of selected set
    name_length:      UInt;
    org_name_length:  UInt;
    table_length:     UInt;
    org_table_length: UInt;
    db_length:        UInt;
    catalog_length:   UInt;
    def_length:       UInt;
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
    charsetnr:        UInt; // Character set
    _type:            TMysqlFieldType;     // Type of field. Se enum_field_types.
  end;
  PMYSQL_FIELD401 = ^MYSQL_FIELD401;
  MYSQL_FIELD401 = record
    name:             PAnsiChar; // Name of column
    org_name:         PAnsiChar; // Original column name, if an alias
    table:            PAnsiChar; // Table of column if column was a field
    org_table:        PAnsiChar; // Org table name if table was an alias
    db:               PAnsiChar; // Database for table
    def:              PAnsiChar; // Default value (set by mysql_list_fields)
    length:           ULong; // Width of column
    max_length:       ULong; // Max width of selected set
    name_length:      UInt;
    org_name_length:  UInt;
    table_length:     UInt;
    org_table_length: UInt;
    db_length:        UInt;
    def_length:       UInt;
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
    charsetnr:        UInt; // Character set
    _type:            TMysqlFieldType;     // Type of field. Se mysql_com.h for types
  end;
  PMYSQL_FIELD40 = ^MYSQL_FIELD40;
  MYSQL_FIELD40 = record
    name:             PAnsiChar; // Name of column
    table:            PAnsiChar; // Table of column if column was a field
    org_table:        PAnsiChar; // Org table name if table was an alias
    db:               PAnsiChar; // Database for table
    def:              PAnsiChar; // Default value (set by mysql_list_fields)
    length:           ULong; // Width of column
    max_length:       ULong; // Max width of selected set
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
    _type:            TMysqlFieldType;     // Type of field. Se mysql_com.h for types
  end;
  PMYSQL_FIELD32 = ^MYSQL_FIELD32;
  MYSQL_FIELD32 = record
    name:             PAnsiChar; // Name of column
    table:            PAnsiChar; // Table of column if column was a field
    def:              PAnsiChar; // Default value (set by mysql_list_fields)
    _type:            TMysqlFieldType;     // Type of field. Se mysql_com.h for types
    length:           UInt; // Width of column
    max_length:       UInt; // Max width of selected set
    flags:            UInt; // Div flags
    decimals:         UInt; // Number of decimals in field
  end;

  // offsets to used MYSQL_FIELDxx members.
  // a negative entry means the field does not exits in the record
  PMYSQL_FIELDOFFSETS = ^TMYSQL_FIELDOFFSETS;
  TMYSQL_FIELDOFFSETS = record
    name            : NativeUInt;
    name_length     : NativeInt;
    org_table       : NativeInt;
    org_table_length: NativeInt;
    org_name        : NativeInt;
    org_name_length : NativeInt;
    db              : NativeInt;
    db_length       : NativeInt;
    charsetnr       : NativeInt;
    _type           : NativeUInt;
    flags           : NativeUInt;
    length          : NativeUInt;
    decimals        : NativeUInt;
  end;

  PMYSQL_BIND041 = ^TMYSQL_BIND041;
  TMYSQL_BIND041 = record
    length: PLongWord;              // output length pointer
    is_null: Pmy_bool;              // Pointer to null indicators
    buffer: PByte;                  // buffer to get/put data
    buffer_type: TMysqlFieldType;  // buffer type
    buffer_length: LongWord;        // buffer length
    param_number: LongWord;         // For null count and error messages
    long_data_used: my_bool;        // If used with mysql_send_long_data
  end;

  PMYSQL_BIND411 = ^TMYSQL_BIND411;
  TMYSQL_BIND411 =  record
    // 4.1.22 definition
    length:           PULong;
    is_null:          Pmy_bool;
    buffer:           Pointer;
    buffer_type:      TMysqlFieldType;
    buffer_length:    ULong;
    //internal fields
    inter_buffer:     PByte;
    offset:           ULong;
    internal_length:  ULong;
    param_number:     UInt;
    pack_length:      UInt;
    is_unsigned:      my_bool;
    long_data_used:   my_bool;
    internal_is_null: my_bool;
    store_param_func: Pointer;
    fetch_result:     Pointer;
    skip_result:      Pointer;
  end;

  PMYSQL_BIND506 = ^TMYSQL_BIND506;
  TMYSQL_BIND506 =  record
    // 5.0.67 up definition
    length:            PULong;
    is_null:           Pmy_bool;
    buffer:            Pointer;
    error:             PByte;
    buffer_type:       TMysqlFieldType;
    buffer_length:     ULong;
    row_ptr:           PByte;
    offset:            ULong;
    length_value:      ULong;
    param_number:      ULong;
    pack_length:       ULong;
    error_value:       my_bool;
    is_unsigned:       my_bool;
    long_data_used:    my_bool;
    is_null_value:     my_bool;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
  end;

  PMYSQL_BIND51 = ^TMYSQL_BIND51;
  TMYSQL_BIND51 =  record
    // 5.1.30 up and 6.x definition
    length:            PULong;
    is_null:           Pmy_bool;
    buffer:            Pointer;
    error:             Pmy_bool;
    row_ptr:           PByte;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
    buffer_length:     ULong;
    offset:            ULong;
    length_value:      ULong;
    param_number:      UInt;
    pack_length:       UInt;
    buffer_type:       TMysqlFieldType;
    error_value:       my_bool;
    is_unsigned:       my_bool;
    long_data_used:    my_bool;
    is_null_value:     my_bool;
    extension:         Pointer;
  end;

  PMARIADB_BIND1027 = ^TMARIADB_BIND1027;
  TMARIADB_BIND1027 =  record
    // MariaDB 10.2.7 up
    length:            PULong;
    is_null:           Pmy_bool;
    buffer:            Pointer;
    error:             Pmy_bool;
    u:                 record
                          case Boolean of
                          False: (row_ptr: PByte);
                          True: (indicator: PShortInt);
                        end;
    store_param_funct: Pointer;
    fetch_result:      Pointer;
    skip_result:       Pointer;
    buffer_length:     ULong;
    offset:            ULong;
    length_value:      ULong;
    param_number:      UInt;
    pack_length:       UInt;
    buffer_type:       TMysqlFieldType;
    error_value:       my_bool;
    is_unsigned:       my_bool;
    long_data_used:    my_bool;
    is_null_value:     my_bool;
    extension:         Pointer;
  end;

  // offsets to used MYSQL_BINDxx members. Filled by GetBindOffsets
  TMYSQL_BINDOFFSETS = record
    buffer_type   :NativeUint;
    buffer_length :NativeUint;
    is_unsigned   :NativeUint;
    buffer        :NativeUint;
    length        :NativeUint;
    is_null       :NativeUint;
    size          :word;    //size of MYSQL_BINDxx
  end;

  PULongArray = ^TULongArray;
  TULongArray = array[0..High(Byte)] of Ulong; //http://dev.mysql.com/doc/refman/4.1/en/column-count-limit.html

  Pmy_bool_array = ^Tmy_bool_array;
  Tmy_bool_array = array[0..High(Byte)] of my_bool; //just 4 debugging

  Tmysql_indicator_type =(
    STMT_INDICATOR_NTS=-1,      //String is null terminated
    STMT_INDICATOR_NONE=0,      //No semantics
    STMT_INDICATOR_NULL=1,      //NULL value
    STMT_INDICATOR_DEFAULT=2,   //Use columns default value
    STMT_INDICATOR_IGNORE=3,    //Skip update of column
    STMT_INDICATOR_IGNORE_ROW=4 //Skip update of row
  );
  Pmysql_indicator_types = ^Tmysql_indicator_types;
  Tmysql_indicator_types = array[0..High(Byte)] of Tmysql_indicator_type;

  PDOBindRecord2 = ^TDOBindRecord2;
  TDOBindRecord2 = record
    buffer:                 Array of Byte; //data place holder
    buffer_address:         PPointer; //we don't need reserved mem in all case, but we need to set the address
    buffer_length_address:  PULong; //set buffer_Length on the fly e.g. lob reading!
    buffer_type:            TMysqlFieldType; //save exact type
    buffer_type_address:    PMysqlFieldType;
    length:                 ULong; //current length of our or retrieved data
    is_null:                Byte; //null indicator
    binary:                 Boolean; //binary field or not? Just for reading!
    is_signed:              Boolean; //signed ordinals or not? Just for reading!
    mysql_bind:             Pointer; //Save exact address of bind for lob reading
    decimals:               Integer;
  end;

  PPMYSQL = ^PMYSQL;
  PMYSQL  = pointer;

  PMY_CHARSET_INFO = ^MY_CHARSET_INFO;
  MY_CHARSET_INFO = record
    number:         UInt;
    state:          UInt;
    csname:         PAnsiChar;
    name:           PAnsiChar;
    comment:        PAnsiChar;
    dir:            PAnsiChar;
    mbminlen:       UInt;
    mbmaxlen:       UInt;
  end;
  // Structure of the MYSQL_RES record isn't used anymore.
  // Access to the fields should be done using library functions
  // Reason : the structure of these records tend to change now and then.
  PMYSQL_RES = Pointer;

  PREP_STMT_STATE=(
    MY_ST_UNKNOWN,
    MY_ST_PREPARE,
    MY_ST_EXECUTE);

  PPMYSQL_STMT = ^PMYSQL_STMT;
  PMYSQL_STMT = Pointer;

  /// <summary>
  ///   Enum for specifying a MySQL fork. Possible values:
  ///   fUnknown, fMySQL, fMariaDB, fSphinx, fPercona, fDrizzle, WebScaleSQL, OurDelta
  /// </summary>
  TMySQLFork = (fUnknown, fMySQL, fMariaDB, fSphinx, fPercona, fDrizzle, WebScaleSQL, OurDelta);

const
  MySQLForkName: array[TMySQLFork] of String = ('Unknown', 'MySQL', 'MariaDB',
    'Sphinx', 'Percona', 'Drizzle', 'WebScaleSQL', 'OurDelta');
  EMBEDDED_DEFAULT_DATA_DIR = {$IFDEF WINDOWS}'.\data\'{$ELSE}'./data/'{$ENDIF};
  SERVER_ARGUMENTS_KEY_PREFIX = 'ServerArgument';
  SERVER_GROUPS : array [0..2] of PAnsiChar = ('embedded'#0, 'server'#0, nil);

  DEFAULT_PARAMS : array [0..2] of PAnsiChar = ('not_used'#0,
                                            '--datadir='+EMBEDDED_DEFAULT_DATA_DIR+#0,
                                            '--set-variable=key_buffer_size=32M'#0);

const
  MaxBlobSize = 1000000;


{** offet of MYSSQL.server_status field:
  The struct of the record tends to change to often and we don't need all the
  definitions

  Value := NativeUInt(@(MYSQLx(Nil).server_status
}
  MYSQL5up_server_status_offset: NativeUInt = 748;
  MYSQL41_server_status_offset: NativeUInt = 436;
  MYSQL323_server_status_offset: NativeUInt = 328;

  //mysql_com.h
  SERVER_PS_OUT_PARAMS = LongWord(4096); //To mark ResultSet containing output parameter values.
  SERVER_MORE_RESULTS_EXIST = LongWord(8); //Multi query - next query exists

{$ENDIF ZEOS_DISABLE_MYSQL}

implementation


end.
