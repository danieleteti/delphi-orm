{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Native Plain Drivers for PostgreSQL           }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZPlainPostgreSqlDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL}

uses {$IFDEF OLDFPC}ZClasses, {$ENDIF}ZCompatibility, ZPlainDriver;

const
  WINDOWS_DLL_LOCATION   = 'libpq.dll';
  WINDOWS_DLL7_LOCATION   = 'libpq74.dll';
  WINDOWS_DLL8_LOCATION   = 'libpq81.dll';
  LINUX_DLL_LOCATION   = 'libpq'+SharedSuffix;
  LINUX_DLL8_LOCATION  = 'libpq'+SharedSuffix+'.4';
  LINUX_DLL82_LOCATION = 'libpq'+SharedSuffix+'.5';
  LINUX_DLL9_LOCATION = LINUX_DLL82_LOCATION;

{ Type Lengths }
  NAMEDATALEN  = 32;

{ OIDNAMELEN should be set to NAMEDATALEN + sizeof(Oid) }
  OIDNAMELEN   = 36;
  InvalidOid   = 0;

  INV_WRITE    = $00020000;
  INV_READ     = $00040000;

  BLOB_SEEK_SET     = 0;
  BLOB_SEEK_CUR     = 1;
  BLOB_SEEK_END     = 2;

{ PostgreSQL basic type OIDs. These OIDs are hard coded in PostgreSQL and according to the following mail never change:
  https://www.postgresql.org/message-id/AANLkTimiNjQa7ws1tyR_W6RQPec6RlxQtWfACNMnZ_1P@mail.gmail.com
  
  From: 	Merlin Moncure <mmoncure(at)gmail(dot)com>
  To: 	zhong ming wu <mr(dot)z(dot)m(dot)wu(at)gmail(dot)com>
  Cc: 	pgsql-general <pgsql-general(at)postgresql(dot)org>
  Subject: 	Re: oid data types mapping in libpq functions
  Date: 	2010-06-17 14:32:01
  Message-ID: 	AANLkTimiNjQa7ws1tyR_W6RQPec6RlxQtWfACNMnZ_1P@mail.gmail.com (view raw or download thread mbox)

  On Wed, Jun 16, 2010 at 10:42 PM, zhong ming wu <mr(dot)z(dot)m(dot)wu(at)gmail(dot)com> wrote:
  > Dear List
  >
  > Where can I find this mapping of oid to pg data types mentioned in
  > libpq documentation?
  > Why is such information not mentioned in the documentation?  A general
  > knowledge?

  curious: what do you need the oids for?

  built in type oids are defined in pg_type.h:
  cat src/include/catalog/pg_type.h | grep OID | grep define

  built in type oids don't change. you can pretty much copy/pasto the
  output of above into an app...just watch out for some types that may
  not be in older versions.

  user defined type oids (tables, views, composite types, enums, and
  domains) have an oid generated when it is created.  since that oid can
  change via ddl so you should look it up by name at appropriate times.

  if you want to be completely abstracted from the type oids, look here:
  http://libpqtypes.esilo.com/

  merlin

  Ok - this information really should be somewhere else, in the developer documentation but I don't want to lose it for now. We can remove it later on.
  
  So what follows is a list of PG hard coded OIDs. All other OIDs must be converted
  by a separate function to determine their type name.
}

{--------------------------------------------------------------------------------------}

{ types as found in pg_type.h in PostgreSQL 9.6 }

  { OIDS 1 - 99 }
  BOOLOID			  = 16;
  BYTEAOID			= 17;
  CHAROID			  = 18;
  NAMEOID			  = 19;
  INT8OID			  = 20;
  INT2OID			  = 21;
  INT2VECTOROID	= 22;
  INT4OID			  = 23;
  REGPROCOID		= 24;
  TEXTOID			  = 25;
  OIDOID			  = 26;
  TIDOID			  = 27;
  XIDOID 			  = 28;
  CIDOID 			  = 29;
  OIDVECTOROID	= 30;

  { OIDS 100 - 199 }
  JSONOID 			= 114;
  XMLOID 			  = 142;
  PGNODETREEOID	= 194;
  PGDDLCOMMANDOID = 32;

  { OIDS 200 - 299 }

  { OIDS 300 - 399 }

  { OIDS 400 - 499 }

  { OIDS 500 - 599 }

  { OIDS 600 - 699 }
  POINTOID		= 600;
  LSEGOID			= 601;
  PATHOID			= 602;
  BOXOID			= 603;
  POLYGONOID	= 604;
  LINEOID			= 628;

  { OIDS 700 - 799 }

  FLOAT4OID 	= 700;
  FLOAT8OID 	= 701;
  ABSTIMEOID	= 702;
  RELTIMEOID	= 703;
  TINTERVALOID = 704;
  UNKNOWNOID	= 705;

  CIRCLEOID		= 718;
  CASHOID 		= 790;

  { OIDS 800 - 899 }
  MACADDROID 	= 829;
  INETOID 		= 869;
  CIDROID 		= 650;

  { OIDS 900 - 999 }

  { OIDS 1000 - 1099 }
  INT2ARRAYOID	= 1005;
  INT4ARRAYOID	= 1007;
  TEXTARRAYOID	= 1009;
  OIDARRAYOID	  = 1028;
  FLOAT4ARRAYOID 	= 1021;
  ACLITEMOID		= 1033;
  CSTRINGARRAYOID	= 1263;
  BPCHAROID		  = 1042;
  VARCHAROID		= 1043;
  DATEOID			  = 1082;
  TIMEOID			  = 1083;

  { OIDS 1100 - 1199 }
  TIMESTAMPOID	= 1114;
  TIMESTAMPTZOID	= 1184;
  INTERVALOID		= 1186;

  { OIDS 1200 - 1299 }
  TIMETZOID		  = 1266;

  { OIDS 1500 - 1599 }
  BITOID	 		  = 1560;
  VARBITOID		  = 1562;

  { OIDS 1600 - 1699 }

  { OIDS 1700 - 1799 }
  NUMERICOID		= 1700;
  REFCURSOROID	= 1790;

  { OIDS 2200 - 2299 }
  REGPROCEDUREOID = 2202;
  REGOPEROID		  = 2203;
  REGOPERATOROID	= 2204;
  REGCLASSOID		  = 2205;
  REGTYPEOID		  = 2206;
  REGROLEOID		  = 4096;
  REGNAMESPACEOID	= 4089;
  REGTYPEARRAYOID = 2211;

  { uuid }
  UUIDOID 		    = 2950;

  { pg_lsn }
  LSNOID			    = 3220;

  { text search }
  TSVECTOROID		  = 3614;
  GTSVECTOROID	  = 3642;
  TSQUERYOID		  = 3615;
  REGCONFIGOID	  = 3734;
  REGDICTIONARYOID = 3769;

  { jsonb }
  JSONBOID 		    = 3802;

  { range types }
  INT4RANGEOID	  = 3904;

{
 * pseudo-types
 *
 * types with typtype='p' represent various special cases in the type system.
 *
 * These cannot be used to define table columns, but are valid as function
 * argument and result types (if supported by the function's implementation
 * language).
 *
 * Note: cstring is a borderline case; it is still considered a pseudo-type,
 * but there is now support for it in records and arrays.  Perhaps we should
 * just treat it as a regular base type?
}
  RECORDOID		    = 2249;
  RECORDARRAYOID	= 2287;
  CSTRINGOID		  = 2275;
  ANYOID			    = 2276;
  ANYARRAYOID		  = 2277;
  VOIDOID			    = 2278;
  TRIGGEROID		  = 2279;
  EVTTRIGGEROID	  = 3838;
  LANGUAGE_HANDLEROID	= 2280;
  INTERNALOID		  = 2281;
  OPAQUEOID		    = 2282;
  ANYELEMENTOID	  = 2283;
  ANYNONARRAYOID	= 2776;
  ANYENUMOID		  = 3500;
  FDW_HANDLEROID	= 3115;
  INDEX_AM_HANDLEROID = 325;
  TSM_HANDLEROID	= 3310;
  ANYRANGEOID		  = 3831;

  { macros }
  TYPTYPE_BASE		  = 'b'; { base type (ordinary scalar type) }
  TYPTYPE_COMPOSITE	= 'c'; { composite (e.g., table's rowtype) }
  TYPTYPE_DOMAIN		= 'd'; { domain over another type }
  TYPTYPE_ENUM		  = 'e'; { enumerated type }
  TYPTYPE_PSEUDO		= 'p'; { pseudo-type }
  TYPTYPE_RANGE		  = 'r'; { range type }

  TYPCATEGORY_INVALID	  = #0;	{ not an allowed category }
  TYPCATEGORY_ARRAY		  = 'A';
  TYPCATEGORY_BOOLEAN	  = 'B';
  TYPCATEGORY_COMPOSITE	= 'C';
  TYPCATEGORY_DATETIME	= 'D';
  TYPCATEGORY_ENUM		  = 'E';
  TYPCATEGORY_GEOMETRIC	= 'G';
  TYPCATEGORY_NETWORK	  = 'I';		{ think INET }
  TYPCATEGORY_NUMERIC	  = 'N';
  TYPCATEGORY_PSEUDOTYPE = 'P';
  TYPCATEGORY_RANGE		  = 'R';
  TYPCATEGORY_STRING		= 'S';
  TYPCATEGORY_TIMESPAN	= 'T';
  TYPCATEGORY_USER		  = 'U';
  TYPCATEGORY_BITSTRING	= 'V';		{ er ... "varbit"? }
  TYPCATEGORY_UNKNOWN	  = 'X';

//some error codes
  indeterminate_datatype: PAnsiChar = '42P18';
  current_transaction_is_aborted: PAnsiChar = '25P02';
{------------------------------------------------------------------------------------------}


  
type

{ Application-visible enum types }
  TZPostgreSQLConnectStatusType = (
    CONNECTION_OK,
    CONNECTION_BAD
  );

  TZPostgreSQLFieldCode=( // FirmOS
            PG_DIAG_SEVERITY=ord('S'),
            PG_DIAG_SQLSTATE=ord('C'){%H-},
            PG_DIAG_MESSAGE_PRIMARY=ord('M'),
            PG_DIAG_MESSAGE_DETAIL=ord('D'),
            PG_DIAG_MESSAGE_HINT=ord('H'),
            PG_DIAG_STATEMENT_POSITION=ord('P'),
            PG_DIAG_INTERNAL_POSITION=ord('p'),
            PG_DIAG_INTERNAL_QUERY=ord('q'),
            PG_DIAG_CONTEXT=ord('W'),
            PG_DIAG_SOURCE_FILE=ord('F'),
            PG_DIAG_SOURCE_LINE=ord('L'),
            PG_DIAG_SOURCE_FUNCTION=ord('R')
            );

  TZPostgreSQLExecStatusType = (
    PGRES_EMPTY_QUERY,
    PGRES_COMMAND_OK,		  { a query command that doesn't return
                            anything was executed properly by the backend }
    PGRES_TUPLES_OK,		  { a query command that returns tuples
				                    was executed properly by the backend,
				                    PGresult contains the result tuples }
    PGRES_COPY_OUT,		    { Copy Out data transfer in progress }
    PGRES_COPY_IN,		    { Copy In data transfer in progress }
    PGRES_BAD_RESPONSE,	  { an unexpected response was recv'd from the backend }
    PGRES_NONFATAL_ERROR, { notice or warning message }
    PGRES_FATAL_ERROR,    { query failed }
    PGRES_COPY_BOTH,		  { Copy In/Out data transfer in progress }
    PGRES_SINGLE_TUPLE    { since 9.2 single tuple from larger resultset }
  );

{ PGnotify represents the occurrence of a NOTIFY message.
  Ideally this would be an opaque typedef, but it's so simple that it's
  unlikely to change.
  NOTE: in Postgres 6.4 and later, the be_pid is the notifying backend's,
  whereas in earlier versions it was always your own backend's PID.
}
  TZPostgreSQLNotify = {packed }record //the reocord is NOT packet
    relname: PAnsiChar;   { name of relation containing data }
    be_pid:  Integer; { process id of backend }
    payload: PAnsiChar; {additional data in notify}
  end;

  PZPostgreSQLNotify = ^TZPostgreSQLNotify;

{ PQnoticeProcessor is the function type for the notice-message callback. }

  TZPostgreSQLNoticeProcessor = procedure(arg: Pointer; message: PAnsiChar); cdecl;

{ Structure for the conninfo parameter definitions returned by PQconndefaults }

  TZPostgreSQLConnectInfoOption = packed record
    keyword:  PAnsiChar;	{ The keyword of the option }
    envvar:   PAnsiChar;	{ Fallback environment variable name }
    compiled: PAnsiChar;	{ Fallback compiled in default value  }
    val:      PAnsiChar;	{ Options value	}
    lab:      PAnsiChar;	{ Label for field in connect dialog }
    disPAnsiChar: PAnsiChar;	{ Character to display for this field
			  in a connect dialog. Values are:
			  ""	Display entered value as is
			  "*"	Password field - hide value
			  "D"	Debug options - don't
			  create a field by default }
    dispsize: Integer;	{ Field size in characters for dialog }
  end;

  PZPostgreSQLConnectInfoOption = ^TZPostgreSQLConnectInfoOption;

{ PQArgBlock -- structure for PQfn() arguments }

  TZPostgreSQLArgBlock = packed record
    len:     Integer;
    isint:   Integer;
    case u: Boolean of
      True:  (ptr: PInteger);	{ can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PZPostgreSQLArgBlock = ^TZPostgreSQLArgBlock;

  PZPostgreSQLConnect = Pointer;
  PZPostgreSQLResult = Pointer;
  PZPostgreSQLCancel = Pointer;
  POid = ^Oid;
  Oid = Cardinal;

TZPgCharactersetType = (
	csSQL_ASCII = 0,	{ SQL/ASCII }
	csEUC_JP,	{ EUC for Japanese }
	csEUC_CN,	{ EUC for Chinese }
	csEUC_KR,	{ EUC for Korean }
	csEUC_TW,	{ EUC for Taiwan }
  csEUC_JIS_2004, {Extended UNIX Code-JP, JIS X 0213 	Japanese}
	csUTF8,		{ Unicode UTF-8 }
	csMULE_INTERNAL,	{ Mule internal code }
	csLATIN1,	{ ISO-8859 Latin 1 }
	csLATIN2,	{ ISO-8859 Latin 2 }
	csLATIN3,	{ ISO-8859 Latin 3 }
	csLATIN4,	{ ISO-8859 Latin 4 }
	csLATIN5,	{ ISO-8859 Latin 5 }
	csLATIN6,	{ ISO-8859 Latin 6 }
	csLATIN7,	{ ISO-8859 Latin 7 }
	csLATIN8,	{ ISO-8859 Latin 8 }
	csLATIN9,	{ ISO-8859 Latin 9 }
	csLATIN10,	{ ISO-8859 Latin 10 }
	csWIN1256,	{ Arabic Windows }
	csWIN1258,	{ Vietnamese Windows }
	csWIN866,	{ Alternativny Variant (MS-DOS CP866) }
	csWIN874,	{ Thai Windows }
  csKOI8, {KOI8-R(U) 	Cyrillic}
	csKOI8R,	{ KOI8-R 	Cyrillic (Russian) }
	csWIN1251,	{ windows-1251 }
  csWIN1252, {Windows CP1252 	Western European}
	csISO_8859_5,	{ ISO-8859-5 }
	csISO_8859_6,	{ ISO-8859-6 }
	csISO_8859_7,	{ ISO-8859-7 }
	csISO_8859_8,	{ ISO-8859-8 }
  csWIN1250, { Windows CP1250 	Central European }
  csWIN1253, { Windows CP1253 	Greek }
  csWIN1254, { Windows CP1254 	Turkish }
  csWIN1255, { Windows CP1255 	Hebrew }
  csWIN1257, { Windows CP1257 	Baltic }
	csKOI8U,	{ KOI8-R 	Cyrillic (Ukrainian) }
	csSJIS,		{ Shift JIS 	Japanese }
	csBIG5,		{ Big Five 	Traditional Chinese }
	csGBK,		{ Extended National Standard 	Simplified Chinese }
	csUHC,		{ Unified Hangul Code 	Korean }
	csGB18030,	{ National Standard 	Chinese }
	csJOHAB,  {JOHAB 	Korean (Hangul)}
  csSHIFT_JIS_2004, {Shift JIS, JIS X 0213 	Japanese}
	csUNICODE_PODBC,{ UNICODE ( < Ver8.1). Can't call it UNICODE as that's already used }
	csTCVN,		{ TCVN ( < Ver8.1) }
	csALT,		{ ALT ( < Var8.1) }
	csWIN,		{ WIN ( < Ver8.1) }
	csOTHER
);

{ ****************** Plain API Types definition ***************** }

type
{ String descriptions of the ExecStatusTypes }
  pgresStatus = array[$00..$ff] of PAnsiChar;

{ PGconn encapsulates a connection to the backend.
  The contents of this struct are not supposed to be known to applications.
}
  PGconn = Pointer;
  PPGconn = Pointer;

{ PGresult encapsulates the result of a query (or more precisely, of a single
  SQL command --- a query string given to PQsendQuery can contain multiple
  commands and thus return multiple PGresult objects).
  The contents of this struct are not supposed to be known to applications.
}
  PGresult = Pointer;
  PPGresult = Pointer;
  PGCancel = Pointer;

{ PQnoticeProcessor is the function type for the notice-message callback. }

  PQnoticeProcessor = procedure(arg: Pointer; message: PAnsiChar); cdecl;

{ Print options for PQprint() }

{
  We can't use the conventional "bool", because we are designed to be
  included in a user's program, and user may already have that type
  defined.  Pqbool, on the other hand, is unlikely to be used.
}

  PPAnsiChar = array[00..$ff] of PAnsiChar;

  PQprintOpt = packed record
    header:    Byte;	   { print output field headings and row count }
    align:     Byte;	   { fill align the fields }
    standard:  Byte;	   { old brain dead format }
    html3:     Byte;	   { output html tables }
    expanded:  Byte;	   { expand tables }
    pager:     Byte;	   { use pager for output if needed }
    fieldSep:  PAnsiChar;	   { field separator }
    tableOpt:  PAnsiChar;      { insert to HTML <table ...> }
    caption:   PAnsiChar;	   { HTML <caption> }
    fieldName: PPAnsiChar; 	   { null terminated array of repalcement field names }
  end;

  PPQprintOpt = ^PQprintOpt;

{ ----------------
  Structure for the conninfo parameter definitions returned by PQconndefaults
  ----------------
}
  PQconninfoOption = packed record
    keyword:  PAnsiChar;	{ The keyword of the option }
    envvar:   PAnsiChar;	{ Fallback environment variable name }
    compiled: PAnsiChar;	{ Fallback compiled in default value  }
    val:      PAnsiChar;	{ Options value	}
    lab:      PAnsiChar;	{ Label for field in connect dialog }
    disPAnsiChar: PAnsiChar;	{ Character to display for this field
			  in a connect dialog. Values are:
			  ""	Display entered value as is
			  "*"	Password field - hide value
			  "D"	Debug options - don't
			  create a field by default }
    dispsize: Integer;	{ Field size in characters for dialog }
  end;

  PPQConninfoOption = ^PQconninfoOption;

{ ----------------
  PQArgBlock -- structure for PQfn() arguments
  ----------------
}
  PQArgBlock = packed record
    len:     Integer;
    isint:   Integer;
    case u: Boolean of
      True:  (ptr: PInteger);	{ can't use void (dec compiler barfs)	 }
      False: (_int: Integer);
  end;

  PPQArgBlock = ^PQArgBlock;

{Prepared statement types}
  TPQparamTypes = {array of }POid;
  TPQparamValues = array of PAnsichar;
  TPQparamLengths = array of Integer;
  TPQparamFormats = array of Integer;


{ ************** Plain API Function types definition ************* }
{ ===	in fe-connect.c === }
  TPQconnectdb     = function(ConnInfo: PAnsiChar): PPGconn; cdecl; // FirmOS 8.1 OK
  TPQsetdbLogin    = function(Host, Port, Options, Tty, Db, User, Passwd: PAnsiChar): PPGconn; cdecl; // FirmOS 8.1 OK
//15022006 FirmOS: omitting   PQconnectStart
//15022006 FirmOS: omitting  PQconnectPoll
  TPQconndefaults  = function: PPQconninfoOption; cdecl;
  TPQfinish        = procedure(Handle: PPGconn); cdecl;
  TPQreset         = procedure(Handle: PPGconn); cdecl;
//15022006 FirmOS: omitting PQresetStart
//15022006 FirmOS: omitting PQresetPoll
  TPQrequestCancel = function(Handle: PPGconn): Integer; cdecl;
  TPQdb            = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQuser          = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQpass          = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQhost          = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQport          = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQtty           = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQoptions       = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQstatus        = function(Handle: PPGconn): TZPostgreSQLConnectStatusType; cdecl;
//TBD  PGTransactionStatusType PQtransactionStatus(const PGconn *conn);
//15022006 FirmOS: omitting const char *PQparameterStatus(const PGconn *conn, const char *paramName);
//15022006 FirmOS: omitting  PQprotocolVersion
  TPQserverVersion = function(Handle: PPGconn): Integer; cdecl;
  TPQerrorMessage  = function(Handle: PPGconn): PAnsiChar; cdecl;
  TPQsocket        = function(Handle: PPGconn): Integer; cdecl;
  TPQbackendPID    = function(Handle: PPGconn): Integer; cdecl;
//15022006 FirmOS: omitting  SSL *PQgetssl(const PGconn *conn);
  TPQtrace         = procedure(Handle: PPGconn; DebugPort: Pointer); cdecl;
  TPQuntrace       = procedure(Handle: PPGconn); cdecl;
  TPQsetNoticeProcessor = procedure(Handle: PPGconn; Proc: PQnoticeProcessor; Arg: Pointer); cdecl;

  TPQclientEncoding = function(Handle: PPGconn): Integer; cdecl; //EgonHugeist
{ === in fe-exec.c === }
//* Simple synchronous query */
  TPQexec          = function(Handle: PPGconn; Query: PAnsiChar): PPGresult; cdecl;
  TPQexecParams    = function(Handle: PPGconn; command: PAnsichar;
        nParams: Integer; paramTypes: TPQparamTypes; paramValues: PPointer;
        paramLengths: PInteger; paramFormats: PInteger;
        resultFormat: Integer): PPGresult; cdecl;
  TPQprepare        = function(Handle: PPGconn; stmtName: PAnsichar;
        query: PAnsiChar; nParams: Integer; paramTypes: PInteger): PPGresult; cdecl;
  TPQexecPrepared   = function(Handle: PPGconn; stmtName: PAnsichar;
        nParams: Integer; paramValues: PPointer; paramLengths: PInteger;
        paramFormats: PInteger; resultFormat: Integer): PPGresult; cdecl;
//* Interface for multiple-result or asynchronous queries */
  TPQsendQuery      = function(Handle: PPGconn; query: PAnsiChar): Integer; cdecl;
  TPQsendQueryParams= function(Handle: PPGconn; command: PAnsichar;
        nParams: Integer; paramTypes: PInteger; paramValues: PPointer;
        paramLengths: PInteger; paramFormats: PInteger;
        resultFormat: Integer): Integer; cdecl;
  TPQsendPrepare    = function(Handle: PPGconn; stmtName: PAnsichar;
        query: PAnsiChar; nParams: Integer; paramTypes: TPQparamTypes): Integer; cdecl;
  TPQsendQueryPrepared = function(Handle: PPGconn; stmtName: PAnsichar;
         nParams: Integer; paramValues: PPointer; paramLengths: PInteger;
         paramFormats: PInteger; resultFormat: Integer): Integer; cdecl;
  TPQgetResult     = function(Handle: PPGconn): PPGresult;  cdecl;
  TPQsetSingleRowMode = function(Handle: PPGconn): Integer; cdecl;
//* Describe prepared statements and portals */
  TPQdescribePrepared = function(Handle: PPGconn; const stmt: PAnsiChar): PPGresult; cdecl;
  TPQdescribePortal = function(Handle: PPGconn; const portal: PAnsiChar): PPGresult; cdecl;
  TPQsendDescribePrepared = function(Handle: PPGconn; const stmt: PAnsiChar): Integer; cdecl;
  TPQsendDescribePortal = function(Handle: PPGconn; const portal: PAnsiChar): Integer; cdecl;

  TPQnotifies      = function(Handle: PPGconn): PZPostgreSQLNotify; cdecl;
  TPQfreeNotify    = procedure(Handle: PZPostgreSQLNotify);cdecl;
  TPQisBusy        = function(Handle: PPGconn): Integer; cdecl;
  TPQconsumeInput  = function(Handle: PPGconn): Integer; cdecl;
  TPQgetCancel     = function(Handle: PPGconn): PGcancel; cdecl;
  TPQfreeCancel    = procedure(Canc: PGcancel); cdecl;
  TPQcancel        = function(Canc: PGcancel; Buffer: PChar; BufSize: Integer): Integer;
  TPQgetline       = function(Handle: PPGconn; Str: PAnsiChar; length: Integer): Integer; cdecl;
  TPQputline       = function(Handle: PPGconn; Str: PAnsiChar): Integer; cdecl;
  TPQgetlineAsync  = function(Handle: PPGconn; Buffer: PAnsiChar; BufSize: Integer): Integer; cdecl;
  TPQputnbytes     = function(Handle: PPGconn; Buffer: PAnsiChar; NBytes: Integer): Integer; cdecl;
  TPQendcopy       = function(Handle: PPGconn): Integer; cdecl;
  TPQfn            = function(Handle: PPGconn; fnid: Integer; result_buf, result_len: PInteger; result_is_int: Integer; args: PPQArgBlock; nargs: Integer): PPGresult; cdecl;
  TPQresultStatus  = function(Result: PPGresult): TZPostgreSQLExecStatusType; cdecl;
  TPQresultErrorMessage = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQresultErrorField=function(result: PPGResult; fieldcode:integer):PAnsiChar;cdecl; // postgresql 8
  TPQntuples       = function(Result: PPGresult): Integer; cdecl;
  TPQnfields       = function(Result: PPGresult): Integer; cdecl;
  TPQbinaryTuples  = function(Result: PPGresult): Integer; cdecl;
  TPQfname         = function(Result: PPGresult; field_num: Integer): PAnsiChar; cdecl;
  TPQfnumber       = function(Result: PPGresult; field_name: PAnsiChar): Integer; cdecl;
  TPQftable        = function(Result: PPGresult; field_num: Integer): Oid; cdecl;
  TPQftablecol     = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQftype         = function(Result: PPGresult; field_num: Integer): Oid; cdecl;
  TPQfsize         = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQfmod          = function(Result: PPGresult; field_num: Integer): Integer; cdecl;
  TPQcmdStatus     = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQoidValue      = function(Result: PPGresult): Oid; cdecl;
  TPQoidStatus     = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQcmdTuples     = function(Result: PPGresult): PAnsiChar; cdecl;
  TPQgetvalue      = function(Result: PPGresult; tup_num, field_num: Integer): PAnsiChar; cdecl;
  TPQgetlength     = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQgetisnull     = function(Result: PPGresult; tup_num, field_num: Integer): Integer; cdecl;
  TPQclear         = procedure(Result: PPGresult); cdecl;
  TPQmakeEmptyPGresult  = function(Handle: PPGconn; status: TZPostgreSQLExecStatusType): PPGresult; cdecl;
  //* Quoting strings before inclusion in queries. */
  // postgresql 8
  TPQescapeStringConn = function(Handle: PGconn; ToChar: PAnsiChar;
    const FromChar: PAnsiChar; length: NativeUInt; error: PInteger): NativeUInt;cdecl; //7.3
  TPQescapeLiteral    = function(Handle: PGconn; const str: PAnsiChar; len: NativeUInt): PAnsiChar;cdecl;
  TPQescapeIdentifier = function(Handle: PGconn; const str: PAnsiChar; len: NativeUInt): PAnsiChar;cdecl; //7.3
  TPQescapeByteaConn  = function(Handle: PPGconn;const from:PAnsiChar;from_length:longword;to_lenght:PLongword):PAnsiChar;cdecl;
  TPQunescapeBytea    = function(const from:PAnsiChar;to_lenght:PLongword):PAnsiChar;cdecl;
  TPQFreemem          = procedure(ptr:Pointer);cdecl;

  //* These forms are deprecated! */
  TPQescapeString     = function(ToChar: PAnsiChar; const FormChar: PAnsiChar; length: NativeUInt): NativeUInt;cdecl; //7.2
  TPQescapeBytea      = function(const from:PAnsiChar;from_length:longword;to_lenght:PLongword):PAnsiChar;cdecl; //7.2

  { === in fe-lobj.c === }
  Tlo_open         = function(Handle: PPGconn; lobjId: Oid; mode: Integer): Integer; cdecl;
  Tlo_close        = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_read         = function(Handle: PPGconn; fd: Integer; buf: PAnsiChar; len: NativeUInt): Integer; cdecl;
  Tlo_write        = function(Handle: PPGconn; fd: Integer; buf: PAnsiChar; len: NativeUInt): Integer; cdecl;
  Tlo_lseek        = function(Handle: PPGconn; fd, offset, whence: Integer): Integer; cdecl;
  Tlo_creat        = function(Handle: PPGconn; mode: Integer): Oid; cdecl;
  Tlo_tell         = function(Handle: PPGconn; fd: Integer): Integer; cdecl;
  Tlo_unlink       = function(Handle: PPGconn; lobjId: Oid): Integer; cdecl;
  Tlo_import       = function(Handle: PPGconn; filename: PAnsiChar): Oid; cdecl;
  Tlo_export       = function(Handle: PPGconn; lobjId: Oid; filename: PAnsiChar): Integer; cdecl;

{ ************** Collection of Plain API Function types definition ************* }
TZPOSTGRESQL_API = record
{ ===	in fe-connect.c === }
  PQconnectdb:     TPQconnectdb;
  PQsetdbLogin:    TPQsetdbLogin;
  PQconndefaults:  TPQconndefaults;
  PQfinish:        TPQfinish;
  PQreset:         TPQreset;
  PQrequestCancel: TPQrequestCancel;
  PQdb:            TPQdb;
  PQuser:          TPQuser;
  PQpass:          TPQpass;
  PQhost:          TPQhost;
  PQport:          TPQport;
  PQtty:           TPQtty;
  PQoptions:       TPQoptions;
  PQstatus:        TPQstatus;
  PQserverVersion: TPQserverVersion;
  PQerrorMessage:  TPQerrorMessage;
  PQsocket:        TPQsocket;
  PQbackendPID:    TPQbackendPID;
  PQtrace:         TPQtrace;
  PQuntrace:       TPQuntrace;
  PQsetNoticeProcessor: TPQsetNoticeProcessor;
  PQclientEncoding: TPQclientEncoding;
{ === in fe-exec.c === }
  PQexec:          TPQexec;
  PQexecParams:    TPQexecParams;
  PQprepare:       TPQprepare;
  PQexecPrepared:  TPQexecPrepared;
  PQsendQuery:     TPQsendQuery;
  PQsendQueryParams: TPQsendQueryParams;
  PQsendPrepare:   TPQsendPrepare;
  PQsendQueryPrepared: TPQsendQueryPrepared;
  PQgetResult:     TPQgetResult;
  PQsetSingleRowMode: TPQsetSingleRowMode;
  //* Describe prepared statements and portals */
  PQdescribePrepared:     TPQdescribePrepared;
  PQdescribePortal:       TPQdescribePortal;
  PQsendDescribePrepared: TPQsendDescribePrepared;
  PQsendDescribePortal:   TPQsendDescribePortal;
  PQnotifies:      TPQnotifies;
  PQfreeNotify:    TPQfreeNotify;
  PQisBusy:        TPQisBusy;
  PQconsumeInput:  TPQconsumeInput;
  PQgetCancel:     TPQgetCancel;
  PQfreeCancel:    TPQfreeCancel;
  PQcancel:        TPQcancel;
  PQgetline:       TPQgetline;
  PQputline:       TPQputline;
  PQgetlineAsync:  TPQgetlineAsync;
  PQputnbytes:     TPQputnbytes;
  PQendcopy:       TPQendcopy;
  PQfn:            TPQfn;
  PQresultStatus:  TPQresultStatus;
  PQresultErrorMessage: TPQresultErrorMessage;
  PQresultErrorField: TPQresultErrorField; // postgresql 8
  PQntuples:       TPQntuples;
  PQnfields:       TPQnfields;
  PQbinaryTuples:  TPQbinaryTuples;
  PQfname:         TPQfname;
  PQfnumber:       TPQfnumber;
  PQftable:        TPQftable;
  PQftablecol:     TPQftablecol;
  PQftype:         TPQftype;
  PQfsize:         TPQfsize;
  PQfmod:          TPQfmod;
  PQcmdStatus:     TPQcmdStatus;
  PQoidValue:      TPQoidValue;
  PQoidStatus:     TPQoidStatus;
  PQcmdTuples:     TPQcmdTuples;
  PQgetvalue:      TPQgetvalue;
  PQgetlength:     TPQgetlength;
  PQgetisnull:     TPQgetisnull;
  PQclear:         TPQclear;
  PQmakeEmptyPGresult:  TPQmakeEmptyPGresult;

  PQescapeStringConn: TPQescapeStringConn; //since 7.3
  PQescapeByteaConn:  TPQescapeByteaConn; // postgresql since 7.3
  PQFreemem:          TPQFreemem; // since postgresql 7.4
  PQescapeString:     TPQescapeString; // since postgresql 7.4
  PQescapeBytea:      TPQescapeBytea; // since postgresql 7.4
  PQunescapeBytea:    TPQunescapeBytea; // since postgresql 8.3
  PQescapeLiteral:    TPQescapeLiteral; // since postgresql 9.0
  PQescapeIdentifier: TPQescapeIdentifier; // since postgresql 9.0

{ === in fe-lobj.c === }
  lo_open:         Tlo_open;
  lo_close:        Tlo_close;
  lo_read:         Tlo_read;
  lo_write:        Tlo_write;
  lo_lseek:        Tlo_lseek;
  lo_creat:        Tlo_creat;
  lo_tell:         Tlo_tell;
  lo_unlink:       Tlo_unlink;
  lo_import:       Tlo_import;
  lo_export:       Tlo_export;
end;

PAPI = ^TZPOSTGRESQL_API;

type

  {** Represents a generic interface to PostgreSQL native API. }
  IZPostgreSQLPlainDriver = interface (IZPlainDriver)
    ['{03CD6345-2D7A-4FE2-B03D-3C5656789FEB}']

    function GetStandardConformingStrings: Boolean;

    function EscapeBytea(Handle: PGconn; from: PAnsiChar; from_length: LongWord; to_lenght:PLongword): PAnsiChar;
    function EscapeString(Handle: PGconn; ToChar: PAnsiChar; const FromChar: PAnsiChar;
      length: NativeUInt; error: PInteger): NativeUInt; overload;
    function UnescapeBytea(const from:PAnsiChar;to_lenght:PLongword):PAnsiChar;
    procedure FreeMem(ptr:Pointer);

    function SupportsEncodeBYTEA: Boolean;
    function SupportsDecodeBYTEA: Boolean;
    function SupportsStringEscaping(const ClientDependend: Boolean): Boolean;

    function ConnectDatabase(ConnInfo: PAnsiChar): PZPostgreSQLConnect;
    function SetDatabaseLogin(Host, Port, Options, TTY, Db, User,Passwd: PAnsiChar): PZPostgreSQLConnect;
    function GetConnectDefaults: PZPostgreSQLConnectInfoOption;

    procedure Finish(Handle: PZPostgreSQLConnect);
    procedure Reset(Handle: PZPostgreSQLConnect);
    function RequestCancel(Handle: PZPostgreSQLConnect): Integer;
    function GetDatabase(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetUser(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetPassword(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetHost(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetPort(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetTTY(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetOptions(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetStatus(Handle: PZPostgreSQLConnect):TZPostgreSQLConnectStatusType;
    function GetClientEncoding(Handle: PPGconn): Integer; //EgonHugeist

    function GetErrorMessage(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetSocket(Handle: PZPostgreSQLConnect): Integer;
    function GetBackendPID(Handle: PZPostgreSQLConnect): Integer;
    procedure Trace(Handle: PZPostgreSQLConnect; DebugPort: Pointer);
    procedure Untrace(Handle: PZPostgreSQLConnect);
    procedure SetNoticeProcessor(Handle: PZPostgreSQLConnect;Proc: TZPostgreSQLNoticeProcessor; Arg: Pointer);

    function ExecuteQuery(Handle: PZPostgreSQLConnect;Query: PAnsiChar): PZPostgreSQLResult;
    function ExecParams(Handle: PPGconn; command: PAnsichar;
        nParams: Integer; paramTypes: TPQparamTypes; paramValues: TPQparamValues;
        paramLengths: TPQparamLengths; paramFormats: TPQparamFormats;
        resultFormat: Integer): PPGresult;
    function Prepare(Handle: PPGconn; stmtName: PAnsichar;
        query: PAnsiChar; nParams: Integer; paramTypes: TPQparamTypes): PPGresult;
    function ExecPrepared(Handle: PPGconn; stmtName: PAnsichar;
        nParams: Integer; paramValues: TPQparamValues; paramLengths: TPQparamLengths;
        paramFormats: TPQparamFormats; resultFormat: Integer): PPGresult;
    function SendQuery(Handle: PZPostgreSQLConnect; Query: PAnsiChar): Integer;
    function SendQueryParams(Handle: PPGconn; command: PAnsichar;
        nParams: Integer; paramTypes: TPQparamTypes; paramValues: TPQparamValues;
        paramLengths: TPQparamLengths; paramFormats: TPQparamFormats;
        resultFormat: Integer): Integer;
    function SendPrepare(Handle: PPGconn; stmtName: PAnsichar;
        query: PAnsiChar; nParams: Integer; paramTypes: TPQparamTypes): Integer;
    function SendQueryPrepared(Handle: PPGconn; stmtName: PAnsichar;
         nParams: Integer; paramValues: TPQparamValues;
         paramLengths: TPQparamLengths; paramFormats: TPQparamFormats;
         resultFormat: Integer): Integer;
    function PGGetResult(Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
    //* Describe prepared statements and portals */
    function DescribePrepared(Handle: PPGconn; const stmt: PAnsiChar): PPGresult;
    function DescribePortal(Handle: PPGconn; const portal: PAnsiChar): PPGresult;
    function SendDescribePrepared(Handle: PPGconn; const stmt: PAnsiChar): Integer;
    function SendDescribePortal(Handle: PPGconn; const portal: PAnsiChar): Integer;
    function Notifies(Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
    procedure FreeNotify(Handle: PZPostgreSQLNotify);

    function IsBusy(Handle: PZPostgreSQLConnect): Integer;
    function ConsumeInput(Handle: PZPostgreSQLConnect): Integer;
    function GetCancel(Handle: PZPostgreSQLConnect): PZPostgreSQLCancel;
    procedure FreeCancel( Canc: PZPostgreSQLCancel);
    function Cancel( Canc: PZPostgreSQLCancel; Buffer: PChar; Length: Integer): Integer;
    function GetLine(Handle: PZPostgreSQLConnect; Str: PAnsiChar;
      Length: Integer): Integer;
    function PutLine(Handle: PZPostgreSQLConnect; Str: PAnsiChar): Integer;
    function GetLineAsync(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;

    function PutBytes(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;
    function EndCopy(Handle: PZPostgreSQLConnect): Integer;
    function ExecuteFunction(Handle: PZPostgreSQLConnect; fnid: Integer;
      result_buf, result_len: PInteger; result_is_int: Integer;
      args: PZPostgreSQLArgBlock; nargs: Integer): PZPostgreSQLResult;
    function PQresultStatus(Res: PZPostgreSQLResult):
      TZPostgreSQLExecStatusType;

    function GetResultErrorMessage(Res: PZPostgreSQLResult): PAnsiChar;
    function GetResultErrorField(Res: PZPostgreSQLResult; FieldCode: TZPostgreSQLFieldCode): PAnsiChar;

    function GetRowCount(Res: PZPostgreSQLResult): Integer;
    function GetFieldCount(Res: PZPostgreSQLResult): Integer;

    function GetBinaryTuples(Res: PZPostgreSQLResult): Integer;
    function PQfname(Res: PZPostgreSQLResult; FieldNum: Integer): PAnsiChar;
    function GetFieldNumber(Res: PZPostgreSQLResult; FieldName: PAnsiChar): Integer;
    function PQftable(Res: PZPostgreSQLResult; FieldNum: Integer) : Oid;
    function PQftablecol(Res: PZPostgreSQLResult; FieldNum: Integer) : Integer;
    function PQftype(Res: PZPostgreSQLResult; FieldNum: Integer): Oid;
    function PQfsize(Res: PZPostgreSQLResult;  FieldNum: Integer): Integer;
    function GetFieldMode(Res: PZPostgreSQLResult; FieldNum: Integer): Integer;
    function GetCommandStatus(Res: PZPostgreSQLResult): PAnsiChar;
    function GetOidValue(Res: PZPostgreSQLResult): Oid;
    function GetOidStatus(Res: PZPostgreSQLResult): PAnsiChar;
    function GetCommandTuples(Res: PZPostgreSQLResult): PAnsiChar;

    function GetValue(Res: PZPostgreSQLResult;  TupNum, FieldNum: Integer): PAnsiChar;
    function GetLength(Res: PZPostgreSQLResult; TupNum, FieldNum: Integer): Integer;
    function GetIsNull(Res: PZPostgreSQLResult; TupNum, FieldNum: Integer): Integer;
    procedure PQclear(Res: PZPostgreSQLResult);

    function MakeEmptyResult(Handle: PZPostgreSQLConnect;
      Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;

    function OpenLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      Mode: Integer): Integer;
    function CloseLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function ReadLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PAnsiChar; Length: Integer): Integer;
    function WriteLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PAnsiChar; Length: Integer): Integer;
    function SeekLargeObject(Handle: PZPostgreSQLConnect;
      Fd, Offset, Whence: Integer): Integer;
    function CreateLargeObject(Handle: PZPostgreSQLConnect;
      Mode: Integer): Oid;
    function TellLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function UnlinkLargeObject(Handle: PZPostgreSQLConnect;
      ObjId: Oid): Integer;
    function ImportLargeObject(Handle: PZPostgreSQLConnect;
      FileName: PAnsiChar): Oid;
    function ExportLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      FileName: PAnsiChar): Integer;
    function GetPlainFunc:PAPI;

    function PQsetSingleRowMode(Handle: PZPostgreSQLConnect): Integer; //PG9+
  end;

  {** Implements a base driver for PostgreSQL}
  TZPostgreSQLBaseDriver = class(TZAbstractPlainDriver, IZPostgreSQLPlainDriver)
  protected
    POSTGRESQL_API: TZPOSTGRESQL_API;
    function GetStandardConformingStrings: Boolean; virtual;
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    procedure LoadApi; override;
  public
    constructor Create;
    function EscapeBytea(Handle: PGconn; from: PAnsiChar; from_length: LongWord; to_lenght:PLongword): PAnsiChar;
    function EscapeString(Handle: PGconn; ToChar: PAnsiChar; const FromChar: PAnsiChar;
      length: NativeUInt; error: PInteger): NativeUInt; overload;
    function UnescapeBytea(const from:PAnsiChar;to_lenght:PLongword):PAnsiChar;
    procedure FreeMem(ptr:Pointer);

    function SupportsEncodeBYTEA: Boolean;
    function SupportsDecodeBYTEA: Boolean;
    function SupportsStringEscaping(const ClientDependend: Boolean): Boolean;

    function ConnectDatabase(ConnInfo: PAnsiChar): PZPostgreSQLConnect;
    function SetDatabaseLogin(Host, Port, Options, TTY, Db, User,
      Passwd: PAnsiChar): PZPostgreSQLConnect;
    function GetConnectDefaults: PZPostgreSQLConnectInfoOption;

    procedure Finish(Handle: PZPostgreSQLConnect);
    procedure Reset(Handle: PZPostgreSQLConnect);
    function RequestCancel(Handle: PZPostgreSQLConnect): Integer;
    function GetDatabase(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetUser(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetPassword(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetHost(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetPort(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetTTY(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetOptions(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetStatus(Handle: PZPostgreSQLConnect):
      TZPostgreSQLConnectStatusType;
    function GetClientEncoding(Handle: PPGconn): Integer; //EgonHugeist

    function GetErrorMessage(Handle: PZPostgreSQLConnect): PAnsiChar;
    function GetSocket(Handle: PZPostgreSQLConnect): Integer;
    function GetBackendPID(Handle: PZPostgreSQLConnect): Integer;
    procedure Trace(Handle: PZPostgreSQLConnect; DebugPort: Pointer);
    procedure Untrace(Handle: PZPostgreSQLConnect);
    procedure SetNoticeProcessor(Handle: PZPostgreSQLConnect;
      Proc: TZPostgreSQLNoticeProcessor; Arg: Pointer);

    function ExecuteQuery(Handle: PZPostgreSQLConnect;
      Query: PAnsiChar): PZPostgreSQLResult;
    function ExecParams(Handle: PPGconn; command: PAnsichar;
        nParams: Integer; paramTypes: TPQparamTypes; paramValues: TPQparamValues;
        paramLengths: TPQparamLengths; paramFormats: TPQparamFormats;
        resultFormat: Integer): PPGresult;
    function Prepare(Handle: PPGconn; stmtName: PAnsichar;
        query: PAnsiChar; nParams: Integer; paramTypes: TPQparamTypes): PPGresult;
    function ExecPrepared(Handle: PPGconn; stmtName: PAnsichar;
        nParams: Integer; paramValues: TPQparamValues; paramLengths: TPQparamLengths;
        paramFormats: TPQparamFormats; resultFormat: Integer): PPGresult;
    function SendQuery(Handle: PZPostgreSQLConnect; Query: PAnsiChar): Integer;
    function SendQueryParams(Handle: PPGconn; command: PAnsichar;
        nParams: Integer; paramTypes: TPQparamTypes; paramValues: TPQparamValues;
        paramLengths: TPQparamLengths; paramFormats: TPQparamFormats;
        resultFormat: Integer): Integer;
    function SendPrepare(Handle: PPGconn; stmtName: PAnsichar;
        query: PAnsiChar; nParams: Integer; paramTypes: TPQparamTypes): Integer;
    function SendQueryPrepared(Handle: PPGconn; stmtName: PAnsichar;
         nParams: Integer; paramValues: TPQparamValues;
         paramLengths: TPQparamLengths; paramFormats: TPQparamFormats;
         resultFormat: Integer): Integer;
    function PGGetResult(Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
    function DescribePrepared(Handle: PPGconn; const stmt: PAnsiChar): PPGresult;
    function DescribePortal(Handle: PPGconn; const portal: PAnsiChar): PPGresult;
    function SendDescribePrepared(Handle: PPGconn; const stmt: PAnsiChar): Integer;
    function SendDescribePortal(Handle: PPGconn; const portal: PAnsiChar): Integer;

    function Notifies(Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
    procedure FreeNotify(Handle: PZPostgreSQLNotify);

    function IsBusy(Handle: PZPostgreSQLConnect): Integer;
    function ConsumeInput(Handle: PZPostgreSQLConnect): Integer;
    function GetCancel(Handle: PZPostgreSQLConnect): PZPostgreSQLCancel;
    procedure FreeCancel( Canc: PZPostgreSQLCancel);
    function Cancel( Canc: PZPostgreSQLCancel; Buffer: PChar; Length: Integer): Integer;
    function GetLine(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;
    function PutLine(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar): Integer;
    function GetLineAsync(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;

    function PutBytes(Handle: PZPostgreSQLConnect; Buffer: PAnsiChar;
      Length: Integer): Integer;
    function EndCopy(Handle: PZPostgreSQLConnect): Integer;
    function ExecuteFunction(Handle: PZPostgreSQLConnect; fnid: Integer;
      result_buf, result_len: PInteger; result_is_int: Integer;
      args: PZPostgreSQLArgBlock; nargs: Integer): PZPostgreSQLResult;
    function PQresultStatus(Res: PZPostgreSQLResult): TZPostgreSQLExecStatusType;
    function GetResultErrorMessage(Res: PZPostgreSQLResult): PAnsiChar;
    function GetResultErrorField(Res: PZPostgreSQLResult;FieldCode:TZPostgreSQLFieldCode):PAnsiChar;

    function GetRowCount(Res: PZPostgreSQLResult): Integer;
    function GetFieldCount(Res: PZPostgreSQLResult): Integer;

    function GetBinaryTuples(Res: PZPostgreSQLResult): Integer;
    function PQfname(Res: PZPostgreSQLResult;
      FieldNum: Integer): PAnsiChar;
    function GetFieldNumber(Res: PZPostgreSQLResult;
      FieldName: PAnsiChar): Integer;
    function PQftable(Res: PZPostgreSQLResult; FieldNum: Integer) : Oid;
    function PQftablecol(Res: PZPostgreSQLResult; FieldNum: Integer) : Integer;
    function PQftype(Res: PZPostgreSQLResult;
      FieldNum: Integer): Oid;
    function PQfsize(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetFieldMode(Res: PZPostgreSQLResult;
      FieldNum: Integer): Integer;
    function GetCommandStatus(Res: PZPostgreSQLResult): PAnsiChar;
    function GetOidValue(Res: PZPostgreSQLResult): Oid;
    function GetOidStatus(Res: PZPostgreSQLResult): PAnsiChar;
    function GetCommandTuples(Res: PZPostgreSQLResult): PAnsiChar;

    function GetValue(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): PAnsiChar;
    function GetLength(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    function GetIsNull(Res: PZPostgreSQLResult;
      TupNum, FieldNum: Integer): Integer;
    procedure PQclear(Res: PZPostgreSQLResult);

    function MakeEmptyResult(Handle: PZPostgreSQLConnect;
      Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;

    function OpenLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      Mode: Integer): Integer;
    function CloseLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function ReadLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PAnsiChar; Length: Integer): Integer;
    function WriteLargeObject(Handle: PZPostgreSQLConnect; Fd: Integer;
      Buffer: PAnsiChar; Length: Integer): Integer;
    function SeekLargeObject(Handle: PZPostgreSQLConnect;
      Fd, Offset, Whence: Integer): Integer;
    function CreateLargeObject(Handle: PZPostgreSQLConnect;
      Mode: Integer): Oid;
    function TellLargeObject(Handle: PZPostgreSQLConnect;
      Fd: Integer): Integer;
    function UnlinkLargeObject(Handle: PZPostgreSQLConnect;
      ObjId: Oid): Integer;
    function ImportLargeObject(Handle: PZPostgreSQLConnect;
      FileName: PAnsiChar): Oid;
    function ExportLargeObject(Handle: PZPostgreSQLConnect; ObjId: Oid;
      FileName: PAnsiChar): Integer;
    function GetPlainFunc:PAPI;
    function PQsetSingleRowMode(Handle: PZPostgreSQLConnect): Integer; //PG9+
  end;

  {** Implements a driver for PostgreSQL 7.4 }
  TZPostgreSQL7PlainDriver = class(TZPostgreSQLBaseDriver, IZPlainDriver,
    IZPostgreSQLPlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
 end;


  {** Implements a driver for PostgreSQL 8.1 }

  { TZPostgreSQL8PlainDriver }

  TZPostgreSQL8PlainDriver = class(TZPostgreSQLBaseDriver, IZPlainDriver,IZPostgreSQLPlainDriver)
  protected
    function Clone: IZPlainDriver; override;
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { TZPostgreSQL8PlainDriver }

  TZPostgreSQL9PlainDriver = class(TZPostgreSQL8PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
    function GetStandardConformingStrings: Boolean; override;
    procedure LoadCodePages; override;
  public
    constructor Create;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL}
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL}
uses SysUtils, ZPlainLoader, Classes, ZEncoding
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZPostgreSQLBaseDriver }

function TZPostgreSQLBaseDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UNICODE';
end;

procedure TZPostgreSQLBaseDriver.LoadCodePages;
begin
  { MultiByte }
  AddCodePage('EUC_JP', Ord(csEUC_JP), ceAnsi, zCP_EUC_JP, '', 3); { EUC_JP 	Japanese EUC }
  AddCodePage('EUC_CN', Ord(csEUC_CN), ceAnsi, zCP_EUC_CN, '', 3); {EUC_CN 	Chinese EUC}
  AddCodePage('EUC_KR', Ord(csEUC_KR), ceAnsi, zCP_euc_kr, '', 3); {Extended UNIX Code-KR 	Korean}
  AddCodePage('JOHAB', Ord(csJOHAB), ceAnsi, ZCP_JOHAB, '', 3); {JOHAB 	Korean (Hangul)}
  AddCodePage('EUC_TW', Ord(csEUC_TW), ceAnsi, $ffff, '', 3); {Extended UNIX Code-TW 	Traditional Chinese, Taiwanese}
  AddCodePage('UNICODE', Ord(csUNICODE_PODBC), ceUTF8, zCP_UTF8, '', 4); {UNICODE 	Unicode (UTF-8)}
  AddCodePage('MULE_INTERNAL', Ord(csMULE_INTERNAL), ceAnsi, $ffff, '', 4); { Mule internal code 	Multilingual Emacs }
  {SingleByte}
  AddCodePage('SQL_ASCII', Ord(csSQL_ASCII), ceAnsi, zCP_us_ascii); {unspecified (see text) 	any}
  AddCodePage('LATIN1', Ord(csLATIN1), ceAnsi, zCP_WIN1252); { ISO 8859-1, ECMA 94 	Western European }
  AddCodePage('LATIN2', Ord(csLATIN2), ceAnsi, zCP_L2_ISO_8859_2);  { 	ISO 8859-2, ECMA 94 	Central European }
  AddCodePage('LATIN3', Ord(csLATIN3), ceAnsi, zCP_L3_ISO_8859_3);  { ISO 8859-3, ECMA 94 	South European }
  AddCodePage('LATIN4', Ord(csLATIN4), ceAnsi, zCP_L4_ISO_8859_4);  { ISO 8859-4, ECMA 94 	North European }
  AddCodePage('LATIN5', Ord(csLATIN5), ceAnsi, zCP_L5_ISO_8859_9);  { ISO 8859-9, ECMA 128 	Turkish }
  AddCodePage('LATIN6', Ord(csLATIN6), ceAnsi, zCP_L6_ISO_8859_10);  { ISO 8859-10, ECMA 144 	Nordic }
  AddCodePage('LATIN7', Ord(csLATIN7), ceAnsi, zCP_L7_ISO_8859_13);  { ISO 8859-13 	Baltic }
  AddCodePage('LATIN8', Ord(csLATIN8), ceAnsi, zCP_L8_ISO_8859_14);  { ISO 8859-14 	Celtic }
  AddCodePage('LATIN9', Ord(csLATIN9), ceAnsi, zCP_L9_ISO_8859_15);  { ISO 8859-15 	LATIN1 with Euro and accents }
  AddCodePage('LATIN10', Ord(csLATIN10), ceAnsi, zCP_L10_ISO_8859_16);  { ISO 8859-16, ASRO SR 14111 	Romanian }
  AddCodePage('ISO_8859_5', Ord(csISO_8859_5), ceAnsi, zCP_L5_ISO_8859_5); { ISO 8859-5, ECMA 113 	Latin/Cyrillic}
  AddCodePage('ISO_8859_6', Ord(csISO_8859_6), ceAnsi, zCP_L6_ISO_8859_6); { ISO 8859-6, ECMA 114 	Latin/Arabic }
  AddCodePage('ISO_8859_7', Ord(csISO_8859_7), ceAnsi, zCP_L7_ISO_8859_7); { ISO 8859-7, ECMA 118 	Latin/Greek }
  AddCodePage('ISO_8859_8', Ord(csISO_8859_8), ceAnsi, zCP_L8_ISO_8859_8);  { ISO 8859-8, ECMA 121 	Latin/Hebrew }
  AddCodePage('KOI8', Ord(csKOI8), ceAnsi, zCP_KOI8R);  { KOI8-R(U) 	Cyrillic }
  AddCodePage('WIN', Ord(csWIN), ceAnsi, zCP_WIN1251); { Windows CP1251 }
  AddCodePage('ALT', Ord(csALT), ceAnsi, zCP_DOS866); { Windows CP866 }
  AddCodePage('WIN1256', Ord(csWIN1256), ceAnsi, zCP_WIN1256);  { Windows CP1256 	Arabic }
  AddCodePage('TCVN', Ord(csTCVN), ceAnsi, zCP_WIN1258); { TCVN-5712/Windows CP1258 (Vietnamese) }
  AddCodePage('WIN874', Ord(csWIN874), ceAnsi, zCP_WIN874); { Windows CP874 (Thai) }
end;

procedure TZPostgreSQLBaseDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
  { ===	in fe-connect.c === }
    @POSTGRESQL_API.PQconnectdb    := GetAddress('PQconnectdb');
    @POSTGRESQL_API.PQsetdbLogin   := GetAddress('PQsetdbLogin');
    @POSTGRESQL_API.PQconndefaults := GetAddress('PQconndefaults');
    @POSTGRESQL_API.PQfinish       := GetAddress('PQfinish');
    @POSTGRESQL_API.PQreset        := GetAddress('PQreset');
    @POSTGRESQL_API.PQrequestCancel := GetAddress('PQrequestCancel');
    @POSTGRESQL_API.PQdb           := GetAddress('PQdb');
    @POSTGRESQL_API.PQuser         := GetAddress('PQuser');
    @POSTGRESQL_API.PQpass         := GetAddress('PQpass');
    @POSTGRESQL_API.PQhost         := GetAddress('PQhost');
    @POSTGRESQL_API.PQport         := GetAddress('PQport');
    @POSTGRESQL_API.PQtty          := GetAddress('PQtty');
    @POSTGRESQL_API.PQoptions      := GetAddress('PQoptions');
    @POSTGRESQL_API.PQstatus       := GetAddress('PQstatus');
    @POSTGRESQL_API.PQserverVersion:= GetAddress('PQserverVersion');
    @POSTGRESQL_API.PQerrorMessage := GetAddress('PQerrorMessage');
    @POSTGRESQL_API.PQsocket       := GetAddress('PQsocket');
    @POSTGRESQL_API.PQbackendPID   := GetAddress('PQbackendPID');
    @POSTGRESQL_API.PQtrace        := GetAddress('PQtrace');
    @POSTGRESQL_API.PQuntrace      := GetAddress('PQuntrace');
    @POSTGRESQL_API.PQsetNoticeProcessor := GetAddress('PQsetNoticeProcessor');
    @POSTGRESQL_API.PQclientEncoding := GetAddress('PQclientEncoding');
  { === in fe-exec.c === }
    @POSTGRESQL_API.PQexec         := GetAddress('PQexec');
    @POSTGRESQL_API.PQexecParams   := GetAddress('PQexecParams');
    @POSTGRESQL_API.PQprepare      := GetAddress('PQprepare');
    @POSTGRESQL_API.PQexecPrepared := GetAddress('PQexecPrepared');
    @POSTGRESQL_API.PQsendQuery    := GetAddress('PQsendQuery');
    @POSTGRESQL_API.PQsendQueryParams:= GetAddress('PQsendQueryParams');
    @POSTGRESQL_API.PQsendPrepare  := GetAddress('PQsendPrepare');
    @POSTGRESQL_API.PQsendQueryPrepared := GetAddress('PQsendQueryPrepared');
    @POSTGRESQL_API.PQgetResult    := GetAddress('PQgetResult');
    @POSTGRESQL_API.PQsetSingleRowMode := GetAddress('PQsetSingleRowMode'); //9+ http://www.postgresql.org/docs/9.2/static/libpq-single-row-mode.html

    @POSTGRESQL_API.PQnotifies     := GetAddress('PQnotifies');
    @POSTGRESQL_API.PQfreeNotify   := GetAddress('PQfreeNotify');
    @POSTGRESQL_API.PQisBusy       := GetAddress('PQisBusy');
    @POSTGRESQL_API.PQconsumeInput := GetAddress('PQconsumeInput');
    @POSTGRESQL_API.PQgetline      := GetAddress('PQgetline');
    @POSTGRESQL_API.PQputline      := GetAddress('PQputline');
    @POSTGRESQL_API.PQgetlineAsync := GetAddress('PQgetlineAsync');
    @POSTGRESQL_API.PQputnbytes    := GetAddress('PQputnbytes');
    @POSTGRESQL_API.PQendcopy      := GetAddress('PQendcopy');
    @POSTGRESQL_API.PQfn           := GetAddress('PQfn');
    @POSTGRESQL_API.PQresultStatus := GetAddress('PQresultStatus');
    @POSTGRESQL_API.PQresultErrorMessage := GetAddress('PQresultErrorMessage');
    @POSTGRESQL_API.PQntuples      := GetAddress('PQntuples');
    @POSTGRESQL_API.PQnfields      := GetAddress('PQnfields');
    @POSTGRESQL_API.PQbinaryTuples := GetAddress('PQbinaryTuples');
    @POSTGRESQL_API.PQfname        := GetAddress('PQfname');
    @POSTGRESQL_API.PQfnumber      := GetAddress('PQfnumber');
    @POSTGRESQL_API.PQftable       := GetAddress('PQftable');
    @POSTGRESQL_API.PQftablecol    := GetAddress('PQftablecol');
    @POSTGRESQL_API.PQftype        := GetAddress('PQftype');
    @POSTGRESQL_API.PQfsize        := GetAddress('PQfsize');
    @POSTGRESQL_API.PQfmod         := GetAddress('PQfmod');
    @POSTGRESQL_API.PQcmdStatus    := GetAddress('PQcmdStatus');
    @POSTGRESQL_API.PQoidValue     := GetAddress('PQoidValue');
    @POSTGRESQL_API.PQoidStatus    := GetAddress('PQoidStatus');
    @POSTGRESQL_API.PQcmdTuples    := GetAddress('PQcmdTuples');
    @POSTGRESQL_API.PQgetvalue     := GetAddress('PQgetvalue');
    @POSTGRESQL_API.PQgetlength    := GetAddress('PQgetlength');
    @POSTGRESQL_API.PQgetisnull    := GetAddress('PQgetisnull');
    @POSTGRESQL_API.PQclear        := GetAddress('PQclear');
    @POSTGRESQL_API.PQmakeEmptyPGresult := GetAddress('PQmakeEmptyPGresult');

  { === in fe-lobj.c === }
    @POSTGRESQL_API.lo_open        := GetAddress('lo_open');
    @POSTGRESQL_API.lo_close       := GetAddress('lo_close');
    @POSTGRESQL_API.lo_read        := GetAddress('lo_read');
    @POSTGRESQL_API.lo_write       := GetAddress('lo_write');
    @POSTGRESQL_API.lo_lseek       := GetAddress('lo_lseek');
    @POSTGRESQL_API.lo_creat       := GetAddress('lo_creat');
    @POSTGRESQL_API.lo_tell        := GetAddress('lo_tell');
    @POSTGRESQL_API.lo_unlink      := GetAddress('lo_unlink');
    @POSTGRESQL_API.lo_import      := GetAddress('lo_import');
    @POSTGRESQL_API.lo_export      := GetAddress('lo_export');
    @POSTGRESQL_API.PQescapeStringConn  := GetAddress('PQescapeStringConn'); //since 7.3
    @POSTGRESQL_API.PQescapeByteaConn   := GetAddress('PQescapeByteaConn'); // postgresql since 7.3
    @POSTGRESQL_API.PQFreemem           := GetAddress('PQfreemem'); // since postgresql 7.4
    @POSTGRESQL_API.PQescapeString      := GetAddress('PQescapeString'); // since postgresql 7.4
    @POSTGRESQL_API.PQescapeBytea       := GetAddress('PQescapeBytea'); // since postgresql 7.4
    @POSTGRESQL_API.PQunescapeBytea     := GetAddress('PQunescapeBytea'); // since postgresql 8.3
    @POSTGRESQL_API.PQescapeLiteral     := GetAddress('PQescapeLiteral'); // since postgresql 9.0
    @POSTGRESQL_API.PQescapeIdentifier  := GetAddress('PQescapeIdentifier'); // since postgresql 9.0

    @POSTGRESQL_API.PQresultErrorField  := GetAddress('PQresultErrorField');
    @POSTGRESQL_API.PQgetCancel         := GetAddress('PQgetCancel');
    @POSTGRESQL_API.PQfreeCancel        := GetAddress('PQfreeCancel');
    @POSTGRESQL_API.PQcancel            := GetAddress('PQcancel');
  end;
end;

constructor TZPostgreSQLBaseDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IFNDEF STRICT_DLL_LOADING}
    {$IFNDEF UNIX}
      FLoader.AddLocation(WINDOWS_DLL_LOCATION);
    {$ELSE}
      FLoader.AddLocation(LINUX_DLL_LOCATION);
    {$ENDIF}
  {$ENDIF}
  LoadCodePages;
end;

procedure TZPostgreSQLBaseDriver.PQclear(Res: PZPostgreSQLResult);
begin
  POSTGRESQL_API.PQclear(Res);
end;

function TZPostgreSQLBaseDriver.CloseLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_close(Handle, Fd);
end;

function TZPostgreSQLBaseDriver.ConnectDatabase(
  ConnInfo: PAnsiChar): PZPostgreSQLConnect;
begin
  Result := POSTGRESQL_API.PQconnectdb(ConnInfo);
end;

function TZPostgreSQLBaseDriver.ConsumeInput(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQconsumeInput(Handle);
end;

function TZPostgreSQLBaseDriver.GetCancel(Handle: PZPostgreSQLConnect): PZPostgreSQLCancel;
begin
  if Assigned(POSTGRESQL_API.PQgetCancel) then
    Result := POSTGRESQL_API.PQgetCancel(Handle)
  else
    Result := nil;
end;

procedure TZPostgreSQLBaseDriver.FreeCancel(Canc: PZPostgreSQLCancel);
begin
  if Assigned(POSTGRESQL_API.PQfreeCancel) then
    POSTGRESQL_API.PQfreeCancel( Canc);
end;

procedure TZPostgreSQLBaseDriver.FreeMem(ptr: Pointer);
begin
  POSTGRESQL_API.PQFreemem(ptr);
end;

function TZPostgreSQLBaseDriver.Cancel(Canc: PZPostgreSQLCancel; Buffer: PChar; Length: Integer): Integer;
begin
  if Assigned(POSTGRESQL_API.PQcancel) then
    Result := POSTGRESQL_API.PQcancel( Canc, Buffer, Length)
  else
    Result := 0;
end;

function TZPostgreSQLBaseDriver.CreateLargeObject(
  Handle: PZPostgreSQLConnect; Mode: Integer): Oid;
begin
  Result := POSTGRESQL_API.lo_creat(Handle, Mode);
end;

function TZPostgreSQLBaseDriver.SupportsEncodeBYTEA: Boolean;
begin
  Result := Assigned(POSTGRESQL_API.PQescapeByteaConn) or
    Assigned(POSTGRESQL_API.PQescapeBytea);
end;

function TZPostgreSQLBaseDriver.SupportsDecodeBYTEA: Boolean;
begin
  Result := Assigned(POSTGRESQL_API.PQUnescapeBytea);
end;

function TZPostgreSQLBaseDriver.SupportsStringEscaping(const ClientDependend: Boolean): Boolean;
begin
  if ClientDependend then
    Result := Assigned(POSTGRESQL_API.PQescapeStringConn)
  else
    Result := Assigned(POSTGRESQL_API.PQescapeStringConn) or
              Assigned(POSTGRESQL_API.PQescapeString);
end;

function TZPostgreSQLBaseDriver.EndCopy( Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQendcopy(Handle);
end;

function TZPostgreSQLBaseDriver.ExecuteFunction(
  Handle: PZPostgreSQLConnect; fnid: Integer; result_buf,
  result_len: PInteger; result_is_int: Integer; args: PZPostgreSQLArgBlock;
  nargs: Integer): PZPostgreSQLResult;
begin
  Result := POSTGRESQL_API.PQfn(Handle, fnid, result_buf,
    result_len, result_is_int, PPQArgBlock(args), nargs);
end;

function TZPostgreSQLBaseDriver.ExecuteQuery(
  Handle: PZPostgreSQLConnect; Query: PAnsiChar): PZPostgreSQLResult;
begin
  Result := POSTGRESQL_API.PQexec(Handle, Query);
end;

function TZPostgreSQLBaseDriver.ExecParams(Handle: PPGconn; command: PAnsichar;
    nParams: Integer; paramTypes: TPQparamTypes; paramValues: TPQparamValues;
    paramLengths: TPQparamLengths; paramFormats: TPQparamFormats;
    resultFormat: Integer): PPGresult;
begin
  if Assigned(POSTGRESQL_API.PQexecParams) then
    Result := POSTGRESQL_API.PQexecParams(Handle, command, nParams, paramtypes,
      Pointer(paramValues), Pointer(paramLengths), Pointer(paramFormats), resultFormat)
  else
    Result := nil;
end;

function TZPostgreSQLBaseDriver.Prepare(Handle: PPGconn; stmtName: PAnsichar;
    query: PAnsiChar; nParams: Integer; paramTypes: TPQparamTypes): PPGresult;
begin
  if Assigned(POSTGRESQL_API.PQprepare) then
    Result := POSTGRESQL_API.PQprepare(Handle, stmtName, query, nParams, Pointer(paramTypes))
  else
    Result := nil;
end;

function TZPostgreSQLBaseDriver.ExecPrepared(Handle: PPGconn; stmtName: PAnsichar;
    nParams: Integer; paramValues: TPQparamValues; paramLengths: TPQparamLengths;
    paramFormats: TPQparamFormats; resultFormat: Integer): PPGresult;
begin
  if Assigned(POSTGRESQL_API.PQexecPrepared) then
    Result := POSTGRESQL_API.PQexecPrepared(Handle, stmtName, nParams,
      Pointer(paramValues), Pointer(paramLengths), Pointer(paramFormats), resultFormat)
  else
    Result := nil;
end;

function TZPostgreSQLBaseDriver.SendQuery(Handle: PZPostgreSQLConnect; Query: PAnsiChar): Integer;
begin
  Result := POSTGRESQL_API.PQsendQuery(Handle, Query);
end;

function TZPostgreSQLBaseDriver.SendQueryParams(Handle: PPGconn; command: PAnsichar;
    nParams: Integer; paramTypes: TPQparamTypes; paramValues: TPQparamValues;
    paramLengths: TPQparamLengths; paramFormats: TPQparamFormats;
    resultFormat: Integer): Integer;
begin
  if Assigned(POSTGRESQL_API.PQsendQueryParams) then
    Result := POSTGRESQL_API.PQsendQueryParams(Handle, command, nParams,
      Pointer(paramTypes), Pointer(paramValues), Pointer(paramLengths), Pointer(paramFormats), resultFormat)
  else
    Result := -1;
end;

function TZPostgreSQLBaseDriver.SendPrepare(Handle: PPGconn; stmtName: PAnsichar;
    query: PAnsiChar; nParams: Integer; paramTypes: TPQparamTypes): Integer;
begin
  if Assigned(POSTGRESQL_API.PQsendPrepare) then
    Result := POSTGRESQL_API.PQsendPrepare(Handle, stmtName, query, nParams,
      paramTypes)
  else
    Result := -1;
end;

function TZPostgreSQLBaseDriver.SendQueryPrepared(Handle: PPGconn; stmtName: PAnsichar;
     nParams: Integer; paramValues: TPQparamValues;
     paramLengths: TPQparamLengths; paramFormats: TPQparamFormats;
     resultFormat: Integer): Integer;
begin
  if Assigned(POSTGRESQL_API.PQsendQueryPrepared) then
    Result := POSTGRESQL_API.PQsendQueryPrepared(Handle, stmtName, nParams,
      Pointer(paramValues), Pointer(paramLengths), Pointer(paramFormats), resultFormat)
  else
    Result := -1;
end;

function TZPostgreSQLBaseDriver.PGGetResult(Handle: PZPostgreSQLConnect): PZPostgreSQLResult;
begin
  Result := POSTGRESQL_API.PQgetResult(Handle);
end;

function TZPostgreSQLBaseDriver.DescribePrepared(Handle: PPGconn;
  const stmt: PAnsiChar): PPGresult;
begin
  if Assigned(POSTGRESQL_API.PQdescribePrepared) then
    Result := POSTGRESQL_API.PQdescribePrepared(Handle, stmt)
  else
    Result := nil;
end;

function TZPostgreSQLBaseDriver.DescribePortal(Handle: PPGconn;
  const portal: PAnsiChar): PPGresult;
begin
  if Assigned(POSTGRESQL_API.PQdescribePortal) then
    Result := POSTGRESQL_API.PQdescribePortal(Handle, portal)
  else
    Result := nil;
end;

function TZPostgreSQLBaseDriver.SendDescribePrepared(Handle: PPGconn;
  const stmt: PAnsiChar): Integer;
begin
  if Assigned(POSTGRESQL_API.PQsendDescribePrepared) then
    Result := POSTGRESQL_API.PQsendDescribePrepared(Handle, stmt)
  else
    Result := -1;
end;

function TZPostgreSQLBaseDriver.SendDescribePortal(Handle: PPGconn;
  const portal: PAnsiChar): Integer;
begin
  if Assigned(POSTGRESQL_API.PQsendDescribePortal) then
    Result := POSTGRESQL_API.PQsendDescribePortal(Handle, portal)
  else
    Result := -1;
end;

function TZPostgreSQLBaseDriver.ExportLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; FileName: PAnsiChar): Integer;
begin
  Result := POSTGRESQL_API.lo_export(Handle, ObjId, FileName);
end;

procedure TZPostgreSQLBaseDriver.Finish(Handle: PZPostgreSQLConnect);
begin
  POSTGRESQL_API.PQfinish(Handle);
end;

procedure TZPostgreSQLBaseDriver.FreeNotify(Handle: PZPostgreSQLNotify);
begin
  POSTGRESQL_API.PQfreeNotify(Handle);
end;

function TZPostgreSQLBaseDriver.GetBackendPID(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQbackendPID(Handle);
end;

function TZPostgreSQLBaseDriver.GetBinaryTuples(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := POSTGRESQL_API.PQbinaryTuples(Res);
end;

function TZPostgreSQLBaseDriver.GetCommandStatus(
  Res: PZPostgreSQLResult): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQcmdStatus(Res);
end;

function TZPostgreSQLBaseDriver.GetCommandTuples(
  Res: PZPostgreSQLResult): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQcmdTuples(Res);
end;

function TZPostgreSQLBaseDriver.GetConnectDefaults:
  PZPostgreSQLConnectInfoOption;
begin
  Result := PZPostgreSQLConnectInfoOption(POSTGRESQL_API.PQconndefaults);
end;

function TZPostgreSQLBaseDriver.GetDatabase(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQdb(Handle);
end;

function TZPostgreSQLBaseDriver.GetErrorMessage(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQerrorMessage(Handle);
end;

function TZPostgreSQLBaseDriver.GetFieldCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := POSTGRESQL_API.PQnfields(Res);
end;

function TZPostgreSQLBaseDriver.GetFieldMode(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQfmod(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.PQfname(Res: PZPostgreSQLResult;
  FieldNum: Integer): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQfname(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetFieldNumber(
  Res: PZPostgreSQLResult; FieldName: PAnsiChar): Integer;
begin
  Result := POSTGRESQL_API.PQfnumber(Res, FieldName);
end;

function TZPostgreSQLBaseDriver.PQftable(Res: PZPostgreSQLResult; FieldNum: Integer) : Oid;
begin
  Result := POSTGRESQL_API.PQftable(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.PQftablecol(Res: PZPostgreSQLResult; FieldNum: Integer) : Integer;
begin
  Result := POSTGRESQL_API.PQftablecol(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.PQfsize(Res: PZPostgreSQLResult;
  FieldNum: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQfsize(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.PQftype(Res: PZPostgreSQLResult;
  FieldNum: Integer): Oid;
begin
  Result := POSTGRESQL_API.PQftype(Res, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetHost(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQhost(Handle);
end;

function TZPostgreSQLBaseDriver.GetIsNull(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQgetisnull(Res, TupNum, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetLength(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQgetlength(Res, TupNum, FieldNum);
end;

function TZPostgreSQLBaseDriver.GetLine(Handle: PZPostgreSQLConnect;
  Buffer: PAnsiChar; Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQgetline(Handle, Buffer, Length);
end;

function TZPostgreSQLBaseDriver.GetLineAsync(
  Handle: PZPostgreSQLConnect; Buffer: PAnsiChar; Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQgetlineAsync(Handle, Buffer, Length);
end;

function TZPostgreSQLBaseDriver.GetOidStatus(
  Res: PZPostgreSQLResult): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQoidStatus(Res);
end;

function TZPostgreSQLBaseDriver.GetOidValue(
  Res: PZPostgreSQLResult): Oid;
begin
  Result := POSTGRESQL_API.PQoidValue(Res);
end;

function TZPostgreSQLBaseDriver.GetOptions(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQoptions(Handle);
end;

function TZPostgreSQLBaseDriver.GetPassword(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQpass(Handle);
end;

function TZPostgreSQLBaseDriver.GetPort(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQport(Handle);
end;

function TZPostgreSQLBaseDriver.GetResultErrorField(Res: PZPostgreSQLResult;  FieldCode: TZPostgreSQLFieldCode): PAnsiChar;
begin
  if Assigned(POSTGRESQL_API.PQresultErrorField) then
    Result := POSTGRESQL_API.PQresultErrorField(Res, ord(FieldCode))
  else
    Result := '';
end;


function TZPostgreSQLBaseDriver.GetResultErrorMessage(
  Res: PZPostgreSQLResult): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQresultErrorMessage(Res);
end;

function TZPostgreSQLBaseDriver.PQresultStatus(
  Res: PZPostgreSQLResult): TZPostgreSQLExecStatusType;
begin
  Result := TZPostgreSQLExecStatusType(POSTGRESQL_API.PQresultStatus(Res));
end;

function TZPostgreSQLBaseDriver.GetRowCount(
  Res: PZPostgreSQLResult): Integer;
begin
  Result := POSTGRESQL_API.PQntuples(Res);
end;

function TZPostgreSQLBaseDriver.GetSocket(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQsocket(Handle);
end;

function TZPostgreSQLBaseDriver.GetStandardConformingStrings: Boolean;
begin
  Result := False;
end;

function TZPostgreSQLBaseDriver.GetStatus(
  Handle: PZPostgreSQLConnect): TZPostgreSQLConnectStatusType;
begin
  Result := TZPostgreSQLConnectStatusType(POSTGRESQL_API.PQstatus(Handle));
end;

function TZPostgreSQLBaseDriver.GetClientEncoding(Handle: PPGconn): Integer; //EgonHugeist
begin
  Result := POSTGRESQL_API.PQclientEncoding(Handle);
end;

function TZPostgreSQLBaseDriver.GetTTY(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQtty(Handle);
end;

function TZPostgreSQLBaseDriver.GetUser(
  Handle: PZPostgreSQLConnect): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQuser(Handle);
end;

function TZPostgreSQLBaseDriver.GetValue(Res: PZPostgreSQLResult;
  TupNum, FieldNum: Integer): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQgetvalue(Res, TupNum, FieldNum);
end;

function TZPostgreSQLBaseDriver.ImportLargeObject(
  Handle: PZPostgreSQLConnect; FileName: PAnsiChar): Oid;
begin
  Result := POSTGRESQL_API.lo_import(Handle, FileName);
end;

function TZPostgreSQLBaseDriver.IsBusy(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQisBusy(Handle);
end;

function TZPostgreSQLBaseDriver.MakeEmptyResult(
  Handle: PZPostgreSQLConnect;
  Status: TZPostgreSQLExecStatusType): PZPostgreSQLResult;
begin
  Result := POSTGRESQL_API.PQmakeEmptyPGresult(Handle,
    TZPostgreSQLExecStatusType(Status));
end;

function TZPostgreSQLBaseDriver.Notifies(
  Handle: PZPostgreSQLConnect): PZPostgreSQLNotify;
begin
  Result := PZPostgreSQLNotify(POSTGRESQL_API.PQnotifies(Handle));
end;

function TZPostgreSQLBaseDriver.OpenLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid; Mode: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_open(Handle, ObjId, Mode);
end;

function TZPostgreSQLBaseDriver.PutBytes(Handle: PZPostgreSQLConnect;
  Buffer: PAnsiChar; Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.PQputnbytes(Handle, Buffer, Length);
end;

function TZPostgreSQLBaseDriver.PutLine(Handle: PZPostgreSQLConnect;
  Buffer: PAnsiChar): Integer;
begin
  Result := POSTGRESQL_API.PQputline(Handle, Buffer);
end;

function TZPostgreSQLBaseDriver.ReadLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PAnsiChar;
  Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_read(Handle, Fd, Buffer, Length);
end;

function TZPostgreSQLBaseDriver.RequestCancel(
  Handle: PZPostgreSQLConnect): Integer;
begin
  Result := POSTGRESQL_API.PQrequestCancel(Handle);
end;

procedure TZPostgreSQLBaseDriver.Reset(Handle: PZPostgreSQLConnect);
begin
  POSTGRESQL_API.PQreset(Handle);
end;

function TZPostgreSQLBaseDriver.SeekLargeObject(
  Handle: PZPostgreSQLConnect; Fd, Offset, Whence: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_lseek(Handle, Fd, Offset, Whence);
end;

function TZPostgreSQLBaseDriver.SetDatabaseLogin(Host, Port, Options,
  TTY, Db, User, Passwd: PAnsiChar): PZPostgreSQLConnect;
begin
  Result := POSTGRESQL_API.PQsetdbLogin(Host, Port, Options, TTY, Db,
    User, Passwd);
end;

procedure TZPostgreSQLBaseDriver.SetNoticeProcessor(
  Handle: PZPostgreSQLConnect; Proc: TZPostgreSQLNoticeProcessor;
  Arg: Pointer);
begin
  POSTGRESQL_API.PQsetNoticeProcessor(Handle, Proc, Arg);
end;

function TZPostgreSQLBaseDriver.TellLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_tell(Handle, Fd);
end;

procedure TZPostgreSQLBaseDriver.Trace(Handle: PZPostgreSQLConnect;
  DebugPort: Pointer);
begin
  POSTGRESQL_API.PQtrace(Handle, DebugPort);
end;

function TZPostgreSQLBaseDriver.UnescapeBytea(const from: PAnsiChar;
  to_lenght: PLongword): PAnsiChar;
begin
  Result := POSTGRESQL_API.PQUnescapeBytea(from, to_lenght);
end;

function TZPostgreSQLBaseDriver.UnlinkLargeObject(
  Handle: PZPostgreSQLConnect; ObjId: Oid): Integer;
begin
  Result := POSTGRESQL_API.lo_unlink(Handle, ObjId);
end;

procedure TZPostgreSQLBaseDriver.Untrace(Handle: PZPostgreSQLConnect);
begin
  POSTGRESQL_API.PQuntrace(Handle);
end;

function TZPostgreSQLBaseDriver.WriteLargeObject(
  Handle: PZPostgreSQLConnect; Fd: Integer; Buffer: PAnsiChar;
  Length: Integer): Integer;
begin
  Result := POSTGRESQL_API.lo_write(Handle, Fd, Buffer, Length);
end;

function TZPostgreSQLBaseDriver.GetPlainFunc():PAPI;
begin
  result:= @POSTGRESQL_API;
end;

function TZPostgreSQLBaseDriver.EscapeBytea(Handle: PGconn; from: PAnsiChar;
  from_length: LongWord; to_lenght: PLongword): PAnsiChar;
begin
  if assigned(POSTGRESQL_API.PQescapeByteaConn) then
    Result := POSTGRESQL_API.PQescapeByteaConn(Handle, from, from_length, to_lenght)
  else if Assigned(POSTGRESQL_API.PQescapeBytea) then
    Result := POSTGRESQL_API.PQescapeBytea(from,from_length,to_lenght)
  else
    raise Exception.Create('can''t escape bytea ');
end;

function TZPostgreSQLBaseDriver.EscapeString(Handle: PGconn; ToChar: PAnsiChar;
  const FromChar: PAnsiChar; length: NativeUInt; error: PInteger): NativeUInt;
begin
  Error^ := 0;
  if (FromChar = nil) or (Length = 0) then
    Result := 0
  else if Assigned(POSTGRESQL_API.PQescapeStringConn) then
    Result := POSTGRESQL_API.PQescapeStringConn(Handle, ToChar, FromChar, Length, error)
  else if Assigned(POSTGRESQL_API.PQescapeString) then
    Result := POSTGRESQL_API.PQescapeString(ToChar, FromChar, Length)
  else
    raise Exception.Create('can''t escape the string!');
end;

function TZPostgreSQLBaseDriver.PQsetSingleRowMode(Handle: PZPostgreSQLConnect): Integer; //PG9+
begin
  //http://www.postgresql.org/docs/9.2/static/libpq-single-row-mode.html
  if Assigned(POSTGRESQL_API.PQsetSingleRowMode) then
    Result := POSTGRESQL_API.PQsetSingleRowMode(Handle)
  else
    Result := 0;
end;

{ TZPostgreSQL7PlainDriver }

function TZPostgreSQL7PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZPostgreSQL7PlainDriver.Create;
end;

constructor TZPostgreSQL7PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL7_LOCATION);
  {$ENDIF}
end;

function TZPostgreSQL7PlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-7';
end;

function TZPostgreSQL7PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 7.x';
end;

{ TZPostgreSQL8PlainDriver }
function TZPostgreSQL8PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZPostgreSQL8PlainDriver.Create;
end;

function TZPostgreSQL8PlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF8';
end;

procedure TZPostgreSQL8PlainDriver.LoadCodePages;
begin
  inherited LoadCodePages;
  { Version 8.1 }
  {MultiByte}
  ResetCodePage(Ord(csUNICODE_PODBC), 'UTF8', Ord(csUTF8), ceUTF8, zCP_UTF8, '', 4); { Unicode, 8-bit 	all }
  AddCodePage('BIG5', Ord(csBIG5), ceAnsi, zCP_Big5, '', 2); { Big Five 	Traditional Chinese }
  AddCodePage('GB18030', Ord(csGB18030), ceAnsi, zCP_GB18030, '', 2); { National Standard 	Chinese }
  AddCodePage('GBK', Ord(csGBK), ceAnsi, zCP_GB2312, '', 2); { Extended National Standard 	Simplified Chinese }
  AddCodePage('SJIS', Ord(csSJIS), ceAnsi, zCP_SHIFTJS, '', 2); { Shift JIS 	Japanese }
  AddCodePage('UHC', Ord(csUHC), ceAnsi, zCP_EUCKR, '', 2); { Unified Hangul Code Korean }
  {SingleByte}
  ResetCodePage(Ord(csALT), 'WIN866', Ord(csWIN866), ceAnsi, zCP_DOS866); { Windows CP866 	Cyrillic } //No longer in use
  AddCodePage('WIN874', Ord(csWIN874), ceAnsi, zCP_WIN874); { Windows CP874 	Thai }
  AddCodePage('WIN1250', Ord(csWIN1250), ceAnsi, zCP_WIN1250); { Windows CP1250 	Central European }
  ResetCodePage(Ord(csWIN), 'WIN1251', Ord(csWIN1251), ceAnsi, zCP_WIN1251); { Windows CP1251 	Cyrillic } //No longer in use
  AddCodePage('WIN1252', Ord(csWIN1252), ceAnsi, zCP_WIN1252); { Windows CP1252 	Western European }
  ResetCodePage(Ord(csTCVN), 'WIN1258', Ord(csWIN1258),ceAnsi, zCP_WIN1258); { Windows CP1258 	Vietnamese } //No longer in use

  { Version 8.3 }
  {MultiByte}
  AddCodePage('EUC_JIS_2004', Ord(csEUC_JIS_2004), ceAnsi, $ffff, '', 3); { Extended UNIX Code-JP, JIS X 0213 	Japanese }
  AddCodePage('SHIFT_JIS_2004', Ord(csSHIFT_JIS_2004), ceAnsi, zCP_SHIFTJS, '', 3); { Shift JIS, JIS X 0213 	Japanese }
  {SingleChar}
  AddCodePage('WIN1253', Ord(csWIN1253), ceAnsi, zCP_WIN1253); { Windows CP1253  Greek }
  AddCodePage('WIN1254', Ord(csWIN1254), ceAnsi, zCP_WIN1254); { Windows CP1254 	Turkish }
  AddCodePage('WIN1255', Ord(csWIN1255), ceAnsi, zCP_WIN1255); { Windows CP1255 	Hebrew }
  AddCodePage('WIN1257', Ord(csWIN1257), ceAnsi, zCP_WIN1257); { Windows CP1257 	Baltic }

  { Version 8.4 }
  {SingleChar}
  AddCodePage('KOI8U', Ord(csKOI8U), ceAnsi, zCP_KOI8U); { 	KOI8-U 	Cyrillic (Ukrainian) }
end;

constructor TZPostgreSQL8PlainDriver.Create;
begin
  inherited Create;
  {$IFNDEF UNIX}
    FLoader.AddLocation(WINDOWS_DLL8_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL82_LOCATION);
    FLoader.AddLocation(LINUX_DLL8_LOCATION);
  {$ENDIF}
end;

function TZPostgreSQL8PlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-8';
end;

function TZPostgreSQL8PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 8.x';
end;

{ TZPostgreSQL9PlainDriver }
function TZPostgreSQL9PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZPostgreSQL9PlainDriver.Create;
end;

procedure TZPostgreSQL9PlainDriver.LoadCodePages;
begin
  inherited LoadCodePages;
  ResetCodePage(Ord(csKOI8), 'KOI8R', Ord(csKOI8R)); { KOI8-R 	Cyrillic (Russian) } //No longer in use
end;

constructor TZPostgreSQL9PlainDriver.Create;
begin
  inherited Create;
  Self.FLoader.ClearLocations;
  {$IFNDEF STRICT_DLL_LOADING}
  {$IFDEF MSWINDOWS}
      FLoader.AddLocation(WINDOWS_DLL_LOCATION);
    {$ELSE}
      FLoader.AddLocation(LINUX_DLL9_LOCATION);
    {$ENDIF}
  {$ENDIF}
end;

function TZPostgreSQL9PlainDriver.GetProtocol: string;
begin
  Result := 'postgresql-9';
end;

function TZPostgreSQL9PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for PostgreSQL 9.x';
end;

function TZPostgreSQL9PlainDriver.GetStandardConformingStrings: Boolean;
begin
  Result := True;
end;
{$ENDIF ZEOS_DISABLE_POSTGRESQL}
end.
