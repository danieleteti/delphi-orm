{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Delphi plain driver interface to DBLibrary         }
{                                                         }
{        Originally written by Janos Fegyverneki          }
{         FreeTDS supportd by Bogdan Dragulin             }
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

unit ZPlainDbLibConstants;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB}

uses // M.A.
   ZCompatibility; // M.A.
   
{***************** Plain API Constants definition ****************}
const
{ General  #define }
  TIMEOUT_IGNORE        = Cardinal(-1);
  TIMEOUT_INFINITE      = 0;
  TIMEOUT_MAXIMUM       = 1200*1000; { 20 minutes maximum timeout value in ms}

{ Used for ServerType in dbgetprocinfo }
  SERVTYPE_UNKNOWN      = 0;
  SERVTYPE_MICROSOFT    = 1;

{ Used by dbcolinfo }
{enum CI_TYPES }
  CI_REGULAR            = 1;
  CI_ALTERNATE          = 2;
  CI_CURSOR             = 3;

{ Bulk Copy Definitions (bcp) }
  DB_IN	                = 1;  { Transfer from client to server }
  DB_OUT	              = 2;  { Transfer from server to client }

  BCPMAXERRS            = 1;  { bcp_control parameter }
  BCPFIRST              = 2;  { bcp_control parameter }
  BCPLAST               = 3;  { bcp_control parameter }
  BCPBATCH              = 4;  { bcp_control parameter }
  BCPKEEPNULLS          = 5;  { bcp_control parameter }
  BCPABORT              = 6;  { bcp_control parameter }
  BCPKEEPIDENTITY	      = 8;  { bcp_control parameter }

  BCPLABELED            = 5;  { bcp_control parameter }
  BCPHINTS              = 6;  { bcp_control parameter }

  DBCMDNONE             = 0;  { bcp_control parameter }
  DBCMDPEND             = 1;  { bcp_control parameter }
  DBCMDSENT             = 2;  { bcp_control parameter }

  TINYBIND              = 1;
  SMALLBIND             = 2;
  INTBIND               = 3;
  CHARBIND              = 4;
  BINARYBIND            = 5;
  BITBIND               = 6;
  DATETIMEBIND          = 7;
  MONEYBIND             = 8;
  FLT8BIND              = 9;
  STRINGBIND            = 10;
  NTBSTRINGBIND         = 11;
  VARYCHARBIND          = 12;
  VARYBINBIND           = 13;
  FLT4BIND              = 14;
  SMALLMONEYBIND        = 15;
  SMALLDATETIBIND       = 16;
  DECIMALBIND           = 17;
  NUMERICBIND           = 18;
  SRCDECIMALBIND        = 19;
  SRCNUMERICBIND        = 20;
  MAXBIND               = SRCNUMERICBIND;

  DBSAVE                = 1;
  DBNOSAVE              = 0;

  DBNOERR               = -1;
  DBFAIL                = 0;
  DBSUCCEED             = 1;

  DBFINDONE             = $04;  { Definately done }
  DBMORE                = $10;  { Maybe more commands waiting }
  DBMORE_ROWS           = $20;  { This command returned rows }

  MAXNAME               = 31;
  DBTXTSLEN             = 8;     { Timestamp length }
  DBTXPLEN              = 16;    { Text pointer length }

{ Error code returns }
  INT_EXIT              = 0;
  INT_CONTINUE          = 1;
  INT_CANCEL            = 2;

  //from FreeTDS sybdb.h:
{ DBVERSION_xxx are used with dbsetlversion() }
  DBVERSION_100= 2; // Sybase TDS 5.0
  DBVERSION_42 = 3; // This can be used for old Microsoft and Sybase servers
  DBVERSION_70 = 4;
  DBVERSION_71 = 5;
  DBVERSION_72 = 6;
  DBVERSION_73 = 7;

{Zeos dbsetversion placeholders}
  ZVersion_UNKNOWN           = 0;
  ZVersion_2_0               = 1;	{ pre 4.0 SQL Server }
  ZVersion_3_4               = 2;	{ Microsoft SQL Server (3.0) }
  ZVersion_4_0               = 3;	{ 4.0 SQL Server }
  ZVersion_4_2               = 4;	{ 4.2 SQL Server }
  ZVersion_4_6               = 5;	{ 2.0 OpenServer and 4.6 SQL Server. }
  ZVersion_4_9_5             = 6;	{ 4.9.5 (NCR) SQL Server }
  ZVersion_5_0               = 7;	{ 5.0 SQL Server }
  ZVersion_7_0               = 8;	{ Microsoft SQL Server 7.0 }
  ZVersion_8_0               = 9;	{ Microsoft SQL Server 2000 }
  ZVersion_9_0               = 10;	{ Microsoft SQL Server 2005 }
  ZVersion_7_1               = 9;	{ Microsoft SQL Server 2000 }
  ZVersion_7_2               = 10;	{ Microsoft SQL Server 2005 }
  ZVersion_7_3               = 11;	{ Microsoft SQL Server 2008 }

  ZVersionMax                = 13; { known count of available versions }
  ZVersionEmpty              = -1; { placeholder for unsuported version }

{ DB-Library datatypes }
const
{Zeos DBOption placeholders}
{ a large list of options, DBTEXTSIZE is needed by sybtcl }
  Z_PARSEONLY             = 0;
  Z_ESTIMATE              = 1;
  Z_SHOWPLAN              = 2;
  Z_NOEXEC                = 3;
  Z_ARITHIGNORE           = 4;
  Z_NOCOUNT               = 5;
  Z_ARITHABORT            = 6;
  Z_TEXTLIMIT             = 7;
  Z_BROWSE                = 8;
  Z_OFFSET                = 9;
  Z_STAT                  = 10;
  Z_ERRLVL                = 11;
  Z_CONFIRM               = 12;
  Z_STORPROCID            = 13;
  Z_BUFFER                = 14;
  Z_NOAUTOFREE            = 15;
  Z_ROWCOUNT              = 16;
  Z_TEXTSIZE              = 17;
  Z_NATLANG               = 18;
  Z_DATEFORMAT            = 19;
  Z_PRPAD                 = 20;
  Z_PRCOLSEP              = 21;
  Z_PRLINELEN             = 22;
  Z_PRLINESEP             = 23;
  Z_LFCONVERT             = 24;
  Z_DATEFIRST             = 25;
  Z_CHAINXACTS            = 26;
  Z_FIPSFLAG              = 27;
  Z_ISOLATION             = 28;
  Z_AUTH                  = 29;
  Z_IDENTITY              = 30;
  Z_NOIDCOL               = 31;
  Z_DATESHORT             = 32;
  Z_CLIENTCURSORS         = 33;
  Z_SETTIME               = 34;
  Z_QUOTEDIDENT           = 35;
  Z_NUMOPTIONS            = 36;
  Z_PADOFF                = 37;
  Z_PADON                 = 38;
  Z_OFF                   = 39;
  Z_ON                    = 40;
  Z_NOSUCHOPTION          = 41;
  Z_MAXOPTTEXT            = 42;
  Z_ANSITOOEM             = 43;
  Z_OEMTOANSI             = 44;

{ loginrec manipulation Placeholders}
  Z_SETHOST               = 0;
  Z_SETUSER               = 1;
  Z_SETPWD                = 2;
  Z_SETHID                = 3;
  Z_SETAPP                = 4;
  Z_SETBCP                = 5;
  Z_SETSECURE             = 6;
  Z_SETLANG               = 7;
  Z_SETNOSHORT            = 8;
  Z_SETHIER               = 9;
  Z_SETCHARSET            = 10;
  Z_SETPACKET             = 11;
  Z_SETENCRYPT            = 12;
  Z_SETLABELED            = 13;
  Z_SETDBNAME             = 14;
  Z_SETLOGINTIME          = 15;
  Z_SETFALLBACK           = 16;

{ DBLib options }
const
  DBLIBDBBUFFER               = 0;
  DBLIBDBOFFSET               = 1;
  DBLIBDBROWCOUNT             = 2;
  DBLIBDBSTAT                 = 3;
  DBLIBDBTEXTLIMIT            = 4;
  DBLIBDBTEXTSIZE             = 5;
  DBLIBDBARITHABORT           = 6;
  DBLIBDBARITHIGNORE          = 7;
  DBLIBDBNOAUTOFREE           = 8;
  DBLIBDBNOCOUNT              = 9;
  DBLIBDBNOEXEC               = 10;
  DBLIBDBPARSEONLY            = 11;
  DBLIBDBSHOWPLAN             = 12;
  DBLIBDBSTORPROCID           = 13;
  DBLIBDBANSITOOEM		        = 14;
  DBLIBDBOEMTOANSI	          = 15;
  DBLIBDBCLIENTCURSORS        = 16;
  DBLIBDBSET_TIME             = 17;
  DBLIBDBQUOTEDIDENT          = 18;

{ FreeTDS options, a large list of options, DBTEXTSIZE is needed by sybtcl }
  TDSPARSEONLY             = 0;
  TDSESTIMATE              = 1;
  TDSSHOWPLAN              = 2;
  TDSNOEXEC                = 3;
  TDSARITHIGNORE           = 4;
  TDSNOCOUNT               = 5;
  TDSARITHABORT            = 6;
  TDSTEXTLIMIT             = 7;
  TDSBROWSE                = 8;
  TDSOFFSET                = 9;
  TDSSTAT                  = 10;
  TDSERRLVL                = 11;
  TDSCONFIRM               = 12;
  TDSSTORPROCID            = 13;
  TDSBUFFER                = 14;
  TDSNOAUTOFREE            = 15;
  TDSROWCOUNT              = 16;
  TDSTEXTSIZE              = 17;
  TDSNATLANG               = 18;
  TDSDATEFORMAT            = 19;
  TDSPRPAD                 = 20;
  TDSPRCOLSEP              = 21;
  TDSPRLINELEN             = 22;
  TDSPRLINESEP             = 23;
  TDSLFCONVERT             = 24;
  TDSDATEFIRST             = 25;
  TDSCHAINXACTS            = 26;
  TDSFIPSFLAG              = 27;
  TDSISOLATION             = 28;
  TDSAUTH                  = 29;
  TDSIDENTITY              = 30;
  TDSNOIDCOL               = 31;
  TDSDATESHORT             = 32;
  TDSCLIENTCURSORS         = 33;
  TDSSETTIME               = 34;
  TDSQUOTEDIDENT           = 35;
  TDSNUMOPTIONS             = 36;
  TDSPADOFF                 = 0;
  TDSPADON                  = 1;
  TDSOFF                    = 0;
  TDSON                     = 1;

  NOSUCHOPTION              = 2;

  MAXOPTTEXT                = 32;

{ common Login manipulations }
  DBSETHOST                 = 1;
  DBSETUSER                 = 2;
  DBSETPWD                  = 3;
{ Sybase Login manipulations }
const
  SYBDBSETHOST              = DBSETHOST;
  SYBDBSETUSER              = DBSETUSER;
  SYBDBSETPWD               = DBSETPWD;
  SYBDBSETHID               = 4;
  SYBDBSETAPP               = 5;
  SYBDBSETBCP               = 6;
  SYBDBSETLANG              = 7;
  SYBDBSETNOSHORT           = 8;
  SYBDBSETHIER              = 9;
  SYBDBSETCHARSET           = 10;
  SYBDBSETPACKET            = 11;
  SYBDBSETENCRYPT           = 12;
  SYBDBSETLABELED           = 13;

{ MsSQL Login manipulations }
const
  MSDBSETHOST               = DBSETHOST;
  MSDBSETUSER               = DBSETUSER;
  MSDBSETPWD                = DBSETPWD;
  MSDBSETAPP                = 4;
  MSDBSETID                 = 5;
  MSDBSETLANG               = 6;
  MSDBSETSECURE             = 7;
  MSDBSET_LOGIN_TIME        = 10;
  MSDBSETFALLBACK           = 12;

{TDS Loginrec manipulations}
  TDSDBSETHOST               = DBSETHOST;
  TDSDBSETUSER               = DBSETUSER;
  TDSDBSETPWD                = DBSETPWD;
  TDSDBSETHID                = 4;
  TDSDBSETAPP                = 5;
  TDSDBSETBCP                = 6;
  TDSDBSETSECURE             = 6;
  TDSDBSETLANG               = 7;
  TDSDBSETNOSHORT            = 8;
  TDSDBSETHIER               = 9;
  TDSDBSETCHARSET            = 10;
  TDSDBSETPACKET             = 11;
  TDSDBSETENCRYPT            = 12;
  TDSDBSETLABELED            = 13;
  TDSDBSETDBNAME             = 14;

{ TDS_DBVERSION_xxx are used with dbsetversion() }
  TDSDBVERSION_UNKNOWN  = 0;
  TDSDBVERSION_46       = 1;
  TDSDBVERSION_100      = 2; // Sybase TDS 5.0
  TDSDBVERSION_42       = 3; // This can be used for old Microsoft and Sybase servers
  TDSDBVERSION_70       = 4;
  TDSDBVERSION_71       = 5;
  TDSDBVERSION_80       = TDSDBVERSION_71;
  TDSDBVERSION_72       = 6;
  TDSDBVERSION_73       = 7;

{ these two are defined by Microsoft for dbsetlversion() }
  DBVER42 	            = 8;
  DBVER60 	            = 9;

(**
 * DBTDS_xxx are returned by DBTDS()
 * The integer values of the constants are poorly chosen.
 *)
  DBTDS_UNKNOWN           = 0;
  DBTDS_2_0               = 1;	{ pre 4.0 SQL Server }
  DBTDS_3_4               = 2;	{ Microsoft SQL Server (3.0) }
  DBTDS_4_0               = 3;	{ 4.0 SQL Server }
  DBTDS_4_2               = 4;	{ 4.2 SQL Server }
  DBTDS_4_6               = 5;	{ 2.0 OpenServer and 4.6 SQL Server. }
  DBTDS_4_9_5             = 6;	{ 4.9.5 (NCR) SQL Server }
  DBTDS_5_0               = 7;	{ 5.0 SQL Server }
  DBTDS_7_0               = 8;	{ Microsoft SQL Server 7.0 }
  DBTDS_8_0               = 9;	{ Microsoft SQL Server 2000 }
  DBTDS_9_0               = 10;	{ Microsoft SQL Server 2005 }
  DBTDS_7_1               = 9;	{ Microsoft SQL Server 2000 }
  DBTDS_7_2               = 10;	{ Microsoft SQL Server 2005 }
  DBTDS_7_3               = 11;	{ Microsoft SQL Server 2008 }


{ Data stream tokens }
  SQLCOLFMT             = $a1;
  OLD_SQLCOLFMT         = $2a;
  SQLPROCID             = $7c;
  SQLCOLNAME            = $a0;
  SQLTABNAME            = $a4;
  SQLCOLINFO            = $a5;
  SQLALTNAME            = $a7;
  SQLALTFMT             = $a8;
  SQLERROR              = $aa;
  SQLINFO               = $ab;
  SQLRETURNVALUE        = $ac;
  SQLRETURNSTATUS       = $79;
  SQLRETURN             = $db;
  SQLCONTROL            = $ae;
  SQLALTCONTROL         = $af;
  SQLROW                = $d1;
  SQLALTROW             = $d3;
  SQLDONE               = $fd;
  SQLDONEPROC           = $fe;
  SQLDONEINPROC         = $ff;
  SQLOFFSET             = $78;
  SQLORDER              = $a9;
  SQLLOGINACK           = $ad; { NOTICE: change to real value }

{ Ag op tokens }
  SQLAOPCNT		= $4b;
  SQLAOPSUM             = $4d;
  SQLAOPAVG             = $4f;
  SQLAOPMIN             = $51;
  SQLAOPMAX             = $52;
  SQLAOPANY             = $53;
  SQLAOPNOOP            = $56;

{ Error numbers (dberrs) DB-Library error codes }
  SQLEMEM               = 10000;
  SQLENULL              = 10001;
  SQLENLOG              = 10002;
  SQLEPWD               = 10003;
  SQLECONN              = 10004;
  SQLEDDNE              = 10005;
  SQLENULLO             = 10006;
  SQLESMSG              = 10007;
  SQLEBTOK              = 10008;
  SQLENSPE              = 10009;
  SQLEREAD              = 10010;
  SQLECNOR              = 10011;
  SQLETSIT              = 10012;
  SQLEPARM              = 10013;
  SQLEAUTN              = 10014;
  SQLECOFL              = 10015;
  SQLERDCN              = 10016;
  SQLEICN               = 10017;
  SQLECLOS              = 10018;
  SQLENTXT              = 10019;
  SQLEDNTI              = 10020;
  SQLETMTD              = 10021;
  SQLEASEC              = 10022;
  SQLENTLL              = 10023;
  SQLETIME              = 10024;
  SQLEWRIT              = 10025;
  SQLEMODE              = 10026;
  SQLEOOB               = 10027;
  SQLEITIM              = 10028;
  SQLEDBPS              = 10029;
  SQLEIOPT              = 10030;
  SQLEASNL              = 10031;
  SQLEASUL              = 10032;
  SQLENPRM              = 10033;
  SQLEDBOP              = 10034;
  SQLENSIP              = 10035;
  SQLECNULL             = 10036;
  SQLESEOF              = 10037;
  SQLERPND              = 10038;
  SQLECSYN              = 10039;
  SQLENONET             = 10040;
  SQLEBTYP              = 10041;
  SQLEABNC              = 10042;
  SQLEABMT              = 10043;
  SQLEABNP              = 10044;
  SQLEBNCR              = 10045;
  SQLEAAMT              = 10046;
  SQLENXID              = 10047;
  SQLEIFNB              = 10048;
  SQLEKBCO              = 10049;
  SQLEBBCI              = 10050;
  SQLEKBCI              = 10051;
  SQLEBCWE              = 10052;
  SQLEBCNN              = 10053;
  SQLEBCOR              = 10054;
  SQLEBCPI              = 10055;
  SQLEBCPN              = 10056;
  SQLEBCPB              = 10057;
  SQLEVDPT              = 10058;
  SQLEBIVI              = 10059;
  SQLEBCBC              = 10060;
  SQLEBCFO              = 10061;
  SQLEBCVH              = 10062;
  SQLEBCUO              = 10063;
  SQLEBUOE              = 10064;
  SQLEBWEF              = 10065;
  SQLEBTMT              = 10066;
  SQLEBEOF              = 10067;
  SQLEBCSI              = 10068;
  SQLEPNUL              = 10069;
  SQLEBSKERR            = 10070;
  SQLEBDIO              = 10071;
  SQLEBCNT              = 10072;
  SQLEMDBP              = 10073;
  SQLINIT               = 10074;
  SQLCRSINV             = 10075;
  SQLCRSCMD             = 10076;
  SQLCRSNOIND           = 10077;
  SQLCRSDIS             = 10078;
  SQLCRSAGR             = 10079;
  SQLCRSORD             = 10080;
  SQLCRSMEM             = 10081;
  SQLCRSBSKEY           = 10082;
  SQLCRSNORES           = 10083;
  SQLCRSVIEW            = 10084;
  SQLCRSBUFR            = 10085;
  SQLCRSFROWN           = 10086;
  SQLCRSBROL            = 10087;
  SQLCRSFRAND           = 10088;
  SQLCRSFLAST           = 10089;
  SQLCRSRO              = 10090;
  SQLCRSTAB             = 10091;
  SQLCRSUPDTAB          = 10092;
  SQLCRSUPDNB           = 10093;
  SQLCRSVIIND           = 10094;
  SQLCRSNOUPD           = 10095;
  SQLCRSOS2             = 10096;
  SQLEBCSA              = 10097;
  SQLEBCRO              = 10098;
  SQLEBCNE              = 10099;
  SQLEBCSK              = 10100;
  SQLEUVBF              = 10101;
  SQLEBIHC              = 10102;
  SQLEBWFF              = 10103;
  SQLNUMVAL             = 10104;
  SQLEOLDVR             = 10105;
  SQLEBCPS	            = 10106;
  SQLEDTC 	            = 10107;
  SQLENOTIMPL	          = 10108;
  SQLENONFLOAT	        = 10109;
  SQLECONNFB            = 10110;

{ The severity levels are defined here }
  EXINFO                = 1;  { Informational, non-error }
  EXUSER                = 2;  { User error }
  EXNONFATAL            = 3;  { Non-fatal error }
  EXCONVERSION          = 4;  { Error in DB-LIBRARY data conversion }
  EXSERVER              = 5;  { The Server has returned an error flag }
  EXTIME                = 6;  { We have exceeded our timeout period while }
                           { waiting for a response from the Server - the }
                           { DBPROCESS is still alive }
  EXPROGRAM             = 7;  { Coding error in user program }
  EXRESOURCE            = 8;  { Running out of resources - the DBPROCESS may be dead }
  EXCOMM                = 9;  { Failure in communication with Server - the DBPROCESS is dead }
  EXFATAL               = 10; { Fatal error - the DBPROCESS is dead }
  EXCONSISTENCY         = 11; { Internal software error  - notify MS Technical Supprt }

{ Offset identifiers }
  OFF_SELECT            = $16d;
  OFF_FROM              = $14f;
  OFF_ORDER             = $165;
  OFF_COMPUTE           = $139;
  OFF_TABLE             = $173;
  OFF_PROCEDURE         = $16a;
  OFF_STATEMENT         = $1cb;
  OFF_PARAM             = $1c4;
  OFF_EXEC              = $12c;

{ Decimal constants }
  MAXNUMERICLEN = 16;
  MAXNUMERICDIG = 38;

  DEFAULTPRECISION = 18;
  DEFAULTSCALE     = 0;

{ DB-Table constants}
{ Pack the following structures on a word boundary }
  TDSMAXTABLENAME  = 512;
  TDSMAXCOLNAMELEN = 512;

{ DB-Table constants}
{ Pack the following structures on a word boundary }
  MAXTABLENAME = 30;
  MAXCOLNAMELEN= 30;

{ DB-Library datatype definitions }
  DBMAXCHAR=256; // Max length of DBVARBINARY and DBVARCHAR, etc.

{ Print lengths for certain fixed length data types }
  PRINT4                = 11;
  PRINT2                = 6;
  PRINT1                = 3;
  PRFLT8                = 20;
  PRMONEY               = 26;
  PRBIT                 = 3;
  PRDATETIME            = 27;
  PRDECIMAL             = (MAXNUMERICDIG + 2);
  PRNUMERIC             = (MAXNUMERICDIG + 2);

  SUCCEED               = 1;
  FAIL                  = 0;
  SUCCEED_ABORT         = 2;

  DBUNKNOWN             = 2; { FALSE = 0, TRUE = 1 }

  MORE_ROWS             = -1;
  NO_MORE_ROWS          = -2;
  REG_ROW               = MORE_ROWS;
  BUF_FULL              = -3; { only if buffering is turned on }

{ Status code for dbresults(). Possible return values are }
{ SUCCEED, FAIL, and NO_MORE_RESULTS. }
  NO_MORE_RESULTS       = 2;
  NO_MORE_RPC_RESULTS   = 3;

{ Standard exit and error values }
  STDEXIT               = 0;
  ERREXIT               = -1;

{ dbrpcinit flags }
  DBRPCRECOMPILE        = $0001;
  DBRPCRESET            = $0004;
  DBRPCCURSOR           = $0008;

{ dbrpcparam flags }
  DBRPCRETURN           = $1;
  DBRPCDEFAULT          = $2;

{ Cursor related constants }

{ Following flags are used in the concuropt parameter in the dbcursoropen function }
  CUR_READONLY          = 1; { Read only cursor, no data modifications }
  CUR_LOCKCC            = 2; { Intent to update, all fetched data locked when }
                       { dbcursorfetch is called inside a transaction block }
  CUR_OPTCC             = 3; { Optimistic concurrency control, data modifications }
                       { succeed only if the row hasn't been updated since }
                       { the last fetch. }
  CUR_OPTCCVAL          = 4; { Optimistic concurrency control based on selected column values }

{ Following flags are used in the scrollopt parameter in dbcursoropen }
  CUR_FORWARD           = 0;   { Forward only scrolling }
  CUR_KEYSET            = -1;  { Keyset driven scrolling }
  CUR_DYNAMIC           = 1;   { Fully dynamic }
  CUR_INSENSITIVE       = -2;  { Server-side cursors only }

{ Following flags define the fetchtype in the dbcursorfetch function }
  FETCH_FIRST           = 1;  { Fetch first n rows }
  FETCH_NEXT            = 2;  { Fetch next n rows }
  FETCH_PREV            = 3;  { Fetch previous n rows }
  FETCH_RANDOM          = 4;  { Fetch n rows beginning with given row # }
  FETCH_RELATIVE        = 5;  { Fetch relative to previous fetch row # }
  FETCH_LAST            = 6;  { Fetch the last n rows }

{ Following flags define the per row status as filled by dbcursorfetch and/or dbcursorfetchex }
  FTC_EMPTY             = $00;  { No row available }
  FTC_SUCCEED           = $01;  { Fetch succeeded, (failed if not set) }
  FTC_MISSING           = $02;  { The row is missing }
  FTC_ENDOFKEYSET       = $04;  { End of the keyset reached }
  FTC_ENDOFRESULTS      = $08;  { End of results set reached }

{ Following flags define the operator types for the dbcursor function }
  CRS_UPDATE            = 1;  { Update operation }
  CRS_DELETE            = 2;  { Delete operation }
  CRS_INSERT            = 3;  { Insert operation }
  CRS_REFRESH           = 4;  { Refetch given row }
  CRS_LOCKCC            = 5;  { Lock given row }

{ Following value can be passed to the dbcursorbind function for NOBIND type }
  NOBIND                = -2; { Return length and pointer to data }

{ Following are values used by DBCURSORINFO's Type parameter }
  CU_CLIENT             = $00000001;
  CU_SERVER             = $00000002;
  CU_KEYSET             = $00000004;
  CU_MIXED              = $00000008;
  CU_DYNAMIC            = $00000010;
  CU_FORWARD            = $00000020;
  CU_INSENSITIVE        = $00000040;
  CU_READONLY           = $00000080;
  CU_LOCKCC             = $00000100;
  CU_OPTCC              = $00000200;
  CU_OPTCCVAL           = $00000400;

{ Following are values used by DBCURSORINFO's Status parameter }
  CU_FILLING            = $00000001;
  CU_FILLED             = $00000002;

{ Following are values used by dbupdatetext's type parameter }
  UT_TEXTPTR            = $0001;
  UT_TEXT               = $0002;
  UT_MORETEXT           = $0004;
  UT_DELETEONLY         = $0008;
  UT_LOG                = $0010;

{ The following values are passed to dbserverenum for searching criteria. }
  NET_SEARCH            = $0001;
  LOC_SEARCH            = $0002;

{ These constants are the possible return values from dbserverenum. }
  ENUM_SUCCESS          = $0000;
  MORE_DATA             = $0001;
  NET_NOT_AVAIL         = $0002;
  OUT_OF_MEMORY         = $0004;
  NOT_SUPPORTED         = $0008;
  ENUM_INVALID_PARAM    = $0010;

{ Netlib Error problem codes.  ConnectionError() should return one of }
{ these as the dblib-mapped problem code, so the corresponding string }
{ is sent to the dblib app's error handler as dberrstr.  Return NE_E_NOMAP }
{ for a generic DB-Library error string (as in prior versions of dblib). }

  NE_E_NOMAP            = 0;   { No string; uses dblib default. }
  NE_E_NOMEMORY         = 1;   { Insufficient memory. }
  NE_E_NOACCESS         = 2;   { Access denied. }
  NE_E_CONNBUSY         = 3;   { Connection is busy. }
  NE_E_CONNBROKEN       = 4;   { Connection broken. }
  NE_E_TOOMANYCONN      = 5;   { Connection limit exceeded. }
  NE_E_SERVERNOTFOUND   = 6;   { Specified SQL server not found. }
  NE_E_NETNOTSTARTED    = 7;   { The network has not been started. }
  NE_E_NORESOURCE       = 8;   { Insufficient network resources. }
  NE_E_NETBUSY          = 9;   { Network is busy. }
  NE_E_NONETACCESS      = 10;  { Network access denied. }
  NE_E_GENERAL          = 11;  { General network error.  Check your documentation. }
  NE_E_CONNMODE         = 12;  { Incorrect connection mode. }
  NE_E_NAMENOTFOUND     = 13;  { Name not found in directory service. }
  NE_E_INVALIDCONN      = 14;  { Invalid connection. }
  NE_E_NETDATAERR       = 15;  { Error reading or writing network data. }
  NE_E_TOOMANYFILES     = 16;  { Too many open file handles. }
  NE_E_CANTCONNECT	    = 17;  { SQL Server does not exist or access denied. }

  NE_MAX_NETERROR       = 17;


const
  MAXSERVERNAME = 30;
  MAXNETLIBNAME = 255;
  MAXNETLIBCONNSTR = 255;

const
  INVALID_UROWNUM       = Cardinal(-1);


{ common TDS protocol Data Type mapping of ntwdblib, dblib, freeTDS }
type
  { tabular data stream protocol types }
  //Enum                  ordinal-value
  TTDSType = (
    tdsVoid               = 31,
    tdsImage              = 34,
    tdsText               = 35,
    tdsUnique             = 36, //Unique identifier type
    tdsVarBinary          = 37,
    tdsIntN               = 38,
    tdsVarchar            = 39,
    tdsBinary             = 45,
    tdsChar               = 47,
    tdsInt1               = 48,
    tdsBit                = 50,
    tdsInt2               = 52,
    tdsInt4               = 56,
    tdsDateTime4          = 58,
    tdsFlt4               = 59,
    tdsMoney              = 60,
    tdsDateTime           = 61,
    tdsFlt8               = 62,
    tdsVariant            = 98, {from tds.h -> sybase only}
    tdsNText              = 99, {from tds.h -> sybase only}
    tdsNVarChar           = 103, {from tds.h -> sybase only}
    tdsBitN               = 104, {from tds.h -> sybase only}
    tdsDecimal            = 106,
    tdsNumeric            = 108,
    tdsFltN               = 109,
    tdsMoneyN             = 110,
    tdsDateTimeN          = 111,
    tdsMoney4             = 122,
    {from tds.h -> sade, sybase only}
    tdsInt8                  = 127,
    tdsBigVarBinary          = 165,
    tdsBigVarChar            = 167,
    tdsBigBinary             = 173,
    tdsBigChar               = 175,
    tdsSybaseLongBinary      = 225,
    tdsBigNVarChar           = 231,
    tdsBigNChar              = 239,
    tdsUDT                   = 240,
    tdsMSXML                 = 241
    );

const
{ different type mappings / copied from tds.h -> sybase only!}
  SYBAOPCNT             = $4b;
  SYBAOPCNTU            = $4c;
  SYBAOPSUM             = $4d;
  SYBAOPSUMU            = $4e;
  SYBAOPAVG             = $4f;
  SYBAOPAVGU            = $50;
  SYBAOPMIN             = $51;
  SYBAOPMAX             = $52;

{ mssql2k compute operator }
  SYBAOPCNT_BIG		      = $09;
  SYBAOPSTDEV		        = $30;
  SYBAOPSTDEVP		      = $31;
  SYBAOPVAR		          = $32;
  SYBAOPVARP		        = $33;
  SYBAOPCHECKSUM_AGG	  = $72;
  {****************** Plain API Types definition *****************}
type
{ DBPROCESS, LOGINREC and DBCURSOR }
  PDBPROCESS            = Pointer;
  PLOGINREC             = Pointer;
  PDBCURSOR             = Pointer;
  PDBHANDLE             = Pointer;
  DBXLATE               = Pointer;
  DBSORTORDER           = Pointer;
  DBLOGINFO             = Pointer;
  DBVOIDPTR             = PPointer;
type
{ DB-Library datatypes }
  DBBOOL                = Byte;
  DBCHAR                = AnsiChar;
  DBBIT                 = Byte;
  DBTINYINT             = Byte;
  DBSMALLINT            = SmallInt; { int16_type }
  DBINT                 = LongInt;  { int32_type }
  DBBIGINT              = Int64;    { int64_type }
  DBBINARY              = Byte;
  DBFLT4                = Single;   { real32_type }
  DBFLT8                = Double;   { real64_type }

  DBSHORT               = SmallInt;
  DBUSMALLINT           = Word;
  DBMONEY4              = LongInt;
  PDBMONEY4             = ^DBMONEY4;

  RETCODE               = Integer;
  PRETCODE              = ^RETCODE;
  STATUS                = Integer;

type
  tdsVARYCHAR=packed record
    len: DBINT;
    str: array[0..DBMAXCHAR-1] of DBCHAR;
  end;

  dblibVARYCHAR=packed record
    len: DBSMALLINT;
    str: array[0..DBMAXCHAR-1] of DBCHAR;
  end;

//typedef int (*INTFUNCPTR) (void *, ...);
//typedef int (*DBWAITFUNC) (void);
//typedef DBWAITFUNC(*DB_DBBUSY_FUNC) (void *dbproc);
//typedef void (*DB_DBIDLE_FUNC) (DBWAITFUNC dfunc, void *dbproc);
//typedef int (*DB_DBCHKINTR_FUNC) (void *dbproc);
//typedef int (*DB_DBHNDLINTR_FUNC) (void *dbproc); *)

  DBREAL                = DBFLT4;
  DBUBOOL               = Cardinal;

  DBDATETIM4 = packed record
    numdays:    Word;        { No of days since Jan-1-1900 }
    nummins:    Word;        { No. of minutes since midnight }
  end;
  PDBDATETIM4 = ^DBDATETIM4;

  {$IFDEF FPC}
    {$PACKRECORDS C}
  {$ENDIF}
type
  DBNUMERIC = packed record
    Precision:  Byte;
    Scale:      Byte;
    Sign:       Byte; { 1 = Positive, 0 = Negative }
    Val:        array[0..MAXNUMERICLEN-1] of Byte;
  end;
  DBDECIMAL = DBNUMERIC;

  TDSDBNUMERIC = packed record
    Precision:  Byte;
    Scale:      Byte;
    Sign:       Byte; { 1 = Positive, 0 = Negative }
    Val:        array[0..MAXNUMERICLEN] of Byte;
  end;
  TDSDBDECIMAL = TDSDBNUMERIC;

  DBVARYCHAR = packed record
    Len: DBSMALLINT;
    Str: array[0..DBMAXCHAR-1] of DBCHAR;
  end;

  DBVARYBIN = packed record
    Len: DBSMALLINT;
    Bytes: array[0..DBMAXCHAR-1] of Byte;
  end;

  DBMONEY = packed record
    mnyhigh:    DBINT;
    mnylow:     LongWord;
  end;
  PDBMONEY = ^DBMONEY;

  PDBDATETIME = ^DBDATETIME;
  DBDATETIME = packed record
    dtdays:	DBINT;          // Days since Jan 1, 1900
    dttime:	LongWord;       // 300ths of a second since midnight, 25920000 unit is 1 day
  end;

  PTDSDBDATETIME = ^TTDSDBDATETIME;
  TTDSDBDATETIME = packed record
    dtdays:	DBINT;          // Days since Jan 1, 1900
    dttime:	DBINT;       // 300ths of a second since midnight, 25920000 unit is 1 day
  end;

(*
 * Sybase & Microsoft use different names for the dbdaterec members.
 * Keep these two structures physically identical in memory.
 * dbdatecrack() casts one to the other for ease of implementation.
 *
 * Giving credit where credit is due, we can acknowledge that
 * Microsoft chose the better names here, hands down.  ("datedmonth"?!)
 *)
  { FreeTDS sybdb.h }
  PTDS_DBDATEREC = ^Ttds_dbdaterec;
  Ttds_dbdaterec = packed record
    { fields }            {microsoft}                 {sybase}
    year:         DBINT;  { 1753 - 9999 }             { 1900 and counting }
    quarter:      DBINT;  { 1 - 4 }                   { 0 - 3 (Microsoft only) }
    month:        DBINT;  { 1 - 12 }                  { 0 - 11 }
    dayofmonth:   DBINT;  { 1 - 31 }                  { 1 - 31 }
    dayofyear:    DBINT;  { 1 - 366 }                 { 1 - 366 (in Sybase.sybdb.h dayofyear and day are changed around!) }
    week:         DBINT;  { 1 - 54 (for leap years) } { 1 - 54 (Microsoft only) }
    weekday:      DBINT;  { 1 - 7  (Mon - Sun) }      { 0 - 6  (Mon - Sun) }
    hour:         DBINT;  { 0 - 23 }                  { 0 - 23 }
    minute:       DBINT;  { 0 - 59 }                  { 0 - 59 }
    second:       DBINT;  { 0 - 59 }                  { 0 - 59 }
    millisecond:  DBINT;  { 0 - 999 }                 { 0 - 997 }
    tzone:        DBINT;  { 0 - 127 (Sybase only!) }  { 0 - 127 }
  end;

{ DBDATEREC structure used by dbdatecrack }
  DBDATEREC = packed record
    year:       DBINT;      { 1753 - 9999 }
    quarter:    DBINT;      { 1 - 4 }
    month:      DBINT;      { 1 - 12 }
    dayofyear:  DBINT;      { 1 - 366 }
    day:        DBINT;      { 1 - 31 }
    week:       DBINT;      { 1 - 54 (for leap years) }
    weekday:    DBINT;      { 1 - 7  (Mon - Sun) }
    hour:       DBINT;      { 0 - 23 }
    minute:     DBINT;      { 0 - 59 }
    second:     DBINT;      { 0 - 59 }
    millisecond: DBINT;     { 0 - 999 }
  end;
  PDBDATEREC = ^DBDATEREC;

type
  //EH: We need a size of 122 Bytes!
  {$IFDEF FPC}
    {$PACKRECORDS 2}
  {$ELSE}
    {$A2}
  {$ENDIF}
  {EH: THIS is a hack: we use entry of typ to have a generic access record }
  PZDBCOL = ^ZDBCOL;
  ZDBCOL = record
   	Typ:            DBSHORT;
   	UserType:       DBINT;
   	MaxLength:      DBINT;
   	Precision:      BYTE;
   	Scale:          BYTE;
   	VarLength:      LongBool; { TRUE, FALSE }
   	Null:           BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	CaseSensitive:  BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	Updatable:      BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	Identity:       LongBool; { TRUE, FALSE or DBUNKNOWN }
  end;
  PDBCOL = ^DBCOL;
  DBCOL = record
   	SizeOfStruct:   DBINT;
   	Name:           array[0..MAXCOLNAMELEN] of DBCHAR;
   	ActualName:     array[0..MAXCOLNAMELEN] of DBCHAR;
   	TableName:      array[0..MAXTABLENAME] of DBCHAR;
    ColInfo:        ZDBCOL;
   	(*Typ:            DBSHORT;
   	UserType:       DBINT;
   	MaxLength:      DBINT;
   	Precision:      BYTE;
   	Scale:          BYTE;
   	VarLength:      LongBool; { TRUE, FALSE }
   	Null:           BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	CaseSensitive:  BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	Updatable:      BYTE;     { TRUE, FALSE or DBUNKNOWN }
   	Identity:       LongBool; { TRUE, FALSE or DBUNKNOWN }*)
  end;

  PTDSDBCOL = ^TTDSDBCOL;
  TTDSDBCOL = record
    SizeOfStruct:   DBINT;
    Name:           array[0..TDSMAXCOLNAMELEN+1] of DBCHAR;
    ActualName:     array[0..TDSMAXCOLNAMELEN+1] of DBCHAR;
    TableName:      array[0..TDSMAXCOLNAMELEN+1] of DBCHAR;
    ColInfo:        ZDBCOL;
    (*Typ:            SmallInt;
    UserType:       DBINT;
    MaxLength:      DBINT;
    Precision:      Byte;
    Scale:          Byte;
    VarLength:      LongBool;{ TRUE, FALSE }
    Null:           Byte;    { TRUE, FALSE or DBUNKNOWN }
    CaseSensitive:  Byte; { TRUE, FALSE or DBUNKNOWN }
    Updatable:      Byte;    { TRUE, FALSE or DBUNKNOWN }
    Identity:       LongBool;{ TRUE, FALSE }*)
  end;

  {$IFDEF FPC}
    {$PACKRECORDS DEFAULT}
  {$ELSE}
    {$A+}
  {$ENDIF}
type
  DBTYPEINFO = packed record
    Precision:  DBINT;
    Scale:      DBINT;
  end;
  PDBTYPEINFO = ^DBTYPEINFO;

  DBPROC_INFO = packed record
    SizeOfStruct:       DBINT;
    ServerType:         Byte;
    ServerMajor:        Word;
    ServerMinor:        Word;
    ServerRevision:     Word;
    ServerName:         array[0..MAXSERVERNAME] of AnsiChar;
    NetLibName:         array[0..MAXNETLIBNAME] of AnsiChar;
    NetLibConnStr:      array[0..MAXNETLIBCONNSTR] of AnsiChar;
  end;
  PDBPROCINFO = ^DBPROC_INFO;

  DBCURSOR_INFO = packed record
    SizeOfStruct:       DBINT;    { Use sizeof(DBCURSORINFO) }
    TotCols:            Cardinal; { Total Columns in cursor }
    TotRows:            Cardinal; { Total Rows in cursor }
    CurRow:             Cardinal; { Current actual row in server }
    TotRowsFetched:     Cardinal; { Total rows actually fetched }
    CurType:            Cardinal; { See CU_... }
    Status:             Cardinal; { See CU_... }
  end;
  PDBCURSORINFO = ^DBCURSOR_INFO;

type
{ Pointer Datatypes }
  PDBINT        = ^DBINT;
  PDBBINARY     = ^DBBINARY;

type
  PDBLibError = ^TDBLibError;
  TDBLibError = record
    dbProc: PDBPROCESS;
    Severity: DBINT;
    DbErr: DBINT;
    OsErr: DBINT;
    DbErrStr: RawByteString;
    OsErrStr: RawByteString;
  end;

  PDBLibMessage = ^TDBLibMessage;
  TDBLibMessage = record
    dbProc: PDBPROCESS;
    MsgNo: DBINT;
    MsgState: DBINT;
    Severity: DBINT;
    MsgText: RawByteString;
    SrvName: RawByteString;
    ProcName: RawByteString;
    Line: DBUSMALLINT;
  end;

type
  TDBVariables = record
    dboptions: array[0..44]  of ShortInt;
    dbSetLoginRec: array[0..16] of ShortInt;
  End;

{common FreeTDS(dblib.dll) and ntwdblib.dll definitions
  requirements: the sam call convention }
  DBERRHANDLE_PROC = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
  DBMSGHANDLE_PROC = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; cdecl;
  Tdberrhandle = function(Handler: DBERRHANDLE_PROC): DBERRHANDLE_PROC; cdecl;
  Tdbmsghandle = function(Handler: DBMSGHANDLE_PROC): DBMSGHANDLE_PROC; cdecl;

  Tdbprocerrhandle = function(DbHandle: PDBHANDLE; Handler: DBERRHANDLE_PROC):
    DBERRHANDLE_PROC; cdecl;
  Tdbprocmsghandle = function(DbHandle: PDBHANDLE; Handler: DBMSGHANDLE_PROC):
    DBMSGHANDLE_PROC; cdecl;

  Tdbadata = function(Proc: PDBPROCESS; ComputeId, Column: Integer): PByte; cdecl;
  Tdbadlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltbind = function(Proc: PDBPROCESS; ComputeId, Column, VarType: Integer;
    VarLen: DBINT; VarAddr: PByte): RETCODE; cdecl;
  Tdbaltcolid = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbaltop = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbalttype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; cdecl;
  Tdbaltutype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
  Tdbanullbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    Indicator: PDBINT): RETCODE; cdecl;
  Tdbbind = function(Proc: PDBPROCESS; Column, VarType, VarLen: Integer;
    VarAddr: PByte): RETCODE; cdecl;
  Tdbbylist = function(Proc: PDBPROCESS; ComputeId: Integer; Size: PInteger):
    PByte; cdecl;
  Tdbcancel = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbcanquery = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbchange = function(Proc: PDBPROCESS): PAnsiChar; cdecl;
  Tdbclrbuf = procedure(Proc: PDBPROCESS; N: DBINT); cdecl;
  Tdbclropt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar): RETCODE; cdecl;
  Tdbcmd = function(Proc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; cdecl;
  Tdbcmdrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbcollen = function(Proc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbcolinfo = function(pdbhandle: PDBHANDLE; _Type: Integer;
    Column: DBINT; ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE; cdecl;
  Tdbcolname = function(Proc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
  Tdbcolsource = function(Proc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
  Tdbcoltype = function(Proc: PDBPROCESS; Column: Integer): Integer; cdecl;
  Tdbcolutype = function(Proc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  Tdbcoltypeinfo = function(Proc: PDBPROCESS; Column: Integer): PDBTYPEINFO; cdecl;
  Tdbconvert = function(Proc: PDBPROCESS; SrcType: Integer; Src: PByte;
    SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; cdecl;
  Tdbiscount = function(Proc: PDBPROCESS): LongBool; cdecl;
  Tdbcurcmd = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbcurrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbdata = function(Proc: PDBPROCESS; Column: Integer): PByte; cdecl;
  Tdbcursor = function(hCursor: PDBCURSOR; OpType, Row: DBINT; Table, Values: PAnsiChar): RETCODE; cdecl;
  Tdbexit = procedure; cdecl;
  Tdbfcmd = function(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; cdecl;
  Tdbfirstrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbfreebuf = procedure(Proc: PDBPROCESS); cdecl;
  Tdbfreequal = procedure(Ptr: PAnsiChar); cdecl;
  Tdbgetchar = function(Proc: PDBPROCESS; N: Integer): PAnsiChar; cdecl;
  Tdbgetoff = function(Proc: PDBPROCESS; OffType: DBUSMALLINT; StartFrom: Integer): Integer; cdecl;
  Tdbgetrow = function(Proc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
  Tdbgettime = function: Integer; cdecl;
  Tdblastrow = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdblogin = function: PLOGINREC; cdecl;
  Tdbmorecmds = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbmoretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE; cdecl;
  Tdbname = function(Proc: PDBPROCESS): PAnsiChar; cdecl;
  Tdbnextrow = function(Proc: PDBPROCESS): STATUS; cdecl;
  Tdbnullbind = function(Proc: PDBPROCESS; Column: Integer; Indicator: PDBINT):
    RETCODE; cdecl;
  Tdbnumalts = function(Proc: PDBPROCESS; ComputeId: Integer): Integer; cdecl;
  Tdbnumcols = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumcompute = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumorders = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbnumrets = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbopen = function(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS; cdecl;
  Tdbprhead = procedure(Proc: PDBPROCESS); cdecl;
  Tdbprrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbprtype = function(Token: Integer): PAnsiChar; cdecl;
  Tdbqual = function(Proc: PDBPROCESS; TabNum: Integer; TabName: PAnsiChar): PAnsiChar; cdecl;
  Tdbordercol = function(Proc: PDBPROCESS; Order: Integer): Integer; cdecl;
  Tdbreadtext = function(dbproc: PDBPROCESS; Buf: Pointer; BufSize: DBINT): DBINT; cdecl;
  Tdbresults = function(dbproc: PDBPROCESS): RETCODE; cdecl;
  Tdbretdata = function(dbproc: PDBPROCESS; RetNum: Integer): PByte; cdecl;
  Tdbretlen = function(dbproc: PDBPROCESS; RetNum: Integer): DBINT; cdecl;
  Tdbretname = function(Proc: PDBPROCESS; RetNum: Integer): PAnsiChar; cdecl;
  Tdbretstatus = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tdbrettype = function(Proc: PDBPROCESS; RetNum: Integer): Integer; cdecl;
  Tdbrows = function(Proc: PDBPROCESS): RETCODE; cdecl; //!!!
  Tdbrowtype = function(Proc: PDBPROCESS): STATUS; cdecl;
  Tdbrpcinit = function(Proc: PDBPROCESS; ProcName: PAnsiChar; Options: DBSMALLINT):
    RETCODE; cdecl; //!!!
  Tdbrpcparam = function(Proc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
    Typ: Integer; MaxLen, DataLen: DBINT; Value: PByte): RETCODE; cdecl;
  Tdbrpcsend = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbrpwclr = procedure(Login: PLOGINREC); cdecl;
  Tdbsetavail = procedure(Proc: PDBPROCESS); cdecl;
  Tdbsetlogintime = function(Seconds: Integer): RETCODE; cdecl;
  Tdbsetnull = function(Proc: PDBPROCESS; BindType, BindLen: Integer;
    BindVal: PByte): RETCODE; cdecl;
  Tdbsettime = function(Seconds: Integer): RETCODE; cdecl;
  Tdbsetuserdata = procedure(Proc: PDBPROCESS; Ptr: Pointer); cdecl;
  Tdbsqlexec = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlok = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbsqlsend = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tdbstrcpy = function(Proc: PDBPROCESS; Start, NumBytes: Integer; Dest: PAnsiChar):
    RETCODE; cdecl;
  Tdbstrlen = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtabcount = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtabname = function(Proc: PDBPROCESS; Table: Integer): PAnsiChar; cdecl;
  Tdbtabsource = function(Proc: PDBPROCESS; Column: Integer; TabNum: PInteger):
    PAnsiChar; cdecl;
  Tdbtsnewlen = function(Proc: PDBPROCESS): Integer; cdecl;
  Tdbtsnewval = function(Proc: PDBPROCESS): PDBBINARY; cdecl;
  Tdbtsput = function(Proc: PDBPROCESS; NewTs: PDBBINARY; NewTsLen,
    TabNum: Integer; TableName: PAnsiChar): RETCODE; cdecl;
  Tdbtxptr = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtimestamp = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; cdecl;
  Tdbtxtsnewval = function(Proc: PDBPROCESS): PDBBINARY; cdecl;
  Tdbtxtsput = function(Proc: PDBPROCESS; NewTxts: PDBBINARY; Column: Integer):
    RETCODE; cdecl;
  Tdbuse = function(Proc: PDBPROCESS; DbName: PAnsiChar): RETCODE; cdecl;
  Tdbwritetext = function(Proc: PDBPROCESS; ObjName: PAnsiChar; TextPtr: PDBBINARY;
    TextPtrLen: DBTINYINT; Timestamp: PDBBINARY; Log: LongBool; Size: DBINT;
    Text: PByte): RETCODE; cdecl;
  (* LOGINREC manipulation *)
  Tdbsetlname = function(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE; cdecl;
{ BCP functions }
  Tbcp_batch = function(const Proc: PDBPROCESS): DBINT; cdecl;
  Tbcp_bind = function(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
    VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_colfmt = function(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
    FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
    TableColumn: Integer): RETCODE; cdecl;
  Tbcp_collen = function(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_colptr = function(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer):
    RETCODE; cdecl;
  Tbcp_columns = function(Proc: PDBPROCESS; FileColCount: Integer): RETCODE; cdecl;
  Tbcp_control = function(Proc: PDBPROCESS; Field: Integer; Value: DBINT):
    RETCODE; cdecl;
  Tbcp_done = function(Proc: PDBPROCESS): DBINT; cdecl;
  Tbcp_exec = function(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE; cdecl;
  Tbcp_init = function(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
    Direction: Integer): RETCODE; cdecl;
  Tbcp_moretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte):
    RETCODE; cdecl;
  Tbcp_readfmt = function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; cdecl;
  Tbcp_sendrow = function(Proc: PDBPROCESS): RETCODE; cdecl;
  Tbcp_setl = function(Login: PLOGINREC; Enable: LongBool): RETCODE; cdecl;
  Tbcp_writefmt = function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; cdecl;
  { Two-phase commit functions }
  Tabort_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Tbuild_xact_string = procedure(XActName, Service: PAnsiChar; CommId: DBINT;
    Result: PAnsiChar); cdecl;
  Tclose_commit = procedure(Proc: PDBPROCESS); cdecl;
  Tcommit_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Topen_commit = function(Login: PLOGINREC; ServerName: PAnsiChar): PDBPROCESS; cdecl;
  Tremove_xact = function(Proc: PDBPROCESS; CommId: DBINT; SiteCount: Integer):
    RETCODE; cdecl;
  Tscan_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; cdecl;
  Tstart_xact = function(Proc: PDBPROCESS; AppName, XActName: PAnsiChar;
    SiteCount: Integer): DBINT; cdecl;
  Tstat_xact = function(Proc: PDBPROCESS; CommId: DBINT): Integer; cdecl;



{FreeTDS spezial API definitions}
  TFreeTDSdb12hour = function(Proc: PDBPROCESS; Language: PAnsiChar): DBBOOL; cdecl;
  TFreeTDSdbcolbrowse = function(Proc: PDBPROCESS; Column: Integer): DBBOOL; cdecl;
  TFreeTDSdbcursorbind = function(hCursor: PDBCURSOR; Col, VarType: Integer; VarLen, POutLen: DBINT;
    VarAddr: PByte; DBTYPEINFO: PDBTYPEINFO): RETCODE; cdecl;
  TFreeTDSdbcursorclose = procedure(DbHandle: PDBHANDLE); cdecl;
  TFreeTDSdbcursorcolinfo = function(hCursor: PDBCURSOR; Column: DBINT; ColName: PAnsiChar;
    ColType, ColLen, UserType: PDBINT): RETCODE; cdecl;
  TFreeTDSdbcursorfetch   = function(hCursor: PDBCURSOR; FetchType, RowNum: DBINT): RETCODE; cdecl;
  TFreeTDSdbcursorinfo    = function(hCursor: PDBCURSOR; nCols, nRows: PDBINT): RETCODE; cdecl;
  TFreeTDSdbcursoropen = function(Proc: PDBPROCESS; Sql: PAnsiChar; ScrollOpt,
    ConCurOpt: DBSHORT; nRows: DBUSMALLINT; PStatus: PDBINT): PDBCURSOR; cdecl;

  TFreeTDSdbaltbind_ps    = function(dbproc: PDBPROCESS; ComputeId, Column: Integer; VarType: Integer; VarLen: DBINT; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; cdecl;
  TFreeTDSdbbind_ps       = function(dbproc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; cdecl;
  TFreeTDSdbbufsize       = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbclose         = procedure(dbproc: PDBPROCESS); cdecl;
  TFreeTDSdbtablecolinfo  = function(dbproc: PDBPROCESS; Column: DBINT; DbColumn: PTDSDBCOL): RETCODE;
  TFreeTDSdbcolinfo       = function(Handle: PDBHANDLE; Typ, Column, ComputeId: Integer; DbColumn: PTDSDBCOL): RETCODE; cdecl;
  TFreeTDSdbconvert_ps    = function(dbproc: PDBPROCESS; SrcType: Integer; Src: PByte; SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT; typinfo: PDBTYPEINFO): Integer; cdecl;
  TFreeTDSdbcount         = function(dbproc: PDBPROCESS): DBINT; cdecl;
  TFreeTDSdbdatecmp       = function(dbproc: PDBPROCESS; d1, d2: PTDS_DBDATEREC): Integer;
  TFreeTDSdbdatecrack     = function(dbproc: PDBPROCESS; DateInfo: PTDS_DBDATEREC; DateType: PTDSDBDATETIME): RETCODE; cdecl;
  TFreeTDSdbdatlen        = function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  TFreeTDSdbdead          = function(dbproc: PDBPROCESS): DBBOOL; cdecl;
  TFreeTDSdbgetcharset    = function(dbproc: PDBPROCESS): PAnsiChar;
  TFreeTDSdbgetlusername  = function(login: PLOGINREC; name_buffer: PByte; buffer_len: Integer): Integer; cdecl;
  TFreeTDSdbgetmaxprocs   = function: Integer; cdecl;
  TFreeTDSdbgetnatlanf    = function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
  TFreeTDSdbgetpacket     = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbgetuserdata   = function(dbproc: PDBPROCESS): PByte; cdecl;
  TFreeTDSdbhasretstat    = function(dbproc: PDBPROCESS): DBBOOL; cdecl;
  TFreeTDSdbinit          = function:RETCODE; cdecl;
  TFreeTDSdbiordesc       = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbiowdesc       = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbisavail       = function(Proc: PDBPROCESS): DBBOOL; cdecl;
  TFreeTDSdbisopt         = function(Proc: PDBPROCESS; Option: Integer; const Param: PAnsiChar): DBBOOL; cdecl;
  TFreeTDSdbloginfree     = procedure(Login: PLOGINREC); cdecl;
  TFreeTDSdbmny4cmp       = function(dbproc: PDBPROCESS; m1, m: PDBMONEY4): Integer; cdecl;
  TFreeTDSdbmnycmp        = function(dbproc: PDBPROCESS; m1, m2: PDBMONEY): Integer; cdecl;
  TFreeTDSdbmny4add       = function(dbproc: PDBPROCESS; m1, m2, sum: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmnydec        = function(dbproc: PDBPROCESS; mnyptr: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmnyinc        = function(dbproc: PDBPROCESS; mnyptr: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmnymaxpos     = function(dbproc: PDBPROCESS; dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmnymaxneg     = function(dbproc: PDBPROCESS; dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmny4minus     = function(dbproc: PDBPROCESS; src, dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmnyminus      = function(dbproc: PDBPROCESS; src, dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmny4sub       = function(dbproc: PDBPROCESS; m1, m2, diff: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmnysub        = function(dbproc: PDBPROCESS; m1, m2, diff: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmny4copy      = function(dbproc: PDBPROCESS; m1, m2: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmnycopy       = function(dbproc: PDBPROCESS; src, dest: PDBMONEY): RETCODE; cdecl;
  TFreeTDSdbmny4zero      = function(dbproc: PDBPROCESS; dest: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmnyzero       = function(dbproc: PDBPROCESS; dest: PDBMONEY4): RETCODE; cdecl;
  TFreeTDSdbmonthname     = function(dbproc: PDBPROCESS; language: PAnsiChar; monthnum: Integer; shortform: DBBOOL): PAnsiChar; cdecl;
  TFreeTDSdbopen          = function(Login: PLOGINREC; const Server: PAnsiChar; msdblib: Integer): PDBPROCESS; cdecl;
  TFreeTDSdbrecftos       = procedure(const FileName: PAnsiChar);
  TDRBUF                  = function(dbproc: PDBPROCESS): DBBOOL; cdecl;
  TFreeTDSdbresults_r     = function(dbproc: PDBPROCESS; Recursive: Integer): RETCODE; cdecl;
  TFreeTDSdbsafestr       = function(dbproc: PDBPROCESS; const Src: PAnsiChar; SrcLen: DBINT; Dest: PAnsiChar; DestLen: DBINT; QuoteType: integer): RETCODE; cdecl;
  TFreeTDSdbservcharset   = function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
  TFreeTDSdbsetdefcharset = function(Charset: PAnsiChar): RETCODE; cdecl;
  TFreeTDSdbsetifile      = procedure(FileName: PAnsiChar); cdecl;
  TFreeTDSdbsetmaxprocs   = function(MaxProcs: Integer): RETCODE; cdecl;
  TFreeTDSdbsetopt        = function(dbproc: PDBPROCESS; Option: DBINT; Param: PAnsiChar; int_param: DBINT): RETCODE; cdecl;
  TFreeTDSdbsetrow        = function(dbproc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
  TFreeTDSdbsetversion    = function(Version: DBINT): RETCODE; cdecl;
  TFreeTDSdbspid          = function(dbproc: PDBPROCESS): Integer; cdecl;
  TFreeTDSdbspr1row       = function(dbproc: PDBPROCESS; Buffer: PAnsiChar; buf_len: DBINT): RETCODE; cdecl;
  TFreeTDSdbspr1rowlen    = function(dbproc: PDBPROCESS): DBINT; cdecl;
  TFreeTDSdbsprhead       = function(dbproc: PDBPROCESS; Buffer: PAnsiChar; buf_len: DBINT): RETCODE; cdecl;
  TFreeTDSdbsprline       = function(dbproc: PDBPROCESS; Buffer: PAnsiChar; buf_len: DBINT; line_char: DBCHAR): RETCODE; cdecl;
  TFreeTDSdbvarylen       = function(dbproc: PDBPROCESS; Column: Integer): DBINT; cdecl;
  TFreeTDSdbtds           = function(dbproc: PDBPROCESS): DBINT; cdecl;
  TFreeTDSdbtextsize      = function(dbproc: PDBPROCESS): DBINT; cdecl;
  TFreeTDSdbwillconvert   = function(SrcType, DestType: Integer): DBBOOL; cdecl;
  TFreeTDSdbtabbrowse     = function(Proc: PDBPROCESS; TabNum: Integer): LongBool; cdecl;
  (* LOGINREC manipulation *)
  TFreeTDSdbsetlbool      = function(Login: PLOGINREC; Value, Item: Integer): RETCODE; cdecl;
  TFreeTDSdbsetllong      = function(Login: PLOGINREC; Value, Item: Integer): RETCODE; cdecl;
  TFreeTDSdbsetlversion   = function(Login: PLOGINREC; Version: Byte): RETCODE; cdecl;
  Ttdsdump_on = procedure ; cdecl;
  Ttdsdump_off = procedure ; cdecl;
  Ttdsdump_open = function (FileName : PAnsiChar): Integer; cdecl;
  Ttdsdump_close = procedure ; cdecl;
  T_tds_socket_init = procedure ; cdecl;
  T_tds_socket_done = procedure ; cdecl;


{ pivot functions
void dbpivot_count (struct col_t *output, const struct col_t *input);
void dbpivot_sum (struct col_t *output, const struct col_t *input);
void dbpivot_min (struct col_t *output, const struct col_t *input);
void dbpivot_max (struct col_t *output, const struct col_t *input);

struct pivot_t;
typedef void (*DBPIVOT_FUNC)(struct col_t *output, const struct col_t *input);
struct pivot_t * dbrows_pivoted(DBPROCESS *dbproc);
STATUS dbnextrow_pivoted(DBPROCESS *dbproc, struct pivot_t *pp);
RETCODE dbpivot(DBPROCESS *dbproc, int nkeys, int *keys, int ncols, int *cols, DBPIVOT_FUNC func, int val);

DBPIVOT_FUNC dbpivot_lookup_name( const char name[] );
}
  //TFreeTDSdbsechandle    = function(_Type: DBINT ; Handler: INTFUNCPTR): PRETCODE; cdecl;
  //TFreeTDSdbsetbusy      = procedure(dbproc: PDBPROCESS; BusyFunc: DB_DBBUSY_FUNC);  cdecl;
  //TFreeTDSdbsetinterrupt = procedure(dbproc: PDBPROCESS; chkintr: DB_DBCHKINTR_FUNC; hndlintr: DB_DBHNDLINTR_FUNC);



{MsSQL-spezial API definitions}

{ Standard DB-Library functions }
  TMsSQLdbclose = function(Proc: PDBPROCESS): RETCODE; cdecl;
  TMsSQLdbcolbrowse = function(Proc: PDBPROCESS; Column: Integer): LongBool; cdecl;
  TMsSQLdbcolinfo = function(Handle: PDBHANDLE; Typ, Column, ComputeId: Integer;
    DbColumn: PDBCOL): RETCODE; cdecl;
  TMsSQLdbcount = function(Proc: PDBPROCESS): Integer; cdecl;

  TMsSQLdbcursorbind = function(hCursor: PDBCURSOR; Col, VarType: Integer; VarLen: DBINT;
    POutLen: PDBINT; VarAddr: PByte): RETCODE; cdecl;
  TMsSQLdbcursorclose = function(DbHandle: PDBHANDLE): RETCODE; cdecl;
  TMsSQLdbcursorcolinfo = function(hCursor: PDBCURSOR; Column: Integer; ColName: PAnsiChar;
    ColType: PInteger; ColLen: PDBINT; UserType: PInteger): RETCODE; cdecl;
  TMsSQLdbcursorfetch = function(hCursor: PDBCURSOR; FetchType, RowNum: Integer): RETCODE; cdecl;
  TMsSQLdbcursorfetchex = function(hCursor: PDBCURSOR; FetchType: Integer; RowNum,
    nFetchRows, Reserved: DBINT): RETCODE; cdecl;
  TMsSQLdbcursorinfo = function(hCursor: PDBCURSOR; nCols: PInteger; nRows: PDBINT):
    RETCODE; cdecl;
  TMsSQLdbcursorinfoex = function(hCursor: PDBCURSOR; DbCursorInfo: PDBCURSORINFO):
    RETCODE; cdecl;
  TMsSQLdbcursoropen = function(Proc: PDBPROCESS; Sql: PAnsiChar; ScrollOpt,
    ConCurOpt: Integer; nRows: Cardinal; PStatus: PDBINT): PDBCURSOR; cdecl;
  TMsSQLdbdataready = function(Proc: PDBPROCESS): LongBool; cdecl;
  TMsSQLdbdatecrack = function(Proc: PDBPROCESS; DateInfo: PDBDATEREC;
    DateType: PDBDATETIME): RETCODE; cdecl;
  TMsSQLdbdatlen = function(Proc: PDBPROCESS; Column: Integer): Integer; cdecl;
  TMsSQLdbdead = function(Proc: PDBPROCESS): LongBool; cdecl;
  TMsSQLdbWinexit = procedure; cdecl;
  TMsSQLdbenlisttrans = function(Proc: PDBPROCESS; Transaction: Pointer): RETCODE; cdecl;
  TMsSQLdbenlistxatrans = function(Proc: PDBPROCESS; EnlistTran: LongBool): RETCODE; cdecl;
  TMsSQLdbgetmaxprocs = function: SmallInt; cdecl;
  TMsSQLdbgetpacket = function(Proc: PDBPROCESS): Cardinal; cdecl;
  TMsSQLdbgetuserdata = function(Proc: PDBPROCESS): Pointer; cdecl;
  TMsSQLdbhasretstat = function(Proc: PDBPROCESS): LongBool; cdecl;
  TMsSQLdbinit = function: PAnsiChar; cdecl;
  TMsSQLdbisavail = function(Proc: PDBPROCESS): LongBool; cdecl;
  TMsSQLdbisopt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar): LongBool; cdecl;
  TMsSQLdbfreelogin = procedure(Login: PLOGINREC); cdecl;
  TMsSQLdbprocinfo = function(Proc: PDBPROCESS; DbProcInfo: PDBPROCINFO): RETCODE; cdecl;
  TMsSQLdbrpcexec = function(Proc: PDBPROCESS): RETCODE; cdecl;
  TMsSQLdbserverenum = function(SearchMode: Word; ServNameBuf: PAnsiChar;
    ServNameBufSize: Word; NumEntries: PWord): Integer; cdecl;
  TMsSQLdbsetmaxprocs = function(MaxProcs: SmallInt): RETCODE; cdecl;
  TMsSQLdbsetlpacket = function(Login: PLOGINREC; PacketSize: Word): RETCODE; cdecl; //TDS: dbsetllong
  TMsSQLdbsetopt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar):
    RETCODE; cdecl;
  TMsSQLdbtabbrowse = function(Proc: PDBPROCESS; TabNum: Integer): LongBool; cdecl;

  TMsSQLdbvarylen = function(Proc: PDBPROCESS; Column: Integer): LongBool; cdecl;
  TMsSQLdbwillconvert = function(SrcType, DestType: Integer): LongBool; cdecl;
  TMsSQLdbupdatetext = function(Proc: PDBPROCESS; DestObject: PAnsiChar; DestTextPtr,
    DestTimestamp: PDBBINARY; UpdateType: Integer; InsertOffset,
    DeleteLength: DBINT; SrcObject: PAnsiChar; SrcSize: DBINT; SrcText: PDBBINARY):
    RETCODE; cdecl;

{************* Plain API Function variables definition ************}

{Sybase API definitions}
type
  SYBDBERRHANDLE_PROC = function(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
    DbErrStr, OsErrStr: PAnsiChar): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  SYBDBMSGHANDLE_PROC = function(Proc: PDBPROCESS; MsgNo: DBINT; MsgState,
    Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar; Line: DBUSMALLINT):
    Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TSybdb12hour = function(Proc: PDBPROCESS; Language: PAnsiChar): DBBOOL; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TSybdberrhandle = function(Handler: SYBDBERRHANDLE_PROC): SYBDBERRHANDLE_PROC; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbmsghandle = function(Handler: SYBDBMSGHANDLE_PROC): SYBDBMSGHANDLE_PROC; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  { Two-phase commit functions }
  TSybabort_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbuild_xact_string = procedure(XActName, Service: PAnsiChar; CommId: DBINT;
      Result: PAnsiChar); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybclose_commit = procedure(Proc: PDBPROCESS); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybcommit_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybopen_commit = function(Login: PLOGINREC; ServerName: PAnsiChar): PDBPROCESS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybremove_xact = function(Proc: PDBPROCESS; CommId: DBINT; SiteCount: Integer): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybscan_xact = function(Proc: PDBPROCESS; CommId: DBINT): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybstart_xact = function(Proc: PDBPROCESS; AppName, XActName: PAnsiChar;
    SiteCount: Integer): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybstat_xact = function(Proc: PDBPROCESS; CommId: DBINT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

{ BCP functions }
  TSybbcp_batch = function(const Proc: PDBPROCESS): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_bind = function(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
    VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_colfmt = function(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
    FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
    TableColumn: Integer): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_collen = function(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_colptr = function(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_columns = function(Proc: PDBPROCESS; FileColCount: Integer): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_control = function(Proc: PDBPROCESS; Field: Integer; Value: DBINT):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_done = function(Proc: PDBPROCESS): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_exec = function(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_init = function(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
    Direction: Integer): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_moretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_readfmt = function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_sendrow = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_setl = function(Login: PLOGINREC; Enable: LongBool): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybbcp_writefmt = function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

{ Standard DB-Library functions }
  TSybdbadata = function(Proc: PDBPROCESS; ComputeId, Column: Integer): PByte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbadlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltcolid = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltlen = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltop = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbalttype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbaltutype = function(Proc: PDBPROCESS; ComputeId, Column: Integer): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbanullbind = function(Proc: PDBPROCESS; ComputeId, Column: Integer;
    Indicator: PDBINT): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbbind = function(Proc: PDBPROCESS; Column, VarType, VarLen: Integer;
    VarAddr: PByte): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbbylist = function(Proc: PDBPROCESS; ComputeId: Integer; Size: PInteger):
    PByte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcancel = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcanquery = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbchange = function(Proc: PDBPROCESS): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbclose = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbclrbuf = procedure(Proc: PDBPROCESS; N: DBINT); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbclropt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcmd = function(Proc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcmdrow = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcolbrowse = function(Proc: PDBPROCESS; Column: Integer): LongBool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcollen = function(Proc: PDBPROCESS; Column: Integer): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcolinfo = function(pdbhandle :PDBHANDLE; _Type: Integer; Column: DBINT; ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE;{$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcolname = function(Proc: PDBPROCESS; Column: Integer): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcolsource = function(Proc: PDBPROCESS; Column: Integer): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcoltypeinfo = function(Proc: PDBPROCESS; Column: Integer): PDBTYPEINFO; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcoltype = function(Proc: PDBPROCESS; Column: Integer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcolutype = function(Proc: PDBPROCESS; Column: Integer): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbconvert = function(Proc: PDBPROCESS; SrcType: Integer; Src: PByte;
  SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcount = function(Proc: PDBPROCESS): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcurcmd = function(Proc: PDBPROCESS): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcurrow = function(Proc: PDBPROCESS): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TSybdbcursor = function(hCursor: PDBCURSOR; OpType, Row: Integer; Table,
    Values: PAnsiChar): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorbind = function(hCursor: PDBCURSOR; Col, VarType: Integer; VarLen: DBINT;
    POutLen: PDBINT; VarAddr: PByte): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorclose = function(DbHandle: PDBHANDLE): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorcolinfo = function(hCursor: PDBCURSOR; Column: Integer; ColName: PAnsiChar;
    ColType: PInteger; ColLen: PDBINT; UserType: PInteger): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorfetch = function(hCursor: PDBCURSOR; FetchType, RowNum: Integer):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursorinfo = function(hCursor: PDBCURSOR; nCols: PInteger; nRows: PDBINT):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbcursoropen = function(Proc: PDBPROCESS; Sql: PAnsiChar; ScrollOpt,
    ConCurOpt: Integer; nRows: Cardinal; PStatus: PDBINT): PDBCURSOR; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbdata = function(Proc: PDBPROCESS; Column: Integer): PByte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbdatecrack = function(Proc: PDBPROCESS; DateInfo: PDBDATEREC;
    DateType: PDBDATETIME): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbdatlen = function(Proc: PDBPROCESS; Column: Integer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbdead = function(Proc: PDBPROCESS): LongBool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbexit = procedure; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbfcmd = function(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbfirstrow = function(Proc: PDBPROCESS): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbfreebuf = procedure(Proc: PDBPROCESS); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbloginfree = procedure(Login: PLOGINREC); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbfreequal = procedure(Ptr: PAnsiChar); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetchar = function(Proc: PDBPROCESS; N: Integer): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetmaxprocs = function: SmallInt; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetoff = function(Proc: PDBPROCESS; OffType: DBUSMALLINT;
    StartFrom: Integer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetpacket = function(Proc: PDBPROCESS): Cardinal; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetrow = function(Proc: PDBPROCESS; Row: DBINT): STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbgetuserdata = function(Proc: PDBPROCESS): Pointer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbhasretstat = function(Proc: PDBPROCESS): LongBool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbinit = function: RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbisavail = function(Proc: PDBPROCESS): LongBool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbisopt = function(Proc: PDBPROCESS; Option: Integer; Param: PAnsiChar): LongBool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdblastrow = function(Proc: PDBPROCESS): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdblogin = function: PLOGINREC; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbmorecmds = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbmoretext = function(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbname = function(Proc: PDBPROCESS): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnextrow = function(Proc: PDBPROCESS): STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnullbind = function(Proc: PDBPROCESS; Column: Integer; Indicator: PDBINT):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumalts = function(Proc: PDBPROCESS; ComputeId: Integer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumcols = function(Proc: PDBPROCESS): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumcompute = function(Proc: PDBPROCESS): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumorders = function(Proc: PDBPROCESS): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbnumrets = function(Proc: PDBPROCESS): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbopen = function(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbordercol = function(Proc: PDBPROCESS; Order: Integer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbprhead = procedure(Proc: PDBPROCESS); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbprrow = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbprtype = function(Token: Integer): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbqual = function(Proc: PDBPROCESS; TabNum: Integer; TabName: PAnsiChar): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbreadtext = function(Proc: PDBPROCESS; Buf: Pointer; BufSize: DBINT): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbresults = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbretdata = function(Proc: PDBPROCESS; RetNum: Integer): PByte; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbretlen = function(Proc: PDBPROCESS; RetNum: Integer): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbretname = function(Proc: PDBPROCESS; RetNum: Integer): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbretstatus = function(Proc: PDBPROCESS): DBINT; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbrettype = function(Proc: PDBPROCESS; RetNum: Integer): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbrows = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF}; //!!!
  TSybdbrowtype = function(Proc: PDBPROCESS): STATUS; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbrpcinit = function(Proc: PDBPROCESS; ProcName: PAnsiChar; Options: DBSMALLINT): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF}; //!!!
  TSybdbrpcparam = function(Proc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
    Typ: Integer; MaxLen, DataLen: DBINT; Value: PByte): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbrpcsend = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TSybdbrpwclr = procedure(Login: PLOGINREC); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsetavail = procedure(Proc: PDBPROCESS); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsetmaxprocs = function(MaxProcs: SmallInt): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsetlname = function(Login: PLOGINREC; Value: PAnsiChar; Item: Integer): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsetlogintime = function(Seconds: Integer): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TSybdbsetnull = function(Proc: PDBPROCESS; BindType, BindLen: Integer;
    BindVal: PByte): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsetopt = function(Proc: PDBPROCESS; Option: Integer; CharParam: PAnsiChar; IntParam: Integer):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsettime = function(Seconds: Integer): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsetuserdata = procedure(Proc: PDBPROCESS; Ptr: Pointer); {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsqlexec = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsqlok = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbsqlsend = function(Proc: PDBPROCESS): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbstrcpy = function(Proc: PDBPROCESS; Start, NumBytes: Integer; Dest: PAnsiChar):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbstrlen = function(Proc: PDBPROCESS): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtabbrowse = function(Proc: PDBPROCESS; TabNum: Integer): LongBool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtabcount = function(Proc: PDBPROCESS): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtabname = function(Proc: PDBPROCESS; Table: Integer): PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtabsource = function(Proc: PDBPROCESS; Column: Integer; TabNum: PInteger):
    PAnsiChar; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtsnewlen = function(Proc: PDBPROCESS): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtsnewval = function(Proc: PDBPROCESS): PDBBINARY; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtsput = function(Proc: PDBPROCESS; NewTs: PDBBINARY; NewTsName,
    TabNum: Integer; TableName: PAnsiChar): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtxptr = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtxtimestamp = function(Proc: PDBPROCESS; Column: Integer): PDBBINARY; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtxtsnewval = function(Proc: PDBPROCESS): PDBBINARY; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbtxtsput = function(Proc: PDBPROCESS; NewTxts: PDBBINARY; Column: Integer):
    RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbuse = function(Proc: PDBPROCESS; DbName: PAnsiChar): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbvarylen = function(Proc: PDBPROCESS; Column: Integer): LongBool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbwillconvert = function(SrcType, DestType: Integer): LongBool; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  TSybdbwritetext = function(Proc: PDBPROCESS; ObjName: PAnsiChar; TextPtr: PDBBINARY;
    TextPtrLen: DBTINYINT; Timestamp: PDBBINARY; Log: LongBool; Size: DBINT;
    Text: PByte): RETCODE; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

  TDBLibAPI = Record
    dberrhandle           : Tdberrhandle;
    dbmsghandle           : Tdbmsghandle;

    dbprocerrhandle       : Tdbprocerrhandle;
    dbprocmsghandle       : Tdbprocmsghandle;

    { Two-phase commit functions }
    abort_xact            : Tabort_xact;
    build_xact_string     : Tbuild_xact_string;
    close_commit          : Tclose_commit;
    commit_xact           : Tcommit_xact;
    open_commit           : Topen_commit;
    remove_xact           : Tremove_xact;
    scan_xact             : Tscan_xact;
    start_xact            : Tstart_xact;
    stat_xact             : Tstat_xact;

  { BCP functions }
    bcp_batch             : Tbcp_batch;
    bcp_bind              : Tbcp_bind;
    bcp_colfmt            : Tbcp_colfmt;
    bcp_collen            : Tbcp_collen;
    bcp_colptr            : Tbcp_colptr;
    bcp_columns           : Tbcp_columns;
    bcp_control           : Tbcp_control;
    bcp_done              : Tbcp_done;
    bcp_exec              : Tbcp_exec;
    bcp_init              : Tbcp_init;
    bcp_moretext          : Tbcp_moretext;
    bcp_readfmt           : Tbcp_readfmt;
    bcp_sendrow           : Tbcp_sendrow;
    bcp_setl              : Tbcp_setl;
    bcp_writefmt          : Tbcp_writefmt;

    dbadata               : Tdbadata;
    dbadlen               : Tdbadlen;
    dbaltbind             : Tdbaltbind;
    dbaltcolid            : Tdbaltcolid;
    dbaltlen              : Tdbaltlen;
    dbaltop               : Tdbaltop;
    dbalttype             : Tdbalttype;
    dbaltutype            : Tdbaltutype;
    dbanullbind           : Tdbanullbind;
    dbbind                : Tdbbind;
    dbbylist              : Tdbbylist;
    dbcancel              : Tdbcancel;
    dbcanquery            : Tdbcanquery;
    dbchange              : Tdbchange;
    dbclrbuf              : Tdbclrbuf;
    dbclropt              : Tdbclropt;
    dbcmd                 : Tdbcmd;
    dbcmdrow              : Tdbcmdrow;
    dbcollen              : Tdbcollen;
    dbcolinfo             : Tdbcolinfo;
    dbcolname             : Tdbcolname;
    dbcolsource           : Tdbcolsource;
    dbcoltype             : Tdbcoltype;
    dbcolutype            : Tdbcolutype;
    dbcoltypeinfo         : Tdbcoltypeinfo;
    dbconvert             : Tdbconvert;
    dbcurcmd              : Tdbcurcmd;
    dbcurrow              : Tdbcurrow;
    dbcursor              : Tdbcursor;
    dbdata                : Tdbdata;
    dbexit                : Tdbexit;
    dbfcmd                : Tdbfcmd;
    dbfirstrow            : Tdbfirstrow;
    dbfreebuf             : Tdbfreebuf;
    dbfreequal            : Tdbfreequal;
    dbgetchar             : Tdbgetchar;
    dbgetoff              : Tdbgetoff;
    dbgetrow              : Tdbgetrow;
    dbgettime             : Tdbgettime;
    dbiscount             : Tdbiscount;
    dblastrow             : Tdblastrow;
    dblogin               : Tdblogin;
    dbmorecmds            : Tdbmorecmds;
    dbmoretext            : Tdbmoretext;
    dbname                : Tdbname;
    dbnextrow             : Tdbnextrow;
    dbnullbind            : Tdbnullbind;
    dbnumalts             : Tdbnumalts;
    dbnumcols             : Tdbnumcols;
    dbnumcompute          : Tdbnumcompute;
    dbnumorders           : Tdbnumorders;
    dbnumrets             : Tdbnumrets;
    dbopen                : Tdbopen;
    dbordercol            : Tdbordercol;
    dbprhead              : Tdbprhead;
    dbprrow               : Tdbprrow;
    dbprtype              : Tdbprtype;
    dbqual                : Tdbqual;
    dbreadtext            : Tdbreadtext;
    dbresults             : Tdbresults;
    dbretdata             : Tdbretdata;
    dbretlen              : Tdbretlen;
    dbretname             : Tdbretname;
    dbretstatus           : Tdbretstatus;
    dbrettype             : Tdbrettype;
    dbrows                : Tdbrows;
    dbrowtype             : Tdbrowtype;
    dbrpcinit             : Tdbrpcinit;
    dbrpcparam            : Tdbrpcparam;
    dbrpcsend             : Tdbrpcsend;
    dbrpwclr              : Tdbrpwclr;
    dbsetavail            : Tdbsetavail;
    dbsetlname            : Tdbsetlname;
    dbsetlogintime        : Tdbsetlogintime;
    dbsetnull             : Tdbsetnull;
    dbsettime             : Tdbsettime;
    dbsetuserdata         : Tdbsetuserdata;
    dbsqlexec             : Tdbsqlexec;
    dbsqlok               : Tdbsqlok;
    dbsqlsend             : Tdbsqlsend;
    dbstrcpy              : Tdbstrcpy;
    dbstrlen              : Tdbstrlen;
    dbtabcount            : Tdbtabcount;
    dbtabname             : Tdbtabname;
    dbtabsource           : Tdbtabsource;
    dbtsnewlen            : Tdbtsnewlen;
    dbtsnewval            : Tdbtsnewval;
    dbtsput               : Tdbtsput;
    dbtxptr               : Tdbtxptr;
    dbtxtimestamp         : Tdbtxtimestamp;
    dbtxtsnewval          : Tdbtxtsnewval;
    dbtxtsput             : Tdbtxtsput;
    dbuse                 : Tdbuse;
    dbwritetext           : Tdbwritetext;
  End;

  TFreeTDSAPI = Record
  {available but not implemented}
    db12hour:       TFreeTDSdb12hour; {no MS}
    dbcolbrowse:    TFreeTDSdbcolbrowse;
    dbcursorbind:   TFreeTDSdbcursorbind;
    dbcursorclose:  TFreeTDSdbcursorclose;
    dbcursorcolinfo:TFreeTDSdbcursorcolinfo;
    dbcursorfetch:  TFreeTDSdbcursorfetch;
    dbcursorinfo:   TFreeTDSdbcursorinfo;


    dbaltbind_ps:   TFreeTDSdbaltbind_ps;
    dbbind_ps:      TFreeTDSdbbind_ps;
    dbbufsize:      TFreeTDSdbbufsize;
    dbclose:        TFreeTDSdbclose;
    dbtablecolinfo: TFreeTDSdbtablecolinfo;
    dbcolinfo:      TFreeTDSdbcolinfo;
    dbconvert_ps:   TFreeTDSdbconvert_ps;
    dbcount:        TFreeTDSdbcount;
    dbdatecmp:      TFreeTDSdbdatecmp;
    dbdatecrack:    TFreeTDSdbdatecrack;
    dbdatlen:       TFreeTDSdbdatlen;
    dbdead:         TFreeTDSdbdead;

    dbgetcharset:   TFreeTDSdbgetcharset;
    dbgetlusername: TFreeTDSdbgetlusername;
    dbgetmaxprocs:  TMsSQLdbgetmaxprocs;
    dbgetnatlanf:   TFreeTDSdbgetnatlanf;
    dbgetpacket:    TFreeTDSdbgetpacket;
    dbgetuserdata:  TFreeTDSdbgetuserdata;
    dbhasretstat:   TFreeTDSdbhasretstat;
    dbinit:         TFreeTDSdbinit;
    dbiordesc:      TFreeTDSdbiordesc;
    dbiowdesc:      TFreeTDSdbiowdesc;
    dbisavail:      TFreeTDSdbisavail;
    dbisopt:        TFreeTDSdbisopt;
    dbloginfree:    TFreeTDSdbloginfree;
    dbmny4cmp:      TFreeTDSdbmny4cmp;
    dbmnycmp:       TFreeTDSdbmnycmp;
    dbmny4add:      TFreeTDSdbmny4add;
    dbmnydec:       TFreeTDSdbmnydec;
    dbmnyinc:       TFreeTDSdbmnyinc;
    dbmnymaxpos:    TFreeTDSdbmnymaxpos;
    dbmnymaxneg:    TFreeTDSdbmnymaxneg;
    dbmny4minus:    TFreeTDSdbmny4minus;
    dbmnyminus:     TFreeTDSdbmnyminus;
    dbmny4sub:      TFreeTDSdbmny4sub;
    dbmnysub:       TFreeTDSdbmnysub;
    dbmny4copy:     TFreeTDSdbmny4copy;
    dbmnycopy:      TFreeTDSdbmnycopy;
    dbmny4zero:     TFreeTDSdbmny4zero;
    dbmnyzero:      TFreeTDSdbmnyzero;
    dbmonthname:    TFreeTDSdbmonthname;
    tdsdbopen:      TFreeTDSdbopen;

  { pivot functions */
  void dbpivot_count (struct col_t *output, const struct col_t *input);
  void dbpivot_sum (struct col_t *output, const struct col_t *input);
  void dbpivot_min (struct col_t *output, const struct col_t *input);
  void dbpivot_max (struct col_t *output, const struct col_t *input);

  struct pivot_t;
  typedef void (*DBPIVOT_FUNC)(struct col_t *output, const struct col_t *input);
  struct pivot_t * dbrows_pivoted(DBPROCESS *dbproc);
  STATUS dbnextrow_pivoted(DBPROCESS *dbproc, struct pivot_t *pp);
  RETCODE dbpivot(DBPROCESS *dbproc, int nkeys, int *keys, int ncols, int *cols, DBPIVOT_FUNC func, int val);

  DBPIVOT_FUNC dbpivot_lookup_name( const char name[] );
  }
    DRBUF:          TDRBUF;
    dbrecftos:      TFreeTDSdbrecftos;
    dbresults_r:    TFreeTDSdbresults_r;
    dbsafestr:      TFreeTDSdbsafestr;
    //dbsechandle:    TFreeTDSdbsechandle;
    dbservcharset:  TFreeTDSdbservcharset;
    //dbsetbusy:      TFreeTDSdbsetbusy;
    dbsetdefcharset:TFreeTDSdbsetdefcharset;
    dbsetifile:     TFreeTDSdbsetifile;
    //dbsetinterrupt: TFreeTDSdbsetinterrupt;
    dbsetmaxprocs:  TMsSQLdbsetmaxprocs;
    dbsetopt:       TFreeTDSdbsetopt;
    dbsetrow:       TFreeTDSdbsetrow;
    dbsetversion:   TFreeTDSdbsetversion;
    dbspid:         TFreeTDSdbspid;
    dbspr1row:      TFreeTDSdbspr1row;
    dbspr1rowlen:   TFreeTDSdbspr1rowlen;
    dbsprhead:      TFreeTDSdbsprhead;
    dbsprline:      TFreeTDSdbsprline;
    dbvarylen:      TFreeTDSdbvarylen;

    dbtds:          TFreeTDSdbtds;
    dbtextsize:     TFreeTDSdbtextsize;
    dbwillconvert:  TFreeTDSdbwillconvert;
    dbsetlbool:     TFreeTDSdbsetlbool;
    dbsetllong:     TFreeTDSdbsetllong;
    dbsetlversion:  TFreeTDSdbsetlversion;

    tdsdump_on            : Ttdsdump_on;
    tdsdump_off           : Ttdsdump_off;
    tdsdump_open          : Ttdsdump_open;
    tdsdump_close         : Ttdsdump_close;
    _tds_socket_init      : T_tds_socket_init;
    _tds_socket_done      : T_tds_socket_done;
  End;

  TMsSQLAPI = record

  { Standard DB-Library functions }
    dbclose               : TMsSQLdbclose;
    dbcolbrowse           : TMsSQLdbcolbrowse;
    dbcolinfo             : TMsSQLdbcolinfo;
    dbcount               : TMsSQLdbcount;

    dbcursorbind          : TMsSQLdbcursorbind;
    dbcursorclose         : TMsSQLdbcursorclose;
    dbcursorcolinfo       : TMsSQLdbcursorcolinfo;
    dbcursorfetch         : TMsSQLdbcursorfetch;
    dbcursorfetchex       : TMsSQLdbcursorfetchex;
    dbcursorinfo          : TMsSQLdbcursorinfo;
    dbcursorinfoex        : TMsSQLdbcursorinfoex;
    dbcursoropen          : TMsSQLdbcursoropen;
    dbdataready           : TMsSQLdbdataready;
    dbdatecrack           : TMsSQLdbdatecrack;
    dbdatlen              : TMsSQLdbdatlen;
    dbdead                : TMsSQLdbdead;
    dbWinexit             : TMsSQLdbWinexit;
    dbenlisttrans         : TMsSQLdbenlisttrans;
    dbenlistxatrans       : TMsSQLdbenlistxatrans;
    dbfreelogin           : TMsSQLdbfreelogin;
    dbgetmaxprocs         : TMsSQLdbgetmaxprocs;
    dbgetpacket           : TMsSQLdbgetpacket;
    dbgetuserdata         : TMsSQLdbgetuserdata;
    dbhasretstat          : TMsSQLdbhasretstat;
    dbinit                : TMsSQLdbinit;
    dbisavail             : TMsSQLdbisavail;
    dbisopt               : TMsSQLdbisopt;
    dbprocinfo            : TMsSQLdbprocinfo;
    dbrpcexec             : TMsSQLdbrpcexec;
    dbserverenum          : TMsSQLdbserverenum;
    dbsetmaxprocs         : TMsSQLdbsetmaxprocs;
    dbsetlpacket          : TMsSQLdbsetlpacket;
    dbsetopt              : TMsSQLdbsetopt;
    dbtabbrowse           : TMsSQLdbtabbrowse;
    dbvarylen             : TMsSQLdbvarylen;
    dbwillconvert         : TMsSQLdbwillconvert;
    dbupdatetext          : TMsSQLdbupdatetext;
  end;

  TSybaseAPI = record
    db12hour              : TSybdb12hour;

    dberrhandle           : TSybdberrhandle;
    dbmsghandle           : TSybdbmsghandle;

    { Two-phase commit functions }
    abort_xact            : TSybabort_xact;
    build_xact_string     : TSybbuild_xact_string;
    close_commit          : TSybclose_commit;
    commit_xact           : TSybcommit_xact;
    open_commit           : TSybopen_commit;
    remove_xact           : TSybremove_xact;
    scan_xact             : TSybscan_xact;
    start_xact            : TSybstart_xact;
    stat_xact             : TSybstat_xact;

  { BCP functions }
    bcp_batch             : TSybbcp_batch;
    bcp_bind              : TSybbcp_bind;
    bcp_colfmt            : TSybbcp_colfmt;
    bcp_collen            : TSybbcp_collen;
    bcp_colptr            : TSybbcp_colptr;
    bcp_columns           : TSybbcp_columns;
    bcp_control           : TSybbcp_control;
    bcp_done              : TSybbcp_done;
    bcp_exec              : TSybbcp_exec;
    bcp_init              : TSybbcp_init;
    bcp_moretext          : TSybbcp_moretext;
    bcp_readfmt           : TSybbcp_readfmt;
    bcp_sendrow           : TSybbcp_sendrow;
    bcp_setl              : TSybbcp_setl;
    bcp_writefmt          : TSybbcp_writefmt;

  { Standard DB-Library functions }
    dbadata               : TSybdbadata;
    dbadlen               : TSybdbadlen;
    dbaltbind             : TSybdbaltbind;
    dbaltcolid            : TSybdbaltcolid;
    dbaltlen              : TSybdbaltlen;
    dbaltop               : TSybdbaltop;
    dbalttype             : TSybdbalttype;
    dbaltutype            : TSybdbaltutype;
    dbanullbind           : TSybdbanullbind;
    dbbind                : TSybdbbind;
    dbbylist              : TSybdbbylist;
    dbcancel              : TSybdbcancel;
    dbcanquery            : TSybdbcanquery;
    dbchange              : TSybdbchange;
    dbclose               : TSybdbclose;
    dbclrbuf              : TSybdbclrbuf;
    dbclropt              : TSybdbclropt;
    dbcmd                 : TSybdbcmd;
    dbcmdrow              : TSybdbcmdrow;
    dbcolbrowse           : TSybdbcolbrowse;
    dbcollen              : TSybdbcollen;
    dbcolinfo             : TSybdbcolinfo;
    dbcolname             : TSybdbcolname;
    dbcolsource           : TSybdbcolsource;
    dbcoltypeinfo         : TSybdbcoltypeinfo;
    dbcoltype             : TSybdbcoltype;
    dbcolutype            : TSybdbcolutype;
    dbconvert             : TSybdbconvert;
    dbcount               : TSybdbcount;
    dbcurcmd              : TSybdbcurcmd;
    dbcurrow              : TSybdbcurrow;

    dbcursor              : TSybdbcursor;
    dbcursorbind          : TSybdbcursorbind;
    dbcursorclose         : TSybdbcursorclose;
    dbcursorcolinfo       : TSybdbcursorcolinfo;
    dbcursorfetch         : TSybdbcursorfetch;
    dbcursorinfo          : TSybdbcursorinfo;
    dbcursoropen          : TSybdbcursoropen;
    dbdata                : TSybdbdata;
    dbdatecrack           : TSybdbdatecrack;
    dbdatlen              : TSybdbdatlen;
    dbdead                : TSybdbdead;
    dbexit                : TSybdbexit;
    dbfcmd                : TSybdbfcmd;
    dbfirstrow            : TSybdbfirstrow;
    dbfreebuf             : TSybdbfreebuf;
    dbloginfree           : TSybdbloginfree;
    dbfreequal            : TSybdbfreequal;
    dbgetchar             : TSybdbgetchar;
    dbgetmaxprocs         : TSybdbgetmaxprocs;
    dbgetoff              : TSybdbgetoff;
    dbgetpacket           : TSybdbgetpacket;
    dbgetrow              : TSybdbgetrow;
    dbgetuserdata         : TSybdbgetuserdata;
    dbhasretstat          : TSybdbhasretstat;
    dbinit                : TSybdbinit;
    dbisavail             : TSybdbisavail;
    dbisopt               : TSybdbisopt;
    dblastrow             : TSybdblastrow;
    dblogin               : TSybdblogin;
    dbmorecmds            : TSybdbmorecmds;
    dbmoretext            : TSybdbmoretext;
    dbname                : TSybdbname;
    dbnextrow             : TSybdbnextrow;
    dbnullbind            : TSybdbnullbind;
    dbnumalts             : TSybdbnumalts;
    dbnumcols             : TSybdbnumcols;
    dbnumcompute          : TSybdbnumcompute;
    dbnumorders           : TSybdbnumorders;
    dbnumrets             : TSybdbnumrets;
    dbopen                : TSybdbopen;
    dbordercol            : TSybdbordercol;
    dbprhead              : TSybdbprhead;
    dbprrow               : TSybdbprrow;
    dbprtype              : TSybdbprtype;
    dbqual                : TSybdbqual;
    dbreadtext            : TSybdbreadtext;
    dbresults             : TSybdbresults;
    dbretdata             : TSybdbretdata;
    dbretlen              : TSybdbretlen;
    dbretname             : TSybdbretname;
    dbretstatus           : TSybdbretstatus;
    dbrettype             : TSybdbrettype;
    dbrows                : TSybdbrows;
    dbrowtype             : TSybdbrowtype;
    dbrpcinit             : TSybdbrpcinit;
    dbrpcparam            : TSybdbrpcparam;
    dbrpcsend             : TSybdbrpcsend;

    dbrpwclr              : TSybdbrpwclr;
    dbsetavail            : TSybdbsetavail;
    dbsetmaxprocs         : TSybdbsetmaxprocs;
    dbsetlname            : TSybdbsetlname;
    dbsetlogintime        : TSybdbsetlogintime;

    dbsetnull             : TSybdbsetnull;
    dbsetopt              : TSybdbsetopt;
    dbsettime             : TSybdbsettime;
    dbsetuserdata         : TSybdbsetuserdata;
    dbsqlexec             : TSybdbsqlexec;
    dbsqlok               : TSybdbsqlok;
    dbsqlsend             : TSybdbsqlsend;
    dbstrcpy              : TSybdbstrcpy;
    dbstrlen              : TSybdbstrlen;
    dbtabbrowse           : TSybdbtabbrowse;
    dbtabcount            : TSybdbtabcount;
    dbtabname             : TSybdbtabname;
    dbtabsource           : TSybdbtabsource;
    dbtsnewlen            : TSybdbtsnewlen;
    dbtsnewval            : TSybdbtsnewval;
    dbtsput               : TSybdbtsput;
    dbtxptr               : TSybdbtxptr;
    dbtxtimestamp         : TSybdbtxtimestamp;
    dbtxtsnewval          : TSybdbtxtsnewval;
    dbtxtsput             : TSybdbtxtsput;
    dbuse                 : TSybdbuse;
    dbvarylen             : TSybdbvarylen;
    dbwillconvert         : TSybdbwillconvert;
    dbwritetext           : TSybdbwritetext;
  end;

{$ENDIF ZEOS_DISABLE_DBLIB}

implementation

end.
