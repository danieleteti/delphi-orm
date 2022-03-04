{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Delphi interface to Oracle Call Interface        }
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

unit ZPlainOracleConstants;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_ORACLE}
{$J+}

uses
  ZCompatibility;

{***************** Plain API types definition ****************}

const
  WINDOWS_DLL_LOCATION = 'oci.dll';
//  WINDOWS_DLL_LOCATION = 'ora803.dll';
  LINUX_DLL_LOCATION = 'libclntsh'+SharedSuffix;
//  LINUX_DLL_LOCATION = 'libwtc8.so';

type
  { Generic Oracle Types }
  sword   = Integer;
  psword  = ^sword;
  eword   = Integer;
  uword   = Cardinal;
  sb4     = Integer;
  ub4     = Cardinal;
  sb2     = SmallInt;
  ub2     = Word;
  sb1     = ShortInt;
  ub1     = Byte;
  dvoid   = Pointer;
  text    = PAnsiChar;
  size_T  = NativeUInt;

  pub1 = ^ub1;
  psb1 = ^sb1;
  pub2 = ^ub2;
  psb2 = ^sb2;
  pub4 = ^ub4;
  psb4 = ^sb4;

  { Handle Types }
  POCIHandle = Pointer;
  PPOCIHandle = ^Pointer;
  POCIEnv = POCIHandle;
  POCIServer = POCIHandle;
  POCIError = POCIHandle;
  POCISvcCtx = POCIHandle;
  POCIStmt = POCIHandle;
  POCIDefine = POCIHandle;
  POCISession = POCIHandle;
  POCIBind = POCIHandle;
  POCIDescribe = POCIHandle;
  POCITrans = POCIHandle;
  POCITable = POCIHandle;
  POCIIter = POCIHandle;
  POCIType = Pointer;
  PPOCIType = ^POCIType;
  POCIInd = Pointer;
  PPOCIInd = ^POCIInd;

  POCITypeCode  = POCIHandle;
  POCITypeElem  = POCIHandle;
  POCITypeIter  = POCIHandle;
  PPOCITypeIter = ^POCITypeIter;
  POCITypeMethod  = POCIHandle;

  OCITypeGetOpt = (
    OCI_TYPEGET_HEADER,// load only the header portion of the TDO when getting type
    OCI_TYPEGET_ALL  //load all attribute and method descriptors as well
    );

  { Descriptor Types }
  POCIDescriptor = Pointer;
  PPOCIDescriptor = ^POCIDescriptor;
  POCISnapshot = POCIDescriptor;          //OCI snapshot descriptor
  POCILobLocator = POCIDescriptor;        //OCI Lob Locator descriptor
  POCIParam = POCIDescriptor;             //OCI Parameter descriptor
  POCIRowid = POCIDescriptor;             //OCI ROWID descriptor
  POCIComplexObjectComp = POCIDescriptor;
  POCIAQEnqOptions = POCIDescriptor;
  POCIAQDeqOptions = POCIDescriptor;
  POCIAQMsgProperties = POCIDescriptor;
  POCIAQAgent = POCIDescriptor;
  POCIDateTime = POCIDescriptor;          //OCI DateTime descriptor
  POCINumber = POCIDescriptor;
  PPOCINumber = ^POCINumber;
  POCIString = POCIDescriptor;
  POCIInterval = POCIDescriptor;          //OCI Interval descriptor
  POCIResult = POCIDescriptor;            //OCI Result Set Descriptor
  PPOCITypeElem = PPOCIDescriptor;
  PPOCITypeMethod = PPOCIDescriptor;

  OCIDuration = ub2;       //enum!
  POCIDuration = ^OCIDuration;
  OCITypeEncap = ub2;      //enum!
  OCITypeMethodFlag = ub2; //enum!
  OCITypeParamMode = sb4;  //enum!
  OCIObjectPropId = ub1;
  OCIRefreshOpt = ub2;     //enum!

  OCITypeCode = ub2; //enum!
  OCIPinOpt = ub2;
  OCILockOpt = pub2;

  POCIComplexObject = Pointer;
  PPOCIComplexObject = ^POCIComplexObject;
const
  OCI_DURATION_INVALID = $FFFF;      { Invalid duration }
  OCI_DURATION_BEGIN = 10;           { beginning sequence of duration }
  OCI_DURATION_NULL = (OCI_DURATION_BEGIN-1); { null duration }
  OCI_DURATION_DEFAULT = (OCI_DURATION_BEGIN-2); { default }
  OCI_DURATION_USER_CALLBACK = (OCI_DURATION_BEGIN-3);
  OCI_DURATION_NEXT = (OCI_DURATION_BEGIN-4); { next special duration }
  OCI_DURATION_SESSION = (OCI_DURATION_BEGIN); { the end of user session }
  OCI_DURATION_TRANS = (OCI_DURATION_BEGIN+1); { the end of user transaction }
  OCI_DURATION_STATEMENT = (OCI_DURATION_BEGIN+3);
{ This is to be used only during callouts.  It is similar to that
of OCI_DURATION_CALL, but lasts only for the duration of a callout.
Its heap is from PGA }
  OCI_DURATION_CALLOUT = (OCI_DURATION_BEGIN+4);
  OCI_DURATION_LAST = OCI_DURATION_CALLOUT; { last of predefined durations }

  OCI_TEMP_BLOB     = 1; { LOB type - BLOB }
  OCI_TEMP_CLOB     = 2; { LOB type - CLOB }

const
  MAXTXNAMELEN    = 64;
  XIDDATASIZE     = 128; { size in bytes }
  MAXGTRIDSIZE    = 64;  { maximum size in bytes of gtrid }
  MAXBQUALSIZE    = 64;  { maximum size in bytes of bqual }
  NULLXID_ID      = -1;

  { Transaction branch identification: XID and NULLXID: }
type
  PXID = ^TXID;
  TXID = record
    formatID: sb4;     { format identifier }
    gtrid_length: sb4; { value from 1 through 64 }
    bqual_length: sb4; { value from 1 through 64 }
    data: array [0 .. XIDDATASIZE - 1] of ub1;
  end;

const
  MAXUB4  = High(ub4);
  MAXSB4  = High(sb4);

{***************** Plain API constants definition ****************}

  { OCI Handle Types }
  OCI_HTYPE_FIRST               = 1;
  OCI_HTYPE_ENV                 = 1;
  OCI_HTYPE_ERROR               = 2;
  OCI_HTYPE_SVCCTX              = 3;
  OCI_HTYPE_STMT                = 4;
  OCI_HTYPE_BIND                = 5;
  OCI_HTYPE_DEFINE              = 6;
  OCI_HTYPE_DESCRIBE            = 7;
  OCI_HTYPE_SERVER              = 8;
  OCI_HTYPE_SESSION             = 9;
  OCI_HTYPE_TRANS               = 10;
  OCI_HTYPE_COMPLEXOBJECT       = 11;
  OCI_HTYPE_SECURITY            = 12;
  OCI_HTYPE_SUBSCRIPTION        = 13;
  OCI_HTYPE_DIRPATH_CTX         = 14;
  OCI_HTYPE_DIRPATH_COLUMN_ARRAY = 15;
  OCI_HTYPE_DIRPATH_STREAM      = 16;
  OCI_HTYPE_PROC                = 17;
  OCI_HTYPE_LAST                = 17;

  { OCI Descriptor Types }
  OCI_DTYPE_FIRST               = 50;
  OCI_DTYPE_LOB                 = 50; //lob  locator
  OCI_DTYPE_SNAP                = 51;
  OCI_DTYPE_RSET                = 52;
  OCI_DTYPE_PARAM               = 53; //a parameter descriptor obtained from ocigparm
  OCI_DTYPE_ROWID               = 54;
  OCI_DTYPE_COMPLEXOBJECTCOMP   = 55;
  OCI_DTYPE_FILE                = 56; //File Lob locator
  OCI_DTYPE_AQENQ_OPTIONS       = 57; //enqueue options
  OCI_DTYPE_AQDEQ_OPTIONS       = 58; //dequeue options
  OCI_DTYPE_AQMSG_PROPERTIES    = 59; //message properties
  OCI_DTYPE_AQAGENT             = 60; //aq agent
  OCI_DTYPE_LOCATOR             = 61;
  OCI_DTYPE_INTERVAL_YM         = 62; //Interval year month
  OCI_DTYPE_INTERVAL_DS         = 63; //Interval day second
  OCI_DTYPE_AQNFY_DESCRIPTOR    = 64;
  OCI_DTYPE_LAST                = 64;
  OCI_DTYPE_DATE                = 65;  { Date }
  OCI_DTYPE_TIME                = 66;  { Time }
  OCI_DTYPE_TIME_TZ             = 67;  { Time with timezone }
  OCI_DTYPE_TIMESTAMP           = 68;  { Timestamp }
  OCI_DTYPE_TIMESTAMP_TZ        = 69;  { Timestamp with timezone }
  OCI_DTYPE_TIMESTAMP_LTZ       = 70;  { Timestamp with local tz }

  { OCI Attributes Types }
  OCI_ATTR_FNCODE               = 1;   // the OCI function code
  OCI_ATTR_OBJECT               = 2;   // is the environment initialized in object mode
  OCI_ATTR_NONBLOCKING_MODE     = 3;   // non blocking mode
  OCI_ATTR_SQLCODE              = 4;   // the SQL verb
  OCI_ATTR_ENV                  = 5;   // the environment handle
  OCI_ATTR_SERVER               = 6;   // the server handle
  OCI_ATTR_SESSION              = 7;   // the user session handle
  OCI_ATTR_TRANS                = 8;   // the transaction handle
  OCI_ATTR_ROW_COUNT            = 9;   // the rows processed so far
  OCI_ATTR_SQLFNCODE            = 10;  // the SQL verb of the statement
  OCI_ATTR_PREFETCH_ROWS        = 11;  // sets the number of rows to prefetch
  OCI_ATTR_NESTED_PREFETCH_ROWS = 12;  // the prefetch rows of nested table
  OCI_ATTR_PREFETCH_MEMORY      = 13;  // memory limit for rows fetched
  OCI_ATTR_NESTED_PREFETCH_MEMORY = 14;// memory limit for nested rows
  OCI_ATTR_CHAR_COUNT           = 15;  // this specifies the bind and define size in characters
  OCI_ATTR_PDSCL                = 16;  // packed decimal scale
  OCI_ATTR_FSPRECISION          = OCI_ATTR_PDSCL; // fs prec for datetime data types
  OCI_ATTR_PDPRC                = 17;  // packed decimal format
  OCI_ATTR_LFPRECISION          = OCI_ATTR_PDPRC; // fs prec for datetime data types
  OCI_ATTR_PARAM_COUNT          = 18;  // number of column in the select list
  OCI_ATTR_ROWID                = 19;  // the rowid
  OCI_ATTR_CHARSET              = 20;  // the character set value
  OCI_ATTR_NCHAR                = 21;  // NCHAR type
  OCI_ATTR_USERNAME             = 22;  // username attribute
  OCI_ATTR_PASSWORD             = 23;  // password attribute
  OCI_ATTR_STMT_TYPE            = 24;  // statement type
  OCI_ATTR_INTERNAL_NAME        = 25;  // user friendly global name
  OCI_ATTR_EXTERNAL_NAME        = 26;  // the internal name for global txn
  OCI_ATTR_XID                  = 27;  // XOPEN defined global transaction id
  OCI_ATTR_TRANS_LOCK           = 28;  //
  OCI_ATTR_TRANS_NAME           = 29;  // string to identify a global transaction
  OCI_ATTR_HEAPALLOC            = 30;  // memory allocated on the heap
  OCI_ATTR_CHARSET_ID           = 31;  // Character Set ID
  OCI_ATTR_CHARSET_FORM         = 32;  // Character Set Form
  OCI_ATTR_MAXDATA_SIZE         = 33;  // Maximumsize of data on the server
  OCI_ATTR_CACHE_OPT_SIZE       = 34;  // object cache optimal size
  OCI_ATTR_CACHE_MAX_SIZE       = 35;  // object cache maximum size percentage
  OCI_ATTR_PINOPTION            = 36;  // object cache default pin option
  OCI_ATTR_ALLOC_DURATION       = 37;  // object cache default allocation duration
  OCI_ATTR_PIN_DURATION         = 38;  // object cache default pin duration
  OCI_ATTR_FDO                  = 39;  // Format Descriptor object attribute
  OCI_ATTR_POSTPROCESSING_CALLBACK = 40;  // Callback to process outbind data
  OCI_ATTR_POSTPROCESSING_CONTEXT = 41; // Callback context to process outbind data
  OCI_ATTR_ROWS_RETURNED        = 42;  // Number of rows returned in current iter - for Bind handles
  OCI_ATTR_FOCBK                = 43;  // Failover Callback attribute
  OCI_ATTR_IN_V8_MODE           = 44;  // is the server/service context in V8 mode
  OCI_ATTR_LOBEMPTY             = 45;  // empty lob ?
  OCI_ATTR_SESSLANG             = 46;  // session language handle

  OCI_ATTR_VISIBILITY           = 47;  // visibility
  OCI_ATTR_RELATIVE_MSGID       = 48;  // relative message id
  OCI_ATTR_SEQUENCE_DEVIATION   = 49;  // sequence deviation

  OCI_ATTR_CONSUMER_NAME        = 50;  // consumer name
  OCI_ATTR_DEQ_MODE             = 51;  // dequeue mode
  OCI_ATTR_NAVIGATION           = 52;  // navigation
  OCI_ATTR_WAIT                 = 53;  // wait
  OCI_ATTR_DEQ_MSGID            = 54;  // dequeue message id

  OCI_ATTR_PRIORITY             = 55;  // priority
  OCI_ATTR_DELAY                = 56;  // delay
  OCI_ATTR_EXPIRATION           = 57;  // expiration
  OCI_ATTR_CORRELATION          = 58;  // correlation id
  OCI_ATTR_ATTEMPTS             = 59;  // # of attempts
  OCI_ATTR_RECIPIENT_LIST       = 60;  // recipient list
  OCI_ATTR_EXCEPTION_QUEUE      = 61;  // exception queue name
  OCI_ATTR_ENQ_TIME             = 62;  // enqueue time (only OCIAttrGet)
  OCI_ATTR_MSG_STATE            = 63;  // message state (only OCIAttrGet)
                                       // NOTE: 64-66 used below
  OCI_ATTR_AGENT_NAME           = 64;  // agent name
  OCI_ATTR_AGENT_ADDRESS        = 65;  // agent address
  OCI_ATTR_AGENT_PROTOCOL       = 66;  // agent protocol

  OCI_ATTR_SENDER_ID            = 68;  // sender id
  OCI_ATTR_ORIGINAL_MSGID       = 69;  // original message id

  OCI_ATTR_QUEUE_NAME           = 70;  // queue name
  OCI_ATTR_NFY_MSGID            = 71;  // message id
  OCI_ATTR_MSG_PROP             = 72;  // message properties

  OCI_ATTR_NUM_DML_ERRORS       = 73;  // num of errs in array DML
  OCI_ATTR_DML_ROW_OFFSET       = 74;  // row offset in the array

  OCI_ATTR_DATEFORMAT           = 75;  // default date format string
  OCI_ATTR_BUF_ADDR             = 76;  // buffer address
  OCI_ATTR_BUF_SIZE             = 77;  // buffer size
  OCI_ATTR_DIRPATH_MODE         = 78;  // mode of direct path operation
  OCI_ATTR_DIRPATH_NOLOG        = 79;  // nologging option
  OCI_ATTR_DIRPATH_PARALLEL     = 80;  // parallel (temp seg) option
  OCI_ATTR_NUM_ROWS             = 81;  // number of rows in column array
                                       // NOTE that OCI_ATTR_NUM_COLS is a column
                                       // array attribute too.

  OCI_ATTR_COL_COUNT            = 82;  // columns of column array processed so far.
  OCI_ATTR_STREAM_OFFSET        = 83;  // str off of last row processed
  OCI_ATTR_SHARED_HEAPALLOC     = 84;  // Shared Heap Allocation Size

  OCI_ATTR_SERVER_GROUP         = 85;  // server group name

  OCI_ATTR_MIGSESSION           = 86;  // migratable session attribute

  OCI_ATTR_NOCACHE              = 87;  // Temporary LOBs

  OCI_ATTR_MEMPOOL_SIZE         = 88;  // Pool Size
  OCI_ATTR_MEMPOOL_INSTNAME     = 89;  // Instance name
  OCI_ATTR_MEMPOOL_APPNAME      = 90;  // Application name
  OCI_ATTR_MEMPOOL_HOMENAME     = 91;  // Home Directory name
  OCI_ATTR_MEMPOOL_MODEL        = 92;  // Pool Model (proc,thrd,both)
  OCI_ATTR_MODES                = 93;  // Modes

  OCI_ATTR_SUBSCR_NAME          = 94;  // name of subscription
  OCI_ATTR_SUBSCR_CALLBACK      = 95;  // associated callback
  OCI_ATTR_SUBSCR_CTX           = 96;  // associated callback context
  OCI_ATTR_SUBSCR_PAYLOAD       = 97;  // associated payload
  OCI_ATTR_SUBSCR_NAMESPACE     = 98;  // associated namespace

  OCI_ATTR_PROXY_CREDENTIALS    = 99;  // Proxy user credentials
  OCI_ATTR_INITIAL_CLIENT_ROLES = 100; // Initial client role list

  OCI_ATTR_UNK                  = 101; // unknown attribute
  OCI_ATTR_NUM_COLS             = 102; // number of columns
  OCI_ATTR_LIST_COLUMNS         = 103; // parameter of the column list
  OCI_ATTR_RDBA                 = 104; // DBA of the segment header
  OCI_ATTR_CLUSTERED            = 105; // whether the table is clustered
  OCI_ATTR_PARTITIONED          = 106; // whether the table is partitioned
  OCI_ATTR_INDEX_ONLY           = 107; // whether the table is index only
  OCI_ATTR_LIST_ARGUMENTS       = 108; // parameter of the argument list
  OCI_ATTR_LIST_SUBPROGRAMS     = 109; // parameter of the subprogram list
  OCI_ATTR_REF_TDO              = 110; // REF to the type descriptor
  OCI_ATTR_LINK                 = 111; // the database link name
  OCI_ATTR_MIN                  = 112; // minimum value
  OCI_ATTR_MAX                  = 113; // maximum value
  OCI_ATTR_INCR                 = 114; // increment value
  OCI_ATTR_CACHE                = 115; // number of sequence numbers cached
  OCI_ATTR_ORDER                = 116; // whether the sequence is ordered
  OCI_ATTR_HW_MARK              = 117; // high-water mark
  OCI_ATTR_TYPE_SCHEMA          = 118; // type's schema name
  OCI_ATTR_TIMESTAMP            = 119; // timestamp of the object
  OCI_ATTR_NUM_ATTRS            = 120; // number of sttributes
  OCI_ATTR_NUM_PARAMS           = 121; // number of parameters
  OCI_ATTR_OBJID                = 122; // object id for a table or view
  OCI_ATTR_PTYPE                = 123; // type of info described by
  OCI_ATTR_PARAM                = 124; // parameter descriptor
  OCI_ATTR_OVERLOAD_ID          = 125; // overload ID for funcs and procs
  OCI_ATTR_TABLESPACE           = 126; // table name space
  OCI_ATTR_TDO                  = 127; // TDO of a type
  OCI_ATTR_LTYPE                = 128; // list type
  OCI_ATTR_PARSE_ERROR_OFFSET   = 129; // Parse Error offset
  OCI_ATTR_IS_TEMPORARY         = 130; // whether table is temporary
  OCI_ATTR_IS_TYPED             = 131; // whether table is typed
  OCI_ATTR_DURATION             = 132; // duration of temporary table
  OCI_ATTR_IS_INVOKER_RIGHTS    = 133; // is invoker rights
  OCI_ATTR_OBJ_NAME             = 134; // top level schema obj name
  OCI_ATTR_OBJ_SCHEMA           = 135; // schema name
  OCI_ATTR_OBJ_ID               = 136; // top level schema object id

  OCI_ATTR_TRANS_TIMEOUT        = 142; // transaction timeout
  OCI_ATTR_SERVER_STATUS        = 143; // state of the server handle
  OCI_ATTR_STATEMENT            = 144; // statement txt in stmt hdl

  OCI_ATTR_DEQCOND              = 146; // dequeue condition
  OCI_ATTR_RESERVED_2           = 147; // reserved


  OCI_ATTR_SUBSCR_RECPT         = 148; // recepient of subscription
  OCI_ATTR_SUBSCR_RECPTPROTO    = 149; // protocol for recepient

  { For values 150 - 151, see DirPathAPI attribute section in this file }

  OCI_ATTR_LDAP_HOST            = 153; //LDAP host to connect to
  OCI_ATTR_LDAP_PORT            = 154; //LDAP port to connect to
  OCI_ATTR_BIND_DN              = 155; //bind DN
  OCI_ATTR_LDAP_CRED            = 156; //credentials to connect to LDAP
  OCI_ATTR_WALL_LOC             = 157; // client wallet location
  OCI_ATTR_LDAP_AUTH            = 158; // LDAP authentication method
  OCI_ATTR_LDAP_CTX             = 159; // LDAP adminstration context DN
  OCI_ATTR_SERVER_DNS           = 160; // list of registration server DNs

  OCI_ATTR_DN_COUNT             = 161; // the number of server DNs
  OCI_ATTR_SERVER_DN            = 162; // server DN attribute

  OCI_ATTR_MAXCHAR_SIZE         = 163; // max char size of data

  OCI_ATTR_CURRENT_POSITION     = 164; // for scrollable result sets

// Added to get attributes for ref cursor to statement handle
  OCI_ATTR_RESERVED_3           = 165; // reserved
  OCI_ATTR_RESERVED_4           = 166; // reserved

// For value 167, see DirPathAPI attribute section in this file

  OCI_ATTR_DIGEST_ALGO          = 168; // digest algorithm
  OCI_ATTR_CERTIFICATE          = 169; // certificate
  OCI_ATTR_SIGNATURE_ALGO       = 170; // signature algorithm
  OCI_ATTR_CANONICAL_ALGO       = 171; // canonicalization algo.
  OCI_ATTR_PRIVATE_KEY          = 172; // private key
  OCI_ATTR_DIGEST_VALUE         = 173; // digest value
  OCI_ATTR_SIGNATURE_VAL        = 174; // signature value
  OCI_ATTR_SIGNATURE            = 175; // signature

// attributes for setting OCI stmt caching specifics in svchp
  OCI_ATTR_STMTCACHESIZE        = 176; // size of the stm cache

// --------------------------- Connection Pool Attributes ------------------
  OCI_ATTR_CONN_NOWAIT          = 178;
  OCI_ATTR_CONN_BUSY_COUNT      = 179;
  OCI_ATTR_CONN_OPEN_COUNT      = 180;
  OCI_ATTR_CONN_TIMEOUT         = 181;
  OCI_ATTR_STMT_STATE           = 182;
  OCI_ATTR_CONN_MIN             = 183;
  OCI_ATTR_CONN_MAX             = 184;
  OCI_ATTR_CONN_INCR            = 185;

// For value 187, see DirPathAPI attribute section in this file

  OCI_ATTR_NUM_OPEN_STMTS       = 188; // open stmts in session
  OCI_ATTR_DESCRIBE_NATIVE      = 189; // get native info via desc

  OCI_ATTR_BIND_COUNT           = 190; // number of bind postions
  OCI_ATTR_HANDLE_POSITION      = 191; // pos of bind/define handle
  OCI_ATTR_RESERVED_5           = 192; // reserverd
  OCI_ATTR_SERVER_BUSY          = 193; // call in progress on server

// For value 194, see DirPathAPI attribute section in this file

// notification presentation for recipient
  OCI_ATTR_SUBSCR_RECPTPRES     = 195;
  OCI_ATTR_TRANSFORMATION       = 196; // AQ message transformation

  OCI_ATTR_ROWS_FETCHED         = 197; // rows fetched in last call

// --------------------------- Snapshot attributes -------------------------
  OCI_ATTR_SCN_BASE             = 198; // snapshot base
  OCI_ATTR_SCN_WRAP             = 199; // snapshot wrap

// --------------------------- Miscellanous attributes ---------------------
  OCI_ATTR_RESERVED_6           = 200; // reserved
  OCI_ATTR_READONLY_TXN         = 201; // txn is readonly
  OCI_ATTR_RESERVED_7           = 202; // reserved
  OCI_ATTR_ERRONEOUS_COLUMN     = 203; // position of erroneous col
  OCI_ATTR_RESERVED_8           = 204; // reserved
  OCI_ATTR_ASM_VOL_SPRT         = 205; // ASM volume supported?

  // for inheritance - part 2
  OCI_ATTR_IS_FINAL_TYPE        = 279; //is final type ?
  OCI_ATTR_IS_INSTANTIABLE_TYPE = 280; //is instantiable type ?
  OCI_ATTR_IS_FINAL_METHOD      = 281; //is final method ?
  OCI_ATTR_IS_INSTANTIABLE_METHOD = 282; // is instantiable method ?
  OCI_ATTR_IS_OVERRIDING_METHOD = 283; // is overriding method ?

  OCI_ATTR_DESC_SYNBASE         = 284; //Describe the base object

  OCI_ATTR_CHAR_USED            = 285; //char length semantics
  OCI_ATTR_CHAR_SIZE            = 286; //char length

  OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE = 438; // default prefetch size

  { OCI Error Return Values }
  OCI_SUCCESS             = 0;
  OCI_SUCCESS_WITH_INFO   = 1;
  OCI_NO_DATA             = 100;
  OCI_ERROR               = -1;
  OCI_INVALID_HANDLE      = -2;
  OCI_NEED_DATA           = 99;
  OCI_STILL_EXECUTING     = -3123;
  OCI_CONTINUE            = -24200;

  { Generic Default Value for Modes, .... }
  OCI_DEFAULT     = $0;

  { OCI Init Mode }
  OCI_THREADED    = $1;
  OCI_OBJECT      = $2;
  OCI_EVENTS      = $4;
  OCI_SHARED      = $10;
  OCI_NO_UCB      = $40;
  OCI_NO_MUTEX    = $80;

  { OCI Credentials }
  OCI_CRED_RDBMS  = 1;
  OCI_CRED_EXT    = 2;
  OCI_CRED_PROXY  = 3;

  { OCI Authentication Mode }
  OCI_MIGRATE     = $0001;             // migratable auth context
  OCI_SYSDBA      = $0002;             // for SYSDBA authorization
  OCI_SYSOPER     = $0004;             // for SYSOPER authorization
  OCI_PRELIM_AUTH = $0008;             // for preliminary authorization

  { OCIPasswordChange }
  OCI_AUTH        = $08;               // Change the password but do not login

  { OCI Data Types }
  SQLT_CHR = 1  ;       //(ORANET TYPE) character string
  SQLT_NUM = 2  ;       //(ORANET TYPE) oracle numeric
  SQLT_INT = 3  ;       //(ORANET TYPE) integer
  SQLT_FLT = 4  ;       //(ORANET TYPE) Floating point number
  SQLT_STR = 5  ;       //zero terminated string
  SQLT_VNU = 6  ;       //NUM with preceding length byte
  SQLT_PDN = 7  ;       //(ORANET TYPE) Packed Decimal Numeric
  SQLT_LNG = 8  ;       //long
  SQLT_VCS = 9  ;       //Variable character string
  SQLT_NON = 10 ;       //Null/empty PCC Descriptor entry
  SQLT_RID = 11 ;       //rowid
  SQLT_DAT = 12 ;       //date in oracle format
  SQLT_VBI = 15 ;       //binary in VCS format
  SQLT_BFLOAT = 21 ;    //Native Binary float
  SQLT_BDOUBLE = 22 ;   //NAtive binary double
  SQLT_BIN = 23 ;       //binary data(DTYBIN)
  SQLT_LBI = 24 ;       //long binary
  _SQLT_PLI = 29;
  SQLT_UIN = 68 ;       //unsigned integer
  SQLT_SLS = 91 ;       //Display sign leading separate
  SQLT_LVC = 94 ;       //Longer longs (char)
  SQLT_LVB = 95 ;       //Longer long binary
  SQLT_AFC = 96 ;       //Ansi fixed char
  SQLT_AVC = 97 ;       //Ansi Var char
  SQLT_IBFLOAT = 100;   //binary float canonical
  SQLT_IBDOUBLE = 101;  //binary double canonical
  SQLT_CUR = 102;       //cursor  type
  SQLT_RDD = 104;       //rowid descriptor
  SQLT_LAB = 105;       //label type
  SQLT_OSL = 106;       //oslabel type
  SQLT_NTY = 108;       //named object type
  SQLT_REF = 110;       //ref typ
  SQLT_CLOB = 112;      //character lob
  SQLT_BLOB = 113;      //binary lob
  SQLT_BFILEE = 114;    //binary file lob
  SQLT_CFILEE = 115;    //character file lob
  SQLT_RSET = 116;      //result set type
  SQLT_NCO = 122;       //named collection type (varray or nested table)
  SQLT_VST = 155;       //OCIString type
  SQLT_ODT = 156;       //OCIDate type

  { datetimes and intervals }
  SQLT_DATE = 184;            //ANSI Date
  SQLT_TIME = 185;            //TIME
  SQLT_TIME_TZ = 186;         //TIME WITH TIME ZONE
  SQLT_TIMESTAMP = 187;       //TIMESTAMP
  SQLT_TIMESTAMP_TZ = 188;    //TIMESTAMP WITH TIME ZONE
  SQLT_INTERVAL_YM = 189;     //INTERVAL YEAR TO MONTH
  SQLT_INTERVAL_DS = 190;     //INTERVAL DAY TO SECOND
  SQLT_TIMESTAMP_LTZ = 232;   //TIMESTAMP WITH LOCAL TZ

  _SQLT_REC = 250;
  _SQLT_TAB = 251;
  _SQLT_BOL = 252;

  { > typecode defines from oro.h }
  OCI_TYPECODE_REF              = SQLT_REF; //SQL/OTS OBJECT REFERENCE
  OCI_TYPECODE_VARRAY           = 247;      //SQL VARRAY  OTS PAGED VARRAY
  OCI_TYPECODE_TABLE            = 248;      //SQL TABLE  OTS MULTISET
  OCI_TYPECODE_OBJECT           = SQLT_NTY; //SQL/OTS NAMED OBJECT TYPE
  OCI_TYPECODE_OPAQUE           = 58;       //SQL/OTS Opaque Types
  OCI_TYPECODE_NAMEDCOLLECTION  = SQLT_NCO;

  { OCI Statement Types }
  OCI_STMT_SELECT  = 1;   // select statement
  OCI_STMT_UPDATE  = 2;   // update statement
  OCI_STMT_DELETE  = 3;   // delete statement
  OCI_STMT_INSERT  = 4;   // Insert Statement
  OCI_STMT_CREATE  = 5;   // create statement
  OCI_STMT_DROP    = 6;   // drop statement
  OCI_STMT_ALTER   = 7;   // alter statement
  OCI_STMT_BEGIN   = 8;   // begin ... (pl/sql statement)
  OCI_STMT_DECLARE = 9;   // declare .. (pl/sql statement)

  { OCI Statement language }
  OCI_NTV_SYNTAX  = 1;    // Use what so ever is the native lang of server
  OCI_V7_SYNTAX   = 2;    // V7 language
  OCI_V8_SYNTAX   = 3;    // V8 language

  { OCI Statement Execute mode }
  OCI_BATCH_MODE        = $01;    // batch the oci statement for execution
  OCI_EXACT_FETCH       = $02;    // fetch the exact rows specified
  OCI_STMT_SCROLLABLE_READONLY = $08;    // cursor scrollable
  OCI_DESCRIBE_ONLY     = $10;    // only describe the statement
  OCI_COMMIT_ON_SUCCESS = $20;    // commit, if successful execution
  OCI_NON_BLOCKING      = $40;    // non-blocking
  OCI_BATCH_ERRORS      = $80;    // batch errors in array dmls
  OCI_PARSE_ONLY        = $100;   // only parse the statement
  OCI_SHOW_DML_WARNINGS = $400;   // return OCI_SUCCESS_WITH_INFO for delete/update w/no where clause
  OCI_RESULT_CACHE      = $20000; // hint to use query caching
  OCI_NO_RESULT_CACHE   = $40000; // hint to bypass query caching

  OCI_DATA_AT_EXEC    = $02;      // data at execute time
  OCI_DYNAMIC_FETCH   = $02;      // fetch dynamically
  OCI_PIECEWISE       = $04;      // piecewise DMLs or fetch

  { OCI Transaction modes }
  OCI_TRANS_NEW          = $00000001; // starts a new transaction branch
  OCI_TRANS_JOIN         = $00000002; // join an existing transaction
  OCI_TRANS_RESUME       = $00000004; // resume this transaction
  OCI_TRANS_STARTMASK    = $000000ff;

  OCI_TRANS_READONLY     = $00000100; // starts a readonly transaction
  OCI_TRANS_READWRITE    = $00000200; // starts a read-write transaction
  OCI_TRANS_SERIALIZABLE = $00000400; // starts a serializable transaction
  OCI_TRANS_ISOLMASK     = $0000ff00;

  OCI_TRANS_LOOSE        = $00010000; // a loosely coupled branch
  OCI_TRANS_TIGHT        = $00020000; // a tightly coupled branch
  OCI_TRANS_TYPEMASK     = $000f0000;

  OCI_TRANS_NOMIGRATE    = $00100000; // non migratable transaction
  OCI_TRANS_TWOPHASE     = $01000000; // use two phase commit

  { OCI piece wise fetch }
  OCI_ONE_PIECE       = 0; // one piece
  OCI_FIRST_PIECE     = 1; // the first piece
  OCI_NEXT_PIECE      = 2; // the next of many pieces
  OCI_LAST_PIECE      = 3; // the last piece

  { OCI fetch modes }
  OCI_FETCH_NEXT      = $02;  // next row
  OCI_FETCH_FIRST     = $04;  // first row of the result set
  OCI_FETCH_LAST      = $08;  // the last row of the result set
  OCI_FETCH_PRIOR     = $10;  // the previous row relative to current
  OCI_FETCH_ABSOLUTE  = $20;  // absolute offset from first
  OCI_FETCH_RELATIVE  = $40;  // offset relative to current

  {****************** Describe Handle Parameter Attributes *****************}

  { Attributes common to Columns and Stored Procs }
  OCI_ATTR_DATA_SIZE      = 1;    // ub2 The maximum size of the type attribute.
                                  // This length is returned in bytes and not
                                  // characters for strings and raws.
                                  // It returns 22 for NUMBERs
  OCI_ATTR_DATA_TYPE      = 2;    // the SQL type of the column/argument
  OCI_ATTR_DISP_SIZE      = 3;    // the display size
  OCI_ATTR_NAME           = 4;    // the name of the column/argument
  OCI_ATTR_PRECISION      = 5;    // precision if number type
  OCI_ATTR_SCALE          = 6;    // scale if number type
  OCI_ATTR_IS_NULL        = 7;    // is it null ?
  OCI_ATTR_TYPE_NAME      = 8;    // name of the named data type or a package name for package private types
  OCI_ATTR_SCHEMA_NAME    = 9;    // the schema name
  OCI_ATTR_SUB_NAME       = 10;   // type name if package private type
  OCI_ATTR_POSITION       = 11;   // relative position of col/arg in the list of cols/args

  { complex object retrieval parameter attributes }
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE         = 50;
  OCI_ATTR_COMPLEXOBJECTCOMP_TYPE_LEVEL   = 51;
  OCI_ATTR_COMPLEXOBJECT_LEVEL            = 52;
  OCI_ATTR_COMPLEXOBJECT_COLL_OUTOFLINE   = 53;

  { Only Columns }
  OCI_ATTR_DISP_NAME                 = 100;  // the display name

  { Only Stored Procs }
  OCI_ATTR_OVERLOAD                  = 210;  // is this position overloaded
  OCI_ATTR_LEVEL                     = 211;  // level for structured types
  OCI_ATTR_HAS_DEFAULT               = 212;  // has a default value
  OCI_ATTR_IOMODE                    = 213;  // in, out inout
  OCI_ATTR_RADIX                     = 214;  // returns a radix
  OCI_ATTR_NUM_ARGS                  = 215;  // total number of arguments

  { only named type attributes }
  OCI_ATTR_TYPECODE                  = 216;   // object or collection
  OCI_ATTR_COLLECTION_TYPECODE       = 217;   // varray or nested table
  OCI_ATTR_VERSION                   = 218;   // user assigned version
  OCI_ATTR_IS_INCOMPLETE_TYPE        = 219;   // is this an incomplete type
  OCI_ATTR_IS_SYSTEM_TYPE            = 220;   // a system type
  OCI_ATTR_IS_PREDEFINED_TYPE        = 221;   // a predefined type
  OCI_ATTR_IS_TRANSIENT_TYPE         = 222;   // a transient type
  OCI_ATTR_IS_SYSTEM_GENERATED_TYPE  = 223;   // system generated type
  OCI_ATTR_HAS_NESTED_TABLE          = 224;   // contains nested table attr
  OCI_ATTR_HAS_LOB                   = 225;   // has a lob attribute
  OCI_ATTR_HAS_FILE                  = 226;   // has a file attribute
  OCI_ATTR_COLLECTION_ELEMENT        = 227;   // has a collection attribute
  OCI_ATTR_NUM_TYPE_ATTRS            = 228;   // number of attribute types
  OCI_ATTR_LIST_TYPE_ATTRS           = 229;   // list of type attributes
  OCI_ATTR_NUM_TYPE_METHODS          = 230;   // number of type methods
  OCI_ATTR_LIST_TYPE_METHODS         = 231;   // list of type methods
  OCI_ATTR_MAP_METHOD                = 232;   // map method of type
  OCI_ATTR_ORDER_METHOD              = 233;   // order method of type

  { only collection element }
  OCI_ATTR_NUM_ELEMS                 = 234;   // number of elements

  { only type methods }
  OCI_ATTR_ENCAPSULATION             = 235;   // encapsulation level
  OCI_ATTR_IS_SELFISH                = 236;   // method selfish
  OCI_ATTR_IS_VIRTUAL                = 237;   // virtual
  OCI_ATTR_IS_INLINE                 = 238;   // inline
  OCI_ATTR_IS_CONSTANT               = 239;   // constant
  OCI_ATTR_HAS_RESULT                = 240;   // has result
  OCI_ATTR_IS_CONSTRUCTOR            = 241;   // constructor
  OCI_ATTR_IS_DESTRUCTOR             = 242;   // destructor
  OCI_ATTR_IS_OPERATOR               = 243;   // operator
  OCI_ATTR_IS_MAP                    = 244;   // a map method
  OCI_ATTR_IS_ORDER                  = 245;   // order method
  OCI_ATTR_IS_RNDS                   = 246;   // read no data state method
  OCI_ATTR_IS_RNPS                   = 247;   // read no process state
  OCI_ATTR_IS_WNDS                   = 248;   // write no data state method
  OCI_ATTR_IS_WNPS                   = 249;   // write no process state

  OCI_ATTR_DESC_PUBLIC               = 250;   // public object

  { Object Cache Enhancements : attributes for User Constructed Instances }
  OCI_ATTR_CACHE_CLIENT_CONTEXT      = 251;
  OCI_ATTR_UCI_CONSTRUCT             = 252;
  OCI_ATTR_UCI_DESTRUCT              = 253;
  OCI_ATTR_UCI_COPY                  = 254;
  OCI_ATTR_UCI_PICKLE                = 255;
  OCI_ATTR_UCI_UNPICKLE              = 256;
  OCI_ATTR_UCI_REFRESH               = 257;

  { for type inheritance }
  OCI_ATTR_IS_SUBTYPE                = 258;
  OCI_ATTR_SUPERTYPE_SCHEMA_NAME     = 259;
  OCI_ATTR_SUPERTYPE_NAME            = 260;

  { for schemas }
  OCI_ATTR_LIST_OBJECTS              = 261;   // list of objects in schema

  { Enable OCI Server-Side Statement Caching }
  OCI_STMT_CACHE       = $40;
  OCI_STMTCACHE_DELETE = $10;

  { for database }
  OCI_ATTR_NCHARSET_ID               = 262;   // char set id
  OCI_ATTR_LIST_SCHEMAS              = 263;   // list of schemas
  OCI_ATTR_MAX_PROC_LEN              = 264;   // max procedure length
  OCI_ATTR_MAX_COLUMN_LEN            = 265;   // max column name length
  OCI_ATTR_CURSOR_COMMIT_BEHAVIOR    = 266;   // cursor commit behavior
  OCI_ATTR_MAX_CATALOG_NAMELEN       = 267;   // catalog namelength
  OCI_ATTR_CATALOG_LOCATION          = 268;   // catalog location
  OCI_ATTR_SAVEPOINT_SUPPORT         = 269;   // savepoint support
  OCI_ATTR_NOWAIT_SUPPORT            = 270;   // nowait support
  OCI_ATTR_AUTOCOMMIT_DDL            = 271;   // autocommit DDL
  OCI_ATTR_LOCKING_MODE              = 272;   // locking mode

  OCI_ATTR_CACHE_ARRAYFLUSH          = $40;
  OCI_ATTR_OBJECT_NEWNOTNULL         = $10;
  OCI_ATTR_OBJECT_DETECTCHANGE       = $20;

  {client side character and national character set ids }
  OCI_ATTR_ENV_CHARSET_ID       = OCI_ATTR_CHARSET_ID;  // charset id in env
  OCI_ATTR_ENV_NCHARSET_ID      = OCI_ATTR_NCHARSET_ID; // ncharset id in env

  { Piece Information }
  OCI_PARAM_IN                       = $01;  // in parameter
  OCI_PARAM_OUT                      = $02;  // out parameter

  { LOB Buffering Flush Flags }
  OCI_LOB_BUFFER_FREE     = 1;
  OCI_LOB_BUFFER_NOFREE   = 2;

  { FILE open modes }
  OCI_FILE_READONLY   = 1;    // readonly mode open for FILE types
  { LOB open modes }
  OCI_LOB_READONLY    = 1;    // readonly mode open for ILOB types
  OCI_LOB_READWRITE   = 2;    // read write mode open for ILOBs

  { CHAR/NCHAR/VARCHAR2/NVARCHAR2/CLOB/NCLOB char set "form" information }
  SQLCS_IMPLICIT = 1;     // for CHAR, VARCHAR2, CLOB w/o a specified set
  SQLCS_NCHAR    = 2;     // for NCHAR, NCHAR VARYING, NCLOB
  SQLCS_EXPLICIT = 3;     // for CHAR, etc, with "CHARACTER SET ..." syntax
  SQLCS_FLEXIBLE = 4;     // for PL/SQL "flexible" parameters
  SQLCS_LIT_NULL = 5;     // for typecheck of NULL and empty_clob() lits

  {************************ OCIDesribeAny *************************}

  { Describe mode }
  OCI_OTYPE_NAME = 1;
  OCI_OTYPE_REF = 2;
  OCI_OTYPE_PTR = 3;

  { Object type }
  OCI_PTYPE_UNK           = 0;    // unknown
  OCI_PTYPE_TABLE         = 1;    // table
  OCI_PTYPE_VIEW          = 2;    // view
  OCI_PTYPE_PROC          = 3;    // procedure
  OCI_PTYPE_FUNC          = 4;    // function
  OCI_PTYPE_PKG           = 5;    // package
  OCI_PTYPE_TYPE          = 6;    // user-defined type
  OCI_PTYPE_SYN           = 7;    // synonym
  OCI_PTYPE_SEQ           = 8;    // sequence
  OCI_PTYPE_COL           = 9;    // column
  OCI_PTYPE_ARG           = 10;   // argument
  OCI_PTYPE_LIST          = 11;   // list
  OCI_PTYPE_TYPE_ATTR     = 12;   // user-defined type's attribute
  OCI_PTYPE_TYPE_COLL     = 13;   // collection type's element
  OCI_PTYPE_TYPE_METHOD   = 14;   // user-defined type's method
  OCI_PTYPE_TYPE_ARG      = 15;   // user-defined type method's argument
  OCI_PTYPE_TYPE_RESULT   = 16;   // user-defined type method's result

  { Proc/Func param type }
  OCI_TYPEPARAM_IN    = 0;
  OCI_TYPEPARAM_OUT   = 1;
  OCI_TYPEPARAM_INOUT = 2;

  { NLS environmet }
  OCI_NLS_CHARSET_MAXBYTESZ = 91;

  { enum OCIPinOpt }
  OCI_PIN_DEFAULT = 1;            //* default pin option */
  OCI_PIN_ANY     = 3;            //* pin any copy of the object */
  OCI_PIN_RECENT  = 4;            //* pin recent copy of the object */
  OCI_PIN_LATEST  = 5;            //* pin latest copy of the object */

  { enum OCILockOpt }
  OCI_LOCK_NONE     = 1;          //* null (same as no lock) */
  OCI_LOCK_X        = 2;          //* exclusive lock */
  OCI_LOCK_X_NOWAIT = 3;          //* exclusive lock, do not wait  */

  { OBJECT FREE OPTION }
  OCI_OBJECTFREE_FORCE =1;
  OCI_OBJECTFREE_NONULL=2;

type
  PPointer = ^Pointer;

  TOCIEnvInit = function(var envhpp: POCIEnv; mode: ub4; xtramemsz: size_T;
    usrmempp: PPointer): sword; cdecl;

  TOCIEnvCreate = function(var envhpp: POCIEnv; mode: ub4; ctxp: Pointer;
    malocfp: Pointer; ralocfp: Pointer; mfreefp: Pointer; xtramemsz: size_T;
    usrmempp: PPointer): sword; cdecl;

  TOCIDefineArrayOfStruct = function(defnpp: POCIDefine; errhp: POCIError;
    pvskip: ub4; indskip: ub4; rlskip: ub4; rcskip: ub4): sword; cdecl;

  TOCIBindByName = function(stmtp: POCIStmt; var bindpp: POCIBind;
    errhp: POCIError; placeholder: text; placeh_len: sb4; valuep: Pointer;
    value_sz: sb4; dty: ub2; indp: Pointer; alenp: Pointer; rcodep: Pointer;
    maxarr_len: ub4; curelep: Pointer; mode: ub4): sword; cdecl;

  TOCIBindDynamic = function(bindp: POCIBind; errhp: POCIError; ictxp: Pointer;
    icbfp: Pointer; octxp: Pointer; ocbfp: Pointer): sword; cdecl;

  TOCITransDetach = function(svchp: POCISvcCtx; errhp: POCIError;
    flags: ub4) :sword; cdecl;

  TOCITransPrepare = function(svchp: POCISvcCtx; errhp: POCIError;
    flags: ub4) :sword; cdecl;

  TOCITransForget = function(svchp: POCISvcCtx; errhp: POCIError;
    flags: ub4) :sword; cdecl;

  TOCIBreak = function(svchp: POCISvcCtx; errhp:POCIError): sword; cdecl;

  TOCIReset = function(svchp: POCISvcCtx; errhp:POCIError): sword; cdecl;

  TOCIDateTimeAssign = function(hndl: POCIEnv; err: POCIError;
    const from: POCIDateTime;_to: POCIDateTime): sword; cdecl;

  TOCIDateTimeCheck = function(hndl: POCIEnv; err: POCIError;
    const date: POCIDateTime; var valid: ub4): sword; cdecl;

  TOCIDateTimeCompare = function(hndl: POCIEnv; err: POCIError;
    const date1: POCIDateTime; const date2: POCIDateTime;
    var result: sword): sword; cdecl;

  TOCIDateTimeConvert = function(hndl: POCIEnv; err: POCIError;
    indate: POCIDateTime; outdate: POCIDateTime): sword; cdecl;

  TOCIDateTimeFromText= function(hndl: POCIEnv; err: POCIError;
    const date_str: text; d_str_length: size_t; const fmt: text;
    fmt_length: ub1; const lang_name: text; lang_length: size_t;
    date: POCIDateTime): sword; cdecl;

  TOCIDateTimeGetTimeZoneOffset = function(hndl: POCIEnv; err: POCIError;
    const datetime: POCIDateTime; var hour: sb1; var minute: sb1): sword; cdecl;

  TOCIDateTimeSysTimeStamp = function(hndl: POCIEnv; err: POCIError;
    sys_date: POCIDateTime): sword; cdecl;

  TOCIDateTimeToText = function(hndl: POCIEnv; err: POCIError;
    const date: POCIDateTime; const fmt: text; fmt_length: ub1;
    fsprec: ub1; const lang_name: text; lang_length: size_t;
    var buf_size: ub4; buf: text): sword; cdecl;

  TOCIDateTimeGetTimeZoneName = function(hndl: POCIEnv; err: POCIError;
    datetime: POCIDateTime; var buf: ub1; var buflen: ub4): sword; cdecl;

  TOCILobAppend = function(svchp: POCISvcCtx; errhp: POCIError; dst_locp,
    src_locp: POCILobLocator): sword; cdecl;

  TOCILobAssign = function(svchp: POCISvcCtx; errhp: POCIError;
    src_locp: POCILobLocator; var dst_locpp: POCILobLocator): sword; cdecl;

  TOCILobCopy = function(svchp: POCISvcCtx; errhp: POCIError;
    dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: ub4;
    dst_offset: ub4; src_offset: ub4): sword; cdecl;

  TOCILobEnableBuffering = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator): sword; cdecl;

  TOCILobDisableBuffering = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator): sword; cdecl;

  TOCILobErase = function(svchp: POCISvcCtx; errhp: POCIError; locp: POCILobLocator;
    var amount: ub4; offset: ub4): sword; cdecl;

  TOCILobFileExists = function(svchp: POCISvcCtx; errhp: POCIError;
    filep: POCILobLocator; var flag: Boolean): sword; cdecl;

  TOCILobFileGetName = function(envhp: POCIEnv; errhp: POCIError;
    filep: POCILobLocator; dir_alias: text; var d_length: ub2; filename: text;
    var f_length: ub2): sword; cdecl;

  TOCILobFileSetName = function(envhp: POCIEnv; errhp: POCIError;
    var filep: POCILobLocator; dir_alias: text; d_length: ub2; filename: text;
    f_length: ub2): sword; cdecl;

  TOCILobFlushBuffer = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; flag: ub4): sword; cdecl;

  TOCILobGetLength = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; var lenp: ub4): sword; cdecl;

  TOCILobIsOpen = function(svchp: POCISvcCtx; errhp: POCIError;
    locp: POCILobLocator; var flag: LongBool): sword; cdecl;

  TOCILobLoadFromFile = function(svchp: POCISvcCtx; errhp: POCIError;
    dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: ub4;
    dst_offset: ub4; src_offset: ub4): sword; cdecl;

  TOCILobLocatorIsInit = function(envhp: POCIEnv; errhp: POCIError;
   locp: POCILobLocator; var is_initialized: LongBool): sword; cdecl;

  TOCIStmtGetPieceInfo = function(stmtp: POCIStmt; errhp: POCIError;
    var hndlpp: Pointer; var typep: ub4; var in_outp: ub1; var iterp: ub4;
    var idxp: ub4; var piecep: ub1): sword; cdecl;

  TOCIStmtSetPieceInfo = function(handle: Pointer; typep: ub4; errhp: POCIError;
    buf: Pointer; var alenp: ub4; piece: ub1; indp: Pointer;
    var rcodep: ub2): sword; cdecl;

(*****************************************************************************
 *                         NUMBER/FLOAT/DECIMAL TYPE                         *
 *****************************************************************************)
const
  OCI_NUMBER_SIZE = 22;

type
  POCINumberPart = ^TOCINumberPart;
  TOCINumberPart = array[1..OCI_NUMBER_SIZE] of ub1;

(*
 * OCINumber - OCI Number mapping in c
 *
 * The OTS types: NUMBER, NUMERIC, INT, SHORTINT, REAL, DOUBLE PRECISION,
 * FLOAT and DECIMAL are represented by OCINumber.
 * The contents of OCINumber is opaque to clients.
 *
 * For binding variables of type OCINumber in OCI calls (OCIBindByName(),
 * OCIBindByPos(), and OCIDefineByPos()) use the type code SQLT_VNU.
 *

 *
   EXAMPLE

   The following example shows how to manipulate an attribute of type
   oracle number.

     struct person
     {
       OCINumber sal;
     };
     typedef struct person person;

     OCIError *err;
     person* joe;
     person* tom;
     person* debbie;
     OCINumber  *joesal;
     OCINumber  *tomsal;
     OCINumber *debsal;
     sword   status;
     int     inum;
     double  dnum;
     OCINumber ornum;
     char    buffer[21];
     ub4     buflen;
     sword   result;

     /o See oci.h for an example of how to initialize OCIError.
      o For this example, assume the OCIEnv and OCIError has been
      o initialized.
      o/

     /o Pin joe, tom and debbie person objects in the object cache. See ori.h
      o for an example on pinning objects. For this example, assume that
      o joe, tom and debbie are pointing to pinned objects.
      o/
     joesal = &joe->sal;
     tomsal = &tom->sal;
     debsal = &debbie->sal;

     /o initialize joe's salary to be $12,000 o/
     inum = 12000;
     status = OCINumberFromInt(err, &inum, sizeof(inum), OCI_NUMBER_SIGNED,
                               joesal);
     if (status != OCI_SUCCESS)
                              /o goto to handle error from OCINumberFromInt o/;

     /o initialize tom's salary to be same as joe o/
     OCINumberAssign(err, joesal, tomsal);

     /o initialize debbie's salary to be 20% more than joe's o/
     dnum = 1.2;
     status = OCINumberFromReal(err, &dnum, sizeof(double), &ornum);
     if (status != OCI_SUCCESS)
                            /o goto to handle error from OCINumberFromReal o/;
     status = OCINumberMul(err, joesal, &ornum, debsal);
     if (status != OCI_SUCCESS)  /o goto to handle error from OCINumberMul o/;

     /o give tom a 50% raise o/
     dnum = 1.5;
     status = OCINumberFromReal(err, &dnum, sizeof(double), &ornum);
     if (status != OCI_SUCCESS)
                            /o goto to handle error from OCINumberFromReal o/;
     status = OCINumberMul(err, tomsal, &ornum, tomsal);
     if (status != OCI_SUCCESS)  /o goto to handle error from OCINumberMul o/;

     /o double joe's salary o/
     status = OCINumberAdd(err, joesal, joesal, joesal);
     if (status != OCI_SUCCESS)  /o goto to handle error from OCINumberAdd o/;

     /o get joe's salary in integer o/
     status = OCINumberToInt(err, joesal, sizeof(inum), OCI_NUMBER_SIGNED,
                             &inum);
     if (status != OCI_SUCCESS)/o goto to handle error from OCINumberToInt o/;
     /o inum is set to 24000 o/

     /o get debbie's salary in double o/
     status = OCINumberToReal(err, debsal, sizeof(dnum), &dnum);
     if (status != OCI_SUCCESS)/o goto to handle error from OCINumberToReal o/;
     /o dnum is set to 14400 o/

     /o print tom's salary as DEM0001`8000.00 o/
     buflen = sizeof(buffer);
     status = OCINumberToText(err, tomsal, "C0999G9999D99", 13,
                 "NLS_NUMERIC_CHARACTERS='.`' NLS_ISO_CURRENCY='Germany'", 54,
                 &buflen, buffer);
     if (status != OCI_SUCCESS)/o goto to handle error from OCINumberToText o/;
     printf("tom's salary = %s\n", buffer);

     /o compare joe and tom's salary o/
     status = OCINumberCmp(err, joesal, tomsal, &result);
     if (status != OCI_SUCCESS)  /o goto to handle error from OCINumberCmp o/;
     /o result is positive o/

     /o read debbie's new salary from string o/
     status = OCINumberFromText(err, "48`000.00", 9, "99G999D99", 9,
                      "NLS_NUMERIC_CHARACTERS='.`'", 27, debsal);
     if (status != OCI_SUCCESS)
                            /o goto to handle error from OCINumberFromText o/;
     /o debbie's salary is now 48000.00 o/

*)

(*----------------------------- OCINumberInc --------------------------------*)

  TOCINumberInc = function(err: POCIError; number: POCINumber): sword; cdecl;
(*
   NAME: OCINumberInc - OCINumber INCrement numbers
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN/OUT) a positive Oracle number to be incremented
   DESCRIPTION:
        Increment Oracle number in place. It is assumed that the input is
        an integer between 0 and 100^21-2. If the is input too large, it will
        be treated as 0 - the result will be an Oracle number 1. If the input
        is not a positive integer, the result will be unpredictable.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*----------------------------- OCINumberDec --------------------------------*)

  TOCINumberDec = function(err: POCIError; number: POCINumber): sword; cdecl;
(*
   NAME: OCINumberDec - OCINumber DECrement numbers
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN/OUT) - a positive Oracle number to be decremented
   DESCRIPTION:
        Decrement Oracle number in place. It is assumed that the input is an
        integer between 1 and 100^21-2. If the input is too large, it will be
        treated as 1 - the result will be an Oracle number 0. If the input is
        not a positive integer, the result will be unpredictable.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*-------------------------- OCINumberSetZero -------------------------------*)

  TOCINumberSetZero = procedure(err: POCIError; number: POCINumber); cdecl;
(*
   NAME: OCINumberSetZero - OCINumber Set number to Zero value
   PARAMETERS:
        err (IN/OUT) - pointer to OCI error handle
        num (OUT) - set to zero value
   DESCRIPTION:
        Initialize the given number to value 0.
 *)

(*--------------------------- OCINumberSetPi --------------------------------*)

  TOCINumberSetPi = procedure(err: POCIError; number: POCINumber); cdecl;
(*
   NAME: OCINumberSetPi - OCINumber Set number to Pi
        err (IN/OUT) - pointer to OCI error handle
        num (OUT) - set to zero value
   DESCRIPTION:
        Initialize the given number to value Pi.
 *)

(*----------------------------- OCINumberAdd --------------------------------*)

  TOCINumberAdd = function(err: POCIError; const number1: POCINumber;
                const number2: POCINumber; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberAdd - OCINumber ADD numbers
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number1, number2 (IN) - numbers to be added
        result (OUT) - result of adding 'number1' with 'number2'
   DESCRIPTION:
        Add 'number1' with 'number2' and return result in 'result'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*----------------------------- OCINumberSub --------------------------------*)

  TOCINumberSub = function(err: POCIError; const number1: POCINumber;
                const number2: POCINumber; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberSub - OCINumber SUBtract numbers
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number1, number2 (IN) - 'number2' subtracted from 'number1'
        result (OUT) - subtraction result
   DESCRIPTION:
        Subtract 'number2' from 'number1' and return result in 'result'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*----------------------------- OCINumberMul --------------------------------*)

  TOCINumberMul = function(err: POCIError; const number1: POCINumber;
                const number2: POCINumber; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberMul - OCINumber MULtiply numbers
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number1, number2 (IN) - numbers to be multiplied
        result (OUT) - multiplication result
   DESCRIPTION:
        Multiply 'number1' with 'number2' and return result in 'result'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*----------------------------- OCINumberDiv --------------------------------*)

  TOCINumberDiv = function(err: POCIError; const number1: POCINumber;
                const number2: POCINumber; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberDiv - OCINumber DIVide numbers
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number1 (IN) - pointer to the numerator
        number2 (IN) - pointer to the denominator
        result (OUT) - division result
   DESCRIPTION:
        Divide 'number1' by 'number2' and return result in 'result'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
          underflow errorr
          overflow errorr
          divide by zero errorr
 *)

(*----------------------------- OCINumberMod --------------------------------*)

  TOCINumberMod = function(err: POCIError; const number1: POCINumber;
                const number2: POCINumber; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberMod - OCINumber MODulous
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number1 (IN) - pointer to the numerator
        number2 (IN) - pointer to the denominator
        result (OUT) - remainder of the result
   DESCRIPTION:
        Finds the remainder of the division of two Oracle numbers.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
          divide by zero errorr
 *)

(*------------------------ OCINumberIntPower --------------------------------*)

  TOCINumberIntPower = function(err: POCIError; const number1: POCINumber;
                const number2: POCINumber; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberIntPower - OCINumber takes an arbitary base to an arbitary
                             integer PoWeR
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        base (IN) - base of the exponentiation
        exp (IN) - exponent to which the base is to be raised
        result (OUT) - output of exponentiation
   DESCRIPTION:
        Takes an arbitary base to an arbitary integer power.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*-------------------------- OCINumberShift ---------------------------------*)

  TOCINumberShift = function(err: POCIError; const number: POCINumber;
                            const nDig: sword; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberShift - OCINumber multiplies by a power of 10.

   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - Oracle Number to be shifted.
        nDig   (IN) - number of decimal places to shift.
        result (OUT) - shift result.
   DESCRIPTION:
        Multiplies number by 10^NDig and sets product to the result.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*----------------------------- OCINumberNeg --------------------------------*)

  TOCINumberNeg = function(err: POCIError; const number: POCINumber;
                            _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberNeg - OCINumber NEGate number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - number to be negated
        result (OUT) - will contain negated value of 'number'
   DESCRIPTION:
        Negates an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*------------------------- OCINumberToText ---------------------------------*)
  Poratext = PAnsiChar;
  PPoratext = PPAnsiChar;

  TOCINumberToText = function(err: POCIError; const number: POCINumber;
                          const fmt: Poratext; fmt_length: ub4;
                          const nls_params: Poratext; nls_p_length: ub4;
                          buf_size: pub4; buf: poratext): sword; cdecl;
(*
   NAME: OCINumberToText - OCINumber convert number TO String
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - Oracle number to be converted
        fmt (IN) - conversion format
        fmt_length (IN) - length of the 'fmt' parameter
        nls_params (IN) - nls format specification, if null string
                i.e. (oratext * )0, then the default parameters for the
                session is used
        nls_p_length (IN) - length of the 'nls_params' parameter
        buf_size (IN/OUT) - size of the buffer must be passed as input by
                the caller, this function will return the length of the
                resulting string in bytes via this parameter. The length
                does not include the terminating null ('\0').
        buf (OUT) - buffer into which the converted string is placed. The
                resulting string is null terminated.
   DESCRIPTION:
        Converts the given number to a character string
        according to the specified format. Refer to "TO_NUMBER" conversion
        function described in "Oracle SQL Language Reference Manual" for a
        description of format and NLS parameters.
        The converted number string is stored in the buffer 'buf', up to
        a max of '*buf_size' bytes. Length of the resulting string is
        returned via 'buf_size'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          'number' or 'buf' is null
          buffer too small
          invalid format
          invalid nls format
          number to text translation for the given format causes overflow
 *)

(*-------------------------- OCINumberFromText ------------------------------*)

  TOCINumberFromText = function(err: POCIError; const str: poratext;
                          str_length: ub4; const fmt: poratext; fmt_length: ub4;
                          const nls_params: poratext; nls_p_length: ub4;
                          number: POCINumber): sword; cdecl;
(*
   NAME: OCINumberFromText - OCINumber convert String TO Number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        str (IN) - input string to be converted to Oracle number
        str_length (IN) - size of the input string
        fmt (IN) - conversion format
        fmt_length (IN) - length of the 'fmt' parameter
        nls_params (IN) - nls format specification, if null string
                i.e. (oratext * )0, then the default parameters for the
                session is used
        nls_p_length (IN) - length of the 'nls_params' parameter
        number (OUT) - given string converted to number
   DESCRIPTION:
        Converts the given string to a number
        according to the specified format. Refer to "TO_NUMBER" conversion
        function described in "Oracle SQL Language Reference Manual" for a
        description of format and NLS parameters.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          'number' or 'str' is null
          'str_length' is 0
          invalid format
          invalid nls format
          invalid input string
 *)

(*-------------------------- OCINumberToInt ---------------------------------*)

const
  OCI_NUMBER_UNSIGNED = 0;                        // Unsigned type -- ubX
  OCI_NUMBER_SIGNED = 2;                          // Signed type -- sbX

type
  TOCINumberToInt = function(err: POCIError; const number: POCINumber;
                rsl_length: uword; rsl_flag: uword; rsl: Pointer): sword; cdecl;
(*
   NAME: OCINumberToInt - OCINumber convert number TO Integer
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - number to be converted
        rsl_length (IN) - size of the desired result
        rsl_s_flag (IN) - flag denoting the desired sign of the output; valid
                values are OCI_NUMBER_UNSIGNED, OCI_NUMBER_SIGNED
        rsl (OUT) - pointer to space for the result
   DESCRIPTION:
        Native type conversion function.
        Converts the given Oracle number into an xbx (e.g. ub2, ub4, sb2 etc.)
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          'number' or 'rsl' is null
          integer value of 'number' is too big -- overflow
          integer value of 'number' is too small -- underflow
          invalid sign flag value ('rsl_s_flag')
 *)

(*--------------------------- OCINumberFromInt ------------------------------*)

  TOCINumberFromInt = function(err: POCIError; const inum: Pointer;
    inum_length: uword; inum_s_flag: uword; number: POCINumber): sword; cdecl;
(*
   NAME: OCINumberFromInt - OCINumber convert Integer TO Number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        inum (IN) - pointer to the integer to be converted
        inum_length (IN) - size of the integer
        inum_s_flag (IN) - flag that designates the sign of the integer; valid
                values are OCI_NUMBER_UNSIGNED, OCI_NUMBER_SIGNED
        number (OUT) - given integer converted to Oracle number
   DESCRIPTION:
        Native type conversion function. Converts any Oracle standard
        machine-native integer type (xbx) to an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          'number' or 'inum' is null
          integer too BIG -- the number is too large to fit into an Oracle
                number
          invalid sign flag value ('inum_s_flag')
 *)

(*------------------------- OCINumberToReal ---------------------------------*)

  TOCINumberToReal = function(err: POCIError; const number: POCINumber;
                        rsl_length: uword; rsl: Pointer): sword; cdecl;
(*
   NAME: OCINumberToReal - OCINumber convert number TO Real
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - number to be converted
        rsl_length (IN) - is the size of the desired result,
                sizeof( float | double | long double)
        rsl (OUT) - pointer to space for storing the result
   DESCRIPTION:
        Native type conversion function. Converts an Oracle number into a
        machine-native real type. This function only converts numbers up to
        LDBL_DIG, DBL_DIG, or FLT_DIG digits of precision and removes
        trailing zeroes. The above constants are defined in float.h
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          'number' or 'rsl' is null
          'rsl_length' is 0
 *)

(*------------------------- OCINumberToRealArray ----------------------------*)

  TOCINumberToRealArray = function(err: POCIError; const number: PPOCINumber;
                             elems: uword; rsl_length: uword; rsl: Pointer): sword; cdecl;
(*
   NAME: OCINumberToRealArray - OCINumber convert array of numbers TO Real
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - Pointer to array of number to be converted
        elems (IN) - Upper bound of number array
        rsl_length (IN) - is the size of the desired result,
                                          sizeof( float | double | long double)
        rsl (OUT) - pointer to array of space for storing the result
   DESCRIPTION:
        Native type conversion function. Converts an Oracle number into a
        machine-native real type. This function only converts numbers up to
        LDBL_DIG, DBL_DIG, or FLT_DIG digits of precision and removes
        trailing zeroes. The above constants are defined in float.h
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          'number' or 'rsl' is null
          'rsl_length' is 0
 *)

(*-------------------------- OCINumberFromReal ------------------------------*)

  TOCINumberFromReal = function(err: POCIError; const rnum: Pointer;
                            rnum_length: uword; number: POCINumber): sword; cdecl;
(*
   NAME: OCINumberFromReal - OCINumber convert Real TO Number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        rnum (IN) - pointer to the floating point number to be converted
        rnum_length (IN) - size of the desired result, i.e.
                sizeof({float | double | long double})
        number (OUT) - given float converted to Oracle number
   DESCRIPTION:
        Native type conversion function. Converts a machine-native floating
        point type to an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          'number' or 'rnum' is null
          'rnum_length' is 0
 *)

(*----------------------------- OCINumberCmp --------------------------------*)

  TOCINumberCmp = function(err: POCIError; const number1: POCINumber;
                       const number2: POCINumber; _result: psword): sword; cdecl;
(*
   NAME: OCINumberCmp - OCINumber CoMPare numbers
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number1, number2 (IN) - numbers to be compared
        result (OUT) - 0 if equal, negative if number1 < number2,
                positive if number1 > number2
   DESCRIPTION:
        The function OCINumberCmp compares two numbers.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
            'number1' or 'number2' or 'result' is null
 *)

(*---------------------------- OCINumberSign --------------------------------*)

  TOCINumberSign = function(err: POCIError; const number: POCINumber;
                        _result: psword): sword; cdecl;
(*
   NAME: OCINumberSign - OCINumber obtains SiGN of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - number whose sign is returned
        result (OUT) - 0 if number == 0, -1 if number < 0,
                1 if number > 0
   DESCRIPTION:
        Obtains sign of an Oracle number
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
            'number' or 'result' is null
 *)

(*---------------------------- OCINumberIsZero ------------------------------*)

  TOCINumberIsZero = function(err: POCIError; const number: POCINumber;
                          _Result: pboolean): sword; cdecl;
(*
   NAME: OCINumberIsZero - OCINumber comparison with ZERo
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - numbers to be compared
        result (OUT) - set to TRUE if equal to zero else FALSE
   DESCRIPTION:
        Test if the given number is equal to zero.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
            'number' or 'result' is null
 *)

(*---------------------------- OCINumberIsInt -------------------------------*)

  TOCINumberIsInt = function(err: POCIError; const number: POCINumber;
                          _result: Pboolean): sword; cdecl;
(*
   NAME: OCINumberIsInt - OCINumber Is Integer value.
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - number to be tested
        result (OUT) - set to TRUE if integer value else FALSE
   DESCRIPTION:
        Test if the given number is an integer value.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
            'number' or 'result' is null
 *)

(*-------------------------- OCINumberAssign --------------------------------*)

  TOCINumberAssign = function(err: POCIError; const from: POCINumber;
                          _to: POCINumber): sword; cdecl;
(*
   NAME: OCINumberAssign - OCINumber ASsiGn number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        from (IN) - number to be assigned
        to (OUT) - number copied into
   DESCRIPTION:
        Assign number 'from' to 'to'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          'from' or 'to' is null
 *)

(*----------------------------- OCINumberAbs --------------------------------*)

  TOCINumberAbs = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberAbs - OCINumber compute ABSolute value
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - input number
        result (OUT) - output which will contain the absolue value of the
                input number
   DESCRIPTION:
        Computes the absolute value of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*---------------------------- OCINumberCeil --------------------------------*)

  TOCINumberCeil = function(err: POCIError; const number: POCINumber;
                        _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberCeil - OCINumber compute the CEiL value of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - input number
        result (OUT) - output which will contain the ceil value of the
                input number
   DESCRIPTION:
        Computes the ceil value of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*--------------------------- OCINumberFloor --------------------------------*)

  TOCINumberFloor = function(err: POCIError; const number: POCINumber;
                        _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberFloor - OCINumber compute the FLooR value of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - input number
        result (OUT) - output which will contain the floor value of the
                input number
   DESCRIPTION:
        Computes the floor value of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*----------------------------- OCINumberSqrt -------------------------------*)

  TOCINumberSqrt = function(err: POCIError; const number: POCINumber;
                        _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberSqrt - OCINumber compute the SQuare Root of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - input number
        result (OUT) - output which will contain the square root of the
                input number
   DESCRIPTION:
        Computes the square root of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
          'number' is negative
 *)

(*--------------------------- OCINumberTrunc --------------------------------*)

  TOCINumberTrunc = function(err: POCIError; const number: POCINumber;
                        _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberTrunc - OCINumber TRUncate an Oracle number at a
                          specified decimal place
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - input number
        decplace (IN) - number of decimal digits to the right of the
                decimal point to truncate at. Negative values are allowed.
        result (OUT) - output of truncation
   DESCRIPTION:
        Truncate an Oracle number at a specified decimal place
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*----------------------------- OCINumberPower ------------------------------*)

  TOCINumberPower = function(err: POCIError; const base: POCINumber;
                         const number: POCINumber; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberPower - OCINumber takes an arbitary Base to an
                          arbitary Power
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        base (IN) - base of the exponentiation
        number (IN) - exponent to which the base is to be raised
        result (OUT) - output of exponentiation
   DESCRIPTION:
        Takes an arbitary base to an arbitary power.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*--------------------------- OCINumberRound --------------------------------*)

  TOCINumberRound = function(err: POCIError; const number: POCINumber;
                         decplace: sword; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberRound - OCINumber ROUnds an Oracle number to a specified
                  decimal place
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - round this number and return result in 'result'
        decplace (IN) - number of decimal digits to the right of the
                decimal point to round to. Negative values are allowed.
        result (OUT) - output of rounding
   DESCRIPTION:
        Rounds an Oracle number to a specified decimal place
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*--------------------------- OCINumberPrec ---------------------------------*)

  TOCINumberPrec = function(err: POCIError; const number: POCINumber;
                         nDigs: sword; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberPrec - Rounds an Oracle number to a specified number of
                         decimal digits.
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - number for which to set precision.
        nDig   (IN) - number of decimal digits desired in the result.
        result (OUT) - result.
   DESCRIPTION:
        Performs a floating point round with respect to the number
        of digits.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*----------------------------- OCINumberSin --------------------------------*)

  TOCINumberSin = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberSin - OCINumber takes the SINe of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - argument of the sine in radians
        result (OUT) - result of the sine
   DESCRIPTION:
        Takes the sine in radians of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*-------------------------- OCINumberArcSin --------------------------------*)

  TOCINumberArcSin = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberArcSin - OCINumber takes the Arc SINe of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - argument of the arc sine
        result (OUT) - result of the arc sine in radians
   DESCRIPTION:
        Takes the arc sine in radians of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
          'number' is < -1 or 'number' is > 1.
 *)

(*-------------------------- OCINumberHypSin --------------------------------*)

  TOCINumberHypSin = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberHypSin - OCINumber takes the SiNe Hyperbolic of an
                           Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - argument of the sine hyperbolic
        result (OUT) - result of the sine hyperbolic
   DESCRIPTION:
        Takes the hyperbolic sine of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
   NOTES:
        An Oracle number overflow causes an unpredictable result value.
 *)

(*----------------------------- OCINumberCos --------------------------------*)

  TOCINumberCos = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberCos - OCINumber takes the COSine of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - argument of the cosine in radians
        result (OUT) - result of the cosine
   DESCRIPTION:
        Takes the cosine in radians of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*-------------------------- OCINumberArcCos --------------------------------*)

  TOCINumberArcCos = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberArcCos - OCINumber takes the Arc COSine of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - argument of the arc cosine
        result (OUT) - result of the arc cosine in radians
   DESCRIPTION:
        Takes the arc cosine in radians of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
          'number' is < -1 or 'number' is > 1.
 *)

(*-------------------------- OCINumberHypCos --------------------------------*)

  TOCINumberHypCos = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberHypCos - OCINumber takes the CoSine Hyperbolic of an
                           Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - argument of the cosine hyperbolic
        result (OUT) - result of the cosine hyperbolic
   DESCRIPTION:
        Takes the hyperbolic cosine of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
   NOTES:
        An Oracle number overflow causes an unpredictable result value.
 *)

(*----------------------------- OCINumberTan --------------------------------*)

  TOCINumberTan = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberTan - OCINumber takes the TANgent of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - argument of the tangent in radians
        result (OUT) - result of the tangent
   DESCRIPTION:
        Takes the tangent in radians of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*-------------------------- OCINumberArcTan --------------------------------*)

  TOCINumberArcTan = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberArcTan - OCINumber takes the Arc TANgent of an Oracle number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - argument of the arc tangent
        result (OUT) - result of the arc tangent in radians
   DESCRIPTION:
        Takes the arc tangent in radians of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*------------------------ OCINumberArcTan2 ---------------------------------*)

  TOCINumberArcTan2 = function(err: POCIError; const number1: POCINumber;
                  const number2: POCINumber; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberArcTan2 - OCINumber takes the ATan2 of 2 Oracle numbers
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number1 (IN) - first argument of atan2(y,x) function which
                corresponds to 'y' parameter in the function
        number2 (IN) - second argument of atan2(y,x) function which
                corresponds to 'x' parameter in the function
        result (OUT) - result of the atan2() in radians
   DESCRIPTION:
        Takes the atan2(number1, number2).
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
          'number2' is 0
 *)

(*----------------------------- OCINumberHypTan -----------------------------*)

  TOCINumberHypTan = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberHypTan - OCINumber takes the TaNgent Hyperbolic of an Oracle
                           number
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - argument of the tangent hyperbolic
        result (OUT) - result of the tangent hyperbolic
   DESCRIPTION:
        Takes the hyperbolic tangent of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
   NOTES:
        An Oracle number overflow causes an unpredictable result value.
 *)

(*--------------------------- OCINumberExp ----------------------------------*)

  TOCINumberExp = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberExp - OCINumber EXPonential
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - e raised to this Oracle number power
        result (OUT) - output of exponentiation
   DESCRIPTION:
        Raises e to the specified Oracle number power
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
 *)

(*----------------------------- OCINumberLn ---------------------------------*)

  TOCINumberLn = function(err: POCIError; const number: POCINumber;
                       _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberLn - OCINumber Logarithm Natural
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        number (IN) - logarithm of this number is computed
        result (OUT) - logarithm result
   DESCRIPTION:
        Takes the logarithm of the given Oracle number with respect
        to the given base.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
          'number' is <= 0
 *)

(*----------------------------- OCINumberLog --------------------------------*)

  TOCINumberLog = function(err: POCIError; const base: POCINumber;
                       const number: POCINumber; _result: POCINumber): sword; cdecl;
(*
   NAME: OCINumberLog - OCINumber LOGarithm any base
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        base (IN) - base of the logarithm
        number (IN) - opearnd
        result (OUT) - logarithm result
   DESCRIPTION:
        Takes the logarithm with the specified base of an Oracle number.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          any of the number arguments is null
          'number' is <= 0
          'base' is <= 0
 *)

(*****************************************************************************
 *                             ORACLE DATE TYPE                              *
 *****************************************************************************)

  POCITime = ^TOCITime;
  TOCITime = record
    OCITimeHH: ub1;                     // hours; range is 0 <= hours <=23
    OCITimeMI: ub1;                     // minutes; range is 0 <= minutes <= 59
    OCITimeSS: ub1;                     // seconds; range is 0 <= seconds <= 59
  end;

(*
 * OCITime - OCI TiMe portion of date
 *
 * This structure should be treated as an opaque structure as the format
 * of this structure may change. Use OCIDateGetTime/OCIDateSetTime
 * to manipulate time portion of OCIDate.
 *)

  POCIDate = ^TOCIDate;
  TOCIDate = record
    OCIDateYYYY: sb2;         // gregorian year; range is -4712 <= year <= 9999
    OCIDateMM: ub1;           // month; range is 1 <= month < 12
    OCIDateDD: ub1;           // day; range is 1 <= day <= 31
    OCIDateTime: TOCITime;    // time
  end;

(*
 * OCIDate - OCI oracle Date representation in C
 *
 * OCIDate represents the C mapping of Oracle date.
 *
 * This structure should be treated as an opaque structure as the format
 * of this structure may change. Use OCIDateGetDate/OCIDateSetDate
 * to access/initialize OCIDate.
 *
 * For binding variables of type OCIDate in OCI calls (OCIBindByName(),
 * OCIBindByPos(), and OCIDefineByPos()) use the type code SQLT_ODT.
 *)

(*
   EXAMPLE

   The following example shows how to manipulate an attribute of type
   oracle date.

     #define FMT "Month dd, YYYY, HH:MI A.M."
     #define LANG "American"

     struct person
     {
       OCIDate start_date;
     };
     typedef struct person person;

     OCIError *err;
     person *joe;
     sword status;                                           /o error status o/

     /o See oci.h for an example of how to initialize OCIError.
      o For this example, assume the OCIEnv and OCIError has been
      o initialized.
      o/

     /o Pin joe person object in the object cache. See ori.h
      o for an example on pinning objects. For this example, assume that
      o joe is pointing to the pinned object.
      o/

     /o set the start date of joe o/
     OCIDateSetTime(&joe->start_date, 8, 0, 0);
     OCIDateSetDate(&joe->start_date, 1990, 10, 5);

     /o check if the date is valid o/
     uword invalid;
     if (OCIDateCheck(err, &joe->start_date, &invalid) != OCI_SUCCESS)
       /o error handling code o/
     if (invalid)
       /o error handling code o/

     /o convert date for display purposes o/
     char str[100];
     ub4 strlen = sizeof(str);
     if (OCIDateToText(err, &joe->start_date, FMT, sizeof(FMT)-1, LANG,
                sizeof(LANG)-1, &strlen, str) != OCI_SUCCESS)
       /o error handling code o/

 *)

(*--------------------------- OCIDateGetTime --------------------------------*/
/* void OCIDateGetTime(/o_ const OCIDate *date, ub1 *hour, ub1 *min,
                           ub1 *sec _o/); */
#define OCIDateGetTime(date, hour, min, sec) \
  { \
     *hour = (date)->OCIDateTime.OCITimeHH; \
     *min = (date)->OCIDateTime.OCITimeMI; \
     *sec = (date)->OCIDateTime.OCITimeSS; \
  }
/*
   NAME: OCIDateGetTime - OCIDate Get Time portion of date
   PARAMETERS:
        date (IN) - Oracle date whose time data is retrieved
        hour (OUT) - hour value returned
        min (OUT) - minute value returned
        sec (OUT) - second value returned
   DESCRIPTION:
        Return time inforamtion stored in the given date. The time
        information returned is: hour, minute and seconds.
   RETURNS:
        NONE
 */

/*--------------------------- OCIDateGetDate --------------------------------*/
/* void OCIDateGetDate(/o_ const OCIDate *date, sb2 *year, ub1 *month,
                           ub1 *day _o/); */
#define OCIDateGetDate(date, year, month, day) \
  { \
     *year = (date)->OCIDateYYYY; \
     *month = (date)->OCIDateMM; \
     *day = (date)->OCIDateDD; \
  }
/*
   NAME: OCIDateGetDate - OCIDate Get Date (year, month, day) portion of date
   PARAMETERS:
        date (IN) - Oracle date whose year, month, day data is retrieved
        year (OUT) - year value returned
        month (OUT) - month value returned
        day (OUT) - day value returned
   DESCRIPTION:
        Return year, month, day inforamtion stored in the given date.
   RETURNS:
        NONE
 */

/*--------------------------- OCIDateSetTime --------------------------------*/
/* void OCIDateSetTime(/o_ OCIDate *date, ub1 hour, ub1 min,
                           ub1 sec _o/); */
#define OCIDateSetTime(date, hour, min, sec) \
  { \
     (date)->OCIDateTime.OCITimeHH = hour; \
     (date)->OCIDateTime.OCITimeMI = min; \
     (date)->OCIDateTime.OCITimeSS = sec; \
  }
/*
   NAME: OCIDateSetTime - OCIDate Set Time portion of date
   PARAMETERS:
        date (OUT) - Oracle date whose time data is set
        hour (IN) - hour value to be set
        min (IN) - minute value to be set
        sec (IN) - second value to be set
   DESCRIPTION:
        Set the date with the given time inforamtion.
   RETURNS:
        NONE
 */

/*--------------------------- OCIDateSetDate --------------------------------*/
/* void OCIDateSetDate(/o_ OCIDate *date, sb2 year, ub1 month, ub1 day _o/); */
#define OCIDateSetDate(date, year, month, day) \
  { \
     (date)->OCIDateYYYY = year; \
     (date)->OCIDateMM = month; \
     (date)->OCIDateDD = day; \
  }
/*
   NAME: OCIDateSetDate - OCIDate Set Date (year, month, day) portion of date
   PARAMETERS:
        date (IN) - Oracle date whose year, month, day data is set
        year (OUT) - year value to be set
        month (OUT) - month value to be set
        day (OUT) - day value to be set
   DESCRIPTION:
        Set the date with the given year, month, day inforamtion.
   RETURNS:
        NONE
 *)

(*--------------------------- OCIDateAssign ---------------------------------*)

  TOCIDateAssign = function(err: POCIError; const from: POCIDate;
                        _to: POCIDate): sword; cdecl;
(*
   NAME: OCIDateAssign - OCIDate Assignment
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        from (IN) - date to be assigned
        to (OUT) - lhs of assignment
   DESCRIPTION:
        Performs date assignment.
   RETURNS:
        OCI_SUCCESS
 *)

(*--------------------------- OCIDateToText ---------------------------------*)

  TOCIDateToText = function(err: POCIError; const date: POCIDate;
                        const fmt: poratext; fmt_length: ub1;
                        const lang_name: poratext; lang_length: ub4;
                        buf_size: pub4; buf: poratext): sword; cdecl;
(*
   NAME: OCIDateToText - OCIDate convert date TO String
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date (IN) - Oracle date to be converted
        fmt (IN) - conversion format, if null string pointer (oratext * )0, then
                the date is converted to a character string in the
                date format "DD-MON-YY".
        fmt_length (IN) - length of the 'fmt' parameter
        lang_name (IN) - specifies the language in which the names and
                abbreviations of months and days are returned;
                default language of session is used if 'lang_name'
                is null i.e. (oratext * )0
        lang_length (IN) - length of the 'nls_params' parameter
        buf_size (IN/OUT) - size of the buffer; size of the resulting string
                is returned via this parameter
        buf (OUT) - buffer into which the converted string is placed
   DESCRIPTION:
        Converts the given date to a string according to the specified format.
        Refer to "TO_DATE" conversion function described in
        "Oracle SQL Language Reference Manual" for a description of format
        and NLS arguments. The converted null-terminated date string is
        stored in the buffer 'buf'.

        An error is reported upon overflow, e.g. trying to convert a number
        of value 10 using format '9' causes an overflow.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          buffer too small
          invalid format
          unknown language
          overflow error
 *)

(*---------------------------- OCIDateFromText ------------------------------*)

  TOCIDateFromText = function(err: POCIError; const date_str: poratext;
                        d_str_length: ub4; const fmt: poratext; fmt_length: ub1;
                        const lang_name: poratext; lang_length: ub4;
                        date: POCIDate): sword; cdecl;
(*
   NAME: OCIDateFromText - OCIDate convert String TO Date
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date_str (IN) - input string to be converted to Oracle date
        d_str_length (IN) - size of the input string, if the length is -1
                then 'date_str' is treated as a null terminated  string
        fmt (IN) - conversion format; if 'fmt' is a null pointer, then
                the string is expected to be in 'DD-MON-YY' format.
        fmt_length (IN) - length of the 'fmt' parameter
        lang_name (IN) - language in which the names and abbreviations of
                days and months are specified, if null i.e. (oratext * )0,
                the default language of session is used,
        lang_length (IN) - length of the 'lang_name' parameter
        date (OUT) - given string converted to date
   DESCRIPTION:
        Converts the given string to Oracle date
        according to the specified format. Refer to "TO_DATE" conversion
        function described in "Oracle SQL Language Reference Manual" for a
        description of format.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          invalid format
          unknown language
          invalid input string
          <to be discovered>
 *)

(*----------------------------- OCIDateCompare ------------------------------*)

  TOCIDateCompare = function(err: POCIError; const date1: POCIDate;
                     const date2: POCIDate; _result: psword): sword; cdecl;
(*
   NAME: OCIDateCompare - OCIDate CoMPare dates
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date1, date2 (IN) - dates to be compared
        result (OUT) - comparison result, 0 if equal, -1 if date1 < date2,
                1 if date1 > date2
   DESCRIPTION:
        The function OCIDateCompare compares two dates. It returns -1 if date1
        is smaller than date2, 0 if they are equal, and 1 if date1 is greater
        than date2.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          invalid date
          <to be discovered>
 *)

(*------------------------- OCIDateAddMonths --------------------------------*)

  TOCIDateAddMonths = function(err: POCIError; const date: POCIDate;
                          num_months: sb4; _result: POCIDate): sword; cdecl;
(*
   NAME: OCIDateAddMonths - OCIDate ADd or subtract Months
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date (IN) - 'num_months' added or subtracted from 'date'
        num_months (IN) - number of months to be added or subtracted
                (a negative value will be subtracted)
        result (IN/OUT) - result of adding or subtracting to 'date'
   DESCRIPTION:
        The function OCIDateAddDays adds or subtracts num_months from the
        date 'date'.
          If the input 'date' is the last day of a month, then
        appropriate adjustments are made to ensure that the output date is
        also the last day of the month. For example, Feb. 28 + 1 month =
        March 31, and November 30 - 3 months = August 31. Otherwise the
        'result' date has the same day component as 'date'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          invalid date
          <to be discovered>
 *)

(*--------------------------- OCIDateAddDays --------------------------------*)

  TOCIDateAddDays = function(err: POCIError; const date: POCIDate; num_days: sb4;
                         _result: POCIDate):sword; cdecl;
(*
   NAME: OCIDateAddDays - OCIDate ADd or subtract Days
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date (IN) - 'num_days' added or subtracted from 'date'
        num_days (IN) - number of days to be added or subtracted
                (a negative value will be subtracted)
        result (IN/OUT) - result of adding or subtracting to 'date'
   DESCRIPTION:
        The function OCIDateAddDays adds or subtracts num_days from the
        date 'date'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          invalid date
          <to be discovered>
 *)

(*--------------------------- OCIDateLastDay --------------------------------*)

  TOCIDateLastDay = function(err: POCIError; const date: POCIDate;
                         last_day: POCIDate): sword; cdecl;
(*
   NAME: OCIDateLastDay - OCIDate get date of the LaST day of the month
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date (IN) - input date
        last_day (OUT) - last day of the month in date 'date'
   DESCRIPTION:
        The function OCIDateLastDay returns the date of the last day of the
        month in date 'date'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          invalid date
          <to be discovered>
 *)

(*----------------------- OCIDateDaysBetween --------------------------------*)

  TOCIDateDaysBetween = function(err: POCIError; const date1: POCIDate;
                             const dtae2: POCIDate; num_days: psb4): sword; cdecl;
(*
   NAME: OCIDateDaysBetween - OCIDate get number of days BeTWeen two dates
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date1, date2 (IN) - input dates
        num_days (OUT) - number of days between date1 and date2
   DESCRIPTION:
        The function OCIDateDaysBetween returns the number of days between
        date1 and date2. The time is ignored in this computation.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          invalid date
          <to be discovered>
 *)

(*------------------------ OCIDateZoneToZone --------------------------------*)

  TOCIDateZoneToZone = function(err: POCIError; const date1: POCIDate;
                            const zon1: poratext;
                            zon1_length: ub4; const zon2: poratext;
                            zon2_length: ub4; dtae2: POCIDate): sword; cdecl;
(*
   NAME: OCIDateZoneToZone - OCIDate convert date from one Zone TO another Zone
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date1 (IN) - date to be converted
        zon1 (IN) - zone of input date
        zon1_length (IN) - length in bytes of string 'zon1'
        zon2 (IN) - zone to be converted to
        zon2_length (IN) - length in bytes of string 'zon2'
        date2 (OUT) - converted date (in 'zon2')
   DESCRIPTION:
        Converts date from one time zone to another. Given date 'date1'
        in time zone 'zon1' returns date 'date2' in time zone 'zon2'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          invlid date
          invald input time zone
          invald output time zone
          <to be discovered>
 *)

(*--------------------------- OCIDateNextDay --------------------------------*)

  TOCIDateNextDay = function(err: POCIError; const dtae: POCIDate;
                         const day_p: poratext; day_length: ub4;
                         next_day: POCIDate): sword; cdecl;
(*
   NAME: OCIDateNextDay - OCIDate get date of Next DaY
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date (IN) - returned date should be later than this date
        day (IN) - first day of week named by this is returned
        day_length (IN) - length in bytes of string 'day'
        next_day (OUT) - first day of the week named by 'day' later than 'date'
   DESCRIPTION:
        Returns the date of the first day of the
        week named by 'day' that is later than date 'date'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          invalid date
          invalid day
          <to be discovered>
 */

/*----------------------------- OCIDateCheck --------------------------------*/

/* Listing of error bits used by OCIDateCheck() */
#define OCI_DATE_INVALID_DAY            0x1                       /* Bad DAy */
#define OCI_DATE_DAY_BELOW_VALID        0x2   /* Bad DAy Low/high bit (1=low)*/
#define OCI_DATE_INVALID_MONTH          0x4                     /* Bad MOnth */
#define OCI_DATE_MONTH_BELOW_VALID      0x8 /* Bad MOnth Low/high bit (1=low)*/
#define OCI_DATE_INVALID_YEAR           0x10                     /* Bad YeaR */
#define OCI_DATE_YEAR_BELOW_VALID       0x20 /* Bad YeaR Low/high bit (1=low)*/
#define OCI_DATE_INVALID_HOUR           0x40                     /* Bad HouR */
#define OCI_DATE_HOUR_BELOW_VALID       0x80 /* Bad HouR Low/high bit (1=low)*/
#define OCI_DATE_INVALID_MINUTE         0x100                  /* Bad MiNute */
#define OCI_DATE_MINUTE_BELOW_VALID     0x200
                                           /* Bad MiNute Low/high bit (1=low)*/
#define OCI_DATE_INVALID_SECOND         0x400                  /* Bad SeCond */
#define OCI_DATE_SECOND_BELOW_VALID     0x800
                                           /* bad second Low/high bit (1=low)*/
#define OCI_DATE_DAY_MISSING_FROM_1582  0x1000
                                  /* Day is one of those "missing" from 1582 */
#define OCI_DATE_YEAR_ZERO              0x2000    /* Year may not equal zero */
#define OCI_DATE_INVALID_FORMAT         0x8000      /* Bad date format input */

sword OCIDateCheck(    OCIError *err, const OCIDate *date, uword *valid    );
/*
   NAME: OCIDateCheck - OCIDate CHecK if the given date is valid
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        date (IN) - date to be checked
        valid (OUT) -  returns zero for a valid date, otherwise
                the ORed combination of all error bits specified below:

   Macro name                   Bit number      Error
   ----------                   ----------      -----
   OCI_DATE_INVALID_DAY         0x1             Bad day
   OCI_DATE_DAY_BELOW_VALID     0x2             Bad DAy Low/high bit (1=low)
   OCI_DATE_INVALID_MONTH       0x4             Bad MOnth
   OCI_DATE_MONTH_BELOW_VALID   0x8             Bad MOnth Low/high bit (1=low)
   OCI_DATE_INVALID_YEAR        0x10            Bad YeaR
   OCI_DATE_YEAR_BELOW_VALID    0x20            Bad YeaR Low/high bit (1=low)
   OCI_DATE_INVALID_HOUR        0x40            Bad HouR
   OCI_DATE_HOUR_BELOW_VALID    0x80            Bad HouR Low/high bit (1=low)
   OCI_DATE_INVALID_MINUTE      0x100           Bad MiNute
   OCI_DATE_MINUTE_BELOW_VALID  0x200           Bad MiNute Low/high bit (1=low)
   OCI_DATE_INVALID_SECOND      0x400           Bad SeCond
   OCI_DATE_SECOND_BELOW_VALID  0x800           bad second Low/high bit (1=low)
   OCI_DATE_DAY_MISSING_FROM_1582 0x1000        Day is one of those "missing"
                                                from 1582
   OCI_DATE_YEAR_ZERO           0x2000          Year may not equal zero
   OCI_DATE_INVALID_FORMAT      0x8000          Bad date format input

   So, for example, if the date passed in was 2/0/1990 25:61:10 in
   (month/day/year hours:minutes:seconds format), the erroor returned
   would be OCI_DATE_INVALID_DAY | OCI_DATE_DAY_BELOW_VALID |
   OCI_DATE_INVALID_HOUR | OCI_DATE_INVALID_MINUTE

   DESCRIPTION:
        Check if the given date is valid.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          'date' and 'valid' pointers are NULL pointers
 */

/*--------------------------- OCIDateSysDate --------------------------------*/

sword OCIDateSysDate(    OCIError *err, OCIDate *sys_date    );
/*
   NAME: OCIDateSysDate - OCIDate get current SYStem date and time
   PARAMETERS:
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        sys_date (OUT) - current system date and time
   DESCRIPTION:
        Returns the current system date and time.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'err' is NULL.
        OCI_ERROR if
          <to be discovered>
 */

/*****************************************************************************/
/*                  FIXED-LENGTH STRING - CHAR (N)                           */
/*****************************************************************************/

/*
 * An ADT attribute declared as "x CHAR(n)" is mapped to "OCIString *x;".
 * The representation of OCIString * is shown below.
 */

/*****************************************************************************/
/*                  VARIABLE-LENGTH STRING                                   */
/*****************************************************************************/

/*
 * The variable-length string is represented in C as a pointer to OCIString
 * structure. The OCIString structure is opaque to the user. Functions are
 * provided to allow the user to manipulate a variable-length string.
 *
 * A variable-length string can be declared as:
 *
 * OCIString *vstr;
 *
 * For binding variables of type OCIString* in OCI calls (OCIBindByName(),
 * OCIBindByPos() and OCIDefineByPos()) use the external type code SQLT_VST.
 */
typedef struct OCIString OCIString;

/*-------------------------- OCIStringAssign --------------------------------*/

sword OCIStringAssign(    OCIEnv *env, OCIError *err, const OCIString *rhs,
                          OCIString **lhs    );
/*
   NAME: OCIStringAssign - OCIString Assign String to String
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        rhs (IN) - RHS of the assignment, the type of rhs is also OCIString
        lhs (IN/OUT) - LHS of the assignment
   DESCRIPTION:
        Assign 'rhs' string to 'lhs' string. The 'lhs' string may be
        resized depending upon the size of the 'rhs'. The assigned string is
        null-terminated. The 'length' field will not include the extra byte
        needed for null termination.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          out of space error
 */

/*---------------------- OCIStringAssignText --------------------------------*/

sword OCIStringAssignText(    OCIEnv *env, OCIError *err, const oratext *rhs,
                              ub4 rhs_len, OCIString **lhs    );
/*
   NAME: OCIStringAssignText - OCIString Assign Text string to String
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        rhs (IN) - RHS of the assignment, the type of rhs is a text string
        rhs_len (IN) - length of the 'rhs' string
        lhs (IN/OUT) - LHS of the assignment
   DESCRIPTION:
        Assign 'rhs' string to 'lhs' string. The 'lhs' string may be
        resized depending upon the size of the 'rhs'. The assigned string is
        null-terminated. The 'length' field will not include the extra byte
        needed for null termination.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          out of space error
 */

/*-------------------------- OCIStringResize --------------------------------*/

sword OCIStringResize(    OCIEnv *env, OCIError *err, ub4 new_size,
                          OCIString **str    );
/*
   NAME: OCIStringResize - OCIString ReSiZe string memory
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        new_size (IN) - new memory size of the string in bytes
        str (IN/OUT) - allocated memory for the string is freed from the
                OOCI heap
   DESCRIPTION:
        This function resizes the memory of the given variable-length string in
        the object cache. The contents of the string are NOT preserved.
        This function may allocate the string in a new memory region in
        which case the original memory occupied by the given string will
        be freed. If the input string is null (str == NULL), then this
        function will allocate memory for the string.

        If the new_size is 0, then this function frees the memory occupied
        by 'str' and a null pointer value is returned.

        NOTE: The caller must compute 'new_size' taking into account space
        for the null character ('\0').
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          out of space error
 */

/*---------------------------- OCIStringSize --------------------------------*/

ub4 OCIStringSize(    OCIEnv *env, const OCIString *vs    );
/*
   NAME: OCIStringSize - OCIString Get String siZe
   PARAMETERS:
        env(IN) - pointer to OCI environment handle
        vs (IN) - string whose size is returned
   DESCRIPTION:
        Return the size of the given string.
   RETURNS:
        size of the string in bytes is returned
 */

/*----------------------------- OCIStringPtr --------------------------------*/

oratext *OCIStringPtr(    OCIEnv *env, const OCIString *vs    );
/*
   NAME: OCIStringPtr - OCIString Get String Pointer
   PARAMETERS:
        env(IN) - pointer to OCI environment handle
        vs (IN) - pointer to the text of this string is returned
   DESCRIPTION:
        Return the pointer to the text of the given string.
   RETURNS:
        pointer to the text of the string is returned
 */

/*----------------------- OCIStringAllocSize --------------------------------*/

sword OCIStringAllocSize(    OCIEnv *env, OCIError *err, const OCIString *vs,
                             ub4 *allocsize    );
/*
   NAME: OCIStringAllocSize - OCIString get Allocated SiZe of string memory
                              in bytes
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        vs (IN) - string whose allocated size in bytes is returned
        allocsize (OUT) - allocated size of string memory in bytes is returned
   DESCRIPTION:
        Return the allocated size of the string memory in bytes. The
        allocated size is >= actual string size.
   REQUIRES:
        vs is a non-null pointer
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR on error
 */

/*****************************************************************************/
/*                       VARIABLE-LENGTH RAW                                 */
/*****************************************************************************/

/*
 * The variable-length raw is represented in C as a pointer to OCIRaw
 * structure. The OCIRaw structure is opaque to the user. Functions are
 * provided to allow the user to manipulate a variable-length raw.
 *
 * A variable-length raw can be declared as:
 *
 * OCIRaw *raw;
 *
 * For binding variables of type OCIRaw* in OCI calls (OCIBindByName(),
 * OCIBindByPos() and OCIDefineByPos()) use the external type code SQLT_LVB.
 */
typedef struct OCIRaw OCIRaw;

/*-------------------------- OCIRawAssignRaw --------------------------------*/

sword OCIRawAssignRaw(    OCIEnv *env, OCIError *err, const OCIRaw *rhs,
                          OCIRaw **lhs    );
/*
   NAME: OCIRawAssignRaw - OCIRaw Assign Raw (of type OCIRaw* ) to
                   Raw (of type OCIRaw* )
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        rhs (IN) - RHS of the assignment, the type of rhs is also OCIRaw
        lhs (IN/OUT) - LHS of the assignment
   DESCRIPTION:
        Assign 'rhs' raw to 'lhs' raw. The 'lhs' raw may be
        resized depending upon the size of the 'rhs'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          out of space error
 */

/*------------------------ OCIRawAssignBytes --------------------------------*/

sword OCIRawAssignBytes(    OCIEnv *env, OCIError *err, const ub1 *rhs,
                            ub4 rhs_len, OCIRaw **lhs    );
/*
   NAME: OCIRawAssignBytes - OCIRaw Assign raw Bytes (of type ub1* ) to Raw
                   (of type OCIRaw* )
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        rhs (IN) - RHS of the assignment, the type of rhs is ub1 *
        rhs_len (IN) - length of the 'rhs' raw
        lhs (IN/OUT) - LHS of the assignment
   DESCRIPTION:
        Assign 'rhs' raw to 'lhs' raw. The 'lhs' raw may be
        resized depending upon the size of the 'rhs'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          out of space error
 */

/*---------------------------- OCIRawResize ---------------------------------*/

sword OCIRawResize(    OCIEnv *env, OCIError *err, ub4 new_size,
                       OCIRaw **raw    );
/*
   NAME: OCIRawResize - OCIRaw ReSiZe memory of variable-length raw
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        new_size (IN) - new size of the raw data in bytes
        raw (IN) - variable-length raw pointer; the raw is
                resized to 'new_size'
   DESCRIPTION:
        This function resizes the memory of the given variable-length raw in
        the object cache.
        The previous contents of the raw are NOT preserved.
        This function may allocate the raw in a new memory region in
        which case the original memory occupied by the given raw will
        be freed. If the input raw is null (raw == NULL), then this
        function will allocate memory for the raw data.

        If the new_size is 0, then this function frees the memory occupied
        by 'raw' and a null pointer value is returned.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          out of space error
 */

/*------------------------------- OCIRawSize --------------------------------*/

ub4 OCIRawSize(    OCIEnv * env, const OCIRaw *raw    );
/*
   NAME: OCIRawSize - OCIRaw Get Raw siZe
   PARAMETERS:
        env (IN)  - pointer to OCI environment handle
        raw (INT) - raw whose size is returned
   DESCRIPTION:
        Return the size of the given raw.
   RETURNS:
        size of the raw in bytes is returned
 */

/*--------------------------------- OCIRawPtr -------------------------------*/
ub1 *OCIRawPtr(    OCIEnv * env, const OCIRaw *raw    );
/*
   NAME: OCIRawPtr - OCIRaw Get Raw data Pointer
   PARAMETERS:
        env (IN) - pointer to OCI environment handle
        raw (IN) - pointer to the data of this raw is returned
   DESCRIPTION:
        Return the pointer to the data of the given raw.
   RETURNS:
        pointer to the data of the raw is returned
 */

/*------------------------------ OCIRawAllocSize ----------------------------*/

sword OCIRawAllocSize(    OCIEnv *env, OCIError *err, const OCIRaw *raw,
                          ub4 *allocsize    );
/*
   NAME: OCIRawAllocSize - OCIRaw get Allocated SiZe of raw memory in bytes
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        raw (IN) - raw whose allocated size in bytes is returned
        allocsize (OUT) - allocated size of raw memory in bytes is returned
   DESCRIPTION:
        Return the allocated size of the raw memory in bytes. The
        allocated size is >= actual raw size.
   REQUIRES:
        raw is a non-null pointer
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR upon error
 *)

(*****************************************************************************
 *                     OBJECT REFERENCE OPERATIONS                           *
 *****************************************************************************)

(*
 * See the definition of OCIRef in oro.h.
 *
 * For binding variables of type OCIRef* in OCI calls (OCIBindByName(),
 * OCIBindByPos() and OCIDefineByPos()) use the code SQLT_REF.
 *
 *)

(*------------------------- OBJECT REFERENCE (REF) --------------------------*)

  PPOCIRef = ^POCIRef;
  POCIRef = Pointer;
(*
 * OCIRef - OCI object REFerence
 *
 * In the Oracle object runtime environment, an object is identified by an
 * object reference (ref) which contains the object identifier plus other
 * runtime information.  The contents of a ref is opaque to clients.  Use
 * OCIObjectNew() to construct a ref.
 *)

(*---------------------------- OCIRefClear ----------------------------------*)
  TOCIRefClear = procedure(env: POCIEnv; ref: POCIRef);
(*
   NAME: OCIRefClear - OCIRef CLeaR or nullify a ref
   PARAMETERS:
        env (IN)     - pointer to OCI environment handle
        ref (IN/OUT) - ref to clear
   DESCRIPTION:
        Clear or nullify the given ref. A ref is considered to be a null ref
        if it does not contain a valid OID (and thus doesn't point to an
        object). Logically, a null ref is a dangling ref.

        Note that a null ref is still a valid SQL value and is not SQL-ly null.
        It can be used as a valid non-null constant ref value for NOT NULL
        column or attribute of a row in a table.

        If a null pointer value is passed as a ref,
        then this function is a no-op.
 *)

(*--------------------------- OCIRefAssign ----------------------------------*)

  TOCIRefAssign = function(env: POCIEnv; err: POCIError; const source: POCIRef;
                       target: PPOCIRef): sword; cdecl;
(*
   NAME: OCIRefAssign - OCIRef CoPY a ref to another
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        source (IN) - ref to copy from
        target (IN/OUT) - ref to copy to
   DESCRIPTION:
        Copy 'source' ref to 'target' ref; both then reference the same
        object. If the target ref pointer is null (i.e. *target == NULL)
        then the copy function will allocate memory for the target ref
        in OOCI heap prior to the copy.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          1) out of memory
 */

/*-------------------------- OCIRefIsEqual ----------------------------------*/
boolean OCIRefIsEqual(    OCIEnv *env, const OCIRef *x, const OCIRef *y    );
/*
   NAME: OCIRefIsEqual - OCIRef compare two refs for EQUality
   PARAMETERS:
        env (IN) - pointer to OCI environment handle
        x (IN)   - ref to compare
        y (IN)   - ref to compare
   DESCRIPTION:
        Compare the given refs for equality.
        Two refs are equal if and only if:
          - they are both referencing the same persistent object, or
          - they are both referencing the same transient object.

        NOTE THAT TWO NULL REFS ARE CONSIDERED NOT EQUAL BY THIS FUNCTION.
   RETURNS:
        TRUE if the two refs are equal
        FALSE if the two refs are not equal, or X is NULL, or Y is NULL
 */

/*--------------------------- OCIRefIsNull ----------------------------------*/
boolean OCIRefIsNull(    OCIEnv *env, const OCIRef *ref    );
/*
   NAME: OCIRefIsNull - OCIRef test if a ref is NULl
   PARAMETERS:
        env (IN) - pointer to OCI environment handle
        ref (IN) - ref to test for null
   DESCRIPTION:
        Return TRUE if the given ref is null; otherwise, return FALSE.
        A ref is null if and only if:
        - it is supposed to be referencing a persistent object, but
          its OID is null, or
        - it is supposed to be referencing a transient object, but it is
          currently not pointing to an object.
        A ref is a dangling ref if the object that it points to does not
        exist.
   RETURNS:
        TRUE if the given ref is NULL
        FALSE if the given ref is not NULL
 */

/*-------------------------- OCIRefHexSize ----------------------------------*/
ub4 OCIRefHexSize(    OCIEnv *env, const OCIRef *ref    );
/*
   NAME: OCIRefHexSize - OCIRef Hexadecimal buffer SiZe in bytes
   PARAMETERS:
        env (IN) - pointer to OCI environment handle
        ref (IN) - ref whose size in hexadecimal representation in bytes is
                returned
   DESCRIPTION:
        Return the size of the buffer in bytes required for the hexadecimal
        representation of the ref. A buffer of at-least this size must be
        passed to ref-to-hex (OCIRefToHex) conversion function.
   RETURNS:
        size of hexadecimal representation of ref
 */

/*-------------------------- OCIRefFromHex ---------------------------------*/
sword OCIRefFromHex(    OCIEnv *env, OCIError *err, const OCISvcCtx *svc,
                        const oratext *hex, ub4 length, OCIRef **ref    );
/*
   NAME:
        OCIRefFromHex - OCIRef convert a Hexadecimal string TO a Ref
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by
                calling OCIErrorGet().
        svc (IN) - OCI service context handle; if the resulting ref is
                initialized with this service context
        hex (IN) - hexadecimal string (that was produced by 'OCIRefToHex()"
                previously) to be convert into a ref
        length (IN) - length of the hexadecimal string
        ref (IN/OUT) - ref is initialized with the given value ('hex').
                If *ref is null, then space for the ref is allocated in the
                object cache, otherwise the memory occupied by the given ref
                is re-used.
   DESCRIPTION:
        Convert the given hexadecimal string into a ref. This function
        ensures that the resulting ref is well formed. It does NOT ensure
        that the object pointed to by the resulting ref exists or not.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
 */

/*--------------------------- OCIRefToHex -----------------------------------*/
sword OCIRefToHex(    OCIEnv *env, OCIError *err, const OCIRef *ref,
                      oratext *hex, ub4 *hex_length    );
/*
   NAME:
        OCIRefToHex - OCIRef convert ref to a Hexadecimal string
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by
                calling OCIErrorGet().
        ref (IN) - ref to be converted into a hexadecimal string; if the
                ref is a null ref (i.e. OCIRefIsNull(ref) == TRUE) then
                a zero hex_length value is returned
        hex (OUT) - buffer that is large enough to contain the resulting
                hexadecimal string; the contents of the string is opaque
                to the caller
        hex_length (IN/OUT) - on input specifies the size of the 'hex' buffer,
                on output specifies the actual size of the hexadecimal
                string being returned in 'hex'
   DESCRIPTION:
        Convert the given ref into a hexadecimal string, and return the length
        of the string. The resulting string is opaque to the caller.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          the given buffer is not big enough to hold the resulting string
 */


/*****************************************************************************/
/*                          COLLECTION FUNCTIONS                             */
/*****************************************************************************/

/*
   The generic collection is represented by the type 'OCIColl'. The following
   operations OCIColl*() are provided on a generic collection:
     - get current size of collection
     - get upper bound of collection
     - get pointer to an element given its index
     - set element at given index (assign element)
     - append an element
     - trim the given number of elements from the end of the collection
     - collection assignment

   The following iterator based scanning functions are also provided on a
   generic collection. These functions make use of an iterator which is
   defined to be of type OCIIter.

     - create an iterator for scanning collection
     - destroy iterator
     - reset iterator to the beginning of collection
     - get pointer to current element pointed by iterator
     - get pointer to next element
     - get pointer to previous element

   The collections variable-length array (varray) and nested table
   are sub-types of generic collection. This means that the OCIColl*()
   functions can also be used to manipulate varray and nested table.

   The varray is represented by OCIArray type and nested table by OCITable.
   Besides OCIColl*() functions no additional functions are provided for
   manipulating varrays. The OCIColl*() functions are a complete set of
   functions to manipulate varrays.

   Besides OCIColl*() functions, the following functions OCITable*() can be
   used to manipulate nested table. The OCITable*() functions operate on
   nested tables only and should not be used on a varray.

     - delete an element at index i. Note that the position
       ordinals of the remaining elements of the table is not changed by the
       delete operation. So delete creates "holes" in the table.
     - check if an element exists at the given index i
     - return the smallest value of i for which exists(i) is true
     - return the largest value of i for which exists(i) is true
     - return pointer to the smallest position j, greater than i, such that
       OCITableExists(j) is true
     - return pointer to the largest position j, less than i, such that
       OCITableExists(j) is true

   For binding variables of type OCIColl* or OCITable* in OCI calls
   (OCIBindByName(), OCIBindByPos() and OCIDefineByPos()) use the external
   type code SQLT_NTY.
 */

/* OCIColl - generic collection type */
typedef struct OCIColl OCIColl;

/* OCIArray - varray collection type */
typedef OCIColl OCIArray;

/* OCITable - nested table collection type */
typedef OCIColl OCITable;

/* OCIIter - collection iterator */
typedef struct OCIIter OCIIter;

/*----------------------------- OCICollSize ---------------------------------*/

sword OCICollSize( OCIEnv *env, OCIError *err, const OCIColl *coll,
                   sb4 *size );
/*
   NAME: OCICollSize - OCIColl return current SIZe of the given collection
   PARAMETERS:
        env(IN) - pointer to OCI environment handle
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        coll (IN) - collection whose number of elements is returned
        size (OUT) - current number of elements in the collection
   DESCRIPTION:
          Returns the current number of elements in the given collection.

          For collections of type nested table wherein 'delete element'
          operation is allowed, the count returned by OCICollSize() will
          NOT be decremented upon deleting elements. For example:

            OCICollSize(...);
            // assume 'size' returned is equal to 5
            OCITableDelete(...); // delete one element
            OCICollSize(...);
            // 'size' returned will still be 5

          To get the count minus the deleted elements use OCITableSize().
          Continuing the above example,

            OCITableSize(...)
            // 'size' returned will be equal to 4

          Note, a trim operation (OCICollTrim) will decrement the count
          by the number of trimmed elements. Continuing the above example,

            OCICollTrim(..,1..); // trim one element
            OCICollSize(...);
            // 'size' returned will be equal to 4
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          error during loading of collection into object cache
          any of the input parameters is null
 */

/*------------------------------ OCICollMax ---------------------------------*/

sb4 OCICollMax(    OCIEnv *env, const OCIColl *coll    );
/*
   NAME: OCICollMax - OCIColl return MAXimum size (upper-bound) of the
                   given collection (in number of elements)
   PARAMETERS:
        env(IN) - pointer to OCI environment handle
        coll (IN) - collection whose upper-bound in number of elements
                is returned
   DESCRIPTION:
        Returns the max number of elements that the given collection can hold.
        A value 0 indicates that the collection has no upper-bound.
   REQUIRES:
        coll must point to a valid collection descriptor
   RETURNS:
        upper-bound of the given collection
 */

/*-------------------------- OCICollGetElem ---------------------------------*/

sword OCICollGetElem(    OCIEnv *env, OCIError *err, const OCIColl *coll,
                         sb4 index, boolean *exists, void  **elem,
                         void  **elemind    );
/*
   NAME: OCICollGetElem - OCIColl GET pointer to the element at the given index
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        coll (IN) - pointer to the element in this collection is returned
        index (IN) - index of the element whose pointer is returned
        exists (OUT) - set to FALSE if element at the specified index does
                not exist else TRUE
        elem (OUT) - address of the desired element is returned
        elemind (OUT) [optional] - address of the null indicator information
                is returned; if (elemind == NULL) then the null indicator
                information will NOT be returned
   DESCRIPTION:
        Get the address of the element at the given position. Optionally
        this function also returns the address of the element's null indicator
        information.

        The following table describes for each collection element type
        what the corresponding element pointer type is. The element pointer
        is returned via the 'elem' parameter of OCICollGetElem().

           Element Type                       *elem is set to
        -----------------------               ---------------
         Oracle Number (OCINumber)              OCINumber*
         Date (OCIDate)                         OCIDate*
         Variable-length string (OCIString* )   OCIString**
         Variable-length raw (OCIRaw* )         OCIRaw**
         object reference (OCIRef* )            OCIRef**
         lob locator (OCILobLocator* )          OCILobLocator**
         object type (e.g. person)              person*

        The element pointer returned by OCICollGetElem() is in a form
        such that it can not only be used to access the
        element data but also is in a form that can be used as the target
        (i.e left-hand-side) of an assignment statement.

        For example, assume the user is iterating over the elements of
        a collection whose element type is object reference (OCIRef* ). A call
        to OCICollGetElem() returns pointer to a reference handle
        (i.e. OCIRef** ). After getting, the pointer to the collection
        element, the user may wish to modify it by assigning a new reference.
        This can be accomplished via the ref assignment function shown below:

        sword OCIRefAssign( OCIEnv *env, OCIError *err, const OCIRef *source,
                            OCIRef **target );

        Note that the 'target' parameter of OCIRefAssign() is of type
        'OCIRef**'. Hence OCICollGetElem() returns 'OCIRef**'.
        If '*target == NULL' a new ref will be allocated by OCIRefAssign()
        and returned via the 'target' parameter.

        Similarly, if the collection element was of type string (OCIString* ),
        OCICollGetElem() returns pointer to string handle
        (i.e. OCIString** ). If a new string is assigned, via
        OCIStringAssign() or OCIStringAssignText() the type of the target
        must be 'OCIString **'.

        If the collection element is of type Oracle number, OCICollGetElem()
        returns OCINumber*. The prototype of OCINumberAssign() is shown below:

        sword OCINumberAssign(OCIError *err, const OCINumber *from,
                              OCINumber *to);
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
 */

/*------------------------- OCICollGetElemArray -----------------------------*/

sword OCICollGetElemArray(  OCIEnv *env, OCIError *err, const OCIColl *coll,
                            sb4 index, boolean *exists, void  **elem,
                            void  **elemind, uword *nelems);
/*
   NAME: OCICollGetElemArray - OCIColl GET pointers to elements from given index
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        coll (IN) - pointers to the elements in this collection is returned
        index (IN) - starting index of the element
        exists (OUT) - set to FALSE if element at the specified index does
                not exist else TRUE
        elem (OUT) - address of the desired elements is returned
        elemind (OUT) [optional] - address of the null indicators information
                is returned; if (elemind == NULL) then the null indicator
                information will NOT be returned
        nelems(IN/OUT) - Upper bound of elem and/or elemind array
   DESCRIPTION:
        Get the address of the elements from the given position. Optionally
        this function also returns the address of the element's null indicator
        information.

        The following table describes for each collection element type
        what the corresponding element pointer type is. The element pointer
        is returned via the 'elem' parameter of OCICollGetElem().

           Element Type                       *elem is set to
        -----------------------               ---------------
         Oracle Number (OCINumber)              OCINumber*
         Date (OCIDate)                         OCIDate*
         Variable-length string (OCIString* )   OCIString**
         Variable-length raw (OCIRaw* )         OCIRaw**
         object reference (OCIRef* )            OCIRef**
         lob locator (OCILobLocator* )          OCILobLocator**
         object type (e.g. person)              person*

        The element pointer returned by OCICollGetElem() is in a form
        such that it can not only be used to access the
        element data but also is in a form that can be used as the target
        (i.e left-hand-side) of an assignment statement.

        For example, assume the user is iterating over the elements of
        a collection whose element type is object reference (OCIRef* ). A call
        to OCICollGetElem() returns pointer to a reference handle
        (i.e. OCIRef** ). After getting, the pointer to the collection
        element, the user may wish to modify it by assigning a new reference.
        This can be accomplished via the ref assignment function shown below:

        sword OCIRefAssign( OCIEnv *env, OCIError *err, const OCIRef *source,
                            OCIRef **target );

        Note that the 'target' parameter of OCIRefAssign() is of type
        'OCIRef**'. Hence OCICollGetElem() returns 'OCIRef**'.
        If '*target == NULL' a new ref will be allocated by OCIRefAssign()
        and returned via the 'target' parameter.

        Similarly, if the collection element was of type string (OCIString* ),
        OCICollGetElem() returns pointer to string handle
        (i.e. OCIString** ). If a new string is assigned, via
        OCIStringAssign() or OCIStringAssignText() the type of the target
        must be 'OCIString **'.

        If the collection element is of type Oracle number, OCICollGetElem()
        returns OCINumber*. The prototype of OCINumberAssign() is shown below:

        sword OCINumberAssign(OCIError *err, const OCINumber *from,
                              OCINumber *to);
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
 */

/*----------------------- OCICollAssignElem ---------------------------------*/

sword OCICollAssignElem(    OCIEnv *env, OCIError *err, sb4 index,
                            const void  *elem,
                            const void  *elemind, OCIColl *coll    );
/*
   NAME: OCICollAssignElem - OCIColl ASsign Element
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        index (IN) - index of the element whose is assigned to
        elem (IN) - element which is assigned from (source element)
        elemind (IN) [optional] - pointer to the element's null indicator
                information; if (elemind == NULL) then the null indicator
                information of the assigned element will be set to non-null.
        coll (IN/OUT) - collection to be updated
   DESCRIPTION:
        Assign the given element value 'elem' to the element at coll[index].
        If the collection is of type nested table, the element at the given
        index may not exist (i.e. may have been deleted). In this case, the
        given element is inserted at index 'index'.
        Otherwise, the element at index 'index' is updated with the value
        of 'elem'.

        Note that the given element is deep-copied and
        'elem' is strictly an input parameter.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
          out of memory error
          given index is out of bounds of the given collection
 */

/*--------------------------- OCICollAssign ---------------------------------*/

sword OCICollAssign(    OCIEnv *env, OCIError *err, const OCIColl *rhs,
                        OCIColl *lhs    );
/*
   NAME: OCICollAssign - OCIColl ASsiGn collection
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        rhs (IN) - collection to be assigned from
        lhs (OUT) - collection to be assigned to
   DESCRIPTION:
        Assign 'rhs' to 'lhs'. The 'lhs' collection may be decreased or
        increased depending upon the size of 'rhs'. If the 'lhs' contains
        any elements then the elements will be deleted prior to the
        assignment. This function performs a deep-copy. The memory for the
        elements comes from the object cache.

        An error is returned if the element types of the lhs and rhs
        collections do not match. Also, an error is returned if the
        upper-bound of the lhs collection is less than the current number of
        elements in the rhs collection.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
          out of memory error
          type mis-match of lhs and rhs collections
          upper-bound of lhs collection is less than the current number of
          elements in the rhs collection
 */

/*--------------------------- OCICollAppend ---------------------------------*/

sword OCICollAppend(    OCIEnv *env, OCIError *err, const void  *elem,
                        const void  *elemind, OCIColl *coll    );
/*
   NAME: OCICollAppend - OCIColl APPend collection
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        elem (IN) - pointer to the element which is appended to the end
                of the given collection
        elemind (IN) [optional] - pointer to the element's null indicator
                information; if (elemind == NULL) then the null indicator
                information of the appended element will be set to non-null.
        coll (IN/OUT) - updated collection
   DESCRIPTION:
        Append the given element to the end of the given collection.
        Appending an element is equivalent to:
          - increasing the size of the collection by 1 element
          - updating (deep-copying) the last element's data with the given
            element's data

        Note that the pointer to the given element 'elem' will not be saved
        by this function. So 'elem' is strictly an input parameter.
        An error is returned if the current size of the collection
        is equal to the max size (upper-bound) of the collection prior to
        appending the element.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
          out of memory error
          current size of collection == max size of the collection
 */

/*----------------------------- OCICollTrim ---------------------------------*/

sword OCICollTrim(    OCIEnv *env, OCIError *err, sb4 trim_num,
                      OCIColl *coll    );
/*
   NAME: OCICollTrim - OCIColl Trim elements from the end of the collection
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        trim_num (IN) - number of elements to trim
        coll (IN/OUT) - 'trim_num' of elements are removed (freed) from the
                end of the collection
   DESCRIPTION:
        Trim the collection by the given number of elements. The elements are
        removed from the end of the collection.

        An error is returned if the 'trim_num' is greater than the current
        size of the collection.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
          'trim_num' is greater than the current size of the collection.
 */

/*--------------------------- OCICollIsLocator ------------------------------*/

sword OCICollIsLocator(OCIEnv *env, OCIError *err, const OCIColl *coll,
                       boolean *result );
/*
Name: OCICollIsLocator - OCIColl indicates whether a collection is locator
                         based or not.
Parameters:
        env(IN) - pointer to OCI environment handle
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        coll (IN) - collection item.
        result (OUT) - TRUE if the collection item is a locator, FALSE
                       otherwise
Description:
        Returns TRUE in the result OUT parameter if the collection item is a
        locator, otherwise returns FALSE.
Returns:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
*/

/*---------------------------- OCIIterCreate --------------------------------*/

sword OCIIterCreate(    OCIEnv *env, OCIError *err, const OCIColl *coll,
                        OCIIter **itr    );
/*
   NAME: OCIIterCreate - OCIColl Create an ITerator to scan the collection
                      elements
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        coll (IN) - collection which will be scanned; the different
                collection types are varray and nested table
        itr (OUT) - address to the allocated collection iterator is
                returned by this function
   DESCRIPTION:
        Create an iterator to scan the elements of the collection. The
        iterator is created in the object cache. The iterator is initialized
        to point to the beginning of the collection.

        If the next function (OCIIterNext) is called immediately
        after creating the iterator then the first element of the collection
        is returned.
        If the previous function (OCIIterPrev) is called immediately after
        creating the iterator then "at beginning of collection" error is
        returned.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
          out of memory error
 */

/*----------------------------- OCIIterDelete ------------------------------*/

sword OCIIterDelete(    OCIEnv *env, OCIError *err, OCIIter **itr    );
/*
   NAME: OCIIterDelete - OCIColl Delete ITerator
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        itr (IN/OUT) - the allocated collection iterator is destroyed and
                the 'itr' is set to NULL prior to returning
   DESCRIPTION:
        Delete the iterator which was previously created by a call to
        OCIIterCreate.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
          to be discovered
 */

/*----------------------------- OCIIterInit ---------------------------------*/

sword OCIIterInit(    OCIEnv *env, OCIError *err, const OCIColl *coll,
                      OCIIter *itr    );
/*
   NAME: OCIIterInit - OCIColl Initialize ITerator to scan the given
                   collection
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        coll (IN) - collection which will be scanned; the different
                collection types are varray and nested table
        itr (IN/OUT) - pointer to an allocated  collection iterator
   DESCRIPTION:
        Initializes the given iterator to point to the beginning of the
        given collection. This function can be used to:

        a. reset an iterator to point back to the beginning of the collection
        b. reuse an allocated iterator to scan a different collection
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
 *)

(*------------------------ OCIIterGetCurrent --------------------------------*)

  TOCIIterGetCurrent = function(hndl: POCIEnv; err: POCIError; itr: POCIIter;
      elem: PPointer; elemind: PPointer): sword; cdecl;
(*
   NAME: OCIIterGetCurrent - OCIColl Iterator based, get CURrent collection
                    element
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        itr (IN) - iterator which points to the current element
        elem (OUT) - address of the element pointed by the iterator is returned
        elemind (OUT) [optional] - address of the element's null indicator
                information is returned; if (elemind == NULL) then the null
                indicator information will NOT be returned
   DESCRIPTION:
        Returns pointer to the current element and its corresponding null
        information.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
 *)

(*------------------------------ OCIIterNext --------------------------------*)

  TOCIIterNext = function(hndl: POCIEnv; err: POCIError; itr: POCIIter;
      elem: PPointer; elemind: PPointer; eoc: PBoolean): sword; cdecl;
(*
   NAME: OCIIterNext - OCIColl Iterator based, get NeXT collection element
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        itr (IN/OUT) - iterator is updated to point to the next element
        elem (OUT) - after updating the iterator to point to the next element,
                address of the element is returned
        elemind (OUT) [optional] - address of the element's null indicator
                information is returned; if (elemind == NULL) then the null
                indicator information will NOT be returned
        eoc (OUT) - TRUE if iterator is at End Of Collection (i.e. next
                element does not exist) else FALSE
   DESCRIPTION:
        Returns pointer to the next element and its corresponding null
        information. The iterator is updated to point to the next element.

        If the iterator is pointing to the last element of the collection
        prior to executing this function, then calling this function will
        set eoc flag to TRUE. The iterator will be left unchanged in this
        situation.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
 *)

(*------------------------------ OCIIterPrev --------------------------------*)

  TOCIIterPrev = function(hndl: POCIEnv; err: POCIError; itr: POCIIter;
                      elem: PPointer; elemind: PPointer; boc: PBoolean): sword; cdecl;
(*
   NAME: OCIIterPrev - OCIColl Iterator based, get PReVious collection element
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        itr (IN/OUT) - iterator is updated to point to the previous
                element
        elem (OUT) - after updating the iterator to point to the previous
                element, address of the element is returned
        elemind (OUT) [optional] - address of the element's null indicator
                information is returned; if (elemind == NULL) then the null
                indicator information will NOT be returned
        boc (OUT) - TRUE if iterator is at Beginning Of Collection (i.e.
                previous element does not exist) else FALSE.
   DESCRIPTION:
        Returns pointer to the previous element and its corresponding null
        information. The iterator is updated to point to the previous element.

        If the iterator is pointing to the first element of the collection
        prior to executing this function, then calling this function will
        set 'boc' to TRUE. The iterator will be left unchanged in this
        situation.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
 *)
(*****************************************************************************
 *           FUNCTIONS WHICH OPERATE ONLY ON NESTED TABLE OCITable*()        *
 *****************************************************************************

 *---------------------------- OCITableSize ---------------------------------*)

  TOCITableSize = function(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                    size: psb4): sword; cdecl;
(*
   NAME: OCITableSize - OCITable return current SIZe of the given
                   nested table (not including deleted elements)
   PARAMETERS:
        env(IN) - pointer to OCI environment handle
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        tbl (IN) - nested table whose number of elements is returned
        size (OUT) - current number of elements in the nested table. The count
                does not include deleted elements.
   DESCRIPTION:
        Returns the count of elements in the given nested table.

        The count returned by OCITableSize() will be decremented upon
        deleting elements from the nested table. So, this count DOES NOT
        includes any "holes" created by deleting elements.
        For example:

            OCITableSize(...);
            // assume 'size' returned is equal to 5
            OCITableDelete(...); // delete one element
            OCITableSize(...);
            // 'size' returned will be equal to 4

        To get the count plus the count of deleted elements use
        OCICollSize(). Continuing the above example,

            OCICollSize(...)
            // 'size' returned will still be equal to 5
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          error during loading of nested table into object cache
          any of the input parameters is null
 *)

(*---------------------- OCITableExists ---------------------------------*)

  TOCITableExists = function(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                             index: sb4; exists: PBoolean): sword; cdecl;
(*
   NAME: OCITableExists - OCITable test whether element at the given index
                    EXIsts
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        tbl (IN) - table in which the given index is checked
        index (IN) - index of the element which is checked for existence
        exists (OUT) - set to TRUE if element at given 'index' exists
                else set to FALSE
   DESCRIPTION:
        Test whether an element exists at the given 'index'.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
 *)

(*--------------------------- OCITableDelete -------------------------------*)

  TOCITableDelete = function(hndl: POCIEnv; err: POCIError; index: sb4;
                      tbl: POCITable): sword; cdecl;
(*
   NAME: OCITableDelete - OCITable DELete element at the specified index
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        index (IN) - index of the element which must be deleted
        tbl (IN) - table whose element is deleted
   DESCRIPTION:
        Delete the element at the given 'index'. Note that the position
        ordinals of the remaining elements of the table is not changed by the
        delete operation. So delete creates "holes" in the table.

        An error is returned if the element at the specified 'index' has
        been previously deleted.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          any of the input parameters is null
          given index is not valid
 *)

(*--------------------------- OCITableFirst ---------------------------------*)

 TOCITableFirst = function(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                             index: sb4): sword; cdecl;
(*
   NAME: OCITableFirst - OCITable return FirST index of table
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        tbl (IN) - table which is scanned
        index (OUT) - first index of the element which exists in the given
                table is returned
   DESCRIPTION:
        Return the first index of the element which exists in the given
        table.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          table is empty
 *)

(*---------------------------- OCITableLast ---------------------------------*)

  TOCITableLast = function(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                             index: sb4): sword; cdecl;
(*
   NAME: OCITableFirst - OCITable return LaST index of table
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        tbl (IN) - table which is scanned
        index (OUT) - last index of the element which exists in the given
                table is returned
   DESCRIPTION:
        Return the last index of the element which exists in the given
        table.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          table is empty
 *)

(*---------------------------- OCITableNext ---------------------------------*)

  TOCITableNext = function(hndl: POCIEnv; err: POCIError; index: sb4;
                       const tbl: POCITable; next_index: psb4;
                       exists: PBoolean): sword; cdecl;
(*
   NAME: OCITableNext - OCITable return NeXT available index of table
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        index (IN) - starting at 'index' the index of the next element
                which exists is returned
        tbl (IN) - table which is scanned
        next_index (OUT) - index of the next element which exists
                is returned
        exists (OUT) - FALSE if no next index available else TRUE
   DESCRIPTION:
        Return the smallest position j, greater than 'index', such that
        exists(j) is TRUE.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          no next index available
 */

/*---------------------------- OCITablePrev ---------------------------------*)

  TOCITablePrev = function(hndl: POCIEnv; err: POCIError; index: sb4;
                       const tbl: POCITable; prev_index: psb4;
                       exists: PBoolean): sword; cdecl;
(*
   NAME: OCITablePrev - OCITable return PReVious available index of table
   PARAMETERS:
        env (IN/OUT) - OCI environment handle initialized in object mode.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        index (IN) - starting at 'index' the index of the previous element
                which exists is returned
        tbl (IN) - table which is scanned
        prev_index (OUT) - index of the previous element which exists
                is returned
        exists (OUT) - FALSE if no next index available else TRUE
   DESCRIPTION:
        Return the largest position j, less than 'index', such that
        exists(j) is TRUE.
   RETURNS:
        OCI_SUCCESS if the function completes successfully.
        OCI_INVALID_HANDLE if 'env' or 'err' is NULL.
        OCI_ERROR if
          no previous index available
 *)

(*---------------------------------orid.h------------------------------------*
 *                           PUBLIC FUNCTIONS                                *
 *---------------------------------------------------------------------------*)

(*-------------------------- OCIObjectSetAttr ----------------------------*)
  TOCIObjectSetAttr = function(env: POCIEnv; err: POCIError; instance: Pointer;
                  null_struct: pointer; tdo: POCIType; const names: PPAnsiChar;
                  const lengths: pub4; const name_count: ub4;
                  const indexes: pub4; const index_count: ub4;
                  const null_status: POCIInd; const attr_null_struct: Pointer;
                  const attr_value: Pointer): sword; cdecl;
(*
   NAME: OCIObjectSetAttr - ORID SET value
   PARAMETERS:
        env  (IN) - OCI environment handle initialized in object mode
        err  (IN) - error handle. If there is an error, it is
                        recorded in 'err' and this function returns OCI_ERROR.
                        The error recorded in 'err' can be retrieved by calling
                        OCIErrorGet().
        instance    (IN) - pointer to an ADT instance
        null_struct (IN) - the null structure of the ADT instance or array
        tdo         (IN) - pointer to the TDO
        names       (IN) - array of attribute names. This is used to specify
                           the names of the attributes in the path expression.
        lengths     (IN) - array of lengths of attribute names.
        name_count  (IN) - number of element in the array 'names'.
        indexes     (IN) [OPTIONAL] - currently NOT SUPPORTED, pass (ub4 * )0.
        index_count (IN) [OPTIONAL] - currently NOT SUPPORTED, pass (ub4)0.
        attr_null_status (IN) - the null status of the attribute if the type of
                                 attribute is primitive.
        attr_null_struct (IN) - the null structure of an ADT or collection
                                 attribute.
        attr_value       (IN) - pointer to the attribute value.
   REQUIRES:
   DESCRIPTION:
        This function set the attribute of the given object with the given
        value.  The position of the attribute is specified as a path
        expression which is an array of names and an array of indexes.
   RETURNS:
        one of OROSTA*
   EXAMPLES:
        For path expression stanford.cs.stu[5].addr, the arrays will look like
          names = {"stanford", "cs", "stu", "addr"}
          lengths = {8, 2, 3, 4}
          indexes = {5}

        Also see the above example.
 */

(*-------------------------- OCIObjectGetAttr ----------------------------*)
  TOCIObjectGetAttr = function(env: POCIEnv; err: POCIError; instance: Pointer;
                  null_struct: Pointer; tdo: POCIType;
                  const names: PPoratext; const lengths: pub4;
                  const name_count: ub4; const indexes: pub4;
                  const index_count: ub4; attr_null_status: PPOCIInd;
                  attr_null_struct, attr_value: PPointer;
                  attr_tdo: PPOCIType): sword; cdecl;
(*
   NAME: OCIObjectGetAttr - ORID GET value
   PARAMETERS:
        env  (IN) - OCI environment handle initialized in object mode
        err  (IN) - error handle. If there is an error, it is
                        recorded in 'err' and this function returns OCI_ERROR.
                        The error recorded in 'err' can be retrieved by calling
                        OCIErrorGet().
        instance    (IN) - pointer to an ADT instance
        null_struct (IN) - the null structure of the ADT instance or array
        tdo         (IN) - pointer to the TDO
        names       (IN) - array of attribute names. This is used to specify
                           the names of the attributes in the path expression.
        lengths     (IN) - array of lengths of attribute names.
        name_count  (IN) - number of element in the array 'names'.
        indexes     (IN) [OPTIONAL] - currently NOT SUPPORTED, pass (ub4 * )0.
        index_count (IN) [OPTIONAL] - currently NOT SUPPORTED, pass (ub4)0.
        attr_null_status (OUT) - the null status of the attribute if the type
                                 of attribute is primitive.
        attr_null_struct (OUT) - the null structure of an ADT or collection
                                 attribute.
        attr_value       (OUT) - pointer to the attribute value.
        attr_tdo         (OUT) - pointer to the TDO of the attribute.
   REQUIRES:
      - a valid OCI environment handle must be given.
   DESCRIPTION:
        This function gets a value from an ADT instance or from an array.
        If the parameter 'instance' points to an ADT instance, then the path
        expression specifies the location of the attribute in the ADT.
        It is assumed that the object is pinned and that the value returned
        is valid until the object is unpinned.
   RETURNS:
        one of OROSTA*
   EXAMPLES:
        See example in OCIObjectSetAttr(). Also see the above example.
 *)

  {--------------------------------ori.h-------------------------------------- }
  {                       PUBLIC TYPES AND CONSTANTS                           }
  {--------------------------------------------------------------------------- }
  { Also see oro.h.  }
  {--------------------------------------------------------------------------- }
  {                           PUBLIC FUNCTIONS                                 }
  {--------------------------------------------------------------------------- }
  {--------------------------------------------------------------------------- }
  {                       OBJECT/INSTANCE OPERATIONS                           }
  {--------------------------------------------------------------------------- }
  {--------------------------- OCIObjectNew ---------------------------------- }
  {--------------------------- OCIObjectPin ---------------------------------- }

  {---------------------------- OCIObjectPinCountReset ----------------------- }
  TOCIObjectPinCountReset = function (env: POCIEnv; err: POCIError;
    const _object: pointer): sword; cdecl;
  {
     NAME: OCIObjectPinCountReset - OCI resets the pin count of a referenceable
                                    object
     PARAMETERS:
          env   (IN/OUT) - OCI environment handle initialized in object mode
          err   (IN/OUT) - error handle. If there is an error, it is
                           recorded in 'err' and this function returns OCI_ERROR.
                           The error recorded in 'err' can be retrieved by
                           calling OCIErrorGet().
          object    (IN) - pointer to an object
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified object must be pinned.
     DESCRIPTION:
          This function completely unpins an object.  When an object is
          completely unpinned, it can be freed without error.

          FOR PERSISTENT OBJECTS:
          When a persistent object is completely unpinned, it becomes a candidate
          for aging. The memory of an object is freed when it is aged out. Aging
          is used to maximize the utilization of memory.  An dirty object cannot
          be aged out unless it is flushed.

          FOR TRANSIENT OBJECTS:
          The pin count of the object is decremented. A transient can be freed
          only at the end of its allocation duration or when it is explicitly
          freed by calling OCIObjectFree().

          FOR VALUE:
          This function will return an error for value.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {--------------------------- OCIObjectLock --------------------------------- }
  TOCIObjectLock = function(env: POCIEnv; err: POCIError;
    const _object: pointer): sword; cdecl;

  {
     NAME: OCIObjectLock - OCI lock a persistent object
     PARAMETERS:
          env   (IN/OUT) - OCI environment handle initialized in object mode
          err   (IN/OUT) - error handle. If there is an error, it is
                           recorded in 'err' and this function returns OCI_ERROR.
                           The error recorded in 'err' can be retrieved by
                           calling OCIErrorGet().
          object    (IN) - pointer to the persistent object
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified object must be pinned.
     DESCRIPTION:
          This function locks a persistent object at the server. Unlike
          OCIObjectLockNoWait() this function waits if another user currently
          holds a lock on the desired object. This function
          returns an error if:
            1) the object is non-existent.

          This function will return an error for transient objects and values.
          The lock of an object is released at the end of a transaction.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
   }
  {------------------------ OCIObjectLockNoWait ------------------------------ }
  TOCIObjectLockNoWait = function(env: POCIEnv; err: POCIError;
    const _object: pointer): sword; cdecl;

  {
     NAME: OCIObjectLockNoWait - OCI lock a persistent object, do not wait for
                                 the lock, return error if lock not available
     PARAMETERS:
          env   (IN/OUT) - OCI environment handle initialized in object mode
          err   (IN/OUT) - error handle. If there is an error, it is
                           recorded in 'err' and this function returns OCI_ERROR.
                           The error recorded in 'err' can be retrieved by
                           calling OCIErrorGet().
          object    (IN) - pointer to the persistent object
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified object must be pinned.
     DESCRIPTION:
          This function locks a persistent object at the server. Unlike
          OCIObjectLock() this function will not wait if another user holds
          the lock on the desired object. This function returns an error if:
            1) the object is non-existent.
            2) the object is currently locked by another user in which
               case this function returns with an error.

          This function will return an error for transient objects and values.
          The lock of an object is released at the end of a transaction.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
   }
  {--------------------------- OCIObjectMarkUpdate --------------------------- }
  TOCIObjectMarkUpdate = function(env: POCIEnv; err: POCIError;
    const _object: pointer): sword; cdecl;

  {
     NAME: OCIObjectMarkUpdate - OCI marks an object as updated
     PARAMETERS:
          env   (IN/OUT) - OCI environment handle initialized in object mode
          err   (IN/OUT) - error handle. If there is an error, it is
                           recorded in 'err' and this function returns OCI_ERROR.
                           The error recorded in 'err' can be retrieved by
                           calling OCIErrorGet().
          object    (IN) - pointer to the persistent object
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified object must be pinned.
     DESCRIPTION:
          FOR PERSISTENT OBJECTS:
          This function marks the specified persistent object as updated. The
          persistent objects will be written to the server when the object cache
          is flushed.  The object is not locked or flushed by this function. It
          is an error to update a deleted object.

          After an object is marked updated and flushed, this function must be
          called again to mark the object as updated if it has been dirtied
          after it is being flushed.

          FOR TRANSIENT OBJECTS:
          This function marks the specified transient object as updated. The
          transient objects will NOT be written to the server. It is an error
          to update a deleted object.

          FOR VALUES:
          It is an no-op for values.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {----------------------------- OCIObjectUnmark ----------------------------- }
  TOCIObjectUnmark = function(env: POCIEnv; err: POCIError;
    const _object:pointer): sword; cdecl;

  {
     NAME: OCIObjectUnmark - OCI unmarks an object
     PARAMETERS:
          env   (IN/OUT) - OCI environment handle initialized in object mode
          err   (IN/OUT) - error handle. If there is an error, it is
                           recorded in 'err' and this function returns OCI_ERROR.
                           The error recorded in 'err' can be retrieved by
                           calling OCIErrorGet().
          object    (IN) - pointer to the persistent object
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified object must be pinned.
     DESCRIPTION:
          FOR PERSISTENT OBJECTS AND TRANSIENT OBJECTS:
          This function unmarks the specified persistent object as dirty. Changes
          that are made to the object will not be written to the server. If the
          object is marked locked, it remains marked locked.  The changes that
          have already made to the object will not be undone implicitly.

          FOR VALUES:
          It is an no-op for values.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {----------------------------- OCIObjectUnmarkByRef ------------------------ }
  TOCIObjectUnmarkByRef = function(env: POCIEnv; err: POCIError;
    const ref: POCIRef): sword; cdecl;

  {
     NAME: OCIObjectUnmarkByRef - OCI unmarks an object by Ref
     PARAMETERS:
          env   (IN/OUT) - OCI environment handle initialized in object mode
          err   (IN/OUT) - error handle. If there is an error, it is
                           recorded in 'err' and this function returns OCI_ERROR.
                           The error recorded in 'err' can be retrieved by
                           calling OCIErrorGet().
          ref   (IN) - reference of the object
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified object must be pinned.
     DESCRIPTION:
          FOR PERSISTENT OBJECTS AND TRANSIENT OBJECTS:
          This function unmarks the specified persistent object as dirty. Changes
          that are made to the object will not be written to the server. If the
          object is marked locked, it remains marked locked.  The changes that
          have already made to the object will not be undone implicitly.

          FOR VALUES:
          It is an no-op for values.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {----------------------- OCIObjectMarkDeleteByRef -------------------------- }
  TOCIObjectMarkDeleteByRef = function(env: POCIEnv; err: POCIError;
    const object_ref:POCIRef): sword; cdecl;

  {
     NAME: OCIObjectMarkDeleteByRef - OCI "delete" (and unpin) an object given
                                      a reference
     PARAMETERS:
          env     (IN/OUT) - OCI environment handle initialized in object mode
          err     (IN/OUT) - error handle. If there is an error, it is
                             recorded in 'err' and this function returns
                             OCI_ERROR.  The error recorded in 'err' can be
                             retrieved by calling OCIErrorGet().
          object_ref  (IN) - ref of the object to be deleted

     REQUIRES:
          - a valid OCI environment handle must be given.
     DESCRIPTION:
          This function marks the object designated by 'object_ref' as deleted.

          FOR PERSISTENT OBJECTS:
          If the object is not loaded, then a temporary object is created and is
          marked deleted. Otherwise, the object is marked deleted.

          The object is deleted in the server when the object is flushed.

          FOR TRANSIENT OBJECTS:
          The object is marked deleted.  The object is not freed until it is
          unpinned.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {--------------------------- OCIObjectMarkDelete --------------------------- }
  TOCIObjectMarkDelete = function(env: POCIEnv; err: POCIError;
    const instance:pointer): sword; cdecl;

  {
     NAME: OCIObjectMarkDelete - OCI "delete" an instance given a Pointer
     PARAMETERS:
          env    (IN/OUT) - OCI environment handle initialized in object mode
          err    (IN/OUT) - error handle. If there is an error, it is
                            recorded in 'err' and this function returns
                            OCI_ERROR.  The error recorded in 'err' can be
                            retrieved by calling OCIErrorGet().
          instance   (IN) - pointer to the instance
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The instance must be standalone.
          - If the instance is a referenceable object, then it must be pinned.
     DESCRIPTION:

          FOR PERSISTENT OBJECTS:
          The object is marked deleted.  The memory of the object is not freed.
          The object is deleted in the server when the object is flushed.

          FOR TRANSIENT OBJECTS:
          The object is marked deleted.  The memory of the object is not freed.

          FOR VALUES:
          This function frees a value immediately.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {---------------------------- OCIObjectFlush ------------------------------- }
  TOCIObjectFlush = function(env: POCIEnv; err: POCIError;
    const _object: pointer): sword; cdecl;

  {
     NAME: OCIObjectFlush - OCI flush a persistent object
     PARAMETERS:
          env    (IN/OUT) - OCI environment handle initialized in object mode
          err    (IN/OUT) - error handle. If there is an error, it is
                            recorded in 'err' and this function returns
                            OCI_ERROR.  The error recorded in 'err' can be
                            retrieved by calling OCIErrorGet().
          object     (IN) - pointer to the persistent object
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified object must be pinned.
     DESCRIPTION:
          This function flushes a modified persistent object to the server.
          An exclusive lock is obtained implicitly for the object when flushed.

          When the object is written to the server, triggers may be fired.
          Objects can be modified by the triggers at the server.  To keep the
          objects in the object cache being coherent with the database, the
          clients can free or refresh the objects in the cache.

          This function will return an error for transient objects and values.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {------------------------ OCIObjectRefresh --------------------------------- }
  TOCIObjectRefresh = function(env: POCIEnv; err: POCIError;
    _object: pointer): sword; cdecl;

  {
     NAME: OCIObjectRefresh - OCI refresh a persistent object
     PARAMETERS:
          env    (IN/OUT) - OCI environment handle initialized in object mode
          err    (IN/OUT) - error handle. If there is an error, it is
                            recorded in 'err' and this function returns
                            OCI_ERROR.  The error recorded in 'err' can be
                            retrieved by calling OCIErrorGet().
          object     (IN) - pointer to the persistent object
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified object must be pinned.
     DESCRIPTION:
          This function refreshes an unmarked object with data retrieved from the
          latest snapshot in the server. An object should be refreshed when the
          objects in the cache are inconsistent with the objects at
          the server:
          1) When an object is flushed to the server, triggers can be fired to
             modify more objects in the server.  The same objects (modified by
             the triggers) in the object cache become obsolete.
          2) When the user issues a SQL or executes a PL/SQL procedure to modify
             any object in the server, the same object in the cache becomes
             obsolete.

          The object that is refreshed will be 'replaced-in-place'. When an
          object is 'replaced-in-place', the top level memory of the object will
          be reused so that new data can be loaded into the same memory address.
          The top level memory of the null structre is also reused. Unlike the
          top level memory chunk, the secondary memory chunks may be resized and
          reallocated.  The client should be careful when holding onto a pointer
          to the secondary memory chunk (e.g. assigning the address of a
          secondary memory to a local variable), since this pointer can become
          invalid after the object is refreshed.

          The object state will be modified as followed after being refreshed:
            - existent : set to appropriate value
            - pinned   : unchanged
            - allocation duration : unchanged
            - pin duration : unchanged

          This function is an no-op for transient objects or values.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {---------------------------- OCIObjectCopy -------------------------------- }
  TOCIObjectCopy = function(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
    const source, null_source, target, null_target: pointer; const tdo: POCIType;
    const duration: OCIDuration; const option: ub1): sword; cdecl;

  {
     NAME: OCIObjectCopy - OCI copy one instance to another
     PARAMETERS:
          env     (IN/OUT) - OCI environment handle initialized in object mode
          err     (IN/OUT) - error handle. If there is an error, it is
                             recorded in 'err' and this function returns
                             OCI_ERROR.  The error recorded in 'err' can be
                             retrieved by calling OCIErrorGet().
          svc         (IN) - OCI service context handle
          source      (IN) - pointer to the source instance
          null_source (IN) - pointer to the null structure of the source
          target      (IN) - pointer to the target instance
          null_target (IN) - pointer to the null structure of the target
          tdo         (IN) - the TDO for both source and target
          duration    (IN) - allocation duration of the target memory
          option      (IN) - specify the copy option:
                          OROOCOSFN - Set Reference to Null. All references
                          in the source will not be copied to the target. The
                          references in the target are set to null.
     REQUIRES:
          - a valid OCI environment handle must be given.
          - If source or target is referenceable, it must be pinned.
          - The target or the containing instance of the target must be already
            be instantiated (e.g. created by OCIObjectNew()).
          - The source and target instances must be of the same type. If the
            source and target are located in a different databases, then the
            same type must exist in both databases.
     DESCRIPTION:
          This function copies the contents of the 'source' instance to the
          'target' instance. This function performs a deep-copy such that the
          data that is copied/duplicated include:
          a) all the top level attributes (see the exceptions below)
          b) all the secondary memory (of the source) that is reachable from the
             top level attributes.
          c) the null structure of the instance

          Memory is allocated with the specified allocation duration.

          Certain data items are not copied:
          a) If the option OCI_OBJECTCOPY_NOREF is specified, then all references
             in the source are not copied. Instead, the references in the target
             are set to null.
          b) If the attribute is a LOB, then it is set to null.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {--------------------------- OCIObjectGetObjectRef ------------------------- }
  TOCIObjectGetObjectRef = function(env: POCIEnv; err: POCIError;
    const _object: pointer; bject_ref: POCIRef): sword; cdecl;

  {
     NAME: OCIObjectGetObjectRef - OCI get the object reference of an
                                   referenceable object
     PARAMETERS:
          env     (IN/OUT) - OCI environment handle initialized in object mode
          err     (IN/OUT) - error handle. If there is an error, it is
                             recorded in 'err' and this function returns
                             OCI_ERROR.  The error recorded in 'err' can be
                             retrieved by calling OCIErrorGet().
          object      (IN) - pointer to a persistent object
          object_ref (OUT) - reference of the given object. The reference must
                             already be allocated.
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified object must be pinned.
          - The reference must already be allocated.
     DESCRIPTION:
          This function returns a reference to the given object.  It returns an
          error for values.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {--------------------------- OCIObjectMakeObjectRef ----------------------- }
  TOCIObjectMakeObjectRef = function(env: POCIEnv; err: POCIError;
    const svc: POCISvcCtx; const table: pointer; const values: PPointer;
    const array_len: ub4; object_ref: POCIRef): sword; cdecl;

  {
     NAME: OCIObjectMakeObjectRef - OCI Create an object reference to a
                                   referenceable object.
     PARAMETERS:
          env     (IN/OUT) - OCI environment handle initialized in object mode
          err     (IN/OUT) - error handle. If there is an error, it is
                             recorded in 'err' and this function returns
                             OCI_ERROR.  The error recorded in 'err' can be
                             retrieved by calling OCIErrorGet().
          svc         (IN) - the service context
          table       (IN) - A pointer to the table object (must be pinned)
          attrlist    (IN) - A list of values (OCI type values) from which
                             the ref is to be created.
          attrcnt     (IN)  - The length of the attrlist array.
          object_ref (OUT) - reference of the given object. The reference must
                             already be allocated.
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified table object must be pinned.
          - The reference must already be allocated.
     DESCRIPTION:
          This function creates a reference given the values that make up the
          reference and also a pointer to the table object.
          Based on the table's OID property, whether it is a pk based OID or
          a system generated OID, the function creates a sys-generated REF or
          a pk based REF.
          In case of system generated REFs pass in a OCIRaw which is 16 bytes
          long contatining the sys generated OID.
          In case of PK refs pass in the OCI equivalent for numbers, chars etc..
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {--------------------------- OCIObjectGetPrimaryKeyTypeRef ---------------  }
  TOCIObjectGetPrimaryKeyTypeRef = function(env: POCIEnv; err: POCIError;
    const svc:POCISvcCtx; const table: pointer;
    type_ref: POCIRef): sword; cdecl;

  {
     NAME: OCIObjectGetPrimaryKeyTypeRef - OCI get the REF to the pk OID type
     PARAMETERS:
          env     (IN/OUT) - OCI environment handle initialized in object mode
          err     (IN/OUT) - error handle. If there is an error, it is
                             recorded in 'err' and this function returns
                             OCI_ERROR.  The error recorded in 'err' can be
                             retrieved by calling OCIErrorGet().
          svc     (IN)     - the service context
          table   (IN)     - pointer to the table object
          type_ref   (OUT) - reference of the pk type. The reference must
                             already be allocated.
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The specified table object must be pinned.
          - The reference must already be allocated.
     DESCRIPTION:
          This function returns a reference to the pk type.  It returns an
          error for values.  If the table is not a Pk oid table/view, then
          it returns error.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {-------------------------- OCIObjectGetInd -------------------------------- }
  TOCIObjectGetInd = function(env: POCIEnv; err: POCIError;
    const instance: pointer; null_struct: PPointer): sword; cdecl;

  {
     NAME: OCIObjectGetInd - OCI get the null structure of a standalone object
     PARAMETERS:
          env     (IN/OUT) - OCI environment handle initialized in object mode
          err     (IN/OUT) - error handle. If there is an error, it is
                             recorded in 'err' and this function returns
                             OCI_ERROR.  The error recorded in 'err' can be
                             retrieved by calling OCIErrorGet().
          instance      (IN) - pointer to the instance
          null_struct (OUT) - null structure
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The object must be standalone.
          - If the object is referenceable, the specified object must be pinned.
     DESCRIPTION:
          This function returns the null structure of an instance. This function
          will allocate the top level memory of the null structure if it is not
          already allocated. If an null structure cannot be allocated for the
          instance, then an error is returned. This function only works for
          ADT or row type instance.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {------------------------- OCIObjectExists -------------------------------- }
  TOCIObjectExists = function(env: POCIEnv; err: POCIError; const ins: pointer;
    exist: PBoolean): sword; cdecl;

  {
     NAME: OCIObjectExist - OCI checks if the object exists
     PARAMETERS:
          env       (IN/OUT) - OCI environment handle initialized in object mode
          err       (IN/OUT) - error handle. If there is an error, it is
                               recorded in 'err' and this function returns
                               OCI_ERROR.  The error recorded in 'err' can be
                               retrieved by calling OCIErrorGet().
          ins           (IN) - pointer to an instance
          exist        (OUT) - return TRUE if the object exists
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The object must be standalone.
          - if object is a referenceable, it must be pinned.
     DESCRIPTION:
          This function returns the existence of an instance. If the instance
          is a value, this function always returns TRUE.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {------------------------- OCIObjectGetProperty --------------------------- }
  TOCIObjectGetProperty = function(envh: POCIEnv; errh: POCIError;
    const obj: pointer; const propertyId: OCIObjectPropId;
    _property: pointer; size: Pub4): sword; cdecl;

  {
     NAME: OCIObjectGetProperty - OCIObject Get Property of given object
     PARAMETERS:
          env       (IN/OUT) - OCI environment handle initialized in object mode
          err       (IN/OUT) - error handle. If there is an error, it is
                               recorded in 'err' and this function returns
                               OCI_ERROR.  The error recorded in 'err' can be
                               retrieved by calling OCIErrorGet().
          obj           (IN) - object whose property is returned
          propertyId    (IN) - id which identifies the desired property
          property     (OUT) - buffer into which the desired property is
                               copied
          size      (IN/OUT) - on input specifies the size of the property buffer
                               passed by caller, on output will contain the
                               size in bytes of the property returned.
                               This parameter is required for string type
                               properties only (e.g OCI_OBJECTPROP_SCHEMA,
                               OCI_OBJECTPROP_TABLE). For non-string
                               properties this parameter is ignored since
                               the size is fixed.
     DESCRIPTION:
          This function returns the specified property of the object.
          The desired property is identified by 'propertyId'. The property
          value is copied into 'property' and for string typed properties
          the string size is returned via 'size'.

          Objects are classified as persistent, transient and value
          depending upon the lifetime and referenceability of the object.
          Some of the properties are applicable only to persistent
          objects and some others only apply to persistent and
          transient objects. An error is returned if the user tries to
          get a property which in not applicable to the given object.
          To avoid such an error, the user should first check whether
          the object is persistent or transient or value
          (OCI_OBJECTPROP_LIFETIME property) and then appropriately
          query for other properties.

          The different property ids and the corresponding type of
          'property' argument is given below.

            OCI_OBJECTPROP_LIFETIME
              This identifies whether the given object is a persistent
              object (OCI_OBJECT_PERSISTENT) or a
              transient object (OCI_OBJECT_TRANSIENT) or a
              value instance (OCI_OBJECT_VALUE).
              'property' argument must be a pointer to a variable of
              type OCIObjectLifetime.

            OCI_OBJECTPROP_SCHEMA
              This returns the schema name of the table in which the
              object exists. An error is returned if the given object
              points to a transient instance or a value. If the input
              buffer is not big enough to hold the schema name an error
              is returned, the error message will communicate the
              required size. Upon success, the size of the returned
              schema name in bytes is returned via 'size'.
              'property' argument must be an array of type text and 'size'
              should be set to size of array in bytes by the caller.

            OCI_OBJECTPROP_TABLE
              This returns the table name in which the object exists. An
              error is returned if the given object points to a
              transient instance or a value. If the input buffer is not
              big enough to hold the table name an error is returned,
              the error message will communicate the required size. Upon
              success, the size of the returned table name in bytes is
              returned via 'size'. 'property' argument must be an array
              of type text and 'size' should be set to size of array in
              bytes by the caller.

            OCI_OBJECTPROP_PIN_DURATION
              This returns the pin duration of the object.
              An error is returned if the given object points to a value
              instance. Valid pin durations are: OCI_DURATION_SESSION and
              OCI_DURATION_TRANS.
              'property' argument must be a pointer to a variable of type
              OCIDuration.

            OCI_OBJECTPROP_ALLOC_DURATION
              This returns the allocation duration of the object.
              Valid allocation durations are: OCI_DURATION_SESSION and
              OCI_DURATION_TRANS.
              'property' argument must be a pointer to a variable of type
              OCIDuration.

            OCI_OBJECTPROP_LOCK
              This returns the lock status of the
              object. The possible lock status is enumerated by OCILockOpt.
              An error is returned if the given object points to a transient
              or value instance.
              'property' argument must be a pointer to a variable of
              type OCILockOpt.
              Note, the lock status of an object can also be retrieved by
              calling OCIObjectIsLocked().

            OCI_OBJECTPROP_MARKSTATUS
              This returns the status flag which indicates whether the
              object is a new object, updated object and/or deleted object.
              The following macros can be used to test the mark status
              flag:

                OCI_OBJECT_IS_UPDATED(flag)
                OCI_OBJECT_IS_DELETED(flag)
                OCI_OBJECT_IS_NEW(flag)
                OCI_OBJECT_IS_DIRTY(flag)

              An object is dirty if it is a new object or marked deleted or
              marked updated.
              An error is returned if the given object points to a transient
              or value instance. 'property' argument must be of type
              OCIObjectMarkStatus.

            OCI_OBJECTPROP_VIEW
              This identifies whether the specified object is a view object
              or not. If property value returned is TRUE, it indicates the
              object is a view otherwise it is not.
              'property' argument must be of type boolean.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR. Possible errors are TBD
    }
  {---------------------------- OCIObjectIsLocked -------------------------- }
  TOCIObjectIsLocked = function(env: POCIEnv; err: POCIError; const ins: pointer;
    lock: Pboolean): sword; cdecl;

  {
     NAME: OCIObjectIsLocked - OCI get the lock status of a standalone object
     PARAMETERS:
          env       (IN/OUT) - OCI environment handle initialized in object mode
          err       (IN/OUT) - error handle. If there is an error, it is
                               recorded in 'err' and this function returns
                               OCI_ERROR.  The error recorded in 'err' can be
                               retrieved by calling OCIErrorGet().
          ins           (IN) - pointer to an instance
          lock         (OUT) - return value for the lock status.
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The instance must be standalone.
          - If the object is referenceable, the specified object must be pinned.
     DESCRIPTION:
          This function returns the lock status of an instance. If the instance
          is a value, this function always returns FALSE.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {------------------------- OCIObjectIsDirty ------------------------------ }
  TOCIObjectIsDirty = function(env: POCIEnv; err: POCIError; const ins: pointer;
    dirty:Pboolean): sword; cdecl;

  {
     NAME: OCIObjectIsDirty - OCI get the dirty status of a standalone object
     PARAMETERS:
          env       (IN/OUT) - OCI environment handle initialized in object mode
          err       (IN/OUT) - error handle. If there is an error, it is
                               recorded in 'err' and this function returns
                               OCI_ERROR.  The error recorded in 'err' can be
                               retrieved by calling OCIErrorGet().
          ins           (IN) - pointer to an instance
          dirty        (OUT) - return value for the dirty status.
     REQUIRES:
          - a valid OCI environment handle must be given.
          - The instance must be standalone.
          - if instance is an object, the instance must be pinned.
     DESCRIPTION:
          This function returns the dirty status of an instance. If the instance
          is a value, this function always returns FALSE.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {--------------------------- OCIObjectPinTable ----------------------------- }
  TOCIObjectPinTable = function(env: POCIEnv; err: POCIError;
    const svc:POCISvcCtx; const schema_name: Poratext; const s_n_length: ub4;
    const object_name: Poratext; const o_n_length:ub4;
    const scope_obj_ref: POCIRef; const pin_duration: OCIDuration;
    _object: PPointer): sword; cdecl;

  {
     NAME: OCIObjectPinTable - OCI get table object
     PARAMETERS:
          env       (IN/OUT) - OCI environment handle initialized in object mode
          err       (IN/OUT) - error handle. If there is an error, it is
                               recorded in 'err' and this function returns
                               OCI_ERROR.  The error recorded in 'err' can be
                               retrieved by calling OCIErrorGet().
          svc                     (IN) - OCI service context handle
          schema_name   (IN, optional) - schema name of the table
          s_n_length    (IN, optional) - length of the schema name
          object_name   (IN) - name of the table
          o_n_length    (IN) - length of the table name
          scope_obj_ref (IN, optional) - reference of the scoping object
          pin_duration  (IN) - pin duration. See description in OCIObjectPin().
          object       (OUT) - the pinned table object
     REQUIRES:
          - a valid OCI environment handle must be given.
     DESCRIPTION:
          This function pin a table object with the specified pin duration.
          The client can unpin the object by calling OCIObjectUnpin(). See
          OCIObjectPin() and OCIObjectUnpin() for more information about pinning
          and unpinning.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {------------------------- OCIObjectArrayPin ------------------------------- }
  TOCIObjectArrayPin = function(env: POCIEnv; err: POCIError;
    const ref_array: PPOCIRef; const array_size: ub4;
    const cor_array: PPOCIComplexObject; const cor_array_size: ub4;
    const pin_option: OCIPinOpt; const pin_duration: OCIDuration;
    const lock: OCILockOpt; obj_array: PPointer;
    pos: Pub4): sword; cdecl;

  {
     NAME: OCIObjectArrayPin - ORIO array pin
     PARAMETERS:
          env       (IN/OUT) - OCI environment handle initialized in object mode
          err       (IN/OUT) - error handle. If there is an error, it is
                               recorded in 'err' and this function returns
                               OCI_ERROR.  The error recorded in 'err' can be
                               retrieved by calling OCIErrorGet().
          ref_array     (IN) - array of references to be pinned
          array_size    (IN) - number of elements in the array of references
          pin_option    (IN) - pin option. See OCIObjectPin().
          pin_duration  (IN) - pin duration. See OCIObjectPin().
          lock_option   (IN) - lock option. See OCIObjectPin().
          obj_array    (OUT) - If this argument is not NULL, the pinned objects
                               will be returned in the array. The user must
                               allocate this array with element type being
                               'void *'. The size of this array is identical to
                               'array'.
          pos          (OUT) - If there is an error, this argument will contain
                               the element that is causing the error.  Note that
                               this argument is set to 1 for the first element in
                               the ref_array.
     REQUIRE:
          - a valid OCI environment handle must be given.
          - If 'obj_array' is not NULL, then it must already be allocated and
               the size of 'obj_array' is 'array_size'.
     DESCRIPTION:
          This function pin an array of references.  All the pinned objects are
          retrieved from the database in one network roundtrip.  If the user
          specifies an output array ('obj_array'), then the address of the
          pinned objects will be assigned to the elements in the array. See
          OCIObjectPin() for more information about pinning.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {--------------------------------------------------------------------------- }
  {                           HEAP/CACHE OPERATIONS                            }
  {--------------------------------------------------------------------------- }
  {--------------------------- OCICacheFlush --------------------------------- }
  TOCICacheFlushGet = function(context: pointer; last: Pub1): POCIRef; cdecl;
  TOCICacheFlush = function(env: POCIEnv; err: POCIError; const svc:POCISvcCtx;
    const context: pointer; const get: TOCICacheFlushGet;
    ref: PPOCIRef): sword; cdecl;

  {
     NAME: OCICacheFlush - OCI flush persistent objects
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                        recorded in 'err' and this function returns
                        OCI_ERROR.  The error recorded in 'err' can be
                        retrieved by calling OCIErrorGet().
          svc      (IN) [optional] - OCI service context.  If null pointer is
                        specified, then the dirty objects in all connections
                        will be flushed.
          context  (IN) [optional] - specifies an user context that is an
                        argument to the client callback function 'get'. This
                        parameter is set to NULL if there is no user context.
          get      (IN) [optional] - an client-defined function which acts an
                        iterator to retrieve a batch of dirty objects that need
                        to be flushed. If the function is not NULL, this function
                        will be called to get a reference of a dirty object.
                        This is repeated until a null reference is returned by
                        the client function or the parameter 'last' is set to
                        TRUE. The parameter 'context' is passed to get()
                        for each invocation of the client function.  This
                        parameter should be NULL if user callback is not given.
                        If the object that is returned by the client function is
                        not a dirtied persistent object, the object is ignored.
                        All the objects that are returned from the client
                        function must be from newed or pinned the same service
                        context, otherwise, an error is signalled. Note that the
                        returned objects are flushed in the order in which they
                        are marked dirty.
          ref     (OUT) [optional] - if there is an error in flushing the
                        objects, (*ref) will point to the object that
                        is causing the error.  If 'ref' is NULL, then the object
                        will not be returned.  If '*ref' is NULL, then a
                        reference will be allocated and set to point to the
                        object.  If '*ref' is not NULL, then the reference of
                        the object is copied into the given space. If the
                        error is not caused by any of the dirtied object,
                        the given ref is initalized to be a NULL reference
                        (OCIRefIsNull(*ref) is TRUE).
     REQUIRES:
          - a valid OCI environment handle must be given.
     DESCRIPTION:
          This function flushes the modified persistent objects from the
          environment heap to the server. The objects are flushed in the order
          that they are marked updated or deleted.

          See OCIObjectFlush() for more information about flushing.

     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {--------------------------- OCICacheRefresh ------------------------------- }
  TOCICacheRefreshGet = function(context: pointer): POCIRef; cdecl;
  TOCICacheRefresh = function(env: POCIEnv; err: POCIError;
    const svc: POCISvcCtx; const option: OCIRefreshOpt; const context: pointer;
    get: TOCICacheRefreshGet; ref: PPOCIRef): sword; cdecl;

  {
     NAME: OCICacheRefresh - OCI ReFreSh persistent objects
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                         recorded in 'err' and this function returns
                         OCI_ERROR.  The error recorded in 'err' can be
                         retrieved by calling OCIErrorGet().
          svc     (IN) [optional] - OCI service context.  If null pointer is
                        specified, then the persistent objects in all connections
                        will be refreshed.
          option   (IN) [optional] - if OCI_REFRESH_LOAD is specified, all
                        objects that is loaded within the transaction are
                        refreshed. If the option is OCI_REFERSH_LOAD and the
                        parameter 'get' is not NULL, this function will ignore
                        the parameter.
          context  (IN) [optional] - specifies an user context that is an
                        argument to the client callback function 'get'. This
                        parameter is set to NULL if there is no user context.
          get      (IN) [optional] - an client-defined function which acts an
                        iterator to retrieve a batch of objects that need to be
                        refreshed. If the function is not NULL, this function
                        will be called to get a reference of an object.  If
                        the reference is not NULL, then the object will be
                        refreshed.  These steps are repeated until a null
                        reference is returned by this function.  The parameter
                        'context' is passed to get() for each invocation of the
                        client function.  This parameter should be NULL if user
                        callback is not given.
          ref     (OUT) [optional] - if there is an error in refreshing the
                        objects, (*ref) will point to the object that
                        is causing the error.  If 'ref' is NULL, then the object
                        will not be returned.  If '*ref' is NULL, then a
                        reference will be allocated and set to point to the
                        object.  If '*ref' is not NULL, then the reference of
                        the object is copied into the given space. If the
                        error is not caused by any of the object,
                        the given ref is initalized to be a NULL reference
                        (OCIRefIsNull(*ref) is TRUE).
     REQUIRES:
          - a valid OCI environment handle must be given.
     DESCRIPTION:
          This function refreshes all pinned persistent objects. All unpinned
          persistent objects are freed.  See OCIObjectRefresh() for more
          information about refreshing.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {---------------------------- OCICacheUnpin -------------------------------- }
  TOCICacheUnpin = function(env: POCIEnv; err: POCIError;
    const svc:POCISvcCtx): sword; cdecl;

  {
     NAME: OCICacheUnpin - OCI UNPin objects
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                         recorded in 'err' and this function returns
                         OCI_ERROR.  The error recorded in 'err' can be
                         retrieved by calling OCIErrorGet().
          svc     (IN) [optional] - OCI service context. If null pointer is
                         specified, then the objects in all connections
                         will be unpinned.
     REQUIRES:
          - a valid OCI environment handle must be given.
     DESCRIPTION:
          If a connection is specified, this function completely unpins the
          persistent objects in that connection. Otherwise, all persistent
          objects in the heap are completely unpinned. All transient objects in
          the heap are also completely unpinned. See OCIObjectUnpin() for more
          information about unpinning.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  {----------------------------- OCICacheFree -------------------------------- }
  TOCICacheFree = function(env: POCIEnv; err: POCIError;
    const svc: POCISvcCtx): sword; cdecl;

  {
     NAME: OCICacheFree - OCI FREe instances
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                         recorded in 'err' and this function returns
                         OCI_ERROR.  The error recorded in 'err' can be
                         retrieved by calling OCIErrorGet().
          svc     (IN) [optional] - OCI service context. If null pointer is
                         specified, then the objects in all connections
                         will be freed.
     REQUIRES:
          - a valid OCI environment handle must be given.
     DESCRIPTION:
          If a connection is specified, this function frees the persistent
          objects, transient objects and values allocated for that connection.
          Otherwise, all persistent objects, transient objects and values in the
          heap are freed. Objects are freed regardless of their pin count.  See
          OCIObjectFree() for more information about freeing an instance.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
   }
  {---------------------------- OCICacheUnmark ------------------------------- }
  TOCICacheUnmark = function(env: POCIEnv; err: POCIError;
    const svc: POCISvcCtx): sword; cdecl;

  {
     NAME: OCICacheUnmark - OCI Unmark all dirty objects
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                         recorded in 'err' and this function returns
                         OCI_ERROR.  The error recorded in 'err' can be
                         retrieved by calling OCIErrorGet().
          svc     (IN) [optional] - OCI service context. If null pointer is
                         specified, then the objects in all connections
                         will be unmarked.
     REQUIRES:
          - a valid OCI environment handle must be given.
     DESCRIPTION:
          If a connection is specified, this function unmarks all dirty objects
          in that connection.  Otherwise, all dirty objects in the cache are
          unmarked. See OCIObjectUnmark() for more information about unmarking
          an object.
     RETURNS:
          if environment handle or error handle is null, return
          OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  TOCIDurationBegin = function(env: POCIEnv; err: POCIError;
    svc: POCISvcCtx; const parent: OCIDuration;
    dur: POCIDuration): sword; cdecl;

  {
     NAME: OCIDurationBegin - OCI DURATION BEGIN
     PARAMETERS:
          env  (IN/OUT) - OCI environment handle initialized in object mode
                          This should be passed NULL, when cartridge services
                          are to be used.
          err  (IN/OUT) - error handle. If there is an error, it is
                          recorded in 'err' and this function returns OCI_ERROR.
                          The error recorded in 'err' can be retrieved by calling
                         OCIErrorGet().
          svc  (IN/OUT) - OCI service handle.
          parent   (IN) - parent for the duration to be started.
          dur     (OUT) - newly created user duration
     REQUIRES:
          - a valid OCI environment handle must be given for non-cartridge
            services.
          - For cartridge services, NULL should be given for environment handle
          - A valid service handle must be given in all cases.
     DESCRIPTION:
          This function starts a new user duration.  A user can have multiple
          active user durations simultaneously. The user durations do not have
          to be nested.

          The object subsystem predefines 3 durations :
            1) session     - memory allocated with session duration comes from
                             the UGA heap (OCI_DURATION_SESSION). A session
                             duration terminates at the end of the user session.
            2) transaction - memory allocated with transaction duration comes
                             from the UGA heap (OCI_DURATION_TRANS). A trans-
                             action duration terminates at the end of the user
                             transaction.
            3) call        - memory allocated with call duration comes from PGA
                             heap (OCI_DURATION_CALL). A call duration terminates
                             at the end of the user call.

          Each user duration has a parent duration.  A parent duration can be a
          predefined duration or another user duration.  The relationship between
          a user duration and its parent duration (child duration) are:

           1) An user duration is nested within the parent duration. When its
               parent duration terminates, the user duration will also terminate.
           2) The memory allocated with an user duration comes from the heap of
               its parent duration. For example, if the parent duration of an
               user duration is call, then the memory allocated with the user
               duration will also come from the PGA heap.

          This function can be used as both part of cartridge services as well
          as without cartridge services.
          The difference in the function in the case of cartridge and
          non-cartridge services is:
                  In case of cartridge services, as descibed above a new user
          duration is created as a child of the "parent" duration.
                  But when used for non-cartridge purposes, when a pre-defined
          duration is passed in as parent, it is mapped to the cache duration
          for that connection (which is created if not already present) and
          the new user duration will be child of the cache duration.

     RETURNS:
          if environment handle and service handle is null or if error
          handle is null return OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }
  TOCIDurationEnd = function(env: POCIEnv; err: POCIError; svc: POCISvcCtx;
    duration: OCIDuration): sword; cdecl;

  {
     NAME: OCIDurationEnd - OCI DURATION END
     PARAMETERS:
          env  (IN/OUT) - OCI environment handle initialized in object mode
                          This should be passed NULL, when cartridge services
                          are to be used.
          err  (IN/OUT) - error handle. If there is an error, it is
                          recorded in 'err' and this function returns OCI_ERROR.
                          The error recorded in 'err' can be retrieved by calling
                         OCIErrorGet().
          svc  (IN/OUT) - OCI service handle.
          dur     (OUT) - a previously created user duration using
                          OCIDurationBegin()
     REQUIRES:
          - a valid OCI environment handle must be given for non-cartridge
            services.
          - For cartridge services, NULL should be given for environment handle
          - A valid service handle must be given in all cases.
     DESCRIPTION:
          This function terminates a user duration.  All memory allocated for
          this duration is freed.

          This function can be used as both part of cartridge services as well
          as without cartridge services.  In both cased, the heap duration
          is freed and all the allocated memory for that duration is freed.
          The difference in the function in the case of cartridge and
          non-cartridge services is:
                  In case of non-cartridge services, if the duration is pre-
          defined, the associated cache duration (see OCIDurationBegin())
          is also terminated and the following is done.
            1) The child durations are terminated.
            2) All objects pinned for this duration are unpinned.
            3) All instances allocated for this duration are freed.

                  In case of cartridge services, only the heap duration is
          freed.  All the context entries allocated for that duration are
          freed from the context hash table..

     RETURNS:
          if environment handle and service handle is null or if error
          handle is null return OCI_INVALID_HANDLE.
          if operation suceeds, return OCI_SUCCESS.
          if operation fails, return OCI_ERROR.
    }

(*-----------------------------ort.h----------------------------------------*)

  {----------------------------- TYPE DESCRIPTION ---------------------------- }
  {
   * OCIType - OCI Type Description Object
   *
   * The contents of an 'OCIType' is private/opaque to clients.  Clients just
   * need to declare and pass 'OCIType' pointers in to the type manage
   * functions.
   * The pointer points to the type in the object cache.  Thus, clients don't
   * need to allocate space for this type and must NEVER free the pointer to the
   * 'OCIType'.
    }

  type
  {------------------------- TYPE ELEMENT DESCRIPTION ------------------------ }
  {
   * OCITypeElem - OCI Type Element object
   *
   * The contents of an 'OCITypeElem' is private/opaque to clients. Clients just
   * need to declare and pass 'OCITypeElem' pointers in to the type manager
   * functions.
   *
   * 'OCITypeElem' objects contains type element information such as the numeric
   * precision for example, for number objects, and the number of elements for
   * arrays.
   * They ARE used to describe type attributes, collection elements,
   * method parameters, and method results. Hence they are pass in or returned
   * by attribute, collection, and method parameter/result accessors.
    }
  {--------------------------- METHOD DESCRIPTION --------------------------- }
  {
   * OCITypeMethod - OCI Method Description object
   *
   * The contents of an 'OCITypeMethod' is private/opaque to clients.  Clients
   * just need to declare and pass 'OCITypeMethod' pointers in to the type
   * manager functions.
   * The pointer points to the method in the object cache.  Thus, clients don't
   * need to allocate space for this type and must NEVER free the pointer to
   * the 'OCITypeMethod'.
    }
  {--------------------------- TYPE ACCESS ITERATOR -------------------------- }
  {
   * OCITypeIter- OCI Type Iterator
   *
   * The contents of an 'orti' is private/opaque to clients.  Clients just
   * need to declare and pass 'orti' pointers in to the type manager functions.
   * The iterator is used to retreive MDO's and ADO's that belong to the TDO
   * one at a time. It needs to be allocated by the 'OCITypeIterNew()' function
   * call and deallocated with the 'OCITypeIterFree()' function call.
    }
  {================== }
  { PUBLIC FUNCTIONS  }
  {================== }
  {-------------------------------------------------------------------------- }
  {                                  ITERATOR                                 }
  {-------------------------------------------------------------------------- }
  {-----------------------_- OCITypeIterNew --------------------------------- }
  { ** OBSOLETE **  }

  TOCITypeIterNew = function (env: POCIEnv; err: POCIError; const tdo: POCIType;
                      iterator_ort: PPOCITypeIter):sword; cdecl;

  {
    NAME: OCITypeIterNew - OCI Iterator NEW
    PARAMETERS:
         env (IN/OUT) - OCI environment handle initialized in object mode
         err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
         tdo (IN) - pointer to the pinned type in the object cache to
                  initialize the iterator with
         iterator_ort (OUT) - pointer to the pointer to the new iterator created
    DESCRIPTION:
         Create a new instance of a method/attribute iterator and initalize
         it's values.
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
             1) any of the required parameters is null.
             2) error while allocating space for the iterator.
   }
  {------------------------ OCITypeIterSet --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeIterSet = function(env: POCIEnv; err: POCIError; const tdo: POCIType;
                              iterator_ort: POCITypeIter): sword; cdecl;

  {
    NAME: OCITypeIterSet - OCI Iterator SET
    PARAMETERS:
         env (IN/OUT) - OCI environment handle initialized in object mode
         err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
         tdo (IN) - pointer to the pinned type in the object cache to
                  initialize the iterator with
         iterator_ort (IN/OUT) - pointer to the iterator to set
    DESCRIPTION:
         Initializes the iterator. This is used to reset the state of the
         iterator.
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
             1) any of the required parameters is null.
   }
  {------------------------ OCITypeIterFree --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeIterFree = function(env: POCIEnv; err: POCIError;
                        iterator_ort: POCITypeIter):sword;

  {
    NAME: OCITypeIterFree - OCI Iterator FREe
    PARAMETERS:
         env (IN/OUT) - OCI environment handle initialized in object mode
         err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
         iterator_ort (IN/OUT) - pointer to the iterator to free
    DESCRIPTION:
         Free space allocated for the iterator.
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
             1) any of the required parameters is null.
             2) error while freeing the iterator, probably bad iterator pointer.
   }
  {-------------------------------------------------------------------------- }
  {                                  TYPE GET                                 }
  {-------------------------------------------------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeByName = function(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
    schema_name: Poratext; const s_length: ub4; const type_name: Poratext;
    const t_length: ub4; version_name: Poratext; const v_length: ub4;
    const pin_duration: OCIDuration; const get_option: OCITypeGetOpt;
    tdo: PPOCIType): sword; cdecl;

  {
    NAME: OCITypeByName - OCI Get the most current version of an existing TYPe
                    by name.
    PARAMETERS:
         env (IN/OUT) - OCI environment handle initialized in object mode
         err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
         svc (IN) - OCI service handle
         schema_name (IN, optional) - name of schema associated with the
                    type.  By default, the user's schema name is used.
         s_length (IN) - length of the 'schema_name' parameter
         type_name (IN) - name of the type to get
         t_length (IN) - length of the 'type_name' parameter
         version_name (IN, optional) - user readable version of the type.
                    Pass (oratext *)0 for the most current version.
         v_length (IN) - length of version_name in bytes. Should be 0 if
                    the most current version is to be retrieved.
         pin_duration (IN) - pin duration (e.g. until the end of current
                    transaction).  See 'oro.h' for a description of
                    each option.
         get_option (IN) - options for loading the types. It can be one of two
                     values:
                    OCI_TYPEGET_HEADER for only the header to be loaded, or
                    OCI_TYPEGET_ALL for the TDO and all ADO and MDOs to be
                      loaded.
         tdo (OUT) - pointer to the pinned type in the object cache
    DESCRIPTION:
         Get a pointer to a version of the existing type associated
         with schema/type name.
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
             1) any of the required parameters is null.
             2) the adt type associated with schema/type name does not exist.
    NOTE:
         Schema and type names are CASE-SENSITIVE. If they have been created
         via SQL, you need to use uppercase names.
   }
  TOCITypeArrayByName = function(env: POCIEnv; err: POCIError; svc: POCISvcCtx;
    array_len: ub4; schema_name:  PPoratext; s_length: Pub4;
    type_name: PPoratext; t_length: Pub4; version_name: PPoratext;
    v_length: Pub4; pin_duration: OCIDuration; get_option: OCITypeGetOpt;
    tdo: PPOCIType): sword; cdecl;

  {
    NAME: OCITypeArrayByName - OCI Get array of TYPes by name.
    PARAMETERS:
         env (IN/OUT) - OCI environment handle initialized in object mode
         err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
         svc (IN) - OCI service handle
         array_len (IN) - number of schema_name/type_name/version_name entries to
                    be retrieved.
         schema_name (IN, optional) - array of schema names associated with the
                    types to be retrieved.  The array must have array_len
                    elements if specified.
                    If 0 is supplied, the default schema is assumed, otherwise
                    it MUST have array_len number of elements.
                    0 can be supplied for one or more of the entries to indicate
                    that the default schema is desired for those entries.
         s_length (IN) - array of schema_name lengths with each entry
                    corresponding to the length of the corresponding schema_name
                    entry in the schema_name array in bytes.
                    The array must either have array_len number of elements or
                    it MUST be 0 if schema_name is not specified.
         type_name (IN) - array of the names of the types to retrieve. This
                    MUST have array_len number of elements.
         t_length (IN) - array of the lengths of type names in the type_name
                    array in bytes.
         version_name (IN) - array of the version names of the types to retrieve
                    corresponding. This can be 0 to indicate retrieval of the
                    most current versions, or it MUST have array_len number of
                    elements.
                    If 0 is supplied, the most current version is assumed,
                    otherwise it MUST have array_len number of elements.
                    0 can be supplied for one or more of the entries to indicate
                    that the current version is desired for those entries.
         v_length (IN) - array of the lengths of version names in the
                    version_name array in bytes.
         pin_duration (IN) - pin duration (e.g. until the end of current
                    transaction) for the types retreieve.  See 'oro.h' for a
                    description of each option.
         get_option (IN) - options for loading the types. It can be one of two
                     values:
                    OCI_TYPEGET_HEADER for only the header to be loaded, or
                    OCI_TYPEGET_ALL for the TDO and all ADO and MDOs to be
                      loaded.
         tdo (OUT) - output array for the pointers to each pinned type in the
                    object cache. It must have space for array_len pointers.
                    Use OCIObjectGetObjectRef() to obtain the CREF to each
                    pinned type descriptor.
    DESCRIPTION:
         Get pointers to the existing types associated with the schema/type name
         array. This is similar to OCITypeByName() except that all the TDO's are
         retreived via a single network roundtrip.
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
             1) any of the required parameters is null.
             2) one or more adt types associated with a schema/type name entry
                does not exist.
   }

  TOCITypeArrayByRef = function(env: POCIEnv; err: POCIError; array_len: ub4;
    type_ref: PPOCIRef; pin_duration: OCIDuration; get_option: OCITypeGetOpt;
    tdo: PPOCIType): sword; cdecl;

  {
    NAME: OCITypeArrayByRef - OCI Get array of TYPes by REF.
    PARAMETERS:
         env (IN/OUT) - OCI environment handle initialized in object mode
         err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
         array_len (IN) - number of schema_name/type_name/version_name entries to
                    be retrieved.
         type_ref (IN) - array of OCIRef * pointing to the particular version of
                    the type descriptor object to obtain.
                    The array must have array_len elements if specified.
         pin_duration (IN) - pin duration (e.g. until the end of current
                    transaction) for the types retreieve.  See 'oro.h' for a
                    description of each option.
         get_option (IN) - options for loading the types. It can be one of two
                     values:
                    OCI_TYPEGET_HEADER for only the header to be loaded, or
                    OCI_TYPEGET_ALL for the TDO and all ADO and MDOs to be
                      loaded.
         tdo (OUT) - output array for the pointers to each pinned type in the
                    object cache. It must have space for array_len pointers.
                    Use OCIObjectGetObjectRef() to obtain the CREF to each
                    pinned type descriptor.
    DESCRIPTION:
         Get pointers to the
         with the schema/type name array. This is similar to OCITypeByName()
         except that all the TDO's are retreived via a single network roundtrip.
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
             1) any of the required parameters is null.
             2) one or more adt types associated with a schema/type name entry
                does not exist.
   }
  {-------------------------------------------------------------------------- }
  {                              TYPE ACCESSORS                               }
  {-------------------------------------------------------------------------- }
  {---------------------------- OCITypeName --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeName = function(env: POCIEnv; err: POCIError; tdo: POCIType;
    n_length: Pub4): poratext; cdecl;

  {
     NAME: OCITypeName -  ORT Get a Type's naME.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
          n_length (OUT) - length (in bytes) of the returned type name.  The
                 caller must allocate space for the ub4 before calling this
                 routine.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
          3) 'n_length' must point to an allocated ub4.
     DESCRIPTION:
          Get the name of the type.
     RETURNS:
          the name of the type
     NOTES:
          The type descriptor, 'tdo', must be unpinned when the accessed
          information is no longer needed.
    }
  {------------------------ OCITypeSchema --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeSchema = function(env: POCIEnv; err: POCIError; const tdo: POCIType;
    n_length: Pub4): poratext; cdecl;

  {
     NAME: OCITypeSchema -  ORT Get a Type's SCHema name.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
          n_length (OUT) - length (in bytes) of the returned schema name.  The
                 caller must allocate space for the ub4 before calling this
                 routine.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
          3) 'n_length' must point to an allocated ub4.
     DESCRIPTION:
          Get the schema name of the type.
     RETURNS:
          the schema name of the type
     NOTES:
          The type descriptor, 'tdo', must be unpinned when the accessed
          information is no longer needed.
    }
  {------------------------ OCITypeTypeCode --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeTypeCode = function(env: POCIEnv; err: POCIError;
    const tdo: POCIType): OCITypeCode; cdecl;

  {
     NAME: OCITypeTypeCode - OCI Get a Type's Type Code.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the type code of the type.
     RETURNS:
          The type code of the type.
     NOTES:
          The type descriptor, 'tdo', must be unpinned when the accessed
          information is no longer needed.
    }
  {----------------------- OCITypeCollTypeCode ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeCollTypeCode = function(env:POCIEnv; err:POCIError;
    const tdo: POCIType): OCITypeCode; cdecl;

  {
     NAME: OCITypeCollTypeCode - OCI Get a Domain Type's Type Code.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
          3) 'tdo' MUST point to a named collection type.
     DESCRIPTION:
          Get the type code of the named collection type. For V8.0, named
          collection types can only be variable length arrays and nested tables.
     RETURNS:
          OCI_TYPECODE_VARRAY for variable length array, and
          OCI_TYPECODE_TABLE for nested tables.
     NOTES:
          The type descriptor, 'tdo', should be unpinned when the accessed
          information is no longer needed.
    }
  {------------------------- OCITypeVersion --------------------------------- }
  { ** OBSOLETE **  }
(* Const before type ignored *)
  TOCITypeVersion = function(env: POCIEnv; err: POCIError; const tdo: POCIType;
    v_length: Pub4): poratext; cdecl;

  {
     NAME: OCITypeVersion - OCI Get a Type's user-readable VersioN.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
          v_length (OUT) - length (in bytes) of the returned user-readable
                 version.  The caller must allocate space for the ub4 before
                 calling this routine.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
          3) 'v_length' must point to an allocated ub4.
     DESCRIPTION:
          Get the user-readable version of the type.
     RETURNS:
          The user-readable version of the type
     NOTES:
          The type descriptor, 'tdo', must be unpinned when the accessed
          information is no longer needed.
    }
  {--------------------------- OCITypeAttrs --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeAttrs = function(env: POCIEnv; err: POCIError;
    const tdo:POCIType): ub4; cdecl;

  {
     NAME: OCITypeAttrs - OCI Get a Type's Number of Attributes.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the number of attributes in the type.
     RETURNS:
          The number of attributes in the type. 0 for ALL non-ADTs.
     NOTES:
          The type descriptor, 'tdo', must be unpinned when the accessed
          information is no longer needed.
    }
  {------------------------- OCITypeMethods --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeMethods = function(env: POCIEnv; err: POCIError;
    const tdo: POCIType): ub4; cdecl;

  {
     NAME: OCITypeMethods - OCI Get a Type's Number of Methods.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the number of methods in a type.
     RETURNS:
          The number of methods in the type
     NOTES:
          The type descriptor, 'tdo', must be unpinned when the accessed
          information is no longer needed.
    }
  {-------------------------------------------------------------------------- }
  {                     TYPE ELEMENT INFORMATION ACCESSORS                    }
  {-------------------------------------------------------------------------- }
  {------------------------ OCITypeElemName --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemName = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem; n_length:Pub4): poratext; cdecl;

  {
     NAME: OCITypeElemName - OCI Get an Attribute's NaMe.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the type element descriptor in the object cache
          n_length (OUT) - length (in bytes) of the returned attribute name.
                 The caller must allocate space for the ub4 before calling this
                 routine.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
          3) 'n_length' must point to an allocated ub4.
     DESCRIPTION:
          Get the name of the attribute.
     RETURNS:
          the name of the attribute and the length in n_length
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeElemTypeCode ------------------------------ }
  { ** OBSOLETE **  }
  TOCITypeElemTypeCode = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem): OCITypeCode; cdecl;

  {
     NAME: OCITypeElemTypeCode - OCI Get an Attribute's TypeCode.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the type element descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the typecode of an attribute's type.
     RETURNS:
          the typecode of the attribute's type.  If this is a scalar type, the
          typecode sufficiently describes the scalar type and no further calls
          need to be made.  Valid scalar types include: OCI_TYPECODE_SIGNED8,
          OCI_TYPECODE_UNSIGNED8, OCI_TYPECODE_SIGNED16, OCI_TYPECODE_UNSIGNED16,
          OCI_TYPECODE_SIGNED32, OCI_TYPECODE_UNSIGNED32, OCI_TYPECODE_REAL,
          OCI_TYPECODE_DOUBLE, OCI_TYPECODE_DATE,
          OCI_TYPECODE_MLSLABEL, OROTCOID, OCI_TYPECODE_OCTET, or OROTCLOB.
          This function converts the CREF (stored in the attribute) into a
          typecode.
     NOTES:
         The type must be unpinned when the accessed information is no
         longer needed.
    }
  {------------------------ OCITypeElemType --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemType = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem; elem_tdo:PPOCIType): sword; cdecl;

  {
    PARAMETERS
       env (IN/OUT) - OCI environment handle initialized in object mode
       err (IN/OUT) - error handle. If there is an error, it is
               recorded in 'err' and this function returns OCI_ERROR.
               The error recorded in 'err' can be retrieved by calling
               OCIErrorGet().
       elem (IN) - pointer to the type element descriptor in the object cache
       elem_tdo (OUT) - If the function completes successfully, 'elem_tdo'
              points to the type descriptor (in the object cache) of the type of
              the element.

    REQUIRES
       1) All type accessors require that the type be pinned before calling
          any accessor.  This can be done by calling 'OCITypeByName()'.
       2) if 'elem' is not null, it must point to a valid type element descriptor
          in the object cache.

    DESCRIPTION
       Get the type tdo of the type of this element.
    RETURNS
       OCI_SUCCESS if the function completes successfully.
       OCI_INVALID_HANDLE if 'env' or 'err' is null.
       OCI_ERROR if
           1) any of the parameters is null.

    NOTES
       The type must be unpinned when the accessed information is no
       longer needed.  This can be done by calling 'OCIObjectUnpin()'.
    }
  {------------------------- OCITypeElemFlags ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemFlags = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem): ub4; cdecl;

  {
     NAME: OCITypeElemFlags - OCI Get a Elem's FLags
                                (inline, constant, virtual, constructor,
                                destructor).
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the type element descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the flags of a type element (attribute, parameter).
     RETURNS:
          The flags of the type element.
     NOTES:
          The flag bits are not externally documented. Use only the macros
          in the last section (ie. OCI_TYPEPARAM_IS_REQUIRED, and
          OCI_TYPEELEM_IS_REF) to test for them only. The type must be unpinned
          when the accessed information is no longer needed.
    }
  {------------------------ OCITypeElemNumPrec ------------------------------ }
  { ** OBSOLETE **  }
  TOCITypeElemNumPrec = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem): ub1; cdecl;

  {
     NAME: OCITypeElemNumPrec - Get a Number's Precision.  This includes float,
                                decimal, real, double, and oracle number.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the type element descriptor in the object cache
     REQUIRES:
          All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the precision of a float, decimal, long, unsigned long, real,
          double, or Oracle number type.
     RETURNS:
          the precision of the float, decimal, long, unsigned long, real, double,
          or Oracle number
    }
  {------------------------- OCITypeElemNumScale ----------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemNumScale = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem): sb1; cdecl;

  {
     NAME: OCITypeElemNumScale - Get a decimal or oracle Number's Scale
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the type element descriptor in the object cache
     REQUIRES:
          All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the scale of a decimal, or Oracle number type.
     RETURNS:
          the scale of the decimal, or Oracle number
    }
  {------------------------ OCITypeElemLength ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemLength = function(env: POCIEnv; err: POCIError;
    const elem:POCITypeElem): ub4; cdecl;

  {
     NAME: OCITypeElemLength - Get a raw, fixed or variable length String's
                               length in bytes.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the type element descriptor in the object cache
     REQUIRES:
          All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the length of a raw, fixed or variable length string type.
     RETURNS:
          length of the raw, fixed or variable length string
    }
  {----------------------- OCITypeElemCharSetID ----------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemCharSetID = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem): ub2; cdecl;

  {
     NAME: OCITypeElemCharSetID - Get a fixed or variable length String's
                                  character set ID
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the type element descriptor in the object cache
     REQUIRES:
          All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the character set ID of a fixed or variable length string type.
     RETURNS:
          character set ID of the fixed or variable length string
    }
  {---------------------- OCITypeElemCharSetForm ---------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemCharSetForm = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem): ub2; cdecl;

  {
     NAME: OCITypeElemCharSetForm - Get a fixed or variable length String's
                                    character set specification form.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the attribute information in the object cache
     REQUIRES:
          All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the character form of a fixed or variable length string type.
          The character form is an enumerated value that can be one of the
          4 values below:
                 SQLCS_IMPLICIT for CHAR, VARCHAR2, CLOB w/o a specified set
                 SQLCS_NCHAR    for NCHAR, NCHAR VARYING, NCLOB
                 SQLCS_EXPLICIT for CHAR, etc, with "CHARACTER SET ..." syntax
                 SQLCS_FLEXIBLE for PL/SQL "flexible" parameters
     RETURNS:
          character form of the fixed or variable string
    }
  {--------------------- OCITypeElemParameterizedType ------------------------ }
  { ** OBSOLETE **  }
  TOCITypeElemParameterizedType = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem; type_stored: PPOCIType): sword; cdecl;

  {
     NAME: OCITypeElemParameterizedType
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the type element descriptor in the object cache
          type_stored (OUT) - If the function completes successfully,
                 and the parameterized type is complex, 'type_stored' is NULL.
                 Otherwise, 'type_stored' points to the type descriptor (in the
                 object cache) of the type that is stored in the parameterized
                 type.  The caller must allocate space for the OCIType*
                 before calling this routine and must not write into the space.
     REQUIRES:
          All input parameters must be valid.
     DESCRIPTION:
          Get a descriptor to the parameter type of a parameterized type.
          Parameterized types are types of the form:
            REF T
            VARRAY (n) OF T
          etc, where T is the parameter in the parameterized type.
          Additionally is_ref is set if the parameter is a PTR or REF.
          For example, it is set for REF T or VARRAY(n) OF REF T.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the parameters is null.
              2) 'type_stored' is not NULL but points to NULL data.
     NOTES:
          Complex parameterized types will be in a future release (once
          typedefs are supported.  When setting the parameterized type
          information, the user must typedef the contents if it's a
          complex parameterized type.  Ex. for varray<varray<car>>, use
          'typedef varray<car> varcar' and then use varray<varcar>.
    }
  {----------------------- OCITypeElemExtTypeCode ---------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemExtTypeCode = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem): OCITypeCode; cdecl;

  {
     NAME: OCITypeElemExtTypeCode - OCI Get an element's SQLT constant.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the type element descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the internal Oracle typecode associated with an attribute's type.
          This is the actual typecode for the attribute when it gets mapped
          to a column in the Oracle database.
     RETURNS:
          The Oracle typecode associated with the attribute's type.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {-------------------------------------------------------------------------- }
  {                           ATTRIBUTE ACCESSORS                             }
  {-------------------------------------------------------------------------- }
  {------------------------ OCITypeAttrByName ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeAttrByName = function(env: POCIEnv; err: POCIError;
    const tdo: POCIType; const name: Poratext; const n_length: ub4;
    elem: PPOCITypeElem): sword; cdecl;

  {
     NAME: OCITypeAttrByName - OCI Get an Attribute By Name.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
          name (IN) - the attribute's name
          n_length (IN) - length (in bytes) of the 'name' parameter
          elem (OUT) - If this function completes successfully, 'elem' points to
                 the selected type element descriptor pertaining to the
                 attributein the object cache.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) if 'tdo' is not null, it must point to a valid type descriptor
             in the object cache.
     DESCRIPTION:
          Get an attribute given its name.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the required parameters is null.
              2) the type does not contain an attribute with the input 'name'.
              3) 'name' is NULL.
     NOTES:
          The type descriptor, 'tdo', must be unpinned when the accessed
          information is no longer needed.
          Schema and type names are CASE-SENSITIVE. If they have been created
          via SQL, you need to use uppercase names.
    }
  {------------------------ OCITypeAttrNext --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeAttrNext = function(env: POCIEnv; err: POCIError;
    iterator_ort: POCITypeIter; elem: PPOCITypeElem): sword; cdecl;

  {
     NAME: OCITypeAttrNext - OCI Get an Attribute By Iteration.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          iterator_ort (IN/OUT) - iterator for retrieving the next attribute;
                 see OCITypeIterNew() to initialize iterator.
          elem (OUT) - If this function completes successfully, 'elem' points to
                 the selected type element descriptor pertaining to the
                 attributein the object cache.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
              any accessor.
          2) if 'tdo' is not null, it must point to a valid type descriptor
             in the object cache.
     DESCRIPTION:
          Iterate to the next attribute to retrieve.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_NO_DATA if there are no more attributes to iterate on; use
              OCITypeIterSet() to reset the iterator if necessary.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the required parameters is null.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {-------------------------------------------------------------------------- }
  {                           COLLECTION ACCESSORS                            }
  {-------------------------------------------------------------------------- }
  {------------------------ OCITypeCollElem --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeCollElem = function(env: POCIEnv; err: POCIError; const tdo:POCIType;
    element: PPOCITypeElem): sword; cdecl;

  {
     NAME: OCITypeCollElem
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to the type descriptor in the object cache
          element (IN/OUT) - If the function completes successfully, this
                 points to the descriptor for the collection's element.
                 It is stored in the same format as an ADT attribute's
                 descriptor.
                 If *element is NULL, OCITypeCollElem() implicitly allocates a
                 new instance of OCITypeElem in the object cache. This instance
                 will be
                 automatically freed at the end of the session, and does not have
                 to be freed explicitly.
                 If *element is not NULL, OCITypeCollElem() assumes that it
                 points to a valid OCITypeElem descriptor and will copy the
                 results into it.
     REQUIRES:
          All input parameters must be valid.
     DESCRIPTION:
          Get a pointer to the descriptor (OCITypeElem) of the element of an
          array or the rowtype of a nested table.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the parameters is null.
              2) the type TDO does not point to a valid collection's type.
     NOTES:
          Complex parameterized types will be in a future release (once
          typedefs are supported.  When setting the parameterized type
          information, the user must typedef the contents if it's a
          complex parameterized type.  Ex. for varray<varray<car>>, use
          'typedef varray<car> varcar' and then use varray<varcar>.
    }
  {------------------------ OCITypeCollSize --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeCollSize = function(env: POCIEnv; err: POCIError; const tdo: POCIType;
    num_elems: Pub4): sword; cdecl;

  {
     NAME: OCITypeCollSize - OCI Get a Collection's Number of Elements.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to the type descriptor in the object cache
          num_elems (OUT) - number of elements in collection
     REQUIRES:
          All input parameters must be valid. tdo points to an array type
          defined as a domain.
     DESCRIPTION:
          Get the number of elements stored in a fixed array or the maximum
          number of elements in a variable array.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the parameters is null.
              2) 'tdo' does not point to a domain with a collection type.
     NOTES:
          Complex parameterized types will be in a future release (once
          typedefs are supported.  When setting the parameterized type
          information, the user must typedef the contents if it's a
          complex parameterized type.  Ex. for varray<varray<car>>, use
          'typedef varray<car> varcar' and then use varray<varcar>.
    }
  {------------------------ OCITypeCollExtTypeCode --------------------------- }
  { ** OBSOLETE **  }
  TOCITypeCollExtTypeCode = function(env: POCIEnv; err: POCIError;
    const tdo:POCIType; sqt_code: POCITypeCode): sword; cdecl;

  {
     NAME: ortcsqt - OCI Get a Collection element's DTY constant.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to the type descriptor in the object cache
          sqt_code (OUT) - SQLT code of type element.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the SQLT constant associated with an domain's element type.
          The SQLT codes are defined in <sqldef.h> and are needed for OCI/OOCI
          use.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the parameters is null.
              2) 'tdo' does not point to a domain with a collection type.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {-------------------------------------------------------------------------- }
  {                             METHOD ACCESSORS                              }
  {-------------------------------------------------------------------------- }
  {------------------------- OCITypeMethodOverload -------------------------- }
  { ** OBSOLETE **  }
  TOCITypeMethodOverload = function(env: POCIEnv; err: POCIError;
    const tdo: POCIType; const method_name: Poratext;
    const m_length: ub4): ub4; cdecl;

  {
     NAME: OCITypeMethodOverload - OCI Get type's Number of Overloaded names
                                   for the given method name.
     PARAMETERS:
          gp (IN/OUT) - pga environment handle.  Any errors are recorded here.
          tdo (IN) - pointer to to the type descriptor in the object cache
          method_name (IN) - the method's name
          m_length (IN) - length (in bytes) of the 'method_name' parameter
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) if 'tdo' is not null, it must point to a valid type descriptor
             in the object cache.
     DESCRIPTION:
          Overloading of methods implies that more than one method may have the
          same method name.  This routine returns the number of methods that
          have the given method name.  If there are no methods with the input
          method name, 'num_methods' is 0.  The caller uses this information when
          allocating space for the array of mdo and/or position pointers before
          calling 'OCITypeMethodByName()' or 'ortgmps()'.
     RETURNS:
          The number of methods with the given name. 0 if none contains the
          name.
     NOTES:
          Schema and type names are CASE-SENSITIVE. If they have been created
          via SQL, you need to use uppercase names.
    }
  {------------------------ OCITypeMethodByName ------------------------------ }
  { ** OBSOLETE **  }
  TOCITypeMethodByName = function(env: POCIEnv; err: POCIError;
    const tdo: POCIType; const method_name: Poratext; const m_length: ub4;
    mdos: PPOCITypeMethod): sword; cdecl;

  {
     NAME: OCITypeMethodByName - OCI Get one or more Methods with Name.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
          method_name (IN) - the methods' name
          m_length (IN) - length (in bytes) of the 'name' parameter
          mdos (OUT) - If this function completes successfully, 'mdos' points to
                  the selected methods in the object cache.  The caller must
                  allocate space for the array of OCITypeMethod pointers before
                  calling this routine and must not write into the space.
                  The number of OCITypeMethod pointers that will be returned can
                  be obtained by calling 'OCITypeMethodOverload()'.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) if 'tdo' is not null, it must point to a valid type descriptor
             in the object cache.
     DESCRIPTION:
          Get one or more methods given the name.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the required parameters is null.
              2) No methods in type has name 'name'.
              3) 'mdos' is not NULL but points to NULL data.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
          Schema and type names are CASE-SENSITIVE. If they have been created
          via SQL, you need to use uppercase names.
    }
  {------------------------ OCITypeMethodNext -------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeMethodNext = function(env: POCIEnv; err: POCIError;
    iterator_ort: POCITypeIter; mdo: PPOCITypeMethod): sword; cdecl;

  {
     NAME: OCITypeMethodNext - OCI Get a Method By Iteration.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          iterator_ort (IN/OUT) - iterator for retrieving the next method;
                 see OCITypeIterNew() to set iterator.
          mdo (OUT) - If this function completes successfully, 'mdo' points to
                 the selected method descriptor in the object cache.  Positions
                 start at 1.  The caller must allocate space for the
                 OCITypeMethod* before calling this routine and must not write
                 nto the space.
     REQUIRES:
           1) All type accessors require that the type be pinned before calling
              any accessor.
          2) if 'tdo' is not null, it must point to a valid type descriptor
             in the object cache.
     DESCRIPTION:
          Iterate to the next method to retrieve.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_NO_DATA if there are no more attributes to iterate on; use
              OCITypeIterSet() to reset the iterator if necessary.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the required parameters is null.
              2) 'mdo' is not NULL but points to NULL data.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeMethodName -------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeMethodName = function(env:POCIEnv; err: POCIError;
    const mdo: POCITypeMethod; n_length: Pub4): poratext; cdecl;
  {
     NAME: OCITypeMethodName - OCI Get a Method's NaMe.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          mdo (IN) - pointer to the method descriptor in the object cache
          n_length (OUT) - length (in bytes) of the 'name' parameter.  The caller
                 must allocate space for the ub4 before calling this routine.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the (non-unique) real name of the method.
     RETURNS:
          the non-unique name of the method or NULL if there is an error.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeMethodEncap ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeMethodEncap = function(env: POCIEnv; err: POCIError;
          const mdo: POCITypeMethod): OCITypeEncap; cdecl;

  {
     NAME: OCITypeMethodEncap - Get a Method's ENcapsulation (private/public).
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          mdo (IN) - pointer to the method descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the encapsulation (private, or public) of a method.
     RETURNS:
          the encapsulation (private, or public) of the method
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeMethodFlags ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeMethodFlags = function(env: POCIEnv; err: POCIError;
      const mdo:POCITypeMethod): OCITypeMethodFlag; cdecl;

  {
     NAME: OCITypeMethodFlags - OCI Get a Method's FLags
                                (inline, constant, virtual, constructor,
                                destructor).
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          mdo (IN) - pointer to the method descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the flags (inline, constant, virutal, constructor, destructor) of
          a method.
     RETURNS:
          the flags (inline, constant, virutal, constructor, destructor) of
          the method
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeMethodMap --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeMethodMap = function(env: POCIEnv; err: POCIError; const tdo: POCIType;
    mdo: PPOCITypeMethod): sword; cdecl;

  {
     NAME: OCITypeMethodMap - OCI Get the Method's MAP function.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
          mdo (OUT) - If this function completes successfully, and there is a
                 map function for this type, 'mdo' points to the selected method
                 descriptor in the object cache.  Otherwise, 'mdo' is null.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All required input parameters must not be NULL and must be valid.
     DESCRIPTION:
          A type may have only one map function.  'OCITypeMethodMap()' finds
          this function, if it exists, and returns a reference and a pointer to
          the method descriptor in the object cache.  If the type does not have a
          map (relative ordering) function, then 'mdo_ref' and 'mdo' are set
          to null and an error is returned.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              the type does not contain a map function.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeMethodOrder ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeMethodOrder = function(env: POCIEnv; err: POCIError;
    const tdo: POCIType; mdo: PPOCITypeMethod): sword; cdecl;

  {
     NAME: OCITypeMethodOrder - OCI Get the Method's ORder function.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          tdo (IN) - pointer to to the type descriptor in the object cache
          mdo (OUT) - If this function completes successfully, and there is a
                 map function for this type, 'mdo' points to the selected method
                 descriptor in the object cache.  Otherwise, 'mdo' is null.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All required input parameters must not be NULL and must be valid.
     DESCRIPTION:
          A type may have only one ORder or MAP function. 'OCITypeMethodOrder()'
          finds this function, if it exists, and returns a ref and a pointer
          to the method descriptor in the object cache.  If the type does not
          have a map (relative ordering) function, then 'mdo_ref' and 'mdo' are
          set to null and an error is returned.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              the type does not contain a map function.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeMethodParams ------------------------------ }
  { ** OBSOLETE **  }
  TOCITypeMethodParams = function(env: POCIEnv; err: POCIError;
    const mdo: POCITypeMethod): ub4; cdecl;

  {
     NAME: OCITypeMethodParams - OCI Get a Method's Number of Parameters.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          mdo (IN) - pointer to the method descriptor in the object cache
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the number of parameters in a method.
     RETURNS:
          the number of parameters in the method
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {-------------------------------------------------------------------------- }
  {                             RESULT ACCESSORS                              }
  {-------------------------------------------------------------------------- }
  {-------------------------- OCITypeResult --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeResult = function(env: POCIEnv; err: POCIError;
    const mdo: POCITypeMethod; elem: PPOCITypeElem): sword; cdecl;

  {
     NAME: OCITypeResult - OCI Get a method's result type descriptor.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          mdo (IN) - pointer to the method descriptor in the object cache
          elem (OUT) - If this function completes successfully, 'rdo' points to
                 the selected result (parameter) descriptor in the object cache.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) 'elem' MUST be the address of an OCITypeElem pointer.
     DESCRIPTION:
          Get the result of a method.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the required parameters is null.
              2) method returns no results.
     NOTES:
          The method must be unpinned when the accessed information is no
          longer needed.
    }
  {-------------------------------------------------------------------------- }
  {                           PARAMETER ACCESSORS                             }
  {-------------------------------------------------------------------------- }
  {------------------------ OCITypeParamByPos ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeParamByPos = function(env: POCIEnv; err: POCIError;
    const mdo: POCITypeMethod; const position: ub4;
    elem: PPOCITypeElem): sword; cdecl;

  {
     NAME: OCITypeParamByPos - OCI Get a Parameter in a method By Position.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          mdo (IN) - pointer to the method descriptor in the object cache
          position (IN) - the parameter's position.  Positions start at 1.
          elem (OUT) - If this function completes successfully, 'elem' points to
                 the selected parameter descriptor in the object cache.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
     DESCRIPTION:
          Get a parameter given its position in the method.  Positions start
          at 1.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the required parameters is null.
              2) 'position' is not >= 1 and <= the number of parameters in the
                 method.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeParamByName ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeParamByName = function(env: POCIEnv; err: POCIError;
    const mdo: POCITypeMethod; const name: Poratext; const n_length: ub4;
    elem:PPOCITypeElem): sword; cdecl;

  {
     NAME: OCITypeParamByName - OCI Get a Parameter in a method By Name.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          mdo (IN) - pointer to the method descriptor in the object cache
          name (IN) - the parameter's name
          n_length (IN) - length (in bytes) of the 'name' parameter
          elem (OUT) - If this function completes successfully, 'elem' points to
                 the selected parameter descriptor in the object cache.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) if 'mdo' is not null, it must point to a valid method descriptor
             in the object cache.
     DESCRIPTION:
          Get a parameter given its name.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the required parameters is null.
              2) the method does not contain a parameter with the input 'name'.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeParamPos --------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeParamPos = function(env: POCIEnv; err: POCIError;
    const mdo: POCITypeMethod; const name: Poratext; const n_length: ub4;
    position: Pub4; elem: PPOCITypeElem): sword; cdecl;

  {
     NAME: OCITypeParamPos - OCI Get a parameter's position in a method
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          mdo (IN) - pointer to the method descriptor in the object cache
          name (IN) - the parameter's name
          n_length (IN) - length (in bytes) of the 'name' parameter
          position (OUT) - If this function completes successfully, 'position'
                 points to the position of the parameter in the method starting
                 at position 1. position MUST point to space for a ub4.
          elem (OUT) - If this function completes successfully, and
                 the input 'elem' is not NULL, 'elem' points to the selected
                 parameter descriptor in the object cache.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) if 'mdo' is not null, it must point to a valid method descriptor
             in the object cache.
     DESCRIPTION:
          Get the position of a parameter in a method.  Positions start at 1.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the parameters is null.
              2) the method does not contain a parameter with the input 'name'.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------ OCITypeParamElemMode ----------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemParamMode = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem): OCITypeParamMode; cdecl;

  {
     NAME: OCITypeElemParamMode - OCI Get a parameter's mode
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the parameter descriptor in the object cache
                  (represented by an OCITypeElem)
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the mode (in, out, or in/out) of the parameter.
     RETURNS:
          the mode (in, out, or in/out) of the parameter
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {------------------------- OCITypeElemDefaultValue ------------------------- }
  { ** OBSOLETE **  }
  TOCITypeElemDefaultValue = function(env: POCIEnv; err: POCIError;
    const elem: POCITypeElem; d_v_length: Pub4): poratext; cdecl;

  {
     NAME: OCITypeElemDefaultValue - OCI Get the element's Default Value.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          elem (IN) - pointer to the parameter descriptor in the object cache
                  (represented by an OCITypeElem)
          d_v_length (OUT) - length (in bytes) of the returned default value.
                 The caller must allocate space for the ub4 before calling this
                 routine.
     REQUIRES:
          1) All type accessors require that the type be pinned before calling
             any accessor.
          2) All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Get the default value in text form (PL/SQL) of an element. For V8.0,
          this only makes sense for a method parameter.
     RETURNS:
          The default value (text) of the parameter.
     NOTES:
          The type must be unpinned when the accessed information is no
          longer needed.
    }
  {-------------------------------------------------------------------------- }
  {                           TYPE VERSION TABLE                              }
  {-------------------------------------------------------------------------- }
  { For V8.0, the type version table is meant to be an internal data structure
     only for Oracle clients for type version maintanence purposes. A more
     general version of the API may be made public in subsequent releases.  }
  {--------------------------- OCITypeVTInit -------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeVTInit = function(env: POCIEnv; err: POCIError): sword; cdecl;

  {
     NAME: OCITypeVTInit - OCI type Version table INItialize
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
     REQUIRES:
          none
     DESCRIPTION:
          Allocate space for and initialize the type version table and the type
          version table's index.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if internal errors occurrs during initialization.
    }
  {--------------------------- OCITypeVTInsert ------------------------------- }
  { ** OBSOLETE **  }
  TOCITypeVTInsert = function(env: POCIEnv; err: POCIError;
    const schema_name: Poratext; const s_n_length: ub4;
    const type_name: Poratext; const t_n_length: ub4;
    const user_version:Poratext; const u_v_length:ub4): sword; cdecl;

  {
     NAME: OCITypeVTInsert - OCI type Version table INSert entry.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          schema_name (IN, optional) - name of schema associated with the
                    type.  By default, the user's schema name is used.
          s_n_length (IN) - length of the 'schema_name' parameter
          type_name (IN) - type name to insert
          t_n_length (IN) - length (in bytes) of the 'type_name' parameter
          user_version (IN) - user readable version of the type
          u_v_length (IN) - length (in bytes) of the 'user_version' parameter
     REQUIRES:
          none
     DESCRIPTION:
          Insert an entry into the type version table and the type version
          table's index.  The entry's type name and user readable version
          fields are updated with the input values.  All other fields are
          initialized to null.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the parameters is invalid.
              2) an entry for 'type_name' has already been registered in the
                 type version table.
    }
  {------------------------------ OCITypeVTSelect ---------------------------- }
  { OCITypeVTSelect - OCI type VERSion table SELECT entry  }
  { ** OBSOLETE **  }
  TOCITypeVTSelect = function(env: POCIEnv; err: POCIError;
    const schema_name: Poratext; const s_n_length: ub4;
    const type_name: Poratext; const t_n_length: ub4; user_version: PPoratext;
    u_v_length: Pub4; version: Pub2): sword; cdecl;

  {
     NAME: OCITypeVTSelect - OCI type Version table SELect entry.
     PARAMETERS:
          env (IN/OUT) - OCI environment handle initialized in object mode
          err (IN/OUT) - error handle. If there is an error, it is
                  recorded in 'err' and this function returns OCI_ERROR.
                  The error recorded in 'err' can be retrieved by calling
                  OCIErrorGet().
          schema_name (IN, optional) - name of schema associated with the
                    type.  By default, the user's schema name is used.
          s_n_length (IN) - length of the 'schema_name' parameter
          type_name (IN) - type name to select
          t_n_length (IN) - length (in bytes) of the 'type_name' parameter
          user_version (OUT, optional) - pointer to user readable version of the
                   type
          u_v_length (OUT, optional) - length (in bytes) of the 'user_version'
                   parameter
          version (OUT, optional) - internal type version
     REQUIRES:
          All input parameters must not be NULL and must be valid.
     DESCRIPTION:
          Select an entry in the type version table by name.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_INVALID_HANDLE if 'env' or 'err' is null.
          OCI_ERROR if
              1) any of the parameters is invalid.
              2) an entry with 'type_name' does not exist.
    }
  { Compatibility function - following function prototype retained for
     compatibility only  }
//  function ortgcty(env:POCIEnv; err:POCIError; coll_tdo:POCIType; collelem_tdo:PPOCIType):sword;

  {--------------------------------------------------------------------------- }
  {               Transient Type Construction functions                        }
  {--------------------------------------------------------------------------- }
//  function TOCITypeBeginCreate(svchp:POCISvcCtx; errhp:POCIError; tc:OCITypeCode; dur:OCIDuration; _type:PPOCIType):sword;

  {
     NAME: OCITypeBeginCreate - OCI Type Begin Creation of a transient type.
     REMARKS
         Begins the construction process for a transient type. The type will be
         anonymous (no name). To create a persistent named type, the CREATE TYPE
         statement should be used from SQL. Transient types have no identity.
         They are pure values.
     PARAMETERS:
         svchp (IN)       - The OCI Service Context.
         errhp (IN/OUT)   - The OCI error handle. If there is an error, it is
                            recorded in errhp and this function returns
                            OCI_ERROR. Diagnostic information can be obtained by
                            calling OCIErrorGet().
         tc               - The TypeCode for the type. The Typecode could
                            correspond to a User Defined Type or a Built-in type.
                            Currently, the permissible values for User Defined
                            Types are OCI_TYPECODE_OBJECT for an Object Type
                            (structured), OCI_TYPECODE_VARRAY for a VARRAY
                            collection type or OCI_TYPECODE_TABLE for a nested
                            table collection type. For Object types,
                            OCITypeAddAttr() needs to be called to add each of
                            the attribute types. For Collection types,
                            OCITypeSetCollection() needs to be called.
                            Subsequently, OCITypeEndCreate() needs to be called
                            to finish the creation process.
                            The permissible values for Built-in type codes are
                            specified in the user manual. Additional information
                            on built-ins if any (like precision, scale for
                            numbers, character set info for VARCHAR2s etc.) must
                            be set with a subsequent call to OCITypeSetBuiltin().
                            Subsequently OCITypeEndCreate() needs to be called
                            to finish the creation process.
         dur              - The allocation duration for the Type. Could be a
                            predefined or a user defined duration.
         type(OUT)        - The OCIType (Type Descriptor) that is being
                            constructed.
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_ERROR on error.
   }
//  function TOCITypeSetCollection(svchp:POCISvcCtx; errhp:POCIError; _type:POCIType; collelem_info:POCIParam; coll_count:ub4):sword;

  {
     NAME: OCITypeSetCollection - OCI Type Set Collection information
     REMARKS :
         Set Collection type information. This call can be called only if the
         OCIType has been constructed with a collection typecode.
     PARAMETERS:
         svchp (IN)      -  The OCI Service Context.
         errhp (IN/OUT)  -  The OCI error handle. If there is an error, it is
                            recorded in errhp and this function returns
                            OCI_ERROR. Diagnostic information can be obtained by
                            calling OCIErrorGet().
         type(IN OUT)    -  The OCIType (Type Descriptor) that is being
                            constructed.
         collelem_info   -  collelem_info provides information on the collection
                            element. It is obtained by allocating an OCIParam
                            (parameter handle) and setting type information in
                            the OCIParam using OCIAttrSet() calls.
         coll_count      -  The count of elements in the collection. Pass 0 for
                            a nested table (unbounded).
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_ERROR on error.
   }
//  function TOCITypeSetBuiltin(svchp:POCISvcCtx; errhp:POCIError; _type:POCIType; builtin_info:POCIParam):sword;

  {
     NAME: OCITypeSetBuiltin - OCI Type Set Builtin information.
     REMARKS:
         Set Built-in type information. This call can be called only if the
         OCIType has been constructed with a built-in typecode
         (OCI_TYPECODE_NUMBER etc.).
     PARAMETERS:
         svchp (IN)       - The OCI Service Context.
         errhp (IN/OUT)   - The OCI error handle. If there is an error, it is
                            recorded in errhp and this function returns
                            OCI_ERROR. Diagnostic information can be obtained by
                            calling OCIErrorGet().
         type(IN OUT)     - The OCIType (Type Descriptor) that is being
                            constructed.
         builtin_info     - builtin_info provides information on the built-in
                            (like precision, scale, charater set etc.). It is
                            obtained by allocating an OCIParam (parameter handle)
                            and setting type information in the OCIParam using
                             OCIAttrSet() calls.
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_ERROR on error.
   }
(* Const before type ignored *)
//  function TOCITypeAddAttr(svchp:POCISvcCtx; errhp:POCIError; _type:POCIType; a_name:Poratext; a_length:ub4;
//             attr_info:POCIParam):sword;

  {
     NAME: OCITypeAddAttr - OCI Type Add Attribute to an Object Type.
     REMARKS:
         Adds an attribute to an Object type (that was constructed earlier with
         typecode OCI_TYPECODE_OBJECT).
     PARAMETERS:
         svchp (IN)       - The OCI Service Context
         errhp (IN/OUT)   - The OCI error handle. If there is an error, it is
                            recorded in errhp and this function returns
                            OCI_ERROR. Diagnostic information can be obtained by
                            calling OCIErrorGet().
         type (IN/OUT)    - The Type description that is being constructed.
         a_name(IN)       - Optional. gives the name of the attribute.
         a_length         - Optional. gives length of attribute name.
         attr_info        - Information on the attribute. It is obtained by
                            allocating an OCIParam (parameter handle) and setting
                            type information in the OCIParam using OCIAttrSet()
                            calls.
    RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_ERROR on error.
   }
//  function TOCITypeEndCreate(svchp:POCISvcCtx; errhp:POCIError; _type:POCIType):sword;

  {
     NAME: OCITypeEndCreate - OCI Type End Creation
     REMARKS:
         Finishes construction of a type description.Subsequently, only access
         will be allowed.
     PARAMETERS:
         svchp (IN)       - The OCI Service Context
         errhp (IN/OUT)   - The OCI error handle. If there is an error, it is
                            recorded in errhp and this function returns
                            OCI_ERROR. Diagnostic information can be obtained by
                            calling OCIErrorGet().
         type (IN/OUT)    - The Type description that is being constructed.
     RETURNS:
          OCI_SUCCESS if the function completes successfully.
          OCI_ERROR on error.
   }
  {========================= }
  { PUBLIC MACROS AND FLAGS  }
  {========================= }
  {-------------------------------------------------------------------------- }
  {                          TYPE ELEMENT FLAGS                               }
  {-------------------------------------------------------------------------- }
  { element is a REF  }
  const
    OCI_TYPEELEM_REF = $8000;
  { parameter is required  }
    OCI_TYPEPARAM_REQUIRED = $0800;
  { macros to test flags  }
  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }
type
  TOCI_TYPEELEM_IS_REF = function(elem_flag : longint) : longint;

  { was #define dname(params) para_def_expr }
  { argument types are unknown }
  { return type might be wrong }
  TOCI_TYPEPARAM_IS_REQUIRED = function (param_flag : longint) : longint;

{$ENDIF ZEOS_DISABLE_ORACLE}

implementation

end.

