{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Native Plain Drivers for Oracle             }
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

unit ZPlainOracleDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_ORACLE}
{$J+}

uses
  ZPlainLoader, ZCompatibility, ZPlainOracleConstants, ZPlainDriver;

{***************** Plain API types definition ****************}

const
  WINDOWS_DLL_LOCATION = 'oci.dll';
//  WINDOWS_DLL_LOCATION = 'ora803.dll';
  LINUX_DLL_LOCATION = 'libclntsh'+SharedSuffix;
//  LINUX_DLL_LOCATION = 'libwtc8.so';

type

  {** Represents a generic interface to Oracle native API. }
  IZOraclePlainDriver = interface (IZPlainDriver)
    ['{22404660-C95F-4346-A3DB-7C6DFE15F115}']
    function EnvNlsCreate(var envhpp: POCIEnv; mode: ub4; ctxp: Pointer;
      malocfp: Pointer; ralocfp: Pointer; mfreefp: Pointer; xtramemsz: size_T;
      usrmempp: PPointer; charset, ncharset: ub2): sword;
    function ServerAttach(srvhp: POCIServer; errhp: POCIError; dblink: text;
      dblink_len: sb4; mode: ub4): sword;
    function ServerDetach(srvhp: POCIServer; errhp: POCIError;
      mode: ub4): sword;
    function ServerRelease(hndlp: POCIHandle;
      errhp: POCIError; bufp: text; bufsz: ub4; hndltype: ub1; version:pointer): sword;
    function ServerVersion(hndlp: POCIHandle; errhp: POCIError; bufp: text;
      bufsz: ub4; hndltype: ub1): sword;

    function SessionBegin(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; credt: ub4; mode: ub4):sword;
    function SessionEnd(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; mode: ub4): sword;

    function TransStart(svchp: POCISvcCtx; errhp: POCIError; timeout: word;
      flags: ub4): sword;
    function TransRollback(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;
    function TransCommit(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;

    function Ping(svchp: POCISvcCtx; errhp: POCIError; mode: ub4 = OCI_DEFAULT): sword;
    function PasswordChange(svchp: POCISvcCtx; errhp: POCIError;
      user_name: text; usernm_len: ub4; opasswd: text; opasswd_len: ub4;
      npasswd: text; npasswd_len: sb4; mode: ub4): sword;

    procedure ClientVersion(major_version, minor_version, update_num,
      patch_num, port_update_num: psword);

    function HandleAlloc(parenth: POCIHandle; var hndlpp: POCIHandle;
      atype: ub4; xtramem_sz: size_T; usrmempp: PPointer): sword;
    function HandleFree(hndlp: Pointer; atype: ub4): sword;
    function ErrorGet(hndlp: Pointer; recordno: ub4; sqlstate: text;
      var errcodep: sb4; bufp: text; bufsiz: ub4; atype: ub4): sword;

    function AttrSet(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; size: ub4; attrtype: ub4; errhp: POCIError): sword;
    function AttrGet(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; sizep: Pointer; attrtype: ub4;
      errhp: POCIError): sword;
    function NlsNumericInfoGet(envhp: POCIEnv; errhp: POCIError; val: psb4; item: ub2): sword;
    {statement api}
    function StmtPrepare(stmtp: POCIStmt; errhp: POCIError; stmt: text;
      stmt_len: ub4; language:ub4; mode: ub4):sword;
    function StmtPrepare2(svchp: POCISvcCtx; var stmtp: POCIStmt; errhp: POCIError;
      stmt: text; stmt_len: ub4; key: text; key_len: ub4;
      language:ub4; mode: ub4): sword;
    function StmtRelease(stmtp: POCIStmt; errhp: POCIError; key: text; key_len: ub4;
      mode: ub4):sword;
    function StmtExecute(svchp: POCISvcCtx; stmtp: POCIStmt;
      errhp: POCIError; iters: ub4; rowoff: ub4; snap_in: POCISnapshot;
      snap_out: POCISnapshot; mode: ub4): sword;
    function ParamGet(hndlp: Pointer; htype: ub4; errhp: POCIError;
      var parmdpp: Pointer; pos: ub4): sword;
    function StmtFetch(stmtp: POCIStmt; errhp: POCIError; nrows: ub4;
      orientation: ub2; mode: ub4): sword;
    function StmtFetch2(stmtp: POCIStmt; errhp: POCIError; const nrows: ub4;
      const orientation: ub2; const fetchOffset: sb4; const mode: ub4): sword;
    function DefineByPos(stmtp: POCIStmt; var defnpp: POCIDefine;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; rlenp: Pointer; rcodep: Pointer; mode: ub4): sword;
    function BindByPos(stmtp: POCIStmt; var bindpp: POCIBind;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; alenp: Pointer; rcodep: Pointer; maxarr_len: ub4;
      curelep: Pointer; mode: ub4): sword;
    function BindObject(bindp: POCIBind; errhp: POCIError;
                    const _type: POCIType; pgvpp: PPointer;
                    pvszsp: pub4; indpp: PPointer;
                    indszp: pub4): sword;
    function ResultSetToStmt(rsetdp: POCIHandle; errhp: POCIError): sword;

    function DefineObject(defnpp: POCIDefine; errhp: POCIError;
      _type: POCIHandle; pgvpp, pvszsp, indpp, indszp: pointer): sword;
    {descriptors}
    function DescriptorAlloc(const parenth: POCIEnv; var descpp: POCIDescriptor;
      const htype: ub4; const xtramem_sz: integer; usrmempp: Pointer): sword;
    function DescriptorFree(const descp: Pointer; const htype: ub4): sword;
    {Lob}
    function LobOpen(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; mode: ub1): sword;
    function LobRead(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword;
    function LobTrim(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; newlen: ub4): sword;
    function LobWrite(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      piece: ub1; ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword;
    function LobCreateTemporary(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; csid: ub2; csfrm: ub1; lobtype: ub1;
      cache: LongBool; duration: OCIDuration): sword;
    function LobIsTemporary(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var is_temporary: LongBool): sword;
    function LobFreeTemporary(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword;
    function LobCharSetForm ( envhp: POCIEnv; errhp: POCIError;
        const locp: POCILobLocator; csfrm: pub1): sword;
    function LobCharSetId ( envhp: POCIEnv; errhp: POCIError;
        const locp: POCILobLocator; csid: pub2): sword;
    function LobClose(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword;
    {DateTime api}
    function IntervalGetYearMonth(hndl: POCIHandle; err: POCIError; yr,mnth: psb4;
                        const int_result: POCIInterval): sword;
    function IntervalGetDaySecond(hndl: POCIHandle; err:  POCIError; dy: psb4;
      hr, mm, ss, fsec: psb4; const int_result: POCIInterval): sword;
    function DateTimeConstruct(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; year: sb2; month: ub1; day: ub1; hour: ub1;
      min: ub1; sec: ub1; fsec: ub4; timezone: text;
      timezone_length: size_t): sword;
    function DateTimeGetDate(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; var year: sb2; var month: ub1;
      var day: ub1): sword;
    function DateTimeGetTime(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; var hour: ub1; var minute: ub1; var sec: ub1;
      var fsec: ub4): sword;

    {object api}
    function TypeByRef(env: POCIEnv; err: POCIError; type_ref: POCIRef;
      pin_duration: OCIDuration; get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword;
    function ObjectNew(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
                       typecode: OCITypeCode; tdo: POCIType; table: Pointer;
                       duration: OCIDuration; value: Longbool;
                       instance: PPointer): sword;
    function ObjectPin(env: POCIEnv; err: POCIError;
      const object_ref: POCIRef; const corhdl: POCIComplexObject;
      const pin_option: OCIPinOpt; const pin_duration: OCIDuration;
      const lock_option: OCILockOpt; _object: PPointer): sword;
    function ObjectUnpin(env: POCIEnv; err: POCIError;
      const _object: Pointer): sword;
    function ObjectFree(hndl: POCIEnv; err: POCIError;
      instance: POCIHandle;flags :ub2):sword;
    function ObjectGetTypeRef(env: POCIEnv; err: POCIError;
      const instance:pointer; type_ref: POCIRef): sword;

    function DescribeAny(svchp: POCISvcCtx; errhp: POCIError;
      objptr: Pointer; objnm_len: ub4; objptr_typ: ub1; info_level: ub1;
      objtyp: ub1; dschp: POCIDescribe): sword;

    (* excluded unused api
    function TransDetach(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;
    function TransPrepare(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;
    function TransForget(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;

    function Break(svchp: POCISvcCtx; errhp:POCIError): sword;
    function Reset(svchp: POCISvcCtx; errhp:POCIError): sword;

    function ObjectUnmark(env: POCIEnv; err: POCIError;
      const _object:pointer): sword;
    function ObjectUnmarkByRef(env: POCIEnv; err: POCIError;
      const ref: POCIRef): sword;
    function ObjectMarkDeleteByRef(env: POCIEnv; err: POCIError;
      const object_ref:POCIRef): sword;
    function ObjectMarkDelete(env: POCIEnv; err: POCIError;
      const instance:pointer): sword;
    function ObjectFlush(env: POCIEnv; err: POCIError;
      const _object: pointer): sword;
    function ObjectRefresh(env: POCIEnv; err: POCIError;
      _object: pointer): sword;
    function ObjectCopy(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
      const source, null_source, target, null_target: pointer; const tdo: POCIType;
      const duration: OCIDuration; const option: ub1): sword;
    function ObjectGetObjectRef(env: POCIEnv; err: POCIError;
      const _object: pointer; object_ref: POCIRef): sword;
    function ObjectMakeObjectRef(env: POCIEnv; err: POCIError;
      const svc: POCISvcCtx; const table: pointer; const values: PPointer;
      const array_len: ub4; object_ref: POCIRef): sword;
    function ObjectGetPrimaryKeyTypeRef(env: POCIEnv; err: POCIError;
      const svc:POCISvcCtx; const table: pointer; type_ref: POCIRef): sword;
    function ObjectGetInd(env: POCIEnv; err: POCIError;
      const instance: pointer; null_struct: PPointer): sword;
    function ObjectExists(env: POCIEnv; err: POCIError; const ins: pointer;
      exist: PBoolean): sword;
    function ObjectGetProperty(envh: POCIEnv; errh: POCIError;
      const obj: pointer; const propertyId: OCIObjectPropId;
      _property: pointer; size: Pub4): sword;
    function ObjectIsLocked(env: POCIEnv; err: POCIError; const ins: pointer;
      lock: Pboolean): sword;
    function ObjectIsDirty(env: POCIEnv; err: POCIError; const ins: pointer;
      dirty: PBoolean): sword;
    function ObjectPinTable(env: POCIEnv; err: POCIError;
      const svc:POCISvcCtx; const schema_name: Poratext; const s_n_length: ub4;
      const object_name: Poratext; const o_n_length:ub4;
      const scope_obj_ref: POCIRef; const pin_duration: OCIDuration;
      _object: PPointer): sword;
    function ObjectArrayPin(env: POCIEnv; err: POCIError;
      const ref_array: PPOCIRef; const array_size: ub4;
      const cor_array: PPOCIComplexObject; const cor_array_size: ub4;
      const pin_option: OCIPinOpt; const pin_duration: OCIDuration;
      const lock: OCILockOpt; obj_array: PPointer;
      pos: Pub4): sword;
    function CacheFlush(env: POCIEnv; err: POCIError; const svc:POCISvcCtx;
      const context: pointer; const get: TOCICacheFlushGet;
      ref: PPOCIRef): sword;
    function CacheRefresh(env: POCIEnv; err: POCIError;
      const svc:POCISvcCtx; const option: OCIRefreshOpt; const context: pointer;
      get: TOCICacheRefreshGet; ref: PPOCIRef): sword;
    function CacheUnpin(env: POCIEnv; err: POCIError;
      const svc:POCISvcCtx): sword;
    function CacheFree(env: POCIEnv; err: POCIError;
      const svc: POCISvcCtx): sword;
    function CacheUnmark(env: POCIEnv; err: POCIError;
      const svc: POCISvcCtx): sword;
    function DurationBegin(env: POCIEnv; err: POCIError;
      svc: POCISvcCtx; const parent: OCIDuration;
      dur: POCIDuration): sword;
    function DurationEnd(env: POCIEnv; err: POCIError; svc: POCISvcCtx;
      duration: OCIDuration): sword;
    { < ori.h}
    function DateTimeAssign(hndl: POCIEnv; err: POCIError;
      const from: POCIDateTime;_to: POCIDateTime): sword;
    function DateTimeCheck(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; var valid: ub4): sword;
    function DateTimeCompare(hndl: POCIEnv; err: POCIError;
      const date1: POCIDateTime; const date2: POCIDateTime;
      var result: sword): sword;
    function DateTimeConvert(hndl: POCIEnv; err: POCIError;
      indate: POCIDateTime; outdate: POCIDateTime): sword;
    function DateTimeFromText(hndl: POCIEnv; err: POCIError;
      const date_str: text; d_str_length: size_t; const fmt: text;
      fmt_length: ub1; const lang_name: text; lang_length: size_t;
      date: POCIDateTime): sword;
    function DateTimeGetTimeZoneOffset(hndl: POCIEnv; err: POCIError;
      const datetime: POCIDateTime; var hour: sb1; var minute: sb1): sword;
    function DateTimeSysTimeStamp(hndl: POCIEnv; err: POCIError;
      sys_date: POCIDateTime): sword;
    function DateTimeToText(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; const fmt: text; fmt_length: ub1;
      fsprec: ub1; const lang_name: text; lang_length: size_t;
      var buf_size: ub4; buf: text): sword;
    function DateTimeGetTimeZoneName(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; var buf: ub1; var buflen: ub4): sword;

    function LobAppend(svchp: POCISvcCtx; errhp: POCIError; dst_locp,
      src_locp: POCILobLocator): sword;
    function LobAssign(svchp: POCISvcCtx; errhp: POCIError;
      src_locp: POCILobLocator; var dst_locpp: POCILobLocator): sword;
    function LobCopy(svchp: POCISvcCtx; errhp: POCIError;
      dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: ub4;
      dst_offset: ub4; src_offset: ub4): sword;
    function LobEnableBuffering(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword;
    function LobDisableBuffering(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword;
    function LobErase(svchp: POCISvcCtx; errhp: POCIError; locp: POCILobLocator;
      var amount: ub4; offset: ub4): sword;
    function LobFileExists(svchp: POCISvcCtx; errhp: POCIError;
      filep: POCILobLocator; var flag: Boolean): sword;
    function LobFileGetName(envhp: POCIEnv; errhp: POCIError;
      filep: POCILobLocator; dir_alias: text; var d_length: ub2; filename: text;
      var f_length: ub2): sword;
    function LobFileSetName(envhp: POCIEnv; errhp: POCIError;
      var filep: POCILobLocator; dir_alias: text; d_length: ub2; filename: text;
      f_length: ub2): sword;
    function LobFlushBuffer(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; flag: ub4): sword;
    function LobGetLength(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var lenp: ub4): sword;
    function LobIsOpen(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var flag: LongBool): sword;
    function LobLoadFromFile(svchp: POCISvcCtx; errhp: POCIError;
      dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: ub4;
      dst_offset: ub4; src_offset: ub4): sword;
    function LobLocatorIsInit(envhp: POCIEnv; errhp: POCIError;
     locp: POCILobLocator; var is_initialized: LongBool): sword;

    function StmtGetPieceInfo(stmtp: POCIStmt; errhp: POCIError;
      var hndlpp: Pointer; var typep: ub4; var in_outp: ub1; var iterp: ub4;
      var idxp: ub4; var piecep: ub1): sword;
    function StmtSetPieceInfo(handle: Pointer; typep: ub4; errhp: POCIError;
      buf: Pointer; var alenp: ub4; piece: ub1; indp: Pointer;
      var rcodep: ub2): sword;
    function NumberInc(err: POCIError; number: POCINumber): sword;
    function NumberDec(err: POCIError; number: POCINumber): sword;
    procedure NumberSetZero(err: POCIError; number: POCINumber);
    procedure NumberSetPi(err: POCIError; number: POCINumber);
    function  NumberAdd(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberSub(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberMul(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberDiv(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberMod(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberIntPower(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberShift(err: POCIError; const number: POCINumber;
      const nDig: sword; _result: POCINumber): sword;
    function NumberNeg(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberToText(err: POCIError; const number: POCINumber;
      const fmt: Poratext; fmt_length: ub4; const nls_params: Poratext;
      nls_p_length: ub4; buf_size: pub4; buf: poratext): sword;
    function NumberFromText(err: POCIError; const str: poratext;
      str_length: ub4; const fmt: poratext; fmt_length: ub4;
      const nls_params: poratext; nls_p_length: ub4; number: POCINumber): sword;
    function NumberToInt(err: POCIError; const number: POCINumber;
      rsl_length: uword; rsl_flag: uword; rsl: Pointer): sword;
    function NumberFromInt(err: POCIError; const inum: Pointer;
      inum_length: uword; inum_s_flag: uword; number: POCINumber): sword;
    function NumberToReal(err: POCIError; const number: POCINumber;
      rsl_length: uword; rsl: Pointer): sword;
    function NumberToRealArray(err: POCIError; const number: PPOCINumber;
      elems: uword; rsl_length: uword; rsl: Pointer): sword;
    function NumberFromReal(err: POCIError; const rnum: Pointer;
      rnum_length: uword; number: POCINumber): sword;
    function NumberCmp(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: psword): sword;
    function NumberSign(err: POCIError; const number: POCINumber;
      _result: psword): sword;
    function NumberIsZero(err: POCIError; const number: POCINumber;
      _Result: pboolean): sword;
    function NumberIsInt(err: POCIError; const number: POCINumber;
      _result: Pboolean): sword;
    function NumberAssign(err: POCIError; const from: POCINumber;
      _to: POCINumber): sword;
    function NumberAbs(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberCeil(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberFloor(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberSqrt(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberTrunc(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberPower(err: POCIError; const base: POCINumber;
      const number: POCINumber; _result: POCINumber): sword;
    function NumberRound(err: POCIError; const number: POCINumber;
      decplace: sword; _result: POCINumber): sword;
    function NumberPrec(err: POCIError; const number: POCINumber;
      nDigs: sword; _result: POCINumber): sword;
    function NumberSin(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberArcSin(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberHypSin(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberCos(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberArcCos(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberHypCos(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberTan(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberArcTan(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberArcTan2(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberHypTan(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberExp(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberLn(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberLog(err: POCIError; const base: POCINumber;
      const number: POCINumber; _result: POCINumber): sword;

    function TableSize(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                    size: psb4): sword;
    function TableExists(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                             index: sb4; exists: PBoolean): sword;
    function TableDelete(hndl: POCIEnv; err: POCIError; index: sb4;
                      tbl: POCITable): sword;
    function TableFirst(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                             index: sb4): sword;
    function TableLast(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                             index: sb4): sword;
    function TableNext(hndl: POCIEnv; err: POCIError; index: sb4;
      const tbl: POCITable; next_index: psb4; exists: PBoolean): sword;
    function TablePrev(hndl: POCIEnv; err: POCIError; index: sb4;
      const tbl: POCITable; prev_index: psb4; exists: PBoolean): sword;
    function ObjectSetAttr(env: POCIEnv; err: POCIError; instance: Pointer;
                  null_struct: pointer; tdo: POCIType; const names: PPAnsiChar;
                  const lengths: pub4; const name_count: ub4;
                  const indexes: pub4; const index_count: ub4;
                  const null_status: POCIInd; const attr_null_struct: Pointer;
                  const attr_value: Pointer): sword; cdecl;
    function ObjectGetAttr(env: POCIEnv; err: POCIError; instance: Pointer;
                  null_struct: Pointer; tdo: POCIType;
                  const names: PPoratext; const lengths: pub4;
                  const name_count: ub4; const indexes: pub4;
                  const index_count: ub4; attr_null_status: POCIInd;
                  attr_null_struct, attr_value: PPointer;
                  attr_tdo: PPOCIType): sword;
    {ociap.h}
    {ort.h}
    function TypeIterNew(env: POCIEnv; err: POCIError; const tdo: POCIType;
                      iterator_ort: PPOCITypeIter):sword;
    function TypeIterSet(env: POCIEnv; err: POCIError; const tdo: POCIType;
                              iterator_ort: POCITypeIter): sword;
    function TypeIterFree(env: POCIEnv; err: POCIError;
                        iterator_ort: POCITypeIter): sword;
    function TypeByName(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
      schema_name: Poratext; const s_length: ub4; const type_name: Poratext;
      const t_length: ub4; version_name: Poratext; const v_length: ub4;
      const pin_duration: OCIDuration; const get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword;
    function TypeArrayByName(env: POCIEnv; err: POCIError; svc: POCISvcCtx;
      array_len: ub4; schema_name:  PPoratext; s_length: Pub4;
      type_name: PPoratext; t_length: Pub4; version_name: PPoratext;
      v_length: Pub4; pin_duration: OCIDuration; get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword;
    function TypeArrayByRef(env: POCIEnv; err: POCIError; array_len: ub4;
      type_ref: PPOCIRef; pin_duration: OCIDuration; get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword;
    function TypeName(env: POCIEnv; err: POCIError; tdo: POCIType;
      n_length: Pub4): poratext;
    function TypeSchema(env: POCIEnv; err: POCIError; const tdo: POCIType;
      n_length: Pub4): poratext;
    function TypeTypeCode(env: POCIEnv; err: POCIError;
                  const tdo: POCIType): OCITypeCode;
    function TypeCollTypeCode(env:POCIEnv; err:POCIError;
      const tdo: POCIType): OCITypeCode;
    function TypeVersion(env: POCIEnv; err: POCIError; const tdo: POCIType;
      v_length: Pub4): poratext;
    function TypeAttrs(env: POCIEnv; err: POCIError;
      const tdo:POCIType): ub4;
    function TypeMethods(env: POCIEnv; err: POCIError;
      const tdo: POCIType): ub4;
    function TypeElemName(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem; n_length:Pub4): poratext;
    function TypeElemTypeCode(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): OCITypeCode;
    function TypeElemType(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem; elem_tdo:PPOCIType): sword;
    function TypeElemFlags(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): ub4;
    function TypeElemNumPrec(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): ub1;
    function TypeElemNumScale(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): sb1;
    function TypeElemLength(env: POCIEnv; err: POCIError;
      const elem:POCITypeElem): ub4;
    function TypeElemCharSetID(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): ub2;
    function TypeElemCharSetForm(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): ub2;
    function TypeElemParameterizedType(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem; type_stored: PPOCIType): sword;
    function TypeElemExtTypeCode(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): OCITypeCode;
    function TypeAttrByName(env: POCIEnv; err: POCIError;
      const tdo: POCIType; const name: Poratext; const n_length: ub4;
      elem: PPOCITypeElem): sword;
    function TypeAttrNext(env: POCIEnv; err: POCIError;
      iterator_ort: POCITypeIter; elem: PPOCITypeElem): sword;
    function TypeCollElem(env: POCIEnv; err: POCIError; const tdo:POCIType;
      element: PPOCITypeElem): sword;
    function TypeCollSize(env: POCIEnv; err: POCIError; const tdo: POCIType;
      num_elems: Pub4): sword;
    function TypeCollExtTypeCode(env: POCIEnv; err: POCIError;
      const tdo:POCIType; sqt_code: POCITypeCode): sword;
    function TypeMethodOverload(env: POCIEnv; err: POCIError;
      const tdo: POCIType; const method_name: Poratext;
      const m_length: ub4): ub4;
    function TypeMethodByName(env: POCIEnv; err: POCIError;
      const tdo: POCIType; const method_name: Poratext; const m_length: ub4;
      mdos: PPOCITypeMethod): sword;
    function TypeMethodNext(env: POCIEnv; err: POCIError;
      iterator_ort: POCITypeIter; mdo: PPOCITypeMethod): sword;
    function TypeMethodName(env:POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; n_length: Pub4): poratext;
    function TypeMethodEncap(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod): OCITypeEncap;
    function TypeMethodFlags(env: POCIEnv; err: POCIError;
        const mdo:POCITypeMethod): OCITypeMethodFlag;
    function TypeMethodMap(env: POCIEnv; err: POCIError; const tdo: POCIType;
      mdo: PPOCITypeMethod): sword;
    function TypeMethodOrder(env: POCIEnv; err: POCIError;
      const tdo: POCIType; mdo: PPOCITypeMethod): sword;
    function TypeMethodParams(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod): ub4;
    function TypeResult(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; elem: PPOCITypeElem): sword;
    function TypeParamByPos(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; const position: ub4;
      elem: PPOCITypeElem): sword;
    function TypeParamByName(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; const name: Poratext; const n_length: ub4;
      elem:PPOCITypeElem): sword;
    function TypeParamPos(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; const name: Poratext; const n_length: ub4;
      position: Pub4; elem: PPOCITypeElem): sword;
    function TypeElemParamMode(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): OCITypeParamMode;
    function TypeElemDefaultValue(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem; d_v_length: Pub4): poratext;
    function TypeVTInit(env: POCIEnv; err: POCIError): sword;
    function TypeVTInsert(env: POCIEnv; err: POCIError;
      const schema_name: Poratext; const s_n_length: ub4;
      const type_name: Poratext; const t_n_length: ub4;
      const user_version:Poratext; const u_v_length:ub4): sword;
    function TypeVTSelect(env: POCIEnv; err: POCIError;
      const schema_name: Poratext; const s_n_length: ub4;
      const type_name: Poratext; const t_n_length: ub4; user_version: PPoratext;
      u_v_length: Pub4; version: Pub2): sword; *)
  end;

  {** Implements a driver for Oracle 9i }
  TZOracle9iPlainDriver = class (TZAbstractPlainDriver, IZPlainDriver,
    IZOraclePlainDriver)
  private
    {api definitions} //EH: if we would drop the interface overheap we simply
      //could move the defs up to public section, omit starting 'OCI'
      //and have native access without addition calls just like inline code
      //10mio calls loose = 1,5 sec with a fast i7 CPU
    OCIInitialize: function(mode: ub4; ctxp: Pointer; malocfp: Pointer;
      ralocfp: Pointer; mfreefp: Pointer): sword; cdecl;
    OCIEnvNlsCreate: function(var envhpp: POCIEnv; mode: ub4; ctxp: Pointer;
      malocfp: Pointer; ralocfp: Pointer; mfreefp: Pointer; xtramemsz: size_T;
      usrmempp: PPointer; charset, ncharset: ub2): sword; cdecl;

    OCIServerAttach: function(srvhp: POCIServer; errhp: POCIError; dblink: text;
      dblink_len: sb4; mode: ub4): sword; cdecl;
    OCIServerDetach: function(srvhp: POCIServer; errhp: POCIError;
      mode: ub4): sword; cdecl;
    OCIServerRelease: function(hndlp: POCIHandle; errhp: POCIError; bufp: text;
      bufsz: ub4; hndltype: ub1; version:pointer): sword; cdecl;
    OCIServerVersion: function(hndlp: POCIHandle; errhp: POCIError; bufp: text;
      bufsz: ub4; hndltype: ub1): sword; cdecl;

    OCISessionBegin: function(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; credt: ub4; mode: ub4):sword; cdecl;
    OCISessionEnd: function(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; mode: ub4): sword; cdecl;
    OCITransStart: function(svchp: POCISvcCtx; errhp: POCIError; timeout: word;
      flags: ub4): sword; cdecl;
    OCITransRollback: function(svchp:POCISvcCtx; errhp:POCIError;
      flags: ub4): sword; cdecl;
    OCITransCommit: function(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4) :sword; cdecl;

    OCIPing: function(svchp: POCISvcCtx; errhp: POCIError; mode: ub4): sword; cdecl;
    OCIPasswordChange: function(svchp: POCISvcCtx; errhp: POCIError;
      user_name: text; usernm_len: ub4; opasswd: text; opasswd_len: ub4;
      npasswd: text; npasswd_len: sb4; mode: ub4): sword; cdecl;

    OCIClientVersion: procedure(major_version, minor_version, update_num,
      patch_num, port_update_num: psword); cdecl;
    OCIHandleAlloc: function(parenth: POCIHandle; var hndlpp: POCIHandle;
      atype: ub4; xtramem_sz: size_T; usrmempp: PPointer): sword; cdecl;
    OCIHandleFree: function(hndlp: Pointer; atype: ub4): sword; cdecl;

    OCIErrorGet: function(hndlp: Pointer; recordno: ub4; sqlstate: text;
      var errcodep: sb4; bufp: text; bufsiz: ub4; atype: ub4): sword; cdecl;

    OCIAttrSet: function(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; size: ub4; attrtype: ub4; errhp: POCIError):sword; cdecl;
    OCIAttrGet: function(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; sizep: Pointer; attrtype: ub4;
      errhp: POCIError):sword; cdecl;
    OCINlsNumericInfoGet: function(envhp: POCIEnv; errhp: POCIError; val: psb4;
      item: ub2): sword; cdecl;
    OCIStmtPrepare: function(stmtp: POCIStmt; errhp: POCIError; stmt: text;
      stmt_len: ub4; language:ub4; mode: ub4):sword; cdecl;

    OCIStmtPrepare2: function(svchp: POCISvcCtx; var stmtp: POCIStmt; errhp: POCIError;
      stmt: text; stmt_len: ub4; key: text; key_len: ub4;
      language:ub4; mode: ub4): sword; cdecl;

    OCIStmtRelease: function(stmtp: POCIStmt; errhp: POCIError; key: text;
      key_len: ub4; mode: ub4):sword; cdecl;

    OCIStmtExecute: function(svchp: POCISvcCtx; stmtp: POCIStmt;
      errhp: POCIError; iters: ub4; rowoff: ub4; snap_in: POCISnapshot;
      snap_out: POCISnapshot; mode: ub4): sword; cdecl;

    OCIParamGet: function(hndlp: Pointer; htype: ub4; errhp: POCIError;
      var parmdpp: Pointer; pos: ub4): sword; cdecl;

    OCIStmtFetch: function(stmtp: POCIStmt; errhp: POCIError; nrows: ub4;
      orientation: ub2; mode: ub4): sword; cdecl;

    OCIStmtFetch2: function(stmtp: POCIStmt; errhp: POCIError; const nrows: ub4;
      const orientation: ub2; const fetchOffset: sb4; const mode: ub4): sword; cdecl;

    OCIDefineByPos: function(stmtp: POCIStmt; var defnpp: POCIDefine;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; rlenp: Pointer; rcodep: Pointer; mode: ub4): sword; cdecl;
    OCIBindByPos: function(stmtp: POCIStmt; var bindpp: POCIBind;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; alenp: Pointer; rcodep: Pointer; maxarr_len: ub4;
      curelep: Pointer; mode: ub4): sword; cdecl;
    OCIBindObject: function(bindp: POCIBind; errhp: POCIError; const _type:
      POCIType; pgvpp: PPointer; pvszsp: pub4; indpp: PPointer;
      indszp: pub4): sword; cdecl;
    OCIDefineObject: function(defnpp: POCIDefine; errhp: POCIError;
      _type: POCIHandle; pgvpp: pointer; pvszsp: Pub4; indpp: pointer;
      indszp: Pub4):sword; cdecl;
    OCIResultSetToStmt: function(rsetdp: POCIHandle; errhp: POCIError): sword; cdecl;

    OCIDescriptorAlloc: function(parenth: POCIEnv; var descpp: POCIDescriptor;
      htype: ub4; xtramem_sz: size_t; usrmempp: Pointer): sword; cdecl;
    OCIDescriptorFree: function(descp: Pointer; htype: ub4): sword; cdecl;

    OCILobOpen: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; mode: ub1): sword; cdecl;
    OCILobRead: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;
    OCILobTrim: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; newlen: ub4): sword; cdecl;
    OCILobWrite: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      piece: ub1; ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword; cdecl;
    OCILobCreateTemporary: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; csid: ub2; csfrm: ub1; lobtype: ub1;
      cache: LongBool; duration: OCIDuration): sword; cdecl;
    OCILobIsTemporary: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var is_temporary: LongBool): sword; cdecl;
    OCILobFreeTemporary: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword; cdecl;
    OCILobCharSetForm: function ( envhp: POCIEnv; errhp: POCIError;
          const locp: POCILobLocator; csfrm: pub1): sword; cdecl;
    OCILobCharSetId: function( envhp: POCIEnv; errhp: POCIError;
          const locp: POCILobLocator; csid: pub2): sword; cdecl;
    OCILobClose: function(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword; cdecl;
    OCIIntervalGetYearMonth: function (hndl: POCIHandle; err: POCIError; yr,mnth: psb4;
                          const int_result: POCIInterval): sword; cdecl;
    OCIIntervalGetDaySecond: function(hndl: POCIHandle; err:  POCIError; dy: psb4;
      hr, mm, ss, fsec: psb4; const int_result: POCIInterval): sword; cdecl;
    OCIDateTimeConstruct: function(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; year: sb2; month: ub1; day: ub1; hour: ub1;
      min: ub1; sec: ub1; fsec: ub4; timezone: text;
      timezone_length: size_t): sword; cdecl;
    OCIDateTimeGetDate: function(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; out year: sb2; out month: ub1;
      out day: ub1): sword; cdecl;
    OCIDateTimeGetTime: function(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; out hour: ub1; out minute: ub1; out sec: ub1;
      out fsec: ub4): sword; cdecl;
    { object api}
    OCITypeByRef: function(env: POCIEnv; err: POCIError; type_ref: POCIRef;
      pin_duration: OCIDuration; get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword; cdecl;
    OCIObjectNew: function(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
      const typecode: OCITypeCode; const tdo: POCIType; const table: Pointer;
      const duration: OCIDuration; const value: boolean;
      instance: PPointer): sword; cdecl;
    OCIObjectPin: function(env: POCIEnv; err: POCIError;
      const object_ref: POCIRef; const corhdl: POCIComplexObject;
      const pin_option: OCIPinOpt; const pin_duration: OCIDuration;
      const lock_option: OCILockOpt; _object: PPointer): sword; cdecl;
    OCIObjectUnpin: function(env: POCIEnv; err: POCIError;
      const _object: Pointer): sword; cdecl;
    OCIObjectFree: function(env: POCIEnv; err: POCIError;
      const instance: pointer; const flags: ub2): sword; cdecl;
    OCIObjectGetTypeRef: function(env: POCIEnv; err: POCIError;
      const instance:pointer; type_ref: POCIRef): sword; cdecl;
    OCIDescribeAny: function(svchp: POCISvcCtx; errhp: POCIError;
      objptr: Pointer; objnm_len: ub4; objptr_typ: ub1; info_level: ub1;
      objtyp: ub1; dschp: POCIDescribe): sword; cdecl;
  protected
    procedure LoadApi; override;
    function Clone: IZPlainDriver; override;
  public
    {interface required api}
    constructor Create;
    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;

    procedure Initialize(const Location: String); override;

    function EnvNlsCreate(var envhpp: POCIEnv; mode: ub4; ctxp: Pointer;
      malocfp: Pointer; ralocfp: Pointer; mfreefp: Pointer; xtramemsz: size_T;
      usrmempp: PPointer; charset, ncharset: ub2): sword;
    function ServerAttach(srvhp: POCIServer; errhp: POCIError; dblink: text;
      dblink_len: sb4; mode: ub4): sword;
    function ServerDetach(srvhp: POCIServer; errhp: POCIError;
      mode: ub4): sword;
    function ServerRelease(hndlp: POCIHandle;
      errhp: POCIError; bufp: text; bufsz: ub4; hndltype: ub1; version:pointer): sword;
    function ServerVersion(hndlp: POCIHandle; errhp: POCIError; bufp: text;
      bufsz: ub4; hndltype: ub1): sword;

    function SessionBegin(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; credt: ub4; mode: ub4):sword;
    function SessionEnd(svchp: POCISvcCtx; errhp: POCIError;
      usrhp: POCISession; mode: ub4): sword;

    function TransStart(svchp: POCISvcCtx; errhp: POCIError; timeout: word;
      flags: ub4): sword;
    function TransRollback(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;
    function TransCommit(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;

    function Ping(svchp: POCISvcCtx; errhp: POCIError; mode: ub4 = OCI_DEFAULT): sword;
    function PasswordChange(svchp: POCISvcCtx; errhp: POCIError;
      user_name: text; usernm_len: ub4; opasswd: text; opasswd_len: ub4;
      npasswd: text; npasswd_len: sb4; mode: ub4): sword;

    procedure ClientVersion(major_version, minor_version, update_num,
      patch_num, port_update_num: psword);

    function HandleAlloc(parenth: POCIHandle; var hndlpp: POCIHandle;
      atype: ub4; xtramem_sz: size_T; usrmempp: PPointer): sword;
    function HandleFree(hndlp: Pointer; atype: ub4): sword;
    function ErrorGet(hndlp: Pointer; recordno: ub4; sqlstate: text;
      var errcodep: sb4; bufp: text; bufsiz: ub4; atype: ub4): sword;

    function AttrSet(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; size: ub4; attrtype: ub4; errhp: POCIError): sword;
    function AttrGet(trgthndlp: POCIHandle; trghndltyp: ub4;
      attributep: Pointer; sizep: Pointer; attrtype: ub4;
      errhp: POCIError): sword;
    function NlsNumericInfoGet(envhp: POCIEnv; errhp: POCIError; val: psb4; item: ub2): sword;
    {statement api}
    function StmtPrepare(stmtp: POCIStmt; errhp: POCIError; stmt: text;
      stmt_len: ub4; language:ub4; mode: ub4):sword;
    function StmtPrepare2(svchp: POCISvcCtx; var stmtp: POCIStmt; errhp: POCIError;
      stmt: text; stmt_len: ub4; key: text; key_len: ub4;
      language:ub4; mode: ub4): sword;
    function StmtRelease(stmtp: POCIStmt; errhp: POCIError; key: text; key_len: ub4;
      mode: ub4):sword;
    function StmtExecute(svchp: POCISvcCtx; stmtp: POCIStmt;
      errhp: POCIError; iters: ub4; rowoff: ub4; snap_in: POCISnapshot;
      snap_out: POCISnapshot; mode: ub4): sword;
    function ParamGet(hndlp: Pointer; htype: ub4; errhp: POCIError;
      var parmdpp: Pointer; pos: ub4): sword;
    function StmtFetch(stmtp: POCIStmt; errhp: POCIError; nrows: ub4;
      orientation: ub2; mode: ub4): sword;
    function StmtFetch2(stmtp: POCIStmt; errhp: POCIError; const nrows: ub4;
      const orientation: ub2; const fetchOffset: sb4; const mode: ub4): sword;
    function DefineByPos(stmtp: POCIStmt; var defnpp: POCIDefine;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; rlenp: Pointer; rcodep: Pointer; mode: ub4): sword;
    function BindByPos(stmtp: POCIStmt; var bindpp: POCIBind;
      errhp: POCIError; position: ub4; valuep: Pointer; value_sz: sb4; dty: ub2;
      indp: Pointer; alenp: Pointer; rcodep: Pointer; maxarr_len: ub4;
      curelep: Pointer; mode: ub4): sword;
    function BindObject(bindp: POCIBind; errhp: POCIError;
                    const _type: POCIType; pgvpp: PPointer;
                    pvszsp: pub4; indpp: PPointer;
                    indszp: pub4): sword;

    function DefineObject(defnpp: POCIDefine; errhp: POCIError;
      _type: POCIHandle; pgvpp, pvszsp, indpp, indszp: pointer): sword;
    function ResultSetToStmt(rsetdp: POCIHandle; errhp: POCIError): sword;
    {descriptors}
    function DescriptorAlloc(const parenth: POCIEnv; var descpp: POCIDescriptor;
      const htype: ub4; const xtramem_sz: integer; usrmempp: Pointer): sword;
    function DescriptorFree(const descp: Pointer; const htype: ub4): sword;
    {Lob}
    function LobOpen(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; mode: ub1): sword;
    function LobRead(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword;
    function LobTrim(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; newlen: ub4): sword;
    function LobWrite(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer; bufl: ub4;
      piece: ub1; ctxp: Pointer; cbfp: Pointer; csid: ub2; csfrm: ub1): sword;
    function LobCreateTemporary(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; csid: ub2; csfrm: ub1; lobtype: ub1;
      cache: LongBool; duration: OCIDuration): sword;
    function LobIsTemporary(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var is_temporary: LongBool): sword;
    function LobFreeTemporary(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword;
    function LobCharSetForm ( envhp: POCIEnv; errhp: POCIError;
        const locp: POCILobLocator; csfrm: pub1): sword;
    function LobCharSetId ( envhp: POCIEnv; errhp: POCIError;
        const locp: POCILobLocator; csid: pub2): sword;
    function LobClose(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword;
    {DateTime api}
    function IntervalGetYearMonth(hndl: POCIHandle; err: POCIError; yr,mnth: psb4;
                        const int_result: POCIInterval): sword;
    function IntervalGetDaySecond(hndl: POCIHandle; err:  POCIError; dy: psb4;
      hr, mm, ss, fsec: psb4; const int_result: POCIInterval): sword;

    function DateTimeConstruct(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; year: sb2; month: ub1; day: ub1; hour: ub1;
      min: ub1; sec: ub1; fsec: ub4; timezone: text;
      timezone_length: size_t): sword;
    function DateTimeGetDate(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; var year: sb2; var month: ub1;
      var day: ub1): sword;
    function DateTimeGetTime(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; var hour: ub1; var minute: ub1; var sec: ub1;
      var fsec: ub4): sword;

    {object api}
    function TypeByRef(env: POCIEnv; err: POCIError; type_ref: POCIRef;
      pin_duration: OCIDuration; get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword;
    function ObjectNew(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
                       typecode: OCITypeCode; tdo: POCIType; table: Pointer;
                       duration: OCIDuration; value: Longbool;
                       instance: PPointer): sword;
    function ObjectPin(env: POCIEnv; err: POCIError;
      const object_ref: POCIRef; const corhdl: POCIComplexObject;
      const pin_option: OCIPinOpt; const pin_duration: OCIDuration;
      const lock_option: OCILockOpt; _object: PPointer): sword;
    function ObjectUnpin(env: POCIEnv; err: POCIError;
      const _object: Pointer): sword;
    function ObjectFree(hndl: POCIEnv; err: POCIError;
      instance: POCIHandle;flags :ub2):sword;
    function ObjectGetTypeRef(env: POCIEnv; err: POCIError;
      const instance:pointer; type_ref: POCIRef): sword;

    function DescribeAny(svchp: POCISvcCtx; errhp: POCIError;
      objptr: Pointer; objnm_len: ub4; objptr_typ: ub1; info_level: ub1;
      objtyp: ub1; dschp: POCIDescribe): sword;
   (* unused api
   function ObjectUnmark(env: POCIEnv; err: POCIError;
      const _object:pointer): sword;
   function ObjectUnmarkByRef(env: POCIEnv; err: POCIError;
      const ref: POCIRef): sword;
    function ObjectMarkDeleteByRef(env: POCIEnv; err: POCIError;
      const object_ref:POCIRef): sword;
    function ObjectMarkDelete(env: POCIEnv; err: POCIError;
      const instance:pointer): sword;
    function ObjectFlush(env: POCIEnv; err: POCIError;
      const _object: pointer): sword;
    function ObjectRefresh(env: POCIEnv; err: POCIError;
      _object: pointer): sword;
    function ObjectCopy(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
      const source, null_source, target, null_target: pointer; const tdo: POCIType;
      const duration: OCIDuration; const option: ub1): sword;
    function ObjectGetObjectRef(env: POCIEnv; err: POCIError;
      const _object: pointer; object_ref: POCIRef): sword;
    function ObjectMakeObjectRef(env: POCIEnv; err: POCIError;
      const svc: POCISvcCtx; const table: pointer; const values: PPointer;
      const array_len: ub4; object_ref: POCIRef): sword;
    function ObjectGetPrimaryKeyTypeRef(env: POCIEnv; err: POCIError;
      const svc:POCISvcCtx; const table: pointer; type_ref: POCIRef): sword;
    function ObjectGetInd(env: POCIEnv; err: POCIError;
      const instance: pointer; null_struct: PPointer): sword;
    function ObjectExists(env: POCIEnv; err: POCIError; const ins: pointer;
      exist: PBoolean): sword;
    function ObjectGetProperty(envh: POCIEnv; errh: POCIError;
      const obj: pointer; const propertyId: OCIObjectPropId;
      _property: pointer; size: Pub4): sword;
    function ObjectIsLocked(env: POCIEnv; err: POCIError; const ins: pointer;
      lock: Pboolean): sword;
    function ObjectIsDirty(env: POCIEnv; err: POCIError; const ins: pointer;
      dirty: PBoolean): sword;
    function ObjectPinTable(env: POCIEnv; err: POCIError;
      const svc:POCISvcCtx; const schema_name: Poratext; const s_n_length: ub4;
      const object_name: Poratext; const o_n_length:ub4;
      const scope_obj_ref: POCIRef; const pin_duration: OCIDuration;
      _object: PPointer): sword;
    function ObjectArrayPin(env: POCIEnv; err: POCIError;
      const ref_array: PPOCIRef; const array_size: ub4;
      const cor_array: PPOCIComplexObject; const cor_array_size: ub4;
      const pin_option: OCIPinOpt; const pin_duration: OCIDuration;
      const lock: OCILockOpt; obj_array: PPointer;
      pos: Pub4): sword;
    function CacheFlush(env: POCIEnv; err: POCIError; const svc:POCISvcCtx;
      const context: pointer; const get: TOCICacheFlushGet;
      ref: PPOCIRef): sword;
    function CacheRefresh(env: POCIEnv; err: POCIError;
      const svc:POCISvcCtx; const option: OCIRefreshOpt; const context: pointer;
      get: TOCICacheRefreshGet; ref: PPOCIRef): sword;
    function CacheUnpin(env: POCIEnv; err: POCIError;
      const svc:POCISvcCtx): sword;
    function CacheFree(env: POCIEnv; err: POCIError;
      const svc: POCISvcCtx): sword;
    function CacheUnmark(env: POCIEnv; err: POCIError;
      const svc: POCISvcCtx): sword;
    function DurationBegin(env: POCIEnv; err: POCIError;
      svc: POCISvcCtx; const parent: OCIDuration;
      dur: POCIDuration): sword;
    function DurationEnd(env: POCIEnv; err: POCIError; svc: POCISvcCtx;
      duration: OCIDuration): sword;
    { < ori.h}

    function TransDetach(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;
    function TransPrepare(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;
    function TransForget(svchp: POCISvcCtx; errhp: POCIError;
      flags: ub4): sword;

    function Break(svchp: POCISvcCtx; errhp:POCIError): sword;
    function Reset(svchp: POCISvcCtx; errhp:POCIError): sword;

    function DateTimeAssign(hndl: POCIEnv; err: POCIError;
      const from: POCIDateTime;_to: POCIDateTime): sword;
    function DateTimeCheck(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; var valid: ub4): sword;
    function DateTimeCompare(hndl: POCIEnv; err: POCIError;
      const date1: POCIDateTime; const date2: POCIDateTime;
      var _result: sword): sword;
    function DateTimeConvert(hndl: POCIEnv; err: POCIError;
      indate: POCIDateTime; outdate: POCIDateTime): sword;
    function DateTimeFromText(hndl: POCIEnv; err: POCIError;
      const date_str: text; d_str_length: size_t; const fmt: text;
      fmt_length: ub1; const lang_name: text; lang_length: size_t;
      date: POCIDateTime): sword;
    function DateTimeGetTimeZoneOffset(hndl: POCIEnv; err: POCIError;
      const datetime: POCIDateTime; var hour: sb1; var minute: sb1): sword;
    function DateTimeSysTimeStamp(hndl: POCIEnv; err: POCIError;
      sys_date: POCIDateTime): sword;
    function DateTimeToText(hndl: POCIEnv; err: POCIError;
      const date: POCIDateTime; const fmt: text; fmt_length: ub1;
      fsprec: ub1; const lang_name: text; lang_length: size_t;
      var buf_size: ub4; buf: text): sword;
    function DateTimeGetTimeZoneName(hndl: POCIEnv; err: POCIError;
      datetime: POCIDateTime; var buf: ub1; var buflen: ub4): sword;

    function LobAppend(svchp: POCISvcCtx; errhp: POCIError; dst_locp,
      src_locp: POCILobLocator): sword;
    function LobAssign(svchp: POCISvcCtx; errhp: POCIError;
      src_locp: POCILobLocator; var dst_locpp: POCILobLocator): sword;
    function LobCopy(svchp: POCISvcCtx; errhp: POCIError;
      dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: ub4;
      dst_offset: ub4; src_offset: ub4): sword;
    function LobEnableBuffering(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword;
    function LobDisableBuffering(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator): sword;
    function LobErase(svchp: POCISvcCtx; errhp: POCIError; locp: POCILobLocator;
      var amount: ub4; offset: ub4): sword;
    function LobFileExists(svchp: POCISvcCtx; errhp: POCIError;
      filep: POCILobLocator; var flag: Boolean): sword;
    function LobFileGetName(envhp: POCIEnv; errhp: POCIError;
      filep: POCILobLocator; dir_alias: text; var d_length: ub2; filename: text;
      var f_length: ub2): sword;
    function LobFileSetName(envhp: POCIEnv; errhp: POCIError;
      var filep: POCILobLocator; dir_alias: text; d_length: ub2; filename: text;
      f_length: ub2): sword;
    function LobFlushBuffer(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; flag: ub4): sword;
    function LobGetLength(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var lenp: ub4): sword;
    function LobIsOpen(svchp: POCISvcCtx; errhp: POCIError;
      locp: POCILobLocator; var flag: LongBool): sword;
    function LobLoadFromFile(svchp: POCISvcCtx; errhp: POCIError;
      dst_locp: POCILobLocator; src_locp: POCILobLocator; amount: ub4;
      dst_offset: ub4; src_offset: ub4): sword;
    function LobLocatorIsInit(envhp: POCIEnv; errhp: POCIError;
     locp: POCILobLocator; var is_initialized: LongBool): sword;

    function StmtGetPieceInfo(stmtp: POCIStmt; errhp: POCIError;
      var hndlpp: Pointer; var typep: ub4; var in_outp: ub1; var iterp: ub4;
      var idxp: ub4; var piecep: ub1): sword;
    function StmtSetPieceInfo(handle: Pointer; typep: ub4; errhp: POCIError;
      buf: Pointer; var alenp: ub4; piece: ub1; indp: Pointer;
      var rcodep: ub2): sword;

    function NumberInc(err: POCIError; number: POCINumber): sword;
    function NumberDec(err: POCIError; number: POCINumber): sword;
    procedure NumberSetZero(err: POCIError; number: POCINumber);
    procedure NumberSetPi(err: POCIError; number: POCINumber);
    function  NumberAdd(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberSub(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberMul(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberDiv(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberMod(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberIntPower(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberShift(err: POCIError; const number: POCINumber;
      const nDig: sword; _result: POCINumber): sword;
    function NumberNeg(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberToText(err: POCIError; const number: POCINumber;
      const fmt: Poratext; fmt_length: ub4; const nls_params: Poratext;
      nls_p_length: ub4; buf_size: pub4; buf: poratext): sword;
    function NumberFromText(err: POCIError; const str: poratext;
      str_length: ub4; const fmt: poratext; fmt_length: ub4;
      const nls_params: poratext; nls_p_length: ub4; number: POCINumber): sword;
    function NumberToInt(err: POCIError; const number: POCINumber;
      rsl_length: uword; rsl_flag: uword; rsl: Pointer): sword;
    function NumberFromInt(err: POCIError; const inum: Pointer;
      inum_length: uword; inum_s_flag: uword; number: POCINumber): sword;
    function NumberToReal(err: POCIError; const number: POCINumber;
      rsl_length: uword; rsl: Pointer): sword;
    function NumberToRealArray(err: POCIError; const number: PPOCINumber;
      elems: uword; rsl_length: uword; rsl: Pointer): sword;
    function NumberFromReal(err: POCIError; const rnum: Pointer;
      rnum_length: uword; number: POCINumber): sword;
    function NumberCmp(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: psword): sword;
    function NumberSign(err: POCIError; const number: POCINumber;
      _result: psword): sword;
    function NumberIsZero(err: POCIError; const number: POCINumber;
      _Result: pboolean): sword;
    function NumberIsInt(err: POCIError; const number: POCINumber;
      _result: Pboolean): sword;
    function NumberAssign(err: POCIError; const from: POCINumber;
      _to: POCINumber): sword;
    function NumberAbs(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberCeil(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberFloor(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberSqrt(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberTrunc(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberPower(err: POCIError; const base: POCINumber;
      const number: POCINumber; _result: POCINumber): sword;
    function NumberRound(err: POCIError; const number: POCINumber;
      decplace: sword; _result: POCINumber): sword;
    function NumberPrec(err: POCIError; const number: POCINumber;
      nDigs: sword; _result: POCINumber): sword;
    function NumberSin(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberArcSin(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberHypSin(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberCos(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberArcCos(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberHypCos(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberTan(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberArcTan(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberArcTan2(err: POCIError; const number1: POCINumber;
      const number2: POCINumber; _result: POCINumber): sword;
    function NumberHypTan(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberExp(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberLn(err: POCIError; const number: POCINumber;
      _result: POCINumber): sword;
    function NumberLog(err: POCIError; const base: POCINumber;
      const number: POCINumber; _result: POCINumber): sword;

    function TableSize(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                    size: psb4): sword;
    function TableExists(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                             index: sb4; exists: PBoolean): sword;
    function TableDelete(hndl: POCIEnv; err: POCIError; index: sb4;
                      tbl: POCITable): sword;
    function TableFirst(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                             index: sb4): sword;
    function TableLast(hndl: POCIEnv; err: POCIError; const tbl: POCITable;
                             index: sb4): sword;
    function TableNext(hndl: POCIEnv; err: POCIError; index: sb4;
      const tbl: POCITable; next_index: psb4; exists: PBoolean): sword;
    function TablePrev(hndl: POCIEnv; err: POCIError; index: sb4;
      const tbl: POCITable; prev_index: psb4; exists: PBoolean): sword;

    function ObjectSetAttr(env: POCIEnv; err: POCIError; instance: Pointer;
                  null_struct: pointer; tdo: POCIType; const names: PPAnsiChar;
                  const lengths: pub4; const name_count: ub4;
                  const indexes: pub4; const index_count: ub4;
                  const null_status: POCIInd; const attr_null_struct: Pointer;
                  const attr_value: Pointer): sword; cdecl;
    function ObjectGetAttr(env: POCIEnv; err: POCIError; instance: Pointer;
                  null_struct: Pointer; tdo: POCIType;
                  const names: PPoratext; const lengths: pub4;
                  const name_count: ub4; const indexes: pub4;
                  const index_count: ub4; attr_null_status: POCIInd;
                  attr_null_struct, attr_value: PPointer;
                  attr_tdo: PPOCIType): sword;
    {ociap.h}
    {ort.h}
    function TypeIterNew(env: POCIEnv; err: POCIError; const tdo: POCIType;
                      iterator_ort: PPOCITypeIter):sword;
    function TypeIterSet(env: POCIEnv; err: POCIError; const tdo: POCIType;
                              iterator_ort: POCITypeIter): sword;
    function TypeIterFree(env: POCIEnv; err: POCIError;
                        iterator_ort: POCITypeIter): sword;
    function TypeByName(env: POCIEnv; err: POCIError; const svc: POCISvcCtx;
      schema_name: Poratext; const s_length: ub4; const type_name: Poratext;
      const t_length: ub4; version_name: Poratext; const v_length: ub4;
      const pin_duration: OCIDuration; const get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword;
    function TypeArrayByName(env: POCIEnv; err: POCIError; svc: POCISvcCtx;
      array_len: ub4; schema_name:  PPoratext; s_length: Pub4;
      type_name: PPoratext; t_length: Pub4; version_name: PPoratext;
      v_length: Pub4; pin_duration: OCIDuration; get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword;
    function TypeArrayByRef(env: POCIEnv; err: POCIError; array_len: ub4;
      type_ref: PPOCIRef; pin_duration: OCIDuration; get_option: OCITypeGetOpt;
      tdo: PPOCIType): sword;
    function TypeName(env: POCIEnv; err: POCIError; tdo: POCIType;
      n_length: Pub4): poratext;
    function TypeSchema(env: POCIEnv; err: POCIError; const tdo: POCIType;
      n_length: Pub4): poratext;
    function TypeTypeCode(env: POCIEnv; err: POCIError;
                  const tdo: POCIType): OCITypeCode;
    function TypeCollTypeCode(env:POCIEnv; err:POCIError;
      const tdo: POCIType): OCITypeCode;
    function TypeVersion(env: POCIEnv; err: POCIError; const tdo: POCIType;
      v_length: Pub4): poratext;
    function TypeAttrs(env: POCIEnv; err: POCIError;
      const tdo:POCIType): ub4;
    function TypeMethods(env: POCIEnv; err: POCIError;
      const tdo: POCIType): ub4;
    function TypeElemName(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem; n_length:Pub4): poratext;
    function TypeElemTypeCode(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): OCITypeCode;
    function TypeElemType(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem; elem_tdo:PPOCIType): sword;
    function TypeElemFlags(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): ub4;
    function TypeElemNumPrec(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): ub1;
    function TypeElemNumScale(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): sb1;
    function TypeElemLength(env: POCIEnv; err: POCIError;
      const elem:POCITypeElem): ub4;
    function TypeElemCharSetID(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): ub2;
    function TypeElemCharSetForm(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): ub2;
    function TypeElemParameterizedType(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem; type_stored: PPOCIType): sword;
    function TypeElemExtTypeCode(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): OCITypeCode;
    function TypeAttrByName(env: POCIEnv; err: POCIError;
      const tdo: POCIType; const name: Poratext; const n_length: ub4;
      elem: PPOCITypeElem): sword;
    function TypeAttrNext(env: POCIEnv; err: POCIError;
      iterator_ort: POCITypeIter; elem: PPOCITypeElem): sword;
    function TypeCollElem(env: POCIEnv; err: POCIError; const tdo:POCIType;
      element: PPOCITypeElem): sword;
    function TypeCollSize(env: POCIEnv; err: POCIError; const tdo: POCIType;
      num_elems: Pub4): sword;
    function TypeCollExtTypeCode(env: POCIEnv; err: POCIError;
      const tdo:POCIType; sqt_code: POCITypeCode): sword;
    function TypeMethodOverload(env: POCIEnv; err: POCIError;
      const tdo: POCIType; const method_name: Poratext;
      const m_length: ub4): ub4;
    function TypeMethodByName(env: POCIEnv; err: POCIError;
      const tdo: POCIType; const method_name: Poratext; const m_length: ub4;
      mdos: PPOCITypeMethod): sword;
    function TypeMethodNext(env: POCIEnv; err: POCIError;
      iterator_ort: POCITypeIter; mdo: PPOCITypeMethod): sword;
    function TypeMethodName(env:POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; n_length: Pub4): poratext;
    function TypeMethodEncap(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod): OCITypeEncap;
    function TypeMethodFlags(env: POCIEnv; err: POCIError;
        const mdo:POCITypeMethod): OCITypeMethodFlag;
    function TypeMethodMap(env: POCIEnv; err: POCIError; const tdo: POCIType;
      mdo: PPOCITypeMethod): sword;
    function TypeMethodOrder(env: POCIEnv; err: POCIError;
      const tdo: POCIType; mdo: PPOCITypeMethod): sword;
    function TypeMethodParams(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod): ub4;
    function TypeResult(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; elem: PPOCITypeElem): sword;
    function TypeParamByPos(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; const position: ub4;
      elem: PPOCITypeElem): sword;
    function TypeParamByName(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; const name: Poratext; const n_length: ub4;
      elem:PPOCITypeElem): sword;
    function TypeParamPos(env: POCIEnv; err: POCIError;
      const mdo: POCITypeMethod; const name: Poratext; const n_length: ub4;
      position: Pub4; elem: PPOCITypeElem): sword;
    function TypeElemParamMode(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem): OCITypeParamMode;
    function TypeElemDefaultValue(env: POCIEnv; err: POCIError;
      const elem: POCITypeElem; d_v_length: Pub4): poratext;
    function TypeVTInit(env: POCIEnv; err: POCIError): sword;
    function TypeVTInsert(env: POCIEnv; err: POCIError;
      const schema_name: Poratext; const s_n_length: ub4;
      const type_name: Poratext; const t_n_length: ub4;
      const user_version:Poratext; const u_v_length:ub4): sword;
    function TypeVTSelect(env: POCIEnv; err: POCIError;
      const schema_name: Poratext; const s_n_length: ub4;
      const type_name: Poratext; const t_n_length: ub4; user_version: PPoratext;
      u_v_length: Pub4; version: Pub2): sword;  *)
  end;

{$ENDIF ZEOS_DISABLE_ORACLE}

implementation

{$IFNDEF ZEOS_DISABLE_ORACLE}

uses ZEncoding;

{ TZOracle9iPlainDriver }

function TZOracle9iPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'AL32UTF8';
end;

procedure TZOracle9iPlainDriver.LoadCodePages;
begin
(*  AddCodePage('AL16UTF16', 2000, ceUTF16, zCP_UTF16); {Unicode 3.1 UTF-16 Universal character set}
  AddCodePage('AL32UTF8', 873, ceUTF8, zCP_UTF8); {Unicode 3.1 UTF-8 Universal character set}
  //AddCodePage('AR8ADOS710', 3); {Arabic MS-DOS 710 Server 8-bit Latin/Arabic}
//  AddCodePage('AR8ADOS710T', 4); {Arabic MS-DOS 710 8-bit Latin/Arabic}
  AddCodePage('AR8ADOS720', 558); {Arabic MS-DOS 720 Server 8-bit Latin/Arabic}
//  AddCodePage('AR8ADOS720T', 6); {Arabic MS-DOS 720 8-bit Latin/Arabic}
//  AddCodePage('AR8APTEC715', 7); {APTEC 715 Server 8-bit Latin/Arabic}
//  AddCodePage('AR8APTEC715T', 8); {APTEC 715 8-bit Latin/Arabic}
//  AddCodePage('AR8ASMO708PLUS', 9); {ASMO 708 Plus 8-bit Latin/Arabic}
  AddCodePage('AR8ASMO8X', 500); {ASMO Extended 708 8-bit Latin/Arabic}
//  AddCodePage('BN8BSCII', 11); {Bangladesh National Code 8-bit BSCII}
//  AddCodePage('TR7DEC', 12); {DEC VT100 7-bit Turkish}
//  AddCodePage('TR8DEC', 13); {DEC 8-bit Turkish}
//  AddCodePage('EL8DEC', 14); {DEC 8-bit Latin/Greek}
//  AddCodePage('EL8GCOS7', 15); {Bull EBCDIC GCOS7 8-bit Greek}
//  AddCodePage('IN8ISCII', 16); {Multiple-Script Indian Standard 8-bit Latin/Indian Languages}
//  AddCodePage('JA16DBCS', 17); {IBM EBCDIC 16-bit Japanese UDC}
//  AddCodePage('JA16EBCDIC930', 18); {IBM DBCS Code Page 290 16-bit Japanese UDC}
  AddCodePage('JA16EUC', 830); {EUC 24-bit Japanese}
  AddCodePage('JA16EUCTILDE', 837); {The same as JA16EUC except for the way that the wave dash and the tilde are mapped to and from Unicode.}
//  AddCodePage('JA16EUCYEN', 21); {EUC 24-bit Japanese with '\' mapped to the Japanese yen character}
//  AddCodePage('JA16MACSJIS', 22); {Mac client Shift-JIS 16-bit Japanese}
  AddCodePage('JA16SJIS', 832); {Shift-JIS 16-bit Japanese UDC}
  AddCodePage('JA16SJISTILDE', 838); {The same as JA16SJIS except for the way that the wave dash and the tilde are mapped to and from Unicode. UDC}
//  AddCodePage('JA16SJISYEN', 25); {Shift-JIS 16-bit Japanese with '\' mapped to the Japanese yen character UDC}
//  AddCodePage('JA16VMS', 26); {JVMS 16-bit Japanese}
//  AddCodePage('RU8BESTA', 27); {BESTA 8-bit Latin/Cyrillic}
//  AddCodePage('SF7ASCII', 28); {ASCII 7-bit Finnish}
//  AddCodePage('KO16DBCS', 29); {IBM EBCDIC 16-bit Korean UDC}
//  AddCodePage('KO16KSCCS', 30); {KSCCS 16-bit Korean}
  AddCodePage('KO16KSC5601', 840); {KSC5601 16-bit Korean}
  AddCodePage('KO16MSWIN949', 846); {MS Windows Code Page 949 Korean UDC}
//  AddCodePage('TH8MACTHAI', 33); {Mac Client 8-bit Latin/Thai}
//  AddCodePage('TH8MACTHAIS', 34); {Mac Server 8-bit Latin/Thai}
  AddCodePage('TH8TISASCII', 41); {Thai Industrial Standard 620-2533 - ASCII 8-bit}
//  AddCodePage('TH8TISEBCDIC', 36); {Thai Industrial Standard 620-2533 - EBCDIC 8-bit}
//  AddCodePage('TH8TISEBCDICS', 37); {Thai Industrial Standard 620-2533-EBCDIC Server 8-bit}
  AddCodePage('US7ASCII', 1); {U.S. 7-bit ASCII American}
  AddCodePage('VN8MSWIN1258', 45); {MS Windows Code Page 1258 8-bit Vietnamese}
//  AddCodePage('VN8VN3', 38); {VN3 8-bit Vietnamese}
//  AddCodePage('WE8GCOS7', 41); {Bull EBCDIC GCOS7 8-bit West European}
//  AddCodePage('YUG7ASCII', 42); {ASCII 7-bit Yugoslavian}
  AddCodePage('ZHS16CGB231280', 850); {CGB2312-80 16-bit Simplified Chinese}
//  AddCodePage('ZHS16DBCS', 44); {IBM EBCDIC 16-bit Simplified Chinese UDC}
  AddCodePage('ZHS16GBK', 852); {GBK 16-bit Simplified Chinese UDC}
//  AddCodePage('ZHS16MACCGB231280', 46); {Mac client CGB2312-80 16-bit Simplified Chinese}
  AddCodePage('ZHS32GB18030', 854); {GB18030-2000}
  AddCodePage('ZHT16BIG5', 856); {BIG5 16-bit Traditional Chinese}
//  AddCodePage('ZHT16CCDC', 49); {HP CCDC 16-bit Traditional Chinese}
//  AddCodePage('ZHT16DBCS', 50); {IBM EBCDIC 16-bit Traditional Chinese UDC}
//  AddCodePage('ZHT16DBT', 51); {Taiwan Taxation 16-bit Traditional Chinese}
  AddCodePage('ZHT16HKSCS', 868); {MS Windows Code Page 950 with Hong Kong Supplementary Character Set}
  AddCodePage('ZHT16MSWIN950', 867); {MS Windows Code Page 950 Traditional Chinese UDC}
  AddCodePage('ZHT32EUC', 860); {EUC 32-bit Traditional Chinese}
//  AddCodePage('ZHT32SOPS', 55); {SOPS 32-bit Traditional Chinese}
//  AddCodePage('ZHT32TRIS', 56); {TRIS 32-bit Traditional Chinese}

//  AddCodePage('WE8DEC', 57); {DEC 8-bit West European}
//  AddCodePage('D7DEC', 58); {DEC VT100 7-bit German}
//  AddCodePage('F7DEC', 59); {DEC VT100 7-bit French}
//  AddCodePage('S7DEC', 60); {DEC VT100 7-bit Swedish}
//  AddCodePage('E7DEC', 61); {DEC VT100 7-bit Spanish}
//  AddCodePage('NDK7DEC', 62); {DEC VT100 7-bit Norwegian/Danish}
//  AddCodePage('I7DEC', 63); {DEC VT100 7-bit Italian}
//  AddCodePage('NL7DEC', 64); {DEC VT100 7-bit Dutch}
//  AddCodePage('CH7DEC', 65); {DEC VT100 7-bit Swiss (German/French)}
//  AddCodePage('SF7DEC', 66); {DEC VT100 7-bit Finnish}
//  AddCodePage('WE8DG', 67); {DG 8-bit West European}
//  AddCodePage('WE8EBCDIC37', 68, ceAnsi, zCP_EBC037); {EBCDIC Code Page 37 8-bit West European}
//  AddCodePage('D8EBCDIC273', 69, ceAnsi, zCP_EBC273); {EBCDIC Code Page 273/1 8-bit Austrian German}
//  AddCodePage('DK8EBCDIC277', 70, ceAnsi, zCP_EBC277); {EBCDIC Code Page 277/1 8-bit Danish}
//  AddCodePage('S8EBCDIC278', 71, ceAnsi, zCP_EBC278); {EBCDIC Code Page 278/1 8-bit Swedish}
//  AddCodePage('I8EBCDIC280', 72, ceAnsi, zCP_EBC280); {EBCDIC Code Page 280/1 8-bit Italian}
//  AddCodePage('WE8EBCDIC284', 73, ceAnsi, zCP_EBC284); {EBCDIC Code Page 284 8-bit Latin American/Spanish}
//  AddCodePage('WE8EBCDIC285', 74); {EBCDIC Code Page 285 8-bit West European}
//  AddCodePage('WE8EBCDIC924', 75); {Latin 9 EBCDIC 924}
//  AddCodePage('WE8EBCDIC1047', 76); {EBCDIC Code Page 1047 8-bit West European}
//  AddCodePage('WE8EBCDIC1047E', 77); {Latin 1/Open Systems 1047}
//  AddCodePage('WE8EBCDIC1140', 78); {EBCDIC Code Page 1140 8-bit West European}
//  AddCodePage('WE8EBCDIC1140C', 79); {EBCDIC Code Page 1140 Client 8-bit West European}
//  AddCodePage('WE8EBCDIC1145', 80); {EBCDIC Code Page 1145 8-bit West European}
//  AddCodePage('WE8EBCDIC1146', 81); {EBCDIC Code Page 1146 8-bit West European}
//  AddCodePage('WE8EBCDIC1148', 82); {EBCDIC Code Page 1148 8-bit West European}
//  AddCodePage('WE8EBCDIC1148C', 83); {EBCDIC Code Page 1148 Client 8-bit West European}
//  AddCodePage('F8EBCDIC297', 84); {EBCDIC Code Page 297 8-bit French}
//  AddCodePage('WE8EBCDIC500', 85); {EBCDIC Code Page 500 8-bit West European}
//  AddCodePage('EE8EBCDIC870', 85); {EBCDIC Code Page 870 8-bit East European}
//  AddCodePage('EE8EBCDIC870C', 87); {EBCDIC Code Page 870 Client 8-bit East European}
//  AddCodePage('EE8EBCDIC870S', 88); {EBCDIC Code Page 870 Server 8-bit East European}
//  AddCodePage('WE8EBCDIC871', 89); {EBCDIC Code Page 871 8-bit Icelandic}
  AddCodePage('EL8EBCDIC875', 90); {EBCDIC Code Page 875 8-bit Greek}
  AddCodePage('EL8EBCDIC875R', 91); {EBCDIC Code Page 875 Server 8-bit Greek}
  AddCodePage('CL8EBCDIC1025', 92); {EBCDIC Code Page 1025 8-bit Cyrillic}
  AddCodePage('CL8EBCDIC1025C', 93); {EBCDIC Code Page 1025 Client 8-bit Cyrillic}
  AddCodePage('CL8EBCDIC1025R', 94); {EBCDIC Code Page 1025 Server 8-bit Cyrillic}
  AddCodePage('CL8EBCDIC1025S', 95); {EBCDIC Code Page 1025 Server 8-bit Cyrillic}
  AddCodePage('CL8EBCDIC1025X', 96); {EBCDIC Code Page 1025 (Modified) 8-bit Cyrillic}
  AddCodePage('BLT8EBCDIC1112', 97); {EBCDIC Code Page 1112 8-bit Baltic Multilingual}
  AddCodePage('BLT8EBCDIC1112S', 98); {EBCDIC Code Page 1112 8-bit Server Baltic Multilingual}
  AddCodePage('D8EBCDIC1141', 99); {EBCDIC Code Page 1141 8-bit Austrian German}
  AddCodePage('DK8EBCDIC1142', 100); {EBCDIC Code Page 1142 8-bit Danish}
  AddCodePage('S8EBCDIC1143', 101); {EBCDIC Code Page 1143 8-bit Swedish}
  AddCodePage('I8EBCDIC1144', 102); {EBCDIC Code Page 1144 8-bit Italian}
  AddCodePage('F8EBCDIC1147', 103); {EBCDIC Code Page 1147 8-bit French}
  AddCodePage('EEC8EUROASCI', 104); {EEC Targon 35 ASCI West European/Greek}
  AddCodePage('EEC8EUROPA3', 105); {EEC EUROPA3 8-bit West European/Greek}
  AddCodePage('LA8PASSPORT', 106); {German Government Printer 8-bit All-European Latin}
  AddCodePage('WE8HP', 107); {HP LaserJet 8-bit West European}
  AddCodePage('WE8ROMAN8', 108); {HP Roman8 8-bit West European}
  AddCodePage('HU8CWI2', 109); {Hungarian 8-bit CWI-2}
  AddCodePage('HU8ABMOD', 110); {Hungarian 8-bit Special AB Mod}
  AddCodePage('LV8RST104090', 111); {IBM-PC Alternative Code Page 8-bit Latvian (Latin/Cyrillic)}
  AddCodePage('US8PC437', 112); {IBM-PC Code Page 437 8-bit American}
  AddCodePage('BG8PC437S', 113); {IBM-PC Code Page 437 8-bit (Bulgarian Modification)}
  AddCodePage('EL8PC437S', 114); {IBM-PC Code Page 437 8-bit (Greek modification)}
  AddCodePage('EL8PC737', 115); {IBM-PC Code Page 737 8-bit Greek/Latin}
  AddCodePage('LT8PC772', 116); {IBM-PC Code Page 772 8-bit Lithuanian (Latin/Cyrillic)}
  AddCodePage('LT8PC774', 117); {IBM-PC Code Page 774 8-bit Lithuanian (Latin)}
  AddCodePage('BLT8PC775', 118); {IBM-PC Code Page 775 8-bit Baltic}
  AddCodePage('WE8PC850', 119); {IBM-PC Code Page 850 8-bit West European}
  AddCodePage('EL8PC851', 120); {IBM-PC Code Page 851 8-bit Greek/Latin}
  AddCodePage('EE8PC852', 121); {IBM-PC Code Page 852 8-bit East European}
  AddCodePage('RU8PC855', 122); {IBM-PC Code Page 855 8-bit Latin/Cyrillic}
  AddCodePage('WE8PC858', 123); {IBM-PC Code Page 858 8-bit West European}
  AddCodePage('WE8PC860', 124); {IBM-PC Code Page 860 8-bit West European}
  AddCodePage('IS8PC861', 125); {IBM-PC Code Page 861 8-bit Icelandic}
  AddCodePage('CDN8PC863', 126); {IBM-PC Code Page 863 8-bit Canadian French}
  AddCodePage('N8PC865', 127); {IBM-PC Code Page 865 8-bit Norwegian}
  AddCodePage('RU8PC866', 128); {IBM-PC Code Page 866 8-bit Latin/Cyrillic}
  AddCodePage('EL8PC869', 129); {IBM-PC Code Page 869 8-bit Greek/Latin}
  AddCodePage('LV8PC1117', 130); {IBM-PC Code Page 1117 8-bit Latvian}
  AddCodePage('US8ICL', 131); {ICL EBCDIC 8-bit American}
  AddCodePage('WE8ICL', 132); {ICL EBCDIC 8-bit West European}
  AddCodePage('WE8ISOICLUK', 133); {ICL special version ISO8859-1}
  AddCodePage('WE8ISO8859P1', 134); {ISO 8859-1 West European}
  AddCodePage('EE8ISO8859P2', 135); {ISO 8859-2 East European}
  AddCodePage('SE8ISO8859P3', 136); {ISO 8859-3 South European}
  AddCodePage('NEE8ISO8859P4', 137); {ISO 8859-4 North and North-East European}
  AddCodePage('CL8ISO8859P5', 138); {ISO 8859-5 Latin/Cyrillic}
  AddCodePage('EL8ISO8859P7', 139); {ISO 8859-7 Latin/Greek}
  AddCodePage('NE8ISO8859P10', 140); {ISO 8859-10 North European}
  AddCodePage('BLT8ISO8859P13', 141); {ISO 8859-13 Baltic}
  AddCodePage('CEL8ISO8859P14', 142); {ISO 8859-13 Celtic}
  AddCodePage('WE8ISO8859P15', 143); {ISO 8859-15 West European}
  AddCodePage('AR8ARABICMAC', 144); {Mac Client 8-bit Latin/Arabic}
  AddCodePage('EE8MACCE', 145); {Mac Client 8-bit Central European}
  AddCodePage('EE8MACCROATIAN', 146); {Mac Client 8-bit Croatian}
  AddCodePage('WE8MACROMAN8', 147); {Mac Client 8-bit Extended Roman8 West European}
  AddCodePage('EL8MACGREEK', 148); {Mac Client 8-bit Greek}
  AddCodePage('IS8MACICELANDIC', 149); {Mac Client 8-bit Icelandic}
  AddCodePage('CL8MACCYRILLIC', 150); {Mac Client 8-bit Latin/Cyrillic}
  AddCodePage('EE8MACCES', 151); {Mac Server 8-bit Central European}
  AddCodePage('EE8MACCROATIANS', 152); {Mac Server 8-bit Croatian}
  AddCodePage('WE8MACROMAN8S', 153); {Mac Server 8-bit Extended Roman8 West European}
  AddCodePage('CL8MACCYRILLICS', 154); {Mac Server 8-bit Latin/Cyrillic}
  AddCodePage('EL8MACGREEKS', 155); {Mac Server 8-bit Greek}
  AddCodePage('IS8MACICELANDICS', 156); {Mac Server 8-bit Icelandic}
  AddCodePage('BG8MSWIN', 157); {MS Windows 8-bit Bulgarian Cyrillic}
  AddCodePage('LT8MSWIN921', 158); {MS Windows Code Page 921 8-bit Lithuanian}
  AddCodePage('ET8MSWIN923', 159); {MS Windows Code Page 923 8-bit Estonian}
  AddCodePage('EE8MSWIN1250', 160, ceAnsi, zCP_WIN1250); {MS Windows Code Page 1250 8-bit East European}
  AddCodePage('CL8MSWIN1251', 161, ceAnsi, zCP_WIN1251); {MS Windows Code Page 1251 8-bit Latin/Cyrillic}
  AddCodePage('WE8MSWIN1252', 162, ceAnsi, zCP_WIN1252); {MS Windows Code Page 1252 8-bit West European}
  AddCodePage('EL8MSWIN1253', 163, ceAnsi, zCP_WIN1253); {MS Windows Code Page 1253 8-bit Latin/Greek}
  AddCodePage('BLT8MSWIN1257', 164, ceAnsi, zCP_WIN1257); {MS Windows Code Page 1257 8-bit Baltic}
  AddCodePage('BLT8CP921', 165); {Latvian Standard LVS8-92(1) Windows/Unix 8-bit Baltic}
  AddCodePage('LV8PC8LR', 166, ceAnsi, zCP_DOS866); {Latvian Version IBM-PC Code Page 866 8-bit Latin/Cyrillic}
  AddCodePage('WE8NCR4970', 167); {NCR 4970 8-bit West European}
  AddCodePage('WE8NEXTSTEP', 168); {NeXTSTEP PostScript 8-bit West European}
  AddCodePage('CL8ISOIR111', 169); {ISOIR111 Cyrillic}
  AddCodePage('CL8KOI8R', 170, ceAnsi, zCP_KOI8R); {RELCOM Internet Standard 8-bit Latin/Cyrillic}
  AddCodePage('CL8KOI8U', 171); {KOI8 Ukrainian Cyrillic}
  AddCodePage('US8BS2000', 172); {Siemens 9750-62 EBCDIC 8-bit American}
  AddCodePage('DK8BS2000', 173); {Siemens 9750-62 EBCDIC 8-bit Danish}
  AddCodePage('F8BS2000', 174); {Siemens 9750-62 EBCDIC 8-bit French}
  AddCodePage('D8BS2000', 175); {Siemens 9750-62 EBCDIC 8-bit German}
  AddCodePage('E8BS2000', 176); {Siemens 9750-62 EBCDIC 8-bit Spanish}
  AddCodePage('S8BS2000', 177); {Siemens 9750-62 EBCDIC 8-bit Swedish}
  AddCodePage('DK7SIEMENS9780X', 178); {Siemens 97801/97808 7-bit Danish}
  AddCodePage('F7SIEMENS9780X', 179); {Siemens 97801/97808 7-bit French}
  AddCodePage('D7SIEMENS9780X', 180); {Siemens 97801/97808 7-bit German}
  AddCodePage('I7SIEMENS9780X', 181); {Siemens 97801/97808 7-bit Italian}
  AddCodePage('N7SIEMENS9780X', 182); {Siemens 97801/97808 7-bit Norwegian}
  AddCodePage('E7SIEMENS9780X', 183); {Siemens 97801/97808 7-bit Spanish}
  AddCodePage('S7SIEMENS9780X', 184); {Siemens 97801/97808 7-bit Swedish}
  AddCodePage('EE8BS2000', 185); {Siemens EBCDIC.DF.04 8-bit East European}
  AddCodePage('WE8BS2000', 186); {Siemens EBCDIC.DF.04 8-bit West European}
  AddCodePage('WE8BS2000E', 187); {Siemens EBCDIC.DF.04 8-bit West European}
  AddCodePage('CL8BS2000', 188); {Siemens EBCDIC.EHC.LC 8-bit Cyrillic}
  AddCodePage('WE8EBCDIC37C', 189); {EBCDIC Code Page 37 8-bit Oracle/c}
  AddCodePage('IW8EBCDIC424', 190); {EBCDIC Code Page 424 8-bit Latin/Hebrew}
  AddCodePage('IW8EBCDIC424S', 191); {EBCDIC Code Page 424 Server 8-bit Latin/Hebrew}
  AddCodePage('WE8EBCDIC500C', 192); {EBCDIC Code Page 500 8-bit Oracle/c}
  AddCodePage('IW8EBCDIC1086', 193); {EBCDIC Code Page 1086 8-bit Hebrew}
  AddCodePage('AR8EBCDIC420S', 194); {EBCDIC Code Page 420 Server 8-bit Latin/Arabic}
  AddCodePage('AR8EBCDICX', 195); {EBCDIC XBASIC Server 8-bit Latin/Arabic}
  AddCodePage('TR8EBCDIC1026', 196, ceAnsi, zCP_IBM1026); {EBCDIC Code Page 1026 8-bit Turkish}
  AddCodePage('TR8EBCDIC1026S', 197); {EBCDIC Code Page 1026 Server 8-bit Turkish}
  AddCodePage('AR8HPARABIC8T', 198); {HP 8-bit Latin/Arabic}
  AddCodePage('TR8PC857', 199); {IBM-PC Code Page 857 8-bit Turkish}
  AddCodePage('IW8PC1507', 200); {IBM-PC Code Page 1507/862 8-bit Latin/Hebrew}
  AddCodePage('AR8ISO8859P6', 201); {ISO 8859-6 Latin/Arabic}
  AddCodePage('IW8ISO8859P8', 201); {ISO 8859-8 Latin/Hebrew}
  AddCodePage('WE8ISO8859P9', 203); {ISO 8859-9 West European & Turkish}
  AddCodePage('LA8ISO6937', 204); {ISO 6937 8-bit Coded Character Set for Text Communication}
  AddCodePage('IW7IS960', 205); {Israeli Standard 960 7-bit Latin/Hebrew}
  AddCodePage('IW8MACHEBREW', 206); {Mac Client 8-bit Hebrew}
  AddCodePage('AR8ARABICMACT', 207); {Mac 8-bit Latin/Arabic}
  AddCodePage('TR8MACTURKISH', 208); {Mac Client 8-bit Turkish}
  AddCodePage('IW8MACHEBREWS', 209); {Mac Server 8-bit Hebrew}
  AddCodePage('TR8MACTURKISHS', 210); {Mac Server 8-bit Turkish}
  AddCodePage('TR8MSWIN1254', 211); {MS Windows Code Page 1254 8-bit Turkish}
  AddCodePage('IW8MSWIN1255', 212); {MS Windows Code Page 1255 8-bit Latin/Hebrew}
  AddCodePage('AR8MSWIN1256', 213); {MS Windows Code Page 1256 8-Bit Latin/Arabic}
  AddCodePage('IN8ISCII', 214); {Multiple-Script Indian Standard 8-bit Latin/Indian Languages}
  AddCodePage('AR8MUSSAD768', 215); {Mussa'd Alarabi/2 768 Server 8-bit Latin/Arabic}
  AddCodePage('AR8MUSSAD768T', 216); {Mussa'd Alarabi/2 768 8-bit Latin/Arabic}
  AddCodePage('AR8NAFITHA711', 217); {Nafitha Enhanced 711 Server 8-bit Latin/Arabic}
  AddCodePage('AR8NAFITHA711T', 218); {Nafitha Enhanced 711 8-bit Latin/Arabic}
  AddCodePage('AR8NAFITHA721', 219); {Nafitha International 721 Server 8-bit Latin/Arabic}
  AddCodePage('AR8NAFITHA721T', 220); {Nafitha International 721 8-bit Latin/Arabic}
  AddCodePage('AR8SAKHR706', 221); {SAKHR 706 Server 8-bit Latin/Arabic}
  AddCodePage('AR8SAKHR707', 222); {SAKHR 707 Server 8-bit Latin/Arabic}
  AddCodePage('AR8SAKHR707T', 223); {SAKHR 707 8-bit Latin/Arabic}
  AddCodePage('AR8XBASIC', 224); {XBASIC 8-bit Latin/Arabic}
  AddCodePage('WE8BS2000L5', 225); {Siemens EBCDIC.DF.04.L5 8-bit West European/Turkish})
  AddCodePage('UTF8', 871, ceUTF8, zCP_UTF8); {Unicode 3.0 UTF-8 Universal character set, CESU-8 compliant}
  AddCodePage('UTFE', 227, ceUTF8, zCP_UTF8); {EBCDIC form of Unicode 3.0 UTF-8 Universal character set}
*)

  //All supporteds from XE
  AddCodePage('US7ASCII', 1, ceAnsi, zCP_us_ascii);
  AddCodePage('US8PC437', 4, ceAnsi, zCP_DOS437);
  AddCodePage('WE8PC850', 10, ceAnsi, zCP_DOS850);
  AddCodePage('WE8PC858', 28, ceAnsi, zCP_DOS858);
  AddCodePage('WE8ISO8859P1', 31, ceAnsi, zCP_L1_ISO_8859_1);
  AddCodePage('EE8ISO8859P2', 32, ceAnsi, zCP_L2_ISO_8859_2);
  AddCodePage('SE8ISO8859P3', 33, ceAnsi, zCP_L3_ISO_8859_3);
  AddCodePage('NEE8ISO8859P4', 34, ceAnsi, zCP_L4_ISO_8859_4);
  AddCodePage('CL8ISO8859P5', 35, ceAnsi, zCP_L5_ISO_8859_5);
  AddCodePage('AR8ISO8859P6', 36, ceAnsi, zCP_L6_ISO_8859_6);
  AddCodePage('EL8ISO8859P7', 37, ceAnsi, zCP_L7_ISO_8859_7);
  AddCodePage('IW8ISO8859P8', 38, ceAnsi, zCP_L8_ISO_8859_8);
  AddCodePage('WE8ISO8859P9', 39, ceAnsi, zCP_L5_ISO_8859_9);
  AddCodePage('NE8ISO8859P10', 40, ceAnsi, zCP_L6_ISO_8859_10);
  AddCodePage('TH8TISASCII', 41, ceAnsi);
  AddCodePage('VN8MSWIN1258', 45, ceAnsi, zCP_WIN1258);
  AddCodePage('WE8ISO8859P15', 46, ceAnsi, zCP_L9_ISO_8859_15);
  AddCodePage('BLT8ISO8859P13', 47, ceAnsi, zCP_L7_ISO_8859_13);
  AddCodePage('CEL8ISO8859P14', 48, ceAnsi, zCP_L8_ISO_8859_14);
  AddCodePage('CL8KOI8U', 51, ceAnsi, zCP_KOI8U);
  AddCodePage('AZ8ISO8859P9E', 52, ceAnsi);
  AddCodePage('EE8PC852', 150, ceAnsi, zCP_DOS852);
  AddCodePage('RU8PC866', 152, ceAnsi, zCP_DOS866);
  AddCodePage('TR8PC857', 156, ceAnsi, zCP_DOS857);
  AddCodePage('EE8MSWIN1250', 170, ceAnsi, zCP_WIN1250);
  AddCodePage('CL8MSWIN1251', 171, ceAnsi, zCP_WIN1251);
  AddCodePage('ET8MSWIN923', 172, ceAnsi, zCP_MSWIN923);
  AddCodePage('EL8MSWIN1253', 174, ceAnsi, zCP_WIN1253);
  AddCodePage('IW8MSWIN1255', 175, ceAnsi, zCP_WIN1255);
  AddCodePage('LT8MSWIN921', 176, ceAnsi, zCP_MSWIN921);
  AddCodePage('TR8MSWIN1254', 177, ceAnsi, zCP_WIN1254);
  AddCodePage('WE8MSWIN1252', 178, ceAnsi, zCP_WIN1252);
  AddCodePage('BLT8MSWIN1257', 179, ceAnsi, zCP_WIN1257);
  AddCodePage('BLT8CP921', 191, ceAnsi, zCP_MSWIN921);
  AddCodePage('CL8KOI8R', 196, ceAnsi, zCP_KOI8R);
  AddCodePage('BLT8PC775', 197, ceAnsi, zCP_DOS775);
  AddCodePage('EL8PC737', 382, ceAnsi, zCP_DOS737);
  AddCodePage('AR8ASMO8X', 500, ceAnsi, zCP_DOS708);
  AddCodePage('AR8ADOS720', 558, ceAnsi, zCP_DOS720);
  AddCodePage('AR8MSWIN1256', 560, ceAnsi, zCP_WIN1256);
  AddCodePage('JA16EUC', 830, ceAnsi, zCP_euc_JP_win);
  AddCodePage('JA16SJIS', 832, ceAnsi, zCP_csISO2022JP);
  AddCodePage('JA16EUCTILDE', 837, ceAnsi);
  AddCodePage('JA16SJISTILDE', 838, ceAnsi);
  AddCodePage('KO16KSC5601', 840, ceAnsi, 601);
  AddCodePage('KO16MSWIN949', 846, ceAnsi, zCP_EUCKR);
  AddCodePage('ZHS16CGB231280', 850, ceAnsi, zCP_GB2312);
  AddCodePage('ZHS16GBK', 852, ceAnsi, zCP_GB2312);
  AddCodePage('ZHS32GB18030', 854, ceAnsi, zCP_GB18030);
  AddCodePage('ZHT32EUC', 860, ceAnsi, zCP_EUCKR);
  AddCodePage('ZHT16BIG5', 865, ceAnsi, zCP_Big5);
  AddCodePage('ZHT16MSWIN950', 867, ceAnsi, zCP_Big5);
  AddCodePage('ZHT16HKSCS', 868, ceAnsi);
  //2018-09-28 UTF8 removed by marsupilami79. UTF8 is CESU-8 in reality which cannot
  //be converted by Zeos correctly. For more information see:
  //https://en.wikipedia.org/wiki/CESU-8
  //https://community.oracle.com/thread/351482
  //UTF8 is aliased to AL32UTF8 to mitigate those rare problems.
  //AddCodePage('UTF8', 871, ceUTF8, zCP_UTF8);
  AddCodePage('UTF8', 871, ceUTF8, zCP_UTF8, 'AL32UTF8', 4);
  AddCodePage('AL32UTF8', 873, ceUTF8, zCP_UTF8, '', 4);
  //AddCodePage('UTF16', 1000, ceUTF16, zCP_UTF16, '', 2);
  //AddCodePage('AL16UTF16', 2000, ceUTF16, zCP_UTF16BE, '', 4);
  //AddCodePage('AL16UTF16LE', 2002, ceUTF16, zCP_UTF16, '', 4);
end;

procedure TZOracle9iPlainDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  with Loader do
  begin
    @OCIInitialize                := GetAddress('OCIInitialize');
    @OCIEnvNlsCreate              := GetAddress('OCIEnvNlsCreate');

    @OCIServerAttach              := GetAddress('OCIServerAttach');
    @OCIServerDetach              := GetAddress('OCIServerDetach');
    @OCIServerRelease             := GetAddress('OCIServerRelease');
    @OCIServerVersion             := GetAddress('OCIServerVersion');

    @OCISessionBegin              := GetAddress('OCISessionBegin');
    @OCISessionEnd                := GetAddress('OCISessionEnd');

    @OCITransStart                := GetAddress('OCITransStart');
    @OCITransCommit               := GetAddress('OCITransCommit');
    @OCITransRollback             := GetAddress('OCITransRollback');

    @OCIPing                      := GetAddress('OCIPing');
    @OCIPasswordChange            := GetAddress('OCIPasswordChange');

    @OCIClientVersion             := GetAddress('OCIClientVersion');

    @OCIHandleAlloc               := GetAddress('OCIHandleAlloc');
    @OCIHandleFree                := GetAddress('OCIHandleFree');
    @OCIErrorGet                  := GetAddress('OCIErrorGet');

    @OCIAttrSet                   := GetAddress('OCIAttrSet');
    @OCIAttrGet                   := GetAddress('OCIAttrGet');
    @OCINlsNumericInfoGet         := GetAddress('OCINlsNumericInfoGet');
    {statement api}
    @OCIStmtPrepare               := GetAddress('OCIStmtPrepare');
    @OCIStmtPrepare2              := GetAddress('OCIStmtPrepare2');
    @OCIStmtRelease               := GetAddress('OCIStmtRelease');
    @OCIStmtExecute               := GetAddress('OCIStmtExecute');
    @OCIParamGet                  := GetAddress('OCIParamGet');
    @OCIStmtFetch                 := GetAddress('OCIStmtFetch');
    @OCIStmtFetch2                := GetAddress('OCIStmtFetch2');

    @OCIDefineByPos               := GetAddress('OCIDefineByPos');
    @OCIBindByPos                 := GetAddress('OCIBindByPos');
    @OCIBindObject                := GetAddress('OCIBindObject');

    @OCIDefineObject              := GetAddress('OCIDefineObject');
    @OCIResultSetToStmt           := GetAddress('OCIResultSetToStmt');
    {descriptors}
    @OCIDescriptorAlloc           := GetAddress('OCIDescriptorAlloc');
    @OCIDescriptorFree            := GetAddress('OCIDescriptorFree');
    {Lob}
    @OCILobOpen                   := GetAddress('OCILobOpen');
    @OCILobRead                   := GetAddress('OCILobRead');
    @OCILobTrim                   := GetAddress('OCILobTrim');
    @OCILobWrite                  := GetAddress('OCILobWrite');

    @OCILobCreateTemporary        := GetAddress('OCILobCreateTemporary');
    @OCILobIsTemporary            := GetAddress('OCILobIsTemporary');
    @OCILobFreeTemporary          := GetAddress('OCILobFreeTemporary');
    @OCILobCharSetForm            := GetAddress('OCILobCharSetForm');
    @OCILobCharSetId              := GetAddress('OCILobCharSetId');
    @OCILobClose                  := GetAddress('OCILobClose');
    {DateTime api}
    @OCIIntervalGetYearMonth      := GetAddress('OCIIntervalGetYearMonth');
    @OCIIntervalGetDaySecond      := GetAddress('OCIIntervalGetDaySecond');
    @OCIDateTimeConstruct         := GetAddress('OCIDateTimeConstruct');
    @OCIDateTimeGetDate           := GetAddress('OCIDateTimeGetDate');
    @OCIDateTimeGetTime           := GetAddress('OCIDateTimeGetTime');
    {object api}
    @OCITypeByRef                 := GetAddress('OCITypeByRef');
    @OCIObjectNew                 := GetAddress('OCIObjectNew');
    @OCIObjectPin                 := GetAddress('OCIObjectPin');
    @OCIObjectUnpin               := GetAddress('OCIObjectUnpin');
    @OCIObjectFree                := GetAddress('OCIObjectFree');
    @OCIObjectGetTypeRef          := GetAddress('OCIObjectGetTypeRef');

    @OCIDescribeAny               := GetAddress('OCIDescribeAny');

    (* unused API
    @OracleAPI.OCIStmtGetPieceInfo := GetAddress('OCIStmtGetPieceInfo');
    @OracleAPI.OCIStmtSetPieceInfo := GetAddress('OCIStmtSetPieceInfo');

    @OracleAPI.OCIBreak           := GetAddress('OCIBreak');

    { For Oracle >= 8.1 }
    @OracleAPI.OCIReset           := GetAddress('OCIReset');

    @OracleAPI.OCITransDetach     := GetAddress('OCITransDetach');
    @OracleAPI.OCITransPrepare    := GetAddress('OCITransPrepare');
    @OracleAPI.OCITransForget     := GetAddress('OCITransForget');


    { > ori.h }
    @OracleAPI.OCIObjectUnmark                := GetAddress('OCIObjectUnmark');
    @OracleAPI.OCIObjectUnmarkByRef           := GetAddress('OCIObjectUnmarkByRef');
    @OracleAPI.OCIObjectMarkDeleteByRef       := GetAddress('OCIObjectMarkDeleteByRef');
    @OracleAPI.OCIObjectMarkDelete            := GetAddress('OCIObjectMarkDelete');
    @OracleAPI.OCIObjectFlush                 := GetAddress('OCIObjectFlush');
    @OracleAPI.OCIObjectRefresh               := GetAddress('OCIObjectRefresh');
    @OracleAPI.OCIObjectCopy                  := GetAddress('OCIObjectCopy');
    @OracleAPI.OCIObjectGetObjectRef          := GetAddress('OCIObjectGetObjectRef');
    @OracleAPI.OCIObjectMakeObjectRef         := GetAddress('OCIObjectMakeObjectRef');
    @OracleAPI.OCIObjectGetPrimaryKeyTypeRef  := GetAddress('OCIObjectGetPrimaryKeyTypeRef');
    @OracleAPI.OCIObjectGetInd                := GetAddress('OCIObjectGetInd');
    @OracleAPI.OCIObjectExists                := GetAddress('OCIObjectExists');
    @OracleAPI.OCIObjectGetProperty           := GetAddress('OCIObjectGetProperty');
    @OracleAPI.OCIObjectIsLocked              := GetAddress('OCIObjectIsLocked');
    @OracleAPI.OCIObjectIsDirty               := GetAddress('OCIObjectIsDirty');
    @OracleAPI.OCIObjectPinTable              := GetAddress('OCIObjectPinTable');
    @OracleAPI.OCIObjectArrayPin              := GetAddress('OCIObjectArrayPin');
    @OracleAPI.OCICacheFlush                  := GetAddress('OCICacheFlush');
    @OracleAPI.OCICacheRefresh                := GetAddress('OCICacheRefresh');
    @OracleAPI.OCICacheUnpin                  := GetAddress('OCICacheUnpin');
    @OracleAPI.OCICacheFree                   := GetAddress('OCICacheFree');
    @OracleAPI.OCICacheUnmark                 := GetAddress('OCICacheUnmark');
    @OracleAPI.OCIDurationBegin               := GetAddress('OCIDurationBegin');
    @OracleAPI.OCIDurationEnd                 := GetAddress('OCIDurationEnd');
    { < ori.h }

    @OracleAPI.OCILobAppend                   := GetAddress('OCILobAppend');
    @OracleAPI.OCILobAssign                   := GetAddress('OCILobAssign');
    @OracleAPI.OCILobCopy                     := GetAddress('OCILobCopy');
    @OracleAPI.OCILobEnableBuffering          := GetAddress('OCILobEnableBuffering');
    @OracleAPI.OCILobDisableBuffering         := GetAddress('OCILobDisableBuffering');
    @OracleAPI.OCILobErase                    := GetAddress('OCILobErase');
    @OracleAPI.OCILobFileExists               := GetAddress('OCILobFileExists');
    @OracleAPI.OCILobFileGetName              := GetAddress('OCILobFileGetName');
    @OracleAPI.OCILobFileSetName              := GetAddress('OCILobFileSetName');
    @OracleAPI.OCILobFlushBuffer              := GetAddress('OCILobFlushBuffer');
    @OracleAPI.OCILobGetLength                := GetAddress('OCILobGetLength');
    @OracleAPI.OCILobLoadFromFile             := GetAddress('OCILobLoadFromFile');
    @OracleAPI.OCILobLocatorIsInit            := GetAddress('OCILobLocatorIsInit');

    { For Oracle >= 8.1 }
    @OracleAPI.OCILobIsOpen                   := GetAddress('OCILobIsOpen');

    @OracleAPI.OCIDateTimeAssign              := GetAddress('OCIDateTimeAssign');
    @OracleAPI.OCIDateTimeCheck               := GetAddress('OCIDateTimeCheck');
    @OracleAPI.OCIDateTimeCompare             := GetAddress('OCIDateTimeCompare');
    @OracleAPI.OCIDateTimeConvert             := GetAddress('OCIDateTimeConvert');
    @OracleAPI.OCIDateTimeFromText            := GetAddress('OCIDateTimeFromText');
    @OracleAPI.OCIDateTimeGetTimeZoneOffset   := GetAddress('OCIDateTimeGetTimeZoneOffset');
    @OracleAPI.OCIDateTimeSysTimeStamp        := GetAddress('OCIDateTimeSysTimeStamp');
    @OracleAPI.OCIDateTimeToText              := GetAddress('OCIDateTimeToText');
    @OracleAPI.OCIDateTimeGetTimeZoneName     := GetAddress('OCIDateTimeGetTimeZoneName');

    { OCI Number mapping }
    @OracleAPI.OCINumberInc       := GetAddress('OCINumberInc');
    @OracleAPI.OCINumberDec       := GetAddress('OCINumberDec');
    @OracleAPI.OCINumberSetZero   := GetAddress('OCINumberSetZero');
    @OracleAPI.OCINumberSetPi     := GetAddress('OCINumberSetPi');
    @OracleAPI.OCINumberAdd       := GetAddress('OCINumberAdd');
    @OracleAPI.OCINumberSub       := GetAddress('OCINumberSub');
    @OracleAPI.OCINumberMul       := GetAddress('OCINumberMul');
    @OracleAPI.OCINumberDiv       := GetAddress('OCINumberDiv');
    @OracleAPI.OCINumberMod       := GetAddress('OCINumberMod');
    @OracleAPI.OCINumberIntPower  := GetAddress('OCINumberIntPower');
    @OracleAPI.OCINumberShift     := GetAddress('OCINumberShift');
    @OracleAPI.OCINumberNeg       := GetAddress('OCINumberNeg');
    @OracleAPI.OCINumberToText    := GetAddress('OCINumberToText');
    @OracleAPI.OCINumberFromText  := GetAddress('OCINumberFromText');
    @OracleAPI.OCINumberToInt     := GetAddress('OCINumberToInt');
    @OracleAPI.OCINumberFromInt   := GetAddress('OCINumberFromInt');
    @OracleAPI.OCINumberToReal    := GetAddress('OCINumberToReal');
    @OracleAPI.OCINumberToRealArray := GetAddress('OCINumberToRealArray');
    @OracleAPI.OCINumberFromReal  := GetAddress('OCINumberFromReal');
    @OracleAPI.OCINumberCmp       := GetAddress('OCINumberCmp');
    @OracleAPI.OCINumberSign      := GetAddress('OCINumberSign');
    @OracleAPI.OCINumberIsZero    := GetAddress('OCINumberIsZero');
    @OracleAPI.OCINumberIsInt     := GetAddress('OCINumberIsInt');
    @OracleAPI.OCINumberAssign    := GetAddress('OCINumberAssign');
    @OracleAPI.OCINumberAbs       := GetAddress('OCINumberAbs');
    @OracleAPI.OCINumberCeil      := GetAddress('OCINumberCeil');
    @OracleAPI.OCINumberFloor     := GetAddress('OCINumberFloor');
    @OracleAPI.OCINumberSqrt      := GetAddress('OCINumberSqrt');
    @OracleAPI.OCINumberTrunc     := GetAddress('OCINumberTrunc');
    @OracleAPI.OCINumberPower     := GetAddress('OCINumberPower');
    @OracleAPI.OCINumberRound     := GetAddress('OCINumberRound');
    @OracleAPI.OCINumberPrec      := GetAddress('OCINumberPrec');
    @OracleAPI.OCINumberSin       := GetAddress('OCINumberSin');
    @OracleAPI.OCINumberArcSin    := GetAddress('OCINumberArcSin');
    @OracleAPI.OCINumberHypSin    := GetAddress('OCINumberHypSin');
    @OracleAPI.OCINumberCos       := GetAddress('OCINumberCos');
    @OracleAPI.OCINumberArcCos    := GetAddress('OCINumberArcCos');
    @OracleAPI.OCINumberHypCos    := GetAddress('OCINumberHypCos');
    @OracleAPI.OCINumberTan       := GetAddress('OCINumberTan');
    @OracleAPI.OCINumberArcTan    := GetAddress('OCINumberArcTan');
    @OracleAPI.OCINumberArcTan2   := GetAddress('OCINumberArcTan2');
    @OracleAPI.OCINumberHypTan    := GetAddress('OCINumberHypTan');
    @OracleAPI.OCINumberExp       := GetAddress('OCINumberExp');
    @OracleAPI.OCINumberLn        := GetAddress('OCINumberLn');
    @OracleAPI.OCINumberLog       := GetAddress('OCINumberLog');

    @OracleAPI.OCITableSize       := GetAddress('OCITableSize');
    @OracleAPI.OCITableExists     := GetAddress('OCITableExists');
    @OracleAPI.OCITableDelete     := GetAddress('OCITableDelete');
    @OracleAPI.OCITableFirst      := GetAddress('OCITableFirst');
    @OracleAPI.OCITableLast       := GetAddress('OCITableLast');
    @OracleAPI.OCITableNext       := GetAddress('OCITableNext');
    @OracleAPI.OCITablePrev       := GetAddress('OCITablePrev');

    @OracleAPI.OCIObjectGetAttr   := GetAddress('OCIObjectGetAttr');
    @OracleAPI.OCIObjectSetAttr   := GetAddress('OCIObjectSetAttr');
    {ort.h}
    @OracleAPI.OCITypeIterNew     := GetAddress('OCITypeIterNew');
    @OracleAPI.OCITypeIterFree    := GetAddress('OCITypeIterFree');
    @OracleAPI.OCITypeByName      := GetAddress('OCITypeByName');
    @OracleAPI.OCITypeArrayByName := GetAddress('OCITypeArrayByName');
    @OracleAPI.OCITypeArrayByRef  := GetAddress('OCITypeArrayByRef');
    @OracleAPI.OCITypeName        := GetAddress('OCITypeName');
    @OracleAPI.OCITypeSchema      := GetAddress('OCITypeSchema');
    @OracleAPI.OCITypeTypeCode    := GetAddress('OCITypeTypeCode');
    @OracleAPI.OCITypeCollTypeCode:= GetAddress('OCITypeCollTypeCode');
    @OracleAPI.OCITypeVersion     := GetAddress('OCITypeVersion');
    @OracleAPI.OCITypeAttrs       := GetAddress('OCITypeAttrs');
    @OracleAPI.OCITypeMethods     := GetAddress('OCITypeMethods');
    @OracleAPI.OCITypeElemName    := GetAddress('OCITypeElemName');
    @OracleAPI.OCITypeElemTypeCode:= GetAddress('OCITypeElemTypeCode');
    @OracleAPI.OCITypeElemType    := GetAddress('OCITypeElemType');
    @OracleAPI.OCITypeElemFlags   := GetAddress('OCITypeElemFlags');
    @OracleAPI.OCITypeElemNumPrec := GetAddress('OCITypeElemNumPrec');
    @OracleAPI.OCITypeElemNumScale:= GetAddress('OCITypeElemNumScale');
    @OracleAPI.OCITypeElemLength  := GetAddress('OCITypeElemLength');
    @OracleAPI.OCITypeElemCharSetID := GetAddress('OCITypeElemCharSetID');
    @OracleAPI.OCITypeElemCharSetForm := GetAddress('OCITypeElemCharSetForm');
    @OracleAPI.OCITypeElemParameterizedType := GetAddress('OCITypeElemParameterizedType');
    @OracleAPI.OCITypeElemExtTypeCode := GetAddress('OCITypeElemExtTypeCode');
    @OracleAPI.OCITypeAttrByName  := GetAddress('OCITypeAttrByName');
    @OracleAPI.OCITypeAttrNext    := GetAddress('OCITypeAttrNext');
    @OracleAPI.OCITypeCollElem    := GetAddress('OCITypeCollElem');
    @OracleAPI.OCITypeCollSize    := GetAddress('OCITypeCollSize');
    @OracleAPI.OCITypeCollExtTypeCode := GetAddress('OCITypeCollExtTypeCode');
    @OracleAPI.OCITypeMethodOverload  := GetAddress('OCITypeMethodOverload');
    @OracleAPI.OCITypeMethodByName:= GetAddress('OCITypeMethodByName');
    @OracleAPI.OCITypeMethodNext  := GetAddress('OCITypeMethodNext');
    @OracleAPI.OCITypeMethodName  := GetAddress('OCITypeMethodName');
    @OracleAPI.OCITypeMethodEncap := GetAddress('OCITypeMethodEncap');
    @OracleAPI.OCITypeMethodFlags := GetAddress('OCITypeMethodFlags');
    @OracleAPI.OCITypeMethodMap   := GetAddress('OCITypeMethodMap');
    @OracleAPI.OCITypeMethodOrder := GetAddress('OCITypeMethodOrder');
    @OracleAPI.OCITypeMethodParams:= GetAddress('OCITypeMethodParams');
    @OracleAPI.OCITypeResult      := GetAddress('OCITypeResult');
    @OracleAPI.OCITypeParamByPos  := GetAddress('OCITypeParamByPos');
    @OracleAPI.OCITypeParamByName := GetAddress('OCITypeParamByName');
    @OracleAPI.OCITypeParamPos    := GetAddress('OCITypeParamPos');
    @OracleAPI.OCITypeElemParamMode := GetAddress('OCITypeElemParamMode');
    @OracleAPI.OCITypeElemDefaultValue := GetAddress('OCITypeElemDefaultValue');
    @OracleAPI.OCITypeVTInit      := GetAddress('OCITypeVTInit');
    @OracleAPI.OCITypeVTInsert    := GetAddress('OCITypeVTInsert');
    @OracleAPI.OCITypeVTSelect    := GetAddress('OCITypeVTSelect');*)
  end;
end;

function TZOracle9iPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZOracle9iPlainDriver.Create;
end;

constructor TZOracle9iPlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
  {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
  {$ENDIF}
  LoadCodePages;
end;

function TZOracle9iPlainDriver.GetProtocol: string;
begin
  Result := 'oracle-9i';
end;

function TZOracle9iPlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Oracle 9i';
end;

{** from ociap.h
  OCIInitialize()
  Name
  OCI Process Initialize
  Purpose
  Initializes the OCI process environment.
  Syntax
  sword OCIInitialize ( ub4           mode,
                      const void   *ctxp,
                      const void   *(*malocfp)
                                    ( void *ctxp,
                                      size_t size ),
                      const void   *(*ralocfp)
                                    ( void *ctxp,
                                      void *memp,
                                      size_t newsize ),
                      const void    (*mfreefp)
                                    ( void *ctxp,
                                      void *memptr ));
  Comments
  This call initializes the OCI process environment.
  OCIInitialize() must be invoked before any other OCI call.
  Parameters
  mode (IN) - specifies initialization of the mode. The valid modes are:
  OCI_DEFAULT - default mode.
  OCI_THREADED - threaded environment. In this mode, internal data
  structures are protected from concurrent accesses by multiple threads.
  OCI_OBJECT - will use navigational object interface.
  ctxp (IN) - user defined context for the memory call back routines.
  malocfp (IN) - user-defined memory allocation function. If mode is
  OCI_THREADED, this memory allocation routine must be thread safe.
  ctxp - context pointer for the user-defined memory allocation function.
  size - size of memory to be allocated by the user-defined memory
  allocation function
  ralocfp (IN) - user-defined memory re-allocation function. If mode is
  OCI_THREADED, this memory allocation routine must be thread safe.
  ctxp - context pointer for the user-defined memory reallocation
  function.
  memp - pointer to memory block
  newsize - new size of memory to be allocated
  mfreefp (IN) - user-defined memory free function. If mode is
  OCI_THREADED, this memory free routine must be thread safe.
  ctxp - context pointer for the user-defined memory free function.
  memptr - pointer to memory to be freed
  Example
  See the description of OCIStmtPrepare() on page 13-96 for an example showing
  the use of OCIInitialize().
  Related Functions
}
procedure TZOracle9iPlainDriver.Initialize(const Location: String);
begin
  inherited Initialize(Location);
  OCIInitialize(OCI_THREADED, nil, nil, nil, nil);
end;

{** from ociap.h
  OCIEnvNlsCreate()
  Name
  OCI ENVironment CREATE with NLS info
  Purpose
  This function does almost everything OCIEnvCreate does, plus enabling setting
  of charset and ncharset programmatically, except OCI_UTF16 mode.
  Syntax
  sword OCIEnvNlsCreate(OCIEnv        **envhpp,
                        ub4           mode,
                        void         *ctxp,
                        void         *(*malocfp)
                                      (void *ctxp,
                                          size_t size),
                        void         *(*ralocfp)
                                      (void *ctxp,
                                         void *memptr,
                                         size_t newsize),
                        void          (*mfreefp)
                                      (void *ctxp,
                                         void *memptr),
                        size_t        xtramemsz,
                        void         **usrmempp,
                        ub2           charset,
                        ub2           ncharset)
  Comments
  The charset and ncharset must be both zero or non-zero.
  The parameters have the same meaning as the ones in OCIEnvCreate().
  When charset or ncharset is non-zero, the corresponding character set will
  be used to replace the ones specified in NLS_LANG or NLS_NCHAR. Moreover,
  OCI_UTF16ID is allowed to be set as charset and ncharset.
  On the other hand, OCI_UTF16 mode is deprecated with this function.
  Applications can achieve the same effects by setting
  both charset and ncharset as OCI_UTF16ID.
}
function TZOracle9iPlainDriver.EnvNlsCreate(var envhpp: POCIEnv; mode: ub4;
  ctxp: Pointer; malocfp: Pointer; ralocfp: Pointer; mfreefp: Pointer;
  xtramemsz: size_T; usrmempp: PPointer; charset, ncharset: ub2): sword;
begin
  Result := OCIEnvNlsCreate(envhpp, mode, ctxp, malocfp, ralocfp,
    mfreefp, xtramemsz, usrmempp, charset, ncharset);
end;

{** from ociap.h
  OCIServerAttach()
  Name
  OCI ATtaCH to server
  Purpose
  Creates an access path to a data source for OCI operations.
  Syntax
  sword OCIServerAttach ( OCIServer    *srvhp,
                        OCIError     *errhp,
                        const OraText   *dblink,
                        sb4          dblink_len,
                        ub4          mode);
  Comments
  This call is used to create an association between an OCI application and a
  particular server.
  This call initializes a server context handle, which must have been previously
  allocated with a call to OCIHandleAlloc().
  The server context handle initialized by this call can be associated with a
  service context through a call to OCIAttrSet(). Once that association has been
  made, OCI operations can be performed against the server.
  If an application is operating against multiple servers, multiple server
  context handles can be maintained. OCI operations are performed against
  whichever server context is currently associated with the service context.
  Parameters
  srvhp (IN/OUT) - an uninitialized server context handle, which gets
  initialized by this call. Passing in an initialized server handle causes an
  error.
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  dblink (IN) - specifies the database (server) to use. This parameter points to
  a character string which specifies a connect string or a service point. If the
  connect string is NULL, then this call attaches to the default host. The length
  of connstr is specified in connstr_len. The connstr pointer may be freed by the
  caller on return.
  dblink_len (IN) - the length of the string pointed to by connstr. For a valid
  connect string name or alias, connstr_len must be non-zero.
  mode (IN) - specifies the various modes of operation.  For release 8.0, pass as
  OCI_DEFAULT - in this mode, calls made to the server on this server context
  are made in blocking mode.
  Example
  See the description of OCIStmtPrepare() on page 13-96 for an example showing
  the use of OCIServerAttach().
  Related Functions
  OCIServerDetach()
}
function TZOracle9iPlainDriver.ServerAttach(srvhp: POCIServer;
  errhp: POCIError; dblink: text; dblink_len: sb4; mode: ub4): sword;
begin
  Result := OCIServerAttach(srvhp, errhp, dblink, dblink_len,
    mode);
end;

{** from ociap.h
  OCIServerDetach()
  Name
  OCI DeTaCH server
  Purpose
  Deletes an access to a data source for OCI operations.
  Syntax
  sword OCIServerDetach ( OCIServer   *svrhp,
                        OCIError    *errhp,
                        ub4         mode);
  Comments
  This call deletes an access to data source for OCI operations, which was
  established by a call to OCIServerAttach().
  Parameters
  srvhp (IN) - a handle to an initialized server context, which gets reset to
  uninitialized state. The handle is not de-allocated.
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  mode (IN) - specifies the various modes of operation. The only valid mode is
  OCI_DEFAULT for the default mode.
  Related Functions
  OCIServerAttach()
}
function TZOracle9iPlainDriver.ServerDetach(srvhp: POCIServer;
  errhp: POCIError; mode: ub4): sword;
begin
  Result := OCIServerDetach(srvhp, errhp, mode);
end;


function TZOracle9iPlainDriver.ServerRelease(hndlp: POCIHandle;
  errhp: POCIError; bufp: text; bufsz: ub4; hndltype: ub1; version:pointer): sword;
begin
  Result := OCI_ERROR;
  if (@OCIServerRelease <> nil) then
    Result := OCIServerRelease(hndlp, errhp, bufp, bufsz,
      hndltype, version);
end;

{** from ociap.h
  OCIServerVersion()
  Name
  OCI VERSion
  Purpose
  Returns the version string of the Oracle server.
  Syntax
  sword OCIServerVersion ( void        *hndlp,
                         OCIError     *errhp,
                         OraText         *bufp,
                         ub4          bufsz
                         ub1          hndltype );
  Comments
  This call returns the version string of the Oracle server.
  For example, the following might be returned as the version string if your
  application is running against a 7.3.2 server:
  Oracle7 Server Release 7.3.2.0.0 - Production Release
  PL/SQL Release 2.3.2.0.0 - Production
  CORE Version 3.5.2.0.0 - Production
  TNS for SEQUENT DYNIX/ptx: Version 2.3.2.0.0 - Production
  NLSRTL Version 3.2.2.0.0 - Production

  Parameters
  hndlp (IN) - the service context handle or the server context handle.
  errhp (IN) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  bufp (IN) - the buffer in which the version information is returned.
  bufsz (IN) - the length of the buffer.
  hndltype (IN) - the type of handle passed to the function.
  Related Functions
}
function TZOracle9iPlainDriver.ServerVersion(hndlp: POCIHandle;
  errhp: POCIError; bufp: text; bufsz: ub4; hndltype: ub1): sword;
begin
  Result := OCIServerVersion(hndlp, errhp, bufp, bufsz,
    hndltype);
end;

{** from ociap.h
  OCISessionBegin()
  Name
  OCI Session Begin and authenticate user
  Purpose
  Creates a user authentication and begins a user session for a given server.
  Syntax
  sword OCISessionBegin ( OCISvcCtx     *svchp,
                        OCIError      *errhp,
                        OCISession    *usrhp,
                        ub4           credt,
                        ub4           mode);

  Comments
  For Oracle8, OCISessionBegin() must be called for any given server handle
  before requests can be made against it. Also, OCISessionBegin() only supports
  authenticating the user for access to the Oracle server specified by the
  server handle in the service context. In other words, after OCIServerAttach()
  is called to initialize a server handle, OCISessionBegin() must be called to
  authenticate the user for that given server.
  When OCISessionBegin() is called for the first time for the given server
  handle, the initialized authentication handle is called a primary
  authentication context. A primary authentication context may not be created
  with the OCI_MIGRATE mode. Also, only one primary authentication context can
  be created for a given server handle and the primary authentication context c
  an only ever be used with that server handle. If the primary authentication
  context is set in a service handle with a different server handle, then an
  error will result.
  After OCISessionBegin() has been called for the server handle, and the primary
  authentication context is set in the service handle, OCISessionBegin() may be
  called again to initialize another authentication handle with different (or
  the same) credentials. When OCISessionBegin() is called with a service handle
  set with a primary authentication context, the returned authentication context
  in authp is called a user authentication context. As many user authentication
  contexts may be initialized as desired.
  User authentication contexts may be created with the OCI_MIGRATE mode.
  If the OCI_MIGRATE mode is not specified, then the user authentication
  context can only ever be used with the same server handle set in svchp. If
  OCI_MIGRATE mode is specified, then the user authentication may be set
  with different server handles. However, the user authentication context is
  restricted to use with only server handles which resolve to the same database
  instance and that have equivalent primary authentication contexts. Equivalent
  authentication contexts are those which were authenticated as the same
  database user.
  OCI_SYSDBA, OCI_SYSOPER, OCI_SYSASM, and OCI_PRELIM_AUTH may only be used
  with a primary authentication context.
  To provide credentials for a call to OCISessionBegin(), one of two methods are
  supported. The first is to provide a valid username and password pair for
  database authentication in the user authentication handle passed to
  OCISessionBegin(). This involves using OCIAttrSet() to set the
  OCI_ATTR_USERNAME and OCI_ATTR_PASSWORD attributes on the
  authentication handle. Then OCISessionBegin() is called with
  OCI_CRED_RDBMS.
  Note: When the authentication handle is terminated using
  OCISessionEnd(), the username and password attributes remain
  unchanged and thus can be re-used in a future call to OCISessionBegin().
  Otherwise, they must be reset to new values before the next
  OCISessionBegin() call.
  The second type of credentials supported are external credentials. No
  attributes need to be set on the authentication handle before calling
  OCISessionBegin(). The credential type is OCI_CRED_EXT. This is equivalent
  to the Oracle7 `connect /' syntax. If values have been set for
  OCI_ATTR_USERNAME and OCI_ATTR_PASSWORD, then these are
  ignored if OCI_CRED_EXT is used.
  Parameters
  svchp (IN) - a handle to a service context. There must be a valid server
  handle set in svchp.
  errhp (IN) - an error handle to the retrieve diagnostic information.
  usrhp (IN/OUT) - a handle to an authentication context, which is initialized
  by this call.
  credt (IN) - specifies the type of credentials to use for authentication.
  Valid values for credt are:
  OCI_CRED_RDBMS - authenticate using a database username and
  password pair as credentials. The attributes OCI_ATTR_USERNAME
  and OCI_ATTR_PASSWORD should be set on the authentication
  context before this call.
  OCI_CRED_EXT - authenticate using external credentials. No username
  or password is provided.
  mode (IN) - specifies the various modes of operation. Valid modes are:
  OCI_DEFAULT - in this mode, the authentication context returned may
  only ever be set with the same server context specified in svchp. This
  establishes the primary authentication context.
  OCI_MIGRATE - in this mode, the new authentication context may be
  set in a service handle with a different server handle. This mode
  establishes the user authentication context.
  OCI_SYSDBA - in this mode, the user is authenticated for SYSDBA
  access.
  OCI_SYSOPER - in this mode, the user is authenticated for SYSOPER
  access.
  OCI_SYSASM - in this mode, the user is authenticated for SYSASM
  access.  Note that only an ASM instance can grant SYSASM access.
  OCI_PRELIM_AUTH - this mode may only be used with OCI_SYSDBA, OCI_SYSASM,
  or OCI_SYSOPER to authenticate for certain administration tasks.
  Related Functions
  OCISessionEnd()
}
function TZOracle9iPlainDriver.SessionBegin(svchp: POCISvcCtx;
  errhp: POCIError; usrhp: POCISession; credt, mode: ub4): sword;
begin
  Result := OCISessionBegin(svchp, errhp, usrhp, credt, mode);
end;

{** from ociap.h
  OCISessionEnd()
  Name
  OCI Terminate user Authentication Context
  Purpose
  Terminates a user authentication context created by OCISessionBegin()
  Syntax
  sword OCISessionEnd ( OCISvcCtx       *svchp,
                      OCIError        *errhp,
                      OCISession      *usrhp,
                      ub4             mode);

  Comments
  The user security context associated with the service context is invalidated
  by this call. Storage for the authentication context is not freed. The
  transaction specified by the service context is implicitly committed. The
  transaction handle, if explicitly allocated, may be freed if not being used.
  Resources allocated on the server for this user are freed.
  The authentication handle may be reused in a new call to OCISessionBegin().
  Parameters
  svchp (IN/OUT) - the service context handle. There must be a valid server
  handle and user authentication handle associated with svchp.
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  usrhp (IN) - de-authenticate this user. If this parameter is passed as NULL,
  the user in the service context handle is de-authenticated.
  mode (IN) - the only valid mode is OCI_DEFAULT.
  Example
  In this example, an authentication context is destroyed.
  Related Functions
  OCISessionBegin()
}
function TZOracle9iPlainDriver.SessionEnd(svchp: POCISvcCtx;
  errhp: POCIError; usrhp: POCISession; mode: ub4): sword;
begin
  Result := OCISessionEnd(svchp, errhp, usrhp, mode);
end;

{** from ociap.h
  OCITransStart()
  Name
  OCI TX (transaction) STart
  Purpose
  Sets the beginning of a transaction.
  Syntax
  sword OCITransStart ( OCISvcCtx    *svchp,
                      OCIError     *errhp,
                      uword        timeout,
                      ub4          flags);

  Comments
  This function sets the beginning of a global or serializable transaction. The
  transaction context currently associated with the service context handle is
  initialized at the end of the call if the flags parameter specifies that a new
  transaction should be started.
  The XID of the transaction is set as an attribute of the transaction handle
  (OCI_ATTR_XID)
  Parameters
  svchp (IN/OUT) - the service context handle. The transaction context in the
  service context handle is initialized at the end of the call if the flag
  specified a new transaction to be started.
  errhp (IN/OUT) - The OCI error handle. If there is an error, it is recorded in
  err and this function returns OCI_ERROR. Diagnostic information can be
  obtained by calling OCIErrorGet().
  timeout (IN) - the time, in seconds, to wait for a transaction to become
  available for resumption when OCI_TRANS_RESUME is specified. When
  OCI_TRANS_NEW is specified, this value is stored and may be used later by
  OCITransDetach().
  flags (IN) - specifies whether a new transaction is being started or an
  existing transaction is being resumed. Also specifies serializiability or
  read-only status. More than a single value can be specified. By default,
  a read/write transaction is started. The flag values are:
  OCI_TRANS_NEW - starts a new transaction branch. By default starts a
  tightly coupled and migratable branch.
  OCI_TRANS_TIGHT - explicitly specifies a tightly coupled branch
  OCI_TRANS_LOOSE - specifies a loosely coupled branch
  OCI_TRANS_RESUME - resumes an existing transaction branch.
  OCI_TRANS_READONLY - start a readonly transaction
  OCI_TRANS_SERIALIZABLE - start a serializable transaction
  Related Functions
  OCITransDetach()
}
function TZOracle9iPlainDriver.TransStart(svchp: POCISvcCtx;
  errhp: POCIError; timeout: word; flags: ub4): sword;
begin
  Result := OCITransStart(svchp, errhp, timeout, flags);
end;

function TZOracle9iPlainDriver.TransRollback(svchp: POCISvcCtx;
  errhp: POCIError; flags: ub4): sword;
begin
  Result := OCITransRollback(svchp, errhp, flags);
end;

{** from ociap.h
OCITransCommit()
Name
OCI TX (transaction) CoMmit
Purpose
Commits the transaction associated with a specified service context.
Syntax
sword OCITransCommit ( OCISvcCtx    *srvcp,
                     OCIError     *errhp,
                     ub4          flags );
Comments
The transaction currently associated with the service context is committed. If
it is a distributed transaction that the server cannot commit, this call
additionally retrieves the state of the transaction from the database to be
returned to the user in the error handle.
If the application has defined multiple transactions, this function operates
on the transaction currently associated with the service context. If the
application is working with only the implicit local transaction created when
database changes are made, that implicit transaction is committed.
If the application is running in the object mode, then the modified or updated
objects in the object cache for this transaction are also committed.
The flags parameter is used for one-phase commit optimization in distributed
transactions. If the transaction is non-distributed, the flags parameter is
ignored, and OCI_DEFAULT can be passed as its value. OCI applications
managing global transactions should pass a value of
OCI_TRANS_TWOPHASE to the flags parameter for a two-phase commit. The
default is one-phase commit.
Under normal circumstances, OCITransCommit() returns with a status
indicating that the transaction has either been committed or rolled back. With
distributed transactions, it is possible that the transaction is now in-doubt
(i.e., neither committed nor aborted). In this case, OCITransCommit()
attempts to retrieve the status of the transaction from the server.
The status is returned.
Parameters
srvcp (IN) - the service context handle.
errhp (IN) - an error handle which can be passed to OCIErrorGet() for
diagnostic information in the event of an error.
flags -see the "Comments" section above.
Related Functions
OCITransRollback() *}
function TZOracle9iPlainDriver.TransCommit(svchp: POCISvcCtx;
  errhp: POCIError; flags: ub4): sword;
begin
  Result := OCITransCommit(svchp, errhp, flags);
end;

function TZOracle9iPlainDriver.Ping(svchp: POCISvcCtx; errhp: POCIError;
  mode: ub4 = OCI_DEFAULT): sword;
begin
  Result := OCIPing(svchp, errhp, mode);
end;

{** from ociap.h
  OCIPasswordChange()
  Name
  OCI Change PassWord
  Purpose
  This call allows the password of an account to be changed.
  Syntax
  sword OCIPasswordChange ( OCISvcCtx     *svchp,
                          OCIError      *errhp,
                          const OraText    *user_name,
                          ub4           usernm_len,
                          const OraText    *opasswd,
                          ub4           opasswd_len,
                          const OraText    *npasswd,
                          sb4           npasswd_len,
                          ub4           mode);
  Comments
  This call allows the password of an account to be changed. This call is
  similar to OCISessionBegin() with the following differences:
  If the user authentication is already established, it authenticates
  the account using the old password and then changes the
  password to the new password
  If the user authentication is not established, it establishes a user
  authentication and authenticates the account using the old
  password, then changes the password to the new password.
  This call is useful when the password of an account is expired and
  OCISessionBegin() returns an error or warning which indicates that the
  password has expired.
  Parameters
  svchp (IN/OUT) - a handle to a service context. The service context handle
  must be initialized and have a server context handle associated with it.
  errhp (IN) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  user_name (IN) - specifies the user name. It points to a character string,
  whose length is specified in usernm_len. This parameter must be NULL if the
  service context has been initialized with an authentication handle.
  usernm_len (IN) - the length of the user name string specified in user_name.
  For a valid user name string, usernm_len must be non-zero.
  opasswd (IN) - specifies the user's old password. It points to a character
  string, whose length is specified in opasswd_len .
  opasswd_len (IN) - the length of the old password string specified in opasswd.
  For a valid password string, opasswd_len must be non-zero.
  npasswd (IN) - specifies the user's new password. It points to a character
  string, whose length is specified in npasswd_len which must be non-zero for a
  valid password string. If the password complexity verification routine is
  specified in the user's profile to verify the new password's complexity, the
  new password must meet the complexity requirements of the verification
  function.
  npasswd_len (IN)  - then length of the new password string specified in
  npasswd. For a valid password string, npasswd_len must be non-zero.
  mode - pass as OCI_DEFAULT.
  Related Functions
  OCISessionBegin()
}
function TZOracle9iPlainDriver.PasswordChange(svchp: POCISvcCtx;
  errhp: POCIError; user_name: text; usernm_len: ub4; opasswd: text;
  opasswd_len: ub4; npasswd: text; npasswd_len: sb4; mode: ub4): sword;
begin
  Result := OCIPasswordChange(svchp, errhp, user_name,
    usernm_len, opasswd, opasswd_len, npasswd, npasswd_len, mode);
end;

procedure TZOracle9iPlainDriver.ClientVersion(major_version, minor_version,
  update_num, patch_num, port_update_num: psword);
begin
  OCIClientVersion(major_version, minor_version,
    update_num, patch_num, port_update_num);
end;

(** from ociap.h
OCIHandleAlloc()
Name
OCI Get HaNDLe
Purpose
This call returns a pointer to an allocated and initialized handle.
Syntax
sword OCIHandleAlloc ( const void   *parenth,
                     void         **hndlpp,
                     ub4           type,
                     size_t        xtramem_sz,
                     void         **usrmempp);
Comments
Returns a pointer to an allocated and initialized structure, corresponding to
the type specified in type. A non-NULL handle is returned on success. Bind
handle and define handles are allocated with respect to a statement handle. All
other handles are allocated with respect to an environment handle which is
passed in as a parent handle.
No diagnostics are available on error. This call returns OCI_SUCCESS if
successful, or OCI_INVALID_HANDLE if an out-of-memory error occurs.
Handles must be allocated using OCIHandleAlloc() before they can be passed
into an OCI call.
Parameters
parenth (IN) - an environment or a statement handle.
hndlpp (OUT) - returns a handle to a handle type.
type (IN) - specifies the type of handle to be allocated. The specific types
are:
OCI_HTYPE_ERROR - specifies generation of an error report handle of
C type OCIError
OCI_HTYPE_SVCCTX - specifies generation of a service context handle
of C type OCISvcCtx
OCI_HTYPE_STMT - specifies generation of a statement (application
request) handle of C type OCIStmt
OCI_HTYPE_BIND - specifies generation of a bind information handle
of C type OCIBind
OCI_HTYPE_DEFINE - specifies generation of a column definition
handle of C type OCIDefine
OCI_HTYPE_DESCRIBE  - specifies generation of a select list
description handle of C type OCIDesc
OCI_HTYPE_SERVER - specifies generation of a server context handle
of C type OCIServer
OCI_HTYPE_SESSION - specifies generation of an authentication
context handle of C type OCISession
OCI_HTYPE_TRANS - specifies generation of a transaction context
handle of C type OCITrans
OCI_HTYPE_COMPLEXOBJECT - specifies generation of a complex
object retrieval handle of C type OCIComplexObject
OCI_HTYPE_SECURITY - specifies generation of a security handle of C
type OCISecurity
xtramem_sz (IN) - specifies an amount of user memory to be allocated.
usrmempp (OUT) - returns a pointer to the user memory of size xtramemsz
allocated by the call for the user.
Related Functions
OCIHandleFree() *)
function TZOracle9iPlainDriver.HandleAlloc(parenth: POCIHandle;
  var hndlpp: POCIHandle; atype: ub4; xtramem_sz: size_T;
  usrmempp: PPointer): sword;
begin
  Result := OCIHandleAlloc(parenth, hndlpp, atype, xtramem_sz,
    usrmempp);
end;

(** from ociap.h
OCIHandleFree()
Name
OCI Free HaNDLe
Purpose
This call explicitly deallocates a handle.
Syntax
sword OCIHandleFree ( void     *hndlp,
                    ub4       type);
Comments
This call frees up storage associated with a handle, corresponding to the type
specified in the type parameter.
This call returns either OCI_SUCCESS or OCI_INVALID_HANDLE.
All handles must be explicitly deallocated. OCI will not deallocate a child
handle if the parent is deallocated.
Parameters
hndlp (IN) - an opaque pointer to some storage.
type (IN) - specifies the type of storage to be allocated. The specific types
are:
OCI_HTYPE_ENV - an environment handle
OCI_HTYPE_ERROR - an error report handle
OCI_HTYPE_SVCCTX - a service context handle
OCI_HTYPE_STMT - a statement (application request) handle
OCI_HTYPE_BIND - a bind information handle
OCI_HTYPE_DEFINE - a column definition handle
OCI_HTYPE_DESCRIBE  - a select list description handle
OCI_HTYPE_SERVER - a server handle
OCI_HTYPE_SESSION - a user authentication handle
OCI_HTYPE_TRANS - a transaction handle
OCI_HTYPE_COMPLEXOBJECT - a complex object retrieval handle
OCI_HTYPE_SECURITY - a security handle
Related Functions
OCIHandleAlloc()
*)
function TZOracle9iPlainDriver.HandleFree(hndlp: Pointer; atype: ub4): sword;
begin
  Result := OCIHandleFree(hndlp, atype);
end;

{** from ociap.h
  OCIErrorGet()
  Name
  OCI Get Diagnostic Record
  Purpose
  Returns an error message in the buffer provided and an ORACLE error.
  Syntax
  sword OCIErrorGet ( void      *hndlp,
                    ub4        recordno,
                    OraText       *sqlstate,
                    ub4        *errcodep,
                    OraText       *bufp,
                    ub4        bufsiz,
                    ub4        type );
  Comments
  Returns an error message in the buffer provided and an ORACLE error.
  Currently does not support SQL state. This call can be called a multiple
  number of times if there are more than one diagnostic record for an error.
  The error handle is originally allocated with a call to OCIHandleAlloc().
  Parameters
  hndlp (IN) - the error handle, in most cases, or the environment handle (for
  errors on OCIEnvInit(), OCIHandleAlloc()).
  recordno (IN) - indicates the status record from which the application seeks
  info. Starts from 1.
  sqlstate (OUT) - Not supported in Version 8.0.
  errcodep (OUT) - an ORACLE Error is returned.
  bufp (OUT) - the error message text is returned.
  bufsiz (IN) - the size of the buffer provide to get the error message.
  type (IN) - the type of the handle.
  Related Functions
  OCIHandleAlloc()
}
function TZOracle9iPlainDriver.ErrorGet(hndlp: Pointer; recordno: ub4;
  sqlstate: text; var errcodep: sb4; bufp: text; bufsiz,
  atype: ub4): sword;
begin
  Result := OCIErrorGet(hndlp, recordno, sqlstate, errcodep,
    bufp, bufsiz, atype);
end;

{** from ociap.h
  OCIAttrSet()
  Name
  OCI Attribute Set
  Purpose
  This call is used to set a particular attribute of a handle or a descriptor.
  Syntax
  sword OCIAttrSet ( void       *trgthndlp,
                   ub4         trghndltyp,
                   void       *attributep,
                   ub4         size,
                   ub4         attrtype,
                   OCIError    *errhp );
  Comments
  This call is used to set a particular attribute of a handle or a descriptor.
  See Appendix B for a list of handle types and their writeable attributes.
  Parameters
  trghndlp (IN/OUT) - the pointer to a handle type whose attribute gets
  modified.
  trghndltyp (IN/OUT) - is the handle type.
  attributep (IN) - a pointer to an attribute value.
  The attribute value is copied into the target handle. If the attribute value
  is a pointer, then only the pointer is copied, not the contents of the pointer.
  size (IN) - is the size of an attribute value. This can be passed in as 0 for
  most attributes as the size is already known by the OCI library. For text*
  attributes, a ub4 must be passed in set to the length of the string.
  attrtype (IN) - the type of attribute being set.
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  Related Functions
  OCIAttrGet()
}
function TZOracle9iPlainDriver.AttrSet(trgthndlp: POCIHandle;
  trghndltyp: ub4; attributep: Pointer; size, attrtype: ub4;
  errhp: POCIError): sword;
begin
  Result := OCIAttrSet(trgthndlp, trghndltyp, attributep, size,
    attrtype, errhp);
end;

{** from ociap.h
  OCIAttrGet()
  Name
  OCI Attribute Get
  Purpose
  This call is used to get a particular attribute of a handle.
  Syntax
  sword OCIAttrGet ( const void    *trgthndlp,
                   ub4            trghndltyp,
                   void          *attributep,
                   ub4            *sizep,
                   ub4            attrtype,
                   OCIError       *errhp );
  Comments
  This call is used to get a particular attribute of a handle.
  See Appendix B,  "Handle Attributes",  for a list of handle types and their
  readable attributes.
  Parameters
  trgthndlp (IN) - is the pointer to a handle type.
  trghndltyp (IN) - is the handle type.
  attributep (OUT) - is a pointer to the storage for an attribute value. The
  attribute value is filled in.
  sizep (OUT) - is the size of the attribute value.
  This can be passed in as NULL for most parameters as the size is well known.
  For text* parameters, a pointer to a ub4 must be passed in to get the length
  of the string.
  attrtype (IN) - is the type of attribute.
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  Related Functions
  OCIAttrSet()
}
function TZOracle9iPlainDriver.AttrGet(trgthndlp: POCIHandle;
  trghndltyp: ub4; attributep, sizep: Pointer; attrtype: ub4;
  errhp: POCIError): sword;
begin
  Result := OCIAttrGet(trgthndlp, trghndltyp, attributep, sizep,
    attrtype, errhp);
end;

{** from ociap.h
   NAME
     OCINlsNumericInfoGet - Get NLS numeric info from OCI environment handle
   REMARKS
     This function generates numeric language information specified by item
     from OCI environment handle envhp into an output number variable.
   RETURNS
     OCI_SUCCESS, OCI_INVALID_HANDLE, or OCI_ERROR on wrong item.
   envhp(IN/OUT)
     OCI environment handle. If handle invalid, returns OCI_INVALID_HANDLE.
   errhp(IN/OUT)
     The OCI error handle. If there is an error, it is record in errhp and
     this function returns a NULL pointer. Diagnostic information can be
     obtained by calling OCIErrorGet().
   val(OUT)
     Pointer to the output number variable. On OCI_SUCCESS return, it will
     contain the requested NLS numeric info.
   item(IN)
     It specifies to get which item in OCI environment handle and can be one
     of following values:
       OCI_NLS_CHARSET_MAXBYTESZ : Maximum character byte size for OCI
                                   environment or session handle charset
       OCI_NLS_CHARSET_FIXEDWIDTH: Character byte size for fixed-width charset;
                                   0 for variable-width charset
}
function TZOracle9iPlainDriver.NlsNumericInfoGet(envhp: POCIEnv;
  errhp: POCIError; val: psb4; item: ub2): sword;
begin
  Result := OCINlsNumericInfoGet(envhp, errhp, val, item);
end;

{** from ociap.h
  OCIStmtPrepare()
  Name
  OCI Statement REQuest
  Purpose
  This call defines the SQL/PLSQL statement to be executed.
  Syntax
  sword OCIStmtPrepare ( OCIStmt      *stmtp,
                       OCIError     *errhp,
                       const OraText   *stmt,
                       ub4          stmt_len,
                       ub4          language,
                       ub4          mode);
  Comments
  This call is used to prepare a SQL or PL/SQL statement for execution. The
  OCIStmtPrepare() call defines an application request.
  This is a purely local call. Data values for this statement initialized in
  subsequent bind calls will be stored in a bind handle which will hang off this
  statement handle.
  This call does not create an association between this statement handle and any
  particular server.
  See the section "Preparing Statements" on page 2-21 for more information
  about using this call.
  Parameters
  stmtp (IN) - a statement handle.
  errhp (IN) - an error handle to retrieve diagnostic information.
  stmt (IN) - SQL or PL/SQL statement to be executed. Must be a null-terminated
  string. The pointer to the OraText of the statement must be available as long
  as the statement is executed.
  stmt_len (IN) - length of the statement. Must not be zero.
  language (IN) - V7, V8, or native syntax. Possible values are:
  OCI_V7_SYNTAX - V7 ORACLE parsing syntax
  OCI_V8_SYNTAX - V8 ORACLE parsing syntax
  OCI_NTV_SYNTAX - syntax depending upon the version of the server.
  mode (IN) - the only defined mode is OCI_DEFAULT for default mode.
  Example
  This example demonstrates the use of OCIStmtPrepare(), as well as the OCI
  application initialization calls.
  Related Functions
  OCIAttrGet(), OCIStmtExecute()
}
function TZOracle9iPlainDriver.StmtPrepare(stmtp: POCIStmt;
  errhp: POCIError; stmt: text; stmt_len, language, mode: ub4): sword;
begin
  Result := OCIStmtPrepare(stmtp, errhp, stmt, stmt_len,
    language, mode);
end;

{** from ociap.h
  OCIStmtPrepare2()
  Name
  OCI Statement REQuest with (a) early binding to svchp and/or
  (b) stmt caching
  Purpose
  This call defines the SQL/PLSQL statement to be executed.
  Syntax
  sword OCIStmtPrepare2 ( OCISvcCtx *svchp,
                       OCIStmt      **stmtp,
                       OCIError     *errhp,
                       const OraText   *stmt,
                       ub4          stmt_len,
                       const OraText *key,
                       ub4          key_len,
                       ub4          language,
                       ub4          mode);
  Comments
  This call is used to prepare a SQL or PL/SQL statement for execution. The
  OCIStmtPrepare() call defines an application request.
  This is a purely local call. Data values for this statement initialized in
  subsequent bind calls will be stored in a bind handle which will hang off this
  statement handle.
  This call creates an association between the statement handle and a service
  context. It differs from OCIStmtPrepare in that respect.It also supports
  stmt caching. The stmt will automatically be cached if the authp of the stmt
  has enabled stmt caching.
  Parameters
  svchp (IN) - the service context handle that contains the session that
               this stmt handle belongs to.
  stmtp (OUT) - an unallocated stmt handle must be pased in. An allocated
                and prepared  statement handle will be returned.
  errhp (IN) - an error handle to retrieve diagnostic information.
  stmt (IN) - SQL or PL/SQL statement to be executed. Must be a null-
              terminated string. The pointer to the OraText of the statement
              must be available as long as the statement is executed.
  stmt_len (IN) - length of the statement. Must not be zero.
  key (IN) - This is only Valid for OCI Stmt Caching. It indicates the
             key to search with. It thus optimizes the search in the cache.
  key_len (IN) - the length of the key. This, too, is onlly valid for stmt
                 caching.
  language (IN) - V7, V8, or native syntax. Possible values are:
  OCI_V7_SYNTAX - V7 ORACLE parsing syntax
  OCI_V8_SYNTAX - V8 ORACLE parsing syntax
  OCI_NTV_SYNTAX - syntax depending upon the version of the server.
  mode (IN) - the defined modes are OCI_DEFAULT and OCI_PREP2_CACHE_SEARCHONLY.
  Example
  Related Functions
  OCIStmtExecute(), OCIStmtRelease()
}
function TZOracle9iPlainDriver.StmtPrepare2(svchp: POCISvcCtx;
  var stmtp: POCIStmt; errhp: POCIError; stmt: text; stmt_len: ub4; key: text;
  key_len: ub4; language:ub4; mode: ub4): sword;
begin
  Result := OCIStmtPrepare2(svchp, stmtp, errhp, stmt, stmt_len, key,
    key_len, language, mode);
end;

{** from ociap.h
  OCIStmtRelease()
  Name
  OCI Statement Release. This call is used to relesae the stmt that
  was retreived using OCIStmtPrepare2(). If the stmt is release
  using this call, OCIHandleFree() must not be called on the stmt
  handle.
  Purpose
  This call releases the statement obtained by OCIStmtPrepare2
  Syntax
  sword OCIStmtRelease ( OCIStmt      *stmtp,
                       OCIError     *errhp,
                       cONST OraText *key,
                       ub4          key_len,
                       ub4          mode);
  Comments
  This call is used to release a handle obtained via OCIStmtPrepare2().
  It also frees the memory associated with the handle.
  This is a purely local call.
  Parameters
  stmtp (IN/OUT) - The statement handle to be released/freed.
  errhp (IN) - an error handle to retrieve diagnostic information.
  key (IN) - This is only Valid for OCI Stmt Caching. It indicates the
             key to tag the stmt with.
  key_len (IN) - the length of the key. This, too, is only valid for stmt
                 caching.
  mode (IN) - the defined modes are OCI_DEFAULT for default mode and
              OCI_STRLS_CACHE_DELETE (only used for Stmt Caching).
  Example
  Related Functions
  OCIStmtExecute(), OCIStmtPrepare2()
}
function TZOracle9iPlainDriver.StmtRelease(stmtp: POCIStmt; errhp: POCIError;
  key: text; key_len: ub4; mode: ub4): sword;
begin
  Result := OCIStmtRelease(stmtp, errhp, key, key_len, mode);
end;

{** from ociap.h
  OCIStmtExecute()
  Name
  OCI EXECute
  Purpose
  This call associates an application request with a server.
  Syntax
  sword OCIStmtExecute ( OCISvcCtx           *svchp,
                       OCIStmt             *stmtp,
                       OCIError            *errhp,
                       ub4                 iters,
                       ub4                 rowoff,
                       const OCISnapshot   *snap_in,
                       OCISnapshot         *snap_out,
                       ub4                 mode );
  Comments
  This function  is used to execute a prepared SQL statement.
  Using an execute call, the application associates a request with a server. On
  success, OCI_SUCCESS is returned.
  If a SELECT statement is executed, the description of the select list follows
  implicitly as a response. This description is buffered on the client side for
  describes, fetches and define type conversions. Hence it is optimal to
  describe a select list only after an execute.
  Also for SELECT statements, some results are available implicitly. Rows will
  be received and buffered at the end of the execute. For queries with small row
  count, a prefetch causes memory to be released in the server if the end of
  fetch is reached, an optimization that may result in memory usage reduction.
  Set attribute call has been defined to set the number of rows to be prefetched
  per result set.
  For SELECT statements, at the end of the execute, the statement handle
  implicitly maintains a reference to the service context on which it is
  executed. It is the user's responsibility to maintain the integrity of the
  service context. If the attributes of a service context is changed for
  executing some operations on this service context, the service context must
  be restored to have the same attributes, that a statement was executed with,
  prior to a fetch on the statement handle. The implicit reference is maintained
  until the statement handle is freed or the fetch is cancelled or an end of
  fetch condition is reached.
  Note: If output variables are defined for a SELECT statement before a
  call to OCIStmtExecute(), the number of rows specified by iters will be
  fetched directly into the defined output buffers and additional rows
  equivalent to the prefetch count will be prefetched. If there are no
  additional rows, then the fetch is complete without calling
  OCIStmtFetch().
  The execute call will return errors if the statement has bind data types that
  are not supported in an Oracle7 server.
  Parameters
  svchp (IN/OUT) - service context handle.
  stmtp (IN/OUT) - an statement handle - defines the statement and the
  associated data to be executed at the server. It is invalid to pass in a
  statement handle that has bind of data types only supported in release 8.0
  when srvchp points to an Oracle7 server.
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error. If the statement is being
  batched and it is successful, then this handle will contain this particular
  statement execution specific errors returned from the server when the batch is
  flushed.
  iters (IN) - the number of times this statement is executed for non-Select
  statements. For Select statements, if iters is non-zero, then defines must
  have been done for the statement handle. The execution fetches iters rows into
  these predefined buffers and prefetches more rows depending upon the prefetch
  row count. This function returns an error if iters=0 for non-SELECT
  statements.
  rowoff (IN) - the index from which the data in an array bind is relevant for
  this multiple row execution.
  snap_in (IN) - this parameter is optional. if supplied, must point to a
  snapshot descriptor of type OCI_DTYPE_SNAP.  The contents of this descriptor
  must be obtained from the snap_out parameter of a previous call.  The
  descriptor is ignored if the SQL is not a SELECT.  This facility allows
  multiple service contexts to ORACLE to see the same consistent snapshot of the
  database's committed data.  However, uncommitted data in one context is not
  visible to another context even using the same snapshot.
  snap_out (OUT) - this parameter optional. if supplied, must point to a
  descriptor of type OCI_DTYPE_SNAP. This descriptor is filled in with an
  opaque representation which is the current ORACLE "system change
  number" suitable as a snap_in input to a subsequent call to OCIStmtExecute().
  This descriptor should not be used any longer than necessary in order to avoid
  "snapshot too old" errors.
  mode (IN) - The modes are:
  If OCI_DEFAULT_MODE, the default mode, is selected, the request is
  immediately executed. Error handle contains diagnostics on error if any.
  OCI_EXACT_FETCH - if the statement is a SQL SELECT, this mode is
  only valid if the application has set the prefetch row count prior to this
  call. In this mode, the OCI library will get up to the number of rows
  specified (i.e., prefetch row count plus iters). If the number of rows
  returned by the query is greater than this value, OCI_ERROR will be
  returned with ORA-01422 as the implementation specific error in a
  diagnostic record. If the number of rows returned by the query is
  smaller than the prefetch row count, OCI_SUCCESS_WITH_INFO will
  be returned with ORA-01403 as the implementation specific error. The
  prefetch buffer size is ignored and the OCI library tries to allocate all the
  space required to contain the prefetched rows. The exact fetch semantics
  apply to only the top level rows. No more rows can be fetched for this
  query at the end of the call.
  OCI_KEEP_FETCH_STATE - the result set rows (not yet fetched) of this
  statement executed in this transaction will be maintained when the
  transaction is detached for migration. By default, a query is cancelled
  when a transaction is detached for migration. This mode is the default
  mode when connected to a V7 server.
  Related Functions
  OCIStmtPrepare()
}
function TZOracle9iPlainDriver.StmtExecute(svchp: POCISvcCtx;
  stmtp: POCIStmt; errhp: POCIError; iters, rowoff: ub4; snap_in,
  snap_out: POCISnapshot; mode: ub4): sword;
begin
  Result := OCIStmtExecute(svchp, stmtp, errhp, iters, rowoff,
    snap_in, snap_out, mode);
end;

{** from aciap.h
  OCIParamGet()
  Name
  OCI Get PARaMeter
  Purpose
  Returns a descriptor of a parameter specified by position in the describe
  handle or statement handle.
  Syntax
  sword OCIParamGet ( const void       *hndlp,
                    ub4         htype,
                    OCIError    *errhp,
                    void    **parmdpp,
                    ub4         pos );
  Comments
  This call returns a descriptor of a parameter specified by position in the
  describe handle or statement handle. Parameter descriptors are always
  allocated internally by the OCI library. They are read-only.
  OCI_NO_DATA may be returned if there are no parameter descriptors for this
  position.
  See Appendix B for more detailed information about parameter descriptor
  attributes.
  Parameters
  hndlp (IN) - a statement handle or describe handle. The OCIParamGet()
  function will return a parameter descriptor for this handle.
  htype (IN) - the type of the handle passed in the handle parameter. Valid
  types are OCI_HTYPE_DESCRIBE, for a describe handle OCI_HTYPE_STMT, for a
  statement handle
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  parmdpp (OUT) - a descriptor of the parameter at the position given in the pos
  parameter.
  pos (IN) - position number in the statement handle or describe handle. A
  parameter descriptor will be returned for this position.
  Note: OCI_NO_DATA may be returned if there are no parameter
  descriptors for this position.
  Related Functions
  OCIAttrGet(), OCIAttrSet()
}
function TZOracle9iPlainDriver.ParamGet(hndlp: Pointer; htype: ub4;
  errhp: POCIError; var parmdpp: Pointer; pos: ub4): sword;
begin
  Result := OCIParamGet(hndlp, htype, errhp, parmdpp, pos);
end;

{** from ociap.h
  OCIStmtFetch()
  Name
  OCI FetCH
  Purpose
  Fetches rows from a query.
  Syntax
  sword OCIStmtFetch ( OCIStmt     *stmtp,
                     OCIError    *errhp,
                     ub4         nrows,
                     ub2         orientation,
                     ub4         mode);
  Comments
  The fetch call is a local call, if prefetched rows suffice. However, this is
  transparent to the application. If LOB columns are being read, LOB locators
  are fetched for subsequent LOB operations to be performed on these locators.
  Prefetching is turned off if LONG columns are involved.
  A fetch with nrows set to 0 rows effectively cancels the fetch for this
  statement.
  Parameters
  stmtp (IN) - a statement (application request) handle.
  errhp (IN) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  nrows (IN) - number of rows to be fetched from the current position.
  orientation (IN) - for release 8.0, the only acceptable value is
  OCI_FETCH_NEXT, which is also the default value.
  mode (IN) - for release 8.0, beta-1, the following mode is defined.
  OCI_DEFAULT - default mode
  OCI_EOF_FETCH - indicates that it is the last fetch from the result set.
  If nrows is non-zero, setting this mode effectively cancels fetching after
  retrieving nrows, otherwise it cancels fetching immediately.
  Related Functions
  OCIAttrGet()
}
function TZOracle9iPlainDriver.StmtFetch(stmtp: POCIStmt; errhp: POCIError;
  nrows: ub4; orientation: ub2; mode: ub4): sword;
begin
  Result := OCIStmtFetch(stmtp, errhp, nrows, orientation, mode);
end;

{** from ociap.h
  OCIStmtFetch2()
  Name
  OCI FetCH2
  Purpose
  Fetches rows from a query.
  Syntax
  sword OCIStmtFetch2 ( OCIStmt     *stmtp,
                     OCIError    *errhp,
                     ub4         nrows,
                     ub2         orientation,
                     ub4         scrollOffset,
                     ub4         mode);
  Comments
  The fetch call works similar to the OCIStmtFetch call with the
  addition of the fetchOffset parameter. It can be used on any
  statement handle, whether it is scrollable or not. For a
  non-scrollable statement handle, the only acceptable value
  will be OCI_FETCH_NEXT, and the fetchOffset parameter will be
  ignored. Applications are encouraged to use this new call.

  A fetchOffset with OCI_FETCH_RELATIVE is equivalent to
  OCI_FETCH_CURRENT with a value of 0, is equivalent to
  OCI_FETCH_NEXT with a value of 1, and equivalent to
  OCI_FETCH_PRIOR with a value of -1. Note that the range of
  accessible rows is [1,OCI_ATTR_ROW_COUNT] beyond which an
  error could be raised if sufficient rows do not exist in

  The fetch call is a local call, if prefetched rows suffice. However, this is
  transparent to the application. If LOB columns are being read, LOB locators
  are fetched for subsequent LOB operations to be performed on these locators.
  Prefetching is turned off if LONG columns are involved.
  A fetch with nrows set to 0 rows effectively cancels the fetch for this
  statement.
  Parameters
  stmtp (IN) - a statement (application request) handle.
  errhp (IN) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  nrows (IN) - number of rows to be fetched from the current position.
  It defaults to 1 for orientation OCI_FETCH_LAST.
  orientation (IN) -  The acceptable values are as follows, with
  OCI_FETCH_NEXT being the default value.
  OCI_FETCH_CURRENT gets the current row,
  OCI_FETCH_NEXT gets the next row from the current position,
  OCI_FETCH_FIRST gets the first row in the result set,
  OCI_FETCH_LAST gets the last row in the result set,
  OCI_FETCH_PRIOR gets the previous row from the current row in the result set,
  OCI_FETCH_ABSOLUTE will fetch the row number (specified by fetchOffset
  parameter) in the result set using absolute positioning,
  OCI_FETCH_RELATIVE will fetch the row number (specified by fetchOffset
  parameter) in the result set using relative positioning.
  scrollOffset(IN) - offset used with the OCI_FETCH_ABSOLUTE and
  OCI_FETCH_RELATIVE orientation parameters only. It specify
  the new current position for scrollable result set. It is
  ignored for non-scrollable result sets.
  mode (IN) - for release 8.0, beta-1, the following mode is defined.
  OCI_DEFAULT - default mode
  OCI_EOF_FETCH - indicates that it is the last fetch from the result set.
  If nrows is non-zero, setting this mode effectively cancels fetching after
  retrieving nrows, otherwise it cancels fetching immediately.
  Related Functions
  OCIAttrGet()
}
function TZOracle9iPlainDriver.StmtFetch2(stmtp: POCIStmt; errhp: POCIError;
  const nrows: ub4; const orientation: ub2; const fetchOffset: sb4; const mode: ub4): sword;
begin
  Result := OCIStmtFetch2(stmtp, errhp, nrows, orientation, fetchOffset, mode);
end;

{** from ociap.h
  OCIDefineByPos()
  Name
  OCI Define By Position
  Purpose
  Associates an item in a select-list with the type and output data buffer.
  Syntax
  sb4 OCIDefineByPos (
                OCIStmt     *stmtp,
                OCIDefine   **defnp,
                OCIError    *errhp,
                ub4         position,
                void       *valuep,
                sb4         value_sz,
                ub2         dty,
                void       *indp,
                ub2         *rlenp,
                ub2         *rcodep,
                ub4         mode );
  Comments
  This call defines an output buffer which will receive data retreived from
  Oracle. The define is a local step which is necessary when a SELECT statement
  returns data to your OCI application.
  This call also implicitly allocates the define handle for the select-list item.
  Defining attributes of a column for a fetch is done in one or more calls. The
  first call is to OCIDefineByPos(), which defines the minimal attributes
  required to specify the fetch.
  This call takes as a parameter a define handle, which must have been
  previously allocated with a call to OCIHandleAlloc().
  Following the call to OCIDefineByPos() additional define calls may be
  necessary for certain data types or fetch modes:
  A call to OCIDefineArrayOfStruct() is necessary to set up skip parameters
  for an array fetch of multiple columns.
  A call to OCIDefineObject() is necessary to set up the appropriate
  attributes of a named data type fetch. In this case the data buffer pointer
  in ocidefn() is ignored.
  Both OCIDefineArrayOfStruct() and OCIDefineObject() must be called
  after ocidefn() in order to fetch multiple rows with a column of named
  data types.
  For a LOB define, the buffer pointer must be a lob locator of type
  OCILobLocator , allocated by the OCIDescAlloc() call. LOB locators, and not
  LOB values, are always returned for a LOB column. LOB values can then be
  fetched using OCI LOB calls on the fetched locator.
  For NCHAR (fixed and varying length), the buffer pointer must point to an
  array of bytes sufficient for holding the required NCHAR characters.
  Nested table columns are defined and fetched like any other named data type.
  If the mode parameter is this call is set to OCI_DYNAMIC_FETCH, the client
  application can fetch data dynamically at runtime.
  Runtime data can be provided in one of two ways:
  callbacks using a user-defined function which must be registered with a
  subsequent call to OCIDefineDynamic(). When the client library needs a
  buffer to return the fetched data, the callback will be invoked and the
  runtime buffers provided will return a piece or the whole data.
  a polling mechanism using calls supplied by the OCI. This mode is
  assumed if no callbacks are defined. In this case, the fetch call returns the
  OCI_NEED_DATA error code, and a piecewise polling method is used
  to provide the data.
  Related Functions: For more information about using the
  OCI_DYNAMIC_FETCH mode, see the section "Runtime Data
  Allocation and Piecewise Operations" on page 5-16 of Volume 1..
  For more information about the define step, see the section "Defining"
  on page 2-30.
  Parameters
  stmtp (IN) - a handle to the requested SQL query operation.
  defnp (IN/OUT) - a pointer to a pointer to a define handle which is implicitly
  allocated by this call.  This handle is used to  store the define information
  for this column.
  errhp (IN) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  position (IN) - the position of this value in the select list. Positions are
  1-based and are numbered from left to right. For example, in the SELECT
  statement
  SELECT empno, ssn, mgrno FROM employees;
  empno is at position 1, ssn is at position 2, and mgrno is at position 3.
  valuep (IN/OUT) - a pointer to a buffer or an array of buffers of the type
  specified in the dty parameter. A number of buffers can be specified when
  results for more than one row are desired in a single fetch call.
  value_sz (IN) - the size of each valuep buffer in bytes. If the data is stored
  internally in VARCHAR2 format, the number of characters desired, if different
  from the buffer size in bytes, may be additionally specified by the using
  OCIAttrSet().
  In an NLS conversion environment, a truncation error will be generated if the
  number of bytes specified is insufficient to handle the number of characters
  desired.
  dty (IN) - the data type. Named data type (SQLT_NTY) and REF (SQLT_REF)
  are valid only if the environment has been intialized with in object mode.
  indp - pointer to an indicator variable or array. For scalar data types,
  pointer to sb2 or an array of sb2s. Ignored for named data types. For named
  data types, a pointer to a named data type indicator structure or an array of
  named data type indicator structures is associated by a subsequent
  OCIDefineObject() call.
  See the section "Indicator Variables" on page 2-43 for more information about
  indicator variables.
  rlenp (IN/OUT) - pointer to array of length of data fetched. Each element in
  rlenp is the length of the data in the corresponding element in the row after
  the fetch.
  rcodep (OUT) - pointer to array of column-level return codes
  mode (IN) - the valid modes are:
  OCI_DEFAULT. This is the default mode.
  OCI_DYNAMIC_FETCH. For applications requiring dynamically
  allocated data at the time of fetch, this mode must be used. The user may
  additionally call OCIDefineDynamic() to set up a callback function that
  will be invoked to receive the dynamically allocated buffers and to set
  up the memory allocate/free callbacks and the context for the callbacks.
  valuep and value_sz are ignored in this mode.
  Related Functions
  OCIDefineArrayOfStruct(), OCIDefineDynamic(), OCIDefineObject()
}
function TZOracle9iPlainDriver.DefineByPos(stmtp: POCIStmt;
  var defnpp: POCIDefine; errhp: POCIError; position: ub4; valuep: Pointer;
  value_sz: sb4; dty: ub2; indp, rlenp, rcodep: Pointer; mode: ub4): sword;
begin
  Result := OCIDefineByPos(stmtp, defnpp, errhp, position,
    valuep, value_sz, dty, indp, rlenp, rcodep, mode);
end;

{** from ociap.h
  OCIBindByPos()
  Name
  OCI Bind by Position
  Purpose
  Creates an association between a program variable and a placeholder in a SQL
  statement or PL/SQL block.
  Syntax
  sword OCIBindByPos (
                OCIStmt      *stmtp,
                OCIBind      **bindp,
                OCIError     *errhp,
                ub4          position,
                void        *valuep,
                sb4          value_sz,
                ub2          dty,
                void        *indp,
                ub2          *alenp,
                ub2          *rcodep,
                ub4          maxarr_len,
                ub4          *curelep,
                ub4          mode);

  Description
  This call is used to perform a basic bind operation. The bind creates an
  association between the address of a program variable and a placeholder in a
  SQL statement or PL/SQL block. The bind call also specifies the type of data
  which is being bound, and may also indicate the method by which data will be
  provided at runtime.
  This function also implicitly allocates the bind handle indicated by the bindp
  parameter.
  Data in an OCI application can be bound to placeholders statically or
  dynamically. Binding is static when all the IN bind data and the OUT bind
  buffers are well-defined just before the execute. Binding is dynamic when the
  IN bind data and the OUT bind buffers are provided by the application on
  demand at execute time to the client library. Dynamic binding is indicated by
  setting the mode parameter of this call to OCI_DATA_AT_EXEC.
  Related Functions: For more information about dynamic binding, see
  the section "Runtime Data Allocation and Piecewise Operations" on
  page 5-16
  Both OCIBindByName() and OCIBindByPos() take as a parameter a bind handle,
  which is implicitly allocated by the bind call A separate bind handle is
  allocated for each placeholder the application is binding.
  Additional bind calls may be required to specify particular attributes
  necessary when binding certain data types or handling input data in certain
  ways:
  If arrays of structures are being utilized, OCIBindArrayOfStruct() must
  be called to set up the necessary skip parameters.
  If data is being provided dynamically at runtime, and the application
  will be using user-defined callback functions, OCIBindDynamic() must
  be called to register the callbacks.
  If a named data type is being bound, OCIBindObject() must be called to
  specify additional necessary information.
  Parameters
  stmth (IN/OUT) - the statement handle to the SQL or PL/SQL statement
  being processed.
  bindp (IN/OUT) - a pointer to a pointer to a bind handle which is implicitly
  allocated by this call.  The bind handle  maintains all the bind information
  for this particular input value. The handle is feed implicitly when the
  statement handle is deallocated.
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  position (IN) - the placeholder attributes are specified by position if
  ocibindp() is being called.
  valuep (IN/OUT) - a pointer to a data value or an array of data values of the
  type specified in the dty parameter. An array of data values can be specified
  for mapping into a PL/SQL table or for providing data for SQL multiple-row
  operations. When an array of bind values is provided, this is called an array
  bind in OCI terms. Additional attributes of the array bind (not bind to a
  column of ARRAY type) are set up in OCIBindArrayOfStruct() call.
  For a REF, named data type  bind, the valuep parameter is used only for IN
  bind data. The pointers to OUT buffers are set in the pgvpp parameter
  initialized by OCIBindObject(). For named data type and REF binds, the bind
  values are unpickled into the Object Cache. The OCI object navigational calls
  can then be used to navigate the objects and the refs in the Object Cache.
  If the OCI_DATA_AT_EXEC mode is specified in the mode parameter, valuep
  is ignored for all data types. OCIBindArrayOfStruct() cannot be used and
  OCIBindDynamic() must be invoked to provide callback functions if desired.
  value_sz (IN) - the size of a data value. In the case of an array bind, this is
  the maximum size of any element possible with the actual sizes being specified
  in the alenp parameter.
  If the OCI_DATA_AT_EXEC mode is specified, valuesz defines the maximum
  size of the data that can be ever provided at runtime for data types other than
  named data types or REFs.
  dty (IN) - the data type of the value(s) being bound. Named data types
  (SQLT_NTY) and REFs (SQLT_REF) are valid only if the application has been
  initialized in object mode. For named data types, or REFs, additional calls
  must be made with the bind handle to set up the datatype-specific attributes.
  indp (IN/OUT) - pointer to an indicator variable or array. For scalar data
  types, this is a pointer to sb2 or an array of sb2s. For named data types,
  this pointer is ignored and the actual pointer to the indicator structure or
  an array of indicator structures is initialized by OCIBindObject(). Ignored
  for dynamic binds.
  See the section "Indicator Variables" on page 2-43 for more information about
  indicator variables.
  alenp (IN/OUT) - pointer to array of actual lengths of array elements. Each
  element in alenp is the length of the data in the corresponding element in the
  bind value array before and after the execute. This parameter is ignored for
  dynamic binds.
  rcodep (OUT) - pointer to array of column level return codes. This parameter
  is ignored for dynamic binds.
  maxarr_len (IN) - the maximum possible number of elements of type dty in a
  PL/SQL binds. This parameter is not required for non-PL/SQL binds. If
  maxarr_len is non-zero, then either OCIBindDynamic() or
  OCIBindArrayOfStruct() can be invoked to set up additional bind attributes.
  curelep(IN/OUT) - a pointer to the actual number of elements. This parameter
  is only required for PL/SQL binds.
  mode (IN) - the valid modes for this parameter are:
  OCI_DEFAULT. This is default mode.
  OCI_DATA_AT_EXEC. When this mode is selected, the value_sz
  parameter defines the maximum size of the data that can be ever
  provided at runtime. The application must be ready to provide the OCI
  library runtime IN data buffers at any time and any number of times.
  Runtime data is provided in one of the two ways:
  callbacks using a user-defined function which must be registered
  with a subsequent call to OCIBindDynamic() .
  a polling mechanism using calls supplied by the OCI. This mode
  is assumed if no callbacks are defined.
  For more information about using the OCI_DATA_AT_EXEC mode, see
  the section "Runtime Data Allocation and Piecewise Operations" on
  page 5-16.
  When the allocated buffers are not required any more, they should be
  freed by the client.
  Related Functions
  OCIBindDynamic(), OCIBindObject(), OCIBindArrayOfStruct(), OCIAttrGet()
}
function TZOracle9iPlainDriver.BindByPos(stmtp: POCIStmt;
  var bindpp: POCIBind; errhp: POCIError; position: ub4; valuep: Pointer;
  value_sz: sb4; dty: ub2; indp, alenp, rcodep: Pointer; maxarr_len: ub4;
  curelep: Pointer; mode: ub4): sword;
begin
  Result := OCIBindByPos(stmtp, bindpp, errhp, position, valuep,
    value_sz, dty, indp, alenp, rcodep, maxarr_len, curelep, mode);
end;

{** from ociap.h
  OCIBindObject()
  Name
  OCI Bind Object
  Purpose
  This function sets up additional attributes which are required for a named
  data type (object)  bind.
  Syntax
  sword OCIBindObject ( OCIBind          *bindp,
                      OCIError         *errhp,
                      const OCIType    *type,
                      void            **pgvpp,
                      ub4              *pvszsp,
                      void            **indpp,
                      ub4              *indszp, );
  Comments
  This function sets up additional attributes which binding a named data type
  or a REF. An error will be returned if this function is called when the OCI
  environment has been initialized in non-object mode.
  This call takes as a paramter a type descriptor object (TDO) of datatype
  OCIType for the named data type being defined.  The TDO can be retrieved
  with a call to OCITypeByName().
  If the OCI_DATA_AT_EXEC mode was specified in ocibindn() or ocibindp(), the
  pointers to the IN buffers are obtained either using the callback icbfp
  registered in the OCIBindDynamic() call or by the OCIStmtSetPieceInfo() call.
  The buffers are dynamically allocated for the OUT data and the pointers to
  these buffers are returned either by calling ocbfp() registered by the
  OCIBindDynamic() or by setting the pointer to the buffer in the buffer passed
  in by OCIStmtSetPieceInfo() called when OCIStmtExecute() returned
  OCI_NEED_DATA. The memory of these client library- allocated buffers must be
  freed when not in use anymore by using the OCIObjectFreee() call.
  Parameters
  bindp ( IN/OUT) - the bind handle returned by the call to OCIBindByName()
  or OCIBindByPos().
  errhp ( IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  type ( IN) - points to the TDO which describes the type of the program
  variable being bound. Retrieved by calling OCITypeByName().
  pgvpp ( IN/OUT) - points to a pointer to the program variable buffer. For an
  array, pgvpp points to an array of pointers. When the bind variable is also an
  OUT variable, the OUT Named Data Type value or REF is allocated
  (unpickled) in the Object Cache, and a pointer to the value or REF is returned,
  At the end of execute, when all OUT values have been received, pgvpp points
  to an array of pointer(s) to these newly allocated named data types in the
  object cache.
  pgvpp is ignored if the OCI_DATA_AT_EXEC mode is set. Then the Named
  Data Type buffers are requested at runtime. For static array binds, skip
  factors may be specified using the OCIBindArrayOfStruct() call. The skip
  factors are used to compute the address of the next pointer to the value, the
  indicator structure and their sizes.
  pvszsp ( IN/OUT) - points to the size of the program variable. The size of the
  named data type is not required on input. For an array, pvszsp is an array of
  ub4s. On return, for OUT bind variables, this points to size(s) of the Named
  Data Types and REFs received. pvszsp is ignored if the OCI_DATA_AT_EXEC
  mode is set. Then the size of the buffer is taken at runtime.
  indpp ( IN/OUT) - points to a pointer to the program variable buffer
  containing the parallel indicator structure. For an array, points to an array
  of pointers. When the bind variable is also an OUT bind variable, memory is
  allocated in the object cache, to store the unpickled OUT indicator values. At
  the end of the execute when all OUT values have been received, indpp points
  to the pointer(s) to these newly allocated indicator structure(s).
  indpp is ignored if the OCI_DATA_AT_EXEC mode is set. Then the indicator
  is requested at runtime.
  indszp ( IN/OUT) - points to the size of the IN indicator structure program
  variable. For an array, it is an array of sb2s. On return for OUT bind
  variables, this points to size(s) of the received OUT indicator structures.
  indszp is ignored if the OCI_DATA_AT_EXEC mode is set. Then the indicator
  size is requested at runtime.
  Related Functions
  OCIAttrGet()
}
function TZOracle9iPlainDriver.BindObject(bindp: POCIBind; errhp: POCIError;
                const _type: POCIType; pgvpp: PPointer;
                pvszsp: pub4; indpp: PPointer;
                indszp: pub4): sword;
begin
  Result := OCIBindObject(bindp, errhp, _type, pgvpp, pvszsp, indpp,
    indszp);
end;

{** from ociap.h
  OCIDefineObject()
  Name
  OCI Define Named Data Type attributes
  Purpose
  Sets up additional attributes necessary for a Named Data Type define.
  Syntax
  sword OCIDefineObject ( OCIDefine       *defnp,
                        OCIError        *errhp,
                        const OCIType   *type,
                        void           **pgvpp,
                        ub4             *pvszsp,
                        void           **indpp,
                        ub4             *indszp );
  Comments
  This call sets up additional attributes necessary for a Named Data Type define.
  An error will be returned if this function is called when the OCI environment
  has been initialized in non-Object mode.
  This call takes as a paramter a type descriptor object (TDO) of datatype
  OCIType for the named data type being defined.  The TDO can be retrieved
  with a call to OCITypeByName().
  See the description of OCIInitialize() on page 13 - 43 for more information
  about initializing the OCI process environment.
  Parameters
  defnp (IN/OUT) - a define handle previously allocated in a call to
  OCIDefineByPos().
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  type (IN, optional) - points to the Type Descriptor Object (TDO) which
  describes the type of the program variable. Only used for program variables
  of type SQLT_NTY. This parameter is optional, and may be passed as NULL
  if it is not being used.
  pgvpp (IN/OUT) - points to a pointer to a program variable buffer. For an
  array, pgvpp points to an array of pointers. Memory for the fetched named data
  type instance(s) is dynamically allocated in the object cache. At the end of
  the fetch when all the values have been received, pgvpp points to the
  pointer(s) to these newly allocated named data type instance(s). The
  application must call OCIObjectMarkDel() to deallocate the named data type
  instance(s) when they are no longer needed.
  pvszsp (IN/OUT) - points to the size of the program variable. For an array, it
  is an array of ub4s. On return points to the size(s) of unpickled fetched
  values.
  indpp (IN/OUT) - points to a pointer to the program variable buffer
  containing the parallel indicator structure. For an array, points to an array
  of pointers. Memory is allocated to store the indicator structures in the
  object cache. At the end of the fetch when all values have been received,
  indpp points to the pointer(s) to these newly allocated indicator structure(s).
  indszp (IN/OUT) - points to the size(s) of the indicator structure program
  variable. For an array, it is an array of ub4s. On return points to the size(s)
  of the unpickled fetched indicator values.
  Related Functions
  OCIAttrGet()
}
function TZOracle9iPlainDriver.DefineObject(defnpp: POCIDefine;
  errhp: POCIError; _type: POCIHandle; pgvpp,pvszsp,indpp,indszp:pointer): sword;
begin
  Result := OCIDefineObject(defnpp, errhp, _type, pgvpp, pvszsp,
    indpp, indszp);
end;

{* from ociap.h
  OCIResultSetToStmt()
  Name
  OCI convert Result Set to Statement Handle
  Purpose
  Converts a descriptor to statement handle for fetching rows.
  Syntax
  sword OCIResultSetToStmt ( OCIResult     *rsetdp,
                           OCIError      *errhp );
  Comments
  Converts a descriptor to statement handle for fetching rows.
  A result set descriptor can be allocated with a call to OCIDescAlloc().
  Parameters
  rsetdp (IN/OUT) - a result set descriptor pointer.
  errhp (IN/OUT) - an error handle which can be passed to OCIErrorGet() for
  diagnostic information in the event of an error.
  Related Functions
  OCIDescAlloc()
}
function TZOracle9iPlainDriver.ResultSetToStmt(rsetdp: POCIHandle;
  errhp: POCIError): sword;
begin
  Result := OCIResultSetToStmt(rsetdp, errhp);
end;

function TZOracle9iPlainDriver.DescriptorAlloc(const parenth: POCIEnv;
  var descpp: POCIDescriptor; const htype: ub4; const xtramem_sz: integer;
  usrmempp: Pointer): sword;
begin
  Result := OCIDescriptorAlloc(parenth, descpp{%H-}, htype,
    xtramem_sz, usrmempp);
end;

function TZOracle9iPlainDriver.DescriptorFree(const descp: Pointer;
  const htype: ub4): sword;
begin
  Result := OCIDescriptorFree(descp, htype);
end;

function TZOracle9iPlainDriver.LobOpen(svchp: POCISvcCtx; errhp: POCIError;
  locp: POCILobLocator; mode: ub1): sword;
begin
  Result := OCILobOpen(svchp, errhp, locp, mode);
end;

function TZOracle9iPlainDriver.LobRead(svchp: POCISvcCtx; errhp: POCIError;
  locp: POCILobLocator; var amtp: ub4; offset: ub4; bufp: Pointer;
  bufl: ub4; ctxp, cbfp: Pointer; csid: ub2; csfrm: ub1): sword;
begin
  Result := OCILobRead(svchp, errhp, locp, amtp, offset, bufp,
    bufl, ctxp, cbfp, csid, csfrm);
end;

function TZOracle9iPlainDriver.LobTrim(svchp: POCISvcCtx; errhp: POCIError;
  locp: POCILobLocator; newlen: ub4): sword;
begin
  Result := OCILobTrim(svchp, errhp, locp, newlen);
end;

function TZOracle9iPlainDriver.LobWrite(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator; var amtp: ub4; offset: ub4;
  bufp: Pointer; bufl: ub4; piece: ub1; ctxp, cbfp: Pointer; csid: ub2;
  csfrm: ub1): sword;
begin
  Result := OCILobWrite(svchp, errhp, locp, amtp, offset,
    bufp, bufl, piece, ctxp, cbfp, csid, csfrm);
end;

function TZOracle9iPlainDriver.LobCreateTemporary(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator; csid: ub2; csfrm, lobtype: ub1;
  cache: LongBool; duration: OCIDuration): sword;
begin
  Result := OCILobCreateTemporary(svchp, errhp, locp,
    csid, csfrm, lobtype, cache, duration);
end;

function TZOracle9iPlainDriver.LobIsTemporary(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator;
  var is_temporary: LongBool): sword;
begin
  Result := OCILobIsTemporary(svchp, errhp, locp, is_temporary);
end;

function TZOracle9iPlainDriver.LobFreeTemporary(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator): sword;
begin
  Result := OCILobFreeTemporary(svchp, errhp, locp);
end;

function TZOracle9iPlainDriver.LobCharSetForm ( envhp: POCIEnv; errhp: POCIError;
    const locp: POCILobLocator; csfrm: pub1): sword;
begin
  Result := OCILobCharSetForm(envhp, errhp, locp, csfrm);
end;

function TZOracle9iPlainDriver.LobCharSetId ( envhp: POCIEnv; errhp: POCIError;
    const locp: POCILobLocator; csid: pub2): sword;
begin
  Result := OCILobCharSetId (envhp, errhp, locp, csid);
end;

function TZOracle9iPlainDriver.LobClose(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator): sword;
begin
  Result := OCILobClose(svchp, errhp, locp);
end;

(* ---------------------- OCIIntervalGetYearMonth --------------------

  DESCRIPTION
     Gets year month from an interval
  PARAMETERS
        hndl (IN) - Session/Env handle.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        year    (OUT)   - year value
        month   (OUT)   - month value
        result     (IN)  - resulting interval
  RETURNS
        OCI_SUCCESS on success
        OCI_INVALID_HANDLE if 'err' is NULL.
*)
function TZOracle9iPlainDriver.IntervalGetYearMonth(hndl: POCIHandle;
  err: POCIError; yr,mnth: psb4; const int_result: POCIInterval): sword;
begin
  Result := OCIIntervalGetYearMonth(hndl, err, yr, mnth, int_result);
end;

(* ---------------------- OCIIntervalGetDaySecond --------------------

  DESCRIPTION
     Gets values of day second interval
  PARAMETERS
        hndl (IN) - Session/Env handle.
        err (IN/OUT) - error handle. If there is an error, it is
                recorded in 'err' and this function returns OCI_ERROR.
                The error recorded in 'err' can be retrieved by calling
                OCIErrorGet().
        day     (OUT) - number of days
        hour    (OUT) - number of hours
        min     (OUT) - number of mins
        sec     (OUT) - number of secs
        fsec    (OUT) - number of fractional seconds
        result     (IN)  - resulting interval
  RETURNS
        OCI_SUCCESS on success
        OCI_INVALID_HANDLE if 'err' is NULL.
*)
function TZOracle9iPlainDriver.IntervalGetDaySecond(hndl: POCIHandle; err:  POCIError;
  dy, hr, mm, ss, fsec: psb4; const int_result: POCIInterval): sword;
begin
  Result := OCIIntervalGetDaySecond(hndl, err, dy, hr, mm, ss, fsec, int_result);
end;

function TZOracle9iPlainDriver.DateTimeConstruct(hndl: POCIEnv;
  err: POCIError; datetime: POCIDateTime; year: sb2; month, day, hour, min,
  sec: ub1; fsec: ub4; timezone: text; timezone_length: size_t): sword;
begin
  Result := OCIDateTimeConstruct(hndl, err, datetime, year, month, day,
    hour, min, sec, fsec, timezone, timezone_length);
end;

function TZOracle9iPlainDriver.DateTimeGetDate(hndl: POCIEnv;
  err: POCIError; const date: POCIDateTime; var year: sb2; var month,
  day: ub1): sword;
begin
  Result := OCIDateTimeGetDate(hndl, err, date, year, month, day);
end;

function TZOracle9iPlainDriver.DateTimeGetTime(hndl: POCIEnv;
  err: POCIError; datetime: POCIDateTime; var hour, minute, sec: ub1;
  var fsec: ub4): sword;
begin
  Result := OCIDateTimeGetTime(hndl, err, datetime,
    hour, minute, sec, fsec);
end;

function TZOracle9iPlainDriver.TypeByRef(env: POCIEnv; err: POCIError;
  type_ref: POCIRef; pin_duration: OCIDuration; get_option: OCITypeGetOpt;
  tdo: PPOCIType): sword;
begin
  Result := OCITypeByRef(env, err, type_ref, pin_duration, get_option, tdo);
end;

{ > ori.h}
{
   NAME: OCIObjectNew - OCI new (create) a standalone instance
   PARAMETERS:
        env  (IN/OUT) - OCI environment handle initialized in object mode
        err  (IN/OUT) - error handle. If there is an error, it is
                        recorded in 'err' and this function returns OCI_ERROR.
                        The error recorded in 'err' can be retrieved by calling
                        OCIErrorGet().
        svc      (IN) - OCI service handle.
        typecode (IN) - the typecode of the type of the instance.
        tdo      (IN, optional) - pointer to the type descriptor object. The
                        TDO describes the type of the instance that is to be
                        created. Refer to OCITypeByName() for obtaining a TDO.
                        The TDO is required for creating a named type (e.g. an
                        object or a collection).
        table (IN, optional) - pointer to a table object which specifies a
                        table in the server.  This parameter can be set to NULL
                        if no table is given. See the description below to find
                        out how the table object and the TDO are used together
                        to determine the kind of instances (persistent,
                        transient, value) to be created. Also see
                        OCIObjectPinTable() for retrieving a table object.
        duration (IN) - this is an overloaded parameter. The use of this
                        parameter is based on the kind of the instance that is
                        to be created.
                        a) persistent object. This parameter specifies the
                           pin duration.
                        b) transient object. This parameter specififes the
                           allocation duration and pin duration.
                        c) value. This parameter specifies the allocation
                           duration.
        value    (IN)  - specifies whether the created object is a value.
                         If TRUE, then a value is created. Otherwise, a
                         referenceable object is created.  If the instance is
                         not an object, then this parameter is ignored.
        instance (OUT) - address of the newly created instance

   REQUIRES:
        - a valid OCI environment handle must be given.
   DESCRIPTION:
        This function creates a new instance of the type specified by the
        typecode or the TDO. Based on the parameters 'typecode' (or 'tdo'),
        'value' and 'table', different kinds of instances can be created:

                                     The parameter 'table' is not NULL?

                                               yes              no
             ----------------------------------------------------------------
             | object type (value=TRUE)   |   value         |   value       |
             ----------------------------------------------------------------
             | object type (value=FALSE)  | persistent obj  | transient obj |
       type  ----------------------------------------------------------------
             | built-in type              |   value         |   value       |
             ----------------------------------------------------------------
             | collection type            |   value         |   value       |
             ----------------------------------------------------------------

        This function allocates the top level memory chunk of an OTS instance.
        The attributes in the top level memory are initialized (e.g. an
        attribute of varchar2 is initialized to a vstring of 0 length).

        If the instance is an object, the object is marked existed but is
        atomically null.

        FOR PERSISTENT OBJECTS:
        The object is marked dirty and existed.  The allocation duration for
        the object is session. The object is pinned and the pin duration is
        specified by the given parameter 'duration'.

        FOR TRANSIENT OBJECTS:
        The object is pinned. The allocation duration and the pin duration are
        specified by the given parameter 'duration'.

        FOR VALUES:
        The allocation duration is specified by the given parameter 'duration'.

   RETURNS:
        if environment handle or error handle is null, return
        OCI_INVALID_HANDLE.
        if operation suceeds, return OCI_SUCCESS.
        if operation fails, return OCI_ERROR.
}
function TZOracle9iPlainDriver.ObjectNew(env: POCIEnv; err: POCIError;
  const svc: POCISvcCtx; typecode: OCITypeCode; tdo: POCIType; table: Pointer;
  duration: OCIDuration; value: Longbool; instance: PPointer): sword;
begin
  Result := OCIObjectNew(env, err, svc, typecode, tdo, table,
    duration, value, instance);
end;
{**
 NAME: OCIObjectPin - OCI pin a referenceable object
 PARAMETERS:
      env        (IN/OUT) - OCI environment handle initialized in object mode
      err        (IN/OUT) - error handle. If there is an error, it is
                            recorded in 'err' and this function returns
                            OCI_ERROR. The error recorded in 'err' can be
                            retrieved by calling OCIErrorGet().
      object_ref     (IN) - the reference to the object.
      corhdl         (IN) - handle for complex object retrieval.
      pin_option     (IN) - See description below.
      pin_duration   (IN) - The duration of which the object is being accesed
                            by a client. The object is implicitly unpinned at
                            the end of the pin duration.
                            If OCI_DURATION_NULL is passed, there is no pin
                            promotion if the object is already loaded into
                            the cache. If the object is not yet loaded, then
                            the pin duration is set to OCI_DURATION_DEFAULT.
      lock_option    (IN) - lock option (e.g., exclusive). If a lock option
                            is specified, the object is locked in the server.
                            See 'oro.h' for description about lock option.
      object        (OUT) - the pointer to the pinned object.

 REQUIRES:
      - a valid OCI environment handle must be given.
 DESCRIPTION:

      This function pins a referenceable object instance given the object
      reference. The process of pinning serves three purposes:

      1) locate an object given its reference. This is done by the object
         cache which keeps track of the objects in the object heap.

      2) notify the object cache that an object is being in use. An object
         can be pinned many times. A pinned object will remain in memory
         until it is completely unpinned (see OCIObjectUnpin()).

      3) notify the object cache that a persistent object is being in use
         such that the persistent object cannot be aged out.  Since a
         persistent object can be loaded from the server whenever is needed,
         the memory utilization can be increased if a completely unpinned
         persistent object can be freed (aged out), even before the
         allocation duration is expired.

      Also see OCIObjectUnpin() for more information about unpinning.

      FOR PERSISTENT OBJECTS:

      When pinning a persistent object, if it is not in the cache, the object
      will be fetched from the persistent store. The allocation duration of
      the object is session. If the object is already in the cache, it is
      returned to the client.  The object will be locked in the server if a
      lock option is specified.

      This function will return an error for a non-existent object.

      A pin option is used to specify the copy of the object that is to be
      retrieved:

      1) If option is OCI_PIN_ANY (pin any), if the object is already
         in the environment heap, return this object. Otherwise, the object
         is retrieved from the database.  This option is useful when the
         client knows that he has the exclusive access to the data in a
         session.

      2) If option is OCI_PIN_LATEST (pin latest), if the object is
         not cached, it is retrieved from the database.  If the object is
         cached, it is refreshed with the latest version. See
         OCIObjectRefresh() for more information about refreshing.

      3) If option is OCI_PIN_RECENT (pin recent), if the object is loaded
         into the cache in the current transaction, the object is returned.
         If the object is not loaded in the current transaction, the object
         is refreshed from the server.

      FOR TRANSIENT OBJECTS:

      This function will return an error if the transient object has already
      been freed. This function does not return an error if an exclusive
      lock is specified in the lock option.

 RETURNS:
      if environment handle or error handle is null, return
      OCI_INVALID_HANDLE.
      if operation suceeds, return OCI_SUCCESS.
      if operation fails, return OCI_ERROR.
}
function TZOracle9iPlainDriver.ObjectPin(env: POCIEnv; err: POCIError;
  const object_ref: POCIRef; const corhdl: POCIComplexObject;
  const pin_option: OCIPinOpt; const pin_duration: OCIDuration;
  const lock_option: OCILockOpt; _object: PPointer): sword;
begin
  Result := OCIObjectPin(env, err, object_ref, corhdl,
    pin_option, pin_duration, lock_option, _object);
end;

{**
  NAME: OCIObjectUnpin - OCI unpin a referenceable object
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
      This function unpins an object.  An object is completely unpinned when
        1) the object was unpinned N times after it has been pinned N times
           (by calling OCIObjectPin()).
        2) it is the end of the pin duration
        3) the function OCIObjectPinCountReset() is called

      There is a pin count associated with each object which is incremented
      whenever an object is pinned. When the pin count of the object is zero,
      the object is said to be completely unpinned. An unpinned object can
      be freed without error.

      FOR PERSISTENT OBJECTS:
      When a persistent object is completely unpinned, it becomes a candidate
      for aging. The memory of an object is freed when it is aged out. Aging
      is used to maximize the utilization of memory.  An dirty object cannot
      be aged out unless it is flushed.

      FOR TRANSIENT OBJECTS:
      The pin count of the object is decremented. A transient can be freed
      only at the end of its allocation duration or when it is explicitly
      deleted by calling OCIObjectFree().

      FOR VALUE:
      This function will return an error for value.

  RETURNS:
      if environment handle or error handle is null, return
      OCI_INVALID_HANDLE.
      if operation suceeds, return OCI_SUCCESS.
      if operation fails, return OCI_ERROR.
}
function TZOracle9iPlainDriver.ObjectUnpin(env: POCIEnv; err: POCIError;
  const _object: Pointer): sword;
begin
  Result := OCIObjectUnpin(env, err, _object);
end;

{**
   NAME: OCIObjectFree - OCI free (and unpin) an standalone instance
   PARAMETERS:
        env    (IN/OUT) - OCI environment handle initialized in object mode
        err    (IN/OUT) - error handle. If there is an error, it is
                          recorded in 'err' and this function returns
                          OCI_ERROR.  The error recorded in 'err' can be
                          retrieved by calling OCIErrorGet().
        instance   (IN) - pointer to a standalone instance.
        flags      (IN) - If OCI_OBJECT_FREE_FORCE is set, free the object
                          even if it is pinned or dirty.
                          If OCI_OBJECT_FREE_NONULL is set, the null
                          structure will not be freed.
   REQUIRES:
        - a valid OCI environment handle must be given.
        - The instance to be freed must be standalone.
        - If the instance is a referenceable object, the object must be pinned.
   DESCRIPTION:
        This function deallocates all the memory allocated for an OTS instance,
        including the null structure.

        FOR PERSISTENT OBJECTS:
        This function will return an error if the client is attempting to free
        a dirty persistent object that has not been flushed. The client should
        either flush the persistent object or set the parameter 'flag' to
        OCI_OBJECT_FREE_FORCE.

        This function will call OCIObjectUnpin() once to check if the object
        can be completely unpin. If it succeeds, the rest of the function will
        proceed to free the object.  If it fails, then an error is returned
        unless the parameter 'flag' is set to OCI_OBJECT_FREE_FORCE.

        Freeing a persistent object in memory will not change the persistent
        state of that object at the server.  For example, the object will
        remain locked after the object is freed.

        FOR TRANSIENT OBJECTS:

        This function will call OCIObjectUnpin() once to check if the object
        can be completely unpin. If it succeeds, the rest of the function will
        proceed to free the object.  If it fails, then an error is returned
        unless the parameter 'flag' is set to OCI_OBJECT_FREE_FORCE.

        FOR VALUES:
        The memory of the object is freed immediately.

   RETURNS:
        if environment handle or error handle is null, return
        OCI_INVALID_HANDLE.
        if operation suceeds, return OCI_SUCCESS.
        if operation fails, return OCI_ERROR.
 }
function TZOracle9iPlainDriver.ObjectFree(hndl: POCIEnv; err: POCIError;
  instance:POCIHandle;flags :ub2):sword;
begin
  Result := OCIObjectFree(hndl, err, instance, flags);
end;

{**
   NAME: OCIObjectGetTypeRef - get the type reference of a standalone object
   PARAMETERS:
        env   (IN/OUT) - OCI environment handle initialized in object mode
        err   (IN/OUT) - error handle. If there is an error, it is
                         recorded in 'err' and this function returns
                         OCI_ERROR.  The error recorded in 'err' can be
                         retrieved by calling OCIErrorGet().
        instance  (IN) - pointer to an standalone instance
        type_ref (OUT) - reference to the type of the object.  The reference
                         must already be allocated.
   REQUIRES:
        - a valid OCI environment handle must be given.
        - The instance must be standalone.
        - If the object is referenceable, the specified object must be pinned.
        - The reference must already be allocated.
   DESCRIPTION:
        This function returns a reference to the TDO of a standalone instance.
   RETURNS:
        if environment handle or error handle is null, return
        OCI_INVALID_HANDLE.
        if operation suceeds, return OCI_SUCCESS.
        if operation fails, return OCI_ERROR.
}
function TZOracle9iPlainDriver.ObjectGetTypeRef(env: POCIEnv; err: POCIError;
  const instance:pointer; type_ref: POCIRef): sword;
begin
  Result := OCIObjectGetTypeRef(env, err, instance, type_ref);
end;

function TZOracle9iPlainDriver.DescribeAny(svchp: POCISvcCtx;
  errhp: POCIError; objptr: Pointer; objnm_len: ub4; objptr_typ,
  info_level, objtyp: ub1; dschp: POCIDescribe): sword;
begin
  Result := OCIDescribeAny(svchp, errhp, objptr,
    objnm_len, objptr_typ, info_level, objtyp, dschp);
end;

(*
function TZOracle9iPlainDriver.ObjectUnmark(env: POCIEnv; err: POCIError;
  const _object:pointer): sword;
begin
  Result := OracleAPI.OCIObjectUnmark(env, err, _object);
end;

function TZOracle9iPlainDriver.ObjectUnmarkByRef(env: POCIEnv; err: POCIError;
  const ref: POCIRef): sword;
begin
  Result := OracleAPI.OCIObjectUnmarkByRef(env, err, ref);
end;

function TZOracle9iPlainDriver.ObjectMarkDeleteByRef(env: POCIEnv;
  err: POCIError; const object_ref:POCIRef): sword;
begin
  Result := OracleAPI.OCIObjectMarkDeleteByRef(env, err, object_ref);
end;

function TZOracle9iPlainDriver.ObjectMarkDelete(env: POCIEnv; err: POCIError;
  const instance:pointer): sword;
begin
  Result := OracleAPI.OCIObjectMarkDelete(env, err, instance);
end;

function TZOracle9iPlainDriver.ObjectFlush(env: POCIEnv; err: POCIError;
  const _object: pointer): sword;
begin
  Result := OracleAPI.OCIObjectFlush(env, err, _object);
end;

function TZOracle9iPlainDriver.ObjectRefresh(env: POCIEnv; err: POCIError;
  _object: pointer): sword;
begin
  Result := OracleAPI.OCIObjectRefresh(env, err, _object);
end;

function TZOracle9iPlainDriver.ObjectCopy(env: POCIEnv; err: POCIError;
  const svc: POCISvcCtx; const source, null_source, target, null_target: pointer;
  const tdo: POCIType; const duration: OCIDuration; const option: ub1): sword;
begin
  Result := OracleAPI.OCIObjectCopy(env, err, svc, source, null_source, target,
    null_target, tdo, duration, option);
end;

function TZOracle9iPlainDriver.ObjectGetObjectRef(env: POCIEnv; err: POCIError;
  const _object: pointer; object_ref: POCIRef): sword;
begin
  Result := OracleAPI.OCIObjectGetObjectRef(env, err, _object, object_ref);
end;

function TZOracle9iPlainDriver.ObjectMakeObjectRef(env: POCIEnv; err: POCIError;
  const svc: POCISvcCtx; const table: pointer; const values: PPointer;
  const array_len: ub4; object_ref: POCIRef): sword;
begin
  Result := OracleAPI.OCIObjectMakeObjectRef(env, err, svc, table, values,
    array_len, object_ref);
end;

function TZOracle9iPlainDriver.ObjectGetPrimaryKeyTypeRef(env: POCIEnv;
  err: POCIError; const svc:POCISvcCtx; const table: pointer;
  type_ref: POCIRef): sword;
begin
  Result := OracleAPI.OCIObjectGetPrimaryKeyTypeRef(env, err, svc, table,
    type_ref);
end;

function TZOracle9iPlainDriver.ObjectGetInd(env: POCIEnv; err: POCIError;
  const instance: pointer; null_struct: PPointer): sword;
begin
  Result := OracleAPI.OCIObjectGetInd(env, err, instance, null_struct);
end;

function TZOracle9iPlainDriver.ObjectExists(env: POCIEnv; err: POCIError;
  const ins: pointer; exist: PBoolean): sword;
begin
  Result := OracleAPI.OCIObjectExists(env, err, ins, exist);
end;

function TZOracle9iPlainDriver.ObjectGetProperty(envh: POCIEnv; errh: POCIError;
  const obj: pointer; const propertyId: OCIObjectPropId;
  _property: pointer; size: Pub4): sword;
begin
  Result := OracleAPI.OCIObjectGetProperty(envh, errh, obj, propertyId,
    _property, size);
end;

function TZOracle9iPlainDriver.ObjectIsLocked(env: POCIEnv; err: POCIError;
  const ins: pointer; lock: Pboolean): sword;
begin
  Result := OracleAPI.OCIObjectIsLocked(env, err, ins, lock);
end;

function TZOracle9iPlainDriver.ObjectIsDirty(env: POCIEnv; err: POCIError;
  const ins: pointer; dirty: PBoolean): sword;
begin
  Result := OracleAPI.OCIObjectIsDirty(env, err, ins, dirty);
end;

function TZOracle9iPlainDriver.ObjectPinTable(env: POCIEnv; err: POCIError;
  const svc:POCISvcCtx; const schema_name: Poratext; const s_n_length: ub4;
  const object_name: Poratext; const o_n_length:ub4;
  const scope_obj_ref: POCIRef; const pin_duration: OCIDuration;
  _object: PPointer): sword;
begin
  Result := OracleAPI.OCIObjectPinTable(env, err, svc, schema_name, s_n_length,
    object_name, o_n_length, scope_obj_ref, pin_duration, _object);
end;

function TZOracle9iPlainDriver.ObjectArrayPin(env: POCIEnv; err: POCIError;
  const ref_array: PPOCIRef; const array_size: ub4;
  const cor_array: PPOCIComplexObject; const cor_array_size: ub4;
  const pin_option: OCIPinOpt; const pin_duration: OCIDuration;
  const lock: OCILockOpt; obj_array: PPointer;
  pos: Pub4): sword;
begin
  Result := OracleAPI.OCIObjectArrayPin(env, err, ref_array, array_size,
    cor_array, cor_array_size, pin_option, pin_duration, lock, obj_array, pos);
end;

function TZOracle9iPlainDriver.CacheFlush(env: POCIEnv; err: POCIError;
  const svc:POCISvcCtx; const context: pointer; const get: TOCICacheFlushGet;
  ref: PPOCIRef): sword;
begin
  Result := OracleAPI.OCICacheFlush(env, err, svc, context, get, ref);
end;

function TZOracle9iPlainDriver.CacheRefresh(env: POCIEnv; err: POCIError;
  const svc:POCISvcCtx; const option: OCIRefreshOpt; const context: pointer;
  get: TOCICacheRefreshGet; ref: PPOCIRef): sword;
begin
  Result := OracleAPI.OCICacheRefresh(env, err, svc, option, context, get, ref);
end;

function TZOracle9iPlainDriver.CacheUnpin(env: POCIEnv; err: POCIError;
  const svc:POCISvcCtx): sword;
begin
  Result := OracleAPI.OCICacheUnpin(env, err, svc);
end;

function TZOracle9iPlainDriver.CacheFree(env: POCIEnv; err: POCIError;
  const svc: POCISvcCtx): sword;
begin
  Result := OracleAPI.OCICacheFree(env, err, svc);
end;

function TZOracle9iPlainDriver.CacheUnmark(env: POCIEnv; err: POCIError;
  const svc: POCISvcCtx): sword;
begin
  Result := OracleAPI.OCICacheUnmark(env, err, svc);
end;

function TZOracle9iPlainDriver.DurationBegin(env: POCIEnv; err: POCIError;
  svc: POCISvcCtx; const parent: OCIDuration;
  dur: POCIDuration): sword;
begin
  Result := OracleAPI.OCIDurationBegin(env, err, svc, parent, dur);
end;

function TZOracle9iPlainDriver.DurationEnd(env: POCIEnv; err: POCIError;
  svc: POCISvcCtx; duration: OCIDuration): sword;
begin
  Result := OracleAPI.OCIDurationEnd(env, err, svc, duration);
end;
{ < ori.h}

function TZOracle9iPlainDriver.Break(svchp: POCISvcCtx;
  errhp: POCIError): sword;
begin
  Result := OracleAPI.OCIBreak(svchp, errhp);
end;

function TZOracle9iPlainDriver.LobAppend(svchp: POCISvcCtx;
  errhp: POCIError; dst_locp, src_locp: POCILobLocator): sword;
begin
  Result := OracleAPI.OCILobAppend(svchp, errhp, dst_locp, src_locp);
end;

function TZOracle9iPlainDriver.LobAssign(svchp: POCISvcCtx; errhp: POCIError;
  src_locp: POCILobLocator; var dst_locpp: POCILobLocator): sword;
begin
  Result := OracleAPI.OCILobAssign(svchp, errhp, src_locp, dst_locpp);
end;

function TZOracle9iPlainDriver.LobCopy(svchp: POCISvcCtx; errhp: POCIError;
  dst_locp, src_locp: POCILobLocator; amount, dst_offset,
  src_offset: ub4): sword;
begin
  Result := OracleAPI.OCILobCopy(svchp, errhp, dst_locp, src_locp,
    amount, dst_offset, src_offset);
end;

function TZOracle9iPlainDriver.LobDisableBuffering(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator): sword;
begin
  Result := OracleAPI.OCILobDisableBuffering(svchp, errhp, locp);
end;

function TZOracle9iPlainDriver.LobEnableBuffering(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator): sword;
begin
  Result := OracleAPI.OCILobEnableBuffering(svchp, errhp, locp);
end;

function TZOracle9iPlainDriver.LobErase(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator; var amount: ub4;
  offset: ub4): sword;
begin
  Result := OracleAPI.OCILobErase(svchp, errhp, locp, amount, offset);
end;

function TZOracle9iPlainDriver.LobFileExists(svchp: POCISvcCtx;
  errhp: POCIError; filep: POCILobLocator; var flag: Boolean): sword;
begin
  Result := OracleAPI.OCILobFileExists(svchp, errhp, filep, flag);
end;

function TZOracle9iPlainDriver.LobFileGetName(envhp: POCIEnv;
  errhp: POCIError; filep: POCILobLocator; dir_alias: text;
  var d_length: ub2; filename: text; var f_length: ub2): sword;
begin
  Result := OracleAPI.OCILobFileGetName(envhp, errhp, filep, dir_alias,
    d_length, filename, f_length);
end;

function TZOracle9iPlainDriver.LobFileSetName(envhp: POCIEnv;
  errhp: POCIError; var filep: POCILobLocator; dir_alias: text;
  d_length: ub2; filename: text; f_length: ub2): sword;
begin
  Result := OracleAPI.OCILobFileSetName(envhp, errhp, filep, dir_alias,
    d_length, filename, f_length);
end;

function TZOracle9iPlainDriver.LobFlushBuffer(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator; flag: ub4): sword;
begin
  Result := OracleAPI.OCILobFlushBuffer(svchp, errhp, locp, flag);
end;

function TZOracle9iPlainDriver.LobGetLength(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator; var lenp: ub4): sword;
begin
  Result := OracleAPI.OCILobGetLength(svchp, errhp, locp, lenp);
end;

function TZOracle9iPlainDriver.LobIsOpen(svchp: POCISvcCtx;
  errhp: POCIError; locp: POCILobLocator; var flag: LongBool): sword;
begin
  Result := OracleAPI.OCILobIsOpen(svchp, errhp, locp, flag);
end;

function TZOracle9iPlainDriver.LobLoadFromFile(svchp: POCISvcCtx;
  errhp: POCIError; dst_locp, src_locp: POCILobLocator; amount, dst_offset,
  src_offset: ub4): sword;
begin
  Result := OracleAPI.OCILobLoadFromFile(svchp, errhp, dst_locp, src_locp,
    amount, dst_offset, src_offset);
end;

function TZOracle9iPlainDriver.LobLocatorIsInit(envhp: POCIEnv;
  errhp: POCIError; locp: POCILobLocator;
  var is_initialized: LongBool): sword;
begin
  Result := OracleAPI.OCILobLocatorIsInit(envhp, errhp, locp,
    is_initialized);
end;

function TZOracle9iPlainDriver.Reset(svchp: POCISvcCtx;
  errhp: POCIError): sword;
begin
  Result := OracleAPI.OCIReset(svchp, errhp);
end;

function TZOracle9iPlainDriver.NumberInc(err: POCIError; number: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberInc(err, number);
end;

function TZOracle9iPlainDriver.NumberDec(err: POCIError; number: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberDec(err, number);
end;

procedure TZOracle9iPlainDriver.NumberSetZero(err: POCIError; number: POCINumber);
begin
  OracleAPI.OCINumberSetZero(err, number);
end;

procedure TZOracle9iPlainDriver.NumberSetPi(err: POCIError; number: POCINumber);
begin
  OracleAPI.OCINumberSetPi(err, number);
end;

function  TZOracle9iPlainDriver.NumberAdd(err: POCIError; const number1: POCINumber;
  const number2: POCINumber; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberAdd(err, number1, number2, _result);
end;

function TZOracle9iPlainDriver.NumberSub(err: POCIError; const number1: POCINumber;
  const number2: POCINumber; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberSub(err, number1, number2, _result);
end;

function TZOracle9iPlainDriver.NumberMul(err: POCIError; const number1: POCINumber;
  const number2: POCINumber; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberMul(err, number1, number2, _result);
end;

function TZOracle9iPlainDriver.NumberDiv(err: POCIError; const number1: POCINumber;
  const number2: POCINumber; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberDiv(err, number1, number2, _result);
end;

function TZOracle9iPlainDriver.NumberMod(err: POCIError; const number1: POCINumber;
  const number2: POCINumber; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberMod(err, number1, number2, _result);
end;

function TZOracle9iPlainDriver.NumberIntPower(err: POCIError; const number1: POCINumber;
  const number2: POCINumber; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberIntPower(err, number1, number2, _result);
end;

function TZOracle9iPlainDriver.NumberShift(err: POCIError; const number: POCINumber;
  const nDig: sword; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberShift(err, number, nDig, _result);
end;

function TZOracle9iPlainDriver.NumberNeg(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberNeg(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberToText(err: POCIError; const number: POCINumber;
  const fmt: Poratext; fmt_length: ub4; const nls_params: Poratext;
  nls_p_length: ub4; buf_size: pub4; buf: poratext): sword;
begin
  Result := OracleAPI.OCINumberToText(err, number, fmt, fmt_length, nls_params,
    nls_p_length, buf_size, buf);
end;

function TZOracle9iPlainDriver.NumberFromText(err: POCIError; const str: poratext;
  str_length: ub4; const fmt: poratext; fmt_length: ub4;
  const nls_params: poratext; nls_p_length: ub4; number: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberFromText(err, str, str_length, fmt, fmt_length,
    nls_params, nls_p_length, number);
end;

function TZOracle9iPlainDriver.NumberToInt(err: POCIError; const number: POCINumber;
  rsl_length: uword; rsl_flag: uword; rsl: Pointer): sword;
begin
  Result := OracleAPI.OCINumberToInt(err, number, rsl_length, rsl_flag, rsl);
end;

function TZOracle9iPlainDriver.NumberFromInt(err: POCIError; const inum: Pointer;
  inum_length: uword; inum_s_flag: uword; number: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberFromInt(err, inum, inum_length, inum_s_flag, number);
end;

function TZOracle9iPlainDriver.NumberToReal(err: POCIError; const number: POCINumber;
  rsl_length: uword; rsl: Pointer): sword;
begin
  Result := OracleAPI.OCINumberToReal(err, number, rsl_length, rsl);
end;

function TZOracle9iPlainDriver.NumberToRealArray(err: POCIError; const number: PPOCINumber;
  elems: uword; rsl_length: uword; rsl: Pointer): sword;
begin
  Result := OracleAPI.OCINumberToRealArray(err, number, elems, rsl_length, rsl);
end;

function TZOracle9iPlainDriver.NumberFromReal(err: POCIError; const rnum: Pointer;
  rnum_length: uword; number: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberFromReal(err, rnum, rnum_length, number);
end;

function TZOracle9iPlainDriver.NumberCmp(err: POCIError; const number1: POCINumber;
  const number2: POCINumber; _result: psword): sword;
begin
  Result := OracleAPI.OCINumberCmp(err, number1, number2, _result);
end;

function TZOracle9iPlainDriver.NumberSign(err: POCIError; const number: POCINumber;
  _result: psword): sword;
begin
  Result := OracleAPI.OCINumberSign(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberIsZero(err: POCIError; const number: POCINumber;
  _Result: pboolean): sword;
begin
  Result := OracleAPI.OCINumberIsZero(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberIsInt(err: POCIError; const number: POCINumber;
  _result: Pboolean): sword;
begin
  Result := OracleAPI.OCINumberIsInt(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberAssign(err: POCIError; const from: POCINumber;
  _to: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberAssign(err, from, _to);
end;

function TZOracle9iPlainDriver.NumberAbs(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberAbs(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberCeil(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberCeil(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberFloor(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberFloor(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberSqrt(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberSqrt(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberTrunc(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberTrunc(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberPower(err: POCIError; const base: POCINumber;
  const number: POCINumber; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberPower(err, base, number, _result);
end;

function TZOracle9iPlainDriver.NumberRound(err: POCIError; const number: POCINumber;
  decplace: sword; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberRound(err, number, decplace, _result);
end;

function TZOracle9iPlainDriver.NumberPrec(err: POCIError; const number: POCINumber;
  nDigs: sword; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberPrec(err, number, nDigs, _result);
end;

function TZOracle9iPlainDriver.NumberSin(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberSin(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberArcSin(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberArcSin(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberHypSin(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberHypSin(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberCos(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberCos(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberArcCos(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberArcCos(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberHypCos(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberHypCos(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberTan(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberTan(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberArcTan(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberArcTan(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberArcTan2(err: POCIError; const number1: POCINumber;
  const number2: POCINumber; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberArcTan2(err, number1, number2, _result);
end;

function TZOracle9iPlainDriver.NumberHypTan(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberHypTan(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberExp(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberExp(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberLn(err: POCIError; const number: POCINumber;
  _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberLn(err, number, _result);
end;

function TZOracle9iPlainDriver.NumberLog(err: POCIError; const base: POCINumber;
  const number: POCINumber; _result: POCINumber): sword;
begin
  Result := OracleAPI.OCINumberLog(err, base, number, _result);
end;

function TZOracle9iPlainDriver.TableSize(hndl: POCIEnv; err: POCIError;
  const tbl: POCITable; size: psb4): sword;
begin
  Result := OracleAPI.OCITableSize(hndl, err, tbl, size);
end;

function TZOracle9iPlainDriver.TableExists(hndl: POCIEnv; err: POCIError;
  const tbl: POCITable; index: sb4; exists: PBoolean): sword;
begin
  Result := OracleAPI.OCITableExists(hndl, err, tbl, index, exists);
end;

function TZOracle9iPlainDriver.TableDelete(hndl: POCIEnv; err: POCIError;
  index: sb4; tbl: POCITable): sword;
begin
  Result := OracleAPI.OCITableDelete(hndl, err, index, tbl);
end;

function TZOracle9iPlainDriver.TableFirst(hndl: POCIEnv; err: POCIError;
  const tbl: POCITable; index: sb4): sword;
begin
  Result := OracleAPI.OCITableFirst(hndl, err, tbl, index);
end;

function TZOracle9iPlainDriver.TableLast(hndl: POCIEnv; err: POCIError;
  const tbl: POCITable; index: sb4): sword;
begin
  Result := OracleAPI.OCITableLast(hndl, err, tbl, index);
end;

function TZOracle9iPlainDriver.TableNext(hndl: POCIEnv; err: POCIError;
  index: sb4; const tbl: POCITable; next_index: psb4; exists: PBoolean): sword;
begin
  Result := OracleAPI.OCITableNext(hndl, err, index, tbl, next_index, exists);
end;

function TZOracle9iPlainDriver.TablePrev(hndl: POCIEnv; err: POCIError;
  index: sb4; const tbl: POCITable; prev_index: psb4; exists: PBoolean): sword;
begin
  Result := OracleAPI.OCITablePrev(hndl, err, index, tbl, prev_index, exists);
end;

function TZOracle9iPlainDriver.ObjectSetAttr(env: POCIEnv; err: POCIError;
  instance: Pointer; null_struct: pointer; tdo: POCIType;
  const names: PPAnsiChar; const lengths: pub4; const name_count: ub4;
  const indexes: pub4; const index_count: ub4; const null_status: POCIInd;
  const attr_null_struct: Pointer; const attr_value: Pointer): sword;
begin
  Result := OracleAPI.OCIObjectSetAttr(env, err, instance, null_struct, tdo,
    names, lengths, name_count, indexes, index_count, null_status,
    attr_null_struct, attr_value);
end;

function TZOracle9iPlainDriver.ObjectGetAttr(env: POCIEnv; err: POCIError;
  instance: Pointer; null_struct: Pointer; tdo: POCIType;
  const names: PPoratext; const lengths: pub4; const name_count: ub4;
  const indexes: pub4; const index_count: ub4; attr_null_status: POCIInd;
  attr_null_struct, attr_value: PPointer; attr_tdo: PPOCIType): sword;
begin
  Result := OracleAPI.OCIObjectGetAttr(env, err, instance, null_struct, tdo,
    names, lengths, name_count, indexes, index_count, attr_null_status,
    attr_null_struct, attr_value, attr_tdo);
end;

function TZOracle9iPlainDriver.StmtGetPieceInfo(stmtp: POCIStmt;
  errhp: POCIError; var hndlpp: Pointer; var typep: ub4; var in_outp: ub1;
  var iterp, idxp: ub4; var piecep: ub1): sword;
begin
  Result := OracleAPI.OCIStmtGetPieceInfo(stmtp, errhp, hndlpp, typep,
    in_outp, iterp, idxp, piecep);
end;

function TZOracle9iPlainDriver.StmtSetPieceInfo(handle: Pointer;
  typep: ub4; errhp: POCIError; buf: Pointer; var alenp: ub4; piece: ub1;
  indp: Pointer; var rcodep: ub2): sword;
begin
  Result := OracleAPI.OCIStmtSetPieceInfo(handle, typep,
    errhp, buf, alenp, piece, indp, rcodep);
end;

function TZOracle9iPlainDriver.TransDetach(svchp: POCISvcCtx;
  errhp: POCIError; flags: ub4): sword;
begin
  Result := OracleAPI.OCITransDetach(svchp, errhp, flags);
end;

function TZOracle9iPlainDriver.TransForget(svchp: POCISvcCtx;
  errhp: POCIError; flags: ub4): sword;
begin
  Result := OracleAPI.OCITransForget(svchp, errhp, flags);
end;

function TZOracle9iPlainDriver.TransPrepare(svchp: POCISvcCtx;
  errhp: POCIError; flags: ub4): sword;
begin
  Result := OracleAPI.OCITransPrepare(svchp, errhp, flags);
end;

function TZOracle9iPlainDriver.DateTimeAssign(hndl: POCIEnv;
  err: POCIError; const from: POCIDateTime; _to: POCIDateTime): sword;
begin
  Result := OracleAPI.OCIDateTimeAssign(hndl, err, from, _to);
end;

function TZOracle9iPlainDriver.DateTimeCheck(hndl: POCIEnv; err: POCIError;
  const date: POCIDateTime; var valid: ub4): sword;
begin
  Result := OracleAPI.OCIDateTimeCheck(hndl, err, date, valid);
end;

function TZOracle9iPlainDriver.DateTimeCompare(hndl: POCIEnv;
  err: POCIError; const date1, date2: POCIDateTime;
  var _result: sword): sword;
begin
  Result := OracleAPI.OCIDateTimeCompare(hndl, err, date1, date2, _result);
end;

function TZOracle9iPlainDriver.DateTimeConvert(hndl: POCIEnv;
  err: POCIError; indate, outdate: POCIDateTime): sword;
begin
  Result := OracleAPI.OCIDateTimeConvert(hndl, err, indate, outdate);
end;

function TZOracle9iPlainDriver.DateTimeFromText(hndl: POCIEnv;
  err: POCIError; const date_str: text; d_str_length: size_t;
  const fmt: text; fmt_length: ub1; const lang_name: text;
  lang_length: size_t; date: POCIDateTime): sword;
begin
  Result := OracleAPI.OCIDateTimeFromText(hndl, err,
    date_str, d_str_length, fmt, fmt_length, lang_name, lang_length, date);
end;

function TZOracle9iPlainDriver.DateTimeGetTimeZoneName(hndl: POCIEnv;
  err: POCIError; datetime: POCIDateTime; var buf: ub1;
  var buflen: ub4): sword;
begin
  Result := OracleAPI.OCIDateTimeGetTimeZoneName(hndl, err, datetime,
    buf, buflen);
end;

function TZOracle9iPlainDriver.DateTimeGetTimeZoneOffset(hndl: POCIEnv;
  err: POCIError; const datetime: POCIDateTime; var hour,
  minute: sb1): sword;
begin
  Result := OracleAPI.OCIDateTimeGetTimeZoneOffset(hndl, err, datetime,
    hour, minute);
end;

function TZOracle9iPlainDriver.DateTimeSysTimeStamp(hndl: POCIEnv;
  err: POCIError; sys_date: POCIDateTime): sword;
begin
  Result := OracleAPI.OCIDateTimeSysTimeStamp(hndl, err, sys_date);
end;

function TZOracle9iPlainDriver.DateTimeToText(hndl: POCIEnv;
  err: POCIError; const date: POCIDateTime; const fmt: text; fmt_length,
  fsprec: ub1; const lang_name: text; lang_length: size_t;
  var buf_size: ub4; buf: text): sword;
begin
  Result := OracleAPI.OCIDateTimeToText(hndl, err, date, fmt, fmt_length,
    fsprec, lang_name, lang_length, buf_size, buf);
end;

{ort.h}
function TZOracle9iPlainDriver.TypeIterNew(env: POCIEnv; err: POCIError;
  const tdo: POCIType; iterator_ort: PPOCITypeIter): sword;
begin
  Result := OracleAPI.OCITypeIterNew(env, err, tdo, iterator_ort);
end;

function TZOracle9iPlainDriver.TypeIterSet(env: POCIEnv; err: POCIError;
  const tdo: POCIType; iterator_ort: POCITypeIter): sword;
begin
  Result := OracleAPI.OCITypeIterSet(env, err, tdo, iterator_ort);
end;

function TZOracle9iPlainDriver.TypeIterFree(env: POCIEnv; err: POCIError;
  iterator_ort: POCITypeIter): sword;
begin
  Result := OracleAPI.OCITypeIterFree(env, err, iterator_ort);
end;

function TZOracle9iPlainDriver.TypeByName(env: POCIEnv; err: POCIError;
  const svc: POCISvcCtx; schema_name: Poratext; const s_length: ub4;
  const type_name: Poratext; const t_length: ub4; version_name: Poratext;
  const v_length: ub4; const pin_duration: OCIDuration;
  const get_option: OCITypeGetOpt; tdo: PPOCIType): sword;
begin
  Result := OracleAPI.OCITypeByName(env, err, svc, schema_name, s_length,
    type_name, t_length, version_name, v_length, pin_duration, get_option, tdo);
end;

function TZOracle9iPlainDriver.TypeArrayByName(env: POCIEnv; err: POCIError;
  svc: POCISvcCtx; array_len: ub4; schema_name:  PPoratext; s_length: Pub4;
  type_name: PPoratext; t_length: Pub4; version_name: PPoratext;
  v_length: Pub4; pin_duration: OCIDuration; get_option: OCITypeGetOpt;
  tdo: PPOCIType): sword;
begin
  Result := OracleAPI.OCITypeArrayByName(env, err, svc, array_len, schema_name,
    s_length, type_name, t_length, version_name, v_length, pin_duration,
    get_option, tdo);
end;

function TZOracle9iPlainDriver.TypeArrayByRef(env: POCIEnv; err: POCIError;
  array_len: ub4; type_ref: PPOCIRef; pin_duration: OCIDuration;
  get_option: OCITypeGetOpt; tdo: PPOCIType): sword;
begin
  Result := OracleAPI.OCITypeArrayByRef(env, err, array_len, type_ref,
    pin_duration, get_option, tdo);
end;

function TZOracle9iPlainDriver.TypeName(env: POCIEnv; err: POCIError;
  tdo: POCIType; n_length: Pub4): poratext;
begin
  Result := OracleAPI.OCITypeName(env, err, tdo, n_length);
end;

function TZOracle9iPlainDriver.TypeSchema(env: POCIEnv; err: POCIError;
  const tdo: POCIType; n_length: Pub4): poratext;
begin
  Result := OracleAPI.OCITypeSchema(env, err, tdo, n_length);
end;

function TZOracle9iPlainDriver.TypeTypeCode(env: POCIEnv; err: POCIError;
  const tdo: POCIType): OCITypeCode;
begin
  Result := OracleAPI.OCITypeTypeCode(env, err, tdo);
end;

function TZOracle9iPlainDriver.TypeCollTypeCode(env: POCIEnv; err:POCIError;
  const tdo: POCIType): OCITypeCode;
begin
  Result := OracleAPI.OCITypeCollTypeCode(env, err, tdo);
end;

function TZOracle9iPlainDriver.TypeVersion(env: POCIEnv; err: POCIError;
  const tdo: POCIType; v_length: Pub4): poratext;
begin
  Result := OracleAPI.OCITypeVersion(env, err, tdo, v_length);
end;

function TZOracle9iPlainDriver.TypeAttrs(env: POCIEnv; err: POCIError;
  const tdo:POCIType): ub4;
begin
  Result := OracleAPI.OCITypeAttrs(env, err, tdo);
end;

function TZOracle9iPlainDriver.TypeMethods(env: POCIEnv; err: POCIError;
  const tdo: POCIType): ub4;
begin
  Result := OracleAPI.OCITypeMethods(env, err, tdo);
end;

function TZOracle9iPlainDriver.TypeElemName(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem; n_length:Pub4): poratext;
begin
  Result := OracleAPI.OCITypeElemName(env, err, elem, n_length);
end;

function TZOracle9iPlainDriver.TypeElemTypeCode(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem): OCITypeCode;
begin
  Result := OracleAPI.OCITypeElemTypeCode(env, err, elem);
end;

function TZOracle9iPlainDriver.TypeElemType(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem; elem_tdo:PPOCIType): sword;
begin
  Result := OracleAPI.OCITypeElemType(env, err, elem, elem_tdo);
end;

function TZOracle9iPlainDriver.TypeElemFlags(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem): ub4;
begin
  Result := OracleAPI.OCITypeElemFlags(env, err, elem);
end;

function TZOracle9iPlainDriver.TypeElemNumPrec(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem): ub1;
begin
  Result := OracleAPI.OCITypeElemNumPrec(env, err, elem);
end;

function TZOracle9iPlainDriver.TypeElemNumScale(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem): sb1;
begin
  Result := OracleAPI.OCITypeElemNumScale(env, err, elem);
end;

function TZOracle9iPlainDriver.TypeElemLength(env: POCIEnv; err: POCIError;
  const elem:POCITypeElem): ub4;
begin
  Result := OracleAPI.OCITypeElemLength(env, err, elem);
end;

function TZOracle9iPlainDriver.TypeElemCharSetID(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem): ub2;
begin
  Result := OracleAPI.OCITypeElemCharSetID(env, err, elem);
end;

function TZOracle9iPlainDriver.TypeElemCharSetForm(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem): ub2;
begin
  Result := OracleAPI.OCITypeElemCharSetForm(env, err, elem);
end;

function TZOracle9iPlainDriver.TypeElemParameterizedType(env: POCIEnv;
  err: POCIError; const elem: POCITypeElem; type_stored: PPOCIType): sword;
begin
  Result := OracleAPI.OCITypeElemParameterizedType(env, err, elem, type_stored);
end;

function TZOracle9iPlainDriver.TypeElemExtTypeCode(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem): OCITypeCode;
begin
  Result := OracleAPI.OCITypeElemExtTypeCode(env, err, elem);
end;

function TZOracle9iPlainDriver.TypeAttrByName(env: POCIEnv; err: POCIError;
  const tdo: POCIType; const name: Poratext; const n_length: ub4;
  elem: PPOCITypeElem): sword;
begin
  Result := OracleAPI.OCITypeAttrByName(env, err, tdo, name, n_length, elem);
end;

function TZOracle9iPlainDriver.TypeAttrNext(env: POCIEnv; err: POCIError;
  iterator_ort: POCITypeIter; elem: PPOCITypeElem): sword;
begin
  Result := OracleAPI.OCITypeAttrNext(env, err, iterator_ort, elem);
end;

function TZOracle9iPlainDriver.TypeCollElem(env: POCIEnv; err: POCIError;
  const tdo:POCIType; element: PPOCITypeElem): sword;
begin
  Result := OracleAPI.OCITypeCollElem(env, err, tdo, element);
end;

function TZOracle9iPlainDriver.TypeCollSize(env: POCIEnv; err: POCIError;
  const tdo: POCIType; num_elems: Pub4): sword;
begin
  Result := OracleAPI.OCITypeCollSize(env, err, tdo, num_elems);
end;

function TZOracle9iPlainDriver.TypeCollExtTypeCode(env: POCIEnv; err: POCIError;
  const tdo:POCIType; sqt_code: POCITypeCode): sword;
begin
  Result := OracleAPI.OCITypeCollExtTypeCode(env, err, tdo, sqt_code);
end;

function TZOracle9iPlainDriver.TypeMethodOverload(env: POCIEnv; err: POCIError;
  const tdo: POCIType; const method_name: Poratext;
  const m_length: ub4): ub4;
begin
  Result := OracleAPI.OCITypeMethodOverload(env, err, tdo, method_name,
    m_length);
end;

function TZOracle9iPlainDriver.TypeMethodByName(env: POCIEnv; err: POCIError;
  const tdo: POCIType; const method_name: Poratext; const m_length: ub4;
  mdos: PPOCITypeMethod): sword;
begin
  Result := OracleAPI.OCITypeMethodByName(env, err, tdo, method_name, m_length,
    mdos);
end;

function TZOracle9iPlainDriver.TypeMethodNext(env: POCIEnv; err: POCIError;
  iterator_ort: POCITypeIter; mdo: PPOCITypeMethod): sword;
begin
  Result := OracleAPI.OCITypeMethodNext(env, err, iterator_ort, mdo);
end;

function TZOracle9iPlainDriver.TypeMethodName(env:POCIEnv; err: POCIError;
  const mdo: POCITypeMethod; n_length: Pub4): poratext;
begin
  Result := OracleAPI.OCITypeMethodName(env, err, mdo, n_length);
end;

function TZOracle9iPlainDriver.TypeMethodEncap(env: POCIEnv; err: POCIError;
  const mdo: POCITypeMethod): OCITypeEncap;
begin
  Result := OracleAPI.OCITypeMethodEncap(env, err, mdo);
end;

function TZOracle9iPlainDriver.TypeMethodFlags(env: POCIEnv; err: POCIError;
    const mdo:POCITypeMethod): OCITypeMethodFlag;
begin
  Result := OracleAPI.OCITypeMethodFlags(env, err, mdo);
end;

function TZOracle9iPlainDriver.TypeMethodMap(env: POCIEnv; err: POCIError;
  const tdo: POCIType; mdo: PPOCITypeMethod): sword;
begin
  Result := OracleAPI.OCITypeMethodMap(env, err, tdo, mdo);
end;

function TZOracle9iPlainDriver.TypeMethodOrder(env: POCIEnv; err: POCIError;
  const tdo: POCIType; mdo: PPOCITypeMethod): sword;
begin
  Result := OracleAPI.OCITypeMethodOrder(env, err, tdo, mdo);
end;

function TZOracle9iPlainDriver.TypeMethodParams(env: POCIEnv; err: POCIError;
  const mdo: POCITypeMethod): ub4;
begin
  Result := OracleAPI.OCITypeMethodParams(env, err, mdo);
end;

function TZOracle9iPlainDriver.TypeResult(env: POCIEnv; err: POCIError;
  const mdo: POCITypeMethod; elem: PPOCITypeElem): sword;
begin
  Result := OracleAPI.OCITypeResult(env, err, mdo, elem);
end;

function TZOracle9iPlainDriver.TypeParamByPos(env: POCIEnv; err: POCIError;
  const mdo: POCITypeMethod; const position: ub4;
  elem: PPOCITypeElem): sword;
begin
  Result := OracleAPI.OCITypeParamByPos(env, err, mdo, position, elem);
end;

function TZOracle9iPlainDriver.TypeParamByName(env: POCIEnv; err: POCIError;
  const mdo: POCITypeMethod; const name: Poratext; const n_length: ub4;
  elem:PPOCITypeElem): sword;
begin
  Result := OracleAPI.OCITypeParamByName(env, err, mdo, name, n_length, elem);
end;

function TZOracle9iPlainDriver.TypeParamPos(env: POCIEnv; err: POCIError;
  const mdo: POCITypeMethod; const name: Poratext; const n_length: ub4;
  position: Pub4; elem: PPOCITypeElem): sword;
begin
  Result := OracleAPI.OCITypeParamPos(env, err, mdo, name, n_length, position,
    elem);
end;

function TZOracle9iPlainDriver.TypeElemParamMode(env: POCIEnv; err: POCIError;
  const elem: POCITypeElem): OCITypeParamMode;
begin
  Result := OracleAPI.OCITypeElemParamMode(env, err, elem);
end;

function TZOracle9iPlainDriver.TypeElemDefaultValue(env: POCIEnv;
  err: POCIError; const elem: POCITypeElem; d_v_length: Pub4): poratext;
begin
  Result := OracleAPI.OCITypeElemDefaultValue(env, err, elem, d_v_length);
end;

function TZOracle9iPlainDriver.TypeVTInit(env: POCIEnv; err: POCIError): sword;
begin
  Result := OracleAPI.OCITypeVTInit(env, err);
end;

function TZOracle9iPlainDriver.TypeVTInsert(env: POCIEnv; err: POCIError;
  const schema_name: Poratext; const s_n_length: ub4;
  const type_name: Poratext; const t_n_length: ub4;
  const user_version:Poratext; const u_v_length:ub4): sword;
begin
  Result := OracleAPI.OCITypeVTInsert(env, err, schema_name, s_n_length,
    type_name, t_n_length, user_version, u_v_length);
end;

function TZOracle9iPlainDriver.TypeVTSelect(env: POCIEnv; err: POCIError;
  const schema_name: Poratext; const s_n_length: ub4;
  const type_name: Poratext; const t_n_length: ub4; user_version: PPoratext;
  u_v_length: Pub4; version: Pub2): sword;
begin
  Result := OracleAPI.OCITypeVTSelect(env, err, schema_name, s_n_length,
    type_name, t_n_length, user_version, u_v_length, version);
end;
*)
{$ENDIF ZEOS_DISABLE_ORACLE}
end.


