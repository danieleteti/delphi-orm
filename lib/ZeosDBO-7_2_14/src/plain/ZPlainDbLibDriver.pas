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

unit ZPlainDbLibDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB}

uses Classes, ZCompatibility, ZPlainDriver, ZPlainDbLibConstants,
  {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF}
  {$IFDEF TLIST_IS_DEPRECATED},ZSysUtils{$ENDIF};

const
  NTWDBLIB_DLL_LOCATION ='ntwdblib.dll';
  LIBSYBDB_WINDOWS_DLL_LOCATION = 'libsybdb.dll';
  LIBSYBDB_LINUX_DLL_LOCATION = 'libsybdb.so';
  FREETDS_MSSQL_WINDOWS_DLL_LOCATION = 'sybdb.dll';
  FREETDS_LINUX_DLL_LOCATION = 'dblib.so';
  FREETDS_OSX_DLL_LOCATION = 'dblib.dylib';
  FREETDS_SYBASE_WINDOWS_DLL_LOCATION = 'sybdb.dll';

type
  TDBLibraryVendorType = (lvtFreeTDS, lvtMS, lvtSybase);
  { ** Represents a generic interface to DBLIB native API. }
  IZDBLibPlainDriver = interface(IZPlainDriver)
    ['{7731C3B4-0608-4B6B-B089-240AC43A3463}']

    procedure CheckError(dbProc: PDBPROCESS); deprecated;
    function GetErrorString(dbProc: PDBPROCESS): String;

    function dbDead(dbProc: PDBPROCESS): Boolean;
    function dbLogin: PLOGINREC;
    procedure dbLoginFree(Login: PLOGINREC);
    function dbSetLoginTime(Seconds: DBINT): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PAnsiChar; Item: DBINT): RETCODE;
    function dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
    function dbSetLUser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
    function dbSetLPwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
    function dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
    function dbSetLSecure(Login: PLOGINREC): RETCODE;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
    function dbSetVersion(Version: DBINT): RETCODE;
    function dbSetTime(Seconds: Integer): RETCODE;
    function dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
    function dbCancel(dbProc: PDBPROCESS): RETCODE;
    function dbCmd(const dbProc: PDBPROCESS; const Cmd: PAnsiChar): RETCODE;
    function dbSqlExec(dbProc: PDBPROCESS; Async: Boolean = False): RETCODE;
    function dbSqlExecSync(dbProc: PDBPROCESS): RETCODE;
    function dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: DBINT;
      Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE;
    function dbClose(dbProc: PDBPROCESS): RETCODE;
    function dbName(dbProc: PDBPROCESS): PAnsiChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): DBINT;
    function dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColSource(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbcoltypeinfo(Proc: PDBPROCESS; Column: Integer): PDBTYPEINFO;
    function dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBInt;
    function dbcolinfo(pdbhandle :PDBHANDLE; _Type: Integer; Column: DBINT;
      ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE;
    function dbData(dbProc: PDBPROCESS; Column: DBINT): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbConvert(dbProc: PDBPROCESS; SrcType: DBINT; Src: PByte;
      SrcLen: DBINT; DestType: DBINT; Dest: PByte; DestLen: DBINT): DBINT;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
    function dbCount(dbProc: PDBPROCESS): DBINT;
    function dbbind(Proc: PDBPROCESS; Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar; Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; Status: Byte;
      Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): DBINT;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean;
    function dbRetName(dbProc: PDBPROCESS; RetNum: DBINT): PAnsiChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: DBINT): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbdataready(Proc: PDBPROCESS): LongBool;
    function GetVariables: TDBVariables;
    { BCP functions }
    function bcp_batch(const Proc: PDBPROCESS): DBINT;
    function bcp_bind(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
      VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer): RETCODE;
    function bcp_colfmt(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
      FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte; FileTermLen,
      TableColumn: Integer): RETCODE;
    function bcp_collen(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer): RETCODE;
    function bcp_colptr(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer): RETCODE;
    function bcp_columns(Proc: PDBPROCESS; FileColCount: Integer): RETCODE;
    function bcp_control(Proc: PDBPROCESS; Field: Integer; Value: DBINT): RETCODE;
    function bcp_done(Proc: PDBPROCESS): DBINT;
    function bcp_exec(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE;
    function bcp_init(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
      Direction: Integer): RETCODE;
    function bcp_moretext(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE;
    function bcp_readfmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
    function bcp_sendrow(Proc: PDBPROCESS): RETCODE;
    function bcp_setl(Login: PLOGINREC; Enable: LongBool): RETCODE;
    function bcp_writefmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;

    function GetDBLibraryVendorType: TDBLibraryVendorType;
  end;

  TZDBLibAbstractPlainDriver = class(TZAbstractPlainDriver, IZPlainDriver)
  private
    FDBLibraryVendorType: TDBLibraryVendorType;
{$IFDEF MSWINDOWS}
    Fdbadata_stdcall: function(dbProc: PDBPROCESS; ComputeId, Column: Integer): PByte; stdcall;
    Fdbadlen_stdcall: function(dbProc: PDBPROCESS; ComputeId, Column: Integer): DBINT; stdcall;
    Fdbaltbind_stdcall: function(dbProc: PDBPROCESS;
      ComputeId, Column, VarType: Integer; VarLen: DBINT; VarAddr: PByte): RETCODE; stdcall;
    Fdbaltbind_ps_stdcall: function(dbProc: PDBPROCESS;
      ComputeId, Column: Integer; VarType: Integer; VarLen: DBINT;
      VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; stdcall;
    Fdbaltcolid_stdcall: function(dbProc: PDBPROCESS;
      ComputeId, Column: Integer): Integer; stdcall;
    Fdbaltlen_stdcall: function(dbProc: PDBPROCESS; ComputeId, Column: Integer): DBINT; stdcall;
    Fdbaltop_stdcall: function(dbProc: PDBPROCESS; ComputeId, Column: Integer): Integer; stdcall;
    Fdbalttype_stdcall: function(dbProc: PDBPROCESS; ComputeId, Column: Integer): Integer; stdcall;
    // Fdbaltutype_stdcall: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; stdcall;
    Fdbanullbind_stdcall: function(dbProc: PDBPROCESS;
      ComputeId, Column: Integer; Indicator: PDBINT): RETCODE; stdcall;
    Fdbbind_stdcall: function(dbProc: PDBPROCESS;
      Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE; stdcall;
    Fdbbind_ps_stdcall: function(dbProc: PDBPROCESS;
      Column, VarType, VarLen: Integer; VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; stdcall;
    Fdbbufsize_stdcall: function(dbProc: PDBPROCESS): Integer; stdcall;
    Fdbbylist_stdcall: function(dbProc: PDBPROCESS; ComputeId: Integer;
      Size: PInteger): PByte; stdcall;
    Fdbcancel_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    Fdbcanquery_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    // Fdbchange_stdcall: function(dbroc: PDBPROCESS): PAnsiChar; stdcall;
    Fdbclose_stdcall: procedure(dbProc: PDBPROCESS); stdcall;
    Fdbclose_MS: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    Fdbclrbuf_stdcall: procedure(dbProc: PDBPROCESS; N: DBINT); stdcall;
    Fdbclropt_stdcall: function(dbProc: PDBPROCESS; Option: Integer;
      Param: PAnsiChar): RETCODE; stdcall;
    Fdbcmd_stdcall: function(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; stdcall;
    Fdbcmdrow_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    // Fdbcolbrowse_stdcall: function(dbproc: PDBPROCESS; Column: Integer): DBBOOL; stdcall;
    // Fdbcolbrowse_MS: function(dbproc: PDBPROCESS; Column: Integer): LongBool; cdecl;
    Fdbcollen_stdcall: function(dbProc: PDBPROCESS; Column: Integer): DBINT; stdcall;
    Fdbcolname_stdcall: function(dbProc: PDBPROCESS; Column: Integer): PAnsiChar; stdcall;
    Fdbcolsource_stdcall: function(dbProc: PDBPROCESS; Column: Integer): PAnsiChar; stdcall;
    Fdbcoltype_stdcall: function(dbProc: PDBPROCESS; Column: Integer): Integer; stdcall;
    Fdbcoltypeinfo_stdcall: function(dbProc: PDBPROCESS; Column: Integer): PDBTYPEINFO; stdcall;
    Fdbcolutype_stdcall: function(dbProc: PDBPROCESS; Column: Integer): DBINT; stdcall;
    Fdbconvert_stdcall: function(dbProc: PDBPROCESS; SrcType: Integer;
      Src: PByte; SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT): Integer; stdcall;
    Fdbconvert_ps_stdcall: function(dbProc: PDBPROCESS; SrcType: Integer;
      Src: PByte; SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT;
      typinfo: PDBTYPEINFO): Integer; stdcall;
    Fdbcount_stdcall: function(dbProc: PDBPROCESS): DBINT; stdcall;
    // Fdbcurcmd_stdcall: function(dbproc: PDBPROCESS): DBINT; stdcall;
    // Fdbcurcmd_MS: function(dbproc: PDBPROCESS): Integer; cdecl;
    Fdbcurrow_stdcall: function(dbProc: PDBPROCESS): DBINT; stdcall;
    Fdbdata_stdcall: function(dbProc: PDBPROCESS; Column: Integer): PByte; stdcall;
    // Fdbdatecrack_stdcall: function(Proc: PDBPROCESS; DateInfo: PDBDATEREC; DateType: PDBDATETIME): RETCODE; cdecl;
    Fdbdatlen_stdcall: function(dbProc: PDBPROCESS; Column: Integer): DBINT; stdcall;
    Fdbdead_stdcall: function(dbProc: PDBPROCESS): DBBOOL; stdcall;
    Fdbdead_MS: function(dbroc: PDBPROCESS): LongBool; cdecl;
    Fdbexit_stdcall: procedure; stdcall;
    FdbHasRetStat_stdcall: function(dbProc: PDBPROCESS): DBBOOL; stdcall;
    FdbHasRetStat_MS: function(dbProc: PDBPROCESS): LongBool; cdecl;
    Fdbfirstrow_stdcall: function(Proc: PDBPROCESS): DBINT; stdcall;
    Fdbfreebuf_stdcall: procedure(Proc: PDBPROCESS); stdcall;
    Fdbfreequal_stdcall: procedure(Ptr: PAnsiChar); stdcall;
    Fdbgetchar_stdcall: function(Proc: PDBPROCESS; N: Integer): PAnsiChar; stdcall;
    Fdbgetcharset_stdcall: function(dbProc: PDBPROCESS): PAnsiChar; stdcall;
    FdbGetRow_stdcall: function(dbProc: PDBPROCESS; Row: DBINT): STATUS; stdcall;
    FdbInit_MS: function: PAnsiChar; cdecl; // returns the librayversion
    FdbInit_stdcall: function: RETCODE; stdcall;
    // SYBASE returns SUCCEED or FAIL
    FdbLogin_stdcall: function: PLOGINREC; stdcall;
    Fdbloginfree_stdcall: procedure(loginptr: PLOGINREC); stdcall;
    FdbMoreCmds_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    FdbName_stdcall: function(dbProc: PDBPROCESS): PAnsiChar; stdcall;
    FdbNextRow_stdcall: function(dbProc: PDBPROCESS): STATUS; stdcall;
    FdbNumCols_stdcall: function(dbProc: PDBPROCESS): DBINT; stdcall;
    FdbOpen_stdcall: function(Login: PLOGINREC; server: PAnsiChar): PDBPROCESS; stdcall;
    FdbResults_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    FdbRetData_stdcall: function(dbProc: PDBPROCESS; RetNum: Integer): Pointer; stdcall;
    FdbRetStatus_stdcall: function(dbProc: PDBPROCESS): DBINT; stdcall;
    FdbRetType_stdcall: function(dbProc: PDBPROCESS; RetNum: DBINT): DBINT; stdcall;
    FdbRpcInit_stdcall: function(dbProc: PDBPROCESS; RpcName: PAnsiChar;
      Options: DBSMALLINT): RETCODE; stdcall;
    FdbRpcParam_stdcall: function(dbProc: PDBPROCESS; ParamName: PAnsiChar;
      STATUS: Byte; Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE; stdcall;
    FdbRpcSend_stdcall: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbRetLen_stdcall: function(dbProc: PDBPROCESS; RetNum: Integer): DBINT; stdcall;
    FdbRetName_stdcall: function(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar; stdcall;
    FdbSqlExec_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    FdbSqlOk_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    Fdbsqlsend_stdcall: function(dbProc: PDBPROCESS): RETCODE; stdcall;
    FdbSetLName_stdcall: function(Login: PLOGINREC; Value: PAnsiChar;
      Item: Integer): RETCODE; stdcall;
    FdbSetLoginTime_stdcall: function(Seconds: Integer): RETCODE; stdcall;
    FdbSetOpt_MS: function(dbProc: PDBPROCESS; Option: Integer;
      Char_Param: PAnsiChar): RETCODE; cdecl;
    FdbSetOpt_stdcall: function(dbProc: PDBPROCESS; Option: Integer;
      Char_Param: PAnsiChar; Int_Param: Integer): RETCODE; stdcall;
    FdbSetTime_stdcall: function(Seconds: Integer): RETCODE; stdcall;
    FdbUse_stdcall: function(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE; stdcall;
    Fdbvarylen_MS: function(Proc: PDBPROCESS; Column: Integer): LongBool; cdecl;
    Fdbvarylen_stdcall: function(Proc: PDBPROCESS; Column: Integer): DBBOOL; stdcall;
    Fdberrhandle_stdcall: function(Handler: SYBDBERRHANDLE_PROC): SYBDBERRHANDLE_PROC; stdcall;
    Fdbmsghandle_stdcall: function(Handler: SYBDBMSGHANDLE_PROC): SYBDBMSGHANDLE_PROC; stdcall;
    FdbSetVersion_stdcall: function(Version: DBINT): RETCODE; stdcall;
    FdbSetMaxprocs_stdcall: function(MaxProcs: DBINT): RETCODE; stdcall;
    // sybase has widened the type!
    // Fdb12hour_stdcall:          function(dbproc: PDBPROCESS; Language: PAnsiChar): DBBOOL; stdcall; //no MS
    Fdbdataready: function(Proc: PDBPROCESS): LongBool; cdecl; // MS only
    { BCP functions }
    Fbcp_batch_stdcall: function(const Proc: PDBPROCESS): DBINT; stdcall;
    Fbcp_bind_stdcall: function(Proc: PDBPROCESS; VarAddr: PByte;
      PrefixLen: Integer; VarLen: DBINT; Terminator: PByte;
      TermLen, Typ, TableColumn: Integer): RETCODE; stdcall;
    Fbcp_colfmt_stdcall: function(Proc: PDBPROCESS; FileColumn: Integer;
      FileType: Byte; FilePrefixLen: Integer; FileColLen: DBINT;
      FileTerm: PByte; FileTermLen, TableColumn: Integer): RETCODE; stdcall;
    Fbcp_collen_stdcall: function(Proc: PDBPROCESS; VarLen: DBINT;
      TableColumn: Integer): RETCODE; stdcall;
    Fbcp_colptr_stdcall: function(Proc: PDBPROCESS; ColPtr: PByte;
      TableColumn: Integer): RETCODE; stdcall;
    Fbcp_columns_stdcall: function(Proc: PDBPROCESS; FileColCount: Integer): RETCODE; stdcall;
    Fbcp_control_stdcall: function(Proc: PDBPROCESS; Field: Integer;
      Value: DBINT): RETCODE; stdcall;
    Fbcp_done_stdcall: function(Proc: PDBPROCESS): DBINT; stdcall;
    Fbcp_exec_stdcall: function(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE; stdcall;
    Fbcp_init_stdcall: function(Proc: PDBPROCESS;
      TableName, hFile, ErrFile: PAnsiChar; Direction: Integer): RETCODE; stdcall;
    Fbcp_moretext_stdcall: function(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE; stdcall;
    Fbcp_readfmt_stdcall: function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; stdcall;
    Fbcp_sendrow_stdcall: function(Proc: PDBPROCESS): RETCODE; stdcall;
    Fbcp_setl_stdcall: function(Login: PLOGINREC; Enable: LongBool): RETCODE; stdcall;
    Fbcp_writefmt_stdcall: function(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE; stdcall;
    FdbRpcExec: function(dbProc: PDBPROCESS): RETCODE; cdecl; // MS only
    // MS only others use dbsetlname
    Fdbsetlpacket: function(Login: PLOGINREC; PacketSize: Word): RETCODE; cdecl;
    FdbSetMaxprocs_MS: function(MaxProcs: SmallInt): RETCODE; cdecl;
    fdbWinexit: procedure; cdecl; // MS only
{$ENDIF}
    Fdbadata: function(dbProc: PDBPROCESS; ComputeId, Column: Integer): PByte; cdecl;
    Fdbadlen: function(dbProc: PDBPROCESS; ComputeId, Column: Integer)
      : DBINT; cdecl;
    Fdbaltbind: function(dbProc: PDBPROCESS;
      ComputeId, Column, VarType: Integer; VarLen: DBINT; VarAddr: PByte)
      : RETCODE; cdecl;
    Fdbaltbind_ps: function(dbProc: PDBPROCESS; ComputeId, Column: Integer;
      VarType: Integer; VarLen: DBINT; VarAddr: PByte; typinfo: PDBTYPEINFO)
      : RETCODE; cdecl;
    Fdbaltcolid: function(dbProc: PDBPROCESS; ComputeId, Column: Integer)
      : Integer; cdecl;
    Fdbaltlen: function(dbProc: PDBPROCESS; ComputeId, Column: Integer)
      : DBINT; cdecl;
    Fdbaltop: function(dbProc: PDBPROCESS; ComputeId, Column: Integer)
      : Integer; cdecl;
    Fdbalttype: function(dbProc: PDBPROCESS; ComputeId, Column: Integer)
      : Integer; cdecl;
    // Fdbaltutype: function(dbproc: PDBPROCESS; ComputeId, Column: Integer): DBINT; cdecl;
    Fdbanullbind: function(dbProc: PDBPROCESS; ComputeId, Column: Integer;
      Indicator: PDBINT): RETCODE; cdecl;
    Fdbbind: function(dbProc: PDBPROCESS; Column, VarType, VarLen: Integer;
      VarAddr: PByte): RETCODE; cdecl;
    Fdbbind_ps: function(dbProc: PDBPROCESS; Column, VarType, VarLen: Integer;
      VarAddr: PByte; typinfo: PDBTYPEINFO): RETCODE; cdecl;
    Fdbbufsize: function(dbProc: PDBPROCESS): Integer; cdecl;
    Fdbbylist: function(dbProc: PDBPROCESS; ComputeId: Integer; Size: PInteger)
      : PByte; cdecl;
    Fdbcancel: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    Fdbcanquery: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    // Fdbchange: function(dbproc: PDBPROCESS): PAnsiChar; cdecl;
    // dbcharsetconv
    { EH: Attention -> call convention and ms version returns a RETCODE }
    Fdbclose_SYB: procedure(dbProc: PDBPROCESS); cdecl;
    Fdbclrbuf: procedure(dbProc: PDBPROCESS; N: DBINT); cdecl;
    Fdbclropt: function(dbProc: PDBPROCESS; Option: Integer; Param: PAnsiChar)
      : RETCODE; cdecl;
    Fdbcmd: function(dbProc: PDBPROCESS; Cmd: PAnsiChar): RETCODE; cdecl;
    Fdbcmdrow: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    // result type(MS vs Syb) size is different
    Fdbcolinfo: function(pdbhandle: pdbhandle; _Type: Integer; Column: DBINT;
      ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE; cdecl; // no SYB but FreeTDS
    // Fdbcolbrowse_SYB: function(dbproc: PDBPROCESS; Column: Integer): DBBOOL; cdecl; //no FreeTDS?
    Fdbcollen: function(dbProc: PDBPROCESS; Column: Integer): DBINT; cdecl;
    Fdbcolname: function(dbProc: PDBPROCESS; Column: Integer): PAnsiChar; cdecl;
    Fdbcolsource: function(dbProc: PDBPROCESS; Column: Integer): PAnsiChar;
      cdecl; // no FreeTDS?
    Fdbcoltype: function(dbProc: PDBPROCESS; Column: Integer): Integer; cdecl;
    Fdbcoltypeinfo: function(dbProc: PDBPROCESS; Column: Integer): PDBTYPEINFO;
      cdecl; // no MS
    Fdbcolutype: function(dbProc: PDBPROCESS; Column: Integer): DBINT; cdecl;
    Fdbconvert: function(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT)
      : Integer; cdecl;
    Fdbconvert_ps: function(dbProc: PDBPROCESS; SrcType: Integer; Src: PByte;
      SrcLen: DBINT; DestType: Integer; Dest: PByte; DestLen: DBINT;
      typinfo: PDBTYPEINFO): Integer; cdecl; // NO MS
    Fdbcount: function(dbProc: PDBPROCESS): DBINT; cdecl;
    // Fdbcurcmd_SYB: function(dbproc: PDBPROCESS): DBINT; cdecl;
    Fdbcurrow: function(dbProc: PDBPROCESS): DBINT; cdecl;
    Fdbdata: function(dbProc: PDBPROCESS; Column: Integer): PByte; cdecl;
    // Fdbdatecrack: function(Proc: PDBPROCESS; DateInfo: PDBDATEREC; DateType: PDBDATETIME): RETCODE; cdecl;
    // Fdbdatecrack_TDS: function(dbproc: PDBPROCESS; DateInfo: PTDS_DBDATEREC; DateType: PTDSDBDATETIME): RETCODE; cdecl;
    Fdbdatlen: function(dbProc: PDBPROCESS; Column: Integer): DBINT; cdecl;

    Fdbdead: function(dbroc: PDBPROCESS): DBBOOL; cdecl;
    Fdbexit: procedure; cdecl;
    FdbHasRetStat_SYB: function(dbProc: PDBPROCESS): DBBOOL; cdecl;
    { Fdbfcmd: function(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; cdecl;
      Fdbfcmd_stdcall: function(Proc: PDBPROCESS; CmdString: PAnsiChar; var Params): RETCODE; stdcall; }
    Fdbfirstrow: function(Proc: PDBPROCESS): DBINT; cdecl;
    Fdbfreebuf: procedure(Proc: PDBPROCESS); cdecl;
    Fdbfreequal: procedure(Ptr: PAnsiChar); cdecl;
    Fdbgetchar: function(Proc: PDBPROCESS; N: Integer): PAnsiChar; cdecl;
    Fdbgetcharset: function(dbProc: PDBPROCESS): PAnsiChar; cdecl; // NO MS
    FdbGetRow: function(dbProc: PDBPROCESS; Row: DBINT): STATUS; cdecl;
    FdbInit: function: RETCODE; cdecl;
    Fdbloginfree: procedure(loginptr: PLOGINREC); cdecl;
    FdbMoreCmds: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbName: function(dbProc: PDBPROCESS): PAnsiChar; cdecl;
    FdbNextRow: function(dbProc: PDBPROCESS): STATUS; cdecl;
    FdbNumCols: function(dbProc: PDBPROCESS): DBINT; cdecl;
    FdbOpen: function(Login: PLOGINREC; server: PAnsiChar): PDBPROCESS; cdecl;
    FtdsDbOpen: function(Login: PLOGINREC; server: PAnsiChar; msdblib: Integer)
      : PDBPROCESS; cdecl;

    FdbResults: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbRetData: function(dbProc: PDBPROCESS; RetNum: Integer): Pointer; cdecl;
    FdbRetStatus: function(dbProc: PDBPROCESS): DBINT; cdecl;
    FdbRetType: function(dbProc: PDBPROCESS; RetNum: DBINT): DBINT; cdecl;
    { rpc i.e remote procedure calls }
    FdbRpcInit: function(dbProc: PDBPROCESS; RpcName: PAnsiChar;
      Options: DBSMALLINT): RETCODE; cdecl;
    FdbRpcParam: function(dbProc: PDBPROCESS; ParamName: PAnsiChar;
      STATUS: Byte; Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer)
      : RETCODE; cdecl;
    FdbRpcSend: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbRetLen: function(dbProc: PDBPROCESS; RetNum: Integer): DBINT; cdecl;
    FdbRetName: function(dbProc: PDBPROCESS; RetNum: Integer): PAnsiChar; cdecl;
    FdbSqlExec: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbSqlOk: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    Fdbsqlsend: function(dbProc: PDBPROCESS): RETCODE; cdecl;
    FdbSetLName: function(Login: PLOGINREC; Value: PAnsiChar; Item: Integer)
      : RETCODE; cdecl;
    FdbSetLoginTime: function(Seconds: Integer): RETCODE; cdecl;
    FdbSetOpt_SYB: function(dbProc: PDBPROCESS; Option: Integer;
      Char_Param: PAnsiChar; Int_Param: Integer): RETCODE; cdecl;
    FdbSetMaxprocs_SYB: function(MaxProcs: DBINT): RETCODE; cdecl;
    FdbSetTime: function(Seconds: Integer): RETCODE; cdecl;
    FdbUse: function(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE; cdecl;
    Fdbvarylen_SYB: function(Proc: PDBPROCESS; Column: Integer): DBBOOL; cdecl;
    // Fdb12hour: function(dbproc: PDBPROCESS; Language: PAnsiChar): DBBOOL; cdecl; //no MS
    Fdberrhandle: function(Handler: DBERRHANDLE_PROC): DBERRHANDLE_PROC; cdecl;
    Fdbmsghandle: function(Handler: DBMSGHANDLE_PROC): DBMSGHANDLE_PROC; cdecl;
    // Fdbtds: function(dbproc: PDBPROCESS): DBINT; cdecl;
    { BCP functions }
    Fbcp_batch: function(const Proc: PDBPROCESS): DBINT; cdecl;
    Fbcp_bind: function(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
      VarLen: DBINT; Terminator: PByte; TermLen, Typ, TableColumn: Integer)
      : RETCODE; cdecl;
    Fbcp_colfmt: function(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
      FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte;
      FileTermLen, TableColumn: Integer): RETCODE; cdecl;
    Fbcp_collen: function(Proc: PDBPROCESS; VarLen: DBINT; TableColumn: Integer)
      : RETCODE; cdecl;
    Fbcp_colptr: function(Proc: PDBPROCESS; ColPtr: PByte; TableColumn: Integer)
      : RETCODE; cdecl;
    Fbcp_columns: function(Proc: PDBPROCESS; FileColCount: Integer)
      : RETCODE; cdecl;
    Fbcp_control: function(Proc: PDBPROCESS; Field: Integer; Value: DBINT)
      : RETCODE; cdecl;
    Fbcp_done: function(Proc: PDBPROCESS): DBINT; cdecl;
    Fbcp_exec: function(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE; cdecl;
    Fbcp_init: function(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
      Direction: Integer): RETCODE; cdecl;
    Fbcp_moretext: function(Proc: PDBPROCESS; Size: DBINT; Text: PByte)
      : RETCODE; cdecl;
    Fbcp_readfmt: function(Proc: PDBPROCESS; FileName: PAnsiChar)
      : RETCODE; cdecl;
    Fbcp_sendrow: function(Proc: PDBPROCESS): RETCODE; cdecl;
    Fbcp_setl: function(Login: PLOGINREC; Enable: LongBool): RETCODE; cdecl;
    Fbcp_writefmt: function(Proc: PDBPROCESS; FileName: PAnsiChar)
      : RETCODE; cdecl;
  private // FreeTDS only
    FdbLogin: function: PLOGINREC; cdecl;
    Fdbsetlversion: function(Login: PLOGINREC; Version: Byte): RETCODE; cdecl;
    // just FreeTDS
    FdbSetVersion: function(Version: DBINT): RETCODE; cdecl;
    Ftdsdump_on: procedure; cdecl;
    Ftdsdump_off: procedure; cdecl;
    Ftdsdump_open: function(FileName: PAnsiChar): Integer; cdecl;
    Ftdsdump_close: procedure; cdecl;
  protected
    DBVariables: TDBVariables;
    procedure LoadApi; override;
    procedure LoadCodePages; override;
  public //
    function dbIntit: RETCODE;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure CheckError(dbProc: PDBPROCESS);
    function GetErrorString(dbProc: PDBPROCESS): String;
    function GetDBLibraryVendorType: TDBLibraryVendorType;
  public //
    function dbDead(dbProc: PDBPROCESS): Boolean;
    function dbLogin: PLOGINREC; virtual;
    procedure dbLoginFree(Login: PLOGINREC);
    function dbSetLoginTime(Seconds: DBINT): RETCODE;
    function dbsetLName(Login: PLOGINREC; Value: PAnsiChar;
      Item: DBINT): RETCODE;
    function dbSetLHost(Login: PLOGINREC; HostName: PAnsiChar): RETCODE;
    function dbSetLUser(Login: PLOGINREC; UserName: PAnsiChar): RETCODE;
    function dbSetLPwd(Login: PLOGINREC; Password: PAnsiChar): RETCODE;
    function dbSetLApp(Login: PLOGINREC; AppName: PAnsiChar): RETCODE;
    function dbSetLNatLang(Login: PLOGINREC; NatLangName: PAnsiChar): RETCODE;
    function dbSetLCharSet(Login: PLOGINREC; CharsetName: PAnsiChar): RETCODE;
    function dbSetLSecure(Login: PLOGINREC): RETCODE;
    function dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
    function dbSetVersion(Version: DBINT): RETCODE;
    function dbSetTime(Seconds: Integer): RETCODE;
    function dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
    function dbCancel(dbProc: PDBPROCESS): RETCODE;
    function dbCmd(const dbProc: PDBPROCESS; const Cmd: PAnsiChar): RETCODE;
    function dbSqlExec(dbProc: PDBPROCESS; Async: Boolean = False): RETCODE;
    function dbSqlExecSync(dbProc: PDBPROCESS): RETCODE;
    function dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
    function dbResults(dbProc: PDBPROCESS): RETCODE;
    function dbCanQuery(dbProc: PDBPROCESS): RETCODE;
    function dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
    function dbUse(dbProc: PDBPROCESS; dbName: PAnsiChar): RETCODE;
    function dbSetOpt(dbProc: PDBPROCESS; Option: DBINT;
      Char_Param: PAnsiChar = nil; Int_Param: DBINT = -1): RETCODE;
    function dbClose(dbProc: PDBPROCESS): RETCODE;
    function dbName(dbProc: PDBPROCESS): PAnsiChar;
    function dbCmdRow(dbProc: PDBPROCESS): RETCODE;
    function dbNumCols(dbProc: PDBPROCESS): DBINT;
    function dbColName(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColSource(dbProc: PDBPROCESS; Column: DBINT): PAnsiChar;
    function dbColType(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbcoltypeinfo(Proc: PDBPROCESS; Column: Integer): PDBTYPEINFO;
    function dbColLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbcolinfo(pdbhandle: pdbhandle; _Type: Integer; Column: DBINT;
      ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE;
    function dbData(dbProc: PDBPROCESS; Column: DBINT): PByte;
    function dbDatLen(dbProc: PDBPROCESS; Column: DBINT): DBINT;
    function dbConvert(dbProc: PDBPROCESS; SrcType: DBINT; Src: PByte;
      SrcLen: DBINT; DestType: DBINT; Dest: PByte; DestLen: DBINT): DBINT;
    function dbNextRow(dbProc: PDBPROCESS): STATUS;
    function dbGetRow(dbProc: PDBPROCESS; Row: DBINT): STATUS;
    function dbCount(dbProc: PDBPROCESS): DBINT;
    function dbbind(Proc: PDBPROCESS; Column, VarType, VarLen: Integer;
      VarAddr: PByte): RETCODE;

    function dbRpcInit(dbProc: PDBPROCESS; RpcName: PAnsiChar;
      Options: SmallInt): RETCODE;
    function dbRpcParam(dbProc: PDBPROCESS; ParamName: PAnsiChar; STATUS: Byte;
      Type_: DBINT; MaxLen: DBINT; DataLen: DBINT; Value: Pointer): RETCODE;
    function dbRpcSend(dbProc: PDBPROCESS): RETCODE;
    function dbRpcExec(dbProc: PDBPROCESS): RETCODE;
    function dbRetStatus(dbProc: PDBPROCESS): DBINT;
    function dbHasRetStat(dbProc: PDBPROCESS): Boolean;
    function dbRetName(dbProc: PDBPROCESS; RetNum: DBINT): PAnsiChar;
    function dbRetData(dbProc: PDBPROCESS; RetNum: DBINT): Pointer;
    function dbRetLen(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbRetType(dbProc: PDBPROCESS; RetNum: DBINT): DBINT;
    function dbdataready(Proc: PDBPROCESS): LongBool;
    function GetVariables: TDBVariables;
    { BCP functions }
    function bcp_batch(const Proc: PDBPROCESS): DBINT;
    function bcp_bind(Proc: PDBPROCESS; VarAddr: PByte; PrefixLen: Integer;
      VarLen: DBINT; Terminator: PByte;
      TermLen, Typ, TableColumn: Integer): RETCODE;
    function bcp_colfmt(Proc: PDBPROCESS; FileColumn: Integer; FileType: Byte;
      FilePrefixLen: Integer; FileColLen: DBINT; FileTerm: PByte;
      FileTermLen, TableColumn: Integer): RETCODE;
    function bcp_collen(Proc: PDBPROCESS; VarLen: DBINT;
      TableColumn: Integer): RETCODE;
    function bcp_colptr(Proc: PDBPROCESS; ColPtr: PByte;
      TableColumn: Integer): RETCODE;
    function bcp_columns(Proc: PDBPROCESS; FileColCount: Integer): RETCODE;
    function bcp_control(Proc: PDBPROCESS; Field: Integer;
      Value: DBINT): RETCODE;
    function bcp_done(Proc: PDBPROCESS): DBINT;
    function bcp_exec(Proc: PDBPROCESS; RowsCopied: PDBINT): RETCODE;
    function bcp_init(Proc: PDBPROCESS; TableName, hFile, ErrFile: PAnsiChar;
      Direction: Integer): RETCODE;
    function bcp_moretext(Proc: PDBPROCESS; Size: DBINT; Text: PByte): RETCODE;
    function bcp_readfmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
    function bcp_sendrow(Proc: PDBPROCESS): RETCODE;
    function bcp_setl(Login: PLOGINREC; Enable: LongBool): RETCODE;
    function bcp_writefmt(Proc: PDBPROCESS; FileName: PAnsiChar): RETCODE;
  end;

  TZDbLibBasePlainDriver = class(TZDBLibAbstractPlainDriver, IZPlainDriver,
    IZDBLibPlainDriver)
  end;

  { ** Implements a dblib driver for Sybase ASE 12.5 }
  TZDBLibSybaseASE125PlainDriver = class(TZDBLibAbstractPlainDriver,
    IZPlainDriver, IZDBLibPlainDriver)
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadCodePages; override;
  public
    constructor Create; override;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { ** Implements a dblib driver for MSSql7 }
  TZDBLibMSSQL7PlainDriver = class(TZDbLibBasePlainDriver, IZPlainDriver,
    IZDBLibPlainDriver)
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadCodePages; override;
  public
    constructor Create; override;

    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  { ** Implements a generic dblib driver }
  IZFreeTDSPlainDriver = interface(IZDBLibPlainDriver)
    ['{12FA5A22-59E5-4CBF-B745-96A7CDF9FBE0}']
    procedure tdsDumpOn;
    procedure tdsDumpOff;
    procedure tdsDump_Open(const FileName: String);
    procedure tdsDump_Close;
  end;

  { ** Implements a dblib driver for Sybase/MSSQL }
  TZFreeTDSBasePlainDriver = class(TZDbLibBasePlainDriver, IZDBLibPlainDriver,
    IZFreeTDSPlainDriver)
  protected
    function Clone: IZPlainDriver; override; abstract;
  public
    constructor Create; override;

    function GetProtocol: string; override;
    function GetDescription: string; override;

    { API functions }
    function dbsetlversion(Login: PLOGINREC): RETCODE; virtual;
    function dbsetversion(Version: DBINT): RETCODE; overload;
    function dbsetversion: RETCODE; overload; virtual;

    function dbLogin: PLOGINREC; override;

    procedure tdsDumpOn;
    procedure tdsDumpOff;
    procedure tdsDump_Open(const FileName: String);
    procedure tdsDump_Close;
  end;

  TZFreeTDS42MsSQLPlainDriver = class(TZFreeTDSBasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
    procedure LoadCodePages; override;
  public
    constructor Create; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion(Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS42SybasePlainDriver = class(TZFreeTDSBasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion( { %H- } Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS50PlainDriver = class(TZFreeTDS42SybasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion( { %H- } Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS70PlainDriver = class(TZFreeTDS42MsSQLPlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetlversion(Login: PLOGINREC): RETCODE; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS71PlainDriver = class(TZFreeTDS70PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetversion: RETCODE; override;
  end;

  TZFreeTDS72PlainDriver = class(TZFreeTDS70PlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    function GetProtocol: string; override;
    function GetDescription: string; override;
    function dbsetversion: RETCODE; override;
  end;

var
  OldFreeTDSErrorHandle: DBERRHANDLE_PROC = nil;
  OldFreeTDSMessageHandle: DBMSGHANDLE_PROC = nil;
  OldSybaseErrorHandle: SYBDBERRHANDLE_PROC = nil;
  OldSybaseMessageHandle: SYBDBMSGHANDLE_PROC = nil;
  OldMsSQLMessageHandle: DBMSGHANDLE_PROC = nil;
  OldMsSQLErrorHandle: DBERRHANDLE_PROC = nil;
  ErrorCS: TCriticalSection;
  SQLErrors: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
  SQLMessages: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};

{$ENDIF ZEOS_DISABLE_DBLIB}

implementation

{$IFNDEF ZEOS_DISABLE_DBLIB}

uses SysUtils, ZPlainLoader, ZEncoding, ZClasses, ZFastCode;

procedure AddSybaseCodePages(PlainDriver: TZAbstractPlainDriver);
begin
  // codepages as found in "SAP Adaptive Server Enterprise 16.0 > Configuration Guide for UNIX Adaptive Server Enterprise 16.0 > Localization Support"
  PlainDriver.AddCodePage('ascii_8', 1, ceAnsi, zCP_us_ascii);
  PlainDriver.AddCodePage('big5', 2, ceAnsi, zCP_Big5);
  PlainDriver.AddCodePage('cp437', 3, ceAnsi, zCP_DOS437);
  PlainDriver.AddCodePage('cp850', 4, ceAnsi, zCP_DOS850);
  PlainDriver.AddCodePage('cp852', 5, ceAnsi, zCP_DOS852);
  PlainDriver.AddCodePage('cp855', 6, ceAnsi, zCP_DOS855);
  PlainDriver.AddCodePage('cp857', 7, ceAnsi, zCP_DOS857);
  PlainDriver.AddCodePage('cp858', 8, ceAnsi, zCP_DOS858);
  PlainDriver.AddCodePage('cp860', 9, ceAnsi, zCP_DOS860);
  PlainDriver.AddCodePage('cp864', 10, ceAnsi, zCP_DOS864);
  PlainDriver.AddCodePage('cp866', 11, ceAnsi, zCP_DOS866);
  PlainDriver.AddCodePage('cp869', 12, ceAnsi, zCP_DOS869);
  PlainDriver.AddCodePage('cp874', 13, ceAnsi, zCP_WIN874);
  PlainDriver.AddCodePage('cp932', 14, ceAnsi, zCP_SHIFTJS);
  PlainDriver.AddCodePage('cp936', 15, ceAnsi, zCP_GB2312);
  PlainDriver.AddCodePage('cp950', 16, ceAnsi, zCP_Big5);
  PlainDriver.AddCodePage('cp1250', 17, ceAnsi, zCP_WIN1250);
  PlainDriver.AddCodePage('cp1251', 18, ceAnsi, zCP_WIN1251);
  PlainDriver.AddCodePage('cp1252', 19, ceAnsi, zCP_WIN1252);
  PlainDriver.AddCodePage('cp1253', 20, ceAnsi, zCP_WIN1253);
  PlainDriver.AddCodePage('cp1254', 21, ceAnsi, zCP_WIN1254);
  PlainDriver.AddCodePage('cp1255', 22, ceAnsi, zCP_WIN1255);
  PlainDriver.AddCodePage('cp1256', 23, ceAnsi, zCP_WIN1256);
  PlainDriver.AddCodePage('cp1257', 24, ceAnsi, zCP_WIN1257);
  PlainDriver.AddCodePage('cp1258', 25, ceAnsi, zCP_WIN1258);
  PlainDriver.AddCodePage('gb18030', 26, ceAnsi, zCP_GB18030);
  PlainDriver.AddCodePage('iso_1', 27, ceAnsi, zCP_L1_ISO_8859_1);
  PlainDriver.AddCodePage('iso88592', 28, ceAnsi, zCP_L2_ISO_8859_2);
  PlainDriver.AddCodePage('iso88595', 29, ceAnsi, zCP_L5_ISO_8859_5);
  PlainDriver.AddCodePage('iso88596', 30, ceAnsi, zCP_L6_ISO_8859_6);
  PlainDriver.AddCodePage('iso88597', 31, ceAnsi, zCP_L7_ISO_8859_7);
  PlainDriver.AddCodePage('iso88598', 32, ceAnsi, zCP_L8_ISO_8859_8);
  PlainDriver.AddCodePage('iso88599', 33, ceAnsi, zCP_L5_ISO_8859_9);
  PlainDriver.AddCodePage('iso15', 34, ceAnsi, zCP_L9_ISO_8859_15);
  PlainDriver.AddCodePage('sjis', 35, ceAnsi, zCP_SHIFTJS);
  PlainDriver.AddCodePage('utf8', 36, ceUTF8, zCP_UTF8);
end;

procedure AddmMSCodePages(PlainDriver: TZAbstractPlainDriver);
begin
  { SingleByte }
  PlainDriver.AddCodePage('WINDOWS-1250', 1, ceAnsi, zCP_WIN1250, '', 1, False);
  { Microsoft Windows Codepage 1250 (East European) }
  PlainDriver.AddCodePage('WINDOWS-1251', 2, ceAnsi, zCP_WIN1251, '', 1, False);
  { Microsoft Windows Codepage 1251 (Cyrl) }
  PlainDriver.AddCodePage('WINDOWS-1252', 3, ceAnsi, zCP_WIN1252, '', 1, False);
  { Microsoft Windows Codepage 1252 (ANSI), USASCCI }
  PlainDriver.AddCodePage('WINDOWS-1253', 4, ceAnsi, zCP_WIN1253, '', 1, False);
  { Microsoft Windows Codepage 1253 (Greek) }
  PlainDriver.AddCodePage('WINDOWS-1254', 5, ceAnsi, zCP_WIN1254, '', 1, False);
  { Microsoft Windows Codepage 1254 (Turk) }
  PlainDriver.AddCodePage('WINDOWS-1255', 6, ceAnsi, zCP_WIN1255, '', 1, False);
  { Microsoft Windows Codepage 1255 (Hebrew) }
  PlainDriver.AddCodePage('WINDOWS-1256', 7, ceAnsi, zCP_WIN1256, '', 1, False);
  { Microsoft Windows Codepage 1256 (Arab) }
  PlainDriver.AddCodePage('WINDOWS-1257', 8, ceAnsi, zCP_WIN1257, '', 1, False);
  { Microsoft Windows Codepage 1257 (BaltRim) }
  PlainDriver.AddCodePage('WINDOWS-1258', 9, ceAnsi, zCP_WIN1258, '', 1, False);
  { Microsoft Windows Codepage 1258 (Viet), TCVN-5712 }
end;

{ Handle sql server error messages }
function SybaseErrorHandle(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer;
{$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
var
  SqlError: PDBLibError;
begin
  ErrorCS.Enter;
  try
    New(SqlError);
    SqlError.dbProc := Proc;
    SqlError.Severity := Severity;
    SqlError.DbErr := DbErr;
    SqlError.OsErr := OsErr;
    if DbErrStr <> nil then
      ZSetString(DbErrStr, StrLen(DbErrStr), SqlError.DbErrStr);
    if OsErrStr <> nil then
      ZSetString(OsErrStr, StrLen(OsErrStr), SqlError.OsErrStr);
    SQLErrors.Add(SqlError);
  finally
    Result := INT_CANCEL;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server messages }
function SybaseMessageHandle(Proc: PDBPROCESS; MsgNo: DBINT;
  MsgState, Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar;
  Line: DBUSMALLINT): Integer; {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl
{$ENDIF};
var
  SQLMessage: PDBLibMessage;
begin
  ErrorCS.Enter;
  try
    New(SQLMessage);
    SQLMessage.dbProc := Proc;
    SQLMessage.MsgNo := MsgNo;
    SQLMessage.MsgState := MsgState;
    SQLMessage.Severity := Severity;
    if MsgText <> nil then
      ZSetString(MsgText, StrLen(MsgText), SQLMessage.MsgText);
    if SrvName <> nil then
      ZSetString(SrvName, StrLen(SrvName), SQLMessage.SrvName);
    if ProcName <> nil then
      ZSetString(ProcName, StrLen(ProcName), SQLMessage.ProcName);
    SQLMessage.Line := Line;
    SQLMessages.Add(SQLMessage);
  finally
    Result := 0;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server error messages }
function DbLibErrorHandle(Proc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
var
  SqlError: PDBLibError;
begin
  ErrorCS.Enter;
  try
    New(SqlError);
    SqlError.dbProc := Proc;
    SqlError.Severity := Severity;
    SqlError.DbErr := DbErr;
    SqlError.OsErr := OsErr;
    if DbErrStr <> nil then
      ZSetString(DbErrStr, StrLen(DbErrStr), SqlError.DbErrStr);
    if OsErrStr <> nil then
      ZSetString(OsErrStr, StrLen(OsErrStr), SqlError.OsErrStr);
    SQLErrors.Add(SqlError);
  finally
    Result := INT_CANCEL;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server messages }
function DbLibMessageHandle(Proc: PDBPROCESS; MsgNo: DBINT;
  MsgState, Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar;
  Line: DBUSMALLINT): Integer; cdecl;
var
  SQLMessage: PDBLibMessage;
begin
  ErrorCS.Enter;
  try
    New(SQLMessage);
    SQLMessage.dbProc := Proc;
    SQLMessage.MsgNo := MsgNo;
    SQLMessage.MsgState := MsgState;
    SQLMessage.Severity := Severity;
    if MsgText <> nil then
      ZSetString(MsgText, StrLen(MsgText), SQLMessage.MsgText);
    if SrvName <> nil then
      ZSetString(SrvName, StrLen(SrvName), SQLMessage.SrvName);
    if ProcName <> nil then
      ZSetString(ProcName, StrLen(ProcName), SQLMessage.ProcName);
    SQLMessage.Line := Line;
    SQLMessages.Add(SQLMessage);
  finally
    Result := 0;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server error messages }
function FreeTDSErrorHandle(dbProc: PDBPROCESS; Severity, DbErr, OsErr: Integer;
  DbErrStr, OsErrStr: PAnsiChar): Integer; cdecl;
var
  SqlError: PDBLibError;
begin
  ErrorCS.Enter;
  try
    New(SqlError);
    SqlError.dbProc := dbProc;
    SqlError.Severity := Severity;
    SqlError.DbErr := DbErr;
    SqlError.OsErr := OsErr;
    if DbErrStr <> nil then
      ZSetString(DbErrStr, StrLen(DbErrStr), SqlError.DbErrStr);
    if OsErrStr <> nil then
      ZSetString(OsErrStr, StrLen(OsErrStr), SqlError.OsErrStr);
    SQLErrors.Add(SqlError);
  finally
    Result := INT_CANCEL;
    ErrorCS.Leave;
  end;
end;

{ Handle sql server messages }
function FreeTDSMessageHandle(dbProc: PDBPROCESS; MsgNo: DBINT;
  MsgState, Severity: Integer; MsgText, SrvName, ProcName: PAnsiChar;
  Line: DBUSMALLINT): Integer; cdecl;
var
  SQLMessage: PDBLibMessage;
begin
  ErrorCS.Enter;
  try
    New(SQLMessage);
    SQLMessage.dbProc := dbProc;
    SQLMessage.MsgNo := MsgNo;
    SQLMessage.MsgState := MsgState;
    SQLMessage.Severity := Severity;
    if MsgText <> nil then
      ZSetString(MsgText, StrLen(MsgText), SQLMessage.MsgText);
    if SrvName <> nil then
      ZSetString(SrvName, StrLen(SrvName), SQLMessage.SrvName);
    if ProcName <> nil then
      ZSetString(ProcName, StrLen(ProcName), SQLMessage.ProcName);
    SQLMessage.Line := Line;
    SQLMessages.Add(SQLMessage);
  finally
    Result := 0;
    ErrorCS.Leave;
  end;
end;

constructor TZDBLibAbstractPlainDriver.Create;
var
  I: Integer;
begin
  inherited Create;
  FLoader := TZNativeLibraryLoader.Create([]);
  for I := 0 to high(DBVariables.DBoptions) do
    DBVariables.DBoptions[I] := -1;
  for I := 0 to high(DBVariables.DBSetLoginRec) do
    DBVariables.DBSetLoginRec[I] := -1;
end;

function TZDBLibAbstractPlainDriver.dbbind(Proc: PDBPROCESS;
  Column, VarType, VarLen: Integer; VarAddr: PByte): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbbind_stdcall) then
    Result := Fdbbind_stdcall(Proc, Column, VarType, VarLen, VarAddr)
  else {$ENDIF MSWINDOWS} Result := Fdbbind(Proc, Column, VarType, VarLen, VarAddr)
end;

(* * Cancel the current command batch *)
function TZDBLibAbstractPlainDriver.dbCancel(dbProc: PDBPROCESS): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbcancel_stdcall) then
    Result := Fdbcancel_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := Fdbcancel(dbProc)
end;

(* * Cancel any rows pending from the most recently executed query *)
function TZDBLibAbstractPlainDriver.dbCanQuery(dbProc: PDBPROCESS): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbcanquery_stdcall) then
    Result := Fdbcanquery_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := Fdbcanquery(dbProc)
end;

(* * Close and deallocate a single DBPROCESS structure *)
function TZDBLibAbstractPlainDriver.dbClose(dbProc: PDBPROCESS): RETCODE;
begin
  Result := DBNOERR;
{$IFDEF MSWINDOWS} if Assigned(Fdbclose_MS) then
    Result := Fdbclose_MS(dbProc)
  else if Assigned(Fdbclose_stdcall) then
    Fdbclose_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Fdbclose_SYB(dbProc);
end;

(* * Add text to the DBPROCESS command buffer *)
function TZDBLibAbstractPlainDriver.dbCmd(const dbProc: PDBPROCESS;
  const Cmd: PAnsiChar): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbcmd_stdcall) then
    Result := Fdbcmd_stdcall(dbProc, Cmd)
  else {$ENDIF MSWINDOWS}Result := Fdbcmd(dbProc, Cmd)
end;

(* * Determine whether the current command can return rows *)
function TZDBLibAbstractPlainDriver.dbCmdRow(dbProc: PDBPROCESS): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbcmdrow_stdcall) then
    Result := Fdbcmdrow_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := Fdbcmdrow(dbProc)
end;

function TZDBLibAbstractPlainDriver.dbcolinfo(pdbhandle: pdbhandle;
  _Type: Integer; Column, ComputeId: DBINT; lpdbcol: PDBCOL): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fdbcolinfo) then
    Result := Fdbcolinfo(pdbhandle, _Type, Column, ComputeId, lpdbcol)
  else {$ENDIF MSWINDOWS}Result := DBFAIL;
end;

(* * Return the maximum length of the data in a regular result column *)
function TZDBLibAbstractPlainDriver.dbColLen(dbProc: PDBPROCESS;
  Column: DBINT): DBINT;
begin
  {$IFDEF MSWINDOWS}if Assigned(Fdbcollen_stdcall) then
    Result := Fdbcollen_stdcall(dbProc, Column)
  else {$ENDIF MSWINDOWS}Result := Fdbcollen(dbProc, Column)
end;

(* * Return the name of a regular result column *)
function TZDBLibAbstractPlainDriver.dbColName(dbProc: PDBPROCESS; Column: DBINT)
  : PAnsiChar;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbcolname_stdcall) then
    Result := Fdbcolname_stdcall(dbProc, Column)
  else {$ENDIF MSWINDOWS}Result := Fdbcolname(dbProc, Column)
end;

(* * Return a pointer to the name of the database column from which the
  specified regular result column was derived *)
function TZDBLibAbstractPlainDriver.dbColSource(dbProc: PDBPROCESS;
  Column: DBINT): PAnsiChar;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbcolsource_stdcall) then
    Result := Fdbcolsource_stdcall(dbProc, Column)
  else {$ENDIF MSWINDOWS} if Assigned(Fdbcolsource) then
      Result := Fdbcolsource(dbProc, Column)
    else
      Result := nil;
end;

(* * Return the datatype for a regular result column *)
function TZDBLibAbstractPlainDriver.dbColType(dbProc: PDBPROCESS;
  Column: DBINT): DBINT;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbcoltype_stdcall) then
    Result := Fdbcoltype_stdcall(dbProc, Column)
  else {$ENDIF MSWINDOWS}Result := Fdbcoltype(dbProc, Column)
end;

(* * Return precision and scale information for a regular
  result column of type numeric or decimal. *)
function TZDBLibAbstractPlainDriver.dbcoltypeinfo(Proc: PDBPROCESS;
  Column: Integer): PDBTYPEINFO;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbcoltypeinfo_stdcall) then
    Result := Fdbcoltypeinfo_stdcall(Proc, Column)
  else {$ENDIF MSWINDOWS} if Assigned(Fdbcoltypeinfo) then
      Result := Fdbcoltypeinfo(Proc, Column)
    else
      Result := nil;
end;

(* * Convert data from one datatype to another *)
function TZDBLibAbstractPlainDriver.dbConvert(dbProc: PDBPROCESS;
  SrcType: DBINT; Src: PByte; SrcLen, DestType: DBINT; Dest: PByte;
  DestLen: DBINT): DBINT;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbconvert_stdcall) then
    Result := Fdbconvert_stdcall(dbProc, SrcType, Src, SrcLen, DestType,
      Dest, DestLen)
  else {$ENDIF MSWINDOWS}Result := Fdbconvert(dbProc, SrcType, Src, SrcLen, DestType, Dest, DestLen)
end;

(* * Returns the number of rows affected by a Transact-SQL command *)
function TZDBLibAbstractPlainDriver.dbCount(dbProc: PDBPROCESS): DBINT;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbcount_stdcall) then
    Result := Fdbcount_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := Fdbcount(dbProc);
end;

(* * Return a pointer to the data in a regular result column *)
function TZDBLibAbstractPlainDriver.dbData(dbProc: PDBPROCESS;
  Column: DBINT): PByte;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbdata_stdcall) then
    Result := Fdbdata_stdcall(dbProc, Column)
  else {$ENDIF MSWINDOWS}Result := Fdbdata(dbProc, Column)
end;

function TZDBLibAbstractPlainDriver.dbdataready(Proc: PDBPROCESS): LongBool;
begin
  {$IFDEF MSWINDOWS}if Assigned(Fdbdataready) then
    Result := Fdbdataready(Proc)
  else {$ENDIF MSWINDOWS}Result := Proc <> nil;
end;

(* * Return the length of the data in a regular result column *)
function TZDBLibAbstractPlainDriver.dbDatLen(dbProc: PDBPROCESS;
  Column: DBINT): DBINT;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbdatlen_stdcall) then
    Result := Fdbdatlen_stdcall(dbProc, Column)
  else {$ENDIF MSWINDOWS}Result := Fdbdatlen(dbProc, Column)
end;

(* * Determine whether a particular DBPROCESS is dead *)
function TZDBLibAbstractPlainDriver.dbDead(dbProc: PDBPROCESS): Boolean;
begin
{$IFDEF MSWINDOWS}
  if Assigned(Fdbdead_stdcall) then
    Result := Fdbdead_stdcall(dbProc) = DBSUCCEED
  else if Assigned(Fdbdead_MS) then
    Result := Fdbdead_MS(dbProc)
  else {$ENDIF MSWINDOWS}Result := Fdbdead(dbProc) = DBSUCCEED;
end;

{ ** Read the specified row in the row buffer. * }
function TZDBLibAbstractPlainDriver.dbGetRow(dbProc: PDBPROCESS;
  Row: DBINT): STATUS;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbGetRow_stdcall) then
    Result := FdbGetRow_stdcall(dbProc, Row)
  else {$ENDIF MSWINDOWS}Result := FdbGetRow(dbProc, Row);
end;

{ ** Determine whether the current command or remote procedure call
  generated a return status number. * }
function TZDBLibAbstractPlainDriver.dbHasRetStat(dbProc: PDBPROCESS): Boolean;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbHasRetStat_MS) then
    Result := FdbHasRetStat_MS(dbProc)
  else if Assigned(FdbHasRetStat_stdcall) then
    Result := FdbHasRetStat_stdcall(dbProc) = DBSUCCEED
  else {$ENDIF MSWINDOWS}Result := FdbHasRetStat_SYB(dbProc) = DBSUCCEED;
end;

function TZDBLibAbstractPlainDriver.dbIntit: RETCODE;
{$IFDEF MSWINDOWS}var
  P: PAnsiChar; {$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if Assigned(FdbInit_MS) then
  begin
    P := FdbInit_MS;
    if P <> nil then
      Result := DBSUCCEED
    else
      Result := DBFAIL
  end
  else if Assigned(FdbInit) then
    Result := FdbInit
  else
    Result := FdbInit_stdcall;
{$ELSE MSWINDOWS}
  Result := FdbInit;
{$ENDIF MSWINDOWS}
end;

function TZDBLibAbstractPlainDriver.bcp_batch(const Proc: PDBPROCESS): DBINT;
begin
{$IFDEF MSWINDOWS} if Assigned(Fbcp_batch_stdcall) then
    Result := Fbcp_batch_stdcall(Proc)
  else {$ENDIF MSWINDOWS}Result := Fbcp_batch(Proc);
end;

function TZDBLibAbstractPlainDriver.bcp_bind(Proc: PDBPROCESS; VarAddr: PByte;
  PrefixLen: Integer; VarLen: DBINT; Terminator: PByte;
  TermLen, Typ, TableColumn: Integer): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_bind_stdcall) then
    Result := Fbcp_bind_stdcall(Proc, VarAddr, PrefixLen, VarLen, Terminator,
      TermLen, Typ, TableColumn)
  else {$ENDIF MSWINDOWS}Result := Fbcp_bind(Proc, VarAddr, PrefixLen, VarLen, Terminator, TermLen, Typ, TableColumn);
end;

function TZDBLibAbstractPlainDriver.bcp_colfmt(Proc: PDBPROCESS;
  FileColumn: Integer; FileType: Byte; FilePrefixLen: Integer;
  FileColLen: DBINT; FileTerm: PByte;
  FileTermLen, TableColumn: Integer): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_colfmt_stdcall) then
    Result := Fbcp_colfmt_stdcall(Proc, FileColumn, FileType, FilePrefixLen, FileColLen, FileTerm, FileTermLen, TableColumn)
  else {$ENDIF MSWINDOWS}Result := Fbcp_colfmt(Proc, FileColumn, FileType, FilePrefixLen, FileColLen, FileTerm, FileTermLen, TableColumn);
end;

function TZDBLibAbstractPlainDriver.bcp_collen(Proc: PDBPROCESS; VarLen: DBINT;
  TableColumn: Integer): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_collen_stdcall) then
    Result := Fbcp_collen_stdcall(Proc, VarLen, TableColumn)
  else {$ENDIF MSWINDOWS}Result := Fbcp_collen(Proc, VarLen, TableColumn);
end;

function TZDBLibAbstractPlainDriver.bcp_colptr(Proc: PDBPROCESS; ColPtr: PByte;
  TableColumn: Integer): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_colptr_stdcall) then
    Result := Fbcp_colptr_stdcall(Proc, ColPtr, TableColumn)
  else {$ENDIF MSWINDOWS}Result := Fbcp_colptr(Proc, ColPtr, TableColumn);
end;

function TZDBLibAbstractPlainDriver.bcp_columns(Proc: PDBPROCESS;
  FileColCount: Integer): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_columns_stdcall) then
    Result := Fbcp_columns_stdcall(Proc, FileColCount)
  else {$ENDIF MSWINDOWS}Result := Fbcp_columns(Proc, FileColCount);
end;

function TZDBLibAbstractPlainDriver.bcp_control(Proc: PDBPROCESS;
  Field: Integer; Value: DBINT): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_control_stdcall) then
    Result := Fbcp_control_stdcall(Proc, Field, Value)
  else {$ENDIF MSWINDOWS}Result := Fbcp_control(Proc, Field, Value);
end;

function TZDBLibAbstractPlainDriver.bcp_done(Proc: PDBPROCESS): DBINT;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_done_stdcall) then
    Result := Fbcp_done_stdcall(Proc)
  else {$ENDIF MSWINDOWS}Result := Fbcp_done(Proc);
end;

function TZDBLibAbstractPlainDriver.bcp_exec(Proc: PDBPROCESS;
  RowsCopied: PDBINT): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_exec_stdcall) then
    Result := Fbcp_exec_stdcall(Proc, RowsCopied)
  else {$ENDIF MSWINDOWS}Result := Fbcp_exec(Proc, RowsCopied);
end;

function TZDBLibAbstractPlainDriver.bcp_init(Proc: PDBPROCESS;
  TableName, hFile, ErrFile: PAnsiChar; Direction: Integer): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_init_stdcall) then
    Result := Fbcp_init_stdcall(Proc, TableName, hFile, ErrFile, Direction)
  else {$ENDIF MSWINDOWS}Result := Fbcp_init(Proc, TableName, hFile, ErrFile, Direction);
end;

function TZDBLibAbstractPlainDriver.bcp_moretext(Proc: PDBPROCESS; Size: DBINT;
  Text: PByte): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_moretext_stdcall) then
    Result := Fbcp_moretext_stdcall(Proc, Size, Text)
  else {$ENDIF MSWINDOWS}Result := Fbcp_moretext(Proc, Size, Text);
end;

function TZDBLibAbstractPlainDriver.bcp_readfmt(Proc: PDBPROCESS;
  FileName: PAnsiChar): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_readfmt_stdcall) then
    Result := Fbcp_readfmt_stdcall(Proc, FileName)
  else {$ENDIF MSWINDOWS}Result := Fbcp_readfmt(Proc, FileName);
end;

function TZDBLibAbstractPlainDriver.bcp_sendrow(Proc: PDBPROCESS): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_sendrow_stdcall) then
    Result := Fbcp_sendrow_stdcall(Proc)
  else {$ENDIF MSWINDOWS}Result := Fbcp_sendrow(Proc);
end;

function TZDBLibAbstractPlainDriver.bcp_setl(Login: PLOGINREC;
  Enable: LongBool): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_setl_stdcall) then
    Result := Fbcp_setl_stdcall(Login, Enable)
  else {$ENDIF MSWINDOWS}Result := Fbcp_setl(Login, Enable);
end;

function TZDBLibAbstractPlainDriver.bcp_writefmt(Proc: PDBPROCESS;
  FileName: PAnsiChar): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(Fbcp_writefmt_stdcall) then
    Result := Fbcp_writefmt(Proc, FileName)
  else {$ENDIF MSWINDOWS}Result := Fbcp_writefmt(Proc, FileName);
end;

{ ** Allocates a login record for use in dbopen * }
function TZDBLibAbstractPlainDriver.dbLogin: PLOGINREC;
begin
{$IFDEF MSWINDOWS}
  if Assigned(FdbLogin) then
    Result := FdbLogin
  else
    Result := FdbLogin_stdcall;
{$ELSE}
  Result := FdbLogin;
{$ENDIF}
end;

{ ** Free a login record * }
procedure TZDBLibAbstractPlainDriver.dbLoginFree(Login: PLOGINREC);
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbloginfree_stdcall) then
    Fdbloginfree_stdcall(Login)
  else {$ENDIF MSWINDOWS}Fdbloginfree(Login);
end;

{ ** Indicate whether there are more commands to be processed. * }
function TZDBLibAbstractPlainDriver.dbMoreCmds(dbProc: PDBPROCESS): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbMoreCmds_stdcall) then
    Result := FdbMoreCmds_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := FdbMoreCmds(dbProc);
end;

{ ** Return the name of the current database. * }
function TZDBLibAbstractPlainDriver.dbName(dbProc: PDBPROCESS): PAnsiChar;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbName_stdcall) then
    Result := FdbName_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := FdbName(dbProc);
end;

{ ** Read the next result row into the row buffer and into any program variables
  that are bound to column data * }
function TZDBLibAbstractPlainDriver.dbNextRow(dbProc: PDBPROCESS): STATUS;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbNextRow_stdcall) then
    Result := FdbNextRow_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := FdbNextRow(dbProc);
end;

{ ** Determine the number of regular columns for the current set of results * }
function TZDBLibAbstractPlainDriver.dbNumCols(dbProc: PDBPROCESS): DBINT;
begin
  {$IFDEF MSWINDOWS}if Assigned(FdbNumCols_stdcall) then
    Result := FdbNumCols_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := FdbNumCols(dbProc);
end;

{ ** Create and initialize a DBPROCESS structure. * }
function TZDBLibAbstractPlainDriver.dbOpen(Login: PLOGINREC; Host: PAnsiChar): PDBPROCESS;
begin
  dbsetlogintime(10);
  if Assigned(FtdsDbOpen) then
    Result := FtdsDbOpen(Login, Host, 1
      // this Ord(..) doesn't work as expected. Using it exits the program whenever an error occurs.
      // So I simply revert this to 1 because we always want freetds to behave like it is MS dblib...
      {Ord(ZFastCode.Pos('Sybase', GetDescription) > 0)})
  else
{$IFNDEF MSWINDOWS}
    Result := FdbOpen(Login, Host);
{$ELSE}
    if Assigned(FdbOpen) then
      Result := FdbOpen(Login, Host)
    else
      Result := FdbOpen_stdcall(Login, Host);
{$ENDIF}
end;

{ ** Set up the results of the next query. * }
function TZDBLibAbstractPlainDriver.dbResults(dbProc: PDBPROCESS): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(FdbResults_stdcall)
  then Result := FdbResults_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := FdbResults(dbProc);
end;

{ ** Return a pointer to a return parameter value generated by a stored procedure. * }
function TZDBLibAbstractPlainDriver.dbRetData(dbProc: PDBPROCESS;
  RetNum: DBINT): Pointer;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbRetData_stdcall) then
    Result := FdbRetData_stdcall(dbProc, RetNum)
  else {$ENDIF MSWINDOWS}Result := FdbRetData(dbProc, RetNum);
end;

{ ** Determine the length of a return parameter value
  generated by a stored procedure. * }
function TZDBLibAbstractPlainDriver.dbRetLen(dbProc: PDBPROCESS;
  RetNum: DBINT): DBINT;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbRetLen_stdcall) then
    Result := FdbRetLen_stdcall(dbProc, RetNum)
  else {$ENDIF MSWINDOWS}Result := FdbRetLen(dbProc, RetNum);
end;

{ ** Determine the name of the stored procedure parameter
  associated with a particular return parameter value. * }
function TZDBLibAbstractPlainDriver.dbRetName(dbProc: PDBPROCESS; RetNum: DBINT)
  : PAnsiChar;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbRetName_stdcall) then
    Result := FdbRetName_stdcall(dbProc, RetNum)
  else {$ENDIF MSWINDOWS}Result := FdbRetName(dbProc, RetNum);
end;

{ ** Determine the stored procedure status number returned by the
  current command or remote procedure call.  * }
function TZDBLibAbstractPlainDriver.dbRetStatus(dbProc: PDBPROCESS): DBINT;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbRetStatus_stdcall) then
    Result := FdbRetStatus_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := FdbRetStatus(dbProc);
end;

{ ** Determine the datatype of a return parameter value
  generated by a stored procedure. * }
function TZDBLibAbstractPlainDriver.dbRetType(dbProc: PDBPROCESS;
  RetNum: DBINT): DBINT;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbRetType_stdcall) then
    Result := FdbRetType_stdcall(dbProc, RetNum)
  else {$ENDIF MSWINDOWS}Result := FdbRetType(dbProc, RetNum);
end;

{ ** emmidate exceute the remote procedure call. * }
function TZDBLibAbstractPlainDriver.dbRpcExec(dbProc: PDBPROCESS): RETCODE;
begin
{$IFDEF MSWINDOWS}
  if Assigned(FdbRpcExec) then
    Result := FdbRpcExec(dbProc)
  else if Assigned(FdbRpcSend) then begin
    Result := FdbRpcSend(dbProc);
    if Result = SUCCEED then
      Result := FdbSqlOk(dbProc);
  end else begin
    Result := FdbRpcSend_stdcall(dbProc);
    if Result = SUCCEED then
      Result := FdbSqlOk_stdcall(dbProc);
  end;
{$ELSE}
  Result := dbRpcExec(dbProc);
  if Result = SUCCEED then
    Result := FdbSqlOk(dbProc);
{$ENDIF}
end;

{ ** Initialize a remote procedure call * }
function TZDBLibAbstractPlainDriver.dbRpcInit(dbProc: PDBPROCESS;
  RpcName: PAnsiChar; Options: SmallInt): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbRpcInit_stdcall) then
    Result := FdbRpcInit_stdcall(dbProc, RpcName, Options)
  else {$ENDIF MSWINDOWS}Result := FdbRpcInit(dbProc, RpcName, Options);
end;

{ ** Add a parameter to a remote procedure call. * }
function TZDBLibAbstractPlainDriver.dbRpcParam(dbProc: PDBPROCESS;
  ParamName: PAnsiChar; STATUS: Byte; Type_, MaxLen, DataLen: DBINT;
  Value: Pointer): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbRpcParam_stdcall) then
    Result := FdbRpcParam_stdcall(dbProc, ParamName, STATUS, Type_, MaxLen,
      DataLen, Value)
  else {$ENDIF MSWINDOWS}Result := FdbRpcParam(dbProc, ParamName, STATUS, Type_, MaxLen, DataLen, Value);
end;

{ ** Signal the end of a remote procedure call. * }
function TZDBLibAbstractPlainDriver.dbRpcSend(dbProc: PDBPROCESS): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbRpcSend_stdcall) then
    Result := FdbRpcSend_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := FdbRpcSend(dbProc)
end;

{ ** Set the application name in the LOGINREC structure * }
function TZDBLibAbstractPlainDriver.dbSetLApp(Login: PLOGINREC;
  AppName: PAnsiChar): RETCODE;
var
  Item: DBINT;
begin
  case FDBLibraryVendorType of
    lvtFreeTDS:
      Item := TDSDBSETAPP;
    lvtSybase:
      Item := SYBDBSETAPP;
  else
    Item := MSDBSETAPP;
  end;
  Result := dbsetLName(Login, AppName, Item)
end;

{ ** Set the character set in the LOGINREC structure. * }
function TZDBLibAbstractPlainDriver.dbSetLCharSet(Login: PLOGINREC;
  CharsetName: PAnsiChar): RETCODE;
begin
  case FDBLibraryVendorType of
    lvtFreeTDS:
      Result := dbsetLName(Login, CharsetName, TDSDBSETCHARSET);
    lvtSybase:
      Result := dbsetLName(Login, CharsetName, SYBDBSETCHARSET);
  else
    Result := DBFAIL;
  end;
end;

{ ** Set the host name in the LOGINREC structure * }
function TZDBLibAbstractPlainDriver.dbSetLHost(Login: PLOGINREC;
  HostName: PAnsiChar): RETCODE;
begin
  Result := dbsetLName(Login, HostName, DBSETHOST)
end;

{ ** Set a value in the LOGINREC structure. * }
function TZDBLibAbstractPlainDriver.dbsetLName(Login: PLOGINREC;
  Value: PAnsiChar; Item: DBINT): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbSetLName_stdcall) then
    Result := FdbSetLName_stdcall(Login, Value, Item)
  else {$ENDIF MSWINDOWS}Result := FdbSetLName(Login, Value, Item);
end;

(* * Set the national language name in the LOGINREC structure. *)
function TZDBLibAbstractPlainDriver.dbSetLNatLang(Login: PLOGINREC;
  NatLangName: PAnsiChar): RETCODE;
var
  Item: DBINT;
begin
  case FDBLibraryVendorType of
    lvtFreeTDS:
      Item := TDSDBSETLANG;
    lvtSybase:
      Item := SYBDBSETLANG;
  else
    Item := MSDBSETLANG;
  end;
  Result := dbsetLName(Login, NatLangName, Item)
end;

{ ** Set the number of seconds that DB-Library waits for a server response to
  a request for a DBPROCESS connection. * }
function TZDBLibAbstractPlainDriver.dbSetLoginTime(Seconds: DBINT): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbSetLoginTime_stdcall) then
    Result := FdbSetLoginTime_stdcall(Seconds)
  else {$ENDIF MSWINDOWS}Result := FdbSetLoginTime(Seconds);
end;

{ ** Set the user server password in the LOGINREC structure. * }
function TZDBLibAbstractPlainDriver.dbSetLPwd(Login: PLOGINREC;
  Password: PAnsiChar): RETCODE;
begin
  Result := dbsetLName(Login, Password, DBSETPWD);
end;

{ ** Set the TDS packet size in an application’s LOGINREC structure. * }
function TZDBLibAbstractPlainDriver.dbSetLSecure(Login: PLOGINREC): RETCODE;
begin
  if FDBLibraryVendorType = lvtMS then
    Result := dbsetLName(Login, nil, MSDBSETSECURE)
  else
    Result := DBFAIL;
end;

{ ** Set the user name in the LOGINREC structure. * }
function TZDBLibAbstractPlainDriver.dbSetLUser(Login: PLOGINREC;
  UserName: PAnsiChar): RETCODE;
begin
  Result := dbsetLName(Login, UserName, DBSETUSER);
end;

{ ** Set the maximum number of simultaneously open DBPROCESS structures. * }
function TZDBLibAbstractPlainDriver.dbSetMaxprocs(MaxProcs: SmallInt): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbSetMaxprocs_stdcall) then
    Result := FdbSetMaxprocs_stdcall(MaxProcs)
  else if Assigned(FdbSetMaxprocs_MS) then
    Result := FdbSetMaxprocs_MS(MaxProcs)
  else {$ENDIF MSWINDOWS} Result := FdbSetMaxprocs_SYB(MaxProcs);
end;

{ ** Set a server or DB-Library option. * }
function TZDBLibAbstractPlainDriver.dbSetOpt(dbProc: PDBPROCESS; Option: DBINT;
  Char_Param: PAnsiChar; Int_Param: DBINT): RETCODE;
begin
{$IFDEF MSWINDOWS}
  case FDBLibraryVendorType of
    lvtSybase:
      Result := FdbSetOpt_stdcall(dbProc, Option, Char_Param, Int_Param);
    lvtMS:
      Result := FdbSetOpt_MS(dbProc, Option, Char_Param);
  else {$ENDIF MSWINDOWS}Result := FdbSetOpt_SYB(dbProc, Option, Char_Param, Int_Param);
{$IFDEF MSWINDOWS} end; {$ENDIF}
end;

function TZDBLibAbstractPlainDriver.dbSetTime(Seconds: Integer): RETCODE;
begin
  {$IFDEF MSWINDOWS}if Assigned(FdbSetTime_stdcall) then
    Result := FdbSetTime_stdcall(Seconds)
  else {$ENDIF MSWINDOWS}if Assigned(FdbSetTime) then
    Result := FdbSetTime(Seconds)
  else Result := DBFAIL;
end;

function TZDBLibAbstractPlainDriver.dbSetVersion(Version: DBINT): RETCODE;
begin
  {$IFDEF MSWINDOWS}if Assigned(FdbSetVersion_stdcall) then
    Result := FdbSetVersion_stdcall(Version)
  else {$ENDIF MSWINDOWS}if Assigned(FdbSetVersion) then
    Result := FdbSetVersion(Version)
  else Result := DBFAIL;
end;

function TZDBLibAbstractPlainDriver.dbSqlExec(dbProc: PDBPROCESS;
  Async: Boolean): RETCODE;
begin
  if Async then
    Result := dbSqlExecAsync(dbProc)
  else
    Result := dbSqlExecSync(dbProc);
end;

{ ** Send a command batch to the server and do not wait for a response. * }
function TZDBLibAbstractPlainDriver.dbSqlExecAsync(dbProc: PDBPROCESS): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(Fdbsqlsend_stdcall) then
    Result := Fdbsqlsend_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := Fdbsqlsend(dbProc);
end;

{ ** Send a command batch to the server. * }
function TZDBLibAbstractPlainDriver.dbSqlExecSync(dbProc: PDBPROCESS): RETCODE;
begin
{$IFDEF MSWINDOWS} if Assigned(FdbSqlExec_stdcall) then
    Result := FdbSqlExec_stdcall(dbProc)
  else {$ENDIF MSWINDOWS}Result := FdbSqlExec(dbProc);
end;

{ ** Use a particular database. }
function TZDBLibAbstractPlainDriver.dbUse(dbProc: PDBPROCESS;
  dbName: PAnsiChar): RETCODE;
begin
  {$IFDEF MSWINDOWS} if Assigned(FdbUse_stdcall)
  then Result := FdbUse_stdcall(dbProc, dbName)
  else {$ENDIF MSWINDOWS}Result := FdbUse(dbProc, dbName);
end;

destructor TZDBLibAbstractPlainDriver.Destroy;
begin
  if Loader.Loaded then
    case FDBLibraryVendorType of
      {$IFDEF MSWINDOWS}
      lvtMS: begin
          fdberrhandle(OldMsSQLErrorHandle);
          fdbmsghandle(OldMsSQLMessageHandle);
          fdbWinexit;
          fdbExit;
        end;
      {$ENDIF}
      lvtSybase: begin
          {$IFDEF MSWINDOWS}
          fdberrhandle_stdcall(OldSybaseErrorHandle);
          fdbmsghandle_stdcall(OldSybaseMessageHandle);
          fdbExit_stdcall;
          {$ELSE}
          fdberrhandle(OldSybaseErrorHandle);
          fdbmsghandle(OldSybaseMessageHandle);
          fdbExit;
          {$ENDIF}
        end;
      else begin //FreeTDS
          fdberrhandle(OldFreeTDSErrorHandle);
          fdbmsghandle(OldFreeTDSMessageHandle);
          fdbExit;
        end;
    end;
  inherited Destroy;
end;

procedure TZDBLibAbstractPlainDriver.CheckError(dbProc: Pointer);
var
  S: String;
begin
  S := GetErrorString(dbProc);
  if S <> '' then
    raise EZSQLException.Create(S);
end;

function TZDBLibAbstractPlainDriver.GetDBLibraryVendorType: TDBLibraryVendorType;
begin
  Result := Self.FDBLibraryVendorType;
end;

function TZDBLibAbstractPlainDriver.GetErrorString(dbProc: PDBPROCESS): String;
var
  I: Integer;
  lErrorEntry: PDBLibError;
  lMesageEntry: PDBLibMessage;

  procedure AddToErrorMsg(const AError: String);
  begin
    if Result <> EmptyRaw then
      Result := Result + LineEnding;
    Result := Result + AError;
  end;

begin
  ErrorCS.Enter;
  Result := '';
  try
    if ((SQLErrors = nil) or (SQLErrors.Count = 0)) and
      ((SQLMessages = nil) or (SQLMessages.Count = 0)) then
      Exit;
    I := 0;
    while I < SQLErrors.Count do
    begin
      lErrorEntry := PDBLibError(SQLErrors[I]);
      if (dbProc = nil) or (lErrorEntry^.dbProc = dbProc) or
        (lErrorEntry^.dbProc = nil) then
      begin
        if lErrorEntry^.Severity > EXINFO then
          AddToErrorMsg(Format('DBError : [%4.4d] : %s', [lErrorEntry^.DbErr,
            String(lErrorEntry^.DbErrStr)]));
        if lErrorEntry^.OsErr > EXINFO then
          AddToErrorMsg(Format('OSError : [%4.4d] : %s', [lErrorEntry^.OsErr,
            String(lErrorEntry^.OsErrStr)]));
        Dispose(lErrorEntry);
        SQLErrors.Delete(I);
      end
      else
        Inc(I);
    end;
    I := 0;
    while I < SQLMessages.Count do
    begin
      lMesageEntry := PDBLibMessage(SQLMessages[I]);
      if (dbProc = nil) or (lMesageEntry^.dbProc = dbProc) or
        (lMesageEntry^.dbProc = nil) then
      begin
        if lMesageEntry^.Severity > EXINFO then
        begin
          if lMesageEntry^.MsgNo <> 5701 then
            AddToErrorMsg(String(lMesageEntry^.MsgText));
        end;
        Dispose(lMesageEntry);
        SQLMessages.Delete(I);
      end
      else
        Inc(I);
    end;
  finally
    ErrorCS.Leave;
  end;
end;

function TZDBLibAbstractPlainDriver.GetVariables: TDBVariables;
begin
  Result := DBVariables;
end;

procedure TZDBLibAbstractPlainDriver.LoadApi;
begin
  with FLoader do
  begin
    // test for not exported methods to identify the libs:
    if (GetAddress('dbcolbrowse') <> nil) and
    // not&never exported by FreeTDS see: http://www.freetds.org/userguide/dblib.api.summary.htm
      (GetAddress('dbcoltypeinfo') <> nil) // not exported by ntwdblib.dll
    // so we link against a sybaselib with stdcall on windows and cdecl for all other Os's
    then
      FDBLibraryVendorType := lvtSybase
    else if (GetAddress('dbcoltypeinfo') <> nil)
    // ntwdblib.dll does not export that function
    then
      FDBLibraryVendorType := lvtFreeTDS
    else
      FDBLibraryVendorType := lvtMS;
    // if type sizes or names are different:
    case FDBLibraryVendorType of
      lvtFreeTDS:
        begin
          @Fdbdead := GetAddress('dbdead');
          @Fdbcmdrow := GetAddress('dbcmdrow');
          @Fdbcount := GetAddress('dbcount');
          @Fdbcurrow := GetAddress('dbcurrow');
          @Fdbfirstrow := GetAddress('dbfirstrow');
          @Fdbclose_SYB := GetAddress('dbclose'); // is a procedure
          @FdbSetMaxprocs_SYB := GetAddress('dbsetmaxprocs'); // uses DBINT
          @Fdbloginfree := GetAddress('dbloginfree'); // name diff to ms
          // @{$IFDEF MSWINDOWS}Fdbcolbrowse_SYB{$ELSE}dbcolbrowse{$ENDIF} := GetAddress('dbcolbrowse'); //no FreeTDS
          @FdbMoreCmds := GetAddress('dbmorecmds'); // name diff to ms
          @FdbSetOpt_SYB := GetAddress('dbsetopt');
          // int_param is available but not computed always
          @FdbHasRetStat_SYB := GetAddress('dbhasretstat');
          // DBBOOL vs. LongBool
          @Fdbvarylen_SYB := GetAddress('dbvarylen'); // DBBOOL vs. LongBool
          @FdbInit := GetAddress('dbinit'); // Result is a RetCode
          @FdbSetVersion := GetAddress('dbsetversion');
          // no MS but ms supports dbSetLVersion
          @Fdbsetlversion := GetAddress('dbsetlversion');
          // no sybase see: https://lists.ibiblio.org/pipermail/freetds/2011q4/027489.html
          @Ftdsdump_on := GetAddress('tdsdump_on');
          @Ftdsdump_off := GetAddress('tdsdump_off');
          @Ftdsdump_open := GetAddress('tdsdump_open');
          @Ftdsdump_close := GetAddress('tdsdump_close');
        end;
      lvtSybase:
        begin // handle lower vs uppercase and the call conventions
          @{$IFDEF MSWINDOWS}Fdbdead_stdcall{$ELSE}Fdbdead{$ENDIF} :=
            GetAddress('DBDEAD'); // as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbdead_stdcall{$ELSE}Fdbdead{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbdead_stdcall{$ELSE}Fdbdead{$ENDIF} :=
              GetAddress('dbdead'); // lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbcmdrow_stdcall{$ELSE}Fdbcmdrow{$ENDIF} :=
            GetAddress('DBCMDROW'); // as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbcmdrow_stdcall{$ELSE}Fdbcmdrow{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbcmdrow_stdcall{$ELSE}Fdbcmdrow{$ENDIF} :=
              GetAddress('dbcmdrow'); // lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbcount_stdcall{$ELSE}Fdbcount{$ENDIF} :=
            GetAddress('DBCOUNT'); // as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbcount_stdcall{$ELSE}Fdbcount{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbcount_stdcall{$ELSE}Fdbcount{$ENDIF} :=
              GetAddress('dbcount'); // lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbcurrow_stdcall{$ELSE}Fdbcurrow{$ENDIF} :=
            GetAddress('DBCURROW'); // as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbcurrow_stdcall{$ELSE}Fdbcurrow{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbcurrow_stdcall{$ELSE}Fdbcurrow{$ENDIF} :=
              GetAddress('dbcurrow'); // lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbfirstrow_stdcall{$ELSE}Fdbfirstrow{$ENDIF} :=
            GetAddress('DBFIRSTROW'); // as documented: uppercase
          if not Assigned({$IFDEF MSWINDOWS}Fdbfirstrow_stdcall{$ELSE}Fdbfirstrow{$ENDIF}) then
            @{$IFDEF MSWINDOWS}Fdbfirstrow_stdcall{$ELSE}Fdbfirstrow{$ENDIF} :=
              GetAddress('dbfirstrow'); // lowercase since 15+
          @{$IFDEF MSWINDOWS}Fdbclose_stdcall{$ELSE}Fdbclose_SYB{$ENDIF} :=
            GetAddress('dbclose');
          @{$IFDEF MSWINDOWS}FdbSetMaxprocs_stdcall{$ELSE}FdbSetMaxprocs_SYB{$ENDIF} := GetAddress('dbsetmaxprocs');
          @{$IFDEF MSWINDOWS}Fdbloginfree_stdcall{$ELSE}Fdbloginfree{$ENDIF} :=
            GetAddress('dbloginfree'); // name diff
          // @{$IFDEF MSWINDOWS}Fdbcolbrowse_stdcall{$ELSE}Fdbcolbrowse_SYB{$ENDIF} := GetAddress('dbcolbrowse'); //no FreeTDS
          @{$IFDEF MSWINDOWS}FdbMoreCmds_stdcall{$ELSE}FdbMoreCmds{$ENDIF} :=
            GetAddress('DBMORECMDS'); // uppercase
          if not Assigned({$IFDEF MSWINDOWS}FdbMoreCmds_stdcall{$ELSE}FdbMoreCmds{$ENDIF}) then
            @{$IFDEF MSWINDOWS}FdbMoreCmds_stdcall{$ELSE}FdbMoreCmds{$ENDIF} :=
              GetAddress('dbmorecmds'); // lowercase since 15+

          @{$IFDEF MSWINDOWS}FdbSetOpt_stdcall{$ELSE}FdbSetOpt_SYB{$ENDIF} :=
            GetAddress('dbsetopt'); // int_param is available
          @{$IFDEF MSWINDOWS}FdbHasRetStat_stdcall{$ELSE}FdbHasRetStat_SYB{$ENDIF} := GetAddress('dbhasretstat'); // DBBOOL vs. LongBool
          @{$IFDEF MSWINDOWS}Fdbvarylen_stdcall{$ELSE}Fdbvarylen_SYB{$ENDIF} :=
            GetAddress('dbvarylen'); // DBBOOL vs. LongBool
          @{$IFDEF MSWINDOWS}FdbInit_stdcall{$ELSE}FdbInit{$ENDIF} :=
            GetAddress('dbinit'); // Result is a RetCode
          @{$IFDEF MSWINDOWS}FdbSetVersion_stdcall{$ELSE}FdbSetVersion{$ENDIF} := GetAddress('dbsetversion'); // no MS
        end;
{$IFDEF MSWINDOWS}
      lvtMS:
        begin
          @FdbWinexit := GetAddress('dbwinexit');
          @Fdbdead_MS := GetAddress('dbdead');
          @Fdbcmdrow := GetAddress('dbcmdrow');
          @Fdbcount := GetAddress('dbcount');
          @Fdbcurrow := GetAddress('dbcurrow');
          @Fdbfirstrow := GetAddress('dbfirstrow');
          @Fdbclose_MS := GetAddress('dbclose'); // is a function
          @Fdbsetlpacket := GetAddress('dbsetlpacket');
          // does not use the dbsetlname method
          @FdbSetMaxprocs_MS := GetAddress('dbsetmaxprocs');
          // uses a two byte int
          @Fdbloginfree := GetAddress('dbfreelogin'); // name diff
          // @Fdbcolbrowse_MS := GetAddress('dbcolbrowse'); //no FreeTDS
          @FdbMoreCmds := GetAddress('dbmorecmds'); // name diff to ms
          @FdbSetOpt_MS := GetAddress('dbsetopt'); // int_param is not available
          @FdbHasRetStat_MS := GetAddress('dbhasretstat');
          // DBBOOL vs. LongBool
          @Fdbvarylen_MS := GetAddress('dbvarylen'); // DBBOOL vs. LongBool
          @FdbInit_MS := GetAddress('dbinit');
          // Result is a PAnsiChar not a RetCode
          @Fdbsetlversion := GetAddress('dbsetlversion')
          // no sybase see: https://lists.ibiblio.org/pipermail/freetds/2011q4/027489.html
        end;
{$ENDIF}
    end;
    { type sizes are equal -> call convention! }
{$IFDEF MSWINDOWS}
    if FDBLibraryVendorType = lvtSybase then
    begin
      @Fdbadata_stdcall := GetAddress('dbadata');
      @Fdbadlen_stdcall := GetAddress('dbadlen');
      @Fdbaltbind_stdcall := GetAddress('dbaltbind');
      @Fdbaltbind_ps_stdcall := GetAddress('dbaltbind_ps');
      @Fdbaltcolid_stdcall := GetAddress('dbaltcolid');
      @Fdbaltlen_stdcall := GetAddress('dbaltlen');
      @Fdbaltop_stdcall := GetAddress('dbaltop');
      @Fdbalttype_stdcall := GetAddress('dbalttype');
      @Fdbanullbind_stdcall := GetAddress('dbanullbind');
      @Fdbbind_stdcall := GetAddress('dbbind');
      @Fdbbind_ps_stdcall := GetAddress('dbbind_ps');
      @Fdbbufsize_stdcall := GetAddress('dbbufsize');
      @Fdbbylist_stdcall := GetAddress('dbbylist');
      @Fdbcancel_stdcall := GetAddress('dbcancel');
      @Fdbcanquery_stdcall := GetAddress('dbcanquery');
      @Fdbclrbuf_stdcall := GetAddress('dbclrbuf');
      @Fdbcmd_stdcall := GetAddress('dbcmd');
      @Fdbclropt_stdcall := GetAddress('dbclropt');
      @Fdbcollen_stdcall := GetAddress('dbcollen');
      @Fdbcolname_stdcall := GetAddress('dbcolname');
      @Fdbcolsource_stdcall := GetAddress('dbcolsource');
      @Fdbcoltype_stdcall := GetAddress('dbcoltype');
      @Fdbcolutype_stdcall := GetAddress('dbcolutype');
      @Fdbcoltypeinfo_stdcall := GetAddress('dbcoltypeinfo'); // no MS ?
      @Fdbcolutype_stdcall := GetAddress('dbcolutype');
      @Fdbconvert_stdcall := GetAddress('dbconvert');
      @Fdbconvert_ps_stdcall := GetAddress('bconvert_ps'); // no MS
      @Fdbdata_stdcall := GetAddress('dbdata');
      @Fdbdatlen_stdcall := GetAddress('dbdatlen');
      @Fdbexit_stdcall := GetAddress('dbexit');
      @Fdbfreebuf_stdcall := GetAddress('dbfreebuf');
      @Fdbfreequal_stdcall := GetAddress('dbfreequal');
      @Fdbgetchar_stdcall := GetAddress('dbgetchar');
      @Fdbgetcharset_stdcall := GetAddress('dbgetcharset');
      @FdbGetRow_stdcall := GetAddress('dbgetrow');
      @FdbLogin_stdcall := GetAddress('dblogin');
      @FdbName_stdcall := GetAddress('dbname');
      @FdbNextRow_stdcall := GetAddress('dbnextrow');
      @FdbNumCols_stdcall := GetAddress('dbnumcols');
      @FdbOpen_stdcall := GetAddress('dbopen');
      @FdbSetLoginTime_stdcall := GetAddress('dbsetlogintime');
      @FdbSetLName_stdcall := GetAddress('dbsetlname');
      @FdbSetTime_stdCall := GetAddress('dbsettime');
      @FdbSqlExec_stdcall := GetAddress('dbsqlexec');
      @FdbSqlOk_stdcall := GetAddress('dbsqlok');
      @Fdbsqlsend_stdcall := GetAddress('dbsqlsend');
      @FdbResults_stdcall := GetAddress('dbresults');
      @FdbRetData_stdcall := GetAddress('dbretdata');
      @FdbRetLen_stdcall := GetAddress('dbretlen');
      @FdbRetName_stdcall := GetAddress('dbretname');
      @FdbRetStatus_stdcall := GetAddress('dbretstatus');
      @FdbRetType_stdcall := GetAddress('dbrettype');
      @FdbRpcInit_stdcall := GetAddress('dbrpcinit');
      @FdbRpcParam_stdcall := GetAddress('dbrpcparam');
      @FdbRpcSend_stdcall := GetAddress('dbrpcsend');
      @FdbUse_stdcall := GetAddress('dbuse');

      @fdberrhandle_stdcall := GetAddress('dberrhandle');
      @fdbmsghandle_stdcall := GetAddress('dbmsghandle');
      OldSybaseErrorHandle := fdberrhandle_stdcall(SybaseErrorHandle);
      OldSybaseMessageHandle := fdbmsghandle_stdcall(SybaseMessageHandle);
      @fbcp_batch_stdcall             := GetAddress('bcp_batch');
      @fbcp_bind_stdcall              := GetAddress('bcp_bind');
      @fbcp_colfmt_stdcall            := GetAddress('bcp_colfmt');
      @fbcp_collen_stdcall            := GetAddress('bcp_collen');
      @fbcp_colptr_stdcall            := GetAddress('bcp_colptr');
      @fbcp_columns_stdcall           := GetAddress('bcp_columns');
      @fbcp_control_stdcall           := GetAddress('bcp_control');
      @fbcp_done_stdcall              := GetAddress('bcp_done');
      @fbcp_exec_stdcall              := GetAddress('bcp_exec');
      @fbcp_init_stdcall              := GetAddress('bcp_init');
      @fbcp_moretext_stdcall          := GetAddress('bcp_moretext');
      @fbcp_readfmt_stdcall           := GetAddress('bcp_readfmt');
      @fbcp_sendrow_stdcall           := GetAddress('bcp_sendrow');
      @fbcp_setl_stdcall              := GetAddress('bcp_setl');
      @fbcp_writefmt_stdcall          := GetAddress('bcp_writefmt');
    end
    else {$ENDIF}begin
      @Fdbadata := GetAddress('dbadata');
      @Fdbadlen := GetAddress('dbadlen');
      @Fdbaltbind := GetAddress('dbaltbind');
      @Fdbaltbind_ps := GetAddress('dbaltbind_ps');
      @Fdbaltcolid := GetAddress('dbaltcolid');
      @Fdbaltlen := GetAddress('dbaltlen');
      @Fdbaltop := GetAddress('dbaltop');
      @Fdbalttype := GetAddress('dbalttype');
      @Fdbanullbind := GetAddress('dbanullbind');
      @Fdbbind := GetAddress('dbbind');
      @Fdbbind_ps := GetAddress('dbbind_ps');
      @Fdbbufsize := GetAddress('dbbufsize');
      @Fdbbylist := GetAddress('dbbylist');
      @Fdbcancel := GetAddress('dbcancel');
      @Fdbcanquery := GetAddress('dbcanquery');
      @Fdbclrbuf := GetAddress('dbclrbuf');
      @Fdbclropt := GetAddress('dbclropt');
      @Fdbcmd := GetAddress('dbcmd');
      @Fdbcollen := GetAddress('dbcollen');
      @Fdbcolinfo := GetAddress('dbcolinfo'); // no sybase but freeTDS and MS
      @Fdbcolname := GetAddress('dbcolname');
      @Fdbcolsource := GetAddress('dbcolsource'); // no FreeTDS?
      @Fdbcoltype := GetAddress('dbcoltype');
      @Fdbcoltypeinfo := GetAddress('dbcoltypeinfo'); // no MS
      @Fdbcolutype := GetAddress('dbcolutype');
      @Fdbconvert := GetAddress('dbconvert');
      @Fdbconvert_ps := GetAddress('bconvert_ps'); // no MS
      @Fdbdata := GetAddress('dbdata');
      {$IFDEF MSWINDOWS}@Fdbdataready := GetAddress('dbdata'); { MS only } {$ENDIF}
      @Fdbdatlen := GetAddress('dbdatlen');
      @Fdbexit := GetAddress('dbexit');
      @Fdbfreebuf := GetAddress('dbfreebuf');
      @Fdbfreequal := GetAddress('dbfreequal');
      @Fdbgetchar := GetAddress('dbgetchar');
      @Fdbgetcharset := GetAddress('dbgetcharset');
      @FdbGetRow := GetAddress('dbgetrow');
      @FdbLogin := GetAddress('dblogin');
      @FdbName := GetAddress('dbname');
      @FdbNextRow := GetAddress('dbnextrow');
      @FdbNumCols := GetAddress('dbnumcols');
      @FdbResults := GetAddress('dbresults');
      @FdbOpen := GetAddress('dbopen');
      @FtdsDbOpen := GetAddress('tdsdbopen');
      @FdbRetData := GetAddress('dbretdata');
      @FdbRetLen := GetAddress('dbretlen');
      @FdbRetName := GetAddress('dbretname');
      @FdbRetStatus := GetAddress('dbretstatus');
      @FdbRetType := GetAddress('dbrettype');
      @FdbRpcInit := GetAddress('dbrpcinit');
      @FdbRpcParam := GetAddress('dbrpcparam');
      @FdbRpcSend := GetAddress('dbrpcsend');

      @FdbSetLoginTime := GetAddress('dbsetlogintime');
      @FdbSetLName := GetAddress('dbsetlname');
      @FdbSetTime := GetAddress('dbsettime');
      @FdbSqlExec := GetAddress('dbsqlexec');
      @FdbSqlOk := GetAddress('dbsqlok');
      @Fdbsqlsend := GetAddress('dbsqlsend');
      @FdbUse := GetAddress('dbuse');

      @Fdbmsghandle := GetAddress('dbmsghandle');
      @Fdberrhandle := GetAddress('dberrhandle');

      @fbcp_batch             := GetAddress('bcp_batch');
      @fbcp_bind              := GetAddress('bcp_bind');
      @fbcp_colfmt            := GetAddress('bcp_colfmt');
      @fbcp_collen            := GetAddress('bcp_collen');
      @fbcp_colptr            := GetAddress('bcp_colptr');
      @fbcp_columns           := GetAddress('bcp_columns');
      @fbcp_control           := GetAddress('bcp_control');
      @fbcp_done              := GetAddress('bcp_done');
      @fbcp_exec              := GetAddress('bcp_exec');
      @fbcp_init              := GetAddress('bcp_init');
      @fbcp_moretext          := GetAddress('bcp_moretext');
      @fbcp_readfmt           := GetAddress('bcp_readfmt');
      @fbcp_sendrow           := GetAddress('bcp_sendrow');
      @fbcp_setl              := GetAddress('bcp_setl');
      @fbcp_writefmt          := GetAddress('bcp_writefmt');
      if FDBLibraryVendorType = lvtFreeTDS then
      begin
        OldFreeTDSErrorHandle := Fdberrhandle(FreeTDSErrorHandle);
        OldFreeTDSMessageHandle := Fdbmsghandle(FreeTDSMessageHandle);
      end
      else
      begin
        OldMsSQLErrorHandle := Fdberrhandle(DbLibErrorHandle);
        OldMsSQLMessageHandle := Fdbmsghandle(DbLibMessageHandle);
      end;
      Assert(dbIntit = SUCCEED, 'dbinit failed');
    end;
  end;
end;

procedure TZDBLibAbstractPlainDriver.LoadCodePages;
begin
  { add the default FreeTDS codepages to all descendants}
  AddCodePage('UTF-8', 1, ceUTF8, zCP_UTF8, '', 4, True);
  AddCodePage('ISO-8859-1', 2, ceAnsi, zCP_L1_ISO_8859_1, '', 1, False);
  AddCodePage('ASCII', 3, ceAnsi, zCP_us_ascii, '', 1, False);
end;

{ TZDBLibSybaseASE125PlainDriver }

function TZDBLibSybaseASE125PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZDBLibSybaseASE125PlainDriver.Create;
end;

constructor TZDBLibSybaseASE125PlainDriver.Create;
begin
  inherited Create;
{$IFDEF MSWINDOWS}
  Loader.AddLocation(LIBSYBDB_WINDOWS_DLL_LOCATION);
{$ELSE}
{$IFDEF UNIX}
  Loader.AddLocation(LIBSYBDB_LINUX_DLL_LOCATION);
{$ENDIF}
{$ENDIF}
  DBVariables.DBoptions[Z_PARSEONLY] := DBLIBDBPARSEONLY;
  DBVariables.DBoptions[Z_SHOWPLAN] := DBLIBDBSHOWPLAN;
  DBVariables.DBoptions[Z_NOEXEC] := DBLIBDBNOEXEC;
  DBVariables.DBoptions[Z_ARITHIGNORE] := DBLIBDBARITHIGNORE;
  DBVariables.DBoptions[Z_NOCOUNT] := DBLIBDBNOCOUNT;
  DBVariables.DBoptions[Z_ARITHABORT] := DBLIBDBARITHABORT;
  DBVariables.DBoptions[Z_TEXTLIMIT] := DBLIBDBTEXTLIMIT;
  DBVariables.DBoptions[Z_OFFSET] := DBLIBDBOFFSET;
  DBVariables.DBoptions[Z_STAT] := DBLIBDBSTAT;
  DBVariables.DBoptions[Z_STORPROCID] := DBLIBDBSTORPROCID;
  DBVariables.DBoptions[Z_BUFFER] := DBLIBDBBUFFER;
  DBVariables.DBoptions[Z_NOAUTOFREE] := DBLIBDBNOAUTOFREE;
  DBVariables.DBoptions[Z_ROWCOUNT] := DBLIBDBROWCOUNT;
  DBVariables.DBoptions[Z_TEXTSIZE] := DBLIBDBTEXTSIZE;
  DBVariables.DBoptions[Z_CLIENTCURSORS] := DBLIBDBCLIENTCURSORS;
  DBVariables.DBoptions[Z_SETTIME] := DBLIBDBSET_TIME;
  DBVariables.DBoptions[Z_QUOTEDIDENT] := DBLIBDBQUOTEDIDENT;
  DBVariables.DBoptions[Z_ANSITOOEM] := DBLIBDBANSITOOEM;
  DBVariables.DBoptions[Z_OEMTOANSI] := DBLIBDBOEMTOANSI;
  { MSSQL Loginrec manipulations }
  DBVariables.DBSetLoginRec[Z_SETHOST] := SYBDBSETHOST;
  DBVariables.DBSetLoginRec[Z_SETUSER] := SYBDBSETUSER;
  DBVariables.DBSetLoginRec[Z_SETPWD] := SYBDBSETPWD;
  DBVariables.DBSetLoginRec[Z_SETHID] := SYBDBSETHID;
  DBVariables.DBSetLoginRec[Z_SETAPP] := SYBDBSETAPP;
  DBVariables.DBSetLoginRec[Z_SETBCP] := SYBDBSETBCP;
  DBVariables.DBSetLoginRec[Z_SETLANG] := SYBDBSETLANG;
  DBVariables.DBSetLoginRec[Z_SETNOSHORT] := SYBDBSETNOSHORT;
  DBVariables.DBSetLoginRec[Z_SETHIER] := SYBDBSETHIER;
  DBVariables.DBSetLoginRec[Z_SETCHARSET] := SYBDBSETCHARSET;
  DBVariables.DBSetLoginRec[Z_SETPACKET] := SYBDBSETPACKET;
  DBVariables.DBSetLoginRec[Z_SETENCRYPT] := SYBDBSETENCRYPT;
  DBVariables.DBSetLoginRec[Z_SETLABELED] := SYBDBSETLABELED;
  LoadCodePages;
end;

procedure TZDBLibSybaseASE125PlainDriver.LoadCodePages;
begin
  inherited LoadCodePages;
  AddSybaseCodePages(Self);
end;

function TZDBLibSybaseASE125PlainDriver.GetProtocol: string;
begin
  Result := 'sybase';
end;

function TZDBLibSybaseASE125PlainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for Sybase ASE 12.5';
end;

{ TZDBLibMSSQL7PlainDriver }

function TZDBLibMSSQL7PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZDBLibMSSQL7PlainDriver.Create;
end;

constructor TZDBLibMSSQL7PlainDriver.Create;
begin
  inherited Create;
  Loader.AddLocation(NTWDBLIB_DLL_LOCATION);

  DBVariables.DBoptions[Z_PARSEONLY] := DBLIBDBPARSEONLY;
  DBVariables.DBoptions[Z_SHOWPLAN] := DBLIBDBSHOWPLAN;
  DBVariables.DBoptions[Z_NOEXEC] := DBLIBDBNOEXEC;
  DBVariables.DBoptions[Z_ARITHIGNORE] := DBLIBDBARITHIGNORE;
  DBVariables.DBoptions[Z_NOCOUNT] := DBLIBDBNOCOUNT;
  DBVariables.DBoptions[Z_ARITHABORT] := DBLIBDBARITHABORT;
  DBVariables.DBoptions[Z_TEXTLIMIT] := DBLIBDBTEXTLIMIT;
  DBVariables.DBoptions[Z_OFFSET] := DBLIBDBOFFSET;
  DBVariables.DBoptions[Z_STAT] := DBLIBDBSTAT;
  DBVariables.DBoptions[Z_STORPROCID] := DBLIBDBSTORPROCID;
  DBVariables.DBoptions[Z_BUFFER] := DBLIBDBBUFFER;
  DBVariables.DBoptions[Z_NOAUTOFREE] := DBLIBDBNOAUTOFREE;
  DBVariables.DBoptions[Z_ROWCOUNT] := DBLIBDBROWCOUNT;
  DBVariables.DBoptions[Z_TEXTSIZE] := DBLIBDBTEXTSIZE;
  DBVariables.DBoptions[Z_CLIENTCURSORS] := DBLIBDBCLIENTCURSORS;
  DBVariables.DBoptions[Z_SETTIME] := DBLIBDBSET_TIME;
  DBVariables.DBoptions[Z_QUOTEDIDENT] := DBLIBDBQUOTEDIDENT;
  DBVariables.DBoptions[Z_ANSITOOEM] := DBLIBDBANSITOOEM;
  DBVariables.DBoptions[Z_OEMTOANSI] := DBLIBDBOEMTOANSI;
  { MsSQL Loginrec manipulations }
  DBVariables.DBSetLoginRec[Z_SETHOST] := MSDBSETHOST;
  DBVariables.DBSetLoginRec[Z_SETUSER] := MSDBSETUSER;
  DBVariables.DBSetLoginRec[Z_SETPWD] := MSDBSETPWD;
  DBVariables.DBSetLoginRec[Z_SETHID] := MSDBSETID;
  DBVariables.DBSetLoginRec[Z_SETAPP] := MSDBSETAPP;
  DBVariables.DBSetLoginRec[Z_SETSECURE] := MSDBSETSECURE;
  DBVariables.DBSetLoginRec[Z_SETLANG] := MSDBSETLANG;
  DBVariables.DBSetLoginRec[Z_SETLOGINTIME] := MSDBSET_LOGIN_TIME;
  DBVariables.DBSetLoginRec[Z_SETFALLBACK] := MSDBSETFALLBACK;
  LoadCodePages;
end;

procedure TZDBLibMSSQL7PlainDriver.LoadCodePages;
begin
  inherited LoadCodePages;
  AddmMSCodePages(Self);
end;

function TZDBLibMSSQL7PlainDriver.GetProtocol: string;
begin
  Result := 'mssql';
end;

function TZDBLibMSSQL7PlainDriver.GetDescription: string;
begin
  Result := 'Native dblib driver for MS SQL 7+';
end;

{ TZFreeTDSBasePlainDriver }

constructor TZFreeTDSBasePlainDriver.Create;
begin
  inherited Create;

  DBVariables.DBoptions[Z_PARSEONLY] := TDSPARSEONLY;
  DBVariables.DBoptions[Z_ESTIMATE] := TDSESTIMATE;
  DBVariables.DBoptions[Z_SHOWPLAN] := TDSSHOWPLAN;
  DBVariables.DBoptions[Z_NOEXEC] := TDSNOEXEC;
  DBVariables.DBoptions[Z_ARITHIGNORE] := TDSARITHIGNORE;
  DBVariables.DBoptions[Z_NOCOUNT] := TDSNOCOUNT;
  DBVariables.DBoptions[Z_ARITHABORT] := TDSARITHABORT;
  DBVariables.DBoptions[Z_TEXTLIMIT] := TDSTEXTLIMIT;
  DBVariables.DBoptions[Z_BROWSE] := TDSBROWSE;
  DBVariables.DBoptions[Z_OFFSET] := TDSOFFSET;
  DBVariables.DBoptions[Z_STAT] := TDSSTAT;
  DBVariables.DBoptions[Z_ERRLVL] := TDSERRLVL;
  DBVariables.DBoptions[Z_CONFIRM] := TDSCONFIRM;
  DBVariables.DBoptions[Z_STORPROCID] := TDSSTORPROCID;
  DBVariables.DBoptions[Z_BUFFER] := TDSBUFFER;
  DBVariables.DBoptions[Z_NOAUTOFREE] := TDSNOAUTOFREE;
  DBVariables.DBoptions[Z_ROWCOUNT] := TDSROWCOUNT;
  DBVariables.DBoptions[Z_TEXTSIZE] := TDSTEXTSIZE;
  DBVariables.DBoptions[Z_NATLANG] := TDSNATLANG;
  DBVariables.DBoptions[Z_DATEFORMAT] := TDSDATEFORMAT;
  DBVariables.DBoptions[Z_PRPAD] := TDSPRPAD;
  DBVariables.DBoptions[Z_PRCOLSEP] := TDSPRCOLSEP;
  DBVariables.DBoptions[Z_PRLINELEN] := TDSPRLINELEN;
  DBVariables.DBoptions[Z_PRLINESEP] := TDSPRLINESEP;
  DBVariables.DBoptions[Z_LFCONVERT] := TDSLFCONVERT;
  DBVariables.DBoptions[Z_DATEFIRST] := TDSDATEFIRST;
  DBVariables.DBoptions[Z_CHAINXACTS] := TDSCHAINXACTS;
  DBVariables.DBoptions[Z_FIPSFLAG] := TDSFIPSFLAG;
  DBVariables.DBoptions[Z_ISOLATION] := TDSISOLATION;
  DBVariables.DBoptions[Z_AUTH] := TDSAUTH;
  DBVariables.DBoptions[Z_IDENTITY] := TDSIDENTITY;
  DBVariables.DBoptions[Z_NOIDCOL] := TDSNOIDCOL;
  DBVariables.DBoptions[Z_DATESHORT] := TDSDATESHORT;
  DBVariables.DBoptions[Z_CLIENTCURSORS] := TDSCLIENTCURSORS;
  DBVariables.DBoptions[Z_SETTIME] := TDSSETTIME;
  DBVariables.DBoptions[Z_QUOTEDIDENT] := TDSQUOTEDIDENT;
  DBVariables.DBoptions[Z_NUMOPTIONS] := TDSNUMOPTIONS;
  DBVariables.DBoptions[Z_PADOFF] := TDSPADOFF;
  DBVariables.DBoptions[Z_PADON] := TDSPADON;
  DBVariables.DBoptions[Z_OFF] := TDSOFF;
  DBVariables.DBoptions[Z_ON] := TDSON;
  DBVariables.DBoptions[Z_NOSUCHOPTION] := NOSUCHOPTION;
  DBVariables.DBoptions[Z_MAXOPTTEXT] := MAXOPTTEXT;
  { TDS Loginrec manipulations }
  DBVariables.DBSetLoginRec[Z_SETHOST] := TDSDBSETHOST;
  DBVariables.DBSetLoginRec[Z_SETUSER] := TDSDBSETUSER;
  DBVariables.DBSetLoginRec[Z_SETPWD] := TDSDBSETPWD;
  DBVariables.DBSetLoginRec[Z_SETHID] := TDSDBSETHID;
  DBVariables.DBSetLoginRec[Z_SETAPP] := TDSDBSETAPP;
  DBVariables.DBSetLoginRec[Z_SETBCP] := TDSDBSETBCP;
  DBVariables.DBSetLoginRec[Z_SETSECURE] := TDSDBSETSECURE;
  DBVariables.DBSetLoginRec[Z_SETLANG] := TDSDBSETLANG;
  DBVariables.DBSetLoginRec[Z_SETNOSHORT] := TDSDBSETNOSHORT;
  DBVariables.DBSetLoginRec[Z_SETHIER] := TDSDBSETHIER;
  DBVariables.DBSetLoginRec[Z_SETCHARSET] := TDSDBSETCHARSET;
  DBVariables.DBSetLoginRec[Z_SETPACKET] := TDSDBSETPACKET;
  DBVariables.DBSetLoginRec[Z_SETENCRYPT] := TDSDBSETENCRYPT;
  DBVariables.DBSetLoginRec[Z_SETLABELED] := TDSDBSETLABELED;
  DBVariables.DBSetLoginRec[Z_SETDBNAME] := TDSDBSETDBNAME;
end;

function TZFreeTDSBasePlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS';
end;

function TZFreeTDSBasePlainDriver.GetDescription: string;
begin
  Result := 'Native FreeTDS driver for Sybase and MSSQL Servers';
end;

function TZFreeTDSBasePlainDriver.dbLogin: PLOGINREC;
begin
  Result := inherited dbLogin;
  if Assigned(Result) then
    if not(dbsetlversion(Result) = DBSUCCEED) then
    begin
      dbLoginFree(Result);
      Result := nil;
    end;
end;

function TZFreeTDSBasePlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := fdbsetlversion(Login, TDSDBVERSION_UNKNOWN);
end;

function TZFreeTDSBasePlainDriver.dbsetversion(Version: DBINT): RETCODE;
begin
  Result := fdbsetversion(Version);
end;

function TZFreeTDSBasePlainDriver.dbsetversion: RETCODE;
begin
  Result := fdbsetversion(TDSDBVERSION_UNKNOWN);
end;

procedure TZFreeTDSBasePlainDriver.tdsDumpOff;
begin
  ftdsdump_off();
end;

procedure TZFreeTDSBasePlainDriver.tdsDumpOn;
begin
  ftdsdump_on();
end;

procedure TZFreeTDSBasePlainDriver.tdsDump_Close;
begin
  ftdsDump_Close();
end;

procedure TZFreeTDSBasePlainDriver.tdsDump_Open(const FileName: String);
begin
{$IFDEF UNICODE}
  ftdsDump_Open(PAnsiChar(ZUnicodeToRaw(FileName, ZOSCodePage)));
{$ELSE}
  ftdsDump_Open(Pointer(FileName));
{$ENDIF}
end;

{ TZFreeTDS42MsSQLPlainDriver }
function TZFreeTDS42MsSQLPlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS42MsSQLPlainDriver.Create;
end;

procedure TZFreeTDS42MsSQLPlainDriver.LoadCodePages;
begin
  AddmMSCodePages(Self);
  inherited;
end;

constructor TZFreeTDS42MsSQLPlainDriver.Create;
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(FREETDS_MSSQL_WINDOWS_DLL_LOCATION);
  {$ELSE}
    {$IFDEF UNIX}
    FLoader.AddLocation(FREETDS_LINUX_DLL_LOCATION);
    {$ELSE}
    FLoader.AddLocation(FREETDS_OSX_DLL_LOCATION);
    {$ENDIF}
  {$ENDIF}
  LoadCodePages;
end;

function TZFreeTDS42MsSQLPlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL<=6.5';
end;

function TZFreeTDS42MsSQLPlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 4.2 protocol for MsSQL <=6.5 Servers';
end;

function TZFreeTDS42MsSQLPlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := fdbsetlversion(Login, DBVERSION_42);
end;

function TZFreeTDS42MsSQLPlainDriver.dbsetversion: RETCODE;
begin
  Result := fdbsetversion(TDSDBVERSION_42);
end;

{ TZFreeTDS42SybasePlainDriver }
function TZFreeTDS42SybasePlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS42SybasePlainDriver.Create;
end;

constructor TZFreeTDS42SybasePlainDriver.Create;
begin
  inherited Create;
  {$IFDEF MSWINDOWS}
    FLoader.AddLocation(FREETDS_SYBASE_WINDOWS_DLL_LOCATION);
  {$ELSE}
    {$IFDEF UNIX}
    FLoader.AddLocation(FREETDS_LINUX_DLL_LOCATION);
    {$ELSE}
    FLoader.AddLocation(FREETDS_OSX_DLL_LOCATION);
    {$ENDIF}
  {$ENDIF}
end;

function TZFreeTDS42SybasePlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_Sybase<10';
end;

function TZFreeTDS42SybasePlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 4.2 protocol for Sybase <10 Servers';
end;

function TZFreeTDS42SybasePlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := DBSUCCEED;
end;

function TZFreeTDS42SybasePlainDriver.dbsetversion: RETCODE;
begin
  Result := fdbsetversion(TDSDBVERSION_42);
end;

{ TZFreeTDS50PlainDriver }
function TZFreeTDS50PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS50PlainDriver.Create;
end;

constructor TZFreeTDS50PlainDriver.Create;
begin
  inherited Create;
  LoadCodePages;
end;

function TZFreeTDS50PlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_Sybase-10+';
end;

function TZFreeTDS50PlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 5.0 Protocol for Sybase >= 10 Servers ';
end;

function TZFreeTDS50PlainDriver.dbsetversion: RETCODE;
begin
  Result := fdbsetversion(TDSDBVERSION_100);
end;

function TZFreeTDS50PlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := fdbsetlversion(Login, DBVERSION_100);
end;

{ TZFreeTDS70PlainDriver }

function TZFreeTDS70PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS70PlainDriver.Create;
end;

function TZFreeTDS70PlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL-7.0';
end;

function TZFreeTDS70PlainDriver.dbsetlversion(Login: PLOGINREC): RETCODE;
begin
  Result := fdbsetlversion(Login, DBVERSION_70);
end;

function TZFreeTDS70PlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 7.0 Protocol for MsSQL 7.0 Servers';
end;

function TZFreeTDS70PlainDriver.dbsetversion: RETCODE;
begin
  Result := fdbsetversion(TDSDBVERSION_70);
end;

{ TZFreeTDS71PlainDriver }
function TZFreeTDS71PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS71PlainDriver.Create;
end;

function TZFreeTDS71PlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL-2000';
end;

function TZFreeTDS71PlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 7.1 Protocol for MsSQL 2000 Servers';
end;

function TZFreeTDS71PlainDriver.dbsetversion: RETCODE;
begin
  Result := fdbsetversion(TDSDBVERSION_70);
end;

{ TZFreeTDS72PlainDriver }

function TZFreeTDS72PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZFreeTDS72PlainDriver.Create;
end;

function TZFreeTDS72PlainDriver.GetProtocol: string;
begin
  Result := 'FreeTDS_MsSQL>=2005';
end;

function TZFreeTDS72PlainDriver.GetDescription: string;
begin
  Result := 'FreeTDS 7.2 Protocol for MsSQL 2005, 2008, 2012 Servers';
end;

function TZFreeTDS72PlainDriver.dbsetversion: RETCODE;
begin
  Result := fdbsetversion(TDSDBVERSION_72);
end;

initialization
  SQLErrors := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  SQLMessages := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  ErrorCS := TCriticalSection.Create;
finalization
  FreeAndnil(ErrorCS);
//Free any record in the list if any
  while SQLErrors.Count > 0 do
  begin
    Dispose(PDBLibError(SQLErrors.Items[0]));
    SQLErrors.Delete(0);
  end;
  if SQLErrors <> nil then
    FreeAndNil(SQLErrors);

//Free any record in the list if any
  while SQLMessages.Count > 0 do
  begin
    Dispose(PDBLibMessage(SQLMessages.Items[0]));
    SQLMessages.Delete(0);
  end;
  if SQLMessages <> nil then
    FreeAndNil(SQLMessages);
{$ENDIF ZEOS_DISABLE_DBLIB}

end.
