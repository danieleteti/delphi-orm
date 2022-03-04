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

unit ZPlainASADriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_ASA}

uses Classes, ZCompatibility, ZPlainDriver, ZPlainASAConstants;

{***************** Plain API Constants definition ****************}

type

  {** Represents a generic interface to ASA native API. }
  IZASAPlainDriver = interface (IZPlainDriver)
    ['{86AFDDD6-D401-4A30-B3BE-4AC5095E13F0}']

    function sqlerror_message(sqlca: PZASASQLCA; Buffer: PAnsiChar;
       MaxSize: Integer): PAnsiChar;
    function db_init( sqlca: PZASASQLCA): Integer;
    function db_fini( sqlca: PZASASQLCA): Integer;
    function db_string_connect(sqlca: PZASASQLCA; Params: PAnsiChar): Integer;
    function db_string_disconnect(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_find_engine(sqlca: PZASASQLCA; Params: PAnsiChar): Word;
    function db_start_engine(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_stop_engine(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_start_database(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_stop_database(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;

    function db_alloc_sqlda( NumVar: LongWord): PASASQLDA;
    function db_fill_sqlda( Parameter: PASASQLDA): PASASQLDA;
    function db_fill_s_sqlda( Parameter: PASASQLDA; MaxLength: Integer):
      PASASQLDA;
    procedure db_free_sqlda( Parameter: PASASQLDA);
    procedure db_free_sqlda_noind( Parameter: PASASQLDA);
    procedure db_free_filled_sqlda( Parameter: PASASQLDA);

    procedure db_setconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
    procedure db_disconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
    procedure db_setoption( sqlca: PZASASQLCA; Temporary: LongInt;
      User: PAnsiChar; Option: PAnsiChar; Descriptor: PASASQLDA);

    procedure db_describe_cursor(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; WhatToDesc: LongWord);
    procedure db_prepare_into(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord);
    procedure db_prepare_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SqlStatement: PAnsiChar; Descriptor: PASASQLDA;
      WhatToDesc: LongWord; LongNames: Word);
    procedure db_declare(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      StatementName: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt;
      Options: Word);
    procedure db_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; Descriptor: PASASQLDA; WhatToDesc: Word);
    procedure db_dropstmt(sqlca: PZASASQLCA; StatementName: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt);
    procedure db_open(sqlca: PZASASQLCA; CursorName: PAnsiChar;
     ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
     BlockSize, IsolationLvl: SmallInt; CursorOptions: Word);
    procedure db_close( sqlca: PZASASQLCA; CursorName: PAnsiChar);

    procedure db_fetch(sqlca: PZASASQLCA; CursorName: PAnsiChar; Offset: Word;
      RelPositon: Integer; Descriptor: PASASQLDA; BlockSize: SmallInt;
      Options: Word);
    procedure db_fetch_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: Integer; Descriptor: PASASQLDA;
      BlockSize: SmallInt; Options, ArrayWidth: Word);
    procedure db_get_data(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      ColumnNumber: Word; Offset: Integer; Descriptor: PASASQLDA);
    procedure db_delete(sqlca: PZASASQLCA; CursorName: PAnsiChar);
    procedure db_update(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
    procedure db_put_into(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA);
    procedure db_put_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA; Rows: Word);

    procedure db_select(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SQLDescriptor, ResultDescriptor: PASASQLDA);
    procedure db_execute_into(sqlca: PZASASQLCA; Statement: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
      ResultDescriptor: PASASQLDA);
    procedure db_execute_imm(sqlca: PZASASQLCA; Statement: PAnsiChar);

    procedure db_commit( sqlca: PZASASQLCA; TransLevel: LongWord);
    procedure db_rollback( sqlca: PZASASQLCA; TransLevel: LongWord);
    procedure db_explain(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
    procedure db_register_callback( sqlca: PZASASQLCA;
      CBIdx: ZASA_db_callback_index; Proc: TZASASQLCallback);
    procedure db_resume(sqlca: PZASASQLCA; CursorName: PAnsiChar);
    function db_cancel_request( sqlca: PZASASQLCA): Integer;
    function db_change_char_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
    function db_change_nchar_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
  end;

  {** Implements a driver for ASA 7.0-9.0}
  TZASABasePlainDriver = class (TZAbstractPlainDriver, IZPlainDriver,
    IZASAPlainDriver)
  private
    Fdb_init: function( sqlca: PZASASQLCA): Integer;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    Fdb_fini: function( sqlca: PZASASQLCA): Integer;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdb_string_connect: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    Fdb_string_disconnect: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    Fdb_start_engine: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    Fdb_stop_engine: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    Fdb_start_database: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    Fdb_stop_database: function(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdb_find_engine: function(sqlca: PZASASQLCA; Params: PAnsiChar): Word;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    FAlloc_sqlda: function( NumVar: LongWord): PASASQLDA;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Ffill_sqlda: function( Parameter: PASASQLDA): PASASQLDA;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    Ffree_filled_sqlda: procedure( Parameter: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Ffill_s_sqlda: function( Parameter: PASASQLDA; MaxLength: Integer): PASASQLDA;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Ffree_sqlda: procedure( Parameter: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    Ffree_sqlda_noind: procedure( Parameter: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_setConnect: procedure(sqlca: PZASASQLCA; ConnName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_disconnect: procedure(sqlca: PZASASQLCA; ConnName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_describe_cursor: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_prepare_into: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_prepare_describe: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord;
      LongNames: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    {ASA12 dbpp_prepare_describe_12, (SQLCA *,char *,char *,short int *,char *,struct sqlda *,struct sqlda *,unsigned int, unsigned short int, a_sql_uint32 ))}
    Fdbpp_prepare_describe_12: procedure (SQLCA: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord;
      LongNames: Word; UnknownUint2: Longword)
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
    {ASA16 dbpp_prepare_describe_16, SQLCA *,char *,char *,short int *,char *,struct sqlda *,struct sqlda *,unsigned int, unsigned short int, a_sql_uint32 )))}
    Fdbpp_prepare_describe_16: procedure (SQLCA: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord;
      LongNames: Word; UnknownUint2: Longword)
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_select: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; Descriptor1,
      Descriptor2: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_open: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      UnKnown: PAnsiChar; ProgName: PAnsiChar; RecordStatementNum: PSmallInt;
      Descriptor1: PASASQLDA; BlockSize: SmallInt; IsolationLvl: SmallInt;
      Options : Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_close: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_fetch: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: LongInt; Descriptor1: PASASQLDA;
      BlockSize: SmallInt; Options: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_declare: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      UnKnown: PAnsiChar; ProgName: PAnsiChar; RecordStatementNum: PSmallInt;
      Options: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_dropstmt: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_describe: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; Descriptor: PASASQLDA;
      WhatToDesc: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_delete: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      UnKnown1: PAnsiChar; UnKnown2: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_update: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_execute_imm: procedure(sqlca: PZASASQLCA; SqlRecordStatement:
      PAnsiChar; UnKnown1: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_put_into: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; UnKnown1: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_put_array: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; Into_sqlda: PASASQLDA; Rows: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_commit: procedure( sqlca: PZASASQLCA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_rollback: procedure( sqlca: PZASASQLCA; SomeNumber: LongWord);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_execute_into: procedure(sqlca: PZASASQLCA; UnKnown: PAnsiChar;
      ProgName: PAnsiChar; RecordStatementNum: PSmallInt; Descriptor1: PASASQLDA;
      Descriptor2: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_get_data: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      ColumnNumber: Word; Offset: LongInt; Descriptor1: PASASQLDA;
      Unknown: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_explain: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      SomeNumber1: Word; Descriptor1: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdb_register_a_callback: procedure( sqlca: PZASASQLCA;
      CBIdx: integer; Proc: TZASASQLCallback);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_setoption: procedure( sqlca: PZASASQLCA; Temporary: LongInt;
      User: PAnsiChar; Option: PAnsiChar; Descriptor: PASASQLDA);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_fetch_array: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: LongInt; Descriptor1: PASASQLDA;
      BlockSize: SmallInt; Options: Word; ArrayWidth: Word);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fsqlerror_message: function(sqlca: PZASASQLCA; Buffer: PAnsiChar;
      MaxSize: Integer): PAnsiChar;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdbpp_resume: procedure(sqlca: PZASASQLCA; CursorName: PAnsiChar);
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdb_cancel_request: function( sqlca: PZASASQLCA): Integer;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdb_change_char_charset: function( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};

    Fdb_change_nchar_charset: function( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
      {$IFDEF MSWINDOWS} stdcall {$ELSE} cdecl {$ENDIF};
  protected
    procedure LoadApi; override;
    function GetUnicodeCodePageName: String; override;
  public
    procedure LoadCodePages; override;
    constructor Create;

    function sqlerror_message(sqlca: PZASASQLCA; Buffer: PAnsiChar;
       MaxSize: Integer): PAnsiChar;
    function db_init( sqlca: PZASASQLCA): Integer;
    function db_fini( sqlca: PZASASQLCA): Integer;
    function db_string_connect(sqlca: PZASASQLCA; Params: PAnsiChar): Integer;
    function db_string_disconnect(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_find_engine(sqlca: PZASASQLCA; Params: PAnsiChar): Word;
    function db_start_engine(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_stop_engine(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_start_database(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;
    function db_stop_database(sqlca: PZASASQLCA; Params: PAnsiChar): LongWord;

    function db_alloc_sqlda( NumVar: LongWord): PASASQLDA;
    function db_fill_sqlda( Parameter: PASASQLDA): PASASQLDA;
    function db_fill_s_sqlda( Parameter: PASASQLDA; MaxLength: Integer):
      PASASQLDA;
    procedure db_free_sqlda( Parameter: PASASQLDA);
    procedure db_free_sqlda_noind( Parameter: PASASQLDA);
    procedure db_free_filled_sqlda( Parameter: PASASQLDA);

    procedure db_setconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
    procedure db_disconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
    procedure db_setoption( sqlca: PZASASQLCA; Temporary: LongInt;
      User: PAnsiChar; Option: PAnsiChar; Descriptor: PASASQLDA);

    procedure db_describe_cursor(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; WhatToDesc: LongWord);
    procedure db_prepare_into(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SqlStatement: PAnsiChar;
      Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord);
    procedure db_prepare_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; SqlStatement: PAnsiChar; Descriptor: PASASQLDA;
      WhatToDesc: LongWord; LongNames: Word);
    procedure db_declare(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      StatementName: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt;
      Options: Word);
    procedure db_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; Descriptor: PASASQLDA; WhatToDesc: Word);
    procedure db_dropstmt(sqlca: PZASASQLCA; StatementName: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt);
    procedure db_open(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
      BlockSize, IsolationLvl: SmallInt; CursorOptions: Word);
    procedure db_close(sqlca: PZASASQLCA; CursorName: PAnsiChar);

    procedure db_fetch(sqlca: PZASASQLCA; CursorName: PAnsiChar; Offset: Word;
      RelPositon: Integer; Descriptor: PASASQLDA; BlockSize: SmallInt;
      Options: Word);
    procedure db_fetch_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Offset: Word; RelPositon: Integer; Descriptor: PASASQLDA;
      BlockSize: SmallInt; Options, ArrayWidth: Word);
    procedure db_get_data(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      ColumnNumber: Word; Offset: Integer; Descriptor: PASASQLDA);
    procedure db_delete(sqlca: PZASASQLCA; CursorName: PAnsiChar);
    procedure db_update(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
    procedure db_put_into(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA);
    procedure db_put_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA; Rows: Word);

    procedure db_select(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; Descriptor, ResultDescriptor: PASASQLDA);
    procedure db_execute_into(sqlca: PZASASQLCA; Statement: PAnsiChar;
      ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
      ResultDescriptor: PASASQLDA);
    procedure db_execute_imm(sqlca: PZASASQLCA; Statement: PAnsiChar);

    procedure db_commit( sqlca: PZASASQLCA; TransLevel: LongWord);
    procedure db_rollback( sqlca: PZASASQLCA; TransLevel: LongWord);
    procedure db_explain(sqlca: PZASASQLCA; CursorName: PAnsiChar;
      Descriptor: PASASQLDA);
    procedure db_register_callback( sqlca: PZASASQLCA;
      CBIdx: ZASA_db_callback_index; Proc: TZASASQLCallback);
    procedure db_resume(sqlca: PZASASQLCA; CursorName: PAnsiChar);
    function db_cancel_request( sqlca: PZASASQLCA): Integer;
    function db_change_char_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
    function db_change_nchar_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
  end;

  TZASA7PlainDriver = class(TZASABasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  TZASA8PlainDriver = class(TZASABasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Implements a driver for ASA 9.0 }
  TZASA9PlainDriver = class (TZASABasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

  {** Implements a driver for ASA 12.0 }
  TZASA12PlainDriver = class (TZASABasePlainDriver)
  protected
    function Clone: IZPlainDriver; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;
  end;

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses SysUtils, ZPlainLoader, ZEncoding;

procedure TZASABasePlainDriver.LoadApi;
begin
  with FLoader do
  begin
    @Fsqlerror_message       := GetAddress('sqlerror_message');
    @Fdb_init                := GetAddress('db_init');
    @Fdb_fini                := GetAddress('db_fini');
    @Fdb_string_connect      := GetAddress('db_string_connect');
    @Fdb_string_disconnect   := GetAddress('db_string_disconnect');
    @Fdb_find_engine         := GetAddress('db_find_engine');
    @Fdb_start_engine        := GetAddress('db_start_engine');
    @Fdb_stop_engine         := GetAddress('db_stop_engine');
    @Fdb_start_database      := GetAddress('db_start_database');
    @Fdb_stop_database       := GetAddress('db_stop_database');
    @Falloc_sqlda            := GetAddress('alloc_sqlda');
    @Ffill_sqlda             := GetAddress('fill_sqlda');
    @Ffill_s_sqlda           := GetAddress('fill_s_sqlda');
    @Ffree_filled_sqlda      := GetAddress('free_filled_sqlda');
    @Ffree_sqlda             := GetAddress('free_sqlda');
    @Ffree_sqlda_noind       := GetAddress('free_sqlda_noind');
    @Fdbpp_setConnect        := GetAddress('dbpp_setconnect');
    @Fdbpp_disconnect        := GetAddress('dbpp_disconnect');
    @Fdbpp_prepare_into      := GetAddress('dbpp_prepare_into');
    @Fdbpp_describe_cursor   := GetAddress('dbpp_describe_cursor');
    @Fdbpp_prepare_describe  := GetAddress('dbpp_prepare_describe');
    @Fdbpp_prepare_describe_12  := GetAddress('dbpp_prepare_describe_12');
    @Fdbpp_prepare_describe_16  := GetAddress('dbpp_prepare_describe_16');
    @Fdbpp_select            := GetAddress('dbpp_select');
    @Fdbpp_open              := GetAddress('dbpp_open');
    @Fdbpp_close             := GetAddress('dbpp_close');
    @Fdbpp_fetch             := GetAddress('dbpp_fetch');
    @Fdbpp_declare           := GetAddress('dbpp_declare');
    @Fdbpp_dropstmt          := GetAddress('dbpp_dropstmt');
    @Fdbpp_describe          := GetAddress('dbpp_describe');
    @Fdbpp_delete            := GetAddress('dbpp_delete');
    @Fdbpp_update            := GetAddress('dbpp_update');
    @Fdbpp_put_into          := GetAddress('dbpp_put_into');
    @Fdbpp_put_array         := GetAddress('dbpp_put_array');
    @Fdbpp_execute_imm       := GetAddress('dbpp_execute_imm');
    @Fdbpp_commit            := GetAddress('dbpp_commit');
    @Fdbpp_rollback          := GetAddress('dbpp_rollback');
    @Fdbpp_execute_into      := GetAddress('dbpp_execute_into');
    @Fdbpp_get_data          := GetAddress('dbpp_get_data');
    @Fdbpp_explain           := GetAddress('dbpp_explain');
    @Fdbpp_setoption         := GetAddress('dbpp_setoption');
    @Fdbpp_fetch_array       := GetAddress('dbpp_fetch_array');
    @Fdb_register_a_callback := GetAddress('db_register_a_callback');
    @Fdbpp_resume            := GetAddress('dbpp_resume');
    @Fdb_cancel_request      := GetAddress('db_cancel_request');
    @Fdb_change_char_charset := GetAddress('db_change_char_charset');
    @Fdb_change_nchar_charset:= GetAddress('db_change_nchar_charset');
  end;
end;

function TZASABasePlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF-8';
end;

procedure TZASABasePlainDriver.LoadCodePages;
begin
  { MultiByte }
  AddCodePage('TIS-620', 1, ceAnsi, 874); {Windows Thailändisch, ISO8859-11, binäre Sortierung}
  AddCodePage('Windows-31J', 2, ceAnsi, 932); {Japanese Shift-JIS mit Microsoft-Erweiterungen}
  AddCodePage('GBK', 3, ceAnsi, 936); {GB2312-80 Simplified Chinese}
  AddCodePage('IBM949', 4, ceAnsi, 949); {Korean KS C 5601-1987-Codierung, Wansung}
  AddCodePage('BIG5', 5, ceAnsi, 950); {Traditionelles Chinesisch, Big 5-Kodierung mit HKSCS}
  AddCodePage('EUC_CHINA', 6, ceAnsi, zCP_GB2312); {GB2312-80 Simplified Chinese}
  AddCodePage('UTF-8', 7, ceUTF8, zCP_UTF8, '', 3); {UTF-8, 8-Bit-Mehrbyte-Zeichensatz für Unicode, binäre Reihenfolge}

  { SingleByte }
  AddCodePage('Windows-1250', 8, ceAnsi, 1250); {Windows Latin 2, Polnisch}
  AddCodePage('Windows-1251', 9, ceAnsi, 1251); {Windows Kyrillisch}
  AddCodePage('Windows-1252', 10, ceAnsi, 1252); { Windows Latin 1, Western}
  AddCodePage('Windows-1253', 11, ceAnsi, 1253); {Windows Griechisch, ISO8859-7 mit Erweiterungen}
  AddCodePage('Windows-1254', 12, ceAnsi, 1254); {Windows Türkisch, ISO8859-9 mit Erweiterungen}
  AddCodePage('Windows-1255', 13, ceAnsi, 1255); {Windows Hebräisch, ISO8859-8 mit Erweiterungen}
  AddCodePage('Windows-1256', 14, ceAnsi, 1256); {Windows Arabisch, ISO8859-6 mit Erweiterungen}
  AddCodePage('Windows-1257', 15, ceAnsi, 1257); {Windows Baltische Staaten, Litauisch}
  AddCodePage('Windows-1258', 16, ceAnsi, 1258); {Windows }

  {*nix}
  AddCodePage('ISO_8859-6:1987', 17, ceAnsi, 1256); {Arabisch, ISO8859-6 mit Erweiterungen}
  AddCodePage('ISO_8859-2:1987', 18, ceAnsi, 1251); {Zentral- und Osteuropäisch}
  //ISO-8859-15 //ISO9LATIN1
  //ISO_8859-7:1987 //Griechisch
  //ISO_8859-8:1988 //Hebräisch
  //ISO-8859-15 //Italienisch
  //EUC-JP //Japanisch
  //EUC-KR //Koreanisch
  //ISO_8859-5:1988 //Russisch
  AddCodePage('GB2312', 19, ceAnsi, zCP_GB2312); {GB2312-80 Simplified Chinese}
  //EUC-TW //Traditionelles Chinesisch - Taiwan
  AddCodePage('Big5-HKSCS', 20, ceAnsi, 950); {Traditionelles Chinesisch, Big 5-Kodierung mit HKSCS}
  AddCodePage('ISO_8859-9:1989', 21, ceAnsi, 920); //Türkisch
end;

constructor TZASABasePlainDriver.Create;
begin
  inherited create;
  FLoader := TZNativeLibraryLoader.Create([]);
  LoadCodePages;
end;

function TZASABasePlainDriver.sqlerror_message(sqlca: PZASASQLCA; Buffer: PAnsiChar;
  MaxSize: Integer): PAnsiChar;
begin
  Result := Fsqlerror_message( sqlca, Buffer, MaxSize);
end;

function TZASABasePlainDriver.db_init( sqlca: PZASASQLCA): Integer;
begin
  Result := Fdb_init( sqlca);
end;

function TZASABasePlainDriver.db_fini( sqlca: PZASASQLCA): Integer;
begin
  Result := Fdb_fini( sqlca);
end;

function TZASABasePlainDriver.db_string_connect(sqlca: PZASASQLCA; Params: PAnsiChar):
  Integer;
begin
  Result := Fdb_string_connect( sqlca, Params);
end;

function TZASABasePlainDriver.db_string_disconnect( sqlca: PZASASQLCA;
  Params: PAnsiChar): LongWord;
begin
  Result := Fdb_string_disconnect( sqlca, Params)
end;

function TZASABasePlainDriver.db_find_engine(sqlca: PZASASQLCA; Params: PAnsiChar):
  Word;
begin
  Result := Fdb_find_engine( sqlca, Params);
end;

function TZASABasePlainDriver.db_start_engine(sqlca: PZASASQLCA; Params: PAnsiChar):
  LongWord;
begin
  Result := Fdb_start_engine( sqlca, Params);
end;

function TZASABasePlainDriver.db_stop_engine(sqlca: PZASASQLCA; Params: PAnsiChar):
  LongWord;
begin
  Result := Fdb_stop_engine( sqlca, Params);
end;

function TZASABasePlainDriver.db_start_database(sqlca: PZASASQLCA; Params: PAnsiChar):
  LongWord;
begin
  Result := Fdb_start_database( sqlca, Params);
end;

function TZASABasePlainDriver.db_stop_database(sqlca: PZASASQLCA; Params: PAnsiChar):
  LongWord;
begin
  Result := Fdb_stop_database( sqlca, Params);
end;

function TZASABasePlainDriver.db_alloc_sqlda( NumVar: LongWord): PASASQLDA;
begin
  Result := Falloc_sqlda( NumVar);
end;

function TZASABasePlainDriver.db_fill_sqlda( Parameter: PASASQLDA): PASASQLDA;
begin
  Result := Ffill_sqlda( Parameter);
end;

function TZASABasePlainDriver.db_fill_s_sqlda( Parameter: PASASQLDA;
  MaxLength: Integer): PASASQLDA;
begin
  Result := Ffill_s_sqlda( Parameter, MaxLength);
end;

procedure TZASABasePlainDriver.db_free_sqlda( Parameter: PASASQLDA);
begin
  Ffree_sqlda( Parameter);
end;

procedure TZASABasePlainDriver.db_free_sqlda_noind( Parameter: PASASQLDA);
begin
  Ffree_sqlda_noind( Parameter);
end;

procedure TZASABasePlainDriver.db_free_filled_sqlda( Parameter: PASASQLDA);
begin
  Ffree_filled_sqlda( Parameter);
end;

procedure TZASABasePlainDriver.db_setconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
begin
  Fdbpp_setconnect( sqlca, ConnStr);
end;

procedure TZASABasePlainDriver.db_disconnect(sqlca: PZASASQLCA; ConnStr: PAnsiChar);
begin
  Fdbpp_disconnect( sqlca, ConnStr);
end;

procedure TZASABasePlainDriver.db_setoption( sqlca: PZASASQLCA; Temporary: Integer;
   User: PAnsiChar; Option: PAnsiChar; Descriptor: PASASQLDA);
begin
  Fdbpp_setoption( sqlca, Temporary, User, Option, Descriptor);
end;

procedure TZASABasePlainDriver.db_describe_cursor( sqlca: PZASASQLCA;
  CursorName: PAnsiChar; Descriptor: PASASQLDA; WhatToDesc: LongWord);
begin
  Fdbpp_describe_cursor( sqlca, CursorName, Descriptor, WhatToDesc);
end;

procedure TZASABasePlainDriver.db_prepare_into( sqlca: PZASASQLCA;
  ProgName: PAnsiChar; StatementNum: PSmallInt; SqlStatement: PAnsiChar;
  Descriptor1: PASASQLDA; Descriptor2: PASASQLDA; WhatToDesc: LongWord);
begin
  Fdbpp_prepare_into( sqlca, nil, ProgName, StatementNum, SqlStatement,
    Descriptor1, Descriptor2, WhatToDesc);
end;

procedure TZASABasePlainDriver.db_prepare_describe( sqlca: PZASASQLCA;
   ProgName: PAnsiChar; StatementNum: PSmallInt; SqlStatement: PAnsiChar;
  Descriptor: PASASQLDA; WhatToDesc: LongWord; LongNames: Word);
var
  U1: LongWord;
begin
  U1 := 0;
  if Assigned(Fdbpp_prepare_describe) then
    Fdbpp_prepare_describe( sqlca, nil, ProgName, StatementNum,
      SqlStatement, nil, Descriptor, WhatToDesc, LongNames)
  else
    if Assigned(Fdbpp_prepare_describe_12) then
    Fdbpp_prepare_describe_12(sqlca, nil, ProgName, StatementNum,
        SqlStatement, nil, Descriptor, WhatToDesc, LongNames, U1)
    else if Assigned(Fdbpp_prepare_describe_16) then
      Fdbpp_prepare_describe_16(sqlca, nil, ProgName, StatementNum,
        SqlStatement, nil, Descriptor, WhatToDesc, LongNames, U1)
end;

procedure TZASABasePlainDriver.db_declare(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  StatementName: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt;
  Options: Word);
begin
  Fdbpp_declare( sqlca, CursorName, StatementName, ProgName,
    StatementNum, Options);
end;

procedure TZASABasePlainDriver.db_describe(sqlca: PZASASQLCA; ProgName: PAnsiChar;
      StatementNum: PSmallInt; Descriptor: PASASQLDA; WhatToDesc: Word);
begin
  Fdbpp_describe( sqlca, nil, ProgName, StatementNum, Descriptor,
    WhatToDesc);
end;

procedure TZASABasePlainDriver.db_dropstmt( sqlca: PZASASQLCA;
  StatementName: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt);
begin
  Fdbpp_dropstmt( sqlca, StatementName, ProgName, StatementNum);
end;

procedure TZASABasePlainDriver.db_open(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  ProgName: PAnsiChar; StatementNum: PSmallInt; Descriptor: PASASQLDA;
  BlockSize, IsolationLvl: SmallInt; CursorOptions: Word);
begin
  Fdbpp_open( sqlca, CursorName, nil, ProgName, StatementNum,
    Descriptor, BlockSize, IsolationLvl, CursorOptions);
end;

procedure TZASABasePlainDriver.db_close(sqlca: PZASASQLCA; CursorName: PAnsiChar);
begin
  Fdbpp_close( sqlca, CursorName);
end;

procedure TZASABasePlainDriver.db_fetch(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Offset: Word; RelPositon: Integer; Descriptor: PASASQLDA;
  BlockSize: SmallInt; Options: Word);
begin
  Fdbpp_fetch( sqlca, CursorName, Offset, RelPositon, Descriptor,
    BlockSize, Options);
end;

procedure TZASABasePlainDriver.db_fetch_array( sqlca: PZASASQLCA;
  CursorName: PAnsiChar; Offset: Word; RelPositon: Integer;
  Descriptor: PASASQLDA; BlockSize: SmallInt; Options, ArrayWidth: Word);
begin
  Fdbpp_fetch_array( sqlca, CursorName, Offset, RelPositon, Descriptor,
    BlockSize, Options, ArrayWidth);
end;

procedure TZASABasePlainDriver.db_get_data(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  ColumnNumber: Word; Offset: Integer; Descriptor: PASASQLDA);
begin
  Fdbpp_get_data( sqlca, CursorName, ColumnNumber, Offset, Descriptor, 0);
end;

procedure TZASABasePlainDriver.db_delete(sqlca: PZASASQLCA; CursorName: PAnsiChar);
begin
  Fdbpp_delete( sqlca, CursorName, nil, nil);
end;

procedure TZASABasePlainDriver.db_update(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Descriptor: PASASQLDA);
begin
  Fdbpp_update( sqlca, CursorName, Descriptor);
end;

procedure TZASABasePlainDriver.db_put_into(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA);
begin
  Fdbpp_put_into( sqlca, CursorName, Descriptor, ResultDescriptor);
end;

procedure TZASABasePlainDriver.db_put_array(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA; Rows: Word);
begin
  Fdbpp_put_array( sqlca, CursorName, Descriptor, ResultDescriptor,
    Rows);
end;

procedure TZASABasePlainDriver.db_select(sqlca: PZASASQLCA; ProgName: PAnsiChar;
  StatementNum: PSmallInt; Descriptor, ResultDescriptor: PASASQLDA);
begin
  Fdbpp_select( sqlca, nil, ProgName, StatementNum, Descriptor, ResultDescriptor);
end;

procedure TZASABasePlainDriver.db_execute_into( sqlca: PZASASQLCA;
  Statement: PAnsiChar; ProgName: PAnsiChar; StatementNum: PSmallInt;
  Descriptor: PASASQLDA; ResultDescriptor: PASASQLDA);
begin
  Fdbpp_execute_into(sqlca, Statement, ProgName, StatementNum, Descriptor, ResultDescriptor);
end;

procedure TZASABasePlainDriver.db_execute_imm( sqlca: PZASASQLCA;
  Statement: PAnsiChar);
begin
  Fdbpp_execute_imm( sqlca, Statement, 2);
end;

procedure TZASABasePlainDriver.db_commit( sqlca: PZASASQLCA; TransLevel: LongWord);
begin
  Fdbpp_commit( sqlca, TransLevel);
end;

procedure TZASABasePlainDriver.db_rollback( sqlca: PZASASQLCA;
  TransLevel: LongWord);
begin
  Fdbpp_rollback( sqlca, TransLevel);
end;

procedure TZASABasePlainDriver.db_explain(sqlca: PZASASQLCA; CursorName: PAnsiChar;
  Descriptor: PASASQLDA);
begin
  Fdbpp_explain( sqlca, CursorName, 0, Descriptor);
end;

procedure TZASABasePlainDriver.db_register_callback( sqlca: PZASASQLCA;
  CBIdx: ZASA_db_callback_index; Proc: TZASASQLCallback);
begin
  Fdb_register_a_callback( sqlca, Integer( CBIdx), Proc);
end;

procedure TZASABasePlainDriver.db_resume(sqlca: PZASASQLCA; CursorName: PAnsiChar);
begin
  Fdbpp_resume( sqlca, CursorName);
end;

function TZASABasePlainDriver.db_cancel_request( sqlca: PZASASQLCA): Integer;
begin
  Result := Fdb_cancel_request( sqlca);
end;

function TZASABasePlainDriver.db_change_char_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
begin
  Result := Fdb_change_char_charset(sqlca, CharSet);
end;

function TZASABasePlainDriver.db_change_nchar_charset( sqlca: PZASASQLCA; const CharSet: PAnsiChar): Word;
begin
  Result := Fdb_change_nchar_charset(sqlca, CharSet);
end;

{TZASA7PlainDriver}

function TZASA7PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASA7PlainDriver.Create;
end;

constructor TZASA7PlainDriver.Create;
begin
  inherited Create;
  FLoader.AddLocation({$IFNDEF LINUX}ASA7_WINDOWS_DLL_LOCATION{$ELSE}ASA7_LINUX_DLL_LOCATION{$ENDIF});
end;

function TZASA7PlainDriver.GetProtocol: string;
begin
  Result := 'ASA7';
end;

function TZASA7PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Sybase ASA 7.0 DBLib';
end;

{TZASA8PlainDriver}

constructor TZASA8PlainDriver.Create;
begin
  inherited Create;
  FLoader.AddLocation({$IFNDEF LINUX}ASA8_WINDOWS_DLL_LOCATION{$ELSE}ASA8_LINUX_DLL_LOCATION{$ENDIF});
end;

function TZASA8PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASA8PlainDriver.Create;
end;

function TZASA8PlainDriver.GetProtocol: string;
begin
  Result := 'ASA8';
end;

function TZASA8PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Sybase ASA 8.0 DBLib';
end;

{TZASA9PlainDriver}

constructor TZASA9PlainDriver.Create;
begin
  inherited Create;
  FLoader.AddLocation({$IFNDEF LINUX}ASA9_WINDOWS_DLL_LOCATION{$ELSE}ASA9_LINUX_DLL_LOCATION{$ENDIF});
end;

function TZASA9PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASA9PlainDriver.Create;
end;

function TZASA9PlainDriver.GetProtocol: string;
begin
  Result := 'ASA9';
end;

function TZASA9PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Sybase ASA 9.0 DBLib';
end;

{TZASA12PlainDriver}

constructor TZASA12PlainDriver.Create;
begin
  inherited Create;
  FLoader.AddLocation({$IFNDEF LINUX}ASA12_WINDOWS_DLL_LOCATION{$ELSE}ASA12_LINUX_DLL_LOCATION{$ENDIF});
end;

function TZASA12PlainDriver.Clone: IZPlainDriver;
begin
  Result := TZASA12PlainDriver.Create;
end;

function TZASA12PlainDriver.GetProtocol: string;
begin
  Result := 'ASA12';
end;

function TZASA12PlainDriver.GetDescription: string;
begin
  Result := 'Native Plain Driver for Sybase ASA 12.0 DBLib';
end;
{$ENDIF ZEOS_DISABLE_ASA}
end.

