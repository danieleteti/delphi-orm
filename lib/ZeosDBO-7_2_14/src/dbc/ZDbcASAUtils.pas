{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Sybase SQL Anywhere Connectivity Classes         }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
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

unit ZDbcASAUtils;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ASA}
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  ZSysUtils, ZDbcIntfs, ZPlainASADriver, ZDbcLogging, ZCompatibility, ZDbcASA,
  ZDbcStatement, ZVariant, ZPlainASAConstants;

const
  StdVars = 20;
  MinBLOBSize = 256;
  BlockSize = 20;

type
  { ASA Error Class}
  EZASAConvertError = class(Exception);

  TZASADECLTYPE = record
    sqlType: SmallInt;
    sqlLen : NativeUInt;
  end;

  { Base interface for sqlda }
  IZASASQLDA = interface
    ['{7606E8EB-9FC8-4F76-8D91-E23AB96409E1}']
    procedure CreateException(const Msg: string);
    procedure AllocateSQLDA( NumVars: Word);
    procedure InitFields;
    procedure FreeSQLDA;

    function GetData: PASASQLDA;
    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldName(const Index: Word): String;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): Word;

    procedure UpdateNull(const Index: Integer; Value: boolean);
    procedure UpdateBoolean(const Index: Integer; Value: boolean);
    procedure UpdateByte(const Index: Integer; Value: Byte);
    procedure UpdateShort(const Index: Integer; Value: ShortInt);
    procedure UpdateSmall(const Index: Integer; Value: SmallInt);
    procedure UpdateWord(const Index: Integer; Value: Word);
    procedure UpdateInt(const Index: Integer; Value: Integer);
    procedure UpdateUInt(const Index: Integer; Value: LongWord);
    procedure UpdateLong(const Index: Integer; Value: Int64);
    procedure UpdateULong(const Index: Integer; Value: UInt64);
    procedure UpdateFloat(const Index: Integer; Value: Single);
    procedure UpdateDouble(const Index: Integer; Value: Double);
    procedure UpdateBigDecimal(const Index: Integer; Value: Extended);
    procedure UpdatePRaw(const Index: Integer; Value: PAnsiChar; Len: NativeUInt);
    procedure UpdateBytes(const Index: Integer; const Value: TBytes);
    procedure UpdateDate(const Index: Integer; Value: TDateTime);
    procedure UpdateTime(const Index: Integer; Value: TDateTime);
    procedure UpdateTimestamp(const Index: Integer; Value: TDateTime);
    procedure WriteBlob(const Index: Integer; Stream: TStream; const BlobType: TZSQLType);

    function IsNull(const Index: Integer): Boolean;
    function IsAssigned(const Index: Integer): Boolean;

    procedure ReadBlobToMem(const Index: Word; out Buffer: Pointer; out Length: NativeUInt; const Binary: Boolean = True);
    procedure ReadBlobToString(const Index: Word; out str: RawByteString);
  end;

  { Base class contain core functions to work with sqlda structure
    Can allocate memory for sqlda structure get basic information }
  TZASASQLDA = class (TInterfacedObject, IZASASQLDA)
  private
    FConSettings: PZConSettings;
    FSQLDA: PASASQLDA;
    FPlainDriver: IZASAPlainDriver;
    FHandle: PZASASQLCA;
    FCursorName: PAnsiChar;
    procedure CreateException(const  Msg: string);
    procedure CheckIndex(const Index: Word);
    procedure CheckRange(const Index: Word);
    procedure SetFieldType(const Index: Word; ASAType: Smallint; Len: LongWord;
      SetDeclType: Boolean = true); overload;
    procedure SetFieldType(ToSQLDA: PASASQLDA; const Index: Word; ASAType: Smallint; Len: LongWord;
      SetDeclType: Boolean = true); overload;
  protected
    FDeclType: array of TZASADECLTYPE;
    procedure ReadBlob(const Index: Word; var Buffer: Pointer; Length: LongWord);
  public
    constructor Create(const PlainDriver: IZASAPlainDriver; Handle: PZASASQLCA;
      CursorName: PAnsiChar; ConSettings: PZConSettings; NumVars: Word = StdVars);
    destructor Destroy; override;

    procedure AllocateSQLDA( NumVars: Word);
    procedure InitFields;
    procedure FreeSQLDA;

    function GetData: PASASQLDA;
    function IsBlob(const Index: Word): boolean;
    function IsNullable(const Index: Word): boolean;

    function GetFieldCount: Integer;
    function GetFieldName(const Index: Word): String;
    function GetFieldIndex(const Name: String): Word;
    function GetFieldScale(const Index: Word): Integer;
    function GetFieldSqlType(const Index: Word): TZSQLType;
    function GetFieldLength(const Index: Word): Word;

    procedure UpdateNull(const Index: Integer; Value: boolean);
    procedure UpdateBoolean(const Index: Integer; Value: boolean);
    procedure UpdateByte(const Index: Integer; Value: Byte);
    procedure UpdateShort(const Index: Integer; Value: ShortInt);
    procedure UpdateSmall(const Index: Integer; Value: SmallInt);
    procedure UpdateWord(const Index: Integer; Value: Word);
    procedure UpdateInt(const Index: Integer; Value: Integer);
    procedure UpdateUInt(const Index: Integer; Value: LongWord);
    procedure UpdateLong(const Index: Integer; Value: Int64);
    procedure UpdateULong(const Index: Integer; Value: UInt64);
    procedure UpdateFloat(const Index: Integer; Value: Single);
    procedure UpdateDouble(const Index: Integer; Value: Double);
    procedure UpdateBigDecimal(const Index: Integer; Value: Extended);
    procedure UpdatePRaw(const Index: Integer; Value: PAnsiChar; Len: NativeUInt);
    procedure UpdateBytes(const Index: Integer; const Value: TBytes);
    procedure UpdateDate(const Index: Integer; Value: TDateTime);
    procedure UpdateTime(const Index: Integer; Value: TDateTime);
    procedure UpdateTimestamp(const Index: Integer; Value: TDateTime);
    procedure UpdateDateTime(const Index: Integer; Value: TDateTime);
    procedure WriteBlob(const Index: Integer; Stream: TStream; const BlobType: TZSQLType);

    function IsNull(const Index: Integer): Boolean;
    function IsAssigned(const Index: Integer): Boolean;

    procedure ReadBlobToMem(const Index: Word; out Buffer: Pointer; out Length: NativeUInt; const Binary: Boolean = True);
    procedure ReadBlobToString(const Index: Word; out str: RawByteString);
  end;

{**
  Converts a ASA native type into ZDBC SQL types.
  @param FieldHandle a handler to field description structure.
  @return a SQL undepended type.
}
function ConvertASATypeToSQLType(const SQLType: SmallInt; const CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Converts a ASA native type into String.
  @param SQLType Field of TASASQLVar structure.
  @return type description.
}
function ConvertASATypeToString( SQLType: SmallInt): String;

function ConvertASAJDBCToSqlType(const FieldType: SmallInt;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
{
procedure TSQLTimeStampToASADateTime( DT: TSQLTimeStamp; const ASADT: PZASASQLDateTime);
function ASADateTimeToSQLTimeStamp( ASADT: PZASASQLDateTime): TSQLTimeStamp;
}
{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckASAError(const PlainDriver: IZASAPlainDriver;
  const Handle: PZASASQLCA; const LogCategory: TZLoggingCategory;
  const ConSettings: PZConSettings; const LogMessage: RawByteString = '';
  const SupressExceptionID: Integer = 0);

function GetCachedResultSet(const SQL: string;
  const Statement: IZStatement; const NativeResultSet: IZResultSet): IZResultSet;

procedure DescribeCursor(const FASAConnection: IZASAConnection; const FSQLData: IZASASQLDA;
  const Cursor: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}; const SQL: RawByteString);

procedure ASAPrepare(const FASAConnection: IZASAConnection; const FSQLData, FParamsSQLData: IZASASQLDA;
   const SQL: RawByteString; StmtNum: PSmallInt; var FPrepared, FMoreResults: Boolean);

procedure PrepareParameters(const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const InParamTypes: TZSQLTypeArray;
  InParamCount: Integer; const ParamSqlData: IZASASQLDA;
  ConSettings: PZConSettings);

function RandomString( Len: integer): string;

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses Variants, Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZFastCode, ZMessages, ZDbcCachedResultSet, ZEncoding, ZDbcUtils, ZClasses;

{ TZASASQLDA }

procedure TZASASQLDA.CreateException(const Msg: string);
begin
  DriverManager.LogError( lcOther, FConSettings^.Protocol, '', -1, ConvertEMsgToRaw(Msg, FConSettings^.ClientCodePage^.CP));
  raise EZSQLException.Create( Format( SSQLError1, [ Msg]));
end;

{**
   Check range count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZASASQLDA.CheckIndex(const Index: Word);
begin
  Assert( Assigned( FSQLDA), 'SQLDA not initialized.');
  Assert( Index < Word(FSQLDA.sqld), 'Out of Range.');
end;

procedure TZASASQLDA.CheckRange(const Index: Word);
begin
  CheckIndex( Index);
  Assert( Assigned( FSQLDA.sqlVar[ Index].sqlData),
    'No memory for variable in SQLDA.');
end;

procedure TZASASQLDA.SetFieldType(ToSQLDA: PASASQLDA; const Index: Word;
  ASAType: Smallint; Len: LongWord; SetDeclType: Boolean = true);
begin
  CheckIndex(Index);
  with ToSQLDA.sqlvar[Index] do
  begin
    if ( ASAType and $FFFE = DT_LONGBINARY) or
       ( ASAType and $FFFE = DT_LONGNVARCHAR) or
       ( ASAType and $FFFE = DT_LONGVARCHAR) then
    begin
      if Assigned( sqlData) then
        ReallocMem( sqlData, SizeOf(TZASABlobStruct)+Len)
      else
        GetMem( sqlData, SizeOf( TZASABlobStruct)+Len);
      PZASABlobStruct( sqlData).array_len := Len;
      PZASABlobStruct( sqlData).stored_len := 0;
      PZASABlobStruct( sqlData).untrunc_len := 0;
      PZASABlobStruct( sqlData).arr[0] := AnsiChar(#0);
      Len := SizeOf( TZASABlobStruct)-1;
      //Inc( Len, SizeOf( TZASABlobStruct)-1);
    end
    else
    begin
      if ( ASAType and $FFFE = DT_BINARY) or
         ( ASAType and $FFFE = DT_VARCHAR) then
        Inc( Len, SizeOf( TZASASQLSTRING));
      if Assigned( sqlData) then
        ReallocMem( sqlData, Len)
      else
        GetMem( sqlData, Len);
      if ( ASAType and $FFFE = DT_BINARY) or
         ( ASAType and $FFFE = DT_VARCHAR) then
        PZASASQLSTRING( sqlData).length := 0;
    end;
    sqlType := ASAType;
    sqllen := Len;
    if SetDeclType then
    begin
      FDeclType[Index].sqlType := sqlType;
      FDeclType[Index].sqlLen := sqlLen;
    end;
  end;
end;

procedure TZASASQLDA.SetFieldType(const Index: Word; ASAType: Smallint;
  Len: LongWord; SetDeclType: Boolean = true);
begin
  SetFieldType(FSQLDA, Index, ASAType, Len, SetDeclType);
end;

constructor TZASASQLDA.Create(const PlainDriver: IZASAPlainDriver; Handle: PZASASQLCA;
   CursorName: PAnsiChar; ConSettings: PZConSettings; NumVars: Word = StdVars);
begin
  FPlainDriver := PlainDriver;
  FHandle := Handle;
  FCursorName := CursorName;
  AllocateSQLDA(NumVars);
  FConSettings := ConSettings;
  inherited Create;
end;

destructor TZASASQLDA.Destroy;
begin
  FreeSQLDA;
  inherited;
end;

{**
   Reallocate SQLDA to fields count length
   @param Value the count fields
}
procedure TZASASQLDA.AllocateSQLDA( NumVars: Word);
begin
  FreeSQLDA;
  FSQLDA := FPlainDriver.db_alloc_sqlda( NumVars);
  if not Assigned( FSQLDA) then CreateException( 'Not enough memory for SQLDA');
  SetLength(FDeclType, FSQLDA.sqln);
end;

{**
   Allocate memory for SQLVar in SQLDA structure for every
   fields by it length.
}
procedure TZASASQLDA.InitFields;
var
  i: Integer;
begin
  if Assigned( FSQLDA) then
  begin
    for i := 0 to FSQLDA.sqld-1 do
    begin
      FDeclType[i].sqlType := FSQLDA.sqlVar[i].sqlType;
      FDeclType[i].sqlLen := FSQLDA.sqlVar[i].sqlLen;
      case FSQLDA.sqlVar[i].sqlType and $FFFE of
        DT_DATE,
        DT_TIME,
        DT_TIMESTAMP:
                        begin
                          FSQLDA.sqlVar[i].sqlType := DT_TIMESTAMP_STRUCT +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := SizeOf( TZASASQLDateTime);
                        end;
        DT_DECIMAL:
                        begin
                          FSQLDA.sqlVar[i].sqlType := DT_DOUBLE +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := SizeOf( Double);
                        end;
        DT_STRING,
        DT_FIXCHAR,
        DT_VARCHAR,
        DT_LONGVARCHAR: if FSQLDA.sqlVar[i].sqlLen < MinBLOBSize then
                          FSQLDA.sqlVar[i].sqlType := DT_VARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001)
                        else
                        begin
                          FSQLDA.sqlVar[i].sqlType := DT_LONGVARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := 0;
                        end;
        DT_BINARY,
        DT_LONGBINARY:  if FSQLDA.sqlVar[i].sqlLen < MinBLOBSize then
                          FSQLDA.sqlVar[i].sqlType := DT_BINARY +
                            ( FSQLDA.sqlVar[i].sqlType and $0001)
                        else
                        begin
                          FSQLDA.sqlVar[i].sqlType := DT_LONGBINARY +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := 0;
                        end;
        DT_NSTRING,
        DT_NFIXCHAR,
        DT_NVARCHAR,
        DT_LONGNVARCHAR: if FSQLDA.sqlVar[i].sqlLen < MinBLOBSize then
                          FSQLDA.sqlVar[i].sqlType := DT_NVARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001)
                        else
                        begin
                          FSQLDA.sqlVar[i].sqlType := DT_LONGNVARCHAR +
                            ( FSQLDA.sqlVar[i].sqlType and $0001);
                          FSQLDA.sqlVar[i].sqlLen := 0;
                        end;
      end;
      SetFieldType( i, FSQLDA.sqlVar[i].sqlType, FSQLDA.sqlVar[i].sqlLen, False);
    end;
  end;
end;

{**
   Clear allocated data for SQLDA parameters
}
procedure TZASASQLDA.FreeSQLDA;
var
  i: integer;
begin
  if Assigned( FSQLDA) then
  begin
    for i := 0 to FSQLDA.sqln-1 do
    begin
      FSQLDA.sqlVar[i].sqlInd := nil;
      if Assigned( FSQLDA.sqlVar[i].sqlData) then
      begin
        FreeMem( FSQLDA.sqlVar[i].sqlData);
        FSQLDA.sqlVar[i].sqlData := nil;
      end;
    end;
    FPlainDriver.db_free_sqlda( FSQLDA);
    FSQLDA := nil;
  end;
  SetLength(FDeclType, 0);
  FDeclType := nil;
end;

{**
   Return pointer to SQLDA structure
}
function TZASASQLDA.GetData: PASASQLDA;
begin
  Result := FSQLDA;
end;

{**
   Indicate blob field
   @param Index the index fields
   @return true if blob field overwise false
}
function TZASASQLDA.IsBlob(const Index: Word): boolean;
begin
  Result := GetFieldSqlType( Index) in
    [ stAsciiStream, stUnicodeStream, stBinaryStream];
end;

{**
   Indicate nullable field
   @param Index the index fields
   @return true if field nullable overwise false
}
function TZASASQLDA.IsNullable(const Index: Word): boolean;
begin
  CheckIndex(Index);
  Result := FSQLDA.sqlvar[Index].sqlType and 1 = 1
end;

{**
   Get fields count not allocated.
   @return fields count
}
function TZASASQLDA.GetFieldCount: Integer;
begin
  if Assigned( FSQLDA) then
    Result := FSQLDA.sqld
  else
    Result := 0;
end;

{**
   Return Name for field
   @param Index the index fields
   @return the name
}
function TZASASQLDA.GetFieldName(const Index: Word): String;
begin
  CheckIndex(Index);
  {$IFDEF UNICODE}
  Result := PRawToUnicode(@FSQLDA.sqlvar[Index].sqlname.data[0],
    FSQLDA.sqlvar[Index].sqlname.length-1, FConSettings^.ClientCodePage^.CP);
  {$ELSE}
    if (not FConSettings^.AutoEncode) or ZCompatibleCodePages(FConSettings^.ClientCodePage^.CP, FConSettings^.CTRL_CP) then
      SetString(Result, PAnsiChar(@FSQLDA.sqlvar[Index].sqlname.data[0]), FSQLDA.sqlvar[Index].sqlname.length-1)
    else
      Result := ZUnicodeToString(PRawToUnicode(@FSQLDA.sqlvar[Index].sqlname.data[0],
        FSQLDA.sqlvar[Index].sqlname.length-1, FConSettings^.ClientCodePage^.CP), FConSettings^.CTRL_CP);
  {$ENDIF}
end;

{**
   Return field index by it name
   @param Index the index fields
   @return the index field
}
function TZASASQLDA.GetFieldIndex(const Name: String): Word;
var FieldName: String;
  P1, P2: PChar;
begin
  for Result := 0 to FSQLDA.sqld - 1 do begin
    FieldName := GetFieldName(Result);
    P1 := Pointer(Name);
    P2 := Pointer(FieldName);
    if Length(FieldName) = Length(name) then
      if StrLIComp(P1, P2, Length(name)) = 0 then
        Exit;
  end;
  CreateException( Format( SFieldNotFound1, [name]));
  Result := 0; // satisfy compiler
end;

{**
   Return field length
   @param Index the index fields
   @return the field lenth
}
function TZASASQLDA.GetFieldLength(const Index: Word): Word;
begin
  CheckIndex( Index);
  if FSQLDA.sqlvar[Index].sqlType and $FFFE <> DT_DECIMAL then
    Result := FSQLDA.sqlvar[Index].sqlLen
  else
    Result := (FSQLDA.sqlvar[Index].sqlLen and $FF) shr 1 + 1; //shr 1 = div 2 but faster
end;

{**
   Return field scale
   @param Index the index fields
   @return the field scale
}
function TZASASQLDA.GetFieldScale(const Index: Word): integer;
begin
  CheckIndex(Index);
  if FSQLDA.sqlvar[Index].sqlType and $FFFE <> DT_DECIMAL then
    Result := 0
  else
    Result := FSQLDA.sqlvar[Index].sqlLen div 256;
end;

{**
   Convert ASA sql type to SQLType
   @param Index the index fields
   @return the SQLType
}
function TZASASQLDA.GetFieldSqlType(const Index: Word): TZSQLType;
begin
  CheckIndex(Index);
  if FSQLDA.sqlvar[Index].sqlType and $FFFE <> DT_TIMESTAMP_STRUCT then
    Result := ConvertASATypeToSQLType(FSQLDA.sqlvar[Index].sqlType,
      FConSettings.CPType)
  else
    Result := ConvertASATypeToSQLType( FDeclType[Index].sqlType,
      FConSettings.CPType)
end;

{**
   Set up parameter null value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateNull(const Index: Integer; Value: Boolean);
begin
  CheckIndex( Index);
  with FSQLDA.sqlvar[ Index] do
  begin
    if not Assigned( sqlData) then
      SetFieldType( Index, DT_TINYINT or 1, SizeOf( Byte));
    if Value then
      sqlind^ := -1 //NULL
    else
      sqlind^ :=  0; //NOT NULL
  end;
end;

{**
   Set up parameter Boolean value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateBoolean(const Index: Integer; Value: boolean);
begin
  SetFieldType( Index, DT_BIT or 1, SizeOf( Byte));
  with FSQLDA.sqlvar[Index] do
  begin
    PByte(sqldata)^ := Ord(Value);
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Byte value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateByte(const Index: Integer; Value: Byte);
begin
  SetFieldType( Index, DT_BIT or 1, SizeOf( Byte));
  with FSQLDA.sqlvar[Index] do
  begin
    PByte(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Byte value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateShort(const Index: Integer; Value: ShortInt);
begin
  SetFieldType( Index, DT_TINYINT or 1, SizeOf(ShortInt));
  with FSQLDA.sqlvar[Index] do
  begin
    PShortInt(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter short value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateSmall(const Index: Integer; Value: SmallInt);
begin
  SetFieldType( Index, DT_SMALLINT or 1, SizeOf( SmallInt));
  with FSQLDA.sqlvar[Index] do
  begin
    PSmallInt(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter short value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateWord(const Index: Integer; Value: Word);
begin
  SetFieldType( Index, DT_UNSSMALLINT or 1, SizeOf(Word));
  with FSQLDA.sqlvar[Index] do
  begin
    PWord(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter integer value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateInt(const Index: Integer; Value: Integer);
begin
  SetFieldType( Index, DT_INT or 1, SizeOf( Integer));
  with FSQLDA.sqlvar[Index] do
  begin
    PInteger(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter integer value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateUInt(const Index: Integer; Value: LongWord);
begin
  SetFieldType( Index, DT_UNSINT or 1, SizeOf(LongWord));
  with FSQLDA.sqlvar[Index] do
  begin
    PLongWord(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Long value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateLong(const Index: integer; Value: Int64);
begin
  SetFieldType( Index, DT_BIGINT or 1, SizeOf( Int64));
  with FSQLDA.sqlvar[Index] do
  begin
    PInt64(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Long value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateULong(const Index: integer; Value: UInt64);
begin
  SetFieldType( Index, DT_UNSBIGINT or 1, SizeOf(UInt64));
  with FSQLDA.sqlvar[Index] do
  begin
    PUInt64(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Float value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateFloat(const Index: Integer; Value: Single);
begin
  SetFieldType( Index, DT_FLOAT or 1, SizeOf( Single));
  with FSQLDA.sqlvar[Index] do
  begin
    PSingle(sqldata)^ := Trunc(Value);
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Double value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateDouble(const Index: Integer; Value: Double);
begin
  SetFieldType( Index, DT_DOUBLE or 1, SizeOf( Double));
  with FSQLDA.sqlvar[Index] do
  begin
    PDouble(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter BigDecimal value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateBigDecimal(const Index: Integer; Value: Extended);
begin
  SetFieldType( Index, DT_DOUBLE or 1, SizeOf( Double));
  with FSQLDA.sqlvar[Index] do
  begin
    PDouble(sqldata)^ := Value;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter String value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdatePRaw(const Index: Integer; Value: PAnsiChar; Len: NativeUInt);
begin
  with FSQLDA.sqlvar[Index] do
  begin
    if Len < MinBLOBSize then
    begin
      SetFieldType( Index, DT_VARCHAR or 1, MinBLOBSize - 1);
      PZASASQLSTRING( sqlData).length := Min(Len, sqllen-3);
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, PZASASQLSTRING( sqlData).data[0], PZASASQLSTRING( sqlData).length);
      AnsiChar((PAnsiChar(@PZASASQLSTRING( sqlData).data[0])+PZASASQLSTRING( sqlData).length)^) := AnsiChar(#0);
    end
    else
    begin
      SetFieldType( Index, DT_LONGVARCHAR or 1, Len);
      PZASABlobStruct( sqlData).array_len := Len;
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value^, PZASABlobStruct( sqlData).arr[0], Len);
      PZASABlobStruct( sqlData).stored_len := Len;
      PZASABlobStruct( sqlData).untrunc_len := Len;
    end;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter byte value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateBytes(const Index: Integer; const Value: TBytes);
var
  BlobSize: Integer;
begin
  BlobSize := Length( Value);
  with FSQLDA.sqlvar[Index] do
  begin
    if BlobSize < MinBLOBSize then
    begin
      SetFieldType( Index, DT_BINARY or 1, MinBLOBSize - 1);
      PZASASQLSTRING( sqlData).length := BlobSize;
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move( Pointer(Value)^, PZASASQLSTRING( sqlData).data[0], BlobSize);
    end
    else
    begin
      SetFieldType( Index, DT_LONGBINARY or 1, BlobSize);
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move( Pointer(Value)^, PZASABlobStruct( sqlData).arr[0], BlobSize);
      PZASABlobStruct( sqlData).stored_len := BlobSize;
      PZASABlobStruct( sqlData).untrunc_len := BlobSize;
    end;
    if (sqlind <> nil) then
      sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Date value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateDate(const Index: Integer; Value: TDateTime);
begin
  UpdateDateTime(Index, Value);
  FDeclType[Index].sqlType := DT_DATE;
end;

{**
   Set up parameter Time value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateTime(const Index: Integer; Value: TDateTime);
begin
  UpdateDateTime(Index, Value);
  FDeclType[Index].sqlType := DT_TIME;
end;

{**
   Set up parameter DateTime value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateDateTime(const Index: Integer;
  Value: TDateTime);
var
  y, m, d: word;
  hr, min, sec, msec: word;
begin
  SetFieldType( Index, DT_TIMESTAMP_STRUCT or 1, SizeOf( TZASASQLDateTime));
  with FSQLDA.sqlvar[Index] do
  begin
    DecodeDate( Value, y, m, d);
    DecodeTime( Value, hr, min, sec, msec);
    PZASASQLDateTime( sqlData).Year := y;
    PZASASQLDateTime( sqlData).Month := m - 1;
    PZASASQLDateTime( sqlData).Day := d;
    PZASASQLDateTime( sqlData).Hour := hr;
    PZASASQLDateTime( sqlData).Minute := min;
    PZASASQLDateTime( sqlData).Second := sec;
    PZASASQLDateTime( sqlData).MicroSecond :=
      msec * 1000;
    PZASASQLDateTime( sqlData).Day_of_Week := 0;
    PZASASQLDateTime( sqlData).Day_of_Year := 0;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Set up parameter Timestamp value
   @param Index the target parameter index
   @param Value the source value
}
procedure TZASASQLDA.UpdateTimestamp(const Index: Integer; Value: TDateTime);
begin
  UpdateDateTime(Index, Value);
  FDeclType[Index].sqlType := DT_TIMESTAMP;
end;

{**
   Write stream to blob field
   @param Index an index field number
   @param Stream the souse data stream
}
procedure TZASASQLDA.WriteBlob(const Index: Integer; Stream: TStream;
  const BlobType: TZSQLType);
var
  BlobSize: Integer;
begin
  stream.Position := 0;
  BlobSize := stream.Size;
  case BlobType of
    stAsciiStream:   SetFieldType( Index, DT_LONGVARCHAR or 1, BlobSize);
    stUnicodeStream: SetFieldType( Index, DT_LONGNVARCHAR or 1, BlobSize);
    stBinaryStream:  SetFieldType( Index, DT_LONGBINARY or 1, BlobSize);
    else
      CreateException( SUnsupportedParameterType);
  end;
  with FSQLDA.sqlvar[Index] do
  begin
    case sqlType and $FFFE of
      DT_LONGVARCHAR, DT_LONGNVARCHAR,
      DT_LONGBINARY:
        begin
          stream.ReadBuffer( PZASABlobStruct( sqlData).arr[0], BlobSize);
          stream.Position := 0;
          PZASABlobStruct( sqlData).stored_len := BlobSize;
          PZASABlobStruct( sqlData).untrunc_len := BlobSize;
        end;
    else
      CreateException( SUnsupportedParameterType);
    end;
    if (sqlind <> nil) then
       sqlind^ := 0; // not null
  end;
end;

{**
   Indicate field null
   @param Index the field index
   @return true if fied value NULL overwise false
}
function TZASASQLDA.IsNull(const Index: Integer): Boolean;
begin
  CheckIndex( Index);
  with FSQLDA.sqlvar[Index] do
    Result := Assigned( sqlind) and (sqlind^ < 0);
end;

{**
   Indicate sqldata assigned
   @param Index the field index
   @return true if assigned field data
}
function TZASASQLDA.IsAssigned(const Index: Integer): Boolean;
begin
  CheckIndex( Index);
  with FSQLDA.sqlvar[Index] do
    Result := Assigned( sqldata);
end;

procedure TZASASQLDA.ReadBlob(const Index: Word; var Buffer: Pointer;
  Length: LongWord);
var
  TempSQLDA: PASASQLDA;
  Offs, Rd: LongWord;
const
  BlockSize = 32700;
begin
  with FSQLDA.sqlvar[Index] do
  begin
    if ( ( sqlType and $FFFE = DT_LONGVARCHAR) or
         ( sqlType and $FFFE = DT_LONGNVARCHAR) or
         ( sqlType and $FFFE = DT_LONGBINARY)) and
       ( PZASABlobStruct( sqlData).array_len > 0) then
    begin
      Assert( PZASABlobStruct( sqlData).array_len = PZASABlobStruct( sqlData).untrunc_len,
        'Blob Record is not correctly initialized');
      if PZASABlobStruct( sqlData).array_len <> Length then
        CreateException( 'Could''nt complete BLOB-Read');
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move( PZASABlobStruct( sqlData).arr[0], Buffer, PZASABlobStruct( sqlData).array_len);
    end
    else
    begin
      TempSQLDA := FPlainDriver.db_alloc_sqlda( 1);
      if not Assigned( TempSQLDA) then
        CreateException( 'Not enough memory for SQLDA');
      try
        with TempSQLDA.sqlvar[ 0] do
        begin
          case Self.GetFieldSqlType(Index) of
            stAsciiStream:
              SetFieldType(TempSQLDA, 0, DT_LONGVARCHAR, Min( Int64(BlockSize), Int64(Length)));
            stUnicodeStream:
              SetFieldType(TempSQLDA, 0, DT_LONGNVARCHAR, Min( Int64(BlockSize), Int64(Length)));
            stBinaryStream:
              SetFieldType(TempSQLDA, 0, DT_LONGBINARY, Min( Int64(BlockSize), Int64(Length)));
            else
              sqlType := DT_FIXCHAR;
          end;
          sqlname.length := 0;
          sqlname.data[0] := AnsiChar(#0);
          TempSQLDA.sqld := TempSQLDA.sqln;

          Offs := 0;
          Rd := 0;

          while True do begin
            FPlainDriver.db_get_data(FHandle, FCursorName, Index + 1, Offs, TempSQLDA);
            CheckASAError( FPlainDriver, FHandle, lcOther, FConSettings);
            if ( sqlind^ < 0 ) then
              break;
            Inc( Rd, PZASABlobStruct( sqlData)^.stored_len);
            if Offs = 0 then ReallocMem(Buffer, PZASABlobStruct( sqlData)^.untrunc_len+Byte(ORd(sqlType and $FFFE <> DT_LONGBINARY))); //keep 1 byte for trailing #0 term
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move((PZASABlobStruct( sqlData)^.arr[0]), (PAnsiChar(Buffer)+Offs)^, PZASABlobStruct( sqlData)^.stored_len);
            if ( sqlind^ = 0 ) or ( RD = Length) then
              break;
            Inc( Offs, PZASABlobStruct( sqlData)^.stored_len);
            sqllen := Min( Int64(BlockSize), Int64(Length-Rd));
          end;
          if Rd <> Length then
            CreateException( 'Could''nt complete BLOB-Read');
          FreeMem(sqlData);
          FPlainDriver.db_free_sqlda( TempSQLDA);
          TempSQLDA := nil;
        end;
      except
        if Assigned( TempSQLDA) then
          FPlainDriver.db_free_sqlda( TempSQLDA);
        raise;
      end;
    end;
  end;
end;

{**
   Read blob data to Buffer
   @param Index an filed index
   @param Str destination string
}
procedure TZASASQLDA.ReadBlobToMem(const Index: Word; out Buffer: Pointer;
  out Length: NativeUInt; const Binary: Boolean);
begin
  CheckRange(Index);
  Buffer := nil;
  Length := 0;
  with FSQLDA.sqlvar[Index] do
  begin
    Length := 0;
    if (sqlind^ < 0) then
       Exit;

    if ( ( sqlType and $FFFE = DT_LONGVARCHAR) or
         ( sqlType and $FFFE = DT_LONGNVARCHAR) or
         ( sqlType and $FFFE = DT_LONGBINARY)) then
    begin
      Length := PZASABlobStruct( sqlData).untrunc_len;
      GetMem( Buffer, NativeInt(Length)+Ord(not Binary));
      if Length = 0 then
        Exit;
      ReadBlob( Index, Buffer, Length);
    end
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
  end;
end;

{**
   Read blob data to string
   @param Index an filed index
   @param Str destination string
}
procedure TZASASQLDA.ReadBlobToString(const Index: Word; out Str: RawByteString);
var Buffer: Pointer;
begin
  CheckRange(Index);
  with FSQLDA.sqlvar[Index] do
  begin
    Str := '';
    if (sqlind^ < 0) then
       Exit;

    if sqlType and $FFFE = DT_LONGVARCHAR then
    begin
      GetMem(Buffer, PZASABlobStruct( sqlData).untrunc_len);
      SetLength( Str, PZASABlobStruct( sqlData).untrunc_len);
      ReadBlob(Index, Buffer, Length(Str));
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(Str)^, PZASABlobStruct( sqlData).untrunc_len);
      FreeMem(buffer);
    end
    else
      CreateException( Format( SErrorConvertionField,
        [ GetFieldName(Index), ConvertASATypeToString( sqlType)]));
  end;
end;

{**
  Converts a ASA native types into ZDBC SQL types.
  @param SQLType Field of TASASQLVar structure.
  @return a SQL undepended type.
}
function ConvertASATypeToSQLType(const SQLType: SmallInt;
  const CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  case SQLType and $FFFE of
    DT_NOTYPE:
      Result := stUnknown;
    DT_SMALLINT:
      Result := stSmall;
    DT_INT:
      Result := stInteger;
    DT_DECIMAL:
      Result := stDouble; //BCD Fields not supported
    DT_FLOAT:
      Result := stFloat;
    DT_DOUBLE:
      Result := stDouble;
    DT_DATE:
      Result := stDate;
    DT_VARIABLE, DT_STRING, DT_FIXCHAR, DT_VARCHAR, DT_NSTRING, DT_NFIXCHAR, DT_NVARCHAR:
      if (CtrlsCPType = cCP_UTF16) then
        Result := stUnicodeString
      else
        Result := stString;
    DT_LONGVARCHAR, DT_LONGNVARCHAR:
      if (CtrlsCPType = cCP_UTF16) then
        Result := stUnicodeStream
      else
        Result := stAsciiStream;
    DT_TIME:
      Result := stTime;
    DT_TIMESTAMP:
      Result := stTimestamp;
    DT_TIMESTAMP_STRUCT:
      Result := stTimestamp;
    DT_BINARY:
      Result := stBytes;
    DT_LONGBINARY:
      Result := stBinaryStream;
    DT_TINYINT:
      Result := stByte;
    DT_BIGINT:
      Result := stLong;
    DT_UNSINT:
      Result := stInteger;
    DT_UNSSMALLINT:
      Result := stSmall;
    DT_UNSBIGINT:
      Result := stLong;
    DT_BIT:
      Result := stBoolean;
  else
    Result := stUnknown;
  end;
end;

{**
  Converts a ASA native type into String.
  @param SQLType Field of TASASQLVar structure.
  @return type description.
}
function ConvertASATypeToString( SQLType: SmallInt): String;
begin
  case SQLType and $FFFE of
    DT_SMALLINT:
      Result := 'DT_SMALLINT';
    DT_INT:
      Result := 'DT_INT';
    DT_DECIMAL:
      Result := 'DT_DECIMAL'; //BCD Fields not supported
    DT_FLOAT:
      Result := 'DT_FLOAT';
    DT_DOUBLE:
      Result := 'DT_DOUBLE';
    DT_DATE:
      Result := 'DT_DATE';
    DT_VARIABLE:
      Result := 'DT_VARIABLE';
    DT_STRING:
      Result := 'DT_STRING';
    DT_FIXCHAR:
      Result := 'DT_FIXCHAR';
    DT_VARCHAR:
      Result := 'DT_VARCHAR';
    DT_LONGVARCHAR:
      Result := 'DT_LONGVARCHAR';
    DT_TIME:
      Result := 'DT_TIME';
    DT_TIMESTAMP:
      Result := 'DT_TIMESTAMP';
    DT_TIMESTAMP_STRUCT:
      Result := 'DT_TIMESTAMP_STRUCT';
    DT_BINARY:
      Result := 'DT_BINARY';
    DT_LONGBINARY:
      Result := 'DT_LONGBINARY';
    DT_TINYINT:
      Result := 'DT_TINYINT';
    DT_BIGINT:
      Result := 'DT_BIGINT';
    DT_UNSINT:
      Result := 'DT_UNSINT';
    DT_UNSSMALLINT:
      Result := 'DT_UNSSMALLINT';
    DT_UNSBIGINT:
      Result := 'DT_UNSBIGINT';
    DT_BIT:
      Result := 'DT_BIT';
    DT_NSTRING:
      Result := 'DT_NSTRING';
    DT_NFIXCHAR:
      Result := 'DT_NFIXCHAR';
    DT_NVARCHAR:
      Result := 'DT_NVARCHAR';
    DT_LONGNVARCHAR:
      Result := 'DT_LONGNVARCHAR';
  else
    Result := 'Unknown';
  end;
end;

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertASAJDBCToSqlType(const FieldType: SmallInt;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  case FieldType of
    1, 12, -8, -9:
      if (CtrlsCPType = cCP_UTF16) then
        Result := stUnicodeString
      else
        Result := stString;
    -7: Result := stBoolean;
    -6: Result := stByte;
    5: Result := stSmall;
    4: Result := stInteger;
    -5 : Result := stLong;
    6, 7, 8: Result := stDouble;
    2, 3: Result := stDouble;  //BCD Feld
    11, 93: Result := stTimestamp;
    -1, -10:
      if (CtrlsCPType = cCP_UTF16) then
        Result := stUnicodeStream
      else
        Result := stAsciiStream;
    -4, -11, 1111: Result := stBinaryStream;
    -3, -2: Result := stBytes;
    92: Result := stTime;
    91: Result := stDate;
  else
    Result := stUnknown;
  end;
end;
{
procedure TSQLTimeStampToASADateTime( DT: TSQLTimeStamp; const ASADT: PZASASQLDateTime);
begin
  ASADT.Year := DT.Year;
  ASADT.Month := DT.Month - 1;
  ASADT.Day := DT.Day;
  ASADT.Hour := DT.Hour;
  ASADT.Minute := DT.Minute;
  ASADT.Second := DT.Second;
  ASADT.MicroSecond := DT.Fractions * 10;
  ASADT.Day_of_Week := 0;
  ASADT.Day_of_Year := 0;
end;

function ASADateTimeToSQLTimeStamp( ASADT: PZASASQLDateTime): TSQLTimeStamp;
begin
  DT.Year := ASADT.Year;
  DT.Month := ASADT.Month + 1;
  DT.Day := ASADT.Day;
  DT.Hour := ASADT.Hour;
  DT.Minute := ASADT.Minute;
  DT.Second := ASADT.Second;
  DT.Fractions := ASADT.MicroSecond div 10;
end;
}
{**
  Checks for possible sql errors.
  @param PlainDriver a MySQL plain driver.
  @param Handle a MySQL connection handle.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckASAError(const PlainDriver: IZASAPlainDriver;
  const Handle: PZASASQLCA; const LogCategory: TZLoggingCategory;
  const ConSettings: PZConSettings; const LogMessage: RawByteString = '';
  const SupressExceptionID: Integer = 0);
var
  ErrorBuf: array[0..1024] of AnsiChar;
  ErrorMessage: RawByteString;
  P: PAnsiChar;
begin
  if Handle.SqlCode < SQLE_NOERROR then
  begin
    P := PlainDriver.sqlError_Message( Handle, @ErrorBuf[0], SizeOf( ErrorBuf));
    ZSetString(P, StrLen(P), ErrorMessage);
    //SyntaxError Position in SQLCount
    if not (SupressExceptionID = Handle.SqlCode ) then
    begin
      DriverManager.LogError( LogCategory, ConSettings^.Protocol, LogMessage,
        Handle.SqlCode, ErrorMessage);

      raise EZSQLException.CreateWithCode( Handle.SqlCode,
        Format(SSQLError1, [ConSettings^.ConvFuncs.ZRawToString(ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)]));
    end;
  end;
end;

{**
  Create CachedResultSet with using TZCachedResultSet and return it.
  @param SQL a sql query command
  @param Statement a zeos statement object
  @param NativeResultSet a native result set
  @return cached ResultSet
}
function GetCachedResultSet(const SQL: string;
  const Statement: IZStatement; const NativeResultSet: IZResultSet): IZResultSet;
var
  CachedResultSet: TZCachedResultSet;
begin
  if (Statement.GetResultSetConcurrency <> rcReadOnly)
    or (Statement.GetResultSetType <> rtForwardOnly) then
  begin
    CachedResultSet := TZCachedResultSet.Create( NativeResultSet, SQL, nil,
      Statement.GetConnection.GetConSettings);
    CachedResultSet.SetResolver( TZASACachedResolver.Create(
      Statement, NativeResultSet.GetMetadata));
    CachedResultSet.SetConcurrency( Statement.GetResultSetConcurrency);
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

procedure DescribeCursor(const FASAConnection: IZASAConnection; const FSQLData: IZASASQLDA;
  const Cursor: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}; const SQL: RawByteString);
begin
  //FSQLData.AllocateSQLDA( StdVars);
  with FASAConnection do
  begin
    GetPlainDriver.db_describe_cursor(GetDBHandle, Pointer(Cursor), FSQLData.GetData, SQL_DESCRIBE_OUTPUT);
    ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, FASAConnection.GetConSettings, SQL);
    if FSQLData.GetData^.sqld <= 0 then
      raise EZSQLException.Create( SCanNotRetrieveResultSetData)
    else if ( FSQLData.GetData^.sqld > FSQLData.GetData^.sqln) then
    begin
      FSQLData.AllocateSQLDA( FSQLData.GetData^.sqld);
      GetPlainDriver.db_describe_cursor(GetDBHandle, PAnsiChar(Cursor), FSQLData.GetData, SQL_DESCRIBE_OUTPUT);
       ZDbcASAUtils.CheckASAError(GetPlainDriver, GetDBHandle, lcExecute, FASAConnection.GetConSettings, SQL);
    end;
    FSQLData.InitFields;
  end;
end;

procedure ASAPrepare(const FASAConnection: IZASAConnection; const FSQLData, FParamsSQLData: IZASASQLDA;
   const SQL: RawByteString; StmtNum: PSmallInt; var FPrepared, FMoreResults: Boolean);
begin
  with FASAConnection do
  begin
    if FPrepared then
    begin
      FParamsSQLData.AllocateSQLDA( StdVars);
      FSQLData.AllocateSQLDA( StdVars);
      if StmtNum^ <> 0 then
      begin
        GetPlainDriver.db_dropstmt( GetDBHandle, nil, nil, StmtNum);
        StmtNum^ := 0;
      end;
    end;
    try
      GetPlainDriver.db_prepare_describe( GetDBHandle, nil, StmtNum, Pointer(SQL),
        FParamsSQLData.GetData, SQL_PREPARE_DESCRIBE_STMTNUM +
          SQL_PREPARE_DESCRIBE_INPUT + SQL_PREPARE_DESCRIBE_VARRESULT, 0);
      ZDbcASAUtils.CheckASAError(GetPlainDriver, GetDBHandle, lcExecute, GetConSettings, SQL);

      FMoreResults := GetDBHandle.sqlerrd[2] = 0;

      if FParamsSQLData.GetData^.sqld > FParamsSQLData.GetData^.sqln then
      begin
        FParamsSQLData.AllocateSQLDA( FParamsSQLData.GetData^.sqld);
        GetPlainDriver.db_describe( GetDBHandle, nil, StmtNum,
          FParamsSQLData.GetData, SQL_DESCRIBE_INPUT);
        ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, GetConSettings, SQL);
      end;

      if not FMoreResults then
      begin
        GetPlainDriver.db_describe( GetDBHandle, nil, StmtNum,
          FSQLData.GetData, SQL_DESCRIBE_OUTPUT);
        ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, GetConSettings, SQL);
        if FSQLData.GetData^.sqld > FSQLData.GetData^.sqln then
        begin
          FSQLData.AllocateSQLDA( FSQLData.GetData^.sqld);
          GetPlainDriver.db_describe( GetDBHandle, nil, StmtNum,
            FSQLData.GetData, SQL_DESCRIBE_OUTPUT);
          ZDbcASAUtils.CheckASAError( GetPlainDriver, GetDBHandle, lcExecute, GetConSettings, SQL);
        end;
        FSQLData.InitFields;
      end;

      FPrepared := true;
      { Logging SQL Command }
      DriverManager.LogMessage( lcExecute, GetConSettings.Protocol,
        'Prepare: '+ SQL);
    except
      on E: Exception do
      begin
        if StmtNum^ <> 0 then
          GetPlainDriver.db_dropstmt( GetDBHandle, nil, nil, StmtNum);
        raise;
      end;
    end;
  end;
end;

procedure PrepareParameters(const ClientVarManager: IZClientVariantManager;
  const InParamValues: TZVariantDynArray; const InParamTypes: TZSQLTypeArray;
  InParamCount: Integer; const ParamSqlData: IZASASQLDA; ConSettings: PZConSettings);
var
  i: Integer;
  TempBlob: IZBlob;
  TempStream: TStream;
  CharRec: TZCharRec;
  {$IFDEF NO_ANSISTRING}
  Raw: RawByteString;
  {$ENDIF}
begin
  if InParamCount <> ParamSqlData.GetFieldCount then
    raise EZSQLException.Create( SInvalidInputParameterCount);
  for i := 0 to ParamSqlData.GetFieldCount-1 do
    if ClientVarManager.IsNull( InParamValues[i])then
      ParamSqlData.UpdateNull( i, True)
    else
      case InParamTypes[i] of
        stBoolean:
          ParamSqlData.UpdateBoolean( i,
            ClientVarManager.GetAsBoolean( InParamValues[i]));
        stByte:
          ParamSqlData.UpdateByte( i,
            ClientVarManager.GetAsInteger( InParamValues[i]));
        stShort, stSmall:
          ParamSqlData.UpdateSmall( i,
            ClientVarManager.GetAsInteger( InParamValues[i]));
        stInteger:
          ParamSqlData.UpdateInt( i,
            ClientVarManager.GetAsInteger( InParamValues[i]));
        stLong:
          ParamSqlData.UpdateLong( i,
            ClientVarManager.GetAsInteger( InParamValues[i]));
        stFloat:
          ParamSqlData.UpdateFloat( i,
            ClientVarManager.GetAsFloat( InParamValues[i]));
        stDouble:
          ParamSqlData.UpdateDouble( i,
            ClientVarManager.GetAsFloat( InParamValues[i]));
        stBigDecimal:
          ParamSqlData.UpdateBigDecimal( i,
            ClientVarManager.GetAsFloat( InParamValues[i]));
        stString, stUnicodeString:
          begin
            CharRec := ClientVarManager.GetAsCharRec( InParamValues[i], ConSettings^.ClientCodePage^.CP);
            ParamSqlData.UpdatePRaw( i, CharRec.P, CharRec.Len);
          end;
        stBytes:
          ParamSqlData.UpdateBytes( i, ClientVarManager.GetAsBytes( InParamValues[i]));
        stDate:
          ParamSqlData.UpdateDate( i,
            ClientVarManager.GetAsDateTime( InParamValues[i]));
        stTime:
          ParamSqlData.UpdateTime( i,
            ClientVarManager.GetAsDateTime( InParamValues[i]));
        stTimestamp:
          ParamSqlData.UpdateTimestamp( i,
            ClientVarManager.GetAsDateTime( InParamValues[i]));
        stAsciiStream,
        stUnicodeStream,
        stBinaryStream:
          begin
            TempBlob := ClientVarManager.GetAsInterface(InParamValues[I]) as IZBlob;
            if not TempBlob.IsEmpty then
            begin
              if (InParamTypes[i] in [stUnicodeStream, stAsciiStream]) then
                if TempBlob.IsClob then
                  TempStream := TempBlob.GetRawByteStream
                else
                {$IFDEF NO_ANSISTRING}
                begin
                  Raw := GetValidatedAnsiStringFromBuffer(
                    TempBlob.GetBuffer, TempBlob.Length, ConSettings);
                  TempStream := StreamFromData(Pointer(Raw), Length(Raw){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
                end
                {$ELSE}
                  TempStream := TStringStream.Create(GetValidatedAnsiStringFromBuffer(
                    TempBlob.GetBuffer, TempBlob.Length, ConSettings))
                {$ENDIF}
              else
                TempStream := TempBlob.GetStream;
              if Assigned(TempStream) then
              begin
                ParamSqlData.WriteBlob(I, TempStream, InParamTypes[i]);
                TempStream.Free;
              end;
            end;
          end;
        else
          RaiseUnsupportedParameterTypeException(InParamTypes[i]);
    end;
end;

{**
   Generate specific length random string and return it
   @param Len a length result string
   @return random string
}
function RandomString( Len: integer): string;
begin
  Result := '';
  while Length( Result) < Len do
    Result := Result + ZFastCode.IntToStr(Random(High(Integer)));
  if Length( Result) > Len then
    Result := Copy( Result, 1, Len);
end;

{$ENDIF ZEOS_DISABLE_ASA}
end.

