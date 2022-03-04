{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6ResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  ZDbcIntfs, ZDbcResultSet, ZDbcInterbase6, ZPlainFirebirdInterbaseConstants,
  ZPlainFirebirdDriver, ZCompatibility, ZDbcResultSetMetadata, ZMessages,
  ZDbcInterbase6Utils, ZSelectSchema;

type
  {** Implements Interbase ResultSet. }
  TZInterbase6XSQLDAResultSet = class(TZAbstractResultSet)
  private
    FCachedBlob: boolean;
    FFetchStat: Integer;
    FStmtHandle: TISC_STMT_HANDLE;
    FStmtHandleAddr: PISC_STMT_HANDLE;
    FXSQLDA: PXSQLDA;
    FIZSQLDA: IZSQLDA;
    FPISC_DB_HANDLE: PISC_DB_HANDLE;
    FPlainDriver: IZInterbasePlainDriver;
    FDialect: Word;
    FStmtType: TZIbSqlStatementType;
    FIBConnection: IZInterbase6Connection;
    FIBTransaction: IZIBTransaction;
    FIsMetadataResultSet: Boolean;
    FClientCP: Word;
    FCodePageArray: TWordDynArray;
    FBlobTemp: IZBlob;
    function GetIbSqlSubType(const Index: Word): Smallint; {$IF defined(WITH_INLINE) and not (defined(WITH_URW1135_ISSUE) or defined(WITH_URW1111_ISSUE))} inline; {$IFEND}
    function GetQuad(ColumnIndex: Integer): TISC_QUAD;
    procedure RegisterCursor;
    procedure DeRegisterCursor;
    function CreateIBConvertError(ColumnIndex: Integer; DataType: ISC_SHORT): EZIBConvertError;
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    TransactionResultSet: Pointer; //EH: this field is a weak resultset reference
      //it may be an address of a cached resultset which owns this instance.
      //this pointer should be registered as open cursor on the TA
      //aim is: if a transaction commit is called the TA checks if
      //all open resultsets a scollable. If so a fetchall will be done by TA.
      //finally the TA can commit the handle (i.e. changing the AutoCommit mode)
      //which would ususally close all pending cursors
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      StatementHandleAddr: PISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
      CachedBlob: boolean; StmtType: TZIbSqlStatementType);

    procedure AfterClose; override;
    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetString(ColumnIndex: Integer): String; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetCurrency(ColumnIndex: Integer): Currency; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString; override;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    {$ENDIF}
    function GetBlob(ColumnIndex: Integer): IZBlob; override;
    function Next: Boolean; override;
  end;

  {** Implements external blob wrapper object for Intebase/Firbird. }
  TZInterbase6UnCachedBlob = Class(TZAbstractUnCachedBlob, IZUnCachedLob)
  private
    FBlobId: TISC_QUAD;
    FPlainDriver: IZInterbasePlainDriver;
    FTransaction: IZIBTransaction;
    FIBConnection: IZInterbase6Connection;
  protected
    procedure ReadLob; override;
  public
    constructor Create(const PlainDriver: IZInterbasePlainDriver;
      var BlobId: TISC_QUAD; const Transaction: IZIBTransaction;
      const Connection: IZInterbase6Connection);
    procedure BeforeDestruction; override;
  end;

  TZInterbase6UnCachedClob = Class(TZAbstractUnCachedClob, IZUnCachedLob)
  private
    FBlobId: TISC_QUAD;
    FPlainDriver: IZInterbasePlainDriver;
    FIBConnection: IZInterbase6Connection;
    FTransaction: IZIBTransaction;
  protected
    procedure ReadLob; override;
  public
    constructor Create(const PlainDriver: IZInterbasePlainDriver;
      var BlobId: TISC_QUAD; const Transaction: IZIBTransaction;
      const Connection: IZInterbase6Connection);
    procedure BeforeDestruction; override;
  end;

  {** Implements Interbase ResultSetMetadata object. }
  TZInterbaseResultSetMetadata = Class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    procedure LoadColumns; override;
    procedure SetColumnPrecisionFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
  public
    function GetCatalogName({%H-}ColumnIndex: Integer): string; override;
    function GetColumnName(ColumnIndex: Integer): string; override;
    function GetSchemaName({%H-}ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsAutoIncrement({%H-}ColumnIndex: Integer): Boolean; override;
  End;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
  ZEncoding, ZFastCode, ZSysUtils, ZClasses, ZTokenizer,
  ZGenericSqlAnalyser,
  ZDbcMetadata, ZDbcLogging;

procedure GetPCharFromTextVar(SQLCode: SmallInt; sqldata: Pointer; sqllen: Short; out P: PAnsiChar; out Len: NativeUInt); {$IF defined(WITH_INLINE)} inline; {$IFEND}
begin
  case SQLCode of
    SQL_TEXT:
      begin
        P := sqldata;
        // Trim only trailing spaces. TrimRight also removes other characters)
        Len := sqllen;
        if Len > 0 then while AnsiChar((P + Len - 1)^) = AnsiChar(' ') do Dec(Len);
      end;
    SQL_VARYING:
      begin
        P := @PISC_VARYING(sqldata).str[0];
        Len := PISC_VARYING(sqldata).strlen;
      end;
    else // should not happen
      begin
        P := nil;
        Len := 0;
      end;
  end;
end;

function GetRawFromTextVar(SQLCode: SmallInt; sqldata: Pointer; sqllen: Short): RawByteString; {$IF defined(WITH_INLINE)} inline; {$IFEND}
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
  GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
  if Len > 0 then
    ZSetString(P, Len, Result{%H-})
  else
    Result := '';
end;

{ TZInterbase6XSQLDAResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
  @param the sql out data previously allocated
  @param the Interbase sql dialect
}
constructor TZInterbase6XSQLDAResultSet.Create(const Statement: IZStatement;
  const SQL: string; StatementHandleAddr: PISC_STMT_HANDLE; const XSQLDA: IZSQLDA;
  CachedBlob: Boolean; StmtType: TZIbSqlStatementType);
begin
  inherited Create(Statement, SQL, TZInterbaseResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);

  FFetchStat := 0;
  FIZSQLDA := XSQLDA; //localize the interface to avoid automatic free the object
  FXSQLDA := XSQLDA.GetData; // localize buffer for fast access

  FCachedBlob := CachedBlob;
  FIBConnection := Statement.GetConnection as IZInterbase6Connection;
  FPISC_DB_HANDLE := FIBConnection.GetDBHandle;
  FPlainDriver := FIBConnection.GetIZPlainDriver as IZInterbasePlainDriver;
  FDialect := FIBConnection.GetDialect;
  FStmtType := StmtType; //required to know how to fetch the columns for ExecProc

  FStmtHandle := StatementHandleAddr^;
  FStmtHandleAddr := StatementHandleAddr;
  ResultSetType := rtForwardOnly;
  ResultSetConcurrency := rcReadOnly;
  FIsMetadataResultSet := (ConSettings.ClientCodePage.ID = CS_NONE) and
    (Statement.GetParameters.Values[DS_Props_IsMetadataResultSet] = 'True');

  FCodePageArray := FPlainDriver.GetCodePageArray;
  FClientCP := ConSettings^.ClientCodePage^.CP;
  FCodePageArray[ConSettings^.ClientCodePage^.ID] := FClientCP; //reset the cp if user wants to wite another encoding e.g. 'NONE' or DOS852 vc WIN1250
  Open;
end;

function TZInterbase6XSQLDAResultSet.CreateIBConvertError(
  ColumnIndex: Integer; DataType: ISC_SHORT): EZIBConvertError;
begin
  Result := EZIBConvertError.Create(Format(SErrorConvertionField,
        [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(DataType and not(1))]));
end;

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZInterbase6XSQLDAResultSet.AfterClose;
begin
  { Free output allocated memory }
  FXSQLDA := nil;
  FIZSQLDA := nil;
  FStmtHandle := 0; //don't forget!
  inherited AfterClose;
end;


{$IFNDEF NO_ANSISTRING}
function TZInterbase6XSQLDAResultSet.GetAnsiString(
  ColumnIndex: Integer): AnsiString;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
  CP: Word;
  procedure WConvert(var Result: AnsiString);
  begin
    fUniTemp := PRawToUnicode(P, Len, CP);
    Result := ZUnicodeToRaw(fUniTemp, zOSCodePage);
  end;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := FloatToRaw(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToRaw(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToRaw(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToRaw(PDouble(sqldata)^);
          else raise CreateIBConvertError(ColumnIndex, SQLCode);
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := FloatToRaw(PDouble(sqldata)^);
          SQL_LONG      : Result := IntToRaw(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToRaw(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := BoolToRawEx(PSmallint(sqldata)^ <> 0);
          SQL_BOOLEAN_FB: Result := BoolToRawEx(PByte(sqldata)^ <> 0);
          SQL_SHORT     : Result := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              if ConSettings^.ClientCodePage^.ID = CS_NONE
              then CP := FCodePageArray[sqlsubtype and 255]
              else CP := FClientCP;
              if CP = ZOSCodePage
              then ZSetString(P, Len, Result)
              else WConvert(Result);
            end;
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});  //localize interface to keep pointer alive
              try
                if FBlobTemp.IsClob then
                  Result := FBlobTemp.GetAnsiString
                else
                  Result := FBlobTemp.GetString;
              finally
                FBlobTemp := nil;
              end;
            End;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;
{$ENDIF NO_ANSISTRING}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
          else raise CreateIBConvertError(ColumnIndex, SQLCode);
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   : begin
                            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
                            ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                          end;
        else
          raise CreateIBConvertError(ColumnIndex, SQLCode)
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZInterbase6XSQLDAResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  BlobId: TISC_QUAD;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Result := nil;
  LastWasNull := IsNull(ColumnIndex);
  if not LastWasNull then begin
    BlobId := GetQuad(ColumnIndex);
    if FCachedBlob then
      case TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType of
        stBinaryStream:
          begin
            Result := TZAbstractBlob.Create;
            ReadBlobBufer(FPlainDriver, FPISC_DB_HANDLE, FIBConnection.GetTrHandle,
              BlobId, Result.GetLengthAddress^, Result.GetBufferAddress^, True, Consettings);
          end;
        stAsciiStream, stUnicodeStream:
          begin
            Result := TZAbstractClob.CreateWithData(nil, 0, Consettings^.ClientCodePage^.CP, ConSettings);
            ReadBlobBufer(FPlainDriver, FPISC_DB_HANDLE, FIBConnection.GetTrHandle,
              BlobId, Result.GetLengthAddress^, Result.GetBufferAddress^, False, Consettings);
          end;
      end
    else
      case TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType of
        stBinaryStream:
          Result := TZInterbase6UnCachedBlob.Create(FPlainDriver, BlobId, FIBConnection.GetActiveTransaction, FIBConnection);
        stAsciiStream, stUnicodeStream:
          Result := TZInterbase6UnCachedClob.Create(FPlainDriver, BlobId, FIBConnection.GetActiveTransaction, FIBConnection);
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZInterbase6XSQLDAResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
label Fail;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := False
  else
  begin
    {$R-}
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale] <> 0;
          SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale] <> 0;
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale] <> 0;
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^) > 0;
        else goto Fail;
        end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^) <> 0;
          SQL_LONG      : Result := PInteger(sqldata)^ <> 0;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^) <> 0;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^ <> 0;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^<>0;
          SQL_SHORT     : Result := PSmallint(sqldata)^ <> 0;
          SQL_INT64     : Result := PInt64(sqldata)^ <> 0;
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              Result := StrToBoolEx(P);
            end;
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := StrToBoolEx(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString)
            else
              goto Fail;
        else
Fail:     raise CreateIBConvertError(ColumnIndex, SQLCode)
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  SQLCode: SmallInt;
begin
  CheckClosed;
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := nil
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      case SQLCode of
        SQL_TEXT, SQL_VARYING:
          Result := BufferToBytes(sqldata, sqllen);
        else
          raise CreateIBConvertError(ColumnIndex, SQLCode)
      end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  TempDate: TCTimeStructure;
  Len: NativeUInt;
  P: PAnsiChar;
  Failed: Boolean;
  SQLCode: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
      SQLCode := (sqltype and not(1));
      case SQLCode of
        SQL_TIMESTAMP :
          begin
            FPlainDriver.isc_decode_timestamp(PISC_TIMESTAMP(sqldata), @TempDate);
            Result := SysUtils.EncodeDate(TempDate.tm_year + 1900,
              TempDate.tm_mon + 1, TempDate.tm_mday);
          end;
        SQL_TYPE_DATE :
          begin
            FPlainDriver.isc_decode_sql_date(PISC_DATE(sqldata), @TempDate);
            Result := SysUtils.EncodeDate(Word(TempDate.tm_year + 1900),
              Word(TempDate.tm_mon + 1), Word(TempDate.tm_mday));
          end;
        SQL_TYPE_TIME : Result := 0;
        SQL_TEXT,
        SQL_VARYING:
          begin
            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
            if Len = ConSettings^.ReadFormatSettings.DateFormatLen
            then Result := RawSQLDateToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
            else Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
                RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed));
            LastWasNull := Result = 0;
          end;
        else
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetDouble(ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}));
      end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
          else raise CreateIBConvertError(ColumnIndex, SQLCode);
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   : begin
                            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
                            ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                          end;
        else
          raise CreateIBConvertError(ColumnIndex, SQLCode)
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$R-}
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
          else raise CreateIBConvertError(ColumnIndex, SQLCode);
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   : begin
                            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
                            ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                          end;
        else
          raise CreateIBConvertError(ColumnIndex, SQLCode)
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  / IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    / IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := PDouble(sqldata)^;
          else raise CreateIBConvertError(ColumnIndex, SQLCode);
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := PDouble(sqldata)^;
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := PSingle(sqldata)^;
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   : begin
                            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
                            ZSysUtils.SQLStrToFloatDef(P, 0, Result, Len);
                          end;
        else
          raise CreateIBConvertError(ColumnIndex, SQLCode)
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetInt(ColumnIndex: Integer): Integer;
label Fail;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else goto Fail;
        end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              Result := RawToIntDef(P, 0);
            end;
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := RawToIntDef(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString, 0)
            else
              goto Fail;
        else
Fail:     raise CreateIBConvertError(ColumnIndex, SQLCode)
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetLong(ColumnIndex: Integer): Int64;
label Fail;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          goto Fail;
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              Result := RawToInt64Def(P, 0);
            end;
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := RawToInt64Def(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString, 0)
            else
              goto Fail;
        else
Fail:     raise CreateIBConvertError(ColumnIndex, SQLCode)
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZInterbase6XSQLDAResultSet.GetULong(ColumnIndex: Integer): UInt64;
label Fail;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    {$IFDEF WITH_UINT64_C1118_ERROR}
    Result := UInt64(0) //need that type cast for D7 else "internal error C1118"
    {$ELSE}
    Result := 0
    {$ENDIF}
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := PSmallInt(sqldata)^ div IBScaleDivisor[sqlscale];
          SQL_LONG   : Result := PInteger(sqldata)^  div IBScaleDivisor[sqlscale];
          SQL_INT64,
          SQL_QUAD   : Result := PInt64(sqldata)^    div IBScaleDivisor[sqlscale];
          SQL_DOUBLE : Result := Trunc(PDouble(sqldata)^);
        else
          goto Fail;
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := Trunc(PDouble(sqldata)^);
          SQL_LONG      : Result := PInteger(sqldata)^;
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := Trunc(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := PSmallint(sqldata)^;
          SQL_BOOLEAN_FB: Result := PByte(sqldata)^;
          SQL_SHORT     : Result := PSmallint(sqldata)^;
          SQL_INT64     : Result := PInt64(sqldata)^;
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              Result := RawToUInt64Def(P, 0);
            end;
          SQL_BLOB:
            if sqlsubtype = 1 then
              Result := RawToUInt64Def(GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}).GetRawByteString, 0)
            else
              goto Fail;
        else
Fail:     raise CreateIBConvertError(ColumnIndex, SQLCode)
        end;
    end;
    {$IFOPT D+}
    {$R+}
    {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  SQLCode: SmallInt;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    Result := '';
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := FloatToRaw(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToRaw(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToRaw(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToRaw(PDouble(sqldata)^);
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := FloatToRaw(PDouble(sqldata)^);
          SQL_LONG      : Result := IntToRaw(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToRaw(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := BoolToRawEx(PSmallint(sqldata)^ <> 0);
          SQL_BOOLEAN_FB: Result := BoolToRawEx(PByte(sqldata)^ <> 0);
          SQL_SHORT     : Result := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT,
          SQL_VARYING   : Result := GetRawFromTextVar(SQLCode, sqldata, sqllen);
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex);
              if FBlobTemp.IsClob then
                Result := FBlobTemp.GetRawByteString
              else
                Result := FBlobTemp.GetString;
              FBlobTemp := nil;
            End;
        else
          raise EZIBConvertError.Create(Format(SErrorConvertionField,
            [FIZSQLDA.GetFieldAliasName(ColumnIndex), GetNameSqlType(SQLCode)]));
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  TempDate: TCTimeStructure;
  Failed: Boolean;
  P: PAnsiChar;
  Len: NativeUInt;
  SQLCode: SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}] do
    begin
      SQLCode := sqltype and not(1);
      case SQLCode of
        SQL_TIMESTAMP :
          begin
            FPlainDriver.isc_decode_timestamp(PISC_TIMESTAMP(sqldata), @TempDate);
            Result := EncodeTime(TempDate.tm_hour, TempDate.tm_min,
              TempDate.tm_sec, Word((PISC_TIMESTAMP(sqldata).timestamp_time mod ISC_TIME_SECONDS_PRECISION) div 10));
          end;
        SQL_TYPE_DATE : Result := 0;
        SQL_TYPE_TIME :
          begin
            FPlainDriver.isc_decode_sql_time(PISC_TIME(sqldata), @TempDate);
            Result := SysUtils.EncodeTime(Word(TempDate.tm_hour), Word(TempDate.tm_min),
              Word(TempDate.tm_sec),  Word((PISC_TIME(sqldata)^ mod ISC_TIME_SECONDS_PRECISION) div 10));
          end;
        SQL_TEXT, SQL_VARYING:
          begin
            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
            if AnsiChar((P+2)^) = AnsiChar(':') then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed)
            else
              Result := Frac(RawSQLTimeStampToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed));
          end;
        else
          Result := Frac(GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}));
      end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
function TZInterbase6XSQLDAResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  P: PAnsiChar;
  Len: NativeUInt;
  SQLCode: SmallInt;
  TempDate: TCTimeStructure;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}] do
    begin
      SQLCode := sqltype and not(1);
      case SQLCode of
        SQL_TIMESTAMP :
          begin
           P := SQLData;
            FPlainDriver.isc_decode_timestamp(PISC_TIMESTAMP(p), @TempDate);

            Result := EncodeDate(TempDate.tm_year + 1900,
              TempDate.tm_mon + 1, TempDate.tm_mday);
            if Result < 0 //d7 bug: EncodeDateTime gives wrong results if Year < 1899
            then Result := Result - EncodeTime(TempDate.tm_hour,
              TempDate.tm_min, TempDate.tm_sec, Word((PISC_TIMESTAMP(sqldata).timestamp_time mod ISC_TIME_SECONDS_PRECISION) div 10))
            else Result := Result + EncodeTime(TempDate.tm_hour,
              TempDate.tm_min, TempDate.tm_sec, Word((PISC_TIMESTAMP(sqldata).timestamp_time mod ISC_TIME_SECONDS_PRECISION) div 10));
          end;
        SQL_TYPE_DATE :
          begin
            FPlainDriver.isc_decode_sql_date(PISC_DATE(sqldata), @TempDate);
            Result := SysUtils.EncodeDate(Word(TempDate.tm_year + 1900),
              Word(TempDate.tm_mon + 1), Word(TempDate.tm_mday));
          end;
        SQL_TYPE_TIME :
          begin
            FPlainDriver.isc_decode_sql_time(PISC_TIME(sqldata), @TempDate);
            Result := SysUtils.EncodeTime(Word(TempDate.tm_hour), Word(TempDate.tm_min),
              Word(TempDate.tm_sec),  Word((PISC_TIME(sqldata)^ mod ISC_TIME_SECONDS_PRECISION) div 10));
          end;
        SQL_TEXT, SQL_VARYING:
          begin
            GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
            if AnsiChar((P+2)^) = AnsiChar(':') then
              Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
            else
              if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Len) <= 4 then
                Result := RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
              else
                Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed);
          end;
        else
          Result := GetDouble(ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF});
      end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZInterbase6XSQLDAResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  {$IFNDEF DISABLE_CHECKING}
  Assert((ColumnIndex >= 0) and (ColumnIndex <= FXSQLDA.sqln), 'Index out of Range.');
  {$ENDIF}
  {$R-}
  with FXSQLDA.sqlvar[ColumnIndex] do
    Result := (sqlind <> nil) and (sqlind^ = ISC_NULL);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZAnsiRec</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var
  SQLCode: SmallInt;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
  begin
    Result := nil;
    Len := 0;
  end
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : FRawTemp := FloatToRaw(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
          SQL_LONG   : FRawTemp := FloatToRaw(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : FRawTemp := FloatToRaw(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
          SQL_DOUBLE : FRawTemp := FloatToRaw(PDouble(sqldata)^);
          else raise CreateIBConvertError(ColumnIndex, SQLCode);
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : FRawTemp := FloatToRaw(PDouble(sqldata)^);
          SQL_LONG      : FRawTemp := IntToRaw(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : FRawTemp := FloatToRaw(PSingle(sqldata)^);
          SQL_BOOLEAN   : FRawTemp := BoolToRawEx(PSmallint(sqldata)^ <> 0);
          SQL_BOOLEAN_FB: FRawTemp := BoolToRawEx(PByte(sqldata)^ <> 0);
          SQL_SHORT     : FRawTemp := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : FRawTemp := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, Result, Len);
              Exit;
            end;
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});  //localize interface to keep pointer alive
              if FBlobTemp.IsClob then
              begin
                Result := FBlobTemp.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                Len := FBlobTemp.Length;
              end
              else
              begin
                Result := FBlobTemp.GetBuffer;
                Len := FBlobTemp.Length;
              End;
              Exit;
            End;
        else raise CreateIBConvertError(ColumnIndex, SQLCode)
      end;
    end;
    Result := Pointer(FRawTemp);
    Len := NativeUInt({%H-}PLengthInt(NativeUInt(FRawTemp) - StringLenOffSet)^);
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
var
  Len: NativeUInt;
  P: PAnsiChar;
begin
  P := GetPAnsiChar(ColumnIndex, Len);
  ZSetString(P, Len, FRawTemp);
  Result := Pointer(FRawTemp);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZInterbase6XSQLDAResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
  CP: Word;
  procedure WConvert(var Result: UTF8String);
  begin
    fUniTemp := PRawToUnicode(P, Len, CP);
    Result := ZUnicodeToRaw(fUniTemp, zCP_UTF8);
  end;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := FloatToRaw(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToRaw(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToRaw(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToRaw(PDouble(sqldata)^);
          else raise CreateIBConvertError(ColumnIndex, SQLCode);
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := FloatToRaw(PDouble(sqldata)^);
          SQL_LONG      : Result := IntToRaw(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToRaw(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := BoolToRawEx(PSmallint(sqldata)^ <> 0);
          SQL_BOOLEAN_FB: Result := BoolToRawEx(PByte(sqldata)^ <> 0);
          SQL_SHORT     : Result := IntToRaw(PSmallint(sqldata)^);
          SQL_INT64     : Result := IntToRaw(PInt64(sqldata)^);
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              if ConSettings^.ClientCodePage^.ID = CS_NONE
              then CP := FCodePageArray[sqlsubtype and 255]
              else CP := FClientCP;
              if CP = zCP_UTF8
              then ZSetString(P, Len, Result)
              else WConvert(Result);
            end;
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});  //localize interface to keep pointer alive
              try
                if FBlobTemp.IsClob then
                  Result := FBlobTemp.GetUTF8String
                else
                  Result := FBlobTemp.GetString;
              finally
                FBlobTemp := nil;
              end;
            End;
        else
          raise CreateIBConvertError(ColumnIndex, SQLCode)
        end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetString(ColumnIndex: Integer): String;
var
  SQLCode: SmallInt;
  P: PAnsiChar;
  Len: NativeUInt;
  CP: Word;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    {$R-}
    with FXSQLDA.sqlvar[ColumnIndex] do
    begin
      SQLCode := (sqltype and not(1));
      if (sqlscale < 0)  then
      begin
        case SQLCode of
          SQL_SHORT  : Result := FloatToStr(PSmallInt(sqldata)^ / IBScaleDivisor[sqlscale]);
          SQL_LONG   : Result := FloatToStr(PInteger(sqldata)^  / IBScaleDivisor[sqlscale]);
          SQL_INT64,
          SQL_QUAD   : Result := FloatToStr(PInt64(sqldata)^    / IBScaleDivisor[sqlscale]);
          SQL_DOUBLE : Result := FloatToStr(PDouble(sqldata)^);
          else raise CreateIBConvertError(ColumnIndex, SQLCode);
        end;
      end
      else
        case SQLCode of
          SQL_DOUBLE    : Result := FloatToStr(PDouble(sqldata)^);
          SQL_LONG      : Result := ZFastCode.IntToStr(PInteger(sqldata)^);
          SQL_D_FLOAT,
          SQL_FLOAT     : Result := FloatToStr(PSingle(sqldata)^);
          SQL_BOOLEAN   : Result := BoolToStrEx(PSmallint(sqldata)^ <> 0);
          SQL_BOOLEAN_FB: Result := BoolToStrEx(PByte(sqldata)^ <> 0);
          SQL_SHORT     : Result := ZFastCode.IntToStr(PSmallint(sqldata)^);
          SQL_INT64     : Result := ZFastCode.IntToStr(PInt64(sqldata)^);
          SQL_TEXT,
          SQL_VARYING   :
            begin
              GetPCharFromTextVar(SQLCode, sqldata, sqllen, P, Len);
              if ConSettings^.ClientCodePage^.ID = CS_NONE
              then CP := FCodePageArray[sqlsubtype and 255]
              else CP := FClientCP;
              {$IFDEF UNICODE}
              Result := PRawToUnicode(P, Len, CP)
              {$ELSE}
              begin
                ZSetString(P, Len, FRawTemp);
                Result := ConSettings^.ConvFuncs.ZRawToString(FRawTemp, CP, ConSettings^.CTRL_CP);
              end
              {$ENDIF}
            end;
          SQL_BLOB      :
            Begin
              FBlobTemp := GetBlob(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF});  //localize interface to keep pointer alive
              try
                if FBlobTemp.IsClob then
                  {$IFDEF UNICODE}
                  Result := FBlobTemp.GetUnicodeString
                  {$ELSE}
                  Result := ConSettings^.ConvFuncs.ZRawToString(FBlobTemp.GetRawByteString,
                    ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)
                  {$ENDIF}
                else
                  {$IFDEF UNICODE}
                  Result := ASCII7ToUnicodeString(FBlobTemp.GetRawByteString);
                  {$ELSE}
                  Result := FBlobTemp.GetRawByteString;
                  {$ENDIF}
              finally
                FBlobTemp := nil;
              end;
            End;
        else raise CreateIBConvertError(ColumnIndex, SQLCode)
      end;
    end;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ZWideString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZInterbase6XSQLDAResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
var
  P: PAnsiChar;
  Len: NativeUInt;
  CP: Word;
  SQLType, SqlSubType: ISC_Short;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else begin
    P := GetPAnsiChar(ColumnIndex, Len);
    {$R-}
    SQLType := FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}].sqltype and not (1);
    SqlSubType := FXSQLDA.sqlvar[ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}].sqlsubtype;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    if (SQLType = SQL_TEXT) or (SQLType = SQL_VARYING) or ((SQLType = SQL_BLOB) and (SqlSubType = 1)) then begin
      if ConSettings^.ClientCodePage^.ID = CS_NONE
      then CP := FCodePageArray[GetIbSqlSubType(ColumnIndex{$IFNDEF GENERIC_INDEX} -1{$ENDIF}) and 255]
      else CP := FClientCP;
      Result := PRawToUnicode(P, Len, CP);
    end else begin
      Result := Ascii7ToUnicodeString(P, Len);
    end;
  end;
end;

{**
  Moves the cursor down one row from its current position.
  A <code>ResultSet</code> cursor is initially positioned
  before the first row; the first call to the method
  <code>next</code> makes the first row the current row; the
  second call makes the second row the current row, and so on.

  <P>If an input stream is open for the current row, a call
  to the method <code>next</code> will
  implicitly close it. A <code>ResultSet</code> object's
  warning chain is cleared when a new row is read.

  @return <code>true</code> if the new current row is valid;
    <code>false</code> if there are no more rows
}
function TZInterbase6XSQLDAResultSet.Next: Boolean;
var
  StatusVector: TARRAY_ISC_STATUS;
  Status: ISC_STATUS;
label CheckE;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or (RowNo > LastRowNo ) or ((MaxRows > 0) and (LastRowNo >= MaxRows) or (FStmtHandleAddr^ = 0)) then
    Exit;

  { Fetch row. }
  if (FStmtType <> stExecProc) then begin //AVZ - Test for ExecProc - this is for multiple rows
    if (RowNo = 0) then
      FStmtHandle := FStmtHandleAddr^;
    Status := FPlainDriver.isc_dsql_fetch(@StatusVector,
      @FStmtHandle, FDialect, FXSQLDA);
    if Status = 0 then begin
      if (RowNo = 0) then
        RegisterCursor;
      RowNo := RowNo + 1;
      LastRowNo := RowNo;
      Result := True;
    end else if Status = 100  then begin
      {no error occoured -> notify IsAfterLast and close the recordset}
      RowNo := RowNo + 1;
      if FPlainDriver.isc_dsql_free_statement(@StatusVector, @FStmtHandle, DSQL_CLOSE) <> 0 then
        goto CheckE;
      FStmtHandle := 0;
      if (FIBTransaction <> nil) then
        DeRegisterCursor;
    end else
CheckE:CheckInterbase6Error(FPlainDriver, StatusVector, ConSettings);
  end else if RowNo = 0 then begin
    Result := True;
    RowNo := 1;
  end else if RowNo = 1 then
    RowNo := 2; //notify AfterLast
end;

{**
   Get Interbase subsql type
   @param Index the index fields
   @return the Interbase subsql
}
function TZInterbase6XSQLDAResultSet.GetIbSqlSubType(const Index: Word): Smallint;
begin
  {$R-}
  Result := FXSQLDA.sqlvar[Index].sqlsubtype;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
   Return Interbase QUAD field value
   @param Index the field index
   @return the field Interbase QUAD value
}
function TZInterbase6XSQLDAResultSet.GetQuad(ColumnIndex: Integer): TISC_QUAD;
begin
  {$R-}
  if not IsNull(ColumnIndex) then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FXSQLDA.sqlvar[ColumnIndex] do
      case (sqltype and not(1)) of
        SQL_QUAD, SQL_DOUBLE, SQL_INT64, SQL_BLOB, SQL_ARRAY: result := PISC_QUAD(sqldata)^;
      else
        raise EZIBConvertError.Create(SUnsupportedDataType + ' ' + {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr((sqltype and not(1))));
      end;
  end
  else
    raise EZIBConvertError.Create('Invalid State.');
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Opens this recordset.
}
procedure TZInterbase6XSQLDAResultSet.Open;
var
  I: Word;
  DataLen: SmallInt;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZColumnInfo;
  ZCodePageInfo: PZCodePage;
  CPID: Word;
label jmpLen;
begin
  if FStmtHandle=0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  ColumnsInfo.Clear;
  if FXSQLDA.sqld > 0 then  //keep track we have a column to avoid range issues see: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=10595
    for I := 0 to FXSQLDA.sqld {FieldCount} - 1 do
    begin
      ColumnInfo := TZColumnInfo.Create;
      with ColumnInfo do
      begin
        TableName := FIZSQLDA.GetFieldRelationName(I);
        if TableName <> '' then
          ColumnName := FIZSQLDA.GetFieldSqlName(I);
        ColumnLabel := FIZSQLDA.GetFieldAliasName(I);
        FieldSqlType := FIZSQLDA.GetFieldSqlType(I);
        DataLen := FIZSQLDA.GetIbSqlLen(I);
        ColumnType := FieldSqlType;

        case FieldSqlType of
          stString, stUnicodeString: begin
              //see test Bug#886194, we retrieve 565 as CP... the modula get returns the FBID of CP
              CPID := FIZSQLDA.GetIbSqlSubType(I) and 255;
                //see: http://sourceforge.net/p/zeoslib/tickets/97/
              ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CPID); //get column CodePage info
              ColumnCodePage := ZCodePageInfo^.CP;
              if ConSettings^.ClientCodePage^.ID <> CS_NONE 
              then Precision := DataLen div ZCodePageInfo^.CharWidth
              else Precision := DataLen;
              if ColumnType = stString then begin
jmpLen:         CharOctedLength := DataLen;
                ColumnDisplaySize := Precision;
              end else begin
                CharOctedLength := Precision shl 1;
                ColumnDisplaySize := Precision;
              end;
            end;
          stAsciiStream, stUnicodeStream: if ConSettings^.ClientCodePage^.ID = CS_NONE
            then if FIsMetadataResultSet
              then ColumnCodePage := zCP_UTF8
              else begin //connected with CS_NONE no transliterions are made by FB
                CPID := FIBConnection.GetSubTypeTextCharSetID(TableName,ColumnName);
                if CPID = CS_NONE
                then ZCodePageInfo := ConSettings^.ClientCodePage
                else ZCodePageInfo := FPlainDriver.ValidateCharEncoding(CPID);
                ColumnCodePage := ZCodePageInfo.CP;
              end else ColumnCodePage := ConSettings^.ClientCodePage^.CP;
          stBytes: begin
              ColumnCodePage := 0;
              Precision := DataLen;
              goto jmpLen;
            end;
          stBinaryStream: ;
          else begin
            ColumnCodePage := zCP_NONE;
            case FieldSqlType of
              stBytes:
                Precision := DataLen;
              stShort, stSmall, stInteger, stLong:
                Signed := True;
              stCurrency, stBigDecimal: begin
                  Signed  := True;
                  Scale   := -FIZSQLDA.GetFieldScale(I);
                  //first digit does not count because of overflow (FB does not allow this)
                  case FIZSQLDA.GetIbSqlType(I) of
                    SQL_SHORT:  Precision := 4;
                    SQL_LONG:   Precision := 9;
                    SQL_INT64:  Precision := 18;
                  end;
                end;
              stTime, stTimeStamp: Scale := {-}4; //fb supports 10s of milli second fractions
            end;
          end;
        end;
        ReadOnly := (TableName = '') or (ColumnName = '') or
          (ColumnName = 'RDB$DB_KEY') or (FieldSqlType = ZDbcIntfs.stUnknown);
        Writable := not ReadOnly;
        Nullable := TZColumnNullableType(Ord(FIZSQLDA.IsNullable(I)));
        Scale := FIZSQLDA.GetFieldScale(I);
        CaseSensitive := UpperCase(ColumnName) <> ColumnName; //non quoted fiels are uppercased by default
      end;
      ColumnsInfo.Add(ColumnInfo);
    end;
  inherited Open;
end;

procedure TZInterbase6XSQLDAResultSet.RegisterCursor;
begin
  FIBTransaction := FIBConnection.GetActiveTransaction;
  FIBTransaction.RegisterOpencursor(IZResultSet(TransactionResultSet));
end;

procedure TZInterbase6XSQLDAResultSet.ResetCursor;
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  if not Closed then begin
    if (FStmtHandle <> 0) then begin
      if (FStmtType <> stExecProc) then begin
         if (FPlainDriver.isc_dsql_free_statement(@StatusVector, @FStmtHandle, DSQL_CLOSE) <> 0) then
          CheckInterbase6Error(FPlainDriver, StatusVector, ConSettings, lcOther, 'isc_dsql_free_statement');
        FStmtHandle := 0;
        if FIBTransaction <> nil then
          DeRegisterCursor;
      end else
        FStmtHandle := 0;
    end;
    inherited ResetCursor;
  end;
end;

procedure TZInterbase6XSQLDAResultSet.DeRegisterCursor;
begin
  FIBTransaction.DeRegisterOpencursor(IZResultSet(TransactionResultSet));
  FIBTransaction := nil;
end;

{ TZInterbase6UnCachedBlob }

procedure TZInterbase6UnCachedBlob.BeforeDestruction;
begin
  FTransaction.DeRegisterOpenUnCachedLob(Self);
  inherited;
end;

constructor TZInterbase6UnCachedBlob.Create(const PlainDriver: IZInterbasePlainDriver;
  var BlobId: TISC_QUAD; const Transaction: IZIBTransaction;
  const Connection: IZInterbase6Connection);
begin
  FBlobId := BlobId;
  FPlainDriver := PlainDriver;
  FTransaction := Transaction;
  FTransaction.RegisterOpenUnCachedLob(Self);
  FIBConnection := Connection;
end;

{**
  Reads the blob information by blob handle.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
}
procedure TZInterbase6UnCachedBlob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  if FIBConnection <> nil then
    ReadBlobBufer(FPlainDriver, FIBConnection.GetDBHandle, FTransaction.GetTrHandle,
    FBlobId, Size, Buffer, True, FIBConnection.GetConSettings);
  BlobSize := Size;
  BlobData := Buffer;
  inherited ReadLob;
end;

{ TZInterbase6UnCachedClob }

{**
  Reads the blob information by blob handle.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
}
procedure TZInterbase6UnCachedClob.BeforeDestruction;
begin
  FTransaction.DeRegisterOpenUnCachedLob(Self);
  inherited;
end;

constructor TZInterbase6UnCachedClob.Create(const PlainDriver: IZInterbasePlainDriver;
  var BlobId: TISC_QUAD; const Transaction: IZIBTransaction;
  const Connection: IZInterbase6Connection);
begin
  inherited CreateWithData(nil, 0, Connection.GetConSettings^.ClientCodePage^.CP,
    Connection.GetConSettings);
  FTransaction := Transaction;
  FTransaction.RegisterOpenUnCachedLob(Self);
  FIBConnection := Connection;
  FBlobId := BlobId;
  FPlainDriver := PlainDriver;
end;

procedure TZInterbase6UnCachedClob.ReadLob;
var
  Size: Integer;
  Buffer: Pointer;
begin
  InternalClear;
  ReadBlobBufer(FPlainDriver, FIBConnection.GetDBHandle, FTransaction.GetTrHandle,
    FBlobId, Size, Buffer, False, FConSettings);
  AnsiChar((PAnsiChar(Buffer)+NativeUInt(Size))^) := AnsiChar(#0); //add #0 terminator
  FCurrentCodePage := FConSettings^.ClientCodePage^.CP;
  FBlobSize := Size+1;
  BlobData := Buffer;
  inherited ReadLob;
end;

{ TZInterbaseResultSetMetadata }

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZInterbaseResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
end;

{**
  Gets the designated column's table's catalog name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
function TZInterbaseResultSetMetadata.GetCatalogName(
  ColumnIndex: Integer): string;
begin
  Result := ''; //not supported by FB/IB
end;

{**
  Get the designated column's name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name
}
function TZInterbaseResultSetMetadata.GetColumnName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnName;
end;

{**
  Get the designated column's table's schema.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZInterbaseResultSetMetadata.GetSchemaName(
  ColumnIndex: Integer): string;
begin
  Result := ''; //not supported by FB/IB
end;

{**
  Gets the designated column's table name.
  @param ColumnIndex the first ColumnIndex is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZInterbaseResultSetMetadata.GetTableName(
  ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).TableName;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbaseResultSetMetadata.IsAutoIncrement(
  ColumnIndex: Integer): Boolean;
begin
  Result := False; //not supported by FB/IB
end;

{**
  Initializes columns with additional data.
}
procedure TZInterbaseResultSetMetadata.LoadColumns;
{$IFNDEF ZEOS_TEST_ONLY}
var
  Current: TZColumnInfo;
  I: Integer;
  TableColumns: IZResultSet;
  Connection: IZConnection;
  Driver: IZDriver;
  IdentifierConvertor: IZIdentifierConvertor;
  Analyser: IZStatementAnalyser;
  Tokenizer: IZTokenizer;
{$ENDIF}
begin
  {$IFDEF ZEOS_TEST_ONLY}
  inherited LoadColumns;
  {$ELSE}
  Connection := Metadata.GetConnection;
  Driver := Connection.GetDriver;
  Analyser := Driver.GetStatementAnalyser;
  Tokenizer := Driver.GetTokenizer;
  IdentifierConvertor := Metadata.GetIdentifierConvertor;
  try
    if Analyser.DefineSelectSchemaFromQuery(Tokenizer, SQL) <> nil then
      for I := 0 to ResultSet.ColumnsInfo.Count - 1 do begin
        Current := TZColumnInfo(ResultSet.ColumnsInfo[i]);
        ClearColumn(Current);
        if Current.TableName = '' then
          continue;
        TableColumns := Metadata.GetColumns(Current.CatalogName, Current.SchemaName, Metadata.AddEscapeCharToWildcards(IdentifierConvertor.Quote(Current.TableName)),'');
        if TableColumns <> nil then begin
          TableColumns.BeforeFirst;
          while TableColumns.Next do
            if TableColumns.GetString(ColumnNameIndex) = Current.ColumnName then begin
              FillColumInfoFromGetColumnsRS(Current, TableColumns, Current.ColumnName);
              Break;
            end;
        end;
      end;
  finally
    Driver := nil;
    Connection := nil;
    Analyser := nil;
    Tokenizer := nil;
    IdentifierConvertor := nil;
  end;
  Loaded := True;
  {$ENDIF}
end;

procedure TZInterbaseResultSetMetadata.SetColumnPrecisionFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  if (ColumnInfo.ColumnType in [stCurrency, stBigDecimal]) and
     not TableColumns.IsNull(TableColColumnSizeIndex) then
    ColumnInfo.Precision := TableColumns.GetInt(TableColColumnSizeIndex);
end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.
