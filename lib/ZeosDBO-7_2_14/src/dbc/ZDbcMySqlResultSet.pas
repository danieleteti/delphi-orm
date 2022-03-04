{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySqlResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  ZClasses, {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  ZDbcIntfs, ZDbcResultSet, ZDbcResultSetMetadata, ZCompatibility, ZDbcCache,
  ZDbcCachedResultSet, ZDbcGenericResolver, ZDbcMySqlStatement,
  ZPlainMySqlDriver, ZPlainMySqlConstants, ZSelectSchema;

type
  {** Implements MySQL ResultSet Metadata. }
  TZMySQLResultSetMetadata = class(TZAbstractResultSetMetadata)
  private
    FHas_ExtendedColumnInfos: Boolean;
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
    procedure LoadColumns; override;
  public
    constructor Create(const Metadata: IZDatabaseMetadata; const SQL: string;
      ParentResultSet: TZAbstractResultSet);
  public
    function GetCatalogName(ColumnIndex: Integer): string; override;
    function GetColumnName(ColumnIndex: Integer): string; override;
    function GetColumnType(ColumnIndex: Integer): TZSQLType; override;
    function GetSchemaName(ColumnIndex: Integer): string; override;
    function GetTableName(ColumnIndex: Integer): string; override;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; override;
  end;

  {** Implements MySQL ResultSet. }

  { TZAbstractMySQLResultSet }

  TZAbstractMySQLResultSet = class(TZAbstractResultSet)
  private
    FHandle: PMySQL;
    FQueryHandle: PZMySQLResult;
    FRowHandle: PZMySQLRow;
    FPlainDriver: IZMySQLPlainDriver;
    FLengthArray: PULongArray;
    FMySQLTypes: array of TMysqlFieldType;
    fServerCursor: Boolean;
    function GetBufferAndLength(ColumnIndex: Integer; var Len: NativeUInt): PAnsiChar; {$IFDEF WITHINLINE}inline;{$ENDIF}
    function GetBuffer(ColumnIndex: Integer): PAnsiChar; {$IFDEF WITHINLINE}inline;{$ENDIF}
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver;
      const Statement: IZStatement; const SQL: string; Handle: PMySQL;
      AffectedRows: PInteger);
    procedure BeforeClose; override;
    procedure AfterClose; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;
    //EH: keep that override 4 all descendants: seek_data is dead slow in a forward only mode
    function Next: Boolean; override;
  end;

  TZMySQL_Store_ResultSet = class(TZAbstractMySQLResultSet)
  public
    function MoveAbsolute(Row: Integer): Boolean; override;
    procedure ResetCursor; override;
  end;

  TZMySQL_Use_ResultSet = class(TZAbstractMySQLResultSet)
  public
    procedure ResetCursor; override;
  end;

  {** Implements Prepared MySQL ResultSet. }
  TZAbstractMySQLPreparedResultSet = class(TZAbstractResultSet)
  private
    FMysQL: PMySQL;
    FPrepStmt: PMySql_Stmt;
    FPlainDriver: IZMySQLPlainDriver;
    FColumnArray: TZMysqlColumnBuffer;
    FBindBuffer: TZMySqlResultSetBindBuffer;
    FColBind: PDOBindRecord2;
    FTempBlob: IZBlob;
    fServerCursor: Boolean;
  protected
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
    procedure Open; override;
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver; const Statement: IZStatement;
      const SQL: string; MySQL: PMySQL; MySQL_Stmt: PMySql_Stmt);

    procedure AfterClose; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetShort(ColumnIndex: Integer): ShortInt; override;
    function GetWord(ColumnIndex: Integer): Word; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetUInt(ColumnIndex: Integer): LongWord; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    function Next: Boolean; override;
  end;

  TZMySQL_Store_PreparedResultSet = class(TZAbstractMySQLPreparedResultSet)
  public
    function MoveAbsolute(Row: Integer): Boolean; override;
    procedure ResetCursor; override;
  end;

  TZMySQL_Use_PreparedResultSet = class(TZAbstractMySQLPreparedResultSet)
  public
    procedure ResetCursor; override;
  end;

  {** Implements a cached resolver with MySQL specific functionality. }
  TZMySQLCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FHandle: PMySQL;
    FPlainDriver: IZMySQLPlainDriver;
    FAutoColumnIndex: Integer;
    //FStatement: IZMysqlStatement;
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver; Handle: PMySQL;
      const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    function FormWhereClause(Columns: TObjectList;
      OldRowAccessor: TZRowAccessor): string; override;
    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;

    // --> ms, 31/10/2005
    function FormCalculateStatement(Columns: TObjectList): string; override;
    // <-- ms
    {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure UpdateAutoIncrementFields(Sender: IZCachedResultSet; {%H-}UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; {%H-}Resolver: IZCachedResolver); override;
    {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
  end;

  TZMySQLPreparedClob = Class(TZAbstractClob)
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver; Bind: PDOBindRecord2;
      StmtHandle: PMySql_Stmt; ColumnIndex: Cardinal; ConSettings: PZConSettings);
  End;

  TZMySQLPreparedBlob = Class(TZAbstractBlob)
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver; Bind: PDOBindRecord2;
      StmtHandle: PMySql_Stmt; ColumnIndex: Cardinal);
  End;

{$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF}
  ZFastCode, ZSysUtils, ZMessages, ZEncoding, ZTokenizer,
  ZGenericSqlAnalyser,
  ZDbcUtils, ZDbcMetadata, ZDbcLogging, ZDbcMySqlUtils, ZDbcMySql;

{ TZMySQLResultSetMetadata }

procedure TZMySQLResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
end;

{**
  Constructs this object and assignes the main properties.
  @param Metadata a database metadata object.
  @param SQL an SQL query statement.
  @param ColumnsInfo a collection of columns info.
}
constructor TZMySQLResultSetMetadata.Create(const Metadata: IZDatabaseMetadata;
  const SQL: string; ParentResultSet: TZAbstractResultSet);
begin
  inherited Create(Metadata, SQL, ParentResultSet);
  FHas_ExtendedColumnInfos := (MetaData.GetConnection.GetIZPlainDriver as IZMySQLPlainDriver).GetClientVersion > 40000;
end;

{**
  Gets the designated column's table's catalog name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
function TZMySQLResultSetMetadata.GetCatalogName(ColumnIndex: Integer): string;
begin
  if not FHas_ExtendedColumnInfos and not Loaded
  then LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).CatalogName;
end;

{**
  Get the designated column's name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name
}
function TZMySQLResultSetMetadata.GetColumnName(ColumnIndex: Integer): string;
begin
  if not FHas_ExtendedColumnInfos and not Loaded
  then LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnName;
end;

{**
  Retrieves the designated column's SQL type.
  @param column the first column is 1, the second is 2, ...
  @return SQL type from java.sql.Types
}
function TZMySQLResultSetMetadata.GetColumnType(ColumnIndex: Integer): TZSQLType;
begin {EH: does anyone know why the LoadColumns was made? Note the column-types are perfect determinable on MySQL}
  //if not Loaded then
    // LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).ColumnType;
end;

{**
  Get the designated column's table's schema.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZMySQLResultSetMetadata.GetSchemaName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).SchemaName;
end;

{**
  Gets the designated column's table name.
  @param ColumnIndex the first ColumnIndex is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZMySQLResultSetMetadata.GetTableName(ColumnIndex: Integer): string;
begin
  if not FHas_ExtendedColumnInfos and not Loaded
  then LoadColumns;
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).TableName;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZMySQLResultSetMetadata.IsAutoIncrement(
  ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(ResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}]).AutoIncrement;
end;

{**
  Initializes columns with additional data.
}
procedure TZMySQLResultSetMetadata.LoadColumns;
var
  Current: TZColumnInfo;
  I: Integer;
  TableColumns: IZResultSet;
  Connection: IZConnection;
  Driver: IZDriver;
  IdentifierConvertor: IZIdentifierConvertor;
  Analyser: IZStatementAnalyser;
  Tokenizer: IZTokenizer;
begin
  if not FHas_ExtendedColumnInfos
  then inherited LoadColumns
  else begin
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
  end;
  Loaded := True;
end;

{ TZAbstractMySQLResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a native MySQL plain driver.
  @param Statement a related SQL statement object.
  @param Handle a MySQL specific query handle.
  @param UseResult <code>True</code> to use results,
    <code>False</code> to store result.
}
constructor TZAbstractMySQLResultSet.Create(const PlainDriver: IZMySQLPlainDriver;
  const Statement: IZStatement; const SQL: string; Handle: PMySQL;
  AffectedRows: PInteger);
begin
  inherited Create(Statement, SQL, TZMySQLResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
      Statement.GetConnection.GetConSettings);
  fServerCursor := Self is TZMySQL_Use_ResultSet;
  FHandle := Handle;
  FQueryHandle := nil;
  FRowHandle := nil;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;

  Open;
  if Assigned(AffectedRows) then
    AffectedRows^ := LastRowNo;
end;

function TZAbstractMySQLResultSet.GetBufferAndLength(ColumnIndex: Integer; var Len: NativeUInt): PAnsiChar;
var
  x: ULong;
begin
  {$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
  {$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex - 1;
  {$ENDIF}
  {$R-}
  x := FLengthArray^[ColumnIndex];
  Result := PMYSQL_ROW(FRowHandle)[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  Len := x;
  LastWasNull := Result = nil;
end;

procedure TZAbstractMySQLResultSet.BeforeClose;
begin
  inherited BeforeClose;
end;

function TZAbstractMySQLResultSet.GetBuffer(ColumnIndex: Integer): PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex - 1;
  {$ENDIF}
  {$R-}
  Result := PMYSQL_ROW(FRowHandle)[ColumnIndex];
  {$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}
  LastWasNull := Result = nil;
end;

{**
  Opens this recordset.
}
procedure TZAbstractMySQLResultSet.Open;
var
  I: Integer;
  FieldHandle: PMYSQL_FIELD;
  FieldOffsets: PMYSQL_FIELDOFFSETS;
  MySQL_FieldType_Bit_1_IsBoolean: Boolean;
begin
  FieldOffsets := GetFieldOffsets(FPlainDriver.IsMariaDBDriver, FPlainDriver.GetClientVersion);
  if fServerCursor
  then FQueryHandle := FPlainDriver.use_result(FHandle)
  else begin
    FQueryHandle := FPlainDriver.StoreResult(FHandle);
    if Assigned(FQueryHandle) then
      LastRowNo := FPlainDriver.GetRowCount(FQueryHandle)
  end;
  MySQL_FieldType_Bit_1_IsBoolean := (GetStatement.GetConnection as IZMySQLConnection).MySQL_FieldType_Bit_1_IsBoolean;
  if not Assigned(FQueryHandle) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  { Fills the column info. }
  ColumnsInfo.Clear;
  SetLength(FMySQLTypes, FPlainDriver.num_fields(FQueryHandle));
  for I := 0 to High(FMySQLTypes) do begin
    FPlainDriver.SeekField(FQueryHandle, I);
    FieldHandle := FPlainDriver.FetchField(FQueryHandle);
    FMySQLTypes[i] := {%H-}PMysqlFieldType({%H-}NativeUInt(FieldHandle)+FieldOffSets._type)^;
    if FieldHandle = nil then
      Break;

    ColumnsInfo.Add(GetMySQLColumnInfoFromFieldHandle(FieldHandle, FieldOffsets,
      ConSettings, MySQL_FieldType_Bit_1_IsBoolean));
  end;

  inherited Open;
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
procedure TZAbstractMySQLResultSet.AfterClose;
begin
  FQueryHandle := nil;
  FRowHandle := nil;
  inherited AfterClose;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractMySQLResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if FRowHandle = nil then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  Result := (GetBuffer(ColumnIndex) = nil);
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
function TZAbstractMySQLResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if (Closed) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (RowNo > LastRowNo) then
    Exit;
  if (FQueryHandle = nil) then begin
    FQueryHandle := FPlainDriver.StoreResult(FHandle);
    if Assigned(FQueryHandle) then
      LastRowNo := FPlainDriver.GetRowCount(FQueryHandle);
  end;
  FRowHandle := FPlainDriver.FetchRow(FQueryHandle);
  if FRowHandle <> nil then begin
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
  end else begin
    if fServerCursor then begin
      LastRowNo := RowNo;
      RowNo := RowNo+1;
    end else
      RowNo := RowNo+1;
    Exit;
  end;
  FLengthArray := FPlainDriver.FetchLengths(FQueryHandle)
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
begin
  Result := GetBufferAndLength(ColumnIndex, Len{%H-});
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
begin
  Result := GetBuffer(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});
  if LastWasNull then
    Result := ''
  else
    ZSetString(Buffer, Len, Result);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractMySQLResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  Buffer: PAnsiChar;
  Len: ULong;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := False
  else if FMySQLTypes[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = FIELD_TYPE_BIT then begin
    {$R-}
    Len := FLengthArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case Len of
      1: Result := PByte(Buffer)^ <> 0;
      2: Result := ReverseWordBytes(Buffer) <> 0;
      3, 4: Result := ReverseLongWordBytes(Buffer, Len) <> 0;
      else //5..8: makes compiler happy
        Result := ReverseQuadWordBytes(Buffer, Len) <> 0;
    end
  end else
    Result := StrToBoolEx(Buffer, True, False);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  Buffer: PAnsiChar;
  Len: Ulong;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := 0
  else if FMySQLTypes[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = FIELD_TYPE_BIT then begin
    {$R-}
    Len := FLengthArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case Len of
      1: Result := PByte(Buffer)^;
      2: Result := ReverseWordBytes(Buffer);
      3, 4: Result := ReverseLongWordBytes(Buffer, Len);
      else //5..8: makes compiler happy
        Result := ReverseQuadWordBytes(Buffer, Len);
    end
  end else
    Result := RawToIntDef(Buffer, 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  Buffer: PAnsiChar;
  Len: ULong;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := 0
  else if FMySQLTypes[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = FIELD_TYPE_BIT then begin
    {$R-}
    Len := FLengthArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    case Len of
      1: Result := PByte(Buffer)^;
      2: Result := ReverseWordBytes(Buffer);
      3, 4: Result := ReverseLongWordBytes(Buffer, Len);
      else //5..8: makes compiler happy
        Result := ReverseQuadWordBytes(Buffer, Len);
    end
  end else
    Result := RawToInt64Def(Buffer, 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractMySQLResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  Buffer: PAnsiChar;
  Len: ULong;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  Buffer := GetBuffer(ColumnIndex);

  if LastWasNull then
    Result := 0
  else if FMySQLTypes[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] = FIELD_TYPE_BIT then begin
    {$R-}
    Len := FLengthArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    {$IF defined (RangeCheckEnabled) and not defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    case Len of
      1: Result := PByte(Buffer)^;
      2: Result := ReverseWordBytes(Buffer);
      3, 4: Result := ReverseLongWordBytes(Buffer, Len);
      else //5..8: makes compiler happy
        Result := ReverseQuadWordBytes(Buffer, Len);
    end
  end else
    Result := RawToUInt64Def(Buffer, 0);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    ZSysUtils.SQLStrToFloatDef(Buffer, 0, Result, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    ZSysUtils.SQLStrToFloatDef(Buffer, 0, Result, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
    ZSysUtils.SQLStrToFloatDef(Buffer, 0, Result, Len);
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
function TZAbstractMySQLResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});
  Result := BufferToBytes(Buffer, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
  begin
    if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
      Result := RawSQLDateToDateTime(Buffer,  Len, ConSettings^.ReadFormatSettings, Failed{%H-})
    else
      Result := Int(RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed));
    LastWasNull := Result = 0;
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
function TZAbstractMySQLResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else
  begin
    if (Buffer+2)^ = ':' then //possible date if Len = 10 then
      Result := RawSQLTimeToDateTime(Buffer,Len, ConSettings^.ReadFormatSettings, Failed{%H-})
    else
      Result := Frac(RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed));
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
function TZAbstractMySQLResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Len: NativeUInt;
  Buffer: PAnsiChar;
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});

  if LastWasNull then
    Result := 0
  else if (Buffer+2)^ = ':' then
    Result := RawSQLTimeToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed{%H-})
  else if (ConSettings^.ReadFormatSettings.DateTimeFormatLen < Len) or (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Len <= 4) then
    Result := RawSQLTimeStampToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed)
  else
    Result := RawSQLDateToDateTime(Buffer, Len, ConSettings^.ReadFormatSettings, Failed);
  LastWasNull := Result = 0;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractMySQLResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  Buffer: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Buffer := GetBufferAndLength(ColumnIndex, Len{%H-});
  if LastWasNull then
    Result := nil
  else
    case GetMetaData.GetColumnType(ColumnIndex) of
      stBytes, stBinaryStream:
        Result := TZAbstractBlob.CreateWithData(Buffer, Len)
      else
        Result := TZAbstractClob.CreateWithData(Buffer, Len,
          ConSettings^.ClientCodePage^.CP, ConSettings)
    end;
end;

{ TZMySQL_Store_ResultSet }

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZMySQL_Store_ResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or ((MaxRows > 0) and (Row > MaxRows)) then
    Exit;

  if (FQueryHandle = nil) then begin
    FQueryHandle := FPlainDriver.StoreResult(FHandle);
    if Assigned(FQueryHandle) then
      LastRowNo := FPlainDriver.GetRowCount(FQueryHandle);
  end;

  { Process negative rows. }
  if Row < 0 then begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if (Row >= 0) and (Row <= LastRowNo + 1) then begin
    RowNo := Row;
    if (Row >= 1) and (Row <= LastRowNo) then begin
      FPlainDriver.SeekData(FQueryHandle, RowNo - 1);
      FRowHandle := FPlainDriver.FetchRow(FQueryHandle);
    end else
      FRowHandle := nil;
  end;

  Result := FRowHandle <> nil;

  if Result
  then FLengthArray := FPlainDriver.FetchLengths(FQueryHandle)
  else FLengthArray := nil;
end;

procedure TZMySQL_Store_ResultSet.ResetCursor;
begin
  inherited ResetCursor;
  if FQueryHandle <> nil then
    FPlainDriver.FreeResult(FQueryHandle);
  FQueryHandle := nil;
end;

{ TZAbstractMySQLPreparedResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a native MySQL plain driver.
  @param Statement a related SQL statement object.
  @param Handle a MySQL specific query handle.
  @param UseResult <code>True</code> to use results,
    <code>False</code> to store result.
}
constructor TZAbstractMySQLPreparedResultSet.Create(
  const PlainDriver: IZMySQLPlainDriver; const Statement: IZStatement;
  const SQL: string; MySQL: PMySQL; MySQL_Stmt: PMySql_Stmt);
begin
  inherited Create(Statement, SQL, TZMySQLResultSetMetadata.Create(
    Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  fServerCursor := Self is TZMySQL_Use_PreparedResultSet;
  FMysQL := MySQL;
  FPrepStmt := MySQL_Stmt;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  Open;
end;

{**
  Opens this recordset.
}
procedure TZAbstractMySQLPreparedResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  FieldHandle: PMYSQL_FIELD;
  FieldCount: Integer;
  FResultMetaData : PZMySQLResult;
  FIELDOFFSETS: PMYSQL_FIELDOFFSETS;
begin
  FieldCount := FPlainDriver.stmt_field_count(FPrepStmt);
  if FieldCount = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  FResultMetaData := FPlainDriver.stmt_result_metadata(FPrepStmt);

  if not Assigned(FResultMetaData) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  { Initialize Bind Array and Column Array }
  FBindBuffer := TZMySqlResultSetBindBuffer.Create(FPlainDriver,FieldCount,FColumnArray);
  FIELDOFFSETS := GetFieldOffsets(FPlainDriver.IsMariaDBDriver, FPlainDriver.GetClientVersion);
  { EH: no we skip that! We use the refetch logic of
  https://bugs.mysql.com/file.php?id=12361&bug_id=33086

  if (Self is TZMySQL_Store_PreparedResultSet) then
    //Note: This slows down the performance but makes synchronized RS possible!
    FPlainDriver.stmt_attr_set(FPrepStmt,STMT_ATTR_UPDATE_MAX_LENGTH, @one);

  { Fills the column info. }
  ColumnsInfo.Clear;
  try
    for I := 0 to FPlainDriver.num_fields(FResultMetaData) - 1 do
    begin
      FPlainDriver.SeekField(FResultMetaData, I);
      FieldHandle := FPlainDriver.FetchField(FResultMetaData);
      if FieldHandle = nil then
        Break;

      ColumnInfo := GetMySQLColumnInfoFromFieldHandle(FieldHandle, FIELDOFFSETS,
        ConSettings, fServerCursor);

      ColumnsInfo.Add(ColumnInfo);

      FBindBuffer.AddColumn(FieldHandle, FIELDOFFSETS);
    end;
  finally
    FPlainDriver.FreeResult(FResultMetaData);
  end;

  if (FPlainDriver.stmt_bind_result(FPrepStmt,FBindBuffer.GetBufferAddress)<>0) then
    raise EZSQLException.Create(SFailedToBindResults);
  {execute after bind but before fetch!}
  //Note: This slows down the performance but makes synchronized RS possible!
  if (Self is TZMySQL_Store_PreparedResultSet) and (FPlainDriver.stmt_store_result(FPrepStmt)=0) then
    LastRowNo := FPlainDriver.stmt_num_rows(FPrepStmt);

  inherited Open;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

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
procedure TZAbstractMySQLPreparedResultSet.AfterClose;
begin
  if Assigned(FBindBuffer) then
    FreeAndNil(FBindBuffer);
  inherited AfterClose;
  FPrepStmt := nil;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractMySQLPreparedResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  Result := FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].is_null =1;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the String in bytes
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLPreparedResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsichar;
var
  TmpDateTime, TmpDateTime2: TDateTime;
begin
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
  begin
    Result := nil;
    Len := 0;
  end
  else
  begin
    case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          FRawTemp := IntToRaw(PShortInt(FColBind^.buffer)^)
        else
          FRawTemp := IntToRaw(PByte(FColBind^.buffer)^);
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          FRawTemp := IntToRaw(PSmallInt(FColBind^.buffer)^)
        else
          FRawTemp := IntToRaw(PWord(FColBind^.buffer)^);
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          FRawTemp := IntToRaw(PInteger(FColBind^.buffer)^)
        else
          FRawTemp := IntToRaw(PCardinal(FColBind^.buffer)^);
      FIELD_TYPE_FLOAT:
        FRawTemp := FloatToSQLRaw(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:
        FRawTemp := FloatToSQLRaw(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:
        FRawTemp := '';
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColBind^.buffer)^.Year,
            PMYSQL_TIME(FColBind^.buffer)^.Month,
            PMYSQL_TIME(FColBind^.buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColBind^.buffer)^.Hour,
            PMYSQL_TIME(FColBind^.buffer)^.Minute,
            PMYSQL_TIME(FColBind^.buffer)^.Second,
            0{PMYSQL_TIME(FColBind^.buffer)^.second_part} , TmpDateTime2 ) then
              TmpDateTime2 := 0;
          FRawTemp := DateTimeToRawSQLTimeStamp(TmpDateTime+TmpDateTime2, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          FRawTemp := IntToRaw(PInt64(FColBind^.buffer)^)
        else
          FRawTemp := IntToRaw(PUInt64(FColBind^.buffer)^);
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColBind^.buffer)^.Year,
            PMYSQL_TIME(FColBind^.buffer)^.Month,
            PMYSQL_TIME(FColBind^.buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          FRawTemp := DateTimeToRawSQLDate(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_TIME:
        begin
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColBind^.buffer)^.Hour,
            PMYSQL_TIME(FColBind^.buffer)^.Minute,
            PMYSQL_TIME(FColBind^.buffer)^.Second,
            0{PMYSQL_TIME(FColBind^.buffer)^.second_part}, TmpDateTime) then
              TmpDateTime := 0;
          FRawTemp := DateTimeToRawSQLTime(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_YEAR:
        FRawTemp := IntToRaw(PWord(FColBind^.buffer)^);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_ENUM, FIELD_TYPE_SET, FIELD_TYPE_STRING:
        begin
          Result := PAnsiChar(FColBind^.buffer);
          Len := FColBind^.length;
          Exit;
        end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
        FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY, MYSQL_TYPE_JSON:
        begin
          FTempBlob := GetBlob(ColumnIndex);
          Len := FTempBlob.Length;
          Result := FTempBlob.GetBuffer;
          Exit;
        end;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end;
    Len := NativeUInt({%H-}PLengthInt(NativeUInt(FRawTemp) - StringLenOffSet)^);
    Result := Pointer(FRawTemp);
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
function TZAbstractMySQLPreparedResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
var Len: NativeUInt;
begin
  Result := GetPAnsiChar(ColumnIndex, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLPreparedResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  TmpDateTime, TmpDateTime2: TDateTime;
begin
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := ''
  else
  begin
    case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := IntToRaw(PShortInt(FColBind^.buffer)^)
        else
          Result := IntToRaw(PByte(FColBind^.buffer)^);
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := IntToRaw(PSmallInt(FColBind^.buffer)^)
        else
          Result := IntToRaw(PWord(FColBind^.buffer)^);
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := IntToRaw(PInteger(FColBind^.buffer)^)
        else
          Result := IntToRaw(PCardinal(FColBind^.buffer)^);
      FIELD_TYPE_FLOAT:
        Result := FloatToSQLRaw(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:
        Result := FloatToSQLRaw(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:
        Result := '';
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColBind^.buffer)^.Year,
            PMYSQL_TIME(FColBind^.buffer)^.Month,
            PMYSQL_TIME(FColBind^.buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColBind^.buffer)^.Hour,
            PMYSQL_TIME(FColBind^.buffer)^.Minute,
            PMYSQL_TIME(FColBind^.buffer)^.Second,
            0{PMYSQL_TIME(FColBind^.buffer)^.second_part} , TmpDateTime2 ) then
              TmpDateTime2 := 0;
          Result := DateTimeToRawSQLTimeStamp(TmpDateTime+TmpDateTime2, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := IntToRaw(PInt64(FColBind^.buffer)^)
        else
          Result := IntToRaw(PUInt64(FColBind^.buffer)^);
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
        begin
          if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColBind^.buffer)^.Year,
            PMYSQL_TIME(FColBind^.buffer)^.Month,
            PMYSQL_TIME(FColBind^.buffer)^.Day, TmpDateTime) then
              TmpDateTime := encodeDate(1900, 1, 1);
          Result := DateTimeToRawSQLDate(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_TIME:
        begin
          if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColBind^.buffer)^.Hour,
            PMYSQL_TIME(FColBind^.buffer)^.Minute,
            PMYSQL_TIME(FColBind^.buffer)^.Second,
            0{PMYSQL_TIME(FColBind^.buffer)^.second_part}, TmpDateTime) then
              TmpDateTime := 0;
          Result := DateTimeToRawSQLTime(TmpDateTime, ConSettings^.ReadFormatSettings, False);
        end;
      FIELD_TYPE_YEAR:
        Result := IntToRaw(PWord(FColBind^.buffer)^);
      FIELD_TYPE_BIT:
        Result := IntToRaw(PByte(FColBind^.buffer)^);
      FIELD_TYPE_ENUM, FIELD_TYPE_SET, FIELD_TYPE_TINY_BLOB,
      FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB,
      FIELD_TYPE_STRING, FIELD_TYPE_GEOMETRY,MYSQL_TYPE_JSON:
        ZSetString(PAnsiChar(FColBind^.buffer),
          FColBind^.length, Result);
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
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
function TZAbstractMySQLPreparedResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := False
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^ <> 0
        else
          Result := PByte(FColBind^.buffer)^ <> 0;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^ <> 0
        else
          Result := PWord(FColBind^.buffer)^ <> 0;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^ <> 0
        else
          Result := PCardinal(FColBind^.buffer)^ <> 0;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColBind^.buffer)^) <> 0;
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColBind^.buffer)^) <> 0;
      FIELD_TYPE_NULL:      Result := False;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := False;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^ <> 0
        else
          Result := PUInt64(FColBind^.buffer)^ <> 0;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^ <> 0;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := StrToBoolEx(PAnsiChar(FColBind^.buffer), True, False);
      FIELD_TYPE_BIT://http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case FColBind^.length of
          1: Result := PByte(FColBind^.buffer)^ <> 0;
          2: Result := ReverseWordBytes(FColBind^.buffer)  <> 0;
          3, 4: Result := ReverseLongWordBytes(FColBind^.buffer, FColBind^.length) <> 0;
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(FColBind^.buffer, FColBind^.length) <> 0;
          end;
          //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
        FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY, MYSQL_TYPE_JSON:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 12{Max Int32 Length = 11} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          Result := StrToBoolEx(PAnsiChar(FTempBlob.GetBuffer));
          FTempBlob := nil;
        end
        else //avoid senceless processing
          Result := False;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLPreparedResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PLongWord(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColBind^.buffer), 0);
      FIELD_TYPE_BIT:
        case FColBind^.length of
          1: Result := PByte(FColBind^.buffer)^;
          2: Result := ReverseWordBytes(FColBind^.buffer);
          3, 4: Result := ReverseLongWordBytes(FColBind^.buffer, FColBind^.length);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(FColBind^.buffer, FColBind^.length);
          end;
          //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 4{max Length = 3} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if FColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLPreparedResultSet.GetShort(ColumnIndex: Integer): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PLongWord(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case FColBind^.length of
          1: Result := PByte(FColBind^.buffer)^;
          2: Result := ReverseWordBytes(FColBind^.buffer);
          3, 4: Result := ReverseLongWordBytes(FColBind^.buffer, FColBind^.length);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(FColBind^.buffer, FColBind^.length);
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 5{Max ShortInt Length = 3+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if FColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Word</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLPreparedResultSet.GetWord(ColumnIndex: Integer): Word;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stWord);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PCardinal(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case FColBind^.length of
          1: Result := PByte(FColBind^.buffer)^;
          2: Result := ReverseWordBytes(FColBind^.buffer);
          3, 4: Result := ReverseLongWordBytes(FColBind^.buffer, FColBind^.length);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(FColBind^.buffer, FColBind^.length);
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 7{Max Word Length = 5+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if FColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>SmallInt</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLPreparedResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stSmall);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PLongWord(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case FColBind^.length of
          1: Result := PByte(FColBind^.buffer)^;
          2: Result := ReverseWordBytes(FColBind^.buffer);
          3, 4: Result := ReverseLongWordBytes(FColBind^.buffer, FColBind^.length);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(FColBind^.buffer, FColBind^.length);
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 8{Max SmallInt Length = 6+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if FColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>LongWord</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractMySQLPreparedResultSet.GetUInt(ColumnIndex: Integer): LongWord;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PCardinal(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToUInt64Def(PAnsiChar(FColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case FColBind^.length of
          1: Result := PByte(FColBind^.buffer)^;
          2: Result := ReverseWordBytes(FColBind^.buffer);
          3, 4: Result := ReverseLongWordBytes(FColBind^.buffer, FColBind^.length);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(FColBind^.buffer, FColBind^.length);
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 12{Max LongWord Length = 10+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if FColBind^.binary then
            Result := RawToUInt64Def(FTempBlob.GetString, 0)
          else
            Result := RawToUInt64Def(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLPreparedResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PLongWord(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToIntDef(PAnsiChar(FColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case FColBind^.length of
          1: Result := PByte(FColBind^.buffer)^;
          2: Result := ReverseWordBytes(FColBind^.buffer);
          3, 4: Result := ReverseLongWordBytes(FColBind^.buffer, FColBind^.length);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(FColBind^.buffer, FColBind^.length);
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 13{Max Int32 Length = 11+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if FColBind^.binary then
            Result := RawToIntDef(FTempBlob.GetString, 0)
          else
            Result := RawToIntDef(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractMySQLPreparedResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PCardinal(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToUInt64Def(PAnsiChar(FColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case FColBind^.length of
          1: Result := PByte(FColBind^.buffer)^;
          2: Result := ReverseWordBytes(FColBind^.buffer);
          3, 4: Result := ReverseLongWordBytes(FColBind^.buffer, FColBind^.length);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(FColBind^.buffer, FColBind^.length);
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 22{Max UInt64 Length = 20+#0} ) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if FColBind^.binary then
            Result := RawToUInt64Def(FTempBlob.GetString, 0)
          else
            Result := RawToUInt64Def(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLPreparedResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PLongWord(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(FColBind^.buffer)^);
      FIELD_TYPE_DOUBLE:    Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(FColBind^.buffer)^);
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := RawToInt64Def(PAnsiChar(FColBind^.buffer), 0);
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        case FColBind^.length of
          1: Result := PByte(FColBind^.buffer)^;
          2: Result := ReverseWordBytes(FColBind^.buffer);
          3, 4: Result := ReverseLongWordBytes(FColBind^.buffer, FColBind^.length);
          else //5..8: makes compiler happy
            Result := ReverseQuadWordBytes(FColBind^.buffer, FColBind^.length);
          end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 22{Max Int64 Length = 20+#0}) then
        begin
          FTempBlob := GetBlob(ColumnIndex);
          if FColBind^.binary then
            Result := RawToInt64Def(FTempBlob.GetString, 0)
          else
            Result := RawToInt64Def(FTempBlob.GetBuffer, 0);
          FTempBlob := nil;
        end
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLPreparedResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PLongWord(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        ZSysUtils.SQLStrToFloatDef(PAnsiChar(FColBind^.buffer), 0, Result, FColBind^.length);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 30{Max Extended Length = 28 ??} ) then
          RawToFloatDef(GetBlob(ColumnIndex).GetBuffer, AnsiChar('.'), 0, Result)
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractMySQLPreparedResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PCardinal(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     if FColBind^.decimals < 20
                            then Result := RoundTo(PSingle(FColBind^.buffer)^, FColBind^.decimals*-1)
                            else Result := PSingle(FColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    if FColBind^.decimals < 20
                            then Result := RoundTo(PDouble(FColBind^.buffer)^, FColBind^.decimals*-1)
                            else Result := PDouble(FColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        ZSysUtils.SQLStrToFloatDef(PAnsiChar(FColBind^.buffer), 0, Result, FColBind^.length);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 30{Max Extended Length = 28 ??+#0} ) then
          RawToFloatDef(GetBlob(ColumnIndex).GetBuffer, AnsiChar('.'), 0, Result)
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLPreparedResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PLongWord(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     if FColBind^.decimals < 20
                            then Result := RoundTo(PSingle(FColBind^.buffer)^, FColBind^.decimals*-1)
                            else Result := PSingle(FColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    if FColBind^.decimals < 20
                            then Result := RoundTo(PDouble(FColBind^.buffer)^, FColBind^.decimals*-1)
                            else Result := PDouble(FColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:   Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        ZSysUtils.SQLStrToFloatDef(PAnsiChar(FColBind^.buffer), 0, Result, FColBind^.length);
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        if ( FColBind^.length > 0 ) and
           (FColBind^.length < 29{Max Extended Length = 28 ??+#0} ) then
          RawToFloatDef(GetBlob(ColumnIndex).GetBuffer, AnsiChar('.'), 0, Result)
        else //avoid senceless processing
          Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
function TZAbstractMySQLPreparedResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := nil
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_TINY,
      FIELD_TYPE_SHORT,
      FIELD_TYPE_LONG,
      FIELD_TYPE_FLOAT,
      FIELD_TYPE_DOUBLE,
      FIELD_TYPE_NULL,
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_TIME, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE,
      FIELD_TYPE_LONGLONG,
      FIELD_TYPE_YEAR: Result := nil;
      FIELD_TYPE_BIT,//http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        Result := BufferToBytes(Pointer(FColBind^.buffer), FColBind^.length);
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        begin
          FTempBlob := GetBlob(ColumnIndex);
          result := FTempBlob.GetBytes;
          FTempBlob := nil;
        end
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLPreparedResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_BIT: //http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        Result := 0;
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PCardinal(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATE, FIELD_TYPE_DATETIME,
      FIELD_TYPE_NEWDATE:
        if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColBind^.buffer)^.Year,
            PMYSQL_TIME(FColBind^.buffer)^.Month,
            PMYSQL_TIME(FColBind^.buffer)^.Day, Result) then
          Result := encodeDate(1900, 1, 1);
      FIELD_TYPE_TIME: Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        if not TryEncodeDate(PWord(FColBind^.buffer)^, 1,1, Result) then
          Result := 0;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        begin
          if FColBind^.length = ConSettings^.ReadFormatSettings.DateFormatLen then
            Result := RawSQLDateToDateTime(PAnsiChar(FColBind^.buffer),
              FColBind^.length, ConSettings^.ReadFormatSettings, Failed)
          else
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
              RawSQLTimeStampToDateTime(PAnsiChar(FColBind^.buffer),
                FColBind^.length, ConSettings^.ReadFormatSettings, Failed));
          LastWasNull := Result = 0;
        end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractMySQLPreparedResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_BIT://http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        Result := 0;
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PCardinal(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE: Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME, FIELD_TYPE_TIME:
        if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColBind^.buffer)^.Hour,
            PMYSQL_TIME(FColBind^.buffer)^.Minute,
            PMYSQL_TIME(FColBind^.buffer)^.Second,
            0{PMYSQL_TIME(FColBind^.buffer)^.second_part}, Result) then
          Result := 0;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        if not TryEncodeDate(PWord(FColBind^.buffer)^, 1,1, Result) then
          Result := 0;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        begin
          if (PAnsiChar(FColBind^.buffer)+2)^ = ':' then //possible date if Len = 10 then
            Result := RawSQLTimeToDateTime(PAnsiChar(FColBind^.buffer),
              FColBind^.length, ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            Result := Frac(RawSQLTimeStampToDateTime(PAnsiChar(FColBind^.buffer),
              FColBind^.length, ConSettings^.ReadFormatSettings, Failed));
          LastWasNull := Result = 0;
        end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
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
function TZAbstractMySQLPreparedResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  tmp: TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := FColBind^.is_null =1;
  if LastWasNull then
    Result := 0
  else
    //http://dev.mysql.com/doc/refman/5.1/de/numeric-types.html
    Case FColBind^.buffer_type of
      FIELD_TYPE_BIT://http://dev.mysql.com/doc/refman/5.0/en/bit-type.html
        Result := 0;
      FIELD_TYPE_TINY:
        if FColBind^.is_signed then
          Result := PShortInt(FColBind^.buffer)^
        else
          Result := PByte(FColBind^.buffer)^;
      FIELD_TYPE_SHORT:
        if FColBind^.is_signed then
          Result := PSmallInt(FColBind^.buffer)^
        else
          Result := PWord(FColBind^.buffer)^;
      FIELD_TYPE_LONG:
        if FColBind^.is_signed then
          Result := PInteger(FColBind^.buffer)^
        else
          Result := PLongWord(FColBind^.buffer)^;
      FIELD_TYPE_FLOAT:     Result := PSingle(FColBind^.buffer)^;
      FIELD_TYPE_DOUBLE:    Result := PDouble(FColBind^.buffer)^;
      FIELD_TYPE_NULL:      Result := 0;
      FIELD_TYPE_DATE, FIELD_TYPE_NEWDATE:
        if not sysUtils.TryEncodeDate(
            PMYSQL_TIME(FColBind^.buffer)^.Year,
            PMYSQL_TIME(FColBind^.buffer)^.Month,
            PMYSQL_TIME(FColBind^.buffer)^.Day, Result) then
          Result := encodeDate(1900, 1, 1);
      FIELD_TYPE_TIME:
        if not sysUtils.TryEncodeTime(
            PMYSQL_TIME(FColBind^.buffer)^.Hour,
            PMYSQL_TIME(FColBind^.buffer)^.Minute,
            PMYSQL_TIME(FColBind^.buffer)^.Second,
            0{PMYSQL_TIME(FColBind^.buffer)^.second_part}, Result) then
          Result := 0;
      FIELD_TYPE_TIMESTAMP, FIELD_TYPE_DATETIME:
        begin
          if not sysUtils.TryEncodeDate(
              PMYSQL_TIME(FColBind^.buffer)^.Year,
              PMYSQL_TIME(FColBind^.buffer)^.Month,
              PMYSQL_TIME(FColBind^.buffer)^.Day, tmp) then
            tmp := encodeDate(1900, 1, 1);
          if not sysUtils.TryEncodeTime(
              PMYSQL_TIME(FColBind^.buffer)^.Hour,
              PMYSQL_TIME(FColBind^.buffer)^.Minute,
              PMYSQL_TIME(FColBind^.buffer)^.Second,
              0{PMYSQL_TIME(FColBind^.buffer)^.second_part}, Result) then
            Result := 0;
          Result := Result + tmp;
        end;
      FIELD_TYPE_LONGLONG:
        if FColBind^.is_signed then
          Result := PInt64(FColBind^.buffer)^
        else
          Result := PUInt64(FColBind^.buffer)^;
      FIELD_TYPE_YEAR:
        if not TryEncodeDate(PWord(FColBind^.buffer)^, 1,1, Result) then
          Result := 0;
      FIELD_TYPE_STRING,
      FIELD_TYPE_ENUM, FIELD_TYPE_SET:
        begin
          if (PAnsiChar(FColBind^.buffer)+2)^ = ':' then
            Result := RawSQLTimeToDateTime(PAnsiChar(FColBind^.buffer),
              FColBind^.length, ConSettings^.ReadFormatSettings, Failed{%H-})
          else
            if (ConSettings^.ReadFormatSettings.DateTimeFormatLen < FColBind^.length) or (ConSettings^.ReadFormatSettings.DateTimeFormatLen - FColBind^.length <= 4) then
              Result := RawSQLTimeStampToDateTime(PAnsiChar(FColBind^.buffer), FColBind^.length, ConSettings^.ReadFormatSettings, Failed)
            else
              Result := RawSQLDateToDateTime(PAnsiChar(FColBind^.buffer), FColBind^.length, ConSettings^.ReadFormatSettings, Failed);
          LastWasNull := Result = 0;
        end;
      FIELD_TYPE_TINY_BLOB, FIELD_TYPE_MEDIUM_BLOB, FIELD_TYPE_LONG_BLOB,
      FIELD_TYPE_BLOB, FIELD_TYPE_GEOMETRY:
        Result := 0;
      else
        raise EZSQLException.Create(Format(SErrorConvertionField,
          ['Field '+ZFastCode.IntToStr(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}),
            DefineColumnTypeName(GetMetadata.GetColumnType(ColumnIndex{$IFNDEF GENERIC_INDEX}+1{$ENDIF}))]));
    end
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractMySQLPreparedResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  RawTemp: RawByteString;
begin
  Result := nil;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}

  LastWasNull := IsNull(ColumnIndex);
  FColBind := @FColumnArray[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  if not LastWasNull then
    case FColBind^.buffer_type of
      FIELD_TYPE_BLOB,
      FIELD_TYPE_TINY_BLOB,
      FIELD_TYPE_MEDIUM_BLOB,
      FIELD_TYPE_LONG_BLOB,
      MYSQL_TYPE_JSON:
        if FColBind^.binary then
          Result := TZMySQLPreparedBlob.Create(FplainDriver,
            FColBind, FPrepStmt, ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF})
        else
          Result := TZMySQLPreparedClob.Create(FplainDriver,
            FColBind, FPrepStmt, ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, ConSettings);
      else
        begin
          RawTemp := InternalGetString(ColumnIndex);
          Result := TZAbstractClob.CreateWithData(PAnsiChar(RawTemp), Length(RawTemp),
            ConSettings^.ClientCodePage^.CP, ConSettings);
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
function TZAbstractMySQLPreparedResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if Closed or ((MaxRows > 0) and (RowNo >= MaxRows)) then
    Exit;

  if FPlainDriver.stmt_fetch(FPrepStmt) in [0, MYSQL_DATA_TRUNCATED] then
  begin
    RowNo := RowNo + 1;
    if LastRowNo < RowNo then
      LastRowNo := RowNo;
    Result := True;
  end
  else
  begin
    if RowNo <= LastRowNo then
      RowNo := LastRowNo + 1;
    Result := False;
  end;
end;

{
procedure TZAbstractMySQLPreparedResultSet.ResetCursor;
begin
  if Assigned(FResultMetaData) then
  begin
    FPlainDriver.FreeResult(FResultMetaData);
    FResultMetaData := nil;
  end;
  FResultMetaData := nil;
  if Assigned(FPrepStmt) then
  begin
    FPlainDriver.FreePreparedResult(FPrepStmt);
    while(FPlainDriver.GetPreparedNextResult(FPrepStmt) = 0) do
      FPlainDriver.FreePreparedResult(FPrepStmt);
    FPrepStmt := nil;
  end;
  inherited ResetCursor;
end;
}

{ TZMySQL_Store_PreparedResultSet }

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
function TZMySQL_Store_PreparedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  CheckClosed;

  { Checks for maximum row. }
  Result := False;
  if (MaxRows > 0) and (Row > MaxRows) then
    Exit;

  { Process negative rows. }
  if Row < 0 then
  begin
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end;

  if (Row >= 0) and (Row <= LastRowNo + 1) then
  begin
    RowNo := Row;
    if (Row >= 1) and (Row <= LastRowNo) then
    begin
      FPlainDriver.stmt_data_seek(FPrepStmt, RowNo - 1);
      Result := FPlainDriver.stmt_fetch(FPrepStmt) = 0;
    end;
  end;
end;

procedure TZMySQL_Store_PreparedResultSet.ResetCursor;
begin
  inherited ResetCursor;
  if Assigned(FPrepStmt) then
    FPlainDriver.stmt_free_result(FPrepStmt);
end;

{ TZMySQL_Use_PreparedResultSet }

procedure TZMySQL_Use_PreparedResultSet.ResetCursor;
begin
  if FPrepStmt <> nil then
    {need to fetch all temporary until handle = nil else all other queries are out of sync
     see: http://dev.mysql.com/doc/refman/5.0/en/mysql-use-result.html}
    while FPlainDriver.stmt_fetch(FPrepStmt) in [0, MYSQL_DATA_TRUNCATED] do;
  inherited ResetCursor;
end;

{ TZMySQLCachedResolver }

{**
  Creates a MySQL specific cached resolver object.
  @param PlainDriver a native MySQL plain driver.
  @param Handle a MySQL specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZMySQLCachedResolver.Create(const PlainDriver: IZMySQLPlainDriver;
  Handle: PMySQL; const Statement: IZStatement; const Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);
  FPlainDriver := PlainDriver;
  FHandle := Handle;

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := InvalidDbcIndex;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
  end;
end;

{**
  Forms a where clause for UPDATE or DELETE DML statements.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZMySQLCachedResolver.FormWhereClause(Columns: TObjectList;
  OldRowAccessor: TZRowAccessor): string;
var
  I, N: Integer;
  Current: TZResolverParameter;
begin
  Result := '';
  N := Columns.Count - WhereColumns.Count;

  for I := 0 to WhereColumns.Count - 1 do
  begin
    Current := TZResolverParameter(WhereColumns[I]);
    if Result <> '' then
      Result := Result + ' AND ';

    Result := Result + IdentifierConvertor.Quote(Current.ColumnName);
    if OldRowAccessor.IsNull(Current.ColumnIndex) then
    begin
      if not (Metadata.IsNullable(Current.ColumnIndex) = ntNullable) then
        case OldRowAccessor.GetColumnType(Current.ColumnIndex) of
          stDate:
            if I > 0 then
            begin
              Current := TZResolverParameter(WhereColumns[I-1]);
              Result := Result+ '=''0000-00-00'' OR '+Result + ' IS NULL';
              Columns.Add(TZResolverParameter.Create(Current.ColumnIndex,
              Current.ColumnName, Current.ColumnType, Current.NewValue, ''));
            end;
          stTime:
            if I > 0 then
            begin
              Current := TZResolverParameter(WhereColumns[I-1]);
              Result := Result+ '=''00:00:00'' OR '+Result + ' IS NULL';
              Columns.Add(TZResolverParameter.Create(Current.ColumnIndex,
              Current.ColumnName, Current.ColumnType, Current.NewValue, ''));
            end;
          stTimeStamp:
            if I > 0 then
            begin
              Current := TZResolverParameter(WhereColumns[I-1]);
              Result := Result+ '=''0000-00-00 00:00:00'' OR '+Result + ' IS NULL';
              Columns.Add(TZResolverParameter.Create(Current.ColumnIndex,
              Current.ColumnName, Current.ColumnType, Current.NewValue, ''));
            end;
          else
            Result := Result + ' IS NULL';
        end
      else
        Result := Result + ' IS NULL ';
      Columns.Delete(N);
    end
    else
    begin
      Result := Result + '=?';
      Inc(N);
    end;
  end;

  if Result <> '' then
    Result := ' WHERE ' + Result;
end;
{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZMySQLCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);
  if (UpdateType = utInserted) then
    UpdateAutoIncrementFields(Sender, UpdateType, OldRowAccessor, NewRowAccessor, Self);
end;

{**
 Do Tasks after Post updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // readonly dataset - parameter not used intentionally
procedure TZMySQLCachedResolver.UpdateAutoIncrementFields(
  Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver);
var
  Plaindriver : IZMysqlPlainDriver;
  LastWasNull: Boolean;
begin
  if not ((FAutoColumnIndex {$IFDEF GENERIC_INDEX}>={$ELSE}>{$ENDIF} 0) and
          (OldRowAccessor.IsNull(FAutoColumnIndex) or (OldRowAccessor.GetULong(FAutoColumnIndex, LastWasNull{%H-})=0))) then
     exit;
  Plaindriver := (Connection as IZMysqlConnection).GetPlainDriver;
  // THIS IS WRONG, I KNOW (MDAEMS) : which function to use depends on the insert statement, not the resultset statement
  {  IF FStatement.IsPreparedStatement  then
    NewRowAccessor.SetULong(FAutoColumnIndex, PlainDriver.GetPreparedInsertID(FStatement.GetStmtHandle))
  else}
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    NewRowAccessor.SetULong(FAutoColumnIndex, PlainDriver.GetLastInsertID(FHandle));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZMySQLCachedResolver.FormCalculateStatement(
  Columns: TObjectList): string;
var
  I: Integer;
  Current: TZResolverParameter;
begin
  Result := '';
  if Columns.Count = 0 then
     Exit;

  for I := 0 to Columns.Count - 1 do
  begin
    Current := TZResolverParameter(Columns[I]);
    if Result <> '' then
      Result := Result + ',';
    if Current.DefaultValue <> '' then
      Result := Result + Current.DefaultValue
    else
      Result := Result + 'NULL';
  end;
  Result := 'SELECT ' + Result;
end;

{ TZMySQLPreparedClob }
constructor TZMySQLPreparedClob.Create(const PlainDriver: IZMySQLPlainDriver;
  Bind: PDOBindRecord2; StmtHandle: PMySql_Stmt;
  ColumnIndex: Cardinal; ConSettings: PZConSettings);
var
  offset: ULong;
begin
  inherited Create;
  FConSettings := ConSettings;
  FCurrentCodePage := ConSettings^.ClientCodePage^.CP;
  FBlobSize := Bind^.Length+1; //MySQL sets a trailing #0 on top of data
  GetMem(FBlobData, FBlobSize);
  offset := 0;
  Bind^.buffer_Length_address^ := Bind^.Length; //indicate size of Buffer
  Bind^.buffer_address^ := FBlobData;
  PlainDriver.stmt_fetch_column(StmtHandle, bind^.mysql_bind, ColumnIndex, offset); //move data to buffer
  Bind^.buffer_address^ := nil; //set nil again
End;

{ TZMySQLPreparedBlob }
constructor TZMySQLPreparedBlob.Create(const PlainDriver: IZMySQLPlainDriver;
  Bind: PDOBindRecord2; StmtHandle: PMySql_Stmt;
  ColumnIndex: Cardinal);
var
  offset: ULong;
begin
  inherited Create;
  FBlobSize := Bind^.Length;
  GetMem(FBlobData, FBlobSize);
  offset := 0;
  Bind^.buffer_Length_address^ := Bind^.Length; //indicate size of Buffer
  Bind^.buffer_address^ := FBlobData;
  PlainDriver.stmt_fetch_column(StmtHandle, bind^.mysql_bind, ColumnIndex, offset); //move data to buffer
  Bind^.buffer_address^ := nil; //set nil again
End;

{ TZMySQL_Use_ResultSet }

procedure TZMySQL_Use_ResultSet.ResetCursor;
begin
  if FQueryHandle <> nil then
    {need to fetch all temporary until handle = nil else all other queries are out of sync
     see: http://dev.mysql.com/doc/refman/5.0/en/mysql-use-result.html}
    while FPlainDriver.FetchRow(FQueryHandle) <> nil do;
  inherited ResetCursor;
  FQueryHandle := nil;
end;

{$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
end.
