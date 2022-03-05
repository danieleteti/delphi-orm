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

unit ZDbcMySqlStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZClasses, ZDbcIntfs, ZDbcStatement, ZDbcMySql, ZVariant, ZPlainMySqlDriver,
  ZPlainMySqlConstants, ZCompatibility, ZDbcLogging, ZDbcUtils;

type
  {** Implements Prepared MySQL Statement. }
  TZMySQLEmulatedPreparedStatement = class(TZEmulatedPreparedStatement_A)
  private
    fMySQLConnection: IZMySQLConnection;
    FPlainDriver: IZMySQLPlainDriver;
    FUseDefaults, FUseResult: Boolean;
    FResultsCount: Integer;
    fMySQL: PMySQL;
    function CreateResultSet(const SQL: string): IZResultSet;
    procedure FlushPendingResults;
  protected
    function GetParamAsString(ParamIndex: Integer): RawByteString; override;
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver;
      const Connection: IZConnection; const SQL: string; Info: TStrings;
      Handle: PMySQL); overload;
    constructor Create(const PlainDriver: IZMySQLPlainDriver;
      const Connection: IZConnection; const SQL: string; Info: TStrings); overload;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; override;
    function GetUpdateCount: Integer; override;

    procedure Unprepare; override;
    procedure Prepare; override;
  end;

  {** Implements MySQL Statement. }
  TZMySQLStatement = class(TZMySQLEmulatedPreparedStatement)
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver;
      const Connection: IZConnection; const Info: TStrings; Handle: PMySQL); overload;
  end;

  TZMysqlColumnBuffer = Array of TDOBindRecord2;
  { TZMySQLBindBuffer }
  {** Encapsulates a MySQL bind buffer. }
  TZMySQLAbstractBindBuffer = class(TZAbstractObject)
  protected
    FAddedColumnCount : Integer;
    FBindOffsets: TMYSQL_BINDOFFSETS;
    FBindArray: TByteDynArray;
    FPColumnArray: ^TZMysqlColumnBuffer;
  public
    constructor Create(PlainDriver:IZMysqlPlainDriver;
      const BindCount : Integer; var ColumnArray: TZMysqlColumnBuffer); virtual;
    function GetBufferAddress : Pointer;
  end;

  {** Encapsulates a MySQL bind buffer for ResultSets. }
  TZMySQLResultSetBindBuffer = class(TZMySQLAbstractBindBuffer)
  public
    procedure AddColumn(MYSQL_FIELD: PMYSQL_FIELD; FieldOffSets: PMYSQL_FIELDOFFSETS);
  end;

  {** Encapsulates a MySQL bind buffer for updates. }
  TZMySQLParamBindBuffer = class(TZMySQLAbstractBindBuffer)
  public
    procedure AddColumn(buffertype: TMysqlFieldType; field_length: integer;
      is_signed: Boolean);
  end;
  {** Implements Prepared SQL Statement. }

  { TZMySQLPreparedStatement }
  TZMySQLPreparedStatement = class(TZAbstractPreparedStatement)
  private
    fMySQL: PMySQL;
    FMySQLConnection: IZMySQLConnection;
    FMYSQL_STMT: PMYSQL_STMT;
    FPlainDriver: IZMySQLPlainDriver;
    FUseResult, FUseDefaults: Boolean;
    FPreparablePrefixTokens: TPreparablePrefixTokens;
    FColumnArray: TZMysqlColumnBuffer;
    FParamBindBuffer: TZMySQLParamBindBuffer;
    FPrefetchRows: Ulong; //Number of rows to fetch from server at a time when using a cursor.
    FResultsCount: Integer;
    function CreateResultSet(const SQL: string): IZResultSet;
    procedure FlushPendingResults;
    function GetFieldType(SQLType: TZSQLType; Var Signed: Boolean;
      MySQL_FieldType_Bit_1_IsBoolean: Boolean): TMysqlFieldType;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function GetCompareFirstKeywordStrings: TPreparablePrefixTokens; override;
    procedure ReleaseConnection; override;
  public
    constructor Create(const PlainDriver: IZMysqlPlainDriver; const Connection: IZConnection;
      const SQL: string; Info: TStrings);
    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; override;
    function GetUpdateCount: Integer; override;
  end;

  {** Implements callable Postgresql Statement. }
  TZMySQLCallableStatement = class(TZAbstractCallableStatement,
    IZParamNamedCallableStatement)
  private
    FPlainDriver: IZMysqlPlainDriver;
    fMySQL: PMySQL;
    FQueryHandle: PZMySQLResult;
    FUseResult: Boolean;
    FParamNames: array [0..1024] of RawByteString;
    FParamTypeNames: array [0..1024] of RawByteString;
    FUseDefaults: Boolean;
    function GetCallSQL: RawByteString;
    function GetOutParamSQL: RawByteString;
    function GetSelectFunctionSQL: RawByteString;
    function PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
  protected
    procedure ClearResultSets; override;
    procedure BindInParameters; override;
    function CreateResultSet(const SQL: string): IZResultSet;
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      ParamTypeName: String; const ParamName: String; Const ColumnSize, {%H-}Precision: Integer);
  public
    constructor Create(const PlainDriver: IZMySQLPlainDriver;
      const Connection: IZConnection; const SQL: string; const Info: TStrings;
      Handle: PMySQL);

    function Execute(const SQL: RawByteString): Boolean; override;
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;

    function IsUseResult: Boolean;
    function IsPreparedStatement: Boolean;

    function GetFirstResultSet: IZResultSet; override;
    function GetPreviousResultSet: IZResultSet; override;
    function GetNextResultSet: IZResultSet; override;
    function GetLastResultSet: IZResultSet; override;
    function BOR: Boolean; override;
    function EOR: Boolean; override;
    function GetResultSetByIndex(const Index: Integer): IZResultSet; override;
    function GetResultSetCount: Integer; override;
    function GetMoreResults: Boolean; override;
  end;

{$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit

uses
  Math, DateUtils, ZFastCode, ZDbcMySqlUtils, ZDbcMySqlResultSet,
  ZSysUtils, ZMessages, ZDbcCachedResultSet, ZEncoding, ZDbcResultSet
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND};

var
  MySQL41PreparableTokens: TPreparablePrefixTokens;
  MySQL50PreparableTokens: TPreparablePrefixTokens;
  MySQL5015PreparableTokens: TPreparablePrefixTokens;
  MySQL5023PreparableTokens: TPreparablePrefixTokens;
  MySQL51PreparableTokens: TPreparablePrefixTokens absolute MySQL5015PreparableTokens; //equals
  MySQL5110PreparableTokens: TPreparablePrefixTokens absolute MySQL5023PreparableTokens; //equals
  MySQL5112PreparableTokens: TPreparablePrefixTokens;
  MySQL55PreparableTokens: TPreparablePrefixTokens absolute MySQL5112PreparableTokens; //equals
  MySQL56PreparableTokens: TPreparablePrefixTokens absolute MySQL55PreparableTokens; //equals
  MySQL568PreparableTokens: TPreparablePrefixTokens;

{ TZMySQLEmulatedPreparedStatement }

procedure TZMySQLEmulatedPreparedStatement.Prepare;
begin
  inherited Prepare;
  FlushPendingResults;
end;

procedure TZMySQLEmulatedPreparedStatement.Unprepare;
begin
  if IsPrepared then begin
    inherited Unprepare;
    FlushPendingResults;
  end;
  FResultsCount := 0;
end;

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL Plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZMySQLEmulatedPreparedStatement.Create(
  const PlainDriver: IZMySQLPlainDriver; const Connection: IZConnection;
  const SQL: string; Info: TStrings; Handle: PMySQL);
begin
  //Keep the old constructors
  Create(PlainDriver, Connection, SQL, Info);
end;

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL Plain driver.
  @param Connection a database connection object.
  @param SQL a statement.
  @param Info a statement parameters.
}
constructor TZMySQLEmulatedPreparedStatement.Create(const PlainDriver: IZMySQLPlainDriver;
  const Connection: IZConnection; const SQL: string; Info: TStrings);
begin
  inherited create(Connection, SQL, Info);
  fMySQLConnection := Connection as IZMySQLConnection;
  fMySQL := fMySQLConnection.GetConnectionHandle;
  FPlainDriver := PlainDriver;
  FUseResult := StrToBoolEx(DefineStatementParameter(Self, 'useresult', 'false'));
  FUseDefaults := StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true'));
  if not FUseResult then
    ResultSetType := rtScrollInsensitive;
  FResultsCount := 0;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLEmulatedPreparedStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZAbstractResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  if (FResultsCount = 1) and (FOpenResultSet <> nil) and (FOpenResultSet <> Pointer(LastResultSet)) then begin
    Result := IZResultSet(FOpenResultSet);
    if fUseResult and ((GetResultSetConcurrency = rcUpdatable) or
       (GetResultSetType = rtScrollInsensitive)) then begin
      Result.Last; //invoke fetch all -> note this is done on msql_strore_result too
      Result.BeforeFirst;
    end;
  end else begin
    if FUseResult //server cursor?
    then NativeResultSet := TZMySQL_Use_ResultSet.Create(FPlainDriver, Self, SQL, fMySQL, nil)
    else NativeResultSet := TZMySQL_Store_ResultSet.Create(FPlainDriver, Self, SQL, fMySQL, nil);
    if (GetResultSetConcurrency = rcUpdatable) or
       ((GetResultSetType = rtScrollInsensitive) and FUseResult) then begin
      if (GetResultSetConcurrency = rcUpdatable)
      then CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver,
          fMySQL, Self, NativeResultSet.GetMetaData)
      else CachedResolver := nil;
      CachedResultSet := TZCachedResultSet.CreateWithColumns(NativeResultSet.ColumnsInfo,
        NativeResultSet, SQL, CachedResolver, ConSettings);
      if fUseResult then begin
        CachedResultSet.Last; //invoke fetch all -> note this is done on msql_strore_result too
        CachedResultSet.BeforeFirst;
      end;
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      Result := CachedResultSet;
      Result.GetMetadata.IsWritable(FirstDbcIndex); //force metadata loading
    end else
      Result := NativeResultSet;
    Inc(FResultsCount);
  end;
end;


{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZMySQLEmulatedPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var RSQL: RawByteString;
begin
  PrepareOpenResultSetForReUse;
  Prepare;
  RSQL := ComposeRawSQLQuery;
  if FPlainDriver.ExecRealQuery(fMySQL, Pointer(RSQL), Length(RSQL)) = 0 then begin
    if FPlainDriver.Field_Count(fMySQL) = 0 then
      if GetMoreResults
      then Result := LastResultSet
      else raise EZSQLException.Create(SCanNotOpenResultSet)
    else Result := CreateResultSet(SQL);
    FOpenResultSet := Pointer(Result);
  end else
    CheckMySQLError(FPlainDriver, fMySQL, lcExecute, RSQL, ConSettings);
  inherited ExecuteQueryPrepared;
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZMySQLEmulatedPreparedStatement.ExecuteUpdatePrepared: Integer;
var
  QueryHandle: PZMySQLResult;
  RSQL: RawByteString;
begin
  Result := -1;
  Prepare;
  RSQL := ComposeRawSQLQuery;
  if FPlainDriver.ExecRealQuery(fMySQL, Pointer(RSQL), Length(RSQL)) = 0 then begin
    { Process queries with result sets }
    if FPlainDriver.Field_Count(fMySQL) > 0 then begin
      QueryHandle := FPlainDriver.StoreResult(fMySQL);
      if QueryHandle <> nil then begin
        Result := FPlainDriver.GetRowCount(QueryHandle);
        FPlainDriver.FreeResult(QueryHandle);
      end else
        Result := FPlainDriver.GetAffectedRows(fMySQL);
      while(FPlainDriver.RetrieveNextRowset(fMySQL) = 0) do begin
        QueryHandle := FPlainDriver.StoreResult(fMySQL);
        if QueryHandle <> nil
        then FPlainDriver.FreeResult(QueryHandle);
      end;
    end
  { Process regular query }
    else
      Result := FPlainDriver.GetAffectedRows(fMySQL);
  end else
    CheckMySQLError(FPlainDriver, fMySQL, lcExecute, RSQL, ConSettings);
  LastUpdateCount := Result;
  Inherited ExecutePrepared;
end;

procedure TZMySQLEmulatedPreparedStatement.FlushPendingResults;
var FQueryHandle: PZMySQLResult;
begin
  while (FPlainDriver.RetrieveNextRowset(fMySQL) = 0) do begin
    Inc(FResultsCount);
    FQueryHandle := FPlainDriver.StoreResult(fMySQL);
    if FQueryHandle <> nil then
      FPlainDriver.FreeResult(FQueryHandle);
  end;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZMySQLEmulatedPreparedStatement.ExecutePrepared: Boolean;
var
  RSQL: RawByteString;
begin
  Result := False;
  Prepare;
  RSQL := ComposeRawSQLQuery;
  if FPlainDriver.ExecRealQuery(fMySQL, Pointer(RSQL), Length(RSQL)) = 0 then begin
    if FPlainDriver.Field_Count(fMySQL) > 0 then begin
      { Process queries with result sets }
      Result := True;
      LastResultSet := CreateResultSet(Self.SQL);
      FOpenResultSet := Pointer(LastResultSet);
    end else { Processes regular query. }
      LastUpdateCount := FPlainDriver.GetAffectedRows(fMySQL);
  end else
    CheckMySQLError(FPlainDriver, fMySQL, lcExecute, RSQL, ConSettings);
  inherited ExecutePrepared;
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
function TZMySQLEmulatedPreparedStatement.GetMoreResults: Boolean;
begin
  Result := False;
  if (FOpenResultSet <> nil)
  then IZResultSet(FOpenResultSet).Close;
  if FPlainDriver.GetClientVersion >= 40100 then begin
    if FPlainDriver.RetrieveNextRowset(fMySQL) > 0
    then CheckMySQLError(FPlainDriver, fMySQL, lcExecute, ASQL, ConSettings);

    FResultsCount := 0; //Reset -> user is expecting more resultsets
    LastResultSet := nil;
    LastUpdateCount := -1;
    if FPlainDriver.Field_Count(fMySQL) > 0 then begin
      Result := True;
      LastResultSet := CreateResultSet(Self.SQL);
    end else
      LastUpdateCount := FPlainDriver.GetAffectedRows(fMySQL);
  end;
end;

function TZMySQLEmulatedPreparedStatement.GetParamAsString(ParamIndex: Integer): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := ZDbcMySQLUtils.MySQLPrepareAnsiSQLParam(fMySQLConnection,
    InParamValues[ParamIndex], InParamDefaultValues[ParamIndex], ClientVarManager,
    InParamTypes[ParamIndex], FUseDefaults);
end;

{**
  Returns the current result as an update count;
  if the result is a <code>ResultSet</code> object or there are no more results, -1
  is returned. This method should be called only once per result.

  @return the current result as an update count; -1 if the current result is a
    <code>ResultSet</code> object or there are no more results
  @see #execute
}
function TZMySQLEmulatedPreparedStatement.GetUpdateCount: Integer;
begin
  Result := inherited GetUpdateCount;
  if (Result = -1) and (FPlainDriver.Field_Count(fMySQL) = 0) then begin
    LastUpdateCount := FPlainDriver.GetAffectedRows(fMySQL);
    Result := LastUpdateCount;
  end;
end;

{ TZMySQLPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZMySQLPreparedStatement.Create(
  const PlainDriver: IZMySQLPlainDriver; const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  if PlainDriver.GetClientVersion      < 40100 then
    FPreparablePrefixTokens := nil
  else if PlainDriver.GetClientVersion < 50000 then
    FPreparablePrefixTokens := MySQL41PreparableTokens
  else if PlainDriver.GetClientVersion < 50015 then
    FPreparablePrefixTokens := MySQL50PreparableTokens
  else if PlainDriver.GetClientVersion < 50023 then
    FPreparablePrefixTokens := MySQL5015PreparableTokens
  else if PlainDriver.GetClientVersion < 50100 then
    FPreparablePrefixTokens := MySQL5023PreparableTokens
  else if PlainDriver.GetClientVersion < 50110 then
    FPreparablePrefixTokens := MySQL51PreparableTokens
  else if PlainDriver.GetClientVersion < 50112 then
    FPreparablePrefixTokens := MySQL5110PreparableTokens
  else if PlainDriver.GetClientVersion < 50500 then
    FPreparablePrefixTokens := MySQL5112PreparableTokens
  else if PlainDriver.GetClientVersion < 50600 then
    FPreparablePrefixTokens := MySQL55PreparableTokens
  else if PlainDriver.GetClientVersion < 50608 then
    FPreparablePrefixTokens := MySQL56PreparableTokens
  else
    FPreparablePrefixTokens := MySQL568PreparableTokens;

  inherited Create(Connection, SQL, Info);
  FMySQLConnection := Connection as IZMySQLConnection;
  fMySQL := FMysqlConnection.GetConnectionHandle;
  FPlainDriver := PlainDriver;

  FUseResult := StrToBoolEx(DefineStatementParameter(Self, 'useresult', 'false'));
  if not FUseResult then
    ResultSetType := rtScrollInsensitive;
  FUseDefaults := StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true'));
  FPrefetchRows := Max(1,{$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(DefineStatementParameter(Self, 'prefetch_rows', '100'),100));
end;

const FSTMT_ATTR_CURSOR_TYPE: ULong = Ord(CURSOR_TYPE_READ_ONLY);
procedure TZMySQLPreparedStatement.Prepare;
begin
  FlushPendingResults;
  if not Prepared then begin
    FMYSQL_STMT := FPlainDriver.stmt_init(fMySQL);
    if (FMYSQL_STMT = nil) then begin
      CheckMySQLPrepStmtError(FPlainDriver, FMYSQL_STMT, lcPrepStmt,
        ConvertZMsgToRaw(SFailedtoInitPrepStmt, ZMessages.cCodePage,
          ConSettings^.ClientCodePage^.CP), ConSettings);
      exit;
    end;
    if (FPlainDriver.stmt_prepare(FMYSQL_STMT, Pointer(ASQL), length(ASQL)) <> 0) then begin
      CheckMySQLPrepStmtError(FPlainDriver, FMYSQL_STMT, lcPrepStmt,
        ConvertZMsgToRaw(SFailedtoPrepareStmt,
        ZMessages.cCodePage, ConSettings^.ClientCodePage^.CP), ConSettings);
      exit;
    end;
    //see user comment: http://dev.mysql.com/doc/refman/5.0/en/mysql-stmt-fetch.html
    if FUseResult and (FPlainDriver.GetClientVersion >= 50020 ) then //supported since 5.0.2
      FPlainDriver.stmt_attr_set(FMYSQL_STMT, STMT_ATTR_CURSOR_TYPE, @FSTMT_ATTR_CURSOR_TYPE); //we need this to be able to use more than !one! stmt -> keep cached
    if FPlainDriver.GetClientVersion >= 50060 then //supported since 5.0.6
      FPlainDriver.stmt_attr_set(FMYSQL_STMT, STMT_ATTR_PREFETCH_ROWS, @FPrefetchRows); //try achieve best performnce. No idea how to calculate it
    LogPrepStmtMessage(lcPrepStmt, ASQL);
    inherited Prepare;
  end;
end;

procedure TZMySQLPreparedStatement.Unprepare;
begin
  inherited Unprepare;
  if FMYSQL_STMT <> nil then begin
    FlushPendingResults;
    //cancel all pending results:
    //https://mariadb.com/kb/en/library/mysql_stmt_close/
    FPlainDriver.stmt_close(FMYSQL_STMT);
    FMYSQL_STMT := nil;
  end;
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
function TZMySQLPreparedStatement.GetMoreResults: Boolean;
begin
  Result := False;
  if (FOpenResultSet <> nil)
  then IZResultSet(FOpenResultSet).Close;
  if (FPlainDriver.GetClientVersion >= 40100) and Assigned(FMYSQL_STMT) then begin
    if FPlainDriver.stmt_next_result(FMYSQL_STMT) > 0
    then checkMySQLPrepStmtError(FPlainDriver, FMYSQL_STMT, lcExecute, ASQL, ConSettings);

    FResultsCount := 0; //Reset -> user is expecting more resultsets
    LastResultSet := nil;
    LastUpdateCount := -1;
    if FPlainDriver.stmt_field_count(FMYSQL_STMT) > 0 then begin
      Result := True;
      LastResultSet := CreateResultSet(Self.SQL);
    end else
      LastUpdateCount := FPlainDriver.stmt_affected_rows(FMYSQL_STMT);
  end;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLPreparedStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZAbstractResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  if (FResultsCount = 1) and (FOpenResultSet <> nil) and (FOpenResultSet <> Pointer(LastResultSet)) then begin
    Result := IZResultSet(FOpenResultSet);
    if fUseResult and ((GetResultSetConcurrency = rcUpdatable) or
       (GetResultSetType = rtScrollInsensitive)) then begin
      Result.Last; //invoke fetch all -> note this is done on msql_strore_result too
      Result.BeforeFirst;
    end;
  end else begin
    if FUseResult
    then NativeResultSet := TZMySQL_Use_PreparedResultSet.Create(FPlainDriver, Self, SQL, fMySQL, FMYSQL_STMT)
    else NativeResultSet := TZMySQL_Store_PreparedResultSet.Create(FPlainDriver, Self, SQL, fMySQL, FMYSQL_STMT);
    if (GetResultSetConcurrency = rcUpdatable) or
       ((GetResultSetType = rtScrollInsensitive) and FUseResult) then begin
      if (GetResultSetConcurrency = rcUpdatable)
      then CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver,
          fMySQL, Self, NativeResultSet.GetMetaData)
      else CachedResolver := nil;
      CachedResultSet := TZCachedResultSet.CreateWithColumns(NativeResultSet.ColumnsInfo,
        NativeResultSet, SQL, CachedResolver, ConSettings);
      if fUseResult then begin
        CachedResultSet.Last; //invoke fetch all -> note this is done on msql_strore_result too
        CachedResultSet.BeforeFirst;
      end;
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      Result := CachedResultSet;
      Result.GetMetadata.IsWritable(FirstDbcIndex); //force metadata loading
    end else
      Result := NativeResultSet;
    FOpenResultSet := Pointer(Result);
    Inc(FResultsCount);
  end;
end;

procedure TZMysqlPreparedStatement.PrepareInParameters;
var
  I: Integer;
  MySQLType: TMysqlFieldType;
  Signed: Boolean;
begin
  { Initialize Bind Array and Column Array }
  Assert(FPlainDriver.stmt_param_count(FMYSQL_STMT) = ULong(InParamCount), SInvalidInputParameterCount);
  FParamBindBuffer := TZMySqlParamBindBuffer.Create(FPlainDriver,InParamCount,FColumnArray);
  for i := 0 to InParamCount -1 do
  begin
    MySQLType := GetFieldType(InParamTypes[i], Signed{%H-}, FMySQLConnection.MySQL_FieldType_Bit_1_IsBoolean);
    FParamBindBuffer.AddColumn(MySQLType, getMySQLFieldSize(MySQLType, ChunkSize), Signed);
  end;
  if (FPlainDriver.stmt_bind_param(FMYSQL_STMT, FParamBindBuffer.GetBufferAddress) <> 0) then
    checkMySQLPrepStmtError (FPlainDriver, FMYSQL_STMT, lcPrepStmt,
      ConvertZMsgToRaw(SBindingFailure, ZMessages.cCodePage,
      ConSettings^.ClientCodePage^.CP), ConSettings);
end;

procedure TZMySQLPreparedStatement.ReleaseConnection;
begin
  inherited;
  FMySQLConnection := nil;
end;

{$WARNINGS OFF} //Len & P might not be init...
procedure TZMysqlPreparedStatement.BindInParameters;
var
  PBuffer: Pointer;
  aTime: TDateTime;
  year, month, day, hour, minute, second, millisecond: word;
  I: integer;
  OffSet, PieceSize: LongWord;
  TempBlob: IZBlob;
  TempAnsi: RawByteString;
  CharRec: TZCharRec;
  ChunkedData: Boolean;
  P: PAnsiChar;
  Len: NativeUInt;

  bind: PDOBindRecord2;
label JmpClob, JmpInherited, JmpCharRec, JmpChunked;
begin
  ChunkedData := False;
  //http://dev.mysql.com/doc/refman/5.0/en/storage-requirements.html
  if (InParamCount = 0) then
     goto JmpInherited;
  For I := 0 to InParamCount - 1 do
  begin
    Bind := @FColumnArray[I];
    PBuffer := Pointer(Bind^.buffer);
    if (InParamValues[I].VType = vtNull) and FUseDefaults and (InParamDefaultValues[I] <> '') then
    begin
      {EH: Hint we're using the ClientVarManager for conversions. This works pretty fine
      except for default Date/TimeStamp-Values like "0000-00-00...."
      So i made this workaround here}
      ClientVarManager.SetAsString(InParamValues[I], Copy(InParamDefaultValues[I], 2, Length(InParamDefaultValues[I])-2)); //extract quotes
      Bind^.buffer_length_address^ := Max(Length(bind^.buffer), ChunkSize+1);
      SetLength(bind^.buffer, Bind^.buffer_length_address^);
      bind^.buffer_type_address^:= FIELD_TYPE_STRING;
      ChunkedData := True;
      goto JmpCharRec; //this skips reset of column-types!
    end;
    if InParamValues[i].vType = vtNull then
      FColumnArray[I].is_null := 1
    else
    begin
      FColumnArray[I].is_null := 0;
      {allways reset type and length if defaults(strings) have been bound before}
      bind^.buffer_address^ := PBuffer; //reset if send chunked before
      bind^.buffer_type_address^ := Bind^.buffer_type; //reset initial type
      Bind^.buffer_length_address^ := Length(Bind^.buffer); //reset Buffer_Length
      case Bind^.buffer_type of
        FIELD_TYPE_TINY:
          if Bind^.is_signed
          then PShortInt(PBuffer)^ := ClientVarManager.GetAsInteger(InParamValues[i])
          else PByte(PBuffer)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        FIELD_TYPE_SHORT:
          if Bind^.is_signed
          then PSmallInt(PBuffer)^ := ClientVarManager.GetAsInteger(InParamValues[i])
          else PWord(PBuffer)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        FIELD_TYPE_LONG:
          if Bind^.is_signed
          then PInteger(PBuffer)^ := ClientVarManager.GetAsInteger(InParamValues[i])
          else PCardinal(PBuffer)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        FIELD_TYPE_LONGLONG:
          if Bind^.is_signed
          then PInt64(PBuffer)^ := ClientVarManager.GetAsInteger(InParamValues[i])
          else PUInt64(PBuffer)^ := ClientVarManager.GetAsUInteger(InParamValues[i]);
        FIELD_TYPE_FLOAT: PSingle(PBuffer)^:= ClientVarManager.GetAsFloat(InParamValues[i]);
        FIELD_TYPE_DOUBLE: PDouble(PBuffer)^:= ClientVarManager.GetAsFloat(InParamValues[i]);
        FIELD_TYPE_STRING:
          case InParamTypes[i] of
            stBoolean:
              begin
                Bind^.Length := 1;
                if ClientVarManager.GetAsBoolean(InParamValues[i]) then
                  PByte(PBuffer)^ := Ord('Y')
                else
                  PByte(PBuffer)^ := Ord('N');
              end;
            stGUID:
              begin
                if InParamValues[i].vType = vtBytes then
                  InParamValues[i] := EncodeRawByteString({$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(GuidToString(PGUID(@InParamValues[i].vBytes[0])^)));
                goto JmpCharRec;
              end;
            stAsciiStream, stUnicodeStream:
              begin
                TempBlob := ClientVarManager.GetAsInterface(InParamValues[i]) as IZBlob;
                if TempBlob.IsEmpty then
                  FColumnArray[I].is_null := 1
                else
                  if TempBlob.IsClob then
                  begin
JmpClob:            P := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP); //set proper encoding if required
                    Bind^.Length := TempBlob.Length;
                    if Bind^.length > Cardinal(ChunkSize)-1  then
                    begin {out of buffer range}
                      {now we've to set the Buffer of binding-record to nil to indicate we send data as chunks}
JmpChunked:           Bind^.buffer_address^ := nil;
                      ChunkedData := True;
                    end else
                      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, PBuffer^, Bind^.Length);
                  end else begin
                    TempAnsi := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                              TempBlob.Length, ConSettings);
                    TempBlob := TZAbstractClob.CreateWithData(Pointer(TempAnsi), Length(TempAnsi),
                      ConSettings^.ClientCodePage.CP, ConSettings);
                    InParamValues[i].vInterface := TempBlob;
                    goto JmpClob;
                  end;
              end;
            else
              begin
JmpCharRec:     CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], ConSettings^.ClientCodePage^.CP);
                Bind^.length := CharRec.Len;
                if CharRec.Len > Cardinal(ChunkSize)-1 then
                  goto JmpChunked
                else {within buffer range}
                  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(CharRec.P^, PBuffer^, CharRec.Len); //trailing #0 included
              end;
          end;
        FIELD_TYPE_TINY_BLOB: {stBytes}
          begin
            P := Pointer(InParamValues[i].VBytes);
            Bind^.length := Length(InParamValues[i].VBytes);
            if Bind^.length > Cardinal(ChunkSize) then
              {out of buffer range}
              goto JmpChunked
            else
              if P = nil then
                Bind^.is_null := 1
              else
                {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, PBuffer^, CharRec.Len);
          end;
        FIELD_TYPE_DATE:
          begin
            FillChar(PBuffer^, SizeOf(TMYSQL_TIME), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
            aTime := ClientVarManager.GetAsDateTime(InParamValues[i]);
            DecodeDate(aTime, Year, Month, Day);
            PMYSQL_TIME(PBuffer)^.neg := Ord(aTime < 0);
            PMYSQL_TIME(PBuffer)^.year := year;
            PMYSQL_TIME(PBuffer)^.month := month;
            PMYSQL_TIME(PBuffer)^.day := day;
            PMYSQL_TIME(PBuffer)^.time_type := MYSQL_TIMESTAMP_DATE;
          end;
        FIELD_TYPE_TIME:
          begin
            FillChar(PBuffer^, SizeOf(TMYSQL_TIME), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
            aTime := ClientVarManager.GetAsDateTime(InParamValues[i]);
            DecodeTime(aTime, hour, minute, second, millisecond);
            PMYSQL_TIME(PBuffer)^.neg := Ord(aTime < 0);
            PMYSQL_TIME(PBuffer)^.hour := hour;
            PMYSQL_TIME(PBuffer)^.minute := minute;
            PMYSQL_TIME(PBuffer)^.second := second;
            PMYSQL_TIME(PBuffer)^.second_part := millisecond;
            PMYSQL_TIME(PBuffer)^.time_type := MYSQL_TIMESTAMP_TIME;
          end;
        FIELD_TYPE_DATETIME:
          begin
            FillChar(PBuffer^, SizeOf(TMYSQL_TIME), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
            aTime := ClientVarManager.GetAsDateTime(InParamValues[i]);
            DecodeDateTime(aTime, Year, Month, Day, hour, minute, second, millisecond);
            PMYSQL_TIME(PBuffer)^.neg := Ord(aTime < 0);
            PMYSQL_TIME(PBuffer)^.year := year;
            PMYSQL_TIME(PBuffer)^.month := month;
            PMYSQL_TIME(PBuffer)^.day := day;
            PMYSQL_TIME(PBuffer)^.hour := hour;
            PMYSQL_TIME(PBuffer)^.minute := minute;
            PMYSQL_TIME(PBuffer)^.second := second;
            PMYSQL_TIME(PBuffer)^.second_part := millisecond;
            PMYSQL_TIME(PBuffer)^.time_type := MYSQL_TIMESTAMP_DATETIME;
          end;
        FIELD_TYPE_BLOB:  //used for stBinaryStream only
          begin
            TempBlob := ClientVarManager.GetAsInterface(InParamValues[i]) as IZBlob;
            if TempBlob.IsEmpty then
              Bind^.is_null := 1
            else
            begin
              P := TempBlob.GetBuffer;
              Bind^.Length := TempBlob.Length;
              if Bind^.Length > Cardinal(ChunkSize) then
                {out of buffer range}
                goto JmpChunked
              else
                {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, PBuffer^, Bind^.Length);
            end;
          end;
        FIELD_TYPE_NULL:;
      end; {case}
    end;
  end;

  if ChunkedData and (FPlainDriver.stmt_bind_param(FMYSQL_STMT, FParamBindBuffer.GetBufferAddress) <> 0) then
  begin
    checkMySQLPrepStmtError (FPlainDriver, FMYSQL_STMT, lcPrepStmt,
      ConvertZMsgToRaw(SBindingFailure, ZMessages.cCodePage,
      ConSettings^.ClientCodePage^.CP), ConSettings);
    exit;
  end;
JmpInherited:
  inherited BindInParameters;

  if ChunkedData then
    // Send large blobs in chuncks
    for I := 0 to InParamCount - 1 do
    begin
      Bind := @FColumnArray[I];
      if (Bind^.is_null = 0) and (Bind^.buffer_address^ = nil) then
      begin
        case Bind^.buffer_type of
          FIELD_TYPE_STRING:
            if inParamTypes[i] in [stAsciiStream, stUnicodeStream] then
            begin
              TempBlob := InParamValues[i].VInterface as IZBlob;
              P := TempBlob.GetBuffer;
              Len := TempBlob.Length;
            end
            else
            begin
              CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], ConSettings^.ClientCodePage^.CP);
              Len := CharRec.Len;
              P := CharRec.P;
            end;
          FIELD_TYPE_TINY_BLOB:
            begin
              P := Pointer(InParamValues[i].vBytes);
              Len := Length(InParamValues[i].vBytes);
            end;
          FIELD_TYPE_BLOB:
            begin
              TempBlob := (InParamValues[I].VInterface as IZBlob);
              Len := TempBlob.Length;
              P := TempBlob.GetBuffer;
            end;
          else
            Continue;
        end;
        OffSet := 0;
        PieceSize := ChunkSize;
        while OffSet < Len do
        begin
          if OffSet+PieceSize > Len then
            PieceSize := Len - OffSet;
          if (FPlainDriver.stmt_send_long_data(FMYSQL_STMT, I, P, PieceSize) <> 0) then
          begin
            checkMySQLPrepStmtError (FPlainDriver, FMYSQL_STMT, lcPrepStmt,
              ConvertZMsgToRaw(SBindingFailure, ZMessages.cCodePage,
              ConSettings^.ClientCodePage^.CP), ConSettings);
            exit;
          end
          else Inc(P, PieceSize);
          Inc(OffSet, PieceSize);
        end;
        TempBlob:=nil;
      end;
    end;
end;
{$WARNINGS ON}

procedure TZMySQLPreparedStatement.UnPrepareInParameters;
begin
  if Assigned(FParamBindBuffer) then
    FreeAndNil(FParamBindBuffer);
end;

function TZMysqlPreparedStatement.GetCompareFirstKeywordStrings: TPreparablePrefixTokens;
begin
  Result := FPreparablePrefixTokens;
end;

function TZMysqlPreparedStatement.getFieldType(SQLType: TZSQLType;
  Var Signed: Boolean; MySQL_FieldType_Bit_1_IsBoolean: Boolean): TMysqlFieldType;
begin
  Signed := SQLType in [stShort, stSmall, stInteger, stLong];
  case SQLType of
    stBoolean:  if MySQL_FieldType_Bit_1_IsBoolean
                then Result := FIELD_TYPE_TINY
                else Result := FIELD_TYPE_STRING;//does NOT WORK: FIELD_TYPE_ENUM('Y'/'N'), TINY LEADS to truncations ):
    stByte, stShort:          Result := FIELD_TYPE_TINY;
    stWord, stSmall:          Result := FIELD_TYPE_SHORT;
    stLongWord, stInteger:    Result := FIELD_TYPE_LONG;
    stULong, stLong:          Result := FIELD_TYPE_LONGLONG;
    stFloat:                  Result := FIELD_TYPE_FLOAT;
    stDouble,
    stCurrency, stBigDecimal: Result := FIELD_TYPE_DOUBLE;
    stString, stUnicodeString,
    stGUID:                   Result := FIELD_TYPE_STRING;
    stBytes:                  Result := FIELD_TYPE_TINY_BLOB; //just indicate stBytes
    stDate:                   Result := FIELD_TYPE_DATE;
    stTime:                   Result := FIELD_TYPE_TIME;
    stTimestamp:              Result := FIELD_TYPE_DATETIME;
    stAsciiStream,
    stUnicodeStream:          Result := FIELD_TYPE_STRING; //all text data need to submitted as this!
    stBinaryStream:           Result := FIELD_TYPE_BLOB;
    else
      //stUnknown, stArray, stDataSet
      Result := FIELD_TYPE_NULL;
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZMySQLPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  PrepareOpenResultSetForReUse;
  Prepare;
  BindInParameters;
  if (FPlainDriver.stmt_execute(FMYSQL_STMT) <> 0) then
      checkMySQLPrepStmtError(FPlainDriver,FMYSQL_STMT, lcExecPrepStmt,
        ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
        ConSettings^.ClientCodePage^.CP), ConSettings);
  if FPlainDriver.stmt_field_count(FMYSQL_STMT) = 0
  then raise EZSQLException.Create(SCanNotOpenResultSet)
  else Result := CreateResultSet(SQL);
  inherited ExecuteQueryPrepared;
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZMySQLPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  if Assigned(FOpenResultSet)
  then IZResultSet(FOpenResultSet).Close;
  Prepare;
  BindInParameters;
  if (FPlainDriver.stmt_execute(FMYSQL_STMT) <> 0) then
    checkMySQLPrepStmtError(FPlainDriver,FMYSQL_STMT, lcExecPrepStmt,
      ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
        ConSettings^.ClientCodePage^.CP),
      ConSettings);

  { Process queries with result sets }
  if FPlainDriver.stmt_field_count(FMYSQL_STMT) > 0 then begin
    FPlainDriver.stmt_store_result(FMYSQL_STMT);
    Result := FPlainDriver.stmt_affected_rows(FMYSQL_STMT);
    FPlainDriver.stmt_free_result(FMYSQL_STMT);
  end else { Process regular query }
    Result := FPlainDriver.stmt_affected_rows(FMYSQL_STMT);
  LastUpdateCount := Result;
  Inherited ExecuteUpdatePrepared;
end;

procedure TZMySQLPreparedStatement.FlushPendingResults;
begin
  if FMYSQL_STMT <> nil then
    while FPlainDriver.stmt_next_result(FMYSQL_STMT) = 0 do
      if FPlainDriver.stmt_field_count(FMYSQL_STMT) > 0 then begin
        FPlainDriver.stmt_store_result(FMYSQL_STMT);
        FPlainDriver.stmt_free_result(FMYSQL_STMT);
        Inc(FResultsCount);
      end;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZMySQLPreparedStatement.ExecutePrepared: Boolean;
begin
  Result := False;
  PrepareLastResultSetForReUse;
  Prepare;
  BindInParameters;
  if (FPlainDriver.stmt_execute(FMYSQL_STMT) <> 0) then
    checkMySQLPrepStmtError(FPlainDriver,FMYSQL_STMT, lcExecPrepStmt,
      ConvertZMsgToRaw(SPreparedStmtExecFailure, ZMessages.cCodePage,
        ConSettings^.ClientCodePage^.CP), ConSettings);
  if FPlainDriver.stmt_field_count(FMYSQL_STMT) > 0 then begin
    Result := True;
    LastResultSet := CreateResultSet(SQL);
  end else { Processes regular query. }
    LastUpdateCount := FPlainDriver.stmt_affected_rows(FMYSQL_STMT);
  inherited ExecutePrepared;
end;

{**
  Returns the current result as an update count;
  if the result is a <code>ResultSet</code> object or there are no more results, -1
  is returned. This method should be called only once per result.

  @return the current result as an update count; -1 if the current result is a
    <code>ResultSet</code> object or there are no more results
  @see #execute
}
function TZMySQLPreparedStatement.GetUpdateCount: Integer;
begin
  Result := inherited GetUpdateCount;
  if (Result = -1) and Assigned(fMySQL) and (FPlainDriver.Field_Count(fMySQL) = 0) then begin
    LastUpdateCount := FPlainDriver.GetAffectedRows(fMySQL);
    Result := LastUpdateCount;
  end;
end;

{ TZMySQLCallableStatement }

{**
   Create sql string for calling stored procedure.
   @return a Stored Procedure SQL string
}
function TZMySQLCallableStatement.GetCallSQL: RawByteString;
  function GenerateParamsStr(Count: integer): RawByteString;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count-1 do
    begin
      if I > 0 then
        Result := Result + ', ';
      if FDBParamTypes[i] in [pctIn..pctReturn] then
        Result := Result + '@'+FParamNames[i];
    end;
  end;

var
  InParams: RawByteString;
begin
  if HasOutParameter then
    InParams := GenerateParamsStr(OutParamCount)
  else
    InParams := GenerateParamsStr(InParamCount);
  Result := 'CALL '+ConSettings^.ConvFuncs.ZStringToRaw(SQL,
            ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)+'('+InParams+')';
end;

function TZMySQLCallableStatement.GetOutParamSQL: RawByteString;
  function GenerateParamsStr: RawByteString;
  var
    I: integer;
  begin
    Result := '';
    I := 0;
    while True do
      if ( I = Length(FDBParamTypes)) or (FDBParamTypes[i] = pctUnknown) then
        break
      else begin
        if FDBParamTypes[i] in [pctInOut..pctReturn] then begin
          if Result <> '' then
            Result := Result + ',';
          if FParamTypeNames[i] = '' then
            Result := Result + ' @'+FParamNames[I]+' AS '+FParamNames[I]
          else
            Result := Result + ' CAST(@'+FParamNames[I]+ ' AS '+FParamTypeNames[i]+') AS '+FParamNames[I];
        end;
        Inc(i);
      end;
  end;

var
  OutParams: RawByteString;
begin
  OutParams := GenerateParamsStr;
  Result := 'SELECT '+ OutParams;
end;

function TZMySQLCallableStatement.GetSelectFunctionSQL: RawByteString;
  function GenerateInParamsStr: RawByteString;
  var
    I: Integer;
  begin
    Result := '';
    for i := 0 to Length(InParamValues) -1 do
      if Result = '' then
        Result := PrepareAnsiSQLParam(I)
      else
        Result := Result+', '+ PrepareAnsiSQLParam(I);
  end;
var
  InParams: RawByteString;
begin
  InParams := GenerateInParamsStr;
  Result := 'SELECT '+ConSettings^.ConvFuncs.ZStringToRaw(SQL,
            ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)+'('+InParams+')';
  Result := Result + ' AS ReturnValue';
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZMySQLCallableStatement.PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := ZDbcMySQLUtils.MySQLPrepareAnsiSQLParam(GetConnection as IZMySQLConnection,
    InParamValues[ParamIndex], InParamDefaultValues[ParamIndex], ClientVarManager,
    InParamTypes[ParamIndex], FUseDefaults);
end;

procedure TZMySQLCallableStatement.ClearResultSets;
begin
  inherited;
  FPlainDriver.FreeResult(FQueryHandle);
  FQueryHandle := nil;
end;

procedure TZMySQLCallableStatement.BindInParameters;
var
  I: integer;
  ExecQuery: RawByteString;
begin
  I := 0;
  ExecQuery := '';
  while True do
    if (i = Length(FDBParamTypes)) then
      break
    else
    begin
      if FDBParamTypes[i] in [pctIn, pctInOut] then
        if ExecQuery = '' then
          ExecQuery := 'SET @'+FParamNames[i]+' = '+PrepareAnsiSQLParam(I)
        else
          ExecQuery := ExecQuery + ', @'+FParamNames[i]+' = '+PrepareAnsiSQLParam(I);
      Inc(i);
    end;
  if not (ExecQuery = '') then
    if FPlainDriver.ExecRealQuery(Self.fMySQL, Pointer(ExecQuery), Length(ExecQuery)) = 0 then
      DriverManager.LogMessage(lcBindPrepStmt, ConSettings^.Protocol, ExecQuery)
    else
      CheckMySQLError(FPlainDriver, fMySQL, lcExecute, ExecQuery, ConSettings);
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZMySQLCallableStatement.CreateResultSet(const SQL: string): IZResultSet;
var
  CachedResolver: TZMySQLCachedResolver;
  NativeResultSet: TZMySQL_Store_ResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZMySQL_Store_ResultSet.Create(FPlainDriver, Self, SQL, fMySQL,
    @LastUpdateCount);
  if (GetResultSetConcurrency <> rcReadOnly) or (FUseResult
    and (GetResultSetType <> rtForwardOnly)) or (not IsFunction) then
  begin
    CachedResolver := TZMySQLCachedResolver.Create(FPlainDriver, fMySQL, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL,
      CachedResolver, ConSettings);
    CachedResultSet.SetConcurrency(rcReadOnly);
    {Need to fetch all data. The handles must be released for mutiple
      Resultsets}
    CachedResultSet.Last;//Fetch all
    CachedResultSet.BeforeFirst;//Move to first pos
    //if FQueryHandle <> nil then
      //FPlainDriver.FreeResult(FQueryHandle);
    //NativeResultSet.ResetCursor; //Release the handles
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

procedure TZMySQLCallableStatement.RegisterParamTypeAndName(const ParameterIndex:integer;
  ParamTypeName: String; const ParamName: String; Const ColumnSize, Precision: Integer);
begin
  FParamNames[ParameterIndex] := ConSettings^.ConvFuncs.ZStringToRaw(ParamName,
    ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
  ParamTypeName := LowerCase(ParamTypeName);
  if ( ZFastCode.Pos('char', ParamTypeName) > 0 ) or
     ( ZFastCode.Pos('set', ParamTypeName) > 0 ) then
    FParamTypeNames[ParameterIndex] := 'CHAR('+ZFastCode.IntToRaw(ColumnSize)+')'
  else
    if ( ZFastCode.Pos('set', ParamTypeName) > 0 ) then
      FParamTypeNames[ParameterIndex] := 'CHAR('+ZFastCode.IntToRaw(ColumnSize)+')'
    else
      if ( ZFastCode.Pos('datetime', ParamTypeName) > 0 ) or
         ( ZFastCode.Pos('timestamp', ParamTypeName) > 0 ) then
        FParamTypeNames[ParameterIndex] := 'DATETIME'
      else
        if ( ZFastCode.Pos('date', ParamTypeName) > 0 ) then
          FParamTypeNames[ParameterIndex] := 'DATE'
        else
          if ( ZFastCode.Pos('time', ParamTypeName) > 0 ) then
            FParamTypeNames[ParameterIndex] := 'TIME'
          else
            if ( ZFastCode.Pos('int', ParamTypeName) > 0 ) or
               ( ZFastCode.Pos('year', ParamTypeName) > 0 ) then
              FParamTypeNames[ParameterIndex] := 'SIGNED'
            else
              if ( ZFastCode.Pos('binary', ParamTypeName) > 0 ) then
                FParamTypeNames[ParameterIndex] := 'BINARY('+ZFastCode.IntToRaw(ColumnSize)+')'
              else
                FParamTypeNames[ParameterIndex] := '';
end;

constructor TZMySQLCallableStatement.Create(const PlainDriver: IZMySQLPlainDriver;
  const Connection: IZConnection; const SQL: string; const Info: TStrings;
  Handle: PMySQL);
begin
  inherited Create(Connection, SQL, Info);
  fMySQL := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtScrollInsensitive;
  FUseResult := StrToBoolEx(DefineStatementParameter(Self, 'useresult', 'false'));
  FUseDefaults := StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true'))
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZMySQLCallableStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  Result := nil;
  ASQL := SQL;
  if FPlainDriver.ExecRealQuery(fMySQL, Pointer(ASQL), Length(ASQL)) = 0 then
  begin
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
    if FPlainDriver.Field_Count(fMySQL) = 0 then
      raise EZSQLException.Create(SCanNotOpenResultSet);
    if IsFunction then
      ClearResultSets;
    FResultSets.Add(CreateResultSet(Self.SQL));
    if FPlainDriver.CheckAnotherRowset(fMySQL) then
    begin
      while FPlainDriver.RetrieveNextRowset(fMySQL) = 0 do
        if FPlainDriver.CheckAnotherRowset(fMySQL) then
          FResultSets.Add(CreateResultSet(Self.SQL))
        else break;
      CheckMySQLError(FPlainDriver, fMySQL, lcExecute, ASQL, ConSettings);
    end;
    FActiveResultset := FResultSets.Count-1;
    Result := IZResultSet(FResultSets[FActiveResultset]);
  end
  else
    CheckMySQLError(FPlainDriver, fMySQL, lcExecute, ASQL, ConSettings);
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZMySQLCallableStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  Result := -1;
  ASQL := SQL;
  if FPlainDriver.ExecRealQuery(fMySQL, Pointer(ASQL), Length(ASQL)) = 0 then
  begin
    { Process queries with result sets }
    if FPlainDriver.Field_Count(fMySQL) > 0 then begin
      ClearResultSets;
      FActiveResultset := 0;
      FResultSets.Add(CreateResultSet(Self.SQL));
      if FPlainDriver.CheckAnotherRowset(fMySQL) then
      begin
        Result := LastUpdateCount;
        while FPlainDriver.RetrieveNextRowset(fMySQL) = 0 do
          if FPlainDriver.CheckAnotherRowset(fMySQL) then
          begin
            FResultSets.Add(CreateResultSet(Self.SQL));
            inc(Result, LastUpdateCount); //LastUpdateCount will be returned from ResultSet.Open
          end
          else break;
        CheckMySQLError(FPlainDriver, fMySQL, lcExecute, ASQL, ConSettings);
      end
      else
        Result := LastUpdateCount;
      FActiveResultset := FResultSets.Count-1;
      LastResultSet := IZResultSet(FResultSets[FActiveResultset]);
    end
    else { Process regular query }
      Result := FPlainDriver.GetAffectedRows(fMySQL);
  end
  else
    CheckMySQLError(FPlainDriver, fMySQL, lcExecute, ASQL, ConSettings);
  LastUpdateCount := Result;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
}
function TZMySQLCallableStatement.Execute(const SQL: RawByteString): Boolean;
begin
  Result := False;
  ASQL := SQL;
  if FPlainDriver.ExecRealQuery(fMySQL, Pointer(ASQL), Length(ASQL)) = 0 then begin
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
    { Process queries with result sets }
    if FPlainDriver.Field_Count(fMySQL) > 0 then begin
      Result := True;
      LastResultSet := CreateResultSet(Self.SQL);
    end else { Processes regular query. }
      LastUpdateCount := FPlainDriver.GetAffectedRows(fMySQL);
  end else
    CheckMySQLError(FPlainDriver, fMySQL, lcExecute, ASQL, ConSettings);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZMySQLCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if IsFunction then
  begin
    TrimInParameters;
    Result := ExecuteQuery(GetSelectFunctionSQL);
  end
  else
  begin
    BindInParameters;
    ExecuteUpdate(GetCallSQL);
    if OutParamCount > 0 then
      Result := ExecuteQuery(GetOutParamSQL) //Get the Last Resultset
    else
      Result := GetLastResultSet;
  end;
  if Assigned(Result) then
    AssignOutParamValuesFromResultSet(Result, OutParamValues, OutParamCount , FDBParamTypes);
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZMySQLCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  if IsFunction then
  begin
    TrimInParameters;
    Result := ExecuteUpdate(GetSelectFunctionSQL);
    AssignOutParamValuesFromResultSet(LastResultSet, OutParamValues, OutParamCount , FDBParamTypes);
  end
  else
  begin
    BindInParameters;
    Result := ExecuteUpdate(GetCallSQL);
    if OutParamCount > 0 then
      AssignOutParamValuesFromResultSet(ExecuteQuery(GetOutParamSQL), OutParamValues, OutParamCount , FDBParamTypes);
    Inc(Result, LastUpdateCount);
  end;
end;

{**
  Checks is use result should be used in result sets.
  @return <code>True</code> use result in result sets,
    <code>False</code> store result in result sets.
}
function TZMySQLCallableStatement.IsUseResult: Boolean;
begin
  Result := FUseResult;
end;

{**
  Checks if this is a prepared mysql statement.
  @return <code>False</code> This is not a prepared mysql statement.
}
function TZMySQLCallableStatement.IsPreparedStatement: Boolean;
begin
  Result := False;
end;

{**
  Get the first resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetNextResultSet: IZResultSet;
begin
  if ( FActiveResultset < FResultSets.Count-1) and ( FResultSets.Count > 1) then
  begin
    Inc(FActiveResultset);
    Result := IZResultSet(FResultSets[FActiveResultset]);
  end
  else
    if FResultSets.Count = 0 then
      Result := nil
    else
      Result := IZResultSet(FResultSets[FActiveResultset]);
end;

{**
  Get the previous resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetPreviousResultSet: IZResultSet;
begin
  if ( FActiveResultset > 0) and ( FResultSets.Count > 0) then
  begin
    Dec(FActiveResultset);
    Result := IZResultSet(FResultSets[FActiveResultset]);
  end
  else
    if FResultSets.Count = 0 then
      Result := nil
    else
      Result := IZResultSet(FResultSets[FActiveResultset]);
end;

{**
  Get the next resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetFirstResultSet: IZResultSet;
begin
  if FResultSets.Count = 0 then
    Result := nil
  else
  begin
    FActiveResultset := 0;
    Result := IZResultSet(FResultSets[0]);
  end;
end;

{**
  Get the last resultset..
  @result <code>IZResultSet</code> if supported
}
function TZMySQLCallableStatement.GetLastResultSet: IZResultSet;
begin
  if FResultSets.Count = 0 then
    Result := nil
  else
  begin
    FActiveResultset := FResultSets.Count -1;
    Result := IZResultSet(FResultSets[FResultSets.Count -1]);
  end;
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
function TZMySQLCallableStatement.GetMoreResults: Boolean;
begin
  Result := FResultSets.Count > 0;
end;

{**
  First ResultSet?
  @result <code>True</code> if first ResultSet
}
function TZMySQLCallableStatement.BOR: Boolean;
begin
  Result := FActiveResultset = 0;
end;

{**
  Last ResultSet?
  @result <code>True</code> if Last ResultSet
}
function TZMySQLCallableStatement.EOR: Boolean;
begin
  Result := FActiveResultset = FResultSets.Count -1;
end;

{**
  Retrieves a ResultSet by his index.
  @param Integer the index of the Resultset
  @result <code>IZResultSet</code> of the Index or nil.
}
function TZMySQLCallableStatement.GetResultSetByIndex(const Index: Integer): IZResultSet;
begin
  Result := nil;
  if ( Index < 0 ) or ( Index > FResultSets.Count -1 ) then
    raise Exception.Create(Format(SListIndexError, [Index]))
  else
    Result := IZResultSet(FResultSets[Index]);
end;

{**
  Returns the Count of retrived ResultSets.
  @result <code>Integer</code> Count
}
function TZMySQLCallableStatement.GetResultSetCount: Integer;
begin
  Result := FResultSets.Count;
end;

{ TZMySQLAbstractBindBuffer }

constructor TZMySQLAbstractBindBuffer.Create(PlainDriver: IZMysqlPlainDriver;
  const BindCount: Integer; var ColumnArray: TZMysqlColumnBuffer);
begin
  inherited Create;
  FBindOffsets := GetBindOffsets(PlainDriver.IsMariaDBDriver, PlainDriver.GetClientVersion);

  if FBindOffsets.buffer_type=0 then
    raise EZSQLException.Create('Unknown dll version : '+ZFastCode.IntToStr(PlainDriver.GetClientVersion));
  FPColumnArray := @ColumnArray;
  setlength(FBindArray,0);
  setlength(ColumnArray,BindCount);
  setlength(FBindArray,BindCount*FBindOffsets.size);
end;

function TZMySQLAbstractBindBuffer.GetBufferAddress: Pointer;
begin
  {$R-}
  result := @FBindArray[0];
  {$R+}
end;

{ TZMySQLResultSetBindBuffer }

procedure TZMySQLResultSetBindBuffer.AddColumn(MYSQL_FIELD: PMYSQL_FIELD;
  FieldOffSets: PMYSQL_FIELDOFFSETS);
var
  ColOffset: NativeUInt;
  Bind: PDOBindRecord2;
begin
  Bind := @FPColumnArray^[FAddedColumnCount];
  bind^.buffer_type := PMysqlFieldType(NativeUInt(MYSQL_FIELD)+FieldOffSets._type)^; //safe initialtype
  if FieldOffSets.charsetnr > 0
  then bind^.binary := PUInt(NativeUInt(MYSQL_FIELD)+NativeUInt(FieldOffSets.charsetnr))^ = 63
  else bind^.binary := PUInt(NativeUInt(MYSQL_FIELD)+FieldOffSets.flags)^ and BINARY_FLAG <> 0;

  bind^.decimals := PUInt(NativeUInt(MYSQL_FIELD)+FieldOffSets.decimals)^;
  case bind^.buffer_type of
    FIELD_TYPE_NULL: bind^.Length := 0;
    FIELD_TYPE_BIT: case PULong(NativeUInt(MYSQL_FIELD)+FieldOffSets.length)^ of
                      0..8  : bind^.Length := SizeOf(Byte);
                      9..16 : bind^.Length := SizeOf(Word);
                      17..32: bind^.Length := SizeOf(Cardinal);
                      else    bind^.Length := SizeOf(UInt64);
                    end;
    FIELD_TYPE_DATE:        bind^.Length := sizeOf(TMYSQL_TIME);
    FIELD_TYPE_TIME:        bind^.Length := sizeOf(TMYSQL_TIME);
    FIELD_TYPE_DATETIME:    bind^.Length := sizeOf(TMYSQL_TIME);
    FIELD_TYPE_TIMESTAMP:   bind^.Length := sizeOf(TMYSQL_TIME);
    FIELD_TYPE_TINY:        bind^.Length := 1;
    FIELD_TYPE_SHORT:       bind^.Length := 2;
    FIELD_TYPE_LONG:        bind^.Length := 4;
    FIELD_TYPE_LONGLONG:    bind^.Length := 8;
    FIELD_TYPE_INT24: //we've no 3Byte integers... so let's convert them
      begin
        bind^.Length := 4;
        bind^.buffer_type := FIELD_TYPE_LONG;
      end;
    FIELD_TYPE_FLOAT,
    FIELD_TYPE_DOUBLE:    if PULong(NativeUInt(MYSQL_FIELD)+FieldOffSets.length)^ < 12 then begin
                            bind^.Length := 4;
                            bind^.buffer_type := FIELD_TYPE_FLOAT;
                          end else begin
                            bind^.Length := 8;
                            bind^.buffer_type := FIELD_TYPE_DOUBLE;
                          end;
    MYSQL_TYPE_JSON,
    FIELD_TYPE_BLOB,
    FIELD_TYPE_TINY_BLOB,
    FIELD_TYPE_MEDIUM_BLOB,
    FIELD_TYPE_LONG_BLOB,
    FIELD_TYPE_GEOMETRY:    bind^.Length := 0;//http://bugs.mysql.com/file.php?id=12361&bug_id=33086
    FIELD_TYPE_VARCHAR,
    FIELD_TYPE_VAR_STRING,
    FIELD_TYPE_STRING:
      begin
        bind^.buffer_type := FIELD_TYPE_STRING;
        bind^.Length := PULong(NativeUInt(MYSQL_FIELD)+FieldOffSets.length)^+Byte(Ord(not bind^.Binary));
      end;
    FIELD_TYPE_NEWDECIMAL,
    FIELD_TYPE_DECIMAL:
      begin //force binary conversion to double values!
        bind^.buffer_type := FIELD_TYPE_DOUBLE;
        bind^.Length := 8;
      end;
  else
    bind^.Length := (((PULong(NativeUInt(MYSQL_FIELD)+FieldOffSets.length)^ -1) shr 3)+1) shl 3; //8Byte Aligned
    //Length := MYSQL_FIELD^.length;
  end;
  SetLength(Bind^.Buffer, bind^.Length+LongWord(Ord(
    (bind^.buffer_type in [FIELD_TYPE_STRING, FIELD_TYPE_ENUM, FIELD_TYPE_SET]) and not bind^.Binary)));
  ColOffset := NativeUInt(FAddedColumnCount*FBindOffsets.size);
  Bind^.mysql_bind := @FbindArray[ColOffset]; //save address
  bind^.buffer_address := @FbindArray[ColOffset+FBindOffsets.buffer]; //save address
  Bind^.buffer_Length_address := @FbindArray[ColOffset+FBindOffsets.buffer_length]; //save address
  Bind^.buffer_type_address := @FbindArray[ColOffset+FBindOffsets.buffer_type];
  Bind^.is_signed := PUInt(NativeUInt(MYSQL_FIELD)+FieldOffSets.flags)^ and UNSIGNED_FLAG = 0;
  if bind^.buffer_type = FIELD_TYPE_GEOMETRY
  then Bind^.buffer_type_address^ := FIELD_TYPE_BLOB
  else Bind^.buffer_type_address^ := bind^.buffer_type;

  PULong(Bind^.buffer_Length_address)^ := Bind^.length;
  PByte(@FbindArray[ColOffset+FBindOffsets.is_unsigned])^:= Ord(not Bind^.is_signed);
  PPointer(@FbindArray[ColOffset+FBindOffsets.buffer])^:= Pointer(Bind^.buffer);
  PPointer(@FbindArray[ColOffset+FBindOffsets.length])^:= @Bind^.length;
  PPointer(@FbindArray[ColOffset+FBindOffsets.is_null])^:= @Bind^.is_null;
  Inc(FAddedColumnCount);
end;

{ TZMySQLParamBindBuffer }

procedure TZMySQLParamBindBuffer.AddColumn(buffertype: TMysqlFieldType;
  field_length: integer; is_signed: Boolean);
var
  ColOffset:NativeUInt;
  Bind: PDOBindRecord2;
begin
  ColOffset:=NativeUInt(FAddedColumnCount*FBindOffsets.size);

  Bind := @FPColumnArray^[FAddedColumnCount];
  Bind^.mysql_bind := @FbindArray[ColOffset]; //save address
  bind^.buffer_address := @FbindArray[ColOffset+FBindOffsets.buffer]; //save address
  Bind^.buffer_Length_address := @FbindArray[ColOffset+FBindOffsets.buffer_length]; //save address
  Bind^.buffer_type := buffertype; //save initial type
  Bind^.is_signed := is_signed;
  Bind^.buffer_type_address := @FbindArray[ColOffset+FBindOffsets.buffer_type];

  //ludob: mysql adds terminating #0 on top of data. Avoid buffer overrun.
  Bind^.length := field_length+Ord(buffertype in
    [FIELD_TYPE_ENUM, FIELD_TYPE_DECIMAL, FIELD_TYPE_MEDIUM_BLOB, MYSQL_TYPE_JSON,
     FIELD_TYPE_LONG_BLOB, FIELD_TYPE_BLOB, FIELD_TYPE_VAR_STRING, FIELD_TYPE_STRING]);
  SetLength(Bind^.buffer, Bind^.length);

  Bind^.is_null := Ord(buffertype = FIELD_TYPE_NULL);
  Bind^.buffer_type_address^ := buffertype;
  PULong(Bind^.buffer_Length_address)^ := Bind^.length;
  PByte(@FbindArray[ColOffset+FBindOffsets.is_unsigned])^ := Ord(not is_signed);
  bind^.buffer_address^ := Pointer(Bind^.buffer);
  PPointer(@FbindArray[ColOffset+FBindOffsets.length])^ := @Bind^.length;
  PPointer(@FbindArray[ColOffset+FBindOffsets.is_null])^ := @Bind^.is_null;
  Inc(FAddedColumnCount);
end;

{ TZMySQLStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a native MySQL Plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZMySQLStatement.Create(const PlainDriver: IZMySQLPlainDriver;
  const Connection: IZConnection; const Info: TStrings; Handle: PMySQL);
begin
  inherited Create(PlainDriver, Connection, '', Info, Handle);
end;

initialization

{ preparable statements: }

{ http://dev.mysql.com/doc/refman/4.1/en/sql-syntax-prepared-statements.html }
SetLength(MySQL41PreparableTokens, 13);
MySQL41PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL41PreparableTokens[0].ChildMatches, 1);
  MySQL41PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL41PreparableTokens[1].MatchingGroup := 'COMMIT';
MySQL41PreparableTokens[2].MatchingGroup := 'CREATE';
  SetLength(MySQL41PreparableTokens[2].ChildMatches, 2);
  MySQL41PreparableTokens[2].ChildMatches[0] := 'INDEX';
  MySQL41PreparableTokens[2].ChildMatches[1] := 'TABLE';
MySQL41PreparableTokens[3].MatchingGroup := 'DROP';
  SetLength(MySQL41PreparableTokens[3].ChildMatches, 2);
  MySQL41PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL41PreparableTokens[3].ChildMatches[1] := 'TABLE';
MySQL41PreparableTokens[4].MatchingGroup := 'DELETE';
MySQL41PreparableTokens[5].MatchingGroup := 'DO';
MySQL41PreparableTokens[6].MatchingGroup := 'INSERT';
MySQL41PreparableTokens[7].MatchingGroup := 'RENAME';
  SetLength(MySQL41PreparableTokens[7].ChildMatches, 1);
  MySQL41PreparableTokens[7].ChildMatches[0] := 'TABLE';
MySQL41PreparableTokens[8].MatchingGroup := 'REPLACE';
MySQL41PreparableTokens[9].MatchingGroup := 'SELECT';
MySQL41PreparableTokens[10].MatchingGroup := 'SET';
MySQL41PreparableTokens[11].MatchingGroup := 'SHOW';
MySQL41PreparableTokens[12].MatchingGroup := 'UPDATE';

{ http://dev.mysql.com/doc/refman/5.0/en/sql-syntax-prepared-statements.html }
SetLength(MySQL50PreparableTokens, 15);
MySQL50PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL50PreparableTokens[0].ChildMatches, 1);
  MySQL50PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL50PreparableTokens[1].MatchingGroup := 'CALL';
MySQL50PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL50PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL50PreparableTokens[3].ChildMatches, 2);
  MySQL50PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL50PreparableTokens[3].ChildMatches[1] := 'TABLE';
MySQL50PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL50PreparableTokens[4].ChildMatches, 2);
  MySQL50PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL50PreparableTokens[4].ChildMatches[1] := 'TABLE';
MySQL50PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL50PreparableTokens[6].MatchingGroup := 'DO';
MySQL50PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL50PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL50PreparableTokens[8].ChildMatches, 1);
  MySQL50PreparableTokens[8].ChildMatches[0] := 'TABLE';
MySQL50PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL50PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL50PreparableTokens[11].MatchingGroup := 'SET';
MySQL50PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL50PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL50PreparableTokens[13].ChildMatches, 1);
  MySQL50PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL50PreparableTokens[14].MatchingGroup := 'UPDATE';

SetLength(MySQL5015PreparableTokens, 15);
MySQL5015PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL5015PreparableTokens[0].ChildMatches, 1);
  MySQL5015PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL5015PreparableTokens[1].MatchingGroup := 'CALL';
MySQL5015PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL5015PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL5015PreparableTokens[3].ChildMatches, 3);
  MySQL5015PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL5015PreparableTokens[3].ChildMatches[1] := 'TABLE';
  MySQL5015PreparableTokens[3].ChildMatches[2] := 'VIEW';
MySQL5015PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL5015PreparableTokens[4].ChildMatches, 3);
  MySQL5015PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL5015PreparableTokens[4].ChildMatches[1] := 'TABLE';
  MySQL5015PreparableTokens[4].ChildMatches[2] := 'VIEW';
MySQL5015PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL5015PreparableTokens[6].MatchingGroup := 'DO';
MySQL5015PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL5015PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL5015PreparableTokens[8].ChildMatches, 1);
  MySQL5015PreparableTokens[8].ChildMatches[0] := 'TABLE';
MySQL5015PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL5015PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL5015PreparableTokens[11].MatchingGroup := 'SET';
MySQL5015PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL5015PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL5015PreparableTokens[13].ChildMatches, 1);
  MySQL5015PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL5015PreparableTokens[14].MatchingGroup := 'UPDATE';

SetLength(MySQL5023PreparableTokens, 18);
MySQL5023PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL5023PreparableTokens[0].ChildMatches, 1);
  MySQL5023PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[1].MatchingGroup := 'CALL';
MySQL5023PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL5023PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL5023PreparableTokens[3].ChildMatches, 3);
  MySQL5023PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL5023PreparableTokens[3].ChildMatches[1] := 'TABLE';
  MySQL5023PreparableTokens[3].ChildMatches[2] := 'VIEW';
MySQL5023PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL5023PreparableTokens[4].ChildMatches, 3);
  MySQL5023PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL5023PreparableTokens[4].ChildMatches[1] := 'TABLE';
  MySQL5023PreparableTokens[4].ChildMatches[2] := 'VIEW';
MySQL5023PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL5023PreparableTokens[6].MatchingGroup := 'DO';
MySQL5023PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL5023PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL5023PreparableTokens[8].ChildMatches, 1);
  MySQL5023PreparableTokens[8].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL5023PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL5023PreparableTokens[11].MatchingGroup := 'SET';
MySQL5023PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL5023PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL5023PreparableTokens[13].ChildMatches, 1);
  MySQL5023PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[14].MatchingGroup := 'UPDATE';
MySQL5023PreparableTokens[15].MatchingGroup := 'ANALYZE';
  SetLength(MySQL5023PreparableTokens[15].ChildMatches, 1);
  MySQL5023PreparableTokens[15].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[16].MatchingGroup := 'OPTIMIZE';
  SetLength(MySQL5023PreparableTokens[16].ChildMatches, 1);
  MySQL5023PreparableTokens[16].ChildMatches[0] := 'TABLE';
MySQL5023PreparableTokens[17].MatchingGroup := 'REPAIR';
  SetLength(MySQL5023PreparableTokens[17].ChildMatches, 1);
  MySQL5023PreparableTokens[17].ChildMatches[0] := 'TABLE';

{http://dev.mysql.com/doc/refman/5.1/en/sql-syntax-prepared-statements.html}
SetLength(MySQL5112PreparableTokens, 30);
MySQL5112PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL5112PreparableTokens[0].ChildMatches, 1);
  MySQL5112PreparableTokens[0].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[1].MatchingGroup := 'CALL';
MySQL5112PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL5112PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL5112PreparableTokens[3].ChildMatches, 5);
  MySQL5112PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL5112PreparableTokens[3].ChildMatches[1] := 'TABLE';
  MySQL5112PreparableTokens[3].ChildMatches[2] := 'VIEW';
  MySQL5112PreparableTokens[3].ChildMatches[3] := 'DATABASE';
  MySQL5112PreparableTokens[3].ChildMatches[4] := 'USER';
MySQL5112PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL5112PreparableTokens[4].ChildMatches, 5);
  MySQL5112PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL5112PreparableTokens[4].ChildMatches[1] := 'TABLE';
  MySQL5112PreparableTokens[4].ChildMatches[2] := 'VIEW';
  MySQL5112PreparableTokens[4].ChildMatches[3] := 'DATABASE';
  MySQL5112PreparableTokens[4].ChildMatches[4] := 'USER';
MySQL5112PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL5112PreparableTokens[6].MatchingGroup := 'DO';
MySQL5112PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL5112PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL5112PreparableTokens[8].ChildMatches, 3);
  MySQL5112PreparableTokens[8].ChildMatches[0] := 'TABLE';
  MySQL5112PreparableTokens[8].ChildMatches[1] := 'DATABASE';
  MySQL5112PreparableTokens[8].ChildMatches[2] := 'USER';
MySQL5112PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL5112PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL5112PreparableTokens[11].MatchingGroup := 'SET';
MySQL5112PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL5112PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL5112PreparableTokens[13].ChildMatches, 1);
  MySQL5112PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[14].MatchingGroup := 'UPDATE';
MySQL5112PreparableTokens[15].MatchingGroup := 'ANALYZE';
  SetLength(MySQL5112PreparableTokens[15].ChildMatches, 1);
  MySQL5112PreparableTokens[15].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[16].MatchingGroup := 'OPTIMIZE';
  SetLength(MySQL5112PreparableTokens[16].ChildMatches, 1);
  MySQL5112PreparableTokens[16].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[17].MatchingGroup := 'REPAIR';
  SetLength(MySQL5112PreparableTokens[17].ChildMatches, 1);
  MySQL5112PreparableTokens[17].ChildMatches[0] := 'TABLE';
MySQL5112PreparableTokens[18].MatchingGroup := 'CACHE';
  SetLength(MySQL5112PreparableTokens[18].ChildMatches, 1);
  MySQL5112PreparableTokens[18].ChildMatches[0] := 'INDEX';
MySQL5112PreparableTokens[19].MatchingGroup := 'CHANGE';
  SetLength(MySQL5112PreparableTokens[19].ChildMatches, 1);
  MySQL5112PreparableTokens[19].ChildMatches[0] := 'MASTER';
MySQL5112PreparableTokens[20].MatchingGroup := 'CHECKSUM';
  SetLength(MySQL5112PreparableTokens[20].ChildMatches, 2);
  MySQL5112PreparableTokens[20].ChildMatches[0] := 'TABLE';
  MySQL5112PreparableTokens[20].ChildMatches[1] := 'TABLES';
MySQL5112PreparableTokens[21].MatchingGroup := 'FLUSH';
  SetLength(MySQL5112PreparableTokens[21].ChildMatches, 10);
  MySQL5112PreparableTokens[21].ChildMatches[0] := 'TABLE';
  MySQL5112PreparableTokens[21].ChildMatches[1] := 'TABLES';
  MySQL5112PreparableTokens[21].ChildMatches[2] := 'HOSTS';
  MySQL5112PreparableTokens[21].ChildMatches[3] := 'PRIVILEGES';
  MySQL5112PreparableTokens[21].ChildMatches[4] := 'LOGS';
  MySQL5112PreparableTokens[21].ChildMatches[5] := 'STATUS';
  MySQL5112PreparableTokens[21].ChildMatches[6] := 'MASTER';
  MySQL5112PreparableTokens[21].ChildMatches[7] := 'SLAVE';
  MySQL5112PreparableTokens[21].ChildMatches[8] := 'DES_KEY_FILE';
  MySQL5112PreparableTokens[21].ChildMatches[9] := 'USER_RESOURCES';
MySQL5112PreparableTokens[22].MatchingGroup := 'GRANT';
MySQL5112PreparableTokens[23].MatchingGroup := 'INSTALL';
  SetLength(MySQL5112PreparableTokens[23].ChildMatches, 1);
  MySQL5112PreparableTokens[23].ChildMatches[0] := 'PLUGIN';
MySQL5112PreparableTokens[24].MatchingGroup := 'KILL';
MySQL5112PreparableTokens[25].MatchingGroup := 'LOAD';
  SetLength(MySQL5112PreparableTokens[25].ChildMatches, 1);
  MySQL5112PreparableTokens[25].ChildMatches[0] := 'INDEX'; //+INTO CACHE
MySQL5112PreparableTokens[26].MatchingGroup := 'RESET';
  SetLength(MySQL5112PreparableTokens[26].ChildMatches, 3);
  MySQL5112PreparableTokens[26].ChildMatches[0] := 'MASTER';
  MySQL5112PreparableTokens[26].ChildMatches[1] := 'SLAVE';
  MySQL5112PreparableTokens[26].ChildMatches[2] := 'QUERY'; //+CACHE
MySQL5112PreparableTokens[27].MatchingGroup := 'REVOKE';
MySQL5112PreparableTokens[28].MatchingGroup := 'SLAVE';
  SetLength(MySQL5112PreparableTokens[28].ChildMatches, 2);
  MySQL5112PreparableTokens[28].ChildMatches[0] := 'START';
  MySQL5112PreparableTokens[28].ChildMatches[1] := 'STOP';
MySQL5112PreparableTokens[29].MatchingGroup := 'UNINSTALL';
  SetLength(MySQL5112PreparableTokens[29].ChildMatches, 1);
  MySQL5112PreparableTokens[29].ChildMatches[0] := 'PLUGIN';

{http://dev.mysql.com/doc/refman/5.6/en/sql-syntax-prepared-statements.html}
SetLength(MySQL568PreparableTokens, 30);
MySQL568PreparableTokens[0].MatchingGroup := 'ALTER';
  SetLength(MySQL568PreparableTokens[0].ChildMatches, 2);
  MySQL568PreparableTokens[0].ChildMatches[0] := 'TABLE';
  MySQL568PreparableTokens[0].ChildMatches[1] := 'USER';
MySQL568PreparableTokens[1].MatchingGroup := 'CALL';
MySQL568PreparableTokens[2].MatchingGroup := 'COMMIT';
MySQL568PreparableTokens[3].MatchingGroup := 'CREATE';
  SetLength(MySQL568PreparableTokens[3].ChildMatches, 5);
  MySQL568PreparableTokens[3].ChildMatches[0] := 'INDEX';
  MySQL568PreparableTokens[3].ChildMatches[1] := 'TABLE';
  MySQL568PreparableTokens[3].ChildMatches[2] := 'VIEW';
  MySQL568PreparableTokens[3].ChildMatches[3] := 'DATABASE';
  MySQL568PreparableTokens[3].ChildMatches[4] := 'USER';
MySQL568PreparableTokens[4].MatchingGroup := 'DROP';
  SetLength(MySQL568PreparableTokens[4].ChildMatches, 5);
  MySQL568PreparableTokens[4].ChildMatches[0] := 'INDEX';
  MySQL568PreparableTokens[4].ChildMatches[1] := 'TABLE';
  MySQL568PreparableTokens[4].ChildMatches[2] := 'VIEW';
  MySQL568PreparableTokens[4].ChildMatches[3] := 'DATABASE';
  MySQL568PreparableTokens[4].ChildMatches[4] := 'USER';
MySQL568PreparableTokens[5].MatchingGroup := 'DELETE';
MySQL568PreparableTokens[6].MatchingGroup := 'DO';
MySQL568PreparableTokens[7].MatchingGroup := 'INSERT';
MySQL568PreparableTokens[8].MatchingGroup := 'RENAME';
  SetLength(MySQL568PreparableTokens[8].ChildMatches, 3);
  MySQL568PreparableTokens[8].ChildMatches[0] := 'TABLE';
  MySQL568PreparableTokens[8].ChildMatches[1] := 'DATABASE';
  MySQL568PreparableTokens[8].ChildMatches[2] := 'USER';
MySQL568PreparableTokens[9].MatchingGroup := 'REPLACE';
MySQL568PreparableTokens[10].MatchingGroup := 'SELECT';
MySQL568PreparableTokens[11].MatchingGroup := 'SET';
MySQL568PreparableTokens[12].MatchingGroup := 'SHOW';
MySQL568PreparableTokens[13].MatchingGroup := 'TRUNCATE';
  SetLength(MySQL568PreparableTokens[13].ChildMatches, 1);
  MySQL568PreparableTokens[13].ChildMatches[0] := 'TABLE';
MySQL568PreparableTokens[14].MatchingGroup := 'UPDATE';
MySQL568PreparableTokens[15].MatchingGroup := 'ANALYZE';
  SetLength(MySQL568PreparableTokens[15].ChildMatches, 1);
  MySQL568PreparableTokens[15].ChildMatches[0] := 'TABLE';
MySQL568PreparableTokens[16].MatchingGroup := 'OPTIMIZE';
  SetLength(MySQL568PreparableTokens[16].ChildMatches, 1);
  MySQL568PreparableTokens[16].ChildMatches[0] := 'TABLE';
MySQL568PreparableTokens[17].MatchingGroup := 'REPAIR';
  SetLength(MySQL568PreparableTokens[17].ChildMatches, 1);
  MySQL568PreparableTokens[17].ChildMatches[0] := 'TABLE';
MySQL568PreparableTokens[18].MatchingGroup := 'CACHE';
  SetLength(MySQL568PreparableTokens[18].ChildMatches, 1);
  MySQL568PreparableTokens[18].ChildMatches[0] := 'INDEX';
MySQL568PreparableTokens[19].MatchingGroup := 'CHANGE';
  SetLength(MySQL568PreparableTokens[19].ChildMatches, 1);
  MySQL568PreparableTokens[19].ChildMatches[0] := 'MASTER';
MySQL568PreparableTokens[20].MatchingGroup := 'CHECKSUM';
  SetLength(MySQL568PreparableTokens[20].ChildMatches, 2);
  MySQL568PreparableTokens[20].ChildMatches[0] := 'TABLE';
  MySQL568PreparableTokens[20].ChildMatches[1] := 'TABLES';
MySQL568PreparableTokens[21].MatchingGroup := 'FLUSH';
  SetLength(MySQL568PreparableTokens[21].ChildMatches, 10);
  MySQL568PreparableTokens[21].ChildMatches[0] := 'TABLE';
  MySQL568PreparableTokens[21].ChildMatches[1] := 'TABLES';
  MySQL568PreparableTokens[21].ChildMatches[2] := 'HOSTS';
  MySQL568PreparableTokens[21].ChildMatches[3] := 'PRIVILEGES';
  MySQL568PreparableTokens[21].ChildMatches[4] := 'LOGS';
  MySQL568PreparableTokens[21].ChildMatches[5] := 'STATUS';
  MySQL568PreparableTokens[21].ChildMatches[6] := 'MASTER';
  MySQL568PreparableTokens[21].ChildMatches[7] := 'SLAVE';
  MySQL568PreparableTokens[21].ChildMatches[8] := 'DES_KEY_FILE';
  MySQL568PreparableTokens[21].ChildMatches[9] := 'USER_RESOURCES';
MySQL568PreparableTokens[22].MatchingGroup := 'GRANT';
MySQL568PreparableTokens[23].MatchingGroup := 'INSTALL';
  SetLength(MySQL568PreparableTokens[23].ChildMatches, 1);
  MySQL568PreparableTokens[23].ChildMatches[0] := 'PLUGIN';
MySQL568PreparableTokens[24].MatchingGroup := 'KILL';
MySQL568PreparableTokens[25].MatchingGroup := 'LOAD';
  SetLength(MySQL568PreparableTokens[25].ChildMatches, 1);
  MySQL568PreparableTokens[25].ChildMatches[0] := 'INDEX'; //+INTO CACHE
MySQL568PreparableTokens[26].MatchingGroup := 'RESET';
  SetLength(MySQL568PreparableTokens[26].ChildMatches, 3);
  MySQL568PreparableTokens[26].ChildMatches[0] := 'MASTER';
  MySQL568PreparableTokens[26].ChildMatches[1] := 'SLAVE';
  MySQL568PreparableTokens[26].ChildMatches[2] := 'QUERY'; //+CACHE
MySQL568PreparableTokens[27].MatchingGroup := 'REVOKE';
MySQL568PreparableTokens[28].MatchingGroup := 'SLAVE';
  SetLength(MySQL568PreparableTokens[28].ChildMatches, 2);
  MySQL568PreparableTokens[28].ChildMatches[0] := 'START';
  MySQL568PreparableTokens[28].ChildMatches[1] := 'STOP';
MySQL568PreparableTokens[29].MatchingGroup := 'UNINSTALL';
  SetLength(MySQL568PreparableTokens[29].ChildMatches, 1);
  MySQL568PreparableTokens[29].ChildMatches[0] := 'PLUGIN';
{$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
end.
