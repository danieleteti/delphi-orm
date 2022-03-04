{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
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

unit ZDbcOracleStatement;

interface

{$I ZDbc.inc}
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  ZSysUtils, ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainOracleDriver,
  ZCompatibility, ZVariant, ZDbcOracleUtils, ZPlainOracleConstants,
  ZDbcOracle;

type

  {** Implements Prepared SQL Statement. }

  { TZOraclePreparedStatement }
  TZOraclePreparedStatement = class(TZAbstractPreparedStatement)
  private
    FHandle: POCIStmt;
    FErrorHandle: POCIError;
    FPlainDriver: IZOraclePlainDriver;
    FOracleConnection: IZOracleConnection;
    FParams: PZSQLVars;
    FRowPrefetchSize: ub4;
    FZBufferSize: Integer;
    FStatementType: ub2;
    FServerStmtCache: Boolean;
    FParamsBuffer: TByteDynArray; { holds all data for bindings }
    FIteration: Integer;
    FCanBindInt64: Boolean;
    {some temporary array for array bindings}
    function ConvertToOracleSQLQuery: RawByteString;
    function CreateResultSet: IZResultSet;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    procedure ReleaseConnection; override;
  public
    constructor Create(const PlainDriver: IZOraclePlainDriver;
      const Connection: IZConnection; const SQL: string; Info: TStrings); overload;
    constructor Create(const PlainDriver: IZOraclePlainDriver;
      const Connection: IZConnection; Info: TStrings); overload;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;
  TZOracleStatement = class(TZAbstractPreparedStatement);

  TZOracleCallableStatement = class(TZAbstractCallableStatement,
    IZParamNamedCallableStatement)
  private
    FOutParamCount: Integer;
    FErrorHandle: POCIError;
    FParams: PZSQLVars;
    FPlainDriver:IZOraclePlainDriver;
    FHandle: POCIStmt;
    FOracleParams: TZOracleParams;
    FOracleParamsCount: Integer;
    FParamNames: TStringDynArray;
    PackageIncludedList: TStrings;
    FParamsBuffer: TByteDynArray;
    FRowPrefetchSize: ub4;
    FZBufferSize: Integer;
    FStatementType: ub2;
    FIteration: Integer;
    FCanBindInt64: Boolean;
    FOracleConnection: IZOracleConnection;
    procedure SortZeosOrderToOCIParamsOrder;
    procedure FetchOutParamsFromOracleVars;
    function GetProcedureSql: RawByteString;
  protected
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      const Value: TZVariant); override;
    procedure RegisterParamTypeAndName(const ParameterIndex:integer;
      ParamTypeName: String; const ParamName: String; Const {%H-}ColumnSize, {%H-}Precision: Integer);
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    procedure RegisterOutParameter(ParameterIndex: Integer; SQLType: Integer); override;
    procedure RegisterParamType(ParameterIndex: integer; ParamType: Integer); override;
    procedure Prepare; override;
    procedure Unprepare; override;

    Function ExecuteUpdatePrepared: Integer; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    constructor Create(const Connection: IZConnection; const pProcName: string; Info: TStrings);
    destructor Destroy; override;
    procedure ClearParameters; override;
  end;

{$ENDIF ZEOS_DISABLE_ORACLE}
implementation
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZFastCode, ZDbcOracleResultSet,
  ZEncoding, ZDbcUtils;

{ TZOraclePreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZOraclePreparedStatement.Create(
  const PlainDriver: IZOraclePlainDriver; const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
  fOracleConnection := Connection as IZOracleConnection;
  ASQL := ConvertToOracleSQLQuery;
  FCanBindInt64 := Connection.GetClientVersion >= 11002000;
  FRowPrefetchSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'row_prefetch_size', ''), 131072);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072);
end;

constructor TZOraclePreparedStatement.Create(const PlainDriver: IZOraclePlainDriver;
  const Connection: IZConnection; Info: TStrings);
begin
  Create(PlainDriver, Connection, '', Info);
end;

{**
  Converts an SQL query into Oracle format.
  @param SQL a query with parameters defined with '?'
  @returns a query with parameters in Oracle format ':pN'.
}
function TZOraclePreparedStatement.ConvertToOracleSQLQuery: RawByteString;
var
  I, N: Integer;
  SelectFound: Boolean;
begin
  FServerStmtCache := False;
  SelectFound := False;
  N := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do begin
    SelectFound := (I = 0) and (UpperCase(CachedQueryRaw[i]) = 'SELECT');
    if IsParamIndex[i] then begin
      FServerStmtCache := True;
      Inc(N);
      Result := Result + ':P' + IntToRaw(N);
    end else begin
      if SelectFound and not FServerStmtCache then
        SelectFound := UpperCase(CachedQueryRaw[i]) <> 'WHERE';
      Result := Result + CachedQueryRaw[i];
    end;
  end;
  FServerStmtCache := SelectFound or FServerStmtCache;
end;

function TZOraclePreparedStatement.CreateResultSet: IZResultSet;
begin
  if FOpenResultSet = nil then begin
    Result := CreateOracleResultSet(FPlainDriver, Self, SQL, FHandle, FErrorHandle, FZBufferSize);
    FOpenResultSet := Pointer(Result);
  end else
    Result := IZResultSet(FOpenResultSet);
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZOraclePreparedStatement.PrepareInParameters;
var
  I: Integer;
  CurrentVar: PZSQLVar;
  Status: Integer;
  BufferSize: Int64;
  CurrentBufferEntry: PAnsiChar;
  Label CheckMaxIter;
begin
  AllocateOracleSQLVars(FParams, InParamCount);
  BufferSize := 0;
  FIteration := 0;
  if FParams^.AllocNum = 0 then goto CheckMaxIter; //nothing to do here

  {first determine oracle type and check out required buffer-size we need }
  for I := 0 to FParams^.AllocNum - 1 do
  begin
    {$R-}
    CurrentVar := @FParams.Variables[I];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    CurrentVar.Handle := nil;

    { Artificially define Oracle internal type. }
    if InParamTypes[I] = stBytes then
      DefineOracleVarTypes(CurrentVar, InParamTypes[I], Max_OCI_Raw_Size, SQLT_LVC, FCanBindInt64)
    else if InParamTypes[I] = stBinaryStream then
      DefineOracleVarTypes(CurrentVar, InParamTypes[I], Max_OCI_String_Size, SQLT_BLOB, FCanBindInt64)
    else if InParamTypes[I] in [stAsciiStream, stUnicodeStream] then
      DefineOracleVarTypes(CurrentVar, InParamTypes[I], Max_OCI_String_Size, SQLT_CLOB, FCanBindInt64)
    else
      DefineOracleVarTypes(CurrentVar, InParamTypes[I], Max_OCI_String_Size, SQLT_STR, FCanBindInt64);
    Inc(BufferSize, CalcBufferSizeOfSQLVar(CurrentVar));
  end; //Buffer size is determined now
  FIteration := Ord((ArrayCount = 0) and (InparamCount > 0)) or ArrayCount; //determine initial iters
  Inc(BufferSize, BufferSize * FIteration); //determine inital buffersize
  if BufferSize >= High(LongWord)-1 then
    raise Exception.Create('Memory out of bounds! OCI-Limit = 4GB -1Byte');
  if Length(FParamsBuffer) < BufferSize then SetLength(FParamsBuffer, BufferSize); //Alloc new buffer if required
  CurrentBufferEntry := Pointer(FParamsBuffer);

  { now let's set data-entries, bind them }
  for i := 0 to FParams.AllocNum -1 do
  begin
    CurrentVar := @FParams.Variables[I];
    CurrentVar.Handle := nil;
    SetVariableDataEntrys(CurrentBufferEntry, CurrentVar, FIteration);
    AllocDesriptors(FPlainDriver, (Connection as IZOracleConnection).GetConnectionHandle,
      CurrentVar, FIteration, True);
    Status := FPlainDriver.BindByPos(FHandle, CurrentVar^.BindHandle, FErrorHandle,
      I + 1, CurrentVar^.Data, CurrentVar^.Length, CurrentVar^.TypeCode,
      CurrentVar^.oIndicatorArray, CurrentVar^.oDataSizeArray, nil, 0, nil, OCI_DEFAULT);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, ASQL, ConSettings);
  end;
  CheckMaxIter:
  FIteration := Max(FIteration, 1);
end;

procedure TZOraclePreparedStatement.ReleaseConnection;
begin
  inherited;
  FOracleConnection := nil;
end;

{**
  Binds the input parameters
}
procedure TZOraclePreparedStatement.BindInParameters;
var
  I: Integer;
begin
  {$R-}
  if FParams^.AllocNum > 0 then
  for I := 0 to FParams^.AllocNum - 1 do
    LoadOracleVar(FPlainDriver, Connection, FErrorHandle, @FParams.Variables[I],
      InParamValues[i], ChunkSize, Max(1, Min(FIteration, ArrayCount)));
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  inherited BindInParameters;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZOraclePreparedStatement.UnPrepareInParameters;
begin
  FreeOracleSQLVars(FPlainDriver, FParams, FIteration,
    (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle, ConSettings)
end;

{**
  Prepares an SQL statement
}
procedure TZOraclePreparedStatement.Prepare;
begin
  if not Prepared then
  begin
    if (FHandle = nil) or (FErrorHandle = nil) then
    { Allocates statement handles. }
    AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle, False{FServerStmtCache});
    { prepare stmt }
    PrepareOracleStatement(FPlainDriver, (Connection as IZOracleConnection).GetContextHandle,
      ASQL, FHandle, FErrorHandle, FRowPrefetchSize, False{FServerStmtCache}, ConSettings);
    { get Statemant type }
    FPlainDriver.AttrGet(FHandle, OCI_HTYPE_STMT, @FStatementType, nil,
      OCI_ATTR_STMT_TYPE, FErrorHandle);
    inherited Prepare;
  end;
end;

procedure TZOraclePreparedStatement.UnPrepare;
const RELEASE_MODE: array[boolean] of integer = (OCI_DEFAULT,OCI_STMTCACHE_DELETE);
begin
  try
    if False and FServerStmtCache then
    CheckOracleError(FPlainDriver, FErrorHandle,
        FplainDriver.StmtRelease(FHandle, FErrorHandle, nil, 0, RELEASE_MODE[False]),
      lcExecute, ASQL, ConSettings)
    else
      FreeOracleStatementHandles(FPlainDriver, FHandle, FErrorHandle);
  finally
    inherited Unprepare;
  end;
end;


const CommitMode: array[Boolean] of ub4 = (OCI_DEFAULT, OCI_COMMIT_ON_SUCCESS);

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecutePrepared: Boolean;
begin
  Result := False;
  { Prepares a statement. }
  Prepare;
  PrepareLastResultSetForReUse;
  BindInParameters;

  if FStatementType = OCI_STMT_SELECT then
  begin
    { Executes the statement and gets a resultset. }
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet;
    Result := LastResultSet <> nil;
  end
  else
  begin
    { Executes the statement and gets a result. }
    CheckOracleError(FPlainDriver, FErrorHandle,
      FPlainDriver.StmtExecute(FOracleConnection.GetContextHandle,
        FHandle, FErrorHandle, FIteration, 0, nil, nil, CommitMode[Connection.GetAutoCommit]),
      lcExecute, ASQL, ConSettings);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
  end;
  inherited ExecutePrepared;

  { Unloads binded variables with values. }
  UnloadOracleVars(FParams, FIteration)
  { Autocommit statement. done by ExecuteOracleStatement}
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZOraclePreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Prepares a statement. }
  Prepare;
  PrepareOpenResultSetForReUse;
  BindInParameters;

  { Executes the statement and gets a resultset. }
  Result := CreateResultSet;
  inherited ExecuteQueryPrepared;

  { Unloads binded variables with values. }
  UnloadOracleVars(FParams, FIteration)
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
function TZOraclePreparedStatement.ExecuteUpdatePrepared: Integer;
var
  ResultSet: IZResultSet;
begin
  { Prepares a statement. }
  Prepare;

  if FOpenResultSet <> nil then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;

  BindInParameters;
  try
    if FStatementType = OCI_STMT_SELECT then
    begin
      LastUpdateCount := -1;

      { Executes the statement and gets a resultset. }
      ResultSet := CreateResultSet;
      try
        while ResultSet.Next do;
        LastUpdateCount := ResultSet.GetRow;
      finally
        ResultSet.Close;
      end;
    end
    else
    begin
      { Executes the statement and gets a result. }
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.StmtExecute(FOracleConnection.GetContextHandle,
          FHandle, FErrorHandle, Max(1, Min(FIteration, ArrayCount)), 0, nil, nil, CommitMode[Connection.GetAutoCommit]),
        lcExecute, ASQL, ConSettings);
      LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
    end;
    Result := LastUpdateCount;
    inherited ExecuteUpdatePrepared;
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FParams, FIteration)
  end;

  { Autocommit statement. done by ExecuteOracleStatement}
end;

procedure TZOracleCallableStatement.Prepare;
begin
  if not Prepared then
  begin
    ASQL := GetProcedureSql;
    { Allocates statement handles. }
    if (FHandle = nil) or (FErrorHandle = nil) then
      AllocateOracleStatementHandles(FPlainDriver, Connection,
        FHandle, FErrorHandle);
    PrepareOracleStatement(FPlainDriver, nil, ASQL, FHandle, FErrorHandle,
          FRowPrefetchSize, False, ConSettings);
    FPlainDriver.AttrGet(FHandle, OCI_HTYPE_STMT, @FStatementType, nil,
      OCI_ATTR_STMT_TYPE, FErrorHandle);
    inherited Prepare;
  end;
end;

procedure TZOracleCallableStatement.UnPrepare;
const {%H-}RELEASE_MODE: array[boolean] of integer = (OCI_DEFAULT,OCI_STMTCACHE_DELETE);
begin
  try
    {if FServerStmtCache then
      CheckOracleError(FPlainDriver, FErrorHandle,
        FplainDriver.StmtRelease(FHandle, FErrorHandle, nil, 0, RELEASE_MODE[False]),
      lcExecute, ASQL, ConSettings)
    else}
      FreeOracleStatementHandles(FPlainDriver, FHandle, FErrorHandle);
  finally
    inherited Unprepare;
  end;
end;

procedure TZOracleCallableStatement.RegisterOutParameter(ParameterIndex,
  SQLType: Integer);
begin
  inherited RegisterOutParameter(ParameterIndex,SQLType);
  with FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do
  begin
    if not GetConnection.UseMetadata then
      pName := 'pOut'+ZFastCode.IntToStr(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
    pSQLType := SQLType;
  end;
end;

procedure TZOracleCallableStatement.RegisterParamType(ParameterIndex: integer;
  ParamType: Integer);
begin
  inherited RegisterParamType(ParameterIndex, ParamType);
  if ParameterIndex > High(FOracleParams) then
    SetLength(FOracleParams, ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
  if ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} > FOracleParamsCount then
    FOracleParamsCount := ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF};
  FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pType := TZProcedureColumnType(ParamType);
  FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pParamIndex := ParameterIndex;
  if ParamType >= Ord(pctInOut) then  begin
    FOracleParams[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].pOutIndex := FOutParamCount;
    Inc(FOutParamCount);
  end;
end;

procedure TZOracleCallableStatement.SetInParam(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: TZVariant);
var 
  AConnection: IZConnection;

  function GetOracleParamIndexOfParameterIndex: Integer;
  var I: Integer;
  begin
    Result := 0;
    for i := 0 to high(FOracleParams) do
      if ParameterIndex = FOracleParams[i].pParamIndex then
      begin
        Result := I;
        Break;
      end;
  end;

begin
  inherited SetInParam(ParameterIndex, SQLType, Value);
  with FOracleParams[GetOracleParamIndexOfParameterIndex] do
  begin
    AConnection := GetConnection;
    if Assigned(AConnection) and ( not AConnection.UseMetadata ) then
      pName := 'p'+ZFastCode.IntToStr(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
    pSQLType := ord(SQLType);
  end;
end;

procedure TZOracleCallableStatement.RegisterParamTypeAndName(const ParameterIndex: integer;
  ParamTypeName: String; const ParamName: String; Const ColumnSize, Precision: Integer);
var
  iPos: Integer;
  ProcName: String;
begin
  FOracleParams[ParameterIndex].pName := ParamName;
  FOracleParams[ParameterIndex].pTypeName := ParamTypeName;
  iPos := ZFastCode.Pos('.', ParamName);
  if iPos > 0 then
  begin
    ProcName := Copy(ParamName, 1, iPos-1); //extract function or Procedure names
    FOracleParams[ParameterIndex].pProcIndex := PackageIncludedList.IndexOf(ProcName); //check index
    if FOracleParams[ParameterIndex].pProcIndex = -1 then //if not exists
      FOracleParams[ParameterIndex].pProcIndex := PackageIncludedList.Add(ProcName); //Add to List
  end
  else //No package
    FOracleParams[ParameterIndex].pProcIndex := 0;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZOracleCallableStatement.PrepareInParameters;
var
  I: Integer;
  CurrentVar: PZSQLVar;
  Status: Integer;
  BufferSize: Int64;
  CurrentBufferEntry: PAnsiChar;
  SQLType: TZSQLType;
  Label CheckMaxIter;
begin
  AllocateOracleSQLVars(FParams, FOracleParamsCount);
  SortZeosOrderToOCIParamsOrder;
  SetLength(FParamNames, FOracleParamsCount);
  BufferSize := 0;
  FIteration := 0;
  if FParams^.AllocNum = 0 then goto CheckMaxIter; //nothing to do here

  {first determine oracle type and check out required buffer-size we need }
  for I := 0 to FParams^.AllocNum - 1 do
  begin
    FParamNames[I] := Self.FOracleParams[I].pName;
    {$R-}
    CurrentVar := @FParams.Variables[I];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    CurrentVar.Handle := nil;
    SQLType := TZSQLType(FOracleParams[I].pSQLType);
    { Artificially define Oracle internal type. }
    if SQLType = stBytes then
      DefineOracleVarTypes(CurrentVar, SQLType, Max_OCI_Raw_Size, SQLT_LVC, FCanBindInt64)
    else if SQLType = stBinaryStream then
      DefineOracleVarTypes(CurrentVar, SQLType, Max_OCI_String_Size, SQLT_BLOB, FCanBindInt64)
    else if SQLType in [stAsciiStream, stUnicodeStream] then
      DefineOracleVarTypes(CurrentVar, SQLType, Max_OCI_String_Size, SQLT_CLOB, FCanBindInt64)
    else
      DefineOracleVarTypes(CurrentVar, SQLType, Max_OCI_String_Size, SQLT_STR, FCanBindInt64);
    Inc(BufferSize, CalcBufferSizeOfSQLVar(CurrentVar));
  end; //Buffer size is determined now
  FIteration := Ord((ArrayCount = 0) and (InparamCount > 0)) or ArrayCount; //determine initial iters
  Inc(BufferSize, BufferSize * FIteration); //determine inital buffersize
  if BufferSize >= High(LongWord)-1 then
    raise Exception.Create('Memory out of bounds! OCI-Limit = 4GB -1Byte');
  if Length(FParamsBuffer) < BufferSize then SetLength(FParamsBuffer, BufferSize); //Alloc new buffer if required
  CurrentBufferEntry := Pointer(FParamsBuffer);

  { now let's set data-entries, bind them }
  for i := 0 to FParams.AllocNum -1 do
  begin
    {$R-}
    CurrentVar := @FParams.Variables[I];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    CurrentVar.Handle := nil;
    SetVariableDataEntrys(CurrentBufferEntry, CurrentVar, FIteration);
    AllocDesriptors(FPlainDriver, (Connection as IZOracleConnection).GetConnectionHandle,
      CurrentVar, FIteration, True);
    Status := FPlainDriver.BindByPos(FHandle, CurrentVar^.BindHandle, FErrorHandle,
      I + 1, CurrentVar^.Data, CurrentVar^.Length, CurrentVar^.TypeCode,
      CurrentVar^.oIndicatorArray, CurrentVar^.oDataSizeArray, nil, 0, nil, OCI_DEFAULT);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcExecute, ASQL, ConSettings);
  end;
  CheckMaxIter:
  FIteration := Max(FIteration, 1);
end;

{**
  Binds the input parameters
}
procedure TZOracleCallableStatement.BindInParameters;
var
  I: Integer;
begin
  FIteration := Max(1, Min(FIteration, ArrayCount));
  if FParams^.AllocNum > 0 then
    {$R-}
    for I := 0 to FParams^.AllocNum - 1 do
      if (FOracleParams[i].pType in [pctIn, pctInOut]) then
        LoadOracleVar(FPlainDriver, Connection, FErrorHandle, @FParams.Variables[I],
          InParamValues[FOracleParams[i].pParamIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}],
            ChunkSize, Max(1, Min(FIteration, ArrayCount)))
      else
        LoadOracleVar(FPlainDriver, Connection, FErrorHandle,
          @FParams.Variables[I], NullVariant, ChunkSize,
            Max(1, Min(FIteration, ArrayCount)));
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  inherited BindInParameters;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZOracleCallableStatement.UnPrepareInParameters;
begin
  FreeOracleSQLVars(FPlainDriver, FParams, FIteration,
    (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle, ConSettings)
end;

procedure TZOracleCallableStatement.SortZeosOrderToOCIParamsOrder;
var
  I, J, NewProcIndex, StartProcIndex: Integer;
  TempOraVar: TZOracleParam;
begin
  NewProcIndex := -1;
  StartProcIndex := 0;
  if IsFunction then
    for i := 0 to high(FOracleParams) do begin
      if not ( FOracleParams[i].pProcIndex = NewProcIndex ) then begin
        NewProcIndex := FOracleParams[i].pProcIndex;
        StartProcIndex := I;
      end;
      if ( FOracleParams[i].pType = pctReturn) and not (i = StartProcIndex) then begin
        TempOraVar := FOracleParams[I];
        for J := I downto StartProcIndex+1 do
          FOracleParams[j] := FOracleParams[j-1];
        FOracleParams[StartProcIndex] := TempOraVar;
      end;
    end;
end;

procedure TZOracleCallableStatement.FetchOutParamsFromOracleVars;
var
  LobLocator: POCILobLocator;
  I: integer;
  TempBlob: IZBlob;

  procedure SetOutParam(CurrentVar: PZSQLVar; Index: Integer);
  var
    Year:SmallInt;
    Month, Day:Byte; Hour, Min, Sec:ub1; MSec: ub4;
    {$IFDEF UNICODE}
    {$ELSE}
    RawTemp: RawByteString;
    {$ENDIF}
  begin
    if CurrentVar^.oIndicatorArray[0] < 0 then
      outParamValues[Index] := NullVariant
    else
      case CurrentVar^.TypeCode of
        SQLT_INT: outParamValues[Index] := EncodeInteger(PInteger(CurrentVar^.Data)^ );
        SQLT_FLT: outParamValues[Index] := EncodeFloat(PDouble(CurrentVar^.Data)^ );
        SQLT_STR:
          begin
            {$IFDEF UNICODE}
            outParamValues[Index] := EncodeString(PRawToUnicode(CurrentVar^.Data,
              CurrentVar^.oDataSizeArray[0], ConSettings^.ClientCodePage^.CP));
            {$ELSE}
            ZSetString(CurrentVar^.Data, CurrentVar^.oDataSizeArray[0], RawTemp{%H-});
            outParamValues[Index] := EncodeString(ConSettings.ConvFuncs.ZRawToString(RawTemp, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP));
            {$ENDIF}
          end;
        SQLT_TIMESTAMP:
          begin
            FPlainDriver.DateTimeGetDate(
              FOracleConnection.GetConnectionHandle ,
              FErrorHandle, PPOCIDescriptor(CurrentVar^.Data)^,
              Year{%H-}, Month{%H-}, Day{%H-});
            FPlainDriver.DateTimeGetTime(
              FOracleConnection.GetConnectionHandle ,
              FErrorHandle, PPOCIDescriptor(CurrentVar^.Data)^,
              Hour{%H-}, Min{%H-}, Sec{%H-},MSec{%H-});
            outParamValues[Index] := EncodeDateTime(EncodeDate(year,month,day )+EncodeTime(Hour,min,sec,  msec div 1000000));
          end;
        SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE:
          begin
            LobLocator := PPOCIDescriptor(CurrentVar^.Data)^;
            if CurrentVar^.TypeCode in [SQLT_BLOB, SQLT_BFILEE] then
              TempBlob := TZOracleBlob.Create(FPlainDriver, nil, 0,
                FOracleConnection.GetContextHandle, FOracleConnection.GetErrorHandle,
                  LobLocator, GetChunkSize, ConSettings)
            else
              TempBlob := TZOracleClob.Create(FPlainDriver, nil, 0,
                FOracleConnection.GetConnectionHandle,
                FOracleConnection.GetContextHandle, FOracleConnection.GetErrorHandle,
                LobLocator, GetChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
            outParamValues[Index] := EncodeInterface(TempBlob);
            TempBlob := nil;
          end;
        SQLT_NTY: //currently not supported
          outParamValues[Index] := NullVariant;
      end;
  end;
begin
  {$R-}
  for I := 0 to FOracleParamsCount -1 do
    if Ord(FOracleParams[i].pType) >= Ord(pctInOut) then
      SetOutParam(@FParams^.Variables[I], FOracleParams[i].pParamIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF});
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

function TZOracleCallableStatement.GetProcedureSql: RawByteString;
var
  sFunc: string;
  I, IncludeCount, LastIndex: Integer;
  PackageBody: TStrings;
  TempResult: String;

  function GenerateParamsStr(Count: integer): string;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      if ( FDBParamTypes[I] = pctReturn ) then
      begin
        sFunc := ' :'+FOracleParams[0].pName+' := ';
        continue;
      end;
      if Result <> '' then
        Result := Result + ',';
      if IsFunction then
        Result := Result + ':'+FOracleParams[I+1].pName
      else
        Result := Result + ':'+FOracleParams[I].pName;
    end;
    Result := '('+Result+')'
  end;

var
  InParams, sName: string;
begin
  sFunc := '';
  if PackageIncludedList.Count > 0 then
  begin
    PackageBody := TStringList.Create;
    PackageBody.Add('BEGIN');
    LastIndex := 0;
    for IncludeCount := 0 to PackageIncludedList.Count -1 do
    begin
      InParams := '';
      sFunc := '';
      for i := LastIndex to high(FOracleParams) do
        if IncludeCount = FOracleParams[i].pProcIndex then
        begin
          sName := RemoveChar('.', FOracleParams[I].pName);
          if ( FOracleParams[I].pType = pctReturn ) then
            sFunc := ' :'+sName+' := '
          else if InParams <> ''
            then InParams := InParams +', :'+sName
            else InParams := InParams +':'+sName
        end else begin
          LastIndex := I;
          break;
        end;
      PackageBody.Add('BEGIN '+sFunc+SQL+
        '.'+GetConnection.GetMetadata.GetIdentifierConvertor.Quote(PackageIncludedList[IncludeCount])+'('+InParams+'); END;');
    end;
    PackageBody.Add('END;');
    TempResult := TrimRight(PackageBody.Text);
    FreeAndNil(PackageBody);
  end
  else
  begin
    InParams := GenerateParamsStr( FOracleParamsCount );
    TempResult := 'BEGIN ' + sFunc +SQL + InParams+'; END;';
  end;
  Result := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(TempResult);
end;

procedure TZOracleCallableStatement.ClearParameters;
begin
  inherited;
  FOracleParamsCount := 0;
  SetLength(FOracleParams, 0);
end;

constructor TZOracleCallableStatement.Create(const Connection: IZConnection;
  const pProcName: string; Info: TStrings);
begin
  inherited Create(Connection, pProcName, Info);
  FOracleConnection := Connection as IZOracleConnection;
  FOracleParamsCount := 0;
  FPlainDriver := Connection.GetIZPlainDriver as IZOraclePlainDriver;
  ResultSetType := rtForwardOnly;
  PackageIncludedList := TStringList.Create;
  FOutParamCount := 0;
  FCanBindInt64 := Connection.GetClientVersion >= 11002000;
  FRowPrefetchSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'row_prefetch_size', ''), 131072);
  FZBufferSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(ZDbcUtils.DefineStatementParameter(Self, 'internal_buffer_size', ''), 131072);
  FIteration := 1;
end;

destructor TZOracleCallableStatement.Destroy;
begin
  FreeOracleSQLVars(FPlainDriver, FParams, FIteration, (Connection as IZOracleConnection).GetConnectionHandle, FErrorHandle, ConSettings);
  PackageIncludedList.Free;
  inherited;
end;

function TZOracleCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  { Prepares a statement. }
  Prepare;

  BindInParameters;
  try
    CheckOracleError(FPlainDriver, FErrorHandle,
      FPlainDriver.StmtExecute(FOracleConnection.GetContextHandle,
        FHandle, FErrorHandle, FIteration, 0, nil, nil, CommitMode[Connection.GetAutoCommit]),
      lcExecute, ASQL, ConSettings);
    LastUpdateCount := GetOracleUpdateCount(FPlainDriver, FHandle, FErrorHandle);
    FetchOutParamsFromOracleVars;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FParams, FIteration)
  end;

  { Autocommit statement. done by ExecuteOracleStatement}
  Result := LastUpdateCount;
end;

function TZOracleCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  { Prepares a statement. }
  Prepare;

  BindInParameters;
  try
    CheckOracleError(FPlainDriver, FErrorHandle,
      FPlainDriver.StmtExecute(FOracleConnection.GetContextHandle,
        FHandle, FErrorHandle, FIteration, 0, nil, nil, CommitMode[Connection.GetAutoCommit]),
      lcExecute, ASQL, ConSettings);
    FetchOutParamsFromOracleVars;
    LastResultSet := CreateOracleResultSet(FPlainDriver, Self, Self.SQL,
      FHandle, FErrorHandle, FParams, FOracleParams);
    Result := LastResultSet;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  finally
    { Unloads binded variables with values. }
    UnloadOracleVars(FParams, FIteration);
  end;
end;

{$ENDIF ZEOS_DISABLE_ORACLE}
end.
