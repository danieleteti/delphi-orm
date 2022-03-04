{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
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

unit ZDbcASAStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_ASA}
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcStatement, ZDbcASA, ZDbcASAUtils, ZPlainASADriver,
  ZCompatibility, ZDbcLogging, ZVariant;

type
  {** Implements Prepared SQL Statement. }
  TZASAPreparedStatement = class(TZAbstractPreparedStatement)
  private
    FCursorOptions: SmallInt;
    FStmtNum: SmallInt;
    FASAConnection: IZASAConnection;
    FPlainDriver: IZASAPlainDriver;
    FParamSQLData: IZASASQLDA;
    FSQLData: IZASASQLDA;
    FMoreResults: Boolean;
  private
    function OpenResultSet: IZResultSet;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    procedure ReleaseConnection; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings); overload;
    constructor Create(const Connection: IZConnection; Info: TStrings); overload;
    destructor Destroy; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    procedure AfterClose; override;
    procedure Cancel; override;
    function GetMoreResults: Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;
  TZASAStatement = Class(TZASAPreparedStatement);

  TZASACallableStatement = class(TZAbstractCallableStatement)
  private
    FCachedBlob: boolean;
    FStmtNum: SmallInt;
    FASAConnection: IZASAConnection;
    FPlainDriver: IZASAPlainDriver;
    FParamSQLData: IZASASQLDA;
    FSQLData: IZASASQLDA;
    FMoreResults: Boolean;
    FPrepared: Boolean;
  protected
    function GetProcedureSQL: RawByteString;
    procedure ReleaseConnection; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    procedure BeforeClose; override;
    procedure Cancel; override;
    function GetMoreResults: Boolean; override;
    function ExecuteQuery(const {%H-}SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const {%H-}SQL: RawByteString): Integer; override;
    function Execute(const {%H-}SQL: RawByteString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses ZSysUtils, ZDbcUtils, ZMessages, ZPlainASAConstants, ZDbcASAResultSet,
  ZEncoding, ZFastCode;

{ TZASAPreparedStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param SQL the query
  @param Info a statement parameters.
}
constructor TZASAPreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FASAConnection := Connection as IZASAConnection;
  FPlainDriver := FASAConnection.GetIZPlainDriver as IZASAPlainDriver;
  FetchSize := BlockSize;
  ResultSetType := rtScrollSensitive;
  CursorName := IntToRaw(NativeUInt(FASAConnection.GetDBHandle))+IntToRaw(FStatementId);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
}
constructor TZASAPreparedStatement.Create(const Connection: IZConnection; Info: TStrings);
begin
  Create(Connection, '', Info);
end;

destructor TZASAPreparedStatement.Destroy;
begin
  FSQLData := nil;
  FParamSQLData := nil;
  inherited Destroy;
  FASAConnection := nil;
end;

function TZASAPreparedStatement.OpenResultSet: IZResultSet;
begin
  With FASAConnection do
  begin
    ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings, ASQL);
    FSQLData := TZASASQLDA.Create(FPlainDriver,
      FASAConnection.GetDBHandle, Pointer(CursorName), ConSettings);
    DescribeCursor(FASAConnection, FSQLData, CursorName, ASQL);
    if ResultSetConcurrency = rcUpdatable then
      Result := GetCachedResultSet(SQL, Self,
        {Result :=  TZASACachedResultSet}TZASANativeResultSet.Create(Self, SQL, FStmtNum, CursorName, FSQLData, CachedLob))
    else
      Result := TZASANativeResultSet.Create(Self, SQL, FStmtNum, CursorName, FSQLData, CachedLob);
    FOpenResultSet := Pointer(Result);
  end;
end;

procedure TZASAPreparedStatement.PrepareInParameters;
begin
  with FASAConnection do
    if FParamSQLData.GetData^.sqld > FParamSQLData.GetData^.sqln then
    begin
      FParamSQLData.AllocateSQLDA(FParamSQLData.GetData^.sqld);
      GetPlainDriver.db_describe(GetDBHandle, nil, @FStmtNum,
        FParamSQLData.GetData, SQL_DESCRIBE_INPUT);
      ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, GetConSettings, ASQL);
      {sade: initfields doesnt't help > ASA describes !paramcount! only}
    end;
end;

procedure TZASAPreparedStatement.ReleaseConnection;
begin
  inherited;
  FASAConnection := nil;
end;

procedure TZASAPreparedStatement.BindInParameters;
begin
  PrepareParameters(ClientVarManager, InParamValues, InParamTypes,
    InParamCount, FParamSQLData, FASAConnection.GetConSettings);
end;

procedure TZASAPreparedStatement.UnPrepareInParameters;
begin
  FParamSQLData := nil;
end;

procedure TZASAPreparedStatement.Prepare;
begin
  if not Prepared then
  begin
    with FASAConnection do
    begin
      if FStmtNum <> 0 then
      begin
        GetPlainDriver.db_dropstmt(GetDBHandle, nil, nil, @FStmtNum);
        FStmtNum := 0;
      end;
      if ResultSetConcurrency = rcUpdatable then
        FCursorOptions := CUR_OPEN_DECLARE + CUR_UPDATE
      else
        FCursorOptions := CUR_OPEN_DECLARE + CUR_READONLY;
      if ResultSetType = rtScrollInsensitive then
        FCursorOptions := FCursorOptions + CUR_INSENSITIVE;
      FParamSQLData := TZASASQLDA.Create(FPlainDriver,
        FASAConnection.GetDBHandle, Pointer(CursorName), ConSettings);
      FPlainDriver.db_prepare_describe(GetDBHandle, nil, @FStmtNum, Pointer(ASQL),
          FParamSQLData.GetData, SQL_PREPARE_DESCRIBE_STMTNUM +
          SQL_PREPARE_DESCRIBE_INPUT + SQL_PREPARE_DESCRIBE_VARRESULT, 0);
      ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, GetConSettings, ASQL);
      FMoreResults := GetDBHandle.sqlerrd[2] = 0; //we need to know if more ResultSets can be retrieved
    end;
    inherited Prepare
  end;
end;

procedure TZASAPreparedStatement.Unprepare;
begin
  FSQLData := nil;
  if not Assigned(FOpenResultSet) then //on closing the RS we exec db_close
    FASAConnection.GetPlainDriver.db_close(FASAConnection.GetDBHandle, Pointer(CursorName));
  inherited Unprepare;
end;

procedure TZASAPreparedStatement.AfterClose;
begin
  if FStmtNum <> 0 then begin
    FASAConnection.GetPlainDriver.db_dropstmt(FASAConnection.GetDBHandle, nil, nil, @FStmtNum);
    FStmtNum := 0;
  end;
  FParamSQLData := nil;
end;

procedure TZASAPreparedStatement.Cancel;
begin
  with FASAConnection do
  begin
    FPlainDriver.db_cancel_request(GetDBHandle);
    ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings, ASQL);
  end;
end;

function TZASAPreparedStatement.GetMoreResults: Boolean;
begin
  Result := FMoreResults;
  if FMoreResults then
  begin
    with FASAConnection do
    begin
      GetPlainDriver.db_resume(GetDBHandle, Pointer(CursorName));
      ZDbcASAUtils.CheckASAError(GetPlainDriver, GetDBHandle, lcExecute, ConSettings);
      if GetDBHandle.sqlcode = SQLE_PROCEDURE_COMPLETE then
        Result := false
      else
        DescribeCursor(FASAConnection, FSQLData, CursorName, '');
    end;
  end;
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

  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZASAPreparedStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  BindInParameters;
  if FMoreResults then
  begin
    LastResultSet := ExecuteQueryPrepared;
    Result := true;
  end
  else
  begin
    FASAConnection.GetPlainDriver.db_open(FASAConnection.GetDBHandle, Pointer(CursorName), nil, @FStmtNum,
      FParamSQLData.GetData, FetchSize, 0, CUR_OPEN_DECLARE + CUR_READONLY);  //need a way to know if a resultset can be retrieved
    if FASAConnection.GetDBHandle.sqlCode = SQLE_OPEN_CURSOR_ERROR then
    begin
      ExecuteUpdatePrepared;
      Result := False;
    end
    else
    begin
      LastResultSet := OpenResultSet;
      Result := true;
    end;
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZASAPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  PrepareOpenResultSetForReUse;
  BindInParameters;

  with FASAConnection do
  begin
    GetPlainDriver.db_open(GetDBHandle, Pointer(CursorName), nil, @FStmtNum,
      FParamSQLData.GetData, FetchSize, 0, FCursorOptions);
    if Assigned(FOpenResultSet) then
      Result := IZResultSet(FOpenResultSet)
    else
      Result := OpenResultSet;
  end;
  { Logging SQL Command and values}
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
function TZASAPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  BindInParameters;
  with FASAConnection do
  begin
    GetPlainDriver.db_execute_into(GetDBHandle, nil, nil, @FStmtNum,
      FParamSQLData.GetData, nil);
    ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings,
      ASQL, SQLE_TOO_MANY_RECORDS);
    Result := GetDBHandle.sqlErrd[2];
    LastUpdateCount := Result;
  end;
  { Autocommit statement. }
//  if Connection.GetAutoCommit then
//    Connection.Commit;
  { Logging SQL Command and values }
  inherited ExecuteUpdatePrepared;
end;


{ TZASACallableStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Info a statement parameters.
}
constructor TZASACallableStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FASAConnection := Connection as IZASAConnection;
  FPlainDriver := FASAConnection.GetIZPlainDriver as IZASAPlainDriver;
  FetchSize := BlockSize;
  ResultSetConcurrency := rcUpdatable;
  ResultSetType := rtScrollSensitive;
  FCachedBlob := StrToBoolEx(DefineStatementParameter(Self, 'cashedblob', 'true'));
  CursorName := IntToRaw(NativeUInt(FASAConnection.GetDBHandle))+IntToRaw(FStatementId);
  FParamSQLData := TZASASQLDA.Create(FPlainDriver,
    FASAConnection.GetDBHandle, Pointer(CursorName), ConSettings);
  FSQLData := TZASASQLDA.Create(FPlainDriver,
    FASAConnection.GetDBHandle, Pointer(CursorName), ConSettings);
end;

destructor TZASACallableStatement.Destroy;
begin
  FSQLData := nil;
  FParamSQLData := nil;
  inherited;
end;

procedure TZASACallableStatement.BeforeClose;
begin
  if not Closed then
  begin
    FASAConnection.GetPlainDriver.db_close(FASAConnection.GetDBHandle, PAnsiChar(CursorName));
    Closed := false;
  end;
  if FStmtNum <> 0 then
  begin
    FASAConnection.GetPlainDriver.db_dropstmt(FASAConnection.GetDBHandle, nil,
     nil, @FStmtNum);
    FStmtNum := 0;
  end;
  inherited BeforeClose;
end;

procedure TZASACallableStatement.Cancel;
begin
  with FASAConnection do
  begin
    FPlainDriver.db_cancel_request(GetDBHandle);
    ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings, ASQL);
  end;
end;

function TZASACallableStatement.GetMoreResults: Boolean;
begin
  Result := FMoreResults;
  if FMoreResults then
  begin
    with FASAConnection do
    begin
      GetPlainDriver.db_resume(GetDBHandle, PAnsiChar(CursorName));
      ZDbcASAUtils.CheckASAError(GetPlainDriver, GetDBHandle, lcExecute, ConSettings);
      if GetDBHandle.sqlcode = SQLE_PROCEDURE_COMPLETE then
        Result := false
      else
        DescribeCursor(FASAConnection, FSQLData, CursorName, '');
    end;
  end;
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
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}

function TZASACallableStatement.Execute(const SQL: RawByteString): Boolean;
var
  ProcSQL: RawByteString;
begin
  TrimInParameters;
  ProcSQL := GetProcedureSQL;
  if not FPrepared or (ASQL <> ProcSQL) then
  begin
    Close;
    ASQL := ProcSQL;
    ASAPrepare(FASAConnection, FSQLData, FParamSQLData, ASQL, @FStmtNum,
      FPrepared, FMoreResults);
  end;
  Result := ExecutePrepared;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZASACallableStatement.ExecutePrepared: Boolean;
begin
  if not FPrepared then
    Result := Execute(ASQL)
  else
  begin
    if FMoreResults or ((FSQLData.GetData.sqld > 0) and
      (FSQLData.GetData.sqlVar[0].sqlInd^ and DT_PROCEDURE_OUT = 0)) then
    begin
      LastResultSet := ExecuteQueryPrepared;
      Result := true;
    end
    else
    begin
      ExecuteUpdatePrepared;
      Result := false;
    end;
  end;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZASACallableStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
var
  ProcSQL: RawByteString;
begin
  TrimInParameters;
  ProcSQL := GetProcedureSQL;
  if not FPrepared or (ASQL <> ProcSQL) then
  begin
    Close;
    ASQL := ProcSQL;
    ASAPrepare(FASAConnection, FSQLData, FParamSQLData, ASQL, @FStmtNum,
      FPrepared, FMoreResults);
  end;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$HINTS OFF}
function TZASACallableStatement.ExecuteQueryPrepared: IZResultSet;
var
  CursorOptions: SmallInt;
begin
  if not FPrepared then
    Result := ExecuteQuery(ASQL)
  else
  begin
    with FASAConnection do
    begin
      PrepareParameters(ClientVarManager, InParamValues, InParamTypes,
        InParamCount, FParamSQLData, FASAConnection.GetConSettings);
      if ResultSetConcurrency = rcUpdatable then
        CursorOptions := CUR_OPEN_DECLARE + CUR_UPDATE
      else
        CursorOptions := CUR_OPEN_DECLARE + CUR_READONLY;
      if ResultSetType = rtScrollInsensitive then
        CursorOptions := CursorOptions + CUR_INSENSITIVE;
      GetPlainDriver.db_open(GetDBHandle, Pointer(FCursorName), nil, @FStmtNum,
        FParamSQLData.GetData, FetchSize, 0, CursorOptions);
      ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings, ASQL);
      Closed := false;
      try
        if FMoreResults then
          DescribeCursor(FASAConnection, FSQLData, FCursorName, ASQL);

        LastUpdateCount := -1;
        Result := GetCachedResultSet(Self.SQL, Self,
          TZASANativeResultSet.Create(Self, Self.SQL, FStmtNum, FCursorName, FSQLData, FCachedBlob));

        { Logging SQL Command }
        DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
      except
        on E: Exception do
        begin
          Self.Close;
          raise;
        end;
      end;
    end;
  end;
end;
{$HINTS ON}

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
function TZASACallableStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
var
  ProcSQL: RawByteString;
begin
  TrimInParameters;
  ProcSQL := GetProcedureSQL;
  if not FPrepared or (ASQL <> ProcSQL) then
  begin
    Close;
    ASQL := ProcSQL;
    ASAPrepare(FASAConnection, FSQLData, FParamSQLData, ASQL, @FStmtNum,
      FPrepared, FMoreResults);
  end;
  Result := ExecuteUpdatePrepared;
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
function TZASACallableStatement.ExecuteUpdatePrepared: Integer;
begin
  if not FPrepared then
    Result := ExecuteUpdate(SQL)
  else
  begin
    with FASAConnection do
    begin
      PrepareParameters(ClientVarManager, InParamValues, InParamTypes,
        InParamCount, FParamSQLData, FASAConnection.GetConSettings);
      GetPlainDriver.db_execute_into(GetDBHandle, nil, nil, @FStmtNum,
        FParamSQLData.GetData, FSQLData.GetData);
      ZDbcASAUtils.CheckASAError(FPlainDriver, GetDBHandle, lcExecute, ConSettings, ASQL);

      Result := GetDBHandle.sqlErrd[2];
      LastUpdateCount := Result;
      { Fetch data and fill Output params }
      AssignOutParamValuesFromResultSet(TZASAParamererResultSet.Create(Self, SQL,
        FStmtNum, CursorName, FSQLData, FCachedBlob), OutParamValues, OutParamCount , FDBParamTypes);
      { Logging SQL Command }
      DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
    end;
  end;
end;

{**
   Create sql string for calling stored procedure.
   @param SelectProc indicate use <b>EXECUTE PROCEDURE</b> or
    <b>SELECT</b> staement
   @return a Stored Procedure SQL string
}
function TZASACallableStatement.GetProcedureSql: RawByteString;

  function GenerateParamsStr(Count: integer): RawByteString;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + '?';
    end;
  end;

var
  InParams: RawByteString;
begin
  InParams := GenerateParamsStr(High(InParamValues) + 1);
  if InParams <> '' then
    InParams := '(' + InParams + ')';
  Result := 'call ' + ConSettings^.ConvFuncs.ZStringToRaw(SQL,
            ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP) + InParams;
end;
procedure TZASACallableStatement.ReleaseConnection;
begin
  inherited;
  FASAConnection := nil;
end;

{$ENDIF ZEOS_DISABLE_ASA}

end.


