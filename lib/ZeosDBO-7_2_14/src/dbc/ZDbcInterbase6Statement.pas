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

unit ZDbcInterbase6Statement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  {$IF defined (WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows, {$IFEND}
  ZDbcIntfs, ZDbcStatement, ZDbcInterbase6, ZDbcInterbase6Utils,
  ZPlainFirebirdInterbaseConstants, ZCompatibility,
  ZDbcLogging, ZVariant, ZMessages;

type
  {** Implements Prepared SQL Statement for Interbase or FireBird. }
  TZInterbase6PreparedStatement = class;

  TZIBStmt = record
    Obj: TZInterbase6PreparedStatement;
    PreparedRowsOfArray: Integer;
  end;

  { TZInterbase6PreparedStatement }
  TZInterbase6PreparedStatement = class(TZAbstractPreparedStatement)
  private
    FParamSQLData: IZParamsSQLDA;
    FResultXSQLDA: IZSQLDA;
    FIBConnection: IZInterbase6Connection;
    FCodePageArray: TWordDynArray;
    FStatusVector: TARRAY_ISC_STATUS;
    FStmtHandle: TISC_STMT_HANDLE;
    FStatementType: TZIbSqlStatementType;
    FTypeTokens: TRawByteStringDynArray;
    FBatchStmts: array[Boolean] of TZIBStmt;
    FMaxRowsPerBatch, FMemPerRow: Integer;
    function ExecuteInternal: Integer;
    function ExceuteBatch: Integer;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function CheckInterbase6Error(const Sql: RawByteString = '') : Integer;
    procedure ReleaseConnection; override;
  public
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings); overload;
    constructor Create(const Connection: IZConnection; Info: TStrings); overload;
    procedure AfterClose; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

  end;
  TZInterbase6Statement = class(TZInterbase6PreparedStatement);

  TZInterbase6CallableStatement = class(TZAbstractPreparedCallableStatement)
  private
    FParamSQLData: IZParamsSQLDA;
    FResultXSQLDA: IZSQLDA;
    FIBConnection: IZInterbase6Connection;
    FCodePageArray: TWordDynArray;
    FStatusVector: TARRAY_ISC_STATUS;
    FStmtHandle: TISC_STMT_HANDLE;
    FStatementType: TZIbSqlStatementType;
    function ExecuteInternal: Integer;
  protected
    procedure CheckInterbase6Error(const Sql: RawByteString = '');
    function GetProcedureSql(SelectProc: boolean): RawByteString;
    procedure ReleaseConnection; override;

    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    procedure AfterClose; override;

    procedure Prepare(SelectProc: Boolean); reintroduce;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses ZSysUtils, ZDbcUtils, ZFastCode, ZPlainFirebirdDriver,
  ZDbcInterbase6ResultSet, ZClasses, ZTokenizer, ZEncoding;

{ TZInterbase6PreparedStatement }

function TZInterbase6PreparedStatement.ExceuteBatch: Integer;
var
  AC: Boolean;
  ArrayOffSet: Integer;
begin
  AC := Connection.GetAutoCommit;
  if AC then
  Connection.SetAutoCommit(False);
  Result := 0;
  try
    ArrayOffSet := 0;
    FIBConnection.GetTrHandle; //restart transaction if required
    try
      if (FBatchStmts[True].Obj <> nil) and (ArrayCount >= FBatchStmts[True].PreparedRowsOfArray) then
        while (ArrayOffSet+FBatchStmts[True].PreparedRowsOfArray <= ArrayCount) do begin
          BindSQLDAInParameters(InParamValues, InParamCount,
            FBatchStmts[True].Obj.FParamSQLData, GetConnection.GetConSettings,
            FCodePageArray, ArrayOffSet, FBatchStmts[True].PreparedRowsOfArray);
          Result := FBatchStmts[True].Obj.ExecuteInternal;
          Inc(ArrayOffSet, FBatchStmts[True].PreparedRowsOfArray);
        end;
      if (FBatchStmts[False].Obj <> nil) and (ArrayOffSet < ArrayCount) then begin
        BindSQLDAInParameters(InParamValues, InParamCount,
          FBatchStmts[False].Obj.FParamSQLData, GetConnection.GetConSettings,
          FCodePageArray, ArrayOffSet, FBatchStmts[False].PreparedRowsOfArray);
        Result := FBatchStmts[False].Obj.ExecuteInternal;
      end;
      if AC then
      Connection.Commit;
    except
      if AC then
      Connection.Rollback;
      raise;
    end;
  finally
    Connection.SetAutoCommit(AC);
  end;
  LastUpdateCount := ArrayCount;
end;

function TZInterbase6PreparedStatement.ExecuteInternal: Integer;
begin
  if ArrayCount = 0 then
    With FIBConnection do begin
      case FStatementType of
        stSelect, stSelectForUpdate: //AVZ Get many rows - only need to use execute not execute2
          GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle, @FStmtHandle,
            GetDialect, FParamSQLData.GetData);
        stExecProc:
          GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @FStmtHandle,
            GetDialect, FParamSQLData.GetData, FResultXSQLDA.GetData); //expecting a result
        else
          GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle, @FStmtHandle,
            GetDialect, FParamSQLData.GetData) //not expecting a result
      end;
      Result := ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver,
        FStatusVector, ConSettings, lcExecute, ASQL);
      if Result <> DISCONNECT_ERROR then
        LastUpdateCount := GetAffectedRows(GetPlainDriver, FStmtHandle, FStatementType, ConSettings);
  end else
    Result := ExceuteBatch;
end;

procedure TZInterbase6PreparedStatement.PrepareInParameters;
var
  StatusVector: TARRAY_ISC_STATUS;
begin
  With FIBConnection do
  begin
    {create the parameter bind structure}
    FParamSQLData := TZParamsSQLDA.Create(Connection);
    if FParamSQLData.GetData^.sqln < InParamCount then begin
      FParamSQLData.GetData^.sqld := InParamCount;
      FParamSQLData.AllocateSQLDA;
    end;

    {check dynamic sql}
    GetPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect, FParamSQLData.GetData);
    ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, StatusVector, ConSettings, lcExecute, ASQL);

    { Resize XSQLDA structure if required }
    if FParamSQLData.GetData^.sqld > FParamSQLData.GetData^.sqln then
    begin
      FParamSQLData.AllocateSQLDA;
      GetPlainDriver.isc_dsql_describe_bind(@StatusVector, @FStmtHandle, GetDialect,FParamSQLData.GetData);
      ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, StatusVector, ConSettings, lcExecute, ASQL);
    end;
    FParamSQLData.InitFields(True);
  end;
end;

procedure TZInterbase6PreparedStatement.ReleaseConnection;
begin
  inherited;
  FIBConnection := nil;
end;

procedure TZInterbase6PreparedStatement.BindInParameters;
begin
  if ArrayCount = 0 then
    BindSQLDAInParameters(ClientVarManager, InParamValues,
      InParamTypes, InParamCount, FParamSQLData, GetConnection.GetConSettings,
        FCodePageArray);
  inherited BindInParameters;
end;

procedure TZInterbase6PreparedStatement.UnPrepareInParameters;
begin
  if assigned(FParamSQLData) then
    FParamSQLData.FreeParamtersValues;
end;

{**
   Check interbase error status
   @param Sql the used sql tring

   @return Integer - Error Code to test for graceful database disconnection
}
function TZInterbase6PreparedStatement.CheckInterbase6Error(const SQL: RawByteString) : Integer;
begin
  Result := ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, ConSettings, lcExecute, SQL);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6PreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FIBConnection := Connection as IZInterbase6Connection;
  FCodePageArray := (FIBConnection.GetIZPlainDriver as IZInterbasePlainDriver).GetCodePageArray;
  FCodePageArray[ConSettings^.ClientCodePage^.ID] := ConSettings^.ClientCodePage^.CP; //reset the cp if user wants to wite another encoding e.g. 'NONE' or DOS852 vc WIN1250
  ResultSetType := rtForwardOnly;
  FStmtHandle := 0;
  FMaxRowsPerBatch := 0;
end;

constructor TZInterbase6PreparedStatement.Create(const Connection: IZConnection;
  Info: TStrings);
begin
  Create(Connection,'', Info);
end;

procedure TZInterbase6PreparedStatement.AfterClose;
begin
  if (FStmtHandle <> 0) then begin// Free statement-handle! Otherwise: Exception!
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_drop);
    FStmtHandle := 0;
  end;
end;

procedure TZInterbase6PreparedStatement.Prepare;
var
  eBlock: RawByteString;
  PreparedRowsOfArray: Integer;

  procedure PrepareArrayStmt(var Slot: TZIBStmt);
  begin
    if (Slot.Obj = nil) or (Slot.PreparedRowsOfArray <> PreparedRowsOfArray) then begin
      if Slot.Obj <> nil then begin
        Slot.Obj.FInParamCount := 0;
        {$IFNDEF AUTOREFCOUNT}
        Slot.Obj._Release;
        {$ENDIF}
        Slot.Obj := nil;
      end;
      Slot.Obj := TZInterbase6PreparedStatement.Create(Connection, '', Info);
      {$IFNDEF AUTOREFCOUNT}
      Slot.Obj._AddRef;
      {$ENDIF}
      Slot.Obj.FASQL := eBlock;
      Slot.Obj.FInParamCount := InParamCount*PreparedRowsOfArray;
      Slot.PreparedRowsOfArray := PreparedRowsOfArray;
      Slot.Obj.Prepare;
    end;
  end;
  procedure PrepareFinalChunk(Rows: Integer);
  begin
    eBlock := GetExecuteBlockString(FParamSQLData,
      IsParamIndex, InParamCount, Rows, CachedQueryRaw,
      FIBConnection.GetPlainDriver, FMemPerRow, PreparedRowsOfArray, FMaxRowsPerBatch,
      FTypeTokens, FStatementType, FIBConnection.GetXSQLDAMaxSize);
    PrepareArrayStmt(FBatchStmts[False]);
  end;
begin
  if (not Prepared) then begin
    with FIBConnection do begin
      FStatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, ASQL, Connection, FStmtHandle); //allocate handle if required or reuse it
      if FStatementType in [stSelect, stExecProc, stSelectForUpdate] then
      begin
        FResultXSQLDA := TZSQLDA.Create(Connection);
        PrepareResultSqlData(GetPlainDriver, GetDialect,
          ASQL, FStmtHandle, FResultXSQLDA, ConSettings);
      end;
    end;
    inherited Prepare; //log action and prepare params
  end;
  if ArrayCount > 0 then begin
    if FMaxRowsPerBatch = 0 then begin
      eBlock := GetExecuteBlockString(FParamSQLData,
        IsParamIndex, InParamCount, ArrayCount, CachedQueryRaw,
        FIBConnection.GetPlainDriver, FMemPerRow, PreparedRowsOfArray, FMaxRowsPerBatch,
          FTypeTokens, FStatementType, FIBConnection.GetXSQLDAMaxSize);
    end else
       eBlock := '';
    if (FMaxRowsPerBatch <= ArrayCount) and (eBlock <> '') then begin
      PrepareArrayStmt(FBatchStmts[True]); //max block size per batch
      if ArrayCount > FMaxRowsPerBatch then //final block count
      PrepareFinalChunk(ArrayCount mod PreparedRowsOfArray);
    end else if (eBlock = '') then begin
      if (FMaxRowsPerBatch > ArrayCount) then begin
        if (FBatchStmts[False].PreparedRowsOfArray <> ArrayCount) then
          PrepareFinalChunk(ArrayCount) //full block of batch
      end else
        if (ArrayCount <> FMaxRowsPerBatch) and (ArrayCount mod FMaxRowsPerBatch > 0) and (FBatchStmts[False].PreparedRowsOfArray <> (ArrayCount mod FMaxRowsPerBatch)) then
          PrepareFinalChunk(ArrayCount mod FMaxRowsPerBatch); //final block of batch
    end else if (FBatchStmts[False].PreparedRowsOfArray <> ArrayCount) then
      PrepareArrayStmt(FBatchStmts[False]); //full block of batch
    end;
end;

procedure TZInterbase6PreparedStatement.Unprepare;
var b: Boolean;
begin
  for b := False to True do
    if FBatchStmts[b].Obj <> nil then begin
      FBatchStmts[b].Obj.FInParamCount := 0;
      {$IFNDEF AUTOREFCOUNT}
      FBatchStmts[b].Obj._Release;
      {$ENDIF}
      FBatchStmts[b].Obj := nil;
      end;
  FMaxRowsPerBatch := 0;
  FResultXSQLDA := nil;
  FParamSQLData := nil;
  SetLength(FTypeTokens, 0);
  inherited Unprepare;
  if (FStmtHandle <> 0) then //check if prepare did fail. otherwise we unprepare the handle
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_UNPREPARE); //unprepare avoids new allocation for the stmt handle
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZInterbase6PreparedStatement.ExecutePrepared: Boolean;
var iError: Integer;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  BindInParameters;
  iError := ExecuteInternal;

  case FStatementType of
    stInsert, stDelete, stUpdate, stSelectForUpdate:
      Result := False;
    else
      Result := True;
  end;

  { Create ResultSet if possible else free Statement Handle }
  if iError <> DISCONNECT_ERROR then
    if (FStatementType in [stSelect, stExecProc]) and (FResultXSQLDA.GetFieldCount <> 0) then
      if not Assigned(LastResultSet) then
        LastResultSet := CreateIBResultSet(SQL, Self,
          TZInterbase6XSQLDAResultSet.Create(Self, SQL, @FStmtHandle,
            FResultXSQLDA, CachedLob, FStatementType))
      else
    else
      LastResultSet := nil;
  inherited ExecutePrepared;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZInterbase6PreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  iError : Integer; //Check for database disconnect AVZ
begin
  Prepare;
  PrepareOpenResultSetForReUse;
  BindInParameters;
  iError := ExecuteInternal;

  if (iError <> DISCONNECT_ERROR) then begin
    if (FStatementType in [stSelect, stExecProc]) and ( FResultXSQLDA.GetFieldCount <> 0) then
      if Assigned(FOpenResultSet) then
        Result := IZResultSet(FOpenResultSet)
      else begin
        Result := CreateIBResultSet(SQL, Self,
          TZInterbase6XSQLDAResultSet.Create(Self, SQL, @FStmtHandle,
            FResultXSQLDA, CachedLob, FStatementType));
        FOpenResultSet := Pointer(Result);
      end
  end else begin
    Result := nil;
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);
  end;

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
function TZInterbase6PreparedStatement.ExecuteUpdatePrepared: Integer;
var
  iError : Integer; //Implementation for graceful disconnect AVZ
begin
  Prepare;
  BindInParameters;
  iError := ExecuteInternal;
  Result := LastUpdateCount;
  if ArrayCount = 0 then
    case FStatementType of
      stCommit, stRollback, stUnknown: Result := -1;
      stSelect: if (iError <> DISCONNECT_ERROR) then
        FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_CLOSE);  //AVZ
      stExecProc:
        { Create ResultSet if possible }
        if FResultXSQLDA.GetFieldCount <> 0 then
          LastResultSet := CreateIBResultSet(SQL, Self,
            TZInterbase6XSQLDAResultSet.Create(Self, SQL, @FStmtHandle,
              FResultXSQLDA, CachedLob, FStatementType));

    end;
  inherited ExecuteUpdatePrepared;
end;

function TZInterbase6PreparedStatement.GetRawEncodedSQL(
  const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var
  I: Integer;
  Temp: RawByteString;
  ParamFound, IsCS_NONE: Boolean;
  Tokens: TZTokenDynArray;
  P: PChar;

  procedure Add(const Value: RawByteString; const Param: Boolean = False);
  begin
    SetLength(FCachedQueryRaw, Length(FCachedQueryRaw)+1);
    FCachedQueryRaw[High(FCachedQueryRaw)] := Value;
    SetLength(FIsParamIndex, Length(FCachedQueryRaw));
    FIsParamIndex[High(FIsParamIndex)] := Param;
    ToBuff(Value, Result);
  end;
begin
  Result := '';
  {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := SQL;
  if FCachedQueryRaw = nil then begin
    ParamFound := (ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSE}Pos{$ENDIF}('?', SQL) > 0);
    IsCS_NONE := (ConSettings^.ClientCodePage^.ID = CS_NONE);
    if ParamFound or {$IFNDEF UNICODE}ConSettings^.AutoEncode or {$ENDIF}IsCS_NONE then begin
      Tokens := Connection.GetDriver.GetTokenizer.TokenizeBuffer(SQL, [toSkipEOF]);
      Temp := '';
      for I := 0 to High(Tokens) do begin
        if ParamFound and (Tokens[I].Value = '?') then begin
          Add(Temp);
          Add('?', True);
          Temp := '';
        end else case (Tokens[i].TokenType) of
          ttQuoted, ttComment,
          ttWord, ttQuotedIdentifier, ttKeyword: begin
            P := Pointer(Tokens[i].Value);
            if IsCS_NONE and ((Tokens[i].TokenType = ttQuotedIdentifier) or
               ((Tokens[i].TokenType = ttWord) and (P^ = '"')))
            then Temp := Temp + ZConvertStringToRawWithAutoEncode(Tokens[i].Value, ConSettings^.CTRL_CP, zCP_UTF8)
            else Temp := Temp + ConSettings^.ConvFuncs.ZStringToRaw(Tokens[i].Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)
          end else
            Temp := Temp + {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Tokens[i].Value);
        end;
      end;
      if (Temp <> '') then
        Add(Temp);
    end else
      Add(ConSettings^.ConvFuncs.ZStringToRaw(SQL, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP))
  end else
    for I := low(FCachedQueryRaw) to high(FCachedQueryRaw) do
      ToBuff(FCachedQueryRaw[i], Result);
  FlushBuff(Result);
end;

{ TZInterbase6CallableStatement }

function TZInterbase6CallableStatement.ExecuteInternal: Integer;
begin
  With FIBConnection do
  begin
    case FStatementType of
      stSelect: //AVZ Get many rows - only need to use execute not execute2
        GetPlainDriver.isc_dsql_execute(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData);
      stExecProc:
        GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData, FResultXSQLDA.GetData); //expecting a result
      else
        GetPlainDriver.isc_dsql_execute2(@FStatusVector, GetTrHandle, @FStmtHandle,
          GetDialect, FParamSQLData.GetData, nil) //not expecting a result
    end;
    Result := ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver,
      FStatusVector, ConSettings, lcExecute, FProcSQL);
  end;
end;

{**
   Check interbase error status
   @param Sql the used sql tring
}
procedure TZInterbase6CallableStatement.CheckInterbase6Error(const Sql: RawByteString);
begin
  ZDbcInterbase6Utils.CheckInterbase6Error(FIBConnection.GetPlainDriver,
    FStatusVector, ConSettings, lcExecute, Sql);
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Handle a connection handle pointer.
  @param Dialect a dialect Interbase SQL must be 1 or 2 or 3.
  @param Info a statement parameters.
}
constructor TZInterbase6CallableStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);

  FIBConnection := Connection as IZInterbase6Connection;
  FCodePageArray := (FIBConnection.GetIZPlainDriver as IZInterbasePlainDriver).GetCodePageArray;
  ResultSetType := rtScrollInsensitive;
  FStmtHandle := 0;
  FStatementType := stUnknown;
end;

procedure TZInterbase6CallableStatement.PrepareInParameters;
begin
  With FIBConnection do
  begin
    {create the parameter bind structure}
    FParamSQLData := TZParamsSQLDA.Create(Connection);
    {check dynamic sql}
    GetPlainDriver.isc_dsql_describe_bind(@FStatusVector, @FStmtHandle, GetDialect,
      FParamSQLData.GetData);
    ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, FStatusVector, ConSettings, lcExecute, ASQL);

    { Resize XSQLDA structure if needed }
    if FParamSQLData.GetData^.sqld > FParamSQLData.GetData^.sqln then
    begin
      FParamSQLData.AllocateSQLDA;
      GetPlainDriver.isc_dsql_describe_bind(@FStatusVector, @FStmtHandle, GetDialect,FParamSQLData.GetData);
      ZDbcInterbase6Utils.CheckInterbase6Error(GetPlainDriver, FStatusVector, ConSettings, lcExecute, ASQL);
    end;

    FParamSQLData.InitFields(True);
  end;
end;

procedure TZInterbase6CallableStatement.ReleaseConnection;
begin
  inherited;
  FIBConnection := nil;
end;

procedure TZInterbase6CallableStatement.BindInParameters;
begin
  TrimInParameters;
  BindSQLDAInParameters(ClientVarManager,
    InParamValues, InParamTypes, InParamCount, FParamSQLData, ConSettings, FCodePageArray);
  inherited BindInParameters;
end;

procedure TZInterbase6CallableStatement.UnPrepareInParameters;
begin
  if assigned(FParamSQLData) then
    FParamSQLData.FreeParamtersValues;
end;

procedure TZInterbase6CallableStatement.Prepare(SelectProc: Boolean);
const
  CallableStmtType: array[Boolean] of TZIbSqlStatementType = (stExecProc, stSelect);
begin
  if CallableStmtType[SelectProc] <> FStatementType then UnPrepare;
  if not Prepared then
  begin
    FProcSql := GetProcedureSql(Self.FIBConnection.StoredProcedureIsSelectable(SQL));
    with FIBConnection do
    begin
      FStatementType := ZDbcInterbase6Utils.PrepareStatement(GetPlainDriver,
        GetDBHandle, GetTrHandle, GetDialect, FProcSql, Connection, FStmtHandle); //allocate handle if required or reuse it

      if FStatementType in [stSelect, stExecProc] then
        begin
          FResultXSQLDA := TZSQLDA.Create(Connection);
          PrepareResultSqlData(GetPlainDriver, GetDialect,
            FProcSql, FStmtHandle, FResultXSQLDA, ConSettings);
        end;
    end;
    CheckInterbase6Error(FProcSql);
    inherited Prepare;
  end;
end;

procedure TZInterbase6CallableStatement.Unprepare;
begin
  inherited Unprepare;
  if FStmtHandle <> 0 then //check if prepare did fail. otherwise we unprepare the handle
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_UNPREPARE);
end;

procedure TZInterbase6CallableStatement.AfterClose;
begin
  inherited AfterClose;
  if FStmtHandle <> 0 then begin// Free statement-handle! On the other hand: Exception!
    FreeStatement(FIBConnection.GetPlainDriver, FStmtHandle, DSQL_DROP);
    FStmtHandle := 0;
  end;
  FResultXSQLDA := nil;
  FParamSQLData := nil;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZInterbase6CallableStatement.ExecutePrepared: Boolean;
var RS: IZResultSet;
begin
  Prepare(False);
  PrepareLastResultSetForReUse;
  PrepareOpenResultSetForReUse;
  with FIBConnection do begin
    BindInParameters;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
    ExecuteInternal;

    LastUpdateCount := GetAffectedRows(GetPlainDriver, FStmtHandle, FStatementType, ConSettings);
    Result := not (FStatementType in [stInsert, stDelete, stUpdate, stSelectForUpdate]);

    if (FStatementType in [stSelect, stExecProc]) and (FResultXSQLDA.GetFieldCount <> 0) then
      if not Assigned(LastResultSet) then
        LastResultSet := TZInterbase6XSQLDAResultSet.Create(Self, SQL,
          @FStmtHandle, FResultXSQLDA, CachedLob, FStatementType)
      else begin
        { Fetch data and fill Output params }
        LastResultSet := nil;
        if not Assigned(FOpenResultSet) then
        begin
          RS := TZInterbase6XSQLDAResultSet.Create(Self, SQL, @FStmtHandle,
            FResultXSQLDA, CachedLob, FStatementType);
          FOpenResultSet := Pointer(RS);
        end;
        AssignOutParamValuesFromResultSet(IZResultSet(FOpenResultSet),
            OutParamValues, OutParamCount , FDBParamTypes);
      end;
  end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZInterbase6CallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;
  Prepare(True);
  PrepareOpenResultSetForReUse;
  with FIBConnection do
  begin
    BindInParameters;

    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, FProcSql);
    ExecuteInternal;
    if (FStatementType in [stSelect, stExecProc]) and (FResultXSQLDA.GetFieldCount <> 0) then
      if Assigned(FOpenResultSet) then
        Result := IZResultSet(FOpenResultSet)
      else
      begin
        Result := TZInterbase6XSQLDAResultSet.Create(Self, Self.SQL,
          @FStmtHandle, FResultXSQLDA, CachedLob, FStatementType);
        FOpenResultSet := Pointer(Result);
      end;
  end;
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
function TZInterbase6CallableStatement.ExecuteUpdatePrepared: Integer;
var RS: IZResultSet;
begin
  Prepare(False);
  PrepareOpenResultSetForReUse;
  with FIBConnection do
  begin
    BindInParameters;

    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, FProcSql);
    ExecuteInternal;

    Result := GetAffectedRows(GetPlainDriver, FStmtHandle, FStatementType, ConSettings);
    LastUpdateCount := Result;
    { Fetch data and fill Output params }
    if not Assigned(FOpenResultSet) then
    begin
      RS := TZInterbase6XSQLDAResultSet.Create(Self, SQL, @FStmtHandle,
        FResultXSQLDA, CachedLob, FStatementType);
      FOpenResultSet := Pointer(RS);
    end;
    AssignOutParamValuesFromResultSet(IZResultSet(FOpenResultSet), OutParamValues, OutParamCount , FDBParamTypes);
  end;
end;

{**
   Create sql string for calling stored procedure.
   @param SelectProc indicate use <b>EXECUTE PROCEDURE</b> or
    <b>SELECT</b> staement
   @return a Stored Procedure SQL string
}
function TZInterbase6CallableStatement.GetProcedureSql(SelectProc: boolean): RawByteString;

  function GenerateParamsStr(Count: integer): RawByteString;
  var
    I: integer;
  begin
    Result := '';
    for I := 0 to Count - 1 do
    begin
      if I > 0 then
        Result := Result + ',';
      Result := Result + '?';
    end;
  end;

var
  InParams: RawByteString;
begin
  //TrimInParameters;
  InParams := GenerateParamsStr(Length(InParamValues));
  if InParams <> '' then
    InParams := '(' + InParams + ')';

  if SelectProc then
    Result := 'SELECT * FROM ' + ASQL + InParams
  else
    Result := 'EXECUTE PROCEDURE ' + ASQL + InParams;
end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.
