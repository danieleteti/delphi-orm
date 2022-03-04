{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSqlStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZDbcIntfs, ZDbcStatement, ZDbcLogging, ZPlainPostgreSqlDriver,
  ZCompatibility, ZVariant, ZDbcGenericResolver, ZDbcCachedResultSet,
  ZDbcPostgreSql, ZDbcUtils;

type
  TEICategory = (eicExecute, eicPrepStmt, eicExecPrepStmt, eicUnprepStmt);

  { TZPostgreSQLPreparedStatement }

  {** PostgreSQL Prepared SQL statement interface. }
  IZPGSQLPreparedStatement = interface(IZPreparedStatement)
    ['{EED35CAA-8F36-4639-8B67-32DF237E8F6F}']
    function GetLastQueryHandle: PZPostgreSQLResult;
  end;

  TZPostgreSQLPreparedStatement = class(TZAbstractPreparedStatement,
    IZPGSQLPreparedStatement)
  private
    FRawPlanName: RawByteString;
    FPostgreSQLConnection: IZPostgreSQLConnection;
    FPlainDriver: IZPostgreSQLPlainDriver;
    QueryHandle: PZPostgreSQLResult;
    FOidAsBlob: Boolean;
    FConnectionHandle: PZPostgreSQLConnect;
    Findeterminate_datatype: Boolean;
    FUseEmulatedStmtsOnly: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    fPrepareCnt: Cardinal;
  protected
    function CreateResultSet(QueryHandle: PZPostgreSQLResult): IZResultSet;
    function ExecuteInternal(const SQL: RawByteString;
      const Category: TEICategory): PZPostgreSQLResult; virtual; abstract;
    function PrepareAnsiSQLQuery: RawByteString;
    function GetDeallocateSQL: RawByteString; virtual; abstract;
    function GetPrepareSQLPrefix: RawByteString; virtual; abstract;
    function GetCompareFirstKeywordStrings: TPreparablePrefixTokens; override;
  public
    constructor Create(const PlainDriver: IZPostgreSQLPlainDriver;
      const Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings); overload;
    constructor Create(const PlainDriver: IZPostgreSQLPlainDriver;
      const Connection: IZPostgreSQLConnection; Info: TStrings); overload;
    procedure AfterConstruction; override;
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
    function GetLastQueryHandle: PZPostgreSQLResult;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure Prepare; override;
    procedure Unprepare; override;
  end;

  {** EgonHugeist: Implements Prepared SQL Statement with AnsiString usage }
  TZPostgreSQLClassicPreparedStatement = class(TZPostgreSQLPreparedStatement)
  private
    FExecSQL: RawByteString;
    function GetAnsiSQLQuery: RawByteString;
  protected
    function ExecuteInternal(const SQL: RawByteString;
      const Category: TEICategory): PZPostgreSQLResult; override;
    function PrepareAnsiSQLParam(ParamIndex: Integer; Escaped: Boolean): RawByteString;
    procedure BindInParameters; override;
    function GetDeallocateSQL: RawByteString; override;
    function GetPrepareSQLPrefix: RawByteString; override;
  end;

  {** EgonHugeist: Implements Prepared SQL Statement based on Protocol3
    ServerVersion 7.4Up and ClientVersion 8.0Up. with C++API usage}
  TZPostgreSQLCAPIPreparedStatement = class(TZPostgreSQLPreparedStatement)
  private
    FPQparamValues: TPQparamValues;
    FPQparamLengths: TPQparamLengths;
    FPQparamFormats: TPQparamFormats;
    FPRawPlanName: PAnsiChar;
  protected
    function ExecuteInternal(const SQL: RawByteString;
      const Category: TEICategory): PZPostgreSQLResult; override;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure UnPrepareInParameters; override;
    function GetDeallocateSQL: RawByteString; override;
    function GetPrepareSQLPrefix: RawByteString; override;
  end;

  {** Implements Standard Postgresql Statement.
      Only for compatibility with old dbc-based code
      Uses the 'Classic' prepared statement to be sure it still works with old postgres servers
  }
  TZPostgreSQLStatement = class(TZPostgreSQLClassicPreparedStatement);

  {** Implements callable Postgresql Statement. }
  TZPostgreSQLCallableStatement = class(TZAbstractCallableStatement)
  private
    FOidAsBlob: Boolean;
    FPlainDriver: IZPostgreSQLPlainDriver;
    FUndefinedVarcharAsStringLength: Integer;
    function GetProcedureSql: string;
    function FillParams(const ASql: String): RawByteString;
    function PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
  protected
    function GetConnectionHandle:PZPostgreSQLConnect;
    function GetPlainDriver:IZPostgreSQLPlainDriver;
    function CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
    procedure TrimInParameters; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
  end;

  {** Implements a specialized cached resolver for PostgreSQL. }
  TZPostgreSQLCachedResolver = class(TZGenericCachedResolver)
  protected
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; override;
  end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZSysUtils, ZFastCode, ZMessages, ZDbcPostgreSqlResultSet, ZDbcPostgreSqlUtils,
  ZEncoding, ZTokenizer, ZDbcResultSet, ZClasses
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND}
  {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};

var PGPreparableTokens: TPreparablePrefixTokens;

{ TZPostgreSQLPreparedStatement }

{**
  Creates a result set based on the current settings.
  @param QueryHandle the Postgres query handle
  @return a created result set object.
}
constructor TZPostgreSQLPreparedStatement.Create(const PlainDriver: IZPostgreSQLPlainDriver;
  const Connection: IZPostgreSQLConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FOidAsBlob := StrToBoolDef(Self.Info.Values['oidasblob'], False) or
    (Connection as IZPostgreSQLConnection).IsOidAsBlob;
  FPostgreSQLConnection := Connection;
  FPlainDriver := PlainDriver;
  //ResultSetType := rtScrollInsensitive;
  FConnectionHandle := Connection.GetConnectionHandle;
  Findeterminate_datatype := False;
  FUndefinedVarcharAsStringLength := StrToInt(ZDbcUtils.DefineStatementParameter(Self, 'Undefined_Varchar_AsString_Length' , '0'));
  { see http://zeoslib.sourceforge.net/viewtopic.php?f=20&t=10695&p=30151#p30151
    the pgBouncer does not support the RealPrepareds.... }
  FUseEmulatedStmtsOnly := StrToBoolEx(ZDbcUtils.DefineStatementParameter(Self, 'EMULATE_PREPARES', 'FALSE'));
end;

procedure TZPostgreSQLPreparedStatement.AfterConstruction;
begin
  inherited;
  fPrepareCnt := 0;
end;

constructor TZPostgreSQLPreparedStatement.Create(const PlainDriver: IZPostgreSQLPlainDriver;
  const Connection: IZPostgreSQLConnection; Info: TStrings);
begin
  Create(PlainDriver, Connection, SQL, Info);
end;

function TZPostgreSQLPreparedStatement.GetLastQueryHandle: PZPostgreSQLResult;
begin
  Result := QueryHandle;
end;

function TZPostgreSQLPreparedStatement.GetRawEncodedSQL(
  const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var
  I, C, N: Integer;
  Temp: RawByteString;
  Tokens: TZTokenDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens;
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
  if (Length(FCachedQueryRaw) = 0) and (SQL <> '') then begin
    {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := SQL;
    if ((ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSE}Pos{$ENDIF}('?', SQL) > 0) or
        (ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSE}Pos{$ENDIF}('$', SQL) > 0)) then begin
      Tokens := Connection.GetDriver.GetTokenizer.TokenizeBuffer(SQL, [toSkipEOF]);
      ComparePrefixTokens := PGPreparableTokens;
      Temp := '';
      N := -1;
      FIsPraparable := False;
      for I := 0 to High(Tokens) do begin
        {check if we've a preparable statement. If ComparePrefixTokens = nil then
          comparing is not required or already done }
        if Assigned(ComparePrefixTokens) and (Tokens[I].TokenType = ttWord) then
          if N = -1 then begin
            for C := 0 to high(ComparePrefixTokens) do
              if ComparePrefixTokens[C].MatchingGroup = UpperCase(Tokens[I].Value) then begin
                if Length(ComparePrefixTokens[C].ChildMatches) = 0 then begin
                  FIsPraparable := True;
                  ComparePrefixTokens := nil;
                end else
                  N := C; //save group
                Break;
              end;
            if N = -1 then //no sub-tokens ?
              ComparePrefixTokens := nil; //stop compare sequence
          end else begin //we already got a group
            FIsPraparable := False;
            for C := 0 to high(ComparePrefixTokens[N].ChildMatches) do
              if ComparePrefixTokens[N].ChildMatches[C] = UpperCase(Tokens[I].Value) then begin
                FIsPraparable := True;
                Break;
              end;
            ComparePrefixTokens := nil; //stop compare sequence
          end;
        P := Pointer(Tokens[I].Value);
        if (P^ = '?') or ((Tokens[I].TokenType = ttWord) and (P^ = '$') and
           ({$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(P+1, -1) <> -1)) then begin
          Add(Temp);
          Add({$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Tokens[I].Value), True);
          Temp := '';
        end else case (Tokens[i].TokenType) of
          ttQuoted, ttComment,
          ttWord, ttQuotedIdentifier, ttKeyword:
            Temp := Temp + ConSettings^.ConvFuncs.ZStringToRaw(Tokens[i].Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)
          else
            Temp := Temp + {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Tokens[i].Value);
        end;
      end;
      if (Temp <> '') then
        Add(Temp);
    end else
      Add(ConSettings^.ConvFuncs.ZStringToRaw(SQL, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
    FlushBuff(Result);
  end else
    Result := ASQL;
end;

function TZPostgreSQLPreparedStatement.CreateResultSet(
  QueryHandle: PZPostgreSQLResult): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZPostgreSQLResultSet.Create(FPlainDriver, Self, Self.SQL,
  FConnectionHandle, QueryHandle, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength);

  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, Self.SQL, nil,
      ConSettings);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
  if Result <> nil then
    FOpenResultSet := Pointer(Result);
end;

{**
  Prepares an SQL statement and inserts all data values.
  @return a RawByteString SQL statement.
}
function TZPostgreSQLPreparedStatement.PrepareAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do
    if IsParamIndex[I] then begin
      if ParamIndex > InParamCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} then
        raise EZSQLException.Create(SInvalidInputParameterCount)
      else
        ToBuff(PGPrepareAnsiSQLParam(InParamValues[ParamIndex],
        ClientVarManager, FPostgreSQLConnection, ChunkSize,
          InParamTypes[ParamIndex], FOidAsBlob, True, False, ConSettings), Result);
      Inc(ParamIndex);
    end else
      ToBuff(CachedQueryRaw[i], Result);
  FlushBuff(Result);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZPostgreSQLPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  Status: TZPostgreSQLExecStatusType;
begin
  Result := nil;
  Prepare;
  PrepareOpenResultSetForReUse;
  if Prepared  then
    if Findeterminate_datatype then
      QueryHandle := ExecuteInternal(PrepareAnsiSQLQuery, eicExecute)
    else
    begin
      BindInParameters;
      QueryHandle := ExecuteInternal(ASQL, eicExecPrepStmt);
    end
  else
    QueryHandle := ExecuteInternal(ASQL, eicExecute);
  Status := FPlainDriver.PQresultStatus(QueryHandle);
  if (QueryHandle <> nil) and (Status = PGRES_TUPLES_OK) then
    if Assigned(FOpenResultSet) then
      Result := IZResultSet(FOpenResultSet)
    else
      Result := CreateResultSet(QueryHandle)
  else
    Result := nil;
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
function TZPostgreSQLPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  Prepare;

  if Prepared  then
    if Findeterminate_datatype then
      QueryHandle := ExecuteInternal(PrepareAnsiSQLQuery, eicExecute)
    else
    begin
      BindInParameters;
      QueryHandle := ExecuteInternal(ASQL, eicExecPrepStmt);
    end
  else
    QueryHandle := ExecuteInternal(ASQL, eicExecute);

  if QueryHandle <> nil then
  begin
    Result := RawToIntDef(FPlainDriver.GetCommandTuples(QueryHandle), 0);
    LastUpdateCount := Result;
    FPlainDriver.PQclear(QueryHandle);
  end;

  inherited ExecuteUpdatePrepared;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZPostgreSQLPreparedStatement.ExecutePrepared: Boolean;
var
  ResultStatus: TZPostgreSQLExecStatusType;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  if Prepared  then
    if Findeterminate_datatype then
      QueryHandle := ExecuteInternal(PrepareAnsiSQLQuery, eicExecute)
    else
    begin
      BindInParameters;
      QueryHandle := ExecuteInternal(ASQL, eicExecPrepStmt);
    end
  else
    QueryHandle := ExecuteInternal(ASQL, eicExecute);

  { Process queries with result sets }
  ResultStatus := FPlainDriver.PQresultStatus(QueryHandle);
  case ResultStatus of
    PGRES_TUPLES_OK:
      begin
        Result := True;
        if not Assigned(LastResultSet) then
          LastResultSet := CreateResultSet(QueryHandle);
      end;
    PGRES_COMMAND_OK:
      begin
        Result := False;
        LastUpdateCount := RawToIntDef(
          FPlainDriver.GetCommandTuples(QueryHandle), 0);
        FPlainDriver.PQclear(QueryHandle);
      end;
    else
      begin
        Result := False;
        LastUpdateCount := RawToIntDef(
          FPlainDriver.GetCommandTuples(QueryHandle), 0);
        FPlainDriver.PQclear(QueryHandle);
      end;
  end;

  inherited ExecutePrepared;
end;

procedure TZPostgreSQLPreparedStatement.Prepare;
var
  TempSQL: RawByteString;
  N, I: Integer;
begin
  if not Prepared then
  begin
    Inc(fPrepareCnt);
    FRawPlanName := IntToRaw(FStatementId)+IntToRaw({%H-}NativeUInt(FConnectionHandle))+IntToRaw(fPrepareCnt);
    TempSQL := GetPrepareSQLPrefix;
    N := 0;
    for I := 0 to High(CachedQueryRaw) do
      if IsParamIndex[i] then
      begin
        Inc(N);
        TempSQL := TempSQL + '$' + IntToRaw(N);
      end else
        TempSQL := TempSQL + CachedQueryRaw[i];

    if (not FUseEmulatedStmtsOnly) and IsPreparable then //detected after tokenizing the query
      QueryHandle := ExecuteInternal(TempSQL, eicPrepStmt)
    else
      Findeterminate_datatype := True;
    inherited Prepare; //we need this step always for Set(A/W)SQL overloads if SQL changes
  end;
end;

function TZPostgreSQLPreparedStatement.GetCompareFirstKeywordStrings: TPreparablePrefixTokens;
begin
{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
  Result := PGPreparableTokens;
end;

procedure TZPostgreSQLPreparedStatement.Unprepare;
begin
  if Prepared and Assigned(FPostgreSQLConnection.GetConnectionHandle) then
  begin
    if not Findeterminate_datatype
    then FPostgreSQLConnection.UnregisterPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName));
  end;
  inherited Unprepare;
end;

{ TZPostgreSQLClassicPreparedStatement }

function TZPostgreSQLClassicPreparedStatement.GetAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  for I := 0 to High(CachedQueryRaw) do
    if IsParamIndex[I] then
    begin
      Result := Result + PrepareAnsiSQLParam(ParamIndex, True);
      Inc(ParamIndex);
    end
    else
      Result := Result + CachedQueryRaw[i];
end;

function TZPostgreSQLClassicPreparedStatement.ExecuteInternal(const SQL: RawByteString;
  const Category: TEICategory): PZPostgreSQLResult;
begin
  case Category of
    eicPrepStmt:
      begin
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, Pointer(SQL));
        try
          Findeterminate_datatype := (CheckPostgreSQLError(Connection, FPlainDriver,
            FConnectionHandle, lcPrepStmt, ASQL, Result) = '42P18');
        except
          Unprepare; //free cached query tokens
          raise;     // handle exception
        end;
        if not Findeterminate_datatype then
        begin
          FPlainDriver.PQclear(Result);
          FPostgreSQLConnection.RegisterPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName));
        end;
      end;
    eicExecPrepStmt:
      begin
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, Pointer(FExecSQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcUnprepStmt, ASQL, Result);
      end;
    eicUnprepStmt:
      if Assigned(FConnectionHandle) then
      begin
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, Pointer(SQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcUnprepStmt, ASQL, Result);
      end
      else Result := nil;
    else
      begin
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, Pointer(SQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecute, ASQL, Result);
      end;
  end;
end;

function TZPostgreSQLClassicPreparedStatement.PrepareAnsiSQLParam(ParamIndex: Integer;
  Escaped: Boolean): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex], ClientVarManager,
    (Connection as IZPostgreSQLConnection), ChunkSize, InParamTypes[ParamIndex],
      FOidAsBlob, Escaped, True, ConSettings);
end;

{**
  Binds the input parameters
}
procedure TZPostgreSQLClassicPreparedStatement.BindInParameters;
var
  I: Integer;
begin
  if InParamCount > 0 then
    if Prepared then
    begin
      FExecSQL := 'EXECUTE "'+FRawPlanName+'"(';
      for i := 0 to InParamCount -1 do
        if I = 0 then
          FExecSQL := FExecSQL+PrepareAnsiSQLParam(i, False)
        else
          FExecSQL := FExecSQL+','+PrepareAnsiSQLParam(i, False);
      FExecSQL := FExecSQL+');';
    end
    else
      FExecSQL := GetAnsiSQLQuery
  else
    FExecSQL := ASQL;
  inherited BindInParameters;
end;

function TZPostgreSQLClassicPreparedStatement.GetDeallocateSQL: RawByteString;
begin
  Result := 'DEALLOCATE "'+FRawPlanName+'"';
end;

function TZPostgreSQLClassicPreparedStatement.GetPrepareSQLPrefix: RawByteString;
begin
  Result := 'PREPARE "'+FRawPlanName+'" AS ';
end;

{ TZPostgreSQLCAPIPreparedStatement }

function TZPostgreSQLCAPIPreparedStatement.ExecuteInternal(const SQL: RawByteString;
  const Category: TEICategory): PZPostgreSQLResult;
begin
  case Category of
    eicPrepStmt:
      begin
        Result := FPlainDriver.Prepare(FConnectionHandle, FPRawPlanName,
          Pointer(SQL), InParamCount, nil);
        try
          Findeterminate_datatype := (CheckPostgreSQLError(Connection, FPlainDriver,
            FConnectionHandle, lcPrepStmt, ASQL, Result) = '42P18');
        except
          Unprepare; //free cached query tokens
          raise;     // handle exception
        end;
        if not Findeterminate_datatype then
        begin
          FPostgreSQLConnection.RegisterPreparedStmtName({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FRawPlanName));
          FPlainDriver.PQclear(Result);
        end;
      end;
    eicExecPrepStmt:
      begin
        Result := FPlainDriver.ExecPrepared(FConnectionHandle,
          FPRawPlanName, InParamCount, FPQparamValues,
          FPQparamLengths, FPQparamFormats, 0);
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecPrepStmt, ASQL, Result);
      end;
    eicUnprepStmt:
      if Assigned(FConnectionHandle) then
        begin
          Result := FPlainDriver.ExecuteQuery(FConnectionHandle, Pointer(SQL));
          CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
            lcUnprepStmt, ASQL, Result);
        end
      else Result := nil;
    else
      begin
        Result := FPlainDriver.ExecuteQuery(FConnectionHandle, Pointer(SQL));
        CheckPostgreSQLError(Connection, FPlainDriver, FConnectionHandle,
          lcExecute, ASQL, Result);
      end;
  end;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZPostgreSQLCAPIPreparedStatement.PrepareInParameters;
begin
  if not (Findeterminate_datatype) then
  begin
    SetLength(FPQparamValues, InParamCount);
    SetLength(FPQparamLengths, InParamCount);
    SetLength(FPQparamFormats, InParamCount);
  end;
end;

{**
  Binds the input parameters
}
procedure TZPostgreSQLCAPIPreparedStatement.BindInParameters;
var
  TempBlob: IZBlob;
  WriteTempBlob: IZPostgreSQLOidBlob;
  ParamIndex: Integer;

  procedure UpdateNull(const Index: Integer);
  begin
    FPQparamValues[Index] := nil;
    FPQparamLengths[Index] := 0;
    FPQparamFormats[Index] := 0;
  end;

  procedure UpdatePAnsiChar(const Value: PAnsiChar; Const Index: Integer);
  begin
    UpdateNull(Index);
    FPQparamValues[Index] := Value;
    {EH: sade.., PG ignores Length settings for string even if it could speed up
      the speed by having a known size instead of checking for #0 terminator}
  end;

  procedure UpdateBinary(Value: Pointer; const Len, Index: Integer);
  begin
    UpdateNull(Index);

    FPQparamValues[Index] := Value;
    FPQparamLengths[Index] := Len;
    FPQparamFormats[Index] := 1;
  end;

begin
  if InParamCount <> Length(FPQparamValues) then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  for ParamIndex := 0 to InParamCount -1 do
  begin
    if ClientVarManager.IsNull(InParamValues[ParamIndex])  then
      UpdateNull(ParamIndex)
    else
      {EH: Nice advanteges of the TZVariant:
        a string(w.Type ever) needs to be localized. So i simply reuse this
        values as vars and have a constant pointer ((: }
      case InParamTypes[ParamIndex] of
        stBoolean,
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
        stFloat, stDouble, stCurrency, stBigDecimal,
        stString, stUnicodeString:
          UpdatePAnsiChar(ClientVarManager.GetAsCharRec(InParamValues[ParamIndex], ConSettings^.ClientCodePage^.CP).P, ParamIndex);
        stBytes:
          begin
            InParamValues[ParamIndex].VBytes := ClientVarManager.GetAsBytes(InParamValues[ParamIndex]);
            UpdateBinary(Pointer(InParamValues[ParamIndex].VBytes), Length(InParamValues[ParamIndex].VBytes), ParamIndex);
          end;
        stDate:
          begin
            InParamValues[ParamIndex].VRawByteString := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(InParamValues[ParamIndex]),
              ConSettings^.WriteFormatSettings, False);
            UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
          end;
        stTime:
          begin
            InParamValues[ParamIndex].VRawByteString := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(InParamValues[ParamIndex]),
              ConSettings^.WriteFormatSettings, False);
            UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
          end;
        stTimestamp:
          begin
            InParamValues[ParamIndex].VRawByteString := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(InParamValues[ParamIndex]),
              ConSettings^.WriteFormatSettings, False);
            UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
          end;
        stAsciiStream, stUnicodeStream, stBinaryStream:
          begin
            TempBlob := ClientVarManager.GetAsInterface(InParamValues[ParamIndex]) as IZBlob;
            if TempBlob.IsEmpty then
              UpdateNull(ParamIndex)
            else
              case InParamTypes[ParamIndex] of
                stBinaryStream:
                  if FOidAsBlob then
                  begin
                    try
                      WriteTempBlob := TZPostgreSQLOidBlob.Create(FPlainDriver, nil, 0,
                        FConnectionHandle, 0, ChunkSize);
                      WriteTempBlob.WriteBuffer(TempBlob.GetBuffer, TempBlob.Length);
                      InParamValues[ParamIndex].VRawByteString := IntToRaw(WriteTempBlob.GetBlobOid);
                      UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
                    finally
                      WriteTempBlob := nil;
                    end;
                  end
                  else
                    UpdateBinary(TempBlob.GetBuffer, TempBlob.Length, ParamIndex);
                stAsciiStream, stUnicodeStream:
                  if TempBlob.IsClob then
                    UpdatePAnsiChar(TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP), ParamIndex)
                  else
                  begin
                    InParamValues[ParamIndex].VRawByteString := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                      TempBlob.Length, ConSettings);
                    UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
                  end;
              end; {case..}
          end;
        stGuid: begin
            if InParamValues[ParamIndex].VType = vtBytes
            then InParamValues[ParamIndex].VRawByteString := GUIDToRaw(InParamValues[ParamIndex].VBytes)
            else InParamValues[ParamIndex].VRawByteString := ClientVarManager.GetAsRawByteString(InParamValues[ParamIndex]);
            UpdatePAnsiChar(PAnsiChar(InParamValues[ParamIndex].VRawByteString), ParamIndex);
          end;
        else
          RaiseUnsupportedParameterTypeException(InParamTypes[ParamIndex]);
      end;
  end;
  inherited BindInParameters;
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZPostgreSQLCAPIPreparedStatement.UnPrepareInParameters;
begin
  { release allocated memory }
  if not (Findeterminate_datatype) then
  begin
    SetLength(FPQparamValues, 0);
    SetLength(FPQparamLengths, 0);
    SetLength(FPQparamFormats, 0);
  end;
end;

function TZPostgreSQLCAPIPreparedStatement.GetDeallocateSQL: RawByteString;
begin
  Result := 'DEALLOCATE "'+FRawPlanName+'";';
end;

function TZPostgreSQLCAPIPreparedStatement.GetPrepareSQLPrefix: RawByteString;
begin
  Result := '';
  FPRawPlanName := Pointer(FRawPlanName);
end;

{ TZPostgreSQLCallableStatement }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
  @param Handle a connection handle pointer.
}
constructor TZPostgreSQLCallableStatement.Create(
  const Connection: IZConnection; const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  ResultSetType := rtScrollInsensitive;
  FPlainDriver := (Connection as IZPostgreSQLConnection).GetPlainDriver;
  FOidAsBlob := StrToBoolDef(Self.Info.Values['oidasblob'], False) or
    (Connection as IZPostgreSQLConnection).IsOidAsBlob;
  FUndefinedVarcharAsStringLength := StrToInt(ZDbcUtils.DefineStatementParameter(Self, 'Undefined_Varchar_AsString_Length' , '0'));
end;

{**
  Provides connection handle from the associated IConnection
  @return a PostgreSQL connection handle.
}
function TZPostgreSQLCallableStatement.GetConnectionHandle:PZPostgreSQLConnect;
begin
  if Self.Connection = nil then
    Result := nil
  else
    Result := (self.Connection as IZPostgreSQLConnection).GetConnectionHandle;
end;

{**
  Creates a result set based on the current settings.
  @return a created result set object.
}
function TZPostgreSQLCallableStatement.CreateResultSet(const SQL: string;
      QueryHandle: PZPostgreSQLResult): IZResultSet;
var
  NativeResultSet: TZPostgreSQLResultSet;
  CachedResultSet: TZCachedResultSet;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  ConnectionHandle := GetConnectionHandle();
  NativeResultSet := TZPostgreSQLResultSet.Create(GetPlainDriver, Self, SQL,
    ConnectionHandle, QueryHandle, CachedLob, ChunkSize, FUndefinedVarcharAsStringLength);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if GetResultSetConcurrency = rcUpdatable then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, SQL, nil,
      ConSettings);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZPostgreSQLCachedResolver.Create(
      Self,  NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
   Returns plain draiver from connection object
   @return a PlainDriver object
}
function TZPostgreSQLCallableStatement.GetPlainDriver():IZPostgreSQLPlainDriver;
begin
  if self.Connection <> nil then
    Result := (self.Connection as IZPostgreSQLConnection).GetPlainDriver
  else
    Result := nil;
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function TZPostgreSQLCallableStatement.PrepareAnsiSQLParam(
  ParamIndex: Integer): RawByteString;
begin
  if InParamCount <= ParamIndex then
    raise EZSQLException.Create(SInvalidInputParameterCount);

  Result := PGPrepareAnsiSQLParam(InParamValues[ParamIndex], ClientVarManager,
    (Connection as IZPostgreSQLConnection), ChunkSize, InParamTypes[ParamIndex],
      FOidAsBlob, True, False, ConSettings);
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLCallableStatement.ExecuteQuery(
  const SQL: RawByteString): IZResultSet;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := nil;
  ConnectionHandle := GetConnectionHandle();
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := GetPlainDriver.ExecuteQuery(ConnectionHandle,
    PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, GetPlainDriver, ConnectionHandle, lcExecute,
    ASQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  if QueryHandle <> nil then
  begin
    Result := CreateResultSet(Self.SQL, QueryHandle);
    AssignOutParamValuesFromResultSet(Result, OutParamValues, OutParamCount , FDBParamTypes);
  end
  else
    Result := nil;
end;

{**
  Prepares and executes an SQL statement that returns a single <code>ResultSet</code> object.
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZPostgreSQLCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  TrimInParameters;
  Result := ExecuteQuery(FillParams(GetProcedureSql));
end;

{**
   Create sql string for calling stored procedure.
   @return a Stored Procedure SQL string
}
function TZPostgreSQLCallableStatement.GetProcedureSql: string;
  function GenerateParamsStr(Count: integer): string;
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
  InParams: string;
begin
  if Length(CachedQueryRaw) <= 1 then  begin//only name in there?
    Unprepare; //reset cached query
    InParams := GenerateParamsStr(InParamCount);
    if (Connection as IZPostgreSQLConnection).StoredProcedureIsSelectable(SQL)
    then Result := Format('SELECT * FROM %s(%s)', [SQL, InParams])
    else Result := Format('CALL %s(%s)', [SQL, InParams]);
    {$IFDEF UNICODE}WSQL{$ELSE}ASQL{$ENDIF} := Result; //sets the cached queries again
  end;
end;

{**
   Fills the parameter (?) tokens with corresponding parameter value
   @return a prepared SQL query for execution
}
function TZPostgreSQLCallableStatement.FillParams(const ASql: String): RawByteString;
var I: Integer;
  ParamIndex: Integer;
begin
  if InParamCount > 0 then
  begin
    Result := '';
    ParamIndex := 0;
    for I := 0 to High(CachedQueryRaw) do
      if IsParamIndex[i] then
      begin
        Result := Result + PrepareAnsiSQLParam(ParamIndex);
        Inc(ParamIndex);
      end
      else
        Result := Result + CachedQueryRaw[i];
  end
  else
    Result := GetRawEncodedSQL(ASql);
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
function TZPostgreSQLCallableStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
var
  QueryHandle: PZPostgreSQLResult;
  ConnectionHandle: PZPostgreSQLConnect;
begin
  Result := -1;
  ConnectionHandle := GetConnectionHandle();
  ASQL := SQL; //Preprepares the SQL and Sets the AnsiSQL
  QueryHandle := GetPlainDriver.ExecuteQuery(ConnectionHandle,
    PAnsiChar(ASQL));
  CheckPostgreSQLError(Connection, GetPlainDriver, ConnectionHandle, lcExecute,
    ASQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);

  if QueryHandle <> nil then
  begin
    Result := RawToIntDef(GetPlainDriver.GetCommandTuples(QueryHandle), 0);
    AssignOutParamValuesFromResultSet(CreateResultSet(Self.SQL, QueryHandle),
      OutParamValues, OutParamCount , FDBParamTypes);
  end;
end;


function TZPostgreSQLCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  TrimInParameters;
  Result := Self.ExecuteUpdate(FillParams(GetProcedureSql));
end;

{**
   Function removes ptResult, ptOutput parameters from
   InParamTypes and InParamValues
}
procedure TZPostgreSQLCallableStatement.TrimInParameters;
var
  I: integer;
  ParamValues: TZVariantDynArray;
  ParamTypes: TZSQLTypeArray;
  ParamCount: Integer;
begin
  ParamCount := 0;
  SetLength(ParamValues, InParamCount);
  SetLength(ParamTypes, InParamCount);

  for I := 0 to High(InParamTypes) do
  begin
    if (Self.FDBParamTypes[i] in [pctOut, pctReturn]) then
      Continue;
    ParamTypes[ParamCount] := InParamTypes[I];
    ParamValues[ParamCount] := InParamValues[I];
    Inc(ParamCount);
  end;

  if ParamCount = InParamCount then
    Exit;

  InParamTypes := ParamTypes;
  InParamValues := ParamValues;
  SetInParamCount(ParamCount);
end;

{ TZPostgreSQLCachedResolver }

{**
  Checks is the specified column can be used in where clause.
  @param ColumnIndex an index of the column.
  @returns <code>true</code> if column can be included into where clause.
}
function TZPostgreSQLCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex)
    in [stUnknown, stBinaryStream]);
end;

initialization

{ RealPrepared stmts:
  http://www.postgresql.org/docs/9.1/static/sql-prepare.html }
SetLength(PGPreparableTokens, 5);
PGPreparableTokens[0].MatchingGroup := 'SELECT';
PGPreparableTokens[1].MatchingGroup := 'INSERT';
PGPreparableTokens[2].MatchingGroup := 'UPDATE';
PGPreparableTokens[3].MatchingGroup := 'DELETE';
PGPreparableTokens[4].MatchingGroup := 'VALUES';

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.

