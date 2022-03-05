{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          DBLib Statement common functionality           }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcDbLibStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
uses Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZCompatibility, ZClasses, ZSysUtils, ZCollections, ZDbcIntfs, ZDbcStatement,
  ZDbcDbLib, ZPlainDbLibConstants, ZPlainDbLibDriver;

type
  {** Implements Prepared SQL Statement for DBLib. With emulation}
  TZDBLibPreparedStatementEmulated = class(TZEmulatedPreparedStatement_A)
  private
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    FHandle: PDBPROCESS;
    FResults: IZCollection;
    FUserEncoding: TZCharEncoding;
    FClientCP: Word;
  protected
    procedure InternalExecuteStatement(const SQL: RawByteString);
    procedure FetchResults;
    procedure FlushPendingResults;
    function GetParamAsString(ParamIndex: Integer): RawByteString; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings); overload;
    constructor Create(const Connection: IZConnection; const Info: TStrings); overload;
    procedure Prepare; override;
    procedure Unprepare; override;
    function GetMoreResults: Boolean; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

  {** Implements Generic DBLib Statement. }
  TZDBLibStatement = class(TZDBLibPreparedStatementEmulated);

  TZDBLibCallableStatement = class(TZAbstractCallableStatement)
  private
    FSQL: string;
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    FHandle: PDBPROCESS;
    FLastRowsAffected: Integer;//Workaround for sybase
    FRetrievedResultSet: IZResultSet;
    FRetrievedUpdateCount: Integer;
    FUserEncoding: TZCharEncoding;

    procedure FetchResults;
  protected
    procedure SetInParamCount(const NewParamCount: Integer); override;
  public
    constructor Create(const Connection: IZConnection; const ProcName: string; Info: TStrings);
    procedure BeforeClose; override;

    procedure RegisterOutParameter(ParameterIndex: Integer;
      SqlType: Integer); override;
    function GetMoreResults: Boolean; override;
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit

(* target:
  declare @p1 int
set @p1=-1
exec sp_prepexec @p1 output,NULL,N'select [PersonID] from [Tasks] t join [PersonSnapShots] pss on t.[CostSnapShotID]=pss.ID where t.[TaskTypeID]=21 and [CompletionDate] is null'
select @p1

https://docs.microsoft.com/en-us/sql/relational-databases/system-stored-procedures/sp-prepare-transact-sql?view=sql-server-2017
https://docs.microsoft.com/en-us/sql/relational-databases/system-stored-procedures/sp-unprepare-transact-sql?view=sql-server-2017
https://docs.microsoft.com/en-us/sql/relational-databases/system-stored-procedures/sp-describe-undeclared-parameters-transact-sql?view=sql-server-2017
*)

uses
  Types, Math,
  ZDbcLogging, ZDbcCachedResultSet, ZDbcDbLibUtils, ZDbcDbLibResultSet,
  ZVariant, ZDbcUtils, ZEncoding, ZDbcResultSet
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
  {$IFDEF FAST_MOVE}, ZFastCode{$ENDIF}, ZMessages;

{ TZDBLibPreparedStatementEmulated }

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param SQL the command text.
  @param Info a statement parameters.
}
constructor TZDBLibPreparedStatementEmulated.Create(
  const Connection: IZConnection; const SQL: string; const Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  if Assigned(FDBLibConnection) then
    FPLainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := FDBLibConnection.GetConnectionHandle;
  ResultSetType := rtScrollInsensitive;
  FResults := TZCollection.Create;
  {note: this is a hack! Purpose is to notify Zeos all Charakter columns are
    UTF8-encoded. e.g. N(VAR)CHAR. Initial idea is made for MSSQL where we've NO
    valid tdsType to determine (Var)Char(Ansi-Encoding) or N(Var)Char encoding
    So this is stopping all encoding detections and increases the performance in
    a high rate. If Varchar fields are fetched you Should use a cast to N-Fields!
    Else all results are invalid!!!!! Just to invoke later questions!}
  if DefineStatementParameter(Self, 'ResetCodePage', '') = 'UTF8' then
    FUserEncoding := ceUTF8
  else
    Self.FUserEncoding := ceDefault;
  FNeedNCharDetection := True;
  FClientCP := ConSettings.ClientCodePage.CP;
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
function TZDBLibPreparedStatementEmulated.GetMoreResults: Boolean;
var
  ResultSet: IZResultSet;
  UpdateCount: IZAnyValue;
begin
  Result := FResults.Count > 0;
  if Result then begin
    if FResults.Items[0].QueryInterface(IZResultSet, ResultSet) = S_OK then begin
      LastResultSet := ResultSet;
      FOpenResultSet := Pointer(ResultSet);
    end else begin
      LastResultSet := nil;
      FOpenResultSet := nil;
      if FResults.Items[0].QueryInterface(IZAnyValue, UpdateCount) = S_OK then
        LastUpdateCount := UpdateCount.GetInteger;
    end;
    FResults.Delete(0);
  end;
end;

function TZDBLibPreparedStatementEmulated.GetParamAsString(
  ParamIndex: Integer): RawByteString;
var P: PAnsiChar;
begin
  // Todo: Talk with EgonHugeist wether this requiresmodifications for his Mextgen effort
  if InParamCount <= ParamIndex
  then Result := 'NULL'
  else Result := PrepareSQLParameter(InParamValues[ParamIndex],
      InParamTypes[ParamIndex], ClientVarManager, ConSettings, IsNCharIndex[ParamIndex] or (FClientCP = zCP_UTF8));
  P := Pointer(Result);
  if (P <> nil) and (PByte(P)^ = Ord(#39)) and not IsNCharIndex[ParamIndex] and
     (FDBLibConnection.GetProvider = dpMsSQL) and (FPlainDriver.GetDBLibraryVendorType = lvtFreeTDS) and
     (PByte(P+Length(Result)-1)^ = Ord(#39)) and (FClientCP = zCP_UTF8)
  then Result := 'N' + Result;
end;

{**
  Executes a Statement.
  Used internally to execute statements.

  @param Handle a DBLib connection handle.
  @sql string containing the statements to execute
}
procedure TZDBLibPreparedStatementEmulated.InternalExecuteStatement(
  const SQL: RawByteString);
var Ansi: RawByteString;
begin
  if FDBLibConnection.GetProvider = dpMsSQL then
    //This one is to avoid a bug in dblib interface as it drops a single backslash before line end
    Ansi := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}StringReplace(SQL, '\'#13, '\\'#13, [rfReplaceAll])
  else
    //This one is to avoid sybase error: Invalid operator for datatype op: is null type: VOID TYPE
    Ansi := StringReplaceAll_CS_LToEQ(SQL, RawByteString(' AND NULL IS NULL'), EmptyRaw);

  FHandle := FDBLibConnection.GetConnectionHandle;
  FPlainDriver := FDBLibConnection.GetPlainDriver;
  //2018-09-16 Coomented by marsupilami79 because this hides errors in the logic
  //result sets might get only partial data without an error
  //if FPlainDriver.dbcancel(FHandle) <> DBSUCCEED then
  //  FDBLibConnection.CheckDBLibError(lcExecute, SQL);

  if FPlainDriver.dbcmd(FHandle, Pointer(Ansi)) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);

  if FPlainDriver.dbsqlexec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcExecute, SQL);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
end;

procedure TZDBLibPreparedStatementEmulated.Prepare;
begin
  FlushPendingResults;
  inherited Prepare;
end;

procedure TZDBLibPreparedStatementEmulated.Unprepare;
begin
  FlushPendingResults;
  inherited UnPrepare;
end;

{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters.
}
constructor TZDBLibPreparedStatementEmulated.Create(
  const Connection: IZConnection; const Info: TStrings);
begin
  Create(Connection, '', Info)
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZDBLibPreparedStatementEmulated.ExecutePrepared: Boolean;
begin
  Prepare;
  InternalExecuteStatement(ComposeRawSQLQuery);
  FetchResults;
  Result := GetMoreResults and (LastResultSet <> nil);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZDBLibPreparedStatementEmulated.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  InternalExecuteStatement(ComposeRawSQLQuery);
  FetchResults;
  while GetMoreResults and (LastResultSet = nil) do ;
  Result := GetResultSet;
  FlastResultSet := nil;
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
function TZDBLibPreparedStatementEmulated.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  InternalExecuteStatement(ComposeRawSQLQuery);
  FetchResults;
  while GetMoreResults and (LastResultSet <> nil) do ;
  Result := GetUpdateCount;
end;

{**
  Fetches all results and creates a cachedresultset object for each resultset
  and a ZAnyValue object for each count value.
}
procedure TZDBLibPreparedStatementEmulated.FetchResults;
var
  NativeResultSet: TZDBLibResultSet;
  CachedResultSet: TZCachedResultSet;
  RowsAffected: Integer;
  ResultsRETCODE, cmdRowRETCODE: RETCODE;
begin
  repeat
    ResultsRETCODE := FPlainDriver.dbresults(FHandle);
    if ResultsRETCODE = DBFAIL then
      FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS/dbresults');
    cmdRowRETCODE := FPlainDriver.dbcmdrow(FHandle);
    //EH: if NO_MORE_RESULTS there might be a final update count see TestSF380(a/b)
    if (cmdRowRETCODE = DBSUCCEED) and (ResultsRETCODE <> NO_MORE_RESULTS) then begin
      {EH: Developer notes:
       the TDS protocol does NOT support any stmt handles. All actions are
       executed sequentially so in ALL cases we need cached Results NO WAY around!!!}
      NativeResultSet := TZDBLibResultSet.Create(Self, Self.SQL, FUserEncoding);
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet,
        Self.SQL, TZDBLibCachedResolver.Create(Self, NativeResultSet.GetMetaData), ConSettings);
      CachedResultSet.SetType(rtScrollInsensitive);//!!!Cached resultsets are allways this
      CachedResultSet.Last;
      CachedResultSet.BeforeFirst; //!!!Just to invoke fetchall
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      FResults.Add(CachedResultSet);
    end else begin
      RowsAffected := FPlainDriver.dbCount(FHandle);
      if RowsAffected > -1 then
        FResults.Add(TZAnyValue.CreateWithInteger(RowsAffected));
    end;
    FPlainDriver.dbCanQuery(FHandle);
  until ResultsRETCODE = NO_MORE_RESULTS;
  FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');

  (*if not FDBLibConnection.FreeTDS then
    if RowsAffected = -1 then
    begin
      FDBLibConnection.InternalExecuteStatement('select @@rowcount');
      try
        FPlainDriver.dbresults(FHandle);
        NativeResultSet := TZDBLibResultSet.Create(Self, 'select @@rowcount');
        try
          if NativeResultset.Next then
            RowsAffected := NativeResultSet.GetInt(FirstDbcIndex);
        finally
          NativeResultSet.Close;
        end;
        FResults.Add(TZAnyValue.CreateWithInteger(RowsAffected));
      finally
        FPlainDriver.dbCancel(FHandle);
      end;
      FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
    end; *)
end;

procedure TZDBLibPreparedStatementEmulated.FlushPendingResults;
var I: Integer;
  ResultSet: IZResultSet;
begin
  if LastResultSet <> nil then
    LastResultSet := nil;
  for I := 0 to FResults.Count -1 do
    if Supports(FResults[I], IZResultSet, ResultSet) then
      ResultSet.Close;
  FResults.Clear;
end;

constructor TZDBLibCallableStatement.Create(const Connection: IZConnection;
  const ProcName: string; Info: TStrings);
begin
  inherited Create(Connection, ProcName, Info);
  Connection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  if Assigned(FDBLibConnection) then
    FPLainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := FDBLibConnection.GetConnectionHandle;
  ResultSetType := rtScrollInsensitive;
  {note: this is a hack! Purpose is to notify Zeos all Character columns are
    UTF8-encoded. e.g. N(VAR)CHAR. Initial idea is made for MSSQL where we've NO
    valid tdsType to determine (Var)Char(Ansi-Encoding) or N(Var)Char encoding
    So this is stopping all encoding detections and increases the performance in
    a high rate. If Varchar fields are fetched you Should use a cast to N-Fields!
    Else all results are invalid!!!!! Just to invoke later questions!}
  if DefineStatementParameter(Self, 'ResetCodePage', '') = 'UTF8' then
    FUserEncoding := ceUTF8
  else
    Self.FUserEncoding := ceDefault;
end;

procedure TZDBLibCallableStatement.BeforeClose;
begin
  FRetrievedResultSet := nil;
  inherited BeforeClose;
end;

procedure TZDBLibCallableStatement.FetchResults;
var
  NativeResultSet: TZDBLibResultSet;
  CachedResultSet: TZCachedResultSet;
begin
//Sybase does not seem to return dbCount at all, so a workaround is made
  FLastRowsAffected := -2;
  while FPlainDriver.dbresults(FHandle) = DBSUCCEED do
  begin
    if FPlainDriver.dbcmdrow(FHandle) = DBSUCCEED then
    begin
      NativeResultSet := TZDBLibResultSet.Create(Self, FSQL);
      NativeResultSet.SetConcurrency(rcReadOnly);
      CachedResultSet := TZCachedResultSet.Create(NativeResultSet, FSQL,
        TZDBLibCachedResolver.Create(Self, NativeResultSet.GetMetaData), ConSettings);
      CachedResultSet.SetType(rtScrollInsensitive);//!!!Cached resultsets are allways this
      CachedResultSet.Last;
      CachedResultSet.BeforeFirst; //!!!Just to invoke fetchall
      CachedResultSet.SetConcurrency(GetResultSetConcurrency);
      FResultSets.Add(CachedResultSet);
    end
    else
    begin
      FLastRowsAffected := FPlainDriver.dbCount(FHandle);
      if FLastRowsAffected > -1 then
        FResultSets.Add(TZAnyValue.CreateWithInteger(FLastRowsAffected));
    end;
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'FETCHRESULTS');
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
function TZDBLibCallableStatement.GetMoreResults: Boolean;
var
  ResultSet: IZResultSet;
  UpdateCount: IZAnyValue;
begin
  Result := False;
  FRetrievedResultSet := nil;
  FRetrievedUpdateCount := -1;
  if FResultSets.Count > 0 then
  begin
    try
      Result := Supports(FResultSets[0], IZResultSet, ResultSet);
      if Result then
      begin
        FRetrievedResultSet := ResultSet;
        FRetrievedUpdateCount := 0;
      end
      else
        if Supports(FResultSets[0], IZAnyValue, UpdateCount) then
          FRetrievedUpdateCount := UpdateCount.GetInteger;
      FResultSets.Delete(0);
    finally
      ResultSet := nil;
      UpdateCount := nil;
    end;
  end;
end;

function TZDBLibCallableStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if not ExecutePrepared then
    while not GetMoreResults and (FRetrievedUpdateCount <> -1) do;
  Result := FRetrievedResultSet;
  FRetrievedResultSet := nil;
end;

function TZDBLibCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  if ExecutePrepared then
    while GetMoreResults and (FRetrievedUpdateCount = -1) do;
  Result := FRetrievedUpdateCount;
  FRetrievedResultSet := nil;
end;

procedure TZDBLibCallableStatement.RegisterOutParameter(ParameterIndex: Integer;
  SqlType: Integer);
begin
  SetOutParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
  OutParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TZSqlType(SqlType);

  //Count inparams must equal count outparams to correct set paramters
  if InParamCount < ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} then
    SetInParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
end;

type TZDbLibParam = record
  AsString: RawByteString;
  CharRec: TZCharRec;
  AsBytes: TBytes;
  TempBlob: IZBlob;
  case word of
    0: (AsBoolean: Boolean);
    1: (AsByte: Byte);
    2: (AsSmall: SmallInt);
    3: (AsInteger: Integer);
    4: (AsFloat: Single);
    5: (AsDouble: Double);
    6: (AsDBDATETIME: DBDATETIME);
end;

function TZDBLibCallableStatement.ExecutePrepared: Boolean;
var
  S: RawByteString;
  I, ParamIndex, DatLen: Integer;
  RetParam: Byte;
  ParamType: TZSQLType;
  P: Pointer;
  Len: NativeUInt;
  RetType: DBINT;
  Temp: TZVariant;
  Params: array of TZDbLibParam;

  OutString: RawByteString;
  OutBytes: TBytes;
  OutDouble: Double;
  OutDBDATETIME: DBDATETIME;
begin
  S := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}Trim(ASql);
  if FPLainDriver.dbRPCInit(FHandle, Pointer(S), 0) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCInit');

  SetLength(Params, InParamCount);

  for I := 1 to InParamCount - 1 do//The 0 parameter is the return value
  begin
    RetParam := 0;
    if OutParamTypes[I] <> stUnknown then
      RetParam := DBRPCRETURN;

    ParamType := InParamTypes[I];
    if ParamType = stUnknown then
      ParamType := OutParamTypes[I];

    if SoftVarManager.IsNull(InParamValues[I]) and (InParamTypes[I] <> stUnknown) then
      FPlainDriver.dbRpcParam(FHandle, nil, RetParam,
        Ord(ConvertSqlTypeToTDSType(InParamTypes[I])), -1, 0, nil)
    else
      case ParamType of
        stBoolean:
          begin
            Params[I].AsBoolean := SoftVarManager.GetAsBoolean(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsBit), -1, -1, @(Params[I].AsBoolean));
          end;
        stByte:
          begin
            Params[I].AsByte := Byte(SoftVarManager.GetAsInteger(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsInt1), -1, -1, @(Params[I].AsByte));
          end;
        stShort, stSmall:
          begin
            Params[I].AsSmall := SmallInt(SoftVarManager.GetAsInteger(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsInt2), -1, -1, @(Params[I].AsSmall));
          end;
        stWord, stInteger:
          begin
            Params[I].AsInteger := Integer(SoftVarManager.GetAsInteger(InParamValues[I]));
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsInt4), -1, -1, @(Params[I].AsInteger));
          end;
        stFloat:
          begin
            Params[I].AsFloat := SoftVarManager.GetAsFloat(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsFlt4), -1, -1, @(Params[I].AsFloat));
          end;
        stLong, stULong, stDouble, stBigDecimal, stCurrency:
          begin
            Params[I].AsDouble := SoftVarManager.GetAsFloat(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsFlt8), -1, -1, @(Params[I].AsDouble));
          end;
        stString, stUnicodeString:
          if IsNCharIndex[i] then
          begin
            Params[I].CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], zCP_UTF8);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsVarchar),
              -1, Max(1, Params[I].CharRec.Len), Params[I].CharRec.P);
          end else
          begin
            Params[I].CharRec := ClientVarManager.GetAsCharRec(InParamValues[I], ConSettings^.ClientCodePage^.CP);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsVarchar),
              -1, Max(1, Params[I].CharRec.Len), Params[I].CharRec.P);
          end;
        stDate:
          begin
            Params[I].AsString := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(InParamValues[I]),
              ConSettings^.WriteFormatSettings, False);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, ord(tdsChar),
              -1, ConSettings^.WriteFormatSettings.DateFormatLen, Pointer(Params[I].AsString));
          end;
        stTime:
          begin
            Params[I].AsString := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(InParamValues[I]),
              ConSettings^.WriteFormatSettings, False);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, ord(tdsChar),
              -1, ConSettings^.WriteFormatSettings.TimeFormatLen, Pointer(Params[I].AsString));
          end;
        stTimeStamp:
          begin
            Params[I].AsString := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(InParamValues[I]),
              ConSettings^.WriteFormatSettings, False);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, ord(tdsChar),
              -1, ConSettings^.WriteFormatSettings.DateTimeFormatLen, Pointer(Params[I].AsString));
          end;
        stAsciiStream, stUnicodeStream, stBinaryStream:
          begin
            Params[I].TempBlob := SoftVarManager.GetAsInterface(InParamValues[I]) as IZBlob;
            if ParamType = stBinaryStream then
              FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsImage),
                -1, Max(1, Params[I].TempBlob.Length), Params[I].TempBlob.GetBuffer)
            else
              if IsNCharIndex[i] then
              begin
                if Params[I].TempBlob.IsClob then
                begin
                  Params[I].CharRec.P := Params[I].TempBlob.GetPAnsiChar(zCP_UTF8);
                  Params[I].CharRec.Len := Max(1, Params[I].TempBlob.Length);
                end
                else
                begin
                  Params[I].AsString := GetValidatedAnsiStringFromBuffer(Params[I].TempBlob.GetBuffer, Params[I].TempBlob.Length, ConSettings, zCP_UTF8);
                  if Pointer(Params[I].AsString) = nil then
                  begin
                    Params[I].CharRec.P := PEmptyAnsiString;
                    Params[I].CharRec.Len := 1;
                  end
                  else
                  begin
                    Params[I].CharRec.P := Pointer(Params[I].AsString);
                    Params[I].CharRec.Len := {%H-}PLengthInt(NativeUInt(Params[I].AsString) - StringLenOffSet)^;
                  end;
                end;
                FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsText),
                    -1, Params[I].CharRec.Len, Params[I].CharRec.P)
              end
              else
              begin
                if Params[I].TempBlob.IsClob then
                begin
                  Params[I].CharRec.P := Params[I].TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                  Params[I].CharRec.Len := Max(1, Params[I].TempBlob.Length);
                end
                else
                begin
                  Params[I].AsString := GetValidatedAnsiStringFromBuffer(Params[I].TempBlob.GetBuffer,
                    Params[I].TempBlob.Length, ConSettings);
                  if Pointer(Params[I].AsString) = nil then
                  begin
                    Params[I].CharRec.P := PEmptyAnsiString;
                    Params[I].CharRec.Len := 1;
                  end
                  else
                  begin
                    Params[I].CharRec.P := Pointer(Params[I].AsString);
                    Params[I].CharRec.Len := {%H-}PLengthInt(NativeUInt(Params[I].AsString) - StringLenOffSet)^;;
                  end;
                end;
                FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsText),
                    -1, Params[I].CharRec.Len, Params[I].CharRec.P)
              end;
          end;
        stBytes:
          begin
            Params[I].AsBytes := SoftVarManager.GetAsBytes(InParamValues[I]);
            FPlainDriver.dbRpcParam(FHandle, nil, RetParam, Ord(tdsBinary),
              -1, Length(Params[I].AsBytes), Pointer(Params[I].AsBytes));
          end;
      else
        FPlainDriver.dbRpcParam(FHandle, nil, 0, Ord(tdsChar), 0, 0, nil);
    end;
  end;

  if FPLainDriver.dbRpcExec(FHandle) <> DBSUCCEED then
    FDBLibConnection.CheckDBLibError(lcOther, 'EXECUTEPREPARED:dbRPCExec');
  SetLength(Params, 0);
  FetchResults;
  Result := GetMoreResults;

  if FPLainDriver.dbHasRetStat(FHandle) then
    Temp := EncodeInteger(FPlainDriver.dbRetStatus(FHandle))
  else
    Temp := NullVariant;
  if Length(OutParamValues) = 0 then // check if DynArray is initialized for RETURN_VALUE
    SetOutParamCount(1);
  OutParamValues[0] := Temp; //set function RETURN_VALUE
  OutString := '';
  ParamIndex := 1;
  for I := 1 to OutParamCount - 1 do
  begin
    if OutParamTypes[I] = stUnknown then
      Continue;
    RetType := FPLainDriver.dbRetType(FHandle, ParamIndex);
    if (FPlainDriver.dbRetData(FHandle, ParamIndex) = nil) or
       (RetType = Ord(tdsVoid)) then
      Temp := NullVariant
    else
      case TTDSType(RetType) of
        tdsNVarChar, tdsBigNChar, tdsBigNVarChar:
          begin
            ZSetString(FPLainDriver.dbRetData(FHandle, ParamIndex),
              FPLainDriver.dbRetLen(FHandle, ParamIndex), OutString);
            ClientVarManager.SetAsUTF8String(Temp, OutString);
          end;
        tdsChar, tdsVarchar, tdsBigChar, tdsBigVarChar:
          begin
            P := FPLainDriver.dbRetData(FHandle, ParamIndex);
            Len := NativeUInt(FPLainDriver.dbRetLen(FHandle, ParamIndex));
            if ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then begin
              ZSetString(P, Len, OutString);
              ClientVarManager.SetAsRawByteString(Temp, OutString);
            end else
              case ZDetectUTF8Encoding(P, Len) of
                etUTF8:
                  begin
                    ZSetString(P, Len, OutString);
                    ClientVarManager.SetAsUTF8String(Temp, OutString);
                  end;
                etUSASCII:
                  begin
                    ZSetString(P, Len, OutString);
                    ClientVarManager.SetAsRawByteString(Temp, OutString);
                  end;
                else
                  ClientVarManager.SetAsUnicodeString(Temp, PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP));
              end;
          end;
        tdsBinary, tdsVarBinary, tdsBigBinary, tdsBigVarBinary:
          begin
            DatLen := FPLainDriver.dbRetLen(FHandle, ParamIndex);
            OutBytes := BufferToBytes(FPLainDriver.dbRetData(FHandle, ParamIndex), DatLen);
            SoftVarManager.SetAsBytes(Temp, OutBytes);
          end;
        tdsInt1:
          SoftVarManager.SetAsInteger(Temp,
            PByte(FPlainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsInt2:
          SoftVarManager.SetAsInteger(Temp,
            PSmallInt(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsInt4:
          SoftVarManager.SetAsInteger(Temp,
            PInteger(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsInt8:
          SoftVarManager.SetAsInteger(Temp,
            PInt64(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsFlt4:
          SoftVarManager.SetAsFloat(Temp,
            PSingle(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsFlt8:
          SoftVarManager.SetAsFloat(Temp,
            PDouble(FPLainDriver.dbRetData(FHandle, ParamIndex))^);
        tdsNumeric,
        tdsDecimal,
        tdsMoney,
        tdsMoney4:
          begin
            FPlainDriver.dbConvert(FHandle, RetType,
              FPlainDriver.dbRetData(FHandle, ParamIndex),
                FPLainDriver.dbRetLen(FHandle, ParamIndex), Ord(tdsFlt8),
              @OutDouble, 8);
            SoftVarManager.SetAsFloat(Temp, OutDouble);
          end;
        tdsDateTime4, tdsDateTimeN:
          begin
            FPLainDriver.dbConvert(FHandle, RetType,
              FPLainDriver.dbRetData(FHandle, ParamIndex), 4,
              RetType, @OutDBDATETIME, 8);
            SoftVarManager.SetAsDateTime(Temp,
              OutDBDATETIME.dtdays + 2 + (OutDBDATETIME.dttime / 25920000));
          end;
        tdsDateTime:
          begin
            OutDBDATETIME := PDBDATETIME(
              FPLainDriver.dbRetData(FHandle, ParamIndex))^;
            SoftVarManager.SetAsDateTime(Temp,
              OutDBDATETIME.dtdays + 2 + (OutDBDATETIME.dttime / 25920000));
          end;
        tdsImage:
          Temp := EncodeInterface(TZAbstractBlob.CreateWithData(
            FPlainDriver.dbRetData(FHandle, ParamIndex),
            FPLainDriver.dbRetLen(FHandle, ParamIndex)));
        tdsText:
          Temp := EncodeInterface(TZAbstractClob.CreateWithData(
            FPlainDriver.dbRetData(FHandle, ParamIndex),
            FPLainDriver.dbRetLen(FHandle, ParamIndex),
            ConSettings^.ClientCodePage^.CP, ConSettings));
        tdsNText:
          Temp := EncodeInterface(TZAbstractClob.CreateWithData(
            FPlainDriver.dbRetData(FHandle, ParamIndex),
            FPLainDriver.dbRetLen(FHandle, ParamIndex),
            zCP_UTF8, ConSettings));
        tdsBit:
          Temp := EncodeBoolean(PBoolean(FPlainDriver.dbRetData(FHandle, ParamIndex))^);
        else
          {
          tdsFltN,
          tdsFltN,
          tdsMoneyN:
          tdsUnique:
          tdsIntN:
          tdsVariant:
          tdsBitN:
          tdsUDT:
          tdsMSXML:}
          Temp := NullVariant;

      end;
    OutParamValues[I] := Temp;
    Inc(ParamIndex);
  end;

//Workaround for sybase. the dbCount does not work, so a select @@rowcount is
//made but this cleared the returned output parameters, so this is moved here
//after reading the output parameters
  //if Self.FDBLibConnection.GetProvider = dpSybase then
    //FetchRowCount;

  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, 'EXEC '+ ASQL);
end;

procedure TZDBLibCallableStatement.SetInParamCount(const NewParamCount: Integer);
begin
  inherited SetInParamCount(NewParamCount);

  if OutParamCount < NewParamCount then
    SetOutParamCount(NewParamCount);
end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
end.
