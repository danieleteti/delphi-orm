{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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

unit ZDbcSqLiteStatement;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcStatement, ZPlainSqLiteDriver, ZCompatibility, ZDbcLogging,
  ZVariant;

type
  {** SQLite Prepared SQL statement interface. }
  IZSQLitePreparedStatement = interface(IZPreparedStatement)
    ['{1C71D4D9-45D5-468F-A6D2-D7D29EB29A89}']
    function GetLastErrorCodeAndHandle(var StmtHandle: Psqlite3_stmt): Integer;
  end;

  {** Implements CAPI Prepared SQL Statement. }
  TZSQLiteCAPIPreparedStatement = class(TZAbstractPreparedStatement,
    IZSQLitePreparedStatement)
  private
    FErrorCode: Integer;
    FHandle: Psqlite;
    FStmtHandle: Psqlite3_stmt;
    FPlainDriver: IZSQLitePlainDriver;
    FBindDoubleDateTimeValues: Boolean;
    FUndefinedVarcharAsStringLength: Integer;
    fBindOrdinalBoolValues: Boolean;
    FExtendedErrorMessage: Boolean;
    function CreateResultSet: IZResultSet;
  protected
    function GetLastErrorCodeAndHandle(var StmtHandle: Psqlite3_stmt): Integer;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
  public
    constructor Create(const PlainDriver: IZSQLitePlainDriver;
      const Connection: IZConnection; const SQL: string; const Info: TStrings;
      const Handle: Psqlite); overload;
    constructor Create(const PlainDriver: IZSQLitePlainDriver;
      const Connection: IZConnection; const Info: TStrings; const Handle: Psqlite); overload;

    procedure Prepare; override;
    procedure Unprepare; override;

    procedure Cancel; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
  end;
  TZSQLiteStatement = class(TZSQLiteCAPIPreparedStatement);


{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit

uses
  {$IFDEF WITH_UNITANSISTRINGS} AnsiStrings,{$ENDIF} ZDbcSqLiteUtils,
  ZDbcSqLiteResultSet, ZSysUtils, ZEncoding, ZMessages, ZDbcCachedResultSet,
  ZDbcUtils;

(* out of use now...
procedure BindingDestructor(Value: PAnsiChar); cdecl;
begin
  {$IFDEF WITH_STRDISPOSE_DEPRECATED}AnsiStrings.{$ENDIF}StrDispose(Value);
end;*)

{ TZSQLiteCAPIPreparedStatement }

function TZSQLiteCAPIPreparedStatement.GetLastErrorCodeAndHandle(
  var StmtHandle: Psqlite3_stmt): Integer;
begin
  Result := FErrorCode;
  StmtHandle := FStmtHandle;
end;

function TZSQLiteCAPIPreparedStatement.CreateResultSet: IZResultSet;
var
  CachedResolver: TZSQLiteCachedResolver;
  NativeResultSet: TZSQLiteResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  { Creates a native result set. }
  NativeResultSet := TZSQLiteResultSet.Create(FPlainDriver, Self, Self.SQL, FHandle,
    FStmtHandle, FUndefinedVarcharAsStringLength, FExtendedErrorMessage);
  NativeResultSet.SetConcurrency(rcReadOnly);

  if (GetResultSetConcurrency = rcUpdatable)
    or (GetResultSetType <> rtForwardOnly) then
  begin
    { Creates a cached result set. }
    CachedResolver := TZSQLiteCachedResolver.Create(FPlainDriver, FHandle, Self,
      NativeResultSet.GetMetaData);
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, Self.SQL,
      CachedResolver,GetConnection.GetConSettings);
    CachedResultSet.SetType(rtScrollInsensitive);
    CachedResultSet.SetConcurrency(GetResultSetConcurrency);

    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
  FOpenResultSet := Pointer(Result); //weak reference to Resultset to avoid NO decrementing of RefCount.
    //we need this reference to close the SQLite resultset and reset the stmt handle.
end;

procedure TZSQLiteCAPIPreparedStatement.PrepareInParameters;
begin
  if FPlainDriver.bind_parameter_count(FStmtHandle) <> InParamCount then
    raise Exception.Create('Invalid InParamCount');
end;

const
  BoolArray: array[Boolean] of PAnsiChar = ('N', 'Y');

procedure TZSQLiteCAPIPreparedStatement.BindInParameters;
var
  TempBlob: IZBlob;
  I: Integer;
  Buffer: PAnsiChar;
  CharRec: TZCharRec;
begin
  FErrorcode := FPlainDriver.clear_bindings(FStmtHandle);
  CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcBindPrepStmt, ASQL, ConSettings, FExtendedErrorMessage);
  for i := 1 to InParamCount do
  begin
    if ClientVarManager.IsNull(InParamValues[i-1])  then
      FErrorcode := FPlainDriver.bind_null(FStmtHandle, I)
    else
    begin
      case InParamTypes[I-1] of
        stBoolean:
          if fBindOrdinalBoolValues then
            FErrorcode := FPlainDriver.bind_int(FStmtHandle, i,
              Ord(ClientVarManager.GetAsBoolean(InParamValues[i-1])))
          else
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              BoolArray[ClientVarManager.GetAsBoolean(InParamValues[i-1])], 1, nil);
        stByte, stShort, stWord, stSmall, stInteger:
          FErrorcode := FPlainDriver.bind_int(FStmtHandle, i,
            ClientVarManager.GetAsInteger(InParamValues[i-1]));
        stLongWord, stLong, stUlong:
          FErrorcode := FPlainDriver.bind_int64(FStmtHandle, i,
            ClientVarManager.GetAsInteger(InParamValues[i-1]));
        stFloat, stDouble, stCurrency, stBigDecimal:
          FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
            ClientVarManager.GetAsFloat(InParamValues[i-1]));
        stBytes:
          begin
            InParamValues[i-1].VBytes := SoftVarManager.GetAsBytes(InParamValues[i-1]);
            FErrorcode := FPlainDriver.bind_blob(FStmtHandle, i,
              @InParamValues[i-1].VBytes[0], Length(InParamValues[i-1].VBytes), nil);
          end;
        stString, stUnicodeString:
          begin
            CharRec := ClientVarManager.GetAsCharRec(InParamValues[i-1], zCP_UTF8);
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              CharRec.P, CharRec.Len, nil);
          end;
        stDate:
          if FBindDoubleDateTimeValues then
            FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
                ClientVarManager.GetAsDateTime(InParamValues[i-1])-JulianEpoch)
          else
          begin
            InParamValues[i-1].VRawByteString := DateTimeToRawSQLDate(
              ClientVarManager.GetAsDateTime(InParamValues[i-1]),
                ConSettings^.WriteFormatSettings, False);
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              Pointer(InParamValues[i-1].VRawByteString),
              ConSettings^.WriteFormatSettings.DateFormatLen, nil);
          end;
        stTime:
          if FBindDoubleDateTimeValues then
            FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
                ClientVarManager.GetAsDateTime(InParamValues[i-1])-JulianEpoch)
          else
          begin
            InParamValues[i-1].VRawByteString := DateTimeToRawSQLTime(
              ClientVarManager.GetAsDateTime(InParamValues[i-1]),
                ConSettings^.WriteFormatSettings, False);
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              Pointer(InParamValues[i-1].VRawByteString),
              ConSettings^.WriteFormatSettings.TimeFormatLen, nil);
          end;
        stTimestamp:
          if FBindDoubleDateTimeValues then
            FErrorcode := FPlainDriver.bind_double(FStmtHandle, i,
                ClientVarManager.GetAsDateTime(InParamValues[i-1])-JulianEpoch)
          else
          begin
            InParamValues[i-1].VRawByteString := DateTimeToRawSQLTimeStamp(
              ClientVarManager.GetAsDateTime(InParamValues[i-1]),
                ConSettings^.WriteFormatSettings, False);
            FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
              Pointer(InParamValues[i-1].VRawByteString),
              ConSettings^.WriteFormatSettings.DateTimeFormatLen, nil);
          end;
        stAsciiStream, stUnicodeStream, stBinaryStream:
          begin
            TempBlob := ClientVarManager.GetAsInterface(InParamValues[i-1]) as IZBlob;
            if not TempBlob.IsEmpty then
              if InParamTypes[I-1] = stBinaryStream then
              begin
                FErrorcode := FPlainDriver.bind_blob(FStmtHandle, i,
                  TempBlob.GetBuffer, TempBlob.Length, nil)
              end
              else
                if TempBlob.IsClob then
                begin
                  Buffer := TempBlob.GetPAnsiChar(zCP_UTF8);
                  FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
                    Buffer, TempBlob.Length, nil);
                end
                else
                begin
                  InParamValues[I-1].VRawByteString := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, ConSettings);
                  FErrorcode := FPlainDriver.bind_text(FStmtHandle, i,
                    Pointer(InParamValues[I-1].VRawByteString),
                    Length(InParamValues[I-1].VRawByteString), nil);
                end
            else
              FErrorcode := FPlainDriver.bind_null(FStmtHandle, I);
          end;
        else
          RaiseUnsupportedParameterTypeException(InParamTypes[I-1]);
      end;
    end;
    CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcBindPrepStmt, ASQL, ConSettings, FExtendedErrorMessage);
  end;
  inherited BindInParameters;
end;

constructor TZSQLiteCAPIPreparedStatement.Create(
  const PlainDriver: IZSQLitePlainDriver; const Connection: IZConnection;
  const SQL: string; const Info: TStrings; const Handle: Psqlite);
begin
  inherited Create(Connection, SQL, Info);
  FStmtHandle := nil;
  FHandle := Handle;
  FPlainDriver := PlainDriver;
  ResultSetType := rtForwardOnly;
  FBindDoubleDateTimeValues :=  StrToBoolEx(DefineStatementParameter(Self, 'BindDoubleDateTimeValues', 'false'));
  FUndefinedVarcharAsStringLength := StrToIntDef(DefineStatementParameter(Self, 'Undefined_Varchar_AsString_Length', '0'), 0);
  fBindOrdinalBoolValues := StrToBoolEx(DefineStatementParameter(Self, 'BindOrdinalBoolValues', 'false'));
  FExtendedErrorMessage := StrToBoolEx(DefineStatementParameter(Self, 'ExtendedErrorMessage', 'false'));
end;

constructor TZSQLiteCAPIPreparedStatement.Create(const PlainDriver: IZSQLitePlainDriver;
  const Connection: IZConnection; const Info: TStrings; const Handle: Psqlite);
begin
  Create(PlainDriver, Connection, '', Info, Handle);
end;

procedure TZSQLiteCAPIPreparedStatement.Prepare;
var pzTail: PAnsichar;
begin
  if not Prepared then
  begin
    FErrorCode := FPlainDriver.Prepare_v2(FHandle, Pointer(ASQL), Length(ASQL), FStmtHandle, pzTail{%H-});
    CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcPrepStmt, ASQL, ConSettings, FExtendedErrorMessage);
    inherited Prepare;
  end;
end;

procedure TZSQLiteCAPIPreparedStatement.Unprepare;
var ErrorCode: Integer;
begin
  { EH: do not change this sequence!: first close possbile opened resultset}
  inherited UnPrepare;
  if Assigned(FStmtHandle) then begin
    ErrorCode := FPlainDriver.finalize(FStmtHandle);
    FStmtHandle := nil; //Keep track we do not try to finalize the handle again on destroy or so
    if ErrorCode <> SQLITE_OK then
      CheckSQLiteError(FPlainDriver, FHandle, ErrorCode,
        lcUnprepStmt, 'sqlite3_finalize', ConSettings, FExtendedErrorMessage);
  end;
end;

{**
  Cancels this <code>Statement</code> object if both the DBMS and
  driver support aborting an SQL statement.
  This method can be used by one thread to cancel a statement that
  is being executed by another thread.
}
procedure TZSQLiteCAPIPreparedStatement.Cancel;
begin
  FPlainDriver.Interrupt(FHandle);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZSQLiteCAPIPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Prepare;
  PrepareOpenResultSetForReUse;
  BindInParameters;

  FErrorCode := FPlainDriver.Step(FStmtHandle); //exec prepared
  CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcOther,
    ConSettings^.ConvFuncs.ZStringToRaw(SCanNotRetrieveResultsetData,
    ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP), ConSettings, FExtendedErrorMessage);
  if FPlainDriver.column_count(FStmtHandle) = 0 then
  begin
    FPlainDriver.reset(FStmtHandle); //reset handle now!
    Result := nil;
  end
  else //expect a resultset
    if Assigned(FOpenResultSet) then
      Result := IZResultSet(FOpenResultSet) //return allready reseted RS
    else
      Result := CreateResultSet; //resultset executes reset stmt-handle

  inherited ExecuteQueryPrepared; //Log values
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
function TZSQLiteCAPIPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Prepare;
  BindInParameters;

  Result := 0;
  try
    CheckSQLiteError(FPlainDriver, FHandle, FPlainDriver.Step(FStmtHandle),
      lcExecPrepStmt, ASQL, ConSettings, FExtendedErrorMessage); //exec prepared
    Result := FPlainDriver.Changes(FHandle);
    inherited ExecuteUpdatePrepared; //log values
  finally
    FPlainDriver.reset(FStmtHandle); //reset handle allways without check else -> leaking mem
    LastUpdateCount := Result;
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
function TZSQLiteCAPIPreparedStatement.ExecutePrepared: Boolean;
begin
  Prepare;
  PrepareLastResultSetForReUse;
  BindInParameters;

  FErrorCode := FPlainDriver.Step(FStmtHandle);
  CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcExecPrepStmt, 'Step', ConSettings, FExtendedErrorMessage);

  { Process queries with result sets }
  if FPlainDriver.column_count(FStmtHandle) <> 0 then
  begin
    Result := True;
    if not Assigned(LastResultSet) then
      LastResultSet := CreateResultSet;
  end
  { Processes regular query. }
  else
  begin
    Result := False;
    LastUpdateCount := FPlainDriver.Changes(FHandle);
    FErrorCode := FPlainDriver.reset(FStmtHandle);
    CheckSQLiteError(FPlainDriver, FHandle, FErrorCode, lcOther, 'Reset', ConSettings, FExtendedErrorMessage);
  end;
  inherited ExecutePrepared;
end;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
end.

