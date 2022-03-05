{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                 ADO Statement Classes                   }
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

unit ZDbcAdoStatement;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}
{$IFNDEF ZEOS_DISABLE_ADO}
uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, ActiveX,
  ZCompatibility, ZSysUtils, ZOleDB,
  ZDbcIntfs, ZDbcStatement, ZDbcAdo, ZPlainAdo, ZVariant, ZDbcAdoUtils;

type
  {** Implements Prepared ADO Statement. }
  TZAdoPreparedStatement = class(TZAbstractPreparedStatement)
  private
    AdoRecordSet: ZPlainAdo.RecordSet;
    FAdoCommand: ZPlainAdo.Command;
    FAdoConnection: IZAdoConnection;
    FIsSelectSQL: Boolean;
  protected
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
    procedure ReleaseConnection; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings); overload;
    constructor Create(const Connection: IZConnection; const Info: TStrings); overload;
    destructor Destroy; override;

    procedure Prepare; override;
    procedure Unprepare; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; overload; override;
    function GetMoreResults(var RS: IZResultSet): Boolean; reintroduce; overload;

    procedure ClearParameters; override;
  end;

  TZAdoEmulatedPreparedStatement = class(TZEmulatedPreparedStatement_W)
  private
    AdoRecordSet: ZPlainAdo.RecordSet;
    FAdoCommand: ZPlainAdo.Command;
    FAdoConnection: IZAdoConnection;
  protected
    function GetParamAsString(ParamIndex: Integer): ZWideString; override;
    procedure ReleaseConnection; override;
  public
    constructor Create(Connection: IZConnection; const SQL: string;
      const Info: TStrings); overload;
    constructor Create(Connection: IZConnection; const Info: TStrings); overload;
  public
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    function GetMoreResults: Boolean; overload; override;
  end;

  TZAdoStatement = class(TZAdoEmulatedPreparedStatement);

  {** Implements Callable ADO Statement. }
  TZAdoCallableStatement = class(TZAbstractCallableStatement)
  private
    AdoRecordSet: ZPlainAdo.RecordSet;
    FAdoCommand: ZPlainAdo.Command;
    FAdoConnection: IZAdoConnection;
    FDirectionTypes: TDirectionTypes;
  protected
    function GetOutParam(ParameterIndex: Integer): TZVariant; override;
    procedure PrepareInParameters; override;
    procedure BindInParameters; override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string;
      const Info: TStrings);
    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;

    procedure RegisterParamType(ParameterIndex: Integer; ParamType: Integer); override;
    function GetMoreResults: Boolean; override;
    procedure Unprepare; override;
  end;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  Variants, Math, {$IFNDEF FPC}Windows{inline},{$ENDIF}
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  {$IFDEF WITH_TOBJECTLIST_INLINE} System.Contnrs{$ELSE} Contnrs{$ENDIF},
  ZEncoding, ZDbcLogging, ZDbcCachedResultSet, ZDbcResultSet,
  ZDbcMetadata, ZDbcResultSetMetadata, ZDbcUtils, ZMessages, ZFastCode;

{ TZAdoPreparedStatement }

constructor TZAdoPreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  FAdoCommand := CoCommand.Create;
  inherited Create(Connection, SQL, Info);
  FAdoCommand.CommandText := WSQL;
  FAdoCommand.CommandType := adCmdText;
  FAdoConnection := Connection as IZAdoConnection;
  FAdoCommand._Set_ActiveConnection(FAdoConnection.GetAdoConnection);
end;

constructor TZAdoPreparedStatement.Create(const Connection: IZConnection;
  const Info: TStrings);
begin
  Create(Connection, '', Info);
end;

destructor TZAdoPreparedStatement.Destroy;
begin
  AdoRecordSet := nil;
  FAdoConnection := nil;
  inherited Destroy;
  FAdoCommand := nil;
end;

procedure TZAdoPreparedStatement.Prepare;
begin
  if Not Prepared then //prevent PrepareInParameters
  begin
      FIsSelectSQL := IsSelect(SQL);
    FAdoCommand.CommandText := WSQL;
    inherited Prepare;
      FAdoCommand.Prepared := True;
  end;
end;

procedure TZAdoPreparedStatement.PrepareInParameters;
begin
  if InParamCount > 0 then
      RefreshParameters(FAdoCommand);
end;

procedure TZAdoPreparedStatement.ReleaseConnection;
begin
  inherited;
  FAdoConnection := nil;
end;

procedure TZAdoPreparedStatement.BindInParameters;
var
  I: Integer;
begin
  if InParamCount = 0 then
    Exit
  else
    if ArrayCount = 0 then
      begin
        for i := 0 to InParamCount-1 do
          if ClientVarManager.IsNull(InParamValues[i]) then
            {if (InParamDefaultValues[i] <> '') and (UpperCase(InParamDefaultValues[i]) <> 'NULL') and
            StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true')) then
            begin
              ClientVarManager.SetAsString(InParamValues[i], InParamDefaultValues[i]);
              ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], InParamValues[i], adParamInput)
            end
            else}
              ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], NullVariant, adParamInput)
          else
            ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], InParamValues[i], adParamInput)
    end
    else
      LastUpdateCount := ADOBindArrayParams(FAdoCommand, FAdoConnection, ConSettings,
            InParamValues, adParamInput, ArrayCount);
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZAdoPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  RC: OleVariant;
begin
  if Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close; //Note keep track we close the RS and DO NOT Try to resync them!
  FOpenResultSet := nil;
  Prepare;
  LastUpdateCount := -1;
  BindInParameters;
  try
    if FIsSelectSQL then
    begin
      AdoRecordSet := CoRecordSet.Create;
      AdoRecordSet.MaxRecords := MaxRows;
      AdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
      AdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
    end
    else
      AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, -1{, adExecuteNoRecords});
    Result := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    LastUpdateCount := {%H-}RC;
    if not Assigned(Result) then
      while (not GetMoreResults(Result)) and (LastUpdateCount > -1) do ;
    FOpenResultSet := Pointer(Result);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
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
function TZAdoPreparedStatement.ExecuteUpdatePrepared: Integer;
var
  RC: OleVariant;
begin
  Prepare;
  LastUpdateCount := -1;
  BindInParameters;
  try
    AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, adExecuteNoRecords);
    LastUpdateCount := {%H-}RC;
    Result := LastUpdateCount;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAdoPreparedStatement.ExecutePrepared: Boolean;
var
  RC: OleVariant;
begin
  LastResultSet := nil;
  LastUpdateCount := -1;

  Prepare;
  BindInParameters;
  try
    if FIsSelectSQL then
    begin
      AdoRecordSet := CoRecordSet.Create;
      AdoRecordSet.MaxRecords := MaxRows;
      AdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
      AdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
    end
    else
        AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, -1{, adExecuteNoRecords});
        LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
          SQL, ConSettings, ResultSetConcurrency);
        LastUpdateCount := {%H-}RC;
    Result := Assigned(LastResultSet);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

function TZAdoPreparedStatement.GetMoreResults: Boolean;
var
  RC: OleVariant;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet := AdoRecordSet.NextRecordset(RC);
    LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(LastResultSet);
    LastUpdateCount := RC;
  end;
end;

function TZAdoPreparedStatement.GetMoreResults(var RS: IZResultSet): Boolean;
var RC: OleVariant;
begin
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet := AdoRecordSet.NextRecordset(RC);
    RS := GetCurrentResultSet(AdoRecordSet, (Connection as IZAdoConnection), Self,
      SQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(RS);
    LastUpdateCount := RC;
  end
  else Result := False;
end;

procedure TZAdoPreparedStatement.Unprepare;
begin
  if FAdoCommand.Prepared then
    FAdoCommand.Prepared := False;
  inherited Unprepare;
end;

procedure TZAdoPreparedStatement.ClearParameters;
begin
  inherited ClearParameters;
    RefreshParameters(FAdoCommand);
end;
{ TZAdoCallableStatement }

constructor TZAdoCallableStatement.Create(const Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FAdoCommand := CoCommand.Create;
  FAdoCommand.CommandText := WSQL;
  FAdoConnection := Connection as IZAdoConnection;
  FAdoCommand._Set_ActiveConnection(FAdoConnection.GetAdoConnection);
  FAdoCommand.CommandType := adCmdStoredProc;
end;

function TZAdoCallableStatement.ExecuteQueryPrepared: IZResultSet;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  ColumnsInfo: TObjectList;
  RS: TZVirtualResultSet;
  IndexAlign: TIntegerDynArray;
  P: Pointer;
  Stream: TStream;
  Temp: OleVariant;
  RC: OleVariant;
  BD: Extended;
  PD: PDecimal;
  L: NativeUInt;
begin
  if Not Prepared then Prepare;
  BindInParameters;
  AdoRecordSet := CoRecordSet.Create;
  AdoRecordSet.MaxRecords := MaxRows;
  AdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
  AdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
  Result := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self, SQL, ConSettings, ResultSetConcurrency);
  LastUpdateCount := RC;
  if Result <> nil then
    Exit;
 // AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, -1{, adExecuteNoRecords});
 // LastUpdateCount := RC;

  SetLength(IndexAlign, 0);
  ColumnsInfo := TObjectList.Create(True);
  Stream := nil;
  if LastResultSet <> nil then begin
    Result := LastResultSet;
    LastResultSet := nil;
  end else
    try
      for I := 0 to FAdoCommand.Parameters.Count -1 do
        if FAdoCommand.Parameters.Item[i].Direction in [adParamOutput,
          adParamInputOutput, adParamReturnValue] then begin
        SetLength(IndexAlign, Length(IndexAlign)+1);
        ColumnInfo := TZColumnInfo.Create;
        with ColumnInfo do begin
          {$IFNDEF UNICODE}
          ColumnLabel := PUnicodeToString(Pointer(FAdoCommand.Parameters.Item[i].Name), Length(FAdoCommand.Parameters.Item[i].Name), ConSettings^.CTRL_CP);
          {$ELSE}
          ColumnLabel := FAdoCommand.Parameters.Item[i].Name;
          {$ENDIF}
          ColumnType := ConvertAdoToSqlType(FAdoCommand.Parameters.Item[I].Type_, ConSettings.CPType);
          ColumnDisplaySize := FAdoCommand.Parameters.Item[I].Precision;
          Precision := FAdoCommand.Parameters.Item[I].Precision;
          IndexAlign[High(IndexAlign)] := I;
        end;
        ColumnsInfo.Add(ColumnInfo);
      end;

      RS := TZVirtualResultSet.CreateWithColumns(ColumnsInfo, '', ConSettings);
      with RS do begin
        SetType(rtScrollInsensitive);
        SetConcurrency(rcReadOnly);
        RS.MoveToInsertRow;
        for i := FirstDbcIndex to ColumnsInfo.Count{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
          with FAdoCommand.Parameters.Item[IndexAlign[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]],
             TZColumnInfo(ColumnsInfo[i{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
            Temp := Value;
            case tagVariant(Temp).vt of
              VT_NULL, VT_EMPTY: ;
              VT_BOOL:        RS.UpdateBoolean(i, tagVARIANT(Temp).vbool);
              VT_UI1:         RS.UpdateByte(i, tagVARIANT(Temp).bVal);
              VT_UI2:         RS.UpdateWord(i, tagVARIANT(Temp).uiVal);
              VT_UI4:         RS.UpdateUInt(i, tagVARIANT(Temp).ulVal);
              VT_UINT:        RS.UpdateUInt(i, tagVARIANT(Temp).uintVal);
              VT_I1:          RS.UpdateShort(i, ShortInt(tagVARIANT(Temp).cVal));
              VT_I2:          RS.UpdateSmall(i, tagVARIANT(Temp).iVal);
              VT_I4:          RS.UpdateInt(i, tagVARIANT(Temp).lVal);
              VT_INT,
              VT_HRESULT:     RS.UpdateInt(i, tagVARIANT(Temp).intVal);
              VT_ERROR:       RS.UpdateInt(i, tagVARIANT(Temp).scode);
              VT_I8:          RS.UpdateLong(i, {$IFDEF WITH_tagVARIANT_UINT64}tagVARIANT(Temp).llVal{$ELSE}PInt64(@tagVARIANT(Temp).cyVal)^{$ENDIF});
              VT_UI8:         RS.UpdateULong(i, {$IFDEF WITH_tagVARIANT_UINT64}tagVARIANT(Temp).ullVal{$ELSE}PUInt64(@tagVARIANT(Temp).cyVal)^{$ENDIF});
              VT_R4:          RS.UpdateFloat(i, tagVARIANT(Temp).fltVal);
              VT_R8:          RS.UpdateDouble(i, tagVARIANT(Temp).dblVal);
              VT_CY:          RS.UpdateCurrency(i, tagVARIANT(Temp).cyVal);
              VT_DATE:        RS.UpdateTimeStamp(i, tagVARIANT(Temp).date);
              VT_BSTR:        begin
                                L := PLongInt(PAnsiChar(tagVARIANT(Temp).bstrVal)-SizeOf(LongInt))^ shr 1; //fpc has no SysStringLen -> improve
                                RS.UpdatePWideChar(i, tagVARIANT(Temp).bstrVal, @L);
                              end;
              VT_DECIMAL:     begin
                                PD := @Temp;
                                if Ord(ColumnType) <= Ord(stULong) then
                                  if PD.Sign > 0
                                  then RS.UpdateLong(i, -UInt64(PD.Lo64))
                                  else RS.UpdateULong(I, UInt64(PD.Lo64))
                                else begin
                                  if PD.Scale > 0
                                  then BD := UInt64(PD.Lo64) / UInt64Tower[Pd.Scale]
                                  else BD := UInt64(PD.Lo64);
                                  if PD.Sign > 0 then
                                    BD := -BD;
                                  RS.UpdateBigDecimal(i, BD);
                                end;
                              end;
              else            case Type_ of
                                adBinary,
                                adVarBinary: RS.UpdateBytes(i, VarToBytes(Temp));
                                adLongVarBinary:
                                    if VarIsArray(Temp) then begin
                                      P := VarArrayLock(Temp);
                                      try
                                        Stream := StreamFromData(P, VarArrayHighBound(Temp, 1)+1);
                                        RS.UpdateBinaryStream(I, Stream);
                                        FreeAndNil(Stream);
                                      finally
                                        VarArrayUnLock(Temp);
                                      end;
                                    end;
                                else UpdateNull(I); //debug -> not required
                              end;
            end;
          end;
        end;
        RS.InsertRow;
      end;
      Result := RS;
    finally
      ColumnsInfo.Free;
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
function TZAdoCallableStatement.ExecuteUpdatePrepared: Integer;
begin
  ExecutePrepared;
  Result := LastUpdateCount;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAdoCallableStatement.ExecutePrepared: Boolean;
var
  RC: OleVariant;
begin
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Not Prepared then Prepare;

  BindInParameters;
  try
    if IsSelect(SQL) then
    begin
      AdoRecordSet := CoRecordSet.Create;
      AdoRecordSet.MaxRecords := MaxRows;
      AdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
      AdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
    end
    else
      AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, -1{, adExecuteNoRecords});
    LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    LastUpdateCount := RC;
    Result := Assigned(LastResultSet);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0,
        ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

procedure TZAdoCallableStatement.RegisterParamType(ParameterIndex: Integer;
  ParamType: Integer);
begin
  inherited RegisterParamType(ParameterIndex, ParamType);
  if Length(FDirectionTypes) < ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} then
    SetLength(FDirectionTypes, ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
  FDirectionTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := ZProcedureColumnType2AdoType[TZProcedureColumnType(ParamType)];
end;

function TZAdoCallableStatement.GetMoreResults: Boolean;
var
  RC: OleVariant;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet := AdoRecordSet.NextRecordset(RC);
    LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(LastResultSet);
    LastUpdateCount := RC;
  end;
end;

procedure TZAdoCallableStatement.Unprepare;
begin
  if FAdoCommand.Prepared then
    FAdoCommand.Prepared := False;
  inherited;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAdoCallableStatement.GetOutParam(ParameterIndex: Integer): TZVariant;
var
  Temp: OleVariant;
  P: Pointer;
  TempBlob: IZBLob;
  PD: PDecimal;
  L: LengthInt;
begin
  if ParameterIndex > OutParamCount then
    Result := NullVariant
  else with FAdoCommand.Parameters.Item[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] do begin
    Temp := Value;
    case tagVARIANT(Temp).vt of
      VT_NULL, VT_EMPTY: ClientVarManager.SetNull(Result);
      VT_BOOL:        ClientVarManager.SetAsBoolean(Result, tagVARIANT(Temp).vbool);
      VT_UI1:         ClientVarManager.SetAsInteger(Result, tagVARIANT(Temp).bVal);
      VT_UI2:         ClientVarManager.SetAsInteger(Result, tagVARIANT(Temp).uiVal);
      VT_UI4:         ClientVarManager.SetAsInteger(Result, tagVARIANT(Temp).ulVal);
      VT_UINT:        ClientVarManager.SetAsInteger(Result, tagVARIANT(Temp).uintVal);
      VT_I1:          ClientVarManager.SetAsInteger(Result, ShortInt(tagVARIANT(Temp).cVal));
      VT_I2:          ClientVarManager.SetAsInteger(Result, tagVARIANT(Temp).iVal);
      VT_I4:          ClientVarManager.SetAsInteger(Result, tagVARIANT(Temp).lVal);
      VT_INT,
      VT_HRESULT:     ClientVarManager.SetAsInteger(Result, tagVARIANT(Temp).intVal);
      VT_ERROR:       ClientVarManager.SetAsInteger(Result, tagVARIANT(Temp).scode);
      VT_I8:          ClientVarManager.SetAsInteger(Result, {$IFDEF WITH_tagVARIANT_UINT64}tagVARIANT(Temp).llVal{$ELSE}PInt64(@tagVARIANT(Temp).cyVal)^{$ENDIF});
      VT_UI8:         ClientVarManager.SetAsUInteger(Result, {$IFDEF WITH_tagVARIANT_UINT64}tagVARIANT(Temp).ullVal{$ELSE}PUInt64(@tagVARIANT(Temp).cyVal)^{$ENDIF});
      VT_R4:          ClientVarManager.SetAsFloat(Result, tagVARIANT(Temp).fltVal);
      VT_R8:          ClientVarManager.SetAsFloat(Result, tagVARIANT(Temp).dblVal);
      VT_CY:          ClientVarManager.SetAsFloat(Result, tagVARIANT(Temp).cyVal);
      VT_DATE:        ClientVarManager.SetAsDateTime(Result, tagVARIANT(Temp).date);
      VT_BSTR:        begin
                        ClientVarManager.SetNull(Result);
                        L := PLongInt(PAnsiChar(tagVARIANT(Temp).bstrVal)-SizeOf(LongInt))^ shr 1; //fpc has no SysStringLen -> improve
                        case type_ of
                          adChar, adWChar:       L := ZDbcUtils.GetAbsorbedTrailingSpacesLen(tagVARIANT(Temp).bstrVal, L);
                        end;
                        if ConSettings.CPType = cCP_UTF16 then begin
                          Result.VType := vtUnicodeString;
                          System.SetString(Result.VUnicodeString, tagVARIANT(Temp).bstrVal, L);
                        end else if ConSettings.CPType = cCP_UTF8 then begin
                          Result.VType := vtUTF8String;
                          Result.VUTF8String := PUnicodeToRaw(tagVARIANT(Temp).bstrVal, L, ConSettings.CTRL_CP);
                        end else begin
                          Result.VType := vtAnsiString;
                          Result.VAnsiString := PUnicodeToRaw(tagVARIANT(Temp).bstrVal, L, ConSettings.CTRL_CP);
                        end;
                      end;
      VT_DECIMAL:     begin
                        PD := @Temp;
                        ClientVarManager.SetNull(Result);
                        Result.VType := vtFloat;
                        if PD.Scale > 0
                        then Result.VFloat := UInt64(PD.Lo64) / UInt64Tower[Pd.Scale]
                        else Result.VFloat := UInt64(PD.Lo64);
                        if PD.Sign > 0 then
                          Result.VFloat := -Result.VFloat;
                      end;
      else            case Type_ of
                        adBinary,
                        adVarBinary: ClientVarManager.SetAsBytes(Result, VarToBytes(Temp));
                        adLongVarBinary: begin
                            TempBlob := nil;
                            if VarIsArray(Temp) then begin
                              P := VarArrayLock(Temp);
                              try
                                TempBlob := TZAbstractBlob.CreateWithData(P, VarArrayHighBound(Temp, 1)+1);
                              finally
                                VarArrayUnLock(Temp);
                              end;
                            end;
                            ClientVarManager.SetAsInterface(Result, TempBlob);
                            TempBlob := nil;
                          end;
                        else ClientVarManager.SetNull(Result);
                      end;
    end;
  end;
  LastWasNull := ClientVarManager.IsNull(Result);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

procedure TZAdoCallableStatement.PrepareInParameters;
var I: Integer;
begin
  if InParamCount > 0 then begin
    RefreshParameters(FAdoCommand, @FDirectionTypes);
    if Length(FDBParamTypes) <> FAdoCommand.Parameters.Count then begin
      SetLength(FDBParamTypes, FAdoCommand.Parameters.Count);
      SetLength(FDirectionTypes, Length(FDBParamTypes));
      for i := 0 to high(FDBParamTypes) do begin
        FDirectionTypes[i] := FAdoCommand.Parameters[i].Direction;
        case FDirectionTypes[i] of
          adParamInput: FDBParamTypes[i] := pctIn;
          adParamOutput: FDBParamTypes[i] := pctOut;
          adParamInputOutput: FDBParamTypes[i] := pctInOut;
          adParamReturnValue: FDBParamTypes[i] := pctReturn;
        end;
      end;
    end;
  end;
  FAdoCommand.Prepared := True;
end;

procedure TZAdoCallableStatement.BindInParameters;
var
  I: Integer;
begin
  if InParamCount = 0 then
    Exit
  else
    for i := 0 to InParamCount-1 do
      if FDBParamTypes[i] in [pctIn, pctInOut] then
        if ClientVarManager.IsNull(InParamValues[i]) then
          if (InParamDefaultValues[i] <> '') and (UpperCase(InParamDefaultValues[i]) <> 'NULL') and
            StrToBoolEx(DefineStatementParameter(Self, 'defaults', 'true')) then
          begin
            ClientVarManager.SetAsString(InParamValues[i], InParamDefaultValues[i]);
            ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], InParamValues[i], adParamInput)
          end
          else
            ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], NullVariant, FDirectionTypes[i])
        else
          ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], InParamValues[i], FDirectionTypes[i])
      else
        ADOSetInParam(FAdoCommand, FAdoConnection, InParamCount, I+1, InParamTypes[i], NullVariant, FDirectionTypes[i]);
end;

{ TZAdoEmulatedPreparedStatement }

constructor TZAdoEmulatedPreparedStatement.Create(Connection: IZConnection;
  const SQL: string; const Info: TStrings);
begin
  FAdoCommand := CoCommand.Create;
  inherited Create(Connection, SQL, Info);
  FAdoCommand.CommandText := WSQL;
  FAdoConnection := Connection as IZAdoConnection;
  FAdoCommand._Set_ActiveConnection(FAdoConnection.GetAdoConnection);
  FAdoCommand.CommandType := adCmdText;
end;

constructor TZAdoEmulatedPreparedStatement.Create(Connection: IZConnection;
  const Info: TStrings);
begin
  Create(Connection, '', Info);
end;

function TZAdoEmulatedPreparedStatement.ExecutePrepared: Boolean;
var
  RC: OleVariant;
begin
  LastResultSet := nil;
  LastUpdateCount := -1;
  Prepare;
  FAdoCommand.CommandText := ComposeWideSQLQuery;
  try
    if IsSelect(SQL) then
    begin
      AdoRecordSet := CoRecordSet.Create;
      AdoRecordSet.MaxRecords := MaxRows;
      AdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
      AdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
    end
    else
      AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, -1{, adExecuteNoRecords});
    LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    LastUpdateCount := {%H-}RC;
    Result := Assigned(LastResultSet);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

function TZAdoEmulatedPreparedStatement.ExecuteQueryPrepared: IZResultSet;
var
  RC: OleVariant;
begin
  if Assigned(FOpenResultSet) then
    IZResultSet(FOpenResultSet).Close;
  FOpenResultSet := nil;
  LastUpdateCount := -1;
  Prepare;
  FAdoCommand.CommandText := ComposeWideSQLQuery;
  try
    if IsSelect(SQL) then
      begin
        AdoRecordSet := CoRecordSet.Create;
        AdoRecordSet.MaxRecords := MaxRows;
        AdoRecordSet._Set_ActiveConnection(FAdoCommand.Get_ActiveConnection);
        AdoRecordSet.Open(FAdoCommand, EmptyParam, adOpenForwardOnly, adLockOptimistic, adAsyncFetch);
      end else
        AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, -1{, adExecuteNoRecords});
    Result := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    LastUpdateCount := {%H-}RC;
    if not Assigned(Result) then
      while (not GetMoreResults) and (LastUpdateCount > -1) do ;
    FOpenResultSet := Pointer(Result);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

function TZAdoEmulatedPreparedStatement.ExecuteUpdatePrepared: Integer;
var
  RC: OleVariant;
begin
  Prepare;
  LastUpdateCount := -1;
  Prepare;
  FAdoCommand.CommandText := ComposeWideSQLQuery;
  try
    AdoRecordSet := FAdoCommand.Execute(RC, EmptyParam, adExecuteNoRecords);
    LastUpdateCount := {%H-}RC;
    Result := LastUpdateCount;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ASQL);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ASQL, 0, ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end
end;

function TZAdoEmulatedPreparedStatement.GetMoreResults: Boolean;
var
  RC: OleVariant;
begin
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet := AdoRecordSet.NextRecordset(RC);
    LastResultSet := GetCurrentResultSet(AdoRecordSet, FAdoConnection, Self,
      SQL, ConSettings, ResultSetConcurrency);
    Result := Assigned(LastResultSet);
    LastUpdateCount := RC;
  end;
end;

function TZAdoEmulatedPreparedStatement.GetParamAsString(
  ParamIndex: Integer): ZWideString;
var
  TempBytes: TBytes;
  TempBlob: IZBlob;
  CharRec: TZCharRec;
  Tmp: ZWideString;
label SetNull;
begin
  if ClientVarManager.IsNull(InParamValues[ParamIndex]) then
SetNull:
    if (InParamDefaultValues[ParamIndex] <> '')
    then Result := ConSettings^.ConvFuncs.ZStringToUnicode(InParamDefaultValues[ParamIndex],
          ConSettings^.CTRL_CP)
    else Result := 'NULL'
  else case InParamTypes[ParamIndex] of
    stBoolean: if ClientVarManager.GetAsBoolean(InParamValues[ParamIndex])
      then Result := '''1'''
      else Result := '''0''';
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := ClientVarManager.GetAsUnicodeString(InParamValues[ParamIndex]);
    stBytes:
      begin
        TempBytes := ClientVarManager.GetAsBytes(InParamValues[ParamIndex]);
        Result := GetSQLHexWideString(PAnsiChar(TempBytes), Length(TempBytes), True);
      end;
    stString, stUnicodeString: begin
      CharRec := ClientVarManager.GetAsCharRec(InParamValues[ParamIndex], zCP_UTF16);
      Result := SQLQuotedStr(CharRec.P, charRec.Len, WideChar(#39));
    end;
    stGUID: if InParamValues[ParamIndex].VType = vtBytes
            then Result := #39+ZSysUtils.GUIDToUnicode(InParamValues[ParamIndex].VBytes)+#39
            else Result := #39+ClientVarManager.GetAsUnicodeString(InParamValues[ParamIndex])+#39;
    stDate:
      Result := DateTimeToUnicodeSQLDate(ClientVarManager.GetAsDateTime(InParamValues[ParamIndex]),
        ConSettings^.WriteFormatSettings, True);
    stTime:
      Result := DateTimeToUnicodeSQLTime(ClientVarManager.GetAsDateTime(InParamValues[ParamIndex]),
        ConSettings^.WriteFormatSettings, True);
    stTimestamp:
      Result := DateTimeToUnicodeSQLTimeStamp(ClientVarManager.GetAsDateTime(InParamValues[ParamIndex]),
        ConSettings^.WriteFormatSettings, True);
    stAsciiStream, stUnicodeStream, stBinaryStream:
      begin
        TempBlob := ClientVarManager.GetAsInterface(InParamValues[ParamIndex]) as IZBlob;
        if not TempBlob.IsEmpty then
          if InParamTypes[ParamIndex] = stBinaryStream
          then Result := GetSQLHexWideString(PAnsichar(TempBlob.GetBuffer), TempBlob.Length, True)
          else if TempBlob.IsClob then begin
            Tmp := TempBlob.GetUnicodeString;
            //CharRec.P := TempBlob.GetPWideChar;
            //Result := SQLQuotedStr(CharRec.P, TempBlob.Length, WideChar(#39));
            Result := SQLQuotedStr(Tmp, WideChar(#39));
          end else RaiseUnsupportedException
        else goto SetNull;
        TempBlob := nil;
      end;
    else
      RaiseUnsupportedParameterTypeException(InParamTypes[ParamIndex]);
  end;
end;

procedure TZAdoEmulatedPreparedStatement.ReleaseConnection;
begin
  inherited;
  FAdoConnection := nil;
end;

{$ENDIF ZEOS_DISABLE_ADO}
end.
