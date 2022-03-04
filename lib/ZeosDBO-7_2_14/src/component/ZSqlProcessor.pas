{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Unidatabase SQLProcessor component          }
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

unit ZSqlProcessor;

interface

{$I ZComponent.inc}

uses Types, Classes, SysUtils, {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF},
  ZDbcIntfs, ZAbstractConnection, ZScriptParser, ZSqlStrings, ZCompatibility;

type

  {** Forward definition of TZSQLProcessor. }
  TZSQLProcessor = class;

  {** Defines an error handle action. }
  TZErrorHandleAction = (eaFail, eaAbort, eaSkip, eaRetry);

  {** Defines an Processor notification event. }
  TZProcessorNotifyEvent = procedure(Processor: TZSQLProcessor;
    StatementIndex: Integer) of object;

  {** Defines an Processor error handling event. }
  TZProcessorErrorEvent = procedure(Processor: TZSQLProcessor;
    StatementIndex: Integer; E: Exception;
    var ErrorHandleAction: TZErrorHandleAction) of object;

  {**
    Implements a unidatabase component which parses and executes SQL Scripts.
  }

  { TZSQLProcessor }

  TZSQLProcessor = class (TComponent)
  private
    FParams: TParams;
    FScript: TZSQLStrings;
    FScriptParser: TZSQLScriptParser;
    FConnection: TZAbstractConnection;
    FBeforeExecute: TZProcessorNotifyEvent;
    FAfterExecute: TZProcessorNotifyEvent;
    FOnError: TZProcessorErrorEvent;

    procedure SetParams(Value: TParams);
    function GetScript: TStrings;
    procedure SetScript(Value: TStrings);
    function GetStatementCount: Integer;
    function GetStatement(Index: Integer): string;
    procedure SetConnection(Value: TZAbstractConnection);
    function GetDelimiterType: TZDelimiterType;
    procedure SetDelimiterType(Value: TZDelimiterType);
    function GetDelimiter: string;
    procedure SetDelimiter(const Value: string);
    function GetCleanupStatements: Boolean;
    procedure SetCleanupStatements(const Value: Boolean);

    function GetParamCheck: Boolean;
    procedure SetParamCheck(Value: Boolean);
    function GetParamChar: Char;
    procedure SetParamChar(Value: Char);
    procedure UpdateSQLStrings({%H-}Sender: TObject);
  protected
    procedure CheckConnected;
    function DoOnError(StatementIndex: Integer; E: Exception):
      TZErrorHandleAction;
    procedure DoBeforeExecute(StatementIndex: Integer);
    procedure DoAfterExecute(StatementIndex: Integer);

    function CreateStatement(const SQL: string; Properties: TStrings):
      IZPreparedStatement; virtual;
    procedure SetStatementParams(Statement: IZPreparedStatement;
      const ParamNames: TStringDynArray; Params: TParams); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream);
    procedure LoadFromFile(const FileName: string);

    procedure Execute;
    procedure Parse;
    procedure Clear;

    function ParamByName(const Value: string): TParam;

    property StatementCount: Integer read GetStatementCount;
    property Statements[Index: Integer]: string read GetStatement;
  published
    property ParamCheck: Boolean read GetParamCheck write SetParamCheck
      default True;
    property ParamChar: Char read GetParamChar write SetParamChar
      default ':';
    property Params: TParams read FParams write SetParams;
    property Script: TStrings read GetScript write SetScript;
    property Connection: TZAbstractConnection read FConnection write SetConnection;
    property DelimiterType: TZDelimiterType read GetDelimiterType
      write SetDelimiterType default dtDefault;
    property Delimiter: string read GetDelimiter write SetDelimiter;
    property CleanupStatements: Boolean read GetCleanupStatements
      write SetCleanupStatements default False; 
    property OnError: TZProcessorErrorEvent read FOnError write FOnError;
    property AfterExecute: TZProcessorNotifyEvent read FAfterExecute write FAfterExecute;
    property BeforeExecute: TZProcessorNotifyEvent read FBeforeExecute write FBeforeExecute;
  end;

implementation

uses ZMessages, ZDbcUtils, ZAbstractRODataset, ZDatasetUtils;

{ TZSQLProcessor }

{**
  Creates this Processor component and assignes the main properties.
  @param AOwner an owner component.
}
constructor TZSQLProcessor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FParams := TParams.Create(Self);
  FScript := TZSQLStrings.Create;
  FScript.Dataset := Self;
  FScript.OnChange := UpdateSQLStrings;
  FScriptParser := TZSQLScriptParser.Create;
  FScriptParser.DelimiterType := dtDefault;
  FScriptParser.Delimiter := ';';
  FScriptParser.CleanupStatements := False;
end;

{**
  Destroys this component and cleanups the memory.
}
destructor TZSQLProcessor.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FScript);
  FreeAndNil(FScriptParser);
  FConnection := nil;
  inherited Destroy;
end;

{**
  Gets a parsed statement by it's index.
  @return a SQL statement.
}
function TZSQLProcessor.GetStatement(Index: Integer): string;
begin
  if (FScriptParser.UncompletedStatement <> '')
    and (Index = FScriptParser.StatementCount) then
    Result := FScriptParser.UncompletedStatement
  else Result := FScriptParser.Statements[Index];
end;

{**
  Gets a statements count.
  @return a number of parsed statements.
}
function TZSQLProcessor.GetStatementCount: Integer;
begin
  Result := FScriptParser.StatementCount;
  if FScriptParser.UncompletedStatement <> '' then
    Inc(Result);
end;

{**
  Sets a new SQL connection component.
  @param Value am SQL connection component.
}
procedure TZSQLProcessor.SetConnection(Value: TZAbstractConnection);
begin
  if FConnection <> Value then
  begin
    FConnection := Value;
    FScriptParser.ClearUncompleted;
  end;
end;

{**
  Gets a script delimiter type;
}
function TZSQLProcessor.GetDelimiterType: TZDelimiterType;
begin
  Result := FScriptParser.DelimiterType;
end;

{**
  Sets a new Processor delimiter type.
  @param Value a new Processor delimiter type.
}
procedure TZSQLProcessor.SetDelimiterType(Value: TZDelimiterType);
begin
  if FScriptParser.DelimiterType <> Value then
  begin
    FScriptParser.DelimiterType := Value;
    FScriptParser.ClearUncompleted;
  end;
end;

{**
  Gets a script delimiter;
}
function TZSQLProcessor.GetDelimiter: string;
begin
  Result := FScriptParser.Delimiter;
end;

{**
  Sets a new Processor delimiter.
  @param Value a new Processor delimiter.
}
procedure TZSQLProcessor.SetDelimiter(const Value: string);
begin
  if FScriptParser.Delimiter <> Value then
  begin
    FScriptParser.Delimiter := Value;
    FScriptParser.ClearUncompleted;
  end;
end;

{**
  Sets a new set of parameters.
  @param Value a set of parameters.
}
procedure TZSQLProcessor.SetParams(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

{**
  Sets a new SQL script.
  @param Value a new SQL script.
}
procedure TZSQLProcessor.SetScript(Value: TStrings);
begin
  FScript.Assign(Value);
  FScriptParser.ClearUncompleted;
end;

{**
  Checks is the database connection assignes and tries to connect.
}
procedure TZSQLProcessor.CheckConnected;
begin
  if Connection = nil then
    raise EZDatabaseError.Create(SConnectionIsNotAssigned);
  Connection.Connect;
end;

{**
  Clears Processor contents and all parsed statements.
}
procedure TZSQLProcessor.Clear;
begin
  FScript.Clear;
  FScriptParser.ClearUncompleted;
end;

{**
  Performs OnError Event and returns an error handle action.
  @param StatementIndex an index of the statement which failt.
  @param E an exception object.
  @return an error handle action.
}
function TZSQLProcessor.DoOnError(StatementIndex: Integer;
  E: Exception): TZErrorHandleAction;
begin
  Result := eaFail;
  if Assigned(FOnError) then
    FOnError(Self, StatementIndex, E, Result);
end;

{**
  Performs an action before execute a statement.
  @param StatementIndex an index of the executing statement.
}
procedure TZSQLProcessor.DoBeforeExecute(StatementIndex: Integer);
begin
  if Assigned(FBeforeExecute) then
    FBeforeExecute(Self, StatementIndex);
end;

{**
  Performs an action action execute a statement.
  @param StatementIndex an index of the executing statement.
}
procedure TZSQLProcessor.DoAfterExecute(StatementIndex: Integer);
begin
  if Assigned(FAfterExecute) then
    FAfterExecute(Self, StatementIndex);
end;

{**
  Loads a SQL Processor from the local file.
  @param FileName a name of the file.
}
procedure TZSQLProcessor.LoadFromFile(const FileName: string);
begin
  FScript.LoadFromFile(FileName);
end;

{**
  Loads a SQL Processor from the stream.
  @param Stream a stream object.
}
procedure TZSQLProcessor.LoadFromStream(Stream: TStream);
begin
  FScript.LoadFromStream(Stream);
end;

{**
  Executes a parsed SQL Processor.
}
procedure TZSQLProcessor.Execute;
var
  I: Integer;
  Statement: IZPreparedStatement;
  Action: TZErrorHandleAction;
  SQL: TZSQLStrings;
begin
  if Connection = nil then
    raise EZDatabaseError.Create(SConnectionIsNotAssigned);

  FConnection.ShowSQLHourGlass;
  try
    SQL := TZSQLStrings.Create;
    SQL.Dataset := Self;
    SQL.ParamCheck := FScript.ParamCheck;
    SQL.MultiStatements := False;
    Parse;

    for I := 0 to Pred(StatementCount) do
    begin
      Action := eaSkip;
      DoBeforeExecute(I);
      repeat
        try
          SQL.Text := GetStatement(I);
{http://zeos.firmos.at/viewtopic.php?t=2885&start=0&postdays=0&postorder=asc&highlight=}
          if SQL.StatementCount > 0 then
            begin
              Statement := CreateStatement(SQL.Statements[0].SQL, nil);
              SetStatementParams(Statement, SQL.Statements[0].ParamNamesArray,
                FParams);
              Statement.ExecuteUpdatePrepared;
            end;
          Statement.Close; //see test Test1049821: if LastResultSet is assigned
          Statement := nil;
        except
          on E: Exception do
          begin
            Statement := nil;
            Action := DoOnError(I, E);
            if Action = eaFail then
              RaiseSQLException(E)
            else if Action = eaAbort then
              Exit;
          end;
        end;
      until Action <> eaRetry;
      DoAfterExecute(I);

    end;
  finally
    FreeAndNil(SQL);
    Connection.HideSQLHourGlass;
  end;
end;

{**
  Gets a SQL parameter by its name.
  @param Value a parameter name.
  @return a found parameter object.
}
function TZSQLProcessor.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

{**
  Parses the loaded SQL Processor.
}
procedure TZSQLProcessor.Parse;
begin
  CheckConnected;
  FScriptParser.Tokenizer := Connection.DbcDriver.GetTokenizer;
// mdaems 20060429 : Clear would reset the delimiter of the scriptparser
//  FScriptParser.Clear;
  FScriptParser.ClearUncompleted;
  FScriptParser.ParseText(FScript.Text);
end;

{**
  Creates a DBC statement for the query.
  @param SQL an SQL query.
  @param Properties a statement specific properties.
  @returns a created DBC statement.
}
function TZSQLProcessor.CreateStatement(const SQL: string;
  Properties: TStrings): IZPreparedStatement;
begin
  Result := FConnection.DbcConnection.PrepareStatementWithParams(SQL, Properties);
end;

{**
  Fill prepared statement with parameters.
  @param Statement a prepared SQL statement.
  @param ParamNames an array of parameter names.
  @param Params a collection of SQL parameters.
}
procedure TZSQLProcessor.SetStatementParams(Statement: IZPreparedStatement;
  const ParamNames: TStringDynArray; Params: TParams);
var
  I: Integer;
  TempParam, Param: TParam;
begin
  TempParam := TParam.Create(nil);
  try
    for I := Low(ParamNames) to High(ParamNames) do
    begin
      Param := Params.FindParam(ParamNames[I]);
      if not Assigned(Param) or (Param.ParamType in [ptOutput, ptResult]) then
        Continue;
      SetStatementParam(I+FirstDbcIndex, Statement, Param);
    end;
  finally
    TempParam.Free;
  end;
end;

{**
  Gets the SQL script.
  @return the SQL script strings.
}
function TZSQLProcessor.GetScript: TStrings;
begin
  Result := FScript;
end;

{**
  Updates parameters from SQL statement.
  @param Sender an event sender object.
}
procedure TZSQLProcessor.UpdateSQLStrings(Sender: TObject);
var
  I: Integer;
  OldParams: TParams;
begin
  OldParams := TParams.Create;
  OldParams.Assign(FParams);
  FParams.Clear;

  try
    for I := 0 to FScript.ParamCount - 1 do
      FParams.CreateParam(ftUnknown, FScript.ParamNames[I], ptUnknown);
    FParams.AssignValues(OldParams);
  finally
    OldParams.Free;
  end;
end;

{**
  Gets a parameters check value.
  @return a parameters check value.
}
function TZSQLProcessor.GetParamCheck: Boolean;
begin
  Result := FScript.ParamCheck;
end;

{**
  Sets a new parameters check value.
  @param Value a parameters check value.
}
procedure TZSQLProcessor.SetParamCheck(Value: Boolean);
begin
  FScript.ParamCheck := Value;
  UpdateSQLStrings(Self);
end;

{**
  Gets a parameters marker.
  @return a parameter marker.
}
function TZSQLProcessor.GetParamChar: Char;
begin
  Result := FScript.ParamChar;
end;

{**
  Sets a new parameter marker.
  @param Value a parameter marker.
}
procedure TZSQLProcessor.SetParamChar(Value: Char);
begin
  FScript.ParamChar := Value;
  UpdateSQLStrings(Self);
end;

function TZSQLProcessor.GetCleanupStatements: Boolean;
begin
  Result := FScriptParser.CleanupStatements;
end;

procedure TZSQLProcessor.SetCleanupStatements(const Value: Boolean);
begin
  if FScriptParser.CleanupStatements <> Value then
  begin
    FScriptParser.CleanupStatements := Value;
    FScriptParser.ClearUncompleted;
  end;
end;

end.

