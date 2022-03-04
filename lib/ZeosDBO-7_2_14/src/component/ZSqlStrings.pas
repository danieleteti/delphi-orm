{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               SQL Query Strings component               }
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

unit ZSqlStrings;

interface

{$I ZComponent.inc}

uses
  Types, Classes, SysUtils, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs{$ELSE}ZClasses{$ENDIF},
  ZDbcIntfs, ZTokenizer, ZGenericSqlToken, ZCompatibility;

type
  {** Represents a SQL statement description object. }
  TZSQLStatement = class (TObject)
  private
    FSQL: string;
    FParamIndices: TIntegerDynArray;
    FParams: TStrings;
    FParamNamesArray: TStringDynArray;

    function GetParamCount: Integer;
    function GetParamName(Index: Integer): string;
    function GetParamNamesArray: TStringDynArray;
  public
    constructor Create(const SQL: string; const ParamIndices: TIntegerDynArray;
      Params: TStrings);
    property SQL: string read FSQL;
    property ParamCount: Integer read GetParamCount;
    property ParamNames[Index: Integer]: string read GetParamName;
    property ParamIndices: TIntegerDynArray read FParamIndices;
    property ParamNamesArray: TStringDynArray read FParamNamesArray;
  end;

  {** Imlements a string list with SQL statements. }

  { TZSQLStrings }

  TZSQLStrings = class (TStringList)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}FDataset: TComponent;
    FParamCheck: Boolean;
    FStatements: TObjectList;
    FParams: TStringList;
    FMultiStatements: Boolean;
    FParamChar: Char;

    function GetParamCount: Integer;
    function GetParamName(Index: Integer): string;
    function GetStatement(Index: Integer): TZSQLStatement;
    function GetStatementCount: Integer;
    function GetTokenizer: IZTokenizer;
    procedure SetDataset(Value: TComponent);
    procedure SetParamCheck(Value: Boolean);
    procedure SetParamChar(Value: Char);
    procedure SetMultiStatements(Value: Boolean);
  protected
    procedure Changed; override;
    function FindParam(const ParamName: string): Integer;
    procedure RebuildAll;
    procedure SetTextStr(const Value: string); override;
  public
    constructor Create;
    destructor Destroy; override;

    property Dataset: TComponent read FDataset write SetDataset;
    property ParamCheck: Boolean read FParamCheck write SetParamCheck;
    property ParamCount: Integer read GetParamCount;
    property ParamChar: Char read FParamChar write SetParamChar;
    property ParamNames[Index: Integer]: string read GetParamName;
    property StatementCount: Integer read GetStatementCount;
    property Statements[Index: Integer]: TZSQLStatement read GetStatement;
    property MultiStatements: Boolean read FMultiStatements
      write SetMultiStatements;
  end;

implementation

uses ZMessages, ZAbstractRODataset, ZDatasetUtils, ZSqlProcessor;

{ TZSQLStatement }

{**
  Creates a SQL statement object and assignes the main properties.
  @param SQL a SQL statement.
  @param ParamIndices a parameter indices.
  @param Params a list with all parameter names.
}
constructor TZSQLStatement.Create(const SQL: string;
  const ParamIndices: TIntegerDynArray; Params: TStrings);
begin
  FSQL := SQL;
  FParamIndices := ParamIndices;
  FParams := Params;
  FParamNamesArray := GetParamNamesArray;
end;

{**
  Gets a parameters count for this statement.
  @return a parameters count.
}
function TZSQLStatement.GetParamCount: Integer;
begin
  if Assigned(FParamIndices) then
    Result := High(FParamIndices) - Low(FParamIndices) + 1
  else Result := 0;
end;

{**
  Gets a parameter name by it's index inside the statement.
  @return a parameter name.
}
function TZSQLStatement.GetParamName(Index: Integer): string;
begin
  if Assigned(FParamIndices) then
    Result := FParams[FParamIndices[Index + Low(FParamIndices)]]
  else Result := '';
end;

{**
  Gets an array of parameter names.
  @return an array of parameter names.
}
function TZSQLStatement.GetParamNamesArray: TStringDynArray;
var
  I: Integer;
begin
  SetLength(Result, High(FParamIndices) - Low(FParamIndices) + 1);
  for I := Low(Result) to High(Result) do
    Result[I] := FParams[FParamIndices[I + Low(FParamIndices)]];
end;

{ TZSQLStrings }

{**
  Creates a SQL strings object and assigns the main properties.
}
constructor TZSQLStrings.Create;
begin
  inherited Create; { -> needed to run the TestSuite else Inheritance(Self).Methods fails}
  FParams := TStringList.Create;
  FParamCheck := True;
  FStatements := TObjectList.Create;
  FMultiStatements := True;
  FParamChar :=':';
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSQLStrings.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FStatements);
  FDataSet := nil;
  inherited Destroy;
end;

{**
  Gets a parameter count.
  @return a count of SQL parameters.
}
function TZSQLStrings.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

{**
  Gets parameter name by it's index.
  @param Index a parameter index.
  @return a parameter name.
}
function TZSQLStrings.GetParamName(Index: Integer): string;
begin
  Result := FParams[Index];
end;

{**
  Gets a SQL statements count.
  @return a SQL statements count.
}
function TZSQLStrings.GetStatementCount: Integer;
begin
  Result := FStatements.Count;
end;

function TZSQLStrings.GetTokenizer: IZTokenizer;
var
  Driver: IZDriver;
begin
  { Defines a SQL specific tokenizer object. }
  Result := nil;
  if FDataset is TZAbstractRODataset then
  begin
    if Assigned(TZAbstractRODataset(FDataset).Connection) then
    begin
      Driver := TZAbstractRODataset(FDataset).Connection.DbcDriver;
      if Assigned(Driver) then
        Result := Driver.GetTokenizer;
    end;
  end
  else if FDataset is TZSQLProcessor then
    if Assigned(TZSQLProcessor(FDataset).Connection) then
    begin
      Driver := TZSQLProcessor(FDataset).Connection.DbcDriver;
      if Assigned(Driver) then
        Result := Driver.GetTokenizer;
    end;
  if Result = nil then
    Result := TZGenericSQLTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Gets a SQL statement by it's index.
  @param Index a SQL statement index.
  @return a SQL statement object.
}
function TZSQLStrings.GetStatement(Index: Integer): TZSQLStatement;
begin
  Result := TZSQLStatement(FStatements[Index]);
end;

{**
  Sets a new ParamCheck value.
  @param Value a new ParamCheck value.
}
procedure TZSQLStrings.SetParamCheck(Value: Boolean);
begin
  if FParamCheck <> Value then
  begin
    FParamCheck := Value;
    RebuildAll;
  end;
end;

procedure TZSQLStrings.SetTextStr(const Value: string);
begin
  if Value <> Text then //prevent rebuildall if nothing changed see:
    inherited SetTextStr(Value);
end;

{**
  Sets a new ParamChar value.
  @param Value a new ParamCheck value.
}
procedure TZSQLStrings.SetParamChar(Value: Char);
begin
  if FParamChar <> Value then
  begin
    If not(GetTokenizer.GetCharacterState(Value) is TZSymbolstate) Then
      raise EZDatabaseError.Create(cSIncorrectParamChar+' : '+Value);
    FParamChar := Value;
    RebuildAll;
  end;
end;

{**
  Sets a new MultiStatements value.
  @param Value a new MultiStatements value.
}
procedure TZSQLStrings.SetMultiStatements(Value: Boolean);
begin
  if FMultiStatements <> Value then
  begin
    FMultiStatements := Value;
    RebuildAll;
  end;
end;

{**
  Sets a new correspondent dataset object.
  @param Value a new dataset object.
}
procedure TZSQLStrings.SetDataset(Value: TComponent);
begin
  if FDataset <> Value then begin
    FDataset := Value;
    RebuildAll;
  end;
end;

{**
  Finds a parameter by it's name.
  @param ParamName a parameter name.
  @return an index of found parameters or -1 if nothing was found.
}
function TZSQLStrings.FindParam(const ParamName: string): Integer;
begin
  FParams.CaseSensitive := False;
  Result := FParams.IndexOf(ParamName);
end;

{**
  Rebuilds all SQL statements.
}
procedure TZSQLStrings.RebuildAll;
var
  Tokens: TStrings;
  TokenValue: string;
  TokenType: TZTokenType;
  TokenIndex: Integer;
  ParamIndex: Integer;
  ParamIndices: TIntegerDynArray;
  ParamIndexCount: Integer;
  ParamName, SQL: string;
  Tokenizer: IZTokenizer;

  procedure NextToken;
  begin
    TokenType := TZTokenType({$IFDEF FPC}Pointer({$ENDIF}
      Tokens.Objects[TokenIndex]{$IFDEF FPC}){$ENDIF});
    TokenValue := Tokens[TokenIndex];
    Inc(TokenIndex);
  end;

begin
  if (FDataset = nil) or not (csLoading in FDataset.ComponentState) then
    FParams.Clear;
  SQL := Text;
  ParamName := Trim(SQL);
  { Optimization for empty query. }
  if ParamName = '' then
    Exit;


  FStatements.Clear;
  SQL := '';
  ParamIndexCount := 0;
  SetLength(ParamIndices, ParamIndexCount);
  

  { Optimization for single query without parameters. }
  if (not FParamCheck or (Pos(FParamChar, Text) = 0))
    and (not FMultiStatements or (Pos(';', Text) = 0)) then
  begin
    FStatements.Add(TZSQLStatement.Create(Text, ParamIndices, FParams));
    Exit;
  end;

  Tokenizer := GetTokenizer;
  Tokens := Tokenizer.TokenizeBufferToList(Text, [toSkipComments, toUnifyWhitespaces]);
  try
    TokenIndex := 0;
    repeat
      NextToken;
      { Processes parameters. }
      if ParamCheck and (TokenValue = FParamChar) then
      begin
        NextToken;
        if (TokenType <> ttEOF) and (TokenValue <> FParamChar) then
        begin
          { Check for correct parameter type. }
          if not (TokenType in [ttWord, ttQuoted, ttQuotedIdentifier, ttKeyWord, ttInteger]) then
            raise EZDatabaseError.Create(SIncorrectToken);

          SQL := SQL + '?';

          ParamName := TokenValue;
          if (ParamName <> '') and CharInSet(ParamName[1], [#39, '`', '"', '[']) then
            ParamName := Tokenizer.GetQuoteState.DecodeString(ParamName, ParamName[1]);

          ParamIndex := FindParam(ParamName);
          if ParamIndex < 0 then
            ParamIndex := FParams.Add(ParamName);

          Inc(ParamIndexCount);
          SetLength(ParamIndices, ParamIndexCount);
          ParamIndices[ParamIndexCount - 1] := ParamIndex;

          Continue;
        end;
      end;

      { Adds a DML statement. }
      if (TokenType = ttEOF) or (FMultiStatements and (TokenValue = ';')) then
      begin
        SQL := Trim(SQL);
        if SQL <> '' then
          FStatements.Add(TZSQLStatement.Create(SQL, ParamIndices, FParams));

        SQL := '';
        ParamIndexCount := 0;
        SetLength(ParamIndices, ParamIndexCount);
      end
      { Adds a default token. }
      else
        SQL := SQL + TokenValue;
    until TokenType = ttEOF;
  finally
    Tokens.Free;
  end;
end;

{**
  Performs action when the content of this string list is changed.
}
procedure TZSQLStrings.Changed;
begin
  if UpdateCount = 0 then
    RebuildAll;
  inherited Changed;
end;

end.
