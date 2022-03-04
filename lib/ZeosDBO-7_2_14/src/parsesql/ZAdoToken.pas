{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            String tokenizing classes for Ado            }
{                                                         }
{         Originally written by Sergey Seroukhov          }
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

unit ZAdoToken;

interface

{$I ZParseSql.inc}

{$IF not defined(MSWINDOWS)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ADO}

uses
  Classes, SysUtils, ZTokenizer, ZGenericSqlToken, ZCompatibility;

type
  {** Implements a quote string state object. }
  TZAdoSQLQuoteState = class (TZQuoteState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;

    function EncodeString(const Value: string; QuoteChar: Char): string; override;
    function DecodeString(const Value: string; QuoteChar: Char): string; override;
  end;

  {** Implements a default tokenizer object. }
  TZAdoSQLTokenizer = class (TZGenericSQLTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

{$ENDIF ZEOS_DISABLE_ADO}

implementation

{$IFNDEF ZEOS_DISABLE_ADO}

{$IFDEF FAST_MOVE}
uses ZFastCode;
{$ENDIF}

{ TZAdoSQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZAdoSQLQuoteState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  LastChar: Char;
begin
  Result.Value := '';
  InitBuf(FirstChar);
  LastChar := #0;
  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    if ((LastChar = FirstChar) and (ReadChar <> FirstChar)
      and (FirstChar <> '[')) or ((FirstChar = '[') and (LastChar = ']')) then
    begin
      Stream.Seek(-SizeOf(Char), soFromCurrent);
      Break;
    end;
    ToBuf(ReadChar, Result.Value);
    if (LastChar = FirstChar) and (ReadChar = FirstChar) then
      LastChar := #0
    else LastChar := ReadChar;
  end;
  FlushBuf(Result.Value);

  if (FirstChar = '"') or (FirstChar='[') then
    Result.TokenType := ttQuotedIdentifier
  else Result.TokenType := ttQuoted;
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZAdoSQLQuoteState.EncodeString(const Value: string; QuoteChar: Char): string;
begin
  if QuoteChar = '[' then
    Result := '[' + Value + ']'
  else if CharInSet(QuoteChar, [#39, '"']) then
    Result := QuoteChar + Value + QuoteChar
  else Result := Value;
end;

{**
  Decodes a string value.
  @param Value a string value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZAdoSQLQuoteState.DecodeString(const Value: string; QuoteChar: Char): string;
begin
  Result := Value;
  if Length(Value) >= 2 then
  begin
    if CharInSet(QuoteChar, [#39, '"']) and (Value[1] = QuoteChar)
      and (Value[Length(Value)] = QuoteChar) then
    begin
      if Length(Value) > 2 then
        Result := AnsiDequotedStr(Value, QuoteChar)
      else Result := '';
    end
    else if (QuoteChar = '[') and (Value[1] = QuoteChar)
      and (Value[Length(Value)] = ']') then
      Result := Copy(Value, 2, Length(Value) - 2)
  end;
end;


{ TZAdoSQLTokenizer }

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZAdoSQLTokenizer.CreateTokenStates;
begin
  NumberState := TZNumberState.Create;
  QuoteState := TZAdoSQLQuoteState.Create;
  WhitespaceState := TZWhitespaceState.Create;
  CommentState := TZCppCommentState.Create;

  SymbolState := TZGenericSQLSymbolState.Create;
  WordState := TZGenericSQLWordState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);
  SetCharacterState('@', '@', WordState);
  SetCharacterState('#', '#', WordState); //added for mssql temp tables

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('`', '`', QuoteState); //MySQL
  SetCharacterState('"', '"', QuoteState); //standard SQL
  SetCharacterState(#39, #39, QuoteState); //standard SQL
  SetCharacterState('[', '[', QuoteState); //ODBC syntax
  SetCharacterState(']', ']', QuoteState); //ODBC syntax

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;
{$ENDIF ZEOS_DISABLE_ADO}

end.
