{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           String tokenizing classes for MySQL           }
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

unit ZMySqlToken;

interface

{$I ZParseSql.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZTokenizer, ZGenericSqlToken, ZCompatibility;

type

  {** Implements a MySQL-specific number state object. }
  TZMySQLNumberState = TZGenericSQLHexNumberState;

  {** Implements a MySQL-specific quote string state object. }
  TZMySQLQuoteState = class (TZQuoteState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;

    function EncodeString(const Value: string; QuoteChar: Char): string; override;
    function DecodeString(const Value: string; QuoteChar: Char): string; override;
  end;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZMySQLCommentState = class (TZCppCommentState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a symbol state object. }
  TZMySQLSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZMySQLWordState = class (TZGenericSQLWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZMySQLTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

{$ENDIF ZEOS_DISABLE_MYSQL}

implementation

{$IFNDEF ZEOS_DISABLE_MYSQL}

{$IFDEF FAST_MOVE}uses ZFastCode;{$ENDIF}

{ TZMySQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZMySQLQuoteState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
const BackSlash = Char('\');
var
  ReadChar: Char;
  LastChar: Char;
begin
  Result.Value := '';
  InitBuf(FirstChar);

  If FirstChar = '`' then
    Result.TokenType := ttQuotedIdentifier
  else
    Result.TokenType := ttQuoted;

  LastChar := #0;
  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    if (LastChar = FirstChar) and (ReadChar <> FirstChar) then begin
      Stream.Seek(-SizeOf(Char), soFromCurrent);
      Break;
    end;
    ToBuf(ReadChar, Result.Value);
    if LastChar = BackSlash then
      LastChar := #0
    else if (LastChar = FirstChar) and (ReadChar = FirstChar) then
      LastChar := #0
    else LastChar := ReadChar;
  end;
  FlushBuf(Result.Value);
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZMySQLQuoteState.EncodeString(const Value: string; QuoteChar: Char): string;
begin
  if (Ord(QuoteChar) in [Ord(#39), Ord('"'), Ord('`')]) then
    Result := QuoteChar + EncodeCString(Value) + QuoteChar
  else Result := Value;
end;

{**
  Decodes a string value.
  @param Value a string value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZMySQLQuoteState.DecodeString(const Value: string; QuoteChar: Char): string;
var
  Len: Integer;
begin
  Len := Length(Value);
  if (Len >= 2) and CharInSet(QuoteChar, [#39, '"', '`'])
    and (Value[1] = QuoteChar) and (Value[Len] = QuoteChar) then
  begin
    if Len > 2 then
      Result := DecodeCString(Copy(Value, 2, Len - 2))
    else Result := '';
  end
  else Result := Value;
end;

{ TZMySQLCommentState }

{**
  Gets a MySQL specific comments like # or /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZMySQLCommentState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum, ReadNum2: Integer;
begin
  Result.TokenType := ttUnknown;
  InitBuf(Firstchar);
  Result.Value := '';

  case FirstChar of
    '-': begin
        ReadNum := Stream.Read(ReadChar{%H-}, SizeOf(Char));
        if ReadNum > 0 then
         //MySQL sees the -- only if it's followed by a whitespace
         //intentenion was substract a negative numer (: like
         //UPDATE account SET credit=credit--1 funny isn't it
          if ReadChar = '-' then begin
            ToBuf(ReadChar, Result.Value);
            ReadNum := Stream.Read(ReadChar{%H-}, SizeOf(Char));
            if (ReadNum > 0) then
              if Ord(ReadChar) <= Ord(' ') then begin
                Result.TokenType := ttComment;
                ToBuf(ReadChar, Result.Value);
                GetSingleLineComment(Stream, Result.Value);
              end else
                Stream.Seek(-(SizeOf(Char) * 2), soFromCurrent)
          end else
            Stream.Seek(-SizeOf(Char), soFromCurrent)
      end;
    '#': begin
        Result.TokenType := ttComment;
        GetSingleLineComment(Stream, Result.Value);
      end;
    '/': begin
        ReadNum := Stream.Read(ReadChar, SizeOf(Char));
        if ReadNum > 0 then
          if ReadChar = '*' then begin
            ToBuf(ReadChar, Result.Value);
            ReadNum2 := Stream.Read(ReadChar, SizeOf(Char));
            // Don't treat '/*!' comments as normal comments!!
            if (ReadNum2 > 0) then begin
              ToBuf(ReadChar, Result.Value);
              if (ReadChar <> '!') then
                Result.TokenType := ttComment
              else
                Result.TokenType := ttSymbol;
              GetMultiLineComment(Stream, Result.Value);
            end;
          end else
            Stream.Seek(-SizeOf(Char), soFromCurrent);
      end;
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil) 
  then Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer)
  else FlushBuf(Result.Value);
end;

{ TZMySQLSymbolState }

{**
  Creates this MySQL-specific symbol state object.
}
constructor TZMySQLSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('<<');
  Add('>>');
  {BEGIN PATCH: added by fduenas}
  Add(':=');
  {END PATCH: added by fduenas}
end;

{ TZMySQLWordState }

{**
  Constructs this MySQL-specific word state object.
}
constructor TZMySQLWordState.Create;
begin
  SetWordChars(#0, #191, False);
  SetWordChars(#192, high(char), True);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('$', '$', True);
  SetWordChars('_', '_', True);
end;

{ TZMySQLTokenizer }

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZMySQLTokenizer.CreateTokenStates;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZMySQLSymbolState.Create;
  NumberState := TZMySQLNumberState.Create;
  QuoteState := TZMySQLQuoteState.Create;
  WordState := TZMySQLWordState.Create;
  CommentState := TZMySQLCommentState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);
  SetCharacterState('`', '`', QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('#', '#', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

{$ENDIF ZEOS_DISABLE_MYSQL}

end.

