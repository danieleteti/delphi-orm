{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for Expressions        }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZExprToken;

interface

{$I ZCore.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZTokenizer;

type

  {** Implements an Expression-specific number state object. }
  TZExpressionNumberState = class (TZNumberState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements an Expression-specific quote string state object. }
  TZExpressionQuoteState = class (TZQuoteState)
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
  TZExpressionCommentState = class (TZCppCommentState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a symbol state object. }
  TZExpressionSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZExpressionWordState = class (TZWordState)
  public
    constructor Create;
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a default tokenizer object. }
  TZExpressionTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

implementation

uses ZCompatibility{$IFDEF FAST_MOVE},ZFastCode{$ENDIF};

const
  {** List of keywords. }
  Keywords: array [0..8] of string = (
    'AND','OR','NOT','XOR','LIKE','IS','NULL','TRUE','FALSE'
  );

{ TZExpressionNumberState }


//gto: all operations on Streams should be done without presuming the size
//     of the read var, like Stream.Read(LastChar, 1), to read 1 char
//
//     Instead, operations should use SizeOf(Type), like this:
//     Stream.Read(LastChar, SizeOf(Char))
//
//     This is unicode safe and ansi (Delphi under 2009) compatible

{**
  Return a number token from a reader.
  @return a number token from a reader
}
function TZExpressionNumberState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  TempChar: Char;
  FloatPoint: Boolean;
  LastChar: Char;

  function ReadDecDigits: string;
  begin
    Result := '';
    LastChar := #0;
    while Stream.Read(LastChar, SizeOf(Char)) > 0 do
    begin
      if ((Ord(LastChar) >= Ord('0')) and (Ord(LastChar) <= Ord('9'))) then
      begin
        Result := Result + LastChar;
        LastChar := #0;
      end
      else
      begin
        Stream.Seek(-SizeOf(Char), soFromCurrent);
        Break;
      end;
    end;
  end;

begin
  FloatPoint := FirstChar = '.';
  Result.Value := FirstChar;
  Result.TokenType := ttUnknown;
  LastChar := #0;

  { Reads the first part of the number before decimal point }
  if not FloatPoint then
  begin
    Result.Value := Result.Value + ReadDecDigits;
    FloatPoint := LastChar = '.';
    if FloatPoint then
    begin
      Stream.Read(TempChar{%H-}, SizeOf(Char));
      Result.Value := Result.Value + TempChar;
    end;
  end;

  { Reads the second part of the number after decimal point }
  if FloatPoint then
    Result.Value := Result.Value + ReadDecDigits;

  { Reads a power part of the number }
  if (Ord(LastChar) in [Ord('e'), Ord('E')]) then
  begin
    Stream.Read(TempChar, SizeOf(Char));
    Result.Value := Result.Value + TempChar;
    FloatPoint := True;

    Stream.Read(TempChar, SizeOf(Char));
    if Ord(TempChar) in [Ord('0')..Ord('9'), Ord('-'), Ord('+')] then
      Result.Value := Result.Value + TempChar + ReadDecDigits
    else
    begin
      Result.Value := Copy(Result.Value, 1, Length(Result.Value) - 1);
      Stream.Seek(-(2 * SizeOf(Char)), soFromCurrent);
    end;
  end;

  { Prepare the result }
  if Result.Value = '.' then
  begin
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer);
  end
  else
  begin
    if FloatPoint then
      Result.TokenType := ttFloat
      else
         Result.TokenType := ttInteger;
  end;
end;

{ TZExpressionSQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZExpressionQuoteState.NextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  LastChar: Char;
begin
  if FirstChar = '"' then
    Result.TokenType := ttWord
   else
      Result.TokenType := ttQuoted;
  Result.Value := FirstChar;
  LastChar := #0;

  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    if (LastChar = FirstChar) and (ReadChar <> FirstChar) then
    begin
      Stream.Seek(-SizeOf(Char), soFromCurrent);
      Break;
    end;
    Result.Value := Result.Value + ReadChar;
    if LastChar = '\' then
      LastChar := #0
    else if (LastChar = FirstChar) and (ReadChar = FirstChar) then
      LastChar := #0
      else
         LastChar := ReadChar;
  end;
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZExpressionQuoteState.EncodeString(const Value: string;
  QuoteChar: Char): string;
begin
  if (Ord(QuoteChar) in [Ord(''''), Ord('"')]) then
    Result := QuoteChar + EncodeCString(Value) + QuoteChar
  else
    Result := Value;
end;

{**
  Decodes a string value.
  @param Value a string value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZExpressionQuoteState.DecodeString(const Value: string;
  QuoteChar: Char): string;
begin
  if (Length(Value) >= 2) and (Ord(QuoteChar) in [Ord(''''), Ord('"')])
     and (Value[1] = QuoteChar) and (Value[Length(Value)] = QuoteChar) then
    Result := DecodeCString(Copy(Value, 2, Length(Value) - 2))
   else
      Result := Value;
end;

{ TZExpressionCommentState }

{**
  Gets an Expression specific comments like /* */.
  @return either just a slash token, or the results of
    delegating to a comment-handling state
}
function TZExpressionCommentState.NextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  Result.TokenType := ttUnknown;
  InitBuf(Firstchar);
  Result.Value := '';

  if FirstChar = '/' then
  begin
    ReadNum := Stream.Read(ReadChar{%H-}, SizeOf(Char));
    if (ReadNum > 0) and (ReadChar = '*') then begin
      Result.TokenType := ttComment;
      ToBuf(ReadChar, Result.Value);
      GetMultiLineComment(Stream, Result.Value);
    end
    else
    begin
      if ReadNum > 0 then
        Stream.Seek(-SizeOf(Char), soFromCurrent);
    end;
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil) then
    Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer)
  else
    FlushBuf(Result.Value);
end;

{ TZExpressionSymbolState }

{**
  Creates this Expression-specific symbol state object.
}
constructor TZExpressionSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('!=');
end;

{ TZExpressionWordState }

{**
  Constructs this Expression-specific word state object.
}
constructor TZExpressionWordState.Create;
begin
  SetWordChars(#0, #191, False);
  SetWordChars(#192, high(char), True);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('_', '_', True);
end;

{**
  Gets a word tokens or special operators.
  @return a processed token.
}
function TZExpressionWordState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  I: Integer;
  Temp: string;
begin
  Result := inherited NextToken(Stream, FirstChar, Tokenizer);
  Temp := UpperCase(Result.Value);

  for I := Low(Keywords) to High(Keywords) do
  begin
    if Temp = Keywords[I] then
    begin
      Result.TokenType := ttKeyword;
      Break;
    end;
  end;
end;

{ TZExpressionTokenizer }

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZExpressionTokenizer.CreateTokenStates;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZExpressionSymbolState.Create;
  NumberState := TZExpressionNumberState.Create;
  QuoteState := TZExpressionQuoteState.Create;
  WordState := TZExpressionWordState.Create;
  CommentState := TZExpressionCommentState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState('''', '''', QuoteState);

  SetCharacterState('/', '/', CommentState);
end;

end.


