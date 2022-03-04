{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       String tokenizing classes for Generic SQL         }
{                                                         }
{       Originally written by Sergey Seroukhov            }
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

unit ZGenericSqlToken;

interface

{$I ZParseSql.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZTokenizer, ZCompatibility;

type

  {**
    Implements a number state object.
    Depending on the FSupportsHex flag it could read hex values.
    It is base abstract class that shouldn't be used.
  }
  TZGenericBaseNumberState = class (TZNumberState)
  private
    FSupportsHex: Boolean;
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  TZGenericSQLNoHexNumberState = class (TZGenericBaseNumberState)
  public
    constructor Create;
  end;

  TZGenericSQLHexNumberState = class (TZGenericBaseNumberState)
  public
    constructor Create;
  end;

  {** Implements a symbol state object. }
  TZGenericSQLSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZGenericSQLWordState = class (TZWordState)
  public
    constructor Create;

    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a quote string state object. }
  TZGenericSQLQuoteState = class (TZQuoteState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      {%H-}Tokenizer: TZTokenizer): TZToken; override;

    function EncodeString(const Value: string; QuoteChar: Char): string; override;
    function DecodeString(const Value: string; QuoteChar: Char): string; override;
  end;

  {** Implements a comment state object.
    Processes common SQL comments like -- and /* */
  }
  TZGenericSQLCommentState = class (TZCppCommentState)
  public
    function NextToken(Stream: TStream; FirstChar: Char;
      Tokenizer: TZTokenizer): TZToken; override;
  end;

  {** Implements a default tokenizer object. }
  TZGenericSQLTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

implementation

{$IFDEF FAST_MOVE}uses ZFastCode;{$ENDIF}

{ TZGenericBaseNumberState }

function TZGenericBaseNumberState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  LastChar: Char;

  // Uses external variables: Stream, LastChar, Result.Value
  procedure ReadDecDigits;
  begin
    LastChar := #0;
    while Stream.Read(LastChar, SizeOf(Char)) > 0 do
      if (Ord(LastChar) >= Ord('0')) and (Ord(LastChar) <= Ord('9')) then
      begin
        ToBuf(LastChar, Result.Value);
        LastChar := #0;
      end
      else
      begin
        Stream.Seek(-SizeOf(Char), soFromCurrent);
        Break;
      end;
  end;

  // Uses external variables: Stream, LastChar, Result.Value
  procedure ReadHexDigits;
  begin
    LastChar := #0;
    while Stream.Read(LastChar, SizeOf(Char)) > 0 do
      if CharInSet(LastChar, ['0'..'9','a'..'f','A'..'F']) then
      begin
        ToBuf(LastChar, Result.Value);
        LastChar := #0;
      end
      else
      begin
        Stream.Seek(-SizeOf(Char), soFromCurrent);
        Break;
      end;
  end;

  // Uses external variables: Stream, LastChar, Result.Value
  procedure ReadExp;
  begin
    Stream.Read(LastChar, SizeOf(Char));
    ToBuf(LastChar, Result.Value);

    Stream.Read(LastChar, SizeOf(Char));
    if CharInSet(LastChar, ['0'..'9','-','+']) then
    begin
      ToBuf(LastChar, Result.Value);
      ReadDecDigits;
    end
    else
    begin
      FlushBuf(Result.Value);
      SetLength(Result.Value, Length(Result.Value) - 1);
      Stream.Seek(-2*SizeOf(Char), soFromCurrent);
    end;
  end;

var
  HexDecimal: Boolean;
  FloatPoint: Boolean;
begin
  HexDecimal := False;
  FloatPoint := FirstChar = '.';
  LastChar := #0;

  Result.Value := '';
  InitBuf(FirstChar);
  Result.TokenType := ttUnknown;

  { Reads the first part of the number before decimal point }
  if not FloatPoint then
  begin
    ReadDecDigits;
    FloatPoint := (LastChar = '.');
    if FloatPoint then
    begin
      Stream.Read(LastChar, SizeOf(Char));
      ToBuf(LastChar, Result.Value);
    end;
  end;

  { Reads the second part of the number after decimal point }
  if FloatPoint then
    ReadDecDigits;

  { Reads a power part of the number }
  if (Ord(LastChar) or $20) = ord('e') then //CharInSet(LastChar, ['e','E']) then
  begin
    FloatPoint := True;
    ReadExp;
  end;

  { Reads the hexadecimal number }
  if FSupportsHex then
  begin
    if (Result.Value = '') and (FirstChar = '0') and
      ((Ord(LastChar) or $20) = ord('x')) then //CharInSet(LastChar, ['x','X']) then
    begin
      Stream.Read(LastChar, SizeOf(Char));
      ToBuf(LastChar, Result.Value);
      ReadHexDigits;
      HexDecimal := True;
    end;
  end;
  FlushBuf(Result.Value);

  { Prepare the result }
  if Result.Value = '.' then
  begin
    if Tokenizer.SymbolState <> nil then
      Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer);
  end
  else if HexDecimal then
    Result.TokenType := ttHexDecimal
  else if FloatPoint then
    Result.TokenType := ttFloat
  else
    Result.TokenType := ttInteger;
end;

{ TZGenericSQLNoHexNumberState }

constructor TZGenericSQLNoHexNumberState.Create;
begin
  inherited;
  FSupportsHex := False;
end;

{ TZGenericSQLHexNumberState }

constructor TZGenericSQLHexNumberState.Create;
begin
  inherited;
  FSupportsHex := True;
end;

{ TZGenericSQLSymbolState }

{**
  Creates this SQL-specific symbol state object.
}
constructor TZGenericSQLSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('<<');
  Add('>>');
end;

{ TZGenericSQLWordState }

{**
  Constructs this SQL-specific word state object.
}
constructor TZGenericSQLWordState.Create;
begin
  SetWordChars(#0, #191, False);
  SetWordChars(#192, high(char), True);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('$', '$', True);
  SetWordChars('_', '_', True);
end;

const
  {** List of keywords. }
  Keywords: array [0..8] of string = (
    'AND','OR','NOT','XOR','LIKE','IS','NULL','TRUE','FALSE'
  );

{**
  Gets a word tokens or special operators.
  @return a processed token.
}
function TZGenericSQLWordState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  I: Integer;
  Temp: string;
begin
  Result := inherited NextToken(Stream, FirstChar, Tokenizer);
  Temp := UpperCase(Result.Value);

  for I := Low(Keywords) to High(Keywords) do
    if Temp = Keywords[I] then begin
      Result.TokenType := ttKeyword;
      Break;
    end;
end;


{ TZGenericSQLQuoteState }

{**
  Return a quoted string token from a reader. This method
  will collect characters until it sees a match to the
  character that the tokenizer used to switch to this state.

  @return a quoted string token from a reader
}
function TZGenericSQLQuoteState.NextToken(Stream: TStream;
  FirstChar: Char; Tokenizer: TZTokenizer): TZToken;
var
  ReadChar, LastChar: Char;
  ReadCounter, NumericCounter, TimeSepCount, DateSepCount, SpaceCount: integer;
begin
  Result.Value := '';
  InitBuf(FirstChar);
  LastChar := #0;
  TimeSepCount := 0;
  DateSepCount := 0;
  SpaceCount := 0;
  ReadCounter := 0;
  NumericCounter := 0;

  while Stream.Read(ReadChar{%H-}, SizeOf(Char)) > 0 do
  begin
    if (LastChar = FirstChar) and (ReadChar <> FirstChar) then
    begin
      Stream.Seek(-SizeOf(Char), soFromCurrent);
      Break;
    end;
    inc(TimeSepCount, Ord(ReadChar = {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}TimeSeparator));
    inc(DateSepCount, Ord(ReadChar = {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DateSeparator));
    inc(SpaceCount, Ord(ReadChar = ' '));
    inc(NumericCounter, Ord(Ord(ReadChar) in [Ord('0')..Ord('9')]));
    Inc(ReadCounter);

    ToBuf(ReadChar, Result.Value);
    if (LastChar = FirstChar) and (ReadChar = FirstChar)
    then LastChar := #0
    else LastChar := ReadChar;
  end;
  FlushBuf(Result.Value);

  if FirstChar = '"' then
    Result.TokenType := ttWord
  else Result.TokenType := ttQuoted;

  if (TimeSepCount = 2) and (DateSepCount = 0) and // test Time constant
    ((NumericCounter + TimeSepCount) = ReadCounter-1) then 
    try //D7 seems to make trouble here: TestDateTimeFilterExpression but why?? -> use a define instead
    //EH: Is this correct??? This method uses Formatsettings which may differ to given Format!
      if StrToTimeDef(DecodeString(Result.Value, FirstChar), 0) = 0 then
        Exit;
      Result.Value := DecodeString(Result.Value,'"');
      Result.TokenType := ttTime;
    except end
  else if (TimeSepCount = 0) and (DateSepCount = 2) and // test Date constant
    ((NumericCounter + DateSepCount) = ReadCounter-1) then 
    try //D7 seems to make trouble here: TestDateTimeFilterExpression but why?? -> use a define instead
      //EH: Is this correct??? This method uses Formatsettings which may differ to given Format!
      if StrToDateDef(DecodeString(Result.Value, FirstChar), 0) = 0 then
        Exit;
      Result.Value := DecodeString(Result.Value,'"');
      Result.TokenType := ttDate;
    except end
  else if (TimeSepCount = 2) and (DateSepCount = 2) and // test DateTime constant
    ((NumericCounter + TimeSepCount + DateSepCount + SpaceCount) = ReadCounter-1) then
    try //D7 seems to make trouble here: TestDateTimeFilterExpression but why?? -> use a define instead
      //EH: Is this correct??? This method uses Formatsettings which may differ to given Format!
      if StrToDateTimeDef(DecodeString(Result.Value, FirstChar), 0) = 0 then
        Exit;
      Result.Value := DecodeString(Result.Value,'"');
      Result.TokenType := ttDateTime;
    except end
end;

{**
  Encodes a string value.
  @param Value a string value to be encoded.
  @param QuoteChar a string quote character.
  @returns an encoded string.
}
function TZGenericSQLQuoteState.EncodeString(const Value: string;
  QuoteChar: Char): string;
begin
  if Ord(QuoteChar) in [Ord(#39), Ord('"'), Ord('`')]
  then Result := SQLQuotedStr(Value, QuoteChar)
  else Result := Value;
end;

{**
  Decodes a string value.
  @param Value a string value to be decoded.
  @param QuoteChar a string quote character.
  @returns an decoded string.
}
function TZGenericSQLQuoteState.DecodeString(const Value: string;
  QuoteChar: Char): string;
var
  Len: Integer;
  P: PChar;
begin
  Len := Length(Value);
  P := Pointer(Value);
  if (Len >= 2) and (Ord(QuoteChar) in [Ord(#39), Ord('"'), Ord('`')]) and
    (P^ = QuoteChar) and ((P+Len-1)^ = QuoteChar)
  then if Len > 2
    then Result := AnsiDequotedStr(Value, QuoteChar)
    else Result := ''
  else Result := Value;
end;

{ TZGenericSQLCommentState }

function TZGenericSQLCommentState.NextToken(Stream: TStream; FirstChar: Char;
  Tokenizer: TZTokenizer): TZToken;
var
  ReadChar: Char;
  ReadNum: Integer;
begin
  InitBuf(FirstChar);
  Result.Value := '';
  Result.TokenType := ttUnknown;

  case FirstChar of
    '-':
      begin
        ReadNum := Stream.Read(ReadChar{%H-}, SizeOf(Char));
        if ReadNum > 0 then
          if ReadChar = '-' then
          begin
            Result.TokenType := ttComment;
            ToBuf(ReadChar, Result.Value);
            GetSingleLineComment(Stream, Result.Value);
          end
          else
            Stream.Seek(-SizeOf(Char), soFromCurrent);
      end;
    '/':
      begin
        ReadNum := Stream.Read(ReadChar{%H-}, SizeOf(Char));
        if ReadNum > 0 then
          if ReadChar = '*' then
          begin
            Result.TokenType := ttComment;
            ToBuf(ReadChar, Result.Value);
            GetMultiLineComment(Stream, Result.Value);
          end
          else
            Stream.Seek(-SizeOf(Char), soFromCurrent);
      end;
  end;

  if (Result.TokenType = ttUnknown) and (Tokenizer.SymbolState <> nil) then
    Result := Tokenizer.SymbolState.NextToken(Stream, FirstChar, Tokenizer)
  else
    FlushBuf(Result.Value);
end;

{ TZGenericSQLTokenizer }

{**
  Constructs a default state table (as described in the class comment).
}
procedure TZGenericSQLTokenizer.CreateTokenStates;
begin
  NumberState := TZNumberState.Create;
  QuoteState := TZGenericSQLQuoteState.Create;
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

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);
  SetCharacterState('`', '`', QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

end.

