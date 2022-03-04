{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                  Regular Expressions                    }
{                                                         }
{            Originally written by Sergey Seroukhov       }
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

unit ZMatchPattern;
{
  Author: Kevin Boylan
  Ported By: Sergey Seroukhov

  This code is meant to allow wildcard pattern matches.
  It is VERY useful for matching filename wildcard patterns.
  It allows unix grep-like pattern comparisons, for instance:

	?	   	Matches any single characer
	*	   	Matches any contiguous characters
	[abc]  	Matches a or b or c at that position
	[^abc]	Matches anything but a or b or c at that position
	[!abc]	Ditto
	[a-e]  	Matches a through e at that position

	'ma?ch.*'	-Would match match.exe, mavch.dat, march.on, etc
	'this [e-n]s a [!zy]est' - Would match 'this is a test',
                               but would not match 'this as a yest'

  This is a Delphi VCL translation from C code that was downloaded from CIS.
  C code was written by J. Kerceval and released to public domain 02/20/1991.
  This code is ofcourse also public domain. I would appreciate it if you would
  let me know if you find any bugs.  I would also appreciate any notes sent my
  way letting me know if you find it useful.
}

{$I ZCore.inc}

interface

uses SysUtils, ZCompatibility;

{ Check if Text equal to pattern }
function IsMatch(const Pattern, Text: string): Boolean;
function Like(const Pattern: ZWideString; PText: PWideChar; TLen: Cardinal): Boolean; overload;
function Like(const Pattern: RawByteString; PText: PAnsiChar; TLen: Cardinal): Boolean; overload;

implementation

const
{ Match defines }
  MATCH_PATTERN	  = 6;
  MATCH_LITERAL	  = 5;
  MATCH_RANGE     = 4;
  MATCH_ABORT     = 3;
  MATCH_END       = 2;
  MATCH_VALID     = 1;
  MATCH_PROCESSING = 0;
{ Pattern defines }
{  PATTERN_VALID  =  0;
  PATTERN_ESC     = -1;
  PATTERN_RANGE   = -2;
  PATTERN_CLOSE   = -3;
  PATTERN_EMPTY   = -4;
}{ Character defines }
  MATCH_CHAR_SINGLE             = '?';
  MATCH_CHAR_SINGLE_SQL         = '_';
  MATCH_CHAR_KLEENE_CLOSURE     = '*';
  MATCH_CHAR_MULTIPLE           = '%';
  MATCH_CHAR_RANGE_OPEN         = '[';
  MATCH_CHAR_RANGE              = '-';
  MATCH_CHAR_RANGE_CLOSE        = ']';
  MATCH_CHAR_CARET_NEGATE       = '^';
  MATCH_CHAR_EXCLAMATION_NEGATE = '!';

function Matche(PPattern, PLast, PText, TLast: PChar): Integer; forward;
function MatchSQL_W(PPattern, PLast, PText, TLast: PWideChar): Integer; forward;
function MatchSQL_A(PPattern, PLast, PText, TLast: PAnsiChar): Integer; forward;
function MatchAfterPercent_W(PPattern, PLast, PText, TLast: PWideChar): Integer; forward;
function MatchAfterPercent_A(PPattern, PLast, PText, TLast: PAnsiChar): Integer; forward;
function MatchAfterStar(PPattern, PLast, PText, TLast: PChar): Integer; forward;

function IsMatch(const Pattern, Text: string): Boolean;
var aPattern, aText: string;
  PPattern, PLast, PText, TLast: PChar;
begin
  {EH: Why is the (Ansi)LowerCase() required? We use it for the filter expressions only and the
    Strings(non Unicode) my have a different encoding!
    I would start from the premisse we match case-sensitive
    otherwise a explicit Lower() or Upper() using the filters should be done, IMHO
    The matche-method wasn't made for SQL }
  aPattern := AnsiLowerCase(Pattern);
  aText    := AnsiLowerCase(Text);
  PPattern := Pointer(aPattern);
  PText := Pointer(aText);
  if (PPattern = nil) then begin
    Result := False;
    Exit;
  end;
  PLast := PPattern + Length(aPattern) -1;
  if (PText = nil) then begin
    Result := (PPattern^ = MATCH_CHAR_KLEENE_CLOSURE) and (PPattern = PLast);
    Exit;
  end;
  TLast := PText + Length(aText) -1;
  Result := (Matche(PPattern, PLast, PText, TLast) = MATCH_VALID);
end;

function Matche(PPattern, PLast, PText, TLast: PChar): Integer;
var
  RangeStart, RangeEnd: PChar;
  Invert, MemberMatch, Loop: Boolean;
Label MP;
begin
  If (PPattern^ = '*') and (PPattern = PLast) then begin { EH: a single '*' matches everything }
    Result := MATCH_VALID;
    exit;
  end;
  Result := MATCH_PROCESSING;
  while ((Result = MATCH_PROCESSING) and (PPattern <= PLast)) do begin
    if PText > TLast then begin
      if (PPattern^ = MATCH_CHAR_KLEENE_CLOSURE) and (PPattern+1 > PLast)
      then Result := MATCH_VALID
      else Result := MATCH_ABORT;
      Exit;
    end else
      case (PPattern^) of
        MATCH_CHAR_KLEENE_CLOSURE:
          Result := MatchAfterStar(PPattern,PLast, PText,TLast);
        MATCH_CHAR_RANGE_OPEN:
          begin
            Inc(PPattern);
            Invert := False;
            if (PPattern > PLast) then goto MP;
            if (PPattern^ = MATCH_CHAR_EXCLAMATION_NEGATE) or
               (PPattern^ = MATCH_CHAR_CARET_NEGATE) then begin
              Invert := True;
              Inc(PPattern);
              if (PPattern > PLast) then goto MP;
            end;
            if (PPattern^ = MATCH_CHAR_RANGE_CLOSE) then goto MP;
            MemberMatch := False;
            Loop := True;
            while (Loop and (PPattern^ <> MATCH_CHAR_RANGE_CLOSE)) do begin
              RangeStart := PPattern;
              RangeEnd := PPattern;
              Inc(PPattern);
              if PPattern > PLast then goto MP;
              if PPattern^ = MATCH_CHAR_RANGE then begin
                Inc(PPattern);
                RangeEnd := PPattern;
                if (PPattern > PLast) or (RangeEnd^ = MATCH_CHAR_RANGE_CLOSE) then goto MP;
                Inc(PPattern);
              end;
              if PPattern > PLast then goto MP;
              if RangeStart < RangeEnd then begin
                if (PText^ >= RangeStart^) and
                   (PText^ <= RangeEnd^) then begin
                  MemberMatch := True;
                  Loop := False;
                end;
              end else begin
                if (PText^ >= RangeEnd^) and
                   (PText^ <= RangeStart^) then begin
                  MemberMatch := True;
                  Loop := False;
                end;
              end;
            end;
            if (Invert and MemberMatch) or (not (Invert or MemberMatch)) then begin
              Result := MATCH_RANGE;
              Exit;
            end;
            if MemberMatch then
              while (PPattern <= PLast) and (PPattern^ <> MATCH_CHAR_RANGE_CLOSE) do
                Inc(PPattern);
              if PPattern > PLast then begin
MP:             Result := MATCH_PATTERN;
                Exit;
              end;
          end;
        else if (PPattern^ <> MATCH_CHAR_SINGLE) then
          if (PPattern^ <> PText^) then
            Result := MATCH_LITERAL;
      end;
    Inc(PPattern);
    Inc(PText);
  end;
  if Result = MATCH_PROCESSING then
    if PText <= TLast
    then Result := MATCH_END
    else Result := MATCH_VALID;
end;

function MatchSQL_W(PPattern, PLast, PText, TLast: PWideChar): Integer;
var
  RangeStart, RangeEnd: PWideChar;
  Invert, MemberMatch, Loop: Boolean;
Label MP;
begin
  If (PWord(PPattern)^ = Word(MATCH_CHAR_MULTIPLE)) and (PPattern = PLast) then begin { EH: a single '%' matches everything }
    Result := MATCH_VALID;
    exit;
  end;
  Result := MATCH_PROCESSING;
  while ((Result = MATCH_PROCESSING) and (PPattern <= PLast)) do begin
    if PText > TLast then begin
      if (PWord(PPattern)^ = Word(MATCH_CHAR_MULTIPLE)) and (PPattern = PLast)
      then Result := MATCH_VALID
      else Result := MATCH_ABORT;
      Exit;
    end else
      case PWord(PPattern)^ of
        Word(MATCH_CHAR_MULTIPLE):
          Result := MatchAfterPercent_W(PPattern,PLast, PText,TLast);
        Word(MATCH_CHAR_RANGE_OPEN):
          begin
            Inc(PPattern);
            Invert := False;
            if (PPattern > PLast) then goto MP;
            if (PPattern^ = MATCH_CHAR_EXCLAMATION_NEGATE) or
               (PPattern^ = MATCH_CHAR_CARET_NEGATE) then begin
              Invert := True;
              Inc(PPattern);
              if (PPattern > PLast) then goto MP;
            end;
            if (PWord(PPattern)^ = Word(MATCH_CHAR_RANGE_CLOSE)) then goto MP;
            MemberMatch := False;
            Loop := True;
            while (Loop and (PWord(PPattern)^ <> Word(MATCH_CHAR_RANGE_CLOSE))) do begin
              RangeStart := PPattern;
              RangeEnd := PPattern;
              Inc(PPattern);
              if PPattern > PLast then goto MP;
              if PWord(PPattern)^ = Word(MATCH_CHAR_RANGE) then begin
                Inc(PPattern);
                RangeEnd := PPattern;
                if (PPattern > PLast) or (PWord(RangeEnd)^ = Word(MATCH_CHAR_RANGE_CLOSE)) then goto MP;
                Inc(PPattern);
              end;
              if PPattern > PLast then goto MP;
              if RangeStart < RangeEnd then begin
                if (PText^ >= RangeStart^) and
                   (PText^ <= RangeEnd^) then begin
                  MemberMatch := True;
                  Loop := False;
                end;
              end else begin
                if (PText^ >= RangeEnd^) and
                   (PText^ <= RangeStart^) then begin
                  MemberMatch := True;
                  Loop := False;
                end;
              end;
            end;
            if (Invert and MemberMatch) or (not (Invert or MemberMatch)) then begin
              Result := MATCH_RANGE;
              Exit;
            end;
            if MemberMatch then
              while (PPattern <= PLast) and (PWord(PPattern)^ <> Word(MATCH_CHAR_RANGE_CLOSE)) do
                Inc(PPattern);
              if PPattern > PLast then begin
MP:             Result := MATCH_PATTERN;
                Exit;
              end;
          end;
        else if (PWord(PPattern)^ <> Word(MATCH_CHAR_SINGLE_SQL)) then
          if (PPattern^ <> PText^) then
            Result := MATCH_LITERAL;
      end;
    Inc(PPattern);
    Inc(PText);
  end;
  if Result = MATCH_PROCESSING then
    if PText <= TLast
    then Result := MATCH_END
    else Result := MATCH_VALID;
end;

function MatchSQL_A(PPattern, PLast, PText, TLast: PAnsiChar): Integer;
var
  RangeStart, RangeEnd: PAnsiChar;
  Invert, MemberMatch, Loop: Boolean;
Label MP;
begin
  If (PByte(PPattern)^ = Byte(MATCH_CHAR_MULTIPLE)) and (PPattern = PLast) then begin { EH: a single '%' matches everything }
    Result := MATCH_VALID;
    exit;
  end;
  Result := MATCH_PROCESSING;
  while ((Result = MATCH_PROCESSING) and (PPattern <= PLast)) do begin
    if PText > TLast then begin
      if (PByte(PPattern)^ = Byte(MATCH_CHAR_MULTIPLE)) and (PPattern = PLast)
      then Result := MATCH_VALID
      else Result := MATCH_ABORT;
      Exit;
    end else
      case PByte(PPattern)^ of
        Byte(MATCH_CHAR_MULTIPLE):
          Result := MatchAfterPercent_A(PPattern,PLast, PText,TLast);
        Byte(MATCH_CHAR_RANGE_OPEN):
          begin
            Inc(PPattern);
            Invert := False;
            if (PPattern > PLast) then goto MP;
            if (PPattern^ = MATCH_CHAR_EXCLAMATION_NEGATE) or
               (PPattern^ = MATCH_CHAR_CARET_NEGATE) then begin
              Invert := True;
              Inc(PPattern);
              if (PPattern > PLast) then goto MP;
            end;
            if (PByte(PPattern)^ = Byte(MATCH_CHAR_RANGE_CLOSE)) then goto MP;
            MemberMatch := False;
            Loop := True;
            while (Loop and (PByte(PPattern)^ <> Byte(MATCH_CHAR_RANGE_CLOSE))) do begin
              RangeStart := PPattern;
              RangeEnd := PPattern;
              Inc(PPattern);
              if PPattern > PLast then goto MP;
              if PByte(PPattern)^ = Byte(MATCH_CHAR_RANGE) then begin
                Inc(PPattern);
                RangeEnd := PPattern;
                if (PPattern > PLast) or (PByte(RangeEnd)^ = Byte(MATCH_CHAR_RANGE_CLOSE)) then goto MP;
                Inc(PPattern);
              end;
              if PPattern > PLast then goto MP;
              if RangeStart < RangeEnd then begin
                if (PText^ >= RangeStart^) and
                   (PText^ <= RangeEnd^) then begin
                  MemberMatch := True;
                  Loop := False;
                end;
              end else begin
                if (PText^ >= RangeEnd^) and
                   (PText^ <= RangeStart^) then begin
                  MemberMatch := True;
                  Loop := False;
                end;
              end;
            end;
            if (Invert and MemberMatch) or (not (Invert or MemberMatch)) then begin
              Result := MATCH_RANGE;
              Exit;
            end;
            if MemberMatch then
              while (PPattern <= PLast) and (PByte(PPattern)^ <> Byte(MATCH_CHAR_RANGE_CLOSE)) do
                Inc(PPattern);
              if PPattern > PLast then begin
MP:             Result := MATCH_PATTERN;
                Exit;
              end;
          end;
        else if (PByte(PPattern)^ <> Byte(MATCH_CHAR_SINGLE_SQL)) then
          if (PPattern^ <> PText^) then
            Result := MATCH_LITERAL;
      end;
    Inc(PPattern);
    Inc(PText);
  end;
  if Result = MATCH_PROCESSING then
    if PText <= TLast
    then Result := MATCH_END
    else Result := MATCH_VALID;
end;

function MatchAfterStar(PPattern, PLast, PText, TLast: PChar): Integer;
label MV, MA;
begin
  Result := MATCH_PROCESSING;
  While (( PText <= TLast ) and (PPattern <= PLast)) and
     (Ord(PPattern^) in [Ord(MATCH_CHAR_SINGLE), Ord(MATCH_CHAR_KLEENE_CLOSURE)]) do begin
    if PPattern^ = MATCH_CHAR_SINGLE then
      Inc(PText);
    Inc(PPattern);
  end;
  If (PText > TLast) then goto MA;
  If (PPattern > PLast) then goto MV;
  repeat
    If (PLast >= PPattern) and ((PPattern^ = PText^) or (PPattern^ = MATCH_CHAR_RANGE_OPEN)) then begin
      Result  := Matche(PPattern, PLast, PText, TLast);
      if Result <> MATCH_VALID then
        Result := MATCH_PROCESSING;//retry until end of Text, (check below) or Result valid
    end;
    if (PText > TLast) or (PPattern > PLast) then begin
MA:   Result := MATCH_ABORT;
      Exit;
    end;
    Inc(PText);
  //until Result <> MATCH_PROCESSING
  Until (Result = MATCH_VALID) or (PText > TLast);
  if (PPattern > PLast) and (PText > TLast) then
MV: Result := MATCH_VALID;
end;

function MatchAfterPercent_W(PPattern, PLast, PText, TLast: PWideChar): Integer;
label MV, MA;
begin
  Result := MATCH_PROCESSING;
  While (( PText <= TLast ) and (PPattern <= PLast)) and
     ((PWord(PPattern)^ = Word(MATCH_CHAR_SINGLE)) or (PWord(PPattern)^ = Word(MATCH_CHAR_MULTIPLE))) do begin
    if PPattern^ = MATCH_CHAR_SINGLE then
      Inc(PText);
    Inc(PPattern);
  end;
  If (PText > TLast) then goto MA;
  If (PPattern > PLast) then goto MV;
  repeat
    If (PLast >= PPattern) and ((PPattern^ = PText^) or (PWord(PPattern)^ = Word(MATCH_CHAR_RANGE_OPEN))) then begin
      Result  := MatchSQL_W(PPattern, PLast, PText, TLast);
      if Result <> MATCH_VALID then
        Result := MATCH_PROCESSING;//retry until end of Text, (check below) or Result valid
    end;
    if (PText > TLast) or (PPattern > PLast) then begin
MA:   Result := MATCH_ABORT;
      Exit;
    end;
    Inc(PText);
  Until (Result = MATCH_VALID) or (PText > TLast);
  if (PPattern > PLast) and (PText > TLast) then
MV: Result := MATCH_VALID;
end;

function MatchAfterPercent_A(PPattern, PLast, PText, TLast: PAnsiChar): Integer;
label MV, MA;
begin
  Result := MATCH_PROCESSING;
  While (( PText <= TLast ) and (PPattern <= PLast)) and
     ((PByte(PPattern)^ = Byte(MATCH_CHAR_SINGLE)) or (PByte(PPattern)^ = Byte(MATCH_CHAR_MULTIPLE))) do begin
    if PPattern^ = MATCH_CHAR_SINGLE then
      Inc(PText);
    Inc(PPattern);
  end;
  If (PText > TLast) then goto MA;
  If (PPattern > PLast) then goto MV;
  repeat
    If (PLast >= PPattern) and ((PPattern^ = PText^) or (PByte(PPattern)^ = Byte(MATCH_CHAR_RANGE_OPEN))) then begin
      Result  := MatchSQL_A(PPattern, PLast, PText, TLast);
      if Result <> MATCH_VALID then
        Result := MATCH_PROCESSING;//retry until end of Text, (check below) or Result valid
    end;
    if (PText > TLast) or (PPattern > PLast) then begin
MA:   Result := MATCH_ABORT;
      Exit;
    end;
    Inc(PText);
  Until (Result = MATCH_VALID) or (PText > TLast);
  if (PPattern > PLast) and (PText > TLast) then
MV: Result := MATCH_VALID;
end;

function Like(const Pattern: ZWideString; PText: PWideChar; TLen: Cardinal): Boolean;
var PPattern, PLast, TLast: PWideChar;
begin
  PPattern := Pointer(Pattern);
  if (PPattern = nil) then begin
    Result := False;
    Exit;
  end;
  PLast := PPattern + Length(Pattern) -1;
  if (PText = nil) then begin
    Result := (PWord(PPattern)^ = Word(MATCH_CHAR_MULTIPLE)) and (PPattern = PLast);
    Exit;
  end;
  TLast := (PText + TLen) -1;
  Result := (MatchSQL_W(PPattern, PLast, PText, TLast) = MATCH_VALID);
end;

function Like(const Pattern: RawByteString; PText: PAnsiChar; TLen: Cardinal): Boolean; overload;
var PPattern, PLast, TLast: PAnsiChar;
begin
  PPattern := Pointer(Pattern);
  if (PPattern = nil) then begin
    Result := False;
    Exit;
  end;
  PLast := PPattern + Length(Pattern) -1;
  if (PText = nil) then begin
    Result := (PByte(PPattern)^ = Byte(MATCH_CHAR_MULTIPLE)) and (PPattern = PLast);
    Exit;
  end;
  TLast := (PText + TLen) -1;
  Result := (MatchSQL_A(PPattern, PLast, PText, TLast) = MATCH_VALID);
end;

end.
