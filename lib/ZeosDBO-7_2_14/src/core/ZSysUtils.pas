{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           System Utility Classes and Functions          }
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

unit ZSysUtils;

interface

{$I ZCore.inc}

uses
  Variants, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils, Types,
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToText
  ZMessages, ZCompatibility;

type
  {** Modified comparison function. }
  TZListSortCompare = function (Item1, Item2: Pointer): Integer of object;

  {** Modified list of pointers. }
  TZSortedList = class({$IFDEF TLIST_IS_DEPRECATED}TObject{$ELSE}TList{$ENDIF})
  {$IFDEF TLIST_IS_DEPRECATED}
  private
    FList: TPointerList;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    class procedure Error(const Msg: string; Data: NativeInt); overload; virtual;
    class procedure Error(Msg: PResStringRec; Data: NativeInt); overload;
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Item: Pointer): Pointer; inline;
    function First: Pointer; inline;
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Last: Pointer;
    function Remove(Item: Pointer): Integer; inline;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: TPointerList read FList;
    property Capacity: Integer read FCapacity write SetCapacity;
  {$ENDIF}
    procedure Sort(Compare: TZListSortCompare);
  end;

  {$IF NOT DECLARED(EArgumentException)}
  type
    EArgumentException = Class(Exception);
  {$IFEND}

const
  StrFalse = 'False';
  StrTrue = 'True';
  BoolStrInts: array[Boolean] of string = ('0', '1');
  {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
  BoolStrIntsRaw: array[Boolean] of RawByteString = ('0', '1');
  BoolStrsRaw: array[Boolean] of RawByteString = (RawByteString(StrFalse), RawByteString(StrTrue));
  {$ENDIF}
  BoolStrs: array[Boolean] of string = (StrFalse, StrTrue);
  BoolStrsW: array[Boolean] of ZWideString = (ZWideString(StrFalse), ZWideString(StrTrue));
  SQLDateTimeFmt = 'YYYY"-"MM"-"DD HH":"NN":"SS';
  SQLDateTimeFmtMSecs = 'YYYY"-"MM"-"DD HH":"NN":"SS"."ZZZ';
  YesNoStrs: array[Boolean] of string = ('NO', 'YES');

var
  TwoDigitLookupHexW: packed array[Byte] of Word;
  TwoDigitLookupHexLW: packed array[Byte] of LongWord;
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING} //can not be initialized ...
  BoolStrIntsRaw: array[Boolean] of RawByteString;
  BoolStrsRaw: array[Boolean] of RawByteString;
  {$ENDIF}

{**
  Determines a position of a first delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the first found delimiter or 0 if no delimiters was found.
}
function FirstDelimiter(const Delimiters, Str: string): Integer;

{**
  Determines a position of a LAST delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the last found delimiter or 0 if no delimiters was found.
}
function LastDelimiter(const Delimiters, Str: string): Integer;

{**
  Compares two Buffers with fixed length
  @param P1 first Pointer
  @param P2 seconds Pointer
  @return <code>Integer</code> if the memory equals else return PByte(P1)-PByte(B2)
}
function ZMemLComp(P1, P2: PAnsiChar; Len: Cardinal): Integer;

{**
  Compares two PWideChars without stopping at #0 (Unicode Version)
  @param P1 first PWideChars
  @param P2 seconds PWideChars
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompUnicode(P1, P2: PWideChar; Len: Integer): Boolean;

{**
  Compares two PAnsiChars without stopping at #0
  @param P1 first PAnsiChar
  @param P2 seconds PAnsiChar
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompAnsi(P1, P2: PAnsiChar; Len: Integer): Boolean;

{**
  Checks is the string starts with substring.
  @param Str a WideString/UnicodeString to be checked.
  @param SubStr a WideString/UnicodeString to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(const Str, SubStr: ZWidestring): Boolean; overload;

{**
  Checks is the string starts with substring.
  @param Str a AnsiString/RawByteString to be checked.
  @param SubStr a AnsiString/RawByteString to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}

function StartsWith(const Str, SubStr: RawByteString): Boolean; overload;

{**
  Checks is the string ends with substring.
  @param Str a AnsiString/RawByteString to be checked.
  @param SubStr a AnsiString/RawByteString to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: RawByteString): Boolean; overload;

{**
  Checks is the string ends with substring.
  @param Str a WideString/UnicodeString to be checked.
  @param SubStr a WideString/UnicodeString to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: ZWideString): Boolean; overload;

{** EH:
  Converts SQL PAnsiChar into float value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to raw data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @return a converted value or Def if conversion did fail.
}
function SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended; Len: Integer = 0): Extended; overload;


{** EH:
  Converts SQL PAnsiChar into float value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to raw data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @param Result return a converted value or Def if conversion did fail.
}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended; out Result: Extended; Len: Integer = 0); overload;


{** EH:
  Converts SQL PAnsiChar into currency value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to raw data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @param Result return a converted value or Def if conversion did fail.
}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency; out Result: Currency; Len: Integer = 0); overload;

{** EH:
  Converts SQL PAnsiChar into currency value.
  Possible is SQLFloat, Float, Money+Suffix
  @param Value an Pointer to raw data we want to convert.
  @param Def a default value if the value can not be converted.
  @param DecimalSep a reference to decimal seperator to be used;
          we will initialize this sep with '.' or replace it if with the Sep we found
  @param Result return a converted value or Def if conversion did fail.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency; Var DecimalSep: Char; out Result: Currency; Len: Integer = 0); overload;
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
{** EH:
  Converts SQL PAnsiChar into double precison value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to raw data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @param Result return a converted value or Def if conversion did fail.
}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Double; out Result: Double; Len: Integer = 0); overload;
{$IFEND}
{** EH:
  Converts SQL PAnsiChar into single precison value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to raw data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @param Result return a converted value or Def if conversion did fail.
}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Single; out Result: Single; Len: Integer = 0); overload;

{** EH:
  Converts SQL PWideChar into REAL10 value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to UTF16 data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @return a converted value or Def if conversion did fail.
}
function SQLStrToFloatDef(Value: PWideChar; const Def: Extended; Len: Integer = 0): Extended; overload;

{** EH:
  Converts a UTF16 buffer into REAL10 value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to UTF16 data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @param Result return a converted value or Def if conversion did fail.
}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Extended; out Result: Extended; Len: Integer = 0); overload;

{** EH:
  Converts a UTF16 buffer into currency value.
  Possible is SQLFloat, Float, Money+Suffix
  @param Value an Pointer to UTF16 data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @param Result return a converted value or Def if conversion did fail.
}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency; out Result: Currency; Len: Integer = 0); overload;

{** EH:
  Converts a UTF16 buffer into currency value.
  Possible is SQLFloat, Float, Money+Suffix
  @param Value an Pointer to UTF16 data we want to convert.
  @param Def a default value if the value can not be converted.
  @param DecimalSep a reference to decimal seperator to be used;
          we will initialize this sep with '.' or replace it if with the Sep we found
  @param Result return a converted value or Def if conversion did fail.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency; Var DecimalSep: Char; out Result: Currency; Len: Integer = 0); overload;
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
{** EH:
  Converts a UTF16 buffer into double precsion value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to UTF16 data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @param Result return a converted value or Def if conversion did fail.
}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Double; out Result: Double; Len: Integer = 0); overload;
{$IFEND}
{** EH:
  Converts a UTF16 buffer into single precsion value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to UTF16 data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @param Result return a converted value or Def if conversion did fail.
}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Single; out Result: Single; Len: Integer = 0); overload;

{**
  Converts a character buffer into pascal string.
  @param Buffer a character buffer pointer.
  @param Length a buffer length.
  @return a string retrived from the buffer.
}
{$IFDEF UNICODE}
function BufferToStr(Buffer: PWideChar; Length: Integer): string;
{$ELSE}
function BufferToStr(Buffer: PAnsiChar; Length: Integer): string;
{$ENDIF}

{**
  Converts a character buffer into pascal string.
  @param Buffer a character buffer pointer.
  @param Length a buffer length.
  @return a TBytes retrived from the buffer.
}
function BufferToBytes(Buffer: Pointer; Length: Integer): TBytes; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{**
  Converts a string into boolean value.
  @param Str a RawByteString value.
  @param CheckInt Check for "0" char too?
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(const Str: RawByteString; const CheckInt: Boolean = True): Boolean; overload;

{**
  Converts a zero terminated raw buffer into boolean value.
  @param Str a PAnsiChar value.
  @param CheckInt Check for "0" char too?
  @param IgnoreTrailingSaces Ignore trailing spaces for fixed char fields f.e.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(Str: PAnsiChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;

{**
  Converts a string into boolean value.
  @param Str a ZWideString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(const Str: ZWideString; const CheckInt: Boolean = True): Boolean; overload;

{**
  Converts a zero terminated UTF16 buffer into boolean value.
  @param Str a PWideChar value.
  @param CheckInt Check for "0" char too?
  @param IgnoreTrailingSaces Ignore trailing spaces for fixed char fields f.e.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(Str: PWideChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean; overload;

{**
  Converts a boolean into string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToUnicodeEx(Value: Boolean): ZWideString; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{**
  Converts a boolean into RawByteString value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToRawEx(Value: Boolean): RawByteString; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{**
  Converts a boolean into native string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToStrEx(Value: Boolean): string; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{$IFDEF ENABLE_POSTGRESQL}
{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
function IsIpAddr(const Str: string): Boolean;
{$ENDIF}

{**
  Splits string using the multiple chars.
  @param Str the source string
  @param Delimiters the delimiters string
  @return the result list where plased delimited string
}
function SplitString(const Str, Delimiters: string): TStrings;

{**
  Puts to list a splitted string using the multiple chars which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure PutSplitString(List: TStrings; const Str, Delimiters: string);

{**
  Appends to list a splitted string using the multiple chars.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitString(List: TStrings; const Str, Delimiters: string);

{**
  Composes a string from the specified strings list delimited with
  a special character.
  @param List a list of strings.
  @param Delimiter a delimiter string.
  @return a composed string from the list.
}
function ComposeString(List: TStrings; const Delimiter: string): string;

{**
  Converts a float value into SQL string with '.' delimiter.
  @param Value a float value to be converted.
  @return a converted string value.
}
function FloatToSQLStr(Value: Extended): string;

{**
  Converts SQL string with '.' delimiter into a float value.
  @param Str a string value to be converted.
  @return a converted float value.
}
function SQLStrToFloat(const Str: String): Extended;

{**
  Puts to list a splitted string using the delimiter string which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure PutSplitStringEx(List: TStrings; const Str, Delimiter: string);

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringEx(const Str, Delimiter: string): TStrings;

{**
  Appends to list a splitted string using the delimeter string.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitStringEx(List: TStrings; const Str, Delimiter: string);

{**
  Converts bytes into a AnsiString representation.
  @param Value an array of bytes to be converted.
  @return a converted AnsiString.
}
function BytesToStr(const Value: TBytes): RawByteString;

{**
  Converts AnsiString into an array of bytes.
  @param Value a AnsiString to be converted.
  @return a converted array of bytes.
}
{$IFNDEF NO_ANSISTRING}
function StrToBytes(const Value: AnsiString): TBytes; overload;
{$ENDIF}

{**
  Converts a UTF8String into an array of bytes.
  @param Value a UTF8String to be converted.
  @return a converted array of bytes.
}
{$IF defined(WITH_RAWBYTESTRING) and not defined(NO_UTF8STRING)}
function StrToBytes(const Value: UTF8String): TBytes; overload;
{$IFEND}
{**
  Converts a UTF8String into an array of bytes.
  @param Value a UTF8String to be converted.
  @return a converted array of bytes.
}
{$IF defined(WITH_RAWBYTESTRING) and not defined(WITH_TBYTES_AS_RAWBYTESTRING)}
function StrToBytes(const Value: RawByteString): TBytes; overload;
{$IFEND}

{**
  Converts a RawByteString into an array of bytes.
  @param Value a RawByteString to be converted.
  @return a converted array of bytes.
}
{$IFDEF MSWINDOWS}
function StrToBytes(const Value: WideString): TBytes; overload;
{$ENDIF}

{**
  Converts a String into an array of bytes.
  @param Value a String to be converted.
  @return a converted array of bytes.
}
{$IFDEF PWIDECHAR_IS_PUNICODECHAR}
function StrToBytes(const Value: UnicodeString): TBytes; overload;
{$ENDIF}
{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function BytesToVar(const Value: TBytes): Variant; overload;
function BytesToVar(const Value: RawByteString): Variant; overload;

{**
  Converts variant into an array of bytes.
  @param Value a varaint to be converted.
  @return a converted array of bytes.
}
function VarToBytes(const Value: Variant): TBytes;

{**
  Converts Ansi SQL Date/Time to TDateTime
  @param Value a date and time string.
  @return a decoded TDateTime value.
}
function AnsiSQLDateToDateTime(const Value: ZWideString): TDateTime; overload;
function AnsiSQLDateToDateTime(P: PWideChar; L: LengthInt): TDateTime; overload;
function AnsiSQLDateToDateTime(const Value: RawByteString): TDateTime; overload;
function AnsiSQLDateToDateTime(P: PAnsiChar; L: LengthInt): TDateTime; overload;

{**
  Converts Ansi SQL Date (DateFormat) to TDateTime
  @param Value a date and time string.
  @param Dateformat a Pointer to DateFormat. May be nil;
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Unicode SQL Date (DateFormat) to TDateTime
  @param Value a date and time string.
  @param Dateformat a Pointer to DateFormat. May be nil;
  @return a decoded TDateTime value.
}
function UnicodeSQLDateToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Ansi SQL Time (hh:nn:ss or hh:mm:nn.zzz or TimeFormat) to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Unicode SQL Time (hh:nn:ss or hh:mm:nn.zzz or TimeFormat) to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLTimeToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Ansi SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeStampToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{**
  Converts Unicode SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLTimeStampToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;

{** EH:
  Converts DateTime value into a raw encoded string with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then DateFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::date.
  @return a formated RawByteString with Date-Format pattern.
}
function DateTimeToRawSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString; overload;

{** EH:
  Converts DateTime value into a raw buffer with format pattern
  @param Value a TDateTime value.
  @param Buf the raw buffer to write in.
  @param ConFormatSettings then DateFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::date.
  @return the length in bytes of written value.
}
function DateTimeToRawSQLDate(const Value: TDateTime; Buf: PAnsichar;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): Word; overload;

{** EH:
  Converts date values into a buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  I'm aware year/month/day with value 0 do not exist but MySQL f.e. allows it!
  @param Year a Year value with range of 0..9999.
  @param Month a Month value with range of 0..12.
  @param Day a Day value with range of 0..31.
  @param Buf a raw buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @param Negative if the date is negative (i.e. bc).
  @return the length in bytes of written value.
}
function DateTimeToRawSQLDate(Year, Month, Day: Word; Buf: PAnsichar;
  const Format: String; Quoted, Negative: Boolean): Byte; overload;

{** EH:
  Converts date value into a WideString/UnicodeString with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then DateFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::date.
  @return a formated WideString/UnicodeString with Date-Format pattern.
}
function DateTimeToUnicodeSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString; overload;

{** EH:
  Converts date value into a UCS2 buffer with format pattern
  @param Value a TDateTime value.
  @param Buf the UCS2 buffer to write in.
  @param ConFormatSettings then DateFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::date.
  @return the length in code-points of written value.
}
function DateTimeToUnicodeSQLDate(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): Word; overload;

{** EH:
  Converts date values into a buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  I'm aware year/month/day with value 0 do not exist but MySQL f.e. allows it!
  @param Year a Year value with range of 0..9999.
  @param Month a Month value with range of 0..12.
  @param Day a Day value with range of 0..31.
  @param Buf a UCS2 buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @param Negative if the date is negative (i.e. bc).
  @return the length in code-points of written value.
}
function DateTimeToUnicodeSQLDate(Year, Month, Day: Word; Buf: PWideChar;
  const Format: String; Quoted, Negative: Boolean): Byte; overload;

{**
  Converts DateTime value to native string
}
function DateTimeToSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: string = ''): string; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{** EH:
  Converts time value into a raw encoded string with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::time.
  @return a formated RawByteString with Time-Format pattern.
}
function DateTimeToRawSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString; overload;

{** EH:
  Converts a time value into a raw buffer with format pattern
  @param Value a TDateTime value.
  @param Buf the raw buffer to write in.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::time.
  @return the length in bytes of written value.
}
function DateTimeToRawSQLTime(const Value: TDateTime; Buffer: PAnsichar;
  const ConFormatSettings: TZFormatSettings;
  Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): Word; overload;

{** EH:
  Converts a time values into a raw buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  @param Hour a hour value with range of 0..23.
  @param Minute a minute value with range of 0..59.
  @param Second a second value with range of 0..59.
  @param MSec a millisecond value with range of 0..999.
  @param Buf the raw buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @return the length in bytes of written value.
}
function DateTimeToRawSQLTime(Hour, Minute, Second, MSec: Word;
  Buf: PAnsichar; const Format: String; Quoted: Boolean): Byte; overload;

{** EH:
  Converts a time value into a WideString/UnicodeString with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::time.
  @return a formated WideString/UnicodeString with Time-Format pattern.
}
function DateTimeToUnicodeSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString; overload;

{** EH:
  Converts a time value into a UCS2 buffer with format pattern
  @param Value a TDateTime value.
  @param Buf then UCS2 buffer to write in.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::time.
  @return the length in codepoints of written value.
}
function DateTimeToUnicodeSQLTime(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): Word; overload;

{** EH:
  Converts a time values into a UCS2 buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  @param Hour a hour value with range of 0..23.
  @param Minute a minute value with range of 0..59.
  @param Second a second value with range of 0..59.
  @param MSec a millisecond value with range of 0..999.
  @param Buf the unicode buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @return the length in codepoints of written value.
}
function DateTimeToUnicodeSQLTime(Hour, Minute, Second, MSec: Word;
  Buf: PWideChar; const Format: String; Quoted: Boolean): Byte; overload;

{**
  Converts DateTime value to native string
}
function DateTimeToSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: string = ''): string; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{** EH:
  Converts datetime value into a raw encoded string with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then DateTimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::timestamp.
  @return a formated RawByteString with DateTime-Format pattern.
}
function DateTimeToRawSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString; overload;

{** EH:
  Converts datetime value into a raw encoded buffer with format pattern
  @param Value a TDateTime value.
  @param Buf the raw buffer we write in.
  @param ConFormatSettings then DateTimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::timestamp.
  @return a formated RawByteString with DateTime-Format pattern.
}
function DateTimeToRawSQLTimeStamp(const Value: TDateTime; Buf: PAnsiChar;
  const ConFormatSettings: TZFormatSettings; Quoted: Boolean;
  const Suffix: RawByteString = EmptyRaw): Word; overload;

{** EH:
  Converts a datetime values into a raw buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  I'm aware year/month/day with value 0 do not exist but MySQL f.e. allows it!
  @param Year a Year value with range of 0..9999.
  @param Month a Month value with range of 0..12.
  @param Day a Day value with range of 0..31.
  @param Hour a hour value with range of 0..23.
  @param Minute a minute value with range of 0..59.
  @param Second a second value with range of 0..59.
  @param MSec a millisecond value with range of 0..999.
  @param Buf the raw buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @param Negative if the date is negative (i.e. bc).
  @return the length in bytes of written value.
}
function DateTimeToRawSQLTimeStamp(Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buf: PAnsiChar; const Format: String; Quoted, Negative: Boolean): Byte; overload;

{** EH:
  Converts datetime value into a Unicode/Widestring with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then DateTimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::timestamp.
  @return a formated UCS2-String with DateTime-Format pattern.
}
function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString; overload;

{** EH:
  Converts a datetime value into a UCS2 buffer with format pattern
  @param Value a TDateTime value.
  @param Buf then UCS2 buffer to write in.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::timestamp.
  @return the length in codepoints of written value.
}
function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): Word; overload;

{** EH:
  Converts date and time values into a UCS2 buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  I'm aware year/month/day with value 0 do not exist but MySQL f.e. allows it!
  @param Year a Year value with range of 0..9999.
  @param Month a Month value with range of 0..12.
  @param Day a Day value with range of 0..31.
  @param Hour a hour value with range of 0..23.
  @param Minute a minute value with range of 0..59.
  @param Second a second value with range of 0..59.
  @param MSec a millisecond value with range of 0..999.
  @param Buf the unicode buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @param Negative if the date is negative (i.e. bc).
  @return the length in bytes of written value.
}
function DateTimeToUnicodeSQLTimeStamp(Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buf: PWideChar; const Format: String; Quoted, Negative: Boolean): Byte; overload;

{**
  Converts DateTime value to native string
}
function DateTimeToSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: string = ''): string; {$IFDEF WITH_INLINE} inline;{$ENDIF}

{**
  Converts TDateTime to Ansi SQL Date/Time
  @param Value an encoded TDateTime value.
  @return a  date and time string.
}
function DateTimeToAnsiSQLDate(Value: TDateTime; WithMMSec: Boolean = False): string;

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeCString(const Value: ZWideString): ZWideString; overload;
function EncodeCString(const Value: RawByteString): RawByteString; overload;
{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeCString(const Value: ZWideString): ZWideString; overload;
function DecodeCString(const Value: RawByteString): RawByteString; overload;

procedure DecodeCString(SrcLength: LengthInt; SrcBuffer: PWideChar; out Result: ZWideString); overload;
procedure DecodeCString(SrcLength: LengthInt; SrcBuffer: PAnsiChar; out Result: RawByteString); overload;

function DecodeCString(SrcLength: LengthInt; SrcBuffer, DestBuffer: PWideChar): LengthInt; overload;
function DecodeCString(SrcLength: LengthInt; SrcBuffer, DestBuffer: PAnsiChar): LengthInt; overload;

{**
  Replace chars in the string
  @param Source a char to search.
  @param Target a char to replace.
  @param Str a source string.
  @return a string with replaced chars.
}
function ReplaceChar(const Source, Target: Char; const Str: string): string;

{**
  Remove chars in the string.
  More obvious and ~35 times faster than StringReplace(Str, ToRemove, '')
  @param ToRemove a char to search and remove.
  @param Str a source string.
  @return a string with removed chars.
}
function RemoveChar(ToRemove: Char; const Str: string): string;

{**
  Append a string to another string separating the added string with delimiter.
  Correctly processes cases where any of the arguments is empty
  @param Str source string to append to. If empty, resulting Str value will be AddStr
  @param AddStr string to append. If empty, Str won't be changed
  @param Delimiter string to separate AddStr from Str
}
procedure AppendSepString(var Str: string; const AddStr, Delimiter: string);

{**
  Break a string into two parts according to appearance of Delimiter.
  @param Str source string
  @param Delimiter separator string; Str=Left+Delimiter+Right
  @param Left left part of Str from the start to the first Delimiter.
    Equals to Str if Str doesn't contain Delimiter
  @param Right left part of Str from the first Delimiter to the end.
    Empty if Str doesn't contain Delimiter
}
procedure BreakString(const Str, Delimiter: String; var Left, Right: String);

{**
  Decodes a Full Version Value encoded with the format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
procedure DecodeSQLVersioning(const FullVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);

{**
  Encodes major, minor and subversion (revision) values in this format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  For example, 4.1.12 is returned as 4001012.
  @param MajorVersion an integer containing the Major Version.
  @param MinorVersion an integer containing the Minor Version.
  @param SubVersion an integer containing the Sub Version (revision).
  @return an integer containing the full version.
}
function EncodeSQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;

{**
  Formats a Zeos SQL Version format to X.Y.Z where:
   X = major_version
   Y = minor_version
   Z = sub version
  @param SQLVersion an integer
  @return Formated Zeos SQL Version Value.
}
function FormatSQLVersion( const SQLVersion: Integer ): String;

{**
  Appends WHERE clause condition. Returns ' and '+Condition if Condition is not empty
  and empty string otherwise.
  This allows short constructions like
    'WHERE 1=1'+AppendCondition(Cond1)+AppendCondition(Cond2)...
}
function AppendCondition(const Condition: string): string;

function ASCII7ToUnicodeString(const Src: RawByteString): ZWideString; overload;
function ASCII7ToUnicodeString(Src: PAnsiChar; const Len: LengthInt): ZWideString; overload;
function UnicodeStringToASCII7(const Src: ZWideString): RawByteString; overload;
function UnicodeStringToASCII7(const Src: PWideChar; const Len: LengthInt): RawByteString; overload;

function FloatToRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): RawByteString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function FloatToRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PAnsiChar): LengthInt; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function FloatToSqlRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): RawByteString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function FloatToSqlRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PAnsiChar): LengthInt; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function FloatToUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): ZWideString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function FloatToUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PWideChar): LengthInt; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function FloatToSqlUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): ZWideString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function FloatToSqlUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PWideChar): LengthInt; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

procedure ZBinToHex(Buffer, Text: PAnsiChar; const Len: LengthInt); overload;
procedure ZBinToHex(Buffer: PAnsiChar; Text: PWideChar; const Len: LengthInt); overload;

procedure GUIDToBuffer(const Source: Pointer; Dest: PAnsiChar; const SetTerm: Boolean = False); overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
procedure GUIDToBuffer(const Source: Pointer; Dest: PWideChar; const SetTerm: Boolean = False); overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

function GUIDToRaw(const GUID: TGUID): RawByteString; overload;
function GUIDToRaw(const Bts: TBytes): RawByteString; overload;
function GUIDToRaw(Buffer: Pointer; Len: NativeInt): RawByteString; overload;

function GUIDToUnicode(const GUID: TGUID): ZWideString; overload;
function GUIDToUnicode(const Bts: TBytes): ZWideString; overload;
function GUIDToUnicode(Buffer: Pointer; Len: NativeInt): ZWideString; overload;

function GUIDToStr(const GUID: TGUID): string; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function GUIDToStr(const Bts: TBytes): string; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function GUIDToStr(Buffer: Pointer; Len: Byte): string; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

procedure ValidGUIDToBinary(Src, Dest: PAnsiChar); overload;
procedure ValidGUIDToBinary(Src: PWideChar; Dest: PAnsiChar); overload;

function SQLQuotedStr(const S: ZWideString; Quote: WideChar): ZWideString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function SQLQuotedStr(Src: PWideChar; Len: LengthInt; Quote: WideChar): ZWideString; overload;
function SQLQuotedStr(const S: RawByteString; Quote: AnsiChar): RawByteString; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function SQLQuotedStr(Src: PAnsiChar; Len: LengthInt; Quote: AnsiChar): RawByteString; overload;

function SQLQuotedStr(const S: string; QuoteLeft, QuoteRight: Char): string; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function SQLQuotedStr(Src: PChar; Len: LengthInt; QuoteLeft, QuoteRight: Char): string; overload;

function SQLDequotedStr(const S: string; QuoteChar: Char): string; overload;
function SQLDequotedStr(Src: PChar; Len: LengthInt; QuoteChar: Char): string; overload;
function SQLDequotedStr(const S: string; QuoteLeft, QuoteRight: Char): string; overload;

function SameText(Val1, Val2: PAnsiChar; Len: LengthInt): Boolean; overload;
function SameText(Val1, Val2: PWideChar; Len: LengthInt): Boolean; overload;

procedure Trim(var L: NativeUInt; var P: PAnsiChar); overload;
function Trim(P: PAnsiChar; L: LengthInt): RawByteString; overload;
function Trim(P: PAnsiChar): RawByteString; overload;
function Trim(P: PWideChar; L: LengthInt): ZWideString; overload;
{$IF defined(UNICODE) and not defined(WITH_UNITANSISTRINGS)}
function Trim(const Value: RawByteString): RawByteString; overload;
function LowerCase(const Value: RawByteString): RawByteString; overload;
function UpperCase(const Value: RawByteString): RawByteString; overload;
{$IFEND}
{$IFNDEF UNICODE}
function Trim(const Value: ZWideString): ZWideString; overload;
{$ENDIF}

{$IFDEF NO_RAW_HEXTOBIN}
function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: Integer): Boolean;
{$ENDIF}
{**
   Creates a memory stream with copy of data in buffer.
   If buffer contains no data, creates an empty stream.
}
function StreamFromData(Buffer: Pointer; Size: Integer): TMemoryStream; overload;
function StreamFromData(const AString: ZWideString): TMemoryStream; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function StreamFromData(const Bytes: TBytes): TMemoryStream; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
function StreamFromData(const AString: RawByteString): TMemoryStream; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
{$ENDIF}

function StringReplaceAll_CS_LToEQ(const Source, OldPattern, NewPattern: RawByteString): RawByteString; overload;
function StringReplaceAll_CS_LToEQ(const Source, OldPattern, NewPattern: ZWideString): ZWideString; overload;
Type TCurrRoundToScale = 0..4;

function RoundCurrTo(const Value: Currency; Scale: TCurrRoundToScale): Currency;const
  // Local copy of current FormatSettings with '.' as DecimalSeparator and empty other fields
  FmtSettFloatDot: TFormatSettings = ( DecimalSeparator: {%H-}'.' );
  MSecMulTable: array[1..3] of Word = (100,10,1);

implementation

uses DateUtils,
  {$IF defined(WITH_RTLCONSTS_SInvalidGuidArray) or defined(TLIST_IS_DEPRECATED)}RTLConsts,{$IFEND}
  SysConst,{keep it after RTLConst -> deprecated warning}
  ZFastCode;


{**
  Determines a position of a first delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the first found delimiter or 0 or -1(Zero based strings) if no delimiters was found.
}
function FirstDelimiter(const Delimiters, Str: string): Integer;
var P: PChar absolute Str;
  PStart, PEnd, PDStart, PDEnd: PChar;
begin
  Result := 0;
  PDStart := Pointer(Delimiters);
  if PDStart = nil then Exit;
  PDEnd := PDStart+Length(Delimiters);
  PStart := P;
  PEnd := PStart+Length(Str);
  while (PStart < PEnd) do begin
    while PDStart < PDEnd do
      if PStart^ = PDStart^ then begin
        Result := PStart-P+1;
        Exit;
      end else
        Inc(PDStart);
    PDStart := Pointer(Delimiters);
    Inc(PStart);
  end;
end;

{**
  Determines a position of a LAST delimiter.
  @param Delimiters a string with possible delimiters.
  @param Str a string to be checked.
  @return a position of the last found delimiter or 0 if no delimiters was found.
}
function LastDelimiter(const Delimiters, Str: string): Integer;
var P: PChar absolute Str;
  PStart, PEnd, PDStart, PDEnd: PChar;
begin
  Result := 0;
  PDStart := Pointer(Delimiters);
  if PDStart = nil then Exit;
  PDEnd := PDStart+Length(Delimiters);
  PStart := P;
  PEnd := PStart+Length(Str)-1;
  while (PEnd >= PStart) do begin
    while PDStart < PDEnd do
      if PEnd^ = PDStart^ then begin
        Result := PEnd-P+1;
        Exit;
      end else
        Inc(PDStart);
    PDStart := Pointer(Delimiters);
    Dec(PEnd);
  end;
end;

{**
  Compares two PWideChars without stopping at #0 (Unicode Version)
  @param P1 first PWideChar
  @param P2 seconds PWideChar
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompUnicode(P1, P2: PWideChar; Len: Integer): Boolean;
begin
  Result := ZMemLComp(Pointer(P1), Pointer(P2), Len shl 1) = 0;
end;

{**  EH:
  Compares two Pointers with a maximum len
  @param P1 first PAnsiChar
  @param P2 seconds PAnsiChar
  @return <code>Integer</code> 0 if the memory at P1 and P2 are equal, otherwise
    return the byte difference
}
function ZMemLComp(P1, P2: PAnsiChar; Len: Cardinal): Integer;
Label Fail;
var
  PEnd: PAnsiChar;
begin
  Result := 0;
  PEnd := P1 + Len;
  while P1+32 < PEnd do //compare 32 Bytes per loop
  begin
    {$IFDEF CPUX64} //32Bit targets a less optimal comparing 8Byte values
    if (PUInt64(P1)^ <> PUInt64(P2)^) then goto Fail;
    if (PUInt64(P1+8)^ <> PUInt64(P2+8)^) then goto Fail;
    if (PUInt64(P1+16)^ <> PUInt64(P2+16)^) then goto Fail;
    if (PUInt64(P1+24)^ <> PUInt64(P2+24)^) then goto Fail;
    {$ELSE}
    if (PLongWord(P1)^ <> PLongWord(P2)^) then goto Fail;
    if (PLongWord(P1+4)^ <> PLongWord(P2+4)^) then goto Fail;
    if (PLongWord(P1+8)^ <> PLongWord(P2+8)^) then goto Fail;
    if (PLongWord(P1+12)^ <> PLongWord(P2+12)^) then goto Fail;
    if (PLongWord(P1+16)^ <> PLongWord(P2+16)^) then goto Fail;
    if (PLongWord(P1+20)^ <> PLongWord(P2+20)^) then goto Fail;
    if (PLongWord(P1+24)^ <> PLongWord(P2+24)^) then goto Fail;
    if (PLongWord(P1+28)^ <> PLongWord(P2+28)^) then goto Fail;
    {$ENDIF}
    Inc(P1, 32); Inc(P2, 32);
  end;
  while P1+8 < PEnd do //compare 8 Bytes per loop
  begin
    {$IFDEF CPUX64}
    if (PUInt64(P1)^ <> PUInt64(P2)^) then goto Fail; //not overflow save so let's check the bytes
    {$ELSE}
    if (PLongWord(P1)^ <> PLongWord(P2)^) then goto Fail;
    if (PLongWord(P1+4)^ <> PLongWord(P2+4)^) then goto Fail;
    {$ENDIF}
    Inc(P1, 8); Inc(P2, 8);
  end;
Fail:
  while P1 < PEnd do
  begin
    Result := PByte(P1)^ - PByte(P2)^; //overflow save
    if Result = 0 then
    begin
      Inc(P1); Inc(P2);
    end
    else
      Exit;
  end;
end;

{**
  Compares two PAnsiChars without stopping at #0
  @param P1 first PAnsiChar
  @param P2 seconds PAnsiChar
  @return <code>True</code> if the memory at P1 and P2 are equal
}
function MemLCompAnsi(P1, P2: PAnsiChar; Len: Integer): Boolean;
begin
  Result := ZMemLComp(P1, P2, Len) = 0;
end;

{**
  Checks is the string starts with substring.
  @param Str a AnsiString/RaweByteString to be checked.
  @param SubStr a AnsiString/RaweByteString to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(const Str, SubStr: RawByteString): Boolean;
var
  LenSubStr: Integer;
begin
  if SubStr = EmptyRaw
  then Result := True
  else begin
    LenSubStr := Length(SubStr);
    if LenSubStr <= Length(Str)
    then Result := MemLCompAnsi(PAnsiChar(Str), PAnsiChar(SubStr), LenSubStr)
    else Result := False;
  end;
end;

{**
  Checks is the string starts with substring.
  @param Str a WideString/UnicodeString to be checked.
  @param SubStr a WideString/UnicodeString to test at the start of the Str.
  @return <code>True</code> if Str started with SubStr;
}
function StartsWith(const Str, SubStr: ZWideString): Boolean;
var
  LenSubStr: Integer;
begin
  if SubStr = ''
  then Result := True
  else begin
    LenSubStr := Length(SubStr);
    if LenSubStr <= Length(Str)
    then Result := MemLCompUnicode(PWideChar(Str), PWideChar(SubStr), LenSubStr)
    else Result := False;
  end;
end;

{**
  Checks is the string ends with substring.
  @param Str a WideString/UnicodeString to be checked.
  @param SubStr a WideString/UnicodeString to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: ZWideString): Boolean;
var
  LenSubStr: Integer;
  LenStr: Integer;
begin
  if SubStr = ''
  then Result := False // act like Delphi's AnsiEndsStr()
  else begin
    LenSubStr := Length(SubStr);
    LenStr := Length(Str);
    if LenSubStr <= LenStr
    then Result := MemLCompUnicode(PWideChar(Str) + LenStr - LenSubStr,
         PWidechar(SubStr), LenSubStr)
    else Result := False;
  end;
end;

{**
  Checks is the string ends with substring.
  @param Str a AnsiString/RawbyteString to be checked.
  @param SubStr a AnsiString/RawbyteString to test at the end of the Str.
  @return <code>True</code> if Str ended with SubStr;
}
function EndsWith(const Str, SubStr: RawByteString): Boolean;
var
  LenSubStr: Integer;
  LenStr: Integer;
begin
  if SubStr = EmptyRaw
  then Result := False // act like Delphi's AnsiEndsStr()
  else begin
    LenSubStr := Length(SubStr);
    LenStr := Length(Str);
    if LenSubStr <= LenStr
    then Result := MemLCompAnsi(PAnsiChar(Str) + LenStr - LenSubStr,
         PAnsiChar(SubStr), LenSubStr)
    else Result := False;
  end;
end;

{** EH:
  Converts SQL PAnsiChar into float value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to raw data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @return a converted value or Def if conversion did fail.
}
function SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended;
  Len: Integer = 0): Extended;
begin
  SQLStrToFloatDef(Value, Def, Result, Len);
end;

function CurrToRawBuff(Value: PAnsiChar; Buf: PByteArray; Len: Integer): Boolean;
var
  I, ValidCount, InvalidPos, DotPos, CommaPos: Integer;
label Fail;
begin
  Result := True;
  DotPos := 0; CommaPos := 0; ValidCount := 0; InvalidPos := 0;
  FillChar(Buf^, Len+1, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  for i := 0 to Len-1 do
    case Ord((Value+i)^) of
      Ord('0')..Ord('9'):
        begin
          Buf[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      Ord(','):
        if ((I-InvalidPos-DotPos) = 3) or ((DotPos=0) and (ValidCount > 0)) then //all others are invalid!
        begin
          CommaPos := I;
          if DotPos = 0 then
            Inc(ValidCount)
          else //align result four Byte block and overwrite last ThousandSeparator
            PLongWord(@Buf[DotPos-1])^ := PLongWord(@Buf[DotPos])^;
          Buf[ValidCount-1] := Ord('.');
        end
        else
          Goto Fail;
      Ord('-'), Ord('+'):
        begin
          Buf[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      Ord('.'):
        begin
          if DotPos > 0 then //previously init so commapos can't be an issue here
          begin
            if (I-InvalidPos-DotPos) = 3 then //all others are invalid!
            begin
              PLongWord(@Buf[DotPos-1])^ := PLongWord(@Buf[DotPos])^;
              Buf[ValidCount-1] := Ord('.');
              Inc(InvalidPos);
            end
            else
              Goto Fail;
          end
          else
            if I < CommaPos then
              Goto Fail
            else
            begin
              Buf[ValidCount] := Ord('.');
              Inc(ValidCount);
            end;
          DotPos := ValidCount;
        end;
      else
        if (ValidCount > 0) then
          if Ord((Value+i)^) = Ord(' ') then //641,22 $ f.e.
            Break
          else
            Goto Fail
        else
          InvalidPos := i+1;
    end;
  Exit;
Fail:
  Result := False;
end;

{** EH:
  Converts SQL PAnsiChar into float value.
  Possible is SQLFloat, Float, Hex, Money+Suffix
  @param Value an Pointer to raw data we want to convert.
  @param Def a default value if the value can not be converted.
  @param Len the length of the buffer. If Len is zero we the buffer should be #0 terminated.
  @param Result return a converted value or Def if conversion did fail.
}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Extended;
  out Result: Extended; Len: Integer);
var
  InvalidPos: Integer;
  DynBuf: TBytes;
  StatBuf: Array[0..32] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValRawExt(Pointer(Value), AnsiChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+Len*Ord(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, AnsiChar(','), Def, Result)
      else
      begin
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        if (InvalidPos > 1) and (Ord((Value+InvalidPos-1)^) = Ord(' ')) then
          Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len) then
          RawToFloatDef(PAnsiChar(PBuf), AnsiChar('.'), Def, Result)
        else
          Result := Def;
      end;
  end;
end;

procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency;
  out Result: Currency; Len: Integer);
var InvalidPos: Integer;
begin
  Result := Def;
  if Assigned(Value) then begin
    if Len = 0 then
      Len := ZFastCode.StrLen(Value);
    InvalidPos := Len;
    Result := ValRawCurr(PByteArray(Value), '.', InvalidPos);
    if InvalidPos = Len then Exit;
    if InvalidPos < Len then begin//posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+InvalidPos)^) in [Ord('0')..Ord('9')]) then begin //nope no money. Just a comma instead of dot.
        InvalidPos := Len;
        Result := ValRawCurr(PByteArray(Value), ',', InvalidPos);
        if InvalidPos = Len then
          Exit;
      end
    end;
    if Ord((Value+InvalidPos-1)^) = Ord(' ') then Exit;
  end;
  Result := Def;
end;

procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Currency; Var DecimalSep: Char; out Result: Currency; Len: Integer = 0); overload;
var InvalidPos: Integer;
begin
  if Assigned(Value) then begin
    if Len = 0 then
      Len := ZFastCode.StrLen(Value);
    if DecimalSep = #0 then
      DecimalSep := '.';
    InvalidPos := Len;
    Result := ValRawCurr(PByteArray(Value), DecimalSep, InvalidPos);
    if InvalidPos = Len then Exit;
    if InvalidPos < Len then begin //posible MoneyType
      if (Ord((Value+InvalidPos-1)^) in [Ord('.'), Ord(',')]) and (Ord((Value+InvalidPos-1)^) <> Ord(DecimalSep)) and (Ord((Value+InvalidPos)^) in [Ord('0')..Ord('9')]) then begin //nope no money. Just a comma instead of dot.
        InvalidPos := Len;
        if DecimalSep = '.'
        then DecimalSep := ','
        else DecimalSep := '.';
        Result := ValRawCurr(PByteArray(Value), DecimalSep, InvalidPos);
        if InvalidPos = Len then
          Exit;
      end;
      if Ord((Value+InvalidPos-1)^) = Ord(' ') then Exit;
    end;
  end;
  Result := Def;
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Double;
  out Result: Double; Len: Integer);
var
  InvalidPos: Integer;
  DynBuf: TBytes;
  StatBuf: Array[0..32] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValRawDbl(Pointer(Value), AnsiChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+Len*Ord(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, AnsiChar(','), Def, Result)
      else
      begin
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        if (InvalidPos > 1) and (Ord((Value+InvalidPos-1)^) = Ord(' ')) then
          Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len) then
          RawToFloatDef(PAnsiChar(PBuf), AnsiChar('.'), Def, Result)
        else
          Result := Def;
      end;
  end;
end;
{$IFEND}

procedure SQLStrToFloatDef(Value: PAnsiChar; const Def: Single;
  out Result: Single; Len: Integer);
var
  InvalidPos: Integer;
  DynBuf: TBytes;
  StatBuf: Array[0..32] of Byte;
  PBuf: PByteArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValRawSin(Pointer(Value), AnsiChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+Len*Ord(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        RawToFloatDef(Value, AnsiChar(','), Def, Result)
      else
      begin
        if Len = 0 then
          Len := ZFastCode.StrLen(Value);
        if (InvalidPos > 1) and (Ord((Value+InvalidPos-1)^) = Ord(' ')) then
          Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToRawBuff(Value, PBuf, Len) then
          RawToFloatDef(PAnsiChar(PBuf), AnsiChar('.'), Def, Result)
        else
          Result := Def;
      end;
  end;
end;

{**
  Converts SQL Unicode String into float value.
  Possible is SQLFloat, Float, Hex, Money+Suffix and ThousandSeparators
  @param Str an SQL AnsiString/RawByteString with comma or dot delimiter.
  @param Def a default value if the string can not be converted.
  @return a converted value or Def if conversion was failt.
}
function SQLStrToFloatDef(Value: PWideChar; const Def: Extended;
  Len: Integer = 0): Extended;
begin
  SQLStrToFloatDef(Value, Def, Result, Len);
end;

function CurrToUnicodeBuf(Value: PWideChar; Buffer: PWordArray; CodePoints: Integer): Boolean;
var
  I, ValidCount, InvalidPos, DotPos, CommaPos: Integer;
label Fail;
begin
  Result := True;
  DotPos := 0; CommaPos := 0; ValidCount := 0; InvalidPos := 0;
  FillChar(Pointer(Buffer)^, (CodePoints+1) shl 1, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  for i := 0 to CodePoints-1 do
    case (Value+i)^ of
      '0'..'9':
        begin
          Buffer[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      ',':
        if ((I-InvalidPos-DotPos) = 3) or ((DotPos=0) and (ValidCount > 0)) then //all others are invalid!
        begin
          CommaPos := I;
          if DotPos = 0 then
            Inc(ValidCount)
          else //align result eight Byte block and overwrite last ThousandSeparator
          begin
            PLongWord(@Buffer[DotPos-1])^ := PLongWord(@Buffer[DotPos])^; //Move first four byte block
            PLongWord(@Buffer[DotPos+1])^ := PLongWord(@Buffer[DotPos+2])^; //Move second four byte block
          end;
          Buffer[ValidCount-1] := Ord('.');
        end
        else
          goto Fail;
      '-', '+':
        begin
          Buffer[ValidCount] := Ord((Value+i)^);
          Inc(ValidCount);
        end;
      '.':
        begin
          if DotPos > 0 then //previously init so commapos can't be an issue here
            if (I-InvalidPos-DotPos) = 3 then //all others are invalid!
            begin
              PLongWord(@Buffer[DotPos-1])^ := PLongWord(@Buffer[DotPos])^; //Move first four byte block
              PLongWord(@Buffer[DotPos+1])^ := PLongWord(@Buffer[DotPos+2])^; //Move second four byte block
              Buffer[ValidCount-1] := Ord('.');
              Inc(InvalidPos);
            end
            else
              goto Fail
          else
            if I < CommaPos then
              goto Fail
            else
            begin
              Buffer[ValidCount] := Ord('.');
              Inc(ValidCount);
            end;
          DotPos := ValidCount;
        end;
      else
        if (ValidCount > 0) then
          if (Value+i)^ = ' ' then //641,22 $ f.e. (PostgreSQL)
            Break
          else
            goto Fail
        else
          InvalidPos := i+1;
    end;
  Exit;
Fail:
  Result := False;
end;

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Extended;
  out Result: Extended; Len: Integer);
var
  InvalidPos: Integer;
  DynBuf: TWordDynArray;
  StatBuf: Array[0..32] of Word;
  PBuf: PWordArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeExt(PWordArray(Value), WideChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (Ord((Value+Len*Ord(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        UnicodeToFloatDef(Value, WideChar(','), Def, Result)
      else
      begin
        if Len = 0 then
          {$IFDEF WITH_PWIDECHAR_STRLEN}
          Len := SysUtils.StrLen(Value)
          {$ELSE}
          Len := Length(Value)
          {$ENDIF}
        else
          if (Len < InvalidPos) and ((Value+InvalidPos-1)^ = ' ') then Exit;//fixed width str
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(PBuf), WideChar('.'), Def, Result)
        else
          Result := Def;
      end;
  end;
end;

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency;
  out Result: Currency; Len: Integer);
var InvalidPos: Integer;
begin
  if Assigned(Value) then begin
    if Len = 0 then
      {$IFDEF WITH_PWIDECHAR_STRLEN}
      Len := SysUtils.StrLen(Value);
      {$ELSE}
      Len := Length(Value);
      {$ENDIF}
    InvalidPos := Len;
    Result := ValUnicodeCurr(PWordArray(Value), '.', InvalidPos);
    if InvalidPos = Len then Exit;
    if InvalidPos < Len then begin//posible MoneyType
      if (Ord((Value+InvalidPos-1)^) = Ord(',')) and (Ord((Value+InvalidPos)^) in [Ord('0')..Ord('9')]) then begin //nope no money. Just a comma instead of dot.
        InvalidPos := Len;
        Result := ValUnicodeCurr(PWordArray(Value), ',', InvalidPos);
        if InvalidPos = Len then
          Exit;
      end;
      if Ord((Value+InvalidPos-1)^) = Ord(' ') then Exit;
    end;
  end;
  Result := Def;
end;

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Currency; Var DecimalSep: Char;
  out Result: Currency; Len: Integer); overload;
var InvalidPos: Integer;
begin
  if Assigned(Value) then begin
    if Len = 0 then
      {$IFDEF WITH_PWIDECHAR_STRLEN}
      Len := SysUtils.StrLen(Value);
      {$ELSE}
      Len := Length(Value);
      {$ENDIF}
    if DecimalSep = #0 then
      DecimalSep := '.';
    InvalidPos := Len;
    Result := ValUnicodeCurr(PWordArray(Value), DecimalSep, InvalidPos);
    if InvalidPos = Len then Exit;
    if InvalidPos < Len then begin//posible MoneyType
      if (Ord((Value+InvalidPos-1)^) in [Ord('.'), Ord(',')]) and (Ord((Value+InvalidPos-1)^) <> Ord(DecimalSep)) and
         (Ord((Value+InvalidPos)^) in [Ord('0')..Ord('9')]) then begin //nope no money. Just a comma instead of dot.
        InvalidPos := Len;
        if DecimalSep = '.'
        then DecimalSep := ','
        else DecimalSep := '.';
        Result := ValUnicodeCurr(PWordArray(Value), DecimalSep, InvalidPos);
        if InvalidPos = Len then Exit;
      end;
      if Ord((Value+InvalidPos-1)^) = Ord(' ') then Exit;
    end;
  end;
  Result := Def;
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure SQLStrToFloatDef(Value: PWideChar; const Def: Double;
  out Result: Double; Len: Integer);
var
  InvalidPos: Integer;
  DynBuf: TWordDynArray;
  StatBuf: Array[0..32] of Word;
  PBuf: PWordArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeDbl(PWordArray(Value), WideChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (Ord((Value+Len*Ord(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        UnicodeToFloatDef(Value, WideChar(','), Def, Result)
      else
      begin
        if Len = 0 then
          {$IFDEF WITH_PWIDECHAR_STRLEN}
          Len := SysUtils.StrLen(Value);
          {$ELSE}
          Len := Length(Value);
          {$ENDIF}
        if (InvalidPos > 1) and ((Value+InvalidPos-1)^ = ' ') then
          Exit;//fixed width str
        Result := Def;
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(PBuf), WideChar('.'), Def, Result);
      end;
  end;
end;
{$IFEND}

procedure SQLStrToFloatDef(Value: PWideChar; const Def: Single;
  out Result: Single; Len: Integer);
var
  InvalidPos: Integer;
  DynBuf: TWordDynArray;
  StatBuf: Array[0..32] of Word;
  PBuf: PWordArray;
begin
  Result := Def;
  if Assigned(Value) then
  begin
    Result := ValUnicodeSin(PWordArray(Value), WideChar('.'), InvalidPos);
    if InvalidPos <> 0 then //posible MoneyType
      if ((Value+InvalidPos-1)^ = ',') and (Ord((Value+Len*Ord(Len>0)-1)^) in [Ord('0')..Ord('9')]) then  //nope no money. Just a comma instead of dot.
        UnicodeToFloatDef(Value, WideChar(','), Def, Result)
      else
      begin
        if Len = 0 then
          {$IFDEF WITH_PWIDECHAR_STRLEN}
          Len := SysUtils.StrLen(Value);
          {$ELSE}
          Len := Length(Value);
          {$ENDIF}
        if (InvalidPos > 1) and ((Value+InvalidPos-1)^ = ' ') then
          Exit;//fixed width str
        Result := Def;
        if Len > SizeOf(StatBuf)-1 then begin
          SetLength(DynBuf, Len+1);
          PBuf := Pointer(DynBuf);
        end else
          PBuf := @StatBuf[0];
        if CurrToUnicodeBuf(Value, PBuf, Len) then
          UnicodeToFloatDef(PWideChar(Value), WideChar('.'), Def, Result);
      end;
  end;
end;

{ Convert string buffer into pascal string }
{$IFDEF UNICODE}
function BufferToStr(Buffer: PWideChar; Length: Integer): string;
{$ELSE}
function BufferToStr(Buffer: PAnsiChar; Length: Integer): string;
{$ENDIF}
begin
  Result := '';
  if Assigned(Buffer) then
    SetString(Result, Buffer, Length);
end;

function BufferToBytes(Buffer: Pointer; Length: Integer): TBytes;
begin
  SetLength(Result, Length);
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(Result)^, Length);
end;

{**
  Converts a string into boolean value.
  @param Str a RawByteString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(const Str: RawByteString; const CheckInt: Boolean = True): Boolean;
begin
  Result := StrToBoolEx(PAnsiChar(Pointer(Str)), CheckInt, False);
end;

{**
  Converts a string into boolean value.
  @param Str a PAnsiChar value.
  @param CheckInt Check for "0" char too?
  @param IgnoreTrailingSaces Ignore trailing spaces for fixed char fields f.e.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/'ON'/<>0
}
function StrToBoolEx(Str: PAnsiChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean;
label SkipSpaces;
begin
  Result := False;
  if Str <> nil then
    case Ord(Str^)  or $20 of //lower
      Ord('t'): //Check mixed case of 'true' or 't' string
        if PByte(Str+1)^ = Ord(#0) then
          Result := True
        else if (PByte(Str+1)^ = Ord(' ')) and IgnoreTrailingSaces then begin
          Inc(Str);
          goto SkipSpaces;
        end else if (PByte(Str+1)^ or $20 = Ord('r')) and (PByte(Str+2)^ or $20 = Ord('u'))
                and (PByte(Str+3)^ or $20 = Ord('e')) then
          if PByte(Str+4)^ = Ord(#0) then
            Result := True
          else if (PByte(Str+4)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,4);
            goto SkipSpaces;
          end;
      Ord('y'): //Check mixed case of 'Yes' or 'y' string
        if PByte(Str+1)^ = Ord(#0) then
          Result := True
        else if (PByte(Str+1)^ = Ord(' ')) and IgnoreTrailingSaces then begin
          Inc(Str);
          goto SkipSpaces;
        end else if (PByte(Str+1)^ or $20 = Ord('e')) and (PByte(Str+2)^ or $20 = Ord('s')) then
          if PByte(Str+3)^ = Ord(#0) then
            Result := True
          else if (PByte(Str+3)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,3);
SkipSpaces: while PByte(Str)^ = Ord(' ') do Inc(Str);
            Result := PByte(Str)^ = Ord(#0);
          end;
      Ord('o'): //Check mixed case of 'ON' or 'on' string
        if PByte(Str+1)^ or $20 = Ord('n') then
          if PByte(Str+2)^ = Ord(#0) then
            Result := True
          else if (PByte(Str+2)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,2);
            goto SkipSpaces;
          end;
      else
        Result := CheckInt and (RawToIntDef(Str, 0) <> 0);
    end;
end;

{**
  Converts a string into boolean value.
  @param Str a ZWideString value.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(const Str: ZWideString; const CheckInt: Boolean = True): Boolean;
begin
  Result := StrToBoolEx(PWideChar(Pointer(Str)), CheckInt, False);
end;

{**
  Converts a string into boolean value.
  @param Str a PWideChar value.
  @param CheckInt Check for "0" char too?
  @param IgnoreTrailingSaces Ignore trailing spaces for fixed char fields f.e.
  @return <code>True</code> is Str = 'Y'/'YES'/'T'/'TRUE'/<>0
}
function StrToBoolEx(Str: PWideChar; const CheckInt: Boolean = True;
  const IgnoreTrailingSaces: Boolean = True): Boolean;
label SkipSpaces;
begin
  Result := False;
  if Str <> nil then
    case Ord(Str^)  or $20 of //lower
      Ord('t'): //Check mixed case of 'true' or 't' string
        if PWord(Str+1)^ = Ord(#0) then
          Result := True
        else if (PWord(Str+1)^ = Ord(' ')) and IgnoreTrailingSaces then begin
          Inc(Str);
          goto SkipSpaces;
        end else if (PWord(Str+1)^ or $20 = Ord('r')) and (PWord(Str+2)^ or $20 = Ord('u'))
                and (PWord(Str+3)^ or $20 = Ord('e')) then
          if PWord(Str+4)^ = Ord(#0) then
            Result := True
          else if (PWord(Str+4)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,4);
            goto SkipSpaces;
          end;
      Ord('y'): //Check mixed case of 'Yes' or 'y' string
        if PWord(Str+1)^ = Ord(#0) then
          Result := True
        else if (PWord(Str+1)^ = Ord(' ')) and IgnoreTrailingSaces then begin
          Inc(Str);
          goto SkipSpaces;
        end else if (PWord(Str+1)^ or $20 = Ord('e')) and (PWord(Str+2)^ or $20 = Ord('s')) then
          if PWord(Str+3)^ = Ord(#0) then
            Result := True
          else if (PWord(Str+3)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,3);
SkipSpaces: while PWord(Str)^ = Ord(' ') do Inc(Str);
            Result := PWord(Str)^ = Ord(#0);
          end;
      Ord('o'): //Check mixed case of 'ON' or 'on' string
        if PWord(Str+1)^ or $20 = Ord('n') then
          if PWord(Str+2)^ = Ord(#0) then
            Result := True
          else if (PWord(Str+2)^ = Ord(' ')) and IgnoreTrailingSaces then begin
            Inc(Str,2);
            goto SkipSpaces;
          end;
      else
        Result := CheckInt and (UnicodeToIntDef(Str, 0) <> 0);
    end;
end;

{**
  Converts a boolean into string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToUnicodeEx(Value: Boolean): ZWideString;
begin
  Result := BoolStrsW[Value];
end;

{**
  Converts a boolean into string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToRawEx(Value: Boolean): RawByteString;
begin
  Result := BoolStrsRaw[Value];
end;

{**
  Converts a boolean into native string value.
  @param Bool a boolean value.
  @return <code>"True"</code> or <code>"False"</code>
}
function BoolToStrEx(Value: Boolean): string;
begin
  Result := BoolStrs[Value];
end;

{$IFDEF ENABLE_POSTGRESQL}
{**
  Checks if the specified string can represent an IP address.
  @param Str a string value.
  @return <code>True</code> if the string can represent an IP address
    or <code>False</code> otherwise.
}
function IsIpAddr(const Str: string): Boolean;
LAbel LExit;
var
  I, N: Integer;
  Splited: TStrings;
begin
  Result := False;
  Splited := SplitString(Str, '.');
  if Splited.Count <> 4 then
    goto LExit
  else
    for i := 0 to 3 do
    begin
      N := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Splited[i], -1);
      if (N < 0) or (N > 255) then
        goto LExit;
    end;
  Result := True;
  LExit: Splited.Free;
end;
{$ENDIF}

procedure SplitToStringList(List: TStrings; const AStr, Delimiters: string);
var
  PStart, PCurr, PEnd, PDelim: PChar;
  S: String;
begin
  //EH: 5x faster version of SplitToStringList
  if AStr = ''
  then Exit
  else if Delimiters = '' then begin
    List.Add(AStr);
    Exit;
  end;
  PStart := Pointer(AStr);
  PCurr := Pointer(AStr);
  PEnd := Pointer(AStr);
  Inc(PEnd, Length(AStr));
  while PCurr < PEnd do begin
    PDelim := Pointer(Delimiters);
    while PDelim^ <> #0 do
      if PDelim^ = PCurr^
      then Break
      else Inc(PDelim);
    if PDelim^ <> #0 then
      if PCurr > PStart then begin
        SetString(S, PStart, PCurr-PStart);
        List.Add(S);
        PStart := PCurr+1;
      end else
        inc(PStart, Ord(PStart^ = PDelim^));
    inc(PCurr)
  end;
  if PCurr > PStart then begin
    SetString(S, PStart, PCurr-PStart);
    List.Add(S);
  end;
end;

(*procedure SplitToStringList(List: TStrings; Str: string; const Delimiters: string);
var
  DelimPos: Integer;
  Str: string;
begin
  Str := AStr;
  repeat
    DelimPos := FirstDelimiter(Delimiters, Str);
    if DelimPos > 0 then
    begin
      if DelimPos > 1 then
        List.Add(Copy(Str, 1, DelimPos - 1));
      Str := Copy(Str, DelimPos + 1, Length(Str) - DelimPos);
      end
      else
      Break;
  until DelimPos <= 0;
  if Str <> '' then
    List.Add(Str);
end;*)

{**
  Splits string using the multiple chars.
  @param Str the source string
  @param Delimiters the delimiters string
  @return the result list where plased delimited string
}
function SplitString(const Str, Delimiters: string): TStrings;
begin
  Result := TStringList.Create;
  try
    SplitToStringList(Result, Str, Delimiters);
  except
    Result.Free;
    raise;
  end;
end;

{**
  Puts to list a splitted string using the multiple chars which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure PutSplitString(List: TStrings; const Str, Delimiters: string);
begin
  List.Clear;
  SplitToStringList(List, Str, Delimiters);
end;

{**
  Appends to list a splitted string using the multiple chars.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitString(List: TStrings; const Str, Delimiters: string);
begin
  SplitToStringList(List, Str, Delimiters);
end;

{**
  Composes a string from the specified strings list delimited with
  a special character.
  @param List a list of strings.
  @param Delimiter a delimiter string.
  @return a composed string from the list.
}
function ComposeString(List: TStrings; const Delimiter: string): string;
var
  i, Len, DelimLen: Integer;
  S: string;
  P: PChar;
begin
  DelimLen := Length(Delimiter);
  Len := 0;
  if List.Count > 0 then
  begin
    Inc(Len, Length(List[0]));
    for i := 1 to List.Count - 1 do
      Inc(Len, DelimLen + Length(List[i]));
  end;
  SetLength(Result, Len);
  P := Pointer(Result);
  for i := 0 to List.Count - 1 do
  begin
    if (i > 0) and (DelimLen > 0) then
    begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Delimiter)^, P^, DelimLen * SizeOf(Char));
      Inc(P, DelimLen);
    end;
    S := List[i];
    Len := Length(S);
    if Len > 0 then
    begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(S)^, P^, Len * SizeOf(Char));
      Inc(P, Len);
    end;
  end;
end;

{**
  Converts a float value into SQL string with '.' delimiter.
  @param Value a float value to be converted.
  @return a converted string value.
}
function FloatToSQLStr(Value: Extended): string;
begin
  Result := FloatToStr(Value, FmtSettFloatDot);
end;

function SQLStrToFloat(const Str: String): Extended;
begin
  Result := StrToFloat(Str, FmtSettFloatDot);
end;

{**
  Split a single string using the delimiter, appending the resulting strings
  to the List. (gto: New version, now unicode safe and without the bug which
  adds a blank line before the last found string)
  @param List a list to append the result.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure SplitToStringListEx(List: TStrings; const Str, Delimiter: string);
var P: PChar absolute Str;
  temp: string;
  i, OffSet: integer;
  PD: PChar;
  L, LD: LengthInt;
begin
  if Str = '' then Exit;
  L := Length(Str);
  PD := Pointer(Delimiter);
  if PD = nil then begin
    List.Add(Str);
    Exit;
  end;
  LD := Length(Delimiter);
  OffSet := 1;
  I := ZFastCode.PosEx(PD, P, LD, L, OffSet);
  while I > 0 do begin
    SetString(temp, (P+OffSet-1), (i-OffSet));
    if (temp <> '') or (List.Count > 0) then
      List.Add(temp);
    OffSet := I+LD;
    I := ZFastCode.PosEx(PD, P, LD, L, OffSet);
  end;
  if OffSet < L then
    if OffSet = 1
    then List.Add(Str)
    else begin
      SetString(temp, (P+OffSet-1), (L-OffSet+1));
      List.Add(temp);
    end;
end;

{**
  Puts to list a splitted string using the delimiter string which replaces
  the previous list content.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiter string
}
procedure PutSplitStringEx(List: TStrings; const Str, Delimiter: string);
begin
  List.Clear;
  SplitToStringListEx(List, Str, Delimiter);
end;

{**
  Splits string using the delimiter string.
  @param Str the source string
  @param Delimiters the delimiter string
  @return the result list where plased delimited string
}
function SplitStringEx(const Str, Delimiter: string): TStrings;
begin
  Result := TStringList.Create;
  try
    SplitToStringListEx(Result, Str, Delimiter);
  except
    Result.Free;
    raise;
  end;
end;

{**
  Appends to list a splitted string using the delimeter string.
  @param List a list with strings.
  @param Str the source string
  @param Delimiters the delimiters string
}
procedure AppendSplitStringEx(List: TStrings; const Str, Delimiter: string);
begin
  SplitToStringListEx(List, Str, Delimiter);
end;

{**
  Converts bytes into a AnsiString representation.
  @param Value an array of bytes to be converted.
  @return a converted AnsiString.
}
function BytesToStr(const Value: TBytes): RawByteString;
{$IF defined(MISS_RBS_SETSTRING_OVERLOAD) and not defined(WITH_TBYTES_AS_RAWBYTESTRING)}
var L: Integer;
{$IFEND}
begin
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  Result := Value;
  {$ELSE}
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    Result := EmptyRaw;
    L := Length(Value);
    SetLength(Result, L);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Result)^, L);
    {$ELSE}
    SetString(Result, PAnsiChar(@Value[0]), Length(Value))
    {$ENDIF}
  {$ENDIF}
end;

{**
  Converts AnsiString into an array of bytes.
  @param Value a AnsiString to be converted.
  @return a converted array of bytes.
}
{$IFNDEF NO_ANSISTRING}
function StrToBytes(const Value: AnsiString): TBytes;
var L: Integer;
begin
  L := Length(Value);
  SetLength(Result, L);
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Result)^, L*SizeOf(AnsiChar));
end;
{$ENDIF}

{**
  Converts a UTF8String into an array of bytes.
  @param Value a UTF8String to be converted.
  @return a converted array of bytes.
}
{$IF defined(WITH_RAWBYTESTRING) and not defined(NO_UTF8STRING)}
function StrToBytes(const Value: UTF8String): TBytes;
var L: Integer;
begin
  L := Length(Value);
  SetLength(Result, L);
  if Value <> '' then
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Result)^, L)
end;
{$IFEND}

{**
  Converts a RawByteString into an array of bytes.
  @param Value a RawByteString to be converted.
  @return a converted array of bytes.
}
{$IF defined(WITH_RAWBYTESTRING) and not defined(WITH_TBYTES_AS_RAWBYTESTRING)}
function StrToBytes(const Value: RawByteString): TBytes;
var L: Integer;
begin
  L := Length(Value);
  SetLength(Result, L);
  if Value <> '' then
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, Pointer(Result)^, L);
end;
{$IFEND}
{**
  Converts a WideString into an array of bytes.
  @param Value a String to be converted.
  @return a converted array of bytes.
}
{$IFDEF MSWINDOWS}
function StrToBytes(const Value: WideString): TBytes;
var
  L: Integer;
  RBS: RawByteString;
begin
  L := Length(Value);
  if L = 0 then
    Result := nil
  else
  begin
    RBS := UnicodeStringToASCII7(Value);
    SetLength(Result, L);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(RBS)^, Pointer(Result)^, L)
  end;
end;
{$ENDIF}

{**
  Converts a String into an array of bytes.
  @param Value a String to be converted.
  @return a converted array of bytes.
}
{$IFDEF PWIDECHAR_IS_PUNICODECHAR}
function StrToBytes(const Value: UnicodeString): TBytes;
var
  L: Integer;
  RBS: RawByteString;
begin
  L := Length(Value);
  if L = 0 then
    Result := nil
  else begin
    RBS := UnicodeStringToASCII7(Value);
    SetLength(Result, L);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(RBS)^, Pointer(Result)^, L)
  end;
end;
{$ENDIF}
{**
  Converts bytes into a variant representation.
  @param Value an array of bytes to be converted.
  @return a converted variant.
}
function BytesToVar(const Value: TBytes): Variant;
var
  I: Integer;
begin
  Result := VarArrayCreate([0, Length(Value) - 1], varByte);
  for I := 0 to Length(Value) - 1 do
    Result[I] := Value[I];
end;

{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
{$IFDEF WITH_NOT_INLINED_WARNING}{$PUSH}{$WARN 6058 off : Call to subroutine "operator := (const Source: Byte): Variant;" marked as inline is not inlined}{$ENDIF}
function BytesToVar(const Value: RawByteString): Variant;
var
  I: Integer;
  P: PByte;
begin
  Result := VarArrayCreate([0, Length(Value) - 1], varByte);
  P := Pointer(Value);
  for I := 0 to Length(Value) - 1 do begin
    Result[I] := P^;
    Inc(P);
  end;
end;
{$IFDEF WITH_NOT_INLINED_WARNING}{$POP}{$ENDIF}
{$ENDIF WITH_TBYTES_AS_RAWBYTESTRING}

{**
  Converts variant into an array of bytes.
  @param Value a varaint to be converted.
  @return a converted array of bytes.
}
function VarToBytes(const Value: Variant): TBytes;
var
  I: Integer;
begin
  if not (VarIsArray(Value) and (VarArrayDimCount(Value) = 1) and
     ((VarType(Value) and VarTypeMask) = varByte)) then
    raise Exception.Create(SInvalidVarByteArray);

  SetLength(Result, VarArrayHighBound(Value, 1) + 1);
  for I := 0 to VarArrayHighBound(Value, 1) do
    Result[I] := Value[I];
end;

{**
  Converts Ansi SQL Date/Time (yyyy-mm-dd hh:nn:ss or yyyy-mm-dd hh:nn:ss.zzz)
  to TDateTime
  @param Value a date and time string.
  @return a decoded TDateTime value.
}
function AnsiSQLDateToDateTime(P: PWideChar; L: LengthInt): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  DateFound: Boolean;
  tmp: TDateTime;
  procedure ExtractTime(P: PWideChar; L: LengthInt);
  begin
    Hour := UnicodeToIntDef(P, P+2, 0);
    Min := UnicodeToIntDef((P+3), (P+5), 0);
    Sec := UnicodeToIntDef((P+6), (p+8), 0);

    //if the time Length is bigger than 8, it can have milliseconds and it ...
    MSec := 0;
    if (L > 8) and ((P+8)^ = '.') then begin
      MSec := UnicodeToIntDef(P+9, (P+L), 0);
      L := L-9;
      if L < 3
      then MSec := MSec * MSecMulTable[L]
      else if L > 3 then
        MSec := MSec div MSecMulTable[3-L]
    end;
  end;
begin
  Result := 0;
  DateFound := False;
  if L >= 10 then begin
    Year := UnicodeToIntDef(P, (P+4), 0);
    Month := UnicodeToIntDef((P+5), (P+7), 0);
    Day := UnicodeToIntDef((P+8), (P+10), 0);

    if (Year <> 0) and (Month <> 0) and (Day <> 0) then begin
      if TryEncodeDate(Year, Month, Day, tmp) then begin
        Result := tmp;
        DateFound := True;
      end;
    end;
  end;

  if (L >= 18) or ( not DateFound ) then begin
    if DateFound
    then ExtractTime(P+11, L-11)
    else ExtractTime(P, L);
    if TryEncodeTime(Hour, Min, Sec, MSec, tmp) then
      if Result >= 0
      then Result := Result + Tmp
      else Result := Result - Tmp
  end;
end;

function AnsiSQLDateToDateTime(const Value: ZWideString): TDateTime;
var P: PWideChar;
begin
  P := Pointer(Value);
  if P = nil
  then Result := 0
  else Result := AnsiSQLDateToDateTime(P, Length(Value));
end;

{**
  Converts Ansi SQL Date/Time (yyyy-mm-dd hh:nn:ss or yyyy-mm-dd hh:nn:ss.zzz)
  to TDateTime
  @param Value a date and time string.
  @return a decoded TDateTime value.
}
function AnsiSQLDateToDateTime(P: PAnsiChar; L: LengthInt): TDateTime;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  DateFound: Boolean;
  tmp: TDateTime;
  procedure ExtractTime(P: PAnsiChar; L: LengthInt);
  begin
    Hour := RawToIntDef(P, P+2, 0);
    Min := RawToIntDef((P+3), (P+5), 0);
    Sec := RawToIntDef((P+6), (p+8), 0);

    //if the time Length is bigger than 8, it can have milliseconds and it ...
    MSec := 0;
    if (L > 8) and ((P+8)^ = '.') then begin
      MSec := RawToIntDef(P+9, (P+L), 0);
      L := L-9;
      if L < 3
      then MSec := MSec * MSecMulTable[L]
      else if L > 3 then
        MSec := MSec div MSecMulTable[3-L]
    end;
  end;
begin
  Result := 0;
  DateFound := False;
  if L >= 10 then begin
    Year := RawToIntDef(P, (P+4), 0);
    Month := RawToIntDef((P+5), (P+7), 0);
    Day := RawToIntDef((P+8), (P+10), 0);

    if (Year <> 0) and (Month <> 0) and (Day <> 0) then begin
      if TryEncodeDate(Year, Month, Day, tmp) then begin
        Result := tmp;
        DateFound := True;
      end;
    end;
  end;

  if (L >= 18) or ( not DateFound ) then begin
    if DateFound
    then ExtractTime(P+11, L-11)
    else ExtractTime(P, L);
    if TryEncodeTime(Hour, Min, Sec, MSec, tmp) then
      if Result >= 0
      then Result := Result + Tmp
      else Result := Result - Tmp
  end;
end;

function AnsiSQLDateToDateTime(const Value: RawByteString): TDateTime;
var P: PAnsiChar;
begin
  P := Pointer(Value);
  if P = nil
  then Result := 0
  else Result := AnsiSQLDateToDateTime(P, Length(Value));
end;

function CheckNumberRange(Value: AnsiChar; out Failed: Boolean): Byte; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Failed := not ((Ord(Value) >= Ord('0')) and (Ord(Value) <= Ord('9')));
  if Failed then
    Result := 0
  else
    Result := Ord(Value) - Ord('0');
end;

function CheckNumberRange(Value: WideChar; out Failed: Boolean): Word; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Failed := not ((Ord(Value) >= Ord('0')) and (Ord(Value) <= Ord('9')));
  if Failed then
    Result := 0
  else
    Result := Ord(Value) - Ord('0');
end;

function CheckNumberRange(Value: AnsiChar): Boolean; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result := ((Ord(Value) >= Ord('0')) and (Ord(Value) <= Ord('9')));
end;

function CheckNumberRange(Value: WideChar): Boolean; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result := ((Ord(Value) >= Ord('0')) and (Ord(Value) <= Ord('9')));
end;

{**
  Converts Ansi SQL Date (DateFormat)
  to TDateTime
  @param Value a date and time string.
  @param Dateformat a Pointer to DateFormat.
  @return a decoded TDateTime value.
}
function RawSQLDateToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
var
  Year, Month: Int64;
  Day: Word;
  DateFormat: PChar;

  procedure TryExtractDateFromFormat(Value: PAnsiChar);
  var
    I: Cardinal;
  begin
    Result := 0;
    Failed := ZFormatSettings.DateFormatLen = 0;
    if not Failed then
    begin
      Year := 0; Month := 0; Day := 0;
      for i := 0 to ZFormatSettings.DateFormatLen-1 do
      begin
        case DateFormat^ of
          'Y', 'y':
            begin
              Year := Year * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
              if Failed then Exit;
            end;
          'M', 'm':
            begin
              Month := Month * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
              if Failed then Exit;
            end;
          'D', 'd':
            begin
              Day := Day * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
              if Failed then Exit;
            end;
        end;
        Inc(DateFormat);
        Inc(Value);
        if I+1 = vallen then Break;
      end;
      Failed := not TryEncodeDate(Year, Month, Day, Result);
    end;
  end;

  procedure TryExtractDateFromUnknownSize;
  var
    DateLenCount: Cardinal;
    YPos, MPos, Code: Integer;
    FltVal: Extended;
  begin
    Result := 0;
    Failed := False;
    if Value <> nil then
    begin
      Year := 0; Month := 0; Day := 0;
      YPos := 0; MPos := 0; DateLenCount := 0;
      while ( DateLenCount < ValLen ) and (not (Ord((Value+DateLenCount)^) in [Ord('-'),Ord('/'),Ord('\'),Ord('.')]) ) do
      begin
        Year := Year * 10 + CheckNumberRange(AnsiChar((Value+DateLenCount)^), Failed);
        if Failed then Exit;
        Inc(DateLenCount);
        Inc(YPos);
      end;
      while ( DateLenCount < ValLen ) and (not CheckNumberRange(AnsiChar((Value+DateLenCount)^))) do
        Inc(DateLenCount);
      while ( DateLenCount < ValLen ) and (not (Ord((Value+DateLenCount)^) in [Ord('-'),Ord('/'),Ord('\')]) ) do
      begin
        Month := Month * 10 + CheckNumberRange(AnsiChar((Value+DateLenCount)^), Failed);
        if Failed then Exit;
        Inc(DateLenCount);
        Inc(MPos);
      end;
      while ( DateLenCount < ValLen ) and (not CheckNumberRange(AnsiChar((Value+DateLenCount)^))) do
        Inc(DateLenCount);
      while ( DateLenCount < ValLen ) and (not (Ord((Value+DateLenCount)^) in [Ord('-'),Ord('/'),Ord('\')]) ) do
      begin
        Day := Day * 10 + CheckNumberRange(AnsiChar((Value+DateLenCount)^), Failed);
        if Failed then Exit
        else Inc(DateLenCount);
      end;
      if MPos > 2 then //float ValueTmp
      begin
        Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(ValRawExt(Pointer(Value), AnsiChar('.'), Code));
        Failed := Code <> 0;
        if Failed then  Exit;
      end;
      if YPos > 4 then //We've a problem! No date delimiters found? -> YYYYMMDD or YYMMDD or ....Float!
        if YPos = 8 then
        begin
          //Let's start from the premise we've LongDateFormat YYYYMMDD
          Day := Year mod 100;
          Month := (Year mod 10000) div 100;
          Year := Year div 10000;
        end
        else
          if YPos = 6 then
          //Let's start from the premise we've ShortDateFormat YYMMDD
          begin
            Day := Year mod 100;
            Month := (Year mod 10000) div 100;
            Year := Year div 10000;
          end
          else
          begin
            Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(ValRawExt(Pointer(Value), AnsiChar('.'), Code));
            if Code <> 0 then
              Result := 0;
            Exit;
          end;
      Failed := not TryEncodeDate(Year, Month, Day, Result);
      if Failed then begin
        FltVal := ValRawExt(Pointer(Value), AnsiChar('.'), Code);
        Failed := (Code <> 0) or (FltVal = 0);
        if Failed
        then Result := 0
        else Result := Int(FltVal);
      end;
    end;
  end;
begin
  DateFormat := Pointer(ZFormatSettings.DateFormat);
  Failed := False;
  if (Value = nil) or (ValLen = 0) then
    Result := 0
  else
  begin
    TryExtractDateFromFormat(Value);
    if Failed and ( ZFormatSettings.DateFormatLen = 0 )then
      TryExtractDateFromUnknownSize;
  end;
end;

{**
  Converts Ansi SQL Date (DateFormat)
  to TDateTime
  @param Value a date and time string.
  @param Dateformat a Pointer to DateFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLDateToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
begin
  Result := RawSQLDateToDateTime(Pointer(UnicodeStringToASCII7(Value, ValLen)),
    ValLen, ZFormatSettings, Failed);
end;

{**
  Converts Ansi SQL Time (TimeFormat)
  to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
var
  Hour, Minute: Int64;
  Sec, MSec: Word;
  TimeFormat: PChar;

  procedure TryExtractTimeFromFormat(Value: PAnsiChar);
  var
    I: Cardinal;
  begin
    Result := 0;
    Failed := ( ZFormatSettings.TimeFormatLen = 0 );
    if not Failed then
    begin
      Hour := 0; Minute := 0; Sec := 0; MSec := 0;
      Failed := ( ZFormatSettings.TimeFormatLen = 0 ) and not (ValLen <= Byte(ZFormatSettings.TimeFormatLen-4));
      if not Failed then
      begin
        for i := 0 to ZFormatSettings.TimeFormatLen-1 do begin
          case TimeFormat^ of
            'H', 'h':
              begin
                Hour := Hour * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then Exit;
              end;
            'N', 'n':
              begin
                Minute := Minute * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then Exit;
              end;
            'S', 's':
              begin
                Sec := Sec * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then Exit;
              end;
            'Z', 'z':
              begin
                MSec := MSec * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if not Failed and ((TimeFormat+1)^ = TimeFormat^) and not (Ord((Value+1)^) in [Ord('0')..Ord('9')]) then begin
                  Inc(TimeFormat,Ord((TimeFormat+2)^ = TimeFormat^));
                  Msec := Msec * MSecMulTable[GetOrdinalDigits(Msec)];
                end;
                if Failed then Exit;
              end;
          end;
          Inc(TimeFormat);
          Inc(Value);
          if i+1 = ValLen then Break;
        end;
        Failed := not TryEncodeTime(Hour, Minute, Sec, MSec, Result);
      end;
    end;
  end;

  procedure TryExtractTimeFromVaryingSize;
  var
    HPos, NPos, Code: Integer;
    TimeLenCount: Cardinal;
  begin
    Result := 0;
    Failed := False;
    if Value <> nil then
    begin
      Hour := 0; Minute := 0; Sec := 0; MSec := 0;
      TimeLenCount := 0; HPos := 0; NPos := 0;
      while ( TimeLenCount < ValLen ) and (not (Ord((Value+TimeLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.')]) ) do
      begin
        Hour := Hour * 10 + CheckNumberRange(AnsiChar((Value+TimeLenCount)^), Failed);
        if Failed then Exit;
        Inc(HPos); Inc(TimeLenCount);
      end;
      while ( TimeLenCount < ValLen ) and (not CheckNumberRange(AnsiChar((Value+TimeLenCount)^))) do
        Inc(TimeLenCount);
      while ( TimeLenCount < ValLen ) and (not (Ord((Value+TimeLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.')]) ) do
      begin
        Minute := Minute * 10 + CheckNumberRange(AnsiChar((Value+TimeLenCount)^), Failed);
        if Failed then Exit;
        Inc(NPos); Inc(TimeLenCount);
      end;
      while ( TimeLenCount < ValLen ) and (not CheckNumberRange(AnsiChar((Value+TimeLenCount)^))) do
        Inc(TimeLenCount);
      while ( TimeLenCount < ValLen ) and (not (Ord((Value+TimeLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.')]) ) do
      begin
        Sec := Sec * 10 + CheckNumberRange(AnsiChar((Value+TimeLenCount)^), Failed);
        if Failed then Exit;
        Inc(TimeLenCount);
      end;
      while ( TimeLenCount < ValLen ) and (not CheckNumberRange(AnsiChar((Value+TimeLenCount)^)) ) do
        Inc(TimeLenCount);
      while ( TimeLenCount < ValLen ) and (not (Ord((Value+TimeLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.')]) ) do
      begin
        MSec := MSec * 10 + CheckNumberRange(AnsiChar((Value+TimeLenCount)^), Failed);
        if Failed then Exit;
        Inc(TimeLenCount);
      end;
      if NPos > 2 then //float value
      begin
        Result := Frac(ValRawExt(Pointer(Value), {$IFDEF NO_ANSICHAR}Ord{$ENDIF}('.'), Code));
        Failed := Code <> 0;
        if Failed then
          Result := 0;
        Exit;
      end;
      if HPos > 4 then //We've a problem! No date delimiters found? -> HHNNSSZZZ or HHNNSS or ....Float!
        case HPos of
          9:
            begin //Let's start from the premise we've LongTimeFormat HHNNSSZZZ
              MSec :=     Hour mod 1000;
              Sec :=     (Hour mod 100000)   div 1000;
              Minute :=  (Hour mod 10000000) div 100000;
              Hour := Hour div 10000000;
            end;
          8:
            begin //Let's start from the premise we've LongTimeFormat HHNNSSZZ
              MSec :=    Hour mod 100;
              Sec :=    (Hour mod 10000)   div 100;
              Minute := (Hour mod 1000000) div 10000;
              Hour := Hour div 1000000;
            end;
          7:
            begin //Let's start from the premise we've LongTimeFormat HHNNSSZ
              MSec :=    Hour mod 10;
              Sec :=    (Hour mod 1000)   div 10;
              Minute := (Hour mod 100000) div 1000;
              Hour := Hour div 100000;
            end;
          6:
            begin//Let's start from the premise we've ShortTimeFormat HHNNSS
              Sec := Hour mod 100;
              Minute := (Hour mod 10000) div 100;
              Hour := Hour div 10000;
            end
            else
            begin
              Result := Frac(ValRawExt(Pointer(Value), AnsiChar('.'), Code));
              Failed := Code <> 0;
              if Failed then Result := 0;
              Exit;
            end;
        end;
      Failed := not TryEncodeTime(Hour, Minute, Sec, MSec, Result);
      if Failed then begin
        Result := Frac(ValRawExt(Pointer(Value), AnsiChar('.'), Code));
        Failed := Code <> 0;
        if Failed then Result := 0;
      end;
    end;
  end;
begin
  Failed := False;
  TimeFormat := Pointer(ZFormatSettings.TimeFormat);
  if (Value = nil) or (ValLen = 0) then
    Result := 0
  else
  begin
    TryExtractTimeFromFormat(Value); //prefered. Adapts to given Format-Mask
    if Failed and ( ZFormatSettings.TimeFormatLen = 0 )then
      TryExtractTimeFromVaryingSize;
  end;
end;

{**
  Converts Unicode SQL Time (TimeFormat) to TDateTime
  @param Value a date and time string.
  @param Timeformat a TimeFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLTimeToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
begin
  Result := RawSQLTimeToDateTime(Pointer(UnicodeStringToAscii7(Value, ValLen)),
    ValLen, ZFormatSettings, Failed);
end;

{**
  Converts Ansi SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function RawSQLTimeStampToDateTime(Value: PAnsiChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
var
  Year, Month: Int64;
  Day, Hour, Minute, Sec, MSec: Word;
  YPos, MPos, HPos: Integer;
  TimeStampFormat: PChar;

  procedure CheckFailAndEncode;
  var timeval: TDateTime;
  begin
    Failed := not TryEncodeDate(Year, Month, Day, Result);
    if Failed then begin
      if ((Year or Month or Day) = 0) and ((Hour or Minute or Sec or MSec) <> 0) then
        Failed := not TryEncodeTime(Hour, Minute, Sec, MSec, Result);
    end else if TryEncodeTime(Hour, Minute, Sec, MSec, timeval) then
        if Result >= 0
        then Result := Result + timeval
        else Result := Result - timeval
  end;

  procedure TryExtractTimeStampFromFormat(Value: PAnsiChar);
  var
    I: Cardinal;
  begin
    Failed := ZFormatSettings.DateTimeFormatLen = 0;
    if not Failed then
    begin
      Failed  := (ValLen <= Byte(ZFormatSettings.DateTimeFormatLen-4));
      if not Failed then
      begin
        Year := 0; Month := 0; Day := 0;
        Hour := 0; Minute := 0; Sec := 0; MSec := 0;
        for i := 0 to ZFormatSettings.DateTimeFormatLen -1 do
        begin
          case TimeStampFormat^ of
            'Y', 'y':
              begin
                Year := Year * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then Exit;
              end;
            'M', 'm':
              begin
                Month := Month * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then Exit;
              end;
            'D', 'd':
              begin
                Day := Day * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then Exit;
              end;
            'H', 'h':
              begin
                Hour := Hour * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then Exit;
              end;
            'N', 'n':
              begin
                Minute := Minute * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then Exit;
              end;
            'S', 's':
              begin
                Sec := Sec * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then Exit;
              end;
            'Z', 'z':
              begin
                MSec := MSec * 10 + CheckNumberRange(AnsiChar(Value^), Failed);
                if Failed then
                begin
                  Failed := not (Ord(Value^) = Ord('+')); //postgres 2013-10-23 12:31:52.48+02 f.e.
                  if Failed then
                    Exit
                  else
                  begin
                    Msec := Msec div 10; //align result again
                    Break;
                  end;
                end else if ((TimeStampFormat+1)^ = TimeStampFormat^) and not (Ord((Value+1)^) in [Ord('0')..Ord('9')]) then begin
                  Inc(TimeStampFormat,Ord((TimeStampFormat+2)^ = TimeStampFormat^));
                  Msec := Msec * MSecMulTable[GetOrdinalDigits(Msec)];
                end;
              end;
            '.':
              if (Ord(Value^) = Ord('+')) then Break; //postgres 1997-02-25 00:00:00+01 f.e.
          end;
          Inc(TimeStampFormat);
          Inc(Value);
          if (i+1) = ValLen then Break;
        end;
        CheckFailAndEncode;
      end;
    end;
  end;

  procedure TryExtractTimeStampFromVaryingSize;
  var
    DotCount, Code: Integer;
    TimeStampLenCount: Cardinal;

    procedure ReadDate;
    begin
      { read date}
      while ( TimeStampLenCount < ValLen ) and (not (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) ) do
      begin
        Year := Year * 10 + CheckNumberRange(AnsiChar((Value+TimeStampLenCount)^), Failed);
        if Failed then Exit;
        Inc(YPos); Inc(TimeStampLenCount);
      end;
      if Ord((Value+TimeStampLenCount)^) = Ord('.') then
        Inc(DotCount); //possible float
      if (Ord((Value+TimeStampLenCount)^) = Ord(':')) and ( YPos < 3) then
      begin
        Hour := Year;
        Year := 0;
        Exit;
      end;
      while ( TimeStampLenCount < ValLen ) and (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) ) do
      begin
        Month := Month * 10 + CheckNumberRange(AnsiChar((Value+TimeStampLenCount)^), Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
        Inc(MPos);
      end;
      while ( TimeStampLenCount < ValLen ) and (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) ) do
      begin
        Day := Day * 10 + CheckNumberRange(AnsiChar((Value+TimeStampLenCount)^), Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
      end;
      while ( TimeStampLenCount < ValLen ) and (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) do
        Inc(TimeStampLenCount);
    end;

    procedure ReadTime;
    begin
      while ( TimeStampLenCount < ValLen ) and (not (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) ) do
      begin
        if HPos = 2 then //hour can't have 3 digits, date was aligned previously instead of time  > let's fix it
        begin
          MSec := Hour * 10 + CheckNumberRange(AnsiChar((Value+TimeStampLenCount)^), Failed);
          if Failed then Exit;
          Sec := Day; Day := 0;
          Minute := Month; Month := 0;
          Hour := Year; Year := 0;
          exit;
        end;
        Hour := Hour * 10 + CheckNumberRange(AnsiChar((Value+TimeStampLenCount)^), Failed);
        if Failed then Exit;
        Inc(HPos); Inc(TimeStampLenCount);
      end;
      while ( TimeStampLenCount < ValLen ) and (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) ) do
      begin
        Minute := Minute * 10 + CheckNumberRange(AnsiChar((Value+TimeStampLenCount)^), Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
      end;
      while ( TimeStampLenCount < ValLen ) and (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) ) do
      begin
        Sec := Sec * 10 + CheckNumberRange(AnsiChar((Value+TimeStampLenCount)^), Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
      end;
      while ( TimeStampLenCount < ValLen ) and (Ord((Value+TimeStampLenCount)^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) do
        Inc(TimeStampLenCount);
      while ( TimeStampLenCount < ValLen ) and (not (Ord(Value^) in [Ord(':'),Ord('-'),Ord('/'),Ord('\'),Ord('.'),Ord(' ')]) ) do
      begin
        MSec := MSec * 10 + CheckNumberRange(AnsiChar((Value+TimeStampLenCount)^), Failed);
        if Failed then Exit;
        Inc(TimeStampLenCount);
      end;
    end;

  begin
    Result := 0;
    Failed := False;
    if Value <> nil then
    begin
      Year := 0; Month := 0; Day := 0;
      Hour := 0; Minute := 0; Sec := 0; MSec := 0;
      YPos := 0; MPos := 0; HPos := 0; TimeStampLenCount := 0; DotCount := 0;
      ReadDate;
      {read time}
      if Failed then Exit
      else ReadTime;
      if Failed then Exit;

      if (MPos > 2) and ( DotCount = 1) then //float value
      begin
        Result := ValRawExt(Pointer(Value), AnsiChar('.'), Code);
        Failed := Code <> 0;
        if Failed then
          Result := 0;
        Exit;
      end;
      if YPos > 4 then //We've a problem! No date delimiters found? -> YYYYMMDDHHNNSSZZZ or ... or .. HHNNSS or ....Float!
        if YPos >= 14 then //YYYYMMDDHHNNSS +ZZZ?
        begin //Let's start from the premise we've LongTimeFormat HHNNSSZZ
          case YPos of
            17: //YYYYMMDDHHNNSSZZZ
              begin
                MSec := Year mod 1000;
                Year := Year div 1000;
              end;
            16: //YYYYMMDDHHNNSSZZ
              begin
                MSec := Year mod 100;
                Year := Year div 100;
              end;
            15: //YYYYMMDDHHNNSSZ
              begin
                MSec := Year mod 10;
                Year := Year div 10;
              end;
          end;
          //YYYYMMDDHHNNSS
          Sec := Year mod 100;
          Minute := (Year mod 10000) div 100;
          Hour := (Year mod 1000000) div 10000;
          Day := (Year mod 100000000) div 1000000;
          Month := (Year mod 10000000000) div 100000000;
          Year := Year div 10000000000;
        end
        else
          if YPos = 8 then //Date?
          begin
            //Let's start from the premise we've LongDateFormat YYYYMMDD
            Day := Year mod 100;
            Month := (Year mod 10000) div 100;
            Year := Year div 10000;
          end
          else
          if YPos = 6 then
          //Let's start from the premise we've ShortTimeFormat HHNNSS
          begin
            if MPos > 5 then
            begin
              case MPos of
                9: //HHNNSSZZZ
                  begin
                    MSec := Month mod 1000;
                    Month := Month div 1000;
                  end;
                8: //HHNNSSZZ
                  begin
                    MSec := Month mod 100;
                    Month := Month div 100;
                  end;
                7: //HHNNSSZ
                  begin
                    MSec := Month mod 100;
                    Month := Month div 100;
                  end;
              end;
              Sec := Month mod 100;
              Minute := (Month mod 10000) div 100;
              Hour := (Month mod 1000000) div 10000;
              Month := 0;
            end;
            Day := Year mod 100;
            Month := (Year mod 10000) div 100;
            Year := Year div 10000;
          end
          else
            if (DotCount = 1) or (DotCount = 0 ) then
            begin
              Result := ValRawExt(Pointer(Value), AnsiChar('.'), Code);
              Failed := ( Code <> 0 );
              if Failed then Result := 0;
              Exit;
            end
            else
            begin
              Failed := True;
              Exit;
            end;
      CheckFailAndEncode;
    end;
  end;
begin
  Failed := False;
  if (Value = nil) or (ValLen = 0) then
    Result := 0
  else
  begin
    TimeStampFormat := Pointer(ZFormatSettings.DateTimeFormat);
    TryExtractTimeStampFromFormat(Value);
    if Failed then
      TryExtractTimeStampFromVaryingSize;
  end;
end;

{**
  Converts Unicode SQL DateTime/TimeStamp (yyyy-mm-dd hh:nn:ss or
    yyyy-mm-dd hh:mm:nn.zzz or DateTimeFormat) to TDateTime
  @param Value a date and time string.
  @param DateTimeformat a DateTimeFormat.
  @return a decoded TDateTime value.
}
function UnicodeSQLTimeStampToDateTime(Value: PWideChar; const ValLen: Cardinal;
  const ZFormatSettings: TZFormatSettings; out Failed: Boolean): TDateTime;
begin
  Result := RawSQLTimeStampToDateTime(Pointer(UnicodeStringToAscii7(Value, ValLen)),
    ValLen, ZFormatSettings, Failed)
end;

{**
  Converts DateTime value into a raw encoded string with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then DateFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::date.
  @return a formated RawByteString with Date-Format pattern.
}
function DateTimeToRawSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString;
var L, L2, Year, Month, Day: Word;
  Buffer: array[0..11] of AnsiChar;
  P: PAnsiChar;
begin
  DecodeDate(Value, Year, Month, Day);
  L := DateTimeToRawSQLDate(Year, Month, Day, @Buffer[0],
    ConFormatSettings.DateFormat, Quoted, False);
  l2 := Length(Suffix);
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  ZSetString(nil, l+l2, Result);
  {$ELSE}
  System.SetString(Result, nil , L+L2);
  {$ENDIF}
  P := Pointer(Result);
  Move(Buffer[0], P^, L);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2);
end;

{** EH:
  Converts DateTime value into a raw buffer with format pattern
  @param Value a TDateTime value.
  @param Buf the raw buffer to write in.
  @param ConFormatSettings then DateFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::date.
  @return the length in bytes of written value.
}
function DateTimeToRawSQLDate(const Value: TDateTime; Buf: PAnsichar;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): Word;
var L, Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := DateTimeToRawSQLDate(Year, Month, Day, Buf,
    ConFormatSettings.DateFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L);
    Result := Result+L;
  end;
end;

{** EH:
  Converts date values into a buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  I'm aware year/month/day with value 0 do not exist but MySQL f.e. allows it!
  @param Year a Year value with range of 0..9999.
  @param Month a Month value with range of 0..12.
  @param Day a Day value with range of 0..31.
  @param Buf a raw buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @param Negative if the date is negative (i.e. bc).
  @return the length in bytes of written value.
}
function DateTimeToRawSQLDate(Year, Month, Day: Word; Buf: PAnsichar;
  const Format: String; Quoted, Negative: Boolean): Byte; overload;
var PStart: PAnsiChar;
  PFormat, PEnd: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2, EQ3, EQ4: Boolean; //equals to C1?
label inc_dbl; //keep code tiny
begin
  PFormat := Pointer(Format);
  if PFormat = nil then begin
    Result := 0;
    Exit;
  end;
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(Negative));
  if Negative then
    PByte(Buf-1)^ := Ord('-');
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1; //possibly read #0
    case C1 of
      Ord('y'): begin
                  { don't read over buffer }
                  EQ3 := EQ2 and (PFormat+2 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+2)^ or $20 = C1);
                  EQ4 := EQ3 and (PFormat+3 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+3)^ or $20 = C1);
                  if EQ4 or (Year >= 1000) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PWord(Buf)^   := TwoDigitLookupW[Result];
                    PWord(Buf+2)^ := TwoDigitLookupW[Year-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^   := TwoDigitLookupW[Year div 100];
                    PWord(Buf+2)^ := TwoDigitLookupW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 4);
                    Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3)+Ord(EQ4));
                    Continue;
                  end else if EQ3 or (Year >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PByte(Buf)^   := Ord('0')+Result;
                    PWord(Buf+1)^ := TwoDigitLookupW[Year-(Result*100)];
                    {$ELSE}
                    PByte(Buf)^   := Ord('0')+Year div 100;
                    PWord(Buf+1)^ := TwoDigitLookupW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 3);
                    Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3));
                    Continue;
                  end else if EQ2 or (Year >= 10) then begin
                    PWord(Buf)^ := TwoDigitLookupW[Year];
Inc_dbl:            Inc(Buf, 2);
                    Inc(PFormat, 1+Ord(EQ2));
                    Continue;
                  end else
                    PByte(Buf)^ := Ord('0') + Year;
                end;
      Ord('m'): if EQ2 or (Month >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Month];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Month;
      Ord('d'): if EQ2 or (Day >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Day];
                  goto inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Day;
      else      PByte(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PByte(PStart)^:= Ord(#39);
    PByte(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

{** EH:
  Converts date value into a UCS2 buffer with format pattern
  @param Value a TDateTime value.
  @param Buf the UCS2 buffer to write in.
  @param ConFormatSettings then DateFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::date.
  @return the length in code-points of written value.
}
function DateTimeToUnicodeSQLDate(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): word;
var L, Year, Month, Day: Word;
begin
  DecodeDate(Value, Year, Month, Day);
  Result := DateTimeToUnicodeSQLDate(Year, Month, Day, Buf,
    ConFormatSettings.DateFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L shl 1);
    Result := Result+L;
  end;
end;

{** EH:
  Converts date values into a buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  I'm aware year/month/day with value 0 do not exist but MySQL f.e. allows it!
  @param Year a Year value with range of 0..9999.
  @param Month a Month value with range of 0..12.
  @param Day a Day value with range of 0..31.
  @param Buf a UCS2 buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @param Negative if the date is negative (i.e. bc).
  @return the length in code-points of written value.
}
function DateTimeToUnicodeSQLDate(Year, Month, Day: Word; Buf: PWideChar;
  const Format: String; Quoted, Negative: Boolean): Byte; overload;
var PStart: PWideChar;
  PFormat, PEnd: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2, EQ3, EQ4: Boolean; //equals to C1?
label inc_dbl; //keep code tiny
begin
  PFormat := Pointer(Format);
  if PFormat = nil then begin
    Result := 0;
    Exit;
  end;
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(Negative));
  if Negative then
    PWord(Buf-1)^ := Ord('-');
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1; //possibly read #0
    case C1 of
      Ord('y'): begin
                  { don't read over buffer }
                  EQ3 := EQ2 and (PFormat+2 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+2)^ or $20 = C1);
                  EQ4 := EQ3 and (PFormat+3 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+3)^ or $20 = C1);
                  if EQ4 or (Year >= 1000) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PLongWord(Buf)^   := TwoDigitLookupLW[Result];
                    PLongWord(Buf+2)^ := TwoDigitLookupLW[Year-(Result*100)];
                    {$ELSE}
                    PLongWord(Buf)^   := TwoDigitLookupLW[Year div 100];
                    PLongWord(Buf+2)^ := TwoDigitLookupLW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 4); Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3)+Ord(EQ4));
                    continue;
                  end else if EQ3 or (Year >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PWord(Buf)^   := Ord('0')+Result;
                    PLongWord(Buf+1)^ := TwoDigitLookupLW[Year-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^   := Ord('0')+Year div 100;
                    PLongWord(Buf+1)^ := TwoDigitLookupLW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 3); Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3));
                    continue;
                  end else if (Year >= 10) or EQ2 then begin
                    PLongWord(Buf)^ := TwoDigitLookupLW[Year];
Inc_dbl:            Inc(Buf, 2);
                    Inc(PFormat, 1+Ord(EQ2));
                    continue;
                  end else
                    PWord(Buf)^ := Ord('0') + Year;
                end;
      Ord('m'): if EQ2 or (Month >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Month];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Month;
      Ord('d'): if EQ2 or (Day >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Day];
                  goto Inc_Dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Day;
      else      PWord(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PWord(PStart)^:= Ord(#39);
    PWord(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

{** EH:
  Converts date value into a WideString/UnicodeString with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then DateFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::date.
  @return a formated WideString/UnicodeString with Date-Format pattern.
}
function DateTimeToUnicodeSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString): ZWideString;
var L, L2, Year, Month, Day: Word;
  Buffer: array[0..11] of WideChar;
  P: PWideChar;
begin
  DecodeDate(Value, Year, Month, Day);
  L := DateTimeToUnicodeSQLDate(Year, Month, Day, @Buffer[0],
    ConFormatSettings.DateFormat, Quoted, False);
  l2 := Length(Suffix);
  System.SetString(Result, nil , L+L2);
  P := Pointer(Result);
  Move(Buffer[0], P^, L shl 1);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2 shl 1);
end;

{**
  Converts DateTime value to native string
}
function DateTimeToSQLDate(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: string): string;
begin
  Result := {$IFDEF UNICODE} DateTimeToUnicodeSQLDate {$ELSE} DateTimeToRawSQLDate {$ENDIF} (Value, ConFormatSettings, Quoted, Suffix);
end;

{** EH:
  Converts time value into a raw encoded string with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::time.
  @return a formated RawByteString with Time-Format pattern.
}
function DateTimeToRawSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString;
var l, l2, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..15] of AnsiChar;
  P: PAnsiChar;
begin
  DecodeTime(Value, Hour, Minute, Second, MSec);
  L := DateTimeToRawSQLTime(Hour, Minute, Second, MSec, @Buffer[0],
    ConFormatSettings.TimeFormat, Quoted);
  l2 := Length(Suffix);
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  ZSetString(nil, l+l2, Result);
  {$ELSE}
  System.SetString(Result, nil , L+L2);
  {$ENDIF}
  P := Pointer(Result);
  Move(Buffer[0], P^, L);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2);
end;

{** EH:
  Converts a time value into a raw buffer with format pattern
  @param Value a TDateTime value.
  @param Buf the raw buffer to write in.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::time.
  @return the length in bytes of written value.
}
function DateTimeToRawSQLTime(const Value: TDateTime; Buffer: PAnsichar;
  const ConFormatSettings: TZFormatSettings;
  Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): Word;
var l, Hour, Minute, Second, MSec: Word;
begin
  DecodeTime(Value, Hour, Minute, Second, MSec);
  Result := DateTimeToRawSQLTime(Hour, Minute, Second, MSec, Buffer,
    ConFormatSettings.TimeFormat, Quoted);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buffer+Result)^, L);
    Result := Result+L;
  end;
end;

{** EH:
  Converts a time values into a raw buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  @param Hour a hour value with range of 0..23.
  @param Minute a minute value with range of 0..59.
  @param Second a second value with range of 0..59.
  @param MSec a millisecond value with range of 0..999.
  @param Buf the raw buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @return the length in bytes of written value.
}
function DateTimeToRawSQLTime(Hour, Minute, Second, MSec: Word;
  Buf: PAnsichar; const Format: String; Quoted: Boolean): Byte;
var PStart: PAnsiChar;
  PFormat, PEnd: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2, EQ3: Boolean;
label inc_dbl; //keep code tiny
begin
  PStart := Buf;
  Inc(Buf, Ord(Quoted));
  PFormat := Pointer(Format);
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1;
    case C1 of
      Ord('h'): if EQ2 or (Hour >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Hour];
Inc_dbl:          Inc(Buf, 2);
                  Inc(PFormat, 1+Ord(EQ2));
                  Continue;
                end else
                  PByte(Buf)^ := Ord('0') + Hour;
      Ord('n'),
      Ord('m'): if EQ2 or (Minute >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Minute];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Minute;
      Ord('s'): if EQ2 or (Second >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Second];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Second;
      Ord('z'): begin
                  EQ3 := EQ2 and (PFormat+2 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+2)^ or $20 = C1);
                  if EQ3 or (MSec >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := MSec div 100;
                    PByte(Buf)^   := Ord('0')+Result;
                    PWord(Buf+1)^ := TwoDigitLookupW[MSec-(Result*100)];
                    {$ELSE}
                    PByte(Buf)^   := Ord('0')+MSec div 100;
                    PWord(Buf+1)^ := TwoDigitLookupW[MSec mod 100];
                    {$ENDIF}
                    Inc(Buf, 3); Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3));
                    continue;
                  end else if EQ2 or (MSec > 9) then begin
                    PWord(Buf)^ := TwoDigitLookupW[MSec];
                    goto Inc_dbl;
                  end else
                    PByte(Buf)^ := Ord('0') + MSec;
                end;
      else      PByte(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PByte(PStart)^:= Ord(#39);
    PByte(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

{** EH:
  Converts a time value into a WideString/UnicodeString with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::time.
  @return a formated WideString/UnicodeString with Time-Format pattern.
}
function DateTimeToUnicodeSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString): ZWideString;
var l, l2, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..15] of WideChar;
  P: PWideChar;
begin
  DecodeTime(Value, Hour, Minute, Second, MSec);
  L := DateTimeToUnicodeSQLTime(Hour, Minute, Second, MSec, @Buffer[0],
    ConFormatSettings.TimeFormat, Quoted);
  l2 := Length(Suffix);
  System.SetString(Result, nil , L+L2);
  P := Pointer(Result);
  Move(Buffer[0], P^, L shl 1);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2 shl 1);
end;

{** EH:
  Converts a time value into a UCS2 buffer with format pattern
  @param Value a TDateTime value.
  @param Buf then UCS2 buffer to write in.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::time.
  @return the length in codepoints of written value.
}
function DateTimeToUnicodeSQLTime(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): Word;
var l, Hour, Minute, Second, MSec: Word;
begin
  DecodeTime(Value, Hour, Minute, Second, MSec);
  Result := DateTimeToUnicodeSQLTime(Hour, Minute, Second, MSec, Buf,
    ConFormatSettings.TimeFormat, Quoted);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L shl 1);
    Result := Result+L;
  end;
end;

{** EH:
  Converts a time values into a UCS2 buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  @param Hour a hour value with range of 0..23.
  @param Minute a minute value with range of 0..59.
  @param Second a second value with range of 0..59.
  @param MSec a millisecond value with range of 0..999.
  @param Buf the unicode buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @return the length in codepoints of written value.
}
function DateTimeToUnicodeSQLTime(Hour, Minute, Second, MSec: Word;
  Buf: PWideChar; const Format: String; Quoted: Boolean): Byte;
var PStart: PWideChar;
  PFormat, PEnd: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2, EQ3: Boolean;
label inc_dbl; //keep code tiny
begin
  PStart := Buf;
  Inc(Buf, Ord(Quoted));
  PFormat := Pointer(Format);
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1;
    case C1 of
      Ord('h'): if EQ2 or (Hour >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Hour];
Inc_dbl:          Inc(Buf, 2);
                  Inc(PFormat, 1+Ord(EQ2));
                  continue;
                end else
                  PWord(Buf)^ := Ord('0') + Hour;
      Ord('m'),
      Ord('n'): if EQ2 or (Minute >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Minute];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Minute;
      Ord('s'): if EQ2 or (Second >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Second];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Second;
      Ord('z'): begin
                  EQ3 := EQ2 and (PFormat+2 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+2)^ or $20 = C1);
                  if EQ3 or (MSec >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := MSec div 100;
                    PWord(Buf)^       := Ord('0')+Result;
                    PLongWord(Buf+1)^ := TwoDigitLookupLW[MSec-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^       := Ord('0')+MSec div 100;
                    PLongWord(Buf+1)^ := TwoDigitLookupLW[MSec mod 100];
                    {$ENDIF}
                    Inc(Buf, 3); Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3));
                    continue;
                  end else if EQ2 or (MSec > 9) then begin
                    PLongWord(Buf)^ := TwoDigitLookupLW[MSec];
                    goto Inc_dbl
                  end else
                    PWord(Buf)^ := Ord('0') + MSec;
                end
      else      PWord(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat)
  end;
  if Quoted then begin
    PWord(PStart)^:= Ord(#39);
    PWord(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

{**
  Converts DateTime value to native string
}
function DateTimeToSQLTime(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: string): string;
begin
  Result := {$IFDEF UNICODE} DateTimeToUnicodeSQLTime {$ELSE} DateTimeToRawSQLTime {$ENDIF} (Value, ConFormatSettings, Quoted, Suffix);
end;

{** EH:
  Converts datetime value into a raw encoded string with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then DateTimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::timestamp.
  @return a formated RawByteString with DateTime-Format pattern.
}
function DateTimeToRawSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: RawByteString = EmptyRaw): RawByteString;
var l, l2, Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..31] of AnsiChar;
  P: PAnsiChar;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  L := DateTimeToRawSQLTimeStamp(Year, Month, Day, Hour, Minute, Second, MSec,
    @Buffer[0], ConFormatSettings.DateTimeFormat, Quoted, False);
  l2 := Length(Suffix);
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  ZSetString(nil, l+l2, Result);
  {$ELSE}
  System.SetString(Result, nil, L+L2);
  {$ENDIF}
  P := Pointer(Result);
  Move(Buffer[0], P^, L);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2);
end;

{** EH:
  Converts datetime value into a raw encoded buffer with format pattern
  @param Value a TDateTime value.
  @param Buf the raw buffer we write in.
  @param ConFormatSettings then DateTimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::timestamp.
  @return a formated RawByteString with DateTime-Format pattern.
}
function DateTimeToRawSQLTimeStamp(const Value: TDateTime; Buf: PAnsiChar;
  const ConFormatSettings: TZFormatSettings; Quoted: Boolean;
  const Suffix: RawByteString = EmptyRaw): Word;
var l, Year, Month, Day, Hour, Minute, Second, MSec: Word;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  Result := DateTimeToRawSQLTimeStamp(Year, Month, Day, Hour, Minute, Second,
    MSec, Buf, ConFormatSettings.DateTimeFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L);
    Result := Result+L;
  end;
end;

{** EH:
  Converts date and time values into a raw buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  I'm aware year/month/day with value 0 do not exist but MySQL f.e. allows it!
  @param Year a Year value with range of 0..9999.
  @param Month a Month value with range of 0..12.
  @param Day a Day value with range of 0..31.
  @param Hour a hour value with range of 0..23.
  @param Minute a minute value with range of 0..59.
  @param Second a second value with range of 0..59.
  @param MSec a millisecond value with range of 0..999.
  @param Buf the raw buffer we write in.
  @param Format the result format.
  @param Quoted if the result should be quoted.
  @param Negative if the date is negative (i.e. bc).
  @return the length in bytes of written value.
}
function DateTimeToRawSQLTimeStamp(Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buf: PAnsiChar; const Format: String; Quoted, Negative: Boolean): Byte; overload;
var PStart: PAnsiChar;
  PFormat, PEnd: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2, EQ3, EQ4: Boolean; //equals to C1?
label inc_dbl, inc_trpl; //keep code tiny
begin
  PFormat := Pointer(Format);
  if PFormat = nil then begin
    Result := 0;
    Exit;
  end;
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(Negative));
  if Negative then
    PByte(Buf-1)^ := Ord('-');
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1; //possibly read #0
    case C1 of
      Ord('y'): begin
                  { don't read over buffer }
                  EQ3 := EQ2 and (PFormat+2 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+2)^ or $20 = C1);
                  EQ4 := EQ3 and (PFormat+3 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+3)^ or $20 = C1);
                  if EQ4 or (Year >= 1000) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PWord(Buf)^   := TwoDigitLookupW[Result];
                    PWord(Buf+2)^ := TwoDigitLookupW[Year-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^   := TwoDigitLookupW[Year div 100];
                    PWord(Buf+2)^ := TwoDigitLookupW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 4);
                    Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3)+Ord(EQ4));
                    Continue;
                  end else if EQ3 or (Year >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PByte(Buf)^   := Ord('0')+Result;
                    PWord(Buf+1)^ := TwoDigitLookupW[Year-(Result*100)];
                    {$ELSE}
                    PByte(Buf)^   := Ord('0')+Year div 100;
                    PWord(Buf+1)^ := TwoDigitLookupW[Year mod 100];
                    {$ENDIF}
inc_trpl:           Inc(Buf, 3);
                    Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3));
                    Continue;
                  end else if EQ2 or (Year >= 10) then begin
                    PWord(Buf)^ := TwoDigitLookupW[Year];
Inc_dbl:            Inc(Buf, 2);
                    Inc(PFormat, 1+Ord(EQ2));
                    Continue;
                  end else
                    PByte(Buf)^ := Ord('0') + Year;
                end;
      Ord('m'): if EQ2 or (Month >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Month];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Month;
      Ord('d'): if EQ2 or (Day >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Day];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Day;
      Ord('h'): if EQ2 or (Hour >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Hour];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Hour;
      Ord('n'): if EQ2 or (Minute >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Minute];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Minute;
      Ord('s'): if EQ2 or (Second >= 10) then begin
                  PWord(Buf)^ := TwoDigitLookupW[Second];
                  goto Inc_dbl;
                end else
                  PByte(Buf)^ := Ord('0') + Second;
      Ord('z'): begin
                  EQ3 := EQ2 and (PFormat+2 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+2)^ or $20 = C1);
                  if EQ3 or (MSec >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := MSec div 100;
                    PByte(Buf)^   := Ord('0')+Result;
                    PWord(Buf+1)^ := TwoDigitLookupW[MSec-(Result*100)];
                    {$ELSE}
                    PByte(Buf)^   := Ord('0')+MSec div 100;
                    PWord(Buf+1)^ := TwoDigitLookupW[MSec mod 100];
                    {$ENDIF}
                    goto inc_trpl;
                  end else if EQ2 or (MSec > 9) then begin
                    PWord(Buf)^ := TwoDigitLookupW[MSec];
                    goto Inc_dbl;
                  end else
                    PByte(Buf)^ := Ord('0') + MSec;
                end;
      else      PByte(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PByte(PStart)^:= Ord(#39);
    PByte(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

{** EH:
  Converts a datetime value into a UCS2 buffer with format pattern
  @param Value a TDateTime value.
  @param Buf then UCS2 buffer to write in.
  @param ConFormatSettings then TimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::timestamp.
  @return the length in codepoints of written value.
}
function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime; Buf: PWideChar;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): Word;
var l, Year, Month, Day, Hour, Minute, Second, MSec: Word;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  Result := DateTimeToUnicodeSQLTimeStamp(Year, Month, Day, Hour, Minute, Second,
    MSec, Buf, ConFormatSettings.DateTimeFormat, Quoted, False);
  L := Length(Suffix);
  if L > 0 then begin
    Move(Pointer(Suffix)^, (Buf+Result)^, L shl 1);
    Result := Result+L;
  end;
end;

{** EH:
  Converts date and time values into a UCS2 buffer with format pattern
  We don't take care for the ranges of the Values. That's users turn to do!
  I'm aware year/month/day with value 0 do not exist but MySQL f.e. allows it!
  @param Year a Year value with range of 0..9999.
  @param Month a Month value with range of 0..12.
  @param Day a Day value with range of 0..31.
  @param Hour a hour value with range of 0..23.
  @param Minute a minute value with range of 0..59.
  @param Second a second value with range of 0..59.
  @param MSec a millisecond value with range of 0..999.
  @param Buf the unicode buffer we write in.
  @param Format the !valid! result format.
  @param Quoted if the result should be quoted.
  @param Negative if the date is negative (i.e. bc).
  @return the length in bytes of written value.
}
function DateTimeToUnicodeSQLTimeStamp(Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buf: PWideChar; const Format: String; Quoted, Negative: Boolean): Byte;
var PStart: PWideChar;
  PFormat, PEnd: PChar;
  C1: {$IFDEF UNICODE}Word{$ELSE}Byte{$ENDIF};
  EQ2, EQ3, EQ4: Boolean; //equals to C1?
label inc_dbl, inc_trpl; //keep code tiny
begin
  PFormat := Pointer(Format);
  if PFormat = nil then begin
    Result := 0;
    Exit;
  end;
  PStart := Buf;
  Inc(Buf, Ord(Quoted)+Ord(Negative));
  if Negative then
    PWord(Buf-1)^ := Ord('-');
  PEnd := PFormat + Length(Format);
  while PEnd > PFormat do begin
    C1 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^ or $20;
    EQ2 := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+1)^ or $20 = C1; //possibly read #0
    case C1 of
      Ord('y'): begin
                  { don't read over buffer }
                  EQ3 := EQ2 and (PFormat+2 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+2)^ or $20 = C1);
                  EQ4 := EQ3 and (PFormat+3 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+3)^ or $20 = C1);
                  if EQ4 or (Year >= 1000) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PLongWord(Buf)^   := TwoDigitLookupLW[Result];
                    PLongWord(Buf+2)^ := TwoDigitLookupLW[Year-(Result*100)];
                    {$ELSE}
                    PLongWord(Buf)^   := TwoDigitLookupLW[Year div 100];
                    PLongWord(Buf+2)^ := TwoDigitLookupLW[Year mod 100];
                    {$ENDIF}
                    Inc(Buf, 4);
                    Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3)+Ord(EQ4));
                    Continue;
                  end else if EQ3 or (Year >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := Year div 100;
                    PWord(Buf)^   := Ord('0')+Result;
                    PLongWord(Buf+1)^ := TwoDigitLookupLW[Year-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^   := Ord('0')+Year div 100;
                    PLongWord(Buf+1)^ := TwoDigitLookupLW[Year mod 100];
                    {$ENDIF}
inc_trpl:           Inc(Buf, 3);
                    Inc(PFormat, 1+Ord(EQ2)+Ord(EQ3));
                    Continue;
                  end else if EQ2 or (Year >= 10) then begin
                    PLongWord(Buf)^ := TwoDigitLookupLW[Year];
Inc_dbl:            Inc(Buf, 2);
                    Inc(PFormat, 1+Ord(EQ2));
                    Continue;
                  end else
                    PWord(Buf)^ := Ord('0') + Year;
                end;
      Ord('m'): if EQ2 or (Month >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Month];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Month;
      Ord('d'): if EQ2 or (Day >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Day];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Day;
      Ord('h'): if EQ2 or (Hour >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Hour];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Hour;
      Ord('n'): if EQ2 or (Minute >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Minute];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Minute;
      Ord('s'): if EQ2 or (Second >= 10) then begin
                  PLongWord(Buf)^ := TwoDigitLookupLW[Second];
                  goto Inc_dbl;
                end else
                  PWord(Buf)^ := Ord('0') + Second;
      Ord('z'): begin
                  EQ3 := EQ2 and (PFormat+2 < PEnd) and
                    ({$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat+2)^ or $20 = C1);
                  if EQ3 or (MSec >= 100) then begin
                    {$IFNDEF HAVE_REZIPROKE_MOD100}
                    Result := MSec div 100;
                    PWord(Buf)^   := Ord('0')+Result;
                    PLongWord(Buf+1)^ := TwoDigitLookupLW[MSec-(Result*100)];
                    {$ELSE}
                    PWord(Buf)^   := Ord('0')+MSec div 100;
                    PLongWord(Buf+1)^ := TwoDigitLookupLW[MSec mod 100];
                    {$ENDIF}
                    goto inc_trpl;
                  end else if EQ2 or (MSec > 9) then begin
                    PLongWord(Buf)^ := TwoDigitLookupLW[MSec];
                    goto Inc_dbl;
                  end else
                    PWord(Buf)^ := Ord('0') + MSec;
                end;
      else      PWord(Buf)^ := {$IFDEF UNICODE}PWord{$ELSE}PByte{$ENDIF}(PFormat)^;
    end;
    Inc(Buf);
    Inc(PFormat);
  end;
  if Quoted then begin
    PWord(PStart)^:= Ord(#39);
    PWord(Buf)^   := Ord(#39);
    Result := Buf-PStart+1;
  end else
    Result := Buf-PStart;
end;

{** EH:
  Converts datetime value into a Unicode/Widestring with format pattern
  @param Value a TDateTime value.
  @param ConFormatSettings then DateTimeFormat settings of the result.
  @param Quoted if the result should be quoted.
  @param Suffix a suffix string which can be appendened to the result String
    i.e. Postgres ::timestamp.
  @return a formated UCS2-String with DateTime-Format pattern.
}
function DateTimeToUnicodeSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: ZWideString = ''): ZWideString;
var l, l2, Year, Month, Day, Hour, Minute, Second, MSec: Word;
  Buffer: array[0..31] of WideChar;
  P: PWideChar;
begin
  DecodeDateTime(Value, Year, Month, Day, Hour, Minute, Second, MSec);
  L := DateTimeToUnicodeSQLTimeStamp(Year, Month, Day, Hour, Minute,
    Second, MSec, @Buffer[0], ConFormatSettings.DateTimeFormat, Quoted, False);
  l2 := Length(Suffix);
  System.SetString(Result, nil, L+L2);
  P := Pointer(Result);
  Move(Buffer[0], P^, L shl 1);
  if L2 > 0 then
    Move(Pointer(Suffix)^, (P+L)^, L2 shl 1);
end;

{**
  Converts DateTime value to native string
}
function DateTimeToSQLTimeStamp(const Value: TDateTime;
  const ConFormatSettings: TZFormatSettings;
  const Quoted: Boolean; const Suffix: string): string;
begin
  Result := {$IFDEF UNICODE} DateTimeToUnicodeSQLTimeStamp {$ELSE} DateTimeToRawSQLTimeStamp {$ENDIF} (Value, ConFormatSettings, Quoted, Suffix);
end;

{**
  Converts TDateTime to Ansi SQL Date/Time
  @param Value an encoded TDateTime value.
  @return a  date and time string.
}
function DateTimeToAnsiSQLDate(Value: TDateTime; WithMMSec: Boolean = False): string;
begin
  if WithMMSec and (MilliSecondOf(Value) <> 0)
    then Result := FormatDateTime(SQLDateTimeFmtMSecs, Value)
    else Result := FormatDateTime(SQLDateTimeFmt, Value);
end;

{ TZSortedList }

{**
  Origial Autor: Aleksandr Sharahov
  see http://guildalfa.ru/alsha/
  Performs hybrid sort algorithm for the list.
  changes by EgonHugeist:
  Replace cardinal casts by using our NativeUInt to make it 64Bit compatible too
  Note Alexandr wrote: For max of speed it is very impotant to use procedures
    QuickSort_0AA and HybridSort_0AA as is (not in class, not included
    in other procedure, and not changed parameters and code).
}
//~1.57 times faster than Delphi QuickSort on E6850
{$Q-}
{$R-}
const
  InsCount = 35; //33..49;
  InsLast = InsCount-1;
  SOP = SizeOf(pointer);
  MSOP = NativeUInt(-SOP);

{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF} // uses pointer maths

procedure QuickSortSha_0AA(L, R: NativeUInt; Compare: TZListSortCompare);
var
  I, J, P, T: NativeUInt;
begin;
  while true do begin
    I := L;
    J := R;
    if J-I <= InsLast * SOP then break;
    T := (J-I) shr 1 and MSOP + I;

    if Compare(PPointer(J)^, PPointer(I)^)<0 then begin
      P := PNativeUInt(I)^;
      PNativeUInt(I)^ := PNativeUInt(J)^;
      PNativeUInt(J)^ := P;
    end;
    P := PNativeUInt(T)^;
    if Compare(Pointer(P), PPointer(I)^)<0 then
    begin
      P := PNativeUInt(I)^;
      PNativeUInt(I)^ := PNativeUInt(T)^;
      PNativeUInt(T)^ := P;
    end
    else
      if Compare(PPointer(J)^, Pointer(P)) < 0 then
      begin
        P := PNativeUInt(J)^;
        PNativeUInt(J)^ := PNativeUInt(T)^;
        PNativeUInt(T)^ := P;
      end;

    repeat
      Inc(I,SOP);
    until not (Compare(PPointer(I)^, Pointer(P)) < 0);
    repeat
      Dec(J,SOP)
    until not (Compare(pointer(P), PPointer(J)^) < 0);
    if I < J then
      repeat
        T := PNativeUInt(I)^;
        PNativeUInt(I)^ := PNativeUInt(J)^;
        PNativeUInt(J)^ := T;
        repeat
          Inc(I,SOP);
        until not (Compare(PPointer(I)^, pointer(P)) < 0 );
        repeat
          Dec(J,SOP);
        until not (Compare(pointer(P), PPointer(J)^) < 0);
      until I >= J;
    Dec(I,SOP); Inc(J,SOP);

    if I-L <= R-J then
    begin
      if L + InsLast * SOP < I then
        QuickSortSha_0AA(L, I, Compare);
      L := J;
    end
    else
    begin
      if J + InsLast * SOP < R
        then QuickSortSha_0AA(J, R, Compare);
      R := I;
    end;
  end;
end;

procedure HybridSortSha_0AA(List: PPointerList; Count: integer; Compare: TZListSortCompare);
var
  I, J, {$IFDEF WITH_IE200706094}J2,{$ENDIF} L, R: NativeUInt;
begin;
  if (List<>nil) and (Count>1) then
  begin
    L := NativeUInt(@List{$IFDEF TLIST_ISNOT_PPOINTERLIST}^{$ENDIF}[0]);
    R := NativeUInt(@List{$IFDEF TLIST_ISNOT_PPOINTERLIST}^{$ENDIF}[Count-1]);
    J := R;
    if Count-1 > InsLast then
    begin
      J:=NativeUInt(@List{$IFDEF TLIST_ISNOT_PPOINTERLIST}^{$ENDIF}[InsLast]);
      QuickSortSha_0AA(L, R, Compare);
    end;

    I := L;
    repeat;
      if Compare(PPointer(J)^, PPointer(I)^) < 0 then I:=J;
      dec(J,SOP);
    until J <= L;

    if I > L then
    begin
      J := PNativeUInt(I)^;
      PNativeUInt(I)^ := PNativeUInt(L)^;
      PNativeUInt(L)^ := J;
    end;

    J := L + SOP;
    while true do
    begin
      repeat;
        if J >= R then exit;
        inc(J,SOP);
      {$IFDEF WITH_IE200706094} //FPC 64Bit raises an internal Error 200706094!
        J2 := J+MSOP;
      until Compare(PPointer(J)^,PPointer(J2)^) < 0;
      {$ELSE}
      until Compare(PPointer(J)^,PPointer(J+MSOP)^) < 0;
      {$ENDIF}
      I := J - SOP;
      L := PNativeUInt(J)^;
      repeat;
        PNativeUInt(I+SOP)^ := PNativeUInt(I)^;
        dec(I,SOP);
      until not (Compare(Pointer(L),PPointer(I)^) < 0);
      PNativeUInt(I + SOP)^ := L;
    end;
  end;
end;

procedure QuickSort(List: PPointerList; L, R: Integer; Compare: TZListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin;
  repeat
    I := L;
    J := R;
    P := List^[(L + R) shr 1];
    repeat;
      while Compare(List^[I], P)<0 do
        Inc(I);         //*
      while Compare(List^[J], P)>0 do
        Dec(J);         //*
      if I<=J then begin;                            //**
        T := List^[I]; List^[I]:=List^[J]; List^[J]:=T;
        Inc(I);
        Dec(J);
      end;
    until I>J;
    if L<J then
      QuickSort(List, L, J, Compare);      //***
    L := I;
  until I >= R;
end;

{$IFDEF FPC} {$POP} {$ENDIF}

{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

{$IFDEF TLIST_IS_DEPRECATED}
function TZSortedList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList[Result] := Item;
  Inc(FCount);
end;

procedure TZSortedList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TZSortedList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList[Index + 1], FList[Index],
      (FCount - Index) * SizeOf(Pointer));
end;

destructor TZSortedList.Destroy;
begin
  Clear;
  inherited;
end;

class procedure TZSortedList.Error(Msg: PResStringRec; Data: NativeInt);
begin
  raise EListError.CreateFmt(LoadResString(Msg), [Data]) at ReturnAddress;
end;

class procedure TZSortedList.Error(const Msg: string; Data: NativeInt);
begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddress;
end;

procedure TZSortedList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, Index2);
  Item := FList[Index1];
  FList[Index1] := FList[Index2];
  FList[Index2] := Item;
end;

function TZSortedList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then begin
    Result := Item;
    FList[I] := nil;
    Delete(I);
  end;
end;

function TZSortedList.First: Pointer;
begin
  Result := Get(0);
end;

function TZSortedList.Get(Index: Integer): Pointer;
begin
  if Cardinal(Index) >= Cardinal(FCount) then
    Error(@SListIndexError, Index);
  Result := FList[Index];
end;

procedure TZSortedList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TZSortedList.IndexOf(Item: Pointer): Integer;
var
  P: PAnsiChar;
begin
  P := Pointer(FList);
  for Result := 0 to FCount - 1 do begin
    if PPointer(P)^ = Item then
      Exit;
    Inc(P, SizeOf(Pointer));
  end;
  Result := -1;
end;

procedure TZSortedList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, Index);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList[Index], FList[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList[Index] := Item;
  Inc(FCount);
end;

function TZSortedList.Last: Pointer;
begin
  if FCount > 0 then
    Result := FList[Count - 1]
  else
  begin
    Error(@SListIndexError, 0);
    Result := nil;
  end;
end;

procedure TZSortedList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, Index);
  if Item <> FList[Index] then begin
    FList[Index] := Item;
  end;
end;

function TZSortedList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TZSortedList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    Error(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    SetLength(FList, NewCapacity);
    FCapacity := NewCapacity;
  end;
end;

procedure TZSortedList.SetCount(NewCount: Integer);
begin
  if NewCount < 0 then
    Error(@SListCountError, NewCount);
  if NewCount <> FCount then begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if NewCount > FCount then
      FillChar(FList[FCount], (NewCount - FCount) * SizeOf(Pointer), 0);
    FCount := NewCount;
  end;
end;
{$ENDIF TLIST_IS_DEPRECATED}
{**
  Performs sorting for this list.
  @param Compare a comparison function.
}
procedure TZSortedList.Sort(Compare: TZListSortCompare);
begin
  {$IFDEF TLIST_ISNOT_PPOINTERLIST}
  if Count > 1 then
    HybridSortSha_0AA(@List, Count, Compare);
    //QuickSort(@List, 0, Count-1, Compare);
  {$ELSE}
  if Count > 1 then
    HybridSortSha_0AA(List, Count, Compare);
    //QuickSort(List, 0, Count-1, Compare);
  {$ENDIF}
end;

{**
  Converts an string into escape PostgreSQL format.
  @param Value a regular string.
  @return a string in PostgreSQL escape format.
}
function EncodeCString(const Value: ZWideString): ZWideString;
var
  I: Integer;
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PWideChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := Pointer(Value);
  DestLength := 0;
  for I := 1 to SrcLength do
  begin
    if SrcBuffer^ = #0
    then Inc(DestLength, 4)
    else case PWord(SrcBuffer)^ of
      Ord('"'), Ord(''''), Ord('\'):
       Inc(DestLength, 2)
      else
       Inc(DestLength);
    end;
    Inc(SrcBuffer);
  end;
  if SrcLength = DestLength then begin
    Result := Value;
    Exit;
  end;

  SrcBuffer := Pointer(Value);
  SetLength(Result, DestLength);
  DestBuffer := Pointer(Result);

  for I := 1 to SrcLength do begin
    case PWord(SrcBuffer)^ of
      Ord(#0): begin
          PWord(DestBuffer)^ := Ord('\');
          PWord(DestBuffer+1)^ := Ord('0') + (Byte(PWord(SrcBuffer)^) shr 6);
          PWord(DestBuffer+2)^ := Ord('0') + ((Byte(PWord(SrcBuffer)^) shr 3) and $07);
          PWord(DestBuffer+3)^ := Ord('0') + (Byte(PWord(SrcBuffer)^) and $07);
          Inc(DestBuffer, 4);
        end;
      Ord('"'), Ord(''''), Ord('\'): begin
          PWord(DestBuffer)^ := Ord('\');
          PWord(DestBuffer+1)^ := Ord(SrcBuffer^);
          Inc(DestBuffer, 2);
        end;
      else begin
          DestBuffer^ := SrcBuffer^;
          Inc(DestBuffer);
        end;
    end;
    Inc(SrcBuffer);
  end;
end;

function EncodeCString(const Value: RawByteString): RawByteString;
var
  I: Integer;
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PAnsiChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := Pointer(Value);
  DestLength := 0;
  for I := 1 to SrcLength do begin
    if PByte(SrcBuffer)^ = Ord(#0)
    then Inc(DestLength, 4)
    else case PByte(SrcBuffer)^ of
      Ord('"'), Ord(''''), Ord('\'):
       Inc(DestLength, 2)
      else
       Inc(DestLength);
    end;
    Inc(SrcBuffer);
  end;
  if SrcLength = DestLength then begin
    Result := Value;
    Exit;
  end;

  SrcBuffer := Pointer(Value);
  SetLength(Result, DestLength);
  DestBuffer := Pointer(Result);

  for I := 1 to SrcLength do begin
    case PByte(SrcBuffer)^ of
      Ord(#0): begin
          PByte(DestBuffer)^ := Ord('\');
          PByte(DestBuffer+1)^ := Ord('0') + (PByte(SrcBuffer)^ shr 6);
          PByte(DestBuffer+2)^ := Ord('0') + ((PByte(SrcBuffer)^ shr 3) and $07);
          PByte(DestBuffer+3)^ := Ord('0') + (PByte(SrcBuffer)^ and $07);
          Inc(DestBuffer, 4);
        end;
      Ord('"'), Ord(''''), Ord('\'): begin
          PByte(DestBuffer)^ := Ord('\');
          PByte(DestBuffer+1)^ := Ord(SrcBuffer^);
          Inc(DestBuffer, 2);
        end;
      else begin
          DestBuffer^ := SrcBuffer^;
          Inc(DestBuffer);
        end;
    end;
    Inc(SrcBuffer);
  end;
end;

{**
  Converts a buffer from escape PostgreSQL format.
  @param SrcLength the souce buffer length.
  @param SrcBuffer the souce buffer.
  @param SrcBuffer the destination buffer we write in.
  @return Length of dest chars.
}
function DecodeCString(SrcLength: LengthInt; SrcBuffer, DestBuffer: PWideChar): LengthInt; overload;
begin
  Result := 0;
  while SrcLength > 0 do begin
    if SrcBuffer^ = '\' then begin
      Inc(SrcBuffer);
      case SrcBuffer^ of
        '0'..'9':
          begin
            DestBuffer^ := WideChar(((Byte(SrcBuffer[0]) - Ord('0')) shl 6)
              or ((Byte(SrcBuffer[1]) - Ord('0')) shl 3)
              or ((Byte(SrcBuffer[2]) - Ord('0'))));
            Inc(SrcBuffer, 3);
            Dec(SrcLength, 4);
          end
        else
          begin
            case SrcBuffer^ of
              'r': DestBuffer^ := #13;
              'n': DestBuffer^ := #10;
              't': DestBuffer^ := #9;
              else
                DestBuffer^ := SrcBuffer^;
            end;
            Inc(SrcBuffer);
            Dec(SrcLength, 2);
          end
      end;
    end else begin
      DestBuffer^ := SrcBuffer^;
      Inc(SrcBuffer);
      Dec(SrcLength);
    end;
    Inc(DestBuffer);
    Inc(Result);
  end;
end;

function DecodeCString(SrcLength: LengthInt; SrcBuffer, DestBuffer: PAnsiChar): LengthInt; overload;
begin
  Result := 0;
  while SrcLength > 0 do begin
    if Ord(SrcBuffer^) = Ord('\') then begin
      Inc(SrcBuffer);
      case Ord(SrcBuffer^) of
        Ord('0')..Ord('9'):
          begin
            AnsiChar(DestBuffer^) := AnsiChar(((Byte(SrcBuffer[0]) - Ord('0')) shl 6)
              or ((Byte(SrcBuffer[1]) - Ord('0')) shl 3)
              or ((Byte(SrcBuffer[2]) - Ord('0'))));
            Inc(SrcBuffer, 3);
            Dec(SrcLength, 4);
          end
        else
          begin
            case Ord(SrcBuffer^) of
              Ord('r'): PByte(DestBuffer)^ := Ord(#13);
              Ord('n'): PByte(DestBuffer)^ := Ord(#10);
              Ord('t'): PByte(DestBuffer)^ := Ord(#9);
              else
                DestBuffer^ := SrcBuffer^;
            end;
            Inc(SrcBuffer);
            Dec(SrcLength, 2);
          end
      end;
    end else begin
      DestBuffer^ := SrcBuffer^;
      Inc(SrcBuffer);
      Dec(SrcLength);
    end;
    Inc(DestBuffer);
    Inc(Result);
  end;
end;

{**
  Converts a string from escape PostgreSQL format.
  @param SrcLength the souce buffer length.
  @param SrcBuffer the souce buffer.
  @return a regular string.
}
procedure DecodeCString(SrcLength: LengthInt; SrcBuffer: PWideChar; out Result: ZWideString);
begin
  SetLength(Result, SrcLength);
  SetLength(Result, DecodeCString(SrcLength, SrcBuffer, Pointer(Result)));
end;

procedure DecodeCString(SrcLength: LengthInt; SrcBuffer: PAnsiChar; out Result: RawByteString);
begin
  SetLength(Result, SrcLength);
  SetLength(Result, DecodeCString(SrcLength, SrcBuffer, Pointer(Result)));
end;

{**
  Converts a string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeCString(const Value: RawByteString): RawByteString;
begin
  DecodeCString(Length(Value), Pointer(Value), Result);
end;

function DecodeCString(const Value: ZWideString): ZWideString;
begin
  DecodeCString(Length(Value), Pointer(Value), Result);
end;


{**
  Replace chars in the string
  @param Source a char to search.
  @param Target a char to replace.
  @param Str a source string.
  @return a string with replaced chars.
}
function ReplaceChar(const Source, Target: Char; const Str: string): string;
var
  P: PChar;
  I:Integer;
begin
  Result := Str;
  UniqueString(Result);
  P := Pointer(Result);
  for i := 0 to Length(Str) - 1 do
  begin
    if P^ = Source then
      P^ := Target;
    Inc(P);
  end;
end;


{**
  Remove chars in the string.
  More obvious and ~35 times faster than StringReplace(Str, ToRemove, '')
  @param ToRemove a char to search and remove.
  @param Str a source string.
  @return a string with removed chars.
}
function RemoveChar(ToRemove: Char; const Str: string): string;
var
  PSrc, PSrcEnd, PDest: PChar;
  Len: Integer;
begin
  Len := Length(Str);
  SetLength(Result, Len);
  if Len = 0 then
    Exit;
  PSrc := Pointer(Str);
  PSrcEnd := PSrc+Len; //address the term
  PDest := Pointer(Result);

  while PSrc < PSrcEnd do
  begin
    if PSrc^ <> ToRemove then
    begin
      PDest^ := PSrc^;
      Inc(PDest);
    end
    else
      Dec(Len);
    Inc(PSrc);
  end;
  SetLength(Result, Len);
end;

{**
  Append a string to another string separating the added string with delimiter.
  Correctly processes cases where any of the arguments could be empty
  @param Str source string to append to. If empty, resulting Str value will be AddStr
  @param AddStr string to append. If empty, Str won't be changed
  @param Delimiter string to separate AddStr from Str
}
procedure AppendSepString(var Str: string; const AddStr, Delimiter: string);
begin
  if AddStr <> '' then
    if Str = '' then
      Str := AddStr
    else
      Str := Str + Delimiter + AddStr;
end;

{**
  Break a string into two parts according to appearance of Delimiter.
  @param Str source string
  @param Delimiter separator string; Str=Left+Delimiter+Right
  @param Left left part of Str from the start to the first Delimiter.
    Equals to Str if Str doesn't contain Delimiter
  @param Right left part of Str from the first Delimiter to the end.
    Empty if Str doesn't contain Delimiter

  NB: "var" modifier here allows using the same variable both as source and dest,
  for ex. in a loop like
    while Str <> '' do
    begin
      BreakString(Str, Delim, Fragment, Str);
      ...
    end;
  "out" modifier will clear the value at the entry of the proc!
}
procedure BreakString(const Str, Delimiter: String; var Left, Right: String);
var
  DelimPos, DelimLen: Integer;
  StrSave: string;
begin
  DelimPos := ZFastCode.Pos(Delimiter, Str);
  if DelimPos > 0 then begin
    DelimLen := Length(Delimiter);
    StrSave := Str; // allow one variable both as Str and Left
    Left := Copy(StrSave, 1, DelimPos-1);
    Right := Copy(StrSave, DelimPos + DelimLen, MaxInt);
  end else begin
    Left := Str;
    Right := '';
  end;
end;


{**
  Decodes a full version value encoded with Zeos SQL format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  into separated major, minor and subversion values
  @param FullVersion an integer containing the Full Version to decode.
  @param MajorVersion an integer containing the Major Version decoded.
  @param MinorVersion an integer containing the Minor Version decoded.
  @param SubVersion an integer contaning the Sub Version (revision) decoded.
}
procedure DecodeSQLVersioning(const FullVersion: Integer;
 out MajorVersion: Integer; out MinorVersion: Integer;
 out SubVersion: Integer);
begin
 MajorVersion := FullVersion div 1000000;
 MinorVersion := (FullVersion - (MajorVersion * 1000000)) div 1000;
 SubVersion   := FullVersion-(MajorVersion*1000000)-(MinorVersion*1000);
end;

{**
  Encodes major, minor and subversion (revision) values in Zeos SQL format:
   (major_version * 1,000,000) + (minor_version * 1,000) + sub_version
  For example, 4.1.12 is returned as 4001012.
  @param MajorVersion an integer containing the Major Version.
  @param MinorVersion an integer containing the Minor Version.
  @param SubVersion an integer containing the Sub Version (revision).
  @return an integer containing the full version.
}
function EncodeSQLVersioning(const MajorVersion: Integer;
 const MinorVersion: Integer; const SubVersion: Integer): Integer;
begin
 Result := (MajorVersion * 1000000) + (MinorVersion * 1000) + SubVersion;
end;

{**
  Formats a Zeos SQL Version format to X.Y.Z where:
   X = major_version
   Y = minor_version
   Z = sub version
  @param SQLVersion an integer
  @return Formated Zeos SQL Version Value.
}
function FormatSQLVersion(const SQLVersion: Integer): string;
var
   MajorVersion, MinorVersion, SubVersion: Integer;
begin
 DecodeSQLVersioning(SQLVersion, MajorVersion, MinorVersion, SubVersion);
 Result := ZFastCode.IntToStr(MajorVersion)+'.'+
           ZFastCode.IntToStr(MinorVersion)+'.'+
           ZFastCode.IntToStr(SubVersion);
end;

function AppendCondition(const Condition: string): string;
begin
  if Condition = ''
    then Result := ''
    else Result := ' AND ' + Condition;
end;

function ASCII7ToUnicodeString(const Src: RawByteString): ZWideString;
var I: Integer;
  Source: PByteArray;
  PEnd: PAnsiChar;
  Dest: PWordArray;
begin
  if Pointer(Src) = nil then begin
    Result := '';
    Exit;
  end;
  Source := Pointer(Src);
  I := {%H-}PLengthInt(NativeUInt(Src) - StringLenOffSet)^;
  System.SetString(Result, nil, i);
  Dest := Pointer(Result);
  PEnd := PAnsiChar(Source)+i-8;
  while PAnsiChar(Source) < PEnd do begin//making a octed processing loop
    Dest[0] := Source[0];
    Dest[1] := Source[1];
    Dest[2] := Source[2];
    Dest[3] := Source[3];
    Dest[4] := Source[4];
    Dest[5] := Source[5];
    Dest[6] := Source[6];
    Dest[7] := Source[7];
    Inc(PWideChar(Dest), 8);
    Inc(PAnsiChar(Source), 8);
  end;
  Inc(PEnd, 8);
  while PAnsiChar(Source) < PEnd do begin//processing final bytes
    Dest[0] := Source[0];
    inc(PAnsiChar(Source));
    inc(PWideChar(Dest));
  end;
end;

function ASCII7ToUnicodeString(Src: PAnsiChar; const Len: LengthInt): ZWideString;
begin
  {$IFDEF FPC}Result := '';{$ENDIF}
  ZSetString(Src, Len, Result);
end;

function UnicodeStringToASCII7(const Src: ZWideString): RawByteString;
var i, l: integer;
begin
  L := System.Length(Src); //temp l speeds x2
  if L = 0 then
    Result := EmptyRaw
  else begin
    if (Pointer(Result) = nil) or //empty ?
      ({%H-}PRefCntInt(NativeUInt(Result) - StringRefCntOffSet)^ <> 1) or { unique string ? }
      (LengthInt(l) <> {%H-}PLengthInt(NativeUInt(Result) - StringLenOffSet)^) then { length as expected ? }
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    begin
      Result := EmptyRaw; //speeds up SetLength x2
      SetLength(Result, l);
    end;
    {$ELSE}
    System.SetString(Result,nil, l);
    {$ENDIF}
{$R-}
    for i := 0 to l-1 do
      PByteArray(Result)[i] := PWordArray(Src)[i]; //0..255 equals to widechars
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

function UnicodeStringToASCII7(const Src: PWideChar; const Len: LengthInt): RawByteString;
var i: integer;
begin
  if (Src = nil) or (Len = 0) then
    Result := EmptyRaw
  else
  begin
    if (Pointer(Result) = nil) or //empty ?
      ({%H-}PRefCntInt(NativeUInt(Result) - StringRefCntOffSet)^ <> 1) or { unique string ? }
      (LengthInt(len) <> {%H-}PLengthInt(NativeUInt(Result) - StringLenOffSet)^) then { length as expected ? }
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    begin
      Result := EmptyRaw; //speeds up SetLength x2
      SetLength(Result, len);
    end;
    {$ELSE}
    System.SetString(Result,nil, Len);
    {$ENDIF}
{$R-}
    for i := 0 to len-1 do
      PByteArray(Result)[i] := PWordArray(Src)[i]; //0..255 equals to widechars
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
end;

function FloatToRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): RawByteString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  {$IFDEF FPC}Result := '';{$ENDIF}
  ZSetString(PAnsiChar(@Buffer[0]), FloatToRaw(Value, @Buffer[0]), Result);
end;

function FloatToRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PAnsiChar): LengthInt;
{$IFDEF NEXTGEN}
var
  Buffer: array[0..63] of WideChar;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF NEXTGEN}
  Result := FloatToText(PWideChar(@Buffer[0]), Value, fvExtended, ffGeneral, 15, 0);
  for I := 0 to Result -1 do
    PByte(Buf+I)^ := Ord(Buffer[i]);
  {$ELSE}
  Result := {$IFDEF WITH_FLOATTOTEXT_DEPRECATED}AnsiStrings.{$ENDIF}FloatToText(
    Buf, Value, {$IFNDEF FPC}fvExtended, {$ENDIF}ffGeneral, 15, 0);
  {$ENDIF}
end;

function FloatToSqlRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): RawByteString;
var
  Buffer: array[0..63] of AnsiChar;
begin
  {$IFDEF FPC}Result := '';{$ENDIF}
  ZSetString(PAnsiChar(@Buffer[0]), FloatToSqlRaw(Value, @Buffer[0]), Result);
end;

function FloatToSqlRaw(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PAnsiChar): LengthInt;
{$IFDEF NEXTGEN}
var
  Buffer: array[0..63] of WideChar;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF NEXTGEN}
  Result := FloatToText(PWideChar(@Buffer[0]), Value, fvExtended, ffGeneral, 15, 0, FmtSettFloatDot);
  for I := 0 to Result -1 do
    PByte(Buf+I)^ := Ord(Buffer[i]);
  {$ELSE}
  Result := {$IFDEF WITH_FLOATTOTEXT_DEPRECATED}AnsiStrings.{$ENDIF}FloatToText(
    Buf, Value, {$IFNDEF FPC}fvExtended, {$ENDIF}ffGeneral, 15, 0, FmtSettFloatDot);
  {$ENDIF}
end;

function FloatToUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): ZWideString;
var
  Buffer: array[0..63] of WideChar;
begin
  System.SetString(Result, PWideChar(@Buffer[0]), FloatToUnicode(Value, @Buffer[0]));
end;

function FloatToUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PWideChar): LengthInt;
{$IFNDEF UNICODE}
var
  Buffer: array[0..63] of AnsiChar;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := FloatToText(Buf, Value, fvExtended, ffGeneral, 15, 0);
  {$ELSE}
  Result := FloatToText(PAnsiChar(@Buffer[0]), Value, {$IFNDEF FPC}fvExtended, {$ENDIF}ffGeneral, 15, 0);
  for I := 0 to Result -1 do
    PWord(Buf+I)^ := Ord(Buffer[i]);
  {$ENDIF}
end;

function FloatToSqlUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}): ZWideString;
var
  Buffer: array[0..63] of WideChar;
begin
  System.SetString(Result, PWideChar(@Buffer[0]), FloatToSqlUnicode(Value, @Buffer[0]));
end;

function FloatToSqlUnicode(const Value: {$IFDEF CPU64}Double{$ELSE}Extended{$ENDIF}; Buf: PWideChar): LengthInt;
{$IFNDEF UNICODE}
var
  Buffer: array[0..63] of AnsiChar;
  I: Integer;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Result := FloatToText(Buf, Value, fvExtended, ffGeneral, 15, 0, FmtSettFloatDot);
  {$ELSE}
  Result := FloatToText(PAnsiChar(@Buffer[0]), Value, {$IFNDEF FPC}fvExtended, {$ENDIF}ffGeneral, 15, 0, FmtSettFloatDot);
  for I := 0 to Result -1 do
    PWord(Buf+I)^ := Ord(Buffer[i]);
  {$ENDIF}
end;

//EgonHugeist: my RTL replacemnet is 2x faster
procedure ZBinToHex(Buffer, Text: PAnsiChar; const Len: LengthInt);
var
  PEnd: PAnsiChar;
begin
  PEnd := Buffer+Len-8;
  while Buffer < PEnd do //try to convert 8Byte per loop
  begin
    PWord(Text)^ := TwoDigitLookupHexW[Ord(Buffer^)];
    PWord(Text+2)^ := TwoDigitLookupHexW[Ord((Buffer+1)^)];
    PWord(Text+4)^ := TwoDigitLookupHexW[Ord((Buffer+2)^)];
    PWord(Text+6)^ := TwoDigitLookupHexW[Ord((Buffer+3)^)];
    PWord(Text+8)^ := TwoDigitLookupHexW[Ord((Buffer+4)^)];
    PWord(Text+10)^ := TwoDigitLookupHexW[Ord((Buffer+5)^)];
    PWord(Text+12)^ := TwoDigitLookupHexW[Ord((Buffer+6)^)];
    PWord(Text+14)^ := TwoDigitLookupHexW[Ord((Buffer+7)^)];
    Inc(Buffer, 8);
    Inc(Text, 16);
  end;
  Inc(PEnd, 8);
  while Buffer < PEnd do
  begin
    PWord(Text)^ := TwoDigitLookupHexW[Ord(Buffer^)];
    Inc(Buffer);
    Inc(Text, 2);
  end;
end;

//EgonHugeist: my RTL replacemnet is 5x faster (BinToHex+Wide-Conv)
procedure ZBinToHex(Buffer: PAnsiChar; Text: PWideChar; const Len: LengthInt);
var
  PEnd: PAnsiChar;
begin
  PEnd := Buffer+Len-8;
  while Buffer < PEnd do  //try to convert 8Byte per loop
  begin
    PLongWord(Text)^ := TwoDigitLookupHexLW[Ord(Buffer^)];
    PLongWord(Text+2)^ := TwoDigitLookupHexLW[Ord((Buffer+1)^)];
    PLongWord(Text+4)^ := TwoDigitLookupHexLW[Ord((Buffer+2)^)];
    PLongWord(Text+6)^ := TwoDigitLookupHexLW[Ord((Buffer+3)^)];
    PLongWord(Text+8)^ := TwoDigitLookupHexLW[Ord((Buffer+4)^)];
    PLongWord(Text+10)^ := TwoDigitLookupHexLW[Ord((Buffer+5)^)];
    PLongWord(Text+12)^ := TwoDigitLookupHexLW[Ord((Buffer+6)^)];
    PLongWord(Text+14)^ := TwoDigitLookupHexLW[Ord((Buffer+7)^)];
    Inc(Buffer, 8);
    Inc(Text, 16);
  end;
  Inc(PEnd, 8);
  while Buffer < PEnd do
  begin
    PLongWord(Text)^ := TwoDigitLookupHexLW[Ord(Buffer^)];
    Inc(Buffer);
    Inc(Text, 2);
  end;
end;

{$IFDEF NO_RAW_HEXTOBIN}
  /// a conversion table from hexa chars into binary data
  // - returns 255 for any character out of 0..9,A..Z,a..z range
  // - used e.g. by HexToBin() function
  // - is defined globally, since may be used from an inlined function
var
  ConvertHexToBin: array[byte] of byte;
type
  PNormTableByte = ^TNormTableByte;
  TNormTableByte = packed array[byte] of byte;

{**
 fast conversion from hexa chars into binary data from Arnaud Bouchez
// - BinBytes contain the bytes count to be converted: Hex^ must contain
//  at least BinBytes*2 chars to be converted, and Bin^ enough space
// - if Bin=nil, no output data is written, but the Hex^ format is checked
// - return false if any invalid (non hexa) char is found in Hex^
// - using this function with Bin^ as an integer value will decode in big-endian
// order (most-signignifican byte first)
*}
function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: Integer): Boolean; overload;
var I: Integer;
    B,C: NativeUInt;//PtrUInt;
    tab: {$ifdef CPUX86}TNormTableByte absolute ConvertHexToBin{$else}PNormTableByte{$endif};
begin
  result := false; // return false if any invalid char
  if Hex=nil then
    exit;
  {$ifndef CPUX86}tab := @ConvertHexToBin;{$endif} // faster on PIC an x86_64
  if Bin<>nil then
    for I := 1 to BinBytes do begin
      B := tab[Ord(Hex^)];
      inc(Hex);
      if B>15 then exit;
      B := B shl 4;
      C := tab[Ord(Hex^)];
      inc(Hex);
      if C>15 then exit;
      Bin^ := B+C;
      inc(Bin);
    end else
    for I := 1 to BinBytes do begin // Bin=nil -> validate Hex^ input
      B := tab[Ord(Hex^)];
      inc(Hex);
      if B>15 then exit;
      C := tab[Ord(Hex^)];
      inc(Hex);
      if C>15 then exit;
    end;
  result := true; // conversion OK
end;
{$ENDIF}

//EgonHugeist: my conversion is 10x faster than IDE's
procedure GUIDToBuffer(const Source: Pointer; Dest: PAnsiChar;
  const SetTerm: Boolean = False);
var
  D1: Cardinal;
  W: Word;
  I: Integer;
begin
  PByte(Dest)^ := Ord('{');
  Inc(Dest);
  D1 := PCardinal(Source)^; //Process D1
  for i := 3 downto 0 do
  begin
    PWord(Dest+(I shl 1))^ := TwoDigitLookupHexW[PByte(@D1)^];
    D1 := D1 shr 8;
  end;
  Inc(Dest, 8);
  PByte(Dest)^ := Ord('-');
  // Source is binary data in fact, we're using PAnsiChar only to allow
  // pointer math for older Delphis
  W := PWord(PAnsiChar(Source)+4)^; //Process D2
  PWord(Dest+3)^ := TwoDigitLookupHexW[PByte(@W)^];
  W := W shr 8;
  PWord(Dest+1)^ := TwoDigitLookupHexW[PByte(@W)^];
  Inc(Dest, 5);
  PByte(Dest)^ := Ord('-');
  W := PWord(PAnsiChar(Source)+6)^; //Process D3
  PWord(Dest+3)^ := TwoDigitLookupHexW[PByte(@W)^];
  W := W shr 8;
  PWord(Dest+1)^ := TwoDigitLookupHexW[PByte(@W)^];
  Inc(Dest, 5);
  PByte(Dest)^ := Ord('-'); //Process D4
  PWord(Dest+1)^ := TwoDigitLookupHexW[PByte(PAnsiChar(Source)+8)^];
  PWord(Dest+3)^ := TwoDigitLookupHexW[PByte(PAnsiChar(Source)+9)^];
  PByte(Dest+5)^ := Ord('-');
  Inc(Dest, 6);
  for i := 0 to 5 do
    PWord(Dest+(I shl 1))^ := TwoDigitLookupHexW[PByte(PAnsiChar(Source)+10+i)^];
  PByte(Dest+12)^ := Ord('}');
  if SetTerm then PByte(Dest+13)^ := Ord(#0); //set trailing term
end;

procedure GUIDToBuffer(const Source: Pointer; Dest: PWideChar; const SetTerm: Boolean = False);
var
  I: Integer;
  D1: Cardinal;
  W: Word;
begin
  Dest^ := '{';
  Inc(Dest);
  D1 := PCardinal(Source)^; //Process D1
  for i := 3 downto 0 do
  begin
    PLongWord(Dest+(I shl 1))^ := TwoDigitLookupHexLW[PByte(@D1)^];
    if D1 > 0 then D1 := D1 shr 8;
  end;
  Inc(Dest, 8);
  PWord(Dest)^ := Ord('-');
  W := PWord(PAnsiChar(Source)+4)^; //Process D2
  PLongWord(Dest+3)^ := TwoDigitLookupHexLW[PByte(@W)^];
  if W > 0 then W := W shr 8;
  PLongWord(Dest+1)^ := TwoDigitLookupHexLW[PByte(@W)^];
  Inc(Dest, 5);
  PWord(Dest)^ := Ord('-');
  W := PWord(PAnsiChar(Source)+6)^; //Process D3
  PLongWord(Dest+3)^ := TwoDigitLookupHexLW[PByte(@W)^];
  if W > 0 then W := W shr 8;
  PLongWord(Dest+1)^ := TwoDigitLookupHexLW[PByte(@W)^];
  Inc(Dest, 5);
  PWord(Dest)^ := Ord('-'); //Process D4
  PLongWord(Dest+1)^ := TwoDigitLookupHexLW[PByte(PAnsiChar(Source)+8)^];
  PLongWord(Dest+3)^ := TwoDigitLookupHexLW[PByte(PAnsiChar(Source)+9)^];
  PWord(Dest+5)^ := Ord('-');
  Inc(Dest, 6);
  for i := 0 to 5 do
    PLongWord(Dest+(I shl 1))^ := TwoDigitLookupHexLW[PByte(PAnsiChar(Source)+10+i)^];
  (Dest+12)^ := '}';
  if SetTerm then (Dest+13)^ := #0; //set trailing term
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToRaw(const GUID: TGUID): RawByteString; overload;
begin
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(@GUID.D1, PAnsiChar(Pointer(Result)));
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToRaw(const Bts: TBytes): RawByteString; overload;
begin
  if Length(Bts) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(Pointer(Bts), PAnsiChar(Pointer(Result)));
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToRaw(Buffer: Pointer; Len: NativeInt): RawByteString; overload;
begin
  if (Buffer = Nil) or (Len <> 16) then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(Buffer, PAnsiChar(Pointer(Result)));
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToUnicode(const GUID: TGUID): ZWideString; overload;
begin
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(@GUID.D1, PWideChar(Pointer(Result)));
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToUnicode(const Bts: TBytes): ZWideString; overload;
begin
  if Length(Bts) <> 16 then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(Pointer(Bts), PWideChar(Pointer(Result)));
end;

//EgonHugeist: my conversion is 10x faster than IDE's
function GUIDToUnicode(Buffer: Pointer; Len: NativeInt): ZWideString; overload;
begin
  if (Buffer = Nil) or (Len <> 16) then
    raise EArgumentException.CreateResFmt(@SInvalidGuidArray, [16]);
  ZSetString(nil, 38, Result{%H-});
  GUIDToBuffer(Buffer, PWideChar(Pointer(Result)));
end;

function GUIDToStr(const GUID: TGUID): string;
begin
  Result := {$IFDEF UNICODE} GUIDToUnicode {$ELSE} GUIDToRaw {$ENDIF} (GUID);
end;

function GUIDToStr(const Bts: TBytes): string;
begin
  Result := {$IFDEF UNICODE} GUIDToUnicode {$ELSE} GUIDToRaw {$ENDIF} (Bts);
end;

function GUIDToStr(Buffer: Pointer; Len: Byte): string;
begin
  Result := {$IFDEF UNICODE} GUIDToUnicode {$ELSE} GUIDToRaw {$ENDIF} (Buffer, Len);
end;

procedure InvalidGUID(C: Char);
begin
  raise EArgumentException.CreateResFmt(@SInvalidGUID, [String(C)]);
end;
{**
  EgonHugeist: my conversion is 1,5x faster than IDE's
  converty hex-dezimal guid-string into a binary format
  @param Src a Pointer to the ansi Source-String
  @param Dest a Pointer to a 16-Bytes buffer
    Note it works with TGUID using @GUID.D1 and all other Buffers
}
procedure ValidGUIDToBinary(Src, Dest: PAnsiChar);
  function HexChar(c: AnsiChar): Byte;
  begin
    case Ord(c) of
      Ord('0')..Ord('9'):  Result := Byte(c) - Byte('0');
      Ord('a')..Ord('f'):  Result := (Byte(c) - Byte('a')) + 10;
      Ord('A')..Ord('F'):  Result := (Byte(c) - Byte('A')) + 10;
    else
      begin
        Result := 0; //satisfy compiler!
        InvalidGUID(Char(C));
      end;
    end;
  end;

  function HexByte(p: PAnsiChar): Byte;
  begin
    Result := Byte((HexChar(AnsiChar(p^)) shl 4) + HexChar(AnsiChar((p+1)^)));
  end;
var
  i: Integer;
begin
  Inc(Src, Ord(Ord(Src^) = Ord('{')));
  for i := 0 to 3 do //process D1
    PByte(dest+I)^ := HexByte(Src+(3-i) shl 1);
  if PByte(Src+8)^ <> Ord('-') then InvalidGUID(Char((Src+8)^));
  Inc(src, 9);
  Inc(dest, 4);
  for i := 0 to 1 do //D2, D3
  begin
    PByte(dest)^ := HexByte(src+2);
    PByte(dest+1)^ := HexByte(src);
    Inc(dest, 2);
    if PByte(Src+4)^ <> Ord('-') then InvalidGUID(Char((Src+4)^));
    Inc(src, 5);
  end;
  PByte(dest)^ := HexByte(src);
  PByte(dest+1)^ := HexByte(src+2);
  Inc(dest, 2);
  if PByte(Src+4)^ <> Ord('-') then InvalidGUID(Char((Src+4)^));
  Inc(src, 5);
  for i := 0 to 5 do
  begin
    PByte(dest)^ := HexByte(src);
    Inc(dest);
    Inc(src, 2);
  end;
  if not ((Ord(Src^) = Ord('}')) or (Ord(Src^) = Ord(#0))) then InvalidGUID(Char(Src^));
end;

{**
  EgonHugeist: my conversion is 1,5x faster than IDE's
  converty hex-dezimal unicode guid-string into a binary format
  @param Src a Pointer to the Unicode Source-String
  @param Dest a Pointer to a 16-Bytes buffer
    Note it works with TGUID using @GUID.D1 and all other Buffers
}
procedure ValidGUIDToBinary(Src: PWideChar; Dest: PAnsiChar);
  function HexChar(c: WideChar): Byte;
  begin
    case c of
      '0'..'9':  Result := Byte(c) - Byte('0');
      'a'..'f':  Result := (Byte(c) - Byte('a')) + 10;
      'A'..'F':  Result := (Byte(c) - Byte('A')) + 10;
    else
      begin
        InvalidGUID(Char(C));
        Result := 0; //satisfy compiler
      end;
    end;
  end;

  function HexByte(p: PWideChar): Byte;
  begin
    Result := Byte((HexChar(p^) shl 4) + HexChar((p+1)^));
  end;
var
  i: Integer;
begin
  Inc(Src, Ord(Src^ = '{'));
  for i := 0 to 3 do //process D1
    PByte(dest+I)^ := HexByte(Src+(3-i) shl 1);
  if (Src+8)^ <> '-' then InvalidGUID(Char((Src+8)^));
  Inc(src, 9);
  Inc(dest, 4);
  for i := 0 to 1 do //D2, D3
  begin
    PByte(dest)^ := HexByte(src+2);
    PByte(dest+1)^ := HexByte(src);
    Inc(dest, 2);
    if (Src+4)^ <> '-' then InvalidGUID(Char((Src+4)^));
    Inc(src, 5);
  end;
  PByte(dest)^ := HexByte(src);
  PByte(dest+1)^ := HexByte(src+2);
  Inc(dest, 2);
  if (Src+4)^ <> '-' then InvalidGUID(Char((Src+4)^));
  Inc(src, 5);
  for i := 0 to 5 do
  begin
    PByte(dest)^ := HexByte(src);
    Inc(dest);
    Inc(src, 2);
  end;
  if not ((Src^ = '}') or (Src^ = #0)) then InvalidGUID(Char(Src^));
end;

{**
  Standard quoting: Result := Quote + Double_Quotes(Src, Quote) + Quote
}
function SQLQuotedStr(Src: PWideChar; Len: LengthInt; Quote: WideChar): ZWideString; overload;
var
  P, Dest, PEnd, PFirst: PWideChar;
begin
  Dest := Nil;
  P := Src;
  PEnd := P + Len;
  PFirst := nil;
  while P < PEnd do begin
    if (P^=Quote) then begin
      if Dest = nil then
        PFirst := P;
      Inc({%H-}NativeUInt(Dest));
    end;
    Inc(P);
  end;
  if Dest = nil then begin
    System.SetLength(Result, Len+2);
    Dest := Pointer(Result);
    Dest^ := Quote;
    if Len > 0 then begin
      System.Move(Src^, (Dest+1)^, Len shl 1);
      Inc(Dest, Len+1);
    end else
      Inc(Dest);
    Dest^ := Quote;
    Exit;
  end;
  SetLength(Result, Len + {%H-}NativeInt(Dest) + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  P := PFirst;
  repeat
    Inc(P);
    Move(Src^, Dest^, (P - Src) shl 1);
    Inc(Dest, (P - Src));
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    while (P<PEnd) do if P^=Quote
      then Break
      else Inc(P);
  until P = PEnd;
  Move(Src^, Dest^, (PEnd - Src) shl 1);
  Inc(Dest, PEnd - Src);
  Dest^ := Quote;
end;

(*
// replacement implementation of SQLQuotedStr because the above implementation doesn't work in some cases.
function SQLQuotedStr(Src: PWideChar; Len: LengthInt; Quote: WideChar): ZWideString; overload;
var
  x: integer;
begin
  Result := '';
  SetLength(Result, Len);
  Move(Src^, Result[1], Len * 2);
  for x := Length(Result) downto 1 do begin
    if Result[x] = Quote then Insert(Quote, Result, x);
  end;
  Result := Quote + Result + Quote;
end;
*)
function SQLQuotedStr(const S: ZWideString; Quote: WideChar): ZWideString;
begin
  Result := SQLQuotedStr(Pointer(S), Length(S), Quote);
end;

function SQLQuotedStr(Src: PAnsiChar; Len: LengthInt; Quote: AnsiChar): RawByteString;
var
  P, Dest, PEnd, PFirst: PAnsiChar;
begin
  Dest := nil;
  P := Src;
  PEnd := P + Len;
  PFirst := nil;
  while P < PEnd do begin
    if (AnsiChar(P^)=Quote) then begin
      if Dest = nil then
        PFirst := P;
      Inc({%H-}NativeUInt(Dest));
    end;
    Inc(P);
  end;
  if Dest = nil then begin
    System.SetLength(Result, Len+2);
    Dest := Pointer(Result);
    AnsiChar(Dest^) := Quote;
    if Len > 0 then begin
      System.Move(Src^, (Dest+1)^, Len);
      Inc(Dest, Len+1);
    end else
      Inc(Dest);
    AnsiChar(Dest^) := Quote;
    Exit;
  end;
  SetLength(Result, Len + {%H-}NativeInt(Dest) + 2);
  Dest := Pointer(Result);
  AnsiChar(Dest^) := Quote;
  Inc(Dest);
  P := PFirst;
  repeat
    Inc(P);
    Move(Src^, Dest^, (P - Src));
    Inc(Dest, P - Src);
    AnsiChar(Dest^) := Quote;
    Inc(Dest);
    Src := P;
    while (P<PEnd) do if AnsiChar(P^)=Quote
      then Break
      else Inc(P);
  until P = PEnd;
  Move(Src^, Dest^, (PEnd - Src));
  Inc(Dest, PEnd - Src);
  AnsiChar(Dest^) := Quote;
end;

function SQLQuotedStr(const S: RawByteString; Quote: AnsiChar): RawByteString;
begin
  Result := SQLQuotedStr(Pointer(S), Length(S), Quote);
end;

{**
  Standard quoting with 2 quoting chars: Result := QuoteLeft + Double_Quotes(Src, QuoteLeft, QuoteRight) + QuoteRight
  This version is a bit slower than that with one quoting char so use it only when QuoteLeft <> QuoteRight
}
function SQLQuotedStr(Src: PChar; Len: LengthInt; QuoteLeft, QuoteRight: Char): string;
var
  EscChars: LengthInt;
  pSrc, pSrcEnd, pDest: PChar;
begin
  // Src must not be empty!
  if Len = 0 then
  begin
    // Str = Char+Char compiles to three (!) internal functions so we've to use pointers
    SetLength(Result, 2);
    pDest := Pointer(Result);
    pDest^ := QuoteLeft;
    (pDest+1)^ := QuoteRight;
    Exit;
  end;

  EscChars := 0;
  pSrc := Src;
  pSrcEnd := Src + Len - 1;
  // Count chars that should be escaped
  while pSrc <= pSrcEnd do
  begin
    if (pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight) then
      Inc(EscChars);
    Inc(pSrc);
  end;

  pSrc := Src;
  SetLength(Result, Len + EscChars + 2);
  pDest := Pointer(Result);
  pDest^ := QuoteLeft;
  Inc(pDest);

  if EscChars > 0 then
  begin
    while pSrc <= pSrcEnd do
    begin
      if (pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight) then
      begin
        pDest^ := pSrc^;
        (pDest+1)^ := pSrc^;
        Inc(pDest, 2);
      end
      else
      begin
        pDest^ := pSrc^;
        Inc(pDest);
      end;
      Inc(pSrc);
    end;
  end
  else
  begin
    Move(pSrc^, pDest^, Len*SizeOf(Char));
    Inc(pDest, Len);
  end;
  pDest^ := QuoteRight;
end;

function SQLQuotedStr(const S: string; QuoteLeft, QuoteRight: Char): string;
begin
  Result := SQLQuotedStr(Pointer(S), Length(S), QuoteLeft, QuoteRight);
end;

const
  SUnescapedChar = 'Unescaped char in string "%s"';
  SUnescapedCharAtEnd = 'Unescaped char at the end of the string "%s"';

function SQLDequotedStr(const S: string; QuoteChar: Char): string;
var
  L: LengthInt;
  P: PChar;
begin
  L := Length(S);
  if L <= 1 then
    Result := S
  else begin
    P := Pointer(S);
    if L = 2 then
      if (P^ = QuoteChar) and ((P+1)^ = QuoteChar) then // just quotes
        Result := ''
      else
        Result := S
    else
      Result := SQLDequotedStr(P, L, QuoteChar);
  end;
end;

function SQLDequotedStr(Src: PChar; Len: LengthInt; QuoteChar: Char): string;
var
  EscChars: LengthInt;
  pSrcBegin, pSrcEnd, pSrc, pDest: PChar;
begin
  pSrcBegin := Pointer(Src);
  // Input must have at least 2 chars, otherwise it is considered unquoted
  // so return as is
  if Len <= 1 then
  begin
    SetString(Result, Src, Len);
    Exit;
  end;

  pSrcEnd := pSrcBegin + Len - 1;
  // Check if input is quoted and return input as is if not
  if (pSrcBegin^ = QuoteChar) and (pSrcEnd^ = QuoteChar) then
  begin
    // just quotes
    if Len = 2 then
    begin
      Result := '';
      Exit;
    end;
    Inc(pSrcBegin);
    Dec(pSrcEnd);
  end
  else
  begin
    SetString(Result, Src, Len);
    Exit;
  end;

  // Count chars that should be escaped
  pSrc := pSrcBegin;
  EscChars := 0;
  while pSrc < pSrcEnd do
  begin
    if (pSrc^ = QuoteChar) then
      if pSrc^ = (pSrc+1)^ then
      begin
        Inc(EscChars);
        Inc(pSrc, 2);
      end
      else
        raise EArgumentException.CreateFmt(SUnescapedChar, [Src])
    else
      Inc(pSrc);
  end;
  // Check last char (pSrc = pSrcEnd is true here only if previous char wasn't
  // quote or was escaped quote)
  if (pSrc = pSrcEnd) and (pSrc^ = QuoteChar) then
    raise EArgumentException.CreateFmt(SUnescapedCharAtEnd, [Src]);

  // Input contains some escaped quotes
  if EscChars > 0 then
  begin
    SetLength(Result, Len - EscChars - 2);
    pSrc := pSrcBegin;
    pDest := Pointer(Result);
    while pSrc <= pSrcEnd do
    begin
      if (pSrc^ = QuoteChar) then
        Inc(pSrc);
      pDest^ := pSrc^;
      Inc(pSrc);
      Inc(pDest);
    end;
  end
  else
  // Input contains no escaped quotes
  begin
    SetLength(Result, Len - 2); // Result Length always > 2 here!
    Move(pSrcBegin^, Pointer(Result)^, (Len - 2)*SizeOf(Char));
  end;
end;

function SQLDequotedStr(const S: string; QuoteLeft, QuoteRight: Char): string;
var
  SrcLen, EscChars: LengthInt;
  pSrcBegin, pSrcEnd, pSrc, pDest: PChar;
begin
  SrcLen := Length(S);
  pSrcBegin := Pointer(S);
  // Input must have at least 2 chars, otherwise it is considered unquoted
  // so return as is
  if SrcLen <= 1 then
  begin
    Result := S;
    Exit;
  end;

  pSrcEnd := pSrcBegin + SrcLen - 1;
  // Check if input is quoted and return input as is if not
  if (pSrcBegin^ = QuoteLeft) and (pSrcEnd^ = QuoteRight) then
  begin
    // just quotes
    if SrcLen = 2 then
    begin
      Result := '';
      Exit;
    end;
    Inc(pSrcBegin);
    Dec(pSrcEnd);
  end
  else
  begin
    Result := S;
    Exit;
  end;

  // Count chars that should be escaped
  pSrc := pSrcBegin;
  EscChars := 0;
  while pSrc < pSrcEnd do
  begin
    if (pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight) then
      if pSrc^ = (pSrc+1)^ then
      begin
        Inc(EscChars);
        Inc(pSrc, 2);
      end
      else
        raise EArgumentException.CreateFmt(SUnescapedChar, [S])
    else
      Inc(pSrc);
  end;
  // Check last char (pSrc = pSrcEnd is true here only if previous char wasn't
  // quote or was escaped quote)
  if (pSrc = pSrcEnd) and ((pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight)) then
    raise EArgumentException.CreateFmt(SUnescapedCharAtEnd, [S]);

  // Input contains some escaped quotes
  if EscChars > 0 then
  begin
    SetLength(Result, SrcLen - EscChars - 2);
    pSrc := pSrcBegin;
    pDest := Pointer(Result);
    while pSrc <= pSrcEnd do
    begin
      if (pSrc^ = QuoteLeft) or (pSrc^ = QuoteRight) then
        Inc(pSrc);
      pDest^ := pSrc^;
      Inc(pSrc);
      Inc(pDest);
    end;
  end
  else
  // Input contains no escaped quotes
  begin
    SetLength(Result, SrcLen - 2); // Result Length always > 2 here!
    Move(pSrcBegin^, Pointer(Result)^, (SrcLen - 2)*SizeOf(Char));
  end;
end;

{$Q-}
{$R-}
function SameText(Val1, Val2: PAnsiChar; Len: LengthInt): Boolean;
var  PEnd: PAnsiChar;
  B: Byte;
begin
  Result := (Len = 0) or (Val1 = Val2);
  if Result then
    Exit;
  PEnd := Val1 + Len -4;
  while Val1 < PEnd do begin//compare 4 Bytes per loop
    if (PCardinal(Val1)^ <> PCardinal(Val2)^) then //equal?
      if PCardinal(Val1)^ and $80808080<>0 then begin //no Ascii quad?
        for B := 0 to 3 do
          if PByteArray(Val1)[B] <> PByteArray(Val2)[B] then
            if (PByteArray(Val1)[B] or $80 <> 0) or (PByteArray(Val2)[B] or $80 <> 0) then //one of both not in ascii range?
              Exit
            else if (PByteArray(Val1)[B] or $20) <> (PByteArray(Val2)[B] or $20) then
              Exit;
      end else if PCardinal(Val1)^ or $20202020 <> PCardinal(Val2)^ or $20202020 then
          Exit;
    Inc(Val1, 4);
    Inc(Val2, 4);
  end;
  Inc(PEnd, 4);
  while Val1 < PEnd do begin
    if (PByte(Val1)^ <> PByte(Val2)^) then //equal binary?
      if (PByte(Val1)^ and $80 <> 0) or (PByte(Val2)^ and $80 <> 0) then //no Ascii byte?
        Exit
      else if PByte(Val1)^ or $20 <> PByte(Val2)^ or $20 then
        Exit;
    Inc(Val1);
    Inc(Val2);
  end;
  Result := True;
end;

function SameText(Val1, Val2: PWideChar; Len: LengthInt): Boolean;
var  PEnd: PWideChar;
  B: Boolean;
begin
  Result := (Len = 0) or (Val1 = Val2);
  if Result then
    Exit;
  PEnd := Val1 + Len -2;
  while Val1 < PEnd do begin//compare 4 Bytes per loop
    if (PCardinal(Val1)^ <> PCardinal(Val2)^) then //equal binary?
      if PCardinal(Val1)^ and $00800080<>0 then begin//no Ascii pair?
        for B := False to True do
          if PWordArray(Val1)[Ord(B)] <> PWordArray(Val2)[Ord(B)] then
            if (PWordArray(Val1)[Ord(B)] or $0080 <> 0) or (PWordArray(Val2)[Ord(B)] or $0080 <> 0) then //one of both not in ascii range?
              Exit
            else if (PWordArray(Val1)[Ord(B)] or $0020) <> (PWordArray(Val2)[Ord(B)] or $0020) then
              Exit;
      end else if PCardinal(Val1)^ or $00200020 <> PCardinal(Val2)^ or $00200020 then
        Exit;
    Inc(Val1, 2);
    Inc(Val2, 2);
  end;
  Inc(PEnd, 2);
  while Val1 < PEnd do begin
    if (PWord(Val1)^ <> PWord(Val2)^) then //equal?
      if (PWord(Val1)^ and $0080 <> 0) or (PWord(Val2)^ and $0080 <> 0) then //no Ascii char?
        Exit
      else if PWord(Val1)^ or $0020 <> PWord(Val2)^ or $0020 then
        Exit;
    Inc(Val1);
    Inc(Val2);
  end;
  Result := True;
end;
{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

procedure Trim(var L: NativeUInt; var P: PAnsiChar);
var PEnd: PAnsiChar;
begin
  PEnd := P + L -1;
  while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
    Inc(P);
  while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
    Dec(PEnd);
  L := (PEnd+1)-P;
end;

function Trim(P: PAnsiChar; L: LengthInt): RawByteString;
var PEnd: PAnsiChar;
begin
  PEnd := P + L -1;
  while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
    Inc(P);
  while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
    Dec(PEnd);
  {$IFDEF FPC}Result := '';{$ENDIF}
  ZSetString(P, PEnd-P+1, Result);
end;

function Trim(P: PAnsiChar): RawByteString;
begin
  Result := Trim(P, StrLen(P));
end;

function Trim(P: PWideChar; L: LengthInt): ZWideString; overload;
var PEnd: PWideChar;
begin
  PEnd := P + L -1;
  while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
    Inc(P);
  while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
    Dec(PEnd);
  System.SetString(Result, P, PEnd-P+1);
end;

{$IF defined(UNICODE) and not defined(WITH_UNITANSISTRINGS)}
function Trim(const Value: RawByteString): RawByteString;
var P, PEnd: PAnsiChar;
begin
  if Pointer(Value) = nil then begin
    Result := Value;
    Exit;
  end;
  P := Pointer(Value);
  PEnd := P + Length(Value) -1;
  if (Ord(P^) > Ord(' ')) and (Ord(PEnd^) > Ord(' ')) then
    Result := Value
  else begin
    while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
      Inc(P);
    while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
      Dec(PEnd);
    ZSetString(P, PEnd-P+1, Result);
  end;
end;

function LowerCase(const Value: RawByteString): RawByteString;
var Len: Integer;
  Dst, Src, PEnd: PByte;
  Ch: Byte;
begin
  Len := Length(Value);
  SetLength(Result, Len);
  if Len > 0 then begin
    Dst := Pointer(Result);
    Src := Pointer(Value);
    PEnd := Dst+Len;
    while Dst < Pend do begin
      Ch := PByte(Src)^;
      if (ch >= Ord('A')) and (ch <= Ord('Z')) then
        Ch := Ch or $20;
      Dst^ := Ch;
      Inc(Dst);
      Inc(Src);
    end;
  end;
end;

function UpperCase(const Value: RawByteString): RawByteString;
var Len: Integer;
  Dst, Src, PEnd: PByte;
  Ch: Byte;
begin
  Len := Length(Value);
  SetLength(Result, Len);
  if Len > 0 then begin
    Dst := Pointer(Result);
    Src := Pointer(Value);
    PEnd := Dst+Len;
    while Dst < Pend do begin
      Ch := PByte(Src)^;
      if (ch >= Ord('a')) and (ch <= Ord('z')) then
        Ch := Ch xor $20;
      Dst^ := Ch;
      Inc(Dst);
      Inc(Src);
    end;
  end;
end;

{$IFEND}

{$IFNDEF UNICODE}
function Trim(const Value: ZWideString): ZWideString; overload;
var
  P, PEnd: PWideChar;
begin
  if Pointer(Value) = nil then begin
    Result := '';
    Exit;
  end;
  P := Pointer(Value);
  PEnd := P + Length(Value) -1;
  if (Ord(P^) > Ord(' ')) and (Ord(PEnd^) > Ord(' ')) then
    Result := Value
  else begin
    while (P <= PEnd) and (Ord(P^   ) <= Ord(' ')) do
      Inc(P);
    while (PEnd >= P) and (Ord(PEnd^) <= Ord(' ')) do
      Dec(PEnd);
    System.SetString(Result, P, PEnd-P+1);
  end;
end;

{$ENDIF}

{**
   Creates a memory stream with copy of data in buffer.
   If buffer contains no data, creates an empty stream.
}
function StreamFromData(Buffer: Pointer; Size: Integer): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  Result.Write(Buffer^, Size);
  Result.Position := 0;
end;

function StreamFromData(const AString: ZWideString): TMemoryStream;
begin
  Result := StreamFromData(Pointer(AString), Length(AString)*SizeOf(WideChar));
end;

function StreamFromData(const Bytes: TBytes): TMemoryStream;
begin
  Result := StreamFromData(Pointer(Bytes), Length(Bytes));
end;

{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
function StreamFromData(const AString: RawByteString): TMemoryStream;
begin
  Result := StreamFromData(Pointer(AString), Length(AString));
end;
{$ENDIF}

procedure HexFiller;
var
  I{$IFDEF NO_RAW_HEXTOBIN}, v{$ENDIF}: Byte;
  Hex: String;
begin
  for i := Low(Byte) to High(Byte) do
  begin
    Hex := IntToHex(I, 2);
    {$IFDEF UNICODE}
    TwoDigitLookupHexLW[i] := PLongWord(Pointer(Hex))^;
    TwoDigitLookupHexW[i] := PWord(Pointer(RawByteString(Hex)))^;
    {$ELSE}
    TwoDigitLookupHexW[i] := PWord(Pointer(Hex))^;
    TwoDigitLookupHexLW[i] := PCardinal(Pointer(ZWideString(Hex)))^;
    {$ENDIF}
  end;
  {$IFDEF NO_RAW_HEXTOBIN}
  //copy from Arnaud Bouchez syncommons.pas
  Fillchar(ConvertHexToBin[0],SizeOf(ConvertHexToBin),255); // all to 255
  V := 0;
  for i := ord('0') to ord('9') do begin
    ConvertHexToBin[i] := v;
    inc(v);
  end;
  for i := ord('A') to ord('F') do begin
    ConvertHexToBin[i] := v;
    ConvertHexToBin[i+(ord('a')-ord('A'))] := v;
    inc(v);
  end;
  {$ENDIF}
end;

function StringReplaceAll_CS_LToEQ(const Source, OldPattern, NewPattern: RawByteString): RawByteString;
var PSrc, PEnd: PAnsiChar;
  POld: PAnsiChar absolute OldPattern;
  PNew: PAnsiChar absolute NewPattern;
  PRes, PResEnd: PAnsiChar;
  L, iPos, LOld, LNew: Integer;
begin
  L := Length(Source);
  LOld := Length(OldPattern);
  PSrc := Pointer(Source);
  iPos := PosEx(POld, PSrc, LOld, L, 1);
  if iPos = 0 then begin
    Result := Source;
    Exit;
  end;
  PEnd := PSrc+L-1;
  if PNew = nil
  then LNew := 0
  else LNew := Length(NewPattern);
  Assert(LNew <= LOld);
  SetLength(Result, L);
  PRes := Pointer(Result);
  PResEnd := PRes+L;
  repeat
    Move(PSrc^, PRes^, iPos-LOld+1);
    Inc(PRes, (iPos-LOld+1));
    if LNew > 0 then begin
      Move(PNew^, PRes^, LNew);
      Inc(PRes, LNew);
    end;
    Inc(PSrc, (iPos-1+LOld));
    IPos := PosEx(POld, PSrc, LOld, (PEnd-PSrc)+1, 1);
  until IPos = 0;
  if (PSrc <= PEnd) then begin
    Move(PSrc^, PRes^, (PEnd-PSrc+1));
    Inc(Pres, (PEnd-PSrc+1));
  end;
  SetLength(Result, L-(PResEnd-PRes));
end;

function StringReplaceAll_CS_LToEQ(const Source, OldPattern, NewPattern: ZWideString): ZWideString;
var PSrc, PEnd: PWideChar;
  POld: PWideChar absolute OldPattern;
  PNew: PWideChar absolute NewPattern;
  PRes, PResEnd: PWideChar;
  L, iPos, LOld, LNew: Integer;
begin
  L := Length(Source);
  LOld := Length(OldPattern);
  PSrc := Pointer(Source);
  iPos := PosEx(POld, PSrc, LOld, L, 1);
  if iPos = 0 then begin
    Result := Source;
    Exit;
  end;
  PEnd := PSrc+L-1;
  if PNew = nil
  then LNew := 0
  else LNew := Length(NewPattern);
  Assert(LNew <= LOld);
  SetLength(Result, L);
  PRes := Pointer(Result);
  PResEnd := PRes+L;
  repeat
    Move(PSrc^, PRes^, (iPos-LOld+1) shl 1);
    Inc(PRes, (iPos-LOld+1));
    if LNew > 0 then begin
      Move(PNew^, PRes^, LNew shl 1);
      Inc(PRes, LNew);
    end;
    Inc(PSrc, (iPos-1+LOld));
    IPos := PosEx(POld, PSrc, LOld, (PEnd-PSrc)+1, 1);
  until IPos = 0;
  if (PSrc <= PEnd) then begin
    Move(PSrc^, PRes^, (PAnsiChar(PEnd)-PAnsiChar(PSrc)+2));
    Inc(Pres, (PEnd-PSrc+1));
  end;
  SetLength(Result, L-(PResEnd-PRes));
end;

{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
procedure BoolConstFiller;
var B: Boolean;
begin
  for B := False to True do begin
    BoolStrIntsRaw[B] := UnicodeStringToASCII7(BoolStrInts[B]);
    BoolStrsRaw[B] := UnicodeStringToASCII7(BoolStrsW[B]);
  end;
end;
{$ENDIF}

{ for a better code align -> move out of method }
{$IFNDEF CPU64}
const CIntTable: array[TCurrRoundToScale] of Cardinal = (10000, 1000, 100, 10, 1);
{$ENDIF}
const CInt64Table: array[TCurrRoundToScale] of Int64  = (10000, 1000, 100, 10, 1);
const PosHalfModulos: array [TCurrRoundToScale] of Integer = ( 4445,  445,  45,  5, 0);
const NegHalfModulos: array [TCurrRoundToScale] of Integer = (-4445, -445, -45, -5, 0);

{** EH:
   round a currency value half away from zero to it's exact scale digits
   @param Value the value to be rounded
   @param Scale the exact scale digt we want to round valid is 0..4. even if 4 is a nop
   @return a rounded value
}
function RoundCurrTo(const Value: Currency; Scale: TCurrRoundToScale): Currency;
var Modulo: Integer;
  {$IFNDEF CPU64}
  sI64Rec: Int64Rec absolute Value;
  dI64Rec: Int64Rec absolute Result;
  {$ENDIF}
  s64: Int64 absolute Value;
  d64: Int64 absolute Result;
begin
  if Scale < 4 then begin
    {$IFNDEF CPU64} //push trunc performance of positive tiny values
    if sI64Rec.Hi = 0 then begin
      dI64Rec.hi := sI64Rec.Lo div CIntTable[Scale];
      dI64Rec.lo := dI64Rec.hi *   CIntTable[Scale];
      Modulo := sI64Rec.Lo - dI64Rec.lo;
      dI64Rec.Hi := 0;
    end else {$ENDIF} begin
      d64 := s64 div CInt64Table[Scale];
      d64 := d64 *   CInt64Table[Scale];
      Modulo := s64 - d64;
    end;
    if Scale > 0 then
      if Modulo < 0 then begin
        if Modulo >= NegHalfModulos[Scale] then
          d64 := d64 - CInt64Table[Scale];
      end else if Modulo >= PosHalfModulos[Scale] then
        d64 := d64 + CInt64Table[Scale];
  end else
  Result := Value
end;

initialization
  HexFiller;  //build up lookup table
{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  BoolConstFiller; //build bool consts
{$ENDIF}
end.
