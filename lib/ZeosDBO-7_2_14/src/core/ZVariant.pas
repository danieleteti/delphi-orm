{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Variant Processing Classes                }
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

unit ZVariant;

interface

{$I ZCore.inc}

uses
  {$IF not defined(FPC) and defined(MSWINDOWS)}Windows,{$IFEND}  //need for inline
  Classes, SysUtils, ZCompatibility, ZClasses;

const
  {** Precision for float values comparison }
  FLOAT_COMPARE_PRECISION = 1.0e-5;
  FLOAT_COMPARE_PRECISION_SINGLE = 1.5e-5;

  {FPC - Compatibility for SQLite (currently) }
  {$IF NOT DECLARED(JulianEpoch)} // sysutils/datih.inc
  const
    JulianEpoch = -2415018.5; // "julian day 0" is January 1, 4713 BC 12:00AM
  {$IFEND}

  StrFalseUp = 'FALSE';
  StrTrueUp = 'TRUE';
  BoolStrsUp: array[Boolean] of string = (StrFalseUp, StrTrueUp);
  BoolStrsUpW: array[Boolean] of ZWideString = (ZWideString(StrFalseUp), ZWideString(StrTrueUp));
{$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
  BoolStrsUpRaw: array[Boolean] of RawByteString = (RawByteString(StrFalseUp), RawByteString(StrTrueUp));
{$ELSE}
var
  BoolStrsUpRaw: array[Boolean] of RawByteString;
{$ENDIF}

type
  {** Defines variant types. }
  TZVariantType = (vtNull, vtBoolean, vtInteger, vtUInteger, vtFloat, vtBytes,
    vtString, {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}{$IFNDEF NO_UTF8STRING}vtUTF8String,{$ENDIF} vtRawByteString, vtUnicodeString, //String Types
    vtDateTime, vtPointer, vtInterface, vtCharRec,
    vtArray{a dynamic array of [vtNull ... vtCharRec]} );

  PZArray =^TZArray;
  TZArray = Record
    VArray: Pointer; { Pointer to a Dynamic Array of X}
    VArrayType: Byte; {ord of TZSQLType}
    VArrayVariantType: TZVariantType; { better way to determine vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtUnicodeString, vtCharRec}
    VIsNullArray: Pointer; { Pointer to a Dynamic Array of a possible NULL indicator, might be integers or boolean types}
    VIsNullArrayType: Byte; {ord of TZSQLType}
    VIsNullArrayVariantType: TZVariantType; { better way to determine vtString, vtAnsiString, vtUTF8String, vtRawByteString, vtUnicodeString, vtCharRec}
  end;

  {** Defines a variant structure. }
  TZVariant = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    VType: TZVariantType;
    VString: String;
    {$IFNDEF NO_ANSISTRING}
    VAnsiString: AnsiString;
    {$ENDIF}
    VRawByteString: RawByteString;
    {$IFNDEF NO_UTF8STRING}
    VUTF8String: UTF8String;
    {$ENDIF}
    VUnicodeString: ZWideString;
    VBytes: TBytes;
    VInterface: IZInterface;
    case TZVariantType of
      vtBoolean: (VBoolean: Boolean);
      vtInteger: (VInteger: Int64);
      vtUInteger: (VUInteger: UInt64);
      vtFloat: (VFloat: Extended);
      {$IFDEF BCC32_vtDateTime_ERROR}
      vtDateTime: (VDateTime: Double);
      {$ELSE}
      vtDateTime: (VDateTime: TDateTime);
      {$ENDIF}
      vtPointer: (VPointer: Pointer);
      vtCharRec: (VCharRec: TZCharRec);
      vtArray: (VArray: TZArray);
  end;

  PZVariant = ^TZVariant;

  {** Defines an array of variants. }
  TZVariantDynArray = array of TZVariant;

  {** Defines a variant processing exception. }
  EZVariantException = class (Exception);

  {** Defines an interface for variant data. }
  {** Defines a Variant Manager interface. }
  IZVariantManager = interface (IZInterface)
    ['{DAA373D9-1A98-4AA8-B65E-4C23167EE83F}']

    function IsNull(const Value: TZVariant): Boolean;
    procedure SetNull(out Value: TZVariant);

    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
    procedure Assign(const SrcValue: TZVariant; out DstValue: TZVariant);
    function Clone(const Value: TZVariant): TZVariant;
    function Compare(const Value1, Value2: TZVariant): Integer;

    function GetAsBoolean(const Value: TZVariant): Boolean;
    function GetAsBytes(const Value: TZVariant): TBytes;
    function GetAsInteger(const Value: TZVariant): Int64;
    function GetAsUInteger(const Value: TZVariant): UInt64;
    function GetAsFloat(const Value: TZVariant): Extended;
    function GetAsString(const Value: TZVariant): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAsAnsiString(const Value: TZVariant): AnsiString;
    {$ENDIF}
    function GetAsRawByteString(const Value: TZVariant): RawByteString; overload;
    function GetAsCharRec(const Value: TZVariant): TZCharRec;
    {$IFNDEF NO_UTF8STRING}
    function GetAsUTF8String(const Value: TZVariant): UTF8String;
    {$ENDIF}
    function GetAsUnicodeString(const Value: TZVariant): ZWideString;
    function GetAsDateTime(const Value: TZVariant): TDateTime;
    function GetAsPointer(const Value: TZVariant): Pointer;
    function GetAsInterface(const Value: TZVariant): IZInterface;
    function GetAsArray(const Value: TZVariant): TZArray;

    procedure SetAsBoolean(out Value: TZVariant; const Data: Boolean);
    procedure SetAsBytes(out Value: TZVariant; const Data: TBytes);
    procedure SetAsInteger(out Value: TZVariant; const Data: Int64);
    procedure SetAsUInteger(out Value: TZVariant; const Data: UInt64);
    procedure SetAsFloat(out Value: TZVariant; const Data: Extended);
    procedure SetAsString(out Value: TZVariant; const Data: String);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAsAnsiString(out Value: TZVariant; const Data: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetAsUTF8String(out Value: TZVariant; const Data: UTF8String);
    {$ENDIF}
    procedure SetAsRawByteString(out Value: TZVariant; const Data: RawByteString);
    procedure SetAsCharRec(out Value: TZVariant; const Data: TZCharRec);
    procedure SetAsUnicodeString(out Value: TZVariant; const Data: ZWideString);
    procedure SetAsDateTime(out Value: TZVariant; const Data: TDateTime);
    procedure SetAsPointer(out Value: TZVariant; const Data: Pointer);
    procedure SetAsInterface(out Value: TZVariant; const Data: IZInterface);
    procedure SetAsArray(out Value: TZVariant; const Data: TZArray);

    function OpAdd(const Value1, Value2: TZVariant): TZVariant;
    function OpSub(const Value1, Value2: TZVariant): TZVariant;
    function OpMul(const Value1, Value2: TZVariant): TZVariant;
    function OpDiv(const Value1, Value2: TZVariant): TZVariant;
    function OpMod(const Value1, Value2: TZVariant): TZVariant;
    function OpPow(const Value1, Value2: TZVariant): TZVariant;
    function OpAnd(const Value1, Value2: TZVariant): TZVariant;
    function OpOr(const Value1, Value2: TZVariant): TZVariant;
    function OpXor(const Value1, Value2: TZVariant): TZVariant;
    function OpNot(const Value: TZVariant): TZVariant;
    function OpNegative(const Value: TZVariant): TZVariant;
    function OpEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpNotEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpMore(const Value1, Value2: TZVariant): TZVariant;
    function OpLess(const Value1, Value2: TZVariant): TZVariant;
    function OpMoreEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpLessEqual(const Value1, Value2: TZVariant): TZVariant;
  end;

  {** Implements a variant manager with strict convertion rules. }
  {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF} = class (TInterfacedObject, IZVariantManager)
  private
    {$IFNDEF NO_ANSISTRING}
    ZAnsiToUTF8: TZAnsiToUTF8;
    ZUTF8ToAnsi: TZUTF8ToAnsi;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    ZUTF8ToString: TZUTF8ToString;
    ZStringToUTF8: TZStringToUTF8;
    {$ENDIF}
    FSystemCodePage: Word;
  protected
    procedure RaiseTypeMismatchError;
    procedure RaiseUnsupportedOperation;
  public
    constructor Create;
    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
      virtual;
    procedure Assign(const SrcValue: TZVariant; out DstValue: TZVariant);
    function Clone(const Value: TZVariant): TZVariant;
    function Compare(const Value1, Value2: TZVariant): Integer;

    function IsNull(const Value: TZVariant): Boolean;
    procedure SetNull(out Value: TZVariant);

    function GetAsBoolean(const Value: TZVariant): Boolean;
    function GetAsBytes(const Value: TZVariant): TBytes;
    function GetAsInteger(const Value: TZVariant): Int64;
    function GetAsUInteger(const Value: TZVariant): UInt64;
    function GetAsFloat(const Value: TZVariant): Extended;
    function GetAsString(const Value: TZVariant): String;
    {$IFNDEF NO_ANSISTRING}
    function GetAsAnsiString(const Value: TZVariant): AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetAsUTF8String(const Value: TZVariant): UTF8String;
    {$ENDIF}
    function GetAsRawByteString(const Value: TZVariant): RawByteString; overload;
    function GetAsRawByteString(const Value: TZVariant; const RawCP: Word): RawByteString; overload; virtual;
    function GetAsCharRec(const Value: TZVariant): TZCharRec; overload;
    function GetAsUnicodeString(const Value: TZVariant): ZWideString;
    function GetAsDateTime(const Value: TZVariant): TDateTime;
    function GetAsPointer(const Value: TZVariant): Pointer;
    function GetAsInterface(const Value: TZVariant): IZInterface;
    function GetAsArray(const Value: TZVariant): TZArray;

    procedure SetAsBoolean(out Value: TZVariant; const Data: Boolean);
    procedure SetAsBytes(out Value: TZVariant; const Data: TBytes);
    procedure SetAsInteger(out Value: TZVariant; const Data: Int64);
    procedure SetAsUInteger(out Value: TZVariant; const Data: UInt64);
    procedure SetAsFloat(out Value: TZVariant; const Data: Extended);
    procedure SetAsString(out Value: TZVariant; const Data: String);
    {$IFNDEF NO_ANSISTRING}
    procedure SetAsAnsiString(out Value: TZVariant; const Data: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetAsUTF8String(out Value: TZVariant; const Data: UTF8String);
    {$ENDIF}
    procedure SetAsRawByteString(out Value: TZVariant; const Data: RawByteString);
    procedure SetAsCharRec(out Value: TZVariant; const Data: TZCharRec);
    procedure SetAsUnicodeString(out Value: TZVariant; const Data: ZWideString);
    procedure SetAsDateTime(out Value: TZVariant; const Data: TDateTime);
    procedure SetAsPointer(out Value: TZVariant; const Data: Pointer);
    procedure SetAsInterface(out Value: TZVariant; const Data: IZInterface);
    procedure SetAsArray(out Value: TZVariant; const Data: TZArray);

    function OpAdd(const Value1, Value2: TZVariant): TZVariant;
    function OpSub(const Value1, Value2: TZVariant): TZVariant;
    function OpMul(const Value1, Value2: TZVariant): TZVariant;
    function OpDiv(const Value1, Value2: TZVariant): TZVariant;
    function OpMod(const Value1, Value2: TZVariant): TZVariant;
    function OpPow(const Value1, Value2: TZVariant): TZVariant;
    function OpAnd(const Value1, Value2: TZVariant): TZVariant;
    function OpOr(const Value1, Value2: TZVariant): TZVariant;
    function OpXor(const Value1, Value2: TZVariant): TZVariant;
    function OpNot(const Value: TZVariant): TZVariant;
    function OpNegative(const Value: TZVariant): TZVariant;
    function OpEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpNotEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpMore(const Value1, Value2: TZVariant): TZVariant;
    function OpLess(const Value1, Value2: TZVariant): TZVariant;
    function OpMoreEqual(const Value1, Value2: TZVariant): TZVariant;
    function OpLessEqual(const Value1, Value2: TZVariant): TZVariant;
  end;

  {** Implements a variant manager with soft convertion rules. }
  {$IFDEF ZEOS_TEST_ONLY}
  TZSoftVariantManager = class (TZDefaultVariantManager)
  public
    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
      override;
  end;
  {$ENDIF ZEOS_TEST_ONLY}

  IZClientVariantManager = Interface(IZVariantManager)
    ['{73A1A2C7-7C38-4620-B7FE-2426BF839BE5}']
    function GetAsRawByteString(const Value: TZVariant; const RawCP: Word): RawByteString; overload;
    function GetAsCharRec(var Value: TZVariant; const CodePage: Word): TZCharRec; overload;
  End;

  {** Implements a variant manager with connection related convertion rules. }
  TZClientVariantManager = class ({$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}, IZClientVariantManager)
  private
    FConSettings: PZConSettings;
  public
    constructor Create(const ConSettings: PZConSettings);
    function Convert(const Value: TZVariant; NewType: TZVariantType): TZVariant;
      override;
    function GetAsRawByteString(const Value: TZVariant; const RawCP: Word): RawByteString; override;
    function GetAsCharRec(var Value: TZVariant; const CodePage: Word): TZCharRec; overload;
  end;

type

  {** Represents any value interface. }
  IZAnyValue = interface (IZClonnable)
    ['{E81988B3-FD0E-4524-B658-B309B02F0B6A}']

    function IsNull: Boolean;
    function GetValue: TZVariant;

    function GetBoolean: Boolean;
    function GetBytes: TBytes;
    function GetInteger: Int64;
    function GetFloat: Extended;
    function GetString: String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString: AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String: UTF8String;
    {$ENDIF}
    function GetUnicodeString: ZWideString;
    function GetDateTime: TDateTime;
  end;

  {** Implements an any value object. }
  TZAnyValue = class(TZAbstractObject, IZAnyValue, IZComparable)
  private
    FValue: TZVariant;
  public
    constructor Create(const Value: TZVariant);
    constructor CreateWithBoolean(Value: Boolean);
    constructor CreateWithInteger(const Value: Int64);
    constructor CreateWithFloat(const Value: Extended);
    constructor CreateWithString(const Value: String);
    {$IFDEF UNICODE}
    // unicodeType is a (dummy) default parameter to avoid
    // the problem described in https://forums.codegear.com/thread.jspa?messageID=65681
    // when dcc creates header (.hpp)-files for c++ builder. Both 'String' and
    // 'UnicodeString' translate into 'UnicodeString' in C++ builder 2009/2010, and
    // CreateWithString and CreateWithUnicodeString would result in duplicate
    // C++ constructors.
    constructor CreateWithUnicodeString(const Value: String; unicodeType: Boolean=true);
    {$ELSE}
    constructor CreateWithUnicodeString(const Value: WideString);
    {$ENDIF}
    constructor CreateWithDateTime(const Value: TDateTime);

    function IsNull: Boolean;
    function GetValue: TZVariant;

    function GetBoolean: Boolean;
    function GetBytes: TBytes;
    function GetInteger: Int64;
    function GetFloat: Extended;
    function GetString: String;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString: AnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String: UTF8String;
    {$ENDIF}
    function GetUnicodeString: ZWideString;
    function GetDateTime: TDateTime;

    function Equals(const Value: IZInterface): Boolean; override;
    function Clone: IZInterface; override;
    function ToString: string; override;
  end;

{**
  Encodes a custom variant value into standard variant.
  @param Value a custom variant value to be encoded.
  @returns an encoded standard variant.
}
function EncodeVariant(const Value: TZVariant): Variant;

{**
  Encodes an array of custom variant values into array of standard variants.
  @param Value an array of custom variant values to be encoded.
  @returns an encoded array of standard variants.
}
function EncodeVariantArray(const Value: TZVariantDynArray): Variant;

{**
  Decodes a standard variant value into custom variant.
  @param Value a standard variant value to be decoded.
  @returns an decoded custom variant.
}
function DecodeVariant(const Value: Variant): TZVariant;

{**
  Decodes an array of standard variant values into array of custom variants.
  @param Value an array of standard variant values to be decoded.
  @returns an decoded array of custom variants.
}
function DecodeVariantArray(const Value: Variant): TZVariantDynArray;

{**
  Encodes null into a custom variant.
  @returns an decoded custom variant.
}
function EncodeNull : TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a boolean into a custom variant.
  @param Value a boolean value to be encoded.
  @returns an encoded custom variant.
}
function EncodeBoolean(const Value: Boolean): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a Byte array into a custom variant.
  @param Value a boolean value to be encoded.
  @returns an encoded custom variant.
}
function EncodeBytes(const Value: TBytes): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a GUID into a custom variant.
  @param Value a GUID to be encoded.
  @returns an encoded custom variant.
}
function EncodeGUID(const Value: TGUID): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes an integer into a custom variant.
  @param Value an intger value to be encoded.
  @returns an encoded custom variant.
}
function EncodeInteger(const Value: Int64): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes an integer into a custom variant.
  @param Value an intger value to be encoded.
  @returns an encoded custom variant.
}
function EncodeUInteger(const Value: UInt64): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a float into a custom variant.
  @param Value a float value to be encoded.
  @returns an encoded custom variant.
}
function EncodeFloat(const Value: Extended): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a String into a custom variant.
  @param Value a String value to be encoded.
  @returns an encoded custom variant.
}
function EncodeString(const Value: String): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{$IFNDEF NO_ANSISTRING}
{**
  Encodes a AnsiString into a custom variant.
  @param Value a AnsiString value to be encoded.
  @returns an encoded custom variant.
}
function EncodeAnsiString(const Value: AnsiString): TZVariant;  {$IFDEF WITH_INLINE}inline;{$ENDIF}
{$ENDIF}
{**
  Encodes a UTF8String into a custom variant.
  @param Value a UTF8String value to be encoded.
  @returns an encoded custom variant.
}
{$IFNDEF NO_UTF8STRING}
function EncodeUTF8String(const Value: UTF8String): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{$ENDIF}
{**
  Encodes a RawByteString into a custom variant.
  @param Value a RawByteString value to be encoded.
  @param CP the CoodePage of the Value string.
  @returns an encoded custom variant.
}
function EncodeRawByteString(const Value: RawByteString): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a TZCharRec into a custom variant.
  @param Value a TZCharRec value to be encoded.
  @returns an encoded custom variant.
}
function EncodeCharRec(const Value: TZCharRec): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a unicodestring into a custom variant.
  @param Value a unicodestring value to be encoded.
  @returns an encoded custom variant.
}
function EncodeUnicodeString(const Value: ZWideString): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a TDateTime into a custom variant.
  @param Value a TDateTime value to be encoded.
  @returns an encoded custom variant.
}
function EncodeDateTime(const Value: TDateTime): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes a pointer into a custom variant.
  @param Value a pointer value to be encoded.
  @returns an encoded custom variant.
}
function EncodePointer(const Value: Pointer): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}
{**
  Encodes an interface into a custom variant.
  @param Value an interface value to be encoded.
  @returns an encoded custom variant.
}
function EncodeInterface(const Value: IZInterface): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{**
  Encodes an TZArray into a custom variant.
  @param Value an interface value to be encoded.
  @returns an encoded custom variant.
}
function EncodeArray(const Value: TZArray): TZVariant; {$IFDEF WITH_INLINE}inline;{$ENDIF}

var
  {** Declares a default variant manager with strict convertion rules. }
  {$IFDEF ZEOS_TEST_ONLY}
  DefVarManager: IZVariantManager;
  {$ENDIF ZEOS_TEST_ONLY}

  {** Declares a variant manager with soft convertion rules. }
  SoftVarManager: IZVariantManager;

  {** A NULL Variant Value. }
  NullVariant: TZVariant;

implementation

uses
  Variants, Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  ZMessages, ZEncoding, ZFastCode, ZSysUtils;

{ TZDefaultVariantManager }

{**
  Constructs this object and assigns the main properties.
}
constructor {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.Create;
begin
  inherited;
  FSystemCodePage := ZOSCodePage;
  {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
  if ZCompatibleCodePages(zCP_UTF8, FSystemCodePage) then
  begin
    {$IFNDEF NO_ANSISTRING}
    ZAnsiToUTF8 := @ZMoveAnsiToUTF8;
    ZUTF8ToAnsi := @ZMoveUTF8ToAnsi;
    {$ENDIF}
    ZUTF8ToString := @ZMoveUTF8ToString;
    ZStringToUTF8 := @ZMoveStringToUTF8;
  end
  else
  begin
    {$IFNDEF NO_ANSISTRING}
    ZAnsiToUTF8 := @ZConvertAnsiToUTF8;
    ZUTF8ToAnsi := @ZConvertUTF8ToAnsi;
    {$ENDIF}
    ZUTF8ToString := @ZConvertUTF8ToString;
    ZStringToUTF8 := @ZConvertStringToUTF8;
  end;
  {$ENDIF}
end;

{**
  Assigns one variant value to another one.
  @param SrcValue a source variant value.
  @param DstValue a destination variant value.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.Assign(const SrcValue: TZVariant;
  out DstValue: TZVariant);
begin
  DstValue.VType := SrcValue.VType;
  case SrcValue.VType of
    vtBoolean: DstValue.VBoolean := SrcValue.VBoolean;
    vtBytes: DstValue.VBytes := SrcValue.VBytes;
    vtInteger: DstValue.VInteger := SrcValue.VInteger;
    vtUInteger: DstValue.VUInteger := SrcValue.VUInteger;
    vtFloat: DstValue.VFloat := SrcValue.VFloat;
    vtString: DstValue.VString := SrcValue.VString;
{$IFNDEF NO_ANSISTRING}
    vtAnsiString: DstValue.VAnsiString := SrcValue.VAnsiString;
{$ENDIF}
    vtRawByteString: DstValue.VRawByteString := SrcValue.VRawByteString;
{$IFNDEF NO_UTF8STRING}
    vtUTF8String: DstValue.VUTF8String := SrcValue.VUTF8String;
{$ENDIF}
    vtCharRec: DstValue.VCharRec := SrcValue.VCharRec;
    vtUnicodeString: DstValue.VUnicodeString := SrcValue.VUnicodeString;
    vtDateTime: DstValue.VDateTime := SrcValue.VDateTime;
    vtPointer: DstValue.VPointer := SrcValue.VPointer;
    vtInterface: DstValue.VInterface := SrcValue.VInterface;
  end;
end;

{**
  Clones a variant value.
  @param Value a source variant value.
  @returns a clonned variant value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.Clone(const Value: TZVariant): TZVariant;
begin
  Assign(Value, Result);
end;

{**
  Raises a type mismatch exception.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.RaiseTypeMismatchError;
begin
  raise EZVariantException.Create(STypesMismatch);
end;

{**
  Raises an unsupported operation exception.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.RaiseUnsupportedOperation;
begin
  raise EZVariantException.Create(SUnsupportedOperation);
end;

{**
  Converts a specified variant value to a new type.
  @param Value a variant value to be converted.
  @param NewType a type of the result variant value.
  @returns a converted variant value.
}
{$IFDEF ZEOS_TEST_ONLY}
function TZDefaultVariantManager.Convert(const Value: TZVariant;
  NewType: TZVariantType): TZVariant;
var
  UniTemp: ZWideString;
label VStringToCharRec; //ugly but saves code lines (;
begin
  Result.VType := NewType;
  case NewType of
    vtBoolean:
      case Value.VType of
        vtNull:
          Result.VBoolean := False;
        vtBoolean:
          Result.VBoolean := Value.VBoolean;
        else
          RaiseTypeMismatchError;
      end;
    vtBytes:
      case Value.VType of
        vtNull:
          Result.VBytes := nil;
        vtBytes:
          Result.VBytes := Value.VBytes;
        vtString:
          Result.VBytes := StrToBytes(Value.VString);
        {$IFNDEF NO_ANSISTRING}
        vtAnsiString:
          Result.VBytes := StrToBytes(Value.VAnsiString);
        {$ENDIF}
        vtRawByteString:
          Result.VBytes := {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}StrToBytes{$ENDIF}(Value.VRawByteString);
        {$IFNDEF NO_UTF8STRING}
        vtUTF8String:
          Result.VBytes := StrToBytes(Value.VUTF8String);
        {$ENDIF}
        vtUnicodeString:
          Result.VBytes := StrToBytes(Value.VUnicodeString);
        else
          RaiseTypeMismatchError;
      end;
    vtInteger:
      case Value.VType of
        vtNull:
          Result.VInteger := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VInteger := 1
          else
            Result.VInteger := 0;
        vtInteger:
          Result.VInteger := Value.VInteger;
        vtUInteger:
          Result.VInteger := Value.VUInteger;
        vtFloat:
          Result.VInteger := Trunc(Value.VFloat);
        vtString:
          Result.VInteger := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(Value.VString, 0);
        vtAnsiString:
          Result.VInteger := RawToInt64Def(Value.VAnsiString, 0);
        vtRawByteString:
          Result.VInteger := RawToInt64Def(Value.VRawByteString, 0);
        vtUTF8String:
          Result.VInteger := RawToInt64Def(Value.VUTF8String, 0);
        vtUnicodeString:
          Result.VInteger := UnicodeToInt64Def(Value.VUnicodeString, 0);
        else
          RaiseTypeMismatchError;
      end;
    vtUInteger:
      case Value.VType of
        vtNull:
          Result.VUInteger := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VUInteger := 1
          else
            Result.VUInteger := 0;
        vtInteger:
          Result.VUInteger := Value.VInteger;
        vtUInteger:
          Result.VUInteger := Value.VUInteger;
        vtString:
          Result.VUInteger := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(Value.VString, 0);
        vtAnsiString:
          Result.VUInteger := RawToUInt64Def(Value.VAnsiString, 0);
        vtRawByteString:
          Result.VUInteger := RawToUInt64Def(Value.VRawByteString, 0);
        vtUTF8String:
          Result.VUInteger := RawToUInt64Def(Value.VUTF8String, 0);
        vtUnicodeString:
          Result.VUInteger := UnicodeToUInt64Def(Value.VUnicodeString, 0);
        else
          RaiseTypeMismatchError;
      end;
    vtFloat:
      case Value.VType of
        vtNull:
          Result.VFloat := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VFloat := 1
          else
            Result.VFloat := 0;
        vtInteger:
          Result.VFloat := Value.VInteger;
        vtFloat:
          Result.VFloat := Value.VFloat;
        else
          RaiseTypeMismatchError;
      end;
    vtString:
      case Value.VType of
        vtNull:
          Result.VString := '';
        vtBytes:
          ZSetString(Pointer(Value.VBytes), Length(Value.VBytes), Result.VString);
        vtString:
          Result.VString := Value.VString;
        vtAnsiString:
          Result.VString := {$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString);
        vtUTF8String:
          Result.VString := ZUTF8ToString(Value.VUTF8String, FSystemCodePage);
        vtUnicodeString:
          Result.VString := {$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString);
        vtCharRec:
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
            {$IFDEF UNICODE}
            SetString(Result.VString, PChar(Value.VCharRec.P), Value.VCharRec.Len)
            {$ELSE}
            Result.VString := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage)
            {$ENDIF}
          else
            {$IFNDEF UNICODE}
            if ZCompatibleCodePages(ZOSCodePage, Value.VCharRec.CP) then
              SetString(Result.VString, PChar(Value.VCharRec.P), Value.VCharRec.Len)
            else
            {$ENDIF}
              Result.VString := {$IFNDEF UNICODE}String{$ENDIF}(PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP));
        else
          RaiseTypeMismatchError;
      end;
    vtAnsiString:
      case Value.VType of
        vtNull:
          Result.VAnsiString := '';
        vtBytes:
          ZSetString(Pointer(Value.VBytes), Length(Value.VBytes), Result.VString);
        vtString:
          Result.VAnsiString := {$IFDEF UNICODE}AnsiString{$ENDIF}(Value.VString);
        vtAnsiString:
          Result.VAnsiString := Value.VAnsiString;
        vtUTF8String:
          Result.VAnsiString := ZUTF8ToAnsi(Value.VUTF8String);
        vtUnicodeString:
          Result.VAnsiString := AnsiString(Value.VUnicodeString);
        vtCharRec:
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
          begin
            SetString(UniTemp, PWideChar(Value.VCharRec.P), Value.VCharRec.Len);
            Result.VAnsiString := AnsiString(UniTemp);
          end
          else
            if ZCompatibleCodePages(ZOSCodePage, Value.VCharRec.CP) then
              SetString(Result.VAnsiString, PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len)
            else
              Result.VAnsiString := AnsiString(PRawToUnicode(Value.VCharRec.P,
                Value.VCharRec.Len, Value.VCharRec.CP));
        else
          RaiseTypeMismatchError;
      end;
    vtUTF8String:
      case Value.VType of
        vtNull:
          Result.VUTF8String := '';
        vtBytes:
          ZSetString(Pointer(Value.VBytes), Length(Value.VBytes), Result.VString);
        vtString:
          Result.VUTF8String := ZStringToUTF8(Value.VString, FSystemCodePage);
       vtAnsiString:
          Result.VUTF8String := ZAnsiToUTF8(Value.VAnsiString);
        vtUTF8String:
          Result.VUTF8String := Value.VUTF8String;
        vtUnicodeString:
          Result.VUTF8String := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(Value.VUnicodeString);
        vtCharRec:
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
          begin
            SetString(UniTemp, PChar(Value.VCharRec.P), Value.VCharRec.Len);
            Result.VUTF8String := {$IFDEF WITH_RAWBYTETRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(UniTemp);
          end
          else
            if ZCompatibleCodePages(zCP_UTF8, Value.VCharRec.CP) then
              ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, Result.VUTF8String)
            else
              Result.VAnsiString := AnsiString(PRawToUnicode(Value.VCharRec.P,
                Value.VCharRec.Len, Value.VCharRec.CP));
        else
          RaiseTypeMismatchError;
      end;
    vtRawByteString:
      case Value.VType of
        vtNull:
          Result.VRawByteString := '';
        vtBytes:
          ZSetString(Pointer(Value.VBytes), Length(Value.VBytes), Result.VString);
        vtRawByteString:
          Result.VRawByteString := Value.VRawByteString;
        else
          RaiseTypeMismatchError;
      end;
    vtUnicodeString:
      case Value.VType of
        vtNull:
          Result.VUnicodeString := '';
        vtString:
          Result.VUnicodeString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(Value.VString); //Cast ansi to Wide/Unicode
        vtAnsiString:
          Result.VUnicodeString := ZWideString(Value.VAnsiString); //Cast ansi to Wide/Unicode
        vtUTF8String:
          Result.VUnicodeString :=
            {$IFDEF WITH_RAWBYTESTRING}
            ZWideString(Value.VUTF8String);
            {$ELSE}
            UTF8ToString(Value.VUTF8String);
            {$ENDIF}
        vtUnicodeString:
          Result.VUnicodeString := Value.VUnicodeString;
        vtCharRec:
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
            SetString(Result.VUnicodeString, PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
          else
            Result.VUnicodeString := PRawToUnicode(Value.VCharRec.P,
              Value.VCharRec.Len, Value.VCharRec.CP);
        else
          RaiseTypeMismatchError;
      end;
    vtDateTime:
      case Value.VType of
        vtNull:
          Result.VDateTime := 0;
        vtDateTime:
          Result.VDateTime := Value.VDateTime;
        else
          RaiseTypeMismatchError;
      end;
    vtPointer:
      case Value.VType of
        vtNull:
          Result.VPointer := nil;
        vtPointer:
          Result.VPointer := Value.VPointer;
        else
          RaiseTypeMismatchError;
      end;
    vtInterface:
      case Value.VType of
        vtNull:
          Result.VInterface := nil;
        vtInterface:
          Result.VInterface := Value.VInterface;
        else
          RaiseTypeMismatchError;
      end;
    vtCharRec:
      case Value.VType of
        vtNull:
          begin
            Result.VCharRec.Len := 0;
            Result.VCharRec.CP := High(Word);
            Result.VCharRec.P := nil;
          end;
        vtBoolean, vtInteger, vtFloat, vtBytes, vtDateTime:
          begin
            Result.VString := Convert(Value, vtString).VString;
            goto VStringToCharRec;
          end;
        vtString:
          begin
            Result.VString := Value.VString;
VStringToCharRec:
            Result.VCharRec.CP := {$IFDEF UNICODE}zCP_UTF16{$ELSE}ZOSCodePage{$ENDIF};
            if Pointer(Result.VString) = nil then
            begin
              Result.VCharRec.Len := 0;
              Result.VCharRec.P := {$IFDEF UNICODE}PEmptyUnicodeString{$ELSE}PEmptyAnsiString{$ENDIF};
            end
            else
            begin
              Result.VCharRec.Len := PLengthInt(NativeUInt(Result.VString) - StringLenOffSet)^; //fast Length() helper
              Result.VCharRec.P := Pointer(Result.VString); //avoid RTL call of PChar conversion
            end;
          end;
        vtAnsiString:
          begin
            Result.VAnsiString := Value.VAnsiString;
            Result.VCharRec.CP := ZOSCodePage;
            if Pointer(Result.VAnsiString) = nil then
            begin
              Result.VCharRec.Len := 0;
              Result.VCharRec.P := PEmptyAnsiString;
            end
            else
            begin
              Result.VCharRec.Len := PLengthInt(NativeUInt(Result.VAnsiString) - StringLenOffSet)^; //fast Length()
              Result.VCharRec.P := Pointer(Result.VAnsiString); //avoid RTL call of PChar conversion
            end;
          end;
        vtUTF8String:
          begin
            Result.VUTF8String := Value.VUTF8String;
            Result.VCharRec.CP := zCP_UTF8;
            if Pointer(Result.VUTF8String) = nil then
            begin
              Result.VCharRec.Len := 0;
              Result.VCharRec.P := PEmptyAnsiString;
            end
            else
            begin
              Result.VCharRec.Len := PLengthInt(NativeUInt(Result.VUTF8String) - StringLenOffSet)^; //fast Length() helper
              Result.VCharRec.P := Pointer(Result.VUTF8String); //avoid RTL call of PChar conversion
            end;
          end;
        vtUnicodeString:
          begin
            Result.VUnicodeString := Value.VUnicodeString;
            Result.VCharRec.CP := zCP_UTF16;
            Result.VCharRec.Len := Length(Result.VUnicodeString);
            if Result.VCharRec.Len = 0 then
              Result.VCharRec.P := PEmptyUnicodeString
            else
              Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL call of PChar conversion
          end;
        vtCharRec:
          Result.VCharRec := Value.VCharRec;
        else
          RaiseTypeMismatchError;
      end;
  end;
end;
{$ENDIF ZEOS_TEST_ONLY}

{**
  Compares two variant values.
  @param Value1 the first variant value.
  @param Value2 the second variant value.
  @return <0 if Value1 < Value 2, =0 if Value1 = Value2, >0 if Value1 > Value2
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.Compare(const Value1,
  Value2: TZVariant): Integer;
var
  TempFloat: Extended;
  i: Int64;
  TempDateTime: TDateTime;
label DoWideCompare;
begin
  case Value1.VType of
    vtNull: if IsNull(Value2) then
              Result := 0 else
              Result := -1;
    vtBoolean:
      Result := Ord(Value1.VBoolean)-Ord(GetAsBoolean(Value2));
    vtInteger: begin
      //EH: aware of range overflow(result is an integer not a Int64) comparing hashes leads to pain:
        i := GetAsInteger(Value2);
        Result := Ord(Value1.VInteger > I)-Ord(Value1.VInteger < I);
      end;
    vtFloat:
      begin
        TempFloat := GetAsFloat(Value2);
        if Value1.VFloat - TempFloat < -FLOAT_COMPARE_PRECISION then
          Result := -1
        else if Value1.VFloat - TempFloat > FLOAT_COMPARE_PRECISION then
          Result := 1
        else
          Result := 0;
      end;
    vtString:
      Result := AnsiCompareStr(Value1.VString, GetAsString(Value2));
{$IFNDEF NO_ANSISTRING}
    vtAnsiString:
      Result := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiCompareStr(Value1.VAnsiString, GetAsAnsiString(Value2));
{$ENDIF}
{$IFNDEF NO_UTF8STRING}
    vtUTF8String,
{$ENDIF}
    vtRawByteString:
DoWideCompare:
      {$IF defined(Delphi) and defined(UNICODE)}
      Result := AnsiCompareStr(GetAsUnicodeString(Value1), GetAsUnicodeString(Value2));
      {$ELSE}
      Result := WideCompareStr(GetAsUnicodeString(Value1), GetAsUnicodeString(Value2));
      {$IFEND}
    vtCharRec:
      if ZCompatibleCodePages(Value1.VCharRec.CP, zCP_UTF16) then
        {$IFDEF UNICODE}
        Result := AnsiStrComp(PWideChar(Value1.VCharRec.P), PWideChar(GetAsUnicodeString(Value2)))
        {$ELSE}
        Result := WideCompareStr(PWideChar(Value1.VCharRec.P), PWideChar(GetAsUnicodeString(Value2)))
        {$ENDIF}
      else
      {$IFNDEF NO_ANSISTRING}
        if ZCompatibleCodePages(Value1.VCharRec.CP, ZOSCodePage) then
          Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrComp(PAnsiChar(Value1.VCharRec.P), PAnsiChar(Pointer(GetAsAnsiString(Value2))))
        else
      {$ENDIF}
          goto DoWideCompare;
    vtUnicodeString:
{$IFNDEF FPC}
   {$IFDEF UNICODE}
      Result := AnsiCompareStr(Value1.VUnicodeString, GetAsUnicodeString(Value2));
   {$ELSE}
      Result := WideCompareStr(Value1.VUnicodeString, GetAsUnicodeString(Value2));
   {$ENDIF}
{$ELSE}
      Result := WideCompareStr(Value1.VUnicodeString, GetAsUnicodeString(Value2));
//      Result := AnsiCompareStr(AnsiString(Value1.VUnicodeString), GetAsString(Value2));
{$ENDIF}
    vtDateTime:
      begin
        TempDateTime := GetAsDateTime(Value2);
        if Value1.VDateTime < TempDateTime then
          Result := -1
        else if Value1.VDateTime > TempDateTime then
          Result := 1
        else
          Result := 0;
      end;
    vtPointer:
      Result := sign(Int64({%H-}NativeUInt(Value1.VPointer) - GetAsUInteger(Value2)));
    else
      Result := 0;
  end;
end;

{**
  Checks is the specified value NULL.
  @param Value a value to be checked.
  @returns <code>True</code> if variant has NULL value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.IsNull(const Value: TZVariant): Boolean;
begin
  Result := Value.VType = vtNull;
end;

{**
  Sets the NULL value to specified variant.
  @param Value variant value to be set to NULL.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetNull(out Value: TZVariant);
begin
  Value := EncodeNull;
end;

{**
  Gets a variant to boolean value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsBoolean(
  const Value: TZVariant): Boolean;
begin
  Result := Convert(Value, vtBoolean).VBoolean;
end;

{**
  Gets a variant to boolean value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsBytes(
  const Value: TZVariant): TBytes;
begin
  Result := Convert(Value, vtBytes).VBytes;
end;

{**
  Gets a variant to integer value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsInteger(
  const Value: TZVariant): Int64;
begin
  Result := Convert(Value, vtInteger).VInteger;
end;

{**
  Gets a variant to UInt64 value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsUInteger(
  const Value: TZVariant): UInt64;
begin
  Result := Convert(Value, vtUInteger).VUInteger;
end;

{**
  Gets a variant to float value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsFloat(
  const Value: TZVariant): Extended;
begin
  Result := Convert(Value, vtFloat).VFloat;
end;

{**
  Gets a variant to string value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsString(
  const Value: TZVariant): String;
begin
  Result := Convert(Value, vtString).VString;
end;

{**
  Gets a variant to string value.
  @param Value a variant to be converted.
  @param a result value.
}
{$IFNDEF NO_ANSISTRING}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsAnsiString(
  const Value: TZVariant): AnsiString;
begin
  Result := Convert(Value, vtAnsiString).VAnsiString;
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsUTF8String(const Value: TZVariant): UTF8String;
begin
  Result := Convert(Value, vtUTF8String).VUTF8String;
end;
{$ENDIF}

function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsRawByteString(const Value: TZVariant): RawByteString;
begin
  Result := Convert(Value, vtRawByteString).VRawByteString;
end;

{$IFDEF FPC} // parameters not used intentionally
  {$PUSH}
  {$WARN 5033 off : Function result does not seem to be set}
  {$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsRawByteString(const Value: TZVariant;
  const RawCP: Word): RawByteString;
begin
  RaiseUnsupportedOperation
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsCharRec(const Value: TZVariant): TZCharRec;
begin
  Result := Convert(Value, vtCharRec).VCharRec;
end;

{**
  Gets a variant to unicode string value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsUnicodeString(
  const Value: TZVariant): ZWideString;
begin
  Result := Convert(Value, vtUnicodeString).VUnicodeString;
end;

{**
  Gets a variant to date and time value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsDateTime(
  const Value: TZVariant): TDateTime;
begin
  Result := Convert(Value, vtDateTime).VDateTime;
end;

{**
  Gets a variant to pointer value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsPointer(
  const Value: TZVariant): Pointer;
begin
  Result := Convert(Value, vtPointer).VPointer;
end;

{**
  Gets a variant to interface value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsInterface(
  const Value: TZVariant): IZInterface;
begin
  Result := Convert(Value, vtInterface).VInterface;
end;

{**
  Gets a variant to TZArray value.
  @param Value a variant to be converted.
  @param a result value.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.GetAsArray(
  const Value: TZVariant): TZArray;
begin
  Result := Convert(Value, vtArray).VArray;
end;

{**
  Assigns a boolean value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsBoolean(out Value: TZVariant;
  const Data: Boolean);
begin
  Value := EncodeBoolean(Data);
end;

{**
  Assigns a Byte array value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsBytes(out Value: TZVariant;
  const Data: TBytes);
begin
  Value := EncodeBytes(Data);
end;

{**
  Assigns an integer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsInteger(out Value: TZVariant;
  const Data: Int64);
begin
  Value := EncodeInteger(Data);
end;

{**
  Assigns an integer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsUInteger(out Value: TZVariant;
  const Data: UInt64);
begin
  Value := EncodeUInteger(Data);
end;

{**
  Assigns a float value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsFloat(out Value: TZVariant;
  const Data: Extended);
begin
  Value := EncodeFloat(Data);
end;

{**
  Assigns a String value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsString(out Value: TZVariant;
  const Data: String);
begin
  Value := EncodeString(Data);
end;

{**
  Assigns a AnsiString value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
{$IFNDEF NO_ANSISTRING}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsAnsiString(out Value: TZVariant;
  const Data: AnsiString);
begin
  Value := EncodeAnsiString(Data);
end;
{$ENDIF}

{**
  Assigns a UTF8string value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
{$IFNDEF NO_UTF8STRING}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsUTF8String(out Value: TZVariant;
  const Data: UTF8String);
begin
  Value := EncodeUTF8String(Data);
end;
{$ENDIF}

{**
  Assigns a RawByteString value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsRawByteString(out Value: TZVariant;
  const Data: RawByteString);
begin
  Value := EncodeRawByteString(Data);
end;

{**
  Assigns a RawByteString value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsCharRec(out Value: TZVariant;
  const Data: TZCharRec);
begin
  Value := EncodeCharRec(Data);
end;

{**
  Assigns a unicode string value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsUnicodeString(out Value: TZVariant;
  const Data: ZWideString);
begin
  Value := EncodeUnicodeString(Data);
end;

{**
  Assigns a datetime value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsDateTime(out Value: TZVariant;
  const Data: TDateTime);
begin
  Value := EncodeDateTime(Data);
end;

{**
  Assigns a pointer value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsPointer(out Value: TZVariant;
  const Data: Pointer);
begin
  Value := EncodePointer(Data);
end;

{**
  Assigns a interface value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsInterface(out Value: TZVariant;
  const Data: IZInterface);
begin
  Value := EncodeInterface(Data);
end;

{**
  Assigns a TZArray value to variant.
  @param Value a variant to store the value.
  @param Data a value to be assigned.
}
procedure {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.SetAsArray(out Value: TZVariant;
  const Data: TZArray);
begin
  Value := EncodeArray(Data);
end;

{**
  Performs '+' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpAdd(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger + GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VUInteger + GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtFloat: Result := EncodeFloat(Value1.VFloat + GetAsFloat(Value2));
    vtString: Result := EncodeString(Value1.VString + GetAsString(Value2));
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: Result := EncodeAnsiString(Value1.VAnsiString + GetAsAnsiString(Value2));
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: Result := EncodeUTF8String(Value1.VUTF8String + GetAsUTF8String(Value2));
    {$ENDIF}
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    vtRawByteString: Result := EncodeRawByteString(RawConcat([Value1.VRawByteString,GetAsRawByteString(Value2)]));
    {$ELSE}
    vtRawByteString: Result := EncodeRawByteString(Value1.VRawByteString + GetAsRawByteString(Value2));
    {$ENDIF}
    vtUnicodeString: Result := EncodeUnicodeString(Value1.VUnicodeString + GetAsUnicodeString(Value2));
    vtDateTime: Result := EncodeDateTime(Value1.VDateTime + GetAsDateTime(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '&' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpAnd(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtBoolean: Result := EncodeBoolean(Value1.VBoolean and GetAsBoolean(Value2));
    vtInteger: Result := EncodeInteger(Value1.VInteger and GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VUInteger and GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '/' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpDiv(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger div GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VUInteger div GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtFloat: Result := EncodeFloat(Value1.VFloat / GetAsFloat(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) = 0);
end;

{**
  Performs '<' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpLess(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) < 0);
end;

{**
  Performs '<=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpLessEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) <= 0);
end;

{**
  Performs '%' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpMod(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger mod GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VUInteger mod GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '>' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpMore(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) > 0);
end;

{**
  Performs '>=' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpMoreEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) >= 0);
end;

{**
  Performs '*' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpMul(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger * GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VUInteger * GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtFloat: Result := EncodeFloat(Value1.VFloat * GetAsFloat(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs unary '-' operation.
  @param Value the variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpNegative(const Value: TZVariant): TZVariant;
begin
  case Value.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(-Value.VInteger);
    vtFloat: Result := EncodeFloat(-Value.VFloat);
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '~' operation.
  @param Value the variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpNot(const Value: TZVariant): TZVariant;
begin
  case Value.VType of
    vtNull: Result := EncodeNull;
    vtBoolean: Result := EncodeBoolean(not Value.VBoolean);
    vtInteger: Result := EncodeInteger(not Value.VInteger);
    vtUInteger: Result := EncodeUInteger(not Value.VUInteger);
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '<>' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpNotEqual(const Value1,
  Value2: TZVariant): TZVariant;
begin
  Result := EncodeBoolean(Compare(Value1, Value2) <> 0);
end;

{**
  Performs '|' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpOr(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: SetNull(Result{%H-});
    vtBoolean: Result := EncodeBoolean(Value1.VBoolean or GetAsBoolean(Value2));
    vtInteger: Result := EncodeInteger(Value1.VInteger or GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VInteger or GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '^' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpPow(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeFloat(Power(Value1.VInteger, GetAsInteger(Value2)));
    vtUInteger: Result := EncodeFloat(Power(Value1.VUInteger, GetAsUInteger(Value2)));
    vtFloat: Result := EncodeFloat(Power(Value1.VFloat, GetAsFloat(Value2)));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '-' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpSub(const Value1,
  Value2: TZVariant): TZVariant;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtInteger: Result := EncodeInteger(Value1.VInteger - GetAsInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger: Result := EncodeUInteger(Value1.VUInteger - GetAsUInteger(Value2));
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtFloat: Result := EncodeFloat(Value1.VFloat - GetAsFloat(Value2));
    else RaiseUnsupportedOperation;
  end;
end;

{**
  Performs '^' operation.
  @param Value1 the first variant argument.
  @param Value2 the second variant argument.
  @returns an operation result.
}
function {$IFDEF ZEOS_TEST_ONLY}TZDefaultVariantManager{$ELSE}TZSoftVariantManager{$ENDIF}.OpXor(const Value1,
  Value2: TZVariant): TZVariant;
var
  TempBool1, TempBool2: Boolean;
  TempInteger1, TempInteger2: Int64;
  TempUInteger1, TempUInteger2: UInt64;
begin
  case Value1.VType of
    vtNull: Result := EncodeNull;
    vtBoolean:
      begin
        TempBool1 := Value1.VBoolean;
        TempBool2 := GetAsBoolean(Value2);
        Result := EncodeBoolean((TempBool1 and not TempBool2)
          or (not TempBool1 and TempBool2));
      end;
    vtInteger:
      begin
        TempInteger1 := Value1.VInteger;
        TempInteger2 := GetAsInteger(Value2);
        Result := EncodeInteger((TempInteger1 and not TempInteger2)
          or (not TempInteger1 and TempInteger2));
      end;
    vtUInteger:
      begin
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
        TempUInteger1 := Value1.VUInteger;
        TempUInteger2 := GetAsUInteger(Value2);
        Result := EncodeUInteger((TempUInteger1 and not TempUInteger2)
          or (not TempUInteger1 and TempUInteger2));
        {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
      end;
    else RaiseUnsupportedOperation;
  end;
end;

{ TZSoftVariantManager }

{**
  Converts a specified variant value to a new type.
  @param Value a variant value to be converted.
  @param NewType a type of the result variant value.
  @returns a converted variant value.
}
function TZSoftVariantManager.Convert(const Value: TZVariant;
  NewType: TZVariantType): TZVariant;

  { !!! Performance note !!!
    This method is executed VERY often so it must be fast as possible.
  But if all the code is placed as one single piece, compiler will add LOTS
  of UStrClr, FinalizeArray etc in the method's prolog. This is caused by string
  assignments so we extract cases of every string type into separate function.
    This even beats additional time for nested function call: every string type case
  adds N finalizing so single piece of code makes N*M (M - number of string types, 6 here)
  calls while variant with nested functions only makes 1 + N calls.
    For string types conversions speedup is just 2 times and for non-string is about 40 TIMES!

    Every nested function probably could be optimized further... }

  procedure ProcessString(const Value: TZVariant; out Result: TZVariant);
  begin
    Result.VType := vtString;
    case Value.VType of
      vtNull:
        Result.VString := '';
      vtBoolean:
        Result.VString := BoolStrsUp[Value.VBoolean];
      vtBytes:
        ZSetString(Pointer(Value.VBytes), Length(Value.VBytes), Result.VString);
      vtInteger:
        Result.VString := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value.VInteger);
      vtUInteger:
        Result.VString := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value.VUInteger);
      vtFloat:
        Result.VString := FloatToSqlStr(Value.VFloat);
      vtString:
        Result.VString := Value.VString;
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        Result.VString := {$IFDEF UNICODE}String{$ENDIF}(Value.VAnsiString);
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String:
        Result.VString := ZUTF8ToString(Value.VUTF8String, FSystemCodePage);
      {$ENDIF}
      vtUnicodeString:
        Result.VString := {$IFNDEF UNICODE}String{$ENDIF}(Value.VUnicodeString); //hint: VarArrayOf(['Test']) returns allways varOleStr which is type WideString don't change that again
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
          {$IFDEF UNICODE}
          SetString(Result.VString, PChar(Value.VCharRec.P), Value.VCharRec.Len)
          {$ELSE}
          Result.VString := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage)
          {$ENDIF}
        else
        {$IFNDEF UNICODE}
        if ZCompatibleCodePages(ZOSCodePage, Value.VCharRec.CP) then
          SetString(Result.VString, PChar(Value.VCharRec.P), Value.VCharRec.Len)
        else
        {$ENDIF}
          Result.VString := {$IFNDEF UNICODE}String{$ENDIF}(PRawToUnicode(
            Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP));
      vtDateTime:
        Result.VString := DateTimeToAnsiSQLDate(Value.VDateTime);
      else
        RaiseTypeMismatchError;
    end;
  end;

  { !! Performance note: if values returned from functions are assigned directly
  to Result's field, the compiler generates lots of string clearing functions in
  the prolog. F.ex., for ProcessAnsiString overall prolog roundtrip consists of
  2*LStrArrayClr, 2*LStrClr and 3*UStrClr. Trick with local variable results in
  1*UStrArrayClr, 1*LStrClr and 1*UStrClr (eliminating 4 function calls! }
  {$IFNDEF NO_ANSISTRING}
  procedure ProcessAnsiString(const Value: TZVariant; out Result: TZVariant);
  var
    ResTmp: AnsiString;
  begin
    Result.VType := vtAnsiString;
    case Value.VType of
      vtNull:
        ResTmp := '';
      vtBoolean:
        ResTmp := BoolStrsUpRaw[Value.VBoolean];
      vtInteger:
        ResTmp := IntToRaw(Value.VInteger);
      vtUInteger:
        ResTmp := IntToRaw(Value.VUInteger);
      vtFloat:
        ResTmp := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(FloatToSqlStr(Value.VFloat));
      vtString:
        ResTmp := {$IFDEF UNICODE}AnsiString{$ENDIF}(Value.VString);
      vtAnsiString:
        ResTmp := Value.VAnsiString;
      vtUTF8String:
        ResTmp := ZUTF8ToAnsi(Value.VUTF8String);
      vtUnicodeString:
        ResTmp := ZUnicodeToRaw(Value.VUnicodeString, ZOSCodePage);
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
          ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage)
        else
          if ZCompatibleCodePages(ZOSCodePage, Value.VCharRec.CP) then
            SetString(ResTmp, PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len)
          else
            ResTmp := ZUnicodeToRaw(PRawToUnicode(Value.
              VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP), ZOSCodePage);
      vtDateTime:
        ResTmp := {$IFDEF UNICODE}AnsiString{$ENDIF}(DateTimeToAnsiSQLDate(Value.VDateTime));
      else
        RaiseTypeMismatchError;
    end;
    Result.VAnsiString := ResTmp;
  end;
  {$ENDIF}
  {$IFNDEF NO_UTF8STRING}
  procedure ProcessUTF8String(const Value: TZVariant; out Result: TZVariant);
  var
    ResTmp: UTF8String;
    UniTemp: ZWideString;
  begin
    Result.VType := vtUTF8String;
    case Value.VType of
      vtNull:
        ResTmp := '';
      vtBoolean:
        ResTmp := BoolStrsUpRaw[Value.VBoolean];
      vtInteger:
        ResTmp := IntToRaw(Value.VInteger);
      vtUInteger:
        ResTmp := IntToRaw(Value.VUInteger);
      vtFloat:
        ResTmp := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ENDIF}(FloatToSqlStr(Value.VFloat));
      vtString:
        ResTmp := ZStringToUTF8(Value.VString, FSystemCodePage);
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        ResTmp := ZAnsiToUTF8(Value.VAnsiString);
      {$ENDIF}
      vtUTF8String:
        ResTmp := Value.VUTF8String;
      vtUnicodeString:
        {$IFDEF WITH_RAWBYTESTRING}
        ResTmp := UTF8String(Value.VUnicodeString);
        {$ELSE}
        ResTmp := UTF8Encode(Value.VUnicodeString);
        {$ENDIF}
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        begin
          SetString(UniTemp, PChar(Value.VCharRec.P), Value.VCharRec.Len);
          ResTmp := {$IFDEF WITH_RAWBYTETRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(UniTemp);
        end
        else
          if ZCompatibleCodePages(zCP_UTF8, Value.VCharRec.CP) then
            ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp)
          else
            ResTmp := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(
              PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP));
      vtDateTime:
        ResTmp := UTF8String(DateTimeToAnsiSQLDate(Value.VDateTime));
      else
        RaiseTypeMismatchError;
    end;
    Result.VUTF8String := ResTmp;
  end;
  {$ENDIF}
  // Performance note: only two clearing functions here, nothing to shorten
  procedure ProcessRawByteString(const Value: TZVariant; out Result: TZVariant);
  begin
    Result.VType := vtRawByteString;
    case Value.VType of
      vtNull:
        Result.VRawByteString := EmptyRaw;
      vtBoolean:
        Result.VRawByteString := BoolStrsUpRaw[Value.VBoolean];
      vtInteger:
        Result.VRawByteString := IntToRaw(Value.VInteger);
      vtUInteger:
        Result.VRawByteString := IntToRaw(Value.VUInteger);
      vtFloat:
        Result.VRawByteString := FloatToSqlRaw(Value.VFloat);
      vtRawByteString:
        Result.VRawByteString := Value.VRawByteString;
      vtDateTime:
        Result.VRawByteString := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(DateTimeToAnsiSQLDate(Value.VDateTime));
      else
        RaiseTypeMismatchError;
    end;
  end;

  procedure ProcessUnicodeString(const Value: TZVariant; out Result: TZVariant);
  begin
    Result.VType := vtUnicodeString;
    case Value.VType of
      vtNull:
        Result.VUnicodeString := '';
      vtBoolean:
        Result.VUnicodeString := BoolStrsUpW[Value.VBoolean];
      vtInteger:
        Result.VUnicodeString := IntToUnicode(Value.VInteger);
      vtUInteger:
        Result.VUnicodeString := IntToUnicode(Value.VUInteger);
      vtFloat:
        Result.VUnicodeString := {$IFNDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FloatToSqlStr(Value.VFloat));
      vtString:
        Result.VUnicodeString := {$IFNDEF UNICODE}ZWideString{$ENDIF}(Value.VString);
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        Result.VUnicodeString := ZRawToUnicode(Value.VAnsiString, ZOSCodePage);
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String:
        Result.VUnicodeString := {$IFDEF WITH_RAWBYTESTRING}ZWideString{$ELSE}UTF8Decode{$ENDIF}(Value.VUTF8String);
      {$ENDIF}
      vtUnicodeString:
        Result.VUnicodeString := Value.VUnicodeString;
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
          SetString(Result.VUnicodeString, PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
        else
          Result.VUnicodeString := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
      vtDateTime:
        Result.VUnicodeString := {$IFNDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(DateTimeToAnsiSQLDate(Value.VDateTime));
      else
        RaiseTypeMismatchError;
    end;
  end;

  procedure ProcessBytes(const Value: TZVariant; out Result: TZVariant);
  begin
    Result.VType := vtBytes;
    case Value.VType of
      vtNull:
        Result.VBytes := nil;
      vtBytes:
        Result.VBytes := Value.VBytes;
      vtString:
        Result.VBytes := StrToBytes(Value.VString);
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        Result.VBytes := StrToBytes(Value.VAnsiString);
      {$ENDIF}
      vtRawByteString:
        Result.VBytes := {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}StrToBytes{$ENDIF}(Value.VRawByteString);
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String:
        Result.VBytes := StrToBytes(Value.VUTF8String);
      {$ENDIF}
      vtUnicodeString:
        Result.VBytes := StrToBytes(Value.VUnicodeString);
      vtCharRec:
        begin
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
            SetLength(Result.VBytes, Value.VCharRec.Len*2)
          else
            SetLength(Result.VBytes, Value.VCharRec.Len);
          {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Value.VCharRec.P^, Pointer(Result.VBytes)^, Length(Result.VBytes));
        end;
      else
        RaiseTypeMismatchError;
    end;
  end;

  procedure ProcessDateTime(const Value: TZVariant; out Result: TZVariant);
  begin
    Result.VType := vtDateTime;
    case Value.VType of
      vtNull:
        Result.VDateTime := 0;
      vtBoolean:
        RaiseTypeMismatchError;
      vtInteger:
        Result.VDateTime := Value.VInteger;
      vtUInteger:
        Result.VDateTime := Value.VUInteger;
      vtFloat:
        Result.VDateTime := Value.VFloat;
      vtString:
        Result.VDateTime := AnsiSQLDateToDateTime(Value.VString);
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        Result.VDateTime := AnsiSQLDateToDateTime({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(Value.VAnsiString));
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String:
        Result.VDateTime := AnsiSQLDateToDateTime({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(Value.VUTF8String));
      {$ENDIF}
      vtRawByteString:
        Result.VDateTime := AnsiSQLDateToDateTime({$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(Value.VRawByteString));
      vtUnicodeString:
        Result.VDateTime := AnsiSQLDateToDateTime({$IFNDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Value.VUnicodeString));
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        begin
          SetString(Result.VUnicodeString, PWideChar(Value.VCharRec.P), Value.VCharRec.Len);
          Result.VDateTime := AnsiSQLDateToDateTime({$IFNDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Value.VUnicodeString));
        end
        else
        begin
          {$IFDEF UNICODE}
          Result.VDateTime := AnsiSQLDateToDateTime(PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP));
          {$ELSE}
          SetString(Result.VString, PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len);
          Result.VDateTime := AnsiSQLDateToDateTime(Value.VString);
          {$ENDIF}
        end;
      vtDateTime:
        Result.VDateTime := Value.VDateTime;
      else
        RaiseTypeMismatchError;
    end;
  end;

  procedure ProcessCharRec(const Value: TZVariant; out Result: TZVariant);
  label AsVCharRecFromVString;
  begin
    Result.VType := vtCharRec;
    case Value.VType of
      vtNull:
        begin
          Result.VCharRec.Len := 0;
          Result.VCharRec.CP := High(Word);
          Result.VCharRec.P := nil;
        end;
      vtBoolean, vtInteger, vtUInteger, vtFloat, vtBytes, vtDateTime:
        begin
          Result.VString := Convert(Value, vtString).VString;
          Goto AsVCharRecFromVString;
        end;
      vtString:
        begin
          Result.VString := Value.VString;
AsVCharRecFromVString:
          Result.VCharRec.CP := {$IFDEF UNICODE}zCP_UTF16{$ELSE}ZOSCodePage{$ENDIF};
          if Pointer(Result.VString) = nil then
          begin
            Result.VCharRec.Len := 0;
            Result.VCharRec.P := {$IFDEF UNICODE}PEmptyUnicodeString{$ELSE}PEmptyAnsiString{$ENDIF};
          end
          else
          begin
            Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VString) - StringLenOffSet)^; //fast Length() helper
            Result.VCharRec.P := Pointer(Result.VString); //avoid RTL call of PChar conversion
          end;
        end;
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        begin
          Result.VAnsiString := Value.VAnsiString;
          Result.VCharRec.CP := ZOSCodePage;
          if Pointer(Result.VAnsiString) = nil then begin
            Result.VCharRec.Len := 0;
            Result.VCharRec.P := PEmptyAnsiString;
          end else begin
            Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VAnsiString) - StringLenOffSet)^; //fast Length() helper
            Result.VCharRec.P := Pointer(Result.VAnsiString); //avoid RTL call of PAnsiChar conversion
          end;
        end;
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String:
        begin
          Result.VUTF8String := Value.VUTF8String;
          Result.VCharRec.CP := zCP_UTF8;
          if Pointer(Result.VUTF8String) = nil then begin
            Result.VCharRec.Len := 0;
            Result.VCharRec.P := PEmptyAnsiString;
          end else begin
            Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VUTF8String) - StringLenOffSet)^; //fast Length() helper for D7..D9 where Length() isn't inlined;
            Result.VCharRec.P := Pointer(Result.VUTF8String); //avoid RTL call of PAnsiChar conversion
          end;
        end;
      {$ENDIF}
      vtUnicodeString:
        begin
          Result.VUnicodeString := Value.VUnicodeString;
          Result.VCharRec.CP := zCP_UTF16;
          Result.VCharRec.Len := Length(Result.VUnicodeString); //don't use PLengthInt helper: VUnicodeString may be Wide/Unicode-String
          if Result.VCharRec.Len = 0 then
            Result.VCharRec.P := PEmptyUnicodeString
          else
            Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL call of PWideChar conversion
        end;
      vtCharRec:
        Result.VCharRec := Value.VCharRec;
      else
        RaiseTypeMismatchError;
    end;
  end;

begin
  Result.VType := NewType;
  case NewType of
    vtBoolean:
      case Value.VType of
        vtNull:
          Result.VBoolean := False;
        vtBoolean:
          Result.VBoolean := Value.VBoolean;
        vtInteger:
          Result.VBoolean := Value.VInteger <> 0;
        vtUInteger:
          Result.VBoolean := Value.VUInteger <> 0;
        vtFloat:
          Result.VBoolean := Value.VFloat <> 0;
        vtString:
          Result.VBoolean := StrToBoolEx(Value.VString);
        {$IFNDEF NO_ANSISTRING}
        vtAnsiString:
          Result.VBoolean := StrToBoolEx(Value.VAnsiString);
        {$ENDIF}
        {$IFNDEF NO_UTF8STRING}
        vtUTF8String:
          Result.VBoolean := StrToBoolEx(Value.VUTF8String);
        {$ENDIF}
        vtRawByteString:
          Result.VBoolean := StrToBoolEx(Value.VRawByteString);
        vtUnicodeString:
          Result.VBoolean := StrToBoolEx(Value.VUnicodeString);
        vtCharRec:
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
            Result.VBoolean := StrToBoolEx(PWideChar(Value.VCharRec.P))
          else
            Result.VBoolean := StrToBoolEx(PAnsiChar(Value.VCharRec.P));
        vtDateTime:
          Result.VBoolean := Value.VDateTime <> 0;
        else
          RaiseTypeMismatchError;
      end;
    vtBytes:
      ProcessBytes(Value, Result);
    vtInteger:
      case Value.VType of
        vtNull:
          Result.VInteger := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VInteger := 1
          else
            Result.VInteger := 0;
        vtInteger:
          Result.VInteger := Value.VInteger;
        vtUInteger:
          Result.VInteger := Value.VUInteger;
        vtFloat:
          Result.VInteger := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value.VFloat);
        vtString:
          Result.VInteger := {$IFDEF UNICODE}UnicodeToInt64Def{$ELSE}RawToInt64Def{$ENDIF}(Value.VString, 0);
        {$IFNDEF NO_ANSISTRING}
        vtAnsiString:
          Result.VInteger := RawToInt64Def(Pointer(Value.VAnsiString), 0);
        {$ENDIF}
        {$IFNDEF NO_UTF8STRING}
        vtUTF8String:
          Result.VInteger := RawToInt64Def(Pointer(Value.VUTF8String), 0);
        {$ENDIF}
        vtRawByteString:
          Result.VInteger := RawToInt64Def(Pointer(Value.VRawByteString), 0);
        vtUnicodeString:
          Result.VInteger := UnicodeToInt64Def(Value.VUnicodeString, 0);
        vtCharRec:
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16)
          then Result.VInteger := UnicodeToInt64Def(PWideChar(Value.VCharRec.P), 0)
          else Result.VInteger := RawToInt64Def(PAnsiChar(Value.VCharRec.P), 0);
        vtDateTime:
          Result.VInteger := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value.VDateTime);
        vtPointer:
          Result.VInteger := Int64({%H-}NativeUInt(Value.VPointer));
        else
          RaiseTypeMismatchError;
      end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
    vtUInteger:
      case Value.VType of
        vtNull:
          Result.VUInteger := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VUInteger := 1
          else
            Result.VUInteger := 0;
        vtInteger:
          Result.VUInteger := Value.VInteger;
        vtUInteger:
          Result.VUInteger := Value.VUInteger;
        vtFloat:
          Result.VUInteger := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value.VFloat);
        vtString:
          Result.VUInteger := {$IFDEF UNICODE}UnicodeToUInt64Def{$ELSE}RawToUInt64Def{$ENDIF}(Value.VString, 0);
        {$IFNDEF NO_ANSISTRING}
        vtAnsiString:
          Result.VUInteger := RawToUInt64Def(Value.VAnsiString, 0);
        {$ENDIF}
        {$IFNDEF NO_UTF8STRING}
        vtUTF8String:
          Result.VUInteger := RawToUInt64Def(Value.VUTF8String, 0);
        {$ENDIF}
        vtRawByteString:
          Result.VUInteger := RawToUInt64Def(Value.VRawByteString, 0);
        vtUnicodeString:
          Result.VUInteger := UnicodeToUInt64Def(Value.VUnicodeString, 0);
        vtCharRec:
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
            Result.VUInteger := UnicodeToUInt64Def(Value.VCharRec.P, 0)
          else
            Result.VUInteger := RawToUInt64Def(Value.VCharRec.P, 0);
        vtDateTime:
          Result.VUInteger := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(Value.VDateTime);
        vtPointer:
          Result.VUInteger := {%H-}NativeUInt(Value.VPointer);
        vtInterface:
          RaiseTypeMismatchError;
      end;
    {$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}
    vtFloat:
      case Value.VType of
        vtNull:
          Result.VFloat := 0;
        vtBoolean:
          if Value.VBoolean then
            Result.VFloat := 1
          else
            Result.VFloat := 0;
        vtInteger:
          Result.VFloat := Value.VInteger;
        vtUInteger:
          Result.VFloat := Value.VUInteger;
        vtFloat:
          Result.VFloat := Value.VFloat;
        vtString:
          SqlStrToFloatDef(PChar(Pointer(Value.VString)), 0, Result.VFloat, Length(Value.VString));
        {$IFNDEF NO_ANSISTRING}
        vtAnsiString:
          SqlStrToFloatDef(PAnsiChar(Pointer(Value.VAnsiString)), 0, Result.VFloat, Length(Value.VAnsiString));
        {$ENDIF}
        {$IFNDEF NO_UTF8STRING}
        vtUTF8String:
          SqlStrToFloatDef(PAnsiChar(Pointer(Value.VUTF8String)), 0, Result.VFloat, Length(Value.VUTF8String));
        {$ENDIF}
        vtRawByteString:
          SqlStrToFloatDef(PAnsiChar(Pointer(Value.VRawByteString)), 0, Result.VFloat, Length(Value.VRawByteString));
        vtUnicodeString:
          SqlStrToFloatDef(PWideChar(Pointer(Value.VUnicodeString)), 0, Result.VFloat, Length(Value.VUnicodeString));
        vtCharRec:
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
            SqlStrToFloatDef(PWideChar(Value.VCharRec.P), 0, Result.VFloat, Value.VCharRec.Len)
          else
            SqlStrToFloatDef(PAnsiChar(Value.VCharRec.P), 0, Result.VFloat, Value.VCharRec.Len);
        vtDateTime:
          Result.VFloat := Value.VDateTime;
        else
          RaiseTypeMismatchError;
      end;
    vtDateTime:
      ProcessDateTime(Value, Result);
    vtPointer:
      case Value.VType of
        vtNull:
          Result.VPointer := nil;
        vtBoolean:
          RaiseTypeMismatchError;
        vtInteger:
          Result.VPointer := {%H-}Pointer(Value.VInteger);
        vtUInteger:
          Result.VPointer := {%H-}Pointer(Value.VUInteger);
        else
          RaiseTypeMismatchError;
      end;
    vtInterface:
      case Value.VType of
        vtNull:
          Result.VInterface := nil;
        vtInterface:
          Result.VInterface := Value.VInterface;
        else
      end;

    vtString:
      ProcessString(Value, Result);
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString:
      ProcessAnsiString(Value, Result);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String:
      ProcessUTF8String(Value, Result);
    {$ENDIF}
    vtRawByteString:
      ProcessRawByteString(Value, Result);

    vtUnicodeString:
      ProcessUnicodeString(Value, Result);

    vtCharRec:
      ProcessCharRec(Value, Result);
  end;
end;

{ TZClientVariantManager }

{**
  Constructs this object and assigns the main properties.
  @param ClientCodePage the current ClientCodePage.
}
constructor TZClientVariantManager.Create(const ConSettings: PZConSettings);
begin
  inherited Create; //Set all standart converters functions

  FConSettings := ConSettings;
end;

{**
  Converts a specified variant value to a new type.
  @param Value a variant value to be converted.
  @param NewType a type of the result variant value.
  @returns a converted variant value.
}
function TZClientVariantManager.Convert(const Value: TZVariant;
  NewType: TZVariantType): TZVariant;

  { !!! Performance note - read comment inside TZSoftVariantManager.Convert !!! }

  procedure ProcessString(const Value: TZVariant; out Result: TZVariant);
  begin
    Result.VType := vtString;
    case Value.VType of
      vtNull:
        Result.VString := '';
      vtBoolean:
        Result.VString := BoolStrsUp[Value.VBoolean];
      vtBytes:
        ZSetString(Pointer(Value.VBytes), Length(Value.VBytes), Result.VString);
      vtInteger:
        Result.VString := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value.VInteger);
      vtUInteger:
        Result.VString := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value.VUInteger);
      vtFloat:
        Result.VString := FloatToSqlStr(Value.VFloat);
      vtString:
        Result.VString := Value.VString;
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        Result.VString := FConSettings^.ConvFuncs.ZAnsiToString(Value.VAnsiString, FConSettings^.CTRL_CP);
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String:
        Result.VString := FConSettings^.ConvFuncs.ZUTF8ToString(Value.VUTF8String, FConSettings^.CTRL_CP);
      {$ENDIF}
      vtRawByteString:
        Result.VString := FConSettings^.ConvFuncs.ZRawToString(Value.VRawByteString, FConSettings^.ClientCodePage^.CP, FConSettings^.CTRL_CP);
     vtUnicodeString:
        //hint: VarArrayOf(['Test']) returns allways varOleStr which is type WideString don't change that again
        //this hint means a cast instead of convert. The user should better use WideString constants!
        Result.VString := FConSettings^.ConvFuncs.ZUnicodeToString(Value.VUnicodeString, FConSettings^.CTRL_CP);
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
          {$IFDEF UNICODE}
          SetString(Result.VString, PChar(Value.VCharRec.P), Value.VCharRec.Len)
          {$ELSE}
          Result.VString := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, ZOSCodePage)
          {$ENDIF}
        else
          {$IFNDEF UNICODE}
          if ZCompatibleCodePages(ZOSCodePage, Value.VCharRec.CP) then
            SetString(Result.VString, PChar(Value.VCharRec.P), Value.VCharRec.Len)
          else begin
            Result.VUnicodeString := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);//localice com widestring
            Result.VString := FConSettings^.ConvFuncs.ZUnicodeToString(Value.VUnicodeString, FConSettings^.CTRL_CP);
            Result.VUnicodeString := '';
          end;
          {$ELSE}
            Result.VString := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);//localice com widestring
          {$ENDIF}
      vtDateTime:
        Result.VString := ZSysUtils.{$IFDEF UNICODE}DateTimeToUnicodeSQLTimeStamp{$ELSE}DateTimeToRawSQLTimeStamp{$ENDIF}(Value.VDateTime, FConSettings^.WriteFormatSettings, False);
      else
        RaiseTypeMismatchError;
    end;
    Result.VType := vtString;
  end;
  {$IFNDEF NO_ANSISTRING}
  procedure ProcessAnsiString(const Value: TZVariant; out Result: TZVariant);
  var
    UniTemp: ZWideString;
    ResTmp: AnsiString;
  begin
    Result.VType := vtAnsiString;
    case Value.VType of
      vtNull:
        ResTmp := '';
      vtBoolean:
        ResTmp := BoolStrsUpRaw[Value.VBoolean];
      vtInteger:
        ResTmp := IntToRaw(Value.VInteger);
      vtUInteger:
        ResTmp := IntToRaw(Value.VUInteger);
      vtFloat:
        ResTmp := {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FloatToSqlStr(Value.VFloat));
      vtString:
        ResTmp := (FConSettings^.ConvFuncs.ZStringToAnsi(Value.VString, FConSettings^.CTRL_CP));
      vtAnsiString:
        ResTmp := Value.VAnsiString;
      vtUTF8String:
        ResTmp := FConSettings^.ConvFuncs.ZUTF8ToAnsi(Value.VUTF8String);
      vtRawByteString:
        ResTmp := FConSettings^.ConvFuncs.ZRawToAnsi(Value.VRawByteString, FConSettings^.ClientCodePage^.CP);
      vtUnicodeString:
        ResTmp := ZUnicodeToRaw(Value.VUnicodeString, ZOSCodePage);
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then begin
          SetString(UniTemp, PWideChar(Value.VCharRec.P), Value.VCharRec.Len); //localize com widestring
          ResTmp := PUnicodeToRaw(Pointer(UniTemp), Length(UniTemp), ZOSCodePage);
        end else if ZCompatibleCodePages(ZOSCodePage, Value.VCharRec.CP) then
          SetString(ResTmp, PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len)
        else begin
          UniTemp := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
          ResTmp := PUnicodeToRaw(Pointer(UniTemp), Length(UniTemp), ZOSCodePage);
        end;
      vtDateTime:
        ResTmp := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FConSettings^.WriteFormatSettings, False);
      else
        RaiseTypeMismatchError;
    end;
    Result.VAnsiString := ResTmp;
  end;
  {$ENDIF}
  {$IFNDEF NO_UTF8STRING}
  procedure ProcessUTF8String(const Value: TZVariant; out Result: TZVariant);
  var
    UniTemp: ZWideString;
    ResTmp: UTF8String;
  begin
    Result.VType := vtUTF8String;
    case Value.VType of
      vtNull:
        ResTmp := '';
      vtBoolean:
        ResTmp := BoolStrsUpRaw[Value.VBoolean];
      vtInteger:
        ResTmp := IntToRaw(Value.VInteger);
      vtUInteger:
        ResTmp := IntToRaw(Value.VUInteger);
      vtFloat:
        ResTmp := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(FloatToSqlStr(Value.VFloat));
      vtString:
        ResTmp := FConSettings^.ConvFuncs.ZStringToUTF8(Value.VString, FConSettings^.CTRL_CP);
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        ResTmp := FConSettings^.ConvFuncs.ZAnsiToUTF8(Value.VAnsiString);
      {$ENDIF}
      vtUTF8String:
        ResTmp := Value.VUTF8String;
      vtRawByteString:
        ResTmp := FConSettings^.ConvFuncs.ZRawToUTF8(Value.VRawByteString, FConSettings^.ClientCodePage^.CP);
      vtUnicodeString:
        {$IFDEF WITH_RAWBYTESTRING}
        ResTmp := UTF8String(Value.VUnicodeString);
        {$ELSE}
        ResTmp := UTF8Encode(Value.VUnicodeString);
        {$ENDIF}
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        begin
          SetString(UniTemp, PChar(Value.VCharRec.P), Value.VCharRec.Len);
          ResTmp := {$IFDEF WITH_RAWBYTETRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(UniTemp);
        end else if ZCompatibleCodePages(zCP_UTF8, Value.VCharRec.CP) then
          ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp)
        else begin
          UniTemp := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP); //localie com WideString
          ResTmp := {$IFDEF WITH_RAWBYTESTRING}UTF8String{$ELSE}UTF8Encode{$ENDIF}(UniTemp);
        end;
      vtDateTime:
        ResTmp := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FConSettings^.WriteFormatSettings, False);
      else
        RaiseTypeMismatchError;
    end;
    Result.VUTF8String := ResTmp;
  end;
  {$ENDIF}
  procedure ProcessRawByteString(const Value: TZVariant; out Result: TZVariant);
  var
    UniTemp: ZWideString;
    ResTmp: RawByteString;
  begin
    Result.VType := vtRawByteString;
    case Value.VType of
      vtNull:
        ResTmp := EmptyRaw;
      vtBoolean:
        ResTmp := BoolStrsUpRaw[Value.VBoolean];
      vtInteger:
        ResTmp := IntToRaw(Value.VInteger);
      vtUInteger:
        ResTmp := IntToRaw(Value.VUInteger);
      vtFloat:
        ResTmp := {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(FloatToSqlStr(Value.VFloat));
      vtString:
        ResTmp := FConSettings^.ConvFuncs.ZStringToRaw(Value.VString, FConSettings^.CTRL_CP, FConSettings^.ClientCodePage^.CP);
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        ResTmp := FConSettings^.ConvFuncs.ZAnsiToRaw(Value.VAnsiString, FConSettings^.ClientCodePage^.CP);
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String:
        ResTmp := FConSettings^.ConvFuncs.ZUTF8ToRaw(Value.VUTF8String, FConSettings^.ClientCodePage^.CP);
      {$ENDIF}
      vtRawByteString:
        ResTmp := Value.VRawByteString;
      vtUnicodeString:
        ResTmp := FConSettings^.ConvFuncs.ZUnicodeToRaw(Value.VUnicodeString, FConSettings^.ClientCodePage^.CP);
      vtDateTime:
        ResTmp := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FConSettings^.WriteFormatSettings, False);
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
          ResTmp := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, FConSettings^.ClientCodePage^.CP)
        else if ZCompatibleCodePages(FConSettings^.ClientCodePage^.CP, Value.VCharRec.CP) then
          ZSetString(PAnsiChar(Value.VCharRec.P), Value.VCharRec.Len, ResTmp)
        else begin
          UniTemp := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
          ResTmp := ZUnicodeToRaw(UniTemp, FConSettings^.ClientCodePage^.CP);
        end;
      else
        RaiseTypeMismatchError;
    end;
    Result.VRawByteString := ResTmp;
  end;

  procedure ProcessUnicodeString(const Value: TZVariant; out Result: TZVariant);
  begin
    Result.VType := vtUnicodeString;
    case Value.VType of
      vtNull:
        Result.VUnicodeString := '';
      vtBoolean:
        Result.VUnicodeString := BoolStrsUpW[Value.VBoolean];
      vtInteger:
        Result.VUnicodeString := IntToUnicode(Value.VInteger);
      vtUInteger:
        Result.VUnicodeString := IntToUnicode(Value.VUInteger);
      vtFloat:
        Result.VUnicodeString := {$IFNDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(FloatToSqlStr(Value.VFloat));
      vtString:
        Result.VUnicodeString := FConSettings^.ConvFuncs.ZStringToUnicode(Value.VString, FConSettings^.CTRL_CP);
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        Result.VUnicodeString := ZRawToUnicode(Value.VAnsiString, ZOSCodePage);
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String:
        Result.VUnicodeString := PRawToUnicode(Pointer(Value.VUTF8String), Length(Value.VUTF8String), zCP_UTF8);
      {$ENDIF}
      vtRawByteString:
        Result.VUnicodeString := FConSettings^.ConvFuncs.ZRawToUnicode(Value.VRawByteString, FConSettings^.ClientCodePage^.CP);
      vtUnicodeString:
        Result.VUnicodeString := Value.VUnicodeString;
      vtDateTime:
        Result.VUnicodeString := ZSysUtils.DateTimeToUnicodeSQLTimeStamp(Value.VDateTime, FConSettings^.WriteFormatSettings, False);
      vtCharRec:
        if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
          SetString(Result.VUnicodeString, PWideChar(Value.VCharRec.P), Value.VCharRec.Len)
        else
          Result.VUnicodeString := PRawToUnicode(Value.VCharRec.P,
            Value.VCharRec.Len, Value.VCharRec.CP);
      else
        RaiseTypeMismatchError;
    end;
  end;

  procedure ProcessCharRec(const Value: TZVariant; out Result: TZVariant);
  begin
    Result.VType := vtCharRec;
    case Value.VType of
      vtNull:
        begin
          Result.VCharRec.Len := 0;
          Result.VCharRec.CP := High(Word);
          Result.VCharRec.P := nil;
        end;
      vtBoolean, vtInteger, vtFloat, vtBytes, vtDateTime:
        begin
          if FConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
          begin
            Result.VRawByteString := Convert(Value, vtRawByteString).VRawByteString;
            Result.VCharRec.CP := FConSettings^.ClientCodePage^.CP;
            if Pointer(Result.VRawByteString) = nil then
            begin
              Result.VCharRec.Len := 0;
              Result.VCharRec.P := PEmptyAnsiString; //avoid nil result
            end
            else
            begin
              Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VRawByteString) - StringLenOffSet)^; //fast Length() helper
              Result.VCharRec.P := Pointer(Result.VRawByteString); //avoid RTL conversion to PAnsiChar
            end;
          end
          else
          begin
            Result.VUnicodeString := Convert(Value, vtUnicodeString).VUnicodeString;
            Result.VCharRec.CP := zCP_UTF16;
            Result.VCharRec.Len := Length(Result.VUnicodeString);
            if Result.VCharRec.Len = 0 then
              Result.VCharRec.P := PEmptyUnicodeString //avoid nil result
            else
              Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL conversion to PAnsiChar
          end;
        end;
      vtString:
        begin
          Result.VString := Value.VString;
          Result.VCharRec.CP := {$IFDEF UNICODE}zCP_UTF16{$ELSE}ZOSCodePage{$ENDIF};
          if Pointer(Result.VString) = nil then
          begin
            Result.VCharRec.Len := 0;
            Result.VCharRec.P := {$IFDEF UNICODE}PEmptyUnicodeString{$ELSE}PEmptyAnsiString{$ENDIF}; //avoid nil result
          end
          else
          begin
            Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VString) - StringLenOffSet)^; //fast Length() helper
            Result.VCharRec.P := Pointer(Result.VString); //avoid RTL conversion to PAnsiChar
          end;
        end;
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString:
        begin
          Result.VAnsiString := Value.VAnsiString;
          Result.VCharRec.CP := ZOSCodePage;
          if Pointer(Result.VAnsiString) = nil then
          begin
            Result.VCharRec.Len := 0;
            Result.VCharRec.P := PEmptyAnsiString; //avoid nil result
          end
          else
          begin
            Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VAnsiString) - StringLenOffSet)^; //fast Length() helper
            Result.VCharRec.P := Pointer(Result.VAnsiString); //avoid RTL conversion to PAnsiChar
          end;
        end;
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String:
        begin
          Result.VUTF8String := Value.VUTF8String;
          Result.VCharRec.CP := zCP_UTF8;
          if Pointer(Result.VUTF8String) = nil then begin
            Result.VCharRec.Len := 0;
            Result.VCharRec.P := PEmptyAnsiString; //avoid nil result
          end else begin
            Result.VCharRec.Len := {%H-}PLengthInt(NativeUInt(Result.VUTF8String) - StringLenOffSet)^; //fast Length() helper
            Result.VCharRec.P := Pointer(Result.VUTF8String); //avoid RTL conversion to PAnsiChar
          end;
        end;
      {$ENDIF}
      vtUnicodeString:
        begin
          Result.VUnicodeString := Value.VUnicodeString;
          Result.VCharRec.CP := zCP_UTF16;
          Result.VCharRec.Len := Length(Result.VUnicodeString);
          if Result.VCharRec.Len = 0 then
            Result.VCharRec.P := PEmptyUnicodeString //avoid nil result
          else
            Result.VCharRec.P := Pointer(Result.VUnicodeString); //avoid RTL conversion to PWideChar
        end;
      vtCharRec:
        Result.VCharRec := Value.VCharRec;
      else
        RaiseTypeMismatchError;
    end;
  end;

var
  Failed: Boolean;
  CharRec: TZCharRec;
label DateTimeFromRaw, DateTimeFromUnicode;
begin
  Result.VType := NewType;
  case NewType of
    vtBoolean:
      case Value.VType of
        vtNull:
          Result.VBoolean := False;
        vtBoolean:
          Result.VBoolean := Value.VBoolean;
        vtInteger:
          Result.VBoolean := Value.VInteger <> 0;
        vtUInteger:
          Result.VBoolean := Value.VUInteger <> 0;
        vtFloat:
          Result.VBoolean := Value.VFloat <> 0;
        vtBytes:
          Result.VBoolean := Pointer(Value.VBytes) <> nil;
        vtString:
          Result.VBoolean := StrToBoolEx(Value.VString);
        {$IFNDEF NO_ANSISTRING}
        vtAnsiString:
          Result.VBoolean := StrToBoolEx(Value.VAnsiString);
        {$ENDIF}
        {$IFNDEF NO_UTF8STRING}
        vtUTF8String:
          Result.VBoolean := StrToBoolEx(Value.VUTF8String);
        {$ENDIF}
        vtRawByteString:
          Result.VBoolean := StrToBoolEx(Value.VRawByteString);
        vtUnicodeString:
          Result.VBoolean := StrToBoolEx(Value.VUnicodeString);
        vtDateTime:
          Result.VBoolean := Value.VDateTime <> 0;
        vtPointer:
          Result.VBoolean := Value.VPointer <> nil;
        vtInterface:
          Result.VBoolean := Value.VInterface <> nil;
        vtCharRec:
          if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
            Result.VBoolean := StrToBoolEx(PWideChar(Value.VCharRec.P))
          else
            Result.VBoolean := StrToBoolEx(PAnsiChar(Value.VCharRec.P));
        vtArray:
          Result.VBoolean := Value.VArray.VArray <> nil;
        else
          RaiseTypeMismatchError;
      end;
    vtBytes, vtInteger, vtUInteger, vtFloat, vtPointer, vtInterface:
      Result := inherited Convert(Value, NewType);
    vtDateTime:
      case Value.VType of
        vtNull:
          Result.VDateTime := 0;
        vtDateTime:
          Result.VDateTime := Value.VDateTime;
        vtFloat:
          Result.VDateTime := Value.VFloat;
        vtString:
          begin
            CharRec.P := Pointer(Value.VString);
            CharRec.Len := Length(Value.VString);
            goto {$IFDEF UNICODE}DateTimeFromUnicode{$ELSE}DateTimeFromRaw{$ENDIF};
          end;
        {$IFNDEF NO_ANSISTRING}
        vtAnsiString:
          begin
            CharRec.P := Pointer(Value.VAnsiString);
            CharRec.Len := Length(Value.VAnsiString);
          end;
        {$ENDIF}
        {$IFNDEF NO_UTF8STRING}
        vtUTF8String:
          begin
            CharRec.P := Pointer(Value.VUTF8String);
            CharRec.Len := Length(Value.VUTF8String);
            goto DateTimeFromRaw;
          end;
        {$ENDIF}
        vtRawByteString:
          begin
            CharRec.P := Pointer(Value.VRawByteString);
            CharRec.Len := Length(Value.VRawByteString);
DateTimeFromRaw:
            if Ord((PAnsiChar(CharRec.P)+2)^) = Ord(':') then
              Result.VDateTime := RawSQLTimeToDateTime(
                CharRec.P, CharRec.Len, FConSettings^.ReadFormatSettings, Failed)
            else
              if (FConSettings^.ReadFormatSettings.DateTimeFormatLen - CharRec.Len) <= 4 then
                Result.VDateTime := RawSQLTimeStampToDateTime(
                  CharRec.P, CharRec.Len, FConSettings^.ReadFormatSettings, Failed)
              else
                Result.VDateTime := RawSQLTimeToDateTime(
                  CharRec.P, CharRec.Len, FConSettings^.ReadFormatSettings, Failed);
          end;
        vtUnicodeString:
          begin
            CharRec.P := Pointer(Value.VUnicodeString);
            CharRec.Len := Length(Value.VUnicodeString);
DateTimeFromUnicode:
            if (PWideChar(CharRec.P)+2)^ = ':' then
              Result.VDateTime := UnicodeSQLTimeToDateTime(CharRec.P, CharRec.Len,
                FConSettings^.ReadFormatSettings, Failed)
            else
              if (FConSettings^.ReadFormatSettings.DateTimeFormatLen - CharRec.Len) <= 4 then
                Result.VDateTime := UnicodeSQLTimeStampToDateTime(CharRec.P, CharRec.Len,
                  FConSettings^.ReadFormatSettings, Failed)
              else
                Result.VDateTime := UnicodeSQLTimeToDateTime(CharRec.P, CharRec.Len,
                  FConSettings^.ReadFormatSettings, Failed);
          end;
        vtCharRec:
          begin
            CharRec := Value.VCharRec;
            if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
              goto DateTimeFromUnicode
            else
              goto DateTimeFromRaw;
          end
        else
          RaiseTypeMismatchError;
      end;

    vtString:
      ProcessString(Value, Result);
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString:
      ProcessAnsiString(Value, Result);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String:
      ProcessUTF8String(Value, Result);
    {$ENDIF}

    vtRawByteString:
      ProcessRawByteString(Value, Result);

    vtUnicodeString:
      ProcessUnicodeString(Value, Result);

    vtCharRec:
      ProcessCharRec(Value, Result);
  end;
end;

function TZClientVariantManager.GetAsRawByteString(const Value: TZVariant;
  const RawCP: Word): RawByteString;
var
  US: ZWideString;
begin
  case Value.VType of
    vtNull:
      Result := EmptyRaw;
    vtBoolean:
      Result := BoolStrsUpRaw[Value.VBoolean];
    vtBytes:
      ZSetString(Pointer(Value.VBytes), Length(Value.VBytes), Result);
    vtInteger:
      Result := IntToRaw(Value.VInteger);
    vtUInteger:
      Result := IntToRaw(Value.VUInteger);
    vtFloat:
      Result := FloatToSqlRaw(Value.VFloat);
    vtString:
      Result := fConSettings.ConvFuncs.ZStringToRaw(Value.VString, FConSettings^.CTRL_CP, RawCP);
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString:
      if ZCompatibleCodePages(ZOSCodePage, RawCP) then
        Result := ZMoveAnsiToRaw(Value.VAnsiString, RawCP)
      else
        Result := ZConvertAnsiToRaw(Value.VAnsiString, RawCP);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String:
      if ZCompatibleCodePages(zCP_UTF8, RawCP) then
        Result := ZMoveUTF8ToRaw(Value.VUTF8String, RawCP)
      else
        Result := ZConvertUTF8ToRaw(Value.VUTF8String, RawCP);
    {$ENDIF}
    vtRawByteString:
      if ZCompatibleCodePages(FConSettings^.ClientCodePage^.CP, RawCP) then
        Result := Value.VRawByteString
      else
      begin
        US := ZRawToUnicode(Value.VRawByteString, FConSettings^.ClientCodePage^.CP);
        Result := ZUnicodeToRaw(US, RawCP);
      end;
    vtUnicodeString:
      Result := ZUnicodeToRaw(Value.VUnicodeString, RawCP);
    vtCharRec:
      if ZCompatibleCodePages(Value.VCharRec.CP, zCP_UTF16) then
        Result := PUnicodeToRaw(Value.VCharRec.P, Value.VCharRec.Len, RawCP)
      else
        if ZCompatibleCodePages(RawCP, Value.VCharRec.CP) then
          ZSetString(Value.VCharRec.P, Value.VCharRec.Len, Result)
        else
        begin
          US := PRawToUnicode(Value.VCharRec.P, Value.VCharRec.Len, Value.VCharRec.CP);
          Result := ZUnicodeToRaw(US, RawCP);
        end;
    vtDateTime:
      Result := ZSysUtils.DateTimeToRawSQLTimeStamp(Value.VDateTime, FConSettings^.WriteFormatSettings, False);
    else
      RaiseTypeMismatchError;
  end;
end;

function TZClientVariantManager.GetAsCharRec(var Value: TZVariant; const CodePage: Word): TZCharRec;
Label AsRBS;
begin
  Result.CP := CodePage;
  case Value.VType of
    vtNull:
      begin
        Result.P := nil;
        Result.Len := 0;
      end;
    vtCharRec:
      if ZCompatibleCodePages(CodePage, Value.VCharRec.CP) then
        Result := Value.VCharRec
      else
        if ZCompatibleCodePages(CodePage, zCP_UTF16) then
        begin
          Value.VUnicodeString := Convert(Value, vtUnicodeString).VUnicodeString;
          Result.Len := Length(Value.VUnicodeString);
          if Result.Len = 0 then
            Result.P := PEmptyUnicodeString //Pointer Result would be nil
          else
            Result.P := Pointer(Value.VUnicodeString); //avoid RTL conversion to PWideChar
        end
        else
          goto AsRBS;
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String:
      if CodePage = zCP_UTF8 then
      begin
        if Pointer(Value.VUTF8String) = nil then
        begin
          Result.P := PEmptyAnsiString; //Pointer Result would be nil
          Result.Len := 0;
        end
        else
        begin
          Result.Len := {%H-}PLengthInt(NativeUInt(Value.VUTF8String) - StringLenOffSet)^; //fast Length() helper for D7..D9 where Length() isn't inlined;
          Result.P := Pointer(Value.VUTF8String); //avoid RTL conversion to PAnsiChar
        end;
      end
      else
        if CodePage = zCP_UTF16 then
        begin
          Value.VUnicodeString := PRawToUnicode(Pointer(Value.VUTF8String), Length(Value.VUTF8String), zCP_UTF8);
          Result.Len := Length(Value.VUnicodeString);
          if Result.Len = 0 then
            Result.P := PEmptyUnicodeString //Pointer Result would be nil
          else
            Result.P := Pointer(Value.VUnicodeString); //avoid RTL conversion to PWideChar
        end
        else
        goto AsRBS;
    {$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString:
      if CodePage = zCP_UTF16 then
      begin
        Value.VUnicodeString := PRawToUnicode(Pointer(Value.VAnsiString), Length(Value.VAnsiString), ZOSCodePage);
        Result.Len := Length(Value.VUnicodeString);
        if Result.Len = 0 then
          Result.P := PEmptyUnicodeString //Pointer Result would be nil
        else
          Result.P := Pointer(Value.VUnicodeString); //avoid RTL conversion to PWideChar
      end
      else goto AsRBS;
    {$ENDIF}
    vtRawByteString:
      if CodePage = zCP_UTF16 then
      begin
        Value.VUnicodeString := Convert(Value, vtUnicodeString).VUnicodeString;
        Result.Len := Length(Value.VUnicodeString);
        if Result.Len = 0 then
          Result.P := PEmptyUnicodeString //Pointer Result would be nil
        else
          Result.P := Pointer(Value.VUnicodeString); //avoid RTL conversion to PWideChar
      end
      else goto AsRBS;
    vtString:
    {$IFDEF UNICODE}
      if CodePage = zCP_UTF16 then
      begin
        Result.Len := Length(Value.VString);
        if Result.Len = 0 then
          Result.P := PEmptyUnicodeString //Pointer Result would be nil
        else
          Result.P := Pointer(Value.VString); //avoid RTL conversion to PWideChar
    {$ELSE}
      if CodePage = zCP_UTF16 then
      begin
        Value.VUnicodeString := Convert(Value, vtUnicodeString).VUnicodeString;
        Result.Len := Length(Value.VUnicodeString);
        if Result.Len = 0 then
          Result.P := PEmptyUnicodeString //Pointer Result would be nil
        else
          Result.P := Pointer(Value.VUnicodeString); //avoid RTL conversion to PWideChar
    {$ENDIF}
      end
      else goto AsRBS;
    vtUnicodeString:
      if CodePage = zCP_UTF16 then
      begin
        Result.Len := Length(Value.VUnicodeString);
        if Result.Len = 0 then
          Result.P := PEmptyUnicodeString //Pointer Result would be nil
        else
          Result.P := Pointer(Value.VUnicodeString); //avoid RTL conversion to PWideChar
      end
      else goto AsRBS;
    else
AsRBS:
      begin
        Value.VRawByteString := GetAsRawByteString(Value, CodePage);
        if Pointer(Value.VRawByteString) = nil then
        begin
          Result.P := PEmptyAnsiString; //Pointer Result would be nil
          Result.Len := 0;
        end
        else
        begin
          Result.Len := {%H-}PLengthInt(NativeUInt(Value.VRawByteString) - StringLenOffSet)^; //fast Length() helper for D7..D9 where Length() isn't inlined;
          Result.P := Pointer(Value.VRawByteString); //avoid RTL conversion to PAnsiChar
        end;
      end;
  end;
end;

{ TZAnyValue }

{**
  Constructs this object and assigns the main properties.
  @param Value an any value.
}
constructor TZAnyValue.Create(const Value: TZVariant);
begin
  FValue := Value;
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a boolean value.
}
constructor TZAnyValue.CreateWithBoolean(Value: Boolean);
begin
  FValue := EncodeBoolean(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a datetime value.
}
constructor TZAnyValue.CreateWithDateTime(const Value: TDateTime);
begin
  FValue := EncodeDateTime(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a float value.
}
constructor TZAnyValue.CreateWithFloat(const Value: Extended);
begin
  FValue := EncodeFloat(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a integer value.
}
constructor TZAnyValue.CreateWithInteger(const Value: Int64);
begin
  FValue := EncodeInteger(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a string value.
}
constructor TZAnyValue.CreateWithString(const Value: String);
begin
  FValue := EncodeString(Value);
end;

{**
  Constructs this object and assigns the main properties.
  @param Value a unicode string value.
}
{$IFDEF UNICODE}
constructor TZAnyValue.CreateWithUnicodeString(const Value: String; unicodeType : Boolean = true);
{$ELSE}
constructor TZAnyValue.CreateWithUnicodeString(const Value: WideString);
{$ENDIF}
begin
  FValue := EncodeUnicodeString(Value);
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZAnyValue.Clone: IZInterface;
begin
  Result := TZAnyValue.Create(FValue);
end;

{**
  Compares this and another property.
  @return <code>True</code> is properties are equal.
}
function TZAnyValue.Equals(const Value: IZInterface): Boolean;
var
  Temp: IZAnyValue;
begin
  if Value <> nil then
  begin
    if Value.QueryInterface(IZAnyValue, Temp) = 0 then
    begin
      Result := SoftVarManager.Compare(FValue, Temp.GetValue) = 0;
      Temp := nil;
    end
    else
      Result := inherited Equals(Value);
  end
  else
    Result := False;
end;

{**
  Gets a stored any value.
  @return a stored any value.
}
function TZAnyValue.GetValue: TZVariant;
begin
  Result := FValue;
end;

{**
  Converts this object into the string representation.
  @return a string representation for this object.
}
function TZAnyValue.ToString: string;
begin
  Result := GetString;
end;

{**
  Checks is the stored value contains NULL.
  @returns <code>True</code> if NULL is stored.
}
function TZAnyValue.IsNull: Boolean;
begin
  Result := SoftVarManager.IsNull(FValue);
end;

{**
  Gets a stored value converted to double.
  @return a stored value converted to double.
}
function TZAnyValue.GetFloat: Extended;
begin
  Result := SoftVarManager.GetAsFloat(FValue);
end;

{**
  Gets a stored value converted to integer.
  @return a stored value converted to integer.
}
function TZAnyValue.GetInteger: Int64;
begin
  Result := SoftVarManager.GetAsInteger(FValue);
end;

{**
  Gets a stored value converted to String.
  @return a stored value converted to string.
}
function TZAnyValue.GetString: String;
begin
  Result := SoftVarManager.GetAsString(FValue);
end;

{**
  Gets a stored value converted to AnsiString.
  @return a stored value converted to string.
}
{$IFNDEF NO_ANSISTRING}
function TZAnyValue.GetAnsiString: AnsiString;
begin
  Result := SoftVarManager.GetAsAnsiString(FValue);
end;
{$ENDIF}

{**
  Gets a stored value converted to AnsiString.
  @return a stored value converted to string.
}
{$IFNDEF NO_UTF8STRING}
function TZAnyValue.GetUTF8String: UTF8String;
begin
  Result := SoftVarManager.GetAsUTF8String(FValue);
end;
{$ENDIF}

{**
  Gets a stored value converted to boolean.
  @return a stored value converted to boolean.
}
function TZAnyValue.GetBoolean: Boolean;
begin
  Result := SoftVarManager.GetAsBoolean(FValue);
end;

{**
  Gets a stored value converted to byte array.
  @return a stored value converted to a byte array.
}
function TZAnyValue.GetBytes: TBytes;
begin
  Result := SoftVarManager.GetAsBytes(FValue);
end;

{**
  Gets a stored value converted to unicode string.
  @return a stored value converted to unicode string.
}
function TZAnyValue.GetUnicodeString: ZWideString;
begin
  Result := SoftVarManager.GetAsUnicodeString(FValue);
end;

{**
  Gets a stored value converted to datetime.
  @return a stored value converted to datetime.
}
function TZAnyValue.GetDateTime: TDateTime;
begin
  Result := SoftVarManager.GetAsDateTime(FValue);
end;

{**
  Encodes a custom variant value into standard variant.
  @param Value a custom variant value to be encoded.
  @returns an encoded standard variant.
}
function EncodeVariant(const Value: TZVariant): Variant;
begin
  case Value.VType of
    vtBoolean: Result := Value.VBoolean;
    vtBytes: Result := BytesToVar(Value.VBytes);
    vtInteger: if (Value.VInteger > -MaxInt) and (Value.VInteger < MaxInt)
      then Result := Integer(Value.VInteger)
      else Result := Value.VInteger;
    vtUInteger: if Int64Rec(Value.VUInteger).Hi = 0
      then Result := Int64Rec(Value.VUInteger).Lo
      else {$IFDEF WITH_VARIANT_UINT64}
            Result := Value.VUInteger;
          {$ELSE}
            Result := ZFastCode.IntToStr(Value.VUInteger);
          {$ENDIF}
    vtFloat: Result := Value.VFloat;
    vtString: Result := Value.VString;
    {$IFNDEF NO_ANSISTRING}
    vtAnsiString: Result := Value.VAnsiString;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    vtUTF8String: Result := Value.VUTF8String;
    {$ENDIF}
    vtRawByteString: Result := Value.VRawByteString;
    vtUnicodeString: Result := Value.VUnicodeString;
    vtDateTime: Result := Value.VDateTime;
    vtPointer:
    {$ifdef fpc}
        Result := {%H-}NativeUInt(Value.VPointer);
    {$else}
        Result := NativeUInt(Value.VPointer);
    {$endif}
    vtInterface: Result := Value.VInterface;
  else
    Result := Null;
  end;
end;

{**
  Encodes an array of custom variant values into array of standard variants.
  @param Value an array of custom variant values to be encoded.
  @returns an encoded array of standard variants.
}
function EncodeVariantArray(const Value: TZVariantDynArray): Variant;
var
  I, L: Integer;
begin
  L := Length(Value);
  Result := VarArrayCreate([0, L - 1], varVariant);
  for I := 0 to L - 1 do
    Result[I] := EncodeVariant(Value[I]);
end;

{**
  Decodes a standard variant value into custom variant.
  @param Value a standard variant value to be decoded.
  @returns an decoded custom variant.
}
function DecodeVariant(const Value: Variant): TZVariant;
begin
  case VarType(Value) of
    varSmallint, varInteger, varByte:
      Result := EncodeInteger(Integer(Value));
    varBoolean: Result := EncodeBoolean(Value);
    varString: Result := EncodeString(Value);
   {$IFDEF UNICODE}
   varUString: Result := EncodeUnicodeString(Value);
   {$ENDIF}
    varSingle, varDouble, varCurrency:
      Result := EncodeFloat(Value);
    varUnknown: Result := EncodeInterface(Value);
    varOleStr:
      Result := EncodeUnicodeString(Value);
    varDate: Result := EncodeDateTime(Value);
    varShortInt, varWord, varLongWord:
      Result := EncodeInteger(Value);
    varInt64:
      Result := EncodeInteger(Value);
    {$IFDEF WITH_VARIANT_UINT64}
    varUInt64: Result := EncodeUInteger(Value);
    {$ENDIF}
  else
    Result := EncodeNull;
  end;
end;

{**
  Decodes an array of standard variant values into array of custom variants.
  @param Value an array of standard variant values to be decoded.
  @returns an decoded array of custom variants.
}
function DecodeVariantArray(const Value: Variant): TZVariantDynArray;
var
  I, L, H: Integer;
begin
  if VarIsArray(Value) then
  begin
    L := VarArrayLowBound(Value, 1);
    H := VarArrayHighBound(Value, 1);
    SetLength(Result, H - L + 1);
    for I := L to H do
      Result[I - L] := DecodeVariant(Value[I]);
  end
  else
  begin
    SetLength(Result, 1);
    Result[0] := DecodeVariant(Value);
  end;
end;

{**
  Creates a null variant.
}
function EncodeNull: TZVariant;
begin
  Result.VType := vtNull;
end;

{**
  Creates a boolean variant.
  @param Value a value to be assigned.
}
function EncodeBoolean(const Value: Boolean): TZVariant;
begin
  Result.VType := vtBoolean;
  Result.VBoolean := Value;
end;

{**
  Creates a bytes array variant.
  @param Value a value to be assigned.
}
function EncodeBytes(const Value: TBytes): TZVariant;
begin
  Result.VType := vtBytes;
  Result.VBytes := Value;
end;

{**
  Creates a bytes array variant from a GUID.
  @param Value a value to be assigned.
}
function EncodeGUID(const Value: TGUID): TZVariant;
begin
  Result.VType := vtBytes;
  SetLength(Result.VBytes, SizeOf(TGUID));
  PGUID(Result.VBytes)^ := Value;
end;

{**
  Creates an integer variant.
  @param Value a value to be assigned.
}
function EncodeInteger(const Value: Int64): TZVariant;
begin
  Result.VType := vtInteger;
  Result.VInteger := Value;
end;

{**
  Creates an unsigned integer variant.
  @param Value a value to be assigned.
}
function EncodeUInteger(const Value: UInt64): TZVariant;
begin
  Result.VType := vtUInteger;
  Result.VUInteger := Value;
end;

{**
  Creates a float variant.
  @param Value a value to be assigned.
}
function EncodeFloat(const Value: Extended): TZVariant;
begin
  Result.VType := vtFloat;
  Result.VFloat := Value;
end;

{**
  Creates a AnsiString variant.
  @param Value a value to be assigned.
}
function EncodeString(const Value: String): TZVariant;
begin
  Result.VType := vtString;
  Result.VString := Value;
end;

{**
  Creates a AnsiString variant.
  @param Value a value to be assigned.
}
{$IFNDEF NO_ANSISTRING}
function EncodeAnsiString(const Value: AnsiString): TZVariant;
begin
  Result.VType := vtAnsiString;
  Result.VAnsiString := Value;
end;
{$ENDIF}

{**
  Creates a UTF8String variant.
  @param Value a value to be assigned.
}
{$IFNDEF NO_UTF8STRING}
function EncodeUTF8String(const Value: UTF8String): TZVariant;
begin
  Result.VType := vtUTF8String;
  Result.VUTF8String := Value;
end;
{$ENDIF}

{**
  Creates a RawByteString variant.
  @param Value a value to be assigned.
}
function EncodeRawByteString(const Value: RawByteString): TZVariant;
begin
  Result.VType := vtRawByteString;
  Result.VRawByteString := Value;
end;

{**
  Creates a TZCharRec variant.
  @param Value a value to be assigned.
}
function EncodeCharRec(const Value: TZCharRec): TZVariant;
begin
  Result.VType := vtCharRec;
  Result.VCharRec := Value;
end;

{**
  Creates a UnicodeString variant.
  @param Value a value to be assigned.
}
function EncodeUnicodeString(const Value: ZWideString): TZVariant;
begin
  Result.VType := vtUnicodeString;
  Result.VUnicodeString := Value;
end;

{**
  Creates a TDateTime variant.
  @param Value a value to be assigned.
}
function EncodeDateTime(const Value: TDateTime): TZVariant;
begin
  Result.VType := vtDateTime;
  Result.VDateTime := Value;
end;

{**
  Creates a pointer variant.
  @param Value a value to be assigned.
}
function EncodePointer(const Value: Pointer): TZVariant;
begin
  Result.VType := vtPointer;
  Result.VPointer := Value;
end;

{**
  Creates an Interface variant.
  @param Value a value to be assigned.
}
function EncodeInterface(const Value: IZInterface): TZVariant;
begin
  Result.VType := vtInterface;
  Result.VInterface := Value;
end;

{**
  Creates an TZArray variant.
  @param Value a value to be assigned.
}
function EncodeArray(const Value: TZArray): TZVariant;
begin
  Result.VType := vtArray;
  Result.VArray := Value;
end;

{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
procedure RawFiller;
var B: Boolean;
begin
  for B := False to True do
    BoolStrsUpRaw[b] := UnicodeStringToASCII7(BoolStrsUpW[b]);
end;
{$ENDIF}

initialization
{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  RawFiller;
{$ENDIF}

  {$IFDEF ZEOS_TEST_ONLY}
  DefVarManager  := TZDefaultVariantManager.Create;
  {$ENDIF ZEOS_TEST_ONLY}
  SoftVarManager := TZSoftVariantManager.Create;
  NullVariant    := EncodeNull;

finalization
  {$IFDEF ZEOS_TEST_ONLY}
  DefVarManager  := nil;
  {$ENDIF ZEOS_TEST_ONLY}
  SoftVarManager := nil;
end.
