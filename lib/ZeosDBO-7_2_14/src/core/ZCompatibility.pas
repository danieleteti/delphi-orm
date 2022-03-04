{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Compatibility Classes and Functions          }
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

unit ZCompatibility;

interface

{$I ZCore.inc}

uses
{$IFDEF FPC}
  {$IFDEF UNIX}
    dynlibs,
  {$endif}
{$ENDIF}
  Classes,
  {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFDEF WITH_LCONVENCODING} LConvEncoding,{$ENDIF}
  Types,
  SysUtils;

type
  {$IF not declared(UInt64)}
  UInt64                = QWord;
  {$IFEND}
  {$IF not declared(PUInt64)}
  PUInt64               = {$IFDEF FPC}PQWord{$ELSE}^UInt64{$ENDIF};
  {$IFEND}
  {$IF not declared(PPLongWord)}
  PPLongWord            = ^PLongWord;
  {$IFEND}
{$IFDEF FPC}
  {$IF not declared(NativeInt)} //since FPC2.7 this type is declared too avoid inconsitent builds
  NativeInt             = PtrInt;
  {$IFEND}
  {$IF not declared(NativeUInt)} //since FPC2.7 this type is declared too avoid inconsitent builds
  NativeUInt            = PtrUInt;
  {$IFEND}
  {$IF not declared(PNativeUInt)} //since FPC2.7 this type is declared too avoid inconsitent builds
  PNativeUInt           = ^NativeUInt;
  {$IFEND}
{$ELSE}
  {$IFNDEF HAVE_TRUE_NATIVE_TYPES}  //introduced since D2007 but "stable" since XE2
  NativeInt             = Integer;
  NativeUInt            = LongWord;
  PNativeUInt           = ^NativeUInt;
  PWord                 = ^Word; // M.A.
  {$ENDIF}
{$ENDIF}

  UInt                  = LongWord;
  PUInt                 = ^UInt;
  ZPPWideChar           = ^PWideChar;//BCB issue: PPWideChar is not part of system

  {EH: just a clear type/IDE to get the length of String reading back from
    initial entry(X) - SizeOf(LengthInt) = Length}

  PLengthInt            = ^LengthInt;
  LengthInt             = {$IFDEF FPC}SizeInt{$ELSE}Integer{$ENDIF};
  PRefCntInt            = ^RefCntInt;
  RefCntInt             = {$IFDEF FPC}SizeInt{$ELSE}Integer{$ENDIF};
  {EH: just two types for determination DynArray Length if ever something changes we just need a define here.}
  ArrayLenInt           = NativeInt;
  PArrayLenInt          = ^ArrayLenInt;

  {$IF not declared(AnsiChar)}
  AnsiChar = Byte;
  PAnsiChar = MarshaledAString;
  PPAnsiChar = ^PAnsiChar;
  {$IFEND}
  {$IF not declared(PInt64Rec)}
  PInt64Rec = ^Int64Rec;
  {$IFEND}
{$IFDEF FPC}
{$IFDEF WITH_RAWBYTESTRING}
Type
  PAnsiRec = ^TAnsiRec;
  TAnsiRec = Record
    CodePage    : TSystemCodePage;
    ElementSize : Word;
{$ifdef CPU64}
    { align fields  }
    Dummy       : DWord;
{$endif CPU64}
    Ref         : SizeInt;
    Len         : SizeInt;
  end;
  {$ENDIF}
  {@-16 : Code page indicator.
  @-12 : Character size (2 bytes)
  @-8  : SizeInt for reference count;
  @-4  : SizeInt for size;
  @    : String + Terminating #0;
  Pchar(Ansistring) is a valid typecast.
  So AS[i] is converted to the address @AS+i-1.}
const
  StringLenOffSet             = SizeOf(SizeInt){PAnsiRec/PUnicodeRec.Len};
  StringRefCntOffSet          = SizeOf(SizeInt){PAnsiRec/PUnicodeRec.Ref}+SizeOf(SizeInt){PAnsiRec/PUnicodeRec.Len};
  {$IFDEF WITH_RAWBYTESTRING}
  AnsiFirstOff                = SizeOf(TAnsiRec);
  {$ENDIF}
  {$ELSE} //system.pas
const
  StringLenOffSet             = SizeOf(Integer); {PStrRec.Len}
  StringRefCntOffSet          = SizeOf(Integer){PStrRec.RefCnt}+SizeOf(Integer){PStrRec.Len};
  CodePageOffSet              = SizeOf(Integer){PAnsiRec/PUnicodeRec.Ref}+SizeOf(Integer){PAnsiRec/PUnicodeRec.Len}+
                                  SizeOf(Word){elementsize}+SizeOf(Word){codePage};  //=12
  {$ENDIF}
  ArrayLenOffSet              = SizeOf(ArrayLenInt);

  {$IF NOT DECLARED(SecsPerHour)}
  SecsPerHour = SecsPerMin * MinsPerHour;
  {$IFEND}
  FirstStringIndex = {$IFDEF ZERO_BASED_STRINGS}0{$ELSE}1{$ENDIF}; //Str[i] fe.

type
  PZCharRec = ^TZCharRec;
  TZCharRec = Record
    Len: Cardinal; //Length of String
    P: Pointer;    //Allocated Mem of String including #0 terminator
    CP: Word;      //CodePage of the String
  end;

  {$IF NOT DECLARED(TBytes)}
  TBytes = TByteDynArray;
  {$IFEND}

  TObjectDynArray       = array of TObject;

{$IFDEF FPC}
type
  TDBScreenCursor = (dcrDefault, dcrHourGlass, dcrSQLWait, dcrOther);

  IDBScreen = interface
    ['{29A1C508-6ADC-44CD-88DE-4F51B25D5995}']
    function GetCursor: TDBScreenCursor;
    procedure SetCursor(Cursor: TDBScreenCursor);

    property Cursor: TDBScreenCursor read GetCursor write SetCursor;
  end;

var
  LoginDialogProc: function (const ADatabaseName: string; var AUserName,
    APassword: string): Boolean;
  DBScreen: IDBScreen;
{$ENDIF}

const
  DefDateFormatDMY = 'DD-MM-YYYY';
  DefDateFormatYMD = 'YYYY-MM-DD';
  DefDateFormatMDY = 'MM/DD/YYYY';
  DefTimeFormat = 'HH:NN:SS';
  DefTimeFormatMsecs = 'HH:NN:SS.ZZZ';
  DefDateTimeFormat = DefDateFormatDMY + ' ' + DefTimeFormat;
  DefDateTimeFormatMsecs = DefDateFormatDMY + ' ' + DefTimeFormatMsecs;
  DefDateTimeFormatMsecsMDY = DefDateFormatYMD + ' ' + DefTimeFormatMsecs;

{$IF NOT DECLARED(LineEnding)} // FPC-style constant, declare for Delphi
const
  LineEnding = sLineBreak;
{$IFEND}

{$IF NOT DECLARED(AnsiProperCase)} // FPC has this function in RTL
{$DEFINE ZAnsiProperCase}
const
  Brackets = ['(',')','[',']','{','}'];
  StdWordDelims = [#0..' ',',','.',';','/','\',':','''','"','`'] + Brackets;

function AnsiProperCase(const S: string; const WordDelims: {$IFDEF WITH_TSYSCHARSET_DEPRECATED} String {$ELSE} TSysCharSet {$ENDIF}): string;
{$IFEND}

{$IFDEF WINDOWS}
const SharedSuffix='.dll';
{$ELSE}
  {$IFDEF DARWIN}
  const SharedSuffix='.dylib';
  {$ELSE}
    {$IFDEF UNIX}
      const SharedSuffix='.so';
    {$ELSE}
      const SharedSuffix='.dll'; //Delphi
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF FPC}
const
  RTLD_GLOBAL = $101;
  INVALID_HANDLE_VALUE = 0;

type
  HMODULE = PtrInt;

function LoadLibrary(ModuleName: PChar): HMODULE;
function FreeLibrary(Module: HMODULE): LongBool;
function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
  {$ENDIF}
{$ENDIF}

type
  {$IFNDEF WITH_RAWBYTESTRING}
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    RawByteString = TBytes;
    {$ELSE}
    RawByteString = AnsiString;
    {$ENDIF}
  {$ENDIF}

  ZWideString = {$IFDEF PWIDECHAR_IS_PUNICODECHAR}UnicodeString{$ELSE}WideString{$ENDIF};

  {$IF not declared(TBooleanDynArray)}
  TBooleanDynArray        = array of Boolean;
  {$IFEND}
  {$IF not declared(PBooleanDynArray)}
  PBooleanDynArray        = ^TBooleanDynArray;
  {$IFEND}
  {$IF not declared(TByteDynArray)}
  TByteDynArray           = array of Byte;
  {$IFEND}
  {$IF not declared(TShortIntDynArray)}
  TShortIntDynArray       = array of ShortInt;
  {$IFEND}
  {$IF not declared(TWordDynArray)}
  TWordDynArray           = array of Word;
  {$IFEND}
  {$IF not declared(TSmallIntDynArray)}
  TSmallIntDynArray       = array of SmallInt;
  {$IFEND}
  {$IF not declared(TLongWordDynArray)}
  TLongWordDynArray       = array of LongWord;
  {$IFEND}
  {$IF not declared(TIntegerDynArray)}
  TIntegerDynArray        = array of Integer;
  {$IFEND}
  {$IF not declared(TCardinalDynArray)}
  TCardinalDynArray       = array of Cardinal;
  {$IFEND}
  {$IF not declared(TUInt64DynArray)}
  TUInt64DynArray         = array of UInt64;
  {$IFEND}
  {$IF not declared(TInt64DynArray)}
  TInt64DynArray          = array of Int64;
  {$IFEND}
  {$IF not declared(TSingleDynArray)}
  TSingleDynArray         = array of Single;
  {$IFEND}
  {$IF not declared(TDoubleDynArray)}
  TDoubleDynArray         = array of Double;
  {$IFEND}
  {$IF not declared(TCurrencyDynArray)}
  TCurrencyDynArray       = array of Currency;
  {$IFEND}
  {$IF not declared(TExtendedDynArray)}
  TExtendedDynArray       = array of Extended;
  {$IFEND}
  {$IF not declared(TDateTimeDynArray)}
  TDateTimeDynArray       = array of TDateTime;
  {$IFEND}
  {$IF not declared(TUTF8StringDynArray) and not defined(NO_UTF8STRING)}
  TUTF8StringDynArray     = array of UTF8String;
  {$IFEND}
  {$IF not declared(TAnsiStringDynArray) and not defined(NO_ANSISTRING)}
  TAnsiStringDynArray     = array of AnsiString;
  {$IFEND}
  {$IF not declared(TRawByteStringDynArray)}
  TRawByteStringDynArray  = array of RawByteString;
  {$IFEND}
  {$IF not declared(TUnicodeStringDynArray)}
  TUnicodeStringDynArray  = array of ZWideString;
  {$IFEND}
  {$IF not declared(TStringDynArray)}
  TStringDynArray  = array of String;
  {$IFEND}
  {$IF not declared(TBytesDynArray)}
  TBytesDynArray  = array of TBytes;
  {$IFEND}
  {$IF not declared(TInterfaceDynArray)}
  TInterfaceDynArray  = array of IInterface;
  {$IFEND}
  {$IF not declared(TGUIDDynArray)}
  TGUIDDynArray  = array of TGUID;
  {$IFEND}
  {$IF not declared(TPointerDynArray)}
  TPointerDynArray  = array of Pointer;
  {$IFEND}
  TZCharRecDynArray = array of TZCharRec;

type
  {declare move or converter functions for the String Types}
  {$IFNDEF NO_ANSISTRING}
  TZAnsiToRaw = function (const Src: AnsiString; const RawCP: Word): RawByteString;
  TZRawToAnsi = function (const Src: RawByteString; const RawCP: Word): AnsiString;
  TZAnsiToUTF8 = function (const Src: AnsiString): UTF8String;
  TZUTF8ToAnsi = function (const Src: UTF8String): AnsiString;
  TZAnsiToString = function (const Src: AnsiString; const StringCP: Word): String;
  TZStringToAnsi = function (const Src: String; const StringCP: Word): AnsiString;
  {$ENDIF}
  {$IFNDEF NO_UTF8STRING}
  TPRawToUTF8 = function(const Src: PAnsiChar; Len: NativeUInt; const RawCP: Word): UTF8String;
  TZRawToUTF8 = function (const Src: RawByteString; const CP: Word): UTF8String;
  TZUTF8ToRaw = function (const Src: UTF8String; const CP: Word): RawByteString;
  TZUTF8ToString = function (const Src: UTF8String; const StringCP: Word): String;
  TZStringToUTF8 = function (const Src: String; const StringCP: Word): UTF8String;
  {$ENDIF}
  TZRawToString = function (const Src: RawByteString; const RawCP, StringCP: Word): String;
  TZStringToRaw = function (const Src: String; const StringCP, RawCP: Word): RawByteString;
  TZRawToUnicode = function (const S: RawByteString; const CP: Word): ZWideString;
  TZUnicodeToRaw = function (const US: ZWideString; CP: Word): RawByteString;
  TZUnicodeToString = function (const Src: ZWideString; const StringCP: Word): String;
  TZStringToUnicode = function (const Src: String; const StringCP: Word): ZWideString;
  TPRawToString = function (Src: PAnsiChar; Len: LengthInt; const RawCP, StringCP: Word): String;
  TPUnicodeToString = function (Src: PWideChar; CodePoints: NativeUInt; const StringCP: Word): String;

  {** Defines the Target Ansi codepages for the Controls }
  TZControlsCodePage = ({$IFDEF UNICODE}cCP_UTF16, cCP_UTF8, cGET_ACP{$ELSE}{$IFDEF FPC}cCP_UTF8, cCP_UTF16, cGET_ACP{$ELSE}cGET_ACP, cCP_UTF8, cCP_UTF16{$ENDIF}{$ENDIF});

  TZCharEncoding = (
    ceDefault,  //Internal switch for the two Functions below do not use it as a CodePage-declaration!
    ceAnsi,     //Base Ansi-String: prefered CodePage
    ceUTF8,     //UTF8_Unicode: 1-4Byte/Char
    ceUTF16,    //Wide or Unicode string encoding for Field-mapping
    ceUTF32);   //reserved

    {Here it's possible to add some more, to handle the Ansi->Unicode-Translations}

  PZCodePage = ^TZCodePage;
  TZCodePage = record
    Name: String;             //Name of Client-CharacterSet
    ID:  Integer;             //may be an ordinal value of predefined Types or the database used id}
    CharWidth: Integer;       //count of Bytes per char
    Encoding: TZCharEncoding; //The Type of String-Translation handling
    CP:  Word;                //The CodePage the AnsiString must have to
    ZAlias: String;           //A possible (saver?) CharacterSet which is more Zeos compatible... If it's empty it will be ignored!!!
    IsStringFieldCPConsistent: Boolean; //Is the current client characterset codepage consistent for all codepages?
  end;

  TConvertEncodingFunctions = record
    {$IFNDEF NO_ANSISTRING}
    ZAnsiToUTF8: TZAnsiToUTF8;
    ZUTF8ToAnsi: TZUTF8ToAnsi;
    ZRawToAnsi: TZRawToAnsi;
    ZAnsiToString: TZAnsiToString;
    ZStringToAnsi: TZStringToAnsi;
    ZAnsiToRaw: TZAnsiToRaw;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    ZUTF8ToString: TZUTF8ToString;
    ZStringToUTF8: TZStringToUTF8;
    ZRawToUTF8: TZRawToUTF8;
    ZUTF8ToRaw: TZUTF8ToRaw;
    ZPRawToUTF8: TPRawToUTF8;
    {$ENDIF}
    ZStringToRaw: TZStringToRaw;
    ZRawToString: TZRawToString;
    ZUnicodeToRaw: TZUnicodeToRaw;
    ZRawToUnicode: TZRawToUnicode;
    ZUnicodeToString: TZUnicodeToString;
    ZStringToUnicode: TZStringToUnicode;
    ZPRawToString: TPRawToString;
    ZPUnicodeToString: TPUnicodeToString;
  end;

  TZFormatSettings = Record
    DateFormat: String;
    DateFormatLen: Byte;
    TimeFormat: String;
    TimeFormatLen: Byte;
    DateTimeFormat: String;
    DateTimeFormatLen: Byte;
  End;

  PZConSettings = ^TZConSettings;
  TZConSettings = record
    AutoEncode: Boolean;        //Check Encoding and or convert string with FromCP ToCP
    CPType: TZControlsCodePage; //the CP-Settings type the controls do expect
    CTRL_CP: Word;              //Target CP of string conversion (CP_ACP/CP_UPF8)
    ConvFuncs: TConvertEncodingFunctions; //a rec for the Convert functions used by the objects
    ClientCodePage: PZCodePage; //The codepage informations of the current characterset
    DisplayFormatSettings: TZFormatSettings;
    ReadFormatSettings: TZFormatSettings;
    WriteFormatSettings: TZFormatSettings;
    {$IFDEF WITH_LCONVENCODING}
    PlainConvertFunc: TConvertEncodingFunction;
    DbcConvertFunc: TConvertEncodingFunction;
    {$ENDIF}
    DataBaseSettings: Pointer;
    Protocol, Database, User: RawByteString;
  end;

  TZCodePagedObject = Class(TInterfacedObject)
  private
    FConSettings: PZConSettings;
  protected
    procedure SetConSettingsFromInfo(Info: TStrings);
    property ConSettings: PZConSettings read FConSettings write FConSettings;
  public
    function GetConSettings: PZConSettings;
  end;

  {$IFDEF WITH_LCONVENCODING}
  function NoConvert(const s: string): string;
  {$ENDIF}


{$IFNDEF WITH_CHARINSET}
function CharInSet(const C: AnsiChar; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
function CharInSet(const C: WideChar; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
function CharInSet(const C: Word; const CharSet: TSysCharSet): Boolean; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$ENDIF}

{$IF not Declared(UTF8ToString)}
{$DEFINE ZUTF8ToString}
function UTF8ToString(const s: RawByteString): ZWideString;
{$IFEND}

function Hash(const S : RawByteString) : LongWord; overload;
function Hash(const Key : ZWideString) : Cardinal; overload;

{$IFNDEF NO_ANSISTRING}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF}); overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$ENDIF}
{$IFNDEF NO_UTF8STRING}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: UTF8String); overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$ENDIF}
procedure ZSetString(Src: PAnsiChar; const Len: LengthInt; var Dest: ZWideString); overload; //{$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$IF defined (WITH_RAWBYTESTRING) or defined(WITH_TBYTES_AS_RAWBYTESTRING)}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: RawByteString); overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$IFEND}
{$IFDEF WITH_RAWBYTESTRING}
procedure ZSetString(Src: PAnsiChar; Len: Cardinal; var Dest: RawByteString; CP: Word); overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$ENDIF}

function RawConcat(const Vals: array of RawByteString): RawByteString;

{$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}
function Min(const A, B: NativeUInt): NativeUInt; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
function Max(const A, B: NativeUInt): NativeUInt; overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
{$ENDIF}

{$IF NOT DEFINED(FPC) AND NOT DECLARED(ReturnAddress)} // intrinsic since XE2
{$DEFINE ZReturnAddress}
function ReturnAddress: Pointer;
{$IFEND}

{$IF defined(CPUARM) and not defined(FPC)}
function align(addr: NativeUInt; alignment: NativeUInt) : NativeUInt; inline;
{$IFEND}

var
  {$IFDEF FPC}
    {$PUSH}
    {$WARN 3177 off : Some fields coming after "$1" were not initialized}
    {$WARN 3175 off : Some fields coming before "$1" were not initialized}
  {$ENDIF}
  ClientCodePageDummy: TZCodepage =
    (CharWidth: 1; Encoding: ceAnsi; CP: $ffff);

  ConSettingsDummy: TZConSettings =
    (AutoEncode: False;
      CPType: {$IFDEF DELPHI}{$IFDEF UNICODE}cCP_UTF16{$ELSE}cGET_ACP{$ENDIF}{$ELSE}cCP_UTF8{$ENDIF};
      ClientCodePage: @ClientCodePageDummy;
      DisplayFormatSettings:
          (DateFormat: DefDateFormatDMY;
          DateFormatLen: Length(DefDateFormatDMY);
          TimeFormat: DefTimeFormatMsecs;
          TimeFormatLen: Length(DefTimeFormatMsecs);
          DateTimeFormat: DefDateTimeFormat;
          DateTimeFormatLen: Length(DefDateTimeFormat));
      ReadFormatSettings:
          (DateFormat: DefDateFormatDMY;
          DateFormatLen: Length(DefDateFormatDMY);
          TimeFormat: DefTimeFormatMsecs;
          TimeFormatLen: Length(DefTimeFormatMsecs);
          DateTimeFormat: DefDateTimeFormatMsecs;
          DateTimeFormatLen: Length(DefDateTimeFormatMsecs));
      WriteFormatSettings:
          (DateFormat: DefDateFormatDMY;
          DateFormatLen: Length(DefDateFormatDMY);
          TimeFormat: DefTimeFormatMsecs;
          TimeFormatLen: Length(DefTimeFormatMsecs);
          DateTimeFormat: DefDateTimeFormatMsecs;
          DateTimeFormatLen: Length(DefDateTimeFormatMsecs));
      {$IFDEF WITH_LCONVENCODING}
      PlainConvertFunc: @NoConvert;
      DbcConvertFunc: @NoConvert;
      {$ENDIF}
    );
  {$IFDEF FPC} {$POP} {$ENDIF}

const
  PEmptyUnicodeString: PWideChar = '';
  PEmptyAnsiString: PAnsiChar = '';
  EmptyRaw = {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}nil{$ELSE}RawByteString(''){$ENDIF};
var
  ZOSCodePage: Word;
  ZOSCodePageMaxCharSize: Word;

implementation

{$IFDEF FAST_MOVE}
uses ZFastCode;
{$ENDIF}

function TZCodePagedObject.GetConSettings: PZConSettings;
begin
  Result := FConSettings;
end;

procedure TZCodePagedObject.SetConSettingsFromInfo(Info: TStrings);
var S: String;
begin
  if Assigned(Info) and Assigned(FConSettings) then begin
    {$IF defined(MSWINDOWS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER) or defined(WITH_LCONVENCODING) or defined(UNICODE)}
    S := Info.Values['AutoEncodeStrings'];
    S := Uppercase(S);
    //EH: Using StrToBoolEx by adding ZSysUtils to the uses clause
    //leads to inline Warnings with some Delphi's.
    //As long this Object (my mistake) is part of core
    //(Should be dbc same like IZClientVariantManagaer and TZURL)
    //and is defined in ZCompatibility we do the boolean evaluation by hand
    ConSettings.AutoEncode := (S <> '') and ((S = 'ON') or (S = 'TRUE') or (S = 'YES') or (StrToIntDef(S,0)<>0)); //compatibitity Option for existing Applications;
    {$ELSE}
    ConSettings.AutoEncode := False;
    {$IFEND}
    S := Info.Values['controls_cp'];
    S := UpperCase(S);
    {$IF defined(Delphi) and defined(UNICODE) and defined(MSWINDOWS)}
    ConSettings.CTRL_CP := DefaultSystemCodePage;
    if Info.Values['controls_cp'] = 'GET_ACP'
    then ConSettings.CPType := cGET_ACP
    else ConSettings.CPType := cCP_UTF16;
    {$ELSE}
    if Info.Values['controls_cp'] = 'GET_ACP' then begin
      ConSettings.CPType := cGET_ACP;
      ConSettings.CTRL_CP := ZOSCodePage;
    end else if Info.Values['controls_cp'] = 'CP_UTF8' then begin
      ConSettings.CPType := cCP_UTF8;
      ConSettings.CTRL_CP := 65001;
    end else if Info.Values['controls_cp'] = 'CP_UTF16' then begin
      {$IFDEF WITH_WIDEFIELDS}
      ConSettings.CPType := cCP_UTF16;
        {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
        ConSettings.CTRL_CP := DefaultSystemCodePage;
        {$ELSE}
        ConSettings.CTRL_CP := ZOSCodePage;
        {$ENDIF}
      {$ELSE}
      ConSettings.CPType := cCP_UTF8;
      ConSettings.CTRL_CP := 65001;
      {$ENDIF}
    end else begin // nothing was found set defaults
      {$IFDEF LCL}
      ConSettings.CPType := cCP_UTF8;
      ConSettings.CTRL_CP := 65001;
      {$ELSE}
        {$IFDEF UNICODE}
        ConSettings.CPType := cCP_UTF16;
        {$ELSE}
        ConSettings.CPType := cGET_ACP;
        {$ENDIF}
        {$IFDEF WITH_DEFAULTSYSTEMCODEPAGE}
        ConSettings.CTRL_CP := DefaultSystemCodePage;
        {$ELSE}
        ConSettings.CTRL_CP := ZOSCodePage;
        {$ENDIF}
      {$ENDIF}
    end;
    {$IFEND}
  end;
end;

{$IFDEF WITH_LCONVENCODING}
function NoConvert(const s: string): string;
begin
  Result := S;
end;
{$ENDIF}


{$IFDEF UNIX}
  {$IFDEF FPC}
function LoadLibrary(ModuleName: PChar): HMODULE;
begin
  Result := dynlibs.LoadLibrary(ModuleName);
end;

function FreeLibrary(Module: HMODULE): LongBool;
begin
  Result := dynlibs.FreeLibrary(Module);
end;

function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
begin
  Result := dynlibs.GetProcAddress(Module,Proc)
end;
  {$ENDIF}
{$ENDIF}

{$Q-}
{$R-}

function Hash(const key: ZWideString): Cardinal;
var
  I: integer;
begin
  Result := 0;
  for I := 1 to length(key) do
  begin
    Result := (Result shl 5) or (Result shr 27);
    Result := Result xor Cardinal(key[I]);
  end;
end; { Hash }

(*function Hash(const S: RawByteString): Cardinal; //perform the FPC used ELF Hash algorithm -> pretty slow(byte hashed) but tiny
Var
  thehash,g,I : LongWord;
begin
  thehash:=0;
  For I:=1 to Length(S) do { 0 terminated }
  begin
    thehash:=thehash shl 4;
    inc(theHash,Ord(S[i]));
    g:=thehash and $f0000000;;
    if g<>0 then
    begin
      thehash:=thehash xor (g shr 24);
      thehash:=thehash xor g;
    end;
  end;
  If theHash=0 then
     Result := $ffffffff
   else
     Result :=TheHash;
end;*)

{ ported from http://stofl.org/questions/3690608/simple-string-hashing-function}
//perform a MurmurHash2 algorithm by Austin Appleby loads faster (4Byte aligned)
//Changes by EgonHugeist:
//use PAnsiChar instead of S[] to inc the 4Byte blocks -> faster!
//note: we can also use 64Bit versions: http://factgrabber.com/index.php?q=MurmurHash&lcid=xrnmicaJ5olmGUbBZJlkwWah5plkiYaJJpkGgSexJhkm
//function MurmurHash2(const S: RawByteString; const Seed: LongWord=$9747b28c): LongWord;
function Hash(const S: RawByteString): LongWord;
var
  k: LongWord;
  Len: LongWord;
  P, PEnd: PAnsiChar;
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5bd1e995;
  r = 24;
begin
  //The default seed, $9747b28c, is from the original C library
  P := Pointer(S);
  if P = nil then
    Result := $ffffffff
  else
  begin
    Len := {%Result-}PLengthInt(P - StringLenOffSet)^;
    // Initialize the hash to a 'random' value
    Result := $9747b28c xor len;

    // Mix 4 bytes at a time into the hash
    PEnd := P + Len - 4;
    while P < PEnd do
    begin
      k := PLongWord(P)^;

      k := k * m;
      k := k xor (k shr r);
      k := k * m;

      Result := Result * m;
      Result := Result xor k;

      Inc(P, 4);
    end;
    Inc(PEnd, 4);
    Len := PEnd-P;

    {   Handle the last few bytes of the input array
            P: ... $69 $18 $2f
    }
    if len = 3 then
      Result := Result xor (LongWord((P+2)^) shl 16);
    if len >= 2 then
      Result := Result xor (LongWord((P+1)^) shl 8);
    if len >= 1 then
    begin
      Result := Result xor (LongWord(P^));
      Result := Result * m;
    end;

    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.
    Result := Result xor (Result shr 13);
    Result := Result * m;
    Result := Result xor (Result shr 15);

    Result := Result;
  end;
end;

{$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
{$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}

{$IFDEF ZAnsiProperCase}

function AnsiProperCase(const S: string; const WordDelims: {$IFDEF WITH_TSYSCHARSET_DEPRECATED} String {$ELSE} TSysCharSet {$ENDIF}): string;
var
  P,PE : PChar;
begin
  Result:=AnsiLowerCase(S);
  P:=PChar(pointer(Result));
  PE:=P+Length(Result);
  while (P<PE) do
    begin
{$IFDEF WITH_TSYSCHARSET_DEPRECATED}
    while (P<PE) and (WordDelims.CountChar(P^) > 0) do
{$ELSE}
    while (P<PE) and CharInSet(P^, WordDelims) do
{$ENDIF}
      inc(P);
    if (P<PE) then
      P^:=UpCase(P^);
{$IFDEF WITH_TSYSCHARSET_DEPRECATED}
    while (P<PE) and not (WordDelims.CountChar(P^) > 0) do
{$ELSE}
    while (P<PE) and not (CharInSet(P^, WordDelims)) do
{$ENDIF}
      inc(P);
    end;
end;
{$ENDIF}

{$IFNDEF WITH_CHARINSET}
function CharInSet(const C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  result := C in Charset;
end;

function CharInSet(const C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  result := CharInSet(Word(C), CharSet);
end;

function CharInSet(const C: Word; const CharSet: TSysCharSet): Boolean;
begin
  result := (C <= High(Byte)) and (AnsiChar(Byte(C)) in Charset);
end;

{$ENDIF}

{$IFDEF  ZUTF8ToString}
function UTF8ToString(const s: RawByteString): ZWideString;
begin
  Result := UTF8Decode(s);
end;
{$UNDEF ZUTF8ToString}
{$ENDIF}

{$IFNDEF NO_ANSISTRING}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: {$IFDEF UNICODE}AnsiString{$ELSE}String{$ENDIF});
begin
  if ( Len = 0 ) then
    Dest := ''
  else
    if (Pointer(Dest) <> nil) and //Empty?
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ = 1) {refcount} and
       ({%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^ = LengthInt(Len)) {length} then begin
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len)
    end else
    {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
    begin
      Dest := '';
      SetLength(Dest, Len);
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
    end;
    {$ELSE}
      SetString(Dest, Src, Len);
    {$ENDIF}
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: UTF8String);
begin
  if ( Len = 0 ) then
    Dest := ''
  else
    if (Pointer(Dest) <> nil) and //Empty?
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ = 1) {refcount} and
       ({%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^ = LengthInt(Len)) {length} then
    begin
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
    end
    else
      {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
      begin
        Dest := '';
        SetLength(Dest, Len);
        if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
      end;
      {$ELSE}
      SetString(Dest, Src, Len);
      {$ENDIF}
end;
{$ENDIF}

//EgonHugeist: my fast ByteToWord shift without encoding maps and/or alloc a ZWideString
procedure ZSetString(Src: PAnsiChar; const Len: LengthInt; var Dest: ZWideString); overload;
var
  PEnd: PAnsiChar;
  PW: PWideChar;
begin
  if ( Len = 0 ) then
    Dest := ''
  else
  begin
    {$IFDEF PWIDECHAR_IS_PUNICODECHAR}
    if (Pointer(Dest) = nil) or//empty
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ <> 1) or { unique string ? }
       (Len <> {%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^) then { length as expected ? }
    {$ELSE}
    if Length(Dest) <> Len then //WideString isn't ref counted
    {$ENDIF}
    SetLength(Dest, Len);
    if Src <> nil then
    begin
      PW := Pointer(Dest);
      PEnd := Src+Len-4;
      while Src < PEnd do //quad conversion per loop
      begin
        PWord(PW)^ := PByte(Src)^;
        PWord(PW+1)^ := PByte(Src+1)^;
        PWord(PW+2)^ := PByte(Src+2)^;
        PWord(PW+3)^ := PByte(Src+3)^;
        Inc(Src, 4);
        Inc(PW, 4);
      end;
      Inc(PEnd, 4);
      while Src < PEnd do
      begin
        PWord(PW)^ := PByte(Src)^;
        Inc(Src);
        Inc(PW);
      end;
    end;
  end;
end;

{$IF defined(WITH_RAWBYTESTRING) or defined(WITH_TBYTES_AS_RAWBYTESTRING)}
procedure ZSetString(const Src: PAnsiChar; const Len: Cardinal; var Dest: RawByteString);
begin
  if ( Len = 0 ) then
    Dest := EmptyRaw
  else
    {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
    if (Pointer(Dest) <> nil) and //Empty?
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ = 1) {refcount} and
       ({%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^ = LengthInt(Len)) {length} then begin
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len)
    end else
    {$ENDIF}
      {$IFDEF MISS_RBS_SETSTRING_OVERLOAD}
      begin
        Dest := EmptyRaw;
        SetLength(Dest, Len{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
        {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}(PByte(Dest)+Len)^ := Ord(#0);{$ENDIF}
        if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
      end;
      {$ELSE}
      SetString(Dest, Src, Len);
      {$ENDIF}
end;
{$IFEND}

{$IFDEF WITH_RAWBYTESTRING}
procedure ZSetString(Src: PAnsiChar; Len: Cardinal; var Dest: RawByteString; CP: Word); overload; {$IFDEF WITH_INLINE}Inline;{$ENDIF}
begin
  if ( Len = 0 ) then
    Dest := EmptyRaw
  else begin
    if (Pointer(Dest) <> nil) and //Empty?
       ({%H-}PRefCntInt(NativeUInt(Dest) - StringRefCntOffSet)^ = 1) {refcount} and
       ({%H-}PLengthInt(NativeUInt(Dest) - StringLenOffSet)^ = LengthInt(Len)) {length} then begin
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
    end else begin
      Dest := EmptyRaw;
      SetLength(Dest, Len);
      if Src <> nil then {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Src^, Pointer(Dest)^, Len);
    end;
    {$IFDEF FPC}
    PAnsiRec(pointer(Dest)-AnsiFirstOff)^.CodePage := CP;
    {$ELSE}
    //System.SetCodePage(Dest, CP, False); is not inlined on FPC and the code inside is alreade executed her
    {%H-}PWord(NativeUInt(Dest) - CodePageOffSet)^ := CP;
    {$ENDIF}
  end;
end;
{$ENDIF}

{$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}
function Min(const A, B: NativeUInt): NativeUInt;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Max(const A, B: NativeUInt): NativeUInt;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;
{$ENDIF}

function RawConcat(const Vals: array of RawByteString): RawByteString;
var
  I: Integer;
  L: LengthInt;
  P: PAnsiChar;
begin
  L := 0;
  for I := Low(Vals) to High(Vals) do
    if Pointer(Vals[i]) <> nil then
      Inc(L, Length(Vals[i]){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
  SetLength(Result, L{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
  P := Pointer(Result);
  AnsiChar((P+L)^) := AnsiChar(#0);
  for I := Low(Vals) to High(Vals) do
    if Pointer(Vals[i]) <> nil then begin
      L := Length(Vals[i]){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
      System.Move(Pointer(Vals[i])^, P^, L);
      Inc(P, L);
    end;
end;

{$IF defined(CPUARM) and not defined(FPC)}
function align(addr: NativeUInt; alignment: NativeUInt) : NativeUInt;
var
  tmp: NativeUInt;
begin
  tmp := addr + (alignment-1);
  result := tmp - (tmp mod alignment)
end;
{$IFEND}

{$IFDEF ZReturnAddress}
function ReturnAddress: Pointer;
{$IFDEF PUREPASCAL}
  begin
    Result := nil;
  end;
{$ELSE}
  asm
          MOV     EAX,[EBP+4]
  end;
{$ENDIF}
{$ENDIF ZReturnAddress}

initialization
  case ConSettingsDummy.CPType of
    cCP_UTF16, cGET_ACP: ConSettingsDummy.CTRL_CP := ZOSCodePage;
    cCP_UTF8: ConSettingsDummy.CTRL_CP := 65001;
  end;
end.
