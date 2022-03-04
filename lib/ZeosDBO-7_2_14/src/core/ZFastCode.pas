{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           System Utility Classes and Functions          }
{                                                         }
{            Originally written by EgonHugeist            }
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
{ Contributor(s): Aleksandr Sharahov,                     }
{                 Dennis Kjaer Christensen                }
{                 John O'Harrow                           }
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

unit ZFastCode;

interface

{$I ZCore.inc}

  {$Q-} //disable OverflowCheck
  {$R-} //disable RangeCheck


uses
  ZCompatibility, SysUtils;

const
  {$IFDEF NEXTGEN}
  TwoDigitLookupUni : packed array[0..99] of array[1..2] of WideChar =
  {$ELSE}
  TwoDigitLookupRaw : packed array[0..99] of array[1..2] of AnsiChar =
  {$ENDIF}
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

  HighInt64 = High(Int64);
  {size of TStrRec in system.pas}
  TStrRecordSize = {$IFDEF WITH_RAWBYTESTRING}12{$ELSE}8{$ENDIF};

var
  TwoDigitLookupW : packed array[0..99] of Word {$IFNDEF NEXTGEN}absolute TwoDigitLookupRaw{$ENDIF};

var
  TwoDigitLookupLW : packed array[0..99] of Cardinal{$IFDEF NEXTGEN}absolute TwoDigitLookupUni{$ENDIF};

{$If defined(Use_FastCodeFillChar) or defined(PatchSystemMove) or defined(USE_FAST_STRLEN) or defined(USE_FAST_CHARPOS)}
  {$D-} {Prevent Steppping into Move Code} //EH: moved after FastCode.inc is loaded to prevent debugging
  {$IFDEF VER170}
    {$DEFINE SSE2}
  {$ENDIF}

  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion >= 18.0}
      {$DEFINE SSE2}
      {$WARN UNSAFE_CODE OFF}
      {$WARN UNSAFE_CAST OFF}
    {$IFEND}
  {$ENDIF}

{***** BEGIN LICENSE BLOCK *****
 FastcodeCPUID extraction
  Version: MPL 1.1

  The contents of this file are subject to the Mozilla Public License Version 1.1
  (the "License"); you may not use this file except in compliance with the
  License. You may obtain a copy of the License at http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is the FastCode CPUID code.

  The Initial Developer of the Original Code is
  Roelof Engelbrecht <roelof@cox-internet.com>. Portions created by
  the Initial Developer are Copyright (C) 2004 by the Initial Developer.
  All Rights Reserved.

  Contributor(s): Dennis Passmore <Dennis_Passmore@ ultimatesoftware.com>,
  Dennis Christensen <marianndkc@home3.gvdnet.dk>,
  Jouni Turunen <jouni.turunen@NOSPAM.iki.fi>.
  John O'Harrow <john@elmcrest.demon.co.uk>

  ***** END LICENSE BLOCK *****

  Version  Changes
  -------  ------
  3.0.8   30-Now-2012 : Modified Fastcode targets
  3.0.7   1-Aug-2012 : Added VerifyOSSupportForYMMRegisters by Philipp S
  3.0.6   3-juli-2012 : Added Win64 support - GetCPUID by Philipp S, IsCPUID_Available advice from Remy Lebeau,
  see http://www.intel.com/content/www/us/en/processors/processor-identification-cpuid-instruction-note.html
  3.0.5   17-Dec-2009 : Added 2009 FastCode Targets (DKC)
  3.0.4   20-Nov-2007 : Added SSE4A, SSE4.1, SSE4.2 and SSE5 detections. Added 2007 FastCode Targets
  3.0.3   20-Nov-2006 : Added SSSE3 Detection
  3.0.2   27 Apr 2006 : AMD X2 text changed from 'AMD_64_SSE3' to 'AMD_64X2'
  3.0.1   18 Apr 2006 : Bug in Yohan fctPMY target fixed, was incorrectly set to fctPMD
  3.0.0   27 Feb 2006 : Added new 2006 computed targets. Added Yonah and Presler
  Removed Prescott, Banias, AMD XP                    (JT)

}
type
 TVendor = (cvUnknown, cvAMD, cvCentaur, cvCyrix, cvIntel,
   cvTransmeta, cvNexGen, cvRise, cvUMC, cvNSC, cvSiS);
 { Note: when changing TVendor, also change VendorStr array below }

 TInstructions =
   (isFPU, { 80x87 }
   isTSC, { RDTSC }
   isCX8, { CMPXCHG8B }
   isSEP, { SYSENTER/SYSEXIT }
   isCMOV, { CMOVcc, and if isFPU, FCMOVcc/FCOMI }
   isMMX, { MMX }
   isFXSR, { FXSAVE/FXRSTOR }
   isSSE, { SSE }
   isSSE2, { SSE2 }
   isSSE3, { SSE3* }
   isSSSE3, { SSSE3 }
   isSSE4A, { SSE4A }
   isSSE41, { SSE4.1 }
   isSSE42, { SSE4.2 }
   isSSE5, { SSE5 }
   isAVX, { AVX }
   isMONITOR, { MONITOR/MWAIT* }
   isCX16, { CMPXCHG16B* }
   isX64, { AMD AMD64* or Intel EM64T* }
   isExMMX, { MMX+ - AMD only }
   isEx3DNow, { 3DNow!+ - AMD only }
   is3DNow); { 3DNow! - AMD only }

 { Note: when changing TInstruction, also change InstructionSupportStr below
   * - instruction(s) not supported in Delphi 7 assembler }
 TInstructionSupport = set of TInstructions;

 TCPU = record
  Vendor: TVendor;
  Signature: Cardinal;
  EffFamily: Byte; { ExtendedFamily + Family }
  EffModel: Byte; { (ExtendedModel shl 4) + Model }
  EffModelBasic: Byte; { Just Model (not ExtendedModel shl 4) + Model) }
  CodeL1CacheSize, { KB or micro-ops for Pentium 4 }
  DataL1CacheSize, { KB }
  L2CacheSize, { KB }
  L3CacheSize: Word; { KB }
  InstructionSupport: TInstructionSupport;
 end;

 // Needs updating after a computed targets poll is closed

 TFastCodeTarget =
   (fctIA32, { not specific to any CPU }
   fctIA32SizePenalty, { not specific to any CPU, In library routines with size penalties used This target was called "fctRTLReplacement" earlier }
   fctMMX, { not specific to any CPU, requires FPU, MMX and CMOV  "Old fctBlended target" }
   fctMMXSizePenalty, { not specific to any CPU, requires FPU, MMX and CMOV  "Old fctBlended target" In library routines with size penalties used }
   fctSSE, { not specific to any CPU, requires FPU, MMX, CMOV and SSE }
   fctSSE2, { not specific to any CPU, requires FPU, MMX, CMOV, SSE, SSE2 }
   fctSSE3, { not specific to any CPU, requires FPU, MMX, CMOV, SSE, SSE2, SSE3 }
   fctSSSE3, { not specific to any CPU, requires FPU, MMX, CMOV, SSE, SSE2, SSE3, SSSE3 }
   fctSSE41, { not specific to any CPU, requires FPU, MMX, CMOV, SSE, SSE2, SSE3, SSSE3 }
   fctPascal, { use Pascal routines in library }
   fctPascalSizePenalty, { use Pascal routines with size penalty in library }
   fctPMNehalem, { Nehalem }
   fctPMWestmere, { Westmere }
   fctPMSandyBridge, { Sandy Bridge }
   fctPMIvyBridge, { Ivy Bridge }
   fctAmdPhenom, { AMD Phenom }
   fctAmdBulldozer); { AMD Bulldozer }

 { Note: when changing TFastCodeTarget, also change FastCodeTargetStr array
   below }

const
 VendorStr: array [Low(TVendor) .. High(TVendor)] of ShortString =
   ('Unknown', 'AMD', 'Centaur (VIA)', 'Cyrix', 'Intel', 'Transmeta',
   'NexGen', 'Rise', 'UMC', 'National Semiconductor', 'SiS');

 InstructionSupportStr:
   array [Low(TInstructions) .. High(TInstructions)] of ShortString =
   ('FPU', 'TSC', 'CX8', 'SEP', 'CMOV', 'MMX', 'FXSR', 'SSE', 'SSE2', 'SSE3',
   'SSSE3', 'SSE4A', 'SSE41', 'SSE42', 'SSE5', 'AVX', 'MONITOR', 'CX16', 'X64', 'MMX+', '3DNow!+', '3DNow!');

 FastCodeTargetStr:
   array [Low(TFastCodeTarget) .. High(TFastCodeTarget)] of ShortString =
   ('IA32', 'IA32_SizePenalty', 'MMX', 'MMX_SizePenalty', 'SSE',
   'SSE2', 'SSE3', 'SSSE3', 'SSE41', 'Pascal', 'Pascal_SizePenalty',
   'Penryn', 'Westmere', 'Sandy Bridge', 'Ivy Bridge', 'AMD_Phenom', 'AMD_Buldozer');

var
 CPU: TCPU;
 FastCodeTarget: TFastCodeTarget;
{$IFEND}

{$IF defined(PatchSystemMove) or defined(FAST_MOVE)} //set in Zeos.inc
var
  Move : procedure(const Source; var Dest; Count : Integer); {Fastest Move}

{Procedures interfaced only for testing and validation purposes}
  procedure Move_JOH_IA32_10(const Source; var Dest; Count : Integer);
  procedure Move_JOH_MMX_10 (const Source; var Dest; Count : Integer);
  procedure Move_JOH_SSE_10 (const Source; var Dest; Count : Integer);
  procedure Move_JOH_SSE2_10(const Source; var Dest; Count : Integer);
  procedure Move_JOH_SSE3_10(const Source; var Dest; Count : Integer);
{$IFEND PatchSystemMove} //set in Zeos.inc

{$IFDEF Use_FastCodeFillChar}
var
  FillChar: procedure (var Dest; count: Integer; Value: AnsiChar);
{$ENDIF Use_FastCodeFillChar}

var
  {$IFDEF FPC}
    {$IFDEF cpuarm}
    StrLen: function(Str: PChar): sizeint;
    {$ELSE}
    StrLen: function(Str: PAnsiChar): {$IFDEF cpui386}LongInt{$ELSE}Int64{$ENDIF};
    {$ENDIF}
  {$ELSE}
  StrLen: function(const Str: PAnsiChar): Cardinal;
  {$ENDIF}

{$IFDEF USE_FAST_CHARPOS}
var
  CharPos: function(ch: char; const s: String): integer;
{$ENDIF}

function IntToStr(Value: Integer): String; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function IntToStr(Value: ShortInt): String; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function IntToStr(Value: Byte): String; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function IntToStr(Value: SmallInt): String; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function IntToStr(Value: Word): String; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function IntToStr(Value: Cardinal): String; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function IntToStr(const Value: Int64): String; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
function IntToStr(const Value: UInt64): String; overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}

{ Integer convertion in Raw and Unicode Strings}
function IntToRaw(Value: Integer): RawByteString; overload;  //keep always this one @first pos because of the Ansi-Delphi faster BASM code (the int64 version call the 32bit within range)
function IntToRaw(Value: Byte): RawByteString; overload;
function IntToRaw(Value: ShortInt): RawByteString; overload;
function IntToRaw(Value: Word): RawByteString; overload;
function IntToRaw(Value: SmallInt): RawByteString; overload;
function IntToRaw(Value: Cardinal): RawByteString; overload;
function IntToRaw(Value: Int64): RawByteString; overload;
function IntToRaw(const Value: UInt64): RawByteString; overload;

procedure IntToRaw(Value: Integer; Buf: PAnsiChar; PEnd: PPAnsiChar = nil); overload;
procedure IntToRaw(Value: Cardinal; Buf: PAnsiChar; PEnd: PPAnsiChar = nil); overload;
procedure IntToRaw(const Value: Int64; Buf: PAnsiChar; PEnd: PPAnsiChar = nil); overload;
procedure IntToRaw(const Value: UInt64; Buf: PAnsiChar; PEnd: PPAnsiChar = nil); overload;

procedure IntToRaw(Value: Cardinal; Buf: PAnsiChar; Digits: Byte); overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure IntToRaw(Value: UInt64; Buf: PAnsiChar; Digits: Byte); overload;

procedure IntToUnicode(Value: Integer; Buf: PWideChar; PEnd: ZPPWideChar = nil); overload;
procedure IntToUnicode(Value: Cardinal; Buf: PWideChar; PEnd: ZPPWideChar = nil); overload;
procedure IntToUnicode(const Value: Int64; Buf: PWideChar; PEnd: ZPPWideChar = nil); overload;
procedure IntToUnicode(const Value: UInt64; Buf: PWideChar; PEnd: ZPPWideChar = nil); overload;

procedure IntToUnicode(Value: Cardinal; Buf: PWideChar; Digits: Byte); overload; {$IFDEF WITH_INLINE}inline;{$ENDIF}
procedure IntToUnicode(Value: UInt64; Buf: PWideChar; Digits: Byte); overload;

function IntToUnicode(Value: Byte): ZWideString; overload;
function IntToUnicode(Value: ShortInt): ZWideString; overload;
function IntToUnicode(Value: Word): ZWideString; overload;
function IntToUnicode(Value: SmallInt): ZWideString; overload;
function IntToUnicode(Value: Cardinal): ZWideString; overload;
function IntToUnicode(Value: Integer): ZWideString; overload;
function IntToUnicode(const Value: Int64): ZWideString; overload;
function IntToUnicode(const Value: UInt64): ZWideString; overload;

procedure CurrToRaw(const Value: Currency; Buf: PAnsiChar; PEnd: PPAnsiChar = nil); overload;
function CurrToRaw(const Value: Currency): RawByteString; overload;

procedure CurrToUnicode(const Value: Currency; Buf: PWideChar; PEnd: ZPPWideChar = nil); overload;
function CurrToUnicode(const Value: Currency): ZWideString; overload;

function RawToInt(const Value: RawByteString): Integer; overload;
function RawToInt(const Value: PAnsiChar): Integer; overload;
function RawToInt64(const Value: RawByteString): Int64;
function RawToUInt64(const Value: RawByteString): UInt64;
function UnicodeToInt(const Value: ZWideString): Integer;
function UnicodeToInt64(const Value: ZWideString): Int64;
function UnicodeToUInt64(const Value: ZWideString): UInt64;

function RawToIntDef(const S: RawByteString; const Default: Integer) : Integer; overload;
function RawToIntDef(const S: PAnsiChar; const Default: Integer) : Integer; overload;
function RawToIntDef(Buf, PEnd: PAnsiChar; const Default: Integer) : Integer; overload;
function RawToInt64Def(const S: RawByteString; const Default: Int64) : Int64; overload;
function RawToInt64Def(const S: PAnsiChar; const Default: Int64) : Int64; overload;
function RawToInt64Def(Buf, PEnd: PAnsiChar; const Default: Int64) : Int64; overload;
function RawToUInt64Def(const S: PAnsiChar; const Default: UInt64) : UInt64; overload;
function RawToUInt64Def(Buf, PEnd: PAnsiChar; const Default: UInt64) : UInt64; overload;
function RawToUInt64Def(const S: RawByteString; const Default: UInt64) : UInt64; overload;

function UnicodeToIntDef(const S: ZWideString; const Default: Integer) : Integer; overload;
function UnicodeToIntDef(const S: PWideChar; const Default: Integer) : Integer; overload;
function UnicodeToIntDef(Buf, PEnd: PWideChar; Default: Integer) : Integer; overload;
function UnicodeToInt64Def(const S: ZWideString; const Default: Int64) : Int64; overload;
function UnicodeToInt64Def(const S: PWideChar; const Default: Int64) : Int64; overload;
function UnicodeToInt64Def(Buf, PEnd: PWideChar; Default: Integer) : Int64; overload;
function UnicodeToUInt64Def(const S: ZWideString; const Default: UInt64) : UInt64; overload;
function UnicodeToUInt64Def(const S: PWideChar; const Default: UInt64) : UInt64; overload;
function UnicodeToUInt64Def(Buf, PEnd: PWideChar; Default: Integer) : UInt64; overload;

{ Float convertion in Raw and Unicode Format}
function RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar): Extended; overload;
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar; var Result: Extended); overload;
{$IFEND}
procedure RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar; var Result: Currency); overload;
procedure RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar; var Result: Double); overload;
procedure RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar; var Result: Single); overload;
function RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar; const Default: Extended): Extended; overload;
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar; const Default: Extended; var Result: Extended); overload;
{$IFEND}
procedure RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar; const Default: Currency; var Result: Currency); overload;
procedure RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar; const Default: Double; var Result: Double); overload;
procedure RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar; const Default: Single; var Result: Single); overload;
function ValRawExt(const S: PByteArray; const DecimalSep: AnsiChar; out code: Integer): Extended;
function ValRawDbl(const S: PByteArray; const DecimalSep: AnsiChar; out code: Integer): Double;
function ValRawSin(const S: PByteArray; const DecimalSep: AnsiChar; out code: Integer): Single;
function ValRawCurr(S: PByteArray; DecimalSep: Char; var Len: Integer): Currency;

function ValRawInt(const s: RawByteString; out code: Integer): Integer; overload;
function ValRawInt(s: PAnsiChar; out code: Integer): Integer; overload;
function ValRawInt(PStart: PAnsiChar; var PEnd: PAnsiChar): Integer; overload;
function ValRawInt64(PStart: PAnsiChar; var PEnd: PAnsiChar): Int64; overload;
function ValRawUInt64(PStart: PAnsiChar; var PEnd: PAnsiChar): UInt64; overload;

function ValUnicodeInt(PStart: PWideChar; var PEnd: PWideChar): Integer; overload;
function ValUnicodeInt64(PStart: PWideChar; var PEnd: PWideChar): Int64; overload;
function ValUnicodeUInt64(PStart: PWideChar; var PEnd: PWideChar): UInt64; overload;

function UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar): Extended; overload;
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar; var Result: Extended); overload;
{$IFEND}
procedure UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar; var Result: Currency); overload;
procedure UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar; var Result: Double); overload;
procedure UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar; var Result: Single); overload;
function UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Extended): Extended; overload;
{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Extended; var Result: Extended); overload;
{$IFEND}
procedure UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Currency; var Result: Currency); overload;
procedure UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Double; var Result: Double); overload;
procedure UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Single; var Result: Single); overload;

function ValUnicodeExt(const s: PWordArray; const DecimalSep: WideChar; out code: Integer): Extended;
function ValUnicodeDbl(const s: PWordArray; const DecimalSep: WideChar; out code: Integer): Double;
function ValUnicodeSin(const s: PWordArray; const DecimalSep: WideChar; out code: Integer): Single;
function ValUnicodeCurr(s: PWordArray; DecimalSep: Char; var Len: Integer): Currency;

{Faster Floating functions ..}

{$IFDEF USE_FAST_TRUNC}
function Trunc(const X: Extended): Int64; overload;
function Trunc(const X: Double): Int64; overload;
function Trunc(const X: Single): Int64; overload;
{$ENDIF USE_FAST_TRUNC}

function Pos(const SubStr: RawByteString; const Str: RawByteString): Integer; overload;
function Pos(const SubStr, Str: ZWideString): Integer; overload;

function PosEx(const SubStr: RawByteString; Str: PAnsiChar; len: LengthInt; Offset: Integer = 1): Integer; overload;
function PosEx(SubStr, Str: PAnsiChar; SubStrLen, Strlen: LengthInt; Offset: Integer = 1): Integer; overload;
function PosEx(const SubStr, S: RawByteString; Offset: Integer = 1): Integer; overload;

function PosEx(const SubStr: ZWideString; Str: PWideChar; len: LengthInt; Offset: Integer = 1): Integer; overload;
function PosEx(SubStr, Str: PWideChar; SubStrLen, Strlen: LengthInt; Offset: Integer = 1): Integer; overload;
function PosEx(const SubStr, S: ZWideString; Offset: Integer = 1): Integer; overload;

function GetOrdinalDigits(const Value: UInt64): Byte; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function GetOrdinalDigits(const Value: Int64): Byte; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function GetOrdinalDigits(Value: Cardinal): Byte; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function GetOrdinalDigits(Value: Integer): Byte; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function GetOrdinalDigits(Value: Word): Byte; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function GetOrdinalDigits(Value: SmallInt): Byte; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function GetOrdinalDigits(Value: Byte): Byte; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}
function GetOrdinalDigits(Value: ShortInt): Byte; overload; {$IFDEF WITH_INLINE} inline;{$ENDIF}

const Int64Tower: array[0..18] of Int64 = (
  $0000000000000001 {                  1},
  $000000000000000A {                 10},
  $0000000000000064 {                100},
  $00000000000003E8 {               1000},
  $0000000000002710 {              10000},
  $00000000000186A0 {             100000},
  $00000000000F4240 {            1000000},
  $0000000000989680 {           10000000},
  $0000000005F5E100 {          100000000},
  $000000003B9ACA00 {         1000000000},
  $00000002540BE400 {        10000000000},
  $000000174876E800 {       100000000000},
  $000000E8D4A51000 {      1000000000000},
  $000009184E72A000 {     10000000000000},
  $00005AF3107A4000 {    100000000000000},
  $00038D7EA4C68000 {   1000000000000000},
  $002386F26FC10000 {  10000000000000000},
  $016345785D8A0000 { 100000000000000000},
  $0DE0B6B3A7640000 {1000000000000000000} );

{$IF defined(NEED_TYPED_UINT64_CONSTANTS) or defined(WITH_UINT64_C1118_ERROR)}
//Handle FPC(up to year 2019) and very old delphi problems with the large constants...
  {$IF DEFINED(FPC) and DEFINED(ENDIAN_BIG)}
  cUInt64Tower: array[0..19] of Int64Rec = (
    (hi: $00000000; lo: $00000001), {                   1}
    (hi: $00000000; lo: $0000000A), {                  10}
    (hi: $00000000; lo: $00000064), {                 100}
    (hi: $00000000; lo: $000003E8), {                1000}
    (hi: $00000000; lo: $00002710), {               10000}
    (hi: $00000000; lo: $000186A0), {              100000}
    (hi: $00000000; lo: $000F4240), {             1000000}
    (hi: $00000000; lo: $00989680), {            10000000}
    (hi: $00000000; lo: $05F5E100), {           100000000}
    (hi: $00000000; lo: $3B9ACA00), {          1000000000}
    (hi: $00000002; lo: $540BE400), {         10000000000}
    (hi: $00000017; lo: $4876E800), {        100000000000}
    (hi: $000000E8; lo: $D4A51000), {       1000000000000}
    (hi: $00000918; lo: $4E72A000), {      10000000000000}
    (hi: $00005AF3; lo: $107A4000), {     100000000000000}
    (hi: $00038D7E; lo: $A4C68000), {    1000000000000000}
    (hi: $002386F2; lo: $6FC10000), {   10000000000000000}
    (hi: $01634578; lo: $5D8A0000), {  100000000000000000}
    (hi: $0DE0B6B3; lo: $A7640000), { 1000000000000000000}
    (hi: $8AC72304; lo: $89E80000));{10000000000000000000}
  _10Trillion: Int64Rec = (hi: $8AC72304; lo: $89E80000);
  {$ELSE}
  cUInt64Tower: array[0..19] of Int64Rec = (
    (lo: $00000001; hi: $00000000), {                   1}
    (lo: $0000000A; hi: $00000000), {                  10}
    (lo: $00000064; hi: $00000000), {                 100}
    (lo: $000003E8; hi: $00000000), {                1000}
    (lo: $00002710; hi: $00000000), {               10000}
    (lo: $000186A0; hi: $00000000), {              100000}
    (lo: $000F4240; hi: $00000000), {             1000000}
    (lo: $00989680; hi: $00000000), {            10000000}
    (lo: $05F5E100; hi: $00000000), {           100000000}
    (lo: $3B9ACA00; hi: $00000000), {          1000000000}
    (lo: $540BE400; hi: $00000002), {         10000000000}
    (lo: $4876E800; hi: $00000017), {        100000000000}
    (lo: $D4A51000; hi: $000000E8), {       1000000000000}
    (lo: $4E72A000; hi: $00000918), {      10000000000000}
    (lo: $107A4000; hi: $00005AF3), {     100000000000000}
    (lo: $A4C68000; hi: $00038D7E), {    1000000000000000}
    (lo: $6FC10000; hi: $002386F2), {   10000000000000000}
    (lo: $5D8A0000; hi: $01634578), {  100000000000000000}
    (lo: $A7640000; hi: $0DE0B6B3), { 1000000000000000000}
    (lo: $89E80000; hi: $8AC72304));{10000000000000000000}
  _10Trillion: Int64Rec = (lo: $89E80000; hi: $8AC72304);
  {$IFEND}
var
  UInt64Tower: array[0..19] of UInt64 absolute cUInt64Tower;
{$ELSE}
  UInt64Tower: array[0..19] of UInt64 = (
    $0000000000000001 {                   1},
    $000000000000000A {                  10},
    $0000000000000064 {                 100},
    $00000000000003E8 {                1000},
    $0000000000002710 {               10000},
    $00000000000186A0 {              100000},
    $00000000000F4240 {             1000000},
    $0000000000989680 {            10000000},
    $0000000005F5E100 {           100000000},
    $000000003B9ACA00 {          1000000000},
    $00000002540BE400 {         10000000000},
    $000000174876E800 {        100000000000},
    $000000E8D4A51000 {       1000000000000},
    $000009184E72A000 {      10000000000000},
    $00005AF3107A4000 {     100000000000000},
    $00038D7EA4C68000 {    1000000000000000},
    $002386F26FC10000 {   10000000000000000},
    $016345785D8A0000 {  100000000000000000},
    $0DE0B6B3A7640000 { 1000000000000000000},
    $8AC7230489E80000 {10000000000000000000});
  _10Trillion = UInt64(10000000000000000000);
{$IFEND}

implementation

uses
  {$IF defined(PatchSystemMove) and defined(MSWINDOWS)}Windows,{$IFEND}
  {$IF defined(WITH_STRLEN_DEPRECATED) and defined(WITH_UNITANSISTRINGS)}AnsiStrings, {$IFEND}
  SysConst, Math;

{$IF defined(PatchSystemMove) or defined(FAST_MOVE)} //set in Zeos.inc
var
  CacheLimit : Integer; {Used within SSE Moves}
{-------------------------------------------------------------------------}
{Move without using any BASM Code}
const
  TINYSIZE = 36;
{$IFEND}

function IntToStr(Value: Byte): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Cardinal(Value));
end;

function IntToStr(Value: ShortInt): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Integer(Value));
end;

function IntToStr(Value: Word): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Cardinal(Value));
end;

function IntToStr(Value: SmallInt): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Integer(Value));
end;

function IntToStr(Value: Integer): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value);
end;

function IntToStr(Value: Cardinal): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value);
end;

function IntToStr(const Value: Int64): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value);
end;

function IntToStr(const Value: UInt64): String;
begin
  Result := {$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value);
end;

{$IFDEF Use_FastCodeFillChar}
//Author:            Dennis Kjaer Christensen
//Date:              22/12 2003
//Optimized for:     Opteron
//Instructionset(s): IA32, MMX, SSE, SSE2
//Size

procedure FillChar_DKC_SSE2_10_b(var Dest; count: Integer; Value: AnsiChar);
asm
   test edx,edx
   jle  @Exit2
   //case Count of
   cmp  edx,31
   jnbe @CaseElse
   jmp  dword ptr [edx*4+@Case1JmpTable]
 @CaseCount0 :
   ret
 @CaseCount1 :
   mov  [eax],cl
   ret
 @CaseCount2 :
   mov  ch,cl
   mov  [eax],cx
   ret
 @CaseCount3 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cl
   ret
 @CaseCount4 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   ret
 @CaseCount5 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cl
   ret
 @CaseCount6 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   ret
 @CaseCount7 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cl
   ret
 @CaseCount8 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   ret
 @CaseCount9 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cl
   ret
 @CaseCount10 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   ret
 @CaseCount11 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cl
   ret
 @CaseCount12 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   ret
 @CaseCount13 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cl
   ret
 @CaseCount14 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   ret
 @CaseCount15 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cl
   ret
 @CaseCount16 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   ret
 @CaseCount17 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cl
   ret
 @CaseCount18 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   ret
 @CaseCount19 :
   mov  ch,cl
   mov  [eax],cx
   mov  [eax+2],cx
   mov  [eax+4],cx
   mov  [eax+6],cx
   mov  [eax+8],cx
   mov  [eax+10],cx
   mov  [eax+12],cx
   mov  [eax+14],cx
   mov  [eax+16],cx
   mov  [eax+18],cl
   ret
 @CaseCount20 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   ret
 @CaseCount21 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cl
   ret
 @CaseCount22 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   ret
 @CaseCount23 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cl
   ret
 @CaseCount24 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   ret
 @CaseCount25 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cl
   ret
 @CaseCount26 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   ret
 @CaseCount27 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cl
   ret
 @CaseCount28 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   ret
 @CaseCount29 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cl
   ret
 @CaseCount30 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   ret
 @CaseCount31 :
   mov   ch,cl
   mov   [eax],cx
   mov   [eax+2],cx
   mov   [eax+4],cx
   mov   [eax+6],cx
   mov   [eax+8],cx
   mov   [eax+10],cx
   mov   [eax+12],cx
   mov   [eax+14],cx
   mov   [eax+16],cx
   mov   [eax+18],cx
   mov   [eax+20],cx
   mov   [eax+22],cx
   mov   [eax+24],cx
   mov   [eax+26],cx
   mov   [eax+28],cx
   mov   [eax+30],cl
   ret
   nop
   nop
   nop
   nop
   nop
 @CaseElse :
   //Need at least 32 bytes here. Max 16 for alignment and 16 for loop
   push    esi
   push    edi
   //Broadcast value
   mov     ch, cl
   movd    xmm0, ecx
   pshuflw xmm0, xmm0, 0
   pshufd  xmm0, xmm0, 0
   //Fill first 16 non aligned bytes
   movdqu  [eax],xmm0
   //StopP2 := P + Count;
   lea     ecx,[eax+edx]
   //16 byte Align
   mov     edi,eax
   and     edi,$F
   mov     esi,16
   sub     esi,edi
   add     eax,esi
   sub     edx,esi
   //I := 0;
   xor     esi,esi
   sub     edx,15
   cmp     edx,1048576
   ja      @Repeat4
 @Repeat1 :
   movdqa  [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat1
   jmp     @Repeat4End
   nop
   nop
 @Repeat4 :
   movntdq [eax+esi],xmm0
   add     esi,16
   cmp     esi,edx
   jl      @Repeat4
 @Repeat4End :
   {movdq2q mm0,xmm0
   movntq  [ecx-16],mm0
   movntq  [ecx-8], mm0
   emms}
   //Fill the rest
   movdqu [ecx-16],xmm0
 @Exit1 :
   pop   edi
   pop   esi
 @Exit2 :
   ret
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop
   nop

@Case1JmpTable:
 dd @CaseCount0
 dd @CaseCount1
 dd @CaseCount2
 dd @CaseCount3
 dd @CaseCount4
 dd @CaseCount5
 dd @CaseCount6
 dd @CaseCount7
 dd @CaseCount8
 dd @CaseCount9
 dd @CaseCount10
 dd @CaseCount11
 dd @CaseCount12
 dd @CaseCount13
 dd @CaseCount14
 dd @CaseCount15
 dd @CaseCount16
 dd @CaseCount17
 dd @CaseCount18
 dd @CaseCount19
 dd @CaseCount20
 dd @CaseCount21
 dd @CaseCount22
 dd @CaseCount23
 dd @CaseCount24
 dd @CaseCount25
 dd @CaseCount26
 dd @CaseCount27
 dd @CaseCount28
 dd @CaseCount29
 dd @CaseCount30
 dd @CaseCount31

end;

//Author:            John O'Harrow
//Optimized for:     Opteron
//Instructionset(s): IA32, MMX, SSE, SSE
//Size
procedure FillChar_JOH_SSE_1_d(var Dest; count: Integer; Value: AnsiChar);
asm {Size = 161 Bytes}
  cmp       edx, 32
  mov       ch, cl                {Copy Value into both Bytes of CX}
  jl        @@Small
  sub       edx, 16
  mov       [eax], cx             {Fill First 4 Bytes}
  mov       [eax+2], cx
  movss     xmm0, [eax]           {Set each byte of XMM0 to Value}
  shufps    xmm0, xmm0, 0
  movups    [eax], xmm0           {Fill First 16 Bytes}
  movups    [eax+edx], xmm0       {Fill Last 16 Bytes}
  mov       ecx, eax              {16-Byte Align Writes}
  and       ecx, 15
  sub       ecx, 16
  sub       eax, ecx
  add       edx, ecx
  add       eax, edx
  neg       edx
  cmp       edx, -512*1024
  jb        @@Large
@@Loop:
  movaps    [eax+edx], xmm0       {Fill 16 Bytes per Loop}
  add       edx, 16
  jl        @@Loop
  ret
@@Large:
  movntps   [eax+edx], xmm0       {Fill 16 Bytes per Loop}
  add       edx, 16
  jl        @@Large
  ret
@@Small:
  test      edx, edx
  jle       @@Done
  mov       [eax+edx-1], cl       {Fill Last Byte}
  and       edx, -2               {No. of Words to Fill}
  neg       edx
  lea       edx, [@@SmallFill + 60 + edx * 2]
  jmp       edx
  nop                             {Align Jump Destinations}
  nop
@@SmallFill:
  mov       [eax+28], cx
  mov       [eax+26], cx
  mov       [eax+24], cx
  mov       [eax+22], cx
  mov       [eax+20], cx
  mov       [eax+18], cx
  mov       [eax+16], cx
  mov       [eax+14], cx
  mov       [eax+12], cx
  mov       [eax+10], cx
  mov       [eax+ 8], cx
  mov       [eax+ 6], cx
  mov       [eax+ 4], cx
  mov       [eax+ 2], cx
  mov       [eax   ], cx
  ret {DO NOT REMOVE - This is for Alignment}
@@Done:
end;

procedure FillChar_JOH_MMX_4_b(var Dest; count: Integer; Value: AnsiChar);
asm
  cmp       edx, 32
  mov       ch, cl                {Copy Value into both Bytes of CX}
  jl        @@Small
  movd      mm0, ecx
  mov       ecx, eax
  sub       edx, 16
  punpcklwd mm0, mm0
  punpckldq mm0, mm0              {Copy Value into all 8 Bytes of MM0}
  movq      [eax], mm0            {Fill First 8 Bytes}
  movq      [eax+edx], mm0        {Fill Last 16 Bytes}
  movq      [eax+edx+8], mm0
  add       eax, edx              {Qword align writes}
  and       ecx, 7
  sub       ecx, 8
  add       edx, ecx
  neg       edx
@@Loop:
  movq      [eax+edx], mm0        {Fill 16 Bytes per Loop}
  movq      [eax+edx+8], mm0
  add       edx, 16
  jl        @@Loop
  emms
  ret
@@Small:
  test      edx, edx
  jle       @@Done
  mov       [eax+edx-1], cl       {Fill Last Byte}
  and       edx, -2               {Byte Pairs to Fill}
  neg       edx
  lea       edx, [@@Fill + 60 + edx * 2]
  jmp       edx
@@Fill:
  mov       [eax+28], cx
  mov       [eax+26], cx
  mov       [eax+24], cx
  mov       [eax+22], cx
  mov       [eax+20], cx
  mov       [eax+18], cx
  mov       [eax+16], cx
  mov       [eax+14], cx
  mov       [eax+12], cx
  mov       [eax+10], cx
  mov       [eax+ 8], cx
  mov       [eax+ 6], cx
  mov       [eax+ 4], cx
  mov       [eax+ 2], cx
  mov       [eax   ], cx
  ret {DO NOT REMOVE - This is for Alignment}
@@Done:
end;

procedure FillChar_JOH_IA32_4_a(var Dest; Count: Integer; Value: Char);
asm
  cmp    edx, 32
  mov    ch, cl           {Copy Value into both Bytes of CX}
  jl     @@SmallFill
  push   edi
  movzx  edi, cx
  shl    ecx, 16
  or     ecx, edi         {All 4 Bytes = Value}
  sub    edx, 4
  mov    [eax], ecx       {Fill First 4 Bytes}
  mov    [eax+edx-4], ecx {Fill Last 8 Bytes}
  mov    [eax+edx], ecx
  add    eax, 4
  and    eax, -4          {DWORD Align}
  mov    edi, eax
  mov    eax, ecx
  shr    edx, 2
  mov    ecx, edx
  rep    stosd
  pop    edi
  ret
  nop                     {Align}
@@SmallFill:
  test   edx, edx
  jle    @@Done
  mov    [eax+edx-1], cl  {Fill Last Byte}
  and    edx, -2          {Byte Pairs to Fill}
  neg    edx
  lea    edx, [@@Fill + 60 + edx * 2]
  jmp    edx
  nop                     {Align Jump Destinations}
  nop
@@Fill:
  mov    [eax+28], cx
  mov    [eax+26], cx
  mov    [eax+24], cx
  mov    [eax+22], cx
  mov    [eax+20], cx
  mov    [eax+18], cx
  mov    [eax+16], cx
  mov    [eax+14], cx
  mov    [eax+12], cx
  mov    [eax+10], cx
  mov    [eax+ 8], cx
  mov    [eax+ 6], cx
  mov    [eax+ 4], cx
  mov    [eax+ 2], cx
  mov    [eax   ], cx
  ret {DO NOT REMOVE - This is for Alignment}
@@Done:
end;
{$ENDIF Use_FastCodeFillChar}

{$IF defined (PatchSystemMove) or defined(FAST_MOVE)} //set in Zeos.inc
(*
Copyright (c) 2005, John O'Harrow (john@almcrest.demon.co.uk)

This software is provided 'as-is', without any express or implied warranty.
In no event will the authors be held liable for any damages arising from the
use of this software.

Permission is granted to anyone to use this software for any purpose, including
commercial applications, and to alter it and redistribute it freely, subject to
the following restrictions:

1. The origin of this software must not be misrepresented; you must not claim
   that you wrote the original software. If you use this software in a product,
   an acknowledgment in the product documentation would be appreciated but is
   not required.

2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

3. This notice may not be removed or altered from any source distribution.

-------------------------------------------------------------------------------

Version: 3.03 : 21-DEC-2005

How to use:

  Include this unit in any uses clause - Thats It!

What is Does:

  This unit replaces all calls to system.move with calls to a faster move
  procedure (Up to 3 times faster).  The code will automatically detect and
  use MMX, SSE, SSE2 or SSE3 where available.

Version  Change
-------  ------
2.00     Updated all Moves to use Move_JOH_XXX_6 (better code alignment).
2.10     VirtualProtect modified to use PAGE_EXECUTE_READWRITE to prevent
         error under WinXP SP2 with AMD64 running DEP.
2.20     Added Check for "Already Patched" to prevent error when using.
         Packages in both DLL's and EXE.
2.30     PrefetchLimit initialization moved outside of ReplaceMove.
3.00     Updated all Moves to use latest Move_JOH_XXX_10 procedures and added
         SSE2 and SSE3 optimized moves.
         Extra VirtualProtect removed.  PatchSystemMove Directive added.
         Local Move Procedure added for direct calling of fastest Move.
3.01     {$D-} Added to prevent stepping into the Move BASM Code
         Badly Names PrefetchLimit renamed CacheLimit
3.02     Incorrect DB equivalents of lddqu instruction corrected
3.03     Compiler Directives for D2006 Added
*)

{-------------------------------------------------------------------------}
{Perform Forward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source+Count, EDX = Dest+Count.  Destroys ECX}
procedure SmallForwardMove_10;
asm
  jmp     dword ptr [@@FwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@FwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Fwd01, @@Fwd02, @@Fwd03, @@Fwd04, @@Fwd05, @@Fwd06, @@Fwd07, @@Fwd08
  dd      @@Fwd09, @@Fwd10, @@Fwd11, @@Fwd12, @@Fwd13, @@Fwd14, @@Fwd15, @@Fwd16
  dd      @@Fwd17, @@Fwd18, @@Fwd19, @@Fwd20, @@Fwd21, @@Fwd22, @@Fwd23, @@Fwd24
  dd      @@Fwd25, @@Fwd26, @@Fwd27, @@Fwd28, @@Fwd29, @@Fwd30, @@Fwd31, @@Fwd32
  dd      @@Fwd33, @@Fwd34, @@Fwd35, @@Fwd36
@@Fwd36:
  mov     ecx, [eax-36]
  mov     [edx-36], ecx
@@Fwd32:
  mov     ecx, [eax-32]
  mov     [edx-32], ecx
@@Fwd28:
  mov     ecx, [eax-28]
  mov     [edx-28], ecx
@@Fwd24:
  mov     ecx, [eax-24]
  mov     [edx-24], ecx
@@Fwd20:
  mov     ecx, [eax-20]
  mov     [edx-20], ecx
@@Fwd16:
  mov     ecx, [eax-16]
  mov     [edx-16], ecx
@@Fwd12:
  mov     ecx, [eax-12]
  mov     [edx-12], ecx
@@Fwd08:
  mov     ecx, [eax-8]
  mov     [edx-8], ecx
@@Fwd04:
  mov     ecx, [eax-4]
  mov     [edx-4], ecx
  ret
  nop
@@Fwd35:
  mov     ecx, [eax-35]
  mov     [edx-35], ecx
@@Fwd31:
  mov     ecx, [eax-31]
  mov     [edx-31], ecx
@@Fwd27:
  mov     ecx, [eax-27]
  mov     [edx-27], ecx
@@Fwd23:
  mov     ecx, [eax-23]
  mov     [edx-23], ecx
@@Fwd19:
  mov     ecx, [eax-19]
  mov     [edx-19], ecx
@@Fwd15:
  mov     ecx, [eax-15]
  mov     [edx-15], ecx
@@Fwd11:
  mov     ecx, [eax-11]
  mov     [edx-11], ecx
@@Fwd07:
  mov     ecx, [eax-7]
  mov     [edx-7], ecx
  mov     ecx, [eax-4]
  mov     [edx-4], ecx
  ret
  nop
@@Fwd03:
  movzx   ecx, word ptr [eax-3]
  mov     [edx-3], cx
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1], cl
  ret
@@Fwd34:
  mov     ecx, [eax-34]
  mov     [edx-34], ecx
@@Fwd30:
  mov     ecx, [eax-30]
  mov     [edx-30], ecx
@@Fwd26:
  mov     ecx, [eax-26]
  mov     [edx-26], ecx
@@Fwd22:
  mov     ecx, [eax-22]
  mov     [edx-22], ecx
@@Fwd18:
  mov     ecx, [eax-18]
  mov     [edx-18], ecx
@@Fwd14:
  mov     ecx, [eax-14]
  mov     [edx-14], ecx
@@Fwd10:
  mov     ecx, [eax-10]
  mov     [edx-10], ecx
@@Fwd06:
  mov     ecx, [eax-6]
  mov     [edx-6], ecx
@@Fwd02:
  movzx   ecx, word ptr [eax-2]
  mov     [edx-2], cx
  ret
  nop
  nop
  nop
@@Fwd33:
  mov     ecx, [eax-33]
  mov     [edx-33], ecx
@@Fwd29:
  mov     ecx, [eax-29]
  mov     [edx-29], ecx
@@Fwd25:
  mov     ecx, [eax-25]
  mov     [edx-25], ecx
@@Fwd21:
  mov     ecx, [eax-21]
  mov     [edx-21], ecx
@@Fwd17:
  mov     ecx, [eax-17]
  mov     [edx-17], ecx
@@Fwd13:
  mov     ecx, [eax-13]
  mov     [edx-13], ecx
@@Fwd09:
  mov     ecx, [eax-9]
  mov     [edx-9], ecx
@@Fwd05:
  mov     ecx, [eax-5]
  mov     [edx-5], ecx
@@Fwd01:
  movzx   ecx, byte ptr [eax-1]
  mov     [edx-1], cl
  ret
@@Done:
end; {SmallForwardMove}

{-------------------------------------------------------------------------}
{Perform Backward Move of 0..36 Bytes}
{On Entry, ECX = Count, EAX = Source, EDX = Dest.  Destroys ECX}
procedure SmallBackwardMove_10;
asm
  jmp     dword ptr [@@BwdJumpTable+ecx*4]
  nop {Align Jump Table}
@@BwdJumpTable:
  dd      @@Done {Removes need to test for zero size move}
  dd      @@Bwd01, @@Bwd02, @@Bwd03, @@Bwd04, @@Bwd05, @@Bwd06, @@Bwd07, @@Bwd08
  dd      @@Bwd09, @@Bwd10, @@Bwd11, @@Bwd12, @@Bwd13, @@Bwd14, @@Bwd15, @@Bwd16
  dd      @@Bwd17, @@Bwd18, @@Bwd19, @@Bwd20, @@Bwd21, @@Bwd22, @@Bwd23, @@Bwd24
  dd      @@Bwd25, @@Bwd26, @@Bwd27, @@Bwd28, @@Bwd29, @@Bwd30, @@Bwd31, @@Bwd32
  dd      @@Bwd33, @@Bwd34, @@Bwd35, @@Bwd36
@@Bwd36:
  mov     ecx, [eax+32]
  mov     [edx+32], ecx
@@Bwd32:
  mov     ecx, [eax+28]
  mov     [edx+28], ecx
@@Bwd28:
  mov     ecx, [eax+24]
  mov     [edx+24], ecx
@@Bwd24:
  mov     ecx, [eax+20]
  mov     [edx+20], ecx
@@Bwd20:
  mov     ecx, [eax+16]
  mov     [edx+16], ecx
@@Bwd16:
  mov     ecx, [eax+12]
  mov     [edx+12], ecx
@@Bwd12:
  mov     ecx, [eax+8]
  mov     [edx+8], ecx
@@Bwd08:
  mov     ecx, [eax+4]
  mov     [edx+4], ecx
@@Bwd04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
  nop
  nop
  nop
@@Bwd35:
  mov     ecx, [eax+31]
  mov     [edx+31], ecx
@@Bwd31:
  mov     ecx, [eax+27]
  mov     [edx+27], ecx
@@Bwd27:
  mov     ecx, [eax+23]
  mov     [edx+23], ecx
@@Bwd23:
  mov     ecx, [eax+19]
  mov     [edx+19], ecx
@@Bwd19:
  mov     ecx, [eax+15]
  mov     [edx+15], ecx
@@Bwd15:
  mov     ecx, [eax+11]
  mov     [edx+11], ecx
@@Bwd11:
  mov     ecx, [eax+7]
  mov     [edx+7], ecx
@@Bwd07:
  mov     ecx, [eax+3]
  mov     [edx+3], ecx
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
  nop
  nop
  nop
@@Bwd03:
  movzx   ecx, word ptr [eax+1]
  mov     [edx+1], cx
  movzx   ecx, byte ptr [eax]
  mov     [edx], cl
  ret
  nop
  nop
@@Bwd34:
  mov     ecx, [eax+30]
  mov     [edx+30], ecx
@@Bwd30:
  mov     ecx, [eax+26]
  mov     [edx+26], ecx
@@Bwd26:
  mov     ecx, [eax+22]
  mov     [edx+22], ecx
@@Bwd22:
  mov     ecx, [eax+18]
  mov     [edx+18], ecx
@@Bwd18:
  mov     ecx, [eax+14]
  mov     [edx+14], ecx
@@Bwd14:
  mov     ecx, [eax+10]
  mov     [edx+10], ecx
@@Bwd10:
  mov     ecx, [eax+6]
  mov     [edx+6], ecx
@@Bwd06:
  mov     ecx, [eax+2]
  mov     [edx+2], ecx
@@Bwd02:
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
  nop
@@Bwd33:
  mov     ecx, [eax+29]
  mov     [edx+29], ecx
@@Bwd29:
  mov     ecx, [eax+25]
  mov     [edx+25], ecx
@@Bwd25:
  mov     ecx, [eax+21]
  mov     [edx+21], ecx
@@Bwd21:
  mov     ecx, [eax+17]
  mov     [edx+17], ecx
@@Bwd17:
  mov     ecx, [eax+13]
  mov     [edx+13], ecx
@@Bwd13:
  mov     ecx, [eax+9]
  mov     [edx+9], ecx
@@Bwd09:
  mov     ecx, [eax+5]
  mov     [edx+5], ecx
@@Bwd05:
  mov     ecx, [eax+1]
  mov     [edx+1], ecx
@@Bwd01:
  movzx   ecx, byte ptr[eax]
  mov     [edx], cl
  ret
@@Done:
end; {SmallBackwardMove}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_IA32_10;
asm
  fild    qword ptr [eax] {First 8}
  lea     eax, [eax+ecx-8]
  lea     ecx, [edx+ecx-8]
  push    edx
  push    ecx
  fild    qword ptr [eax] {Last 8}
  neg     ecx {QWORD Align Writes}
  and     edx, -8
  lea     ecx, [ecx+edx+8]
  pop     edx
@@Loop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @@Loop
  pop     eax
  fistp   qword ptr [edx] {Last 8}
  fistp   qword ptr [eax] {First 8}
end; {Forwards_IA32}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_IA32_10;
asm
  sub     ecx, 8
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx {QWORD Align Writes}
  push    ecx
  and     ecx, -8
  sub     ecx, edx
@@Loop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @@Loop
  pop     eax
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [eax] {Last 8}
end; {Backwards_IA32}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_MMX_10;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE {Size at which using MMX becomes worthwhile}
  jl      Forwards_IA32_10
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  push    ebx
  mov     ebx, edx
  movq    mm0, [eax] {First 8 Bytes}
  add     eax, ecx {QWORD Align Writes}
  add     ecx, edx
  and     edx, -8
  add     edx, 40
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
  nop {Align Loop}
@@FwdLoopMMX:
  movq    mm1, [eax+ecx-32]
  movq    mm2, [eax+ecx-24]
  movq    mm3, [eax+ecx-16]
  movq    mm4, [eax+ecx- 8]
  movq    [edx+ecx-32], mm1
  movq    [edx+ecx-24], mm2
  movq    [edx+ecx-16], mm3
  movq    [edx+ecx- 8], mm4
  add     ecx, 32
  jle     @@FwdLoopMMX
  movq    [ebx], mm0 {First 8 Bytes}
  emms
  pop     ebx
  neg     ecx
  add     ecx, 32
  jmp     SmallForwardMove_10
  nop {Align Loop}
  nop
@@FwdLargeMove:
  push    ebx
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    SmallForwardMove_10
@@FwdAligned:
  mov     ecx, ebx
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    esi
  push    edi
  mov     esi, eax          {ESI = Source}
  mov     edi, edx          {EDI = Dest}
  mov     eax, ecx          {EAX = Count}
  and     eax, -64          {EAX = No of Bytes to Blocks Moves}
  and     ecx, $3F          {ECX = Remaining Bytes to Move (0..63)}
  add     esi, eax
  add     edi, eax
  neg     eax
@@MMXcopyloop:
  movq    mm0, [esi+eax   ]
  movq    mm1, [esi+eax+ 8]
  movq    mm2, [esi+eax+16]
  movq    mm3, [esi+eax+24]
  movq    mm4, [esi+eax+32]
  movq    mm5, [esi+eax+40]
  movq    mm6, [esi+eax+48]
  movq    mm7, [esi+eax+56]
  movq    [edi+eax   ], mm0
  movq    [edi+eax+ 8], mm1
  movq    [edi+eax+16], mm2
  movq    [edi+eax+24], mm3
  movq    [edi+eax+32], mm4
  movq    [edi+eax+40], mm5
  movq    [edi+eax+48], mm6
  movq    [edi+eax+56], mm7
  add     eax, 64
  jnz     @@MMXcopyloop
  emms                   {Empty MMX State}
  add     ecx, ebx
  shr     ecx, 2
  rep     movsd
  mov     ecx, ebx
  and     ecx, 3
  rep     movsb
  pop     edi
  pop     esi
  pop     ebx
end; {Forwards_MMX}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_MMX_10;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE {Size at which using MMX becomes worthwhile}
  jl      Backwards_IA32_10
  push    ebx
  movq    mm0, [eax+ecx-8] {Get Last QWORD}
  lea     ebx, [edx+ecx] {QWORD Align Writes}
  and     ebx, 7
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
@@BwdLoopMMX:
  movq    mm1, [eax+ecx   ]
  movq    mm2, [eax+ecx+ 8]
  movq    mm3, [eax+ecx+16]
  movq    mm4, [eax+ecx+24]
  movq    [edx+ecx+24], mm4
  movq    [edx+ecx+16], mm3
  movq    [edx+ecx+ 8], mm2
  movq    [edx+ecx   ], mm1
  sub     ecx, 32
  jge     @@BwdLoopMMX
  movq    [edx+ebx-8], mm0 {Last QWORD}
  emms
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove_10
end; {Backwards_MMX}

{-------------------------------------------------------------------------}
procedure LargeAlignedSSEMove;
asm
@@Loop:
  movaps  xmm0, [eax+ecx]
  movaps  xmm1, [eax+ecx+16]
  movaps  xmm2, [eax+ecx+32]
  movaps  xmm3, [eax+ecx+48]
  movaps  [edx+ecx], xmm0
  movaps  [edx+ecx+16], xmm1
  movaps  [edx+ecx+32], xmm2
  movaps  [edx+ecx+48], xmm3
  movaps  xmm4, [eax+ecx+64]
  movaps  xmm5, [eax+ecx+80]
  movaps  xmm6, [eax+ecx+96]
  movaps  xmm7, [eax+ecx+112]
  movaps  [edx+ecx+64], xmm4
  movaps  [edx+ecx+80], xmm5
  movaps  [edx+ecx+96], xmm6
  movaps  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeAlignedSSEMove}

{-------------------------------------------------------------------------}
procedure LargeUnalignedSSEMove;
asm
@@Loop:
  movups  xmm0, [eax+ecx]
  movups  xmm1, [eax+ecx+16]
  movups  xmm2, [eax+ecx+32]
  movups  xmm3, [eax+ecx+48]
  movaps  [edx+ecx], xmm0
  movaps  [edx+ecx+16], xmm1
  movaps  [edx+ecx+32], xmm2
  movaps  [edx+ecx+48], xmm3
  movups  xmm4, [eax+ecx+64]
  movups  xmm5, [eax+ecx+80]
  movups  xmm6, [eax+ecx+96]
  movups  xmm7, [eax+ecx+112]
  movaps  [edx+ecx+64], xmm4
  movaps  [edx+ecx+80], xmm5
  movaps  [edx+ecx+96], xmm6
  movaps  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeUnalignedSSEMove}

{-------------------------------------------------------------------------}
procedure HugeAlignedSSEMove;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movaps  xmm0, [eax+ecx]
  movaps  xmm1, [eax+ecx+16]
  movaps  xmm2, [eax+ecx+32]
  movaps  xmm3, [eax+ecx+48]
  movntps [edx+ecx], xmm0
  movntps [edx+ecx+16], xmm1
  movntps [edx+ecx+32], xmm2
  movntps [edx+ecx+48], xmm3
  movaps  xmm4, [eax+ecx+64]
  movaps  xmm5, [eax+ecx+80]
  movaps  xmm6, [eax+ecx+96]
  movaps  xmm7, [eax+ecx+112]
  movntps [edx+ecx+64], xmm4
  movntps [edx+ecx+80], xmm5
  movntps [edx+ecx+96], xmm6
  movntps [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeAlignedSSEMove}

{-------------------------------------------------------------------------}
procedure HugeUnalignedSSEMove;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movups  xmm0, [eax+ecx]
  movups  xmm1, [eax+ecx+16]
  movups  xmm2, [eax+ecx+32]
  movups  xmm3, [eax+ecx+48]
  movntps [edx+ecx], xmm0
  movntps [edx+ecx+16], xmm1
  movntps [edx+ecx+32], xmm2
  movntps [edx+ecx+48], xmm3
  movups  xmm4, [eax+ecx+64]
  movups  xmm5, [eax+ecx+80]
  movups  xmm6, [eax+ecx+96]
  movups  xmm7, [eax+ecx+112]
  movntps [edx+ecx+64], xmm4
  movntps [edx+ecx+80], xmm5
  movntps [edx+ecx+96], xmm6
  movntps [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeUnalignedSSEMove}

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure LargeSSEMove(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  neg     ecx
  cmp     ecx, CacheLimit       {Count > Limit - Use Prefetch}
  jl      @@Huge
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@LargeUnaligned
  call    LargeAlignedSSEMove   {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    LargeUnalignedSSEMove
  jmp     @@Remainder
@@Huge:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@HugeUnaligned
  call    HugeAlignedSSEMove    {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@HugeUnaligned:                {Source Not 16-Byte Aligned}
  call    HugeUnalignedSSEMove
@@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
  movups  xmm0, [eax+ebx]
  movaps  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end; {LargeSSEMove}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_SSE_10;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE
  jle     Forwards_IA32_10
  push    ebx
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  movups  xmm0, [eax] {First 16 Bytes}
  mov     ebx, edx
  add     eax, ecx {Align Writes}
  add     ecx, edx
  and     edx, -16
  add     edx, 48
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
  nop {Align Loop}
@@FwdLoopSSE:
  movups  xmm1, [eax+ecx-32]
  movups  xmm2, [eax+ecx-16]
  movaps  [edx+ecx-32], xmm1
  movaps  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @@FwdLoopSSE
  movups  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     SmallForwardMove_10
@@FwdLargeMove:
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdLargeAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    SmallForwardMove_10
  mov     ecx, ebx
@@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    LargeSSEMove
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove_10
end; {Forwards_SSE}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_SSE_10;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE
  jle     Backwards_IA32_10
  push    ebx
  movups  xmm0, [eax+ecx-16] {Last 16 Bytes}
  lea     ebx, [edx+ecx] {Align Writes}
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
@@BwdLoop:
  movups  xmm1, [eax+ecx]
  movups  xmm2, [eax+ecx+16]
  movaps  [edx+ecx], xmm1
  movaps  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @@BwdLoop
  movups  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove_10
end; {Backwards_SSE}

{-------------------------------------------------------------------------}
procedure LargeAlignedSSE2Move; {Also used in SSE3 Move}
asm
@@Loop:
  movdqa  xmm0, [eax+ecx]
  movdqa  xmm1, [eax+ecx+16]
  movdqa  xmm2, [eax+ecx+32]
  movdqa  xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], xmm0
  movdqa  [edx+ecx+16], xmm1
  movdqa  [edx+ecx+32], xmm2
  movdqa  [edx+ecx+48], xmm3
  movdqa  xmm4, [eax+ecx+64]
  movdqa  xmm5, [eax+ecx+80]
  movdqa  xmm6, [eax+ecx+96]
  movdqa  xmm7, [eax+ecx+112]
  movdqa  [edx+ecx+64], xmm4
  movdqa  [edx+ecx+80], xmm5
  movdqa  [edx+ecx+96], xmm6
  movdqa  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeAlignedSSE2Move}

{-------------------------------------------------------------------------}
procedure LargeUnalignedSSE2Move;
asm
@@Loop:
  movdqu  xmm0, [eax+ecx]
  movdqu  xmm1, [eax+ecx+16]
  movdqu  xmm2, [eax+ecx+32]
  movdqu  xmm3, [eax+ecx+48]
  movdqa  [edx+ecx], xmm0
  movdqa  [edx+ecx+16], xmm1
  movdqa  [edx+ecx+32], xmm2
  movdqa  [edx+ecx+48], xmm3
  movdqu  xmm4, [eax+ecx+64]
  movdqu  xmm5, [eax+ecx+80]
  movdqu  xmm6, [eax+ecx+96]
  movdqu  xmm7, [eax+ecx+112]
  movdqa  [edx+ecx+64], xmm4
  movdqa  [edx+ecx+80], xmm5
  movdqa  [edx+ecx+96], xmm6
  movdqa  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeUnalignedSSE2Move}

{-------------------------------------------------------------------------}
procedure HugeAlignedSSE2Move; {Also used in SSE3 Move}
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movdqa  xmm0, [eax+ecx]
  movdqa  xmm1, [eax+ecx+16]
  movdqa  xmm2, [eax+ecx+32]
  movdqa  xmm3, [eax+ecx+48]
  movntdq [edx+ecx], xmm0
  movntdq [edx+ecx+16], xmm1
  movntdq [edx+ecx+32], xmm2
  movntdq [edx+ecx+48], xmm3
  movdqa  xmm4, [eax+ecx+64]
  movdqa  xmm5, [eax+ecx+80]
  movdqa  xmm6, [eax+ecx+96]
  movdqa  xmm7, [eax+ecx+112]
  movntdq [edx+ecx+64], xmm4
  movntdq [edx+ecx+80], xmm5
  movntdq [edx+ecx+96], xmm6
  movntdq [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeAlignedSSE2Move}

{-------------------------------------------------------------------------}
procedure HugeUnalignedSSE2Move;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
  movdqu  xmm0, [eax+ecx]
  movdqu  xmm1, [eax+ecx+16]
  movdqu  xmm2, [eax+ecx+32]
  movdqu  xmm3, [eax+ecx+48]
  movntdq [edx+ecx], xmm0
  movntdq [edx+ecx+16], xmm1
  movntdq [edx+ecx+32], xmm2
  movntdq [edx+ecx+48], xmm3
  movdqu  xmm4, [eax+ecx+64]
  movdqu  xmm5, [eax+ecx+80]
  movdqu  xmm6, [eax+ecx+96]
  movdqu  xmm7, [eax+ecx+112]
  movntdq [edx+ecx+64], xmm4
  movntdq [edx+ecx+80], xmm5
  movntdq [edx+ecx+96], xmm6
  movntdq [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeUnalignedSSE2Move}

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure LargeSSE2Move(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  neg     ecx
  cmp     ecx, CacheLimit       {Count > Limit - Use Prefetch}
  jl      @@Huge
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@LargeUnaligned
  call    LargeAlignedSSE2Move  {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    LargeUnalignedSSE2Move
  jmp     @@Remainder
@@Huge:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@HugeUnaligned
  call    HugeAlignedSSE2Move   {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@HugeUnaligned:                {Source Not 16-Byte Aligned}
  call    HugeUnalignedSSE2Move
@@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
  movdqu  xmm0, [eax+ebx]
  movdqa  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end; {LargeSSE2Move}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_SSE2_10;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE
  jle     Forwards_IA32_10
  push    ebx
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
  movdqu  xmm0, [eax] {First 16 Bytes}
  mov     ebx, edx
  add     eax, ecx {Align Writes}
  add     ecx, edx
  and     edx, -16
  add     edx, 48
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
@@FwdLoopSSE2:
  movdqu  xmm1, [eax+ecx-32]
  movdqu  xmm2, [eax+ecx-16]
  movdqa  [edx+ecx-32], xmm1
  movdqa  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @@FwdLoopSSE2
  movdqu  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     SmallForwardMove_10
@@FwdLargeMove:
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdLargeAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    SmallForwardMove_10
  mov     ecx, ebx
@@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    LargeSSE2Move
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove_10
end; {Forwards_SSE2}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_SSE2_10;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE
  jle     Backwards_IA32_10
  push    ebx
  movdqu  xmm0, [eax+ecx-16] {Last 16 Bytes}
  lea     ebx, [edx+ecx] {Align Writes}
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
  add     edi, 0 {3-Byte NOP Equivalent to Align Loop}
@@BwdLoop:
  movdqu  xmm1, [eax+ecx]
  movdqu  xmm2, [eax+ecx+16]
  movdqa  [edx+ecx], xmm1
  movdqa  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @@BwdLoop
  movdqu  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove_10
end; {Backwards_SSE2}

{-------------------------------------------------------------------------}
procedure LargeUnalignedSSE3Move;
asm
@@Loop:
{$IFDEF SSE2}
  lddqu   xmm0, [eax+ecx]
  lddqu   xmm1, [eax+ecx+16]
  lddqu   xmm2, [eax+ecx+32]
  lddqu   xmm3, [eax+ecx+48]
{$ELSE}
  DB      $F2,$0F,$F0,$04,$01
  DB      $F2,$0F,$F0,$4C,$01,$10
  DB      $F2,$0F,$F0,$54,$01,$20
  DB      $F2,$0F,$F0,$5C,$01,$30
{$ENDIF}
  movdqa  [edx+ecx], xmm0
  movdqa  [edx+ecx+16], xmm1
  movdqa  [edx+ecx+32], xmm2
  movdqa  [edx+ecx+48], xmm3
{$IFDEF SSE2}
  lddqu   xmm4, [eax+ecx+64]
  lddqu   xmm5, [eax+ecx+80]
  lddqu   xmm6, [eax+ecx+96]
  lddqu   xmm7, [eax+ecx+112]
{$ELSE}
  DB      $F2,$0F,$F0,$64,$01,$40
  DB      $F2,$0F,$F0,$6C,$01,$50
  DB      $F2,$0F,$F0,$74,$01,$60
  DB      $F2,$0F,$F0,$7C,$01,$70
{$ENDIF}
  movdqa  [edx+ecx+64], xmm4
  movdqa  [edx+ecx+80], xmm5
  movdqa  [edx+ecx+96], xmm6
  movdqa  [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
end; {LargeUnalignedSSE3Move}

{-------------------------------------------------------------------------}
procedure HugeUnalignedSSE3Move;
const
  Prefetch = 512;
asm
@@Loop:
  prefetchnta [eax+ecx+Prefetch]
  prefetchnta [eax+ecx+Prefetch+64]
{$IFDEF SSE2}
  lddqu   xmm0, [eax+ecx]
  lddqu   xmm1, [eax+ecx+16]
  lddqu   xmm2, [eax+ecx+32]
  lddqu   xmm3, [eax+ecx+48]
{$ELSE}
  DB      $F2,$0F,$F0,$04,$01
  DB      $F2,$0F,$F0,$4C,$01,$10
  DB      $F2,$0F,$F0,$54,$01,$20
  DB      $F2,$0F,$F0,$5C,$01,$30
{$ENDIF}
  movntdq [edx+ecx], xmm0
  movntdq [edx+ecx+16], xmm1
  movntdq [edx+ecx+32], xmm2
  movntdq [edx+ecx+48], xmm3
{$IFDEF SSE2}
  lddqu   xmm4, [eax+ecx+64]
  lddqu   xmm5, [eax+ecx+80]
  lddqu   xmm6, [eax+ecx+96]
  lddqu   xmm7, [eax+ecx+112]
{$ELSE}
  DB      $F2,$0F,$F0,$64,$01,$40
  DB      $F2,$0F,$F0,$6C,$01,$50
  DB      $F2,$0F,$F0,$74,$01,$60
  DB      $F2,$0F,$F0,$7C,$01,$70
{$ENDIF}
  movntdq [edx+ecx+64], xmm4
  movntdq [edx+ecx+80], xmm5
  movntdq [edx+ecx+96], xmm6
  movntdq [edx+ecx+112], xmm7
  add     ecx, 128
  js      @@Loop
  sfence
end; {HugeUnalignedSSE3Move}

{-------------------------------------------------------------------------}
{Dest MUST be 16-Byes Aligned, Count MUST be multiple of 16 }
procedure LargeSSE3Move(const Source; var Dest; Count: Integer);
asm
  push    ebx
  mov     ebx, ecx
  and     ecx, -128             {No of Bytes to Block Move (Multiple of 128)}
  add     eax, ecx              {End of Source Blocks}
  add     edx, ecx              {End of Dest Blocks}
  neg     ecx
  cmp     ecx, CacheLimit       {Count > Limit - Use Prefetch}
  jl      @@Huge
  test    eax, 15               {Check if Both Source/Dest are Aligned}
  jnz     @@LargeUnaligned
  call    LargeAlignedSSE2Move  {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@LargeUnaligned:               {Source Not 16-Byte Aligned}
  call    LargeUnalignedSSE3Move
  jmp     @@Remainder
@@Huge:
  test    eax, 15               {Check if Both Source/Dest Aligned}
  jnz     @@HugeUnaligned
  call    HugeAlignedSSE2Move   {Both Source and Dest 16-Byte Aligned}
  jmp     @@Remainder
@@HugeUnaligned:                {Source Not 16-Byte Aligned}
  call    HugeUnalignedSSE3Move
@@Remainder:
  and     ebx, $7F              {Remainder (0..112 - Multiple of 16)}
  jz      @@Done
  add     eax, ebx
  add     edx, ebx
  neg     ebx
@@RemainderLoop:
{$IFDEF SSE2}
  lddqu   xmm0, [eax+ebx]
{$ELSE}
  DB      $F2,$0F,$F0,$04,$03
{$ENDIF}
  movdqa  [edx+ebx], xmm0
  add     ebx, 16
  jnz     @@RemainderLoop
@@Done:
  pop     ebx
end; {LargeSSE3Move}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX > EDX and ECX > 36 (TINYSIZE)}
procedure Forwards_SSE3_10;
const
  SMALLSIZE = 64;
  LARGESIZE = 2048;
asm
  cmp     ecx, SMALLSIZE
  jle     Forwards_IA32_10
  push    ebx
  cmp     ecx, LARGESIZE
  jge     @@FwdLargeMove
{$IFDEF SSE2}
  lddqu   xmm0, [eax] {First 16 Bytes}
{$ELSE}
  DB      $F2,$0F,$F0,$00
{$ENDIF}
  mov     ebx, edx
  add     eax, ecx {Align Writes}
  add     ecx, edx
  and     edx, -16
  add     edx, 48
  sub     ecx, edx
  add     edx, ecx
  neg     ecx
@@FwdLoopSSE3:
{$IFDEF SSE2}
  lddqu   xmm1, [eax+ecx-32]
  lddqu   xmm2, [eax+ecx-16]
{$ELSE}
  DB      $F2,$0F,$F0,$4C,$01,$E0
  DB      $F2,$0F,$F0,$54,$01,$F0
{$ENDIF}
  movdqa  [edx+ecx-32], xmm1
  movdqa  [edx+ecx-16], xmm2
  add     ecx, 32
  jle     @@FwdLoopSSE3
  movdqu  [ebx], xmm0 {First 16 Bytes}
  neg     ecx
  add     ecx, 32
  pop     ebx
  jmp     SmallForwardMove_10
@@FwdLargeMove:
  mov     ebx, ecx
  test    edx, 15
  jz      @@FwdLargeAligned
  lea     ecx, [edx+15] {16 byte Align Destination}
  and     ecx, -16
  sub     ecx, edx
  add     eax, ecx
  add     edx, ecx
  sub     ebx, ecx
  call    SmallForwardMove_10
  mov     ecx, ebx
@@FwdLargeAligned:
  and     ecx, -16
  sub     ebx, ecx {EBX = Remainder}
  push    edx
  push    eax
  push    ecx
  call    LargeSSE3Move
  pop     ecx
  pop     eax
  pop     edx
  add     ecx, ebx
  add     eax, ecx
  add     edx, ecx
  mov     ecx, ebx
  pop     ebx
  jmp     SmallForwardMove_10
end; {Forwards_SSE3}

{-------------------------------------------------------------------------}
{Move ECX Bytes from EAX to EDX, where EAX < EDX and ECX > 36 (TINYSIZE)}
procedure Backwards_SSE3_10;
const
  SMALLSIZE = 64;
asm
  cmp     ecx, SMALLSIZE
  jle     Backwards_IA32_10
  push    ebx
{$IFDEF SSE2}
  lddqu   xmm0, [eax+ecx-16] {Last 16 Bytes}
{$ELSE}
  DB      $F2,$0F,$F0,$44,$01,$F0
{$ENDIF}
  lea     ebx, [edx+ecx] {Align Writes}
  and     ebx, 15
  sub     ecx, ebx
  add     ebx, ecx
  sub     ecx, 32
  add     edi, 0 {3-Byte NOP Equivalent to Align Loop}
@@BwdLoop:
{$IFDEF SSE2}
  lddqu   xmm1, [eax+ecx]
  lddqu   xmm2, [eax+ecx+16]
{$ELSE}
  DB      $F2,$0F,$F0,$0C,$01
  DB      $F2,$0F,$F0,$54,$01,$10
{$ENDIF}
  movdqa  [edx+ecx], xmm1
  movdqa  [edx+ecx+16], xmm2
  sub     ecx, 32
  jge     @@BwdLoop
  movdqu  [edx+ebx-16], xmm0  {Last 16 Bytes}
  add     ecx, 32
  pop     ebx
  jmp     SmallBackwardMove_10
end; {Backwards_SSE3}

{-------------------------------------------------------------------------}
{Move using IA32 Instruction Set Only}
procedure Move_JOH_IA32_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_10
@@SmallCheck:
  jne     SmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_IA32_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_IA32_10
  jmp     Backwards_IA32_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_IA32}

{-------------------------------------------------------------------------}
{Move using MMX Instruction Set}
procedure Move_JOH_MMX_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_10
@@SmallCheck:
  jne     SmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_MMX_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_MMX_10
  jmp     Backwards_MMX_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_MMX}

{-------------------------------------------------------------------------}
{Move using SSE Instruction Set}
procedure Move_JOH_SSE_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_10
@@SmallCheck:
  jne     SmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_SSE_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE_10
  jmp     Backwards_SSE_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_SSE}

{-------------------------------------------------------------------------}
{Move using SSE2 Instruction Set}
procedure Move_JOH_SSE2_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_10
@@SmallCheck:
  jne     SmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_SSE2_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE2_10
  jmp     Backwards_SSE2_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_SSE2}

{-------------------------------------------------------------------------}
{Move using SSE3 Instruction Set}
procedure Move_JOH_SSE3_10(const Source; var Dest; Count : Integer);
asm
  cmp     ecx, TINYSIZE
  ja      @@Large {Count > TINYSIZE or Count < 0}
  cmp     eax, edx
  jbe     @@SmallCheck
  add     eax, ecx
  add     edx, ecx
  jmp     SmallForwardMove_10
@@SmallCheck:
  jne     SmallBackwardMove_10
  ret {For Compatibility with Delphi's move for Source = Dest}
@@Large:
  jng     @@Done {For Compatibility with Delphi's move for Count < 0}
  cmp     eax, edx
  ja      Forwards_SSE3_10
  je      @@Done {For Compatibility with Delphi's move for Source = Dest}
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     Forwards_SSE3_10
  jmp     Backwards_SSE3_10 {Source/Dest Overlap}
@@Done:
end; {MoveJOH_SSE3}

{-------------------------------------------------------------------------}
{$IFDEF PatchSystemMove}
procedure PatchMove; {Overwrite System.Move with Main Procedure of New Move}
const
  JumpFarId = $E9;
  JumpPtrId = $25FF;
type {Jump Positions in Move Prodedures Above - Really Horrible but it Works}
  NewMoveType = packed record {Size = 56 Bytes, System.Move Size = 64 Bytes}
    Padding1  : array[1..14] of Byte;
    Jump1Dest : Integer; {jmp SmallForwardMove}
    Padding2  : array[1.. 2] of Byte;
    Jump2Dest : Integer; {jmp SmallBackwardMove}
    Padding3  : array[1.. 7] of Byte;
    Jump3Dest : Integer; {jg  Forwards_XXX}
    Padding4  : array[1..11] of Byte;
    Jump4Dest : Integer; {jg  Backwards_XXX}
    Padding5  : array[1.. 1] of Byte;
    Jump5Dest : Integer; {jmp Forwards_XXX}
    Padding6  : array[1.. 1] of Byte;
  end;
var
  I, Offset  : Integer;
  Src, Dest  : PByteArray;
  NewMove    : NewMoveType;
  Protect,
  OldProtect : DWORD;
begin
  VirtualProtect(@System.Move, 256, PAGE_EXECUTE_READWRITE, @OldProtect);
  if PByte(@System.Move)^ <> JumpFarId then {Check if Already Patched}
    if PWord(@System.Move)^ = JumpPtrId then
      begin {System.Move Starts JMP DWORD PTR [XXXXXXXX] (ie. Using Packages)}
        PByte(@System.Move)^ := JumpFarId;
        PInteger(Integer(@System.Move)+1)^ :=
          Integer(@Move) - Integer(@System.Move)-5; {Change Destination}
      end
    else
      begin {Patch System.Move.  Adjust Jump Destinations in Copied Procedure}
        Move(Pointer(@Move)^, NewMove, SizeOf(NewMove));
        Offset := Integer(@Move) - Integer(@System.Move);
        Inc(NewMove.Jump1Dest, Offset);
        Inc(NewMove.Jump2Dest, Offset);
        Inc(NewMove.Jump3Dest, Offset);
        Inc(NewMove.Jump4Dest, Offset);
        Inc(NewMove.Jump5Dest, Offset);
        Src  := @NewMove;
        Dest := @System.Move;
        for I := 0 to SizeOf(NewMove) - 1 do
          Dest[I] := Src[I]; {Overwrite System.Move}
      end;
  VirtualProtect(@System.Move, 256, OldProtect, @Protect);
  FlushInstructionCache(GetCurrentProcess, @System.Move, SizeOf(NewMove));
end; {PatchMove}
{$ENDIF PatchSystemMove}
{$IFEND} //set in Zeos.inc

function IntToRaw(Value: Cardinal): RawByteString;
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  ZSetString(nil, Digits, Result);
  IntToRaw(Value, Pointer(Result), Digits);
end;

{$IF defined(Delphi) and defined(WIN32)}
function IntToRaw(Value: Integer): RawByteString;
//function IntToStr32_JOH_IA32_6_d(Value: Integer): string;
asm
  push   ebx
  push   edi
  push   esi
  mov    ebx, eax                {Value}
  sar    ebx, 31                 {0 for +ve Value or -1 for -ve Value}
  xor    eax, ebx
  sub    eax, ebx                {ABS(Value)}
  mov    esi, 10                 {Max Digits in Result}
  mov    edi, edx                {@Result}
  cmp    eax, 10
  sbb    esi, 0
  cmp    eax, 100
  sbb    esi, 0
  cmp    eax, 1000
  sbb    esi, 0
  cmp    eax, 10000
  sbb    esi, 0
  cmp    eax, 100000
  sbb    esi, 0
  cmp    eax, 1000000
  sbb    esi, 0
  cmp    eax, 10000000
  sbb    esi, 0
  cmp    eax, 100000000
  sbb    esi, 0
  cmp    eax, 1000000000
  sbb    esi, ebx                {Digits (Including Sign Character)}
  mov    ecx, [edx]              {Result}
  test   ecx, ecx
  je     @@NewStr                {Create New String for Result}
  cmp    dword ptr [ecx-8], 1
  jne    @@ChangeStr             {Reference Count <> 1}
  cmp    esi, [ecx-4]
  je     @@LengthOk              {Existing Length = Required Length}
  sub    ecx, TStrRecordSize     {Allocation Address}
  push   eax                     {ABS(Value)}
  push   ecx
  mov    eax, esp
  lea    edx, [esi+TStrRecordSize+1] {New Allocation Size}
  call   system.@ReallocMem      {Reallocate Result String}
  pop    ecx
  pop    eax                     {ABS(Value)}
  add    ecx, TStrRecordSize     {Result}
  mov    [ecx-4], esi            {Set New Length}
  mov    byte ptr [ecx+esi], 0   {Add Null Terminator}
  mov    [edi], ecx              {Set Result Address}
  jmp    @@LengthOk
@@ChangeStr:
  mov     edx, dword ptr [ecx-8]  {Reference Count}
  add     edx, 1
  jz      @@NewStr                {RefCount = -1 (String Constant)}
  lock    dec dword ptr [ecx-8]   {Decrement Existing Reference Count}
@@NewStr:
  push   eax                     {ABS(Value)}
  mov    eax, esi                {Length}
{$IFDEF WITH_RAWBYTESTRING}
  mov    edx, 20127          // ASCII7 code page for Delphi Unicode and FPC2.7+ otherwise UnicodeString() convesrions fail
{$ENDIF}
  call   system.@NewAnsiString
  mov    [edi], eax              {Set Result Address}
  mov    ecx, eax                {Result}
  pop    eax                     {ABS(Value)}
@@LengthOk:
  mov    byte ptr [ecx], '-'     {Store '-' Character (May be Overwritten)}
  add    esi, ebx                {Digits (Excluding Sign Character)}
  sub    ecx, ebx                {Destination of 1st Digit}
  sub    esi, 2                  {Digits (Excluding Sign Character) - 2}
  jle    @@FinalDigits           {1 or 2 Digit Value}
  cmp    esi, 8                  {10 Digit Value?}
  jne    @@SetResult             {Not a 10 Digit Value}
  sub    eax, 2000000000         {Digit 10 must be either '1' or '2'}
  mov    dl, '2'
  jnc    @@SetDigit10            {Digit 10 = '2'}
  mov    dl, '1'                 {Digit 10 = '1'}
  add    eax, 1000000000
@@SetDigit10:
  mov    [ecx], dl               {Save Digit 10}
  mov    esi, 7                  {9 Digits Remaining}
  add    ecx, 1                  {Destination of 2nd Digit}
@@SetResult:
  mov    edi, $28F5C29           {((2^32)+100-1)/100}
@@Loop:
  mov    ebx, eax                {Dividend}
  mul    edi                     {EDX = Dividend DIV 100}
  mov    eax, edx                {Set Next Dividend}
  imul   edx, -200               {-2 * (100 * Dividend DIV  100)}
  movzx  edx, word ptr [TwoDigitLookupRaw+ebx*2+edx] {Dividend MOD 100 in ASCII}
  mov    [ecx+esi], dx
  sub    esi, 2
  jg     @@Loop                  {Loop Until 1 or 2 Digits Remaining}
@@FinalDigits:
  pop    esi
  pop    edi
  pop    ebx
  jnz    @@LastDigit
  movzx  eax, word ptr [TwoDigitLookupRaw+eax*2]
  mov    [ecx], ax               {Save Final 2 Digits}
  ret
@@LastDigit:
  or     al , '0'                {Ascii Adjustment}
  mov    [ecx], al               {Save Final Digit}
end;

function IntToRaw(Value: Int64): RawByteString;
//function IntToStr64_JOH_IA32_6_d(Value: Int64): string;
asm
  push   ebx
  mov    ecx, [ebp+8]            {Low Integer of Value}
  mov    edx, [ebp+12]           {High Integer of Value}
  xor    ebp, ebp                {Clear Sign Flag (EBP Already Pushed)}
  mov    ebx, ecx                {Low Integer of Value}
  test   edx, edx
  jnl    @@AbsValue
  mov    ebp, 1                  {EBP = 1 for -ve Value or 0 for +ve Value}
  neg    ecx
  adc    edx, 0
  neg    edx
@@AbsValue:                      {EDX:ECX = Abs(Value)}
  jnz    @@Large
  test   ecx, ecx
  js     @@Large
  mov    edx, eax                {@Result}
  mov    eax, ebx                {Low Integer of Value}
  //call   IntToStr32_JOH_IA32_6_d {Call Fastest Integer IntToStr Function}
  call   IntToRaw {Call Fastest Integer IntToStr Function}
  pop    ebx
@@Exit:
  pop    ebp                     {Restore Stack and Exit}
  ret    8
@@Large:
  push   edi
  push   esi
  mov    edi, eax
  xor    ebx, ebx
  xor    eax, eax
@@Test15:                        {Test for 15 or More Digits}
  cmp    edx, $00005af3          {100000000000000 div $100000000}
  jne    @@Check15
  cmp    ecx, $107a4000          {100000000000000 mod $100000000}
@@Check15:
  jb     @@Test13
@@Test17:                        {Test for 17 or More Digits}
  cmp    edx, $002386f2          {10000000000000000 div $100000000}
  jne    @@Check17
  cmp    ecx, $6fc10000          {10000000000000000 mod $100000000}
@@Check17:
  jb     @@Test15or16
@@Test19:                        {Test for 19 Digits}
  cmp    edx, $0de0b6b3          {1000000000000000000 div $100000000}
  jne    @@Check19
  cmp    ecx, $a7640000          {1000000000000000000 mod $100000000}
@@Check19:
  jb     @@Test17or18
  mov    al, 19
  jmp    @@SetLength
@@Test17or18:                    {17 or 18 Digits}
  mov    bl, 18
  cmp    edx, $01634578          {100000000000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $5d8a0000          {100000000000000000 mod $100000000}
  jmp    @@SetLen
@@Test15or16:                    {15 or 16 Digits}
  mov    bl, 16
  cmp    edx, $00038d7e          {1000000000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $a4c68000          {1000000000000000 mod $100000000}
  jmp    @@SetLen
@@Test13:                        {Test for 13 or More Digits}
  cmp    edx, $000000e8          {1000000000000 div $100000000}
  jne    @@Check13
  cmp    ecx, $d4a51000          {1000000000000 mod $100000000}
@@Check13:
  jb     @@Test11
@@Test13or14:                    {13 or 14 Digits}
  mov    bl, 14
  cmp    edx, $00000918          {10000000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $4e72a000          {10000000000000 mod $100000000}
  jmp    @@SetLen
@@Test11:                        {10, 11 or 12 Digits}
  cmp    edx, $02                {10000000000 div $100000000}
  jne    @@Check11
  cmp    ecx, $540be400          {10000000000 mod $100000000}
@@Check11:
  mov    bl, 11
  jb     @@SetLen                {10 Digits}
@@Test11or12:                    {11 or 12 Digits}
  mov    bl, 12
  cmp    edx, $17                {100000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $4876e800          {100000000000 mod $100000000}
@@SetLen:
  sbb    eax, 0                  {Adjust for Odd/Evem Digit Count}
  add    eax, ebx
@@SetLength:                     {Abs(Value) in EDX:ECX, Digits in EAX}
  push   ecx                     {Save Abs(Value)}
  push   edx
  lea    edx, [eax+ebp]          {Digits Needed (Including Sign Character)}
  mov    ecx, [edi]              {@Result}
  mov    esi, edx                {Digits Needed (Including Sign Character)}
  test   ecx, ecx
  je     @@NewStr                {Create New String for Result}
  cmp    dword ptr [ecx-8], 1
  jne    @@ChangeStr             {Reference Count <> 1}
  cmp    esi, [ecx-4]
  je     @@LengthOk              {Existing Length = Required Length}
  sub    ecx, TStrRecordSize     {Allocation Address}
  push   eax                     {ABS(Value)}
  push   ecx
  mov    eax, esp
  lea    edx, [esi+TStrRecordSize+1]{New Allocation Size}
  call   system.@ReallocMem      {Reallocate Result String}
  pop    ecx
  pop    eax                     {ABS(Value)}
  add    ecx, TStrRecordSize     {@Result}
  mov    [ecx-4], esi            {Set New Length}
  mov    byte ptr [ecx+esi], 0   {Add Null Terminator}
  mov    [edi], ecx              {Set Result Address}
  jmp    @@LengthOk
@@ChangeStr:
  mov     edx, dword ptr [ecx-8]  {Reference Count}
  add     edx, 1
  jz      @@NewStr                {RefCount = -1 (String Constant)}
  lock    dec dword ptr [ecx-8]   {Decrement Existing Reference Count}
@@NewStr:
  push   eax                     {ABS(Value)}
  mov    eax, esi                {Length}
{$IFDEF WITH_RAWBYTESTRING}
  mov    edx, 20127          // ASCII7 code page for Delphi Unicode and FPC2.7+ otherwise UnicodeString() convesrions fail
{$ENDIF}
  call   system.@NewAnsiString
  mov    [edi], eax              {Set Result Address}
  mov    ecx, eax                {@Result}
  pop    eax                     {ABS(Value)}
@@LengthOk:
  mov    edi, [edi]              {@Result}
  sub    esi, ebp                {Digits Needed (Excluding Sign Character)}
  mov    byte ptr [edi], '-'     {Store '-' Character (May be Overwritten)}
  add    edi, ebp                {Destination of 1st Digit}
  pop    edx                     {Restore Abs(Value)}
  pop    eax
  cmp    esi, 17
  jl     @@LessThan17Digits      {Digits < 17}
  je     @@SetDigit17            {Digits = 17}
  cmp    esi, 18
  je     @@SetDigit18            {Digits = 18}
  mov    cl, '0' - 1
  mov    ebx, $a7640000          {1000000000000000000 mod $100000000}
  mov    ebp, $0de0b6b3          {1000000000000000000 div $100000000}
@@CalcDigit19:
  add    ecx, 1
  sub    eax, ebx
  sbb    edx, ebp
  jnc    @@CalcDigit19
  add    eax, ebx
  adc    edx, ebp
  mov    [edi], cl
  add    edi, 1
@@SetDigit18:
  mov    cl, '0' - 1
  mov    ebx, $5d8a0000          {100000000000000000 mod $100000000}
  mov    ebp, $01634578          {100000000000000000 div $100000000}
@@CalcDigit18:
  add    ecx, 1
  sub    eax, ebx
  sbb    edx, ebp
  jnc    @@CalcDigit18
  add    eax, ebx
  adc    edx, ebp
  mov    [edi], cl
  add    edi, 1
@@SetDigit17:
  mov    cl, '0' - 1
  mov    ebx, $6fc10000          {10000000000000000 mod $100000000}
  mov    ebp, $002386f2          {10000000000000000 div $100000000}
@@CalcDigit17:
  add    ecx, 1
  sub    eax, ebx
  sbb    edx, ebp
  jnc    @@CalcDigit17
  add    eax, ebx
  adc    edx, ebp
  mov    [edi], cl
  add    edi, 1                  {Update Destination}
  mov    esi, 16                 {Set 16 Digits Left}
@@LessThan17Digits:              {Process Next 8 Digits}
  mov    ecx, 100000000          {EDX:EAX = Abs(Value) = Dividend}
  div    ecx
  mov    ebp, eax                {Dividend DIV 100000000}
  mov    ebx, edx
  mov    eax, edx                {Dividend MOD 100000000}
  mov    edx, $51EB851F
  mul    edx
  shr    edx, 5                  {Dividend DIV 100}
  mov    eax, edx                {Set Next Dividend}
  lea    edx, [edx*4+edx]
  lea    edx, [edx*4+edx]
  shl    edx, 2                  {Dividend DIV 100 * 100}
  sub    ebx, edx                {Remainder (0..99)}
  movzx  ebx, word ptr [TwoDigitLookupRaw+ebx*2]
  shl    ebx, 16
  mov    edx, $51EB851F
  mov    ecx, eax                {Dividend}
  mul    edx
  shr    edx, 5                  {Dividend DIV 100}
  mov    eax, edx
  lea    edx, [edx*4+edx]
  lea    edx, [edx*4+edx]
  shl    edx, 2                  {Dividend DIV 100 * 100}
  sub    ecx, edx                {Remainder (0..99)}
  or     bx, word ptr [TwoDigitLookupRaw+ecx*2]
  mov    [edi+esi-4], ebx        {Store 4 Digits}
  mov    ebx, eax
  mov    edx, $51EB851F
  mul    edx
  shr    edx, 5                  {EDX := Dividend DIV 100}
  lea    eax, [edx*4+edx]
  lea    eax, [eax*4+eax]
  shl    eax, 2                  {EDX = Dividend DIV 100 * 100}
  sub    ebx, eax                {Remainder (0..99)}
  movzx  ebx, word ptr [TwoDigitLookupRaw+ebx*2]
  movzx  ecx, word ptr [TwoDigitLookupRaw+edx*2]
  shl    ebx, 16
  or     ebx, ecx
  mov    [edi+esi-8], ebx        {Store 4 Digits}
  mov    eax, ebp                {Remainder}
  sub    esi, 10                 {Digits Left - 2}
  jz     @@Last2Digits
@@SmallLoop:                     {Process Remaining Digits}
  mov    edx, $28F5C29           {((2^32)+100-1)/100}
  mov    ebx, eax                {Dividend}
  mul    edx
  mov    eax, edx                {Set Next Dividend}
  imul   edx, -200
  movzx  edx, word ptr [TwoDigitLookupRaw+ebx*2+edx] {Dividend MOD 100 in ASCII}
  mov    [edi+esi], dx
  sub    esi, 2
  jg     @@SmallLoop             {Repeat Until Less than 2 Digits Remaining}
  jz     @@Last2Digits
  or     al , '0'                {Ascii Adjustment}
  mov    [edi], al               {Save Final Digit}
  jmp    @@Done
@@Last2Digits:
  movzx  eax, word ptr [TwoDigitLookupRaw+eax*2]
  mov    [edi], ax               {Save Final 2 Digits}
@@Done:
  pop    esi
  pop    edi
  pop    ebx
end;
{$ELSE}
function IntToRaw(Value: Integer): RawByteString;
var C: Cardinal;
  Digits: Byte;
  Negative: Boolean;
  P: PAnsiChar;
begin
  Negative := Value < 0;
  if Negative
  then C := Cardinal(-Value)
  else C := Cardinal(Value);
  Digits := GetOrdinalDigits(C);
  ZSetString(nil, Digits+Ord(Negative), Result);
  P := Pointer(Result);
  if Negative then
    PByte(P)^ := Ord('-');
  IntToRaw(C, P+Ord(Negative), Digits);
end;

function IntToRaw(Value: Int64): RawByteString;
var U: UInt64;
  Digits: Byte;
  Negative: Boolean;
  P: PAnsiChar;
begin
  Negative := Value < 0;
  if Negative
  then U := UInt64(-Value)
  else U := UInt64(Value);
  Digits := GetOrdinalDigits(U);
  ZSetString(nil, Digits+Ord(Negative), Result);
  P := Pointer(Result);
  if Negative then
    PByte(P)^ := Ord('-');
  IntToRaw(U, P+Ord(Negative), Digits);
end;
{$IFEND}

function IntToRaw(Value: Byte): RawByteString;
begin
  Result := IntToRaw(Cardinal(Value));
end;

function IntToRaw(Value: ShortInt): RawByteString;
begin
  Result := IntToRaw(Integer(Value));
end;

function IntToRaw(Value: Word): RawByteString;
begin
  Result := IntToRaw(Cardinal(Value));
end;

function IntToRaw(Value: SmallInt): RawByteString;
begin
  Result := IntToRaw(Integer(Value));
end;

procedure IntToRaw(Value: Integer; Buf: PAnsiChar; PEnd: PPAnsiChar = nil);
begin
  if Value < 0 then begin
    PByte(Buf)^ := Ord('-');
    IntToRaw(Cardinal(-Value), Buf+1, PEnd);
  end else
    IntToRaw(Cardinal(Value), Buf, PEnd);
end;

procedure IntToRaw(Value: Cardinal; Buf: PAnsiChar; PEnd: PPAnsiChar = nil);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  IntToRaw(Value, Buf, Digits);
  if PEnd <> nil
  then PEnd^ := Buf + Digits
  else PByte(Buf + Digits)^ := Ord(#0);
end;

procedure IntToRaw(Value: Cardinal; Buf: PAnsiChar; Digits: Byte);
var J, K: Cardinal;
begin
  if Digits > 2 then
    repeat
      J  := Value div 100;           {Dividend div 100}
      K  := J * 100;
      K  := Value - K;               {Dividend mod 100}
      Value  := J;                   {Next Dividend}
      Dec(Digits, 2);
      PWord(Buf+Digits)^ := TwoDigitLookupW[K];
    until Digits <= 2;
  if Digits = 2
  then PWord(Buf)^ := TwoDigitLookupW[Value]
  else PByte(Buf)^ := Value or Byte('0');
end;

procedure IntToRaw(const Value: Int64; Buf: PAnsiChar; PEnd: PPAnsiChar = nil);
begin
  if Value < 0 then begin
    PByte(Buf)^ := Byte('-');
    IntToRaw(UInt64(-Value), Buf+1, PEnd);
  end else
    IntToRaw(UInt64(Value), Buf, PEnd);
end;

procedure IntToRaw(const Value: UInt64; Buf: PAnsiChar; PEnd: PPAnsiChar = nil);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  IntToRaw(Value, Buf, Digits);
  if PEnd <> nil
  then PEnd^ := Buf + Digits
  else PByte(Buf + Digits)^ := Ord(#0);
end;

procedure IntToRaw(Value: UInt64; Buf: PAnsiChar; Digits: Byte); overload;
//fast pure pascal by John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//function IntToStr64_JOH_PAS_5(Value: Int64): string;
type
  PByteArray = ^TByteArray;
  TByteArray = array[0..32767] of Byte;
var
  J64, K64           : UInt64;
  I32, J32, K32, L32 : Cardinal;
label cardinal_range;
begin
  if (Int64Rec(Value).Hi = 0) then begin{Within Integer Range - Use Faster Integer Version}
    i32 := Int64Rec(Value).Lo;
    goto cardinal_range;
  end;
  if Digits = 20 then begin
    PByte(Buf)^ := Ord('1');
    Inc(Buf);
    {$IFDEF FPC} //fatal error?
    Value := Value - UInt64(_10Trillion);
    {$ELSE}
    Dec(Value, UInt64(_10Trillion));
    {$ENDIF}
    Dec(Digits);
  end;
  if Digits > 17 then begin {18 or 19 Digits}
    if Digits = 19 then begin
      PByte(Buf)^ := Ord('0');
      while Value >= {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(1000000000000000000){$ELSE}1000000000000000000{$ENDIF} do begin
        Dec(Value, {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(1000000000000000000){$ELSE}1000000000000000000{$ENDIF});
        Inc(Buf^);
      end;
      Inc(Buf);
    end;
    PByte(Buf)^ := Ord('0');
    while Value >= {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(100000000000000000){$ELSE}100000000000000000{$ENDIF} do begin
      Dec(Value, {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(100000000000000000){$ELSE}100000000000000000{$ENDIF});
      Inc(Buf^);
    end;
    Inc(Buf);
    Digits := 17;
  end;
  J64 := Value div {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(100000000){$ELSE}100000000{$ENDIF}; {Very Slow prior to Delphi 2005}
  K64 := Value - (J64 * {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(100000000){$ELSE}100000000{$ENDIF}); {Remainder = 0..99999999}
  I32 := K64;
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  I32 := J32 div 100;
  L32 := I32 * 100;
  L32 := J32 - L32;
  Dec(Digits, 4);
  J32 := (TwoDigitLookupW[K32] shl 16) + TwoDigitLookupW[L32];
  PCardinal(@PByteArray(Buf)[Digits])^ := J32;
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  Dec(Digits, 4);
  I32 := (TwoDigitLookupW[K32] shl 16) + TwoDigitLookupW[J32];
  PCardinal(@PByteArray(Buf)[Digits])^ := I32;
  I32 := J64; {Dividend now Fits within Integer - Use Faster Version}
cardinal_range:
  if Digits > 2 then
    repeat
      J32 := I32 div 100;
      K32 := J32 * 100;
      K32 := I32 - K32;
      I32 := J32;
      Dec(Digits, 2);
      PWord(@PByteArray(Buf)[Digits])^ := TwoDigitLookupW[K32];
    until Digits <= 2;
  if Digits = 2 then
    PWord(@PByteArray(Buf)[Digits-2])^ := TwoDigitLookupW[I32]
  else
    PByte(Buf)^ := I32 or ord('0');
end;

function IntToRaw(const Value: UInt64): RawByteString;
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  ZSetString(nil, Digits, Result);
  IntToRaw(Value, Pointer(Result), Digits);
end;

procedure CurrToRaw(const Value: Currency; Buf: PAnsiChar; PEnd: PPAnsiChar = nil);
var
  I64: UInt64;
  I64Rec: Int64Rec absolute I64;
  Digits: Byte;
begin
  if Value = 0 then begin
    PByte(Buf)^ := ord('0');
    Inc(Buf);
  end else begin
    if Value<0 then begin
      PByte(Buf)^ := Ord('-');
      Inc(Buf);
      I64 := -PInt64(@Value)^;
    end else
      I64 := PInt64(@Value)^;
    Digits := GetOrdinalDigits(I64);
    if Digits < 5 then begin
      PLongWord(Buf)^ := ord('0')+ord('.') shl 8+ord('0') shl 16+ord('0') shl 24; //fill 0.00 @once
      if Digits = 1
      then PWord(Buf+4)^ := Ord('0')+(Ord('0')+I64Rec.Lo) shl 8 //finalize last 0x pair directyl
      else IntToRaw(I64Rec.Lo, Buf+2+Ord(Digits<3)+Ord(Digits<4), Digits);
      Inc(Buf, 5);
    end else begin
      IntToRaw(i64, Buf, Digits);
      Inc(Buf, Digits);
      PCardinal(Buf-3)^ := PCardinal(Buf-4)^; //move trailing digits one pos forward;
      PByte(Buf-4)^ := Ord('.');
    end;
    //dec by trailing zeroes
    if PByte(Buf)^ = Ord('0') then begin
      if PByte(Buf-1)^ = Ord('0') then
        if PByte(Buf-2)^ = Ord('0') then
          if PByte(Buf-3)^ = Ord('0')
          then Dec(Buf, 4)
          else Dec(Buf, 2)
        else Dec(Buf)
    end else
      Inc(Buf);
  end;
  if PEnd <> nil
  then PEnd^ := Buf
  else PByte(Buf)^ := Ord(#0);
end;

function CurrToRaw(const Value: Currency): RawByteString;
var buf: array[0..31] of AnsiChar;
  P: PAnsiChar;
begin
  CurrToRaw(Value, @buf[0], @P);
  ZSetString(PAnsiChar(@Buf[0]), P-PAnsiChar(@Buf[0]), Result);
end;

procedure CurrToUnicode(const Value: Currency; Buf: PWideChar; PEnd: ZPPWideChar = nil);
var
  I64: UInt64;
  I64Rec: Int64Rec absolute I64;
  Digits: Byte;
begin
  if Value = 0 then begin
    PWord(Buf)^ := ord('0');
    Inc(Buf);
  end else begin
    if Value<0 then begin
      PWord(Buf)^ := Ord('-');
      Inc(Buf);
      I64 := -PInt64(@Value)^;
    end else
      I64 := PInt64(@Value)^;
    Digits := GetOrdinalDigits(I64);
    if Digits < 5 then begin
      PLongWord(Buf)^   := ord('0') + ord('.') shl 16; //write "0." @once
      PLongWord(Buf+2)^ := ord('0') + ord('0') shl 16; //fill "00" @once
      if Digits = 1
      then PLongWord(Buf+4)^ := Ord('0')+(Ord('0')+I64Rec.Lo) shl 16 //finalize last 0x pair directly
      else IntToUnicode(I64Rec.Lo, Buf+2+Ord(Digits<3)+Ord(Digits<4), Digits);
      Inc(Buf, 5);
    end else begin
      IntToUnicode(i64, Buf, Digits);
      Inc(Buf, Digits);
      I64 := PUInt64(Buf-4)^; //localize (CPU32 makes two cadinal moves and the value would be incorrect then)
      PUInt64(Buf-3)^ := i64; //move trailing digits one pos forward;
      PWord(Buf-4)^ := Ord('.');
    end;
    if PWord(Buf)^ = Ord('0') then begin
      if PWord(Buf-1)^ = Ord('0') then
        if PWord(Buf-2)^ = Ord('0') then
          if PWord(Buf-3)^ = Ord('0')
          then Dec(Buf, 4)
          else Dec(Buf, 2)
        else Dec(Buf)
    end else
      Inc(Buf);
  end;
  if PEnd <> nil
  then PEnd^ := Buf
  else PWord(Buf)^ := Ord(#0);
end;

function CurrToUnicode(const Value: Currency): ZWideString;
var buf: array[0..31] of WideChar;
  P: PWideChar;
begin
  CurrToUnicode(Value, @buf[0], @P);
  System.SetString(Result, PWideChar(@Buf[0]), P-PWideChar(@Buf[0]));
end;

procedure IntToUnicode(Value: Integer; Buf: PWideChar; PEnd: ZPPWideChar = nil);
begin
  if Value < 0 then begin
    PWord(Buf)^ := Ord('-');
    IntToUnicode(Cardinal(-Value), Buf+1, PEnd);
  end else
    IntToUnicode(Cardinal(Value), Buf, PEnd);
end;

procedure IntToUnicode(Value: Cardinal; Buf: PWideChar; PEnd: ZPPWideChar = nil);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  IntToUnicode(Value, Buf, Digits);
  if PEnd <> nil
  then PEnd^ := Buf + Digits
  else PWord(Buf + Digits)^ := Ord(#0);
end;

//fast pure pascal by John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//function IntToStr64_JOH_PAS_5(Value: Int64): string;
procedure IntToUnicode(Value: Cardinal; Buf: PWideChar; Digits: Byte);
var J, K: Cardinal;
begin
  if Digits > 2 then
    repeat
      J  := Value div 100;           {Dividend div 100}
      K  := J * 100;
      K  := Value - K;               {Dividend mod 100}
      Value  := J;                   {Next Dividend}
      Dec(Digits, 2);
      PLongWord(Buf+Digits)^ := TwoDigitLookupLW[K];
    until Digits <= 2;
  if Digits = 2
  then PLongWord(Buf)^ := TwoDigitLookupLW[Value]
  else PWord(Buf)^ := Value or Ord('0');
end;

procedure IntToUnicode(const Value: Int64; Buf: PWideChar; PEnd: ZPPWideChar = nil);
begin
  if Value < 0 then begin
    PWord(Buf)^ := Ord('-');
    IntToUnicode(UInt64(-Value), Buf+1, PEnd);
  end else
    IntToUnicode(UInt64(Value), Buf, PEnd);
end;

procedure IntToUnicode(const Value: UInt64; Buf: PWideChar; PEnd: ZPPWideChar = nil);
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  IntToUnicode(Value, Buf, Digits);
  if PEnd <> nil
  then PEnd^ := Buf + Digits
  else PWord(Buf + Digits)^ := Ord(#0);
end;

procedure IntToUnicode(Value: UInt64; Buf: PWideChar; Digits: Byte);
//fast pure pascal by John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//function IntToStr64_JOH_PAS_5(Value: Int64): string;
var
  J64, K64           : UInt64;
  I32, J32, K32, L32 : Cardinal;
  label cardinal_range;
begin
  if (Int64Rec(Value).Hi = 0) then begin{Within Integer Range - Use Faster Integer Version}
    I32 := Int64Rec(Value).Lo;
    goto cardinal_range;
  end;
  if Digits = 20 then begin
    PWord(Buf)^ := Word('1');
    Inc(Buf);
    {$IFDEF FPC} //(???? Dec seems not supporting integers with range > MaxInt64 -> Fatal: Internal error 200706094
    Value := Value - UInt64(_10Trillion);
    {$ELSE}
    Dec(Value, UInt64(_10Trillion));
    {$ENDIF}
    Dec(Digits);
  end;
  if Digits > 17 then begin {18 or 19 Digits}
    if Digits = 19 then begin
      PWord(Buf)^ := Word('0');
      while Value >= {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(1000000000000000000){$ELSE}1000000000000000000{$ENDIF} do begin
        Dec(Value, {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(1000000000000000000){$ELSE}1000000000000000000{$ENDIF});
        Inc(PWord(Buf)^);
      end;
      Inc(Buf);
    end;
    PWord(Buf)^ := Word('0');
    while Value >= {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(100000000000000000){$ELSE}100000000000000000{$ENDIF} do begin
      Dec(Value, {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(100000000000000000){$ELSE}100000000000000000{$ENDIF});
      Inc(PWord(Buf)^);
    end;
    Inc(Buf);
    Digits := 17;
  end;
  J64 := Value div {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(100000000){$ELSE}100000000{$ENDIF}; {Very Slow prior to Delphi 2005}
  K64 := Value - (J64 * {$IFDEF NEED_TYPED_UINT64_CONSTANTS}UInt64(100000000){$ELSE}100000000{$ENDIF}); {Remainder = 0..99999999}
  I32 := K64;
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PLongWord(Buf + Digits - 2)^ := TwoDigitLookupLW[K32];
  I32 := J32 div 100;
  L32 := I32 * 100;
  L32 := J32 - L32;
  PLongWord(Buf + Digits - 4)^ := TwoDigitLookupLW[L32];
  J32 := I32 div 100;
  K32 := J32 * 100;
  K32 := I32 - K32;
  PLongWord(Buf + Digits - 6)^ := TwoDigitLookupLW[K32];
  PLongWord(Buf + Digits - 8)^ := TwoDigitLookupLW[J32];
  Dec(Digits, 8);
  I32 := J64; {Dividend now Fits within Integer - Use Faster Version}
cardinal_range:
  if Digits > 2 then
    repeat
      J32 := I32 div 100;
      K32 := J32 * 100;
      K32 := I32 - K32;  {Dividend mod 100}
      I32 := J32;        {next Dividend}
      Dec(Digits, 2);
      PLongWord(Buf + Digits)^ := TwoDigitLookupLW[K32];
    until Digits <= 2;
  if Digits = 2
  then PLongWord(Buf)^ := TwoDigitLookupLW[I32]
  else PWord(Buf)^ := Word(I32 or ord('0'));
end;

function IntToUnicode(Value: Byte): ZWideString;
begin
  Result := IntToUnicode(Cardinal(Value));
end;

function IntToUnicode(Value: ShortInt): ZWideString;
begin
  Result := IntToUnicode(Integer(Value));
end;

function IntToUnicode(Value: Word): ZWideString;
begin
  Result := IntToUnicode(Cardinal(Value));
end;

function IntToUnicode(Value: SmallInt): ZWideString;
begin
  Result := IntToUnicode(Integer(Value));
end;

function IntToUnicode(Value: Cardinal): ZWideString;
var
  Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  System.SetString(Result, nil, Digits);
  IntToUnicode(Value, Pointer(Result), Digits);
end;

function IntToUnicode(Value: Integer): ZWideString;
var C: Cardinal;
  Digits: Byte;
  Negative: Boolean;
  P: PWideChar;
begin
  Negative := Value < 0;
  if Negative
  then C := Cardinal(-Value)
  else C := Cardinal(Value);
  Digits := GetOrdinalDigits(C);
  System.SetString(Result, nil, Digits+Ord(Negative));
  P := Pointer(Result);
  if Negative then
    PWord(P)^ := Ord('-');
  IntToUnicode(C, P+Ord(Negative), Digits);
end;

function IntToUnicode(const Value: Int64): ZWideString;
var U: UInt64;
  Digits: Byte;
  Negative: Boolean;
  P: PWideChar;
begin
  Negative := Value < 0;
  if Negative
  then U := UInt64(-Value)
  else U := UInt64(Value);
  Digits := GetOrdinalDigits(U);
  System.SetString(Result, nil, Digits+Ord(Negative));
  P := Pointer(Result);
  if Negative then
    PWord(P)^ := Ord('-');
  IntToUnicode(U, P+Ord(Negative), Digits);
end;

function IntToUnicode(const Value: UInt64): ZWideString;
var Digits: Byte;
begin
  Digits := GetOrdinalDigits(Value);
  System.SetString(Result, nil, Digits);
  IntToUnicode(Value, Pointer(Result), Digits);
end;

{$IF defined (WIN32) and not defined(FPC)}
procedure StrToIntError(const S: PAnsiChar);
begin
  raise EConvertError.CreateResFmt(@SInvalidInteger, [S]);
end;
{$IFEND}

function RawToInt(const Value: RawByteString): Integer;
begin
  Result := RawToInt(Pointer(Value));
end;

{$WARNINGS OFF} {Prevent False Compiler Warning on Digit not being Initialized}
function RawToInt(const Value: PAnsiChar): Integer;
{$IF defined (WIN32) and not defined(FPC)}
//originally written by John O'Harrow
//http://fastcode.sourceforge.net/
//function StrToInt32_JOH_IA32_7_b(const S: string) : Integer;
asm
  test  eax, eax
  jz    @@Failed
  push  eax
  push  ebx
  push  edi
  push  esi
  mov   edx, eax            {String Pointer}
  xor   ebx, ebx            {Clear Sign Flag (top bit) and Valid Flag}
  xor   eax, eax            {Clear Result}
  mov   edi, '0'
  mov   esi, 9
@@Trim:                     {Strip Leading Spaces}
  movzx ecx, [edx]
  inc   edx
  cmp   cl, ' '
  je    @@Trim
  cmp   ecx, edi            {cl <= '0'?}
  jle   @@CheckFirstChar    {Yes, Check +, -, $, 0x, 0X}
  test  cl, not 'x'
  jz    @@CheckX            {May start with 'x' or 'X'}

@@Numeric:
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@TestValid         {Not '0'..'9'}
  mov   eax, ecx            {Result := Digit}

  movzx ecx, [edx]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+1]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+2]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+3]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+4]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+5]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+6]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

  movzx ecx, [edx+7]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@CheckDone         {Not '0'..'9'}
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}

@@NumLoop:
  movzx ecx, [edx+8]
  sub   ecx, edi
  cmp   ecx, esi
  ja    @@NumDone           {Not '0'..'9'}
  cmp   eax, MaxInt/10
  ja    @@Error
  inc   edx
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}
  jmp   @@NumLoop

@@TestValid:
  test  bl, bl              {Got Valid Number?}
  jz    @@Error             {No, Error}
@@CheckDone:
  add   ecx, edi            {Last Character = Null Terminator?}
  jnz   @@Error             {No, Error}
  sar   ebx, 31             {Set Each Bit to Top Bit (Sign Flag)}
  xor   eax, ebx            {Negate Result if Necessary}
  sub   eax, ebx
  pop   esi
  pop   edi
  pop   ebx
  pop   ecx
  ret

@@NumDone:
  cmp   eax, $80000000
  jb    @@CheckDone         {No Overflow}
  jne   @@Error             {Overflow}
  test  ebx, ebx            {Sign Flag Set?}
  js    @@CheckDone         {Yes, Result is Valid (-MaxInt-1)}
@@Error:
  pop   esi
  pop   edi
  pop   ebx
  pop   eax
@@Failed:
  jmp   StrToIntError

@@CheckFirstChar:           {First Char <= '0'}
  cmp   cl, '$'
  je    @@Hex
  cmp   cl, '-'
  je    @@Minus
  cmp   cl, '+'
  je    @@Plus
  cmp   ecx, edi            {Starts with '0'?}
  jne   @@Error             {No, Error}
  movzx ecx, [edx]          {Character after '0'}
  mov   bl, 1               {Valid := True}
  inc   edx
  jmp   @@CheckX
@@Minus:
  mov   ebx, $80000000      {Set Sign Flag (Top Bit), Valid := False}
@@Plus:
  movzx ecx, [edx]          {Character after '+' or '-'}
  inc   edx
  cmp   cl, '$'
  je    @@Hex               {Starts with '+$' or '-$'}
  cmp   ecx, edi            {Starts with '+0' or '-0'?}
  jne   @@CheckAlpha        {No, May start with '+x', '-x', '+X' or '-X'}
  movzx ecx, [edx]          {Character after '+0' or '-0'}
  inc   ebx                 {Starts with '+0' or '-0', Valid := True}
  inc   edx
@@CheckAlpha:
  test  cl, not 'x'         {Could Char be 'x' or 'X'?}
  jnz   @@Numeric           {No, Assume Numeric}
@@CheckX:
  or    cl, $20             {'X' -> 'x'}
  cmp   cl, 'x'             {Char = 'X' or 'x'?}
  movzx ecx, [edx-1]        {Reload Character}
  jne   @@Numeric           {Does Not start with +/-('x', 'X', '0x' or '0X')}
  mov   bl, 0               {Reset Valid to False}
@@Hex:
  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex1              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@TestValid         {Check for Valid and Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex1:
  mov   eax, ecx            {Result = Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex2              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex2:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex3              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex3:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex4              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex4:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex5              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex5:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex6              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex6:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex7              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex7:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

  movzx ecx, [edx]
  inc   edx
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@Hex8              {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@Hex8:
  shl   eax, 4
  add   eax, ecx            {Result = Result * 16 + Digit}

@@HexLoop:
  movzx ecx, [edx]
  sub   ecx, edi
  cmp   ecx, esi
  jna   @@CheckRange        {'0'..'9'}
  cmp   cl, 'z'-'0'
  ja    @@CheckDone         {Check for Null Terminator}
  or    cl, $20             {'A'..'F' -> 'a'..'f'}
  sub   cl, 'a'-'0'-10      {'A'..'F' or 'a'..'f' -> 10..15}
  cmp   cl, 15
  ja    @@Error             {Not Hex Character}
@@CheckRange:
  cmp   eax, MaxInt/8       {High(ULONG) div 16}
  ja    @@Error             {Overflow}
  shl   eax, 4
(*
  shl   eax, 4              //Using this instead of the above 3 lines wrongly
  jc    @@Error             //  passes validation with S='$200000000000000'
*)
  add   eax, ecx            {Result = Result * 16 + Digit}
  inc   edx
  jmp   @@HexLoop
{$ELSE}
  //function StrToInt32_JOH_PAS_7_c(const s: string): Integer;
  //originally written by John O'Harrow
  //http://fastcode.sourceforge.net/
  //EgonHugeist:
  //Patched to use PAnsiChar values only
  const
    AdjustLowercase = Ord('a') - 10;
    AdjustUppercase = Ord('A') - 10;
  var
    Digit: Integer;
    Neg, Hex, Valid: Boolean;
    P: PAnsiChar;
  begin
    //P := Pointer(Value);
    P := Value;
    if not Assigned(P) then
      raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
    Neg   := False;
    Hex   := False;
    Valid := False;
    while Ord(P^) = Ord(' ') do
      Inc(P);
    if Ord(P^) in [Ord('+'), Ord('-')] then
      begin
        Neg := (Ord(P^) = Ord('-'));
        inc(P);
      end;
    if Ord(P^) = Ord('$') then
      begin
        Hex := True;
        inc(P);
      end
    else
      begin
        if Ord(P^) = Ord('0') then
          begin
            Valid := True;
            inc(P);
          end;
        if (Ord(P^) or $20) = ord('x') then
          begin {Upcase(P^) = 'X'}
            Hex := True;
            inc(P);
          end;
      end;
    Result := 0;
    if Hex then
      begin
        Valid := False;
        while True do
          begin
            case Ord(P^) of
              Ord('0')..Ord('9'): Digit := Ord(P^) - Ord('0');
              Ord('a')..Ord('f'): Digit := Ord(P^) - AdjustLowercase;
              Ord('A')..Ord('F'): Digit := Ord(P^) - AdjustUppercase;
              else      Break;
            end;
            if Cardinal(Result) > MaxInt div 8 then
              Break;
            Result := (Result shl 4) + Digit;
            Valid := True;
            inc(P);
          end;
      end
    else
      begin
        while True do
          begin
            if not (Ord(P^) in [Ord('0')..Ord('9')]) then
              Break;
            if Result > (MaxInt div 10) then
              Break;
            Result := (Result * 10) + Ord(P^) - Ord('0');
            Valid := True;
            inc(P);
          end;
        if Result < 0 then {Possible Overflow}
          if (Cardinal(Result) <> $80000000) or (not neg) then
            begin {Min(Integer) = $80000000 is a Valid Result}
              Dec(P);
              Valid := False;
            end;
      end;
    if Neg then
      Result := -Result;
    if (not Valid) or (Ord(P^) <> Ord(#0)) then
      raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
{$IFEND}
end;
{$WARNINGS ON}

{$WARNINGS OFF}
function UnicodeToInt(const Value: ZWideString): Integer;
//function StrToInt32_JOH_PAS_7_c(const s: string): Integer;
//originally wrtten by John O'Harrow
//http://fastcode.sourceforge.net/
const
  AdjustLowercase = Ord('a') - 10;
  AdjustUppercase = Ord('A') - 10;
var
  Digit: Integer;
  Neg, Hex, Valid: Boolean;
  P: PWideChar;
  W: PWord absolute P;
begin
  P := Pointer(Value);
  if not Assigned(P) then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
  Neg   := False;
  Hex   := False;
  Valid := False;
  while P^ = ' ' do
    Inc(P);
  if W^ in [Ord('+'), Ord('-')] then
    begin
      Neg := Ord(W^) = Ord('-');
      inc(P);
    end;
  if W^ = Ord('$') then
    begin
      Hex := True;
      inc(P);
    end
  else
    begin
      if W^ = Ord('0') then begin
        Valid := True;
        inc(P);
      end;
      if (W^ or $20) = ord('x') then begin {Upcase(P^) = 'X'}
        Hex := True;
        inc(P);
      end;
    end;
  Result := 0;
  if Hex then
    begin
      Valid := False;
      while True do begin
        case W^ of
          Ord('0')..Ord('9'): Digit := W^ - Ord('0');
          Ord('a')..Ord('f'): Digit := W^ - AdjustLowercase;
          Ord('A')..Ord('F'): Digit := W^ - AdjustUppercase;
          else  Break;
        end;
        if Cardinal(Result) > MaxInt div 8 then
          Break;
        Result := (Result shl 4) + Digit;
        Valid := True;
        inc(P);
      end;
    end
  else
    begin
      while True do
        begin
          if not (Ord(P^) in [Ord('0')..Ord('9')]) then
            Break;
          if Result > (MaxInt div 10) then
            Break;
          Result := (Result * 10) + Ord(P^) - Ord('0');
          Valid := True;
          inc(P);
        end;
      if Result < 0 then {Possible Overflow}
        if (Cardinal(Result) <> $80000000) or (not neg) then
          begin {Min(Integer) = $80000000 is a Valid Result}
            Dec(P);
            Valid := False;
          end;
    end;
  if Neg then
    Result := -Result;
  if (not Valid) or (P^ <> #0) then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
end;
{$WARNINGS ON}

{$IF defined(WIN32) and not defined(FPC)}
function ValLong_JOH_IA32_8_a(const s: PAnsiChar; out code: Integer): Integer;
//fast asm by John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//EgonHugeist: Changed in S type from String to PAnsiChar
asm
  test  eax, eax
  jz    @@Null
  push  ebx
  push  esi
  push  eax                 {Save String Pointer}
  mov   esi, eax            {String Pointer}
  xor   ebx, ebx            {Clear Valid Flag and Sign Flag}
  xor   eax, eax            {Clear Result}
  jmp   @@TrimEntry
@@Null:
  mov   [edx], eax
  inc   [edx]               {Code = 1}
  ret
@@Trim:                     {Strip Leading Spaces}
  inc   esi
@@TrimEntry:
  movzx ecx, [esi]
  cmp   cl, ' '
  je    @@Trim
  cmp   cl, '0'
  jle   @@CheckFirstChar
@@CheckAlpha:
  test  cl, not 'x'
  jz    @@CheckX            {May be 'x' or 'X'}
@@NumLoop:
  sub   cl, '0'
  cmp   cl, 9
  ja    @@NumDone           {Not '0'..'9'}
  cmp   eax, MaxInt/10
  ja    @@SetSign
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}
  inc   esi
  mov   bl, 1               {Valid := True}
  movzx ecx, [esi]
  jmp   @@NumLoop
@@NumDone:
  cmp   eax, $80000000
  jb    @@SetSign           {No Overflow}
  jne   @@Overflow
  test  ebx, ebx            {Sign Flag}
  js    @@SetSign           {Result is Valid (-MaxInt-1)}
@@Overflow:
  dec   esi
  mov   bl, 0               {Valid := False}
  jmp   @@SetSign
@@CheckFirstChar:
  cmp   cl, '+'
  je    @@PlusMinus
  cmp   cl, '-'
  jne   @@SignSet
@@PlusMinus:                {Starts with '+' or '-'}
  mov   bl, '+'+1
  sub   ebx, ecx            {Set Sign Flag: '+' -> +1, '-' -> -1}
  inc   esi
  mov   bl, 0               {Valid := False}
  movzx ecx, [esi]          {Character after '+' or '-'}
@@SignSet:
  cmp   cl, '$'
  je    @@Hex               {Hexadecimal}
  cmp   cl, '0'
  jne   @@CheckAlpha        {May start with 'x' or 'X'}
  inc   esi
  mov   bl, 1               {Assume Valid = True}
  movzx ecx, [esi]          {Character after '0'}
  jmp   @@CheckAlpha        {May start with '0x' or '0X'}
@@CheckX:
  mov   bh, cl
  or    bh, $20             {'X' -> 'x'}
  cmp   bh, 'x'
  jne   @@NumLoop
@@Hex:
  mov   bl, 0               {Valid := False}
@@HexLoop:
  inc   esi
  movzx ecx, [esi]
  cmp   cl, 'a'
  jb    @@CheckNum
  sub   cl, 'a'-'A'         {'a' > 'A'}
@@CheckNum:
  sub   cl, '0'
  cmp   cl, 9
  jna   @@CheckHexRange     {'0'..'9'}
  sub   cl, 'A'-'0'
  cmp   cl, 5               {Valid Hex Character?}
  ja    @@NotHex            {No, Invalid}
  add   cl, 10              {Yes, Adjust Digit}
@@CheckHexRange:
  cmp   eax, MaxInt/8       {High(ULONG) div 16}
  ja    @@SetSign           {Overflow}
  shl   eax, 4              {Result = Result * 16}
  mov   bl, 1               {Valid := True}
  add   eax, ecx
  jmp   @@HexLoop
@@NotHex:
  add   cl, 'A'-'0'         {Restore Char-'0'}
@@SetSign:
  mov   ch, bl              {Save Valid Flag}
  sar   ebx, 31             {Set Each Bit to Top Bit}
  dec   ch                  {0 if Valid, -1 if Invalid}
  xor   eax, ebx            {Negate Result if Necessary}
  sub   eax, ebx
  or    cl, ch              {If Invalid, Force CL = -1}
  cmp   cl, -'0'            {Last Character = #0?}
  jne   @@Error             {Not Valid or Not End of String}
  xor   esi, esi            {Code := 0}
  pop   ecx                 {Dump String Pointer}
@@Finished:
  mov   [edx], esi          {Set Error Code}
  pop   esi
  pop   ebx
  ret
@@Error:
  inc   esi
  pop   ecx                 {String Pointer}
  sub   esi, ecx
  jmp   @@Finished          {Exit Setting Error Code}
end;
{$ELSE}
{$WARNINGS OFF} {Prevent False Compiler Warning on Digit not being Initialized}
function ValLong_JOH_PAS_4_b(const S: PAnsiChar; out code: Integer): Integer;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//EH: changed to NEXGEN save PByte support
var
  Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
  P: PAnsiChar;
begin
  Code := 0;
  P := S;
  if not Assigned(P) then begin
    Result := 0;
    inc(Code);
    Exit;
  end;
  Flags := 0;
  while Ord(P^) = Ord(' ') do
    Inc(P);
  if Ord(P^) in [Ord('+'), Ord('-')] then begin
    Flags := Flags or (Ord(P^) - Ord('+')); {Set/Reset Neg}
    inc(P);
  end;
  if Ord(P^) = Ord('$') then begin
    inc(P);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if Ord(P^) = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
    if (Ord(P^) or $20) = ord('x') then begin //Upcase(P^) = 'X'
      Flags := Flags or 4; {Hex := True}
      inc(P);
    end;
  end;
  Result := 0;
  if (Flags and 4) <> 0 then begin
    Flags := Flags and (not 1); {Valid := False}
    while True do begin
      case Ord(P^) of
        Ord('0')..Ord('9'): Digit := Ord(P^) - Ord('0');
        Ord('a')..Ord('f'): Digit := Ord(P^) - Ord('a') + 10;
        Ord('A')..Ord('F'): Digit := Ord(P^) - Ord('A') + 10;
        else Break;
      end;
      if (Result < 0) or (Result > $0FFFFFFF) then
        Break;
      Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
  end else begin
    while True do begin
      if not (Ord(P^) in [Ord('0')..Ord('9')]) then
        break;
      if Result > (MaxInt div 10) then
        break;
      Result := (Result * 10) + Ord(P^) - Ord('0');
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
    if Result < 0 then {Possible Overflow}
      if (Cardinal(Result) <> $80000000) or ((Flags and 2) = 0) then
        begin {Min(Integer) = $80000000 is a Valid Result}
          Dec(P);
          Flags := Flags or 1; {Valid := True}
        end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if ((Flags and 1) <> 0) and (Ord(P^) = Ord(#0)) then
    Code := 0 {Valid=True and End Reached}
  else
    Code := P-S+1;
end;
{$WARNINGS ON}
{$IFEND}

function RawToIntDef(const S: RawByteString; const Default: Integer) : Integer;
begin
  Result := RawToIntDef(Pointer(S), Default);
end;

function RawToIntDef(const S: PAnsiChar; const Default: Integer) : Integer;
var
  E: Integer;
begin
  {$IF defined(WIN32) and not defined(FPC)}
  Result := ValLong_JOH_IA32_8_a(S, E);
  {$ELSE}
  Result := ValLong_JOH_PAS_4_b(S, E{%H-});
  {$IFEND}
  if E > 0 then
    if not ((E > 0) and Assigned(S) and (AnsiChar((S+E-1)^)=AnsiChar(' '))) then
      Result := Default;
end;

function RawToIntDef(Buf, PEnd: PAnsiChar; const Default: Integer) : Integer;
var P: PAnsiChar;
begin
  P := PEnd;
  Result := ValRawInt(Buf, PEnd);
  if PEnd <> P then
    Result := Default;
end;

{$WARNINGS OFF} //value digits might not be initialized
function ValLong_JOH_PAS_4_b_unicode(const S: PWideChar; out code: Integer): Integer;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for unicode chars
var
  Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
  P: PWideChar;
  W: PWord absolute P;  //FPC solution: PWideChar comparision is dead slow!
begin
  Code := 0;
  P := S;
  if not Assigned(P) then
    begin
      Result := 0;
      inc(Code);
      Exit;
    end;
  Flags := 0;
  while W^ = Ord(' ') do
    Inc(P);
  if W^ in [Ord('+'), Ord('-')] then begin
    Flags := Flags or (W^ - Ord('+')); {Set/Reset Neg}
    inc(P);
  end;
  if W^ = Ord('$') then begin
    inc(P);
    Flags := Flags or Byte(4); {Hex := True}
  end else begin
    if W^ = Ord('0') then begin
      inc(P);
      Flags := Flags or 1; {Valid := True}
    end;
    if (W^ or $20) = ord('x') then begin //upcase
      inc(P);
      Flags := Flags or 4; {Hex := True}
    end;
  end;
  Result := 0;
  if (Flags and 4) <> 0 then begin
    Flags := Flags and (not 1); {Valid := False}
    while True do begin
      case W^ of
        Ord('0')..Ord('9'): Digit := W^ - Ord('0');
        Ord('a')..Ord('f'): Digit := W^ - Ord('a') + 10;
        Ord('A')..Ord('F'): Digit := W^ - Ord('A') + 10;
        else Break;
      end;
      if (Result < 0) or (Result > $0FFFFFFF) then
        Break;
      Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
  end else begin
    while True do begin
      if not (W^ in [Ord('0')..Ord('9')]) then
        break;
      if Result > (MaxInt div 10) then
        break;
      Result := (Result * 10) + W^ - Ord('0');
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
    if Result < 0 then {Possible Overflow}
      if (Cardinal(Result) <> $80000000) or ((Flags and 2) = 0) then
        begin {Min(Integer) = $80000000 is a Valid Result}
          Dec(P);
          Flags := Flags or 1; {Valid := True}
        end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if ((Flags and 1) <> 0) and (W^ = Ord(#0)) then
    Code := 0 {Valid=True and End Reached}
  else
    Code := P-S+1;
end;
{$WARNINGS ON}

function UnicodeToIntDef(const S: ZWideString; const Default: Integer) : Integer;
var
  E: Integer;
begin
  Result := ValLong_JOH_PAS_4_b_unicode(Pointer(S), E{%H-});
  if E > 0 then
    if not ((E > 0) and Assigned(Pointer(S)) and ((S[E])=' ')) then
      Result := Default;
end;

function UnicodeToIntDef(const S: PWideChar; const Default: Integer) : Integer;
var
  E: Integer;
begin
  Result := ValLong_JOH_PAS_4_b_unicode(Pointer(S), E{%H-});
  if E > 0 then
    if not ((E > 0) and Assigned(S) and ((S+E-1)^=' ')) then
      Result := Default;
end;

function UnicodeToIntDef(Buf, PEnd: PWideChar; Default: Integer) : Integer;
var P: PWideChar;
begin
  P := PEnd;
  Result := ValUnicodeInt(Buf, PEnd);
  if P <> PEnd then
    Result := Default;
end;

{$WARNINGS OFF} //value digits might not be initialized
function ValInt64_JOH_PAS_8_a_raw(const s: PAnsiChar; out code: Integer): Int64;
//function ValInt64_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Int64;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified by EgonHugeist for faster conversion and PAnsiChar/PByte
const
  AdjustLowercase = Ord('a') - 10;
  AdjustUppercase = Ord('A') - 10;
var
  I, Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
  P: PAnsiChar;
begin
  Result := 0;
  Code   := 0;
  if (S = nil) or (Ord(S^) = Ord(#0)) then begin
    inc(Code);
    Exit;
  end;
  Flags := 0;
  P := S;
  while Ord(P^) = Ord(' ') do
    Inc(P);
  if Ord(P^) in [Ord('+'), Ord('-')] then begin
    Flags := Flags or Byte((Ord(S^) - Ord('+'))); {Set/Reset Neg}
    inc(P);
  end;
  if Ord(P^) = Ord('$') then begin
    inc(P);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if Ord(P^) = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
    if (Ord(P^) or $20) = ord('x') then begin {S[Code+1] in ['X','x']}
      Flags := Flags or 4; {Hex := True}
      inc(P);
    end;
  end;
  if (Flags and 4) <> 0 then begin {Hex = True}
    Flags := Flags and (not 1); {Valid := False}
    while true do begin
      case Ord(P^) of
        Ord('0')..Ord('9'): Digit := Ord(P^) - Ord('0');
        Ord('a')..Ord('f'): Digit := Ord(P^) - AdjustLowercase;
        Ord('A')..Ord('F'): Digit := Ord(P^) - AdjustUppercase;
        else      Break;
      end;
      if UInt64(Result) > (HighInt64 shr 3) then
        Break;
      if UInt64(Result) < (MaxInt div 16)-15 then begin {Use Integer Math instead of Int64}
          I := Result;
          I := (I shl 4) + Digit;
          Result := I;
        end
      else
        Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      Inc(P);
    end;
  end else begin
    while true do begin
      if ( not (Ord(P^) in [Ord('0')..Ord('9')]) ) or
         ( UInt64(Result) > (HighInt64 div 10)) then begin
        inc(P, Ord(Ord(P^) <> Ord(#0)));
        break;
      end;
      if UInt64(Result) < (MaxInt div 10)-9 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I * 10) + Ord(P^) - Ord('0');
        Result := I;
      end else {Result := (Result * 10) + Ord(Ch) - Ord('0');}
        Result := (Result shl 1) + (Result shl 3) + Ord(P^) - Ord('0');
      Flags := Flags or 1; {Valid := True}
      Inc(P);
    end;
    {$IFDEF FPC}
    if UInt64(Result) >= UInt64(Int64($8000000000000000)) then {Possible Overflow}
      if ((Flags and 2) = 0) or (UInt64(Result) <> UInt64(Int64($8000000000000000))) then
    {$ELSE}
    if UInt64(Result) >= $8000000000000000 then {Possible Overflow}
      if ((Flags and 2) = 0) or (UInt64(Result) <> $8000000000000000) then
    {$ENDIF}
        begin {Overflow}
          if ((Flags and 2) <> 0) then {Neg=True}
            Result := -Result;
          Code := P-S;
          Exit;
        end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if ((Flags and 1) <> 0) and (Ord(P^) = Ord(#0)) then
    Code := 0 {Valid=True and End Reached}
  else
    Code := P-S+1;
end;
{$WARNINGS ON}

{$WARNINGS OFF} //value digits might not be initialized
function ValUInt64_JOH_PAS_8_a_raw(const s: PAnsiChar; out code: Integer): UInt64;
//function ValInt64_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Int64;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified by EgonHugeist for faster conversion, PAnsiChar, UInt64
const
  AdjustLowercase = Ord('a') - 10;
  AdjustUppercase = Ord('A') - 10;
var
  I, Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
  P: PAnsiChar;
begin
  Result := 0;
  Code   := 0;
  if (S = nil) or (Ord(S^) = Ord(#0)) then
    begin
      inc(Code);
      Exit;
    end;
  Flags := 0;
  P := S;
  while Ord(P^) = Ord(' ') do
    Inc(P);
  if Ord(P^) in [Ord('+'), Ord('-')] then
    if Ord(P^) = Ord('-') then begin//can't be negative
      Code := P-S;
      Exit;
    end else begin
      Flags := Flags or (Ord(S^) - Ord('+')); {Set/Reset Neg}
      inc(P);
    end;
  if Ord(P^) = Ord('$') then begin
    inc(P);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if Ord(P^) = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
    if (Ord(P^) or $20) = ord('x') then begin {S[Code+1] in ['X','x']}
      Flags := Flags or 4; {Hex := True}
      inc(P);
    end;
  end;
  if (Flags and 4) <> 0 then begin {Hex = True}
    Flags := Flags and (not 1); {Valid := False}
    while true do begin
      case Ord(P^) of
        Ord('0')..Ord('9'): Digit := Ord(P^) - Ord('0');
        Ord('a')..Ord('f'): Digit := Ord(P^) - AdjustLowercase;
        Ord('A')..Ord('F'): Digit := Ord(P^) - AdjustUppercase;
        else      Break;
      end;
      if UInt64(Result) > (High(UInt64) shr 3) then
        Break;
      if UInt64(Result) < (MaxInt div 16)-15 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I shl 4) + Digit;
        Result := I;
      end else
        Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      Inc(P);
    end;
  end else begin
    while true do begin
      //High(Uint64) div 10 = $1999999999999999
      {$IFDEF WITH_UINT64_C1118_ERROR}
      if (( not (Ord(P^) in [Ord('0')..Ord('9')]) ) or ( (Ord(P^) > Ord('5')) and (Result >= $1999999999999999) )) then //prevent overflow
        if (Ord(P^) > Ord('5')) and ( Result = $1999999999999999) then begin //overflow
      {$ELSE}
      if (( not (Ord(P^) in [Ord('0')..Ord('9')]) ) or ( (Ord(P^) > Ord('5')) and (Result >= (High(UInt64) div 10)) )) then //prevent overflow
        if (Ord(P^) > Ord('5')) and ( Result = (High(UInt64) div 10)) then begin //overflow
      {$ENDIF}
          Code := P-S+1;
          Exit;
        end else begin
          inc(P, Ord(Ord(P^) <> Ord(#0)));
          break;
        end;
      if Result < (MaxInt div 10)-9 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I * 10) + Ord(P^) - Ord('0');
        Result := I;
      end else {Result := (Result * 10) + Ord(Ch) - Ord('0');}
        Result := (Result shl 1) + (Result shl 3) + Ord(P^) - Ord('0');
      Flags := Flags or 1; {Valid := True}
      Inc(P);
    end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if ((Flags and 1) <> 0) and (Ord(P^) = Ord(#0)) then
    Code := 0 {Valid=True and End Reached}
  else
    Code := P-S+1;
end;
{$WARNINGS ON}

{$WARNINGS OFF} //value digits might not be initialized
function ValUInt64_JOH_PAS_8_a_Unicode(const s: PWideChar; out code: Integer): UInt64;
//function ValInt64_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Int64;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified by EgonHugeist for faster conversion, PAnsiChar, UInt64
const
  AdjustLowercase = Ord('a') - 10;
  AdjustUppercase = Ord('A') - 10;
var
  I, Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
  P: PWideChar;
  W: PWord absolute P;
begin
  Result := 0;
  Code   := 0;
  if (S = nil) or (Ord(S^) = Ord(#0)) then begin
    inc(Code);
    Exit;
  end;
  Flags := 0;
  P := S;
  while W^ = Ord(' ') do
    Inc(P);
  if W^ in [Ord('+'), Ord('-')] then
    if W^ = Ord('-') then begin//can't be negative
      Code := P-S;
      Exit;
    end else
      inc(P);
  if W^ = Ord('$') then begin
    inc(P);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if W^ = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
    if (W^ or $20) = ord('x') then begin {S[Code+1] in ['X','x']}
      Flags := Flags or 4; {Hex := True}
      inc(P);
    end;
  end;
  if (Flags and 4) <> 0 then begin {Hex = True}
    Flags := Flags and (not 1); {Valid := False}
    while true do
      begin
        case W^ of
          Ord('0')..Ord('9'): Digit := W^ - Ord('0');
          Ord('a')..Ord('f'): Digit := W^ - AdjustLowercase;
          Ord('A')..Ord('F'): Digit := W^ - AdjustUppercase;
          else      Break;
        end;
        if UInt64(Result) > (High(UInt64) shr 3) then
          Break;
        if UInt64(Result) < (MaxInt div 16)-15 then
          begin {Use Integer Math instead of Int64}
            I := Result;
            I := (I shl 4) + Digit;
            Result := I;
          end
        else
          Result := (Result shl 4) + Digit;
        Flags := Flags or 1; {Valid := True}
        Inc(P);
      end;
  end else begin
    while true do begin
      {$IFDEF WITH_UINT64_C1118_ERROR}
      if (( not (Ord(W^) in [Ord('0')..Ord('9')]) ) or ( (Ord(W^) > Ord('5')) and (Result >= $1999999999999999) )) then //prevent overflow
        if (Ord(W^) > Ord('5')) and ( Result = $1999999999999999) then begin //overflow
      {$ELSE}
      if (( not (Ord(W^) in [Ord('0')..Ord('9')]) ) or ( (Ord(W^) > Ord('5')) and (Result >= (High(UInt64) div 10)) )) then //prevent overflow
        if (Ord(W^) > Ord('5')) and ( Result = (High(UInt64) div 10)) then begin //overflow
      {$ENDIF}
          Code := P-S+1;
          Exit;
        end else begin
          inc(P, Ord(W^ <> Ord(#0)));
          break;
        end;
      if UInt64(Result) < (MaxInt div 10)-9 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I * 10) + W^ - Ord('0');
        Result := I;
      end else {Result := (Result * 10) + Ord(Ch) - Ord('0');}
        Result := (Result shl 1) + (Result shl 3) + W^ - Ord('0');
      Flags := Flags or 1; {Valid := True}
      Inc(P);
    end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if ((Flags and 1) <> 0) and (P^ = #0) then
    Code := 0 {Valid=True and End Reached}
  else
    Code := P-S+1;
end;
{$WARNINGS ON}

{$IF defined(WIN32) and not defined(FPC)}
function ValInt64_JOH_IA32_8_a(const s: PAnsiChar; out code: Integer): Int64;
asm
  test  eax, eax
  jz    @@Null
  push  ebx
  push  esi
  push  edi
  push  edx                 {Save Code Address}
  push  eax                 {Save String Pointer}
  mov   esi, eax            {String Pointer}
  xor   ebx, ebx            {Clear Valid Flag and Sign Flag}
  xor   eax, eax            {Clear Result}
  xor   edx, edx
  jmp   @@TrimEntry
@@Null:
  mov   [edx], eax
  inc   [edx]               {Code = 1}
  xor   edx, edx            {Result = 0}
  ret
@@Trim:                     {Strip Leading Spaces}
  inc   esi
@@TrimEntry:
  movzx ecx, [esi]
  cmp   cl, ' '
  je    @@Trim
  cmp   cl, '0'
  jle   @@CheckFirstChar
@@CheckAlpha:
  test  cl, $87
  jz    @@CheckX            {May be 'x' or 'X'}
@@NumLoop:
  sub   ecx, '0'
  cmp   ecx, 9
  ja    @@NumDone           {Not '0'..'9'}
  cmp   edx, $0ccccccc
  jae   @@CheckNumRange     {May be Out of Range}
@@InRange:
  test  edx, edx
  jnz   @@LargeNum
  cmp   eax, MaxInt/10-9    {MaxInt div 10)-9}
  ja    @@LargeNum
  lea   eax, [eax*4+eax]
  lea   eax, [eax*2+ecx]    {Result = Result * 10 + Digit}
  jmp   @@DoneNumMul
@@LargeNum:
  mov   bh, cl              {Save Digit}
  add   eax, eax
  adc   edx, edx
  mov   ecx, eax
  mov   edi, edx            {edi:ecx = Result * 2}
  shld  edx, eax, 2
  add   eax, eax
  add   eax, eax            {edx:eax = Result * 8}
  add   eax, ecx
  adc   edx, edi            {Result = Result * 10}
  movzx ecx, bh             {Restore Digit}
  add   eax, ecx            {Add Digit to Result}
  adc   edx, 0
@@DoneNumMul:
  inc   esi
  mov   bl, 1               {Valid := True}
  movzx ecx, [esi]
  jmp   @@NumLoop
@@CheckNumRange:
  ja    @@SetSign           {Out of Range}
  cmp   eax, $cccccccc
  jna   @@InRange           {Within Range}
  jmp   @@SetSign
@@NumDone:
  cmp   edx, $80000000      {Check for Overflow}
  jb    @@SetSign
  jne   @@Overflow
  test  eax, eax
  jnz   @@Overflow
  test  ebx, ebx            {Sign Flag}
  js    @@Setsign           {Result is Valid (-MaxInt64-1)}
@@Overflow:
  dec   esi
  mov   bl, 0               {Valid := False}
  jmp   @@SetSign
@@CheckFirstChar:
  cmp   cl, '-'
  je    @@PlusMinus
  cmp   cl, '+'
  jne   @@SignSet
@@PlusMinus:                {Starts with '+' or '-'}
  mov   bl, '+'+1
  sub   ebx, ecx            {Set Sign Flag: '+' -> +1, '-' -> -1}
  inc   esi
  mov   bl, 0               {Valid := False}
  movzx ecx, [esi]          {Character after '+' or '-'}
@@SignSet:
  cmp   cl, '$'
  je    @@Hex               {Hexadecimal}
  cmp   cl, '0'
  jne   @@CheckAlpha        {May start with 'x' or 'X'}
  inc   esi
  mov   bl, 1               {Assume Valid = True}
  movzx ecx, [esi]          {Character after '0'}
  jmp   @@CheckAlpha        {May start with '0x' or '0X'}
@@CheckX:
  mov   bh, cl
  or    bh, $20             {'X' -> 'x'}
  cmp   bh, 'x'
  jne   @@NumLoop
@@Hex:
  mov   bl, 0               {Valid := False}
@@HexLoop:
  inc   esi
  movzx ecx, [esi]
  cmp   cl, 'a'
  jb    @@CheckNum
  sub   cl, 'a'-'A'         {'a' > 'A'}
@@CheckNum:
  sub   cl, '0'
  cmp   cl, 9
  jna   @@CheckHexRange     {'0'..'9'}
  sub   cl, 'A'-'0'
  cmp   cl, 5               {Valid Hex Character?}
  ja    @@NotHex            {No, Invalid}
  add   cl, 10              {Yes, Adjust Digit}
@@CheckHexRange:
  cmp   edx, $10000000
  jae   @@SetSign           {Overflow}
  shld  edx, eax, 4         {Result := Result * 16}
  shl   eax, 4
  add   eax, ecx            {Add Digit}
  adc   edx, 0
  mov   bl, 1               {Valid := True}
  jmp   @@HexLoop
@@NotHex:
  add   cl, 'A'-'0'         {Restore Char-'0'}
@@SetSign:
  mov   ch, bl              {Save Valid Flag}
  sar   ebx, 31             {Set Each Bit to Top Bit (Sign Flag)}
  xor   eax, ebx            {Negate Result if Necessary}
  xor   edx, ebx
  sub   eax, ebx
  sbb   edx, ebx
  dec   ch                  {0 if Valid, -1 if Invalid}
  or    cl, ch              {If Invalid, Force CL = -1}
  cmp   cl, -'0'
  jne   @@Error             {Not Valid or Not End of String}
  xor   esi, esi            {Code := 0}
  pop   ebx                 {Dump String Pointer}
@@Finished:
  pop   ecx
  mov   [ecx], esi          {Set Error Code}
  pop   edi
  pop   esi
  pop   ebx
  ret
@@Error:
  inc   esi
  pop   ecx                 {String Pointer}
  sub   esi, ecx
  jmp   @@Finished
end;
{$IFEND}

function RawToInt64Def(const S: RawByteString; const Default: Int64) : Int64;
begin
  Result := RawToInt64Def(Pointer(S), Default);
end;

function RawToInt64(const Value: RawByteString) : Int64;
var
  E: Integer;
begin
  {$IF defined(WIN32) and not defined(FPC)}
  Result := ValInt64_JOH_IA32_8_a(Pointer(Value), E);
  {$ELSE}
  Result := ValInt64_JOH_PAS_8_a_raw(Pointer(Value), E{%H-});
  {$IFEND}
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
end;

function RawToInt64Def(const S: PAnsiChar; const Default: Int64) : Int64;
var
  E: Integer;
begin
  {$IF defined(WIN32) and not defined(FPC)}
  Result := ValInt64_JOH_IA32_8_a(s, E);
  {$ELSE}
  Result := ValInt64_JOH_PAS_8_a_raw(S, E{%H-});
  {$IFEND}
  if E > 0 then
    if not ((E > 0) and Assigned(S) and (Ord((S+E-1)^)=Ord(' '))) then
      Result := Default;
end;

function RawToInt64Def(Buf, PEnd: PAnsiChar; const Default: Int64) : Int64;
var P: PAnsiChar;
begin
  P := PEnd;
  Result := ValRawInt64(Buf, PEnd);
  if P <> PEnd then
    Result := Default;
end;

function RawToUInt64Def(const S: PAnsiChar; const Default: UInt64) : UInt64;
var
  E: Integer;
begin
  Result := ValUInt64_JOH_PAS_8_a_raw(S, E{%H-});
  if E > 0 then
    if not ((E > 0) and Assigned(S) and (Ord((S+E-1)^)=Ord(' '))) then
      Result := Default;
end;

function RawToUInt64Def(Buf, PEnd: PAnsiChar; const Default: UInt64) : UInt64;
var P: PAnsiChar;
begin
  P := PEnd;
  Result := ValRawUInt64(Buf, P);
  if P <> PEnd then
    Result := Default;
end;

function RawToUInt64(const Value: RawByteString) : UInt64;
var
  E: Integer;
begin
  Result := ValUInt64_JOH_PAS_8_a_raw(Pointer(Value), E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
end;

function RawToUInt64Def(const S: RawByteString; const Default: UInt64) : UInt64;
begin
  Result := RawToUInt64Def(Pointer(S), Default);
end;

{$WARNINGS OFF} //value digits might not be initialized
function ValInt64_JOH_PAS_8_a_unicode(const s: PWideChar; out code: Integer): Int64;
//function ValInt64_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Int64;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified by EgonHugeist for faster conversion and PWideChar
const
  AdjustLowercase = Ord('a') - 10;
  AdjustUppercase = Ord('A') - 10;
var
  I, Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
  P: PWideChar;
  W: PWord absolute P; //FPC performance work around -> compiler is a crude using WideChar comparision
begin
  Result := 0;
  Code   := 0;
  if (S = nil) or (Ord(S^) = $0) then begin
    inc(Code);
    Exit;
  end;
  P := S;
  Flags := 0;
  while W^ = Ord(' ') do
    Inc(P);
  if W^ in [Ord('+'), Ord('-')] then begin
    Flags := Flags or (W^ - Ord('+')); {Set/Reset Neg}
    inc(P);
  end;
  if W^ = Ord('$') then begin
    inc(P);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if W^ = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
    if (W^ or $20) = Ord('x') then begin {S[Code+1] in ['X','x']}
      Flags := Flags or 4; {Hex := True}
      inc(P);
    end;
  end;
  if (Flags and 4) <> 0 then begin {Hex = True}
    Flags := Flags and (not 1); {Valid := False}
    while true do begin
      case W^ of
        Ord('0')..Ord('9'): Digit := W^ - Ord('0');
        Ord('a')..Ord('f'): Digit := W^ - AdjustLowercase;
        Ord('A')..Ord('F'): Digit := W^ - AdjustUppercase;
        else      Break;
      end;
      if UInt64(Result) > (HighInt64 shr 3) then
        Break;
      if UInt64(Result) < (MaxInt div 16)-15 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I shl 4) + Digit;
        Result := I;
      end else
        Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      inc(P);
    end;
  end else begin
    while true do begin
      if ( not (W^ in [Ord('0')..Ord('9')] )) or
         ( UInt64(Result) > (HighInt64 div 10)) then begin
        inc(P, Ord(W^ <> Ord(#0)));
        break;
      end;
      if UInt64(Result) < (MaxInt div 10)-9 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I * 10) + Ord(P^) - Ord('0');
        Result := I;
      end else {Result := (Result * 10) + Ord(Ch) - Ord('0');}
        Result := (Result shl 1) + (Result shl 3) + W^ - Ord('0');
      Flags := Flags or 1; {Valid := True}
      Inc(P);
    end;
    {$IFDEF FPC}
    if UInt64(Result) >= UInt64(Int64($8000000000000000)) then {Possible Overflow}
      if ((Flags and 2) = 0) or (UInt64(Result) <> UInt64(Int64($8000000000000000))) then
    {$ELSE}
    if UInt64(Result) >= $8000000000000000 then {Possible Overflow}
      if ((Flags and 2) = 0) or (UInt64(Result) <> $8000000000000000) then
    {$ENDIF}
        begin {Overflow}
          if ((Flags and 2) <> 0) then {Neg=True}
            Result := -Result;
          Code := P-S;
          Exit;
        end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if ((Flags and 1) <> 0) and (P^ = #0) then
    Code := 0 {Valid=True and End Reached}
  else
    Code := P-S+1;
end;
{$WARNINGS ON}

function UnicodeToInt64Def(const S: ZWideString; const Default: Int64) : Int64;
var
  E: Integer;
begin
  Result := ValInt64_JOH_PAS_8_a_unicode(Pointer(S), E{%H-});
  if E > 0 then
    if not ((E > 0) and Assigned(Pointer(S)) and ((S[E])=' ')) then
      Result := Default;
end;

function UnicodeToInt64(const Value: ZWideString) : Int64;
var
  E: Integer;
begin
  Result := ValInt64_JOH_PAS_8_a_unicode(Pointer(Value), E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
end;

function UnicodeToInt64Def(const S: PWideChar; const Default: Int64) : Int64;
var
  E: Integer;
begin
  Result := ValInt64_JOH_PAS_8_a_unicode(S, E{%H-});
  if E > 0 then
    if not ((E > 0) and Assigned(S) and ((S+E-1)^=' ')) then
      Result := Default;
end;

function UnicodeToInt64Def(Buf, PEnd: PWideChar; Default: Integer) : Int64;
var P: PWideChar;
begin
  P := PEnd;
  Result := ValUnicodeInt64(Buf, PEnd);
  if P <> PEnd then
    Result := Default;
end;

function UnicodeToUInt64(const Value: ZWideString) : UInt64;
var
  E: Integer;
begin
  Result := ValUInt64_JOH_PAS_8_a_unicode(Pointer(Value), E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [Value]);
end;

function UnicodeToUInt64Def(const S: ZWideString; const Default: UInt64) : UInt64;
var
  E: Integer;
begin
  Result := ValUInt64_JOH_PAS_8_a_unicode(Pointer(S), E{%H-});
  if E > 0 then
    if not ((E > 0) and Assigned(Pointer(S)) and ((S[E])=' ')) then
      Result := Default;
end;

function UnicodeToUInt64Def(const S: PWideChar; const Default: UInt64) : UInt64;
var
  E: Integer;
begin
  Result := ValUInt64_JOH_PAS_8_a_unicode(S, E{%H-});
  if E > 0 then
    if not ((E > 0) and Assigned(S) and ((S+E-1)^=' ')) then
      Result := Default;
end;

function UnicodeToUInt64Def(Buf, PEnd: PWideChar; Default: Integer) : UInt64;
var P: PWideChar;
begin
  P := PEnd;
  Result := ValUnicodeUInt64(Buf, PEnd);
  if P <> PEnd then
    Result := Default;
end;

function RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar;
  const Default: Extended): Extended;
var
  E: Integer;
begin
  Result :=  ValRawExt(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;

procedure RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar;
  const Default: Extended; var Result: Extended);
var
  E: Integer;
begin
  Result :=  ValRawExt(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;

procedure RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar;
  const Default: Currency; var Result: Currency);
var
  E: Integer;
begin
  Result :=  ValRawDbl(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar;
  const Default: Double; var Result: Double);
var
  E: Integer;
begin
  Result :=  ValRawDbl(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;
{$IFEND}

procedure RawToFloatDef(const s: PAnsiChar; const DecimalSep: AnsiChar;
  const Default: Single; var Result: Single);
var
  E: Integer;
begin
  Result :=  ValRawSin(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;

function RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar): Extended; overload;
var
  E: Integer;
begin
  Result :=  ValRawExt(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar; var Result: Extended); overload;
var
  E: Integer;
begin
  Result :=  ValRawExt(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;
{$IFEND}

procedure RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar; var Result: Currency); overload;
var
  E: Integer;
begin
  Result :=  ValRawDbl(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

procedure RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar; var Result: Double); overload;
var
  E: Integer;
begin
  Result :=  ValRawDbl(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

procedure RawToFloat(const s: PAnsiChar; const DecimalSep: AnsiChar; var Result: Single); overload;
var
  E: Integer;
begin
  Result :=  ValRawSin(Pointer(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

const
  EBase: Extended = 10.0;
  DBase: Double = 10.0;
  SBase: Single = 10.0;

//Author:            Dennis Kjaer Christensen
//Date:              15/10 2003
//Optimized for:     Blended
//Instructionset(s): IA32

//EH: Note this version replaces the pascal Match.IntPower which have a range overflow
{$IFDEF WITH_PUREPASCAL_INTPOWER}

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
function IntPowerDKCPas4(const Base: Extended; const Exponent: Integer): Extended; overload;
var
 I, I2, I3 : Integer;
 Result2 : Extended;
begin
 if Base = 0 then
  begin
   if Exponent = 0 then
    Result := 1
   else
    Result := 0;
  end
 else if Exponent = 0 then
  Result := 1
 else if Exponent = 1 then
  Result := Base
 else if Exponent = 2 then
  Result := Base * Base
 else if Exponent > 2 then
  begin
   Result := Base;
   Result2 := 1;
   I := 2;
   I2 := Exponent;
   repeat
    I3 := I2 and 1;
    if I3 = 1 then
     Result2 := Result2 * Result;
    I2 := I2 shr 1;
    Result := Result * Result;
    I := I * 2;
   until(I > Exponent);
   Result := Result * Result2;
  end
 else if Exponent = -1 then
  Result := 1/Base
 else if Exponent = -2 then
  Result := 1/(Base*Base)
 else //if Exponent < -2 then
  begin
   Result := Base;
   Result2 := 1;
   I := 2;
   I2 := -Exponent;
   repeat
    I3 := I2 and 1;
    if I3 = 1 then
     Result2 := Result2 * Result;
    I2 := I2 shr 1;
    Result := Result * Result;
    I := I * 2;
   until(I > -Exponent);
   Result := Result * Result2;
   Result := 1 / Result;
  end;
end;
{$IFEND}

function IntPowerDKCPas4(const Base: Double; const Exponent: Integer): Double; overload;
var
 I, I2, I3 : Integer;
 Result2 : Extended;
begin
 if Base = 0 then
  begin
   if Exponent = 0 then
    Result := 1
   else
    Result := 0;
  end
 else if Exponent = 0 then
  Result := 1
 else if Exponent = 1 then
  Result := Base
 else if Exponent = 2 then
  Result := Base * Base
 else if Exponent > 2 then
  begin
   Result := Base;
   Result2 := 1;
   I := 2;
   I2 := Exponent;
   repeat
    I3 := I2 and 1;
    if I3 = 1 then
     Result2 := Result2 * Result;
    I2 := I2 shr 1;
    Result := Result * Result;
    I := I * 2;
   until(I > Exponent);
   Result := Result * Result2;
  end
 else if Exponent = -1 then
  Result := 1/Base
 else if Exponent = -2 then
  Result := 1/(Base*Base)
 else //if Exponent < -2 then
  begin
   Result := Base;
   Result2 := 1;
   I := 2;
   I2 := -Exponent;
   repeat
    I3 := I2 and 1;
    if I3 = 1 then
     Result2 := Result2 * Result;
    I2 := I2 shr 1;
    Result := Result * Result;
    I := I * 2;
   until(I > -Exponent);
   Result := Result * Result2;
   Result := 1 / Result;
  end;
end;

function IntPowerDKCPas4(const Base: Single; const Exponent: Integer): Single; overload;
var
 I, I2, I3 : Integer;
 Result2 : Extended;
begin
 if Base = 0 then
  begin
   if Exponent = 0 then
    Result := 1
   else
    Result := 0;
  end
 else if Exponent = 0 then
  Result := 1
 else if Exponent = 1 then
  Result := Base
 else if Exponent = 2 then
  Result := Base * Base
 else if Exponent > 2 then
  begin
   Result := Base;
   Result2 := 1;
   I := 2;
   I2 := Exponent;
   repeat
    I3 := I2 and 1;
    if I3 = 1 then
     Result2 := Result2 * Result;
    I2 := I2 shr 1;
    Result := Result * Result;
    I := I * 2;
   until(I > Exponent);
   Result := Result * Result2;
  end
 else if Exponent = -1 then
  Result := 1/Base
 else if Exponent = -2 then
  Result := 1/(Base*Base)
 else //if Exponent < -2 then
  begin
   Result := Base;
   Result2 := 1;
   I := 2;
   I2 := -Exponent;
   repeat
    I3 := I2 and 1;
    if I3 = 1 then
     Result2 := Result2 * Result;
    I2 := I2 shr 1;
    Result := Result * Result;
    I := I * 2;
   until(I > -Exponent);
   Result := Result * Result2;
   Result := 1 / Result;
  end;
end;
{$ENDIF WITH_PUREPASCAL_INTPOWER}

function ValRawExt(const S: PByteArray; const DecimalSep: AnsiChar; out code: Integer): Extended;
//function ValExt_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Extended;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for varying DecimalSeperator
var
  Digits, ExpValue: Integer;
  Ch: Byte;
  Neg, NegExp, Valid: Boolean;
begin
  Result := 0.0;
  Code   := 0;
  if S = nil then begin
    inc(Code);
    Exit;
  end;
  Neg    := False;
  NegExp := False;
  Valid  := False;
  while S[code] = Ord(' ') do
    Inc(Code);
  Ch := S[code];
  if Ch in [Ord('+'), Ord('-')] then begin
    inc(Code);
    Neg := (Ch = Ord('-'));
  end;
  while true do begin
    Ch := S[code];
    inc(Code);
    if (Ch < Ord('0')) or (Ch > Ord('9')) then //if not (Ch in ['0'..'9']) then
      break;
    Result := (Result * 10) + Ch - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if Ch = Ord(DecimalSep) then
    while true do begin
      Ch := S[code];
      inc(Code);
      if (Ch < Ord('0')) or (Ch > Ord('9')) then begin //if not (Ch in ['0'..'9']) then
        if not valid then begin {Starts with '.'}
          if Ch = 0 then
            dec(code); {s = '.'}
        end;
        break;
      end;
      Result := (Result * 10) + Ch - Ord('0');
      Dec(Digits);
      Valid := true;
    end;
  ExpValue := 0;
  if (Ch or $20) = ord('e') then begin {Ch in ['E','e']}
    Valid := false;
    Ch := S[code];
    if Ch in [Ord('+'), Ord('-')] then begin
      inc(Code);
      NegExp := (Ch = Ord('-'));
    end;
    while true do begin
      Ch := S[code];
      inc(Code);
      if (Ch < Ord('0')) or (Ch > Ord('9')) then //if not (Ch in ['0'..'9']) then
        break;
      ExpValue := (ExpValue * 10) + Ch - Ord('0');
      Valid := true;
    end;
   if NegExp then
     ExpValue := -ExpValue;
  end;
  Digits := Digits + ExpValue;
  if Digits <> 0 then
    {$IFDEF WITH_PUREPASCAL_INTPOWER}
    Result := Result * IntPowerDKCPas4(EBase, Digits);
    {$ELSE}
    Result := Result * Math.IntPower(EBase, Digits);
    {$ENDIF}
  if Neg then
    Result := -Result;
  if Valid and (ch = $0) then
    code := 0;
end;

function ValRawDbl(const s: PByteArray; const DecimalSep: AnsiChar; out code: Integer): Double;
//function ValExt_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Extended;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for varying DecimalSeperator
var
  Digits, ExpValue: Integer;
  Ch: Byte;
  Neg, NegExp, Valid: Boolean;
begin
  Result := 0.0;
  Code   := 0;
  if S = nil then begin
    inc(Code);
    Exit;
  end;
  Neg    := False;
  NegExp := False;
  Valid  := False;
  while S[code] = Ord(' ') do
    Inc(Code);
  Ch := S[code];
  if Ch in [Ord('+'), Ord('-')] then begin
    inc(Code);
    Neg := (Ch = Ord('-'));
  end;
  if (Ch or $20 = Byte('n')) and not Neg then //test NAN (overrun safe)
    if (S[Code+1] or $20 = Byte('a')) and (S[Code+2] or $20 = Byte('n')) and (S[Code+3] = 0) then begin
      Code := 0;
      Result := NaN;
      Exit;
    end;
  if (S[code] or $20 = Byte('i')) then //test Infinity (overrun safe)
    if (S[Code+1] or $20 = Byte('n')) and (S[Code+2] or $20 = Byte('f')) and
       (S[Code+3] or $20 = Byte('i')) and (S[Code+4] or $20 = Byte('n')) and
       (S[Code+5] or $20 = Byte('i')) and (S[Code+6] or $20 = Byte('t')) and
       (S[Code+7] or $20 = Byte('y')) and (S[Code+8] = 0) then begin
      Code := 0;
      if Neg
      then Result := NegInfinity
      else Result := Infinity;
      Exit;
    end;
  while true do begin
    Ch := S[code];
    inc(Code);
    if (Ch < Ord('0')) or (Ch > Ord('9')) then //if not (Ch in ['0'..'9']) then
      break;
    Result := (Result * 10) + Ch - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if Ch = Ord(DecimalSep) then
    while true do begin
      Ch := S[code];
      inc(Code);
      if (Ch < Ord('0')) or (Ch > Ord('9')) then begin //if not (Ch in ['0'..'9']) then
        if not valid then begin {Starts with '.'}
          if Ch = 0 then
            dec(code); {s = '.'}
        end;
        break;
      end;
      Result := (Result * 10) + Ch - Ord('0');
      Dec(Digits);
      Valid := true;
    end;
  ExpValue := 0;
  if (Ch or $20) = ord('e') then begin {Ch in ['E','e']}
    Valid := false;
    Ch := S[code];
    if Ch in [Ord('+'), Ord('-')] then begin
      inc(Code);
      NegExp := (Ch = Ord('-'));
    end;
    while true do begin
      Ch := S[code];
      inc(Code);
      if (Ch < Ord('0')) or (Ch > Ord('9')) then //if not (Ch in ['0'..'9']) then
        break;
      ExpValue := (ExpValue * 10) + Ch - Ord('0');
      Valid := true;
    end;
   if NegExp then
     ExpValue := -ExpValue;
  end;
  Digits := Digits + ExpValue;
  if Digits <> 0 then
    {$IFDEF WITH_PUREPASCAL_INTPOWER}
    Result := Result * IntPowerDKCPas4(DBase, Digits);
    {$ELSE}
    Result := Result * Math.IntPower(DBase, Digits);
    {$ENDIF}
  if Neg then
    Result := -Result;
  if Valid and (ch = $0) then
    code := 0;
end;

function ValRawCurr(S: PByteArray; DecimalSep: Char; var Len: Integer): Currency;
//function ValExt_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Extended;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for varying DecimalSeperator, fixed Len and result is high prec Currency
var
  Digits, Code: Integer;
  Ch: Byte;
  Neg, Valid: Boolean;
  i64: Int64 absolute Result;
begin
  i64 := 0;
  Code   := 0;
  if S = nil then begin
    Len := 0;
    Exit;
  end;
  Neg    := False;
  Valid  := False;
  while S[code] = Ord(' ') do
    Inc(Code);
  Ch := S[code];
  if Ch in [Ord('+'), Ord('-')] then begin
    inc(Code);
    Neg := (Ch = Ord('-'));
  end;
  while Code < Len do begin
    Ch := S[code];
    inc(Code);
    if (Ch < Ord('0')) or (Ch > Ord('9')) then //if not (Ch in ['0'..'9']) then
      break;
    i64 := (i64 * 10) + Ch - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if Neg then
    i64 := -i64;
  if Ch = Ord(DecimalSep) then
    while (Code < Len) and (Digits < 4) do begin
      Ch := S[code];
      inc(Code);
      if (Ch < Ord('0')) or (Ch > Ord('9')) then begin //if not (Ch in ['0'..'9']) then
        if not valid then begin {Starts with '.'}
          if Ch = 0 then
            dec(code); {s = '.'}
        end;
        break;
      end;
      if Neg
      then i64 := (i64 * 10) {%H-}- (Ch - Ord('0'))
      else i64 := (i64 * 10) {%H-}+ (Ch - Ord('0'));
      Inc(Digits);
      Valid := true;
    end;
  if Digits < 4 then
    i64 := i64 * Int64Tower[4-Digits];
  if not Valid or (Code <> Len) then
    Len := code;
end;

function ValRawSin(const S: PByteArray; const DecimalSep: AnsiChar; out code: Integer): Single;
//function ValExt_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Extended;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for varying DecimalSeperator
var
  Digits, ExpValue: Integer;
  Ch: Byte;
  Neg, NegExp, Valid: Boolean;
begin
  Result := 0.0;
  Code   := 0;
  if S = nil then begin
    inc(Code);
    Exit;
  end;
  Neg    := False;
  NegExp := False;
  Valid  := False;
  while S[code] = Ord(' ') do
    Inc(Code);
  Ch := S[code];
  if Ch in [Ord('+'), Ord('-')] then begin
    inc(Code);
    Neg := (Ch = Ord('-'));
  end;
  if (Ch or $20 = Byte('n')) and not Neg then //test NAN (overrun safe)
    if (S[Code+1] or $20 = Byte('a')) and (S[Code+2] or $20 = Byte('n')) and (S[Code+3] = 0) then begin
      Code := 0;
      Result := NaN;
      Exit;
    end;
  if (S[code] or $20 = Byte('i')) then //test Infinity (overrun safe)
    if (S[Code+1] or $20 = Byte('n')) and (S[Code+2] or $20 = Byte('f')) and
       (S[Code+3] or $20 = Byte('i')) and (S[Code+4] or $20 = Byte('n')) and
       (S[Code+5] or $20 = Byte('i')) and (S[Code+6] or $20 = Byte('t')) and
       (S[Code+7] or $20 = Byte('y')) and (S[Code+8] = 0) then begin
      Code := 0;
      if Neg
      then Result := NegInfinity
      else Result := Infinity;
      Exit;
    end;
  while true do begin
    Ch := S[code];
    inc(Code);
    if (Ch < Ord('0')) or (Ch > Ord('9')) then //if not (Ch in ['0'..'9']) then
      break;
    Result := (Result * 10) + Ch - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if Ch = Ord(DecimalSep) then
    while true do begin
      Ch := S[code];
      inc(Code);
      if (Ch < Ord('0')) or (Ch > Ord('9')) then begin //if not (Ch in ['0'..'9']) then
        if not valid then begin {Starts with '.'}
          if Ch = 0 then
            dec(code); {s = '.'}
        end;
        break;
      end;
      Result := (Result * 10) + Ch - Ord('0');
      Dec(Digits);
      Valid := true;
    end;
  ExpValue := 0;
  if (Ch or $20) = ord('e') then begin {Ch in ['E','e']}
    Valid := false;
    Ch := S[code];
    if Ch in [Ord('+'), Ord('-')] then begin
      inc(Code);
      NegExp := (Ch = Ord('-'));
    end;
    while true do begin
      Ch := S[code];
      inc(Code);
      if (Ch < Ord('0')) or (Ch > Ord('9')) then //if not (Ch in ['0'..'9']) then
        break;
      ExpValue := (ExpValue * 10) + Ch - Ord('0');
      Valid := true;
    end;
   if NegExp then
     ExpValue := -ExpValue;
  end;
  Digits := Digits + ExpValue;
  if Digits <> 0 then
    {$IFDEF WITH_PUREPASCAL_INTPOWER}
    Result := Result * IntPowerDKCPas4(SBase, Digits);
    {$ELSE}
    Result := Result * Math.IntPower(SBase, Digits);
    {$ENDIF}
  if Neg then
    Result := -Result;
  if Valid and (ch = $0) then
    code := 0;
end;

function ValRawInt(const s: RawByteString; out code: Integer): Integer;
begin
  {$IF defined(WIN32) and not defined(FPC)}
  Result := ValLong_JOH_IA32_8_a(Pointer(s), Code);
  {$ELSE}
  Result := ValLong_JOH_PAS_4_b(Pointer(S), Code);
  {$IFEND}
end;

function ValRawInt(s: PAnsiChar; out code: Integer): Integer;
begin
  {$IF defined(WIN32) and not defined(FPC)}
  Result := ValLong_JOH_IA32_8_a(s, Code);
  {$ELSE}
  Result := ValLong_JOH_PAS_4_b(S, Code);
  {$IFEND}
end;

const
  AdjustLowercase = Ord('a') - 10;
  AdjustUppercase = Ord('A') - 10;

{$WARNINGS OFF} {Prevent False Compiler Warning on Digit not being Initialized}
function ValRawInt(PStart: PAnsiChar; var PEnd: PAnsiChar): Integer; overload;
//function ValLong_JOH_PAS_4_b(const S: PAnsiChar; out code: Integer): Integer;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//EH: -changed to NEXGEN save PByte support
//    -changed Code to a p(raw)char pointer
var
  Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
begin
  if (PStart = nil) or (PStart = PEnd)  then begin
    Result := 0;
    PEnd := PStart;
    Exit;
  end;
  Flags := 0;
  while Ord(PStart^) = Ord(' ') do
    Inc(PStart);
  if Ord(PStart^) in [Ord('+'), Ord('-')] then begin
    Flags := Flags or (Ord(PStart^) - Ord('+')); {Set/Reset Neg}
    inc(PStart);
  end;
  if Ord(PStart^) = Ord('$') then begin
    inc(PStart);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if Ord(PStart^) = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
    if (Ord(PStart^) or $20) = ord('x') then begin //Upcase(P^) = 'X'
      Flags := Flags or 4; {Hex := True}
      inc(PStart);
    end;
  end;
  Result := 0;
  if (Flags and 4) <> 0 then begin
    Flags := Flags and (not 1); {Valid := False}
    while PStart < PEnd do begin
      case Ord(PStart^) of
        Ord('0')..Ord('9'): Digit := Ord(PStart^) - Ord('0');
        Ord('a')..Ord('f'): Digit := Ord(PStart^) - AdjustLowercase;
        Ord('A')..Ord('F'): Digit := Ord(PStart^) - AdjustUppercase;
        else Break;
      end;
      if (Result < 0) or (Result > $0FFFFFFF) then
        Break;
      Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
  end else begin
    while PStart < PEnd do begin
      if not (Ord(PStart^) in [Ord('0')..Ord('9')]) then
        break;
      if Result > (MaxInt div 10) then
        break;
      Result := (Result * 10) + Ord(PStart^) - Ord('0');
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
    if Result < 0 then {Possible Overflow}
      if (Cardinal(Result) <> $80000000) or ((Flags and 2) = 0) then begin {Min(Integer) = $80000000 is a Valid Result}
        Dec(PStart);
        Flags := Flags or 1; {Valid := True}
      end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if not (((Flags and 1) <> 0) and (PStart = PEnd)) then
    PEnd := PStart;
end;

function ValRawInt64(PStart: PAnsiChar; var PEnd: PAnsiChar): Int64;
//function ValInt64_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Int64;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified by EgonHugeist for faster conversion and PAnsiChar/PByte
var
  I, Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
begin
  Result := 0;
  if (PStart = nil) or (PStart = PEnd) then begin
    PEnd := PStart;
    Exit;
  end;
  Flags := 0;
  while Ord(PStart^) = Ord(' ') do
    Inc(PStart);
  if Ord(PStart^) in [Ord('+'), Ord('-')] then begin
    Flags := Flags or Byte((Ord(PStart^) - Ord('+'))); {Set/Reset Neg}
    inc(PStart);
  end;
  if Ord(PStart^) = Ord('$') then begin
    inc(PStart);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if Ord(PStart^) = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
    if (Ord(PStart^) or $20) = ord('x') then begin {S[Code+1] in ['X','x']}
      Flags := Flags or 4; {Hex := True}
      inc(PStart);
    end;
  end;
  if (Flags and 4) <> 0 then begin {Hex = True}
    Flags := Flags and (not 1); {Valid := False}
    while PStart < PEnd do begin
      case Ord(PStart^) of
        Ord('0')..Ord('9'): Digit := Ord(PStart^) - Ord('0');
        Ord('a')..Ord('f'): Digit := Ord(PStart^) - AdjustLowercase;
        Ord('A')..Ord('F'): Digit := Ord(PStart^) - AdjustUppercase;
        else      Break;
      end;
      if UInt64(Result) > (HighInt64 shr 3) then
        Break;
      if UInt64(Result) < (MaxInt div 16)-15 then
        begin {Use Integer Math instead of Int64}
          I := Result;
          I := (I shl 4) + Digit;
          Result := I;
        end
      else
        Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      Inc(PStart);
    end;
  end else begin
    while PStart < PEnd do begin
      if ( not (Ord(PStart^) in [Ord('0')..Ord('9')]) ) or
         ( UInt64(Result) > (HighInt64 div 10)) then begin
        inc(PStart, Ord(Ord(PStart^) <> Ord(#0)));
        break;
      end;
      if UInt64(Result) < (MaxInt div 10)-9 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I * 10) + Ord(PStart^) - Ord('0');
        Result := I;
      end else {Result := (Result * 10) + Ord(Ch) - Ord('0');}
        Result := (Result shl 1) + (Result shl 3) + Ord(PStart^) - Ord('0');
      Flags := Flags or 1; {Valid := True}
      Inc(PStart);
    end;
    {$IFDEF FPC}
    if UInt64(Result) >= UInt64(Int64($8000000000000000)) then {Possible Overflow}
      if ((Flags and 2) = 0) or (UInt64(Result) <> UInt64(Int64($8000000000000000))) then
    {$ELSE}
    if UInt64(Result) >= $8000000000000000 then {Possible Overflow}
      if ((Flags and 2) = 0) or (Result <> $8000000000000000) then
    {$ENDIF}
        begin {Overflow}
          if ((Flags and 2) <> 0) then {Neg=True}
            Result := -Result;
          PEnd := PStart;
          Exit;
        end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if not (((Flags and 1) <> 0) and (PStart = PEnd)) then
    PEnd := PStart;
end;

function ValRawUInt64(PStart: PAnsiChar; var PEnd: PAnsiChar): UInt64; overload;
//function ValInt64_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Int64;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified by EgonHugeist for faster conversion, PAnsiChar, UInt64
var
  I, Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
begin
  Result := 0;
  if (PStart = nil) or (PStart = PEnd) then begin
    PEnd := PStart;
    Exit;
  end;
  Flags := 0;
  while Ord(PStart^) = Ord(' ') do
    Inc(PStart);
  if Ord(PStart^) in [Ord('+'), Ord('-')] then
    if Ord(PStart^) = Ord('-') then begin//can't be negative
      PEnd := PStart;
      Exit;
    end else
      inc(PStart);
  if Ord(PStart^) = Ord('$') then begin
    inc(PStart);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if Ord(PStart^) = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
    if (Ord(PStart^) or $20) = ord('x') then begin {S[Code+1] in ['X','x']}
      Flags := Flags or 4; {Hex := True}
      inc(PStart);
    end;
  end;
  if (Flags and 4) <> 0 then begin {Hex = True}
    Flags := Flags and (not 1); {Valid := False}
    while PStart < PEnd do begin
      case Ord(PStart^) of
        Ord('0')..Ord('9'): Digit := Ord(PStart^) - Ord('0');
        Ord('a')..Ord('f'): Digit := Ord(PStart^) - AdjustLowercase;
        Ord('A')..Ord('F'): Digit := Ord(PStart^) - AdjustUppercase;
        else      Break;
      end;
      if UInt64(Result) > (High(UInt64) shr 3) then
        Break;
      if UInt64(Result) < (MaxInt div 16)-15 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I shl 4) + Digit;
        Result := I;
      end else
        Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      Inc(PStart);
    end;
  end else begin
    while PStart < PEnd do begin
      if ( not (Ord(PStart^) in [Ord('0')..Ord('9')]) ) or ( (Ord(PStart^) > Ord('5')) and (Result = (High(UInt64) div 10)) ) then //prevent overflow
      if (Ord(PStart^) > Ord('5')) and ( Result = (High(UInt64) div 10)) then begin //overflow
          PEnd := PStart+1;
          Exit;
        end else begin
          inc(PStart, Ord(Ord(PStart^) <> Ord(#0)));
          break;
        end;
      if UInt64(Result) < (MaxInt div 10)-9 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I * 10) + Ord(PStart^) - Ord('0');
        Result := I;
      end else {Result := (Result * 10) + Ord(Ch) - Ord('0');}
        Result := (Result shl 1) + (Result shl 3) + Ord(PStart^) - Ord('0');
      Flags := Flags or 1; {Valid := True}
      Inc(PStart);
    end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if not (((Flags and 1) <> 0) and (PStart = PEnd)) then
    PEnd := PStart;
end;

{$WARNINGS OFF} {Prevent False Compiler Warning on Digit not being Initialized}
function ValUnicodeInt(PStart: PWideChar; var PEnd: PWideChar): Integer; overload;
//function ValLong_JOH_PAS_4_b(const S: PAnsiChar; out code: Integer): Integer;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//EH: -changed to NEXGEN save PByte support
//    -changed Code to a p(raw)char pointer
var
  Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
begin
  if (PStart = nil) or (PStart = PEnd)  then begin
    Result := 0;
    PEnd := PStart;
    Exit;
  end;
  Flags := 0;
  while Ord(PStart^) = Ord(' ') do
    Inc(PStart);
  if Ord(PStart^) in [Ord('+'), Ord('-')] then begin
    Flags := Flags or (Ord(PStart^) - Ord('+')); {Set/Reset Neg}
    inc(PStart);
  end;
  if Ord(PStart^) = Ord('$') then begin
    inc(PStart);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if Ord(PStart^) = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
    if (Ord(PStart^) or $20) = ord('x') then begin //Upcase(P^) = 'X'
      Flags := Flags or 4; {Hex := True}
      inc(PStart);
    end;
  end;
  Result := 0;
  if (Flags and 4) <> 0 then begin
    Flags := Flags and (not 1); {Valid := False}
    while PStart < PEnd do begin
      case Ord(PStart^) of
        Ord('0')..Ord('9'): Digit := Ord(PStart^) - Ord('0');
        Ord('a')..Ord('f'): Digit := Ord(PStart^) - AdjustLowercase;
        Ord('A')..Ord('F'): Digit := Ord(PStart^) - AdjustUppercase;
        else Break;
      end;
      if (Result < 0) or (Result > $0FFFFFFF) then
        Break;
      Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
  end else begin
    while PStart < PEnd do begin
      if not (Ord(PStart^) in [Ord('0')..Ord('9')]) then
        break;
      if Result > (MaxInt div 10) then
        break;
      Result := (Result * 10) + Ord(PStart^) - Ord('0');
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
    if Result < 0 then {Possible Overflow}
      if (Cardinal(Result) <> $80000000) or ((Flags and 2) = 0) then begin {Min(Integer) = $80000000 is a Valid Result}
        Dec(PStart);
        Flags := Flags or 1; {Valid := True}
      end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if not (((Flags and 1) <> 0) and (PStart = PEnd)) then
    PEnd := PStart;
end;

function ValUnicodeInt64(PStart: PWideChar; var PEnd: PWideChar): Int64;
//function ValInt64_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Int64;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified by EgonHugeist for faster conversion and PAnsiChar/PByte
var
  I, Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
begin
  Result := 0;
  if (PStart = nil) or (PStart = PEnd) then begin
    PEnd := PStart;
    Exit;
  end;
  Flags := 0;
  while Ord(PStart^) = Ord(' ') do
    Inc(PStart);
  if Ord(PStart^) in [Ord('+'), Ord('-')] then begin
    Flags := Flags or Byte((Ord(PStart^) - Ord('+'))); {Set/Reset Neg}
    inc(PStart);
  end;
  if Ord(PStart^) = Ord('$') then begin
    inc(PStart);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if Ord(PStart^) = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
    if (Ord(PStart^) or $20) = ord('x') then begin {S[Code+1] in ['X','x']}
      Flags := Flags or 4; {Hex := True}
      inc(PStart);
    end;
  end;
  if (Flags and 4) <> 0 then begin {Hex = True}
    Flags := Flags and (not 1); {Valid := False}
    while PStart < PEnd do begin
      case Ord(PStart^) of
        Ord('0')..Ord('9'): Digit := Ord(PStart^) - Ord('0');
        Ord('a')..Ord('f'): Digit := Ord(PStart^) - AdjustLowercase;
        Ord('A')..Ord('F'): Digit := Ord(PStart^) - AdjustUppercase;
        else      Break;
      end;
      if UInt64(Result) > (HighInt64 shr 3) then
        Break;
      if UInt64(Result) < (MaxInt div 16)-15 then
        begin {Use Integer Math instead of Int64}
          I := Result;
          I := (I shl 4) + Digit;
          Result := I;
        end
      else
        Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      Inc(PStart);
    end;
  end else begin
    while PStart < PEnd do begin
      if ( not (Ord(PStart^) in [Ord('0')..Ord('9')]) ) or
         ( UInt64(Result) > (HighInt64 div 10)) then begin
        inc(PStart, Ord(Ord(PStart^) <> Ord(#0)));
        break;
      end;
      if UInt64(Result) < (MaxInt div 10)-9 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I * 10) + Ord(PStart^) - Ord('0');
        Result := I;
      end else {Result := (Result * 10) + Ord(Ch) - Ord('0');}
        Result := (Result shl 1) + (Result shl 3) + Ord(PStart^) - Ord('0');
      Flags := Flags or 1; {Valid := True}
      Inc(PStart);
    end;
    {$IFDEF FPC}
    if UInt64(Result) >= UInt64(Int64($8000000000000000)) then {Possible Overflow}
      if ((Flags and 2) = 0) or (UInt64(Result) <> UInt64(Int64($8000000000000000))) then
    {$ELSE}
    if UInt64(Result) >= $8000000000000000 then {Possible Overflow}
      if ((Flags and 2) = 0) or (Result <> $8000000000000000) then
    {$ENDIF}
        begin {Overflow}
          if ((Flags and 2) <> 0) then {Neg=True}
            Result := -Result;
          PEnd := PStart;
          Exit;
        end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if not (((Flags and 1) <> 0) and (PStart = PEnd)) then
    PEnd := PStart;
end;

function ValUnicodeUInt64(PStart: PWideChar; var PEnd: PWideChar): UInt64;
//function ValInt64_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Int64;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified by EgonHugeist for faster conversion, PAnsiChar, UInt64
var
  I, Digit: Integer;
  Flags: Byte; {Bit 0 = Valid, Bit 1 = Negative, Bit 2 = Hex}
begin
  Result := 0;
  if (PStart = nil) or (PStart = PEnd) then begin
    PEnd := PStart;
    Exit;
  end;
  Flags := 0;
  while Ord(PStart^) = Ord(' ') do
    Inc(PStart);
  if Ord(PStart^) in [Ord('+'), Ord('-')] then
    if Ord(PStart^) = Ord('-') then begin//can't be negative
      PEnd := PStart;
      Exit;
    end else
      inc(PStart);
  if Ord(PStart^) = Ord('$') then begin
    inc(PStart);
    Flags := Flags or 4; {Hex := True}
  end else begin
    if Ord(PStart^) = Ord('0') then begin
      Flags := Flags or 1; {Valid := True}
      inc(PStart);
    end;
    if (Ord(PStart^) or $20) = ord('x') then begin {S[Code+1] in ['X','x']}
      Flags := Flags or 4; {Hex := True}
      inc(PStart);
    end;
  end;
  if (Flags and 4) <> 0 then begin {Hex = True}
    Flags := Flags and (not 1); {Valid := False}
    while PStart < PEnd do begin
      case Ord(PStart^) of
        Ord('0')..Ord('9'): Digit := Ord(PStart^) - Ord('0');
        Ord('a')..Ord('f'): Digit := Ord(PStart^) - AdjustLowercase;
        Ord('A')..Ord('F'): Digit := Ord(PStart^) - AdjustUppercase;
        else      Break;
      end;
      if UInt64(Result) > (High(UInt64) shr 3) then
        Break;
      if UInt64(Result) < (MaxInt div 16)-15 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I shl 4) + Digit;
        Result := I;
      end else
        Result := (Result shl 4) + Digit;
      Flags := Flags or 1; {Valid := True}
      Inc(PStart);
    end;
  end else begin
    while PStart < PEnd do begin
      if ( not (Ord(PStart^) in [Ord('0')..Ord('9')]) ) or ( (Ord(PStart^) > Ord('5')) and (Result = (High(UInt64) div 10)) ) then //prevent overflow
      if (Ord(PStart^) > Ord('5')) and ( Result = (High(UInt64) div 10)) then begin //overflow
          PEnd := PStart+1;
          Exit;
        end else begin
          inc(PStart, Ord(Ord(PStart^) <> Ord(#0)));
          break;
        end;
      if UInt64(Result) < (MaxInt div 10)-9 then begin {Use Integer Math instead of Int64}
        I := Result;
        I := (I * 10) + Ord(PStart^) - Ord('0');
        Result := I;
      end else {Result := (Result * 10) + Ord(Ch) - Ord('0');}
        Result := (Result shl 1) + (Result shl 3) + Ord(PStart^) - Ord('0');
      Flags := Flags or 1; {Valid := True}
      Inc(PStart);
    end;
  end;
  if ((Flags and 2) <> 0) then {Neg=True}
    Result := -Result;
  if not (((Flags and 1) <> 0) and (PStart = PEnd)) then
    PEnd := PStart;
end;

{$WARNINGS ON}

function UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar): Extended;
var
  E: Integer;
begin
  Result :=  ValUnicodeExt(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar; var Result: Extended);
var
  E: Integer;
begin
  Result :=  ValUnicodeExt(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;
{$IFEND}

procedure UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar; var Result: Currency); overload;
var
  E: Integer;
begin
  Result :=  ValUnicodeExt(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

procedure UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar; var Result: Double); overload;
var
  E: Integer;
begin
  Result :=  ValUnicodeDbl(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

procedure UnicodeToFloat(const s: PWideChar; const DecimalSep: WideChar; var Result: Single); overload;
var
  E: Integer;
begin
  Result :=  ValUnicodeSin(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidFloat, [s]);
end;

function UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Extended): Extended;
var
  E: Integer;
begin
  Result :=  ValUnicodeExt(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;

{$IF defined(DELPHI) or defined(FPC_HAS_TYPE_EXTENDED)}
procedure UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Extended; var Result: Extended);
var
  E: Integer;
begin
  Result :=  ValUnicodeExt(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;
{$IFEND}

procedure UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Currency; var Result: Currency);
var
  E: Integer;
begin
  Result :=  ValUnicodeDbl(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;

procedure UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Double; var Result: Double);
var
  E: Integer;
begin
  Result :=  ValUnicodeDbl(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;

procedure UnicodeToFloatDef(const s: PWideChar; const DecimalSep: WideChar; const Default: Single; var Result: Single);
var
  E: Integer;
begin
  Result :=  ValUnicodeSin(PWordArray(s), DecimalSep, E{%H-});
  if E <> 0 then Result := Default;
end;

{$WARNINGS OFF} //suppress a wrong warning!!
function ValUnicodeExt(const s: PWordArray; const DecimalSep: WideChar; out code: Integer): Extended;
//function ValExt_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Extended;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for varying DecimalSeperator and Fast conversion for FPC too (PWideChar is dead slow)
var
  Digits, ExpValue: Integer;
  W: Word;
  Neg, NegExp, Valid: Boolean;
begin
  Result := 0.0;
  Code   := 0;
  if s = nil then
    begin
      inc(Code);
      Exit;
    end;
  Neg    := False;
  NegExp := False;
  Valid  := False;
  while S[code] = Ord(' ') do
    Inc(Code);
  W := S[code];
  if W in [Ord('+'), Ord('-')] then begin
    inc(Code);
    Neg := (W = Ord('-'));
  end;
  while true do begin
    W := S[code];
    inc(Code);
    if not (W in [Ord('0')..Ord('9')]) then
      break;
    Result := (Result * 10) + W - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if W = Ord(DecimalSep) then begin
    while true do
      begin
        W := S[code];
        inc(Code);
        if not (W in [Ord('0')..Ord('9')]) then
        begin
          if not valid then {Starts with '.'}
            if W = Ord(#0) then
              dec(code); {s = '.'}
          break;
        end;
        Result := (Result * 10) + W - Ord('0');
        Dec(Digits);
        Valid := true;
      end;
    end;
  ExpValue := 0;
  if (W or $20) = ord('e') then
    begin {Ch in ['E','e']}
      Valid := false;
      W := S[code];
      if W in [Ord('+'), Ord('-')] then
        begin
          inc(Code);
          NegExp := (W = Ord('-'));
        end;
      while true do
        begin
          W := S[code];
          inc(Code);
          if not (W in [Ord('0')..Ord('9')]) then
            break;
          ExpValue := (ExpValue * 10) + W - Ord('0');
          Valid := true;
        end;
     if NegExp then
       ExpValue := -ExpValue;
    end;
  Digits := Digits + ExpValue;
  if Digits <> 0 then
    {$IFDEF WITH_PUREPASCAL_INTPOWER}
    Result := Result * IntPowerDKCPas4(EBase, Digits);
    {$ELSE}
    Result := Result * Math.IntPower(EBase, Digits);
    {$ENDIF}
  if Neg then
    Result := -Result;
  if Valid and (W = Ord(#0)) then
    code := 0;
end;

function ValUnicodeDbl(const s: PWordArray; const DecimalSep: WideChar; out code: Integer): Double;
//function ValExt_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Extended;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for varying DecimalSeperator and Fast conversion for FPC too (PWideChar is dead slow)
var
  Digits, ExpValue: Integer;
  W: Word;
  Neg, NegExp, Valid: Boolean;
begin
  Result := 0.0;
  Code   := 0;
  if s = nil then
    begin
      inc(Code);
      Exit;
    end;
  Neg    := False;
  NegExp := False;
  Valid  := False;
  while S[code] = Ord(' ') do
    Inc(Code);
  W := S[code];
  if W in [Ord('+'), Ord('-')] then begin
    inc(Code);
    Neg := (W = Ord('-'));
  end;
  if (W or $0020 = Byte('n')) and not Neg then //test NAN (overrun safe)
    if (S[Code+1] or $0020 = Byte('a')) and (S[Code+2] or $0020 = Byte('n')) and (S[Code+3] = 0) then begin
      Code := 0;
      Result := NaN;
      Exit;
    end;
  if (S[code] or $0020 = Byte('i')) then //test Infinity (overrun safe)
    if (S[Code+1] or $0020 = Byte('n')) and (S[Code+2] or $0020 = Byte('f')) and
       (S[Code+3] or $0020 = Byte('i')) and (S[Code+4] or $0020 = Byte('n')) and
       (S[Code+5] or $0020 = Byte('i')) and (S[Code+6] or $0020 = Byte('t')) and
       (S[Code+7] or $0020 = Byte('y')) and (S[Code+8] = 0) then begin
      Code := 0;
      if Neg
      then Result := NegInfinity
      else Result := Infinity;
      Exit;
    end;
  while true do begin
    W := S[code];
    inc(Code);
    if not (W in [Ord('0')..Ord('9')]) then
      break;
    Result := (Result * 10) + W - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if W = Ord(DecimalSep) then begin
    while true do
      begin
        W := S[code];
        inc(Code);
        if not (W in [Ord('0')..Ord('9')]) then
        begin
          if not valid then {Starts with '.'}
            if W = Ord(#0) then
              dec(code); {s = '.'}
          break;
        end;
        Result := (Result * 10) + W - Ord('0');
        Dec(Digits);
        Valid := true;
      end;
    end;
  ExpValue := 0;
  if (W or $20) = ord('e') then
    begin {Ch in ['E','e']}
      Valid := false;
      W := S[code];
      if W in [Ord('+'), Ord('-')] then
        begin
          inc(Code);
          NegExp := (W = Ord('-'));
        end;
      while true do
        begin
          W := S[code];
          inc(Code);
          if not (W in [Ord('0')..Ord('9')]) then
            break;
          ExpValue := (ExpValue * 10) + W - Ord('0');
          Valid := true;
        end;
     if NegExp then
       ExpValue := -ExpValue;
    end;
  Digits := Digits + ExpValue;
  if Digits <> 0 then
    {$IFDEF WITH_PUREPASCAL_INTPOWER}
    Result := Result * IntPowerDKCPas4(DBase, Digits);
    {$ELSE}
    Result := Result * Math.IntPower(DBase, Digits);
    {$ENDIF}
  if Neg then
    Result := -Result;
  if Valid and (W = Ord(#0)) then
    code := 0;
end;

function ValUnicodeSin(const s: PWordArray; const DecimalSep: WideChar; out code: Integer): Single;
//function ValExt_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Extended;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for varying DecimalSeperator
//modified for varying DecimalSeperator and Fast conversion for FPC too (PWideChar is dead slow)
var
  Digits, ExpValue: Integer;
  W: Word;
  Neg, NegExp, Valid: Boolean;
begin
  Result := 0.0;
  Code   := 0;
  if s = nil then
    begin
      inc(Code);
      Exit;
    end;
  Neg    := False;
  NegExp := False;
  Valid  := False;
  while S[code] = Ord(' ') do
    Inc(Code);
  W := S[code];
  if W in [Ord('+'), Ord('-')] then begin
    inc(Code);
    Neg := (W = Ord('-'));
  end;
  if (W or $0020 = Byte('n')) and not Neg then //test NAN (overrun safe)
    if (S[Code+1] or $0020 = Byte('a')) and (S[Code+2] or $0020 = Byte('n')) and (S[Code+3] = 0) then begin
      Code := 0;
      Result := NaN;
      Exit;
    end;
  if (S[code] or $0020 = Byte('i')) then //test Infinity (overrun safe)
    if (S[Code+1] or $0020 = Byte('n')) and (S[Code+2] or $0020 = Byte('f')) and
       (S[Code+3] or $0020 = Byte('i')) and (S[Code+4] or $0020 = Byte('n')) and
       (S[Code+5] or $0020 = Byte('i')) and (S[Code+6] or $0020 = Byte('t')) and
       (S[Code+7] or $0020 = Byte('y')) and (S[Code+8] = 0) then begin
      Code := 0;
      if Neg
      then Result := NegInfinity
      else Result := Infinity;
      Exit;
    end;
  while true do begin
    W := S[code];
    inc(Code);
    if not (W in [Ord('0')..Ord('9')]) then
      break;
    Result := (Result * 10) + W - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if W = Ord(DecimalSep) then begin
    while true do
      begin
        W := S[code];
        inc(Code);
        if not (W in [Ord('0')..Ord('9')]) then
        begin
          if not valid then {Starts with '.'}
            if W = Ord(#0) then
              dec(code); {s = '.'}
          break;
        end;
        Result := (Result * 10) + W - Ord('0');
        Dec(Digits);
        Valid := true;
      end;
    end;
  ExpValue := 0;
  if (W or $20) = ord('e') then
    begin {Ch in ['E','e']}
      Valid := false;
      W := S[code];
      if W in [Ord('+'), Ord('-')] then
        begin
          inc(Code);
          NegExp := (W = Ord('-'));
        end;
      while true do
        begin
          W := S[code];
          inc(Code);
          if not (W in [Ord('0')..Ord('9')]) then
            break;
          ExpValue := (ExpValue * 10) + W - Ord('0');
          Valid := true;
        end;
     if NegExp then
       ExpValue := -ExpValue;
    end;
  Digits := Digits + ExpValue;
  if Digits <> 0 then
    {$IFDEF WITH_PUREPASCAL_INTPOWER}
    Result := Result * IntPowerDKCPas4(SBase, Digits);
    {$ELSE}
    Result := Result * Math.IntPower(SBase, Digits);
    {$ENDIF}
  if Neg then
    Result := -Result;
  if Valid and (W = Ord(#0)) then
    code := 0;
end;

function ValUnicodeCurr(s: PWordArray; DecimalSep: Char; var Len: Integer): Currency;
//function ValExt_JOH_PAS_8_a(const s: AnsiString; out code: Integer): Extended;
//fast pascal from John O'Harrow see:
//http://www.fastcode.dk/fastcodeproject/fastcodeproject/61.htm
//modified for varying DecimalSeperator, fixed Len and result is high prec Currency
var
  Digits, Code: Integer;
  Ch: Word;
  Neg, Valid: Boolean;
  i64: Int64 absolute Result;
begin
  i64 := 0;
  Code   := 0;
  if S = nil then begin
    Len := 0;
    Exit;
  end;
  Neg    := False;
  Valid  := False;
  while S[code] = Ord(' ') do
    Inc(Code);
  Ch := S[code];
  if Ch in [Ord('+'), Ord('-')] then begin
    inc(Code);
    Neg := (Ch = Ord('-'));
  end;
  while Code < Len do begin
    Ch := S[code];
    inc(Code);
    if (Ch < Ord('0')) or (Ch > Ord('9')) then //if not (Ch in ['0'..'9']) then
      break;
    i64 := (i64 * 10) + Ch - Ord('0');
    Valid := True;
  end;
  Digits := 0;
  if Neg then
    i64 := -i64;
  if Ch = Ord(DecimalSep) then
    while (Code < Len) and (Digits < 4) do begin
      Ch := S[code];
      inc(Code);
      if (Ch < Ord('0')) or (Ch > Ord('9')) then begin //if not (Ch in ['0'..'9']) then
        if not valid then begin {Starts with '.'}
          if Ch = 0 then
            dec(code); {s = '.'}
        end;
        break;
      end;
      if Neg
      then i64 := (i64 * 10) {%H-}- (Ch - Ord('0'))
      else i64 := (i64 * 10) {%H-}+ (Ch - Ord('0'));
      Inc(Digits);
      Valid := true;
    end;
  if Digits < 4 then
    i64 := i64 * Int64Tower[4-Digits];
  if not Valid or (Code <> Len) then
    Len := code;
end;

{$IFDEF USE_FAST_TRUNC}
procedure RaiseInvalidOpException;
begin
  raise EInvalidOp.Create('Trunc error: The floating point number is outside the Int64 range.');
end;

function Trunc(const X: Extended): Int64;
//function TruncExtended_PLR_IA32_1(const X: Extended): Int64;
asm
  {On entry: X = [esp + 8]}
  {Extended variable layout: S1E15M64}
  {Get the mantissa into edx: eax}
  mov eax, [esp + 8]
  mov edx, [esp + 12]
  {Get the exponent and sign in ecx}
  movsx ecx, word ptr [esp + 16]
  {Save the sign in ebp}
  mov ebp, ecx
  {Mask out the sign bit of ecx}
  and ecx, $7fff
  {Get the number of positions to shift}
  neg ecx
  add ecx, 16383 + 63
  {Negative shift = number is too large}
  cmp ecx, 32
  jb @ShiftLessThan32Bits
  sub ecx, 32
  mov eax, edx
  xor edx, edx
  cmp ecx, 31
  ja @AbsoluteValueLessThan1OrOverFlow
@ShiftLessThan32Bits:
  shrd eax, edx, cl
  shr edx, cl
  {Is the number negative?}
  test ebp, ebp
  js @NegativeNumber
  {Sign bit may not be set for positive numbers}
  test edx, edx
  jns @Done
  jmp RaiseInvalidOpException
@NegativeNumber:
  {Negate the result}
  neg eax
  adc edx, 0
  neg edx
  jns RaiseInvalidOpException
  jmp @Done
@AbsoluteValueLessThan1OrOverFlow:
  test ecx, ecx
  js RaiseInvalidOpException
  xor eax, eax
@Done:
end;

function Trunc(const X: Double): Int64;
//function TruncDouble_PLR_IA32_1(const X: Double): Int64;
asm
  {On entry: X = [esp + 8]}
  {Double variable layout: S1E11M52}
  {Get the mantissa into edx:eax}
  mov eax, [esp + 8]
  mov ecx, [esp + 12]
  mov edx, ecx
  {Save the sign in ebp}
  mov ebp, ecx
  {Get the exponent in ecx}
  and ecx, $7fffffff
  shr ecx, 20
  {Get the number of positions to shift the mantissa}
  neg ecx
  add ecx, 1023 + 63
  {Shift the mantissa all the way to the left}
  shld edx, eax, 11
  shl eax, 11
  {Add the implied 1 bit to the mantissa}
  or edx, $80000000
  {Negative shift = number is too large}
  cmp ecx, 32
  jb @ShiftLessThan32Bits
  sub ecx, 32
  mov eax, edx
  xor edx, edx
  cmp ecx, 31
  ja @AbsoluteValueLessThan1OrOverFlow
@ShiftLessThan32Bits:
  shrd eax, edx, cl
  shr edx, cl
  {Is the number negative?}
  test ebp, ebp
  js @NegativeNumber
  {Sign bit may not be set for positive numbers}
  test edx, edx
  jns @Done
  jmp RaiseInvalidOpException
@NegativeNumber:
  {Negate the result}
  neg eax
  adc edx, 0
  neg edx
  jns RaiseInvalidOpException
  jmp @Done
@AbsoluteValueLessThan1OrOverFlow:
  test ecx, ecx
  js RaiseInvalidOpException
  xor eax, eax
@Done:
end;

function Trunc(const X: Single): Int64;
//function TruncSingle_PLR_IA32_1(const X: Single): Int64;
asm
  {On entry: X = [esp + 8]}
  {Double variable layout: S1E8M23}
  {Get the mantissa into edx}
  mov ecx, [esp + 8]
  mov edx, ecx
  {Save the sign in ebp}
  mov ebp, ecx
  {Get the exponent in ecx}
  and ecx, $7fffffff
  shr ecx, 23
  {Get the number of positions to shift the mantissa}
  neg ecx
  add ecx, 127 + 63
  {Shift the mantissa all the way to the left}
  shl edx, 8
  {Add the implied 1 bit to the mantissa}
  or edx, $80000000
  xor eax, eax
  {Negative shift = number is too large}
  cmp ecx, 32
  jb @ShiftLessThan32Bits
  sub ecx, 32
  mov eax, edx
  xor edx, edx
  cmp ecx, 31
  ja @AbsoluteValueLessThan1OrOverFlow
@ShiftLessThan32Bits:
  shrd eax, edx, cl
  shr edx, cl
  {Is the number negative?}
  test ebp, ebp
  js @NegativeNumber
  {Sign bit may not be set for positive numbers}
  test edx, edx
  jns @Done
  jmp RaiseInvalidOpException
@NegativeNumber:
  {Negate the result}
  neg eax
  adc edx, 0
  neg edx
  jns RaiseInvalidOpException
  jmp @Done
@AbsoluteValueLessThan1OrOverFlow:
  test ecx, ecx
  js RaiseInvalidOpException
  xor eax, eax
@Done:
end;

//Author:            Dennis Kjaer Christensen
//Date:              28/2 2004
//Optimized for:     Prescott
//Instructionset(s): IA32, SSE3

//EH: This one is the fastest for Single but uses SSE3 register.
//we need a way to override the previous function which works (IA32) in all cases
{function TruncSingle_DKC_SSE3_1(const X : Single) : Int64;
asm
 fld X
 db $DD, $4C, $24, $F8 // fisttp qword ptr [esp-8]
 mov    eax, [esp-8]
 mov    edx, [esp-4]
end;}

{$ENDIF USE_FAST_TRUNC}

function GetOrdinalDigits(const Value: UInt64): Byte;
var I64Rec: Int64Rec absolute Value;
begin
  if I64Rec.Hi = 0 then
    Result := GetOrdinalDigits(i64Rec.Lo) //faster cardinal version
  else if Value >= UInt64(100000000000000) then
    if Value    >= UInt64(10000000000000000) then
      if Value  >= UInt64(1000000000000000000) then
        Result := 19 + Ord(Value  >= UInt64(_10Trillion))
      else Result := 17 + Ord(Value >= UInt64(100000000000000000)) //$016345785D8A0000
    else Result := 15 + Ord(Value >= UInt64(1000000000000000))
  else if Value >= UInt64(1000000000000) then
    Result := 13 + Ord(Value >= UInt64(10000000000000))
  else if Value >= UInt64(10000000000) then
    Result := 11 + Ord(Value >= UInt64(100000000000))
  else
    Result := 10; //it's a nop -> GetOrdinalDigits(i64Rec.Lo)
end;

function GetOrdinalDigits(const Value: Int64): Byte;
begin
  if Value < 0
  then Result := GetOrdinalDigits(UInt64(-Value))
  else Result := GetOrdinalDigits(UInt64(Value))
end;

function GetOrdinalDigits(Value: Cardinal): Byte;
begin
  if Value >= 10000 then
    if Value >= 1000000 then
      if Value >= 100000000
      then Result := 9 + Ord(Value >= 1000000000)
      else Result := 7 + Ord(Value >= 10000000)
    else Result := 5 + Ord(Value >= 100000)
  else if Value >= 100 then
    Result := 3 + Ord(Value >= 1000)
  else
    Result := 1 + Ord(Value >= 10);
end;

function GetOrdinalDigits(Value: Integer): Byte;
begin
  if Value < 0
  then Result := GetOrdinalDigits(Cardinal(-Value))
  else Result := GetOrdinalDigits(Cardinal(Value));
end;

function GetOrdinalDigits(Value: Word): Byte;
begin
  if Value >= 10000 then
    Result := 5
  else if Value >= 100 then
    Result := 3 + Ord(Value >= 1000)
  else
    Result := 1 + Ord(Value >= 10);
end;

function GetOrdinalDigits(Value: SmallInt): Byte;
begin
  if Value < 0
  then Result := GetOrdinalDigits(Word(-Value))
  else Result := GetOrdinalDigits(Word(Value));
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  {$IFDEF OverFlowCheckEnabled} {$Q+} {$ENDIF}
end;

function GetOrdinalDigits(Value: Byte): Byte;
begin
  if Value >= 100
  then Result := 3
  else Result := 1 + Ord(Value >= 10);
end;

function GetOrdinalDigits(Value: ShortInt): Byte;
begin
  if Value < 0
  then Result := GetOrdinalDigits(Byte(-Value))
  else Result := GetOrdinalDigits(Byte(Value));
end;

{$IFDEF USE_FAST_STRLEN}
function StrLen_JOH_SSE2_2_a(const Str: PAnsiChar): Cardinal; //Unreleased
asm
  lea      ecx, [eax+16]
  test     ecx, $ff0
  pxor     xmm0, xmm0
  jz       @@NearPageEnd     {Within 16 Bytes of Page End}
@@WithinPage:
  movdqu   xmm1, [eax]       {Check First 16 Bytes for #0}
  add      eax, 16
  pcmpeqb  xmm1, xmm0
  pmovmskb edx, xmm1
  test     edx, edx
  jnz      @@SetResult
  and      eax, -16          {Align Memory Reads}
@@AlignedLoop:
  movdqa   xmm1, [eax]       {Check Next 16 Bytes for #0}
  add      eax, 16
  pcmpeqb  xmm1, xmm0
  pmovmskb edx, xmm1
  test     edx, edx
  jz       @@AlignedLoop
@@SetResult:
  bsf      edx, edx          {#0 Found - Set Result}
  add      eax, edx
  sub      eax, ecx
  ret
@@NearPageEnd:
  mov      edx, eax
@@Loop:
  cmp      byte ptr [eax], 0 {Loop until #0 Found or 16-Byte Aligned}
  je       @@SetResult2
  add      eax, 1
  test     eax, 15
  jnz      @@Loop
  jmp      @@AlignedLoop
@@SetResult2:
  sub      eax, edx
end;

function StrLen_JOH_IA32_7_d(const Str: PAnsiChar): Cardinal;
asm
  cmp   byte ptr [eax], 0
  je    @@0
  cmp   byte ptr [eax+1], 0
  je    @@1
  cmp   byte ptr [eax+2], 0
  je    @@2
  cmp   byte ptr [eax+3], 0
  je    @@3
  push  eax
  and   eax, -4              {DWORD Align Reads}
@@Loop:
  add   eax, 4
  mov   edx, [eax]           {4 Chars per Loop}
  lea   ecx, [edx-$01010101]
  not   edx
  and   edx, ecx
  and   edx, $80808080       {Set Byte to $80 at each #0 Position}
  jz    @@Loop               {Loop until any #0 Found}
@@SetResult:
  pop   ecx
  bsf   edx, edx             {Find First #0 Position}
  shr   edx, 3               {Byte Offset of First #0}
  add   eax, edx             {Address of First #0}
  sub   eax, ecx
  ret
@@0:
  xor   eax, eax
  ret
@@1:
  mov   eax, 1
  ret
@@2:
  mov   eax, 2
  ret
@@3:
  mov   eax, 3
end;
{$ELSE}
{$IFDEF PUREPASCAL}
//Author:            John O'Harrow
//Optimized for:     Pure-Pascal

//changed to PByte support:
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function StrLen_JOH_PAS_3_a(const Str: PAnsiChar): Cardinal;
var
  P, PStr: PAnsiChar;
  I, J: Integer;
begin
  if (Str = nil) or (Ord(Str^) = Ord(#0)) then
    begin
      Result := 0; Exit;
    end;
  if Ord((Str+1)^) = Ord(#0) then
    begin
      Result := 1; Exit;
    end;
  if Ord((Str+2)^) = Ord(#0) then
    begin
      Result := 2; Exit;
    end;
  if Ord((Str+3)^) = Ord(#0) then
    begin
      Result := 3; Exit;
    end;
 P := Pointer(Str);
 PStr := P;
 P := Pointer(NativeInt(P) and -4);
 repeat
   Inc(P, 4);
   I := PInteger(P)^;
   J := I - $01010101;
   I := not(I);
   I := I and J;
 until (I and $80808080) <> 0;
 Result := P - PStr;
 if I and $80 = 0 then
   if I and $8000 <> 0 then
     Inc(Result)
   else
     if I and $800000 <> 0 then
       Inc(Result, 2)
     else
       Inc(Result, 3)
end;
{$IFDEF FPC} {$POP} {$ENDIF}
  {$ENDIF PUREPASCAL}
{$ENDIF USE_FAST_STRLEN}

{$IFDEF USE_FAST_CHARPOS}
function CharPos_JOH_SSE2_1_c(Ch : Char; const Str : AnsiString) : Integer;
asm
  test      edx, edx
  jz        @@NullString
  mov       ecx, [edx-4]
  push      ebx
  mov       ebx, eax
  cmp       ecx, 16
  jl        @@Small
@@NotSmall:
  mov       ah, al           {Fill each Byte of XMM1 with AL}
  movd      xmm1, eax
  pshuflw   xmm1, xmm1, 0
  pshufd    xmm1, xmm1, 0
@@First16:
  movups    xmm0, [edx]      {Unaligned}
  pcmpeqb   xmm0, xmm1       {Compare First 16 Characters}
  pmovmskb  eax, xmm0
  test      eax, eax
  jnz       @@FoundStart     {Exit on any Match}
  cmp       ecx, 32
  jl        @@Medium         {If Length(Str) < 32, Check Remainder}
@@Align:
  sub       ecx, 16          {Align Block Reads}
  push      ecx
  mov       eax, edx
  neg       eax
  and       eax, 15
  add       edx, ecx
  neg       ecx
  add       ecx, eax
@@Loop:
  movaps    xmm0, [edx+ecx]  {Aligned}
  pcmpeqb   xmm0, xmm1       {Compare Next 16 Characters}
  pmovmskb  eax, xmm0
  test      eax, eax
  jnz       @@Found          {Exit on any Match}
  add       ecx, 16
  jle       @@Loop
  pop       eax              {Check Remaining Characters}
  add       edx, 16
  add       eax, ecx         {Count from Last Loop End Position}
  jmp       dword ptr [@@JumpTable2-ecx*4]
  nop
  nop
@@NullString:
  xor       eax, eax         {Result = 0}
  ret
  nop
@@FoundStart:
  bsf       eax, eax         {Get Set Bit}
  pop       ebx
  inc       eax              {Set Result}
  ret
  nop
  nop
@@Found:
  pop       edx
  bsf       eax, eax         {Get Set Bit}
  add       edx, ecx
  pop       ebx
  lea       eax, [eax+edx+1] {Set Result}
  ret
@@Medium:
  add       edx, ecx         {End of String}
  mov       eax, 16          {Count from 16}
  jmp       dword ptr [@@JumpTable1-64-ecx*4]
  nop
  nop
@@Small:
  add       edx, ecx         {End of String}
  xor       eax, eax         {Count from 0}
  jmp       dword ptr [@@JumpTable1-ecx*4]
  nop
@@JumpTable1:
  dd        @@NotFound, @@01, @@02, @@03, @@04, @@05, @@06, @@07
  dd        @@08, @@09, @@10, @@11, @@12, @@13, @@14, @@15, @@16
@@JumpTable2:
  dd        @@16, @@15, @@14, @@13, @@12, @@11, @@10, @@09, @@08
  dd        @@07, @@06, @@05, @@04, @@03, @@02, @@01, @@NotFound
@@16:
  add       eax, 1
  cmp       bl, [edx-16]
  je        @@Done
@@15:
  add       eax, 1
  cmp       bl, [edx-15]
  je        @@Done
@@14:
  add       eax, 1
  cmp       bl, [edx-14]
  je        @@Done
@@13:
  add       eax, 1
  cmp       bl, [edx-13]
  je        @@Done
@@12:
  add       eax, 1
  cmp       bl, [edx-12]
  je        @@Done
@@11:
  add       eax, 1
  cmp       bl, [edx-11]
  je        @@Done
@@10:
  add       eax, 1
  cmp       bl, [edx-10]
  je        @@Done
@@09:
  add       eax, 1
  cmp       bl, [edx-9]
  je        @@Done
@@08:
  add       eax, 1
  cmp       bl, [edx-8]
  je        @@Done
@@07:
  add       eax, 1
  cmp       bl, [edx-7]
  je        @@Done
@@06:
  add       eax, 1
  cmp       bl, [edx-6]
  je        @@Done
@@05:
  add       eax, 1
  cmp       bl, [edx-5]
  je        @@Done
@@04:
  add       eax, 1
  cmp       bl, [edx-4]
  je        @@Done
@@03:
  add       eax, 1
  cmp       bl, [edx-3]
  je        @@Done
@@02:
  add       eax, 1
  cmp       bl, [edx-2]
  je        @@Done
@@01:
  add       eax, 1
  cmp       bl, [edx-1]
  je        @@Done
@@NotFound:
  xor       eax, eax
  pop       ebx
  ret
@@Done:
  pop       ebx
end;

function CharPos_Sha_Pas_2_b(ch: char; const s: AnsiString): integer;
const
  cMinusOnes = -$01010101;
  cSignums   =  $80808080;
var
  Ndx, Len, c, d, Mask, Sign, Save, SaveEnd: integer;
label
  Small, Middle, Large,
  Found0, Found1, Found2, Found3, NotFound,
  Matched, MatchedPlus1, MatchedMinus1, NotMatched,
  Return;
begin
  c:=integer(@pchar(integer(s))[-4]);
  if c=-4 then goto NotFound;
  Len:=pinteger(c)^;
  if Len>24 then goto Large;
  Ndx:=4;
  if Ndx>Len then goto Small;

Middle:
  if pchar(c)[Ndx+0]=ch then goto Found0;
  if pchar(c)[Ndx+1]=ch then goto Found1;
  if pchar(c)[Ndx+2]=ch then goto Found2;
  if pchar(c)[Ndx+3]=ch then goto Found3;
  inc(Ndx,4);
  if Ndx<=Len then goto Middle;

  Ndx:=Len+1;
  if pchar(c)[Len+1]=ch then goto Found0;
  if pchar(c)[Len+2]=ch then goto Found1;
  if pchar(c)[Len+3]<>ch then goto NotFound;
  Result:=integer(@pchar(Ndx)[-1]); exit;
  goto Return; //drop Ndx

Small:
  if Len=0 then goto NotFound; if pchar(c)[Ndx+0]=ch then goto Found0;
  if Len=1 then goto NotFound; if pchar(c)[Ndx+1]=ch then goto Found1;
  if Len=2 then goto NotFound; if pchar(c)[Ndx+2]<>ch then goto NotFound;

Found2: Result:=integer(@pchar(Ndx)[-1]); exit;
Found1: Result:=integer(@pchar(Ndx)[-2]); exit;
Found0: Result:=integer(@pchar(Ndx)[-3]); exit;
NotFound: Result:=0; exit;
  goto NotFound; //kill warning 'Ndx might not have been initialized'
Found3: Result:=integer(@pchar(Ndx)[0]); exit;
  goto Return; //drop Ndx

Large:
  Save:=c;
    Mask:=ord(ch);
  Ndx:=integer(@pchar(c)[+4]);

    d:=Mask;
  inc(Len,c);
  SaveEnd:=Len;
    Mask:=(Mask shl 8);
  inc(Len,+4-16+3);

    Mask:=Mask or d;
  Len:=Len and (-4);
    d:=Mask;
  cardinal(Sign):=cSignums;

    Mask:=Mask shl 16;
  c:=pintegerArray(Ndx)[0];
    Mask:=Mask or d;
  inc(Ndx,4);

    c:=c xor Mask;
    d:=integer(@pchar(c)[cMinusOnes]);
    c:=c xor (-1);
    c:=c and d;
    d:=Mask;

    if c and Sign<>0 then goto MatchedMinus1;
    Ndx:=Ndx and (-4);
    d:=d xor pintegerArray(Ndx)[0];

    if cardinal(Ndx)<cardinal(Len) then repeat;
      c:=integer(@pchar(d)[cMinusOnes]);
      d:=d xor (-1);
      c:=c and d;
      d:=Mask;

      d:=d xor pintegerArray(Ndx)[1];
      if c and Sign<>0 then goto Matched;
      c:=integer(@pchar(d)[cMinusOnes]);
      d:=d xor (-1);
      c:=c and d;
      d:=pintegerArray(Ndx)[2];
      if c and Sign<>0 then goto MatchedPlus1;
      d:=d xor Mask;

      c:=integer(@pchar(d)[cMinusOnes]);
      d:=d xor (-1);
      inc(Ndx,12);
      c:=c and d;

      //if c and Sign<>0 then goto MatchedMinus1;
      d:=Mask;
      if c and Sign<>0 then goto MatchedMinus1;
      d:=d xor pintegerArray(Ndx)[0];
      until cardinal(Ndx)>=cardinal(Len);

    Len:=SaveEnd;
    while true do begin;
      c:=integer(@pchar(d)[cMinusOnes]);
      d:=d xor (-1);
      c:=c and d;
      inc(Ndx,4);
      if c and Sign<>0 then goto MatchedMinus1;
      d:=Mask;
      if cardinal(Ndx)<=cardinal(Len)
      then d:=d xor pintegerArray(Ndx)[0]
      else begin;
        if Len=0 then goto NotMatched;
        d:=d xor pintegerArray(Len)[0];
        Ndx:=Len;
        Len:=0;
        end
      end;

NotMatched:
  Result:=0; exit;

MatchedPlus1:   inc(Ndx,8);
MatchedMinus1:  dec(Ndx,4);
Matched:
    c:=c and Sign;
    dec(Ndx,integer(Save)+2);
    if word(c)=0 then begin;
      c:=c shr 16; inc(Ndx,2);
      end;
    if byte(c)<>0 then dec(Ndx);
    Result:=Ndx;
Return:
  end;
{$ENDIF USE_FAST_CHARPOS}

{$IF defined(Delphi) and defined(WIN32)} //had no success with FPC here
//Author:            John O'Harrow
//Date:              20/10 2010
//Instructionset(s): IA32
//  function Pos_JOH_IA32_6(const SubStr : AnsiString; const Str : AnsiString) : Integer;
function Pos(const SubStr : RawByteString; const Str : RawByteString) : Integer; overload;
asm {Slightly Cut-Down version of PosEx_JOH_6}
  push    ebx
  cmp     eax, 1
  sbb     ebx, ebx         {-1 if SubStr = '' else 0}
  sub     edx, 1           {-1 if S = ''}
  sbb     ebx, 0           {Negative if S = '' or SubStr = '' else 0}
  jl      @@InvalidInput
  push    edi
  push    esi
  push    ebp
  push    edx
  mov     edi, [eax-4]     {Length(SubStr)}
  mov     esi, [edx-3]     {Length(S)}
  cmp     edi, esi
  jg      @@NotFound       {Offset to High for a Match}
  test    edi, edi
  jz      @@NotFound       {Length(SubStr = 0)}
  lea     ebp, [eax+edi]   {Last Character Position in SubStr + 1}
  add     esi, edx         {Last Character Position in S}
  movzx   eax, [ebp-1]     {Last Character of SubStr}  //FPC don't compile here!!!
  add     edx, edi         {Search Start Position in S for Last Character}
  mov     ah, al
  neg     edi              {-Length(SubStr)}
  mov     ecx, eax
  shl     eax, 16
  or      ecx, eax         {All 4 Bytes = Last Character of SubStr}
@@MainLoop:
  add     edx, 4
  cmp     edx, esi
  ja      @@Remainder      {1 to 4 Positions Remaining}
  mov     eax, [edx-4]     {Check Next 4 Bytes of S}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@MainLoop       {Loop Until any Match on Last Character Found}
  bsf     eax, eax         {Find First Match Bit}
  shr     eax, 3           {Byte Offset of First Match (0..3)}
  lea     edx, [eax+edx-3] {Address of First Match on Last Character + 1}
@@Compare:
  cmp     edi, -4
  jle     @@Large          {Lenght(SubStr) >= 4}
  cmp     edi, -1
  je      @@SetResult      {Exit with Match if Lenght(SubStr) = 1}
  movzx   eax, word ptr [ebp+edi] {Last Char Matches - Compare First 2 Chars}
  cmp     ax, [edx+edi]
  jne     @@MainLoop       {No Match on First 2 Characters}
@@SetResult:               {Full Match}
  lea     eax, [edx+edi]   {Calculate and Return Result}
  pop     edx
  pop     ebp
  pop     esi
  pop     edi
  pop     ebx
  sub     eax, edx         {Subtract Start Position}
  ret
@@NotFound:
  pop     edx              {Dump Start Position}
  pop     ebp
  pop     esi
  pop     edi
@@InvalidInput:
  pop     ebx
  xor     eax, eax         {No Match Found - Return 0}
  ret
@@Remainder:               {Check Last 1 to 4 Characters}
  mov     eax, [esi-3]     {Last 4 Characters of S - May include Length Bytes}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@NotFound       {No Match Possible}
  lea     eax, [edx-4]     {Check Valid Match Positions}
  cmp     cl, [eax]
  lea     edx, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+2]
  cmp     cl, [eax+1]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+3]
  cmp     cl, [eax+2]
  je      @@Compare
  cmp     edx, esi
  ja      @@NotFound
  lea     edx, [eax+4]
  jmp     @@Compare
@@Large:
  mov     eax, [ebp-4]     {Compare Last 4 Characters of S and SubStr}
  cmp     eax, [edx-4]
  jne     @@MainLoop       {No Match on Last 4 Characters}
  mov     ebx, edi
@@CompareLoop:             {Compare Remaining Characters}
  add     ebx, 4           {Compare 4 Characters per Loop}
  jge     @@SetResult      {All Characters Matched}
  mov     eax, [ebp+ebx-4]
  cmp     eax, [edx+ebx-4]
  je      @@CompareLoop    {Match on Next 4 Characters}
  jmp     @@MainLoop       {No Match}
end;
//Author:            Dennis Kjaer Christensen
//Date:              20/10 2010
//Instructionset(s): IA32

//SubStr ptr - eax
//Str ptr - edx
//SubStrLen - ecx
//StrLen - [esp]
//Temp - ebx
//I - edi
//J - esi
//Index - [esp+$04]
//Result - [esp+$08]
//OuterLoopEnd - [esp+$14]
//Match - [esp+$10]

//function PosUnicode32_DKC_IA32_3_a(const SubStr, Str: UnicodeString): Integer; overload;
//changes by EgonHugeist: ZWideString might be a WideString(D7-D007, old FPC)
//where length address returns size in bytes -> shrink to codepoints
function Pos(const SubStr, Str: ZWideString): Integer; overload;
asm
 push  ebx
 push  esi
 push  edi
 add   esp,-$18
 //Result := 0;
 xor   ebx,ebx
 mov   [esp+$08],ebx //Save Result=0
 //StrLen := Length(Str);
 mov   ebx,edx       //StrLen=0 if Str=nil
 mov   [esp],ebx     //Save StrLen
 test  edx,edx       //Str <> nil
 jz    @L1
 mov   ebx,[edx-4]   //StrLen in ebx
 {$IFNDEF PWIDECHAR_IS_PUNICODECHAR} //widestrlen is Length in bytes !!
 shr   ebx, 1 //shrink to Length in codepoints
 {$ENDIF}
 mov   [esp],ebx     //Save StrLen
@L1 :
 //SubStrLen := Length(SubStr);
 mov   ecx,eax       //SubStrLen=0 if SubStr=nil
 test  eax,eax       //SubStr <> nil
 jz    @L2
 mov   ecx,[eax-4]   //SubStrLen in ecx
 {$IFNDEF PWIDECHAR_IS_PUNICODECHAR} //widestrlen is Length in bytes !!
 shr   ecx, 1 //shrink to Length in codepoints
 {$ENDIF}
@L2 :
 //if (SubStrLen <> 0) then
 test  ecx,ecx
 jz    @Done
 //if (StrLen >= SubStrLen) then
 mov   ebx,[esp]
 cmp   ebx,ecx
 jl    @Done
 //for I := 1 to StrLen-SubStrLen+1 do
 mov   ebx,[esp]
 sub   ebx,ecx
 inc   ebx
 mov   [esp+$0c],ebx
 test  ebx,ebx
 jle   @Done
 mov   [esp+$14],ebx
 mov   edi,1         //I=1
 //if Str[I] = SubStr[1] then
@OuterLoop:
 movzx ebx,word ptr [edx+edi*2-$02]
 cmp   bx,[eax]
 jnz   @L5
 //Match := True;
 mov   byte ptr [esp+$10],1
 //for J := 1 to SubStrLen do
 test  ecx,ecx
 jle   @L6
 mov   esi,1
 //Index := I+J-1;
@L9 :
 lea   ebx,[edi+esi]
 dec   ebx
 mov   [esp+$04],ebx
 //if StrLen > Index then
 cmp   ebx,[esp]
 ja   @L7
 //if Str[Index] <> SubStr[J] then
 mov   ebx,[esp+$04]
 movzx ebx,word ptr [edx+ebx*2-$02]
 cmp   bx,[eax+esi*2-$02]
 jz    @L8
 //Match := False;
 mov   byte ptr [esp+$10],0
 //Break;
 jmp   @L6
 //Match := False;
@L7 :
 mov   byte ptr [esp+$10],0
@L8 :
 inc   esi
 //for J := 1 to SubStrLen do
 cmp   esi,ecx
 jle   @L9
@L6 :
 //if Match then
 cmp   byte ptr [esp+$10],$00
 jz    @L5
 //Result := I;
 mov   [esp+$08],edi //Save Result
 //Break;
 jmp   @Done
@L5 :
 inc   edi
 //for I := 1 to StrLen-SubStrLen+1 do
 mov   ebx,[esp+$0c]
 cmp   edi,ebx
 jle   @OuterLoop
@Done:
 mov   eax,[esp+$08] //Load Result
 add   esp,$18
 pop   edi
 pop   esi
 pop   ebx
end;

function PosEx(const SubStr, S: RawByteString; Offset: Integer = 1): Integer;
//from John O'Harrow PosEx_JOH_IA32_7_c(const SubStr, S: string; Offset: Integer = 1): Integer;
asm {180 Bytes}
  push    ebx
  cmp     eax, 1
  sbb     ebx, ebx         {-1 if SubStr = '' else 0}
  sub     edx, 1           {-1 if S = ''}
  sbb     ebx, 0           {Negative if S = '' or SubStr = '' else 0}
  dec     ecx              {Offset - 1}
  or      ebx, ecx         {Negative if S = '' or SubStr = '' or Offset < 1}
  jl      @@InvalidInput
  push    edi
  push    esi
  push    ebp
  push    edx
  mov     edi, [eax-4]     {Length(SubStr)}
  mov     esi, [edx-3]     {Length(S)}
  add     ecx, edi
  cmp     ecx, esi
  jg      @@NotFound       {Offset to High for a Match}
  test    edi, edi
  jz      @@NotFound       {Length(SubStr = 0)}
  lea     ebp, [eax+edi]   {Last Character Position in SubStr + 1}
  add     esi, edx         {Last Character Position in S}
  movzx   eax, [ebp-1]     {Last Character of SubStr}
  add     edx, ecx         {Search Start Position in S for Last Character}
  mov     ah, al
  neg     edi              {-Length(SubStr)}
  mov     ecx, eax
  shl     eax, 16
  or      ecx, eax         {All 4 Bytes = Last Character of SubStr}
@@MainLoop:
  add     edx, 4
  cmp     edx, esi
  ja      @@Remainder      {1 to 4 Positions Remaining}
  mov     eax, [edx-4]     {Check Next 4 Bytes of S}
  xor     eax, ecx         {Zero Byte at each Matching Position}
  lea     ebx, [eax-$01010101]
  not     eax
  and     eax, ebx
  and     eax, $80808080   {Set Byte to $80 at each Match Position else $00}
  jz      @@MainLoop       {Loop Until any Match on Last Character Found}
  bsf     eax, eax         {Find First Match Bit}
  shr     eax, 3           {Byte Offset of First Match (0..3)}
  lea     edx, [eax+edx-4] {Address of First Match on Last Character}
@@Compare:
  inc     edx
  cmp     edi, -4
  jle     @@Large          {Lenght(SubStr) >= 4}
  cmp     edi, -1
  je      @@SetResult      {Exit with Match if Lenght(SubStr) = 1}
  mov     ax, [ebp+edi]    {Last Char Matches - Compare First 2 Chars}
  cmp     ax, [edx+edi]
  jne     @@MainLoop       {No Match on First 2 Characters}
@@SetResult:               {Full Match}
  lea     eax, [edx+edi]   {Calculate and Return Result}
  pop     edx
  pop     ebp
  pop     esi
  pop     edi
  pop     ebx
  sub     eax, edx         {Subtract Start Position}
  ret
@@NotFound:
  pop     edx              {Dump Start Position}
  pop     ebp
  pop     esi
  pop     edi
@@InvalidInput:
  pop     ebx
  xor     eax, eax         {No Match Found - Return 0}
  ret
@@Remainder:               {Check Last 1 to 4 Characters}
  sub     edx, 4
@@RemainderLoop:
  cmp     cl, [edx]
  je      @@Compare
  cmp     edx, esi
  jae     @@NotFound
  inc     edx
  jmp     @@RemainderLoop
@@Large:
  mov     eax, [ebp-4]     {Compare Last 4 Characters}
  cmp     eax, [edx-4]
  jne     @@MainLoop       {No Match on Last 4 Characters}
  mov     ebx, edi
@@CompareLoop:             {Compare Remaining Characters}
  add     ebx, 4           {Compare 4 Characters per Loop}
  jge     @@SetResult      {All Characters Matched}
  mov     eax, [ebp+ebx-4]
  cmp     eax, [edx+ebx-4]
  je      @@CompareLoop    {Match on Next 4 Characters}
  jmp     @@MainLoop       {No Match}
end; {PosEx}
{$ELSE}
//Author:            Aleksandr Sharahov
//function Pos_Sha_Pas_3(const SubStr: AnsiString; const Str: AnsiString): Integer;
//faster than Delphi and FPC RTL
function Pos(const SubStr: RawByteString; const Str: RawByteString): Integer; overload;
var
  len, lenSub: LengthInt;
  ch: AnsiChar;
  p, pSub, pStart, pStop: PAnsiChar;
label
  Ret, Ret0, Ret1, Next0, Next1;
begin;
  p:=pointer(Str);
  pSub:=pointer(SubStr);

  if (p=nil) or (pSub=nil) then begin;
    Result:=0;
    exit;
    end;
  {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
  len := Length(Str)-1;
  lenSub := Length(SubStr)-1;
  {$else}
  len:=PLengthInt(p-StringLenOffSet)^;
  lenSub:=PLengthInt(pSub-StringLenOffSet)^;
  {$ENDIF}
  if (len<lenSub) or (lenSub<=0) then begin;
    Result:=0;
    exit;
    end;

  lenSub:=lenSub-1;
  pStop:=p+len;
  p:=p+lenSub;
  pSub:=pSub+lenSub;
  pStart:=p;

  ch:=AnsiChar(pSub[0]);

  if lenSub=0 then begin;
    repeat;
      if ch=AnsiChar(p[0]) then goto Ret0;
      if ch=AnsiChar(p[1]) then goto Ret1;
      p:=p+2;
      until p>=pStop;
    Result:=0;
    exit;
    end;

  lenSub:=-lenSub;
  repeat;
    if ch=AnsiChar(p[0]) then begin;
      len:=lenSub;
      repeat;
        if pword(psub+len)^<>pword(p+len)^ then goto Next0;
        len:=len+2;
        until len>=0;
      goto Ret0;
Next0:end;

    if ch=AnsiChar(p[1]) then begin;
      len:=lenSub;
      repeat;
        if pword(@psub[len])^<>pword(@p[len+1])^ then goto Next1;
        len:=len+2;
        until len>=0;
      goto Ret1;
Next1:end;

    p:=p+2;
    until p>=pStop;
  Result:=0;
  exit;

Ret1:
  p:=p+2;
  if p<=pStop then goto Ret;
  Result:=0;
  exit;
Ret0:
  inc(p);
Ret:
  Result:=p-pStart;
  end;
//Author:            Dennis Kjaer Christensen
//Date:              18/07 2012
//faster than Delphi and FPC RTL
function Pos(const SubStr, Str: ZWideString): Integer;
{$IFDEF ZERO_BASED_STRINGS}
begin
  Result := PosEx(SubStr, Pointer(Str), Length(Str));
{$ELSE ZERO_BASED_STRINGS}
var
 I, J, StrLen, SubStrLen, Index : Integer;
 Match: Boolean;

begin
  Result := 0;
  StrLen := Length(Str);  //Length returns 0 if Str=nil
  SubStrLen := Length(SubStr);
  if (SubStrLen <> 0) and (StrLen >= SubStrLen) then
  begin
    for I := 1 to StrLen-SubStrLen+1 do
    begin
      if Str[I] = SubStr[1] then
      begin
        Match := True;
        for J := 1 to SubStrLen do
        begin
          Index := I+J-1;
          if Index <= StrLen then
          begin
            if Str[Index] <> SubStr[J] then
            begin
             // No Match
             Match := False;
             Break;
            end;
          end
          else
            Match := False;
        end;
        if Match then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  end;
{$ENDIF ZERO_BASED_STRINGS}
end;

// from Aleksandr Sharahov's PosEx_Sha_Pas_2()
function PosEx(const SubStr, S: RawByteString; Offset: Integer = 1): Integer;
begin
  Result := PosEx(SubStr, Pointer(S), Length(S){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, Offset);
end;
{$IFEND}

// from Aleksandr Sharahov's PosEx_Sha_Pas_2()
// changed to a unicode pointer version
function PosEx(SubStr, Str: PWideChar; SubStrLen, Strlen: LengthInt; Offset: Integer = 1): Integer;
var ch: WideChar;
    pStart, pStop: PWideChar;
label Loop0, Loop4, TestT, Test0, Test1, Test2, Test3, Test4,
      AfterTestT, AfterTest0, Ret, Exit;
begin;
  if (Str=nil) or (SubStr=nil) or (Offset<FirstStringIndex) then begin
    Result := 0;
    goto Exit;
  end;
  SubStrLen := SubStrLen-1;
  if (Strlen<SubStrLen+LengthInt(Offset)) or (SubStrLen<0) then begin
    Result := 0;
    goto Exit;
  end;
  pStop := Str+Strlen;
  Str := Str+SubStrLen;
  SubStr := SubStr+SubStrLen;
  pStart := Str;
  Str := Str+Offset+3;
  ch := SubStr^;
  SubStrLen := -SubStrLen;
  if Str<pStop then goto Loop4;
  Str := Str-4;
  goto Loop0;
Loop4:
  if ch=(Str-4)^ then goto Test4;
  if ch=(Str-3)^ then goto Test3;
  if ch=(Str-2)^ then goto Test2;
  if ch=(Str-1)^ then goto Test1;
Loop0:
  if ch=Str^ then goto Test0;
AfterTest0:
  if ch=(Str+1)^ then goto TestT;
AfterTestT:
  Str := Str+6;
  if Str<pStop then goto Loop4;
  Str := Str-4;
  if Str<pStop then goto Loop0;
  Result := 0;
  goto Exit;
Test3: Str := Str-2;
Test1: Str := Str-2;
TestT: Strlen := SubStrLen;
  if SubStrLen<>0 then
  repeat
    if (SubStr[Strlen]<>Str[Strlen+1]) or (SubStr[Strlen+1]<>Str[Strlen+2]) then
      goto AfterTestT;
    Strlen := Strlen+2;
  until Strlen>=0;
  Str := Str+2;
  if Str<=pStop then goto Ret;
  Result := 0;
  goto Exit;
Test4: Str := Str-2;
Test2: Str := Str-2;
Test0: Strlen := SubStrLen;
  if SubStrLen<>0 then
  repeat
    if (SubStr[Strlen]<>Str[Strlen]) or (SubStr[Strlen+1]<>Str[Strlen+1]) then
      goto AfterTest0;
    Strlen := Strlen+2;
  until Strlen>=0;
  inc(Str);
Ret:
  Result := Str-pStart;
Exit:
end;

// from Aleksandr Sharahov's PosEx_Sha_Pas_2()
// changed to a unicode pointer version
function PosEx(const SubStr: ZWideString; Str: PWideChar; len: LengthInt; Offset: Integer = 1): Integer;
begin;
  Result := PosEx(Pointer(SubStr), Str, Length(SubStr), len, OffSet);
end;

// from Aleksandr Sharahov's PosEx_Sha_Pas_2()
// changed to a unicode version
function PosEx(const SubStr, S: ZWideString; Offset: Integer = 1): Integer;
begin;
  Result := PosEx(SubStr, Pointer(S), Length(S), Offset);
end;

// from Aleksandr Sharahov's PosEx_Sha_Pas_2()
// changed to a raw pointer version
function PosEx(SubStr, Str: PAnsiChar; SubStrLen, Strlen: LengthInt; Offset: Integer = 1): Integer;
var ch: AnsiChar;
    pStart, pStop: PAnsiChar;
label Loop0, Loop4, TestT, Test0, Test1, Test2, Test3, Test4,
      AfterTestT, AfterTest0, Ret, Exit;
begin;
  if (Str=nil) or (SubStr=nil) or (Offset<FirstStringIndex) then begin
    Result := 0;
    goto Exit;
  end;
  SubStrLen := SubStrLen-1;
  if (Strlen<SubStrLen+LengthInt(Offset)) or (SubStrLen<0) then begin
    Result := 0;
    goto Exit;
  end;
  pStop := Str+Strlen;
  Str := Str+SubStrLen;
  SubStr := SubStr+SubStrLen;
  pStart := Str;
  Str := Str+Offset+3;
  ch := {$IFDEF NO_ANSICHAR}PByte{$ENDIF}(SubStr)^;
  SubStrLen := -SubStrLen;
  if Str<pStop then goto Loop4;
  Str := Str-4;
  goto Loop0;
Loop4:
  if ch={$IFDEF NO_ANSICHAR}PByte{$ENDIF}(Str-4)^ then goto Test4;
  if ch={$IFDEF NO_ANSICHAR}PByte{$ENDIF}(Str-3)^ then goto Test3;
  if ch={$IFDEF NO_ANSICHAR}PByte{$ENDIF}(Str-2)^ then goto Test2;
  if ch={$IFDEF NO_ANSICHAR}PByte{$ENDIF}(Str-1)^ then goto Test1;
Loop0:
  if ch={$IFDEF NO_ANSICHAR}PByte{$ENDIF}(Str)^ then goto Test0;
AfterTest0:
  if ch={$IFDEF NO_ANSICHAR}PByte{$ENDIF}(Str+1)^ then goto TestT;
AfterTestT:
  Str := Str+6;
  if Str<pStop then goto Loop4;
  Str := Str-4;
  if Str<pStop then goto Loop0;
  Result := 0;
  goto Exit;
Test3: Str := Str-2;
Test1: Str := Str-2;
TestT: Strlen := SubStrLen;
  if SubStrLen<>0 then
  repeat
    if (SubStr[Strlen]<>Str[Strlen+1]) or (SubStr[Strlen+1]<>Str[Strlen+2]) then
      goto AfterTestT;
    Strlen := Strlen+2;
  until Strlen>=0;
  Str := Str+2;
  if Str<=pStop then goto Ret;
  Result := 0;
  goto Exit;
Test4: Str := Str-2;
Test2: Str := Str-2;
Test0: Strlen := SubStrLen;
  if SubStrLen<>0 then
  repeat
    if (SubStr[Strlen]<>Str[Strlen]) or (SubStr[Strlen+1]<>Str[Strlen+1]) then
      goto AfterTest0;
    Strlen := Strlen+2;
  until Strlen>=0;
  inc(Str);
Ret:
  Result := Str-pStart;
Exit:
end;

// from Aleksandr Sharahov's PosEx_Sha_Pas_2()
// changed to a unicode pointer version
function PosEx(const SubStr: RawByteString; Str: PAnsiChar; len: LengthInt; Offset: Integer = 1): Integer;
begin;
  Result := PosEx(Pointer(SubStr), Str, Length(SubStr){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF}, len, OffSet);
end;

{$If defined(Use_FastCodeFillChar) or defined(PatchSystemMove) or defined(USE_FAST_STRLEN) or defined(USE_FAST_CHARPOS)}
type
 TRegisters = record
  EAX,
    EBX,
    ECX,
    EDX: Cardinal;
 end;

 TVendorStr = string[12];

 TCpuFeatures =
   ( { in EDX }
   cfFPU, cfVME, cfDE, cfPSE, cfTSC, cfMSR, cfPAE, cfMCE,
   cfCX8, cfAPIC, cf_d10, cfSEP, cfMTRR, cfPGE, cfMCA, cfCMOV,
   cfPAT, cfPSE36, cfPSN, cfCLFSH, cf_d20, cfDS, cfACPI, cfMMX,
   cfFXSR, cfSSE, cfSSE2, cfSS, cfHTT, cfTM, cfIA_64, cfPBE,
   { in ECX }
   cfSSE3, cf_c1, cf_c2, cfMON, cfDS_CPL, cf_c5, cf_c6, cfEIST,
   cfTM2, cfSSSE3, cfCID, cfSSE5, cf_c12, cfCX16, cfxTPR, cf_c15,
   cf_c16, cf_c17, cf_c18, cfSSE41, cfSSE42, cf_c21, cf_c22, cf_c23,
   cf_c24, cf_c25, cf_c26, cf_c27, cfAVX, cf_c29, cf_c30, cf_c31);
 TCpuFeatureSet = set of TCpuFeatures;

 TCpuExtendedFeatures =
   ( { in EDX }
   cefLahv, cefCMP, cefSVM, cefEXT, cefALT, cefABM, cefSSE4A, cefMisAlign,
   cef3DPre, cefOSVW, cef_d10, cef_d11, cefSkinit, cefWDT, cef_d14, cef_d15,
   cef_d16, cef_d17, cef_d18, cef_d19, cef_d20, cef_d21, cef_d22, cef_d23,
   cef_d24, cef_d25, cef_d26, cef_d27, cef_d28, cef_d29, cef_d30, cef_d31,
   { in ECX }
   cefFPU, cefVME, cefDE, cefPSE, cefTSC, cefMSR, cefPAE, cefMCE,
   cefCX8, cefAPIC, cef_10, cefSEP, cefMTRR, cefPGE, cefMCA, cefCMOV,
   cefPAT, cefPSE36, cef_18, ceMPC, ceNX, cef_21, cefExMMX, cefMMX,
   cefFXSR, cef_25, cef_26, cef_27, cef_28, cefLM, cefEx3DNow, cef3DNow);
 TCpuExtendedFeatureSet = set of TCpuExtendedFeatures;

const
 VendorIDString: array [Low(TVendor) .. High(TVendor)] of TVendorStr =
   ('',
   'AuthenticAMD', 'CentaurHauls', 'CyrixInstead', 'GenuineIntel',
   'GenuineTMx86', 'NexGenDriven', 'RiseRiseRise', 'UMC UMC UMC ',
   'Geode by NSC', 'SiS SiS SiS');

 { CPU signatures }

 IntelLowestSEPSupportSignature = $633;
 K7DuronA0Signature = $630;
 C3Samuel2EffModel = 7;
 C3EzraEffModel = 8;
 PMBaniasEffModel = 9;
 PMDothanEffModel = $D;
 PMYonahEffModel = $E;
 PMConroeEffModel = $F;
 PMPenrynEffModel = $17; //$1D
 PMNehalemEffModel = $1E; //$1A $2E
 PMWestmereEffModel = $25; //$2C, $2F
 PMSandyBridgeEffModel = $2A; //$2D
 PMIvyBridgeEffModel = $3A;
 P3LowestEffModel = 7;

function IsCPUID_Available: Boolean; register;
{$IFDEF  WIN32}
asm
 PUSHFD                 { save EFLAGS to stack }
 POP     EAX            { store EFLAGS in EAX }
 MOV     EDX, EAX       { save in EDX for later testing }
 XOR     EAX, $200000;  { flip ID bit in EFLAGS }
 PUSH    EAX            { save new EFLAGS value on stack }
 POPFD                  { replace current EFLAGS value }
 PUSHFD                 { get new EFLAGS }
 POP     EAX            { store new EFLAGS in EAX }
 XOR     EAX, EDX       { check if ID bit changed }
 JZ      @exit          { no, CPUID not available }
 MOV     EAX, True      { yes, CPUID is available }
@exit:
end;
// {$ELSEIF  WIN64}
{$ELSE}
begin
 // All 64 bit CPUs support CPUID
 Result := True;
end;
{$ENDIF}


function IsFPU_Available: Boolean;
{$IFDEF  WIN32}
var
 _FCW, _FSW: Word;
 asm
  MOV     EAX, False     { initialize return register }
  MOV     _FSW, $5A5A    { store a non-zero value }
  FNINIT                 { must use non-wait form }
  FNSTSW  _FSW           { store the status }
  CMP     _FSW, 0        { was the correct status read? }
  JNE     @exit          { no, FPU not available }
  FNSTCW  _FCW           { yes, now save control word }
  MOV     DX, _FCW       { get the control word }
  AND     DX, $103F      { mask the proper status bits }
  CMP     DX, $3F        { is a numeric processor installed? }
  JNE     @exit          { no, FPU not installed }
  MOV     EAX, True      { yes, FPU is installed }
 @exit:
end;
// {$ELSEIF  WIN64}
{$ELSE}
begin
 // All 64 bit CPUs have a FPU
 Result := True;
end;
{$ENDIF}


procedure GetCPUID(Param: Cardinal; var Registers: TRegisters);
{$IFDEF  WIN32}
asm
 PUSH    EBX                         { save affected registers }
 PUSH    EDI
 MOV     EDI, Registers
 XOR     EBX, EBX                    { clear EBX register }
 XOR     ECX, ECX                    { clear ECX register }
 XOR     EDX, EDX                    { clear EDX register }
 DB $0F, $A2                         { CPUID opcode }
 MOV     TRegisters(EDI).&EAX, EAX   { save EAX register }
 MOV     TRegisters(EDI).&EBX, EBX   { save EBX register }
 MOV     TRegisters(EDI).&ECX, ECX   { save ECX register }
 MOV     TRegisters(EDI).&EDX, EDX   { save EDX register }
 POP     EDI                         { restore registers }
 POP     EBX
end;
// {$ELSEIF  WIN64}
{$ELSE}
asm
 mov eax, ecx   // eax:= Param, setting upper 32-bit of RAX to 0
 mov r9, rdx    // R9:= @Registers
 mov r10, rbx   // Save rbx
 XOR     EBX, EBX                    { clear RBX register }
 XOR     ECX, ECX                    { clear RCX register }
 XOR     EDX, EDX                    { clear RDX register }
 cpuid
 MOV     TRegisters(R9).&EAX, EAX   { save EAX register }
 MOV     TRegisters(R9).&EBX, EBX   { save EBX register }
 MOV     TRegisters(R9).&ECX, ECX   { save ECX register }
 MOV     TRegisters(R9).&EDX, EDX   { save EDX register }
 mov rbx, r10   // restore rbx
end;
{$ENDIF}


procedure GetCPUVendor;
var
 VendorStr: TVendorStr;
 Registers: TRegisters;
begin
 { call CPUID function 0 }
 GetCPUID(0, Registers);

 { get vendor string }
 SetLength(VendorStr, 12);
  System.Move(Registers.EBX, VendorStr[1], 4); //need system prefix because var Move is NOT assigned here
  System.Move(Registers.EDX, VendorStr[5], 4); //need system prefix because var Move is NOT assigned here
  System.Move(Registers.ECX, VendorStr[9], 4); //need system prefix because var Move is NOT assigned here

 { get CPU vendor from vendor string }
 CPU.Vendor := High(TVendor);
 while (VendorStr <> VendorIDString[CPU.Vendor]) and
   (CPU.Vendor > Low(TVendor)) do
  Dec(CPU.Vendor);
end;

procedure GetCPUFeatures;
{ preconditions: 1. maximum CPUID must be at least $00000001
  2. GetCPUVendor must have been called }
type
 _Int64 = packed record
  Lo: Longword;
  Hi: Longword;
 end;
var
 Registers: TRegisters;
 CpuFeatures: TCpuFeatureSet;
begin
 { call CPUID function $00000001 }
 GetCPUID($00000001, Registers);

 { get CPU signature }
 CPU.Signature := Registers.EAX;

 { extract effective processor family and model }
 CPU.EffFamily := CPU.Signature and $00000F00 shr 8;
 CPU.EffModel := CPU.Signature and $000000F0 shr 4;
 CPU.EffModelBasic := CPU.EffModel;
 CPU.EffFamily := CPU.EffFamily + (CPU.Signature and $0FF00000 shr 20);
 CPU.EffModel := CPU.EffModel + (CPU.Signature and $000F0000 shr 12);

 { get CPU features }
  System.Move(Registers.EDX, _Int64(CpuFeatures).Lo, 4); //need system prefix because var Move is NOT assigned here
  System.Move(Registers.ECX, _Int64(CpuFeatures).Hi, 4); //need system prefix because var Move is NOT assigned here

 { get instruction support }
 if cfFPU in CpuFeatures then
  Include(CPU.InstructionSupport, isFPU);
 if cfTSC in CpuFeatures then
  Include(CPU.InstructionSupport, isTSC);
 if cfCX8 in CpuFeatures then
  Include(CPU.InstructionSupport, isCX8);
 if cfSEP in CpuFeatures then
  begin
   Include(CPU.InstructionSupport, isSEP);
   { for Intel CPUs, qualify the processor family and model to ensure that the
     SYSENTER/SYSEXIT instructions are actually present - see Intel Application
     Note AP-485 }
   if (CPU.Vendor = cvIntel) and
     (CPU.Signature and $0FFF3FFF < IntelLowestSEPSupportSignature) then
    Exclude(CPU.InstructionSupport, isSEP);
  end;
 if cfCMOV in CpuFeatures then
  Include(CPU.InstructionSupport, isCMOV);
 if cfFXSR in CpuFeatures then
  Include(CPU.InstructionSupport, isFXSR);
 if cfMMX in CpuFeatures then
  Include(CPU.InstructionSupport, isMMX);
 if cfSSE in CpuFeatures then
  Include(CPU.InstructionSupport, isSSE);
 if cfSSE2 in CpuFeatures then
  Include(CPU.InstructionSupport, isSSE2);
 if cfSSE3 in CpuFeatures then
  Include(CPU.InstructionSupport, isSSE3);
 if cfSSSE3 in CpuFeatures then
  Include(CPU.InstructionSupport, isSSSE3);
 if cfSSE41 in CpuFeatures then
  Include(CPU.InstructionSupport, isSSE41);
 if cfSSE42 in CpuFeatures then
  Include(CPU.InstructionSupport, isSSE42);
 if cfSSE5 in CpuFeatures then
  Include(CPU.InstructionSupport, isSSE5);
 if cfAVX in CpuFeatures then
  Include(CPU.InstructionSupport, isAVX);
 if (CPU.Vendor = cvIntel) and (cfMON in CpuFeatures) then
  Include(CPU.InstructionSupport, isMONITOR);
 if cfCX16 in CpuFeatures then
  Include(CPU.InstructionSupport, isCX16);
end;

procedure GetCPUExtendedFeatures;
{ preconditions: maximum extended CPUID >= $80000001 }
type
 _Int64 = packed record
  Lo: Longword;
  Hi: Longword;
 end;
var
 Registers: TRegisters;
 CpuExFeatures: TCpuExtendedFeatureSet;
begin
 { call CPUID function $80000001 }
 GetCPUID($80000001, Registers);

 { get CPU extended features }
  System.Move(Registers.ECX, _Int64(CPUExFeatures).Lo, 4);
  System.Move(Registers.EDX, _Int64(CPUExFeatures).Hi, 4);

 { get instruction support }
 if cefLM in CpuExFeatures then
  Include(CPU.InstructionSupport, isX64);
 if cefExMMX in CpuExFeatures then
  Include(CPU.InstructionSupport, isExMMX);
 if cefEx3DNow in CpuExFeatures then
  Include(CPU.InstructionSupport, isEx3DNow);
 if cef3DNow in CpuExFeatures then
  Include(CPU.InstructionSupport, is3DNow);
 if cefSSE4A in CpuExFeatures then
  Include(CPU.InstructionSupport, isSSE4A);

end;

procedure GetProcessorCacheInfo;
{ preconditions: 1. maximum CPUID must be at least $00000002
  2. GetCPUVendor must have been called }
type
 TConfigDescriptor = packed array [0 .. 15] of Byte;
var
 Registers: TRegisters;
 i, j: Integer;
 QueryCount: Byte;
begin
 { call CPUID function 2 }
 GetCPUID($00000002, Registers);
 QueryCount := Registers.EAX and $FF;
 for i := 1 to QueryCount do
  begin
   for j := 1 to 15 do
    with CPU do
     { decode configuration descriptor byte }
     case TConfigDescriptor(Registers)[j] of
      $06:
       CodeL1CacheSize := 8;
      $08:
       CodeL1CacheSize := 16;
      $09:
       CodeL1CacheSize := 32;
      $0A:
       DataL1CacheSize := 8;
      $0C:
       DataL1CacheSize := 16;
      $0D:
       DataL1CacheSize := 16;
      $21:
       L2CacheSize := 256;
      $22:
       L3CacheSize := 512;
      $23:
       L3CacheSize := 1024;
      $25:
       L3CacheSize := 2048;
      $29:
       L3CacheSize := 4096;
      $2C:
       DataL1CacheSize := 32;
      $30:
       CodeL1CacheSize := 32;
      $39:
       L2CacheSize := 128;
      $3B:
       L2CacheSize := 128;
      $3C:
       L2CacheSize := 256;
      $3D:
       L2CacheSize := 384;
      $3E:
       L2CacheSize := 512;
      $40: { no 2nd-level cache or, if processor contains a valid 2nd-level
         cache, no 3rd-level cache }
       if L2CacheSize <> 0 then
        L3CacheSize := 0;
      $41:
       L2CacheSize := 128;
      $42:
       L2CacheSize := 256;
      $43:
       L2CacheSize := 512;
      $44:
       L2CacheSize := 1024;
      $45:
       L2CacheSize := 2048;
      $46:
       L3CacheSize := 4096;
      $47:
       L3CacheSize := 8192;
      $48:
       L2CacheSize := 3072;
      $49:
       if (CPU.Vendor = cvIntel) and (CPU.EffFamily = $F) and (CPU.EffModel = 6) then
        L3CacheSize := 4096
       else
        L2CacheSize := 4096;
      $4A:
       L3CacheSize := 6144;
      $4B:
       L3CacheSize := 8192;
      $4C:
       L3CacheSize := 12288;
      $4D:
       L3CacheSize := 16384;
      $4E:
       L2CacheSize := 6144;
      $60:
       DataL1CacheSize := 16;
      $66:
       DataL1CacheSize := 8;
      $67:
       DataL1CacheSize := 16;
      $68:
       DataL1CacheSize := 32;
      $70:
       if not(CPU.Vendor in [cvCyrix, cvNSC]) then
        CodeL1CacheSize := 12; { K micro-ops }
      $71:
       CodeL1CacheSize := 16; { K micro-ops }
      $72:
       CodeL1CacheSize := 32; { K micro-ops }
      $78:
       L2CacheSize := 1024;
      $79:
       L2CacheSize := 128;
      $7A:
       L2CacheSize := 256;
      $7B:
       L2CacheSize := 512;
      $7C:
       L2CacheSize := 1024;
      $7D:
       L2CacheSize := 2048;
      $7F:
       L2CacheSize := 512;
      $80:
       if CPU.Vendor in [cvCyrix, cvNSC] then
        begin { Cyrix and NSC only - 16 KB unified L1 cache }
         CodeL1CacheSize := 8;
         DataL1CacheSize := 8;
        end;
      $82:
       L2CacheSize := 256;
      $83:
       L2CacheSize := 512;
      $84:
       L2CacheSize := 1024;
      $85:
       L2CacheSize := 2048;
      $86:
       L2CacheSize := 512;
      $87:
       L2CacheSize := 1024;
      $D0:
       L3CacheSize := 512;
      $D1:
       L3CacheSize := 1024;
      $D2:
       L3CacheSize := 2048;
      $D6:
       L3CacheSize := 1024;
      $D7:
       L3CacheSize := 2048;
      $D8:
       L3CacheSize := 4096;
      $DC:
       L3CacheSize := 1536;
      $DD:
       L3CacheSize := 3072;
      $DE:
       L3CacheSize := 6144;
      $E2:
       L3CacheSize := 2048;
      $E3:
       L3CacheSize := 4096;
      $E4:
       L3CacheSize := 8192;
      $EA:
       L3CacheSize := 12288;
      $EB:
       L3CacheSize := 18432;
      $EC:
       L3CacheSize := 24576;
     end;
   if i < QueryCount then
    GetCPUID(2, Registers);
  end;
end;

procedure GetExtendedProcessorCacheInfo;
{ preconditions: 1. maximum extended CPUID must be at least $80000006
  2. GetCPUVendor and GetCPUFeatures must have been called }
var
 Registers: TRegisters;
begin
 { call CPUID function $80000005 }
 GetCPUID($80000005, Registers);

 { get L1 cache size }
 { Note: Intel does not support function $80000005 for L1 cache size, so ignore.
   Cyrix returns CPUID function 2 descriptors (already done), so ignore. }
 if not(CPU.Vendor in [cvIntel, cvCyrix]) then
  begin
   CPU.CodeL1CacheSize := Registers.EDX shr 24;
   CPU.DataL1CacheSize := Registers.ECX shr 24;
  end;

 { call CPUID function $80000006 }
 GetCPUID($80000006, Registers);

 { get L2 cache size }
 if (CPU.Vendor = cvAMD) and (CPU.Signature and $FFF = K7DuronA0Signature) then
  { workaround for AMD Duron Rev A0 L2 cache size erratum - see AMD Technical
    Note TN-13 }
  CPU.L2CacheSize := 64
 else if (CPU.Vendor = cvCentaur) and (CPU.EffFamily = 6) and
   (CPU.EffModel in [C3Samuel2EffModel, C3EzraEffModel]) then
  { handle VIA (Centaur) C3 Samuel 2 and Ezra non-standard encoding }
  CPU.L2CacheSize := Registers.ECX shr 24
 else { standard encoding }
  CPU.L2CacheSize := Registers.ECX shr 16;
end;

procedure VerifyOSSupportForXMMRegisters;
{$IFDEF  WIN32}
begin
 { try a SSE instruction that operates on XMM registers }
 try
  asm
   ANDPS XMM0, XMM0
  end
 except
  begin
   { if it fails, assume that none of the SSE instruction sets are available }
   Exclude(CPU.InstructionSupport, isSSE);
   Exclude(CPU.InstructionSupport, isSSE2);
   Exclude(CPU.InstructionSupport, isSSE3);
   Exclude(CPU.InstructionSupport, isSSSE3);
   Exclude(CPU.InstructionSupport, isSSE41);
   Exclude(CPU.InstructionSupport, isSSE42);
   Exclude(CPU.InstructionSupport, isSSE5);
   Exclude(CPU.InstructionSupport, isSSE4A);
  end;
 end;
end;
{$ELSE}
begin
 { try a SSE instruction that operates on XMM registers }
 try
//  asm
//   ANDPS XMM0, XMM0
//  end
 except
  begin
   { if it fails, assume that none of the SSE instruction sets are available }
   Exclude(CPU.InstructionSupport, isSSE);
   Exclude(CPU.InstructionSupport, isSSE2);
   Exclude(CPU.InstructionSupport, isSSE3);
   Exclude(CPU.InstructionSupport, isSSSE3);
   Exclude(CPU.InstructionSupport, isSSE41);
   Exclude(CPU.InstructionSupport, isSSE42);
   Exclude(CPU.InstructionSupport, isSSE5);
   Exclude(CPU.InstructionSupport, isSSE4A);
  end;
 end;
end;
{$ENDIF}

function IsXmmYmmOSEnabled: boolean;
asm
 {$IFDEF CPU386}
  push ebx
 {$ELSE CPUX64}
  mov r10, rbx
 {$ENDIF}
  mov eax, 1
  cpuid
  bt ecx, 27      // CPUID.1:ECX.OSXSAVE[bit 27] = 1 means that XGETBV is enabled for application use; this also implies XGETBV is an available instruction
  jnc @not_supported
    xor ecx, ecx  //Specify control register XCR0 = XFEATURE_ENABLED_MASK register
    db 0Fh, 01h, 0D0h // XGETBV //Reads XCR (extended control register) -> EDX:EAX
      //NB: LGDT eax = db 0Fh, 01h = privileged instruction, so don't go here unless XGETBV is allowed/enabled
      //CHECK XFEATURE_ENABLED_MASK[2:1] = 11b
      and eax, 06h // 06h= 00000000000000000000000000000110b
      cmp eax, 06h // check OS has enabled both XMM (bit 1) and YMM (bit 2) state management support
    jne @not_supported
      mov eax, 1
      jmp @out
  @not_supported:
    xor eax, eax
  @out:
{$IFDEF CPU386}
  pop ebx
{$ELSE CPUX64}
  mov rbx, r10
{$ENDIF}
end;

//http://software.intel.com/en-us/articles/introduction-to-intel-advanced-vector-extensions/
// Necessary to check that IsXmmYmmOSEnabled = true before using AVX, AVX2, FMA, etc. instructions!
procedure VerifyOSSupportForYMMRegisters;
begin
 if not IsXmmYmmOSEnabled then
  begin
   Exclude(CPU.InstructionSupport, isAVX);
  end;
end;

procedure GetCPUInfo;
var
 Registers: TRegisters;
 MaxCPUID: Cardinal;
 MaxExCPUID: Cardinal;
begin
 { initialize - just to be sure }
 System.FillChar(CPU, SizeOf(CPU), 0); //need System prefix because var FillChar is empty here

 try
  if not IsCPUID_Available then
   begin
    if IsFPU_Available then
     Include(CPU.InstructionSupport, isFPU);
   end
  else
   begin
    { get maximum CPUID input value }
    GetCPUID($00000000, Registers);
    MaxCPUID := Registers.EAX;

    { get CPU vendor - Max CPUID will always be >= 0 }
    GetCPUVendor;

    { get CPU features if available }
    if MaxCPUID >= $00000001 then
     GetCPUFeatures;

    { get cache info if available }
    if MaxCPUID >= $00000002 then
     GetProcessorCacheInfo;

    { get maximum extended CPUID input value }
    GetCPUID($80000000, Registers);
    MaxExCPUID := Registers.EAX;

    { get CPU extended features if available }
    if MaxExCPUID >= $80000001 then
     GetCPUExtendedFeatures;

    { verify operating system support for XMM registers }
    if isSSE in CPU.InstructionSupport then
     VerifyOSSupportForXMMRegisters;

    { verify operating system support for YMM registers }
    if isAVX in CPU.InstructionSupport then
     VerifyOSSupportForYMMRegisters;

    { get extended cache features if available }
    { Note: ignore processors that only report L1 cache info,
      i.e. have a MaxExCPUID = $80000005 }
    if MaxExCPUID >= $80000006 then
     GetExtendedProcessorCacheInfo;
   end;
 except
  { silent exception - should not occur, just ignore }
 end;
end;


procedure GetFastCodeTarget;
{ precondition: GetCPUInfo must have been called }
begin
{$IFDEF FastcodeSizePenalties}
 FastCodeTarget := fctIA32SizePenalty;
{$ELSE}
 FastCodeTarget := fctIA32;
{$ENDIF}
 if (isSSE41 in CPU.InstructionSupport) then
  FastCodeTarget := fctSSE41
 else
   if (isSSSE3 in CPU.InstructionSupport) then
  FastCodeTarget := fctSSSE3
 else
   if (isSSE3 in CPU.InstructionSupport) then
  FastCodeTarget := fctSSE3
 else
   if (isSSE2 in CPU.InstructionSupport) then
  FastCodeTarget := fctSSE2
 else
   if (isSSE in CPU.InstructionSupport) then
  FastCodeTarget := fctSSE
 else
   if ([isFPU, isMMX, isCMOV] <= CPU.InstructionSupport) then
{$IFDEF FastcodeSizePenalties}
  FastCodeTarget := fctMMX_SizePenalty;
{$ELSE}
  FastCodeTarget := fctMMX;
{$ENDIF}
 case CPU.Vendor of
  cvIntel:
   case CPU.EffFamily of
    6: { Intel P6, P2, P3, PM }
     case CPU.EffModel of
      $1E, $1A, $2E:
       FastCodeTarget := fctPMNehalem;     // Nehalem
      $25, $2C, $2F:
       FastCodeTarget := fctPMWestmere;    // Westmere
      $2A, $2D:
       FastCodeTarget := fctPMSandyBridge; // Sandy Bridge
      PMIvyBridgeEffModel:
       FastCodeTarget := fctPMIvyBridge;   // Ivy Bridge
     end;
   end;
  cvAMD:
   case CPU.EffFamily of
    $10: { AMD K10 }
     FastCodeTarget := fctAmdPhenom;
    $15: { AMD Bulldozer }
     FastCodeTarget := fctAmdBulldozer;
   end;
 end;

end;
{$IFEND}

{$IFDEF NEXTGEN}
procedure FillTwoDigitLoopW;
var
  I: Integer;
  W: Array[0..1] of Byte;
begin
  for i := 0 to 99 do
  begin
    W[0] := Ord(TwoDigitLookupUni[I][1]);
    W[1] := Ord(TwoDigitLookupUni[I][2]);
    TwoDigitLookupW[I] := PWord(@W)^;
  end;
end;
{$ELSE}
procedure FillTwoDigitLoopLW;
var
  I: Integer;
  W: Array[0..1] of Word;
begin
  for i := 0 to 99 do
  begin
    W[0] := Ord(TwoDigitLookupRaw[I][1]);
    W[1] := Ord(TwoDigitLookupRaw[I][2]);
    TwoDigitLookupLW[I] := PLongWord(@W)^;
  end;
end;
{$ENDIF}

initialization
{$IFDEF NEXTGEN}
FillTwoDigitLoopW;
{$ELSE}
FillTwoDigitLoopLW;
{$ENDIF}

{$If defined(Use_FastCodeFillChar) or defined(PatchSystemMove) or defined(USE_FAST_STRLEN) or defined(USE_FAST_CHARPOS) or defined(FAST_MOVE)}
  GetCPUInfo;
  GetFastCodeTarget;
{$IFEND}

{$IFDEF Use_FastCodeFillChar}
  if isSSE3 in CPU.InstructionSupport then
    FillChar := FillChar_DKC_SSE2_10_b {Processor Supports SSE3}
  else
    if isSSE2 in CPU.InstructionSupport then
      FillChar := FillChar_DKC_SSE2_10_b {Processor Supports SSE2}
    else
      if isSSE in CPU.InstructionSupport then
        FillChar := FillChar_JOH_SSE_1_d {Processor Supports SSE}
      else
        if isMMX in CPU.InstructionSupport then
          FillChar := FillChar_JOH_MMX_4_b {Processor Supports MMX}
        else
          FillChar := FillChar_JOH_IA32_4_a; {Processor does not Support MMX or SSE}
{$ENDIF Use_FastCodeFillChar}

{$IF defined(PatchSystemMove) or defined(FAST_MOVE)} //set in Zeos.inc
  if isSSE3 in CPU.InstructionSupport then
    Move := Move_JOH_SSE3_10 {Processor Supports SSE3}
  else
    if isSSE2 in CPU.InstructionSupport then
      Move := Move_JOH_SSE2_10 {Processor Supports SSE2}
    else
      if isSSE in CPU.InstructionSupport then
        Move := Move_JOH_SSE_10 {Processor Supports SSE}
      else
        if isMMX in CPU.InstructionSupport then
          Move := Move_JOH_MMX_10 {Processor Supports MMX}
        else
          Move := Move_JOH_IA32_10; {Processor does not Support MMX or SSE}
  CacheLimit := CPU.L2CacheSize * -512; {Used within SSE Based Moves}
  {$IFDEF PatchSystemMove}
  PatchMove; {Patch Delphi's System.Move}
  {$ENDIF}
{$IFEND} //set in Zeos.inc

{$IFDEF USE_FAST_STRLEN}
  if isSSE3 in CPU.InstructionSupport then
    StrLen := StrLen_JOH_SSE2_2_a {Processor Supports SSE3}
  else
    if isSSE2 in CPU.InstructionSupport then
      StrLen := StrLen_JOH_SSE2_2_a {Processor Supports SSE2}
    else
      StrLen := StrLen_JOH_IA32_7_d; {Processor does not Support SSE3 or SSE2}
{$ELSE}
  {$IFDEF WITH_STRLEN_DEPRECATED}
    StrLen := AnsiStrings.StrLen;
  {$ELSE}
    {$IFDEF NEXTGEN}
    StrLen := StrLen_JOH_PAS_3_a;
    {$ELSE}
    StrLen := {$IFDEF FPC}System.{$ELSE}SysUtils.{$ENDIF}StrLen;
    {$ENDIF}
  {$ENDIF WITH_STRLEN_DEPRECATED}
{$ENDIF USE_FAST_STRLEN}

{$IFDEF USE_FAST_CHARPOS}
if isSSE2 in CPU.InstructionSupport then
  CharPos := CharPos_JOH_SSE2_1_c {Processor Supports SSE2}
else
  CharPos := CharPos_Sha_Pas_2_b;
{$ENDIF}

end.
