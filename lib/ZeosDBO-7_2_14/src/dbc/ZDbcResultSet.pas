{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Abstract Database Connectivity Classes          }
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

unit ZDbcResultSet;

interface

{$I ZDbc.inc}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  ZDbcIntfs, ZClasses, ZSysUtils, ZCompatibility, ZVariant;

type
  {** Implements Abstract ResultSet. }
  TZAbstractResultSet = class(TZCodePagedObject, IZResultSet)
  private
    FRowNo: Integer;
    FLastRowNo: Integer;
    FMaxRows: Integer;
    FClosed: Boolean;
    FFetchDirection: TZFetchDirection;
    FFetchSize: Integer;
    FResultSetType: TZResultSetType;
    FResultSetConcurrency: TZResultSetConcurrency;
    FPostUpdates: TZPostUpdatesMode;
    FLocateUpdates: TZLocateUpdatesMode;
    FColumnsInfo: TObjectList;
    FMetadata: TContainedObject;
    FStatement: IZStatement;
    FWeakIntfPtrOfSelf: Pointer; //EH: Remainder for dereferencing on stmt
    //note: while in destruction IZResultSet(Self) has no longer the same pointer address!
    //so we mark the address in constructor
  protected
    FRawTemp: RawByteString;
    FUniTemp: ZWideString;
    LastWasNull: Boolean;

    function InternalGetString(ColumnIndex: Integer): RawByteString; virtual;

    procedure RaiseUnsupportedException;
    procedure RaiseForwardOnlyException;
    procedure RaiseReadOnlyException;
    procedure CheckClosed;
    procedure CheckColumnConvertion(ColumnIndex: Integer; ResultType: TZSQLType);
    procedure CheckBlobColumn(ColumnIndex: Integer);
    procedure Open; virtual;

    function GetColumnIndex(const ColumnName: string): Integer;
    property RowNo: Integer read FRowNo write FRowNo;
    property LastRowNo: Integer read FLastRowNo write FLastRowNo;
    property MaxRows: Integer read FMaxRows write FMaxRows;
    property Closed: Boolean read FClosed write FClosed;
    property FetchDirection: TZFetchDirection
      read FFetchDirection write FFetchDirection;
    property FetchSize: Integer read FFetchSize write FFetchSize;
    property ResultSetType: TZResultSetType
      read FResultSetType write FResultSetType;
    property ResultSetConcurrency: TZResultSetConcurrency
      read FResultSetConcurrency write FResultSetConcurrency;
    property Statement: IZStatement read FStatement;
    property Metadata: TContainedObject read FMetadata write FMetadata;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      Metadata: TContainedObject; ConSettings: PZConSettings);
    destructor Destroy; override;

    procedure SetType(Value: TZResultSetType);
    procedure SetConcurrency(Value: TZResultSetConcurrency);

    function Next: Boolean; virtual;
    procedure BeforeClose; virtual;
    procedure Close; virtual;
    procedure AfterClose; virtual;
    procedure ResetCursor; virtual;
    function WasNull: Boolean; virtual;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean; virtual;
    function GetPChar(ColumnIndex: Integer): PChar; virtual;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; overload; virtual;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; overload; virtual;
    function GetPWideChar(ColumnIndex: Integer): PWidechar; overload; virtual;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; overload; virtual;
    function GetString(ColumnIndex: Integer): String; virtual;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString; virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String; virtual;
    {$ENDIF}
    function GetRawByteString(ColumnIndex: Integer): RawByteString; virtual;
    function GetBinaryString(ColumnIndex: Integer): RawByteString; virtual;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; virtual;
    function GetBoolean(ColumnIndex: Integer): Boolean; virtual;
    function GetByte(ColumnIndex: Integer): Byte; virtual;
    function GetShort(ColumnIndex: Integer): ShortInt; virtual;
    function GetWord(ColumnIndex: Integer): Word; virtual;
    function GetSmall(ColumnIndex: Integer): SmallInt; virtual;
    function GetUInt(ColumnIndex: Integer): Cardinal; virtual;
    function GetInt(ColumnIndex: Integer): Integer; virtual;
    function GetULong(ColumnIndex: Integer): UInt64; virtual;
    function GetLong(ColumnIndex: Integer): Int64; virtual;
    function GetFloat(ColumnIndex: Integer): Single; virtual;
    function GetDouble(ColumnIndex: Integer): Double; virtual;
    function GetCurrency(ColumnIndex: Integer): Currency; virtual;
    function GetBigDecimal(ColumnIndex: Integer): Extended; virtual;
    function GetBytes(ColumnIndex: Integer): TBytes; virtual;
    function GetDate(ColumnIndex: Integer): TDateTime; virtual;
    function GetTime(ColumnIndex: Integer): TDateTime; virtual;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; virtual;
    function GetAsciiStream(ColumnIndex: Integer): TStream; virtual;
    function GetUnicodeStream(ColumnIndex: Integer): TStream; virtual;
    function GetBinaryStream(ColumnIndex: Integer): TStream; virtual;
    function GetBlob(ColumnIndex: Integer): IZBlob; virtual;
    function GetDataSet(ColumnIndex: Integer): IZDataSet; virtual;
    function GetValue(ColumnIndex: Integer): TZVariant; virtual;
    function GetDefaultExpression(ColumnIndex: Integer): String; virtual;

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    function IsNullByName(const ColumnName: string): Boolean; virtual;
    function GetPCharByName(const ColumnName: string): PChar; virtual;
    function GetPAnsiCharByName(const ColumnName: string): PAnsiChar; overload; virtual;
    function GetPAnsiCharByName(const ColumnName: string; out Len: NativeUInt): PAnsiChar; overload; virtual;
    function GetPWideCharByName(const ColumnName: string): PWidechar; overload; virtual;
    function GetPWideCharByName(const ColumnName: string; out Len: NativeUInt): PWideChar; overload; virtual;
    function GetStringByName(const ColumnName: string): String; virtual;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiStringByName(const ColumnName: string): AnsiString; virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8StringByName(const ColumnName: string): UTF8String; virtual;
    {$ENDIF}
    function GetRawByteStringByName(const ColumnName: string): RawByteString; virtual;
    function GetBinaryStringByName(const ColumnName: string): RawByteString;
    function GetUnicodeStringByName(const ColumnName: string): ZWideString; virtual;
    function GetBooleanByName(const ColumnName: string): Boolean; virtual;
    function GetByteByName(const ColumnName: string): Byte; virtual;
    function GetShortByName(const ColumnName: string): ShortInt; virtual;
    function GetWordByName(const ColumnName: string): Word; virtual;
    function GetSmallByName(const ColumnName: string): SmallInt; virtual;
    function GetUIntByName(const ColumnName: string): Cardinal; virtual;
    function GetIntByName(const ColumnName: string): Integer; virtual;
    function GetULongByName(const ColumnName: string): UInt64; virtual;
    function GetLongByName(const ColumnName: string): Int64; virtual;
    function GetFloatByName(const ColumnName: string): Single; virtual;
    function GetDoubleByName(const ColumnName: string): Double; virtual;
    function GetCurrencyByName(const ColumnName: string): Currency; virtual;
    function GetBigDecimalByName(const ColumnName: string): Extended; virtual;
    function GetBytesByName(const ColumnName: string): TBytes; virtual;
    function GetDateByName(const ColumnName: string): TDateTime; virtual;
    function GetTimeByName(const ColumnName: string): TDateTime; virtual;
    function GetTimestampByName(const ColumnName: string): TDateTime; virtual;
    function GetAsciiStreamByName(const ColumnName: string): TStream; virtual;
    function GetUnicodeStreamByName(const ColumnName: string): TStream; virtual;
    function GetBinaryStreamByName(const ColumnName: string): TStream; virtual;
    function GetBlobByName(const ColumnName: string): IZBlob; virtual;
    function GetDataSetByName(const ColumnName: String): IZDataSet; virtual;
    function GetValueByName(const ColumnName: string): TZVariant; virtual;

    //=====================================================================
    // Advanced features:
    //=====================================================================

    function GetWarnings: EZSQLWarning; virtual;
    procedure ClearWarnings; virtual;

    function GetCursorName: String; virtual;
    function GetMetaData: IZResultSetMetaData; virtual;
    function FindColumn(const ColumnName: string): Integer; virtual;

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    function IsBeforeFirst: Boolean; virtual;
    function IsAfterLast: Boolean; virtual;
    function IsFirst: Boolean; virtual;
    function IsLast: Boolean; virtual;
    procedure BeforeFirst; virtual;
    procedure AfterLast; virtual;
    function First: Boolean; virtual;
    function Last: Boolean; virtual;
    function GetRow: NativeInt; virtual;
    function MoveAbsolute(Row: Integer): Boolean; virtual;
    function MoveRelative(Rows: Integer): Boolean; virtual;
    function Previous: Boolean; virtual;

    //---------------------------------------------------------------------
    // Properties
    //---------------------------------------------------------------------

    procedure SetFetchDirection(Direction: TZFetchDirection); virtual;
    function GetFetchDirection: TZFetchDirection; virtual;

    procedure SetFetchSize(Rows: Integer); virtual;
    function GetFetchSize: Integer; virtual;

    function GetType: TZResultSetType; virtual;
    function GetConcurrency: TZResultSetConcurrency; virtual;

    function GetPostUpdates: TZPostUpdatesMode;
    function GetLocateUpdates: TZLocateUpdatesMode;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean; virtual;
    function RowInserted: Boolean; virtual;
    function RowDeleted: Boolean; virtual;

    procedure UpdateNull(ColumnIndex: Integer); virtual;
    procedure UpdateBoolean(ColumnIndex: Integer; const Value: Boolean); virtual;
    procedure UpdateByte(ColumnIndex: Integer; const Value: Byte); virtual;
    procedure UpdateShort(ColumnIndex: Integer; const Value: ShortInt); virtual;
    procedure UpdateWord(ColumnIndex: Integer; const Value: Word); virtual;
    procedure UpdateSmall(ColumnIndex: Integer; const Value: SmallInt); virtual;
    procedure UpdateUInt(ColumnIndex: Integer; const Value: Cardinal); virtual;
    procedure UpdateInt(ColumnIndex: Integer; const Value: Integer); virtual;
    procedure UpdateULong(ColumnIndex: Integer; const Value: UInt64); virtual;
    procedure UpdateLong(ColumnIndex: Integer; const Value: Int64); virtual;
    procedure UpdateFloat(ColumnIndex: Integer; const Value: Single); virtual;
    procedure UpdateDouble(ColumnIndex: Integer; const Value: Double); virtual;
    procedure UpdateCurrency(ColumnIndex: Integer; const Value: Currency); virtual;
    procedure UpdateBigDecimal(ColumnIndex: Integer; const Value: Extended); virtual;
    procedure UpdatePChar(ColumnIndex: Integer; const Value: PChar); virtual;
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar); overload; virtual;
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar; Len: PNativeUInt); overload; virtual;
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar); overload; virtual;
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar; Len: PNativeUInt); overload; virtual;
    procedure UpdateString(ColumnIndex: Integer; const Value: String); virtual;
    {$IFNDEF NO_ANSISTRING}
    procedure UpdateAnsiString(ColumnIndex: Integer; const Value: AnsiString); virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure UpdateUTF8String(ColumnIndex: Integer; const Value: UTF8String); virtual;
    {$ENDIF}
    procedure UpdateRawByteString(ColumnIndex: Integer; const Value: RawByteString); virtual;
    procedure UpdateBinaryString(ColumnIndex: Integer; const Value: RawByteString);
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: ZWideString); virtual;
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TBytes); virtual;
    procedure UpdateDate(ColumnIndex: Integer; const Value: TDateTime); virtual;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TDateTime); virtual;
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TDateTime); virtual;
    procedure UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream); virtual;
    procedure UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream); virtual;
    procedure UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream); virtual;
    procedure UpdateLob(ColumnIndex: Integer; const Value: IZBlob); virtual;
    procedure UpdateDataSet(ColumnIndex: Integer; const Value: IZDataSet); virtual;
    procedure UpdateValue(ColumnIndex: Integer; const Value: TZVariant); virtual;
    procedure UpdateDefaultExpression(ColumnIndex: Integer; const Value: string); virtual;

    //======================================================================
    // Methods for accessing results by column name
    //======================================================================

    procedure UpdateNullByName(const ColumnName: string); virtual;
    procedure UpdateBooleanByName(const ColumnName: string; const Value: Boolean); virtual;
    procedure UpdateByteByName(const ColumnName: string; const Value: Byte); virtual;
    procedure UpdateShortByName(const ColumnName: string; const Value: ShortInt); virtual;
    procedure UpdateWordByName(const ColumnName: string; const Value: Word); virtual;
    procedure UpdateSmallByName(const ColumnName: string; const Value: SmallInt); virtual;
    procedure UpdateUIntByName(const ColumnName: string; const Value: Cardinal); virtual;
    procedure UpdateIntByName(const ColumnName: string; const Value: Integer); virtual;
    procedure UpdateULongByName(const ColumnName: string; const Value: UInt64); virtual;
    procedure UpdateLongByName(const ColumnName: string; const Value: Int64); virtual;
    procedure UpdateFloatByName(const ColumnName: string; const Value: Single); virtual;
    procedure UpdateDoubleByName(const ColumnName: string; const Value: Double); virtual;
    procedure UpdateCurrencyByName(const ColumnName: string; const Value: Currency); virtual;
    procedure UpdateBigDecimalByName(const ColumnName: string; const Value: Extended); virtual;
    procedure UpdatePAnsiCharByName(const ColumnName: string; Value: PAnsiChar); overload; virtual;
    procedure UpdatePAnsiCharByName(const ColumnName: string; Value: PAnsiChar; Len: PNativeUInt); overload; virtual;
    procedure UpdatePCharByName(const ColumnName: string; const Value: PChar); virtual;
    procedure UpdatePWideCharByName(const ColumnName: string; Value: PWideChar); overload; virtual;
    procedure UpdatePWideCharByName(const ColumnName: string; Value: PWideChar; Len: PNativeUInt); overload; virtual;
    procedure UpdateStringByName(const ColumnName: string; const Value: String); virtual;
    {$IFNDEF NO_ANSISTRING}
    procedure UpdateAnsiStringByName(const ColumnName: string; const Value: AnsiString); virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure UpdateUTF8StringByName(const ColumnName: string; const Value: UTF8String); virtual;
    {$ENDIF}
    procedure UpdateRawByteStringByName(const ColumnName: string; const Value: RawByteString); virtual;
    procedure UpdateBinaryStringByName(const ColumnName: string; const Value: RawByteString);
    procedure UpdateUnicodeStringByName(const ColumnName: string; const Value: ZWideString); virtual;
    procedure UpdateBytesByName(const ColumnName: string; const Value: TBytes); virtual;
    procedure UpdateDateByName(const ColumnName: string; const Value: TDateTime); virtual;
    procedure UpdateTimeByName(const ColumnName: string; const Value: TDateTime); virtual;
    procedure UpdateTimestampByName(const ColumnName: string; const Value: TDateTime); virtual;
    procedure UpdateAsciiStreamByName(const ColumnName: string; const Value: TStream); virtual;
    procedure UpdateUnicodeStreamByName(const ColumnName: string; const Value: TStream); virtual;
    procedure UpdateBinaryStreamByName(const ColumnName: string; const Value: TStream); virtual;
    procedure UpdateDataSetByName(const ColumnName: string; const Value: IZDataSet); virtual;
    procedure UpdateValueByName(const ColumnName: string; const Value: TZVariant); virtual;

    procedure InsertRow; virtual;
    procedure UpdateRow; virtual;
    procedure DeleteRow; virtual;
    procedure RefreshRow; virtual;
    procedure CancelRowUpdates; virtual;
    procedure MoveToInsertRow; virtual;
    procedure MoveToCurrentRow; virtual;

    function CompareRows(Row1, Row2: NativeInt; const ColumnIndices: TIntegerDynArray;
      const CompareFuncs: TCompareFuncs): Integer; virtual;
    function GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
      const CompareKinds: TComparisonKindArray): TCompareFuncs; virtual;

    function GetStatement: IZStatement; virtual;

    property ColumnsInfo: TObjectList read FColumnsInfo write FColumnsInfo;
  end;

  {** implents a optimal Converter function for Date, Time, DateTime conversion }
  TDateTimeConverter = function (Value, Format: PAnsiChar;
    Const ValLen, FormatLen: Cardinal; var OptConFunc: Pointer): TDateTime;

  {** Implements external or internal blob wrapper object. }
  TZAbstractBlob = class(TInterfacedObject, IZBlob)
  private
  protected
    FBlobData: Pointer;
    FBlobSize: Integer; //All Mem operations except AllocMem(also calls FillChar(P, 0)) use integers. So we can only load MaxInt bytes. More intersting on 64Bit env.
    FUpdated: Boolean;
    procedure InternalClear; virtual;
    property BlobData: Pointer read FBlobData write FBlobData;
    property BlobSize: Integer read FBlobSize write FBlobSize;
    property Updated: Boolean read FUpdated write FUpdated;
  public
    constructor CreateWithStream(Stream: TStream); virtual;
    constructor CreateWithData(Data: Pointer; Size: Integer); virtual;
    destructor Destroy; override;

    function IsEmpty: Boolean; virtual;
    function IsUpdated: Boolean; virtual;
    function Length: Integer; virtual;

    function GetString: RawByteString; virtual;
    procedure SetString(const Value: RawByteString); virtual;
    function GetBytes: TBytes; virtual;
    procedure SetBytes(const Value: TBytes); virtual;
    function GetStream: TStream; virtual;
    procedure SetStream(const Value: TStream); overload; virtual;
    function GetBufferAddress: PPointer;
    function GetLengthAddress: PInteger;
    function GetBuffer: Pointer; virtual;
    procedure SetBuffer(const Buffer: Pointer; const Length: Integer);
    {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
    procedure SetBlobData(const Buffer: Pointer; const Len: Cardinal); overload;
    {$ENDIF}

    procedure Clear; virtual;
    function Clone(Empty: Boolean = False): IZBlob; virtual;
    function IsClob: Boolean; virtual;

    {clob operations}
    function GetRawByteString: RawByteString; virtual;
    procedure SetRawByteString(Const Value: RawByteString; const CodePage: Word); virtual;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString: AnsiString; virtual;
    procedure SetAnsiString(Const Value: AnsiString); virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String: UTF8String; virtual;
    procedure SetUTF8String(Const Value: UTF8String); virtual;
    {$ENDIF}
    procedure SetUnicodeString(const Value: ZWideString); virtual;
    function GetUnicodeString: ZWideString; virtual;
    procedure SetStream(const Value: TStream; const CodePage: Word); overload; virtual;
    function GetRawByteStream: TStream; virtual;
    function GetAnsiStream: TStream; virtual;
    function GetUTF8Stream: TStream; virtual;
    function GetUnicodeStream: TStream; virtual;
    function GetPAnsiChar(const CodePage: Word): PAnsiChar; virtual;
    procedure SetPAnsiChar(const Buffer: PAnsiChar; const CodePage: Word; const Len: Cardinal); virtual;
    function GetPWideChar: PWideChar; virtual;
    procedure SetPWideChar(const Buffer: PWideChar; const Len: Cardinal); virtual;
    {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
    procedure SetBlobData(const Buffer: Pointer; const Len: Cardinal; const CodePage: Word); overload; virtual;
    {$ENDIF}
  end;

  TZAbstractUnCachedBlob = class(TZAbstractBlob)
  private
    FLoaded: Boolean;
  protected
    procedure ReadLob; virtual;
    procedure WriteLob; virtual;
    property Loaded: Boolean read FLoaded;
  public
    function IsEmpty: Boolean; override;
    function Length: Integer; override;
    function GetString: RawByteString; override;
    function GetBytes: TBytes; override;
    function GetStream: TStream; override;
    function GetBuffer: Pointer; override;
    function Clone(Empty: Boolean = False): IZBlob; override;
    procedure FlushBuffer; virtual;
  end;

  {** Implements external or internal clob wrapper object. }
  TZAbstractCLob = class(TZAbstractBlob)
  protected
    FCurrentCodePage: Word;
    FConSettings: PZConSettings;
    procedure InternalSetRawByteString(Const Value: RawByteString; const CodePage: Word);
    {$IFNDEF NO_ANSISTRING}
    procedure InternalSetAnsiString(Const Value: AnsiString);
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure InternalSetUTF8String(Const Value: UTF8String);
    {$ENDIF}
    procedure InternalSetUnicodeString(const Value: ZWideString);
    procedure InternalSetPAnsiChar(const Buffer: PAnsiChar; CodePage: Word; const Len: Cardinal);
    procedure InternalSetPWideChar(const Buffer: PWideChar; const Len: Cardinal);
    property Updated: Boolean read FUpdated write FUpdated;
    property CurrentCodePage: Word read FCurrentCodePage;
  public
    constructor CreateWithStream(Stream: TStream; const CodePage: Word;
      const ConSettings: PZConSettings); reintroduce;
    constructor CreateWithData(Data: PAnsiChar; const Len: Cardinal;
      const CodePage: Word; const ConSettings: PZConSettings); reintroduce; overload;
    constructor CreateWithData(Data: PWideChar; const Len: Cardinal;
      const ConSettings: PZConSettings); reintroduce; overload;

    function Length: Integer; override;
    function GetString: RawByteString; override; //deprected;
    function GetRawByteString: RawByteString; override;
    procedure SetRawByteString(Const Value: RawByteString; const CodePage: Word); override;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString: AnsiString; override;
    procedure SetAnsiString(Const Value: AnsiString); override;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String: UTF8String; override;
    procedure SetUTF8String(Const Value: UTF8String); override;
    {$ENDIF}
    function GetUnicodeString: ZWideString; override;
    procedure SetUnicodeString(const Value: ZWideString); override;
    function GetStream: TStream; override;
    procedure SetStream(const Value: TStream); overload; override;
    procedure SetStream(const Value: TStream; const CodePage: Word); reintroduce; overload; override;
    function GetRawByteStream: TStream; override;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiStream: TStream; override;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8Stream: TStream; override;
    {$ENDIF}
    function GetUnicodeStream: TStream; override;
    function GetPAnsiChar(const CodePage: Word): PAnsiChar; override;
    procedure SetPAnsiChar(const Buffer: PAnsiChar; const CodePage: Word; const Len: Cardinal); override;
    function GetPWideChar: PWideChar; override;
    procedure SetPWideChar(const Buffer: PWideChar; const Len: Cardinal); override;
    {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
    procedure SetBlobData(const Buffer: Pointer; const Len: Cardinal; const CodePage: Word); override;
    {$ENDIF}

    function Clone(Empty: Boolean = False): IZBLob; override;
    function IsClob: Boolean; override;
  end;

  TZAbstractUnCachedCLob = Class(TZAbstractCLob)
  private
    FLoaded: Boolean;
  protected
    property Loaded: Boolean read FLoaded;
    procedure ReadLob; virtual;
    procedure WriteLob; virtual;
  public
    function Length: Integer; override;
    function IsEmpty: Boolean; override;
    function GetRawByteString: RawByteString; override;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString: AnsiString; override;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String: UTF8String; override;
    {$ENDIF}
    function GetUnicodeString: ZWideString; override;
    function GetStream: TStream; override;
    function GetRawByteStream: TStream; override;
    function GetAnsiStream: TStream; override;
    function GetUTF8Stream: TStream; override;
    function GetUnicodeStream: TStream; override;
    function GetPAnsiChar(const CodePage: Word): PAnsiChar; override;
    function GetPWideChar: PWideChar; override;
    function GetBuffer: Pointer; override;
    function Clone(Empty: Boolean = False): IZBLob; override;
    procedure FlushBuffer; virtual;
  End;

implementation

uses ZMessages, ZDbcUtils, ZDbcResultSetMetadata, ZEncoding, ZFastCode
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}{$IFNDEF FPC},Math{$ENDIF};

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // parameters not used intentionally
function CompareNothing(const Null1, Null2: Boolean; const V1, V2): Integer; //emergency exit for complex types we can't sort quickly like arrays, dataset ...
begin
  Result := 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

function CompareBoolean_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(TZVariant(V1).VBoolean)-Ord(TZVariant(V2).VBoolean);
end;

function CompareBoolean_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareBoolean_Asc(Null1, Null2, V1, V2);
end;

function CompareInt64_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(TZVariant(V1).VInteger > TZVariant(V2).VInteger)-Ord(TZVariant(V1).VInteger < TZVariant(V2).VInteger);
end;

function CompareInt64_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareInt64_Asc(Null1, Null2, V1, V2);
end;

function CompareUInt64_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(TZVariant(V1).VUInteger > TZVariant(V2).VUInteger)-Ord(TZVariant(V1).VUInteger < TZVariant(V2).VUInteger);
end;

function CompareUInt64_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUInt64_Asc(Null1, Null2, V1, V2);
end;

function CompareFloat_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(TZVariant(V1).VFloat > TZVariant(V2).VFloat)-Ord(TZVariant(V1).VFloat < TZVariant(V2).VFloat);
end;

function CompareFloat_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareFloat_Asc(Null1, Null2, V1, V2);
end;

function CompareDateTime_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := Ord(TZVariant(V1).VDateTime > TZVariant(V2).VDateTime)-Ord(TZVariant(V1).VDateTime < TZVariant(V2).VDateTime);
end;

function CompareDateTime_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareDateTime_Asc(Null1, Null2, V1, V2);
end;

function CompareBytes_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1 else
  begin
    Result := Length(TZVariant(V1).VBytes) - Length(TZVariant(V2).VBytes); //overflow save!
    if Result = 0 then
      Result := ZMemLComp(Pointer(TZVariant(V1).VBytes), Pointer(TZVariant(V2).VBytes),
        Length(TZVariant(V1).VBytes));
  end;
end;

function CompareBytes_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareBytes_Asc(Null1, Null2, V1, V2);
end;

{$IFNDEF WITH_USC2_ANSICOMPARESTR_ONLY}
function CompareRawByteString_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  else Result := {$IFDEF WITH_ANSISTRCOMP_DEPRECATED}AnsiStrings.{$ENDIF}
    AnsiStrComp(PAnsiChar(TZVariant(V1).VRawByteString), PAnsiChar(TZVariant(V2).VRawByteString));
end;

function CompareRawByteString_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareRawByteString_Asc(Null1, Null2, V1, V2);
end;
{$ENDIF}

function CompareUnicodeString_Asc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  if Null1 and Null2 then Result := 0
  else if Null1 then Result := -1
  else if Null2 then Result := 1
  {$IFDEF UNICODE}
  else Result := AnsiCompareStr(TZVariant(V1).VUnicodeString, TZVariant(V2).VUnicodeString);
  {$ELSE}
  else Result := WideCompareStr(TZVariant(V1).VUnicodeString, TZVariant(V2).VUnicodeString);
  {$ENDIF}
end;

function CompareUnicodeString_Desc(const Null1, Null2: Boolean; const V1, V2): Integer;
begin
  Result := -CompareUnicodeString_Asc(Null1, Null2, V1, V2);
end;

{ TZAbstractResultSet }

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
  @param Metadata a resultset metadata object.
  @param ConSettings the pointer to Connection Settings record
}
constructor TZAbstractResultSet.Create(const Statement: IZStatement; const SQL: string;
  Metadata: TContainedObject; ConSettings: PZConSettings);
var
  DatabaseMetadata: IZDatabaseMetadata;
  RS: IZResultSet;
begin
  Self.ConSettings := ConSettings;
  LastWasNull := True;
  FRowNo := 0;
  FLastRowNo := 0;
  FClosed := True;

  { the constructor keeps the refcount to 1}
  QueryInterface(IZResultSet, RS);
  FWeakIntfPtrOfSelf := Pointer(RS); //Remainder for unregister on stmt!
  RS := nil;
  if Statement = nil then begin
    FResultSetType := rtForwardOnly;
    FResultSetConcurrency := rcReadOnly;
    FPostUpdates := poColumnsAll;
    FLocateUpdates := loWhereAll;
    FMaxRows := 0;
  end else begin
    FFetchDirection := Statement.GetFetchDirection;
    FFetchSize := Statement.GetFetchSize;
    FResultSetType := Statement.GetResultSetType;
    FResultSetConcurrency := Statement.GetResultSetConcurrency;
    FPostUpdates := Statement.GetPostUpdates;
    FLocateUpdates := Statement.GetLocateUpdates;
    FStatement := Statement;
    FMaxRows := Statement.GetMaxRows;
  end;

  if Metadata = nil then begin
    if Statement <> nil
    then DatabaseMetadata := GetStatement.GetConnection.GetMetadata
    else DatabaseMetadata := nil;
    FMetadata := TZAbstractResultSetMetadata.Create(DatabaseMetadata, SQL, Self);
   end else
    FMetadata := Metadata;

  FColumnsInfo := TObjectList.Create(True); //Free the MemoryLeaks of TZColumnInfo
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractResultSet.Destroy;
begin
  if not FClosed then
      Close;
  FreeAndNil(FMetadata);
  FreeAndNil(FColumnsInfo);
  inherited Destroy;
end;

function TZAbstractResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := EmptyRaw;
end;

{**
  Raises unsupported operation exception.
}
procedure TZAbstractResultSet.RaiseUnsupportedException;
begin
  raise EZSQLException.Create(SUnsupportedOperation);
end;

{**
  Raises operation is not allowed in FORWARD ONLY mode exception.
}
procedure TZAbstractResultSet.RaiseForwardOnlyException;
begin
  raise EZSQLException.Create(SOperationIsNotAllowed1);
end;

{**
  Raises operation is not allowed in READ ONLY mode exception.
}
procedure TZAbstractResultSet.RaiseReadOnlyException;
begin
  raise EZSQLException.Create(SOperationIsNotAllowed2);
end;

{**
  Checks if result set is open and operation is allowed.
}
procedure TZAbstractResultSet.CheckClosed;
begin
  if FClosed then
    raise EZSQLException.Create(SOperationIsNotAllowed4);
end;

{**
  Checks is the column convertion from one type to another type allowed.
  @param ColumnIndex an index of column.
  @param ResultType a requested data type.
}
procedure TZAbstractResultSet.CheckColumnConvertion(ColumnIndex: Integer;
  ResultType: TZSQLType);
var
  InitialType: TZSQLType;
  Metadata: TZAbstractResultSetMetadata;
begin
  CheckClosed;
  Metadata := TZAbstractResultSetMetadata(FMetadata);
  if (Metadata = nil) or (ColumnIndex < FirstDbcIndex) or
     (ColumnIndex > Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));

  InitialType := Metadata.GetColumnType(ColumnIndex);
  if not CheckConvertion(InitialType, ResultType) then
    raise EZSQLException.Create(Format(SConvertionIsNotPossible, [ColumnIndex,
       DefineColumnTypeName(InitialType), DefineColumnTypeName(ResultType)]));
end;

{**
  Checks for blob expected column.
  @param ColumnIndex an index of column.
}
procedure TZAbstractResultSet.CheckBlobColumn(ColumnIndex: Integer);
var
  InitialType: TZSQLType;
  Metadata: TZAbstractResultSetMetadata;
begin
  CheckClosed;
  Metadata := TZAbstractResultSetMetadata(FMetadata);
  if (Metadata = nil) or (ColumnIndex < FirstDbcIndex) or
     (ColumnIndex > Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));

  InitialType := Metadata.GetColumnType(ColumnIndex);
  if not (InitialType in [stAsciiStream, stBinaryStream, stUnicodeStream]) then
    raise EZSQLException.Create(Format(SCanNotAccessBlobRecord,
      [ColumnIndex, DefineColumnTypeName(InitialType)]));
end;

{**
  Set the concurrency mode of this <code>ResultSet</code> object.
  The concurrency used is determined by the
  <code>Statement</code> object that created the result set.

  @param the concurrency type, either <code>CONCUR_READ_ONLY</code>
    or <code>CONCUR_UPDATABLE</code>
}
procedure TZAbstractResultSet.SetConcurrency(Value: TZResultSetConcurrency);
begin
  ResultSetConcurrency := Value;
end;

{**
  Set the type of this <code>ResultSet</code> object.
  The type is determined by the <code>Statement</code> object
  that created the result set.

  @param <code>TYPE_FORWARD_ONLY</code>,
    <code>TYPE_SCROLL_INSENSITIVE</code>,
    or <code>TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractResultSet.SetType(Value: TZResultSetType);
begin
  ResultSetType := Value;
end;

{**
  Opens this recordset.
}
procedure TZAbstractResultSet.Open;
begin
  FClosed := False;
end;

{**
  Resets cursor position of this recordset and
  the overrides should reset the prepared handles.
}
procedure TZAbstractResultSet.ResetCursor;
begin
  if not FClosed then begin
    if Assigned(Statement){virtual RS ! }  then begin
    FFetchSize := Statement.GetFetchSize;
    FPostUpdates := Statement.GetPostUpdates;
    FLocateUpdates := Statement.GetLocateUpdates;
    FMaxRows := Statement.GetMaxRows;
    end;
    FRowNo := 0;
    FLastRowNo := 0;
    LastWasNull := True;
  end;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZAbstractResultSet.Close;
var RefCountAdded: Boolean;
begin
  if not Closed then begin
    BeforeClose;
    FClosed := True;
    RefCountAdded := False;
    try
      if (FStatement <> nil) then begin
        if (RefCount = 1) then begin
          _AddRef;
          RefCountAdded := True;
        end;
        FStatement.FreeOpenResultSetReference(IZResultSet(FWeakIntfPtrOfSelf));
        FStatement := nil;
      end;
      AfterClose;
    finally
      if RefCountAdded then begin
        if (RefCount = 1) then
          DriverManager.AddGarbage(Self);
        _Release;
      end;
  end;
  end;
end;

{**
  Reports whether
  the last column read had a value of SQL <code>NULL</code>.
  Note that you must first call one of the <code>getXXX</code> methods
  on a column to try to read its value and then call
  the method <code>wasNull</code> to see if the value read was
  SQL <code>NULL</code>.

  @return <code>true</code> if the last column value read was SQL
    <code>NULL</code> and <code>false</code> otherwise
}
function TZAbstractResultSet.WasNull: Boolean;
begin
  Result := LastWasNull;
end;

//======================================================================
// Methods for accessing results by column index
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // base class - parameter not used intentionally
function TZAbstractResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  Result := True;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}

function TZAbstractResultSet.GetPChar(ColumnIndex: Integer): PChar;
begin
  {$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF} := GetString(ColumnIndex);
  Result := PChar({$IFDEF UNICODE}FUniTemp{$ELSE}FRawTemp{$ENDIF});
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
begin
  FRawTemp := GetRawByteString(ColumnIndex);
  if Pointer(FRawTemp) = nil then begin
    Len := 0;
    Result := PEmptyAnsiString;
  end else begin
    {$IFNDEF WITH_TBYTES_AS_RAWBYTESTRING}
    Len := NativeUInt({%H-}PLengthInt(NativeUInt(FRawTemp) - StringLenOffSet)^);
    {$ELSE}
    Len := Length(FRawTemp)-1;
    {$ENDIF}
    Result := Pointer(FRawTemp);
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
begin
  FRawTemp := GetRawByteString(ColumnIndex);
  if Pointer(FRawTemp) = nil then
    Result := PEmptyAnsiString
  else
    Result := Pointer(FRawTemp);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPWideChar(ColumnIndex: Integer): PWidechar;
begin
  FUniTemp := GetUnicodeString(ColumnIndex);
  if Pointer(FUniTemp) = nil then
    Result := PEmptyUnicodeString
  else
    Result := Pointer(FUniTemp);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of UCS2 string in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPWideChar(ColumnIndex: Integer;
  out Len: NativeUInt): PWideChar;
begin
  FUniTemp := GetUnicodeString(ColumnIndex);
  Len := Length(FUniTemp);
  {no RTL conversion to PWideChar}
  if Len = 0 then
    Result := PEmptyUnicodeString
  else
    Result := Pointer(FUniTemp);
end;
{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetString(ColumnIndex: Integer): String;
{$IFDEF UNICODE}
var
  Len: NativeUInt;
  P: PAnsiChar;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  P := GetPAnsiChar(ColumnIndex, Len);
  Result := PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP);
  {$ELSE}
  Result := ConSettings^.ConvFuncs.ZRawToString(InternalGetString(ColumnIndex),
    ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
  {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_ANSISTRING}
function TZAbstractResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
begin
  Result := ConSettings^.ConvFuncs.ZRawToAnsi(InternalGetString(ColumnIndex),
    ConSettings^.ClientCodePage^.CP);
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZAbstractResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var
  P: PAnsiChar;
  Len: NativeUInt;
begin
  P := GetPAnsiChar(ColumnIndex, Len);
  Result := ConSettings^.ConvFuncs.ZPRawToUTF8(P, Len, ConSettings^.ClientCodePage^.CP);
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
begin
  Result := InternalGetString(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetBinaryString(ColumnIndex: Integer): RawByteString;
begin
  Result := InternalGetString(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeString);
{$ENDIF}
  Result := ConSettings^.ConvFuncs.ZRawToUnicode(InternalGetString(ColumnIndex),
    ConSettings^.ClientCodePage^.CP);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  Result := False;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stByte);
{$ENDIF}
  Result := Byte(GetInt(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetShort(ColumnIndex: Integer): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stShort);
{$ENDIF}
  Result := ShortInt(GetInt(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>word</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetWord(ColumnIndex: Integer): Word;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stWord);
{$ENDIF}
  Result := Word(GetInt(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stSmall);
{$ENDIF}
  Result := SmallInt(GetInt(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>uint</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLongWord);
{$ENDIF}
  Result := Cardinal(GetLong(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ulong</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZAbstractResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  Result := 0;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetCurrency(ColumnIndex: Integer): Currency;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stCurrency);
{$ENDIF}
  Result := GetBigDecimal(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.BigDecimal</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param scale the number of digits to the right of the decimal point
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := nil;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
  value returned is <code>null</code>
  @exception SQLException if a database access error occurs
}
function TZAbstractResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimestamp);
{$ENDIF}
  Result := 0;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a stream of ASCII characters. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <char>LONGVARCHAR</char> values.
  The JDBC driver will
  do any necessary conversion from the database format into ASCII.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream of one-byte ASCII characters; if the value is SQL
    <code>NULL</code>, the value returned is <code>null</code>
}
function TZAbstractResultSet.GetAsciiStream(ColumnIndex: Integer): TStream;
var
  Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stAsciiStream);
{$ENDIF}
  Result := nil;
  if not IsNull(ColumnIndex) then
  begin
    Blob := GetBlob(ColumnIndex);
    if Blob <> nil then
      if Blob.IsClob then
        Result := Blob.GetStream
      else if Self.GetMetaData.GetColumnType(ColumnIndex) = stUnicodeStream then begin
        FRawTemp := GetValidatedAnsiStringFromBuffer(Blob.GetBuffer,
            Blob.Length, ConSettings, ConSettings.CTRL_CP);
        Result := StreamFromData(Pointer(FRawTemp), Length(FRawTemp){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF});
      end else
          Result := Blob.GetStream;
  end;
  LastWasNull := (Result = nil);
end;

{**
  Gets the value of a column in the current row as a stream of
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  as a stream of Unicode characters.
  The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large<code>LONGVARCHAR</code>values.  The JDBC driver will
  do any necessary conversion from the database format into Unicode.
  The byte format of the Unicode stream must be Java UTF-8,
  as specified in the Java virtual machine specification.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream in Java UTF-8 byte format; if the value is SQL
    <code>NULL</code>, the value returned is <code>null</code>
}
function TZAbstractResultSet.GetUnicodeStream(ColumnIndex: Integer): TStream;
var
  Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stUnicodeStream);
{$ENDIF}
  Result := nil;
  if not IsNull(ColumnIndex) then
  begin
    Blob := GetBlob(ColumnIndex);
    if Blob <> nil then
      if Blob.IsClob then
        Result := Blob.GetUnicodeStream
      else
        Result := Blob.GetStream;
  end;
  LastWasNull := (Result = nil);
end;

{**
  Gets the value of a column in the current row as a stream of
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a binary stream of
  uninterpreted bytes. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARBINARY</code> values.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream.  Also, a
  stream may return <code>0</code> when the method
  <code>InputStream.available</code>
  is called whether there is data available or not.

  @param columnIndex the first column is 1, the second is 2, ...
  @return a Java input stream that delivers the database column value
    as a stream of uninterpreted bytes;
    if the value is SQL <code>NULL</code>, the value returned is <code>null</code>
}
function TZAbstractResultSet.GetBinaryStream(ColumnIndex: Integer): TStream;
var
  Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBinaryStream);
{$ENDIF}
  Result := nil;
  if not IsNull(ColumnIndex) then
  begin
    Blob := GetBlob(ColumnIndex);
    if Blob <> nil then
      Result := Blob.GetStream;
  end;
  LastWasNull := (Result = nil);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  Result := TZAbstractBlob.CreateWithStream(nil);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>IZResultSet</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>IZResultSet</code> object representing the SQL
    <code>IZResultSet</code> value in the specified column
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // base class - parameter not used intentionally
function TZAbstractResultSet.GetDataSet(ColumnIndex: Integer): IZDataSet;
begin
  Result := nil;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Variant</code> object.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Variant</code> object representing the SQL
    any value in the specified column
}
function TZAbstractResultSet.GetValue(ColumnIndex: Integer): TZVariant;
var
  Metadata: TZAbstractResultSetMetadata;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
{$ENDIF}
  Metadata := TZAbstractResultSetMetadata(FMetadata);
{$IFNDEF DISABLE_CHECKING}
  if (Metadata = nil) or (ColumnIndex < FirstDbcIndex)
    or (ColumnIndex > Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
{$ENDIF}

  case Metadata.GetColumnType(ColumnIndex) of
    stBoolean:
      Result := EncodeBoolean(GetBoolean(ColumnIndex));
    stShort, stSmall, stInteger, stLong:
      Result := EncodeInteger(GetLong(ColumnIndex));
    stByte, stWord, stLongWord, stULong:
      Result := EncodeUInteger(GetULong(ColumnIndex));
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := EncodeFloat(GetBigDecimal(ColumnIndex));
    stDate, stTime, stTimestamp:
      Result := EncodeDateTime(GetTimestamp(ColumnIndex));
    stBytes, stBinaryStream, stGUID:
      Result := EncodeBytes(GetBytes(ColumnIndex));
    stString, stAsciiStream, stUnicodeString, stUnicodeStream:
      {$IFDEF WITH_USC2_ANSICOMPARESTR_ONLY}
      Result := EncodeUnicodeString(GetUnicodeString(ColumnIndex));
      {$ELSE}
      if (not ConSettings^.ClientCodePage^.IsStringFieldCPConsistent) or
         (ConSettings^.ClientCodePage^.Encoding in [ceUTf8, ceUTF16]) then
        Result := EncodeUnicodeString(GetUnicodeString(ColumnIndex))
      else
        Result := EncodeRawByteString(GetRawByteString(ColumnIndex));
      {$ENDIF}
    else
      Result.VType := vtNull;
  end;

  if WasNull then
    Result.VType := vtNull;
end;

{**
  Gets the DefaultExpression value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the DefaultExpression value
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // readonly dataset - parameter not used intentionally
function TZAbstractResultSet.GetDefaultExpression(ColumnIndex: Integer): string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
  Result := '';
end;
{$IFDEF FPC} {$POP} {$ENDIF}

//======================================================================
// Methods for accessing results by column name
//======================================================================

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnName the SQL name of the column
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAbstractResultSet.IsNullByName(const ColumnName: string): Boolean;
begin
  Result := IsNull(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPCharByName(const ColumnName: string): PChar;
begin
  Result := GetPChar(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPAnsiCharByName(const ColumnName: string): PAnsiChar;
begin
  Result := GetPAnsiChar(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnName the SQL name of the column
  @param Len the length in bytes
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPAnsiCharByName(const ColumnName: string;
  out Len: NativeUInt): PAnsiChar;
begin
  Result := GetPAnsiChar(GetColumnIndex(ColumnName), Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnName the SQL name of the column
  @param Len the Length of th UCS2 string in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPWideCharByName(const ColumnName: string;
  out Len: NativeUInt): PWideChar;
begin
  Result := GetPWideChar(GetColumnIndex(ColumnName), Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetPWideCharByName(const ColumnName: string): PWideChar;
begin
  Result := GetPWideChar(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetStringByName(const ColumnName: string): String;
begin
  Result := GetString(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>AnsiString</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_ANSISTRING}
function TZAbstractResultSet.GetAnsiStringByName(const ColumnName: string): AnsiString;
begin
  Result := GetAnsiString(GetColumnIndex(ColumnName));
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
{$IFNDEF NO_UTF8STRING}
function TZAbstractResultSet.GetUTF8StringByName(const ColumnName: string): UTF8String;
begin
  Result := GetUTF8String(GetColumnIndex(ColumnName));
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>RawByteString</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetRawByteStringByName(const ColumnName: string): RawByteString;
begin
  Result := GetRawByteString(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetBinaryStringByName(const ColumnName: string): RawByteString;
begin
  Result := GetBinaryString(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the Object Pascal programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetUnicodeStringByName(const ColumnName: string):
  ZWideString;
begin
  Result := GetUnicodeString(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractResultSet.GetBooleanByName(const ColumnName: string): Boolean;
begin
  Result := GetBoolean(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetByteByName(const ColumnName: string): Byte;
begin
  Result := GetByte(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetShortByName(const ColumnName: string): ShortInt;
begin
  Result := GetSmall(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>word</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetWordByName(const ColumnName: string): Word;
begin
  Result := GetWord(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>small</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetSmallByName(const ColumnName: string): SmallInt;
begin
  Result := GetSmall(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>uint</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetUIntByName(const ColumnName: string): Cardinal;
begin
  Result := GetUInt(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetIntByName(const ColumnName: string): Integer;
begin
  Result := GetInt(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ulong</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetULongByName(const ColumnName: string): UInt64;
begin
  Result := GetULong(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetLongByName(const ColumnName: string): Int64;
begin
  Result := GetLong(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetFloatByName(const ColumnName: string): Single;
begin
  Result := GetFloat(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetDoubleByName(const ColumnName: string): Double;
begin
  Result := GetDouble(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractResultSet.GetCurrencyByName(const ColumnName: string): Currency;
begin
  Result := GetCurrency(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.math.BigDecimal</code> in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetBigDecimalByName(const ColumnName: string): Extended;
begin
  Result := GetBigDecimal(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> array in the Java programming language.
  The bytes represent the raw values returned by the driver.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetBytesByName(const ColumnName: string): TBytes;
begin
  Result := GetBytes(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetDateByName(const ColumnName: string): TDateTime;
begin
  Result := GetDate(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>,
    the value returned is <code>null</code>
}
function TZAbstractResultSet.GetTimeByName(const ColumnName: string): TDateTime;
begin
  Result := GetTime(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Timestamp</code> object.

  @param columnName the SQL name of the column
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractResultSet.GetTimestampByName(const ColumnName: string): TDateTime;
begin
  Result := GetTimestamp(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a stream of
  ASCII characters. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARCHAR</code> values.
  The JDBC driver will
  do any necessary conversion from the database format into ASCII.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream. Also, a
  stream may return <code>0</code> when the method <code>available</code>
  is called whether there is data available or not.

  @param columnName the SQL name of the column
  @return a Java input stream that delivers the database column value
    as a stream of one-byte ASCII characters.
    If the value is SQL <code>NULL</code>,
    the value returned is <code>null</code>.
}
function TZAbstractResultSet.GetAsciiStreamByName(const ColumnName: string): TStream;
begin
  Result := GetAsciiStream(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a stream of
  Unicode characters. The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARCHAR</code> values.
  The JDBC driver will
  do any necessary conversion from the database format into Unicode.
  The byte format of the Unicode stream must be Java UTF-8,
  as defined in the Java virtual machine specification.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream. Also, a
  stream may return <code>0</code> when the method <code>available</code>
  is called whether there is data available or not.

  @param columnName the SQL name of the column
  @return a Java input stream that delivers the database column value
    as a stream of two-byte Unicode characters.
    If the value is SQL <code>NULL</code>, the value returned is <code>null</code>.
}
function TZAbstractResultSet.GetUnicodeStreamByName(const ColumnName: string): TStream;
begin
  Result := GetUnicodeStream(GetColumnIndex(ColumnName));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a stream of uninterpreted
  <code>byte</code>s.
  The value can then be read in chunks from the
  stream. This method is particularly
  suitable for retrieving large <code>LONGVARBINARY</code>
  values.

  <P><B>Note:</B> All the data in the returned stream must be
  read prior to getting the value of any other column. The next
  call to a <code>getXXX</code> method implicitly closes the stream. Also, a
  stream may return <code>0</code> when the method <code>available</code>
  is called whether there is data available or not.

  @param columnName the SQL name of the column
  @return a Java input stream that delivers the database column value
    as a stream of uninterpreted bytes;
    if the value is SQL <code>NULL</code>, the result is <code>null</code>
}
function TZAbstractResultSet.GetBinaryStreamByName(const ColumnName: string): TStream;
begin
  Result := GetBinaryStream(GetColumnIndex(ColumnName));
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param colName the name of the column from which to retrieve the value
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractResultSet.GetBlobByName(const ColumnName: string): IZBlob;
begin
  Result := GetBlob(GetColumnIndex(ColumnName));
end;

function TZAbstractResultSet.GetDataSetByName(const ColumnName: string): IZDataSet;
begin
  Result := GetDataSet(GetColumnIndex(ColumnName));
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Variant</code> object.

  @param colName the name of the column from which to retrieve the value
  @return a <code>Blob</code> object representing the SQL <code>Any</code>
    value in the specified column
}
function TZAbstractResultSet.GetValueByName(const ColumnName: string): TZVariant;
begin
  Result := GetValue(GetColumnIndex(ColumnName));
end;

//=====================================================================
// Advanced features:
//=====================================================================

{**
  Returns the first warning reported by calls on this
  <code>ResultSet</code> object.
  Subsequent warnings on this <code>ResultSet</code> object
  will be chained to the <code>SQLWarning</code> object that
  this method returns.

  <P>The warning chain is automatically cleared each time a new
  row is read.

  <P><B>Note:</B> This warning chain only covers warnings caused
  by <code>ResultSet</code> methods.  Any warning caused by
  <code>Statement</code> methods
  (such as reading OUT parameters) will be chained on the
  <code>Statement</code> object.

  @return the first <code>SQLWarning</code> object reported or <code>null</code>
}
function TZAbstractResultSet.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all warnings reported on this <code>ResultSet</code> object.
  After this method is called, the method <code>getWarnings</code>
  returns <code>null</code> until a new warning is
  reported for this <code>ResultSet</code> object.
}
procedure TZAbstractResultSet.ClearWarnings;
begin
end;

{**
  Gets the name of the SQL cursor used by this <code>ResultSet</code>
  object.

  <P>In SQL, a result table is retrieved through a cursor that is
  named. The current row of a result set can be updated or deleted
  using a positioned update/delete statement that references the
  cursor name. To insure that the cursor has the proper isolation
  level to support update, the cursor's <code>select</code> statement should be
  of the form 'select for update'. If the 'for update' clause is
  omitted, the positioned updates may fail.

  <P>The JDBC API supports this SQL feature by providing the name of the
  SQL cursor used by a <code>ResultSet</code> object.
  The current row of a <code>ResultSet</code> object
  is also the current row of this SQL cursor.

  <P><B>Note:</B> If positioned update is not supported, a
  <code>SQLException</code> is thrown.

  @return the SQL name for this <code>ResultSet</code> object's cursor
}
function TZAbstractResultSet.GetCursorName: String;
begin
  Result := '';
end;

{**
  Retrieves the  number, types and properties of
  this <code>ResultSet</code> object's columns.
  @return the description of this <code>ResultSet</code> object's columns
}
function TZAbstractResultSet.GetMetaData: IZResultSetMetaData;
begin
  Result := TZAbstractResultSetMetadata(FMetadata);
end;

{**
  Maps the given <code>ResultSet</code> column name to its
  <code>ResultSet</code> column index.

  @param columnName the name of the column
  @return the column index of the given column name
}
function TZAbstractResultSet.GetColumnIndex(const ColumnName: string): Integer;
begin
  Result := FindColumn(ColumnName);

  if Result = InvalidDbcIndex then
    raise EZSQLException.Create(Format(SColumnWasNotFound, [ColumnName]));
end;

{**
  Maps the given <code>ResultSet</code> column name to its
  <code>ResultSet</code> column index.

  @param columnName the name of the column
  @return the column index of the given column name
}
function TZAbstractResultSet.FindColumn(const ColumnName: string): Integer;
var
  I: Integer;
  Metadata: TZAbstractResultSetMetadata;
begin
  CheckClosed;
  Metadata := TZAbstractResultSetMetadata(FMetadata);
  Result := InvalidDbcIndex;

  { Search for case sensitive columns. }
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if Metadata.GetColumnLabel(I) = ColumnName then
    begin
      Result := I;
      Exit;
    end;

  { Search for case insensitive columns. }
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if AnsiUpperCase(Metadata.GetColumnLabel(I)) = AnsiUpperCase(ColumnName) then
    begin
      Result := I;
      Exit;
    end;
end;

//---------------------------------------------------------------------
// Traversal/Positioning
//---------------------------------------------------------------------

{**
  Indicates whether the cursor is before the first row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is before the first row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZAbstractResultSet.IsBeforeFirst: Boolean;
begin
  Result := (FRowNo = 0);
end;

{**
  Indicates whether the cursor is after the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is after the last row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZAbstractResultSet.IsAfterLast: Boolean;
begin
  Result := {(FLastRowNo > 0) and} (FRowNo > FLastRowNo);
end;

{**
  Indicates whether the cursor is on the first row of
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on the first row;
    <code>false</code> otherwise
}
function TZAbstractResultSet.IsFirst: Boolean;
begin
  Result := (FRowNo = 1);
end;

{**
  Indicates whether the cursor is on the last row of
  this <code>ResultSet</code> object.
  Note: Calling the method <code>isLast</code> may be expensive
  because the JDBC driver
  might need to fetch ahead one row in order to determine
  whether the current row is the last row in the result set.

  @return <code>true</code> if the cursor is on the last row;
    <code>false</code> otherwise
}
function TZAbstractResultSet.IsLast: Boolean;
begin
  Result := {(FLastRowNo > 0) and} (FRowNo = FLastRowNo);
end;

{**
  Moves the cursor to the front of
  this <code>ResultSet</code> object, just before the
  first row. This method has no effect if the result set contains no rows.
}
procedure TZAbstractResultSet.BeforeClose;
begin
  ResetCursor;
end;

procedure TZAbstractResultSet.BeforeFirst;
begin
  MoveAbsolute(0);
end;

{**
  Moves the cursor to the end of
  this <code>ResultSet</code> object, just after the
  last row. This method has no effect if the result set contains no rows.
}
procedure TZAbstractResultSet.AfterClose;
begin
  FColumnsInfo.Clear;
end;

procedure TZAbstractResultSet.AfterLast;
begin
  Last;
  Next;
end;

{**
  Moves the cursor to the first row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
  <code>false</code> if there are no rows in the result set
}
function TZAbstractResultSet.First: Boolean;
begin
  Result := MoveAbsolute(1);
end;

{**
  Moves the cursor to the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if there are no rows in the result set
}
function TZAbstractResultSet.Last: Boolean;
begin
  Result := MoveAbsolute(FLastRowNo);
end;

{**
  Retrieves the current row number.  The first row is number 1, the
  second number 2, and so on.
  @return the current row number; <code>0</code> if there is no current row
}
function TZAbstractResultSet.GetRow: NativeInt;
begin
  Result := FRowNo;
end;

{**
  Moves the cursor to the given row number in
  this <code>ResultSet</code> object.

  <p>If the row number is positive, the cursor moves to
  the given row number with respect to the
  beginning of the result set.  The first row is row 1, the second
  is row 2, and so on.

  <p>If the given row number is negative, the cursor moves to
  an absolute row position with respect to
  the end of the result set.  For example, calling the method
  <code>absolute(-1)</code> positions the
  cursor on the last row; calling the method <code>absolute(-2)</code>
  moves the cursor to the next-to-last row, and so on.

  <p>An attempt to position the cursor beyond the first/last row in
  the result set leaves the cursor before the first row or after
  the last row.

  <p><B>Note:</B> Calling <code>absolute(1)</code> is the same
  as calling <code>first()</code>. Calling <code>absolute(-1)</code>
  is the same as calling <code>last()</code>.

  @return <code>true</code> if the cursor is on the result set;
    <code>false</code> otherwise
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // base class - parameter not used intentionally
function TZAbstractResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := False;
  RaiseForwardOnlyException;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Moves the cursor a relative number of rows, either positive or negative.
  Attempting to move beyond the first/last row in the
  result set positions the cursor before/after the
  the first/last row. Calling <code>relative(0)</code> is valid, but does
  not change the cursor position.

  <p>Note: Calling the method <code>relative(1)</code>
  is different from calling the method <code>next()</code>
  because is makes sense to call <code>next()</code> when there
  is no current row,
  for example, when the cursor is positioned before the first row
  or after the last row of the result set.

  @return <code>true</code> if the cursor is on a row;
    <code>false</code> otherwise
}
function TZAbstractResultSet.MoveRelative(Rows: Integer): Boolean;
begin
  Result := MoveAbsolute(FRowNo + Rows);
end;

{**
  Moves the cursor to the previous row in this
  <code>ResultSet</code> object.

  <p><B>Note:</B> Calling the method <code>previous()</code> is not the same as
  calling the method <code>relative(-1)</code> because it
  makes sense to call</code>previous()</code> when there is no current row.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if it is off the result set
}
function TZAbstractResultSet.Previous: Boolean;
begin
  Result := MoveAbsolute(FRowNo - 1);
end;

{**
  Moves the cursor down one row from its current position.
  A <code>ResultSet</code> cursor is initially positioned
  before the first row; the first call to the method
  <code>next</code> makes the first row the current row; the
  second call makes the second row the current row, and so on.

  <P>If an input stream is open for the current row, a call
  to the method <code>next</code> will
  implicitly close it. A <code>ResultSet</code> object's
  warning chain is cleared when a new row is read.

  @return <code>true</code> if the new current row is valid;
    <code>false</code> if there are no more rows
}
function TZAbstractResultSet.Next: Boolean;
begin
  Result := MoveAbsolute(FRowNo + 1);
end;

//---------------------------------------------------------------------
// Properties
//---------------------------------------------------------------------

{**
  Returns the fetch direction for this
  <code>ResultSet</code> object.
  @return the current fetch direction for this <code>ResultSet</code> object
}
function TZAbstractResultSet.GetFetchDirection: TZFetchDirection;
begin
  Result := FFetchDirection;
end;

{**
  Gives a hint as to the direction in which the rows in this
  <code>ResultSet</code> object will be processed.
  The initial value is determined by the
  <code>Statement</code> object
  that produced this <code>ResultSet</code> object.
  The fetch direction may be changed at any time.
}
procedure TZAbstractResultSet.SetFetchDirection(Direction: TZFetchDirection);
begin
  if Direction <> fdForward then
    RaiseUnsupportedException;
end;

{**
  Returns the fetch size for this
  <code>ResultSet</code> object.
  @return the current fetch size for this <code>ResultSet</code> object
}
function TZAbstractResultSet.GetFetchSize: Integer;
begin
  Result := FFetchSize;
end;

{**
  Gives the JDBC driver a hint as to the number of rows that should
  be fetched from the database when more rows are needed for this
  <code>ResultSet</code> object.
  If the fetch size specified is zero, the JDBC driver
  ignores the value and is free to make its own best guess as to what
  the fetch size should be.  The default value is set by the
  <code>Statement</code> object
  that created the result set.  The fetch size may be changed at any time.

  @param rows the number of rows to fetch
}
procedure TZAbstractResultSet.SetFetchSize(Rows: Integer);
begin
  FFetchSize := Rows;
end;

{**
  Returns the type of this <code>ResultSet</code> object.
  The type is determined by the <code>Statement</code> object
  that created the result set.

  @return <code>TYPE_FORWARD_ONLY</code>,
    <code>TYPE_SCROLL_INSENSITIVE</code>,
    or <code>TYPE_SCROLL_SENSITIVE</code>
}
function TZAbstractResultSet.GetType: TZResultSetType;
begin
  Result := FResultSetType;
end;

{**
  Returns the concurrency mode of this <code>ResultSet</code> object.
  The concurrency used is determined by the
  <code>Statement</code> object that created the result set.

  @return the concurrency type, either <code>CONCUR_READ_ONLY</code>
    or <code>CONCUR_UPDATABLE</code>
}
function TZAbstractResultSet.GetConcurrency: TZResultSetConcurrency;
begin
  Result := FResultSetConcurrency;
end;

{**
  Gets an assigned post locate mode.
  @param the assigned post locate mode.
}
function TZAbstractResultSet.GetLocateUpdates: TZLocateUpdatesMode;
begin
  Result := FLocateUpdates;
end;

function TZAbstractResultSet.GetPostUpdates: TZPostUpdatesMode;
begin
  Result := FPostUpdates;
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Indicates whether the current row has been updated.  The value returned
  depends on whether or not the result set can detect updates.

  @return <code>true</code> if the row has been visibly updated
    by the owner or another, and updates are detected
}
function TZAbstractResultSet.RowUpdated: Boolean;
begin
  Result := False;
end;

{**
  Indicates whether the current row has had an insertion.
  The value returned depends on whether or not this
  <code>ResultSet</code> object can detect visible inserts.

  @return <code>true</code> if a row has had an insertion
    and insertions are detected; <code>false</code> otherwise
}
function TZAbstractResultSet.RowInserted: Boolean;
begin
  Result := False;
end;

{**
  Indicates whether a row has been deleted.  A deleted row may leave
  a visible "hole" in a result set.  This method can be used to
  detect holes in a result set.  The value returned depends on whether
  or not this <code>ResultSet</code> object can detect deletions.

  @return <code>true</code> if a row was deleted and deletions are detected;
    <code>false</code> otherwise
}
function TZAbstractResultSet.RowDeleted: Boolean;
begin
  Result := False;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // readonly dataset - parameter not used intentionally

{**
  Gives a nullable column a null value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code>
  or <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZAbstractResultSet.UpdateNull(ColumnIndex: Integer);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBoolean(ColumnIndex: Integer; const Value: Boolean);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>byte</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateByte(ColumnIndex: Integer;
  const Value: Byte);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>short</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateShort(ColumnIndex: Integer;
  const Value: ShortInt);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>word</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.


  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateWord(ColumnIndex: Integer;
  const Value: Word);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>small</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateSmall(ColumnIndex: Integer;
  const Value: SmallInt);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with an <code>uint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUInt(ColumnIndex: Integer;
  const Value: Cardinal);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with an <code>int</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateInt(ColumnIndex: Integer;
  const Value: Integer);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>ulong</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateULong(ColumnIndex: Integer;
  const Value: UInt64);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateLong(ColumnIndex: Integer;
  const Value: Int64);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>float</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateFloat(ColumnIndex: Integer;
  const Value: Single);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>double</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDouble(ColumnIndex: Integer;
  const Value: Double);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>currency</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateCurrency(ColumnIndex: Integer;
  const Value: Currency);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>java.math.BigDecimal</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBigDecimal(ColumnIndex: Integer;
  const Value: Extended);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>PChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePChar(ColumnIndex: Integer;
  const Value: PChar);
begin
  {$IFDEF UNICODE}
  UpdatePWideChar(ColumnIndex, Value);
  {$ELSE}
  UpdatePAnsiChar(ColumnIndex, Value);
  {$ENDIF}
end;

{**
  Updates the designated column with a <code>PAnsiChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePAnsiChar(ColumnIndex: Integer;
  Value: PAnsiChar);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>TZAnsiRec</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the pointer to length in bytes
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePAnsiChar(ColumnIndex: Integer;
  Value: PAnsiChar; Len: PNativeUInt);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>TZAnsiRec</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePWideChar(ColumnIndex: Integer;
  Value: PWideChar);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>TZAnsiRec</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len a pointer to length of the string in codepoints
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePWideChar(ColumnIndex: Integer;
  Value: PWideChar; Len: PNativeUInt);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateString(ColumnIndex: Integer;
  const Value: String);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>AnsiString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractResultSet.UpdateAnsiString(ColumnIndex: Integer;
  const Value: AnsiString);
begin
  RaiseReadOnlyException;
end;
{$ENDIF}

{**
  Updates the designated column with a <code>UTF8String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
{$IFNDEF NO_UTF8STRING}
procedure TZAbstractResultSet.UpdateUTF8String(ColumnIndex: Integer;
  const Value: UTF8String);
begin
  RaiseReadOnlyException;
end;
{$ENDIF}

{**
  Updates the designated column with a <code>RawByteString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateRawByteString(ColumnIndex: Integer; const
  Value: RawByteString);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBinaryString(ColumnIndex: Integer;
  const Value: RawByteString);
begin
  case GetMetaData.GetColumnType(ColumnIndex) of
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    stBytes: UpdateBytes(ColumnIndex, Value);
    stBinaryStream: GetBlob(ColumnIndex).SetBytes(Value);
    {$ELSE}
    stBytes: UpdateBytes(ColumnIndex, StrToBytes(Value));
    stBinaryStream: GetBlob(ColumnIndex).SetString(Value);
    {$ENDIF}
    else
      UpdateRawByteString(ColumnIndex, Value);
  end;
end;

{**
  Updates the designated column with a <code>WideString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeString(ColumnIndex: Integer;
  const Value: ZWideString);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>byte</code> array value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBytes(ColumnIndex: Integer;
  const Value: TBytes);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDate(ColumnIndex: Integer;
  const Value: TDateTime);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTime(ColumnIndex: Integer;
  const Value: TDateTime);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimestamp(ColumnIndex: Integer;
  const Value: TDateTime);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with an ascii stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateAsciiStream(ColumnIndex: Integer;
  const Value: TStream);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a binary stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
  @param length the length of the stream
}
procedure TZAbstractResultSet.UpdateBinaryStream(ColumnIndex: Integer;
  const Value: TStream);
begin
  RaiseReadOnlyException;
end;

procedure TZAbstractResultSet.UpdateLob(ColumnIndex: Integer; const Value: IZBlob);
begin
  RaiseReadOnlyException;
end;

procedure TZAbstractResultSet.UpdateDataSet(ColumnIndex: Integer;
  const Value: IZDataSet);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a character stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeStream(ColumnIndex: Integer;
  const Value: TStream);
begin
  RaiseReadOnlyException;
end;

{**
  Updates the designated column with a variant value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateValue(ColumnIndex: Integer;
  const Value: TZVariant);
begin
  case Value.VType of
    vtBoolean: UpdateBoolean(ColumnIndex, Value.VBoolean);
    vtInteger: UpdateLong(ColumnIndex, Value.VInteger);
    vtFloat: UpdateBigDecimal(ColumnIndex, Value.VFloat);
    vtString: UpdateString(ColumnIndex, Value.VString);
{$IFNDEF NO_ANSISTRING}
    vtAnsiString: UpdateAnsiString(ColumnIndex, Value.VAnsiString);
{$ENDIF}
{$IFNDEF NO_UTF8STRING}
    vtUTF8String: UpdateUTF8String(ColumnIndex, Value.VUTF8String);
{$ENDIF}
    vtRawByteString: UpdateRawByteString(ColumnIndex, Value.VRawByteString);
    vtBytes: UpdateBytes(ColumnIndex, Value.VBytes);
    vtDateTime: UpdateTimestamp(ColumnIndex, Value.VDateTime);
    vtUnicodeString: UpdateUnicodeString(ColumnIndex, Value.VUnicodeString);
  else
    UpdateNull(ColumnIndex);
  end;
end;

{**
  Updates the DefaultExpression of the designated column with a <code>String</code> value.
  This changes the behaviour of the RowAccessor used by the Resultset
  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new DefaultExpression value for the column
}
procedure TZAbstractResultSet.UpdateDefaultExpression(ColumnIndex: Integer; const Value: string);
begin
  RaiseReadOnlyException;
end;

{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Updates the designated column with a <code>null</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
}
procedure TZAbstractResultSet.UpdateNullByName(const ColumnName: string);
begin
  UpdateNull(GetColumnIndex(ColumnName));
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBooleanByName(const ColumnName: string;
  const Value: Boolean);
begin
  UpdateBoolean(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>byte</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateByteByName(const ColumnName: string;
  const Value: Byte);
begin
  UpdateByte(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>ShortInt</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateShortByName(const ColumnName: string;
  const Value: ShortInt);
begin
  UpdateShort(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>Word</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateWordByName(const ColumnName: string;
  const Value: Word);
begin
  UpdateWord(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>SmallInt</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateSmallByName(const ColumnName: string;
  const Value: SmallInt);
begin
  UpdateSmall(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with an <code>uint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUIntByName(const ColumnName: string;
  const Value: Cardinal);
begin
  UpdateUInt(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with an <code>int</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateIntByName(const ColumnName: string;
  const Value: Integer);
begin
  UpdateInt(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateULongByName(const ColumnName: string;
  const Value: UInt64);
begin
  UpdateULong(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>long</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateLongByName(const ColumnName: string;
  const Value: Int64);
begin
  UpdateLong(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>float	</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateFloatByName(const ColumnName: string;
  const Value: Single);
begin
  UpdateFloat(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>double</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDoubleByName(const ColumnName: string;
  const Value: Double);
begin
  UpdateDouble(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>currency</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateCurrencyByName(const ColumnName: string;
  const Value: Currency);
begin
  UpdateCurrency(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.BigDecimal</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBigDecimalByName(const ColumnName: string;
  const Value: Extended);
begin
  UpdateBigDecimal(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>PAnsiChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePAnsiCharByName(const ColumnName: string;
  Value: PAnsiChar);
begin
  UpdatePAnsiChar(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>PAnsiChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param Len the pointer to the length in bytes
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePAnsiCharByName(const ColumnName: string;
  Value: PAnsiChar; Len: PNativeUInt);
begin
  UpdatePAnsiChar(GetColumnIndex(ColumnName), Value, Len);
end;

{**
  Updates the designated column with a <code>PChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePCharByName(const ColumnName: string;
  const Value: PChar);
begin
  UpdatePChar(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>PWideChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePWideCharByName(const ColumnName: string;
  Value: PWideChar);
begin
  UpdatePWideChar(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>PWideChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param Len the pointer to the length of the string in codepopints
  @param x the new column value
}
procedure TZAbstractResultSet.UpdatePWideCharByName(const ColumnName: string;
  Value: PWideChar; Len: PNativeUInt);
begin
  UpdatePWideChar(GetColumnIndex(ColumnName), Value, Len);
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateStringByName(const ColumnName: string;
   const Value: String);
begin
  UpdateString(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>AnsiString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractResultSet.UpdateAnsiStringByName(const ColumnName: string;
   const Value: AnsiString);
begin
  UpdateAnsiString(GetColumnIndex(ColumnName), Value);
end;
{$ENDIF}

{**
  Updates the designated column with a <code>UTF8String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
{$IFNDEF NO_UTF8STRING}
procedure TZAbstractResultSet.UpdateUTF8StringByName(const ColumnName: string;
   const Value: UTF8String);
begin
  UpdateUTF8String(GetColumnIndex(ColumnName), Value);
end;
{$ENDIF}

{**
  Updates the designated column with a <code>RawByteString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateRawByteStringByName(const ColumnName: string;
   const Value: RawByteString);
begin
  UpdateRawByteString(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>String</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBinaryStringByName(const ColumnName: string;
   const Value: RawByteString);
begin
  UpdateBinaryString(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>WideString</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeStringByName(const ColumnName: string;
  const Value: ZWideString);
begin
  UpdateUnicodeString(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>boolean</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  JDBC 2.0

  Updates a column with a byte array value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row, or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or <code>insertRow</code>
  methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBytesByName(const ColumnName: string;
  const Value: TBytes);
begin
  UpdateBytes(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.Date</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateDateByName(const ColumnName: string;
  const Value: TDateTime);
begin
  UpdateDate(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.Time</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimeByName(const ColumnName: string;
  const Value: TDateTime);
begin
  UpdateTime(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>java.sql.Timestamp</code>
  value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateTimestampByName(const ColumnName: string;
  const Value: TDateTime);
begin
  UpdateTimestamp(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with an ascii stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateAsciiStreamByName(const ColumnName: string;
  const Value: TStream);
begin
  UpdateAsciiStream(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a binary stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateBinaryStreamByName(const ColumnName: string;
  const Value: TStream);
begin
  UpdateBinaryStream(GetColumnIndex(ColumnName), Value);
end;

procedure TZAbstractResultSet.UpdateDataSetByName(const ColumnName: string;
  const Value: IZDataSet);
begin
  UpdateDataSet(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a character stream value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateUnicodeStreamByName(const ColumnName: string;
  const Value: TStream);
begin
  UpdateUnicodeStream(GetColumnIndex(ColumnName), Value);
end;

{**
  Updates the designated column with a <code>Variant</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnName the name of the column
  @param x the new column value
}
procedure TZAbstractResultSet.UpdateValueByName(const ColumnName: string;
  const Value: TZVariant);
begin
  UpdateValue(GetColumnIndex(ColumnName), Value);
end;

{**
  Inserts the contents of the insert row into this
  <code>ResultSet</code> objaect and into the database.
  The cursor must be on the insert row when this method is called.
}
procedure TZAbstractResultSet.InsertRow;
begin
  RaiseReadOnlyException;
end;

{**
  Updates the underlying database with the new contents of the
  current row of this <code>ResultSet</code> object.
  This method cannot be called when the cursor is on the insert row.
}
procedure TZAbstractResultSet.UpdateRow;
begin
  RaiseReadOnlyException;
end;

{**
  Deletes the current row from this <code>ResultSet</code> object
  and from the underlying database.  This method cannot be called when
  the cursor is on the insert row.
}
procedure TZAbstractResultSet.DeleteRow;
begin
  RaiseReadOnlyException;
end;

{**
  Refreshes the current row with its most recent value in
  the database.  This method cannot be called when
  the cursor is on the insert row.

  <P>The <code>refreshRow</code> method provides a way for an
  application to
  explicitly tell the JDBC driver to refetch a row(s) from the
  database.  An application may want to call <code>refreshRow</code> when
  caching or prefetching is being done by the JDBC driver to
  fetch the latest value of a row from the database.  The JDBC driver
  may actually refresh multiple rows at once if the fetch size is
  greater than one.

  <P> All values are refetched subject to the transaction isolation
  level and cursor sensitivity.  If <code>refreshRow</code> is called after
  calling an <code>updateXXX</code> method, but before calling
  the method <code>updateRow</code>, then the
  updates made to the row are lost.  Calling the method
  <code>refreshRow</code> frequently will likely slow performance.
}
procedure TZAbstractResultSet.RefreshRow;
begin
  RaiseUnsupportedException;
end;

{**
  Cancels the updates made to the current row in this
  <code>ResultSet</code> object.
  This method may be called after calling an
  <code>updateXXX</code> method(s) and before calling
  the method <code>updateRow</code> to roll back
  the updates made to a row.  If no updates have been made or
  <code>updateRow</code> has already been called, this method has no
  effect.
}
procedure TZAbstractResultSet.CancelRowUpdates;
begin
  RaiseReadOnlyException;
end;

{**
  Moves the cursor to the insert row.  The current cursor position is
  remembered while the cursor is positioned on the insert row.

  The insert row is a special row associated with an updatable
  result set.  It is essentially a buffer where a new row may
  be constructed by calling the <code>updateXXX</code> methods prior to
  inserting the row into the result set.

  Only the <code>updateXXX</code>, <code>getXXX</code>,
  and <code>insertRow</code> methods may be
  called when the cursor is on the insert row.  All of the columns in
  a result set must be given a value each time this method is
  called before calling <code>insertRow</code>.
  An <code>updateXXX</code> method must be called before a
  <code>getXXX</code> method can be called on a column value.
}
procedure TZAbstractResultSet.MoveToInsertRow;
begin
  RaiseReadOnlyException;
end;

{**
  Moves the cursor to the remembered cursor position, usually the
  current row.  This method has no effect if the cursor is not on
  the insert row.
}
procedure TZAbstractResultSet.MoveToCurrentRow;
begin
end;

{**
  Compares fields from two row buffers.
  @param Row1 the first row buffer to compare.
  @param Row2 the second row buffer to compare.
  @param ColumnIndices column indices to compare.
  @param ColumnDirs compare direction for each columns.
}
function TZAbstractResultSet.CompareRows(Row1, Row2: NativeInt;
  const ColumnIndices: TIntegerDynArray; const CompareFuncs: TCompareFuncs): Integer;
var
  I: Integer;
  ColumnIndex: Integer;
  SaveRowNo: Integer;
  Value1, Value2: TZVariant;
begin
  Result := 0;
  SaveRowNo := RowNo;
  try
    for I := Low(ColumnIndices) to High(ColumnIndices) do
    begin
      ColumnIndex := ColumnIndices[I];

      MoveAbsolute(Row1);
      Value1 := GetValue(ColumnIndex);
      MoveAbsolute(Row2);
      Value2 := GetValue(ColumnIndex);
      Result := CompareFuncs[i]((Value1.VType = vtNull), (Value2.VType = vtNull), Value1, Value2);
      if Result <> 0 then Break;
    end;
  finally
    MoveAbsolute(SaveRowNo);
  end;
end;

function TZAbstractResultSet.GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
  const CompareKinds: TComparisonKindArray): TCompareFuncs;
var I: Integer;
begin
  SetLength(Result, Length(ColumnIndices));
  for i := low(ColumnIndices) to high(ColumnIndices) do
    case CompareKinds[i] of
      ckAscending:
        case TZAbstractResultSetMetadata(FMetadata).GetColumnType(ColumnIndices[i]) of
          stBoolean:
            Result[i] := CompareBoolean_Asc;
          stShort, stSmall, stInteger, stLong:
            Result[i] := CompareInt64_Asc;
          stByte, stWord, stLongWord, stULong:
            Result[i] := CompareUInt64_Asc;
          stFloat, stDouble, stCurrency, stBigDecimal:
            Result[i] := CompareFloat_Asc;
          stDate, stTime, stTimestamp:
            Result[i] := CompareDateTime_Asc;
          stBytes, stBinaryStream, stGUID:
            Result[i] := CompareBytes_Asc;
          stString, stAsciiStream, stUnicodeString, stUnicodeStream:
            {$IFDEF WITH_USC2_ANSICOMPARESTR_ONLY}
            Result[i] := CompareUnicodeString_Asc;
            {$ELSE}
            if (not ConSettings^.ClientCodePage^.IsStringFieldCPConsistent) or
                (ConSettings^.ClientCodePage^.Encoding in [ceUTf8, ceUTF16]) then
              Result[i] := CompareUnicodeString_Asc
            else
              Result[I] := CompareRawByteString_Asc
            {$ENDIF}
          else
            Result[i] := CompareNothing;
        end;
      ckDescending:
        case TZAbstractResultSetMetadata(FMetadata).GetColumnType(ColumnIndices[i]) of
          stBoolean:
            Result[i] := CompareBoolean_Desc;
          stShort, stSmall, stInteger, stLong:
            Result[i] := CompareInt64_Desc;
          stByte, stWord, stLongWord, stULong:
            Result[i] := CompareUInt64_Desc;
          stFloat, stDouble, stCurrency, stBigDecimal:
            Result[i] := CompareFloat_Desc;
          stDate, stTime, stTimestamp:
            Result[i] := CompareDateTime_Desc;
          stBytes, stBinaryStream, stGUID:
            Result[i] := CompareBytes_Desc;
          stString, stAsciiStream, stUnicodeString, stUnicodeStream:
            {$IFDEF WITH_USC2_ANSICOMPARESTR_ONLY}
            Result[i] := CompareUnicodeString_Desc;
            {$ELSE}
            if (not ConSettings^.ClientCodePage^.IsStringFieldCPConsistent) or
                (ConSettings^.ClientCodePage^.Encoding in [ceUTf8, ceUTF16]) then
              Result[i] := CompareUnicodeString_Desc
            else
              Result[I] := CompareRawByteString_Desc
            {$ENDIF}
          else
            Result[i] := CompareNothing;
        end;
      ckEquals: raise Exception.Create('Compare Equals is not allowed here!');
    end;
end;

{**
  Returns the <code>Statement</code> object that produced this
  <code>ResultSet</code> object.
  If the result set was generated some other way, such as by a
  <code>DatabaseMetaData</code> method, this method returns
  <code>null</code>.

  @return the <code>Statment</code> object that produced
    this <code>ResultSet</code> object or <code>null</code>
    if the result set was produced some other way
}
function TZAbstractResultSet.GetStatement: IZStatement;
begin
  Result := FStatement;
end;

{ TZAbstractBlob }

{**
  Constructs this class and assignes the main properties.
  @param Stream a data string object.
}
constructor TZAbstractBlob.CreateWithStream(Stream: TStream);
begin
  inherited Create;
  FUpdated := False;
  if Assigned(Stream) then
  begin
    FBlobSize := Stream.Size;
    if FBlobSize > 0 then
    begin
      GetMem(FBlobData, FBlobSize);
      Stream.Position := 0;
      Stream.ReadBuffer(FBlobData^, FBlobSize);
    end;
  end
  else
  begin
    FBlobSize := -1;
    FBlobData := nil;
  end;
end;

{**
  Constructs this class and assignes the main properties.
  @param Data a pointer to the blobdata.
  @param Size the size of the blobdata.
}
constructor TZAbstractBlob.CreateWithData(Data: Pointer; Size: Integer);
begin
  inherited Create;
  FBlobData := nil;
  FBlobSize := Size;
  if FBlobSize > 0 then
  begin
    GetMem(FBlobData, FBlobSize);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Data^, FBlobData^, FBlobSize);
  end;
  FUpdated := False;
end;

procedure TZAbstractBlob.InternalClear;
begin
  if Assigned(FBlobData) then
    FreeMem(FBlobData);
  FBlobData := nil;
  FBlobSize := -1;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractBlob.Destroy;
begin
  Clear;
  inherited Destroy;
end;

{**
  Clears the content of this blob.
}
procedure TZAbstractBlob.Clear;
begin
  InternalClear;
  FUpdated := True;
end;

{**
  Clones this blob object.
  @return a clonned blob object.
}
function TZAbstractBlob.Clone(Empty: Boolean = False): IZBlob;
begin
  if Empty then
    Result := TZAbstractBlob.CreateWithData(nil, 0)
  else
    Result := TZAbstractBlob.CreateWithData(FBlobData, FBlobSize);
end;

function TZAbstractBlob.IsClob: Boolean;
begin
  Result := False;
end;

function TZAbstractBlob.GetRawByteString: RawByteString;
begin
  ZSetString(FBlobData, FBlobSize, Result);
end;

{$IFDEF FPC}
  {$PUSH}
  {$WARN 5024 off : Parameter "$1" not used}                 // base class - parameters not used intentionally
  {$WARN 5033 off : Function result does not seem to be set} // base class - result not returned intentionally
{$ENDIF}

procedure TZAbstractBlob.SetRawByteString(Const Value: RawByteString; const CodePage: Word);
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

{$IFNDEF NO_ANSISTRING}
function TZAbstractBlob.GetAnsiString: AnsiString;
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

procedure TZAbstractBlob.SetAnsiString(Const Value: AnsiString);
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZAbstractBlob.GetUTF8String: UTF8String;
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

procedure TZAbstractBlob.SetUTF8String(Const Value: UTF8String);
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;
{$ENDIF}

procedure TZAbstractBlob.SetUnicodeString(const Value: ZWideString);
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

function TZAbstractBlob.GetUnicodeString: ZWideString;
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

procedure TZAbstractBlob.SetStream(const Value: TStream; const CodePage: Word);
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

function TZAbstractBlob.GetRawByteStream: TStream;
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

function TZAbstractBlob.GetAnsiStream: TStream;
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

function TZAbstractBlob.GetUTF8Stream: TStream;
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

function TZAbstractBlob.GetUnicodeStream: TStream;
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

function TZAbstractBlob.GetPAnsiChar(const CodePage: Word): PAnsiChar;
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

procedure TZAbstractBlob.SetPAnsiChar(const Buffer: PAnsiChar; const CodePage: Word; const Len: Cardinal);
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

function TZAbstractBlob.GetPWideChar: PWideChar;
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

procedure TZAbstractBlob.SetPWideChar(const Buffer: PWideChar; const Len: Cardinal);
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;

{$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
procedure TZAbstractBlob.SetBlobData(const Buffer: Pointer; const Len: Cardinal);
begin
  if Buffer <> FBlobData then
    Clear;
  Self.FBlobData := Buffer;
  Self.FBlobSize := Len;
  Self.FUpdated := True;
end;

procedure TZAbstractBlob.SetBlobData(const Buffer: Pointer; const Len: Cardinal;
  Const CodePage: Word);
begin
  raise Exception.Create(Format(cSOperationIsNotAllowed3, ['binary']));
end;
{$ENDIF}

{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Checks if this blob has an empty content.
  @return <code>True</code> if this blob is empty.
}
function TZAbstractBlob.IsEmpty: Boolean;
begin
  Result := FBlobSize < 0;
end;

{**
  Checks if the content of this blob was updated.
  @return <code>True</code> is this blob was updated.
}
function TZAbstractBlob.IsUpdated: Boolean;
begin
  Result := FUpdated;
end;

{**
  Gets the length of the stored data.
  @return the length of the stored data or null if the blob is empty.
}
function TZAbstractBlob.Length: Integer;
begin
  Result := FBlobSize;
end;

{**
  Gets the string from the stored data.
  @return a string which contains the stored data.
}
function TZAbstractBlob.GetString: RawByteString;
begin
  SetLength(Result, FBlobSize);
  {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FBlobData^, Pointer(Result)^, FBlobSize);
end;

{**
  Sets a new string data to this blob content.
  @param Value a new string data.
}
procedure TZAbstractBlob.SetString(const Value: RawByteString);
begin
  Clear;
  if IsClob then begin
    FBlobSize := System.Length(Value)+1;
    GetMem(FBlobData, FBlobSize);
    if FBlobSize > 1 then
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, FBlobData^, FBlobSize);
    AnsiChar((PAnsiChar(FBlobData)+FBlobSize-1)^) := AnsiChar(#0);
  end else begin
    FBlobSize := System.Length(Value);
    if FBlobSize > 0 then begin
      GetMem(FBlobData, FBlobSize);
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, FBlobData^, FBlobSize);
    end;
  end;
  FUpdated := True;
end;

{**
  Gets the byte buffer from the stored data.
  @return a byte buffer which contains the stored data.
}
function TZAbstractBlob.GetBytes: TBytes;
begin
  if not IsEmpty then
  begin
    if (FBlobSize > 0) and Assigned(FBlobData) then begin
      Result := BufferToBytes(FBlobData, FBlobSize)
    end else
      Result := nil;
  end
  else
    Result := nil;
end;

function TZAbstractBlob.GetLengthAddress: PInteger;
begin
  Result := @FBlobSize;
end;

{**
  Sets a new byte buffer to this blob content.
  @param Value a new byte buffer.
}
procedure TZAbstractBlob.SetBytes(const Value: TBytes);
begin
  Clear;
  if Value <> nil then
  begin
    FBlobSize := System.Length(Value);
    if FBlobSize > 0 then
    begin
      GetMem(FBlobData, FBlobSize);
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, FBlobData^, FBlobSize);
    end;
  end;
  FUpdated := True;
end;

{**
  Gets the associated stream object.
  @return an associated or newly created stream object.
}
function TZAbstractBlob.GetStream: TStream;
begin
  if (FBlobSize > 0) and Assigned(FBlobData) then
    Result := StreamFromData(FBlobData, FBlobSize)
  else
    Result := TMemoryStream.Create;
end;

{**
  Sets a data from the specified stream into this blob.
  @param Value a stream object to be stored into this blob.
}
procedure TZAbstractBlob.SetStream(const Value: TStream);
begin
  Clear;
  if Assigned(Value) then
  begin
    FBlobSize := Value.Size;
    if FBlobSize > 0 then
    begin
      GetMem(FBlobData, FBlobSize);
      Value.Position := 0;
      Value.ReadBuffer(FBlobData^, FBlobSize);
    end;
  end
  else
  begin
    FBlobSize := -1;
    FBlobData := nil;
  end;
  FUpdated := True;
end;

function TZAbstractBlob.GetBuffer: Pointer;
begin
  Result := FBlobData;
end;

function TZAbstractBlob.GetBufferAddress: PPointer;
begin
  Result := @FBlobData;
end;

procedure TZAbstractBlob.SetBuffer(const Buffer: Pointer; const Length: Integer);
begin
  InternalClear;
  FBlobSize := Length;
  if Assigned(Buffer) and ( Length > 0 ) then
  begin
    GetMem(FBlobData, Length);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, FBlobData^, Length);
  end;
  FUpdated := True;
end;

{ TZAbstractUnCachedBlob }

procedure TZAbstractUnCachedBlob.ReadLob;
begin
  FLoaded := True;
end;

procedure TZAbstractUnCachedBlob.WriteLob;
begin
  //do nothing here, just a placeholder
end;

{**
  Checks if this blob has an empty content.
  @return <code>True</code> if this blob is empty.
}
function TZAbstractUnCachedBlob.IsEmpty: Boolean;
begin
  if not FLoaded then ReadLob;
  Result := inherited IsEmpty;
end;

function TZAbstractUnCachedBlob.Length: Integer;
begin
  if not FLoaded then ReadLob;
  Result := inherited Length;
end;

{**
  Gets the string from the stored data.
  @return a RawByteString which contains the stored data.
}
function TZAbstractUnCachedBlob.GetString: RawByteString;
begin
  if not FLoaded then ReadLob;
  Result := inherited GetString;
end;

{**
  Gets the byte buffer from the stored data.
  @return a byte buffer which contains the stored data.
}
function TZAbstractUnCachedBlob.GetBytes: TBytes;
begin
  if not FLoaded then ReadLob;
  Result := inherited GetBytes;
end;

function TZAbstractUnCachedBlob.GetStream: TStream;
begin
  if not FLoaded then ReadLob;
  Result := inherited GetStream;
end;

function TZAbstractUnCachedBlob.GetBuffer: Pointer;
begin
  if not FLoaded then ReadLob;
  Result := inherited Getbuffer;
end;

function TZAbstractUnCachedBlob.Clone(Empty: Boolean = False): IZBlob;
begin
  if not Empty and not Floaded then
  begin
    ReadLob;
    Result := inherited Clone(Empty);
    FlushBuffer;
  end
  else
    Result := inherited Clone(Empty);
end;

procedure TZAbstractUnCachedBlob.FlushBuffer;
begin
  if not FUpdated then
  begin
    InternalClear;
    Floaded := False;
  end;
end;

{ TZAbstractCLob }

procedure TZAbstractCLob.InternalSetRawByteString(Const Value: RawByteString;
  const CodePage: Word);
begin
  FBlobSize := System.Length(Value)+1;
  FCurrentCodePage := CodePage;
  ReallocMem(FBlobData, FBlobSize);
  if fBlobSize = 1
  then PByte(FBlobData)^ := 0
  else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, FBlobData^, FBlobSize);
end;

{$IFNDEF NO_ANSISTRING}
procedure TZAbstractCLob.InternalSetAnsiString(Const Value: AnsiString);
begin
  FBlobSize := System.Length(Value)+1;
  FCurrentCodePage := ZOSCodePage;
  ReallocMem(FBlobData, FBlobSize);
  if fBlobSize = 1
  then PByte(FBlobData)^ := 0
  else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, FBlobData^, FBlobSize);
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
procedure TZAbstractCLob.InternalSetUTF8String(Const Value: UTF8String);
begin
  FBlobSize := System.Length(Value)+1;
  FCurrentCodePage := zCP_UTF8;
  ReallocMem(FBlobData, FBlobSize);
  if fBlobSize = 1
  then PByte(FBlobData)^ := 0
  else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, FBlobData^, FBlobSize);
end;
{$ENDIF}

procedure TZAbstractCLob.InternalSetUnicodeString(const Value: ZWideString);
begin
  FBlobSize := (System.Length(Value)+1) shl 1;
  FCurrentCodePage := zCP_UTF16;
  ReallocMem(FBlobData, FBlobSize);
  if fBlobSize = 2
  then PWord(FBlobData)^ := 0
  else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, FBlobData^, FBlobSize);
end;

procedure TZAbstractCLob.InternalSetPAnsiChar(const Buffer: PAnsiChar; CodePage: Word; const Len: Cardinal);
var RawTemp: RawByteString;
label SetData;
begin
  InternalClear;
  if Buffer <> nil then
  begin
    if CodePage = zCP_NONE then
    begin
      if Len mod 2 = 0 then //could be UTF16
      begin
        RawTemp := GetValidatedAnsiStringFromBuffer(Buffer, Len, FConSettings);
        InternalSetRawByteString(RawTemp, FConSettings^.ClientCodePage^.CP);
      end
      else //can't be UCS2
      begin
        case ZEncoding.ZDetectUTF8Encoding(Buffer, Len) of
          etUSASCII: CodePage := FConSettings^.ClientCodePage^.CP;
          etUTF8: CodePage := zCP_UTF8;
          else
            if zCompatibleCodePages(FConSettings^.ClientCodePage^.CP, zCP_UTF8) then
              if ZCompatibleCodePages(FConSettings^.CTRL_CP, zCP_UTF8) then
                CodePage := ZOSCodePage
              else
                CodePage := FConSettings^.CTRL_CP
            else
              CodePage := FConSettings^.ClientCodePage^.CP;
        end;
        goto SetData;
      end;
    end
    else
    begin
SetData:
      FBlobSize := Len +1;
      FCurrentCodePage := CodePage;
      GetMem(FBlobData, FBlobSize);
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, FBlobData^, FBlobSize-1);
      PByte((PAnsiChar(FBlobData)+Len))^ := Ord(#0); //set leading terminator
    end;
  end;
end;

procedure TZAbstractCLob.InternalSetPWideChar(const Buffer: PWideChar; const Len: Cardinal);
begin
  if Buffer = nil then
    Clear
  else begin
    FBlobSize := (Len +1) shl 1; //shl 1 = * 2 but faster
    FCurrentCodePage := zCP_UTF16;
    ReallocMem(FBlobData, FBlobSize);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, FBlobData^, FBlobSize-2);
    PWord((PWideChar(FBlobData)+Len))^ := Ord(#0); //set leading terminator
  end;
end;

constructor TZAbstractCLob.CreateWithStream(Stream: TStream; const CodePage: Word;
  const ConSettings: PZConSettings);
begin
  inherited Create;
  FBlobData := nil;
  FCurrentCodePage := CodePage;
  FConSettings := ConSettings;
  if Stream = nil then
    FBlobSize := -1
  else
    if (CodePage = zCP_UTF16) or (CodePage = zCP_UTF16BE) then
      InternalSetPWidechar(TMemoryStream(Stream).Memory, Stream.Size shr 1)
    else
      InternalSetPAnsiChar(TMemoryStream(Stream).Memory, CodePage, Stream.Size);
  FUpdated := False;
end;

constructor TZAbstractCLob.CreateWithData(Data: PAnsiChar; const Len: Cardinal;
  const CodePage: Word; const ConSettings: PZConSettings);
begin
  inherited Create;
  FBlobData := nil;
  FCurrentCodePage := CodePage;
  FConSettings := ConSettings;
  if Data = nil then
    FBlobSize := -1
  else
    InternalSetPAnsiChar(Data, CodePage, Len);
  FUpdated := False;
end;

constructor TZAbstractCLob.CreateWithData(Data: PWideChar; const Len: Cardinal;
  const ConSettings: PZConSettings);
begin
  inherited Create;
  FBlobData := nil;
  FBlobSize := Len;
  FCurrentCodePage := zCP_UTF16;
  FConSettings := ConSettings;
  if Data <> nil then
  begin
    FBlobSize := (Len+1) shl 1; //shl 1 = * 2 but faster, include #0#0 terminator
    GetMem(FBlobData, FBlobSize);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Data^, FBlobData^, FBlobSize);
    PWord((PWideChar(FBlobData)+Len))^ := Ord(#0);
  end
  else
    FBlobSize := 0;
  FUpdated := False;
end;

function TZAbstractCLob.Length: Integer;
begin
  if FBlobSize < 1 then
    Result := 0
  else
    if ( FCurrentCodePage = zCP_UTF16 ) or
       ( FCurrentCodePage = zCP_UTF16BE ) then
      Result := FBlobSize -2
    else
      Result := FBlobSize -1;

end;

function TZAbstractCLob.GetString: RawByteString;
begin
  Result := GetRawByteString;
end;

{**
  Gets the string from the stored data.
  @return a RawByteString which contains the stored data - client encoded.
}
function TZAbstractCLob.GetRawByteString: RawByteString;
var
  WS: ZWideString; //possible WideString which is COM based -> localize it
begin
  Result := EmptyRaw;
  if FBlobSize > 0 then
    if ZCompatibleCodePages(FCurrentCodePage, FConSettings^.ClientCodePage^.CP) then
      ZSetString(FBlobData, FBlobSize-1, Result)
    else
    begin
      if ( FCurrentCodePage = zCP_UTF16 ) or
         ( FCurrentCodePage = zCP_UTF16BE ) then
        Result := PUnicodeToRaw(FBlobData, (FBlobSize shr 1) -1, FConSettings^.ClientCodePage^.CP)
      else
      begin
        WS := PRawToUnicode(FBlobData, FBlobSize-1, FCurrentCodePage);
        Result := ZUnicodeToRaw(WS, FConSettings^.ClientCodePage^.CP);
      end;
      InternalSetRawByteString(Result, FConSettings^.ClientCodePage^.CP);
    end;
end;

procedure TZAbstractCLob.SetRawByteString(Const Value: RawByteString; const CodePage: Word);
begin
  InternalSetRawByteString(Value, CodePage);
  FUpdated := True;
end;

{$IFNDEF NO_ANSISTRING}
function TZAbstractCLob.GetAnsiString: AnsiString;
var
  UniTemp: ZWideString;
begin
  Result := '';
  if FBlobSize > 0 then
    if ZCompatibleCodePages(FCurrentCodePage, ZOSCodePage) then
       System.SetString(Result, PAnsiChar(FBlobData), FBlobSize -1)
    else
    begin
      if ( FCurrentCodePage = zCP_UTF16 ) or
         ( FCurrentCodePage = zCP_UTF16BE ) then
        System.SetString(UniTemp, PWidechar(FBlobData), (FBlobSize shr 1) -1)
      else
        UniTemp := PRawToUnicode(FBlobData, FBlobSize-1, FCurrentCodePage); //localize possible COM based WideString to prevent overflow
      Result := ZUnicodeToRaw(UniTemp, ZOSCodePage);
      InternalSetAnsiString(Result);
    end;
end;
{$ENDIF}

{$IFNDEF NO_ANSISTRING}
procedure TZAbstractCLob.SetAnsiString(Const Value: AnsiString);
begin
  InternalSetAnsiString(Value);
  FUpdated := True;
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZAbstractCLob.GetUTF8String: UTF8String;
var
  Uni: ZWideString;
begin
  Result := '';
  if FBlobSize > 0 then
    if ZCompatibleCodePages(FCurrentCodePage, zCP_UTF8) then
      ZSetString(PAnsiChar(FBlobData), FBlobSize -1, Result)
    else begin
      if ( FCurrentCodePage = zCP_UTF16 ) or
         ( FCurrentCodePage = zCP_UTF16BE ) then
      begin
        System.SetString(Uni, PWidechar(FBlobData), (FBlobSize shr 1) -1);
        {$IFDEF WITH_RAWBYTESTRING}
        Result := UTF8String(Uni)
        {$ELSE}
        Result := UTF8Encode(Uni)
        {$ENDIF}
      end
      else
      begin
        Uni := PRawToUnicode(FBlobData, FBlobSize-1, FCurrentCodePage);
        {$IFDEF WITH_RAWBYTESTRING}
        Result := UTF8String(Uni);
        {$ELSE}
        Result := UTF8Encode(Uni);
        {$ENDIF}
      end;
      InternalSetUTF8String(Result);
    end;
end;
{$ENDIF}

procedure TZAbstractCLob.SetUnicodeString(Const Value: ZWideString);
begin
  InternalSetUnicodeString(Value);
  FUpdated := True;
end;

function TZAbstractCLob.GetUnicodeString: ZWideString;
begin
  Result := '';
  if FBlobSize > 0 then
    if (FCurrentCodePage = zCP_UTF16) or
       (FCurrentCodePage = zCP_UTF16BE) then
    begin
      SetLength(Result, (FBlobSize shr 1) -1);
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(FBlobData^, PWideChar(Result)^, FBlobSize - 2);
    end
    else
    begin
      Result := PRawToUnicode(FBlobData, FBlobSize -1, FCurrentCodePage);
      InternalSetUnicodeString(Result);
    end;
end;

{$IFNDEF NO_UTF8STRING}
procedure TZAbstractCLob.SetUTF8String(Const Value: UTF8String);
begin
  InternalSetUTF8String(Value);
  FUpdated := True;
end;
{$ENDIF}

{**
  Gets the associated stream object.
  @return an associated or newly created stream object.
}
function TZAbstractCLob.GetStream: TStream;
begin
  if (FBlobSize > 0) and Assigned(FBlobData) then
  begin
    if FConSettings^.AutoEncode then
      GetPAnsiChar(FConSettings^.CTRL_CP)
    else
      GetPAnsiChar(FConSettings^.ClientCodePage^.CP);
    Result := StreamFromData(FBlobData, Length);
  end
  else
    Result := TMemoryStream.Create;
end;

procedure TZAbstractCLob.SetStream(const Value: TStream);
begin
  SetStream(Value, zCP_NONE); //because we don't know the codepage here
end;

procedure TZAbstractCLob.SetStream(const Value: TStream; const CodePage: Word);
begin
  if Value = nil then
    InternalClear
  else
  begin
    if (CodePage = zCP_UTF16) or (CodePage = zCP_UTF16BE) then
      SetPWideChar(TMemoryStream(Value).Memory, Value.Size shr 1)
    else
      SetPAnsiChar(TMemoryStream(Value).Memory, CodePage, Value.Size)
  end;
  FUpdated := True;
end;

function TZAbstractCLob.GetRawByteStream: TStream;
var Tmp: RawByteString;
begin
  if (FBlobSize > 0) and Assigned(FBlobData) then
  begin
    if ZCompatibleCodePages(FCurrentCodePage, FConSettings^.ClientCodePage^.CP) then
      Result := StreamFromData(FBlobData, FBlobSize-1)
    else
    begin
      Tmp := GetRawByteString;
      Result := StreamFromData(Pointer(Tmp), Length);
    end;
  end
  else
    Result := TMemoryStream.Create;
end;

{$IFNDEF NO_ANSISTRING}
function TZAbstractCLob.GetAnsiStream: TStream;
begin
  if (FBlobSize > 0) and Assigned(FBlobData) then
  begin
    if ZCompatibleCodePages(FCurrentCodePage, ZOSCodePage) then
      Result := StreamFromData(FBlobData, Length)
    else
    begin
      GetAnsiString; //does the required conversion
      Result := StreamFromData(FBlobData, Length);
    end;
  end
  else
    Result := TMemoryStream.Create;
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZAbstractCLob.GetUTF8Stream: TStream;
begin
  if (FBlobSize > 0) and Assigned(FBlobData) then
  begin
    if ZCompatibleCodePages(FCurrentCodePage, zCP_UTF8) then
      Result := StreamFromData(FBlobData, FBlobSize-1)
    else
    begin
      GetUTF8String; //does the required conversion
      Result := StreamFromData(FBlobData, Length);
    end;
  end
  else
    Result := TMemoryStream.Create;
end;
{$ENDIF}

function TZAbstractCLob.GetUnicodeStream: TStream;
begin
  if (FBlobSize > 0) and Assigned(FBlobData) then
  begin
    if (FCurrentCodePage = zCP_UTF16) or
       (FCurrentCodePage = zCP_UTF16) then
      Result := StreamFromData(FBlobData, FBlobSize-2)
    else
    begin
      GetUnicodeString;
      Result := StreamFromData(FBlobData, FBlobSize-2)
    end;
  end
  else
    Result := TMemoryStream.Create;
end;

function TZAbstractCLob.GetPAnsiChar(const CodePage: Word): PAnsiChar;
var
  TempRaw: RawByteString;
  WS: ZWideString;
begin
  if FBlobData = nil then
    Result := nil
  else
    if ZCompatibleCodePages(FCurrentCodePage, CodePage) then
      Result := FBlobData
    else
    begin
      if (FCurrentCodePage = zCP_UTF16) or
         (FCurrentCodePage = zCP_UTF16BE) then
        TempRaw := PUnicodeToRaw(FBlobData, (FBlobSize shr 1) -1, CodePage)
      else
      begin
        WS := PRawToUnicode(FBlobData, FBlobSize -1, FCurrentCodePage);
        TempRaw := ZUniCodeToRaw(WS, CodePage);
      end;
      InternalSetRawByteString(TempRaw, CodePage);
      Result := PAnsiChar(FBlobData);
    end;
end;

procedure TZAbstractCLob.SetPAnsiChar(const Buffer: PAnsiChar;
  const CodePage: Word; const Len: Cardinal);
begin
  InternalSetPAnsiChar(Buffer, CodePage, Len);
  FUpdated := True;
end;

function TZAbstractCLob.GetPWideChar: PWideChar;
begin
  if FBlobData = nil then
    Result := nil
  else
    if (FCurrentCodePage = zCP_UTF16) or
       (FCurrentCodePage = zCP_UTF16BE) then
      Result := PWideChar(FBlobData)
    else
    begin
      FBlobSize := (PRaw2PUnicodeBuf(FBlobData, Length, 0, FBlobData, FCurrentCodePage)+1) shl 1 ;
      FCurrentCodePage := zCP_UTF16;
      Result := PWideChar(FBlobData);
    end;
end;

procedure TZAbstractCLob.SetPWideChar(const Buffer: PWideChar; const Len: Cardinal);
begin
  InternalSetPWideChar(Buffer, Len);
  FUpdated := True;
end;

{$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
procedure TZAbstractCLob.SetBlobData(const Buffer: Pointer; const Len: Cardinal;
  const CodePage: Word);
begin
  if Buffer <> FBlobData then
    InternalClear;
  FBlobData := Buffer;
  FBlobSize := Len;
  FCurrentCodePage := CodePage;
  FUpdated := True;
end;
{$ENDIF}

{**
  Clones this blob object.
  @return a clonned blob object.
}
function TZAbstractCLob.Clone(Empty: Boolean = False): IZBLob;
begin
  if (FCurrentCodePage = zCP_UTF16) or
     (FCurrentCodePage = zCP_UTF16BE) then
    if Empty or not Assigned(FBlobData) or (FBlobSize <= 1) then
      Result := TZAbstractCLob.CreateWithData(nil, 0, FConSettings)
    else
      Result := TZAbstractCLob.CreateWithData(FBlobData, (FBlobSize shr 1)-1, FConSettings)
  else
    if Empty or not Assigned(FBlobData) or (FBlobSize <= 0) then
      Result := TZAbstractCLob.CreateWithData(nil, 0, FCurrentCodePage, FConSettings)
    else
      Result := TZAbstractCLob.CreateWithData(FBlobData, FBlobSize-1, FCurrentCodePage, FConSettings);
end;

function TZAbstractCLob.IsClob: Boolean;
begin
  Result := True;
end;

{ TZAbstractUnCachedCLob }

procedure TZAbstractUnCachedCLob.ReadLob;
begin
  FLoaded := True;
end;

procedure TZAbstractUnCachedCLob.WriteLob;
begin
end;

function TZAbstractUnCachedCLob.Length: Integer;
begin
  if not Loaded then ReadLob;
  Result := inherited Length;
end;

{**
  Checks if this blob has an empty content.
  @return <code>True</code> if this blob is empty.
}
function TZAbstractUnCachedCLob.IsEmpty: Boolean;
begin
  if not Loaded then ReadLob;
  Result := inherited IsEmpty;
end;

{**
  Gets the string from the stored data.
  @return a RawByteString which contains the stored data - client encoded.
}
function TZAbstractUnCachedCLob.GetRawByteString: RawByteString;
begin
  if not Loaded then ReadLob;
  Result := inherited GetRawByteString;
end;

{$IFNDEF NO_ANSISTRING}
function TZAbstractUnCachedCLob.GetAnsiString: AnsiString;
begin
  if not Loaded then ReadLob;
  Result := inherited GetAnsiString;
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZAbstractUnCachedCLob.GetUTF8String: UTF8String;
begin
  if not Loaded then ReadLob;
  Result := inherited GetUTF8String;
end;
{$ENDIF}

function TZAbstractUnCachedCLob.GetUnicodeString: ZWideString;
begin
  if not Loaded then ReadLob;
  Result := inherited GetUnicodeString;
end;

{**
  Gets the associated stream object.
  @return an associated or newly created stream object.
}
function TZAbstractUnCachedCLob.GetStream: TStream;
begin
  if not Loaded then ReadLob;
  Result := inherited GetStream;
end;

function TZAbstractUnCachedCLob.GetRawByteStream: TStream;
begin
  if not Loaded then ReadLob;
  Result := inherited GetRawByteStream;
end;

function TZAbstractUnCachedCLob.GetAnsiStream: TStream;
begin
  if not Loaded then ReadLob;
  Result := inherited GetAnsiStream;
end;

function TZAbstractUnCachedCLob.GetUTF8Stream: TStream;
begin
  if not Loaded then ReadLob;
  Result := inherited GetUTF8Stream;
end;

function TZAbstractUnCachedCLob.GetUnicodeStream: TStream;
begin
  if not Loaded then ReadLob;
  Result := inherited GetUnicodeStream;
end;

function TZAbstractUnCachedCLob.GetPAnsiChar(const CodePage: Word): PAnsiChar;
begin
  if not Loaded then ReadLob;
  Result := inherited GetPAnsiChar(CodePage);
end;

function TZAbstractUnCachedCLob.GetPWideChar: PWideChar;
begin
  if not Loaded then ReadLob;
  Result := inherited GetPWideChar;
end;

function TZAbstractUnCachedCLob.GetBuffer: Pointer;
begin
  if not Loaded then ReadLob;
  Result := inherited GetBuffer;
end;

{**
  Clones this blob object.
  @return a clonned blob object.
}
function TZAbstractUnCachedCLob.Clone(Empty: Boolean = False): IZBLob;
begin
  if not Empty and not Loaded then
  begin
    ReadLob;
    Result := inherited Clone(Empty);
    FlushBuffer;
  end
  else
    Result := inherited Clone(Empty);
end;

procedure TZAbstractUnCachedCLob.FlushBuffer;
begin
  if not FUpdated then
  begin
    InternalClear;
    FLoaded := False;
  end;
end;

end.
