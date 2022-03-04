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

unit ZDbcStatement;

interface

{$I ZDbc.inc}
{$Z-}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZDbcIntfs, ZTokenizer, ZCompatibility, ZVariant, ZDbcLogging, ZClasses,
  ZDbcUtils;

type
  TZSQLTypeArray = array of TZSQLType;

  {** Implements Abstract Generic SQL Statement. }

  { TZAbstractStatement }

  TZAbstractStatement = class(TZCodePagedObject, IZStatement, IZLoggingObject)
  private
    fABufferIndex, fWBufferIndex: Integer;
    FMaxFieldSize: Integer;
    FMaxRows: Integer;
    FEscapeProcessing: Boolean;
    FQueryTimeout: Integer;
    FLastUpdateCount: Integer;
    FFetchDirection: TZFetchDirection;
    FFetchSize: Integer;
    FResultSetConcurrency: TZResultSetConcurrency;
    FResultSetType: TZResultSetType;
    FPostUpdates: TZPostUpdatesMode;
    FLocateUpdates: TZLocateUpdatesMode;
    FBatchQueries: TStrings;
    FConnection: IZConnection;
    FInfo: TStrings;
    FChunkSize: Integer; //size of buffer chunks for large lob's related to network settings
    FClosed: Boolean;
    FCachedLob: Boolean;
    procedure SetLastResultSet(const ResultSet: IZResultSet);
  protected
    FLastResultSet: IZResultSet;
    FCursorName: RawByteString;
    fWBuffer: array[Byte] of WideChar;
    fABuffer: array[Byte] of AnsiChar;
    FWSQL: ZWideString;
    FaSQL: RawByteString;
    FStatementId : Integer;
    FOpenResultSet: Pointer; //weak reference to avoid memory-leaks and cursor issues
    procedure ToBuff(const Value: ZWideString; var Result: ZWideString); overload;
    procedure ToBuff(const Value: RawByteString; var Result: RawByteString); overload;
    procedure FlushBuff(var Result: ZWideString); overload;
    procedure FlushBuff(var Result: RawByteString); overload;
    procedure PrepareOpenResultSetForReUse; virtual;
    procedure PrepareLastResultSetForReUse; virtual;
    procedure FreeOpenResultSetReference(const ResultSet: IZResultSet);
    procedure SetASQL(const Value: RawByteString); virtual;
    procedure SetWSQL(const Value: ZWideString); virtual;
    class function GetNextStatementId : integer;
    procedure RaiseUnsupportedException;
    procedure ReleaseConnection; virtual;
    property MaxFieldSize: Integer read FMaxFieldSize write FMaxFieldSize;
    property MaxRows: Integer read FMaxRows write FMaxRows;
    property EscapeProcessing: Boolean
      read FEscapeProcessing write FEscapeProcessing;
    property QueryTimeout: Integer read FQueryTimeout write FQueryTimeout;
    property LastUpdateCount: Integer
      read FLastUpdateCount write FLastUpdateCount;
    property LastResultSet: IZResultSet
      read FLastResultSet write SetLastResultSet;
    property FetchDirection: TZFetchDirection
      read FFetchDirection write FFetchDirection;
    property FetchSize: Integer read FFetchSize write FFetchSize;
    property ResultSetConcurrency: TZResultSetConcurrency
      read FResultSetConcurrency write FResultSetConcurrency;
    property ResultSetType: TZResultSetType
      read FResultSetType write FResultSetType;
    property CursorName: RawByteString read FCursorName write FCursorName;
    property BatchQueries: TStrings read FBatchQueries;
    property Connection: IZConnection read FConnection;
    property Info: TStrings read FInfo;
    property Closed: Boolean read FClosed write FClosed;

    property SQL: String read {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
    property WSQL: ZWideString read FWSQL write SetWSQL;
    property ASQL: RawByteString read FaSQL write SetASQL;
    property ChunkSize: Integer read FChunkSize;
    property CachedLob: Boolean read FCachedLob;
    function CreateStmtLogEvent(Category: TZLoggingCategory;
      const Msg: RawByteString=EmptyRaw): TZLoggingEvent;
  public
    constructor Create(const Connection: IZConnection; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; overload; virtual;
    function ExecuteUpdate(const SQL: ZWideString): Integer; overload; virtual;
    function Execute(const SQL: ZWideString): Boolean; overload; virtual;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; overload; virtual;
    function ExecuteUpdate(const SQL: RawByteString): Integer; overload; virtual;
    function Execute(const SQL: RawByteString): Boolean; overload; virtual;

    function GetSQL : String;

    procedure BeforeClose; virtual;
    procedure Close;
    procedure AfterClose; virtual;

    function GetMaxFieldSize: Integer; virtual;
    procedure SetMaxFieldSize(Value: Integer); virtual;
    function GetMaxRows: Integer; virtual;
    procedure SetMaxRows(Value: Integer); virtual;
    procedure SetEscapeProcessing(Value: Boolean); virtual;
    function GetQueryTimeout: Integer; virtual;
    procedure SetQueryTimeout(Value: Integer); virtual;
    procedure Cancel; virtual;
    procedure SetCursorName(const Value: String); virtual;

    function GetResultSet: IZResultSet; virtual;
    function GetUpdateCount: Integer; virtual;
    function GetMoreResults: Boolean; virtual;

    procedure SetFetchDirection(Value: TZFetchDirection); virtual;
    function GetFetchDirection: TZFetchDirection; virtual;
    procedure SetFetchSize(Value: Integer); virtual;
    function GetFetchSize: Integer; virtual;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency); virtual;
    function GetResultSetConcurrency: TZResultSetConcurrency; virtual;
    procedure SetResultSetType(Value: TZResultSetType); virtual;
    function GetResultSetType: TZResultSetType; virtual;

    procedure SetPostUpdates(Value: TZPostUpdatesMode);
    function GetPostUpdates: TZPostUpdatesMode;
    procedure SetLocateUpdates(Value: TZLocateUpdatesMode);
    function GetLocateUpdates: TZLocateUpdatesMode;

    procedure AddBatch(const SQL: string); virtual;
    procedure AddBatchRequest(const SQL: string); virtual;
    procedure ClearBatch; virtual;
    function ExecuteBatch: TIntegerDynArray; virtual;

    function GetConnection: IZConnection;
    function GetParameters: TStrings;
    function GetChunkSize: Integer;

    function GetWarnings: EZSQLWarning; virtual;
    procedure ClearWarnings; virtual;
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; virtual;
    function GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString; virtual;
    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent; virtual;
  end;

  {** Implements Abstract Prepared SQL Statement. }

  { TZAbstractPreparedStatement }

  TZAbstractPreparedStatement = class(TZAbstractStatement, IZPreparedStatement)
  private
    FInParamValues: TZVariantDynArray;
    FInParamTypes: TZSQLTypeArray;
    FInParamDefaultValues: TStringDynArray;
    FInitialArrayCount: ArrayLenInt;
    FPrepared : Boolean;
    FClientVariantManger: IZClientVariantManager;
  protected
    FInParamCount: Integer;
    FCachedQueryRaw: TRawByteStringDynArray;
    FCachedQueryUni: TUnicodeStringDynArray;
    FNCharDetected: TBooleanDynArray;
    FIsParamIndex: TBooleanDynArray;
    FIsPraparable: Boolean;
    function GetClientVariantManger: IZClientVariantManager;
    procedure PrepareInParameters; virtual;
    procedure BindInParameters; virtual;
    procedure UnPrepareInParameters; virtual;

    procedure SetInParamCount(const NewParamCount: Integer); virtual;
    procedure SetInParam(ParameterIndex: Integer; SQLType: TZSQLType;
      const Value: TZVariant); virtual;
    procedure LogPrepStmtMessage(Category: TZLoggingCategory; const Msg: RawByteString = EmptyRaw);
    function GetInParamLogValue(Value: TZVariant): RawByteString;
    function GetOmitComments: Boolean; virtual;
    function GetCompareFirstKeywordStrings: TPreparablePrefixTokens; virtual;

    property InParamValues: TZVariantDynArray read FInParamValues write FInParamValues;
    property InParamTypes: TZSQLTypeArray read FInParamTypes write FInParamTypes;
    property InParamDefaultValues: TStringDynArray
      read FInParamDefaultValues write FInParamDefaultValues;
    property InParamCount: Integer read FInParamCount write FInParamCount;
    property ClientVarManager: IZClientVariantManager read FClientVariantManger;
    property CachedQueryRaw: TRawByteStringDynArray read FCachedQueryRaw;
    property CachedQueryUni: TUnicodeStringDynArray read FCachedQueryUni;
    property IsParamIndex: TBooleanDynArray read FIsParamIndex;
    property IsNCharIndex: TBooleanDynArray read FNCharDetected;
    property IsPreparable: Boolean read FIsPraparable;
    property ArrayCount: ArrayLenInt read FInitialArrayCount;
    procedure SetASQL(const Value: RawByteString); override;
    procedure SetWSQL(const Value: ZWideString); override;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    destructor Destroy; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;

    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; virtual;
    function ExecuteUpdatePrepared: Integer; virtual;
    function ExecutePrepared: Boolean; virtual;

    procedure BeforeClose; override;
    function GetSQL : String;
    procedure Prepare; virtual;
    procedure Unprepare; virtual;
    function IsPrepared: Boolean; virtual;
    property Prepared: Boolean read IsPrepared;

    procedure SetDefaultValue(ParameterIndex: Integer; const Value: string);

    procedure SetNull(ParameterIndex: Integer; const SQLType: TZSQLType); virtual;
    procedure SetBoolean(ParameterIndex: Integer; const Value: Boolean); virtual;
    procedure SetByte(ParameterIndex: Integer; const Value: Byte); virtual;
    procedure SetShort(ParameterIndex: Integer; const Value: ShortInt); virtual;
    procedure SetWord(ParameterIndex: Integer; const Value: Word); virtual;
    procedure SetSmall(ParameterIndex: Integer; const Value: SmallInt); virtual;
    procedure SetUInt(ParameterIndex: Integer; const Value: Cardinal); virtual;
    procedure SetInt(ParameterIndex: Integer; const Value: Integer); virtual;
    procedure SetULong(ParameterIndex: Integer; const Value: UInt64); virtual;
    procedure SetLong(ParameterIndex: Integer; const Value: Int64); virtual;
    procedure SetFloat(ParameterIndex: Integer; const Value: Single); virtual;
    procedure SetDouble(ParameterIndex: Integer; const Value: Double); virtual;
    procedure SetCurrency(ParameterIndex: Integer; const Value: Currency); virtual;
    procedure SetBigDecimal(ParameterIndex: Integer; const Value: Extended); virtual;
    procedure SetPChar(ParameterIndex: Integer; const Value: PChar); virtual;
    procedure SetCharRec(ParameterIndex: Integer; const Value: TZCharRec); virtual;
    procedure SetString(ParameterIndex: Integer; const Value: String); virtual;
    {$IFNDEF NO_ANSISTRING}
    procedure SetAnsiString(ParameterIndex: Integer; const Value: AnsiString); virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetUTF8String(ParameterIndex: Integer; const Value: UTF8String); virtual;
    {$ENDIF}
    procedure SetRawByteString(ParameterIndex: Integer; const Value: RawByteString); virtual;
    procedure SetUnicodeString(ParameterIndex: Integer; const Value: ZWideString);  virtual; //AVZ
    procedure SetBytes(ParameterIndex: Integer; const Value: TBytes); virtual;
    procedure SetGUID(ParameterIndex: Integer; const Value: TGUID); virtual;
    procedure SetDate(ParameterIndex: Integer; const Value: TDateTime); virtual;
    procedure SetTime(ParameterIndex: Integer; const Value: TDateTime); virtual;
    procedure SetTimestamp(ParameterIndex: Integer; const Value: TDateTime); virtual;
    procedure SetAsciiStream(ParameterIndex: Integer; const Value: TStream); virtual;
    procedure SetUnicodeStream(ParameterIndex: Integer; const Value: TStream); virtual;
    procedure SetBinaryStream(ParameterIndex: Integer; const Value: TStream); virtual;
    procedure SetBlob(ParameterIndex: Integer; const SQLType: TZSQLType; const Value: IZBlob); virtual;
    procedure SetValue(ParameterIndex: Integer; const Value: TZVariant); virtual;
    procedure SetNullArray(ParameterIndex: Integer; const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull); virtual;
    procedure SetDataArray(ParameterIndex: Integer; const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull); virtual;

    procedure ClearParameters; virtual;

    procedure AddBatchPrepared; virtual;
    function GetMetaData: IZResultSetMetaData; virtual;
    function GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString; override;
    function GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString; override;
    function CreateLogEvent(const Category: TZLoggingCategory): TZLoggingEvent; override;

    procedure SetResultSetConcurrency(Value: TZResultSetConcurrency); override;
    procedure SetResultSetType(Value: TZResultSetType); override;
  end;

  {** Implements Abstract Callable SQL statement. }
  TZAbstractCallableStatement = class(TZAbstractPreparedStatement,
    IZCallableStatement)
  private
    FOutParamValues: TZVariantDynArray;
    FOutParamTypes: TZSQLTypeArray;
    FOutParamCount: Integer;
    FLastWasNull: Boolean;
    FTemp: String;
    FSQL: String;
    FProcSql: RawByteString;
    FIsFunction: Boolean;
    FHasOutParameter: Boolean;
  protected
    FResultSets: IZCollection;
    FActiveResultset: Integer;
    FDBParamTypes: TZProcedureColumnTypeDynArray;
    procedure ClearResultSets; virtual;
    procedure TrimInParameters; virtual;
    procedure SetOutParamCount(NewParamCount: Integer); virtual;
    function GetOutParam(ParameterIndex: Integer): TZVariant; virtual;
    procedure SetProcSQL(const Value: RawByteString); virtual;

    property OutParamValues: TZVariantDynArray
      read FOutParamValues write FOutParamValues;
    property OutParamTypes: TZSQLTypeArray
      read FOutParamTypes write FOutParamTypes;
    property OutParamCount: Integer read FOutParamCount write FOutParamCount;
    property LastWasNull: Boolean read FLastWasNull write FLastWasNull;
    property ProcSql: RawByteString read FProcSQL write SetProcSQL;
    property SQL: String read FSQL;
  public
    constructor Create(const Connection: IZConnection; const SQL: string; Info: TStrings);
    procedure ClearParameters; override;
    procedure BeforeClose; override;

    function IsFunction: Boolean;
    function HasOutParameter: Boolean;
    function GetFirstResultSet: IZResultSet; virtual;
    function GetPreviousResultSet: IZResultSet; virtual;
    function GetNextResultSet: IZResultSet; virtual;
    function GetLastResultSet: IZResultSet; virtual;
    function BOR: Boolean; virtual;
    function EOR: Boolean; virtual;
    function GetResultSetByIndex(const Index: Integer): IZResultSet; virtual;
    function GetResultSetCount: Integer; virtual;

    procedure RegisterOutParameter(ParameterIndex: Integer;
      SQLType: Integer); virtual;
    procedure RegisterParamType(ParameterIndex:integer;ParamType:Integer);virtual;
    function WasNull: Boolean; virtual;

    function IsNull(ParameterIndex: Integer): Boolean; virtual;
    function GetPChar(ParameterIndex: Integer): PChar; virtual;
    function GetString(ParameterIndex: Integer): String; virtual;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ParameterIndex: Integer): AnsiString; virtual;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ParameterIndex: Integer): UTF8String; virtual;
    {$ENDIF}
    function GetRawByteString(ParameterIndex: Integer): RawByteString; virtual;
    function GetUnicodeString(ParameterIndex: Integer): ZWideString; virtual;
    function GetBoolean(ParameterIndex: Integer): Boolean; virtual;
    function GetByte(ParameterIndex: Integer): Byte; virtual;
    function GetShort(ParameterIndex: Integer): ShortInt; virtual;
    function GetWord(ParameterIndex: Integer): Word; virtual;
    function GetSmall(ParameterIndex: Integer): SmallInt; virtual;
    function GetUInt(ParameterIndex: Integer): Cardinal; virtual;
    function GetInt(ParameterIndex: Integer): Integer; virtual;
    function GetULong(ParameterIndex: Integer): UInt64; virtual;
    function GetLong(ParameterIndex: Integer): Int64; virtual;
    function GetFloat(ParameterIndex: Integer): Single; virtual;
    function GetDouble(ParameterIndex: Integer): Double; virtual;
    function GetCurrency(ParameterIndex: Integer): Currency; virtual;
    function GetBigDecimal(ParameterIndex: Integer): Extended; virtual;
    function GetBytes(ParameterIndex: Integer): TBytes; virtual;
    function GetDate(ParameterIndex: Integer): TDateTime; virtual;
    function GetTime(ParameterIndex: Integer): TDateTime; virtual;
    function GetTimestamp(ParameterIndex: Integer): TDateTime; virtual;
    function GetValue(ParameterIndex: Integer): TZVariant; virtual;
  end;

  {** Implements a real Prepared Callable SQL Statement. }
  TZAbstractPreparedCallableStatement = CLass(TZAbstractCallableStatement)
  protected
    FProcSQL: RawByteString;
    procedure SetProcSQL(const Value: RawByteString); override;
  public
    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;
    function Execute(const SQL: RawByteString): Boolean; override;
  end;

  {** Implements an Emulated Prepared SQL Statement. }

  { TZEmulatedPreparedStatement }

  TZEmulatedPreparedStatement = class(TZAbstractPreparedStatement)
  private
    FExecStatement: IZStatement;
    FLastStatement: IZStatement;
    procedure SetLastStatement(const LastStatement: IZStatement);
  protected
    FNeedNCharDetection: Boolean;
    property ExecStatement: IZStatement read FExecStatement write FExecStatement;
    property LastStatement: IZStatement read FLastStatement write SetLastStatement;

    function CreateExecStatement: IZStatement; virtual; abstract;
    function PrepareWideSQLParam({%H-}ParamIndex: Integer): ZWideString; virtual;
    function PrepareAnsiSQLParam({%H-}ParamIndex: Integer): RawByteString; virtual;
    function GetExecStatement: IZStatement;
    procedure TokenizeSQLQueryRaw;
    procedure TokenizeSQLQueryUni;
    function PrepareWideSQLQuery: ZWideString; virtual;
    function PrepareAnsiSQLQuery: RawByteString; virtual;
  public
    procedure BeforeClose; override;

    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;
    function Execute(const SQL: RawByteString): Boolean; override;

    function ExecuteQueryPrepared: IZResultSet; override;
    function ExecuteUpdatePrepared: Integer; override;
    function ExecutePrepared: Boolean; override;
    function CreateLogEvent(const {%H-}Category: TZLoggingCategory): TZLoggingEvent; override;
  end;

  TZAbstractEmulatedPreparedStatement = class(TZAbstractPreparedStatement)
  protected
    FNeedNCharDetection: Boolean;
  public
    function ExecuteQuery(const SQL: ZWideString): IZResultSet; override;
    function ExecuteQuery(const SQL: RawByteString): IZResultSet; override;
    function ExecuteUpdate(const SQL: ZWideString): Integer; override;
    function ExecuteUpdate(const SQL: RawByteString): Integer; override;
    function Execute(const SQL: ZWideString): Boolean; override;
    function Execute(const SQL: RawByteString): Boolean; override;
  end;

  TZEmulatedPreparedStatement_A = class(TZAbstractEmulatedPreparedStatement)
  protected
    procedure TokenizeSQLQueryRaw;
    function GetParamAsString(ParamIndex: Integer): RawByteString; virtual; abstract;
    function ComposeRawSQLQuery: RawByteString;
  end;

  TZEmulatedPreparedStatement_W = class(TZAbstractEmulatedPreparedStatement)
  protected
    procedure TokenizeSQLQueryUni;
    function GetParamAsString(ParamIndex: Integer): ZWideString; virtual; abstract;
    function ComposeWideSQLQuery: ZWideString;
  end;

implementation

uses ZFastCode, ZSysUtils, ZMessages, ZDbcResultSet, ZCollections,
  ZEncoding
  {$IF defined(NO_INLINE_SIZE_CHECK) and not defined(UNICODE) and defined(MSWINDOWS)},Windows{$IFEND}
  {$IFDEF NO_INLINE_SIZE_CHECK}, Math{$ENDIF};

var
{**
  Holds the value of the last assigned statement ID.
  Only Accessible using TZAbstractStatement.GetNextStatementId.
}
  GlobalStatementIdCounter : integer;

{ TZAbstractStatement }

{**
  Constructs this class and defines the main properties.
  @param Connection a database connection object.
  @param Info a statement parameters;
}
constructor TZAbstractStatement.Create(const Connection: IZConnection; Info: TStrings);
begin
  { Sets the default properties. }
  inherited Create;
  Self.ConSettings := Connection.GetConSettings;
  FLastUpdateCount := -1;

  FConnection := Connection;
  Connection.RegisterStatement(Self);
  FBatchQueries := TStringList.Create;

  FInfo := TStringList.Create;
  if Info <> nil then
    FInfo.AddStrings(Info);
  FChunkSize := StrToIntDef(DefineStatementParameter(Self, 'chunk_size', '4096'), 4096);
  FCachedLob := StrToBoolEx(DefineStatementParameter(Self, 'cachedlob', 'false'));
  FStatementId := Self.GetNextStatementId;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractStatement.Destroy;
begin
  Close;
    FreeAndNil(FBatchQueries);
  FConnection.DeregisterStatement(Self);
  FConnection := nil;
  FreeAndNil(FInfo);
  FLastResultSet := nil;
  inherited Destroy;
end;

{**
  Sets the preprepared SQL-Statement in an String and AnsiStringForm.
  @param Value: the SQL-String which has to be optional preprepared
}
procedure TZAbstractStatement.SetWSQL(const Value: ZWideString);
{$IFNDEF UNICODE}var CP: Word;{$ENDIF}
begin
  if FWSQL <> Value then
    FClosed := False;
    {$IFDEF UNICODE}
    if (ConSettings^.ClientCodePage^.Encoding = ceUTF16) then begin
      FWSQL := GetUnicodeEncodedSQL(Value);
      {$IFDEF DEBUG}FASQL := ZUnicodeToRaw(FWSQL, ConSettings^.CTRL_CP);{$ENDIF}
    end else begin
      FASQL := GetRawEncodedSQL(Value);
      FWSQL := Value;
    end;
    {$ELSE !UNICODE}
    begin
      if (ConSettings^.ClientCodePage^.Encoding = ceUTF16)
      then CP := ConSettings^.CTRL_CP
      else CP := ConSettings.ClientCodePage.CP;
      FaSQL := GetRawEncodedSQL(ConSettings^.ConvFuncs.ZUnicodeToRaw(Value, CP)); //required for the resultsets
      FWSQL := Value;
    end;
    {$ENDIF UNICODE}
end;

procedure TZAbstractStatement.ToBuff(const Value: RawByteString;
  var Result: RawByteString);
var
  P: PAnsiChar;
  L: Integer;
begin
  L := Length(Value);
  if L = 0 then Exit;
  if L < (SizeOf(fABuffer)-fABufferIndex) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> no move
    then fABuffer[fABufferIndex] := AnsiChar(P^)
    else System.Move(Pointer(Value)^, fABuffer[fABufferIndex], L);
    Inc(fABufferIndex, L);
  end else begin
    SetLength(Result, Length(Result)+fABufferIndex+L);
    P := Pointer(Result);
    Inc(P, Length(Result)-fABufferIndex-L);
    if fABufferIndex > 0 then begin
      System.Move(fABuffer[0], P^, fABufferIndex);
      Inc(P, fABufferIndex);
      fABufferIndex := 0;
    end;
    System.Move(Pointer(Value)^, P^, L);
  end;
end;

procedure TZAbstractStatement.ToBuff(const Value: ZWideString;
  var Result: ZWideString);
var
  P: PWideChar;
  L: Integer;
begin
  L := Length(Value);
  if L = 0 then Exit;
  if L < ((SizeOf(fWBuffer) shr 1)-fWBufferIndex) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> no move
    then fWBuffer[fWBufferIndex] := P^
    else System.Move(Pointer(Value)^, fWBuffer[fWBufferIndex], L shl 1);
    Inc(fWBufferIndex, L);
  end else begin
    SetLength(Result, Length(Result)+fWBufferIndex+L);
    P := Pointer(Result);
    Inc(P, Length(Result)-fWBufferIndex-L);
    if fWBufferIndex > 0 then begin
      System.Move(fWBuffer[0], P^, fWBufferIndex shl 1);
      Inc(P, fWBufferIndex);
      fWBufferIndex := 0;
    end;
    System.Move(Pointer(Value)^, P^, L shl 1);
  end;
end;

procedure TZAbstractStatement.SetASQL(const Value: RawByteString);
begin
  if FASQL <> Value then begin
    FClosed := False;
    {$IFDEF UNICODE}
    FASQL := Value;
    FWSQL := ZRawToUnicode(FASQL, ConSettings^.ClientCodePage.CP); //required for the resultsets
    {$ELSE !UNICODE}
    if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then begin
      FWSQL := GetUnicodeEncodedSQL(Value);
      FASQL := ZUnicodeToRaw(FWSQL, ConSettings^.CTRL_CP);
    end else begin
      FASQL := GetRawEncodedSQL(Value);
      {$IFDEF DEBUG}FWSQL := ZRawToUnicode(FASQL, FClientCP);{$ENDIF}
    end;
    {$ENDIF UNICODE}
  end;
end;

{**
  Raises unsupported operation exception.
}
procedure TZAbstractStatement.RaiseUnsupportedException;
begin
  raise EZSQLException.Create(SUnsupportedOperation);
end;

procedure TZAbstractStatement.ReleaseConnection;
begin
  FConnection := nil;
end;

{**
  Sets a last result set to avoid problems with reference counting.
  @param ResultSet the lastest executed result set.
}
procedure TZAbstractStatement.SetLastResultSet(const ResultSet: IZResultSet);
begin
  if (FLastResultSet <> nil) and (Pointer(ResultSet) <> Pointer(FLastResultSet)) then
    FLastResultSet.Close;

  FLastResultSet := ResultSet;
end;

procedure TZAbstractStatement.PrepareOpenResultSetForReUse;
begin
  if Assigned(FOpenResultSet) then
    if not Assigned(IZResultSet(FOpenResultSet).GetMetaData) then //is there another way to test if open?
      FOpenResultSet := nil
    else
      if (IZResultSet(FOpenResultSet).GetConcurrency = GetResultSetConcurrency) and
         (IZResultSet(FOpenResultSet).GetFetchDirection = GetFetchDirection) then
        IZResultSet(FOpenResultSet).ResetCursor
      else
      begin
        IZResultSet(FOpenResultSet).Close;
        FOpenResultSet := nil;
      end;
end;

procedure TZAbstractStatement.PrepareLastResultSetForReUse;
begin
  if Assigned(FLastResultSet) then
    if not Assigned(FLastResultSet.GetMetaData) then //is there another way to test if open?
      FLastResultSet := nil
    else
      if (FLastResultSet.GetConcurrency = GetResultSetConcurrency) and
         (FLastResultSet.GetFetchDirection = GetFetchDirection) then
        FLastResultSet.ResetCursor
      else
      begin
        FLastResultSet.Close;
        FLastResultSet := nil;
      end;
end;

procedure TZAbstractStatement.FlushBuff(var Result: RawByteString);
var P: PAnsiChar;
begin
  if fABufferIndex > 0 then begin
    SetLength(Result, Length(Result)+fABufferIndex);
    P := Pointer(Result);
    Inc(P, Length(Result)-fABufferIndex);
    System.Move(fABuffer[0], P^, fABufferIndex);
    fABufferIndex := 0;
  end;
end;

procedure TZAbstractStatement.FlushBuff(var Result: ZWideString);
var P: PWideChar;
begin
  if fWBufferIndex > 0 then begin
    SetLength(Result, Length(Result)+fWBufferIndex);
    P := Pointer(Result);
    Inc(P, Length(Result)-fWBufferIndex);
    System.Move(fWBuffer[0], P^, fWBufferIndex shl 1);
    fWBufferIndex := 0;
  end;
end;

procedure TZAbstractStatement.FreeOpenResultSetReference(const ResultSet: IZResultSet);
begin
  if FOpenResultSet = Pointer(ResultSet) then
    FOpenResultSet := nil;
  if Pointer(FLastResultSet) = Pointer(ResultSet) then
    FLastResultSet := nil;
end;

class function TZAbstractStatement.GetNextStatementId: integer;
begin
  Inc(GlobalStatementIdCounter);
  Result := GlobalStatementIdCounter;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
begin
  WSQL := SQL;
  Result := ExecuteQuery(ASQL);
end;

function TZAbstractStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  ASQL := SQL;
  Result := nil;
  DriverManager.LogMessage(lcExecute,Self);
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZAbstractStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
begin
  WSQL := SQL;
  Result := ExecuteUpdate(ASQL);
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZAbstractStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  ASQL := SQL;
  Result := -1;
  DriverManager.LogMessage(lcExecute,Self);
end;

{**
  Releases this <code>Statement</code> object's database
  and JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.
  It is generally good practice to release resources as soon as
  you are finished with them to avoid tying up database
  resources.
  <P><B>Note:</B> A <code>Statement</code> object is automatically closed when it is
  garbage collected. When a <code>Statement</code> object is closed, its current
  <code>ResultSet</code> object, if one exists, is also closed.
}
procedure TZAbstractStatement.Close;
var RefCountAdded: Boolean;
begin
  if not fClosed then begin
    RefCountAdded := (RefCount = 1) and (Assigned(FOpenResultSet) or Assigned(FLastResultSet));
  if RefCountAdded then _AddRef;
  try
        BeforeClose;
        FClosed := True;
        AfterClose;
    finally
    FClosed := True;
    if RefCountAdded then begin
      if (RefCount = 1) then begin
        DriverManager.AddGarbage(Self);
          ReleaseConnection;
      end;
      _Release;
    end;
  end;
  end;
end;

{**
  Returns the maximum number of bytes allowed
  for any column value.
  This limit is the maximum number of bytes that can be
  returned for any column value.
  The limit applies only to <code>BINARY</code>,
  <code>VARBINARY</code>, <code>LONGVARBINARY</code>, <code>CHAR</code>, <code>VARCHAR</code>, and <code>LONGVARCHAR</code>
  columns.  If the limit is exceeded, the excess data is silently
  discarded.
  @return the current max column size limit; zero means unlimited
}
function TZAbstractStatement.GetMaxFieldSize: Integer;
begin
  Result := FMaxFieldSize;
end;

{**
  Sets the limit for the maximum number of bytes in a column to
  the given number of bytes.  This is the maximum number of bytes
  that can be returned for any column value.  This limit applies
  only to <code>BINARY</code>, <code>VARBINARY</code>,
  <code>LONGVARBINARY</code>, <code>CHAR</code>, <code>VARCHAR</code>, and
  <code>LONGVARCHAR</code> fields.  If the limit is exceeded, the excess data
  is silently discarded. For maximum portability, use values
  greater than 256.

  @param max the new max column size limit; zero means unlimited
}
procedure TZAbstractStatement.SetMaxFieldSize(Value: Integer);
begin
  FMaxFieldSize := Value;
end;

{**
  Retrieves the maximum number of rows that a
  <code>ResultSet</code> object can contain.  If the limit is exceeded, the excess
  rows are silently dropped.

  @return the current max row limit; zero means unlimited
}
function TZAbstractStatement.GetMaxRows: Integer;
begin
  Result := FMaxRows;
end;

{**
  Sets the limit for the maximum number of rows that any
  <code>ResultSet</code> object can contain to the given number.
  If the limit is exceeded, the excess rows are silently dropped.

  @param max the new max rows limit; zero means unlimited
}
procedure TZAbstractStatement.SetMaxRows(Value: Integer);
begin
  FMaxRows := Value;
end;

{**
  Sets escape processing on or off.
  If escape scanning is on (the default), the driver will do
  escape substitution before sending the SQL to the database.

  Note: Since prepared statements have usually been parsed prior
  to making this call, disabling escape processing for prepared
  statements will have no effect.

  @param enable <code>true</code> to enable; <code>false</code> to disable
}
procedure TZAbstractStatement.SetEscapeProcessing(Value: Boolean);
begin
  FEscapeProcessing := Value;
end;

{**
  Retrieves the number of seconds the driver will
  wait for a <code>Statement</code> object to execute. If the limit is exceeded, a
  <code>SQLException</code> is thrown.

  @return the current query timeout limit in seconds; zero means unlimited
}
function TZAbstractStatement.GetQueryTimeout: Integer;
begin
  Result := FQueryTimeout;
end;

{**
  Sets the number of seconds the driver will
  wait for a <code>Statement</code> object to execute to the given number of seconds.
  If the limit is exceeded, an <code>SQLException</code> is thrown.

  @param seconds the new query timeout limit in seconds; zero means unlimited
}
procedure TZAbstractStatement.SetQueryTimeout(Value: Integer);
begin
  FQueryTimeout := Value;
end;

{**
  Cancels this <code>Statement</code> object if both the DBMS and
  driver support aborting an SQL statement.
  This method can be used by one thread to cancel a statement that
  is being executed by another thread.
}
procedure TZAbstractStatement.Cancel;
begin
  RaiseUnsupportedException;
end;

{**
  Retrieves the first warning reported by calls on this <code>Statement</code> object.
  Subsequent <code>Statement</code> object warnings will be chained to this
  <code>SQLWarning</code> object.

  <p>The warning chain is automatically cleared each time
    a statement is (re)executed.

  <P><B>Note:</B> If you are processing a <code>ResultSet</code> object, any
  warnings associated with reads on that <code>ResultSet</code> object
  will be chained on it.

  @return the first <code>SQLWarning</code> object or <code>null</code>
}
function TZAbstractStatement.GetWarnings: EZSQLWarning;
begin
  Result := nil;
end;

{**
  Clears all the warnings reported on this <code>Statement</code>
  object. After a call to this method,
  the method <code>getWarnings</code> will return
  <code>null</code> until a new warning is reported for this
  <code>Statement</code> object.
}
procedure TZAbstractStatement.ClearWarnings;
begin
end;

function TZAbstractStatement.GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var
  SQLTokens: TZTokenDynArray;
  i: Integer;
begin
  if ConSettings^.AutoEncode then begin
    Result := EmptyRaw; //init for FPC
    SQLTokens := GetConnection.GetDriver.GetTokenizer.TokenizeBuffer(SQL, [toSkipEOF]); //Disassembles the Query
    {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := '';
    for i := Low(SQLTokens) to high(SQLTokens) do begin //Assembles the Query
      {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} + SQLTokens[i].Value;
      case SQLTokens[i].TokenType of
        ttQuoted, ttComment,
        ttWord, ttQuotedIdentifier, ttKeyword:
          ToBuff(ConSettings^.ConvFuncs.ZStringToRaw(SQLTokens[i].Value,
            ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP), Result);
        else
          ToBuff({$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(SQLTokens[i].Value), Result);
      end;
    end;
    FlushBuff(Result);
  end else begin
    {$IFDEF UNICODE}
    FWSQL := SQL;
    Result := ConSettings^.ConvFuncs.ZUnicodeToRaw(SQL, ConSettings^.ClientCodePage^.CP);
    {$ELSE}
    FASQL := SQL;
    Result := SQL;
    {$ENDIF}
  end;
end;

function TZAbstractStatement.GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString;
var
  SQLTokens: TZTokenDynArray;
  i: Integer;
  {$IFNDEF UNICODE}US: ZWideString;{$ENDIF}
begin
  if ConSettings^.AutoEncode then begin
    Result := ''; //init
    {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := '';
    SQLTokens := GetConnection.GetDriver.GetTokenizer.TokenizeBuffer(SQL, [toSkipEOF]); //Disassembles the Query
    for i := Low(SQLTokens) to high(SQLTokens) do begin //Assembles the Query
      {$IFDEF UNICODE}
      ToBuff(SQLTokens[i].Value, Result);
      {$ELSE !UNICODE}
      ToBuff(SQLTokens[i].Value, FASQL);
      case (SQLTokens[i].TokenType) of
        ttQuoted, ttComment,
        ttWord, ttQuotedIdentifier, ttKeyword: begin
          US := ConSettings^.ConvFuncs.ZStringToUnicode(SQL, ConSettings.CTRL_CP);
          ToBuff(US, Result);
        end else begin
          US := ASCII7ToUnicodeString(SQLTokens[i].Value);
          ToBuff(US, Result);
        end;
      end;
      {$ENDIF UNICODE}
    end;
    FlushBuff(Result);
    FWSQL := Result;
    {$IFNDEF UNICODE}
    FlushBuff(FASQL);
    {$ENDIF}
  end
  else
  begin
    {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := SQL;
    {$IFDEF UNICODE}
    Result := SQL;
    {$ELSE !UNICODE}
    Result := ConSettings^.ConvFuncs.ZStringToUnicode(SQL, ConSettings.CTRL_CP);
    {$ENDIF UNICODE}
      FWSQL := '';
  end;
end;

function TZAbstractStatement.CreateStmtLogEvent(Category: TZLoggingCategory;
  const Msg: RawByteString = EmptyRaw): TZLoggingEvent;
begin
  if msg <> EmptyRaw then
    result := TZLoggingEvent.Create(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId)+' : '+ Msg, 0, EmptyRaw)
  else
    result := TZLoggingEvent.Create(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId), 0, EmptyRaw);
end;

function TZAbstractStatement.CreateLogEvent(
  const Category: TZLoggingCategory ): TZLoggingEvent;
begin
  case Category of
    lcPrepStmt, lcExecute:
      result := CreateStmtLogEvent(Category, ASQL);
    lcExecPrepStmt, lcUnprepStmt:
      result := CreateStmtLogEvent(Category);
  else
    result := nil;
  end;
end;

{**
  Defines the SQL cursor name that will be used by
  subsequent <code>Statement</code> object <code>execute</code> methods.
  This name can then be
  used in SQL positioned update/delete statements to identify the
  current row in the <code>ResultSet</code> object generated by this statement.  If
  the database doesn't support positioned update/delete, this
  method is a noop.  To insure that a cursor has the proper isolation
  level to support updates, the cursor's <code>SELECT</code> statement should be
  of the form 'select for update ...'. If the 'for update' phrase is
  omitted, positioned updates may fail.

  <P><B>Note:</B> By definition, positioned update/delete
  execution must be done by a different <code>Statement</code> object than the one
  which generated the <code>ResultSet</code> object being used for positioning. Also,
  cursor names must be unique within a connection.

  @param name the new cursor name, which must be unique within a connection
}
procedure TZAbstractStatement.SetCursorName(const Value: String);
begin
  FCursorName := ConSettings^.ConvFuncs.ZStringToRaw(Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractStatement.Execute(const SQL: ZWideString): Boolean;
begin
  WSQL := SQL;
  Result := Execute(ASQL);
end;

function TZAbstractStatement.Execute(const SQL: RawByteString): Boolean;
begin
  ASQL := SQL;
  Result := False;
  LastResultSet := nil;
  LastUpdateCount := -1;
  DriverManager.LogMessage(lcExecute,Self);
end;

function TZAbstractStatement.GetSQL: String;
begin
  Result := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
end;

{**
  Returns the current result as a <code>ResultSet</code> object.
  This method should be called only once per result.

  @return the current result as a <code>ResultSet</code> object;
  <code>null</code> if the result is an update count or there are no more results
  @see #execute
}
function TZAbstractStatement.GetResultSet: IZResultSet;
begin
  Result := FLastResultSet;
  { does not work as TZGenericTestDbcResultSet.TestLastQuery does expect it! }
  {FLastResultSet := nil;
  FOpenResultSet := Pointer(Result);}
end;

{**
  Returns the current result as an update count;
  if the result is a <code>ResultSet</code> object or there are no more results, -1
  is returned. This method should be called only once per result.

  @return the current result as an update count; -1 if the current result is a
    <code>ResultSet</code> object or there are no more results
  @see #execute
}
function TZAbstractStatement.GetUpdateCount: Integer;
begin
  Result := FLastUpdateCount;
end;

{**
  Moves to a <code>Statement</code> object's next result.  It returns
  <code>true</code> if this result is a <code>ResultSet</code> object.
  This method also implicitly closes any current <code>ResultSet</code>
  object obtained with the method <code>getResultSet</code>.

  <P>There are no more results when the following is true:
  <PRE>
        <code>(!getMoreResults() && (getUpdateCount() == -1)</code>
  </PRE>

 @return <code>true</code> if the next result is a <code>ResultSet</code> object;
   <code>false</code> if it is an update count or there are no more results
 @see #execute
}
function TZAbstractStatement.GetMoreResults: Boolean;
begin
  Result := False;
end;

{**
  Retrieves the direction for fetching rows from
  database tables that is the default for result sets
  generated from this <code>Statement</code> object.
  If this <code>Statement</code> object has not set
  a fetch direction by calling the method <code>setFetchDirection</code>,
  the return value is implementation-specific.

  @return the default fetch direction for result sets generated
    from this <code>Statement</code> object
}
function TZAbstractStatement.GetFetchDirection: TZFetchDirection;
begin
  Result := FFetchDirection;
end;

{**
  Gives the driver a hint as to the direction in which
  the rows in a result set
  will be processed. The hint applies only to result sets created
  using this <code>Statement</code> object.  The default value is
  <code>ResultSet.FETCH_FORWARD</code>.
  <p>Note that this method sets the default fetch direction for
  result sets generated by this <code>Statement</code> object.
  Each result set has its own methods for getting and setting
  its own fetch direction.
  @param direction the initial direction for processing rows
}
procedure TZAbstractStatement.SetFetchDirection(Value: TZFetchDirection);
begin
  FFetchDirection := Value;
end;

{**
  Retrieves the number of result set rows that is the default
  fetch size for result sets
  generated from this <code>Statement</code> object.
  If this <code>Statement</code> object has not set
  a fetch size by calling the method <code>setFetchSize</code>,
  the return value is implementation-specific.
  @return the default fetch size for result sets generated
    from this <code>Statement</code> object
}
function TZAbstractStatement.GetFetchSize: Integer;
begin
  Result := FFetchSize;
end;

{**
  Gives the JDBC driver a hint as to the number of rows that should
  be fetched from the database when more rows are needed.  The number
  of rows specified affects only result sets created using this
  statement. If the value specified is zero, then the hint is ignored.
  The default value is zero.

  @param rows the number of rows to fetch
}
procedure TZAbstractStatement.SetFetchSize(Value: Integer);
begin
  FFetchSize := Value;
end;

{**
  Sets a result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param Concurrency either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
procedure TZAbstractStatement.SetResultSetConcurrency(
  Value: TZResultSetConcurrency);
begin
  FResultSetConcurrency := Value;
end;

{**
  Retrieves the result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @return either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
function TZAbstractStatement.GetResultSetConcurrency: TZResultSetConcurrency;
begin
  Result := FResultSetConcurrency;
end;

{**
  Sets a result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param ResultSetType one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractStatement.SetResultSetType(Value: TZResultSetType);
begin
  FResultSetType := Value;
end;

{**
  Retrieves the result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @return one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
function TZAbstractStatement.GetResultSetType: TZResultSetType;
begin
  Result := FResultSetType;
end;

{**
  Gets the current value for locate updates.
  @returns the current value for locate updates.
}
function TZAbstractStatement.GetLocateUpdates: TZLocateUpdatesMode;
begin
  Result := FLocateUpdates;
end;

{**
  Sets a new value for locate updates.
  @param Value a new value for locate updates.
}
procedure TZAbstractStatement.SetLocateUpdates(Value: TZLocateUpdatesMode);
begin
  FLocateUpdates := Value;
end;

{**
  Gets the current value for post updates.
  @returns the current value for post updates.
}
function TZAbstractStatement.GetPostUpdates: TZPostUpdatesMode;
begin
  Result := FPostUpdates;
end;

{**
  Sets a new value for post updates.
  @param Value a new value for post updates.
}
procedure TZAbstractStatement.SetPostUpdates(Value: TZPostUpdatesMode);
begin
  FPostUpdates := Value;
end;

{**
  Adds an SQL command to the current batch of commmands for this
  <code>Statement</code> object. This method is optional.

  @param sql typically this is a static SQL <code>INSERT</code> or
  <code>UPDATE</code> statement
}
procedure TZAbstractStatement.AddBatch(const SQL: string);
begin
  AddBatchRequest(SQL);
end;

{**
  Adds an SQL command to the current batch of commmands for this
  <code>Statement</code> object. This method is optional.

  @param sql typically this is a static SQL <code>INSERT</code> or
  <code>UPDATE</code> statement
}
procedure TZAbstractStatement.AddBatchRequest(const SQL: string);
begin
  FBatchQueries.Add(SQL);
end;

procedure TZAbstractStatement.AfterClose;
begin

end;

procedure TZAbstractStatement.BeforeClose;
begin
  if Assigned(FLastResultSet) then
    LastResultSet.Close;
  if Assigned(FOpenResultSet) then begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;
end;

{**
  Makes the set of commands in the current batch empty.
  This method is optional.
}
procedure TZAbstractStatement.ClearBatch;
begin
  FBatchQueries.Clear;
end;

{**
  Submits a batch of commands to the database for execution and
  if all commands execute successfully, returns an array of update counts.
  The <code>int</code> elements of the array that is returned are ordered
  to correspond to the commands in the batch, which are ordered
  according to the order in which they were added to the batch.
  The elements in the array returned by the method <code>executeBatch</code>
  may be one of the following:
  <OL>
  <LI>A number greater than or equal to zero -- indicates that the
  command was processed successfully and is an update count giving the
  number of rows in the database that were affected by the command's
  execution
  <LI>A value of <code>-2</code> -- indicates that the command was
  processed successfully but that the number of rows affected is
  unknown
  <P>
  If one of the commands in a batch update fails to execute properly,
  this method throws a <code>BatchUpdateException</code>, and a JDBC
  driver may or may not continue to process the remaining commands in
  the batch.  However, the driver's behavior must be consistent with a
  particular DBMS, either always continuing to process commands or never
  continuing to process commands.  If the driver continues processing
  after a failure, the array returned by the method
  <code>BatchUpdateException.getUpdateCounts</code>
  will contain as many elements as there are commands in the batch, and
  at least one of the elements will be the following:
  <P>
  <LI>A value of <code>-3</code> -- indicates that the command failed
  to execute successfully and occurs only if a driver continues to
  process commands after a command fails
  </OL>
  <P>
  A driver is not required to implement this method.
  The possible implementations and return values have been modified in
  the Java 2 SDK, Standard Edition, version 1.3 to
  accommodate the option of continuing to proccess commands in a batch
  update after a <code>BatchUpdateException</code> obejct has been thrown.

  @return an array of update counts containing one element for each
  command in the batch.  The elements of the array are ordered according
  to the order in which commands were added to the batch.
}
function TZAbstractStatement.ExecuteBatch: TIntegerDynArray;
var
  I: Integer;
begin
  SetLength(Result, FBatchQueries.Count);
  for I := 0 to FBatchQueries.Count -1 do
    Result[I] := ExecuteUpdate(FBatchQueries[I]);
  ClearBatch;
end;

{**
  Returns the <code>Connection</code> object
  that produced this <code>Statement</code> object.
  @return the connection that produced this statement
}
function TZAbstractStatement.GetConnection: IZConnection;
begin
  Result := FConnection;
end;

{**
  Gets statement parameters.
  @returns a list with statement parameters.
}
function TZAbstractStatement.GetParameters: TStrings;
begin
  Result := FInfo;
end;

{**
  Returns the ChunkSize for reading/writing large lobs
  @returns the chunksize in bytes.
}
function TZAbstractStatement.GetChunkSize: Integer;
begin
  Result := FChunkSize;
end;

{ TZAbstractPreparedStatement }

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
}
constructor TZAbstractPreparedStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, Info);
  FClientVariantManger := Connection.GetClientVariantManager;
  {$IFDEF UNICODE}WSQL{$ELSE}ASQL{$ENDIF} := SQL;
  SetInParamCount(0);
  FPrepared := False;
  FInitialArrayCount := 0;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractPreparedStatement.Destroy;
begin
  Unprepare;
  inherited Destroy;
  ClearParameters;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractPreparedStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
begin
  WSQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZAbstractPreparedStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
begin
  WSQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractPreparedStatement.Execute(const SQL: ZWideString): Boolean;
begin
  WSQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractPreparedStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  ASQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZAbstractPreparedStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  ASQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractPreparedStatement.Execute(const SQL: RawByteString): Boolean;
begin
  ASQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Return a VariantManager which supports client encoded RawByteStrings
  @returns IZClientVariantManager
}
function TZAbstractPreparedStatement.GetClientVariantManger: IZClientVariantManager;
begin
  Result := FClientVariantManger;
end;

{**
  Prepares eventual structures for binding input parameters.
}
procedure TZAbstractPreparedStatement.PrepareInParameters;
begin
end;

{**
  Binds the input parameters
}
procedure TZAbstractPreparedStatement.BindInParameters;
begin
  DriverManager.LogMessage(lcBindPrepStmt,Self);
end;

{**
  Removes eventual structures for binding input parameters.
}
procedure TZAbstractPreparedStatement.UnPrepareInParameters;
begin
end;

{**
  Sets a new parameter count and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractPreparedStatement.SetInParamCount(const NewParamCount: Integer);
var
  I: Integer;
begin
  SetLength(FInParamValues, NewParamCount);
  SetLength(FInParamTypes, NewParamCount);
  SetLength(FInParamDefaultValues, NewParamCount);
  for I := FInParamCount to NewParamCount - 1 do
  begin
    FInParamValues[I] := NullVariant;
    FInParamTypes[I] := stUnknown;

    FInParamDefaultValues[I] := '';
  end;
  FInParamCount := NewParamCount;
end;

{**
  Sets a variant value into specified parameter.
  @param ParameterIndex a index of the parameter.
  @param SqlType a parameter SQL type.
  @paran Value a new parameter value.
}
procedure TZAbstractPreparedStatement.SetInParam(ParameterIndex: Integer;
  SQLType: TZSQLType; const Value: TZVariant);
begin
  if ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} >= FInParamCount then
    SetInParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FInParamTypes[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := SQLType;
  FInParamValues[ParameterIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := Value;
end;

{**
  Logs a message about prepared statement event with normal result code.
  @param Category a category of the message.
  @param Protocol a name of the protocol.
  @param Msg a description message.
}
procedure TZAbstractPreparedStatement.LogPrepStmtMessage(Category: TZLoggingCategory;
  const Msg: RawByteString = EmptyRaw);
begin
  if DriverManager.HasLoggingListener then
    if msg <> EmptyRaw then
      DriverManager.LogMessage(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId)+' : '+Msg)
    else
      DriverManager.LogMessage(Category, ConSettings^.Protocol, 'Statement '+IntToRaw(FStatementId));
end;


function TZAbstractPreparedStatement.GetInParamLogValue(Value: TZVariant): RawByteString;
begin
  With Value do
    case VType of
      vtNull : result := '(NULL)';
      vtBoolean : if VBoolean then result := '(TRUE)' else result := '(FALSE)';
      vtBytes : Result := GetSQLHexAnsiString(Pointer(VBytes), Length(VBytes), False);
      vtInteger : result := IntToRaw(VInteger);
      vtFloat : result := FloatToRaw(VFloat);
      vtString,
      {$IFNDEF NO_ANSISTRING}
      vtAnsiString,
      {$ENDIF}
      {$IFNDEF NO_UTF8STRING}
      vtUTF8String,
      {$ENDIF}
      vtRawByteString,
      vtUnicodeString,
      vtCharRec: result := #39 + ClientVarManager.GetAsRawByteString(Value) + #39;
      vtDateTime : result := ClientVarManager.GetAsRawByteString(Value);
      vtPointer : result := '(POINTER)';
      vtInterface : result := '(INTERFACE)';
    else
      result := '(UNKNOWN TYPE)'
    end;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
{$IFDEF FPC}
  {$PUSH} {$WARN 5033 off : Function result does not seem to be set} // base class - result not returned intentionally
{$ENDIF}
function TZAbstractPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  Result := nil;
  { Logging Execution }
  DriverManager.LogMessage(lcExecPrepStmt,Self);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZAbstractPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  Result := -1;
  { Logging Execution }
  DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

{**
  Sets the designated parameter the default SQL value.
  <P><B>Note:</B> You must specify the default value.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param Value the default value normally defined in the field's DML SQL statement
}
procedure TZAbstractPreparedStatement.SetDefaultValue(
  ParameterIndex: Integer; const Value: string);
begin
 if ParameterIndex >= FInParamCount then
   SetInParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FInParamDefaultValues[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := Value;
end;

{**
  Sets the designated parameter to SQL <code>NULL</code>.
  <P><B>Note:</B> You must specify the parameter's SQL type.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param sqlType the SQL type code defined in <code>java.sql.Types</code>
}
procedure TZAbstractPreparedStatement.SetNull(ParameterIndex: Integer;
  const SQLType: TZSQLType);
begin
  SetInParam(ParameterIndex, SQLType, NullVariant);
end;

{**
  Sets the designated parameter to a Java <code>boolean</code> value.
  The driver converts this
  to an SQL <code>BIT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetBoolean(ParameterIndex: Integer;
  const Value: Boolean);
begin
  SetInParam(ParameterIndex, stBoolean, EncodeBoolean(Value));
end;

{**
  Sets the designated parameter to a Java <code>byte</code> value.
  The driver converts this
  to an SQL <code>Byte</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetByte(ParameterIndex: Integer;
  const Value: Byte);
begin
  SetInParam(ParameterIndex, stByte, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>ShortInt</code> value.
  The driver converts this
  to an SQL <code>ShortInt</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetShort(ParameterIndex: Integer;
  const Value: ShortInt);
begin
  SetInParam(ParameterIndex, stShort, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetWord(ParameterIndex: Integer;
  const Value: Word);
begin
  SetInParam(ParameterIndex, stWord, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>SmallInt</code> value.
  The driver converts this
  to an SQL <code>SMALLINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetSmall(ParameterIndex: Integer;
  const Value: SmallInt);
begin
  SetInParam(ParameterIndex, stSmall, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>uint</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetUInt(ParameterIndex: Integer;
  const Value: Cardinal);
begin
  SetInParam(ParameterIndex, stLongWord, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>int</code> value.
  The driver converts this
  to an SQL <code>INTEGER</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetInt(ParameterIndex: Integer;
  const Value: Integer);
begin
  SetInParam(ParameterIndex, stInteger, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>ulong</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetULong(ParameterIndex: Integer;
  const Value: UInt64);
begin
  SetInParam(ParameterIndex, stULong, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>long</code> value.
  The driver converts this
  to an SQL <code>BIGINT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetLong(ParameterIndex: Integer;
  const Value: Int64);
begin
  SetInParam(ParameterIndex, stLong, EncodeInteger(Value));
end;

{**
  Sets the designated parameter to a Java <code>float</code> value.
  The driver converts this
  to an SQL <code>FLOAT</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetFloat(ParameterIndex: Integer;
  const Value: Single);
begin
  SetInParam(ParameterIndex, stFloat, EncodeFloat(Value));
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetDouble(ParameterIndex: Integer;
  const Value: Double);
begin
  SetInParam(ParameterIndex, stDouble, EncodeFloat(Value));
end;

{**
  Sets the designated parameter to a Java <code>double</code> value.
  The driver converts this
  to an SQL <code>DOUBLE</code> value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetCurrency(ParameterIndex: Integer;
  const Value: Currency);
begin
  SetInParam(ParameterIndex, stCurrency, EncodeFloat(Value));
end;

{**
  Sets the designated parameter to a <code>java.math.BigDecimal</code> value.
  The driver converts this to an SQL <code>NUMERIC</code> value when
  it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetBigDecimal(
  ParameterIndex: Integer; const Value: Extended);
begin
  SetInParam(ParameterIndex, stBigDecimal, EncodeFloat(Value));
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetPChar(ParameterIndex: Integer;
   const Value: PChar);
begin
  SetInParam(ParameterIndex, stString, EncodeString(Value));
end;

{**
  Sets the designated parameter to a Java <code>TZCharRec</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetCharRec(ParameterIndex: Integer;
  const Value: TZCharRec);
begin
  SetInParam(ParameterIndex, stString, EncodeCharRec(Value));
end;

{**
  Sets the designated parameter to a Java <code>String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetString(ParameterIndex: Integer;
   const Value: String);
begin
  SetInParam(ParameterIndex, stString, EncodeString(Value));
end;

{**
  Sets the designated parameter to a Java <code>AnsiString</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_ANSISTRING}
procedure TZAbstractPreparedStatement.SetAnsiString(ParameterIndex: Integer;
   const Value: AnsiString);
begin
  SetInParam(ParameterIndex, stString, EncodeAnsiString(Value));
end;
{$ENDIF}

{**
  Sets the designated parameter to a Java <code>UTF8String</code> value.
  The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
{$IFNDEF NO_UTF8STRING}
procedure TZAbstractPreparedStatement.SetUTF8String(ParameterIndex: Integer;
   const Value: UTF8String);
begin
  SetInParam(ParameterIndex, stString, EncodeUTF8String(Value));
end;
{$ENDIF}
{**
  Sets the designated parameter to a Java <code>RawByteString</code> value.
  The driver dosn't converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetRawByteString(ParameterIndex: Integer;
   const Value: RawByteString);
begin
  SetInParam(ParameterIndex, stString, EncodeRawByteString(Value));
end;

{**
  Sets a result set concurrency for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param Concurrency either <code>ResultSet.CONCUR_READ_ONLY</code> or
  <code>ResultSet.CONCUR_UPDATABLE</code>
}
procedure TZAbstractPreparedStatement.SetResultSetConcurrency(
  Value: TZResultSetConcurrency);
begin
  if Value <> FResultSetConcurrency then begin
    if Assigned(FOpenResultSet) then begin
      IZResultSet(FOpenResultSet).Close;
      FOpenResultSet := nil;
    end;
    inherited SetResultSetConcurrency(Value);
  end;
end;

{**
  Sets a result set type for <code>ResultSet</code> objects
  generated by this <code>Statement</code> object.

  @param ResultSetType one of <code>ResultSet.TYPE_FORWARD_ONLY</code>,
    <code>ResultSet.TYPE_SCROLL_INSENSITIVE</code>, or
    <code>ResultSet.TYPE_SCROLL_SENSITIVE</code>
}
procedure TZAbstractPreparedStatement.SetResultSetType(Value: TZResultSetType);
begin
  if Value <> FResultSetType then begin
    if Assigned(FOpenResultSet) then begin
      IZResultSet(FOpenResultSet).Close;
      FOpenResultSet := nil;
    end;
    inherited SetResultSetType(Value);
  end;
end;

{**
  Sets the designated parameter to a Object Pascal <code>WideString</code>
  value. The driver converts this
  to an SQL <code>VARCHAR</code> or <code>LONGVARCHAR</code> value
  (depending on the argument's
  size relative to the driver's limits on <code>VARCHAR</code> values)
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetUnicodeString(ParameterIndex: Integer;
  const Value: ZWideString);
begin
  SetInParam(ParameterIndex, stUnicodeString, EncodeUnicodeString(Value));
end;

{**
  Sets the designated parameter to a Java array of bytes.  The driver converts
  this to an SQL <code>VARBINARY</code> or <code>LONGVARBINARY</code>
  (depending on the argument's size relative to the driver's limits on
  <code>VARBINARY</code> values) when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetBytes(ParameterIndex: Integer;
  const Value: TBytes);
begin
  SetInParam(ParameterIndex, stBytes, EncodeBytes(Value));
end;

{**
  Sets the designated parameter to a GUID.
  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetGUID(ParameterIndex: Integer; const Value: TGUID);
begin
  SetInParam(ParameterIndex, stGUID, EncodeGUID(Value));
end;

{**
  Sets the designated parameter to a <code<java.sql.Date</code> value.
  The driver converts this to an SQL <code>DATE</code>
  value when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetDate(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  SetInParam(ParameterIndex, stDate, EncodeDateTime(Value));
end;

{**
  Sets the designated parameter to a <code>java.sql.Time</code> value.
  The driver converts this to an SQL <code>TIME</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetTime(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  SetInParam(ParameterIndex, stTime, EncodeDateTime(Value));
end;

{**
  Sets the designated parameter to a <code>java.sql.Timestamp</code> value.
  The driver converts this to an SQL <code>TIMESTAMP</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetTimestamp(ParameterIndex: Integer;
  const Value: TDateTime);
begin
  SetInParam(ParameterIndex, stTimestamp, EncodeDateTime(Value));
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large ASCII value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code>. Data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from ASCII to the database char format.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the Java input stream that contains the ASCII parameter value
  @param length the number of bytes in the stream
}
procedure TZAbstractPreparedStatement.SetAsciiStream(
  ParameterIndex: Integer; const Value: TStream);
var
  CLob: IZBlob; //use a local variable for the FPC
  MyMemoryStream: TMemoryStream;
  CreatedMemoryStream: boolean;
begin
  MyMemoryStream := nil;
  CreatedMemoryStream := false;
  try
    if Value is TMemoryStream then begin
      MyMemoryStream := Value as TMemoryStream;
    end else begin
      MyMemoryStream := TMemoryStream.Create;
      CreatedMemoryStream := True;
      MyMemoryStream.CopyFrom(Value, Value.Size);
    end;

    if MyMemoryStream.Memory = nil
    then CLob := TZAbstractClob.CreateWithData(PEmptyAnsiString, MyMemoryStream.Size, ConSettings^.ClientCodePage^.CP, ConSettings)
    else if ConSettings^.AutoEncode
      then CLob := TZAbstractClob.CreateWithData(MyMemoryStream.Memory, MyMemoryStream.Size, zCP_NONE, ConSettings)
      else CLob := TZAbstractClob.CreateWithData(MyMemoryStream.Memory, MyMemoryStream.Size, ConSettings^.ClientCodePage^.CP, ConSettings);
    SetBlob(ParameterIndex, stAsciiStream, Clob)
  finally
    if CreatedMemoryStream then
      FreeAndNil(MyMemoryStream);
  end;
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large UNICODE value is input to a <code>LONGVARCHAR</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.  The JDBC driver will
  do any necessary conversion from UNICODE to the database char format.
  The byte format of the Unicode stream must be Java UTF-8, as
  defined in the Java Virtual Machine Specification.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the UNICODE parameter value
}
procedure TZAbstractPreparedStatement.SetUnicodeStream(
  ParameterIndex: Integer; const Value: TStream);
begin
  if TMemoryStream(Value).Memory = nil
  then SetBlob(ParameterIndex, stUnicodeStream, TZAbstractClob.CreateWithData(PEmptyUnicodeString, Value.Size, ConSettings))
  else SetBlob(ParameterIndex, stUnicodeStream, TZAbstractClob.CreateWithData(TMemoryStream(Value).Memory, Value.Size, zCP_UTF16, ConSettings));
end;

{**
  Sets the designated parameter to the given input stream, which will have
  the specified number of bytes.
  When a very large binary value is input to a <code>LONGVARBINARY</code>
  parameter, it may be more practical to send it via a
  <code>java.io.InputStream</code> object. The data will be read from the stream
  as needed until end-of-file is reached.

  <P><B>Note:</B> This stream object can either be a standard
  Java stream object or your own subclass that implements the
  standard interface.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the java input stream which contains the binary parameter value
}
procedure TZAbstractPreparedStatement.SetBinaryStream(
  ParameterIndex: Integer; const Value: TStream);
begin
  SetBlob(ParameterIndex, stBinaryStream, TZAbstractBlob.CreateWithStream(Value));
end;

{**
  Sets a blob object for the specified parameter.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @param Value the java blob object.
}
procedure TZAbstractPreparedStatement.SetBlob(ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value: IZBlob);
begin
  if not (SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
    raise EZSQLException.Create(SWrongTypeForBlobParameter);
  SetInParam(ParameterIndex, SQLType, EncodeInterface(Value));
end;

{**
  Sets a variant value for the specified parameter.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @param Value the variant value.
}
procedure TZAbstractPreparedStatement.SetValue(ParameterIndex: Integer;
  const Value: TZVariant);
var
  SQLType: TZSQLType;
  TempBlob: IZBlob;
begin
  case Value.VType of
    vtBoolean: SQLType := stBoolean;
    vtInteger: SQLType := stLong;
    vtUInteger: SQLType := stULong;
    vtFloat: SQLType := stBigDecimal;
    vtUnicodeString: SQLType := stUnicodeString;
    vtDateTime: SQLType := stTimestamp;
    vtBytes: SQLType := stBytes;
    vtArray: SQLType := TZSQLType(Value.VArray.VArrayType);
    vtInterface:
      if Supports(Value.VInterface, IZBlob, TempBlob) then
        if TempBlob.IsClob then
          SQLType := stAsciiStream
        else
          SQLType := stBinaryStream
      else
        SQLType := stString; //???
  else
    SQLType := stString;
  end;
  SetInParam(ParameterIndex, SQLType, Value);
end;

{**
  Sets the designated parameter to a <code>T???DynArray</code> value.
  The driver converts this to an SQL <code>Array of X</code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param x the TZSQLType
  @param x the parameter value
}
procedure TZAbstractPreparedStatement.SetNullArray(ParameterIndex: Integer;
  const SQLType: TZSQLType; const Value; const VariantType: TZVariantType = vtNull);
begin
  if InParamCount < ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF} then
    raise Exception.Create('Set Array-Value first');
  {$IFNDEF GENERIC_INDEX}
  ParameterIndex := ParameterIndex -1;
  {$ENDIF}
  if InParamValues[ParameterIndex].VType <> vtArray then
    raise Exception.Create('No Array bound before!');
  InParamValues[ParameterIndex].VArray.VIsNullArray := Pointer(Value);
  InParamValues[ParameterIndex].VArray.VIsNullArrayType := Ord(SQLType);
  InParamValues[ParameterIndex].VArray.VIsNullArrayVariantType := VariantType;
end;

{**
  Sets the designated parameter to a <code>Array of ???</code> value.
  The driver converts this to an SQL <code>Array of </code> value
  when it sends it to the database.

  @param parameterIndex the first parameter is 1, the second is 2, ...
  @param A dynamic array of X.
  @param SQLType the TZSQLType of the value
  @param VariantType the TZVariantType SubType of the value
}
procedure TZAbstractPreparedStatement.SetDataArray(ParameterIndex: Integer;
  const Value; const SQLType: TZSQLType; const VariantType: TZVariantType = vtNull);
var
  V: TZVariant;
  {using mem entry of ZData is faster then casting and save imbelievable many codelines for all possible types!}
  ZArray: Pointer absolute Value;

  procedure AssertLength;
  var Len: ArrayLenInt;
  begin
    Len := {%H-}PArrayLenInt({%H-}NativeUInt(ZArray) - ArrayLenOffSet)^{$IFDEF FPC}+1{$ENDIF}; //FPC returns High() for this pointer location
    if (ParameterIndex = FirstDbcIndex) or ((ParameterIndex > FirstDbcIndex) and
       (InParamValues[ParameterIndex{$IFNDEF GENERIC_INDEX} - 2{$ELSE}-1{$ENDIF}].VArray.VArray = nil))  then
      FInitialArrayCount := Len
    else
      if Len <> FInitialArrayCount then
        raise Exception.Create('Array count does not equal with initial count!');
  end;
begin
  if Connection.GetMetadata.GetDatabaseInfo.SupportsArrayBindings then
  begin
    if ZArray <> nil then
      case SQLType of
        stUnknown: raise Exception.Create('Invalid SQLType for Array binding!');
        stBoolean, stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong,
        stLong, stFloat, stDouble, stCurrency, stBigDecimal, stBytes, stGUID, stDate,
        stTime, stTimestamp, stAsciiStream, stUnicodeStream, stBinaryStream:
          AssertLength;
        stString:
          case VariantType of
            vtString, {$IFNDEF NO_ANSISTRING}vtAnsiString, {$ENDIF}vtUTF8String, vtRawByteString, vtCharRec:
              AssertLength
            else
              raise Exception.Create('Invalid Variant-Type for String-Array binding!');
          end;
        stUnicodeString:
          case VariantType of
            vtUnicodeString, vtCharRec:
              AssertLength
            else
              raise Exception.Create('Invalid Variant-Type for String-Array binding!');
          end;
        stArray:          raise Exception.Create('Invalid SQL-Type for Array binding!');
        stDataSet: ;
      end;
    V.VType := vtArray;
    V.VArray.VArray := Pointer(Value);
    V.VArray.VArrayVariantType := VariantType;
    V.VArray.VArrayType := Ord(SQLType);
    V.VArray.VIsNullArray := nil;
    V.VArray.VIsNullArrayType := 0;
    V.VArray.VIsNullArrayVariantType := vtNull;
    SetInParam(ParameterIndex, SQLType, V);
  end
  else
    raise EZSQLException.Create('ArrayBindings are not supported!');
end;

{**
  Clears the current parameter values immediately.
  <P>In general, parameter values remain in force for repeated use of a
  statement. Setting a parameter value automatically clears its
  previous value.  However, in some cases it is useful to immediately
  release the resources used by the current parameter values; this can
  be done by calling the method <code>clearParameters</code>.
}
procedure TZAbstractPreparedStatement.ClearParameters;
var
  I: Integer;
begin
  for I := FirstDbcIndex to FInParamCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    SetInParam(I, stUnknown, NullVariant);
    SetDefaultValue(I, '');
  end;
  SetInParamCount(0);
  FInitialArrayCount := 0;
end;

{**
  Executes any kind of SQL statement.
  Some prepared statements return multiple results; the <code>execute</code>
  method handles these complex statements as well as the simpler
  form of statements handled by the methods <code>executeQuery</code>
  and <code>executeUpdate</code>.
  @see Statement#execute
}
function TZAbstractPreparedStatement.ExecutePrepared: Boolean;
begin
  Result := False;
  { Logging Execution }
  DriverManager.LogMessage(lcExecPrepStmt,Self);
end;

procedure TZAbstractPreparedStatement.BeforeClose;
begin
  inherited BeforeClose;
    if Prepared then
      Unprepare;
end;

function TZAbstractPreparedStatement.GetSQL: String;
begin
  Result := {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF};
end;

procedure TZAbstractPreparedStatement.Prepare;
begin
  DriverManager.LogMessage(lcPrepStmt,Self);
  PrepareInParameters;
  FPrepared := True;
end;

procedure TZAbstractPreparedStatement.Unprepare;
begin
  if Assigned(FOpenResultSet) then
  begin
    IZResultSet(FOpenResultSet).Close;
    FOpenResultSet := nil;
  end;
  LastResultSet := nil;
  UnPrepareInParameters;
  FPrepared := False;
  Self.FInitialArrayCount := 0;
  SetLength(FCachedQueryRaw, 0);
  SetLength(FCachedQueryUni, 0);
end;

function TZAbstractPreparedStatement.IsPrepared: Boolean;
begin
  Result := FPrepared;
end;

{**
  Adds a set of parameters to this <code>PreparedStatement</code>
  object's batch of commands.
  @see Statement#addBatch
}
procedure TZAbstractPreparedStatement.AddBatchPrepared;
begin
  RaiseUnsupportedException;
end;

{**
  Gets the number, types and properties of a <code>ResultSet</code>
  object's columns.
  @return the description of a <code>ResultSet</code> object's columns
}
function TZAbstractPreparedStatement.GetMetaData: IZResultSetMetaData;
begin
  Result := nil;
  RaiseUnsupportedException;
end;

function TZAbstractPreparedStatement.GetRawEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): RawByteString;
var I: Integer;
begin
  if Length(FCachedQueryRaw) = 0 then
  begin
    {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := SQL;
    FCachedQueryRaw := ZDbcUtils.TokenizeSQLQueryRaw({$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF}, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected, GetCompareFirstKeywordStrings, @FIsPraparable);

    Result := EmptyRaw; //init Result
    for I := 0 to High(FCachedQueryRaw) do
      Result := Result + FCachedQueryRaw[i];
  end
  else
    Result := Inherited GetRawEncodedSQL(SQL);
end;

function TZAbstractPreparedStatement.GetUnicodeEncodedSQL(const SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}): ZWideString;
var I: Integer;
begin
  if Length(FCachedQueryUni) = 0 then
  begin
    {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF} := SQL;
    FCachedQueryUni := ZDbcUtils.TokenizeSQLQueryUni({$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF}, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected, GetCompareFirstKeywordStrings, @FIsPraparable);

    Result := ''; //init Result
    for I := 0 to High(FCachedQueryUni) do
      Result := Result + FCachedQueryUni[i];
  end
  else
    Result := inherited GetUnicodeEncodedSQL(SQL);
end;

function TZAbstractPreparedStatement.CreateLogEvent(
  const Category: TZLoggingCategory): TZLoggingEvent;
var
  I : integer;
  LogString : RawByteString;
begin
  LogString := '';
  case Category of
    lcBindPrepStmt:
        if InParamCount = 0 then
          result := nil
        else
          begin { Prepare Log Output}
            For I := 0 to InParamCount - 1 do
              LogString := LogString + GetInParamLogValue(InParamValues[I])+',';
            result := CreateStmtLogEvent(Category, Logstring);
          end;
  else
    result := inherited CreatelogEvent(Category);
  end;
end;

procedure TZAbstractPreparedStatement.SetASQL(const Value: RawByteString);
begin
  if ( ASQL <> Value ) then begin
    SetLength(FCachedQueryRaw, 0);
    SetLength(FCachedQueryUni, 0);
    if Prepared then
      Unprepare;
    inherited SetASQL(Value);
  end;
end;

procedure TZAbstractPreparedStatement.SetWSQL(const Value: ZWideString);
begin
  if ( WSQL <> Value ) then begin
    SetLength(FCachedQueryRaw, 0);
    SetLength(FCachedQueryUni, 0);
    if Prepared then
      Unprepare;
    inherited SetWSQL(Value);
  end;
end;

function TZAbstractPreparedStatement.GetOmitComments: Boolean;
begin
  Result := False;
end;

function TZAbstractPreparedStatement.GetCompareFirstKeywordStrings: TPreparablePrefixTokens;
begin
  Result := nil;
end;

{ TZAbstractCallableStatement }

{**
  Constructs this object and assigns main properties.
  @param Connection a database connection object.
  @param Sql a prepared Sql statement.
  @param Info a statement parameters.
}
constructor TZAbstractCallableStatement.Create(const Connection: IZConnection;
  const SQL: string; Info: TStrings);
begin
  inherited Create(Connection, SQL, Info);
  FSQL := SQL;
  FOutParamCount := 0;
  SetOutParamCount(0);
  FProcSql := EmptyRaw; //Init -> FPC
  FLastWasNull := True;
  FResultSets := TZCollection.Create;
  FIsFunction := False;
end;

{**
  Close and release a list of returned resultsets.
}
procedure TZAbstractCallableStatement.ClearResultSets;
var
  I: Integer;
  RS: IZResultSet;
begin
  for i := 0 to FResultSets.Count -1 do
    if Supports(FResultSets[i], IZResultSet, RS) then //possible IZUpdateCount e.g. DBLib, ASA
      RS.Close;
  FResultSets.Clear;
  LastResultSet := nil;
end;

{**
   Function remove stUnknown and ptResult, ptOutput paramters from
   InParamTypes and InParamValues because the out-params are added after
   fetching.
}
procedure TZAbstractCallableStatement.TrimInParameters;
var
  I: integer;
  ParamValues: TZVariantDynArray;
  ParamTypes: TZSQLTypeArray;
  ParamCount: Integer;
begin
  ParamCount := 0;
  SetLength(ParamValues, InParamCount);
  SetLength(ParamTypes, InParamCount);

  {Need for dbc access, where no metadata is used to register the ParamTypes}
  if Length(FDBParamTypes) < InParamCount then
    SetLength(FDBParamTypes, InParamCount);
  {end for dbc access}

  for I := 0 to High(InParamTypes) do
  begin
    if ( InParamTypes[I] = ZDbcIntfs.stUnknown ) then
      Continue;
    if Ord(FDBParamTypes[i]) > Ord(pctInOut) then //[ptResult, ptOutput]
      continue; //EgonHugeist: Ignore known OutParams! else StatmentInparamCount <> expect ProcedureParamCount
    ParamTypes[ParamCount] := InParamTypes[I];
    ParamValues[ParamCount] := InParamValues[I];
    Inc(ParamCount);
  end;
  if ParamCount = InParamCount then
    Exit;
  InParamTypes := ParamTypes;
  InParamValues := ParamValues;
  SetInParamCount(ParamCount); //AVZ
end;

{**
  Sets a new parameter count and initializes the buffers.
  @param NewParamCount a new parameters count.
}
procedure TZAbstractCallableStatement.SetOutParamCount(NewParamCount: Integer);
var
  I: Integer;
begin
  SetLength(FOutParamValues, NewParamCount);
  SetLength(FOutParamTypes, NewParamCount);
  SetLength(FNCharDetected, NewParamCount);
  for I := FOutParamCount to NewParamCount - 1 do
  begin
    FOutParamValues[I] := NullVariant;
    FOutParamTypes[I] := stUnknown;
  end;
  FOutParamCount := NewParamCount;
end;

{**
  Clears the current parameter values immediately.
  <P>In general, parameter values remain in force for repeated use of a
  statement. Setting a parameter value automatically clears its
  previous value.  However, in some cases it is useful to immediately
  release the resources used by the current parameter values; this can
  be done by calling the method <code>clearParameters</code>.
}
procedure TZAbstractCallableStatement.ClearParameters;
var
  I: Integer;
begin
  inherited;
  for I := 0 to FOutParamCount-1 do
  begin
    OutParamValues[I] := NullVariant;
    OutParamTypes[I] := stUnknown;
  end;
  SetOutParamCount(0);
end;

{**
  Releases this <code>Statement</code> object's database
  and JDBC resources immediately instead of waiting for
  this to happen when it is automatically closed.
  It is generally good practice to release resources as soon as
  you are finished with them to avoid tying up database
  resources.
  <P><B>Note:</B> A <code>Statement</code> object is automatically closed when it is
  garbage collected. When a <code>Statement</code> object is closed, its current
  <code>ResultSet</code> object, if one exists, is also closed.
}
procedure TZAbstractCallableStatement.BeforeClose;
begin
  ClearResultSets;
  inherited BeforeClose;
end;


{**
  Do we call a function or a procedure?
  @result Returns <code>True</code> if we call a function
}
function TZAbstractCallableStatement.IsFunction: Boolean;
begin
  Result := FIsFunction;
end;

{**
  Do we have ptInputOutput or ptOutput paramets in a function or procedure?
  @result Returns <code>True</code> if ptInputOutput or ptOutput is available
}
function TZAbstractCallableStatement.HasOutParameter: Boolean;
begin
  Result := FHasOutParameter;
end;

{**
  Get the first resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetFirstResultSet: IZResultSet;
begin
  Result := nil;
end;

{**
  Get the previous resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetPreviousResultSet: IZResultSet;
begin
  Result := nil;
end;

{**
  Get the next resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetNextResultSet: IZResultSet;
begin
  Result := nil;
end;

{**
  Get the last resultset..
  @result <code>IZResultSet</code> if supported
}
function TZAbstractCallableStatement.GetLastResultSet: IZResultSet;
begin
  Result := nil;
end;

{**
  First ResultSet?
  @result <code>True</code> if first ResultSet
}
function TZAbstractCallableStatement.BOR: Boolean;
begin
  Result := True;
end;

{**
  Last ResultSet?
  @result <code>True</code> if Last ResultSet
}
function TZAbstractCallableStatement.EOR: Boolean;
begin
  Result := True;
end;

{**
  Retrieves a ResultSet by his index.
  @param Index the index of the Resultset
  @result <code>IZResultSet</code> of the Index or nil.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // encoding unknown - parameter not used intentionally
function TZAbstractCallableStatement.GetResultSetByIndex(const Index: Integer): IZResultSet;
begin
  Result := nil;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Returns the Count of retrived ResultSets.
  @result <code>Integer</code> Count
}
function TZAbstractCallableStatement.GetResultSetCount: Integer;
begin
  Result := 0;
end;

{**
  Registers the OUT parameter in ordinal position
  <code>parameterIndex</code> to the JDBC type
  <code>sqlType</code>.  All OUT parameters must be registered
  before a stored procedure is executed.
  <p>
  The JDBC type specified by <code>sqlType</code> for an OUT
  parameter determines the Java type that must be used
  in the <code>get</code> method to read the value of that parameter.
  <p>
  If the JDBC type expected to be returned to this output parameter
  is specific to this particular database, <code>sqlType</code>
  should be <code>java.sql.Types.OTHER</code>.  The method retrieves the value.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @param sqlType the JDBC type code defined by <code>java.sql.Types</code>.
  If the parameter is of JDBC type <code>NUMERIC</code>
  or <code>DECIMAL</code>, the version of
  <code>registerOutParameter</code> that accepts a scale value should be used.
}
procedure TZAbstractCallableStatement.RegisterOutParameter(ParameterIndex,
  SQLType: Integer);
begin
  SetOutParamCount(ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});
  OutParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TZSQLType(SQLType);
end;

procedure TZAbstractCallableStatement.RegisterParamType(ParameterIndex,
  ParamType: Integer);
begin
  if ({$IFDEF GENERIC_INDEX}High{$ELSE}Length{$ENDIF}(FDBParamTypes) < ParameterIndex) then
    SetLength(FDBParamTypes, ParameterIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF});

  FDBParamTypes[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}] := TZProcedureColumnType(ParamType);
  if not FIsFunction then FIsFunction := ParamType = Ord(pctReturn);
  if not FHasOutParameter then FHasOutParameter := TZProcedureColumnType(ParamType) in [pctOut,pctInOut]; //ptOutput, ptInputOutput
end;

{**
  Gets a output parameter value by it's index.
  @param ParameterIndex a parameter index.
  @returns a parameter value.
}
function TZAbstractCallableStatement.GetOutParam(
  ParameterIndex: Integer): TZVariant;
begin
  if Assigned(OutParamValues) then
  begin
    Result := OutParamValues[ParameterIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
    FLastWasNull := ClientVarManager.IsNull(Result);
  end
  else
  begin
    Result := NullVariant;
    FLastWasNull := True;
  end;
end;

procedure TZAbstractCallableStatement.SetProcSQL(const Value: RawByteString);
begin
  FProcSql := Value;
end;

{**
  Indicates whether or not the last OUT parameter read had the value of
  SQL <code>NULL</code>.  Note that this method should be called only after
  calling a <code>getXXX</code> method; otherwise, there is no value to use in
  determining whether it is <code>null</code> or not.
  @return <code>true</code> if the last parameter read was SQL
  <code>NULL</code>; <code>false</code> otherwise
}
function TZAbstractCallableStatement.WasNull: Boolean;
begin
  Result := FLastWasNull;
end;

{**
  Indicates whether or not the specified OUT parameter read had the value of
  SQL <code>NULL</code>.
  @return <code>true</code> if the parameter read was SQL
  <code>NULL</code>; <code>false</code> otherwise
}
function TZAbstractCallableStatement.IsNull(ParameterIndex: Integer): Boolean;
begin
  GetOutParam(ParameterIndex);
  Result := FLastWasNull;
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>String</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetPChar(ParameterIndex: Integer): PChar;
begin
  FTemp := GetString(ParameterIndex);
  Result := PChar(FTemp);
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>String</code> object
  returned is ControlsCodePage encoded value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetString(ParameterIndex: Integer): String;
begin
  Result := ClientVarManager.GetAsString(GetOutParam(ParameterIndex));
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>AsniString</code> object
  returned is a Ansi(CP_GETACP) encoded value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
{$IFNDEF NO_ANSISTRING}
function TZAbstractCallableStatement.GetAnsiString(ParameterIndex: Integer): AnsiString;
begin
  Result := ClientVarManager.GetAsAnsiString(GetOutParam(ParameterIndex));
end;
{$ENDIF NO_ANSISTRING}

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>UTF8String</code> object
  returned is a UTF8 encoded value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
{$IFNDEF NO_UTF8STRING}
function TZAbstractCallableStatement.GetUTF8String(ParameterIndex: Integer): UTF8String;
begin
  Result := ClientVarManager.GetAsUTF8String(GetOutParam(ParameterIndex));
end;
{$ENDIF}

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>RawByteString</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetRawByteString(ParameterIndex: Integer): RawByteString;
begin
  Result := ClientVarManager.GetAsRawByteString(GetOutParam(ParameterIndex));
end;

{**
  Retrieves the value of a JDBC <code>CHAR</code>, <code>VARCHAR</code>,
  or <code>LONGVARCHAR</code> parameter as a <code>String</code> in
  the Java programming language.
  <p>
  For the fixed-length type JDBC <code>CHAR</code>,
  the <code>WideString</code> object
  returned has exactly the same value the JDBC
  <code>CHAR</code> value had in the
  database, including any padding added by the database.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value. If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
  @exception SQLException if a database access error occurs
}
function TZAbstractCallableStatement.GetUnicodeString(
  ParameterIndex: Integer): ZWideString;
begin
  Result := ClientVarManager.GetAsUnicodeString(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>BIT</code> parameter as a <code>boolean</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>false</code>.
}
function TZAbstractCallableStatement.GetBoolean(ParameterIndex: Integer): Boolean;
begin
  Result := SoftvarManager.GetAsBoolean(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>TINYINT</code> parameter as a <code>byte</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetByte(ParameterIndex: Integer): Byte;
begin
  Result := Byte(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>SHORTINT</code> parameter as a <code>short</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetShort(ParameterIndex: Integer): ShortInt;
begin
  Result := ShortInt(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>SMALLINT</code> parameter as a <code>word</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetWord(ParameterIndex: Integer): Word;
begin
  Result := Word(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>SMALLINT</code> parameter as a <code>small</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetSmall(ParameterIndex: Integer): SmallInt;
begin
  Result := SmallInt(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>INTEGER</code> parameter as an <code>uint</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetUInt(ParameterIndex: Integer): Cardinal;
begin
  Result := Cardinal(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>INTEGER</code> parameter as an <code>int</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetInt(ParameterIndex: Integer): Integer;
begin
  Result := Integer(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>ulong</code> parameter as a <code>long</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetULong(ParameterIndex: Integer): UInt64;
begin
  Result := UInt64(ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex)));
end;

{**
  Gets the value of a JDBC <code>BIGINT</code> parameter as a <code>long</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetLong(ParameterIndex: Integer): Int64;
begin
  Result := ClientVarManager.GetAsInteger(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>FLOAT</code> parameter as a <code>float</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetFloat(ParameterIndex: Integer): Single;
begin
  Result := ClientVarManager.GetAsFloat(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>DOUBLE</code> parameter as a <code>double</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetDouble(ParameterIndex: Integer): Double;
begin
  Result := ClientVarManager.GetAsFloat(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>CURRENCY</code> parameter as a <code>double</code>
  in the Java programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is 0.
}
function TZAbstractCallableStatement.GetCurrency(ParameterIndex: Integer): Currency;
begin
  Result := ClientVarManager.GetAsFloat(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>NUMERIC</code> parameter as a
  <code>java.math.BigDecimal</code> object with scale digits to
  the right of the decimal point.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result is
  <code>null</code>.
}
function TZAbstractCallableStatement.GetBigDecimal(ParameterIndex: Integer):
  Extended;
begin
  Result := ClientVarManager.GetAsFloat(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>BINARY</code> or <code>VARBINARY</code>
  parameter as an array of <code>byte</code> values in the Java
  programming language.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result is
   <code>null</code>.
}
function TZAbstractCallableStatement.GetBytes(ParameterIndex: Integer):
  TBytes;
begin
  Result := ClientVarManager.GetAsBytes(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>DATE</code> parameter as a
  <code>java.sql.Date</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement.GetDate(ParameterIndex: Integer):
  TDateTime;
begin
  Result := ClientVarManager.GetAsDateTime(GetOutParam(ParameterIndex));
end;

{**
  Get the value of a JDBC <code>TIME</code> parameter as a
  <code>java.sql.Time</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement.GetTime(ParameterIndex: Integer):
  TDateTime;
begin
  Result := ClientVarManager.GetAsDateTime(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>TIMESTAMP</code> parameter as a
  <code>java.sql.Timestamp</code> object.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>, the result
  is <code>null</code>.
}
function TZAbstractCallableStatement.GetTimestamp(ParameterIndex: Integer):
  TDateTime;
begin
  Result := ClientVarManager.GetAsDateTime(GetOutParam(ParameterIndex));
end;

{**
  Gets the value of a JDBC <code>Variant</code> parameter value.
  @param parameterIndex the first parameter is 1, the second is 2,
  and so on
  @return the parameter value.  If the value is SQL <code>NULL</code>,
  the result is <code>null</code>.
}
function TZAbstractCallableStatement.GetValue(ParameterIndex: Integer):
  TZVariant;
begin
  Result := GetOutParam(ParameterIndex);
end;

{ TZAbstractPreparedCallableStatement }

procedure TZAbstractPreparedCallableStatement.SetProcSQL(const Value: RawByteString);
begin
  if Value <> ProcSQL then Unprepare;
  inherited SetProcSQL(Value);
  if (Value <> EmptyRaw) and ( not Prepared ) then Prepare;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractPreparedCallableStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
begin
  if (SQL <> Self.WSQL) and (Prepared) then Unprepare;
  WSQL := SQL;
  Result := ExecuteQueryPrepared;
end;

function TZAbstractPreparedCallableStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  if (SQL <> Self.ASQL) and (Prepared) then Unprepare;
  Self.ASQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZAbstractPreparedCallableStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
begin
  if (SQL <> WSQL) and (Prepared) then Unprepare;
  WSQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

function TZAbstractPreparedCallableStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  if (SQL <> ASQL) and (Prepared) then Unprepare;
  ASQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}

function TZAbstractPreparedCallableStatement.Execute(const SQL: ZWideString): Boolean;
begin
  if (SQL <> WSQL) and (Prepared) then Unprepare;
  WSQL := SQL;
  Result := ExecutePrepared;
end;

function TZAbstractPreparedCallableStatement.Execute(const SQL: RawByteString): Boolean;
begin
  if (SQL <> ASQL) and (Prepared) then Unprepare;
  ASQL := SQL;
  Result := ExecutePrepared;
end;

{ TZEmulatedPreparedStatement }

{**
  Sets a reference to the last statement.
  @param LastStatement the last statement interface.
}
procedure TZEmulatedPreparedStatement.SetLastStatement(
  const LastStatement: IZStatement);
begin
  if FLastStatement <> nil then
    FLastStatement.Close;
  FLastStatement := LastStatement;
end;

function TZEmulatedPreparedStatement.PrepareWideSQLParam(ParamIndex: Integer): ZWideString;
begin
  Result := '';
end;

function TZEmulatedPreparedStatement.PrepareAnsiSQLParam(ParamIndex: Integer): RawByteString;
begin
  Result := '';
end;

{**
  Creates a temporary statement which executes queries.
  @param Info a statement parameters.
  @return a created statement object.
}
function TZEmulatedPreparedStatement.GetExecStatement: IZStatement;
begin
  if ExecStatement = nil then
    ExecStatement := CreateExecStatement;
  if ExecStatement <> nil then //set new options if required
  begin
    ExecStatement.SetMaxFieldSize(GetMaxFieldSize);
    ExecStatement.SetMaxRows(GetMaxRows);
    ExecStatement.SetEscapeProcessing(EscapeProcessing);
    ExecStatement.SetQueryTimeout(GetQueryTimeout);
    ExecStatement.SetCursorName(String(CursorName));

    ExecStatement.SetFetchDirection(GetFetchDirection);
    ExecStatement.SetFetchSize(GetFetchSize);
    ExecStatement.SetResultSetConcurrency(GetResultSetConcurrency);
    ExecStatement.SetResultSetType(GetResultSetType);
  end;
  Result := ExecStatement;
end;

{**
  Splits a SQL query into a list of sections.
  @returns a list of splitted sections.
}
procedure TZEmulatedPreparedStatement.TokenizeSQLQueryRaw;
begin
  if Length(FCachedQueryRaw) = 0 then
    FCachedQueryRaw := ZDbcUtils.TokenizeSQLQueryRaw(
        {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF}, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected,
      GetCompareFirstKeywordStrings, @FIsPraparable, FNeedNCharDetection);
end;

{**
  Splits a SQL query into a list of sections.
  @returns a list of splitted sections.
}
procedure TZEmulatedPreparedStatement.TokenizeSQLQueryUni;
begin
  if Length(FCachedQueryUni) = 0 then
    FCachedQueryUni := ZDbcUtils.TokenizeSQLQueryUni(
        {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF}, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected,
      GetCompareFirstKeywordStrings, @FIsPraparable, FNeedNCharDetection);
end;

{**
  Prepares an SQL statement and inserts all data values.
  @return a prepared SQL statement.
}
function TZEmulatedPreparedStatement.PrepareWideSQLQuery: ZWideString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  TokenizeSQLQueryUni;

  for I := 0 to High(FCachedQueryUni) do
    if FIsParamIndex[i] then begin
      ToBuff(PrepareWideSQLParam(ParamIndex), Result);
      Inc(ParamIndex);
    end else
      ToBuff(FCachedQueryUni[I], Result);
  FlushBuff(Result);
end;

{**
  Prepares an SQL statement and inserts all data values.
  @return a prepared SQL statement.
}
function TZEmulatedPreparedStatement.PrepareAnsiSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  TokenizeSQLQueryRaw;

  for I := 0 to High(FCachedQueryRaw) do
    if IsParamIndex[i] then begin
      ToBuff(PrepareAnsiSQLParam(ParamIndex), Result);
      Inc(ParamIndex);
    end else
      ToBuff(FCachedQueryRaw[I], Result);
  FlushBuff(Result);
end;

{**
  Closes this statement and frees all resources.
}
procedure TZEmulatedPreparedStatement.BeforeClose;
begin
  inherited BeforeClose;
  if LastStatement <> nil then
  begin
    FLastStatement.Close;
    FLastStatement := nil;
  end;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
}
function TZEmulatedPreparedStatement.Execute(const SQL: ZWideString): Boolean;
begin
  LastStatement := GetExecStatement;
  Result := LastStatement.Execute(SQL);
  if Result then
    LastResultSet := LastStatement.GetResultSet
  else
    LastUpdateCount := LastStatement.GetUpdateCount;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
}
function TZEmulatedPreparedStatement.Execute(const SQL: RawByteString): Boolean;
begin
  LastStatement := GetExecStatement;
  Result := LastStatement.Execute(SQL);
  if Result then
    LastResultSet := LastStatement.GetResultSet
  else
    LastUpdateCount := LastStatement.GetUpdateCount;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZEmulatedPreparedStatement.ExecuteQuery(const SQL: ZWideString): IZResultSet;
begin
  Result := GetExecStatement.ExecuteQuery(SQL);
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZEmulatedPreparedStatement.ExecuteQuery(const SQL: RawByteString): IZResultSet;
begin
  Result := GetExecStatement.ExecuteQuery(SQL);
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZEmulatedPreparedStatement.ExecuteUpdate(const SQL: ZWideString): Integer;
begin
  Result := GetExecStatement.ExecuteUpdate(SQL);
  LastUpdateCount := Result;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZEmulatedPreparedStatement.ExecuteUpdate(const SQL: RawByteString): Integer;
begin
  Result := GetExecStatement.ExecuteUpdate(SQL);
  LastUpdateCount := Result;
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZEmulatedPreparedStatement.ExecutePrepared: Boolean;
begin
  if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then
    Result := Execute(PrepareWideSQLQuery)
  else
    Result := Execute(PrepareAnsiSQLQuery);
end;

function TZEmulatedPreparedStatement.CreateLogEvent(
  const Category: TZLoggingCategory): TZLoggingEvent;
begin
  Result:=nil; // All logic happens using non-prepared statements, so we don't need to log the 'empty' prepare, unprepare, ...
end;

{**
  Executes the SQL query in this <code>PreparedStatement</code> object
  and returns the result set generated by the query.

  @return a <code>ResultSet</code> object that contains the data produced by the
    query; never <code>null</code>
}
function TZEmulatedPreparedStatement.ExecuteQueryPrepared: IZResultSet;
begin
  if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then
    Result := ExecuteQuery(PrepareWideSQLQuery)
  else
    Result := ExecuteQuery(PrepareAnsiSQLQuery)
end;

{**
  Executes the SQL INSERT, UPDATE or DELETE statement
  in this <code>PreparedStatement</code> object.
  In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @return either the row count for INSERT, UPDATE or DELETE statements;
  or 0 for SQL statements that return nothing
}
function TZEmulatedPreparedStatement.ExecuteUpdatePrepared: Integer;
begin
  if ConSettings^.ClientCodePage^.Encoding = ceUTF16 then
    Result := ExecuteUpdate(PrepareWideSQLQuery)
  else
    Result := ExecuteUpdate(PrepareAnsiSQLQuery);
end;

{ TZEmulatedPreparedStatement_A }

function TZEmulatedPreparedStatement_A.ComposeRawSQLQuery: RawByteString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  TokenizeSQLQueryRaw;

  if Length(FCachedQueryRaw) = 1
  then Result := FCachedQueryRaw[0]
  else begin
    for I := 0 to High(FCachedQueryRaw) do
      if IsParamIndex[i] then begin
        ToBuff(GetParamAsString(ParamIndex), Result);
        Inc(ParamIndex);
      end else
        ToBuff(FCachedQueryRaw[I], Result);
    FlushBuff(Result);
  end;
end;

procedure TZEmulatedPreparedStatement_A.TokenizeSQLQueryRaw;
begin
  if Length(FCachedQueryRaw) = 0 then
    FCachedQueryRaw := ZDbcUtils.TokenizeSQLQueryRaw(
        {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF}, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected,
      GetCompareFirstKeywordStrings, @FIsPraparable, FNeedNCharDetection);
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{ TZEmulatedPreparedStatement_W }

function TZEmulatedPreparedStatement_W.ComposeWideSQLQuery: ZWideString;
var
  I: Integer;
  ParamIndex: Integer;
begin
  ParamIndex := 0;
  Result := '';
  TokenizeSQLQueryUni;
  if Length(FCachedQueryUni) = 1
  then Result := FCachedQueryUni[0]
  else begin
    for I := 0 to High(FCachedQueryUni) do
      if FIsParamIndex[i] then begin
        ToBuff(GetParamAsString(ParamIndex), Result);
        Inc(ParamIndex);
      end else
        ToBuff(FCachedQueryUni[I], Result);
    FlushBuff(Result);
  end;
end;

procedure TZEmulatedPreparedStatement_W.TokenizeSQLQueryUni;
begin
  if Length(FCachedQueryUni) = 0 then
    FCachedQueryUni := ZDbcUtils.TokenizeSQLQueryUni(
        {$IFDEF UNICODE}FWSQL{$ELSE}FASQL{$ENDIF}, ConSettings,
      Connection.GetDriver.GetTokenizer, FIsParamIndex, FNCharDetected,
      GetCompareFirstKeywordStrings, @FIsPraparable, FNeedNCharDetection);
end;

{ TZAbstractEmulatedPreparedStatement }

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement UCS2 encoded
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractEmulatedPreparedStatement.Execute(const SQL: ZWideString): Boolean;
begin
  WSQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes an SQL statement that may return multiple results.
  Under some (uncommon) situations a single SQL statement may return
  multiple result sets and/or update counts.  Normally you can ignore
  this unless you are (1) executing a stored procedure that you know may
  return multiple results or (2) you are dynamically executing an
  unknown SQL string.  The  methods <code>execute</code>,
  <code>getMoreResults</code>, <code>getResultSet</code>,
  and <code>getUpdateCount</code> let you navigate through multiple results.

  The <code>execute</code> method executes an SQL statement and indicates the
  form of the first result.  You can then use the methods
  <code>getResultSet</code> or <code>getUpdateCount</code>
  to retrieve the result, and <code>getMoreResults</code> to
  move to any subsequent result(s).

  @param sql any SQL statement raw encoded
  @return <code>true</code> if the next result is a <code>ResultSet</code> object;
  <code>false</code> if it is an update count or there are no more results
  @see #getResultSet
  @see #getUpdateCount
  @see #getMoreResults
}
function TZAbstractEmulatedPreparedStatement.Execute(
  const SQL: RawByteString): Boolean;
begin
  ASQL := SQL;
  Result := ExecutePrepared;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement UCS2 encoded
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractEmulatedPreparedStatement.ExecuteQuery(
  const SQL: RawByteString): IZResultSet;
begin
  ASQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes an SQL statement that returns a single <code>ResultSet</code> object.
  @param sql typically this is a static SQL <code>SELECT</code> statement raw encoded
  @return a <code>ResultSet</code> object that contains the data produced by the
    given query; never <code>null</code>
}
function TZAbstractEmulatedPreparedStatement.ExecuteQuery(
  const SQL: ZWideString): IZResultSet;
begin
  WSQL := SQL;
  Result := ExecuteQueryPrepared;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZAbstractEmulatedPreparedStatement.ExecuteUpdate(
  const SQL: RawByteString): Integer;
begin
  ASQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

{**
  Executes an SQL <code>INSERT</code>, <code>UPDATE</code> or
  <code>DELETE</code> statement. In addition,
  SQL statements that return nothing, such as SQL DDL statements,
  can be executed.

  @param sql an SQL <code>INSERT</code>, <code>UPDATE</code> or
    <code>DELETE</code> statement or an SQL statement that returns nothing
  @return either the row count for <code>INSERT</code>, <code>UPDATE</code>
    or <code>DELETE</code> statements, or 0 for SQL statements that return nothing
}
function TZAbstractEmulatedPreparedStatement.ExecuteUpdate(
  const SQL: ZWideString): Integer;
begin
  WSQL := SQL;
  Result := ExecuteUpdatePrepared;
end;

end.

