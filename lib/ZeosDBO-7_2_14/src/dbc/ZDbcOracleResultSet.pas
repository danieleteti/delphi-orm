{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Oracle Database Connectivity Classes        }
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

unit ZDbcOracleResultSet;

interface

{$I ZDbc.inc}
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  ZSysUtils, ZDbcIntfs, ZDbcOracle, ZDbcResultSet, ZPlainOracleDriver,
  ZDbcResultSetMetadata, ZDbcLogging, ZCompatibility, ZDbcOracleUtils,
  ZPlainOracleConstants;

type
  {** Implements Oracle ResultSet. }
  TZOracleAbstractResultSet = class(TZAbstractResultSet)
  private
    FStmtHandle: POCIStmt;
    FErrorHandle: POCIError;
    FConnectionHandle: POCIEnv;
    FContextHandle: POCISvcCtx;
    FPlainDriver: IZOraclePlainDriver;
    FConnection: IZOracleConnection;
    FColumns: PZSQLVars;
    FChunkSize: Integer;
    FIteration: Integer; //Max count of rows which fit into BufferSize <= FZBufferSize
    FCurrentRowBufIndex: Cardinal; //The current row in buffer! NOT the current row of RS
    FZBufferSize: Integer; //max size for multiple rows. If Row > Value ignore it!
    FRowsBuffer: TByteDynArray; //Buffer for multiple rows if possible which is reallocated or freed by IDE -> mem leak save!
    function GetSQLVarHolder(ColumnIndex: Integer): PZSQLVar; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetAsDateTimeValue(const SQLVarHolder: PZSQLVar): TDateTime; {$IFDEF WITH_INLINE}inline;{$ENDIF}
    function GetFinalObject(Obj: POCIObject): POCIObject;
  protected
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    procedure BeforeClose; override;
    constructor Create(const PlainDriver: IZOraclePlainDriver;
      const Statement: IZStatement; const SQL: string;
      const StmtHandle: POCIStmt; const ErrorHandle: POCIError;
      const ZBufferSize: Integer);

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer): PAnsiChar; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetDataSet(ColumnIndex: Integer): IZDataSet; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;
  end;

  TZOracleResultSet = class(TZOracleAbstractResultSet)
  private
    FMaxBufIndex: Integer;
  protected
    procedure Open; override;
  public
    procedure ResetCursor; override;
    procedure BeforeClose; override;
    function Next: Boolean; override;
  end;

  TZOracleCallableResultSet = Class(TZOracleAbstractResultSet)
  private
    FFieldNames: TStringDynArray;
    function PrepareOracleOutVars(const Params: PZSQLVars;
      const OracleParams: TZOracleParams): PZSQLVars;
  protected
    procedure Open; override;
  public
    constructor Create(const PlainDriver: IZOraclePlainDriver;
      const Statement: IZStatement; const SQL: string; StmtHandle: POCIStmt;
      ErrorHandle: POCIError; const OutParams: PZSQLVars; const OracleParams: TZOracleParams);
    procedure Close; override;
    function Next: Boolean; override;
  End;

  {** Represents an interface, specific for Oracle blobs. }
  IZOracleBlob = interface(IZBlob)
    ['{3D861AAC-B263-42F1-B359-2A188D1D986A}']
    procedure CreateBlob;
    procedure ReadLob;
    procedure WriteLob;
    procedure WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
  end;

  {** Implements external blob wrapper object for Oracle. }
  TZOracleBlob = class(TZAbstractBlob, IZOracleBlob)
  private
    FContextHandle: POCISvcCtx;
    FErrorHandle: POCIError;
    FLobLocator: POCILobLocator;
    FPlainDriver: IZOraclePlainDriver;
    FTemporary: Boolean;
    FChunkSize: Integer;
    FConSettings: PZConSettings;
  protected
    procedure InternalSetData(AData: Pointer; ASize: Integer);
  public
    constructor Create(const PlainDriver: IZOraclePlainDriver;
      const Data: Pointer; const Size: Int64; const ContextHandle: POCISvcCtx;
      const ErrorHandle: POCIError; const LobLocator: POCILobLocator;
      const ChunkSize: Integer; const ConSettings: PZConSettings);
    destructor Destroy; override;

    procedure CreateBlob;
    procedure ReadLob; //override;
    procedure WriteLob; //override;
    procedure WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
  end;

  {EH: my current uncached implementation doesn't work here since we've no
   scrollable RS}
  {** Implements external blob wrapper object for Oracle. }
  TZOracleClob = class(TZAbstractCLob, IZOracleBlob)
  private
    FContextHandle: POCISvcCtx;
    FErrorHandle: POCIError;
    FLobLocator: POCILobLocator;
    FConnectionHandle: POCIEnv;
    FPlainDriver: IZOraclePlainDriver;
    FTemporary: Boolean;
    FChunkSize: Integer;
  public
    constructor Create(const PlainDriver: IZOraclePlainDriver;
      const Data: Pointer; const Size: Cardinal; const ConnectionHandle: POCIEnv;
      const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
      const LobLocator: POCILobLocator; const ChunkSize: Integer;
      const ConSettings: PZConSettings; const CodePage: Word);
    destructor Destroy; override;

    procedure CreateBlob;
    procedure ReadLob; //override;
    procedure WriteLob; //override;
    procedure WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
  end;

{$ENDIF ZEOS_DISABLE_ORACLE}
implementation
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  Math, {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings,{$ENDIF} ZFastCode,
  ZMessages, ZEncoding, ZClasses;

{ TZOracleAbstractResultSet }

procedure TZOracleAbstractResultSet.BeforeClose;
begin
  FreeOracleSQLVars(FPlainDriver, FColumns, FIteration, FConnectionHandle,
    FErrorHandle, ConSettings);
  inherited BeforeClose;
end;

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a Oracle plain driver.
  @param Statement a related SQL statement object.
  @param SQL a SQL statement.
  @param Handle a Oracle specific query handle.
}
constructor TZOracleAbstractResultSet.Create(const PlainDriver: IZOraclePlainDriver;
  const Statement: IZStatement; const SQL: string; const StmtHandle: POCIStmt;
  const ErrorHandle: POCIError; const ZBufferSize: Integer);
begin
  inherited Create(Statement, SQL, nil, Statement.GetConnection.GetConSettings);

  FStmtHandle := StmtHandle;
  FErrorHandle := ErrorHandle;
  FPlainDriver := PlainDriver;
  ResultSetConcurrency := rcReadOnly;
  FConnection := Statement.GetConnection as IZOracleConnection;
  FConnectionHandle := FConnection.GetConnectionHandle;
  FContextHandle := FConnection.GetContextHandle;
  FChunkSize := Statement.GetChunkSize;
  FIteration := 1;
  FCurrentRowBufIndex := 0;
  FZBufferSize := ZBufferSize;
  Open;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZOracleAbstractResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
  if (ColumnIndex < FirstDbcIndex) or (ColumnIndex > FColumns^.AllocNum{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
{$ENDIF}
  {$R-}
  Result := FColumns^.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}].oIndicatorArray^[FCurrentRowBufIndex] < 0;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
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
function TZOracleAbstractResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUint): PAnsiChar;
var
  SQLVarHolder: PZSQLVar;
  Blob: IZBlob;
begin
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
  begin
    Len := 0;
    Result := nil;
  end
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          begin
            Len := oDataSize;
            Result := {%H-}Pointer({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            while (Result+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
          end;
        SQLT_INT:
          begin
            FRawTemp := IntToRaw({%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
            Result := Pointer(FRawTemp);
            Len := {$IFDEF WITH_INLINE}System.Length(FRawTemp){$ELSE}{%H-}PInteger(NativeUInt(FRawTemp) - 4)^{$ENDIF};
          end;
        SQLT_FLT:
          begin
            FRawTemp := FloatToSQLRaw({%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
            Result := Pointer(FRawTemp);
            Len := {$IFDEF WITH_INLINE}System.Length(FRawTemp){$ELSE}{%H-}PInteger(NativeUInt(FRawTemp) - 4)^{$ENDIF};
          end;
        SQLT_STR:
          begin
            Result := {%H-}Pointer({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            {$R-}
            Len := oDataSizeArray^[FCurrentRowBufIndex];
            {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
          end;
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          begin
            Result := {%H-}Pointer({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)+ SizeOf(Integer));
            Len := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
          end;
        SQLT_DAT, SQLT_TIMESTAMP:
          begin
            FRawTemp := ZSysUtils.DateTimeToRawSQLTimeStamp(GetAsDateTimeValue(SQLVarHolder),
              ConSettings^.ReadFormatSettings, False);
            Result := Pointer(FRawTemp);
            Len := NativeUInt({%H-}PLengthInt(NativeUInt(FRawTemp) - StringLenOffSet)^);
          end;
        SQLT_BLOB, SQLT_CLOB:
          begin
            Blob := GetBlob(ColumnIndex);
            Result := Blob.GetBuffer;
            Len := Blob.Length;
          end;
        else
          raise Exception.Create('Missing OCI Type?');
      end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var
  SQLVarHolder: PZSQLVar;
  Blob: IZBlob;
  Len: NativeUInt;
  P: PAnsiChar;
begin
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          begin
            Len := oDataSize;
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            while (P+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            Result := ConSettings^.ConvFuncs.ZPRawToUTF8(P, Len, ConSettings^.ClientCodePage^.CP)
          end;
        SQLT_INT: Result := IntToRaw({%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_UIN: Result := IntToRaw({%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_FLT: Result := FloatToSQLRaw({%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_STR:
          {$R-}
          Result := ConSettings^.ConvFuncs.ZPRawToUTF8(
            {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)),
            NativeUInt(oDataSizeArray^[FCurrentRowBufIndex]), ConSettings^.ClientCodePage^.CP);
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          ZSetString({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))+SizeOf(Integer),
            {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^, Result);
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := ZSysUtils.DateTimeToRawSQLTimeStamp(GetAsDateTimeValue(SQLVarHolder),
            ConSettings^.ReadFormatSettings, False);
        SQLT_BLOB:
          begin
            Blob := GetBlob(ColumnIndex);
            ZSetString(PAnsiChar(Blob.GetBuffer), Blob.Length, Result);
            Blob := nil;
          end;
        SQLT_CLOB:
          GetBlob(ColumnIndex).GetUTF8String;
        else
          raise Exception.Create('Missing OCI Type?');
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
function TZOracleAbstractResultSet.GetPAnsiChar(ColumnIndex: Integer): PAnsiChar;
var Len: NativeUInt;
begin
  Result := GetPAnsiChar(ColumnIndex, Len);
end;

{**
  Gets a holder for SQL output variable.
  @param ColumnIndex an index of the column to read.
  @returns an output variable holder or <code>nil</code> if column is empty.
}
function TZOracleAbstractResultSet.GetSQLVarHolder(ColumnIndex: Integer): PZSQLVar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckClosed;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
{$ENDIF}
  {$R-}
  Result := @FColumns^.Variables[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  LastWasNull := (Result^.oIndicatorArray[FCurrentRowBufIndex] < 0) or (Result.Data = nil);
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>DateTime</code>.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @param SQLVarHolder a reference to SQL variable holder or <code>nil</code>
    to force retrieving the variable.
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetAsDateTimeValue(const SQLVarHolder: PZSQLVar): TDateTime;
var
  Status: Integer;
  Year: SmallInt;
  yr, mnth, dy, hr, mm, ss, fsec: sb4;
  Month, Day: Byte;
  Hour, Minute, Second: Byte;
  Millis: ub4;
  P: PAnsiChar;
  Ptr: POraDate absolute P;
begin
  with SQLVarHolder^ do
    if TypeCode = SQLT_DAT then
    begin
      P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
      if (PInteger(P)^=0) and (PInteger(P+3)^=0) then //all 0 ?
        result := 0
      else
      begin
        Result := 0;
        if Ptr^.Cent <= 100 then // avoid TDateTime values < 0 (generates wrong DecodeTime) //thanks to ab of synopse
          result := 0
        else
          if ColType in [stDate, stTimestamp] then
            result := EncodeDate((Ptr^.Cent-100)*100+Ptr^.Year-100,Ptr^.Month,Ptr^.Day);
        if (ColType in [stTime, stTimestamp]) and ((Ptr^.Hour<>0) or (Ptr^.Min<>0) or (Ptr^.Sec<>0)) then
          result := result + EncodeTime(Ptr^.Hour-1,Ptr^.Min-1,Ptr^.Sec-1,0);
      end;
    end
    else
      case oDataType of
        SQLT_INTERVAL_DS:
          begin
            Status := FPlainDriver.IntervalGetDaySecond(FContextHandle, FErrorHandle, @dy, @hr, @mm, @ss, @fsec, {%H-}PPOCIDescriptor({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
            if (Status = OCI_SUCCESS) then
              Result := EncodeTime(hr, mm, ss, fsec div 100000)
            else
              Result := 0;
          end;
        SQLT_INTERVAL_YM:
          begin
            Status := FPlainDriver.IntervalGetYearMonth(FContextHandle, FErrorHandle, @yr, @mnth, {%H-}PPOCIDescriptor({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
            if (Status = OCI_SUCCESS) and (not (yr and mnth = 1)) then
              Result := EncodeDate(yr, mnth, 0)
            else Result := 0;
          end;
        else
        begin
          if ColType in [stDate, stTimestamp] then
          begin
            Status := FPlainDriver.DateTimeGetDate(FConnectionHandle, FErrorHandle,
              {%H-}PPOCIDescriptor({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^,
              Year{%H-}, Month{%H-}, Day{%H-});
          // attention : this code handles all timestamps on 01/01/0001 as a pure time value
          // reason : oracle doesn't have a pure time datatype so all time comparisons compare
          //          TDateTime values on 30 Dec 1899 against oracle timestamps on 01 januari 0001 (negative TDateTime)
            if (Status = OCI_SUCCESS) and (not ((Year=1) and (Month=1) and (Day=1))) then
              Result := EncodeDate(Year, Month, Day)
            else Result := 0;
          end
          else Result := 0;
          if ColType in [stTime, stTimestamp] then
          begin
            Status := FPlainDriver.DateTimeGetTime(FConnectionHandle, FErrorHandle,
              {%H-}PPOCIDescriptor({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^,
                Hour{%H-}, Minute{%H-}, Second{%H-}, Millis{%H-});
            if Status = OCI_SUCCESS then
              Result := Result + EncodeTime(
                Hour, Minute, Second, Millis div 1000000);
          end;
        end;
      end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  Len: Cardinal;
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Blob: IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stString);
{$ENDIF}
begin
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          begin
            Len := oDataSize;
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            while (P+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            ZSetString(P, Len, Result);
          end;
        SQLT_INT:
          Result := IntToRaw({%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_FLT:
          Result := FloatToSQLRaw({%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_STR:
          {$R-}
          ZSetString({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)), SQLVarHolder.oDataSizeArray[FCurrentRowBufIndex], Result);
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          ZSetString({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)) + SizeOf(Integer), {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^, Result);
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := ZSysUtils.DateTimeToRawSQLTimeStamp(GetAsDateTimeValue(SQLVarHolder),
            ConSettings^.ReadFormatSettings, False);
        SQLT_BLOB, SQLT_CLOB:
          begin
            Blob := GetBlob(ColumnIndex);
            Result := Blob.GetString;
          end;
        else
          raise Exception.Create('Missing OCI Type?');
      end;
  end;
end;

{**
  Gets the final object of a type/named-collection/nested-table,array

  @param obj the parent-object
  @return the Object which contains the final object descriptor
}
function TZOracleAbstractResultSet.GetFinalObject(Obj: POCIObject): POCIObject;
begin
  if Obj.is_final_type = 1 then
    Result := Obj
  else
    Result := GetFinalObject(Obj.next_subtype); //recursive call
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZOracleAbstractResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  LEn: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBoolean);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := False
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^ <> 0;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^ <> 0;
        SQLT_FLT:
          Result := Trunc({%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^) <> 0;
        SQLT_AFC:
          begin
            Len := oDataSize;
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            while (P+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            ZSetString(P, Len, FRawTemp);
            Result := StrToBoolEx(FRawTemp);
          end;
        SQLT_STR:
          Result := StrToBoolEx({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)), True, False);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := False;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(SQLVarHolder) <> 0;
        SQLT_BLOB, SQLT_CLOB:
          Result := StrToBoolEx(PAnsiChar(GetBlob(ColumnIndex).GetBuffer));
      else
        Result := False;
      end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  SQLVarHolder: PZSQLVar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stInteger);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          Result := RawToIntDef({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)), 0);
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_FLT:
          Result := Trunc({%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_STR:
          Result := RawToIntDef({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)), 0);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetAsDateTimeValue(SQLVarHolder));
        SQLT_BLOB, SQLT_CLOB:
          Result := RawToIntDef(GetBlob(ColumnIndex).GetBuffer, 0);
      else
        Result := 0;
      end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  SQLVarHolder: PZSQLVar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stLong);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          Result := RawToInt64Def({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)), 0);
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_FLT:
          Result := Trunc({%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_STR:
          Result := RawToInt64Def({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)), 0);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetAsDateTimeValue(SQLVarHolder));
        SQLT_BLOB, SQLT_CLOB:
          Result := RawToInt64Def(GetBlob(ColumnIndex).GetBuffer, 0);
      else
        Result := 0;
      end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UInt64</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZOracleAbstractResultSet.GetULong(ColumnIndex: Integer): UInt64;
var
  SQLVarHolder: PZSQLVar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stULong);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          Result := RawToUInt64Def({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)), 0);
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_FLT:
          Result := Trunc({%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_STR:
          Result := RawToUInt64Def({%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)), 0);
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(GetAsDateTimeValue(SQLVarHolder));
        SQLT_BLOB, SQLT_CLOB:
          Result := RawToUInt64Def(GetBlob(ColumnIndex).GetBuffer, 0);
      else
        Result := 0;
      end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stFloat);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          begin
            Len := oDataSize;
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            while (P+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            SqlStrToFloatDef(P, 0, Result, Len);
          end;
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_FLT:
          Result := {%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_STR:
           {$R-}
           SqlStrToFloatDef({%H-}PAnsichar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)),
            0, Result, oDataSizeArray^[FCurrentRowBufIndex]);
           {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(SQLVarHolder);
        SQLT_BLOB, SQLT_CLOB:
          SqlStrToFloatDef(PAnsiChar(GetBlob(ColumnIndex).GetBuffer), 0, Result);
      else
        Result := 0;
      end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZOracleAbstractResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDouble);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          begin
            Len := oDataSize;
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            while (P+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            SqlStrToFloatDef(P, 0, Result, Len);
          end;
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_FLT:
          Result := {%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_STR:
          {$R-}
          SqlStrToFloatDef({%H-}PAnsichar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)),
            0, Result, oDataSizeArray^[FCurrentRowBufIndex]);
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(SQLVarHolder);
        SQLT_BLOB, SQLT_CLOB:
          SqlStrToFloatDef(PAnsiChar(GetBlob(ColumnIndex).GetBuffer), 0, Result);
      else
        Result := 0;
      end;
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
function TZOracleAbstractResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBigDecimal);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          begin
            Len := oDataSize;
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            while (P+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            SqlStrToFloatDef(P, 0, Result, Len);
          end;
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_FLT:
          Result := {%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_STR:
          {$R-}
          SqlStrToFloatDef({%H-}PAnsichar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length)),
            0, Result, oDataSizeArray^[FCurrentRowBufIndex]);
          {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(SQLVarHolder);
        SQLT_BLOB, SQLT_CLOB:
          SqlStrToFloatDef(PAnsiChar(GetBlob(ColumnIndex).GetBuffer), 0, Result);
      else
        Result := 0;
      end;
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
function TZOracleAbstractResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stBytes);
{$ENDIF}
  Result := StrToBytes(InternalGetString(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Len: NativeUInt;
  Blob: IZBlob;
label ConvFromString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stDate);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          begin
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            Len := oDataSize;
            while (P+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            goto ConvFromString;
          end;
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_FLT:
          Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc({%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_STR:
          begin
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            {$R-}
            Len := oDataSizeArray^[FCurrentRowBufIndex];
            {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    ConvFromString:
            if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
              Result := RawSQLDateToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
            else
              Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
                RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed));
            LastWasNull := Result = 0;
          end;
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(SQLVarHolder);
        SQLT_BLOB, SQLT_CLOB:
          begin
            Blob := Getblob(ColumnIndex);
            P := Blob.GetBuffer;
            Len := Blob.Length;
            goto ConvFromString;
          end;
      else
        Result := 0;
      end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZOracleAbstractResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Len: NativeUInt;
  Failed: Boolean;
  Blob: IZBlob;
label ConvFromString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTime);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          begin
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            Len := oDataSize;
            while (P+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            goto ConvFromString;
          end;
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_FLT:
          Result := Frac({%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^);
        SQLT_STR:
          begin
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            {$R-}
            Len := oDataSizeArray^[FCurrentRowBufIndex];
            {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    ConvFromString:
            if (P+2)^ = ':' then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
            else
              Result := Frac(RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed));
            LastWasNull := Result = 0;
          end;
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(SQLVarHolder);
        SQLT_BLOB, SQLT_CLOB:
          begin
            Blob := Getblob(ColumnIndex);
            P := Blob.GetBuffer;
            Len := Blob.Length;
            goto ConvFromString;
          end;
      else
        Result := 0;
      end;
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
function TZOracleAbstractResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  SQLVarHolder: PZSQLVar;
  P: PAnsiChar;
  Len: NativeUInt;
  Blob: IZBlob;
label ConvFromString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckColumnConvertion(ColumnIndex, stTimeStamp);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
    with SQLVarHolder^ do
      case TypeCode of
        SQLT_AFC:
          begin
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            Len := oDataSize;
            while (P+Len-1)^ = ' ' do Dec(Len); //omit trailing spaces
            goto ConvFromString;
          end;
        SQLT_INT:
          Result := {%H-}PInteger({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_UIN:
          Result := {%H-}PCardinal({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_FLT:
          Result := {%H-}PDouble({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length))^;
        SQLT_STR:
          begin
            P := {%H-}PAnsiChar({%H-}NativeUInt(Data)+(FCurrentRowBufIndex*Length));
            {$R-}
            Len := oDataSizeArray^[FCurrentRowBufIndex];
            {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    ConvFromString:
            if (P+2)^ = ':' then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
            else
              if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Len) <= 4 then
                Result := RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
              else
                Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed);
            LastWasNull := Result = 0;
          end;
        SQLT_LVB, SQLT_LVC, SQLT_BIN:
          Result := 0;
        SQLT_DAT, SQLT_TIMESTAMP:
          Result := GetAsDateTimeValue(SQLVarHolder);
        SQLT_BLOB, SQLT_CLOB:
          begin
            Blob := Getblob(ColumnIndex);
            P := Blob.GetBuffer;
            Len := Blob.Length;
            goto ConvFromString;
          end;
      else
        Result := 0;
      end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>IZResultSet</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>IZResultSet</code> object representing the SQL
    <code>IZResultSet</code> value in the specified column
}
function TZOracleAbstractResultSet.GetDataSet(ColumnIndex: Integer): IZDataSet;
var
  CurrentVar: PZSQLVar;
  type_Ref: POCIRef;
  //tdo: POCIType;
begin
  Result := nil ;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
      Exit;

  CurrentVar := Self.GetSQLVarHolder(ColumnIndex);
  Result := nil;
  if CurrentVar.TypeCode = SQLT_NTY then
    {$R-}
    if CurrentVar.oIndicatorArray[FCurrentRowBufIndex] >= 0 then begin
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
      if CurrentVar._Obj.is_final_type = 1 then
        // here we've the final object lets's read it to test it
        // later we only need the reference-pointers to create a new dataset
      else
      begin
         //http://cpansearch.perl.org/src/TIMB/DBD-Oracle-1.26/oci8.c

        //create a temporary object
        type_ref := nil;
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.ObjectNew(FConnectionHandle,
            FConnection.GetErrorHandle, FContextHandle, OCI_TYPECODE_REF,
              nil, nil, OCI_DURATION_DEFAULT, TRUE, @type_ref),
          lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO', ConSettings);
        //Get the type reference
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.ObjectGetTypeRef(FConnectionHandle,
            FConnection.GetErrorHandle, CurrentVar._Obj.obj_value, type_Ref),
          lcOther, 'OCIObjectGetTypeRef(obj_value)', ConSettings);

        //Now let's get the new tdo
        //Excptions????????
        {CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.TypeByRef(FConnectionHandle,
            FConnection.GetErrorHandle, type_ref, OCI_DURATION_DEFAULT,
            OCI_TYPEGET_ALL, @tdo),
          lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO', ConSettings);}
        //free the temporary object
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.ObjectFree(FConnectionHandle,
            FConnection.GetErrorHandle, type_ref, ub2(0)),
          lcOther, 'ObjectFree()', ConSettings);
      end;


      {CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.ResultSetToStmt(CurrentVar^._Obj.obj_ind,
          FErrorHandle), lcOther, 'Nested Table to Stmt handle', ConSettings);
      Result := CreateOracleResultSet(FPlainDriver, GetStatement,
        'Fetch Nested Table', CurrentVar^._Obj.obj_ref, FErrorHandle)};
    end;
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZOracleAbstractResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  SQLVarHolder: PZSQLVar;
  LobLocator: POCILobLocator;
  Len: NativeUInt;
begin
  Result := nil ;
{$IFNDEF DISABLE_CHECKING}
  CheckBlobColumn(ColumnIndex);
{$ENDIF}
  SQLVarHolder := GetSQLVarHolder(ColumnIndex);
  if LastWasNull then
      Exit;
  with SQLVarHolder^ do
    if TypeCode in [SQLT_BLOB, SQLT_CLOB, SQLT_BFILEE, SQLT_CFILEE] then
    begin
      LobLocator := {%H-}PPOCIDescriptor({%H-}NativeUInt(Data)+FCurrentRowBufIndex*Length)^;
      if TypeCode in [SQLT_BLOB, SQLT_BFILEE] then
        Result := TZOracleBlob.Create(FPlainDriver, nil, 0, FContextHandle,
          FConnection.GetErrorHandle, LobLocator, FChunkSize, ConSettings)
      else
        Result := TZOracleClob.Create(FPlainDriver, nil, 0,
          FConnectionHandle, FContextHandle,
          FConnection.GetErrorHandle, LobLocator, FChunkSize, ConSettings,
          ConSettings^.ClientCodePage^.CP);
      (Result as IZOracleBlob).ReadLob; //nasty: we've got only one descriptor if we fetch the rows. Loading on demand isn't possible
    end else if TypeCode=SQLT_NTY then
      Result := TZAbstractBlob.CreateWithStream(nil)
    else if TypeCode in [SQLT_LVB, SQLT_LVC, SQLT_BIN]
    then Result := TZAbstractBlob.CreateWithData({%H-}PAnsiChar({%H-}NativeUInt(Data)+FCurrentRowBufIndex*Length)+ SizeOf(Integer),
        {%H-}PInteger({%H-}NativeUInt(Data)+FCurrentRowBufIndex*Length)^)
    else Result := TZAbstractClob.CreateWithData(GetPAnsiChar(ColumnIndex, Len), Len,
        ConSettings^.ClientCodePage^.CP, ConSettings);
end;

{ TZOracleResultSet }

{**
  Opens this recordset.
}
procedure TZOracleResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  CurrentVar: PZSQLVar;
  ColumnCount: ub4;
  TempColumnNameLen, CSForm: Integer;
  P: PAnsiChar;
  RowSize: Integer;
  BufferPos: PAnsiChar;
  DescriptorColumnCount,SubObjectColumnCount: Integer;
  function AttributeToString(var P: PAnsiChar; Len: Integer): string;
  begin
    if P <> nil then
      {$IFDEF UNICODE}
      Result := ZEncoding.PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP)
      {$ELSE}
      if (not ConSettings^.AutoEncode) or ZCompatibleCodePages(ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP) then
        Result := BufferToStr(P, Len)
      else
        Result := ZUnicodeToString(PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP)
      {$ENDIF}
    else
      Result := '';
    P := nil;
  end;
begin
  if ResultSetConcurrency = rcUpdatable then
    raise EZSQLException.Create(SLiveResultSetsAreNotSupported);

  if not Assigned(FStmtHandle) or not Assigned(FErrorHandle) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.StmtExecute(FContextHandle, FStmtHandle, FErrorHandle, 1, 0,
      nil, nil, OCI_DESCRIBE_ONLY),
      lcExecute, 'OCIStmtExecute', ConSettings);

  { Resize SQLVARS structure if needed }
  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.AttrGet(FStmtHandle, OCI_HTYPE_STMT, @ColumnCount, nil,
      OCI_ATTR_PARAM_COUNT, FErrorHandle),
      lcExecute, 'OCIStmtExecute', ConSettings);

  AllocateOracleSQLVars(FColumns, ColumnCount);
  RowSize := 0;
  DescriptorColumnCount := 0; SubObjectColumnCount := 0;
  { collect informations for result set columns }
  for I := 1 to ColumnCount do
  begin
    {$R-}
    CurrentVar := @FColumns.Variables[I-1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    CurrentVar^.Handle := nil;

    FPlainDriver.ParamGet(FStmtHandle, OCI_HTYPE_STMT, FErrorHandle,
      CurrentVar^.Handle, I);
    FPlainDriver.AttrGet(CurrentVar^.Handle, OCI_DTYPE_PARAM,
      @CurrentVar^.oDataSize, nil, OCI_ATTR_DATA_SIZE, FErrorHandle);
    FPlainDriver.AttrGet(CurrentVar^.Handle, OCI_DTYPE_PARAM,
      @CurrentVar^.oDataType, nil, OCI_ATTR_DATA_TYPE, FErrorHandle);
    CurrentVar^.Scale := 0;
    CurrentVar^.Precision := 0;

    case CurrentVar^.oDataType of
      SQLT_AFC, SQLT_CHR, SQLT_VCS, SQLT_AVC, SQLT_STR, SQLT_VST:
        CurrentVar^.ColType := stString;
      SQLT_NUM: //unsigned char[21](binary) see: http://docs.oracle.com/cd/B19306_01/appdev.102/b14250/oci03typ.htm
        begin {11g returns Precision = 38 in all cases}
          FPlainDriver.AttrGet(CurrentVar^.Handle, OCI_DTYPE_PARAM,
            @CurrentVar^.Precision, nil, OCI_ATTR_PRECISION, FErrorHandle);
          FPlainDriver.AttrGet(CurrentVar^.Handle, OCI_DTYPE_PARAM,
            @CurrentVar^.Scale, nil, OCI_ATTR_SCALE, FErrorHandle);

          {by default convert number to double}
          CurrentVar^.ColType := stDouble;
          CurrentVar.Length := SizeOf(Double);
          if (CurrentVar^.Scale = 0) and (CurrentVar^.Precision <> 0) then
            //No digits found, but possible signed or not/overrun of converiosn? No way to find this out -> just use a "save" type
            case CurrentVar^.Precision of
              0..2: CurrentVar^.ColType := stShort; // -128..127
              3..4: CurrentVar^.ColType := stSmall; // -32768..32767
              5..9: CurrentVar^.ColType := stInteger; // -2147483648..2147484647
              10..19: CurrentVar^.ColType := stLong; // -9223372036854775808..9223372036854775807
              //skip 20 can be UInt64 or Int64  assume Double values instead
              21: CurrentVar^.ColType := stULong; //0..18446744073709551615
            end;
        end;
      SQLT_BFLOAT, SQLT_BDOUBLE, SQLT_IBFLOAT, SQLT_IBDOUBLE:
        CurrentVar^.ColType := stDouble;
      SQLT_INT, _SQLT_PLI:
        CurrentVar^.ColType := stInteger;
      SQLT_LNG, SQLT_LVC:
        CurrentVar^.ColType := stAsciiStream;
      SQLT_RID, SQLT_RDD:
        begin
          CurrentVar^.ColType := stString;
          CurrentVar^.oDataSize := 20;
        end;
      SQLT_DAT, SQLT_DATE:
        { oracle DATE precission - 1 second}
         CurrentVar^.ColType := stTimestamp;
      SQLT_TIME, SQLT_TIME_TZ:
        begin
          CurrentVar^.ColType := stTime;
          inc(DescriptorColumnCount);
        end;
      SQLT_TIMESTAMP, SQLT_TIMESTAMP_TZ, SQLT_TIMESTAMP_LTZ, SQLT_INTERVAL_DS, SQLT_INTERVAL_YM:
        begin
          CurrentVar^.ColType := stTimestamp;
          inc(DescriptorColumnCount);
        end;
      SQLT_BIN, SQLT_LBI:
        if CurrentVar^.oDataSize = 0 then
          CurrentVar^.ColType := stBinaryStream
        else
          CurrentVar^.ColType := stBytes;
      SQLT_CLOB:
        begin
          CurrentVar^.ColType := stAsciiStream;
          inc(DescriptorColumnCount)
        end;
      SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE:
        begin
          CurrentVar^.ColType := stBinaryStream;
          inc(DescriptorColumnCount)
        end;
      SQLT_NTY:
        begin
          Inc(SubObjectColumnCount);
          CurrentVar^.Length := SizeOf(PPOCIDescriptor);
          CurrentVar^.ColType := stDataSet;
          CurrentVar^.TypeCode := CurrentVar^.oDataType;

          CurrentVar^._Obj := DescribeObject(FplainDriver, FConnection,
            CurrentVar^.Handle, FStmtHandle, 0);
          if CurrentVar^._Obj.col_typecode = OCI_TYPECODE_TABLE then
            CurrentVar^.ColType := stDataSet
          else if CurrentVar^._Obj.col_typecode = OCI_TYPECODE_VARRAY then
            CurrentVar^.ColType := stArray
          else //more possible types
            CurrentVar^.ColType := stBinaryStream;
        end;
      else
        CurrentVar^.ColType := stUnknown;
    end;
    if (CurrentVar^.ColType = stString) then begin
      FPlainDriver.AttrGet(CurrentVar^.Handle, OCI_DTYPE_PARAM,
        @CurrentVar^.Precision, nil, OCI_ATTR_DISP_SIZE, FErrorHandle);
      FPlainDriver.AttrGet(CurrentVar^.Handle, OCI_DTYPE_PARAM,
        @CSForm, nil, OCI_ATTR_CHARSET_FORM, FErrorHandle);
      if CSForm = SQLCS_NCHAR then
        CurrentVar^.Precision := CurrentVar^.Precision shr 1;
      {EH: Oracle does not calculate true data size if the attachment charset is a multibyte one
        and is different to the native db charset
        so we'll increase the buffers to avoid truncation errors. Here we go:}
      if Consettings.ClientCodePage.Encoding <> ceUTF16
      then CurrentVar^.oDataSize := CurrentVar^.Precision * ConSettings.ClientCodePage.CharWidth
      else CurrentVar^.oDataSize := CurrentVar^.Precision shl 1;
      if Consettings.CPType =cCP_UTF16 then
        CurrentVar^.ColType := stUnicodeString;
      CurrentVar^.CodePage := ConSettings.ClientCodePage.CP;
    end else if (CurrentVar^.ColType = stAsciiStream) then begin
      if (CurrentVar^.oDataType <> SQLT_LNG) and (ConSettings.CPType = cCP_UTF16) then
        CurrentVar^.ColType := stUnicodeStream;
      CurrentVar^.CodePage := ConSettings.ClientCodePage.CP;
    end else
      CurrentVar^.CodePage := High(Word);
    {set new datatypes if required}
    DefineOracleVarTypes(CurrentVar, CurrentVar^.ColType, CurrentVar^.oDataSize, CurrentVar^.oDataType, FConnection.GetClientVersion >= 11002000);
    {calc required size of field}
    Inc(RowSize, CalcBufferSizeOfSQLVar(CurrentVar));
  end;
  { now let's calc the iters we can use }
  if RowSize > FZBufferSize
  then FIteration := 1
  else FIteration := FZBufferSize div RowSize;
  if ( DescriptorColumnCount > 0 ) and (DescriptorColumnCount * FIteration > 1000) then //take care we do not create too much descriptors
    FIteration := 1000 div DescriptorColumnCount;
  if (SubObjectColumnCount > 0) then
    FIteration := 1; //EH: current code isn't prepared -> Bugfix required

  SetLength(FRowsBuffer, RowSize * FIteration); //Alloc mem we need for multiple rows
  BufferPos := Pointer(FRowsBuffer);
  {give our Vars it's addressation in RowsBuffer}

  { Bind handle and Fills the column info. }
  ColumnsInfo.Clear;
  for I := 1 to FColumns.AllocNum do begin
    {$R-}
    CurrentVar := @FColumns.Variables[I-1];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    SetVariableDataEntrys(BufferPos, CurrentVar, FIteration);
    if CurrentVar^.ColType <> stUnknown then begin//do not BIND unknown types
      AllocDesriptors(FPlainDriver, FConnectionHandle, CurrentVar, FIteration, False); //alloc Desciptors if required
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.DefineByPos(FStmtHandle, CurrentVar^.Define,
          FErrorHandle, I, CurrentVar^.Data, CurrentVar^.Length, CurrentVar^.TypeCode,
          CurrentVar^.oIndicatorArray, CurrentVar^.oDataSizeArray, nil, OCI_DEFAULT),
        lcExecute, 'OCIDefineByPos', ConSettings);
    end;
    if CurrentVar^.oDataType=SQLT_NTY then
      //second step: http://www.csee.umbc.edu/portal/help/oracle8/server.815/a67846/obj_bind.htm
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.DefineObject(CurrentVar^.Define, FErrorHandle, CurrentVar^._Obj.tdo,
           @CurrentVar^._Obj.obj_value, nil, nil, nil),
        lcExecute, 'OCIDefineObject', ConSettings);

    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do begin
      ColumnCodePage := CurrentVar^.CodePage;
      P := nil; //init
      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.AttrGet(CurrentVar^.Handle, OCI_DTYPE_PARAM,
        @P, @TempColumnNameLen, OCI_ATTR_NAME, FErrorHandle),
        lcExecute, 'OCI_ATTR_NAME', ConSettings);
      ColumnLabel := AttributeToString(P, TempColumnNameLen);

      CheckOracleError(FPlainDriver, FErrorHandle,
        FPlainDriver.AttrGet(CurrentVar^.Handle, OCI_DTYPE_PARAM,
        @P, @TempColumnNameLen, OCI_ATTR_SCHEMA_NAME, FErrorHandle),
        lcExecute, 'OCI_ATTR_SCHEMA_NAME', ConSettings);
      SchemaName := AttributeToString(P, TempColumnNameLen);

      Signed := True;
      Nullable := ntNullable;
      CharOctedLength := CurrentVar^.oDataSize;

      ColumnType := CurrentVar^.ColType;
      Scale := CurrentVar^.Scale;
      Precision := CurrentVar^.Precision;
      if (ColumnType in [stString, stUnicodeString]) then
        ColumnDisplaySize := CurrentVar^.Precision
      else if (ColumnType = stBytes ) then
        Precision := CurrentVar^.oDataSize
      else
        Precision := CurrentVar^.Precision;
    end;

    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
end;

procedure TZOracleResultSet.ResetCursor;
begin
  FCurrentRowBufIndex := 0;
  inherited;

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
procedure TZOracleResultSet.BeforeClose;
begin
  inherited BeforeClose;
  SetLength(Self.FRowsBuffer, 0);
  { prepared statement own handles, so dont free them }
  FStmtHandle := nil;
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
function TZOracleResultSet.Next: Boolean;
var
  Status: Integer;
  FetchedRows: LongWord;
label Success;  //ugly but faster and no double code
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows)) or (FStmtHandle = nil) then
    Exit;

  if RowNo = 0 then begin//fetch Iteration count of rows
    Status := FPlainDriver.StmtExecute(FContextHandle, FStmtHandle,
      FErrorHandle, FIteration, 0, nil, nil, OCI_DEFAULT);
    if Status = OCI_SUCCESS then begin
      FMaxBufIndex := FIteration -1; //FFetchedRows is an index [0...?] / FIteration is Count 1...?
      goto success; //skip next if's
    end;
  end else if Integer(FCurrentRowBufIndex) < FMaxBufIndex then begin
    Inc(FCurrentRowBufIndex);
    goto Success; //skip next if's
  end else if FMaxBufIndex+1 < FIteration then begin
    RowNo := RowNo + 1;
    Exit;
  end else begin //fetch Iteration count of rows
    Status := FPlainDriver.StmtFetch2(FStmtHandle, FErrorHandle,
      FIteration, OCI_FETCH_NEXT, 0, OCI_DEFAULT);
    FCurrentRowBufIndex := 0; //reset
    if Status = OCI_SUCCESS then begin
      FMaxBufIndex := FIteration -1;
      goto success;
    end;
  end;

  if Status = OCI_NO_DATA then begin
    FPlainDriver.AttrGet(FStmtHandle,OCI_HTYPE_STMT,@FetchedRows,nil,OCI_ATTR_ROWS_FETCHED,FErrorHandle);
    LastRowNo := RowNo+Integer(FetchedRows);  //this makes Exit out in first check on next fetch
    FMaxBufIndex := Integer(FetchedRows)-1;
    RowNo := RowNo + 1;
    //did we retrieve a row or is table empty?
    if FetchedRows > 0 then
      Result := True;
    Exit;
  end;

  CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'FETCH ROW', ConSettings);

  if Status in [OCI_SUCCESS, OCI_SUCCESS_WITH_INFO] then begin
Success:
    RowNo := RowNo + 1;
    if FMaxBufIndex+1 = FIteration then
      LastRowNo := RowNo;
    Result := True;
  end;
end;

{ TZOracleCallableResultSet }

function TZOracleCallableResultSet.PrepareOracleOutVars(const Params: PZSQLVars;
  const OracleParams: TZOracleParams): PZSQLVars;
var
  I, J: Integer;
begin
  J := 0;
  for i := 0 to High(OracleParams) do
    if Ord(OracleParams[I].pType) >= Ord(pctInOut) then
      Inc(J);

  Result := nil;
  AllocateOracleSQLVars(Result, J);
  SetLength(FFieldNames, J);

  for I := 0 to High(OracleParams) do
  begin
    J := OracleParams[I].pOutIndex;
    if Ord(OracleParams[I].pType) >= Ord(pctInOut) then begin
      {$R-}
      Result.Variables[J].ColType := Params.Variables[I].ColType;
      Result.Variables[J].TypeCode := Params.Variables[I].TypeCode;
      Result.Variables[J].oDataSize := Params.Variables[I].oDataSize;
      Result.Variables[J].Length := Params.Variables[I].Length;
      Result.Variables[J].oDataSizeArray := Params.Variables[I].oDataSizeArray;  //reference to array entry in stmt buffer -> no move!
      Result.Variables[J].oIndicatorArray := Params.Variables[I].oIndicatorArray; //reference to array entry in stmt buffer -> no move!
      Result.Variables[J].Data := Params.Variables[I].Data; //reference to data entry in stmt buffer -> no move!
      Result.Variables[J].DescriptorType := Params.Variables[I].DescriptorType;
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
      FFieldNames[J] := OracleParams[I].pName;
    end;
  end;
end;

procedure TZOracleCallableResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  CurrentVar: PZSQLVar;
begin
  { Fills the column info. }
  ColumnsInfo.Clear;
  for I := 0 to FColumns.AllocNum -1 do
  begin
    {$R-}
    CurrentVar := @FColumns.Variables[I];
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    ColumnInfo := TZColumnInfo.Create;

    with ColumnInfo do
    begin
      ColumnName := '';
      TableName := '';

      ColumnLabel := FFieldNames[i];
      ColumnDisplaySize := 0;
      AutoIncrement := False;
      Signed := True;
      Nullable := ntNullable;

      ColumnType := CurrentVar^.ColType;
      Scale := CurrentVar^.Scale;

      {Reset the column type which can be changed by user before}
      if CurrentVar^.ColType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then
      begin
        ColumnInfo.ColumnCodePage := ConSettings^.ClientCodePage^.CP;
        if (ColumnType = stUnicodeStream) and not ( ConSettings^.CPType = cCP_UTF16) then
          ColumnType := stAsciiStream;
        if (ColumnType = stAsciiStream) and ( ConSettings^.CPType = cCP_UTF16) then
          ColumnType := stUnicodeStream;
        if (ColumnType = stUnicodeString) and not ( ConSettings^.CPType = cCP_UTF16) then
          ColumnType := stString;
        if (ColumnType = stString) and ( ConSettings^.CPType = cCP_UTF16) then
          ColumnType := stUnicodeString;
      end
      else
        ColumnInfo.ColumnCodePage := High(Word);

      if ( ColumnType in [stString, stUnicodeString] ) then
      begin
        ColumnDisplaySize := CurrentVar.oDataSize;
        Precision := CurrentVar.oDataSize;
      end
      else
        Precision := CurrentVar.Precision;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;

  inherited Open;
end;

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param PlainDriver a Oracle plain driver.
  @param Statement a related SQL statement object.
  @param SQL a SQL statement.
  @param Handle a Oracle specific query handle.
}
constructor TZOracleCallableResultSet.Create(const PlainDriver: IZOraclePlainDriver;
  const Statement: IZStatement; const SQL: string; StmtHandle: POCIStmt;
  ErrorHandle: POCIError; const OutParams: PZSQLVars;
  const OracleParams: TZOracleParams);
begin
  FColumns := PrepareOracleOutVars(OutParams, OracleParams);
  inherited Create(PlainDriver, Statement, SQL, StmtHandle, ErrorHandle, 0);
  FConnection := Statement.GetConnection as IZOracleConnection;
  LastRowNo := 1;
  MaxRows := 1;
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
procedure TZOracleCallableResultSet.Close;
begin
  { stmt holds buffers and handles. Here we just reference it. So we do NOTHING here, IDE frees all DynArrays}
  FreeOracleSQLVars(FPlainDriver, FColumns, FIteration, FConnectionHandle,
    FErrorHandle, ConSettings);
  SetLength(Self.FRowsBuffer, 0);
  { prepared statement own handles, so dont free them }
  FStmtHandle := nil;
  inherited Close;
end;

function TZOracleCallableResultSet.Next: Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if (RowNo = 1) then
    Exit;
  RowNo := 1;
  Result := True;
end;

{ TZOracleBlob }

{**
  Constructs this class and assignes the main properties.
  @param PlainDriver a Oracle plain driver.
  @param Data a pointer to the blobdata.
  @param Size the size of the blobdata.
  @param Handle a Oracle connection reference.
  @param LobLocator an Oracle lob locator reference.
  @param BlobType a blob type.
}
constructor TZOracleBlob.Create(const PlainDriver: IZOraclePlainDriver;
  const Data: Pointer; const Size: Int64; const ContextHandle: POCISvcCtx;
  const ErrorHandle: POCIError; const LobLocator: POCILobLocator;
  const ChunkSize: Integer; const ConSettings: PZConSettings);
begin
  inherited CreateWithData(Data, Size);
  FContextHandle := ContextHandle;
  FErrorHandle := ErrorHandle;
  FLobLocator := LobLocator;
  FPlainDriver := PlainDriver;
  FTemporary := False;
  FChunkSize := ChunkSize;
  FConSettings := ConSettings;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOracleBlob.Destroy;
begin
  if FTemporary then
    FPlainDriver.LobFreeTemporary(FContextHandle, FErrorHandle, FLobLocator);
  inherited Destroy;
end;

{**
  Creates a temporary blob.
}
procedure TZOracleBlob.CreateBlob;
begin
  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.LobCreateTemporary(FContextHandle, FErrorHandle,
      FLobLocator, OCI_DEFAULT, OCI_DEFAULT, OCI_TEMP_BLOB, False,
      OCI_DURATION_DEFAULT),
    lcOther, 'Create Large Object', FConSettings);

  FTemporary := True;
end;

{**
  Reads the blob by the blob handle.
}
procedure TZOracleBlob.ReadLob;
const
  MemDelta = 1 shl 12;  // read page (2^...)
var
  Status: Integer;
  Buf: PByteArray;
  ReadNumBytes, Offset, Cap: ub4;
begin
  if not Updated and (FLobLocator <> nil)
    and (BlobData = nil) and (not FTemporary) then
  begin
    { Opens a large object or file for read. }
    Status := FPlainDriver.LobOpen(FContextHandle, FErrorHandle, FLobLocator, OCI_LOB_READONLY);
    if Status <> OCI_SUCCESS then
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Open Large Object', FConSettings);
    try
      { Reads data in chunks by MemDelta or more }
      Offset := 0;
      Buf := nil;
      try
        repeat
          {Calc new progressive by 1/8 and aligned by MemDelta capacity for buffer}
          Cap := (Offset + (Offset shr 3) + 2 * MemDelta - 1) and not (MemDelta - 1);
          ReallocMem(Buf, Cap);
          ReadNumBytes := Cap - Offset;

          Status := FPlainDriver.LobRead(FContextHandle, FErrorHandle,
            FLobLocator, ReadNumBytes, Offset + 1, {$R-}@Buf[Offset]{$IFDEF RangeCheckEnabled}{$R+}{$ENDIF}, ReadNumBytes,
            nil, nil, 0, SQLCS_IMPLICIT);
          if Status <> OCI_SUCCESS then
            CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Read Large Object', FConSettings);
          if ReadNumBytes > 0 then
            Inc(Offset, ReadNumBytes);
        until Offset < Cap;
      except
        FreeMem(Buf);
        Buf := nil;
        raise;
      end;
    finally
      { Closes large object or file. }
      Status := FPlainDriver.LobClose(FContextHandle,FErrorHandle, FLobLocator);
      if Status <> OCI_SUCCESS then
        CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Close Large Object', FConSettings);
    end;
    { Assigns data }
    InternalSetData(Buf, Offset);
  end;
  //inherited ReadLob;
end;

{**
  Writes the blob by the blob handle.
}
procedure TZOracleBlob.WriteLob;
begin
  OraWriteLob(FPlainDriver, BlobData, FContextHandle, FErrorHandle, FLobLocator,
    FChunkSize, BlobSize, True, nil);
end;

procedure TZOracleBlob.WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
begin
  OraWriteLob(FPlainDriver, Buffer, FContextHandle, FErrorHandle, FLobLocator,
    FChunkSize, Len, True, nil);
end;

{**
  Replace data in blob by AData without copy (keep ref of AData)
}
procedure TZOracleBlob.InternalSetData(AData: Pointer; ASize: Integer);
begin
  InternalClear;
  BlobData := AData;
  BlobSize := ASize;
end;

{ TZOracleClob }

constructor TZOracleClob.Create(const PlainDriver: IZOraclePlainDriver;
  const Data: Pointer; const Size: Cardinal; const ConnectionHandle: POCIEnv;
  const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
  const LobLocator: POCILobLocator; const ChunkSize: Integer;
  const ConSettings: PZConSettings; const CodePage: Word);
begin
  if ZCompatibleCodePages(CodePage, zCP_UTF16) then
    inherited CreateWithData(Data, Size shr 1, ConSettings) //shr 1 = div 2 but faster
  else
    inherited CreateWithData(Data, Size, CodePage, ConSettings);
  FContextHandle := ContextHandle;
  FConnectionHandle := ConnectionHandle;
  FErrorHandle := ErrorHandle;
  FLobLocator := LobLocator;
  FPlainDriver := PlainDriver;
  FTemporary := False;
  FChunkSize := ChunkSize;
end;

destructor TZOracleClob.Destroy;
begin
  if FTemporary then
    FPlainDriver.LobFreeTemporary(FContextHandle, FErrorHandle, FLobLocator);
  inherited Destroy;
end;

{**
  Creates a temporary blob.
}
procedure TZOracleClob.CreateBlob;
begin
  CheckOracleError(FPlainDriver, FErrorHandle,
    FPlainDriver.LobCreateTemporary(FContextHandle, FErrorHandle, FLobLocator,
      OCI_DEFAULT, OCI_DEFAULT, OCI_TEMP_CLOB, False, OCI_DURATION_DEFAULT),
    lcOther, 'Create Large Object', FConSettings);

  FTemporary := True;
end;

procedure TZOracleClob.ReadLob;
var
  Status: Integer;
  Buf: PByteArray;
  ReadNumChars, Offset: ub4;
  csfrm: ub1;

  procedure DoRead(const csid: ub2; const csfrm: ub1);
  begin
    ReadNumChars := 0;
    Status := FPlainDriver.LobRead(FContextHandle,FErrorHandle, FLobLocator,
      ReadNumChars, Offset + 1, Buf, FChunkSize, nil, nil, csid, csfrm);
    if ReadNumChars > 0 then
    begin
      Inc(Offset, ReadNumChars);
      ReallocMem(FBlobData, Offset+1);
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf^, (PAnsiChar(FBlobData)+NativeUInt(OffSet-ReadNumChars))^, ReadNumChars);
    end;
  end;
begin
  FCurrentCodePage := FConSettings^.ClientCodePage^.CP;
  if not Updated and (FLobLocator <> nil) and (BlobData = nil) and (not FTemporary) then
  begin
    { Opens a large object or file for read. }
    Status := FPlainDriver.LobOpen(FContextHandle, FErrorHandle, FLobLocator, OCI_LOB_READONLY);
    CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Open Large Object', FConSettings);
    try
      { Reads data in chunks by MemDelta or more }
      Offset := 0;
      Buf := nil;
      try
        GetMem(Buf, FChunkSize+1);
        Offset := 0;
        CheckOracleError(FPlainDriver, FErrorHandle,
          FPlainDriver.LobCharSetForm(FConnectionHandle, FErrorHandle,
            FLobLocator, @csfrm),
          lcOther, 'Determine LOB SCFORM', FConSettings); //need to determine proper CharSet-Form
        DoRead(FConSettings^.ClientCodePage^.ID, csfrm);
        while Status = OCI_NEED_DATA do
          DoRead(FConSettings^.ClientCodePage^.ID, csfrm);
        CheckOracleError(FPlainDriver, FErrorHandle,
          Status, lcOther, 'Read Large Object', FConSettings);
        BlobSize := OffSet+1; //oracle includes #0 terminator
        if OffSet = 0 then ReallocMem(FBlobData, 1);
        (PAnsiChar(FBlobData)+NativeUInt(OffSet))^ := #0;
      except
        FreeMem(Buf);
        Buf := nil;
        raise;
      end;
    finally
      { Closes large object or file. }
      Status := FPlainDriver.LobClose(FContextHandle, FErrorHandle, FLobLocator);
      CheckOracleError(FPlainDriver, FErrorHandle, Status, lcOther, 'Close Large Object', FConSettings);
      if Buf <> nil then
        FreeMem(Buf);
    end;
  end;
  //inherited ReadLob;
end;

procedure TZOracleClob.WriteLob;
begin
  GetPAnsiChar(FConSettings^.ClientCodePage^.CP); //convert if required
  OraWriteLob(FPlainDriver, BlobData, FContextHandle, FErrorHandle, FLobLocator,
    FChunkSize, BlobSize, False, FConSettings);
end;

procedure TZOracleClob.WriteLobFromBuffer(const Buffer: Pointer; const Len: Cardinal);
begin
  if Buffer = nil then
    OraWriteLob(FPlainDriver, Buffer, FContextHandle, FErrorHandle, FLobLocator,
      FChunkSize, Len, False, FConSettings)
  else
    OraWriteLob(FPlainDriver, Buffer, FContextHandle, FErrorHandle, FLobLocator,
      FChunkSize, Int64(Len)+1, False, FConSettings);
end;
{$ENDIF ZEOS_DISABLE_ORACLE}

end.
