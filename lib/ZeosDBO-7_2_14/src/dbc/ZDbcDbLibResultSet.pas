{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         DBLib Resultset common functionality            }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZDbcDbLibResultSet;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
uses
{$IFNDEF FPC}
  DateUtils,
{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  ZDbcIntfs, ZDbcResultSet, ZCompatibility, ZDbcResultsetMetadata,
  ZDbcGenericResolver, ZDbcCachedResultSet, ZDbcCache, ZDbcDBLib,
  ZPlainDbLibConstants, ZPlainDBLibDriver;

type
  TZAbstractDblibDataProvider = Class
    protected
      FPLainDriver: IZDBLibPlainDriver;
      FHandle: PDBPROCESS;
      FConnection: IZDBLibConnection;
      FNeedDbCanQuery: Boolean;
    public
      property needDbCanQuery: Boolean read FNeedDbCanQuery;
      constructor Create(Connection: IZDBLibConnection); virtual;
      function Next: Boolean; virtual; abstract;
      procedure GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer); virtual; abstract;
  end;

  TZPlainDblibDataProvider = class(TZAbstractDblibDataProvider)
    protected
      FCheckDbDead: Boolean;
    public
      constructor Create(Connection: IZDBLibConnection; const CheckDbDead: Boolean); reintroduce;
      function Next: Boolean; override;
      procedure GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer); override;
  end;

  TZCachedDblibField = record
    IsNull: Boolean;
    Data: TBytes;
  end;

  TZCachedDblibRow = class
    Fields: Array of TZCachedDblibField;
    NextRow: TZCachedDblibRow;
  end;

  TZCachedDblibDataProvider = class(TZAbstractDblibDataProvider)
    protected
      FRootRow: TZCachedDblibRow;
    public
      procedure LoadData;
      constructor Create(Connection: IZDBLibConnection); override;
      destructor Destroy; override;
      function Next: Boolean; override;
      procedure GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer); override;
  end;

  IZDblibResultSetMetadata = interface(IZResultSetMetaData)
  ['{48B1C2EC-DBD4-4A5D-B41B-E75F1E22F909}']
    procedure LoadColumns;
  end;

  TZDblibResultSetMetadata = class(TZAbstractResultSetMetadata, IZDblibResultSetMetadata)
  protected
    procedure SetColumnCodePageFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); override;
  end;

  {** Implements DBLib ResultSet. }
  TZDBLibResultSet = class(TZAbstractResultSet)
  private
    FSQL: string;
    FCheckDBDead: Boolean;
    FHandle: PDBPROCESS;
    DBLibColTypeCache: Array of TTDSType;
    DBLibColumnCount: Integer;
    FUserEncoding: TZCharEncoding;
    procedure CheckColumnIndex(ColumnIndex: Integer);
  protected
    FDBLibConnection: IZDBLibConnection;
    FPlainDriver: IZDBLibPlainDriver;
    FDataProvider: TZAbstractDblibDataProvider;
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      UserEncoding: TZCharEncoding = ceDefault);
    destructor Destroy; override;

    procedure BeforeClose; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    function Next: Boolean; override;
  end;

  {** Implements a cached resolver with mssql and sybase specific functionality. }
  TZDBLibCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FAutoColumnIndex: Integer;
  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit

uses ZMessages, ZDbcLogging, ZDbcDBLibUtils, ZEncoding, ZSysUtils, ZFastCode,
  ZClasses, ZDbcMetadata
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

constructor TZAbstractDblibDataProvider.Create(Connection: IZDBLibConnection);
begin
  inherited Create;
  FConnection := Connection;
  FPLainDriver := Connection.GetPlainDriver;
  FHandle := Connection.GetConnectionHandle;
end;

constructor TZPlainDblibDataProvider.Create(Connection: IZDBLibConnection; const CheckDbDead: Boolean);
begin
  inherited Create(Connection);
  FCheckDbDead := CheckDbDead;
  FNeedDbCanQuery := True;
end;

function TZPlainDblibDataProvider.Next: Boolean;
begin
  Result := False;
  if FCheckDBDead then
    if FPlainDriver.dbDead(FHandle) then
      Exit;
//!!! maybe an error message other than dbconnection is dead should be raised
  case FPlainDriver.dbnextrow(FHandle) of
    REG_ROW: Result := True;
    NO_MORE_ROWS: ;
    DBFAIL: FConnection.CheckDBLibError(lcOther, 'NEXT');
    BUF_FULL: raise EZSQLException.Create('The data doesn''t fit the dblib buffer');//should not happen because we are not using dblibc buffering.
  else
   // If a compute row is read, the computeid of the row is returned
    Result := False;
  end;
end;

procedure TZPlainDblibDataProvider.GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer);
begin
  DatPtr := FPLainDriver.dbData(FHandle, ColIndex);
  DatLen := FPLainDriver.dbDatLen(FHandle, ColIndex);
end;

constructor TZCachedDblibDataProvider.Create(Connection: IZDBLibConnection);
begin
  inherited;
  FRootRow := nil;
  FNeedDbCanQuery := false;
end;

destructor TZCachedDblibDataProvider.Destroy;
begin
  while Next do ; // next removes one row after the other
  inherited;
end;

function TZCachedDblibDataProvider.Next: Boolean;
var
  currentRow: TZCachedDblibRow;
begin
  Result := false;
  if Assigned(FRootRow) then begin
    currentRow := FRootRow;
    FRootRow := currentRow.NextRow;
    SetLength(currentRow.Fields, 0);
    FreeAndNil(currentRow);
    Result := Assigned(FRootRow);
  end;
end;

procedure TZCachedDblibDataProvider.GetColData(ColIndex: Integer; out DatPtr: Pointer; out DatLen: Integer);
begin
  Dec(ColIndex);
  DatPtr := nil;
  DatLen := 0;
  if Assigned(FRootRow) then begin
    if (ColIndex >= 0) and (ColIndex <= High(FRootRow.Fields)) then begin
      DatLen := Length(FRootRow.Fields[ColIndex].Data);
      if DatLen > 0 then begin
        DatPtr := Pointer(@(FRootRow.Fields[ColIndex].Data[0]))
      end else begin
        if FRootRow.Fields[ColIndex].IsNull
        then DatPtr := nil
        else DatPtr := Pointer(High(NativeUInt));
      end;
    end;
  end;
end;

procedure TZCachedDblibDataProvider.LoadData;
var
  colCount: Integer;
  plainProvider: TZPlainDblibDataProvider;
  DatLen: Integer;
  Data: Pointer;
  currentRow: TZCachedDblibRow;
  x: Integer;

begin
  colCount := FPlainDriver.dbnumcols(FHandle);
  plainProvider := TZPlainDblibDataProvider.Create(FConnection, false);
  try
    FRootRow := TZCachedDblibRow.Create; // this is just a dummy to be on for being before the first row.
    currentRow := FRootRow;
    SetLength(currentRow.Fields, colCount);
    while plainProvider.Next do begin
      currentRow.NextRow := TZCachedDblibRow.Create;
      currentRow := currentRow.NextRow;

      SetLength(currentRow.Fields, colCount);
      for x := 0 to colCount - 1 do begin
        plainProvider.GetColData(x + 1, Data, DatLen);
        currentRow.Fields[x].IsNull := (Data = nil) and (DatLen = 0);
        if DatLen > 0 then begin
          SetLength(currentRow.Fields[x].Data, DatLen);
          Move(Data^, currentRow.Fields[x].Data[0], DatLen);
        end;
      end;
    end;
  finally
    FreeAndNil(plainProvider);
  end;
  FPLainDriver.dbCanQuery(FHandle);
end;


{ TZDBLibResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param Handle a DBLib specific query handle.
}
constructor TZDBLibResultSet.Create(const Statement: IZStatement; const SQL: string;
  UserEncoding: TZCharEncoding);
begin
  inherited Create(Statement, SQL,
    TZDblibResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  Statement.GetConnection.QueryInterface(IZDBLibConnection, FDBLibConnection);
  FPlainDriver := FDBLibConnection.GetPlainDriver;
  FHandle := FDBLibConnection.GetConnectionHandle;
  FSQL := SQL;
  FCheckDBDead := FPlainDriver.GetProtocol = 'mssql';
  FUserEncoding := UserEncoding;
  //FDataProvider := TZPlainDblibDataProvider.Create(Statement.GetConnection as IZDbLibConnection , FCheckDBDead);
  FDataProvider := TZCachedDblibDataProvider.Create(Statement.GetConnection as IZDbLibConnection);
  Open;
end;

destructor TZDBLibResultSet.Destroy;
begin
  if Assigned(FDataProvider) then
    FreeAndNil(FDataProvider);
  inherited;
end;

{**
  Opens this recordset.
}
procedure TZDBLibResultSet.Open;
var
  ColumnInfo: TZColumnInfo;
  I: Integer;
  ColInfo: DBCOL;
  tdsColInfo: TTDSDBCOL;
  NeedsLoading: Boolean;
label AssignGeneric;
  procedure AssignGenericColumnInfoFromZDBCOL(ArrayIndex: Integer; ColInfo: ZDBCOL);
  begin
    DBLibColTypeCache[ArrayIndex] := TTDSType(ColInfo.Typ);
    ColumnInfo.ColumnType := ConvertTDSTypeToSqlType(DBLibColTypeCache[ArrayIndex], ConSettings.CPType);
    if ColumnInfo.ColumnType = stUnknown
    then NeedsLoading := true;
    if DBLibColTypeCache[ArrayIndex] in [tdsNumeric, tdsDecimal] then
      ColumnInfo.Scale := ColInfo.Scale
    else
      ColumnInfo.Scale := 0;
    ColumnInfo.Precision := ColInfo.MaxLength;
    if (DBLibColTypeCache[ArrayIndex] = tdsNumeric) and (colinfo.Scale = 0) and (ColInfo.Precision = 19)
    then NeedsLoading := true; // we cannot be sure if it is bigint or numeric - let somebody else deal with this...
    ColumnInfo.CaseSensitive := ColInfo.CaseSensitive = 1;
    ColumnInfo.Nullable := TZColumnNullableType(ColInfo.Null);
    ColumnInfo.ReadOnly := not (ColInfo.Updatable = 1);
    ColumnInfo.Writable := ColInfo.Updatable = 1;
    ColumnInfo.AutoIncrement := ColInfo.Identity;
    ColumnInfo.Signed := ColumnInfo.ColumnType in [stShort, stSmall, stInteger, stLong];
  end;
  function ValueToString(P: PAnsiChar): String;
  begin
    {$IFDEF UNICODE}
    Result := PRawToUnicode(P, ZFastCode.StrLen(P), ConSettings^.ClientCodePage^.CP);
    {$ELSE}
    ZSetString(P, ZFastCode.StrLen(P), Result);
    Result := ConSettings^.ConvFuncs.ZRawToString(Result,
          ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);
    {$ENDIF}
  end;
begin
  NeedsLoading := false;
//Check if the current statement can return rows
  if FPlainDriver.dbCmdRow(FHandle) <> DBSUCCEED then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  { Fills the column info }
  ColumnsInfo.Clear;
  DBLibColumnCount := FPlainDriver.dbnumcols(FHandle);
  SetLength(DBLibColTypeCache, DBLibColumnCount);
  for i := 1 to DBLibColumnCount do
  begin
    ColumnInfo := TZColumnInfo.Create;
    if (FPlainDriver.GetDBLibraryVendorType = lvtFreeTDS) then
    begin
      tdsColInfo.SizeOfStruct := SizeOf(TTDSDBCOL);
      FillChar(tdsColInfo.Name[0], tdsColInfo.SizeOfStruct- SizeOf(DBInt), #0);
      if FPlainDriver.dbcolinfo(FHandle, CI_REGULAR, I, 0, @tdsColInfo) <> DBSUCCEED then //might be possible for computed or cursor columns
        goto AssignGeneric;
      ColumnInfo.ColumnName := ValueToString(@tdsColInfo.Name[0]);
      if Byte(tdsColInfo.ActualName[0]) = Ord(#0) then
        ColumnInfo.ColumnLabel := ColumnInfo.ColumnName
      else
        ColumnInfo.ColumnLabel := ValueToString(@tdsColInfo.ActualName[0]);
      if Byte(tdsColInfo.TableName[0]) <> Ord(#0) then
        ColumnInfo.TableName := ValueToString(@tdsColInfo.TableName[0]);
      AssignGenericColumnInfoFromZDBCOL(I-1, tdsColInfo.ColInfo);
    end else if FDBLibConnection.GetProvider = dpMsSQL then begin
      ColInfo.SizeOfStruct := SizeOf(DBCOL); //before execute dbcolinfo we need to set the record size -> 122 Byte or we fail
      if FPlainDriver.dbcolinfo(FHandle, CI_REGULAR, I, 0, @ColInfo) <> DBSUCCEED then //might be possible for computed or cursor columns
        goto AssignGeneric;
      ColumnInfo.ColumnName := ValueToString(@ColInfo.Name[0]);
      if Byte(ColInfo.ActualName[0]) = Ord(#0) then
        ColumnInfo.ColumnLabel := ColumnInfo.ColumnName
      else
        ColumnInfo.ColumnLabel := ValueToString(@ColInfo.ActualName[0]);
      if (Byte(ColInfo.TableName[0]) <> Ord(#0)) then
        ColumnInfo.TableName := ValueToString(@ColInfo.TableName[0]);
      AssignGenericColumnInfoFromZDBCOL(I-1, ColInfo.ColInfo);
    end
    else
    begin
AssignGeneric:  {this is the old way we did determine the ColumnInformations}
      ColumnInfo.ColumnName := ValueToString(FPlainDriver.dbColSource(FHandle, I));
      ColumnInfo.ColumnLabel := ValueToString(FPlainDriver.dbColName(FHandle, I));
      DBLibColTypeCache[I-1] := TTDSType(FPlainDriver.dbColtype(FHandle, I));
      ColumnInfo.ColumnType := ConvertTDSTypeToSqlType(DBLibColTypeCache[I-1], ConSettings.CPType);
      if ColumnInfo.ColumnType = stUnknown
      then NeedsLoading := true;
      ColumnInfo.Currency := DBLibColTypeCache[I-1] in [tdsMoney, tdsMoney4, tdsMoneyN];
      ColumnInfo.Precision := FPlainDriver.dbCollen(FHandle, I);
      ColumnInfo.Scale := 0;
      ColumnInfo.Signed := not (DBLibColTypeCache[I-1] = tdsInt1);
    end;
    if (ColumnInfo.ColumnType in [stString, stUnicodeString]) then begin
      if (ConSettings.ClientCodepage^.IsStringFieldCPConsistent) then
        ColumnInfo.ColumnCodePage := ConSettings^.ClientCodePage^.CP
      else if (FUserEncoding = ceUTF8) then
        ColumnInfo.ColumnCodePage := zCP_UTF8;
    end;
    ColumnsInfo.Add(ColumnInfo);
  end;

  if FDataProvider is TZCachedDblibDataProvider then begin
    (FDataProvider as TZCachedDblibDataProvider).LoadData;
    if NeedsLoading then
      (GetMetaData as IZDblibResultSetMetadata).LoadColumns;
  end;

  inherited Open;
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
procedure TZDBLibResultSet.BeforeClose;
begin
{ TODO -ofjanos -cGeneral : Maybe it needs a dbcanquery here. }
  if FDataProvider.needDbCanQuery then
    if Assigned(FHandle) then
      if not FPlainDriver.dbDead(FHandle) then
        if FPlainDriver.dbCanQuery(FHandle) <> DBSUCCEED then
          FDBLibConnection.CheckDBLibError(lcDisconnect, 'CLOSE QUERY');
  FHandle := nil;
  inherited BeforeClose;
end;

{**
  Checks if the columnindex is in the proper range.
  An exception is generated if somthing is not ok.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZDBLibResultSet.CheckColumnIndex(ColumnIndex: Integer);
begin
  if (ColumnIndex > DBLibColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) or
     (ColumnIndex < FirstDbcIndex) then
    raise EZSQLException.Create(
      Format(SColumnIsNotAccessable, [ColumnIndex]));
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZDBLibResultSet.IsNull(ColumnIndex: Integer): Boolean;
var
  dbData: Pointer;
  DatLen: Integer;
begin
  CheckColumnIndex(ColumnIndex);
  FDataProvider.GetColData(ColumnIndex{$IFDEF GENERIC_INDEX}+1{$ENDIF}, dbData, DatLen);
  Result := (dbData = nil) and (DatLen = 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the string in bytes
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
var
  DT: TTDSType;
  dbData: Pointer;
  dbDatLen: Integer;
  label Convert{, DecLenByTrailingSpaces, AssignFromFRawTemp};
begin
  Len := 0;
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  ColumnIndex := ColumnIndex +1;
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, dbData, dbDatLen);
  Result := dbData;
  LastWasNull := Result = nil;
  if not LastWasNull then
    if (DT = tdsChar) or (DT = tdsText) then
    begin
      Len := NativeUInt(dbDatLen);
      while (Len > 0) and (AnsiChar((Result+Len -1)^) = AnsiChar(' ')) do Dec(Len);
      if TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnCodePage = zCP_NONE then
        case ZDetectUTF8Encoding(Result, Len) of
          etUTF8: TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnCodePage := zCP_UTF8;
          etAnsi: TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnCodePage := ConSettings^.ClientCodePage^.CP;
          else
            ;
        end;
    end
    else
    if DT = tdsUnique then
    begin
      FRawTemp := GUIDToRaw(PGUID(Result)^);
      Result := Pointer(FRawTemp);
      Len := 38;
    end
    else
    if (DT = tdsImage) then
      Len := NativeUInt(dbDatLen)
    else
    begin
      Convert:
      SetLength(FRawTemp, 4001);
      Len := FPlainDriver.dbconvert(FHandle, Ord(DT), dbData,
        Len, Ord(tdsChar), Pointer(FRawTemp), 4001);
      while (Len > 0) and (AnsiChar((Result+Len -1)^) = AnsiChar(' ')) do Dec(Len);
      Result := Pointer(FRawTemp);
    end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GetPAnsiChar');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString/UnicodeString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
var
  DT: TTDSType;
  Tmp: RawByteString;
  P: PAnsiChar;
  Len: LengthInt;
  DatLen: Integer;
begin
  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  FDataProvider.GetColData(ColumnIndex+1, Pointer(P), DatLen); //hint DBLib isn't #0 terminated @all
  {$ELSE}
  FDataProvider.GetColData(ColumnIndex, Pointer(P), DatLen); //hint DBLib isn't #0 terminated @all
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  Len := DatLen;
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := P = nil;
  if LastWasNull then
    Result := ''
  else
    if (DT = tdsChar) or (DT = tdsText) then
    begin
      while (Len > 0) and (AnsiChar((P+Len -1)^) = AnsiChar(' ')) do Dec(Len);
      if Len = 0 then
        Result := ''
      else
      {TDS protocol issue: we dont't know if UTF8(NCHAR) or ANSI(CHAR) fields are coming in: no idea about encoding..
       So selt's test encoding until we know it -----> bad for ASCII7 only }
        case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
          stAsciiStream, stUnicodeStream:  //DBlib doesn't convert NTEXT so in all cases we've ansi-Encoding
            Result := PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP);
          else//stString, stUnicodeString:
            if TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage = zCP_NONE then
              case ZDetectUTF8Encoding(P, Len) of
                etUTF8:
                  begin
                    TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage := zCP_UTF8;
                    Result := PRawToUnicode(P, Len, zCP_UTF8);
                  end;
                etAnsi:
                  begin
                    TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage := ConSettings^.ClientCodePage^.CP;
                    Result := PRawToUnicode(P, Len, ConSettings^.ClientCodePage^.CP);
                  end;
                else //ASCII7
                  Result := USASCII7ToUnicodeString(P, Len);
              end
            else
              Result := PRawToUnicode(P, Len, TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage)
        end
    end else
    if DT = tdsUnique then
      Result := GUIDToUnicode(PGUID(P)^)
    else if DT = tdsSybaseLongBinary then begin
      SetLength(Result, Len div 2);
      Move(P^, Pointer(Result)^, Len);
    end else if (DT = tdsImage) then
      ZSetString(P, Len, Result)
    else
    begin
      SetLength(Tmp, 4001);
      Len := FPlainDriver.dbconvert(FHandle, Ord(DT), Pointer(P), Len,
        Ord(tdsChar), Pointer(tmp), 4001);
      FDBLibConnection.CheckDBLibError(lcOther, 'GETSTRING');
      while (Len > 0) and (tmp[Len] = ' ') do Dec(Len);
      Result := USASCII7ToUnicodeString(Pointer(tmp), Len);
    end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETSTRING');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  CheckClosed;
  CheckColumnIndex(ColumnIndex);

  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  FDataProvider.GetColData(ColumnIndex+1, Data, DL); //hint DBLib isn't #0 terminated @all
  {$ELSE}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  DT := DBLibColTypeCache[ColumnIndex];
  LastWasNull := Data = nil;
  if LastWasNull then
    Result := ''
  else
  begin
    Result := '';
    if (DT = tdsChar) or (DT = tdsText) then
    begin
      while (DL > 0) and (AnsiChar((PAnsiChar(Data)-DL - 1)^) = AnsiChar(' ')) do
              Dec(DL);
      if DL > 0 then
      begin
        SetLength(Result, DL);
        {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Data^, Pointer(Result)^, DL);
      end;
    end else
    if DT = tdsUnique then
      FRawTemp := GUIDToRaw(PGUID(Data)^)
    else
    if (DT = tdsImage) then
      ZSetString(Data, DL, Result)
    else
    begin
      SetLength(Result, 4001);
      DL := FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL,
        Ord(tdsChar), Pointer(Result), Length(Result));
      while (DL > 0) and (Result[DL] = ' ') do
          Dec(DL);
      SetLength(Result, DL);
    end;
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETSTRING');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZDBLibResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all
  LastWasNull := Data = nil;

  Result := False;
  if Data <> nil then
  begin
    if DT = tdsBit then
      Result := System.PBoolean(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsBit),
        @Result, SizeOf(Result));
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETBOOLEAN');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetByte(ColumnIndex: Integer): Byte;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all

  LastWasNull := Data = nil;
  Result := 0;
  if Data <> nil then
  begin
    if DT = tdsInt1 then
      Result := PByte(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsInt1),
        @Result, SizeOf(Result));
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETBYTE');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all

  LastWasNull := Data = nil;
  Result := 0;
  if Data <> nil then
  begin
    if DT = tdsInt2 then
      Result := PSmallInt(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsInt2),
        @Result, SizeOf(Result));
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GetSmall');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetInt(ColumnIndex: Integer): Integer;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
    if DT = tdsInt4 then
      Result := PInteger(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsInt4),
        @Result, SizeOf(Result));
  FDBLibConnection.CheckDBLibError(lcOther, 'GETINT');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetLong(ColumnIndex: Integer): Int64;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
    case DT of
      tdsInt1: Result := PByte(Data)^;
      tdsInt2: Result := PSmallInt(Data)^;
      tdsInt4: Result := PInteger(Data)^;
      tdsInt8: Result := PInt64(Data)^;
      else FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsInt8),
        @Result, SizeOf(Int64));
    end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETLONG');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetFloat(ColumnIndex: Integer): Single;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
  begin
    if DT = tdsFlt4 then
      Result := PSingle(Data)^
    else
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsFlt4),
        @Result, SizeOf(Result));
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETFLOAT');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZDBLibResultSet.GetDouble(ColumnIndex: Integer): Double;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
    case DT of
      tdsInt1: Result := PByte(Data)^;
      tdsInt2: Result := PSmallInt(Data)^;
      tdsInt4: Result := PInteger(Data)^;
      tdsInt8: Result := PInt64(Data)^;
      tdsFlt4: Result := PSingle(Data)^;
      tdsFlt8: Result := PDouble(Data)^;
      else FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsFlt8),
          @Result, SizeOf(Result));
    end
  else Result := 0;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETDOUBLE');
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
function TZDBLibResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  Result := GetDouble(ColumnIndex);
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
function TZDBLibResultSet.GetBytes(ColumnIndex: Integer): TBytes;
var
  DL: Integer;
  Data: Pointer;
begin
  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  Inc(ColumnIndex);
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all
  FDBLibConnection.CheckDBLibError(lcOther, 'GETBYTES');
  LastWasNull := Data = nil;

  Result := BufferToBytes(Data, DL);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
  Result := System.Int(GetTimestamp(ColumnIndex));
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZDBLibResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
  Result := Frac(GetTimestamp(ColumnIndex));
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
function TZDBLibResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  DL: Integer;
  Data: Pointer;
  DT: TTDSType;
  TempDate: DBDATETIME;
  tdsTempDate: TTDSDBDATETIME;
  Failed: Boolean;
begin
  DT := DBLibColTypeCache[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}];
  {$IFDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex +1; //DBLib -----> Col/Param starts whith index 1
  {$ENDIF}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all
  LastWasNull := Data = nil;

  Result := 0;
  if Data <> nil then
  begin
    //Perfect conversion no need to crack and reencode the date.
    if DT = tdsDateTime then
      if (FPlainDriver.GetDBLibraryVendorType = lvtFreeTDS) then //type diff
        Result := PTDSDBDATETIME(Data)^.dtdays + 2 + (PTDSDBDATETIME(Data)^.dttime / 25920000)
      else
        Result := PDBDATETIME(Data)^.dtdays + 2 + (PDBDATETIME(Data)^.dttime / 25920000)
    else if (DT in [tdsNText, tdsNVarChar, tdsText, tdsVarchar, tdsChar]) then
      if AnsiChar((PAnsiChar(Data)+2)^) = AnsiChar(':') then
        Result := RawSQLTimeToDateTime(Data, DL, ConSettings^.ReadFormatSettings, Failed)
      else if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Word(DL)) <= 4 then
          Result := RawSQLTimeStampToDateTime(Data, DL, ConSettings^.ReadFormatSettings, Failed)
      else
        Result := RawSQLTimeToDateTime(Data, DL, ConSettings^.ReadFormatSettings, Failed)
    else if (FPlainDriver.GetDBLibraryVendorType = lvtFreeTDS) then begin//type diff
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsDateTime),
        @tdsTempDate, SizeOf(tdsTempDate));
      Result := tdsTempDate.dtdays + 2 + (tdsTempDate.dttime / 25920000);
    end else begin
      FPlainDriver.dbconvert(FHandle, Ord(DT), Data, DL, Ord(tdsDateTime),
        @TempDate, SizeOf(TempDate));
      Result := TempDate.dtdays + 2 + (TempDate.dttime / 25920000);
    end;
  end;
  FDBLibConnection.CheckDBLibError(lcOther, 'GETTIMESTAMP');
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZDBLibResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  DL: Integer;
  Data: Pointer;
  TempAnsi: RawByteString;
begin
  {$IFDEF GENERIC_INDEX}
  //DBLib -----> Col/Param starts whith index 1
  FDataProvider.GetColData(ColumnIndex+1, Data, DL); //hint DBLib isn't #0 terminated @all
  {$ELSE}
  FDataProvider.GetColData(ColumnIndex, Data, DL); //hint DBLib isn't #0 terminated @all
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  LastWasNull := Data = nil;

  Result := nil;
  if not LastWasNull then
    case TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnType of
      stBytes, stBinaryStream:
        Result := TZAbstractBlob.CreateWithData(Data, DL);
      stAsciiStream, stUnicodeStream:
        begin
          if (DL = 1) and (AnsiChar(PAnsiChar(Data)^) = AnsiChar(' ')) then DL := 0; //improve empty lobs, where len = 1 but string should be ''
          Result := TZAbstractClob.CreateWithData(Data, DL,
            FDBLibConnection.GetServerAnsiCodePage, ConSettings);
        end;
      stString, stUnicodeString:
        if TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage = zCP_NONE then
          case ZDetectUTF8Encoding(Data, DL) of
            etUTF8:
              begin
                TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage := zCP_UTF8;
                Result := TZAbstractClob.CreateWithData(Data, DL, zCP_UTF8, ConSettings);
              end;
            etAnsi:
              begin
                TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage := ConSettings^.ClientCodePage^.CP;
                Result := TZAbstractClob.CreateWithData(Data, DL, ConSettings^.ClientCodePage^.CP, ConSettings);
              end;
            else //ASCII7
              Result := TZAbstractClob.CreateWithData(Data, DL, zCP_us_ascii, ConSettings);
          end
        else
          Result := TZAbstractClob.CreateWithData(Data, DL,
            TZColumnInfo(ColumnsInfo[ColumnIndex]).ColumnCodePage, ConSettings);
      else
      begin
        TempAnsi := InternalGetString(ColumnIndex);
        Result := TZAbstractClob.CreateWithData(PAnsiChar(TempAnsi),
          Length(TempAnsi), ConSettings^.ClientCodePage^.CP, ConSettings);
      end;
    end;
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
function TZDBLibResultSet.Next: Boolean;
begin
  Result := FDataProvider.Next;
end;


{ TZDBLibCachedResolver }

{**
  Creates a DBLib specific cached resolver object.
  @param PlainDriver a native DBLib plain driver.
  @param Handle a DBLib specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZDBLibCachedResolver.Create(const Statement: IZStatement;
  const Metadata: IZResultSetMetadata);
begin
  inherited Create(Statement, Metadata);

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := -1;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZDBLibCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Statement: IZStatement;
  ResultSet: IZResultSet;
  I: Integer;
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  { Defines an index of autoincrement field. }
  if FAutoColumnIndex = -1 then
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if Metadata.IsAutoIncrement(I) then
      begin
        FAutoColumnIndex := I;
        Break;
      end;

  if (UpdateType = utInserted) and (FAutoColumnIndex > InvalidDbcIndex)
    and OldRowAccessor.IsNull(FAutoColumnIndex) then
  begin
    Statement := Connection.CreateStatement;
    ResultSet := Statement.ExecuteQuery('SELECT @@IDENTITY');
    try
      if ResultSet.Next then
        NewRowAccessor.SetLong(FAutoColumnIndex, ResultSet.GetLong(FirstDbcIndex));
    finally
      ResultSet.Close;
      Statement.Close;
    end;
  end;
end;

{ TZDblibResultSetMetadata }
procedure TZDblibResultSetMetadata.SetColumnCodePageFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo;
  const TableColumns: IZResultSet);
var ColTypeName: string;
  P: PChar;
begin
  if ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then begin
    ColTypeName := TableColumns.GetString(TableColColumnTypeNameIndex);
    P := Pointer(ColTypeName);
    if P = nil
    then ColumnInfo.ColumnCodePage := FConSettings^.ClientCodePage^.CP
    else if (Length(ColTypeName) > 3) and SameText(P, PChar('UNI'), 3) //sybase only and FreeTDS does not map the type to UTF8?
      then ColumnInfo.ColumnCodePage := zCP_UTF16{UNICHAR, UNIVARCHAR}
      else if (Ord(P^) or $20 = Ord('n')) and not FConSettings^.ClientCodePage^.IsStringFieldCPConsistent
        then ColumnInfo.ColumnCodePage := zCP_UTF8 {NTEXT, NVARCHAR, NCHAR}
        else ColumnInfo.ColumnCodePage := FConSettings^.ClientCodePage^.CP; //assume server CP instead
  end else
    ColumnInfo.ColumnCodePage := zCP_NONE;
end;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
end.
