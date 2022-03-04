{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Ado Resultset common functionality              }
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

unit ZDbcAdoResultSet;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ADO}
uses
{$IFDEF USE_SYNCOMMONS}
  SynCommons, SynTable,
{$ENDIF USE_SYNCOMMONS}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs{$ELSE}Types{$ENDIF},
  Windows, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcGenericResolver,
  ZDbcCachedResultSet, ZDbcCache, ZDbcResultSet, ZDbcResultsetMetadata, ZCompatibility, ZPlainAdo;

type
  {** Implements SQLite ResultSet Metadata. }
  TZADOResultSetMetadata = class(TZAbstractResultSetMetadata)
  protected
    procedure ClearColumn(ColumnInfo: TZColumnInfo); override;
  end;

  {** Implements Ado ResultSet. }
  TZAdoResultSet = class(TZAbstractResultSet)
  private
    AdoColTypeCache: TIntegerDynArray;
    AdoColumnCount: Integer;
    FFirstFetch: Boolean;
    FAdoRecordSet: ZPlainAdo.RecordSet;
    FFields: Fields15;
    FField20: Field20;
    FValueAddr: Pointer;
    FValueType: Word;
    FColValue: OleVariant;
    FTinyBuffer: array[Byte] of Byte;
  protected
    procedure Open; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      const AdoRecordSet: ZPlainAdo.RecordSet);
    procedure AfterClose; override;
    procedure ResetCursor; override;
    function Next: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function GetRow: NativeInt; override;
    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetString(ColumnIndex: Integer): String; override;
    function GetAnsiString(ColumnIndex: Integer): AnsiString; override;
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    function GetRawByteString(ColumnIndex: Integer): RawByteString; override;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWideString; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetUInt(ColumnIndex: Integer): Cardinal; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetCurrency(ColumnIndex: Integer): Currency; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;
  end;

  {** Implements a cached resolver with Ado specific functionality. }
  TZAdoCachedResolver = class (TZGenericCachedResolver, IZCachedResolver)
  private
    FHandle: ZPlainAdo.Command;
    FAutoColumnIndex: Integer;
  public
    constructor Create(const Handle: ZPlainAdo.Connection;
      const Statement: IZStatement; const Metadata: IZResultSetMetadata);

    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); override;
  end;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  Variants, {$IFDEF FPC}ZOleDB{$ELSE}OleDB{$ENDIF}, ActiveX,
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF} //need for inlined FloatToRaw
  ZMessages, ZDbcAdoUtils, ZEncoding, ZFastCode, ZClasses, ZDbcUtils;

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query string.
  @param AdoRecordSet a ADO recordset object, the source of the ResultSet.
}
constructor TZAdoResultSet.Create(const Statement: IZStatement; const SQL: string; const AdoRecordSet: ZPlainAdo.RecordSet);
begin
  inherited Create(Statement, SQL,
    TZADOResultSetMetadata.Create(Statement.GetConnection.GetMetadata, SQL, Self),
    Statement.GetConnection.GetConSettings);
  FAdoRecordSet := AdoRecordSet;
  Open;
end;

{**
  Opens this recordset and initializes the Column information.
}
procedure TZAdoResultSet.Open;
var
  OleDBRowset: IUnknown;
  OleDBColumnsInfo: IColumnsInfo;
  pcColumns: NativeUInt;
  prgInfo, OriginalprgInfo: PDBColumnInfo;
  ppStringsBuffer: PWideChar;
  I,j: Integer;
  FieldSize: Integer;
  ColumnInfo: TZColumnInfo;
  ColName: string;
  ColType: Integer;
  F: ZPlainAdo.Field20;
  Prop: Property_;
  function StringFromVar(const V: OleVariant): String;
  begin
    if not VarIsStr(V)
    then Result := ''
    else Result := V;
  end;
begin
//Check if the current statement can return rows
  if not Assigned(FAdoRecordSet) or (FAdoRecordSet.State = adStateClosed) then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  (FAdoRecordSet as ADORecordsetConstruction).Get_Rowset(OleDBRowset);
  OleDBRowset.QueryInterface(IColumnsInfo, OleDBColumnsInfo);

  OleDBColumnsInfo.GetColumnInfo(pcColumns{%H-}, prgInfo, ppStringsBuffer);
  OriginalprgInfo := prgInfo;

  { Fills the column info }
  ColumnsInfo.Clear;
  AdoColumnCount := FAdoRecordSet.Fields.Count;
  SetLength(AdoColTypeCache, AdoColumnCount);

  if Assigned(prgInfo) then
    if prgInfo.iOrdinal = 0 then
      Inc({%H-}NativeInt(prgInfo), SizeOf(TDBColumnInfo));

  for I := 0 to AdoColumnCount - 1 do
  begin
    ColumnInfo := TZColumnInfo.Create;

    F := FAdoRecordSet.Fields.Item[I];
    {$IFDEF UNICODE}
    ColName := F.Name;
    {$ELSE}
    ColName := PUnicodeToString(Pointer(F.Name), Length(F.Name), ConSettings.CTRL_CP);
    {$ENDIF}
    ColType := F.Type_;
    ColumnInfo.ColumnLabel := ColName;

    for j := 0 to F.Properties.Count -1 do begin
      Prop := F.Properties.Item[j];
      if Prop.Name = 'BASECOLUMNNAME' then
        ColumnInfo.ColumnName := StringFromVar(Prop.Value)
      else if Prop.Name = 'BASETABLENAME' then
        ColumnInfo.TableName := StringFromVar(Prop.Value)
      else if Prop.Name = 'BASECATALOGNAME' then
        ColumnInfo.CatalogName := StringFromVar(Prop.Value)
      else if Prop.Name = 'BASESCHEMANAME' then
        ColumnInfo.SchemaName := StringFromVar(Prop.Value)
      else if (Prop.Name = 'ISAUTOINCREMENT') and not (TVarData(Prop.Value).VType in [varEmpty, varNull]) then
        ColumnInfo.AutoIncrement := Prop.Value
    end;

    ColumnInfo.ColumnType := ConvertAdoToSqlType(ColType, ConSettings.CPType);
    FieldSize := F.DefinedSize;
    if FieldSize < 0 then
      FieldSize := 0;
    if F.Type_ = adGuid
    then ColumnInfo.ColumnDisplaySize := 38
    else ColumnInfo.ColumnDisplaySize := FieldSize;
    if ColType = adCurrency then begin
      ColumnInfo.Precision := 19;
      ColumnInfo.Scale := 4;
      ColumnInfo.Currency := True;
    end else if ColType in [adDecimal, adNumeric] then begin
      ColumnInfo.Precision := F.Precision;
      ColumnInfo.Scale := F.NumericScale;
    end else begin
      ColumnInfo.Precision := FieldSize;
    end;
    ColumnInfo.Signed := ColType in [adTinyInt, adSmallInt, adInteger, adBigInt, adDouble, adSingle, adCurrency, adDecimal, adNumeric];

    ColumnInfo.Writable := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0) and (F.Properties.Item['BASECOLUMNNAME'].Value <> null) and not ColumnInfo.AutoIncrement;
    ColumnInfo.ReadOnly := (prgInfo.dwFlags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0) or ColumnInfo.AutoIncrement;
    ColumnInfo.Searchable := (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) = 0;
    if (prgInfo.dwFlags and DBCOLUMNFLAGS_ISLONG) <> 0 then
    case ColumnInfo.ColumnType of
      stString: ColumnInfo.ColumnType := stAsciiStream;
      stUnicodeString: ColumnInfo.ColumnType := stUnicodeStream;
    end;

    ColumnsInfo.Add(ColumnInfo);

    AdoColTypeCache[I] := ColType;
    Inc({%H-}NativeInt(prgInfo), SizeOf(TDBColumnInfo));  //M.A. Inc(Integer(prgInfo), SizeOf(TDBColumnInfo));
  end;
  if Assigned(ppStringsBuffer) then ZAdoMalloc.Free(ppStringsBuffer);
  if Assigned(OriginalprgInfo) then ZAdoMalloc.Free(OriginalprgInfo);
  FFirstFetch := True;
  inherited;
end;

{**
  Releases this <code>ResultSet</code> object's database and
  ADO resources immediately instead of waiting for
  this to happen when it is automatically closed.

  <P><B>Note:</B> A <code>ResultSet</code> object
  is automatically closed by the
  <code>Statement</code> object that generated it when
  that <code>Statement</code> object is closed,
  re-executed, or is used to retrieve the next result from a
  sequence of multiple results. A <code>ResultSet</code> object
  is also automatically closed when it is garbage collected.
}
procedure TZAdoResultSet.AfterClose;
begin
  FAdoRecordSet := nil;
  inherited AfterClose;
end;

procedure TZAdoResultSet.ResetCursor;
begin
  { Resync the Adorecordsets leads to pain with huge collection of Data !!}
  FFields := nil;
  FField20 := nil;
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
function TZAdoResultSet.Next: Boolean;
begin
  Result := False;
  FFields := nil;
  if (FAdoRecordSet = nil) or (FAdoRecordSet.BOF and FAdoRecordSet.EOF) then
    Exit;
  if FAdoRecordSet.BOF then
    FAdoRecordSet.MoveFirst
  else if not FAdoRecordSet.EOF and not FFirstFetch then
    FAdoRecordSet.MoveNext;
  FFirstFetch := False;
  Result := not FAdoRecordSet.EOF;
  if Result then FFields := FAdoRecordSet.Fields;
  RowNo := RowNo +1;
  if Result then
    LastRowNo := RowNo;
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
function TZAdoResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  FFields := nil;
  if FAdoRecordSet.EOF or FAdoRecordSet.BOF then
     FAdoRecordSet.MoveFirst;
  if Row > 0 then
    FAdoRecordSet.Move(Row - 1, adBookmarkFirst)
  else
    FAdoRecordSet.Move(Abs(Row) - 1, adBookmarkLast);
  Result := not (FAdoRecordSet.EOF or FAdoRecordSet.BOF);
  if Result then FFields := FAdoRecordSet.Fields;
end;

{**
  Retrieves the current row number.  The first row is number 1, the
  second number 2, and so on.
  @return the current row number; <code>0</code> if there is no current row
}
function TZAdoResultSet.GetRow: NativeInt;
begin
  if FAdoRecordSet.EOF or FAdoRecordSet.BOF then
    Result := -1
  else
    Result := FAdoRecordSet.AbsolutePosition;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZAdoResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex-1;
  {$ENDIF}
  if (FFields = nil) then begin
    Result := True;
    FValueAddr := nil;
  end else begin
    FField20 := FFields.Get_Item(ColumnIndex);
    FColValue := FField20.Value;
    FValueType := tagVariant(FColValue).vt;
    if (FValueType = VT_NULL) or (FValueType = VT_EMPTY) then begin
      Result := True;
      FValueAddr := nil;
    end else begin
      Result := False;
      if FValueType and VT_BYREF = VT_BYREF then begin
        FValueType := FValueType xor VT_BYREF;
        FValueAddr := tagVariant(FColValue).unkVal;
      end else if FValueType = VT_DECIMAL
        then FValueAddr := @FColValue
        else if (FValueType = VT_BSTR)
          then FValueAddr := tagVariant(FColValue).bstrVal
          else FValueAddr := @tagVariant(FColValue).bVal;
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
function TZAdoResultSet.GetString(ColumnIndex: Integer): String;
var P: {$IFDEF UNICODE}PWidechar{$ELSE}PAnsiChar{$ENDIF};
  L: NativeUInt;
begin
  {$IFDEF UNICODE}
  P := GetPWideChar(ColumnIndex, L);
  if (P <> nil) and (L > 0) then
    if P = Pointer(FUniTemp)
    then Result := FUniTemp
    else System.SetString(Result, P, L)
  else Result := '';
  {$ELSE}
  if ConSettings.AutoEncode then
    if ConSettings.CPType = cCP_UTF8
    then Result := GetUTF8String(ColumnIndex)
    else Result := GetAnsiString(ColumnIndex)
  else begin
    P := GetPAnsiChar(ColumnIndex, L);
    if (P <> nil) and (L > 0) then
      if P = Pointer(FRawTemp)
      then Result := FRawTemp
      else System.SetString(Result, P, L)
    else Result := '';
  end;
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
function TZAdoResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
var Len: NativeUInt;
  PW: PWideChar;
  PA: PAnsiChar absolute PW;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
     Result := ''
  else with FField20 do begin
    case Type_ of
      adChar, adWChar, adVarChar, adLongVarChar, adBSTR, adVarWChar,
      adLongVarWChar: begin
                        PW := GetPWidechar(ColumnIndex, Len);
                        Result := PUnicodeToRaw(PW, Len, ZOSCodePage);
                      end;
      else            begin
                        PA := GetPAnsiChar(ColumnIndex, Len);
                        ZSetString(PA, Len, Result);
                      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
var Len: NativeUInt;
  PW: PWideChar;
  PA: PAnsiChar absolute PW;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
     Result := ''
  else with FField20 do begin
    case Type_ of
      adChar, adWChar, adVarChar, adLongVarChar, adBSTR, adVarWChar,
      adLongVarWChar: begin
                        PW := GetPWidechar(ColumnIndex, Len);
                        Result := PUnicodeToRaw(PW, Len, zCP_UTF8);
                      end;
      else            begin
                        PA := GetPAnsiChar(ColumnIndex, Len);
                        ZSetString(PA, Len, Result);
                      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>UTF8String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
var P: PAnsiChar;
  L: NativeUInt;
begin
  P := GetPAnsiChar(ColumnIndex, L);
  if (P <> nil) and (L > 0) then
    if P = Pointer(FRawTemp)
    then Result := FRawTemp
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    else ZSetString(P, L, Result)
    {$ELSE}
    else System.SetString(Result, P, L)
    {$ENDIF}
  else Result := EmptyRaw;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the value in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetPAnsiChar(ColumnIndex: Integer;
  out Len: NativeUInt): PAnsiChar;
var E: Extended;
  PW: PWideChar;
label Set_From_Buf;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else with FField20 do begin
    case tagVARIANT(FColValue).vt of
      VT_BOOL:        if PWordBool(FValueAddr)^ then begin
                        Result := Pointer(BoolStrsRaw[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsRaw[False]);
                        Len := 5;
                      end;
      VT_UI1, VT_UI2, VT_UI4, VT_UINT: begin
                        IntToRaw(GetUInt(ColumnIndex), @FTinyBuffer[0], @Result);
                        goto Set_From_Buf;
                      end;
      VT_I1,  VT_I2,  VT_I4,  VT_INT, VT_HRESULT, VT_ERROR: begin
                        IntToRaw(GetInt(ColumnIndex), @FTinyBuffer[0], @Result);
                        goto Set_From_Buf;
                      end;
      VT_UI8:         begin
                        IntToRaw(PUInt64(FValueAddr)^, @FTinyBuffer[0], @Result);
                        goto Set_From_Buf;
                      end;
      VT_I8:          begin
                        IntToRaw(PInt64(FValueAddr)^, @FTinyBuffer[0], @Result);
                        goto Set_From_Buf;
                      end;
      VT_CY:          begin
                        CurrToRaw(PCurrency(FValueAddr)^, @FTinyBuffer[0], @Result);
Set_From_Buf:           Len := Result - PAnsiChar(@fTinyBuffer[0]);
                        Result := @fTinyBuffer[0];
                      end;
      VT_DECIMAL:     begin
                        Result := @FTinyBuffer[0];
                        E := UInt64(PDecimal(Result).Lo64) / ZFastCode.UInt64Tower[PDecimal(Result).scale];
                        if PDecimal(Result).sign > 0 then
                          E := -E;
                        Len := FloatToSQLRaw(E, Result);
                      end;
      VT_R4:          begin
                        Result := @FTinyBuffer[0];
                        Len := FloatToSQLRaw(PSingle(FValueAddr)^, Result);
                      end;
      VT_R8:          begin
                        Result := @FTinyBuffer[0];
                        Len := FloatToSQLRaw(PDouble(FValueAddr)^, Result);
                      end;
      VT_DATE:        case Type_ of
                        adDate, adDBDate: begin
                            Result := @FTinyBuffer[0];
                            Len := DateTimeToRawSQLDate(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                        adDBTime: begin
                            Result := @FTinyBuffer[0];
                            Len := DateTimeToRawSQLTime(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                        else begin
                            Result := @FTinyBuffer[0];
                            Len := DateTimeToRawSQLTimeStamp(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                      end;
      else case Type_ of
        adVarBinary,
        adLongVarBinary,
        adBinary:     begin
                        Result := TVarData(FColValue).VArray.Data;
                        Len := ActualSize;
                      end;
        else begin
          PW := GetPWideChar(ColumnIndex, Len);
          FRawTemp := PUnicodeToRaw(PW, Len, ConSettings.CTRL_CP);
          Result := Pointer(FRawTemp);
          Len := Length(FRawTemp);
        end;
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PWideChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the length of the value in codepoints
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
var E: Extended;
label Set_From_Buf;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := nil;
    Len := 0;
  end else with FField20 do begin
    case FValueType of
      VT_BOOL:        if PWordBool(FValueAddr)^ then begin
                        Result := Pointer(BoolStrsW[True]);
                        Len := 4;
                      end else begin
                        Result := Pointer(BoolStrsW[False]);
                        Len := 5;
                      end;
      VT_UI1, VT_UI2, VT_UI4, VT_UINT: begin
                        IntToUnicode(GetUInt(ColumnIndex), @FTinyBuffer[0], @Result);
                        goto Set_From_Buf; end;
      VT_I1,  VT_I2,  VT_I4,  VT_INT, VT_HRESULT, VT_ERROR: begin
                        IntToUnicode(GetInt(ColumnIndex), @FTinyBuffer[0], @Result);
                        goto Set_From_Buf;
                      end;
      VT_UI8:         begin
                        IntToUnicode(PUInt64(FValueAddr)^, @FTinyBuffer[0], @Result);
                        goto Set_From_Buf;
                      end;
      VT_I8:          begin
                        IntToUnicode(PInt64(FValueAddr)^, @FTinyBuffer[0], @Result);
                        goto Set_From_Buf;
                      end;
      VT_CY:          begin
                        CurrToUnicode(PCurrency(FValueAddr)^, @FTinyBuffer[0], @Result);
Set_From_Buf:           Len := Result - PWideChar(@fTinyBuffer[0]);
                        Result := @fTinyBuffer[0];
                      end;
      VT_DECIMAL:     begin
                        Result := @FTinyBuffer[0];
                        E := UInt64(PDecimal(Result).Lo64) / ZFastCode.UInt64Tower[PDecimal(Result).scale];
                        if PDecimal(Result).sign > 0 then
                          E := -E;
                        Len := FloatToSQLUnicode(E, Result);
                      end;
      VT_R4:          begin
                        Result := @FTinyBuffer[0];
                        Len := FloatToSQLUnicode(PSingle(FValueAddr)^, Result);
                      end;
      VT_R8:          begin
                        Result := @FTinyBuffer[0];
                        Len := FloatToSQLUnicode(PDouble(FValueAddr)^, Result);
                      end;
      VT_DATE:        case Type_ of
                        adDate, adDBDate: begin
                            Result := @FTinyBuffer[0];
                            Len := DateTimeToUnicodeSQLDate(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                        adDBTime: begin
                            Result := @FTinyBuffer[0];
                            Len := DateTimeToUnicodeSQLTime(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                        else begin
                            Result := @FTinyBuffer[0];
                            Len := DateTimeToUnicodeSQLTimeStamp(TDateTime(PDouble(FValueAddr)^),
                              Result, ConSettings.ReadFormatSettings, False);
                          end;
                      end;
      else case Type_ of
        adGUID:       begin
                        Result := FValueAddr;
                        Len := 38;
                      end;
        adChar:       begin
                        Result := FValueAddr;
                        Len := ZDbcUtils.GetAbsorbedTrailingSpacesLen(Result, ActualSize);
                      end;
        adVarChar,
        adLongVarChar: begin
                        Result := FValueAddr;
                        Len := ActualSize;
                      end;
        adWChar:      begin
                        Result := FValueAddr;
                        Len := ZDbcUtils.GetAbsorbedTrailingSpacesLen(Result, ActualSize shr 1);
                      end;
        adLongVarWChar,
        adVarWChar:   begin
                        Result := FValueAddr;
                        Len := ActualSize shr 1;
                      end;
        else          try
                        FUniTemp := FColValue;
                        Len := Length(FUniTemp);
                        Result := Pointer(FUniTemp);
                      except
                        Len := 0;
                        Result := nil;
                        LastWasNull := True;
                      end;
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>WideString</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetUnicodeString(ColumnIndex: Integer): ZWideString;
var P: PWideChar;
  L: NativeUInt;
begin
  P := GetPWideChar(ColumnIndex, L);
  if LastWasNull or (L = 0) then
    Result := ''
  else if P = Pointer(FUniTemp)
    then Result := FUniTemp
    else System.SetString(Result, P, L);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAdoResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
var P: PWideChar;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := False;
  end else case tagVARIANT(FColValue).vt of
    VT_BOOL:        Result := PWordBool(FValueAddr)^;
    VT_UI1, VT_UI2, VT_UI4, VT_UINT: Result := GetUInt(ColumnIndex) <> 0;
    VT_I1, VT_I2, VT_I4, VT_INT:  Result := GetInt(ColumnIndex) <> 0;
    VT_HRESULT, VT_ERROR:  Result := PHResult(FValueAddr)^ <> 0;
    VT_UI8:         Result := PUInt64(FValueAddr)^ <> 0;
    VT_I8:          Result := PInt64(FValueAddr)^ <> 0;
    VT_CY:          Result := PCurrency(FValueAddr)^ <> 0;
    VT_DECIMAL:     Result := PDecimal(@FColValue).Lo64 <> 0;
    VT_R4, VT_R8, VT_DATE: Result := Trunc(GetDouble(ColumnIndex)) <> 0;
    else begin
      P := GetPWideChar(ColumnIndex, Len);
      Result := StrToBoolEx(P, True, False);
    end;
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
function TZAdoResultSet.GetInt(ColumnIndex: Integer): Integer;
var P: PWideChar;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := 0;
  end else case tagVARIANT(FColValue).vt of
    VT_BOOL:        Result := Ord(PWord(FValueAddr)^ <> 0);
    VT_UI1:         Result := PByte(FValueAddr)^;
    VT_UI2:         Result := PWord(FValueAddr)^;
    VT_UI4:         Result := PInteger(FValueAddr)^;
    VT_UINT:        Result := PCardinal(FValueAddr)^;
    VT_I1:          Result := PShortInt(FValueAddr)^;
    VT_I2:          Result := PSmallInt(FValueAddr)^;
    VT_I4:          Result := Pinteger(FValueAddr)^;
    VT_INT:         Result := tagVARIANT(FColValue).intVal;
    VT_HRESULT,
    VT_ERROR:       Result := PHResult(FValueAddr)^;
    VT_UI8, VT_I8, VT_CY, VT_DECIMAL, VT_R4, VT_R8, VT_DATE: Result := GetLong(ColumnIndex);
    else begin
      P := GetPWideChar(ColumnIndex, Len);
      Result := UnicodeToIntDef(P, P+Len, 0);
    end;
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
function TZAdoResultSet.GetLong(ColumnIndex: Integer): Int64;
var P: PWideChar;
  PD: PDecimal absolute P;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := 0;
  end else with FField20 do begin
    case tagVARIANT(FColValue).vt of
      VT_BOOL, VT_UI1, VT_UI2, VT_UI4, VT_UINT: Result := GetUInt(ColumnIndex);
      VT_I1, VT_I2, VT_I4, VT_INT:  Result := GetInt(ColumnIndex);
      VT_HRESULT, VT_ERROR:  Result := PHResult(FValueAddr)^;
      VT_UI8:         Result := PUInt64(FValueAddr)^;
      VT_I8:          Result := PInt64(FValueAddr)^;
      VT_CY:          Result := PInt64(FValueAddr)^ div 10000;
      VT_DECIMAL: begin
                    PD := @FColValue;
                    Result := Int64(PD.Lo64);
                    if PD.scale > 0 then
                      Result := Result div Int64Tower[PD.scale];
                    if PD.sign > 0 then
                      Result := -Result;
                  end;
      VT_R4, VT_R8, VT_DATE: Result := Trunc(GetDouble(ColumnIndex));
      else begin
        P := GetPWideChar(ColumnIndex, Len);
        Result := UnicodeToInt64Def(P, P+Len, 0);
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>uint</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
var P: PWideChar;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := 0;
  end else with FField20 do begin
    case tagVARIANT(FColValue).vt of
      VT_BOOL:        Result := Ord(PWord(FValueAddr)^ <> 0);
      VT_UI1:         Result := PByte(FValueAddr)^;
      VT_UI2:         Result := PWord(FValueAddr)^;
      VT_UI4:         Result := PInteger(FValueAddr)^;
      VT_UINT:        Result := PCardinal(FValueAddr)^;
      VT_I1:          Result := PShortInt(FValueAddr)^;
      VT_I2:          Result := PSmallInt(FValueAddr)^;
      VT_I4:          Result := PInteger(FValueAddr)^;
      VT_INT:         Result := tagVARIANT(FColValue).intVal;
      VT_HRESULT,
      VT_ERROR:       Result := PHResult(FValueAddr)^;
      VT_UI8, VT_I8, VT_CY, VT_DECIMAL, VT_R4, VT_R8, VT_DATE: Result := GetULong(ColumnIndex);
      else begin
        P := GetPWideChar(ColumnIndex, Len);
        Result := UnicodeToUInt64Def(P, P+Len, 0);
      end;
    end;
  end;
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
function TZAdoResultSet.GetULong(ColumnIndex: Integer): UInt64;
var P: PWideChar;
  PD: PDecimal absolute P;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then begin
    Result := 0;
  end else with FField20 do begin
    case tagVARIANT(FColValue).vt of
      VT_BOOL, VT_UI1, VT_UI2, VT_UI4, VT_UINT: Result := GetUInt(ColumnIndex);
      VT_I1, VT_I2, VT_I4, VT_INT:  Result := GetInt(ColumnIndex);
      VT_HRESULT, VT_ERROR:  Result := PHResult(FValueAddr)^;
      VT_UI8:         Result := PUInt64(FValueAddr)^;
      VT_I8:          Result := PInt64(FValueAddr)^;
      VT_CY:          Result := PInt64(FValueAddr)^ div 10000;
      VT_DECIMAL: begin
                    PD := @FColValue;
                    Result := UInt64(PD.Lo64);
                    if PD.scale > 0 then
                      Result := Result div Uint64(Int64Tower[PD.scale]);
                  end;
      VT_R4, VT_R8, VT_DATE: Result := Trunc(GetDouble(ColumnIndex));
      else begin
        P := GetPWideChar(ColumnIndex, Len);
        Result := UnicodeToUInt64Def(P, P+Len, 0);
      end;
    end;
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
function TZAdoResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  Result := GetDouble(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAdoResultSet.GetDouble(ColumnIndex: Integer): Double;
var PD: PDecimal;
  Len: NativeUInt;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
    Result := 0
  else with FField20 do begin
    case tagVARIANT(FColValue).vt of
      VT_R8:          Result := PDouble(FValueAddr)^;
      VT_R4:          Result := PSingle(FValueAddr)^;
      VT_DECIMAL:     begin
                        PD := @FColValue;
                        Result := Uint64(PDecimal(PD).Lo64) / ZFastCode.Int64Tower[PD.Scale];
                        if PD.sign > 0 then
                          Result := -Result;
                      end;
      VT_DATE:        Result := PDouble(FValueAddr)^;
      VT_BOOL, VT_UI1, VT_UI2, VT_UI4, VT_UINT: Result := GetUInt(ColumnIndex);
      VT_I1,  VT_I2,  VT_I4,  VT_INT, VT_HRESULT, VT_ERROR: Result := GetInt(ColumnIndex);
      VT_UI8:         Result := PUInt64(FValueAddr)^;
      VT_I8:          Result := PInt64(FValueAddr)^;
      VT_CY:          Result := PCurrency(FValueAddr)^;
      else  UnicodeToFloatDef(GetPWideChar(ColumnIndex, Len), WideChar('.'), 0, Result)
    end;
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
function TZAdoResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
var
  Len: NativeUint;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else with FField20, TZColumnInfo(ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]) do begin
    case FValueType of
      VT_BOOL:        Result := Ord(PWord(FValueAddr)^ <> 0);
      VT_UI1:         Result := PByte(FValueAddr)^;
      VT_UI2:         Result := PWord(FValueAddr)^;
      VT_UI4:         Result := PCardinal(FValueAddr)^;
      VT_UINT:        Result := PLongWord(FValueAddr)^;
      VT_UI8:         Result := PUInt64(FValueAddr)^;
      VT_I1:          Result := PShortInt(FValueAddr)^;
      VT_I2:          Result := PSmallInt(FValueAddr)^;
      VT_HRESULT,
      VT_ERROR,
      VT_I4:          Result := PInteger(FValueAddr)^;
      VT_I8:          Result := PInt64(FValueAddr)^;
      VT_INT:         Result := PLongInt(FValueAddr)^;
      VT_CY:          Result := PCurrency(FValueAddr)^;
      VT_DECIMAL:     begin
                        Result := UInt64(PDecimal(FValueAddr)^.Lo64) / UInt64Tower[PDecimal(FValueAddr)^.Scale];
                        if PDecimal(FValueAddr)^.Sign > 0 then
                          Result := -Result;
      end;
      VT_R4:          Result := PSingle(FValueAddr)^;
      VT_R8, VT_DATE: Result := PDouble(FValueAddr)^;
      else  UnicodeToFloatDef(GetPWideChar(ColumnIndex, Len), WideChar('.'), 0, Result)
    end;
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
function TZAdoResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
    Result := nil
  else with FField20 do
    case Type_ of
      adGUID:  begin
          SetLength(Result, 16);
          ValidGUIDToBinary(PWideChar(FValueAddr), Pointer(Result));
        end;
      adBinary,
      adVarBinary,
      adLongVarBinary:
          Result := BufferToBytes(TVarData(FColValue).VArray.Data, ActualSize);
      else Result := BufferToBytes(FValueAddr, ActualSize);
    end;
end;

function TZAdoResultSet.GetCurrency(ColumnIndex: Integer): Currency;
var
  Len: NativeUint;
  P: PWideChar;
  PD: PDecimal absolute P;
  i64: Int64 absolute Result;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else case tagVARIANT(FColValue).vt of
    VT_UI1, VT_I1, VT_UI2, VT_I2, VT_UI4, VT_I4, VT_UI8, VT_I8, VT_INT,
    VT_UINT, VT_HRESULT, VT_ERROR, VT_BOOL: Result := GetLong(FirstDbcIndex);
    VT_CY: Result := PCurrency(FValueAddr)^;
    VT_DECIMAL: begin
                  PD := PDecimal(@FColValue);
                  i64 := UInt64(PD.Lo64);
                  if PD.sign > 0 then
                    i64 := -i64;
                  if PD.scale < 4 then
                    i64 := i64 * ZFastCode.Int64Tower[4-PD.scale]
                  else if PD.scale > 4 then
                    i64 := i64 div ZFastCode.Int64Tower[PD.scale-4];
                end;
    VT_R4:      Result := PSingle(FValueAddr)^;
    VT_R8, VT_DATE: Result := PDouble(FValueAddr)^;
    VT_BSTR:    begin
                  P := GetPWidechar(ColumnIndex, Len);
                  UnicodeToFloatDef(P, WideChar('.'), Len);
                end;
    else        try   //should not happen
                  Result := FColValue;
                except
                  Result := 0;
                end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAdoResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var P: PWideChar;
  Len: NativeUint;
  Failed: Boolean;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else case tagVARIANT(FColValue).vt of
    VT_DATE: Result := Int(PDouble(FValueAddr)^);
    VT_BSTR:    begin
                  P := GetPWidechar(ColumnIndex, Len);
                  Result := UnicodeSQLDateToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed);
                  if Failed then
                    Result := Int(UnicodeSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed));
                end;
    else     Result := Int(GetDouble(ColumnIndex));
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
function TZAdoResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var P: PWideChar;
  Len: NativeUint;
  Failed: Boolean;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else case tagVARIANT(FColValue).vt of
    VT_DATE: Result := Frac(PDouble(FValueAddr)^);
    VT_BSTR:    begin
                  P := GetPWidechar(ColumnIndex, Len);
                  Result := UnicodeSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed);
                  if Failed then
                    Result := Frac(UnicodeSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed));
                end;
    else     Result := Frac(GetDouble(ColumnIndex));
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
function TZAdoResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var P: PWideChar;
  Len: NativeUint;
  Failed: Boolean;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else case tagVARIANT(FColValue).vt of
    VT_DATE: Result := PDouble(FValueAddr)^;
    VT_BSTR:    begin
                  P := GetPWidechar(ColumnIndex, Len);
                  Result := UnicodeSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed);
                end;
    else     Result := GetDouble(ColumnIndex);
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
function TZAdoResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var L: LengthInt;
label CLOB;
begin
  LastWasNull := IsNull(ColumnIndex); //sets fColValue variant
  if LastWasNull then
    Result := nil
  else with FField20 do
    case Type_ of
      adGUID:   begin L := 38; goto CLOB; end;
      adChar:   begin
                  L := ZDbcUtils.GetAbsorbedTrailingSpacesLen(PWidechar(FValueAddr), ActualSize);
                  goto CLOB; end;
      adWChar:  begin
                  L := ZDbcUtils.GetAbsorbedTrailingSpacesLen(PWidechar(FValueAddr), ActualSize shr 1);
                  goto CLOB; end;
      adVarChar,
      adLongVarChar: begin
                  L := ActualSize;
                  goto CLOB; end;
      adBSTR,
      adLongVarWChar,
      adVarWChar: begin
                  L := ActualSize shr 1;
CLOB:             Result := TZAbstractClob.CreateWithData(PWidechar(FValueAddr), L, ConSettings);
                end;

      adVarBinary,
      adLongVarBinary,
      adBinary:   Result := TZAbstractBlob.CreateWithData(TVarData(FColValue).VArray.Data, ActualSize);
      else
        Result := nil;
    end;
end;


{ TZAdoCachedResolver }

{**
  Creates a Ado specific cached resolver object.
  @param PlainDriver a native Ado plain driver.
  @param Handle a Ado specific query handle.
  @param Statement a related SQL statement object.
  @param Metadata a resultset metadata reference.
}
constructor TZAdoCachedResolver.Create(const Handle: ZPlainAdo.Connection;
  const Statement: IZStatement; const Metadata: IZResultSetMetadata);
var
  I: Integer;
begin
  inherited Create(Statement, Metadata);
  FHandle := ZPlainAdo.CoCommand.Create;
  FHandle._Set_ActiveConnection(Handle);
  FHandle.CommandText := 'SELECT @@IDENTITY';
  FHandle.CommandType := adCmdText;

  { Defines an index of autoincrement field. }
  FAutoColumnIndex := InvalidDbcIndex;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
    if Metadata.IsAutoIncrement(I) and
      (Metadata.GetColumnType(I) in [stByte,stShort,stWord,stSmall,stLongWord,stInteger,stULong,stLong]) then
    begin
      FAutoColumnIndex := I;
      Break;
    end;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZAdoCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Recordset: ZPlainAdo.Recordset;
  RA: OleVariant;
begin
  inherited PostUpdates(Sender, UpdateType, OldRowAccessor, NewRowAccessor);

  if (UpdateType = utInserted) and (FAutoColumnIndex > InvalidDbcIndex)
    and OldRowAccessor.IsNull(FAutoColumnIndex) then
  begin
    Recordset := FHandle.Execute(RA, null, 0);
    if Recordset.RecordCount > 0 then
      NewRowAccessor.SetLong(FAutoColumnIndex, Recordset.Fields.Item[0].Value);
  end;
end;

{ TZADOResultSetMetadata }

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZADOResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  inherited ClearColumn(ColumnInfo);
  {ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;}
end;
{$ENDIF ZEOS_DISABLE_ADO}
end.
