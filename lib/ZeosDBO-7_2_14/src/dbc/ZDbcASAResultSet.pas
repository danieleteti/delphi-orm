{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Sybase SQL Anywhere Connectivity Classes        }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcASAResultSet;

interface

{$I ZDbc.inc}
{$IFNDEF ZEOS_DISABLE_ASA}

uses
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}System.Types, System.Contnrs,{$ENDIF}
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcResultSet, ZDbcASA, ZCompatibility,
  ZDbcResultSetMetadata, ZDbcASAUtils, ZMessages, ZPlainASAConstants,
  ZPlainASADriver;

type

  {** Implements ASA ResultSet. }
  TZASAAbstractResultSet = class(TZAbstractResultSet)
  private
    FSQLDA: PASASQLDA;
    FCachedBlob: boolean;
    FFetchStat: Integer;
    FCursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
    FStmtNum: SmallInt;
    FSqlData: IZASASQLDA;
    FASAConnection: IZASAConnection;
  private
    procedure CheckIndex(const Index: Word);
    procedure CheckRange(const Index: Word);
  protected
    procedure Open; override;
    function InternalGetString(ColumnIndex: Integer): RawByteString; override;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
      const SqlData: IZASASQLDA; CachedBlob: boolean);

    procedure BeforeClose; override;
    procedure AfterClose; override;
    procedure ResetCursor; override;

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetShort(ColumnIndex: Integer): ShortInt; override;
    function GetWord(ColumnIndex: Integer): Word; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetUInt(ColumnIndex: Integer): Cardinal; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;

    property SQLData: IZASASQLDA read FSQLData;
  end;

  TZASAParamererResultSet = Class(TZASAAbstractResultSet)
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      var StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}; const SqlData: IZASASQLDA;
      CachedBlob: boolean);
    function Next: Boolean; override;
  end;

  TZASANativeResultSet = Class(TZASAAbstractResultSet)
  public
    function Last: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
    function MoveRelative(Rows: Integer): Boolean; override;
    function Previous: Boolean; override;
    function Next: Boolean; override;
  end;

  TZASACachedResultSet = Class(TZASANativeResultSet)
  private
    FInsert: Boolean;
    FUpdate: Boolean;
    FDelete: Boolean;
    FUpdateSqlData: IZASASQLDA;
    FPLainDriver: IZASAPlainDriver;
    procedure PrepareUpdateSQLData;
  public
    constructor Create(const Statement: IZStatement; const SQL: string;
      var StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}; const SqlData: IZASASQLDA;
      CachedBlob: boolean);

    procedure BeforeClose; override;

    function RowUpdated: Boolean; override;
    function RowInserted: Boolean; override;
    function RowDeleted: Boolean; override;

    procedure UpdateNull(ColumnIndex: Integer); override;
    procedure UpdateBoolean(ColumnIndex: Integer; const Value: Boolean); override;
    procedure UpdateByte(ColumnIndex: Integer; const Value: Byte); override;
    procedure UpdateShort(ColumnIndex: Integer; const Value: ShortInt); override;
    procedure UpdateWord(ColumnIndex: Integer; const Value: Word); override;
    procedure UpdateSmall(ColumnIndex: Integer; const Value: SmallInt); override;
    procedure UpdateUInt(ColumnIndex: Integer; const Value: LongWord); override;
    procedure UpdateInt(ColumnIndex: Integer; const Value: Integer); override;
    procedure UpdateULong(ColumnIndex: Integer; const Value: UInt64); override;
    procedure UpdateLong(ColumnIndex: Integer; const Value: Int64); override;
    procedure UpdateFloat(ColumnIndex: Integer; const Value: Single); override;
    procedure UpdateDouble(ColumnIndex: Integer; const Value: Double); override;
    procedure UpdateBigDecimal(ColumnIndex: Integer; const Value: Extended); override;
    procedure UpdateString(ColumnIndex: Integer; const Value: String); override;
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: ZWideString); override;
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TBytes); override;
    procedure UpdateDate(ColumnIndex: Integer; const Value: TDateTime); override;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TDateTime); override;
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TDateTime); override;
    procedure UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream); override;
    procedure UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream); override;
    procedure UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream); override;

    procedure InsertRow; override;
    procedure UpdateRow; override;
    procedure DeleteRow; override;
    procedure RefreshRow; override;
    procedure CancelRowUpdates; override;
    procedure MoveToInsertRow; override;
    procedure MoveToCurrentRow; override;

    function MoveAbsolute(Row: Integer): Boolean; override;
    function MoveRelative(Rows: Integer): Boolean; override;
  End;

  {** Implements external clob wrapper object for ASA. }
  TZASAClob = class(TZAbstractClob)
  public
    constructor Create(const SqlData: IZASASQLDA; const ColID: Integer;
      Const ConSettings: PZConSettings);
  end;

{$ENDIF ZEOS_DISABLE_ASA}
implementation
{$IFNDEF ZEOS_DISABLE_ASA}

uses
{$IFNDEF FPC}
  Variants,
{$ENDIF}
 Math, ZFastCode, ZDbcLogging, ZEncoding, ZClasses;

{ TZASAResultSet }

{**
  Constructs this object, assignes main properties and
  opens the record set.
  @param Statement a related SQL statement object.
  @param handle a Interbase6 database connect handle.
  @param the statement previously prepared
  @param the sql out data previously allocated
  @param the Interbase sql dialect
}
constructor TZASAAbstractResultSet.Create(const Statement: IZStatement;
  const SQL: string; StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
  const SqlData: IZASASQLDA; CachedBlob: boolean);
begin
  inherited Create(Statement, SQL, nil,Statement.GetConnection.GetConSettings);

  FFetchStat := 0;
  FSqlData := SqlData;
  Self.FSQLDA := FSqlData.GetData;
  FCursorName := CursorName;
  FCachedBlob := CachedBlob;
  FASAConnection := Statement.GetConnection as IZASAConnection;

  FStmtNum := StmtNum;
  ResultSetType := rtScrollSensitive;
  ResultSetConcurrency := rcUpdatable;

  Open;
end;

{**
   Check range count fields. If index out of range raised exception.
   @param Index the index field
}
procedure TZASAAbstractResultSet.CheckIndex(const Index: Word);
begin
  Assert(Assigned(FSQLDA), 'SQLDA not initialized.');
  Assert(Index < Word(FSQLDA.sqld), 'Out of Range.');
end;

procedure TZASAAbstractResultSet.CheckRange(const Index: Word);
begin
  CheckIndex(Index);
  Assert(Assigned(FSQLDA.sqlVar[ Index].sqlData),
    'No memory for variable in SQLDA.');
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAAbstractResultSet.InternalGetString(ColumnIndex: Integer): RawByteString;
begin
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := ''
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := IntToRaw(PShortInt(sqldata)^);
        DT_BIT         : Result := BoolToRawEx(PByte(sqldata)^ = 1);
        DT_SMALLINT    : Result := IntToRaw(PSmallint(sqldata)^);
        DT_UNSSMALLINT : Result := IntToRaw(PWord(sqldata)^);
        DT_INT         : Result := IntToRaw(PInteger(sqldata)^);
        DT_UNSINT      : Result := IntToRaw(PLongWord(sqldata)^);
        DT_BIGINT      : Result := IntToRaw(PInt64(sqldata)^);
        DT_UNSBIGINT   : Result := IntToRaw(PUInt64(sqldata)^);
        DT_FLOAT       : Result := FloatToRaw(PSingle(sqldata)^);
        DT_DOUBLE      : Result := FloatToRaw(PDouble(sqldata)^);
        DT_VARCHAR     :
          ZSetString(@PZASASQLSTRING(sqlData).data[0], PZASASQLSTRING(sqlData).length, Result);
        DT_LONGVARCHAR : FSqlData.ReadBlobToString(ColumnIndex, Result);
        DT_TIMESTAMP_STRUCT : Result := DateTimeToRawSQLTimeStamp(GetTimestamp(ColumnIndex),
          ConSettings^.ReadFormatSettings, False);
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
    end;
end;

{**
  Indicates if the value of the designated column in the current row
  of this <code>ResultSet</code> object is Null.

  @param columnIndex the first column is 1, the second is 2, ...
  @return if the value is SQL <code>NULL</code>, the
    value returned is <code>true</code>. <code>false</code> otherwise.
}
function TZASAAbstractResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}
  CheckRange(ColumnIndex);
  with FSQLDA.sqlvar[ColumnIndex] do
    Result := Assigned(sqlind) and (sqlind^ < 0);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZASAAbstractResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := False
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := False;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^ <> 0;
        DT_BIT         : Result := PByte(sqldata)^ <> 0;
        DT_SMALLINT    : Result := PSmallint(sqldata)^ <> 0;
        DT_UNSSMALLINT : Result := PWord(sqldata)^ <> 0;
        DT_INT         : Result := PInteger(sqldata)^ <> 0;
        DT_UNSINT      : Result := PLongWord(sqldata)^ <> 0;
        DT_BIGINT      : Result := PInt64(sqldata)^ <> 0;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^ <> 0;
        DT_FLOAT       : Result := PSingle(sqldata)^ <> 0;
        DT_DOUBLE      : Result := PDouble(sqldata)^ <> 0;
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := StrToBoolEx(FRawTemp);
           end;
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            Result := StrToBoolEx(FRawTemp);
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAAbstractResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := 0;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt(FRawTemp);
           end;
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            Result := ZFastCode.RawToInt(FRawTemp);
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ShortInt</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAAbstractResultSet.GetShort(ColumnIndex: Integer): ShortInt;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := 0;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt(FRawTemp);
           end;
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            Result := ZFastCode.RawToInt(FRawTemp);
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Word</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAAbstractResultSet.GetWord(ColumnIndex: Integer): Word;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := 0;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt(FRawTemp);
           end;
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            Result := ZFastCode.RawToInt(FRawTemp);
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAAbstractResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := 0;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt(FRawTemp);
           end;
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            Result := ZFastCode.RawToInt(FRawTemp);
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>LongWord</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAAbstractResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := 0;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt64(FRawTemp);
           end;
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            Result := ZFastCode.RawToInt64(FRawTemp);
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
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
function TZASAAbstractResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := 0;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt(FRawTemp);
           end;
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            Result := ZFastCode.RawToInt(FRawTemp);
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
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
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZASAAbstractResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := 0;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToUInt64(FRawTemp);
           end;
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            Result := ZFastCode.RawToUInt64(FRawTemp);
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
    end;
  end;
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
function TZASAAbstractResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := 0
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := 0;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PSingle(sqldata)^);
        DT_DOUBLE      : Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(PDouble(sqldata)^);
        DT_VARCHAR:
           begin
             ZSetString(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), PZASASQLSTRING(sqlData).length, FRawTemp{%H-});
             Result := RawToInt64(FRawTemp);
           end;
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            Result := ZFastCode.RawToInt64(FRawTemp);
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
    end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZASAAbstractResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := PSingle(sqldata)^;
        DT_DOUBLE      : Result := PDouble(sqldata)^;
        DT_VARCHAR     : SQLStrToFloatDef(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), 0, Result, PZASASQLSTRING(sqlData).length);
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            SQLStrToFloatDef(PAnsiChar(Pointer(FRawTemp)), 0, Result, Length(fRawTemp));
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(columnIndex), ConvertASATypeToString(sqlType)]));
      end;
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
function TZASAAbstractResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := PSingle(sqldata)^;
        DT_DOUBLE      : Result := PDouble(sqldata)^;
        DT_VARCHAR     : SQLStrToFloatDef(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), 0, Result, PZASASQLSTRING(sqlData).length);
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            SQLStrToFloatDef(PAnsiChar(Pointer(FRawTemp)), 0, Result, Length(fRawTemp));
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(columnIndex), ConvertASATypeToString(sqlType)]));
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
function TZASAAbstractResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
  CheckClosed;
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
      case sqlType and $FFFE of
        DT_TINYINT     : Result := PShortInt(sqldata)^;
        DT_BIT         : Result := PByte(sqldata)^;
        DT_SMALLINT    : Result := PSmallint(sqldata)^;
        DT_UNSSMALLINT : Result := PWord(sqldata)^;
        DT_INT         : Result := PInteger(sqldata)^;
        DT_UNSINT      : Result := PLongWord(sqldata)^;
        DT_BIGINT      : Result := PInt64(sqldata)^;
        DT_UNSBIGINT   : Result := PUInt64(sqldata)^;
        DT_FLOAT       : Result := PSingle(sqldata)^;
        DT_DOUBLE      : Result := PDouble(sqldata)^;
        DT_VARCHAR     : SQLStrToFloatDef(PAnsiChar(@PZASASQLSTRING(sqlData).data[0]), 0, Result, PZASASQLSTRING(sqlData).length);
        DT_LONGVARCHAR :
          begin
            FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
            SQLStrToFloatDef(PAnsiChar(Pointer(FRawTemp)), 0, Result, Length(fRawTemp));
          end;
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(columnIndex), ConvertASATypeToString(sqlType)]));
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
function TZASAAbstractResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
  CheckColumnConvertion(ColumnIndex, stBytes);
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := nil
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
      case sqlType and $FFFE of
        DT_BINARY:
          Result := BufferToBytes(
            @(PZASASQLSTRING(sqlData).data), PZASASQLSTRING(sqlData).length)
        else
          FSqlData.CreateException(Format(SErrorConvertionField,
            [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
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
function TZASAAbstractResultSet.GetDate(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  P: PAnsiChar;
  Len: NativeUInt;
begin
  CheckColumnConvertion(ColumnIndex, stDate);
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
      case sqlType and $FFFE of
        DT_VARCHAR:
          begin
            P := @PZASASQLSTRING(sqlData).data[0];
            Len := PZASASQLSTRING(sqlData).length;
            if Len = ConSettings^.ReadFormatSettings.DateFormatLen then
              Result := RawSQLDateToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
            else
              Result := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}Trunc(
                RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed));
          end;
           DT_TIMESTAMP_STRUCT:
              begin
                Result := EncodeDate(PZASASQLDateTime(sqlData).Year,
                                      PZASASQLDateTime(sqlData).Month + 1,
                                      PZASASQLDateTime(sqlData).Day);
              end;
        else
          FSqlData.CreateException(Format(SErrorConvertionField,
            [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
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
function TZASAAbstractResultSet.GetTime(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  P: PAnsiChar;
  Len: NativeUInt;
begin
  CheckColumnConvertion(ColumnIndex, stTime);
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
      case sqlType and $FFFE of
        DT_VARCHAR:
          begin
            P := @PZASASQLSTRING(sqlData).data[0];
            Len := PZASASQLSTRING(sqlData).length;
            if AnsiChar((P+2)^) = AnsiChar(':') then //possible date if Len = 10 then
              Result := RawSQLTimeToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed)
            else
              Result := Frac(RawSQLTimeStampToDateTime(P,Len, ConSettings^.ReadFormatSettings, Failed));
          end;
        DT_TIMESTAMP_STRUCT:
          begin
            Result :=  EncodeTime(PZASASQLDateTime(sqlData).Hour,
                                  PZASASQLDateTime(sqlData).Minute,
                                  PZASASQLDateTime(sqlData).Second,
                                  PZASASQLDateTime(sqlData).MicroSecond div 1000);
          end;
        else
          FSqlData.CreateException(Format(SErrorConvertionField,
            [FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
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
function TZASAAbstractResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
var
  Failed: Boolean;
  P: PAnsiChar;
  Len: NativeUInt;
begin
  CheckColumnConvertion(ColumnIndex, stTime);
  LastWasNull := IsNull(ColumnIndex);
  Result := 0;
  if not LastWasNull then
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
      case sqlType and $FFFE of
        DT_VARCHAR:
          begin
            P := @PZASASQLSTRING(sqlData).data[0];
            Len := PZASASQLSTRING(sqlData).length;
            if AnsiChar((P+2)^) = AnsiChar(':') then
              Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
            else
              if (ConSettings^.ReadFormatSettings.DateTimeFormatLen - Len) <= 4 then
                Result := RawSQLTimeStampToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed)
              else
                Result := RawSQLTimeToDateTime(P, Len, ConSettings^.ReadFormatSettings, Failed);
          end;
        DT_TIMESTAMP_STRUCT:
          begin
              Result := EncodeDate(PZASASQLDateTime(sqlData).Year,
                                    PZASASQLDateTime(sqlData).Month + 1,
                                    PZASASQLDateTime(sqlData).Day) +
                                    EncodeTime(PZASASQLDateTime(sqlData).Hour,
                                    PZASASQLDateTime(sqlData).Minute,
                                    PZASASQLDateTime(sqlData).Second,
                                    PZASASQLDateTime(sqlData).MicroSecond div 1000);
          end;
        else
          FSqlData.CreateException(Format(SErrorConvertionField,
            [FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
  end;
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>TZAnsiRec</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len the Length of the PAnsiChar String
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZASAAbstractResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
begin
  CheckColumnConvertion(ColumnIndex, stString);
  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
  begin
    Result := nil;
    Len := 0;
  end
  else
  begin
    {$IFNDEF GENERIC_INDEX}
    ColumnIndex := ColumnIndex -1;
    {$ENDIF}
    with FSQLDA.sqlvar[ColumnIndex] do
    begin
      Result := nil;
      Len := 0;
      if (sqlind^ < 0) then
         Exit;

      case sqlType and $FFFE of
        DT_TINYINT     : FRawTemp := IntToRaw(PShortInt(sqldata)^);
        DT_BIT         : FRawTemp := BoolToRawEx(PByte(sqldata)^ = 1);
        DT_SMALLINT    : FRawTemp := IntToRaw(PSmallint(sqldata)^);
        DT_UNSSMALLINT : FRawTemp := IntToRaw(PWord(sqldata)^);
        DT_INT         : FRawTemp := IntToRaw(PInteger(sqldata)^);
        DT_UNSINT      : FRawTemp := IntToRaw(PLongWord(sqldata)^);
        DT_BIGINT      : FRawTemp := IntToRaw(PInt64(sqldata)^);
        DT_UNSBIGINT   : FRawTemp := IntToRaw(PUInt64(sqldata)^);
        DT_FLOAT       : FRawTemp := FloatToRaw(PSingle(sqldata)^);
        DT_DOUBLE      : FRawTemp := FloatToRaw(PDouble(sqldata)^);
        DT_VARCHAR     :
          begin
            Result := @PZASASQLSTRING(sqlData).data[0];
            Len := PZASASQLSTRING(sqlData).length;
            Exit;
          end;
        DT_LONGVARCHAR : FSqlData.ReadBlobToString(ColumnIndex, FRawTemp);
        DT_TIMESTAMP_STRUCT : FRawTemp := DateTimeToRawSQLTimeStamp(GetTimestamp(ColumnIndex),
          ConSettings^.ReadFormatSettings, False);
      else
        FSqlData.CreateException(Format(SErrorConvertionField,
          [ FSqlData.GetFieldName(ColumnIndex), ConvertASATypeToString(sqlType)]));
      end;
    end;
    Len := Length(FRawTemp);
    if Len = 0 then
      Result := PEmptyAnsiString
    else
      Result := Pointer(FRawTemp);
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
function TZASAAbstractResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
var
  TempBytes: TBytes;
  TempRaw: RawByteString;
  Buffer: Pointer;
  Len: NativeUint;
begin
  CheckBlobColumn(ColumnIndex);

  LastWasNull := IsNull(ColumnIndex);
  if LastWasNull then
    Result := nil
  else
    case GetMetadata.GetColumnType(ColumnIndex) of
      stAsciiStream, stUnicodeStream:
        Result := TZASAClob.Create(FsqlData, ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, ConSettings);
      stBinaryStream:
        begin
          Buffer := nil; //satisfy FPC compiler
          FSqlData.ReadBlobToMem(ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}, Buffer, Len{%H-});
          {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
          Result := TZAbstractBlob.Create;
          Result.SetBlobData(Buffer, Len); //no Move!
          {$ELSE}
          Result := TZAbstractBlob.CreateWithData(Buffer, Len);
          FreeMem(Buffer); //hmpf we need to move the memory
          {$ENDIF}
        end;
      stBytes:
        begin
          TempBytes := GetBytes(ColumnIndex);
          Result := TZAbstractBlob.CreateWithData(Pointer(TempBytes), Length(TempBytes));
        end;
      else
      begin
        TempRaw := InternalGetString(ColumnIndex);
        Result := TZAbstractClob.CreateWithData(PAnsiChar(TempRaw), Length(TempRaw),
          ConSettings^.ClientCodePage^.CP, ConSettings);
      end;
    end;
end;

{**
  Opens this recordset.
}
procedure TZASAAbstractResultSet.Open;
var
  i: Integer;
  FieldSqlType: TZSQLType;
  ColumnInfo: TZColumnInfo;
begin
  if FStmtNum = 0 then
    raise EZSQLException.Create(SCanNotRetrieveResultSetData);

  ColumnsInfo.Clear;
  for i := 0 to FSqlData.GetFieldCount - 1 do begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo, FSqlData  do
    begin
      FieldSqlType := GetFieldSqlType(I);
      ColumnLabel := GetFieldName(I);
      ColumnType := FieldSqlType;

      if FieldSqlType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then begin
        ColumnCodePage := ConSettings^.ClientCodePage^.CP;
        if ColumnType = stString then begin
          CharOctedLength := GetFieldLength(I)-4;
          Precision := CharOctedLength div ConSettings^.ClientCodePage^.CharWidth;
          ColumnDisplaySize := Precision;
        end else if FieldSQLType = stUnicodeString then begin
          Precision := GetFieldLength(I)-4 div ConSettings^.ClientCodePage^.CharWidth;
          CharOctedLength := Precision shl 1;
          ColumnDisplaySize := Precision;
        end;
      end else if FieldSqlType = stBytes then begin
        Precision := GetFieldLength(I)-4;
        CharOctedLength := Precision;
      end;

        ColumnCodePage := High(Word);

      ReadOnly := False;

      if IsNullable(I) then
        Nullable := ntNullable
      else
        Nullable := ntNoNulls;
      Nullable := ntNullable;

      Scale := GetFieldScale(I);
      AutoIncrement := False;
      //Signed := False;
      CaseSensitive := False;
    end;
    ColumnsInfo.Add(ColumnInfo);
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
procedure TZASAAbstractResultSet.AfterClose;
begin
  FCursorName := '';
  inherited AfterClose;
end;

procedure TZASAAbstractResultSet.BeforeClose;
begin
  FSqlData := nil;
  inherited BeforeClose; //Calls ResetCursor so db_close is called!
end;

{**
  Resets cursor position of this recordset to beginning and
  the overrides should reset the prepared handles.
}
procedure TZASAAbstractResultSet.ResetCursor;
begin
  if FCursorName <> '' then
    FASAConnection.GetPlainDriver.db_close(FASAConnection.GetDBHandle, PAnsiChar(FCursorName));
  inherited ResetCursor;
end;

{ TZASAParamererResultSet }

constructor TZASAParamererResultSet.Create(const Statement: IZStatement;
  const SQL: string; var StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
  const SqlData: IZASASQLDA; CachedBlob: boolean);
begin
  inherited Create(Statement, SQL, StmtNum, CursorName, SqlData, CachedBlob);
  SetType(rtForwardOnly);
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
function TZASAParamererResultSet.Next: Boolean;
begin
  Result := (not Closed) and (RowNo = 0);
  if Result then RowNo := 1;
end;

{ TZASANativeResultSet }

{**
  Moves the cursor to the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if there are no rows in the result set
}
function TZASANativeResultSet.Last: Boolean;
begin
  if LastRowNo <> MaxInt then
    Result := MoveAbsolute(LastRowNo)
  else
    Result := MoveAbsolute(-1);
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
function TZASANativeResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := False;
  if Closed or ((MaxRows > 0) and (Row >= MaxRows)) then
    Exit;

  FASAConnection.GetPlainDriver.db_fetch(FASAConnection.GetDBHandle,
    Pointer(FCursorName), CUR_ABSOLUTE, Row, FSqlData.GetData, BlockSize, CUR_FORREGULAR);
  ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
    FASAConnection.GetDBHandle, lcOther, ConSettings);

  if FASAConnection.GetDBHandle.sqlCode <> SQLE_NOTFOUND then begin
    RowNo := Row;
    Result := True;
    FFetchStat := 0;
  end else begin
    FFetchStat := FASAConnection.GetDBHandle.sqlerrd[2];
    if FFetchStat > 0 then
      LastRowNo := Max(Row - FFetchStat, 0);
  end;
end;

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
function TZASANativeResultSet.MoveRelative(Rows: Integer): Boolean;
begin
  Result := False;
  if Closed or ((RowNo > LastRowNo) or ((MaxRows > 0) and (RowNo >= MaxRows))) then
    Exit;
  FASAConnection.GetPlainDriver.db_fetch(FASAConnection.GetDBHandle,
    Pointer(FCursorName), CUR_RELATIVE, Rows, FSqlData.GetData, BlockSize, CUR_FORREGULAR);
    ZDbcASAUtils.CheckASAError( FASAConnection.GetPlainDriver,
      FASAConnection.GetDBHandle, lcOther, ConSettings, '', SQLE_CURSOR_NOT_OPEN); //handle a known null resultset issue (cursor not open)
  if FASAConnection.GetDBHandle.sqlCode = SQLE_CURSOR_NOT_OPEN then Exit;
  if FASAConnection.GetDBHandle.sqlCode <> SQLE_NOTFOUND then
  begin
    //if (RowNo > 0) or (RowNo + Rows < 0) then
    RowNo := RowNo + Rows;
    if Rows > 0 then
      LastRowNo := RowNo;
    Result := True;
    FFetchStat := 0;
  end else begin
    FFetchStat := FASAConnection.GetDBHandle.sqlerrd[2];
    if (FFetchStat > 0) and (RowNo > 0) then
      LastRowNo := Max(RowNo + Rows - FFetchStat, 0);
    if Rows > 0 then
      RowNo := LastRowNo + 1;
  end;
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
function TZASANativeResultSet.Previous: Boolean;
begin
  Result := MoveRelative(-1);
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
function TZASANativeResultSet.Next: Boolean;
begin
  Result := MoveRelative(1);
end;

{ TZASACachedResultSet }
constructor TZASACachedResultSet.Create(const Statement: IZStatement; const SQL: string;
  var StmtNum: SmallInt; const CursorName: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF}; const SqlData: IZASASQLDA;
  CachedBlob: boolean);
begin
  inherited Create(Statement, SQL, StmtNum, CursorName, SqlData, CachedBlob);
  FPLainDriver := Statement.GetConnection.GetIZPlainDriver as IZASAPlainDriver;
  FInsert := False;
  FUpdate := False;
  FDelete := False;
end;

procedure TZASACachedResultSet.PrepareUpdateSQLData;
begin
  FUpdate := not FInsert;
  if not Assigned(FUpdateSQLData) then
  begin
    FUpdateSQLData := TZASASQLDA.Create(FPlainDriver,
      FASAConnection.GetDBHandle, Pointer(FCursorName), ConSettings, FSQLData.GetFieldCount);
    FSQLDA := FUpdateSQLData.GetData;
    FSQLDA^.sqld := FSQLDA^.sqln;
  end
  else
    if FUpdateSQLData.GetFieldCount <> Self.FSqlData.GetFieldCount then
      FUpdateSQLData.AllocateSQLDA(FSQLData.GetFieldCount);
end;

procedure TZASACachedResultSet.BeforeClose;
begin
  FUpdateSQLData := nil;
  inherited BeforeClose;
end;

function TZASACachedResultSet.RowUpdated: Boolean;
begin
  Result := FUpdate;
end;

function TZASACachedResultSet.RowInserted: Boolean;
begin
  Result := FInsert;
end;

function TZASACachedResultSet.RowDeleted: Boolean;
begin
  Result := FDelete;
end;

procedure TZASACachedResultSet.UpdateNull(ColumnIndex: Integer);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateNull(ColumnIndex, True);
end;

procedure TZASACachedResultSet.UpdateBoolean(ColumnIndex: Integer; const Value: Boolean);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateBoolean(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateByte(ColumnIndex: Integer; const Value: Byte);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateByte(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateShort(ColumnIndex: Integer; const Value: ShortInt);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateSmall(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateWord(ColumnIndex: Integer; const Value: Word);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateWord(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateSmall(ColumnIndex: Integer; const Value: SmallInt);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateSmall(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateUInt(ColumnIndex: Integer; const Value: LongWord);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateUInt(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateInt(ColumnIndex: Integer; const Value: Integer);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateInt(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateULong(ColumnIndex: Integer; const Value: UInt64);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateULong(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateLong(ColumnIndex: Integer; const Value: Int64);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateLong(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateFloat(ColumnIndex: Integer; const Value: Single);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateFloat(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateDouble(ColumnIndex: Integer; const Value: Double);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateDouble(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateBigDecimal(ColumnIndex: Integer; const Value: Extended);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateBigDecimal(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateString(ColumnIndex: Integer; const Value: String);
begin
  PrepareUpdateSQLData;
  FRawTemp := ConSettings^.ConvFuncs.ZStringToRaw(Value,
            ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
  FUpdateSqlData.UpdatePRaw(ColumnIndex, Pointer(FRawTemp), Length(FRawTemp));
end;

procedure TZASACachedResultSet.UpdateUnicodeString(ColumnIndex: Integer; const Value: ZWideString);
begin
  PrepareUpdateSQLData;
  FRawTemp := ConSettings^.ConvFuncs.ZUnicodeToRaw(Value, ConSettings^.ClientCodePage^.CP);
  FUpdateSqlData.UpdatePRaw(ColumnIndex, Pointer(FRawTemp), Length(FRawTemp));
end;

procedure TZASACachedResultSet.UpdateBytes(ColumnIndex: Integer; const Value: TBytes);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateBytes(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateDate(ColumnIndex: Integer; const Value: TDateTime);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateDate(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateTime(ColumnIndex: Integer; const Value: TDateTime);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateTime(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateTimestamp(ColumnIndex: Integer; const Value: TDateTime);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.UpdateTimestamp(ColumnIndex, Value);
end;

procedure TZASACachedResultSet.UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.WriteBlob(ColumnIndex, Value, stAsciiStream);
end;

procedure TZASACachedResultSet.UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.WriteBlob(ColumnIndex, Value, stUnicodeStream);
end;

procedure TZASACachedResultSet.UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream);
begin
  PrepareUpdateSQLData;
  FUpdateSqlData.WriteBlob(ColumnIndex, Value, stBinaryStream);
end;

procedure TZASACachedResultSet.InsertRow;
begin
  if Assigned(FUpdateSQLData) and FInsert then
  begin
    FASAConnection.GetPlainDriver.db_put_into(FASAConnection.GetDBHandle,
      PAnsiChar(FCursorName), FUpdateSQLData.GetData, FSQLData.GetData);
    ZDbcASAUtils.CheckASAError(FPlainDriver,
      FASAConnection.GetDBHandle, lcOther, ConSettings, 'Insert row');

    FInsert := false;
    Self.FSQLDA :=  FSqlData.GetData;
  end;
end;

procedure TZASACachedResultSet.UpdateRow;
begin
  if Assigned(FUpdateSQLData) and FUpdate then
  begin
    FASAConnection.GetPlainDriver.db_update(FASAConnection.GetDBHandle,
      PAnsiChar(FCursorName), FUpdateSQLData.GetData);
    ZDbcASAUtils.CheckASAError(FPlainDriver,
      FASAConnection.GetDBHandle, lcOther, ConSettings, 'Update row:' + IntToRaw(RowNo));

    FUpdate := false;
    FUpdateSQLData.FreeSQLDA;
    FSQLDA := FSqlData.GetData;
  end;
end;

procedure TZASACachedResultSet.DeleteRow;
begin
  FASAConnection.GetPlainDriver.db_delete(FASAConnection.GetDBHandle,
    PAnsiChar(FCursorName));
  ZDbcASAUtils.CheckASAError(FPlainDriver,
    FASAConnection.GetDBHandle, lcOther, ConSettings, 'Delete row:' + IntToRaw(RowNo));

  FDelete := True;
  LastRowNo := LastRowNo - FASAConnection.GetDBHandle.sqlerrd[2];
end;

procedure TZASACachedResultSet.RefreshRow;
begin
  MoveRelative(0);
end;

procedure TZASACachedResultSet.CancelRowUpdates;
begin
  FUpdate := false;
  if Assigned(FUpdateSQLData) then
  begin
    FUpdateSQLData.FreeSQLDA;
    FSQLDA := FSqlData.GetData;
  end;
end;

procedure TZASACachedResultSet.MoveToInsertRow;
begin
  FInsert := true;
end;

procedure TZASACachedResultSet.MoveToCurrentRow;
begin
  FInsert := false;
  if Assigned(FUpdateSQLData) then
    FUpdateSQLData.FreeSQLDA;
end;

function TZASACachedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  Result := inherited MoveAbsolute(Row);
  if Result then
  begin
    FDelete := False;
    FInsert := False;
    FUpdate := False;
  end;
end;

function TZASACachedResultSet.MoveRelative(Rows: Integer): Boolean;
begin
  Result := inherited MoveRelative(Rows);
  if Result then
  begin
    FDelete := False;
    FInsert := False;
    FUpdate := False;
  end;
end;

{ TZASAClob }
constructor TZASAClob.Create(const SqlData: IZASASQLDA; const ColID: Integer;
  Const ConSettings: PZConSettings);
var
  Buffer: Pointer;
  Len: NativeUInt;
begin
  inherited CreateWithData(nil, 0, ConSettings^.ClientCodePage^.CP, ConSettings);
  SQLData.ReadBlobToMem(ColId, Buffer{%H-}, Len{%H-}, False);
  (PAnsiChar(Buffer)+Len)^ := #0; //add leading terminator
  FBlobData := Buffer;
  FBlobSize := Len+1;
end;

{$ENDIF ZEOS_DISABLE_ASA}
end.
