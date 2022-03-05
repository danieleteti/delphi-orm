{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Caching Classes and Interfaces               }
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

unit ZDbcCachedResultSet;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF}
  {$IFDEF TLIST_IS_DEPRECATED}ZSysUtils,{$ENDIF}
  ZClasses, ZDbcIntfs, ZDbcResultSet, ZDbcCache, ZCompatibility;

type
  // Forward declarations.
  IZCachedResultSet = interface;

  {** Resolver to post updates. }
  IZCachedResolver = interface (IZInterface)
    ['{546ED716-BB88-468C-8CCE-D7111CF5E1EF}']

    procedure CalculateDefaults(Sender: IZCachedResultSet;
      RowAccessor: TZRowAccessor);
    procedure PostUpdates(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor);
    {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure UpdateAutoIncrementFields(Sender: IZCachedResultSet; UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver);
    {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure RefreshCurrentRow(Sender: IZCachedResultSet;RowAccessor: TZRowAccessor); //FOS+ 07112006
  end;

  {** Represents a cached result set. }
  IZCachedResultSet = interface (IZResultSet)
    ['{BAF24A92-C8CE-4AB4-AEBC-3D4A9BCB0946}']

    function GetResolver: IZCachedResolver;
    procedure SetResolver(Resolver: IZCachedResolver);

    {BEGIN PATCH [1214009] Calc Defaults in TZUpdateSQL and Added Methods to GET and SET the database Native Resolver
      will help to implemented feature to Calculate default values in TZUpdateSQL
      comment: add this properties to get the original Resolver
      this will be useful whn using TZUpdateSQL //added by fduenas
    }
    function GetNativeResolver: IZCachedResolver;
   {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}

    function IsCachedUpdates: Boolean;
    procedure SetCachedUpdates(Value: Boolean);
    function IsPendingUpdates: Boolean;

    procedure PostUpdates;
    procedure CancelUpdates;
    procedure PostUpdatesCached;
    procedure DisposeCachedUpdates;
    procedure RevertRecord;
    procedure MoveToInitialRow;
  end;

  {** Implements cached ResultSet. }
  TZAbstractCachedResultSet = class (TZAbstractResultSet, IZCachedResultSet)
  private
    FCachedUpdates: Boolean;
    FRowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    FInitialRowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    FCurrentRowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
    FSelectedRow: PZRowBuffer;
    FUpdatedRow: PZRowBuffer;
    FInsertedRow: PZRowBuffer;
    FRowAccessor: TZRowAccessor;
    FNewRowAccessor: TZRowAccessor;
    FOldRowAccessor: TZRowAccessor;
    FNextRowIndex: Integer;
    FResolver: IZCachedResolver;
    {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
    FNativeResolver: IZCachedResolver;
    {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  protected
    procedure CheckAvailable;
    procedure CheckUpdatable;
    procedure Open; override;
    function GetNextRowIndex: Integer;

    procedure CalculateRowDefaults(RowAccessor: TZRowAccessor); virtual;
    procedure PostRowUpdates(OldRowAccessor,
      NewRowAccessor: TZRowAccessor); virtual;
    function LocateRow(RowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}; RowIndex: Integer): Integer;
    function AppendRow(Row: PZRowBuffer): PZRowBuffer;
    procedure PrepareRowForUpdates;

    property CachedUpdates: Boolean read FCachedUpdates write FCachedUpdates;
    property RowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF} read FRowsList write FRowsList;
    property InitialRowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF} read FInitialRowsList
      write FInitialRowsList;
    property CurrentRowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF} read FCurrentRowsList
      write FCurrentRowsList;
    property SelectedRow: PZRowBuffer read FSelectedRow write FSelectedRow;
    property UpdatedRow: PZRowBuffer read FUpdatedRow write FUpdatedRow;
    property InsertedRow: PZRowBuffer read FInsertedRow write FInsertedRow;
    property RowAccessor: TZRowAccessor read FRowAccessor write FRowAccessor;
    property OldRowAccessor: TZRowAccessor read FOldRowAccessor
      write FOldRowAccessor;
    property NewRowAccessor: TZRowAccessor read FNewRowAccessor
      write FNewRowAccessor;
    property NextRowIndex: Integer read FNextRowIndex write FNextRowIndex;
    property Resolver: IZCachedResolver read FResolver write FResolver;
    {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
    property NativeResolver: IZCachedResolver read FNativeResolver;
    {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  public
    constructor CreateWithStatement(const SQL: string; const Statement: IZStatement;
      ConSettings: PZConSettings);
    constructor CreateWithColumns(ColumnsInfo: TObjectList; const SQL: string;
      ConSettings: PZConSettings); overload;
    constructor CreateWithColumns(const Statement: IZStatement;
      ColumnsInfo: TObjectList; const SQL: string; ConSettings: PZConSettings); overload;
    destructor Destroy; override;

    procedure AfterClose; override;
    procedure ResetCursor; override;

    //======================================================================
    // Methods for accessing results by column index
    //======================================================================

    function IsNull(ColumnIndex: Integer): Boolean; override;
    function GetPChar(ColumnIndex: Integer): PChar; override;
    function GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar; override;
    function GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar; override;
    function GetString(ColumnIndex: Integer): String; override;
    {$IFNDEF NO_ANSISTRING}
    function GetAnsiString(ColumnIndex: Integer): AnsiString; override;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetUTF8String(ColumnIndex: Integer): UTF8String; override;
    {$ENDIF}
    function GetRawByteString(ColumnIndex: Integer): RawByteString; override;
    function GetUnicodeString(ColumnIndex: Integer): ZWidestring; override;
    function GetBoolean(ColumnIndex: Integer): Boolean; override;
    function GetByte(ColumnIndex: Integer): Byte; override;
    function GetShort(ColumnIndex: Integer): ShortInt; override;
    function GetWord(ColumnIndex: Integer): Word; override;
    function GetSmall(ColumnIndex: Integer): SmallInt; override;
    function GetUInt(ColumnIndex: Integer): Cardinal; override;
    function GetInt(ColumnIndex: Integer): Integer; override;
    function GetULong(ColumnIndex: Integer): UInt64; override;
    function GetLong(ColumnIndex: Integer): Int64; override;
    function GetFloat(ColumnIndex: Integer): Single; override;
    function GetDouble(ColumnIndex: Integer): Double; override;
    function GetCurrency(ColumnIndex: Integer): Currency; override;
    function GetBigDecimal(ColumnIndex: Integer): Extended; override;
    function GetBytes(ColumnIndex: Integer): TBytes; override;
    function GetDate(ColumnIndex: Integer): TDateTime; override;
    function GetTime(ColumnIndex: Integer): TDateTime; override;
    function GetTimestamp(ColumnIndex: Integer): TDateTime; override;
    function GetBlob(ColumnIndex: Integer): IZBlob; override;
    function GetDefaultExpression(ColumnIndex: Integer): string; override;

    //---------------------------------------------------------------------
    // Traversal/Positioning
    //---------------------------------------------------------------------

    function MoveAbsolute(Row: Integer): Boolean; override;

    //---------------------------------------------------------------------
    // Updates
    //---------------------------------------------------------------------

    function RowUpdated: Boolean; override;
    function RowInserted: Boolean; override;
    function RowDeleted: Boolean; override;

    procedure UpdateNull(ColumnIndex: Integer); override;
    procedure UpdateBoolean(ColumnIndex: Integer; const Value: Boolean); override;
    procedure UpdateByte(ColumnIndex: Integer; const Value: Byte); override;
    procedure UpdateShort(ColumnIndex: Integer; const Value: ShortInt); override;
    procedure UpdateWord(ColumnIndex: Integer; const Value: Word); override;
    procedure UpdateSmall(ColumnIndex: Integer; const Value: SmallInt); override;
    procedure UpdateUInt(ColumnIndex: Integer; const Value: Cardinal); override;
    procedure UpdateInt(ColumnIndex: Integer; const Value: Integer); override;
    procedure UpdateULong(ColumnIndex: Integer; const Value: UInt64); override;
    procedure UpdateLong(ColumnIndex: Integer; const Value: Int64); override;
    procedure UpdateFloat(ColumnIndex: Integer; const Value: Single); override;
    procedure UpdateDouble(ColumnIndex: Integer; const Value: Double); override;
    procedure UpdateCurrency(ColumnIndex: Integer; const Value: Currency); override;
    procedure UpdateBigDecimal(ColumnIndex: Integer; const Value: Extended); override;
    procedure UpdatePChar(ColumnIndex: Integer; const Value: PChar); override;
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar); override;
    procedure UpdatePAnsiChar(ColumnIndex: Integer; Value: PAnsiChar; Len: PNativeUint); override;
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar); override;
    procedure UpdatePWideChar(ColumnIndex: Integer; Value: PWideChar; Len: PNativeUint); override;
    procedure UpdateString(ColumnIndex: Integer; const Value: String); override;
    {$IFNDEF NO_ANSISTRING}
    procedure UpdateAnsiString(ColumnIndex: Integer; const Value: AnsiString); override;
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure UpdateUTF8String(ColumnIndex: Integer; const Value: UTF8String); override;
    {$ENDIF}
    procedure UpdateRawByteString(ColumnIndex: Integer; const Value: RawByteString); override;
    procedure UpdateUnicodeString(ColumnIndex: Integer; const Value: ZWideString); override;
    procedure UpdateBytes(ColumnIndex: Integer; const Value: TBytes); override;
    procedure UpdateDate(ColumnIndex: Integer; const Value: TDateTime); override;
    procedure UpdateTime(ColumnIndex: Integer; const Value: TDateTime); override;
    procedure UpdateTimestamp(ColumnIndex: Integer; const Value: TDateTime); override;
    procedure UpdateAsciiStream(ColumnIndex: Integer; const Value: TStream); override;
    procedure UpdateUnicodeStream(ColumnIndex: Integer; const Value: TStream); override;
    procedure UpdateBinaryStream(ColumnIndex: Integer; const Value: TStream); override;
    procedure UpdateLob(ColumnIndex: Integer; const Value: IZBlob); override;
    procedure UpdateDefaultExpression(ColumnIndex: Integer; const Value: string); override;

    procedure InsertRow; override;
    procedure UpdateRow; override;
    procedure DeleteRow; override;
    procedure CancelRowUpdates; override;
    procedure RefreshRow;override;// FOS+ 071106


    procedure MoveToInsertRow; override;
    procedure MoveToCurrentRow; override;

    function CompareRows(Row1, Row2: NativeInt; const ColumnIndices: TIntegerDynArray;
      const CompareFuncs: TCompareFuncs): Integer; override;
    function GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
      const CompareKinds: TComparisonKindArray): TCompareFuncs; override;

    //---------------------------------------------------------------------
    // Cached Updates
    //---------------------------------------------------------------------

    function GetResolver: IZCachedResolver;
    procedure SetResolver(Resolver: IZCachedResolver);
    {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
    function GetNativeResolver: IZCachedResolver;
    {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
    function IsCachedUpdates: Boolean;
    procedure SetCachedUpdates(Value: Boolean);
    function IsPendingUpdates: Boolean; virtual;

    procedure PostUpdates; virtual;
    procedure CancelUpdates; virtual;
    procedure RevertRecord; virtual;
    procedure MoveToInitialRow; virtual;
    procedure PostUpdatesCached; virtual;
    procedure DisposeCachedUpdates; virtual;
  end;

  TZStringFieldAssignFromResultSet = procedure(ColumnIndex: Integer) of object;

  {**
    Implements Abstract cached ResultSet. This class should be extended
    with database specific logic to form SQL data manipulation statements.
  }
  TZCachedResultSet = class(TZAbstractCachedResultSet)
  private
    FResultSet: IZResultSet;
    FStringFieldAssignFromResultSet: TZStringFieldAssignFromResultSet;
    procedure ZStringFieldAssignFromResultSet_AnsiRec(ColumnIndex: Integer);
    procedure ZStringFieldAssignFromResultSet_Unicode(ColumnIndex: Integer);
  protected
    procedure Open; override;
    function Fetch: Boolean; virtual;
    procedure FetchAll; virtual;

    property ResultSet: IZResultSet read FResultSet write FResultSet;
  public
    constructor Create(const ResultSet: IZResultSet; const SQL: string;
      const Resolver: IZCachedResolver; ConSettings: PZConSettings);
    constructor CreateWithColumns(const ColumnsInfo: TObjectList;
      const ResultSet: IZResultSet; const SQL: string;
      const Resolver: IZCachedResolver; ConSettings: PZConSettings);

    procedure AfterClose; override;
    procedure ResetCursor; override;
    function GetMetaData: IZResultSetMetaData; override;

    function IsAfterLast: Boolean; override;
    function IsLast: Boolean; override;
    procedure AfterLast; override;
    function Last: Boolean; override;
    function MoveAbsolute(Row: Integer): Boolean; override;
  end;

implementation

uses ZMessages, ZDbcResultSetMetadata, ZDbcGenericResolver, ZDbcUtils, ZEncoding;


{ TZAbstractCachedResultSet }

constructor TZAbstractCachedResultSet.CreateWithColumns(
  const Statement: IZStatement; ColumnsInfo: TObjectList; const SQL: string;
  ConSettings: PZConSettings);
begin
  inherited Create(Statement, SQL, nil, ConSettings);

  CopyColumnsInfo(ColumnsInfo, Self.ColumnsInfo);
  FCachedUpdates := False;
  Open;
end;

{**
  Creates this object and assignes the main properties.
  @param Statement an SQL statement object.
  @param SQL an SQL query.
}
constructor TZAbstractCachedResultSet.CreateWithStatement(const SQL: string;
  const Statement: IZStatement; ConSettings: PZConSettings);
begin
  inherited Create(Statement, SQL, nil, ConSettings);
  FCachedUpdates := False;
end;

{**
  Creates this object and assignes the main properties.
  @param SQL an SQL query.
  @param ColumnsInfo a columns info for cached rows.
}
constructor TZAbstractCachedResultSet.CreateWithColumns(
  ColumnsInfo: TObjectList; const SQL: string; ConSettings: PZConSettings);
begin
  inherited Create(nil, SQL, nil, ConSettings);

  CopyColumnsInfo(ColumnsInfo, Self.ColumnsInfo);
  FCachedUpdates := False;
  Open;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractCachedResultSet.Destroy;
begin
  inherited Destroy;
  FResolver := nil;
  {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  FNativeResolver := nil;
  {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
end;

{**
  Checks for availability of the cached buffer.
}
procedure TZAbstractCachedResultSet.CheckAvailable;
begin
  CheckClosed;
  if (FRowAccessor = nil) or (FRowAccessor.RowBuffer = nil) then
    raise EZSQLException.Create(SRowDataIsNotAvailable);
end;

{**
  Checks is the cached buffer updatable.
}
procedure TZAbstractCachedResultSet.CheckUpdatable;
begin
  CheckAvailable;
  if ResultSetConcurrency <> rcUpdatable then
    RaiseReadOnlyException;
end;

{**
  Generates the next row index value.
  @return the new generated row index.
}
function TZAbstractCachedResultSet.GetNextRowIndex: Integer;
begin
  Result := FNextRowIndex;
  Inc(FNextRowIndex);
end;

{**
  Finds a row with specified index among list of rows.
  @param RowsList a list of rows.
  @param Index a row index.
  @return a found row buffer of <code>null</code> otherwise.
}
function TZAbstractCachedResultSet.LocateRow(RowsList: {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF};
  RowIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to RowsList.Count - 1 do
  begin
    if PZRowBuffer(RowsList[I]).Index = RowIndex then
    begin
      Result := I;
      Break;
    end;
  end;
end;

{**
  Appends a row to the list of rows if such row is not exist.
  @param Row a row buffer.
  @return an appended row buffer.
}
function TZAbstractCachedResultSet.AppendRow(Row: PZRowBuffer): PZRowBuffer;
begin
  if LocateRow(FInitialRowsList, Row.Index) < 0 then
  begin
    Result := FRowAccessor.AllocBuffer;
    FRowAccessor.CopyBuffer(Row, Result);
    FInitialRowsList.Add(Result);
    FCurrentRowsList.Add(Row);
  end
  else
    Result := nil;
end;

{**
  Prepares the current selected row for updates.
}
procedure TZAbstractCachedResultSet.PrepareRowForUpdates;
begin
  if (RowAccessor.RowBuffer = FSelectedRow) and (FSelectedRow <> FUpdatedRow) then
  begin
    FSelectedRow := FUpdatedRow;
    RowAccessor.RowBuffer := FSelectedRow;
    RowAccessor.CloneFrom(PZRowBuffer(FRowsList[RowNo - 1]));
  end;
end;

{**
  Calculates column default values..
  @param RowAccessor a row accessor which contains new column values.
}
procedure TZAbstractCachedResultSet.CalculateRowDefaults(
  RowAccessor: TZRowAccessor);
begin
{$IFNDEF DISABLE_CHECKING}
  if Resolver = nil then
    raise EZSQLException.Create(SResolverIsNotSpecified);
{$ENDIF}
  Resolver.CalculateDefaults(Self, RowAccessor);
end;

{**
  Post changes to database server.
  @param OldRowAccessor a row accessor which contains old column values.
  @param NewRowAccessor a row accessor which contains new or updated
    column values.
}
procedure TZAbstractCachedResultSet.PostRowUpdates(OldRowAccessor,
  NewRowAccessor: TZRowAccessor);
begin
{$IFNDEF DISABLE_CHECKING}
  if Resolver = nil then
    raise EZSQLException.Create(SResolverIsNotSpecified);
{$ENDIF}
  Resolver.PostUpdates(Self, NewRowAccessor.RowBuffer.UpdateType,
    OldRowAccessor, NewRowAccessor);
end;

{**
  Gets a cached updates resolver object.
  @return a cached updates resolver object.
}
function TZAbstractCachedResultSet.GetResolver: IZCachedResolver;
begin
  Result := FResolver;
end;

{**
  Sets a new cached updates resolver object.
  @param Resolver a cached updates resolver object.
}
procedure TZAbstractCachedResultSet.SetResolver(Resolver: IZCachedResolver);
begin
  FResolver := Resolver;
{BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  if FNativeResolver = nil then
     FNativeResolver := Resolver;
{END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
end;
{BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
{**
  Gets a Native cached updates resolver object.
  @return a Native cached updates resolver object.
}
function TZAbstractCachedResultSet.GetNativeResolver: IZCachedResolver;
begin
  Result := FNativeResolver;
end;
{END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}

{**
  Checks is the cached updates mode turned on.
  @return <code>True</code> if the cached updates mode turned on.
}
function TZAbstractCachedResultSet.IsCachedUpdates: Boolean;
begin
  Result := FCachedUpdates;
end;

{**
  Switched the cached updates mode.
  @param Value boolean flag which turns on/off the cached updates mode.
}
procedure TZAbstractCachedResultSet.SetCachedUpdates(Value: Boolean);
begin
  if FCachedUpdates <> Value then
  begin
    FCachedUpdates := Value;
    if not FCachedUpdates then
      PostUpdates;
  end;
end;

{**
  Checks is cached updates pending.
  @return <code>True</code> if the cached updates pending.
}
function TZAbstractCachedResultSet.IsPendingUpdates: Boolean;
begin
  //patch by Soner
  Result := Assigned(FInitialRowsList) and (FInitialRowsList.Count > 0);
  //Original:  Result := FInitialRowsList.Count > 0;
  //this can cause error if you Call TZQuery.UpdatesPending at DoBeforeClose;
  //because FInitialRowsList can be nil if nothing inserted/deleted/modified
end;

{**
  Moves to the current row with initial column values.
}
procedure TZAbstractCachedResultSet.MoveToInitialRow;
var
  Index: Integer;
begin
  CheckClosed;
  if (RowNo >= 1) and (RowNo <= LastRowNo) and (FSelectedRow <> nil) then
  begin
    Index := LocateRow(FInitialRowsList, FSelectedRow.Index);
    if Index >= 0 then
    begin
      FSelectedRow := FInitialRowsList[Index];
      FRowAccessor.RowBuffer := FSelectedRow;
    end;
  end
  else
    FRowAccessor.RowBuffer := nil;
end;

{**
  Posts all saved updates to the server.
}
procedure TZAbstractCachedResultSet.PostUpdates;
begin
  CheckClosed;
  if FInitialRowsList.Count > 0 then
  begin
    while FInitialRowsList.Count > 0 do
    begin
      OldRowAccessor.RowBuffer := PZRowBuffer(FInitialRowsList[0]);
      NewRowAccessor.RowBuffer := PZRowBuffer(FCurrentRowsList[0]);

      { Updates default field values. }
      if NewRowAccessor.RowBuffer.UpdateType in [utInserted, utModified] then
        CalculateRowDefaults(NewRowAccessor);

      { Posts row updates and processes the exceptions. }
      PostRowUpdates(OldRowAccessor, NewRowAccessor);

      { If post was Ok - update the row update type. }
      if NewRowAccessor.RowBuffer.UpdateType <> utDeleted then
      begin
        NewRowAccessor.RowBuffer.UpdateType := utUnmodified;
        if (FSelectedRow <> nil)
          and (FSelectedRow.Index = NewRowAccessor.RowBuffer.Index) then
          FSelectedRow.UpdateType := utUnmodified;
      end;

      { Removes cached rows. }
      OldRowAccessor.Dispose;
      FInitialRowsList.Delete(0);
      FCurrentRowsList.Delete(0);
    end;
  end;
end;

{**
  Posts all saved updates to the server but keeps them cached.
}
procedure TZAbstractCachedResultSet.PostUpdatesCached;
var
  i: Integer;
begin
  CheckClosed;
  if FInitialRowsList.Count > 0 then
  begin
    i := 0;
    while i < FInitialRowsList.Count do
    begin
      OldRowAccessor.RowBuffer := PZRowBuffer(FInitialRowsList[i]);
      NewRowAccessor.RowBuffer := PZRowBuffer(FCurrentRowsList[i]);
      Inc(i);

      { Updates default field values. }
      if NewRowAccessor.RowBuffer.UpdateType in [utInserted, utModified] then
        CalculateRowDefaults(NewRowAccessor);

      { Posts row updates. }
      PostRowUpdates(OldRowAccessor, NewRowAccessor);
    end;
  end;
end;

{**
  Frees the updates and marks records as unmodified. Complements
  PostUpdatesCached.
}
procedure TZAbstractCachedResultSet.DisposeCachedUpdates;
begin
  while FInitialRowsList.Count > 0 do
  begin
    OldRowAccessor.RowBuffer := PZRowBuffer(FInitialRowsList[0]);
    NewRowAccessor.RowBuffer := PZRowBuffer(FCurrentRowsList[0]);

    if NewRowAccessor.RowBuffer.UpdateType <> utDeleted then
    begin
      NewRowAccessor.RowBuffer.UpdateType := utUnmodified;
      if (FSelectedRow <> nil)
        and (FSelectedRow.Index = NewRowAccessor.RowBuffer.Index) then
          FSelectedRow.UpdateType := utUnmodified;
    end;

    { Remove cached rows. }
    OldRowAccessor.Dispose;
    FInitialRowsList.Delete(0);
    FCurrentRowsList.Delete(0);
  end;
end;

{**
  Cancels updates for all rows.
}
procedure TZAbstractCachedResultSet.CancelUpdates;
var
  InitialRow, CurrentRow: PZRowBuffer;
begin
  CheckClosed;
  while FInitialRowsList.Count > 0 do
  begin
    InitialRow := PZRowBuffer(FInitialRowsList[0]);
    CurrentRow := PZRowBuffer(FCurrentRowsList[0]);

    if CurrentRow.UpdateType = utInserted then
      InitialRow.UpdateType := utDeleted;

    FRowAccessor.CopyBuffer(InitialRow, CurrentRow);
    if (FSelectedRow = FUpdatedRow)
      and (FSelectedRow.Index = InitialRow.Index) then
      FRowAccessor.CopyBuffer(InitialRow, FSelectedRow);

    FRowAccessor.DisposeBuffer(InitialRow);
    FInitialRowsList.Delete(0);
    FCurrentRowsList.Delete(0);
  end;
end;

{**
  Cancels updates for the current row.
}
procedure TZAbstractCachedResultSet.RefreshRow;
begin
  if Resolver = nil then
    raise EZSQLException.Create(SResolverIsNotSpecified);
  Resolver.RefreshCurrentRow(Self,RowAccessor);
end;

procedure TZAbstractCachedResultSet.RevertRecord;
var
  Index: Integer;
  InitialRow, CurrentRow: PZRowBuffer;
begin
  CheckClosed;
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    Index := LocateRow(FInitialRowsList, FSelectedRow.Index);
    if Index >= 0 then
    begin
      InitialRow := PZRowBuffer(FInitialRowsList[Index]);
      CurrentRow := PZRowBuffer(FRowsList[RowNo - 1]);

      if CurrentRow.UpdateType = utInserted then
        InitialRow.UpdateType := utDeleted;
      FRowAccessor.CopyBuffer(InitialRow, CurrentRow);
      if (FSelectedRow = FUpdatedRow) then
        FRowAccessor.CopyBuffer(InitialRow, FSelectedRow);

      FRowAccessor.DisposeBuffer(InitialRow);
      FInitialRowsList.Delete(Index);
      FCurrentRowsList.Delete(Index);
    end;
  end;
end;

{**
  Opens this recordset.
}
procedure TZAbstractCachedResultSet.Open;
begin
  if not Closed then
    raise EZSQLException.Create(SResultsetIsAlreadyOpened);

  FRowsList := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  FInitialRowsList := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;
  FCurrentRowsList := {$IFDEF TLIST_IS_DEPRECATED}TZSortedList{$ELSE}TList{$ENDIF}.Create;

  FRowAccessor := TZRowAccessor.Create(ColumnsInfo, ConSettings);
  FOldRowAccessor := TZRowAccessor.Create(ColumnsInfo, ConSettings);
  FNewRowAccessor := TZRowAccessor.Create(ColumnsInfo, ConSettings);

  FUpdatedRow :=FRowAccessor.AllocBuffer;
  FInsertedRow := FRowAccessor.AllocBuffer;
  FSelectedRow := nil;

  FNextRowIndex := 0;

  if (Resolver = nil) and (GetConcurrency = rcUpdatable) then
    Resolver := TZGenericCachedResolver.Create(GetStatement, GetMetadata);

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
procedure TZAbstractCachedResultSet.AfterClose;
var
  I: Integer;
begin
  inherited AfterClose;
  if Assigned(FRowAccessor) then begin
    for I := 0 to FRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FRowsList[I]));
    for I := 0 to FInitialRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FInitialRowsList[I]));

    FRowAccessor.DisposeBuffer(FUpdatedRow);
    FUpdatedRow := nil;
    FRowAccessor.DisposeBuffer(FInsertedRow);
    FInsertedRow := nil;
    FSelectedRow := nil;

    FreeAndNil(FRowsList);
    FreeAndNil(FInitialRowsList);
    FreeAndNil(FCurrentRowsList);

    FreeAndNil(FRowAccessor);
    FreeAndNil(FOldRowAccessor);
    FreeAndNil(FNewRowAccessor);
  end;
end;

procedure TZAbstractCachedResultSet.ResetCursor;
var
  I: Integer;
begin
  if Assigned(FRowAccessor) then
  begin
    for I := 0 to FRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FRowsList[I]));
    for I := 0 to FInitialRowsList.Count - 1 do
      FRowAccessor.DisposeBuffer(PZRowBuffer(FInitialRowsList[I]));
    FRowsList.Clear;
    FInitialRowsList.Clear;
    FCurrentRowsList.Clear;
  end;
  inherited ResetCursor;
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
function TZAbstractCachedResultSet.IsNull(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.IsNull(ColumnIndex);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>PAnsiChar</code> in the Delphi programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetPChar(ColumnIndex: Integer): PChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  {$IFDEF UNICODE}
  FUniTemp := FRowAccessor.GetString(ColumnIndex, LastWasNull);
  if Pointer(FUniTemp) = nil then
    Result := PEmptyUnicodeString
  else
    Result := Pointer(FUniTemp); // no RTL conversion!
  {$ELSE}
  FRawTemp := FRowAccessor.GetString(ColumnIndex, LastWasNull);
  if Pointer(FRawTemp) = nil then
    Result := PEmptyAnsiString
  else
    Result := Pointer(FRawTemp); // no RTL conversion!
  {$ENDIF}
end;

function TZAbstractCachedResultSet.GetPAnsiChar(ColumnIndex: Integer; out Len: NativeUInt): PAnsiChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetPAnsiChar(ColumnIndex, LastWasNull, Len);
end;

function TZAbstractCachedResultSet.GetPWideChar(ColumnIndex: Integer; out Len: NativeUInt): PWideChar;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetPWideChar(ColumnIndex, LastWasNull, Len);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetString(ColumnIndex: Integer): String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetString(ColumnIndex, LastWasNull);
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
function TZAbstractCachedResultSet.GetAnsiString(ColumnIndex: Integer): AnsiString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetAnsiString(ColumnIndex, LastWasNull);
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
function TZAbstractCachedResultSet.GetUTF8String(ColumnIndex: Integer): UTF8String;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetUTF8String(ColumnIndex, LastWasNull);
end;
{$ENDIF}

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>RawByteString</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetRawByteString(ColumnIndex: Integer): RawByteString;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetRawByteString(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>Widestring</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetUnicodeString(ColumnIndex: Integer): ZWidestring;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetUnicodeString(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>boolean</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>false</code>
}
function TZAbstractCachedResultSet.GetBoolean(ColumnIndex: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBoolean(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>byte</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetByte(ColumnIndex: Integer): Byte;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetByte(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>short</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetShort(ColumnIndex: Integer): ShortInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetShort(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>word</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetWord(ColumnIndex: Integer): Word;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetWord(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>small</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetSmall(ColumnIndex: Integer): SmallInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetSmall(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>uint</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetUInt(ColumnIndex: Integer): Cardinal;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetUInt(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  an <code>int</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetInt(ColumnIndex: Integer): Integer;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetInt(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>ulong</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetULong(ColumnIndex: Integer): UInt64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetULong(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>long</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetLong(ColumnIndex: Integer): Int64;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetLong(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>float</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetFloat(ColumnIndex: Integer): Single;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetFloat(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>double</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetDouble(ColumnIndex: Integer): Double;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetDouble(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>currency</code> in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>0</code>
}
function TZAbstractCachedResultSet.GetCurrency(ColumnIndex: Integer): Currency;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetCurrency(ColumnIndex, LastWasNull);
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
function TZAbstractCachedResultSet.GetBigDecimal(ColumnIndex: Integer): Extended;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBigDecimal(ColumnIndex, LastWasNull);
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
function TZAbstractCachedResultSet.GetBytes(ColumnIndex: Integer): TBytes;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBytes(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Date</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetDate(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetDate(ColumnIndex, LastWasNull);
end;

{**
  Gets the value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>java.sql.Time</code> object in the Java programming language.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the column value; if the value is SQL <code>NULL</code>, the
    value returned is <code>null</code>
}
function TZAbstractCachedResultSet.GetTime(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetTime(ColumnIndex, LastWasNull);
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
function TZAbstractCachedResultSet.GetTimestamp(ColumnIndex: Integer): TDateTime;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetTimestamp(ColumnIndex, LastWasNull);
end;

{**
  Returns the value of the designated column in the current row
  of this <code>ResultSet</code> object as a <code>Blob</code> object
  in the Java programming language.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a <code>Blob</code> object representing the SQL <code>BLOB</code> value in
    the specified column
}
function TZAbstractCachedResultSet.GetBlob(ColumnIndex: Integer): IZBlob;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetBlob(ColumnIndex, LastWasNull);
end;

{**
  Gets the DefaultExpression value of the designated column in the current row
  of this <code>ResultSet</code> object as
  a <code>String</code>.

  @param columnIndex the first column is 1, the second is 2, ...
  @return the DefaultExpression value
}
function TZAbstractCachedResultSet.GetDefaultExpression(ColumnIndex: Integer): string;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckAvailable;
{$ENDIF}
  Result := FRowAccessor.GetColumnDefaultExpression(ColumnIndex);
end;

//---------------------------------------------------------------------
// Updates
//---------------------------------------------------------------------

{**
  Gives a nullable column a null value.

  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code>
  or <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
}
procedure TZAbstractCachedResultSet.UpdateNull(ColumnIndex: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetNull(ColumnIndex);
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
procedure TZAbstractCachedResultSet.UpdateBoolean(ColumnIndex: Integer;
  const Value: Boolean);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetBoolean(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateByte(ColumnIndex: Integer;
  const Value: Byte);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetByte(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>smallint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateShort(ColumnIndex: Integer;
  const Value: ShortInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetShort(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateWord(ColumnIndex: Integer;
  const Value: Word);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetWord(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>smallint</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateSmall(ColumnIndex: Integer;
  const Value: SmallInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetSmall(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateUInt(ColumnIndex: Integer;
  const Value: Cardinal);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetUInt(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateInt(ColumnIndex: Integer;
  const Value: Integer);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetInt(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateULong(ColumnIndex: Integer;
  const Value: UInt64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetULong(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateLong(ColumnIndex: Integer;
  const Value: Int64);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetLong(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateFloat(ColumnIndex: Integer;
  const Value: Single);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetFloat(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateDouble(ColumnIndex: Integer;
  const Value: Double);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetDouble(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateCurrency(ColumnIndex: Integer;
  const Value: Currency);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetCurrency(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateBigDecimal(ColumnIndex: Integer;
  const Value: Extended);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetBigDecimal(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdatePChar(ColumnIndex: Integer;
  const Value: PChar);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetString(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdatePAnsiChar(ColumnIndex: Integer;
  Value: PAnsiChar);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetPAnsiChar(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdatePAnsiChar(ColumnIndex: Integer;
  Value: PAnsiChar; Len: PNativeUInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetPAnsiChar(ColumnIndex, Value, Len);
end;

{**
  Updates the designated column with a <code>PWideChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdatePWideChar(ColumnIndex: Integer;
  Value: PWideChar);
var Len: NativeUInt;
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  if Value = nil then
    Len := 0
  else
    Len := {$IFDEF WITH_PWIDECHAR_STRLEN}SysUtils.StrLen{$ELSE}Length{$ENDIF}(Value);
  FRowAccessor.SetPWideChar(ColumnIndex, Value, @Len);
end;

{**
  Updates the designated column with a <code>PWideChar</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param Len a pointer to the Length of the value in codepoints
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdatePWideChar(ColumnIndex: Integer;
  Value: PWideChar; Len: PNativeUInt);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetPWideChar(ColumnIndex, Value, Len);
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
procedure TZAbstractCachedResultSet.UpdateString(ColumnIndex: Integer;
  const Value: String);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetString(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateAnsiString(ColumnIndex: Integer;
  const Value: AnsiString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetAnsiString(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateUTF8String(ColumnIndex: Integer;
  const Value: UTF8String);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetUTF8String(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateRawByteString(ColumnIndex: Integer;
  const Value: RawByteString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetRawByteString(ColumnIndex, Value);
end;

{**
  Updates the designated column with a <code>Widestring</code> value.
  The <code>updateXXX</code> methods are used to update column values in the
  current row or the insert row.  The <code>updateXXX</code> methods do not
  update the underlying database; instead the <code>updateRow</code> or
  <code>insertRow</code> methods are called to update the database.

  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new column value
}
procedure TZAbstractCachedResultSet.UpdateUnicodeString(ColumnIndex: Integer;
  const Value: ZWideString);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetUnicodeString(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateBytes(ColumnIndex: Integer;
  const Value: TBytes);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetBytes(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateDate(ColumnIndex: Integer;
  const Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetDate(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateTime(ColumnIndex: Integer;
  const Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetTime(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateTimestamp(ColumnIndex: Integer;
  const Value: TDateTime);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetTimestamp(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateAsciiStream(ColumnIndex: Integer;
  const Value: TStream);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetAsciiStream(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateBinaryStream(
  ColumnIndex: Integer; const Value: TStream);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetBinaryStream(ColumnIndex, Value);
end;

procedure TZAbstractCachedResultSet.UpdateLob(ColumnIndex: Integer;
  const Value: IZBlob);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetBlob(ColumnIndex, Value);
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
procedure TZAbstractCachedResultSet.UpdateUnicodeStream(
  ColumnIndex: Integer; const Value: TStream);
begin
{$IFNDEF DISABLE_CHECKING}
  CheckUpdatable;
{$ENDIF}
  PrepareRowForUpdates;
  FRowAccessor.SetUnicodeStream(ColumnIndex, Value);
end;

{**
  Updates the DefaultExpression of the designated column with a <code>String</code> value.
  This changes the behaviour of the RowAccessor used by the Resultset
  @param columnIndex the first column is 1, the second is 2, ...
  @param x the new DefaultExpression value for the column
}
procedure TZAbstractCachedResultSet.UpdateDefaultExpression(ColumnIndex: Integer;
  const Value: string);
begin
  FNewRowAccessor.SetColumnDefaultExpression(ColumnIndex, Value);
end;

//---------------------------------------------------------------------
// Processing methods
//---------------------------------------------------------------------

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
function TZAbstractCachedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
{$IFNDEF DISABLE_CHECKING}
  // 2018-09-16 commented out because it seems to be current policy for other
  // result sets to not raise an exception if the result set is closed but simply
  // return false here. What is our specification?
  //CheckClosed;
  if (ResultSetType = rtForwardOnly) and (Row < RowNo) then
    RaiseForwardOnlyException;
{$ENDIF}

  if not Closed and (Row >= 0) and (Row <= LastRowNo + 1) then
  begin
    RowNo := Row;
    if (Row >= 1) and (Row <= LastRowNo) then
    begin
      Result := True;
      FSelectedRow := PZRowBuffer(FRowsList[Row - 1]);
      RowAccessor.RowBuffer := FSelectedRow;
    end
    else
    begin
      Result := False;
      FSelectedRow := nil;
      RowAccessor.RowBuffer := FSelectedRow;
    end;
  end
  else
    Result := False;
end;

{**
  Indicates whether the current row has been updated.  The value returned
  depends on whether or not the result set can detect updates.

  @return <code>true</code> if the row has been visibly updated
    by the owner or another, and updates are detected
}
function TZAbstractCachedResultSet.RowUpdated: Boolean;
var
  CurrentRow: PZRowBuffer;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    CurrentRow := PZRowBuffer(FRowsList[RowNo - 1]);
    Result := CurrentRow^.UpdateType = utModified;
  end
  else
    Result := False;
end;

{**
  Indicates whether the current row has had an insertion.
  The value returned depends on whether or not this
  <code>ResultSet</code> object can detect visible inserts.

  @return <code>true</code> if a row has had an insertion
    and insertions are detected; <code>false</code> otherwise
}
function TZAbstractCachedResultSet.RowInserted: Boolean;
var
  CurrentRow: PZRowBuffer;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    CurrentRow := PZRowBuffer(FRowsList[RowNo - 1]);
    Result := CurrentRow^.UpdateType = utInserted;
  end
  else
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
function TZAbstractCachedResultSet.RowDeleted: Boolean;
var
  UpdateType: TZRowUpdateType;
begin
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
  begin
    UpdateType := PZRowBuffer(FRowsList[RowNo - 1])^.UpdateType;
    Result := UpdateType = utDeleted;
  end
  else
    Result := False;
end;

{**
  Inserts the contents of the insert row into this
  <code>ResultSet</code> object and into the database.
  The cursor must be on the insert row when this method is called.
}
procedure TZAbstractCachedResultSet.InsertRow;
var
  TempRow: PZRowBuffer;
begin
  CheckClosed;

  { Creates a new row. }
  TempRow := FRowAccessor.RowBuffer;
  FRowAccessor.Alloc;
  FRowAccessor.MoveFrom(FInsertedRow);
  FRowAccessor.RowBuffer^.UpdateType := utInserted;
  FRowAccessor.RowBuffer^.Index := GetNextRowIndex;

  AppendRow(FRowAccessor.RowBuffer);

  { Posts non-cached updates. }
  if not FCachedUpdates then
  begin
    try
      PostUpdates;
    except
      on E: Exception do
      begin
        { Restore the previous state. }
        FRowAccessor.DisposeBuffer(FInitialRowsList[FInitialRowsList.Count - 1]);
        FInitialRowsList.Delete(FInitialRowsList.Count - 1);
        FRowAccessor.DisposeBuffer(FCurrentRowsList[FCurrentRowsList.Count - 1]);
        FCurrentRowsList.Delete(FCurrentRowsList.Count - 1);
        FRowAccessor.RowBuffer := TempRow;

        { Reraises the exception. }
        RaiseSQLException(E);
      end;
    end;
  end;

  FRowsList.Add(FRowAccessor.RowBuffer);
  LastRowNo := FRowsList.Count;
  MoveAbsolute(LastRowNo);
end;

{**
  Updates the underlying database with the new contents of the
  current row of this <code>ResultSet</code> object.
  This method cannot be called when the cursor is on the insert row.
}
procedure TZAbstractCachedResultSet.UpdateRow;
begin
  CheckUpdatable;
  if (RowNo < 1) or (RowNo > LastRowNo) then
    raise EZSQLException.Create(SCanNotUpdateEmptyRow);

  if PZRowBuffer(FRowsList[RowNo - 1]).UpdateType = utDeleted then
    raise EZSQLException.Create(SCanNotUpdateDeletedRow);

  if FSelectedRow <> FUpdatedRow then
      Exit;

  AppendRow(FRowsList[RowNo - 1]);

  FSelectedRow := PZRowBuffer(FRowsList[RowNo - 1]);
  FRowAccessor.CopyBuffer(FUpdatedRow, FSelectedRow);
  FRowAccessor.RowBuffer := FSelectedRow;
  if FSelectedRow.UpdateType = utUnmodified then
    FSelectedRow.UpdateType := utModified;

  { Posts non-cached updates. }
  if not FCachedUpdates then
  begin
    try
      PostUpdates;
    except
      on E: Exception do
      begin
        { Restore the previous state. }
        FRowAccessor.DisposeBuffer(FRowsList[RowNo - 1]);
        FRowsList[RowNo - 1] := FInitialRowsList[FInitialRowsList.Count - 1];
        FInitialRowsList.Delete(FInitialRowsList.Count - 1);
        FCurrentRowsList.Delete(FCurrentRowsList.Count - 1);

        FSelectedRow := PZRowBuffer(FRowsList[RowNo - 1]);
        FRowAccessor.RowBuffer := FSelectedRow;

        { Reraises the exception. }
        RaiseSQLException(E);
      end;
    end;
  end;
end;

{**
  Deletes the current row from this <code>ResultSet</code> object
  and from the underlying database.  This method cannot be called when
  the cursor is on the insert row.
}
procedure TZAbstractCachedResultSet.DeleteRow;
begin
  CheckUpdatable;
  if (RowNo < 1) or (RowNo > LastRowNo) or (FSelectedRow = nil) then
    raise EZSQLException.Create(SCanNotDeleteEmptyRow);

  if FSelectedRow^.UpdateType = utInserted then
    RevertRecord
  else
  begin
    AppendRow(FRowsList[RowNo - 1]);

    FSelectedRow^.UpdateType := utDeleted;
    if FSelectedRow = FUpdatedRow then
      FRowAccessor.CopyBuffer(FUpdatedRow, FRowsList[RowNo - 1]);

    { Posts non-cached updates. }
    if not FCachedUpdates then
    begin
      try
        PostUpdates;
      except
        on E: Exception do
        begin
          { Restores the previous state. }
          FRowAccessor.DisposeBuffer(FRowsList[RowNo - 1]);
          FRowsList[RowNo - 1] := FInitialRowsList[FInitialRowsList.Count - 1];
          FSelectedRow := FRowsList[RowNo - 1];
          FInitialRowsList.Delete(FInitialRowsList.Count - 1);
          FCurrentRowsList.Delete(FCurrentRowsList.Count - 1);

          { Rethrows the exception. }
          RaiseSQLException(E);
        end;
      end;
    end;
  end;
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
procedure TZAbstractCachedResultSet.CancelRowUpdates;
begin
  MoveAbsolute(RowNo);
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
procedure TZAbstractCachedResultSet.MoveToInsertRow;
begin
  CheckClosed;
  FRowAccessor.RowBuffer := FInsertedRow;
end;

{**
  Moves the cursor to the remembered cursor position, usually the
  current row.  This method has no effect if the cursor is not on
  the insert row.
}
procedure TZAbstractCachedResultSet.MoveToCurrentRow;
begin
  CheckClosed;
  if (RowNo >= 1) and (RowNo <= LastRowNo) then
    FRowAccessor.RowBuffer := FSelectedRow
  else
    FRowAccessor.RowBuffer := nil;
end;

{**
  Compares fields from two row buffers.
  @param Row1 the first row buffer to compare.
  @param Row2 the second row buffer to compare.
  @param ColumnIndices column indices to compare.
  @param ColumnDirs compare direction for each columns.
}
function TZAbstractCachedResultSet.CompareRows(Row1, Row2: NativeInt;
  const ColumnIndices: TIntegerDynArray; const CompareFuncs: TCompareFuncs): Integer;
var
  RowBuffer1, RowBuffer2: PZRowBuffer;
begin
{$IFNDEF DISABLE_CHECKING}
  if ResultSetType = rtForwardOnly then
    RaiseForwardOnlyException;
{$ENDIF}
  RowBuffer1 := PZRowBuffer(FRowsList[Row1 - 1]);
  RowBuffer2 := PZRowBuffer(FRowsList[Row2 - 1]);
  Result := FRowAccessor.CompareBuffers(RowBuffer1, RowBuffer2,
    ColumnIndices, CompareFuncs);
end;

function TZAbstractCachedResultSet.GetCompareFuncs(const ColumnIndices: TIntegerDynArray;
  const CompareKinds: TComparisonKindArray): TCompareFuncs;
begin
  Result := FRowAccessor.GetCompareFuncs(ColumnIndices, CompareKinds);
end;

{ TZCachedResultSet }

{**
  Creates this object and assignes the main properties.
  @param ResultSet a wrapped resultset object.
  @param Resolver a cached updates resolver object.
}
constructor TZCachedResultSet.Create(const ResultSet: IZResultSet; const SQL: string;
  const Resolver: IZCachedResolver; ConSettings: PZConSettings);
begin
  inherited Create(ResultSet.GetStatement, SQL, nil, ConSettings);
  FResultSet := ResultSet;
  FResolver := Resolver;
  {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  FNativeResolver := Resolver;
  {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  if (ConSettings^.ClientCodePage^.Encoding in [ceAnsi, ceUTF8]) and
    ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
      FStringFieldAssignFromResultSet := ZStringFieldAssignFromResultSet_AnsiRec
    else
      FStringFieldAssignFromResultSet := ZStringFieldAssignFromResultSet_Unicode;
  Open;
end;

procedure TZCachedResultSet.ZStringFieldAssignFromResultSet_AnsiRec(ColumnIndex: Integer);
var Len: NativeUInt;
begin
  RowAccessor.SetPAnsiChar(ColumnIndex, ResultSet.GetPAnsiChar(ColumnIndex, Len), @Len);
end;

procedure TZCachedResultSet.ZStringFieldAssignFromResultSet_Unicode(ColumnIndex: Integer);
var Len: NativeUInt;
begin
  RowAccessor.SetPWideChar(ColumnIndex, ResultSet.GetPWideChar(ColumnIndex, Len), @Len);
end;

{**
  Fetches one row from the wrapped result set object.
  @return <code>True</code> if row was successfuly fetched
    or <code>False</code> otherwise.
}
function TZCachedResultSet.Fetch: Boolean;
var
  I: Integer;
  TempRow: PZRowBuffer;
begin
  if Assigned(FResultSet) then
    Result := FResultSet.Next
  else
    Result := False;
  if not Result or ((MaxRows > 0) and (LastRowNo >= MaxRows)) then
    Exit;

  TempRow := RowAccessor.RowBuffer;
  try
    RowAccessor.Alloc;
    RowAccessor.RowBuffer.Index := GetNextRowIndex;
    RowAccessor.RowBuffer.UpdateType := utUnmodified;

    for I := FirstDbcIndex to ColumnsInfo.Count{$IFDEF GENERIC_INDEX}-1{$ENDIF} do begin
      if ResultSet.IsNull(i) then
        continue
      else case TZColumnInfo(ColumnsInfo[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType of
        stBoolean: RowAccessor.SetBoolean(I, ResultSet.GetBoolean(I));
        stByte: RowAccessor.SetByte(I, ResultSet.GetByte(I));
        stShort: RowAccessor.SetShort(I, ResultSet.GetShort(I));
        stWord: RowAccessor.SetWord(I, ResultSet.GetWord(I));
        stSmall: RowAccessor.SetSmall(I, ResultSet.GetSmall(I));
        stLongWord: RowAccessor.SetUInt(I, ResultSet.GetUInt(I));
        stInteger: RowAccessor.SetInt(I, ResultSet.GetInt(I));
        stULong: RowAccessor.SetULong(I, ResultSet.GetULong(I));
        stLong: RowAccessor.SetLong(I, ResultSet.GetLong(I));
        stFloat: RowAccessor.SetFloat(I, ResultSet.GetFloat(I));
        stDouble: RowAccessor.SetDouble(I, ResultSet.GetDouble(I));
        stCurrency: RowAccessor.SetCurrency(I, ResultSet.GetCurrency(I));
        stBigDecimal: RowAccessor.SetBigDecimal(I, ResultSet.GetBigDecimal(I));
        stString, stUnicodeString: FStringFieldAssignFromResultSet(i);
        stBytes,stGUID: RowAccessor.SetBytes(I, ResultSet.GetBytes(I));
        stDate: RowAccessor.SetDate(I, ResultSet.GetDate(I));
        stTime: RowAccessor.SetTime(I, ResultSet.GetTime(I));
        stTimestamp: RowAccessor.SetTimestamp(I, ResultSet.GetTimestamp(I));
        stAsciiStream, stBinaryStream, stUnicodeStream:
          RowAccessor.SetBlob(I, ResultSet.GetBlob(I));
        stDataSet: RowAccessor.SetDataSet(i, ResultSet.GetDataSet(I));
      end;
      if ResultSet.WasNull then
        RowAccessor.SetNull(I);
    end;

    RowsList.Add(RowAccessor.RowBuffer);
    LastRowNo := RowsList.Count;
  finally
    RowAccessor.RowBuffer := TempRow;
  end;
end;

{**
  Fetches all of the rest rows from the wrapped result set.
}
procedure TZCachedResultSet.FetchAll;
begin
  while Fetch do;
end;

{**
  Opens this recordset.
}
procedure TZCachedResultSet.Open;
var
  I: Integer;
  ColumnInfo: TZColumnInfo;
  MetaData : IZResultSetMetaData;
begin
  ColumnsInfo.Clear;
  MetaData := FResultSet.GetMetadata;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    ColumnInfo := TZColumnInfo.Create;
    with ColumnInfo do
    begin
      Currency := Metadata.IsCurrency(I);
      Signed := Metadata.IsSigned(I);
      ColumnDisplaySize := Metadata.GetColumnDisplaySize(I);
      ColumnLabel := Metadata.GetColumnLabel(I);
      Precision := Metadata.GetPrecision(I);
      Scale := Metadata.GetScale(I);
      ColumnType := Metadata.GetColumnType(I);
      ColumnCodePage := MetaData.GetColumnCodePage(I);
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
procedure TZCachedResultSet.AfterClose;
begin
  inherited AfterClose;
  If Assigned(FResultset) then begin
    FResultset.Close;
    FResultSet := nil;
  end;
end;

constructor TZCachedResultSet.CreateWithColumns(const ColumnsInfo: TObjectList;
  const ResultSet: IZResultSet; const SQL: string;
  const Resolver: IZCachedResolver; ConSettings: PZConSettings);
begin
  inherited Create(ResultSet.GetStatement, SQL, nil, ConSettings);
  FResultSet := ResultSet;
  FResolver := Resolver;
  {BEGIN PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  FNativeResolver := Resolver;
  {END PATCH [1214009] CalcDefaults in TZUpdateSQL and Added Methods to GET the DB NativeResolver}
  if (ConSettings^.ClientCodePage^.Encoding in [ceAnsi, ceUTF8]) and
    ConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
      FStringFieldAssignFromResultSet := ZStringFieldAssignFromResultSet_AnsiRec
    else
      FStringFieldAssignFromResultSet := ZStringFieldAssignFromResultSet_Unicode;
  ZDbcUtils.CopyColumnsInfo(ColumnsInfo, Self.ColumnsInfo);
  inherited Open;
end;

procedure TZCachedResultSet.ResetCursor;
begin
  if not Closed then begin
    If Assigned(FResultset) then
      FResultset.ResetCursor;
    inherited ResetCursor;
  end;
end;
{**
  Retrieves the  number, types and properties of
  this <code>ResultSet</code> object's columns.
  @return the description of this <code>ResultSet</code> object's columns
}
function TZCachedResultSet.GetMetadata: IZResultSetMetadata;
begin
  If Assigned(FResultset) then
    Result := ResultSet.GetMetadata
  else
    Result := nil;
end;

{**
  Indicates whether the cursor is after the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is after the last row;
    <code>false</code> if the cursor is at any other position or the
    result set contains no rows
}
function TZCachedResultSet.IsAfterLast: Boolean;
begin
  FetchAll;
  Result := inherited IsAfterLast;
end;

{**
  Moves the cursor to the end of
  this <code>ResultSet</code> object, just after the
  last row. This method has no effect if the result set contains no rows.
}
procedure TZCachedResultSet.AfterLast;
begin
  FetchAll;
  inherited AfterLast;
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
function TZCachedResultSet.IsLast: Boolean;
begin
  FetchAll;
  Result := inherited IsLast;
end;

{**
  Moves the cursor to the last row in
  this <code>ResultSet</code> object.

  @return <code>true</code> if the cursor is on a valid row;
    <code>false</code> if there are no rows in the result set
}
function TZCachedResultSet.Last: Boolean;
begin
  FetchAll;
  Result := inherited Last;
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
function TZCachedResultSet.MoveAbsolute(Row: Integer): Boolean;
begin
  { Checks for maximum row. }
  Result := False;
  if ((MaxRows > 0) and (Row > MaxRows)) then
    Exit;

  { Processes negative rows }
  if Row < 0 then
  begin
    FetchAll;
    Row := LastRowNo - Row + 1;
    if Row < 0 then
       Row := 0;
  end
  else
  { Processes moving after last row }
    while (LastRowNo < Row) and Fetch do;

  Result := inherited MoveAbsolute(Row);
end;

end.

