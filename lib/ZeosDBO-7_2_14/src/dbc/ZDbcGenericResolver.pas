{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                Generic Cached Resolver                  }
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

unit ZDbcGenericResolver;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF} StrUtils,
  ZVariant, ZDbcIntfs, ZDbcCache, ZDbcCachedResultSet, ZCompatibility,
  ZSelectSchema, {$IF defined(OLDFPC) or defined(NO_UNIT_CONTNRS)}ZClasses,{$IFEND} ZCollections;

type

  {** Implements a resolver parameter object. }
  TZResolverParameter = class (TObject)
  private
    FColumnIndex: Integer;
    FColumnName: string;
    FColumnType: TZSQLType;
    FNewValue: Boolean;
    FDefaultValue: string;
  public
    constructor Create(ColumnIndex: Integer; const ColumnName: string;
      ColumnType: TZSQLType; NewValue: Boolean; const DefaultValue: string);

    property ColumnIndex: Integer read FColumnIndex write FColumnIndex;
    property ColumnName: string read FColumnName write FColumnName;
    property ColumnType: TZSQLType read FColumnType write FColumnType;
    property NewValue: Boolean read FNewValue write FNewValue;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
  end;

  {**
    Implements a generic cached resolver object which generates
    DML SQL statements and posts resultset updates to database.
  }

  { TZGenericCachedResolver }

  TZGenericCachedResolver = class (TInterfacedObject, IZCachedResolver)
  private
    FConnection: IZConnection;
    FStatement : IZStatement;
    FMetadata: IZResultSetMetadata;
    FDatabaseMetadata: IZDatabaseMetadata;
    FIdentifierConvertor: IZIdentifierConvertor;

    FInsertColumns: TObjectList;
    FUpdateColumns: TObjectList;
    FWhereColumns: TObjectList;

    FInsertParams: TObjectList;
    FUpdateParams: TObjectList;
    FDeleteParams: TObjectList;

    FCalcDefaults: Boolean;
    FWhereAll: Boolean;
    FUpdateAll: Boolean;

    FStatements : TZHashMap;
  protected
    InsertStatement   : IZPreparedStatement;
    UpdateStatement   : IZPreparedStatement;
    DeleteStatement   : IZPreparedStatement;

    procedure CopyResolveParameters(FromList, ToList: TObjectList);
    function ComposeFullTableName(const Catalog, Schema, Table: string): string;
    function DefineTableName: string;

    function CreateResolverStatement(const SQL : String):IZPreparedStatement;

    procedure DefineCalcColumns(Columns: TObjectList;
      RowAccessor: TZRowAccessor);
    procedure DefineInsertColumns(Columns: TObjectList);
    procedure DefineUpdateColumns(Columns: TObjectList;
      OldRowAccessor, NewRowAccessor: TZRowAccessor);
    procedure DefineWhereKeyColumns(Columns: TObjectList);
    procedure DefineWhereAllColumns(Columns: TObjectList; IgnoreKeyColumn: Boolean = False);
    function CheckKeyColumn(ColumnIndex: Integer): Boolean; virtual;

    procedure FillStatement(const Statement: IZPreparedStatement;
      Params: TObjectList; OldRowAccessor, NewRowAccessor: TZRowAccessor);

    property Connection: IZConnection read FConnection write FConnection;
    property Metadata: IZResultSetMetadata read FMetadata write FMetadata;
    property DatabaseMetadata: IZDatabaseMetadata read FDatabaseMetadata
      write FDatabaseMetadata;
    property IdentifierConvertor: IZIdentifierConvertor
      read FIdentifierConvertor write FIdentifierConvertor;

    property InsertColumns: TObjectList read FInsertColumns;
    property UpdateColumns: TObjectList read FUpdateColumns;
    property WhereColumns: TObjectList read FWhereColumns;

    property CalcDefaults: Boolean read FCalcDefaults write FCalcDefaults;
    property WhereAll: Boolean read FWhereAll write FWhereAll;
    property UpdateAll: Boolean read FUpdateAll write FUpdateAll;

  public
    constructor Create(const Statement: IZStatement; const Metadata: IZResultSetMetadata);
    destructor Destroy; override;

    function FormWhereClause(Columns: TObjectList;
      OldRowAccessor: TZRowAccessor): string; virtual;
    function FormInsertStatement(Columns: TObjectList;
      {%H-}NewRowAccessor: TZRowAccessor): string; virtual;
    function FormUpdateStatement(Columns: TObjectList;
      OldRowAccessor, NewRowAccessor: TZRowAccessor): string; virtual;
    function FormDeleteStatement(Columns: TObjectList;
      OldRowAccessor: TZRowAccessor): string;
    function FormCalculateStatement(Columns: TObjectList): string; virtual;

    procedure CalculateDefaults(Sender: IZCachedResultSet;
      RowAccessor: TZRowAccessor);
    procedure PostUpdates(Sender: IZCachedResultSet;
      UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor); virtual;
    {BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure UpdateAutoIncrementFields(Sender: IZCachedResultSet;
      UpdateType: TZRowUpdateType;
      OldRowAccessor, NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver); virtual;
    {END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
    procedure RefreshCurrentRow(Sender: IZCachedResultSet; RowAccessor: TZRowAccessor); //FOS+ 07112006

  end;

implementation

uses ZMessages, ZSysUtils, ZDbcMetadata, ZDbcUtils
  {$IF not defined(OLDFPC) and not defined(NO_UNIT_CONTNRS)},ZClasses{$IFEND}
  {$IFDEF FAST_MOVE}, ZFastCode{$ENDIF};

{ TZResolverParameter }

{**
  Constructs this resolver parameter and assignes the main properties.
  @param ColumnIndex a result set column index.
  @param ColumnName a result set column name.
  @param NewValue <code>True</code> for new value and <code>False</code>
    for an old one.
  @param DefaultValue a default column value to evalute on server.
}
constructor TZResolverParameter.Create(ColumnIndex: Integer;
  const ColumnName: string; ColumnType: TZSQLType; NewValue: Boolean; const DefaultValue: string);
begin
  FColumnType := ColumnType;
  FColumnIndex := ColumnIndex;
  FColumnName := ColumnName;
  FNewValue := NewValue;
  FDefaultValue := DefaultValue;
end;

{ TZGenericCachedResolver }

{**
  Creates a cached resolver and assignes the main properties.
  @param ResultSet a related ResultSet object.
}
constructor TZGenericCachedResolver.Create(const Statement: IZStatement;
  const Metadata: IZResultSetMetadata);
begin
  FStatement := Statement;
  FConnection := Statement.GetConnection;
  FMetadata := Metadata;
  FDatabaseMetadata := Statement.GetConnection.GetMetadata;
  FIdentifierConvertor := FDatabaseMetadata.GetIdentifierConvertor;

  FInsertColumns := TObjectList.Create(True);
  FWhereColumns := TObjectList.Create(True);
  FUpdateColumns := TObjectList.Create(True);

  FInsertParams := TObjectList.Create(True);
  FUpdateParams := TObjectList.Create(True);
  FDeleteParams := TObjectList.Create(True);

  FCalcDefaults := StrToBoolEx(DefineStatementParameter(Statement,
    'defaults', 'true'));
  FUpdateAll := UpperCase(DefineStatementParameter(Statement,
    'update', 'changed')) = 'ALL';
  FWhereAll := UpperCase(DefineStatementParameter(Statement,
    'where', 'keyonly')) = 'ALL';

  InsertStatement := nil;
  FStatements := TZHashMap.Create;
  DeleteStatement := nil;

end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZGenericCachedResolver.Destroy;
procedure FlustStmt(var Stmt: IZPreparedStatement);
begin
  if Stmt <> nil then begin
    Stmt.Close;
    Stmt := nil
  end;
end;

begin
  FMetadata := nil;
  FDatabaseMetadata := nil;

  FreeAndNil(FInsertColumns);
  FreeAndNil(FUpdateColumns);
  FreeAndNil(FWhereColumns);

  FreeAndNil(FInsertParams);
  FreeAndNil(FUpdateParams);
  FreeAndNil(FDeleteParams);

  FreeAndNil(FStatements);
  FlustStmt(InsertStatement);
  FlustStmt(UpdateStatement);
  FlustStmt(DeleteStatement);
  inherited Destroy;
end;

{**
  Copies resolver parameters from source list to destination list.
  @param FromList the source object list.
  @param ToList the destination object list.
}
procedure TZGenericCachedResolver.CopyResolveParameters(
  FromList: TObjectList; ToList: TObjectList);
var
  I: Integer;
  Current: TZResolverParameter;
begin
  for I := 0 to FromList.Count - 1 do
  begin
    Current := TZResolverParameter(FromList[I]);
    if Current.ColumnName <> '' then
      ToList.Add(TZResolverParameter.Create(Current.ColumnIndex,
        Current.ColumnName, Current.ColumnType, Current.NewValue, ''));
  end;
end;

{**
  Composes a fully quilified table name.
  @param Catalog a table catalog name.
  @param Schema a table schema name.
  @param Table a table name.
  @return a fully qualified table name.
}
function TZGenericCachedResolver.ComposeFullTableName(const Catalog, Schema,
  Table: string): string;
begin
  if Table <> '' then begin
    Result := IdentifierConvertor.Quote(Table);
    if (Schema <> '') and FDatabaseMetadata.GetDatabaseInfo.SupportsSchemasInDataManipulation then
      Result := IdentifierConvertor.Quote(Schema) + '.' + Result;
    if (Catalog <> '') and FDatabaseMetadata.GetDatabaseInfo.SupportsCatalogsInDataManipulation then
      Result := IdentifierConvertor.Quote(Catalog) + '.' + Result;
  end else
    Result := '';
end;

{**
  Defines a table name from the select statement.
}
function TZGenericCachedResolver.DefineTableName: string;
var
  I: Integer;
  Temp: string;
begin
  Result := '';
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    Temp := ComposeFullTableName(Metadata.GetCatalogName(I),
      Metadata.GetSchemaName(I), Metadata.GetTableName(I));
    if (Result = '') and (Temp <> '') then
      Result := Temp
    else if (Result <> '') and (Temp <> '') and (Temp <> Result) then
      raise EZSQLException.Create(SCanNotUpdateComplexQuery);
  end;
  if Result = '' then
    raise EZSQLException.Create(SCanNotUpdateThisQueryType);
end;

function TZGenericCachedResolver.CreateResolverStatement(const SQL: String): IZPreparedStatement;
var
  Temp : TStrings;
begin
  if StrToBoolEx(FStatement.GetParameters.Values['preferprepared']) then
    begin
      Temp := TStringList.Create;
      Temp.Values['preferprepared'] := 'true';
      if not ( Connection.GetParameters.Values['chunk_size'] = '' ) then //ordered by precedence
        Temp.Values['chunk_size'] := Connection.GetParameters.Values['chunk_size']
      else
        Temp.Values['chunk_size'] := FStatement.GetParameters.Values['chunk_size'];
      Result := Connection.PrepareStatementWithParams(SQL, Temp);
      Temp.Free;
    end
  else
    Result := Connection.PrepareStatement(SQL);

end;

{**
  Gets a collection of data columns for INSERT statements.
  @param Columns a collection of columns.
}
procedure TZGenericCachedResolver.DefineInsertColumns(Columns: TObjectList);
var
  I: Integer;
begin
  { Precache insert parameters. }
  if InsertColumns.Count = 0 then
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if (Metadata.GetTableName(I) <> '') and (Metadata.GetColumnName(I) <> '')
        and Metadata.IsWritable(I) then
      begin
        InsertColumns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I), True, ''));
      end;
  { Use cached insert parameters }
  CopyResolveParameters(InsertColumns, Columns);
end;

{**
  Gets a collection of data columns for UPDATE statements.
  @param Columns a collection of columns.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZGenericCachedResolver.DefineUpdateColumns(
  Columns: TObjectList; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var I: Integer;
begin
  { Use precached parameters. }
  if UpdateAll and (UpdateColumns.Count > 0) then begin
    CopyResolveParameters(UpdateColumns, Columns);
    Exit;
  end;

  { Defines parameters for UpdateAll mode. }
  if UpdateAll then begin
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if (Metadata.GetTableName(I) <> '') and
         (Metadata.GetColumnName(I) <> '') and Metadata.IsWritable(I) then
        UpdateColumns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I), True, ''));
    CopyResolveParameters(UpdateColumns, Columns);
  end else { Defines parameters for UpdateChanged mode. }
    for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      if (Metadata.GetTableName(I) <> '') and
         (Metadata.GetColumnName(I) <> '') and Metadata.IsWritable(I) and
         (OldRowAccessor.CompareBuffer(OldRowAccessor.RowBuffer,
          NewRowAccessor.RowBuffer, I, NewRowAccessor.GetCompareFunc(I, ckEquals))  <> 0) then
        Columns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I), True, ''));
end;

{**
  Gets a collection of where key columns for DELETE or UPDATE DML statements.
  @param Columns a collection of key columns.
}
procedure TZGenericCachedResolver.DefineWhereKeyColumns(Columns: TObjectList);
var
  I: Integer;
  Found: Boolean;
  ColumnName: string;
  Catalog, Schema, Table: string;
  PrimaryKeys: IZResultSet;
begin
  { Use precached values. }
  if WhereColumns.Count > 0 then
  begin
    CopyResolveParameters(WhereColumns, Columns);
    Exit;
  end;

  { Defines catalog, schema and a table. }
  Table := DefineTableName;
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    Table := Metadata.GetTableName(I);
    if Table <> '' then
    begin
      Schema := Metadata.GetSchemaName(I);
      Catalog := Metadata.GetCatalogName(I);
      Break;
    end;
  end;

  { Tryes to define primary keys. }
  if not WhereAll then
  begin
    {For exact results: quote all identifiers SEE: http://sourceforge.net/p/zeoslib/tickets/81/
    If table names have mixed case ConstructNameCondition will return wrong results
    and we fall back to WhereAll}
    PrimaryKeys := DatabaseMetadata.GetPrimaryKeys(IdentifierConvertor.Quote(Catalog),
      IdentifierConvertor.Quote(Schema), IdentifierConvertor.Quote(Table));
    while PrimaryKeys.Next do
    begin
      ColumnName := PrimaryKeys.GetString(ColumnNameIndex);
      Found := False;
      for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      begin
        if (ColumnName = Metadata.GetColumnName(I))
          and (Table = Metadata.GetTableName(I)) then
        begin
          Found := True;
          Break;
        end;
      end;
      if not Found then
      begin
        WhereColumns.Clear;
        Break;
      end;
      WhereColumns.Add(TZResolverParameter.Create(I, ColumnName,
        stUnknown, False, ''));
    end;
  end;

  if WhereColumns.Count > 0 then
    CopyResolveParameters(WhereColumns, Columns)
  else
    DefineWhereAllColumns(Columns);
end;

{**
  Gets a collection of where all columns for DELETE or UPDATE DML statements.
  @param Columns a collection of key columns.
}
procedure TZGenericCachedResolver.DefineWhereAllColumns(Columns: TObjectList;
  IgnoreKeyColumn: Boolean = False);
var
  I: Integer;
begin
  { Use precached values. }
  if WhereColumns.Count > 0 then
  begin
    CopyResolveParameters(WhereColumns, Columns);
    Exit;
  end;

  { Takes a a key all non-blob fields. }
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    if CheckKeyColumn(I) then
      WhereColumns.Add(TZResolverParameter.Create(I,
        Metadata.GetColumnName(I), Metadata.GetColumnType(I), False, ''))
    else
      if IgnoreKeyColumn then
        WhereColumns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I), False, ''));
  end;
  if ( WhereColumns.Count = 0 ) and ( not IgnoreKeyColumn ) then
    DefineWhereAllColumns(Columns, True)
  else
    { Copy defined parameters to target columns }
    CopyResolveParameters(WhereColumns, Columns);
end;

{**
  Checks is the specified column can be used in where clause.
  @param ColumnIndex an index of the column.
  @returns <code>true</code> if column can be included into where clause.
}
function TZGenericCachedResolver.CheckKeyColumn(ColumnIndex: Integer): Boolean;
begin
  Result := (Metadata.GetTableName(ColumnIndex) <> '')
    and (Metadata.GetColumnName(ColumnIndex) <> '')
    and Metadata.IsSearchable(ColumnIndex)
    and not (Metadata.GetColumnType(ColumnIndex)
    in [stUnknown, stAsciiStream, stBinaryStream, stUnicodeStream]);
end;

{**
  Gets a collection of data columns to initialize before INSERT statements.
  @param Columns a collection of columns.
  @param RowAccessor an accessor object to column values.
}
procedure TZGenericCachedResolver.DefineCalcColumns(Columns: TObjectList;
  RowAccessor: TZRowAccessor);
var
  I: Integer;
begin
  for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
  begin
    if RowAccessor.IsNull(I) and (Metadata.GetTableName(I) <> '')
      and ((Metadata.GetDefaultValue(I) <> '') or (RowAccessor.GetColumnDefaultExpression(I) <> '')) then
    begin
      // DefaultExpression takes takes precedence on database default value
      if RowAccessor.GetColumnDefaultExpression(I) <> '' then
        Columns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I),
          True, RowAccessor.GetColumnDefaultExpression(I)))
      else
        Columns.Add(TZResolverParameter.Create(I,
          Metadata.GetColumnName(I), Metadata.GetColumnType(I),
          True, Metadata.GetDefaultValue(I)));
    end;
  end;
end;

{**
  Fills the specified statement with stored or given parameters.
  @param ResultSet a source result set object.
  @param Statement a DBC statement object.
  @param Config an UpdateStatement configuration.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZGenericCachedResolver.FillStatement(const Statement: IZPreparedStatement;
  Params: TObjectList; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  I: Integer;
  ColumnIndex: Integer;
  Current: TZResolverParameter;
  RowAccessor: TZRowAccessor;
  WasNull: Boolean;
  TempBytes: TBytes;
begin
  WasNull := False;
  for I := 0 to Params.Count - 1 do
  begin
    Current := TZResolverParameter(Params[I]);
    if Current.NewValue then
      RowAccessor := NewRowAccessor
    else
      RowAccessor := OldRowAccessor;
    ColumnIndex := Current.ColumnIndex;

    if FCalcDefaults then
      Statement.SetDefaultValue(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Metadata.GetDefaultValue(ColumnIndex));
    if RowAccessor.IsNull(ColumnIndex) then
      Statement.SetNull(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Metadata.GetColumnType(ColumnIndex))
    else case Metadata.GetColumnType(ColumnIndex) of
      stBoolean:
        Statement.SetBoolean(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          RowAccessor.GetBoolean(ColumnIndex, WasNull));
      stByte:
        Statement.SetByte(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetByte(ColumnIndex, WasNull));
      stShort:
        Statement.SetShort(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetShort(ColumnIndex, WasNull));
      stWord:
        Statement.SetWord(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetWord(ColumnIndex, WasNull));
      stSmall:
        Statement.SetSmall(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetSmall(ColumnIndex, WasNull));
      stLongWord:
        Statement.SetUInt(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetUInt(ColumnIndex, WasNull));
      stInteger:
        Statement.SetInt(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetInt(ColumnIndex, WasNull));
      stULong:
        Statement.SetULong(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetULong(ColumnIndex, WasNull));
      stLong:
        Statement.SetLong(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetLong(ColumnIndex, WasNull));
      stFloat:
        Statement.SetFloat(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetFloat(ColumnIndex, WasNull));
      stCurrency:
        Statement.SetCurrency(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetCurrency(ColumnIndex, WasNull));
      stDouble:
        Statement.SetDouble(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetDouble(ColumnIndex, WasNull));
      stBigDecimal:
        Statement.SetBigDecimal(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          RowAccessor.GetBigDecimal(ColumnIndex, WasNull));
      stString, stUnicodeString:
        Statement.SetCharRec(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          RowAccessor.GetCharRec(ColumnIndex, WasNull));
      stBytes:
        Statement.SetBytes(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetBytes(ColumnIndex, WasNull));
      stGUID: begin
          TempBytes := RowAccessor.GetBytes(ColumnIndex, WasNull);
          if Length(TempBytes) <> 16
          then raise EZSQLException.Create('The rowaccessor did not return 16 bytes while trying to set a GUID.');
          Statement.SetGuid(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, PGUID(@TempBytes[0])^);
        end;
      stDate:
        Statement.SetDate(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetDate(ColumnIndex, WasNull));
      stTime:
        Statement.SetTime(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, RowAccessor.GetTime(ColumnIndex, WasNull));
      stTimestamp:
        Statement.SetTimestamp(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF},
          RowAccessor.GetTimestamp(ColumnIndex, WasNull));
      stAsciiStream:
         Statement.SetBlob(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stAsciiStream,
           RowAccessor.GetBlob(ColumnIndex, WasNull));
      stUnicodeStream:
         Statement.SetBlob(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stUnicodeStream,
           RowAccessor.GetBlob(ColumnIndex, WasNull));
      stBinaryStream:
         Statement.SetBlob(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, stBinaryStream,
           RowAccessor.GetBlob(ColumnIndex, WasNull));
    end;
  end;
end;

{**
  Forms a where clause for UPDATE or DELETE DML statements.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZGenericCachedResolver.FormWhereClause(Columns: TObjectList;
  OldRowAccessor: TZRowAccessor): string;
var
  I, N: Integer;
  Current: TZResolverParameter;
  Condition: string;
begin
  Result := '';
  N := Columns.Count - WhereColumns.Count;

  for I := 0 to WhereColumns.Count - 1 do
  begin
    Current := TZResolverParameter(WhereColumns[I]);

    Condition := IdentifierConvertor.Quote(Current.ColumnName);
    if OldRowAccessor.IsNull(Current.ColumnIndex) then
    begin
      Condition := Condition + ' IS NULL';
      Columns.Delete(N);
    end
    else
    begin
      Condition := Condition + '=?';
      Inc(N);
    end;
    AppendSepString(Result, Condition, ' AND ');
  end;

  if Result <> '' then
    Result := ' WHERE ' + Result;
end;

{**
  Forms a where clause for INSERT statements.
  @param Columns a collection of key columns.
  @param NewRowAccessor an accessor object to new column values.
}
function TZGenericCachedResolver.FormInsertStatement(Columns: TObjectList;
  NewRowAccessor: TZRowAccessor): string;
var
  I: Integer;
  Current: TZResolverParameter;
  TableName: string;
  Temp1: string;
begin
  TableName := DefineTableName;
  DefineInsertColumns(Columns);
  if Columns.Count = 0 then begin
    Result := '';
    Exit;
  end;

  Temp1 := '';
  for I := 0 to Columns.Count - 1 do begin
    Current := TZResolverParameter(Columns[I]);
    AppendSepString(Temp1, IdentifierConvertor.Quote(Current.ColumnName), ',');
  end;

  Result := 'INSERT INTO '+TableName+' ('+Temp1+') VALUES ('+
    DupeString('?,', Columns.Count - 1) + '?' +')';
end;

{**
  Forms a where clause for UPDATE statements.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
function TZGenericCachedResolver.FormUpdateStatement(Columns: TObjectList;
  OldRowAccessor, NewRowAccessor: TZRowAccessor): string;
var
  I: Integer;
  Current: TZResolverParameter;
  TableName: string;
  Temp: string;
begin
  TableName := DefineTableName;
  DefineUpdateColumns(Columns, OldRowAccessor, NewRowAccessor);
  if Columns.Count = 0 then
  begin
    Result := '';
    Exit;
  end;

  Temp := '';
  for I := 0 to Columns.Count - 1 do
  begin
    Current := TZResolverParameter(Columns[I]);
    AppendSepString(Temp, IdentifierConvertor.Quote(Current.ColumnName) + '=?', ',');
  end;

  Result := 'UPDATE '+TableName+' SET '+Temp;
  DefineWhereKeyColumns(Columns);
  Result := Result + FormWhereClause(Columns, OldRowAccessor);
end;

{**
  Forms a where clause for DELETE statements.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZGenericCachedResolver.FormDeleteStatement(Columns: TObjectList;
  OldRowAccessor: TZRowAccessor): string;
begin
  Result := 'DELETE FROM '+ DefineTableName;
  DefineWhereKeyColumns(Columns);
  Result := Result + FormWhereClause(Columns, OldRowAccessor);
end;

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZGenericCachedResolver.FormCalculateStatement(
  Columns: TObjectList): string;
var
  I: Integer;
  Current: TZResolverParameter;
begin
  Result := '';
  if Columns.Count = 0 then
     Exit;

  for I := 0 to Columns.Count - 1 do
  begin
    Current := TZResolverParameter(Columns[I]);
    if Current.DefaultValue <> '' then
      AppendSepString(Result, Current.DefaultValue, ',')
    else
      AppendSepString(Result, 'NULL', ',');
  end;
  Result := 'SELECT ' + Result;
end;

{**
  Posts updates to database.
  @param Sender a cached result set object.
  @param UpdateType a type of updates.
  @param OldRowAccessor an accessor object to old column values.
  @param NewRowAccessor an accessor object to new column values.
}
procedure TZGenericCachedResolver.PostUpdates(Sender: IZCachedResultSet;
  UpdateType: TZRowUpdateType; OldRowAccessor, NewRowAccessor: TZRowAccessor);
var
  Statement            : IZPreparedStatement;
  SQL                  : string;
  SQLParams            : TObjectList;
  lUpdateCount         : Integer;
  lValidateUpdateCount : Boolean;
  TempKey              : IZAnyValue;
  SenderStatement      : IZStatement;
begin
  if (UpdateType = utDeleted) and (OldRowAccessor.RowBuffer.UpdateType = utInserted) then
    Exit;

  case UpdateType of
    utInserted:
      begin
        if InsertStatement = nil then begin
          SQL := FormInsertStatement(FInsertParams, NewRowAccessor);
          InsertStatement := CreateResolverStatement(SQL);
          Statement := InsertStatement;
        end;
        Statement := InsertStatement;
        SQLParams := FInsertParams;
      end;
    utDeleted:
      begin
        if not FWhereAll then begin
          If DeleteStatement = nil then begin
            SQL := FormDeleteStatement(FDeleteParams, OldRowAccessor);
            DeleteStatement := CreateResolverStatement(SQL);
          end;
          Statement := DeleteStatement;
          SQLParams := FDeleteParams;
        end else begin
          FDeleteParams.Clear;  //EH: where columns propably are cached after 1. call
          SQL := FormDeleteStatement(FDeleteParams, OldRowAccessor);
          if SQL = '' then Exit;
          TempKey := TZAnyValue.CreateWithInteger(Hash(SQL));
          Statement := FStatements.Get(TempKey) as IZPreparedStatement;
          If Statement = nil then begin
            Statement := CreateResolverStatement(SQL);
            FStatements.Put(TempKey, Statement);
          end;
          SQLParams := FDeleteParams;
        end;
      end;
    utModified:
      begin
        FUpdateParams.Clear;  //EH: where columns propably are cached after 1. call
        //now what's faster?: caching stmts too by using a hashmap or recreate always
        //first of all: we need the new command-stmt
        SQL := FormUpdateStatement(FUpdateParams, OldRowAccessor, NewRowAccessor);
        If SQL = '' then exit;// no fields have been changed
        TempKey := TZAnyValue.CreateWithInteger(Hash(SQL));
        UpdateStatement := FStatements.Get(TempKey) as IZPreparedStatement;
        If UpdateStatement = nil then begin
          UpdateStatement := CreateResolverStatement(SQL);
          FStatements.Put(TempKey, UpdateStatement);
        end;
        Statement := UpdateStatement;
        SQLParams := FUpdateParams;
      end;
    else
      Exit;
  end;

  FillStatement(Statement, SQLParams, OldRowAccessor, NewRowAccessor);

  // if Property ValidateUpdateCount isn't set : assume it's true
  SenderStatement := Sender.GetStatement;
  if Assigned(SenderStatement) then begin
    SQL := SenderStatement.GetParameters.Values['ValidateUpdateCount'];
    lValidateUpdateCount := (SQL = '') or StrToBoolEx(SQL);
  end else begin
    lValidateUpdateCount := true;
  end;

  lUpdateCount := Statement.ExecuteUpdatePrepared;
  {$IFDEF WITH_VALIDATE_UPDATE_COUNT}
  if  (lValidateUpdateCount) and (lUpdateCount <> 1   ) then
    raise EZSQLException.Create(Format(SInvalidUpdateCount, [lUpdateCount]));
  {$ENDIF}
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // abstract method - parameters not used intentionally
procedure TZGenericCachedResolver.RefreshCurrentRow(Sender: IZCachedResultSet; RowAccessor: TZRowAccessor);
begin
 raise EZSQLException.Create(SRefreshRowOnlySupportedWithUpdateObject);
end;

{**
  Calculate default values for the fields.
  @param Sender a cached result set object.
  @param RowAccessor an accessor object to column values.
}
procedure TZGenericCachedResolver.CalculateDefaults(
  Sender: IZCachedResultSet; RowAccessor: TZRowAccessor);
var
  I: Integer;
  SQL: string;
  SQLParams: TObjectList;
  Statement: IZStatement;
  ResultSet: IZResultSet;
  Metadata: IZResultSetMetadata;
  Current: TZResolverParameter;
  Len: NativeUInt;
begin
  if not FCalcDefaults then
     Exit;

  SQLParams := TObjectList.Create(True);
  try
    DefineCalcColumns(SQLParams, RowAccessor);
    SQL := FormCalculateStatement(SQLParams);
    if SQL = '' then
       Exit;

    { Executes statement and fills default fields. }
    Statement := Connection.CreateStatement;
    try
      ResultSet := Statement.ExecuteQuery(SQL);
      if ResultSet.Next then
      begin
        Metadata := ResultSet.GetMetadata;
        for I := FirstDbcIndex to Metadata.GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
        begin
          Current := TZResolverParameter(SQLParams[I{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]);
          try
            case Current.ColumnType of
              stBoolean:
                RowAccessor.SetBoolean(Current.ColumnIndex,
                  ResultSet.GetBoolean(I));
              stByte:
                RowAccessor.SetByte(Current.ColumnIndex, ResultSet.GetByte(I));
              stShort:
                RowAccessor.SetShort(Current.ColumnIndex, ResultSet.GetShort(I));
              stWord:
                RowAccessor.SetWord(Current.ColumnIndex, ResultSet.GetWord(I));
              stSmall:
                RowAccessor.SetShort(Current.ColumnIndex, ResultSet.GetSmall(I));
              stInteger:
                RowAccessor.SetInt(Current.ColumnIndex, ResultSet.GetInt(I));
              stLongWord:
                RowAccessor.SetUInt(Current.ColumnIndex, ResultSet.GetUInt(I));
              stLong:
                RowAccessor.SetLong(Current.ColumnIndex, ResultSet.GetLong(I));
              stULong:
                RowAccessor.SetULong(Current.ColumnIndex, ResultSet.GetULong(I));
              stFloat:
                RowAccessor.SetFloat(Current.ColumnIndex, ResultSet.GetFloat(I));
              stCurrency:
                RowAccessor.SetCurrency(Current.ColumnIndex, ResultSet.GetCurrency(I));
              stDouble:
                RowAccessor.SetDouble(Current.ColumnIndex, ResultSet.GetDouble(I));
              stBigDecimal:
                RowAccessor.SetBigDecimal(Current.ColumnIndex, ResultSet.GetBigDecimal(I));
              stString, stAsciiStream, stUnicodeString, stUnicodeStream:
                if RowAccessor.IsRaw then
                  RowAccessor.SetPAnsiChar(Current.ColumnIndex, ResultSet.GetPAnsiChar(I, Len), @Len)
                else
                  RowAccessor.SetPWideChar(Current.ColumnIndex, ResultSet.GetPWideChar(I, Len), @Len);
              stBytes, stGUID:
                RowAccessor.SetBytes(Current.ColumnIndex, ResultSet.GetBytes(I));
              stDate:
                RowAccessor.SetDate(Current.ColumnIndex, ResultSet.GetDate(I));
              stTime:
                RowAccessor.SetTime(Current.ColumnIndex, ResultSet.GetTime(I));
              stTimestamp:
                RowAccessor.SetTimestamp(Current.ColumnIndex, ResultSet.GetTimestamp(I));
            end;

            if ResultSet.WasNull then
              RowAccessor.SetNull(Current.ColumnIndex);
          except
            { Supress any errors in default fields. }
          end;
        end;
      end;
      ResultSet.Close;
    finally
      Statement.Close;
    end;
  finally
    FreeAndNil(SQLParams);
  end;
end;

{BEGIN of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }
procedure TZGenericCachedResolver.UpdateAutoIncrementFields(
  Sender: IZCachedResultSet; UpdateType: TZRowUpdateType; OldRowAccessor,
  NewRowAccessor: TZRowAccessor; Resolver: IZCachedResolver);
begin
 //Should be implemented at Specific database Level Cached resolver
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{END of PATCH [1185969]: Do tasks after posting updates. ie: Updating AutoInc fields in MySQL }

end.

