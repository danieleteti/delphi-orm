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

unit ZDbcResultSetMetadata;

interface

{$I ZDbc.inc}

uses
  Classes, SysUtils, {$IFNDEF NO_UNIT_CONTNRS}Contnrs, {$ENDIF}
  ZDbcIntfs, ZCollections,
  {$IF defined(OLDFPC) or defined (NO_UNIT_CONTNRS)}ZClasses,{$IFEND}
  ZGenericSqlAnalyser,
  ZTokenizer, ZSelectSchema, ZCompatibility, ZDbcResultSet;

type

  {** Implements a column information structure. }
  TZColumnInfo = class(TObject)
  protected
    FAutoIncrement: Boolean;
    FCaseSensitive: Boolean;
    FSearchable: Boolean;
    FCurrency: Boolean;
    FNullable: TZColumnNullableType;
    FSigned: Boolean;
    FColumnDisplaySize: Integer;
    FCharOctedLength: Integer;
    FColumnLabel: string;
    FColumnName: string;
    FSchemaName: string;
    FPrecision: Integer;
    FScale: Integer;
    FTableName: string;
    FCatalogName: string;
    FColumnType: TZSQLType;
    FReadOnly: Boolean;
    FWritable: Boolean;
    FDefinitelyWritable: Boolean;
    FDefaultValue: string;
    FDefaultExpression : string;
    FColumnCodePage: Word;
  public
    constructor Create;
    function GetColumnTypeName: string;

    property AutoIncrement: Boolean read FAutoIncrement write FAutoIncrement;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property Searchable: Boolean read FSearchable write FSearchable;
    property Currency: Boolean read FCurrency write FCurrency;
    property Nullable: TZColumnNullableType read FNullable write FNullable;

    property Signed: Boolean read FSigned write FSigned;
    property ColumnDisplaySize: Integer read FColumnDisplaySize
      write FColumnDisplaySize;
    property CharOctedLength: Integer read FCharOctedLength
      write FCharOctedLength;
    property ColumnLabel: string read FColumnLabel write FColumnLabel;
    property ColumnName: string read FColumnName write FColumnName;
    property SchemaName: string read FSchemaName write FSchemaName;
    property Precision: Integer read FPrecision write FPrecision;
    property Scale: Integer read FScale write FScale;
    property TableName: string read FTableName write FTableName;
    property CatalogName: string read FCatalogName write FCatalogName;
    property ColumnType: TZSQLType read FColumnType write FColumnType;
    property ReadOnly: Boolean read FReadOnly write FReadOnly;
    property Writable: Boolean read FWritable write FWritable;
    property DefinitelyWritable: Boolean read FDefinitelyWritable
      write FDefinitelyWritable;
    property DefaultValue: string read FDefaultValue write FDefaultValue;
    property DefaultExpression: string read FDefaultExpression write FDefaultExpression;
    property ColumnCodePage: Word read FColumnCodePage write FColumnCodePage;
  end;

  {** Implements Abstract ResultSet Metadata. }
  TZAbstractResultSetMetadata = class(TContainedObject, IZResultSetMetaData)
  private
    FLoaded: Boolean;
    FMetadata: IZDatabaseMetadata;
    FColumnsLabels: TStrings;
    FSQL: string;
    FTableColumns: TZHashMap;
    FIdentifierConvertor: IZIdentifierConvertor;
    FResultSet: TZAbstractResultSet;
    procedure SetMetadata(const Value: IZDatabaseMetadata);
  protected
    FConSettings: PZConSettings;
    procedure SetAutoIncrementFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetCaseSensitiveFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnCodePageFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnNullableFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetDefaultValueFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetDefinitelyWritableFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetIsSearchableFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnPrecisionFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetReadOnlyFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnScaleFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetColumnTypeFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure SetWritableFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet); virtual;
    procedure LoadColumn(ColumnIndex: Integer; ColumnInfo: TZColumnInfo;
      const SelectSchema: IZSelectSchema); virtual;
  protected
    procedure FillColumInfoFromGetColumnsRS({$IFDEF AUTOREFCOUNT}const{$ENDIF}
      ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet; const FieldName: String);
    function GetTableColumns(TableRef: TZTableRef): IZResultSet;
    function ReadColumnByRef(FieldRef: TZFieldRef; ColumnInfo: TZColumnInfo): Boolean;
    function ReadColumnByName(const FieldName: string; TableRef: TZTableRef;
      ColumnInfo: TZColumnInfo): Boolean;
    procedure ClearColumn(ColumnInfo: TZColumnInfo); virtual;
    procedure LoadColumns; virtual;
    procedure ReplaceStarColumns(const SelectSchema: IZSelectSchema);

    property MetaData: IZDatabaseMetadata read FMetadata write SetMetadata;
    property ColumnsLabels: TStrings read FColumnsLabels write FColumnsLabels;
    property SQL: string read FSQL write FSQL;
    property IdentifierConvertor: IZIdentifierConvertor
      read FIdentifierConvertor write FIdentifierConvertor;
    property Loaded: Boolean read FLoaded write FLoaded;
    property ResultSet: TZAbstractResultSet read FResultSet write FResultSet;
  public
    constructor Create(const Metadata: IZDatabaseMetadata; const SQL: string;
      ParentResultSet: TZAbstractResultSet);
    destructor Destroy; override;

    function GetColumnCount: Integer; virtual;
    function IsAutoIncrement(ColumnIndex: Integer): Boolean; virtual;
    function IsCaseSensitive(ColumnIndex: Integer): Boolean; virtual;
    function IsSearchable(ColumnIndex: Integer): Boolean; virtual;
    function IsCurrency(ColumnIndex: Integer): Boolean; virtual;
    function IsNullable(ColumnIndex: Integer): TZColumnNullableType; virtual;

    function IsSigned(ColumnIndex: Integer): Boolean; virtual;
    function GetColumnDisplaySize(ColumnIndex: Integer): Integer; virtual;
    function GetColumnLabel(ColumnIndex: Integer): string; virtual;
    function GetColumnName(ColumnIndex: Integer): string; virtual;
    function GetColumnCodePage(ColumnIndex: Integer): Word;
    function GetSchemaName(ColumnIndex: Integer): string; virtual;
    function GetPrecision(ColumnIndex: Integer): Integer; virtual;
    function GetCharOctedLength(ColumnIndex: Integer): Integer; virtual;
    function GetScale(ColumnIndex: Integer): Integer; virtual;
    function GetTableName(ColumnIndex: Integer): string; virtual;
    function GetCatalogName(ColumnIndex: Integer): string; virtual;
    function GetColumnType(ColumnIndex: Integer): TZSQLType; virtual;
    function GetColumnTypeName(ColumnIndex: Integer): string; virtual;
    function IsReadOnly(ColumnIndex: Integer): Boolean; virtual;
    function IsWritable(ColumnIndex: Integer): Boolean; virtual;
    function IsDefinitelyWritable(ColumnIndex: Integer): Boolean; virtual;
    function GetDefaultValue(ColumnIndex: Integer): string; virtual;
    function HasDefaultValue(ColumnIndex: Integer): Boolean; virtual;
  end;

implementation

uses ZFastCode, ZVariant, ZDbcUtils, ZDbcMetadata, ZSysUtils, ZEncoding;

{ TZColumnInfo }

{**
  Constructs this object and assigns main properties.
}
constructor TZColumnInfo.Create;
begin
  FAutoIncrement := False;
  FCaseSensitive := False;
  FSearchable := False;
  FCurrency := False;
  FNullable := ntNullableUnknown;
  FSigned := False;
  FColumnDisplaySize := 0;
  FColumnLabel := '';
  FColumnName := '';
  FSchemaName := '';
  FPrecision := 0;
  FScale := 0;
  FTableName := '';
  FCatalogName := '';
  FDefaultValue := '';
  FColumnType := stUnknown;
  FReadOnly := True;
  FWritable := False;
  FDefinitelyWritable := False;
  FColumnCodePage := zCP_NONE;
end;

{**
  Retrieves the designated column's database-specific type name.
  @return type name used by the database. If the column type is
    a user-defined type, then a fully-qualified type name is returned.
}
function TZColumnInfo.GetColumnTypeName: string;
begin
  Result := DefineColumnTypeName(FColumnType);
end;

{ TZAbstractResultSetMetadata }

{**
  Constructs this object and assignes the main properties.
  @param Metadata a database metadata object.
  @param SQL an SQL query statement.
  @param ColumnsInfo a collection of columns info.
}
constructor TZAbstractResultSetMetadata.Create(const Metadata: IZDatabaseMetadata;
  const SQL: string; ParentResultSet: TZAbstractResultSet);
begin
  inherited Create(ParentResultSet);

  SetMetadata(Metadata);
  FSQL := SQL;
  FLoaded := not ((FMetadata <> nil) and FMetadata.GetConnection.UseMetadata);
  FTableColumns := TZHashMap.Create;
  FResultSet := ParentResultSet;

  FConSettings := FResultSet.GetConSettings;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractResultSetMetadata.Destroy;
begin
  FIdentifierConvertor := nil;
  FMetadata := nil;
  FreeAndNil(FTableColumns);
  FreeAndNil(FColumnsLabels);
  inherited Destroy;
end;

procedure TZAbstractResultSetMetadata.FillColumInfoFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo;
  const TableColumns: IZResultSet; const FieldName: String);
begin
  ColumnInfo.CatalogName := TableColumns.GetString(CatalogNameIndex);
  ColumnInfo.SchemaName := TableColumns.GetString(SchemaNameIndex);
  ColumnInfo.TableName := TableColumns.GetString(TableNameIndex);
  ColumnInfo.ColumnName := TableColumns.GetString(ColumnNameIndex);;

  if FConSettings = nil then //fix if on creation nil was assigned
    FConSettings := ResultSet.GetStatement.GetConnection.GetConSettings;
  { Overwrite ColumnTypes if necessary }
  SetColumnTypeFromGetColumnsRS(ColumnInfo, TableColumns);
  {Assign ColumnCodePages if necessary }
  SetColumnCodePageFromGetColumnsRS(ColumnInfo, TableColumns);
  {Precision or column-size}
  SetColumnPrecisionFromGetColumnsRS(ColumnInfo, TableColumns);
  {Scale}
  SetColumnScaleFromGetColumnsRS(ColumnInfo, TableColumns);
  {nullable}
  SetColumnNullableFromGetColumnsRS(ColumnInfo, TableColumns);
  {auto increment field}
  SetAutoIncrementFromGetColumnsRS(ColumnInfo, TableColumns);
  {Case sensitive}
  SetCaseSensitiveFromGetColumnsRS(ColumnInfo, TableColumns);
  {Is searchable}
  SetIsSearchableFromGetColumnsRS(ColumnInfo, TableColumns);
  {Writable}
  SetWritableFromGetColumnsRS(ColumnInfo, TableColumns);
  {DefinitelyWritable}
  SetDefinitelyWritableFromGetColumnsRS(ColumnInfo, TableColumns);
  {readonly}
  SetReadOnlyFromGetColumnsRS(ColumnInfo, TableColumns);
  {default value}
  SetDefaultValueFromGetColumnsRS(ColumnInfo, TableColumns);
end;

{**
  Returns the number of columns in this <code>ResultSet</code> object.
  @return the number of columns
}
function TZAbstractResultSetMetadata.GetColumnCount: Integer;
begin
  Result := FResultSet.ColumnsInfo.Count;
end;

{**
  Indicates whether the designated column is automatically numbered, thus read-only.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsAutoIncrement(ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).AutoIncrement;
end;

{**
  Indicates whether a column's case matters.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsCaseSensitive(ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).CaseSensitive;
end;

{**
  Indicates whether the designated column can be used in a where clause.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsSearchable(ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Searchable;
end;

{**
  Indicates whether the designated column is a cash value.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsCurrency(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Currency;
end;

{**
  Indicates the nullability of values in the designated column.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the nullability status of the given column; one of <code>columnNoNulls</code>,
    <code>columnNullable</code> or <code>columnNullableUnknown</code>
}
function TZAbstractResultSetMetadata.IsNullable(
  ColumnIndex: Integer): TZColumnNullableType;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Nullable;
end;

{**
  Indicates whether values in the designated column are signed numbers.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsSigned(ColumnIndex: Integer): Boolean;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Signed;
end;

{**
  Indicates the designated column's normal maximum width in characters.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the normal maximum number of characters allowed as the width
    of the designated column
}
function TZAbstractResultSetMetadata.GetColumnDisplaySize(
  ColumnIndex: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnDisplaySize;
end;

{**
  Gets the designated column's suggested title for use in printouts and
  displays.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return the suggested column title
}
function TZAbstractResultSetMetadata.GetColumnLabel(ColumnIndex: Integer): string;
var
  I, J, N: Integer;
  ColumnName, OrgLabel: string;
  ColumnsInfo: TObjectList;
  B: Boolean;
begin
  { Prepare unique column labels. }
  if FColumnsLabels = nil then
  begin
    ColumnsInfo := FResultSet.ColumnsInfo;
    FColumnsLabels := TStringList.Create;
    for I := 0 to ColumnsInfo.Count - 1 do begin
      N := 0;
      ColumnName := TZColumnInfo(ColumnsInfo[I]).ColumnLabel;
      if ColumnName = '' then
        ColumnName := 'Column';
      OrgLabel := ColumnName;
      Repeat
        //see test TestDuplicateColumnNames or
        //https://zeoslib.sourceforge.io/viewtopic.php?f=50&t=120692
        b := False;
        for J := 0 to I - 1 do
          if TZColumnInfo(ColumnsInfo[J]).ColumnLabel = ColumnName then
            Begin
             Inc(N);
             b := True;
            End;
        if N > 0 then
          ColumnName := OrgLabel + '_' + ZFastCode.IntToStr(N);
      Until Not b;
      FColumnsLabels.Add(ColumnName);
    end;
  end;

  Result := ColumnsLabels[ColumnIndex{$IFNDEF GENERIC_INDEX} - 1{$ENDIF}];
end;

{**
  Get the designated column's name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name
}
function TZAbstractResultSetMetadata.GetColumnName(
  ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnName;
end;

{**
  Get the designated column's codepage.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetColumnCodePage(ColumnIndex: Integer): Word;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnCodePage;
end;

{**
  Get the designated column's table's schema.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return schema name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetSchemaName(
  ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).SchemaName;
end;

{**
  Get the designated column's number of decimal digits.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return precision
}
function TZAbstractResultSetMetadata.GetPrecision(ColumnIndex: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Precision;
end;

{**
  Gets the designated column's number of digits to right of the decimal point.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return scale
}
function TZAbstractResultSetMetadata.GetScale(ColumnIndex: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Scale;
end;

{**
  Gets the designated column's table name.
  @param ColumnIndex the first ColumnIndex is 1, the second is 2, ...
  @return table name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetTableName(ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).TableName;
end;

{**
  Gets the designated column's table's catalog name.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return column name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetCatalogName(ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).CatalogName;
end;

{**
  Gets the designated column's table's character octed length. This is
  count of bytes for a buffer to store the data. This may depend to DB's
  character set or true UFT16 vs Raw encoded strings
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return char octed length name or "" if not applicable
}
function TZAbstractResultSetMetadata.GetCharOctedLength(ColumnIndex: Integer): Integer;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).CharOctedLength;
end;

{**
  Retrieves the designated column's SQL type.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return SQL type from java.sql.Types
}
function TZAbstractResultSetMetadata.GetColumnType(ColumnIndex: Integer): TZSQLType;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex{$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ColumnType;
end;

{**
  Retrieves the designated column's database-specific type name.

  @param ColumnIndex the first column is 1, the second is 2, ...
  @return type name used by the database. If the column type is
    a user-defined type, then a fully-qualified type name is returned.
}
function TZAbstractResultSetMetadata.GetColumnTypeName(ColumnIndex: Integer): string;
begin
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).GetColumnTypeName;
end;

{**
  Indicates whether the designated column is definitely not writable.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsReadOnly(ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).ReadOnly;
end;

{**
  Indicates whether it is possible for a write on the designated column to succeed.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsWritable(ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).Writable;
end;

{**
  Indicates whether a write on the designated column will definitely succeed.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAbstractResultSetMetadata.IsDefinitelyWritable(
  ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).DefinitelyWritable;
end;

{**
  Gets a default value for this field.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return a default value for this field.
}
function TZAbstractResultSetMetadata.GetDefaultValue(
  ColumnIndex: Integer): string;
begin
  if not Loaded then
     LoadColumns;
  Result := TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).DefaultValue;
end;

{**
  Finds whether this field has a default value.
  @param ColumnIndex the first column is 1, the second is 2, ...
  @return true if this field has a default value.
}
function TZAbstractResultSetMetadata.HasDefaultValue(
  ColumnIndex: Integer): Boolean;
begin
  if not Loaded then
     LoadColumns;
  // '' = NULL / no default value, '''''' = empty string (''), etc.
  Result := not(TZColumnInfo(FResultSet.ColumnsInfo[ColumnIndex {$IFNDEF GENERIC_INDEX}-1{$ENDIF}]).DefaultValue = '');
end;

{**
  Gets a table description result set.
  @param TableRef a table reference object.
  @return a result set with table columns from database metadata.
}
function TZAbstractResultSetMetadata.GetTableColumns(
  TableRef: TZTableRef): IZResultSet;
var
  TableKey: IZAnyValue;
begin
  TableKey := TZAnyValue.CreateWithString(TableRef.FullName);
  if FTableColumns.Get(TableKey) = nil then
  begin
    Result := Metadata.GetColumns(TableRef.Catalog,
      TableRef.Schema, FResultSet.GetStatement.GetConnection.GetMetadata.AddEscapeCharToWildcards(TableRef.Table), '');
    FTableColumns.Put(TableKey, Result);
  end else
    Result := FTableColumns.Get(TableKey) as IZResultSet;
end;

{**
  Clears specified column information.
  @param ColumnInfo a column information object.
}
procedure TZAbstractResultSetMetadata.ClearColumn(ColumnInfo: TZColumnInfo);
begin
  ColumnInfo.ReadOnly := True;
  ColumnInfo.Writable := False;
  ColumnInfo.DefinitelyWritable := False;
  ColumnInfo.CatalogName := '';
  ColumnInfo.SchemaName := '';
  ColumnInfo.TableName := '';
  ColumnInfo.ColumnName := '';
end;

{**
  Reads a column information from table metadata.
  @param FieldName a name of the field.
  @param TableRef a table reference object.
  @param ColumnInfo a column information object.
  @return <code>True</code> is column was found and read.
}
function TZAbstractResultSetMetadata.ReadColumnByName(const FieldName: string;
  TableRef: TZTableRef; ColumnInfo: TZColumnInfo): Boolean;
var
  TableColumns: IZResultSet;
  Catalog, UpCatalog, Schema, UpSchema, UpField: String;
begin
  Result := False;
  if (FieldName = '') then
    Exit;
  TableColumns := GetTableColumns(TableRef);
  { Checks for unexisted table. }
  if not Assigned(TableColumns) then
    Exit;
  Catalog := TableRef.Catalog;
  Schema  := TableRef.Schema;
  with FMetadata.GetDatabaseInfo do
    if SupportsCatalogsInDataManipulation and (TableRef.Catalog = '') then
      Catalog := FMetadata.GetConnection.GetCatalog
    else if SupportsSchemasInDataManipulation and (TableRef.Schema = '') and (TableRef.Catalog = '') then
      Schema := FMetadata.GetConnection.GetCatalog;

  { Locates a column row casesensitive. }
  TableColumns.BeforeFirst;
  while TableColumns.Next do
    if TableColumns.GetString(ColumnNameIndex) = FieldName then begin
      if (Catalog = '') or (TableColumns.GetString(CatalogNameIndex) = Catalog) then
        if (Schema = '') or (TableColumns.GetString(SchemaNameIndex) = Schema) then
          Break;
    end;
  if TableColumns.IsAfterLast then begin
    UpField := AnsiUpperCase(FieldName);
    UpCatalog := AnsiUpperCase(Catalog);
    UpSchema := AnsiUpperCase(Schema);
    { Locates a column row with case insensitivity. }
    TableColumns.BeforeFirst;
    while TableColumns.Next do
      if AnsiUpperCase(TableColumns.GetString(ColumnNameIndex)) = UpField then begin
        if (Catalog = '') or (TableColumns.GetString(CatalogNameIndex) = Catalog) or
           (AnsiUpperCase(TableColumns.GetString(CatalogNameIndex)) = UpCatalog) then
          if (Schema = '') or (TableColumns.GetString(SchemaNameIndex) = Schema) or
             (AnsiUpperCase(TableColumns.GetString(SchemaNameIndex)) = UpSchema) then
            Break;
    end;
    if TableColumns.IsAfterLast then
      Exit;
  end;

  { Reads a column information. }
  Result := True;
  FillColumInfoFromGetColumnsRS(ColumnInfo, TableColumns, FieldName);
end;

{**
  Reads a column information from table metadata.
  @param FieldRef a field reference object.
  @param ColumnInfo a column information object.
  @return <code>True</code> if column was found and read.
}
function TZAbstractResultSetMetadata.ReadColumnByRef(
  FieldRef: TZFieldRef; ColumnInfo: TZColumnInfo): Boolean;
begin
  Result := False;
  ClearColumn(ColumnInfo);
  { Checks for uncompleted field reference. }
  if not Assigned(FieldRef) or not Assigned(FieldRef.TableRef) then
    Exit;
  if not FieldRef.IsField then
    Exit;

  Result := ReadColumnByName(FieldRef.Field, FieldRef.TableRef, ColumnInfo);
end;

{**
  Initializes on single column of the result set.
  @param ColumnIndex a column index in the query.
  @param ColumnInfo a column information object to be initialized.
  @param SelectSchema a schema of the select statement.
}
procedure TZAbstractResultSetMetadata.LoadColumn(ColumnIndex: Integer;
  ColumnInfo: TZColumnInfo; const SelectSchema: IZSelectSchema);
var
  I: Integer;
  FieldRef: TZFieldRef;
  TableRef: TZTableRef;
  Found: Boolean;
  AName: String;
begin
  { Initializes single columns with specified table. }
  FieldRef := SelectSchema.LinkFieldByIndexAndShortName(ColumnIndex,
    ColumnInfo.ColumnLabel, IdentifierConvertor);
  if ReadColumnByRef(FieldRef, ColumnInfo) then //else double processing down below
    Exit;
 //EH commented: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=71516&start=15
 // if ColumnInfo.ColumnName <> '' then
   // Exit;
  if Assigned(FieldRef) and not FieldRef.IsField then
    Exit;
  { Initializes single columns without specified table. }
  I := 0;
  Found := False;
  while {(ColumnInfo.ColumnName = '') and }(I < SelectSchema.TableCount) and not Found do begin
    TableRef := SelectSchema.Tables[I];
    if Assigned(FieldRef)
    then AName := IdentifierConvertor.ExtractQuote(FieldRef.Field)
    else AName := IdentifierConvertor.ExtractQuote(ColumnInfo.ColumnLabel);
    Found := ReadColumnByName(AName, TableRef, ColumnInfo);
    Inc(I);
  end;
end;

{**
  Replaces '*' columns in the select schema.
  @param SelectSchema a query select schema.
}
procedure TZAbstractResultSetMetadata.ReplaceStarColumns(
  const SelectSchema: IZSelectSchema);
var
  I: Integer;
  Current: TZFieldRef;
  FieldRef: TZFieldRef;
  TableRef: TZTableRef;
  ResultSet: IZResultSet;
begin
  I := 0;
  while I < SelectSchema.FieldCount do begin
    Current := SelectSchema.Fields[I];
    if (Current.Field = '*') and (Current.TableRef <> nil) then begin
      TableRef := Current.TableRef;
      ResultSet := GetTableColumns(TableRef);
      if ResultSet <> nil then begin
        ResultSet.BeforeFirst;
        while ResultSet.Next do begin
          FieldRef := TZFieldRef.Create(True, TableRef.Catalog, TableRef.Schema,
            TableRef.Table, ResultSet.GetString(ColumnNameIndex), '', TableRef);
          SelectSchema.InsertField(I, FieldRef);
          Inc(I);
        end;
      end;
      SelectSchema.DeleteField(Current);
      Dec(I);
    end;
    Inc(I);
  end;
end;

procedure TZAbstractResultSetMetadata.SetAutoIncrementFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnAutoIncIndex) then
    ColumnInfo.AutoIncrement := TableColumns.GetBoolean(TableColColumnAutoIncIndex);
end;

procedure TZAbstractResultSetMetadata.SetCaseSensitiveFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnCaseSensitiveIndex) then
    ColumnInfo.CaseSensitive := TableColumns.GetBoolean(TableColColumnCaseSensitiveIndex);
end;

procedure TZAbstractResultSetMetadata.SetColumnCodePageFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  if ColumnInfo.ColumnType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream] then begin
    if FConSettings^.ClientCodePage^.IsStringFieldCPConsistent then
      if FConSettings^.ClientCodePage.Encoding = ceUTF16
      then ColumnInfo.ColumnCodePage := zCP_UTF16
      else ColumnInfo.ColumnCodePage := FConSettings^.ClientCodePage^.CP
    {else keep Resultset info}
  end else
    ColumnInfo.ColumnCodePage := zCP_NONE; //not a character column
end;

procedure TZAbstractResultSetMetadata.SetColumnNullableFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnNullableIndex) then
    ColumnInfo.Nullable := TZColumnNullableType(TableColumns.GetInt(TableColColumnNullableIndex));
end;

procedure TZAbstractResultSetMetadata.SetColumnPrecisionFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  //it's a nop -> use a Override if necessary
end;

procedure TZAbstractResultSetMetadata.SetColumnScaleFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF} ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  //it's a nop -> use a Override if necessary
end;

procedure TZAbstractResultSetMetadata.SetColumnTypeFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo;
  const TableColumns: IZResultSet);
var tempColType: TZSQLType;
begin
//If the returned column information is null or is unknown then the value assigned during
//the resultset.open will be kept
  if not TableColumns.IsNull(TableColColumnTypeIndex) and (TableColumns.GetSmall(TableColColumnTypeIndex) > Ord(stUnknown)) then begin
    //since Pointer referencing by RowAccessor we've a pointer and GetBlob
    //raises an exception if the pointer is a reference to PPAnsiChar or
    //ZPPWideChar. if we execute a cast of a lob field the database meta-informtions
    //assume a IZLob-Pointer. So let's prevent this case and check for
    //stByte, stString, stUnicoeString first. If this type is returned from the
    //ResultSet-Metadata we do NOT overwrite the column-type
    //f.e. select cast( name as varchar(100)), cast(setting as varchar(100)) from pg_settings

    //or the same vice versa:
    //(CASE WHEN (Ticket51_B."Name" IS NOT NULL) THEN Ticket51_B."Name" ELSE 'Empty' END) As "Name"
    //we've NO fixed length for a case(postgres and FB2.5up f.e.) select
    tempColType := TZSQLType(TableColumns.GetSmall(TableColColumnTypeIndex));
    if not (tempColType in [stBinaryStream, stAsciiStream,
        stUnicodeStream, stBytes, stString, stUnicodeString]) or (ColumnInfo.ColumnType = stUnknown) then
      ColumnInfo.ColumnType := tempColType;
  end;
end;

procedure TZAbstractResultSetMetadata.SetDefaultValueFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnColDefIndex) then
    ColumnInfo.DefaultValue := TableColumns.GetString(TableColColumnColDefIndex);
end;

procedure TZAbstractResultSetMetadata.SetDefinitelyWritableFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnDefinitelyWritableIndex) then
    if ColumnInfo.AutoIncrement and Assigned(FMetadata) then {improve ADO where the metainformations do not bring autoincremental fields through}
      if FMetadata.GetDatabaseInfo.SupportsUpdateAutoIncrementFields then
        ColumnInfo.DefinitelyWritable := TableColumns.GetBoolean(TableColColumnDefinitelyWritableIndex)
      else
        ColumnInfo.DefinitelyWritable := False
    else
      ColumnInfo.DefinitelyWritable := TableColumns.GetBoolean(TableColColumnDefinitelyWritableIndex);
end;

procedure TZAbstractResultSetMetadata.SetIsSearchableFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnSearchableIndex) then
    ColumnInfo.Searchable := TableColumns.GetBoolean(TableColColumnSearchableIndex);
end;

procedure TZAbstractResultSetMetadata.SetMetadata(
  const Value: IZDatabaseMetadata);
begin
  FMetadata := Value;
  if Value<>nil then
    FIdentifierConvertor := Value.GetIdentifierConvertor
  else
    FIdentifierConvertor := TZDefaultIdentifierConvertor.Create(FMetadata);
end;

procedure TZAbstractResultSetMetadata.SetReadOnlyFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnReadonlyIndex) then
    if ColumnInfo.AutoIncrement and Assigned(FMetadata) then {improve ADO where the metainformations do not bring autoincremental fields through}
      if FMetadata.GetDatabaseInfo.SupportsUpdateAutoIncrementFields then
        ColumnInfo.ReadOnly := TableColumns.GetBoolean(TableColColumnReadonlyIndex)
      else
        ColumnInfo.ReadOnly := True
    else
      ColumnInfo.ReadOnly := TableColumns.GetBoolean(TableColColumnReadonlyIndex);
end;

procedure TZAbstractResultSetMetadata.SetWritableFromGetColumnsRS(
  {$IFDEF AUTOREFCOUNT}const{$ENDIF}ColumnInfo: TZColumnInfo; const TableColumns: IZResultSet);
begin
  { Zeos all time code.. Is this correct? if not use a override and fix it}
  if not TableColumns.IsNull(TableColColumnWritableIndex) then
    if ColumnInfo.AutoIncrement and Assigned(FMetadata) then {improve ADO where the metainformations do not bring autoincremental fields through}
      if FMetadata.GetDatabaseInfo.SupportsUpdateAutoIncrementFields
      then ColumnInfo.Writable := TableColumns.GetBoolean(TableColColumnWritableIndex)
      else ColumnInfo.Writable := False
    else ColumnInfo.Writable := TableColumns.GetBoolean(TableColColumnWritableIndex);
end;

{**
  Initializes columns with additional data.
}
procedure TZAbstractResultSetMetadata.LoadColumns;
var
  I: Integer;
  Driver: IZDriver;
  Tokenizer: IZTokenizer;
  StatementAnalyser: IZStatementAnalyser;
  SelectSchema: IZSelectSchema;
  FillByIndices: Boolean;
begin
  { Parses the Select statement and retrieves a schema object. }
  Driver := Metadata.GetConnection.GetDriver;
  Tokenizer := Driver.GetTokenizer;
  StatementAnalyser := Driver.GetStatementAnalyser;
  SelectSchema := StatementAnalyser.DefineSelectSchemaFromQuery(Tokenizer, SQL);
  if Assigned(SelectSchema) then
  begin
    SelectSchema.LinkReferences(IdentifierConvertor);
    ReplaceStarColumns(SelectSchema);
    FillByIndices := SelectSchema.FieldCount = FResultSet.ColumnsInfo.Count;
    for I := 0 to FResultSet.ColumnsInfo.Count - 1 do
    begin
      if FillByIndices then
        LoadColumn(I {$IFNDEF GENERIC_INDEX}+1{$ENDIF}, TZColumnInfo(FResultSet.ColumnsInfo[I]), SelectSchema)
      else
        LoadColumn(-1, TZColumnInfo(FResultSet.ColumnsInfo[I]), SelectSchema);
    end;
  end;
  Loaded := True;
end;

end.

