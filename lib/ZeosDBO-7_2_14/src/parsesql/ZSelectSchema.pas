{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        SQL Select Objects and Assembler classes         }
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

unit ZSelectSchema;

interface

{$I ZParseSql.inc}

uses ZClasses
  {$IFNDEF NO_UNIT_CONTNRS},Contnrs{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_CLASSES}, Classes{$ENDIF}
  {$IFDEF WITH_TOBJECTLIST_REQUIRES_SYSTEM_TYPES}, System.Types{$ENDIF};

type

  {** Implements an enum for and identifier case Sensitive/Unsensitive value }
  TZIdentifierCase = (icNone, icLower, icUpper, icMixed, icSpecial);

  {** Case Sensitive/Unsensitive identificator processor. }
  IZIdentifierConvertor = interface (IZInterface)
    ['{2EB07B9B-1E96-4A42-8084-6F98D9140B27}']
    function IsCaseSensitive(const Value: string): Boolean;
    function IsQuoted(const Value: string): Boolean;
    function GetIdentifierCase(const Value: String; TestKeyWords: Boolean): TZIdentifierCase;
    function Quote(const Value: string): string;
    function ExtractQuote(const Value: string): string;
  end;

  {** Implements a table reference assembly. }
  TZTableRef = class (TObject)
  private
    FCatalog: string;
    FSchema: string;
    FTable: string;
    FAlias: string;
  public
    constructor Create(const Catalog, Schema, Table, Alias: string);
    function FullName: string;

    property Catalog: string read FCatalog write FCatalog;
    property Schema: string read FSchema write FSchema;
    property Table: string read FTable write FTable;
    property Alias: string read FAlias write FAlias;
  end;

  {** Implements a field reference assembly. }
  TZFieldRef = class (TObject)
  private
    FIsField: Boolean;
    FCatalog: string;
    FSchema: string;
    FTable: string;
    FField: string;
    FAlias: string;
    FTableRef: TZTableRef;
    FLinked: Boolean;
  public
    constructor Create(IsField: Boolean; const Catalog, Schema, Table,
      Field, Alias: string; TableRef: TZTableRef);

    property IsField: Boolean read FIsField write FIsField;
    property Catalog: string read FCatalog write FCatalog;
    property Schema: string read FSchema write FSchema;
    property Table: string read FTable write FTable;
    property Field: string read FField write FField;
    property Alias: string read FAlias write FAlias;
    property TableRef: TZTableRef read FTableRef write FTableRef;
    property Linked: Boolean read FLinked write FLinked;
  end;

  {** Defines an interface to select assembly. }
  IZSelectSchema = interface (IZInterface)
    ['{3B892975-57E9-4EB7-8DB1-BDDED91E7FBC}']

    procedure AddField({$IFDEF AUTOREFCOUNT}const{$ENDIF}FieldRef: TZFieldRef);
    procedure InsertField(Index: Integer; {$IFDEF AUTOREFCOUNT}const{$ENDIF}FieldRef: TZFieldRef);
    procedure DeleteField({$IFDEF AUTOREFCOUNT}const{$ENDIF}FieldRef: TZFieldRef);

    procedure AddTable({$IFDEF AUTOREFCOUNT}const{$ENDIF}TableRef: TZTableRef);

    procedure LinkReferences(const Convertor: IZIdentifierConvertor);

    function FindTableByFullName(const Catalog, Schema, Table: string): TZTableRef;
    function FindTableByShortName(const Table: string): TZTableRef;
    function FindFieldByShortName(const Field: string): TZFieldRef;

    function LinkFieldByIndexAndShortName(ColumnIndex: Integer; const Field: string;
      const Convertor: IZIdentifierConvertor): TZFieldRef;

    function GetFieldCount: Integer;
    function GetTableCount: Integer;
    function GetField(Index: Integer): TZFieldRef;
    function GetTable(Index: Integer): TZTableRef;

    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TZFieldRef read GetField;
    property TableCount: Integer read GetTableCount;
    property Tables[Index: Integer]: TZTableRef read GetTable;
  end;

  {** Implements a select assembly. }
  TZSelectSchema = class (TZAbstractObject, IZSelectSchema)
  private
    FFields: TObjectList;
    FTables: TObjectList;

    procedure ConvertIdentifiers(const Convertor: IZIdentifierConvertor);
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddField({$IFDEF AUTOREFCOUNT}const{$ENDIF}FieldRef: TZFieldRef);
    procedure InsertField(Index: Integer; {$IFDEF AUTOREFCOUNT}const{$ENDIF}FieldRef: TZFieldRef);
    procedure DeleteField({$IFDEF AUTOREFCOUNT}const{$ENDIF}FieldRef: TZFieldRef);

    procedure AddTable({$IFDEF AUTOREFCOUNT}const{$ENDIF}TableRef: TZTableRef);

    procedure LinkReferences(const Convertor: IZIdentifierConvertor);

    function FindTableByFullName(const Catalog, Schema, Table: string): TZTableRef;
    function FindTableByShortName(const Table: string): TZTableRef;
    function FindFieldByShortName(const Field: string): TZFieldRef;

    function LinkFieldByIndexAndShortName(ColumnIndex: Integer; const Field: string;
      const Convertor: IZIdentifierConvertor): TZFieldRef;

    function GetFieldCount: Integer;
    function GetTableCount: Integer;
    function GetField(Index: Integer): TZFieldRef;
    function GetTable(Index: Integer): TZTableRef;

    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TZFieldRef read GetField;
    property TableCount: Integer read GetTableCount;
    property Tables[Index: Integer]: TZTableRef read GetTable;
  end;

implementation

uses SysUtils;

{ TZTableRef }

{**
  Creates a table reference object.
  @param Catalog a catalog name.
  @param Schema a schema name.
  @param Table a table name.
  @param Alias a table alias.
}
constructor TZTableRef.Create(const Catalog, Schema, Table, Alias: string);
begin
  FCatalog := Catalog;
  FSchema := Schema;
  FTable := Table;
  FAlias := Alias;
end;

{**
  Gets a full database table name.
  @return a full database table name.
}
function TZTableRef.FullName: string;
var P: PChar absolute Result;
  I: Integer;
begin
  Result := FCatalog + '.' + FSchema + '.' + FTable;
  if P^ = '.' then begin
    if (P+1)^ = '.'
    then I := 2
    else I := 1;
    Result := Copy(Result, I+1, Length(Result)-I);
  end;
end;

{ TZFieldRef }

{**
  Creates a field reference object.
  @param IsField flag which separates table columns from expressions.
  @param Catalog a catalog name.
  @param Schema a schema name.
  @param Table a table name.
  @param Field a field name.
  @param Alias a field alias.
}
constructor TZFieldRef.Create(IsField: Boolean; const Catalog, Schema, Table,
  Field, Alias: string; TableRef: TZTableRef);
begin
  FIsField := IsField;
  FCatalog := Catalog;
  FSchema := Schema;
  FTable := Table;
  FField := Field;
  FAlias := Alias;
  FTableRef := TableRef;
  //EH: Dev-Note the Linked attribute is a tag if a column was found infieldlist!
  //FLinked := False;
end;

{ TZSelectSchema }

{**
  Constructs this assembly object and assignes the main properties.
}
constructor TZSelectSchema.Create;
begin
  FFields := TObjectList.Create;
  FTables := TObjectList.Create;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZSelectSchema.Destroy;
begin
  FreeAndNil(FFields);
  FreeAndNil(FTables);
end;

{**
  Finds a table reference by catalog and table name.
  @param Catalog a database catalog name.
  @param Schema a database schema name.
  @param Table a database table name.
  @return a found table reference object or <code>null</code> otherwise.
}
function TZSelectSchema.FindTableByFullName(
  const Catalog, Schema, Table: string): TZTableRef;
var
  I: Integer;
  Current: TZTableRef;
begin
  Result := nil;

  { Looks a table by it's full name. }
  for I := 0 to FTables.Count - 1 do begin
    Current := TZTableRef(FTables[I]);
    if (Current.Catalog = Catalog) and (Current.Schema = Schema) and (Current.Table = Table) then begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a table by it's schema and table  name. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := TZTableRef(FTables[I]);
    if (Current.Schema = Schema) and (Current.Table = Table) then
    begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a table by it's short name. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := TZTableRef(FTables[I]);
    if (Current.Schema = '') and (Current.Table = Table) then
    begin
      Result := Current;
      Exit;
    end;
  end;
end;

{**
  Finds a table reference by table name or table alias.
  @param Table a database table name or alias.
  @return a found table reference object or <code>null</code> otherwise.
}
function TZSelectSchema.FindTableByShortName(const Table: string): TZTableRef;
var
  I: Integer;
  Current: TZTableRef;
begin
  Result := nil;

  { Looks a table by it's alias. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := TZTableRef(FTables[I]);
    if Current.Alias = Table then
    begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a table by it's name. }
  for I := 0 to FTables.Count - 1 do
  begin
    Current := TZTableRef(FTables[I]);
    if Current.Table = Table then
    begin
      Result := Current;
      Exit;
    end;
  end;
end;

{**
  Finds a field reference by field name or field alias.
  @param Field a table field name or alias.
  @return a found field reference object or <code>null</code> otherwise.
}
function TZSelectSchema.FindFieldByShortName(const Field: string): TZFieldRef;
var
  I: Integer;
  Current: TZFieldRef;
begin
  Result := nil;
  if Field = '' then
    Exit;

  { Looks a field by it's alias. }
  for I := 0 to FFields.Count - 1 do
  begin
    Current := TZFieldRef(FFields[I]);
    if Current.Alias = Field then
    begin
      Result := Current;
      Exit;
    end;
  end;

  { Looks a field by it's name. }
  for I := 0 to FFields.Count - 1 do
  begin
    Current := TZFieldRef(FFields[I]);
    if Current.Field = Field then
    begin
      Result := Current;
      Exit;
    end;
  end;
end;

{**
  Links a field reference by index and/or field name or field alias.
  @param ColumnIndex an index of the column.
  @param Field a table field name or alias.
  @return a found field reference object or <code>null</code> otherwise.
}
function TZSelectSchema.LinkFieldByIndexAndShortName(ColumnIndex: Integer;
  const Field: string; const Convertor: IZIdentifierConvertor): TZFieldRef;
var
  I: Integer;
  Current: TZFieldRef;
  FieldQuoted, FieldUnquoted: string;
begin
  Result := nil;
  if Field = '' then
    Exit;

  FieldQuoted := Convertor.Quote(Field);
  FieldUnquoted := Convertor.ExtractQuote(Field);

  {$IFNDEF GENERIC_INDEX}
  ColumnIndex := ColumnIndex -1;
  {$ENDIF}

  { Looks by field index. }
  if (ColumnIndex >= 0) and (ColumnIndex <= FFields.Count - 1) then begin
    Current := TZFieldRef(FFields[ColumnIndex]);
    if ((Current.Alias = Field) or (Current.Field = Field) or (Current.Field = FieldQuoted) or (Current.Alias = FieldUnquoted)) then begin
      Result := Current;
      Result.Linked := True;
      Exit;
    end;
  end;

  { Looks a field by it's alias. }
  for I := 0 to FFields.Count - 1 do begin
    Current := TZFieldRef(FFields[I]);
    if not Current.Linked and (Current.Alias <> '') and
       ((Current.Alias = Field) or (Current.Alias = FieldQuoted) or (Current.Alias = FieldUnquoted)) then begin
      Result := Current;
      Result.Linked := True;
      Exit;
    end;
  end;

  { Looks a field by field and table aliases. }
  for I := 0 to FFields.Count - 1 do begin
    Current := TZFieldRef(FFields[I]);
    if not Current.Linked and Assigned(Current.TableRef) and
       (((Current.TableRef.Alias + '.' + Current.Field) = Field) or
        (((Current.TableRef.Table + '.' + Current.Field) = Field))) then begin
      Result := Current;
      Result.Linked := True;
      Exit;
    end;
  end;

  { Looks a field by it's name. }
  for I := 0 to FFields.Count - 1 do begin
    Current := TZFieldRef(FFields[I]);
    if not Current.Linked and (Current.Field <> '') and
       ((Current.Field = Field) or (Current.Field = FieldQuoted) or (Current.Field = FieldUnquoted)) then begin
      Result := Current;
      Result.Linked := True;
      Exit;
    end;
  end;
end;

{**
  Convert all table and field identifiers..
  @param Convertor an identifier convertor.
}
procedure TZSelectSchema.ConvertIdentifiers(const Convertor: IZIdentifierConvertor);
var
  I: Integer;
  function ExtractNeedlessQuote(const Value : String) : String;
  begin
    if Value = '' then
    begin
      Result := '';
      Exit;
    end;
    Result := Convertor.ExtractQuote(Value);
    if Convertor.GetIdentifierCase(Result, True) in [icMixed, icSpecial] then
      Result := Value;
  end;
begin
  if Convertor = nil then Exit;

  for I := 0 to FFields.Count - 1 do
  begin
    with TZFieldRef(FFields[I]) do
    begin
      Catalog := ExtractNeedlessQuote(Catalog);
      Schema := ExtractNeedlessQuote(Schema);
      Table := ExtractNeedlessQuote(Table);
      Field := ExtractNeedlessQuote(Field);
      Alias := ExtractNeedlessQuote(Alias);
    end;
  end;

  for I := 0 to FTables.Count - 1 do
  begin
    with TZTableRef(FTables[I]) do
    begin
      Catalog := ExtractNeedlessQuote(Catalog);
      Schema := ExtractNeedlessQuote(Schema);
      Table := ExtractNeedlessQuote(Table);
      Alias := ExtractNeedlessQuote(Alias);
    end;
  end;
end;

{**
  Links references between fields and tables.
  @param Convertor an identifier convertor.
}
procedure TZSelectSchema.LinkReferences(const Convertor: IZIdentifierConvertor);
var
  I, J: Integer;
  FieldRef: TZFieldRef;
  TableRef: TZTableRef;
  TempFields: TObjectList;
begin
  ConvertIdentifiers(Convertor);
  TempFields := FFields;
  FFields := TObjectList.Create;

  try
    for I := 0 to TempFields.Count - 1 do begin
      FieldRef := TZFieldRef(TempFields[I]);
      TableRef := nil;

      if not FieldRef.IsField then begin
        FFields.Add(TZFieldRef.Create(FieldRef.IsField, FieldRef.Catalog,
          FieldRef.Schema, FieldRef.Table, FieldRef.Field, FieldRef.Alias,
          FieldRef.TableRef));
        Continue;
      end else if (FieldRef.Schema <> '') and (FieldRef.Table <> '') then
        TableRef := FindTableByFullName(FieldRef.Catalog, FieldRef.Schema,
          FieldRef.Table)
      else if FieldRef.Table <> '' then
        TableRef := FindTableByShortName(FieldRef.Table)
      else if FieldRef.Field = '*' then begin
        { Add all fields from all tables. }
        for J := 0 to FTables.Count - 1 do
        begin
          with TZTableRef(FTables[J]) do
            FFields.Add(TZFieldRef.Create(True, Catalog, Schema,
              Table, '*', '', TZTableRef(FTables[J])));
        end;
        Continue;
      end;

      if TableRef <> nil
      then FFields.Add(TZFieldRef.Create(True, TableRef.Catalog, TableRef.Schema,
          TableRef.Table, FieldRef.Field, FieldRef.Alias, TableRef))
      else FFields.Add(TZFieldRef.Create(True, FieldRef.Catalog, FieldRef.Schema,
          FieldRef.Table, FieldRef.Field, FieldRef.Alias, TableRef));
    end;
  finally
    FreeAndNil(TempFields);
  end;
end;

{**
  Adds a new field to this select schema.
  @param FieldRef a field reference object.
}
procedure TZSelectSchema.AddField({$IFDEF AUTOREFCOUNT}const{$ENDIF}FieldRef: TZFieldRef);
begin
  FFields.Add(FieldRef);
end;

{**
  Inserts a new field to this select schema.
  @param Index an index where to insert a new field reference.
  @param FieldRef a field reference object.
}
procedure TZSelectSchema.InsertField(Index: Integer; {$IFDEF AUTOREFCOUNT}const{$ENDIF}FieldRef: TZFieldRef);
begin
  FFields.Insert(Index, FieldRef);
end;

{**
  Deletes a field from this select schema.
  @param FieldRef a field reference object.
}
procedure TZSelectSchema.DeleteField({$IFDEF AUTOREFCOUNT}const{$ENDIF}FieldRef: TZFieldRef);
begin
  FFields.Remove(FieldRef);
end;

{**
  Adds a new table to this select schema.
  @param TableRef a table reference object.
}
procedure TZSelectSchema.AddTable({$IFDEF AUTOREFCOUNT}const{$ENDIF}TableRef: TZTableRef);
begin
  FTables.Add(TableRef);
end;

{**
  Gets a field reference by index.
  @param Index an index of the reference.
  @returns a pointer to the field reference.
}
function TZSelectSchema.GetField(Index: Integer): TZFieldRef;
begin
  Result := TZFieldRef(FFields[Index]);
end;

{**
  Gets a count of field references.
  @returns a count of field references.
}
function TZSelectSchema.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

{**
  Gets a table reference by index.
  @param Index an index of the reference.
  @returns a pointer to the table reference.
}
function TZSelectSchema.GetTable(Index: Integer): TZTableRef;
begin
  Result := TZTableRef(FTables[Index]);
end;

{**
  Gets a count of table references.
  @returns a count of table references.
}
function TZSelectSchema.GetTableCount: Integer;
begin
  Result := FTables.Count;
end;

end.

