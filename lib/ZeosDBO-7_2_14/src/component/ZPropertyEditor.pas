{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Component Property Editors                }
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

unit ZPropertyEditor;

interface

{$I ZComponent.inc}
{.$DEFINE WITH_PROPERTY_EDITOR}
{.$DEFINE USE_METADATA}
{$IFDEF WITH_PROPERTY_EDITOR}

uses
  Types, Classes, ZClasses, ZCompatibility, ZDbcIntfs,
  ZConnectionGroup, ZAbstractConnection, ZURL,
{$IFDEF WITH_UNIT_WIDESTRINGS}
  WideStrings,
{$ENDIF}
{$IFNDEF FPC}
  DesignIntf, DesignEditors;
{$ELSE}
    PropEdits;
{$ENDIF}

type

  {** Implements the basic methods of the property editor. }
  TZStringProperty = class(TStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual; abstract;
    procedure GetValues(Proc: TGetStrProc); override;
    function  GetZComponent:TPersistent; virtual;
  end;

  {** Shows all Fields received from FieldDefs. }
  TZDataFieldPropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Fields received from IndexDefs - not used yet. }
  TZIndexFieldPropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Fields received from the MasterSource's DataSet.FieldDefs. }
  TZMasterFieldPropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Tables received from Connection.IZDatabaseMetadata. }
  TZTableNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Procedures received from Connection.IZDatabaseMetadata. }
  TZProcedureNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Sequences received from Connection.IZDatabaseMetadata. }
  TZSequenceNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Implements a property editor for ZConnection.Protocol property. }
  TZProtocolPropertyEditor = class(TZStringProperty)
  public
    function  GetValue: string; override;
    procedure GetValueList(List: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  {** Implements a property editor for ZConnection.ClientCodePage property. }
  TZClientCodePagePropertyEditor = class(TZStringProperty)
  public
    function  GetValue: string; override;
    procedure GetValueList(List: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  {** Implements a property editor for ZConnection.Database property. }
  TZDatabasePropertyEditor = class(TZStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure GetValueList(List: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  {** Implements a property editor for ZConnection.LibLocation property. }
  TZLibLocationPropertyEditor = class(TZStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
  end;

  // Modified by Una.Bicicleta 2010/10/31
  {** Implements a property editor for ZConnectionGroup.Database property. }
  TZConnectionGroupPropertyEditor = class(TZStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure GetValueList(List: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  {** Implements a property editor for ZGroupedConnection.Catalog property. }
  TZGroupedConnectionCatalogPropertyEditor = class(TZStringProperty)
  public
    function  GetValue: string; override;
    procedure GetValueList(List: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

  {** Implements a property editor for ZGroupedConnection.LibLocation property. }
  {** added 2013/02/20 }
  TZConnectionGroupLibLocationPropertyEditor = class(TZStringProperty)
  public
    function  GetAttributes: TPropertyAttributes; override;
    function  GetValue: string; override;
    procedure Edit; override;
    procedure SetValue(const Value: string); override;
  end;
  /////////////////////////////////////////////////////////


  {** Implements a property editor for ZConnection.Catalog property. }
  TZCatalogPropertyEditor = class(TZStringProperty)
  public
    function  GetValue: string; override;
    procedure GetValueList(List: TStrings); override;
    procedure SetValue(const Value: string); override;
  end;

{$IFDEF USE_METADATA}
  {** Shows all Catalogs received from Connection.IZDatabaseMetadata. }
  TZCatalogProperty = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Columns received from Connection.IZDatabaseMetadata. }
  TZColumnNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Schemes received from Connection.IZDatabaseMetadata. }
  TZSchemaPropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

  {** Shows all Types received from Connection.IZDatabaseMetadata. }
  TZTypeNamePropertyEditor = class(TZStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;
{$ENDIF}

const
  CRLF = LineEnding;

{$ENDIF}

implementation

{$IFDEF WITH_PROPERTY_EDITOR}

uses SysUtils, Forms, Dialogs, Controls, DB, TypInfo, ZSysUtils, ZSelectSchema
{$IFDEF USE_METADATA}
  , ZSqlMetadata
{$ENDIF}
{$IFNDEF UNIX}
  {$IFNDEF FPC}
  {$IFDEF ENABLE_ADO}
, ZDbcAdoUtils
  {$ENDIF}
  {$ENDIF}
{$ENDIF}
{$IFDEF SHOW_WARNING}
,ZMessages
{$ENDIF SHOW_WARNING}
;

{$IFDEF FPC}
procedure GetItemNames(FieldDefs: TFieldDefs; List: TStrings); overload;
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to Pred(FieldDefs.Count) do
    List.Append(FieldDefs[i].Name);
end;

procedure GetItemNames(IndexDefs: TIndexDefs; List: TStrings); overload;
var
  i: Integer;
begin
  List.Clear;
  for i := 0 to Pred(IndexDefs.Count) do
    List.Append(IndexDefs[i].Name);
end;
{$ENDIF}

{ Returns the IndexDefs from Dataset }
function GetIndexDefs(Component: TPersistent): TIndexDefs;
var
  DataSet: TDataSet;
begin
  DataSet := Component as TDataSet;
  Result := GetObjectProp(DataSet, 'IndexDefs') as TIndexDefs;
  if Assigned(Result) then
  begin
    Result.Updated := False;
    Result.Update;
  end;
end;

function IsEmpty(const s: string): Boolean;
begin
  Result := Trim(s) = '';
end;

{ TZStringProperty }

{**
  Gets a type of property attributes.
  @return a type of property attributes.
}
function TZStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

{**
  Processes a list of list items.
  @param Proc a procedure to process the list items.
}
procedure TZStringProperty.GetValues(Proc: TGetStrProc);
var
  i: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for i := 0 to Pred(Values.Count) do
      Proc(Values[i]);
  finally
    Values.Free;
  end;
end;

{**
  Gets the component that has the property.
}
function TZStringProperty.GetZComponent:TPersistent;
begin
  Result:=GetComponent(0);
end;

{ TZDataFieldPropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZDataFieldPropertyEditor.GetValueList(List: TStrings);
begin
  try
    with (GetZComponent as TDataSet) do
    begin
      // Update the FieldDefs and return the Fieldnames
      FieldDefs.Updated := False;
      FieldDefs.Update;
      {$IFNDEF FPC}
      FieldDefs.GetItemNames(List);
      {$ELSE}
      GetItemNames(FieldDefs, List);
      {$ENDIF}
    end;
  except
  end;
end;

{ TZIndexFieldPropertyEditor }

procedure TZIndexFieldPropertyEditor.GetValueList(List: TStrings);
begin
  {$IFNDEF FPC}
  GetIndexDefs(GetZComponent).GetItemNames(List);
  {$ELSE}
  GetItemNames(GetIndexDefs(GetZComponent), List);
  {$ENDIF}
end;

{ TZMasterFieldPropertyEditor }

procedure TZMasterFieldPropertyEditor.GetValueList(List: TStrings);
var
  DataSource: TDataSource;
begin
  DataSource := GetObjectProp(GetZComponent, 'MasterSource') as TDataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    {$IFDEF WITH_WIDESTRINGS_GETFIELDNAMES}
    DataSource.DataSet.GetFieldNames(TWideStrings(List));
    {$ELSE}
    DataSource.DataSet.GetFieldNames(List);
    {$ENDIF}
end;

{ TZTableNamePropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZTableNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZAbstractConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
  Schema, Tablename:String;
  IdentifierConvertor: IZIdentifierConvertor;
  Catalog: string;
  IsTZSqlMetadata: Boolean;
begin
  Connection := GetObjectProp(GetZComponent, 'Connection') as TZAbstractConnection;
  if Assigned(Connection) and Connection.Connected then
  begin
    Metadata := Connection.DbcConnection.GetMetadata;
    IdentifierConvertor := Metadata.GetIdentifierConvertor;
    Catalog := Connection.Catalog;
    Schema := '';
{$IFDEF USE_METADATA}
    IsTZSqlMetadata := GetZComponent is TZSqlMetadata;
    if IsTZSqlMetadata then
    begin
      Catalog := GetStrProp(GetZComponent, 'Catalog');
      Schema := GetStrProp(GetZComponent, 'Schema');
  {$IFDEF SHOW_WARNING}
      if not (IsEmpty(Catalog) and IsEmpty(Schema)) or
       (MessageDlg(SPropertyQuery + CRLF + SPropertyTables + CRLF +
        SPropertyExecute, mtWarning, [mbYes,mbNo], 0) = mrYes) then
        begin
        // continue
        end
      else
        exit;
  {$ENDIF}
    end;
{$ELSE}
  IsTZSqlMetadata := False;
{$ENDIF}
    begin
      try
        // Look for the Tables of the defined Catalog and Schema
        ResultSet := Metadata.GetTables(Catalog, Metadata.AddEscapeCharToWildcards(Schema), '', nil);
        while ResultSet.Next do
          if IsTZSqlMetadata then
            List.Add(IdentifierConvertor.Quote(ResultSet.GetStringByName('TABLE_NAME')))
          else begin
            TableName := IdentifierConvertor.Quote(ResultSet.GetStringByName('TABLE_NAME'));
            if Connection.DbcConnection.GetMetadata.GetDatabaseInfo.SupportsSchemasInTableDefinitions
            then Schema := ResultSet.GetStringByName('TABLE_SCHEM')
            else Schema := '';
            if (Catalog <> '') and Connection.DbcConnection.GetMetadata.GetDatabaseInfo.SupportsCatalogsInTableDefinitions
            then if Schema <> ''
              then TableName := IdentifierConvertor.Quote(Catalog) + '.'+ IdentifierConvertor.Quote(Schema) + '.' + TableName
              else TableName := IdentifierConvertor.Quote(Catalog) + '.' + TableName
            else if (Schema <> '') then
              TableName := IdentifierConvertor.Quote(Schema) + '.' + TableName;
            List.Add(TableName);
          end;
      finally
        ResultSet.Close;
      end;
    end;
  end;
end;

{ TZProcedureNamePropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZProcedureNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZAbstractConnection;
  Metadata: IZDatabaseMetadata;
  IdentifierConvertor: IZIdentifierConvertor;
  ResultSet: IZResultSet;
  Catalog, Schema: string;
  ProcedureName: string;
  IsTZSqlMetadata: Boolean;

  procedure ExtractOverload(OverloadSeparator: String);
  var
    I: Integer;
    SL: TStrings;
  begin
    SL := TStringList.Create;
    PutSplitString(SL, ProcedureName, OverloadSeparator);
    if SL.Count > 1 then
    begin
      SL.Delete(SL.Count -1);
      ProcedureName := '';
      for i := 0 to SL.Count -1 do
        if ProcedureName = '' then
          ProcedureName := ProcedureName + SL[i]
        else
          ProcedureName :=  ProcedureName +OverloadSeparator+ SL[i]; //don't forget to give the delimiter back too
    end;
  end;
begin
  Connection := GetObjectProp(GetZComponent, 'Connection') as TZAbstractConnection;
  if Assigned(Connection) and Connection.Connected then
  begin
    Metadata := Connection.DbcConnection.GetMetadata;
    IdentifierConvertor := Metadata.GetIdentifierConvertor;
    Catalog := Connection.Catalog;
    Schema := '';
{$IFDEF USE_METADATA}
    IsTZSqlMetadata := GetZComponent is TZSqlMetadata;
    if IsTZSqlMetadata then
    begin
      Catalog := GetStrProp(GetZComponent, 'Catalog');
      Schema := GetStrProp(GetZComponent, 'Schema');
{$IFDEF SHOW_WARNING}
      if not (IsEmpty(Catalog) and IsEmpty(Schema)) or
       (MessageDlg(SPropertyQuery + CRLF + SPropertyProcedures + CRLF +
        SPropertyExecute, mtWarning, [mbYes,mbNo], 0) = mrYes) then
        begin
        // continue
        end
      else
        exit;
{$ENDIF}
    end;
{$ELSE}
  IsTZSqlMetadata := False;
{$ENDIF}
    begin
      try
        Metadata := Connection.DbcConnection.GetMetadata;
        // Look for the Procedures
        ResultSet := Metadata.GetProcedures(Catalog, Metadata.AddEscapeCharToWildcards(Schema), '');
        while ResultSet.Next do
        begin
          ProcedureName := ResultSet.GetStringByName('PROCEDURE_NAME');
          if ( not Metadata.GetDatabaseInfo.SupportsOverloadPrefixInStoredProcedureName ) then
            if not ( StartsWith(ProcedureName, MetaData.GetDatabaseInfo.GetIdentifierQuoteString) or
                     EndsWith(ProcedureName, MetaData.GetDatabaseInfo.GetIdentifierQuoteString) or
                     (Pos('.', ProcedureName) > 0) ) then
              ProcedureName := IdentifierConvertor.Quote(ProcedureName);
          if IsTZSqlMetadata then
            List.Add(ProcedureName)
          else begin
            if Metadata.GetDatabaseInfo.SupportsSchemasInProcedureCalls
            then Schema := ResultSet.GetStringByName('PROCEDURE_SCHEM')
            else Schema := '';
            if (Catalog <> '') and Metadata.GetDatabaseInfo.SupportsCatalogsInProcedureCalls
            then if Schema <> ''
              then ProcedureName := IdentifierConvertor.Quote(Catalog) +'.'+ IdentifierConvertor.Quote(Schema) + '.' + ProcedureName
              else ProcedureName := IdentifierConvertor.Quote(Catalog) +'.'+ ProcedureName
            else if Schema <> '' then
              ProcedureName := IdentifierConvertor.Quote(Schema) + '.' + ProcedureName;
            List.Add(ProcedureName);
          end;
        end;
      finally
        ResultSet.Close;
      end;
    end;
  end;
end;

{ TZSequenceNamePropertyEditor }

{**
  Gets a selected string value.
  @return a selected string value.
}
procedure TZSequenceNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZAbstractConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
{$IFDEF USE_METADATA}
  Catalog, Schema: string;
{$ENDIF}
begin
  Connection := GetObjectProp(GetZComponent, 'Connection') as TZAbstractConnection;
  if Assigned(Connection) and Connection.Connected then
  begin
{$IFDEF USE_METADATA}
    if GetZComponent is TZSqlMetadata then
    begin
      Catalog := GetStrProp(GetZComponent, 'Catalog');
      Schema := GetStrProp(GetZComponent, 'Schema');
{$IFDEF SHOW_WARNING}
      if not (IsEmpty(Catalog) and IsEmpty(Schema)) or
       (MessageDlg(SPropertyQuery + CRLF + SPropertySequences + CRLF +
        SPropertyExecute, mtWarning, [mbYes,mbNo], 0) = mrYes) then
{$ENDIF}
      try
        Metadata := Connection.DbcConnection.GetMetadata;
        // Look for the Procedures of the defined Catalog and Schema
        Schema := Metadata.AddEscapeCharToWildcards(Schema);
        ResultSet := Metadata.GetSequences(Catalog, Schema, '');
        while ResultSet.Next do
          List.Add(ResultSet.GetStringByName('SEQUENCE_NAME'));
      finally
        ResultSet.Close;
      end;
    end
    else
{$ENDIF}
    begin
      try
        Metadata := Connection.DbcConnection.GetMetadata;
        // Look for the Procedures
        ResultSet := Metadata.GetSequences(Connection.Catalog, '', '');
        while ResultSet.Next do
          if ResultSet.GetStringByName('SEQUENCE_SCHEM') <> '' then
            List.Add(ResultSet.GetStringByName('SEQUENCE_SCHEM')+
              '.'+ResultSet.GetStringByName('SEQUENCE_NAME'))
          else
            List.Add(ResultSet.GetStringByName('SEQUENCE_NAME'));
      finally
        ResultSet.Close;
      end;
    end;
  end;
end;

{ TZProtocolPropertyEditor }

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZProtocolPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZProtocolPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
  if GetZComponent is TZAbstractConnection then
    (GetZComponent as TZAbstractConnection).Connected := False;
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZProtocolPropertyEditor.GetValueList(List: TStrings);
var
  I, J: Integer;
  Drivers: IZCollection;
  Protocols: TStringDynArray;
begin
  Drivers := DriverManager.GetDrivers;
  Protocols := nil;
  for I := 0 to Drivers.Count - 1 do
  begin
    Protocols := (Drivers[I] as IZDriver).GetSupportedProtocols;
    for J := Low(Protocols) to High(Protocols) do
      List.Append(Protocols[J]);
  end;
end;

{TZClientCodePagePropertyEditor -> EgonHugeist 19.01.2012}

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
function  TZClientCodePagePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZClientCodePagePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZAbstractConnection;
  I: Integer;
  SDyn: TStringDynArray;
  Url: TZURL;
begin

  if GetZComponent is TZAbstractConnection then
    Connection := (GetZComponent as TZAbstractConnection)
  else
    Connection := nil;

  if Assigned(Connection) then
  begin
    if Connection.Protocol = '' then
      List.Append('No Protocol selected!')
    else
    begin
      Url := TZURL.Create;
      Url.Protocol :=  Connection.Protocol;
      SDyn := DriverManager.GetDriver(Url.URL).GetSupportedClientCodePages(Url,
        {$IFNDEF UNICODE}Connection.AutoEncodeStrings, {$ENDIF}True, Connection.ControlsCodePage);
      Url.Free;
      for i := 0 to high(SDyn) do
        List.Append(SDyn[i]);

      TStringList(List).Sort;
    end;
  end;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZClientCodePagePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
  if GetZComponent is TZAbstractConnection then
    (GetZComponent as TZAbstractConnection).Connected := False;
end;


{ TZDatabasePropertyEditor }

{**
  Gets a type of property attributes.
  @return a type of property attributes.
}
function TZDatabasePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  if GetZComponent is TZAbstractConnection then
  begin
    if ((GetZComponent as TZAbstractConnection).Protocol = 'mssql') or
    ((GetZComponent as TZAbstractConnection).Protocol = 'sybase') then
      Result := inherited GetAttributes
    else
      Result := [paDialog];
  end;
end;

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZDatabasePropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZDatabasePropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
  if GetZComponent is TZAbstractConnection then
    (GetZComponent as TZAbstractConnection).Connected := False;
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZDatabasePropertyEditor.GetValueList(List: TStrings);
var
  DbcConnection: IZConnection;
  Url: string;
begin
  if GetZComponent is TZAbstractConnection then
  try
    URL := (GetZComponent as TZAbstractConnection).GetURL;
      (GetZComponent as TZAbstractConnection).ShowSqlHourGlass;
    try
      DbcConnection := DriverManager.GetConnectionWithParams(Url,
        (GetZComponent as TZAbstractConnection).Properties);

      with DbcConnection.GetMetadata.GetCatalogs do
      try
        while Next do
          List.Append(GetStringByName('TABLE_CAT'));
      finally
        Close;
      end;

    finally
      (GetZComponent as TZAbstractConnection).HideSqlHourGlass;
    end;
  except
//    raise;
  end;
end;

{**
  Brings up the proper database property editor dialog.
}
procedure TZDatabasePropertyEditor.Edit;
var
  OD: TOpenDialog;
begin
  if GetZComponent is TZAbstractConnection then
  begin
    if ((GetZComponent as TZAbstractConnection).Protocol = 'mssql') or
    ((GetZComponent as TZAbstractConnection).Protocol = 'sybase') then
      inherited
{$IFNDEF UNIX}
{$IFNDEF FPC}
{$IFDEF ENABLE_ADO}
    else
    if ((GetZComponent as TZAbstractConnection).Protocol = 'ado') then
      (GetZComponent as TZAbstractConnection).Database := PromptDataSource(Application.Handle,
        (GetZComponent as TZAbstractConnection).Database)
{$ENDIF}
{$ENDIF}
{$ENDIF}
    else
    begin
      OD := TOpenDialog.Create(nil);
      try
        OD.InitialDir := ExtractFilePath((GetZComponent as TZAbstractConnection).Database);
        if OD.Execute then
          (GetZComponent as TZAbstractConnection).Database := OD.FileName;
      finally
        OD.Free;
      end;
    end;
  end
  else
    inherited;
end;

{ TZLibLocationPropertyEditor }

{**
  Gets a type of property attributes.
  @return a type of property attributes.
}
function TZLibLocationPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  if GetZComponent is TZAbstractConnection then
    Result := [paDialog];
end;

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZLibLocationPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZLibLocationPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
  if GetZComponent is TZAbstractConnection then
    (GetZComponent as TZAbstractConnection).Connected := False;
end;

{**
  Brings up the proper LibLocation property editor dialog.
}
procedure TZLibLocationPropertyEditor.Edit;
var
  OD: TOpenDialog;
begin
  if GetZComponent is TZAbstractConnection then
  begin
    OD := TOpenDialog.Create(nil);
    try
      OD.InitialDir := ExtractFilePath((GetZComponent as TZAbstractConnection).LibLocation);
      if OD.Execute then
        (GetZComponent as TZAbstractConnection).LibLocation := OD.FileName;
    finally
      OD.Free;
    end;
  end
  else
    inherited;
end;

{ TZCatalogPropertyEditor }

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZCatalogPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZCatalogPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZCatalogPropertyEditor.GetValueList(List: TStrings);
var
  DbcConnection: IZConnection;
  Url: string;
begin
  if GetZComponent is TZAbstractConnection then
  try
    URL := (GetZComponent as TZAbstractConnection).GetURL;
    (GetZComponent as TZAbstractConnection).ShowSqlHourGlass;
    try
      DbcConnection := DriverManager.GetConnectionWithParams(Url,
        (GetZComponent as TZAbstractConnection).Properties);

      if Assigned(DbcConnection) then
        if DbcConnection.GetMetadata.GetDatabaseInfo.SupportsCatalogsInDataManipulation then
          with DbcConnection.GetMetadata.GetCatalogs do
          try
            while Next do
              List.Append(GetStringByName('TABLE_CAT'));
          finally
            Close;
          end
        else if DbcConnection.GetMetadata.GetDatabaseInfo.SupportsSchemasInDataManipulation then
          with DbcConnection.GetMetadata.GetSchemas do
          try
            while Next do
              List.Append(GetStringByName('TABLE_SCHEM'));
          finally
            Close;
          end;
    finally
      (GetZComponent as TZAbstractConnection).HideSqlHourGlass;
    end;
  except
//    raise;
  end;
end;

{$IFDEF USE_METADATA}

{ TZCatalogProperty }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZCatalogProperty.GetValueList(List: TStrings);
var
  Connection: TZAbstractConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  Connection := GetObjectProp(GetZComponent, 'Connection') as TZAbstractConnection;
  if Assigned(Connection) and Connection.Connected then
  try
    Metadata := Connection.DbcConnection.GetMetadata;
    ResultSet := Metadata.GetCatalogs;
    while ResultSet.Next do
      List.Add(ResultSet.GetStringByName('TABLE_CAT'));
  finally
    ResultSet.Close;
  end;
end;

{ TZColumnNamePropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZColumnNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZAbstractConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
  Catalog, Schema, TableName: string;
  MetadataType: TZMetadataType;
begin
  Connection := GetObjectProp(GetZComponent, 'Connection') as TZAbstractConnection;
  if Assigned(Connection) and Connection.Connected then
  begin
    Catalog := GetStrProp(GetZComponent, 'Catalog');
    Schema := GetStrProp(GetZComponent, 'Schema');
    MetadataType := TZMetadataType(GetOrdProp(GetZComponent, 'MetadataType'));
    if MetadataType = mdProcedureColumns
    then TableName := GetStrProp(GetZComponent, 'ProcedureName')
    else TableName := GetStrProp(GetZComponent, 'TableName');
{$IFDEF SHOW_WARNING}
    if not IsEmpty(TableName) or not (IsEmpty(Schema) and IsEmpty(Schema)) or
     (MessageDlg(SPropertyQuery + CRLF + SPropertyTables + CRLF +
      SPropertyExecute, mtWarning, [mbYes,mbNo], 0) = mrYes) then
{$ENDIF}
    try
      Metadata := Connection.DbcConnection.GetMetadata;
      // Look for the Columns of the defined Catalog, Schema and TableName
      if MetadataType = mdProcedureColumns
      then ResultSet := Metadata.GetProcedureColumns(Catalog, Metadata.AddEscapeCharToWildcards(Schema), Metadata.AddEscapeCharToWildcards(TableName), '')
      else ResultSet := Metadata.GetColumns(Catalog, Metadata.AddEscapeCharToWildcards(Schema), Metadata.AddEscapeCharToWildcards(TableName), '');
      while ResultSet.Next do
        if List.IndexOf(ResultSet.GetStringByName('COLUMN_NAME')) = -1 then
          List.Add(ResultSet.GetStringByName('COLUMN_NAME'));
    finally
      ResultSet.Close;
    end;
  end;
end;

{ TZSchemaPropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZSchemaPropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZAbstractConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  Connection := GetObjectProp(GetZComponent, 'Connection') as TZAbstractConnection;
  if Assigned(Connection) and Connection.Connected then
  try
    Metadata := Connection.DbcConnection.GetMetadata;
    ResultSet := Metadata.GetSchemas;
    while ResultSet.Next do
      List.Add(ResultSet.GetStringByName('TABLE_SCHEM'));
  finally
    ResultSet.Close;
  end;
end;

{ TZTypeNamePropertyEditor }

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZTypeNamePropertyEditor.GetValueList(List: TStrings);
var
  Connection: TZAbstractConnection;
  Metadata: IZDatabaseMetadata;
  ResultSet: IZResultSet;
begin
  Connection := GetObjectProp(GetZComponent, 'Connection') as TZAbstractConnection;
  if Assigned(Connection) and Connection.Connected then
  try
    Metadata := Connection.DbcConnection.GetMetadata;
    ResultSet := Metadata.GetTypeInfo;
    while ResultSet.Next do
      List.Add(ResultSet.GetStringByName('TYPE_NAME'));
  finally
    ResultSet.Close;
  end;
end;


//xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

{ TZConnectionGroupCatalogPropertyEditor }


function TZGroupedConnectionCatalogPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

procedure TZGroupedConnectionCatalogPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
end;

procedure TZGroupedConnectionCatalogPropertyEditor.GetValueList(List: TStrings);
var
  DbcConnection: IZConnection;
  Url: string;
begin
  if GetZComponent is TZAbstractConnection then
  try
    URL := (GetZComponent as TZAbstractConnection).GetURL;
    (GetZComponent as TZAbstractConnection).ShowSqlHourGlass;
    try
      DbcConnection := DriverManager.GetConnectionWithParams(Url,
        (GetZComponent as TZAbstractConnection).Properties);

      with DbcConnection.GetMetadata.GetCatalogs do
      try
        while Next do
          List.Append(GetStringByName('TABLE_CAT'));
      finally
        Close;
      end;

    finally
      (GetZComponent as TZAbstractConnection).HideSqlHourGlass;
    end;
  except
//    raise;
  end;
end;




{ TZConnectionGroupPropertyEditor }

{**
  Gets a type of property attributes.
  @return a type of property attributes.
}
function TZConnectionGroupPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  if GetZComponent is TZConnectionGroup then
  begin
    if ((GetZComponent as TZConnectionGroup).Protocol = 'mssql') or
    ((GetZComponent as TZConnectionGroup).Protocol = 'sybase') then
      Result := inherited GetAttributes
    else
      Result := [paDialog];
  end;
end;

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZConnectionGroupPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZConnectionGroupPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
//  if GetZComponent is TZConnectionGroup then
//    (GetZComponent as TZConnectionGroup).Connected := False;
end;

{**
  Processes a list of list items.
  @param List the list to process the list items.
}
procedure TZConnectionGroupPropertyEditor.GetValueList(List: TStrings);
var
  DbcConnection: IZConnection;
begin
  if GetZComponent is TZConnectionGroup then
  try
    DbcConnection := (GetZComponent as TZAbstractConnection).DbcConnection;
    with DbcConnection.GetMetadata.GetCatalogs do
    try
      while Next do
        List.Append(GetStringByName('TABLE_CAT'));
    finally
      Close;
    end;
  except
//    raise;
  end;
end;

{**
  Brings up the proper database property editor dialog.
}
procedure TZConnectionGroupPropertyEditor.Edit;
var
  OD: TOpenDialog;
begin
  if GetZComponent is TZConnectionGroup then
  begin
    if ((GetZComponent as TZConnectionGroup).Protocol = 'mssql') or
    ((GetZComponent as TZConnectionGroup).Protocol = 'sybase') then
      inherited
{$IFNDEF UNIX}
{$IFNDEF FPC}
{$IFDEF ENABLE_ADO}
    else
    if ((GetZComponent as TZConnectionGroup).Protocol = 'ado') then
      (GetZComponent as TZConnectionGroup).Database := PromptDataSource(Application.Handle,
        (GetZComponent as TZConnectionGroup).Database)
{$ENDIF}
{$ENDIF}
{$ENDIF}
    else
    begin
      OD := TOpenDialog.Create(nil);
      try
        OD.InitialDir := ExtractFilePath((GetZComponent as TZConnectionGroup).Database);
        if OD.Execute then
          (GetZComponent as TZConnectionGroup).Database := OD.FileName;
      finally
        OD.Free;
      end;
    end;
  end
  else
    inherited;
end;

{** added 2013/02/20 }
{ TZConnectionGroupLibLocationPropertyEditor }

{**
  Gets a type of property attributes.
  @return a type of property attributes.
}
function TZConnectionGroupLibLocationPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  if GetZComponent is TZConnectionGroup then
    Result := [paDialog];
end;

{**
  Gets a selected string value.
  @return a selected string value.
}
function TZConnectionGroupLibLocationPropertyEditor.GetValue: string;
begin
  Result := GetStrValue;
end;

{**
  Sets a new selected string value.
  @param Value a new selected string value.
}
procedure TZConnectionGroupLibLocationPropertyEditor.SetValue(const Value: string);
begin
  SetStrValue(Value);
  //if GetZComponent is TZAbstractConnection then
  //  (GetZComponent as TZAbstractConnection).Connected := False;
end;

{**
  Brings up the proper LibLocation property editor dialog.
}
procedure TZConnectionGroupLibLocationPropertyEditor.Edit;
var
  OD: TOpenDialog;
begin
  if GetZComponent is TZConnectionGroup then
  begin
    OD := TOpenDialog.Create(nil);
    try
      OD.InitialDir := ExtractFilePath((GetZComponent as TZConnectionGroup).LibraryLocation);
      if OD.Execute then
        (GetZComponent as TZConnectionGroup).LibraryLocation := OD.FileName;
    finally
      OD.Free;
    end;
  end
  else
    inherited;
end;

{$ENDIF}

{$ENDIF}

end.


