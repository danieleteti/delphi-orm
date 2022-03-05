unit ZDbcProxyIntf;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Windows, ActiveX,
  zeosproxy;

  type IZDbcProxy = Interface(IUnknown)
    ['{374CAA55-95CD-44FE-8FF3-F90BF8D1DF8C}']
    procedure Connect(const UserName, Password, DbHost, DbName: WideString; var Properties: WideString; out DbInfo: WideString); safecall;
    procedure Disconnect; safecall;
    procedure SetAutoCommit(const Value: LongBool); safecall;
    procedure Commit; safecall;
    procedure Rollback; safecall;
    function SetProperties(const Properties : WideString): WideString; safecall;
    function ExecuteStatement(const SQL, Parameters: WideString; const MaxRows: LongWord): WideString; safecall;
    function GetTables(const Catalog, SchemaPattern, TableNamePattern, Types: WideString): WideString; safecall;
    function GetSchemas: WideString; safecall;
    function GetCatalogs: WideString; safecall;
    function GetTableTypes: WideString; safecall;
    function GetColumns(const Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern: WideString): WideString; safecall;
    function GetTablePrivileges(const Catalog, SchemaPattern, TableNamePattern: WideString): WideString; safecall;
    function GetColumnPrivileges(const Catalog, Schema, Table, ColumnNamePattern: WideString): WideString; safecall;
    function GetPrimaryKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
    function GetImportedKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
    function GetExportedKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
    function GetCrossReference(const PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable: WideString): WideString; safecall;
    function GetIndexInfo(const Catalog, Schema, Table: WideString; const Unique, Approximate: LongBool):WideString; safecall;
    function GetSequences(const Catalog, SchemaPattern, SequenceNamePattern : WideString ): WideString; safecall;
    function GetTriggers(const Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern: WideString): WideString; safecall;
    function GetProcedures(const Catalog, SchemaPattern, ProcedureNamePattern : WideString): WideString; safecall;
    function GetProcedureColumns(const Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern: WideString): WideString; safecall;
    function GetCharacterSets(): WideString; safecall;
  end;

  type TZDbcProxy = class(TInterfacedObject, IZDbcProxy, ISupportErrorInfo)
    protected
      FService: IZeosProxy;
      FConnectionID: WideString;
      procedure CheckConnected;
      // this is necessary for safecall exception handling
      function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
    public
      // this is necessary for safecall exception handling
      function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;

      procedure Connect(const UserName, Password, DbHost, DbName: WideString; var Properties: WideString; out DbInfo: WideString); safecall;
      procedure Disconnect; safecall;
      procedure SetAutoCommit(const Value: LongBool); safecall;
      procedure Commit; safecall;
      procedure Rollback; safecall;
      function SetProperties(const Properties : WideString): WideString; safecall;
      function ExecuteStatement(const SQL, Parameters: WideString; const MaxRows: LongWord): WideString; safecall;
      function GetTables(const Catalog, SchemaPattern, TableNamePattern, Types: WideString): WideString; safecall;
      function GetSchemas: WideString; safecall;
      function GetCatalogs: WideString; safecall;
      function GetTableTypes: WideString; safecall;
      function GetColumns(const Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern: WideString): WideString; safecall;
      function GetTablePrivileges(const Catalog, SchemaPattern, TableNamePattern: WideString): WideString; safecall;
      function GetColumnPrivileges(const Catalog, Schema, Table, ColumnNamePattern: WideString): WideString; safecall;
      function GetPrimaryKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
      function GetImportedKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
      function GetExportedKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
      function GetCrossReference(const PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable: WideString): WideString; safecall;
      function GetIndexInfo(const Catalog, Schema, Table: WideString; const Unique, Approximate: LongBool):WideString; safecall;
      function GetSequences(const Catalog, SchemaPattern, SequenceNamePattern : WideString ): WideString; safecall;
      function GetTriggers(const Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern: WideString): WideString; safecall;
      function GetProcedures(const Catalog, SchemaPattern, ProcedureNamePattern : WideString): WideString; safecall;
      function GetProcedureColumns(const Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern: WideString): WideString; safecall;
      function GetCharacterSets(): WideString; safecall;

      constructor Create;
      destructor Destroy; override;
  end;

implementation

uses ComObj,
     fpc_http_protocol, soap_formatter, zeosproxy_proxy;

procedure TZDbcProxy.CheckConnected;
begin
  if not Assigned(FService) then
    raise Exception.Create('No connection has been established yet!');
end;

function TZDbcProxy.InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
begin
  if GetInterfaceEntry(iid) <> nil then
    Result := S_OK else
    Result := S_FALSE;
end;

function TZDbcProxy.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, StringToGUID('{374CAA55-95CD-44FE-8FF3-F90BF8D1DF8C}'), 'libzdbcproxy.dll', '');
end;

constructor TZDbcProxy.Create;
begin
  FService := nil;
end;

destructor TZDbcProxy.Destroy;
begin
 FService := nil;
end;

procedure TZDbcProxy.Connect(const UserName, Password, DbHost, DbName: WideString; var Properties: WideString; out DbInfo: WideString); safecall;
var
  Url: UnicodeString;
  MyDbInfo: UnicodeString;
  MyInProperties: UnicodeString;
  MyOutProperties: UnicodeString;
begin
 Url := 'http://' + DbHost + ':8000/services/IZeosProxy';
 //FService := wst_CreateInstance_IZeosProxy('SOAP:', 'HTTP:', 'http://127.0.0.1:8000/services/IZeosProxy');
 FService := wst_CreateInstance_IZeosProxy('SOAP:', 'HTTP:', Url);

// if using a reverse proxy, this seems to work well:
// Url := 'https://' + DbHost + '/services/IZeosProxy';
// //FService := wst_CreateInstance_IZeosProxy('SOAP:', 'HTTP:', 'http://127.0.0.1:8000/services/IZeosProxy');
// FService := wst_CreateInstance_IZeosProxy('SOAP:', 'HTTP:', Url);

 MyInProperties := Properties;
 FConnectionID := FService.Connect(UserName, Password, DbName, MyInProperties, MyOutProperties, MyDbInfo);
 Properties := MyOutProperties;
 DbInfo := MyDbInfo;
end;

procedure TZDbcProxy.Disconnect; safecall;
begin
 CheckConnected;
 try
   FService.Disconnect(FConnectionID);
 finally
   FConnectionID := '';
 end;
end;

procedure TZDbcProxy.SetAutoCommit(const Value: LongBool); safecall;
begin
  CheckConnected;
  FService.SetAutoCommit(FConnectionID, Value);
end;

procedure TZDbcProxy.Commit; safecall;
begin
  CheckConnected;
  FService.Commit(FConnectionID);
end;

procedure TZDbcProxy.Rollback; safecall;
begin
  CheckConnected;
  FService.Rollback(FConnectionID);
end;

function TZDbcProxy.SetProperties(const Properties : WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.SetProperties(FConnectionID, Properties);
end;

function TZDbcProxy.ExecuteStatement(const SQL, Parameters: WideString; const MaxRows: LongWord): WideString; safecall;
begin
  CheckConnected;
  Result := FService.ExecuteStatement(FConnectionID, SQL, Parameters, MaxRows);
end;

function TZDbcProxy.GetTables(const Catalog, SchemaPattern, TableNamePattern, Types: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetTables(FConnectionID, Catalog, SchemaPattern, TableNamePattern, Types);
end;

function TZDbcProxy.GetSchemas: WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetSchemas(FConnectionID);
end;

function TZDbcProxy.GetCatalogs: WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetCatalogs(FConnectionID);
end;

function TZDbcProxy.GetTableTypes: WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetTableTypes(FConnectionID);
end;

function TZDbcProxy.GetColumns(const Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetColumns(FConnectionID, Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);
end;

function TZDbcProxy.GetTablePrivileges(const Catalog, SchemaPattern, TableNamePattern: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetTablePrivileges(FConnectionID, Catalog, SchemaPattern, TableNamePattern);
end;

function TZDbcProxy.GetColumnPrivileges(const Catalog, Schema, Table, ColumnNamePattern: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetColumnPrivileges(FConnectionID, Catalog, Schema, Table, ColumnNamePattern);
end;

function TZDbcProxy.GetPrimaryKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetPrimaryKeys(FConnectionID, Catalog, Schema, Table);
end;

function TZDbcProxy.GetImportedKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetImportedKeys(FConnectionID, Catalog, Schema, Table);
end;

function TZDbcProxy.GetExportedKeys(const Catalog, Schema, Table: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetExportedKeys(FConnectionID, Catalog, Schema, Table);
end;

function TZDbcProxy.GetCrossReference(const PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetCrossReference(FConnectionID, PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable);
end;

function TZDbcProxy.GetIndexInfo(const Catalog, Schema, Table: WideString; const Unique, Approximate: LongBool):WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetIndexInfo(FConnectionID, Catalog, Schema, Table, Unique, Approximate);
end;

function TZDbcProxy.GetSequences(const Catalog, SchemaPattern, SequenceNamePattern : WideString ): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetSequences(FConnectionID, Catalog, SchemaPattern, SequenceNamePattern);
end;

function TZDbcProxy.GetTriggers(const Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetTriggers(FConnectionID, Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern);
end;

function TZDbcProxy.GetProcedures(const Catalog, SchemaPattern, ProcedureNamePattern : WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetProcedures(FConnectionID, Catalog, SchemaPattern, ProcedureNamePattern);
end;

function TZDbcProxy.GetProcedureColumns(const Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern: WideString): WideString; safecall;
begin
  CheckConnected;
  Result := FService.GetProcedureColumns(FConnectionID, Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);
end;

function TZDbcProxy.GetCharacterSets(): WideString; safecall;
begin
 CheckConnected;
 Result := FService.GetCharacterSets(FConnectionID);
end;

initialization
  FPC_RegisterHTTP_Transport();

end.

