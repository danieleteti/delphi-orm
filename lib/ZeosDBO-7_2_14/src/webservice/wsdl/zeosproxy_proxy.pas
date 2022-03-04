{
This unit has been produced by ws_helper.
  Input unit name : "zeosproxy".
  This unit name  : "zeosproxy_proxy".
  Date            : "12.01.2020 21:27:15".
}

Unit zeosproxy_proxy;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, zeosproxy;

Type


  TZeosProxy_Proxy=class(TBaseProxy,zeosproxy.IZeosProxy)
  Protected
    class function GetServiceType() : PTypeInfo;override;
    function Connect(
      const  UserName : UnicodeString; 
      const  Password : UnicodeString; 
      const  DbName : UnicodeString; 
      const  InProperties : UnicodeString; 
      out  OutProperties : UnicodeString; 
      out  DbInfo : UnicodeString
    ):UnicodeString;
    procedure Disconnect(
      const  ConnectionID : UnicodeString
    );
    procedure SetAutoCommit(
      const  ConnectionID : UnicodeString; 
      const  Value : boolean
    );
    procedure Commit(
      const  ConnectionID : UnicodeString
    );
    procedure Rollback(
      const  ConnectionID : UnicodeString
    );
    function SetProperties(
      const  ConnectionID : UnicodeString; 
      const  Properties : UnicodeString
    ):UnicodeString;
    function ExecuteStatement(
      const  ConnectionID : UnicodeString; 
      const  SQL : UnicodeString; 
      const  Parameters : UnicodeString; 
      const  MaxRows : LongWord
    ):UnicodeString;
    function GetTables(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  TableNamePattern : UnicodeString; 
      const  Types : UnicodeString
    ):UnicodeString;
    function GetSchemas(
      const  ConnectionID : UnicodeString
    ):UnicodeString;
    function GetCatalogs(
      const  ConnectionID : UnicodeString
    ):UnicodeString;
    function GetTableTypes(
      const  ConnectionID : UnicodeString
    ):UnicodeString;
    function GetColumns(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  TableNamePattern : UnicodeString; 
      const  ColumnNamePattern : UnicodeString
    ):UnicodeString;
    function GetTablePrivileges(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  TableNamePattern : UnicodeString
    ):UnicodeString;
    function GetColumnPrivileges(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString; 
      const  ColumnNamePattern : UnicodeString
    ):UnicodeString;
    function GetPrimaryKeys(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString
    ):UnicodeString;
    function GetImportedKeys(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString
    ):UnicodeString;
    function GetExportedKeys(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString
    ):UnicodeString;
    function GetCrossReference(
      const  ConnectionID : UnicodeString; 
      const  PrimaryCatalog : UnicodeString; 
      const  PrimarySchema : UnicodeString; 
      const  PrimaryTable : UnicodeString; 
      const  ForeignCatalog : UnicodeString; 
      const  ForeignSchema : UnicodeString; 
      const  ForeignTable : UnicodeString
    ):UnicodeString;
    function GetIndexInfo(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  Schema : UnicodeString; 
      const  Table : UnicodeString; 
      const  Unique : boolean; 
      const  Approximate : boolean
    ):UnicodeString;
    function GetSequences(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  SequenceNamePattern : UnicodeString
    ):UnicodeString;
    function GetTriggers(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  TableNamePattern : UnicodeString; 
      const  TriggerNamePattern : UnicodeString
    ):UnicodeString;
    function GetProcedures(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  ProcedureNamePattern : UnicodeString
    ):UnicodeString;
    function GetProcedureColumns(
      const  ConnectionID : UnicodeString; 
      const  Catalog : UnicodeString; 
      const  SchemaPattern : UnicodeString; 
      const  ProcedureNamePattern : UnicodeString; 
      const  ColumnNamePattern : UnicodeString
    ):UnicodeString;
    function GetCharacterSets(
      const  ConnectionID : UnicodeString
    ):UnicodeString;
  End;

  Function wst_CreateInstance_IZeosProxy(const AFormat : string = 'SOAP:'; const ATransport : string = 'HTTP:'; const AAddress : string = ''):IZeosProxy;

Implementation
uses wst_resources_imp, metadata_repository;


Function wst_CreateInstance_IZeosProxy(const AFormat : string; const ATransport : string; const AAddress : string):IZeosProxy;
Var
  locAdr : string;
Begin
  locAdr := AAddress;
  if ( locAdr = '' ) then
    locAdr := GetServiceDefaultAddress(TypeInfo(IZeosProxy));
  Result := TZeosProxy_Proxy.Create('IZeosProxy',AFormat+GetServiceDefaultFormatProperties(TypeInfo(IZeosProxy)),ATransport + 'address=' + locAdr);
End;

{ TZeosProxy_Proxy implementation }

class function TZeosProxy_Proxy.GetServiceType() : PTypeInfo;
begin
  result := TypeInfo(zeosproxy.IZeosProxy);
end;

function TZeosProxy_Proxy.Connect(
  const  UserName : UnicodeString; 
  const  Password : UnicodeString; 
  const  DbName : UnicodeString; 
  const  InProperties : UnicodeString; 
  out  OutProperties : UnicodeString; 
  out  DbInfo : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Connect', GetTarget(),locCallContext);
      locSerializer.Put('UserName', TypeInfo(UnicodeString), UserName);
      locSerializer.Put('Password', TypeInfo(UnicodeString), Password);
      locSerializer.Put('DbName', TypeInfo(UnicodeString), DbName);
      locSerializer.Put('InProperties', TypeInfo(UnicodeString), InProperties);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);
      locStrPrmName := 'OutProperties';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, OutProperties);
      locStrPrmName := 'DbInfo';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, DbInfo);

  Finally
    locSerializer.Clear();
  End;
End;

procedure TZeosProxy_Proxy.Disconnect(
  const  ConnectionID : UnicodeString
);
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Disconnect', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);

  Finally
    locSerializer.Clear();
  End;
End;

procedure TZeosProxy_Proxy.SetAutoCommit(
  const  ConnectionID : UnicodeString; 
  const  Value : boolean
);
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SetAutoCommit', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Value', TypeInfo(boolean), Value);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);

  Finally
    locSerializer.Clear();
  End;
End;

procedure TZeosProxy_Proxy.Commit(
  const  ConnectionID : UnicodeString
);
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Commit', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);

  Finally
    locSerializer.Clear();
  End;
End;

procedure TZeosProxy_Proxy.Rollback(
  const  ConnectionID : UnicodeString
);
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('Rollback', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.SetProperties(
  const  ConnectionID : UnicodeString; 
  const  Properties : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('SetProperties', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Properties', TypeInfo(UnicodeString), Properties);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.ExecuteStatement(
  const  ConnectionID : UnicodeString; 
  const  SQL : UnicodeString; 
  const  Parameters : UnicodeString; 
  const  MaxRows : LongWord
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('ExecuteStatement', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('SQL', TypeInfo(UnicodeString), SQL);
      locSerializer.Put('Parameters', TypeInfo(UnicodeString), Parameters);
      locSerializer.Put('MaxRows', TypeInfo(LongWord), MaxRows);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetTables(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString; 
  const  Types : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetTables', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('SchemaPattern', TypeInfo(UnicodeString), SchemaPattern);
      locSerializer.Put('TableNamePattern', TypeInfo(UnicodeString), TableNamePattern);
      locSerializer.Put('Types', TypeInfo(UnicodeString), Types);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetSchemas(
  const  ConnectionID : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetSchemas', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetCatalogs(
  const  ConnectionID : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetCatalogs', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetTableTypes(
  const  ConnectionID : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetTableTypes', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetColumns(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString; 
  const  ColumnNamePattern : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetColumns', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('SchemaPattern', TypeInfo(UnicodeString), SchemaPattern);
      locSerializer.Put('TableNamePattern', TypeInfo(UnicodeString), TableNamePattern);
      locSerializer.Put('ColumnNamePattern', TypeInfo(UnicodeString), ColumnNamePattern);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetTablePrivileges(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetTablePrivileges', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('SchemaPattern', TypeInfo(UnicodeString), SchemaPattern);
      locSerializer.Put('TableNamePattern', TypeInfo(UnicodeString), TableNamePattern);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetColumnPrivileges(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString; 
  const  ColumnNamePattern : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetColumnPrivileges', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('Schema', TypeInfo(UnicodeString), Schema);
      locSerializer.Put('Table', TypeInfo(UnicodeString), Table);
      locSerializer.Put('ColumnNamePattern', TypeInfo(UnicodeString), ColumnNamePattern);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetPrimaryKeys(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetPrimaryKeys', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('Schema', TypeInfo(UnicodeString), Schema);
      locSerializer.Put('Table', TypeInfo(UnicodeString), Table);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetImportedKeys(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetImportedKeys', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('Schema', TypeInfo(UnicodeString), Schema);
      locSerializer.Put('Table', TypeInfo(UnicodeString), Table);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetExportedKeys(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetExportedKeys', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('Schema', TypeInfo(UnicodeString), Schema);
      locSerializer.Put('Table', TypeInfo(UnicodeString), Table);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetCrossReference(
  const  ConnectionID : UnicodeString; 
  const  PrimaryCatalog : UnicodeString; 
  const  PrimarySchema : UnicodeString; 
  const  PrimaryTable : UnicodeString; 
  const  ForeignCatalog : UnicodeString; 
  const  ForeignSchema : UnicodeString; 
  const  ForeignTable : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetCrossReference', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('PrimaryCatalog', TypeInfo(UnicodeString), PrimaryCatalog);
      locSerializer.Put('PrimarySchema', TypeInfo(UnicodeString), PrimarySchema);
      locSerializer.Put('PrimaryTable', TypeInfo(UnicodeString), PrimaryTable);
      locSerializer.Put('ForeignCatalog', TypeInfo(UnicodeString), ForeignCatalog);
      locSerializer.Put('ForeignSchema', TypeInfo(UnicodeString), ForeignSchema);
      locSerializer.Put('ForeignTable', TypeInfo(UnicodeString), ForeignTable);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetIndexInfo(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString; 
  const  Unique : boolean; 
  const  Approximate : boolean
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetIndexInfo', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('Schema', TypeInfo(UnicodeString), Schema);
      locSerializer.Put('Table', TypeInfo(UnicodeString), Table);
      locSerializer.Put('Unique', TypeInfo(boolean), Unique);
      locSerializer.Put('Approximate', TypeInfo(boolean), Approximate);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetSequences(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  SequenceNamePattern : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetSequences', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('SchemaPattern', TypeInfo(UnicodeString), SchemaPattern);
      locSerializer.Put('SequenceNamePattern', TypeInfo(UnicodeString), SequenceNamePattern);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetTriggers(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString; 
  const  TriggerNamePattern : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetTriggers', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('SchemaPattern', TypeInfo(UnicodeString), SchemaPattern);
      locSerializer.Put('TableNamePattern', TypeInfo(UnicodeString), TableNamePattern);
      locSerializer.Put('TriggerNamePattern', TypeInfo(UnicodeString), TriggerNamePattern);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetProcedures(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  ProcedureNamePattern : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetProcedures', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('SchemaPattern', TypeInfo(UnicodeString), SchemaPattern);
      locSerializer.Put('ProcedureNamePattern', TypeInfo(UnicodeString), ProcedureNamePattern);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetProcedureColumns(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  ProcedureNamePattern : UnicodeString; 
  const  ColumnNamePattern : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetProcedureColumns', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
      locSerializer.Put('Catalog', TypeInfo(UnicodeString), Catalog);
      locSerializer.Put('SchemaPattern', TypeInfo(UnicodeString), SchemaPattern);
      locSerializer.Put('ProcedureNamePattern', TypeInfo(UnicodeString), ProcedureNamePattern);
      locSerializer.Put('ColumnNamePattern', TypeInfo(UnicodeString), ColumnNamePattern);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;

function TZeosProxy_Proxy.GetCharacterSets(
  const  ConnectionID : UnicodeString
):UnicodeString;
Var
  locSerializer : IFormatterClient;
  locCallContext : ICallContext;
  locStrPrmName : string;
Begin
  locCallContext := Self as ICallContext;
  locSerializer := GetSerializer();
  Try
    locSerializer.BeginCall('GetCharacterSets', GetTarget(),locCallContext);
      locSerializer.Put('ConnectionID', TypeInfo(UnicodeString), ConnectionID);
    locSerializer.EndCall();

    MakeCall();

    locSerializer.BeginCallRead(locCallContext);
      locStrPrmName := 'result';
      locSerializer.Get(TypeInfo(UnicodeString), locStrPrmName, Result);

  Finally
    locSerializer.Clear();
  End;
End;


initialization
  {$i zeosproxy.wst}

  {$IF DECLARED(Register_zeosproxy_ServiceMetadata)}
  Register_zeosproxy_ServiceMetadata();
  {$IFEND}
End.
