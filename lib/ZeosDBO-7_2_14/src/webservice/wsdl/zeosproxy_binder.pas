{
This unit has been produced by ws_helper.
  Input unit name : "zeosproxy".
  This unit name  : "zeosproxy_binder".
  Date            : "12.01.2020 21:27:15".
}
unit zeosproxy_binder;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
interface

uses SysUtils, Classes, base_service_intf, server_service_intf, zeosproxy;


type
  TZeosProxy_ServiceBinder = class(TBaseServiceBinder)
  protected
    procedure ConnectHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure DisconnectHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetAutoCommitHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure CommitHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure RollbackHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure SetPropertiesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure ExecuteStatementHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetTablesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSchemasHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCatalogsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetTableTypesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetColumnsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetTablePrivilegesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetColumnPrivilegesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetPrimaryKeysHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetImportedKeysHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetExportedKeysHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCrossReferenceHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetIndexInfoHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetSequencesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetTriggersHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetProceduresHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetProcedureColumnsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
    procedure GetCharacterSetsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
  public
    constructor Create();
  end;

type
  TZeosProxy_ServiceBinderFactory = class(TInterfacedObject,IItemFactory)
  private
    FInstance : IInterface;
  protected
    function CreateInstance():IInterface;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  procedure Server_service_RegisterZeosProxyService();

Implementation
uses TypInfo, wst_resources_imp,metadata_repository;

{ TZeosProxy_ServiceBinder implementation }
procedure TZeosProxy_ServiceBinder.ConnectHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  UserName : UnicodeString;
  Password : UnicodeString;
  DbName : UnicodeString;
  InProperties : UnicodeString;
  OutProperties : UnicodeString;
  DbInfo : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'UserName';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,UserName);
  locStrPrmName := 'Password';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Password);
  locStrPrmName := 'DbName';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,DbName);
  locStrPrmName := 'InProperties';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,InProperties);
  locStrPrmName := 'OutProperties';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,OutProperties);
  locStrPrmName := 'DbInfo';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,DbInfo);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.Connect(UserName,Password,DbName,InProperties,OutProperties,DbInfo);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
      AFormatter.Put('OutProperties',TypeInfo(UnicodeString),OutProperties);
      AFormatter.Put('DbInfo',TypeInfo(UnicodeString),DbInfo);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.DisconnectHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.Disconnect(ConnectionID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.SetAutoCommitHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Value : boolean;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Value';  AFormatter.Get(TypeInfo(boolean),locStrPrmName,Value);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.SetAutoCommit(ConnectionID,Value);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.CommitHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.Commit(ConnectionID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.RollbackHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    tmpObj.Rollback(ConnectionID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.SetPropertiesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Properties : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Properties';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Properties);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.SetProperties(ConnectionID,Properties);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.ExecuteStatementHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  SQL : UnicodeString;
  Parameters : UnicodeString;
  MaxRows : LongWord;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'SQL';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,SQL);
  locStrPrmName := 'Parameters';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Parameters);
  locStrPrmName := 'MaxRows';  AFormatter.Get(TypeInfo(LongWord),locStrPrmName,MaxRows);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.ExecuteStatement(ConnectionID,SQL,Parameters,MaxRows);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetTablesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  SchemaPattern : UnicodeString;
  TableNamePattern : UnicodeString;
  Types : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'SchemaPattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,SchemaPattern);
  locStrPrmName := 'TableNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,TableNamePattern);
  locStrPrmName := 'Types';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Types);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetTables(ConnectionID,Catalog,SchemaPattern,TableNamePattern,Types);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetSchemasHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSchemas(ConnectionID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetCatalogsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetCatalogs(ConnectionID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetTableTypesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetTableTypes(ConnectionID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetColumnsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  SchemaPattern : UnicodeString;
  TableNamePattern : UnicodeString;
  ColumnNamePattern : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'SchemaPattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,SchemaPattern);
  locStrPrmName := 'TableNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,TableNamePattern);
  locStrPrmName := 'ColumnNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ColumnNamePattern);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetColumns(ConnectionID,Catalog,SchemaPattern,TableNamePattern,ColumnNamePattern);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetTablePrivilegesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  SchemaPattern : UnicodeString;
  TableNamePattern : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'SchemaPattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,SchemaPattern);
  locStrPrmName := 'TableNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,TableNamePattern);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetTablePrivileges(ConnectionID,Catalog,SchemaPattern,TableNamePattern);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetColumnPrivilegesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  Schema : UnicodeString;
  Table : UnicodeString;
  ColumnNamePattern : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'Schema';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Schema);
  locStrPrmName := 'Table';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Table);
  locStrPrmName := 'ColumnNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ColumnNamePattern);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetColumnPrivileges(ConnectionID,Catalog,Schema,Table,ColumnNamePattern);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetPrimaryKeysHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  Schema : UnicodeString;
  Table : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'Schema';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Schema);
  locStrPrmName := 'Table';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Table);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetPrimaryKeys(ConnectionID,Catalog,Schema,Table);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetImportedKeysHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  Schema : UnicodeString;
  Table : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'Schema';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Schema);
  locStrPrmName := 'Table';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Table);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetImportedKeys(ConnectionID,Catalog,Schema,Table);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetExportedKeysHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  Schema : UnicodeString;
  Table : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'Schema';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Schema);
  locStrPrmName := 'Table';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Table);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetExportedKeys(ConnectionID,Catalog,Schema,Table);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetCrossReferenceHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  PrimaryCatalog : UnicodeString;
  PrimarySchema : UnicodeString;
  PrimaryTable : UnicodeString;
  ForeignCatalog : UnicodeString;
  ForeignSchema : UnicodeString;
  ForeignTable : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'PrimaryCatalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,PrimaryCatalog);
  locStrPrmName := 'PrimarySchema';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,PrimarySchema);
  locStrPrmName := 'PrimaryTable';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,PrimaryTable);
  locStrPrmName := 'ForeignCatalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ForeignCatalog);
  locStrPrmName := 'ForeignSchema';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ForeignSchema);
  locStrPrmName := 'ForeignTable';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ForeignTable);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetCrossReference(ConnectionID,PrimaryCatalog,PrimarySchema,PrimaryTable,ForeignCatalog,ForeignSchema,ForeignTable);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetIndexInfoHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  Schema : UnicodeString;
  Table : UnicodeString;
  Unique : boolean;
  Approximate : boolean;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'Schema';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Schema);
  locStrPrmName := 'Table';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Table);
  locStrPrmName := 'Unique';  AFormatter.Get(TypeInfo(boolean),locStrPrmName,Unique);
  locStrPrmName := 'Approximate';  AFormatter.Get(TypeInfo(boolean),locStrPrmName,Approximate);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetIndexInfo(ConnectionID,Catalog,Schema,Table,Unique,Approximate);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetSequencesHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  SchemaPattern : UnicodeString;
  SequenceNamePattern : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'SchemaPattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,SchemaPattern);
  locStrPrmName := 'SequenceNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,SequenceNamePattern);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetSequences(ConnectionID,Catalog,SchemaPattern,SequenceNamePattern);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetTriggersHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  SchemaPattern : UnicodeString;
  TableNamePattern : UnicodeString;
  TriggerNamePattern : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'SchemaPattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,SchemaPattern);
  locStrPrmName := 'TableNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,TableNamePattern);
  locStrPrmName := 'TriggerNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,TriggerNamePattern);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetTriggers(ConnectionID,Catalog,SchemaPattern,TableNamePattern,TriggerNamePattern);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetProceduresHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  SchemaPattern : UnicodeString;
  ProcedureNamePattern : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'SchemaPattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,SchemaPattern);
  locStrPrmName := 'ProcedureNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ProcedureNamePattern);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetProcedures(ConnectionID,Catalog,SchemaPattern,ProcedureNamePattern);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetProcedureColumnsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  Catalog : UnicodeString;
  SchemaPattern : UnicodeString;
  ProcedureNamePattern : UnicodeString;
  ColumnNamePattern : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  locStrPrmName := 'Catalog';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,Catalog);
  locStrPrmName := 'SchemaPattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,SchemaPattern);
  locStrPrmName := 'ProcedureNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ProcedureNamePattern);
  locStrPrmName := 'ColumnNamePattern';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ColumnNamePattern);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetProcedureColumns(ConnectionID,Catalog,SchemaPattern,ProcedureNamePattern,ColumnNamePattern);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;

procedure TZeosProxy_ServiceBinder.GetCharacterSetsHandler(AFormatter : IFormatterResponse; AContext : ICallContext);
var
  cllCntrl : ICallControl;
  objCntrl : IObjectControl;
  hasObjCntrl : Boolean;
  tmpObj : IZeosProxy;
  callCtx : ICallContext;
  locStrPrmName : string;
  procName,trgName : string;
  ConnectionID : UnicodeString;
  returnVal : UnicodeString;
begin
  callCtx := AContext;
  
  locStrPrmName := 'ConnectionID';  AFormatter.Get(TypeInfo(UnicodeString),locStrPrmName,ConnectionID);
  
  tmpObj := Self.GetFactory().CreateInstance() as IZeosProxy;
  if Supports(tmpObj,ICallControl,cllCntrl) then
    cllCntrl.SetCallContext(callCtx);
  hasObjCntrl := Supports(tmpObj,IObjectControl,objCntrl);
  if hasObjCntrl then
    objCntrl.Activate();
  try
    returnVal := tmpObj.GetCharacterSets(ConnectionID);
    
    procName := AFormatter.GetCallProcedureName();
    trgName := AFormatter.GetCallTarget();
    AFormatter.Clear();
    AFormatter.BeginCallResponse(procName,trgName);
      AFormatter.Put('result',TypeInfo(UnicodeString),returnVal);
    AFormatter.EndCallResponse();
    
    callCtx := nil;
  finally
    if hasObjCntrl then
      objCntrl.Deactivate();
    Self.GetFactory().ReleaseInstance(tmpObj);
  end;
end;


constructor TZeosProxy_ServiceBinder.Create();
begin
  inherited Create(GetServiceImplementationRegistry().FindFactory('IZeosProxy'));
  RegisterVerbHandler('Connect',{$IFDEF FPC}@{$ENDIF}ConnectHandler);
  RegisterVerbHandler('Disconnect',{$IFDEF FPC}@{$ENDIF}DisconnectHandler);
  RegisterVerbHandler('SetAutoCommit',{$IFDEF FPC}@{$ENDIF}SetAutoCommitHandler);
  RegisterVerbHandler('Commit',{$IFDEF FPC}@{$ENDIF}CommitHandler);
  RegisterVerbHandler('Rollback',{$IFDEF FPC}@{$ENDIF}RollbackHandler);
  RegisterVerbHandler('SetProperties',{$IFDEF FPC}@{$ENDIF}SetPropertiesHandler);
  RegisterVerbHandler('ExecuteStatement',{$IFDEF FPC}@{$ENDIF}ExecuteStatementHandler);
  RegisterVerbHandler('GetTables',{$IFDEF FPC}@{$ENDIF}GetTablesHandler);
  RegisterVerbHandler('GetSchemas',{$IFDEF FPC}@{$ENDIF}GetSchemasHandler);
  RegisterVerbHandler('GetCatalogs',{$IFDEF FPC}@{$ENDIF}GetCatalogsHandler);
  RegisterVerbHandler('GetTableTypes',{$IFDEF FPC}@{$ENDIF}GetTableTypesHandler);
  RegisterVerbHandler('GetColumns',{$IFDEF FPC}@{$ENDIF}GetColumnsHandler);
  RegisterVerbHandler('GetTablePrivileges',{$IFDEF FPC}@{$ENDIF}GetTablePrivilegesHandler);
  RegisterVerbHandler('GetColumnPrivileges',{$IFDEF FPC}@{$ENDIF}GetColumnPrivilegesHandler);
  RegisterVerbHandler('GetPrimaryKeys',{$IFDEF FPC}@{$ENDIF}GetPrimaryKeysHandler);
  RegisterVerbHandler('GetImportedKeys',{$IFDEF FPC}@{$ENDIF}GetImportedKeysHandler);
  RegisterVerbHandler('GetExportedKeys',{$IFDEF FPC}@{$ENDIF}GetExportedKeysHandler);
  RegisterVerbHandler('GetCrossReference',{$IFDEF FPC}@{$ENDIF}GetCrossReferenceHandler);
  RegisterVerbHandler('GetIndexInfo',{$IFDEF FPC}@{$ENDIF}GetIndexInfoHandler);
  RegisterVerbHandler('GetSequences',{$IFDEF FPC}@{$ENDIF}GetSequencesHandler);
  RegisterVerbHandler('GetTriggers',{$IFDEF FPC}@{$ENDIF}GetTriggersHandler);
  RegisterVerbHandler('GetProcedures',{$IFDEF FPC}@{$ENDIF}GetProceduresHandler);
  RegisterVerbHandler('GetProcedureColumns',{$IFDEF FPC}@{$ENDIF}GetProcedureColumnsHandler);
  RegisterVerbHandler('GetCharacterSets',{$IFDEF FPC}@{$ENDIF}GetCharacterSetsHandler);
end;


{ TZeosProxy_ServiceBinderFactory }

function TZeosProxy_ServiceBinderFactory.CreateInstance():IInterface;
begin
  Result := FInstance;
end;

constructor TZeosProxy_ServiceBinderFactory.Create();
begin
  FInstance := TZeosProxy_ServiceBinder.Create() as IInterface;
end;

destructor TZeosProxy_ServiceBinderFactory.Destroy();
begin
  FInstance := nil;
  inherited Destroy();
end;


procedure Server_service_RegisterZeosProxyService();
Begin
  GetServerServiceRegistry().Register('IZeosProxy',TZeosProxy_ServiceBinderFactory.Create() as IItemFactory);
End;

initialization

  {$i zeosproxy.wst}

  {$IF DECLARED(Register_zeosproxy_ServiceMetadata)}
  Register_zeosproxy_ServiceMetadata();
  {$IFEND}

End.
