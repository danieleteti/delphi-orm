{
This unit has been produced by ws_helper.
  Input unit name : "zeosproxy".
  This unit name  : "zeosproxy_imp".
  Date            : "12.01.2020 21:27:15".
}
Unit zeosproxy_imp;
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}
Interface

Uses SysUtils, Classes, 
     base_service_intf, server_service_intf, server_service_imputils, zeosproxy;


type
  TZeosProxy_ServiceImp=class(TBaseServiceImplementation,IZeosProxy)
  Protected
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


  procedure RegisterZeosProxyImplementationFactory();

Implementation
uses config_objects;

{ TZeosProxy_ServiceImp implementation }
function TZeosProxy_ServiceImp.Connect(
  const  UserName : UnicodeString; 
  const  Password : UnicodeString; 
  const  DbName : UnicodeString; 
  const  InProperties : UnicodeString; 
  out  OutProperties : UnicodeString; 
  out  DbInfo : UnicodeString
):UnicodeString;
Begin
// your code here
End;

procedure TZeosProxy_ServiceImp.Disconnect(
  const  ConnectionID : UnicodeString
);
Begin
// your code here
End;

procedure TZeosProxy_ServiceImp.SetAutoCommit(
  const  ConnectionID : UnicodeString; 
  const  Value : boolean
);
Begin
// your code here
End;

procedure TZeosProxy_ServiceImp.Commit(
  const  ConnectionID : UnicodeString
);
Begin
// your code here
End;

procedure TZeosProxy_ServiceImp.Rollback(
  const  ConnectionID : UnicodeString
);
Begin
// your code here
End;

function TZeosProxy_ServiceImp.SetProperties(
  const  ConnectionID : UnicodeString; 
  const  Properties : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.ExecuteStatement(
  const  ConnectionID : UnicodeString; 
  const  SQL : UnicodeString; 
  const  Parameters : UnicodeString; 
  const  MaxRows : LongWord
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetTables(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString; 
  const  Types : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetSchemas(
  const  ConnectionID : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetCatalogs(
  const  ConnectionID : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetTableTypes(
  const  ConnectionID : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetColumns(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString; 
  const  ColumnNamePattern : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetTablePrivileges(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetColumnPrivileges(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString; 
  const  ColumnNamePattern : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetPrimaryKeys(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetImportedKeys(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetExportedKeys(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetCrossReference(
  const  ConnectionID : UnicodeString; 
  const  PrimaryCatalog : UnicodeString; 
  const  PrimarySchema : UnicodeString; 
  const  PrimaryTable : UnicodeString; 
  const  ForeignCatalog : UnicodeString; 
  const  ForeignSchema : UnicodeString; 
  const  ForeignTable : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetIndexInfo(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  Schema : UnicodeString; 
  const  Table : UnicodeString; 
  const  Unique : boolean; 
  const  Approximate : boolean
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetSequences(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  SequenceNamePattern : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetTriggers(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  TableNamePattern : UnicodeString; 
  const  TriggerNamePattern : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetProcedures(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  ProcedureNamePattern : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetProcedureColumns(
  const  ConnectionID : UnicodeString; 
  const  Catalog : UnicodeString; 
  const  SchemaPattern : UnicodeString; 
  const  ProcedureNamePattern : UnicodeString; 
  const  ColumnNamePattern : UnicodeString
):UnicodeString;
Begin
// your code here
End;

function TZeosProxy_ServiceImp.GetCharacterSets(
  const  ConnectionID : UnicodeString
):UnicodeString;
Begin
// your code here
End;



procedure RegisterZeosProxyImplementationFactory();
Begin
  GetServiceImplementationRegistry().Register('IZeosProxy',TImplementationFactory.Create(TZeosProxy_ServiceImp,wst_GetServiceConfigText('IZeosProxy')) as IServiceImplementationFactory);
End;

End.
