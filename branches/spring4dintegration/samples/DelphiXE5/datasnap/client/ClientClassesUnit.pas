//
// Created by the DataSnap proxy generator.
// 07/11/2011 15.33.18
//

unit ClientClassesUnit;

interface

uses Data.DBXCommon, Data.DBXClient, Data.DBXDataSnap, Data.DBXJSON, DSProxy, Classes, System.SysUtils, Data.DB, Data.SqlExpr, Data.DBXDBReaders, System.Generics.Collections, BusinessObjects, Data.DBXJSONReflect;

type
  TdormServerSampleClient = class(TDSAdminClient)
  private
    FDataModuleCreateCommand: TDBXCommand;
    FDataModuleDestroyCommand: TDBXCommand;
    FEchoStringCommand: TDBXCommand;
    FReverseStringCommand: TDBXCommand;
    FGetPeopleCommand: TDBXCommand;
    FLoadRelationsCommand: TDBXCommand;
    FLoadPersonByOIDCommand: TDBXCommand;
    FPersistCommand: TDBXCommand;
    FDeleteCommand: TDBXCommand;
    FSearchByNameCommand: TDBXCommand;
  public
    constructor Create(ADBXConnection: TDBXConnection); overload;
    constructor Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean); overload;
    destructor Destroy; override;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    function GetPeople: TObjectList<BusinessObjects.TPerson>;
    procedure LoadRelations(var APerson: TPerson);
    function LoadPersonByOID(OID: Integer): TPerson;
    function Persist(AObject: TObject): Integer;
    procedure Delete(AObject: TObject);
    function SearchByName(AName: string): TObjectList<BusinessObjects.TPerson>;
  end;

implementation

procedure TdormServerSampleClient.DataModuleCreate(Sender: TObject);
begin
  if FDataModuleCreateCommand = nil then
  begin
    FDataModuleCreateCommand := FDBXConnection.CreateCommand;
    FDataModuleCreateCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FDataModuleCreateCommand.Text := 'TdormServerSample.DataModuleCreate';
    FDataModuleCreateCommand.Prepare;
  end;
  if not Assigned(Sender) then
    FDataModuleCreateCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDBXClientCommand(FDataModuleCreateCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FDataModuleCreateCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(Sender), True);
      if FInstanceOwner then
        Sender.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FDataModuleCreateCommand.ExecuteUpdate;
end;

procedure TdormServerSampleClient.DataModuleDestroy(Sender: TObject);
begin
  if FDataModuleDestroyCommand = nil then
  begin
    FDataModuleDestroyCommand := FDBXConnection.CreateCommand;
    FDataModuleDestroyCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FDataModuleDestroyCommand.Text := 'TdormServerSample.DataModuleDestroy';
    FDataModuleDestroyCommand.Prepare;
  end;
  if not Assigned(Sender) then
    FDataModuleDestroyCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDBXClientCommand(FDataModuleDestroyCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FDataModuleDestroyCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(Sender), True);
      if FInstanceOwner then
        Sender.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FDataModuleDestroyCommand.ExecuteUpdate;
end;

function TdormServerSampleClient.EchoString(Value: string): string;
begin
  if FEchoStringCommand = nil then
  begin
    FEchoStringCommand := FDBXConnection.CreateCommand;
    FEchoStringCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FEchoStringCommand.Text := 'TdormServerSample.EchoString';
    FEchoStringCommand.Prepare;
  end;
  FEchoStringCommand.Parameters[0].Value.SetWideString(Value);
  FEchoStringCommand.ExecuteUpdate;
  Result := FEchoStringCommand.Parameters[1].Value.GetWideString;
end;

function TdormServerSampleClient.ReverseString(Value: string): string;
begin
  if FReverseStringCommand = nil then
  begin
    FReverseStringCommand := FDBXConnection.CreateCommand;
    FReverseStringCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FReverseStringCommand.Text := 'TdormServerSample.ReverseString';
    FReverseStringCommand.Prepare;
  end;
  FReverseStringCommand.Parameters[0].Value.SetWideString(Value);
  FReverseStringCommand.ExecuteUpdate;
  Result := FReverseStringCommand.Parameters[1].Value.GetWideString;
end;

function TdormServerSampleClient.GetPeople: TObjectList<BusinessObjects.TPerson>;
begin
  if FGetPeopleCommand = nil then
  begin
    FGetPeopleCommand := FDBXConnection.CreateCommand;
    FGetPeopleCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FGetPeopleCommand.Text := 'TdormServerSample.GetPeople';
    FGetPeopleCommand.Prepare;
  end;
  FGetPeopleCommand.ExecuteUpdate;
  if not FGetPeopleCommand.Parameters[0].Value.IsNull then
  begin
    FUnMarshal := TDBXClientCommand(FGetPeopleCommand.Parameters[0].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TObjectList<BusinessObjects.TPerson>(FUnMarshal.UnMarshal(FGetPeopleCommand.Parameters[0].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FGetPeopleCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

procedure TdormServerSampleClient.LoadRelations(var APerson: TPerson);
begin
  if FLoadRelationsCommand = nil then
  begin
    FLoadRelationsCommand := FDBXConnection.CreateCommand;
    FLoadRelationsCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FLoadRelationsCommand.Text := 'TdormServerSample.LoadRelations';
    FLoadRelationsCommand.Prepare;
  end;
  if not Assigned(APerson) then
    FLoadRelationsCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDBXClientCommand(FLoadRelationsCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FLoadRelationsCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(APerson), True);
      if FInstanceOwner then
        APerson.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FLoadRelationsCommand.ExecuteUpdate;
  if not FLoadRelationsCommand.Parameters[0].Value.IsNull then
  begin
    FUnMarshal := TDBXClientCommand(FLoadRelationsCommand.Parameters[0].ConnectionHandler).GetJSONUnMarshaler;
    try
      APerson := TPerson(FUnMarshal.UnMarshal(FLoadRelationsCommand.Parameters[0].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FLoadRelationsCommand.FreeOnExecute(APerson);
    finally
      FreeAndNil(FUnMarshal)
    end;
  end
  else
    APerson := nil;
end;

function TdormServerSampleClient.LoadPersonByOID(OID: Integer): TPerson;
begin
  if FLoadPersonByOIDCommand = nil then
  begin
    FLoadPersonByOIDCommand := FDBXConnection.CreateCommand;
    FLoadPersonByOIDCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FLoadPersonByOIDCommand.Text := 'TdormServerSample.LoadPersonByOID';
    FLoadPersonByOIDCommand.Prepare;
  end;
  FLoadPersonByOIDCommand.Parameters[0].Value.SetInt32(OID);
  FLoadPersonByOIDCommand.ExecuteUpdate;
  if not FLoadPersonByOIDCommand.Parameters[1].Value.IsNull then
  begin
    FUnMarshal := TDBXClientCommand(FLoadPersonByOIDCommand.Parameters[1].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TPerson(FUnMarshal.UnMarshal(FLoadPersonByOIDCommand.Parameters[1].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FLoadPersonByOIDCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;

function TdormServerSampleClient.Persist(AObject: TObject): Integer;
begin
  if FPersistCommand = nil then
  begin
    FPersistCommand := FDBXConnection.CreateCommand;
    FPersistCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FPersistCommand.Text := 'TdormServerSample.Persist';
    FPersistCommand.Prepare;
  end;
  if not Assigned(AObject) then
    FPersistCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDBXClientCommand(FPersistCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FPersistCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(AObject), True);
      if FInstanceOwner then
        AObject.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FPersistCommand.ExecuteUpdate;
  Result := FPersistCommand.Parameters[1].Value.GetInt32;
end;

procedure TdormServerSampleClient.Delete(AObject: TObject);
begin
  if FDeleteCommand = nil then
  begin
    FDeleteCommand := FDBXConnection.CreateCommand;
    FDeleteCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FDeleteCommand.Text := 'TdormServerSample.Delete';
    FDeleteCommand.Prepare;
  end;
  if not Assigned(AObject) then
    FDeleteCommand.Parameters[0].Value.SetNull
  else
  begin
    FMarshal := TDBXClientCommand(FDeleteCommand.Parameters[0].ConnectionHandler).GetJSONMarshaler;
    try
      FDeleteCommand.Parameters[0].Value.SetJSONValue(FMarshal.Marshal(AObject), True);
      if FInstanceOwner then
        AObject.Free
    finally
      FreeAndNil(FMarshal)
    end
    end;
  FDeleteCommand.ExecuteUpdate;
end;

function TdormServerSampleClient.SearchByName(AName: string): TObjectList<BusinessObjects.TPerson>;
begin
  if FSearchByNameCommand = nil then
  begin
    FSearchByNameCommand := FDBXConnection.CreateCommand;
    FSearchByNameCommand.CommandType := TDBXCommandTypes.DSServerMethod;
    FSearchByNameCommand.Text := 'TdormServerSample.SearchByName';
    FSearchByNameCommand.Prepare;
  end;
  FSearchByNameCommand.Parameters[0].Value.SetWideString(AName);
  FSearchByNameCommand.ExecuteUpdate;
  if not FSearchByNameCommand.Parameters[1].Value.IsNull then
  begin
    FUnMarshal := TDBXClientCommand(FSearchByNameCommand.Parameters[1].ConnectionHandler).GetJSONUnMarshaler;
    try
      Result := TObjectList<BusinessObjects.TPerson>(FUnMarshal.UnMarshal(FSearchByNameCommand.Parameters[1].Value.GetJSONValue(True)));
      if FInstanceOwner then
        FSearchByNameCommand.FreeOnExecute(Result);
    finally
      FreeAndNil(FUnMarshal)
    end
  end
  else
    Result := nil;
end;


constructor TdormServerSampleClient.Create(ADBXConnection: TDBXConnection);
begin
  inherited Create(ADBXConnection);
end;


constructor TdormServerSampleClient.Create(ADBXConnection: TDBXConnection; AInstanceOwner: Boolean);
begin
  inherited Create(ADBXConnection, AInstanceOwner);
end;


destructor TdormServerSampleClient.Destroy;
begin
  FreeAndNil(FDataModuleCreateCommand);
  FreeAndNil(FDataModuleDestroyCommand);
  FreeAndNil(FEchoStringCommand);
  FreeAndNil(FReverseStringCommand);
  FreeAndNil(FGetPeopleCommand);
  FreeAndNil(FLoadRelationsCommand);
  FreeAndNil(FLoadPersonByOIDCommand);
  FreeAndNil(FPersistCommand);
  FreeAndNil(FDeleteCommand);
  FreeAndNil(FSearchByNameCommand);
  inherited;
end;

end.

