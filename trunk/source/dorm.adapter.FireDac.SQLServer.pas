unit dorm.adapter.FireDac.SQLServer;

interface

uses dorm.adapter.FireDac.BaseAdapter, dorm.adapter.FireDac.Facade, superobject,
      uADPhysODBCBase, uADPhysMSSQL, Classes;

type
  TFireDacSQLServerPersistStrategy = class(TFireDacBaseAdapter)
  protected
    function CalculatePrimaryKey: Boolean; override;
  public
    function CreateFireDacFacade(Conf: ISuperObject): TFireDacFacade; override;
    class procedure register;
  end;

  TFireDacSQLServerTableSequence = class(TFireDacBaseTableSequence)
  private
  public
    function NewIntegerKey(const Entity: string): UInt64; override;
    class procedure register;
  end;

implementation

class procedure TFireDacSQLServerPersistStrategy.register;
begin
  //
end;

function TFireDacSQLServerPersistStrategy.CalculatePrimaryKey: Boolean;
begin
   Result:=False;
end;

function TFireDacSQLServerPersistStrategy.CreateFireDacFacade(Conf: ISuperObject)
  : TFireDacFacade;
var Params: TStringList;
begin
  Params:=TStringList.Create;
  try
    Params.Values['SERVER']:=Conf.S['hostname'];
    Params.Values['User_Name']:=Conf.S['username'];
    Params.Values['Password']:=Conf.S['password'];
    Params.Values['Database']:=Conf.S['database_connection_string'];
    Params.Values['DriverID']:='MSSQL';
    Result := TFireDacFacade.Create(Params);
  finally
    Params.Free;
  end;
end;

function TFireDacSQLServerTableSequence.NewIntegerKey(const Entity: string): UInt64;
begin
  Result := FPersistStrategy.ExecuteAndGetFirst('SELECT @@IDENTITY');
end;

class procedure TFireDacSQLServerTableSequence.register;
begin
  // do nothing
end;

initialization

TFireDacSQLServerPersistStrategy.register;
TFireDacSQLServerTableSequence.register;

finalization

end.
