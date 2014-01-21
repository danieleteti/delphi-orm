unit dorm.adapter.FireDAC.SQLServer;

interface

uses
  dorm.adapter.FireDAC.BaseAdapter,
  dorm.adapter.FireDAC.Facade,
  FireDAC.Phys.MSSQL,
  superobject;

type
  TFireDACSQLServerPersistStrategy = class(TFireDACBaseAdapter)
  private
    FDriverLink: TFDPhysMSSQLDriverLink;
  protected
    // SqlServer does not support boolean field. So boolean value are mapped to 0 = false and 1 = true
    function GetBooleanValueAsString(Value: Boolean): String; override;
  public
    destructor Destroy; override;
    function CreateFireDACFacade(Conf: ISuperObject): TFireDACFacade; override;
    class procedure register;
  end;

implementation

destructor TFireDACSQLServerPersistStrategy.Destroy;
begin
  if Assigned(FDriverLink) then FDriverLink.Free;
  inherited;
end;

function TFireDACSQLServerPersistStrategy.GetBooleanValueAsString
  (Value: Boolean): String;
begin
  if Value then
    Result := '1'
  else
    Result := '0';
end;

class procedure TFireDACSQLServerPersistStrategy.register;
begin
  //
end;

function TFireDACSQLServerPersistStrategy.CreateFireDACFacade(Conf: ISuperObject)
  : TFireDACFacade;
begin
  FDriverLink := TFDPhysMSSQLDriverLink.Create(nil);
  Result := TFireDACFacade.Create(Conf.S['database_connection_string'],FDriverLink);
end;

initialization

TFireDACSQLServerPersistStrategy.register;

finalization

end.
