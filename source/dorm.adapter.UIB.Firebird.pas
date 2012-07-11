unit dorm.adapter.UIB.Firebird;

interface

uses dorm.adapter.UIB.BaseAdapter, dorm.adapter.UIB.Facade, superobject;

type
  TUIBFirebirdPersistStrategy = class(TUIBBaseAdapter)
  public
    function CreateUIBFacade(Conf: ISuperObject): TUIBFacade; override;
    class procedure register;
  end;

  TUIBFirebirdTableSequence = class(TUIBBaseTableSequence)
  public
    class procedure register;
  end;

implementation

class procedure TUIBFirebirdPersistStrategy.register;
begin
  //
end;

function TUIBFirebirdPersistStrategy.CreateUIBFacade(Conf: ISuperObject)
  : TUIBFacade;
begin
  Result := TUIBFacade.Create('fbclient.dll',
    Conf.S['username'],
    Conf.S['password'],
    Conf.S['database_connection_string']
    );
end;

class procedure TUIBFirebirdTableSequence.register;
begin
  //
end;

initialization

TUIBFirebirdPersistStrategy.register;
TUIBFirebirdTableSequence.register;

finalization

end.
