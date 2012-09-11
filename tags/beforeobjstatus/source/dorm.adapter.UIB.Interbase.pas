unit dorm.adapter.UIB.Interbase;

interface

uses dorm.adapter.UIB.BaseAdapter, dorm.adapter.UIB.Facade, superobject;

type
  TUIBInterbasePersistStrategy = class(TUIBBaseAdapter)
  public
    function CreateUIBFacade(Conf: ISuperObject): TUIBFacade; override;
    class procedure register;
  end;

  TUIBInterbaseTableSequence = class(TUIBBaseTableSequence)
  public
    class procedure register;
  end;

implementation

class procedure TUIBInterbasePersistStrategy.register;
begin
  //
end;

function TUIBInterbasePersistStrategy.CreateUIBFacade(Conf: ISuperObject)
  : TUIBFacade;
begin
  Result := TUIBFacade.Create('gds32.dll',
    Conf.S['username'],
    Conf.S['password'],
    Conf.S['database_connection_string']
    );
end;

class procedure TUIBInterbaseTableSequence.register;
begin
  //
end;

initialization

TUIBInterbasePersistStrategy.register;
TUIBInterbaseTableSequence.register;

finalization

end.
