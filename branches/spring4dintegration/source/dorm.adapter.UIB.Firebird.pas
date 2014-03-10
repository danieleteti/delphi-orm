unit dorm.adapter.UIB.Firebird;

interface

uses dorm.adapter.UIB.BaseAdapter, dorm.adapter.UIB.Facade, superobject;

type
  TUIBFirebirdPersistStrategy = class(TUIBBaseAdapter)
  protected
    // firebird does not support boolean field. So boolean value are mapped to 0 = false and 1 = true
    function GetBooleanValueAsString(Value: Boolean): String; override;
  public
    function CreateUIBFacade(Conf: ISuperObject): TUIBFacade; override;
    class procedure register;
  end;

  TUIBFirebirdTableSequence = class(TUIBBaseTableSequence)
  protected
    function GetSequenceFormatTemplate: String; override;
  public
    class procedure register;
  end;

  TUIBFirebirdTableGenerator = class(TUIBBaseTableSequence)
  protected
    function GetSequenceFormatTemplate: String; override;
  public
    class procedure register;
  end;

implementation

function TUIBFirebirdPersistStrategy.GetBooleanValueAsString
  (Value: Boolean): String;
begin
  if Value then
    Result := '1'
  else
    Result := '0';
end;

class procedure TUIBFirebirdPersistStrategy.register;
begin
  //
end;

function TUIBFirebirdPersistStrategy.CreateUIBFacade(Conf: ISuperObject)
  : TUIBFacade;
begin
  Result := TUIBFacade.Create('fbclient.dll', Conf.S['username'],
    Conf.S['password'], Conf.S['database_connection_string']);
end;

function TUIBFirebirdTableSequence.GetSequenceFormatTemplate: String;
begin
  Result := 'SEQ_%s_ID';
end;

class procedure TUIBFirebirdTableSequence.register;
begin
  // do nothing
end;

{ TUIBFirebirdTableGenerator }

function TUIBFirebirdTableGenerator.GetSequenceFormatTemplate: String;
begin
  Result := 'GEN_%s_ID';
end;

class procedure TUIBFirebirdTableGenerator.register;
begin
  // do nothing
end;

initialization

TUIBFirebirdPersistStrategy.register;
TUIBFirebirdTableSequence.register;
TUIBFirebirdTableGenerator.register;

finalization

end.
