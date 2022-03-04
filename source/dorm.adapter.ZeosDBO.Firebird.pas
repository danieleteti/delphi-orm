unit dorm.adapter.ZeosDBO.Firebird;

interface

uses
  superobject,
  dorm.adapter.ZeosDBO.BaseAdapter,
  dorm.adapter.ZeosDBO.Facade
  ;



type
  TZeosDBOFirebirdPersistStrategy = class(TZeosDBOBaseAdapter)
  protected
    // firebird does not support boolean field. So boolean value are mapped to 0 = false and 1 = true
    function GetBooleanValueAsString(Value: Boolean): String; override;
  public
    class procedure register;

    function CreateZeosDBOFacade(Conf: ISuperObject): TZeosDBOFacade; override;
  end;

  TZeosDBOFirebirdTableSequence = class(TZeosDBOBaseTableSequence)
  protected
    function GetSequenceFormatTemplate: String; override;
  public
    class procedure register;
  end;

  TZeosDBOFirebirdTableGenerator = class(TZeosDBOBaseTableSequence)
  protected
    function GetSequenceFormatTemplate: String; override;
  public
    class procedure register;
  end;

implementation

uses
  System.SysUtils;



{ TZeosDBOFirebirdPersistStrategy }

function TZeosDBOFirebirdPersistStrategy.CreateZeosDBOFacade(
  Conf: ISuperObject): TZeosDBOFacade;
var
  CharSet,
  Properties: String;
begin
  CharSet := Trim(LowerCase(Conf.S['charset']));

  // Charset can be set in config charset property or into config properties property
  if CharSet = '' then
    CharSet := 'UTF8';

  Properties := Conf.S['properties'];

  Properties := 'CodePage=' + Charset + ';' + Properties;

  Result := TZeosDBOFacade.Create(
    Conf.S['protocol'],
    Conf.S['username'],
    Conf.S['password'],
    Conf.S['database_connection_string'],
    Properties
  );
end;

function TZeosDBOFirebirdPersistStrategy.GetBooleanValueAsString(
  Value: Boolean): String;
begin
  if Value then
    Result := '1'
  else
    Result := '0';
end;

class procedure TZeosDBOFirebirdPersistStrategy.register;
begin
  // do nothing
end;

{ TZeosDBOFirebirdTableSequence }

function TZeosDBOFirebirdTableSequence.GetSequenceFormatTemplate: String;
begin
  Result := 'SEQ_%s_ID';
end;

class procedure TZeosDBOFirebirdTableSequence.register;
begin
  // do nothing
end;

{ TZeosDBOFirebirdTableGenerator }

function TZeosDBOFirebirdTableGenerator.GetSequenceFormatTemplate: String;
begin
  Result := 'GEN_%s_ID';
end;

class procedure TZeosDBOFirebirdTableGenerator.register;
begin
  // do nothing
end;


initialization

  TZeosDBOFirebirdPersistStrategy.register;
  TZeosDBOFirebirdTableSequence.register;
  TZeosDBOFirebirdTableGenerator.register;

finalization

end.
