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
    function CheckServerVersion(const Version: string): boolean;
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
  System.SysUtils,
  dorm.Commons
  ;


const
  FirebirdSQLServerVersions: array[0..5] of string = (
    '1.0',
    '1.5',
    '2.0',
    '2.1',
    '2.5',
    '3.0'
  );



{ TZeosDBOFirebirdPersistStrategy }

function TZeosDBOFirebirdPersistStrategy.CheckServerVersion(
  const Version: string): boolean;
var
  I: integer;
  Found: Boolean;
begin
  Result := False;

  I := 0;
  Found := False;

  while (not Found) and (I < Length(FirebirdSQLServerVersions)) do
  begin
    Found := FirebirdSQLServerVersions[I] = Version;
    Inc(I);
  end;

  Result := Found;
end;

function TZeosDBOFirebirdPersistStrategy.CreateZeosDBOFacade(
  Conf: ISuperObject): TZeosDBOFacade;
var
  CharSet,
  Protocol,
  Properties: String;
  ServerVersion: string;
begin
  ServerVersion := Conf.S['server_version'];

  if not CheckServerVersion(ServerVersion) then
    raise EdormException.CreateFmt('Invalid FirebirdSQL server version %s.' + #13#10 +
      'Unable to create ZeosDBOFacade.', [ServerVersion]);

  Protocol := 'firebird-' + ServerVersion;
  CharSet := Trim(LowerCase(Conf.S['charset']));

  // Charset can be set in config charset property or into config properties property
  if CharSet = '' then
    CharSet := 'UTF8';

  Properties := Conf.S['database_properties'];

  if Properties <> EmptyStr then
    Properties := 'CodePage=' + Charset + ';' + Properties
  else
    Properties := 'CodePage=' + Charset;

  Result := TZeosDBOFacade.Create(
    Protocol,
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
