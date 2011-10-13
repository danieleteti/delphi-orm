unit BaseTestCase;

interface

uses
  TestFramework,
  dorm,
  dorm.Collections;

type
  TBaseTestCase = class(TTestCase)
  protected
    Session: dorm.TSession;
    function GetDORMConfigFileName: String; virtual; abstract;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

implementation

uses
  ioutils,
  classes,
  dorm.Commons;

procedure TBaseTestCase.SetUp;
begin
  inherited;
  Session := TSession.Create(deTest);
  Session.Configure(TStreamReader.Create(GetDORMConfigFileName));
  Session.StartTransaction;
end;

procedure TBaseTestCase.TearDown;
begin
  Session.Free;
  inherited;
end;

end.
