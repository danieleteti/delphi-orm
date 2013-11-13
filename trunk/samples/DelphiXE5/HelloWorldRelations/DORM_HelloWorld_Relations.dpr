program DORM_HelloWorld_Relations;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.Classes,
  dorm,
  dorm.commons,
  dorm.loggers,
  BObjectsU in '..\..\Commons\BObjectsU.pas',
  dorm.ObjectStatus,
  RandomUtilsU in '..\..\Commons\RandomUtilsU.pas';

procedure RelationOneToMany;
var
  dormSession: TSession;
  Person: TPerson;
  Phone1: TPhone;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create('..\..\dorm.conf'), TdormEnvironment.deDevelopment);
  try
    Person := TPerson.Create;
    Person.FirstName := 'John';
    Person.LastName := 'Doe';
    Person.BornDate := EncodeDate(1979, 11, 4);
    Phone1 := TPhone.Create;
    Person.Phones.Add(Phone1);
    Phone1.Number := '555-233454';
    Phone1.Model := 'Samsung Galaxy S4';
    Phone1 := TPhone.Create;
    Person.Phones.Add(Phone1);
    Phone1.Number := '+39 1234567';
    Phone1.Model := 'Nexus 5';
    dormSession.Persist(Person);
    Person.Free;
  finally
    dormSession.Free;
  end;
end;

begin
  RelationOneToMany;
  ReadLn;

end.
