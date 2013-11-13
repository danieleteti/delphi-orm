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
  RandomUtilsU in '..\..\Commons\RandomUtilsU.pas',
  Console in '..\..\Commons\Console.pas';

procedure RelationOneToMany;
var
  dormSession: TSession;
  Person: TPerson;
  Phone1: TPhone;
  ID: Integer;
  phone: TPhone;

  // retireve and print a person
  procedure PrintPerson(const ID: Integer);
  var
    Person: TPerson;
    phone: TPhone;
  begin
    // retrieve the objects graph
    Person := dormSession.Load<TPerson>(ID);
    Writeln(Person.ToString);
    for phone in Person.Phones do
      Writeln(phone.ToString);
    Person.Free;
  end;

begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create('..\..\dorm.conf'), TdormEnvironment.deDevelopment);
  try
    // the main object
    Person := TPerson.Create;
    Person.FirstName := 'John';
    Person.LastName := 'Doe';
    Person.BornDate := EncodeDate(1979, 11, 4);

    // first phone
    Phone1 := TPhone.Create;
    Person.Phones.Add(Phone1);
    Phone1.Number := '+43 5552454';
    Phone1.Model := 'Samsung Galaxy S4';

    // second phone
    Phone1 := TPhone.Create;
    Person.Phones.Add(Phone1);
    Phone1.Number := '+39 1234567';
    Phone1.Model := 'Nexus 4';

    // third phone
    Phone1 := TPhone.Create;
    Person.Phones.Add(Phone1);
    Phone1.Number := '+39 88774578';
    Phone1.Model := 'HTC ONE';

    dormSession.Persist(Person); // persists main object and all its childs
    ID := Person.ID;
    Person.Free;

    Writeln('FIRST VERSION ****************');
    PrintPerson(ID);
    WriteLn('');
    // add and update a phone
    Person := dormSession.Load<TPerson>(ID);
    Person.Phones[0].Model := 'CHANGED MODEL';
    Person.Phones[1].Number := 'CHANGED NUMBER';

    // add another phone
    phone := TPhone.Create;
    phone.Model := 'Nexus 5';
    phone.Number := '+39 8745241';
    Person.Phones.Add(phone);

    // save all changes
    dormSession.Persist(Person);
    Person.Free;

    Writeln('SECOND VERSION ***************');
    PrintPerson(ID);
  finally
    dormSession.Free;
  end;
end;

begin
  RelationOneToMany;
  ReadLn;

end.
