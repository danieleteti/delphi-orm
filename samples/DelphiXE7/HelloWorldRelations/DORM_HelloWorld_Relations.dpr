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

{$IFDEF LINK_SQLSERVERFIREDAC_ADAPTER}


const
  CONFIG_FILE = '..\..\dorm_sqlserver_firedac.conf';
{$ENDIF}
{$IFNDEF LINK_SQLSERVERFIREDAC_ADAPTER}


const
  CONFIG_FILE = '..\..\dorm.conf';
{$ENDIF}

  // retireve and print a person
procedure PrintPerson(dormSession: TSession; const ID: Integer);
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

procedure PrintFather(dormSession: TSession; const ID: Integer);
var
  Father: TFather;
  phone: TPhone;
begin
  // retrieve the objects graph
  Father := dormSession.Load<TFather>(ID);
  Writeln('** FATHER: ' + Father.ToString);
  Writeln('**** CHILD: ' + Father.Child.ToString);
  Writeln;
  Writeln('** FATHER''s PHONES: ');
  for phone in Father.Phones do
    Writeln(' --> ', phone.ToString);
  Father.Free;
end;

procedure RelationOneToMany;
var
  dormSession: TSession;
  Person: TPerson;
  Phone1: TPhone;
  ID: Integer;
  phone: TPhone;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
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
    PrintPerson(dormSession, ID);
    Writeln('');
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
    PrintPerson(dormSession, ID);
  finally
    dormSession.Free;
  end;
end;

procedure RelationOneToOne;
var
  dormSession: TSession;
  Father: TFather;
  Phone1: TPhone;
  ID: Integer;
  phone: TPhone;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    // the main object
    Father := TFather.Create;
    Father.FirstName := 'John';
    Father.LastName := 'Doe';
    Father.BornDate := EncodeDate(1979, 11, 4);

    // the hasone relation
    Father.Child.FirstName := 'Tim';
    Father.Child.LastName := 'Doe';
    Father.Child.BornDate := EncodeDate(2010, 11, 4);

    // first phone
    Phone1 := TPhone.Create;
    Father.Phones.Add(Phone1);
    Phone1.Number := '+43 5552454';
    Phone1.Model := 'Samsung Galaxy S4';

    // second phone
    Phone1 := TPhone.Create;
    Father.Phones.Add(Phone1);
    Phone1.Number := '+39 1234567';
    Phone1.Model := 'Nexus 4';

    dormSession.Persist(Father); // persists main object and all its childs
    ID := Father.ID;
    Father.Free;
    dormSession.Commit(true);

    Writeln('FIRST VERSION ****************');
    PrintFather(dormSession, ID);
    Writeln('');

    // reload the father and do some changes
    Father := dormSession.Load<TFather>(ID);

    // change child's name
    Father.Child.FirstName := 'Frank';

    // add and update a phone
    Father.Phones[0].Model := 'CHANGED MODEL';
    Father.Phones[1].Number := 'CHANGED NUMBER';

    // add another phone
    phone := TPhone.Create;
    phone.Model := 'Nexus 5';
    phone.Number := '+39 8745241';
    Father.Phones.Add(phone);

    // save all changes
    dormSession.Persist(Father);
    Father.Free;

    Writeln('SECOND VERSION ***************');
    PrintFather(dormSession, ID);
  finally
    dormSession.Free;
  end;
end;

var
  K: char;

begin
  try
    repeat
      ClrScr;
      Writeln('1. One to Many relation sample');
      Writeln('2. One to One relation sample');
      Writeln('0. Quit');
      while true do
      begin
        K := ReadKey;
        if K in ['0', '1', '2'] then
          Break;
      end;

      case K of
        '0':
          Break;
        '1':
          begin
            ClrScr;
            RelationOneToMany;
            Writeln;
            Write('Hit return to terminate this sample');
            ReadLn;
          end;
        '2':
          begin
            ClrScr;
            RelationOneToOne;
            Writeln;
            Write('Hit return to terminate this sample');
            ReadLn;
          end;
      end;
    until false;
  except
    on E: Exception do
      Writeln(E.ClassName + ' ' + E.Message);
  end;
  Writeln('See you... hit return to terminate');
  ReadLn;

end.
