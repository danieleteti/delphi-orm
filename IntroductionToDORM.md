# Introduction to DORM #
Let say that you have these objects (PODOs)
```
TPerson = class(TObject)
  private
    FLastName: string;
    FAge: Int32;
    FFirstName: string;
    FID: UInt8;
  public
    constructor Create(FirstName, LastName: String; Age: UInt8);
    property ID: Integer read FID write SetID;
    property FirstName: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property Age: Int32 read FAge write FAge;
  end;

TManager = class(TPerson)
  private 
    FBenefits: String;
  public
    property Benefits: String read FBenefits write FBenefits;
  end;
```

The following code can insert and update objects state from a database

```
var 
  person: TPerson; oid: Integer;
begin
  person := TPerson.Create(‘Daniele’,’Teti’,32);
  Session.Save(person);
  oid := person.id;
  person.Free;
  person := Session.Load<TPerson>(oid);
  WriteLn(‘Person name is: ‘, person.FirstName);
  person.Free;
end;
```

The following code, update and delete an object representation from the database

```
var 
  person: TPerson; oid: Integer;
begin
  person := Session.Load<TPerson>(1);  //"1" is the Object's identifier
  try
    person.FirstName := ‘Daniele’;
    person.LastName := ‘Teti’;
    Session.Update(person);
    Session.Delete(person);
  finally 
    person.Free; 
  end;
end;
```