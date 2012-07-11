unit BusinnessObjects;

interface

type
  
  TViewPeopleSummary = class(TObject)
    //fields
    Id: Integer;
    FirstName: String;
    LastName: String;
    Age: Integer;
    BornDate: TDate;
    BornDateTime: TDateTime;
    Photo: TStream;
    EmailsCount: Integer;
  end;
  
  TEmail = class(TObject)
    //primary key
    Id: Integer;
    //fields
    Address: String;
    IdPerson: Integer;
  end;
  
  TPeople = class(TObject)
    //primary key
    Id: Integer;
    //fields
    FirstName: String;
    LastName: String;
    Age: Integer;
    BornDate: TDate;
    BornDateTime: TDateTime;
    Photo: TStream;
  end;
  
  TPhone = class(TObject)
    //primary key
    Id: Integer;
    //fields
    Number: String;
    Model: String;
    IdPerson: Integer;
  end;
  
  TCar = class(TObject)
    //primary key
    Id: Integer;
    //fields
    Brand: String;
    Model: String;
    IdPerson: Integer;
  end;
implementation


initialization

finalization

end.
