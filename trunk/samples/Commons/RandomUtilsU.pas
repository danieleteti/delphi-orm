unit RandomUtilsU;

interface

const
  FirstNames: array [0 .. 9] of string = (
    'Daniele',
    'Debora',
    'Mattia',
    'Jack',
    'James',
    'William',
    'Joseph',
    'David',
    'Charles',
    'Thomas'
    );

  LastNames: array [0 .. 9] of string = (
    'Smith',
    'JOHNSON',
    'Williams',
    'Brown',
    'Jones',
    'Miller',
    'Davis',
    'Wilson',
    'Martinez',
    'Anderson'
    );

  Countries: array [0 .. 9] of string = (
    'Italy',
    'New York',
    'Illinois',
    'Arizona',
    'Nevada',
    'UK',
    'France',
    'Germany',
    'Norway',
    'California'
    );

function RandomFirstName: String;
function RandomLastName: String;
function RandomCountry: String;

implementation

function RandomCountry: String;
begin
  Result := Countries[Random(10)];
end;

function RandomFirstName: String;
begin
  Result := FirstNames[Random(10)];
end;

function RandomLastName: String;
begin
  Result := LastNames[Random(10)];
end;

end.
