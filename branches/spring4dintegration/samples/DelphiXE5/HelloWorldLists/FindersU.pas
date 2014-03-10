unit FindersU;

interface

uses
  dorm.Commons, dorm.Filters, System.TypInfo;

type
  TPeopleFinder = class(TdormCriteria, ICustomCriteria)
  public
    function GetSQL: string;
    function GetItemClassInfo: PTypeInfo;
  end;

implementation

{ TPeopleFinder }

uses BObjectsU;

function TPeopleFinder.GetItemClassInfo: PTypeInfo;
begin
  Result := TypeInfo(TCustomerOS);
end;

function TPeopleFinder.GetSQL: string;
begin
  Result := 'SELECT * FROM CUSTOMERS_OS WHERE CREATED_AT BETWEEN ''2013-01-01'' AND ''2013-06-30'' ORDER BY CREATED_AT';
end;

end.
