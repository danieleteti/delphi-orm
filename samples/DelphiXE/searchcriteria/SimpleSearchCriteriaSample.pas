unit SimpleSearchCriteriaSample;

interface

uses
  dorm.Commons, TypInfo;


type
  TGetAllPersone = class(TdormInterfacedObject, IdormSearchCriteria)
    function GetSQL: String;
    function GetItemClassInfo: PTypeInfo;
  end;

implementation

uses
  dorm.bo.Persona;

{ TGetAllPersone }

function TGetAllPersone.GetItemClassInfo: PTypeInfo;
begin
  Result := TypeInfo(TPersona);
end;

function TGetAllPersone.GetSQL: String;
begin
  Result := 'SELECT * FROM PERSONE ORDER BY LAST_NAME';
end;

end.
