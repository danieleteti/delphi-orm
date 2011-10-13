unit dorm.Finders;

interface

uses
  dorm,
  dorm.Commons,
  dorm.Collections,
  rtti,
  Classes,
  SysUtils,
  TypInfo;

type
  TLimitedQuery = class abstract(TdormInterfacedObject, IdormSearchCriteria)
  private
    FMaxRows: Integer;
    procedure SetMaxRows(const Value: Integer);
  public
    constructor Create; override;
    function GetSQL: string; virtual; abstract;
    function GetItemClassInfo: PTypeInfo; virtual; abstract;
    property MaxRows: Integer read FMaxRows write SetMaxRows;
  end;

  TLikeFilter = class(TdormInterfacedObject, IdormSearchCriteria)
  private
    FFieldName: string;
    FValue: string;
  public
    function GetSQL: string; virtual; abstract;
    function GetItemClassInfo: PTypeInfo; virtual; abstract;
    property FieldName: string read FFieldName write FFieldName;
    property Value: string read FValue write FValue;
  end;

implementation

function FBEscape(const Value: string): string;
begin
  Result := StringReplace(Value, '''', '''''', [rfReplaceAll]);
end;

{ TLimitedQuery }

constructor TLimitedQuery.Create;
begin
  inherited;
  FMaxRows := 50;
end;

procedure TLimitedQuery.SetMaxRows(const Value: Integer);
begin
  FMaxRows := Value;
end;

end.
