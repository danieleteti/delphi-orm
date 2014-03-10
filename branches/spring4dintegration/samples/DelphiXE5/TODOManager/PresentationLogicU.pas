unit PresentationLogicU;

interface

uses
  Data.Bind.Grid, bo, system.Generics.collections;
function NeedsToDrawCheck(ARow: Integer;
  ALinkGridToDataSourceColumn: TLinkGridToDataSourceColumn;
  ATodos: TObjectList<TTodo>): boolean;

implementation

function NeedsToDrawCheck(ARow: Integer;
  ALinkGridToDataSourceColumn: TLinkGridToDataSourceColumn;
  ATodos: TObjectList<TTodo>): boolean;
begin
  Result := (ARow > 0) and (ATodos[ARow - 1].Done) and
    (ALinkGridToDataSourceColumn.MemberName = 'Done');
end;

end.
