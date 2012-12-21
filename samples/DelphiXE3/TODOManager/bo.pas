unit bo;

interface

uses
  dorm,
  dorm.Mappings,
  dorm.Commons;

type

  [Entity('todos')]
  TTodo = class(TdormBaseObject)
  private
    FDone: boolean;
    FTitle: String;
    FDescription: String;
    FDueDate: TDate;
    FID: Integer;
    procedure SetDescription(const Value: String);
    procedure SetDone(const Value: boolean);
    procedure SetDueDate(const Value: TDate);
    procedure SetTitle(const Value: String);
    procedure SetID(const Value: Integer);
  public
    constructor Create; virtual;
    function ToString: String;
    property ID: Integer read FID write SetID;
    property Title: String read FTitle write SetTitle;
    property Description: String read FDescription write SetDescription;
    [Column('DUE_DATE')]
    property DueDate: TDate read FDueDate write SetDueDate;
    property Done: boolean read FDone write SetDone;
  end;

implementation

uses
  System.SysUtils,
  System.StrUtils,
  System.math;

{ TTodo }

constructor TTodo.Create;
begin
  inherited;
  FDueDate := date + 1; // default to tomorrow
end;

procedure TTodo.SetDescription(const Value: String);
begin
  FDescription := Value;
end;

procedure TTodo.SetDone(const Value: boolean);
begin
  FDone := Value;
end;

procedure TTodo.SetDueDate(const Value: TDate);
begin
  FDueDate := Value;
end;

procedure TTodo.SetID(const Value: Integer);
begin
  FID := Value;
end;

procedure TTodo.SetTitle(const Value: String);
begin
  FTitle := Value;
end;

function TTodo.ToString: String;
begin
  Result := Format('%-4d) %-40s %-10s (status: %s) %s',
    [self.ID, self.Title, datetimetostr(self.DueDate), self.ObjStatusAsString,
    ifthen(self.Done, 'OK', '--')]);
end;

end.
