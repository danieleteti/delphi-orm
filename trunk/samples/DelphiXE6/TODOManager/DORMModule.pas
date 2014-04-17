unit DORMModule;

interface

uses
  System.SysUtils,
  System.Classes,
  dorm;

type
  TMainDORM = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  strict private
    FSession: TSession;
  private

  public
    function GetDORMSession: TSession;
    procedure CreateSomeData;
  end;

var
  MainDORM: TMainDORM;

function GetDORMSession: TSession;

implementation

uses
  math,
  bo,
  dorm.Commons;

{ %CLASSGROUP 'System.Classes.TPersistent' }

{$R *.dfm}


function FakeTodoTitles: TArray<String>;
begin
  SetLength(Result, 10);
  Result[0] := 'Complete DORM documentations';
  Result[1] := 'Create some DORM demos';
  Result[2] := 'Spread the word about DORM';
  Result[3] := 'Write an article for some magazine';
  Result[4] := 'Eat a lot of sushi!!';
  Result[5] := 'Speak about DORM on facebook';
  Result[6] := 'Speak about DORM on twitter';
  Result[7] := 'Speak about DORM on Google+';
  Result[8] := 'Create a better logo';
  Result[9] := 'Do some cool public applications using DORM';
end;

function GetDORMSession: TSession;
begin
  Result := MainDORM.GetDORMSession;
end;

procedure TMainDORM.CreateSomeData;
var
  todo: TTodo;
  I: Integer;
  Titles: TArray<string>;
begin
  Titles := FakeTodoTitles;
  FSession.DeleteAll(TTodo);
  Randomize;
  todo := TTodo.Create;
  try
    for I := 1 to 10 do
    begin
      FSession.ClearOID(todo); // sets also ObjStatus to osDirty
      todo.Title := Titles[RandomRange(0, 9)];
      todo.Description := 'This is a description about a todo with the title "' + todo.Title + '"';
      todo.DueDate := Date + RandomRange(20, 50);
      todo.Done := RandomRange(1, 100) mod 2 = 0;
      FSession.Persist(todo); // insert
    end;
  finally
    todo.Free;
  end;

end;

procedure TMainDORM.DataModuleCreate(Sender: TObject);
begin
  FSession := TSession.CreateConfigured(
    TStreamReader.Create('..\..\dorm.conf'), TdormEnvironment.deDevelopment);
  FSession.StartTransaction;
  if FSession.Count(TTodo) = 0 then
    CreateSomeData;
  FSession.Commit(true);
end;

procedure TMainDORM.DataModuleDestroy(Sender: TObject);
begin
  FSession.Free;
end;

function TMainDORM.GetDORMSession: TSession;
begin
  Result := FSession;
end;

end.
