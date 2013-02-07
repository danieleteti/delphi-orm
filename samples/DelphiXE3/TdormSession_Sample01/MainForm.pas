unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.Generics.collections,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dorm.Component.Session, dorm.Commons,
  Vcl.ExtCtrls,
  Vcl.StdCtrls, BObjectsU;

type
  TForm2 = class(TForm)
    dormSession1: TdormSession;
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Panel1: TPanel;
    ListBox2: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure dormSession1AfterPersistObject(Sender, AObject: TObject);

  private
    FPeople: TObjectList<TPerson>;
    procedure RefreshList;
    procedure RefreshData;

  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses dorm.ObjectStatus;

{$R *.dfm}

procedure TForm2.Button1Click(Sender: TObject);
var
  People: TObjectList<TPerson>;
  Person: TPerson;
begin
  dormSession1.Open;
  Person := TPerson.Create;
  FPeople.Add(Person);
  Person.FirstName := 'Daniele';
  Person.LastName := 'Teti';
  Person.DateOfBirth := Date - 20000 - Random(10000);
  dormSession1.Persist(Person);
  dormSession1.Close;
  RefreshList;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  dormSession1.Open;
  dormSession1.DeleteAll(TPerson);
  dormSession1.Close;
  RefreshData;
  RefreshList;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  People: TObjectList<TPerson>;
  Person: TPerson;
begin
  Person := TPerson.Create;
  FPeople.Add(Person);
  Person.FirstName := 'Daniele';
  Person.LastName := 'Teti';
  Person.DateOfBirth := Date - 20000 - Random(10000);

  Person := TPerson.Create;
  FPeople.Add(Person);
  Person.FirstName := 'Scott';
  Person.LastName := 'Summer';
  Person.DateOfBirth := Date - 20000 - Random(10000);

  Person := TPerson.Create;
  FPeople.Add(Person);
  Person.FirstName := 'Matt';
  Person.LastName := 'Murdock';
  Person.DateOfBirth := Date - 20000 - Random(10000);

  dormSession1.Open;
  dormSession1.PersistCollection(FPeople);
  dormSession1.Close;

  RefreshList;
end;

procedure TForm2.Button4Click(Sender: TObject);
begin
  RefreshData;
  RefreshList;
end;

procedure TForm2.Button5Click(Sender: TObject);
var
  People: TObjectList<TPerson>;
  Person: TPerson;
begin
  Person := TPerson.Create;
  FPeople.Add(Person);
  Person.FirstName := 'Daniele';
  Person.LastName := 'Teti';
  Person.DateOfBirth := Date - 20000 - Random(10000);

  Person := TPerson.Create;
  FPeople.Add(Person);
  Person.FirstName := 'Scott';
  Person.LastName := 'Summer';
  Person.DateOfBirth := Date - 20000 - Random(10000);

  Person := TPerson.Create;
  FPeople.Add(Person);
  Person.FirstName := 'Matt';
  Person.LastName := 'Murdock';
  Person.DateOfBirth := Date - 20000 - Random(10000);

  RefreshList;
end;

procedure TForm2.Button6Click(Sender: TObject);
begin
  dormSession1.Open;
  dormSession1.PersistCollection(FPeople);
  dormSession1.Close;
  RefreshData;
  RefreshList;
end;

procedure TForm2.dormSession1AfterPersistObject(Sender, AObject: TObject);
begin
  ListBox2.Items.Add('Persist ' + AObject.ClassName + ' with id ' +
    IntToStr(TPerson(AObject).ID));
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  ListBox2.Items.Add('PERSIST HISTORY');
  ListBox2.Items.Add('================');
  FPeople := TObjectList<TPerson>.Create;
  RefreshList;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  FPeople.Free;
end;

procedure TForm2.RefreshData;
begin
  FPeople.Clear;
  dormSession1.Open;
  dormSession1.LoadList<TPerson>(nil, FPeople);
  dormSession1.Close;
end;

procedure TForm2.RefreshList;
var
  Person: TPerson;
  DirtyPeople: Integer;
  PersistedPeople: Integer;
begin
  DirtyPeople := 0;
  PersistedPeople := 0;
  ListBox1.Clear;
  for Person in FPeople do
  begin
    ListBox1.Items.Add(IntToStr(Person.ID) + '] ' + Person.FirstName + ' ' +
      Person.LastName + ' ' + DateToStr(Person.DateOfBirth));
    if Person.ObjStatus in [osDirty] then
      Inc(DirtyPeople);
    if Person.ObjStatus in [osClean] then
      Inc(PersistedPeople);
  end;
  Panel1.Caption :=
    Format('There are %d people loaded from the database and %d people only in the list',
    [PersistedPeople, DirtyPeople])
end;

end.
