unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dorm.Component.Session, Vcl.StdCtrls;

type
  TForm2 = class(TForm)
    dormSession1: TdormSession;
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses BObjectsU, System.Generics.Collections;

procedure TForm2.Button1Click(Sender: TObject);
var
  People: TObjectList<TPerson>;
  Person: TPerson;
begin
  dormSession1.Open;
  Person := TPerson.Create;
  Person.FirstName := 'Daniele';
  Person.LastName := 'Teti';
  Person.DateOfBirth := Date - 20000 - Random(10000);
  dormSession1.Persist(Person);
  Person.Free;
  People := dormSession1.LoadList<TPerson>;
  ListBox1.Clear;
  for Person in People do
  begin
    ListBox1.Items.Add(IntToStr(Person.ID) + '] ' + Person.FirstName + ' ' +
      Person.LastName + ' ' + DateToStr(Person.DateOfBirth));
  end;
  People.Free;
  dormSession1.Close;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  dormSession1.Open;
  dormSession1.DeleteAll(TPerson);
  dormSession1.Close;
  ListBox1.Clear;
end;

end.
