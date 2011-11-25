unit MainForm;

interface

uses
  dorm, dorm.Collections, dorm.Commons, Generics.Collections,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  BusinessObjects, Data.Bind.EngExt, FMX.Bind.DBEngExt, Data.Bind.Components,
  System.Rtti, System.Bindings.Outputs, System.Bindings.Helper,
  FMX.Edit, FMX.Objects,
  FMX.Bind.Editors, ClientModuleUnit; // Used to bind ListBox1 (Editor)

type
  TfrmClientMain = class(TForm)
    ListBox1: TListBox;
    BindingsList1: TBindingsList;
    BindScopePeople: TBindScope;
    BindList1: TBindList;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    BindScopePerson: TBindScope;
    BindExprItems1: TBindExprItems;
    Edit2: TEdit;
    SpinBox1: TSpinBox;
    BindExprItems2: TBindExprItems;
    BindExprItems3: TBindExprItems;
    Rectangle1: TRectangle;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    BindExpression1: TBindExpression;
    Button3: TButton;
    Edit3: TEdit;
    Button4: TButton;
    Label4: TLabel;
    Button5: TButton;
    StyleBook1: TStyleBook;
    ScaledLayout1: TScaledLayout;
    BindScopeLaptops: TBindScope;
    ListBox2: TListBox;
    BindList2: TBindList;
    Label5: TLabel;
    Label6: TLabel;
    Button6: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    Person: TPerson;
    People: TObjectList<TPerson>;
  public
    { Public declarations }
  end;

var
  frmClientMain: TfrmClientMain;

implementation

{$R *.fmx}


procedure TfrmClientMain.FormCreate(Sender: TObject);
begin
  // When are on the client, I want to manage the objects lifecycle
  ClientProxy.InstanceOwner := False;
  People := ClientProxy.dormServerSampleClient.GetPeople;
  BindScopePeople.DataObject := People;
  BindScopePeople.Active := True;
  BindList1.FillList;
end;

procedure TfrmClientMain.Button1Click(Sender: TObject);
begin
  BindList1.FillList;
end;

procedure TfrmClientMain.Button2Click(Sender: TObject);
begin
  BindScopePerson.Active := False;
  BindScopePeople.Active := False;
  FreeAndNil(Person);
  Person := TPerson.Create('...', '...', 20);
  People.Add(Person);
  BindList1.FillList;
  ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  BindScopePerson.DataObject := Person;
  BindScopeLaptops.DataObject := Person.Laptops;
  BindScopePerson.Active := True;
  BindScopePeople.Active := True;
end;

procedure TfrmClientMain.Button3Click(Sender: TObject);
begin
  People[ListBox1.ItemIndex].ID :=
    ClientProxy.dormServerSampleClient.Persist(BindScopePerson.DataObject);
  BindList1.FillList;
end;

procedure TfrmClientMain.Button4Click(Sender: TObject);
var
  Criteria: TdormCriteria;
begin
  BindScopePeople.Active := False;
  BindScopePerson.Active := False;
  FreeAndNil(People);
  People := ClientProxy.dormServerSampleClient.SearchByName(Edit3.Text);
  BindScopePeople.DataObject := People;
  BindScopePeople.Active := True;
  BindList1.FillList;
end;

procedure TfrmClientMain.Button5Click(Sender: TObject);
begin
  BindScopePerson.Active := False;
  BindScopeLaptops.Active := False;
  ClientProxy.dormServerSampleClient.Delete(BindScopePerson.DataObject);
  People.Delete(ListBox1.ItemIndex);
  BindScopePerson.DataObject := nil;
  BindScopeLaptops.DataObject := nil;
  BindScopePerson.Active := True;
  BindScopeLaptops.Active := True;
  BindList1.FillList;
end;

procedure TfrmClientMain.Button6Click(Sender: TObject);
var
  res: TArray<String>;
  l: TLaptop;
begin
  SetLength(res, 3);
  if InputQuery('New Laptop', ['Model', 'RAM [MB]', 'Cores'], res) then
  begin
    l := TLaptop.Create(res[0], strtoint(res[1]), strtoint(res[2]));
    TPerson(BindScopePerson.DataObject).Laptops.Add(l);
    l.Owner := People[ListBox1.ItemIndex] as TPerson;
    ClientProxy.dormServerSampleClient.Persist(l);
    BindList2.FillList;
  end;
end;

procedure TfrmClientMain.Edit1Exit(Sender: TObject);
begin
  TBindings.Notify(Sender, 'Text');
end;

procedure TfrmClientMain.FormDestroy(Sender: TObject);
begin
  BindScopePerson.Active := False;
  BindScopePeople.Active := False;
  FreeAndNil(Person);
  People.Free;
end;

procedure TfrmClientMain.ListBox1Click(Sender: TObject);
var
  PersonOID: Integer;
begin
  if ListBox1.ItemIndex > -1 then
  begin
    PersonOID := People[ListBox1.ItemIndex].ID;
    BindScopePerson.Active := False;
    BindScopeLaptops.Active := False;
    FreeAndNil(Person);
    Person := ClientProxy.dormServerSampleClient.LoadPersonByOID(PersonOID);
    BindScopePerson.DataObject := Person; // People[ListBox1.ItemIndex];
    BindScopeLaptops.DataObject := Person.Laptops;
    BindScopePerson.Active := True;
    BindScopeLaptops.Active := True;
    BindList2.FillList;
  end;
end;

procedure TfrmClientMain.SpinBox1Change(Sender: TObject);
begin
  TBindings.Notify(Sender, 'Value');
end;

end.
