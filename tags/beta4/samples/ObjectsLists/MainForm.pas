unit MainForm;

interface

uses
  dorm, dorm.Collections, dorm.Commons,
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.ListBox,
  BusinessObjects, Data.Bind.EngExt, FMX.Bind.DBEngExt, Data.Bind.Components,
  System.Rtti, System.Bindings.Outputs, System.Bindings.Helper,
  FMX.Edit, FMX.Objects,
  FMX.Bind.Editors; // Used to bind ListBox1 (Editor)

type
  TForm11 = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure SpinBox1Change(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    People: TdormCollection;
    Session: TSession;
  public
    { Public declarations }
  end;

var
  Form11: TForm11;

implementation

uses
  Generics.Collections;

{$R *.fmx}

procedure TForm11.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Session.Free;
end;

procedure TForm11.FormCreate(Sender: TObject);
begin
  Session := TSession.CreateConfigured(TStreamReader.Create('dorm.conf'),
    deDevelopment);
  People := Session.ListAll<TPerson>;
{$REGION 'Insert some data'}
//  People := NewList;
//  Session.DeleteAll(TPerson);
//  People.Add(TPerson.Create('Daniele', 'Teti', 32));
//  People.Add(TPerson.Create('Scott', 'Summers', 40));
//  People.Add(TPerson.Create('Bruce', 'Banner', 50));
//  People.Add(TPerson.Create('Sue', 'Storm', 35));
//  People.Add(TPerson.Create('Peter', 'Parker', 17));
//  Session.InsertCollection(People);
{$ENDREGION}
  BindScopePeople.DataObject := People;
  BindScopePeople.Active := True;
  BindList1.FillList;
end;

procedure TForm11.Button1Click(Sender: TObject);
begin
  BindList1.FillList;
end;

procedure TForm11.Button2Click(Sender: TObject);
begin
  BindScopePerson.Active := False;
  BindScopePerson.DataObject := TPerson.Create('...', '...', 20);
  People.Add(BindScopePerson.DataObject as TPerson);
  BindList1.FillList;
  ListBox1.ItemIndex := ListBox1.Items.Count - 1;
end;

procedure TForm11.Button3Click(Sender: TObject);
begin
  Session.Persist(BindScopePerson.DataObject);
  BindList1.FillList;
end;

procedure TForm11.Button4Click(Sender: TObject);
var
  Criteria: TdormCriteria;
begin
  BindScopePeople.Active := False;
  FreeAndNil(People);
  if Edit3.Text = EmptyStr then
    People := Session.ListAll<TPerson>
  else
  begin
    Criteria := TdormCriteria.NewCriteria('FirstName',
      TdormCompareOperator.Equal, Edit3.Text);
    People := Session.List<TPerson>(Criteria);
  end;
  BindScopePeople.DataObject := People;
  BindList1.FillList;
end;

procedure TForm11.Button5Click(Sender: TObject);
begin
  BindScopePerson.Active := False;
  Session.Delete(People.Extract(BindScopePerson.DataObject));
  BindScopePerson.DataObject := nil;
  BindList1.FillList;
end;

procedure TForm11.Edit1Exit(Sender: TObject);
begin
  TBindings.Notify(Sender, 'Text');
end;

procedure TForm11.FormDestroy(Sender: TObject);
begin
  People.Free;
end;

procedure TForm11.ListBox1Click(Sender: TObject);
begin
  if ListBox1.ItemIndex > -1 then
  begin
    BindScopePerson.Active := False;
    BindScopePerson.DataObject := People[ListBox1.ItemIndex];
  end;
end;

procedure TForm11.SpinBox1Change(Sender: TObject);
begin
  TBindings.Notify(Sender, 'Value');
end;

end.
