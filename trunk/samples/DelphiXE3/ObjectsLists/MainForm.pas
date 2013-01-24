unit MainForm;

interface

uses
  Data.Bind.EngExt,
  Fmx.Bind.DBEngExt,
  System.Rtti,
  System.Bindings.Outputs,
  Data.Bind.Components,
  Fmx.Edit,
  Fmx.ListBox,
  Fmx.Objects,
  Generics.Collections,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Fmx.Types,
  Fmx.Controls,
  Fmx.Forms,
  Fmx.Dialogs,
  Fmx.Layouts,
  BusinessObjects,
  System.Bindings.Helper,
  Fmx.Bind.Editors, // Used to bind ListBox1 (Editor)
  dorm,
  dorm.adapters, // includes all the available adapters
  dorm.Collections,
  dorm.Commons,
  dorm.Configuration,
  dorm.Loggers,
  dorm.Filters;

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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    People: TObjectList<TPerson>;
    Session: TSession;
    procedure InitializeData;
  public
    { Public declarations }
  end;

var
  Form11: TForm11;

implementation

uses
  dorm.ObjectStatus;

{$R *.fmx}

procedure TForm11.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Session.Free;
end;

procedure TForm11.FormCreate(Sender: TObject);
var
  ConfigFileName: string;
begin
{$IFDEF SQLITE3_STRATEGY}
  ConfigFileName := 'dorm_sqlite3.conf';
{$ENDIF}
{$IFDEF INTERBASE_STRATEGY}
  ConfigFileName := 'dorm_interbase.conf';
{$ENDIF}
{$IFDEF FIREBIRD_STRATEGY}
  ConfigFileName := 'dorm_firebird.conf';
{$ENDIF}
{$IFDEF FIREBIRD_UIB_STRATEGY}
  ConfigFileName := 'dorm_firebird_uib.conf';
{$ENDIF}
{$IFDEF INTERBASE_UIB_STRATEGY}
  ConfigFileName := 'dorm_interbase_uib.conf';
{$ENDIF}
{$IFDEF SQLSERVER_STRATEGY}
  ConfigFileName := 'dorm_sqlserver.conf';
{$ENDIF}
  Caption := Caption + ' using ' + ConfigFileName;

  Session := TSession.CreateConfigured(TStreamReader.Create(ConfigFileName),
    TStreamReader.Create('samples.mapping'), deDevelopment);
  Session.StartTransaction;
{$REGION 'Insert some data'}
  InitializeData;
{$ENDREGION}
  People := TObjectList<TPerson>.Create;
  Session.FillList<TPerson>(People);
  BindScopePeople.DataObject := People;
  BindScopePeople.Active := True;
  BindList1.FillList;
end;

procedure TForm11.InitializeData;
var
  p: TPerson;
begin
  People := TObjectList<TPerson>.Create;
  Session.DeleteAll(TPerson);
  Session.DeleteAll(TLaptop);
  p := TPerson.Create('Daniele', 'Teti', 32);
  p.Laptops.Add(TLaptop.Create('DELL LATITUDE', 2048, 2));
  p.Laptops.Add(TLaptop.Create('COMPAQ PRESARIO', 2048, 4));
  People.Add(p);

  p := TPerson.Create('Scott', 'Summers', 40);
  p.Laptops.Add(TLaptop.Create('DELL A707', 4096, 8));
  People.Add(p);

  p := TPerson.Create('Bruce', 'Banner', 50);
  p.Laptops.Add(TLaptop.Create('DELL A101', 1024, 1));
  People.Add(p);

  People.Add(TPerson.Create('Sue', 'Storm', 35));
  People.Add(TPerson.Create('Peter', 'Parker', 17));
  Session.PersistCollection(People);
  People.Free;
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
  Criteria: ICriteria;
begin
  BindScopePeople.Active := False;
  if Edit3.Text = EmptyStr then
  begin
    Session.FillList<TPerson>(People);
  end
  else
  begin
    Criteria := TdormCriteria.NewCriteria('FirstName',
      TdormCompareOperator.coEqual, Edit3.Text);
    Session.FillList<TPerson>(People, Criteria);
  end;
  BindScopePeople.DataObject := People;
  BindScopePeople.Active := True;
  BindList1.FillList;
end;

procedure TForm11.Button5Click(Sender: TObject);
var
  p: TPerson;
begin
  BindScopePerson.Active := False;
  p := BindScopePerson.DataObject as TPerson;
  p.ObjStatus := osDeleted;
  Session.Persist(People.Extract(p));
  p.Free;
  BindScopePerson.DataObject := nil;
  BindList1.FillList;
end;

procedure TForm11.Button6Click(Sender: TObject);
var
  res: TArray<String>;
  l: TLaptop;
begin
  SetLength(res, 3);
  if InputQuery('New Laptop', ['Model', 'RAM [MB]', 'Cores'], res) then
  begin
    l := TLaptop.Create(res[0], strtoint(res[1]), strtoint(res[2]));
    TPerson(People[ListBox1.ItemIndex]).Laptops.Add(l);
    l.Owner := People[ListBox1.ItemIndex] as TPerson;
    Session.Persist(l);
    BindList2.FillList;
  end;
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
    BindScopeLaptops.Active := False;
    Session.LoadRelations(People[ListBox1.ItemIndex]);
    BindScopePerson.DataObject := People[ListBox1.ItemIndex];
    BindScopeLaptops.DataObject := TPerson(People[ListBox1.ItemIndex]).Laptops;
    BindScopePerson.Active := True;
    BindScopeLaptops.Active := True;
    BindList2.FillList;
  end;
end;

procedure TForm11.SpinBox1Change(Sender: TObject);
begin
  TBindings.Notify(Sender, 'Value');
end;

end.
