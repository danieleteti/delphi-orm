unit MainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.ActnList,
  Vcl.Buttons,
  System.Generics.collections,
  bo,
  Vcl.Menus;

type
  TfrmMain = class(TForm)
    lbTodos: TListBox;
    Panel1: TPanel;
    ActionList1: TActionList;
    acRefresh: TAction;
    acNew: TAction;
    acEdit: TAction;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Panel2: TPanel;
    BitBtn4: TBitBtn;
    BitBtn1: TBitBtn;
    acPersist: TAction;
    Label1: TLabel;
    chkFilter: TCheckBox;
    BitBtn5: TBitBtn;
    acDelete: TAction;
    PopupMenu1: TPopupMenu;
    acCreateRandomData: TAction;
    Createsomerandomdata1: TMenuItem;
    procedure acRefreshExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acNewExecute(Sender: TObject);
    procedure acPersistExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure lbTodosDblClick(Sender: TObject);
    procedure acEditUpdate(Sender: TObject);
    procedure chkFilterClick(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteUpdate(Sender: TObject);
    procedure acCreateRandomDataExecute(Sender: TObject);
  private
    FTodoList: TObjectList<TTodo>;
    procedure RefreshList;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}


uses DORMModule,
  Vcl.GraphUtil,
  EditTodoForm,
  dorm.Commons,
  dorm.Filters;

procedure TfrmMain.acCreateRandomDataExecute(Sender: TObject);
begin
  MainDORM.CreateSomeData;
  acRefresh.Execute;
end;

procedure TfrmMain.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg('Are you sure?', mtConfirmation, [mbYes, mbno], 0) = mrYes then
  begin
    FTodoList[lbTodos.ItemIndex].ObjStatus := osDeleted;
    GetDORMSession.Persist(FTodoList[lbTodos.ItemIndex]);
    FTodoList.Delete(lbTodos.ItemIndex);
    RefreshList;
  end;
end;

procedure TfrmMain.acDeleteUpdate(Sender: TObject);
begin
  acDelete.Enabled := acEdit.Enabled;
end;

procedure TfrmMain.acEditExecute(Sender: TObject);
var
  frm: TfrmEditTodo;
begin
  frm := TfrmEditTodo.Create(self);
  try
    frm.CurrentToDo := FTodoList[lbTodos.ItemIndex];
    if frm.ShowModal = mrOK then
      FTodoList[lbTodos.ItemIndex].ObjStatus := osDirty;
    RefreshList;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.acEditUpdate(Sender: TObject);
begin
  acEdit.Enabled := lbTodos.ItemIndex > -1;
end;

procedure TfrmMain.acNewExecute(Sender: TObject);
var
  frm: TfrmEditTodo;
begin
  frm := TfrmEditTodo.Create(self);
  try
    frm.CurrentToDo := TTodo.Create;
    if frm.ShowModal = mrOK then
    begin
      FTodoList.Add(frm.CurrentToDo);
    end
    else
      frm.CurrentToDo.Free;
    RefreshList;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.acPersistExecute(Sender: TObject);
begin
  GetDORMSession.PersistCollection(WrapAsList(FTodoList));
  RefreshList;
end;

procedure TfrmMain.acRefreshExecute(Sender: TObject);
var
  todo: TTodo;
  Criteria: ICriteria;
begin
  Criteria := nil;
  if chkFilter.Checked then
    Criteria := NewCriteria('done', coEqual, false);
  GetDORMSession.FillList<TTodo>(FTodoList, Criteria);
  RefreshList;
end;

procedure TfrmMain.chkFilterClick(Sender: TObject);
begin
  acRefresh.Execute;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FTodoList.Free;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FTodoList := TObjectList<TTodo>.Create(true);
  acRefresh.Execute;
end;

procedure TfrmMain.lbTodosDblClick(Sender: TObject);
begin
  acEdit.Execute;
end;

procedure TfrmMain.RefreshList;
var
  todo: TTodo;
begin
  lbTodos.Items.BeginUpdate;
  try
    lbTodos.Clear;
    for todo in FTodoList do
    begin
      lbTodos.Items.Add(todo.ToString);
    end;
  finally
    lbTodos.Items.EndUpdate;
  end;
end;

end.
