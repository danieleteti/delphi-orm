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
  Vcl.Menus, Data.Bind.Components, Data.Bind.ObjectScope, System.Actions,
  Data.Bind.GenData,
  Vcl.Grids, Data.Bind.EngExt, Vcl.Bind.DBEngExt, Vcl.Bind.Grid, System.Rtti,
  System.Bindings.Outputs, Vcl.Bind.Editors, Data.Bind.Grid;

type
  TfrmMain = class(TForm)
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
    BindSourceTodos: TPrototypeBindSource;
    StringGrid1: TStringGrid;
    BindingsList1: TBindingsList;
    LinkGridToDataSource1: TLinkGridToDataSource;
    procedure acRefreshExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure acNewExecute(Sender: TObject);
    procedure acPersistExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure acEditUpdate(Sender: TObject);
    procedure chkFilterClick(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acDeleteUpdate(Sender: TObject);
    procedure acCreateRandomDataExecute(Sender: TObject);
    procedure BindSourceTodosCreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure StringGrid1DblClick(Sender: TObject);
  private
    FTodoList: TObjectList<TTodo>;
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
  dorm.Filters, PresentationLogicU;

procedure TfrmMain.acCreateRandomDataExecute(Sender: TObject);
begin
  MainDORM.CreateSomeData;
  acRefresh.Execute;
end;

procedure TfrmMain.acDeleteExecute(Sender: TObject);
begin
  if MessageDlg('Are you sure?', mtConfirmation, [mbYes, mbno], 0) = mrYes then
  begin
    FTodoList[BindSourceTodos.ItemIndex].ObjStatus := osDeleted;
    GetDORMSession.Persist(FTodoList[BindSourceTodos.ItemIndex]);
    FTodoList.Delete(BindSourceTodos.ItemIndex);
    acRefresh.Execute;
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
    frm.CurrentTodo := FTodoList[BindSourceTodos.ItemIndex];
    if frm.ShowModal = mrOK then
      frm.CurrentTodo.ObjStatus := osDirty;
    BindSourceTodos.Refresh;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.acEditUpdate(Sender: TObject);
begin
  acEdit.Enabled := BindSourceTodos.ItemIndex > -1;
end;

procedure TfrmMain.acNewExecute(Sender: TObject);
var
  frm: TfrmEditTodo;
begin
  frm := TfrmEditTodo.Create(self);
  try
    frm.CurrentTodo := TTodo.Create;
    if frm.ShowModal = mrOK then
    begin
      FTodoList.Add(frm.CurrentTodo);
    end
    else
      frm.CurrentTodo.Free;
    BindSourceTodos.Refresh;
  finally
    frm.Free;
  end;
end;

procedure TfrmMain.acPersistExecute(Sender: TObject);
begin
  GetDORMSession.PersistCollection(WrapAsList(FTodoList));
  BindSourceTodos.Active := False;
  BindSourceTodos.Active := True;
end;

procedure TfrmMain.acRefreshExecute(Sender: TObject);
var
  Criteria: ICriteria;
begin
  Criteria := nil;
  if chkFilter.Checked then
    Criteria := NewCriteria('done', coEqual, False);
  GetDORMSession.FillList<TTodo>(FTodoList, Criteria);
  BindSourceTodos.Active := False;
  BindSourceTodos.Active := True;
end;

procedure TfrmMain.BindSourceTodosCreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  ABindSourceAdapter := TListBindSourceAdapter<TTodo>.Create(BindSourceTodos);
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
  FTodoList := TObjectList<TTodo>.Create(True);
  (BindSourceTodos.InternalAdapter as TListBindSourceAdapter<TTodo>)
    .SetList(FTodoList, False);
  acRefresh.Execute;
end;

procedure TfrmMain.StringGrid1DblClick(Sender: TObject);
begin
  acEdit.Execute;
end;

procedure TfrmMain.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
begin
  if NeedsToDrawCheck(ARow, LinkGridToDataSource1.Columns[ACol], FTodoList) then
    with (Sender as TStringGrid) do
    begin
      Canvas.Pen.Color := clGreen;
      DrawCheck(Canvas, Rect.CenterPoint, 5);
    end;
end;

end.
