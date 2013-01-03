unit EditTodoForm;

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
  dorm,
  dorm.loggers,
  dorm.adapters,
  dorm.Commons,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  bo,
  Vcl.ActnList,
  Vcl.Buttons;

type
  TfrmEditTodo = class(TForm)
    edtTitle: TLabeledEdit;
    memDescription: TMemo;
    Label1: TLabel;
    dtpDueDate: TDateTimePicker;
    chkDone: TCheckBox;
    ActionList1: TActionList;
    acLoad: TAction;
    acUpdate: TAction;
    acSaveAsNew: TAction;
    acCommitAndRestart: TAction;
    BitBtn1: TBitBtn;
    Label2: TLabel;
    BitBtn2: TBitBtn;
    procedure acLoadExecute(Sender: TObject);
    procedure acUpdateExecute(Sender: TObject);
    procedure acSaveAsNewExecute(Sender: TObject);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure acSaveAsNewUpdate(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
  private
    Session: TSession;
    FCurrentToDo: TTodo;
    procedure SetCurrentToDo(const Value: TTodo);
  protected
    procedure bo2gui;
    procedure gui2bo;
  public
    property currenttodo: TTodo read FCurrentToDo write SetCurrentToDo;
  end;

{$R *.dfm}

implementation

procedure TfrmEditTodo.bo2gui;
begin
  edtTitle.Text := currenttodo.Title;
  memDescription.Lines.Text := currenttodo.Description;
  dtpDueDate.Date := currenttodo.DueDate;
  chkDone.Checked := currenttodo.Done;
end;

procedure TfrmEditTodo.acLoadExecute(Sender: TObject);
begin
  // FreeAndNil(FCurrentToDo);
  // currenttodo := Session.Load<TTodo>(strtoint(Edit1.Text));
  // if not assigned(currenttodo) then
  // begin
  // ShowMessage('Cannot find TODO with ID = ' + Edit1.Text);
  // end
  // else
  // bo2gui;
end;

procedure TfrmEditTodo.acSaveAsNewExecute(Sender: TObject);
begin
  // gui2bo;
  // currenttodo.ObjStatus := osDirty;
  // Session.Persist(currenttodo);
end;

procedure TfrmEditTodo.acSaveAsNewUpdate(Sender: TObject);
begin
  // acSaveAsNew.Enabled := assigned(currenttodo);
end;

procedure TfrmEditTodo.acUpdateExecute(Sender: TObject);
begin
  // gui2bo;
  // currenttodo.ObjStatus := osDirty;
  // Session.Persist(currenttodo);
end;

procedure TfrmEditTodo.BitBtn2Click(Sender: TObject);
begin
  gui2bo;
end;

procedure TfrmEditTodo.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    acLoad.Execute;

end;

procedure TfrmEditTodo.gui2bo;
begin
  currenttodo.Title := edtTitle.Text;
  currenttodo.Description := memDescription.Lines.Text;
  currenttodo.DueDate := trunc(dtpDueDate.Date);
  currenttodo.Done := chkDone.Checked;
end;

procedure TfrmEditTodo.SetCurrentToDo(const Value: TTodo);
begin
  FreeAndNil(FCurrentToDo);
  FCurrentToDo := Value;
  if Assigned(FCurrentToDo) then
    bo2gui;
end;

end.
