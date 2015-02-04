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
  Vcl.Buttons, System.Actions, Data.Bind.GenData, Data.Bind.Components,
  Data.Bind.ObjectScope, System.Rtti, System.Bindings.Outputs, Vcl.Bind.Editors,
  Data.Bind.EngExt,
  Vcl.Bind.DBEngExt;

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
    BindSourceTodo: TPrototypeBindSource;
    BindingsList1: TBindingsList;
    LinkControlToField1: TLinkControlToField;
    LinkControlToField2: TLinkControlToField;
    LinkPropertyToField1: TLinkPropertyToField;
    LinkControlToField3: TLinkControlToField;
    procedure BitBtn2Click(Sender: TObject);
    procedure BindSourceTodoCreateAdapter(Sender: TObject;
      var ABindSourceAdapter: TBindSourceAdapter);
    procedure BitBtn1Click(Sender: TObject);
    procedure dtpDueDateChange(Sender: TObject);
  private
    FCurrentToDo: TTodo;
    procedure SetCurrentToDo(const Value: TTodo);
  public
    property CurrentTodo: TTodo read FCurrentToDo write SetCurrentToDo;
  end;
{$R *.dfm}

implementation

procedure TfrmEditTodo.BindSourceTodoCreateAdapter(Sender: TObject;
  var ABindSourceAdapter: TBindSourceAdapter);
begin
  ABindSourceAdapter := TObjectBindSourceAdapter<TTodo>.Create(BindSourceTodo);
end;

procedure TfrmEditTodo.BitBtn1Click(Sender: TObject);
begin
  BindSourceTodo.Cancel;
end;

procedure TfrmEditTodo.BitBtn2Click(Sender: TObject);
begin
  BindSourceTodo.Post;
end;

procedure TfrmEditTodo.dtpDueDateChange(Sender: TObject);
begin
  (BindSourceTodo.InternalAdapter as TObjectBindSourceAdapter<TTodo>)
    .FindField('DueDate').SetTValue(dtpDueDate.Date);
end;

procedure TfrmEditTodo.SetCurrentToDo(const Value: TTodo);
begin
  FreeAndNil(FCurrentToDo);
  FCurrentToDo := Value;
  if Assigned(FCurrentToDo) then
  begin
    (BindSourceTodo.InternalAdapter as TObjectBindSourceAdapter<TTodo>)
      .SetDataObject(FCurrentToDo, False);
    BindSourceTodo.Active := True;
    BindSourceTodo.Edit;
  end;
end;

end.
