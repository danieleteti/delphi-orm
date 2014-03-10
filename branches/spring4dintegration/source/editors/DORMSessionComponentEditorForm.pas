unit DORMSessionComponentEditorForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, dorm.Component.Session;

type
  TDORMSessionEditorForm = class(TForm)
  private
    FDORMSession: TdormSession;
    procedure SetDORMSession(const Value: TdormSession);
    { Private declarations }
  public
    property DORMSession: TdormSession read FDORMSession write SetDORMSession;
  end;

implementation

{$R *.dfm}

{ TDORMSessionEditorForm }

procedure TDORMSessionEditorForm.SetDORMSession(const Value: TdormSession);
begin
  FDORMSession := Value;
end;

end.
