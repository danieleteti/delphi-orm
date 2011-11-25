unit MainServerForm;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;

type
  TForm11 = class(TForm)
    Label1: TLabel;
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form11: TForm11;

implementation

{$R *.dfm}


procedure TForm11.FormCreate(Sender: TObject);
begin
{$IFDEF SQLITE3_STRATEGY}
  Caption := Caption + ' using dorm_sqlite3.conf';
{$ENDIF}
{$IFDEF FIREBIRD_STRATEGY}
  Caption := Caption + ' using dorm_firebird.conf';
{$ENDIF}
{$IFDEF INTERBASE_STRATEGY}
  Caption := Caption + ' using dorm_interbase.conf';
{$ENDIF}
end;

procedure TForm11.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

end.
