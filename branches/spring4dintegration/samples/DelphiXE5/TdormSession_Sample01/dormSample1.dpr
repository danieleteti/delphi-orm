program dormSample1;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form2},
  BObjectsU in 'BObjectsU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
