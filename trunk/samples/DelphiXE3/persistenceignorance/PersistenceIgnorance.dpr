program PersistenceIgnorance;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form11};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm11, Form11);
  Application.Run;
end.
