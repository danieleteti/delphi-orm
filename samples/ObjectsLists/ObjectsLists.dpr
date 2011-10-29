program ObjectsLists;

uses
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form11},
  BusinessObjects in '..\BusinessObjects.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm11, Form11);
  Application.Run;
end.
