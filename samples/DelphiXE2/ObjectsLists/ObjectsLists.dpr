program ObjectsLists;

uses
  FastMM4,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form11},
  BusinessObjects in '..\BusinessObjects.pas';

{$R *.res}


begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TForm11, Form11);
  Application.Run;

end.
