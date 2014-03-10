program dsserver;

uses
  FastMM4,
  Vcl.Forms,
  MainServerForm in 'MainServerForm.pas' {Form11} ,
  ServerMethodsUnit in 'ServerMethodsUnit.pas' {dormServerSample: TDataModule} ,
  ServerContainerUnit
    in 'ServerContainerUnit.pas' {ServerContainer: TDataModule} ,
  BusinessObjects in '..\..\BusinessObjects.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm11, Form11);
  Application.CreateForm(TServerContainer, ServerContainer);
  Application.Run;

end.
