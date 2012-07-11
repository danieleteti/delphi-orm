program dsclient;

uses
  //FastMM4,
  FMX.Forms,
  MainForm in 'MainForm.pas' {frmClientMain},
  BusinessObjects in '..\..\BusinessObjects.pas',
  ClientClassesUnit in 'ClientClassesUnit.pas',
  ClientModuleUnit in 'ClientModuleUnit.pas' {ClientProxy: TDataModule};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TClientProxy, ClientProxy);
  Application.CreateForm(TfrmClientMain, frmClientMain);
  Application.Run;

end.
