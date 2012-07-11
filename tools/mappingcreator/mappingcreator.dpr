program mappingcreator;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  dorm.MappingCreator in 'lib\dorm.MappingCreator.pas',
  dorm.MappingCreator.Strategy in 'lib\dorm.MappingCreator.Strategy.pas',
  dorm.MappingCreator.Strategy.Firebird in 'lib\dorm.MappingCreator.Strategy.Firebird.pas',
  dorm.CodeGenerator in 'lib\dorm.CodeGenerator.pas',
  dorm.CodeGenerator.Delphi in 'lib\dorm.CodeGenerator.Delphi.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
