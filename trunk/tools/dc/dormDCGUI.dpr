program dormDCGUI;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  dorm.adapter.Firebird in '..\..\source\dorm.adapter.Firebird.pas',
  dorm.Collections in '..\..\source\dorm.Collections.pas',
  dorm.Commons in '..\..\source\dorm.Commons.pas',
  dorm.Core.IdentityMap in '..\..\source\dorm.Core.IdentityMap.pas',
  dorm.InterposedObject in '..\..\source\dorm.InterposedObject.pas',
  dorm.loggers.CodeSite in '..\..\source\dorm.loggers.CodeSite.pas',
  dorm.loggers.FileLog in '..\..\source\dorm.loggers.FileLog.pas',
  dorm in '..\..\source\dorm.pas',
  dorm.UOW in '..\..\source\dorm.UOW.pas',
  dorm.Utils in '..\..\source\dorm.Utils.pas',
  dorm.utils.Sequences in '..\..\source\dorm.utils.Sequences.pas',
  superobject in '..\..\lib\superobject\superobject.pas',
  dorm.DBCreator.Firebird in 'lib\dorm.DBCreator.Firebird.pas',
  dorm.DBCreator in 'lib\dorm.DBCreator.pas',
  dorm.adapter.DBExpress.Factory in '..\..\source\dorm.adapter.DBExpress.Factory.pas',
  dorm.DBCreator.Interbase in 'lib\dorm.DBCreator.Interbase.pas',
  dorm.adapter.Interbase in '..\..\source\dorm.adapter.Interbase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
