program dormDCGUI;

uses
  Forms,
  MainForm in 'MainForm.pas' {frmMain},
  dorm.DBCreator.SQLServer in 'lib\dorm.DBCreator.SQLServer.pas',
  dorm.DBCreator in 'lib\dorm.DBCreator.pas',
  dorm.DBCreator.Interbase in 'lib\dorm.DBCreator.Interbase.pas',
  dorm.DBCreator.Sqlite3 in 'lib\dorm.DBCreator.Sqlite3.pas',
  dorm.utils.Sequences in '..\..\source\dorm.utils.Sequences.pas',
  dorm.adapter.all in '..\..\source\dorm.adapter.all.pas',
  dorm.adapter.DBExpress.Factory in '..\..\source\dorm.adapter.DBExpress.Factory.pas',
  dorm.adapter.Firebird in '..\..\source\dorm.adapter.Firebird.pas',
  dorm.adapter.Interbase in '..\..\source\dorm.adapter.Interbase.pas',
  dorm.adapter.Sqlite3 in '..\..\source\dorm.adapter.Sqlite3.pas',
  dorm.adapter.SQLServer9 in '..\..\source\dorm.adapter.SQLServer9.pas',
  dorm.adapter.SQLServerDevart in '..\..\source\dorm.adapter.SQLServerDevart.pas',
  dorm.Collections in '..\..\source\dorm.Collections.pas',
  dorm.Commons in '..\..\source\dorm.Commons.pas',
  dorm.Core.IdentityMap in '..\..\source\dorm.Core.IdentityMap.pas',
  dorm.Filters in '..\..\source\dorm.Filters.pas',
  dorm.Finders in '..\..\source\dorm.Finders.pas',
  dorm.InterposedObject in '..\..\source\dorm.InterposedObject.pas',
  dorm.loggers.CodeSite in '..\..\source\dorm.loggers.CodeSite.pas',
  dorm.loggers.FileLog in '..\..\source\dorm.loggers.FileLog.pas',
  dorm.Mappings in '..\..\source\dorm.Mappings.pas',
  dorm.Mappings.Strategies in '..\..\source\dorm.Mappings.Strategies.pas',
  dorm.Metadata in '..\..\source\dorm.Metadata.pas',
  dorm in '..\..\source\dorm.pas',
  dorm.UOW in '..\..\source\dorm.UOW.pas',
  dorm.Utils in '..\..\source\dorm.Utils.pas',
  dorm.DBCreator.Firebird in 'lib\dorm.DBCreator.Firebird.pas',
  dorm.adapter.SQLServer in '..\..\source\dorm.adapter.SQLServer.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
