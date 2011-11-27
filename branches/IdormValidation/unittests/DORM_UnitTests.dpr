program DORM_UnitTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  FastMM4,
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestDORM in 'TestDORM.pas',
  TestDORMSearchCriteria in 'TestDORMSearchCriteria.pas',
  TestDORMSpeed in 'TestDORMSpeed.pas',
  TestDORMRelations in 'TestDORMRelations.pas',
  dorm.Collections in '..\source\dorm.Collections.pas',
  dorm.Commons in '..\source\dorm.Commons.pas',
  dorm.Core.IdentityMap in '..\source\dorm.Core.IdentityMap.pas',
  dorm.InterposedObject in '..\source\dorm.InterposedObject.pas',
  dorm.loggers.FileLog in '..\source\dorm.loggers.FileLog.pas',
  dorm in '..\source\dorm.pas',
  dorm.UOW in '..\source\dorm.UOW.pas',
  dorm.Utils in '..\source\dorm.Utils.pas',
  dorm.utils.Sequences in '..\source\dorm.utils.Sequences.pas',
  dorm.tests.bo in 'dorm.tests.bo.pas',
  BaseTestCase in 'BaseTestCase.pas',
  FindersTests in 'FindersTests.pas',
  FrameworkTests in 'FrameworkTests.pas',
  dorm.Finders in '..\source\dorm.Finders.pas',
  dorm.loggers.CodeSite in '..\source\dorm.loggers.CodeSite.pas',
  dorm.adapter.DBExpress.Factory in '..\source\dorm.adapter.DBExpress.Factory.pas',
  dorm.adapter.Firebird in '..\source\dorm.adapter.Firebird.pas',
  dorm.adapter.Interbase in '..\source\dorm.adapter.Interbase.pas',
  dorm.adapter.Sqlite3 in '..\source\dorm.adapter.Sqlite3.pas';

{$R *.RES}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests do
      Free
  else
    GUITestRunner.RunRegisteredTests;
end.

