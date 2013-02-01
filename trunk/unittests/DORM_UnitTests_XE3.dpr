program DORM_UnitTests_XE3;

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
  dorm in '..\source\dorm.pas',
  dorm.UOW in '..\source\dorm.UOW.pas',
  dorm.Utils in '..\source\dorm.Utils.pas',
  dorm.Utils.Sequences in '..\source\dorm.utils.Sequences.pas',
  dorm.tests.bo in 'dorm.tests.bo.pas',
  BaseTestCase in 'BaseTestCase.pas',
  FindersTests in 'FindersTests.pas',
  FrameworkTests in 'FrameworkTests.pas',
  dorm.Finders in '..\source\dorm.Finders.pas',
  dorm.Filters in '..\source\dorm.Filters.pas',
  dorm.Mappings in '..\source\dorm.Mappings.pas',
  dorm.Mappings.Strategies in '..\source\dorm.Mappings.Strategies.pas',
  dorm.CodeGenerator in '..\tools\mappingcreator\lib\dorm.CodeGenerator.pas',
  dorm.CodeGenerator.Delphi
    in '..\tools\mappingcreator\lib\dorm.CodeGenerator.Delphi.pas',
  dorm.loggers in '..\source\dorm.loggers.pas',
  dorm.loggers.FileLog in '..\source\dorm.loggers.FileLog.pas',
  dorm.adapters in '..\source\dorm.adapters.pas',
  TestDORMMapping.Merge in 'TestDORMMapping.Merge.pas',
  TestDORMMapping.Attributes in 'TestDORMMapping.Attributes.pas',
  TestDORMMapping.CoC in 'TestDORMMapping.CoC.pas',
  TestDORMMapping.FileJSON in 'TestDORMMapping.FileJSON.pas',
  dorm.adapter.Base in '..\source\dorm.adapter.Base.pas',
  TestDORMDuckTyping in 'TestDORMDuckTyping.pas',
  dorm.tests.objstatus.bo in 'dorm.tests.objstatus.bo.pas',
  TestDORMObjStatus in 'TestDORMObjStatus.pas',
  dorm.Query in '..\source\dorm.Query.pas';

{$R *.RES}


begin

{$IFNDEF CONSOLE_TESTRUNNER}

  ReportMemoryLeaksOnShutdown := True;

{$ENDIF}

  Application.Initialize;
  if IsConsole then
    with TextTestRunner.RunRegisteredTests(rxbHaltOnFailures) do
      Free
  else
    GUITestRunner.RunRegisteredTests;

end.
