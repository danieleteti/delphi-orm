program ztestall;

{$I ../../src/Zeos.inc}

(*{$mode objfpc}*){$H+}

{$SAFEFPUEXCEPTIONS ON}

uses
  custapp, sysutils, math,
  Interfaces, Forms, GuiTestRunner, LResources,
  Classes, consoletestrunner, fpcunit, fpcunitreport, plaintestreport,
  {$IFDEF FPC2_6DOWN}
  latextestreport, testregistry,
  {$ENDIF}
  ZTestConfig,
  ZSqlTestCase,
  zxmltestreport,
  //core
  ZTestCore,
  //parsesql
  ZTestParseSql,
  //dbc
  ZTestDbc,
  //component
  ZTestComponents,
  //bugreport
  ZTestBugReports,
  //performance
  ZTestPerformance
  ;

type
  { TMyResultsWriter }

  TMyResultsWriter = class(TPlainResultsWriter)
  protected
  // override the protected methods of TCustomResultsWriter to customize its behavior
    procedure WriteTestFooter({%H-}ATest: TTest; {%H-}ALevel:{%H-} integer; {%H-}ATiming: TDateTime); override;
    procedure WriteSuiteHeader({%H-}ATestSuite: TTestSuite; {%H-}ALevel: integer); override;
    procedure WriteSuiteFooter({%H-}ATestSuite: TTestSuite; {%H-}ALevel: integer;
      {%H-}ATiming: TDateTime; {%H-}ANumRuns:{%H-} integer; {%H-}ANumErrors: integer;
      {%H-}ANumFailures: integer; {%H-}ANumIgnores: integer); override;
  public
  end;

  { TMyTestRunner }

  TMyTestRunner = class(TTestRunner)
  private
    {$IFNDEF FPC}
    FullRegistryItems: TFPList;
    CurrentRegistryItems: TFPList;
    {$ENDIF}
  protected
  // override the protected methods of TTestRunner to customize its behavior
    procedure WriteCustomHelp; override;
    function GetShortOpts: string; override;
    function GetResultsWriter: TCustomResultsWriter; override;
    procedure DoTestRun(ATest: TTest); override;
    {$IFDEF FPC2_6DOWN}
    procedure DoRun; override;
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TMyGUITestRunner }

  TMyGUITestRunner = class(TGUITestRunner)
  private
    {$IFNDEF FPC}
    FullRegistryItems: TFPList;
    CurrentRegistryItems: TFPList;
    {$ENDIF}
  protected
  // override the protected methods of TGUITestRunner to customize its behavior
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TMyGUITestRunner.Create(TheOwner: TComponent);
begin
  {$IFNDEF FPC}
  // Dirty Workaround to make sure ALL tests can be destroyed in the destructor
  FullRegistryItems := TFPList.Create;
  CurrentRegistryItems := GetExecutedTests;
  FullRegistryItems.Assign(GetTestRegistry.Tests); //save old list
  GetTestRegistry.Tests.Assign(CurrentRegistryItems); //assign new or same list
  {$ENDIF}
  inherited Create(TheOwner);
end;

destructor TMyGUITestRunner.Destroy;
begin
  {$IFNDEF FPC}
  GetTestRegistry.Tests.Assign(FullRegistryItems); //assign old list again -> destroy the tests
  FreeAndNil(FullRegistryItems); //free old list
  FreeAndNil(CurrentRegistryItems); //free possible changed list
  {$ENDIF}
  inherited Destroy;
end;

procedure TMyResultsWriter.WriteTestFooter(ATest: TTest; ALevel: integer;
  ATiming: TDateTime);
begin
  { //don't write the verbose test footer information
  inherited WriteTestFooter(ATest, ALevel, ATiming);}
end;

procedure TMyResultsWriter.WriteSuiteHeader(ATestSuite: TTestSuite;
  ALevel: integer);
begin
  { //don't write the verbose suite header information
  inherited WriteSuiteHeader(ATestSuite, ALevel);}
end;

procedure TMyResultsWriter.WriteSuiteFooter(ATestSuite: TTestSuite;
  ALevel: integer; ATiming: TDateTime; ANumRuns: integer; ANumErrors: integer;
  ANumFailures: integer; ANumIgnores: integer);
begin
  { //don't write the verbose suite footer information
  inherited WriteSuiteFooter(ATestSuite, ALevel, ATiming, ANumRuns, ANumErrors,
    ANumFailures, ANumIgnores); }
end;

{ TMyTestRunner }

procedure TMyTestRunner.WriteCustomHelp;
begin
  inherited WriteCustomHelp;
  writeln('  -c <filename>                        custom config file name');
  writeln('  -b or --batch                        don''t run the GUI interface');
  writeln('  -v or --verbose                      show full output (otherwise compact report is used)');
  writeln('  -n or --norebuild                    don''t rebuild the databases');
  writeln('  -m <filename> or -monitor <filename> sqlmonitor file name');
  writeln(' --suite="<layer>.<optional testname>.<optional MethodeName>"');
  writeln(' --memcheck <filename>')
end;

function TMyTestRunner.GetShortOpts: string;
begin
  Result:=inherited GetShortOpts+'bvcnm:';
end;

function TMyTestRunner.GetResultsWriter: TCustomResultsWriter;
begin
  if (FormatParam = fPlain) and not CommandLineSwitches.verbose then
    Result := TMyResultsWriter.Create(nil)
  else if FormatParam = fXML then
    Result := TZXMLResultsWriter.Create(nil)
  else
    Result:=inherited GetResultsWriter;
end;

constructor TMyTestRunner.Create(AOwner: TComponent);
begin
  {$IFNDEF FPC}
  // Dirty Workaround to make sure ALL tests can be destroyed in the destructor
  FullRegistryItems := TFPList.Create;
  CurrentRegistryItems := GetExecutedTests;
  FullRegistryItems.Assign(GetTestRegistry.Tests); //save old list
  GetTestRegistry.Tests.Assign(CurrentRegistryItems); //assign new or same list
  {$ENDIF}
  inherited Create(AOwner);
  longopts.Add('batch');
  longopts.Add('verbose');
  longopts.Add('norebuild');
  longopts.Add('monitor:');
  longopts.Add('suite:');
  LongOpts.Add('xml:');
  LongOpts.Add('batch');
  LongOpts.Add('suitename:');
  if CommandLineSwitches.xml
  then DefaultFormat := fXML;
  if CommandLineSwitches.xmlfilename <> ''
  then FileName := CommandLineSwitches.xmlfilename;
end;

destructor TMyTestRunner.Destroy;
begin
  {$IFNDEF FPC}
  GetTestRegistry.Tests.Assign(FullRegistryItems); //assign old list again -> destroy the tests
  FreeAndNil(FullRegistryItems); //free old list
  FreeAndNil(CurrentRegistryItems); //free possible changed list
  {$ENDIF}
  inherited Destroy;
end;

procedure TMyTestRunner.DoTestRun(ATest: TTest);
begin
  if CommandLineSwitches.suitename <> '' then ;
    if ATest.TestName = 'SuiteList' then
      (ATest as TTestSuite).TestName := CommandLineSwitches.suitename;
  inherited DoTestRun(ATest);
end;

{$IFDEF FPC2_6DOWN}
// This is an exact copy of the DoRun procedure from TTestRunner in FPCs
// consoletestrunner.pas. I just added CheckTestRegistryEx to allow specifying
// more than one suite to run.
procedure TMyTestRunner.DoRun;

  procedure CheckTestRegistry (test:TTest; ATestName:string);
  var s, c : string;
      I, p : integer;
  begin
    if test is TTestSuite then
      begin
      p := pos ('.', ATestName);
      if p > 0 then
        begin
        s := copy (ATestName, 1, p-1);
        c := copy (ATestName, p+1, maxint);
        end
      else
        begin
        s := '';
        c := ATestName;
        end;
      if comparetext(c, test.TestName) = 0 then
        DoTestRun(test)
      else if (CompareText( s, Test.TestName) = 0) or (s = '') then
        for I := 0 to TTestSuite(test).Tests.Count - 1 do
          CheckTestRegistry (TTest(TTestSuite(test).Tests[I]), c)
      end
    else // if test is TTestCase then
      begin
      if comparetext(test.TestName, ATestName) = 0 then
        DoTestRun(test);
      end;
  end;

  procedure CheckTestRegistryEx (test:TTest; ATestName:string);
  var
    x, y: Integer;
    TestNames: Array of String;
  begin
    if ATestName = '' then CheckTestRegistry(test, ATestName) else begin
      x := 1;
      while ATestName <> '' do begin
        y := Pos(',', ATestName);
        if y = 0 then y := length(ATestName) + 1;
        SetLength(TestNames, x);
        TestNames[x-1] := copy(ATestName, 1, y - 1);
        inc(x);
        delete(ATestName, 1, y);
      end;

      for x := Low(TestNames) to High(TestNames) do begin
        CheckTestRegistry(test, TestNames[x]);
      end;
    end;
  end;

var
  I: integer;
  S: string;
begin
  S := CheckOptions(GetShortOpts, LongOpts);
  if (S <> '') then
    Writeln(S);

  ParseOptions;

  //get a list of all registed tests
  if HasOption('l', 'list') then
    case FormatParam of
      fLatex: Write(GetSuiteAsLatex(GetTestRegistry));
      fPlain: Write(GetSuiteAsPlain(GetTestRegistry));
    else
      Write(GetSuiteAsLatex(GetTestRegistry));;
    end;

  //run the tests
  if HasOption('suite') then
  begin
    S := '';
    S := GetOptionValue('suite');
    if S = '' then
      for I := 0 to GetTestRegistry.Tests.Count - 1 do
        writeln(GetTestRegistry[i].TestName)
    else
      for I := 0 to GetTestRegistry.Tests.count-1 do
        CheckTestRegistryEx(GetTestregistry[I], S);
  end
  else if HasOption('a', 'all') or (DefaultRunAllTests and Not HasOption('l','list')) then
    DoTestRun(GetTestRegistry) ;
  Terminate;
end;
{$ENDIF}

var
  Applicationc: TMyTestRunner;

{$IFDEF WINDOWS}{$R ztestall.rc}{$ENDIF}

begin
  {$I ztestall.lrs}
  SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);
  if CommandLineSwitches.memcheck and (CommandLineSwitches.memcheck_file <> '') then
  begin
    if FileExists(CommandLineSwitches.memcheck_file) then
      DeleteFile(CommandLineSwitches.memcheck_file);
    {$IF FPC_FULLVERSION<30000}
    SetHeapTraceOutput(CommandLineSwitches.memcheck_file);
    {$IFEND}
  end;
  TestGroup := COMMON_GROUP;

  If Not CommandLineSwitches.help and
     Not CommandLineSwitches.norebuild then
    RebuildTestDatabases;

  If CommandLineSwitches.sqlmonitor then
    EnableZSQLMonitor;

  If CommandLineSwitches.batch or CommandLineSwitches.xml then
  begin
    Applicationc := TMyTestRunner.Create(nil);
    Applicationc.Initialize;
    Applicationc.Run;
    Applicationc.Free;
  end
  else
  begin
    Application.Initialize;
    Application.CreateForm(TMyGuiTestRunner, TestRunner);
    Application.Run;
  end;

end.
