unit FindersTests;

interface

uses
  TestFramework,
  BaseTestCase,
  dorm.Finders,
  dorm,
  dorm.Collections;

type
  TFindersTests = class(TBaseTestCase)
  published

  end;

implementation

{ TestFinders }

initialization

RegisterTest(TFindersTests.Suite);

end.
