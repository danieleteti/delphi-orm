unit TestDORMDuckTyping;

interface

uses
  TestFramework,
  dorm.Commons,
  BaseTestCase;

type
  TTestDuckTyping = class(TBaseTestCase)
  strict private
    procedure CallAllMethods(AValidateable: TdormValidateable);

  strict protected
    FDuckObject: TDuckTypedObject;

  public
    procedure SetUp; override;
    procedure TearDown; override;

  published
    procedure Test_TSimpleValidatingObjectWithNoMethod;
    procedure Test_TSimpleValidatingObject;
    procedure Test_TSimpleValidatingObjectInsertAndUpdate;
    procedure Test_ErrorOnlyInOnBeforeSave;
    procedure Test_All_Methods_Called;
    procedure Test_LifeCicle;
    procedure Test_MethodsOnLoadList;
    procedure Test_FilterList;
  end;

implementation

uses
  dorm.Utils,
  System.Rtti,
  System.SysUtils,
  System.Classes,
  dorm.tests.objstatus.bo,
  dorm.Query,
  System.Generics.Collections;

type
  TSimpleValidatingObjectWithNoMethod = class(TObject)
    // no methods
  end;

  TSimpleValidatingObject = class(TSimpleValidatingObjectWithNoMethod)
  public
    function Validate: boolean; virtual;
  end;

  TSimpleValidatingObjectInsertAndUpdate = class(TSimpleValidatingObject)
  public
    function InsertValidate: boolean; virtual;
    function UpdateValidate: boolean; virtual;
  end;

  TValidatingObjectWithExceptionInBeforeSave = class(TSimpleValidatingObject)
  public
    procedure OnBeforeSave; virtual;
  end;

  TValidatingMock = class
  protected
    FHistory        : TStringList;
    FValidate       : boolean;
    FInsertValidate : boolean;
    FUpdateValidate : boolean;
    FDeleteValidate : boolean;
    FOnAfterLoad    : boolean;
    FOnBeforeLoad   : boolean;
    FOnBeforePersist: boolean;
    FOnAfterPersist : boolean;
    FOnBeforeInsert : boolean;
    FOnAfterInsert  : boolean;
    FOnBeforeUpdate : boolean;
    FOnAfterUpdate  : boolean;
    FOnBeforeDelete : boolean;
    FOnAfterDelete  : boolean;

  public
    constructor Create;
    destructor Destroy; override;
    /// //
    procedure Validate;
    procedure InsertValidate;
    procedure UpdateValidate;
    procedure DeleteValidate;
    /// ////
    procedure OnAfterLoad;
    procedure OnBeforeLoad;
    procedure OnBeforePersist;
    procedure OnAfterPersist;
    procedure OnBeforeInsert;
    procedure OnAfterInsert;
    procedure OnBeforeUpdate;
    procedure OnAfterUpdate;
    procedure OnBeforeDelete;
    procedure OnAfterDelete;

    // check
    function AllMethodsCalled: boolean;
    function GetHistory: TStringList;
  end;

  { TTestDuckTyping }

procedure TTestDuckTyping.CallAllMethods(AValidateable: TdormValidateable);
begin
  AValidateable.Validate;
  AValidateable.InsertValidate;
  AValidateable.UpdateValidate;
  AValidateable.DeleteValidate;
  AValidateable.OnBeforeLoad;
  AValidateable.OnAfterLoad;

  AValidateable.OnBeforeInsert;
  AValidateable.OnAfterInsert;

  AValidateable.OnBeforeUpdate;
  AValidateable.OnAfterUpdate;

  AValidateable.OnBeforeDelete;
  AValidateable.OnAfterDelete;

  AValidateable.OnBeforePersist;
  AValidateable.OnAfterPersist;
end;

procedure TTestDuckTyping.SetUp;
begin
  inherited;
  FDuckObject := TDuckTypedObject.Create;
end;

procedure TTestDuckTyping.TearDown;
begin
  FDuckObject.Free;
  inherited;

end;

procedure TTestDuckTyping.Test_All_Methods_Called;
var
  v        : TValidatingMock;
  Validable: TdormValidateable;
begin
  v := TValidatingMock.Create;
  CheckFalse(v.AllMethodsCalled);
  Validable := WrapAsValidateableObject(v, FDuckObject);
  CallAllMethods(Validable);
  CheckTrue(v.AllMethodsCalled);
  v.Free;
end;

procedure TTestDuckTyping.Test_ErrorOnlyInOnBeforeSave;
var
  obj         : TValidatingObjectWithExceptionInBeforeSave;
  Validateable: TdormValidateable;
begin
  obj := TValidatingObjectWithExceptionInBeforeSave.Create;
  Validateable := WrapAsValidateableObject(obj, FDuckObject);
  CheckNotNull(Validateable);
  try
    CallAllMethods(Validateable);
  except
    on E: Exception do
    begin
      CheckTrue(E.ClassType = EdormValidationException);
      CheckEquals('OnBeforeSave', E.Message);
    end;
  end;
  obj.Free;
end;

procedure TTestDuckTyping.Test_FilterList;
var
  L: IWrappedList;
begin
  Session.Persist(TPersonOS.Create('Daniele', 'Teti')).Free;
  Session.Persist(TPersonOS.Create('Peter', 'Parker')).Free;
  Session.Persist(TPersonOS.Create('Bruce', 'Banner')).Free;
  Session.Persist(TPersonOS.Create('Sue', 'Storm')).Free;

  L := Session.Filter<TPersonOS>(
    WrapAsList(Session.LoadListSQL<TPersonOS>(Select.From(TPersonOS)), true),
    function(O: TPersonOS): boolean
    begin
      Result := O.FirstName.Contains('r');
    end);
  CheckEquals(2, L.Count);
end;

procedure TTestDuckTyping.Test_LifeCicle;
var
  p : TPersonOS;
  s : string;
  p2: TPersonOS;
begin
  p := TPersonOS.NewPerson;
  Session.Persist(p);
  s := p.GetEventAndValidationHistory.DelimitedText;
  CheckEquals
    ('TPersonOS.OnBeforePersist-TPersonOS.Validate-TPersonOS.InsertValidate-TPersonOS.OnBeforeInsert-TPersonOS.OnAfterInsert-TPersonOS.OnAfterPersist',
    s);

  p2 := Session.Load<TPersonOS>(p.ID);
  s := p2.GetEventAndValidationHistory.DelimitedText;
  CheckEquals
    ('TPersonOS.OnAfterLoad',
    s);
  p2.Free;
  p.Free;
end;

procedure TTestDuckTyping.Test_MethodsOnLoadList;
var
  List: TObjectList<TPersonOS>;
begin
  Session.Persist(TPersonOS.NewPerson).Free;
  Session.Persist(TPersonOS.NewPerson).Free;
  Session.Persist(TPersonOS.NewPerson).Free;
  Session.Persist(TPersonOS.NewPerson).Free;

  List := Session.LoadList<TPersonOS>();
  try
    Session.ForEach<TPersonOS>(List,
      procedure(O: TPersonOS)
      begin
        CheckTrue(Session.IsClean(O));
        CheckEquals('TPersonOS.OnAfterLoad', O.GetEventAndValidationHistory.DelimitedText);
      end);
  finally
    List.Free;
  end;

  List := Session.LoadListSQL<TPersonOS>(Select.From(TPersonOS));
  try
    Session.ForEach<TPersonOS>(List,
      procedure(O: TPersonOS)
      begin
        CheckTrue(Session.IsClean(O));
        CheckEquals('TPersonOS.OnAfterLoad', O.GetEventAndValidationHistory.DelimitedText);
      end);
  finally
    List.Free;
  end;
end;

procedure TTestDuckTyping.Test_TSimpleValidatingObject;
var
  obj         : TSimpleValidatingObject;
  Validateable: TdormValidateable;
begin
  obj := TSimpleValidatingObject.Create;
  Validateable := WrapAsValidateableObject(obj, FDuckObject);
  CheckNotNull(Validateable);
  CallAllMethods(Validateable);
  obj.Free;

end;

procedure TTestDuckTyping.Test_TSimpleValidatingObjectInsertAndUpdate;
var
  obj         : TSimpleValidatingObjectInsertAndUpdate;
  Validateable: TdormValidateable;
begin
  obj := TSimpleValidatingObjectInsertAndUpdate.Create;
  Validateable := WrapAsValidateableObject(obj, FDuckObject);
  CheckNotNull(Validateable);
  Validateable.InsertValidate;
  Validateable.UpdateValidate;
  obj.Free;
end;

procedure TTestDuckTyping.Test_TSimpleValidatingObjectWithNoMethod;
var
  obj         : TSimpleValidatingObjectWithNoMethod;
  Validateable: TdormValidateable;
begin
  obj := TSimpleValidatingObjectWithNoMethod.Create;
  Validateable := WrapAsValidateableObject(obj, FDuckObject);
  CheckNotNull(Validateable);
  CallAllMethods(Validateable);
  obj.Free;
end;

{ TSimpleValidatingObject }

function TSimpleValidatingObject.Validate: boolean;
begin
  Result := true;
end;

{ TSimpleValidatingObjectInsertAndUpdate }

function TSimpleValidatingObjectInsertAndUpdate.InsertValidate: boolean;
begin
  Result := true;
end;

function TSimpleValidatingObjectInsertAndUpdate.UpdateValidate: boolean;
begin
  Result := False;
end;

{ TValidatingObjectWithExceptionInBeforeSave }

procedure TValidatingObjectWithExceptionInBeforeSave.OnBeforeSave;
begin
  raise EdormValidationException.Create('OnBeforeSave');
end;

{ TValidatingMock }

function TValidatingMock.AllMethodsCalled: boolean;
begin
  Result :=
    FValidate and
    FInsertValidate and
    FUpdateValidate and
    FDeleteValidate and
    FOnAfterLoad and
    FOnBeforeLoad and
    FOnBeforePersist and
    FOnAfterPersist and
    FOnBeforeInsert and
    FOnAfterInsert and
    FOnBeforeUpdate and
    FOnAfterUpdate and
    FOnBeforeDelete and
    FOnAfterDelete;
end;

constructor TValidatingMock.Create;
begin
  inherited;
  FHistory := TStringList.Create;
end;

procedure TValidatingMock.DeleteValidate;
begin
  FDeleteValidate := true;
  FHistory.Add('DeleteValidate');
end;

destructor TValidatingMock.Destroy;
begin
  FHistory.Free;
  inherited;
end;

function TValidatingMock.GetHistory: TStringList;
begin
  Result := FHistory;
end;

procedure TValidatingMock.InsertValidate;
begin
  FInsertValidate := true;
  FHistory.Add('InsertValidate');
end;

procedure TValidatingMock.OnAfterDelete;
begin
  FOnAfterDelete := true;
  FHistory.Add('OnAfterDelete');
end;

procedure TValidatingMock.OnAfterInsert;
begin
  FOnAfterInsert := true;
  FHistory.Add('OnAfterInsert');
end;

procedure TValidatingMock.OnAfterLoad;
begin
  FOnAfterLoad := true;
  FHistory.Add('OnAfterLoad');
end;

procedure TValidatingMock.OnAfterPersist;
begin
  FOnAfterPersist := true;
  FHistory.Add('OnAfterPersist');
end;

procedure TValidatingMock.OnAfterUpdate;
begin
  FOnAfterUpdate := true;
  FHistory.Add('OnAfterUpdate');
end;

procedure TValidatingMock.OnBeforeDelete;
begin
  FOnBeforeDelete := true;
  FHistory.Add('OnBeforeDelete');
end;

procedure TValidatingMock.OnBeforeInsert;
begin
  FOnBeforeInsert := true;
  FHistory.Add('OnBeforeInsert');
end;

procedure TValidatingMock.OnBeforeLoad;
begin
  FOnBeforeLoad := true;
  FHistory.Add('OnBeforeLoad');
end;

procedure TValidatingMock.OnBeforePersist;
begin
  FOnBeforePersist := true;
  FHistory.Add('OnBeforePersist');
end;

procedure TValidatingMock.OnBeforeUpdate;
begin
  FOnBeforeUpdate := true;
  FHistory.Add('OnBeforeUpdate');
end;

procedure TValidatingMock.UpdateValidate;
begin
  FUpdateValidate := true;
  FHistory.Add('UpdateValidate');
end;

procedure TValidatingMock.Validate;
begin
  FValidate := true;
  FHistory.Add('Validate');
end;

initialization

RegisterTest(TTestDuckTyping.Suite);

finalization

end.
