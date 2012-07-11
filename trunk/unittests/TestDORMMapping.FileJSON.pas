{ *******************************************************************************
  Copyright 2010-2012 Daniele Teti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.

  Author: Marcos Barreto
  ********************************************************************************}
unit TestDORMMapping.FileJSON;

interface

uses
  TestFramework,
  superobject,
  Rtti,
  dorm.Mappings.Strategies,
  dorm.Mappings;

type
  TFileMappingStrategyTests = class(TTestCase)
  private
    FJson: ISuperObject;
    FMapping: IMappingStrategy;
    FContext: TRttiContext;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetMappingEntity_Car;
    procedure TestGetMappingEntity_Email;
    procedure TestGetMappingEntity_Person;
    procedure TestGetMappingEntity_Phone;

    procedure TestGetMappingFields_Car;
    procedure TestGetMappingFields_Person;
    procedure TestGetMappingFields_Email;
    procedure TestGetMappingFields_Phone;

    procedure TestGetMappingPersonHasOneCar;
    procedure TestGetMappingPersonHasOneEmail;

    procedure TestGetMappingPersonHasManyPhones;

    procedure TestGetMappingCarBelongsToPerson;
  end;

implementation

uses
  Classes,
  dorm.tests.bo;


{ TFileMappingStrategyTests }

procedure TFileMappingStrategyTests.SetUp;
var
  _Reader: TStreamReader;
  _Content: String;
begin
  inherited;
  _Reader := TStreamReader.Create('dorm_tests.mapping');
  try
    _Content := _Reader.ReadToEnd;
    FJson := TSuperObject.ParseString(PChar(_Content), True);
  finally
    _Reader.Free;
  end;
  FMapping := TFileMappingStrategy.Create(FJson);
  FContext := TRttiContext.Create;
end;

procedure TFileMappingStrategyTests.TearDown;
begin
  inherited;
  FJson := nil;
  FMapping := nil;
  FContext.Free;
end;

procedure TFileMappingStrategyTests.TestGetMappingEntity_Car;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCar), _Table);
    CheckEquals('CARS', _Table.TableName);
    CheckEquals('dorm.tests.bo', _Table.Package);
    CheckEquals(4, _Table.Fields.Count);
    CheckEquals(0, _Table.HasOneList.Count);
    CheckEquals(0, _Table.HasManyList.Count);
    CheckEquals(1, _Table.BelongsToList.Count);
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingEntity_Email;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TEmail), _Table);
    CheckEquals('EMAILS', _Table.TableName);
    CheckEquals('dorm.tests.bo', _Table.Package);
    CheckEquals(3, _Table.Fields.Count);
    CheckEquals(0, _Table.HasOneList.Count);
    CheckEquals(0, _Table.HasManyList.Count);
    CheckEquals(0, _Table.BelongsToList.Count);
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingEntity_Person;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPerson), _Table);
    CheckEquals('PEOPLE', _Table.TableName);
    CheckEquals('dorm.tests.bo', _Table.Package);
    CheckEquals(7, _Table.Fields.Count);
    CheckEquals(2, _Table.HasOneList.Count);
    CheckEquals(1, _Table.HasManyList.Count);
    CheckEquals(0, _Table.BelongsToList.Count);
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingEntity_Phone;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPhone), _Table);
    CheckEquals('PHONES', _Table.TableName);
    CheckEquals('dorm.tests.bo', _Table.Package);
    CheckEquals(4, _Table.Fields.Count);
    CheckEquals(0, _Table.HasOneList.Count);
    CheckEquals(0, _Table.HasManyList.Count);
    CheckEquals(0, _Table.BelongsToList.Count);
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingFields_Car;
var
  _Table: TMappingTable;
  _Field: TMappingField;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCar), _Table);

    _Field := _Table.FindByName('ID');
    CheckNotNull(_Field, 'TCar should be have property ID');
    CheckTrue(_Field.IsPK, 'TCar.Id should be a primary key');
    CheckEquals('ID', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Brand');
    CheckNotNull(_Field, 'TCar should be have property Brand');
    CheckFalse(_Field.IsPK, 'TCar.Brand should not be a primary key');
    CheckEquals('BRAND', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(30, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Model');
    CheckNotNull(_Field, 'TCar should be have property Model');
    CheckFalse(_Field.IsPK, 'TCar.Model should not be a primary key');
    CheckEquals('MODEL', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(30, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('PersonID');
    CheckNotNull(_Field, 'TCar should be have property PersonID');
    CheckFalse(_Field.IsPK, 'TCar.PersonID should not be a primary key');
    CheckEquals('ID_PERSON', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingFields_Email;
var
  _Table: TMappingTable;
  _Field: TMappingField;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TEmail), _Table);

    _Field := _Table.FindByName('ID');
    CheckNotNull(_Field, 'TEmail should be have property ID');
    CheckTrue(_Field.IsPK, 'TEmail.Id should be a primary key');
    CheckEquals('ID', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Value');
    CheckNotNull(_Field, 'TEmail should be have property Value');
    CheckFalse(_Field.IsPK, 'TEmail.Value should not be a primary key');
    CheckEquals('ADDRESS', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(100, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('PersonID');
    CheckNotNull(_Field, 'TEmail should be have property PersonID');
    CheckFalse(_Field.IsPK, 'TEmail.PersonID should not be a primary key');
    CheckEquals('ID_PERSON', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingFields_Person;
var
  _Table: TMappingTable;
  _Field: TMappingField;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPerson), _Table);
    _Field := _Table.FindByName('ID');
    CheckNotNull(_Field, 'TPerson should be have property ID');
    CheckTrue(_Field.IsPK, 'TPerson.Id should be a primary key');
    CheckEquals('ID', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('FirstName');
    CheckNotNull(_Field, 'TPerson should be have property FirstName');
    CheckFalse(_Field.IsPK, 'TPerson.FirstName should not be a primary key');
    CheckEquals('FIRST_NAME', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(50, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('LastName');
    CheckNotNull(_Field, 'TPerson should be have property LastName');
    CheckFalse(_Field.IsPK, 'TPerson.LastName should not be a primary key');
    CheckEquals('LAST_NAME', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(50, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Age');
    CheckNotNull(_Field, 'TPerson should be have property AGE');
    CheckFalse(_Field.IsPK, 'TPerson.Age should not be a primary key');
    CheckEquals('AGE', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('BornDate');
    CheckNotNull(_Field, 'TPerson should be have property BornDate');
    CheckFalse(_Field.IsPK, 'TPerson.BornDate should not be a primary key');
    CheckEquals('BORN_DATE', _Field.FieldName);
    CheckEquals('date', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('BornTimeStamp');
    CheckNotNull(_Field, 'TPerson should be have property BornTimeStamp');
    CheckFalse(_Field.IsPK, 'TPerson.BornTimeStamp should not be a primary key');
    CheckEquals('BORN_DATE_TIME', _Field.FieldName);
    CheckEquals('datetime', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Photo');
    CheckNotNull(_Field, 'TPerson should be have property Photo');
    CheckFalse(_Field.IsPK, 'TPerson.Photo should not be a primary key');
    CheckEquals('PHOTO', _Field.FieldName);
    CheckEquals('blob', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    CheckNull(_Table.FindByName('FullName'),
      'TPerson.FullName should be a non persistent property');
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingFields_Phone;
var
  _Table: TMappingTable;
  _Field: TMappingField;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPhone), _Table);

    _Field := _Table.FindByName('ID');
    CheckNotNull(_Field, 'TPhone should be have property ID');
    CheckTrue(_Field.IsPK, 'TPhone.Id should be a primary key');
    CheckEquals('ID', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Number');
    CheckNotNull(_Field, 'TPhone should be have property Number');
    CheckFalse(_Field.IsPK, 'TPhone.Number should not be a primary key');
    CheckEquals('NUMBER', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(50, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Model');
    CheckNotNull(_Field, 'TPhone should be have property Model');
    CheckFalse(_Field.IsPK, 'TPhone.Number should not be a primary key');
    CheckEquals('MODEL', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(50, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('PersonID');
    CheckNotNull(_Field, 'TPhone should be have property PersonID');
    CheckFalse(_Field.IsPK, 'TPhone.PersonID should not be a primary key');
    CheckEquals('ID_PERSON', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingPersonHasOneCar;
var
  _Table: TMappingTable;
  _Relation: TMappingRelation;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPerson), _Table);
    _Relation := _Table.FindHasOneByName('Car');
    CheckNotNull(_Relation, 'TPerson.Car property should be a HasOne relation');
    with _Relation do
    begin
      CheckEquals('TCar', _Relation.ChildClassName);
      CheckEquals('PersonID', _Relation.ChildFieldName);
      CheckFalse(_Relation.LazyLoad, 'TPerson.Car should be not lazyload');
    end;
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingPersonHasOneEmail;
var
  _Table: TMappingTable;
  _Relation: TMappingRelation;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPerson), _Table);
    _Relation := _Table.FindHasOneByName('Email');
    CheckNotNull(_Relation, 'TPerson.Email property should be a HasOne relation');
    with _Relation do
    begin
      CheckEquals('TEmail', _Relation.ChildClassName);
      CheckEquals('PersonID', _Relation.ChildFieldName);
      CheckFalse(_Relation.LazyLoad, 'TPerson.Email should be not lazyload');
    end;
  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingPersonHasManyPhones;
var
  _Table: TMappingTable;
  _Relation: TMappingRelation;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPerson), _Table);

    _Relation := _Table.FindHasManyByName('Phones');
    CheckNotNull(_Relation, 'TPerson.Phones property should be a HasMany relation');
    with _Relation do
    begin
      CheckEquals('TPhone', _Relation.ChildClassName);
      CheckEquals('PersonID', _Relation.ChildFieldName);
      CheckFalse(_Relation.LazyLoad, 'TPerson.Phones should be not lazyload');
    end;

  finally
    _Table.Free;
  end;
end;

procedure TFileMappingStrategyTests.TestGetMappingCarBelongsToPerson;
var
  _Table: TMappingTable;
  _Relation: TMappingBelongsTo;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCar), _Table);
    _Relation := _Table.FindBelongsToByName('Owner');
    CheckNotNull(_Relation, 'TPerson.Owner property should be a BelongsTo relation');
    with _Relation do
    begin
      CheckEquals('TPerson', _Relation.OwnerClassName);
      CheckEquals('PersonID', _Relation.RefFieldName);
      CheckTrue(_Relation.LazyLoad, 'TPerson.Phones should be not lazyload');
    end;
  finally
    _Table.Free;
  end;
end;

initialization
  RegisterTest(TFileMappingStrategyTests.Suite);


  {
  var
  Table: TMappingTable;
  Field: TMappingField;
  Rel: TMappingRelation;
begin
  Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FPersonType, Table);
    CheckEquals('', Table.TableName);
    CheckEquals('dorm.tests.bo', Table.Package);
    CheckEquals(7, Table.Fields.Count);

    Field := FindField(Table, 'ID');
    CheckEquals('ID', Field.FieldName);
    CheckEquals('integer', Field.FieldType);
    CheckEquals(0, Field.Size);

    Field := FindField(Table, 'FirstName');
    CheckEquals('FIRST_NAME', Field.FieldName);
    CheckEquals('string', Field.FieldType);
    CheckEquals(50, Field.Size);

    Field := FindField(Table, 'LastName');
    CheckEquals('LAST_NAME', Field.FieldName);
    CheckEquals('string', Field.FieldType);
    CheckEquals(50, Field.Size);

    Field := FindField(Table, 'Age');
    CheckEquals('AGE', Field.FieldName);
    CheckEquals('integer', Field.FieldType);
    CheckEquals(0, Field.Size);

    Field := FindField(Table, 'BornDate');
    CheckEquals('BORN_DATE', Field.FieldName);
    CheckEquals('date', Field.FieldType);
    CheckEquals(0, Field.Size);

    Field := FindField(Table, 'BornTimeStamp');
    CheckEquals('BORN_DATE_TIME', Field.FieldName);
    CheckEquals('datetime', Field.FieldType);
    CheckEquals(0, Field.Size);

    Field := FindField(Table, 'Photo');
    CheckEquals('PHOTO', Field.FieldName);
    CheckEquals('blob', Field.FieldType);
    CheckEquals(0, Field.Size);

    CheckEquals(2, Table.HasOneList.Count);
    Rel := FindHasOne(Table, 'Car');
    CheckEquals('TCar', Rel.ChildClassname);
    CheckEquals('PersonID', Rel.ChildFieldName);
    CheckFalse(Rel.LazyLoad);

    Rel := FindHasOne(Table, 'Email');
    CheckEquals('TEmail', Rel.ChildClassName);
    CheckEquals('PersonID', Rel.ChildFieldName);
    CheckFalse(Rel.LazyLoad);

    CheckEquals(1, Table.HasManyList.Count);
    Rel := FindHasMany(Table, 'Phones');
    CheckEquals('TPhone', Rel.ChildClassName);
    CheckEquals('PersonID', Rel.ChildFieldName);
    CheckFalse(Rel.LazyLoad);
  finally
    Table.Free;
  end;
  }

end.
