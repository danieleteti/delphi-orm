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
unit TestDORMMapping.Attributes;

interface

uses
  TestFramework,
  Rtti,
  superobject,
  dorm.Mappings,
  dorm.Mappings.Strategies,
  dorm.Utils;

{$RTTI EXPLICIT
  FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])
  METHODS([vcPrivate, vcProtected, vcPublic, vcPublished])
  PROPERTIES([vcPrivate, vcProtected, vcPublic, vcPublished])}

type
  TAttributesMappingStrategyTests = class(TTestCase)
  private
    FMapping: IMappingStrategy;
    FContext: TRttiContext;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetMappingEntity_ClassWithoutEntity;
    procedure TestGetMappingEntity_ClassJustWithEntity;
    procedure TestGetMappingEntity_CustomerWithEntityName;
    procedure TestGetMappingEntity_CustomerComplete;

    procedure TestGetMappingEntity_CarATR;
    procedure TestGetMappingEntity_EmailATR;
    procedure TestGetMappingEntity_PersonATR;
    procedure TestGetMappingEntity_PhoneATR;

    procedure TestGetMappingFields_CustomerComplete;
    procedure TestGetMappingFields_CarATR;
    procedure TestGetMappingFields_EmailATR;
    procedure TestGetMappingFields_PersonATR;
    procedure TestGetMappingFields_PhoneATR;

    procedure TestGetMappingHasOne;
    procedure TestGetMappingHasMany;
    procedure TestGetMappingBelongsTo;
  end;

implementation

uses
  Classes,
  Generics.Collections;

type
  TPersonATR = class;
  TPhoneATR = class;

{$IF CompilerVersion >= 23}

  TPhones = class(TObjectList<TPhoneATR>)
{$ELSE}
  TPhones = class(TdormObjectList<TPhoneATR>)
{$IFEND}
  end;

  TCustomer = class
  private
    FName: String;
    FId: Integer;
  public
    property Id: Integer read FId write FId;
    property Name: String read FName write FName;
  end;

  [Entity]
  TCustomerWithEntity = class
  end;

  [Entity('TBL_CUSTOMER')]
  TCustomerWithEntityName = class
  end;

  [Entity('TBL_CUSTOMER_COMPLETE', 'DBO')]
  TCustomerComplete = class
  private
    FLastName: string;
    FAge: Int32;
    FFirstName: string;
    FId: Integer;
    FBornDate: TDate;
    FBornTimeStamp: TDateTime;
    FValor: Double;
    FValor2: Extended;
    FValor3: Currency;
    FTransientProp: TDate;
  public
    [Id]
    property Id: Integer read FId write FId;
    [Column('CUSTOMER_FIRSTNAME')]
    [Size(100)]
    property FirstName: string read FFirstName write FFirstName;
    [Column('CUSTOMER_LASTNAME', 'String', 0, 0, 'Default LastName')]
    property LastName: string read FLastName write FLastName;
    [DefaultValue('58')]
    property Age: Int32 read FAge write FAge;
    property BornDate: TDate read FBornDate write FBornDate;
    property BornTimeStamp: TDateTime read FBornTimeStamp write FBornTimeStamp;
    [Size(15, 5)]
    property Valor: Double read FValor write FValor;
    [Column('VALOR_EXTENDED')]
    property Valor2: Extended read FValor2 write FValor2;
    [Column('VALOR_CURRENCY', 12, 2)]
    property Valor3: Currency read FValor3 write FValor3;
    [Transient]
    property TransientProp: TDate read FTransientProp write FTransientProp;
  end;

  [Entity('CARS', 'dorm.tests.bo')]
  TCarATR = class
  private
    FModel: string;
    FBrand: string;
    FPersonID: Integer;
    FId: Integer;
    FOwner: TPersonATR;
    // Private!!!
    [Column('ID_PERSON')]
    property PersonID: Integer read FPersonID write FPersonID;
  public
    [Id]
    [Column('IDCAR')]
    property Id: Integer read FId write FId;
    [Column('BRAND', 30)]
    property Brand: string read FBrand write FBrand;
    [Column('MODEL', 30)]
    property Model: string read FModel write FModel;
    [BelongsTo('PersonID', True)]
    property Owner: TPersonATR read FOwner write FOwner;
  end;

  [Entity('EMAILS', 'dorm.tests.bo')]
  TEmailATR = class
  private
    FValue: string;
    FPersonID: Integer;
    FId: Integer;
    // Private!!!
    FOwner: TPersonATR;
    [Column('ID_PERSON')]
    property PersonID: Integer read FPersonID write FPersonID;
  public
    [Id]
    [Column('IDMAIL')]
    property Id: Integer read FId write FId;
    [Column('ADDRESS', 100)]
    property Value: string read FValue write FValue;
    [BelongsTo('PersonID', True)]
    property Owner: TPersonATR read FOwner write FOwner;
  end;

  [Entity('PEOPLE', 'dorm.tests.bo')]
  TPersonATR = class
  private
    FLastName: string;
    FAge: Int32;
    FFirstName: string;
    FId: Integer;
    FBornDate: TDate;
    FPhones: TPhones;
    FCar: TCarATR;
    FEmail: TEmailATR;
    FBornTimeStamp: TDateTime;
    FPhoto: TStream;
    function GetFullName: string;
  public
    [Id]
    [Column('ID')]
    property Id: Integer read FId write FId;
    [Column('FIRST_NAME', 50)]
    property FirstName: string read FFirstName write FFirstName;
    [Column('LAST_NAME', 50)]
    property LastName: string read FLastName write FLastName;
    [Column('AGE')]
    property Age: Int32 read FAge write FAge;
    [Column('BORN_DATE')]
    property BornDate: TDate read FBornDate write FBornDate;
    [Column('BORN_DATE_TIME')]
    property BornTimeStamp: TDateTime read FBornTimeStamp write FBornTimeStamp;
    [Column('PHOTO')]
    property Photo: TStream read FPhoto write FPhoto;
    [HasMany('PersonID')]
    property Phones: TPhones read FPhones;
    [HasOne('PersonID')]
    property Car: TCarATR read FCar write FCar;
    [HasOne('PersonID')]
    property Email: TEmailATR read FEmail write FEmail;
    [Transient]
    property FullName: string read GetFullName;
  end;

  [Entity('PHONES', 'dorm.tests.bo')]
  TPhoneATR = class
  private
    FNumber: string;
    FModel: string;
    FId: Integer;
    FPersonID: Integer;
    // Private!!!
    [Column('ID_PERSON')]
    property PersonID: Integer read FPersonID write FPersonID;
  public
    [Id]
    [Column('ID')]
    property Id: Integer read FId write FId;
    [Column('NUMBER', 50)]
    property Number: string read FNumber write FNumber;
    [Column('MODEL', 50)]
    property Model: string read FModel write FModel;
  end;

  { TPersonATR }

function TPersonATR.GetFullName: string;
begin
  // Fake
end;

{ TAttributesMappingStrategyTests }

procedure TAttributesMappingStrategyTests.SetUp;
begin
  inherited;
  FContext := TRttiContext.Create;
  FMapping := TAttributesMappingStrategy.Create;
end;

procedure TAttributesMappingStrategyTests.TearDown;
begin
  inherited;
  FMapping := nil;
  FContext.Free;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingEntity_ClassWithoutEntity;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCustomer), _Table);
    CheckEquals('', _Table.TableName);
    CheckEquals('', _Table.Package);
    CheckEquals(0, _Table.Fields.Count);
    CheckEquals(0, _Table.HasOneList.Count);
    CheckEquals(0, _Table.HasManyList.Count);
    CheckEquals(0, _Table.BelongsToList.Count);
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingEntity_ClassJustWithEntity;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCustomerWithEntity), _Table);
    CheckEquals('', _Table.TableName);
    CheckEquals('', _Table.Package);
    CheckEquals(0, _Table.Fields.Count);
    CheckEquals(0, _Table.HasOneList.Count);
    CheckEquals(0, _Table.HasManyList.Count);
    CheckEquals(0, _Table.BelongsToList.Count);
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingEntity_CustomerWithEntityName;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCustomerWithEntityName), _Table);
    CheckEquals('TBL_CUSTOMER', _Table.TableName);
    CheckEquals('', _Table.Package);
    CheckEquals(0, _Table.Fields.Count);
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingEntity_CustomerComplete;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCustomerComplete), _Table);
    CheckEquals('TBL_CUSTOMER_COMPLETE', _Table.TableName);
    CheckEquals('DBO', _Table.Package);
    CheckEquals(7, _Table.Fields.Count);
    CheckEquals(0, _Table.HasOneList.Count);
    CheckEquals(0, _Table.HasManyList.Count);
    CheckEquals(0, _Table.BelongsToList.Count);
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingEntity_CarATR;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCarATR), _Table);
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

procedure TAttributesMappingStrategyTests.TestGetMappingEntity_EmailATR;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TEmailATR), _Table);
    CheckEquals('EMAILS', _Table.TableName);
    CheckEquals('dorm.tests.bo', _Table.Package);
    CheckEquals(3, _Table.Fields.Count);
    CheckEquals(0, _Table.HasOneList.Count);
    CheckEquals(0, _Table.HasManyList.Count);
    CheckEquals(1, _Table.BelongsToList.Count);
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingEntity_PersonATR;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPersonATR), _Table);
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

procedure TAttributesMappingStrategyTests.TestGetMappingEntity_PhoneATR;
var
  _Table: TMappingTable;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPhoneATR), _Table);
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

procedure TAttributesMappingStrategyTests.TestGetMappingFields_CustomerComplete;
var
  _Table: TMappingTable;
  _Field: TMappingField;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCustomerComplete), _Table);
    CheckEquals(_Table.Fields.Count, 7, 'Missing MappingFields');

    _Field := _Table.FindByName('Id');
    CheckNotNull(_Field, 'Property Id should be exists in TCustomerComplete');
    CheckTrue(_Field.IsPK, 'Property Id should be a primary key');
    CheckEquals('', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('FirstName');
    CheckNotNull(_Field, 'Property FirstName should be exists in TCustomerComplete');
    CheckFalse(_Field.IsPK, 'Property FirstName should not be a primary key');
    CheckEquals('CUSTOMER_FIRSTNAME', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(100, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('LastName');
    CheckNotNull(_Field, 'Property LastName should be exists in TCustomerComplete');
    CheckFalse(_Field.IsPK, 'Property LastName should not be a primary key');
    CheckEquals('CUSTOMER_LASTNAME', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('Default LastName', _Field.DefaultValue);

    _Field := _Table.FindByName('Age');
    CheckNotNull(_Field, 'Property Age should be exists in TCustomerComplete');
    CheckFalse(_Field.IsPK, 'Property Age should not be a primary key');
    CheckEquals('', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('58', _Field.DefaultValue);

    _Field := _Table.FindByName('BornDate');
    CheckNull(_Field, 'Property BornDate should be Transient');

    _Field := _Table.FindByName('Valor');
    CheckNotNull(_Field, 'Property Valor should be exists in TCustomerComplete');
    CheckFalse(_Field.IsPK, 'Property Valor should not be a primary key');
    CheckEquals('', _Field.FieldName);
    CheckEquals('float', _Field.FieldType);
    CheckEquals(15, _Field.Size);
    CheckEquals(5, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Valor2');
    CheckNotNull(_Field, 'Property Valor2 should be exists in TCustomerComplete');
    CheckFalse(_Field.IsPK, 'Property Valor2 should not be a primary key');
    CheckEquals('VALOR_EXTENDED', _Field.FieldName);
    CheckEquals('float', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Valor3');
    CheckNotNull(_Field, 'Property Valor3 should be exists in TCustomerComplete');
    CheckFalse(_Field.IsPK, 'Property Valor3 should not be a primary key');
    CheckEquals('VALOR_CURRENCY', _Field.FieldName);
    CheckEquals('decimal', _Field.FieldType);
    CheckEquals(12, _Field.Size);
    CheckEquals(2, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingFields_CarATR;
var
  _Table: TMappingTable;
  _Field: TMappingField;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TCarATR), _Table);

    _Field := _Table.FindByName('ID');
    CheckNotNull(_Field, 'TCarATR should be have property ID');
    CheckTrue(_Field.IsPK, 'TCarATR.Id should be a primary key');
    CheckEquals('IDCAR', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Brand');
    CheckNotNull(_Field, 'TCarATR should be have property Brand');
    CheckFalse(_Field.IsPK, 'TCarATR.Brand should not be a primary key');
    CheckEquals('BRAND', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(30, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Model');
    CheckNotNull(_Field, 'TCarATR should be have property Model');
    CheckFalse(_Field.IsPK, 'TCarATR.Model should not be a primary key');
    CheckEquals('MODEL', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(30, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('PersonID');
    CheckNotNull(_Field, 'TCarATR should be have property PersonID');
    CheckFalse(_Field.IsPK, 'TCarATR.PersonID should not be a primary key');
    CheckEquals('ID_PERSON', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingFields_EmailATR;
var
  _Table: TMappingTable;
  _Field: TMappingField;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TEmailATR), _Table);

    _Field := _Table.FindByName('ID');
    CheckNotNull(_Field, 'TEmailATR should be have property ID');
    CheckTrue(_Field.IsPK, 'TEmailATR.Id should be a primary key');
    CheckEquals('IDMAIL', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Value');
    CheckNotNull(_Field, 'TEmailATR should be have property Value');
    CheckFalse(_Field.IsPK, 'TEmailATR.Value should not be a primary key');
    CheckEquals('ADDRESS', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(100, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('PersonID');
    CheckNotNull(_Field, 'TEmailATR should be have property PersonID');
    CheckFalse(_Field.IsPK, 'TEmailATR.PersonID should not be a primary key');
    CheckEquals('ID_PERSON', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingFields_PersonATR;
var
  _Table: TMappingTable;
  _Field: TMappingField;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPersonATR), _Table);
    _Field := _Table.FindByName('ID');
    CheckNotNull(_Field, 'TPersonATR should be have property ID');
    CheckTrue(_Field.IsPK, 'TPersonATR.Id should be a primary key');
    CheckEquals('ID', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('FirstName');
    CheckNotNull(_Field, 'TPersonATR should be have property FirstName');
    CheckFalse(_Field.IsPK, 'TPersonATR.FirstName should not be a primary key');
    CheckEquals('FIRST_NAME', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(50, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('LastName');
    CheckNotNull(_Field, 'TPersonATR should be have property LastName');
    CheckFalse(_Field.IsPK, 'TPersonATR.LastName should not be a primary key');
    CheckEquals('LAST_NAME', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(50, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Age');
    CheckNotNull(_Field, 'TPersonATR should be have property AGE');
    CheckFalse(_Field.IsPK, 'TPersonATR.Age should not be a primary key');
    CheckEquals('AGE', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('BornDate');
    CheckNotNull(_Field, 'TPersonATR should be have property BornDate');
    CheckFalse(_Field.IsPK, 'TPersonATR.BornDate should not be a primary key');
    CheckEquals('BORN_DATE', _Field.FieldName);
    CheckEquals('date', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('BornTimeStamp');
    CheckNotNull(_Field, 'TPersonATR should be have property BornTimeStamp');
    CheckFalse(_Field.IsPK, 'TPersonATR.BornTimeStamp should not be a primary key');
    CheckEquals('BORN_DATE_TIME', _Field.FieldName);
    CheckEquals('datetime', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Photo');
    CheckNotNull(_Field, 'TPersonATR should be have property Photo');
    CheckFalse(_Field.IsPK, 'TPersonATR.Photo should not be a primary key');
    CheckEquals('PHOTO', _Field.FieldName);
    CheckEquals('blob', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    CheckNull(_Table.FindByName('FullName'),
      'TPersonATR.FullName should be a non persistent property');
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingFields_PhoneATR;
var
  _Table: TMappingTable;
  _Field: TMappingField;
begin
  _Table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPhoneATR), _Table);

    _Field := _Table.FindByName('ID');
    CheckNotNull(_Field, 'TPhoneATR should be have property ID');
    CheckTrue(_Field.IsPK, 'TPhoneATR.Id should be a primary key');
    CheckEquals('ID', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Number');
    CheckNotNull(_Field, 'TPhoneATR should be have property Number');
    CheckFalse(_Field.IsPK, 'TPhoneATR.Number should not be a primary key');
    CheckEquals('NUMBER', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(50, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('Model');
    CheckNotNull(_Field, 'TPhoneATR should be have property Model');
    CheckFalse(_Field.IsPK, 'TPhoneATR.Number should not be a primary key');
    CheckEquals('MODEL', _Field.FieldName);
    CheckEquals('string', _Field.FieldType);
    CheckEquals(50, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);

    _Field := _Table.FindByName('PersonID');
    CheckNotNull(_Field, 'TPhoneATR should be have property PersonID');
    CheckFalse(_Field.IsPK, 'TPhoneATR.PersonID should not be a primary key');
    CheckEquals('ID_PERSON', _Field.FieldName);
    CheckEquals('integer', _Field.FieldType);
    CheckEquals(0, _Field.Size);
    CheckEquals(0, _Field.Precision);
    CheckEquals('', _Field.DefaultValue);
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingHasOne;
var
  _Table: TMappingTable;
  _CustomerType: TRttiType;
  _Relation: TMappingRelation;
begin
  _Table := TMappingTable.Create;
  try
    _CustomerType := FContext.GetType(TPersonATR);
    FMapping.GetMapping(_CustomerType, _Table);
    CheckEquals(_Table.HasOneList.Count, 2, 'Invalid count of HasOne relations in TPersonATR');

    _Relation := _Table.FindHasOneByName('Car');
    CheckNotNull(_Relation, 'TPersonATR.Car property should be a HasOne relation');
    with _Relation do
    begin
      CheckEquals(_Relation.ChildClassName, 'TCarATR');
      CheckEquals(_Relation.ChildFieldName, 'PersonID');
      CheckFalse(_Relation.LazyLoad, 'TPersonATR.Car should be not lazyload');
    end;

    _Relation := _Table.FindHasOneByName('Email');
    CheckNotNull(_Relation, 'TPersonATR.Email property should be a HasOne relation');
    with _Relation do
    begin
      CheckEquals(_Relation.ChildClassName, 'TEmailATR');
      CheckEquals(_Relation.ChildFieldName, 'PersonID');
      CheckFalse(_Relation.LazyLoad, 'TPersonATR.Email should be not lazyload');
    end;
  finally
    _Table.Free;
  end;
end;

procedure TAttributesMappingStrategyTests.TestGetMappingHasMany;
begin

end;

procedure TAttributesMappingStrategyTests.TestGetMappingBelongsTo;
begin

end;

initialization

RegisterTest(TAttributesMappingStrategyTests.Suite);

end.
