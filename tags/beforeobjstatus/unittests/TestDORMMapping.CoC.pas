unit TestDORMMapping.CoC;

interface

uses
  TestFramework,
  Generics.Collections,
  Rtti,
  dorm.Mappings.Strategies;

{$RTTI EXPLICIT
  FIELDS([vcPrivate, vcProtected, vcPublic, vcPublished])
  METHODS([vcPrivate, vcProtected, vcPublic, vcPublished])
  PROPERTIES([vcPrivate, vcProtected, vcPublic, vcPublished])}

type
  TCocMappingStrategyTests = class(TTestCase)
  private
    FMapping: IMappingStrategy;
    FContext: TRttiContext;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGetMappingEntity_EmailCoC;
    procedure TestGetMappingEntity_PersonCoC;
    procedure TestGetMappingEntity_PhoneCoC;
    procedure TestGetMappingEntity_LaptopCoC;

    procedure TestGetMappingFields_EmailCoC;
    procedure TestGetMappingFields_PersonCoC;
    procedure TestGetMappingFields_PhoneCoC;
    procedure TestGetMappingFields_LaptopCoC;

    procedure TestGetMappingPersonHasOneCar;
    procedure TestGetMappingPersonHasOneEmail;

    procedure TestGetMappingLaptopBelongToPerson;

    procedure TestGetMappingPersonHasManyPhones;
  end;

implementation

uses
  Classes,
  dorm.Mappings;

type
  TPersonCoC = class;

  TPhoneCoC = class
  private
    FNumber: string;
    FModel: string;
    FID: Integer;
    FPersonCoCID: Integer;
    // Private!!!
    property PersonCoCID: Integer read FPersonCoCID write FPersonCoCID;
  public
    property ID: Integer read FID write FID;
    property Number: string read FNumber write FNumber;
    property Model: string read FModel write FModel;
  end;

{$IF CompilerVersion >= 23}

  TPhonesCoC = class(TObjectList<TPhoneCoC>)
{$ELSE}
  TPhonesCoC = class(TdormObjectList<TPhoneCoC>)
{$IFEND}
  end;

  TCarCoC = class
  private
    FModel: string;
    FBrand: string;
    FPersonCoCID: Integer;
    FID: Integer;
    FOwner: TPersonCoC;
    // Private!!!
    property PersonCoCID: Integer read FPersonCoCID write FPersonCoCID;
  public
    property ID: Integer read FID write FID;
    property Brand: string read FBrand write FBrand;
    property Model: string read FModel write FModel;
    property Owner: TPersonCoC read FOwner write FOwner;
  end;

  TEmailCoC = class
  private
    FValue: string;
    FPersonID: Integer;
    FID: Integer;
    // Private!!!
    property PersonID: Integer read FPersonID write FPersonID;
  public
    property ID: Integer read FID write FID;
    property Value: string read FValue write FValue;
  end;

  TLaptopCoC = class;

{$IF CompilerVersion >= 23}

  TLaptopsCoC = class(TObjectList<TLaptopCoC>)
{$ELSE}
  TLaptopsCoC = class(TdormObjectList<TLaptopCoC>)
{$IFEND}
  end;

  TPersonCoC = class
  private
    FLastName: string;
    FAge: Int32;
    FFirstName: string;
    FID: Integer;
    FBornDate: TDate;
    FPhones: TPhonesCoC;
    FCar: TCarCoC;
    FEmail: TEmailCoC;
    FBornTimeStamp: TDateTime;
    FPhoto: TStream;
    FLaptops: TLaptopsCoC;
    function GetFullName: string;
  public
    property ID: Integer read FID write FID;
    property FirstName: string read FFirstName write FFirstName;
    property LastName: string read FLastName write FLastName;
    property Age: Int32 read FAge write FAge;
    property BornDate: TDate read FBornDate write FBornDate;
    property BornTimeStamp: TDateTime read FBornTimeStamp write FBornTimeStamp;
    property Photo: TStream read FPhoto write FPhoto;
    property Phones: TPhonesCoC read FPhones;
    property Car: TCarCoC read FCar write FCar;
    property Email: TEmailCoC read FEmail write FEmail;
    property Laptops: TLaptopsCoC read FLaptops write FLaptops;
    [Transient]
    property FullName: string read GetFullName;
  end;

  TLaptopCoC = class
  private
    FModel: String;
    FPersonCoCID: Integer;
    FID: Integer;
    FOwner: TPersonCoC;
    property PersonCoCID: Integer read FPersonCoCID write FPersonCoCID;
  public
    constructor Create(AModel: String); overload;
    property Owner: TPersonCoC read FOwner write FOwner;
    property ID: Integer read FID write FID;
    property Model: String read FModel write FModel;
  end;

  { TPersonCoC }

function TPersonCoC.GetFullName: string;
begin
  // Fake
end;

{ TCocMappingStrategyTests }

procedure TCocMappingStrategyTests.SetUp;
begin
  inherited;
  FContext := TRttiContext.Create;
  FMapping := TCoCMappingStrategy.Create;
end;

procedure TCocMappingStrategyTests.TearDown;
begin
  inherited;
  FMapping := nil;
  FContext.Free;
end;

procedure TCocMappingStrategyTests.TestGetMappingEntity_EmailCoC;
var
  table: TMappingTable;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TEmailCoC), table);
    CheckEquals('EMAILCOC', table.TableName);
    CheckEquals('TestDORMMapping.CoC', table.Package);
    CheckEquals(3, table.Fields.Count, 'Fields.Count error');
    CheckEquals(0, table.HasOneList.Count, 'HasOneList.Count error');
    CheckEquals(0, table.HasManyList.Count, 'HasManyList.Count error');
    CheckEquals(0, table.BelongsToList.Count, 'BelongsToList.Count error');
  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingEntity_LaptopCoC;
var
  table: TMappingTable;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TLaptopCoC), table);
    CheckEquals('LAPTOPCOC', table.TableName);
    CheckEquals('TestDORMMapping.CoC', table.Package);
    CheckEquals(3, table.Fields.Count, 'Fields.Count error');
    CheckEquals(0, table.HasOneList.Count, 'HasOneList.Count error');
    CheckEquals(0, table.HasManyList.Count, 'HasManyList.Count error');
    CheckEquals(1, table.BelongsToList.Count, 'BelongsToList.Count error');
  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingEntity_PersonCoC;
var
  table: TMappingTable;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPersonCoC), table);
    CheckEquals('PERSONCOC', table.TableName);
    CheckEquals('TestDORMMapping.CoC', table.Package);
    CheckEquals(7, table.Fields.Count, 'Fields.Count error');
    // TEmailCoC has propositaly PersonID instead PersonCoCID, for this reason we
    // have here just 1 HasOne instead 2.
    CheckEquals(1, table.HasOneList.Count, 'HasOneList.Count error');
    CheckEquals(2, table.HasManyList.Count, 'HasManyList.Count error');
    CheckEquals(0, table.BelongsToList.Count, 'BelongsToList.Count error');
  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingEntity_PhoneCoC;
var
  table: TMappingTable;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPhoneCoC), table);
    CheckEquals('PHONECOC', table.TableName);
    CheckEquals('TestDORMMapping.CoC', table.Package);
    CheckEquals(4, table.Fields.Count, 'Fields.Count error');
    CheckEquals(0, table.HasOneList.Count, 'HasOneList.Count error');
    CheckEquals(0, table.HasManyList.Count, 'HasManyList.Count error');
    CheckEquals(0, table.BelongsToList.Count, 'BelongsToList.Count error');
  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingFields_EmailCoC;
var
  table: TMappingTable;
  field: TMappingField;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TEmailCoC), table);

    field := table.FindByName('ID');
    CheckNotNull(field, 'TEmailCoC should be have property ID');
    CheckTrue(field.IsPK, 'TEmailCoC.Id should be a primary key');
    CheckEquals('ID', field.FieldName);
    CheckEquals('integer', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('Value');
    CheckNotNull(field, 'TEmailCoC should be have property Value');
    CheckFalse(field.IsPK, 'TEmailCoC.Value should not be a primary key');
    CheckEquals('VALUE', field.FieldName);
    CheckEquals('string', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingFields_LaptopCoC;
var
  table: TMappingTable;
  field: TMappingField;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TLaptopCoC), table);

    field := table.FindByName('ID');
    CheckNotNull(field, 'TLaptopCoC should be have property ID');
    CheckTrue(field.IsPK, 'TLaptopCoC.Id should be a primary key');
    CheckEquals('ID', field.FieldName);
    CheckEquals('integer', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('Model');
    CheckNotNull(field, 'TLaptopCoC should be have property Model');
    CheckFalse(field.IsPK, 'TLaptopCoC.Model should not be a primary key');
    CheckEquals('MODEL', field.FieldName);
    CheckEquals('string', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingFields_PersonCoC;
var
  table: TMappingTable;
  field: TMappingField;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPersonCoC), table);
    field := table.FindByName('ID');
    CheckNotNull(field, 'TPersonCoC should be have property ID');
    CheckTrue(field.IsPK, 'TPersonCoC.Id should be a primary key');
    CheckEquals('ID', field.FieldName);
    CheckEquals('integer', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('FirstName');
    CheckNotNull(field, 'TPersonCoC should be have property FirstName');
    CheckFalse(field.IsPK, 'TPersonCoC.FirstName should not be a primary key');
    CheckEquals('FIRSTNAME', field.FieldName);
    CheckEquals('string', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('LastName');
    CheckNotNull(field, 'TPersonCoC should be have property LastName');
    CheckFalse(field.IsPK, 'TPersonCoC.LastName should not be a primary key');
    CheckEquals('LASTNAME', field.FieldName);
    CheckEquals('string', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('Age');
    CheckNotNull(field, 'TPersonCoC should be have property AGE');
    CheckFalse(field.IsPK, 'TPersonCoC.Age should not be a primary key');
    CheckEquals('AGE', field.FieldName);
    CheckEquals('integer', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('BornDate');
    CheckNotNull(field, 'TPersonCoC should be have property BornDate');
    CheckFalse(field.IsPK, 'TPersonCoC.BornDate should not be a primary key');
    CheckEquals('BORNDATE', field.FieldName);
    CheckEquals('date', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('BornTimeStamp');
    CheckNotNull(field, 'TPersonCoC should be have property BornTimeStamp');
    CheckFalse(field.IsPK, 'TPersonCoC.BornTimeStamp should not be a primary key');
    CheckEquals('BORNTIMESTAMP', field.FieldName);
    CheckEquals('datetime', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('Photo');
    CheckNotNull(field, 'TPersonCoC should be have property Photo');
    CheckFalse(field.IsPK, 'TPersonCoC.Photo should not be a primary key');
    CheckEquals('PHOTO', field.FieldName);
    CheckEquals('blob', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    CheckNull(table.FindByName('FullName'),
      'TPersonCoC.FullName should be a non persistent property');
  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingFields_PhoneCoC;
var
  table: TMappingTable;
  field: TMappingField;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPhoneCoC), table);

    field := table.FindByName('ID');
    CheckNotNull(field, 'TPhoneCoC should be have property ID');
    CheckTrue(field.IsPK, 'TPhoneCoC.Id should be a primary key');
    CheckEquals('ID', field.FieldName);
    CheckEquals('integer', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('Number');
    CheckNotNull(field, 'TPhoneCoC should be have property Number');
    CheckFalse(field.IsPK, 'TPhoneCoC.Number should not be a primary key');
    CheckEquals('NUMBER', field.FieldName);
    CheckEquals('string', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

    field := table.FindByName('Model');
    CheckNotNull(field, 'TPhoneCoC should be have property Model');
    CheckFalse(field.IsPK, 'TPhoneCoC.Number should not be a primary key');
    CheckEquals('MODEL', field.FieldName);
    CheckEquals('string', field.FieldType);
    CheckEquals(0, field.Size);
    CheckEquals(0, field.Precision);
    CheckEquals('', field.DefaultValue);

  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingPersonHasOneCar;
var
  table: TMappingTable;
  relation: TMappingRelation;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPersonCoC), table);
    relation := table.FindHasOneByName('Car');
    CheckNotNull(relation, 'TPersonCoC.Car property should be a HasOne relation');
    with relation do
    begin
      CheckEquals('TCarCoC', relation.ChildClassName);
      CheckEquals('PersonCoCID', relation.ChildFieldName);
      CheckFalse(relation.LazyLoad, 'TPersonCoC.Car should be not lazyload');
    end;
  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingPersonHasOneEmail;
var
  table: TMappingTable;
  relation: TMappingRelation;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPersonCoC), table);
    relation := table.FindHasOneByName('Email');
    CheckNull(relation,
      'TPersonCoC.Email property dont should be a HasOne relation because not has PersosCoCID property');
  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingLaptopBelongToPerson;
var
  table: TMappingTable;
  relation: TMappingBelongsTo;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TLaptopCoC), table);
    relation := table.FindBelongsToByName('Owner');
    CheckNotNull(relation, 'TLaptopCoC.Owner property should be a BelongsTo relation');
    with relation do
    begin
      CheckEquals('TPersonCoC', relation.OwnerClassName);
      CheckEquals('PersonCoCID', relation.RefFieldName);
      CheckTrue(relation.LazyLoad, 'TLaptopCoC.Owner should be lazyload');
    end;
  finally
    table.Free;
  end;
end;

procedure TCocMappingStrategyTests.TestGetMappingPersonHasManyPhones;
var
  table: TMappingTable;
  relation: TMappingRelation;
begin
  table := TMappingTable.Create;
  try
    FMapping.GetMapping(FContext.GetType(TPersonCoC), table);
    relation := table.FindHasManyByName('Phones');
    CheckNotNull(relation, 'TPersonCoC.Phones property should be a HasMany relation');
    with relation do
    begin
      CheckEquals('TPhoneCoC', relation.ChildClassName);
      CheckEquals('PersonCoCID', relation.ChildFieldName);
      CheckFalse(relation.LazyLoad, 'TPersonCoC.Phones should be not lazyload');
    end;
  finally
    table.Free;
  end;
end;

{ TLaptop }

constructor TLaptopCoC.Create(AModel: String);
begin
  inherited Create;
  FModel := AModel;
end;

initialization

RegisterTest(TCocMappingStrategyTests.Suite);

end.
