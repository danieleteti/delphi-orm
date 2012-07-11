unit ServerMethodsUnit;

interface

uses
  SysUtils,
  Classes,
  DSServer,
  DSAuth,
  dorm,
  BusinessObjects,
  Generics.Collections;

type
{$METHODINFO ON}
  TdormServerSample = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    Session: TSession;
  public
    { Public declarations }
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
    function GetPeople: TPeople;
    procedure LoadRelations(var APerson: TPerson);
    function LoadPersonByOID(OID: Integer): TPerson;
    function Persist(AObject: TObject): Integer;
    procedure Delete(AObject: TObject);
    function SearchByName(const AName: string): TObjectList<TPerson>;
  end;
{$METHODINFO OFF}

implementation

{$R *.dfm}


uses
  StrUtils,
  dorm.Commons,
  dorm.Collections,
  dorm.Filters,
  dorm.Utils,
  Windows;

procedure TdormServerSample.DataModuleCreate(Sender: TObject);
begin
  Session := TSession.CreateConfigured(
    TStreamReader.Create('dorm.conf'),
    TStreamReader.Create('dorm.mapping'),
    deDevelopment);
end;

procedure TdormServerSample.DataModuleDestroy(Sender: TObject);
begin
  Session.Free;
end;

procedure TdormServerSample.Delete(AObject: TObject);
begin
  Session.Delete(AObject);
end;

function TdormServerSample.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TdormServerSample.GetPeople: TPeople;
begin
  Result := TPeople.Create;
  Session.FillList<TPerson>(Result);
end;

function TdormServerSample.LoadPersonByOID(OID: Integer): TPerson;
begin
  Result := Session.Load<TPerson>(OID);
end;

procedure TdormServerSample.LoadRelations(var APerson: TPerson);
begin
  Session.LoadRelations(APerson, [HasMany, HasOne]);
end;

function TdormServerSample.Persist(AObject: TObject): Integer;
begin
  Session.Persist(AObject);
  Result := TdormUtils.GetProperty(AObject, 'ID').AsInteger;
end;

function TdormServerSample.ReverseString(Value: string): string;
begin
  Result := StrUtils.ReverseString(Value);
end;

function TdormServerSample.SearchByName(const AName: string)
  : TObjectList<TPerson>;
var
  Criteria: TdormCriteria;
begin
  Result := TObjectList<TPerson>.Create;
  Criteria := nil;
  if AName <> EmptyStr then
    Criteria := TdormCriteria.
      NewCriteria('FirstName', TdormCompareOperator.coEqual, AName).
      AddOr('LastName', TdormCompareOperator.coEqual, AName);
  Session.FillList<TPerson>(Result, Criteria);
end;

end.
