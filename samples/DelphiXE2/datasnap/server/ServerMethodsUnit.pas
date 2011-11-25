unit ServerMethodsUnit;

interface

uses System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth, dorm,
  BusinessObjects, Generics.Collections;

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
    function GetPeople: TObjectList<TPerson>;
    procedure LoadRelations(var APerson: TPerson);
    function LoadPersonByOID(OID: Integer): TPerson;
    function Persist(AObject: TObject): Integer;
    procedure Delete(AObject: TObject);
    function SearchByName(const AName: String): TObjectList<TPerson>;
  end;
{$METHODINFO OFF}

implementation

{$R *.dfm}


uses System.StrUtils, dorm.Commons, dorm.Collections, dorm.Utils;

procedure TdormServerSample.DataModuleCreate(Sender: TObject);
var
  ConfigFileName: string;
begin
{$IFDEF SQLITE3_STRATEGY}
  ConfigFileName := 'dorm_sqlite3.conf';
{$ENDIF}
{$IFDEF FIREBIRD_STRATEGY}
  ConfigFileName := 'dorm_firebird.conf';
{$ENDIF}
{$IFDEF INTERBASE_STRATEGY}
  ConfigFileName := 'dorm_interbase.conf';
{$ENDIF}
  Session := TSession.CreateConfigured(TStreamReader.Create(ConfigFileName),
    deDevelopment);
//  Session := TSession.CreateConfigured(TStreamReader.Create('dorm.conf'),
//    deDevelopment);
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

function TdormServerSample.GetPeople: TObjectList<TPerson>;
begin
  Result := TObjectList<TPerson>.Create;
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
  Result := System.StrUtils.ReverseString(Value);
end;

function TdormServerSample.SearchByName(const AName: String)
  : TObjectList<TPerson>;
var
  Criteria: TdormCriteria;
begin
  Result := TObjectList<TPerson>.Create;
  Criteria := nil;
  if AName <> EmptyStr then
    Criteria := TdormCriteria.
      NewCriteria('FirstName', TdormCompareOperator.Equal, AName).
      AddOr('LastName', TdormCompareOperator.Equal, AName);
  Session.FillList<TPerson>(Result, Criteria);
end;

end.
