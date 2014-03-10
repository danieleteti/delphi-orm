unit ServerMethodsUnit;

interface

uses System.SysUtils,
  System.Classes,
  Datasnap.DSServer,
  Datasnap.DSAuth,
  dorm,
  dorm.Configuration,
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


uses System.StrUtils,
  dorm.Commons,
  dorm.Collections,
  dorm.Utils,
  dorm.Filters;

procedure TdormServerSample.DataModuleCreate(Sender: TObject);
var
  ConfigFileName: string;
begin
{$IFDEF SQLITE3_STRATEGY}
  ConfigFileName := 'dorm_sqlite3.conf';
{$ENDIF}
{$IFDEF INTERBASE_STRATEGY}
  ConfigFileName := 'dorm_interbase.conf';
{$ENDIF}
{$IFDEF FIREBIRD_STRATEGY}
  ConfigFileName := 'dorm_firebird.conf';
{$ENDIF}
{$IFDEF FIREBIRD_UIB_STRATEGY}
  ConfigFileName := 'dorm_firebird_uib.conf';
{$ENDIF}
{$IFDEF INTERBASE_UIB_STRATEGY}
  ConfigFileName := 'dorm_interbase_uib.conf';
{$ENDIF}
{$IFDEF SQLSERVER_STRATEGY}
  ConfigFileName := 'dorm_sqlserver.conf';
{$ENDIF}
  Session := TSession.CreateConfigured(
    TStreamReader.Create(ConfigFileName),
    TStreamReader.Create('samples.mapping'),

    deDevelopment);
  Session.StartTransaction;
end;

procedure TdormServerSample.DataModuleDestroy(Sender: TObject);
begin
  Session.Free;
end;

procedure TdormServerSample.Delete(AObject: TObject);
begin
  Session.Delete(AObject);
  Session.Commit(true);
end;

function TdormServerSample.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TdormServerSample.GetPeople: TObjectList<TPerson>;
begin
  Result := TObjectList<TPerson>.Create;
  Session.FillList<TPerson>(Result);
  Session.Commit(true);
end;

function TdormServerSample.LoadPersonByOID(OID: Integer): TPerson;
begin
  Result := Session.Load<TPerson>(OID);
  Session.Commit(true);
end;

procedure TdormServerSample.LoadRelations(var APerson: TPerson);
begin
  Session.LoadRelations(APerson, [drHasMany, drHasOne]);
  Session.Commit(true);
end;

function TdormServerSample.Persist(AObject: TObject): Integer;
begin
  Session.Persist(AObject);
  Result := TdormUtils.GetProperty(AObject, 'ID').AsInteger;
  Session.Commit(true);
end;

function TdormServerSample.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

function TdormServerSample.SearchByName(const AName: String): TObjectList<TPerson>;
var
  Criteria: ICriteria;
begin
  Result := TObjectList<TPerson>.Create;
  Criteria := nil;
  if AName <> EmptyStr then
    Criteria := TdormCriteria.
      NewCriteria('FirstName', coEqual, AName)._Or('LastName', coEqual, AName);
  Session.FillList<TPerson>(Result, Criteria);
  Session.Commit(true);
end;

end.
