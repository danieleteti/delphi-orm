unit ServerContainerUnit;

interface

uses
  System.SysUtils,
  System.Classes,
  Datasnap.DSTCPServerTransport,
  Datasnap.DSHTTPCommon,
  Datasnap.DSHTTP,
  Datasnap.DSServer,
  Datasnap.DSCommonServer,
  Datasnap.DSAuth,
  IndyPeerImpl,
  BusinessObjects,
  Generics.Collections,
  dorm,
  dorm.Configuration,
  dorm.Loggers,
  dorm.adapters;

type
  TServerContainer = class(TDataModule)
    DSServer1: TDSServer;
    DSTCPServerTransport1: TDSTCPServerTransport;
    DSHTTPService1: TDSHTTPService;
    DSServerClass1: TDSServerClass;
    procedure DSServerClass1GetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    procedure InitializeData(Session: TSession);
    { Private declarations }
  public
  end;

var
  ServerContainer: TServerContainer;

implementation

uses
  Winapi.Windows,
  ServerMethodsUnit,
  dorm.Commons;

{$R *.dfm}


procedure TServerContainer.DataModuleCreate(Sender: TObject);
var
  Session: TSession;
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
{$REGION 'Insert some data'}
  InitializeData(Session);
{$ENDREGION}
  Session.Free;
end;

procedure TServerContainer.InitializeData(Session: TSession);
var
  p: TPerson;
  People: TObjectList<TPerson>;
begin
  Session.StartTransaction;
  try
    Session.DeleteAll(TPerson);
    People := TObjectList<TPerson>.Create;
    try
      p := TPerson.Create('Daniele', 'Teti', 32);
      p.Laptops.Add(TLaptop.Create('DELL LATITUDE', 2048, 2));
      p.Laptops.Add(TLaptop.Create('COMPAQ PRESARIO', 2048, 4));
      People.Add(p);

      p := TPerson.Create('Scott', 'Summers', 40);
      p.Laptops.Add(TLaptop.Create('DELL A707', 4096, 8));
      People.Add(p);

      p := TPerson.Create('Bruce', 'Banner', 50);
      p.Laptops.Add(TLaptop.Create('DELL A101', 1024, 1));
      People.Add(p);

      People.Add(TPerson.Create('Sue', 'Storm', 35));
      People.Add(TPerson.Create('Peter', 'Parker', 17));
      Session.PersistCollection(People);
    finally
      People.Free;
    end;
    Session.Commit;
  except
    Session.Rollback;
  end;
end;

procedure TServerContainer.DataModuleDestroy(Sender: TObject);
begin
  DSServer1.Stop;
end;

procedure TServerContainer.DSServerClass1GetClass(DSServerClass
  : TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := ServerMethodsUnit.TdormServerSample;
end;

end.
