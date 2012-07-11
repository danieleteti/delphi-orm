unit ServerContainerUnit;

interface

uses
  SysUtils,
  Classes,
  DSTCPServerTransport,
  DSHTTPCommon,
  DSHTTP,
  DSServer,
  DSCommonServer,
  DSAuth,
  BusinessObjects,
  dorm.adapter.Firebird,
  Generics.Collections,
  dorm, IndyPeerImpl;

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
  dorm.loggers,
  Windows,
  ServerMethodsUnit,
  dorm.Commons;

{$R *.dfm}


procedure TServerContainer.DataModuleCreate(Sender: TObject);
var
  Session: TSession;
begin
  Session := TSession.CreateConfigured(
    TStreamReader.Create('dorm.conf'),
    TStreamReader.Create('dorm.mapping'),
    deDevelopment);
{$REGION 'Insert some data'}
  InitializeData(Session);
{$ENDREGION}
  Session.Free;
end;

procedure TServerContainer.InitializeData(Session: TSession);
var
  p: TPerson;
  People: TPeople;
begin
  Session.DeleteAll(TPerson);
  People := TPeople.Create;
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
    Session.InsertCollection(People);
  finally
    People.Free;
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
