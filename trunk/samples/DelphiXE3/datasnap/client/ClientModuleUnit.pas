unit ClientModuleUnit;

interface

uses
  System.SysUtils, System.Classes, ClientClassesUnit, Data.DBXDataSnap,
  IndyPeerImpl, Data.DBXCommon, Data.DB, Data.SqlExpr;

type
  TClientProxy = class(TDataModule)
    SQLConnection1: TSQLConnection;
  private
    FInstanceOwner: Boolean;
    FdormServerSampleClient: TdormServerSampleClient;
    function GetdormServerSampleClient: TdormServerSampleClient;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property InstanceOwner: Boolean read FInstanceOwner write FInstanceOwner;
    property dormServerSampleClient: TdormServerSampleClient read GetdormServerSampleClient write FdormServerSampleClient;

end;

var
  ClientProxy: TClientProxy;

implementation

{$R *.dfm}

{%CLASSGROUP 'System.Classes.TPersistent'}

constructor TClientProxy.Create(AOwner: TComponent);
begin
  inherited;
  FInstanceOwner := True;
end;

destructor TClientProxy.Destroy;
begin
  FdormServerSampleClient.Free;
  inherited;
end;

function TClientProxy.GetdormServerSampleClient: TdormServerSampleClient;
begin
  if FdormServerSampleClient = nil then
  begin
    SQLConnection1.Open;
    FdormServerSampleClient:= TdormServerSampleClient.Create(SQLConnection1.DBXConnection, FInstanceOwner);
  end;
  Result := FdormServerSampleClient;
end;

end.
