unit zeosproxyunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp, server_listener;

type

  { TZeosProxyDaemon }

  TZeosProxyDaemon = class(TDaemon)
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    AppObject: TwstListener;
  public

  end;

var
  ZeosProxyDaemon: TZeosProxyDaemon;

implementation

uses
  {wst}fpc_http_server, server_service_soap,
  {synapse}
  {local}zeosproxy, zeosproxy_binder, zeosproxy_imp, DbcProxyUtils,
  DbcProxyConnectionManager, DbcProxyConfigManager, ZDbcProxyManagement;

procedure RegisterDaemon;
begin
  RegisterDaemonClass(TZeosProxyDaemon)
end;

{$R *.lfm}

{ TZeosProxyDaemon }

procedure TZeosProxyDaemon.DataModuleStart(Sender: TCustomDaemon;
  var OK: Boolean);
begin
  ConnectionManager := TDbcProxyConnectionManager.Create;
  ConfigManager := TDbcProxyConfigManager.Create;
  ConfigManager.LoadConfigInfo(ExtractFilePath(ParamStr(0)) + 'ZDbcProxy.ini');

  //Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  //Server_service_RegisterXmlRpcFormat();

  RegisterZeosProxyImplementationFactory();
  Server_service_RegisterZeosProxyService();
  AppObject := TwstFPHttpListener.Create('0.0.0.0');
  (AppObject as  TwstFPHttpListener).Options := [loExecuteInThread];
  AppObject.Start();
end;

procedure TZeosProxyDaemon.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean
  );
begin
  AppObject.Stop();
  FreeAndNil(AppObject);
end;


initialization
  RegisterDaemon;
end.

