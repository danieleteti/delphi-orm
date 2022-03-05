program ZDbcProxyServer;

{$mode delphi}{$H+}

uses
  {$IFDEF UNIX}
  cwstring,
    {$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  //{fpc}lazutils,
  {wst}server_listener, fpc_http_server, server_service_soap, zdbc,
  {synapse}
  {local}zeosproxy, zeosproxy_binder, zeosproxy_imp, DbcProxyUtils,
  DbcProxyConnectionManager, DbcProxyConfigManager, ZDbcProxyManagement,
  ZDbcInterbase6, ZDbcPostgreSql;

type

  { TZDbcProxyServer }

  TZDbcProxyServer = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure WriteProtocols;
  end;

{ TZDbcProxyServer }

procedure TZDbcProxyServer.WriteProtocols;
begin

end;

procedure TZDbcProxyServer.DoRun;
var
  ErrorMsg: String;
  AppObject : TwstListener;
  configFile: String;
begin
  {$IFDEF LINUX}
  configFile := '/etc/zeosproxy.ini';
  {$ELSE}
  configFile := ExtractFilePath(ParamStr(0)) + ZDbcProxy.ini;
  {$ENDIF}

  // quick check parameters
  ErrorMsg:=String(CheckOptions('h', 'help'));
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(UTF8Encode(ErrorMsg)));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  ConnectionManager := TDbcProxyConnectionManager.Create;
  ConfigManager := TDbcProxyConfigManager.Create;
  ConfigManager.LoadConfigInfo(configFile);

  //Server_service_RegisterBinaryFormat();
  Server_service_RegisterSoapFormat();
  //Server_service_RegisterXmlRpcFormat();

  RegisterZeosProxyImplementationFactory();
  Server_service_RegisterZeosProxyService();
  AppObject := TwstFPHttpListener.Create('0.0.0.0');
  try
    WriteLn('"Web Service Toolkit" HTTP Server sample listening at:');
    WriteLn('');
    WriteLn('http://0.0.0.0:8000/');
    WriteLn('');
    WriteLn('Press enter to quit.');
    (AppObject as  TwstFPHttpListener).Options := [loExecuteInThread];
    AppObject.Start();
    ReadLn();
    WriteLn('Stopping the Server...');
    AppObject.Stop()
  finally
    FreeAndNil(AppObject);
  end;

  if Assigned(ConnectionManager) then
    FreeAndNil(ConnectionManager);

  // stop program loop
  Terminate;
end;

constructor TZDbcProxyServer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TZDbcProxyServer.Destroy;
begin
  inherited Destroy;
end;

procedure TZDbcProxyServer.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TZDbcProxyServer;
begin
  {$IFDEF WINDOWS}
  SetMultiByteConversionCodePage(CP_UTF8);
  {$IFEND}
  Application:=TZDbcProxyServer.Create(nil);
  Application.Title:='Zeos DBC Proxy Server';
  Application.Run;
  Application.Free;
end.

