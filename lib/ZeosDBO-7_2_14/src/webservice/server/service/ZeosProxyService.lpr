Program ZeosProxyService;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, zdbc, ZeosProxyMapperUnit, zeosproxyunit
  { add your units here };

begin
  SetMultiByteConversionCodePage(65001);
  Application.Title:='Zeos Web Service Proxy Server';
  Application.Initialize;
  Application.Run;
end.
