unit ZeosProxyMapperUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DaemonApp;

type
  TZeosProxyMapper = class(TDaemonMapper)
  private

  public

  end;

var
  ZeosProxyMapper: TZeosProxyMapper;

implementation

procedure RegisterMapper;
begin
  RegisterDaemonMapper(TZeosProxyMapper)
end;

{$R *.lfm}


initialization
  RegisterMapper;
end.

