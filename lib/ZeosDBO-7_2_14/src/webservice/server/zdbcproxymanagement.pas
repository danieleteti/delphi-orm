unit ZDbcProxyManagement;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ZDbcIntfs, SyncObjs;

type
  TDbcProxyConnection = class
  protected
    FID: String;
    FZeosConnection: IZConnection;
    FLastAccessTime: TDateTime;
    FCriticalSection: TCriticalSection;
  public
    constructor Create(AConnection: IZConnection); virtual;
    destructor Destroy; override;
    property ZeosConnection: IZConnection read FZeosConnection;
    property ID: String read FID;
    procedure Lock;
    procedure Unlock;
  end;

//var
  //LastConnectionId: Integer;

procedure RaiseNotImplemented(FunctionName: String);

implementation

procedure RaiseNotImplemented(FunctionName: String);
begin
  raise Exception.Create('Function ' + FunctionName + ' is not implemented yet!');
end;

constructor TDbcProxyConnection.Create(AConnection: IZConnection);
var
  UUID: TGuid;
begin
  FCriticalSection := TCriticalSection.Create;
  //Inc(LastConnectionId);
  //FID := IntToStr(LastConnectionId);
  CreateGUID(UUID);
  FID := GUIDToString(UUID);
  FZeosConnection := AConnection;
end;

destructor TDbcProxyConnection.Destroy;
begin
  if Assigned(FCriticalSection) then
    FreeAndNil(FCriticalSection);
  inherited;
end;

procedure TDbcProxyConnection.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TDbcProxyConnection.Unlock;
begin
  FCriticalSection.Leave;
end;

//initialization
//  LastConnectionId := 0;

end.

