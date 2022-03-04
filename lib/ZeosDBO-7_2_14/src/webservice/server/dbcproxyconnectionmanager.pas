unit DbcProxyConnectionManager;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, ZDbcProxyManagement, ZDbcIntfs, generics.collections;

type
  TDbcProxyConnectionList = TList<TDbcProxyConnection>;

  TDbcProxyConnectionManager = class
  protected
    Synchronizer: TMultiReadExclusiveWriteSynchronizer;
    List: TDbcProxyConnectionList;
  public
    function GetConnectionCount: Integer;
    function FindConnection(ID: String): TDbcProxyConnection;
    function AddConnection(Connection: IZConnection): String;
    procedure RemoveConnection(ID: String);
    function LockConnection(ID: String): TDbcProxyConnection;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

constructor TDbcProxyConnectionManager.Create;
begin
  inherited;
  Synchronizer := TMultiReadExclusiveWriteSynchronizer.Create;
  List := TDbcProxyConnectionList.Create;
end;

destructor TDbcProxyConnectionManager.Destroy;
begin
  if Assigned(Synchronizer) then
    FreeAndNil(Synchronizer);
  if Assigned(List) then
    FreeAndNil(List);
  inherited;
end;

function TDbcProxyConnectionManager.FindConnection(ID: String): TDbcProxyConnection;
var
  x: Integer;
begin
  Result := nil;
  Synchronizer.Beginread;
  try
    for x := 0 to List.Count - 1 do begin
      if List.Items[x].ID = ID then begin
        Result := List.Items[x];
        break;
      end;
    end;
  finally
    Synchronizer.Endread;
  end;
end;

function TDbcProxyConnectionManager.LockConnection(ID: String): TDbcProxyConnection;
begin
  Result := FindConnection(ID);
  if Assigned(Result) then Result.Lock else raise Exception.Create('No connection with ID ' + ID + ' was found!');
end;

function TDbcProxyConnectionManager.AddConnection(Connection: IZConnection): String;
var
  ProxyConn: TDbcProxyConnection;
begin
  ProxyConn := TDbcProxyConnection.Create(Connection);
  Result := ProxyConn.ID;
  Synchronizer.Beginwrite;
  try
    List.Add(ProxyConn);
  finally
    Synchronizer.Endwrite;
  end;
end;

procedure TDbcProxyConnectionManager.RemoveConnection(ID: String);
var
  Conn: TDbcProxyConnection;
  x: Integer;
begin
  Synchronizer.Beginwrite;
  try
    for x := 0 to List.Count - 1 do begin
      if List.Items[x].ID = ID then begin
        Conn := List.Items[x];
        List.Delete(x);
        break;
      end;
    end;
  finally
    Synchronizer.Endwrite;
  end;

  if Assigned(Conn) then
    Conn := nil;
end;

function TDbcProxyConnectionManager.GetConnectionCount: Integer;
begin
  Result := List.Count;
end;

end.

