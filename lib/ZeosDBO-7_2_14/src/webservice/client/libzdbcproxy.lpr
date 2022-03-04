library libzdbcproxy;

{$mode delphi}{$H+}

uses
  Classes, ZDbcProxyIntf, SysUtils
  { you can add units after this };

var
  LastErrorStr: UnicodeString;

function GetLastErrorStr: WideString; stdcall;
begin
  Result := LastErrorStr;
end;

function GetInterface: IZDbcProxy; stdcall;
begin
  try
    result := TZDbcProxy.Create as IZDbcProxy;
  except
    on E: Exception do begin
      LastErrorStr := E.Message;
      result := nil;
    end;
  end;
end;

exports GetInterface, GetLastErrorStr;

begin
  LastErrorStr := 'No error happened yet!';
end.

