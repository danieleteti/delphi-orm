{ *******************************************************************************
  Copyright 2010-2011 Daniele Teti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ******************************************************************************** }

unit dorm.loggers.FileLog;

interface

uses
  dorm.Commons,
  Classes;

type
  TdormFileLog = class(TdormInterfacedObject, IdormLogger)
  protected
    FFile: TStreamWriter;
    function GetTimeStamp: string;
    procedure Log(const Value: string);
    procedure Init;
  public
    class procedure register;
    destructor Destroy; override;
    procedure Error(const Value: string);
    procedure Warning(const Value: string);
    procedure Info(const Value: string);
    procedure Debug(const Value: string);
    procedure AfterConstruction; override;
    procedure EnterLevel(const Value: string);
    procedure ExitLevel(const Value: string);
  end;

implementation

uses
  SysUtils;

{ TdormFileLog }

procedure TdormFileLog.AfterConstruction;
begin
  inherited;
  Init;
end;

procedure TdormFileLog.Debug(const Value: string);
begin
  Log(GetTimeStamp + ' [DEBUG] ' + Value);
end;

destructor TdormFileLog.Destroy;
begin
  FFile.Close;
  FFile.BaseStream.Free;
  FFile.Free;
  inherited;
end;

procedure TdormFileLog.EnterLevel(const Value: string);
begin
  Debug('ENTER: ' + Value);
end;

procedure TdormFileLog.Error(const Value: string);
begin
  Log(GetTimeStamp + ' [ERROR] ' + Value);
end;

procedure TdormFileLog.ExitLevel(const Value: string);
begin
  Debug('EXIT: ' + Value);
end;

function TdormFileLog.GetTimeStamp: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz ', now);
end;

procedure TdormFileLog.Info(const Value: string);
begin
  Log(GetTimeStamp + ' [INFO] ' + Value);
end;

procedure TdormFileLog.Init;
begin
  FFile := TStreamWriter.Create(TFileStream.Create(ChangeFileExt(ParamStr(0),
    '.txt') + FormatDateTime('yyyymmdd_hhnnss', now), fmCreate or fmOpenWrite or
    fmShareDenyWrite));
  Info(ClassName + ' Logger initialized');
end;

procedure TdormFileLog.Log(const Value: string);
begin
  FFile.WriteLine(Value);
  if FFile.BaseStream.Position > 1024000 then
  begin
    FFile.Flush;
    FFile.Close;
    FFile.Free;
    Init;
  end;
end;

class procedure TdormFileLog.register;
begin
  //
end;

procedure TdormFileLog.Warning(const Value: string);
begin
  Log(GetTimeStamp + ' [WARNING] ' + Value);
end;

initialization

TdormFileLog.register;

end.
