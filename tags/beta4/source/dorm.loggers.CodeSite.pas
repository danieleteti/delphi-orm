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

unit dorm.loggers.CodeSite;

interface

uses
  dorm.Commons,
  Classes;

type
  TdormFileLog = class(TdormInterfacedObject, IdormLogger)
  public
    class procedure register;
    destructor Destroy; override;
    procedure EnterLevel(const Value: string);
    procedure ExitLevel(const Value: string);
    procedure Error(const Value: string);
    procedure Warning(const Value: string);
    procedure Info(const Value: string);
    procedure Debug(const Value: string);
    procedure AfterConstruction; override;
  end;

implementation

uses
  SysUtils,
  CodeSiteLogging;

{ TdormFileLog }

procedure TdormFileLog.AfterConstruction;
begin
  inherited;
  Info(ClassName + ' Logger initialized');
end;

procedure TdormFileLog.Debug(const Value: string);
begin
  CodeSite.Send(Value);
end;

destructor TdormFileLog.Destroy;
begin
  Info(ClassName + ' Logger is shutting down');
  inherited;
end;

procedure TdormFileLog.EnterLevel(const Value: string);
begin
  CodeSite.EnterMethod(Value);
end;

procedure TdormFileLog.Error(const Value: string);
begin
  CodeSite.SendError(Value);
end;

procedure TdormFileLog.ExitLevel(const Value: string);
begin
  CodeSite.ExitMethod(Value);
end;

procedure TdormFileLog.Info(const Value: string);
begin
  CodeSite.Send(Value);
end;

class procedure TdormFileLog.register;
begin
  //
end;

procedure TdormFileLog.Warning(const Value: string);
begin
  CodeSite.SendWarning(Value);
end;

initialization

CodeSite.Enabled := CodeSite.Installed;
TdormFileLog.register;

end.
