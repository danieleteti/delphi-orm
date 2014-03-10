{ *******************************************************************************
  Copyright 2010-2013 Daniele Teti

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

unit dorm.loggers.SmartInspect;

interface

uses
  dorm.Commons,
  Classes,
  SmartInspect,
  SiAuto;

type
  TdormSILog = class abstract(TdormInterfacedObject, IdormLogger)
  protected
    session : TSiSession;
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


resourcestring
   SISessionName = 'dorm';


implementation


{ TdormSILog }

procedure TdormSILog.AfterConstruction;
begin
  inherited;
  Session := Si.AddSession(SISessionName);
  Info(ClassName + ' Logger is starting up');
end;

procedure TdormSILog.Debug(const Value: string);
begin
  Session.LogDebug(Value);
end;

destructor TdormSILog.Destroy;
begin
  Info(ClassName + ' Logger is shutting down');
  Si.DeleteSession(session);
  inherited;
end;

procedure TdormSILog.EnterLevel(const Value: string);
begin
  session.EnterMethod(value);
end;

procedure TdormSILog.Error(const Value: string);
begin
  session.LogError(Value);
end;

procedure TdormSILog.ExitLevel(const Value: string);
begin
  session.LeaveMethod(Value);
end;

procedure TdormSILog.Info(const Value: string);
begin
  session.LogMessage(Value);
end;

class procedure TdormSILog.register;
begin
  //
end;

procedure TdormSILog.Warning(const Value: string);
begin
  session.LogWarning(Value);
end;


initialization

TdormSILog.register;

end.
