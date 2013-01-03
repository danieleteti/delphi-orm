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

unit dorm.loggers.CodeSite;

interface

uses
  dorm.Commons,
  Classes,
  CodeSiteLogging;

type
  TdormCodeSiteBaseLog = class abstract(TdormInterfacedObject, IdormLogger)
  protected
    dest: TCodeSiteDestination;
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

  TCodeSiteFileLog = class(TdormCodeSiteBaseLog)
    procedure AfterConstruction; override;
  end;

  TCodeSiteLiveLog = class(TdormCodeSiteBaseLog)
    procedure AfterConstruction; override;
  end;

implementation

uses
  SysUtils;

{ TdormCodeSiteBaseLog }

procedure TdormCodeSiteBaseLog.AfterConstruction;
begin
  inherited;
  CodeSite.Enabled := CodeSite.Installed;
end;

procedure TdormCodeSiteBaseLog.Debug(const Value: string);
begin
  CodeSite.Send(Value);
end;

destructor TdormCodeSiteBaseLog.Destroy;
begin
  Info(ClassName + ' Logger is shutting down');
  dest.Free;
  inherited;
end;

procedure TdormCodeSiteBaseLog.EnterLevel(const Value: string);
begin
  CodeSite.EnterMethod(Value);
end;

procedure TdormCodeSiteBaseLog.Error(const Value: string);
begin
  CodeSite.SendError(Value);
end;

procedure TdormCodeSiteBaseLog.ExitLevel(const Value: string);
begin
  CodeSite.ExitMethod(Value);
end;

procedure TdormCodeSiteBaseLog.Info(const Value: string);
begin
  CodeSite.Send(Value);
end;

class procedure TdormCodeSiteBaseLog.register;
begin
  //
end;

procedure TdormCodeSiteBaseLog.Warning(const Value: string);
begin
  CodeSite.SendWarning(Value);
end;

{ TCodeSiteLiveLog }

procedure TCodeSiteLiveLog.AfterConstruction;
begin
  inherited;
  dest := TCodeSiteDestination.Create(nil);
  dest.Viewer.Active := true;
  dest.LogFile.Active := false;
  CodeSite.Destination := dest;
  Info(ClassName + ' Logger initialized');
end;

{ TCodeSiteFileLog }

procedure TCodeSiteFileLog.AfterConstruction;
begin
  inherited;
  dest := TCodeSiteDestination.Create(nil);
  dest.Viewer.Active := false;
  dest.LogFile.Active := true;
  dest.LogFile.FilePath :=
    ExtractFilePath(GetModuleName(HInstance));
  dest.LogFile.FileName :=
    ChangeFileExt(ExtractFileName(GetModuleName(HInstance)), '.csl');
  dest.LogFile.MaxSize := 1024 * 5;
  dest.LogFile.MaxParts := 5;
  dest.Viewer.Active := false;
  CodeSite.Destination := dest;
  Info(ClassName + ' Logger initialized');
end;

initialization


TCodeSiteFileLog.register;
TCodeSiteLiveLog.register;

end.
