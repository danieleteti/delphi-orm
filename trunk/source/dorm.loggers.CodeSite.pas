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
  var
    Destination: TCodeSiteDestination;


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
    {$IFNDEF DEBUG}
    if CodeSite.Enabled then
    begin
      Destination := TCodeSiteDestination.Create(nil);
      Destination.LogFile.Active := True;
      Destination.LogFile.FileName :=
        ChangeFileExt(ExtractFileName(paramstr(0)), '.csl');
      Destination.LogFile.FilePath := ExtractFilepath(paramstr(0));
      CodeSite.Destination := Destination;
      CodeSite.Clear
    end;
   {$ENDIF}
TdormFileLog.register;

end.
