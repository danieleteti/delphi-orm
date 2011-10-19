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
  FFile := TStreamWriter.Create(TFileStream.Create(ChangeFileExt(ParamStr(0), '.txt'),
    fmCreate or fmOpenWrite or fmShareDenyWrite));
  Info(ClassName + ' Logger initialized');
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

procedure TdormFileLog.Log(const Value: string);
begin
  FFile.WriteLine(Value);
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
