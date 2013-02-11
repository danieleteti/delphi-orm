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

unit dorm.loggers.FileLog;

interface

uses
  dorm.Commons,
  Classes,
  Generics.Collections;

type
  TdormFileLog = class(TdormInterfacedObject, IdormLogger)
  private type
    TdormLoggerThread = class(TThread)
    private
      FStreamWriter: TStreamWriter;
      FQueue: TThreadedQueue<string>;
      FFileStream: TFileStream;
      procedure InitLogFile;
      procedure ShiftFileNames;
    protected
      function GetLogFileName(FileIndex: Integer = 0): String;
      procedure Rotate;
      procedure Release;
      procedure Execute; override;
    public const
      MAX_FILE_SIZE = 1024 * 1024 * 5; { todo: configuration? }
      MAX_FILES_NUMBER = 5; { todo: configuration? }
      constructor Create;
      procedure Append(const Value: String);
      destructor Destroy; override;
    end;

  protected
    FIndent: Integer;
    FFile: TStreamWriter;

  const
    INDENT_WIDTH = 2;
    function IndentToStr: String;
    function GetTimeStamp: string;
    procedure Log(const Value: string);
    procedure Init;
    procedure IncIndent;
    procedure DecIndent;
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
  SysUtils,
  Math,
  ioutils,
  strutils,
  TimeSpan,
  {$IF CompilerVersion > 22}
  Winapi.Windows,
  System.SyncObjs;
  {$ELSE}
  Windows,
  SyncObjs;
  {$IFEND}

var
  FLoggerThread: TdormFileLog.TdormLoggerThread;
  FEvent: TEvent;
  cs: TCriticalSection;

  { TdormFileLog }

procedure TdormFileLog.AfterConstruction;
begin
  inherited;
  Init;
end;

procedure TdormFileLog.Debug(const Value: string);
begin
  Log('[DEBUG] ' + Value);
end;

procedure TdormFileLog.DecIndent;
begin
  FIndent := Max(0, FIndent - 1);
end;

destructor TdormFileLog.Destroy;
begin
  Info('Shutting down TdormFileLog...');
  inherited;
end;

procedure TdormFileLog.EnterLevel(const Value: string);
begin
  Debug('[ENTER: ' + Value + ']');
  IncIndent;
end;

procedure TdormFileLog.Error(const Value: string);
begin
  Log('[ERROR] ' + Value);
end;

procedure TdormFileLog.ExitLevel(const Value: string);
begin
  DecIndent;
  Debug('[EXIT: ' + Value + ']');
end;

function TdormFileLog.GetTimeStamp: string;
begin
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz ', now);
end;

procedure TdormFileLog.IncIndent;
begin
  inc(FIndent);
end;

function TdormFileLog.IndentToStr: String;
begin
  Result := StringOfChar(' ', FIndent * INDENT_WIDTH);
end;

procedure TdormFileLog.Info(const Value: string);
begin
  Log('[INFO] ' + Value);
end;

procedure TdormFileLog.Init;
begin
  FIndent := 0;
  if not Assigned(FLoggerThread) then
  begin
    cs.Enter;
    try
      if not Assigned(FLoggerThread) then
      begin
        FLoggerThread := TdormLoggerThread.Create;
        FEvent.WaitFor(TTimeSpan.FromSeconds(120));
      end;
    finally
      cs.Leave;
    end;
  end;
  Info(ClassName + ' Logger initialized');
end;

procedure TdormFileLog.Log(const Value: string);
begin
  FLoggerThread.Append(GetTimeStamp + '[' +
    inttostr(TThread.CurrentThread.ThreadID) + ']' + IndentToStr + Value);
end;

class procedure TdormFileLog.register;
begin
  //
end;

procedure TdormFileLog.Warning(const Value: string);
begin
  Log('[WARNING] ' + Value);
end;

{ TdormFileLog.TdormLoggerThread }

procedure TdormFileLog.TdormLoggerThread.Append(const Value: String);
begin
  FQueue.PushItem(Value);
end;

constructor TdormFileLog.TdormLoggerThread.Create;
begin
  FQueue := TThreadedQueue<string>.Create(1000, MaxLongint, 100);
  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TdormFileLog.TdormLoggerThread.Destroy;
begin
  FQueue.Free;
  inherited;
end;

procedure TdormFileLog.TdormLoggerThread.Execute;
var
  s: String;
begin
  NameThreadForDebugging('TdormFileLog.TdormLoggerThread');
  FEvent.SetEvent;
  Rotate;
  try
    try
      repeat
        while FQueue.PopItem(s) <> wrTimeout do
        begin
          Rotate;
          FStreamWriter.WriteLine(s);
        end;
      until Terminated;
    finally
      Release;
    end;
  except
  end;
  FEvent.SetEvent;
end;

function TdormFileLog.TdormLoggerThread.GetLogFileName
  (FileIndex: Integer): String;
var
  mname: string;
  ext: string;
begin
  mname := GetModuleName(HInstance);
  ext := ExtractFileExt(mname);
  Delete(mname, Length(mname) - Length(ext) + 1, Length(mname));
  if FileIndex = 0 then
  begin
    Result := mname + '_dormFileLog.log';
  end
  else
    Result := Format(mname + '_dormFileLog_%3.3d.log', [FileIndex]);
end;

procedure TdormFileLog.TdormLoggerThread.Release;
begin
  if Assigned(FStreamWriter) then
  begin
    FStreamWriter.Close;
    FStreamWriter.Free;
    FStreamWriter := nil;
  end;
end;

procedure TdormFileLog.TdormLoggerThread.InitLogFile;
begin
  if TFile.Exists(GetLogFileName) then
    FFileStream := TFileStream.Create(GetLogFileName, fmOpenReadWrite or
      fmShareDenyWrite)
  else
    FFileStream := TFileStream.Create(GetLogFileName, fmCreate or fmOpenWrite or
      fmShareDenyWrite);
  FFileStream.Seek(0, soEnd);
  FStreamWriter := TStreamWriter.Create(FFileStream);
  FStreamWriter.OwnStream;
end;

procedure TdormFileLog.TdormLoggerThread.ShiftFileNames;
var
  i: Integer;
begin
  DeleteFile(PChar(GetLogFileName(MAX_FILES_NUMBER)));
  for i := MAX_FILES_NUMBER - 1 downto 0 do
  begin
    RenameFile(GetLogFileName(i), GetLogFileName(i + 1))
  end;
end;

procedure TdormFileLog.TdormLoggerThread.Rotate;
begin
  if Assigned(FStreamWriter) then
  begin
    if FStreamWriter.BaseStream.Position >= MAX_FILE_SIZE then
    begin
      FreeAndNil(FStreamWriter);
      ShiftFileNames;
      InitLogFile;
    end;
  end
  else
  begin
    InitLogFile;
  end;
  Assert(Assigned(FStreamWriter));
end;

initialization

TdormFileLog.register;
cs := TCriticalSection.Create;
FEvent := TEvent.Create;
FEvent.ResetEvent;

finalization

if Assigned(FLoggerThread) then
begin
  FLoggerThread.Terminate;
  FLoggerThread.WaitFor;
  FEvent.WaitFor(TTimeSpan.FromMinutes(1));
  FLoggerThread.Free;
end;
FEvent.Free;
cs.Free;

end.
