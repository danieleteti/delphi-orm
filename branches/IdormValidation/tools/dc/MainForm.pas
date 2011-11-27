unit MainForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  dorm,

  dorm.Commons,
  dorm.DBCreator, Vcl.ActnList, Vcl.Buttons;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Label2: TLabel;
    ListBox1: TListBox;
    SpeedButton1: TSpeedButton;
    FileOpenDialog1: TFileOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
  private
    OutputFilename: string;
    CurrentEnv: TdormEnvironment;
    function GetConfigFileName: String;
    procedure Generate;
    procedure SetConfigFileName(const Value: String);
  public
    property ConfigFileName: String read GetConfigFileName
      write SetConfigFileName;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  dorm.DBCreator.Firebird,
  shellapi,
  inifiles, dorm.DBCreator.Interbase;

{$R *.dfm}


procedure TfrmMain.Generate;
var
  dbc: TdormDBCreator;
  pref: string;
begin
  if ListBox1.ItemIndex = -1 then
  begin
    ShowMessage('Select a database generator from the list!');
    Exit;
  end;

  dbc := TdormDBCreatorsRegister.GetDBCreators
    [ListBox1.Items[ListBox1.ItemIndex]]
    .Create(TSession.CreateConfigured(TStreamReader.Create(ConfigFileName),
    CurrentEnv));
  try
    dbc.Execute;
    OutputFilename := '';
    case CurrentEnv of
      deDevelopment:
        pref := 'development';
      deTest:
        pref := 'test';
      deRelease:
        pref := 'release';
    end;
    OutputFilename := IncludeTrailingPathDelimiter
      (ExtractFilePath(ConfigFileName)) + pref + '_' +
      ChangeFileExt(ExtractFileName(ConfigFileName), '.sql');

    dbc.GetSQLScript.SaveToFile(OutputFilename);
    ShowMessage('Generation completed' + sLineBreak +
      'Output file is located to: ' + OutputFilename);
    dbc.CreateDatabase(CurrentEnv);
  finally
    dbc.Free;
  end;
end;

procedure TfrmMain.SetConfigFileName(const Value: String);
begin
  Edit1.Text := Value;
end;

procedure TfrmMain.SpeedButton1Click(Sender: TObject);
begin
  if FileExists(ConfigFileName) then
    FileOpenDialog1.DefaultFolder := ExtractFilePath(ConfigFileName);
  if FileOpenDialog1.Execute then
  begin
    if FileExists(FileOpenDialog1.FileName) then
      ConfigFileName := FileOpenDialog1.FileName
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  CurrentEnv := TdormEnvironment.deDevelopment;
  Generate;
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  CurrentEnv := TdormEnvironment.deTest;
  Generate;
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  ShellExecute(0, nil, PWideChar(OutputFilename), nil, nil, SW_NORMAL);
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  CurrentEnv := TdormEnvironment.deRelease;
  Generate;
end;

function TfrmMain.GetConfigFileName: String;
begin
  Result := Edit1.Text;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  ini: TIniFile;
begin
  try
    ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
    try
      ini.WriteString('LASTRUN', 'CONFIG_FILE_FULL_PATH', ConfigFileName);
    finally
      ini.Free;
    end;
  except
  end;

end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  c: string;
begin
  ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    ConfigFileName := ini.ReadString('LASTRUN', 'CONFIG_FILE_FULL_PATH', '');
  finally
    ini.Free;
  end;

  ListBox1.Clear;
  for c in TdormDBCreatorsRegister.GetDBCreators.Keys do
    ListBox1.Items.Add(c);
end;

end.
