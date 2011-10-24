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
  dorm.DBCreator, Vcl.ActnList;

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
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    outputfilename: string;
    CurrentEnv: TdormEnvironment;
    function ConfigFileName: String;
    procedure Generate;
  public
    { Public declarations }
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
    outputfilename := '';
    case CurrentEnv of
      deDevelopment:
        pref := 'development';
      deTest:
        pref := 'test';
      deRelease:
        pref := 'release';
    end;
    outputfilename := IncludeTrailingPathDelimiter
      (ExtractFilePath(ConfigFileName)) + pref + '_' +
      ChangeFileExt(ExtractFileName(ConfigFileName), '.sql');

    dbc.GetSQLScript.SaveToFile(outputfilename);
    ShowMessage('Generation completed' + sLineBreak +
      'Output file is located to: ' + outputfilename);
  finally
    dbc.Free;
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
  ShellExecute(0, nil, PWideChar(outputfilename), nil, nil, SW_NORMAL);
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  CurrentEnv := TdormEnvironment.deRelease;
  Generate;
end;

function TfrmMain.ConfigFileName: String;
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
      ini.WriteString('LASTRUN', 'CONFIG_FILE_FULL_PATH', Edit1.Text);
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
    Edit1.Text := ini.ReadString('LASTRUN', 'CONFIG_FILE_FULL_PATH', '');
  finally
    ini.Free;
  end;

  ListBox1.Clear;
  for c in TdormDBCreatorsRegister.GetDBCreators.Keys do
  begin
    ListBox1.Items.Add(c);
  end;
end;

end.
