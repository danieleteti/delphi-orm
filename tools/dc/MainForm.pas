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
  dorm.DBCreator;

type
  TfrmMain = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    function ConfigFileName: String;
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

uses
  dorm.DBCreator.Firebird,
  shellapi,
  inifiles,
  dorm.Commons;

{$R *.dfm}

procedure Generate(const FileName: string; Env: TdormEnvironment);
var
  dbc: TdormDBCreator;
begin
  dbc := TdormFirebirdDBCreator.Create
    (TSession.CreateConfigured(TStreamReader.Create(FileName), Env));
  try
    dbc.Execute;
    dbc.GetSQLScript.SaveToFile(ChangeFileExt(Application.ExeName, '.sql'));
  finally
    dbc.Free;
  end;
end;

procedure TfrmMain.Button1Click(Sender: TObject);
begin
  Generate(ConfigFileName, deDevelopment);
  ShowMessage('Finished');
end;

procedure TfrmMain.Button2Click(Sender: TObject);
begin
  Generate(ConfigFileName, deTest);
  ShowMessage('Finished');
end;

procedure TfrmMain.Button3Click(Sender: TObject);
begin
  ShellExecute(0, nil, pwidechar(ChangeFileExt(Application.ExeName, '.sql')),
    nil, nil, SW_NORMAL);
end;

procedure TfrmMain.Button4Click(Sender: TObject);
begin
  Generate(ConfigFileName, deRelease);
  ShowMessage('Finished');
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
begin
  ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    Edit1.Text := ini.ReadString('LASTRUN', 'CONFIG_FILE_FULL_PATH', '');
  finally
    ini.Free;
  end;
end;

end.
