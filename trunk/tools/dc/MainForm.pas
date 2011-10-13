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
  TForm6 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure JvProgressComponent1Close(Sender: TObject);
  private
    function ConfigFileName: String;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses
  dorm.DBCreator.Firebird,
  shellapi,
  dorm.Commons;

{$R *.dfm}


procedure Generate(const FileName: string; Env: TdormEnvironment);
var
  dbc: TdormDBCreator;
begin
  dbc := TdormFirebirdDBCreator.Create(TSession.CreateConfigured(TStreamReader.Create(FileName), Env));
  try
    dbc.Execute;
    dbc.GetSQLScript.SaveToFile(ChangeFileExt(Application.ExeName, '.sql'));
  finally
    dbc.Free;
  end;
end;

procedure TForm6.Button1Click(Sender: TObject);
begin
  Generate(ExtractFilePath(Application.ExeName) + 'preventivi.conf', deDevelopment);
  ShowMessage('Finished');
end;

procedure TForm6.Button2Click(Sender: TObject);
begin
  Generate(ExtractFilePath(Application.ExeName) + 'preventivi.conf', deTest);
  ShowMessage('Finished');
end;

procedure TForm6.Button3Click(Sender: TObject);
begin
  ShellExecute(0, nil, pwidechar(ChangeFileExt(Application.ExeName, '.sql')), nil, nil, SW_NORMAL);
end;

procedure TForm6.Button4Click(Sender: TObject);
begin
  Generate(ExtractFilePath(Application.ExeName) + 'preventivi.conf', deRelease);
  ShowMessage('Finished');
end;

function TForm6.ConfigFileName: String;
begin
  Result := Edit1.Text;
end;

procedure TForm6.JvProgressComponent1Close(Sender: TObject);
begin
  Abort;
end;

end.
