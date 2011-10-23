unit MainForm;

interface

uses
  dorm, dorm.Commons,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TButton = class(Vcl.StdCtrls.TButton)
  public
    constructor Create;
  end;

  TEdit = class(Vcl.StdCtrls.TEdit)
  public
    constructor Create;
  end;

  TForm11 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    ButtonID, EditID: Integer;
    Session: TSession;
  public
    { Public declarations }
  end;

var
  Form11: TForm11;

implementation

{$R *.dfm}

procedure TForm11.Button1Click(Sender: TObject);
begin
  Session.Persist(Button1);
  ButtonID := Button1.Tag;
  RemoveControl(Button1);
  Button1.Free;
end;

procedure TForm11.Button2Click(Sender: TObject);
begin
  Button1 := Session.Load<TButton>(ButtonID);
  Button1.OnClick := Button1Click;
  InsertControl(Button1);
end;

procedure TForm11.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Session.Free;
end;

procedure TForm11.FormCreate(Sender: TObject);
begin
  Session := TSession.CreateConfigured(TStreamReader.Create('dorm.conf'),
    deDevelopment);
end;

{ TButtonWrapper }

constructor TButton.Create;
begin
  inherited Create(Form11);
end;

{ TEdit }

constructor TEdit.Create;
begin
  inherited Create(Form11)
end;

end.
