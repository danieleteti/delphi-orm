unit MainForm;

interface

uses
  dorm,
  dorm.Commons,
  dorm.adapters,
  dorm.loggers,
  dorm.Configuration,
  dorm.Mappings,
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls;

type

  [NoAutomapping]
  TButton = class(Vcl.StdCtrls.TButton)
  private
    FObjStatus: TdormObjectStatus;
    procedure SetObjStatus(const Value: TdormObjectStatus);
  public
    constructor Create; // A parameterless constructor
    [Transient]
    property ObjStatus: TdormObjectStatus read FObjStatus write SetObjStatus;
  end;

  TForm11 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    ButtonID: Integer;
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
  Button1.ObjStatus := osDirty;
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
    TStreamReader.Create('dorm.mapping'), deDevelopment);
  Session.StartTransaction;
end;

{ TButtonWrapper }

constructor TButton.Create;
begin
  inherited Create(Form11);
end;

procedure TButton.SetObjStatus(const Value: TdormObjectStatus);
begin
  FObjStatus := Value;
end;

end.
