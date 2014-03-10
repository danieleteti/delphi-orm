unit PersistenceCreatorView;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.Rtti,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ActnList,
  Vcl.ExtCtrls,
  Vcl.Menus,
  Vcl.Grids,
  Vcl.ValEdit,
  Vcl.ComCtrls,
  dorm.Commons,
  dorm,
  dorm.adapters,
  dorm.loggers,
  dorm.PersistenceCreator.Intf;

type
  TViewInternalStatus = (vsNew, vsEdit);

  TfrmPersistenceCreatorView = class(TForm, IPersistenceCreatorView)
    pnlMain: TPanel;
    lbxEnvironments: TListBox;
    lblEnvironments: TLabel;
    cmbDatabaseAdapter: TComboBox;
    edtDatabaseConnectionString: TEdit;
    cmbKeysGenerator: TComboBox;
    lblDatabaseAdapter: TLabel;
    lblDatabaseConnectionString: TLabel;
    btnOpenPersistenceFile: TButton;
    btnSavePersistenceFile: TButton;
    lblKeysGenerator: TLabel;
    lblKeyType: TLabel;
    lblNullKeyValue: TLabel;
    cmbLoggerClassName: TComboBox;
    edtNullKeyValue: TEdit;
    lblLoggerClassName: TLabel;
    cmbKeyType: TComboBox;
    vleCustomAdapterConfig: TValueListEditor;
    lblCustomAdapterConfig: TLabel;
    btnCreateNewMappingFile: TButton;
    btnPickMappingFile: TButton;
    lbxPersistentClasses: TListBox;
    lblPersistentClasses: TLabel;
    stbStatus: TStatusBar;
    mnuPersistentClasses: TPopupMenu;
    mnuAddNewClass: TMenuItem;
    mnuRemoveClass: TMenuItem;
    btnNewPersistentFile: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnOpenPersistenceFileClick(Sender: TObject);
    procedure btnSavePersistenceFileClick(Sender: TObject);
    procedure lbxEnvironmentsClick(Sender: TObject);
    procedure mnuRemoveClassClick(Sender: TObject);
    procedure mnuAddNewClassClick(Sender: TObject);
    procedure btnNewPersistentFileClick(Sender: TObject);
  strict private
    FViewCommand: IPersistenceCreatorViewCommand;
    FViewStatus: TViewInternalStatus;
  private
    procedure LoadComboDatabaseAdapter;
    procedure LoadComboKeysGenerator;
    procedure LoadComboKeyType;
    procedure LoadComboLoggerClassName;
  public
    // View
    procedure SetEnvironments(AList: TStrings);
    procedure SetPersistentClass(AList: TStrings);
    procedure SetDatabaseAdapter(AValue: String);
    procedure SetKeysGenerator(AValue: String);
    procedure SetLoggerClassName(AValue: String);
    procedure SetKeyType(AValue: String);
    procedure SetNullKeyValue(AText: String);
    procedure SetCustomAdapterConfig(AList: TStrings);
    procedure SetDatabaseConnectionString(AText: String);
    procedure SetTitleCaption(AText: String);
    procedure SetStatusBarSimpleText(AText: String);
    function GetEnvironments: TStrings;
    function GetPersistentClass: TStrings;
    function GetDatabaseAdapter: String;
    function GetKeysGenerator: String;
    function GetLoggerClassName: String;
    function GetKeyType: String;
    function GetCustomAdapterConfig: TStrings;
    function GetDatabaseConnectionString: String;
    function GetNullKeyValue: String;
    procedure SetViewCommand(AViewCommand: IPersistenceCreatorViewCommand);
    function StartView: Integer;
  end;

const
  FORM_TITLE =
    'dorm (the Delphi ORM), Persistence file creator Current File: %s';

implementation

uses
  System.ioutils;
{$R *.dfm}

procedure TfrmPersistenceCreatorView.btnNewPersistentFileClick(Sender: TObject);
begin
  FViewStatus := vsNew;
  FViewCommand.NewPersistenceFileExecute;
end;

procedure TfrmPersistenceCreatorView.btnOpenPersistenceFileClick
  (Sender: TObject);
begin
  FViewStatus := vsEdit;
  FViewCommand.OpenPersistenceFileExecute;
end;

procedure TfrmPersistenceCreatorView.btnSavePersistenceFileClick
  (Sender: TObject);
begin
  FViewCommand.SavePersistenceFileExecute;
end;

procedure TfrmPersistenceCreatorView.mnuAddNewClassClick(Sender: TObject);
begin
  FViewCommand.AddPersistentClassExecute;
end;

procedure TfrmPersistenceCreatorView.mnuRemoveClassClick(Sender: TObject);
begin
  if lbxPersistentClasses.ItemIndex <> -1 then
  begin
    FViewCommand.RemovePersistentClassExecute
      (lbxPersistentClasses.Items[lbxPersistentClasses.ItemIndex]);
  end;
end;

procedure TfrmPersistenceCreatorView.FormCreate(Sender: TObject);
begin
  Self.Caption := Format(FORM_TITLE, ['']);
  LoadComboDatabaseAdapter;
  LoadComboKeysGenerator;
  LoadComboKeyType;
  LoadComboLoggerClassName;
end;

function TfrmPersistenceCreatorView.GetCustomAdapterConfig: TStrings;
begin
  Result := vleCustomAdapterConfig.Strings;
end;

function TfrmPersistenceCreatorView.GetDatabaseAdapter: String;
begin
  Result := cmbDatabaseAdapter.Text;
end;

function TfrmPersistenceCreatorView.GetDatabaseConnectionString: String;
begin
  Result := edtDatabaseConnectionString.Text;
end;

function TfrmPersistenceCreatorView.GetEnvironments: TStrings;
begin
  Result := lbxEnvironments.Items;
end;

function TfrmPersistenceCreatorView.GetKeysGenerator: String;
begin
  Result := cmbKeysGenerator.Text;
end;

function TfrmPersistenceCreatorView.GetKeyType: String;
begin
  Result := cmbKeyType.Text;
end;

function TfrmPersistenceCreatorView.GetLoggerClassName: String;
begin
  Result := cmbLoggerClassName.Text;
end;

function TfrmPersistenceCreatorView.GetNullKeyValue: String;
begin
  Result := edtNullKeyValue.Text;
end;

function TfrmPersistenceCreatorView.GetPersistentClass: TStrings;
begin
  Result := lbxPersistentClasses.Items;
end;

procedure TfrmPersistenceCreatorView.lbxEnvironmentsClick(Sender: TObject);
begin
  FViewCommand.ChangeEnvironmentExecute
    (lbxEnvironments.Items[lbxEnvironments.ItemIndex]);
end;

procedure TfrmPersistenceCreatorView.SetNullKeyValue(AText: String);
begin
  edtNullKeyValue.Text := AText;
end;

procedure TfrmPersistenceCreatorView.SetCustomAdapterConfig(AList: TStrings);
begin
  vleCustomAdapterConfig.Strings.Clear;
  vleCustomAdapterConfig.Strings := AList;
end;

procedure TfrmPersistenceCreatorView.SetDatabaseConnectionString(AText: String);
begin
  edtDatabaseConnectionString.Text := AText;
end;

procedure TfrmPersistenceCreatorView.SetEnvironments(AList: TStrings);
begin
  lbxEnvironments.Clear;
  lbxEnvironments.Items := AList;
  if lbxEnvironments.Items.Count > 0 then
    lbxEnvironments.ItemIndex := 0;
end;

procedure TfrmPersistenceCreatorView.SetDatabaseAdapter(AValue: String);
begin
  cmbDatabaseAdapter.ItemIndex := cmbDatabaseAdapter.Items.IndexOf(AValue);
end;

procedure TfrmPersistenceCreatorView.SetKeysGenerator(AValue: String);
begin
  cmbKeysGenerator.ItemIndex := cmbKeysGenerator.Items.IndexOf(AValue);
end;

procedure TfrmPersistenceCreatorView.SetKeyType(AValue: String);
begin
  cmbKeyType.ItemIndex := cmbKeyType.Items.IndexOf(AValue);
end;

procedure TfrmPersistenceCreatorView.SetLoggerClassName(AValue: String);
begin
  cmbLoggerClassName.ItemIndex := cmbLoggerClassName.Items.IndexOf(AValue);
end;

procedure TfrmPersistenceCreatorView.SetPersistentClass(AList: TStrings);
begin
  lbxPersistentClasses.Clear;
  lbxPersistentClasses.Items := AList;
end;

procedure TfrmPersistenceCreatorView.SetStatusBarSimpleText(AText: String);
begin
  stbStatus.Panels[0].Text := AText;
  if FViewStatus = vsNew then
    stbStatus.Panels[1].Text := Format('View status: %s', ['Add new file'])
  else
    stbStatus.Panels[1].Text := Format('View status: %s', ['Edit file']);
end;

procedure TfrmPersistenceCreatorView.SetTitleCaption(AText: String);
begin
  Self.Caption := AText;
end;

procedure TfrmPersistenceCreatorView.SetViewCommand(AViewCommand
  : IPersistenceCreatorViewCommand);
begin
  FViewCommand := AViewCommand;
end;

function TfrmPersistenceCreatorView.StartView: Integer;
begin
  Result := ShowModal;
end;

procedure TfrmPersistenceCreatorView.LoadComboDatabaseAdapter;
var
  LContext: TRttiContext;
  LType: TRttiType;
  Metaclass: TClass;
begin
  cmbDatabaseAdapter.Clear;
  { Obtain the RTTI context }
  LContext := TRttiContext.Create;
  { Enumerate all types declared in the application }
  for LType in LContext.GetTypes() do
  begin
    if LType.IsInstance then
    begin
      Metaclass := LType.AsInstance.MetaclassType;
      if Assigned(Metaclass) then
      begin
        if Supports(LType.AsInstance.MetaclassType, IdormPersistStrategy) then
        begin
          cmbDatabaseAdapter.Items.Add(LType.QualifiedName);
        end;
      end;
    end;
  end;
  LContext.Free;
end;

procedure TfrmPersistenceCreatorView.LoadComboKeysGenerator;
var
  LContext: TRttiContext;
  LType: TRttiType;
  Metaclass: TClass;
begin
  cmbKeysGenerator.Clear;
  { Obtain the RTTI context }
  LContext := TRttiContext.Create;
  { Enumerate all types declared in the application }
  for LType in LContext.GetTypes() do
  begin
    if LType.IsInstance then
    begin
      Metaclass := LType.AsInstance.MetaclassType;
      if Assigned(Metaclass) then
      begin
        if Supports(LType.AsInstance.MetaclassType, IdormKeysGenerator) then
        begin
          cmbKeysGenerator.Items.Add(LType.QualifiedName);
        end;
      end;
    end;
  end;
  LContext.Free;
end;

procedure TfrmPersistenceCreatorView.LoadComboLoggerClassName;
var
  LContext: TRttiContext;
  LType: TRttiType;
  Metaclass: TClass;
begin
  cmbLoggerClassName.Clear;
  { Obtain the RTTI context }
  LContext := TRttiContext.Create;
  { Enumerate all types declared in the application }
  for LType in LContext.GetTypes() do
  begin
    if LType.IsInstance then
    begin
      Metaclass := LType.AsInstance.MetaclassType;
      if Assigned(Metaclass) then
      begin
        if Supports(LType.AsInstance.MetaclassType, IdormLogger) then
        begin
          cmbLoggerClassName.Items.Add(LType.QualifiedName);
        end;
      end;
    end;
  end;
  LContext.Free;
end;

procedure TfrmPersistenceCreatorView.LoadComboKeyType;
begin
  cmbKeyType.Clear;
  cmbKeyType.Items.Add('integer');
  cmbKeyType.Items.Add('string');
end;

end.
