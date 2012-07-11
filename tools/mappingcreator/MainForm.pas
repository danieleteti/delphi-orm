unit MainForm;

interface

uses
  Windows,
  Messages,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls,
  ExtCtrls,
  Grids,
  ValEdit,
  dorm.MappingCreator,
  dorm.MappingCreator.Strategy.Firebird,
  dorm.MappingCreator.Strategy,
  Generics.Collections,
  ActnList,
  ExtDlgs,
  Menus,
  superobject,
  ImgList,
  Buttons;

const
  CAPTION_FORMAT = 'dorm Mapping Creator [%s]';

type
  TfrmMain = class(TForm)
    ActionList1: TActionList;
    acGenerateMapping: TAction;
    SaveTextFileDialog1: TSaveTextFileDialog;
    lbStrategies: TListBox;
    Label1: TLabel;
    vlUserProperties: TValueListEditor;
    Label2: TLabel;
    Label3: TLabel;
    edtFileName: TEdit;
    Button1: TButton;
    acSelectFileName: TAction;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    Close1: TMenuItem;
    Mapping1: TMenuItem;
    Generate1: TMenuItem;
    acOpenProject: TAction;
    acSaveProject: TAction;
    asSaveProjectAs: TAction;
    acClose: TAction;
    Close2: TMenuItem;
    FileSaveDialog1: TFileSaveDialog;
    FileOpenDialog1: TFileOpenDialog;
    memLogs: TMemo;
    Label4: TLabel;
    Panel1: TPanel;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    chkClasses: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure acGenerateMappingExecute(Sender: TObject);
    procedure lbStrategiesClick(Sender: TObject);
    procedure acGenerateMappingUpdate(Sender: TObject);
    procedure acSelectFileNameExecute(Sender: TObject);
    procedure acSaveProjectExecute(Sender: TObject);
    procedure acOpenProjectExecute(Sender: TObject);
  private
    FCurrentStrategy: TMappingCreatorStrategy;
    FProjectFileName: String;
    procedure LoadLog(AStrategy: TMappingCreatorStrategy);
    procedure SetCurrentStrategy(const Value: TMappingCreatorStrategy);
    procedure SetProjectFileName(const Value: String);
    function CreateJSONFromGUI: ISuperObject;
    procedure FillGUIFromJSON(Project: ISuperObject);
    procedure UpdateUserProperties;
  public
    property CurrentStrategy: TMappingCreatorStrategy read FCurrentStrategy
      write SetCurrentStrategy;
    property ProjectFileName: String read FProjectFileName
      write SetProjectFileName;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  SysUtils,
  System.ioutils;

{$R *.dfm}


procedure TfrmMain.acGenerateMappingExecute(Sender: TObject);
var
  OutputFile: TStreamWriter;
  Names: TArray<string>;
  PropName: string;
begin
  Names := CurrentStrategy.GetUserPropertiesNames;
  for PropName in Names do
  begin
    CurrentStrategy.SetUserProperty(PropName,
      vlUserProperties.Values[PropName]);
  end;

  if FileExists(edtFileName.Text) then
    DeleteFile(edtFileName.Text);
  OutputFile := TStreamWriter.Create(edtFileName.Text);
  try
    if FCurrentStrategy.Execute(OutputFile.BaseStream) then
      ShowMessage(Format('File [%s] created', [edtFileName.Text]))
    else
      ShowMessage('Cannot create file. Check the logs');
    LoadLog(FCurrentStrategy);
  finally
    OutputFile.Free;
  end;
end;

procedure TfrmMain.acGenerateMappingUpdate(Sender: TObject);
begin
  acGenerateMapping.Enabled := trim(edtFileName.Text) <> '';
end;

procedure TfrmMain.acOpenProjectExecute(Sender: TObject);
var
  json: ISuperObject;
  FileName: TFileName;
begin
  if FileOpenDialog1.Execute then
  begin
    FileName := FileOpenDialog1.FileName;
    json := TSuperObject.ParseString(PWideChar(TFile.ReadAllText(FileName)),
      true, false);
    if assigned(json) then
    begin
      FillGUIFromJSON(json);
      ProjectFileName := FileName;
    end
    else
      ShowMessage('Cannot load project. Corrupted structure.');
  end;
end;

procedure TfrmMain.acSaveProjectExecute(Sender: TObject);
var
  proj: ISuperObject;
begin
  if ProjectFileName = EmptyStr then
  begin
    if FileSaveDialog1.Execute then
    begin
      ProjectFileName := FileSaveDialog1.FileName;
      CreateJSONFromGUI.SaveTo(ProjectFileName, true);
    end
  end
  else
    CreateJSONFromGUI.SaveTo(ProjectFileName, true);
end;

procedure TfrmMain.acSelectFileNameExecute(Sender: TObject);
begin
  if SaveTextFileDialog1.Execute then
    edtFileName.Text := SaveTextFileDialog1.FileName;
end;

function TfrmMain.CreateJSONFromGUI: ISuperObject;
var
  UserProperties: ISuperObject;
  Names: TArray<String>;
  PropName: String;
begin
  Result := SO();
  Result.S['version'] := VERSION;
  Result.S['creation_date'] := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
  Result.S['persistence_strategy'] := CurrentStrategy.QualifiedClassName;
  Result.S['persistence_strategy_name'] := lbStrategies.Items
    [lbStrategies.ItemIndex];
  Result.B['generate_delphi_classes'] := chkClasses.Checked;
  UserProperties := SO();
  Names := CurrentStrategy.GetUserPropertiesNames;
  for PropName in Names do
  begin
    UserProperties.S[PropName] := CurrentStrategy.GetUserParameterByName
      (PropName);
  end;
  Result.O['userproperties'] := UserProperties;
  Result.S['outputfilepath'] := edtFileName.Text;
end;

procedure TfrmMain.FillGUIFromJSON(Project: ISuperObject);
var
  UserProperties: ISuperObject;
  Names: TArray<String>;
  PropName: String;
begin
  if lbStrategies.Items.IndexOf(Project.S['persistence_strategy_name']) > -1
  then
    lbStrategies.ItemIndex := lbStrategies.Items.IndexOf
      (Project.S['persistence_strategy_name']);
  lbStrategiesClick(self);
  edtFileName.Text := Project.S['outputfilepath'];
  chkClasses.Checked := Project.B['generate_delphi_classes'];
  UserProperties := Project.O['userproperties'];
  Names := CurrentStrategy.GetUserPropertiesNames;
  for PropName in Names do
    CurrentStrategy.SetUserProperty(PropName, UserProperties.S[PropName]);
  UpdateUserProperties;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  Keys: TArray<string>;
  S: string;
begin
  ProjectFileName := '';
  lbStrategies.Clear;
  Keys := TMappingCreatorStrategyRegister.GetMappingStrategies.Keys.ToArray;
  for S in Keys do
    lbStrategies.Items.Add(S);
end;

procedure TfrmMain.lbStrategiesClick(Sender: TObject);
begin
  CurrentStrategy := TMappingCreatorStrategyRegister.CreateStrategy
    (lbStrategies.Items[lbStrategies.ItemIndex]);
  UpdateUserProperties;
end;

procedure TfrmMain.UpdateUserProperties;
var
  Names: TArray<string>;
  PropName: string;

begin
  while vlUserProperties.RowCount > 2 do
    vlUserProperties.DeleteRow(1);

  Names := CurrentStrategy.GetUserPropertiesNames;
  for PropName in Names do
  begin
    vlUserProperties.Values[PropName] := CurrentStrategy.GetUserParameterByName
      (PropName);
  end;
end;

procedure TfrmMain.LoadLog(AStrategy: TMappingCreatorStrategy);
var
  err, war: string;
begin
  memLogs.Clear;
  for war in AStrategy.Warnings do
  begin
    memLogs.Lines.Add('WARNING: ' + err);
  end;
  for err in AStrategy.Errors do
  begin
    memLogs.Lines.Add('ERROR: ' + err);
  end;
  memLogs.Lines.Add(Format('FINISHED with %d warnings and %d errors', [
    AStrategy.Warnings.Count,
    AStrategy.Errors.Count
    ]));
  memLogs.SelStart := length(memLogs.Text);
end;

procedure TfrmMain.SetCurrentStrategy(const Value: TMappingCreatorStrategy);
begin
  if assigned(FCurrentStrategy) then
    FCurrentStrategy.Free;
  FCurrentStrategy := Value;
end;

procedure TfrmMain.SetProjectFileName(const Value: String);
begin
  FProjectFileName := Value;
  Caption := Format(CAPTION_FORMAT, [FProjectFileName]);
end;

end.
