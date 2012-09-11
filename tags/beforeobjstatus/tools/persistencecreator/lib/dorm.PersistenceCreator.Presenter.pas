unit dorm.PersistenceCreator.Presenter;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Contnrs,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Dialogs,
  dorm,
  dorm.Commons,
  dorm.PersistenceCreator.Intf,
  dorm.PersistenceCreator,
  dorm.PersistenceCreator.PersistenceJsonFile;

type
  TPersistenceCreatorPresenter = class
  strict private
    FPersistenceView: IPersistenceCreatorView;
    FPersistenceViewCommand: IPersistenceCreatorViewCommand;
    FPersistenceCreator: TPersistenceCreator;
    FActiveEnvironment: String;
    function InternalGetEnvironmentIndex(AEnvironmentName: String): Integer;
    procedure InternalSetEnvironmentCreatorView(AIndex: Integer);
  public
    constructor Create(APersistenceView: IPersistenceCreatorView);
    destructor Destroy; override;
    procedure NewPersistenceFileAction;
    procedure OpenPersistenceFileAction;
    procedure SavePersistenceFileAction;
    procedure ChangeEnvironmentAction(ANewEnvironment: String);
    procedure AddPersistentClassAction;
    procedure RemovePersistentClassAction(AClassName: String);
    property Command: IPersistenceCreatorViewCommand read FPersistenceViewCommand write FPersistenceViewCommand;
  end;

  TPersistenceViewCommand = class(TInterfacedObject, IPersistenceCreatorViewCommand)
  strict private
    FReceiver: TPersistenceCreatorPresenter;
  public
    constructor Create(AReceiver: TPersistenceCreatorPresenter);
    procedure NewPersistenceFileExecute;
    procedure OpenPersistenceFileExecute;
    procedure SavePersistenceFileExecute;
    procedure ChangeEnvironmentExecute(ANewEnvironment: String);
    procedure AddPersistentClassExecute;
    procedure RemovePersistentClassExecute(AClassName: String);
  end;

const FORM_TITLE = 'dorm (the Delphi ORM), Persistence file creator Current File: %s';
      STATUS_TEXT = 'Active Environment: [%s]';
      CONF_FILTER = 'PersistenceJsonFile (*.conf)|*.conf|All Files (*.*)|*.*';

implementation

{ TPersistenceViewCommand }

procedure TPersistenceViewCommand.AddPersistentClassExecute;
begin
  FReceiver.AddPersistentClassAction;
end;

procedure TPersistenceViewCommand.ChangeEnvironmentExecute(
  ANewEnvironment: string);
begin
  FReceiver.ChangeEnvironmentAction(ANewEnvironment);
end;

constructor TPersistenceViewCommand.Create(
  AReceiver: TPersistenceCreatorPresenter);
begin
  FReceiver := AReceiver;
end;

procedure TPersistenceViewCommand.NewPersistenceFileExecute;
begin
  FReceiver.NewPersistenceFileAction;
end;

procedure TPersistenceViewCommand.OpenPersistenceFileExecute;
begin
  FReceiver.OpenPersistenceFileAction;
end;

procedure TPersistenceViewCommand.RemovePersistentClassExecute(AClassName: String);
begin
  FReceiver.RemovePersistentClassAction(AClassName);
end;

procedure TPersistenceViewCommand.SavePersistenceFileExecute;
begin
  FReceiver.SavePersistenceFileAction;
end;

{ TPersistenceCreatorPresenter }

procedure TPersistenceCreatorPresenter.ChangeEnvironmentAction(
  ANewEnvironment: string);
var
  idx: Integer;
begin
  //*-----------------------*
  //* Save values in memory *
  //*-----------------------*
  Idx := InternalGetEnvironmentIndex(FActiveEnvironment);

  //FPersistenceCreatorGetEnvironments[Idx].EnvironmentName := FActiveEnviroment;
  FPersistenceCreator.GetEnvironments[Idx].DatabaseAdapter := FPersistenceView.GetDatabaseAdapter;
  FPersistenceCreator.GetEnvironments[Idx].DatabaseConnectionString := FPersistenceView.GetDatabaseConnectionString;
  FPersistenceCreator.GetEnvironments[Idx].KeysGenerator := FPersistenceView.GetKeysGenerator;
  FPersistenceCreator.GetEnvironments[Idx].KeyType := FPersistenceView.GetKeyType;
  FPersistenceCreator.GetEnvironments[Idx].NullKeyValue := FPersistenceView.GetNullKeyValue;
  FPersistenceCreator.GetEnvironments[Idx].CustomAdapterParameter.Assign(FPersistenceView.GetCustomAdapterConfig);
  //*------------------------*
  //* Set environment values *
  //*------------------------*
  Idx := InternalGetEnvironmentIndex(ANewEnvironment);
  if Assigned(FPersistenceCreator.GetEnvironments[Idx]) then
  begin
    //*------------------------*
    //* Set active environment *
    //*------------------------*
    with FPersistenceView do
    begin
      InternalSetEnvironmentCreatorView(Idx);
    end;
  end;
end;

constructor TPersistenceCreatorPresenter.Create(APersistenceView: IPersistenceCreatorView);
begin
  FPersistenceView := APersistenceView;
  FPersistenceViewCommand := TPersistenceViewCommand.Create(Self);
  FPersistenceCreator := TPersistenceJsonFileCreator.Create;
end;

destructor TPersistenceCreatorPresenter.Destroy;
begin
  FPersistenceCreator.Free;
  inherited;
end;

function TPersistenceCreatorPresenter.InternalGetEnvironmentIndex(
  AEnvironmentName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to FPersistenceCreator.GetEnvironments.Count-1 do
  begin
    if FPersistenceCreator.GetEnvironments.Items[i].EnvironmentName = AEnvironmentName then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TPersistenceCreatorPresenter.InternalSetEnvironmentCreatorView(AIndex: Integer);
begin
  with FPersistenceView do
  begin
    SetStatusBarSimpleText(Format(STATUS_TEXT, [FPersistenceCreator.GetEnvironments[AIndex].EnvironmentName]));
    //*------------------------*
    //* Set active environment *
    //*------------------------*
    FActiveEnvironment := FPersistenceCreator.GetEnvironments[AIndex].EnvironmentName;
    SetDatabaseConnectionString(FPersistenceCreator.GetEnvironments[AIndex].DatabaseConnectionString);
    SetDatabaseAdapter(FPersistenceCreator.GetEnvironments[AIndex].DatabaseAdapter);
    SetKeysGenerator(FPersistenceCreator.GetEnvironments[AIndex].KeysGenerator);
    SetKeyType(FPersistenceCreator.GetEnvironments[AIndex].KeyType);
    SetNullKeyValue(FPersistenceCreator.GetEnvironments[AIndex].NullKeyValue);
    SetCustomAdapterConfig(FPersistenceCreator.GetEnvironments[AIndex].CustomAdapterParameter);
  end;
end;

procedure TPersistenceCreatorPresenter.NewPersistenceFileAction;
begin
  FPersistenceCreator.Initialize;
  with FPersistenceView do
  begin
    //*-----------------*
    //* Set view values *
    //*-----------------*
    SetTitleCaption(Format(FORM_TITLE,['Creating New File']));
    SetEnvironments(FPersistenceCreator.GetEnvironmentsList);
    //*--------------------------------------*
    //* Set first default environment values *
    //*--------------------------------------*
    if Assigned(FPersistenceCreator.GetEnvironments[0]) then
    begin
      InternalSetEnvironmentCreatorView(0);
    end;
    SetPersistentClass(FPersistenceCreator.GetPersistenceClassesList);
    SetLoggerClassName(FPersistenceCreator.GetConfig.LoggerClassName);
  end;
end;

procedure TPersistenceCreatorPresenter.OpenPersistenceFileAction;
var
  FileName: TFileName;
  Odl: TOpenDialog;
begin
  Odl := TOpenDialog.Create(nil);
  try
    Odl.Filter := CONF_FILTER;
    if Odl.Execute then
    begin
      FileName := Odl.FileName;
      FPersistenceCreator.Load(FileName);
      with FPersistenceView do
      begin
        //*-----------------*
        //* Set view values *
        //*-----------------*
        SetTitleCaption(Format(FORM_TITLE,[FileName]));
        SetEnvironments(FPersistenceCreator.GetEnvironmentsList);
        //*------------------------------*
        //* Set first environment values *
        //*------------------------------*
        if Assigned(FPersistenceCreator.GetEnvironments[0]) then
        begin
          InternalSetEnvironmentCreatorView(0);
        end;

        SetPersistentClass(FPersistenceCreator.GetPersistenceClassesList);
        SetLoggerClassName(FPersistenceCreator.GetConfig.LoggerClassName);
      end;
    end;
  finally
    Odl.Free;
  end;
end;

procedure TPersistenceCreatorPresenter.AddPersistentClassAction;
var
  PersClass: string;
  i: Integer;
begin
  if FActiveEnvironment<>'' then
  begin
    PersClass := InputBox('Persistent Classes','Insert New Persistent Class','');

    if PersClass<>'' then
    begin
      for i := 0 to FPersistenceCreator.GetPersistenceClasses.Count-1 do
      begin
        if FPersistenceCreator.GetPersistenceClasses.Items[i].PersistenceClassName = PersClass then
        begin
          raise Exception.Create('Persistent Class already insert');
        end;
      end;
      FPersistenceCreator.GetPersistenceClasses.Add(TPersistenceClass.Create(PersClass));
      FPersistenceView.SetPersistentClass(FPersistenceCreator.GetPersistenceClassesList);
    end;
  end;
end;

procedure TPersistenceCreatorPresenter.RemovePersistentClassAction(AClassName: string);
var
  i: Integer;
begin
  for i := 0 to FPersistenceCreator.GetPersistenceClasses.Count-1 do
  begin
    if FPersistenceCreator.GetPersistenceClasses.Items[i].PersistenceClassName = AClassName then
    begin
      FPersistenceCreator.GetPersistenceClasses.Delete(i);
      Break;
    end;
  end;
  FPersistenceView.SetPersistentClass(FPersistenceCreator.GetPersistenceClassesList);
end;

procedure TPersistenceCreatorPresenter.SavePersistenceFileAction;
var
  FileName: TFileName;
  Odl: TSaveDialog;
begin
  if FActiveEnvironment='' then raise Exception.Create('No file was loaded');

  ChangeEnvironmentAction(FActiveEnvironment);

  Odl := TSaveDialog.Create(nil);
  Odl.Filter := CONF_FILTER;
  try
    if Odl.Execute then
    begin
      if Assigned(FPersistenceCreator) and (FPersistenceCreator.GetEnvironments.Count>0) then
      begin
        FileName := Odl.FileName;

        if FileExists(FileName) then
        begin
         if MessageDlg('File already exists. Do you want to overwrite?',mtConfirmation,[mbYes, mbNo],0) = mrYes then
         begin
           FPersistenceCreator.Save(FileName);
         end;
        end
        else FPersistenceCreator.Save(FileName);
      end;
    end;
  finally
    Odl.Free;
  end;
end;

end.
