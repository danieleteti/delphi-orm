unit dorm.Component.SessionComponentEditor;

interface

uses
  DesignEditors;

type
  TdormSessionEditor = class(TComponentEditor)
  protected
    procedure InitConfigFile(const FileName: String);
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

implementation

{ TdormSessionEditor }

uses DORMSessionComponentEditorForm, dorm.Component.Session,
  PersistenceCreatorView,
  dorm.PersistenceCreator.Presenter, dorm.PersistenceCreator.Intf,
  dorm.PersistenceCreator.PersistenceJsonFile, system.ioutils;

procedure TdormSessionEditor.ExecuteVerb(Index: Integer);
var
  PersistenceCreatorView: IPersistenceCreatorView;
  Presenter: TPersistenceCreatorPresenter;
  Session: TdormSession;
begin
  case Index of
    0:
      begin
        Session := GetComponent as TdormSession;
        if Session.ConfigFileName = '' then
        begin
          Session.ConfigFileName := 'dorm.conf';
          InitConfigFile(Session.ConfigFileName);
        end;
        PersistenceCreatorView := TfrmPersistenceCreatorView.Create(nil);
        Presenter := TPersistenceCreatorPresenter.Create
          (PersistenceCreatorView);

        PersistenceCreatorView.SetViewCommand(Presenter.Command);
        Presenter.Start;
      end;
  end;
end;

function TdormSessionEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Edit DORM Configuration File';
  end;
end;

function TdormSessionEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TdormSessionEditor.InitConfigFile(const FileName: String);
var
  s: string;
begin
  s := '{ ' + '"persistence": {' + '  "development": {' +
    '  "database_adapter": "",' + '  "database_connection_string": "",' +
    '  "key_type": "integer", ' + '  "null_key_value": "0", ' +
    '  "keys_generator": "", ' + '  "username": "sysdba", ' +
    '  "password":"masterkey" ' + '  }, ' + '  "release": { ' +
    '   "database_adapter": "Select DatabaseAdapter", ' +
    '   "database_connection_string": "Set DatabaseConnectionString", ' +
    '   "key_type": "Set KeyType", ' +
    '   "null_key_value": "Set NullKeyValue", ' +
    '   "keys_generator": "Select KeysGenerator" ' + '  }, ' + ' "test": { ' +
    '   "database_adapter": "Select DatabaseAdapter", ' +
    '   "database_connection_string": "Set DatabaseConnectionString", ' +
    '   "key_type": "Set KeyType", ' +
    '   "null_key_value": "Set NullKeyValue", ' +
    '   "keys_generator": "Select KeysGenerator" ' + ' }, ' +
    ' "persistent_classes": [] ' + ' }, ' +
    '"config": {"logger_class_name": "dorm.loggers.CodeSite.TCodeSiteLiveLog"}}';
  TFile.WriteAllText(FileName, s);
end;

end.
