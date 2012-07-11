program persistencecreator;

uses
  Vcl.Forms,
  PersistenceCreatorView in 'PersistenceCreatorView.pas' {frmPersistenceCreatorView},
  dorm.PersistenceCreator.PersistenceJsonFile in 'lib\dorm.PersistenceCreator.PersistenceJsonFile.pas',
  dorm.PersistenceCreator.Presenter in 'lib\dorm.PersistenceCreator.Presenter.pas',
  dorm.PersistenceCreator in 'lib\dorm.PersistenceCreator.pas',
  dorm.PersistenceCreator.Intf in 'lib\dorm.PersistenceCreator.Intf.pas';

{$R *.res}

var
  View: TfrmPersistenceCreatorView;
  Presenter: TPersistenceCreatorPresenter;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPersistenceCreatorView, View);
  Presenter := TPersistenceCreatorPresenter.Create(View as IPersistenceCreatorView);
  View.SetViewCommand(Presenter.Command);

  Application.Run;

  Presenter.Free;

end.
