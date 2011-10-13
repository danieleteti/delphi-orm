program dormDCGUI;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form6},
  dorm.DBCreator in '..\lib\dorm.DBCreator.pas',
  dorm.DBCreator.Firebird in '..\lib\dorm.DBCreator.Firebird.pas',
  dorm.utils.Sequences in '..\..\lib\DORM\dorm.utils.Sequences.pas',
  dorm in '..\..\lib\DORM\dorm.pas',
  dorm.loggers.FileLog in '..\..\lib\DORM\dorm.loggers.FileLog.pas',
  dorm.Core.IdentityMap in '..\..\lib\DORM\dorm.Core.IdentityMap.pas',
  dorm.Commons in '..\..\lib\DORM\dorm.Commons.pas',
  dorm.Collections in '..\..\lib\DORM\dorm.Collections.pas',
  dorm.adapter.Firebird in '..\..\lib\DORM\dorm.adapter.Firebird.pas',
  dorm.adapter.Firebird.Factory in '..\..\lib\DORM\dorm.adapter.Firebird.Factory.pas',
  dorm.loggers.CodeSite in '..\..\lib\DORM\dorm.loggers.CodeSite.pas',
  dorm.Utils in '..\..\lib\DORM\dorm.Utils.pas',
  dorm.UOW in '..\..\lib\DORM\dorm.UOW.pas',
  dorm.InterposedObject in '..\..\lib\DORM\dorm.InterposedObject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm6, Form6);
  Application.Run;
end.
