unit dorm.Component.Registrations;

interface

uses
  System.SysUtils, System.Classes, dorm.Component.Session, DesignIntf;

procedure Register;

implementation

//uses dorm.Component.SessionComponentEditor;

procedure Register;
begin
  RegisterComponents('DORM', [TdormSession]);
  //RegisterComponentEditor(TdormSession, TdormSessionEditor);
end;

end.
