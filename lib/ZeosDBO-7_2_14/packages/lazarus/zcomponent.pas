{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit zcomponent;

{$warn 5023 off : no warning about unused units}
interface

uses
  ZComponentReg, ZConnection, ZDatasetUtils, ZUpdateSqlEditor, 
  ZConnectionGroup, ZGroupedConnection, ZIBEventAlerter, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ZComponentReg', @ZComponentReg.Register);
end;

initialization
  RegisterPackage('zcomponent', @Register);
end.
