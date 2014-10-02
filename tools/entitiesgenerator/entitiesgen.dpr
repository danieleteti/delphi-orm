program entitiesgen;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  MainU in 'MainU.pas',
  MainDMU in 'MainDMU.pas' {dmMain: TDataModule},
  GeneratorU in 'GeneratorU.pas',
  CommonsU in 'CommonsU.pas';

begin
  try
    Main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

  // if DebugHook<>0 then
  // ReadLn;
end.
