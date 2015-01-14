program entitiesgen;

{$APPTYPE CONSOLE}
{$R *.res}


uses
  System.SysUtils,
  MainU in 'MainU.pas',
  MainDMU in 'MainDMU.pas' {dmMain: TDataModule} ,
  GeneratorU in 'GeneratorU.pas',
  CommonsU in 'CommonsU.pas';

procedure Welcome;
begin
  WriteLn('******************************************************************');
  WriteLn(' DORM Entities Generator - Copyright 2010-2015 Daniele Teti');
  WriteLn(' Web site: www.bittimeprofessionals.it - Blog: www.danieleteti.it');
  WriteLn('******************************************************************')
end;

begin
  try
    Welcome;
    Main;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;

  // if DebugHook<>0 then
  // ReadLn;
end.
