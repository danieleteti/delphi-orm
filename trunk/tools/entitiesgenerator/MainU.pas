unit MainU;

interface

uses cOMMONSu;

procedure Main;

implementation

uses
  System.ioutils, MainDMU, System.Classes, GeneratorU, Data.DB, sysutils,
  strutils;

procedure Main;
var
  ConfigFileName, s: String;
  dm: TdmMain;
  slTables, sl: TStringList;
  Gen: TGenerator;

begin
  ConfigFileName := ParamStr(1);
  sl := TStringList.Create;
  try
    sl.LoadFromFile('generator_config.txt');
    TConfig.CAPITALIZE := sl.Values['CAPITALIZE'].trim = '1';
    TConfig.PROPERTYID := sl.Values['PROPERTYID'].trim = '1';
    TConfig.PROPERTYOBJVERSION := sl.Values['PROPERTYOBJVERSION'].trim = '1';
    TConfig.COLUMNATTRIBUTE := sl.Values['COLUMNATTRIBUTE'].trim = '1';
    TConfig.PARENTCLASS := ifthen(not sl.Values['PARENTCLASS'].trim.IsEmpty,
      sl.Values['PARENTCLASS'], 'TObject');
    TConfig.TABLES := sl.Values['TABLES'].trim;
    TConfig.OUTPUTFILENAME :=
      ifthen(not sl.Values['OUTPUTFILENAME'].trim.IsEmpty,
      sl.Values['OUTPUTFILENAME'], 'GeneratedClasses.pas');
    if TConfig.CAPITALIZE and TConfig.COLUMNATTRIBUTE then
      raise Exception.Create
        ('Cannot use CAPITALIZED properties without COLUMN attribute');

  finally
    sl.Free;
  end;

  dm := TdmMain.Create(nil);
  try
    // dm.Connection.Open;
    // dm.Connection.Params.Clear;
    // // dm.Connection.Params.LoadFromFile(ConfigFileName);
    // dm.Connection.DriverName := 'MSSQL';
    // dm.Connection.Params.Values['DriverID'] := 'MSSQL';
    // dm.Connection.Params.Values['Database'] := 'keystone2';
    // dm.Connection.Params.Values['User_Name'] := 'sa';
    // dm.Connection.Params.Values['Password'] := 'B4s1cs2013';
    // dm.Connection.Params.Values['Server'] := '192.138.3.64\SQLEXPRESS2012';

    {
      Database=keystone2
      User_Name=sa
      Password=B4s1cs2013
      Server=192.138.3.64\SQLEXPRESS2012
      DriverID=MSSQL

    }

    // for s in dm.Connection.Params do
    // begin
    // WriteLn(s);
    // end;

    dm.Connection.Connected := True;
    slTables := TStringList.Create;
    try
      if TConfig.TABLES.IsEmpty then
        dm.Connection.GetTableNames('keystone2', 'dbo', '', slTables)
      else
      begin
        slTables.AddStrings(TConfig.TABLES.Split([',']));
      end;
      Gen := TGenerator.Create(slTables,
        function(ATableName: String): TDataSet
        begin
          dm.qry.Close;
          dm.qry.Open('SELECT * FROM ' + ATableName + ' WHERE 1=0');
          Exit(dm.qry);
        end);
      try
        Gen.Execute;
        Gen.Output.SaveToFile(TConfig.OUTPUTFILENAME);
      finally
        Gen.Free;
      end;
    finally
      slTables.Free;
    end;
  finally
    dm.Free;
  end;
end;

end.
