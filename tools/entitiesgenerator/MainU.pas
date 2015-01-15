unit MainU;

interface

uses CommonsU;

procedure Main;

implementation

uses
  System.ioutils, MainDMU, System.Classes, GeneratorU, Data.DB, sysutils,
  strutils;

procedure Main;
var
  ConfigFileName: String;
  dm: TdmMain;
  slTables, sl, LGeneration: TStringList;
  Gen: TGenerator;
  ExcludedTables: TArray<string>;
  i: integer;
  LRelativePath: String;
begin
  ConfigFileName := ParamStr(1);
  LRelativePath := GetCurrentDir; // TPath.GetDirectoryName(ParamStr(0));
  WriteLn('Using: ', ConfigFileName);
  WriteLn('Relative path: ', LRelativePath);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(TPath.Combine(LRelativePath, 'generator_config.txt'));
    TConfig.CAPITALIZE := sl.Values['CAPITALIZE'].trim = '1';
    TConfig.PROPERTYID := sl.Values['PROPERTYID'].trim = '1';
    TConfig.PROPERTYOBJVERSION := sl.Values['PROPERTYOBJVERSION'].trim = '1';
    TConfig.COLUMNATTRIBUTE := sl.Values['COLUMNATTRIBUTE'].trim = '1';
    TConfig.PARENTCLASS := ifthen(not sl.Values['PARENTCLASS'].trim.IsEmpty,
      sl.Values['PARENTCLASS'], 'TObject');
    TConfig.CLASSSUFFIX := sl.Values['CLASSSUFFIX'].trim;
    TConfig.TABLES := sl.Values['TABLES'].trim;
    TConfig.ExcludedTables := sl.Values['EXCLUDEDTABLES'].trim;
    TConfig.EXCLUDEDCOLUMNS := sl.Values['EXCLUDEDCOLUMNS'].trim;
    TConfig.INTERFACEUSES := sl.Values['INTERFACEUSES'].trim;
    TConfig.OUTPUTFILENAME :=
      ifthen(not sl.Values['OUTPUTFILENAME'].trim.IsEmpty,
      sl.Values['OUTPUTFILENAME'], 'GeneratedClasses.pas');
    TConfig.TABLESCOLUMNSNULLABLE := sl.Values['TABLESCOLUMNSNULLABLE'].trim;
    TConfig.FIELDSSERIALIZEASSTRING :=
      sl.Values['FIELDSSERIALIZEASSTRING'].trim;
    TConfig.CATALOGNAME := sl.Values['CATALOGNAME'].trim;
    TConfig.SCHEMANAME := sl.Values['SCHEMANAME'].trim;
    if TConfig.CAPITALIZE and (not TConfig.COLUMNATTRIBUTE) then
      raise Exception.Create
        ('Cannot use CAPITALIZED properties without COLUMN attribute');
  finally
    sl.Free;
  end;

  dm := TdmMain.Create(nil);
  try
    dm.Connection.Params.Clear;
    dm.Connection.Params.LoadFromFile(ConfigFileName);
    dm.Connection.Connected := True;
    slTables := TStringList.Create;
    try
      if TConfig.TABLES.IsEmpty then
        dm.Connection.GetTableNames(TConfig.CATALOGNAME, TConfig.SCHEMANAME,
          '', slTables)
      else
      begin
        slTables.AddStrings(TConfig.TABLES.Split([',']));
      end;
      if not TConfig.ExcludedTables.IsEmpty then
      begin
        ExcludedTables := TConfig.ExcludedTables.Split([',']);
      end;
      for i := slTables.Count - 1 downto 0 do
      begin
        if MatchText(slTables[i], ExcludedTables) then
          slTables.Delete(i);
      end;

      LGeneration := TStringList.Create;
      try

        Gen := TGenerator.Create(slTables,
          function(ATableName: String): TDataSet
          begin
            if LGeneration.IndexOf(ATableName) = -1 then
            begin
              LGeneration.Add(ATableName);
              WriteLn('Generating INTERFACE for ', ATableName);
            end
            else
            begin
              WriteLn('Generating IMPLEMENTATION for ', ATableName)
            end;
            dm.qry.Close;
            dm.qry.Open('SELECT * FROM ' + ATableName + ' WHERE 1=0');
            Exit(dm.qry);
          end);
        try
          Gen.Execute;
          Gen.Output.SaveToFile(TPath.Combine(LRelativePath,
            TConfig.OUTPUTFILENAME));
          WriteLn('Generated file saved to ', TPath.Combine(LRelativePath,
            TConfig.OUTPUTFILENAME));
        finally
          Gen.Free;
        end;
      finally
        LGeneration.Free;
      end;
    finally
      slTables.Free;
    end;
  finally
    dm.Free;
  end;
end;

end.
