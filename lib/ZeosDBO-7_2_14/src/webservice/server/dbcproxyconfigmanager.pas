unit DbcProxyConfigManager;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, generics.collections, ZDbcIntfs;

type
  TDbcProxyConnConfig = record
    ConfigName: String;
    ClientCodepage: String;
    HostName: String;
    Database: String;
    Properties: String;
    LibraryLocation: String;
    Port: Integer;
    Protocol: String;
  end;

  TDbcProxyConnConfigList = TList<TDbcProxyConnConfig>;

  TDbcProxyConfigManager = class
  protected
    ConfigList: TDbcProxyConnConfigList;
  public
    function ConstructUrl(ConfigName, UserName, Password: String): String;
    procedure LoadConfigInfo(SourceFile: String);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  IniFiles;

constructor TDbcProxyConfigManager.Create;
begin
  ConfigList := TDbcProxyConnConfigList.Create;
end;

destructor TDbcProxyConfigManager.Destroy;
begin
  if Assigned(ConfigList) then
    FreeAndNil(ConfigList);
  inherited;
end;

procedure TDbcProxyConfigManager.LoadConfigInfo(SourceFile: String);
var
  ConfigInfo: TDbcProxyConnConfig;
  IniFile: TIniFile;
  Sections: TStringList;
  Section: String;
begin
  IniFile := TIniFile.Create(SourceFile);
  try
    Sections := TStringList.Create;
    try
      IniFile.ReadSections(Sections);
      while Sections.Count > 0 do begin
        Section := Sections.Strings[0];

        ConfigInfo.ConfigName := LowerCase(Section);
        ConfigInfo.ClientCodepage := IniFile.ReadString(Section, 'ClientCodepage', 'UTF8');
        ConfigInfo.Database := IniFile.ReadString(Section, 'Database', '');
        ConfigInfo.HostName := IniFile.ReadString(Section, 'HostName', '');
        ConfigInfo.LibraryLocation := IniFile.ReadString(Section, 'LibraryLocation', '');
        ConfigInfo.Port := IniFile.ReadInteger(Section, 'Port', 0);
        ConfigInfo.Protocol := IniFile.ReadString(Section, 'Protocol', '');
        ConfigInfo.Properties := IniFile.ReadString(Section, 'Properties', '');

        ConfigList.Add(ConfigInfo);
        Sections.Delete(0);
      end;
    finally
      FreeAndNil(Sections);
    end;
  finally
    FreeAndNil(IniFile);
  end;
end;

function TDbcProxyConfigManager.ConstructUrl(ConfigName, UserName, Password: String): String;
var
  x: Integer;
  found: Boolean;
  Cfg: TDbcProxyConnConfig;
begin
  ConfigName := LowerCase(ConfigName);
  found := false;
  for x := 0 to ConfigList.Count - 1 do begin
    if ConfigList.Items[x].ConfigName = ConfigName then begin
      Cfg := ConfigList.Items[x];
      found := true;
      break;
    end;
  end;

  if not found then raise Exception.Create('No config named ' + ConfigName + ' was found.');

  Result := DriverManager.ConstructURL(Cfg.Protocol, Cfg.HostName, Cfg.Database, UserName, Password, Cfg.Port, nil, Cfg.LibraryLocation);
end;

end.

