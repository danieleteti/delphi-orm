{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               URL Connectivity Classes                  }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}
unit ZURL;

interface
{$I ZCore.inc}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  SysUtils;

type
  // List of URL properties that could operate with URL-escaped strings
  TZURLStringList = Class(TStringList)
  protected
    function GetURLText: String;
    procedure SetURLText(const Value: string);
  public
    property URLText: String read GetURLText write SetURLText;
  end;

  TZURL = class
  private
    FPrefix: string;
    FProtocol: string;
    FHostName: string;
    FPort: Integer;
    FDatabase: string;
    FUserName: string;
    FPassword: string;
    FLibLocation: String;
    FProperties: TZURLStringList;
    FOnPropertiesChange: TNotifyEvent;
    procedure SetPrefix(const Value: string);
    procedure SetProtocol(const Value: string);
    procedure SetHostName(const Value: string);
    procedure SetConnPort(const Value: Integer);
    function GetDatabase: string;
    procedure SetDatabase(const Value: string);
    function GetUserName: string;
    procedure SetUserName(const Value: string);
    function GetPassword: string;
    procedure SetPassword(const Value: string);
    function GetLibLocation: String;
    procedure SetLibLocation(const Value: String);
    function GetURL: string;
    procedure SetURL(const Value: string);
    procedure DoOnPropertiesChange(Sender: TObject);
    procedure AddValues(Values: TStrings);
  public
    constructor Create; overload;
    constructor Create(const AURL: String); overload;
    constructor Create(const AURL: String; Info: TStrings); overload;
    constructor Create(const AURL: TZURL); overload;
    constructor Create(Const AURL, AHostName: string; const APort: Integer;
      const ADatabase, AUser, APassword: string; Info: TStrings); overload;

    destructor Destroy; override;
    property Prefix: string read FPrefix write SetPrefix;
    property Protocol: string read FProtocol write SetProtocol;
    property HostName: string read FHostName write SetHostName;
    property Port: Integer read FPort write SetConnPort;
    property Database: string read GetDatabase write SetDatabase;
    property UserName: string read GetUserName write SetUserName;
    property Password: string read GetPassword write SetPassword;
    property LibLocation: string read GetLibLocation write SetLibLocation;
    property Properties: TZURLStringList read FProperties;
    property URL: string read GetURL write SetURL;

    property OnPropertiesChange: TNotifyEvent read FOnPropertiesChange write FOnPropertiesChange;
  end;

implementation

uses ZCompatibility, ZFastCode, ZSysUtils;

// escape the ';' char to #9 and LineEnding to ';'
function Escape(const S: string): string; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result := ReplaceChar(';', #9, S);
  Result := StringReplace(Result, LineEnding, ';', [rfReplaceAll]);
end;

// unescape the ';' to LineEnding and #9 char to ';'
function UnEscape(const S: string): string; {$IFDEF WITH_INLINE}inline;{$ENDIF}
begin
  Result := StringReplace(S, ';', LineEnding, [rfReplaceAll]);
  Result := ReplaceChar(#9, ';', Result);
end;

{TZURLStringList}

function TZURLStringList.GetURLText: String;
var P: PChar absolute Result;
begin
  Result := Escape(Text);
  if (P+Length(Result)-1)^ = ';' then
    SetLength(Result, Length(Result)-1);
end;

procedure TZURLStringList.SetURLText(const Value: string);
begin
  Text := UnEscape(Value);
end;

{ TZURL }

constructor TZURL.Create;
begin
  inherited;

  FPrefix := 'zdbc';
  FProperties := TZURLStringList.Create;
  FProperties.CaseSensitive := False;
  FProperties.NameValueSeparator := '=';
  FProperties.OnChange := DoOnPropertiesChange;
end;

constructor TZURL.Create(const AURL: String);
begin
  Create;
  Self.URL := AURL;
end;

// Values from Info overwrite those from URL
constructor TZURL.Create(const AURL: String; Info: TStrings);
begin
  Create(AURL);
  if Assigned(Info) then
    AddValues(Info);
end;

constructor TZURL.Create(const AURL: TZURL);
begin
  Create(AURL.URL);
end;

// Values from parameters overwrite those from URL and values from Info overwrite both
// TODO: this method is odd... properties of URL, except protocol, get overridden
// with parameters. Likely AProtocol should go here instead of AURL
constructor TZURL.Create(Const AURL, AHostName: string; const APort: Integer;
  const ADatabase, AUser, APassword: string; Info: TStrings);
begin
  Create(AURL);
  Self.HostName := AHostName;
  Self.Port := APort;
  Self.Database := ADataBase;
  Self.UserName := AUser;
  Self.Password := APassword;
  if Assigned(Info) then
    AddValues(Info);
end;

destructor TZURL.Destroy;
begin
  FProperties.Free;

  inherited;
end;

procedure TZURL.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

procedure TZURL.SetProtocol(const Value: string);
begin
  FProtocol := Value;
end;

procedure TZURL.SetHostName(const Value: string);
begin
  FHostName := Escape(Value);
end;

procedure TZURL.SetConnPort(const Value: Integer);
begin
  FPort := Value;
end;

function TZURL.GetDatabase: string;
begin
  Result := UnEscape(FDatabase);
end;

procedure TZURL.SetDatabase(const Value: string);
begin
  FDatabase := Escape(Value);
end;

function TZURL.GetUserName: string;
begin
  Result := UnEscape(FUserName);
end;

procedure TZURL.SetUserName(const Value: string);
begin
  FUserName := Escape(Value);
end;

function TZURL.GetPassword: string;
begin
  Result := UnEscape(FPassword);
end;

procedure TZURL.SetPassword(const Value: string);
begin
  FPassword := Escape(Value);
end;

function TZURL.GetLibLocation: String;
begin
  Result := UnEscape(FLibLocation);
end;

procedure TZURL.SetLibLocation(const Value: String);
begin
  FLibLocation := Escape(Value);
end;

function TZURL.GetURL: string;
var
  Params: string;
begin
  // Prefix, Protocol and always set the doubleslash to avoid unix '/' path issues if host is empty
  Result := Prefix + ':' + Protocol + ':' + '//';

  // HostName/Port
  if HostName <> '' then
  begin
    Result := Result + HostName;
    if Port <> 0 then
      Result := Result + ':' + ZFastCode.IntToStr(Port);
  end;

  // Database
  if Database <> '' then
    Result := Result + '/' + FDatabase;

  // Join the params

  Params := '';

  if FUserName <> '' then
    AppendSepString(Params, 'username=' + FUserName, ';');
  if FPassword <> '' then
    AppendSepString(Params, 'password=' + FPassword, ';');
  if Properties.Count > 0 then
    AppendSepString(Params, Properties.URLText, ';'); //Adds the escaped string
  if FLibLocation <> '' then
    AppendSepString(Params, 'LibLocation='+ FLibLocation, ';');

  // Construct the final string

  if Params <> '' then
    Result := Result + '?' + Params;
end;

procedure TZURL.SetURL(const Value: string);
var
  APrefix: string;
  AProtocol: string;
  AHostName: string;
  APort: string;
  ADatabase: string;
  AProperties: string;
  AValue: string;
  I: Integer;
begin
  APrefix := '';
  AProtocol := '';
  AHostName := '';
  APort := '';
  ADatabase := '';
  AProperties := '';

  // Strip out the parameters
  BreakString(Value, '?', AValue, AProperties);

  // APrefix
  I := ZFastCode.Pos(':', AValue);
  if I = 0 then
    raise Exception.Create('TZURL.SetURL - The prefix is missing');
  BreakString(AValue, ':', APrefix, AValue);

  // AProtocol
  I := ZFastCode.Pos(':', AValue);
  if I = 0 then
    raise Exception.Create('TZURL.SetURL - The protocol is missing');
  BreakString(AValue, ':', AProtocol, AValue);

  if StartsWith(AValue, '//') then
  begin
    Delete(AValue, 1, Length('//'));
    // Strip "hostname[:port]" out of "/database"
    BreakString(AValue, '/', AValue, ADatabase);
    // AHostName, APort
    BreakString(AValue, ':', AHostName, APort);
  end
  else
  begin
    // Likely a database delimited by / so remove the /
    if StartsWith(AValue, '/') then
      Delete(AValue, 1, Length('/'));
    // ADatabase
    ADatabase := AValue;
  end;

  FPrefix := APrefix;
  FProtocol := AProtocol;
  FHostName := AHostName;
  FPort := StrToIntDef(APort, 0);
  FDatabase := ADatabase;

  // Clear fields that MUST be assigned from properties even if empty.
  // LibLocation should remain uncleared
  FUserName := '';
  FPassword := '';
  FProperties.URLText := AProperties; // will launch DoOnPropertiesChange
end;

procedure TZURL.DoOnPropertiesChange(Sender: TObject);

  // Return a value named ValueName from FProperties and delete the item
  function ExtractValueFromProperties(const ValueName: string): string;
  var I: Integer;
  begin
    Result := '';
    I := FProperties.IndexOfName(ValueName);
    if I = -1 then Exit;
    Result := FProperties.ValueFromIndex[I];
    FProperties.Delete(I);
  end;

var
  S: string;
begin
  FProperties.OnChange := nil; // prevent re-entering

  // Assign UserName, Password and LibLocation if they're set in Properties
  S := ExtractValueFromProperties('UID');
  if S <> '' then
    UserName := S;

  S := ExtractValueFromProperties('username');
  if S <> '' then
    UserName := S;

  S := ExtractValueFromProperties('PWD');
  if S <> '' then
    Password := S;

  S := ExtractValueFromProperties('password');
  if S <> '' then
    Password := S;

  S := ExtractValueFromProperties('LibLocation');
  if S <> '' then
    LibLocation := S;

  FProperties.OnChange := DoOnPropertiesChange;

  if Assigned(FOnPropertiesChange) then
    FOnPropertiesChange(Sender);
end;

procedure TZURL.AddValues(Values: TStrings);
var
  I: Integer;
  Param, Value: String;
begin
  FProperties.BeginUpdate; // prevent calling OnChange on every iteration
  for I := 0 to Values.Count -1 do
  begin
    BreakString(Values[I], '=', Param, Value);
    if Value <> '' then
      FProperties.Values[Param] := Value
    else
      if FProperties.IndexOf(Values[I]) = -1 then //add unique params only!
        FProperties.Add(Values[I]);
  end;
  FProperties.EndUpdate;
end;

end.
