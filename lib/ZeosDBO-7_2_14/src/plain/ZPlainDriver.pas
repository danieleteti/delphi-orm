{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Interfaces for Native Plain Drivers          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
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

unit ZPlainDriver;

interface

{$I ZPlain.inc}

uses ZClasses, ZPlainLoader, ZCompatibility, Types;

type

  {** Represents a generic interface to plain driver. }
  IZPlainDriver = interface (IZInterface)
    ['{2A0CC600-B3C4-43AF-92F5-C22A3BB1BB7D}']
    function GetProtocol: string;
    function GetDescription: string;
    {EgonHugeist:
      Why this here? -> No one else then Plaindriver knows which Characterset
      is supported. Here i've made a intervention in dependency of used Compiler.}
    function GetSupportedClientCodePages(const {$IFNDEF UNICODE}AutoEncode,{$ENDIF} IgnoreUnsupported: Boolean;
      CtrlsCPType: TZControlsCodePage = cCP_UTF16): TStringDynArray;
    function ValidateCharEncoding(const CharacterSetName: String; const DoArrange: Boolean = False): PZCodePage; overload;
    function ValidateCharEncoding(const CharacterSetID: Integer; const DoArrange: Boolean = False): PZCodePage; overload;
    procedure Initialize(const Location: String = '');
    function Clone: IZPlainDriver;
    procedure AddCodePage(const Name: String; const ID:  Integer;
      Encoding: TZCharEncoding = ceAnsi; const CP: Word = $ffff;
      const ZAlias: String = ''; CharWidth: Integer = 1;
      const ConsistentCP: Boolean = True);
  end;

  {ADDED by fduenas 15-06-2006}
  {** Base class of a generic plain driver with TZNativeLibraryLoader-object. }

  TZAbstractPlainDriver = class(TInterfacedObject, IZPlainDriver)
  protected
    FCodePages: array of TZCodePage;
    FLoader: TZNativeLibraryLoader;
    procedure LoadApi; virtual;
    function Clone: IZPlainDriver; reintroduce; virtual; abstract;
    procedure LoadCodePages; virtual; abstract;
    function GetUnicodeCodePageName: String; virtual;
    function ValidateCharEncoding(const CharacterSetName: String; const DoArrange: Boolean = False): PZCodePage; overload;
    function ValidateCharEncoding(const CharacterSetID: Integer; const DoArrange: Boolean = False): PZCodePage; overload;
  public
    constructor Create;
    constructor CreateWithLibrary(const LibName : String);
    destructor Destroy; override;
    function GetProtocol: string; virtual; abstract;
    function GetDescription: string; virtual; abstract;
    function GetSupportedClientCodePages(const {$IFNDEF UNICODE}AutoEncode,{$ENDIF} IgnoreUnsupported: Boolean;
      CtrlsCPType: TZControlsCodePage = cCP_UTF16): TStringDynArray;
    procedure Initialize(const Location: String = ''); virtual;

    property Loader: TZNativeLibraryLoader read FLoader;
    procedure AddCodePage(const Name: String; const ID:  Integer;
      Encoding: TZCharEncoding = ceAnsi; const CP: Word = $ffff;
      const ZAlias: String = ''; CharWidth: Integer = 1;
      const ConsistentCP: Boolean = True);
    procedure ResetCodePage(const OldID: Integer; const Name: String;
      const ID:  Integer; {may be an ordinal value of predefined Types...}
      Encoding: TZCharEncoding = ceAnsi; const CP: Word = $ffff;
      const ZAlias: String = ''; CharWidth: Integer = 1;
      const ConsistentCP: Boolean = True);
  end;
  {END ADDED by fduenas 15-06-2006}

implementation

uses SysUtils, ZEncoding{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};


{TZAbstractPlainDriver}

function TZAbstractPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := '';
end;

{**
   Checks if the given ClientCharacterSet and returns the PZCodePage
   @param CharacterSetName the Name wich has to be validated
   @param DoArrange means if the CharacterSet is empty or unsupported then find
          a supported CodePage
   @result the PZCodePage of the ClientCharacterSet
}
function TZAbstractPlainDriver.ValidateCharEncoding(const CharacterSetName: String;
  const DoArrange: Boolean = False): PZCodePage;

  function GetClientCodePageInformations(
    const ClientCharacterSet: String): PZCodePage;
  var
    I: Integer;
  begin
    {now check for PlainDriver-Informations...}
    {$IFDEF FPC} //if the user didn't set it
    if ClientCharacterSet = '' then
    begin
      for i := Low(FCodePages) to high(FCodePages) do
        if UpperCase(FCodePages[i].Name) = UpperCase(GetUnicodeCodePageName) then
        begin
          Result := @FCodePages[i];
          Exit;
        end;
    end
    else
    {$ENDIF}
    for i := Low(FCodePages) to high(FCodePages) do
      if UpperCase(FCodePages[i].Name) = UpperCase(ClientCharacterSet) then
      begin
        Result := @FCodePages[i];
        Exit;
      end;
    Result := @ClientCodePageDummy;
  end;
begin
  Result := GetClientCodePageInformations(CharacterSetName);
  if (DoArrange) and (Result^.ZAlias <> '' ) then
    Result := ValidateCharEncoding(Result^.ZAlias); //recalls em selves
end;

{**
   Checks if the given ClientCharacterSet and returns the PZCodePage
   @param CharacterSetID the ID wich has to be validated
   @param DoArrange means if the CharacterSet is empty or unsupported then find
          a supported CodePage
   @result the PZCodePage of the ClientCharacterSet
}
function TZAbstractPlainDriver.ValidateCharEncoding(const CharacterSetID: Integer;
  const DoArrange: Boolean = False): PZCodePage;

  function GetClientCodePageInformations(const ClientCharacterSetID: Word): PZCodePage;
  var
    I: Integer;
  begin
    {now check for PlainDriver-Informations...}
    for i := Low(FCodePages) to high(FCodePages) do
      if FCodePages[i].ID = ClientCharacterSetID then
      begin
        Result := @FCodePages[i];
        Exit;
      end;
    Result := @ClientCodePageDummy;
  end;
begin
  Result := GetClientCodePageInformations(CharacterSetID);

  if (DoArrange) and (Result^.ZAlias <> '' ) then
    ValidateCharEncoding(Result^.ZAlias); //recalls em selves
end;

procedure TZAbstractPlainDriver.AddCodePage(const Name: String;
      const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = $ffff; const ZAlias: String = '';
      CharWidth: Integer = 1; const ConsistentCP: Boolean = True);
begin
  SetLength(FCodePages, Length(FCodePages)+1);
  FCodePages[High(FCodePages)].Name := Name;
  FCodePages[High(FCodePages)].ID := ID;
  FCodePages[High(FCodePages)].Encoding := Encoding;
  FCodePages[High(FCodePages)].CP := CP;
  FCodePages[High(FCodePages)].CharWidth := CharWidth;
  FCodePages[High(FCodePages)].ZAlias := ZAlias;
  FCodePages[High(FCodePages)].IsStringFieldCPConsistent := ConsistentCP;

  if CP = $ffff then
    FCodePages[High(FCodePages)].ZAlias := GetUnicodeCodePageName;
end;

procedure TZAbstractPlainDriver.ResetCodePage(const OldID: Integer;
      const Name: String; const ID:  Integer; Encoding: TZCharEncoding = ceAnsi;
      const CP: Word = $ffff;
      const ZAlias: String = ''; CharWidth: Integer = 1;
      const ConsistentCP: Boolean = True);
var
  I: Integer;
begin
  for i := low(FCodePages) to high(FCodePages) do
    if OldID = FCodePages[I].ID then
    begin
      FCodePages[I].ID := ID;
      FCodePages[I].Name := Name;
      FCodePages[I].Encoding := Encoding;
      FCodePages[I].CP := CP;
      FCodePages[I].ZAlias := ZAlias;
      FCodePages[I].CharWidth := CharWidth;
      FCodePages[I].IsStringFieldCPConsistent := ConsistentCP;

      if CP = $ffff then
        FCodePages[I].ZAlias := GetUnicodeCodePageName;
      Break;
    end;
end;

function TZAbstractPlainDriver.GetSupportedClientCodePages(
  const {$IFNDEF UNICODE}AutoEncode,{$ENDIF} IgnoreUnsupported: Boolean;
  CtrlsCPType: TZControlsCodePage = cCP_UTF16): TStringDynArray;
var
  I: Integer;

  procedure AddCurrent;
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := FCodePages[i].Name;
  end;

begin
  SetLength(Result, 0);
  for i := low(FCodePages) to high(FCodePages) do
    if IgnoreUnsupported then
      AddCurrent
    else
      case CtrlsCPType of
        cGET_ACP:
          {$IFDEF UNICODE}
          AddCurrent; //result are ?valid? but does that makes sence for all if not CP_UTF8?
          {$ELSE}
          if ( FCodePages[i].CP = ZOSCodePage ) then
            AddCurrent
          else
            if AutoEncode then
              {$IF defined(MSWINDOWS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER) }
              AddCurrent //result are ?valid? but does that makes sence for all if not CP_UTF8?
              {$ELSE}
                {$IFDEF WITH_LCONVENCODING} //Lazarus only
                if ( IsLConvEncodingCodePage(FCodePages[i].CP) ) or
                   ( FCodePages[i].Encoding = ceUTF8 ) then
                  AddCurrent //allways valid because result is allways UTF8 which lazarus expects
                {$ENDIF}
              {$IFEND}
            else Continue;
          {$ENDIF}
        {$IFNDEF UNICODE}
        cCP_UTF8:
          if ( FCodePages[i].Encoding = ceUTF8 ) then
            AddCurrent
          else
            if AutoEncode then
              {$IF defined(MSWINDOWS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER) }
              AddCurrent //All charsets can be converted to UTF8 if a valid WideString-Manager does exists
              {$ELSE}
                {$IFDEF WITH_LCONVENCODING} //Lazarus only
                if ( IsLConvEncodingCodePage(FCodePages[i].CP) ) then
                  AddCurrent
                {$ENDIF}
              {$IFEND}
            else Continue;
        {$ENDIF}
        else
          {$IF defined(MSWINDOWS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER) or defined(UNICODE)}
          AddCurrent; //all remaining charset can be converted to wide if a valid WideString-Manager does exists
          {$ELSE}
            {$IFDEF WITH_LCONVENCODING} //Lazarus only
            if ( IsLConvEncodingCodePage(FCodePages[i].CP) ) or //Lazarus can convert to UTF8 then we convert to wide (double En/Decoding!)
               ( FCodePages[i].Encoding = ceUTF8 ) or //decode the strings to wide
               ( FCodePages[i].CP = ZOSCodePage ) then //to allow a valid cast
              AddCurrent; //all these charset can be converted to wide
            {$ELSE}
            if ( FCodePages[i].CP = ZOSCodePage ) or //to allow a valid cast
               ( FCodePages[i].Encoding = ceUTF8 ) then //decode the strings to wide
              AddCurrent;
            {$ENDIF}
          {$IFEND}
      end;
end;

constructor TZAbstractPlainDriver.Create;
begin
  inherited Create;
end;

destructor TZAbstractPlainDriver.Destroy;
begin
  SetLength(FCodePages, 0);
  if Assigned(FLoader) then
    FreeAndNil(FLoader);
  inherited Destroy;
end;


procedure TZAbstractPlainDriver.LoadApi;
begin

end;

constructor TZAbstractPlainDriver.CreateWithLibrary(const LibName: String);
begin
  Inherited Create;
  if Assigned(FLoader) then
  begin
    Loader.ClearLocations;
    Loader.AddLocation(LibName);
  end;
end;

procedure TZAbstractPlainDriver.Initialize(const Location: String);
begin
  If Assigned(Loader) then
    if not Loader.Loaded then
    begin
      if Location <> '' then
      begin
        Loader.ClearLocations;
        Loader.AddLocation(Location);
      end;
      If Loader.LoadNativeLibrary then
        LoadApi;
    end;
end;

end.

