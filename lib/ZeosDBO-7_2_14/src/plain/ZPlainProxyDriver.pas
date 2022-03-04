{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           DBC Layer Proxy Connectivity Classes          }
{                                                         }
{        Originally written by Jan Baumgarten             }
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
{  http://zeoslib.sourceforge.net  (FORUM)                }
{  http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER) }
{  http://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{  http://www.sourceforge.net/projects/zeoslib.           }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZPlainProxyDriver;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_PROXY}

uses SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  {$IFDEF OLDFPC}ZClasses,{$ENDIF} ZCompatibility, ZPlainDriver,
  {$IFDEF ZEOS_PROXY_USE_INTERNAL_PROXY}ZPlainProxyDriverInternalProxy, {$ENDIF}
  ZPlainProxyDriverIntf;

{$IFNDEF ZEOS_PROXY_USE_INTERNAL_PROXY}
const
  WINDOWS_DLL_LOCATION = 'libzdbcproxy.dll';
  LINUX_DLL_LOCATION = 'libzdbcproxy.'+SharedSuffix;
{$ENDIF ZEOS_PROXY_USE_INTERNAL_PROXY}

{$IFNDEF ZEOS_PROXY_USE_INTERNAL_PROXY}
type
  TZDbcProxy_GetInterface = function(): IZDbcProxy; stdcall;
  TZDbcProxy_GetLastErrorStr = function(): WideString; stdcall;
{$ENDIF ZEOS_PROXY_USE_INTERNAL_PROXY}

  { ************* Plain API Function variables definition ************ }
type
  {** Represents a generic interface to DBC Proxy native API. }
  IZProxyPlainDriver = interface (IZPlainDriver)
    ['{89102437-9555-449C-922A-5734581AC569}']
    function GetLibraryInterface(): IZDbcProxy;
    function GetLastErrorStr(): WideString;
  end;

  {** Implements a base driver for DBC Proxy}
  TZProxyBaseDriver = class (TZAbstractPlainDriver, IZPlainDriver, IZProxyPlainDriver)
  private
  protected
    {$IFNDEF ZEOS_PROXY_USE_INTERNAL_PROXY}
    FGetInterface: TZDbcProxy_GetInterface;
    FGetLastErrorStr: TZDbcProxy_GetLastErrorStr;
    {$ENDIF ZEOS_PROXY_USE_INTERNAL_PROXY}

    function GetUnicodeCodePageName: String; override;
    procedure LoadCodePages; override;
    function Clone: IZPlainDriver; override;
    procedure LoadApi; override;
  public
    constructor Create;
    function GetProtocol: string; override;
    function GetDescription: string; override;

    function GetLibraryInterface(): IZDbcProxy;
    function GetLastErrorStr(): WideString;
  end;

{$ENDIF ZEOS_DISABLE_PROXY}

implementation

{$IFNDEF ZEOS_DISABLE_PROXY}

uses ZPlainLoader, ZEncoding{$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZSQLiteBaseDriver }

function TZProxyBaseDriver.GetUnicodeCodePageName: String;
begin
  Result := 'UTF-16'
end;

procedure TZProxyBaseDriver.LoadCodePages;  //Egonhugeist
begin
  { MultiByte }
  AddCodePage('UTF-16', 4, ceUTF16, zCP_UTF16); //Setting this will be ignored by actual Excute of Plaindriver
end;

constructor TZProxyBaseDriver.Create;
begin
  inherited create;
  {$IFNDEF ZEOS_PROXY_USE_INTERNAL_PROXY}
    FLoader := TZNativeLibraryLoader.Create([]);
    {$IFDEF MSWINDOWS}
    FLoader.AddLocation(WINDOWS_DLL_LOCATION);
    {$ELSE}
    FLoader.AddLocation(LINUX_DLL_LOCATION);
    FLoader.AddLocation(LINUX_DLL_LOCATION+'.0');
    {$ENDIF}
  {$ENDIF ZEOS_PROXY_USE_INTERNAL_PROXY}
end;

procedure TZProxyBaseDriver.LoadApi;
begin
{ ************** Load adresses of API Functions ************* }
  {$IFNDEF ZEOS_PROXY_USE_INTERNAL_PROXY}
  with Loader do
  begin
    {@}FGetInterface                := GetAddress('GetInterface');
    {@}FGetLastErrorStr             := GetAddress('GetLastErrorStr');
  end;
  {$ENDIF ZEOS_PROXY_USE_INTERNAL_PROXY}
end;

function TZProxyBaseDriver.GetProtocol: string;
begin
  Result := 'WebServiceProxy';
end;

function TZProxyBaseDriver.GetDescription: string;
begin
  Result := 'Native driver for Web Service based Proxy driver';
end;

function TZProxyBaseDriver.Clone: IZPlainDriver;
begin
  Result := TZProxyBaseDriver.Create;
end;

function TZProxyBaseDriver.GetLibraryInterface(): IZDbcProxy;
begin
  {$IFNDEF ZEOS_PROXY_USE_INTERNAL_PROXY}
  Result := FGetInterface();
  {$ELSE}
  Result := ZPlainProxyDriverInternalProxy.GetInterface;
  {$ENDIF ZEOS_PROXY_USE_INTERNAL_PROXY}
end;

function TZProxyBaseDriver.GetLastErrorStr(): WideString;
begin
  {$IFNDEF ZEOS_PROXY_USE_INTERNAL_PROXY}
  Result := FGetLastErrorStr();
  {$ELSE}
  Result := ZPlainProxyDriverInternalProxy.GetLastErrorStr;
  {$ENDIF ZEOS_PROXY_USE_INTERNAL_PROXY}
end;

{$ENDIF ZEOS_DISABLE_PROXY}

end.

