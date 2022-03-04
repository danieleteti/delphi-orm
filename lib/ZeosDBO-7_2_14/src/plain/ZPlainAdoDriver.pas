{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Delphi plain driver interface to ADO            }
{                                                         }
{        Originally written by Janos Fegyverneki          }
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

unit ZPlainAdoDriver;

interface

{$I ZPlain.inc}

{$IF not defined(MSWINDOWS)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ADO}

uses {$IFDEF OLDFPC}ZClasses,{$ENDIF} ZPlainDriver;

type
  TZAdoPlainDriver = class (TZAbstractPlainDriver, IZPlainDriver)
  protected
    function GetUnicodeCodePageName: String; override;
  public
    constructor Create;

    procedure LoadCodePages; override;
    function GetProtocol: string; override;
    function GetDescription: string; override;
    procedure Initialize(const {%H-}Location: String = ''); override;
    function Clone: IZPlainDriver; override;
  end;

{$ENDIF ZEOS_DISABLE_ADO}

implementation

{$IFNDEF ZEOS_DISABLE_ADO}

uses ZCompatibility, ZEncoding, Windows;

procedure TZAdoPlainDriver.LoadCodePages;
begin
  AddCodePage('CP_UTF16', 0, ceUTF16, GetACP,'', 1, True);
end;

constructor TZAdoPlainDriver.Create;
begin
  LoadCodePages;
end;

function TZAdoPlainDriver.GetProtocol: string;
begin
  Result := 'ado';
end;

function TZAdoPlainDriver.GetUnicodeCodePageName: String;
begin
  Result := 'CP_UTF16';
end;

function TZAdoPlainDriver.GetDescription: string;
begin
  Result := 'Native driver for Microsoft ADO';
end;

procedure TZAdoPlainDriver.Initialize(const Location: String = '');
begin
end;

function TZAdoPlainDriver.Clone: IZPlainDriver;
begin
  Result := Self;
end;

{$ENDIF ZEOS_DISABLE_ADO}

end.