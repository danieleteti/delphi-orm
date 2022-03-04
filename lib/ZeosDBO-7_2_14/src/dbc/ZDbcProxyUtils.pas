{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{       DBC Layer Proxy Database Connectivity Classes     }
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

unit ZDbcProxyUtils;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_PROXY} //if set we have an empty unit

function XMLEncode(Input: String): String;

{$ENDIF ZEOS_DISABLE_PROXY} //if set we have an empty unit

implementation

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_PROXY} //if set we have an empty unit

uses SysUtils;

function XMLEncode(Input: String): String;
var
  x: Integer;
  Position: Integer;

  procedure CutAndInsert(Replacement: String);
  begin
    if Position < x then Result := Result + Copy(Input, Position, x - Position);
    Result := Result + Replacement;
    Position := x + 1;
  end;
begin
  Position := 1;
  Result := '';
  for x := 1 to Length(Input) do begin
    case Input[x] of
      #9, #10, #13, '%': CutAndInsert('&#' + IntToStr(Ord(Input[x])) + ';');
      #00..#8, #11, #12, #14..#31: raise Exception.Create('Character #' + IntToStr(Ord(Input[x])) + ' is not allowed in strings.');
      '<': CutAndInsert('&lt;');
      '>': CutAndInsert('&gt;');
      '&': CutAndInsert('&amp;');
      '''': CutAndInsert('&apos;');
      '"': CutAndInsert('&quot;');
    end;
  end;
  if Position <= Length(Input) then Result := Result + Copy(Input, Position, Length(Input));
end;

{$ENDIF ZEOS_DISABLE_PROXY} //if set we have an empty unit

end.