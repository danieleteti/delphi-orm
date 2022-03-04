{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Abstract Table component                  }
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

unit ZAbstractTable;

interface

{$I ZComponent.inc}

uses
  SysUtils, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF}
  ZAbstractDataset;

type

  {**
    Abstract dataset component which works with one specified table.
  }
  TZAbstractTable = class(TZAbstractDataset)
  private
    FTableName: string;

  private
    function GetExists: Boolean;
    procedure SetTableName(const Value: string);

  protected
    function PSIsSQLBased: Boolean; override;
  {$IFDEF WITH_IPROVIDER}
    {$IFDEF  WITH_IPROVIDERWIDE}
    function PSGetTableNameW: WideString; override;
    {$ELSE}
    function PSGetTableName: string; override;
    {$ENDIF}
    procedure PSSetCommandText(const ACommandText: string); override;
  {$ENDIF}

  protected
    property Exists: Boolean read GetExists;
    property TableName: string read FTableName write SetTableName;
  end;

implementation


{ TZAbstractTable }

{**
  Checks if a table with the corresponding name exists in the database.
  @return <code>True</code> if the the table exists.
}
function TZAbstractTable.GetExists: Boolean;
var
  TableList: TStringList;
begin
  TableList := TStringList.Create;
  try
    CheckConnected;
    Connection.GetTableNames(TableName, TableList);
    TableList.CaseSensitive := False;
    Result := (TableList.IndexOf(TableName) >= 0); // look for an exact match
  finally
    TableList.Free;
  end;
end;

{**
  Sets a new table name and generates a related SQL statement.
  @param Value a new name of table.
}
procedure TZAbstractTable.SetTableName(const Value: string);
begin
  if FTableName <> Value then
  begin
    FTableName := Value;
    if Value <> '' then
      SQL.Text := Format('SELECT * FROM %s', [FTableName])
    else SQL.Text := '';
  end;
end;

{**
  Checks if dataset can execute SQL queries?
  @returns <code>True</code> if the query can execute SQL.
}
function TZAbstractTable.PSIsSQLBased: Boolean;
begin
  Result := False;
end;

{$IFDEF WITH_IPROVIDER}

{**
  Gets the name of the table.
  @returns the name of this table.
}
{$IFDEF WITH_IPROVIDERWIDE}
function TZAbstractTable.PSGetTableNameW: WideString;
{$ELSE}
function TZAbstractTable.PSGetTableName: string;
{$ENDIF}
begin
  Result := TableName;
end;

{**
  Assignes a new name for this table.
  @param ACommandText a new name for this table.
}
procedure TZAbstractTable.PSSetCommandText(const ACommandText: string);
begin
  TableName := ACommandText;
end;

{$ENDIF}

end.
