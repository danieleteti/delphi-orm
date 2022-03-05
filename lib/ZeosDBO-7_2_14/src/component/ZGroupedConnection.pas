{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{      Grouped Database Connection Component              }
{                                                         }
{        Originally written by una.bicicleta              }
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

unit ZGroupedConnection;

interface
{$I ZComponent.inc}

uses
  SysUtils, {Messages, }Classes, ZDbcIntfs, DB, {Forms,}
  ZCompatibility, ZAbstractConnection, ZSequence, //Dialogs,
  ZConnectionGroup {$IFDEF FPC}, LMessages{$ENDIF};
(*
{$IFNDEF FPC}
 const  CM_ZCONNECTIONGROUPCHANGED = WM_USER + 100;
 const  CM_ZCONNECTIONGROUPCHANGE  = WM_USER + 101;
{$ELSE}
const  CM_ZCONNECTIONGROUPCHANGED = LM_USER + 100;
const  CM_ZCONNECTIONGROUPCHANGE  = LM_USER + 101;
{$ENDIF}
  *)
type
  TMsgZDbConnecitionChange = record
    Msg: Cardinal;
    Sender: TComponent;
    ZConnectionGroup: TZConnectionGroup;
    Result: Longint;
  end;

type
  TZGroupedConnection  = class(tZAbstractConnection)
  protected
    FZConnectionGroup: TZConnectionGroup;
    FZConnectionGroupLink: TZConnectionGroupLink;
    procedure SetConnectionGroup(Value: TZConnectionGroup);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
(*  private
    function getUser: string;
    function getPassword: string;
    function getHostName: string;
    function getDatabase: string;
    function GetLibLocation: String;
    //function getCatalog: string;
    procedure DoZConnectionGroupChange(Sender: TObject);
    procedure ParentZConnectionGroupChange(var Msg: TMessage); *)
  published
    property ConnectionGroup: TZConnectionGroup read FZConnectionGroup write SetConnectionGroup;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

(*procedure InternalZConnectionGroupChanged(AControl: TComponent; AZConnectionGroup: TZConnectionGroup);
var
  Msg: TMsgZDbConnecitionChange;
begin
  Msg.Msg := CM_ZCONNECTIONGROUPCHANGED;
  Msg.Sender := AControl;
  Msg.ZConnectionGroup := AZConnectionGroup;
  Msg.Result := 0;
  //AControl.Broadcast(Msg);
end;*)

// === { TZGroupedConnection  } =============================================
constructor TZGroupedConnection .Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FZConnectionGroupLink := TZConnectionGroupLink.Create;
  //FZConnectionGroupLink.OnChange := DoZConnectionGroupChange;
end;

destructor TZGroupedConnection .Destroy;
begin
  FZConnectionGroupLink.Free;
  inherited Destroy;
end;

(*procedure TZGroupedConnection .DoZConnectionGroupChange(Sender: TObject);
begin
  if (Sender is TZConnectionGroup) then
  begin
    FURL.UserName := (Sender as TZConnectionGroup).User;
    FURL.Protocol := (Sender as TZConnectionGroup).Protocol;
    FURL.Password := (Sender as TZConnectionGroup).Password;
    FURL.HostName := (Sender as TZConnectionGroup).HostName;
    FURL.Database := (Sender as TZConnectionGroup).Database;
    FURL.LibLocation := (Sender as TZConnectionGroup).LibraryLocation;
  end;
end;

procedure TZGroupedConnection .ParentZConnectionGroupChange(var Msg: TMessage);
begin
  InternalZConnectionGroupChanged(Self, FZConnectionGroup);
end;
*)
procedure TZGroupedConnection .Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if (AComponent is TDataset) then
      UnregisterDataSet(TDataset(AComponent));
    if (AComponent is TZSequence) then
      UnregisterSequence(TZSequence(AComponent));
    if (AComponent = FZConnectionGroup) then
      FZConnectionGroup := nil;
  end;
end;

procedure TZGroupedConnection .SetConnectionGroup(Value: TZConnectionGroup);
begin
  if FZConnectionGroup<>nil then
    FZConnectionGroup.UnRegisterChanges(FZConnectionGroupLink);

  FZConnectionGroup := Value;
  if Value <> nil then
  begin
    FZConnectionGroup.RegisterChanges(FZConnectionGroupLink);
    FURL.UserName := FZConnectionGroup.User;
    FURL.Protocol := FZConnectionGroup.Protocol;
    FURL.Password := FZConnectionGroup.Password;
    FURL.HostName := FZConnectionGroup.HostName;
    FURL.Database := FZConnectionGroup.Database;
    FURL.LibLocation := FZConnectionGroup.LibraryLocation;
  end;
  //InternalZConnectionGroupChanged(Self, Value);
end;
(*
function TZGroupedConnection .getUser: string;
begin
  if FZConnectionGroup <> nil then
  begin
    FURL.UserName := FZConnectionGroup.User;
    Result := FURL.UserName;
  end
  else
    FURL.UserName := '';
end;

{
function TZGroupedConnection .getProtocol: string;
begin
  if FZConnectionGroup <> nil then
  begin
    FProtocol := FZConnectionGroup.Protocol;
    Result := FProtocol;
  end
  else
    FProtocol := '';
end;
}

function TZGroupedConnection .getPassword: string;
begin
  if FZConnectionGroup <> nil then
  begin
    FURL.Password := FZConnectionGroup.Password;
    Result := FURL.Password;
  end
  else
    FURL.Password := '';
end;

function TZGroupedConnection .getHostName: string;
begin
  if FZConnectionGroup <> nil then
  begin
    FURL.HostName := FZConnectionGroup.HostName;
    Result := FURL.HostName;
  end
  else
    FURL.HostName := '';
end;

function TZGroupedConnection .getDatabase: string;
begin
  if FZConnectionGroup <> nil then
  begin
    FURL.Database := FZConnectionGroup.Database;
    Result := FURL.Database;
  end
  else
    FURL.Database := '';
end;

function TZGroupedConnection.GetLibLocation: String;
begin
  if FZConnectionGroup <> nil then
  begin
    FURL.LibLocation := FZConnectionGroup.LibraryLocation;
    Result := FURL.LibLocation;
  end
  else
    FURL.LibLocation := '';
end;

{
function TZGroupedConnection .getCatalog: string;
begin
  if FZConnectionGroup <> nil then
  begin
    FCatalog := FZConnectionGroup.Catalog;
    Result := FCatalog;
  end
  else
    FCatalog := '';
end;
}
  *)
end.
