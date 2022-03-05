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

unit ZPlainProxyDriverInternalProxy;

interface

{$I ZPlain.inc}

{$IFNDEF ZEOS_DISABLE_PROXY}

uses
  Classes, ZPlainProxyDriverIntf, ZPlainProxyDriverSoapProxy;

function GetLastErrorStr: WideString; stdcall;
function GetInterface: IZDbcProxy; stdcall;

{$ENDIF ZEOS_DISABLE_PROXY}

implementation

{$IFNDEF ZEOS_DISABLE_PROXY}

uses SysUtils, {$IFNDEF NEXTGEN}ActiveX, ComObj,{$ENDIF} SOAPHTTPClient;

type
  TZDbcProxy = class(TInterfacedObject, IZDbcProxy{$IFNDEF NEXTGEN}, ISupportErrorInfo{$ENDIF})
    protected
      FService: IZeosProxy;
      FConnectionID: WideString;
      procedure CheckConnected;
      // this is necessary for safecall exception handling
      {$IFNDEF NEXTGEN}
      function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
      {$ENDIF}
    public
      // this is necessary for safecall exception handling
      {$IFNDEF NEXTGEN}
      function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
      {$ENDIF}

      procedure Connect(const UserName, Password, DbHost, DbName: WideString; var Properties: WideString; out DbInfo: WideString); {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      procedure Disconnect; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      procedure SetAutoCommit(const Value: LongBool); {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      procedure Commit; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      procedure Rollback; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function SetProperties(const Properties : WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function ExecuteStatement(const SQL, Parameters: WideString; const MaxRows: LongWord): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetTables(const Catalog, SchemaPattern, TableNamePattern, Types: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetSchemas: WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetCatalogs: WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetTableTypes: WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetColumns(const Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetTablePrivileges(const Catalog, SchemaPattern, TableNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetColumnPrivileges(const Catalog, Schema, Table, ColumnNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetPrimaryKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetImportedKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetExportedKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetCrossReference(const PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetIndexInfo(const Catalog, Schema, Table: WideString; const Unique, Approximate: LongBool):WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetSequences(const Catalog, SchemaPattern, SequenceNamePattern : WideString ): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetTriggers(const Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetProcedures(const Catalog, SchemaPattern, ProcedureNamePattern : WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetProcedureColumns(const Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
      function GetCharacterSets(): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}

      constructor Create;
      destructor Destroy; override;
  end;


var
  LastErrorStr: UnicodeString;

function GetLastErrorStr: WideString; stdcall;
begin
  Result := LastErrorStr;
end;

function GetInterface: IZDbcProxy; stdcall;
begin
  try
    result := TZDbcProxy.Create as IZDbcProxy;
  except
    on E: Exception do begin
      LastErrorStr := E.Message;
      result := nil;
    end;
  end;
end;

procedure TZDbcProxy.CheckConnected;
begin
  if not Assigned(FService) then
    raise Exception.Create('No connection has been established yet!');
end;

{$IFNDEF NEXTGEN}
function TZDbcProxy.InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
begin
  if GetInterfaceEntry(iid) <> nil then
    Result := S_OK else
    Result := S_FALSE;
end;

function TZDbcProxy.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
begin
  Result := HandleSafeCallException(ExceptObject, ExceptAddr, StringToGUID('{374CAA55-95CD-44FE-8FF3-F90BF8D1DF8C}'), 'libzdbcproxy.dll', '');
end;
{$ENDIF}

constructor TZDbcProxy.Create;
begin
  FService := nil;
end;

destructor TZDbcProxy.Destroy;
begin
 FService := nil;
end;

procedure TZDbcProxy.Connect(const UserName, Password, DbHost, DbName: WideString; var Properties: WideString; out DbInfo: WideString); {$IFNDEF NEXTGEN}safecall;{$ENDIF}
var
  Url: String;
  LocalProperties: String;
  FRIO: THTTPRIO;
  MyInProperties: UnicodeString;
  MyOutProperties: UnicodeString;
  MyDbInfo: UnicodeString;
begin
// Url := 'http://' + DbHost + ':8000/services/IZeosProxy';
// //FService := wst_CreateInstance_IZeosProxy('SOAP:', 'HTTP:', 'http://127.0.0.1:8000/services/IZeosProxy');
// FService := wst_CreateInstance_IZeosProxy('SOAP:', 'HTTP:', Url);

// if using a reverse proxy, this seems to work well:
// Url := 'https://' + DbHost + '/services/IZeosProxy';
// //FService := wst_CreateInstance_IZeosProxy('SOAP:', 'HTTP:', 'http://127.0.0.1:8000/services/IZeosProxy');
// FService := wst_CreateInstance_IZeosProxy('SOAP:', 'HTTP:', Url);

  //FService := GetIZeosProxy(false, 'http://zeos-test:8000/services/IZeosProxy');
//  FService := GetIZeosProxy(false, 'http://' + DbHost + ':8000/services/IZeosProxy');
  FRIO := THTTPRIO.Create(nil);
  // if the certificate should not be checked, then the option soIgnoreInvalidCerts
  // from the SOAPHTTPTrans unit can be set or the following line can be removed.
  // [soIgnoreInvalidCerts, soAutoCheckAccessPointViaUDDI] is the default setting.
  FRIO.HTTPWebNode.InvokeOptions := [];
  FService := GetIZeosProxy(false, 'http://' + DbHost + ':8000/services/IZeosProxy', FRIO);
  if Assigned(FService) then begin
    MyInProperties := Properties;
    FConnectionID := FService.Connect(UserName, Password, DbName, MyInProperties, MyOutProperties, MyDbInfo);
    Properties := MyOutProperties;
    DbInfo := MyDbInfo;
  end else begin
    FreeAndNil(FRIO);
  end;
end;

procedure TZDbcProxy.Disconnect; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
 CheckConnected;
 try
   FService.Disconnect(FConnectionID);
 finally
   FConnectionID := '';
 end;
end;

procedure TZDbcProxy.SetAutoCommit(const Value: LongBool); {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  FService.SetAutoCommit(FConnectionID, Value);
end;

procedure TZDbcProxy.Commit; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  FService.Commit(FConnectionID);
end;

procedure TZDbcProxy.Rollback; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  FService.Rollback(FConnectionID);
end;

function TZDbcProxy.SetProperties(const Properties : WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.SetProperties(FConnectionID, Properties);
end;

function TZDbcProxy.ExecuteStatement(const SQL, Parameters: WideString; const MaxRows: LongWord): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.ExecuteStatement(FConnectionID, SQL, Parameters, MaxRows);
end;

function TZDbcProxy.GetTables(const Catalog, SchemaPattern, TableNamePattern, Types: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetTables(FConnectionID, Catalog, SchemaPattern, TableNamePattern, Types);
end;

function TZDbcProxy.GetSchemas: WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetSchemas(FConnectionID);
end;

function TZDbcProxy.GetCatalogs: WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetCatalogs(FConnectionID);
end;

function TZDbcProxy.GetTableTypes: WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetTableTypes(FConnectionID);
end;

function TZDbcProxy.GetColumns(const Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetColumns(FConnectionID, Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);
end;

function TZDbcProxy.GetTablePrivileges(const Catalog, SchemaPattern, TableNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetTablePrivileges(FConnectionID, Catalog, SchemaPattern, TableNamePattern);
end;

function TZDbcProxy.GetColumnPrivileges(const Catalog, Schema, Table, ColumnNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetColumnPrivileges(FConnectionID, Catalog, Schema, Table, ColumnNamePattern);
end;

function TZDbcProxy.GetPrimaryKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetPrimaryKeys(FConnectionID, Catalog, Schema, Table);
end;

function TZDbcProxy.GetImportedKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetImportedKeys(FConnectionID, Catalog, Schema, Table);
end;

function TZDbcProxy.GetExportedKeys(const Catalog, Schema, Table: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetExportedKeys(FConnectionID, Catalog, Schema, Table);
end;

function TZDbcProxy.GetCrossReference(const PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetCrossReference(FConnectionID, PrimaryCatalog, PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable);
end;

function TZDbcProxy.GetIndexInfo(const Catalog, Schema, Table: WideString; const Unique, Approximate: LongBool):WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetIndexInfo(FConnectionID, Catalog, Schema, Table, Unique, Approximate);
end;

function TZDbcProxy.GetSequences(const Catalog, SchemaPattern, SequenceNamePattern : WideString ): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetSequences(FConnectionID, Catalog, SchemaPattern, SequenceNamePattern);
end;

function TZDbcProxy.GetTriggers(const Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetTriggers(FConnectionID, Catalog, SchemaPattern, TableNamePattern, TriggerNamePattern);
end;

function TZDbcProxy.GetProcedures(const Catalog, SchemaPattern, ProcedureNamePattern : WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetProcedures(FConnectionID, Catalog, SchemaPattern, ProcedureNamePattern);
end;

function TZDbcProxy.GetProcedureColumns(const Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern: WideString): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
  CheckConnected;
  Result := FService.GetProcedureColumns(FConnectionID, Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);
end;

function TZDbcProxy.GetCharacterSets(): WideString; {$IFNDEF NEXTGEN}safecall;{$ENDIF}
begin
 CheckConnected;
 Result := FService.GetCharacterSets(FConnectionID);
end;

initialization
  LastErrorStr := 'No Error happened yet!'

{$ENDIF ZEOS_DISABLE_PROXY}

end.
