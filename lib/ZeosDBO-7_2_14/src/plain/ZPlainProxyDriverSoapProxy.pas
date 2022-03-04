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

// ************************************************************************ //
// Die in dieser Datei deklarierten Typen wurden aus Daten der unten
// beschriebenen WSDL-Datei generiert:

// WSDL     : http://localhost:8000/WSDL/zeosproxy
//  >Import : http://localhost:8000/WSDL/zeosproxy>0
// Codierung : utf-8
// Version: 1.0
// (12.01.2020 21:31:48 - - $Rev: 69934 $)
// ************************************************************************ //

unit ZPlainProxyDriverSoapProxy;

interface

uses Soap.InvokeRegistry, Soap.SOAPHTTPClient, System.Types, Soap.XSBuiltIns;

type

  // ************************************************************************ //
  // Die folgenden Typen, auf die im WSDL-Dokument Bezug genommen wird, sind in dieser Datei
  // nicht repräsentiert. Sie sind entweder Aliase[@] anderer repräsentierter Typen oder auf sie wurde Bezug genommen,
  // aber sie sind in diesem Dokument nicht[!] deklariert. Die Typen aus letzterer Kategorie
  // sind in der Regel vordefinierten/bekannten XML- oder Embarcadero-Typen zugeordnet; sie könnten aber auf 
  // ein inkorrektes WSDL-Dokument hinweisen, das einen Schematyp nicht deklariert oder importiert hat.
  // ************************************************************************ //
  // !:unsignedInt     - "http://www.w3.org/2001/XMLSchema"[]
  // !:boolean         - "http://www.w3.org/2001/XMLSchema"[]
  // !:UnicodeString   - "http://www.w3.org/2001/XMLSchema"[Lit][]


  // ************************************************************************ //
  // Namespace : zproxy
  // soapAction: zproxy/IZeosProxy%operationName%
  // Transport : http://schemas.xmlsoap.org/soap/http
  // Stil     : rpc
  // Verwenden von       : literal
  // Bindung   : IZeosProxyBinding
  // Service   : IZeosProxy
  // Port      : IZeosProxyPort
  // URL       : 0.0.0.0/services/IZeosProxy
  // ************************************************************************ //
  IZeosProxy = interface(IInvokable)
  ['{269AF2BC-9AAB-FBA4-61C1-37129CFC7BFC}']

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabemeldung besteht aus mehreren Parts
    function  Connect(const UserName: UnicodeString; const Password: UnicodeString; const DbName: UnicodeString; const InProperties: UnicodeString; out OutProperties: UnicodeString; out DbInfo: UnicodeString
                      ): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    procedure Disconnect(const ConnectionID: UnicodeString); stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    procedure SetAutoCommit(const ConnectionID: UnicodeString; const Value: Boolean); stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    procedure Commit(const ConnectionID: UnicodeString); stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    procedure Rollback(const ConnectionID: UnicodeString); stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  SetProperties(const ConnectionID: UnicodeString; const Properties: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  ExecuteStatement(const ConnectionID: UnicodeString; const SQL: UnicodeString; const Parameters: UnicodeString; const MaxRows: Cardinal): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetTables(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const SchemaPattern: UnicodeString; const TableNamePattern: UnicodeString; const Types: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    //     - Ausgabe-Part verweist auf kein Element
    function  GetSchemas(const ConnectionID: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    //     - Ausgabe-Part verweist auf kein Element
    function  GetCatalogs(const ConnectionID: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    //     - Ausgabe-Part verweist auf kein Element
    function  GetTableTypes(const ConnectionID: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetColumns(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const SchemaPattern: UnicodeString; const TableNamePattern: UnicodeString; const ColumnNamePattern: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetTablePrivileges(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const SchemaPattern: UnicodeString; const TableNamePattern: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetColumnPrivileges(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const Schema: UnicodeString; const Table: UnicodeString; const ColumnNamePattern: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetPrimaryKeys(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const Schema: UnicodeString; const Table: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetImportedKeys(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const Schema: UnicodeString; const Table: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetExportedKeys(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const Schema: UnicodeString; const Table: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetCrossReference(const ConnectionID: UnicodeString; const PrimaryCatalog: UnicodeString; const PrimarySchema: UnicodeString; const PrimaryTable: UnicodeString; const ForeignCatalog: UnicodeString; const ForeignSchema: UnicodeString; 
                                const ForeignTable: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetIndexInfo(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const Schema: UnicodeString; const Table: UnicodeString; const Unique: Boolean; const Approximate: Boolean
                           ): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetSequences(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const SchemaPattern: UnicodeString; const SequenceNamePattern: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetTriggers(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const SchemaPattern: UnicodeString; const TableNamePattern: UnicodeString; const TriggerNamePattern: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetProcedures(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const SchemaPattern: UnicodeString; const ProcedureNamePattern: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabemeldung besteht aus mehreren Parts
    //     - Ausgabe-Part verweist auf kein Element
    function  GetProcedureColumns(const ConnectionID: UnicodeString; const Catalog: UnicodeString; const SchemaPattern: UnicodeString; const ProcedureNamePattern: UnicodeString; const ColumnNamePattern: UnicodeString): UnicodeString; stdcall;

    // Entpacken nicht möglich: 
    //     - Eingabe-Part verweist auf kein Element
    //     - Ausgabe-Part verweist auf kein Element
    function  GetCharacterSets(const ConnectionID: UnicodeString): UnicodeString; stdcall;
  end;

function GetIZeosProxy(UseWSDL: Boolean=System.False; Addr: string=''; HTTPRIO: THTTPRIO = nil): IZeosProxy;


implementation
  uses System.SysUtils;

function GetIZeosProxy(UseWSDL: Boolean; Addr: string; HTTPRIO: THTTPRIO): IZeosProxy;
const
  defWSDL = 'http://localhost:8000/WSDL/zeosproxy';
  defURL  = '0.0.0.0/services/IZeosProxy';
  defSvc  = 'IZeosProxy';
  defPrt  = 'IZeosProxyPort';
var
  RIO: THTTPRIO;
begin
  Result := nil;
  if (Addr = '') then
  begin
    if UseWSDL then
      Addr := defWSDL
    else
      Addr := defURL;
  end;
  if HTTPRIO = nil then
    RIO := THTTPRIO.Create(nil)
  else
    RIO := HTTPRIO;
  try
    Result := (RIO as IZeosProxy);
    if UseWSDL then
    begin
      RIO.WSDLLocation := Addr;
      RIO.Service := defSvc;
      RIO.Port := defPrt;
    end else
      RIO.URL := Addr;
  finally
    if (Result = nil) and (HTTPRIO = nil) then
      RIO.Free;
  end;
end;


initialization
  { IZeosProxy }
  InvRegistry.RegisterInterface(TypeInfo(IZeosProxy), 'zproxy', 'utf-8');
  InvRegistry.RegisterDefaultSOAPAction(TypeInfo(IZeosProxy), 'zproxy/IZeosProxy%operationName%');
  InvRegistry.RegisterInvokeOptions(TypeInfo(IZeosProxy), ioLiteral);

end.