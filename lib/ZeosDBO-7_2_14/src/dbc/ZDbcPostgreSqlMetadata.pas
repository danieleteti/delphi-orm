{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
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

unit ZDbcPostgreSqlMetadata;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcMetadata, ZCompatibility, ZDbcPostgreSqlUtils,
  ZSelectSchema, ZPlainPostgreSqlDriver;

type
  {** Implements a PostgreSQL Case Sensitive/Unsensitive identifier convertor. }
  TZPostgreSQLIdentifierConvertor = class (TZDefaultIdentifierConvertor)
  protected
    function IsSpecialCase(const Value: string): Boolean; override;
  public
    function IsQuoted(const Value: string): Boolean; override;
    function Quote(const Value: string): string; override;
    function ExtractQuote(const Value: string): string; override; 
  end; 
 
  {**
    Database information interface for PostgreSQL. Adds some PostgreSQL-specific
     methods to IZDatabaseInfo.
  } // technobot 2008-06-27
  IZPostgreSQLDatabaseInfo = interface(IZDatabaseInfo)
    ['{7D48BBAA-FAE2-48EA-8B9E-663CCA5690EC}']
    // database and driver info:
    function HasMinimumServerVersion(MajorVersion: Integer;
      MinorVersion: Integer): Boolean;
  end;
  IZPostgreDBInfo = IZPostgreSQLDatabaseInfo; // shorthand alias

  // technobot 2008-06-27 - methods moved as is from TZPostgreSQLDatabaseMetadata:
  {** Implements PostgreSQL Database Information. }
  TZPostgreSQLDatabaseInfo = class(TZAbstractDatabaseInfo, IZPostgreSQLDatabaseInfo)
  protected
    function GetMaxIndexKeys: Integer;
    function GetMaxNameLength: Integer;
//    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
//      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;
  public
    constructor Create(const Metadata: TZAbstractDatabaseMetadata);
    destructor Destroy; override;

    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
//    function GetDriverVersion: string; override; -> Same as parent
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
    function GetServerVersion: string; override;
    function HasMinimumServerVersion(MajorVersion: Integer;
      MinorVersion: Integer): Boolean; // was TZPostgreSQLDatabaseMetadata.HaveMinimumServerVersion

    // capabilities (what it can/cannot do):
//    function AllProceduresAreCallable: Boolean; override; -> Not implemented
//    function AllTablesAreSelectable: Boolean; override; -> Not implemented
    function SupportsMixedCaseIdentifiers: Boolean; override;
    function SupportsMixedCaseQuotedIdentifiers: Boolean; override;
//    function SupportsAlterTableWithAddColumn: Boolean; override; -> Not implemented
//    function SupportsAlterTableWithDropColumn: Boolean; override; -> Not implemented
//    function SupportsColumnAliasing: Boolean; override; -> Not implemented
//    function SupportsConvert: Boolean; override; -> Not implemented
//    function SupportsConvertForTypes(FromType: TZSQLType; ToType: TZSQLType):
//      Boolean; override; -> Not implemented
//    function SupportsTableCorrelationNames: Boolean; override; -> Not implemented
//    function SupportsDifferentTableCorrelationNames: Boolean; override; -> Not implemented
    function SupportsExpressionsInOrderBy: Boolean; override;
    function SupportsOrderByUnrelated: Boolean; override;
    function SupportsGroupBy: Boolean; override;
    function SupportsGroupByUnrelated: Boolean; override;
    function SupportsGroupByBeyondSelect: Boolean; override;
//    function SupportsLikeEscapeClause: Boolean; override; -> Not implemented
//    function SupportsMultipleResultSets: Boolean; override; -> Not implemented
//    function SupportsMultipleTransactions: Boolean; override; -> Not implemented
//    function SupportsNonNullableColumns: Boolean; override; -> Not implemented
//    function SupportsMinimumSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsCoreSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsExtendedSQLGrammar: Boolean; override; -> Not implemented
//    function SupportsANSI92EntryLevelSQL: Boolean; override; -> Not implemented
//    function SupportsANSI92IntermediateSQL: Boolean; override; -> Not implemented
//    function SupportsANSI92FullSQL: Boolean; override; -> Not implemented
    function SupportsIntegrityEnhancementFacility: Boolean; override;
//    function SupportsOuterJoins: Boolean; override; -> Not implemented
//    function SupportsFullOuterJoins: Boolean; override; -> Not implemented
//    function SupportsLimitedOuterJoins: Boolean; override; -> Not implemented
    function SupportsSchemasInDataManipulation: Boolean; override;
    function SupportsSchemasInProcedureCalls: Boolean; override;
    function SupportsSchemasInTableDefinitions: Boolean; override;
    function SupportsSchemasInIndexDefinitions: Boolean; override;
    function SupportsSchemasInPrivilegeDefinitions: Boolean; override;
    function SupportsCatalogsInDataManipulation: Boolean; override;
    function SupportsCatalogsInProcedureCalls: Boolean; override;
    function SupportsCatalogsInTableDefinitions: Boolean; override;
    function SupportsCatalogsInIndexDefinitions: Boolean; override;
    function SupportsCatalogsInPrivilegeDefinitions: Boolean; override;
    function SupportsPositionedDelete: Boolean; override;
    function SupportsPositionedUpdate: Boolean; override;
    function SupportsSelectForUpdate: Boolean; override;
    function SupportsStoredProcedures: Boolean; override;
    function SupportsSubqueriesInComparisons: Boolean; override;
    function SupportsSubqueriesInExists: Boolean; override;
    function SupportsSubqueriesInIns: Boolean; override;
    function SupportsSubqueriesInQuantifieds: Boolean; override;
    function SupportsCorrelatedSubqueries: Boolean; override;
    function SupportsUnion: Boolean; override;
    function SupportsUnionAll: Boolean; override;
    function SupportsOpenCursorsAcrossCommit: Boolean; override;
    function SupportsOpenCursorsAcrossRollback: Boolean; override;
    function SupportsOpenStatementsAcrossCommit: Boolean; override;
    function SupportsOpenStatementsAcrossRollback: Boolean; override;
    function SupportsTransactions: Boolean; override;
    function SupportsTransactionIsolationLevel(const Level: TZTransactIsolationLevel):
      Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
    function SupportsResultSetType(const _Type: TZResultSetType): Boolean; override;
    function SupportsResultSetConcurrency(const _Type: TZResultSetType;
      const Concurrency: TZResultSetConcurrency): Boolean; override;
//    function SupportsBatchUpdates: Boolean; override; -> Not implemented

    // maxima:
    function GetMaxBinaryLiteralLength: Integer; override;
    function GetMaxCharLiteralLength: Integer; override;
    function GetMaxColumnNameLength: Integer; override;
    function GetMaxColumnsInGroupBy: Integer; override;
    function GetMaxColumnsInIndex: Integer; override;
    function GetMaxColumnsInOrderBy: Integer; override;
    function GetMaxColumnsInSelect: Integer; override;
    function GetMaxColumnsInTable: Integer; override;
    function GetMaxConnections: Integer; override;
    function GetMaxCursorNameLength: Integer; override;
    function GetMaxIndexLength: Integer; override;
    function GetMaxSchemaNameLength: Integer; override;
    function GetMaxProcedureNameLength: Integer; override;
    function GetMaxCatalogNameLength: Integer; override;
    function GetMaxRowSize: Integer; override;
    function GetMaxStatementLength: Integer; override;
    function GetMaxStatements: Integer; override;
    function GetMaxTableNameLength: Integer; override;
    function GetMaxTablesInSelect: Integer; override;
    function GetMaxUserNameLength: Integer; override;

    // policies (how are various data and operations handled):
//    function IsReadOnly: Boolean; override; -> Not implemented
//    function IsCatalogAtStart: Boolean; override; -> Not implemented
    function DoesMaxRowSizeIncludeBlobs: Boolean; override;
//    function NullsAreSortedHigh: Boolean; override; -> Not implemented
//    function NullsAreSortedLow: Boolean; override; -> Not implemented
//    function NullsAreSortedAtStart: Boolean; override; -> Not implemented
//    function NullsAreSortedAtEnd: Boolean; override; -> Not implemented
//    function NullPlusNonNullIsNull: Boolean; override; -> Not implemented
//    function UsesLocalFiles: Boolean; override; -> Not implemented
    function UsesLocalFilePerTable: Boolean; override;
    function StoresUpperCaseIdentifiers: Boolean; override;
    function StoresLowerCaseIdentifiers: Boolean; override;
    function StoresMixedCaseIdentifiers: Boolean; override;
    function StoresUpperCaseQuotedIdentifiers: Boolean; override;
    function StoresLowerCaseQuotedIdentifiers: Boolean; override;
    function StoresMixedCaseQuotedIdentifiers: Boolean; override;
    function GetDefaultTransactionIsolation: TZTransactIsolationLevel; override;
    function DataDefinitionCausesTransactionCommit: Boolean; override;
    function DataDefinitionIgnoredInTransactions: Boolean; override;

    // interface details (terms, keywords, etc):
    function GetSchemaTerm: string; override;
    function GetProcedureTerm: string; override;
    function GetCatalogTerm: string; override;
    function GetCatalogSeparator: string; override;
    function GetSQLKeywords: string; override;
    function GetNumericFunctions: string; override;
    function GetStringFunctions: string; override;
    function GetSystemFunctions: string; override;
    function GetTimeDateFunctions: string; override;
    function GetSearchStringEscape: string; override;
    function GetExtraNameCharacters: string; override;
  end;

  IZPGDatabaseMetadata = Interface(IZDatabaseMetadata)
    ['{24E96886-F7E3-45F6-86C7-014A3376889F}']
    function GetColumnsByTableOID(Value: OID): IZResultSet;
  End;

  TZPGTableOID = record
    OID: Oid;
    ColumnRS: IZResultSet;
  end;

  {** Implements PostgreSQL Database Metadata. }
  TZPostgreSQLDatabaseMetadata = class(TZAbstractDatabaseMetadata, IZPGDatabaseMetadata)
  private
    fZPGTableOIDArray: array of TZPGTableOID;
    function GetRuleType(const Rule: String): TZImportedKey;
    function GetColumnsByTableOID(Value: OID): IZResultSet;
    function InternalUncachedGetColumns(const Catalog, SchemaPattern,
      TableNamePattern, ColumnNamePattern, TableOID: string): IZResultSet;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-27

    // (technobot) should any of these be moved to TZPostgreSQLDatabaseInfo?:
    function GetPostgreSQLType(Oid: OID): string;
    function GetSQLTypeByOid(Oid: OID): TZSQLType;
    function GetSQLTypeByName(const TypeName: string): TZSQLType;
    function TableTypeSQLExpression(const TableType: string; UseSchemas: Boolean):
      string;
    procedure ParseACLArray(List: TStrings; const AclString: string);
    function GetPrivilegeName(Permission: char): string;
    // (technobot) end of questioned section

    function EscapeString(const S: string): string; override;
    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
    function UncachedGetSchemas: IZResultSet; override;
    function UncachedGetCatalogs: IZResultSet; override;
    function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; override;
    function UncachedGetColumnPrivileges(const Catalog: string; const Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetPrimaryKeys(const {%H-}Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const {%H-}Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; {%H-}Approximate: Boolean): IZResultSet; override;
     function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): IZResultSet; override;
    function UncachedGetProcedures(const {%H-}Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; override;
    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
    function UncachedGetCharacterSets: IZResultSet; override; //EgonHugeist

  public
    destructor Destroy; override;
    function GetIdentifierConvertor: IZIdentifierConvertor; override;
    procedure ClearCache; override;
 end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses
  //Math,
  ZFastCode, ZMessages, ZSysUtils, ZDbcPostgreSql;

{ TZPostgreSQLDatabaseInfo }

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
}
constructor TZPostgreSQLDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata);
begin
  inherited;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLDatabaseInfo.Destroy;
begin
  inherited;
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZPostgreSQLDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'PostgreSQL';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZPostgreSQLDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZPostgreSQLDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for PostgreSQL';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZPostgreSQLDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZPostgreSQLDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Returns the server version
  @return the server version string
}
function TZPostgreSQLDatabaseInfo.GetServerVersion: string;
begin
  with Metadata.GetConnection as IZPostgreSQLConnection do
    Result := Format('%d.%d', [GetServerMajorVersion, GetServerMinorVersion]);
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZPostgreSQLDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetSQLKeywords: string;
begin
  Result := 'abort,absolute,access,action,add,admin,after,aggregate,all,also,'+
            'alter,always,analyse,analyze,and,any,array,asc,assertion,assignment,'+
            'asymmetric,at,authorization,'+
            'backward,before,begin,between,bigint,binary,bit,boolean,both,'+
            'cache,called,cascade,cascaded,case,cast,catalog,chain,character,'+
            'characteristics,check,checkpoint,class,close,cluster,coalesce,'+
            'collate,column,comment,commit,committed,concurrently,configuration,'+
            'connect,connection,constraint,constraints,content,continue,'+
            'conversion,convert,copy,cost,createdb,createrole,createuser,cross,'+
            'csv,current,current_catalog,current_date,current_role,current_schema,'+
            'current_time,current_timestamp,current_user,cursor,cycle,'+
            'data,database,day,deallocate,dec,decimal,declare,default,defaults,'+
            'deferrable,deferred,definer,delimiter,delimiters,desc,dictionary,'+
            'disable,discard,distinct,do,document,domain,double,'+
            'each,else,enable,encoding,encrypted,end,end-exec,enum,escape,except,'+
            'excluding,exclusive,exec,execute,exists,explain,external,extract,'+
            'false,family,fetch,first,float,following,for,force,foreign,forward,'+
            'freeze,full,function,'+
            'global,grant,granted,greatest,'+
            'handler,header,hold,hour,'+
            'identity,if,ilike,immediate,immutable,implicit,in,including,'+
            'increment,indexes,inherit,inherits,initially,inner,inout,input,'+
            'insensitive,instead,int,intersect,interval,invoker,isnull,isolation,'+
            'join,'+
            'lancompiler,language,large,last,lc_collate,lc_ctype,leading,least,'+
            'left,level,like,limit,listen,load,local,localtime,localtimestamp,'+
            'location,lock,login,'+
            'mapping,match,maxvalue,minute,minvalue,mode,month,move,'+
            'name,names,national,natural,nchar,new,next,no,nocreatedb,nocreaterole,'+
            'nocreateuser,noinherit,nologin,none,nosuperuser,not,nothing,notify,'+
            'notnull,nowait,nullif,nulls,numeric,'+
            'object,of,off,offset,oids,old,only,operator,option,options,or,out,'+
            'outer,over,overlaps,overlay,owned,owner,'+
            'parser,partial,partition,password,placing,plans,position,preceding,'+
            'precision,prepare,prepared,preserve,prior,privileges,procedural,'+
            'procedure,'+
            'quote,'+
            'range,read,real,reassign,recheck,recursive,references,reindex,'+
            'relative,release,rename,repeatable,replace,replica,reset,restart,'+
            'restrict,return,returning,returns,revoke,right,role,rollback,row,'+
            'rows,rule,'+
            'savepoint,schema,scroll,search,second,security,sequence,serializable,'+
            'server,session,session_user,setof,share,show,similar,simple,smallint,'+
            'some,stable,standalone,start,statement,statistics,stdin,stdout,'+
            'storage,strict,strip,substring,superuser,symmetric,sysid,system,'+
            'tablespace,temp,template,temporary,text,then,time,timestamp,to,'+
            'trailing,transaction,treat,trigger,trim,true,truncate,trusted,type,'+
            'unbounded,uncommitted,unencrypted,union,unique,unknown,unlisten,'+
            'until,user,using,'+
            'vacuum,valid,validator,value,variadic,varying,verbose,version,view,'+
            'volatile,'+
            'when,whitespace,window,with,without,work,wrapper,write,'+
            'xml,xmlattributes,xmlconcat,xmlelement,xmlforest,xmlparse,xmlpi,'+
            'xmlroot,xmlserialize,'+
            'year,yes,'+
            'zone';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetNumericFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetStringFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetSystemFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZPostgreSQLDatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := '';
end;

{**
  Gets the string that can be used to escape wildcard characters.
  This is the string that can be used to escape '_' or '%' in
  the string pattern style catalog search parameters.

  <P>The '_' character represents any single character.
  <P>The '%' character represents any sequence of zero or
  more characters.

  @return the string used to escape wildcard characters
}
function TZPostgreSQLDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '\';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZPostgreSQLDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := True;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := HasMinimumServerVersion(6, 4);
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := HasMinimumServerVersion(6, 4);
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := HasMinimumServerVersion(6, 4);
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZPostgreSQLDatabaseInfo.GetSchemaTerm: string;
begin
  Result := 'schema';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZPostgreSQLDatabaseInfo.GetProcedureTerm: string;
begin
  Result := 'function';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZPostgreSQLDatabaseInfo.GetCatalogTerm: string;
begin
  Result := 'database';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZPostgreSQLDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '.';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := HasMinimumServerVersion(7, 3);
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := False;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := False;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := HasMinimumServerVersion(6, 5);
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := HasMinimumServerVersion(7, 1);
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := HasMinimumServerVersion(7, 1);
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZPostgreSQLDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZPostgreSQLDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZPostgreSQLDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZPostgreSQLDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := True;
end;

//----------------------------------------------------------------------
// The following group of methods exposes various limitations
// based on the target database with the current driver.
// Unless otherwise specified, a result of zero means there is no
// limit, or the limit is not known.

{**
  How many hex characters can you have in an inline binary literal?
  @return max binary literal length in hex characters;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := GetMaxIndexKeys;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 1600;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 8192;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxRowSize: Integer;
begin
  if HasMinimumServerVersion(7, 1) then
    Result := 1073741824
  else Result := 8192;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := True;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxStatementLength: Integer;
begin
  if HasMinimumServerVersion(7, 0) then
    Result := 0
  else Result := 16348
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 1;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZPostgreSQLDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := GetMaxNameLength;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZPostgreSQLDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZPostgreSQLDatabaseInfo.SupportsTransactionIsolationLevel(
  const Level: TZTransactIsolationLevel): Boolean;
begin
  Result := (Level = tiSerializable) or (Level = tiReadCommitted);
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := False;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := False;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsResultSetType(
  const _Type: TZResultSetType): Boolean;
begin
  Result := _Type = rtScrollInsensitive;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZPostgreSQLDatabaseInfo.SupportsResultSetConcurrency(
  const _Type: TZResultSetType; const Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := (_Type = rtScrollInsensitive) and (Concurrency = rcReadOnly);
end;

//----------------------------------------------------------------------
// Additional functions.

function TZPostgreSQLDatabaseInfo.HasMinimumServerVersion(
  MajorVersion: Integer; MinorVersion: Integer): Boolean;
var
  PostgreSQLConnection: IZPostgreSQLConnection;
begin
  PostgreSQLConnection := Metadata.GetConnection as IZPostgreSQLConnection;
  Result := (MajorVersion < PostgreSQLConnection.GetServerMajorVersion)
    or ((MajorVersion = PostgreSQLConnection.GetServerMajorVersion)
    and (MinorVersion <= PostgreSQLConnection.GetServerMinorVersion));
end;

function TZPostgreSQLDatabaseInfo.GetMaxIndexKeys: Integer;
var
  SQL, From: string;
begin
  if HasMinimumServerVersion(7, 3) then
  begin
    From := ' pg_catalog.pg_namespace n, pg_catalog.pg_type t1,'
      + ' pg_catalog.pg_type t2 WHERE t1.typnamespace=n.oid'
      + ' AND n.nspname=''pg_catalog'' AND ';
  end else
    From := ' pg_type t1, pg_type t2 WHERE ';
  SQL := ' SELECT t1.typlen/t2.typlen FROM ' + From
    + ' t1.typelem=t2.oid AND t1.typname=''oidvector'' ';

  with Metadata.GetConnection.CreateStatement.ExecuteQuery(SQL) do
  begin
    if not Next then
      raise Exception.Create(SUnknownError); //CHANGE IT!
    Result := GetInt(FirstDbcIndex);
    Close;
  end;
end;

function TZPostgreSQLDatabaseInfo.GetMaxNameLength: Integer;
var
  SQL: string;
begin
  if HasMinimumServerVersion(7, 3) then
  begin
    SQL := ' SELECT t.typlen FROM pg_catalog.pg_type t,'
      + ' pg_catalog.pg_namespace n WHERE t.typnamespace=n.oid'
      + ' AND t.typname=''name'' AND n.nspname=''pg_catalog'' ';
  end else
    SQL := ' SELECT typlen FROM pg_type WHERE typname=''name'' ';

  with Metadata.GetConnection.CreateStatement.ExecuteQuery(SQL) do
  begin
    if not Next then
      raise Exception.Create(SUnknownError); //CHANGE IT!
    Result := GetIntByName('typlen');
    Close;
  end;
end;


{ TZPostgreSQLDatabaseMetadata }


{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLDatabaseMetadata.Destroy;
begin
  inherited Destroy;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
procedure TZPostgreSQLDatabaseMetadata.ClearCache;
begin
  inherited;
  SetLength(fZPGTableOIDArray, 0);
end;

function TZPostgreSQLDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZPostgreSQLDatabaseInfo.Create(Self);
end;

{**
  @param S a string.
  @return escaped string
}
function TZPostgreSQLDatabaseMetadata.EscapeString(const S: string): string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(Result) downto 1 do
    if (Result[I] = '''') or (Result[I] = '\') then
      Insert('\', Result, I);
  Result := '''' + Result + '''';
  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(8, 1) then
    Result := 'E' + Result;
end;

function TZPostgreSQLDatabaseMetadata.GetRuleType(const Rule: String): TZImportedKey;
begin
  if Rule = 'RESTRICT' then
    Result := ikRestrict
  else if Rule = 'NO ACTION' then
    Result := ikNoAction
  else if Rule = 'CASCADE' then
    Result := ikCascade
  else if Rule = 'SET DEFAULT' then
    Result := ikSetDefault
  else if Rule = 'SET NULL' then
    Result := ikSetNull
  else
    Result := ikNotDeferrable; //impossible!
end;

{**
  Gets a description of the stored procedures available in a
  catalog.

  <P>Only procedure descriptions matching the schema and
  procedure name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM, and PROCEDURE_NAME.

  <P>Each procedure description has the the following columns:
   <OL>
 	<LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
 	<LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
 	<LI><B>PROCEDURE_NAME</B> String => procedure name
   <LI> reserved for future use
   <LI> reserved for future use
   <LI> reserved for future use
 	<LI><B>REMARKS</B> String => explanatory comment on the procedure
 	<LI><B>PROCEDURE_TYPE</B> short => kind of procedure:
       <UL>
       <LI> procedureResultUnknown - May return a result
       <LI> procedureNoResult - Does not return a result
       <LI> procedureReturnsResult - Returns a result
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @return <code>ResultSet</code> - each row is a procedure description
  @see #getSearchStringEscape
}
function TZPostgreSQLDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  SQL, ProcedureCondition, SchemaCondition: string;
begin
  SchemaCondition := ConstructNameCondition(SchemaPattern,'n.nspname');
  ProcedureCondition := ConstructNameCondition(ProcedureNamePattern,'p.proname');
  with (GetDatabaseInfo as IZPostgreDBInfo) do begin
    if HasMinimumServerVersion(7, 3) then begin
      SQL := 'SELECT NULL AS PROCEDURE_CAT, n.nspname AS PROCEDURE_SCHEM,'
        + ' p.proname AS PROCEDURE_NAME, NULL AS RESERVED1, NULL AS RESERVED2,'
        + ' NULL AS RESERVED3, d.description AS REMARKS, ';
      if HasMinimumServerVersion(11, 0) then
        SQL := SQL+ 'case when p.prokind = ''p'' then '+ZFastCode.IntToStr(Ord(ProcedureNoResult))+ ' else '+ZFastCode.IntToStr(Ord(ProcedureReturnsResult))+ ' end'
      else
        SQL := SQL+ ZFastCode.IntToStr(Ord(ProcedureReturnsResult));
      SQL := SQL + ' AS PROCEDURE_TYPE '
        + ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_proc p  '
        + ' LEFT JOIN pg_catalog.pg_description d ON (p.oid=d.objoid) '
        + ' LEFT JOIN pg_catalog.pg_class c ON (d.classoid=c.oid AND'
        + ' c.relname=''pg_proc'') LEFT JOIN pg_catalog.pg_namespace pn ON'
        + ' (c.relnamespace=pn.oid AND pn.nspname=''pg_catalog'') '
        + ' WHERE p.pronamespace=n.oid';
      if SchemaCondition <> '' then
        SQL := SQL + ' AND ' + Schemacondition;
      if ProcedureCondition <> '' then
        SQL := SQL + ' AND ' + ProcedureCondition;
      SQL := SQL + ' ORDER BY PROCEDURE_SCHEM, PROCEDURE_NAME';
    end else begin
      SQL := 'SELECT NULL AS PROCEDURE_CAT, NULL AS PROCEDURE_SCHEM,'
        + ' p.proname AS PROCEDURE_NAME, NULL AS RESERVED1, NULL AS RESERVED2,'
        + ' NULL AS RESERVED3, NULL AS REMARKS, '
        + ZFastCode.IntToStr(Ord(ProcedureReturnsResult)) + ' AS PROCEDURE_TYPE'
        + ' FROM pg_proc p';
      if ProcedureCondition <> '' then
        SQL := SQL + ' WHERE ' + ProcedureCondition;
      SQL := SQL + ' ORDER BY PROCEDURE_NAME';
    end;
  end;

  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(ProceduresColumnsDynArray));
end;

{**
  Gets a description of a catalog's stored procedure parameters
  and result columns.

  <P>Only descriptions matching the schema, procedure and
  parameter name criteria are returned.  They are ordered by
  PROCEDURE_SCHEM and PROCEDURE_NAME. Within this, the return value,
  if any, is first. Next are the parameter descriptions in call
  order. The column descriptions follow in column number order.

  <P>Each row in the <code>ResultSet</code> is a parameter description or
  column description with the following fields:
   <OL>
 	<LI><B>PROCEDURE_CAT</B> String => procedure catalog (may be null)
 	<LI><B>PROCEDURE_SCHEM</B> String => procedure schema (may be null)
 	<LI><B>PROCEDURE_NAME</B> String => procedure name
 	<LI><B>COLUMN_NAME</B> String => column/parameter name
 	<LI><B>COLUMN_TYPE</B> Short => kind of column/parameter:
       <UL>
       <LI> procedureColumnUnknown - nobody knows
       <LI> procedureColumnIn - IN parameter
       <LI> procedureColumnInOut - INOUT parameter
       <LI> procedureColumnOut - OUT parameter
       <LI> procedureColumnReturn - procedure return value
       <LI> procedureColumnResult - result column in <code>ResultSet</code>
       </UL>
   <LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => SQL type name, for a UDT type the
   type name is fully qualified
 	<LI><B>PRECISION</B> int => precision
 	<LI><B>LENGTH</B> int => length in bytes of data
 	<LI><B>SCALE</B> short => scale
 	<LI><B>RADIX</B> short => radix
 	<LI><B>NULLABLE</B> short => can it contain NULL?
       <UL>
       <LI> procedureNoNulls - does not allow NULL values
       <LI> procedureNullable - allows NULL values
       <LI> procedureNullableUnknown - nullability unknown
       </UL>
 	<LI><B>REMARKS</B> String => comment describing parameter/column
   </OL>

  <P><B>Note:</B> Some databases may not return the column
  descriptions for a procedure. Additional columns beyond
  REMARKS can be defined by the database.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param procedureNamePattern a procedure name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row describes a stored procedure parameter or
       column
  @see #getSearchStringEscape
}
function TZPostgreSQLDatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;

  procedure InsertProcedureColumnRow(const AResultSet: IZResultSet;
    const ASchema, AProcedureName, AColumnName: string;
    const AColumnType, ADataType: integer; const ATypeName: string;
    const ANullable: integer);
  begin
    AResultSet.MoveToInsertRow;
    //AResultSet.UpdateNull(CatalogNameIndex);
    AResultSet.UpdateString(SchemaNameIndex, ASchema);
    AResultSet.UpdateString(ProcColProcedureNameIndex, AProcedureName);
    AResultSet.UpdateString(ProcColColumnNameIndex, AColumnName);
    AResultSet.UpdateInt(ProcColColumnTypeIndex, AColumnType);
    AResultSet.UpdateInt(ProcColDataTypeIndex, ADataType);
    AResultSet.UpdateString(ProcColTypeNameIndex, ATypeName);
    AResultSet.UpdateNull(ProcColPrecisionIndex);
    AResultSet.UpdateNull(ProcColLengthIndex);
    AResultSet.UpdateNull(ProcColScaleIndex);
    AResultSet.UpdateNull(ProcColRadixIndex);
    AResultSet.UpdateInt(ProcColNullableIndex, ANullable);
    AResultSet.UpdateNull(ProcColRemarksIndex);
    AResultSet.InsertRow;
  end;

var
  I, ReturnType: Integer;
  ColumnTypeOid, ArgOid: OID;
  SQL, ReturnTypeType: string;
  IsInParam, IsOutParam: Boolean;
  ArgTypes, ArgNames, ArgModes: TStrings;
  Ver73Up, Ver80Up: Boolean;
  ResultSet: IZResultSet;
  ColumnsRS: IZResultSet;
  ArgMode: PChar;
  OutParamCount: Integer;
  ColumnName: string;
  ColumnType: Integer;
  ProcedureCondition, SchemaCondition: string;
begin
  SchemaCondition := ConstructNameCondition(SchemaPattern,'n.nspname');
  ProcedureCondition := ConstructNameCondition(ProcedureNamePattern,'p.proname');
  Result := inherited UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);

  Ver80Up := (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(8, 0);
  Ver73Up := Ver80Up or (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3);
  if Ver80Up then
  begin
    SQL := 'SELECT n.nspname,p.proname,p.prorettype,p.proargtypes,t.typtype,'
      + 'p.proallargtypes,p.proargnames,p.proargmodes,t.typrelid '
      + 'FROM pg_catalog.pg_proc p, pg_catalog.pg_namespace n, pg_catalog.pg_type t '
      + 'WHERE p.pronamespace=n.oid AND p.prorettype=t.oid';
    if SchemaPattern <> '' then
      SQL := SQL + ' AND ' + SchemaCondition;
    if ProcedureNamePattern <> '' then
      SQL := SQL + ' AND ' + ProcedureCondition;
    SQL := SQL + ' ORDER BY n.nspname, p.proname';
  end
  else
  if Ver73Up then
  begin
    SQL := 'SELECT n.nspname,p.proname,p.prorettype,p.proargtypes,t.typtype,'
      + 'NULL AS proallargtypes,NULL AS proargnames,NULL AS proargnames,t.typrelid '
      + 'FROM pg_catalog.pg_proc p, pg_catalog.pg_namespace n,'
      + ' pg_catalog.pg_type t WHERE p.pronamespace=n.oid AND p.prorettype=t.oid';
    if SchemaPattern <> '' then
        SQL := SQL + ' AND ' + SchemaCondition;
    if ProcedureNamePattern <> '' then
        SQL := SQL + ' AND ' + ProcedureCondition;
    SQL := SQL + ' ORDER BY n.nspname, p.proname';
  end
  else
  begin
    SQL := 'SELECT NULL AS nspname,p.proname,p.prorettype,p.proargtypes,'
      + ' NULL AS proallargtypes,NULL AS proargnames,NULL AS proargnames,t.typtype,t.typrelid'
      + ' FROM pg_proc p, pg_type t'
      + ' WHERE p.prorettype=t.oid';
    if ProcedureNamePattern <> '' then
      SQL := SQL + ' AND ' + ProcedureCondition;
    SQL := SQL + ' ORDER BY p.proname';
  end;

  ArgTypes := TStringList.Create;
  ArgNames := TStringList.Create;
  ArgModes := TStringList.Create;
  try
    ResultSet := GetConnection.CreateStatement.ExecuteQuery(SQL); //FirmOS Patch
    with ResultSet do
    begin
      while Next do
      begin
        ReturnType := GetIntByName('prorettype');
        ReturnTypeType := GetStringByName('typtype');

        ArgTypes.Clear;
        ArgNames.Clear;
        ArgModes.Clear;

        if (IsNullByName('proallargtypes')) then
          PutSplitString(ArgTypes, GetStringByName('proargtypes'), #10#13#9' ')
        else
          ParseACLArray(ArgTypes, GetStringByName('proallargtypes'));
        ParseACLArray(ArgNames, GetStringByName('proargnames'));
        ParseACLArray(ArgModes, GetStringByName('proargmodes'));

        OutParamCount := 0;
        for I := 0 to ArgTypes.Count - 1 do
        begin
          IsInParam := True;
          IsOutParam := False;
          if ArgModes.Count > I then begin
            ArgMode := Pointer(ArgModes[i]);
            IsInParam := Ord(ArgMode^) in [Ord('i'), Ord('b'), ORd('v')];
            IsOutParam := Ord(ArgMode^) in [Ord('o'), Ord('b'), Ord('t')];
          end;

          if IsOutParam then
            Inc(OutParamCount);

          // column name
          ArgOid := {$IFDEF UNICODE}UnicodeToInt64{$ELSE}RawToInt64{$ENDIF}(ArgTypes.Strings[i]);
          if ArgNames.Count > I then
            ColumnName := ArgNames.Strings[I]
          else
            ColumnName := '$' + ZFastCode.IntToStr(I + 1);

          // column type
          if IsInParam then begin
            if IsOutParam
            then ColumnType := Ord(pctInOut)
            else ColumnType := Ord(pctIn);
          end else if IsOutParam
           then ColumnType := Ord(pctOut)
           else ColumnType := Ord(pctUnknown);

          InsertProcedureColumnRow(Result, GetStringByName('nspname'),
            GetStringByName('proname'), ColumnName, ColumnType,
            Ord(GetSQLTypeByOid(ArgOid)), GetPostgreSQLType(ArgOid),
            Ord(ntNullableUnknown));
        end;

        if (OutParamCount > 0) then
          Continue;

        if (ReturnTypeType = 'c') then begin // Extract composit type columns
          ColumnsRS := GetConnection.CreateStatement.ExecuteQuery(
            Format('SELECT a.attname,a.atttypid'
              + ' FROM pg_catalog.pg_attribute a WHERE a.attrelid=%s'
              + ' ORDER BY a.attnum',
              [ResultSet.GetStringByName('typrelid')]));
          while ColumnsRS.Next do begin
            ColumnTypeOid := ColumnsRS.GetUIntByName('atttypid');
            InsertProcedureColumnRow(Result, GetStringByName('nspname'),
              GetStringByName('proname'), ColumnsRS.GetStringByName('attname'),
              Ord(pctResultSet), Ord(GetSQLTypeByOid(ColumnTypeOid)),
              GetPostgreSQLType(ColumnTypeOid), Ord(ntNullableUnknown));
          end;
          ColumnsRS.Close;
        end else if (ReturnTypeType <> 'p') then // Single non-pseudotype return value
          InsertProcedureColumnRow(Result, GetStringByName('nspname'),
            GetStringByName('proname'), 'returnValue', Ord(pctReturn),
            Ord(GetSQLTypeByOid(ReturnType)), GetPostgreSQLType(ReturnType),
            Ord(ntNullableUnknown));
      end;
      Close;
    end;
  finally
    ArgTypes.Free;
    ArgNames.Free;
    ArgModes.Free;
  end;
end;

{**
  Gets a description of tables available in a catalog.

  <P>Only table descriptions matching the catalog, schema, table
  name and type criteria are returned.  They are ordered by
  TABLE_TYPE, TABLE_SCHEM and TABLE_NAME.

  <P>Each table description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
 	<LI><B>REMARKS</B> String => explanatory comment on the table
   </OL>

  <P><B>Note:</B> Some databases may not return information for
  all tables.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param types a list of table types to include; null returns all types
  @return <code>ResultSet</code> - each row is a table description
  @see #getSearchStringEscape
}
function TZPostgreSQLDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  I: Integer;
  TableType, OrderBy, SQL: string;
  UseSchemas: Boolean;
  LTypes: TStringDynArray;
  TableNameCondition, SchemaCondition, CatalogCondition: string;
begin
  CatalogCondition := ConstructNameCondition(Catalog,'dn.nspname');
  SchemaCondition := ConstructNameCondition(SchemaPattern,'n.nspname');
  TableNameCondition := ConstructNameCondition(TableNamePattern,'c.relname');
  UseSchemas := True;

  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
  begin
    SQL := ' SELECT dn.nspname AS TABLE_CAT, n.nspname AS TABLE_SCHEM,'
      + ' c.relname AS TABLE_NAME,  '
      + ' CASE (n.nspname LIKE ''pg\\_%'')'
      + '   OR (n.nspname=''information_schema'')'
      + ' WHEN true THEN CASE n.nspname '
      + '   WHEN ''pg_catalog'' THEN CASE c.relkind '
      + '     WHEN ''r'' THEN ''SYSTEM TABLE'''
      + '     WHEN ''v'' THEN ''SYSTEM VIEW'' '
      + '     WHEN ''i'' THEN ''SYSTEM INDEX'' '
      + '     ELSE NULL '
      + '   END '
      + '   WHEN ''information_schema'' THEN CASE c.relkind '
      + '     WHEN ''r'' THEN ''SYSTEM TABLE'''
      + '     WHEN ''v'' THEN ''SYSTEM VIEW'' '
      + '     WHEN ''i'' THEN ''SYSTEM INDEX'' '
      + '     ELSE NULL '
      + '   END '
      + '   WHEN ''pg_toast'' THEN CASE c.relkind '
      + '     WHEN ''r'' THEN ''SYSTEM TOAST TABLE'' '
      + '     WHEN ''i'' THEN ''SYSTEM TOAST INDEX'' '
      + '     ELSE NULL '
      + '   END '
      + '   ELSE CASE c.relkind '
      + '	WHEN ''r'' THEN ''TEMPORARY TABLE'' '
      + '	WHEN ''i'' THEN ''TEMPORARY INDEX'' '
      + '	ELSE NULL '
      + '   END '
      + ' END '
      + ' WHEN false THEN CASE c.relkind '
      + '   WHEN ''r'' THEN ''TABLE'' '
      + '   WHEN ''i'' THEN ''INDEX'' '
      + '   WHEN ''S'' THEN ''SEQUENCE'' '
      + '   WHEN ''v'' THEN ''VIEW'' '
      + '   ELSE NULL '
      + ' END '
      + ' ELSE NULL '
      + ' END '
      + ' AS TABLE_TYPE, d.description AS REMARKS '
      + ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class c '
      + ' LEFT JOIN pg_catalog.pg_description d ON (c.oid = d.objoid AND d.objsubid = 0) '
      + ' LEFT JOIN pg_catalog.pg_class dc ON (d.classoid=dc.oid AND dc.relname=''pg_class'') '
      + ' LEFT JOIN pg_catalog.pg_namespace dn ON (dn.oid=dc.relnamespace AND dn.nspname=''pg_catalog'') '
      + ' WHERE c.relnamespace = n.oid ';
    if CatalogCondition <> '' then
      SQL := SQL + ' AND ' + CatalogCondition;
    if SchemaPattern <> '' then
      SQL := SQL + ' AND ' + SchemaCondition;

    OrderBy := ' ORDER BY TABLE_TYPE,TABLE_SCHEM,TABLE_NAME';
  end
  else
  begin
    UseSchemas := False;
    TableType := ' CASE c.relname LIKE ''pg\\_%'' '
      + 'WHEN true THEN CASE c.relname LIKE ''pg\\_toast\\_%'' '
      + 'WHEN true THEN CASE c.relkind '
      + '  WHEN ''r'' THEN ''SYSTEM TOAST TABLE'' '
      + '  WHEN ''i'' THEN ''SYSTEM TOAST INDEX'' '
      + '  ELSE NULL '
      + 'END '
      + 'WHEN false THEN CASE c.relname LIKE ''pg\\_temp\\_%'' '
      + '  WHEN true THEN CASE c.relkind '
      + '    WHEN ''r'' THEN ''TEMPORARY TABLE'' '
      + '    WHEN ''i'' THEN ''TEMPORARY INDEX'' '
      + '    ELSE NULL '
      + '  END '
      + '  WHEN false THEN CASE c.relkind '
      + '    WHEN ''r'' THEN ''SYSTEM TABLE'' '
      + '    WHEN ''v'' THEN ''SYSTEM VIEW'' '
      + '    WHEN ''i'' THEN ''SYSTEM INDEX'' '
      + '    ELSE NULL '
      + '  END '
      + '  ELSE NULL '
      + 'END '
      + 'ELSE NULL '
      + 'END '
      + 'WHEN false THEN CASE c.relkind '
      + '  WHEN ''r'' THEN ''TABLE'' '
      + '  WHEN ''i'' THEN ''INDEX'' '
      + '  WHEN ''S'' THEN ''SEQUENCE'' '
      + '  WHEN ''v'' THEN ''VIEW'' '
      + '  ELSE NULL '
      + 'END '
      + 'ELSE NULL '
      + ' END ';
    OrderBy := ' ORDER BY TABLE_TYPE,TABLE_NAME ';
    SQL := 'SELECT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM,'
      + ' c.relname AS TABLE_NAME, ' + TableType + ' AS TABLE_TYPE,'
      + ' NULL AS REMARKS FROM pg_class c WHERE true ';
  end;

  if (Pointer(Types) = nil) then begin
    SetLength(LTypes, 3);
    // SetLength(LTypes, 6);
    LTypes[0] := 'TABLE';
    LTypes[1] := 'VIEW';
    LTypes[2] := 'TEMPORARY TABLE';
    // LTypes[3] := 'SYSTEM TABLE';
    // LTypes[4] := 'SYSTEM TOAST TABLE';
    // LTypes[5] := 'SYSTEM VIEW';
  end
  else
    LTypes := Types;

  If TableNameCondition <> '' then
    SQL := SQL + ' AND ' + TableNameCondition;

  SQL := SQL + ' AND (false';
  for I := 0 to High(LTypes) do
    SQL := SQL + ' OR (' + TableTypeSQLExpression(LTypes[i], UseSchemas) + ')';
  SQL := SQL + ')' + OrderBy;

  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(TableColumnsDynArray));
  (*
  {now let's complete missing catalog informations ... if possible ): i didn't found a way to get it running without the IS}
  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(8, 4) then //information_schema only persits since 8.4
  begin
    (Result as IZVirtualResultSet).SetConcurrency(rcUpdatable);
    SchemaCondition := ConstructNameCondition(SchemaPattern,'information_schema.tables.table_schema');
    TableNameCondition := ConstructNameCondition(TableNamePattern,'information_schema.tables.table_name');
    SQL :='SELECT table_catalog,table_schema,table_name from information_schema.tables';
    if (SchemaCondition <> '') or (TableNameCondition <> '')then
    begin
      SQL := SQL + ' where ';
      if (SchemaCondition <> '') then
      begin
        SQL := SQL + SchemaCondition;
        if (TableNameCondition <> '') then
          SQL := SQL + ' and '+TableNameCondition;
      end
      else
        SQL := SQL + TableNameCondition;
    end;
    SQL := SQL + ' order by table_name, table_schema';
    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.Next;
        TempRes := Result.GetPAnsiChar(TableNameIndex);
        TempIS := GetPAnsiChar(TableNameIndex);
        if MemLCompAnsi(TempRes.P, TempIS.P, Max(TempRes.Len, TempIS.Len)) then
        begin
          TempRes := Result.GetPAnsiChar(SchemaNameIndex);
          TempIS := GetPAnsiChar(SchemaNameIndex);
          if MemLCompAnsi(TempRes.P, TempIS.P, Max(TempRes.Len, TempIS.Len)) then
          begin
            Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(CatalogNameIndex));
            Result.UpdateRow;
          end;
        end;
      end;
      (Result as IZVirtualResultSet).BeforeFirst;
      (Result as IZVirtualResultSet).SetConcurrency(rcReadOnly);
      Close;
    end;
  end;
  *)
end;

{**
  Gets the schema names available in this database.  The results
  are ordered by schema name.

  <P>The schema column is:
   <OL>
 	<LI><B>TABLE_SCHEM</B> String => schema name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  schema name
}
function TZPostgreSQLDatabaseMetadata.UncachedGetSchemas: IZResultSet;
var
  SQL: string;
begin
    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := 'SELECT nspname AS TABLE_SCHEM FROM pg_catalog.pg_namespace'
        + ' WHERE nspname <> ''pg_toast'' AND nspname NOT'
        + ' LIKE ''pg\\_temp\\_%'' ORDER BY TABLE_SCHEM';
    end else
      SQL := 'SELECT ''''::text AS TABLE_SCHEM ORDER BY TABLE_SCHEM';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(SchemaColumnsDynArray));
end;

{**
  Gets the catalog names available in this database.  The results
  are ordered by catalog name.

  <P>The catalog column is:
   <OL>
 	<LI><B>TABLE_CAT</B> String => catalog name
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  catalog name
}
function TZPostgreSQLDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
var
  SQL: string;
begin
    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      SQL := 'SELECT datname AS TABLE_CAT FROM pg_catalog.pg_database'
        + ' ORDER BY TABLE_CAT';
    end else
      SQL := 'SELECT datname AS TABLE_CAT FROM pg_database ORDER BY TABLE_CAT';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(CatalogColumnsDynArray));
end;

{**
  Gets the table types available in this database.  The results
  are ordered by table type.

  <P>The table type is:
   <OL>
 	<LI><B>TABLE_TYPE</B> String => table type.  Typical types are "TABLE",
 			"VIEW",	"SYSTEM TABLE", "GLOBAL TEMPORARY",
 			"LOCAL TEMPORARY", "ALIAS", "SYNONYM".
   </OL>

  @return <code>ResultSet</code> - each row has a single String column that is a
  table type
}
function TZPostgreSQLDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  Types: array [0..10] of string = ('TABLE', 'VIEW', 'INDEX',
    'SEQUENCE', 'SYSTEM TABLE', 'SYSTEM TOAST TABLE',
    'SYSTEM TOAST INDEX', 'SYSTEM VIEW', 'SYSTEM INDEX',
    'TEMPORARY TABLE', 'TEMPORARY INDEX');
var
  I: Integer;
begin
 Result:=inherited UncachedGetTableTypes;

 for I := 0 to 10 do
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(TableTypeColumnTableTypeIndex, Types[I]);
      Result.InsertRow;
    end;
end;

{**
  Gets a description of table columns available in
  the specified catalog.

  <P>Only column descriptions matching the catalog, schema, table
  and column name criteria are returned.  They are ordered by
  TABLE_SCHEM, TABLE_NAME and ORDINAL_POSITION.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name,
   for a UDT the type name is fully qualified
 	<LI><B>COLUMN_SIZE</B> int => column size.  For char or date
 	    types this is the maximum number of characters, for numeric or
 	    decimal types this is precision.
 	<LI><B>BUFFER_LENGTH</B> is not used.
 	<LI><B>DECIMAL_DIGITS</B> int => the number of fractional digits
 	<LI><B>NUM_PREC_RADIX</B> int => Radix (typically either 10 or 2)
 	<LI><B>NULLABLE</B> int => is NULL allowed?
       <UL>
       <LI> columnNoNulls - might not allow NULL values
       <LI> columnNullable - definitely allows NULL values
       <LI> columnNullableUnknown - nullability unknown
       </UL>
 	<LI><B>REMARKS</B> String => comment describing column (may be null)
  	<LI><B>COLUMN_DEF</B> String => default value (may be null)
 	<LI><B>SQL_DATA_TYPE</B> int => unused
 	<LI><B>SQL_DATETIME_SUB</B> int => unused
 	<LI><B>CHAR_OCTET_LENGTH</B> int => for char types the
        maximum number of bytes in the column
 	<LI><B>ORDINAL_POSITION</B> int	=> index of column in table
       (starting at 1)
 	<LI><B>IS_NULLABLE</B> String => "NO" means column definitely
       does not allow NULL values; "YES" means the column might
       allow NULL values.  An empty string means nobody knows.
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column description
  @see #getSearchStringEscape
}
function TZPostgreSQLDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
begin
  Result := InternalUncachedGetColumns(Catalog, SchemaPattern, TableNamePattern,
    ColumnNamePattern, '');
end;

{**
  Gets a description of the access rights for a table's columns.

  <P>Only privileges matching the column name criteria are
  returned.  They are ordered by COLUMN_NAME and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>GRANTOR</B> => grantor of access (may be null)
 	<LI><B>GRANTEE</B> String => grantee of access
 	<LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
 	<LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param columnNamePattern a column name pattern
  @return <code>ResultSet</code> - each row is a column privilege description
  @see #getSearchStringEscape
}
function TZPostgreSQLDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  I, J: Integer;
  SQL, Column, Owner: string;
  Privileges, Grantable, Grantee: string;
  Permissions, PermissionsExp: TStrings;
  ColumnNameCondition, TableNameCondition, SchemaCondition: string;
begin
  SchemaCondition := ConstructNameCondition(Schema,'n.nspname');
  TableNameCondition := ConstructNameCondition(Table,'c.relname');
  ColumnNameCondition := ConstructNameCondition(ColumnNamePattern,'a.attname');
  Result:=inherited UncachedGetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);

  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
  begin
    SQL := 'SELECT n.nspname,c.relname,u.usename,c.relacl,a.attname '
      + ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class c,'
      + ' pg_catalog.pg_user u, pg_catalog.pg_attribute a '
      + ' WHERE c.relnamespace = n.oid AND u.usesysid = c.relowner '
      + ' AND c.oid = a.attrelid AND c.relkind = ''r'''
      + ' AND a.attnum > 0 AND NOT a.attisdropped';
    if Schema <> '' then
      SQL := SQL + ' AND ' + SchemaCondition;
  end
  else
  begin
    SQL := 'SELECT NULL::text AS nspname,c.relname,u.usename,c.relacl,'
      + 'a.attname FROM pg_class c, pg_user u,pg_attribute a '
      + ' WHERE u.usesysid = c.relowner AND c.oid = a.attrelid '
      + ' AND a.attnum > 0 AND c.relkind = ''r''';
  end;

  If TableNameCondition <> '' then
    SQL := SQL + ' AND ' + TableNameCondition;
  If ColumnNameCondition <> '' then
    SQL := SQL + ' AND '+ ColumnNameCondition;
  SQL := SQL + ' ORDER BY attname';

  Permissions := TStringList.Create;
  PermissionsExp := TStringList.Create;
  try
    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        //SchemaName := GetStringByName('nspname');
        //TableName := GetStringByName('relname');
        Column := GetStringByName('attname');
        Owner := GetStringByName('usename');
        Permissions.Clear;
        ParseACLArray(Permissions, GetStringByName('relacl'));
        for I := 0 to Permissions.Count-1 do
        begin
          PutSplitString(PermissionsExp, Permissions.Strings[I], '=');
          if PermissionsExp.Count < 2 then
            Continue;
          Grantee := PermissionsExp.Strings[0];
          if Grantee = '' then
            Grantee := 'PUBLIC';
          Privileges := PermissionsExp.Strings[1];
          for J := 1 to Length(Privileges) do
          begin
            if Owner = Grantee then
              Grantable := 'YES'
            else Grantable := 'NO';
            Result.MoveToInsertRow;
            Result.UpdateString(SchemaNameIndex, Schema);
            Result.UpdateString(TableNameIndex, Table);
            Result.UpdateString(ColumnNameIndex, Column);
            Result.UpdateString(TableColPrivGrantorIndex, Owner);
            Result.UpdateString(TableColPrivGranteeIndex, Grantee);
            Result.UpdateString(TableColPrivPrivilegeIndex, GetPrivilegeName(Privileges[J]));
            Result.UpdateString(TableColPrivIsGrantableIndex, grantable);
            Result.InsertRow;
          end;
        end;
      end;
      Close;
    end;
  finally
    Permissions.Free;
    PermissionsExp.Free;
  end;
end;

{**
  Gets a description of the access rights for each table available
  in a catalog. Note that a table privilege applies to one or
  more columns in the table. It would be wrong to assume that
  this priviledge applies to all columns (this may be true for
  some systems but is not true for all.)

  <P>Only privileges matching the schema and table name
  criteria are returned.  They are ordered by TABLE_SCHEM,
  TABLE_NAME, and PRIVILEGE.

  <P>Each privilige description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>GRANTOR</B> => grantor of access (may be null)
 	<LI><B>GRANTEE</B> String => grantee of access
 	<LI><B>PRIVILEGE</B> String => name of access (SELECT,
       INSERT, UPDATE, REFRENCES, ...)
 	<LI><B>IS_GRANTABLE</B> String => "YES" if grantee is permitted
       to grant to others; "NO" if not; null if unknown
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param tableNamePattern a table name pattern
  @return <code>ResultSet</code> - each row is a table privilege description
  @see #getSearchStringEscape
}
function TZPostgreSQLDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  I, J: Integer;
  SQL, SchemaName, TableName, Owner: string;
  Privileges, Grantable, Grantee: string;
  Permissions, PermissionsExp: TStringList;
  TableNameCondition, SchemaCondition: string;
begin
  SchemaCondition := ConstructNameCondition(SchemaPattern,'n.nspname');
  TableNameCondition := ConstructNameCondition(TableNamePattern,'c.relname');
  Result:=inherited UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
  begin
    SQL := 'SELECT n.nspname,c.relname,u.usename,c.relacl '
      + ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class c,'
      + ' pg_catalog.pg_user u WHERE c.relnamespace = n.oid '
      + ' AND u.usesysid = c.relowner AND c.relkind = ''r'' ';
    if SchemaPattern <> '' then
      SQL := SQL + ' AND ' + SchemaCondition;
  end
  else
  begin
    SQL := 'SELECT NULL::text AS nspname,c.relname,u.usename,c.relacl '
      + ' FROM pg_class c, pg_user u WHERE u.usesysid = c.relowner '
      + ' AND c.relkind = ''r'' ';
  end;

  SQL := SQL + ' AND ' + TableNameCondition
    + ' ORDER BY nspname, relname';

  Permissions := TStringList.Create;
  PermissionsExp := TStringList.Create;
  try
    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        SchemaName := GetStringByName('nspname');
        TableName := GetStringByName('relname');
        Owner := GetStringByName('usename');
        SchemaName := GetStringByName('nspname');
        Permissions.Clear;
        ParseACLArray(Permissions, GetStringByName('relacl'));
        Permissions.Sort;
        for I := 0 to Permissions.Count-1 do
        begin
          PutSplitString(PermissionsExp, Permissions.Strings[I], '=');
          if PermissionsExp.Count < 2 then
            Continue;
          Grantee := PermissionsExp.Strings[0];
          if Grantee = '' then
          Grantee := 'PUBLIC';
          Privileges := PermissionsExp.Strings[1];
          for J := 1 to Length(Privileges) do
          begin
            if Owner = Grantee then
              Grantable := 'YES'
            else Grantable := 'NO';
            Result.MoveToInsertRow;
            Result.UpdateNull(CatalogNameIndex);
            Result.UpdateString(SchemaNameIndex, SchemaName);
            Result.UpdateString(TableNameIndex, TableName);
            Result.UpdateString(TablePrivGrantorIndex, Owner);
            Result.UpdateString(TablePrivGranteeIndex, Grantee);
            Result.UpdateString(TablePrivPrivilegeIndex, GetPrivilegeName(Privileges[J]));
            Result.UpdateString(TablePrivIsGrantableIndex, grantable);
            Result.InsertRow;
          end;
        end;
      end;
      Close;
    end;
  finally
    Permissions.Free;
    PermissionsExp.Free;
  end;
end;

{**
  Gets a description of a table's columns that are automatically
  updated when any value in a row is updated.  They are
  unordered.

  <P>Each column description has the following columns:
   <OL>
 	<LI><B>SCOPE</B> short => is not used
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>TYPE_NAME</B> String => Data source dependent type name
 	<LI><B>COLUMN_SIZE</B> int => precision
 	<LI><B>BUFFER_LENGTH</B> int => length of column value in bytes
 	<LI><B>DECIMAL_DIGITS</B> short	 => scale
 	<LI><B>PSEUDO_COLUMN</B> short => is this a pseudo column
       like an Oracle ROWID
       <UL>
       <LI> versionColumnUnknown - may or may not be pseudo column
       <LI> versionColumnNotPseudo - is NOT a pseudo column
       <LI> versionColumnPseudo - is a pseudo column
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a column description
  @exception SQLException if a database access error occurs
}
function TZPostgreSQLDatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
    Result:=inherited UncachedGetVersionColumns(Catalog, Schema, Table);

    Result.MoveToInsertRow;
    //Result.UpdateNull(TableColVerScopeIndex);
    Result.UpdateString(TableColVerColNameIndex, 'ctid');
    Result.UpdateInt(TableColVerDataTypeIndex, Ord(GetSQLTypeByName('tid')));
    Result.UpdateString(TableColVerTypeNameIndex, 'tid');
    //Result.UpdateNull(TableColVerColSizeIndex);
    //Result.UpdateNull(TableColVerBufLengthIndex);
    //Result.UpdateNull(TableColVerDecimalDigitsIndex);
    Result.UpdateInt(TableColVerPseudoColumnIndex, Ord(vcPseudo));
    Result.InsertRow;
end;

{**
  Gets a description of a table's primary key columns.  They
  are ordered by COLUMN_NAME.

  <P>Each primary key column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>COLUMN_NAME</B> String => column name
 	<LI><B>KEY_SEQ</B> short => sequence number within primary key
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @exception SQLException if a database access error occurs
}
function TZPostgreSQLDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  SQL, Select, From, Where: string;
  TableNameCondition, SchemaCondition: string;
begin
  SchemaCondition := ConstructNameCondition(Schema,'n.nspname');
  TableNameCondition := ConstructNameCondition(Table,'ct.relname');
  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
  begin
    Select := 'SELECT NULL AS TABLE_CAT, n.nspname AS TABLE_SCHEM,';
    From := ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class ct,'
      + ' pg_catalog.pg_class ci, pg_catalog.pg_attribute a,'
      + ' pg_catalog.pg_index i';
    Where := ' AND ct.relnamespace = n.oid';
    if Schema <> '' then
      Where := Where + ' AND ' + SchemaCondition;
  end
  else
  begin
    Select := 'SELECT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM,';
    From := ' FROM pg_class ct, pg_class ci, pg_attribute a, pg_index i';
  end;
  SQL := Select + ' ct.relname AS TABLE_NAME, a.attname AS COLUMN_NAME,'
    + ' a.attnum AS KEY_SEQ, ci.relname AS PK_NAME'
    + From
    + ' WHERE ct.oid=i.indrelid AND ci.oid=i.indexrelid'
    + ' AND a.attrelid=ci.oid AND i.indisprimary';
  if Table <> '' then
     SQL := SQL + ' AND ' + TableNameCondition;
  SQL := SQL + Where + ' ORDER BY table_name, pk_name, key_seq';

  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(PrimaryKeyColumnsDynArray));
end;

{**
  Gets a description of the primary key columns that are
  referenced by a table's foreign key columns (the primary keys
  imported by a table).  They are ordered by PKTABLE_CAT,
  PKTABLE_SCHEM, PKTABLE_NAME, and KEY_SEQ.

  <P>Each primary key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog
       being imported (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema
       being imported (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
       being imported
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
       being imported
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a primary key column description
  @see #getExportedKeys
}
function TZPostgreSQLDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
const
  {%H-}tc_constraint_catalog_Index = FirstDbcIndex + 0;
  tc_constraint_schema_Index       = FirstDbcIndex + 1;
  ccu_table_name_Index             = FirstDbcIndex + 2;
  ccu_column_name_Index            = FirstDbcIndex + 3;
  {%H-}kcu_table_catalog_Index     = FirstDbcIndex + 4;
  kcu_constraint_schema_Index      = FirstDbcIndex + 5;
  kcu_table_name_Index             = FirstDbcIndex + 6;
  kcu_column_name_Index            = FirstDbcIndex + 7;
  kcu_ordinal_position_Index       = FirstDbcIndex + 8;
  rf_update_rule_Index             = FirstDbcIndex + 9;
  rf_delete_rule_Index             = FirstDbcIndex + 10;
  kcu_constraint_name_Index        = FirstDbcIndex + 11;
  rf_unique_constraint_name_Index  = FirstDbcIndex + 12;
  tc_is_deferrable_Index           = FirstDbcIndex + 13;
var
  Len: NativeUInt;
  SQL: string;
  TableNameCondition, SchemaCondition, CatalogCondition: string;
begin
  CatalogCondition := ConstructNameCondition(Catalog,'kcu.table_catalog');
  SchemaCondition := ConstructNameCondition(Schema,'kcu.constraint_schema');
  TableNameCondition := ConstructNameCondition(Table,'kcu.table_name');
  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 4) then
  begin
    Result:=inherited UncachedGetImportedKeys(Catalog, Schema, Table);
    SQL := 'SELECT '+
      'tc.constraint_catalog as PKTABLE_CAT, '+
      'tc.constraint_schema as PKTABLE_SCHEM, '+
      'ccu.table_name as PKTABLE_NAME, '+
      'ccu.column_name as PKCOLUMN_NAME, '+
      'kcu.table_catalog as FKTABLE_CAT, '+
      'kcu.constraint_schema as FKTABLE_SCHEM, '+
      'kcu.table_name as PKTABLE_NAME, '+
      'kcu.column_name as FKCOLUMN_NAME, '+
      'kcu.ordinal_position as PK_NAME, '+
      'rf.update_rule as UPDATE_RULE, '+
      'rf.delete_rule as DELETE_RULE, '+
      'kcu.constraint_name as FK_NAME, '+
      'rf.unique_constraint_name as PK_NAME, '+
      'tc.is_deferrable as DEFERRABILITY '+
      'FROM information_schema.table_constraints AS tc '+
      'JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name '+
      'JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name '+
      'join information_schema.referential_constraints as rf on rf.constraint_name = tc.constraint_name '+
      'WHERE constraint_type = ''FOREIGN KEY''';
    if Catalog <> '' then
      SQL := SQL + ' and ' + CatalogCondition;
    if Schema <> '' then
      SQL := SQL + ' and ' + SchemaCondition;
    if Table <> '' then
      SQL := SQL + ' and ' + TableNameCondition;

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        //Result.UpdatePAnsiChar(ImportedKeyColPKTableCatalogIndex, GetPAnsiChar(tc_constraint_catalog_Index, Len), @Len);
        Result.UpdatePAnsiChar(ImportedKeyColPKTableSchemaIndex, GetPAnsiChar(tc_constraint_schema_Index, Len), @Len);
        Result.UpdatePAnsiChar(ImportedKeyColPKTableNameIndex, GetPAnsiChar(ccu_table_name_Index, Len), @Len);
        Result.UpdatePAnsiChar(ImportedKeyColPKColumnNameIndex, GetPAnsiChar(ccu_column_name_Index, Len), @Len);
        //Result.UpdatePAnsiChar(ImportedKeyColFKTableCatalogIndex, GetPAnsiChar(kcu_table_catalog_Index, Len), @Len);
        Result.UpdatePAnsiChar(ImportedKeyColFKTableSchemaIndex, GetPAnsiChar(kcu_constraint_schema_Index, Len), @Len);
        Result.UpdatePAnsiChar(ImportedKeyColFKTableNameIndex, GetPAnsiChar(kcu_table_name_Index, Len), @Len);
        Result.UpdatePAnsiChar(ImportedKeyColFKColumnNameIndex, GetPAnsiChar(kcu_column_name_Index, Len), @Len);
        Result.UpdateSmall(ImportedKeyColKeySeqIndex, GetSmall(kcu_ordinal_position_Index));
        Result.UpdateSmall(ImportedKeyColUpdateRuleIndex, Ord(GetRuleType(GetString(rf_update_rule_Index))));
        Result.UpdateSmall(ImportedKeyColDeleteRuleIndex, Ord(GetRuleType(GetString(rf_delete_rule_Index))));
        Result.UpdatePAnsiChar(ImportedKeyColFKNameIndex, GetPAnsiChar(kcu_constraint_name_Index, Len), @Len);
        Result.UpdatePAnsiChar(ImportedKeyColPKNameIndex, GetPAnsiChar(rf_unique_constraint_name_Index, Len), @Len);
        if GetString(tc_is_deferrable_Index) = 'NO' then
          Result.UpdateSmall(ImportedKeyColDeferrabilityIndex, Ord(ikNotDeferrable))
        else
          Result.UpdateSmall(ImportedKeyColDeferrabilityIndex, Ord(ikInitiallyDeferred));
        Result.InsertRow;
      end;
      Close;
    end;
  end
  else
    Result := UncachedGetCrossReference('', '', '', Catalog, Schema, Table);
end;

{**
  Gets a description of the foreign key columns that reference a
  table's primary key columns (the foreign keys exported by a
  table).  They are ordered by FKTABLE_CAT, FKTABLE_SCHEM,
  FKTABLE_NAME, and KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those
  without a schema
  @param table a table name
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZPostgreSQLDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
const
  {%H-}tc_constraint_catalog_Index = FirstDbcIndex + 0;
  tc_constraint_schema_Index       = FirstDbcIndex + 1;
  ccu_table_name_Index             = FirstDbcIndex + 2;
  ccu_column_name_Index            = FirstDbcIndex + 3;
  {%H-}kcu_table_catalog_Index     = FirstDbcIndex + 4;
  kcu_constraint_schema_Index      = FirstDbcIndex + 5;
  kcu_table_name_Index             = FirstDbcIndex + 6;
  kcu_column_name_Index            = FirstDbcIndex + 7;
  kcu_ordinal_position_Index       = FirstDbcIndex + 8;
  rf_update_rule_Index             = FirstDbcIndex + 9;
  rf_delete_rule_Index             = FirstDbcIndex + 10;
  kcu_constraint_name_Index        = FirstDbcIndex + 11;
  rf_unique_constraint_name_Index  = FirstDbcIndex + 12;
  tc_is_deferrable_Index           = FirstDbcIndex + 13;
var
  Len: NativeUInt;
  SQL: string;
  TableNameCondition, SchemaCondition, CatalogCondition: string;
begin
  CatalogCondition := ConstructNameCondition(Catalog,'tc.constraint_catalog');
  SchemaCondition := ConstructNameCondition(Schema,'tc.constraint_schema');
  TableNameCondition := ConstructNameCondition(Table,'ccu.table_name');
  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 4) then
  begin
    Result:=inherited UncachedGetImportedKeys(Catalog, Schema, Table);
    SQL := 'SELECT '+
      'tc.constraint_catalog as PKTABLE_CAT, '+
      'tc.constraint_schema as PKTABLE_SCHEM, '+
      'ccu.table_name as PKTABLE_NAME, '+
      'ccu.column_name as PKCOLUMN_NAME, '+
      'kcu.table_catalog as FKTABLE_CAT, '+
      'kcu.constraint_schema as FKTABLE_SCHEM, '+
      'kcu.table_name as PKTABLE_NAME, '+
      'kcu.column_name as FKCOLUMN_NAME, '+
      'kcu.ordinal_position as KEY_SEQ, '+
      'rf.update_rule as UPDATE_RULE, '+
      'rf.delete_rule as DELETE_RULE, '+
      'kcu.constraint_name as FK_NAME, '+
      'rf.unique_constraint_name as PK_NAME, '+
      'tc.is_deferrable as DEFERRABILITY '+
      'FROM information_schema.table_constraints AS tc '+
      'JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name '+
      'JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name '+
      'join information_schema.referential_constraints as rf on rf.constraint_name = tc.constraint_name '+
      'WHERE constraint_type = ''FOREIGN KEY''';
    if Catalog <> '' then
      SQL := SQL + ' and ' + CatalogCondition;
    if Schema <> '' then
      SQL := SQL + ' and ' + SchemaCondition;
    if Table <> '' then
      SQL := SQL + ' and ' + TableNameCondition;
    SQL := SQL + ' order by kcu.table_name;';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        //Result.UpdatePAnsiChar(ExportedKeyColPKTableCatalogIndex, GetPAnsiChar(tc_constraint_catalog_Index, Len), @Len);
        Result.UpdatePAnsiChar(ExportedKeyColPKTableSchemaIndex, GetPAnsiChar(tc_constraint_schema_Index, Len), @Len);
        Result.UpdatePAnsiChar(ExportedKeyColPKTableNameIndex, GetPAnsiChar(ccu_table_name_Index, Len), @Len);
        Result.UpdatePAnsiChar(ExportedKeyColPKColumnNameIndex, GetPAnsiChar(ccu_column_name_Index, Len), @Len);
        //Result.UpdatePAnsiChar(ExportedKeyColFKTableCatalogIndex, GetPAnsiChar(kcu_table_catalog_Index, Len), @Len);
        Result.UpdatePAnsiChar(ExportedKeyColFKTableSchemaIndex, GetPAnsiChar(kcu_constraint_schema_Index, Len), @Len);
        Result.UpdatePAnsiChar(ExportedKeyColFKTableNameIndex, GetPAnsiChar(kcu_table_name_Index, Len), @Len);
        Result.UpdatePAnsiChar(ExportedKeyColFKColumnNameIndex, GetPAnsiChar(kcu_column_name_Index, Len), @Len);
        Result.UpdateSmall(ExportedKeyColKeySeqIndex, GetSmall(kcu_ordinal_position_Index));
        Result.UpdateSmall(ExportedKeyColUpdateRuleIndex, Ord(GetRuleType(GetString(rf_update_rule_Index))));
        Result.UpdateSmall(ExportedKeyColDeleteRuleIndex, Ord(GetRuleType(GetString(rf_delete_rule_Index))));
        Result.UpdatePAnsiChar(ExportedKeyColFKNameIndex, GetPAnsiChar(kcu_constraint_name_Index, Len), @Len);
        Result.UpdatePAnsiChar(ExportedKeyColPKNameIndex, GetPAnsiChar(rf_unique_constraint_name_Index, Len), @Len);
        if GetString(tc_is_deferrable_Index) = 'NO' then
          Result.UpdateSmall(ExportedKeyColDeferrabilityIndex, Ord(ikNotDeferrable))
        else
          Result.UpdateSmall(ExportedKeyColDeferrabilityIndex, Ord(ikInitiallyDeferred));
        Result.InsertRow;
      end;
      Close;
    end;
  end
  else
    Result := UncachedGetCrossReference('', '', '', Catalog, Schema, Table);
end;

{**
  Gets a description of the foreign key columns in the foreign key
  table that reference the primary key columns of the primary key
  table (describe how one table imports another's key.) This
  should normally return a single foreign key/primary key pair
  (most tables only import a foreign key from a table once.)  They
  are ordered by FKTABLE_CAT, FKTABLE_SCHEM, FKTABLE_NAME, and
  KEY_SEQ.

  <P>Each foreign key column description has the following columns:
   <OL>
 	<LI><B>PKTABLE_CAT</B> String => primary key table catalog (may be null)
 	<LI><B>PKTABLE_SCHEM</B> String => primary key table schema (may be null)
 	<LI><B>PKTABLE_NAME</B> String => primary key table name
 	<LI><B>PKCOLUMN_NAME</B> String => primary key column name
 	<LI><B>FKTABLE_CAT</B> String => foreign key table catalog (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_SCHEM</B> String => foreign key table schema (may be null)
       being exported (may be null)
 	<LI><B>FKTABLE_NAME</B> String => foreign key table name
       being exported
 	<LI><B>FKCOLUMN_NAME</B> String => foreign key column name
       being exported
 	<LI><B>KEY_SEQ</B> short => sequence number within foreign key
 	<LI><B>UPDATE_RULE</B> short => What happens to
        foreign key when primary is updated:
       <UL>
       <LI> importedNoAction - do not allow update of primary
                key if it has been imported
       <LI> importedKeyCascade - change imported key to agree
                with primary key update
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been updated
       <LI> importedKeySetDefault - change imported key to default values
                if its primary key has been updated
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       </UL>
 	<LI><B>DELETE_RULE</B> short => What happens to
       the foreign key when primary is deleted.
       <UL>
       <LI> importedKeyNoAction - do not allow delete of primary
                key if it has been imported
       <LI> importedKeyCascade - delete rows that import a deleted key
       <LI> importedKeySetNull - change imported key to NULL if
                its primary key has been deleted
       <LI> importedKeyRestrict - same as importedKeyNoAction
                                  (for ODBC 2.x compatibility)
       <LI> importedKeySetDefault - change imported key to default if
                its primary key has been deleted
       </UL>
 	<LI><B>FK_NAME</B> String => foreign key name (may be null)
 	<LI><B>PK_NAME</B> String => primary key name (may be null)
 	<LI><B>DEFERRABILITY</B> short => can the evaluation of foreign key
       constraints be deferred until commit
       <UL>
       <LI> importedKeyInitiallyDeferred - see SQL92 for definition
       <LI> importedKeyInitiallyImmediate - see SQL92 for definition
       <LI> importedKeyNotDeferrable - see SQL92 for definition
       </UL>
   </OL>

  @param primaryCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param primarySchema a schema name; "" retrieves those
  without a schema
  @param primaryTable the table name that exports the key
  @param foreignCatalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param foreignSchema a schema name; "" retrieves those
  without a schema
  @param foreignTable the table name that imports the key
  @return <code>ResultSet</code> - each row is a foreign key column description
  @see #getImportedKeys
}
function TZPostgreSQLDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
const
  {%H-}tc_constraint_catalog_Index_74 = FirstDbcIndex + 0;
  tc_constraint_schema_Index_74       = FirstDbcIndex + 1;
  ccu_table_name_Index_74             = FirstDbcIndex + 2;
  ccu_column_name_Index_74            = FirstDbcIndex + 3;
  {%H-}kcu_table_catalog_Index_74     = FirstDbcIndex + 4;
  kcu_constraint_schema_Index_74      = FirstDbcIndex + 5;
  kcu_table_name_Index_74             = FirstDbcIndex + 6;
  kcu_column_name_Index_74            = FirstDbcIndex + 7;
  kcu_ordinal_position_Index_74       = FirstDbcIndex + 8;
  rf_update_rule_Index_74             = FirstDbcIndex + 9;
  rf_delete_rule_Index_74             = FirstDbcIndex + 10;
  kcu_constraint_name_Index_74        = FirstDbcIndex + 11;
  rf_unique_constraint_name_Index_74  = FirstDbcIndex + 12;
  tc_is_deferrable_Index_74           = FirstDbcIndex + 13;

  pnspname_index             = FirstDbcIndex + 0;
  fnspname_index             = FirstDbcIndex + 1;
  prelname_index             = FirstDbcIndex + 2;
  frelname_index             = FirstDbcIndex + 3;
  {%H-}t1_tgconstrname_index = FirstDbcIndex + 4;
  keyseq_index               = FirstDbcIndex + 5;
  {%H-}fkeyname_index        = FirstDbcIndex + 6;
  t1_tgdeferrable_index      = FirstDbcIndex + 7;
  t1_tginitdeferred_index    = FirstDbcIndex + 8;
  {%H-}t1_tgnargs_index      = FirstDbcIndex + 9;
  t1_tgargs_index            = FirstDbcIndex + 10;
  updaterule_index           = FirstDbcIndex + 11;
  deleterule_index           = FirstDbcIndex + 12;
var
  Len: NativeUInt;
  SQL, Select, From, Where: string;
  DeleteRule, UpdateRule, Rule: string;
  {FKeyName, }FKeyColumn, PKeyColumn, Targs: string;
  Action, KeySequence, Advance: Integer;
  List: TStrings;
  Deferrability: Integer;
  Deferrable, InitiallyDeferred: Boolean;
begin
  Result:=inherited UncachedGetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable,
                                              ForeignCatalog, ForeignSchema, ForeignTable);

  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 4) then
  begin
    SQL := 'SELECT '+
      'tc.constraint_catalog as PKTABLE_CAT, '+
      'tc.constraint_schema as PKTABLE_SCHEM, '+
      'ccu.table_name as PKTABLE_NAME, '+
      'ccu.column_name as PKCOLUMN_NAME, '+
      'kcu.table_catalog as FKTABLE_CAT, '+
      'kcu.constraint_schema as FKTABLE_SCHEM, '+
      'kcu.table_name as PKTABLE_NAME, '+
      'kcu.column_name as FKCOLUMN_NAME, '+
      'kcu.ordinal_position as KEY_SEQ, '+
      'rf.update_rule as UPDATE_RULE, '+
      'rf.delete_rule as DELETE_RULE, '+
      'kcu.constraint_name as FK_NAME, '+
      'rf.unique_constraint_name as PK_NAME, '+
      'tc.is_deferrable as DEFERRABILITY '+
      'FROM information_schema.table_constraints AS tc '+
      'JOIN information_schema.key_column_usage AS kcu ON tc.constraint_name = kcu.constraint_name '+
      'JOIN information_schema.constraint_column_usage AS ccu ON ccu.constraint_name = tc.constraint_name '+
      'join information_schema.referential_constraints as rf on rf.constraint_name = tc.constraint_name '+
      'WHERE constraint_type = ''FOREIGN KEY''';
    if PrimaryCatalog <> '' then
      SQL := SQL + ' and tc.constraint_catalog = '''+PrimaryCatalog+'''';
    if PrimarySchema <> '' then
      SQL := SQL + ' and tc.constraint_schema = '''+PrimarySchema+'''';
    if PrimaryTable <> '' then
      SQL := SQL + ' and ccu.table_name = '''+PrimaryTable+'''';
    if ForeignCatalog <> '' then
      SQL := SQL + ' and kcu.table_catalog = '''+ForeignCatalog+'''';
    if ForeignSchema <> '' then
      SQL := SQL + ' and kcu.constraint_schema = '''+ForeignSchema+'''';
    if ForeignTable <> '' then
      SQL := SQL + ' and kcu.table_name = '''+ForeignTable+'''';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        //Result.UpdatePAnsiChar(CrossRefKeyColPKTableCatalogIndex, GetPAnsiChar(tc_constraint_catalog_Index_74, Len), @Len);
        Result.UpdatePAnsiChar(CrossRefKeyColPKTableSchemaIndex, GetPAnsiChar(tc_constraint_schema_Index_74, Len), @Len);
        Result.UpdatePAnsiChar(CrossRefKeyColPKTableNameIndex, GetPAnsiChar(ccu_table_name_Index_74, Len), @Len);
        Result.UpdatePAnsiChar(CrossRefKeyColPKColumnNameIndex, GetPAnsiChar(ccu_column_name_Index_74, Len), @Len);
        //Result.UpdatePAnsiChar(CrossRefKeyColFKTableCatalogIndex, GetPAnsiChar(kcu_table_catalog_Index_74, Len), @Len);
        Result.UpdatePAnsiChar(CrossRefKeyColFKTableSchemaIndex, GetPAnsiChar(kcu_constraint_schema_Index_74, Len), @Len);
        Result.UpdatePAnsiChar(CrossRefKeyColFKTableNameIndex, GetPAnsiChar(kcu_table_name_Index_74, Len), @Len);
        Result.UpdatePAnsiChar(CrossRefKeyColFKColumnNameIndex, GetPAnsiChar(kcu_column_name_Index_74, Len), @Len);
        Result.UpdateSmall(CrossRefKeyColKeySeqIndex, GetSmall(kcu_ordinal_position_Index_74));
        Result.UpdateSmall(CrossRefKeyColUpdateRuleIndex, Ord(GetRuleType(GetString(rf_update_rule_Index_74))));
        Result.UpdateSmall(CrossRefKeyColDeleteRuleIndex, Ord(GetRuleType(GetString(rf_delete_rule_Index_74))));
        Result.UpdatePAnsiChar(CrossRefKeyColFKNameIndex, GetPAnsiChar(kcu_constraint_name_Index_74, Len), @Len);
        Result.UpdatePAnsiChar(CrossRefKeyColPKNameIndex, GetPAnsiChar(rf_unique_constraint_name_Index_74, Len), @Len);
        if GetString(tc_is_deferrable_Index_74) = 'NO' then
          Result.UpdateSmall(CrossRefKeyColDeferrabilityIndex, Ord(ikNotDeferrable))
        else
          Result.UpdateSmall(CrossRefKeyColDeferrabilityIndex, Ord(ikInitiallyDeferred));
        Result.InsertRow;
      end;
      Close;
    end;
  end
  else
  begin
    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      Select := 'SELECT DISTINCT n1.nspname as pnspname,n2.nspname as fnspname,';
      From := ' FROM pg_catalog.pg_namespace n1 JOIN pg_catalog.pg_class c1'
        + ' ON (c1.relnamespace = n1.oid) JOIN pg_catalog.pg_index i'
        + ' ON (c1.oid=i.indrelid) JOIN pg_catalog.pg_class ic'
        + ' ON (i.indexrelid=ic.oid) JOIN pg_catalog.pg_attribute a'
        + ' ON (ic.oid=a.attrelid), pg_catalog.pg_namespace n2'
        + ' JOIN pg_catalog.pg_class c2 ON (c2.relnamespace=n2.oid),'
        + ' pg_catalog.pg_trigger t1 JOIN pg_catalog.pg_proc p1'
        + ' ON (t1.tgfoid=p1.oid), pg_catalog.pg_trigger t2'
        + ' JOIN pg_catalog.pg_proc p2 ON (t2.tgfoid=p2.oid)';
      Where := '';
      if PrimarySchema <> ''then
      begin
        Where := Where + ' AND n1.nspname = '
          + EscapeString(PrimarySchema);
      end;
      if ForeignSchema <> '' then
      begin
        Where := Where + ' AND n2.nspname = '
          + EscapeString(ForeignSchema);
      end;
    end
    else
    begin
      Select := 'SELECT DISTINCT NULL::text as pnspname, NULL::text as fnspname,';
      From := ' FROM pg_class c1 JOIN pg_index i ON (c1.oid=i.indrelid)'
        + ' JOIN pg_class ic ON (i.indexrelid=ic.oid) JOIN pg_attribute a'
        + ' ON (ic.oid=a.attrelid), pg_class c2, pg_trigger t1'
        + ' JOIN pg_proc p1 ON (t1.tgfoid=p1.oid), pg_trigger t2'
        + ' JOIN pg_proc p2 ON (t2.tgfoid=p2.oid)';
    end;

    SQL := Select + ' c1.relname as prelname, c2.relname as frelname,'
      + ' t1.tgconstrname, a.attnum as keyseq, ic.relname as fkeyname,'
      + ' t1.tgdeferrable, t1.tginitdeferred, t1.tgnargs,t1.tgargs,'
      + ' p1.proname as updaterule, p2.proname as deleterule'
      + From
      + ' WHERE (t1.tgrelid=c1.oid AND t1.tgisconstraint'
      + ' AND t1.tgconstrrelid=c2.oid AND p1.proname'
      + ' LIKE ' + EscapeString('RI\_FKey\_%\_upd')
      + ') AND (t2.tgrelid=c1.oid'
      + ' AND t2.tgisconstraint AND t2.tgconstrrelid=c2.oid '
      + ' AND p2.proname LIKE ' + EscapeString('RI\_FKey\_%\_del')
      + ') AND i.indisprimary'
      + Where;
    if PrimaryTable <> '' then
      SQL := SQL + ' AND c1.relname=' + EscapeString(PrimaryTable);
    if ForeignTable <> '' then
      SQL := SQL + ' AND c2.relname=' + EscapeString(ForeignTable);
    SQL := SQL + ' ORDER BY ';

    if PrimaryTable <> '' then
    begin
      if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
        SQL := SQL + 'fnspname, ';
      SQL := SQL + 'frelname';
    end
    else
    begin
      if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
        SQL := SQL + 'pnspname, ';
      SQL := SQL + 'prelname';
    end;

    SQL := SQL + ', keyseq';

    List := TStringList.Create;
    try
      with GetConnection.CreateStatement.ExecuteQuery(SQL) do
      begin
        while Next do
        begin
          Result.MoveToInsertRow;
          Result.UpdatePAnsiChar(CrossRefKeyColPKTableSchemaIndex, GetPAnsiChar(pnspname_index, Len), @Len);
          Result.UpdatePAnsiChar(CrossRefKeyColFKTableSchemaIndex, GetPAnsiChar(fnspname_index, Len), @Len);
          Result.UpdatePAnsiChar(CrossRefKeyColPKTableNameIndex, GetPAnsiChar(prelname_index, Len), @Len);
          Result.UpdatePAnsiChar(CrossRefKeyColFKTableNameIndex, GetPAnsiChar(frelname_index, Len), @Len);

          //FKeyName := GetString(t1_tgconstrname_index);
          UpdateRule := GetString(updaterule_index);
          if UpdateRule <> '' then
          begin
            Rule := Copy(UpdateRule, 9, Length(UpdateRule) - 12);
            Action := Ord(ikNoAction);
            if (Rule = '') or (Rule = 'noaction') then
              Action := Ord(ikNoAction);
            if Rule = 'cascade' then
              Action := Ord(ikCascade);
            if Rule = 'setnull' then
              Action := Ord(ikSetNull);
            if Rule = 'setdefault' then
              Action := Ord(ikSetDefault);
            if Rule = 'restrict' then
             Action := Ord(ikRestrict);
            Result.UpdateInt(CrossRefKeyColUpdateRuleIndex, Action);
          end;

          DeleteRule := GetString(deleterule_index);
          if DeleteRule <> '' then
          begin
            Rule := Copy(DeleteRule, 9, Length(DeleteRule) - 12);
            Action := Ord(ikNoAction);
            if Rule = 'cascade' then
              Action := Ord(ikCascade);
            if Rule = 'setnull' then
              Action := Ord(ikSetNull);
            if Rule = 'setdefault' then
              Action := Ord(ikSetDefault);
            if Rule = 'restrict' then
              Action := Ord(ikRestrict);
            Result.UpdateInt(CrossRefKeyColDeleteRuleIndex, Action);
          end;

          KeySequence := GetInt(keyseq_index);
          Targs := GetString(t1_tgargs_index);

          //<unnamed>\000ww\000vv\000UNSPECIFIED\000m\000a\000n\000b\000
          //for Postgresql 7.3
          {%H-}//$1\000ww\000vv\000UNSPECIFIED\000m\000a\000n\000b\000
          {%H-}//$2\000ww\000vv\000UNSPECIFIED\000m\000a\000n\000b\000

          Advance := 4 + (KeySequence - 1) shl 1; //shl 1 = * 2 but faster
          PutSplitStringEx(List, Targs, '\000');

          if Advance <= List.Count-1 then
            FKeyColumn := List.Strings[Advance];
          if Advance + 1 <= List.Count-1 then
            PKeyColumn := List.Strings[Advance+1];
          Result.UpdateString(CrossRefKeyColPKColumnNameIndex, PKeyColumn);
          Result.UpdateString(CrossRefKeyColFKColumnNameIndex, FKeyColumn);
          Result.UpdateSmall(CrossRefKeyColKeySeqIndex, GetSmall(keyseq_index));

          if List.Strings[0] = '<unnamed>' then
            Result.UpdateString(CrossRefKeyColFKNameIndex, Targs) //FK_NAME
          else Result.UpdateString(CrossRefKeyColFKNameIndex, List.Strings[0]); //FK_NAME

          Result.UpdateString(CrossRefKeyColPKNameIndex, GetString(keyseq_index)); //EH: OLD CODE!!! This is wrong, i know!

          Deferrability := Ord(ikNotDeferrable);
          Deferrable := GetBoolean(t1_tgdeferrable_index);
          InitiallyDeferred := GetBoolean(t1_tginitdeferred_index);
          if Deferrable then
          begin
            if InitiallyDeferred then
              Deferrability := Ord(ikInitiallyDeferred)
            else Deferrability := Ord(ikInitiallyImmediate);
          end;
          Result.UpdateInt(CrossRefKeyColPKNameIndex, Deferrability);
          Result.InsertRow;
        end;
        Close;
      end;
    finally
      List.Free;
    end;
  end;
end;

{**
  Gets a description of all the standard SQL types supported by
  this database. They are ordered by DATA_TYPE and then by how
  closely the data type maps to the corresponding JDBC SQL type.

  <P>Each type description has the following columns:
   <OL>
 	<LI><B>TYPE_NAME</B> String => Type name
 	<LI><B>DATA_TYPE</B> short => SQL data type from java.sql.Types
 	<LI><B>PRECISION</B> int => maximum precision
 	<LI><B>LITERAL_PREFIX</B> String => prefix used to quote a literal
       (may be null)
 	<LI><B>LITERAL_SUFFIX</B> String => suffix used to quote a literal
        (may be null)
 	<LI><B>CREATE_PARAMS</B> String => parameters used in creating
       the type (may be null)
 	<LI><B>NULLABLE</B> short => can you use NULL for this type?
       <UL>
       <LI> typeNoNulls - does not allow NULL values
       <LI> typeNullable - allows NULL values
       <LI> typeNullableUnknown - nullability unknown
       </UL>
 	<LI><B>CASE_SENSITIVE</B> Boolean=> is it case sensitive?
 	<LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
 	<LI><B>UNSIGNED_ATTRIBUTE</B> Boolean => is it unsigned?
 	<LI><B>FIXED_PREC_SCALE</B> Boolean => can it be a money value?
 	<LI><B>AUTO_INCREMENT</B> Boolean => can it be used for an
       auto-increment value?
 	<LI><B>LOCAL_TYPE_NAME</B> String => localized version of type name
       (may be null)
 	<LI><B>MINIMUM_SCALE</B> short => minimum scale supported
 	<LI><B>MAXIMUM_SCALE</B> short => maximum scale supported
 	<LI><B>SQL_DATA_TYPE</B> int => unused
 	<LI><B>SQL_DATETIME_SUB</B> int => unused
 	<LI><B>NUM_PREC_RADIX</B> int => usually 2 or 10
   </OL>

  @return <code>ResultSet</code> - each row is an SQL type description
}
function TZPostgreSQLDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
var
  SQL: string;
  Len: NativeUInt;
begin
    Result:=inherited UncachedGetTypeInfo;

    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
      SQL := ' SELECT typname FROM pg_catalog.pg_type '
    else SQL := ' SELECT typname FROM pg_type ';

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePAnsiChar(TypeInfoTypeNameIndex, GetPAnsiChar(FirstDbcIndex, Len), @Len);
        Result.UpdateInt(TypeInfoDataTypeIndex, Ord(GetSQLTypeByName(GetString(FirstDbcIndex))));
        Result.UpdateInt(TypeInfoPecisionIndex, 9);
        Result.UpdateInt(TypeInfoNullAbleIndex, Ord(ntNoNulls));
        Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, False);
        Result.UpdateBoolean(TypeInfoSearchableIndex, False);
        Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, False);
        Result.UpdateBoolean(TypeInfoAutoIncrementIndex, False);
        Result.UpdateInt(TypeInfoNumPrecRadix, 10);
        Result.InsertRow;
      end;
      Close;
    end;
end;

{**
  Gets a description of a table's indices and statistics. They are
  ordered by NON_UNIQUE, TYPE, INDEX_NAME, and ORDINAL_POSITION.

  <P>Each index column description has the following columns:
   <OL>
 	<LI><B>TABLE_CAT</B> String => table catalog (may be null)
 	<LI><B>TABLE_SCHEM</B> String => table schema (may be null)
 	<LI><B>TABLE_NAME</B> String => table name
 	<LI><B>NON_UNIQUE</B> Boolean => Can index values be non-unique?
       false when TYPE is tableIndexStatistic
 	<LI><B>INDEX_QUALIFIER</B> String => index catalog (may be null);
       null when TYPE is tableIndexStatistic
 	<LI><B>INDEX_NAME</B> String => index name; null when TYPE is
       tableIndexStatistic
 	<LI><B>TYPE</B> short => index type:
       <UL>
       <LI> tableIndexStatistic - this identifies table statistics that are
            returned in conjuction with a table's index descriptions
       <LI> tableIndexClustered - this is a clustered index
       <LI> tableIndexHashed - this is a hashed index
       <LI> tableIndexOther - this is some other style of index
       </UL>
 	<LI><B>ORDINAL_POSITION</B> short => column sequence number
       within index; zero when TYPE is tableIndexStatistic
 	<LI><B>COLUMN_NAME</B> String => column name; null when TYPE is
       tableIndexStatistic
 	<LI><B>ASC_OR_DESC</B> String => column sort sequence, "A" => ascending,
       "D" => descending, may be null if sort sequence is not supported;
       null when TYPE is tableIndexStatistic
 	<LI><B>CARDINALITY</B> int => When TYPE is tableIndexStatistic, then
       this is the number of rows in the table; otherwise, it is the
       number of unique values in the index.
 	<LI><B>PAGES</B> int => When TYPE is  tableIndexStatisic then
       this is the number of pages used for the table, otherwise it
       is the number of pages used for the current index.
 	<LI><B>FILTER_CONDITION</B> String => Filter condition, if any.
       (may be null)
   </OL>

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" retrieves those without a schema
  @param table a table name
  @param unique when true, return only indices for unique values;
      when false, return indices regardless of whether unique or not
  @param approximate when true, result is allowed to reflect approximate
      or out of data values; when false, results are requested to be
      accurate
  @return <code>ResultSet</code> - each row is an index column description
}
function TZPostgreSQLDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  SQL, Select, From, Where: string;
begin
    if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
    begin
      Select := 'SELECT NULL AS TABLE_CAT, n.nspname AS TABLE_SCHEM,';
      From := ' FROM pg_catalog.pg_namespace n, pg_catalog.pg_class ct,'
        + ' pg_catalog.pg_class ci, pg_catalog.pg_index i,'
        + ' pg_catalog.pg_attribute a, pg_catalog.pg_am am';
      Where := ' AND n.oid = ct.relnamespace';
      if Schema <> '' then
        Where := Where + ' AND n.nspname = ' + EscapeString(Schema);
    end
    else
    begin
      Select := 'SELECT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM,';
      From := ' FROM pg_class ct, pg_class ci, pg_index i, pg_attribute a,'
        + ' pg_am am';
    end;

    SQL := Select + ' ct.relname AS TABLE_NAME, NOT i.indisunique'
      + ' AS NON_UNIQUE, NULL AS INDEX_QUALIFIER, ci.relname AS INDEX_NAME,'
      + ' CASE i.indisclustered WHEN true THEN ' + ZFastCode.IntToStr(Ord(tiClustered))
      + ' ELSE CASE am.amname WHEN ''hash'' THEN ' + ZFastCode.IntToStr(Ord(tiHashed))
      + ' ELSE ' + ZFastCode.IntToStr(Ord(tiOther)) + ' END END AS TYPE,'
      + ' a.attnum AS ORDINAL_POSITION, a.attname AS COLUMN_NAME,'
      + ' NULL AS ASC_OR_DESC, ci.reltuples AS CARDINALITY,'
      + ' ci.relpages AS PAGES, NULL AS FILTER_CONDITION'
      + From
      + ' WHERE ct.oid=i.indrelid AND ci.oid=i.indexrelid'
      + ' AND a.attrelid=ci.oid AND ci.relam=am.oid' + Where
      + ' AND ct.relname = ' + EscapeString(Table);

    if Unique then
      SQL := SQL + ' AND i.indisunique';
    SQL := SQL + ' ORDER BY NON_UNIQUE, TYPE, INDEX_NAME, ORDINAL_POSITION';

    Result := CopyToVirtualResultSet(
      GetConnection.CreateStatement.ExecuteQuery(SQL),
      ConstructVirtualResultSet(IndexInfoColumnsDynArray));
end;

function TZPostgreSQLDatabaseMetadata.UncachedGetSequences(const Catalog, SchemaPattern,
  SequenceNamePattern: string): IZResultSet;
var
  SQL: string;
begin
    Result:=inherited UncachedGetSequences(Catalog, SchemaPattern, SequenceNamePattern);

    SQL := ' SELECT nspname, relname ' +
      'FROM pg_catalog.pg_namespace n, pg_catalog.pg_class ct ' +
      'WHERE relkind = ''S'' ' +
      'AND n.oid = ct.relnamespace';

    if SequenceNamePattern <> '' then
      SQL := SQL + ' AND ' + Format('relname = ''%s''', [SequenceNamePattern]);
    if SchemaPattern <> '' then
      SQL := SQL + ' AND ' + Format('nspname = ''%s''', [SchemaPattern]);

    with GetConnection.CreateStatement.ExecuteQuery(SQL) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdateNull(CatalogNameIndex);
        Result.UpdateString(SchemaNameIndex, GetStringByName('nspname'));
        Result.UpdateString(TableNameIndex, GetStringByName('relname'));
        Result.InsertRow;
      end;
      Close;
    end;
end;

function TZPostgreSQLDatabaseMetadata.GetPostgreSQLType(Oid: OID): string;
begin
  Result := (GetConnection as IZPostgreSQLConnection).GetTypeNameByOid(Oid);
end;

function TZPostgreSQLDatabaseMetadata.GetSQLTypeByOid(Oid: OID): TZSQLType;
var
  PostgreSQLConnection: IZPostgreSQLConnection;
begin
  PostgreSQLConnection := GetConnection as IZPostgreSQLConnection;
  Result := PostgreSQLToSQLType(PostgreSQLConnection,
    PostgreSQLConnection.GetTypeNameByOid(Oid));
end;

function TZPostgreSQLDatabaseMetadata.InternalUncachedGetColumns(const Catalog,
  SchemaPattern, TableNamePattern, ColumnNamePattern,
  TableOID: string): IZResultSet;
const
  nspname_index     = FirstDbcIndex + 0;
  relname_index     = FirstDbcIndex + 1;
  attname_index     = FirstDbcIndex + 2;
  atttypid_index    = FirstDbcIndex + 3;
  attnotnull_index  = FirstDbcIndex + 4;
  atttypmod_index   = FirstDbcIndex + 5;
  attlen_index      = FirstDbcIndex + 6;
  attnum_index      = FirstDbcIndex + 7;
  adsrc_index       = FirstDbcIndex + 8;
  description_index = FirstDbcIndex + 9;
  cnspname_index    = FirstDbcIndex + 10;
  cdomain_oid_Index = FirstDbcIndex + 11;
var
  Len: NativeUInt;
  TypeOid: Cardinal;
  AttTypMod, Precision: Integer;
  SQL, PgType: string;
  SQLType: TZSQLType;
  CheckVisibility: Boolean;
  ColumnNameCondition, TableNameCondition, SchemaCondition, CatalogCondition: string;
  PGConnection: IZPostgreSQLConnection;
label FillSizes;
begin
  //http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=11174
  CheckVisibility := (GetConnection as IZPostgreSQLConnection).CheckFieldVisibility;
  if TableOID = '' then begin
    CatalogCondition := ConstructNameCondition(Catalog,'dn.relname');
    SchemaCondition := ConstructNameCondition(SchemaPattern,'n.nspname');
    TableNameCondition := ConstructNameCondition(TableNamePattern,'c.relname');
    ColumnNameCondition := ConstructNameCondition(ColumnNamePattern,'a.attname');
  end;
  Result:=inherited UncachedGetColumns(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);

  if (GetDatabaseInfo as IZPostgreDBInfo).HasMinimumServerVersion(7, 3) then
  begin
    SQL := 'SELECT n.nspname,' {nspname_index}
      + 'c.relname,' {relname_index}
      //+ 'case t.typtype when ''d'' then t.typname else a.attname end as attname,' {attname_index}
      + 'a.attname,' {attname_index}
      + 'case t.typtype when ''d'' then t.typbasetype else t.oid end as atttypid,' {atttypid_index}
      + 'case t.typtype when ''d'' then t.typnotnull else a.attnotnull end as attnotnull,' {attnotnull_index}
      + 'case t.typtype when ''d'' then t.typtypmod else a.atttypmod end as atttypmod,' {atttypmod_index}
      + 'case t.typtype when ''d'' then t.typlen else a.attlen end as attlen,' {attlen_index}
      + 'a.attnum,' {attnum_index}
      + 'pg_get_expr(def.adbin, def.adrelid) as adsrc,' {adsrc_index}
      + 'dsc.description, ' {description_index}
      + 'dn.nspname as cnspname, ' {cnspname_index}
      + 'case t.typtype when ''d'' then t.oid else null end as domain_oid'
      + ' FROM pg_catalog.pg_namespace n '
      + ' JOIN pg_catalog.pg_class c ON (c.relnamespace = n.oid) '
      + ' JOIN pg_catalog.pg_attribute a ON (a.attrelid=c.oid) '
      + ' JOIN pg_catalog.pg_type t ON (t.oid = a.atttypid)'
      + ' LEFT JOIN pg_catalog.pg_attrdef def ON (a.attrelid=def.adrelid AND a.attnum = def.adnum)'
      + ' LEFT JOIN pg_catalog.pg_description dsc ON (c.oid=dsc.objoid AND a.attnum = dsc.objsubid) '
      + ' LEFT JOIN pg_catalog.pg_class dc ON (dc.oid=dsc.classoid AND dc.relname=''pg_class'') '
      + ' LEFT JOIN pg_catalog.pg_namespace dn ON (dc.relnamespace=dn.oid AND dn.nspname=''pg_catalog'') ';
    if TableOID <> '' then
      SQL := SQL + ' WHERE a.attnum > 0 AND c.oid = '+TableOID
    else begin
        SQL := SQL + ' WHERE a.attnum > 0 AND NOT a.attisdropped';
      if Catalog <> '' then
        SQL := SQL + ' AND ' + CatalogCondition;
      if SchemaPattern <> '' then
        SQL := SQL + ' AND ' + SchemaCondition;
      //not by default: because of Speed decrease: http://http://zeoslib.sourceforge.net/viewtopic.php?p=16646&sid=130
      if CheckVisibility then
        SQL := SQL + ' AND pg_table_is_visible (c.oid) ';
    end;
  end
  else
  begin
    SQL := 'SELECT NULL::text AS nspname,' {nspname_index}
      + 'c.relname,' {relname_index}
      + 'a.attname,' {attname_index}
      + 'a.atttypid,' {atttypid_index}
      + 'a.attnotnull,' {attnotnull_index}
      + 'a.atttypmod,' {atttypmod_index}
      + 'a.attlen,' {attlen_index}
      + 'a.attnum,' {attnum_index}
      + 'NULL AS adsrc,' {adsrc_index}
      + 'NULL AS description, ' {description_index}
      + 'NULL::text AS cnspname' {cnspname_index}
      + 'NULL::OID as domain_oid'
      + 'FROM pg_class c, pg_attribute a';
    if TableOID <> '' then
      SQL := SQL + ' WHERE c.oid = '+TableOID
    else
      SQL := SQL + ' WHERE a.attrelid=c.oid AND a.attnum > 0 ';
  end;

  if TableOID = '' then begin
    If TableNameCondition <> '' then
      SQL := SQL + ' AND ' + TableNameCondition;
    If ColumnNameCondition <> '' then
      SQL := SQL+ ' AND ' + ColumnNameCondition;
  end;
  SQL := SQL+ ' ORDER BY nspname,relname,attnum';
  GetConnection.QueryInterface(IZPostgreSQLConnection,PGConnection);
  with PGConnection.CreateStatement.ExecuteQuery(SQL) do begin
    while Next do begin
      AttTypMod := GetInt(atttypmod_index);

      TypeOid := GetUInt(atttypid_index);
      PgType := GetPostgreSQLType(TypeOid);

      Result.MoveToInsertRow;
      if not IsNull(cnspname_index) then
        Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(cnspname_index, Len), @Len);
      if not IsNull(nspname_index) then
        Result.UpdatePAnsiChar(SchemaNameIndex, GetPAnsiChar(nspname_index, Len), @Len);
      Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(relname_index, Len), @Len);
      Result.UpdatePAnsiChar(ColumnNameIndex, GetPAnsiChar(attname_index, Len), @Len);
      SQLType := GetSQLTypeByOid(TypeOid);
      Result.UpdateInt(TableColColumnTypeIndex, Ord(SQLType));
      Result.UpdateString(TableColColumnTypeNameIndex, PgType);

      Result.UpdateInt(TableColColumnBufLengthIndex, 0);

      if (PgType = 'bpchar') or (PgType = 'varchar') or (PgType = 'enum') then
      begin
        if AttTypMod <> -1 then begin
          Precision := AttTypMod - 4;
FillSizes:Result.UpdateInt(TableColColumnSizeIndex, Precision);
          if SQLType = stString then begin
            Result.UpdateInt(TableColColumnBufLengthIndex, Precision * ConSettings^.ClientCodePage^.CharWidth +1);
            Result.UpdateInt(TableColColumnCharOctetLengthIndex, Precision * ConSettings^.ClientCodePage^.CharWidth);
          end else if SQLType = stUnicodeString then begin
            Result.UpdateInt(TableColColumnBufLengthIndex, (Precision+1) shl 1);
            Result.UpdateInt(TableColColumnCharOctetLengthIndex, Precision shl 1);
          end;
        end else
          if (PgType = 'varchar') then
            if ( (GetConnection as IZPostgreSQLConnection).GetUndefinedVarcharAsStringLength = 0 ) then
            begin
              Result.UpdateInt(TableColColumnTypeIndex, Ord(GetSQLTypeByOid(25))); //Assume text-lob instead
              Result.UpdateInt(TableColColumnSizeIndex, 0); // need no size for streams
            end
            else begin //keep the string type but with user defined count of chars
              Precision := (GetConnection as IZPostgreSQLConnection).GetUndefinedVarcharAsStringLength;
              goto FillSizes;
            end
          else
            Result.UpdateInt(TableColColumnSizeIndex, 0);
      end
      else if (PgType = 'uuid') then
      begin
        // I set break point and see code reaching here. Below assignments, I have no idea what I am doing.
        Result.UpdateInt(TableColColumnBufLengthIndex, 16); // MSSQL returns 16 here - which makes sense since a GUID is 16 bytes long.
        // TableColColumnCharOctetLengthIndex is removed - PG returns 0 and in the dblib driver 0 is also used, although MSSQL returns null...
      end
      else if (PgType = 'numeric') or (PgType = 'decimal') then
      begin
        Result.UpdateInt(TableColColumnSizeIndex, ((AttTypMod - 4) div 65536)); //precision
        Result.UpdateInt(TableColColumnDecimalDigitsIndex, ((AttTypMod -4) mod 65536)); //scale
        Result.UpdateInt(TableColColumnNumPrecRadixIndex, 10); //base? ten as default
      end else if (TypeOID = CASHOID) then begin
        Result.UpdateInt(TableColColumnSizeIndex, 22); //precision
        Result.UpdateInt(TableColColumnDecimalDigitsIndex, 2); //scale
        Result.UpdateInt(TableColColumnNumPrecRadixIndex, 10); //base? ten as default
      end else if (PgType = 'bit') or (PgType = 'varbit') then begin
        Result.UpdateInt(TableColColumnSizeIndex, AttTypMod);
        Result.UpdateInt(TableColColumnNumPrecRadixIndex, 2);
      end
      else
      begin
        Result.UpdateInt(TableColColumnSizeIndex, GetInt(attlen_index));
        Result.UpdateInt(TableColColumnNumPrecRadixIndex, 2);
      end;
      if GetBoolean(attnotnull_index) then
      begin
        Result.UpdateString(TableColColumnIsNullableIndex, 'NO');
        Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNoNulls));
      end
      else
      begin
        Result.UpdateString(TableColColumnIsNullableIndex, 'YES');
        Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNullable));
      end;

      Result.UpdatePAnsiChar(TableColColumnRemarksIndex, GetPAnsiChar(description_index {description}, Len), @Len);
      Result.UpdatePAnsiChar(TableColColumnColDefIndex, GetPAnsiChar(adsrc_index {adsrc}, Len), @Len);
      Result.UpdateInt(TableColColumnCharOctetLengthIndex, Result.GetInt(attlen_index));
      Result.UpdateInt(TableColColumnOrdPosIndex, GetInt(attnum_index));

      Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, IC.IsCaseSensitive(GetString(attname_index)));
      Result.UpdateBoolean(TableColColumnSearchableIndex, True);
      Result.UpdateBoolean(TableColColumnWritableIndex, True);
      Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, True);
      Result.UpdateBoolean(TableColColumnReadonlyIndex, False);
      if not IsNull(cdomain_oid_Index) then
        PGConnection.AddDomain2BaseTypeIfNotExists(GetUInt(cdomain_oid_Index), TypeOid);
      Result.InsertRow;
    end;
    Close;
  end;
end;

function TZPostgreSQLDatabaseMetadata.GetSQLTypeByName(
  const TypeName: string): TZSQLType;
begin
  Result := PostgreSQLToSQLType(
    GetConnection as IZPostgreSQLConnection, TypeName);
end;

function TZPostgreSQLDatabaseMetadata.TableTypeSQLExpression(
  const TableType: string; UseSchemas: Boolean): string;
begin
  if UseSchemas then
  begin
    if TableType = 'TABLE' then
      Result := ' c.relkind = ''r'' AND n.nspname NOT LIKE ''pg\\_%'' '
    else if TableType = 'VIEW' then
      Result := ' c.relkind = ''v'' AND n.nspname <> ''pg_catalog'' '
    else if TableType = 'INDEX' then
      Result := ' c.relkind = ''i'' AND n.nspname NOT LIKE ''pg\\_%'' '
    else if TableType = 'SEQUENCE' then
      Result := ' c.relkind = ''S'' '
    else if TableType = 'SYSTEM TABLE' then
      Result := ' c.relkind = ''r'' AND n.nspname = ''pg_catalog'' '
    else if TableType = 'SYSTEM TOAST TABLE' then
      Result := ' c.relkind = ''r'' AND n.nspname = ''pg_toast'' '
    else if TableType = 'SYSTEM TOAST INDEX' then
      Result := ' c.relkind = ''i'' AND n.nspname = ''pg_toast'' '
    else if TableType = 'SYSTEM VIEW' then
      Result := ' c.relkind = ''v'' AND n.nspname = ''pg_catalog'' '
    else if TableType = 'SYSTEM INDEX' then
      Result := ' c.relkind = ''i'' AND n.nspname = ''pg_catalog'' '
    else if TableType = 'TEMPORARY TABLE' then
      Result := ' c.relkind = ''r'' AND n.nspname LIKE ''pg\\_temp\\_%'' '
    else if TableType = 'TEMPORARY INDEX' then
      Result := 'c.relkind = ''i'' AND n.nspname LIKE ''pg\\_temp\\_%'' ';
  end
  else
  begin
    if TableType = 'TABLE' then
      Result := ' c.relkind = ''r'' AND c.relname NOT LIKE ''pg\\_%'' '
    else if TableType = 'VIEW' then
      Result := ' c.relkind = ''v'' AND c.relname NOT LIKE ''pg\\_%'' '
    else if TableType = 'INDEX' then
      Result := ' c.relkind = ''i'' AND c.relname NOT LIKE ''pg\\_%'' '
    else if TableType = 'SEQUENCE' then
      Result := ' c.relkind = ''S'' '
    else if TableType = 'SYSTEM TABLE' then
      Result := ' c.relkind = ''r'' AND c.relname LIKE ''pg\\_%'' AND c.relname '+
        'NOT LIKE ''pg\\_toast\\_%'' AND c.relname NOT LIKE ''pg\\_temp\\_%'' '
    else if TableType = 'SYSTEM TOAST TABLE' then
      Result := ' c.relkind = ''r'' AND c.relname LIKE ''pg\\_toast\\_%'' '
    else if TableType = 'SYSTEM TOAST INDEX' then
      Result := ' c.relkind = ''i'' AND c.relname LIKE ''pg\\_toast\\_%'' '
    else if TableType = 'SYSTEM VIEW' then
      Result := 'c.relkind = ''v'' AND c.relname LIKE ''pg\\_%'''
    else if TableType = 'SYSTEM INDEX' then
    begin
      Result := ' c.relkind = ''v'' AND c.relname LIKE ''pg\\_%'' AND '+
        'c.relname NOT LIKE ''pg\\_toast\\_%'' AND c.relname '+
        'NOT LIKE ''pg\\_temp\\_%'' '
    end
    else if TableType = 'TEMPORARY TABLE' then
      Result := ' c.relkind = ''r'' AND c.relname LIKE ''pg\\_temp\\_%'' '
    else if TableType = 'TEMPORARY INDEX' then
      Result := ' c.relkind = ''i'' AND c.relname LIKE ''pg\\_temp\\_%'' '
  end;
end;

procedure TZPostgreSQLDatabaseMetadata.ParseACLArray(
  List: TStrings; const AclString: string);
var
  PrevChar: Char;
  InQuotes: Boolean;
  I, BeginIndex: Integer;
  P: PChar;
begin
  if AclString = '' then Exit;
  InQuotes := False;
  PrevChar := ' ';
  BeginIndex := 2;
  P := Pointer(AclString);
  for I := BeginIndex to Length(AclString) do begin
    Inc(P);
    if (P^ = '"') and (PrevChar <> '\' )
    then InQuotes := not InQuotes
    else if (P^ = ',') and not InQuotes then begin
      List.Add(Copy(AclString, BeginIndex, I - BeginIndex));
      BeginIndex := I+1;
    end;
    PrevChar := P^;
  end;

  // add last element removing the trailing "}"
  List.Add(Copy(AclString, BeginIndex, Length(AclString) - BeginIndex));

  // Strip out enclosing quotes, if any.
  for I := 0 to List.Count-1 do begin
    P := Pointer(List.Strings[i]);
    if (P^ = '"') and ((P+Length(List.Strings[i])-1)^ = '"') then
      List.Strings[i] := Copy(List.Strings[i], 2, Length(List.Strings[i])-2);
  end;
end;

function TZPostgreSQLDatabaseMetadata.GetPrivilegeName(Permission: Char): string;
begin
 case Permission of
   'a': Result := 'INSERT';
   'r': Result := 'SELECT';
   'w': Result := 'UPDATE';
   'd': Result := 'DELETE';
   'R': Result := 'RULE';
   'x': Result := 'REFERENCES';
   't': Result := 'TRIGGER';
   'X': Result := 'EXECUTE';
   'U': Result := 'USAGE';
   'C': Result := 'CREATE';
   'T': Result := 'CREATE TEMP';
   else Result := 'UNKNOWN';
 end;
end;

function TZPostgreSQLDatabaseMetadata.GetColumnsByTableOID(
  Value: OID): IZResultSet;
var I: Integer;
  Key: String;
begin
  Result := nil;
  for i := low(fZPGTableOIDArray) to high(fZPGTableOIDArray) do
    with fZPGTableOIDArray[i] do
      if OID = Value then begin
        Result := ColumnRS;
        Break;
      end;
  if Result = nil then begin
    Result := InternalUncachedGetColumns('', '', '', '', ZFastCode.{$IFDEF UNICODE}IntToUnicode{$ELSE}IntToRaw{$ENDIF}(Value));
    if Result.Next then begin
      Key := GetColumnsCacheKey(
        Result.GetString(CatalogNameIndex), Result.GetString(SchemaNameIndex),
        AddEscapeCharToWildcards(IC.Quote(Result.GetString(TableNameIndex))),''); //use same analogy for key_gen as done in the RS_Metadat
      Result.BeforeFirst;
      if not HasKey(Key) then
        AddResultSetToCache(Key, Result); // so others may use the result too
    end;
    SetLength(fZPGTableOIDArray, Length(fZPGTableOIDArray)+1);
    with fZPGTableOIDArray[High(fZPGTableOIDArray)] do begin
      OID := Value;
      ColumnRS := Result;
    end;
  end;
end;

function TZPostgreSQLDatabaseMetadata.GetIdentifierConvertor: IZIdentifierConvertor;
begin
  Result := TZDefaultIdentifierConvertor.Create(Self);
  //Result:= TZPostgreSQLIdentifierConvertor.Create(Self);
end;

{**
  Gets the all supported CharacterSets:
  @return <code>ResultSet</code> - each row is a CharacterSetName and it's ID
}
function TZPostgreSQLDatabaseMetadata.UncachedGetCharacterSets: IZResultSet; //EgonHugeist
const
  enc_Index  = FirstDbcIndex + 0;
  name_Index = FirstDbcIndex + 1;
var Len: NativeUInt;
begin
  Self.GetConnection.CreateStatement.Execute(
  ' CREATE OR REPLACE FUNCTION get_encodings() RETURNS INTEGER AS '''+
  ' DECLARE '+
  '   enc     INTEGER := 0; '+
  '   name    VARCHAR; '+
  ' BEGIN '+
  '   CREATE TEMP TABLE encodings ( enc_code int, enc_name text ); '+
  '   LOOP '+
  '       SELECT INTO name pg_encoding_to_char( enc ); '+
  '       IF( name = '''''''' ) THEN '+
  '           EXIT; '+
  '       ELSE '+
  '           INSERT INTO encodings VALUES( enc, name ); '+
  '        END IF; '+
  '       enc := enc + 1; '+
  '   END LOOP; '+
  '   RETURN enc; '+
  ' END; '+
  ''' LANGUAGE ''plpgsql'';');
  Self.GetConnection.CreateStatement.ExecuteQuery('select get_encodings();').Close;

  Result:=inherited UncachedGetCharacterSets;

  with Self.GetConnection.CreateStatement.ExecuteQuery(
   'select * from encodings;') do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdatePAnsiChar(CharacterSetsNameIndex, GetPAnsiChar(name_Index, Len), @Len); //CHARACTER_SET_NAME
      Result.UpdateSmall(CharacterSetsIDIndex, GetSmall(enc_Index)); //CHARACTER_SET_ID
      Result.InsertRow;
    end;
    CLose;
  end;
end;

{ TZPostgresIdentifierConvertor }

function TZPostgreSQLIdentifierConvertor.ExtractQuote(
  const Value: string): string;
var
  QuoteDelim: string;
  P: PChar absolute QuoteDelim;
begin
  if IsQuoted(Value) then begin
    QuoteDelim := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;
    case Length(QuoteDelim) of
      1: Result := SQLDequotedStr(Value, P^);
      2: Result := SQLDequotedStr(Value, P^, (P+1)^);
      else Result := Value;
    end;
  end else
    Result := AnsiLowerCase(Value);
end;

function TZPostgreSQLIdentifierConvertor.IsQuoted(const Value: string): Boolean;
var
  QuoteDelim: string;
  pQ, pV: PChar;
begin
  QuoteDelim := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;
  pQ := Pointer(QuoteDelim);
  pV := Pointer(Value);
  Result := (pQ <> nil) and (pV <> nil) and (pQ^ = pV^) and
            ((pV+Length(Value)-1)^ = (pQ+Length(QuoteDelim)-1)^);
end;

function TZPostgreSQLIdentifierConvertor.IsSpecialCase(
  const Value: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  if not CharInSet(Value[1], ['a'..'z','_']) then
  begin
    Result := True;
    Exit;
  end;
  for I := 1 to Length(Value) do
  begin
    if not CharInSet(Value[I], ['A'..'Z','a'..'z','0'..'9','_']) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TZPostgreSQLIdentifierConvertor.Quote(const Value: string): string;
var
  QuoteDelim: string;
  P: PChar absolute QuoteDelim;
begin
  Result := Value;
  if IsCaseSensitive(Value) then begin
    QuoteDelim := Metadata.GetDatabaseInfo.GetIdentifierQuoteString;
    case Length(QuoteDelim) of
      0: Result := Value;
      1: Result := SQLQuotedStr(Value, P^);
      2: Result := SQLQuotedStr(Value, P^, (P+1)^);
      else Result := Value;
    end;
  end;
end;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.
