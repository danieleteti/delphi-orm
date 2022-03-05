{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               Ado metadata information                  }
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

unit ZDbcAdoMetadata;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ADO}
uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZSysUtils, ZDbcIntfs, ZDbcMetadata, ZDbcResultSet, ZURL,
  ZCompatibility, ZGenericSqlAnalyser, ZPlainAdo, ZDbcConnection,
  {$IFDEF FPC}ZOleDB{$ELSE}OleDB{$ENDIF}, ActiveX;

type
  ULONG = LongWord;
  IZOleDBDatabaseInfo = interface(IZDatabaseInfo)
    ['{FCAE90AA-B0B6-49A2-AB74-33E604FF8804}']
    procedure InitilizePropertiesFromDBInfo(const DBInitialize: IDBInitialize; const Malloc: IMalloc);
  end;

  // technobot 2008-06-27 - methods moved as is from TZAdoDatabaseMetadata:
  {** Implements Ado Database Information. }
  TZAdoDatabaseInfo = class(TZAbstractDatabaseInfo, IZOleDBDatabaseInfo)
  private
    fDBPROP_MULTIPLEPARAMSETS: Boolean;
    fDBPROP_PROVIDERFRIENDLYNAME: String;
    fDBPROP_PROVIDERVER: String;
    fDBPROP_DBMSNAME: String;
    fDBPROP_DBMSVER: string;
    fSupportedTransactIsolationLevels: set of TZTransactIsolationLevel;
    fSupportsMultipleResultSets: Boolean;
    fSupportsMultipleStorageObjects: Boolean;
    fDBPROP_CATALOGUSAGE: Integer;
    fDBPROP_SCHEMAUSAGE: Integer;
    fDBPROP_IDENTIFIERCASE: Integer;
    fDBPROP_QUOTEDIDENTIFIERCASE: Integer;
    fDBPROP_MAXROWSIZE: Integer;
    fDBPROP_MAXROWSIZEINCLUDESBLOB: Boolean;
    fDBPROP_SQLSUPPORT: Integer;
    fDBPROP_CATALOGTERM: String;
    fDBPROP_SCHEMATERM: String;
    fDBPROP_PROCEDURETERM: String;
    fDBPROP_SUPPORTEDTXNDDL: Integer;
    fDBPROP_CONCATNULLBEHAVIOR: Boolean;
    fDBPROP_NULLCOLLATION: Integer;
    fDBPROP_SUBQUERIES: Integer;
    fDBPROP_GROUPBY: Integer;
    fDBPROP_ORDERBYCOLUMNSINSELECT: Boolean;
    fDBPROP_PREPAREABORTBEHAVIOR: Integer;
    fDBPROP_PREPARECOMMITBEHAVIOR: Integer;
  public
    constructor Create(const Metadata: TZAbstractDatabaseMetadata);
    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
    function GetDriverVersion: string; override;
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
    function GetServerVersion: string; override;

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
    function SupportsMultipleResultSets: Boolean; override;
    function SupportsMultipleStorageObjects: Boolean;
//    function SupportsMultipleTransactions: Boolean; override; -> Not implemented
//    function SupportsNonNullableColumns: Boolean; override; -> Not implemented
    function SupportsMinimumSQLGrammar: Boolean; override;
    function SupportsCoreSQLGrammar: Boolean; override;
    function SupportsExtendedSQLGrammar: Boolean; override;
    function SupportsANSI92EntryLevelSQL: Boolean; override;
    function SupportsANSI92IntermediateSQL: Boolean; override;
    function SupportsANSI92FullSQL: Boolean; override;
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
    function SupportsOverloadPrefixInStoredProcedureName: Boolean; override;
    function SupportsParameterBinding: Boolean; override;
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
    function SupportsTransactionIsolationLevel(const {%H-}Level: TZTransactIsolationLevel):
      Boolean; override;
    function SupportsDataDefinitionAndDataManipulationTransactions: Boolean; override;
    function SupportsDataManipulationTransactionsOnly: Boolean; override;
    function SupportsResultSetType(const {%H-}_Type: TZResultSetType): Boolean; override;
    function SupportsResultSetConcurrency(const {%H-}_Type: TZResultSetType;
      const {%H-}Concurrency: TZResultSetConcurrency): Boolean; override;
//    function SupportsBatchUpdates: Boolean; override; -> Not implemented
    function SupportsNonEscapedSearchStrings: Boolean; override;
    function SupportsUpdateAutoIncrementFields: Boolean; override;
    function SupportsArrayBindings: Boolean; override;

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
    function NullsAreSortedHigh: Boolean; override;
    function NullsAreSortedLow: Boolean; override;
    function NullsAreSortedAtStart: Boolean; override;
    function NullsAreSortedAtEnd: Boolean; override;
    function NullPlusNonNullIsNull: Boolean; override;
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

    //Ole related
    procedure InitilizePropertiesFromDBInfo(const DBInitialize: IDBInitialize; const Malloc: IMalloc);
  end;

  {** Implements Ado Metadata. }
  TZAdoDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    FAdoConnection: ZPlainAdo.Connection;
    FSupportedSchemasInitialized: Boolean;
    function AdoOpenSchema(Schema: Integer; const Args: array of const): ZPlainAdo.RecordSet;
    procedure InitializeSchemas;
    function SchemaSupported(SchemaId: Integer): Boolean; // (technobot) should be moved to TZAdoDatabaseInfo?
    function FindSchema(SchemaId: Integer): Integer;
    function BuildRestrictions(SchemaId: Integer; const Args: array of const): Variant;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-27

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
    function UncachedGetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
//     function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
//      const SequenceNamePattern: string): IZResultSet; virtual; -> Not implemented
    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; override;
    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;
  public
    constructor Create(Connection: TZAbstractConnection; const Url: TZURL); override;
//    function GetTokenizer: IZTokenizer; override;
  end;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  Variants,
  Math, ZGenericSqlToken, ZDbcAdoUtils, ZDbcAdo, ZFastCode,
  {$IFDEF WITH_UNIT_NAMESPACES}System.Win.ComObj{$ELSE}ComObj{$ENDIF},
  ZDbcAdoResultSet;

type
  TSuppSchemaRec = record
    SchemaGuid: TGuid;
    SupportedRestrictions: Integer;
    AdoSchemaId: Integer;
  end;

  IDBSchemaRowset = interface(IUnknown)
    ['{0c733a7b-2a1c-11ce-ade5-00aa0044773d}']
    function GetRowset(
            const pUnkOuter : IUnknown;
            const rguidSchema : TGUID;
            cRestrictions : Integer;
            var rgRestrictions : PVariant;{!!was: const VARIANT __RPC_FAR rgRestrictions[  ],}
            const riid : IUnknown;
            cPropertySets : Integer;
            var rgPropertySets : TDBPROPSET;
            var ppRowset : IUnknown) : HResult; stdcall;
      function GetSchemas(
            var pcSchemas : Integer;
            var prgSchemas : PGUID;
            var prgRestrictionSupport : PInteger) : HResult; stdcall;
    end;

var
  SupportedSchemas: array of TSuppSchemaRec;

{ TZAdoDatabaseInfo }

{**
  Constructs this object.
  @param Metadata the interface of the correpsonding database metadata object
  @param IdentifierQuotes the default Quotes for Identifiers used by the driver
}
constructor TZAdoDatabaseInfo.Create(const Metadata: TZAbstractDatabaseMetadata);
begin
  inherited Create(MetaData, '[]');
end;

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZAdoDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := fDBPROP_DBMSNAME;
end;

{**
  What's the version of this database product?
  @return database version
}
function TZAdoDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := fDBPROP_DBMSVER;
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZAdoDatabaseInfo.GetDriverName: string;
begin
  Result := fDBPROP_PROVIDERFRIENDLYNAME;
end;

{**
  What's the version of this JDBC driver?
  @return JDBC driver version
}
function TZAdoDatabaseInfo.GetDriverVersion: string;
begin
  Result := fDBPROP_PROVIDERVER;
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZAdoDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := ZFastCode.{$IFDEF UNICODE}UnicodeToInt{$ELSE}RawToInt{$ENDIF}(Copy(fDBPROP_PROVIDERVER, 1, 2));
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZAdoDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := ZFastCode.{$IFDEF UNICODE}UnicodeToInt{$ELSE}RawToInt{$ENDIF}(Copy(fDBPROP_PROVIDERVER, 4, 2));
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZAdoDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Is the ODBC Minimum SQL grammar supported?
  All JDBC Compliant<sup><font size=-2>TM</font></sup> drivers must return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsMinimumSQLGrammar: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQL_ODBC_MINIMUM = DBPROPVAL_SQL_ODBC_MINIMUM;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will
  always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := fDBPROP_IDENTIFIERCASE = DBPROPVAL_IC_SENSITIVE;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := fDBPROP_IDENTIFIERCASE = DBPROPVAL_IC_UPPER;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := fDBPROP_IDENTIFIERCASE = DBPROPVAL_IC_LOWER;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := fDBPROP_IDENTIFIERCASE = DBPROPVAL_IC_MIXED;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := fDBPROP_QUOTEDIDENTIFIERCASE = DBPROPVAL_IC_SENSITIVE;
end;

{**
  Are multiple <code>ResultSet</code> from a single execute supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsMultipleResultSets: Boolean;
begin
  Result := fSupportsMultipleResultSets;
end;

function TZAdoDatabaseInfo.SupportsMultipleStorageObjects: Boolean;
begin
  Result := fSupportsMultipleStorageObjects;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := fDBPROP_QUOTEDIDENTIFIERCASE = DBPROPVAL_IC_UPPER;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := fDBPROP_QUOTEDIDENTIFIERCASE = DBPROPVAL_IC_LOWER;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := fDBPROP_QUOTEDIDENTIFIERCASE = DBPROPVAL_IC_MIXED ;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZAdoDatabaseInfo.GetSQLKeywords: string;
begin
  { TODO -ofjanos -cAPI : SQL Keywords that are not SQL92 compliant }
  Result := inherited GetSQLKeywords;
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAdoDatabaseInfo.GetNumericFunctions: string;
begin
  Result := 'ABS,ACOS,ASIN,ATAN,ATN2,CEILING,COS,COT,DEGREES,EXP,FLOOR,LOG,LOG10,'+
            'PI,POWER,RADIANS,RAND,ROUND,SIGN,SIN,SQUARE,SQRT,TAN';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAdoDatabaseInfo.GetStringFunctions: string;
begin
  Result := 'ASCII,CHAR,CHARINDEX,DIFFERENCE,LEFT,LEN,LOWER,LTRIM,NCHAR,PATINDEX,'+
            'REPLACE,QUOTENAME,REPLICATE,REVERSE,RIGHT,RTRIM,SOUNDEX,SPACE,STR,'+
            'STUFF,SUBSTRING,UNICODE,UPPER';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZAdoDatabaseInfo.GetSystemFunctions: string;
begin
  Result := 'APP_NAME,CASE,CAST,CONVERT,COALESCE,CURRENT_TIMESTAMP,CURRENT_USER,'+
            'DATALENGTH,@@ERROR,FORMATMESSAGE,GETANSINULL,HOST_ID,HOST_NAME,'+
            'IDENT_INCR,IDENT_SEED,@@IDENTITY,IDENTITY,ISDATE,ISNULL,ISNUMERIC,'+
            'NEWID,NULLIF,PARSENAME,PERMISSIONS,@@ROWCOUNT,SESSION_USER,STATS_DATE,'+
            'SYSTEM_USER,@@TRANCOUNT,USER_NAME';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZAdoDatabaseInfo.GetTimeDateFunctions: string;
begin
  Result := 'DATEADD,DATEDIFF,DATENAME,DATEPART,DAY,GETDATE,MONTH,YEAR';
end;

procedure TZAdoDatabaseInfo.InitilizePropertiesFromDBInfo(
  const DBInitialize: IDBInitialize; const Malloc: IMalloc);
const
  VARIANT_TRUE = -1;
  PropCount = 26;
  rgPropertyIDs: array[0..PropCount-1] of DBPROPID =
    ( DBPROP_PROVIDERFRIENDLYNAME,
      DBPROP_PROVIDERVER,
      DBPROP_DBMSNAME,
      DBPROP_DBMSVER,
      DBPROP_SUPPORTEDTXNISOLEVELS,
      DBPROP_MULTIPLERESULTS,
      DBPROP_MULTIPLESTORAGEOBJECTS,
      DBPROP_SCHEMAUSAGE,
      DBPROP_CATALOGUSAGE,
      DBPROP_IDENTIFIERCASE,
      DBPROP_QUOTEDIDENTIFIERCASE,
      DBPROP_MAXROWSIZE,
      DBPROP_MAXROWSIZEINCLUDESBLOB,
      DBPROP_SQLSUPPORT,
      DBPROP_CATALOGTERM,
      DBPROP_SCHEMATERM,
      DBPROP_PROCEDURETERM,
      DBPROP_SUPPORTEDTXNDDL,
      DBPROP_CONCATNULLBEHAVIOR,
      DBPROP_NULLCOLLATION,
      DBPROP_SUBQUERIES,
      DBPROP_GROUPBY,
      DBPROP_ORDERBYCOLUMNSINSELECT,
      DBPROP_PREPAREABORTBEHAVIOR,
      DBPROP_PREPARECOMMITBEHAVIOR,
      DBPROP_MULTIPLEPARAMSETS);
var
  DBProperties: IDBProperties;
  PropIDSet: array[0..PropCount-1] of TDBPROPIDSET;
  prgPropertySets: PDBPropSet;
  PropSet: TDBPropSet;
  nPropertySets: ULONG;
  i, intProp: Integer;
begin
  DBProperties := nil;
  OleCheck(DBInitialize.QueryInterface(IID_IDBProperties, DBProperties));
  try
    PropIDSet[0].rgPropertyIDs   := @rgPropertyIDs;
    PropIDSet[0].cPropertyIDs    := PropCount;
    PropIDSet[0].guidPropertySet := DBPROPSET_DATASOURCEINFO;
    nPropertySets := 0;
    prgPropertySets := nil;
    OleCheck( DBProperties.GetProperties( 1, @PropIDSet, nPropertySets, prgPropertySets ) );
    Assert( nPropertySets = 1 ); Assert(prgPropertySets.cProperties = PropCount);
    for i := 0 to prgPropertySets.cProperties-1 do begin
      PropSet := prgPropertySets^;
      if PropSet.rgProperties^[i].dwStatus <> DBPROPSTATUS(DBPROPSTATUS_OK) then
        Continue;
      case PropSet.rgProperties^[i].dwPropertyID of
        DBPROP_PROVIDERFRIENDLYNAME:    fDBPROP_PROVIDERFRIENDLYNAME := String(PropSet.rgProperties^[i].vValue);
        DBPROP_PROVIDERVER:             fDBPROP_PROVIDERVER := String(PropSet.rgProperties^[i].vValue);
        DBPROP_DBMSNAME:                fDBPROP_DBMSNAME := String(PropSet.rgProperties^[i].vValue);
        DBPROP_DBMSVER:                 fDBPROP_DBMSVER := String(PropSet.rgProperties^[i].vValue);
        DBPROP_SUPPORTEDTXNISOLEVELS:   begin
            intProp := PropSet.rgProperties^[i].vValue;
            fSupportedTransactIsolationLevels := [];
            if ISOLATIONLEVEL_CHAOS and intProp = ISOLATIONLEVEL_CHAOS then
              Include(fSupportedTransactIsolationLevels, tiNone);
            if ISOLATIONLEVEL_READUNCOMMITTED and intProp = ISOLATIONLEVEL_READUNCOMMITTED then
              Include(fSupportedTransactIsolationLevels, tiReadUncommitted);
            if ISOLATIONLEVEL_READCOMMITTED and intProp = ISOLATIONLEVEL_READCOMMITTED then
              Include(fSupportedTransactIsolationLevels, tiReadCommitted);
            if ISOLATIONLEVEL_REPEATABLEREAD and intProp = ISOLATIONLEVEL_REPEATABLEREAD then
              Include(fSupportedTransactIsolationLevels, tiRepeatableRead);
            if ISOLATIONLEVEL_SERIALIZABLE and intProp = ISOLATIONLEVEL_SERIALIZABLE then
              Include(fSupportedTransactIsolationLevels, tiSerializable);
          end;
        DBPROP_MULTIPLERESULTS:         fSupportsMultipleResultSets := PropSet.rgProperties^[i].vValue <> DBPROPVAL_MR_NOTSUPPORTED;
        DBPROP_MULTIPLESTORAGEOBJECTS:  fSupportsMultipleStorageObjects := PropSet.rgProperties^[i].vValue = VARIANT_TRUE;
        DBPROP_SCHEMAUSAGE:             fDBPROP_SCHEMAUSAGE := PropSet.rgProperties^[i].vValue;
        DBPROP_CATALOGUSAGE:            fDBPROP_CATALOGUSAGE := PropSet.rgProperties^[i].vValue;
        DBPROP_QUOTEDIDENTIFIERCASE:    fDBPROP_QUOTEDIDENTIFIERCASE := PropSet.rgProperties^[i].vValue;
        DBPROP_IDENTIFIERCASE:          fDBPROP_IDENTIFIERCASE := PropSet.rgProperties^[i].vValue;
        DBPROP_MAXROWSIZE:              fDBPROP_MAXROWSIZE := PropSet.rgProperties^[i].vValue;
        DBPROP_MAXROWSIZEINCLUDESBLOB:  fDBPROP_MAXROWSIZEINCLUDESBLOB := PropSet.rgProperties^[i].vValue = VARIANT_TRUE;
        DBPROP_SQLSUPPORT:              fDBPROP_SQLSUPPORT := PropSet.rgProperties^[i].vValue;
        DBPROP_CATALOGTERM:             fDBPROP_CATALOGTERM := String(PropSet.rgProperties^[i].vValue);
        DBPROP_SCHEMATERM:              fDBPROP_SCHEMATERM := String(PropSet.rgProperties^[i].vValue);
        DBPROP_PROCEDURETERM:           fDBPROP_PROCEDURETERM := String(PropSet.rgProperties^[i].vValue);
        DBPROP_SUPPORTEDTXNDDL:         fDBPROP_SUPPORTEDTXNDDL := PropSet.rgProperties^[i].vValue;
        DBPROP_CONCATNULLBEHAVIOR:      fDBPROP_CONCATNULLBEHAVIOR := PropSet.rgProperties^[i].vValue = DBPROPVAL_CB_NULL;
        DBPROP_NULLCOLLATION:           fDBPROP_NULLCOLLATION := PropSet.rgProperties^[i].vValue;
        DBPROP_SUBQUERIES:              fDBPROP_SUBQUERIES := PropSet.rgProperties^[i].vValue;
        DBPROP_GROUPBY:                 fDBPROP_GROUPBY := PropSet.rgProperties^[i].vValue;
        DBPROP_ORDERBYCOLUMNSINSELECT:  fDBPROP_ORDERBYCOLUMNSINSELECT := PropSet.rgProperties^[i].vValue = VARIANT_TRUE;
        DBPROP_PREPAREABORTBEHAVIOR:    fDBPROP_PREPAREABORTBEHAVIOR := PropSet.rgProperties^[i].vValue;
        DBPROP_PREPARECOMMITBEHAVIOR:   fDBPROP_PREPARECOMMITBEHAVIOR := PropSet.rgProperties^[i].vValue;
        DBPROP_MULTIPLEPARAMSETS:       fDBPROP_MULTIPLEPARAMSETS := PropSet.rgProperties^[i].vValue = VARIANT_TRUE;
      end;
      VariantClear(PropSet.rgProperties^[i].vValue);
    end;
    // free and clear elements of PropIDSet
    MAlloc.Free(PropSet.rgProperties);
    MAlloc.Free(prgPropertySets); //free prgPropertySets
  finally
    DBProperties := nil;
  end;
end;

{**
  Are concatenations between NULL and non-NULL values NULL?
  For SQL-92 compliance, a JDBC technology-enabled driver will
  return <code>true</code>.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.NullPlusNonNullIsNull: Boolean;
begin
  Result := fDBPROP_CONCATNULLBEHAVIOR
end;

function TZAdoDatabaseInfo.NullsAreSortedAtEnd: Boolean;
begin
  Result := fDBPROP_NULLCOLLATION = DBPROPVAL_NC_START;
end;

{**
  Are NULL values sorted at the start regardless of sort order?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.NullsAreSortedAtStart: Boolean;
begin
  Result := fDBPROP_NULLCOLLATION = DBPROPVAL_NC_END;
end;

{**
  Are NULL values sorted high?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.NullsAreSortedHigh: Boolean;
begin
  Result := fDBPROP_NULLCOLLATION = DBPROPVAL_NC_HIGH;
end;

{**
  Are NULL values sorted low?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.NullsAreSortedLow: Boolean;
begin
  Result := fDBPROP_NULLCOLLATION = DBPROPVAL_NC_LOW;
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
function TZAdoDatabaseInfo.GetSearchStringEscape: string;
begin
{ TODO -ofjanos -cgeneral : 
In sql server this must be specified as the parameter of like.
example: WHERE ColumnA LIKE '%5/%%' ESCAPE '/' }
  Result := '/';
end;

{**
  Returns the server version
  @return the server version string
}
function TZAdoDatabaseInfo.GetServerVersion: string;
begin
  Result := fDBPROP_DBMSVER;
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZAdoDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '@$#';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := True;
end;

{**
  Is the ODBC Extended SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsExtendedSQLGrammar: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQL_ODBC_EXTENDED = DBPROPVAL_SQL_ODBC_EXTENDED;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := not fDBPROP_ORDERBYCOLUMNSINSELECT;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := fDBPROP_GROUPBY <> 0;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := (fDBPROP_GROUPBY = DBPROPVAL_GB_CONTAINS_SELECT) or SupportsGroupByBeyondSelect;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := fDBPROP_GROUPBY = DBPROPVAL_GB_NO_RELATION;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQL_ANSI89_IEF = DBPROPVAL_SQL_ANSI89_IEF;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZAdoDatabaseInfo.GetSchemaTerm: string;
begin
  Result := fDBPROP_SCHEMATERM;
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZAdoDatabaseInfo.GetProcedureTerm: string;
begin
  Result := fDBPROP_PROCEDURETERM;
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZAdoDatabaseInfo.GetCatalogTerm: string;
begin
  Result := fDBPROP_CATALOGTERM;
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZAdoDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '.';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := fDBPROP_SCHEMAUSAGE and DBPROPVAL_SU_DML_STATEMENTS = DBPROPVAL_SU_DML_STATEMENTS;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  //NA
  Result := SupportsStoredProcedures and SupportsSchemasInTableDefinitions;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := fDBPROP_SCHEMAUSAGE and DBPROPVAL_SU_TABLE_DEFINITION = DBPROPVAL_SU_TABLE_DEFINITION;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := fDBPROP_SCHEMAUSAGE and DBPROPVAL_SU_INDEX_DEFINITION = DBPROPVAL_SU_INDEX_DEFINITION;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := fDBPROP_SCHEMAUSAGE and DBPROPVAL_SU_PRIVILEGE_DEFINITION = DBPROPVAL_SU_PRIVILEGE_DEFINITION;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := fDBPROP_CATALOGUSAGE and DBPROPVAL_CU_DML_STATEMENTS = DBPROPVAL_CU_DML_STATEMENTS;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  //NA
  Result := SupportsStoredProcedures and SupportsCatalogsInTableDefinitions;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := fDBPROP_CATALOGUSAGE and DBPROPVAL_CU_TABLE_DEFINITION = DBPROPVAL_CU_TABLE_DEFINITION;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := fDBPROP_CATALOGUSAGE and DBPROPVAL_CU_INDEX_DEFINITION = DBPROPVAL_CU_INDEX_DEFINITION;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := fDBPROP_CATALOGUSAGE and DBPROPVAL_CU_PRIVILEGE_DEFINITION = DBPROPVAL_CU_PRIVILEGE_DEFINITION;
end;

{**
  Can a stored procedure have an additional overload suffix?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsOverloadPrefixInStoredProcedureName: Boolean;
begin
  Result := True;
end;

{**
  Is parameter bindings supported by Provider?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsParameterBinding: Boolean;
begin
  Result := fDBPROP_MULTIPLEPARAMSETS
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
//CURRENT OF
//Specifies that the DELETE is done at the current position of the specified cursor.
  Result := True;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := True;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := True;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQ_COMPARISON = DBPROPVAL_SQ_COMPARISON;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQ_EXISTS = DBPROPVAL_SQ_EXISTS;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQ_IN = DBPROPVAL_SQ_IN;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQ_QUANTIFIED = DBPROPVAL_SQ_QUANTIFIED;
end;

{**
  Is the ODBC Core SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsCoreSQLGrammar: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQL_ODBC_CORE = DBPROPVAL_SQL_ODBC_CORE;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := fDBPROP_SUBQUERIES and DBPROPVAL_SQ_CORRELATEDSUBQUERIES = DBPROPVAL_SQ_CORRELATEDSUBQUERIES;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZAdoDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := fDBPROP_PREPARECOMMITBEHAVIOR = DBPROPVAL_CB_PRESERVE;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZAdoDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := fDBPROP_PREPAREABORTBEHAVIOR = DBPROPVAL_CB_PRESERVE;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZAdoDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := fDBPROP_PREPARECOMMITBEHAVIOR = DBPROPVAL_CB_PRESERVE;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZAdoDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := fDBPROP_PREPAREABORTBEHAVIOR = DBPROPVAL_CB_PRESERVE;
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
function TZAdoDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 16000;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 8000;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 4096;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 1024;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := 128;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 900;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := fDBPROP_MAXROWSIZE;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := fDBPROP_MAXROWSIZEINCLUDESBLOB;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 0;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 128;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 256;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZAdoDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 128;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZAdoDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiReadCommitted;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := fSupportedTransactIsolationLevels <> [];
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZAdoDatabaseInfo.SupportsTransactionIsolationLevel(
  const Level: TZTransactIsolationLevel): Boolean;
begin
  Result := Level in fSupportedTransactIsolationLevels;
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := fDBPROP_SUPPORTEDTXNDDL and DBPROPVAL_TC_ALL = DBPROPVAL_TC_ALL;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := fDBPROP_SUPPORTEDTXNDDL and DBPROPVAL_TC_DML = DBPROPVAL_TC_DML;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := fDBPROP_SUPPORTEDTXNDDL and DBPROPVAL_TC_DDL_COMMIT = DBPROPVAL_TC_DDL_COMMIT;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := fDBPROP_SUPPORTEDTXNDDL and DBPROPVAL_TC_DDL_IGNORE = DBPROPVAL_TC_DDL_IGNORE;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsResultSetType(
  const _Type: TZResultSetType): Boolean;
begin
  Result := True;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsResultSetConcurrency(
  const _Type: TZResultSetType; const Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := True;
end;

{**
  Does the Database or Actual Version understand non escaped search strings?
  @return <code>true</code> if the DataBase does understand non escaped
  search strings
}
function TZAdoDatabaseInfo.SupportsNonEscapedSearchStrings: Boolean;
begin
  Result := True;
end;

{**
  Does the Database support updating auto incremental fields?
  @return <code>true</code> if the DataBase allows it.
}
function TZAdoDatabaseInfo.SupportsUpdateAutoIncrementFields: Boolean;
begin
  Result := False;
end;

{**
  Is the ANSI92 entry level SQL grammar supported?
  All JDBC Compliant<sup><font size=-2>TM</font></sup> drivers must return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsANSI92EntryLevelSQL: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQL_ANSI92_ENTRY = DBPROPVAL_SQL_ANSI92_ENTRY;
end;

{**
  Is the ANSI92 full SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsANSI92FullSQL: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQL_ANSI92_FULL = DBPROPVAL_SQL_ANSI92_FULL;
end;

{**
  Is the ANSI92 intermediate SQL grammar supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZAdoDatabaseInfo.SupportsANSI92IntermediateSQL: Boolean;
begin
  Result := fDBPROP_SQLSUPPORT and DBPROPVAL_SQL_ANSI92_INTERMEDIATE = DBPROPVAL_SQL_ANSI92_INTERMEDIATE;
end;

{**
  Does the Database support binding arrays? Is the ZDbc ready for this?
  @return <code>true</code> if the DataBase allows it.
}
function TZAdoDatabaseInfo.SupportsArrayBindings: Boolean;
begin
  Result := True;
end;


{ TZAdoDatabaseMetadata }


{**
  Constructs this object and assignes the main properties.
  @param Connection a database connection object.
  @param Url a database connection url string.
  @param Info an extra connection properties.
}
constructor TZAdoDatabaseMetadata.Create(Connection: TZAbstractConnection;
  const Url: TZURL);
begin
  inherited Create(Connection, Url);
  FAdoConnection := nil;
end;

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZAdoDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZAdoDatabaseInfo.Create(Self);
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
function TZAdoDatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetProcedures(Catalog, SchemaPattern, ProcedureNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaProcedures,
    [Catalog, SchemaPattern, ProcedureNamePattern, '']);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('PROCEDURE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('PROCEDURE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(ProcedureNameIndex, GetPWideCharByName('PROCEDURE_NAME', Len), @Len);
        Result.UpdatePWideChar(ProcedureRemarksIndex, GetPWideCharByName('DESCRIPTION', Len), @Len);
        Result.UpdateSmall(ProcedureTypeIndex, GetSmallByName('PROCEDURE_TYPE') - 1);
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaProcedureParameters,
    [Catalog, SchemaPattern, ProcedureNamePattern]);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('PROCEDURE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('PROCEDURE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(ProcColProcedureNameIndex, GetPWideCharByName('PROCEDURE_NAME', Len), @Len);
        Result.UpdatePWideChar(ProcColColumnNameIndex, GetPWideCharByName('PARAMETER_NAME', Len), @Len);
        case GetSmallByName('PARAMETER_TYPE') of
          1: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctIn));
          2: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctInOut));
          3: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctOut));
          4: Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctReturn));
        else
          Result.UpdateSmall(ProcColColumnTypeIndex, Ord(pctUnknown));
        end;
        Result.UpdateSmall(ProcColDataTypeIndex, Ord(ConvertAdoToSqlType(
          GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
        Result.UpdatePWideChar(ProcColTypeNameIndex, GetPWideCharByName('TYPE_NAME', Len), @Len);
        Result.UpdateInt(ProcColPrecisionIndex, GetIntByName('NUMERIC_PRECISION'));
        Result.UpdateInt(ProcColLengthIndex, GetIntByName('CHARACTER_OCTET_LENGTH'));
        Result.UpdateSmall(ProcColScaleIndex, GetSmallByName('NUMERIC_SCALE'));
  //      Result.UpdateSmall(ProcColRadixIndex, GetSmallByName('RADIX'));
        if GetStringByName('IS_NULLABLE') = 'NO' then
          Result.UpdateSmall(ProcColNullableIndex, 0)
        else
          if GetStringByName('IS_NULLABLE') = 'YES' then
            Result.UpdateSmall(ProcColNullableIndex, 1)
          else
            Result.UpdateSmall(ProcColNullableIndex, 2);
        Result.UpdatePWideChar(ProcColRemarksIndex, GetPWideCharByName('DESCRIPTION', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
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
function TZAdoDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  I: Integer;
  TableTypes: string;
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);

  TableTypes := '';
  for I := Low(Types) to High(Types) do
  begin
    if Length(TableTypes) > 0 then
      TableTypes := TableTypes + ',';
    TableTypes := TableTypes + Types[I];
  end;

  AdoRecordSet := AdoOpenSchema(adSchemaTables,
    [Catalog, SchemaPattern, TableNamePattern, TableTypes]);
  if Assigned(AdoRecordSet) then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordset) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(TableColumnsSQLType, GetPWideCharByName('TABLE_TYPE', Len), @Len);
        Result.UpdatePWideChar(TableColumnsRemarks, GetPWideCharByName('DESCRIPTION', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetSchemas: IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result := inherited UncachedGetSchemas;

  AdoRecordSet := AdoOpenSchema(adSchemaSchemata, []);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(SchemaColumnsTableSchemaIndex,
          GetPWideCharByName('SCHEMA_NAME', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetCatalogs;

  AdoRecordSet := AdoOpenSchema(adSchemaCatalogs, []);
  if Assigned(AdoRecordSet) then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('CATALOG_NAME', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TableTypes: array[0..7] of ZWideString = (
    'ALIAS', 'TABLE', 'SYNONYM', 'SYSTEM TABLE', 'VIEW',
    'GLOBAL TEMPORARY', 'LOCAL TEMPORARY', 'SYSTEM VIEW'
  );
var
  I: Integer;
begin
  Result:=inherited UncachedGetTableTypes;

  for I := 0 to 7 do
  begin
    Result.MoveToInsertRow;
    Result.UpdateUnicodeString(TableTypeColumnTableTypeIndex, TableTypes[I]);
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
function TZAdoDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Flags: Integer;
  SQLType: TZSQLType;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetColumns(Catalog, SchemaPattern,
      TableNamePattern, ColumnNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaColumns,
    [DecomposeObjectString(Catalog), DecomposeObjectString(SchemaPattern),
    DecomposeObjectString(TableNamePattern), DecomposeObjectString(ColumnNamePattern)]);
  if Assigned(AdoRecordSet) then
  begin
    AdoRecordSet.Sort := 'ORDINAL_POSITION';
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(ColumnNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);

        SQLType := ConvertAdoToSqlType(GetSmallByName('DATA_TYPE'),
          ConSettings.CPType);
        Flags := GetIntByName('COLUMN_FLAGS');
//!!!If the field type is long then this is the only way to know it because it just returns string type
        if ((Flags and DBCOLUMNFLAGS_ISLONG) <> 0 ) and (SQLType in [stBytes, stString, stUnicodeString]) then
          case SQLType of
            stBytes: SQLType := stBinaryStream;
            stString: SQLType := stAsciiStream;
            stUnicodeString: SQLType := stUnicodeStream;
          end;
        Result.UpdateSmall(TableColColumnTypeIndex, Ord(SQLType));
        Result.UpdateInt(TableColColumnSizeIndex, GetIntByName('CHARACTER_MAXIMUM_LENGTH'));
        Result.UpdateInt(TableColColumnBufLengthIndex, GetIntByName('CHARACTER_MAXIMUM_LENGTH'));
        Result.UpdateInt(TableColColumnDecimalDigitsIndex, GetIntByName('NUMERIC_SCALE'));
        Result.UpdateInt(TableColColumnNumPrecRadixIndex, GetSmallByName('NUMERIC_PRECISION'));
        if GetBooleanByName('IS_NULLABLE') then
          Result.UpdateSmall(TableColColumnNullableIndex, 1)
        else
          Result.UpdateSmall(TableColColumnNullableIndex, 0);
        Result.UpdatePWideChar(TableColColumnRemarksIndex, GetPWideCharByName('DESCRIPTION', Len), @Len);
        Result.UpdatePWideChar(TableColColumnColDefIndex, GetPWideCharByName('COLUMN_DEFAULT', Len), @Len);
        Result.UpdateSmall(TableColColumnSQLDataTypeIndex, GetSmallByName('DATETIME_PRECISION'));
        Result.UpdateInt(TableColColumnCharOctetLengthIndex, GetIntByName('CHARACTER_OCTET_LENGTH'));
        Result.UpdateInt(TableColColumnOrdPosIndex, GetIntByName('ORDINAL_POSITION'));
        if UpperCase(GetStringByName('IS_NULLABLE')) = 'FALSE' then
          Result.UpdateString(TableColColumnIsNullableIndex, 'NO')
        else
          Result.UpdateString(TableColColumnIsNullableIndex, 'YES');

        //Result.UpdateNullByName(TableColColumnAutoIncIndex);
        Result.UpdateBoolean(TableColColumnSearchableIndex, (Flags and (DBCOLUMNFLAGS_ISLONG) = 0));
        Result.UpdateBoolean(TableColColumnWritableIndex, (Flags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) <> 0));
        Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, (Flags and (DBCOLUMNFLAGS_WRITE) <> 0));
        Result.UpdateBoolean(TableColColumnReadonlyIndex, (Flags and (DBCOLUMNFLAGS_WRITE or DBCOLUMNFLAGS_WRITEUNKNOWN) = 0));
        Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, IC.IsCaseSensitive(GetStringByName('COLUMN_NAME')));
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
  end;
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
function TZAdoDatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetColumnPrivileges(Catalog, Schema, Table, ColumnNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaColumnPrivileges,
    [Catalog, Schema, Table, ColumnNamePattern]);
  if Assigned(AdoRecordSet) then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(ColumnNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);
        Result.UpdatePWideChar(TableColPrivGrantorIndex, GetPWideCharByName('GRANTOR', Len), @Len);
        Result.UpdatePWideChar(TableColPrivGranteeIndex, GetPWideCharByName('GRANTEE', Len), @Len);
        Result.UpdatePWideChar(TableColPrivPrivilegeIndex, GetPWideCharByName('PRIVILEGE_TYPE', Len), @Len);
        if GetBooleanByName('IS_GRANTABLE') then
          Result.UpdateString(TableColPrivIsGrantableIndex, 'YES')
        else
          Result.UpdateString(TableColPrivIsGrantableIndex, 'NO');
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

  AdoRecordSet := AdoOpenSchema(adSchemaTablePrivileges,
    [Catalog, SchemaPattern, TableNamePattern]);
  if Assigned(AdoRecordSet) then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(TablePrivGrantorIndex, GetPWideCharByName('GRANTOR', Len), @Len);
        Result.UpdatePWideChar(TablePrivGranteeIndex, GetPWideCharByName('GRANTEE', Len), @Len);
        Result.UpdatePWideChar(TablePrivPrivilegeIndex, GetPWideCharByName('PRIVILEGE_TYPE', Len), @Len);
        if GetBooleanByName('IS_GRANTABLE') then
          Result.UpdateString(TablePrivIsGrantableIndex, 'YES')
        else
          Result.UpdateString(TablePrivIsGrantableIndex, 'NO');
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
function TZAdoDatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
const
  DBCOLUMNFLAGS_ISROWVER = $00000200;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetVersionColumns(Catalog, Schema, Table);

  AdoRecordSet := AdoOpenSchema(adSchemaColumns, [Catalog, Schema, Table]);
  if Assigned(AdoRecordSet) then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        if (GetIntByName('COLUMN_FLAGS')
          and DBCOLUMNFLAGS_ISROWVER) = 0 then
          Continue;
        Result.MoveToInsertRow;
        Result.UpdateSmall(TableColVerScopeIndex, 0);
        Result.UpdatePWideChar(TableColVerColNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);
        Result.UpdateSmall(TableColVerDataTypeIndex, Ord(ConvertAdoToSqlType(
          GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
        Result.UpdatePWideChar(TableColVerTypeNameIndex, GetPWideCharByName('TYPE_NAME', Len), @Len);
        Result.UpdateInt(TableColVerColSizeIndex, GetIntByName('CHARACTER_OCTET_LENGTH'));
        Result.UpdateInt(TableColVerBufLengthIndex, GetIntByName('CHARACTER_OCTET_LENGTH'));
        Result.UpdateInt(TableColVerDecimalDigitsIndex, GetIntByName('NUMERIC_SCALE'));
        Result.UpdateSmall(TableColVerPseudoColumnIndex, 0);
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
  end;
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
function TZAdoDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetPrimaryKeys(Catalog, Schema, Table);

  AdoRecordSet := AdoOpenSchema(adSchemaPrimaryKeys,
    [Catalog, Schema, Table]);
  if AdoRecordSet <> nil then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(PrimaryKeyColumnNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);
        Result.UpdateSmall(PrimaryKeyKeySeqIndex, GetSmallByName('ORDINAL'));
        if FindColumn('PK_NAME') > FirstDbcIndex then
          Result.UpdatePWideChar(PrimaryKeyPKNameIndex, GetPWideCharByName('PK_NAME', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
  end;
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
function TZAdoDatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
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
function TZAdoDatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference(Catalog, Schema, Table, '', '', '');
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
function TZAdoDatabaseMetadata.UncachedGetCrossReference(const PrimaryCatalog: string;
  const PrimarySchema: string; const PrimaryTable: string; const ForeignCatalog: string;
  const ForeignSchema: string; const ForeignTable: string): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;

  function GetRuleType(const Rule: String): TZImportedKey;
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
begin
  Result:=inherited UncachedGetCrossReference(PrimaryCatalog, PrimarySchema, PrimaryTable,
                                              ForeignCatalog, ForeignSchema, ForeignTable);

  AdoRecordSet := AdoOpenSchema(adSchemaForeignKeys, [PrimaryCatalog,
    PrimarySchema, PrimaryTable, ForeignCatalog, ForeignSchema, ForeignTable]);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CrossRefKeyColPKTableCatalogIndex, GetPWideCharByName('PK_TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColPKTableSchemaIndex, GetPWideCharByName('PK_TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColPKTableNameIndex, GetPWideCharByName('PK_TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColPKColumnNameIndex, GetPWideCharByName('PK_COLUMN_NAME', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColFKTableCatalogIndex, GetPWideCharByName('FK_TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColFKTableSchemaIndex, GetPWideCharByName('FK_TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColFKTableNameIndex, GetPWideCharByName('FK_TABLE_NAME', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColFKColumnNameIndex, GetPWideCharByName('FK_COLUMN_NAME', Len), @Len);
        Result.UpdateSmall(CrossRefKeyColKeySeqIndex, GetSmallByName('ORDINAL'));
        Result.UpdateSmall(CrossRefKeyColUpdateRuleIndex, Ord(GetRuleType(GetStringByName('UPDATE_RULE'))));
        Result.UpdateSmall(CrossRefKeyColDeleteRuleIndex, Ord(GetRuleType(GetStringByName('DELETE_RULE'))));
        Result.UpdatePWideChar(CrossRefKeyColFKNameIndex, GetPWideCharByName('FK_NAME', Len), @Len);
        Result.UpdatePWideChar(CrossRefKeyColPKNameIndex, GetPWideCharByName('PK_NAME', Len), @Len);
        Result.UpdateInt(CrossRefKeyColDeferrabilityIndex, GetSmallByName('DEFERRABILITY'));
        Result.InsertRow;
      end;
      Close;
      Free;
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
 	<LI><B>CASE_SENSITIVE</B> boolean=> is it case sensitive?
 	<LI><B>SEARCHABLE</B> short => can you use "WHERE" based on this type:
       <UL>
       <LI> typePredNone - No support
       <LI> typePredChar - Only supported with WHERE .. LIKE
       <LI> typePredBasic - Supported except for WHERE .. LIKE
       <LI> typeSearchable - Supported for all WHERE ..
       </UL>
 	<LI><B>UNSIGNED_ATTRIBUTE</B> boolean => is it unsigned?
 	<LI><B>FIXED_PREC_SCALE</B> boolean => can it be a money value?
 	<LI><B>AUTO_INCREMENT</B> boolean => can it be used for an
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
function TZAdoDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetTypeInfo;

  AdoRecordSet := AdoOpenSchema(adSchemaProviderTypes, []);
  if AdoRecordSet <> nil then
  begin
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(TypeInfoTypeNameIndex, GetPWideCharByName('TYPE_NAME', Len), @Len);
        Result.UpdateSmall(TypeInfoDataTypeIndex, Ord(ConvertAdoToSqlType(
          GetSmallByName('DATA_TYPE'), ConSettings.CPType)));
        Result.UpdateInt(TypeInfoPecisionIndex, 0);//GetIntByName('PRECISION'));
        Result.UpdatePWideChar(TypeInfoLiteralPrefixIndex, GetPWideCharByName('LITERAL_PREFIX', Len), @Len);
        Result.UpdatePWideChar(TypeInfoLiteralSuffixIndex, GetPWideCharByName('LITERAL_SUFFIX', Len), @Len);
        Result.UpdatePWideChar(TypeInfoCreateParamsIndex, GetPWideCharByName('CREATE_PARAMS', Len), @Len);
        if GetBooleanByName('IS_NULLABLE') then
          Result.UpdateSmall(TypeInfoNullAbleIndex, 1)
        else
          Result.UpdateSmall(TypeInfoNullAbleIndex, 0);
        Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, GetBooleanByName('CASE_SENSITIVE'));
        Result.UpdateSmall(TypeInfoSearchableIndex, GetSmallByName('SEARCHABLE'));
        Result.UpdateBoolean(TypeInfoUnsignedAttributeIndex, GetBooleanByName('UNSIGNED_ATTRIBUTE'));
        Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, GetBooleanByName('FIXED_PREC_SCALE'));
        Result.UpdateBoolean(TypeInfoAutoIncrementIndex, False);
        Result.UpdatePWideChar(TypeInfoLocaleTypeNameIndex, GetPWideCharByName('LOCAL_TYPE_NAME', Len), @Len);
        Result.UpdateSmall(TypeInfoMinimumScaleIndex, GetSmallByName('MINIMUM_SCALE'));
        Result.UpdateSmall(TypeInfoMaximumScaleIndex, GetSmallByName('MAXIMUM_SCALE'));
  //      Result.UpdateSmall(TypeInfoSQLDataTypeIndex, GetSmallByName('SQL_DATA_TYPE'));
  //      Result.UpdateSmall(TypeInfoSQLDateTimeSubIndex, GetSmallByName('SQL_DATETIME_SUB'));
  //      Result.UpdateSmall(TypeInfoNumPrecRadix, GetSmallByName('NUM_PREC_RADIX'));
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
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
 	<LI><B>NON_UNIQUE</B> boolean => Can index values be non-unique?
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
function TZAdoDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
var
  AdoRecordSet: ZPlainAdo.RecordSet;
  Len: NativeUInt;
begin
  Result:=inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  AdoRecordSet := AdoOpenSchema(adSchemaIndexes,
    [Catalog, Schema, '', '', Table]);
  if AdoRecordSet <> nil then
    with TZAdoResultSet.Create(GetStatement, '', AdoRecordSet) do
    begin
      while Next do
      begin
        Result.MoveToInsertRow;
        Result.UpdatePWideChar(CatalogNameIndex, GetPWideCharByName('TABLE_CATALOG', Len), @Len);
        Result.UpdatePWideChar(SchemaNameIndex, GetPWideCharByName('TABLE_SCHEMA', Len), @Len);
        Result.UpdatePWideChar(TableNameIndex, GetPWideCharByName('TABLE_NAME', Len), @Len);
        Result.UpdateBoolean(IndexInfoColNonUniqueIndex, not GetBooleanByName('UNIQUE'));
        Result.UpdatePWideChar(IndexInfoColIndexQualifierIndex, GetPWideCharByName('INDEX_CATALOG', Len), @Len);
        Result.UpdatePWideChar(IndexInfoColIndexNameIndex, GetPWideCharByName('INDEX_NAME', Len), @Len);
        Result.UpdateSmall(IndexInfoColTypeIndex, GetSmallByName('TYPE'));
        Result.UpdateSmall(IndexInfoColOrdPositionIndex, GetSmallByName('ORDINAL_POSITION'));
        Result.UpdatePWideChar(IndexInfoColColumnNameIndex, GetPWideCharByName('COLUMN_NAME', Len), @Len);
  //!!!      Result.UpdatePWideChar(IndexInfoColAscOrDescIndex, GetPWideCharByName('COLLATION', Len), @Len);
        Result.UpdateInt(IndexInfoColCardinalityIndex, GetIntByName('CARDINALITY'));
        Result.UpdateInt(IndexInfoColPagesIndex, GetIntByName('PAGES'));
        Result.UpdatePWideChar(IndexInfoColFilterConditionIndex, GetPWideCharByName('FILTER_CONDITION', Len), @Len);
        Result.InsertRow;
      end;
      Close;
      Free;
    end;
end;

{**

  Gets a description of the user-defined types defined in a particular
  schema.  Schema-specific UDTs may have type JAVA_OBJECT, STRUCT,
  or DISTINCT.

  <P>Only types matching the catalog, schema, type name and type
  criteria are returned.  They are ordered by DATA_TYPE, TYPE_SCHEM
  and TYPE_NAME.  The type name parameter may be a fully-qualified
  name.  In this case, the catalog and schemaPattern parameters are
  ignored.

  <P>Each type description has the following columns:
   <OL>
 	<LI><B>TYPE_CAT</B> String => the type's catalog (may be null)
 	<LI><B>TYPE_SCHEM</B> String => type's schema (may be null)
 	<LI><B>TYPE_NAME</B> String => type name
   <LI><B>CLASS_NAME</B> String => Java class name
 	<LI><B>DATA_TYPE</B> String => type value defined in java.sql.Types.
   One of JAVA_OBJECT, STRUCT, or DISTINCT
 	<LI><B>REMARKS</B> String => explanatory comment on the type
   </OL>

  <P><B>Note:</B> If the driver does not support UDTs, an empty
  result set is returned.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schemaPattern a schema name pattern; "" retrieves those
  without a schema
  @param typeNamePattern a type name pattern; may be a fully-qualified name
  @param types a list of user-named types to include (JAVA_OBJECT,
  STRUCT, or DISTINCT); null returns all types
  @return <code>ResultSet</code> - each row is a type description
}
function TZAdoDatabaseMetadata.UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
  const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet;
begin

  Result:=inherited UncachedGetUDTs(Catalog, SchemaPattern, TypeNamePattern, Types);

//  AdoRecordSet := AdoOpenSchema(adSchemaIndexes, Restrictions);
//  if Assigned(AdoRecordSet) then
//  with GetStatement.ExecuteQuery(
//    Format('select TYPE_CAT = db_name(), TYPE_SCHEM = user_name(uid),'
//      + ' TYPE_NAME = st.name, DATA_TYPE from master.dbo.spt_datatype_info'
//      + ' sti left outer join systypes st on (sti.ss_dtype = st.xtype)'
//      + ' where st.xusertype > 255 and user_name(uid) like %s and st.name'
//      + ' like %s', [SchemaPattern, TypeNamePattern])) do
//    while Next do
//    begin
//      Result.MoveToInsertRow;
//      Result.UpdatePWideChar(CatalogNameIndex, GetPWideChar('TYPE_CAT', Len), @Len);
//      Result.UpdatePWideChar(SchemaNameIndex, GetPWideChar('TYPE_SCHEM', Len), @Len);
//      Result.UpdatePWideChar(UDTColTypeNameIndex, GetPWideChar('TYPE_NAME', Len), @Len);
//      Result.UpdateNull(UDTColClassNameIndex);
//      Result.UpdateSmall(UDTColDataTypeIndex, GetSmall('DATA_TYPE'));
//      Result.UpdateNull(UDTColRemarksIndex);
//      Result.InsertRow;
//    end;

end;

{**
  Open a schema rowset from ado

  @Schema Ado identifier
  @Args Variant array with restrictions
  @return ADO recordset with the schemas; nil if the schema is not supported
}
function TZAdoDatabaseMetadata.AdoOpenSchema(Schema: Integer; const Args: array of const): ZPlainAdo.RecordSet;
var
  Restrictions: Variant;
begin
  Result := nil;
  if not FSupportedSchemasInitialized then
    InitializeSchemas;
  if not SchemaSupported(Schema) then
    Exit;
  try
    Restrictions := BuildRestrictions(Schema, Args);
    Result := (GetConnection as IZAdoConnection).GetAdoConnection.
      OpenSchema(Schema, Restrictions, EmptyParam);
  except
    Result := nil;
  end;
end;

{**
  Initialize supported schemas and restrictions from the OleDB provider
}
procedure TZAdoDatabaseMetadata.InitializeSchemas;
var
  AdoConnection: IZAdoConnection;
  OleDBSession: IUnknown;
  SchemaRS: IDBSchemaRowset;
  PG, OriginalPG: PGUID;
  IA: PIntegerArray;
  Nr: Integer;
  I: Integer;
begin
  if not FSupportedSchemasInitialized then
  begin
    if not Assigned(FAdoConnection) then begin
      GetConnection.QueryInterface(IZAdoConnection, AdoConnection);
      FAdoConnection := AdoConnection.GetAdoConnection;
    end;
    (FAdoConnection as ADOConnectionConstruction).Get_Session(OleDBSession);
    if Assigned(OleDBSession) then begin
      OleDBSession.QueryInterface(IDBSchemaRowset, SchemaRS);
      if Assigned(SchemaRS) then
      begin
        SchemaRS.GetSchemas(Nr{%H-}, PG{%H-}, PInteger({%H-}IA));
        OriginalPG := PG;
        SetLength(SupportedSchemas, Nr);
        for I := 0 to Nr - 1 do
        begin
          SupportedSchemas[I].SchemaGuid := PG^;
          SupportedSchemas[I].SupportedRestrictions := IA^[I];
          SupportedSchemas[I].AdoSchemaId := ConvertOleDBToAdoSchema(PG^);
          Inc({%H-}NativeInt(PG), SizeOf(TGuid));  //M.A. Inc(Integer(PG), SizeOf(TGuid));
        end;
        FSupportedSchemasInitialized := True;
        if Assigned(OriginalPG) then ZAdoMalloc.Free(OriginalPG);
        if Assigned(IA) then ZAdoMalloc.Free(IA);
      end;
    end;
  end;
end;

{**
  Find the Schema Id in the supported schemas

  @SchemaId Ado identifier
  @return Index of the schema in the supported schemas array
}
function TZAdoDatabaseMetadata.FindSchema(SchemaId: Integer): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(SupportedSchemas) - 1 do
    if SupportedSchemas[I].AdoSchemaId = SchemaId then
    begin
      Result := I;
      Break;
    end;
end;

{**
  Is the schema supported by the OleDB provider?

  @SchemaId Ado identifier
  @return True if the schema is supported
}
function TZAdoDatabaseMetadata.SchemaSupported(SchemaId: Integer): Boolean;
begin
  Result := FindSchema(SchemaId) > -1;
end;

{**
  Build a variant array from the provided parameters based on the supported restrictions

  @SchemaId Ado identifier
  @Args Restrictions
  @return Variant array of restrictions
}
function TZAdoDatabaseMetadata.BuildRestrictions(SchemaId: Integer;
  const Args: array of const): Variant;
var
  SchemaIndex: Integer;
  I: Integer;
begin
  Result := Null;
  if High(Args) = -1 then
    Exit; 
  SchemaIndex := FindSchema(SchemaId);
  if SchemaIndex = -1 then
    Exit;

  Result := VarArrayCreate([0, High(Args)], varVariant);
  for I := 0 to High(Args) do
  begin
    if (SupportedSchemas[SchemaIndex].SupportedRestrictions
      and (1 shl I)) <> 0 then
    begin
      {$IFDEF UNICODE}
      Result[I] := string(Args[I].VPWideChar);
      if (Args[I].VType = VtUnicodeString) then
        if string(Args[I].VPWideChar) = '' then
      {$ELSE}
      Result[I] := string(Args[I].VAnsiString);
      if (Args[I].VType = vtAnsiString) then
        if string(Args[I].VAnsiString) = '' then
      {$ENDIF}
          Result[I] := UnAssigned;
    end else
      Result[I] := UnAssigned;
  end;
end;

{$ENDIF ZEOS_DISABLE_ADO}
end.
