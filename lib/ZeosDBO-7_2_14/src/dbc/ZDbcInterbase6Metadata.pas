{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         Interbase Database Connectivity Classes         }
{                                                         }
{        Originally written by Sergey Merkuriev           }
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

unit ZDbcInterbase6Metadata;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
uses
  Types, Classes, SysUtils, StrUtils, ZSysUtils, ZDbcIntfs, ZDbcMetadata, ZCompatibility,
  ZDbcInterbase6;

type

  // technobot 2008-06-25 - methods moved as is from TZInterbase6DatabaseMetadata:
  {** Implements Interbase6 Database Information. }
  IZInterbaseDatabaseInfo = Interface(IZDatabaseInfo)
    ['{F2895A2A-C427-4984-9356-79349EAAD44F}']
    function HostIsFireBird: Boolean;
    function GetHostVersion: Integer;

    function SupportsNextValueFor: Boolean;
    function SupportsTrim: Boolean;
    function SupportsBinaryInSQL: Boolean;

    function GetMaxSQLDASize: LongWord;

    procedure CollectServerInformations;
  End;

  TZInterbase6DatabaseInfo = class(TZAbstractDatabaseInfo, IZInterbaseDatabaseInfo)
  private
    FIsFireBird: Boolean;
    FServerVersion: string;
    FProductVersion: String;
    FHostVersion: Integer;
//    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
//      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;
  public
    procedure CollectServerInformations;
    function GetHostVersion: Integer;
    function HostIsFireBird: Boolean;
    function SupportsNextValueFor: Boolean;
    function SupportsTrim: Boolean;
    function SupportsBinaryInSQL: Boolean;
    function GetMaxSQLDASize: LongWord;
    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
//    function GetDriverVersion: string; override; -> Same as parent
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

  {** Implements Interbase6 Database Metadata. }

  { TZInterbase6DatabaseMetadata }

  TZInterbase6DatabaseMetadata = class(TZAbstractDatabaseMetadata)
  private
    FInfo: TStrings;
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-25
    function ConstructNameCondition(Pattern: string; Column: string): string; override;

    function UncachedGetTables(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
//    function UncachedGetSchemas: IZResultSet; override; -> Not implemented
//    function UncachedGetCatalogs: IZResultSet; override; -> Not Implemented
    function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string): IZResultSet; override;
    function UncachedGetColumnPrivileges(const {%H-}Catalog: string; const {%H-}Schema: string;
      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetPrimaryKeys(const {%H-}Catalog: string; const {%H-}Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetCrossReference(const {%H-}PrimaryCatalog: string; const {%H-}PrimarySchema: string;
      const PrimaryTable: string; const {%H-}ForeignCatalog: string; const {%H-}ForeignSchema: string;
      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
    function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
      const SequenceNamePattern: string): IZResultSet; override;
    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string): IZResultSet; override;
    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
      const ProcedureNamePattern: string; const ColumnNamePattern: string):
      IZResultSet; override;
    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
    function UncachedGetTriggers(const Catalog: string;
      const SchemaPattern: string; const TableNamePattern: string;
      const TriggerNamePattern: string): IZResultSet; override; //EgonHugesit
    function UncachedGetCollationAndCharSet(const Catalog, SchemaPattern,
      TableNamePattern, ColumnNamePattern: string): IZResultSet; override; //EgonHugeist
    function UncachedGetCharacterSets: IZResultSet; override; //EgonHugeist
  public
    function CreateStatement: IZStatement;
    procedure SetUTF8CodePageInfo;
    procedure BeforeDestruction; override;
  end;

{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit

uses ZMessages, ZDbcInterbase6Utils, ZPlainFirebirdInterbaseConstants, ZClasses,
  ZFastCode, ZSelectSchema, ZEncoding, Math;

const
  DBProvider: array[Boolean] of String = ('Interbase', 'Firebird');

{ TZInterbase6DatabaseInfo }

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

procedure TZInterbase6DatabaseInfo.CollectServerInformations;
var
  FIBConnection: IZInterbase6Connection;
  I: Integer;
  tmp: string;
begin
  if FServerVersion = '' then
  begin
    FIBConnection := Metadata.GetConnection as IZInterbase6Connection;
    FServerVersion := GetISC_StringInfo(FIBConnection.GetPlainDriver,
      FIBConnection.GetDBHandle, isc_info_version, FIBConnection.GetConSettings);
    FIsFireBird := ZFastCode.Pos('Firebird', FServerVersion) > 0;
    FProductVersion := Copy(FServerVersion, ZFastCode.Pos(DBProvider[FIsFireBird],
      FServerVersion)+8+Ord(not FIsFireBird)+1, Length(FServerVersion));
    I := ZFastCode.Pos('.', FProductVersion);
    FHostVersion := StrToInt(Copy(FProductVersion, 1, I-1))*1000000;
    if ZFastCode.Pos(' ', FProductVersion) > 0 then //possible beta or alfa release
      tmp := Copy(FProductVersion, I+1, ZFastCode.Pos(' ', FProductVersion)-I-1)
    else
      tmp := Copy(FProductVersion, I+1, MaxInt);
    FHostVersion := FHostVersion + StrToInt(tmp)*1000;
  end;
end;

function TZInterbase6DatabaseInfo.GetHostVersion: Integer;
begin
  Result := FHostVersion;
end;

function TZInterbase6DatabaseInfo.HostIsFireBird: Boolean;
begin
  Result := FIsFireBird;
end;

// Increased size for FB 3.0+
function TZInterbase6DatabaseInfo.GetMaxSQLDASize: LongWord;
begin
  if FIsFireBird and (FHostVersion >= 3000000) then
    Result := 10*1024*1024 //might be much more! 4GB? 10MB sounds enough / roundtrip
  else
    Result := 64*1024; //64KB by default
end;

// FB 2.5+: binary hex string inside SQL
function TZInterbase6DatabaseInfo.SupportsBinaryInSQL: Boolean;
begin
  Result := FIsFireBird and (FHostVersion >= 2005000);
end;

// FB 2.0+: SQL-compliant syntax "NEXT VALUE FOR" for sequences
function TZInterbase6DatabaseInfo.SupportsNextValueFor: Boolean;
begin
  Result := FIsFireBird and (FHostVersion >= 2000000);
end;

// FB 2.0+: has TRIM internal function
function TZInterbase6DatabaseInfo.SupportsTrim: Boolean;
begin
  Result := FIsFireBird and (FHostVersion >= 2000000);
end;

{**
  What's the name of this database product?
  @return database product name
}
function TZInterbase6DatabaseInfo.GetDatabaseProductName: string;
begin
  Result := DBProvider[FIsFireBird];
end;

{**
  What's the version of this database product?
  @return database version
}
function TZInterbase6DatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := FProductVersion;
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZInterbase6DatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for Interbase and Firebird';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZInterbase6DatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZInterbase6DatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets the version of the server.
  @returns the version of the server.
}
function TZInterbase6DatabaseInfo.GetServerVersion: string;
begin
  Result := FServerVersion;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZInterbase6DatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZInterbase6DatabaseInfo.GetSQLKeywords: string;
begin
  Result := 'ACTIVE,AFTER,ASCENDING,BASE_NAME,BEFORE,BLOB,' +
    'CACHE,CHECK_POINT_LENGTH,COMPUTED,CONDITIONAL,CONTAINING,' +
    'CSTRING,DATABASE,RDB$DB_KEY,DEBUG,DESCENDING,DO,ENTRY_POINT,' +
    'EXIT,FILE,FILTER,FUNCTION,GDSCODE,GENERATOR,GEN_ID,' +
    'GROUP_COMMIT_WAIT_TIME,IF,INACTIVE,INPUT_TYPE,' +
    'LOGFILE,LOG_BUFFER_SIZE,MANUAL,MAXIMUM_SEGMENT,MERGE,MESSAGE,' +
    'MODULE_NAME,NCHAR,NUM_LOG_BUFFERS,OUTPUT_TYPE,OVERFLOW,PAGE,' +
    'PAGES,PAGE_SIZE,PARAMETER,PASSWORD,PLAN,POST_EVENT,PROTECTED,' +
    'RAW_PARTITIONS,RESERV,RESERVING,RETAIN,RETURNING_VALUES,RETURNS,' +
    'SEGMENT,SHADOW,SHARED,SINGULAR,SNAPSHOT,SORT,STABILITY,STARTS,' +
    'STARTING,STATISTICS,SUB_TYPE,SUSPEND,TRIGGER,VARIABLE,RECORD_VERSION,' +
    'WAIT,WHILE,WORK,POSITION,USER,CURRENCY,OPTION,DATE,START,END,' +
    'READ,PARENT,TYPE'+
    {Ticket #63: http://sourceforge.net/p/zeoslib/tickets/62/}
    ',DEC,TIME,MIN,MAX'+
    {FireBird 3.0}
    ',DETERMINISTIC,OVER,RETURN,SCROLL,SQLSTATE';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZInterbase6DatabaseInfo.GetNumericFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZInterbase6DatabaseInfo.GetStringFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZInterbase6DatabaseInfo.GetSystemFunctions: string;
begin
  Result := '';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZInterbase6DatabaseInfo.GetTimeDateFunctions: string;
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
function TZInterbase6DatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '\';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZInterbase6DatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '$';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := False;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZInterbase6DatabaseInfo.GetSchemaTerm: string;
begin
  Result := '';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZInterbase6DatabaseInfo.GetProcedureTerm: string;
begin
  Result := 'PROCEDURE';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZInterbase6DatabaseInfo.GetCatalogTerm: string;
begin
  Result := '';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZInterbase6DatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := True;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := True;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := True;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZInterbase6DatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
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
function TZInterbase6DatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 1024;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 31;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 32767;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 32767;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := 31;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 198;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 27;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 32664;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := False;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxStatementLength: Integer;
begin
  // Increased size for FB 3.0+
  // See release notes. Can be used by the legacy API too, if Karol Bieniaszewski
  // is right at http://tracker.firebirdsql.org/browse/CORE-1117?focusedCommentId=31493&page=com.atlassian.jira.plugin.system.issuetabpanels%3Acomment-tabpanel#action_31493
  if FIsFireBird and (FHostVersion >= 3000000) {and Client Library Version >= 3000000} then
    Result := 10*1024*1024 //Release notes say there is an hard coded limit at 10MB
  else
    Result := 64*1024; //64KB by default
end;
{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 31;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 16;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZInterbase6DatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 31;
end;

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZInterbase6DatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  Result := tiSerializable;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZInterbase6DatabaseInfo.SupportsTransactionIsolationLevel(
  const Level: TZTransactIsolationLevel): Boolean;
begin
  Result := Level in [tiNone, tiRepeatableRead, tiReadCommitted, tiSerializable]
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := False;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := True;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZInterbase6DatabaseInfo.SupportsResultSetType(
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
function TZInterbase6DatabaseInfo.SupportsResultSetConcurrency(
  const _Type: TZResultSetType; const Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := (_Type = rtScrollInsensitive) and (Concurrency = rcReadOnly);
end;

{**
  Does the Database support binding arrays? Is the ZDbc ready for this?
  @return <code>true</code> if the DataBase allows it.
}
function TZInterbase6DatabaseInfo.SupportsArrayBindings: Boolean;
begin
  // we use the execute block syntax that is only available from Firebird 2.0 on
  Result := FIsFireBird and (FHostVersion >= 2000000)
end;

{ TZInterbase6DatabaseMetadata }

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZInterbase6DatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZInterbase6DatabaseInfo.Create(Self);
end;

function TZInterbase6DatabaseMetadata.CreateStatement: IZStatement;
var Connection: IZConnection;
begin
  Connection := GetConnection;
  Result := Connection.CreateStatementWithParams(FInfo);
end;

procedure TZInterbase6DatabaseMetadata.BeforeDestruction;
begin
  inherited;
  FreeAndNil(FInfo);
end;

function TZInterbase6DatabaseMetadata.ConstructNameCondition(Pattern: string;
  Column: string): string;
begin
  if HasNoWildcards(Pattern) then begin
    Result := Inherited ConstructnameCondition(Pattern,Column)
  end else begin
    if not (GetDatabaseInfo as IZInterbaseDatabaseInfo).SupportsTrim then begin
      //Old FireBird do NOT support 'trim'
      //-> raise exception to find bugs in Software...
      if Pattern = '%'
      then Result := ''
      else raise EZSQLException.Create('Wildcard searches are not suported with Interbase and Firebird 1.5 and 1.0. Use IZDatabaseMetadata.AddEscapeCharToWildcards to escape wildcards in table names.')
    end else begin
      // add trim because otherwise the like condition will not find the table columns
      // because they are padded with spaces in Firebird
      Result := Inherited ConstructnameCondition(Pattern,'trim('+Column+')');
    end;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // encoding unknown - parameter not used intentionally
function TZInterbase6DatabaseMetadata.UncachedGetTriggers(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const TriggerNamePattern: string): IZResultSet;
var
  SQL: string;
  LTriggerNamePattern: string;
  LTableNamePattern: string;
begin
  LTriggerNamePattern := ConstructNameCondition(TriggerNamePattern,
    'RDB$TRIGGER_NAME');
  LTableNamePattern := ConstructNameCondition(TableNamePattern,
    'RDB$RELATION_NAME');

  SQL := 'SELECT NULL AS TRIGGER_CAT, NULL AS TRIGGER_SCHEM,'
    + ' RDB$TRIGGER_NAME, RDB$RELATION_NAME,'
    + ' RDB$TRIGGER_TYPE, RDB$TRIGGER_INACTIVE,'
    + ' RDB$TRIGGER_SOURCE, RDB$DESCRIPTION FROM RDB$TRIGGERS'
    + ' WHERE 1=1'
    + AppendCondition(LTriggerNamePattern) + AppendCondition(LTableNamePattern);

  Result := CopyToVirtualResultSet(
    CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(TriggersColumnsDynArray));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

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
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // encoding unknown - parameter not used intentionally
function TZInterbase6DatabaseMetadata.UncachedGetProcedures(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string): IZResultSet;
var
  SQL: string;
  LProcedureNamePattern: string;
begin
  LProcedureNamePattern := ConstructNameCondition(ProcedureNamePattern,
    'RDB$PROCEDURE_NAME');

  SQL := Format(
    'SELECT NULL AS PROCEDURE_CAT, NULL AS PROCEDURE_SCHEM,'
    + ' RDB$PROCEDURE_NAME AS PROCEDURE_NAME, NULL AS PROCEDURE_OVERLOAD,'
    + ' NULL AS RESERVED1, NULL AS RESERVED2, RDB$DESCRIPTION AS REMARKS,'
    + ' case when RDB$PROCEDURE_OUTPUTS IS NULL then %d else %d end AS PROCEDURE_TYPE'
    + ' FROM RDB$PROCEDURES'
    + ' WHERE 1=1' + AppendCondition(LProcedureNamePattern),
    [Ord(prtNoResult), Ord(prtReturnsResult)]);

  Result := CopyToVirtualResultSet(
    CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(ProceduresColumnsDynArray));
end;
{$IFDEF FPC} {$POP} {$ENDIF}
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
function TZInterbase6DatabaseMetadata.UncachedGetProcedureColumns(const Catalog: string;
  const SchemaPattern: string; const ProcedureNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
const
  PROCEDURE_NAME_Index   = FirstDbcIndex;
  PARAMETER_NAME_Index   = FirstDbcIndex + 1;
  PARAMETER_TYPE_Index   = FirstDbcIndex + 2;
  FIELD_TYPE_Index       = FirstDbcIndex + 3;
  FIELD_SUB_TYPE_Index   = FirstDbcIndex + 4;
  FIELD_SCALE_Index      = FirstDbcIndex + 5;
//FIELD_LENGTH_Index    = FirstDbcIndex + 6; - not used
  DESCRIPTION_Index     = FirstDbcIndex + 7;
  FIELD_PRECISION_Index = FirstDbcIndex + 8;
  NULL_FLAG_Index       = FirstDbcIndex + 9;
  CHARACTER_SET_ID_Index= FirstDbcIndex +10;
var
  SQL: string;
  Len: NativeUInt;
  LProcedureNamePattern, LColumnNamePattern: string;
  TypeName, SubTypeName: Integer;
begin
  Result := inherited UncachedGetProcedureColumns(Catalog, SchemaPattern, ProcedureNamePattern, ColumnNamePattern);

  LProcedureNamePattern := ConstructNameCondition(ProcedureNamePattern,
    'P.RDB$PROCEDURE_NAME');
  LColumnNamePattern := ConstructNameCondition(ColumnNamePattern,
    'PP.RDB$PARAMETER_NAME');

  SQL := GetDatabaseInfo.GetServerVersion;
  if (ZFastCode.Pos('Interbase 5', SQL) > 0) or (ZFastCode.Pos('V5.', SQL) > 0) then
    SQL := ' SELECT P.RDB$PROCEDURE_NAME, PP.RDB$PARAMETER_NAME,'
      + ' PP.RDB$PARAMETER_TYPE, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE,'
      + ' F.RDB$FIELD_SCALE, F.RDB$FIELD_LENGTH, F.RDB$NULL_FLAG,'
      + ' PP.RDB$DESCRIPTION, F.RDB$FIELD_SCALE as RDB$FIELD_PRECISION,'
      + ' F.RDB$NULL_FLAG, F.RDB$CHARACTER_SET_ID FROM RDB$PROCEDURES P'
      + ' JOIN RDB$PROCEDURE_PARAMETERS PP ON P.RDB$PROCEDURE_NAME'
      + '=PP.RDB$PROCEDURE_NAME JOIN RDB$FIELDS F ON PP.RDB$FIELD_SOURCE'
      + '=F.RDB$FIELD_NAME '
      + ' WHERE 1=1 '+AppendCondition(LProcedureNamePattern) + AppendCondition(LColumnNamePattern)
      + ' ORDER BY  P.RDB$PROCEDURE_NAME,'
      + ' PP.RDB$PARAMETER_TYPE, PP.RDB$PARAMETER_NUMBER'
  else
    SQL := ' SELECT P.RDB$PROCEDURE_NAME, PP.RDB$PARAMETER_NAME,'
      + ' PP.RDB$PARAMETER_TYPE, F.RDB$FIELD_TYPE, F.RDB$FIELD_SUB_TYPE,'
      + ' F.RDB$FIELD_SCALE, F.RDB$FIELD_LENGTH, F.RDB$NULL_FLAG,'
      + ' PP.RDB$DESCRIPTION, F.RDB$FIELD_PRECISION, F.RDB$NULL_FLAG, '
      + ' F.RDB$CHARACTER_SET_ID '
      + ' FROM RDB$PROCEDURES P JOIN RDB$PROCEDURE_PARAMETERS PP ON'
      + ' P.RDB$PROCEDURE_NAME = PP.RDB$PROCEDURE_NAME '
      + ' JOIN RDB$FIELDS F ON PP.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME '
      + ' WHERE 1=1 '+AppendCondition(LProcedureNamePattern) + AppendCondition(LColumnNamePattern)
      + ' ORDER BY  P.RDB$PROCEDURE_NAME,'
      + ' PP.RDB$PARAMETER_TYPE, PP.RDB$PARAMETER_NUMBER';

  with CreateStatement.ExecuteQuery(SQL) do
  begin
    while Next do
    begin
      TypeName := GetInt(FIELD_TYPE_Index);
      // For text fields subtype = 0, we get codepage number instead
      if TypeName in [blr_text, blr_text2, blr_varying, blr_varying2, blr_cstring, blr_cstring2] then
        SubTypeName := GetInt(CHARACTER_SET_ID_Index)
      else
        SubTypeName := GetInt(FIELD_SUB_TYPE_Index);

      Result.MoveToInsertRow;
      Result.UpdatePAnsiChar(ProcColProcedureNameIndex, GetPAnsiChar(PROCEDURE_NAME_Index, Len), @Len);
      Result.UpdatePAnsiChar(ProcColColumnNameIndex, GetPAnsiChar(PARAMETER_NAME_Index, Len), @Len);
      case GetInt(PARAMETER_TYPE_Index) of
        0: Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctIn));
        1: Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctOut));
        else
          Result.UpdateInt(ProcColColumnTypeIndex, Ord(pctUnknown));
      end;

      Result.UpdateInt(ProcColDataTypeIndex,
        Ord(ConvertInterbase6ToSqlType(TypeName, SubTypeName, GetInt(FIELD_SCALE_Index),
          ConSettings.CPType))); //DATA_TYPE
      Result.UpdatePAnsiChar(ProcColTypeNameIndex,GetPAnsiChar(FIELD_TYPE_Index, Len),  @Len);
      Result.UpdateInt(ProcColPrecisionIndex, GetInt(FIELD_PRECISION_Index));
      Result.UpdateNull(ProcColLengthIndex);    //BUFFER_LENGTH
      Result.UpdateInt(ProcColScaleIndex, GetInt(FIELD_SCALE_Index));
      Result.UpdateInt(ProcColRadixIndex, 10);
      Result.UpdateInt(ProcColNullableIndex, GetInt(NULL_FLAG_Index));
      Result.UpdatePAnsiChar(ProcColRemarksIndex, GetPAnsiChar(DESCRIPTION_Index, Len), @Len);
      Result.InsertRow;
    end;
    Close;
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
function TZInterbase6DatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  SQL, TableNameCondition: string;
  I: Integer;
begin
  Result := inherited UncachedGetTables(Catalog, SchemaPattern, TableNamePattern, Types);

  TableNameCondition := ConstructNameCondition(TableNamePattern,
    'RDB$RELATION_NAME');

  SQL := 'SELECT DISTINCT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM,'
    + ' RDB$RELATION_NAME AS TABLE_NAME, '
    + ' CASE WHEN RDB$VIEW_SOURCE IS NOT NULL THEN ''VIEW'' '
    + '      WHEN RDB$SYSTEM_FLAG = 1 THEN ''SYSTEM TABLE'' '
    + '      ELSE ''TABLE'' END AS TABLE_TYPE, '
    + ' RDB$DESCRIPTION AS REMARKS FROM RDB$RELATIONS '
    + ' WHERE 1=1' + AppendCondition(TableNameCondition);
  if Types <> nil then begin
    SQL := SQL + ' AND (';
    for I := 0 to High(Types) do begin
      if I > 0 then
        SQL := SQL + ' OR ';
      if Types[I] = 'VIEW' then
        SQL := SQL + 'RDB$VIEW_SOURCE IS NOT NULL'
      else if Types[I] = 'SYSTEM TABLE' then
        SQL := SQL + 'RDB$SYSTEM_FLAG = 1'
      else
        SQL := SQL + '(RDB$SYSTEM_FLAG = 0 AND RDB$VIEW_SOURCE IS NULL)'
        end;
    SQL := SQL +')';
  end;
  SQL := SQL + 'ORDER BY RDB$RELATION_NAME';

  Result := CopyToVirtualResultSet(
    CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(TableColumnsDynArray));
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
function TZInterbase6DatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TablesTypes: array [0..2] of String = ('TABLE', 'VIEW', 'SYSTEM TABLE');
var
  I: Integer;
begin
  Result := inherited UncachedGetTableTypes;

  for I := Low(TablesTypes) to High(TablesTypes) do
  begin
    Result.MoveToInsertRow;
    Result.UpdateString(TableTypeColumnTableTypeIndex, TablesTypes[I]);
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
const
  cDefault: RawByteString = 'DEFAULT';
  cNowQ: RawByteString = '''NOW''';
  cNowI: RawByteString = '"NOW"';
  cCURRENT_DATE: RawByteString = 'CURRENT_DATE';
  cCURRENT_TIME: RawByteString = 'CURRENT_TIME';
  cCURRENT_TIMESTAMP: RawByteString = 'CURRENT_TIMESTAMP';

function TZInterbase6DatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
const
  RELATION_NAME_Index         = FirstDbcIndex;
  FIELD_NAME_Index            = FirstDbcIndex + 1;
  FIELD_POSITION_Index        = FirstDbcIndex + 2;
  NULL_FLAG_Index             = FirstDbcIndex + 3;
//  DEFAULT_VALUE_Index         = FirstDbcIndex + 4; - not used
  FIELD_LENGTH_Index          = FirstDbcIndex + 5;
  FIELD_SCALE_Index           = FirstDbcIndex + 6;
  TYPE_NAME_Index             = FirstDbcIndex + 7;
  FIELD_TYPE_Index            = FirstDbcIndex + 8;
  FIELD_SUB_TYPE_Index        = FirstDbcIndex + 9;
  DESCRIPTION_Index           = FirstDbcIndex + 10;
  CHARACTER_LENGTH_Index      = FirstDbcIndex + 11;
  FIELD_PRECISION_Index       = FirstDbcIndex + 12;
  DEFAULT_SOURCE_Index        = FirstDbcIndex + 13;
  DEFAULT_SOURCE_DOMAIN_Index = FirstDbcIndex + 14;
  COMPUTED_SOURCE_Index       = FirstDbcIndex + 15;
  CHARACTER_SET_ID_Index      = FirstDbcIndex + 16;

var
  SQL, ColumnName: String;
  BLRSubType, SubTypeName, FieldScale, FieldLength, Precision: Integer;
  LTableNamePattern, LColumnNamePattern: string;
  SQLType: TZSQLType;
  L: NativeUInt;
  P: PAnsiChar;
label Str_Size;
begin
  Result := inherited UncachedGetColumns(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);

    LTableNamePattern := ConstructNameCondition(TableNamePattern,
      'a.RDB$RELATION_NAME');
    LColumnNamePattern := ConstructNameCondition(ColumnNamePattern,
      'a.RDB$FIELD_NAME');
  SQL := GetDatabaseInfo.GetServerVersion;
  if (ZFastCode.Pos('Interbase 5', SQL) > 0) or (ZFastCode.Pos('V5.', SQL) > 0) then
    SQL := 'SELECT a.RDB$RELATION_NAME, a.RDB$FIELD_NAME, a.RDB$FIELD_POSITION,'
      + ' a.RDB$NULL_FLAG,  null as RDB$DEFAULT_VALUE, b.RDB$FIELD_LENGTH,'
      + ' b.RDB$FIELD_SCALE,c.RDB$TYPE_NAME, b.RDB$FIELD_TYPE,'
      + ' b.RDB$FIELD_SUB_TYPE, b.RDB$DESCRIPTION, b.RDB$CHARACTER_LENGTH,'
      + ' b.RDB$FIELD_SCALE as RDB$FIELD_PRECISION, a.RDB$DEFAULT_SOURCE, b.RDB$DEFAULT_SOURCE'
      + ' as RDB$DEFAULT_SOURCE_DOMAIN, b.RDB$COMPUTED_SOURCE'
      + ' , b.RDB$CHARACTER_SET_ID FROM RDB$RELATION_FIELDS a'
      + ' JOIN RDB$FIELDS b ON (b.RDB$FIELD_NAME = a.RDB$FIELD_SOURCE)'
      + ' LEFT JOIN RDB$TYPES c ON b.RDB$FIELD_TYPE = c.RDB$TYPE'
      + ' and c.RDB$FIELD_NAME = ''RDB$FIELD_TYPE'''
      + ' WHERE 1=1 '+ AppendCondition(LTableNamePattern) + AppendCondition(LColumnNamePattern)
      + ' ORDER BY a.RDB$RELATION_NAME, a.RDB$FIELD_POSITION'
  else
    SQL := ' SELECT a.RDB$RELATION_NAME, a.RDB$FIELD_NAME, a.RDB$FIELD_POSITION,'
      + ' a.RDB$NULL_FLAG, a.RDB$DEFAULT_VALUE, b.RDB$FIELD_LENGTH,'
      + ' b.RDB$FIELD_SCALE, c.RDB$TYPE_NAME, b.RDB$FIELD_TYPE,'
      + ' b.RDB$FIELD_SUB_TYPE, b.RDB$DESCRIPTION, b.RDB$CHARACTER_LENGTH,'
      + ' b.RDB$FIELD_PRECISION, a.RDB$DEFAULT_SOURCE, b.RDB$DEFAULT_SOURCE'
      + ' as RDB$DEFAULT_SOURCE_DOMAIN,b.RDB$COMPUTED_SOURCE'
      + ' , b.RDB$CHARACTER_SET_ID FROM RDB$RELATION_FIELDS a'
      + ' JOIN RDB$FIELDS b ON (b.RDB$FIELD_NAME = a.RDB$FIELD_SOURCE)'
      + ' LEFT JOIN RDB$TYPES c ON (b.RDB$FIELD_TYPE = c.RDB$TYPE'
      + ' and c.RDB$FIELD_NAME = ''RDB$FIELD_TYPE'')'
      + ' WHERE 1=1 ' +  AppendCondition(LTableNamePattern) + AppendCondition(LColumnNamePattern)
      + ' ORDER BY a.RDB$RELATION_NAME, a.RDB$FIELD_POSITION';

  with CreateStatement.ExecuteQuery(SQL) do begin
    while Next do begin
      BLRSubType := GetInt(FIELD_TYPE_Index);
      // For text fields subtype = 0, we get codepage number instead
      if BLRSubType in [blr_text, blr_text2, blr_varying, blr_varying2, blr_cstring, blr_cstring2] then
        SubTypeName := GetInt(CHARACTER_SET_ID_Index)
      else
        SubTypeName := GetInt(FIELD_SUB_TYPE_Index);
      FieldScale := GetInt(FIELD_SCALE_Index);
      ColumnName := GetString(FIELD_NAME_Index);
      FieldLength := GetInt(FIELD_LENGTH_Index);
      Precision   := GetInt(FIELD_PRECISION_Index);

      P := GetPAnsiChar(COMPUTED_SOURCE_Index, L);
      if (P<>nil) and (L > 0) then  //AVZ -- not isNull(14) was not working correcly here could be ' ' - subselect
        //Computed by Source  & Sub Selects  //AVZ
        if ((BLRSubType = blr_int64) and (FieldScale < 0)) then
          SubTypeName := RDB_NUMBERS_NUMERIC; // Fix for 0 subtype which removes decimals

      Result.MoveToInsertRow;
      Result.UpdateString(TableNameIndex, GetString(RELATION_NAME_Index));    //TABLE_NAME
      Result.UpdateString(ColumnNameIndex, ColumnName);    //COLUMN_NAME

      SQLType := ConvertInterbase6ToSqlType(BLRSubType, SubTypeName, FieldScale,
        ConSettings.CPType);

      Result.UpdateInt(TableColColumnTypeIndex, Ord(SQLType));
      // TYPE_NAME
      case BLRSubType of
        blr_short, blr_long, blr_int64: begin
            case SubTypeName of
              RDB_NUMBERS_NUMERIC: Result.UpdateRawByteString(TableColColumnTypeNameIndex, 'NUMERIC');
              RDB_NUMBERS_DECIMAL: Result.UpdateRawByteString(TableColColumnTypeNameIndex, 'DECIMAL');
              else case BLRSubType of
                  blr_short:  Result.UpdateRawByteString(TableColColumnTypeNameIndex, 'SMALLINT');
                  blr_long:   Result.UpdateRawByteString(TableColColumnTypeNameIndex, 'INTEGER' );
                  blr_int64:  Result.UpdatePAnsiChar(TableColColumnTypeNameIndex, GetPAnsiChar(TYPE_NAME_Index, L), @L);
                end;
            end;
            Result.UpdateInt(TableColColumnDecimalDigitsIndex, -FieldScale);
            Result.UpdateInt(TableColColumnSizeIndex, Precision);
        end;
        blr_varying: begin
            Result.UpdateRawByteString(TableColColumnTypeNameIndex, 'VARCHAR'); // Instead of VARYING
            goto Str_Size;
          end;
        blr_text: begin
            Result.UpdatePAnsiChar(TableColColumnTypeNameIndex, GetPAnsiChar(TYPE_NAME_Index, L), @L);
Str_Size:   Result.UpdateInt(TableColColumnCharOctetLengthIndex, FieldLength*GetInt(CHARACTER_LENGTH_Index));   //CHAR_OCTET_LENGTH
            Result.UpdateInt(TableColColumnSizeIndex, FieldLength);
          end;
        else
          Result.UpdatePAnsiChar(TableColColumnTypeNameIndex, GetPAnsiChar(TYPE_NAME_Index, L), @L);
      end;
      Result.UpdateInt(TableColColumnNumPrecRadixIndex, 10);   //NUM_PREC_RADIX

      if GetInt(NULL_FLAG_Index) <> 0
      then Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNoNulls))   //NULLABLE
      else Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNullable));
      P := GetPAnsiChar(DESCRIPTION_Index, L);
      if P <> nil then begin
        L := min(L,255);
        Result.UpdatePAnsiChar(TableColColumnRemarksIndex, P, @L);   //REMARKS
      end;
      P := GetPAnsiChar(DEFAULT_SOURCE_Index, L);
      if L = 0 then
        P := GetPAnsiChar(DEFAULT_SOURCE_DOMAIN_Index, L);
      if P <> nil then begin
        ZSysUtils.Trim(L, P);
        if (L > 8) and SameText(P, Pointer(cDefault), Length(cDefault)) then begin
          Inc(P, 8);
          Dec(L, 8);
          IF (L = 5) and (SameText(P, Pointer(cNowQ), 5) or SameText(P, Pointer(cNowI), 5)) then
            case BLRSubType of
              blr_sql_date:   begin
                                P := Pointer(cCURRENT_DATE);
                                L := Length(cCURRENT_DATE);
                              end;
              blr_sql_time:   begin
                                P := Pointer(cCURRENT_TIME);
                                L := Length(cCURRENT_TIME);
                              end;
              blr_timestamp:  begin
                                P := Pointer(cCURRENT_TIMESTAMP);
                                L := Length(cCURRENT_TIMESTAMP);
                              end;
            end;
        end;
        Result.UpdatePAnsiChar(TableColColumnColDefIndex, P, @L);   //COLUMN_DEF
      end;

      Result.UpdateInt(TableColColumnSQLDataTypeIndex, BLRSubType);   //SQL_DATA_TYPE
      //Result.UpdateNull(TableColColumnSQLDateTimeSubIndex);   //SQL_DATETIME_SUB
      Result.UpdateInt(TableColColumnCharOctetLengthIndex, GetInt(FIELD_SCALE_Index));   //CHAR_OCTET_LENGTH
      Result.UpdateInt(TableColColumnOrdPosIndex, GetInt(FIELD_POSITION_Index)+ 1);   //ORDINAL_POSITION
      Result.UpdateString(TableColColumnIsNullableIndex, YesNoStrs[IsNull(NULL_FLAG_Index)]);   //IS_NULLABLE
     // Result.UpdateNull(TableColColumnAutoIncIndex); //AUTO_INCREMENT

      Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, IC.IsCaseSensitive(ColumnName)); //CASE_SENSITIVE

      Result.UpdateBoolean(TableColColumnSearchableIndex, True); //SEARCHABLE
      if isNull(COMPUTED_SOURCE_Index) and (ColumnName <> 'RDB$DB_KEY') then begin
        Result.UpdateBoolean(TableColColumnWritableIndex, True); //WRITABLE
        Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, True); //DEFINITELYWRITABLE
        Result.UpdateBoolean(TableColColumnReadonlyIndex, False); //READONLY
      end else begin
        Result.UpdateBoolean(TableColColumnWritableIndex, False); //WRITABLE
        Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, False); //DEFINITELYWRITABLE
        Result.UpdateBoolean(TableColColumnReadonlyIndex, True); //READONLY
      end;
      Result.InsertRow;
    end;
    Close;
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
function TZInterbase6DatabaseMetadata.UncachedGetColumnPrivileges(const Catalog: string;
  const Schema: string; const Table: string; const ColumnNamePattern: string): IZResultSet;
var SQL, LColumnNameCondition, LTable: String;
begin
  LTable := ConstructNameCondition(AddEscapeCharToWildcards(Table), 'a.RDB$RELATION_NAME');
  LColumnNameCondition := AppendCondition(ConstructNameCondition(AddEscapeCharToWildcards(ColumnNamePattern), 'a.RDB$FIELD_NAME'));

  SQL := 'SELECT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM, '
    + ' a.RDB$RELATION_NAME AS TABLE_NAME, COALESCE(a.RDB$FIELD_NAME, RF.RDB$FIELD_NAME) AS COLUMN_NAME,'
    + ' a.RDB$GRANTOR AS GRANTOR, a.RDB$USER AS GRANTEE, '
    + ' CASE a.RDB$PRIVILEGE WHEN ''S'' THEN ''SELECT'' '
    + '                      WHEN ''I'' THEN ''INSERT'' '
    + '                      WHEN ''U'' THEN ''UPDATE'' '
    + '                      WHEN ''D'' THEN ''DELETE'' '
    + '                      WHEN ''R'' THEN ''REFERENCE'' '
    + ' ELSE NULL END AS PRIVILEGE, '
    + ' CASE WHEN a.RDB$GRANTOR = a.RDB$USER THEN ''YES'' ELSE ''NO'' END AS IS_GRANTABLE'
    + ' FROM RDB$USER_PRIVILEGES a '
    + ' INNER JOIN RDB$TYPES b on a.RDB$OBJECT_TYPE = b.RDB$TYPE '
    + ' LEFT JOIN RDB$RELATION_FIELDS RF ON RF.RDB$RELATION_NAME = a.RDB$RELATION_NAME AND a.RDB$FIELD_NAME IS NULL'
    + ' WHERE b.RDB$FIELD_NAME=''RDB$OBJECT_TYPE'''
    + AppendCondition(LTable) + LColumnNameCondition
    + ' and b.RDB$TYPE_NAME IN (''RELATION'', ''VIEW'','
    + ' ''COMPUTED_FIELD'', ''FIELD'' ) AND'
    + ' RF.RDB$FIELD_NAME NOT IN (SELECT b.RDB$FIELD_NAME FROM RDB$USER_PRIVILEGES b where b.RDB$RELATION_NAME = a.RDB$RELATION_NAME AND b.RDB$FIELD_NAME IS NOT NULL)'
    + ' ORDER BY a.RDB$FIELD_NAME, a.RDB$PRIVILEGE';

  Result := CopyToVirtualResultSet(
    CreateStatement.ExecuteQuery(SQL),
     ConstructVirtualResultSet(TableColPrivColumnsDynArray));
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
function TZInterbase6DatabaseMetadata.UncachedGetTablePrivileges(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string): IZResultSet;
var SQL, LTableNamePattern: String;
begin
  Result:=inherited UncachedGetTablePrivileges(Catalog, SchemaPattern, TableNamePattern);

  LTableNamePattern := ConstructNameCondition(TableNamePattern, 'a.RDB$RELATION_NAME');

  SQL := 'SELECT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM, '
    + ' a.RDB$RELATION_NAME as TABLE_NAME, a.RDB$GRANTOR AS GRANTOR, '
    + ' a.RDB$USER AS GRANTEE, a.RDB$PRIVILEGE AS PRIVILEGE,'
    + ' CASE WHEN a.RDB$USER = a.RDB$GRANTOR THEN ''YES'' ELSE ''NO'' END AS IS_GRANTABLE'
    + ' FROM RDB$USER_PRIVILEGES a,'
    + ' RDB$TYPES b WHERE a.RDB$OBJECT_TYPE = b.RDB$TYPE AND '
    + ' b.RDB$TYPE_NAME IN (''RELATION'', ''VIEW'', ''COMPUTED_FIELD'','
    + ' ''FIELD'' ) AND a.RDB$FIELD_NAME IS NULL '
    + AppendCondition(LTableNamePattern)
    + ' ORDER BY a.RDB$RELATION_NAME, a.RDB$PRIVILEGE';

  Result := CopyToVirtualResultSet(
    CreateStatement.ExecuteQuery(SQL),
     ConstructVirtualResultSet(TablePrivColumnsDynArray));
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
function TZInterbase6DatabaseMetadata.UncachedGetVersionColumns(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := inherited UncachedGetVersionColumns(Catalog, Schema, Table);

  Result.MoveToInsertRow;
  Result.UpdateNull(TableColVerScopeIndex);
  Result.UpdateString(TableColVerColNameIndex, 'ctid');
//  Result.UpdateInt(TableColVerDataTypeIndex, GetSQLType('tid')); //FIX IT
  Result.UpdateString(TableColVerTypeNameIndex, 'tid');
  Result.UpdateNull(TableColVerColSizeIndex);
  Result.UpdateNull(TableColVerBufLengthIndex);
  Result.UpdateNull(TableColVerDecimalDigitsIndex);
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
function TZInterbase6DatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
var
  SQL: string;
  LTable: string;
begin
  LTable := ConstructNameCondition(AddEscapeCharToWildcards(Table), 'a.RDB$RELATION_NAME');

  SQL := 'SELECT NULL AS TABLE_CAT, NULL AS TABLE_SCHEM,'
    + ' a.RDB$RELATION_NAME AS TABLE_NAME, b.RDB$FIELD_NAME AS COLUMN_NAME,'
    + ' b.RDB$FIELD_POSITION+1 AS KEY_SEQ, a.RDB$INDEX_NAME AS PK_NAME'
    + ' FROM RDB$RELATION_CONSTRAINTS a JOIN RDB$INDEX_SEGMENTS b ON'
    + ' (a.RDB$INDEX_NAME = b.RDB$INDEX_NAME)'
    + ' WHERE RDB$CONSTRAINT_TYPE = ''PRIMARY KEY'''
    + AppendCondition(LTable)
    + ' ORDER BY a.RDB$RELATION_NAME, b.RDB$FIELD_NAME';

  Result := CopyToVirtualResultSet(
    CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(PrimaryKeyColumnsDynArray));
end;

function GetRuleTypeString(const RuleFieldWithAlias, AsField: String): String;
begin
  RESULT := 'CASE '+RuleFieldWithAlias+
    ' WHEN ''RESTRICT'' THEN '+IntToStr(Ord(ikRestrict))+
    ' WHEN ''NO ACTION'' THEN '+IntToStr(Ord(ikNoAction))+
    ' WHEN ''CASCADE'' THEN '+IntToStr(Ord(ikCascade))+
    ' WHEN ''SET DEFAULT'' THEN '+IntToStr(Ord(ikSetDefault))+
    ' WHEN ''SET NULL'' THEN '+IntToStr(Ord(ikSetNull))+
    ' ELSE '+IntToStr(Ord(ikNotDeferrable))+ //impossible!
    ' END AS '+AsField
end;

function GetDereferabilityTypeString(const RelationContraintAlias: String): String;
begin
  //usally a nop, just prepared, FB does not support deferrable constraints yet
  Result := 'CASE '+RelationContraintAlias+'.RDB$INITIALLY_DEFERRED'+
    ' WHEN ''YES'' THEN '+IntToStr(Ord(ikInitiallyDeferred))+
    ' ELSE CASE '+RelationContraintAlias+'.RDB$DEFERRABLE'+
    ' WHEN ''YES'' THEN '+IntToStr(Ord(ikInitiallyImmediate))+
    ' ELSE '+IntToStr(Ord(ikNotDeferrable))+ ' END '+
    ' END AS DEFERRABILITY'
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
function TZInterbase6DatabaseMetadata.UncachedGetImportedKeys(const Catalog: string;
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
function TZInterbase6DatabaseMetadata.UncachedGetExportedKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
begin
  Result := UncachedGetCrossReference(Catalog, Schema, Table, '', '', '');
end;

{**
  EgonHugeist:
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
function TZInterbase6DatabaseMetadata.UncachedGetCrossReference(
  const PrimaryCatalog: string; const PrimarySchema: string;
  const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
  const ForeignTable: string): IZResultSet;
var
  SQL, PKTable, FKTable: String;
begin
  PKTable := ConstructNameCondition(AddEscapeCharToWildcards(PrimaryTable), 'RELC_PK.RDB$RELATION_NAME');
  FKTable := ConstructNameCondition(AddEscapeCharToWildcards(ForeignTable), 'RELC_FK.RDB$RELATION_NAME');

  SQL := 'SELECT '+
    'NULL AS PKTABLE_CAT, '+
    'NULL AS PKTABLE_SCHEM, '+
    'RELC_PK.RDB$RELATION_NAME AS PKTABLE_NAME, '+
    'IS_PK.RDB$FIELD_NAME AS PKCOLUMN_NAME, '+
    'NULL AS FKTABLE_CAT, '+
    'NULL AS FKTABLE_SCHEM, '+
    'RELC_FK.RDB$RELATION_NAME as FKTABLE_NAME, '+
    'IS_FK.RDB$FIELD_NAME AS FKCOLUMN_NAME, '+
    'IS_FK.RDB$FIELD_POSITION+1 AS KEY_SEQ, '+
    GetRuleTypeString('REFC_FK.RDB$UPDATE_RULE', 'UPDATE_RULE')+', '+
    GetRuleTypeString('REFC_FK.RDB$DELETE_RULE', 'DELETE_RULE')+', '+
    'RELC_FK.RDB$CONSTRAINT_NAME AS FK_NAME, '+
    'RELC_PK.RDB$INDEX_NAME as PK_NAME, '+
    GetDereferabilityTypeString('RELC_FK')+
    ' FROM RDB$RELATION_CONSTRAINTS RELC_FK'+
    ' INNER JOIN RDB$INDEX_SEGMENTS IS_FK ON IS_FK.RDB$INDEX_NAME = RELC_FK.RDB$INDEX_NAME'+
    ' INNER JOIN RDB$REF_CONSTRAINTS REFC_FK ON RELC_FK.RDB$CONSTRAINT_NAME = REFC_FK.RDB$CONSTRAINT_NAME'+
    ' INNER JOIN RDB$RELATION_CONSTRAINTS RELC_PK ON RELC_PK.RDB$CONSTRAINT_NAME = REFC_FK.RDB$CONST_NAME_UQ'+
    ' INNER JOIN RDB$INDEX_SEGMENTS IS_PK ON IS_PK.RDB$INDEX_NAME = RELC_PK.RDB$INDEX_NAME'+
    ' WHERE RELC_FK.RDB$CONSTRAINT_TYPE = ''FOREIGN KEY'' '+
    AppendCondition(PKTable)+ AppendCondition(FKTable)+
    ' ORDER BY RELC_FK.RDB$RELATION_NAME, IS_FK.RDB$FIELD_POSITION';
  Result := CopyToVirtualResultSet(
    CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(CrossRefColumnsDynArray));
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
function TZInterbase6DatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
const
  RDB_TYPE_Index      = FirstDbcIndex + 0;
  RDB_TYPE_NAME_Index = FirstDbcIndex + 1;
var
  SQL: string;
  Len: NativeUInt;
begin
  Result := inherited UncachedGetTypeInfo;

  SQL := 'SELECT RDB$TYPE, RDB$TYPE_NAME FROM RDB$TYPES' +
    ' WHERE RDB$FIELD_NAME = ''RDB$FIELD_TYPE''';
  with CreateStatement.ExecuteQuery(SQL) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      Result.UpdatePAnsiChar(TypeInfoTypeNameIndex, GetPAnsiChar(RDB_TYPE_NAME_Index, Len), @Len);
      Result.UpdateInt(TypeInfoDataTypeIndex, Ord(ConvertInterbase6ToSqlType(
        GetInt(RDB_TYPE_Index), 0, 10, ConSettings.CPType))); //added a scale > 4 since type_info doesn't deal with user defined scale
      Result.UpdateInt(TypeInfoPecisionIndex, 9);
      Result.UpdateInt(TypeInfoNullAbleIndex, Ord(ntNoNulls));
      Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, false);
      Result.UpdateBoolean(TypeInfoSearchableIndex, false);
      Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, false);
      Result.UpdateBoolean(TypeInfoAutoIncrementIndex, false);
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
function TZInterbase6DatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
const
  RDB_RELATION_NAME_Index      = FirstDbcIndex + 0;
  RDB_UNIQUE_FLAG_Index        = FirstDbcIndex + 1;
  RDB_INDEX_NAME_Index         = FirstDbcIndex + 2;
  RDB_FIELD_POSITION_Index     = FirstDbcIndex + 3;
  RDB_FIELD_NAME_Index         = FirstDbcIndex + 4;
  //RDB_SEGMENT_COUNT_Index      = FirstDbcIndex + 5; unused
  RDB_PAGE_NUMBER_Index        = FirstDbcIndex + 6;
var
  SQL : string;
  LTable: String;
  L: NativeUint;
begin
  LTable := ConstructNameCondition(AddEscapeCharToWildcards(Table), 'I.RDB$RELATION_NAME');

  Result := inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  SQL := 'SELECT I.RDB$RELATION_NAME, I.RDB$UNIQUE_FLAG, I.RDB$INDEX_NAME,'
    + ' ISGMT.RDB$FIELD_POSITION,	ISGMT.RDB$FIELD_NAME, I.RDB$INDEX_TYPE,'
    + ' I.RDB$SEGMENT_COUNT, COUNT (DISTINCT P.RDB$PAGE_NUMBER)'
    + ' FROM RDB$INDICES I JOIN RDB$INDEX_SEGMENTS ISGMT ON'
    + ' I.RDB$INDEX_NAME = ISGMT.RDB$INDEX_NAME JOIN RDB$RELATIONS R ON'
    + ' (R.RDB$RELATION_NAME = I.RDB$RELATION_NAME) JOIN RDB$PAGES P ON'
    + ' (P.RDB$RELATION_ID = R.RDB$RELATION_ID AND P.RDB$PAGE_TYPE = 7'
    + ' OR P.RDB$PAGE_TYPE = 6) WHERE'
    + IfThen(Unique, ' I.RDB$UNIQUE_FLAG = 1 AND')
    + ' I.RDB$RELATION_NAME != ''''' + AppendCondition(LTable)
    + ' GROUP BY'
    + ' I.RDB$INDEX_NAME, I.RDB$RELATION_NAME, I.RDB$UNIQUE_FLAG,'
    + ' ISGMT.RDB$FIELD_POSITION, ISGMT.RDB$FIELD_NAME, I.RDB$INDEX_TYPE,'
    + ' I.RDB$SEGMENT_COUNT ORDER BY 1,2,3,4';

  with CreateStatement.ExecuteQuery(SQL) do
  begin
    while Next do
    begin
      Result.MoveToInsertRow;
      //Result.UpdateNull(CatalogNameIndex); //TABLE_CAT
      //Result.UpdateNull(SchemaNameIndex); //TABLE_SCHEM
      Result.UpdatePAnsiChar(TableNameIndex, GetPAnsiChar(RDB_RELATION_NAME_Index, L), @L); //TABLE_NAME, RDB$RELATION_NAME
      Result.UpdateBoolean(IndexInfoColNonUniqueIndex, not GetBoolean(RDB_UNIQUE_FLAG_Index)); //NON_UNIQUE, RDB$UNIQUE_FLAG
      //Result.UpdateNull(IndexInfoColIndexQualifierIndex); //INDEX_QUALIFIER
      Result.UpdatePAnsiChar(IndexInfoColIndexNameIndex, GetPAnsiChar(RDB_INDEX_NAME_Index, L), @L); //INDEX_NAME, RDB$INDEX_NAME
      Result.UpdateInt(IndexInfoColTypeIndex, Ord(ntNoNulls)); //TYPE
      Result.UpdateInt(IndexInfoColOrdPositionIndex, GetInt(RDB_FIELD_POSITION_Index){$IFNDEF GENERIC_INDEX} + 1{$ENDIF}); //ORDINAL_POSITION, RDB$FIELD_POSITION
      Result.UpdatePAnsiChar(IndexInfoColColumnNameIndex, GetPAnsiChar(RDB_FIELD_NAME_Index, L), @L); //COLUMN_NAME, RDB$FIELD_NAME
      //Result.UpdateNull(IndexInfoColAscOrDescIndex); //ASC_OR_DESC
      //Result.UpdateNull(IndexInfoColCardinalityIndex); //CARDINALITY
      Result.UpdateInt(IndexInfoColPagesIndex, GetInt(RDB_PAGE_NUMBER_Index)); //PAGES, COUNT (DISTINCT P.RDB$PAGE_NUMBER)
      //Result.UpdateNull(IndexInfoColFilterConditionIndex); //FILTER_CONDITION
      Result.InsertRow;
    end;
    Close;
  end;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // encoding unknown - parameter not used intentionally
function TZInterbase6DatabaseMetadata.UncachedGetSequences(
  const Catalog: string; const SchemaPattern: string;
  const SequenceNamePattern: string): IZResultSet;
var
  SQL: string;
  LSequenceNamePattern: string;
begin
  LSequenceNamePattern := ConstructNameCondition(SequenceNamePattern,
    'RDB$GENERATOR_NAME');

  SQL := 'SELECT NULL AS SEQUENCE_CAT, NULL AS SEQUENCE_SCHEM,'
    + ' RDB$GENERATOR_NAME FROM RDB$GENERATORS'
    + ' WHERE (RDB$SYSTEM_FLAG IS NULL OR RDB$SYSTEM_FLAG = 0)'
    + AppendCondition(LSequenceNamePattern);

  Result := CopyToVirtualResultSet(
    CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(SequenceColumnsDynArray));
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TZInterbase6DatabaseMetadata.SetUTF8CodePageInfo;
begin
  if FInfo = nil then
    FInfo := TStringList.Create;
  FInfo.Values[DS_Props_IsMetadataResultSet] := 'True';
end;

{**
  Gets the used Collation and CharacterSet of spezified Object.

  @param catalog a catalog name; "" retrieves those without a
  catalog; null means drop catalog name from the selection criteria
  @param schema a schema name; "" and Catolog "" retrieves nothing
  @param table a table name; "" retrieves the Schema Colloation and CharacterSet
  @param ColumnNamePattern ColumnPattern;"" retrieves the
    Table(if @param TablePattern is set) or
    Schema(if @param TablePattern is NULL)
      Colloation and CharacterSet
  @return <code>ResultSet</code> - each row is a Collation, CharacterSet, ID,
    and ByteLength per Char of speziefied Object
}
function TZInterbase6DatabaseMetadata.UncachedGetCollationAndCharSet(const Catalog, SchemaPattern,
  TableNamePattern, ColumnNamePattern: string): IZResultSet; //EgonHugeist
const
  CHARACTER_SET_NAME_Index   = FirstDbcIndex + 0;
  DEFAULT_COLLATE_NAME_Index = FirstDbcIndex + 1;
  CHARACTER_SET_ID_Index     = FirstDbcIndex + 2;
  BYTES_PER_CHARACTER_Index  = FirstDbcIndex + 3;
var
  SQL, LCatalog: string;
  LColumnNamePattern, LTableNamePattern: string;
begin
  if Catalog = '' then
  begin
    if SchemaPattern <> '' then
      LCatalog := SchemaPattern
    else
      LCatalog := FDatabase;
  end
  else
    LCatalog := Catalog;
  LTableNamePattern := ConstructNameCondition(TableNamePattern,'R.RDB$RELATION_NAME');
  LColumnNamePattern := ConstructNameCondition(ColumnNamePattern,'R.RDB$FIELD_NAME');

  Result := inherited UncachedGetCollationAndCharSet(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);

  if (LCatalog <> '') and (TableNamePattern <> '') and (ColumnNamePattern <> '') then
  begin
    SQL := 'SELECT C.RDB$CHARACTER_SET_NAME, C.RDB$DEFAULT_COLLATE_NAME,'
      + ' C.RDB$CHARACTER_SET_ID, C.RDB$BYTES_PER_CHARACTER'
      + ' FROM RDB$RELATION_FIELDS R'
      + ' right join RDB$FIELDS F on R.RDB$FIELD_SOURCE = F.RDB$FIELD_NAME'
      + ' left join RDB$CHARACTER_SETS C on C.RDB$CHARACTER_SET_ID = F.RDB$CHARACTER_SET_ID'
      + ' left join RDB$TYPES T on F.RDB$FIELD_TYPE = T.RDB$TYPE'
      + ' where C.RDB$CHARACTER_SET_NAME <> ''NONE'' AND T.RDB$FIELD_NAME=''RDB$FIELD_TYPE'''
      + AppendCondition(LColumnNamePattern)+AppendCondition(LTableNamePattern)
      + ' order by R.RDB$FIELD_POSITION';
    with CreateStatement.ExecuteQuery(SQL) do
    begin
      if Next then
      begin
        Result.MoveToInsertRow;
        //Result.UpdateString(CatalogNameIndex, LCatalog);   //COLLATION_CATALOG
        //Result.UpdateString(SchemaNameIndex, LCatalog);   //COLLATION_SCHEMA
        Result.UpdateString(TableNameIndex, TableNamePattern); //COLLATION_TABLE
        Result.UpdateString(ColumnNameIndex, ColumnNamePattern);//COLLATION_COLUMN
        Result.UpdateString(CollationNameIndex, GetString(DEFAULT_COLLATE_NAME_Index)); //COLLATION_NAME
        Result.UpdateString(CharacterSetNameIndex, GetString(CHARACTER_SET_NAME_Index)); //CHARACTER_SET_NAME
        Result.UpdateSmall(CharacterSetIDIndex, GetSmall(CHARACTER_SET_ID_Index)); //CHARACTER_SET_ID
        Result.UpdateSmall(CharacterSetSizeIndex, GetSmall(BYTES_PER_CHARACTER_Index)); //CHARACTER_SET_SIZE
        Result.InsertRow;
        Close;
        Exit;
      end;
      Close;
    end;
  end;
  {Brings Defaults for Table or Database up}
  SQL := 'SELECT D.RDB$CHARACTER_SET_NAME, CS.RDB$DEFAULT_COLLATE_NAME, '+
         'CS.RDB$CHARACTER_SET_ID, CS.RDB$BYTES_PER_CHARACTER '+
         'FROM RDB$DATABASE D '+
         'LEFT JOIN RDB$CHARACTER_SETS CS on '+
         'D.RDB$CHARACTER_SET_NAME = CS.RDB$CHARACTER_SET_NAME';
  with CreateStatement.ExecuteQuery(SQL) do
  begin
    if Next then
    begin
      Result.MoveToInsertRow;
      Result.UpdateString(CatalogNameIndex, LCatalog);   //COLLATION_CATALOG
      Result.UpdateString(SchemaNameIndex, LCatalog);   //COLLATION_SCHEMA
      Result.UpdateString(TableNameIndex, TableNamePattern); //COLLATION_TABLE
      Result.UpdateNull(ColumnNameIndex);//COLLATION_COLUMN
      Result.UpdateString(CollationNameIndex, GetString(DEFAULT_COLLATE_NAME_Index)); //COLLATION_NAME
      Result.UpdateString(CharacterSetNameIndex, GetString(CHARACTER_SET_NAME_Index)); //CHARACTER_SET_NAME
      Result.UpdateSmall(CharacterSetIDIndex, GetSmall(CHARACTER_SET_ID_Index)); //CHARACTER_SET_ID
      Result.UpdateSmall(CharacterSetSizeIndex, GetSmall(BYTES_PER_CHARACTER_Index)); //CHARACTER_SET_SIZE
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets the supported CharacterSets:
  @return <code>ResultSet</code> - each row is a CharacterSetName and it's ID
}
function TZInterbase6DatabaseMetadata.UncachedGetCharacterSets: IZResultSet; //EgonHugeist
var
  SQL: string;
begin
  SQL := 'SELECT RDB$CHARACTER_SET_NAME, RDB$CHARACTER_SET_ID FROM RDB$CHARACTER_SETS';

  Result := CopyToVirtualResultSet(
    CreateStatement.ExecuteQuery(SQL),
    ConstructVirtualResultSet(CharacterSetsColumnsDynArray));
end;
{$ENDIF ZEOS_DISABLE_INTERBASE} //if set we have an empty unit
end.
