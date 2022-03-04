{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          SQLite Database Connectivity Classes           }
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
{   https://zeoslib.sourceforge.io/ (FORUM)               }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZDbcSqLiteMetadata;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
uses
  Types, Classes, SysUtils, ZSysUtils, ZDbcIntfs, ZDbcMetadata,
  ZCompatibility, ZDbcSQLiteUtils;

type

  // technobot 2008-06-28 - methods moved as is from TZSQLiteDatabaseMetadata:
  {** Implements SQLite Database Information. }
  TZSQLiteDatabaseInfo = class(TZAbstractDatabaseInfo)
//    function UncachedGetUDTs(const Catalog: string; const SchemaPattern: string;
//      const TypeNamePattern: string; const Types: TIntegerDynArray): IZResultSet; override;
  public
    // database/driver/server info:
    function GetDatabaseProductName: string; override;
    function GetDatabaseProductVersion: string; override;
    function GetDriverName: string; override;
//    function GetDriverVersion: string; override; -> Same as parent
    function GetDriverMajorVersion: Integer; override;
    function GetDriverMinorVersion: Integer; override;
//    function GetServerVersion: string; -> Not implemented

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
    function SupportsTransactionIsolationLevel(const {%H-}Level: TZTransactIsolationLevel):
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
//    function GetIdentifierQuoteString: string; override; -> Not implemented
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

  {** Implements SQLite Database Metadata. }
  TZSQLiteDatabaseMetadata = class(TZAbstractDatabaseMetadata)
  protected
    function CreateDatabaseInfo: IZDatabaseInfo; override; // technobot 2008-06-28

    function UncachedGetTables(const Catalog: string; const {%H-}SchemaPattern: string;
      const TableNamePattern: string; const Types: TStringDynArray): IZResultSet; override;
//    function UncachedGetSchemas: IZResultSet; override;  -> not implemented
    function UncachedGetCatalogs: IZResultSet; override;
    function UncachedGetTableTypes: IZResultSet; override;
    function UncachedGetColumns(const Catalog: string; const SchemaPattern: string;
      const TableNamePattern: string; const ColumnNamePattern: string): IZResultSet; override;
//    function UncachedGetTablePrivileges(const Catalog: string; const SchemaPattern: string;  -> not implemented
//      const TableNamePattern: string): IZResultSet; override;
//    function UncachedGetColumnPrivileges(const Catalog: string; const Schema: string;  -> not implemented
//      const Table: string; const ColumnNamePattern: string): IZResultSet; override;
    function UncachedGetPrimaryKeys(const Catalog: string; const Schema: string;
      const Table: string): IZResultSet; override;
//    function UncachedGetImportedKeys(const Catalog: string; const Schema: string;
//      const Table: string): IZResultSet; override;
//    function UncachedGetExportedKeys(const Catalog: string; const Schema: string;
//      const Table: string): IZResultSet; override;
//    function UncachedGetCrossReference(const PrimaryCatalog: string; const PrimarySchema: string;
//      const PrimaryTable: string; const ForeignCatalog: string; const ForeignSchema: string;
//      const ForeignTable: string): IZResultSet; override;
    function UncachedGetIndexInfo(const Catalog: string; const Schema: string; const Table: string;
      Unique: Boolean; Approximate: Boolean): IZResultSet; override;
//     function UncachedGetSequences(const Catalog: string; const SchemaPattern: string;
//      const SequenceNamePattern: string): IZResultSet; virtual; -> Not implemented
//    function UncachedGetProcedures(const Catalog: string; const SchemaPattern: string;
//      const ProcedureNamePattern: string): IZResultSet; override;
//    function UncachedGetProcedureColumns(const Catalog: string; const SchemaPattern: string;
//      const ProcedureNamePattern: string; const ColumnNamePattern: string):
//      IZResultSet; override;
//    function UncachedGetVersionColumns(const Catalog: string; const Schema: string;
//      const Table: string): IZResultSet; override;
    function UncachedGetTypeInfo: IZResultSet; override;
    function UncachedGetCharacterSets: IZResultSet; override; //EgonHugeist
  end;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit

uses
  ZDbcUtils, ZDbcSqLite, ZFastCode, ZSelectSchema, ZMatchPattern,
  ZEncoding;

{ TZSQLiteDatabaseInfo }

//----------------------------------------------------------------------
// First, a variety of minor information about the target database.

{**
  What's the name of this database product?
  @return database product name
}
function TZSQLiteDatabaseInfo.GetDatabaseProductName: string;
begin
  Result := 'SQLite';
end;

{**
  What's the version of this database product?
  @return database version
}
function TZSQLiteDatabaseInfo.GetDatabaseProductVersion: string;
begin
  Result := '';
end;

{**
  What's the name of this JDBC driver?
  @return JDBC driver name
}
function TZSQLiteDatabaseInfo.GetDriverName: string;
begin
  Result := 'Zeos Database Connectivity Driver for SQLite';
end;

{**
  What's this JDBC driver's major version number?
  @return JDBC driver major version
}
function TZSQLiteDatabaseInfo.GetDriverMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  What's this JDBC driver's minor version number?
  @return JDBC driver minor version number
}
function TZSQLiteDatabaseInfo.GetDriverMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Does the database use a file for each table?
  @return true if the database uses a local file for each table
}
function TZSQLiteDatabaseInfo.UsesLocalFilePerTable: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return false.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsMixedCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.StoresUpperCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.StoresLowerCaseIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case unquoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.StoresMixedCaseIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case sensitive and as a result store them in mixed case?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver will always return true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in upper case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.StoresUpperCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in lower case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.StoresLowerCaseQuotedIdentifiers: Boolean;
begin
  Result := False;
end;

{**
  Does the database treat mixed case quoted SQL identifiers as
  case insensitive and store them in mixed case?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.StoresMixedCaseQuotedIdentifiers: Boolean;
begin
  Result := True;
end;

{**
  Gets a comma-separated list of all a database's SQL keywords
  that are NOT also SQL92 keywords.
  @return the list
}
function TZSQLiteDatabaseInfo.GetSQLKeywords: string;
begin
  Result := 'ALL,AND,BETWEEN,CASE,CHECK,COLLATE,COMMIT,CONSTRAINT,'
    + 'DEFAULT,DEFERRABLE,DISTINCT,ELSE,EXCEPT,FOREIGN,GLOB,'
    + 'IN,INTERSECT,ISNULL,LIMIT,'
    + 'NOT,NOTNULL,REFERENCES,ROLLBACK,'
    + 'THEN,TRANSACTION,UNION,UNIQUE,USING,WHEN,'
    + 'ABORT,AFTER,ASC,ATTACH,BEFORE,BEGIN,DEFERRED,CASCADE,CLUSTER,CONFLICT,'
    + 'COPY,CROSS,DATABASE,DELIMITERS,DESC,DETACH,EACH,END,EXPLAIN,FAIL,'
    + 'FULL,IGNORE,IMMEDIATE,INITIALLY,INNER,INSTEAD,LEFT,MATCH,NATURAL,'
    + 'OF,OFFSET,OUTER,PRAGMA,RAISE,REPLACE,RESTRICT,RIGHT,ROW,STATEMENT,'
    + 'TEMP,TEMPORARY,TRIGGER,VACUUM,VIEW';
end;

{**
  Gets a comma-separated list of math functions.  These are the
  X/Open CLI math function names used in the JDBC function escape
  clause.
  @return the list
}
function TZSQLiteDatabaseInfo.GetNumericFunctions: string;
begin
  Result := 'ABS,MAX,MIN,RANDOM,ROUND';
end;

{**
  Gets a comma-separated list of string functions.  These are the
  X/Open CLI string function names used in the JDBC function escape
  clause.
  @return the list
}
function TZSQLiteDatabaseInfo.GetStringFunctions: string;
begin
  Result := 'LENGTH,LIKE,LOWER,SOUNDEX,SUBSTRING,UPPER';
end;

{**
  Gets a comma-separated list of system functions.  These are the
  X/Open CLI system function names used in the JDBC function escape
  clause.
  @return the list
}
function TZSQLiteDatabaseInfo.GetSystemFunctions: string;
begin
  Result := 'LAST_INSERT_ROWID,SQLITE_VERSION,TYPEOF';
end;

{**
  Gets a comma-separated list of time and date functions.
  @return the list
}
function TZSQLiteDatabaseInfo.GetTimeDateFunctions: string;
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
function TZSQLiteDatabaseInfo.GetSearchStringEscape: string;
begin
  Result := '/';
end;

{**
  Gets all the "extra" characters that can be used in unquoted
  identifier names (those beyond a-z, A-Z, 0-9 and _).
  @return the string containing the extra characters
}
function TZSQLiteDatabaseInfo.GetExtraNameCharacters: string;
begin
  Result := '';
end;

//--------------------------------------------------------------------
// Functions describing which features are supported.

{**
  Are expressions in "ORDER BY" lists supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsExpressionsInOrderBy: Boolean;
begin
  Result := False;
end;

{**
  Can an "ORDER BY" clause use columns not in the SELECT statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsOrderByUnrelated: Boolean;
begin
  Result := False;
end;

{**
  Is some form of "GROUP BY" clause supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsGroupBy: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause use columns not in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsGroupByUnrelated: Boolean;
begin
  Result := True;
end;

{**
  Can a "GROUP BY" clause add columns not in the SELECT
  provided it specifies all the columns in the SELECT?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsGroupByBeyondSelect: Boolean;
begin
  Result := True;
end;

{**
  Is the SQL Integrity Enhancement Facility supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsIntegrityEnhancementFacility: Boolean;
begin
  Result := False;
end;

{**
  What's the database vendor's preferred term for "schema"?
  @return the vendor term
}
function TZSQLiteDatabaseInfo.GetSchemaTerm: string;
begin
  Result := '';
end;

{**
  What's the database vendor's preferred term for "procedure"?
  @return the vendor term
}
function TZSQLiteDatabaseInfo.GetProcedureTerm: string;
begin
  Result := '';
end;

{**
  What's the database vendor's preferred term for "catalog"?
  @return the vendor term
}
function TZSQLiteDatabaseInfo.GetCatalogTerm: string;
begin
  Result := 'database';
end;

{**
  What's the separator between catalog and table name?
  @return the separator string
}
function TZSQLiteDatabaseInfo.GetCatalogSeparator: string;
begin
  Result := '.';
end;

{**
  Can a schema name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSchemasInDataManipulation: Boolean;
begin
  Result := True;
end;

{**
  Can a schema name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSchemasInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSchemasInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSchemasInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a schema name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSchemasInPrivilegeDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a data manipulation statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsCatalogsInDataManipulation: Boolean;
begin
  Result := True;
end;

{**
  Can a catalog name be used in a procedure call statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsCatalogsInProcedureCalls: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a table definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsCatalogsInTableDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in an index definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsCatalogsInIndexDefinitions: Boolean;
begin
  Result := False;
end;

{**
  Can a catalog name be used in a privilege definition statement?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsCatalogsInPrivilegeDefinitions: Boolean;
begin
  Result := True;
end;

{**
  Is positioned DELETE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsPositionedDelete: Boolean;
begin
  Result := False;
end;

{**
  Is positioned UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsPositionedUpdate: Boolean;
begin
  Result := False;
end;

{**
  Is SELECT for UPDATE supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSelectForUpdate: Boolean;
begin
  Result := False;
end;

{**
  Are stored procedure calls using the stored procedure escape
  syntax supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsStoredProcedures: Boolean;
begin
  Result := False;
end;

{**
  Are subqueries in comparison expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSubqueriesInComparisons: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'exists' expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSubqueriesInExists: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in 'in' statements supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSubqueriesInIns: Boolean;
begin
  Result := True;
end;

{**
  Are subqueries in quantified expressions supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsSubqueriesInQuantifieds: Boolean;
begin
  Result := True;
end;

{**
  Are correlated subqueries supported?
  A JDBC Compliant<sup><font size=-2>TM</font></sup> driver always returns true.
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsCorrelatedSubqueries: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsUnion: Boolean;
begin
  Result := True;
end;

{**
  Is SQL UNION ALL supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsUnionAll: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across commits?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZSQLiteDatabaseInfo.SupportsOpenCursorsAcrossCommit: Boolean;
begin
  Result := True;
end;

{**
  Can cursors remain open across rollbacks?
  @return <code>true</code> if cursors always remain open;
        <code>false</code> if they might not remain open
}
function TZSQLiteDatabaseInfo.SupportsOpenCursorsAcrossRollback: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across commits?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZSQLiteDatabaseInfo.SupportsOpenStatementsAcrossCommit: Boolean;
begin
  Result := False;
end;

{**
  Can statements remain open across rollbacks?
  @return <code>true</code> if statements always remain open;
        <code>false</code> if they might not remain open
}
function TZSQLiteDatabaseInfo.SupportsOpenStatementsAcrossRollback: Boolean;
begin
  Result := False;
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
function TZSQLiteDatabaseInfo.GetMaxBinaryLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the max length for a character literal?
  @return max literal length;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxCharLiteralLength: Integer;
begin
  Result := 0;
end;

{**
  What's the limit on column name length?
  @return max column name length;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxColumnNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "GROUP BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxColumnsInGroupBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns allowed in an index?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxColumnsInIndex: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in an "ORDER BY" clause?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxColumnsInOrderBy: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a "SELECT" list?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxColumnsInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of columns in a table?
  @return max number of columns;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxColumnsInTable: Integer;
begin
  Result := 0;
end;

{**
  How many active connections can we have at a time to this database?
  @return max number of active connections;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxConnections: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum cursor name length?
  @return max cursor name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxCursorNameLength: Integer;
begin
  Result := 0;
end;

{**
  Retrieves the maximum number of bytes for an index, including all
  of the parts of the index.
  @return max index length in bytes, which includes the composite of all
   the constituent parts of the index;
   a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxIndexLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length allowed for a schema name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxSchemaNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a procedure name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxProcedureNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a catalog name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxCatalogNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a single row?
  @return max row size in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxRowSize: Integer;
begin
  Result := 0;
end;

{**
  Did getMaxRowSize() include LONGVARCHAR and LONGVARBINARY
  blobs?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.DoesMaxRowSizeIncludeBlobs: Boolean;
begin
  Result := True;
end;

{**
  What's the maximum length of an SQL statement?
  @return max length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxStatementLength: Integer;
begin
  Result := 65535;
end;

{**
  How many active statements can we have open at one time to this
  database?
  @return the maximum number of statements that can be open at one time;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxStatements: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a table name?
  @return max name length in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxTableNameLength: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum number of tables in a SELECT statement?
  @return the maximum number of tables allowed in a SELECT statement;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxTablesInSelect: Integer;
begin
  Result := 0;
end;

{**
  What's the maximum length of a user name?
  @return max user name length  in bytes;
       a result of zero means that there is no limit or the limit is not known
}
function TZSQLiteDatabaseInfo.GetMaxUserNameLength: Integer;
begin
  Result := 0;
end;

//----------------------------------------------------------------------

{**
  What's the database's default transaction isolation level?  The
  values are defined in <code>java.sql.Connection</code>.
  @return the default isolation level
  @see Connection
}
function TZSQLiteDatabaseInfo.GetDefaultTransactionIsolation:
  TZTransactIsolationLevel;
begin
  //https://sqlite.org/pragma.html#pragma_read_uncommitted
  Result := tiSerializable;
end;

{**
  Are transactions supported? If not, invoking the method
  <code>commit</code> is a noop and the isolation level is TRANSACTION_NONE.
  @return <code>true</code> if transactions are supported; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsTransactions: Boolean;
begin
  Result := True;
end;

{**
  Does this database support the given transaction isolation level?
  @param level the values are defined in <code>java.sql.Connection</code>
  @return <code>true</code> if so; <code>false</code> otherwise
  @see Connection
}
function TZSQLiteDatabaseInfo.SupportsTransactionIsolationLevel(
  const Level: TZTransactIsolationLevel): Boolean;
begin
  //https://sqlite.org/pragma.html#pragma_read_uncommitted
  Result := Level in [tiSerializable, tiReadUncommitted];
end;

{**
  Are both data definition and data manipulation statements
  within a transaction supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.
  SupportsDataDefinitionAndDataManipulationTransactions: Boolean;
begin
  Result := True;
end;

{**
  Are only data manipulation statements within a transaction
  supported?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.
  SupportsDataManipulationTransactionsOnly: Boolean;
begin
  Result := True;
end;

{**
  Does a data definition statement within a transaction force the
  transaction to commit?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.DataDefinitionCausesTransactionCommit: Boolean;
begin
  Result := True;
end;

{**
  Is a data definition statement within a transaction ignored?
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.DataDefinitionIgnoredInTransactions: Boolean;
begin
  Result := False;
end;

{**
  Does the database support the given result set type?
  @param type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsResultSetType(
  const _Type: TZResultSetType): Boolean;
begin
  Result := _Type = rtForwardOnly;
end;

{**
  Does the database support the concurrency type in combination
  with the given result set type?

  @param type defined in <code>java.sql.ResultSet</code>
  @param concurrency type defined in <code>java.sql.ResultSet</code>
  @return <code>true</code> if so; <code>false</code> otherwise
}
function TZSQLiteDatabaseInfo.SupportsResultSetConcurrency(
  const _Type: TZResultSetType; const Concurrency: TZResultSetConcurrency): Boolean;
begin
  Result := (_Type = rtForwardOnly) and (Concurrency = rcReadOnly);
end;


{ TZSQLiteDatabaseMetadata }

{**
  Constructs a database information object and returns the interface to it. Used
  internally by the constructor.
  @return the database information object interface
}
function TZSQLiteDatabaseMetadata.CreateDatabaseInfo: IZDatabaseInfo;
begin
  Result := TZSQLiteDatabaseInfo.Create(Self);
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
function TZSQLiteDatabaseMetadata.UncachedGetTables(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const Types: TStringDynArray): IZResultSet;
var
  WhereClause, SQL, Tmp_Catalog: string;
  function IncludedType(const TypeName: string): Boolean;
  var I: Integer;
  begin
    Result := False;
    for I := Low(Types) to High(Types) do
      Result := Result or (UpperCase(Types[I]) = TypeName);
    Result := Result or (Length(Types) = 0);
  end;

begin
  if IncludedType('TABLE')
  then WhereClause := 'TYPE=''table'''
  else WhereClause := '';
  if IncludedType('VIEW') then
    if WhereClause <> ''
    then WhereClause := '(' + WhereClause + ' OR TYPE=''view'')'
    else WhereClause := 'TYPE=''view''';

  if Catalog <> ''
  then Tmp_Catalog := Catalog
  else Tmp_Catalog := 'main';// 'null';

  SQL := 'SELECT '''+Tmp_Catalog +''' AS TABLE_CAT, NULL AS TABLE_SCHEM,'
    + ' TBL_NAME AS TABLE_NAME, UPPER(TYPE) AS TABLE_TYPE, NULL AS REMARKS'
    + ' FROM '+ Tmp_Catalog + '.SQLITE_MASTER WHERE ' + WhereClause+ ' AND TBL_NAME ';
  if (TableNamePattern <> '') and HasNoWildcards(TableNamePattern)
  then SQL := SQL + '= '''+ StripEscape(TableNamePattern)
  else SQL := SQL + 'LIKE ''' + ToLikeString(TableNamePattern);
  SQL := SQL + '''';
  Result := CopyToVirtualResultSet(
    GetConnection.CreateStatement.ExecuteQuery(SQL),
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
function TZSQLiteDatabaseMetadata.UncachedGetTableTypes: IZResultSet;
const
  TableTypeCount = 2;
  Types: array [1..TableTypeCount] of UTF8String = ('TABLE', 'VIEW');
var
  I: Integer;
begin
  Result:=inherited UncachedGetTableTypes;

  for I := 1 to TableTypeCount do
    begin
      Result.MoveToInsertRow;
      Result.UpdateUTF8String(TableTypeColumnTableTypeIndex, Types[I]);
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
function TZSQLiteDatabaseMetadata.UncachedGetColumns(const Catalog: string;
  const SchemaPattern: string; const TableNamePattern: string;
  const ColumnNamePattern: string): IZResultSet;
const
  cid_index = FirstDbcIndex;
  name_index = cid_index+1;
  type_index = name_index+1;
  notnull_index = type_index+1;
  dflt_value_index = notnull_index+1;
  pk_index = dflt_value_index+1;
var
  Temp_scheme, TempTableNamePattern: String;
  UndefinedVarcharAsStringLength: Integer;
  ResSet, TblRS: IZResultSet;
  TblTmp, SchemaTmp: RawByteString;
  TableTypes: TStringDynArray;
  procedure FillResult(const RS: IZResultSet; UndefinedVarcharAsStringLength: Integer; ColumnNamePattern: String;
    Const SchemaName, TableName: RawByteString);
  var
    Len: NativeUInt;
    Precision, Decimals: Integer;
    ColPatTemp, TypeTmp: RawByteString;
    SQLType: TZSQLType;
    P: PAnsiChar;
    CompareColLike, CompareEquals: Boolean;
    S: String;
    function IsAutoIncrement(const SchemaName, TableName: RawByteString; ColumnName: String): Boolean;
    var
      CreateSQL, CreateSQLUp: String;
      colIdx, aiIdx: Integer;
      SL: TStrings;
    begin
      Result := False;
      with GetStatement.ExecuteQuery('select sql from '+SchemaName+'.sqlite_master where name = '''+TableName+'''') do begin
        if Next
        then CreateSQL := GetString(FirstDbcIndex)
        else CreateSQL := '';
        Close;
      end;
      if CreateSQL = '' then Exit;
      CreateSQLUp := UpperCase(CreateSQL);
      aiIdx := ZFastCode.Pos('AUTOINCREMENT', CreateSQLUp);
      if (aiIdx > 0) then begin
        if IC.IsCaseSensitive(ColumnName) then
          ColumnName := IC.Quote(ColumnName);
        colIdx := ZFastCode.Pos(ColumnName, CreateSQL);
        if (colIdx > 0) and (colIdx < aiIdx) then begin
          colIdx := PosEx('INTEGER', CreateSQLUp, colIdx);
          CreateSQLUp := Copy(CreateSQLUp, colIdx+7, (aiIdx-colIdx)-7);
          CreateSQLUp := Trim(CreateSQLUp);
          SL := SplitString(CreateSQLUp, ' '#10);
          Result := (SL.Count = 2) and (SL[0] = 'PRIMARY') and (SL[1] = 'KEY');
          SL.Free;
        end;
      end;
    end;
  begin
    if (ColumnNamePattern <> '') and (ColumnNamePattern <> '%') then begin
      if HasNoWildcards(ColumnNamePattern) then begin//test for escaped wildcards
        CompareEquals := True;
        ColumnNamePattern := StripEscape(ColumnNamePattern);
        CompareColLike := False;
      end else begin
        S := StripEscape(ColumnNamePattern);
        CompareColLike := (S = ColumnNamePattern) and ((ZFastCode.Pos('_', ColumnNamePattern) > 0) or (ZFastCode.Pos('%', ColumnNamePattern) > 0));
        CompareEquals := not CompareColLike;
        ColumnNamePattern := S;
      end;
      ColumnNamePattern := NormalizePatternCase(ColumnNamePattern);
      {$IFDEF UNICODE}
      ColPatTemp := ZUnicodeToRaw(ColumnNamePattern, zCP_UTF8);
      {$ELSE}
      ColPatTemp := ColumnNamePattern;
      {$ENDIF}
    end else begin
      CompareColLike := False;
      CompareEquals := False;
      ColPatTemp := EmptyRaw;
    end;
    {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
    if RS <> nil then with RS do begin
      while Next do begin
        P := GetPAnsiChar(name_index, Len);
        if CompareColLike or CompareEquals then begin
          if (CompareColLike and not ZMatchPattern.Like(ColPatTemp, P, Len)) or
             (CompareEquals and ((NativeUint(Length(ColPatTemp)) <> Len) or not CompareMem(P, Pointer(ColPatTemp), Len))) then
            continue;
        end;
        Result.MoveToInsertRow;
        if SchemaName <> '' then
          Result.UpdateRawByteString(CatalogNameIndex, SchemaName);
        Result.UpdateRawByteString(TableNameIndex, TableName);
        Result.UpdatePAnsiChar(ColumnNameIndex, P, @Len);
        TypeTmp := GetRawByteString(type_index);
        SQLType := ConvertSQLiteTypeToSQLType(TypeTmp, UndefinedVarcharAsStringLength,
          Precision, Decimals, ConSettings.CPType);
        Result.UpdateSmall(TableColColumnTypeIndex, Ord(SQLType));

        Len := Length(TypeTmp);
        Result.UpdatePAnsiChar(TableColColumnTypeNameIndex, Pointer(TypeTmp), @Len);

        Result.UpdateInt(TableColColumnSizeIndex, Precision);  //Precision will be converted higher up
        if SQLType = stString then begin
          Result.UpdateInt(TableColColumnBufLengthIndex, (Precision shl 2) +1);
          Result.UpdateInt(TableColColumnCharOctetLengthIndex, Precision shl 2);
        end else if SQLType = stUnicodeString then begin
          Result.UpdateInt(TableColColumnBufLengthIndex, (Precision+1) shl 1);
          Result.UpdateInt(TableColColumnCharOctetLengthIndex, Precision shl 1);
        end else if SQLType = stBytes then
          Result.UpdateInt(TableColColumnBufLengthIndex, Precision)
        else if not (SQLType in [stAsciiStream, stUnicodeStream, stBinaryStream]) then
          Result.UpdateInt(TableColColumnBufLengthIndex, ZSQLTypeToBuffSize(SQLType));

        Result.UpdateInt(TableColColumnDecimalDigitsIndex, Decimals);
        Result.UpdateInt(TableColColumnNumPrecRadixIndex, 0);

        if GetInt(notnull_index) <> 0 then
        begin
          Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNoNulls));
          Result.UpdateRawByteString(TableColColumnIsNullableIndex, 'NO');
        end
        else
        begin
          Result.UpdateInt(TableColColumnNullableIndex, Ord(ntNullable));
          Result.UpdateRawByteString(TableColColumnIsNullableIndex, 'YES');
        end;

        P := GetPAnsiChar(dflt_value_index, Len);
        if Len > 0 then
          Result.UpdatePAnsiChar(TableColColumnColDefIndex, P, @Len);

        Result.UpdateInt(TableColColumnOrdPosIndex, GetInt(cid_index) +1);
        S := GetString(name_index);
        if (GetInt(pk_index) = 1) then begin
          Result.UpdateBoolean(TableColColumnAutoIncIndex, True); //the rowid is not automatically a AUTOINCREMENT attribute
          Result.UpdateBoolean(TableColColumnReadonlyIndex, (TypeTmp = 'INTEGER') and IsAutoIncrement(SchemaName, Result.GetUTF8String(TableNameIndex), S));
        end else begin
          Result.UpdateBoolean(TableColColumnAutoIncIndex, False);
          Result.UpdateBoolean(TableColColumnReadonlyIndex, False);
        end;
        Result.UpdateBoolean(TableColColumnCaseSensitiveIndex, IC.IsCaseSensitive(S));
        Result.UpdateBoolean(TableColColumnSearchableIndex, True);
        Result.UpdateBoolean(TableColColumnWritableIndex, True);
        Result.UpdateBoolean(TableColColumnDefinitelyWritableIndex, True);
        Result.UpdateBoolean(TableColColumnReadonlyIndex, False);

        Result.InsertRow;
      end;
      Close;
    end;
  end;
begin
  Result:=inherited UncachedGetColumns(Catalog, SchemaPattern, TableNamePattern, ColumnNamePattern);

  UndefinedVarcharAsStringLength := (GetConnection as IZSQLiteConnection).GetUndefinedVarcharAsStringLength;
  with GetStatement do begin
    if HasNoWildcards(TableNamePattern) and HasNoWildcards(SchemaPattern) and ((ColumnNamePattern = '') or (ColumnNamePattern = '%')) then begin
      TempTableNamePattern := StripEscape(TableNamePattern);
      TempTableNamePattern := NormalizePatternCase(TempTableNamePattern);
      {$IFDEF UNICODE}
      TblTmp := ZUnicodeToRaw(TempTableNamePattern, zCP_UTF8);
      {$ELSE}
      TblTmp := TempTableNamePattern;
      {$ENDIF}

      Temp_scheme := StripEscape(SchemaPattern);
      if Temp_scheme = '' then
        Temp_scheme := Catalog;
      if Temp_scheme = '' then
        Temp_scheme := 'main';
      {$IFDEF UNICODE}
      SchemaTmp := ZUnicodeToRaw(Temp_scheme, zCP_UTF8);
      {$ELSE}
      SchemaTmp := Temp_scheme;
      {$ENDIF}
      ResSet := GetStatement.ExecuteQuery('PRAGMA '+SchemaTmp+'.table_info('''+TblTmp+''')');
      FillResult(ResSet, UndefinedVarcharAsStringLength, ColumnNamePattern, SchemaTmp, TblTmp);
    end else begin
      {$IFDEF WITH_VAR_INIT_WARNING}TableTypes := nil;{$ENDIF}
      SetLength(TableTypes, 1);
      TableTypes[0] := 'TABLE';
      TblRS := GetTables(Catalog, SchemaPattern, TableNamePattern, TableTypes);
      while TblRS.Next do begin
        SchemaTmp := TblRS.GetRawByteString(CatalogNameIndex);
        TblTmp := TblRS.GetRawByteString(TableNameIndex);
        ResSet := GetStatement.ExecuteQuery('PRAGMA '+SchemaTmp+'.table_info('''+TblTmp+''')');
        FillResult(ResSet, UndefinedVarcharAsStringLength, ColumnNamePattern, SchemaTmp, TblTmp);
      end;
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
function TZSQLiteDatabaseMetadata.UncachedGetPrimaryKeys(const Catalog: string;
  const Schema: string; const Table: string): IZResultSet;
const
  cid_index = FirstDbcIndex;
  name_index = cid_index+1;
  {%H-}type_index = name_index+1;
  {%H-}notnull_index = type_index+1;
  {%H-}dflt_value_index = notnull_index+1;
  pk_index = dflt_value_index+1;
var
  Len: NativeUInt;
  Temp_scheme, Temp_Table: string;
  RS: IZResultSet;
  {$IFDEF UNICODE}
  Raw_Schema, Raw_Table: RawByteString;
  {$ENDIF}
begin
  Result:=inherited UncachedGetPrimaryKeys(Catalog, Schema, Table);

  if Schema = '' then
    if Catalog <> ''
    then Temp_scheme := Catalog
    else Temp_scheme := 'main'
  else Temp_scheme := Schema;
  Temp_Table := NormalizePatternCase(Table);
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  {$IFDEF UNICODE}
  Raw_Schema := ZUnicodeToRaw(Temp_scheme, zCP_UTF8);
  Raw_Table := ZUnicodeToRaw(Temp_Table, zCP_UTF8);
  {$ENDIF}
  RS := GetConnection.CreateStatement.ExecuteQuery(
    'PRAGMA '+{$IFDEF UNICODE}Raw_Schema{$ELSE}Temp_scheme{$ENDIF}+'.table_info('''+{$IFDEF UNICODE}Raw_Table{$ELSE}Temp_Table{$ENDIF}+''')');
  if RS <> nil then with RS do begin
    while Next do
    begin
      if GetInt(pk_index) = 0 then
        Continue;
      Result.MoveToInsertRow;
      Result.UpdateRawByteString(CatalogNameIndex, {$IFDEF UNICODE}Raw_Schema{$ELSE}Temp_scheme{$ENDIF});
      Result.UpdateRawByteString(TableNameIndex, {$IFDEF UNICODE}Raw_Table{$ELSE}Temp_Table{$ENDIF});
      Result.UpdatePAnsiChar(PrimaryKeyColumnNameIndex, GetPAnsiChar(name_index, Len), @Len);
      Result.UpdateInt(PrimaryKeyKeySeqIndex, GetInt(cid_index)+1);
      Result.InsertRow;
    end;
    Close;
  end;
  if Result.IsBeforeFirst then begin
    Result.MoveToInsertRow;
    Result.UpdateRawByteString(CatalogNameIndex, {$IFDEF UNICODE}Raw_Schema{$ELSE}Temp_scheme{$ENDIF});
    Result.UpdateRawByteString(TableNameIndex, {$IFDEF UNICODE}Raw_Table{$ELSE}Temp_Table{$ENDIF});
    Result.UpdateRawByteString(PrimaryKeyColumnNameIndex, 'rowid');
    Result.UpdateInt(PrimaryKeyKeySeqIndex, 0);
    Result.InsertRow;
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
function TZSQLiteDatabaseMetadata.UncachedGetTypeInfo: IZResultSet;
const
  MaxTypeCount = 22;
  TypeNames: array[1..MaxTypeCount] of UTF8String = (
    'BOOLEAN', 'TINYINT', 'SMALLINT', 'MEDIUMINT', 'INTEGER',
    'BIGINT', 'REAL', 'FLOAT', 'NUMERIC', 'DECIMAL', 'NUMBER',
    'DOUBLE', 'CHAR', 'VARCHAR', 'BINARY', 'VARBINARY', 'DATE',
    'TIME', 'DATETIME', 'TIMESTAMP', 'BLOB', 'TEXT');
  TypeCodes: array[1..MaxTypeCount] of TZSQLType = (
    stBoolean, stByte, stSmall, stInteger, stInteger, stLong,
    stFloat, stFloat, stDouble, stDouble, stDouble, stDouble,
    stString, {$IFDEF UNICODE}stUnicodeString{$ELSE}stString{$ENDIF},
    stBytes, stBytes, stDate, stTime, stTimestamp,
    stTimestamp, stBinaryStream, {$IFDEF UNICODE}stUnicodeStream{$ELSE}stAsciiStream{$ENDIF});
  TypePrecision: array[1..MaxTypeCount] of Integer = (
    -1, 2, 4, 9, 9, 16, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    -1, -1, -1, -1, -1);
var
  I: Integer;
begin
  Result:=inherited UncachedGetTypeInfo;

  for I := 1 to MaxTypeCount do
  begin
    Result.MoveToInsertRow;

    Result.UpdateUTF8String(TypeInfoTypeNameIndex, TypeNames[I]);
    Result.UpdateInt(TypeInfoDataTypeIndex, Ord(TypeCodes[I]));
    if TypePrecision[I] >= 0 then
      Result.UpdateInt(TypeInfoPecisionIndex, TypePrecision[I]);
    //else Result.UpdateNull(TypeInfoPecisionIndex);
    if TypeCodes[I] in [stString, stBytes, stDate, stTime,
      stTimeStamp, stBinaryStream, stAsciiStream, stUnicodeString] then
    begin
      Result.UpdateString(TypeInfoLiteralPrefixIndex, '''');
      Result.UpdateString(TypeInfoLiteralSuffixIndex, '''');
    end;
    Result.UpdateInt(TypeInfoNullAbleIndex, Ord(ntNullable));
    Result.UpdateBoolean(TypeInfoCaseSensitiveIndex, False);
    Result.UpdateBoolean(TypeInfoSearchableIndex, False);
    Result.UpdateBoolean(TypeInfoUnsignedAttributeIndex, False);
    Result.UpdateBoolean(TypeInfoFixedPrecScaleIndex, False);
    Result.UpdateBoolean(TypeInfoAutoIncrementIndex, TypeNames[I] = 'INTEGER');
    Result.UpdateInt(TypeInfoNumPrecRadix, 10);

    Result.InsertRow;
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
function TZSQLiteDatabaseMetadata.UncachedGetIndexInfo(const Catalog: string;
  const Schema: string; const Table: string; Unique: Boolean;
  Approximate: Boolean): IZResultSet;
const
  {%H-}main_seq_field_index = FirstDbcIndex;
  main_name_field_index = main_seq_field_index+1;
  main_unique_field_index = main_name_field_index+1;

  sub_seqno_field_index = FirstDbcIndex;
  {%H-}sub_cid_field_index = sub_seqno_field_index+1;
  sub_name_field_index = sub_cid_field_index+1;
var
  Len: NativeUInt;
  MainResultSet, ResultSet: IZResultSet;
  Temp_scheme: string;
  {$IFDEF UNICODE}
  Raw_Schema, Raw_Table: RawByteString;
  {$ENDIF}
begin
  Result:=inherited UncachedGetIndexInfo(Catalog, Schema, Table, Unique, Approximate);

  if Schema = '' then
    if Catalog <> ''
    then Temp_scheme := Catalog
    else Temp_scheme := 'main'
  else Temp_scheme := Schema;
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  {$IFDEF UNICODE}
  Raw_Schema := ZUnicodeToRaw(Temp_scheme, zCP_UTF8);
  Raw_Table := ZUnicodeToRaw(Table, zCP_UTF8);
  {$ENDIF}
  MainResultSet := GetConnection.CreateStatement.ExecuteQuery(
    'PRAGMA '+{$IFDEF UNICODE}Raw_Schema{$ELSE}Temp_scheme{$ENDIF}+'.index_list('''+{$IFDEF UNICODE}Raw_Table{$ELSE}Table{$ENDIF}+''')');
  if MainResultSet<>nil then
  begin
    while MainResultSet.Next do
    begin
      if (ZFastCode.Pos({$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF}(' autoindex '), MainResultSet.GetRawByteString(main_name_field_index)) = 0)
        and ((Unique = False) or (MainResultSet.GetInt(main_unique_field_index) = 0)) then
      begin
        ResultSet := GetConnection.CreateStatement.ExecuteQuery(
          'PRAGMA '+{$IFDEF UNICODE}Raw_Schema{$ELSE}Temp_scheme{$ENDIF}+'.index_info('''+MainResultSet.GetRawByteString(main_name_field_index)+''')');
        while ResultSet.Next do
        begin
          Result.MoveToInsertRow;
          Result.UpdateRawByteString(CatalogNameIndex, {$IFDEF UNICODE}Raw_Schema{$ELSE}Temp_scheme{$ENDIF});
          Result.UpdateRawByteString(TableNameIndex, {$IFDEF UNICODE}Raw_Table{$ELSE}Table{$ENDIF});
          Result.UpdateBoolean(IndexInfoColNonUniqueIndex, MainResultSet.GetInt(main_unique_field_index) = 0);
          Result.UpdatePAnsiChar(IndexInfoColIndexNameIndex, MainResultSet.GetPAnsiChar(main_name_field_index, Len), @Len);
          Result.UpdateInt(IndexInfoColOrdPositionIndex, ResultSet.GetInt(sub_seqno_field_index)+FirstDbcIndex);
          Result.UpdatePAnsiChar(IndexInfoColColumnNameIndex, ResultSet.GetPAnsiChar(sub_name_field_index, Len), @Len);
          Result.UpdateRawByteString(IndexInfoColAscOrDescIndex, 'A');
          Result.UpdateInt(IndexInfoColCardinalityIndex, 0);
          Result.UpdateInt(IndexInfoColPagesIndex, 0);
          Result.InsertRow;
        end;
        ResultSet.Close;
      end;
    end;
    MainResultSet.Close;
  end;
end;

function TZSQLiteDatabaseMetadata.UncachedGetCatalogs: IZResultSet;
var RS: IZResultSet;
    Len: NativeUInt;
begin
  Result := inherited UncachedGetCatalogs;
  RS := GetConnection.CreateStatement.ExecuteQuery(
    RawByteString('PRAGMA database_list'));
  {$IFDEF WITH_VAR_INIT_WARNING}Len := 0;{$ENDIF}
  with RS do begin
    while Next do begin
      Result.MoveToInsertRow;
      Result.UpdatePAnsiChar(CatalogNameIndex, GetPAnsiChar(FirstDbcIndex+1, Len), @Len);
      Result.InsertRow;
    end;
    Close;
  end;
end;

{**
  Gets the supported CharacterSets:
  @return <code>ResultSet</code> - each row is a CharacterSetName and it's ID
}
function TZSQLiteDatabaseMetadata.UncachedGetCharacterSets: IZResultSet; //EgonHugeist
begin
  Result:=inherited UncachedGetCharacterSets;
  Result.MoveToInsertRow;
  Result.UpdateRawByteString(CharacterSetsNameIndex, 'UTF-8'); //CHARACTER_SET_NAME
  Result.UpdateSmall(CharacterSetsIDIndex, 1); //CHARACTER_SET_ID
  Result.InsertRow;
end;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
end.

