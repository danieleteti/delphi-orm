{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
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

unit ZDbcPostgreSql;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(DELPHI) and defined(MSWINDOWS)}Windows,{$IFEND}
  ZDbcIntfs, ZDbcConnection, ZPlainPostgreSqlDriver, ZDbcLogging, ZTokenizer,
  ZGenericSqlAnalyser, ZURL, ZCompatibility, ZSysUtils;

type

  {** Implements PostgreSQL Database Driver. }
  TZPostgreSQLDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

type
  PZPGTableInfo = ^TZPGTableInfo;
  TZPGTableInfo = record
    OID: Oid;
    Name: String;
    Schema: String;
    ColNames: Array of String;
    ColCount: Integer;
  end;

  { TZPGTableInfoCache }

  TZPGTableInfoCache = class(TObject)
    protected
      FTblInfo: Array of TZPGTableInfo;
      FConSettings: PZconSettings;
      FPlainDriver: Pointer;
      FHandle: PZPostgreSQLConnect;
      function LoadTblInfo(const TblOid: Oid; out Index: Integer; ZPGTableInfo: PZPGTableInfo): Boolean;
      function GetTblPos(const TblOid: Oid): Integer;
    public
      constructor Create(const ConSettings: PZConSettings;
        const Handle: PZPostgreSQLConnect; const PlainDriver: IZPostgreSQLPlainDriver);
      function GetTableInfo(const TblOid: Oid): PZPGTableInfo;
      procedure Clear;
  end;

  {** Defines a PostgreSQL specific connection. }
  IZPostgreSQLConnection = interface(IZConnection)
    ['{8E62EA93-5A49-4F20-928A-0EA44ABCE5DB}']

    function IsOidAsBlob: Boolean;
    function Is_bytea_output_hex: Boolean;

    function GetTypeNameByOid(Id: Oid): string;
    function GetPlainDriver: IZPostgreSQLPlainDriver;
    function GetConnectionHandle: PZPostgreSQLConnect;
    function GetServerMajorVersion: Integer;
    function GetServerMinorVersion: Integer;
    function EncodeBinary(Buf: Pointer; Len: Integer; Quoted: Boolean): RawByteString; overload;
    function EncodeBinary(const Value: TBytes; Quoted: Boolean): RawByteString; overload;
    function EscapeString(const FromChar: PAnsiChar; len: NativeUInt; Quoted: Boolean): RawByteString; overload;
    procedure RegisterPreparedStmtName(const value: String);
    procedure UnregisterPreparedStmtName(const value: String);
    function ClientSettingsChanged: Boolean;
    function GetUndefinedVarcharAsStringLength: Integer;
    function GetTableInfo(const TblOid: Oid): PZPGTableInfo;
    function CheckFieldVisibility: Boolean;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;
    procedure AddDomain2BaseTypeIfNotExists(DomainOID, BaseTypeOID: OID);
    function FindDomainBaseType(DomainOID: OID; out BaseTypeOID: OID): Boolean;
  end;

  PZPGDomain2BaseTypeMap = ^TZPGDomain2BaseTypeMap;
  TZPGDomain2BaseTypeMap = record
    DomainOID: OID; //the domain oid
    BaseTypeOID: OID; //the native underlaing oid
    Known: WordBool;
  end;

  TZOID2OIDMapList = class(TZSortedList)
  private
    fUnkownCount: Integer;
    function SortCompare(Item1, Item2: Pointer): Integer;
  public
    procedure AddIfNotExists(DomainOID, BaseTypeOID: OID);
    function GetOrAddBaseTypeOID(DomainOID: OID; out BaseTypeOID: OID): Boolean;
    procedure Clear; override;
  public
    property UnkownCount: Integer read fUnkownCount;
  end;

  {** Implements PostgreSQL Database Connection. }

  { TZPostgreSQLConnection }

  TZPostgreSQLConnection = class(TZAbstractConnection, IZPostgreSQLConnection)
  private
    FStandardConformingStrings: Boolean;
    FHandle: PZPostgreSQLConnect;
//  Jan: Not sure wether we still need that. What was its intended use?
//    FBeginRequired: Boolean;
    FTypeList: TStrings;
    FDomain2BaseTypMap: TZOID2OIDMapList;
    FOidAsBlob: Boolean;
    FServerMajorVersion: Integer;
    FServerMinorVersion: Integer;
    FServerSubVersion: Integer;
    FNoticeProcessor: TZPostgreSQLNoticeProcessor;
    FPreparedStmts: TStrings;
    //a collection of statement handles that are not used anymore. These can be
    //safely deallocated upon the next transaction start or immediately if we
    //are in autocommit mode. See SF#137:
    FPreparedStatementTrashBin: TStrings;
    FProcedureTypesCache: TStrings;
    FClientSettingsChanged: Boolean;
    FTableInfoCache: TZPGTableInfoCache;
    FIs_bytea_output_hex: Boolean;
    FCheckFieldVisibility: Boolean;
    FNoTableInfoCache: Boolean;
    fPlainDriver: IZPostgreSQLPlainDriver;
  protected
    procedure InternalCreate; override;
    function GetUndefinedVarcharAsStringLength: Integer;
    function GetTableInfo(const TblOid: Oid): PZPGTableInfo;
    function BuildConnectStr: RawByteString;
    procedure DeallocatePreparedStatements;
    procedure DoStartTransaction;
    procedure DoCommit;
    procedure DoRollback;
    procedure LoadServerVersion;
    procedure OnPropertiesChange(Sender: TObject); override;
    procedure SetStandardConformingStrings(const Value: Boolean);
    function EncodeBinary(const Value: RawByteString; Quoted: Boolean): RawByteString; overload;
    function EncodeBinary(const Value: TBytes; Quoted: Boolean): RawByteString; overload;
    function EncodeBinary(Buf: Pointer; Len: Integer; Quoted: Boolean): RawByteString; overload;
    function EscapeString(const FromChar: PAnsiChar; len: NativeUInt; Quoted: Boolean): RawByteString; overload;
    procedure RegisterPreparedStmtName(const value: String);
    procedure UnregisterPreparedStmtName(const value: String);
    function ClientSettingsChanged: Boolean;
  public
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;
    function CreateSequence(const Sequence: string; BlockSize: Integer):
      IZSequence; override;

    procedure SetAutoCommit(Value: Boolean); override;

    procedure Commit; override;
    procedure Rollback; override;
    //2Phase Commit Support initially for PostgresSQL (firmos) 21022006
    procedure PrepareTransaction(const transactionid: string);override;
    procedure CommitPrepared(const transactionid:string);override;
    procedure RollbackPrepared(const transactionid:string);override;

    procedure Open; override;
    procedure InternalClose; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    function IsOidAsBlob: Boolean;
    function Is_bytea_output_hex: Boolean;
    function CheckFieldVisibility: Boolean;

    procedure AddDomain2BaseTypeIfNotExists(DomainOID, BaseTypeOID: OID);
    function FindDomainBaseType(DomainOID: OID; out BaseTypeOID: OID): Boolean;
    function GetTypeNameByOid(Id: Oid): string;
    function GetPlainDriver: IZPostgreSQLPlainDriver;
    function GetConnectionHandle: PZPostgreSQLConnect;

    function GetHostVersion: Integer; override;
    function GetServerMajorVersion: Integer;
    function GetServerMinorVersion: Integer;
    function GetServerSubVersion: Integer;
    function StoredProcedureIsSelectable(const ProcName: String): Boolean;

    function PingServer: Integer; override;

    procedure SetReadOnly(Value: Boolean); override;

    function EscapeString(const Value: RawByteString): RawByteString; overload; override;
    function GetBinaryEscapeString(const Value: RawByteString): String; overload; override;
    function GetBinaryEscapeString(const Value: TBytes): String; overload; override;
    function GetEscapeString(const Value: ZWideString): ZWideString; overload; override;
    function GetEscapeString(const Value: RawByteString): RawByteString; overload; override;
    function GetServerSetting(const AName: RawByteString): string;
    procedure SetServerSetting(const AName, AValue: RawbyteString);
    {$IFDEF ZEOS_TEST_ONLY}
    constructor Create(const ZUrl: TZURL);
    {$ENDIF}
    function GetServerProvider: TZServerProvider; override;	
  end;

var
  {** The common driver manager object. }
  PostgreSQLDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses
  ZFastCode, ZMessages, ZDbcPostgreSqlStatement,
  ZDbcPostgreSqlUtils, ZDbcPostgreSqlMetadata, ZPostgreSqlToken,
  ZPostgreSqlAnalyser, ZEncoding, ZDbcUtils, ZDbcMetadata;

const
  FON = String('ON');
  standard_conforming_strings = 'standard_conforming_strings';
  cBegin: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = 'BEGIN';
  cCommit: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = 'COMMIT';
  cRollback: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF} = 'ROLLBACK';

procedure DefaultNoticeProcessor({%H-}arg: Pointer; message: PAnsiChar); cdecl;
begin
  DriverManager.LogMessage(lcOther,'Postgres NOTICE', message);
end;

{ TZPGTableInfoCache }
function TZPGTableInfoCache.LoadTblInfo(const TblOid: Oid; 
  out Index: Integer; ZPGTableInfo: PZPGTableInfo): Boolean;
var
  SQL: RawByteString;
  TblInfo: PZPGTableInfo;
  RawOid: RawByteString;
  QueryHandle: PZPostgreSQLResult;
  I: Integer;
  function GetInt(const Row, Col: Integer): Integer;
  begin
    Result := RawToInt(IZPostgreSQLPlainDriver(FPlainDriver).GetValue(QueryHandle, Row, Col));
  end;

  function GetString(const Row, Col: Integer): String;
  begin
    {$IFDEF UNICODE}
    Result := PRawToUnicode(IZPostgreSQLPlainDriver(FPlainDriver).GetValue(QueryHandle, Row, Col),
      IZPostgreSQLPlainDriver(FPlainDriver).GetLength(QueryHandle, Row, Col), FConSettings^.ClientCodePage^.CP);
    {$ELSE}
    SetString(Result, IZPostgreSQLPlainDriver(FPlainDriver).GetValue(QueryHandle, Row, Col),
      IZPostgreSQLPlainDriver(FPlainDriver).GetLength(QueryHandle, Row, Col));
    {$ENDIF}
  end;
begin
  RawOID := IntToRaw(TblOid);

  SQL := 'select pc.relname, pns.nspname, pa.attnum, pa.attname from ' +
    'pg_catalog.pg_class pc ' +
    'join pg_catalog.pg_namespace pns on pc.relnamespace = pns.oid ' +
    'join pg_catalog.pg_attribute pa on pa.attrelid = pc.oid ' +
    'where pc.oid = ' + RawOID + ' and pa.attnum > 0';

  QueryHandle := IZPostgreSQLPlainDriver(FPlainDriver).ExecuteQuery(FHandle, Pointer(SQL));
  CheckPostgreSQLError(nil, IZPostgreSQLPlainDriver(FPlainDriver), FHandle, lcExecute, SQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, FConSettings^.Protocol, SQL);

  Result := IZPostgreSQLPlainDriver(FPlainDriver).GetRowCount(QueryHandle) > 0;
  if Result then
  begin
    if ZPGTableInfo <> nil then //just overwrite all values
      tblInfo := ZPGTableInfo
    else
    begin //we need a new cache
      SetLength(FTblInfo, Length(FTblInfo) +1);
      Index := High(FTblInfo);
      TblInfo := @FTblInfo[Index];
    end;
    TblInfo^.OID := TblOid;
    TblInfo^.Name := GetString(0, 0);
    TblInfo^.Schema := GetString(0, 1);
    TblInfo^.ColCount := IZPostgreSQLPlainDriver(FPlainDriver).GetRowCount(QueryHandle);
    SetLength(TblInfo^.ColNames, TblInfo^.ColCount);

    for I := 0 to TblInfo^.ColCount - 1 do
      TblInfo^.ColNames[GetInt(I, 2)-1] := GetString(i, 3);
    IZPostgreSQLPlainDriver(FPlainDriver).PQclear(QueryHandle);
  end
  else
    Index := -1;
end;

function TZPGTableInfoCache.GetTblPos(const TblOid: Oid): Integer;
var
  x: Integer;
begin
  Result := -1;
  if TblOid <> InvalidOid then
    for x := 0 to Length(FTblInfo) - 1 do
      if FTblInfo[x].OID = TblOid then
      begin
        Result := x;
        Break;
      end;
end;

constructor TZPGTableInfoCache.Create(const ConSettings: PZConSettings;
  const Handle: PZPostgreSQLConnect; const PlainDriver: IZPostgreSQLPlainDriver);
begin
  FConSettings := ConSettings;
  FPlainDriver := Pointer(PlainDriver);
  FHandle := Handle;

  Clear;
end;

function TZPGTableInfoCache.GetTableInfo(const TblOid: Oid): PZPGTableInfo;
var Idx: Integer;
begin
  Idx := GetTblPos(TblOid);
  if (Idx = -1) then
    if (TblOid <> InvalidOid) and (LoadTblInfo(TblOid, Idx, nil)) then
      Result := @FTblInfo[Idx]
    else
      Result := nil
  else
  begin
    Result := @FTblInfo[Idx];
  end;
end;

procedure TZPGTableInfoCache.Clear;
begin
  SetLength(FTblInfo, 0);
end;

{ TZPostgreSQLDriver }

{**
  Constructs this object with default properties.
}
constructor TZPostgreSQLDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZPostgreSQL9PlainDriver.Create, 'postgresql'));
  AddSupportedProtocol(AddPlainDriverToCache(TZPostgreSQL7PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZPostgreSQL8PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZPostgreSQL9PlainDriver.Create));
end;

{**
  Attempts to make a database connection to the given URL.
  The driver should return "null" if it realizes it is the wrong kind
  of driver to connect to the given URL.  This will be common, as when
  the JDBC driver manager is asked to connect to a given URL it passes
  the URL to each loaded driver in turn.

  <P>The driver should raise a SQLException if it is the right
  driver to connect to the given URL, but has trouble connecting to
  the database.

  <P>The java.util.Properties argument can be used to passed arbitrary
  string tag/value pairs as connection arguments.
  Normally at least "user" and "password" properties should be
  included in the Properties.

  @param url the URL of the database to which to connect
  @param info a list of arbitrary string tag/value pairs as
    connection arguments. Normally at least a "user" and
    "password" property should be included.
  @return a <code>Connection</code> object that represents a
    connection to the URL
}
function TZPostgreSQLDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZPostgreSQLConnection.Create(Url);
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZPostgreSQLDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZPostgreSQLDriver.GetMinorVersion: Integer;
begin
  Result := 3;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZPostgreSQLDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZPostgreSQLTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZPostgreSQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZPostgreSQLStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZPostgreSQLConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZPostgreSQLConnection.InternalCreate;
begin
  FProcedureTypesCache := TStringList.Create;
  FMetaData := TZPostgreSQLDatabaseMetadata.Create(Self, Url);
  FPreparedStmts := nil;
  FPreparedStatementTrashBin := nil;
  FTableInfoCache := nil;

  FDomain2BaseTypMap := TZOID2OIDMapList.Create;
  { Sets a default PostgreSQL port }
  if Self.Port = 0 then
     Self.Port := 5432;

  { Define connect options. }
//  Jan: Not sure wether we still need that. What was its intended use?
//  if Info.Values['beginreq'] <> '' then
//    FBeginRequired := StrToBoolEx(Info.Values['beginreq'])
//  else
//    FBeginRequired := True;

  inherited SetTransactionIsolation(tiReadCommitted);

  { Processes connection properties. }
  if Info.Values['oidasblob'] <> '' then
    FOidAsBlob := StrToBoolEx(Info.Values['oidasblob'])
  else
    FOidAsBlob := False;
  FUndefinedVarcharAsStringLength := StrToIntDef(Info.Values['Undefined_Varchar_AsString_Length'], 0);
  FCheckFieldVisibility := StrToBoolEx(Info.Values['CheckFieldVisibility']);
  FNoTableInfoCache := StrToBoolEx(Info.Values['NoTableInfoCache']);
  OnPropertiesChange(nil);

  FNoticeProcessor := DefaultNoticeProcessor;
end;


function TZPostgreSQLConnection.GetUndefinedVarcharAsStringLength: Integer;
begin
  Result := FUndefinedVarcharAsStringLength;
end;

function TZPostgreSQLConnection.GetTableInfo(const TblOid: Oid): PZPGTableInfo;
begin
  if FNoTableInfoCache then
    Result := nil
  else
    Result := FTableInfoCache.GetTableInfo(TblOid);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZPostgreSQLConnection.Destroy;
begin
  if FTypeList <> nil then FreeAndNil(FTypeList);
  inherited Destroy;
  if FTableInfoCache <> nil then FreeAndNil(FTableInfoCache);
  if FPreparedStmts <> nil then FreeAndNil(FPreparedStmts);
  if Assigned(FPreparedStatementTrashBin) then FreeAndNil(FPreparedStatementTrashBin);
  FreeAndNil(FProcedureTypesCache);
  FreeAndNil(FDomain2BaseTypMap);
end;

{**
procedure TZPostgreSQLConnection.AddDomain2BaseTypeIfNotExists(DomainOID,
  BaseTypeOID: OID);
begin
  FDomain2BaseTypMap.AddIfNotExists(DomainOID, BaseTypeOID);
end;

{**
  Builds a connection string for PostgreSQL.
  @return a built connection string.
}
procedure TZPostgreSQLConnection.AddDomain2BaseTypeIfNotExists(DomainOID,
  BaseTypeOID: OID);
begin
  FDomain2BaseTypMap.AddIfNotExists(DomainOID, BaseTypeOID);
end;

function TZPostgreSQLConnection.BuildConnectStr: RawByteString;
var
  ConnectTimeout, Cnt: Integer;
  Buf: TRawBuff;
  //parameters should be separated by whitespace
  procedure AddParamToResult(const AParam: RawByteString;
    const AValue: String);
  begin
    if Cnt > 0 then
      ToBuff(AnsiChar(' '),Buf, Result);
    ToBuff(AParam, Buf, Result);
    ToBuff(AnsiChar('='),Buf, Result);
    // backslashes and single quotes must be escaped with backslashes
    ToBuff(SQLQuotedStr(EncodeCString({$IFDEF UNICODE}RawByteString{$ENDIF}(AValue)), AnsiChar(#39)),Buf, Result);
    Inc(Cnt);
  end;
begin
  //Init the result to empty string.
  Result := '';
  Cnt := 0;
  Buf.Pos := 0;
  //Entering parameters from the ZConnection
  If IsIpAddr(HostName) then
    AddParamToResult('hostaddr', HostName)
  else
    AddParamToResult('host', HostName);

  AddParamToResult('port', ZFastCode.IntToStr(Port));
  AddParamToResult('dbname', Database);
  if user <> '' then begin
    AddParamToResult('user', User);
    AddParamToResult('password', Password);
  end;

  If Info.Values['sslmode'] <> '' then
  begin
    // the client (>= 7.3) sets the ssl mode for this connection
    // (possible values are: require, prefer, allow, disable)
    AddParamToResult('sslmode', Info.Values['sslmode']);
  end
  else if Info.Values['requiressl'] <> '' then
  begin
    // the client (< 7.3) sets the ssl encription for this connection
    // (possible values are: 0,1)
    AddParamToResult('requiressl', Info.Values['requiressl']);
  end;

  if Info.Values['sslcompression'] <> '' then AddParamToResult('sslcompression', Info.Values['sslcompression']);
  if Info.Values['sslcert'] <> '' then AddParamToResult('sslcert', Info.Values['sslcert']);
  if Info.Values['sslkey'] <> '' then AddParamToResult('sslkey', Info.Values['sslkey']);
  if Info.Values['sslrootcert'] <> '' then AddParamToResult('sslrootcert', Info.Values['sslrootcert']);
  if Info.Values['sslcrl'] <> '' then AddParamToResult('sslcrl', Info.Values['sslcrl']);
  { tcp keepalives by Luca Olivetti }
  if Info.Values['keepalives'] <> '' then AddParamToResult('keepalives',Info.Values['keepalives']);
  if Info.Values['keepalives_idle'] <> '' then AddParamToResult('keepalives_idle',Info.Values['keepalives_idle']);
  if Info.Values['keepalives_interval'] <> '' then AddParamToResult('keepalives_interval',Info.Values['keepalives_interval']);
  if Info.Values['keepalives_count'] <> '' then AddParamToResult('keepalives_count',Info.Values['keepalives_count']);

  { Sets a connection timeout. }
  ConnectTimeout := StrToIntDef(Info.Values['timeout'], -1);
  if ConnectTimeout >= 0 then
    AddParamToResult('connect_timeout', ZFastCode.IntToStr(ConnectTimeout));

  { Sets the application name }
  if Info.Values['application_name'] <> '' then
    AddParamToResult('application_name', Info.Values['application_name']);
  FlushBuff(Buf, Result);
end;

{**
  Checks is oid should be treated as Large Object.
  @return <code>True</code> if oid should represent a Large Object.
}
function TZPostgreSQLConnection.IsOidAsBlob: Boolean;
begin
  Result := FOidAsBlob;
end;

{**
  Checks is bytea_output hex.
  @return <code>True</code> if hex is set.
}
function TZPostgreSQLConnection.Is_bytea_output_hex: Boolean;
begin
  Result := FIs_bytea_output_hex;
end;

{**
  Checks if DataBaseMetaData should check FieldVisibility too.
  @return <code>True</code> if user did set it.
}
function TZPostgreSQLConnection.CheckFieldVisibility: Boolean;
begin
  Result := FCheckFieldVisibility;
end;

{**
  Deallocates prepared statements. This procedure is intended for driver internal
  use only and should normally only be called when in auto-commit mode. This
  either happens when unregistering a prepared statement and being in auto-commit
  mode or when committing or rolling back a transaction and before staring the
  next transaction block.
  @return <code>True</code> if user did set it.
}
procedure TZPostgreSQLConnection.DeallocatePreparedStatements;
var
  QueryHandle: PZPostgreSQLResult;
  SQL: RawByteString;
  x: Integer;
begin
  if Assigned(FPreparedStatementTrashBin) and (FHandle <> nil) and (FPreparedStatementTrashBin.Count > 0) then
    try
      for x := FPreparedStatementTrashBin.Count - 1 downto 0 do begin
        SQL := 'DEALLOCATE "' + {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(FPreparedStatementTrashBin.Strings[x]) + '";';;
        QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
        CheckPostgreSQLError(nil, GetPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
        GetPlainDriver.PQclear(QueryHandle);
        DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
        FPreparedStatementTrashBin.Delete(x);
      end;
    finally
      FPreparedStatementTrashBin.Clear;
    end;
end;

{**
  Starts an explicit transaction.
}
procedure TZPostgreSQLConnection.DoStartTransaction;
var
  QueryHandle: PZPostgreSQLResult;
begin
//  Jan: Not sure wether we still need that. What was its intended use?
//  if FBeginRequired then begin
  QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(cBegin));
  CheckPostgreSQLError(nil, GetPlainDriver, FHandle, lcExecute, cBegin,QueryHandle);
  GetPlainDriver.PQclear(QueryHandle);
  DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, cBegin);
//  end;
end;

{**
  Commits an explicit transaction.
}
procedure TZPostgreSQLConnection.DoCommit;
var
  QueryHandle: PZPostgreSQLResult;
begin
  if not Closed then begin
    QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(cCommit));
    CheckPostgreSQLError(nil, GetPlainDriver, FHandle, lcExecute, cCommit,QueryHandle);
    GetPlainDriver.PQclear(QueryHandle);
    DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, cCommit);
  end;
end;

{**
  Rolls an explicit transaction back.
}
procedure TZPostgreSQLConnection.DoRollback;
var
  QueryHandle: PZPostgreSQLResult;
  SQL: RawByteString;
begin
  if not Closed then
  begin
    SQL := 'ROLLBACK';
    QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
    CheckPostgreSQLError(nil, GetPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    GetPlainDriver.PQclear(QueryHandle);
    DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, SQL);
  end;
end;

{**
  Encodes a Binary-AnsiString to a PostgreSQL format
  @param Value the Binary String
  @result the encoded String
}
function TZPostgreSQLConnection.EncodeBinary(const Value: TBytes; Quoted: Boolean): RawByteString;
begin
  Result := EncodeBinary(Pointer(Value), Length(Value), Quoted);
end;
{**
  Encodes a Binary-AnsiString to a PostgreSQL format
  @param Value the Binary String
  @result the encoded String
}
function TZPostgreSQLConnection.EncodeBinary(const Value: RawByteString; Quoted: Boolean): RawByteString;
begin
  Result := EncodeBinary(Pointer(Value), Length(Value), Quoted);
end;

procedure TZPostgreSQLConnection.RegisterPreparedStmtName(const value: String);
begin
  FPreparedStmts.Add(Value);
end;

procedure TZPostgreSQLConnection.UnregisterPreparedStmtName(const value: String);
var Index: Integer;
begin
  Index := FPreparedStmts.IndexOf(Value);
  if Index > -1 then begin
    FPreparedStatementTrashBin.Add(FPreparedStmts.Strings[Index]);
    FPreparedStmts.Delete(Index);
  end;
  if GetAutoCommit then DeallocatePreparedStatements;
end;

function TZPostgreSQLConnection.ClientSettingsChanged: Boolean;
begin
  Result := FClientSettingsChanged;
end;
{**
  Opens a connection to database server with specified parameters.
}
procedure TZPostgreSQLConnection.Open;

var
  SCS, Temp: string;
  LogMessage: RawByteString;
begin
  if not Closed then
    Exit;


  { Connect to PostgreSQL database. }
  LogMessage := BuildConnectStr;
  FHandle := GetPlainDriver.ConnectDatabase(Pointer(LogMessage));
  LogMessage := 'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"';
  try
    if GetPlainDriver.GetStatus(FHandle) = CONNECTION_BAD then
    begin
      CheckPostgreSQLError(nil, GetPlainDriver, FHandle,
                            lcConnect, LogMessage,nil)
    end
    else
      DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);

    { Set the notice processor (default = nil)}
    GetPlainDriver.SetNoticeProcessor(FHandle,FNoticeProcessor,nil);

    { Gets the current codepage }
    Temp := GetPlainDriver.ValidateCharEncoding(GetPlainDriver.GetClientEncoding(FHandle)).Name;

    { Sets a client codepage if necessary }
    if ( FClientCodePage <> '' ) and (Temp <> FClientCodePage) then
      SetServerSetting('CLIENT_ENCODING', {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(FClientCodePage));

    inherited Open;

    SetTransactionIsolation(GetTransactionIsolation);
    if not GetAutoCommit then
      DoStartTransaction;
    if ReadOnly then begin
      inherited SetReadOnly(False);
      SetReadOnly(True);
    end;

    if ReadOnly then begin
      inherited SetReadOnly(False);
      SetReadOnly(True);
    end;

    { Gets the current codepage if it wasn't set..}
    if ( FClientCodePage = '') then
      CheckCharEncoding(Temp)
    else
    begin
      CheckCharEncoding(FClientCodePage);
      FClientSettingsChanged := True;
    end;

    if FPreparedStmts = nil then
      FPreparedStmts := TStringList.Create;
    if not Assigned(FPreparedStatementTrashBin) then
      FPreparedStatementTrashBin := TStringList.Create;
    if FTableInfoCache = nil then
      FTableInfoCache := TZPGTableInfoCache.Create(ConSettings, FHandle, GetPlainDriver);

    { sets standard_conforming_strings according to Properties if available }
    SCS := Info.Values[standard_conforming_strings];
    if SCS <> '' then begin
      SetServerSetting(standard_conforming_strings, {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(SCS));
      FClientSettingsChanged := True;
      SetStandardConformingStrings(StrToBoolEx(SCS));
    end else
      SetStandardConformingStrings(StrToBoolEx(GetServerSetting(#39+standard_conforming_strings+#39)));
    FIs_bytea_output_hex := UpperCase(GetServerSetting('''bytea_output''')) = 'HEX';
  finally
    if self.IsClosed and (Self.FHandle <> nil) then
    begin
      GetPlainDriver.Finish(Self.FHandle);
      Self.FHandle := nil;
    end;
  end;
end;

procedure TZPostgreSQLConnection.PrepareTransaction(const transactionid: string);
var
   QueryHandle: PZPostgreSQLResult;
   SQL: RawByteString;
begin
  if not Closed then
  begin
    SQL:='PREPARE TRANSACTION '''+copy(ConSettings^.ConvFuncs.ZStringToRaw(transactionid,
      ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP),1,200)+'''';
    QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, PAnsiChar(SQL));
    CheckPostgreSQLError(nil, GetPlainDriver, FHandle, lcExecute, SQL, QueryHandle);
    GetPlainDriver.PQclear(QueryHandle);
    DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, SQL);
    DoStartTransaction;
  end;
end;

{**
  Creates a <code>Statement</code> object for sending
  SQL statements to the database.
  SQL statements without parameters are normally
  executed using Statement objects. If the same SQL statement
  is executed many times, it is more efficient to use a
  <code>PreparedStatement</code> object.
  <P>
  Result sets created using the returned <code>Statement</code>
  object will by default have forward-only type and read-only concurrency.

  @param Info a statement parameters.
  @return a new Statement object
}
function TZPostgreSQLConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
    Open;
  {$IFDEF ZEOS_TEST_ONLY}
  Case GetTestMode of
    0:
  {$ENDIF}
      if GetServerMajorVersion >= 8 then
        Result := TZPostgreSQLCAPIPreparedStatement.Create(GetPlainDriver, Self, '', Info)
      else
        Result := TZPostgreSQLClassicPreparedStatement.Create(GetPlainDriver, Self, '', Info);
  {$IFDEF ZEOS_TEST_ONLY}
    1: Result := TZPostgreSQLClassicPreparedStatement.Create(GetPlainDriver, Self, '', Info);
  end;
  {$ENDIF}
end;

{**
  Creates a <code>PreparedStatement</code> object for sending
  parameterized SQL statements to the database.

  A SQL statement with or without IN parameters can be
  pre-compiled and stored in a PreparedStatement object. This
  object can then be used to efficiently execute this statement
  multiple times.

  <P><B>Note:</B> This method is optimized for handling
  parametric SQL statements that benefit from precompilation. If
  the driver supports precompilation,
  the method <code>prepareStatement</code> will send
  the statement to the database for precompilation. Some drivers
  may not support precompilation. In this case, the statement may
  not be sent to the database until the <code>PreparedStatement</code> is
  executed.  This has no direct effect on users; however, it does
  affect which method throws certain SQLExceptions.

  Result sets created using the returned PreparedStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?' IN
    parameter placeholders
  @param Info a statement parameters.
  @return a new PreparedStatement object containing the
    pre-compiled statement
}
function TZPostgreSQLConnection.CreatePreparedStatement(
  const SQL: string; Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;

  {$IFDEF ZEOS_TEST_ONLY}
  Case GetTestMode of
    0:
  {$ENDIF}
      if GetServerMajorVersion >= 8 then
        Result := TZPostgreSQLCAPIPreparedStatement.Create(GetPlainDriver, Self, SQL, Info)
      else
        Result := TZPostgreSQLClassicPreparedStatement.Create(GetPlainDriver, Self, SQL, Info);
  {$IFDEF ZEOS_TEST_ONLY}
    1: Result := TZPostgreSQLClassicPreparedStatement.Create(GetPlainDriver, Self, SQL, Info);
  end;
  {$ENDIF}
end;


{**
  Creates a <code>CallableStatement</code> object for calling
  database stored procedures (functions in PostgreSql).
  The <code>CallableStatement</code> object provides
  methods for setting up its IN and OUT parameters, and
  methods for executing the call to a stored procedure.

  <P><B>Note:</B> This method is optimized for handling stored
  procedure call statements. Some drivers may send the call
  statement to the database when the method <code>prepareCall</code>
  is done; others
  may wait until the <code>CallableStatement</code> object
  is executed. This has no
  direct effect on users; however, it does affect which method
  throws certain SQLExceptions.

  Result sets created using the returned CallableStatement will have
  forward-only type and read-only concurrency, by default.

  @param sql a SQL statement that may contain one or more '?'
    parameter placeholders. Typically this  statement is a JDBC
    function call escape string.
  @param Info a statement parameters.
  @return a new CallableStatement object containing the
    pre-compiled SQL statement
}
function TZPostgreSQLConnection.CreateCallableStatement(
  const SQL: string; Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  Result := TZPostgreSQLCallableStatement.Create(Self, SQL, Info);
end;

{**
  Sets this connection's auto-commit mode.
  If a connection is in auto-commit mode, then all its SQL
  statements will be executed and committed as individual
  transactions.  Otherwise, its SQL statements are grouped into
  transactions that are terminated by a call to either
  the method <code>commit</code> or the method <code>rollback</code>.
  By default, new connections are in auto-commit mode.

  The commit occurs when the statement completes or the next
  execute occurs, whichever comes first. In the case of
  statements returning a ResultSet, the statement completes when
  the last row of the ResultSet has been retrieved or the
  ResultSet has been closed. In advanced cases, a single
  statement may return multiple results as well as output
  parameter values. In these cases the commit occurs when all results and
  output parameter values have been retrieved.

  @param autoCommit true enables auto-commit; false disables auto-commit.
}
procedure TZPostgreSQLConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> GetAutoCommit then begin
    if not Closed then
      if Value
      then DoCommit
      else DoStartTransaction;
    inherited SetAutoCommit(Value);
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.Commit;
begin
  if GetAutoCommit then
    raise Exception.Create(SInvalidOpInAutoCommit);

  if not Closed then begin
    DoCommit;
    DeallocatePreparedStatements;
    DoStartTransaction;
  end;
end;

{**
  Commits a prepared transaction in a 2-Phase commit.
  This method should be used only when in auto-commit mode.
  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.CommitPrepared(const transactionid: string);
var
  QueryHandle: PZPostgreSQLResult;
  SQL: RawByteString;
begin
  if GetAutoCommit
  then raise Exception.Create('Commiting a prepared transaction is not supported while an explicit transaction is running.');

  if not Closed then begin
    SQL := 'COMMIT PREPARED '''+copy(RawByteString(transactionid),1,200)+'''';
    QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
    CheckPostgreSQLError(nil, GetPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    GetPlainDriver.PQclear(QueryHandle);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.

  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.Rollback;
begin
  if GetAutoCommit then
    raise Exception.Create(SInvalidOpInAutoCommit);

  if not Closed then begin
    DoRollback;
    DeallocatePreparedStatements;
    DoStartTransaction;
  end;
end;


{**
  Rolls back a transaction that was prepared for 2-Phase commit.
  This method can only be used when auto-commit is enabled.
  @see #setAutoCommit
}
procedure TZPostgreSQLConnection.RollbackPrepared(const transactionid: string);
var
   QueryHandle: PZPostgreSQLResult;
   SQL: RawByteString;
begin
  if not GetAutoCommit
  then raise Exception.Create('Rolling back a prepared transaction is not supported while an explicit transaction is running.');

  if not Closed then
  begin
    SQL := 'ROLLBACK PREPARED '''+copy(RawByteString(transactionid),1,200)+'''';
    QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
    CheckPostgreSQLError(nil, GetPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
    GetPlainDriver.PQclear(QueryHandle);
    DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, SQL);
  end;
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZPostgreSQLConnection.InternalClose;
var
  LogMessage: RawbyteString;
  QueryHandle: PPGresult;
  PError: PAnsiChar;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;
    QueryHandle := FPlainDriver.ExecuteQuery(FHandle, Pointer(cCommit));
    PError := FPlainDriver.GetErrorMessage(FHandle);
    if (PError = nil) or (PError^ = #0) then begin
      FPlainDriver.PQclear(QueryHandle);
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, cCommit);
    end else begin
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, cCommit);
      PError := FPlainDriver.GetResultErrorField(QueryHandle,PG_DIAG_SQLSTATE);
      //transaction aborted and in postre zombi status? If so a rollback is required
      if (PError = nil) or (ZSysUtils.ZMemLComp(PError, current_transaction_is_aborted, 5) = 0) then begin
        FPlainDriver.PQclear(QueryHandle);
        QueryHandle := FPlainDriver.ExecuteQuery(FHandle, Pointer(cRollback));
      end;
      if QueryHandle <> nil then
        FPlainDriver.PQclear(QueryHandle); //raise no exception
    end;
  try
    DeallocatePreparedStatements;
  finally
    if FHandle <> nil then
      FPlainDriver.Finish(FHandle);
    FHandle := nil;
    LogMessage := 'DISCONNECT FROM "'+ConSettings^.Database+'"';
    DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol, LogMessage);
  end;
end;

const cROTxn: array[Boolean] of RawByteString = (' READ WRITE', ' READ ONLY');
{**
  Sets a new transact isolation level. tiNone, tiReadUncommitted
  will be mapped to tiReadCommitted since PostgreSQL will treat
  them the same anyway.
  For Versions earlier than 8.0 tiRepeatableRead will be mapped
  to tiSerializable since versions priot to 8.0 don't support it.
  @param Level a new transact isolation level.
}
procedure TZPostgreSQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  QueryHandle: PZPostgreSQLResult;
  SQL: RawByteString;
begin
  if Level <> GetTransactionIsolation then begin
    if not Closed then begin
      SQL := RawByteString('SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL ');
      case level of
        tiNone, tiReadUncommitted, tiReadCommitted:
          SQL := SQL + RawByteString('READ COMMITTED');
        tiRepeatableRead:
          if (GetServerMajorVersion >= 8)
          then SQL := SQL + RawByteString('REPEATABLE READ')
          else SQL := SQL + RawByteString('SERIALIZABLE');
        tiSerializable:
          SQL := SQL + RawByteString('SERIALIZABLE');
      end;
      SQL := SQL + cROTxn[ReadOnly];
      QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
      CheckPostgreSQLError(nil, GetPlainDriver, FHandle, lcExecute, SQL ,QueryHandle);
      GetPlainDriver.PQclear(QueryHandle);
      DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, SQL);
    end;
    inherited SetTransactionIsolation(Level);
  end;
end;

function TZPostgreSQLConnection.StoredProcedureIsSelectable(
  const ProcName: String): Boolean;
var I: Integer;
  function AddToCache(const ProcName: String): Boolean;
  var RS: IZResultSet;
    //Stmt: IZStatement;
    Catalog, Schema, ObjName: String;
  begin
    Result := True;
    if GetServerMajorVersion < 11 then
      Exit;
    SplitQualifiedObjectName(ProcName, True, True, Catalog, Schema, ObjName);
    if UseMetadata then with GetMetadata do begin
      RS := GetProcedures(Catalog, AddEscapeCharToWildcards(Schema), AddEscapeCharToWildcards(ObjName));
      if RS.Next then
        Result := RS.GetInt(ProcedureTypeIndex) = ProcedureReturnsResult;
    end;
    FProcedureTypesCache.AddObject(ProcName, TObject(Ord(Result)));
  end;
begin
  I := FProcedureTypesCache.IndexOf(ProcName);
  if I = -1
  then Result := AddToCache(ProcName)
  else Result := FProcedureTypesCache.Objects[I] <> nil;
end;

{**
  Gets a reference to PostgreSQL connection handle.
  @return a reference to PostgreSQL connection handle.
}
function TZPostgreSQLConnection.GetConnectionHandle: PZPostgreSQLConnect;
begin
  Result := FHandle;
end;

{**
  Gets a PostgreSQL plain driver interface.
  @return a PostgreSQL plain driver interface.
}
function TZPostgreSQLConnection.GetPlainDriver: IZPostgreSQLPlainDriver;
begin
  if FPlainDriver = nil then
    FPlainDriver := PlainDriver as IZPostgreSQLPlainDriver;
  Result := FPlainDriver;
end;

{**
  Gets a type name by it's oid number.
  @param Id a type oid number.
  @return a type name or empty string if there was no such type found.
}
function TZPostgreSQLConnection.GetTypeNameByOid(Id: Oid): string;
var
  I, Index: Integer;
  QueryHandle: PZPostgreSQLResult;
  SQL: RawByteString;
  TypeCode, BaseTypeCode: Integer;
  TypeName: string;
  LastVersion, IsEnum: boolean;
begin
  if Closed then
     Open;

  if (GetServerMajorVersion < 7 ) or
    ((GetServerMajorVersion = 7) and (GetServerMinorVersion < 3)) then
    LastVersion := True
  else
    LastVersion := False;

  { Fill the list with existed types }
  if not Assigned(FTypeList) then
  begin
    if LastVersion then
      SQL := 'SELECT oid, typname FROM pg_type WHERE oid<10000'
    else
      SQL := 'SELECT oid, typname, typbasetype,typtype FROM pg_type' +
             ' WHERE (typtype = ''b'' and oid < 10000) OR typtype = ''p'' OR typtype = ''e'' OR typbasetype<>0 ORDER BY oid';

    QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
    CheckPostgreSQLError(Self, GetPlainDriver, FHandle, lcExecute, SQL, QueryHandle);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);

    FTypeList := TStringList.Create;
    for I := 0 to GetPlainDriver.GetRowCount(QueryHandle)-1 do
    begin
      TypeCode := RawToIntDef(
        GetPlainDriver.GetValue(QueryHandle, I, 0), 0);
      isEnum := LowerCase(String(GetPlainDriver.GetValue(QueryHandle, I, 3))) = 'e';
      if isEnum then
        TypeName := 'enum'
      else
        TypeName := String(GetPlainDriver.GetValue(QueryHandle, I, 1));

      if LastVersion then
        BaseTypeCode := 0
      else
        BaseTypeCode := RawToIntDef(
          GetPlainDriver.GetValue(QueryHandle, I, 2), 0);

      if BaseTypeCode <> 0 then
      begin
        Index := FTypeList.IndexOfObject(TObject(BaseTypeCode));
        if Index >= 0 then
          TypeName := FTypeList[Index]
        else
          TypeName := '';
      end;
      FTypeList.AddObject(TypeName, TObject(TypeCode));
    end;
    GetPlainDriver.PQclear(QueryHandle);
  end;

  I := FTypeList.IndexOfObject(TObject(Id));
  if I >= 0 then
    Result := FTypeList[I]
  else
    Result := '';
end;

{**
  Gets the host's full version number. Initially this should be 0.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this server's full version number
}
function TZPostgreSQLConnection.GetHostVersion: Integer;
begin
 Result := GetServerMajorVersion*1000000+GetServerMinorversion*1000+GetServerSubversion;
end;

{**
  Gets a server major version.
  @return a server major version number.
}
function TZPostgreSQLConnection.GetServerMajorVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerMajorVersion;
end;

{**
  Gets a server minor version.
  @return a server minor version number.
}
function TZPostgreSQLConnection.GetServerMinorVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerMinorVersion;
end;

function TZPostgreSQLConnection.GetServerProvider: TZServerProvider;
begin
  Result := spPostgreSQL;
end;

{**
  Gets a server sub version.
  @return a server sub version number.
}
function TZPostgreSQLConnection.GetServerSubVersion: Integer;
begin
  if (FServerMajorVersion = 0) and (FServerMinorVersion = 0) then
    LoadServerVersion;
  Result := FServerSubVersion;
end;

{**
  Loads a server major and minor version numbers.
}
procedure TZPostgreSQLConnection.LoadServerVersion;
var
  Temp: string;
  List: TStrings;
  QueryHandle: PZPostgreSQLResult;
  SQL: RawByteString;
begin
  if Closed then
    Open;
  SQL := 'SELECT version()';
  QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
  CheckPostgreSQLError(Self, GetPlainDriver, FHandle, lcExecute, SQL,QueryHandle);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);

  Temp := String(GetPlainDriver.GetValue(QueryHandle, 0, 0));
  GetPlainDriver.PQclear(QueryHandle);

  List := TStringList.Create;
  try
    { Splits string by space }
    PutSplitString(List, Temp, ' ');
    { first - PostgreSQL, second X.Y.Z}
    Temp := List.Strings[1];
    { Splits string by dot }
    PutSplitString(List, Temp, '.');

    FServerMajorVersion := StrToIntDef(List.Strings[0], 0);
    if List.Count > 1 then
      FServerMinorVersion := GetMinorVersion(List.Strings[1])
    else
      FServerMinorVersion := 0;
    if List.Count > 2 then
      FServerSubVersion := GetMinorVersion(List.Strings[2])
    else
      FServerSubVersion := 0;
  finally
    List.Free;
  end;
end;

{** 
Ping Current Connection's server, if client was disconnected, 
the connection is resumed. 
@return 0 if succesfull or error code if any error occurs 
} 
function TZPostgreSQLConnection.PingServer: Integer; 
const 
  PING_ERROR_ZEOSCONNCLOSED = -1;
var
  res: PZPostgreSQLResult;
  isset: boolean;
begin
  Result := PING_ERROR_ZEOSCONNCLOSED;
  if Not Closed and (FHandle <> nil) then begin
    res := GetPlainDriver.ExecuteQuery(FHandle,'');
    isset := assigned(res);
    GetPlainDriver.PQclear(res);
    if isset and (GetPlainDriver.GetStatus(FHandle) = CONNECTION_OK) then
      Result := 0
    else
      try
        GetPlainDriver.Reset(FHandle);
        res := GetPlainDriver.ExecuteQuery(FHandle,'');
        isset := assigned(res);
        GetPlainDriver.PQclear(res);
        if isset and (GetPlainDriver.GetStatus(FHandle) = CONNECTION_OK) then
          Result := 0;
      except
        Result := 1;
      end;
  end;
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations. This procedure does nothing for PosgreSQL
  versions prior to 7.4 because they don't support changing a transaction to
  read only.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZPostgreSQLConnection.SetReadOnly(Value: Boolean);
var
  SQL: RawByteString;
  QueryHandle: PZPostgreSQLResult;
begin
  if not Closed and (GetServerMajorVersion > 7) or ((GetServerMajorVersion = 7) and (GetServerMinorVersion >= 4)) then begin
    if Value <> isReadOnly then begin
      SQL := RawByteString('SET SESSION CHARACTERISTICS AS TRANSACTION ');
      case Value of
        true:
          SQL := SQL + RawByteString('READ ONLY');
        false:
          SQL := SQL + RawByteString('READ WRITE');
      end;

      QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
      CheckPostgreSQLError(nil, GetPlainDriver, FHandle, lcExecute, SQL ,QueryHandle);
      GetPlainDriver.PQclear(QueryHandle);
      DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
    end;
  end;
  inherited SetReadOnly(Value);
end;

function TZPostgreSQLConnection.EscapeString(const Value: RawByteString): RawByteString;
begin
  Result := EscapeString(Pointer(Value), Length(Value), True)
end;

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZPostgreSQLConnection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  Result := TZPostgreSQLSequence.Create(Self, Sequence, BlockSize);
end;

function TZPostgreSQLConnection.FindDomainBaseType(DomainOID: OID;
  out BaseTypeOID: OID): Boolean;
begin
  Result := FDomain2BaseTypMap.GetOrAddBaseTypeOID(DomainOID, BaseTypeOID);
end;

{**
  EgonHugeist:
  Returns the BinaryString in a Tokenizer-detectable kind
  If the Tokenizer don't need to predetect it Result = BinaryString
  @param Value represents the Binary-String
  @param EscapeMarkSequence represents a Tokenizer detectable EscapeSequence (Len >= 3)
  @result the detectable Binary String
}
function TZPostgreSQLConnection.GetBinaryEscapeString(const Value: RawByteString): String;
begin
  Result := String(EncodeBinary(Value, True));
end;

{**
  EgonHugeist:
  Returns the BinaryString in a Tokenizer-detectable kind
  If the Tokenizer don't need to predetect it Result = BinaryString
  @param Value represents the Binary-String
  @param EscapeMarkSequence represents a Tokenizer detectable EscapeSequence (Len >= 3)
  @result the detectable Binary String
}
function TZPostgreSQLConnection.GetBinaryEscapeString(const Value: TBytes): String;
var Tmp: RawByteString;
begin
  ZSetString(PAnsiChar(Value), Length(Value), Tmp{%H-});
  Result := {$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(EncodeBinary(Tmp, True));
end;

{**
  EgonHugeist:
  Returns a String in a Tokenizer-detectable kind
  If the Tokenizer don't need to predetect it Result = BinaryString
  @param Value represents the String
  @param EscapeMarkSequence represents a Tokenizer detectable EscapeSequence (Len >= 3)
  @result the detectable Postrgres-compatible String
}
function TZPostgreSQLConnection.GetEscapeString(const Value: ZWideString): ZWideString;
begin
  Result := ConSettings^.ConvFuncs.ZRawToUnicode(EscapeString(ConSettings.ConvFuncs.ZUnicodeToRaw(Value, ConSettings^.ClientCodePage^.CP)), ConSettings^.ClientCodePage^.CP);
end;

function TZPostgreSQLConnection.GetEscapeString(const Value: RawByteString): RawByteString;
begin
  Result := EscapeString(Value);
end;

{**
  Gets a current setting of run-time parameter.
  @param AName a parameter name.
  @result a parmeter value retrieved from server.
}
function TZPostgreSQLConnection.GetServerSetting(const AName: RawByteString): string;
var
  SQL: RawByteString;
  QueryHandle: PZPostgreSQLResult;
begin
  SQL := 'select setting from pg_settings where name = '+AName;
  QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
  CheckPostgreSQLError(Self, GetPlainDriver, FHandle, lcExecute, SQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);

  Result := String(GetPlainDriver.GetValue(QueryHandle, 0, 0));
  GetPlainDriver.PQclear(QueryHandle);
end;

procedure TZPostgreSQLConnection.OnPropertiesChange(Sender: TObject);
var
  SCS: string;
begin
  inherited OnPropertiesChange(Sender);

  { Define standard_conforming_strings setting}
  SCS := Trim(Info.Values[standard_conforming_strings]);
  if SCS <> '' then
    SetStandardConformingStrings(UpperCase(SCS) = FON)
  else
    SetStandardConformingStrings(GetPlainDriver.GetStandardConformingStrings);
end;

{**
  Sets current setting of run-time parameter.
  String values should be already quoted.
  @param AName a parameter name.
  @param AValue a new parameter value.
}
procedure TZPostgreSQLConnection.SetServerSetting(const AName,
  AValue: RawbyteString);
var
  SQL: RawByteString;
  QueryHandle: PZPostgreSQLResult;
begin
  SQL := 'SET '+AName+' = '+AValue;
  QueryHandle := GetPlainDriver.ExecuteQuery(FHandle, Pointer(SQL));
  CheckPostgreSQLError(Self, GetPlainDriver, FHandle, lcExecute, SQL, QueryHandle);
  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);

  GetPlainDriver.PQclear(QueryHandle);
end;

{$IFDEF ZEOS_TEST_ONLY}
constructor TZPostgreSQLConnection.Create(const ZUrl: TZURL);
begin
 inherited Create(ZUrl);
end;
{$ENDIF}

procedure TZPostgreSQLConnection.SetStandardConformingStrings(const Value: Boolean);
begin
  FStandardConformingStrings := Value;
  ( Self.GetDriver.GetTokenizer as IZPostgreSQLTokenizer ).SetStandardConformingStrings(FStandardConformingStrings);
end;

function TZPostgreSQLConnection.EncodeBinary(Buf: Pointer;
  Len: Integer; Quoted: Boolean): RawByteString;
var
  escapedBuf: PAnsiChar;
  escapedLen: LongWord;
begin
  if (Buf = nil) or (Len = 0) then
    if Quoted then
      Result := '''''' else
      Result := ''
  else if GetPlainDriver.SupportsEncodeBYTEA then begin
    escapedBuf := GetPlainDriver.EscapeBytea(GetConnectionHandle, Buf, Len, @escapedLen);
    escapedLen := escapedLen -1; //return length including #0
    ZSetString(nil, escapedLen+Byte(Ord(Quoted) shl 1), Result);
    if Quoted then begin
      Result[1] := '''';
      Result[Length(Result)] := '''';
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(escapedBuf^, Result[1+Ord(Quoted)], escapedLen);
    GetPlainDriver.FreeMem(escapedBuf);
  end else
    Result := ZDbcPostgreSqlUtils.EncodeBinaryString(Buf, Len, Quoted);
end;

function TZPostgreSQLConnection.EscapeString(const FromChar: PAnsiChar;
  len: NativeUInt; Quoted: Boolean): RawByteString;
var
  Buf: Array[0..2048] of AnsiChar;
  iError: Integer;
  P: PAnsiChar;
begin
  if GetPlainDriver.SupportsStringEscaping(FClientSettingsChanged) then begin
    if (Len+Byte(Ord(Quoted))) shl 1 > (SizeOf(Buf)-1) then begin
      SetLength(Result, (Len+Byte(Ord(Quoted))) shl 1);
      SetLength(Result, GetPlainDriver.EscapeString(FHandle, PAnsiChar(Pointer(Result))+Ord(Quoted), FromChar, Len, @iError)+(Byte(Ord(Quoted)) shl 1));
    end else
      ZSetString(@Buf[0], GetPlainDriver.EscapeString(FHandle, @Buf[0+Ord(Quoted)], FromChar, Len, @iError)+(Byte(Ord(Quoted) shl 1)), Result);
    if iError <> 0 then
      raise Exception.Create('Wrong string escape behavior!');
    if Quoted then begin
      P := Pointer(Result);
      P^ := #39;
      (P+Length(Result)-1)^ := #39;
    end;
  end else
    Result := ZDbcPostgreSqlUtils.PGEscapeString(FromChar, Len, ConSettings, Quoted);
end;

{ TZOID2OIDMapList }

procedure TZOID2OIDMapList.AddIfNotExists(DomainOID, BaseTypeOID: OID);
var FoundOID: OID;
  I: Integer;
begin
  if not GetOrAddBaseTypeOID(DomainOID, FoundOID) then
    for i := 0 to Count-1 do
      if (PZPGDomain2BaseTypeMap(Items[i]).DomainOID = DomainOID) then begin
        PZPGDomain2BaseTypeMap(Items[i]).BaseTypeOID := BaseTypeOID;
        PZPGDomain2BaseTypeMap(Items[i]).Known := True;
        Dec(fUnkownCount);
      end;
end;

procedure TZOID2OIDMapList.Clear;
var i: Integer;
begin
  for i := Count-1 downto 0 do
    FreeMem(Items[i], SizeOf(TZPGDomain2BaseTypeMap));
  inherited;
  fUnkownCount := 0;
end;

function TZOID2OIDMapList.GetOrAddBaseTypeOID(DomainOID: OID; out BaseTypeOID: OID): Boolean;
var i: Integer;
  Val: PZPGDomain2BaseTypeMap;
begin
  Result := False;
  BaseTypeOID := InvalidOID;
  for i := 0 to Count-1 do
    if (PZPGDomain2BaseTypeMap(Items[i]).DomainOID = DomainOID) and PZPGDomain2BaseTypeMap(Items[i]).Known then begin
      Result := PZPGDomain2BaseTypeMap(Items[i]).Known;
      BaseTypeOID := PZPGDomain2BaseTypeMap(Items[i]).BaseTypeOID;
      Exit;
    end;
  GetMem(Val, SizeOf(TZPGDomain2BaseTypeMap));
  Val.DomainOID := DomainOID;
  Val.BaseTypeOID := InvalidOID;
  Val.Known := False;
  Add(Val);
  Sort(SortCompare);
  Inc(fUnkownCount);
end;

function TZOID2OIDMapList.SortCompare(Item1, Item2: Pointer): Integer;
begin
  Result := Ord(PZPGDomain2BaseTypeMap(Item1)^.DomainOID > PZPGDomain2BaseTypeMap(Item2)^.DomainOID)-
            Ord(PZPGDomain2BaseTypeMap(Item1)^.DomainOID < PZPGDomain2BaseTypeMap(Item2)^.DomainOID);
end;

initialization
  PostgreSQLDriver := TZPostgreSQLDriver.Create;
  DriverManager.RegisterDriver(PostgreSQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(PostgreSQLDriver);
  PostgreSQLDriver := nil;
{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.
