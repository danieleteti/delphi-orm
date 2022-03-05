{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{               ADO Connectivity Classes                  }
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

unit ZDbcAdo;

interface

{$I ZDbc.inc}

{$IF not defined(MSWINDOWS) and not defined(ZEOS_DISABLE_ADO)}
  {$DEFINE ZEOS_DISABLE_ADO}
{$IFEND}

{$IFNDEF ZEOS_DISABLE_ADO}
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcConnection, ZDbcIntfs, ZCompatibility, ZPlainAdoDriver,
  ZPlainAdo, ZURL, ZTokenizer;

type
  {** Implements Ado Database Driver. }
  TZAdoDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;
    function GetTokenizer: IZTokenizer; override;
  end;

  {** Represents an Ado specific connection interface. }
  IZAdoConnection = interface (IZConnection)
    ['{50D1AF76-0174-41CD-B90B-4FB770EFB14F}']
    function GetAdoConnection: ZPlainAdo.Connection;
    procedure InternalExecuteStatement(const SQL: ZWideString);
  end;

  TZServerProvider = (spUnknown, spMSSQL, spMSJet, spOracle, spASE, spASA,
    spPostgreSQL, spIB_FB, spMySQL, spNexusDB, spSQLite, spDB2, spAS400,
    spInformix, spCUBRID, spFoxPro);

  {** Implements a generic Ado Connection. }
  TZAdoConnection = class(TZAbstractConnection, IZAdoConnection)
  private
    fServerProvider: TZServerProvider;
    procedure ReStartTransactionSupport;
  protected
    FAdoConnection: ZPlainAdo.Connection;
    function GetAdoConnection: ZPlainAdo.Connection;
    procedure InternalExecuteStatement(const SQL: ZWideString);
    procedure StartTransaction;
    procedure InternalCreate; override;
  public
    destructor Destroy; override;

    function GetBinaryEscapeString(const Value: TBytes): String; overload; override;
    function GetBinaryEscapeString(const Value: RawByteString): String; overload; override;
    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;
    function CreateSequence(const Sequence: string; BlockSize: Integer):
      IZSequence; override;

    procedure SetAutoCommit(Value: Boolean); override;
    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure InternalClose; override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;
  end;

var
  {** The common driver manager object. }
  AdoDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_ADO}
implementation
{$IFNDEF ZEOS_DISABLE_ADO}

uses
  Variants, ActiveX, {$IFDEF FPC}ZOleDB{$ELSE}OleDB{$ENDIF},
  ZDbcUtils, ZDbcLogging, ZAdoToken, ZSysUtils, ZMessages,
  ZDbcAdoStatement, ZDbcAdoMetadata, ZEncoding, ZDbcAdoUtils;

const                                                //adXactUnspecified
  IL: array[TZTransactIsolationLevel] of TOleEnum = (adXactChaos, adXactReadUncommitted, adXactReadCommitted, adXactRepeatableRead, adXactSerializable);

{ TZAdoDriver }

{**
  Constructs this object with default properties.
}
constructor TZAdoDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZAdoPlainDriver.Create));
end;

{**
  Attempts to make a database connection to the given URL.
}
function TZAdoDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZAdoConnection.Create(Url);
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZAdoDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZAdoDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

function TZAdoDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZAdoSQLTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

var //eh: was threadvar but this defintely does not work! we just need !one! value
  AdoCoInitialized: integer;

procedure CoInit;
begin
  inc(AdoCoInitialized);
  if AdoCoInitialized=1 then
    CoInitialize(nil);
end;

procedure CoUninit;
begin
  assert(AdoCoInitialized>0);
  dec(AdoCoInitialized);
  if AdoCoInitialized=0 then
    CoUninitialize;
end;
{ TZAdoConnection }

procedure TZAdoConnection.InternalCreate;
begin
  CoInit;
  FAdoConnection := CoConnection.Create;
  Self.FMetadata := TZAdoDatabaseMetadata.Create(Self, URL);
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAdoConnection.Destroy;
begin
  Close;
  FAdoConnection := nil;
  inherited Destroy;
  CoUninit;
end;

{**
  Just return the Ado Connection
}
function TZAdoConnection.GetAdoConnection: ZPlainAdo.Connection;
begin
  Result := FAdoConnection;
end;

{**
  Executes simple statements internally.
}
procedure TZAdoConnection.InternalExecuteStatement(const SQL: ZWideString);
var
  RowsAffected: OleVariant;
begin
  try
    FAdoConnection.Execute(SQL, RowsAffected, adExecuteNoRecords);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, ConSettings^.ConvFuncs.ZUnicodeToRaw(SQL, ZOSCodePage));
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, ConSettings^.ConvFuncs.ZUnicodeToRaw(SQL, ZOSCodePage), 0,
        ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end;
end;

{**
  Starts a transaction support.
}
procedure TZAdoConnection.ReStartTransactionSupport;
begin
  if Closed then Exit;

  if not (AutoCommit) then
    StartTransaction;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZAdoConnection.Open;
var
  LogMessage: RawByteString;
  ConnectStrings: TStrings;
  DBInitialize: IDBInitialize;
  Command: ZPlainAdo.Command;
  DBCreateCommand: IDBCreateCommand;
  GetDataSource: IGetDataSource;
  function ProviderNamePrefix2ServerProvider(const ProviderNamePrefix: String): TZServerProvider;
  type
    TDriverNameAndServerProvider = record
      ProviderNamePrefix: String;
      Provider: TZServerProvider;
    end;
  const
    KnownDriverName2TypeMap: array[0..12] of TDriverNameAndServerProvider = (
      (ProviderNamePrefix: 'ORAOLEDB';      Provider: spOracle),
      (ProviderNamePrefix: 'MSDAORA';       Provider: spOracle),
      (ProviderNamePrefix: 'SQLNCLI';       Provider: spMSSQL),
      (ProviderNamePrefix: 'SQLOLEDB';      Provider: spMSSQL),
      (ProviderNamePrefix: 'SSISOLEDB';     Provider: spMSSQL),
      (ProviderNamePrefix: 'MSDASQL';       Provider: spMSSQL), //??
      (ProviderNamePrefix: 'MYSQLPROV';     Provider: spMySQL),
      (ProviderNamePrefix: 'IBMDA400';      Provider: spAS400),
      (ProviderNamePrefix: 'IFXOLEDBC';     Provider: spInformix),
      (ProviderNamePrefix: 'MICROSOFT.JET.OLEDB'; Provider: spMSJet),
      (ProviderNamePrefix: 'IB';            Provider: spIB_FB),
      (ProviderNamePrefix: 'POSTGRESSQL';   Provider: spPostgreSQL),
      (ProviderNamePrefix: 'CUBRID';        Provider: spCUBRID)
      );
  var
    I: Integer;
    ProviderNamePrefixUp: string;
  begin
    Result := spMSSQL;
    ProviderNamePrefixUp := UpperCase(ProviderNamePrefix);
    for i := low(KnownDriverName2TypeMap) to high(KnownDriverName2TypeMap) do
      if StartsWith(ProviderNamePrefixUp, KnownDriverName2TypeMap[i].ProviderNamePrefix) then begin
        Result := KnownDriverName2TypeMap[i].Provider;
        Break;
      end;
  end;
begin
  if not Closed then Exit;

  LogMessage := 'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"';
  try
    if ReadOnly then
      FAdoConnection.Set_Mode(adModeRead)
    else
      FAdoConnection.Set_Mode(adModeUnknown);

    ConnectStrings := SplitString(DataBase, ';');
    FServerProvider := ProviderNamePrefix2ServerProvider(ConnectStrings.Values['Provider']);
    FreeAndNil(ConnectStrings);

    FAdoConnection.Open(WideString(Database), WideString(User), WideString(Password), -1{adConnectUnspecified});
    FAdoConnection.Set_CursorLocation(adUseClient);
    DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);
    ConSettings^.AutoEncode := {$IFDEF UNICODE}False{$ELSE}True{$ENDIF};
    CheckCharEncoding('CP_UTF16');
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcConnect, ConSettings^.Protocol, LogMessage, 0,
        ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end;

  inherited Open;

  {EH: the only way to get back to generic Ole is using the command ... }
  Command := CoCommand.Create;
  Command.Set_ActiveConnection(FAdoConnection);
  if Succeeded(((Command as ADOCommandConstruction).OLEDBCommand as ICommand).GetDBSession(IID_IDBCreateCommand, IInterface(DBCreateCommand))) then
    if DBCreateCommand.QueryInterface(IID_IGetDataSource, GetDataSource) = S_OK then
      if Succeeded(GetDataSource.GetDataSource(IID_IDBInitialize, IInterFace(DBInitialize))) then
        (GetMetadata.GetDatabaseInfo as IZOleDBDatabaseInfo).InitilizePropertiesFromDBInfo(DBInitialize, ZAdoMalloc);

  if not GetMetadata.GetDatabaseInfo.SupportsTransactionIsolationLevel(GetTransactionIsolation) then
    inherited SetTransactionIsolation(GetMetadata.GetDatabaseInfo.GetDefaultTransactionIsolation);
  FAdoConnection.IsolationLevel := IL[GetTransactionIsolation];
  ReStartTransactionSupport;
end;

function TZAdoConnection.GetBinaryEscapeString(const Value: TBytes): String;
begin
  Result := GetSQLHexString(PAnsiChar(Value), Length(Value), True);
end;

function TZAdoConnection.GetBinaryEscapeString(const Value: RawByteString): String;
begin
  Result := GetSQLHexString(PAnsiChar(Value), Length(Value), True);
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
function TZAdoConnection.CreateRegularStatement(Info: TStrings): IZStatement;
begin
  if IsClosed then Open;
  Result := TZAdoStatement.Create(Self, Info);
end;

const
  TZDefaultProviderSequenceClasses: array[TZServerProvider] of TZAbstractSequenceClass = (
    {spUnknown}   nil,
    {spMSSQL}     TZMSSQLSequence,
    {spMSJet}     nil,
    {spOracle}    TZOracleSequence,
    {spASE}       nil,
    {spASA}       TZDotCurrvalNextvalSequence,
    {spPostgreSQL}TZPostgreSQLSequence,
    {spIB_FB}     TZFirebird2UpSequence,
    {spMySQL}     nil,
    {spNexusDB}   nil,
    {spSQLite}    nil,
    {spDB2}       TZDB2Squence,
    {spAS400}     nil,
    {spInformix}  TZInformixSquence,
    {spCUBRID}    TZCubridSquence,
    {spFoxPro}    nil
    );

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZAdoConnection.CreateSequence(const Sequence: string;
  BlockSize: Integer): IZSequence;
begin
  if TZDefaultProviderSequenceClasses[fServerProvider] <> nil then
    Result := TZDefaultProviderSequenceClasses[fServerProvider].Create(Self, Sequence, BlockSize)
  else
    Result := inherited CreateSequence(Sequence, BlockSize);
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
function TZAdoConnection.CreatePreparedStatement(
  const SQL: string; Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then Open;
  if GetMetadata.GetDatabaseInfo.SupportsParameterBinding
  then Result := TZAdoPreparedStatement.Create(Self, SQL, Info)
  else Result := TZAdoEmulatedPreparedStatement.Create(Self, SQL, Info)
end;

{**
  Creates a <code>CallableStatement</code> object for calling
  database stored procedures.
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
function TZAdoConnection.CreateCallableStatement(const SQL: string; Info: TStrings):
  IZCallableStatement;
begin
  if IsClosed then Open;
  Result := TZAdoCallableStatement.Create(Self, SQL, Info);
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
procedure TZAdoConnection.SetAutoCommit(Value: Boolean);
begin
  if AutoCommit = Value then  Exit;
  if not Closed and Value then
  begin
    if (FAdoConnection.State = adStateOpen) and
       (GetTransactionIsolation <> tiNone) then
      begin
        FAdoConnection.CommitTrans;
        DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, 'COMMIT');
      end;
  end;
  inherited;
  ReStartTransactionSupport;
end;

{**
  Attempts to change the transaction isolation level to the one given.
  The constants defined in the interface <code>Connection</code>
  are the possible transaction isolation levels.

  <P><B>Note:</B> This method cannot be called while
  in the middle of a transaction.

  @param level one of the TRANSACTION_* isolation values with the
    exception of TRANSACTION_NONE; some databases may not support other values
  @see DatabaseMetaData#supportsTransactionIsolationLevel
}
procedure TZAdoConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if GetTransactionIsolation = Level then Exit;

  if not Closed and not AutoCommit and (GetTransactionIsolation <> tiNone) then
  begin
    FAdoConnection.CommitTrans;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, 'COMMIT');
  end;

  inherited;

  if not Closed then
    FAdoConnection.IsolationLevel := IL[Level];

  RestartTransactionSupport;
end;

{**
  Starts a new transaction. Used internally.
}
procedure TZAdoConnection.StartTransaction;
var
  LogMessage: RawByteString;
begin
  LogMessage := 'BEGIN TRANSACTION';
  try
    FAdoConnection.BeginTrans;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, LogMessage);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, LogMessage, 0,
       ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZAdoConnection.Commit;
var
  LogMessage: RawByteString;
begin
  LogMessage := 'COMMIT';
  if not (AutoCommit or (GetTransactionIsolation = tiNone)) then
  try
    FAdoConnection.CommitTrans;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, LogMessage);
    StartTransaction;
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, LogMessage, 0,
       ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZAdoConnection.Rollback;
var
  LogMessage: RawbyteString;
begin
  LogMessage := 'ROLLBACK';
  if not (AutoCommit or (GetTransactionIsolation = tiNone)) then
  try
    FAdoConnection.RollbackTrans;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, LogMessage);
    StartTransaction;
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, LogMessage, 0,
       ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
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
procedure TZAdoConnection.InternalClose;
var
  LogMessage: RawByteString;
begin
  if Closed or (not Assigned(PlainDriver)) then
    Exit;

  SetAutoCommit(True);
  try
    LogMessage := 'CLOSE CONNECTION TO "'+ConSettings^.Database+'"';
    if FAdoConnection.State = adStateOpen then
      FAdoConnection.Close;
//      FAdoConnection := nil;
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, LogMessage);
  except
    on E: Exception do begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, LogMessage, 0,
       ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end;
end;

{**
  Sets a catalog name in order to select
  a subspace of this Connection's database in which to work.
  If the driver does not support catalogs, it will
  silently ignore this request.
}
procedure TZAdoConnection.SetCatalog(const Catalog: string);
var
  LogMessage: RawByteString;
begin
  if Closed then Exit;

  LogMessage := 'SET CATALOG '+ConSettings^.ConvFuncs.ZStringToRaw(Catalog, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP);
  try
    if Catalog <> '' then //see https://sourceforge.net/p/zeoslib/tickets/117/
      FAdoConnection.DefaultDatabase := WideString(Catalog);
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, LogMessage);
  except
    on E: Exception do
    begin
      DriverManager.LogError(lcExecute, ConSettings^.Protocol, LogMessage, 0,
       ConvertEMsgToRaw(E.Message, ConSettings^.ClientCodePage^.CP));
      raise;
    end;
  end;
end;

{**
  Returns the Connection's current catalog name.
  @return the current catalog name or null
}
function TZAdoConnection.GetCatalog: string;
begin
  Result := String(FAdoConnection.DefaultDatabase);
end;

initialization
  AdoCoInitialized := 0;
  AdoDriver := TZAdoDriver.Create;
  DriverManager.RegisterDriver(AdoDriver);
finalization
  if Assigned(DriverManager) then
    DriverManager.DeregisterDriver(AdoDriver);
  AdoDriver := nil;
{$ENDIF ZEOS_DISABLE_ADO}
end.