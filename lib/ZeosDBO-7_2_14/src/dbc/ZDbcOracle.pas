{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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

unit ZDbcOracle;

interface

{$I ZDbc.inc}
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs{$ELSE}ZClasses{$ENDIF},
  ZCompatibility, ZDbcIntfs, ZDbcConnection, ZPlainOracleDriver, ZDbcLogging,
  ZTokenizer, ZDbcGenericResolver, ZURL, ZGenericSqlAnalyser,
  ZPlainOracleConstants;

type

  {** Implements Oracle Database Driver. }
  TZOracleDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a Oracle specific connection interface. }
  IZOracleConnection = interface (IZConnection)
    ['{C7F36FDF-8A64-477B-A0EB-3E8AB7C09F8D}']

    function GetPlainDriver: IZOraclePlainDriver;
    function GetConnectionHandle: POCIEnv;
    function GetContextHandle: POCISvcCtx;
    function GetErrorHandle: POCIError;
    function GetServerHandle: POCIServer;
    function GetSessionHandle: POCISession;
    function GetTransactionHandle: POCITrans;
    function GetDescribeHandle: POCIDescribe;
  end;

  {** Implements Oracle Database Connection. }
  TZOracleConnection = class(TZAbstractConnection, IZOracleConnection)
  private
    FCatalog: string;
    FHandle: POCIEnv;
    FContextHandle: POCISvcCtx;
    FErrorHandle: POCIError;
    FServerHandle: POCIServer;
    FSessionHandle: POCISession;
    FTransHandle: POCITrans;
    FDescibeHandle: POCIDescribe;
    FStatementPrefetchSize: Integer;
    FBlobPrefetchSize: Integer;
    FStmtMode: ub4;
    FPlainDriver: IZOraclePlainDriver;
    procedure InternalSetCatalog(const Catalog: RawByteString);
  protected
    procedure InternalCreate; override;
    procedure StartTransactionSupport;

  public
    destructor Destroy; override;

    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;

    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    function PingServer: Integer; override;

    procedure Open; override;
    procedure InternalClose; override;

    procedure SetCatalog(const Value: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence; override;

    function GetPlainDriver: IZOraclePlainDriver;
    function GetConnectionHandle: POCIEnv;
    function GetContextHandle: POCISvcCtx;
    function GetErrorHandle: POCIError;
    function GetServerHandle: POCIServer;
    function GetSessionHandle: POCISession;
    function GetTransactionHandle: POCITrans;
    function GetDescribeHandle: POCIDescribe;
    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;
    function GetBinaryEscapeString(const Value: TBytes): String; overload; override;
    function GetBinaryEscapeString(const Value: RawByteString): String; overload; override;
    function GetServerProvider: TZServerProvider; override;	
  end;

  {** Implements a specialized cached resolver for Oracle. }
  TZOracleCachedResolver = class(TZGenericCachedResolver)
  public
    function FormCalculateStatement(Columns: TObjectList): string; override;
  end;

var
  {** The common driver manager object. }
  OracleDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_ORACLE}
implementation
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  ZMessages, ZGenericSqlToken, ZDbcOracleStatement, ZSysUtils, ZFastCode,
  ZDbcOracleUtils, ZDbcOracleMetadata, ZOracleToken, ZOracleAnalyser
  {$IFNDEF NO_UNIT_CONTNRS}, ZClasses{$ENDIF};

{ TZOracleDriver }

{**
  Constructs this object with default properties.
}
constructor TZOracleDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZOracle9iPlainDriver.Create, 'oracle'));
  AddSupportedProtocol(AddPlainDriverToCache(TZOracle9iPlainDriver.Create));
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
function TZOracleDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZOracleConnection.Create(Url);
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZOracleDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZOracleDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZOracleDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZOracleTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZOracleDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZOracleStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZOracleConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZOracleConnection.InternalCreate;
begin
  FMetaData := TZOracleDatabaseMetadata.Create(Self, URL);

  { Sets a default properties }
  if Self.Port = 0 then
      Self.Port := 1521;

  if Info.Values['ServerCachedStmts'] = '' then
    FStmtMode := OCI_STMT_CACHE //use by default
  else
    if StrToBoolEx(Info.Values['ServerCachedStmts'], False) then
      FStmtMode := OCI_STMT_CACHE //use by default
    else
      FStmtMode := OCI_DEFAULT;
  FStatementPrefetchSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Info.Values['StatementCache'], 30); //default = 20
  FBlobPrefetchSize := FChunkSize;
end;

procedure TZOracleConnection.InternalSetCatalog(const Catalog: RawByteString);
begin
  CreateRegularStatement(nil).ExecuteUpdate('ALTER SESSION SET CURRENT_SCHEMA = '+Catalog)
end;

function TZOracleConnection.CreateCallableStatement(const SQL: string;
  Info: TStrings): IZCallableStatement;
begin
  if IsClosed then
     Open;
  Result := TZOracleCallableStatement.Create(Self, SQL, Info);
  Result.ClearParameters;
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZOracleConnection.Destroy;
begin
  if not IsClosed then
     Close;

  if FHandle <> nil then
  begin
    GetPlainDriver.HandleFree(FHandle, OCI_HTYPE_ENV);
    FHandle := nil;
  end;

  inherited Destroy;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZOracleConnection.Open;
var
  Status: Integer;
  LogMessage: RawByteString;
  OCI_CLIENT_CHARSET_ID,  OCI_CLIENT_NCHARSET_ID: ub2;
  S: String;
  mode: ub4;
  procedure CleanupOnFail;
  begin
    GetPlainDriver.HandleFree(FDescibeHandle, OCI_HTYPE_DESCRIBE);
    FDescibeHandle := nil;
    GetPlainDriver.HandleFree(FContextHandle, OCI_HTYPE_SVCCTX);
    FContextHandle := nil;
    GetPlainDriver.HandleFree(FErrorHandle, OCI_HTYPE_ERROR);
    FErrorHandle := nil;
    GetPlainDriver.HandleFree(FServerHandle, OCI_HTYPE_SERVER);
    FServerHandle := nil;
  end;

begin
  if not Closed then
     Exit;

  LogMessage := 'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"';

  { Sets a default port number. }
  if Port = 0 then
     Port := 1521;

  { Sets a client codepage. }
  OCI_CLIENT_CHARSET_ID := ConSettings.ClientCodePage^.ID;
  mode := OCI_OBJECT;
  S := Info.Values['OCIMultiThreaded'];
  if StrToBoolEx(S, False) Then
    mode := mode + OCI_THREADED;
  { Connect to Oracle database. }
  if ( FHandle = nil ) then begin
    FErrorHandle := nil;
    Status := GetPlainDriver.EnvNlsCreate(FHandle, mode, nil, nil, nil, nil, 0, nil,
      OCI_CLIENT_CHARSET_ID, OCI_CLIENT_CHARSET_ID);
    CheckOracleError(GetPlainDriver, FErrorHandle, Status, lcOther, 'EnvNlsCreate failed.', ConSettings);
  end;
  FErrorHandle := nil;
  GetPlainDriver.HandleAlloc(FHandle, FErrorHandle, OCI_HTYPE_ERROR, 0, nil);
  FServerHandle := nil;
  GetPlainDriver.HandleAlloc(FHandle, FServerHandle, OCI_HTYPE_SERVER, 0, nil);
  FContextHandle := nil;
  GetPlainDriver.HandleAlloc(FHandle, FContextHandle, OCI_HTYPE_SVCCTX, 0, nil);
  FDescibeHandle := nil;
  GetPlainDriver.HandleAlloc(FHandle, FDescibeHandle, OCI_HTYPE_DESCRIBE, 0, nil);

  Status := GetPlainDriver.ServerAttach(FServerHandle, FErrorHandle,
      PAnsiChar(ConSettings^.Database), Length(ConSettings^.Database), 0);
  try
    CheckOracleError(GetPlainDriver, FErrorHandle, Status, lcConnect, LogMessage, ConSettings);
  except
    CleanupOnFail;
    raise;
  end;

  if OCI_CLIENT_CHARSET_ID = 0 then
  begin
    OCI_CLIENT_NCHARSET_ID := High(ub2);
    GetPlainDriver.AttrGet(FHandle, OCI_HTYPE_ENV, @OCI_CLIENT_CHARSET_ID,
      nil, OCI_ATTR_ENV_CHARSET_ID, FErrorHandle); //Get Server default CodePage
    CheckCharEncoding(GetPlainDriver.ValidateCharEncoding(OCI_CLIENT_CHARSET_ID)^.Name);
    if OCI_CLIENT_CHARSET_ID <> OCI_CLIENT_NCHARSET_ID then
    begin
      CleanupOnFail;
      Open;
      Exit;
    end;
  end;
  CheckOracleError(GetPlainDriver, FErrorHandle,
    GetPlainDriver.NlsNumericInfoGet(FHandle, FErrorHandle,
      @ConSettings^.ClientCodePage^.CharWidth, OCI_NLS_CHARSET_MAXBYTESZ),
    lcConnect, LogMessage, ConSettings);

  GetPlainDriver.AttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FServerHandle, 0,
    OCI_ATTR_SERVER, FErrorHandle);
  GetPlainDriver.HandleAlloc(FHandle, FSessionHandle, OCI_HTYPE_SESSION, 0, nil);
  GetPlainDriver.AttrSet(FSessionHandle, OCI_HTYPE_SESSION, PAnsiChar(ConSettings^.User),
    Length(ConSettings^.User), OCI_ATTR_USERNAME, FErrorHandle);
  GetPlainDriver.AttrSet(FSessionHandle, OCI_HTYPE_SESSION, PAnsiChar({$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Password)),
    Length(Password), OCI_ATTR_PASSWORD, FErrorHandle);
  GetPlainDriver.AttrSet(FSessionHandle,OCI_HTYPE_SESSION,@fBlobPrefetchSize,0,
    OCI_ATTR_DEFAULT_LOBPREFETCH_SIZE,FErrorHandle);
  S := Info.Values['OCIAuthenticateMode'];
  Mode := StrToIntDef(S, OCI_DEFAULT);
  Status := GetPlainDriver.SessionBegin(FContextHandle, FErrorHandle,
    FSessionHandle, OCI_CRED_RDBMS, Mode);
  try
    CheckOracleError(GetPlainDriver, FErrorHandle, Status, lcConnect, LogMessage, ConSettings);
  except
    CleanupOnFail;
    raise;
  end;
  GetPlainDriver.AttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FSessionHandle, 0,
    OCI_ATTR_SESSION, FErrorHandle);
  DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);

  StartTransactionSupport;

  inherited Open;
  if FCatalog <> '' then
    InternalSetCatalog(ConSettings.ConvFuncs.ZStringToRaw(FCatalog, Consettings.CTRL_CP, ConSettings.ClientCodePage.CP));
end;

{**
  Starts a transaction support.
}
procedure TZOracleConnection.StartTransactionSupport;
var
  SQL: RawByteString;
  Status: Integer;
  Isolation: Integer;
begin
  if TransactIsolationLevel = tiNone then
  begin
    SQL := 'SET TRANSACTION ISOLATION LEVEL DEFAULT';
    Isolation := OCI_DEFAULT;
  end
  else if TransactIsolationLevel = tiReadCommitted then
  begin
// Behaviour changed by mdaems 31/05/2006 : Read Committed is the default
// isolation level used by oracle. This property should not be abused to add
// the non-standard isolation level 'read only' thats invented by oracle.
//    SQL := 'SET TRANSACTION ISOLATION LEVEL READONLY';
//    Isolation := OCI_TRANS_READONLY;
    SQL := 'SET TRANSACTION ISOLATION LEVEL DEFAULT';
    Isolation := OCI_DEFAULT;
  end
  else if TransactIsolationLevel = tiRepeatableRead then
  begin
    SQL := 'SET TRANSACTION ISOLATION LEVEL READWRITE';
    Isolation := OCI_TRANS_READWRITE;
  end
  else if TransactIsolationLevel = tiSerializable then
  begin
    SQL := 'SET TRANSACTION ISOLATION LEVEL SERIALIZABLE';
    Isolation := OCI_TRANS_SERIALIZABLE;
  end
  else
    raise EZSQLException.Create(SIsolationIsNotSupported);

  FTransHandle := nil;
  GetPlainDriver.HandleAlloc(FHandle, FTransHandle, OCI_HTYPE_TRANS, 0, nil);
  GetPlainDriver.AttrSet(FContextHandle, OCI_HTYPE_SVCCTX, FTransHandle, 0,
    OCI_ATTR_TRANS, FErrorHandle);

  Status := GetPlainDriver.TransStart(FContextHandle, FErrorHandle, 0, Isolation);
  CheckOracleError(GetPlainDriver, FErrorHandle, Status, lcExecute, SQL, ConSettings);

  if FStmtMode = OCI_STMT_CACHE  then
  begin
    Status := GetPlainDriver.AttrSet(FContextHandle,OCI_HTYPE_SVCCTX,@FStatementPrefetchSize,0,
        OCI_ATTR_STMTCACHESIZE,FErrorHandle);
    CheckOracleError(GetPlainDriver, FErrorHandle, Status, lcExecute, SQL, ConSettings);
  end;

  DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
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
function TZOracleConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZOraclePreparedStatement.Create(GetPlainDriver, Self, '', Info);
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
function TZOracleConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  Result := TZOraclePreparedStatement.Create(GetPlainDriver, Self, SQL, Info);
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZOracleConnection.Commit;
var
  Status: Integer;
  SQL: RawByteString;
begin
  if not Closed then begin
    SQL := 'COMMIT';

    Status := GetPlainDriver.TransCommit(FContextHandle, FErrorHandle,
      OCI_DEFAULT);
    CheckOracleError(GetPlainDriver, FErrorHandle, Status, lcExecute, SQL, ConSettings);

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
procedure TZOracleConnection.Rollback;
var
  Status: Integer;
  SQL: RawByteString;
begin
  if not Closed then
  begin
    SQL := 'ROLLBACK';

    Status := GetPlainDriver.TransRollback(FContextHandle, FErrorHandle,
      OCI_DEFAULT);
    CheckOracleError(GetPlainDriver, FErrorHandle, Status, lcExecute, SQL, ConSettings);

    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
  end;
end;

{**
  Ping Current Connection's server, if client was disconnected,
  the connection is resumed.
  @return 0 if succesfull or error code if any error occurs
}
function TZOracleConnection.PingServer: Integer;
begin
  if Closed or (FContextHandle = nil) Or (FErrorHandle = nil)
  then Result := -1
  else begin
    Result := GetPlainDriver.Ping(FContextHandle, FErrorHandle, OCI_DEFAULT);
    CheckOracleError(GetPlainDriver, FErrorHandle, Result, lcExecute, 'Ping Server', ConSettings);
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
procedure TZOracleConnection.InternalClose;
var
  LogMessage: RawByteString;
begin
  if Closed or not Assigned(PlainDriver) then
    Exit;
  LogMessage := 'DISCONNECT FROM "'+ConSettings^.Database+'"';
  { Closes started transaction }
  CheckOracleError(GetPlainDriver, FErrorHandle,
    GetPlainDriver.TransRollback(FContextHandle, FErrorHandle, OCI_DEFAULT),
    lcDisconnect, LogMessage, ConSettings);
  GetPlainDriver.HandleFree(FTransHandle, OCI_HTYPE_TRANS);
  FTransHandle := nil;

  { Closes the session }
  CheckOracleError(GetPlainDriver, FErrorHandle,
    GetPlainDriver.SessionEnd(FContextHandle, FErrorHandle, FSessionHandle,
    OCI_DEFAULT), lcDisconnect, LogMessage, ConSettings);

  { Detaches from the server }
  CheckOracleError(GetPlainDriver, FErrorHandle,
    GetPlainDriver.ServerDetach(FServerHandle, FErrorHandle, OCI_DEFAULT),
    lcDisconnect, LogMessage, ConSettings);

  { Frees all handlers }
  GetPlainDriver.HandleFree(FDescibeHandle, OCI_HTYPE_DESCRIBE);
  FDescibeHandle := nil;
  GetPlainDriver.HandleFree(FSessionHandle, OCI_HTYPE_SESSION);
  FSessionHandle := nil;
  GetPlainDriver.HandleFree(FContextHandle, OCI_HTYPE_SVCCTX);
  FContextHandle := nil;
  GetPlainDriver.HandleFree(FServerHandle, OCI_HTYPE_SERVER);
  FServerHandle := nil;
  GetPlainDriver.HandleFree(FErrorHandle, OCI_HTYPE_ERROR);
  FErrorHandle := nil;

  DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol, LogMessage);
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZOracleConnection.GetCatalog: string;
begin
  if not Closed and (FCatalog = '') then
    with CreateRegularStatement(nil).ExecuteQuery('SELECT SYS_CONTEXT (''USERENV'', ''CURRENT_SCHEMA'') FROM DUAL') do begin
      if Next then
        FCatalog := GetString(FirstDBCIndex);
      Close;
    end;
  Result := FCatalog;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZOracleConnection.SetCatalog(const Value: string);
begin
  if Value <> FCatalog then begin
    FCatalog := Value;
    if not Closed and (Value <> '') then
      InternalSetCatalog(ConSettings.ConvFuncs.ZStringToRaw(Value, Consettings.CTRL_CP, ConSettings.ClientCodePage.CP));
  end;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZOracleConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
var
  SQL: RawByteString;
begin
  if TransactIsolationLevel <> Level then
  begin
    inherited SetTransactionIsolation(Level);

    if not Closed then
    begin
      SQL := 'END TRANSACTION';
      CheckOracleError(GetPlainDriver, FErrorHandle,
        GetPlainDriver.TransRollback(FContextHandle, FErrorHandle, OCI_DEFAULT),
          lcExecute, SQL, ConSettings);
      GetPlainDriver.HandleFree(FTransHandle, OCI_HTYPE_TRANS);
      FTransHandle := nil;
      DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);

      StartTransactionSupport;
    end;
  end;
end;

{**
  Creates a sequence generator object.
  @param Sequence a name of the sequence generator.
  @param BlockSize a number of unique keys requested in one trip to SQL server.
  @returns a created sequence object.
}
function TZOracleConnection.CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence; 
begin
  Result := TZOracleSequence.Create(Self, Sequence, BlockSize);
end;
{**
  Gets a Oracle plain driver interface.
  @return a Oracle plain driver interface.
}
function TZOracleConnection.GetPlainDriver: IZOraclePlainDriver;
begin
  if fPlainDriver = nil then
    fPlainDriver := PlainDriver as IZOraclePlainDriver;
  Result := fPlainDriver;
end;

{**
  Gets a reference to Oracle connection handle.
  @return a reference to Oracle connection handle.
}
function TZOracleConnection.GetConnectionHandle: POCIEnv;
begin
  Result := FHandle;
end;

{**
  Gets a reference to Oracle context handle.
  @return a reference to Oracle context handle.
}
function TZOracleConnection.GetContextHandle: POCISvcCtx;
begin
  Result := FContextHandle;
end;

{**
  Gets a reference to Oracle error handle.
  @return a reference to Oracle error handle.
}
function TZOracleConnection.GetErrorHandle: POCIError;
begin
  Result := FErrorHandle;
end;

{**
  Gets a reference to Oracle server handle.
  @return a reference to Oracle server handle.
}
function TZOracleConnection.GetServerHandle: POCIServer;
begin
  Result := FServerHandle;
end;

function TZOracleConnection.GetServerProvider: TZServerProvider;
begin
  Result := spOracle;
end;

{**
  Gets a reference to Oracle session handle.
  @return a reference to Oracle session handle.
}
function TZOracleConnection.GetSessionHandle: POCISession;
begin
  Result := FSessionHandle;
end;

{**
  Gets a reference to Oracle transaction handle.
  @return a reference to Oracle transacton handle.
}
function TZOracleConnection.GetTransactionHandle: POCITrans;
begin
  Result := FTransHandle;
end;

{**
  Gets a reference to Oracle describe handle.
  @return a reference to Oracle describe handle.
}
function TZOracleConnection.GetDescribeHandle: POCIDescribe;
begin
  Result := FDescibeHandle;
end;

function TZOracleConnection.GetClientVersion: Integer;
var
  major_version, minor_version, update_num,
      patch_num, port_update_num: sword;
begin
  GetPlainDriver.ClientVersion(@major_version, @minor_version, @update_num,
      @patch_num, @port_update_num);
  Result := EncodeSQLVersioning(major_version,minor_version,update_num);
end;

function TZOracleConnection.GetHostVersion: Integer;
var
  buf:text;
  version:ub4;
begin
  result:=0;
  getmem(buf,1024);
  if GetPlainDriver.ServerRelease(FServerHandle,FErrorHandle,buf,1024,OCI_HTYPE_SERVER,@version)=OCI_SUCCESS then
    Result := EncodeSQLVersioning((version shr 24) and $ff,(version shr 20) and $f,(version shr 12) and $ff);
  freemem(buf);
end;

function TZOracleConnection.GetBinaryEscapeString(const Value: TBytes): String;
var
  L: Integer;
  P: PChar;
begin
  L := Length(Value);
  SetLength(Result, L*2+2);
  P := PChar(Result);
  P^ := #39;
  Inc(p);
  ZBinToHex(PAnsiChar(Value), P, L);
  (P+L)^ := #39;
end;

function TZOracleConnection.GetBinaryEscapeString(const Value: RawByteString): String;
var
  L: Integer;
  P: PChar;
begin
  L := Length(Value);
  SetLength(Result, L*2+2);
  P := PChar(Result);
  P^ := #39;
  Inc(p);
  ZBinToHex(PAnsiChar(Value), P, L);
  (P+L)^ := #39;
end;

{ TZOracleCachedResolver }

{**
  Forms a where clause for SELECT statements to calculate default values.
  @param Columns a collection of key columns.
  @param OldRowAccessor an accessor object to old column values.
}
function TZOracleCachedResolver.FormCalculateStatement(
  Columns: TObjectList): string;
var
   iPos: Integer;
begin
  Result := inherited FormCalculateStatement(Columns);
  if Result <> '' then
  begin
    iPos := ZFastCode.pos('FROM', uppercase(Result));
    if iPos > 0 then
    begin
      Result := copy(Result, 1, iPos+3) + ' DUAL';
    end
    else
    begin
      Result := Result + ' FROM DUAL';
    end;
  end;
end;

initialization
  OracleDriver := TZOracleDriver.Create;
  DriverManager.RegisterDriver(OracleDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(OracleDriver);
  OracleDriver := nil;
{$ENDIF ZEOS_DISABLE_ORACLE}
end.

