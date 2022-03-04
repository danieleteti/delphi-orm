{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           SQLite Database Connectivity Classes          }
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

unit ZDbcSqLite;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZDbcConnection, ZPlainSqLiteDriver, ZDbcLogging, ZTokenizer,
  ZGenericSqlAnalyser, ZURL, ZCompatibility;

type

  {** Implements SQLite Database Driver. }
  TZSQLiteDriver = class(TZAbstractDriver)
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
  end;

  {** Represents a SQLite specific connection interface. }
  IZSQLiteConnection = interface (IZConnection)
    ['{A4B797A9-7CF7-4DE9-A5BB-693DD32D07D2}']
    function GetPlainDriver: IZSQLitePlainDriver;
    function GetConnectionHandle: Psqlite;
    function GetUndefinedVarcharAsStringLength: Integer;
    function ExtendedErrorMessage: Boolean;
    function enable_load_extension(OnOff: Boolean): Integer;
    function load_extension(const zFile, zProc: String; out ErrMsg: String): Integer;
  end;

  {** Implements SQLite Database Connection. }

  { TZSQLiteConnection }
  TSQLite3TransactionAction = (traBegin, traCommit, traRollBack);
  TSQLite3TransactionStmt = record
    Stmt: Psqlite3_stmt;
    SQL: RawByteString;
    nBytes: Integer;
  end;
  TZSQLiteConnection = class(TZAbstractConnection, IZSQLiteConnection)
  private
    FCatalog: string;
    FHandle: Psqlite;
    FPlainDriver: IZSQLitePlainDriver;
    FTransactionStmts: array[TSQLite3TransactionAction] of TSQLite3TransactionStmt;
    FExtendedErrorMessage: Boolean;
    procedure ExecTransactionStmt(Action: TSQLite3TransactionAction);
  protected
    procedure InternalCreate; override;
    procedure StartTransactionSupport;
    function GetUndefinedVarcharAsStringLength: Integer;
    function ExtendedErrorMessage: Boolean;
    function enable_load_extension(OnOff: Boolean): Integer;
    function load_extension(const zFile, zProc: String; out ErrMsg: String): Integer;
  public
    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    procedure Open; override;
    procedure InternalClose; override;

    procedure SetAutoCommit(Value: Boolean); override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;

    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;

    function GetPlainDriver: IZSQLitePlainDriver;
    function GetConnectionHandle: Psqlite;

    function ReKey(const Key: string): Integer;
    function Key(const Key: string): Integer;
	
    function GetServerProvider: TZServerProvider; override;	
  end;

var
  {** The common driver manager object. }
  SQLiteDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_SQLITE} //if set we have an empty unit

uses
  ZSysUtils, ZDbcSqLiteStatement, ZSqLiteToken, ZFastCode, ZEncoding,
  ZDbcSqLiteUtils, ZDbcSqLiteMetadata, ZSqLiteAnalyser, ZMessages
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{ TZSQLiteDriver }

{**
  Constructs this object with default properties.
}
constructor TZSQLiteDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZSQLite3PlainDriver.Create, 'sqlite'));
  AddSupportedProtocol(AddPlainDriverToCache(TZSQLite3PlainDriver.Create));
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
function TZSQLiteDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZSQLiteConnection.Create(Url);
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZSQLiteDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZSQLiteDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZSQLiteDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZSQLiteTokenizer.Create; { thread save! Allways return a new Tokenizer! }
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZSQLiteDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZSQLiteStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{ TZSQLiteConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZSQLiteConnection.InternalCreate;
begin
  FMetadata := TZSQLiteDatabaseMetadata.Create(Self, Url);
  //https://sqlite.org/pragma.html#pragma_read_uncommitted
  inherited SetTransactionIsolation(tiSerializable);
  CheckCharEncoding('UTF-8');
  FUndefinedVarcharAsStringLength := StrToIntDef(Info.Values['Undefined_Varchar_AsString_Length'], 0);
  FTransactionStmts[traBegin].SQL := 'BEGIN TRANSACTION';
  FTransactionStmts[traBegin].nBytes := Length(FTransactionStmts[traBegin].SQL);
  FTransactionStmts[traCommit].SQL := 'COMMIT TRANSACTION';
  FTransactionStmts[traCommit].nBytes := Length(FTransactionStmts[traCommit].SQL);
  FTransactionStmts[traRollBack].SQL := 'ROLLBACK TRANSACTION';
  FTransactionStmts[traRollBack].nBytes := Length(FTransactionStmts[traRollBack].SQL);
  FExtendedErrorMessage := StrToBoolDef(Info.Values['ExtendedErrorMessage'], False);
end;

{**
  Set encryption key for a database
  @param Key the key used to encrypt your database.
  @return error code from SQLite Key function.
}
function TZSQLiteConnection.Key(const Key: string):Integer;
var
  ErrorCode: Integer;
begin
  {$IFDEF UNICODE}
  ErrorCode := GetPlainDriver.Key(FHandle, PAnsiChar(UTF8String(Key)), ZFastCode.StrLen(PAnsiChar(UTF8String(Key))));
  {$ELSE}
  ErrorCode := GetPlainDriver.Key(FHandle, PAnsiChar(Key), ZFastCode.StrLen(PAnsiChar(Key)));
  {$ENDIF}
  Result := ErrorCode;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5057 off : hint local variable "pzErrMsg" does not seem to be intialized} {$ENDIF}
function TZSQLiteConnection.load_extension(const zFile, zProc: String;
  out ErrMsg: String): Integer;
var rFile, rProc: RawByteString;
  pzErrMsg: PAnsiChar;
  L: LengthInt;
begin
  {$IFDEF UNICODE}
  rFile := ZUnicodeToRaw(zFile, zCP_UTF8);
  rProc := ZUnicodeToRaw(zProc, zCP_UTF8);
  {$ELSE !UNICODE}
    {$IF defined(LCL) or not defined(MSWINDOWS)}
  rFile := zFile;
  rProc := zProc;
    {$ELSE}
  if ZDetectUTF8Encoding(Pointer(zFile), Length(zFile)) = etANSI
  then rFile := ZConvertAnsiToUTF8(zFile)
  else rFile := zFile;
  if ZDetectUTF8Encoding(Pointer(zProc), Length(zProc)) = etANSI
  then rProc := ZConvertAnsiToUTF8(zProc)
  else rProc := zProc;
    {$IFEND}
  {$ENDIF UNICODE}
  Result := FPlainDriver.load_extension(FHandle, Pointer(rFile), Pointer(rProc), pzErrMsg);
  if (Result = SQLITE_ERROR) then begin
    L := StrLen(pzErrMsg);
    {$IFDEF UNICODE}
    ErrMsg := PRawToUnicode(pzErrMsg, L, zCP_UTF8);
    {$ELSE}
      {$IF defined(LCL) or not defined(MSWINDOWS)}
      System.SetString(ErrMsg, pzErrMsg, L);
      {$ELSE}
      System.SetString(ErrMsg, pzErrMsg, L);
      ErrMsg := ZConvertUTF8ToAnsi(ErrMsg);
      {$IFEND}
    {$ENDIF}
    FPlainDriver.FreeMem(pzErrMsg);
  end else ErrMsg := '';
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Reencrypt a database with a new key. The old/current key needs to be
  set before calling this function.
  @param Key the new key used to encrypt your database.
  @return error code from SQLite ReKey function.
}
function TZSQLiteConnection.ReKey(const Key: string):Integer;
var
  ErrorCode: Integer;
begin
  {$IFDEF UNICODE}
  ErrorCode := GetPlainDriver.ReKey(FHandle, PAnsiChar(UTF8String(Key)), ZFastCode.StrLen(PAnsiChar(UTF8String(Key))));
  {$ELSE}
  ErrorCode := GetPlainDriver.ReKey(FHandle, PAnsiChar(Key), ZFastCode.StrLen(PAnsiChar(Key)));
  {$ENDIF}
  Result := ErrorCode;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZSQLiteConnection.Open;
var
  LogMessage: RawByteString;
  SQL, zVfs: RawByteString;
  TmpInt, Flags: Integer;
  Stmt: IZStatement;
  S: String;
begin
  if not Closed then
    Exit;

  LogMessage := 'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"';

  SQL := {$IFDEF UNICODE}UTF8String{$ENDIF}(Database);
  S := Info.Values['SQLiteOpen_Flags'];
  Flags := StrToIntDef(S, 0);
  S := Info.Values['SQLiteOpen_zVfs'];
  zVfs := {$IFDEF UNICODE}UTF8String{$ENDIF}(S);

  //patch by omaga software see https://sourceforge.net/p/zeoslib/tickets/312/
  if (zVfs <> '') or (Flags <> 0)
  then TmpInt := GetPlainDriver.open_V2(Pointer(SQL), FHandle, Flags, Pointer(zVfs))
  else TmpInt := GetPlainDriver.open(Pointer(SQL), FHandle);
  if TmpInt <> SQLITE_OK then
    CheckSQLiteError(FPlainDriver, FHandle, TmpInt, lcConnect, LogMessage, ConSettings, FExtendedErrorMessage);
  DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);

  { Turn on encryption if requested }
  if StrToBoolEx(Info.Values['encrypted']) and (Password <> '') then
  begin
    SQL := {$IFDEF UNICODE}UTF8String{$ENDIF}(Password);
    CheckSQLiteError(FPlainDriver, FHandle,
      GetPlainDriver.Key(FHandle, Pointer(SQL), Length(SQL)),
      lcConnect, 'SQLite.Key', ConSettings, FExtendedErrorMessage);
  end;

  { Set busy timeout if requested }
  TmpInt := StrToIntDef(Info.Values['busytimeout'], -1);
  if TmpInt >= 0 then
    GetPlainDriver.BusyTimeout(FHandle, TmpInt);

  Stmt := TZSQLiteStatement.Create(GetPlainDriver, Self, Info, FHandle);
  { pimp performance }
  Stmt.ExecuteUpdate('PRAGMA cache_size = '+IntToRaw(StrToIntDef(Info.Values['cache_size'], 10000)));

  //see http://www.sqlite.org/pragma.html#pragma_synchronous
  //0 brings best performance
  if Info.Values['synchronous'] <> '' then
    Stmt.ExecuteUpdate('PRAGMA synchronous = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values['synchronous']));

  //see http://www.sqlite.org/pragma.html#pragma_locking_mode
  //EXCLUSIVE brings best performance
  if Info.Values['locking_mode'] <> '' then
    Stmt.ExecuteUpdate('PRAGMA locking_mode = '+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(Info.Values['locking_mode']));

  try
    if ( FClientCodePage <> '' ) and (FClientCodePage <> 'UTF-8') then
      Stmt.ExecuteUpdate('PRAGMA encoding = '''+{$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FClientCodePage)+'''');

    Stmt.ExecuteUpdate('PRAGMA show_datatypes = ON');

    if Info.Values['foreign_keys'] <> '' then
      Stmt.ExecuteUpdate('PRAGMA foreign_keys = '+BoolStrIntsRaw[StrToBoolEx(Info.Values['foreign_keys'])] );
    if not GetAutoCommit then
      ExecTransactionStmt(traBegin);
  except
    GetPlainDriver.Close(FHandle);
    FHandle := nil;
    raise;
  end;

  inherited Open;
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
function TZSQLiteConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
    Open;

  Result := TZSQLiteStatement.Create(GetPlainDriver, Self, Info, FHandle);
end;

function TZSQLiteConnection.enable_load_extension(OnOff: Boolean): Integer;
begin
  Result := FPlainDriver.enable_load_extension(FHandle, Ord(OnOff));
end;

procedure TZSQLiteConnection.ExecTransactionStmt(
  Action: TSQLite3TransactionAction);
var
  pzTail: PAnsiChar;
begin
  with FTransactionStmts[Action] do begin
    if Stmt = nil then
      CheckSQLiteError(GetPlainDriver, FHandle,
        GetPlainDriver.Prepare(FHandle, Pointer(SQL), nBytes, Stmt, pzTail{%H-}),
          lcTransaction, SQL, ConSettings, FExtendedErrorMessage);
    try
      CheckSQLiteError(FPlainDriver, FHandle, FPlainDriver.Step(Stmt),
        lcTransaction, SQL, ConSettings, FExtendedErrorMessage);
      if Assigned(DriverManager) and DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, SQL);
    finally
      FPlainDriver.reset(Stmt);
    end;
  end;
end;

function TZSQLiteConnection.ExtendedErrorMessage: Boolean;
begin
  Result := FExtendedErrorMessage;
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
function TZSQLiteConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
    Open;
  Result := TZSQLiteCAPIPreparedStatement.Create(GetPlainDriver, Self, SQL, Info, FHandle);
end;

{**
  Starts a transaction support.
}
procedure TZSQLiteConnection.StartTransactionSupport;
begin
  if not Closed and not AutoCommit then
    ExecTransactionStmt(traBegin);
end;

function TZSQLiteConnection.GetUndefinedVarcharAsStringLength: Integer;
begin
  Result := FUndefinedVarcharAsStringLength;
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZSQLiteConnection.Commit;
begin
  if not Closed then
    if not GetAutoCommit then begin
      ExecTransactionStmt(traCommit);
      ExecTransactionStmt(traBegin);
    end else
      raise Exception.Create(SInvalidOpInAutoCommit);
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZSQLiteConnection.Rollback;
begin
  if not Closed then
    if not GetAutoCommit then begin
      ExecTransactionStmt(traRollback);
      ExecTransactionStmt(traBegin);
    end else
      raise Exception.Create(SInvalidOpInAutoCommit);
end;

{**
  Releases a Connection's database and JDBC resources
  immediately instead of waiting for
  them to be automatically released.

  <P><B>Note:</B> A Connection is automatically closed when it is
  garbage collected. Certain fatal errors also result in a closed
  Connection.
}
procedure TZSQLiteConnection.InternalClose;
var
  LogMessage: RawByteString;
  ErrorCode: Integer;
  TransactionAction: TSQLite3TransactionAction;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;
  LogMessage := 'DISCONNECT FROM "'+ConSettings^.Database+'"';
  for TransactionAction := low(TSQLite3TransactionAction) to high(TSQLite3TransactionAction) do
    if FTransactionStmts[TransactionAction].Stmt <> nil then begin
      GetPlainDriver.finalize(FTransactionStmts[TransactionAction].Stmt);
      FTransactionStmts[TransactionAction].Stmt := nil;
    end;
  ErrorCode := GetPlainDriver.Close(FHandle);
  FHandle := nil;
  CheckSQLiteError(GetPlainDriver, FHandle, ErrorCode,
    lcOther, LogMessage, ConSettings, FExtendedErrorMessage);
  if Assigned(DriverManager) and DriverManager.HasLoggingListener then //thread save
    DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol, LogMessage);
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZSQLiteConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

function TZSQLiteConnection.GetClientVersion: Integer;
begin
  Result := ConvertSQLiteVersionToSQLVersion(GetPlainDriver.LibVersion);
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZSQLiteConnection.SetAutoCommit(Value: Boolean);
begin
  if Value <> GetAutoCommit then begin
    if not GetAutoCommit and not Closed then
      ExecTransactionStmt(traRollBack);
    inherited SetAutoCommit(Value);
    if not Value and not Closed then
      ExecTransactionStmt(traBegin);
  end;
end;

procedure TZSQLiteConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZSQLiteConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if Level <> GetTransactionIsolation then begin
    if not GetAutoCommit and not Closed then
      ExecTransactionStmt(traRollBack);
    inherited SetTransactionIsolation(Level);
    if not GetAutoCommit and not Closed then
      ExecTransactionStmt(traBegin);
  end;
end;

{**
  Gets a reference to SQLite connection handle.
  @return a reference to SQLite connection handle.
}
function TZSQLiteConnection.GetConnectionHandle: Psqlite;
begin
  Result := FHandle;
end;

function TZSQLiteConnection.GetServerProvider: TZServerProvider;
begin
  Result := spSQLite;
end;

{**
  Gets a SQLite plain driver interface.
  @return a SQLite plain driver interface.
}
function TZSQLiteConnection.GetPlainDriver: IZSQLitePlainDriver;
begin
  if fPlainDriver = nil then
    fPlainDriver := PlainDriver as IZSQLitePlainDriver;
  Result := fPlainDriver;
end;

function TZSQLiteConnection.GetHostVersion: Integer;
begin
  Result := ConvertSQLiteVersionToSQLVersion(GetPlainDriver.LibVersion);
end;

initialization
  SQLiteDriver := TZSQLiteDriver.Create;
  DriverManager.RegisterDriver(SQLiteDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(SQLiteDriver);
  SQLiteDriver := nil;

{$ENDIF ZEOS_DISABLE_SQLITE} //if set we have an empty unit
end.
