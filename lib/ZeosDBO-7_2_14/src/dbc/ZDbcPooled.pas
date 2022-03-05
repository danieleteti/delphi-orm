{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
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

unit ZDbcPooled;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POOLED} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SyncObjs,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ENDIF} DateUtils, SysUtils,
  ZCompatibility, ZClasses, ZURL, ZDbcConnection, ZDbcIntfs, ZPlainDriver,
  ZMessages, ZVariant;

type
  TConnectionPool = class;

  { This class searchs for timed out connections in a pool and destroy them.
    Maybe it would be nice to have a global instance to check all pools. This
    way, we could avoid creating many threads. }
  TConnectionTimeoutThread = class(TThread)
  private
    FConnectionPool: TConnectionPool;
  protected
    procedure Execute; override;
  public
    constructor Create(const ConnectionPool: TConnectionPool);
  end;

  { This class keeps a pool of connections which shares the same URL.
    When a new connection is needed, it looks first if there is an available
    connection in the pool, and returns it. If there is no connection available,
    it creates a new one. Each created connection will be returned automatically
    to the pool when it is not used anymore. }
  TConnectionPool = class
  private
    FConnections: array of IZConnection;
    FConnectionsReturnTimes: array of TDateTime;
    FSlotsInUse: TBits;
    FConnectionTimeout: Integer;
    FConnectionTimeoutThread: TConnectionTimeoutThread;
    FCriticalSection: TCriticalSection;
    FCriticalSectionDriverManager: TCriticalSection;
    FDefaultAutoCommit: Boolean;
    FDefaultTransactIsolationLevel: TZTransactIsolationLevel;
    FMaxConnections: Integer;
    FURL: string;
    FWait: Boolean;
  public
    { URL
        The connection URL
      ConnectionTimeout
        How many time a pooled connection will be kept in the pool. Zero = infinite
      MaxConnections
        The maximum numbers of connections this pool will hold. Zero = infinite
      Wait
        True - When a pool reach its maximum number of connections and someone
               tries to acquire a new one, it waits until a connection is
               returned to the pool
        False - Raises an exception instead of wait }
    constructor Create(const URL: string; const ConnectionTimeout: Integer = 0; const MaxConnections: Integer = 0; const Wait: Boolean = True);
    destructor Destroy; override;
    function Acquire: IZConnection;
    procedure ReturnToPool(const Connection: IZConnection);
  end;

  { This class embedds a real connection and redirects all methods to it.
    When it is droped or closed, it returns the real connection to the pool. }

  { TZDbcPooledConnection }

  TZDbcPooledConnection = class(TZCodePagedObject, IZConnection)
  private
    FConnection: IZConnection;
    FConnectionPool: TConnectionPool;
    FAutoEncodeStrings: Boolean;
    FUseMetadata: Boolean;
    {$IFDEF ZEOS_TEST_ONLY}
    FTestMode: Byte;
    {$ENDIF}
    function GetConnection: IZConnection;
  protected // IZConnection
    FClientCodePage: String;
    procedure RegisterStatement(const Value: IZStatement);
    procedure DeregisterStatement(const Value: IZStatement);
    procedure CheckCharEncoding(CharSet: String;
      const DoArrange: Boolean = False);
    function GetClientCodePageInformations: PZCodePage; //EgonHugeist
    function GetClientVariantManager: IZClientVariantManager;
    function GetAutoEncodeStrings: Boolean; //EgonHugeist
    procedure SetAutoEncodeStrings(const Value: Boolean);
    function CreateStatement: IZStatement;
    function PrepareStatement(const SQL: string): IZPreparedStatement;
    function PrepareCall(const SQL: string): IZCallableStatement;
    function CreateStatementWithParams(Info: TStrings): IZStatement;
    function PrepareStatementWithParams(const SQL: string; Info: TStrings): IZPreparedStatement;
    function PrepareCallWithParams(const SQL: string; Info: TStrings): IZCallableStatement;
    function CreateNotification(const Event: string): IZNotification;
    function CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence;
    function NativeSQL(const SQL: string): string;
    procedure SetAutoCommit(Value: Boolean);
    function GetAutoCommit: Boolean;
    procedure Commit;
    procedure Rollback;
    procedure PrepareTransaction(const transactionid: string);
    procedure CommitPrepared(const transactionid: string);
    procedure RollbackPrepared(const transactionid: string);
    function PingServer: Integer;
    function EscapeString(const Value : RawByteString) : RawByteString;
    procedure Open;
    procedure Close;
    function IsClosed: Boolean;
    function GetDriver: IZDriver;
    function GetIZPlainDriver: IZPlainDriver;
    function GetMetadata: IZDatabaseMetadata;
    function GetParameters: TStrings;
    function GetClientVersion: Integer;
    function GetHostVersion: Integer;
    procedure SetReadOnly(Value: Boolean);
    function IsReadOnly: Boolean;
    procedure SetCatalog(const Value: string);
    function GetCatalog: string;
    procedure SetTransactionIsolation(Value: TZTransactIsolationLevel);
    function GetTransactionIsolation: TZTransactIsolationLevel;
    function GetWarnings: EZSQLWarning;
    procedure ClearWarnings;
    function UseMetadata: boolean;
    procedure SetUseMetadata(Value: Boolean);
  public
    constructor Create(const ConnectionPool: TConnectionPool);
    destructor Destroy; override;
    function GetBinaryEscapeString(const Value: RawByteString): String; overload;
    function GetBinaryEscapeString(const Value: TBytes): String; overload;
    function GetEscapeString(const Value: ZWideString): ZWideString; overload; virtual;
    function GetEscapeString(const Value: RawByteString): RawByteString; overload; virtual;
    function GetEncoding: TZCharEncoding;
    function GetConSettings: PZConSettings;
    {$IFDEF ZEOS_TEST_ONLY}
    function GetTestMode : Byte;
    procedure SetTestMode(Mode: Byte);
    {$ENDIF}
    function GetServerProvider: TZServerProvider;	
  end;

  TZDbcPooledConnectionDriver = class(TZAbstractDriver)
  private
    PoolList: TObjectList;
    URLList: TStringList;
    function GetEmbeddedURL(const URL: String): String;
  public
    //function GetSupportedProtocols: TStringDynArray; override;
    function Connect(const URL: TZURL): IZConnection; override;
    function GetClientVersion(const URL: string): Integer; override;
    function AcceptsURL(const URL: string): Boolean; override;
    function GetPropertyInfo(const URL: string; Info: TStrings): TStrings; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;
    function GetSubVersion: Integer; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

{$ENDIF ZEOS_DISABLE_POOLED} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POOLED} //if set we have an empty unit
{ TConnectionPool }

constructor TConnectionPool.Create(const URL: string; const ConnectionTimeout: Integer = 0; const MaxConnections: Integer = 0; const Wait: Boolean = True);
begin
  FURL := URL;
  FWait := Wait;

  FSlotsInUse := TBits.Create;
  FCriticalSection := TCriticalSection.Create;
  FCriticalSectionDriverManager := TCriticalSection.Create;

  FMaxConnections := MaxConnections;
  if FMaxConnections = 0 then
  begin
    SetLength(FConnections, 10);
    SetLength(FConnectionsReturnTimes, 10);
    FSlotsInUse.Size := 10;
  end
  else
  begin
    SetLength(FConnections, FMaxConnections);
    SetLength(FConnectionsReturnTimes, FMaxConnections);
    FSlotsInUse.Size := FMaxConnections;
  end;

  //
  // If there is a connection timeout, an instance of TConnectionTimeoutThread
  // will monitor all unused connections and drop them when they timeout.
  //
  FConnectionTimeout := ConnectionTimeout;
  if FConnectionTimeout <> 0 then
    FConnectionTimeoutThread := TConnectionTimeoutThread.Create(Self);
end;

destructor TConnectionPool.Destroy;
begin
  if FConnectionTimeoutThread <> nil then
  begin
    FConnectionTimeoutThread.Terminate;
    FConnectionTimeoutThread.WaitFor;
    FConnectionTimeoutThread.Free;
  end;

  SetLength(FConnections, 0);
  FSlotsInUse.Free;
  FCriticalSection.Free;
  FCriticalSectionDriverManager.Free;

  inherited;
end;

function TConnectionPool.Acquire: IZConnection;
var
  I: Integer;
begin
  Result := nil;

  repeat
    FCriticalSection.Enter;
    try
      // Try to get an existing connection
      I := 0;
      while I < FSlotsInUse.Size do
      begin
        if (FConnections[I] <> nil) and (not FSlotsInUse[I]) then
        begin
          try
            // Test for dead connections
            FConnections[I].Rollback; // PingServer did not work (tested with FB)
            FSlotsInUse[I] := True;
            Break;
          except
            // An exception can be raised when the dead connection is dropped
            try
              FConnections[I] := nil;
            except
            end;
            Inc(I);
          end;
    end
    else
          Inc(I);
      end;

      // Try to get a free slot if there is no existing connection available
      if I = FSlotsInUse.Size then
      begin
        I := 0;
        while I < FSlotsInUse.Size do
        begin
          if (FConnections[I] = nil) and (not FSlotsInUse[I]) then
          begin
            FSlotsInUse[I] := True;
            Break;
          end;
          Inc(I);
        end;
      end;

      // Increase the pool if there is no free slot in the pool
      if I = FSlotsInUse.Size then
    begin
      if FMaxConnections = 0 then
      begin
        SetLength(FConnections, Length(FConnections) + 10);
        SetLength(FConnectionsReturnTimes, Length(FConnectionsReturnTimes) + 10);
          FSlotsInUse.Size := FSlotsInUse.Size + 10;
          FSlotsInUse[I] := True;
        end;
    end;
  finally
      FCriticalSection.Leave;
    end;

    if I < FSlotsInUse.Size then
      Break;

    // No connection available. Wait and try again later
    if FWait then
      Sleep(100)
    else
      raise Exception.Create(ClassName + '.Acquire'+LineEnding+'Connection pool reached the maximum limit');
            //2013-10-13 mse: please replace non ASCII characters (>127) by the 
            //#nnn notation in order to have encoding independent sources
  until False;

  //
  // If there is no connection in the pool, create a new one.
  // This block is separated from the block above because there could be some
  // delay to create a connection, and it would not be nice to keep the critical
  // section locked during this delay.
  //
  if FConnections[I] = nil then
  begin
    try
      // I had a strong feeling that DriverManager is not thread-safe, because i
      // had random access violations on high load operations at this point.
      // For now, i will serialize DriverManager access, until further
      // investigation (maybe the problem is in the pool driver, as
      // DriverManager should be thread-safe in essence.
      FCriticalSectionDriverManager.Enter;
      try
        FConnections[I] := DriverManager.GetConnection(FURL);
      finally
        FCriticalSectionDriverManager.Leave;
      end;
      FConnections[I].Open;
    except
      on E: Exception do
      begin
        FCriticalSection.Enter;
        try
          FSlotsInUse[I] := False;
          FConnections[I] := nil;
        finally
          FCriticalSection.Leave;
          raise Exception.Create(ClassName + '.Acquire'+LineEnding+'Error while trying to acquire a new connection'+LineEnding+LineEnding+E.Message);
        end;
      end;
    end;
    FDefaultAutoCommit := FConnections[I].GetAutoCommit;
    FDefaultTransactIsolationLevel := FConnections[I].GetTransactionIsolation;
  end;

  Result := IZConnection(FConnections[I]);
  Result.SetAutoCommit(True);
  Result.SetTransactionIsolation(tiReadCommitted);
end;

procedure TConnectionPool.ReturnToPool(const Connection: IZConnection);
var
  I: Integer;
begin
  //
  // Return the connection to the pool.
  //
  FCriticalSection.Enter;
  try
    for I := 0 to Length(FConnections) - 1 do
    begin
      if FConnections[I] = Connection then
      begin
        //
        // If there is some problem with the connection, a RollBack will raise
        // an exception, and the connection will be dropped.
        //
        try
          FSlotsInUse[I] := False;
        FConnectionsReturnTimes[I] := Now;
          FConnections[I].Rollback;
        except
          try
            FConnections[I] := nil;
          except
          end;
        end;
        Break;
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

{ TZDbcPooledConnection }

constructor TZDbcPooledConnection.Create(const ConnectionPool: TConnectionPool);
begin
  FConnectionPool := ConnectionPool;
  {$IFDEF ZEOS_TEST_ONLY}
  FTestMode := 0;
  {$ENDIF}
end;

procedure TZDbcPooledConnection.DeregisterStatement(const Value: IZStatement);
begin
  GetConnection.DeregisterStatement(Value);
end;

destructor TZDbcPooledConnection.Destroy;
begin
  if FConnection <> nil then
  begin
    FConnectionPool.ReturnToPool(FConnection);
    FConnection := nil;
  end;

  FConnectionPool := nil;

  inherited;
end;

function TZDbcPooledConnection.GetConnection: IZConnection;
begin
  if FConnection = nil then
    FConnection := FConnectionPool.Acquire;
  Result := FConnection;
end;

procedure TZDbcPooledConnection.ClearWarnings;
begin
  GetConnection.ClearWarnings;
end;

function TZDbcPooledConnection.UseMetadata: boolean;
begin
  result := FUseMetadata;
end;

procedure TZDbcPooledConnection.SetUseMetadata(Value: Boolean);
begin
  FUseMetadata := Value;
end;

procedure TZDbcPooledConnection.Close;
begin
  if FConnection <> nil then
  begin
    FConnectionPool.ReturnToPool(FConnection);
    FConnection := nil;
  end;
end;

procedure TZDbcPooledConnection.Commit;
begin
  GetConnection.Commit;
end;

procedure TZDbcPooledConnection.CommitPrepared(const transactionid: string);
begin
  GetConnection.CommitPrepared(transactionid);
end;

function TZDbcPooledConnection.CreateNotification(const Event: string): IZNotification;
begin
  Result := GetConnection.CreateNotification(Event);
end;

function TZDbcPooledConnection.CreateSequence(const Sequence: string; BlockSize: Integer): IZSequence;
begin
  Result := GetConnection.CreateSequence(Sequence, BlockSize);
end;

function TZDbcPooledConnection.CreateStatement: IZStatement;
begin
  Result := GetConnection.CreateStatement;
end;


function TZDbcPooledConnection.CreateStatementWithParams(Info: TStrings): IZStatement;
begin
  Result := GetConnection.CreateStatementWithParams(Info);
end;

function TZDbcPooledConnection.EscapeString(const Value: RawByteString): RawByteString;
begin
  Result := GetConnection.EscapeString(Value);
end;

function TZDbcPooledConnection.GetAutoCommit: Boolean;
begin
  Result := GetConnection.GetAutoCommit;  
end;

function TZDbcPooledConnection.GetCatalog: string;
begin
  Result := GetConnection.GetCatalog;
end;

function TZDbcPooledConnection.GetClientVersion: Integer;
begin
  Result := GetConnection.GetClientVersion;
end;

function TZDbcPooledConnection.GetDriver: IZDriver;
begin
  Result := GetConnection.GetDriver;
end;

function TZDbcPooledConnection.GetIZPlainDriver: IZPlainDriver;
begin
  Result := GetConnection.GetIZPlainDriver;
end;

function TZDbcPooledConnection.GetHostVersion: Integer;
begin
  Result := GetConnection.GetHostVersion;
end;

function TZDbcPooledConnection.GetMetadata: IZDatabaseMetadata;
begin
  Result := GetConnection.GetMetadata;
end;

function TZDbcPooledConnection.GetParameters: TStrings;
begin
  Result := GetConnection.GetParameters;
end;

function TZDbcPooledConnection.GetServerProvider: TZServerProvider;
begin
  Result := GetConnection.GetServerProvider;
end;

function TZDbcPooledConnection.GetTransactionIsolation: TZTransactIsolationLevel;
begin
  Result := GetConnection.GetTransactionIsolation;  
end;

function TZDbcPooledConnection.GetWarnings: EZSQLWarning;
begin
  Result := GetConnection.GetWarnings;
end;

function TZDbcPooledConnection.IsClosed: Boolean;
begin
  Result := (FConnection = nil) or FConnection.IsClosed;
end;

function TZDbcPooledConnection.IsReadOnly: Boolean;
begin
  Result := GetConnection.IsReadOnly;
end;

function TZDbcPooledConnection.NativeSQL(const SQL: string): string;
begin
  Result := GetConnection.NativeSQL(SQL);
end;

procedure TZDbcPooledConnection.Open;
begin
  GetConnection.Open;
end;

function TZDbcPooledConnection.PingServer: Integer;
begin
  Result := GetConnection.PingServer;
end;

function TZDbcPooledConnection.PrepareCall(const SQL: string): IZCallableStatement;
begin
  Result := GetConnection.PrepareCall(SQL);
end;

function TZDbcPooledConnection.PrepareCallWithParams(const SQL: string; Info: TStrings): IZCallableStatement;
begin
  Result := GetConnection.PrepareCallWithParams(SQL, Info);
end;

function TZDbcPooledConnection.PrepareStatement(const SQL: string): IZPreparedStatement;
begin
  Result := GetConnection.PrepareStatement(SQL);
end;

function TZDbcPooledConnection.PrepareStatementWithParams(const SQL: string; Info: TStrings): IZPreparedStatement;
begin
  Result := GetConnection.PrepareStatementWithParams(SQL, Info);
end;

procedure TZDbcPooledConnection.PrepareTransaction(const transactionid: string);
begin
  GetConnection.PrepareTransaction(transactionid);
end;

procedure TZDbcPooledConnection.RegisterStatement(const Value: IZStatement);
begin
  GetConnection.RegisterStatement(Value);
end;

procedure TZDbcPooledConnection.Rollback;
begin
  GetConnection.Rollback;
end;

procedure TZDbcPooledConnection.RollbackPrepared(const transactionid: string);
begin
  GetConnection.RollbackPrepared(transactionid);
end;

procedure TZDbcPooledConnection.SetAutoCommit(Value: Boolean);
begin
  GetConnection.SetAutoCommit(Value);  
end;

procedure TZDbcPooledConnection.SetCatalog(const Value: string);
begin
  GetConnection.SetCatalog(Value);
end;

procedure TZDbcPooledConnection.SetReadOnly(Value: Boolean);
begin
  GetConnection.SetReadOnly(Value);
end;

procedure TZDbcPooledConnection.SetTransactionIsolation(Value: TZTransactIsolationLevel);
begin
  GetConnection.SetTransactionIsolation(Value);
end;

{**
  EgonHugeist: Check if the given Charset for Compiler/Database-Support!!
    Not supported means if there is a pissible String-DataLoss.
    So it raises an Exception if case of settings. This handling
    is an improofment to inform Zeos-Users about the troubles the given
    CharacterSet may have.
  @param CharSet the CharacterSet which has to be proofed
  @param DoArrange represents a switch to check and set a aternative ZAlias as
    default. This means it ignores the choosen Client-CharacterSet and sets a
    "more" Zeos-Compatible Client-CharacterSet if known.
}
procedure TZDbcPooledConnection.CheckCharEncoding(CharSet: String;
  const DoArrange: Boolean = False);
begin
  Self.GetConSettings.ClientCodePage := GetIZPlainDriver.ValidateCharEncoding(CharSet, DoArrange);
  FClientCodePage := ConSettings.ClientCodePage^.Name; //resets the developer choosen ClientCodePage
end;


{**
  EgonHugeist: this is a compatibility-Option for exiting Applictions.
    Zeos is now able to preprepare direct insered SQL-Statements.
    Means do the UTF8-preparation if the CharacterSet was choosen.
    So we do not need to do the SQLString + UTF8Encode(Edit1.Test) for example.
  @result True if coAutoEncodeStrings was choosen in the TZAbstractConnection
}
function TZDbcPooledConnection.GetAutoEncodeStrings: Boolean;
begin
  Result := FAutoEncodeStrings;
end;

procedure TZDbcPooledConnection.SetAutoEncodeStrings(const Value: Boolean);
begin
  FAutoEncodeStrings := Value;
end;

{**
  EgonHugeist:
  Returns the BinaryString in a Tokenizer-detectable kind
  If the Tokenizer don't need to predetect it Result = BinaryString
  @param Value represents the Binary-String
  @param EscapeMarkSequence represents a Tokenizer detectable EscapeSequence (Len >= 3)
  @result the detectable Binary String
}
function TZDbcPooledConnection.GetBinaryEscapeString(const Value: RawByteString): String;
begin
  Result := GetConnection.GetBinaryEscapeString(Value);
end;

function TZDbcPooledConnection.GetBinaryEscapeString(const Value: TBytes): String;
begin
  Result := GetConnection.GetBinaryEscapeString(Value);
end;

function TZDbcPooledConnection.GetEscapeString(const Value: ZWideString): ZWideString;
begin
  Result := GetConnection.GetEscapeString(Value);
end;

function TZDbcPooledConnection.GetEscapeString(const Value: RawByteString): RawByteString;
begin
  Result := GetConnection.GetEscapeString(Value);
end;

function TZDbcPooledConnection.GetEncoding: TZCharEncoding;
begin
  Result := ConSettings^.ClientCodePage^.Encoding;
end;

function TZDbcPooledConnection.GetConSettings: PZConSettings;
begin
  Result := @ConSettings;
end;

{$IFDEF ZEOS_TEST_ONLY}
function TZDbcPooledConnection.GetTestMode: Byte;
begin
  Result := FTestMode;
end;

procedure TZDbcPooledConnection.SetTestMode(Mode: Byte);
begin
  FTestMode := Mode;
end;
{$ENDIF}

{**
  Result 100% Compiler-Compatible
  And sets it Result to ClientCodePage by calling the
    PlainDriver.GetClientCodePageInformations function

  @param ClientCharacterSet the CharacterSet which has to be checked
  @result PZCodePage see ZCompatible.pas
}
function TZDbcPooledConnection.GetClientCodePageInformations: PZCodePage; //EgonHugeist
begin
  Result := ConSettings^.ClientCodePage
end;

function TZDbcPooledConnection.GetClientVariantManager: IZClientVariantManager;
begin
  Result := GetConnection.GetClientVariantManager;
end;

{ TZDbcPooledConnectionDriver }

constructor TZDbcPooledConnectionDriver.Create;
begin
  inherited Create;

  PoolList := TObjectList.Create(True);
  URLList := TStringList.Create;
  AddSupportedProtocol(PooledPrefix + '*');
end;

destructor TZDbcPooledConnectionDriver.Destroy;
begin
  PoolList.Free;
  URLList.Free;

  inherited;
end;

function TZDbcPooledConnectionDriver.AcceptsURL(const URL: string): Boolean;
begin
  Result := Copy(URL, 1, 5 + Length(PooledPrefix)) = 'zdbc:' + PooledPrefix;
end;

function TZDbcPooledConnectionDriver.Connect(const URL: TZURL): IZConnection;
var
  TempURL: TZURL;
  I: Integer;
  ConnectionPool: TConnectionPool;
  ConnectionTimeout: Integer;
  MaxConnections: Integer;
  Wait: Boolean;
begin
  Result := nil;

  TempURL := TZURL.Create(GetEmbeddedURL(URL.URL), URL.Properties);
  try
    ConnectionPool := nil;

{ TODO
  - Read and process connection properties 'timeout', 'poolsize' and 'wait' }

    //
    // Search for an existing pool for the URL.
    // There is room to improve the algorithm used to decide when a pool is
    // compatible with a given URL. For now, i am just comparing the URL strings.
    //
    for I := 0 to PoolList.Count - 1 do
      if URLList[I] = TempURL.URL then
      begin
        ConnectionPool := TConnectionPool(PoolList[I]);
        Break;
      end;

    //
    // Create a new pool if needed.
    //
    if ConnectionPool = nil then
    begin
      ConnectionTimeout := StrToIntDef(TempURL.Properties.Values['ConnectionTimeout'], 0);
      MaxConnections := StrToIntDef(TempURL.Properties.Values['MaxConnections'], 0);
      Wait := StrToBoolDef(TempURL.Properties.Values['Wait'], True);
      ConnectionPool := TConnectionPool.Create(TempURL.URL, ConnectionTimeout, MaxConnections, Wait);
      PoolList.Add(ConnectionPool);
      URLList.Add(TempURL.URL);
    end;

    Result := TZDbcPooledConnection.Create(ConnectionPool);
  finally
    TempURL.Free;
  end;
end;

function TZDbcPooledConnectionDriver.GetClientVersion(const URL: string): Integer;
begin
  Result := DriverManager.GetDriver(GetEmbeddedURL(URL)).GetClientVersion(GetEmbeddedURL(URL));
end;

function TZDbcPooledConnectionDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

function TZDbcPooledConnectionDriver.GetMinorVersion: Integer;
begin
  Result := 0;
end;

function TZDbcPooledConnectionDriver.GetPropertyInfo(const URL: string; Info: TStrings): TStrings;
begin
  Result := DriverManager.GetDriver(GetEmbeddedURL(URL)).GetPropertyInfo(GetEmbeddedURL(URL), Info);
  if Result = nil then
    Result := TStringList.Create;
  Result.Values['ConnectionTimeout'] := '0';
  Result.Values['MaxConnections'] := '0';
  Result.Values['Wait'] := 'True';
end;

function TZDbcPooledConnectionDriver.GetSubVersion: Integer;
begin
  Result := 0;
end;

{function TZDbcPooledConnectionDriver.GetSupportedProtocols: TStringDynArray;
begin
  SetLength(Result, 1);
  Result[0] := PooledPrefix + '*';
end;}

function TZDbcPooledConnectionDriver.GetEmbeddedURL(const URL: String): String;
begin
  if Copy(URL, 1, 5 + Length(PooledPrefix)) = 'zdbc:' + PooledPrefix then
    Result := 'zdbc:' + Copy(URL, 5 + Length(PooledPrefix) + 1, Length(URL))
  else
    raise Exception.Create('TZDbcPooledConnectionDriver.GetRealURL - URL must start with ''zdbc:' + PooledPrefix+ '''');
end;

var
  _Driver: IZDriver;

{ TConnectionTimeoutThread }

constructor TConnectionTimeoutThread.Create(const ConnectionPool: TConnectionPool);
begin
  inherited Create(False);

  FConnectionPool := ConnectionPool;
  FreeOnTerminate := False;
end;

procedure TConnectionTimeoutThread.Execute;
var
  I: Integer;
begin
  while not Terminated  do
  begin
    Sleep(1000);

    //
    // Check if there are timed out connections and releases them
    //
    FConnectionPool.FCriticalSection.Enter;
    try
      for I := 0 to Length(FConnectionPool.FConnections) - 1 do
        if (FConnectionPool.FConnections[I] <> nil) and
           (not FConnectionPool.FSlotsInUse[I]) and
           (FConnectionPool.FConnectionsReturnTimes[I] <> 0) and
           (MilliSecondsBetween(FConnectionPool.FConnectionsReturnTimes[I], Now) {%H-}> FConnectionPool.FConnectionTimeout * 1000) then
             FConnectionPool.FConnections[I] := nil;
    finally
      FConnectionPool.FCriticalSection.Leave;
    end;
  end;
end;

initialization
  _Driver := TZDbcPooledConnectionDriver.Create;
  DriverManager.RegisterDriver(_Driver);

finalization
  DriverManager.DeregisterDriver(_Driver);

{$ENDIF ZEOS_DISABLE_POOLED} //if set we have an empty unit
end.

