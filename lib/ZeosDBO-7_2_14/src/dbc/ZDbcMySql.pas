{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           MySQL Database Connectivity Classes           }
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

unit ZDbcMySql;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(UNICODE) and not defined(WITH_UNICODEFROMLOCALECHARS)}Windows,{$IFEND}
  ZCompatibility, ZDbcIntfs, ZDbcConnection, ZPlainMySqlDriver, ZPlainDriver,
  ZURL, ZDbcLogging, ZTokenizer, ZGenericSqlAnalyser, ZPlainMySqlConstants;

type

  {** Implements MySQL Database Driver. }

  { TZMySQLDriver }
  TZMySQLDriver = class(TZAbstractDriver)
  protected
    function GetPlainDriver(const Url: TZURL; const InitDriver: Boolean = True): IZPlainDriver; override;
  public
    constructor Create; override;
    function Connect(const Url: TZURL): IZConnection; override;
    function GetMajorVersion: Integer; override;
    function GetMinorVersion: Integer; override;

    function GetTokenizer: IZTokenizer; override;
    function GetStatementAnalyser: IZStatementAnalyser; override;
    function GetClientVersion(const Url: string): Integer; override;
  end;

  {** Represents a MYSQL specific connection interface. }
  IZMySQLConnection = interface (IZConnection)
    ['{68E33DD3-4CDC-4BFC-8A28-E9F2EE94E457}']

    function GetPlainDriver: IZMySQLPlainDriver;
    function GetConnectionHandle: PMySQL;
    function EscapeString(From: PAnsiChar; Len: ULong; Quoted: Boolean): RawByteString; overload;
    function GetDatabaseName: String;
    function MySQL_FieldType_Bit_1_IsBoolean: Boolean;
    function SupportsFieldTypeBit: Boolean;
  end;

  {** Implements MySQL Database Connection. }
  TZMySQLConnection = class(TZAbstractConnection, IZMySQLConnection)
  private
    FCatalog: string;
    FHandle: PMySQL;
    FMaxLobSize: ULong;
    FDatabaseName: String;
    FIKnowMyDatabaseName, FMySQL_FieldType_Bit_1_IsBoolean,
    FSupportsBitType, FSupportsReadOnly: Boolean;
    FPlainDriver: IZMySQLPlainDriver;
    procedure InternalSetIsolationLevel(Level: TZTransactIsolationLevel);
  protected
    procedure InternalCreate; override;
  public
    function CreateRegularStatement(Info: TStrings): IZStatement; override;
    function CreatePreparedStatement(const SQL: string; Info: TStrings):
      IZPreparedStatement; override;
    function CreateCallableStatement(const SQL: string; Info: TStrings):
      IZCallableStatement; override;

    procedure Commit; override;
    procedure Rollback; override;

    function PingServer: Integer; override;
    function EscapeString(const Value: RawByteString): RawByteString; overload; override;
    function EscapeString(From: PAnsiChar; Len: ULong; Quoted: Boolean): RawByteString; overload;

    procedure Open; override;
    procedure InternalClose; override;

    procedure SetCatalog(const Catalog: string); override;
    function GetCatalog: string; override;

    procedure SetTransactionIsolation(Level: TZTransactIsolationLevel); override;
    procedure SetAutoCommit(Value: Boolean); override;
    procedure SetReadOnly(Value: Boolean); override;
    {ADDED by fduenas 15-06-2006}
    function GetClientVersion: Integer; override;
    function GetHostVersion: Integer; override;
    {END ADDED by fduenas 15-06-2006}
    function GetPlainDriver: IZMySQLPlainDriver;
    function GetConnectionHandle: PMySQL;
    function GetEscapeString(const Value: ZWideString): ZWideString; override;
    function GetEscapeString(const Value: RawByteString): RawByteString; override;
    function GetDatabaseName: String;
	  function GetServerProvider: TZServerProvider; override;
    function MySQL_FieldType_Bit_1_IsBoolean: Boolean;
    function SupportsFieldTypeBit: Boolean;
  end;

var
  {** The common driver manager object. }
  MySQLDriver: IZDriver;

{$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_MYSQL} //if set we have an empty unit

uses
  ZMessages, ZSysUtils, ZDbcMySqlStatement, ZMySqlToken, ZFastCode,
  ZDbcMySqlUtils, ZDbcMySqlMetadata, ZMySqlAnalyser, TypInfo, Math,
  {$IFDEF FPC}syncobjs{$ELSE}SyncObjs{$ENDIF},
  ZEncoding, ZClasses;

{ TZMySQLDriver }

{**
  Constructs this object with default properties.
}
constructor TZMySQLDriver.Create;
begin
  inherited Create;
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQL5PlainDriver.Create, 'mysql'));
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQL41PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQL5PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQLD41PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZMySQLD5PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZMariaDB5PlainDriver.Create));
  AddSupportedProtocol(AddPlainDriverToCache(TZMariaDB10PlainDriver.Create));
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
function TZMySQLDriver.Connect(const Url: TZURL): IZConnection;
begin
  Result := TZMySQLConnection.Create(Url);
end;

{**
  Gets the driver's major version number. Initially this should be 1.
  @return this driver's major version number
}
function TZMySQLDriver.GetMajorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets the driver's minor version number. Initially this should be 0.
  @return this driver's minor version number
}
function TZMySQLDriver.GetMinorVersion: Integer;
begin
  Result := 1;
end;

{**
  Gets a SQL syntax tokenizer.
  @returns a SQL syntax tokenizer object.
}
function TZMySQLDriver.GetTokenizer: IZTokenizer;
begin
  Result := TZMySQLTokenizer.Create;
end;

{**
  Creates a statement analyser object.
  @returns a statement analyser object.
}
function TZMySQLDriver.GetStatementAnalyser: IZStatementAnalyser;
begin
  Result := TZMySQLStatementAnalyser.Create; { thread save! Allways return a new Analyser! }
end;

{**
  Gets plain driver for selected protocol.
  @param Url a database connection URL.
  @return a selected plaindriver.
}
function TZMySQLDriver.GetPlainDriver(const Url: TZURL;
  const InitDriver: Boolean = True): IZPlainDriver;
begin
  // added by tohenk, 2009-10-11
  // before PlainDriver is initialized, we can perform pre-library loading
  // requirement check here, e.g. Embedded server argument params
  Result := inherited GetPlainDriver(URL, False);
  if Assigned(Result) then
  begin
    if Url.Properties.Count >0  then
      (Result as IZMySQLPlainDriver).SetDriverOptions(Url.Properties);
    // end added by tohenk, 2009-10-11
    if InitDriver then Result.Initialize(Url.LibLocation);
  end
  else
    raise Exception.Create('Can''t receive Plaindriver!');
end;

{**
  Returns the version of the plain driver library that will be used to open a connection
  to the given URL.
  @param url the URL of the database
  @return the version number of the plain driver library for the give URL
}
function TZMySQLDriver.GetClientVersion(const Url: string): Integer;
var
  TempURL: TZURL;
begin
  TempURL := TZURL.Create(Url);
  Result := ConvertMySQLVersionToSQLVersion((GetPlainDriver(TempUrl) as IZMySQLPlainDriver).GetClientVersion);
  TempUrl.Free
end;

{ TZMySQLConnection }

{**
  Constructs this object and assignes the main properties.
}
procedure TZMySQLConnection.InternalCreate;
begin
  FIKnowMyDatabaseName := False;
  if Self.Port = 0 then
     Self.Port := MYSQL_PORT;
  inherited SetTransactionIsolation(tiRepeatableRead);
  FMetaData := TZMySQLDatabaseMetadata.Create(Self, Url);
end;

const
  MySQLSessionTransactionIsolation: array[TZTransactIsolationLevel] of
    {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = (
    'SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ',
    'SET SESSION TRANSACTION ISOLATION LEVEL READ UNCOMMITTED',
    'SET SESSION TRANSACTION ISOLATION LEVEL READ COMMITTED',
    'SET SESSION TRANSACTION ISOLATION LEVEL REPEATABLE READ',
    'SET SESSION TRANSACTION ISOLATION LEVEL SERIALIZABLE');
  MySQLSessionTransactionReadOnly: array[Boolean] of
    {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF} = (
    'SET SESSION TRANSACTION READ ONLY',
    'SET SESSION TRANSACTION READ WRITE');
procedure TZMySQLConnection.InternalSetIsolationLevel(
  Level: TZTransactIsolationLevel);
begin
  if GetPlainDriver.ExecRealQuery(FHandle,
    Pointer(MySQLSessionTransactionIsolation[Level]), Length(MySQLSessionTransactionIsolation[Level])) <> 0 then
      CheckMySQLError(GetPlainDriver, FHandle, lcExecute, MySQLSessionTransactionIsolation[Level], ConSettings);
  if DriverManager.HasLoggingListener then
    DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, MySQLSessionTransactionIsolation[Level]);
end;

function TZMySQLConnection.MySQL_FieldType_Bit_1_IsBoolean: Boolean;
begin
  Result := FMySQL_FieldType_Bit_1_IsBoolean;
end;

function TZMySQLConnection.EscapeString(From: PAnsiChar;
  Len: ULong; Quoted: Boolean): RawByteString;
var
  Buf: array[0..2048] of AnsiChar;
  P: PAnsichar;
begin
  if ((Len+Byte(Ord(Quoted))) shl 1) > (SizeOf(Buf)-1) then begin
    SetLength(Result, (Len+Byte(Ord(Quoted))) shl 1);
    SetLength(Result, GetPlainDriver.EscapeString(FHandle, PAnsiChar(Pointer(Result))+Ord(Quoted), From, Len)+(Byte(Ord(Quoted)) shl 1));
  end else
    ZSetString(@Buf[0], GetPlainDriver.EscapeString(FHandle, @Buf[0+Ord(Quoted)], From, Len)+(Byte(Ord(Quoted) shl 1)), Result);
  if Quoted then begin
    P := Pointer(Result);
    P^ := #39;
    (P+Length(Result)-1)^ := #39;
  end;
end;

{**
  Opens a connection to database server with specified parameters.
}
procedure TZMySQLConnection.Open;
var
  LogMessage: RawByteString;
  UIntOpt: UInt;
  MyBoolOpt: Byte;
  ClientFlag : Cardinal;
  SslCa, SslCaPath, SslKey, SslCert, SslCypher: PAnsiChar;
  myopt: TMySQLOption;
  sMyOpt: string;
  my_client_Opt:TMYSQL_CLIENT_OPTIONS;
  sMy_client_Opt, sMy_client_Char_Set:String;
  ClientVersion: Integer;
  SQL: RawByteString;
label setuint;
begin
  if not Closed then
    Exit;

  LogMessage := 'CONNECT TO "'+ConSettings^.Database+'" AS USER "'+ConSettings^.User+'"';
  GlobalCriticalSection.Enter;
  try
    FHandle := GetPlainDriver.Init(FHandle); //is not threadsave!
  finally
    GlobalCriticalSection.Leave;
  end;
  {EgonHugeist: get current characterset first }
  sMy_client_Char_Set := {$IFDEF UNICODE}ASCII7ToUnicodeString{$ENDIF}(GetPlainDriver.character_set_name(FHandle));
  if (sMy_client_Char_Set <> '') {mysql 4down doesn't have this function } and
     (sMy_client_Char_Set <> FClientCodePage) then begin
    ConSettings^.ClientCodePage := GetPlainDriver.ValidateCharEncoding(sMy_client_Char_Set);
    ZEncoding.SetConvertFunctions(ConSettings);
  end;
  try
    { Sets a default port number. }
    if Port = 0 then
       Port := MYSQL_PORT;

    { Turn on compression protocol. }
    if StrToBoolEx(Info.Values['compress']) and
      (Info.Values['MYSQL_OPT_COMPRESS'] = '') and
       (Info.IndexOf('MYSQL_OPT_COMPRESS') = -1) then
      Info.Values['MYSQL_OPT_COMPRESS'] := Info.Values['compress']; //check if user allready did set the value!
    { Sets connection timeout. }
    if (StrToIntDef(Info.Values['timeout'], 0) >= 0) and
       (Info.Values['MYSQL_OPT_CONNECT_TIMEOUT'] = '') then //check if user allready did set the value!
      Info.Values['MYSQL_OPT_CONNECT_TIMEOUT'] := Info.Values['timeout'];

   (*Added lines to handle option parameters 21 november 2007 marco cotroneo*)
    ClientVersion := GetPlainDriver.GetClientVersion;
    for myopt := low(TMySQLOption) to high(TMySQLOption) do
    begin
      sMyOpt:= GetEnumName(typeInfo(TMySQLOption), integer(myOpt));
      if ClientVersion >= TMySqlOptionMinimumVersion[myopt] then //version checked (:
        case myopt of
          {unsigned int options ...}
          MYSQL_OPT_CONNECT_TIMEOUT,
          MYSQL_OPT_PROTOCOL,
          MYSQL_OPT_READ_TIMEOUT,
          MYSQL_OPT_WRITE_TIMEOUT,
          MYSQL_OPT_MAX_ALLOWED_PACKET,
          MYSQL_OPT_NET_BUFFER_LENGTH,
          MYSQL_OPT_SSL_MODE,
          MYSQL_OPT_RETRY_COUNT,
          MYSQL_OPT_SSL_FIPS_MODE,
          MYSQL_OPT_ZSTD_COMPRESSION_LEVEL:
            if Info.Values[sMyOpt] <> '' then begin
setuint:      UIntOpt := StrToIntDef(Info.Values[sMyOpt], 0);
              GetPlainDriver.SetOptions(FHandle, myopt, @UIntOpt);
            end;
          MYSQL_OPT_LOCAL_INFILE: {optional empty or unsigned int}
            if Info.Values[sMyOpt] <> '' then
              goto setuint
            else
              if Info.IndexOf(sMyOpt) > -1 then
                GetPlainDriver.SetOptions(FHandle, myopt, nil);
          { no value options }
          MYSQL_OPT_COMPRESS,
          MYSQL_OPT_GUESS_CONNECTION,
          MYSQL_OPT_NAMED_PIPE,
          MYSQL_OPT_USE_REMOTE_CONNECTION,
          MYSQL_OPT_USE_EMBEDDED_CONNECTION,
          MYSQL_OPT_USE_RESULT,
          MYSQL_OPT_CONNECT_ATTR_RESET:
            if (Info.Values[sMyOpt] <> '') or (Info.IndexOf(sMyOpt) > -1) then
              GetPlainDriver.SetOptions(FHandle, myopt, nil);
          { my_bool * options}
          MYSQL_REPORT_DATA_TRUNCATION,
          MYSQL_SECURE_AUTH,
          MYSQL_OPT_RECONNECT,
          MYSQL_OPT_SSL_VERIFY_SERVER_CERT,
          MYSQL_ENABLE_CLEARTEXT_PLUGIN,
          MYSQL_OPT_CAN_HANDLE_EXPIRED_PASSWORDS,
          MYSQL_OPT_SSL_ENFORCE,
          MYSQL_OPT_GET_SERVER_PUBLIC_KEY,
          MYSQL_OPT_OPTIONAL_RESULTSET_METADATA:
            if Info.Values[sMyOpt] <> '' then begin
              MyBoolOpt := Ord(StrToBoolEx(Info.Values[sMyOpt]));
              GetPlainDriver.SetOptions(FHandle, myopt, @MyBoolOpt);
            end;
          { unsigned char * options }
          MYSQL_OPT_SSL_KEY, MYSQL_OPT_SSL_CERT,
          MYSQL_OPT_SSL_CA, MYSQL_OPT_SSL_CAPATH, MYSQL_OPT_SSL_CIPHER,
          MYSQL_OPT_TLS_CIPHERSUITES,
          MYSQL_OPT_COMPRESSION_ALGORITHMS: ;//skip, processed down below
          else
            if Info.Values[sMyOpt] <> '' then
              GetPlainDriver.SetOptions(FHandle, myopt, PAnsiChar(
                ConSettings^.ConvFuncs.ZStringToRaw(Info.Values[sMyOpt],
                  ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)));
        end;
    end;

    { Set ClientFlag }
    ClientFlag := 0;
    if Not StrToBoolEx(Info.Values['dbless'])
       then ClientFlag := {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}trunc(power(2, GetEnumValue(TypeInfo(TMYSQL_CLIENT_OPTIONS),'_CLIENT_CONNECT_WITH_DB')));

    for my_client_Opt := low(TMYSQL_CLIENT_OPTIONS) to high(TMYSQL_CLIENT_OPTIONS) do
    begin
      sMy_client_Opt:= GetEnumName(typeInfo(TMYSQL_CLIENT_OPTIONS), integer(my_client_Opt));
      if StrToBoolEx(Info.Values[sMy_client_Opt]) then
          ClientFlag:= ClientFlag or {$IFDEF USE_FAST_TRUNC}ZFastCode.{$ENDIF}trunc(power(2, GetEnumValue(TypeInfo(TMYSQL_CLIENT_OPTIONS),sMy_client_Opt)));
    end;

  { Set SSL properties before connect}
  SslKey := nil; SslCert := nil; SslCa := nil; SslCaPath := nil; SslCypher := nil;
  if StrToBoolEx(Info.Values['MYSQL_SSL']) then
    begin
       if Info.Values['MYSQL_SSL_KEY'] <> '' then
          SslKey := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values['MYSQL_SSL_KEY'], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       if Info.Values['MYSQL_SSL_CERT'] <> '' then
          SslCert := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values['MYSQL_SSL_CERT'], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       if Info.Values['MYSQL_SSL_CA'] <> '' then
          SslCa := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values['MYSQL_SSL_CA'], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       if Info.Values['MYSQL_SSL_CAPATH'] <> '' then
          SslCaPath := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values['MYSQL_SSL_CAPATH'], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       if Info.Values['MYSQL_SSL_CYPHER'] <> '' then
          SslCypher := PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(
            Info.Values['MYSQL_SSL_CYPHER'], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
       GetPlainDriver.SslSet(FHandle, SslKey, SslCert, SslCa, SslCaPath,
          SslCypher);
       DriverManager.LogMessage(lcConnect, ConSettings^.Protocol,
          'SSL options set');
    end;

    { Connect to MySQL database. }
    {$IFDEF UNICODE}
    if GetPlainDriver.RealConnect(FHandle, PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(HostName, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)),
                              PAnsiChar(ConSettings^.User), PAnsiChar(ConSettings^.ConvFuncs.ZStringToRaw(Password, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)),
    {$ELSE}
    if GetPlainDriver.RealConnect(FHandle, PAnsiChar(HostName),
                              PAnsiChar(ConSettings^.User), PAnsiChar(Password),
    {$ENDIF}
                              PAnsiChar(ConSettings^.Database), Port, nil,
                              ClientFlag) = nil then begin
      CheckMySQLError(GetPlainDriver, FHandle, lcConnect, LogMessage, ConSettings);
      DriverManager.LogError(lcConnect, ConSettings^.Protocol, LogMessage,
        0, ConSettings.ConvFuncs.ZStringToRaw(SUnknownError,
          ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
      raise EZSQLException.Create(SCanNotConnectToServer);
    end;
    DriverManager.LogMessage(lcConnect, ConSettings^.Protocol, LogMessage);

    { Fix Bugs in certain Versions where real_conncet resets the Reconnect flag }
    if (Info.Values['MYSQL_OPT_RECONNECT'] <> '') and
      ((ClientVersion>=50013) and (ClientVersion<50019)) or
      ((ClientVersion>=50100) and (ClientVersion<50106)) then begin
      MyBoolOpt := Ord(StrToBoolEx(Info.Values['MYSQL_OPT_RECONNECT']));
      GetPlainDriver.SetOptions(FHandle, MYSQL_OPT_RECONNECT, @MyBoolOpt);
    end;
    if (FClientCodePage = '') and (sMy_client_Char_Set <> '') then
      FClientCodePage := sMy_client_Char_Set;

    //EH: MariaDB needs a explizit set of charset to be synced on Client<>Server!
    if (FClientCodePage <> sMy_client_Char_Set) or (FPlainDriver.IsMariaDBDriver and (FClientCodePage <> '')) then begin
      //http://dev.mysql.com/doc/refman/5.7/en/mysql-set-character-set.html
      //take care mysql_real_escape_string works like expected!
      SQL := {$IFDEF UNICODE}UnicodeStringToAscii7{$ENDIF}(FClientCodePage);
      if GetPlainDriver.set_character_set(FHandle, Pointer(SQL)) <> 0 then begin //failed? might be possible the function does not exists
        SQL := 'SET NAMES '+SQL;
        GetPlainDriver.ExecRealQuery(FHandle, Pointer(SQL), Length(SQL));
        CheckMySQLError(GetPlainDriver, FHandle, lcExecute, SQL, ConSettings);
        DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
      end;
      CheckCharEncoding(FClientCodePage);
    end;

    FMaxLobSize := {$IFDEF UNICODE}UnicodeToIntDef{$ELSE}RawToIntDef{$ENDIF}(Info.Values['MaxLobSize'], 0);
    if FMaxLobSize <> 0 then begin
      SQL := 'SET GLOBAL max_allowed_packet='+IntToRaw(FMaxLobSize);
      GetPlainDriver.ExecRealQuery(FHandle, Pointer(SQL), Length(SQL));
      CheckMySQLError(GetPlainDriver, FHandle, lcExecute, SQL, ConSettings);
      DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, SQL);
    end else
      FMaxLobSize := MaxBlobSize;


    { Sets transaction isolation level. }
    if not (TransactIsolationLevel in [tiNone,tiRepeatableRead]) then
      InternalSetIsolationLevel(TransactIsolationLevel);

    { Sets an auto commit mode. }
    if not GetAutoCommit then begin
      GetPlaindriver.SetAutocommit(FHandle, GetAutoCommit);
      CheckMySQLError(GetPlainDriver, FHandle, lcExecute, 'Native SetAutoCommit '+BoolToRawEx(AutoCommit)+'call', ConSettings);
    end;

    inherited Open;
    //no real version check required -> the user can simply switch off treading
    //enum('Y','N')
    FMySQL_FieldType_Bit_1_IsBoolean := StrToBoolEx(Info.Values['MySQL_FieldType_Bit_1_IsBoolean']);
    (GetMetadata as IZMySQLDatabaseMetadata).SetMySQL_FieldType_Bit_1_IsBoolean(FMySQL_FieldType_Bit_1_IsBoolean);
    FSupportsBitType := (
      (    FPlainDriver.IsMariaDBDriver and (ClientVersion >= 100109) ) or
      (not FPlainDriver.IsMariaDBDriver and (ClientVersion >=  50003) ) ) and (GetHostVersion >= EncodeSQLVersioning(5,0,3));
    //if not explizit !un!set -> assume as default since Zeos 7.3
    with (GetMetadata as IZMySQLDatabaseMetadata) do begin
      SetMySQL_FieldType_Bit_1_IsBoolean(FMySQL_FieldType_Bit_1_IsBoolean);
      FSupportsReadOnly := ( IsMariaDB and (GetHostVersion >= EncodeSQLVersioning(10,0,0))) or
                           ( IsMySQL and (GetHostVersion >= EncodeSQLVersioning( 5,6,0)));
      SetDataBaseName(GetDatabaseName);
    end;
    if FSupportsReadOnly and ReadOnly then begin
      ReadOnly := False;
      SetReadOnly(True);
    end;

  except
    GetPlainDriver.Close(FHandle);
    FHandle := nil;
    raise;
  end;

  if FClientCodePage = '' then begin //workaround for MySQL 4 down
    with CreateStatement.ExecuteQuery('show variables like "character_set_database"') do begin
      if Next then
        FClientCodePage := GetString(FirstDbcIndex+1);
      Close;
    end;
    ConSettings^.ClientCodePage := GetPlainDriver.ValidateCharEncoding(FClientCodePage);
    ZEncoding.SetConvertFunctions(ConSettings);
  end;
end;

{**
  Ping Current Connection's server, if client was disconnected,
  the connection is resumed.
  @return 0 if succesfull or error code if any error occurs
}
function TZMySQLConnection.PingServer: Integer;
const
   PING_ERROR_ZEOSCONNCLOSED = -1;
var
   Closing: boolean;
begin
   Closing := FHandle = nil;
   if Closed or Closing then
      Result := PING_ERROR_ZEOSCONNCLOSED
   else
      Result := GetPlainDriver.Ping(FHandle);
end;

{**
  Escape a string so it's acceptable for the Connection's server.
  @param value string that should be escaped
  @return Escaped string
}
function TZMySQLConnection.EscapeString(const Value: RawByteString): RawByteString;
begin
  Result := EscapeString(Pointer(Value), Length(Value), True);
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
function TZMySQLConnection.CreateRegularStatement(Info: TStrings):
  IZStatement;
begin
  if IsClosed then
     Open;
  Result := TZMySQLStatement.Create(GetPlainDriver, Self, Info, FHandle);
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
function TZMySQLConnection.CreatePreparedStatement(const SQL: string;
  Info: TStrings): IZPreparedStatement;
begin
  if IsClosed then
     Open;
  if Assigned(Info) then
    if StrToBoolEx(Info.Values['preferprepared']) then
      Result := TZMySQLPreparedStatement.Create(GetPlainDriver, Self, SQL, Info)
    else
      Result := TZMySQLEmulatedPreparedStatement.Create(GetPlainDriver, Self, SQL, Info, FHandle)
  else
    Result := TZMySQLEmulatedPreparedStatement.Create(GetPlainDriver, Self, SQL, Info, FHandle);
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
function TZMySQLConnection.CreateCallableStatement(const SQL: string; Info: TStrings):
  IZCallableStatement;
begin
  Result := TZMySQLCallableStatement.Create(GetPlainDriver, Self, SQL, Info, FHandle);
end;

{**
  Makes all changes made since the previous
  commit/rollback permanent and releases any database locks
  currently held by the Connection. This method should be
  used only when auto-commit mode has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.Commit;
begin
  if GetAutoCommit then
    raise Exception.Create(SInvalidOpInAutoCommit);

  if not Closed then begin
    If not GetPlaindriver.Commit(FHandle) then
      CheckMySQLError(GetPlainDriver, FHandle, lcExecute, 'Native Commit call', ConSettings);
    DriverManager.LogMessage(lcExecute, ConSettings.Protocol, 'Native Commit call');
  end;
end;

{**
  Drops all changes made since the previous
  commit/rollback and releases any database locks currently held
  by this Connection. This method should be used only when auto-
  commit has been disabled.
  @see #setAutoCommit
}
procedure TZMySQLConnection.Rollback;
begin
  if GetAutoCommit then
    raise Exception.Create(SInvalidOpInAutoCommit);

  if not Closed then begin
    If not GetPlaindriver.Rollback(FHandle) then
      CheckMySQLError(GetPlainDriver, FHandle, lcExecute, 'Native Rollback call', ConSettings);
    DriverManager.LogMessage(lcExecute, ConSettings.Protocol, 'Native Rollback call');
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
procedure TZMySQLConnection.InternalClose;
var
  LogMessage: RawByteString;
begin
  if ( Closed ) or (not Assigned(PlainDriver)) then
    Exit;
  GetPlainDriver.Close(FHandle);
  FHandle := nil;
  LogMessage := 'DISCONNECT FROM "'+ConSettings^.Database+'"';
  DriverManager.LogMessage(lcDisconnect, ConSettings^.Protocol, LogMessage);
end;

{**
  Gets a selected catalog name.
  @return a selected catalog name.
}
function TZMySQLConnection.GetCatalog: string;
begin
  Result := FCatalog;
end;

{**
  Sets a new selected catalog name.
  @param Catalog a selected catalog name.
}
procedure TZMySQLConnection.SetCatalog(const Catalog: string);
begin
  FCatalog := Catalog;
end;

{**
  Puts this connection in read-only mode as a hint to enable
  database optimizations.

  <P><B>Note:</B> This method cannot be called while in the
  middle of a transaction.

  @param readOnly true enables read-only mode; false disables
    read-only mode.
}
procedure TZMySQLConnection.SetReadOnly(Value: Boolean);
begin
  if Value <> ReadOnly then begin
    if not Closed then begin
      if not FSupportsReadOnly then
        raise EZSQLException.Create(SUnsupportedOperation);
      if GetPlainDriver.ExecRealQuery(FHandle,
        Pointer(MySQLSessionTransactionReadOnly[Value]), Length(MySQLSessionTransactionReadOnly[Value])) <> 0 then
          CheckMySQLError(GetPlainDriver, FHandle, lcTransaction, MySQLSessionTransactionReadOnly[Value], ConSettings);
      if DriverManager.HasLoggingListener then
        DriverManager.LogMessage(lcTransaction, ConSettings^.Protocol, MySQLSessionTransactionReadOnly[Value]);
    end;
    ReadOnly := Value;
  end;
end;

{**
  Sets a new transact isolation level.
  @param Level a new transact isolation level.
}
procedure TZMySQLConnection.SetTransactionIsolation(
  Level: TZTransactIsolationLevel);
begin
  if TransactIsolationLevel <> Level then begin
    inherited SetTransactionIsolation(Level);
    if not Closed then
      InternalSetIsolationLevel(Level);
  end;
end;

function TZMySQLConnection.SupportsFieldTypeBit: Boolean;
begin
  if Closed then
    Open;
  Result := FSupportsBitType;
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
procedure TZMySQLConnection.SetAutoCommit(Value: Boolean);
begin
  if GetAutoCommit <> Value then begin
    inherited SetAutoCommit(Value);
    if not Closed then begin
      if not GetPlaindriver.SetAutocommit(FHandle, Value) then
        CheckMySQLError(GetPlainDriver, FHandle, lcExecute, 'Native SetAutoCommit '+BoolToRawEx(AutoCommit)+'call', ConSettings);
      DriverManager.LogMessage(lcExecute, ConSettings^.Protocol, 'Native SetAutoCommit '+BoolToRawEx(AutoCommit)+'call');
    end;
  end;
end;

{**
  Gets client's full version number.
  The format of the version returned must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZMySQLConnection.GetClientVersion: Integer;
begin
 Result := ConvertMySQLVersionToSQLVersion( GetPlainDriver.GetClientVersion );
end;

{**
  Gets server's full version number.
  The format of the returned version must be XYYYZZZ where
   X   = Major version
   YYY = Minor version
   ZZZ = Sub version
  @return this clients's full version number
}
function TZMySQLConnection.GetHostVersion: Integer;
begin
 Result := ConvertMySQLVersionToSQLVersion( GetPlainDriver.GetServerVersion(FHandle) );
 CheckMySQLError(GetPlainDriver, FHandle, lcExecute, 'mysql_get_server_version()', ConSettings);
end;

{**
  Gets a reference to MySQL connection handle.
  @return a reference to MySQL connection handle.
}
function TZMySQLConnection.GetConnectionHandle: PMySQL;
begin
  Result := FHandle;
end;

function TZMySQLConnection.GetServerProvider: TZServerProvider;
begin
  Result := spMySQL;
end;

{**
  Gets a MySQL plain driver interface.
  @return a MySQL plain driver interface.
}
function TZMySQLConnection.GetPlainDriver: IZMySQLPlainDriver;
begin
  if FPlainDriver = nil then
    fPlainDriver := PLainDriver as IZMySQLPlainDriver;
  Result := fPlainDriver;
end;

{**
  EgonHugeist:
  Returns the BinaryString in a Tokenizer-detectable kind
  If the Tokenizer don't need to predetect it Result := BinaryString
  @param Value represents the Binary-String
  @param EscapeMarkSequence represents a Tokenizer detectable EscapeSequence (Len >= 3)
  @result the detectable Binary String
}
function TZMySQLConnection.GetEscapeString(const Value: ZWideString): ZWideString;
var tmp: RawByteString;
begin
  tmp := GetEscapeString(PUnicodeToRaw(Pointer(Value), Length(Value), ConSettings^.ClientCodePage^.CP));
  Result := PRawToUnicode(Pointer(tmp), Length(tmp), ConSettings^.ClientCodePage^.CP);
end;

function TZMySQLConnection.GetEscapeString(const Value: RawByteString): RawByteString;
begin
  Result := inherited GetEscapeString(EscapeString(Pointer(Value), Length(Value), True));
end;

function TZMySQLConnection.GetDatabaseName: String;
var
  ResultSet: IZResultSet;
begin
  if not FIKnowMyDatabaseName then begin
    ResultSet := CreateStatement.ExecuteQuery('select database() as ''DATABASE''');
    if ResultSet.Next
    then FDatabaseName := ResultSet.GetStringByName('DATABASE');
    FIKnowMyDatabaseName := True;
    ResultSet.Close;
    ResultSet := nil;
  end;
  Result := FDatabaseName;
end;

initialization
  MySQLDriver := TZMySQLDriver.Create;
  DriverManager.RegisterDriver(MySQLDriver);
finalization
  if DriverManager <> nil then
    DriverManager.DeregisterDriver(MySQLDriver);
  MySQLDriver := nil;

  {$ENDIF ZEOS_DISABLE_MYSQL} //if set we have an empty unit
end.
