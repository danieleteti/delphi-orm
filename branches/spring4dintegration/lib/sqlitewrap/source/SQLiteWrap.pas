{ : @abstract(SQLite 3.x object based wrapper)

  Can be used for object based access to SQLite3 databases.

  Note: Requires Sqlite 3.6.8 and higher!

  Designed for Delphi 6+ and Freepascal, Unicode support for Delphi 2009+

  V2.1.0
  Added BLOB parameters and improved BLOB handling.
  Added TotalChanges property.
  Some bugfixes.

  V2.0.0  29 June 2010
  Ported to D2009 Unicode by Roger Lascelles (support@veecad.com)

  V1.0.0
  by Lukáš Gebauer at http://www.ararat.cz/doku.php/en:sqlitewrap.
  based on work by Tim Anderson (tim@itwriting.com)

  UNICODE HANDLING:
  Delphi 2009+
  Pass data as native UnicodeString. Datas are converted to SQLite native
  UTF-8 internally.

  pre-Delphi 2009
  Pass data as UTF8String (it is AnsiString contains UTF8 datas) and you must
  convert data to UTF-8 explicitly!
  Pasing data by UTF8String typed variable made your source forward compatible
  with Delphi 2009+.


  Sample usage:
  @longcode(#
  procedure sample;
  var
  database: TSqliteDatabase;
  tab: TSqliteTable;
  s: utf8string;
  begin
  database := TSqliteDatabase.Create('somedatabase.db3');
  try
  database.AddParamInt(':key', 123456);
  tab := database.GetTable('SELECT * FROM some_table WHERE ROWID=:key');
  try
  while not tab.EOF do
  begin
  s := tab.FieldAsString(tab.FieldIndex['ROWID']);
  //do something with 'S' variable...
  //...
  //...then go to nexr row.
  tab.next;
  end;
  finally
  tab.free;
  end;
  finally
  database.free;
  end;
  end;
  #)
}

unit SQLiteWrap;

interface

{$IFDEF FPC}
{$MODE Delphi}{$H+}
{$ENDIF}

{$IFDEF UNICODE}
{$IFNDEF FPC}
{$DEFINE SQUNI}
{$ENDIF}
{$ENDIF}


uses
{$IFDEF WIN32}
    Windows,
{$ENDIF}
  SQLite3,
  Classes,
  SysUtils;

type
  { : @abstract(Exception Class for SQLite based errors) }
  ESQLiteException = class(Exception)
  end;

  { : @abstract(Class for storing deferred parameters)
    Do not use it, it is used internally only!
  }
  TSQliteParam = class
  public
    name: UTF8String;
    valuetype: integer;
    valueinteger: int64;
    valuefloat: double;
    valuedata: UTF8String;
  end;

  { : @abstract(procedural prototype for @link(OnQuery) hook.) }
  THookQuery = procedure(Sender: TObject; const SQL: String) of object;

  TSQLiteTable = class;

  { : @abstract(Class for handling SQLite database) }
  TSQLiteDatabase = class(TObject)
  private
    fDB: TSQLiteDB;
    fParams: TList;
    FOnQuery: THookQuery;
    procedure RaiseError(const s, SQL: String);
    procedure SetParams(const Stmt: TSQLiteStmt);
  protected
    procedure DoQuery(const value: String);
  public
    { : Class constructor. Pass filename of database. If databas not exists, then new one is created.
      If you pass empty string as filename, then temporary database is created.
      If you pass ':memory:' as filename, then temporary database in memory is created.

      Warning:
      Pass FileName as AnsiString (if you are not using Unicode Delphi version)
      or as UnicodeString (on Delphi 2009 and higher)!
    }
    constructor Create(const FileName: String);
    { : Class descructor. Call Free instead. }
    destructor Destroy; override;
    { : Run SQL command without result.
      You can call before functions AddParam* for set query parameters. }
    procedure ExecSQL(const SQL: String);
    { : Run SQL command with result.
      You can call before functions AddParam* for set query parameters.
      If you set PerpareOnly, then query is just prepared but first row is not fetched! }
    function GetTable(const SQL: String; PrepareOnly: Boolean = false): TSQLiteTable;
    { : Run SQL command and number from first field in first row is returned.
      You can call before functions AddParam* for set query parameters. }
    function GetTableValue(const SQL: String): int64;
    { : Run SQL command and value from first field in first row is returned.
      You can call before functions AddParam* for set query parameters. }
    function GetTableString(const SQL: String): String;
    { : Run SQL command and values from first field in each row is filled to stringlist.
      You can call before functions AddParam* for set query parameters. }
    procedure GetTableStrings(const SQL: String; const value: TStrings);
    { : Return @True if database is in transaction state. }
    function InTransaction: Boolean;
    { : Start transaction. You can modify transaction type by Param parameter.
      If you use non-empty Name parameter, then savepoint is used. Savepoint is named and can be nested. }
    procedure Start(const name: String; const param: String = '');
    { : Commit transaction.
      If you use non-empty Name parameter, then savepoint is used. Savepoint is named and can be nested. }
    procedure Commit(const name: String);
    { : Rollback transaction.
      If you use non-empty Name parameter, then savepoint is used. Savepoint is named and can be nested. }
    procedure Rollback(const name: String);
    { : Get ROWID of last inserted row. }
    function LastInsertRowID: int64;
    { : Return number of modified rows by last query. }
    function LastChangedRows: int64;
    { : Return number of modified rows starting by opened database connection. }
    function TotalChanges: int64;
    { : Set wait timeout. if database is locked, then it wait this timeout.
      If database is not released within this timeout, then error is returned. }
    procedure SetTimeout(value: integer);
    { : Return SQLite engine version. }
    function Version: String;
    { : Add custom sorting procedure as new Collate. }
    procedure AddCustomCollate(name: String; xCompare: TCollateXCompare);
    { : Add collate named SYSTEM for correct data sorting by user's locale }
    Procedure AddSystemCollate;
    { : Clear all query parameters. }
    procedure ParamsClear;
    { : Add named query parameter of integer type. }
    procedure AddParamInt(const name: String; value: int64);
    { : Add named query parameter of floating-point type. }
    procedure AddParamFloat(const name: String; value: double);
    { : Add named query parameter of string or binary type. }
    procedure AddParamText(const name: String; const value: String);
    { : Add named query parameter with null value. }
    procedure AddParamNull(const name: String);
    { : Add named query parameter of BLOB type from the memory buffer. }
    procedure AddParamBlobPtr(const name: String; buffer: pointer; len: integer);
    { : Add named query parameter of BLOB type from the binary string. }
    procedure AddParamBlobText(const name: String; const value: AnsiString);
    { : Add named query parameter of BLOB type from the stream. }
    procedure AddParamBlob(const name: String; const value: TStream; len: integer);
    { : SQLite database handler. }
    property DB: TSQLiteDB read fDB;
    { : Debug hook for log all called queries. }
    property OnQuery: THookQuery read FOnQuery write FOnQuery;
  end;

  { : @abstract(Class for handling SQLite query result) }
  TSQLiteTable = class(TObject)
  private
    fColCount: cardinal;
    fCols: TStringList;
    fRow: cardinal;
    fEOF: Boolean;
    fStmt: TSQLiteStmt;
    fDB: TSQLiteDatabase;
    fSQL: String;
    function GetFields(I: cardinal): String;
    function GetColumns(I: integer): String;
    function GetFieldByName(FieldName: String): String;
    function GetFieldIndex(FieldName: String): integer;
  public
    { : Class constructor. Called internally by @link(TSqliteDatabase) }
    constructor Create(const DB: TSQLiteDatabase; const SQL: String; PrepareOnly: Boolean = false);
    { : Class descructor. Call Free instead. }
    destructor Destroy; override;
    { : Read field from current row as integer. }
    function FieldAsInteger(I: cardinal): int64;
    { : Read field from current row as blob to memory stream. }
    function FieldAsBlob(I: cardinal): TMemoryStream;
    { : Read field from current row as pointer to memory. }
    function FieldAsBlobPtr(I: cardinal; out iNumBytes: integer): pointer;
    { : Read field from current row as blob to AnsiString. }
    function FieldAsBlobText(I: cardinal): AnsiString;
    { : Test if field from current row contains null value. }
    function FieldIsNull(I: cardinal): Boolean;
    { : Read field from current row as string. }
    function FieldAsString(I: cardinal): String;
    { : Read field from current row as floating-point. }
    function FieldAsDouble(I: cardinal): double;
    { : Go to next row. }
    function Next: Boolean;
    { : Reset all query params. }
    procedure ParamsClear;
    { : Reset current result set. After this you can set new query parameters values
      and call prepared query again by @link(next) }
    procedure Reset;
    { : Add named query parameter of integer type. }
    procedure AddParamInt(const name: String; value: int64);
    { : Add named query parameter of floating-point type. }
    procedure AddParamFloat(const name: String; value: double);
    { : Add named query parameter of string or binary type. }
    procedure AddParamText(const name: String; const value: String);
    { : Add named query parameter with null value. }
    procedure AddParamNull(const name: String);
    { : Add named query parameter of BLOB type from memory buffer. }
    procedure AddParamBlobPtr(const name: String; buffer: pointer; len: integer);
    { : Add named query parameter of BLOB type from binary string. }
    procedure AddParamBlobText(const name: String; const value: AnsiString);
    { : Add named query parameter of BLOB type from stream. }
    procedure AddParamBlob(const name: String; const value: TStream; len: integer);
    { : Return value of some field in current row. }
    property Fields[I: cardinal]: String read GetFields;
    { : Return value of named field in current row. }
    property FieldByName[FieldName: String]: String read GetFieldByName;
    { : Return index of some named field. }
    property FieldIndex[FieldName: String]: integer read GetFieldIndex;
    { : Return field type of some field. }
    property Columns[I: integer]: String read GetColumns;
    { : Indicate last row in result set. }
    property EOF: Boolean read fEOF;
    { : Return number of fields in row. }
    property ColCount: cardinal read fColCount;
    { : Number of current row. }
    property Row: cardinal read fRow;
  end;

implementation

resourcestring
  c_unknown = 'Unknown error';
  c_failopen = 'Failed to open database "%s" : %s';
  c_error = '.' + slinebreak + 'Error [%d]: %s.' + slinebreak + '"%s": %s';
  c_nomessage = 'No message';
  c_errorsql = 'Error executing SQL';
  c_errorprepare = 'Could not prepare SQL statement';
  c_errorexec = 'Error executing SQL statement';
  c_errorempty = 'Field %s Not found. Empty dataset';
  c_errorfield = 'Field not found in dataset: %s';
  c_errordata = 'Could not retrieve data';

{$IFDEF WIN32}

function SystemCollate(Userdta: pointer; Buf1Len: integer; Buf1: pointer;
  Buf2Len: integer; Buf2: pointer): integer; cdecl;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, 0, PWideChar(Buf1), Buf1Len,
    PWideChar(Buf2), Buf2Len) - 2;
end;
{$ENDIF}

{ TSQLiteDatabase }

constructor TSQLiteDatabase.Create(const FileName: String);
var
  Msg: PAnsiChar;
  iResult: integer;
  s: String;
begin
  inherited Create;
  fParams := TList.Create;
  Msg := nil;
  fDB := nil;
  try
{$IFDEF SQUNI}
    iResult := SQLite3_Open(PAnsiChar(UTF8String(FileName)), fDB);
{$ELSE}
    iResult := SQLite3_Open(PAnsiChar(AnsiToUtf8(FileName)), fDB);
{$ENDIF}
    if iResult <> SQLITE_OK then
    begin
      s := c_unknown;
      if Assigned(fDB) then
      begin
        Msg := Sqlite3_ErrMsg(fDB);
        s := String(UTF8String(Msg));
      end;
      raise ESQLiteException.CreateFmt(c_failopen, [FileName, s]);
    end;
  finally
    if Assigned(Msg) then
      SQLite3_Free(Msg);
  end;
end;

destructor TSQLiteDatabase.Destroy;
begin
  if Assigned(fDB) then
    SQLite3_Close(fDB);
  ParamsClear;
  fParams.Free;
  inherited;
end;

function TSQLiteDatabase.LastInsertRowID: int64;
begin
  Result := Sqlite3_LastInsertRowID(self.fDB);
end;

function TSQLiteDatabase.LastChangedRows: int64;
begin
  Result := SQLite3_Changes(self.fDB);
end;

function TSQLiteDatabase.TotalChanges: int64;
begin
  Result := SQLite3_TotalChanges(self.fDB);
end;

procedure TSQLiteDatabase.RaiseError(const s, SQL: String);
var
  Msg: PAnsiChar;
  ret: integer;
begin
  Msg := nil;
  ret := sqlite3_errcode(self.fDB);
  if ret <> SQLITE_OK then
    Msg := Sqlite3_ErrMsg(self.fDB);
  if Msg <> nil then
    raise ESQLiteException.CreateFmt(s + c_error, [ret, SQLiteErrorStr(ret), SQL, Msg])
  else
    raise ESQLiteException.CreateFmt(s, [SQL, c_nomessage]);
end;

procedure TSQLiteDatabase.ExecSQL(const SQL: String);
var
  Stmt: TSQLiteStmt;
  NextSQLStatement: PAnsiChar;
  iStepResult: integer;
begin
  try
    if Sqlite3_Prepare_v2(self.fDB, PAnsiChar(UTF8String(SQL)), -1, Stmt, NextSQLStatement) <>
      SQLITE_OK then
      RaiseError(c_errorsql, SQL);
    if (Stmt = nil) then
      RaiseError(c_errorprepare, SQL);
    DoQuery(SQL);
    SetParams(Stmt);

    iStepResult := Sqlite3_step(Stmt);
    if (iStepResult <> SQLITE_DONE) then
    begin
      SQLite3_reset(Stmt);
      RaiseError(c_errorexec, SQL);
    end;
  finally
    if Assigned(Stmt) then
      Sqlite3_Finalize(Stmt);
  end;
end;

function TSQLiteDatabase.GetTable(const SQL: string; PrepareOnly: Boolean = false): TSQLiteTable;
begin
  Result := TSQLiteTable.Create(self, SQL, PrepareOnly);
end;

function TSQLiteDatabase.GetTableValue(const SQL: String): int64;
var
  Table: TSQLiteTable;
begin
  Result := 0;
  Table := self.GetTable(SQL);
  try
    if not Table.EOF then
      Result := Table.FieldAsInteger(0);
  finally
    Table.Free;
  end;
end;

function TSQLiteDatabase.GetTableString(const SQL: String): String;
var
  Table: TSQLiteTable;
begin
  Result := '';
  Table := self.GetTable(SQL);
  try
    if not Table.EOF then
      Result := Table.FieldAsString(0);
  finally
    Table.Free;
  end;
end;

procedure TSQLiteDatabase.GetTableStrings(const SQL: String;
  const value: TStrings);
var
  Table: TSQLiteTable;
begin
  value.Clear;
  Table := self.GetTable(SQL);
  try
    while not Table.EOF do
    begin
      value.Add(Table.FieldAsString(0));
      Table.Next;
    end;
  finally
    Table.Free;
  end;
end;

procedure TSQLiteDatabase.Start(const name: String; const param: String = '');
var
  s: String;
begin
  if name = '' then
  begin
    s := 'BEGIN';
    if param <> '' then
      s := s + ' ' + param;
    self.ExecSQL(s);
  end
  else
    self.ExecSQL('SAVEPOINT ' + name);
end;

procedure TSQLiteDatabase.Commit(const name: String);
begin
  if name = '' then
    self.ExecSQL('COMMIT')
  else
    self.ExecSQL('RELEASE ' + name);
end;

procedure TSQLiteDatabase.Rollback(const name: String);
begin
  if name = '' then
    self.ExecSQL('ROLLBACK')
  else
    self.ExecSQL('ROLLBACK TO ' + name);
end;

procedure TSQLiteDatabase.SetTimeout(value: integer);
begin
  SQLite3_BusyTimeout(self.fDB, value);
end;

function TSQLiteDatabase.Version: String;
begin
  Result := String(UTF8String(SQLite3_Version));
end;

procedure TSQLiteDatabase.AddCustomCollate(name: String;
  xCompare: TCollateXCompare);
begin
  sqlite3_create_collation(fDB, PAnsiChar(UTF8String(name)), SQLITE_UTF8, nil, xCompare);
end;

procedure TSQLiteDatabase.AddSystemCollate;
begin
{$IFDEF WIN32}
  sqlite3_create_collation(fDB, 'SYSTEM', SQLITE_UTF16LE, nil, @SystemCollate);
{$ENDIF}
end;

procedure TSQLiteDatabase.ParamsClear;
var
  n: integer;
begin
  for n := fParams.Count - 1 downto 0 do
    TSQliteParam(fParams[n]).Free;
  fParams.Clear;
end;

procedure TSQLiteDatabase.AddParamInt(const name: String; value: int64);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String(name);
  par.valuetype := SQLITE_INTEGER;
  par.valueinteger := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamFloat(const name: String; value: double);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String(name);
  par.valuetype := SQLITE_FLOAT;
  par.valuefloat := value;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamText(const name: String; const value: String);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String(name);
  par.valuetype := SQLITE_TEXT;
  par.valuedata := UTF8String(value);
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamNull(const name: String);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String(name);
  par.valuetype := SQLITE_NULL;
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamBlobPtr(const name: String; buffer: pointer;
  len: integer);
var
  par: TSQliteParam;
begin
  par := TSQliteParam.Create;
  par.name := UTF8String(name);
  par.valuetype := SQLITE_BLOB;
  setlength(par.valuedata, len);
  if len > 0 then
    move(buffer^, pointer(par.valuedata)^, len);
  fParams.Add(par);
end;

procedure TSQLiteDatabase.AddParamBlobText(const name: String; const value: AnsiString);
begin
  AddParamBlobPtr(name, PAnsiChar(value), length(value));
end;

procedure TSQLiteDatabase.AddParamBlob(const name: String;
  const value: TStream; len: integer);
var
  buffer: AnsiString;
  x: integer;
begin
  setlength(buffer, len);
  x := value.Read(pointer(buffer[1])^, len);
  setlength(buffer, x);
  AddParamBlobText(name, buffer);
end;

procedure TSQLiteDatabase.SetParams(const Stmt: TSQLiteStmt);
var
  n: integer;
  I: integer;
  par: TSQliteParam;
begin
  try
    for n := 0 to fParams.Count - 1 do
    begin
      par := TSQliteParam(fParams[n]);
      if par.name = '' then
        continue;
      I := sqlite3_bind_parameter_index(Stmt, @par.name[1]);
      if I > 0 then
      begin
        case par.valuetype of
          SQLITE_INTEGER:
            sqlite3_bind_int64(Stmt, I, par.valueinteger);
          SQLITE_FLOAT:
            sqlite3_bind_double(Stmt, I, par.valuefloat);
          SQLITE_TEXT:
            if par.valuedata = '' then
              sqlite3_bind_text(Stmt, I, PAnsiChar(par.valuedata),
                0, SQLITE_TRANSIENT)
            else
              sqlite3_bind_text(Stmt, I, @par.valuedata[1],
                length(par.valuedata), SQLITE_TRANSIENT);
          SQLITE_BLOB:
            if par.valuedata = '' then
              sqlite3_bind_blob(Stmt, I, PAnsiChar(par.valuedata),
                0, SQLITE_TRANSIENT)
            else
              sqlite3_bind_blob(Stmt, I, @par.valuedata[1],
                length(par.valuedata), SQLITE_TRANSIENT);
          SQLITE_NULL:
            sqlite3_bind_null(Stmt, I);
        end;
      end;
    end;
  finally
    ParamsClear;
  end;
end;

procedure TSQLiteDatabase.DoQuery(const value: String);
begin
  if Assigned(OnQuery) then
    OnQuery(self, value);
end;

function TSQLiteDatabase.InTransaction: Boolean;
begin
  Result := SQLite3_Get_Autocommit(fDB) = 0;
end;

{ TSQLiteTable }

procedure TSQLiteTable.AddParamFloat(const name: String; value: double);
var
  I: integer;
begin
  if name = '' then
    exit;
  I := sqlite3_bind_parameter_index(fStmt, @UTF8String(name)[1]);
  if I > 0 then
    sqlite3_bind_double(fStmt, I, value);
end;

procedure TSQLiteTable.AddParamInt(const name: String; value: int64);
var
  I: integer;
begin
  if name = '' then
    exit;
  I := sqlite3_bind_parameter_index(fStmt, @UTF8String(name)[1]);
  if I > 0 then
    sqlite3_bind_int64(fStmt, I, value);
end;

procedure TSQLiteTable.AddParamNull(const name: String);
var
  I: integer;
begin
  if name = '' then
    exit;
  I := sqlite3_bind_parameter_index(fStmt, @UTF8String(name)[1]);
  if I > 0 then
    sqlite3_bind_null(fStmt, I);
end;

procedure TSQLiteTable.AddParamText(const name: string; const value: String);
var
  I: integer;
  valueUTF8: UTF8String;
begin
  if name = '' then
    exit;
  I := sqlite3_bind_parameter_index(fStmt, @UTF8String(name)[1]);
  if I > 0 then
  begin
    valueUTF8 := UTF8String(value);
    if valueUTF8 = '' then
      sqlite3_bind_text(fStmt, I, PAnsiChar(valueUTF8), 0, SQLITE_TRANSIENT)
    else
      sqlite3_bind_text(fStmt, I, @valueUTF8[1], length(valueUTF8), SQLITE_TRANSIENT);
  end;
end;

procedure TSQLiteTable.AddParamBlobPtr(const name: String; buffer: pointer; len: integer);
var
  I: integer;
begin
  if name = '' then
    exit;
  I := sqlite3_bind_parameter_index(fStmt, @UTF8String(name)[1]);
  if I > 0 then
    sqlite3_bind_blob(fStmt, I, buffer, len, SQLITE_TRANSIENT);
end;

procedure TSQLiteTable.AddParamBlobText(const name: String; const value: AnsiString);
begin
  if name = '' then
    exit;
  AddParamBlobPtr(name, PAnsiChar(value), length(value));
end;

procedure TSQLiteTable.AddParamBlob(const name: String;
  const value: TStream; len: integer);
var
  buffer: AnsiString;
  x: integer;
begin
  if name = '' then
    exit;
  setlength(buffer, len);
  x := value.Read(pointer(buffer[1])^, len);
  setlength(buffer, x);
  AddParamBlobText(name, buffer);
end;

constructor TSQLiteTable.Create(const DB: TSQLiteDatabase; const SQL: String;
  PrepareOnly: Boolean = false);
var
  NextSQLStatement: PAnsiChar;
  I: integer;
begin
  inherited Create;
  self.fDB := DB;
  self.fEOF := false;
  self.fRow := 0;
  self.fColCount := 0;
  self.fSQL := SQL;
  if Sqlite3_Prepare_v2(DB.fDB, PAnsiChar(UTF8String(SQL)), -1, fStmt, NextSQLStatement) <> SQLITE_OK
  then
    DB.RaiseError(c_errorsql, SQL);
  if (fStmt = nil) then
    DB.RaiseError(c_errorprepare, SQL);
  DB.DoQuery(SQL);
  // get data types
  fCols := TStringList.Create;
  fColCount := SQLite3_ColumnCount(fStmt);
  for I := 0 to Pred(fColCount) do
    // AnsiUpperCase operates on UNICODE strings but according to Ansi Collation Rules!
    fCols.Add(AnsiUpperCase(String(UTF8String(Sqlite3_ColumnName(fStmt, I)))));
  if not PrepareOnly then
  begin
    DB.SetParams(fStmt);
    Next;
  end;
end;

destructor TSQLiteTable.Destroy;
begin
  if Assigned(fStmt) then
    Sqlite3_Finalize(fStmt);
  if Assigned(fCols) then
    fCols.Free;
  inherited;
end;

function TSQLiteTable.FieldAsBlob(I: cardinal): TMemoryStream;
var
  iNumBytes: integer;
  ptr: pointer;
begin
  Result := TMemoryStream.Create;
  iNumBytes := Sqlite3_ColumnBytes(fStmt, I);
  if iNumBytes > 0 then
  begin
    ptr := Sqlite3_ColumnBlob(fStmt, I);
    Result.writebuffer(ptr^, iNumBytes);
    Result.Position := 0;
  end;
end;

function TSQLiteTable.FieldAsBlobPtr(I: cardinal; out iNumBytes: integer): pointer;
begin
  iNumBytes := Sqlite3_ColumnBytes(fStmt, I);
  Result := Sqlite3_ColumnBlob(fStmt, I);
end;

function TSQLiteTable.FieldAsBlobText(I: cardinal): AnsiString;
var
  MemStream: TMemoryStream;
  buffer: PAnsiChar;
begin
  Result := '';
  MemStream := self.FieldAsBlob(I);
  if MemStream <> nil then
    try
      if MemStream.Size > 0 then
      begin
        MemStream.Position := 0;
        buffer := MemStream.Memory;
        SetString(Result, buffer, MemStream.Size);
        // {$IFDEF UNICODE}
        // Buffer := AnsiStralloc(MemStream.Size + 1);
        // {$ELSE}
        // Buffer := Stralloc(MemStream.Size + 1);
        // {$ENDIF}
        // MemStream.readbuffer(Buffer[0], MemStream.Size);
        // (Buffer + MemStream.Size)^ := chr(0);
        // SetString(Result, Buffer, MemStream.size);
        // strdispose(Buffer);
      end;
    finally
      MemStream.Free;
    end
end;

function TSQLiteTable.FieldAsDouble(I: cardinal): double;
begin
  Result := Sqlite3_ColumnDouble(fStmt, I);
end;

function TSQLiteTable.FieldAsInteger(I: cardinal): int64;
begin
  Result := Sqlite3_ColumnInt64(fStmt, I);
end;

function TSQLiteTable.FieldAsString(I: cardinal): String;
begin
  Result := self.GetFields(I);
end;

function TSQLiteTable.FieldIsNull(I: cardinal): Boolean;
begin
  Result := Sqlite3_ColumnText(fStmt, I) = nil;
end;

function TSQLiteTable.GetColumns(I: integer): String;
begin
  Result := fCols[I];
end;

function TSQLiteTable.GetFieldByName(FieldName: String): String;
begin
  Result := GetFields(self.GetFieldIndex(FieldName));
end;

function TSQLiteTable.GetFieldIndex(FieldName: String): integer;
begin
  if (fCols = nil) then
  begin
    raise ESQLiteException.Create(format(c_errorempty, [FieldName]));
    exit;
  end;
  if (fCols.Count = 0) then
  begin
    raise ESQLiteException.Create(format(c_errorempty, [FieldName]));
    exit;
  end;
  Result := fCols.IndexOf(AnsiUpperCase(FieldName));
  if (Result < 0) then
  begin
    raise ESQLiteException.Create(format(c_errorfield, [FieldName]))
  end;
end;

function TSQLiteTable.GetFields(I: cardinal): String;
begin
  Result := String(UTF8String((Sqlite3_ColumnText(fStmt, I))));
end;

function TSQLiteTable.Next: Boolean;
var
  iStepResult: integer;
begin
  fEOF := true;
  iStepResult := Sqlite3_step(fStmt);
  case iStepResult of
    SQLITE_ROW:
      begin
        fEOF := false;
        inc(fRow);
      end;
    SQLITE_DONE:
      // we are on the end of dataset
      // return EOF=true only
      ;
    else
      begin
        SQLite3_reset(fStmt);
        fDB.RaiseError(c_errordata, fSQL);
      end;
  end;
  Result := not fEOF;
end;

procedure TSQLiteTable.ParamsClear;
begin
  sqlite3_clear_bindings(fStmt);
end;

procedure TSQLiteTable.Reset;
begin
  SQLite3_reset(fStmt);
end;

initialization

SQLite3_Initialize;

finalization

SQLite3_Shutdown;

end.
