unit DbcProxyUtils;

{$ifdef fpc}
{$H+}
{$ifend}

{$I ../../Zeos.inc}

interface

uses
  Classes, SysUtils, ZDbcIntfs;

function encodeResultSet(const RS: IZResultSet; const MaxRows: LongWord = 0; const UpdateCount: Integer = 0): String;
function encodeResultSetMetaData(const MD: IZResultSetMetadata): String;
function encodeResultSetRows(const RS: IZResultSet; const MaxRows: LongWord): String;
function encodeConnectionProperties(const Connection: IZConnection): String;
function encodeDatabaseInfo(const Connection: IZConnection): String;

procedure decodeParameters(const ParamXML: String; Statement: IZPreparedStatement);
procedure applyConnectionProperties(const Connection: IZConnection; const Properties: String);

function XMLEncode(Input: String): String;

implementation

uses
  typinfo, dom, XMLRead, Base64{$IFDEF ZEOS73UP}, FMTBCD{$ENDIF};

var
  ProxyFormatSettings: TFormatSettings;

{$IFNDEF FPC}
const
  LineEnding = sLineBreak;
{$IFEND}

function encodeResultSet(const RS: IZResultSet; const MaxRows: LongWord; const UpdateCount: Integer = 0): String;
var
  MD: IZResultSetMetadata;
begin
  MD := RS.GetMetadata;
  Result := '<resultset updatecount="' + IntToStr(UpdateCount) + '">' + LineEnding
          + encodeResultSetMetaData(MD) + LineEnding
          + encodeResultSetRows(RS, MaxRows) + LineEnding
          + '</resultset>';
end;

{--------------- metadata encoding --------------------------------------------}

function encodeResultSetMetaData(const MD: IZResultSetMetadata): String;
const
  MetadataStart = '<metadata>';
  MetadataEnd = '</metadata>';
  ColumnStart = '<column';
  ColumnEnd = ' />';
var
  x: Integer;
  Line: String;

  procedure addProperty(const Name, Value: String); overload;
  begin
    Line := Line + ' ' + Name + '="' + Value +'"';
  end;

  procedure addProperty(const Name: String; Const Value: Integer); overload;
  begin
    Line := Line + ' ' + Name + '="' + IntToStr(Value) +'"';
  end;

  procedure addProperty(const Name: String; const Value: TZSQLType); overload;
  var
    TypeName: String;
  begin
    TypeName := GetEnumName(TypeInfo(Value), Ord(Value));
    Line := Line + ' ' + Name + '="' + TypeName +'"';
  end;

  procedure addProperty(const Name: String; const Value: TZColumnNullableType); overload;
  var
    TypeName: String;
  begin
    TypeName := GetEnumName(TypeInfo(Value), Ord(Value));
    Line := Line + ' ' + Name + '="' + TypeName +'"';
  end;

  procedure addProperty(const Name: String; const Value: Boolean); overload;
  begin
      Line := Line + ' ' + Name + '="' + BoolToStr(Value, True) + '"';
  end;

begin
  Result := MetadataStart + LineEnding;
  // todo: adapt to the current start and end of Zeos enumerations
  for x := 1 to MD.GetColumnCount do begin
    Line := '';
    addProperty('catalogname', MD.GetCatalogName(x));
    addProperty('codepage', IntToStr(MD.GetColumnCodePage(x)));  // is this needed? All data is unicode in the end?
    {$IFNDEF ZEOS73UP}
    addProperty('displaysize', MD.GetColumnDisplaySize(x));
    {$ENDIF}
    addProperty('label', MD.GetColumnLabel(x));
    addProperty('name', MD.GetColumnName(x));
    addProperty('type', MD.GetColumnType(x));
    addProperty('defaultvalue', MD.GetDefaultValue(x));
    addProperty('precision', MD.GetPrecision(x));
    addproperty('scale', MD.GetScale(x));
    addproperty('schemaname', MD.GetSchemaName(x));
    addProperty('tablename', MD.GetTableName(x));
    addproperty('hasdefaultvalue', MD.HasDefaultValue(x));
    addproperty('isautoincrement', MD.IsAutoIncrement(x));
    addProperty('iscasesensitive', MD.IsCaseSensitive(x));
    addProperty('iscurrency', MD.IsCurrency(x));
    addProperty('isdefinitlywritable', MD.IsDefinitelyWritable(x));
    addProperty('isnullable', MD.IsNullable(x));
    addProperty('isreadonly', MD.IsReadOnly(x));
    addProperty('issearchable', MD.IsSearchable(x));
    addproperty('issigned', MD.IsSigned(x));
    addProperty('iswritable', MD.IsWritable(x));
    Result := Result + ColumnStart + Line + ColumnEnd + LineEnding;
  end;
  Result := Result + MetadataEnd;
end;

{------------------ record set encoding ---------------------------------------}

function ConvertBool(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + BoolToStr(RS.GetBoolean(Idx), True) + '" />';
end;

function ConvertInt(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + IntToStr(RS.GetInt(Idx)) + '" />';
end;

function ConvertInt64(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + IntToStr(RS.GetLong(Idx)) + '" />';
end;

function ConvertSingle(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + FloatToStr(RS.GetFloat(Idx), ProxyFormatSettings) + '" />';
end;

function ConvertDouble(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + FloatToStr(RS.GetDouble(Idx), ProxyFormatSettings) + '" />';
end;

function ConvertCurrency(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + CurrToStr(RS.GetCurrency(Idx), ProxyFormatSettings) + '" />';
end;

{$IFNDEF ZEOS73UP}
function ConvertExtended(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + FloatToStr(RS.GetBigDecimal(Idx), ProxyFormatSettings) + '" />';
end;
{$ELSE}
function ConvertBcd(const RS: IZResultSet; Const Idx: Integer): String;
var
  BCD: TBCD;
begin
  RS.GetBigDecimal(Idx, BCD);
  Result := '<field value="' + BCDToStr(BCD, ProxyFormatSettings) + '" />';
end;
{$ENDIF}

function ConvertString(const RS: IZResultSet; Const Idx: Integer): String;
var
  Value: UnicodeString;
begin
  Value := RS.GetUnicodeString(Idx);
  Result := '<field value="' + XMLEncode(Utf8Encode(Value)) + '" />';
end;

function ConvertBinaryStream(const RS: IZResultSet; Const Idx: Integer): String;
var
  DbStream: TStream;
  EncodingStream: TBase64EncodingStream;
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    EncodingStream := TBase64EncodingStream.Create(StringStream);
    try
      DbStream := RS.GetBinaryStream(Idx);
      try
        EncodingStream.CopyFrom(DbStream, DbStream.Size);
      finally
        FreeAndNil(DbStream);
      end;
      EncodingStream.Flush;
    finally
      FreeAndNil(EncodingStream);
    end;
    Result := '<field value="' + XMLEncode(StringStream.DataString) + '" />';
  finally
    FreeAndNil(StringStream);
  end;
end;

function ConvertBytes(const RS: IZResultSet; Const Idx: Integer): String;
var
  DbValue: TBytes;
  EncodingStream: TBase64EncodingStream;
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create('');
  try
    EncodingStream := TBase64EncodingStream.Create(StringStream);
    try
      DbValue := RS.GetBytes(Idx);
      EncodingStream.Write(DbValue[0], Length(DbValue));
      EncodingStream.Flush;
    finally
      FreeAndNil(EncodingStream);
    end;
    Result := '<field value="' + XMLEncode(StringStream.DataString) + '" />';
  finally
    FreeAndNil(StringStream);
  end;
end;

function ConvertDate(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + DateToStr(RS.GetDate(Idx), ProxyFormatSettings) + '" />';
end;

function ConvertTime(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + TimeToStr(RS.GetTime(Idx), ProxyFormatSettings) + '" />';
end;

function ConvertDateTime(const RS: IZResultSet; Const Idx: Integer): String;
begin
  Result := '<field value="' + DateTimeToStr(RS.GetTimestamp(Idx), ProxyFormatSettings) + '" />';
end;

function ConvertNull: String;
begin
  Result := '<field isnull="True" />';
end;

function encodeResultSetRows(const RS: IZResultSet; const MaxRows: LongWord): String;
type
  TRSConversionProc = function(const RS: IZResultSet; Const Idx: Integer): String;
var
  Idx: Integer;
  CF: Array of TRSConversionProc;
  Line: String;
  MD: IZResultSetMetadata;
  Rows: TStringList;
begin
  if not RS.IsAfterLast then begin
    MD := RS.GetMetadata;
    SetLength(CF, MD.GetColumnCount);
    for Idx := FirstDbcIndex to MD.GetColumnCount - 1 + FirstDbcIndex do begin
      case MD.GetColumnType(Idx) of
        stBoolean: CF[Idx - 1] := ConvertBool;
        stByte, stShort, stWord, stSmall, stLongWord, stInteger: CF[Idx - 1] := ConvertInt;
        stULong, stLong: CF[Idx - 1] := ConvertInt64;
        stFloat: CF[Idx - 1] := ConvertSingle;
        stDouble: CF[Idx - 1] := ConvertDouble;
        stCurrency: CF[Idx - 1] := ConvertCurrency;
        stBigDecimal: CF[Idx - 1] := {$IFNDEF ZEOS73UP}ConvertExtended{$ELSE}ConvertBcd{$ENDIF};
        stString, stUnicodeString: CF[Idx - 1] := ConvertString;
        stDate: CF[Idx - 1] := ConvertDate;
        stTime: CF[Idx - 1] := ConvertTime;
        stTimestamp: CF[Idx - 1] := ConvertDateTime;
        stAsciiStream, stUnicodeStream: CF[Idx - 1] := ConvertString;
        stBinaryStream: CF[Idx - 1] := ConvertBinaryStream;
        stBytes: CF[Idx-1] := ConvertBytes;
        else raise Exception.Create('Conversion of type ' + MD.GetColumnTypeName(Idx) + ' is not supported (yet).');
      end;
    end;

    Rows := TStringList.Create;
    try
      while RS.Next do begin
        if (MaxRows <> 0) then
          if (Rows.Count >= MaxRows) then Break;
        Line := '<row>';
        for idx := 1 to MD.GetColumnCount do begin
          if RS.IsNull(idx) then
            Line := Line + ConvertNull
          else
            Line := Line + CF[Idx - 1](RS, Idx);
        end;
        Rows.Add(Line + '</row>');
      end;
      Line := Rows.Text;
    finally
      FreeAndNil(Rows);
    end;
    Result := '<rows>' + LineEnding + Line + '</rows>';
  end;
end;

{------------------------- DecodeParameters -----------------------------------}

procedure DecodeParameters(const ParamXML: String; Statement: IZPreparedStatement);
var
  Doc: TXMLDocument;
  ParamsNode: TDomNode;
  ParamNode: TDOMNode;
  IsNull: Boolean;
  ParamTypeStr: String;
  ParamType: TZSQLType;
  ParamValue: String;
  x: Integer;
  Stream: TStringStream;

  function GetNodeValue(Node: TDomNode): String;
  begin
    if Assigned(Node) then
      Result := UTF8Encode(Node.NodeValue)
    else
      Result := '';
  end;

  function BinaryToBytes(const Base64Str: String): TBytes;
  var
    StringStream: TStringStream;
    DecodingStream: TBase64DecodingStream;
  begin
    StringStream := TStringStream.Create(Base64Str);
    try
      DecodingStream := TBase64DecodingStream.Create(StringStream);
      try
        SetLength(Result, DecodingStream.Size);
        DecodingStream.Read(Result[0], DecodingStream.Size);
      finally
        FreeAndNil(DecodingStream);
      end;
    finally
      FreeAndNil(StringStream);
    end;
  end;

begin
  ParamType := stBoolean;
  Stream := TStringStream.Create(ParamXML);
  try
    XMLRead.ReadXMLFile(Doc, Stream);
    try
      ParamsNode := Doc.GetChildNodes.Item[0];

      for x := 1 to ParamsNode.GetChildNodes.Count do begin
        ParamNode := ParamsNode.GetChildNodes.Item[x - 1];
        IsNull := StrToBoolDef(GetNodeValue(ParamNode.Attributes.GetNamedItem('isnull')), false);
        ParamTypeStr := GetNodeValue(ParamNode.Attributes.GetNamedItem('type'));
        ParamType := TZSQLType(GetEnumValue(TypeInfo(ParamType), ParamTypeStr));
        ParamValue := GetNodeValue(ParamNode.Attributes.GetNamedItem('value'));
        if IsNull then
          Statement.SetNull(x, ParamType)
        else begin
          case ParamType of
            stBoolean:
              Statement.SetBoolean(x, StrToBool(ParamValue));
            stByte:
              Statement.SetByte(x, StrToInt(ParamValue));
            stShort:
              Statement.SetShort(x, StrToInt(ParamValue));
            stWord:
              Statement.SetWord(x, StrToInt(ParamValue));
            stSmall:
              Statement.SetSmall(x, StrToInt(ParamValue));
            stLongWord:
              Statement.SetUInt(x, StrToDWord(ParamValue));
            stInteger:
              Statement.SetInt(x, StrToInt(ParamValue));
            stULong:
              Statement.SetULong(x, StrToQWord(ParamValue));
            stLong:
              Statement.SetLong(x, StrToInt64(ParamValue));
            stFloat:
              Statement.SetFloat(x, StrToFloat(ParamValue, ProxyFormatSettings));
            stDouble:
              Statement.SetDouble(x, StrToFloat(ParamValue, ProxyFormatSettings));
            stCurrency:
              Statement.SetCurrency(x, StrToCurr(ParamValue, ProxyFormatSettings));
            stBigDecimal:
              Statement.SetBigDecimal(x, StrToFloat(ParamValue, ProxyFormatSettings));
            stString, stUnicodeString:
              Statement.SetString(x, ParamValue);
            stDate:
              Statement.SetDate(x, StrToDate(ParamValue, ProxyFormatSettings));
            stTime:
              Statement.SetTime(x, StrToTime(ParamValue, ProxyFormatSettings));
            stTimestamp:
              Statement.SetTimestamp(x, StrToDateTime(ParamValue, ProxyFormatSettings));
            stAsciiStream, stUnicodeStream:
              Statement.SetString(x, ParamValue);
            stBinaryStream, stBytes:
              Statement.SetBytes(x, BinaryToBytes(ParamValue));
            else
              raise Exception.Create('Conversion of parameter of type ' + ParamTypeStr + ' is not supported (yet).');
          end;
        end;
      end;
    finally
      FreeAndNil(Doc);
    end;
  finally
    FreeAndNil(Stream);
  end;
end;

function encodeConnectionProperties(const Connection: IZConnection): String;
var
  List: TStringList;
  TempStr: AnsiString;
  TransactionIsolation: TZTransactIsolationLevel;
  ServerProvider: TZServerProvider;
begin
  List := TStringList.Create;
  try
    List.Values['readonly'] := BoolToStr(Connection.IsReadOnly, true);
    List.Values['catalog'] := Connection.GetCatalog;
    TransactionIsolation := Connection.GetTransactionIsolation;
    TempStr := GetEnumName(TypeInfo(TransactionIsolation), Ord(TransactionIsolation));
    List.Values['transactionisolation'] := TempStr;
    List.Values['usemetadata'] := BoolToStr(Connection.UseMetadata, true);
    {$IFNDEF ZEOS73UP}
    List.Values['autoencodestrings'] := BoolToStr(Connection.GetAutoEncodeStrings, true);
    {$IFEND}
    ServerProvider := Connection.GetServerProvider;
    TempStr := GetEnumName(TypeInfo(ServerProvider), Ord(ServerProvider));
    List.Values['serverprovider'] := TempStr;
    Result := List.Text;
  finally
    FreeAndNil(List);
  end;
end;

procedure applyConnectionProperties(const Connection: IZConnection; const Properties: String);
var
  List: TStringList;
  TempStr: AnsiString;
  TransactionIsolation: TZTransactIsolationLevel;
begin
  TransactionIsolation := tiNone;
  List := TStringList.Create;
  try
    List.Text := Properties;
    //- Set-/IsReadOnly
    TempStr := List.Values['readonly'];
    if TempStr <> '' then
      Connection.SetReadOnly(StrToBool(TempStr));
    //- Set-/GetCatalog
    TempStr := List.Values['catalog'];
    if TempStr <> '' then
      Connection.SetCatalog(TempStr);
    //- Set-/GetTransactionIsolation
    TempStr := List.Values['transactionisolation'];
    if TempStr <> '' then begin
      TransactionIsolation := TZTransactIsolationLevel(GetEnumValue(TypeInfo(TransactionIsolation), TempStr));
      Connection.SetTransactionIsolation(TransactionIsolation);
    end;
    //- (Set)UseMetaData
    TempStr := List.Values['usemetadata'];
    if TempStr <> '' then
      Connection.SetUseMetadata(StrToBool(TempStr));
    //- Get-/SetAutoEncodeStrings
    {$IFNDEF ZEOS73UP}
    TempStr := List.Values['autoencodestrings'];
    if TempStr <> '' then
      Connection.SetAutoEncodeStrings(StrToBool(TempStr));
    {$ENDIF}
    //- Get-/SetAutoCommit (as part of initial property transfer)
    TempStr := List.Values['autocommit'];
    if TempStr <> '' then
      Connection.SetAutoCommit(StrToBool(TempStr));
  finally
    FreeAndNil(List);
  end;
end;

function XMLEncode(Input: String): String;
var
  x: Integer;
  Position: Integer;

  procedure CutAndInsert(Replacement: String);
  begin
    if Position < x then Result := Result + Copy(Input, Position, x - Position);
    Result := Result + Replacement;
    Position := x + 1;
  end;
begin
  Position := 1;
  Result := '';
  for x := 1 to Length(Input) do begin
    case Input[x] of
      #00..#31, '%': CutAndInsert('&#' + IntToStr(Ord(Input[x])) + ';');
      '<': CutAndInsert('&lt;');
      '>': CutAndInsert('&gt;');
      '&': CutAndInsert('&amp;');
      '''': CutAndInsert('&apos;');
      '"': CutAndInsert('&quot;');
    end;
  end;
  if Position <= Length(Input) then Result := Result + Copy(Input, Position, Length(Input));
end;

function encodeDatabaseInfo(const Connection: IZConnection): String;
var
  DbInfo: IZDatabaseInfo;
  PropList: TStringList;
  TempStr: AnsiString;
  TransactionIsolation: TZTransactIsolationLevel;
begin
  Result := '';
  DbInfo := Connection.GetMetadata.GetDatabaseInfo;
  PropList := TStringList.Create;
  try
    PropList.Values['AllProceduresAreCallable'] := BoolToStr(DbInfo.AllProceduresAreCallable, true);
    PropList.Values['AllTablesAreSelectable'] := BoolToStr(DbInfo.AllTablesAreSelectable, true);
    PropList.Values['DataDefinitionCausesTransactionCommit'] := BoolToStr(DbInfo.DataDefinitionCausesTransactionCommit, true);
    PropList.Values['DoesMaxRowSizeIncludeBlobs'] := BoolToStr(DbInfo.DoesMaxRowSizeIncludeBlobs, true);
    PropList.Values['DataDefinitionIgnoredInTransactions'] := BoolToStr(DbInfo.DataDefinitionIgnoredInTransactions, true);
    PropList.Values['CatalogSeparator'] := DbInfo.GetCatalogSeparator;
    PropList.Values['CatalogTerm'] := DbInfo.GetCatalogTerm;
    PropList.Values['DatabaseProductName'] := DbInfo.GetDatabaseProductName;
    PropList.Values['DatabaseProductVersion'] := DbInfo.GetDatabaseProductVersion;
    TransactionIsolation := DbInfo.GetDefaultTransactionIsolation;
    TempStr := GetEnumName(TypeInfo(TransactionIsolation), Ord(TransactionIsolation));
    PropList.Values['DefaultTransactionIsolation'] := TempStr;
    PropList.Values['DriverMajorVersion'] := IntToStr(DbInfo.GetDriverMajorVersion);
    PropList.Values['DriverMinorVersion'] := IntToStr(DbInfo.GetDriverMinorVersion);
    PropList.Values['DriverName'] := DbInfo.GetDriverName;
    PropList.Values['DriverVersion'] := DbInfo.GetDriverVersion;
    PropList.Values['ExtraNameCharacters'] := DbInfo.GetExtraNameCharacters;
    // todo: encode StringList
    //PropList.Values['GetIdentifierQuoteKeywordsSorted'] := DbInfo.GetIdentifierQuoteKeywordsSorted;
    PropList.Values['IdentifierQuoteString'] := DbInfo.GetIdentifierQuoteString;
    PropList.Values['MaxBinaryLiteralLength'] := IntToStr(DbInfo.GetMaxBinaryLiteralLength);
    PropList.Values['MaxCatalogNameLength'] := IntToStr(DbInfo.GetMaxCatalogNameLength);
    PropList.Values['MaxCharLiteralLength'] := IntToStr(DbInfo.GetMaxCharLiteralLength);
    PropList.Values['MaxColumnNameLength'] := IntToStr(DbInfo.GetMaxColumnNameLength);
    PropList.Values['MaxColumnsInGroupBy'] := IntToStr(DbInfo.GetMaxColumnsInGroupBy);
    PropList.Values['MaxColumnsInIndex'] := IntToStr(DbInfo.GetMaxColumnsInIndex);
    PropList.Values['MaxColumnsInOrderBy'] := IntToStr(DbInfo.GetMaxColumnsInOrderBy);
    PropList.Values['MaxColumnsInSelect'] := IntToStr(DbInfo.GetMaxColumnsInSelect);
    PropList.Values['MaxColumnsInTable'] := IntToStr(DbInfo.GetMaxColumnsInTable);
    PropList.Values['MaxConnections'] := IntToStr(DbInfo.GetMaxConnections);
    PropList.Values['MaxCursorNameLength'] := IntToStr(DbInfo.GetMaxCursorNameLength);
    PropList.Values['MaxIndexLength'] := IntToStr(DbInfo.GetMaxIndexLength);
    PropList.Values['MaxProcedureNameLength'] := IntToStr(DbInfo.GetMaxProcedureNameLength);
    PropList.Values['MaxRowSize'] := IntToStr(DbInfo.GetMaxRowSize);
    PropList.Values['MaxSchemaNameLength'] := IntToStr(DbInfo.GetMaxSchemaNameLength);
    PropList.Values['MaxStatementLength'] := IntToStr(DbInfo.GetMaxStatementLength);
    PropList.Values['MaxStatements'] := IntToStr(DbInfo.GetMaxStatements);
    PropList.Values['MaxTableNameLength'] := IntToStr(DbInfo.GetMaxTableNameLength);
    PropList.Values['MaxTablesInSelect'] := IntToStr(DbInfo.GetMaxTablesInSelect);
    PropList.Values['MaxUserNameLength'] := IntToStr(DbInfo.GetMaxUserNameLength);
    PropList.Values['NumericFunctions'] := DbInfo.GetNumericFunctions;
    PropList.Values['ProcedureTerm'] := DbInfo.GetProcedureTerm;
    PropList.Values['SchemaTerm'] := DbInfo.GetSchemaTerm;
    PropList.Values['SearchStringEscape'] := DbInfo.GetSearchStringEscape;
    PropList.Values['ServerVersion'] := DbInfo.GetServerVersion;
    PropList.Values['SQLKeywords'] := DbInfo.GetSQLKeywords;
    PropList.Values['StringFunctions'] := DbInfo.GetStringFunctions;
    PropList.Values['SystemFunctions'] := DbInfo.GetSystemFunctions;
    PropList.Values['TimeDateFunctions'] := DbInfo.GetTimeDateFunctions;
    PropList.Values['IsCatalogAtStart'] := BoolToStr(DbInfo.IsCatalogAtStart, True);
    PropList.Values['IsReadOnly'] := BoolToStr(DbInfo.IsReadOnly, True);
    PropList.Values['NullPlusNonNullIsNull'] := BoolToStr(DbInfo.NullPlusNonNullIsNull, True);
    PropList.Values['NullsAreSortedAtEnd'] := BoolToStr(DbInfo.NullsAreSortedAtEnd, True);
    PropList.Values['NullsAreSortedAtStart'] := BoolToStr(DbInfo.NullsAreSortedAtStart, True);
    PropList.Values['NullsAreSortedHigh'] := BoolToStr(DbInfo.NullsAreSortedHigh, True);
    PropList.Values['NullsAreSortedLow'] := BoolToStr(DbInfo.NullsAreSortedLow, True);
    PropList.Values['StoresLowerCaseIdentifiers'] := BoolToStr(DbInfo.StoresLowerCaseIdentifiers, True);
    PropList.Values['StoresLowerCaseQuotedIdentifiers'] := BoolToStr(DbInfo.StoresLowerCaseQuotedIdentifiers, True);
    PropList.Values['StoresMixedCaseIdentifiers'] := BoolToStr(DbInfo.StoresMixedCaseIdentifiers, True);
    PropList.Values['StoresMixedCaseQuotedIdentifiers'] := BoolToStr(DbInfo.StoresMixedCaseQuotedIdentifiers, True);
    PropList.Values['StoresUpperCaseIdentifiers'] := BoolToStr(DbInfo.StoresUpperCaseIdentifiers, True);
    PropList.Values['StoresUpperCaseQuotedIdentifiers'] := BoolToStr(DbInfo.StoresUpperCaseQuotedIdentifiers, True);
    PropList.Values['SupportsAlterTableWithAddColumn'] := BoolToStr(DbInfo.SupportsAlterTableWithAddColumn, True);
    PropList.Values['SupportsAlterTableWithDropColumn'] := BoolToStr(DbInfo.SupportsAlterTableWithDropColumn, True);
    PropList.Values['SupportsANSI92EntryLevelSQL'] := BoolToStr(DbInfo.SupportsANSI92EntryLevelSQL, True);
    PropList.Values['SupportsANSI92FullSQL'] := BoolToStr(DbInfo.SupportsANSI92FullSQL, True);
    PropList.Values['SupportsANSI92IntermediateSQL'] := BoolToStr(DbInfo.SupportsANSI92IntermediateSQL, True);
    PropList.Values['SupportsArrayBindings'] := BoolToStr(DbInfo.SupportsArrayBindings, True);
    PropList.Values['SupportsBatchUpdates'] := BoolToStr(DbInfo.SupportsBatchUpdates, True);
    PropList.Values['SupportsCatalogsInDataManipulation'] := BoolToStr(DbInfo.SupportsCatalogsInDataManipulation, True);
    PropList.Values['SupportsCatalogsInIndexDefinitions'] := BoolToStr(DbInfo.SupportsCatalogsInIndexDefinitions, True);
    PropList.Values['SupportsCatalogsInPrivilegeDefinitions'] := BoolToStr(DbInfo.SupportsCatalogsInPrivilegeDefinitions, True);
    PropList.Values['SupportsCatalogsInProcedureCalls'] := BoolToStr(DbInfo.SupportsCatalogsInProcedureCalls, True);
    PropList.Values['SupportsCatalogsInTableDefinitions'] := BoolToStr(DbInfo.SupportsCatalogsInTableDefinitions, True);
    PropList.Values['SupportsColumnAliasing'] := BoolToStr(DbInfo.SupportsColumnAliasing, True);
    PropList.Values['SupportsConvert'] := BoolToStr(DbInfo.SupportsConvert, True);
    // todo: not easily transportable. All combinations would have to be tested.
    //PropList.Values['SupportsConvertForTypes'] := BoolToStr(DbInfo.SupportsConvertForTypes(), True);
    PropList.Values['SupportsCoreSQLGrammar'] := BoolToStr(DbInfo.SupportsCoreSQLGrammar, True);
    PropList.Values['SupportsCorrelatedSubqueries'] := BoolToStr(DbInfo.SupportsCorrelatedSubqueries, True);
    PropList.Values['SupportsDataDefinitionAndDataManipulationTransactions'] := BoolToStr(DbInfo.SupportsDataDefinitionAndDataManipulationTransactions, True);
    PropList.Values['SupportsDataManipulationTransactionsOnly'] := BoolToStr(DbInfo.SupportsDataManipulationTransactionsOnly, True);
    PropList.Values['SupportsDifferentTableCorrelationNames'] := BoolToStr(DbInfo.SupportsDifferentTableCorrelationNames, True);
    PropList.Values['SupportsExpressionsInOrderBy'] := BoolToStr(DbInfo.SupportsExpressionsInOrderBy, True);
    PropList.Values['SupportsExtendedSQLGrammar'] := BoolToStr(DbInfo.SupportsExtendedSQLGrammar, True);
    PropList.Values['SupportsFullOuterJoins'] := BoolToStr(DbInfo.SupportsFullOuterJoins, True);
    PropList.Values['SupportsGroupBy'] := BoolToStr(DbInfo.SupportsGroupBy, True);
    PropList.Values['SupportsGroupByBeyondSelect'] := BoolToStr(DbInfo.SupportsGroupByBeyondSelect, True);
    PropList.Values['SupportsGroupByUnrelated'] := BoolToStr(DbInfo.SupportsGroupByUnrelated, True);
    PropList.Values['SupportsIntegrityEnhancementFacility'] := BoolToStr(DbInfo.SupportsIntegrityEnhancementFacility, True);
    PropList.Values['SupportsLikeEscapeClause'] := BoolToStr(DbInfo.SupportsLikeEscapeClause, True);
    PropList.Values['SupportsLimitedOuterJoins'] := BoolToStr(DbInfo.SupportsLimitedOuterJoins, True);
    PropList.Values['SupportsMilliseconds'] := BoolToStr(DbInfo.SupportsMilliseconds, True);
    PropList.Values['SupportsMinimumSQLGrammar'] := BoolToStr(DbInfo.SupportsMinimumSQLGrammar, True);
    PropList.Values['SupportsMixedCaseIdentifiers'] := BoolToStr(DbInfo.SupportsMixedCaseIdentifiers, True);
    PropList.Values['SupportsMixedCaseQuotedIdentifiers'] := BoolToStr(DbInfo.SupportsMixedCaseQuotedIdentifiers, True);
    PropList.Values['SupportsMultipleResultSets'] := BoolToStr(DbInfo.SupportsMultipleResultSets, True);
    PropList.Values['SupportsMultipleTransactions'] := BoolToStr(DbInfo.SupportsMultipleTransactions, True);
    PropList.Values['SupportsNonEscapedSearchStrings'] := BoolToStr(DbInfo.SupportsNonEscapedSearchStrings, True);
    PropList.Values['SupportsNonNullableColumns'] := BoolToStr(DbInfo.SupportsNonNullableColumns, True);
    PropList.Values['SupportsOpenCursorsAcrossCommit'] := BoolToStr(DbInfo.SupportsOpenCursorsAcrossCommit, True);
    PropList.Values['SupportsOpenCursorsAcrossRollback'] := BoolToStr(DbInfo.SupportsOpenCursorsAcrossRollback, True);
    PropList.Values['SupportsOpenStatementsAcrossCommit'] := BoolToStr(DbInfo.SupportsOpenStatementsAcrossCommit, True);
    PropList.Values['SupportsOpenStatementsAcrossRollback'] := BoolToStr(DbInfo.SupportsOpenStatementsAcrossRollback, True);
    PropList.Values['SupportsOrderByUnrelated'] := BoolToStr(DbInfo.SupportsOrderByUnrelated, True);
    PropList.Values['SupportsOuterJoins'] := BoolToStr(DbInfo.SupportsOuterJoins, True);
    PropList.Values['SupportsOverloadPrefixInStoredProcedureName'] := BoolToStr(DbInfo.SupportsOverloadPrefixInStoredProcedureName, True);
    PropList.Values['SupportsParameterBinding'] := BoolToStr(DbInfo.SupportsParameterBinding, True);
    PropList.Values['SupportsPositionedDelete'] := BoolToStr(DbInfo.SupportsPositionedDelete, True);
    PropList.Values['SupportsPositionedUpdate'] := BoolToStr(DbInfo.SupportsPositionedUpdate, True);
    // todo: not easily transportable. All combinations would have to be tested.
    //PropList.Values['SupportsResultSetConcurrency'] := BoolToStr(DbInfo.SupportsResultSetConcurrency(), True);
    // todo: not easily transportable. All combinations would have to be tested.
    //PropList.Values['SupportsResultSetType'] := BoolToStr(DbInfo.SupportsResultSetType(), True);
    PropList.Values['SupportsSchemasInDataManipulation'] := BoolToStr(DbInfo.SupportsSchemasInDataManipulation, True);
    PropList.Values['SupportsSchemasInIndexDefinitions'] := BoolToStr(DbInfo.SupportsSchemasInIndexDefinitions, True);
    PropList.Values['SupportsSchemasInPrivilegeDefinitions'] := BoolToStr(DbInfo.SupportsSchemasInPrivilegeDefinitions, True);
    PropList.Values['SupportsSchemasInProcedureCalls'] := BoolToStr(DbInfo.SupportsSchemasInProcedureCalls, True);
    PropList.Values['SupportsSchemasInTableDefinitions'] := BoolToStr(DbInfo.SupportsSchemasInTableDefinitions, True);
    PropList.Values['SupportsSelectForUpdate'] := BoolToStr(DbInfo.SupportsSelectForUpdate, True);
    PropList.Values['SupportsStoredProcedures'] := BoolToStr(DbInfo.SupportsStoredProcedures, True);
    PropList.Values['SupportsSubqueriesInComparisons'] := BoolToStr(DbInfo.SupportsSubqueriesInComparisons, True);
    PropList.Values['SupportsSubqueriesInExists'] := BoolToStr(DbInfo.SupportsSubqueriesInExists, True);
    PropList.Values['SupportsSubqueriesInIns'] := BoolToStr(DbInfo.SupportsSubqueriesInIns, True);
    PropList.Values['SupportsSubqueriesInQuantifieds'] := BoolToStr(DbInfo.SupportsSubqueriesInQuantifieds, True);
    PropList.Values['SupportsTableCorrelationNames'] := BoolToStr(DbInfo.SupportsTableCorrelationNames, True);
    // todo: not easily transportable. All combinations would have to be tested.
    //PropList.Values['SupportsTransactionIsolationLevel'] := BoolToStr(DbInfo.SupportsTransactionIsolationLevel(), True);
    PropList.Values['SupportsTransactions'] := BoolToStr(DbInfo.SupportsTransactions, True);
    PropList.Values['SupportsUnion'] := BoolToStr(DbInfo.SupportsUnion, True);
    PropList.Values['SupportsUnionAll'] := BoolToStr(DbInfo.SupportsUnionAll, True);
    PropList.Values['SupportsUpdateAutoIncrementFields'] := BoolToStr(DbInfo.SupportsUpdateAutoIncrementFields, True);
    PropList.Values['UsesLocalFilePerTable'] := BoolToStr(DbInfo.UsesLocalFilePerTable, True);
    PropList.Values['UsesLocalFiles'] := BoolToStr(DbInfo.UsesLocalFiles, True);
    Result := PropList.Text;
  finally
    FreeAndNil(PropList);
  end;
end;

initialization
  ProxyFormatSettings.DateSeparator := '-';
  ProxyFormatSettings.LongDateFormat := 'YYYY/MM/DD';
  ProxyFormatSettings.ShortDateFormat := 'YYYY/MM/DD';
  ProxyFormatSettings.LongTimeFormat := 'HH:NN:SS.ZZZ';
  ProxyFormatSettings.ShortTimeFormat := 'HH:NN:SS.ZZZ';
  ProxyFormatSettings.DecimalSeparator := '.';
  ProxyFormatSettings.TimeSeparator := ':';
  ProxyFormatSettings.ThousandSeparator := ',';

end.

