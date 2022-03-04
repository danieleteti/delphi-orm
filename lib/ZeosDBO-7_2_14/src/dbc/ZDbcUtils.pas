{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{            Database Connectivity Functions              }
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

unit ZDbcUtils;

interface

{$I ZDbc.inc}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IFDEF NO_UNIT_CONTNRS}ZClasses{$ELSE}Contnrs{$ENDIF}, TypInfo,
  ZCompatibility, ZDbcIntfs, ZDbcResultSetMetadata, ZTokenizer, ZVariant;

type
  TPreparablePrefixToken = Record
    MatchingGroup: String;
    ChildMatches: TStringDynArray;
  end;
  TPreparablePrefixTokens = array of TPreparablePrefixToken;

  TRawBuff = record
    Pos: Word;
    Buf: array[Byte] of AnsiChar;
  end;

  TUCS2Buff = record
    Pos: Word;
    Buf: array[Byte] of WideChar;
  end;

{**
  Resolves a connection protocol and raises an exception with protocol
  is not supported.
  @param Url an initial database URL.
  @param SuupportedProtocols a driver's supported subprotocols.
}
function ResolveConnectionProtocol(const Url: string;
  const SupportedProtocols: TStringDynArray): string;

{**
  Resolves a database URL and fills the database connection parameters.
  @param Url an initial database URL.
  @param Info an initial info parameters.
  @param HostName a name of the database host.
  @param Port a port number.
  @param Database a database name.
  @param UserName a name of the database user.
  @param Password a user's password.
  @param ResutlInfo a result info parameters.
}
procedure ResolveDatabaseUrl(const Url: string; Info: TStrings;
  var HostName: string; var Port: Integer; var Database: string;
  var UserName: string; var Password: string; ResultInfo: TStrings);

{**
  Checks is the convertion from one type to another type allowed.
  @param InitialType an initial data type.
  @param ResultType a result data type.
  @return <code>True</code> if convertion is allowed
    or <code>False</code> otherwise.
}
function CheckConvertion(InitialType: TZSQLType; ResultType: TZSQLType): Boolean;

{**
  Defines a name of the column type.
  @param ColumnType a type of the column.
  @return a name of the specified type.
}
function DefineColumnTypeName(ColumnType: TZSQLType): string;

{**
  Raises a copy of the given exception.
  @param E an exception to be raised.
}
procedure RaiseSQLException(E: Exception);

{**
  Copies column information objects from one object list to another one.
  @param FromList the source object list.
  @param ToList the destination object list.
}
procedure CopyColumnsInfo(FromList: TObjectList; ToList: TObjectList);

{**
  Defines a statement specific parameter.
  @param Statement a statement interface reference.
  @param ParamName a name of the parameter.
  @param Default a parameter default value.
  @return a parameter value or default if nothing was found.
}
function DefineStatementParameter(const Statement: IZStatement;
  const ParamName: string; const Default: string): string; overload;
  
function DefineStatementParameter(const Connection: IZConnection;
  const StmtInfo: TStrings; const ParamName: string;
  const Default: string): string; overload;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(const QualifiedName: string;
  const SupportsCatalogs, SupportsSchemas: Boolean;
  out Catalog, Schema, ObjectName: string);

{**
  ToLikeString returns the given string or if the string is empty it returns '%'
  @param Value the string
  @return given Value or '%'
}
function ToLikeString(const Value: string): string;

{**
  GetSQLHexString returns a valid x'..' database understandable String from
    binary data
  @param Value the ansistring-pointer to the binary data
  @param Len then length of the binary Data
  @param ODBC a boolean if output result should be with a starting 0x...
  @returns a valid hex formated unicode-safe string
}
function GetSQLHexWideString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): ZWideString;
function GetSQLHexAnsiString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): RawByteString;
function GetSQLHexString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): String;

function TokenizeSQLQueryRaw(var SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex, IsNCharIndex: TBooleanDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens; const CompareSuccess: PBoolean;
  const NeedNCharDetection: Boolean = False): TRawByteStringDynArray;

function TokenizeSQLQueryUni(var SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex, IsNCharIndex: TBooleanDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens; const CompareSuccess: PBoolean;
  const NeedNCharDetection: Boolean = False): TUnicodeStringDynArray;

procedure AssignOutParamValuesFromResultSet(const ResultSet: IZResultSet;
  const OutParamValues: TZVariantDynArray; const OutParamCount: Integer;
  const PAramTypes: TZProcedureColumnTypeDynArray);

{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): RawByteString; overload;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): RawByteString; overload;

function GetValidatedAnsiString(const Ansi: RawByteString;
  ConSettings: PZConSettings; const FromDB: Boolean): RawByteString; overload;

{**
  GetValidatedUnicodeStream the incoming Stream for his given Memory and
  returns a valid Unicode/Widestring Stream
  @param Buffer the pointer to the Data
  @return a valid Unicode encoded stringstram
}
function GetValidatedUnicodeStream(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; FromDB: Boolean): TStream; overload;

function ZSQLTypeToBuffSize(SQLType: TZSQLType): Integer;

procedure RaiseUnsupportedParameterTypeException(ParamType: TZSQLType);

procedure ToBuff(const Value: RawByteString; var Buf: TRawBuff; var Result: RawByteString); overload;
procedure ToBuff(Value: Pointer; L: LengthInt; var Buf: TRawBuff; var Result: RawByteString); overload;
procedure ToBuff(Value: AnsiChar; var Buf: TRawBuff; var Result: RawByteString); overload;
procedure ToBuff(const Value: ZWideString; var Buf: TUCS2Buff; var Result: ZWideString); overload;
procedure ToBuff(Value: WideChar; var Buf: TUCS2Buff; var Result: ZWideString); overload;

procedure FlushBuff(var Buf: TRawBuff; var Result: RawByteString); overload;
procedure FlushBuff(var Buf: TUCS2Buff; var Result: ZWideString); overload;

function GetAbsorbedTrailingSpacesLen(Buf: PAnsiChar; Len: LengthInt): LengthInt; {$IFDEF WITH_INLINE}inline;{$ENDIF} overload;
function GetAbsorbedTrailingSpacesLen(Buf: PWideChar; Len: LengthInt): LengthInt; {$IFDEF WITH_INLINE}inline;{$ENDIF} overload;
const
  i4SpaceRaw: Integer = Ord(#32)+Ord(#32) shl 8 + Ord(#32) shl 16 +Ord(#32) shl 24;  //integer representation of the four space chars
  i4SpaceUni: Int64 = 9007336695791648;  //integer representation of the four wide space chars
implementation

uses ZMessages, ZSysUtils, ZEncoding, ZFastCode, ZGenericSqlToken, Math
  {$IFNDEF NO_UNIT_CONTNRS}, ZClasses{$ENDIF};

{**
  Resolves a connection protocol and raises an exception with protocol
  is not supported.
  @param Url an initial database URL.
  @param SupportedProtocols a driver's supported subprotocols.
}
function ResolveConnectionProtocol(const Url: string;
  const SupportedProtocols: TStringDynArray): string;
var
  I: Integer;
  Protocol: string;
  Index: Integer;
begin
  Result := '';

  Index := FirstDelimiter(':', Url);
  if Index > 0 then
    Protocol := Copy(Url, Index + 1, Length(Url) - Index)
  else
    Protocol := '';
  Index := FirstDelimiter(':', Protocol);
  if Index > 1 then
    Protocol := Copy(Protocol, 1, Index - 1)
  else
    Protocol := '';

  if Protocol = '' then
    raise EZSQLException.Create(Format(SIncorrectConnectionURL, [Url]));

  for I := Low(SupportedProtocols) to High(SupportedProtocols) do
  begin
    if SupportedProtocols[I] = Protocol then
    begin
      Result := Protocol;
      Break;
    end;
  end;

  if Result = '' then
    raise EZSQLException.Create(Format(SUnsupportedProtocol, [Protocol]));
end;

{**
  Resolves a database URL and fills the database connection parameters.
  @param Url an initial database URL.
  @param Info an initial info parameters.
  @param HostName a name of the database host.
  @param Port a port number.
  @param Database a database name.
  @param UserName a name of the database user.
  @param Password a user's password.
  @param ResutlInfo a result info parameters.
}
procedure ResolveDatabaseUrl(const Url: string; Info: TStrings;
  var HostName: string; var Port: Integer; var Database: string;
  var UserName: string; var Password: string; ResultInfo: TStrings);
var
  Temp: string;
begin
   { assign URL first -> define all out out params }
   {A correct builded URL exports all these Params if they are expected!}
  DriverManager.ResolveDatabaseUrl(URL, HostName, Port, DataBase, UserName, Password, ResultInfo);

  { Retrieves non special-escaped-parameters }
  Temp := Url;
  while FirstDelimiter('?', Temp) > 0 do //Get all aditional Parameters
    Temp := Copy(Temp, FirstDelimiter('?', Temp)+1, Length(Temp));
  PutSplitString(ResultInfo, Temp, ';'); //overrides all Strings
  ResultInfo.Text := ReplaceChar(#9, ';', ResultInfo.Text); //unescape the #9 char

  if Assigned(Info) then //isn't that strange? (Shouldn't we pick out double-values?)
    Resultinfo.AddStrings(Info);//All possible PWD/Password and UID/UserName are aviable now, but for what? And the can also be doubled!

  { Redefines user name if not avialble in the URL}
  if UserName = '' then //Priority 1: URL.UserName
  begin
    UserName := ResultInfo.Values['UID']; //Priority 2: Info-UID
    if UserName = '' then
      UserName := ResultInfo.Values['username']; //Priority 3: Info-username
  end;

  { Redefines user password if not avialble in the URL }
  if Password = '' then //Priority 1: URL.Password
  begin
    Password := ResultInfo.Values['PWD']; //Priority 2: Info-PWD
    if Password = '' then
      Password := ResultInfo.Values['password']; //Priority 3: Info-password
  end;
end;

{**
  Checks is the convertion from one type to another type allowed.
  @param InitialType an initial data type.
  @param ResultType a result data type.
  @return <code>True</code> if convertion is allowed
    or <code>False</code> otherwise.
}
function CheckConvertion(InitialType: TZSQLType; ResultType: TZSQLType): Boolean;
begin
  case ResultType of
    stBoolean,
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
    stFloat, stCurrency, stBigDecimal:
      Result := InitialType in [stBoolean,
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stUlong, stLong,
        stFloat, stDouble, stCurrency, stBigDecimal,
        stString, stUnicodeString];
    stDouble:
      Result := InitialType in [stBoolean,
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stUlong, stLong,
        stFloat, stDouble, stCurrency, stBigDecimal,
        stString, stUnicodeString,
        stTime, stDate, stTimeStamp];
    stString, stUnicodeString:
      Result := True;
    stBytes:
      Result := InitialType in [stString, stUnicodeString, stBytes, stGUID,
        stAsciiStream, stUnicodeStream, stBinaryStream];
    stTimestamp:
      Result := InitialType in [stString, stUnicodeString, stDate, stTime, stTimestamp, stDouble];
    stDate:
      Result := InitialType in [stString, stUnicodeString, stDate, stTimestamp, stDouble];
    stTime:
      Result := InitialType in [stString, stUnicodeString, stTime, stTimestamp, stDouble];
    stBinaryStream:
      Result := (InitialType in [stBinaryStream, stBytes]) and (InitialType <> stUnknown);
    stAsciiStream, stUnicodeStream:
      Result := (InitialType in [stString, stUnicodeString, stAsciiStream, stUnicodeStream]) and (InitialType <> stUnknown);
    else
      Result := (ResultType = InitialType) and (InitialType <> stUnknown);
  end;
end;

{**
  Defines a name of the column type.
  @param ColumnType a type of the column.
  @return a name of the specified type.
}
function DefineColumnTypeName(ColumnType: TZSQLType): string;
begin
  case ColumnType of
    stBoolean:        Result := 'Boolean';
    stByte:           Result := 'Byte';
    stShort:          Result := 'Short';
    stWord:           Result := 'Word';
    stSmall:          Result := 'Small';
    stLongWord:       Result := 'LongWord';
    stInteger:        Result := 'Integer';
    stULong:          Result := 'ULong';
    stLong:           Result := 'Long';
    stFloat:          Result := 'Float';
    stDouble:         Result := 'Double';
    stCurrency:       Result := 'Currency';
    stBigDecimal:     Result := 'BigDecimal';
    stString:         Result := 'String';
    stUnicodeString:  Result := 'UnicodeString';
    stBytes:          Result := 'Bytes';
    stGUID:           Result := 'GUID';
    stDate:           Result := 'Date';
    stTime:           Result := 'Time';
    stTimestamp:      Result := 'Timestamp';
    stAsciiStream:    Result := 'AsciiStream';
    stUnicodeStream:  Result := 'UnicodeStream';
    stBinaryStream:   Result := 'BinaryStream';
    stArray:          Result := 'Array';
    stDataSet:        Result := 'DataSet';
    else
      Result := 'Unknown';
  end;
end;

{**
  Raises a copy of the given exception.
  @param E an exception to be raised.
}
procedure RaiseSQLException(E: Exception);
begin
  if E is EZSQLException then
    raise EZSQLException.CreateClone(EZSQLException(E))
  else
    raise EZSQLException.Create(E.Message);
end;

{**
  Copies column information objects from one object list to another one.
  @param FromList the source object list.
  @param ToList the destination object list.
}
procedure CopyColumnsInfo(FromList: TObjectList; ToList: TObjectList);
var
  I: Integer;
  Current: TZColumnInfo;
  ColumnInfo: TZColumnInfo;
begin
  for I := 0 to FromList.Count - 1 do
  begin
    Current := TZColumnInfo(FromList[I]);
    ColumnInfo := TZColumnInfo.Create;

    ColumnInfo.AutoIncrement := Current.AutoIncrement;
    ColumnInfo.CaseSensitive := Current.CaseSensitive;
    ColumnInfo.Searchable := Current.Searchable;
    ColumnInfo.Currency := Current.Currency;
    ColumnInfo.Nullable := Current.Nullable;
    ColumnInfo.Signed := Current.Signed;
    ColumnInfo.ColumnDisplaySize := Current.ColumnDisplaySize;
    ColumnInfo.ColumnLabel := Current.ColumnLabel;
    ColumnInfo.ColumnName := Current.ColumnName;
    ColumnInfo.SchemaName := Current.SchemaName;
    ColumnInfo.Precision := Current.Precision;
    ColumnInfo.Scale := Current.Scale;
    ColumnInfo.TableName := Current.TableName;
    ColumnInfo.CatalogName := Current.CatalogName;
    ColumnInfo.ColumnType := Current.ColumnType;
    ColumnInfo.ReadOnly := Current.ReadOnly;
    ColumnInfo.Writable := Current.Writable;
    ColumnInfo.DefinitelyWritable := Current.DefinitelyWritable;
    ColumnInfo.ColumnCodePage := Current.ColumnCodePage;

    ToList.Add(ColumnInfo);
  end;
end;

{**
  Defines a statement specific parameter.
  @param Statement a statement interface reference.
  @param ParamName a name of the parameter.
  @param Default a parameter default value.
  @return a parameter value or default if nothing was found.
}
function DefineStatementParameter(const Statement: IZStatement; const ParamName: string;
  const Default: string): string;
begin
  Result := Statement.GetParameters.Values[ParamName];
  if Result = '' then
    Result := Statement.GetConnection.GetParameters.Values[ParamName];
  if Result = '' then
    Result := Default;
end;

function DefineStatementParameter(const Connection: IZConnection;
  const StmtInfo: TStrings; const ParamName: string;
  const Default: string): string;
begin
  Result := '';
  if StmtInfo <> nil then
    Result := StmtInfo.Values[ParamName];
  if (Result = '') then
    Result := Connection.GetParameters.Values[ParamName];
  if Result = '' then
    Result := Default;
end;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(const QualifiedName: string;
  const SupportsCatalogs, SupportsSchemas: Boolean;
  out Catalog, Schema, ObjectName: string);
var
  SL: TStrings;
  I: Integer;
begin
  SL := ZSysUtils.SplitString(QualifiedName, '.');
  try
    Catalog := '';
    Schema := '';
    case SL.Count of
      0, 1: ObjectName := QualifiedName;
      2: begin
          if SupportsCatalogs then begin
            Catalog := SL.Strings[0];
            if SupportsSchemas
            then Schema := SL.Strings[1]
            else ObjectName := SL.Strings[1];
          end else if SupportsSchemas then begin
            Schema := SL.Strings[0];
            ObjectName := SL.Strings[1];
          end else
            ObjectName := SL.Strings[0]+'.'+SL.Strings[1];
        end;
      3: if SupportsCatalogs then begin
          Catalog := SL.Strings[0];
          if SupportsSchemas then begin
            Schema := SL.Strings[1];
            ObjectName := SL.Strings[2]
          end else
            ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
        end else if SupportsSchemas then begin
          Schema := SL.Strings[0];
          ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
        end else
          ObjectName := SL.Strings[0]+'.'+SL.Strings[1]+'.'+SL.Strings[2];
      else if SupportsCatalogs then begin
        Catalog := SL.Strings[0];
        if SupportsSchemas then begin
          Schema := SL.Strings[1];
          for i := 2 to SL.Count-1 do
            if i = 2
            then ObjectName := SL.Strings[i]
            else ObjectName := ObjectName+'.'+SL.Strings[i];
        end else begin
          ObjectName := '';
          for i := 2 to SL.Count-1 do
            if I = 2
            then ObjectName := SL.Strings[i]
            else ObjectName := ObjectName+'.'+SL.Strings[i];
        end;
      end else if SupportsSchemas then begin
        Schema := SL.Strings[0];
        for i := 1 to SL.Count-1 do
          if i = 1
          then ObjectName := SL.Strings[i]
          else ObjectName := ObjectName+'.'+SL.Strings[i];
      end else
        for i := 0 to SL.Count-1 do
          if I = 0
          then ObjectName := SL.Strings[i]
          else ObjectName := ObjectName+'.'+SL.Strings[i];
    end;
  finally
    SL.Free;
  end;
end;
{**
  ToLikeString returns the given string or if the string is empty it returns '%'
  @param Value the string
  @return given Value or '%'
}
function ToLikeString(const Value: string): string;
begin
  if Value = '' then
    Result := '%'
  else
    Result := Value;
end;

{**
  GetSQLHexString returns a valid x'..' database understandable String from
    binary data
  @param Value the ansistring-pointer to the binary data
  @param Length then length of the binary Data
  @param ODBC a boolean if output result should be with a starting 0x...
  @returns a valid hex formated unicode-safe string
}

function GetSQLHexWideString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): ZWideString;
var P: PWideChar;
begin
  ZSetString(nil, ((Len+1) shl 1)+Ord(not Odbc), Result{%H-});
  if ODBC then begin
    P := Pointer(Result);
    Word(P^) := Ord('0');
    Word((P+1)^) := Ord('x');
    Inc(P, 2);
    if (Value <> nil) and (Len > 0)then
      ZBinToHex(Value, P, Len);
  end else begin
    P := Pointer(Result);
    Word(P^) := Ord('x');
    Word((P+1)^) := Ord(#39);
    Inc(P,2);
    if (Value <> nil) and (Len > 0)then
      ZBinToHex(Value, P, Len);
    Inc(P, Len shl 1); //shl 1 = * 2 but faster
    Word(P^) := Word(#39);
  end;
end;

function GetSQLHexAnsiString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): RawByteString;
var P: PAnsiChar;
begin
  ZSetString(nil, ((Len+1) shl 1)+Ord(not Odbc), Result{%H-});
  if ODBC then begin
    P := Pointer(Result);
    Byte(P^) := Ord('0');
    Byte((P+1)^) := Ord('x');
    Inc(P, 2);
    if (Value <> nil) and (Len > 0)then
      ZBinToHex(Value, P, Len);
  end else begin
    P := Pointer(Result);
    Byte(P^) := Ord('x');
    Byte((P+1)^) :=Ord(#39);
    Inc(P,2);
    if (Value <> nil) and (Len > 0)then
      ZBinToHex(Value, P, Len);
    Inc(P, Len shl 1); //shl 1 = * 2 but faster
    Byte(P^) := Ord(#39);
  end;
end;

function GetSQLHexString(Value: PAnsiChar; Len: Integer; ODBC: Boolean = False): String;
begin
  {$IFDEF UNICODE}
  Result := GetSQLHexWideString(Value, Len, ODBC);
  {$ELSE}
  Result := GetSQLHexAnsiString(Value, Len, ODBC);
  {$ENDIF}
end;

{**
  Splits a SQL query into a list of sections.
  @returns a list of splitted sections.
}
function TokenizeSQLQueryRaw(var SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex, IsNCharIndex: TBooleanDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens; const CompareSuccess: PBoolean;
  const NeedNCharDetection: Boolean = False): TRawByteStringDynArray;
var
  I, C, N: Integer;
  Temp: RawByteString;
  NextIsNChar, ParamFound: Boolean;
  Tokens: TZTokenDynArray;

  procedure Add(const Value: RawByteString; const Param: Boolean = False);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := Value;
    SetLength(IsParamIndex, Length(Result));
    IsParamIndex[High(IsParamIndex)] := Param;
    SetLength(IsNCharIndex, Length(Result));
    if Param and NextIsNChar then
    begin
      IsNCharIndex[High(IsNCharIndex)] := True;
      NextIsNChar := False;
    end
    else
      IsNCharIndex[High(IsNCharIndex)] := False;
  end;
begin
  ParamFound := (ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSE}Pos{$ENDIF}('?', SQL) > 0);
  if ParamFound or ConSettings^.AutoEncode or Assigned(ComparePrefixTokens) then
  begin
    Tokens := Tokenizer.TokenizeBuffer(SQL, [toSkipEOF]);
    Temp := '';
    SQL := '';
    NextIsNChar := False;
    N := -1;
    CompareSuccess^ := False;
    for I := 0 to High(Tokens) do
    begin
      {check if we've a preparable statement. If ComparePrefixTokens = nil then
        comparing is not required or already done }
      if (Tokens[I].TokenType = ttWord) and Assigned(ComparePrefixTokens) then
        if N = -1 then
        begin
          for C := 0 to high(ComparePrefixTokens) do
            if ComparePrefixTokens[C].MatchingGroup = UpperCase(Tokens[I].Value) then
            begin
              if Length(ComparePrefixTokens[C].ChildMatches) = 0 then
                CompareSuccess^ := True
              else
                N := C; //save group
              Break;
            end;
          if N = -1 then //no sub-tokens ?
            ComparePrefixTokens := nil; //stop compare sequence
        end
        else
        begin //we already got a group
          for C := 0 to high(ComparePrefixTokens[N].ChildMatches) do
            if ComparePrefixTokens[N].ChildMatches[C] = UpperCase(Tokens[I].Value) then
            begin
              CompareSuccess^ := True;
              Break;
            end;
          ComparePrefixTokens := nil; //stop compare sequence
        end;
      SQL := SQL + Tokens[I].Value;
      if ParamFound and (Tokens[I].Value = '?') then
      begin
        Add(Temp);
        Add('?', True);
        Temp := '';
      end
      else
        if ParamFound and NeedNCharDetection and (Tokens[I].Value = 'N') and
          (Length(Tokens) > i) and (Tokens[i+1].Value = '?') then
        begin
          Add(Temp);
          Add('N');
          Temp := '';
          NextIsNChar := True;
        end
        else
          case (Tokens[i].TokenType) of
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier, ttKeyword:
              Temp := Temp + ConSettings^.ConvFuncs.ZStringToRaw(Tokens[i].Value, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP)
            else
              Temp := Temp + {$IFDEF UNICODE}UnicodeStringToASCII7{$ENDIF}(Tokens[i].Value);
          end;
    end;
    if (Temp <> '') then
      Add(Temp);
  end
  else
    Add(ConSettings^.ConvFuncs.ZStringToRaw(SQL, ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP));
end;

{**
  Splits a SQL query into a list of sections.
  @returns a list of splitted sections.
}
function TokenizeSQLQueryUni(var SQL: {$IF defined(FPC) and defined(WITH_RAWBYTESTRING)}RawByteString{$ELSE}String{$IFEND}; Const ConSettings: PZConSettings;
  const Tokenizer: IZTokenizer; var IsParamIndex, IsNCharIndex: TBooleanDynArray;
  ComparePrefixTokens: TPreparablePrefixTokens; const CompareSuccess: PBoolean;
  const NeedNCharDetection: Boolean = False): TUnicodeStringDynArray;
var
  I, C, N: Integer;
  Tokens: TZTokenDynArray;
  Temp: ZWideString;
  NextIsNChar, ParamFound: Boolean;
  procedure Add(const Value: ZWideString; Const Param: Boolean = False);
  begin
    SetLength(Result, Length(Result)+1);
    Result[High(Result)] := Value;
    SetLength(IsParamIndex, Length(Result));
    IsParamIndex[High(IsParamIndex)] := Param;
    SetLength(IsNCharIndex, Length(Result));
    if Param and NextIsNChar then
    begin
      IsNCharIndex[High(IsNCharIndex)] := True;
      NextIsNChar := False;
    end
    else
      IsNCharIndex[High(IsNCharIndex)] := False;
  end;
begin
  ParamFound := (ZFastCode.{$IFDEF USE_FAST_CHARPOS}CharPos{$ELSe}Pos{$ENDIF}('?', SQL) > 0);
  if ParamFound or ConSettings^.AutoEncode or Assigned(ComparePrefixTokens) then
  begin
    Tokens := Tokenizer.TokenizeBuffer(SQL, [toSkipEOF]);

    Temp := '';
    SQL := '';
    NextIsNChar := False;
    N := -1;
    for I := 0 to High(Tokens) do
    begin
      {check if we've a preparable statement. If ComparePrefixTokens = nil then
        comparing is not required or already done }
      if (Tokens[I].TokenType = ttWord) and Assigned(ComparePrefixTokens) then
        if N = -1 then
        begin
          for C := 0 to high(ComparePrefixTokens) do
            if ComparePrefixTokens[C].MatchingGroup = UpperCase(Tokens[I].Value) then
            begin
              if Length(ComparePrefixTokens[C].ChildMatches) = 0 then
                CompareSuccess^ := True
              else
                N := C; //save group
              Break;
            end;
          if N = -1 then //no sub-tokens ?
            ComparePrefixTokens := nil; //stop compare sequence
        end
        else
        begin //we already got a group
          for C := 0 to high(ComparePrefixTokens[N].ChildMatches) do
            if ComparePrefixTokens[N].ChildMatches[C] = UpperCase(Tokens[I].Value) then
            begin
              CompareSuccess^ := True;
              Break;
            end;
          ComparePrefixTokens := nil; //stop compare sequence
        end;
      SQL := SQL + Tokens[I].Value;
      if ParamFound and (Tokens[I].Value = '?') then
      begin
        Add(Temp);
        Add('?', True);
        Temp := '';
      end
      else
        if ParamFound and NeedNCharDetection and (Tokens[I].Value = 'N') and
          (Length(Tokens) > i) and (Tokens[i+1].Value = '?') then
        begin
          Add(Temp);
          Add('N');
          Temp := '';
          NextIsNChar := True;
        end
        else
          {$IFDEF UNICODE}
          Temp := Temp + Tokens[i].Value;
          {$ELSE}
          case (Tokens[i].TokenType) of
            ttQuoted, ttComment,
            ttWord, ttQuotedIdentifier, ttKeyword:
              Temp := Temp + ConSettings^.ConvFuncs.ZStringToUnicode(Tokens[i].Value, ConSettings^.CTRL_CP)
            else
              Temp := Temp + ASCII7ToUnicodeString(Tokens[i].Value);
          end;
          {$ENDIF}
    end;
    if (Temp <> '') then
      Add(Temp);
  end
  else
    {$IFDEF UNICODE}
    Add(SQL);
    {$ELSE}
    Add(ConSettings^.ConvFuncs.ZStringToUnicode(SQL, ConSettings^.CTRL_CP));
    {$ENDIF}
end;

procedure AssignOutParamValuesFromResultSet(const ResultSet: IZResultSet;
  const OutParamValues: TZVariantDynArray; const OutParamCount: Integer;
  const ParamTypes: TZProcedureColumnTypeDynArray);
var
  ParamIndex, I: Integer;
  HasRows: Boolean;
  SupportsMoveAbsolute: Boolean;
  Meta: IZResultSetMetadata;
begin
  SupportsMoveAbsolute := ResultSet.GetType <> rtForwardOnly;
  if SupportsMoveAbsolute then ResultSet.BeforeFirst;
  HasRows := ResultSet.Next;

  I := FirstDbcIndex;
  Meta := ResultSet.GetMetadata;
  for ParamIndex := 0 to OutParamCount - 1 do
  begin
    if not (ParamTypes[ParamIndex] in [pctOut, pctInOut, pctReturn]) then
      Continue;
    if I > Meta.GetColumnCount {$IFDEF GENERIC_INDEX}-1{$ENDIF} then
      Break;

    if (not HasRows) or (ResultSet.IsNull(I)) then
      OutParamValues[ParamIndex] := NullVariant
    else
      case Meta.GetColumnType(I) of
        stBoolean:
          OutParamValues[ParamIndex] := EncodeBoolean(ResultSet.GetBoolean(I));
        stByte:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetByte(I));
        stShort:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetShort(I));
        stWord:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetWord(I));
        stSmall:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetSmall(I));
        stLongword:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetUInt(I));
        stInteger:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetInt(I));
        stULong:
          OutParamValues[ParamIndex] := EncodeUInteger(ResultSet.GetULong(I));
        stLong:
          OutParamValues[ParamIndex] := EncodeInteger(ResultSet.GetLong(I));
        stBytes:
          OutParamValues[ParamIndex] := EncodeBytes(ResultSet.GetBytes(I));
        stFloat:
          OutParamValues[ParamIndex] := EncodeFloat(ResultSet.GetFloat(I));
        stDouble:
          OutParamValues[ParamIndex] := EncodeFloat(ResultSet.GetDouble(I));
        stCurrency:
          OutParamValues[ParamIndex] := EncodeFloat(ResultSet.GetCurrency(I));
        stBigDecimal:
          OutParamValues[ParamIndex] := EncodeFloat(ResultSet.GetBigDecimal(I));
        stString, stAsciiStream:
          OutParamValues[ParamIndex] := EncodeString(ResultSet.GetString(I));
        stUnicodeString, stUnicodeStream:
          OutParamValues[ParamIndex] := EncodeUnicodeString(ResultSet.GetUnicodeString(I));
        stDate:
          OutParamValues[ParamIndex] := EncodeDateTime(ResultSet.GetDate(I));
        stTime:
          OutParamValues[ParamIndex] := EncodeDateTime(ResultSet.GetTime(I));
        stTimestamp:
          OutParamValues[ParamIndex] := EncodeDateTime(ResultSet.GetTimestamp(I));
        stBinaryStream:
          OutParamValues[ParamIndex] := EncodeInterface(ResultSet.GetBlob(I));
        else
          OutParamValues[ParamIndex] := EncodeString(ResultSet.GetString(I));
      end;
    Inc(I);
  end;
  if SupportsMoveAbsolute then ResultSet.BeforeFirst;
end;

function TestEncoding(const Bytes: TByteDynArray; const Size: Cardinal;
  const ConSettings: PZConSettings): TZCharEncoding;
begin
  Result := ceDefault;
  {EgonHugeist:
    Step one: Findout, wh at's comming in! To avoid User-Bugs as good as possible
      it is possible that a PAnsiChar OR a PWideChar was written into
      the Stream!!!  And these chars could be trunced with changing the
      Stream.Size.
      I know this can lead to pain with two byte ansi chars, but what else can i do?
    step two: detect the encoding }

  if (Size mod 2 = 0) and ( ZFastCode.StrLen(Pointer(Bytes)) {%H-}< Size ) then //Sure PWideChar written!! A #0 was in the byte-sequence!
    result := ceUTF16
  else
    if ConSettings.AutoEncode then
      case ZDetectUTF8Encoding(Pointer(Bytes), Size) of
        etUSASCII: Result := ceDefault; //Exact!
        etAnsi:
          { Sure this isn't right in all cases!
            Two/four byte WideChars causing the same result!
            Leads to pain! Is there a way to get a better test?
            I've to start from the premise the function which calls this func
            should decide wether ansi or unicode}
          Result := ceAnsi;
        etUTF8: Result := ceUTF8; //Exact!
      end
    else
      Result := ceDefault
end;

{**
  GetValidatedTextStream the incoming Stream for his given Memory and
  returns a valid UTF8/Ansi StringStream
  @param Stream the Stream with the unknown format and data
  @return a valid utf8 encoded stringstram
}
function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings): RawByteString;
var
  US: ZWideString; //possible com base widestring -> prevent overflow
  Bytes: TByteDynArray;
  Encoding: TZCharEncoding;
begin
  if Size = 0 then
    Result := EmptyRaw
  else
  begin
    SetLength(Bytes, Size +2);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.move(Buffer^, Pointer(Bytes)^, Size);
    Encoding := TestEncoding(Bytes, Size, ConSettings);
    SetLength(Bytes, 0);
    case Encoding of
      ceDefault: ZSetString(Buffer, Size, Result);
      ceAnsi:
        if ConSettings.ClientCodePage.Encoding in [ceAnsi, ceUTF16] then
          if ( ConSettings.CTRL_CP = zCP_UTF8) or (ConSettings.CTRL_CP = ConSettings.ClientCodePage.CP) then //second test avoids encode the string twice
            ZSetString(Buffer, Size, Result)  //should be exact
          else
          begin
            US := PRawToUnicode(Buffer, Size, ConSettings.CTRL_CP);
            Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP)
          end
        else begin  //Database expects UTF8
          if ( ConSettings.CTRL_CP = zCP_UTF8) then
            if ZOSCodePage = zCP_UTF8 then
              US := ZSysUtils.ASCII7ToUnicodeString(Buffer, Size) //Can't localize the ansi CP
            else
              US := PRawToUnicode(Buffer, Size, ZOSCodePage)
          else
            US := PRawToUnicode(Buffer, Size, ConSettings.CTRL_CP);
          Result := ZUnicodeToRaw(US, zCP_UTF8);
        end;
      ceUTF8:
        if (ConSettings.ClientCodePage.Encoding in [ceAnsi, ceUTF16]) then begin//ansi expected
          {$IFDEF WITH_LCONVENCODING}
          ZSetString(Buffer, Size, Result);
          Result := Consettings.PlainConvertFunc(Result);
          {$ELSE}
          US := PRawToUnicode(Buffer, Size, zCP_UTF8);
          Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP)
          {$ENDIF}
         end else //UTF8 Expected
           ZSetString(Buffer, Size, Result);  //should be exact
      ceUTF16:
        begin
          SetLength(US, Size shr 1);
          {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(US)^, Size);
          if ConSettings.ClientCodePage.Encoding = ceAnsi then
            {$IFDEF WITH_LCONVENCODING}
            Result := Consettings.PlainConvertFunc(UTF8Encode(US))
            {$ELSE}
            Result := ZUnicodeToRaw(US, ConSettings.ClientCodePage.CP)
            {$ENDIF}
          else
            Result := ZUnicodeToRaw(US, zCP_UTF8);
        end;
      else
        Result := EmptyRaw;
    end;
  end;
end;

function GetValidatedAnsiStringFromBuffer(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; ToCP: Word): RawByteString;
var DB_CP: Word;
begin
  DB_CP := ConSettings.ClientCodePage.CP;
  ConSettings.ClientCodePage.CP := ToCP;
  Result := GetValidatedAnsiStringFromBuffer(Buffer, Size, ConSettings);
  ConSettings.ClientCodePage.CP := DB_CP;
end;

function GetValidatedAnsiString(const Ansi: RawByteString;
  ConSettings: PZConSettings; const FromDB: Boolean): RawByteString;
begin
  if FromDB then
    if ( ConSettings.CTRL_CP = ConSettings.ClientCodePage.CP ) or not ConSettings.AutoEncode then
      Result := Ansi
    else
      {$IFDEF WITH_LCONVENCODING}
      Result := Consettings.DbcConvertFunc(Ansi)
      {$ELSE}
      Result := ZUnicodeToRaw(ZRawToUnicode(Ansi, ConSettings^.ClientCodePage^.CP), ConSettings^.CTRL_CP)
      {$ENDIF}
  else
    Result := EmptyRaw; // not done yet  and not needed. Makes the compiler happy
end;

{**
  GetValidatedUnicodeStream the incoming Stream for his given Memory and
  returns a valid Unicode/Widestring Stream
  @param Stream the Stream with the unknown format and data
  @return a valid Unicode encoded stringstram
}
function GetValidatedUnicodeStream(const Buffer: Pointer; Size: Cardinal;
  ConSettings: PZConSettings; FromDB: Boolean): TStream;
var
  US: ZWideString;
  Bytes: TByteDynArray;
  Encoding: TZCharEncoding;
begin
  Result := nil;
  US := '';
  if Assigned(Buffer) and ( Size > 0 ) then
  begin
    SetLength(Bytes, Size +2);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(Bytes)^, Size);
    if FromDB then //do not check encoding twice
      US := PRawToUnicode(Buffer, Size, ConSettings.ClientCodePage.CP)
    else begin
      Encoding := TestEncoding(Bytes, Size, ConSettings);
      SetLength(Bytes, 0);
      case Encoding of
        ceDefault: US := USASCII7ToUnicodeString(Buffer, Size);
        ceAnsi: //We've to start from the premisse we've got a Unicode string in here ):
          begin
            SetLength(US, Size shr 1);
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(US)^, Size);
          end;
        ceUTF8: US := PRawToUnicode(Buffer, size, zCP_UTF8);
        ceUTF16:
          begin
            SetLength(US, Size shr 1);
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buffer^, Pointer(US)^, Size);
          end;
      end;
    end;

    if US <> '' then
      Result := StreamFromData(US);
  end;
end;

function ZSQLTypeToBuffSize(SQLType: TZSQLType): Integer;
begin
  Result := 0;
  case SQLType of
    stUnknown: ;
    stBoolean: Result := SizeOf(WordBool);
    stByte, stShort: Result := 1;
    stWord, stSmall: Result := 2;
    stLongWord, stInteger, stFloat: Result := 4;
    stULong, stLong, stDouble, stCurrency, stDate, stTime, stTimestamp: Result := 8;
    stBigDecimal: Result := SizeOf(Extended);
    stGUID: Result := SizeOf(TGUID);
  end;
end;

procedure RaiseUnsupportedParameterTypeException(ParamType: TZSQLType);
var
  TypeName: String;
begin
  TypeName := GetEnumName(TypeInfo(TZSQLType), Ord(ParamType));
  raise EZSQLException.Create(SUnsupportedParameterType + ': ' + TypeName);
end;

procedure ToBuff(const Value: RawByteString; var Buf: TRawBuff; var Result: RawByteString); overload;
var
  P: PAnsiChar;
  L, LRes: LengthInt;
begin
  L := Length(Value){$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}-1{$ENDIF};
  if L <= 0 then Exit;
  if L < (SizeOf(Buf.Buf)-Buf.Pos) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> worth it the check
    then Buf.Buf[Buf.Pos] := AnsiChar(P^)
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Buf.Buf[Buf.Pos], L);
    Inc(Buf.Pos, L);
  end else begin
    LRes := Length(Result)+Buf.Pos+L;
    SetLength(Result, LRes{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
    P := Pointer(Result);
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    PByte(P+LRes)^ := Ord(#0);
    {$ENDIF}
    Inc(P, LRes-Buf.Pos-L);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, P^, L);
  end;
end;

procedure ToBuff(Value: Pointer; L: LengthInt; var Buf: TRawBuff; var Result: RawByteString); overload;
var
  P: PAnsiChar;
  LRes: LengthInt;
begin
  if L <= 0 then Exit;
  if L < (SizeOf(Buf.Buf)-Buf.Pos) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> worth it the check
    then Buf.Buf[Buf.Pos] := AnsiChar(P^)
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Buf.Buf[Buf.Pos], L);
    Inc(Buf.Pos, L);
  end else begin
    LRes := Length(Result)+Buf.Pos+L;
    SetLength(Result, LRes{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
    P := Pointer(Result);
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    PByte(P+LRes)^ := Ord(#0);
    {$ENDIF}
    Inc(P, LRes-Buf.Pos-L);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, P^, L);
  end;
end;

procedure ToBuff(Value: AnsiChar; var Buf: TRawBuff; var Result: RawByteString); overload;
var
  P: PAnsiChar;
  L: LengthInt;
begin
  if Buf.Pos < (SizeOf(Buf.Buf)) then begin
    Buf.Buf[Buf.Pos] := Value;
    Inc(Buf.Pos);
  end else begin
    L := Length(Result)+Buf.Pos+1;
    SetLength(Result, L{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
    P := Pointer(Result);
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    PByte(P+L)^ := Ord(#0);
    {$ENDIF}
    Inc(P, Length(Result)-Buf.Pos-1);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    AnsiChar(P^) := Value;
  end;
end;

procedure ToBuff(const Value: ZWideString; var Buf: TUCS2Buff; var Result: ZWideString); overload;
var
  P: PWideChar;
  L: LengthInt;
begin
  L := Length(Value);
  if L <= 0 then Exit;
  if L < ((SizeOf(Buf.Buf) shr 1)-Buf.Pos) then begin
    P := Pointer(Value);
    if L = 1 //happens very often (comma,space etc) -> worth it the check
    then Buf.Buf[Buf.Pos] := P^
    else {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Buf.Buf[Buf.Pos], L shl 1);
    Inc(Buf.Pos, L);
  end else begin
    SetLength(Result, Length(Result)+Buf.Pos+L);
    P := Pointer(Result);
    Inc(P, Length(Result)-Buf.Pos-L);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos shl 1);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value)^, P^, L shl 1);
  end;
end;

procedure ToBuff(Value: WideChar; var Buf: TUCS2Buff; var Result: ZWideString); overload;
var
  P: PWideChar;
  L: LengthInt;
begin
  if (Buf.Pos < (SizeOf(Buf.Buf) shr 1)) then begin
    Buf.Buf[Buf.Pos] := Value;
    Inc(Buf.Pos);
  end else begin
    L := Length(Result)+Buf.Pos+1;
    SetLength(Result, L);
    P := Pointer(Result);
    Inc(P, L-Buf.Pos-1);
    if Buf.Pos > 0 then begin
      {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos shl 1);
      Inc(P, Buf.Pos);
      Buf.Pos := 0;
    end;
    P^ := Value;
  end;
end;

procedure FlushBuff(var Buf: TRawBuff; var Result: RawByteString); overload;
var P: PAnsiChar;
  L: LengthInt;
begin
  if Buf.Pos > 0 then begin
    L := Length(Result)+Buf.Pos;
    SetLength(Result, L{$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}+1{$ENDIF});
    P := Pointer(Result);
    {$IFDEF WITH_TBYTES_AS_RAWBYTESTRING}
    PByte(P+L)^ := Ord(#0);
    {$ENDIF}
    Inc(P, L-Buf.Pos);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos);
    Buf.Pos := 0;
  end;
end;

procedure FlushBuff(var Buf: TUCS2Buff; var Result: ZWideString); overload;
var P: PWideChar;
  L: LengthInt;
begin
  if Buf.Pos > 0 then begin
    L := Length(Result)+Buf.Pos;
    SetLength(Result, L);
    P := Pointer(Result);
    Inc(P, L-Buf.Pos);
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Buf.Buf[0], P^, Buf.Pos shl 1);
    Buf.Pos := 0;
  end;
end;

function GetAbsorbedTrailingSpacesLen(Buf: PAnsiChar; Len: LengthInt): LengthInt;
var PEnd: PAnsiChar;
begin
  if Len > 4 then begin
    PEnd := Buf + Len - 4;
    while (PEnd >= Buf) and (PInteger(PEnd)^ = i4SpaceRaw) do
      Dec(PEnd, 4);
    Inc(PEnd, 4);
  end else
    PEnd := Buf+Len;
  while (PEnd > Buf) and (PByte(PEnd-1)^ = Ord(' ')) do
    Dec(PEnd);
  Result := PEnd - Buf;
end;

function GetAbsorbedTrailingSpacesLen(Buf: PWideChar; Len: LengthInt): LengthInt;
var PEnd: PWideChar;
begin
  if Len > 4 then begin
    PEnd := Buf + Len - 4;
    while (PEnd >= Buf) and (PInt64(PEnd)^ = i4SpaceUni) do
      Dec(PEnd, 4);
    Inc(PEnd, 4);
  end else
    PEnd := Buf+Len;
  while (PEnd > Buf) and (PWord(PEnd-1)^ = Ord(' ')) do
    Dec(PEnd);
  Result := PEnd- Buf;
end;
end.
