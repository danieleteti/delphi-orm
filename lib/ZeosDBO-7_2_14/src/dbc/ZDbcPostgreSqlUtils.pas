{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{         PostgreSQL Database Connectivity Classes        }
{                                                         }
{         Originally written by Sergey Seroukhov          }
{                           and Sergey Merkuriev          }
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

unit ZDbcPostgreSqlUtils;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
uses
  Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  ZDbcIntfs, ZPlainPostgreSqlDriver, ZDbcPostgreSql, ZDbcLogging,
  ZCompatibility, ZVariant;

{**
  Indicate what field type is a number (integer, float and etc.)
  @param  the SQLType field type value
  @result true if field type number
}
function IsNumber(Value: TZSQLType): Boolean;

{**
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param The TypeName is PostgreSQL type name
   @return The ZSQLType type
}
function PostgreSQLToSQLType(const Connection: IZPostgreSQLConnection;
  const TypeName: string): TZSQLType; overload;

{**
    Another version of PostgreSQLToSQLType()
      - comparing integer should be faster than AnsiString?
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param TypeOid is PostgreSQL type OID
   @return The ZSQLType type
}
function PostgreSQLToSQLType(const ConSettings: PZConSettings;
  const OIDAsBlob: Boolean; const TypeOid: Integer): TZSQLType; overload;

{**
   Return PostgreSQL type name from ZSQLType
   @param The ZSQLType type
   @return The Postgre TypeName
}
function SQLTypeToPostgreSQL(SQLType: TZSQLType; IsOidAsBlob: Boolean): string;

{**
  add by Perger -> based on SourceForge:
  [ 1520587 ] Fix for 1484704: bytea corrupted on post when not using utf8,
  file: 1484704.patch

  Converts a binary string into escape PostgreSQL format.
  @param Value a binary stream.
  @return a string in PostgreSQL binary string escape format.
}
function EncodeBinaryString(SrcBuffer: PAnsiChar; Len: Integer; Quoted: Boolean = False): RawByteString;

{**
  Encode string which probably consists of multi-byte characters.
  Characters ' (apostraphy), low value (value zero), and \ (back slash) are encoded. Since we have noticed that back slash is the second byte of some BIG5 characters (each of them is two bytes in length), we need a characterset aware encoding function.
  @param CharactersetCode the characterset in terms of enumerate code.
  @param Value the regular string.
  @return the encoded string.
}
function PGEscapeString(SrcBuffer: PAnsiChar; SrcLength: Integer;
    ConSettings: PZConSettings; Quoted: Boolean): RawByteString;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeString(const Value: RawByteString): RawByteString;

{**
  Checks for possible sql errors.
  @param Connection a reference to database connection to execute Rollback.
  @param PlainDriver a PostgreSQL plain driver.
  @param Handle a PostgreSQL connection reference.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
  @param ResultHandle the Handle to the Result
}
function CheckPostgreSQLError(const Connection: IZConnection;
  const PlainDriver: IZPostgreSQLPlainDriver; const Handle: PZPostgreSQLConnect;
  const LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  const ResultHandle: PZPostgreSQLResult): String;


{**
   Resolve problem with minor version in PostgreSql bettas
   @param Value a minor version string like "4betta2"
   @return a miror version number
}
function GetMinorVersion(const Value: string): Word;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function PGPrepareAnsiSQLParam(const Value: TZVariant; const ClientVarManager: IZClientVariantManager;
  const Connection: IZPostgreSQLConnection; ChunkSize: Cardinal; InParamType: TZSQLType;
  oidasblob, DateTimePrefix, QuotedNumbers: Boolean; ConSettings: PZConSettings): RawByteString;

{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit

uses ZFastCode, ZMessages, ZDbcPostgreSqlResultSet, ZDbcUtils, ZSysUtils,ZClasses;

{**
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param The TypeName is PostgreSQL type name
   @return The ZSQLType type
}
function PostgreSQLToSQLType(const Connection: IZPostgreSQLConnection;
  const TypeName: string): TZSQLType;
var
  TypeNameLo: string;
  P: PChar absolute TypeNameLo;
begin
  TypeNameLo := LowerCase(TypeName);
  if (TypeNameLo = 'interval') or (TypeNameLo = 'char') or (TypeNameLo = 'bpchar')
    or (TypeNameLo = 'varchar') or (TypeNameLo = 'bit') or (TypeNameLo = 'varbit')
  then if (Connection.GetConSettings.CPType = cCP_UTF16) then
      Result := stUnicodeString
    else
      Result := stString
  else if (TypeNameLo = 'text') or (TypeNameLo = 'citext') then
    Result := stAsciiStream
  else if TypeNameLo = 'oid' then
  begin
    if Connection.IsOidAsBlob() then
      Result := stBinaryStream
    else
      Result := stLongWord;
  end
  else if TypeNameLo = 'name' then
    Result := stString
  else if TypeNameLo = 'enum' then
    Result := stString
  else if TypeNameLo = 'cidr' then
    Result := stString
  else if TypeNameLo = 'inet' then
    Result := stString
  else if TypeNameLo = 'macaddr' then
    Result := stString
  else if TypeNameLo = 'int2' then
    Result := stSmall
  else if TypeNameLo = 'int4' then
    Result := stInteger
  else if TypeNameLo = 'int8' then
    Result := stLong
  else if TypeNameLo = 'float4' then
    Result := stFloat
  else if (TypeNameLo = 'float8') or (TypeNameLo = 'decimal')
    or (TypeNameLo = 'numeric') then
    Result := stDouble
  else if TypeNameLo = 'money' then
    Result := stCurrency
  else if TypeNameLo = 'bool' then
    Result := stBoolean
  else if TypeNameLo = 'date' then
    Result := stDate
  else if TypeNameLo = 'time' then
    Result := stTime
  else if (TypeNameLo = 'datetime') or (TypeNameLo = 'timestamp')
    or (TypeNameLo = 'timestamptz') or (TypeNameLo = 'abstime') then
    Result := stTimestamp
  else if TypeNameLo = 'regproc' then
    Result := stString
  else if TypeNameLo = 'bytea' then
  begin
    if Connection.IsOidAsBlob then
      Result := stBytes
    else
      Result := stBinaryStream;
  end
  else if (TypeNameLo = 'int2vector') or (TypeNameLo = 'oidvector') then
    Result := stAsciiStream
  else if (TypeNameLo <> '') and (P^ = '_') then // ARRAY TYPES
    Result := stAsciiStream
  else if (TypeNameLo = 'uuid') then
    Result := stGuid
  else if StartsWith(TypeNameLo, 'json') then
    Result := stAsciiStream
  else if StartsWith(TypeNameLo, 'xml') then
    Result := stAsciiStream
  else
    Result := stUnknown;

  if (Connection.GetConSettings.CPType = cCP_UTF16) then
    if Result = stAsciiStream then
      Result := stUnicodeStream;
end;

{**
   Another version of PostgreSQLToSQLType()
     - comparing integer should be faster than AnsiString.
   Return ZSQLType from PostgreSQL type name
   @param Connection a connection to PostgreSQL
   @param TypeOid is PostgreSQL type OID
   @return The ZSQLType type
}
function PostgreSQLToSQLType(const ConSettings: PZConSettings;
  const OIDAsBlob: Boolean; const TypeOid: Integer): TZSQLType; overload;
begin
  case TypeOid of
    INTERVALOID, CHAROID, BPCHAROID, VARCHAROID:  { interval/char/bpchar/varchar }
      if (ConSettings.CPType = cCP_UTF16) then
          Result := stUnicodeString
        else
          Result := stString;
    TEXTOID: Result := stAsciiStream; { text }
    OIDOID: if OidAsBlob
            then Result := stBinaryStream
            else Result := stLongWord;
    NAMEOID: Result := stString; { name }
    INT2OID: Result := stSmall; { int2 }
    INT4OID: Result := stInteger; { int4 }
    INT8OID: Result := stLong; { int8 }
    CIDROID: Result := stString; { cidr }
    INETOID: Result := stString; { inet }
    MACADDROID: Result := stString; { macaddr }
    FLOAT4OID: Result := stFloat; { float4 }
    FLOAT8OID, NUMERICOID: Result := stDouble; { float8/numeric. no 'decimal' any more }
    CASHOID: Result := stCurrency; { money }
    BOOLOID: Result := stBoolean; { bool }
    DATEOID: Result := stDate; { date }
    TIMEOID: Result := stTime; { time }
    TIMESTAMPOID, TIMESTAMPTZOID, ABSTIMEOID: Result := stTimestamp; { timestamp,timestamptz/abstime. no 'datetime' any more}
    BITOID, VARBITOID: Result := stString; {bit/ bit varying string}
    REGPROCOID: Result := stString; { regproc }
    1034: Result := stAsciiStream; {aclitem[]}
    BYTEAOID: { bytea }
      begin
        if OidAsBlob then
          Result := stBytes
        else
          Result := stBinaryStream;
      end;
    UUIDOID: Result := stGUID; {uuid}
    JSONOID, JSONBOID: Result := stAsciiStream;
    XMLOID: Result := stAsciiStream;
    INT2VECTOROID, OIDVECTOROID: Result := stAsciiStream; { int2vector/oidvector. no '_aclitem' }
    143,629,651,719,791,1000..OIDARRAYOID,1040,1041,1115,1182,1183,1185,1187,1231,1263,
    1270,1561,1563,2201,2207..2211,2949,2951,3643,3644,3645,3735,3770 : { other array types }
      Result := stAsciiStream;
    else
      Result := stUnknown;
  end;

  if (ConSettings.CPType = cCP_UTF16) then
    if Result = stAsciiStream then
      Result := stUnicodeStream;
end;

function SQLTypeToPostgreSQL(SQLType: TZSQLType; IsOidAsBlob: boolean): string;
begin
  case SQLType of
    stBoolean: Result := 'bool';
    stByte, stSmall, stInteger, stLong: Result := 'int';
    stFloat, stDouble, stBigDecimal: Result := 'numeric';
    stString, stUnicodeString, stAsciiStream, stUnicodeStream: Result := 'text';
    stDate: Result := 'date';
    stTime: Result := 'time';
    stTimestamp: Result := 'timestamp';
    stGuid: Result := 'uuid';
    stBinaryStream, stBytes:
      if IsOidAsBlob then
        Result := 'oid'
      else
        Result := 'bytea';
  end;
end;

{**
  Indicate what field type is a number (integer, float and etc.)
  @param  the SQLType field type value
  @result true if field type number
}
function IsNumber(Value: TZSQLType): Boolean;
begin
  Result := Value in [stByte, stSmall, stInteger, stLong,
    stFloat, stDouble, stBigDecimal];
end;

{**
  Encode string which probably consists of multi-byte characters.
  Characters ' (apostraphy), low value (value zero), and \ (back slash) are encoded.
  Since we have noticed that back slash is the second byte of some BIG5 characters
    (each of them is two bytes in length), we need a characterset aware encoding function.
  @param CharactersetCode the characterset in terms of enumerate code.
  @param Value the regular string.
  @return the encoded string.
}
function PGEscapeString(SrcBuffer: PAnsiChar; SrcLength: Integer;
    ConSettings: PZConSettings; Quoted: Boolean): RawByteString;
var
  I, LastState: Integer;
  DestLength: Integer;
  DestBuffer: PAnsiChar;

  function pg_CS_stat(stat: integer; character: integer;
          CharactersetCode: TZPgCharactersetType): integer;
  begin
    if character = 0 then
      stat := 0;

    case CharactersetCode of
      csUTF8, csUNICODE_PODBC:
        begin
          if (stat < 2) and (character >= $80) then
          begin
            if character >= $fc then
              stat := 6
            else if character >= $f8 then
              stat := 5
            else if character >= $f0 then
              stat := 4
            else if character >= $e0 then
              stat := 3
            else if character >= $c0 then
              stat := 2;
          end
          else
            if (stat > 2) and (character > $7f) then
              Dec(stat)
            else
              stat := 0;
        end;
  { Shift-JIS Support. }
      csSJIS:
        begin
      if (stat < 2)
        and (character > $80)
        and not ((character > $9f) and (character < $e0)) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;
  { Chinese Big5 Support. }
      csBIG5:
        begin
      if (stat < 2) and (character > $A0) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;
  { Chinese GBK Support. }
      csGBK:
        begin
      if (stat < 2) and (character > $7F) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;

  { Korian UHC Support. }
      csUHC:
        begin
      if (stat < 2) and (character > $7F) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;

  { EUC_JP Support }
      csEUC_JP:
        begin
      if (stat < 3) and (character = $8f) then { JIS X 0212 }
        stat := 3
      else
      if (stat <> 2)
        and ((character = $8e) or
        (character > $a0)) then { Half Katakana HighByte & Kanji HighByte }
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;

  { EUC_CN, EUC_KR, JOHAB Support }
      csEUC_CN, csEUC_KR, csJOHAB:
        begin
      if (stat < 2) and (character > $a0) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;
      csEUC_TW:
        begin
      if (stat < 4) and (character = $8e) then
        stat := 4
      else if (stat = 4) and (character > $a0) then
        stat := 3
      else if ((stat = 3) or (stat < 2)) and (character > $a0) then
        stat := 2
      else if stat = 2 then
        stat := 1
      else
        stat := 0;
        end;
        { Chinese GB18030 support.Added by Bill Huang <bhuang@redhat.com> <bill_huanghb@ybb.ne.jp> }
      csGB18030:
        begin
      if (stat < 2) and (character > $80) then
        stat := 2
      else if stat = 2 then
      begin
        if (character >= $30) and (character <= $39) then
          stat := 3
        else
          stat := 1;
      end
      else if stat = 3 then
      begin
        if (character >= $30) and (character <= $39) then
          stat := 1
        else
          stat := 3;
      end
      else
        stat := 0;
        end;
      else
      stat := 0;
    end;
    Result := stat;
  end;

begin
  DestBuffer := SrcBuffer; //safe entry
  DestLength := Ord(Quoted) shl 1;
  LastState := 0;
  for I := 1 to SrcLength do
  begin
    LastState := pg_CS_stat(LastState,integer(SrcBuffer^),
      TZPgCharactersetType(ConSettings.ClientCodePage.ID));
    if (PByte(SrcBuffer)^ in [Ord(#0), Ord(#39)]) or ((PByte(SrcBuffer)^ = Ord('\')) and (LastState = 0))
    then Inc(DestLength, 4)
    else Inc(DestLength);
    Inc(SrcBuffer);
  end;

  SrcBuffer := DestBuffer; //restore entry
  SetLength(Result, DestLength);
  DestBuffer := Pointer(Result);
  if Quoted then begin
    PByte(DestBuffer)^ := Ord(#39);
    Inc(DestBuffer);
  end;

  LastState := 0;
  for I := 1 to SrcLength do begin
    LastState := pg_CS_stat(LastState,integer(SrcBuffer^),
      TZPgCharactersetType(ConSettings.ClientCodePage.ID));
    if (PByte(SrcBuffer)^ in [Ord(#0), Ord(#39)]) or ((PByte(SrcBuffer)^ = Ord('\')) and (LastState = 0)) then begin
      PByte(DestBuffer)^ := Ord('\');
      PByte(DestBuffer+1)^ := Ord('0') + (Byte(SrcBuffer^) shr 6);
      PByte(DestBuffer+2)^ := Ord('0') + ((Byte(SrcBuffer^) shr 3) and $07);
      PByte(DestBuffer+3)^ := Ord('0') + (Byte(SrcBuffer^) and $07);
      Inc(DestBuffer, 4);
    end else begin
      DestBuffer^ := SrcBuffer^;
      Inc(DestBuffer);
    end;
    Inc(SrcBuffer);
  end;
  if Quoted then
    PByte(DestBuffer)^ := Ord(#39);
end;


{**
  add by Perger -> based on SourceForge:
  [ 1520587 ] Fix for 1484704: bytea corrupted on post when not using utf8,
  file: 1484704.patch

  Converts a binary string into escape PostgreSQL format.
  @param Value a binary stream.
  @return a string in PostgreSQL binary string escape format.
}
function EncodeBinaryString(SrcBuffer: PAnsiChar; Len: Integer; Quoted: Boolean = False): RawByteString;
var
  I: Integer;
  DestLength: Integer;
  DestBuffer: PAnsiChar;
begin
  DestBuffer := SrcBuffer; //save entry
  DestLength := Ord(Quoted) shl 1;
  for I := 1 to Len do
  begin
    if (Byte(SrcBuffer^) < 32) or (Byte(SrcBuffer^) > 126) or (PByte(SrcBuffer)^ in [Ord(#39), Ord('\')])
    then Inc(DestLength, 5)
    else Inc(DestLength);
    Inc(SrcBuffer);
  end;
  SrcBuffer := DestBuffer; //restore

  SetLength(Result, DestLength);
  DestBuffer := Pointer(Result);
  if Quoted then begin
    PByte(DestBuffer)^ := Ord(#39);
    Inc(DestBuffer);
  end;

  for I := 1 to Len do begin
    if (Byte(SrcBuffer^) < 32) or (Byte(SrcBuffer^) > 126) or (PByte(SrcBuffer)^ in [Ord(#39), Ord('\')]) then begin
      PByte(DestBuffer)^ := Ord('\');
      PByte(DestBuffer+1)^ := Ord('\');
      PByte(DestBuffer+2)^ := Ord('0') + (Byte(SrcBuffer^) shr 6);
      PByte(DestBuffer+3)^ := Ord('0') + ((Byte(SrcBuffer^) shr 3) and $07);
      PByte(DestBuffer+4)^ := Ord('0') + (Byte(SrcBuffer^) and $07);
      Inc(DestBuffer, 5);
    end else begin
      DestBuffer^ := SrcBuffer^;
      Inc(DestBuffer);
    end;
    Inc(SrcBuffer);
  end;
  if Quoted then
    DestBuffer^ := '''';
end;

{**
  Converts an string from escape PostgreSQL format.
  @param Value a string in PostgreSQL escape format.
  @return a regular string.
}
function DecodeString(const Value: RawByteString): RawByteString;
var
  SrcLength, DestLength: Integer;
  SrcBuffer, DestBuffer: PAnsiChar;
begin
  SrcLength := Length(Value);
  SrcBuffer := PAnsiChar(Value);
  SetLength(Result, SrcLength);
  DestLength := 0;
  DestBuffer := PAnsiChar(Result);

  while SrcLength > 0 do
  begin
    if SrcBuffer^ = '\' then
    begin
      Inc(SrcBuffer);
      if PByte(SrcBuffer)^ in [Ord('\'), Ord('''')] then begin
        DestBuffer^ := SrcBuffer^;
        Inc(SrcBuffer);
        Dec(SrcLength, 2);
      end else begin
        PByte(DestBuffer)^ := ((Byte(SrcBuffer[0]) - Ord('0')) shl 6)
          or ((Byte(SrcBuffer[1]) - Ord('0')) shl 3)
          or ((Byte(SrcBuffer[2]) - Ord('0')));
        Inc(SrcBuffer, 3);
        Dec(SrcLength, 4);
      end;
    end else begin
      DestBuffer^ := SrcBuffer^;
      Inc(SrcBuffer);
      Dec(SrcLength);
    end;
    Inc(DestBuffer);
    Inc(DestLength);
  end;
  SetLength(Result, DestLength);
end;

{**
  Checks for possible sql errors.
  @param Connection a reference to database connection to execute Rollback.
  @param PlainDriver a PostgreSQL plain driver.
  @param Handle a PostgreSQL connection reference.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
  //FirmOS 22.02.06
  @param ResultHandle the Handle to the Result
}
function CheckPostgreSQLError(const Connection: IZConnection;
  const PlainDriver: IZPostgreSQLPlainDriver; const Handle: PZPostgreSQLConnect;
  const LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  const ResultHandle: PZPostgreSQLResult): String;
var
   ErrorMessage: RawbyteString;
//FirmOS
   ConnectionLost: boolean;

   function GetMessage(const AMessage: RawByteString): String;
   begin
    if Assigned(Connection) then
      Result := Trim(Connection.GetConSettings^.ConvFuncs.ZRawToString(AMessage,
        Connection.GetConSettings^.ClientCodePage^.CP, Connection.GetConSettings^.CTRL_CP))
    else
      {$IFDEF UNICODE}
      Result := Trim(UTF8ToString(AMessage));
      {$ELSE}
        {$IFDEF DELPHI}
        Result := Trim(Utf8ToAnsi(AMessage));
        {$ELSE}
        Result := Trim(AMessage);
        {$ENDIF}
     {$ENDIF}
   end;
begin
  if Assigned(Handle) then
    ErrorMessage := PlainDriver.GetErrorMessage(Handle)
  else
    ErrorMessage := '';

  if ErrorMessage <> '' then
  begin
    if Assigned(ResultHandle) then
     Result := GetMessage(PlainDriver.GetResultErrorField(ResultHandle,PG_DIAG_SQLSTATE))
    else
      Result := '';
  end;

  if ErrorMessage <> '' then
  begin
    ConnectionLost := (PlainDriver.GetStatus(Handle) = CONNECTION_BAD);

    if Assigned(Connection) then begin
      DriverManager.LogError(LogCategory, Connection.GetConSettings^.Protocol, LogMessage,
        0, ErrorMessage);
    end else begin
      DriverManager.LogError(LogCategory, 'some PostgreSQL protocol', LogMessage,
        0, ErrorMessage);
    end;

    if ResultHandle <> nil then PlainDriver.PQclear(ResultHandle);

    if not ( ConnectionLost and ( LogCategory = lcUnprepStmt ) ) then
      if not (Result = '42P18') then
        raise EZSQLException.CreateWithStatus(Result,Format(SSQLError1, [GetMessage(ErrorMessage)]));
  end;
end;

{**
   Resolve problem with minor version in PostgreSql bettas
   @param Value a minor version string like "4betta2"
   @return a miror version number
}
function GetMinorVersion(const Value: string): Word;
var
  I: integer;
  Temp: string;
begin
  Temp := '';
  for I := 1 to Length(Value) do
    if CharInSet(Value[I], ['0'..'9']) then
      Temp := Temp + Value[I]
    else
      Break;
  Result := StrToIntDef(Temp, 0);
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function PGPrepareAnsiSQLParam(const Value: TZVariant; const ClientVarManager: IZClientVariantManager;
  const Connection: IZPostgreSQLConnection; ChunkSize: Cardinal;
  InParamType: TZSQLType; oidasblob, DateTimePrefix, QuotedNumbers: Boolean;
  ConSettings: PZConSettings): RawByteString;
var
  TempBlob: IZBlob;
  WriteTempBlob: IZPostgreSQLOidBlob;
  CharRec: TZCharRec;
  TempVar: TZVariant;
begin
  if ClientVarManager.IsNull(Value)  then
    Result := 'NULL'
  else case InParamType of
    stBoolean:
      Result := BoolStrsUpRaw[SoftVarManager.GetAsBoolean(Value)];
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stUlong, stLong,
    stFloat, stDouble, stCurrency, stBigDecimal:
      begin
        Result := ClientVarManager.GetAsRawByteString(Value);
        if QuotedNumbers then Result := #39+Result+#39;
      end;
    stBytes:
      Result := Connection.EncodeBinary(SoftVarManager.GetAsBytes(Value), True);
    stString, stUnicodeString: begin
        ClientVarManager.Assign(Value, TempVar);
        CharRec := ClientVarManager.GetAsCharRec(TempVar, ConSettings.ClientCodePage^.CP);
        Result := Connection.EscapeString(CharRec.P, CharRec.Len, True);
      end;
    stGuid: if Value.VType = vtBytes
            then Result := #39+GUIDToRaw(Value.VBytes)+#39
            else Result := #39+ClientVarManager.GetAsRawByteString(Value)+#39;
    stDate:
      if DateTimePrefix then
        Result := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True, '::date')
      else
        Result := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True);
    stTime:
      if DateTimePrefix then
        Result := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True, '::time')
      else
        Result := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True);
    stTimestamp:
      if DateTimePrefix then
        Result := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True, '::timestamp')
      else
        Result := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(Value),
          ConSettings^.WriteFormatSettings, True);
    stAsciiStream, stUnicodeStream, stBinaryStream:
      begin
        TempBlob := ClientVarManager.GetAsInterface(Value) as IZBlob;
        if not TempBlob.IsEmpty then
        begin
          case InParamType of
            stBinaryStream:
              if (Connection.IsOidAsBlob) or oidasblob then
              begin
                try
                  WriteTempBlob := TZPostgreSQLOidBlob.Create(Connection.GetPlainDriver, nil, 0,
                    Connection.GetConnectionHandle, 0, ChunkSize);
                  WriteTempBlob.WriteBuffer(TempBlob.GetBuffer, TempBlob.Length);
                  Result := IntToRaw(WriteTempBlob.GetBlobOid);
                finally
                  WriteTempBlob := nil;
                end;
              end
              else
                Result := Connection.EncodeBinary(TempBlob.GetBuffer, TempBlob.Length, True);
            stAsciiStream, stUnicodeStream:
              if TempBlob.IsClob then begin
                CharRec.P := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                Result := Connection.EscapeString(CharRec.P, TempBlob.Length, True)
              end else
                Result := Connection.EscapeString(GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                  TempBlob.Length, ConSettings));
          end; {case..}
        end
        else
          Result := 'NULL';
        TempBlob := nil;
      end; {if not TempBlob.IsEmpty then}
    else
      RaiseUnsupportedParameterTypeException(InParamType);
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}


{$ENDIF ZEOS_DISABLE_POSTGRESQL} //if set we have an empty unit
end.
