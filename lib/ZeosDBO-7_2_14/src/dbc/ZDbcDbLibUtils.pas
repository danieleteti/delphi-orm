{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                DBLib Utility Functions                  }
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

unit ZDbcDbLibUtils;

interface

{$I ZDbc.inc}

{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
uses Classes, SysUtils,
  ZVariant, ZDbcIntfs, ZPlainDBLibDriver, ZCompatibility, ZPlainDbLibConstants;

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertODBCToSqlType(FieldType: SmallInt; CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Converts a DBLib native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertTDSTypeToSqlType(const FieldType: TTDSType;
  const CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Convert string DBLib field type to SqlType
  @param string field type value
  @result the SqlType field type value
}
function ConvertDBLibTypeToSqlType(const {%H-}Value: string): TZSQLType;

{**
  Converts ZDBC SQL types into MS SQL native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToTDSType(FieldType: TZSQLType): TTDSType;

{**
  Converts ZDBC SQL types into MS SQL native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToDBLibTypeName(FieldType: TZSQLType): string;
function ConvertSqlTypeToFreeTDSTypeName(FieldType: TZSQLType): string;

{**
  Converts a DBLib nullability value into ZDBC TZColumnNullableType.
  @param DBLibNullability dblibc native nullability.
  @return a SQL TZColumnNullableType.
}
function ConvertDBLibNullability(DBLibNullability: Byte): TZColumnNullableType;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function PrepareSQLParameter(const Value: TZVariant; ParamType: TZSQLType;
  const ClientVarManager: IZClientVariantManager; ConSettings: PZConSettings;
  NChar: Boolean = False): RawByteString;

{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
implementation
{$IFNDEF ZEOS_DISABLE_DBLIB} //if set we have an empty unit

uses ZSysUtils, ZEncoding, ZDbcUtils, ZClasses, ZFastCode
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{**
  Converts an ODBC native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertODBCToSqlType(FieldType: SmallInt;
  CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  case FieldType of
    1{char}, 12{varchar}, -8{nchar}, -9{nvarchar}: Result := stString;
    -7{bit}: Result := stBoolean;
//Bug #889223, bug with tinyint on mssql
    -6: Result := stByte;
    -5: Result := stLong;
//    -6: Result := stSmall;
    5: Result := stSmall;
    4: Result := stInteger;
    2, 3, 6, 7, 8: Result := stDouble;
    11, 93: Result := stTimestamp;
    -1{text}, -10: Result{ntext} := stAsciiStream;
    -4{image}: Result := stBinaryStream;
    -2{binary},-3{varbinary}: Result := stBytes;
    -11{uniqueidentifier}: Result := stGUID;
  else
    Result := stUnknown;
  end;
  if CtrlsCPType = cCP_UTF16 then
  case Result of
    stString: Result := stUnicodeString;
    stAsciiStream: Result := stUnicodeStream;
  end;
end;

{**
  Converts a tabular data stream native types into ZDBC SQL types.
  @param FieldType dblibc native field type.
  @param CtrlsCPType the string code Page of the IDE Controls
  @return a SQL undepended type.
}
function ConvertTDSTypeToSqlType(const FieldType: TTDSType;
  const CtrlsCPType: TZControlsCodePage): TZSQLType;
begin
  case FieldType of
    tdsVoid, tdsUDT:
      Result := stUnknown; //Null columns
    tdsImage:
      Result := stBinaryStream;
    tdsText, tdsNText, tdsMSXML:
      if CtrlsCPType = cCP_UTF16 then
        Result := stUnicodeStream
      else
        Result := stAsciiStream;
    tdsUnique:
      Result := stGUID;
    tdsBinary, tdsVarBinary, tdsBigBinary, tdsBigVarBinary:
      Result := stBytes;
    tdsIntN:
      Result := stInteger;
    tdsVarchar, tdsNVarChar, tdsBigVarChar, tdsBigNVarChar:
      if CtrlsCPType = cCP_UTF16 then
        Result := stUnicodeString
      else
        Result := stString;
    tdsChar, tdsBigChar, tdsBigNChar:
      if CtrlsCPType = cCP_UTF16 then
        Result := stUnicodeString
      else
        Result := stString;
    tdsInt1:
      Result := stByte;
    tdsBit, tdsBitN:
      Result := stBoolean;
    tdsInt2:
      Result := stSmall;
    tdsInt4:
      Result := stInteger;
    tdsDateTime, tdsDateTimeN, tdsDateTime4:
      Result := stTimeStamp;
    tdsFlt4, tdsFltN:
      Result := stFloat;
    tdsMoney, tdsMoney4, tdsMoneyN:
      Result := stCurrency;
    tdsFlt8:
      Result := stDouble;
    tdsDecimal, tdsNumeric:
      Result := stDouble;
    //tdsVariant: {from tds.h -> sybase only -> na't test it}
    tdsInt8:
      Result := stLong;
    else
      Result := stUnknown;
  end;
end;

{**
  Convert string DBLib field type to SqlType
  @param string field type value
  @result the SqlType field type value
}
function ConvertDBLibTypeToSqlType(const Value: string): TZSQLType;
begin
  Result := stUnknown;
end;

{**
  Converts ZDBC SQL types into DBLib native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToDBLibTypeName(FieldType: TZSQLType): string;
begin
  Result := '';
  case FieldType of
    stBoolean: Result := 'bit';
    stByte: Result := 'tinyint';
    stSmall: Result := 'smallint';
    stInteger: Result := 'int';
    stLong: Result := 'bigint';
    stFloat: Result := 'float(24)';
    stDouble: Result := 'float(53)';
    stBigDecimal: Result := 'float(53)';
    stString: Result := 'varchar(8000)';
    stBytes: Result := 'varbinary(8000)';
    stDate: Result := 'datetime';
    stTime: Result := 'datetime';
    stTimestamp: Result := 'datetime';
    stAsciiStream: Result := 'text';
    stUnicodeStream: Result := 'ntext';
    stBinaryStream: Result := 'image';
  end;
end;

{**
  Converts ZDBC SQL types into FreeTDS native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToTDSType(FieldType: TZSQLType): TTDSType;
begin
  Result := tdsVoid;
  case FieldType of
    stBoolean: Result := tdsBit;
    stByte: Result := tdsInt1;
    stShort, stSmall: Result := tdsInt2;
    stWord, stInteger: Result := tdsInt4;
    stLongWord, stLong, stUlong: Result := tdsFlt8; //EH: Better would nbe tdsInt8
    stFloat: Result := tdsFlt4;
    stDouble, stBigDecimal: Result := tdsFlt8;
    stString, stUnicodeString: Result := tdsVarChar;
    stBytes: Result := tdsVarBinary;
    stDate, stTime, stTimestamp: Result := tdsDateTime;
    stAsciiStream, stUnicodeStream: Result := tdsText;
    stBinaryStream: Result := tdsImage;
  end;
end;

{**
  Converts ZDBC SQL types into FreeTDS native types.
  @param FieldType dblibc native field type.
  @return a SQL undepended type.
}
function ConvertSqlTypeToFreeTDSTypeName(FieldType: TZSQLType): string;
begin
  Result := '';
  case FieldType of
    stBoolean: Result := 'bit';
    stByte: Result := 'tinyint';
    stSmall: Result := 'smallint';
    stInteger: Result := 'int';
    stLong: Result := 'bigint';
    stFloat: Result := 'float(24)';
    stDouble: Result := 'float(53)';
    stBigDecimal: Result := 'float(53)';
    stString: Result := 'varchar(8000)';
    stUnicodeString: Result := 'nvarchar(4000)';
    stBytes: Result := 'varbinary(8000)';
    stDate: Result := 'datetime';
    stTime: Result := 'datetime';
    stTimestamp: Result := 'datetime';
    stAsciiStream: Result := 'text';
    stUnicodeStream: Result := 'ntext';
    stBinaryStream: Result := 'image';
  end;
end;


{**
  Converts a DBLib nullability value into ZDBC TZColumnNullableType.
  @param DBLibNullability dblibc native nullability.
  @return a SQL TZColumnNullableType.
}
function ConvertDBLibNullability(DBLibNullability: Byte): TZColumnNullableType;
const
  Nullability: array[0..2] of TZColumnNullableType =
    (ntNoNulls, ntNullable, ntNullableUnknown);
begin
  Result := Nullability[DBLibNullability];
end;

{**
  Prepares an SQL parameter for the query.
  @param ParameterIndex the first parameter is 1, the second is 2, ...
  @return a string representation of the parameter.
}
function PrepareSQLParameter(const Value: TZVariant; ParamType: TZSQLType;
  const ClientVarManager: IZClientVariantManager;
  ConSettings: PZConSettings; NChar: Boolean = False): RawByteString;
var
  TempBytes: TBytes;
  TempBlob: IZBlob;
begin
  TempBytes := nil;

  if SoftVarManager.IsNull(Value)
  then Result := 'NULL'
  else case ParamType of
    stBoolean:
      Result := BoolStrIntsRaw[ClientVarManager.GetAsBoolean(Value)];
    stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong,
    stFloat, stDouble, stCurrency, stBigDecimal:
      Result := ClientVarManager.GetAsRawByteString(Value);
    stString, stUnicodeString:
      if NChar
      then Result := ZSysUtils.SQLQuotedStr(ClientVarManager.GetAsRawByteString(Value, zCP_UTF8),AnsiChar(#39))
      else Result := ZSysUtils.SQLQuotedStr(ClientVarManager.GetAsRawByteString(Value), AnsiChar(#39));
    stBytes:
      begin
        TempBytes := ClientVarManager.GetAsBytes(Value);
        if Length(TempBytes) = 0 then
          Result := 'NULL'
        else
          Result := GetSQLHexAnsiString(PAnsiChar(TempBytes), Length(TempBytes), True);
      end;
    stGuid:
      begin
        TempBytes := ClientVarManager.GetAsBytes(Value);
        case Length(TempBytes) of
          0: Result := 'NULL';
          16: Result := ''''+GUIDToRaw(TempBytes)+'''';
          else EZSQLException.Create('The TBytes was not 16 bytes long when trying to convert it to a GUID');
        end;
      end;
    stDate:
      Result := DateTimeToRawSQLDate(ClientVarManager.GetAsDateTime(Value),
        ConSettings^.WriteFormatSettings, True);
    stTime:
      Result := DateTimeToRawSQLTime(ClientVarManager.GetAsDateTime(Value),
        ConSettings^.WriteFormatSettings, True);
    stTimestamp:
      Result := DateTimeToRawSQLTimeStamp(ClientVarManager.GetAsDateTime(Value),
        ConSettings^.WriteFormatSettings, True);
    stAsciiStream, stUnicodeStream, stBinaryStream:
      begin
        TempBlob := SoftVarManager.GetAsInterface(Value) as IZBlob;
        if not TempBlob.IsEmpty then
        begin
          if ParamType = stBinaryStream then
            Result := GetSQLHexAnsiString(PAnsiChar(TempBlob.GetBuffer), TempBlob.Length, True)
          else begin
            if TempBlob.IsClob then begin
              if NChar
              then TempBlob.GetPAnsiChar(zCP_UTF8)
              else TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage.CP);
              Result := SQLQuotedStr(PAnsiChar(TempBlob.GetBuffer), TempBlob.Length, AnsiChar(#39))
            end else
              if NChar then
                Result := SQLQuotedStr(
                  GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, ConSettings, zCP_UTF8), AnsiChar(#39))
              else
                Result := SQLQuotedStr(
                  GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, ConSettings), AnsiChar(#39));
            if ZFastCode.Pos(RawByteString(#0), Result) > 1
            then raise EZSQLException.Create('Character 0x00 is not allowed in Strings with Text and NText fields and this driver.');
          end;
        end else
          Result := 'NULL';
        TempBlob := nil;
      end;
    else
      Result := 'NULL';
  end;
end;
{$ENDIF ZEOS_DISABLE_DBLIB} //if set we have an empty unit
end.
