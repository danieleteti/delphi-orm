{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        Dataset utility functions and classes            }
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

unit ZDatasetUtils;

interface

{$I ZComponent.inc}

uses
  Types, Classes, SysUtils, {$IFDEF MSEgui}mclasses, mdb{$ELSE}Db{$ENDIF},
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs,{$ELSE}ZClasses,{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  ZDbcIntfs, ZDbcCache, ZCompatibility, ZExpression, ZVariant, ZTokenizer,
  ZSelectSchema;

{**
  Converts DBC Field Type to TDataset Field Type.
  @param Value an initial DBC field type.
  @return a converted TDataset field type.
}
function ConvertDbcToDatasetType(Value: TZSQLType): TFieldType;

{**
  Converts TDataset Field Type to DBC Field Type.
  @param Value an initial TDataset field type.
  @return a converted DBC field type.
}
function ConvertDatasetToDbcType(Value: TFieldType): TZSQLType;

{**
  Converts field definitions into column information objects.
  @param Fields a collection of field definitions.
  @return a collection of column information objects.
}
function ConvertFieldsToColumnInfo(Fields: TFields): TObjectList;

{**
  Fetches columns from specified resultset.
  @param ResultSet a source resultset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure FetchFromResultSet(ResultSet: IZResultSet;
  const FieldsLookupTable: TPointerDynArray; Fields: TFields;
  RowAccessor: TZRowAccessor);

{**
  Posts columns from specified resultset.
  @param ResultSet a source resultset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure PostToResultSet(ResultSet: IZResultSet;
  const FieldsLookupTable: TPointerDynArray; Fields: TFields;
  RowAccessor: TZRowAccessor);

{**
  Defines fields indices for the specified dataset.
  @param DataSet a dataset object.
  @param FieldNames a list of field names or field indices separated by ',' or ';'
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
function DefineFields(DataSet: TDataset; const FieldNames: string;
  out OnlyDataFields: Boolean; const Tokenizer: IZTokenizer): TObjectDynArray;

{**
  Defins a indices of filter fields.
  @param Dataset a dataset object.
  @param Expression a expression calculator.
  @returns an array with field object references.
}
function DefineFilterFields(DataSet: TDataset;
  Expression: IZExpression): TObjectDynArray;

{**
  Retrieves a set of specified field values.
  @param FieldRefs an array with interested field object references.
  @param ResultSet an initial result set object.
  @param ResultValues a container for result values.
  @return an array with field values.
}
procedure RetrieveDataFieldsFromResultSet(const FieldRefs: TObjectDynArray;
  ResultSet: IZResultSet; var ResultValues: TZVariantDynArray);

{**
  Retrieves a set of specified field values.
  @param FieldRefs an array with interested field object references.
  @param FieldIndices an array with interested field indices.
  @param RowAccessor a row accessor object.
  @param ResultValues a container for result values.
  @return an array with field values.
}
procedure RetrieveDataFieldsFromRowAccessor(const FieldRefs: TObjectDynArray;
  const FieldIndices: TIntegerDynArray; RowAccessor: TZRowAccessor;
  var ResultValues: TZVariantDynArray);

{**
  Copy a set of specified field values to variables.
  @param Fields an array with interested field object references.
  @param ResultSet an initial result set object.
  @param Variables a list of variables.
}
procedure CopyDataFieldsToVars(const Fields: TObjectDynArray;
  ResultSet: IZResultSet; Variables: IZVariablesList);

{**
  Prepares values for comparison by CompareFieldsFromResultSet.
  @param FieldRefs an array with interested field object references.
  @param DecodedKeyValues given values.
  @param ResultSet  a resultset to get field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
}
procedure PrepareValuesForComparison(const FieldRefs: TObjectDynArray;
  var DecodedKeyValues: TZVariantDynArray; ResultSet: IZResultSet;
  PartialKey: Boolean; CaseInsensitive: Boolean);

{**
  Compares row field values with the given ones.
  @param KeyValues given values.
  @param RowValues row field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
  @return <code> if values are equal.
}
function CompareDataFields(const KeyValues, RowValues: TZVariantDynArray;
  PartialKey: Boolean; CaseInsensitive: Boolean): Boolean;

{**
  Compares row field values with the given ones.
  @param FieldRefs an array with interested field object references.
  @param KeyValues given values.
  @param RowValues row field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
  @return <code> if values are equal.
}
function CompareFieldsFromResultSet(const FieldRefs: TObjectDynArray;
  const KeyValues: TZVariantDynArray; ResultSet: IZResultSet; PartialKey: Boolean;
  CaseInsensitive: Boolean): Boolean;

{**
  Defines a list of key field names.
  @param Fields a collection of dataset fields.
  @param IZIdentifierConvertor IdentifierConverter for the used database
  @return a list of key field names.
}
function DefineKeyFields(Fields: TFields; const IdConverter: IZIdentifierConvertor): string;

{**
  Converts datetime value into TDataset internal presentation.
  @param DataType a type of date-time field.
  @param Data a data which contains a value.
  @param Buffer a field buffer pointer
}
procedure DateTimeToNative(DataType: TFieldType; Data: TDateTime; Buffer: Pointer);

{**
  Converts date times from TDataset internal presentation into datetime value.
  @param DataType a type of date-time field.
  @param Buffer a field buffer pointer
  @return a data which contains a value.
}
function NativeToDateTime(DataType: TFieldType; Buffer: Pointer): TDateTime;

{**
  Compare values from two key fields.
  @param Field1 the first field object.
  @param ResultSet the resultset to read the first field value.
  @param Field2 the second field object.
}
function CompareKeyFields(Field1: TField; ResultSet: IZResultSet;
  Field2: TField): Boolean;

{**
  Defins a indices and directions for sorted fields.
  @param Dataset a dataset object.
  @param SortedFields an encoded fields for sorting in the format
    <Field Name> [ASC | DESC] [, ...]
  @param FieldRefs a decoded field object references.
  @param FieldDirs a decoded field directions.
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
procedure DefineSortedFields(DataSet: TDataset;
  const SortedFields: string; out FieldRefs: TObjectDynArray;
  out CompareKinds: TComparisonKindArray; out OnlyDataFields: Boolean);

{**
  Creates a fields lookup table to define fixed position
  of the field in dataset.
  @param Fields a collection of TDataset fields in initial order.
  @returns a fields lookup table.
}
function CreateFieldsLookupTable(Fields: TFields): TPointerDynArray;

{**
  Defines an original field index in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Field a TDataset field object.
  @returns an original fields index or -1 otherwise.
}
function DefineFieldIndex(const FieldsLookupTable: TPointerDynArray;
  Field: TField): Integer;

{**
  Defines an original field indices in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param FieldRefs a TDataset field object references.
  @returns an array with original fields indices.
}
function DefineFieldIndices(const FieldsLookupTable: TPointerDynArray;
  const FieldRefs: TObjectDynArray): TIntegerDynArray;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(QualifiedName: string;
  out Catalog, Schema, ObjectName: string); overload;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(QualifiedName: string;
  const SupportsCatalogs, SupportsSchemas: Boolean;
  out Catalog, Schema, ObjectName: string); overload;

{**
  Assigns a Statement value from a TParam
  @param Index the index of Statement.SetParam(Idex..);
  @param Statement the PrepredStatement where the values have been assigned
  @param Param the TParam where the value is assigned from
}
procedure SetStatementParam(Index: Integer;
  Statement: IZPreparedStatement; Param: TParam);

const ProcColDbcToDatasetType: array[TZProcedureColumnType] of TParamType =
  (ptUnknown{pctUnknown}, ptInput{pctIn}, ptInputOutput{pctInOut},
   ptOutPut{pctOut}, ptResult{pctReturn}, ptResult{pctResultSet});
const DatasetTypeToProcColDbc: array[TParamType] of TZProcedureColumnType =
  (pctUnknown{ptUnknown}, pctIn{ptInput}, pctOut{ptOutPut},
    pctInOut{ptInputOutput}, pctReturn{ptResult});

{** Common variables. }
var
  CommonTokenizer: IZTokenizer;

implementation

uses
  ZFastCode, ZMessages, ZGenericSqlToken, ZDbcResultSetMetadata, ZAbstractRODataset,
  ZSysUtils, ZDbcResultSet;

{**
  Converts DBC Field Type to TDataset Field Type.
  @param Value an initial DBC field type.
  @return a converted TDataset field type.
}
function ConvertDbcToDatasetType(Value: TZSQLType): TFieldType;
begin
  case Value of
    stBoolean:
      Result := ftBoolean;
    stByte:
      Result := {$IFDEF WITH_FTBYTE}ftByte{$ELSE}ftSmallInt{$ENDIF}; // ! dangerous - field will get a type with greater size
    stShort:
      Result := {$IFDEF WITH_FTSHORTINT}ftShortint{$ELSE}ftSmallInt{$ENDIF}; // !
    stSmall:
      Result := ftSmallInt;
    stWord:
      Result := ftWord;
    stInteger:
      Result := ftInteger;
    stLongWord:
      Result := {$IFDEF WITH_FTLONGWORD}ftLongWord{$ELSE}ftLargeInt{$ENDIF}; // !
    stLong, stULong:
      Result := ftLargeInt;
    {$IFDEF WITH_FTSINGLE}
    stFloat:
      Result := ftSingle;
    {$ENDIF}
    {$IFDEF WITH_FTEXTENDED}
    stBigDecimal:
      Result := ftExtended;
    {$ENDIF}
    {$IFNDEF WITH_FTSINGLE}stFloat,{$ENDIF}
    stDouble
    {$IFNDEF WITH_FTEXTENDED},stBigDecimal{$ENDIF}:
      Result := ftFloat;
    stCurrency:
      //Result := ftCurrency;
      Result := ftBCD;
    stString:
      Result := ftString;
    stBytes{$IFNDEF WITH_FTGUID}, stGUID{$ENDIF}:
      Result := ftBytes;
    {$IFDEF WITH_FTGUID}
    stGUID:
      Result := ftGUID;
    {$ENDIF}
    stDate:
      Result := ftDate;
    stTime:
      Result := ftTime;
    stTimestamp:
      Result := ftDateTime;
    stAsciiStream:
      Result := ftMemo;
    stBinaryStream:
      Result := ftBlob;
    stUnicodeString:
      Result := ftWideString;
    stUnicodeStream:
      Result := {$IFNDEF WITH_WIDEMEMO}ftWideString{$ELSE}ftWideMemo{$ENDIF};
    {$IFDEF WITH_FTDATASETSUPPORT}
    stDataSet:
      Result := ftDataSet;
    {$ENDIF}
    stArray:
      Result := ftArray;
    else
      Result := ftUnknown;
  end;
end;

{**
  Converts TDataset Field Type to DBC Field Type.
  @param Value an initial TDataset field type.
  @return a converted DBC field type.
}
function ConvertDatasetToDbcType(Value: TFieldType): TZSQLType;
begin
  case Value of
    ftBoolean:
      Result := stBoolean;
    {$IFDEF WITH_FTBYTE}
    ftByte:
      Result := stByte;
    {$ENDIF}
    {$IFDEF WITH_FTSHORTINT}
    ftShortInt:
      Result := stShort;
    {$ENDIF}
    ftWord:
      Result := stWord;
    ftSmallInt:
      Result := stSmall;
    ftInteger, ftAutoInc:
      Result := stInteger;
    {$IFDEF WITH_FTLONGWORD}
    ftLongWord:
      Result := stLongWord;
    {$ENDIF}
    {$IFDEF WITH_FTSINGLE}
    ftSingle:
      Result := stFloat;
    {$ENDIF}
    ftFloat:
      Result := stDouble;
    {$IFDEF WITH_FTEXTENDED}
    ftExtended:
      Result := stBigDecimal;
    {$ENDIF}
    ftLargeInt:
      Result := stLong;
    ftCurrency, ftBCD:
      Result := stCurrency;
    ftString:
      Result := stString;
    ftBytes, ftVarBytes:
      Result := stBytes;
    ftDate:
      Result := stDate;
    ftTime:
      Result := stTime;
    ftDateTime:
      Result := stTimestamp;
    ftMemo:
      Result := stAsciiStream;
    ftBlob, ftGraphic:
      Result := stBinaryStream;
    ftWideString:
      Result := stUnicodeString;
    {$IFDEF WITH_FTGUID}
    ftGuid:
      Result := stGUID;
    {$ENDIF}
    {$IFDEF WITH_WIDEMEMO}
    ftWideMemo:
      Result := stUnicodeStream;
    {$ENDIF}
    {$IFDEF WITH_FTDATASETSUPPORT}
    ftDataSet:
      Result := stDataSet;
    {$ENDIF}
    ftArray:
      Result := stArray;
    else
      Result := stUnknown;
  end;
end;

{**
  Converts field definitions into column information objects.
  @param Fields a collection of field definitions.
  @return a collection of column information objects.
}
function ConvertFieldsToColumnInfo(Fields: TFields): TObjectList;
var
  I: Integer;
  Current: TField;
  ColumnInfo: TZColumnInfo;
begin
  Result := TObjectList.Create(True);
  for I := 0 to Fields.Count - 1 do
  begin
    Current := Fields[I];
    ColumnInfo := TZColumnInfo.Create;

    ColumnInfo.ColumnType := ConvertDatasetToDbcType(Current.DataType);
    ColumnInfo.ColumnName := Current.FieldName;
    ColumnInfo.Precision := Current.Size;
    ColumnInfo.Scale := 0;
    ColumnInfo.ColumnLabel := Current.DisplayName;
    ColumnInfo.ColumnDisplaySize := Current.DisplayWidth;
    ColumnInfo.DefaultExpression := Current.DefaultExpression;

    Result.Add(ColumnInfo);
  end;
end;

{**
  Fetches columns from specified resultset.
  @param ResultSet a source resultset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure FetchFromResultSet(ResultSet: IZResultSet;
  const FieldsLookupTable: TPointerDynArray; Fields: TFields;
  RowAccessor: TZRowAccessor);
var
  I, FieldIndex: Integer;
  Current: TField;
  ColumnIndex, ColumnCount: Integer;
  Len: NativeUInt;
begin
  RowAccessor.RowBuffer.Index := ResultSet.GetRow;
  ColumnCount := ResultSet.GetMetadata.GetColumnCount;

  for I := 0 to Fields.Count - 1 do
  begin
    Current := Fields[I];
    if not (Current.FieldKind in [fkData, fkInternalCalc]) then
      Continue;

    ColumnIndex := Current.FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
    FieldIndex := DefineFieldIndex(FieldsLookupTable, Current);
    if (ColumnIndex < FirstDbcIndex) or (ColumnIndex > ColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
      Continue;

    case Current.DataType of
      ftBoolean:
        RowAccessor.SetBoolean(FieldIndex, ResultSet.GetBoolean(ColumnIndex));
      {$IFDEF WITH_FTBYTE}
      ftByte:
        RowAccessor.SetByte(FieldIndex, ResultSet.GetByte(ColumnIndex));
      {$ENDIF}
      {$IFDEF WITH_FTSHORTINT}
      ftShortInt:
        RowAccessor.SetShort(FieldIndex, ResultSet.GetShort(ColumnIndex));
      {$ENDIF}
      ftWord:
        RowAccessor.SetWord(FieldIndex, ResultSet.GetWord(ColumnIndex));
      ftSmallInt:
        RowAccessor.SetSmall(FieldIndex, ResultSet.GetSmall(ColumnIndex));
      {$IFDEF WITH_FTLONGWORD}
      ftLongWord:
        RowAccessor.SetUInt(FieldIndex, ResultSet.GetUInt(ColumnIndex));
      {$ENDIF}
      ftInteger, ftAutoInc:
        RowAccessor.SetInt(FieldIndex, ResultSet.GetInt(ColumnIndex));
      {$IFDEF WITH_FTSINGLE}
      ftSingle:
        RowAccessor.SetFloat(FieldIndex, ResultSet.GetFloat(ColumnIndex));
      {$ENDIF}
      ftFloat:
        RowAccessor.SetDouble(FieldIndex, ResultSet.GetDouble(ColumnIndex));
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
        RowAccessor.SetBigDecimal(FieldIndex, ResultSet.GetBigDecimal(ColumnIndex));
      {$ENDIF}
      ftLargeInt:
        RowAccessor.SetLong(FieldIndex, ResultSet.GetLong(ColumnIndex));
      ftCurrency, ftBCD:
        RowAccessor.SetCurrency(FieldIndex, ResultSet.GetCurrency(ColumnIndex));
      ftString, ftWideString:
        if RowAccessor.IsRaw then
          RowAccessor.SetPAnsiChar(FieldIndex, ResultSet.GetPAnsiChar(ColumnIndex, Len), @Len)
        else
          RowAccessor.SetPWideChar(FieldIndex, ResultSet.GetPWideChar(ColumnIndex, Len), @Len);
      ftBytes, ftVarBytes{$IFDEF WITH_FTGUID}, ftGuid{$ENDIF}:
        RowAccessor.SetBytes(FieldIndex, ResultSet.GetBytes(ColumnIndex));
      ftDate:
        RowAccessor.SetDate(FieldIndex, ResultSet.GetDate(ColumnIndex));
      ftTime:
        RowAccessor.SetTime(FieldIndex, ResultSet.GetTime(ColumnIndex));
      ftDateTime:
        RowAccessor.SetTimestamp(FieldIndex, ResultSet.GetTimestamp(ColumnIndex));
      ftMemo, ftBlob, ftGraphic {$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}:
        RowAccessor.SetBlob(FieldIndex, ResultSet.GetBlob(ColumnIndex));
      {$IFDEF WITH_FTDATASETSUPPORT}
      ftDataSet:
        RowAccessor.SetDataSet(FieldIndex, ResultSet.GetDataSet(ColumnIndex));
      {$ENDIF}
    end;

    if ResultSet.WasNull then
      RowAccessor.SetNull(FieldIndex);
  end;
end;

{**
  Posts columns from specified resultset.
  @param ResultSet a source resultset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Fields a collection of field definitions.
  @param RowAccessor a destination row accessor.
}
procedure PostToResultSet(ResultSet: IZResultSet;
  const FieldsLookupTable: TPointerDynArray; Fields: TFields;
  RowAccessor: TZRowAccessor);
var
  I, FieldIndex: Integer;
  Current: TField;
  WasNull: Boolean;
  ColumnIndex, ColumnCount: Integer;
  Blob: IZBlob;
  Len: NativeUInt;
begin
  WasNull := False;
  RowAccessor.RowBuffer.Index := ResultSet.GetRow;
  ColumnCount := ResultSet.GetMetadata.GetColumnCount;

  for I := 0 to Fields.Count - 1 do
  begin
    Current := Fields[I];
    if Current.FieldKind <> fkData then
      Continue;

    ColumnIndex := Current.FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
    FieldIndex := DefineFieldIndex(FieldsLookupTable, Current);
    if (ColumnIndex < FirstDbcIndex) or (ColumnIndex > ColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF}) then
      Continue;

//    if (Current.Required = True) and (WasNull = True) then
//      raise EZDatabaseError.Create(Format(SFieldCanNotBeNull, [Current.FieldName]));
    case Current.DataType of
      ftBoolean:
        ResultSet.UpdateBoolean(ColumnIndex, RowAccessor.GetBoolean(FieldIndex, WasNull));
      {$IFDEF WITH_FTBYTE}
      ftByte:
        ResultSet.UpdateByte(ColumnIndex, RowAccessor.GetByte(FieldIndex, WasNull));
      {$ENDIF}
      {$IFDEF WITH_FTSHORTINT}
      ftShortInt:
        ResultSet.UpdateShort(ColumnIndex, RowAccessor.GetShort(FieldIndex, WasNull));
      {$ENDIF}
      ftWord:
        ResultSet.UpdateWord(ColumnIndex, RowAccessor.GetWord(FieldIndex, WasNull));
      ftSmallInt:
        ResultSet.UpdateSmall(ColumnIndex, RowAccessor.GetSmall(FieldIndex, WasNull));
      {$IFDEF WITH_FTLONGWORD}
      ftLongWord:
        ResultSet.UpdateUInt(ColumnIndex, RowAccessor.GetUInt(FieldIndex, WasNull));
      {$ENDIF}
      ftInteger, ftAutoInc:
        ResultSet.UpdateInt(ColumnIndex, RowAccessor.GetInt(FieldIndex, WasNull));
      {$IFDEF WITH_FTSINGLE}
      ftSingle:
        ResultSet.UpdateFloat(ColumnIndex, RowAccessor.GetFloat(FieldIndex, WasNull));
      {$ENDIF}
      ftFloat:
        ResultSet.UpdateDouble(ColumnIndex, RowAccessor.GetDouble(FieldIndex, WasNull));
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
        ResultSet.UpdateBigDecimal(ColumnIndex, RowAccessor.GetBigDecimal(FieldIndex, WasNull));
      {$ENDIF}
      ftLargeInt:
        ResultSet.UpdateLong(ColumnIndex, RowAccessor.GetLong(FieldIndex, WasNull));
      ftCurrency, ftBCD:
        ResultSet.UpdateCurrency(ColumnIndex,
          RowAccessor.GetCurrency(FieldIndex, WasNull));
      ftString, ftWidestring:
        if RowAccessor.IsRaw then
          ResultSet.UpdatePAnsiChar(ColumnIndex,
            RowAccessor.GetPAnsiChar(FieldIndex, WasNull, Len), @Len)
        else
          ResultSet.UpdatePWideChar(ColumnIndex,
            RowAccessor.GetPWideChar(FieldIndex, WasNull, Len), @Len);
      ftBytes, ftVarBytes{$IFDEF WITH_FTGUID}, ftGuid{$ENDIF}:
        ResultSet.UpdateBytes(ColumnIndex, RowAccessor.GetBytes(FieldIndex, WasNull));
      ftDate:
        ResultSet.UpdateDate(ColumnIndex, RowAccessor.GetDate(FieldIndex, WasNull));
      ftTime:
        ResultSet.UpdateTime(ColumnIndex, RowAccessor.GetTime(FieldIndex, WasNull));
      ftDateTime:
        ResultSet.UpdateTimestamp(ColumnIndex,
          RowAccessor.GetTimestamp(FieldIndex, WasNull));
      {$IFDEF WITH_WIDEMEMO}
      ftWideMemo,
      {$ENDIF}
      ftMemo, ftBlob, ftGraphic:
        begin
          Blob := RowAccessor.GetBlob(FieldIndex, WasNull);
          WasNull := (Blob = nil) or (Blob.IsEmpty); //need a check for IsEmpty too
          ResultSet.UpdateLob(ColumnIndex, Blob);
        end;
      {$IFDEF WITH_FTDATASETSUPPORT}
      ftDataSet:
        ResultSet.UpdateDataSet(ColumnIndex, RowAccessor.GetDataSet(FieldIndex, WasNull));
      {$ENDIF}
    end;

    if WasNull then
    begin
      // Performance thing :
      // The default expression will only be set when necessary : if the value really IS null
      Resultset.UpdateDefaultExpression(ColumnIndex, RowAccessor.GetColumnDefaultExpression(FieldIndex));
      ResultSet.UpdateNull(ColumnIndex);
    end;
  end;
end;

{**
  Defines fields indices for the specified dataset.
  @param DataSet a dataset object.
  @param FieldNames a list of field names.
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
function DefineFields(DataSet: TDataset; const FieldNames: string;
  out OnlyDataFields: Boolean; const Tokenizer: IZTokenizer): TObjectDynArray;
var
  I, TokenValueInt: Integer;
  Tokens: TStrings;
  TokenType: TZTokenType;
  TokenValue: string;
  Field: TField;
  FieldCount: Integer;
begin
  OnlyDataFields := True;
  FieldCount := 0;
  SetLength(Result, FieldCount);
  Tokens := Tokenizer.TokenizeBufferToList(FieldNames,
    [toSkipEOF, toSkipWhitespaces, toUnifyNumbers, toDecodeStrings]);

  try
    for I := 0 to Tokens.Count - 1 do
    begin
      TokenType := TZTokenType({$IFDEF oldFPC}Pointer{$ENDIF}(Tokens.Objects[I]));
      TokenValue := Tokens[I];
      Field := nil;

      case TokenType of
        ttQuoted, ttQuotedIdentifier, ttWord:
          Field := DataSet.FieldByName(TokenValue); // Will raise exception if field not present
        ttNumber:
          begin
            TokenValueInt := StrToInt(TokenValue);
            // Tokenizer always returns numbers > 0
            if TokenValueInt >= Dataset.Fields.Count then
              raise EZDatabaseError.CreateFmt(SFieldNotFound2, [TokenValueInt]);
            Field := Dataset.Fields[TokenValueInt];
          end;
        ttSymbol:
          if (TokenValue <> ',') and (TokenValue <> ';') then
            raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [TokenValue]);
        else
          raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [TokenValue]);
      end;

      if Field <> nil then
      begin
        OnlyDataFields := OnlyDataFields and (Field.FieldKind = fkData);
        Inc(FieldCount);
        SetLength(Result, FieldCount);
        Result[FieldCount - 1] := Field;
      end;
    end;
  finally
    Tokens.Free;
  end;
end;

{**
  Defins a indices of filter fields.
  @param Dataset a dataset object.
  @param Expression a expression calculator.
  @returns an array with field object references.
}
function DefineFilterFields(DataSet: TDataset;
  Expression: IZExpression): TObjectDynArray;
var
  I: Integer;
  Current: TField;
begin
  if Expression.Expression <> '' then
  begin
    SetLength(Result, Expression.DefaultVariables.Count);
    for I := 0 to Expression.DefaultVariables.Count - 1 do
    begin
      Current := DataSet.FindField(Expression.DefaultVariables.Names[I]);
      if Current <> nil then
        Result[I] := Current
      else
        Result[I] := nil;
    end;
  end
  else
    SetLength(Result, 0);
end;

{**
  Retrieves a set of specified field values.
  @param FieldRefs an array with interested field object references.
  @param ResultSet an initial result set object.
  @param ResultValues a container for result values.
  @return an array with field values.
}
procedure RetrieveDataFieldsFromResultSet(const FieldRefs: TObjectDynArray;
  ResultSet: IZResultSet; var ResultValues: TZVariantDynArray);
var
  I, ColumnIndex: Integer;
begin
  for I := 0 to High(FieldRefs) do
  begin
    ColumnIndex := TField(FieldRefs[I]).FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
    if ColumnIndex >= 0 then
    begin
      case TField(FieldRefs[I]).DataType of
        ftString:
          ResultValues[I] := EncodeString(ResultSet.GetString(ColumnIndex));
        ftBoolean:
          ResultValues[I] := EncodeBoolean(ResultSet.GetBoolean(ColumnIndex));
        {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}{$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
        ftWord, ftSmallInt, ftInteger, ftAutoInc:
          ResultValues[I] := EncodeInteger(ResultSet.GetInt(ColumnIndex));
        {$IFDEF WITH_FTSINGLE}ftSingle,{$ENDIF}
        ftFloat,
        ftCurrency, ftBCD
        {$IFDEF WITH_FTEXTENDED},ftExtended{$ENDIF}:
          ResultValues[I] := EncodeFloat(ResultSet.GetBigDecimal(ColumnIndex));
        {$IFDEF WITH_FTLONGWORD}ftLongword,{$ENDIF}ftLargeInt:
          ResultValues[I] := EncodeInteger(ResultSet.GetLong(ColumnIndex));
        ftDate, ftTime, ftDateTime:
          ResultValues[I] := EncodeDateTime(ResultSet.GetTimestamp(ColumnIndex));
        ftWidestring{$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}:
          ResultValues[I] := EncodeUnicodeString(ResultSet.GetUnicodeString(ColumnIndex));
        ftBytes, ftVarBytes, ftBlob, ftGraphic:
          ResultValues[I] := EncodeBytes(ResultSet.GetBytes(ColumnIndex));
        else
          ResultValues[I] := EncodeString(ResultSet.GetString(ColumnIndex));
      end;
      if ResultSet.WasNull then
        ResultValues[I] := NullVariant;
    end
    else
      ResultValues[I] := NullVariant;
  end;
end;

{**
  Retrieves a set of specified field values.
  @param FieldRefs an array with interested field object references.
  @param FieldIndices an array with interested field indices.
  @param RowAccessor a row accessor object.
  @param ResultValues a container for result values.
  @return an array with field values.
}
procedure RetrieveDataFieldsFromRowAccessor(const FieldRefs: TObjectDynArray;
  const FieldIndices: TIntegerDynArray; RowAccessor: TZRowAccessor;
  var ResultValues: TZVariantDynArray);
var
  I: Integer;
  ColumnIndex: Integer;
  WasNull: Boolean;
begin
  WasNull := False;
  for I := 0 to High(FieldRefs) do
  begin
    ColumnIndex := FieldIndices[I];
    case TField(FieldRefs[I]).DataType of
      ftString, ftMemo:
        ResultValues[I] := EncodeString(RowAccessor.GetString(ColumnIndex, WasNull));
      ftBoolean:
        ResultValues[I] := EncodeBoolean(RowAccessor.GetBoolean(ColumnIndex, WasNull));
      {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}{$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
      ftWord, ftSmallInt, ftInteger, ftAutoInc:
        ResultValues[I] := EncodeInteger(RowAccessor.GetInt(ColumnIndex, WasNull));
      {$IFDEF WITH_FTSINGLE}ftSingle,{$ENDIF}
      ftFloat, ftCurrency, ftBCD
      {$IFDEF WITH_FTEXTENDED},ftExtended{$ENDIF}:
        ResultValues[I] := EncodeFloat(RowAccessor.GetBigDecimal(ColumnIndex, WasNull));
      {$IFDEF WITH_FTLONGWORD}ftLongword,{$ENDIF}ftLargeInt:
        ResultValues[I] := EncodeInteger(RowAccessor.GetLong(ColumnIndex, WasNull));
      ftDate, ftTime, ftDateTime:
        ResultValues[I] := EncodeDateTime(RowAccessor.GetTimestamp(ColumnIndex, WasNull));
      ftWidestring{$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}:
        ResultValues[I] := EncodeUnicodeString(RowAccessor.GetUnicodeString(ColumnIndex, WasNull));
      ftBytes, ftVarBytes:
        ResultValues[I] := EncodeBytes(RowAccessor.GetBytes(ColumnIndex, WasNull));
      else
        ResultValues[I] := EncodeString(RowAccessor.GetString(ColumnIndex, WasNull));
    end;
    if WasNull then
      ResultValues[I] := NullVariant;
  end;
end;

{**
  Copy a set of specified field values to variables.
  @param Fields an array with interested field object references.
  @param ResultSet an initial result set object.
  @param Variables a list of variables.
}
procedure CopyDataFieldsToVars(const Fields: TObjectDynArray;
  ResultSet: IZResultSet; Variables: IZVariablesList);
var
  I, ColumnIndex: Integer;
  procedure CopyFromField;
  begin
    if TField(Fields[I]).IsNull then
      Variables.Values[I] := NullVariant
    else case TField(Fields[I]).DataType of
      ftBoolean:
        Variables.Values[I] := EncodeBoolean(TField(Fields[I]).AsBoolean);
      {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}{$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
      ftWord, ftSmallInt, ftInteger, ftAutoInc:
        Variables.Values[I] := EncodeInteger(TField(Fields[I]).AsInteger);
      {$IFDEF WITH_FTSINGLE}
      ftSingle,
      {$ENDIF}
      ftCurrency,
      ftFloat:
        Variables.Values[I] := EncodeFloat(TField(Fields[I]).AsFloat);
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
        Variables.Values[I] := EncodeFloat(TField(Fields[I]).AsExtended);
      {$ENDIF}
      {$IFDEF WITH_FTLONGWORD}ftLongword,{$ENDIF}ftLargeInt:
        Variables.Values[I] := EncodeInteger(ResultSet.GetLong(ColumnIndex));
      ftBCD:
        Variables.Values[I] := EncodeFloat(ResultSet.GetCurrency(ColumnIndex));
      ftFmtBCD: Variables.Values[I] := EncodeFloat(TField(Fields[I]).AsFloat);
      ftDate, ftTime, ftDateTime:
        Variables.Values[I] := EncodeDateTime(TField(Fields[I]).AsDateTime);
      //ftString, ftMemo:
        //Variables.Values[I] := EncodeString(TField(Fields[I]).AsString);
    {$IFNDEF UNICODE}
      {$IFDEF WITH_FTWIDESTRING}
      ftWidestring{$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}:
        Variables.Values[I] := EncodeUnicodeString(TField(Fields[I]).AsWideString);
      {$ENDIF}
    {$ENDIF}
      ftBytes, ftVarBytes:
        {$IFDEF TFIELD_HAS_ASBYTES}
        Variables.Values[I] := EncodeBytes(TField(Fields[I]).AsBytes);
        {$ELSE}
        Variables.Values[I] := EncodeBytes(VarToBytes(TField(Fields[I]).AsVariant));
        {$ENDIF}
      ftArray, ftDataSet: raise EZDatabaseError.Create(SDataTypeDoesNotSupported)
      else Variables.Values[I] := EncodeString(TField(Fields[I]).AsString);
    end;
  end;
begin
  for I := 0 to High(Fields) do begin
    if Fields[I] = nil then
      Continue;

    ColumnIndex := TField(Fields[I]).FieldNo {$IFDEF GENERIC_INDEX}-1{$ENDIF};
    if ColumnIndex = -1 then
      CopyFromField
    else if not ResultSet.IsNull(ColumnIndex) then
      case TField(Fields[I]).DataType of
        ftBoolean:
          Variables.Values[I] := EncodeBoolean(ResultSet.GetBoolean(ColumnIndex));
        {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}{$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
        ftWord, ftSmallInt, ftInteger, ftAutoInc:
          Variables.Values[I] := EncodeInteger(ResultSet.GetInt(ColumnIndex));
        {$IFDEF WITH_FTSINGLE}
        ftSingle:
          Variables.Values[I] := EncodeFloat(ResultSet.GetFloat(ColumnIndex));
        {$ENDIF}
        ftFloat:
          Variables.Values[I] := EncodeFloat(ResultSet.GetDouble(ColumnIndex));
        {$IFDEF WITH_FTEXTENDED}
        ftExtended:
          Variables.Values[I] := EncodeFloat(ResultSet.GetBigDecimal(ColumnIndex));
        {$ENDIF}
        ftFmtBCD:
          Variables.Values[I] := EncodeFloat(ResultSet.GetBigDecimal(ColumnIndex));
        {$IFDEF WITH_FTLONGWORD}ftLongword,{$ENDIF}ftLargeInt:
          Variables.Values[I] := EncodeInteger(ResultSet.GetLong(ColumnIndex));
        ftCurrency, ftBCD:
          Variables.Values[I] := EncodeFloat(ResultSet.GetCurrency(ColumnIndex));
        ftDate:
          Variables.Values[I] := EncodeDateTime(ResultSet.GetDate(ColumnIndex));
        ftTime:
          Variables.Values[I] := EncodeDateTime(ResultSet.GetTime(ColumnIndex));
        ftDateTime:
          Variables.Values[I] := EncodeDateTime(ResultSet.GetTimestamp(ColumnIndex));
        //ftString, ftMemo:
          //Variables.Values[I] := EncodeString(ResultSet.GetString(ColumnIndex));
        {$IFNDEF UNICODE}
        ftWidestring{$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}:
          Variables.Values[I] := EncodeUnicodeString(ResultSet.GetUnicodeString(ColumnIndex));
        {$ENDIF}
        ftBytes, ftVarBytes:
          Variables.Values[I] := EncodeBytes(ResultSet.GetBytes(ColumnIndex));
        ftArray,
        ftDataSet: raise EZDatabaseError.Create(SDataTypeDoesNotSupported);
        else
          Variables.Values[I] := EncodeString(ResultSet.GetString(ColumnIndex));
    end
    else
      Variables.Values[I] := NullVariant;
  end;
end;

{**
  Compares row field values with the given ones.
  @param KeyValues given values.
  @param RowValues row field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
  @return <code> if values are equal.
}
function CompareDataFields(const KeyValues, RowValues: TZVariantDynArray;
  PartialKey: Boolean; CaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  {$IFNDEF NEXTGEN}
  Value1, Value2: {$IFNDEF NO_ANSISTRING}AnsiString{$ELSE}RawByteString{$ENDIF};
  {$ENDIF}
  WValue1, WValue2: ZWideString;
begin
  Result := True;
  for I := 0 to High(KeyValues) do begin
    {$IFNDEF NEXTGEN}
    case KeyValues[I].VType of
      vtUnicodeString{$IFDEF UNICODE}, vtString{$ENDIF}:
        begin
    {$ENDIF}
          WValue1 := SoftVarManager.GetAsUnicodeString(KeyValues[I]);
          WValue2 := SoftVarManager.GetAsUnicodeString(RowValues[I]);
          if CaseInsensitive then begin
            if PartialKey then begin
              {$IFDEF MSWINDOWS}
                Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
                  PWideChar(WValue2), Length(WValue1), PWideChar(WValue1), Length(WValue1)) - 2{CSTR_EQUAL} = 0;
              {$ELSE}
                {$IFDEF UNICODE}
                Result := SysUtils.AnsiStrLComp(PWideChar(WValue2), PWideChar(WValue1), Length(WValue1)) = 0;
                {$ELSE}
                Value1 := AnsiString(WValue1);
                Value2 := AnsiString(WValue2);
                Result := AnsiStrLComp(PAnsiChar(Value2), PAnsiChar(Value1), Length(Value1)) = 0;
                {$ENDIF}
              {$ENDIF}
            end else
              Result := WValue1 = WValue2
          end else if PartialKey then begin
            {$IFDEF MSWINDOWS}
              Result := CompareStringW(LOCALE_USER_DEFAULT, 0,
                PWideChar(WValue2), Length(WValue1), PWideChar(WValue1), Length(WValue1)) - 2{CSTR_EQUAL} = 0;
            {$ELSE}
              {$IFDEF UNICODE}
              Result := SysUtils.AnsiStrLComp(PWideChar(WValue2), PWideChar(WValue1), Length(WValue1)) = 0;
              {$ELSE}
              Value1 := AnsiString(WValue1);
              Value2 := AnsiString(WValue2);
              Result := AnsiStrLComp(PAnsiChar(Value2), PAnsiChar(Value1), Length(Value1)) = 0;
              {$ENDIF}
            {$ENDIF}
          end else
            Result := SoftVarManager.Compare(KeyValues[I], RowValues[I]) = 0;
    {$IFNDEF NEXTGEN}
        end;
      else
      begin
        {$IFDEF NO_ANSISTRING}
        Value1 := SoftVarManager.GetAsRawByteString(KeyValues[I]);
        Value2 := SoftVarManager.GetAsRawByteString(RowValues[I]);
        {$ELSE}
        Value1 := SoftVarManager.GetAsAnsiString(KeyValues[I]);
        Value2 := SoftVarManager.GetAsAnsiString(RowValues[I]);
        {$ENDIF}
        if CaseInsensitive then begin
          Value1 := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiUpperCase(Value1);
          Value2 := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiUpperCase(Value2);
          if PartialKey
          then Result := {$IFDEF WITH_ANSISTRLCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrLComp(PAnsiChar(Value2), PAnsiChar(Value1), Length(Value1)) = 0
          else Result := Value1 = Value2
        end else begin
          if PartialKey
          then Result := {$IFDEF WITH_ANSISTRLCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrLComp(PAnsiChar(Value2), PAnsiChar(Value1), Length(Value1)) = 0
          else Result := SoftVarManager.Compare(KeyValues[I], RowValues[I]) = 0;
        end;
      end;
    end;
    {$ENDIF}
    if not Result then
      Break;
  end;
end;

{**
  Prepares values for comparison by CompareFieldsFromResultSet.
  @param FieldRefs an array with interested field object references.
  @param DecodedKeyValues given values.
  @param ResultSet  a resultset to get field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
}
procedure PrepareValuesForComparison(const FieldRefs: TObjectDynArray;
  var DecodedKeyValues: TZVariantDynArray; ResultSet: IZResultSet;
  PartialKey: Boolean; CaseInsensitive: Boolean);
var
  I: Integer;
  Current: TField;
  CurrentType : TZSQLType;
begin
  { Preprocesses cycle variables. }
  for I := 0 to High(FieldRefs) do
  begin
    Current := TField(FieldRefs[I]);

    if DecodedKeyValues[I].VType = vtNull then
      Continue;
    CurrentType := ResultSet.GetMetadata.GetColumnType(Current.FieldNo{$IFDEF GENERIC_INDEX} -1{$ENDIF});

    if PartialKey then
    begin
      if CurrentType = stUnicodeString then
      begin
        DecodedKeyValues[I] := SoftVarManager.Convert(
          DecodedKeyValues[I], vtUnicodeString);
        if CaseInsensitive then begin
          if DecodedKeyValues[I].VType = vtString then begin
            DecodedKeyValues[I].VString := Uppercase(DecodedKeyValues[I].VString);
            DecodedKeyValues[I].VUnicodeString := DecodedKeyValues{%H-}[I].VString;
          end else begin
            DecodedKeyValues[I].VUnicodeString :=
              {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(DecodedKeyValues[I].VUnicodeString);
          end;
        end;
      end
      else
      begin
        DecodedKeyValues[I] := SoftVarManager.Convert(
          DecodedKeyValues[I], vtString);
        if CaseInsensitive then
        begin
          {$IFDEF LAZARUSUTF8HACK} // Is this correct? Assumes the Lazarus convention all strings are UTF8. But is that
                       // true in this point, or should that be converted higher up?
          DecodedKeyValues[I].VString :=
            WideUpperCase(UTF8Decode (DecodedKeyValues[I].VString));
          {$ELSE}
          DecodedKeyValues[I].VString :=
            AnsiUpperCase(DecodedKeyValues[I].VString);
          {$ENDIF}
        end;
      end;
    end
    else
    begin
      case CurrentType of
        stBoolean:
          DecodedKeyValues[I] := SoftVarManager.Convert(
            DecodedKeyValues[I], vtBoolean);
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stULong, stLong:
          DecodedKeyValues[I] := SoftVarManager.Convert(
            DecodedKeyValues[I], vtInteger);
        stFloat, stDouble, stCurrency, stBigDecimal:
          DecodedKeyValues[I] := SoftVarManager.Convert(
            DecodedKeyValues[I], vtFloat);
        stUnicodeString:
          begin
            if CaseInsensitive then
            begin
              if DecodedKeyValues[I].VType = vtString then
              begin
                DecodedKeyValues[I].VString := Uppercase(DecodedKeyValues[I].VString);
                DecodedKeyValues[I].VUnicodeString := DecodedKeyValues{%H-}[I].VString;
              end
              else
              begin
                DecodedKeyValues[I].VUnicodeString :=
                  {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(DecodedKeyValues[I].VUnicodeString);
              end;
            end
            else
            begin
              DecodedKeyValues[I] := SoftVarManager.Convert(
                DecodedKeyValues[I], vtUnicodeString);
            end;
          end;
        stDate, stTime, stTimestamp:
          DecodedKeyValues[I] := SoftVarManager.Convert(
            DecodedKeyValues[I], vtDateTime);
        else
          if CaseInsensitive then
          begin
            DecodedKeyValues[I] := SoftVarManager.Convert( 
              DecodedKeyValues[I], vtString); 
            {$IFDEF LAZARUSUTF8HACK}
                    // Is this correct? Assumes the Lazarus convention all strings are UTF8. But is that
                    // true in this point, or should that be converted higher up?
            DecodedKeyValues[I].VString :=
              WideUpperCase(UTF8Decode (DecodedKeyValues[I].VString));
            {$ELSE}
            DecodedKeyValues[I].VString := 
              AnsiUpperCase(DecodedKeyValues[I].VString); 
            {$ENDIF} 
          end
          else
          begin
            DecodedKeyValues[I] := SoftVarManager.Convert(
              DecodedKeyValues[I], vtString);
          end;
      end;
    end;
  end;
end;

{**
  Compares row field values with the given ones.
  @param FieldRefs an array with interested field object references.
  @param KeyValues given values.
  @param ResultSet  a resultset to get field values.
  @param PartialKey <code>True</code> if values should be started with the keys.
  @param CaseInsensitive <code>True</code> if keys are case insensitive.
  @return <code> if values are equal.
}
function CompareFieldsFromResultSet(const FieldRefs: TObjectDynArray;
  const KeyValues: TZVariantDynArray; ResultSet: IZResultSet; PartialKey: Boolean;
  CaseInsensitive: Boolean): Boolean;
var
  I: Integer;
  ColumnIndex: Integer;
  {$IFNDEF NEXTGEN}
  AValue1, AValue2: {$IFDEF NO_ANSISTRING}RawByteString{$ELSE}AnsiString{$ENDIF};
  {$ENDIF}
  WValue1, WValue2: ZWideString;
  CurrentType : TZSQLType;
begin
  Result := True;
  for I := 0 to High(KeyValues) do
  begin
    ColumnIndex := TField(FieldRefs[I]).FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};

    if KeyValues[I].VType = vtNull then begin
      Result := ResultSet.IsNull(ColumnIndex);
      if not Result then
         Break;
      Continue;
    end;

    CurrentType := ResultSet.GetMetadata.GetColumnType(ColumnIndex);

    if PartialKey then begin
      {$IFNDEF NEXTGEN}
      if CurrentType = stUnicodeString then begin
      {$ENDIF}
        {$IFDEF NEXGEN}
        WValue1 := SoftVarManager.GetAsUnicodeString(KeyValues[I]);
        {$ELSE}
        WValue1 := KeyValues[I].VUnicodeString;
        {$ENDIF}
        WValue2 := ResultSet.GetUnicodeString(ColumnIndex);

        if CaseInsensitive then
          WValue2 := {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(WValue2);
        {$IFDEF UNICODE}
        Result := SysUtils.AnsiStrLComp(PWideChar(WValue2), PWideChar(WValue1), Length(WValue1)) = 0;
        {$ELSE}
          AValue1 := UTF8ToAnsi(UTF8Encode(WValue1));
          AValue2 := UTF8ToAnsi(UTF8Encode(WValue2));
          Result := AnsiStrLComp(PAnsiChar(AValue2), PAnsiChar(AValue1), Length(AValue1)) = 0;
        {$ENDIF}
      {$IFNDEF NEXTGEN}
      end else begin
        AValue1 := AnsiString(KeyValues[I].VString);
        if (ResultSet.GetConSettings.ClientCodePage^.Encoding = ceAnsi)
          or (ResultSet.GetConSettings.AutoEncode and ( ResultSet.GetConSettings.CTRL_CP <> 65001 )) then
          AValue2 := AnsiString(ResultSet.GetString(ColumnIndex))
        else
          AValue2 := AnsiString({$IFNDEF UNICODE}UTF8ToAnsi{$ENDIF}(ResultSet.GetString(ColumnIndex)));

        if CaseInsensitive then
          AValue2 := {$IFDEF WITH_UNITANSISTRINGS}AnsiStrings.{$ENDIF}AnsiUpperCase(AValue2);
        Result := {$IFDEF WITH_ANSISTRLCOMP_DEPRECATED}AnsiStrings.{$ENDIF}AnsiStrLComp(PAnsiChar(AValue2), PAnsiChar(AValue1), Length(AValue1)) = 0;
      end;
      {$ENDIF}
    end else
      case CurrentType of
        stBoolean: Result := KeyValues[I].VBoolean = ResultSet.GetBoolean(ColumnIndex);
        stByte, stShort, stWord, stSmall, stLongWord, stInteger, stUlong, stLong:
          Result := KeyValues[I].VInteger = ResultSet.GetLong(ColumnIndex);
        stFloat:
          Result := Abs(KeyValues[I].VFloat -
            ResultSet.GetBigDecimal(ColumnIndex)) < FLOAT_COMPARE_PRECISION_SINGLE;
        stDouble,
        stCurrency,
        stBigDecimal:
          Result := Abs(KeyValues[I].VFloat -
            ResultSet.GetBigDecimal(ColumnIndex)) < FLOAT_COMPARE_PRECISION;
        stDate,
        stTime,
        stTimestamp:
          Result := KeyValues[I].VDateTime = ResultSet.GetTimestamp(ColumnIndex);
        stUnicodeString:
          begin
            if CaseInsensitive
            then Result := KeyValues[I].VUnicodeString =
                {$IFDEF UNICODE}AnsiUpperCase{$ELSE}WideUpperCase{$ENDIF}(ResultSet.GetUnicodeString(ColumnIndex))
            else Result := KeyValues[I].VUnicodeString =
                ResultSet.GetUnicodeString(ColumnIndex);
          end;
        else
          if CaseInsensitive then
          begin
            {$IFDEF LAZARUSUTF8HACK}
            Result := KeyValues[I].VString =
              AnsiUpperCase (Utf8ToAnsi(ResultSet.GetString(ColumnIndex)));
            {$ELSE}
            Result := KeyValues[I].VString =
              AnsiUpperCase(ResultSet.GetString(ColumnIndex));
            {$ENDIF}
          end
          else
          begin
            Result := KeyValues[I].VString =
              ResultSet.GetString(ColumnIndex);
          end;
      end;

    Result := Result and not ResultSet.WasNull;
    if not Result then
      Break;
  end;
end;

{**
  Defines a list of key field names.
  @param Fields a collection of dataset fields.
  @return a list of key field names.
}
function DefineKeyFields(Fields: TFields; const IdConverter: IZIdentifierConvertor): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Fields.Count - 1 do
  begin
    if (Fields[I].FieldKind = fkData)
      and not (Fields[I].DataType in [ftBlob, ftGraphic, ftMemo, ftBytes, ftVarBytes {$IFDEF WITH_WIDEMEMO}, ftWideMemo{$ENDIF}]) then
      AppendSepString(Result, IdConverter.Quote(Fields[I].FieldName), ',');
  end;
end;

{**
  Converts datetime value into TDataset internal presentation.
  @param DataType a type of date-time field.
  @param Data a data which contains a value.
  @param Buffer a field buffer pointer
}
procedure DateTimeToNative(DataType: TFieldType; Data: TDateTime;
  Buffer: Pointer);
var
  TimeStamp: TTimeStamp;
begin
  TimeStamp := DateTimeToTimeStamp(Data);
  case DataType of
    ftDate: Integer(Buffer^) := TimeStamp.Date;
    ftTime: Integer(Buffer^) := TimeStamp.Time;
  else
    TDateTime(Buffer^) := TimeStampToMSecs(TimeStamp);
  end;
end;

{**
  Converts date times from TDataset internal presentation into datetime value.
  @param DataType a type of date-time field.
  @param Buffer a field buffer pointer
  @return a data which contains a value.
}
function NativeToDateTime(DataType: TFieldType; Buffer: Pointer): TDateTime;
{$IFNDEF OLDFPC}
var
  TimeStamp: TTimeStamp;
begin
  case DataType of
    ftDate:
      begin
        TimeStamp.Time := 0;
        TimeStamp.Date := Integer(Buffer^);
      end;
    ftTime:
      begin
        {$IFDEF WITH_FPC_FTTIME_BUG}
        TimeStamp := DateTimeToTimeStamp(TDateTime(Buffer^));
        {$ELSE}
        TimeStamp.Time := Integer(Buffer^);
        TimeStamp.Date := DateDelta;
        {$ENDIF}
      end;
  else
    try
      {$IF not defined(cpui386) and defined(FPC)}
      TimeStamp := MSecsToTimeStamp(System.Trunc(Int(TDateTime(Buffer^))));
      {$ELSE}
        TimeStamp := MSecsToTimeStamp(TDateTime(Buffer^){%H-});
      {$IFEND}
    except
      TimeStamp.Time := 0;
      TimeStamp.Date := 0;
    end;
  end;
  Result := TimeStampToDateTime(TimeStamp);
{$ELSE}
begin
  Result := TDateTime(Buffer^);
{$ENDIF}
end;

{**
  Compare values from two key fields.
  @param Field1 the first field object.
  @param ResultSet the resultset to read the first field value.
  @param Field2 the second field object.
}
function CompareKeyFields(Field1: TField; ResultSet: IZResultSet;
  Field2: TField): Boolean;
var
  ColumnIndex: Integer;
begin
  Result := False;
  if Field1.FieldNo >= 1 then
  begin
    ColumnIndex := Field1.FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
    case Field1.DataType of
      ftBoolean:
        Result := ResultSet.GetBoolean(ColumnIndex) = Field2.AsBoolean;
      {$IFDEF WITH_FTBYTE}ftByte,{$ENDIF}
      {$IFDEF WITH_FTSHORTINT}ftShortInt,{$ENDIF}
      ftSmallInt, ftInteger, ftAutoInc:
        Result := ResultSet.GetInt(ColumnIndex) = Field2.AsInteger;
      {$IFDEF WITH_FTSINGLE}
      ftSingle:
        Result := Abs(ResultSet.GetFloat(ColumnIndex)
          - Field2.AsSingle) < FLOAT_COMPARE_PRECISION_SINGLE;
      {$ENDIF}
      ftFloat:
        begin
          Result := Abs(ResultSet.GetDouble(ColumnIndex)
            - Field2.AsFloat) < FLOAT_COMPARE_PRECISION;
        end;
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
        Result := Abs(ResultSet.GetBigDecimal(ColumnIndex)
          - Field2.AsExtended) < FLOAT_COMPARE_PRECISION_SINGLE;
      {$ENDIF}
      {$IFDEF WITH_FTLONGWORD}
      ftLongword:
        Result := ResultSet.GetULong(ColumnIndex)
          = Field2.{$IFDEF TFIELD_HAS_ASLARGEINT}AsLargeInt{$ELSE}AsInteger{$ENDIF};
      {$ENDIF}
      ftLargeInt:
        begin
          if Field2 is TLargeIntField then
            Result := ResultSet.GetLong(ColumnIndex)
              = TLargeIntField(Field2).AsLargeInt
          else
            Result := ResultSet.GetInt(ColumnIndex) = Field2.AsInteger;
        end;
      ftBCD: Result := ResultSet.GetCurrency(ColumnIndex) = TBCDField(Field2).Value;
      ftCurrency: Result := (ResultSet.GetDouble(ColumnIndex) - Field2.AsFloat) < FLOAT_COMPARE_PRECISION;
      ftDate: Result := ResultSet.GetDate(ColumnIndex) = Field2.AsDateTime;
      ftTime: Result := ResultSet.GetTime(ColumnIndex) = Field2.AsDateTime;
      ftDateTime: Result := ResultSet.GetTimestamp(ColumnIndex) = Field2.AsDateTime;
      ftWideString:
        Result := ResultSet.GetUnicodeString(ColumnIndex) =
          Field2.{$IFDEF WITH_ASVARIANT}AsVariant{$ELSE}AsWideString{$ENDIF};
      else
        Result := ResultSet.GetString(ColumnIndex) = Field2.AsString;
    end;
  end;
end;

{**
  Defins a indices and directions for sorted fields.
  @param Dataset a dataset object.
  @param SortedFields an encoded fields for sorting in the format
    <Field Name> [ASC | DESC] [, ...]
  @param FieldRefs a decoded field object references.
  @param FieldDirs a decoded field directions.
  @param OnlyDataFields <code>True</code> if only data fields selected.
}
procedure DefineSortedFields(DataSet: TDataset;
  const SortedFields: string; out FieldRefs: TObjectDynArray;
  out CompareKinds: TComparisonKindArray; out OnlyDataFields: Boolean);
var
  I, TokenValueInt: Integer;
  Tokens: TStrings;
  TokenType: TZTokenType;
  TokenValue: string;
  Field: TField;
  FieldCount: Integer;
  PrevTokenWasField: Boolean;
begin
  OnlyDataFields := True;
  FieldCount := 0;
  PrevTokenWasField := False;
  SetLength(FieldRefs, FieldCount);
  SetLength(CompareKinds, FieldCount);
  Tokens := CommonTokenizer.TokenizeBufferToList(SortedFields,
    [toSkipEOF, toSkipWhitespaces, toUnifyNumbers, toDecodeStrings]);

  try
    for I := 0 to Tokens.Count - 1 do
    begin
      TokenType := TZTokenType({$IFDEF oldFPC}Pointer{$ENDIF}(Tokens.Objects[I]));
      TokenValue := Tokens[I];
      Field := nil;

      case TokenType of
        ttQuoted, ttQuotedIdentifier, ttWord:
          begin
            // Check if current token is a sort order marker
            // Note that ASC/DESC are valid field identifiers! So we must check
            // if previous token was a field and then set sort order
            // Otherwise - add current token as a field ("Field1 desc, Asc, Field2 desc")

            // Could this be a sort order marker?
            if PrevTokenWasField then
            begin
              if SysUtils.SameText(TokenValue, 'DESC') then
                CompareKinds[FieldCount - 1] := ckDescending
              else if SysUtils.SameText(TokenValue, 'ASC') then
                CompareKinds[FieldCount - 1] := ckAscending
              else
                raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [TokenValue]);
            end
            else
            // No, this is a field
              Field := DataSet.FieldByName(TokenValue);  // Will raise exception if field not present
          end;
        ttNumber:
          begin
            TokenValueInt := StrToInt(TokenValue);
            // Tokenizer always returns numbers > 0
            if TokenValueInt >= Dataset.Fields.Count then
              raise EZDatabaseError.CreateFmt(SFieldNotFound2, [TokenValueInt]);
            Field := Dataset.Fields[TokenValueInt];
          end;
        ttSymbol:
          if (TokenValue <> ',') and (TokenValue <> ';') then
            raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [TokenValue]);
        else
          raise EZDatabaseError.CreateFmt(SIncorrectSymbol, [TokenValue]);
      end;

      PrevTokenWasField := (Field <> nil);
      if Field <> nil then
      begin
        OnlyDataFields := OnlyDataFields and (Field.FieldKind = fkData);
        Inc(FieldCount);
        SetLength(FieldRefs, FieldCount);
        SetLength(CompareKinds, FieldCount);
        FieldRefs[FieldCount - 1] := Field;
        CompareKinds[FieldCount - 1] := ckAscending;
      end;
    end;
  finally
    Tokens.Free;
  end;
end;

{**
  Creates a fields lookup table to define fixed position
  of the field in dataset.
  @param Fields a collection of TDataset fields in initial order.
  @returns a fields lookup table.
}
type
  THackZField = Class(TZField); //access protected property
function CreateFieldsLookupTable(Fields: TFields): TPointerDynArray;
var
  I: Integer;
begin
  SetLength(Result, Fields.Count);
  for I := 0 to Fields.Count - 1 do
  begin
    Result[I] := Fields[I];
    if Fields[i] is TZField then
      THackZField(Fields[i]).FieldIndex := I+1;
  end;
end;

{**
  Defines an original field index in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param Field a TDataset field object.
  @returns an original fields index or -1 otherwise.
}
function DefineFieldIndex(const FieldsLookupTable: TPointerDynArray;
  Field: TField): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FieldsLookupTable) do
    if FieldsLookupTable[I] = Field then
    begin
      Result := I{$IFNDEF GENERIC_INDEX}+1{$ENDIF};
      Break;
    end;
end;

{**
  Defines an original field indices in the dataset.
  @param FieldsLookupTable a lookup table to define original index.
  @param FieldRefs a TDataset field object references.
  @returns an array with original fields indices.
}
function DefineFieldIndices(const FieldsLookupTable: TPointerDynArray;
  const FieldRefs: TObjectDynArray): TIntegerDynArray;
var
  I: Integer;
begin
  if FieldRefs = nil then
  begin
    Result := nil;
    Exit;
  end;

  SetLength(Result, Length(FieldRefs));
  for I := 0 to High(Result) do
    Result[I] := DefineFieldIndex(FieldsLookupTable, TField(FieldRefs[I]));
end;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(QualifiedName: string;
  out Catalog, Schema, ObjectName: string);

{$IFDEF OLDFPC}
function ExtractStrings(Separators, WhiteSpace: TSysCharSet; Content: PChar;
  Strings: TStrings): Integer;
var
  Head, Tail: PChar;
  EOS, InQuote: Boolean;
  QuoteChar: Char;
  Item: string;
begin
  Result := 0;
  if (Content = nil) or (Content^ = #0) or (Strings = nil) then
     Exit;
  Tail := Content;
  InQuote := False;
  QuoteChar := #0;
  Strings.BeginUpdate;
  try
    repeat
      while CharInSet(Tail^, WhiteSpace + [#13, #10]) do
        Inc(Tail);
      Head := Tail;
      while True do
      begin
        while (InQuote and not CharInSet(Tail^, [QuoteChar, #0])) or
               not CharInSet(Tail^, Separators + [#0, #13, #10, '''', '"']) do
           Inc(Tail);
        if CharInSet(Tail^, ['''', '"']) then
        begin
          if (QuoteChar <> #0) and (QuoteChar = Tail^) then
            QuoteChar := #0
          else
            QuoteChar := Tail^;
          InQuote := QuoteChar <> #0;
          Inc(Tail);
        end
        else
          Break;
      end;
      EOS := Tail^ = #0;
      if (Head <> Tail) and (Head^ <> #0) then
      begin
        if Strings <> nil then
        begin
          SetString(Item, Head, Tail - Head);
          Strings.Add(Item);
        end;
        Inc(Result);
      end;
      Inc(Tail);
    until EOS;
  finally
    Strings.EndUpdate;
  end;
end;
{$ENDIF}

var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    Catalog := '';
    Schema := '';
    ObjectName := QualifiedName;
    ExtractStrings(['.'], [' '], PChar(QualifiedName), SL);
    case SL.Count of
      0, 1: ;
      2: begin
           Schema := SL.Strings[0];
           ObjectName := SL.Strings[1];
         end;
      3: begin
           Catalog := SL.Strings[0];
           Schema := SL.Strings[1];
           ObjectName := SL.Strings[2];
         end;
    else
      begin
        ObjectName := SL.Strings[SL.Count - 1];
        Schema := SL.Strings[SL.Count - 2];
        for I := 0 to SL.Count - 3 do
        begin
          Catalog := Catalog + SL.Strings[I];
          if I < SL.Count - 3 then
            Catalog := Catalog + '.';
        end;
      end;
    end;
  finally
    SL.Free;
  end;
end;

{**
  Splits up a qualified object name into pieces. Catalog, schema
  and objectname.
}
procedure SplitQualifiedObjectName(QualifiedName: string;
  const SupportsCatalogs, SupportsSchemas: Boolean;
  out Catalog, Schema, ObjectName: string);
var
  SL: TStringList;
  I: Integer;
begin
  if SupportsCatalogs and SupportsSchemas then
    SplitQualifiedObjectName(QualifiedName, Catalog, Schema, ObjectName)
  else
  begin
    SL := TStringList.Create;
    try
      Catalog := '';
      Schema := '';
      ObjectName := QualifiedName;
      ExtractStrings(['.'], [' '], PChar(QualifiedName), SL);
      case SL.Count of
        0, 1: ;
        2:
          begin
            if SupportsCatalogs then
            begin
              Catalog := SL.Strings[0];
              if SupportsSchemas then
                Schema := SL.Strings[1]
              else
                ObjectName := SL.Strings[1];
            end
            else
              if SupportsSchemas then
              begin
                Schema := SL.Strings[0];
                ObjectName := SL.Strings[1];
              end
              else
                ObjectName := SL.Strings[0]+'.'+SL.Strings[1];
          end;
        3:
          if SupportsCatalogs then
          begin
            Catalog := SL.Strings[0];
            if SupportsSchemas then
            begin
              Schema := SL.Strings[1];
              ObjectName := SL.Strings[2]
            end
            else
              ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
          end
          else
            if SupportsSchemas then
            begin
              Schema := SL.Strings[0];
              ObjectName := SL.Strings[1]+'.'+SL.Strings[2];
            end
            else
              ObjectName := SL.Strings[0]+'.'+SL.Strings[1]+'.'+SL.Strings[2];
        else
          if SupportsCatalogs then
          begin
            Catalog := SL.Strings[0];
            if SupportsSchemas then
            begin
              Schema := SL.Strings[1];
              for i := 2 to SL.Count-1 do
                if i = 2 then
                  ObjectName := SL.Strings[i]
                else
                  ObjectName := ObjectName+'.'+SL.Strings[i];
            end
            else
            begin
              ObjectName := '';
              for i := 2 to SL.Count-1 do
                if I = 2 then
                  ObjectName := SL.Strings[i]
                else
                  ObjectName := ObjectName+'.'+SL.Strings[i];
            end;
          end
          else
            if SupportsSchemas then
            begin
              Schema := SL.Strings[0];
              for i := 1 to SL.Count-1 do
                if i = 1 then
                  ObjectName := SL.Strings[i]
                else
                  ObjectName := ObjectName+'.'+SL.Strings[i];
            end
            else
              for i := 0 to SL.Count-1 do
                if I = 0 then
                  ObjectName := SL.Strings[i]
                else
                  ObjectName := ObjectName+'.'+SL.Strings[i];
        end;
    finally
      SL.Free;
    end;
  end;
end;

{**
  Assigns a Statement value from a TParam
  @param Index the index of Statement.SetXxxx(ColumnIndex, xxx);
  @param Statement the PrepredStatement where the values have been assigned
  @param Param the TParam where the value is assigned from
}
procedure SetStatementParam(Index: Integer;
  Statement: IZPreparedStatement; Param: TParam);
var
  Stream: TStream;
  BlobData: TBlobData;
  P: Pointer;
  UniTemp: ZWideString;
begin
  if Param.IsNull then
    Statement.SetNull(Index, ConvertDatasetToDbcType(Param.DataType))
  else
  begin
    case Param.DataType of
      ftBoolean:
        Statement.SetBoolean(Index, Param.AsBoolean);
      {$IFDEF WITH_FTBYTE}
      ftByte:
        Statement.SetByte(Index, Param.AsByte);
      {$ENDIF}
      {$IFDEF WITH_FTSHORTINT}
      ftShortInt:
        Statement.SetShort(Index, Param.AsShortInt);
      {$ENDIF}
      ftWord:
        Statement.SetWord(Index, Param.AsWord);
      ftSmallInt:
        Statement.SetSmall(Index, Param.AsSmallInt);
      ftInteger, ftAutoInc:
        Statement.SetInt(Index, Param.AsInteger);
      {$IFDEF WITH_FTSINGLE}
      ftSingle:
        Statement.SetFloat(Index, Param.AsSingle);
      {$ENDIF}
      ftFloat:
        Statement.SetDouble(Index, Param.AsFloat);
      {$IFDEF WITH_FTEXTENDED}
      ftExtended:
        Statement.SetBigDecimal(Index, Param.AsFloat);
      {$ENDIF}
      {$IFDEF WITH_FTLONGWORD}
      ftLongWord:
        Statement.SetInt(Index, Integer(Param.AsLongWord));
      {$ENDIF}
      ftLargeInt:
        Statement.SetLong(Index, {$IFDEF WITH_PARAM_ASLARGEINT}Param.AsLargeInt{$ELSE}StrToInt64(Param.AsString){$ENDIF});
      ftCurrency, ftBCD:
        Statement.SetBigDecimal(Index, Param.AsCurrency);
      ftString, ftFixedChar:
        {$IFNDEF UNICODE}
        if (TVarData(Param.Value).VType = varOleStr) {$IFDEF WITH_varUString} or (TVarData(Param.Value).VType = varUString){$ENDIF}
        then Statement.SetUnicodeString(Index, Param.Value)
        else {$ENDIF}Statement.SetString(Index, Param.AsString);
      {$IFDEF WITH_FTWIDESTRING}
      ftWideString:
        Statement.SetUnicodeString(Index, Param.AsWideString);
      {$ENDIF}
      ftBytes, ftVarBytes{$IFDEF WITH_FTGUID}, ftGuid{$ENDIF}:
        begin
          {$IFDEF TPARAM_HAS_ASBYTES}
          Statement.SetBytes(Index, Param.AsBytes);
          {$ELSE}
          Statement.SetBytes(Index, VarToBytes(Param.Value));
          {$ENDIF}
        end;
      ftDate:
        Statement.SetDate(Index, Param.AsDate);
      ftTime:
        Statement.SetTime(Index, Param.AsTime);
      ftDateTime:
        Statement.SetTimestamp(Index, Param.AsDateTime);
      ftMemo: case TvarData(Param.Value).VType of
          {$IFDEF WITH_varUString}varUString,{$ENDIF}
          {$IFDEF UNICODE}varString,{$ENDIF} //otherwise we get a conversion warning
          varOleStr: begin
              UniTemp := Param.{$IFDEF UNICODE}AsMemo{$ELSE}Value{$ENDIF};
              P :=  Pointer(UniTemp);
              if P = nil then
                P := PEmptyUnicodeString;
              Statement.SetBlob(Index, stUnicodeStream, TZAbstractClob.CreateWithData(PWideChar(P), Length(UniTemp), Statement.GetConnection.GetConSettings));
            end;
          else begin
            {EgonHugeist: On reading a Param as Memo the Stream reads Byte-wise
              on Changing to stUnicodeString/Delphi12Up a String is from
              Type wide/unicode so we have to give him back as
              Stream!}
              {$IFDEF UNICODE}
              Stream := Param.AsStream;
              {$ELSE}
              Stream := TStringStream.Create(Param.AsMemo);
              {$ENDIF}
            try
              Statement.SetAsciiStream(Index, Stream);
            finally
              Stream.Free;
            end;
          end;
        end;
      {$IFDEF WITH_WIDEMEMO}
      ftWideMemo:
        begin
          UniTemp := Param.AsWideString;
          P :=  Pointer(UniTemp);
          if P = nil then
            P := PEmptyUnicodeString;
          Statement.SetBlob(Index, stUnicodeStream, TZAbstractClob.CreateWithData(PWideChar(P), Length(UniTemp), Statement.GetConnection.GetConSettings));
        end;
      {$ENDIF}
      ftBlob, ftGraphic:
        begin
          BlobData := Param.AsBlob;
          Statement.SetBlob(Index, stBinaryStream, TZAbstractBlob.CreateWithData(Pointer(BlobData), Length(BlobData)));
        end;
      else
        raise EZDatabaseError.Create(SUnKnownParamDataType + ' ' + {$IFNDEF WITH_FASTCODE_INTTOSTR}ZFastCode.{$ENDIF}IntToStr(Ord(Param.DataType)));
    end;
  end;
end;

initialization
  CommonTokenizer := TZGenericSQLTokenizer.Create as IZTokenizer;
finalization
  CommonTokenizer := nil;
end.
