{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{           Oracle Database Connectivity Classes          }
{                                                         }
{          Originally written by Sergey Seroukhov         }
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

unit ZDbcOracleUtils;

interface

{$I ZDbc.inc}
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses
  Types, Classes, {$IFDEF MSEgui}mclasses,{$ENDIF} SysUtils,
  {$IF defined(WITH_INLINE) and defined(MSWINDOWS) and not defined(WITH_UNICODEFROMLOCALECHARS)}
  Windows,
  {$IFEND}
  ZSysUtils, ZDbcIntfs, ZVariant, ZPlainOracleDriver, ZDbcLogging,
  ZCompatibility, ZPlainOracleConstants;

const
  MAX_SQLVAR_LIMIT = 1024;
  Max_OCI_String_Size = 4000;
  Max_OCI_Raw_Size = 2000;


type
  {** Declares SQL Object }

  POCIObject = ^TOCIObject;
  TObjFields = array of POCIObject;
  TOCIObject = Record                 // embedded object or table will work recursively
    type_name:      String;           //object's name (TDO)
    type_schema:    String;           //object's schema name (TDO)
    parmdp:         POCIParam;        //Describe attributes of the object OCI_DTYPE_PARAM
    parmap:         POCIParam;        //Describe attributes of the object OCI_ATTR_COLLECTION_ELEMENT OCI_ATTR_PARAM
    tdo:            POCIType;         //object's TDO handle
    typecode:       OCITypeCode;      //object's OCI_ATTR_TYPECODE
    col_typecode:   OCITypeCode;      //if collection this is its OCI_ATTR_COLLECTION_TYPECODE
    elem_typecode:  OCITypeCode;      //if collection this is its element's OCI_ATTR_TYPECODE
    obj_ref:        POCIRef;          //if an embeded object this is ref handle to its TDO
    obj_ind:        POCIInd;          //Null indictator for object
    obj_value:      POCIComplexObject;//the actual value from the DB
    obj_type:       POCIType;         //if an embeded object this is the  OCIType returned by a OCIObjectPin
    is_final_type:  ub1;              //object's OCI_ATTR_IS_FINAL_TYPE
    fields:         TObjFields;       //one object for each field/property
    field_count:    ub2;              //The number of fields Not really needed but nice to have
    next_subtype:   POCIObject;       //There is strored information about subtypes for inteherited objects
    stmt_handle:    POCIStmt;         //the Statement-Handle
    Level:          Integer;          //the instance level
    Pinned:         Boolean;          //did we pin the obj on decribe?
  end;

  PUB2Array = ^TUB2Array;
  TUB2Array = array[0..0] of ub2;
  PSB2Array = ^TSB2Array;
  TSB2Array = array[0..0] of sb2;

  PZSQLVar = ^TZSQLVar;
  TZSQLVar = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    {OCI Handles}
    Handle:    POCIHandle;
    Define:    POCIHandle;
    BindHandle: POCIBind;
    {binding values}
    Data:      Pointer;
    oDataType:  ub2;
    oDataSize:  ub2;
    oIndicatorArray: PSB2Array; //indicates NULL ...
    oDataSizeArray: PUB2Array; //value length for strings/bytes
    _Obj:      POCIObject;
    {Zeos proceesing values}
    DescriptorType: sb4;
    TypeCode:  ub2;
    Length:    NativeUInt; //indicate size of Data
    Precision: Integer; //field.precision
    Scale:     Integer; //field.scale
    ColType:   TZSQLType; //Zeos SQLType
    lobs:      TInterfaceDynArray; //temporary interface
    CodePage:  Word; //ColumnCodePage
  end;

  TZSQLVars = {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif} record
    AllocNum:  ub4;
    Variables: array[0..MAX_SQLVAR_LIMIT] of TZSQLVar; //just a nice dubugging range
  end;
  PZSQLVars = ^TZSQLVars;

  TZOracleParam = Record
    pName: string;
    pSQLType: Integer;
    pTypeName: String;
    pType: TZProcedureColumnType;
    pProcIndex: Integer;
    pParamIndex: Integer; //Current ZeosParameter index
    pOutIndex: Integer;
  End;
  TZOracleParams = array of TZOracleParam;

type
  {$A-}
  TOraDate = record
    Cent, Year, Month, Day, Hour, Min, Sec: Byte;
  end;
  POraDate = ^TOraDate;
  {$A+}
{**
  Allocates memory for Oracle SQL Variables.
  @param Variables a pointer to array of variables.
  @param Count a number of SQL variables.
}
procedure AllocateOracleSQLVars(var Variables: PZSQLVars; Count: Integer);

{**
  Frees memory Oracle SQL Variables from the memory.
  @param PlainDriver an Oracle plain driver.
  @param Variables a pointer to array of variables.
  @param Handle a OCIEnvironment pointer
  @param ErrorHandle the OCI ErrorHandle
  @param ConSetttings the Pointer to the TZConSettings record
}
procedure FreeOracleSQLVars(const PlainDriver: IZOraclePlainDriver;
  var Variables: PZSQLVars; const Iteration: Integer; const Handle: POCIEnv;
  const ErrorHandle: POCIError; const {%H-}ConSettings: PZConSettings);

procedure DefineOracleVarTypes(var Variable: PZSQLVar; DataType: TZSQLType;
  DataSize: Integer; OracleType: ub2; OCICanBindInt64: Boolean);

procedure SetVariableDataEntrys(var BufferEntry: PAnsiChar; var Variable: PZSQLVar;
  Iteration: NativeUInt); {$IFDEF WITH_INLINE}inline;{$ENDIF}

function CalcBufferSizeOfSQLVar(Const Variable: PZSQLVar): Integer; {$IFDEF WITH_INLINE}inline;{$ENDIF}

procedure AllocDesriptors(const PlainDriver: IZOraclePlainDriver;
  ConnectionHandle: POCIEnv; var Variable: PZSQLVar; Iteration: Integer;
  AllocTemporyLobs: Boolean);

{**
  Loads Oracle variable binded to SQL statement with data.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param ErrorHandle the stmt ErrorHandle
  @param Variable Oracle variable holder.
  @param Values a values to be loaded.
  @param ChunkSize the size in bytes we send the lobs in chunks
  @param Iteration the Iters we can use
}
procedure LoadOracleVar(const PlainDriver: IZOraclePlainDriver;
  const Connection: IZConnection; const ErrorHandle: POCIError;
  const Variable: PZSQLVar; var Value: TZVariant; ChunkSize: Integer;
  Iteration: Integer);

{**
  Unloads Oracle variables binded to SQL statement with data.
  @param Variables Oracle variable holders or array of TDesciptorRec.
  @param ArrayCount count of bound arrays
}
procedure UnloadOracleVars(var Variables: PZSQLVars; const Iteration: Integer);

{**
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(const TypeName: string;
  Precision, Scale: Integer; const CtrlsCPType: TZControlsCodePage): TZSQLType;

{**
  Checks for possible SQL errors.
  @param PlainDriver an Oracle plain driver.
  @param Handle an Oracle error handle.
  @param Status a command return status.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckOracleError(const PlainDriver: IZOraclePlainDriver;
  const ErrorHandle: POCIError; const Status: Integer;
  const LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  const ConSettings: PZConSettings);

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(const PlainDriver: IZOraclePlainDriver;
  const Statement: IZStatement; const LogSQL: string; const Handle: POCIStmt;
  const ErrorHandle: POCIError; ZBufferSize: Integer): IZResultSet; overload;

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(const PlainDriver: IZOraclePlainDriver;
  const Statement: IZStatement; const LogSQL: string; StmtHandle: POCIStmt;
  ErrorHandle: POCIError; const Params: PZSQLVars;
  Const OracleParams: TZOracleParams): IZResultSet; overload;

{**
  Allocates in memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection object.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure AllocateOracleStatementHandles(const PlainDriver: IZOraclePlainDriver;
  const Connection: IZConnection; var Handle: POCIStmt; var ErrorHandle: POCIError;
  UserServerCachedStmt: Boolean = False);

{**
  Frees from memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure FreeOracleStatementHandles(const PlainDriver: IZOraclePlainDriver;
  var Handle: POCIStmt; var ErrorHandle: POCIError);

{**
  Prepares an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure PrepareOracleStatement(const PlainDriver: IZOraclePlainDriver;
  ContextHandle: POCISvcCtx; const SQL: RawByteString; var Handle: POCIStmt;
  const ErrorHandle: POCIError; PrefetchMemory: ub4; ServerCachedStmtHandle: Boolean;
  const ConSettings: PZConSettings);

{**
  Gets a number of updates made by executed Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @returns a number of updates.
}
function GetOracleUpdateCount(const PlainDriver: IZOraclePlainDriver;
  const Handle: POCIStmt; const ErrorHandle: POCIError): ub4;

function DescribeObject(const PlainDriver: IZOraclePlainDriver; const Connection: IZConnection;
  ParamHandle: POCIParam; {%H-}stmt_handle: POCIHandle; Level: ub2): POCIObject;

procedure OraWriteLob(const PlainDriver: IZOraclePlainDriver; const BlobData: Pointer;
  const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
  const LobLocator: POCILobLocator; const ChunkSize: Integer;
  BlobSize: Int64; Const BinaryLob: Boolean; const ConSettings: PZConSettings);

{$ENDIF ZEOS_DISABLE_ORACLE}
implementation
{$IFNDEF ZEOS_DISABLE_ORACLE}

uses Math, ZMessages, ZDbcOracle, ZDbcOracleResultSet, ZDbcCachedResultSet,
  ZDbcUtils, ZEncoding, ZFastCode, ZClasses
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF};

{**
  Allocates memory for Oracle SQL Variables.
  @param Variables a pointer to array of variables.
  @param Count a number of SQL variables.
}
procedure AllocateOracleSQLVars(var Variables: PZSQLVars; Count: Integer);
var
  Size: Integer;
begin
  if Variables <> nil then
    FreeMem(Variables);

  Size := SizeOf(TZSQLVars) + Count * SizeOf(TZSQLVar);
  GetMem(Variables, Size);
  FillChar(Variables^, Size, {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  Variables^.AllocNum := Count;
end;

{**
  Frees memory Oracle SQL Variables from the memory.
  @param PlainDriver an Oracle plain driver.
  @param Variables a pointer to array of variables.
  @param Handle a OCIEnvironment pointer
  @param ErrorHandle the OCI ErrorHandle
  @param ConSetttings the Pointer to the TZConSettings record
}
procedure FreeOracleSQLVars(const PlainDriver: IZOraclePlainDriver;
  var Variables: PZSQLVars; const Iteration: Integer; const Handle: POCIEnv;
  const ErrorHandle: POCIError; const ConSettings: PZConSettings);
var
  I: Integer;
  J: NativeUInt;
  CurrentVar: PZSQLVar;

  procedure DisposeObject(var Obj: POCIObject);
  var
    I: Integer;
  begin
    for i := 0 to High(Obj.fields) do
      DisposeObject(Obj.fields[i]);
    SetLength(Obj.fields, 0);
    if Assigned(Obj.next_subtype) then
    begin
      DisposeObject(Obj.next_subtype);
      Obj.next_subtype := nil;
    end;
    if Obj.Pinned then
      {Unpin tdo}
      //CheckOracleError(PlainDriver, ErrorHandle, //debug
        PlainDriver.ObjectUnpin(Handle,ErrorHandle, CurrentVar^._Obj.tdo)
        ;//debug, lcOther, 'OCIObjectUnpin', ConSettings);
    if (Obj.Level = 0) and assigned(Obj.tdo) then
      {Free Object}
      //debugCheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.ObjectFree(Handle,ErrorHandle, CurrentVar^._Obj.tdo, 0)
      ;//debug, lcOther, 'OCIObjectFree', ConSettings);
    Dispose(Obj);
    Obj := nil;
  end;

begin
  if Variables <> nil then begin
    { Frees allocated memory for output variables }
    if Variables.AllocNum > 0 then
    for I := 0 to Variables.AllocNum-1 do begin
      {$R-}
      CurrentVar := @Variables.Variables[I];
      {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
      if Assigned(CurrentVar^._Obj) then
        DisposeObject(CurrentVar^._Obj);
      if (CurrentVar^.Data <> nil) and (CurrentVar^.DescriptorType > 0) then
        for J := 0 to Iteration-1 do
          if ({%H-}PPOCIDescriptor({%H-}NativeUInt(CurrentVar^.Data)+(J*SizeOf(Pointer))))^ <> nil then
            PlainDriver.DescriptorFree({%H-}PPOCIDescriptor({%H-}NativeUInt(CurrentVar^.Data)+(J*SizeOf(Pointer)))^,
              CurrentVar^.DescriptorType);
      if (CurrentVar^.lobs <> nil) then
        SetLength(CurrentVar^.lobs, 0);
      CurrentVar^.Data := nil;
      CurrentVar^.oIndicatorArray := nil;
      CurrentVar^.oDataSizeArray := nil;
    end;
    FreeMem(Variables);
    Variables := nil;
  end;
end;

procedure DefineOracleVarTypes(var Variable: PZSQLVar; DataType: TZSQLType;
  DataSize: Integer; OracleType: ub2; OCICanBindInt64: Boolean);
begin
  with Variable^ do
  begin
    ColType := DataType;
    TypeCode := OracleType;
    oDataSize := DataSize;
    DescriptorType := 0; //init ?
    case ColType of
      stBoolean, stByte, stShort, stWord, stSmall, stInteger:
        begin
          TypeCode := SQLT_INT;
          Length := SizeOf(Integer);
        end;
      stUlong:
        begin
          TypeCode := SQLT_STR;
          oDataSize := 23;
          Length := 23; //for trailing #0
        end;
      stLong:
        if OCICanBindInt64 then
        begin
          TypeCode := SQLT_INT;
          Length := SizeOf(Int64);
        end
        else
        begin
          TypeCode := SQLT_FLT;
          Length := SizeOf(Double);
        end;
      stFloat, stDouble, stLongWord, stCurrency, stBigDecimal:
        begin
          TypeCode := SQLT_FLT;
          Length := SizeOf(Double);
        end;
      stDate, stTime, stTimestamp:
        case OracleType of
          SQLT_DAT: Length := DataSize; //reading without conversions!
          SQLT_DATE:
            begin
              TypeCode := SQLT_DAT;
              Length := 7;
            end;
          SQLT_INTERVAL_DS:
            begin
              DescriptorType := OCI_DTYPE_INTERVAL_DS;
              Length := SizeOf(POCIInterval);
            end;
          SQLT_INTERVAL_YM:
            begin
              DescriptorType := OCI_DTYPE_INTERVAL_YM;
              Length := SizeOf(POCIInterval);
            end;
          else
            begin //for all other DateTime vals we would loose msec precision...
              DescriptorType := OCI_DTYPE_TIMESTAMP;
              TypeCode := SQLT_TIMESTAMP;
              Length := SizeOf(POCIDateTime);
            end;
        end;
      stGUID:
        begin
          TypeCode := SQLT_STR;
          oDataSize := 39;
          Length := 39; //for trailing #0
        end;
      stString, stUnicodeString:
        if OracleType = SQLT_AFC then
          Length := oDataSize
        else
        begin
          TypeCode := SQLT_STR;
          Length := oDataSize + 1;
        end;
      stAsciiStream, stUnicodeStream, stBinaryStream, stBytes:
        if (TypeCode in [SQLT_CLOB, SQLT_BLOB, SQLT_BFILEE, SQLT_CFILEE,SQLT_NTY]) then
        begin
          if not (OracleType = SQLT_NTY) then
            DescriptorType := OCI_DTYPE_LOB;
          Length := SizeOf(POCILobLocator);
        end
        else
        begin
          if ColType = stAsciiStream then
            TypeCode := SQLT_LVC
          else
            TypeCode := SQLT_LVB;
          if oDataSize = 0 then
            Length := 128 * 1024 + SizeOf(Integer)
          else
            Length := oDataSize + SizeOf(Integer);
        end;
      stDataSet: ; //Do nothing here!
      stUnknown:
    end;
  end;
end;

procedure SetVariableDataEntrys(var BufferEntry: PAnsiChar; var Variable: PZSQLVar;
  Iteration: NativeUInt);
begin
  with Variable^ do
  begin
  {now let's set binding entrys}
  //step one: set null indicators
    oIndicatorArray := Pointer(BufferEntry);
    Inc(BufferEntry, SizeOf(sb2)*Iteration);
  //Step two: set Length Indicators if required
    oDataSizeArray := {%H-}Pointer({%H-}NativeUInt(BufferEntry) * Byte((TypeCode=SQLT_STR) and (oDataSize > 0))); //either nil or valid address
    Inc(BufferEntry, Byte((TypeCode=SQLT_STR) and (oDataSize > 0))*NativeUInt(SizeOf(ub2))*Iteration); //inc 0 or SizeOf(ub2)
  //Step three: set data entrys if required
    Data := {%H-}Pointer({%H-}NativeUInt(BufferEntry) * Byte(ColType <> stUnknown)); //either nil or valid address
    Inc(BufferEntry, Byte(ColType <> stUnknown)*Length*Iteration); //inc 0 or Length
  end;
end;

function CalcBufferSizeOfSQLVar(Const Variable: PZSQLVar): Integer;
begin
  Result := (
    (Integer(Variable^.Length))+ {if we use locale adressation then skip it}
    SizeOf(sb2){NullIndicator}+ {allways present}
    (Ord((Variable^.TypeCode = SQLT_STR) and (Variable^.oDataSize > 0))*SizeOf(ub2)){LengthIndicator} //only for SQLT_STR and skip if field is null
    );
end;

procedure AllocDesriptors(const PlainDriver: IZOraclePlainDriver;
  ConnectionHandle: POCIEnv; var Variable: PZSQLVar; Iteration: Integer;
  AllocTemporyLobs: Boolean);
var
  i: LongWord;
begin
  if Variable^.DescriptorType > 0 then
  begin
    for i := 0 to Iteration -1 do
      PlainDriver.DescriptorAlloc(ConnectionHandle,
        {%H-}PPOCIDescriptor({%H-}NativeUInt(Variable^.Data)+(I*SizeOf(POCIDescriptor)))^, Variable^.DescriptorType, 0, nil);
    if AllocTemporyLobs and (Variable^.DescriptorType = OCI_DTYPE_LOB) then
    SetLength(Variable^.lobs, Iteration);
  end;
end;

{**
  Loads Oracle variable binded to SQL statement with data.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection Object.
  @param ErrorHandle the stmt ErrorHandle
  @param Variable Oracle variable holder.
  @param Values a values to be loaded.
  @param ChunkSize the size in bytes we send the lobs in chunks
  @param Iteration the Iters we can use
}
procedure LoadOracleVar(const PlainDriver: IZOraclePlainDriver;
  const Connection: IZConnection; const ErrorHandle: POCIError;
  const Variable: PZSQLVar; var Value: TZVariant; ChunkSize: Integer;
  Iteration: Integer);
var
  Len: Integer;
  I: Longword; //use unsigned type
  TempDate: TDateTime;
  TempBytes: TBytes;
  TempBlob: IZBlob;
  WriteTempBlob: IZOracleBlob;

  Year, Month, Day, Hour, Min, Sec, MSec: Word;
  OracleConnection: IZOracleConnection;
  ClientVarManager: IZClientVariantManager;
  Buffer: Pointer;
  AnsiTemp: RawByteString;
  ConSettings: PZConSettings;
  CharRec: TZCharRec;
  { array DML bindings }
  ZData: Pointer; //array entry
  {using mem entry of ZData is faster then casting}
  ZBooleanArray: TBooleanDynArray absolute ZData;
  ZByteArray: TByteDynArray absolute ZData;
  ZShortIntArray: TShortIntDynArray absolute ZData;
  ZWordArray: TWordDynArray absolute ZData;
  ZSmallIntArray: TSmallIntDynArray absolute ZData;
  ZLongWordArray: TLongWordDynArray absolute ZData;
  ZIntegerArray: TIntegerDynArray absolute ZData;
  ZInt64Array: TInt64DynArray absolute ZData;
  ZUInt64Array: TUInt64DynArray absolute ZData;
  ZSingleArray: TSingleDynArray absolute ZData;
  ZDoubleArray: TDoubleDynArray absolute ZData;
  ZCurrencyArray: TCurrencyDynArray absolute ZData;
  ZExtendedArray: TExtendedDynArray absolute ZData;
  ZDateTimeArray: TDateTimeDynArray absolute ZData;
  ZRawByteStringArray: TRawByteStringDynArray absolute ZData;
  {$IFNDEF NO_ANSISTRING}
  ZAnsiStringArray: TAnsiStringDynArray absolute ZData;
  {$ENDIF}
  {$IFNDEF NO_UTF8STRING}
  ZUTF8StringArray: TUTF8StringDynArray absolute ZData;
  {$ENDIF}
  ZStringArray: TStringDynArray absolute ZData;
  ZUnicodeStringArray: TUnicodeStringDynArray absolute ZData;
  ZCharRecArray: TZCharRecDynArray absolute ZData;
  ZBytesArray: TBytesDynArray absolute ZData;
  ZInterfaceArray: TInterfaceDynArray absolute ZData;
  ZGUIDArray: TGUIDDynArray absolute ZData;
  WS: ZWideString;
  LobBuffer: Pointer;

  procedure SetEmptyString;
  begin
    {$R-}
    Variable^.oIndicatorArray^[I] := -1;
    Variable^.oDataSizeArray^[i] := 1; //place of #0
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
    ({%H-}PAnsiChar({%H-}NativeUInt(Variable^.Data)+I*Variable^.Length))^ := #0; //OCI expects the trailing $0 byte
  end;
  procedure MoveString(Const Data: Pointer; Iter: LongWord);
  begin
    {$R-}
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Data^, {%H-}Pointer({%H-}NativeUInt(Variable^.Data)+Iter*Variable^.Length)^, Variable^.oDataSizeArray^[Iter]);
    ({%H-}PAnsiChar({%H-}NativeUInt(Variable^.Data)+Iter*Variable^.Length)+Variable^.oDataSizeArray^[Iter]-1)^ := #0; //improve  StrLCopy... set a leadin #0 if truncation happens
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
  end;
begin
  OracleConnection := Connection as IZOracleConnection;
  ClientVarManager := Connection.GetClientVariantManager;
  ConSettings := Connection.GetConSettings;
  if (Iteration = 1) and (Value.VType <> vtArray) then
  { single row execution }
  begin
    if ClientVarManager.IsNull(Value) then
      Variable^.oIndicatorArray^[0] := -1
    else
    begin
      Variable^.oIndicatorArray^[0] := 0;
      case Variable^.TypeCode of
        SQLT_INT:
          if Variable^.Length = 8 then
            PInt64(Variable^.Data)^ := ClientVarManager.GetAsInteger(Value)
          else
            PInteger(Variable^.Data)^ := ClientVarManager.GetAsInteger(Value);
        SQLT_FLT:
          PDouble(Variable^.Data)^ := ClientVarManager.GetAsFloat(Value);
        SQLT_STR:
          begin
            CharRec := ClientVarManager.GetAsCharRec(Value, ConSettings^.ClientCodePage^.CP);
            Variable^.oDataSizeArray^[0] := Math.Min(CharRec.Len, Max_OCI_String_Size)+1; //need the leading $0, because oracle expects it
            MoveString(CharRec.P, 0);
          end;
        SQLT_TIMESTAMP:
          begin
            TempDate := ClientVarManager.GetAsDateTime(Value);
            DecodeDate(TempDate, Year, Month, Day);
            DecodeTime(TempDate, Hour, Min, Sec, MSec);
            CheckOracleError(PlainDriver, ErrorHandle,
              PlainDriver.DateTimeConstruct(OracleConnection.GetConnectionHandle,
                ErrorHandle, PPOCIDescriptor(Variable^.Data)^,
                Year, Month, Day, Hour, Min, Sec, MSec * 1000000, nil, 0),
              lcOther, '', ConSettings);
          end;
        SQLT_LVB, SQLT_LVC:
            if Pointer(Value.VBytes) = nil then
            begin
              Variable^.oIndicatorArray^[0] := -1;
              PInteger(Variable^.Data)^ := 0;
            end
            else
            begin
              PInteger(Variable^.Data)^ := Math.Min(System.Length(Value.VBytes), Variable^.oDataSize);
              {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Value.VBytes)^, {%H-}Pointer({%H-}NativeUInt(Variable^.Data)+SizeOf(Integer))^, PInteger(Variable^.Data)^);
            end;
        SQLT_BLOB:
          begin
            SetLength(Variable^.lobs, 1);
            if Value.VType = vtBytes then
            begin
              TempBytes := ClientVarManager.GetAsBytes(Value);
              Len := Length(TempBytes);
              if Len > 0 then
                Buffer := @TempBytes[0]
              else
                Buffer := nil;
            end
            else
            begin
              TempBlob := ClientVarManager.GetAsInterface(Value) as IZBlob;
              if TempBlob.IsEmpty then
              begin
                Buffer := nil;
                Len := 0;
              end
              else
              begin
                Buffer := TempBlob.GetBuffer;
                Len := TempBlob.Length;
              end;
            end;
            WriteTempBlob := TZOracleBlob.Create(PlainDriver, nil, 0,
              OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
              PPOCIDescriptor(Variable^.Data)^, ChunkSize, ConSettings);
            WriteTempBlob.CreateBlob;
            WriteTempBlob.WriteLobFromBuffer(Buffer, Len);
            Variable^.lobs[0] := WriteTempBlob;
          end;
        SQLT_CLOB:
          try
            TempBlob := ClientVarManager.GetAsInterface(Value) as IZBlob;
            if TempBlob.IsClob then
            begin
              WriteTempBlob := TZOracleClob.Create(PlainDriver,
                nil, 0, OracleConnection.GetConnectionHandle,
                OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                PPOCIDescriptor(Variable^.Data)^, ChunkSize, ConSettings,
                ConSettings^.ClientCodePage^.CP);
              WriteTempBlob.CreateBlob;
              Buffer := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
              WriteTempBlob.WriteLobFromBuffer(Buffer, TempBlob.Length);
            end
            else
            begin
              if TempBlob.IsEmpty then
              begin
                Buffer := nil;
                Len := 0;
              end
              else
              begin
                AnsiTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, Connection.GetConSettings);
                Len := Length(AnsiTemp);
                Buffer := Pointer(AnsiTemp);
              end;
              WriteTempBlob := TZOracleClob.Create(PlainDriver, nil, 0,
                OracleConnection.GetConnectionHandle, OracleConnection.GetContextHandle,
                OracleConnection.GetErrorHandle, PPOCIDescriptor(Variable^.Data)^,
                ChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
              WriteTempBlob.CreateBlob;
              WriteTempBlob.WriteLobFromBuffer(Buffer, Len);
            end;
            Variable^.lobs[0] := WriteTempBlob;
          finally
            WriteTempBlob := nil;
          end
      end;
    end;
  end
  else
  { array DML binding }
  {$R-}
  begin
    //More code(the loops), i know but this avoids loads of If's /case processing
    //step one: build up null inticators
    if (Value.VArray.VArray = nil) then
      for i := 0 to Iteration -1 do
      begin
        Variable^.oIndicatorArray^[i] := -1; //set all null
        if Variable^.TypeCode = SQLT_STR then
          {%H-}PAnsiChar({%H-}NativeUInt(Variable^.Data)+I*Variable^.Length)^ := #0; //oci expects a terminating $0 byte
        Exit; //we are ready here
      end
    else if (Value.VArray.VIsNullArray = nil) then
      for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[i] := 0 //set all not null
    else begin
      ZData := Value.VArray.VIsNullArray;
      case TZSQLType(Value.VArray.VIsNullArrayType) of
        stBoolean:        for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZBooleanArray[I]);
        stByte:           for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZByteArray[I] <> 0);
        stShort:          for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZShortIntArray[I] <> 0);
        stWord:           for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZWordArray[I] <> 0);
        stSmall:          for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZSmallIntArray[I] <> 0);
        stLongWord:       for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZLongWordArray[I] <> 0);
        stInteger:        for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZIntegerArray[I] <> 0);
        stULong:          for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZUInt64Array[I] <> 0);
        stLong:           for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZInt64Array[I] <> 0);
        stFloat:          for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZSingleArray[I] <> 0);
        stDouble:         for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZDoubleArray[I] <> 0);
        stCurrency:       for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZCurrencyArray[I] <> 0);
        stBigDecimal:     for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZExtendedArray[I] <> 0);
        stDate,
        stTime,
        stTimeStamp: for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(ZDateTimeArray[I] <> 0);
        stUnicodeString:
          if Value.VArray.VIsNullArrayVariantType = vtCharRec then
            for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(StrToBoolEx(PWideChar(ZCharRecArray[I].P)))
          else
            for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(StrToBoolEx(ZUnicodeStringArray[I]));
        stString:
          if Value.VArray.VIsNullArrayVariantType = vtCharRec then
            for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(StrToBoolEx(PAnsiChar(ZCharRecArray[I].P)))
          else
            for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord(StrToBoolEx(ZRawByteStringArray[I]));
        stAsciiStream, 
        stUnicodeStream, 
        stBinaryStream:   for i := 0 to Iteration -1 do Variable^.oIndicatorArray^[I] := -Ord((ZInterfaceArray[I] <> nil) or not (ZInterfaceArray[I] as IZBLob).IsEmpty);
       end; 
    end;
    { now let's assign the data }    
    ZData := Value.VArray.VArray;
    if ZData <> nil then
      case Variable^.ColType of
        stBoolean: //Oracle doesn't support boolean types so lets use integers and OCI converts it..
          for i := 0 to Iteration -1 do {%H-}PInteger({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Integer))^ := Ord(ZBooleanArray[I]);
        stByte: //Oracle doesn't support byte type so lets use integers and OCI converts it..
          for i := 0 to Iteration -1 do {%H-}PInteger({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Integer))^ := ZByteArray[I];
        stShort: //Oracle doesn't support ShortInt type so lets use integers and OCI converts it..
          for i := 0 to Iteration -1 do {%H-}PInteger({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Integer))^ := ZShortIntArray[I];
        stWord: //Oracle doesn't support word type so lets use integers and OCI converts it..
          for i := 0 to Iteration -1 do {%H-}PInteger({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Integer))^ := ZWordArray[I];
        stSmall: //Oracle doesn't support smallint type so lets use integers and OCI converts it..
          for i := 0 to Iteration -1 do {%H-}PInteger({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Integer))^ := ZSmallIntArray[I];
        stLongWord:
          //since 11.2 we can use Int64 types too
          if Connection.GetClientVersion >= 11002000 then
            for i := 0 to Iteration -1 do {%H-}PInt64({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Int64))^ := ZLongWordArray[I]
          else
            for i := 0 to Iteration -1 do {%H-}PDouble({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Double))^ := ZLongWordArray[I];
        stInteger: { no conversion required }
          {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(ZIntegerArray[0], Variable^.Data^, Iteration*SizeOf(Integer));
        stULong: //we use String types here
          for i := 0 to Iteration -1 do
          begin
            AnsiTemp := IntToRaw(ZUInt64Array[I]);
            Variable^.oDataSizeArray^[i] := Length(AnsiTemp)+1;
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(AnsiTemp)^, {%H-}Pointer({%H-}NativeUInt(Variable^.Data)+I*Variable^.Length)^, Variable^.oDataSizeArray^[i]);
          end;
        stLong: //conversion required below 11.2
          //since 11.2 we can use Int64 types too
          if Connection.GetClientVersion >= 11002000 then
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(ZInt64Array[0], Variable^.Data^, Iteration*SizeOf(Int64))
          else
            for i := 0 to Iteration -1 do {%H-}PDouble({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Double))^ := ZInt64Array[I];
        stFloat: //conversion required
          for i := 0 to Iteration -1 do {%H-}PDouble({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Double))^ := ZSingleArray[I];
        stDouble: //no conversion required
          {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(ZDoubleArray[0], Variable^.Data^, Iteration*SizeOf(Double));
        stCurrency: //conversion required
          for i := 0 to Iteration -1 do {%H-}PDouble({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Double))^ := ZCurrencyArray[I];
        stBigDecimal: //conversion required
          for i := 0 to Iteration -1 do {%H-}PDouble({%H-}NativeUInt(Variable^.Data)+I*SizeOf(Double))^ := ZExtendedArray[I];
        stString:
          case Value.VArray.VArrayVariantType of
            vtString:
              for i := 0 to Iteration -1 do
                if (Variable^.oIndicatorArray^[I] = -1) or (Pointer(ZStringArray[I]) = nil) then //Length = 0
                //Oracle doesn't support empty strings. OCI convert empty string to NULL silently
                  SetEmptyString
                else
                begin
                  AnsiTemp := ConSettings^.ConvFuncs.ZStringToRaw(ZStringArray[I], ConSettings^.CTRL_CP, ConSettings^.ClientCodePage^.CP); //conversion possible or just an inc of RefCount? ):);
                  Variable^.oDataSizeArray^[i] := Math.Min(Length(AnsiTemp)+1, LengthInt(Variable^.Length));
                  MoveString(Pointer(AnsiTemp), I);
                end;
            {$IFNDEF NO_ANSISTRING}
            vtAnsiString:
              for i := 0 to Iteration -1 do
                if (Variable^.oIndicatorArray^[I] = -1) or (Pointer(ZAnsiStringArray[I]) = nil) then //Length = 0
                  SetEmptyString
                else
                begin
                  AnsiTemp := ConSettings^.ConvFuncs.ZAnsiToRaw(ZAnsiStringArray[I], ConSettings^.ClientCodePage^.CP); //conversion possible or just an inc of RefCount? ):);
                  Variable^.oDataSizeArray^[i] := Math.Min(Length(AnsiTemp)+1, LengthInt(Variable^.Length));
                  MoveString(Pointer(AnsiTemp), I);
                end;
            {$ENDIF}
            {$IFNDEF NO_UTF8STRING}
            vtUTF8String:
              if ZCompatibleCodePages(zCP_UTF8, ConSettings^.ClientCodePage^.CP) then
                for i := 0 to Iteration -1 do
                  if (Variable^.oIndicatorArray^[I] = -1) or (Pointer(ZUTF8StringArray[I]) = nil) then //Length = 0
                    SetEmptyString
                  else
                  begin
                    Variable^.oDataSizeArray^[i] := Math.Min(Length(ZUTF8StringArray[I])+1, LengthInt(Variable^.Length));
                    MoveString(Pointer(ZUTF8StringArray[I]), I);
                  end
              else
                for i := 0 to Iteration -1 do
                  if (Variable^.oIndicatorArray^[I] = -1) or (Pointer(ZUTF8StringArray[I]) = nil) then //Length = 0
                    SetEmptyString
                  else
                  begin
                    AnsiTemp := ConSettings^.ConvFuncs.ZUTF8ToRaw(ZUTF8StringArray[I], ConSettings^.ClientCodePage^.CP); //conversion possible or just an inc of RefCount? ):);
                    Variable^.oDataSizeArray^[i] := Math.Min(Length(AnsiTemp)+1, LengthInt(Variable^.Length));
                    MoveString(Pointer(AnsiTemp), I);
                  end;
            {$ENDIF}
            vtRawByteString:
              for i := 0 to Iteration -1 do
                if (Variable^.oIndicatorArray^[I] = -1) or (Pointer(ZRawByteStringArray[I]) = nil) then //Length = 0
                  SetEmptyString
                else
                begin
                  Variable^.oDataSizeArray^[i] := Math.Min(Length(ZRawByteStringArray[I])+1, Variable^.oDataSize);
                  MoveString(Pointer(ZRawByteStringArray[I]), I);
                end;
            vtCharRec:
              {in array bindings we assume all codepages are equal!}
              if ZCompatibleCodePages(ZCharRecArray[0].CP, ConSettings^.ClientCodePage^.CP) then
                for i := 0 to Iteration -1 do
                  if (Variable^.oIndicatorArray^[I] = -1) or (ZCharRecArray[I].Len = 0) then //Length = 0
                    SetEmptyString
                  else
                  begin
                    Variable^.oDataSizeArray^[i] := {%H-}Math.Min(ZCharRecArray[I].Len+1, LengthInt(Variable^.Length));
                    MoveString(Pointer(ZCharRecArray[I].P), I);
                  end
              else
                if ZCompatibleCodePages(ZCharRecArray[0].CP, zCP_UTF16) then
                  for i := 0 to Iteration -1 do
                    if (Variable^.oIndicatorArray^[I] = -1) or (ZCharRecArray[I].Len = 0) then //Length = 0
                      SetEmptyString
                    else
                    begin
                      AnsiTemp := PUnicodeToRaw(ZCharRecArray[i].P, ZCharRecArray[i].Len,ConSettings^.ClientCodePage^.CP); //convert to client encoding
                      Variable^.oDataSizeArray^[i] := Math.Min(Length(AnsiTemp)+1, LengthInt(Variable^.Length));
                      MoveString(Pointer(AnsiTemp), I);
                    end
                else
                  for i := 0 to Iteration -1 do
                    if (Variable^.oIndicatorArray^[I] = -1) or (ZCharRecArray[I].Len = 0) then
                      SetEmptyString
                    else
                    begin
                      WS := PRawToUnicode(ZCharRecArray[i].P, ZCharRecArray[i].Len, ZCharRecArray[i].CP); //localize ?WideString? to avoid overrun
                      AnsiTemp := ZUnicodeToRaw(WS, ConSettings^.ClientCodePage^.CP); //convert to client encoding
                      Variable^.oDataSizeArray^[i] := Math.Min(Length(AnsiTemp)+1, LengthInt(Variable^.Length));
                      MoveString(Pointer(AnsiTemp), I);
                    end;
            else
              raise Exception.Create('Unsupported String Variant');
          end;
        stUnicodeString:
          case Value.VArray.VArrayVariantType of
            vtUnicodeString:
              for i := 0 to Iteration -1 do
                if (Variable^.oIndicatorArray^[I] = -1) or (Pointer(ZUnicodeStringArray[i]) = nil) then
                  SetEmptyString
                else
                begin
                  AnsiTemp := ZUnicodeToRaw(ZUnicodeStringArray[i], ConSettings^.ClientCodePage^.CP); //convert to client encoding
                  Variable^.oDataSizeArray^[i] := Math.Min(Length(AnsiTemp)+1, LengthInt(Variable^.Length));
                  MoveString(Pointer(AnsiTemp), I);
                end;
            vtCharRec:
              for i := 0 to Iteration -1 do
                if (Variable^.oIndicatorArray^[I] = -1) or (ZCharRecArray[I].Len = 0) then
                  SetEmptyString
                else
                begin
                  AnsiTemp := PUnicodeToRaw(ZCharRecArray[I].P, ZCharRecArray[I].Len, ConSettings^.ClientCodePage^.CP); //convert to client encoding
                  Variable^.oDataSizeArray^[i] := Math.Min(Length(AnsiTemp)+1, LengthInt(Variable^.Length));
                  MoveString(Pointer(AnsiTemp), I);
                end;
            else
              raise Exception.Create('Unsupported String Variant');
          end;
        stBytes:
          for i := 0 to Iteration -1 do
            if Pointer(ZBytesArray[I]) = nil then
            begin
              Variable^.oIndicatorArray^[I] := -1;
              {%H-}PInteger({%H-}NativeUInt(Variable^.Data)+I*Variable^.Length)^ := 0;
            end
            else
            begin
              {%H-}PInteger({%H-}NativeUInt(Variable^.Data)+I*Variable^.Length)^ := Math.Min(System.Length(ZBytesArray[I]), Variable^.oDataSize);
              {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(ZBytesArray[I])^, {%H-}Pointer({%H-}NativeUInt(Variable^.Data)+I*Variable^.Length+SizeOf(Integer))^,{%H-}PInteger({%H-}NativeUInt(Variable^.Data)+I*Variable^.Length)^);
            end;
        stGUID: //AFAIK OCI doesn't support GUID fields so let's convert them to stings
          for i := 0 to Iteration -1 do
            if (Variable^.oIndicatorArray^[I] = 0) then
            begin
              AnsiTemp := GUIDToRaw(ZGUIDArray[I]);
              Variable^.oDataSizeArray^[i] := 39;
              {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(AnsiTemp)^, {%H-}Pointer({%H-}NativeUInt(Variable^.Data)+I*39)^, 39);
            end;
        stDate, stTime, stTimeStamp:
          for i := 0 to Iteration -1 do
            if Variable^.oIndicatorArray^[I] = 0 then
            begin
              DecodeDate(ZDateTimeArray[i], Year, Month, Day);
              DecodeTime(ZDateTimeArray[i], Hour, Min, Sec, MSec);
              CheckOracleError(PlainDriver, ErrorHandle,
                PlainDriver.DateTimeConstruct(OracleConnection.GetConnectionHandle,
                  ErrorHandle, {%H-}PPOCIDescriptor({%H-}NativeUInt(Variable^.Data)+I*SizeOf(PPOCIDescriptor))^, //direct addressing descriptore to array. So we don't need to free the mem again
                  Year, Month, Day, Hour, Min, Sec, MSec * 1000000, nil, 0),
                lcOther, 'OCIDateTimeConstruct', ConSettings);
            end;
        stArray, stDataSet: ; //no idea yet
        stAsciiStream, stUnicodeStream:
          for i := 0 to Iteration -1 do
            if Variable^.oIndicatorArray^[I] = 0 then //we already checked this above
            begin
              TempBlob := ZInterfaceArray[I] as IZBLob;
              if TempBlob.IsClob then
              begin
                WriteTempBlob := TZOracleClob.Create(PlainDriver,
                  nil, 0, OracleConnection.GetConnectionHandle,
                  OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                  {%H-}PPOCIDescriptor({%H-}NativeUInt(Variable^.Data)+I*SizeOf(PPOCIDescriptor))^,
                  ChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
                WriteTempBlob.CreateBlob;
                LobBuffer := TempBlob.GetPAnsiChar(ConSettings^.ClientCodePage^.CP);
                WriteTempBlob.WriteLobFromBuffer(LobBuffer, TempBlob.Length);
              end
              else
              begin
                AnsiTemp := GetValidatedAnsiStringFromBuffer(TempBlob.GetBuffer,
                    TempBlob.Length, Connection.GetConSettings);
                LobBuffer := Pointer(AnsiTemp);
                WriteTempBlob := TZOracleClob.Create(PlainDriver, nil, 0,
                  OracleConnection.GetConnectionHandle, OracleConnection.GetContextHandle,
                  OracleConnection.GetErrorHandle, {%H-}PPOCIDescriptor({%H-}NativeUInt(Variable^.Data)+I*SizeOf(PPOCIDescriptor))^,
                  ChunkSize, ConSettings, ConSettings^.ClientCodePage^.CP);
                WriteTempBlob.CreateBlob;
                WriteTempBlob.WriteLobFromBuffer(LobBuffer, Length(AnsiTemp));
              end;
              Variable^.lobs[I] := WriteTempBlob; //ref interface to keep OCILob alive otherwise OCIFreeLobTemporary will be called
            end;
        stBinaryStream:
          for i := 0 to Iteration -1 do
            if Variable^.oIndicatorArray^[I] = 0 then //we already checked this above
            begin
              TempBlob := ZInterfaceArray[I] as IZBLob;
              WriteTempBlob := TZOracleBlob.Create(PlainDriver, nil, 0,
                OracleConnection.GetContextHandle, OracleConnection.GetErrorHandle,
                {%H-}PPOCIDescriptor({%H-}NativeUInt(Variable^.Data)+I*SizeOf(PPOCIDescriptor))^,
                ChunkSize, ConSettings);
              WriteTempBlob.CreateBlob;
              WriteTempBlob.WriteLobFromBuffer(TempBlob.GetBuffer, TempBlob.Length);
              Variable^.lobs[I] := WriteTempBlob; //ref interface to keep OCILob alive otherwise OCIFreeLobTemporary will be called
            end;
      end;
  end;
  {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Unloads Oracle variables binded to SQL statement with data.
  @param Variables Oracle variable holders.
}
procedure UnloadOracleVars(var Variables: PZSQLVars; const Iteration: Integer);
var
  I: Integer;
  J: LongWord;
begin
  if Variables^.AllocNum > 0 then
  for i := 0 to Variables^.AllocNum -1 do
    {$R-}
    if (Variables^.Variables[i].DescriptorType > 0) and (Length(Variables^.Variables[i].Lobs) > 0) then
      for j := 0 to High(Variables^.Variables[i].Lobs) do
        Variables^.Variables[i].Lobs[j] := nil;
    {$IFDEF RangeCheckEnabled} {$R+} {$ENDIF}
end;

{**
  Convert string Oracle field type to SQLType
  @param string field type value
  @result the SQLType field type value
}
function ConvertOracleTypeToSQLType(const TypeName: string;
  Precision, Scale: Integer; const CtrlsCPType: TZControlsCodePage): TZSQLType;
var TypeNameUp: string;
begin
  TypeNameUp := UpperCase(TypeName);

  if (TypeNameUp = 'CHAR') or (TypeNameUp = 'VARCHAR2') then
    Result := stString
  else if (TypeNameUp = 'NCHAR') or (TypeNameUp = 'NVARCHAR2') then
    Result := stString
  else if (TypeNameUp = 'FLOAT') or (TypeNameUp = 'BINARY_FLOAT') or (TypeNameUp = 'BINARY_DOUBLE') then
    Result := stDouble
  else if TypeNameUp = 'DATE' then  {precission - 1 sec, so Timestamp}
    Result := stTimestamp
  else if TypeNameUp = 'BLOB' then
    Result := stBinaryStream
  else if (TypeNameUp = 'RAW') then
    Result := stBytes
  else if (TypeNameUp = 'LONG RAW') then
    Result := stBinaryStream
  else if TypeNameUp = 'CLOB' then
    Result := stAsciiStream
  else if TypeNameUp = 'NCLOB' then
    Result := stAsciiStream
  else if TypeNameUp = 'LONG' then
    Result := stAsciiStream
  else if (TypeNameUp = 'ROWID') or (TypeNameUp = 'UROWID') then
    Result := stString
  else if StartsWith(TypeNameUp, 'TIMESTAMP') then
    Result := stTimestamp
  else if TypeNameUp = 'BFILE' then
    Result := stBinaryStream else
  if TypeNameUp = 'NUMBER' then begin //numer is signed always
    if (Scale = 0) and (Precision > 0) and (Precision <= 18) then begin
      if Precision <= 2 then
        Result := stShort
      else if Precision <= 4 then
        Result := stSmall
      else if Precision <= 8 then
        Result := stInteger
      else
        Result := stLong
    end else Result := stDouble;  { default for number types}
  end
  else if StartsWith(TypeNameUp, 'INTERVAL') then
    Result := stTimestamp
  else
    Result := stUnknown;
  if ( CtrlsCPType = cCP_UTF16 ) then
    case result of
      stString: Result := stUnicodeString;
      stAsciiStream: if not (TypeNameUp = 'LONG') then Result := stUnicodeStream; //fix: http://zeos.firmos.at/viewtopic.php?t=3530
    end;
end;

{**
  Checks for possible SQL errors.
  @param PlainDriver an Oracle plain driver.
  @param Handle an Oracle error handle.
  @param Status a command return status.
  @param LogCategory a logging category.
  @param LogMessage a logging message.
}
procedure CheckOracleError(const PlainDriver: IZOraclePlainDriver;
  const ErrorHandle: POCIError; const Status: Integer;
  const LogCategory: TZLoggingCategory; const LogMessage: RawByteString;
  const ConSettings: PZConSettings);
var
  ErrorMessage: RawByteString;
  ErrorBuffer: TRawBuff;
  ErrorCode: SB4;
begin
  ErrorBuffer.Pos := 0;
  ErrorCode := Status;

  case Status of
    OCI_SUCCESS:
      Exit;
    OCI_SUCCESS_WITH_INFO:
      begin
        PlainDriver.ErrorGet(ErrorHandle, 1, nil, ErrorCode, @ErrorBuffer.Buf[0], SizeOf(ErrorBuffer.Buf)-1, OCI_HTYPE_ERROR);
        ErrorBuffer.Pos := StrLen(@ErrorBuffer.Buf[0])+1;
        ErrorMessage := 'OCI_SUCCESS_WITH_INFO: ';
      end;
    OCI_NEED_DATA:  ErrorMessage := 'OCI_NEED_DATA';
    OCI_NO_DATA:    ErrorMessage := 'OCI_NO_DATA';
    OCI_ERROR:
      begin
        if PlainDriver.ErrorGet(ErrorHandle, 1, nil, ErrorCode, @ErrorBuffer.Buf[0], SizeOf(ErrorBuffer.Buf)-1, OCI_HTYPE_ERROR) = 100
        then ErrorMessage := 'OCI_ERROR: Unkown(OCI_NO_DATA)'
        else begin
          ErrorMessage := 'OCI_ERROR: ';
          ErrorBuffer.Pos := StrLen(@ErrorBuffer.Buf[0])+1;
      end;
      end;
    OCI_INVALID_HANDLE:
      ErrorMessage := 'OCI_INVALID_HANDLE';
    OCI_STILL_EXECUTING:
      ErrorMessage := 'OCI_STILL_EXECUTING';
    OCI_CONTINUE:
      ErrorMessage := 'OCI_CONTINUE';
    else ErrorMessage := '';
  end;
  FlushBuff(ErrorBuffer, ErrorMessage);

  if (Status <> OCI_SUCCESS_WITH_INFO) and (ErrorMessage <> '') then
  begin
    if Assigned(DriverManager) then //Thread-Safe patch
      DriverManager.LogError(LogCategory, ConSettings^.Protocol, LogMessage,
        ErrorCode, ErrorMessage);
    if not ( ( LogCategory = lcDisconnect ) and ( ErrorCode = 3314 ) ) then //patch for disconnected Server
      //on the other hand we can't close the connction  MantisBT: #0000227
      if LogMessage <> ''
      then raise EZSQLException.CreateWithCode(ErrorCode,
        Format(cSSQLError3, [ConSettings^.ConvFuncs.ZRawToString(ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP), ErrorCode, LogMessage]))
      else raise EZSQLException.CreateWithCode(ErrorCode,
        Format(SSQLError1, [ConSettings^.ConvFuncs.ZRawToString(ErrorMessage, ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP)]));
  end;
  if (Status = OCI_SUCCESS_WITH_INFO) and (ErrorMessage <> '') then
    if Assigned(DriverManager) then //Thread-Safe patch
      DriverManager.LogMessage(LogCategory, ConSettings^.Protocol, ErrorMessage);
end;

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(const PlainDriver: IZOraclePlainDriver;
  const Statement: IZStatement; const LogSQL: string; const Handle: POCIStmt;
  const ErrorHandle: POCIError; ZBufferSize: Integer): IZResultSet;
var
  NativeResultSet: TZOracleResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZOracleResultSet.Create(PlainDriver, Statement,
    LogSQL, Handle, ErrorHandle, ZBufferSize);
  NativeResultSet.SetConcurrency(rcReadOnly);
  if (Statement.GetResultSetConcurrency = rcUpdatable)
    or (Statement.GetResultSetType <> rtForwardOnly) then
  begin
    CachedResultSet := TZCachedResultSet.Create(NativeResultSet, LogSQL, nil,
      Statement.GetConnection.GetConSettings);
    CachedResultSet.SetConcurrency(rcUpdatable);
    CachedResultSet.SetResolver(TZOracleCachedResolver.Create(
      Statement, NativeResultSet.GetMetadata));
    Result := CachedResultSet;
  end
  else
    Result := NativeResultSet;
end;

{**
  Creates an Oracle result set based on the current settings.
  @return a created result set object.
}
function CreateOracleResultSet(const PlainDriver: IZOraclePlainDriver;
      const Statement: IZStatement; const LogSQL: string; StmtHandle: POCIStmt;
      ErrorHandle: POCIError; const Params: PZSQLVars;
      Const OracleParams: TZOracleParams): IZResultSet;
var
  NativeResultSet: TZOracleCallableResultSet;
  CachedResultSet: TZCachedResultSet;
begin
  NativeResultSet := TZOracleCallableResultSet.Create(PlainDriver, Statement,
    LogSQL, StmtHandle, ErrorHandle, Params, OracleParams);
  NativeResultSet.SetConcurrency(rcReadOnly);
  CachedResultSet := TZCachedResultSet.Create(NativeResultSet, LogSQL, nil,
    Statement.GetConnection.GetConSettings);
  CachedResultSet.SetConcurrency(rcReadOnly);
  CachedResultSet.SetResolver(TZOracleCachedResolver.Create(
    Statement, NativeResultSet.GetMetadata));
  CachedResultSet.Last;
  CachedResultSet.BeforeFirst;
  Result := CachedResultSet;
end;

{**
  Allocates in memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Connection an Oracle connection object.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure AllocateOracleStatementHandles(const PlainDriver: IZOraclePlainDriver;
  const Connection: IZConnection; var Handle: POCIStmt; var ErrorHandle: POCIError;
  UserServerCachedStmt: Boolean);
var
  OracleConnection: IZOracleConnection;
begin
  OracleConnection := Connection as IZOracleConnection;
  ErrorHandle := nil;
  PlainDriver.HandleAlloc(OracleConnection.GetConnectionHandle,
    ErrorHandle, OCI_HTYPE_ERROR, 0, nil);
  Handle := nil;
  //if not UserServerCachedStmt then
    PlainDriver.HandleAlloc(OracleConnection.GetConnectionHandle,
      Handle, OCI_HTYPE_STMT, 0, nil);
end;

{**
  Frees from memory Oracle handlers for Statement object.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure FreeOracleStatementHandles(const PlainDriver: IZOraclePlainDriver;
  var Handle: POCIStmt; var ErrorHandle: POCIError);
begin
  if ErrorHandle <> nil then
  begin
    PlainDriver.HandleFree(ErrorHandle, OCI_HTYPE_ERROR);
    ErrorHandle := nil;
  end;
  if Handle <> nil then
  begin
    PlainDriver.HandleFree(Handle, OCI_HTYPE_STMT);
    Handle := nil;
  end;
end;

{**
  Prepares an Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param SQL an SQL query to be prepared.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
}
procedure PrepareOracleStatement(const PlainDriver: IZOraclePlainDriver;
  ContextHandle: POCISvcCtx; const SQL: RawByteString; var Handle: POCIStmt;
  const ErrorHandle: POCIError; PrefetchMemory: ub4; ServerCachedStmtHandle: Boolean;
  const ConSettings: PZConSettings);
var
  PrefetchCount: ub4;
begin
  PrefetchCount := 0;
  if ServerCachedStmtHandle then
  begin
    {indicate length + 1! see: https://sourceforge.net/p/zeoslib/tickets/93/}
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.StmtPrepare2(ContextHandle, Handle, ErrorHandle,
        Pointer(SQL), Length(SQL)+1,nil,0,OCI_NTV_SYNTAX,OCI_DEFAULT),
      lcExecute, SQL, ConSettings);
    CheckOracleError(PlainDriver, ErrorHandle,
     PlainDriver.AttrSet(Handle,OCI_HTYPE_STMT, @PrefetchCount ,0, OCI_ATTR_PREFETCH_ROWS,ErrorHandle),
        lcOther, 'Prefetch_Count', ConSettings);
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.AttrSet(Handle,OCI_HTYPE_STMT,@PrefetchMemory,0,OCI_ATTR_PREFETCH_MEMORY,ErrorHandle),
        lcOther, 'Prefetch_Memory', ConSettings);
  end
  else
  begin
    CheckOracleError(PlainDriver, ErrorHandle,
     PlainDriver.AttrSet(Handle,OCI_HTYPE_STMT, @PrefetchCount ,0, OCI_ATTR_PREFETCH_ROWS,ErrorHandle),
        lcOther, 'Prefetch_Count', ConSettings);
    CheckOracleError(PlainDriver, ErrorHandle,
      PlainDriver.AttrSet(Handle,OCI_HTYPE_STMT,@PrefetchMemory,0,OCI_ATTR_PREFETCH_MEMORY,ErrorHandle),
        lcOther, 'Prefetch_Memory', ConSettings);
    CheckOracleError(PlainDriver, ErrorHandle, PlainDriver.StmtPrepare(Handle,
      ErrorHandle, Pointer(SQL), Length(SQL)+1, OCI_NTV_SYNTAX, OCI_DEFAULT),
      lcExecute, SQL, ConSettings);
  end;
end;

{**
  Gets a number of updates made by executed Oracle statement.
  @param PlainDriver an Oracle plain driver.
  @param Handle a holder for Statement handle.
  @param ErrorHandle a holder for Error handle.
  @returns a number of updates.
}
function GetOracleUpdateCount(const PlainDriver: IZOraclePlainDriver;
  const Handle: POCIStmt; const ErrorHandle: POCIError): ub4;
begin
  Result := 0;
  PlainDriver.AttrGet(Handle, OCI_HTYPE_STMT, @Result, nil,
    OCI_ATTR_ROW_COUNT, ErrorHandle);
end;

{**
  recurses down the field's TDOs and saves the little bits it need for later
  use on a fetch SQLVar._obj
}
function DescribeObject(const PlainDriver: IZOraclePlainDriver; const Connection: IZConnection;
  ParamHandle: POCIParam; stmt_handle: POCIHandle; Level: ub2): POCIObject;
var
  type_ref: POCIRef;
  ConSettings: PZConSettings;

  function AllocateObject: POCIObject;
  begin
    Result := New(POCIObject);
    FillChar(Result^, SizeOf(TOCIObject), {$IFDEF Use_FastCodeFillChar}#0{$ELSE}0{$ENDIF});
  end;

  procedure DescribeObjectByTDO(const PlainDriver: IZOraclePlainDriver;
    const Connection: IZConnection; var obj: POCIObject);
  var
    FConnection: IZOracleConnection;
    list_attibutes: POCIParam;
    name: PAnsiChar;
    temp: RawByteString;
    len: ub4;
    I: ub2;
    Fld: POCIObject;
  begin
    FConnection := Connection as IZOracleConnection;

    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.DescribeAny(FConnection.GetContextHandle,
        FConnection.GetErrorHandle, obj.tdo, 0, OCI_OTYPE_PTR, OCI_DEFAULT,
        OCI_PTYPE_TYPE, FConnection.GetDescribeHandle),
      lcOther, 'OCIDescribeAny(OCI_PTYPE_TYPE) of OCI_OTYPE_PTR', ConSettings);

    //we have the Actual TDO  so lets see what it is made up of by a describe
    Len := 0;  //and we store it in the object's paramdp for now
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(FConnection.GetDescribeHandle, OCI_HTYPE_DESCRIBE,
        @obj.parmdp, @Len, OCI_ATTR_PARAM, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_HTYPE_DESCRIBE) of OCI_ATTR_PARAM', ConSettings);

    //Get the SchemaName of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @name, @len, OCI_ATTR_SCHEMA_NAME, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_SCHEMA_NAME) of OCI_DTYPE_PARAM', ConSettings);

    ZSetString(name, len, temp{%H-});
    Obj.type_schema := ConSettings^.ConvFuncs.ZRawToString(temp,
      ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

    //Get the TypeName of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @name, @len, OCI_ATTR_NAME, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_NAME) of OCI_DTYPE_PARAM', ConSettings);

    ZSetString(name, len, temp);
    Obj.type_name := ConSettings^.ConvFuncs.ZRawToString(temp,
      ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

    //Get the TypeCode of the Object
    CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
      PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
        @Obj.typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
      lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE) of OCI_DTYPE_PARAM', ConSettings);

    if (obj.typecode = OCI_TYPECODE_OBJECT ) or ( obj.typecode = OCI_TYPECODE_OPAQUE) then
    begin
      //we will need a reff to the TDO for the pin operation
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.obj_ref, nil, OCI_ATTR_REF_TDO, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_REF_TDO) of OCI_DTYPE_PARAM', ConSettings);

      //now we'll pin the object
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.ObjectPin(FConnection.GetConnectionHandle, FConnection.GetErrorHandle,
          Obj.obj_ref, nil, OCI_PIN_LATEST, OCI_DURATION_SESSION, pub2(OCI_LOCK_NONE),
          @obj.obj_type),
        lcOther, 'OCIObjectPin(OCI_PIN_LATEST, OCI_DURATION_SESSION, OCI_LOCK_NONE)', ConSettings);
      Obj.Pinned := True;

      //is the object the final type or an type-descriptor?
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.is_final_type, nil, OCI_ATTR_IS_FINAL_TYPE, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_IS_FINAL_TYPE) of OCI_DTYPE_PARAM(SubType)', ConSettings);

      //Get the FieldCount
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @Obj.field_count, nil, OCI_ATTR_NUM_TYPE_ATTRS, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_NUM_TYPE_ATTRS) of OCI_DTYPE_PARAM(SubType)', ConSettings);

      //now get the differnt fields of this object add one field object for property
      SetLength(Obj.fields, Obj.field_count);

      //a field is just another instance of an obj not a new struct
      CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
        PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
          @list_attibutes, nil, OCI_ATTR_LIST_TYPE_ATTRS, FConnection.GetErrorHandle),
        lcOther, 'OCIAttrGet(OCI_ATTR_LIST_TYPE_ATTRS) of OCI_DTYPE_PARAM(SubType)', ConSettings);

      if obj.field_count > 0 then
        for I := 0 to obj.field_count-1 do
        begin
          Fld := AllocateObject;  //allocate a new object
          Obj.fields[i] := Fld;  //assign the object to the field-list

          CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
            PlainDriver.ParamGet(list_attibutes, OCI_DTYPE_PARAM,
              FConnection.GetErrorHandle, Fld.parmdp, I+1),
            lcOther, 'OCIParamGet(OCI_DTYPE_PARAM) of OCI_DTYPE_PARAM(Element)', ConSettings);

          // get the name of the attribute
          len := 0;
          CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
            PlainDriver.AttrGet(Fld.parmdp, OCI_DTYPE_PARAM,
              @name, @len, OCI_ATTR_NAME, FConnection.GetErrorHandle),
            lcOther, 'OCIAttrGet(OCI_ATTR_NAME) of OCI_DTYPE_PARAM(Element)', ConSettings);

          ZSetString(name, len, temp);
          Fld.type_name := ConSettings^.ConvFuncs.ZRawToString(temp,
            ConSettings^.ClientCodePage^.CP, ConSettings^.CTRL_CP);

          // get the typeCode of the attribute
          CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
            PlainDriver.AttrGet(Fld.parmdp, OCI_DTYPE_PARAM,
              @Fld.typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
            lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE) of OCI_DTYPE_PARAM(Element)', ConSettings);

          if (fld.typecode = OCI_TYPECODE_OBJECT) or
             (fld.typecode = OCI_TYPECODE_VARRAY) or
             (fld.typecode = OCI_TYPECODE_TABLE) or
             (fld.typecode = OCI_TYPECODE_NAMEDCOLLECTION) then
            //this is some sort of object or collection so lets drill down some more
            fld.next_subtype := DescribeObject(PlainDriver, Connection, fld.parmdp,
              obj.stmt_handle, obj.Level+1);
        end;
      end
      else
      begin
        //this is an embedded table or varray of some form so find out what is in it*/

        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
            @obj.col_typecode, nil, OCI_ATTR_COLLECTION_TYPECODE, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_COLLECTION_TYPECODE) of OCI_DTYPE_PARAM', ConSettings);

        //first get what sort of collection it is by coll typecode
        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
            @obj.parmap, nil, OCI_ATTR_COLLECTION_ELEMENT, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_COLLECTION_ELEMENT) of OCI_DTYPE_PARAM', ConSettings);

        CheckOracleError(PlainDriver, FConnection.GetErrorHandle,
          PlainDriver.AttrGet(obj.parmdp, OCI_DTYPE_PARAM,
            @obj.elem_typecode, nil, OCI_ATTR_TYPECODE, FConnection.GetErrorHandle),
          lcOther, 'OCIAttrGet(OCI_ATTR_TYPECODE of Element) of OCI_DTYPE_PARAM', ConSettings);

        if (obj.elem_typecode = OCI_TYPECODE_OBJECT) or
           (obj.elem_typecode = OCI_TYPECODE_VARRAY) or
           (obj.elem_typecode = OCI_TYPECODE_TABLE) or
           (obj.elem_typecode = OCI_TYPECODE_NAMEDCOLLECTION) then
          //this is some sort of object or collection so lets drill down some more
          obj.next_subtype := DescribeObject(PlainDriver, Connection, obj.parmap,
            obj.stmt_handle, obj.Level+1);
      end;
  end;
begin
  ConSettings := Connection.GetConSettings;

  Result := AllocateObject;

  //Describe the field (OCIParm) we know it is a object or a collection

  //Get the Actual TDO
  CheckOracleError(PlainDriver, (Connection as IZOracleConnection).GetErrorHandle,
    PlainDriver.AttrGet(ParamHandle, OCI_DTYPE_PARAM, @type_ref, nil,
      OCI_ATTR_REF_TDO, (Connection as IZOracleConnection).GetErrorHandle),
    lcOther, 'OCIAttrGet OCI_ATTR_REF_TDO of OCI_DTYPE_PARAM', ConSettings);

  CheckOracleError(PlainDriver, (Connection as IZOracleConnection).GetErrorHandle,
    PlainDriver.TypeByRef((Connection as IZOracleConnection).GetConnectionHandle,
      (Connection as IZOracleConnection).GetErrorHandle, type_ref,
      OCI_DURATION_TRANS, OCI_TYPEGET_ALL, @Result.tdo),
    lcOther, 'OCITypeByRef from OCI_ATTR_REF_TDO', ConSettings);
  Result^.Level := Level;
  DescribeObjectByTDO(PlainDriver, Connection, Result);
end;

procedure OraWriteLob(const PlainDriver: IZOraclePlainDriver; const BlobData: Pointer;
  const ContextHandle: POCISvcCtx; const ErrorHandle: POCIError;
  const LobLocator: POCILobLocator; const ChunkSize: Integer;
  BlobSize: Int64; Const BinaryLob: Boolean; const ConSettings: PZConSettings);
var
  Status: sword;
  ContentSize, OffSet: ub4;

  function DoWrite(AOffSet: ub4; AChunkSize: ub4; APiece: ub1): sword;
  var
    AContentSize: ub4;
  begin
    if BinaryLob then
    begin
      AContentSize := ContentSize;
      Result := PlainDriver.LobWrite(ContextHandle, ErrorHandle, LobLocator,
        AContentSize, AOffSet, (PAnsiChar(BlobData)+OffSet), AChunkSize, APiece,
        nil, nil, 0, SQLCS_IMPLICIT);
    end
    else
    begin
      if ContentSize > 0 then
        AContentSize := ConSettings^.ClientCodePage^.CharWidth
      else
      begin
        AContentSize := ContentSize;
        AChunkSize := ConSettings^.ClientCodePage^.CharWidth;
      end;

      Result := PlainDriver.LobWrite(ContextHandle, ErrorHandle, LobLocator,
        AContentSize, AOffSet, (PAnsiChar(BlobData)+OffSet), AChunkSize, APiece,
        nil, nil, ConSettings^.ClientCodePage^.ID, SQLCS_IMPLICIT);
    end;
    ContentSize := AContentSize;
    inc(OffSet, AChunkSize);
  end;
begin

  { Opens a large object or file for read. }
  Status := PlainDriver.LobOpen(ContextHandle, ErrorHandle, LobLocator, OCI_LOB_READWRITE);
  CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Open Large Object', ConSettings);

  { Checks for empty blob.}
  { This test doesn't use IsEmpty because that function does allow for zero length blobs}
  if (BlobSize > 0) then
  begin
    if not BinaryLob then
      BlobSize := BlobSize-1;
    if BlobSize > ChunkSize then
    begin
      OffSet := 0;
      ContentSize := 0;

      Status := DoWrite(1, ChunkSize, OCI_FIRST_PIECE);
      if Status <> OCI_NEED_DATA then
        CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Write Large Object', ConSettings);

      if (BlobSize - OffSet) > ChunkSize then
        while (BlobSize - OffSet) > ChunkSize do //take care there is room left for LastPiece
        begin
          Status := DoWrite(offset, ChunkSize, OCI_NEXT_PIECE);
          if Status <> OCI_NEED_DATA then
            CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Write Large Object', ConSettings);
        end;
      Status := DoWrite(offset, BlobSize - OffSet, OCI_LAST_PIECE);
    end
    else
    begin
      ContentSize := BlobSize;
      Status := PlainDriver.LobWrite(ContextHandle, ErrorHandle, LobLocator,
        ContentSize, 1, BlobData, BlobSize, OCI_ONE_PIECE, nil, nil, 0, SQLCS_IMPLICIT);
    end;
  end
  else
    Status := PlainDriver.LobTrim(ContextHandle, ErrorHandle, LobLocator, 0);

  CheckOracleError(PlainDriver, ErrorHandle,
    Status, lcOther, 'Write Large Object', ConSettings);

  { Closes large object or file. }
  Status := PlainDriver.LobClose(ContextHandle, ErrorHandle, LobLocator);
  CheckOracleError(PlainDriver, ErrorHandle, Status, lcOther, 'Close Large Object', ConSettings);
end;

{$ENDIF ZEOS_DISABLE_ORACLE}

end.


