{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{          Abstract Read/Only Dataset component           }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2012 Zeos Development Group       }
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

unit ZAbstractRODataset;

interface

{$I ZComponent.inc}
{$IF defined(ZEOS_TEST_ONLY) and defined(TEST_ZFIELDS)}
  {$IFNDEF WITH_ZSTRINGFIELDS}
    {$DEFINE WITH_ZSTRINGFIELDS}
  {$ENDIF}
{$IFEND}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Variants, Types, SysUtils, Classes, FMTBcd, {$IFNDEF FPC}SqlTimSt,{$ENDIF}
  {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF},
  ZSysUtils, ZAbstractConnection, ZDbcIntfs, ZSqlStrings,
  {$IFNDEF NO_UNIT_CONTNRS}Contnrs, {$ENDIF}ZDbcCache, ZDbcCachedResultSet, ZCompatibility, ZExpression, ZClasses
  {$IFDEF WITH_GENERIC_TLISTTFIELD}, Generics.Collections{$ENDIF};

type
  {$IFDEF xFPC} // fixed in r3943 or earlier 2006-06-25
  TUpdateStatusSet = set of TUpdateStatus;

  EUpdateError = class(EDatabaseError)
  end;
  {$ENDIF}

  {$IF NOT DECLARED(TRecordBuffer)}
  TRecordBuffer = {$IFDEF WITH_TRECBUF_PBYTE}TRecBuf{$ELSE}PChar{$ENDIF};
  {$IFEND}

  TGetCalcFieldsParamType = {$IFDEF WITH_GETCALCFIELDS_TRECBUF}TRecBuf{$ELSE}TRecordBuffer{$ENDIF};

  TSortType = (stAscending, stDescending, stIgnored);   {bangfauzan addition}

  {** Options for dataset. }
  TZDatasetOption = ({$IFNDEF NO_TDATASET_TRANSLATE}doOemTranslate, {$ENDIF}
    doCalcDefaults, doAlwaysDetailResync,
    doSmartOpen, doPreferPrepared, doDontSortOnPost, doUpdateMasterFirst,
    doCachedLobs, doAlignMaxRequiredWideStringFieldSize, doNoAlignDisplayWidth);

  {** Set of dataset options. }
  TZDatasetOptions = set of TZDatasetOption;

  // Forward declarations.
  TZAbstractRODataset = class;

  {** Implements a Zeos specific database exception with SQL error code. }
  EZDatabaseError = class(EDatabaseError)
  private
    FErrorCode: Integer;
    FStatusCode: String;
    procedure SetStatusCode(const Value: String);
   public
    constructor Create(const Msg: string);
    constructor CreateFromException(E: EZSQLThrowable);

    property ErrorCode: Integer read FErrorCode write FErrorCode;
    property StatusCode: String read FStatusCode write SetStatusCode;
  end;

  {** Dataset Linker class. }
  TZDataLink = class(TMasterDataLink)
  private
    FDataset: TZAbstractRODataset;
  protected
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADataset: TZAbstractRODataset); {$IFDEF FPC}reintroduce;{$ENDIF}
  end;
  TZFieldDef = Class;

{$IFNDEF TFIELDEFS_OWNER_IS_TPERSISTENT}
  TFieldDefClass = class of TFieldDef;
  TFieldDefsClass = class of TFieldDefs;
{$ENDIF}

{$IFNDEF WITH_FIELDDEFLIST}
{ TFlatList }

  TFlatList = class(TStringList)
  private
    FDataSet: TDataSet;
    FLocked: Boolean;
    FUpdated: Boolean;
  protected
    procedure ListChanging(Sender: TObject);
    function FindItem(const Name: string; MustExist: Boolean): TObject; virtual;
    function GetCount: Integer; override;
    function GetUpdated: Boolean; virtual;
    procedure UpdateList; virtual; abstract;
    property Updated: Boolean read GetUpdated write FUpdated;
    property Locked: Boolean read FLocked write FLocked;
  public
    constructor Create(ADataSet: TDataSet); virtual;
    procedure Update;
    property DataSet: TDataSet read FDataSet;
  end;

{ TFieldDefList }

  TZFieldDefList = class(TFlatList)
  private
    function GetFieldDef(Index: Integer): TZFieldDef;
  protected
    function GetUpdated: Boolean; override;
    procedure UpdateList; override;
  public
    function FieldByName(const Name: string): TZFieldDef;
    function Find(const Name: string): TZFieldDef; reintroduce;
    property FieldDefs[Index: Integer]: TZFieldDef read GetFieldDef; default;
  end;

  TFieldDefListClass = class of TZFieldDefList;

  {$ENDIF WITH_FIELDDEFLIST}
  TStringFieldSetter = procedure(ColumnIndex: Integer; Buffer: PAnsiChar) of object;
  TStringFieldGetter = function(ColumnIndex, FieldSize: Integer; Buffer: PAnsiChar): Boolean of object;
  TWideStringFieldGetter = function(ColumnIndex, FieldSize: Integer; Buffer: PWideChar): Boolean of object;

  {$IFNDEF WITH_TDATASETFIELD}
  TDataSetField = class;
  {$ENDIF WITH_TDATASETFIELD}
  {** Abstract dataset component optimized for read/only access. }
  {$IFDEF WITH_WIDEDATASET}
  TZAbstractRODataset = class(TWideDataSet)
  {$ELSE}
  TZAbstractRODataset = class(TDataSet)
  {$ENDIF}
  private
{$IFNDEF WITH_FUNIDIRECTIONAL}
    FUniDirectional: Boolean;
{$ENDIF}
{$IFNDEF WITH_FIELDDEFLIST}
    FFieldDefList: TZFieldDefList;
{$ENDIF WITH_FIELDDEFLIST}
    FCurrentRow: Integer;
    FRowAccessor: TZRowAccessor;
    FOldRowBuffer: PZRowBuffer;
    FNewRowBuffer: PZRowBuffer;
    FCurrentRows: TZSortedList;
    FFetchCount: Integer;
    FFieldsLookupTable: TPointerDynArray;
    FRowsAffected: Integer;

    FFilterEnabled: Boolean;
    FFilterExpression: IZExpression;
    FFilterStack: TZExecutionStack;
    FFilterFieldRefs: TObjectDynArray;
    FInitFilterFields: Boolean;

    FRequestLive: Boolean;
    FFetchRow: integer;    // added by Patyi

    FSQL: TZSQLStrings;
    FParams: TParams;
    FShowRecordTypes: TUpdateStatusSet;
    FOptions: TZDatasetOptions;

    FProperties: TStrings;
    FConnection: TZAbstractConnection;
    FStatement: IZPreparedStatement;
    FResultSet: IZResultSet;

    FRefreshInProgress: Boolean;
    FNativeFormatOverloadCalled: Boolean; //circumvent a TClientDataSet BCDField bug.
      //The ClientDataSets do not call the Get/SetData overload with NativeFormat overload
      //so they use the slow TBCD record instead (while we are in Currency range)
      //and convert all values to/from the currency
      //Zitat of DB.TBCDField.GetDataSize: Integer:
      //"SizeOf(TBcd) is used here instead of SizeOf(Currency) because some
      // datasets store the currency data in TBcd format in the record buffer.
      // For these classes (TBDEDataset & TClientDataset) a call to
      // TField.GetData(Buffer, True) will return a TBcd."

    {FFieldDefsInitialized: boolean;}  // commented out because this causes SF#286

    FDataLink: TDataLink;
    FMasterLink: TMasterDataLink;
    FLinkedFields: string; {renamed by bangfauzan}
    FIndexFieldNames : String; {bangfauzan addition}

    FIndexFields: {$IFDEF WITH_GENERIC_TLISTTFIELD}TList<TField>{$ELSE}TList{$ENDIF};

    FSortType : TSortType; {bangfauzan addition}

    FLastRowFetched: Boolean;
    FSortedFields: string;
    FSortedFieldRefs: TObjectDynArray;
    FSortedFieldIndices: TIntegerDynArray;
    FSortedComparsionKinds: TComparisonKindArray;
    FSortedOnlyDataFields: Boolean;
    FCompareFuncs: TCompareFuncs;
    FSortRowBuffer1: PZRowBuffer;
    FSortRowBuffer2: PZRowBuffer;
    FPrepared: Boolean;
    FDoNotCloseResultset: Boolean;
    FUseCurrentStatment: Boolean;
    FUseZFields: Boolean;
    FStringFieldSetter: TStringFieldSetter;
    FStringFieldGetter: TStringFieldGetter;
    FWideStringFieldGetter: TWideStringFieldGetter;
    {$IFNDEF WITH_NESTEDDATASETS}
    FNestedDataSets: TList;
    {$ENDIF}
    {$IFNDEF WITH_NESTEDDATASETCLASS}
    FNestedDatasetClass: TDataSetClass;
    {$ENDIF}
    {$IFNDEF WITH_DATASETFIELD}
    FDataSetField: TDataSetField;
    {$ENDIF}
    {$IFNDEF WITH_OBJECTVIEW}
    FObjectView: Boolean;
    {$ENDIF WITH_OBJECTVIEW}
    {$IFNDEF WITH_SPARSEARRAYS}
    FSparseArrays: Boolean;
    procedure SetSparseArrays(Value: Boolean);
    {$ENDIF WITH_SPARSEARRAYS}
    {$IFNDEF WITH_NESTEDDATASETS}
    function GetNestedDataSets: TList;
    {$ENDIF}
    procedure SetStringFieldSetterAndSetter;
    {$IFDEF WITH_ZSTRINGFIELDS}
    procedure SetUseZFields(const Value: Boolean);
    {$ENDIF}
    {$IFNDEF UNICODE}
    procedure StringFieldSetterFromRawAutoEncode(ColumnIndex: Integer; Buffer: PAnsiChar);
    procedure StringFieldSetterFromRaw(ColumnIndex: Integer; Buffer: PAnsiChar);
    {$ELSE}
    procedure StringFieldSetterFromAnsi(ColumnIndex: Integer; Buffer: PAnsiChar);
    {$ENDIF}
    procedure StringFieldSetterRawToUnicode(ColumnIndex: Integer; Buffer: PAnsiChar);
    function StringFieldGetterFromUnicode(ColumnIndex, FieldSize: Integer; Buffer: PAnsiChar): Boolean;
    function StringFieldGetterFromAnsiRec(ColumnIndex, FieldSize: Integer; Buffer: PAnsiChar): Boolean;
    function StringFieldGetterRaw2RawConvert(ColumnIndex, FieldSize: Integer; Buffer: PAnsiChar): Boolean;
    function WideStringGetterFromUnicode(ColumnIndex, FieldSize: Integer; Buffer: PWideChar): Boolean;
    function WideStringGetterFromRaw(ColumnIndex, FieldSize: Integer; Buffer: PWideChar): Boolean;
  private
    function GetReadOnly: Boolean;
    procedure SetReadOnly(Value: Boolean);
    function GetSQL: TStrings;
    procedure SetSQL(Value: TStrings);
    function GetParamCheck: Boolean;
    procedure SetParamCheck(Value: Boolean);
    function GetParamChar: Char;
    procedure SetParamChar(Value: Char);
    procedure SetParams(Value: TParams);
    function GetShowRecordTypes: TUpdateStatusSet;
    procedure SetShowRecordTypes(Value: TUpdateStatusSet);
    procedure SetConnection(Value: TZAbstractConnection);
    procedure SetDataSource(Value: TDataSource);
    function GetMasterFields: string;
    procedure SetMasterFields(const Value: string);
    function GetMasterDataSource: TDataSource;
    procedure SetMasterDataSource(Value: TDataSource);
    function GetLinkedFields: string; {renamed by bangfauzan}
    procedure SetLinkedFields(const Value: string);  {renamed by bangfauzan}
    function GetIndexFieldNames : String; {bangfauzan addition}
    procedure SetIndexFieldNames(const Value : String); {bangfauzan addition}
    procedure SetOptions(Value: TZDatasetOptions);
    procedure SetSortedFields({const} Value: string); {bangfauzan modification}
    procedure SetProperties(const Value: TStrings);

    function GetSortType : TSortType; {bangfauzan addition}
    Procedure SetSortType(Value : TSortType); {bangfauzan addition}

    procedure UpdateSQLStrings(Sender: TObject);
    procedure ReadParamData(Reader: TReader);
    procedure WriteParamData(Writer: TWriter);

    procedure SetPrepared(Value : Boolean);
    {$IFNDEF WITH_FUNIDIRECTIONAL}
    procedure SetUniDirectional(const Value: boolean);
    {$ENDIF}
    function  GetUniDirectional: boolean;

  protected
    procedure CheckOpened;
    procedure CheckConnected;
    procedure CheckBiDirectional;
    procedure CheckSQLQuery; virtual;
    procedure RaiseReadOnlyError;

    function FetchOneRow: Boolean;
    function FetchRows(RowCount: Integer): Boolean;
    function FilterRow(RowNo: NativeInt): Boolean;
    function GotoRow(RowNo: NativeInt): Boolean; // added by tohenk
    procedure RereadRows;
    procedure SetStatementParams(Statement: IZPreparedStatement;
      ParamNames: TStringDynArray; Params: TParams;
      DataLink: TDataLink); virtual;
    procedure MasterChanged(Sender: TObject);
    procedure MasterDisabled(Sender: TObject);
    procedure DoOnNewRecord; override;

    function GetDataSource: TDataSource; override;
  protected { Internal protected properties. }
    function CreateStatement(const SQL: string; Properties: TStrings):
      IZPreparedStatement; virtual;
    function CreateResultSet(const {%H-}SQL: string; MaxRows: Integer):
      IZResultSet; virtual;
    {$IFDEF HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES} //EH: there is something weired with cirtcular references + FPC and implementation uses! So i added this virtual function to get a IsUpdatable state
    function GetUpdatable: Boolean; virtual;
    property Updatable: Boolean read GetUpdatable;
    {$ENDIF}
    property RowAccessor: TZRowAccessor read FRowAccessor write FRowAccessor;
    property CurrentRow: Integer read FCurrentRow write FCurrentRow;
    property OldRowBuffer: PZRowBuffer read FOldRowBuffer write FOldRowBuffer;
    property NewRowBuffer: PZRowBuffer read FNewRowBuffer write FNewRowBuffer;
    property CurrentRows: TZSortedList read FCurrentRows write FCurrentRows;
    property FetchCount: Integer read FFetchCount write FFetchCount;
    property FieldsLookupTable: TPointerDynArray read FFieldsLookupTable
      write FFieldsLookupTable;

    property FilterEnabled: Boolean read FFilterEnabled write FFilterEnabled;
    property FilterExpression: IZExpression read FFilterExpression
      write FFilterExpression;
    property FilterStack: TZExecutionStack read FFilterStack write FFilterStack;
    property FilterFieldRefs: TObjectDynArray read FFilterFieldRefs
      write FFilterFieldRefs;
    property InitFilterFields: Boolean read FInitFilterFields
      write FInitFilterFields;

    property Statement: IZPreparedStatement read FStatement write FStatement;
    property ResultSet: IZResultSet read FResultSet write FResultSet;

  protected { External protected properties. }
    property DataLink: TDataLink read FDataLink;
    property MasterLink: TMasterDataLink read FMasterLink;
    property IndexFields: {$IFDEF WITH_GENERIC_TLISTTFIELD}TList<TField>{$ELSE}TList{$ENDIF} read FIndexFields;
    property RequestLive: Boolean read FRequestLive write FRequestLive
      default False;
    property FetchRow: integer read FFetchRow write FFetchRow default 0;  // added by Patyi
    property ParamCheck: Boolean read GetParamCheck write SetParamCheck
      default True;
    property ParamChar: Char read GetParamChar write SetParamChar
      default ':';
    property SQL: TStrings read GetSQL write SetSQL;
    property Params: TParams read FParams write SetParams;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default True;
    property ShowRecordTypes: TUpdateStatusSet read GetShowRecordTypes
      write SetShowRecordTypes default [usUnmodified, usModified, usInserted];
    property IsUniDirectional: Boolean read GetUniDirectional
      write SetUniDirectional default False;
    property Properties: TStrings read FProperties write SetProperties;
    property Options: TZDatasetOptions read FOptions write SetOptions
      default [doCalcDefaults];
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property MasterFields: string read GetMasterFields
      write SetMasterFields;
    property MasterSource: TDataSource read GetMasterDataSource
      write SetMasterDataSource;
    property LinkedFields: string read GetLinkedFields
      write SetLinkedFields; {renamed by bangfauzan}
    property IndexFieldNames:String read GetIndexFieldNames
      write SetIndexFieldNames; {bangfauzan addition}
    property DoNotCloseResultset: Boolean read FDoNotCloseResultset;
    {$IFNDEF WITH_NESTEDDATASETS}
    property NestedDataSets: TList read GetNestedDataSets;
    {$ENDIF}
    {$IFNDEF WITH_NESTEDDATASETCLASS}
    property NestedDataSetClass: TDataSetClass read FNestedDataSetClass write FNestedDataSetClass;
    {$ENDIF}
  protected { Abstracts methods }
    {$IFNDEF WITH_InternalAddRecord_TRecBuf}
    procedure InternalAddRecord({%H-}Buffer: Pointer; {%H-}Append: Boolean); override;
    {$ELSE}
    procedure InternalAddRecord(Buffer: TRecBuf; Append: Boolean); override;
    {$ENDIF}
    procedure InternalDelete; override;
    procedure InternalPost; override;
    {$IFNDEF FPC}
    procedure SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
      NativeFormat: Boolean); override;
    procedure SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF}); override;
    {$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    function GetActiveBuffer(out RowBuffer: PZRowBuffer): Boolean;
    {$IFNDEF WITH_AllocRecBuf_TRecBuf}
    function AllocRecordBuffer: TRecordBuffer; override;
    {$ELSE}
    function AllocRecBuf: TRecBuf; override;
    {$ENDIF}
    {$IFNDEF WITH_FreeRecBuf_TRecBuf}
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    {$ELSE}
    procedure FreeRecBuf(var Buffer: TRecBuf); override;
    {$ENDIF}
    function CreateNestedDataSet({%H-}DataSetField: TDataSetField): TDataSet; {$IFDEF WITH_FTDATASETSUPPORT}override;{$ENDIF}
    procedure CloseBlob({%H-}Field: TField); override;

    procedure CheckFieldCompatibility(Field: TField; AFieldDef: TFieldDef); {$IFDEF WITH_CHECKFIELDCOMPATIBILITY} override;{$ENDIF}
    procedure CreateFields; override;

    procedure ClearCalcFields(Buffer: TRecordBuffer); override;

    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    {$IFDEF WITH_InternalGotoBookmark_TBookmark}
    procedure InternalGotoBookmark(Bookmark: TBookmark); override;
    {$ELSE}
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    {$ENDIF}
    procedure InternalRefresh; override;
    procedure InternalHandleException; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer;
      Data:{$IFDEF WITH_BOOKMARKDATA_TBOOKMARK}TBookMark{$ELSE}Pointer{$ENDIF}); override;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer;
      Data: {$IFDEF WITH_BOOKMARKDATA_TBOOKMARK}TBookMark{$ELSE}Pointer{$ENDIF}); override;
{$IFNDEF WITH_FIELDDEFLIST}
  protected {indirect creation of internal objects}
    function GetFieldDefListClass: TFieldDefListClass; virtual;
{$ENDIF}
{$IFNDEF WITH_VIRTUAL_DEFCHANGED}
    procedure DefChanged(Sender: TObject); virtual;
{$ENDIF}
    {$IFNDEF WITH_DATASETFIELD}
    procedure SetDataSetField(const Value: TDataSetField); virtual;
    {$ENDIF}
    function InternalLocate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): LongInt;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterText(const Value: string); override;
    {$IFNDEF WITH_OBJECTVIEW}
    procedure SetObjectView(const Value: Boolean);
    {$ENDIF WITH_OBJECTVIEW}
    procedure SetAnotherResultset(const Value: IZResultSet);
    procedure InternalSort;
    function ClearSort(Item1, Item2: Pointer): Integer;
    function HighLevelSort(Item1, Item2: Pointer): Integer;
    function LowLevelSort(Item1, Item2: Pointer): Integer;

    function GetCanModify: Boolean; override;
    function GetRecNo: Integer; override;
    function GetRecordCount: Integer; override;
    procedure MoveRecNo(Value: Integer);
    procedure SetRecNo(Value: Integer); override;
    function IsCursorOpen: Boolean; override;

    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;

    procedure RefreshParams; virtual;

    procedure InternalPrepare; virtual;
    procedure InternalUnPrepare; virtual;
  protected
  {$IFDEF WITH_IPROVIDER}
    procedure PSStartTransaction; override;
    procedure PSEndTransaction(Commit: Boolean); override;
    // Silvio Clecio
    {$IFDEF WITH_IPROVIDERWIDE}
    function PSGetTableNameW: WideString; override;
    function PSGetQuoteCharW: WideString; override;
    function PSGetKeyFieldsW: WideString; override;
    procedure PSSetCommandText(const CommandText: WideString); overload; override;
    procedure PSSetCommandText(const CommandText: string); overload; override;
    //??     function PSGetCommandTextW: WideString; override;
    function PSExecuteStatement(const ASQL: WideString; AParams: TParams;
      ResultSet: Pointer = nil): Integer; override;
    {$ELSE}
    function PSGetTableName: string; override;
    function PSGetQuoteChar: string; override;
    function PSGetKeyFields: string; override;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      {$IFDEF WITH_IProviderSupportNG}var ResultSet: TDataSet
      {$ELSE} {%H-}ResultSet: Pointer = nil{$ENDIF}): Integer; override;
    procedure PSSetCommandText(const CommandText: string); override;
    {$ENDIF}
    function PSGetUpdateException(E: Exception;
      Prev: EUpdateError): EUpdateError; override;
    function PSIsSQLSupported: Boolean; override;
    procedure PSReset; override;
    function PSUpdateRecord({%H-}UpdateKind: TUpdateKind;
      {%H-}Delta: TDataSet): Boolean; override;
    procedure PSExecute; override;
    function PSGetParams: TParams; override;
    procedure PSSetParams(AParams: TParams); override;
    function PSInTransaction: Boolean; override;
  {$ENDIF}
  function PSIsSQLBased: Boolean; {$IFDEF WITH_IPROVIDER}override{$ELSE}virtual{$ENDIF};

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FetchAll; virtual;  // added by Patyi
    procedure ExecSQL; virtual;
    function RowsAffected: LongInt;
    function ParamByName(const Value: string): TParam;

    {$IFDEF FPC} // FPC has these methods virtual plainly returning False while on Delphi they use FindRecord
    function FindFirst: Boolean; override;
    function FindLast: Boolean; override;
    function FindNext: Boolean; override;
    function FindPrior: Boolean; override;
    {$ENDIF}
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function IsSequenced: Boolean; override;

    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
      override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;

    function GetFieldData(Field: TField; {$IFDEF WITH_VAR_TVALUEBUFFER}var{$ENDIF}Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF}): Boolean; override;
    function GetFieldData(Field: TField; {$IFDEF WITH_VAR_TVALUEBUFFER}var{$ENDIF}Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
      NativeFormat: Boolean): Boolean; override;
    {$IFDEF FPC}
    procedure SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
      NativeFormat: Boolean); override;
    procedure SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF}); override;
    {$ENDIF}
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
      override;
    function UpdateStatus: TUpdateStatus; override;
    {$IFNDEF NO_TDATASET_TRANSLATE}
    function Translate(Src, Dest: PAnsiChar; ToOem: Boolean): Integer; override;
    {$ENDIF}
    procedure Prepare;
    procedure Unprepare;
    {$IFNDEF WITH_FIELDDEFLIST}
    property FieldDefList: TZFieldDefList read FFieldDefList;
    {$ENDIF WITH_FIELDDEFLIST}
  public
    property Active;
    property Prepared: Boolean read FPrepared write SetPrepared;
    property FieldDefs stored False;
    property DbcStatement: IZPreparedStatement read FStatement;
    property DbcResultSet: IZResultSet read FResultSet;
    {$IFNDEF WITH_OBJECTVIEW}
    property ObjectView: Boolean read FObjectView write SetObjectView;
    {$ENDIF WITH_OBJECTVIEW}
    {$IFNDEF WITH_SPARSEARRAYS}
    property SparseArrays: Boolean read FSparseArrays write SetSparseArrays;
    {$ENDIF WITH_SPARSEARRAYS}
    {$IFNDEF WITH_DATASETFIELD}
    property DataSetField: TDataSetField read FDataSetField write SetDataSetField;
    {$ENDIF}
  published
    property Connection: TZAbstractConnection read FConnection write SetConnection;
    property SortedFields: string read FSortedFields write SetSortedFields;
    property SortType : TSortType read FSortType write SetSortType
      default stAscending; {bangfauzan addition}

    property AutoCalcFields;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeRefresh;
    property AfterRefresh;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnFilterRecord;
    property Filter;
    property Filtered;
    {$IFDEF WITH_ZSTRINGFIELDS}
    property UseZFields: Boolean read FUseZFields write SetUseZFields default True;
    {$ENDIF}
  public
    function NextResultSet: Boolean; virtual;
  end;

  {$IFNDEF WITH_TFIELD_PARENTFIELD}
  TObjectField = class;
  {$ENDIF}

  TZField = class(TField)
  private
    FEmptyAsNull: Boolean;
    FFieldIndex: Integer;
    FRowBuffer: PZRowBuffer;
    FValidating: Boolean;
    FValueBuffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
    {$IFNDEF WITH_TFIELD_PARENTFIELD}
    FParentField: TObjectField;
    {$ENDIF}
    function GetActiveRowBuffer: Boolean;
    function IsFieldEditable: Boolean;
    {$IFNDEF WITH_FIELD_VALIDATELOOKUPINFO}
    procedure ValidateLookupInfo(All: Boolean);
    {$ENDIF}
  protected
    {$IFNDEF WITH_TFIELD_PARENTFIELD}
    procedure SetParentField(AField: TObjectField); virtual;
    {$ENDIF}
    {$IFNDEF WITH_TFIELD_FREEBUFFERS}
    procedure FreeBuffers; virtual;
    {$ENDIF}
    {ZGenerics}
    function GetValidationBuffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
    {property Assignments .... }
    function GetAsBoolean: Boolean; override;
    //function GetAsByteArray: Variant; virtual;
    { time values }
    function GetAsDateTime: TDateTime; override;
    function GetAsDate: TDateTime;
    function GetAsTime: TDateTime;
    //function GetAsSQLTimeStamp: TSQLTimeStamp; virtual;
    //function GetAsSQLTimeStampOffset: TSQLTimeStampOffset; virtual;

    { decimal/floating values}
    function GetAsCurrency: Currency; override;
    function GetAsBCD: TBcd; override;
    function GetAsSingle: Single; {$IFDEF WITH_FTSINGLE}override;{$ENDIF}
    function GetAsFloat: Double; override;
    function GetAsExtended: Extended; {$IFDEF WITH_FTEXTENDED}override;{$ENDIF}
    { signed integer values }
    function GetAsShortInt: ShortInt;
    function GetAsSmallInt: SmallInt;
    function GetAsInteger: Longint; override;
    function GetAsLargeInt: Largeint; {$IFDEF TFIELD_HAS_ASLARGEINT}override;{$ENDIF}
    { unsigned integer values }
    function GetAsByte: Byte;
    function GetAsWord: Word;
    function GetAsLongWord: LongWord; {$IFDEF TFIELD_HAS_ASLONGWORD}override;{$ENDIF}
    function GetAsUInt64: UInt64;
    { string values }
    function GetAsString: string; override;
    function GetAsWideString: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF}; {$IFDEF WITH_FTWIDESTRING}override;{$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    function GetAsAnsiString: AnsiString; {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    function GetAsUTF8String: UTF8String; {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    {$ENDIF}
    function GetAsRawByteString: RawByteString;
    { record/array types }
    function GetAsGuid: TGUID; {$IFDEF WITH_VIRTUAL_TFIELD_GETASGUID} override; {$ENDIF}
    function GetAsBytes: TBytes; {$IFDEF TFIELD_HAS_ASBYTES}override;{$ENDIF}
    function GetAsVariant: Variant; override;
    //function GetCanModify: Boolean; virtual;
    //function GetDataSize: Integer; virtual;
    //function GetDefaultWidth: Integer; virtual;}
    function GetIsNull: Boolean; override;
    {$IFNDEF WITH_VIRTUAL_GETHASCONSTRAINTS}
    function GetHasConstraints: Boolean; virtual;
    {$ENDIF}
    procedure SetAsBCD(const Value: TBcd); override;
    procedure SetAsBoolean(Value: Boolean); override;
    //procedure SetAsByteArray(const Value: Variant); virtual;
    procedure SetAsDateTime(Value: TDateTime); override;
    //procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); virtual;
    //procedure SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset); virtual;
    { decimal/floating values}
    procedure SetAsCurrency(Value: Currency); override;
    procedure SetAsSingle(Value: Single); {$IFDEF WITH_FTSINGLE}override;{$ENDIF}
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsExtended(Value: Extended); {$IFDEF WITH_FTEXTENDED}override;{$ENDIF}
    { signed integer values }
    procedure SetAsShortInt(Value: ShortInt); virtual;
    procedure SetAsSmallInt(Value: SmallInt); virtual;
    procedure SetAsInteger(Value: Longint); override;
    procedure SetAsLargeInt(Value: Largeint); {$IFDEF TFIELD_HAS_ASLARGEINT}override;{$ENDIF}
    { unsigned integer values }
    procedure SetAsByte(Value: Byte); virtual;
    procedure SetAsWord(Value: Word); virtual;
    procedure SetAsLongWord(Value: LongWord); {$IFDEF TFIELD_HAS_ASLONGWORD}override;{$ELSE}virtual;{$ENDIF}
    procedure SetAsUInt64(Value: UInt64); virtual;
    { string values }
    procedure SetAsString(const Value: string); override;
    procedure SetAsWideString(const Value: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF}); {$IFDEF WITH_FTWIDESTRING}override;{$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    procedure SetAsAnsiString(const Value: AnsiString); {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetAsUTF8String(const Value: UTF8String); {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    {$ENDIF}
    procedure SetAsRawByteString(const Value: RawByteString);

    procedure SetAsBytes(const Value: TBytes); {$IFDEF TFIELD_HAS_ASBYTES}override;{$ENDIF}
    procedure SetAsVariant(const Value: Variant); override;
    //procedure SetDataSet(ADataSet: TDataSet); virtual;
    //procedure SetText(const Value: string); virtual;
    //procedure SetWideText(const Value: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF}); virtual;
    //procedure SetVarValue(const Value: Variant); virtual;
    property FieldIndex: Integer read FFieldIndex write FFieldIndex;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Validate(Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF});
    property AsBCD;
    property AsBoolean;
    property AsByte: Byte read GetAsByte write SetAsByte;
    property AsCurrency;
    property AsDateTime;
    {$IFNDEF WITH_TFIELD_PARENTFIELD}
    property ParentField: TObjectField read FParentField write SetParentField;
    {$ENDIF}

    {$IFNDEF FPC}
    property AsSQLTimeStamp;
    {$ENDIF}
    {$IFDEF WITH_FTTIMESTAMPOFFSET}
    property AsSQLTimeStampOffset;
    {$ENDIF WITH_FTTIMESTAMPOFFSET}
    property AsSingle{$IFNDEF WITH_FTSINGLE}: Single read GetAsSingle write SetAsSingle{$ENDIF};
    property AsFloat;
    property AsExtended {$IFNDEF WITH_FTEXTENDED}: Extended read GetAsExtended write SetAsExtended{$ENDIF};
    property AsInteger;
    property AsLargeInt {$IFNDEF TFIELD_HAS_ASLARGEINT}: LargeInt read GetAsLargeInt write SetAsLargeInt{$ENDIF};
    property AsUInt64: UInt64 read GetAsUInt64 write SetAsUInt64;
    property AsString;
    property AsWideString{$IFNDEF WITH_FTWIDESTRING}: WideString read GetAsWideString write SetAsWideString{$ENDIF};
    {$IFNDEF NO_ANSISTRING}
    property AsAnsiString{$IFNDEF WITH_ASANSISTRING}: AnsiString read GetAsAnsiString write SetAsAnsiString{$ENDIF};
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    property AsUTF8String: UTF8String read GetAsUTF8String write SetAsUTF8String;
    {$ENDIF}
    property AsBytes{$IFNDEF WITH_ASBYTES}: TBytes read GetAsBytes write SetAsBytes{$ENDIF};
    property AsVariant;
    property AttributeSet;
    property Calculated;
    property CanModify;
    property CurValue;
    property DataSet;
    property DataSize;
    property DataType;
    property DisplayName;
    property DisplayText;
    property EditMask;
    property EditMaskPtr;
    property FieldNo;
    {$IFNDEF FPC}
    property FullName;
    {$ENDIF}
    property IsIndexField;
    property IsNull;
    property Lookup;
    property LookupList;
    property NewValue;
    property Offset;
    property OldValue;
    {$IFNDEF FPC}
    property ParentField;
    {$ENDIF}
    property Size;
    property Text;
    property ValidChars;
    property Value;
  published
    property EmptyStringAsNull: Boolean read FEmptyAsNull write FEmptyAsNull default False;
    property OnValidate;
  end;

  TZStringField = Class(TZField)
  private
    FFixedChar: Boolean;
    FTransliterate: Boolean;
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetDataSize: Integer; override;
    property Transliterate: Boolean read FTransliterate write FTransliterate default False;//we try to prevent this case!
  public
    constructor Create(AOwner: TComponent); override;
    {$IFNDEF NO_ANSISTRING}
    property Value: AnsiString read GetAsAnsiString write SetAsAnsiString;
    {$ENDIF}
  published
    property EditMask;
    property FixedChar: Boolean read FFixedChar write FFixedChar default False;
    property Size default 20;
  End;

  TZWideStringField = Class(TZStringField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF} read GetAsWideString write SetAsWideString;
  end;

  TZNumericField = Class(TZField)
  private
    FDisplayFormat: string;
    FEditFormat: string;
    FRangeCheck: Boolean;
    procedure CheckRange(const Value; const ValueType: TZSQLType); virtual; abstract;
  protected
    function ConvertSigned(const Value; const ValueType: TZSQLType): Int64;
    function ConvertUnSigned(const Value; const ValueType: TZSQLType): UInt64;
    function ConvertExtended(const Value; const ValueType: TZSQLType): Extended;

    procedure SetAsBCD(const Value: TBcd); override;
    procedure SetAsBoolean(Value: Boolean); override;
    //procedure SetAsByteArray(const Value: Variant); virtual;
    procedure SetAsDateTime(Value: TDateTime); override;
    //procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); virtual;
    //procedure SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset); virtual;
    { decimal/floating values}
    procedure SetAsCurrency(Value: Currency); override;
    procedure SetAsSingle(Value: Single); {$IFDEF WITH_FTSINGLE}override;{$ENDIF}
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsExtended(Value: Extended); {$IFDEF WITH_FTEXTENDED}override;{$ENDIF}
    { signed integer values }
    procedure SetAsShortInt(Value: ShortInt); override;
    procedure SetAsSmallInt(Value: SmallInt); override;
    procedure SetAsInteger(Value: Longint); override;
    procedure SetAsLargeInt(Value: Largeint); {$IFDEF TFIELD_HAS_ASLARGEINT}override;{$ENDIF}
    { unsigned integer values }
    procedure SetAsByte(Value: Byte); override;
    procedure SetAsWord(Value: Word); override;
    procedure SetAsLongWord(Value: LongWord); override;
    procedure SetAsUInt64(Value: UInt64); override;
    { string values }
    procedure SetAsString(const Value: string); override;
    procedure SetAsWideString(const Value: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF}); {$IFDEF WITH_FTWIDESTRING}override;{$ENDIF}
    {$IFNDEF NO_ANSISTRING}
    procedure SetAsAnsiString(const Value: AnsiString); {$IFDEF WITH_ASANSISTRING}override;{$ENDIF}
    {$ENDIF}
    {$IFNDEF NO_UTF8STRING}
    procedure SetAsUTF8String(const Value: UTF8String); {$IFDEF WITH_VIRTUAL_TFIELD_ASUTF8STRING}override;{$ENDIF}
    {$ENDIF}
    procedure SetAsRawByteString(const Value: RawByteString);
  protected
    procedure RangeError(Value, Min, Max: Extended);
    procedure SetDisplayFormat(const Value: string);
    procedure SetEditFormat(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment default taRightJustify;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property EditFormat: string read FEditFormat write SetEditFormat;
  end;

{ TZByteField }
  TZByteField = class(TZNumericField)
  private
    FMinValue: Byte;
    FMaxValue: Byte;
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
    procedure SetMaxValue(Value: Byte);
    procedure SetMinValue(Value: Byte);
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Byte read GetAsByte write SetAsByte;
  published
    property MaxValue: Byte read FMaxValue write SetMaxValue default 0;
    property MinValue: Byte read FMinValue write SetMinValue default 0;
  end;

{ TZShortIntField }

  TZShortIntField = class(TZNumericField)
  private
    FMinValue: ShortInt;
    FMaxValue: ShortInt;
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
    procedure SetMaxValue(Value: ShortInt);
    procedure SetMinValue(Value: ShortInt);
  protected
    function GetDataSize: Integer; override;
  public
    property Value: ShortInt read GetAsShortInt write SetAsShortInt;
    constructor Create(AOwner: TComponent); override;
  published
    property MaxValue: ShortInt read FMaxValue write SetMaxValue default 0;
    property MinValue: ShortInt read FMinValue write SetMinValue default 0;
  end;

{ TZWordField }

  TZWordField = class(TZNumericField)
  private
    FMinValue: Word;
    FMaxValue: Word;
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
    procedure SetMaxValue(Value: Word);
    procedure SetMinValue(Value: Word);
  protected
    function GetDataSize: Integer; override;
  public
    property Value: Word read GetAsWord write SetAsWord;
    constructor Create(AOwner: TComponent); override;
  published
    property MaxValue: Word read FMaxValue write SetMaxValue default 0;
    property MinValue: Word read FMinValue write SetMinValue default 0;
  end;

{ TZSmallIntField }

  TZSmallIntField = class(TZNumericField)
  private
    FMinValue: SmallInt;
    FMaxValue: SmallInt;
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
    procedure SetMaxValue(Value: SmallInt);
    procedure SetMinValue(Value: SmallInt);
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: SmallInt read GetAsSmallInt write SetAsSmallInt;
  published
    property MaxValue: SmallInt read FMaxValue write SetMaxValue default 0;
    property MinValue: SmallInt read FMinValue write SetMinValue default 0;
  end;

{ TZIntegerField }

  TZIntegerField = class(TZNumericField)
  private
    FMinValue: Longint;
    FMaxValue: Longint;
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
    procedure SetMaxValue(Value: Longint);
    procedure SetMinValue(Value: Longint);
  protected
    function GetDataSize: Integer; override;
  public
    property Value: Longint read GetAsInteger write SetAsInteger;
    constructor Create(AOwner: TComponent); override;
  published
    property MaxValue: Longint read FMaxValue write SetMaxValue default 0;
    property MinValue: Longint read FMinValue write SetMinValue default 0;
  end;

{ TZLongWordField }

  TZLongWordField = class(TZNumericField)
  private
    FMinValue: LongWord;
    FMaxValue: LongWord;
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
    procedure SetMaxValue(Value: LongWord);
    procedure SetMinValue(Value: LongWord);
  protected
    function GetDataSize: Integer; override;
  public
    property Value: LongWord read GetAsLongWord write SetAsLongWord;
    constructor Create(AOwner: TComponent); override;
  published
    property MaxValue: LongWord read FMaxValue write SetMaxValue default 0;
    property MinValue: LongWord read FMinValue write SetMinValue default 0;
  end;

{ TZInt64Field }

  TZInt64Field = class(TZNumericField)
  private
    FMinValue: Int64;
    FMaxValue: Int64;
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
    procedure SetMaxValue(Value: Int64);
    procedure SetMinValue(Value: Int64);
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Int64 read GetAsLargeInt write SetAsLargeInt;
  published
    property MaxValue: Int64 read FMaxValue write SetMaxValue default 0;
    property MinValue: Int64 read FMinValue write SetMinValue default 0;
  end;

{ TZUInt64Field }

  TZUInt64Field = class(TZNumericField)
  private
    FMinValue: UInt64;
    FMaxValue: UInt64;
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
    procedure SetMaxValue(Value: UInt64);
    procedure SetMinValue(Value: UInt64);
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: UInt64 read GetAsUInt64 write SetAsUInt64;
  published
    property MaxValue: UInt64 read FMaxValue write SetMaxValue {$IF NOT(defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR))}default 0{$IFEND};
    property MinValue: UInt64 read FMinValue write SetMinValue {$IF NOT(defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR))}default 0{$IFEND};
  end;

(*{ TAutoIncField }

  TAutoIncField = class(TIntegerField)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TUnsignedAutoIncField }

  TUnsignedAutoIncField = class(TLongWordField)
  public
    constructor Create(AOwner: TComponent); override;
  end;
*)
{ TZFloatField }

  TZFloatField = class(TZNumericField)
  private
    FCurrency: Boolean;
    FPrecision: Integer;
    procedure SetCurrency(Value: Boolean);
  protected
    procedure SetPrecision(Value: Integer); virtual;
    procedure InternalGetText(Value: Extended; out Text: string; DisplayText: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property Precision: Integer read FPrecision write SetPrecision;
  end;

{ TZSingleField }

  TZSingleField = class(TZFloatField)
  private
    FMinValue: Single;
    FMaxValue: Single;
    procedure SetMaxValue(Value: Single);
    procedure SetMinValue(Value: Single);
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
  protected
    procedure SetPrecision(Value: Integer); override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Single read GetAsSingle write SetAsSingle;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property MaxValue: Single read FMaxValue write SetMaxValue;
    property MinValue: Single read FMinValue write SetMinValue;
    property Precision default 7;
  end;

{ TZDoubleField }

  TZDoubleField = class(TZFloatField)
  private
    FMinValue: Double;
    FMaxValue: Double;
    procedure SetMaxValue(Value: Double);
    procedure SetMinValue(Value: Double);
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
  protected
    procedure SetPrecision(Value: Integer); override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Double read GetAsFloat write SetAsFloat;
  published
    property MaxValue: Double read FMaxValue write SetMaxValue;
    property MinValue: Double read FMinValue write SetMinValue;
    property Precision default 15;
  end;

{ TZCurrencyField }

  TZCurrencyField = class(TZFloatField)
  private
    FMinValue: Currency;
    FMaxValue: Currency;
    procedure SetMaxValue(Value: Currency);
    procedure SetMinValue(Value: Currency);
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
  protected
    procedure SetPrecision(Value: Integer); override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Currency read GetAsCurrency write SetAsCurrency;
  published
    property MaxValue: Currency read FMaxValue write SetMaxValue;
    property MinValue: Currency read FMinValue write SetMinValue;
    property Precision default 15;
  end;

{ TZExtendedField }

  TZExtendedField = class(TZFloatField)
  private
    FMinValue: Extended;
    FMaxValue: Extended;
    procedure SetMaxValue(Value: Extended);
    procedure SetMinValue(Value: Extended);
    procedure CheckRange(const Value; const ValueType: TZSQLType); override;
  protected
    procedure SetPrecision(Value: Integer); override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Extended read GetAsExtended write SetAsExtended;
  published
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property MinValue: Extended read FMinValue write SetMinValue;
    property Precision default 15;
  end;

(*{ TBooleanField }

  TBooleanField = class(TField)
  private
    FDisplayValues: string;
    FTextValues: array[Boolean] of string;
    procedure LoadTextValues;
    procedure SetDisplayValues(const Value: string);
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure SetAsBoolean(Value: Boolean); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Boolean read GetAsBoolean write SetAsBoolean;
  published
    property DisplayValues: string read FDisplayValues write SetDisplayValues;
  end;

{ TDateTimeField }

  TDateTimeField = class(TField)
  private
    FDisplayFormat: string;
    function GetValue(var Value: TDateTime): Boolean; inline;
    procedure SetDisplayFormat(const Value: string);
  protected
    procedure CopyData(Source, Dest: Pointer); override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: TDateTime read GetAsDateTime write SetAsDateTime;
  published
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property EditMask;
  end;

{ TSQLTimeStampField }

  TSQLTimeStampField = class(TField)
  private
    FDisplayFormat: string;
    function GetValue(var Value: TSQLTimeStamp): Boolean; inline;
    procedure SetDisplayFormat(const Value: string);
  protected
    procedure CopyData(Source, Dest: Pointer); override;
    function GetAsSQLTimeStamp: TSQLTimeStamp; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsSQLTimeStamp(const Value: TSQLTimeStamp); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: TSQLTimeStamp read GetAsSQLTimeStamp write SetAsSQLTimeStamp;
  published
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property EditMask;
  end;

  TSQLTimeStampOffsetField = class(TSQLTimeStampField)
  private
    function GetValue(var Value: TSQLTimeStampOffset): Boolean;
  protected
    procedure CopyData(Source, Dest: Pointer); override;
    function GetAsDateTime: TDateTime; override;
    function GetAsVariant: Variant; override;
    function GetAsSQLTimeStampOffset: TSQLTimeStampOffset; override;
    function GetDataSize: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: TSQLTimeStampOffset read GetAsSQLTimeStampOffset write SetAsSQLTimeStampOffset;
  end;

{ TDateField }

  TDateField = class(TDateTimeField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TTimeField }

  TTimeField = class(TDateTimeField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TBinaryField }

  TBinaryField = class(TField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    procedure CopyData(Source, Dest: Pointer); override;
    function GetAsString: string; override;
    function GetAsAnsiString: AnsiString; override;
    function GetAsBytes: TBytes; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetAsVariant: Variant; override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsAnsiString(const Value: AnsiString); override;
    procedure SetAsBytes(const Value: TBytes); override;
    procedure SetText(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Size default 16;
  end;

{ TBytesField }

  TBytesField = class(TBinaryField)
  protected
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TVarBytesField }

  TVarBytesField = class(TBytesField)
  protected
    function GetDataSize: Integer; override;
    procedure SetAsByteArray(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TBCDField }

  {PBcd struct moved to FmtBcd.pas}

  TBCDField = class(TNumericField)
  private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FMinValue: Currency;
    FMaxValue: Currency;
    FPrecision: Integer;
    procedure SetCurrency(Value: Boolean);
    procedure SetMaxValue(Value: Currency);
    procedure SetMinValue(Value: Currency);
    procedure SetPrecision(Value: Integer);
    procedure UpdateCheckRange;
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    procedure CopyData(Source, Dest: Pointer); override;
    function GetAsBCD: TBcd; override;
    function GetAsCurrency: Currency; override;
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetValue(var Value: Currency): Boolean; inline;
    procedure SetAsBCD(const Value: TBcd); override;
    procedure SetAsCurrency(Value: Currency); override;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Longint); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Currency read GetAsCurrency write SetAsCurrency;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property MaxValue: Currency read FMaxValue write SetMaxValue;
    property MinValue: Currency read FMinValue write SetMinValue;
    property Precision: Integer read FPrecision write SetPrecision default 0;
    property Size default 4;
  end;

{ TFMTBCDField }

  TFMTBCDField = class(TNumericField)
  private
    FCurrency: Boolean;
    FCheckRange: Boolean;
    FMinValue: string;
    FMaxValue: string;
    FPrecision: Integer;
    procedure BcdRangeError(Value: Variant; Max, Min: string);
    procedure SetCurrency(Value: Boolean);
    procedure SetMaxValue(Value: string);
    procedure SetMinValue(Value: string);
    procedure SetPrecision(Value: Integer);
    procedure UpdateCheckRange;
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    procedure CopyData(Source, Dest: Pointer); override;
    function GetAsCurrency: Currency; override;
    function GetAsBCD: TBcd; override;
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    function GetDefaultWidth: Integer; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    function GetValue(var Value: TBcd): Boolean; inline;
    procedure SetAsCurrency(Value: Currency); override;
    procedure SetAsBCD(const Value: TBcd); override;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Longint); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: TBcd read GetAsBCD write SetAsBCD;
  published
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property MaxValue: string read FMaxValue write SetMaxValue;
    property MinValue: string read FMinValue write SetMinValue;
    property Precision: Integer read FPrecision write SetPrecision default 0;
    property Size default 8;
  end;



{ TBlobField }

  TBlobType = ftBlob..ftWideMemo;

  TBlobField = class(TField)
  private
    FModifiedRecord: Integer;
    FModified: Boolean;
    FGraphicHeader: Boolean;
    FTransliterate: Boolean;
    function GetBlobType: TBlobType;
    function GetModified: Boolean;
    procedure LoadFromBlob(Blob: TBlobField);
    procedure LoadFromStrings(Strings: TWideStrings); overload;
    procedure LoadFromStrings(Strings: TStrings); overload;
    procedure LoadFromStreamPersist(StreamPersist: IStreamPersist);
    procedure SaveToStrings(Strings: TWideStrings); overload;
    procedure SaveToStrings(Strings: TStrings); overload;
    procedure SaveToStreamPersist(StreamPersist: IStreamPersist);
    procedure SetBlobType(Value: TBlobType);
    procedure SetModified(Value: Boolean);
    function SupportsStreamPersist(const Persistent: TPersistent;
      var StreamPersist: IStreamPersist): Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure FreeBuffers; override;
    function GetAsString: string; override;
    function GetAsWideString: UnicodeString; override;
    function GetAsAnsiString: AnsiString; override;
    function GetAsVariant: Variant; override;
    function GetAsBytes: TBytes; override;
    function GetBlobSize: Integer; virtual;
    function GetClassDesc: string; override;
    function GetIsNull: Boolean; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure SetAsAnsiString(const Value: AnsiString); override;
    procedure SetAsBytes(const Value: TBytes); override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsWideString(const Value: UnicodeString); override;
    procedure SetData(Buffer: Pointer; Len: Integer); overload;
    procedure SetVarValue(const Value: Variant); override;
    function GetDataSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    class function IsBlob: Boolean; override;
    procedure LoadFromFile(const FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SetFieldType(Value: TFieldType); override;
    property BlobSize: Integer read GetBlobSize;
    property Modified: Boolean read GetModified write SetModified;
    property Value: TBytes read GetAsBytes write SetAsBytes;
    property Transliterate: Boolean read FTransliterate write FTransliterate;
  published
    property BlobType: TBlobType read GetBlobType write SetBlobType default ftBlob;
    property GraphicHeader: Boolean read FGraphicHeader write FGraphicHeader default True;
    property Size default 0;
  end;

{ TMemoField }

  TMemoField = class(TBlobField)
  public
    constructor Create(AOwner: TComponent); override;
    function GetAsString: string; override;
    function GetAsWideString: UnicodeString; override;
    function GetAsVariant: Variant; override;
    procedure SetAsString(const Value: string); override;
    procedure SetAsWideString(const Value: UnicodeString); override;
    property Value: AnsiString read GetAsAnsiString write SetAsAnsiString;
  published
    property Transliterate default True;
  end;

{ TWideMemoField }

  TWideMemoField = class(TBlobField)
  protected
    function GetAsAnsiString: AnsiString; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure SetAsAnsiString(const Value: AnsiString); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: UnicodeString read GetAsWideString write SetAsWideString;
  end;

{ TGraphicField }

  TGraphicField = class(TBlobField)
  public
    constructor Create(AOwner: TComponent); override;
  end;   *)

{ TObjectField }

  {$IFNDEF WITH_TOBJECTFIELD}
  TObjectField = class(TZField)
  private
    FFields: TFields;
    FOwnedFields: TFields;
    FObjectType: string;
    FUnNamed: Boolean;
    procedure DataSetChanged;
    procedure ReadUnNamed(Reader: TReader);
    procedure WriteUnNamed(Writer: TWriter);
  protected
    class procedure CheckTypeSize({%H-}Value: Integer); override;
    {$IFNDEF WITH_VIRTUAL_TFIELD_BIND}
    procedure Bind(Binding: Boolean); virtual;
    {$ENDIF}
    procedure DefineProperties(Filer: TFiler); override;
    procedure FreeBuffers; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDefaultWidth: Integer; override;
    function GetFieldCount: Integer;
    function GetFields: TFields; virtual;
    function GetFieldValue(Index: Integer): Variant; virtual;
    function GetHasConstraints: Boolean; override;
    procedure SetChildOrder(Component: TComponent; Order: Integer); override;
    procedure SetDataSet(ADataSet: TDataSet); override;
    procedure SetFieldKind(Value: TFieldKind); {$IFDEF WITH_VIRTUAL_SETFIELDKIND}override{$ELSE}virtual{$ENDIF};
    procedure SetFieldValue(Index: Integer; const Value: Variant); virtual;
    procedure SetParentField(AField: TObjectField); override;
    procedure SetUnNamed(Value: Boolean); inline;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property FieldCount: Integer read GetFieldCount;
    property Fields: TFields read GetFields;
    property FieldValues[Index: Integer]: Variant read GetFieldValue
      write SetFieldValue; default;
    property UnNamed: Boolean read FUnNamed default False;
  published
    property ObjectType: string read FObjectType write FObjectType;
  end;
  {$ENDIF !WITH_TOBJECTFIELD}
(*
{ TADTField }

  TADTField = class(TObjectField)
  private
    procedure FieldsChanged(Sender: TObject);
  protected
    function GetSize: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;*)
{$IFNDEF WITH_TARRAYFIELD}
{ TArrayField }

  TArrayField = class(TObjectField)
  protected
    procedure Bind(Binding: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Size default 10;
  end;
{$ENDIF !WITH_TARRAYFIELD}

{ TDataSetField }
{$IFNDEF WITH_TDATASETFIELD}
  TDataSetField = class(TObjectField)
  private
    FOwnedDataSet: TDataSet;
    FNestedDataSet: TDataSet;
    FIncludeObjectField: Boolean;
    function GetNestedDataSet: TDataSet;
    procedure AssignNestedDataSet(Value: TDataSet);
    procedure SetIncludeObjectField(Value: Boolean);
  protected
    procedure Bind(Binding: Boolean); override;
    function GetCanModify: Boolean; override;
    function GetFields: TFields; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property NestedDataSet: TDataSet read GetNestedDataSet;
  published
    property IncludeObjectField: Boolean read FIncludeObjectField write SetIncludeObjectField default False;
  end;
{$ENDIF}
(*
{ TReferenceField }

  TReferenceField = class(TDataSetField)
  private
    FReferenceTableName: string;
  protected
    function GetAsVariant: Variant; override;
    function GetDataSize: Integer; override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ReferenceTableName: string read FReferenceTableName write FReferenceTableName;
    property Size default 0;
  end;

{ TVariantField }

  TVariantField = class(TField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsBCD: TBcd; override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsSQLTimeStamp: TSqlTimeStamp; override;
    function GetAsSingle: Single; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsLargeInt: Largeint; override;
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    function GetDefaultWidth: Integer; override;
    procedure SetAsBCD(const Value: TBcd); override;
    procedure SetAsBoolean(Value: Boolean); override;
    procedure SetAsSQLTimeStamp(const Value: TSqlTimeStamp); override;
    procedure SetAsDateTime(Value: TDateTime); override;
    procedure SetAsSingle(Value: Single); override;
    procedure SetAsFloat(Value: Double); override;
    procedure SetAsInteger(Value: Longint); override;
    procedure SetAsLargeInt(Value: Largeint); override;
    procedure SetAsString(const Value: string); override;
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TInterfaceField }

  TInterfaceField = class(TField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetValue: IUnknown;
    function GetAsVariant: Variant; override;
    procedure SetValue(const Value: IUnknown);
    procedure SetVarValue(const Value: Variant); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: IUnknown read GetValue write SetValue;
  end;

{ TIDispatchField }

  TIDispatchField = class(TInterfaceField)
  protected
    function GetValue: IDispatch;
    procedure SetValue(const Value: IDispatch);
  public
    constructor Create(AOwner: TComponent); override;
    property Value: IDispatch read GetValue write SetValue;
  end;

{ TGuidField }

  TGuidField = class(TStringField)
  protected
    class procedure CheckTypeSize(Value: Integer); override;
    function GetAsGuid: TGUID;
    function GetDefaultWidth: Integer; override;
    procedure SetAsGuid(const Value: TGUID);
  public
    constructor Create(AOwner: TComponent); override;
    property AsGuid: TGUID read GetAsGuid write SetAsGuid;
  end;

{ TAggregateField }

  TAggregateField = class(TField)
  private
    FActive: Boolean;
    FCurrency: Boolean;
    FDisplayName: string;
    FDisplayFormat: string;
    FExpression: string;
    FGroupingLevel: Integer;
    FIndexName: string;
    FHandle: Pointer;
    FPrecision: Integer;
    FResultType: TFieldType;
    procedure SetHandle(Value: Pointer); virtual;
    procedure SetActive(Value: Boolean);
    function GetHandle: Pointer; virtual;
    procedure SetGroupingLevel(Value: Integer);
    procedure SetIndexName(Value: string);
    procedure SetExpression(Value: string);
    procedure SetPrecision(Value: Integer);
    procedure SetCurrency(Value: Boolean);
  protected
    function GetAsString: string; override;
    function GetAsVariant: Variant; override;
    procedure GetText(var Text: string; DisplayText: Boolean); override;
    procedure Reset;
    procedure SetDisplayFormat(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    property Handle: Pointer read GetHandle write SetHandle;
    property ResultType: TFieldType read FResultType write FResultType;
  published
    property Active: Boolean read FActive write SetActive default False;
    { Lowercase to avoid name clash with C++ Currency type }
    property currency: Boolean read FCurrency write SetCurrency default False;
    property DisplayName: string read FDisplayName write FDisplayName;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property Expression: string read FExpression write SetExpression;
    property FieldKind default fkAggregate;
    property GroupingLevel: Integer read FGroupingLevel write SetGroupingLevel default 0;
    property IndexName: string read FIndexName write SetIndexName;
    property Precision: Integer read FPrecision write SetPrecision default 15;
    property Visible default False;
  end;  *)

  TZFieldDef = Class(TFieldDef)
  private
    FSQLType: TZSQLType;
    {$IFNDEF TFIELDDEF_HAS_CHILDEFS}
    FChildDefs: TFieldDefs;
    function GetChildDefs: TFieldDefs;
    procedure SetChildDefs(Value: TFieldDefs);
    {$ENDIF}
    function CreateFieldComponent(Owner: TComponent;
      ParentField: TObjectField = nil; FieldName: string = ''): TField;
    {$IFNDEF TFIELDDEF_HAS_CHILDEFS}
    function GetChildDefsClass: TFieldDefsClass; virtual;
    {$ENDIF}
  public
    constructor Create(Owner: TFieldDefs; const Name: string; FieldType: TFieldType;
      SQLType: TZSQLType; Size: Integer; Required: Boolean; FieldNo: Integer
      {$IFDEF WITH_CODEPAGE_AWARE_FIELD}; ACodePage: TSystemCodePage = CP_ACP{$ENDIF}); reintroduce;
    {$IFNDEF TFIELDDEF_HAS_CHILDEFS}
    destructor Destroy; override;
    function HasChildDefs: Boolean;
    {$ENDIF}
    function CreateField(Owner: TComponent; ParentField: TObjectField = nil;
      const FieldName: string = ''; CreateChildren: Boolean = True): TField;
  {$IFNDEF TFIELDDEF_HAS_CHILDEFS}
  published
    property ChildDefs: TFieldDefs read GetChildDefs write SetChildDefs stored HasChildDefs;
  {$ENDIF}
  End;

const
  ZSQLFieldClasses: array[TZSQLType] of TFieldClass = (
    {$IFDEF FPC}
    TField,
    {$ELSE}
    nil,                       { stUnknown }
    {$ENDIF}
    TBooleanField,             { stBoolean }
    TZByteField,               { stByte }
    TZShortIntField,           { stShort }
    TZWordField,               { stWord }
    TZSmallIntField,           { stSmall }
    TZLongWordField,           { stLongWord }
    TZIntegerField,            { stInteger }
    TZUInt64Field,          { stULong }
    TZInt64Field,           { stLong }
    TZSingleField,               { stFloat }
    TZDoubleField,             { stDouble }
    TZCurrencyField,           { stCurrency }
    TZExtendedField,           { stBigDecimal }
    TZStringField,             { stString }
    TZWideStringField,         { stUnicodeString }
    TBytesField,               { stBytes }
    TStringField,              { stGUID }
    TDateField,                { stDate }
    TTimeField,                { stTime }
    TDateTimeField,            { stTimestamp }
    TArrayField,               { stArray }
    TDataSetField,             { stDataSet }
    TMemoField,                { stAsciiStream }
    {$IFDEF WITH_WIDEMEMO}TWideMemoField{$ELSE}TMemoField{$ENDIF},            { stUnicodeStream }
    TBlobField                 { stBinaryStream }
    );
  {$IFNDEF WITH_OBJECTFIELDTYPES}
  ObjectFieldTypes = [ftADT, ftArray, ftReference, ftDataSet];
  {$ENDIF}

{$IFNDEF WITH_FIELDDEFLIST}
var
  DefaultFieldDefsClass        : TFieldDefsClass        = TFieldDefs;
  DefaultFieldDefListClass     : TFieldDefListClass     = TZFieldDefList;
  DefaultFieldDefClass         : TFieldDefClass         = TZFieldDef;
{$ENDIF}

implementation

uses ZFastCode, Math, ZVariant, ZMessages, ZDatasetUtils, ZStreamBlob, ZSelectSchema,
  ZGenericSqlToken, ZTokenizer, ZGenericSqlAnalyser, ZEncoding
  {$IFNDEF HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES}, ZAbstractDataset{$ENDIF} //see comment of Updatable property
  {$IFDEF WITH_DBCONSTS}, DBConsts {$ELSE}, DBConst{$ENDIF}
  {$IFDEF WITH_UNITANSISTRINGS}, AnsiStrings{$ENDIF}
  {$IFNDEf WITH_FIELDDEFLIST}, RTLConsts{$ENDIF};

{ EZDatabaseError }

{**
  Constructs a database exception with a string message.
  @param Msg a string message which describes the error.
}
constructor EZDatabaseError.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

{**
  Constructs a database exception from TZSQLThrowable instance.
  @param E an original TZSQLThrowable instance.
}
constructor EZDatabaseError.CreateFromException(E: EZSQLThrowable);
begin
  inherited Create(E.Message);
  ErrorCode := E.ErrorCode;
  Statuscode:= E.StatusCode;
end;

procedure EZDatabaseError.SetStatusCode(const Value: String);
begin
  FStatusCode := value;
end;

{ TZDataLink }

{**
  Creates this dataset link object.
  @param ADataset an owner linked dataset component.
}
constructor TZDataLink.Create(ADataset: TZAbstractRODataset);
begin
  inherited Create(ADataset);
  FDataset := ADataset;
end;

{**
  Processes changes in state of linked dataset.
}
procedure TZDataLink.ActiveChanged;
begin
  if FDataset.Active and not (csDestroying in FDataset.Componentstate) then
    FDataset.RefreshParams;
end;

{**
  Processes changes in fields of the linked dataset.
  @param Field a field which was changed.
}
procedure TZDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) and FDataset.Active then
    FDataset.RefreshParams;
end;

{$IFNDEf WITH_FIELDDEFLIST}
{ TFlatList }

constructor TFlatList.Create(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
  inherited Create;
  OnChanging := ListChanging;
  FLocked := True;
end;

function TFlatList.FindItem(const Name: string; MustExist: Boolean): TObject;
var
  I: Integer;
begin
  if not Updated then Update;
  I := IndexOf(Name);
  if I > -1 then
    Result := GetObject(I)
  else
  begin
    if MustExist then
      DatabaseErrorFmt(SFieldNotFound, [Name], DataSet);
    Result := nil;
  end;
end;

function TFlatList.GetCount: Integer;
begin
  if not Updated then Update;
  Result := inherited GetCount;
end;

function TFlatList.GetUpdated: Boolean;
begin
  Result := FUpdated;
end;

{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // parameter not used intentionally
procedure TFlatList.ListChanging(Sender: TObject);
begin
  if Locked then
    DatabaseError(SReadOnlyProperty, DataSet);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

procedure TFlatList.Update;
begin
  if not Updated then
  begin
    Locked := False;
    BeginUpdate;
    try
      Clear;
      UpdateList;
      FUpdated := True;
    finally
      EndUpdate;
      Locked := True;
    end;
  end;
end;

{ TZFieldDefList }

function TZFieldDefList.GetFieldDef(Index: Integer): TZFieldDef;
begin
  if not Updated then Update;
  Result := TZFieldDef(Objects[Index]);
end;

function TZFieldDefList.Find(const Name: string): TZFieldDef;
begin
  Result := TZFieldDef(FindItem(Name, False));
end;

function TZFieldDefList.FieldByName(const Name: string): TZFieldDef;
begin
  Result := TZFieldDef(FindItem(Name, True));
end;

procedure TZFieldDefList.UpdateList;

  procedure AddFieldDefs(const ParentName: string; const FieldDefs: TFieldDefs);
  var
    ChildCount, J, I: Integer;
    ChildDef, FieldDef: TZFieldDef;
    FieldName, ItemName: string;
  begin
    for I := 0 to FieldDefs.Count - 1 do
    begin
      FieldDef := FieldDefs[I] as TZFieldDef;
      FieldName := ParentName+FieldDef.Name;
      AddObject(FieldName, FieldDef);
      if FieldDef.HasChildDefs then
        if FieldDef.DataType = ftArray then
        begin
          ChildDef := FieldDef.ChildDefs[0] as TZFieldDef;
          ChildCount := FieldDef.Size;
          for J := 0 to ChildCount - 1 do
          begin
            ItemName := Format('%s[%d]', [FieldName, J]);
            AddObject(ItemName, ChildDef);
            if ChildDef.DataType = ftADT then
              AddFieldDefs(ItemName+'.', ChildDef.ChildDefs);
          end;
        end
        else if faUnNamed in FieldDef.Attributes then
          AddFieldDefs('',FieldDef.ChildDefs)
        else
          AddFieldDefs(ParentName+FieldDef.Name+'.', FieldDef.ChildDefs);
    end;
  end;

begin
  if DataSet.Active then DataSet.FieldDefs.Update;
  AddFieldDefs('', TFieldDefs(DataSet.FieldDefs));
end;

function TZFieldDefList.GetUpdated: Boolean;
begin
  Result := FUpdated and DataSet.FieldDefs.Updated;
end;

{$ENDIF WITH_FIELDDEFLIST}
{ TZAbstractRODataset }

{**
  Constructs this object and assignes the mail properties.
  @param AOwner a component owner.
}
constructor TZAbstractRODataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSQL := TZSQLStrings.Create;
  TZSQLStrings(FSQL).Dataset := Self;
  TZSQLStrings(FSQL).MultiStatements := False;
  FSQL.OnChange := UpdateSQLStrings;
  FParams := TParams.Create(Self);
  FCurrentRows := TZSortedList.Create;
  BookmarkSize := SizeOf(Integer);
  FShowRecordTypes := [usModified, usInserted, usUnmodified];
  FRequestLive := False;
  FFetchRow := 0;                // added by Patyi
  FOptions := [doCalcDefaults];

  FFilterEnabled := False;
  FProperties := TStringList.Create;
  FFilterExpression := TZExpression.Create;
  FFilterExpression.Tokenizer := CommonTokenizer;
  FFilterStack := TZExecutionStack.Create;

  FDataLink := TZDataLink.Create(Self);
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
  {$IFDEF WITH_GENERIC_TLISTTFIELD}
  FIndexFields := TList<TField>.Create;
  {$ELSE}
  FIndexFields := TList.Create;
  {$ENDIF}
  {$IFNDEF WITH_NESTEDDATASETS}
  FNestedDataSets := TList.Create;
  {$ENDIF}
  {$IF defined(ZEOS_TEST_ONLY) and defined(TEST_ZFIELDS)}
  FUseZFields := True;
  {$IFEND}
end;

{**
  Destroys this object and cleanups the memory.
}
destructor TZAbstractRODataset.Destroy;
begin
  Unprepare;
  if Assigned(Connection) then
  begin
    try
      SetConnection(nil);
    except
    end;
  end;

  FreeAndNil(FSQL);
  FreeAndNil(FParams);
  FreeAndNil(FCurrentRows);
  FreeAndNil(FProperties);
  FreeAndNil(FFilterStack);

  FreeAndNil(FDataLink);
  FreeAndNil(FMasterLink);
  FreeAndNil(FIndexFields);
  {$IFNDEF WITH_NESTEDDATASETS}
  FreeAndNil(FNestedDataSets);
  {$ENDIF}

  inherited Destroy;
end;

procedure TZAbstractRODataset.StringFieldSetterRawToUnicode(ColumnIndex: Integer;
  Buffer: PAnsiChar);
var
  len: NativeUInt;
  wBuf: array[0..dsMaxStringSize shr 1] of WideChar;
  wDynBuf: array of WideChar;
  Dest: PWideChar;
begin
  len := ZFastCode.StrLen(Buffer);
  if Len > dsMaxStringSize shr 1 then begin
    SetLength(wDynBuf, Len);
    Dest := @wDynBuf[0];
  end else
    Dest := @wBuf[0];
  Len := PRaw2PUnicodeBuf(Buffer, Dest, Len, RowAccessor.ConSettings^.CTRL_CP);
  RowAccessor.SetPWideChar(ColumnIndex, Dest, @Len);
end;


{$IFNDEF UNICODE}
procedure TZAbstractRODataset.StringFieldSetterFromRawAutoEncode(
  ColumnIndex: Integer; Buffer: PAnsiChar);
begin
  case ZDetectUTF8Encoding(Buffer, ZFastCode.StrLen(Buffer)) of
    etUSASCII: RowAccessor.SetRawByteString(ColumnIndex, Buffer);
    etAnsi: RowAccessor.SetAnsiString(ColumnIndex, Buffer);
    etUTF8: RowAccessor.SetUTF8String(ColumnIndex, Buffer);
  end;
end;

procedure TZAbstractRODataset.StringFieldSetterFromRaw(
  ColumnIndex: Integer; Buffer: PAnsiChar);
begin
  RowAccessor.SetRawByteString(ColumnIndex, Buffer);
end;

{$ELSE}
procedure TZAbstractRODataset.StringFieldSetterFromAnsi(
  ColumnIndex: Integer; Buffer: PAnsiChar);
begin
  {$IFNDEF NO_ANSISTRING}
  RowAccessor.SetAnsiString(ColumnIndex, Buffer);
  {$ELSE}
  RowAccessor.SetRawByteString(ColumnIndex, Buffer);
  {$ENDIF}
end;
{$ENDIF}

function TZAbstractRODataset.StringFieldGetterFromAnsiRec(
  ColumnIndex, FieldSize: Integer; Buffer: PAnsiChar): Boolean;
var
  P: PAnsiChar;
  L: NativeUInt;
begin
  P := RowAccessor.GetPAnsiChar(ColumnIndex, Result, L);
  if not Result then begin //instead of StrPLCopy
    L := {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(L, NativeUInt(Max(dsMaxStringSize, FieldSize-1))); //left for String truncation if option FUndefinedVarcharAsStringLength is <> 0
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Buffer^, L);
  end;
  PByte(Buffer+L)^ := Ord(#0);
end;

function TZAbstractRODataset.StringFieldGetterFromUnicode(ColumnIndex, FieldSize: Integer;
  Buffer: PAnsiChar): Boolean;
var
  P: PWideChar;
  L: NativeUInt;
begin
  P := RowAccessor.GetPWideChar(ColumnIndex, Result, L);
  if not Result then //instead of StrPLCopy
    L := PUnicode2PRawBuf(P, Buffer, L, Max(dsMaxStringSize, FieldSize-1), RowAccessor.ConSettings^.CTRL_CP);
  PByte(Buffer+L)^ := Ord(#0);
end;

function TZAbstractRODataset.StringFieldGetterRaw2RawConvert(ColumnIndex,
  FieldSize: Integer; Buffer: PAnsiChar): Boolean;
var
  P: PAnsiChar;
  L: NativeUInt;
begin
  P := RowAccessor.GetPAnsiChar(ColumnIndex, Result, L);
  if not Result then //instead of WStrLCopy
    L := PRawToPRawBuf(P, Buffer, L, Max(dsMaxStringSize, FieldSize-1),
      RowAccessor.ConSettings^.ClientCodePage^.CP, RowAccessor.ConSettings^.CTRL_CP);
  PByte(Buffer+L)^ := Ord(#0)
end;

function TZAbstractRODataset.WideStringGetterFromRaw(ColumnIndex, FieldSize: Integer;
  Buffer: PWideChar): Boolean;
var
  P: PAnsiChar;
  L: NativeUInt;
begin
  P := RowAccessor.GetPAnsiChar(ColumnIndex, Result, L);
  if Result then
    PWord(Buffer)^ := Ord(#0)
  else //instead of WStrLCopy
    PRaw2PUnicode(P, Buffer, LengthInt(L), LengthInt(Max(dsMaxStringSize, FieldSize-2)) shr 1, RowAccessor.ConSettings^.ClientCodePage^.CP);
end;

function TZAbstractRODataset.WideStringGetterFromUnicode(ColumnIndex, FieldSize: Integer;
  Buffer: PWideChar): Boolean;
var
  P: PWideChar;
  L: NativeUInt;
begin
  P := RowAccessor.GetPWideChar(ColumnIndex, Result, L);
  if not Result then begin //instead of WStrCopy
    L := {$IFDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}ZCompatibility.{$ENDIF}Min(L, NativeUInt(Max(dsMaxStringSize, FieldSize -2) shr 1)); //left for String truncation if option FUndefinedVarcharAsStringLength is <> 0
    {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Pointer(Buffer)^, L shl 1);
  end;
  PWord(Buffer+L)^ := Ord(#0);
end;

{**
  Sets database connection object.
  @param Value a database connection object.
}
procedure TZAbstractRODataset.SetConnection(Value: TZAbstractConnection);
begin
  if FConnection <> Value then begin
    if Active then
       Close;
    Unprepare;
    if FConnection <> nil then
      FConnection.UnregisterDataSet(Self);
    FConnection := Value;
    if FConnection <> nil then begin
      FConnection.RegisterDataSet(Self);
      if (FSQL.Count > 0) and PSIsSQLBased{do not rebuild all!} and (Fields.Count = 0{persistent fields?}) then begin
      {EH: force rebuild all of the SQLStrings ->
        in some case the generic tokenizer fails for several reasons like:
        keyword detection, identifier detection, Field::=x(ParamEsacaping to ":=" ) vs. Field::BIGINT (pg-TypeCasting)
        using persistent components where creation order means the datasets are
        created before the connection+protocol is available the generic
        tokenizer fails in all areas}
        FSQL.BeginUpdate;
        FSQL.EndUpdate;
      end;
    end;
  end;
end;

{**
  Gets the SQL query.
  @return the SQL query strings.
}

function TZAbstractRODataset.GetSQL: TStrings;
begin
  Result := FSQL;
end;

{$IFNDEF WITH_FUNIDIRECTIONAL}
function TZAbstractRODataset.SetUniDirectional(const Value: boolean);
begin
  FUniDirectional := Value;
end;
{$ENDIF}
{**
  Gets unidirectional state of dataset.
  @return the unidirectional flag (delphi).
}
function TZAbstractRODataset.GetUniDirectional: boolean;
begin
  Result := {$IFNDEF WITH_FUNIDIRECTIONAL}FUniDirectional{$ELSE}inherited IsUniDirectional{$ENDIF};
end;

{$IFNDEF WITH_SPARSEARRAYS}
procedure TZAbstractRODataset.SetSparseArrays(Value: Boolean);
begin
  CheckInactive;
  FSparseArrays := Value;
end;
{$ENDIF WITH_SPARSEARRAYS}

{$IFNDEF WITH_NESTEDDATASETS}
function TZAbstractRODataset.GetNestedDataSets: TList;
begin
  if FNestedDataSets = nil then
    FNestedDataSets := TList.Create;
  Result := FNestedDataSets;
end;
{$ENDIF}


procedure TZAbstractRODataset.SetStringFieldSetterAndSetter;
var ConSettings: PZConSettings;
begin
  ConSettings := Connection.DbcConnection.GetConSettings;
  if (ConSettings^.ClientCodePage^.Encoding = ceUTF16) or
     (not ConSettings^.ClientCodePage^.IsStringFieldCPConsistent) then begin
    FStringFieldGetter := StringFieldGetterFromUnicode;
    {$IFNDEF UNICODE}
    if ConSettings^.AutoEncode then
      FStringFieldSetter := StringFieldSetterFromRawAutoEncode
    else
    {$ENDIF}
      FStringFieldSetter := StringFieldSetterRawToUnicode;
    FWideStringFieldGetter := WideStringGetterFromUnicode;
  end else
    FWideStringFieldGetter := WideStringGetterFromRaw;
    {$IFNDEF UNICODE}
    //Hint: the UnicodeIDE's do return allways a AnsiString casted UnicodeString
    //So it's impossible to retrieve a UTF8 encoded string SAFELY
    //It might be possible a user did Assign such a casted value. But that's
    //not Unicode-Save since the AnsiString(AUnicodeString) cast.
    //Known issues: Simplified chinese or Persian f.e. have some equal UTF8
    //two/four byte sequense wich lead to data loss. So success is randomly!!
    if ConSettings^.AutoEncode then
    begin
      FStringFieldSetter := StringFieldSetterFromRawAutoEncode;
      if ConSettings.CPType = cCP_UTF8 then
        if (ConSettings^.ClientCodePage^.Encoding = ceUTF8) then
          FStringFieldGetter := StringFieldGetterFromAnsiRec
        else
          FStringFieldGetter := StringFieldGetterRaw2RawConvert
      else if (ConSettings^.ClientCodePage^.Encoding = ceAnsi) and
              ZCompatibleCodePages(ZOSCodePage, ConSettings^.ClientCodePage^.CP) then
        FStringFieldGetter := StringFieldGetterFromAnsiRec
      else
        FStringFieldGetter := StringFieldGetterRaw2RawConvert;
    end else begin
      FStringFieldGetter := StringFieldGetterFromAnsiRec;
      FStringFieldSetter := StringFieldSetterFromRaw;
    end;
    {$ELSE}
    if ZCompatibleCodePages(ZOSCodePage, ConSettings^.ClientCodePage^.CP) then
      FStringFieldGetter := StringFieldGetterFromAnsiRec
    else
      FStringFieldGetter := StringFieldGetterRaw2RawConvert;
    FStringFieldSetter := StringFieldSetterFromAnsi;
    {$ENDIF}
end;

{**
  Sets a new SQL query.
  @param Value a new SQL query.
}
procedure TZAbstractRODataset.SetSQL(Value: TStrings);
begin
  FSQL.Assign(Value);
end;

{**
  Gets a parameters check value.
  @return a parameters check value.
}
function TZAbstractRODataset.GetParamCheck: Boolean;
begin
  Result := FSQL.ParamCheck;
end;

{**
  Sets a new parameters check value.
  @param Value a parameters check value.
}
procedure TZAbstractRODataset.SetParamCheck(Value: Boolean);
begin
  if Value <> FSQL.ParamCheck then begin
    FSQL.ParamCheck := Value;
    UpdateSQLStrings(FSQL);
  end;
end;

{**
  Gets a parameters marker.
  @return a parameter marker.
}
function TZAbstractRODataset.GetParamChar: Char;
begin
  Result := FSQL.ParamChar;
end;

{**
  Sets a new parameter marker.
  @param Value a parameter marker.
}
procedure TZAbstractRODataset.SetParamChar(Value: Char);
begin
  if Value <> FSQL.ParamChar then begin
    FSQL.ParamChar := Value;
    UpdateSQLStrings(FSQL);
  end;
end;

{**
  Sets a new set of parameters.
  @param Value a set of parameters.
}
procedure TZAbstractRODataset.SetParams(Value: TParams);
begin
  FParams.AssignValues(Value);
end;

{**
  Defines a persistent dataset properties.
  @param Filer a persistent manager object.
}
procedure TZAbstractRODataset.DefineProperties(Filer: TFiler);

  function WriteData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not FParams.IsEqual(TZAbstractRODataset(Filer.Ancestor).FParams)
    else
      Result := FParams.Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('ParamData', ReadParamData, WriteParamData, WriteData);
end;

{**
  Reads parameter data from persistent storage.
  @param Reader an input data stream.
}
procedure TZAbstractRODataset.ReadParamData(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FParams);
end;

{**
  Writes parameter data from persistent storage.
  @param Writer an output data stream.
}
procedure TZAbstractRODataset.WriteParamData(Writer: TWriter);
begin
  Writer.WriteCollection(Params);
end;

{**
  Gets a SQL parameter by its name.
  @param Value a parameter name.
  @return a found parameter object.
}
function TZAbstractRODataset.ParamByName(const Value: string): TParam;
begin
  Result := FParams.ParamByName(Value);
end;

{**
  Updates parameters from SQL statement.
  @param Sender an event sender object.
}
procedure TZAbstractRODataset.UpdateSQLStrings(Sender: TObject);
var
  I: Integer;
  OldParams: TParams;
begin
  FieldDefs.Clear;
  if Active
  then Close
  else if assigned(Statement) then begin
    Statement.Close;
    Statement := nil;
  end;

  UnPrepare;
  if (csLoading in ComponentState) then
    Exit;
  OldParams := TParams.Create;
  OldParams.Assign(FParams);
  FParams.Clear;

  try
    for I := 0 to TZSQLStrings(Sender).ParamCount - 1 do
      FParams.CreateParam(ftUnknown, TZSQLStrings(Sender).ParamNames[I], ptUnknown);
    FParams.AssignValues(OldParams);
  finally
    OldParams.Free;
  end;
end;

{$IFDEF WITH_ZSTRINGFIELDS}
procedure TZAbstractRODataset.SetUseZFields(const Value: Boolean);
begin
  if Active then
    raise Exception.Create(Format(cSOperationIsNotAllowed3, ['active DataSet']));
  FUseZFields := Value;
end;
{$ENDIF}

{**
  Gets the ReadOnly property.
  @return <code>True</code> if the opened result set read only.
}
function TZAbstractRODataset.GetReadOnly: Boolean;
begin
  Result := not RequestLive;
end;

{**
  Sets a new ReadOnly property.
  @param Value <code>True</code> to set result set read-only.
}
procedure TZAbstractRODataset.SetReadOnly(Value: Boolean);
begin
  RequestLive := not Value;
end;

{**
  Gets a visible updated records types.
  @param return visible UpdateRecordTypes value.
}
function TZAbstractRODataset.GetShowRecordTypes: TUpdateStatusSet;
begin
  Result := FShowRecordTypes;
end;

{**
  Sets a new visible updated records types.
  @param Value a new visible UpdateRecordTypes value.
}
procedure TZAbstractRODataset.SetShowRecordTypes(Value: TUpdateStatusSet);
begin
  if Value <> FShowRecordTypes then
  begin
    FShowRecordTypes := Value;
    RereadRows;
  end;
end;

{**
  Checks if this dataset is opened.
}
procedure TZAbstractRODataset.CheckOpened;
begin
  if not Active then
    DatabaseError(SOperationIsNotAllowed4);
end;

{**
  Checks if the database connection is assigned
  and tries to connect.
}
procedure TZAbstractRODataset.CheckConnected;
begin
  if Connection = nil then
    raise EZDatabaseError.Create(SConnectionIsNotAssigned);
  Connection.Connect;
end;

{**
  Checks is the database has bidirectional access.
}
procedure TZAbstractRODataset.CheckBiDirectional;
begin
  if IsUniDirectional then
    raise EZDatabaseError.Create(SOperationIsNotAllowed1);
end;

{**
  Checks the correct SQL query.
}
procedure TZAbstractRODataset.CheckSQLQuery;
begin
  if FSQL.StatementCount < 1 then
    raise EZDatabaseError.Create(SQueryIsEmpty);
  if FSQL.StatementCount > 1 then
    raise EZDatabaseError.Create(SCanNotExecuteMoreQueries);
end;

{**
  Raises an error 'Operation is not allowed in read-only dataset.
}
procedure TZAbstractRODataset.RaiseReadOnlyError;
begin
  raise EZDatabaseError.Create(SOperationIsNotAllowed2);
end;

{**
  Fetches specified number of records.
  @param RowCount a specified number of rows to be fetched.
  @return <code>True</code> if all required rows were fetched.
}
function TZAbstractRODataset.FetchRows(RowCount: Integer): Boolean;
begin
  if (CurrentRows.Count < RowCount) or (RowCount = 0) then
    if FLastRowFetched
    then Result := CurrentRows.Count >= RowCount
    else begin
      Connection.ShowSQLHourGlass;
      try
        if (RowCount = 0) then begin
          while FetchOneRow do;
          Result := True;
        end else begin
          while (CurrentRows.Count < RowCount) do
            if not FetchOneRow then
              Break;
          Result := CurrentRows.Count >= RowCount;
        end;
      finally
        Connection.HideSQLHourGlass;
      end;
    end
  else Result := True;
end;

{**
  Fetches one row from the result set.
  @return <code>True</code> if record was successfully fetched.
}
function TZAbstractRODataset.FetchOneRow: Boolean;
begin
  if Assigned(ResultSet) then
    repeat
      if (FetchCount = 0) or (ResultSet.GetRow = FetchCount) or
          ResultSet.MoveAbsolute(FetchCount)
      then begin
        Result := ResultSet.Next;
        FLastRowFetched := not Result;
      end else Result := False;
      if Result then begin
        Inc(FFetchCount);
        if FilterRow(ResultSet.GetRow) then
          CurrentRows.Add({%H-}Pointer(ResultSet.GetRow))
        else
          Continue;
      end;
    until True
  else
    Result := False;
end;

{**
  Checks the specified row with the all filters.
  @param RowNo a number of the row.
  @return <code>True</code> if the row sutisfy to all filters.
}
function TZAbstractRODataset.FilterRow(RowNo: NativeInt): Boolean;
var
  I: Integer;
  SavedRow: Integer;
  SavedRows: TZSortedList;
  SavedState: TDatasetState;
begin
  Result := True;

  { Locates the result set to the specified row. }
  if ResultSet.GetRow <> RowNo then
  begin
    if not ResultSet.MoveAbsolute(RowNo) then
      Result := False;
  end;
  if not Result then
     Exit;

  { Checks record by ShowRecordType }
  if ResultSet.RowUpdated then
    Result := usModified in ShowRecordTypes
  else if ResultSet.RowInserted then
    Result := usInserted in ShowRecordTypes
  else if ResultSet.RowDeleted then
    Result := usDeleted in ShowRecordTypes
  else
    Result := usUnmodified in ShowRecordTypes;
  if not Result then
     Exit;

  { Check master-detail links }
  if MasterLink.Active then
  begin
    for I := 0 to MasterLink.Fields.Count - 1 do
    begin
      if I < IndexFields.Count then
        Result := CompareKeyFields(TField(IndexFields[I]), ResultSet,
          TField(MasterLink.Fields[I]));

      if not Result then
        Break;
    end;
  end;
  if not Result then
     Exit;

  { Checks record by OnFilterRecord event }
  if FilterEnabled and Assigned(OnFilterRecord) then
  begin
    SavedRow := CurrentRow;
    SavedRows := CurrentRows;
    CurrentRows := TZSortedList.Create;

    SavedState := SetTempState(dsNewValue);
    CurrentRows.Add({%H-}Pointer(RowNo));
    CurrentRow := 1;

    try
      OnFilterRecord(Self, Result);
    except
      if Assigned(ApplicationHandleException)
      then ApplicationHandleException(Self);
    end;

    CurrentRow := SavedRow;
    {$IFDEF AUTOREFCOUNT}
    CurrentRows := nil;
    {$ELSE}
    CurrentRows.Free;
    {$ENDIF}
    CurrentRows := SavedRows;
    RestoreState(SavedState);

  end;
  if not Result then
     Exit;

  { Check the record by filter expression. }
  if FilterEnabled and (FilterExpression.Expression <> '') then begin
    if not InitFilterFields then begin
      FilterFieldRefs := DefineFilterFields(Self, FilterExpression);
      InitFilterFields := True;
    end;
    CopyDataFieldsToVars(FilterFieldRefs, ResultSet,
      FilterExpression.DefaultVariables);
    Result := FilterExpression.VariantManager.GetAsBoolean(
      FilterExpression.Evaluate4(FilterExpression.DefaultVariables,
      FilterExpression.DefaultFunctions, FilterStack));
  end;
  if not Result then
     Exit;
end;

{**
  Go to specified row.
  @param RowNo a number of the row.
  @return <code>True</code> if the row successfully located.
}
function TZAbstractRODataset.GotoRow(RowNo: NativeInt): Boolean;
var
  Index: Integer;
begin
  Result := False;
  Index := CurrentRows.IndexOf({%H-}Pointer(RowNo));
  if Index >= 0 then
  begin
    if Index < CurrentRow then
      CheckBiDirectional;
    CurrentRow := Index + 1;
    Result := True;
  end;
end;

{**
  Rereads all rows and applies a filter.
}
procedure TZAbstractRODataset.RereadRows;
var
  I: NativeUInt;
  RowNo: NativeInt;
begin
  if not (State in [dsInactive]) and not IsUniDirectional then
  begin
    UpdateCursorPos; //see http://sourceforge.net/p/zeoslib/tickets/89/
    if (CurrentRow > 0) and (CurrentRow <= CurrentRows.Count) and
       (CurrentRows.Count > 0) then
      RowNo := {%H-}NativeInt(CurrentRows[CurrentRow - 1])
    else
      RowNo := -1;
    CurrentRows.Clear;

    for I := 1 to FetchCount do
      if FilterRow(I) then
        CurrentRows.Add({%H-}Pointer(I));

    CurrentRow := CurrentRows.IndexOf({%H-}Pointer(RowNo)) + 1;
    CurrentRow := Min(Max(1, CurrentRow), CurrentRows.Count);

    if FSortedFields <> '' then
      InternalSort
    else
      Resync([]);
  end;
end;

{**
  Fill prepared statement with parameters.
  @param Statement a prepared SQL statement.
  @param ParamNames an array of parameter names.
  @param Params a collection of SQL parameters.
  @param DataLink a datalink to get parameters.
}
procedure TZAbstractRODataset.SetStatementParams(Statement: IZPreparedStatement;
  ParamNames: TStringDynArray; Params: TParams; DataLink: TDataLink);
var
  I: Integer;
  TempParam, Param: TParam;
  Dataset: TDataset;
  Field: TField;
begin
  if DataLink.Active then
    Dataset := DataLink.DataSet
  else
    Dataset := nil;

  if (not ParamCheck) and (not Assigned(ParamNames)) and (FParams.Count > 0) then begin
    for I := 0 to Params.Count -1 do begin
      Param := Params[i];
      if not Assigned(Param) or (Param.ParamType in [ptOutput, ptResult]) then
        Continue;
      SetStatementParam(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Statement, Param);
    end;
  end else begin
    TempParam := TParam.Create(nil);
    try
      for I := Low(ParamNames) to High(ParamNames) do
      begin
        if Assigned(Dataset) then
          Field := Dataset.FindField(ParamNames[I])
        else
          Field := nil;

        if Assigned(Field) then
        begin
          TempParam.AssignField(Field);
          Param := TempParam;
        end
        else
        begin
          Param := Params.FindParam(ParamNames[I]);
          if not Assigned(Param) or (Param.ParamType in [ptOutput, ptResult]) then
            Continue;
        end;

        SetStatementParam(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Statement, Param);
      end;
    finally
      TempParam.Free;
    end;
  end;
end;

{**
  Locates a specified record in dataset.
  @param Buffer a record buffer to put the contents of the row.
  @param GetMode a location mode.
  @param DoCheck flag to perform checking.
  @return a location result.
}
function TZAbstractRODataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  RowNo: NativeInt;
begin
  // mad stub for unidirectional (problem in TDataSet.MoveBuffer) - dont know about FPC
  // we always use same TDataSet-level buffer, because we can see only one row
  {$IFNDEF WITH_UNIDIRECTIONALBUG}
  if IsUniDirectional then
    Buffer := TRecordBuffer(Buffers[0]);
  {$ENDIF}

  Result := grOK;
  case GetMode of
    gmNext:
      begin
        if FetchRows(CurrentRow + 1) then
          CurrentRow := CurrentRow + 1
        else
          Result := grEOF;
      end;
    gmPrior:
      begin
        CheckBiDirectional;
        if (CurrentRow > 1) and (CurrentRows.Count > 0) then
          CurrentRow := CurrentRow - 1
        else
          Result := grBOF;
      end;
    gmCurrent:
      begin
        if CurrentRow < CurrentRows.Count then
          CheckBiDirectional;

        if CurrentRow = 0 then
        begin
          if CurrentRows.Count = 0 then
            FetchRows(1);
          CurrentRow := Min(CurrentRows.Count, 1);
        end
        else if not FetchRows(CurrentRow) then
          CurrentRow := Max(1, Min(CurrentRows.Count, CurrentRow));

        if CurrentRows.Count = 0 then
          Result := grError;
      end;
  end;

  if Result = grOK then
  begin
    RowNo := {%H-}NativeInt(CurrentRows[CurrentRow - 1]);
    if ResultSet.GetRow <> RowNo then
      ResultSet.MoveAbsolute(RowNo);
    RowAccessor.RowBuffer := PZRowBuffer(Buffer);
    RowAccessor.RowBuffer^.Index := RowNo;
    FetchFromResultSet(ResultSet, FieldsLookupTable, Fields, RowAccessor);
    FRowAccessor.RowBuffer^.BookmarkFlag := Ord(bfCurrent);
    GetCalcFields(TGetCalcFieldsParamType(Buffer));
  end;

  if (Result = grError) and DoCheck then
    raise EZDatabaseError.Create(SNoMoreRecords);
end;

{**
  Gets the current record buffer depended on the current dataset state.
  @param RowBuffer a reference to the result row buffer.
  @return <code>True</code> if the buffer was defined.
}
function TZAbstractRODataset.GetActiveBuffer(out RowBuffer: PZRowBuffer):
  Boolean;
var
  RowNo: NativeInt;
  CachedResultSet: IZCachedResultSet;
begin
  RowBuffer := nil;
  {%H-}case State of
    dsBrowse,dsblockread:
      if not IsEmpty then
        RowBuffer := PZRowBuffer(ActiveBuffer);
    dsEdit, dsInsert:
      RowBuffer := PZRowBuffer(ActiveBuffer);
    dsCalcFields:
      RowBuffer := PZRowBuffer(CalcBuffer);
    dsOldValue, dsNewValue, dsCurValue:
      begin
        RowNo := {%H-}NativeInt(CurrentRows[CurrentRow - 1]);
        if RowNo <> ResultSet.GetRow then
          CheckBiDirectional;

        if State = dsOldValue then
          RowBuffer := OldRowBuffer
        else
          RowBuffer := NewRowBuffer;

        if RowBuffer.Index <> RowNo then
        begin
          RowAccessor.RowBuffer := RowBuffer;
          RowAccessor.Clear;
          if (ResultSet.GetRow = RowNo) or ResultSet.MoveAbsolute(RowNo) then
          begin
            if (State = dsOldValue) and (ResultSet.
              QueryInterface(IZCachedResultSet, CachedResultSet) = 0) then
              CachedResultSet.MoveToInitialRow;
            FetchFromResultSet(ResultSet, FieldsLookupTable, Fields, RowAccessor);
            RowBuffer.Index := RowNo;
            ResultSet.MoveToCurrentRow;
          end
          else
            RowBuffer := nil;
        end;
      end;
  end;
  Result := RowBuffer <> nil;
end;

function TZAbstractRODataset.GetFieldData(Field: TField;
  {$IFDEF WITH_VAR_TVALUEBUFFER}var{$ENDIF}Buffer:
  {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
  NativeFormat: Boolean): Boolean;
begin
  if Field.DataType in [ftWideString, ftBCD] then begin
    NativeFormat := True;
    FNativeFormatOverloadCalled := True;
  end else FNativeFormatOverloadCalled := NativeFormat;
  Result := inherited GetFieldData(Field, Buffer, NativeFormat);
end;

{**
  Retrieves the column value and stores it into the field buffer.
  @param Field an field object to be retrieved.
  @param Buffer a field value buffer.
  @return <code>True</code> if non-null value was retrieved.
}
function TZAbstractRODataset.GetFieldData(Field: TField;
  {$IFDEF WITH_VAR_TVALUEBUFFER}var{$ENDIF}Buffer:
    {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF}): Boolean;
var
  ColumnIndex: Integer;
  bLen: Word;
  P: Pointer;
  RowBuffer: PZRowBuffer;
begin
  if GetActiveBuffer(RowBuffer) then
  begin
    if FUseZFields and (Field is TZField) then
      ColumnIndex := (Field as TZField).FFieldIndex
    else
      ColumnIndex := DefineFieldIndex(FieldsLookupTable, Field);
    RowAccessor.RowBuffer := RowBuffer;
    if Buffer <> nil then begin
      case Field.DataType of
        { Processes DateTime fields. }
        ftDate, ftTime, ftDateTime:
          if Field.DataType <> ftTime then
            DateTimeToNative(Field.DataType,
              RowAccessor.GetTimestamp(ColumnIndex, Result), Buffer)
          else
            DateTimeToNative(Field.DataType,
              RowAccessor.GetTime(ColumnIndex, Result), Buffer);
        { Processes binary fields. }
        ftVarBytes:
          begin
            P := RowAccessor.GetBytes(ColumnIndex, Result, PWord(Buffer)^);
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move((PAnsiChar(P)+SizeOf(Word))^,
              Pointer(Buffer)^, Min(PWord(Buffer)^, RowAccessor.GetColumnDataSize(ColumnIndex)));
          end;
        ftBytes:
          begin
            P := RowAccessor.GetBytes(ColumnIndex, Result, bLen);
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(P^, Pointer(Buffer)^, Min(bLen, RowAccessor.GetColumnDataSize(ColumnIndex)));
            FillChar((PAnsiChar(Buffer)+bLen)^, RowAccessor.GetColumnDataSize(ColumnIndex)-blen, #0);
          end;
        { Processes blob fields. }
        ftBlob, ftMemo, ftGraphic, ftFmtMemo {$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF} :
          Result := RowAccessor.GetBlob(ColumnIndex, Result).IsEmpty;
        { Processes String fields. }
        ftWideString:
          Result := FWideStringFieldGetter(ColumnIndex, Field.Size, PWideChar(Buffer));
        ftString:
          Result := FStringFieldGetter(ColumnIndex, Field.Size, PAnsiChar(Buffer));
        {$IFDEF WITH_FTGUID}
        ftGUID:
          begin
            P := RowAccessor.GetColumnData(ColumnIndex, Result);
            if Result then
              PAnsiChar(Buffer)^ := #0
            else
              GUIDToBuffer(P, PAnsiChar(Buffer), True);
          end;
        {$ENDIF}
        {$IFDEF WITH_FTDATASETSUPPORT}
        ftDataSet:
          Result := RowAccessor.GetDataSet(ColumnIndex, Result).IsEmpty;
        {$ENDIF}
        { Processes all other fields. }
        ftCurrency: //sade TCurrencyField is Descendant of TFloatField and uses Double values
          PDouble(Buffer)^ := RowAccessor.GetDouble(ColumnIndex, Result);
        ftBcd: if FNativeFormatOverloadCalled
          then PCurrency(Buffer)^ := RowAccessor.GetCurrency(ColumnIndex, Result)
          else CurrToBCD(RowAccessor.GetCurrency(ColumnIndex, Result), PBCD(Buffer)^);
        else
          {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(RowAccessor.GetColumnData(ColumnIndex, Result)^,
            Pointer(Buffer)^, RowAccessor.GetColumnDataSize(ColumnIndex));
      end;
      Result := not Result;
    end
    else
      if Field.DataType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo {$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}] then
        Result := not RowAccessor.GetBlob(ColumnIndex, Result).IsEmpty
      else
      // added by KestasL
      begin
        {$IFDEF WITH_TVALUEBUFFER}
        //See: http://sourceforge.net/p/zeoslib/tickets/118/
        if Field.DataType = ftExtended then
        begin
          SetLength(Buffer, SizeOf(Extended));
          PExtended(Buffer)^ := RowAccessor.GetBigDecimal(ColumnIndex, Result);
          Result := not Result;
        end
        else
        {$ENDIF WITH_TVALUEBUFFER}
          Result := not RowAccessor.IsNull(ColumnIndex);
      end;
  end
  else
    Result := False;
end;

{**
  Support for widestring field
}
procedure TZAbstractRODataset.SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
  NativeFormat: Boolean);
begin
  if Field.DataType in [ftWideString, ftBCD] then begin
    NativeFormat := True;
    FNativeFormatOverloadCalled := True;
  end else FNativeFormatOverloadCalled := NativeFormat;

  {$IFNDEF VIRTUALSETFIELDDATA}
  inherited SetFieldData(Field, Buffer, NativeFormat);
  {$ELSE}
  SetFieldData(Field, Buffer);
  {$ENDIF}
end;

{**
  Stores the column value from the field buffer.
  @param Field an field object to be stored.
  @param Buffer a field value buffer.
}
procedure TZAbstractRODataset.SetFieldData(Field: TField; Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF});
var
  ColumnIndex: Integer;
  RowBuffer: PZRowBuffer;
  WasNull: Boolean;
begin
  WasNull := False;
  if not Active then
    raise EZDatabaseError.Create(SOperationIsNotAllowed4);
  if not RequestLive and (Field.FieldKind = fkData) then
    RaiseReadOnlyError;
  // Check for readonly updates
  // Lookup values are requeried automatically on edit of all fields.
  // Didn't find a way to avoid this...
  if Field.ReadOnly and (Field.FieldKind <> fkLookup)
                    and not (State in [dsSetKey, dsCalcFields, dsFilter, dsBlockRead, dsInternalCalc, dsOpening]) then
    DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);
  if not (State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);

  if GetActiveBuffer(RowBuffer) then
  begin
    ColumnIndex := DefineFieldIndex(FieldsLookupTable, Field);
    RowAccessor.RowBuffer := RowBuffer;

    {$IFNDEF WITH_ZSTRINGFIELDS}
    if State in [dsEdit, dsInsert] then
      Field.Validate(Buffer);
    {$ENDIF}

    if Assigned(Buffer) then
    begin
      case Field.DataType of
        ftDate, ftDateTime: { Processes Date/DateTime fields. }
          RowAccessor.SetTimestamp(ColumnIndex, NativeToDateTime(Field.DataType, Buffer));
        ftTime: { Processes Time fields. }
          RowAccessor.SetTime(ColumnIndex, NativeToDateTime(Field.DataType, Buffer));
        ftVarBytes: { Processes varbinary fields. }
          RowAccessor.SetBytes(ColumnIndex, PAnsiChar(Buffer)+SizeOf(Word), PWord(Buffer)^);
        ftBytes: { Processes binary array fields. }
          RowAccessor.SetBytes(ColumnIndex, Pointer(Buffer), Field.Size);
        ftWideString: { Processes widestring fields. }
          //EH: Using the WideRec setter doesn't perform better. Don't know why but it seems like the IDE's are faster by setting the UnicodeStrings directly
          {$IFDEF WITH_PWIDECHAR_TOWIDESTRING}
          RowAccessor.SetUnicodeString(ColumnIndex, PWideChar(Buffer));
          {$ELSE}
          RowAccessor.SetUnicodeString(ColumnIndex, PWideString(Buffer)^);
          {$ENDIF}
        ftString: { Processes string fields. }
          FStringFieldSetter(ColumnIndex, PAnsichar(Buffer));
        {$IFDEF WITH_FTGUID}
        ftGUID:
          begin
            ValidGUIDToBinary(PAnsiChar(Buffer), RowAccessor.GetColumnData(ColumnIndex, WasNull));
            RowAccessor.SetNotNull(ColumnIndex);
          end;
        {$ENDIF}
        ftCurrency:
          RowAccessor.SetCurrency(ColumnIndex, PDouble(Buffer)^); //cast Double to Currency
        ftBCD: if FNativeFormatOverloadCalled then begin
            if (Field.Size < 4) then //right truncation? Using the Tbcd record's behaves equal
            PCurrency(Buffer)^ := RoundCurrTo(PCurrency(Buffer)^, Field.Size);
            RowAccessor.SetCurrency(ColumnIndex, PCurrency(Buffer)^);
          end else
            RowAccessor.SetBigDecimal(ColumnIndex, BCDToDouble(PBCD(Buffer)^));
        else  { Processes all other fields. }
          begin
            {$IFDEF FAST_MOVE}ZFastCode{$ELSE}System{$ENDIF}.Move(Pointer(Buffer)^, RowAccessor.GetColumnData(ColumnIndex, WasNull)^,
            RowAccessor.GetColumnDataSize(ColumnIndex));
            RowAccessor.SetNotNull(ColumnIndex);
          end;
      end;
    end
    else
      RowAccessor.SetNull(ColumnIndex);

    if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, NativeInt(Field));
  end
  else
    raise EZDatabaseError.Create(SRowDataIsNotAvailable);

  if Field.FieldKind = fkData then
  begin
    OldRowBuffer.Index := -1;
    NewRowBuffer.Index := -1;
  end;
end;

{**
  Checks is the cursor opened.
  @return <code>True</code> if the cursor is opened.
}
function TZAbstractRODataset.IsCursorOpen: Boolean;
begin
  Result := ResultSet <> nil;
end;

{**
  Gets an affected rows by the last executed statement.
  @return a number of last updated rows.
}
function TZAbstractRODataset.RowsAffected: LongInt;
begin
  Result := FRowsAffected;
end;

{**
  Gets the size of the record buffer.
  @return the size of the record buffer.
}
function TZAbstractRODataset.GetRecordSize: Word;
begin
  Result := RowAccessor.RowSize;
end;

{**
  Allocates a buffer for new record.
  @return an allocated record buffer.
}
{$IFNDEF WITH_AllocRecBuf_TRecBuf}
function TZAbstractRODataset.AllocRecordBuffer: TRecordBuffer;
{$ELSE}
function TZAbstractRODataset.AllocRecBuf: TRecBuf;
{$ENDIF}
begin
  {Dev notes:
   This will be called for OldRowBuffer, NewRowBuffer and for count of visible rows
   so NO memory wasting happens here!
  }
  RowAccessor.Alloc;
  {$IFNDEF WITH_AllocRecBuf_TRecBuf}
  Result := TRecordBuffer(RowAccessor.RowBuffer);
  {$ELSE}
  Result := TRecBuf(RowAccessor.RowBuffer);
  {$ENDIF}
end;

{**
  Frees a previously allocated record buffer.
  @param Buffer a previously allocated buffer.
}
{$IFNDEF WITH_FreeRecBuf_TRecBuf}
procedure TZAbstractRODataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  RowAccessor.DisposeBuffer(PZRowBuffer(Buffer));
  Buffer := nil;
end;
{$ELSE}
procedure TZAbstractRODataset.FreeRecBuf(var Buffer: TRecBuf);
begin
  RowAccessor.DisposeBuffer(PZRowBuffer(Buffer));
  Buffer := 0;
end;
{$ENDIF}

{**
  Fetch all records. Added by Patyi
}
procedure TZAbstractRODataset.FetchAll;
begin
  Connection.ShowSQLHourGlass;
  FetchRows(0);
  if Active then
    UpdateCursorPos;
  Connection.HideSQLHourGlass;
end;

{**
  Executes a DML SQL statement.
}
procedure TZAbstractRODataset.ExecSQL;
begin
  if Active then
    begin
      Connection.ShowSQLHourGlass;
      try
        Close;
      finally
        Connection.HideSQLHourGlass;
      end;
    end;

  Prepare;

  Connection.ShowSQLHourGlass;
  try
    SetStatementParams(Statement, FSQL.Statements[0].ParamNamesArray,
      FParams, FDataLink);

    FRowsAffected := Statement.ExecuteUpdatePrepared;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

{**
  Performs an internal initialization of field defiitions.
}
procedure TZAbstractRODataset.InternalInitFieldDefs;
var
  I, J, Size: Integer;
  AutoInit: Boolean;
  FieldType: TFieldType;
  SQLType: TZSQLType;
  ResultSet: IZResultSet;
  FieldName: string;
  FName: string;
  {$IFDEF WITH_CODEPAGE_AWARE_FIELD}
  CodePage: TSystemCodePage;
  {$ELSE}
  ConSettings: PZConSettings;
  {$ENDIF}
begin
  FieldDefs.Clear;
  ResultSet := Self.ResultSet;
  AutoInit := ResultSet = nil;

  try
    { Opens an internal result set if query is closed. }
    if AutoInit then
    begin
      CheckSQLQuery;
      CheckConnected;
      Prepare;
      ResultSet := CreateResultSet(FSQL.Statements[0].SQL, 0);
    end;
    if not Assigned(ResultSet) then
      raise Exception.Create(SCanNotOpenResultSet);

    { Reads metadata from resultset. }

    with ResultSet.GetMetadata do
    begin
    {$IFNDEF WITH_CODEPAGE_AWARE_FIELD}
    ConSettings := ResultSet.GetConSettings;
    {$ENDIF}
    if GetColumnCount > 0 then
      for I := FirstDbcIndex to GetColumnCount{$IFDEF GENERIC_INDEX}-1{$ENDIF} do
      begin
        SQLType := GetColumnType(I);
        FieldType := ConvertDbcToDatasetType(SQLType);
        if (FieldType = ftCurrency) and not ResultSet.GetMetadata.IsCurrency(I) then
           FieldType := ftBCD;
        if FieldType in [ftBytes, ftVarBytes, ftString, ftWidestring] then begin
          Size := GetPrecision(I);
          {$IFNDEF WITH_CODEPAGE_AWARE_FIELD}
          if (FieldType = ftString) then
            if (ConSettings^.CPType = cCP_UTF8)
            then Size := Size * 4
            else Size := Size * ZOSCodePageMaxCharSize
          else {$ENDIF}if (FieldType = ftWideString) and (doAlignMaxRequiredWideStringFieldSize in Options) {and (ConSettings.ClientCodePage.CharWidth > 3)} then
            Size := Size * 2;

            {if (ConSettings^.CPType = cCP_UTF8) or (ConSettings^.ClientCodePage^.Encoding = ceUTF16) or
               ((not ConSettings^.AutoEncode) and (ConSettings^.ClientCodePage^.Encoding = ceUTF8)) or
               ((ConSettings^.CPType = cGET_ACP) and (ZOSCodePage = zCP_UTF8)) then
              Size := Size * 4
            else
              Size := Size * ConSettings^.ClientCodePage^.CharWidth;}
        end else
          {$IFDEF WITH_FTGUID}
          if FieldType = ftGUID then
            Size := 38
          else
          {$ENDIF}
          if FieldType = ftBCD then
            Size := GetScale(I)
          else
            Size := 0;

        J := 0;
        FieldName := GetColumnLabel(I);
        FName := FieldName;
        while FieldDefs.IndexOf(FName) >= 0 do
        begin
          Inc(J);
          FName := Format('%s_%d', [FieldName, J]);
        end;
        {$IFDEF WITH_CODEPAGE_AWARE_FIELD}
        if FieldType in [ftWideString, ftWideMemo] then
          CodePage := zCP_UTF16
        else if FieldType in [ftString, ftFixedChar, ftMemo] then begin
          CodePage := GetColumnCodePage(I);
          if (CodePage = zCP_UTF16) or Connection.AutoEncodeStrings then
            if Connection.ControlsCodePage = cGET_ACP
            then CodePage := CP_ACP
            else CodePage := zCP_UTF8
        end else CodePage := CP_ACP;
        {$ENDIF}

        if FUseZFields then
          with TZFieldDef.Create(FieldDefs, FName, FieldType, SQLType, Size, False, I{$IFDEF WITH_CODEPAGE_AWARE_FIELD}, CodePage{$ENDIF}) do begin
            if not (ReadOnly or IsUniDirectional) then begin
              {$IFNDEF OLDFPC}
              Required := IsWritable(I) and (IsNullable(I) = ntNoNulls);
              {$ENDIF}
              if IsReadOnly(I) then Attributes := Attributes + [faReadonly];
            end else
              Attributes := Attributes + [faReadonly];
            Precision := GetPrecision(I);
            DisplayName := FName;
          end
        else with TFieldDef.Create(FieldDefs, FName, FieldType, Size, False, I{$IFDEF WITH_CODEPAGE_AWARE_FIELD}, CodePage{$ENDIF}) do begin
          if not (ReadOnly or IsUniDirectional) then begin
            {$IFNDEF OLDFPC}
            Required := IsWritable(I) and (IsNullable(I) = ntNoNulls);
            {$ENDIF}
            if IsReadOnly(I) then Attributes := Attributes + [faReadonly];
          end else
            Attributes := Attributes + [faReadonly];
            Precision := GetPrecision(I);
            DisplayName := FName;
          end;
      end;
    end;

  finally
    { Closes localy opened resultset. }
    if AutoInit then
    begin
      if ResultSet <> nil then
      begin
        ResultSet.Close;
        ResultSet := nil;
      end;
      UnPrepare;
    end;
  end;
end;

{**
  Creates a DBC statement for the query.
  @param SQL an SQL query.
  @param Properties a statement specific properties.
  @returns a created DBC statement.
}
function TZAbstractRODataset.CreateStatement(const SQL: string; Properties: TStrings):
  IZPreparedStatement;
var
  Temp: TStrings;
begin
  Temp := TStringList.Create;
  try
    if Assigned(Properties) then
      Temp.AddStrings(Properties);
    { Define TDataset specific parameters. }
    if doCalcDefaults in FOptions then
      Temp.Values['defaults'] := 'true'
    else
      Temp.Values['defaults'] := 'false';
    if doPreferPrepared in FOptions then
      Temp.Values['preferprepared'] := 'true'
    else
      Temp.Values['preferprepared'] := 'false';
    if doCachedLobs in FOptions then
      Temp.Values['cachedlob'] := 'true'
    else
      Temp.Values['cachedlob'] := 'false';

    Result := FConnection.DbcConnection.PrepareStatementWithParams(SQL, Temp);
  finally
    Temp.Free;
  end;
end;

{**
  Creates a DBC resultset for the query.
  @param SQL an SQL query.
  @param MaxRows a maximum rows number (-1 for all).
  @returns a created DBC resultset.
}
function TZAbstractRODataset.CreateResultSet(const SQL: string;
  MaxRows: Integer): IZResultSet;
begin
  Connection.ShowSQLHourGlass;
  try
    SetStatementParams(Statement, FSQL.Statements[0].ParamNamesArray,
      FParams, FDataLink);
    if RequestLive then
      Statement.SetResultSetConcurrency(rcUpdatable)
    else
      Statement.SetResultSetConcurrency(rcReadOnly);
    Statement.SetFetchDirection(fdForward);
    if IsUniDirectional then
      Statement.SetResultSetType(rtForwardOnly)
    else
      Statement.SetResultSetType(rtScrollInsensitive);
    if MaxRows > 0 then
      Statement.SetMaxRows(MaxRows);

    if doSmartOpen in FOptions then
    begin
      if Statement.ExecutePrepared then
        Result := Statement.GetResultSet
      else
        Result := nil;
    end
    else
      Result := Statement.ExecuteQueryPrepared;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

{**
  Performs internal query opening.
}
procedure TZAbstractRODataset.InternalOpen;
var
  ColumnList: TObjectList;
  I: Integer;
  {$IFDEF WITH_TAUTOREFRESHFLAG}
  MetaData: IZResultSetMetaData;
  {$ENDIF WITH_TAUTOREFRESHFLAG}
begin
  {$IFNDEF FPC}
  If (csDestroying in Componentstate) then
    raise Exception.Create(SCanNotOpenDataSetWhenDestroying);
  {$ENDIF}
  if not FUseCurrentStatment then Prepare;

  CurrentRow := 0;
  FetchCount := 0;
  CurrentRows.Clear;
  FLastRowFetched := False;

  Connection.ShowSQLHourGlass;
  try
    { Creates an SQL statement and resultsets }
    if not FUseCurrentStatment then
      if FSQL.StatementCount> 0 then
        ResultSet := CreateResultSet(FSQL.Statements[0].SQL, -1)
      else
        ResultSet := CreateResultSet('', -1);
      if not Assigned(ResultSet) then
      begin
        if not (doSmartOpen in FOptions) then
          raise Exception.Create(SCanNotOpenResultSet)
        else
          Exit;
      end;

    { Initializes field and index defs. }
    if (not FRefreshInProgress) then
      InternalInitFieldDefs;

    {$IFDEF WITH_LIFECYCLES}
    if ((FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles)) and not FRefreshInProgress then
    {$ELSE}
    if DefaultFields and not FRefreshInProgress then
    {$ENDIF}
    begin
      CreateFields;
      {$IFDEF WITH_TAUTOREFRESHFLAG}
      MetaData := ResultSet.GetMetadata;
      {$ENDIF WITH_TAUTOREFRESHFLAG}
      if not (doNoAlignDisplayWidth in FOptions) then
        for i := 0 to Fields.Count -1 do begin
          if Fields[i].DataType = ftString then
            Fields[i].DisplayWidth := ResultSet.GetMetadata.GetColumnDisplaySize(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF})
          {$IFDEF WITH_FTGUID}
          else if Fields[i].DataType = ftGUID then Fields[i].DisplayWidth := 40 //looks better in Grid
          {$ENDIF};
          {$IFDEF WITH_TAUTOREFRESHFLAG}
          if MetaData.IsAutoIncrement({$IFNDEF GENERIC_INDEX}+1{$ENDIF}) then
            Fields[i].AutoGenerateValue := arAutoInc;
          {$ENDIF !WITH_TAUTOREFRESHFLAG}
        end;
    end;
    BindFields(True);

    if not FRefreshInProgress then begin
      { Initializes accessors and buffers. }
      ColumnList := ConvertFieldsToColumnInfo(Fields);
      try
        RowAccessor := TZRowAccessor.Create(ColumnList, Connection.DbcConnection.GetConSettings)
      finally
        ColumnList.Free;
      end;
      if not IsUnidirectional then
      begin
        {$IFDEF WITH_AllocRecBuf_TRecBuf}
        FOldRowBuffer := PZRowBuffer(AllocRecBuf);
        FNewRowBuffer := PZRowBuffer(AllocRecBuf);
        {$ELSE}
        FOldRowBuffer := PZRowBuffer(AllocRecordBuffer);
        FNewRowBuffer := PZRowBuffer(AllocRecordBuffer);
        {$ENDIF}
      end;

      SetStringFieldSetterAndSetter;

      FieldsLookupTable := CreateFieldsLookupTable(Fields);

      InitFilterFields := False;

      IndexFields.Clear;
      GetFieldList(IndexFields, FLinkedFields); {renamed by bangfauzan}
    end;

    { Performs sorting. }
    if FSortedFields <> '' then
      InternalSort;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

{**
  Performs internal query closing.
}
procedure TZAbstractRODataset.InternalClose;
begin
  if ResultSet <> nil then
    if not FDoNotCloseResultSet then
      ResultSet.ResetCursor;
  ResultSet := nil;
  FLastRowFetched := False;

  if not FRefreshInProgress then begin
    if (FOldRowBuffer <> nil) then
      {$IFNDEF WITH_FreeRecBuf_TRecBuf}
      FreeRecordBuffer(TRecordBuffer(FOldRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
      {$ELSE}
      FreeRecBuf(TRecordBuffer(FOldRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
      {$ENDIF}
    FOldRowBuffer := nil;

    if (FNewRowBuffer <> nil) and not FRefreshInProgress then
      {$IFNDEF WITH_FreeRecBuf_TRecBuf}
      FreeRecordBuffer(TRecordBuffer(FNewRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
      {$ELSE}
      FreeRecBuf(TRecordBuffer(FNewRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
      {$ENDIF}
    FNewRowBuffer := nil;

    {$IFNDEF AUTOREFCOUNT}
    if RowAccessor <> nil then
      RowAccessor.Free;
    {$ENDIF}
    RowAccessor := nil;

    { Destroy default fields }
    {$IFDEF WITH_LIFECYCLES}
    if ((FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in Fields.LifeCycles)) then
    {$ELSE}
    if DefaultFields then
    {$ENDIF}
      DestroyFields;

    FieldsLookupTable := nil;
  end;

  CurrentRows.Clear;
end;

{**
  Performs internal go to first record.
}
procedure TZAbstractRODataset.InternalFirst;
begin
  if CurrentRow > 0 then
    CheckBiDirectional;
  CurrentRow := 0;
end;

{**
  Performs internal go to last record.
}
procedure TZAbstractRODataset.InternalLast;
begin
  FetchRows(0);
  if CurrentRows.Count > 0 then
    CurrentRow := CurrentRows.Count + 1
  else
    CurrentRow := 0;
end;

{**
  Processes internal exception handling.
}
procedure TZAbstractRODataset.InternalHandleException;
begin
//  Application.HandleException(Self);
end;

{**
  Gets the maximum records count.
  @return the maximum records count.
}
function TZAbstractRODataset.GetRecordCount: LongInt;
var RC: Integer;
begin
  CheckActive;
  if not IsUniDirectional and not FLastRowFetched then begin
    RC := FFetchRow;
    if (RC <> 0) and (CurrentRows.Count > FFetchRow) and (CurrentRow = CurrentRows.Count) and
      ((CurrentRows.Count mod FFetchRow) = 0) then
      RC := CurrentRows.Count + FFetchRow; //EH: load data chunked see https://sourceforge.net/p/zeoslib/tickets/399/
    FetchRows(RC);     // the orginal code was FetchRows(0); modifyed by Patyi
  end;
  Result := CurrentRows.Count;
end;

{**
  Gets the current record number.
  @return the current record number.
}
function TZAbstractRODataset.GetRecNo: Longint;
begin
  if Active then
    UpdateCursorPos;
  Result := CurrentRow;
  //EH: load data chunked see https://sourceforge.net/p/zeoslib/tickets/399/
  if not IsUniDirectional and not FLastRowFetched and
    (CurrentRow = CurrentRows.Count) and (FFetchRow > 0) then begin
    FetchRows(CurrentRows.Count+FFetchRow);
    Resync([rmCenter]); //notify we've widened the records
  end;
end;

{**
  Moves current record to the specified record.
  @param Value a new current record number.
}
procedure TZAbstractRODataset.MoveRecNo(Value: Integer);
var
  PreviousCurrentRow: Integer;
begin
  Value := Max(1, Value);
  if Value < CurrentRow then
    CheckBiDirectional;

  if FetchRows(Value) then
    CurrentRow := Value
  else
    CurrentRow := CurrentRows.Count;

  PreviousCurrentRow := CurrentRow;//Resync moves the current row away
  try
    if not (State in [dsInactive]) then
       Resync([]);
  finally
    CurrentRow := PreviousCurrentRow;
  end;
  UpdateCursorPos;
end;

{**
  Sets a new currenct record number.
  @param Value a new current record number.
}
procedure TZAbstractRODataset.SetRecNo(Value: Integer);
begin
  CheckOpened;
  Value := Max(1, Value);
  if Value < CurrentRow then
    CheckBiDirectional;

  DoBeforeScroll;
  MoveRecNo(Value);
  DoAfterScroll;
end;

{**
  Defines is the query editable?
  @return <code>True</code> if the query is editable.
}
function TZAbstractRODataset.GetCanModify: Boolean;
begin
  Result := RequestLive;
end;

{**
  Gets a linked datasource.
  @returns a linked datasource.
}
function TZAbstractRODataset.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

{$IFDEF HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES}
function TZAbstractRODataset.GetUpdatable: Boolean;
begin
  Result := False;
end;
{$ENDIF}

{**
  Sets the value of the Prepared property.
  Setting to <code>True</code> prepares the query. Setting to <code>False</code> unprepares.
  @param Value a new value for the Prepared property.
}
procedure TZAbstractRODataset.SetPrepared(Value: Boolean);
begin
  FUseCurrentStatment := False;
  FDoNotCloseResultSet := False;
  If Value <> FPrepared then
    begin
      If Value then
        InternalPrepare
      else
        InternalUnprepare;
      FPrepared := Value;
    end;
end;

{**
  Sets a new linked datasource.
  @param Value a new linked datasource.
}
procedure TZAbstractRODataset.SetDataSource(Value: TDataSource);
begin
  {$IFNDEF FPC}
  if IsLinkedTo(Value) then
  {$ELSE}
  if Value.IsLinkedTo(Self) then
  {$ENDIF}
    raise EZDatabaseError.Create(SCircularLink);
  DataLink.DataSource := Value;
end;

{**
  Gets a master datasource.
  @returns a master datasource.
}
function TZAbstractRODataset.GetMasterDataSource: TDataSource;
begin
  Result := MasterLink.DataSource;
end;

{**
  Sets a new master datasource.
  @param Value a new master datasource.
}
procedure TZAbstractRODataset.SetMasterDataSource(Value: TDataSource);
begin
  {$IFNDEF FPC}
  if IsLinkedTo(Value) then
  {$ELSE}
  if Value.IsLinkedTo(Self) then
  {$ENDIF}
    raise EZDatabaseError.Create(SCircularLink);
  MasterLink.DataSource := Value;
  RereadRows;
end;

{**
  Gets master link fields.
  @returns a list with master fields.
}
function TZAbstractRODataset.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

{**
  Sets master link fields.
  @param Value a new master link fields.
}
procedure TZAbstractRODataset.SetMasterFields(const Value: string);
begin
  if FMasterLink.FieldNames <> Value then
  begin
    FMasterLink.FieldNames := Value;
    RereadRows;
  end;
end;

{**
  Processes change events from the master dataset.
  @param Sender an event sender object.
}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // TNotifyEvent - parameter not used intentionally
procedure TZAbstractRODataset.MasterChanged(Sender: TObject);
begin
  CheckBrowseMode;
  if (doAlwaysDetailResync in FOptions) or (FMasterLink.DataSet = nil)
    or not (FMasterLink.DataSet.State in [dsEdit, dsInsert]) then
    RereadRows;
end;

{**
  Processes disable events from the master dataset.
  @param Sender an event sender object.
}
procedure TZAbstractRODataset.MasterDisabled(Sender: TObject);
begin
  RereadRows;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Initializes new record with master fields.
}
procedure TZAbstractRODataset.DoOnNewRecord;
var
  I: Integer;
  MasterField, DetailField: TField;
  Temp: Int64;
  P1, P2 : Integer;
begin
  if MasterLink.Active and (MasterLink.Fields.Count > 0) then
  begin
    for I := 0 to MasterLink.Fields.Count - 1 do
    begin
      if I < IndexFields.Count then
      begin
        MasterField := TField(MasterLink.Fields[I]);
        DetailField := TField(IndexFields[I]);
        // Processes LargeInt fields.
        if (MasterField is TLargeIntField)
          or (DetailField is TLargeIntField) then
        begin
          if MasterField is TLargeIntField then
            Temp := TLargeIntField(
              MasterField).{$IFDEF WITH_ASLARGEINT}AsLargeInt{$ELSE}Value{$ENDIF}
          else
            Temp := MasterField.AsInteger;
          if DetailField is TLargeIntField then
            TLargeIntField(DetailField).{$IFDEF WITH_ASLARGEINT}AsLargeInt{$ELSE}Value{$ENDIF} := Temp
          else
            DetailField.AsString := ZFastCode.IntToStr(Temp);
        end
        // Processes all other fields.
        else
          DetailField.Value := MasterField.Value;
      end;
    end;
  end
  else
  begin
    if DataLink.Active and (DataLink.dataset.Fields.Count > 0) then
    begin
      p1 := 1; p2 := 1;
      while (P1 <= Length(LinkedFields)) and (p2 <= Length(MasterFields)) do
      begin
        DetailField := FieldByName(ExtractFieldName(LinkedFields, P1));
        MasterField := DataLink.DataSet.FieldByName (ExtractFieldName(MasterFields, P2));
        DetailField.Assign(MasterField);
      end;
    end;
  end;
  inherited DoOnNewRecord;
end;

{**
  Gets a list of index field names.
  @returns a list of index field names.
}
function TZAbstractRODataset.GetLinkedFields: string; {renamed by bangfauzan}
begin
  Result := FLinkedFields; {renamed by bangfauzan}
end;

{**
  Sets a new list of index field names.
  @param Value a new list of index field names.
}
procedure TZAbstractRODataset.SetLinkedFields(const Value: string); {renamed by bangfauzan}
begin
  if FLinkedFields <> Value then {renamed by bangfauzan}
  begin
    FLinkedFields := Value; {renamed by bangfauzan}
    IndexFields.Clear;
    if State <> dsInactive then
    begin
      GetFieldList(IndexFields, FLinkedFields); {renamed by bangfauzan}
      RereadRows;
    end;
  end;
end;

{**
  Sets a new set of dataset options.
  @param Value a new set of dataset options.
}
procedure TZAbstractRODataset.SetOptions(Value: TZDatasetOptions);
begin
  if FOptions <> Value then
    FOptions := Value;
end;

{**
  Sets a new sorted fields.
  @param Value a new sorted fields.
}
procedure TZAbstractRODataset.SetSortedFields({const} Value: string); {bangfauzan modification}
begin
  Value:=Trim(Value); {bangfauzan addition}
  if (FSortedFields <> Value) or (FIndexFieldNames <> Value)then {bangfauzan modification}
  begin
    FIndexFieldNames:=Value;
    FSortType := GetSortType; {bangfauzan addition}
    {removing ASC or DESC behind space}
    if (FSortType <> stIgnored) then
    begin {pawelsel modification}
       Value:=StringReplace(Value,' Desc','',[rfReplaceAll,rfIgnoreCase]);
       Value:=StringReplace(Value,' Asc','',[rfReplaceAll,rfIgnoreCase]);
    end;
    FSortedFields := Value;
    if Active then
      if not ({$IFDEF FPC}Updatable{$ELSE}Self is TZAbstractDataSet{$ENDIF}) then
        InternalSort //enables clearsort which prevents rereading data
      else
        {bangfauzan modification}
        if (FSortedFields = '') then
          InternalRefresh
        else
          InternalSort;
      {end of bangfauzan modification}
  end;
end;

{**
  Refreshes parameters and reopens the dataset.
}
procedure TZAbstractRODataset.RefreshParams;
var
  DataSet: TDataSet;
begin
  DisableControls;
  try
    if FDataLink.DataSource <> nil then
    begin
      DataSet := FDataLink.DataSource.DataSet;
      if DataSet <> nil then
        if DataSet.Active and not (DataSet.State in [dsSetKey, dsEdit]) then
        begin
          Refresh;
        end;
    end;
  finally
    EnableControls;
  end;
end;

{**
  Performs the internal preparation of the query.
}
procedure TZAbstractRODataset.InternalPrepare;
begin
  CheckSQLQuery;
  CheckInactive;  //AVZ - Need to check this
  CheckConnected;

  Connection.ShowSQLHourGlass;
  try
    if (FSQL.StatementCount > 0) and((Statement = nil) or (Statement.GetConnection.IsClosed)) then
      Statement := CreateStatement(FSQL.Statements[0].SQL, Properties)
    else
      if (Assigned(Statement)) then
         Statement.ClearParameters;
  finally
    Connection.HideSQLHourGlass;
  end;
end;

{**
  Rolls back the internal preparation of the query.
}
procedure TZAbstractRODataset.InternalUnPrepare;
begin
  if Statement <> nil then begin
    Statement.Close;
    Statement := nil;
  end;
end;

{**
  Performs internal switch to the specified bookmark.
  @param Bookmark a specified bookmark.
}
{$IFDEF WITH_InternalGotoBookmark_TBookmark}
procedure TZAbstractRODataset.InternalGotoBookmark(Bookmark: TBookmark);
{$ELSE}
procedure TZAbstractRODataset.InternalGotoBookmark(Bookmark: Pointer);
{$ENDIF}
begin
  if not GotoRow(PInteger(Bookmark)^) then
    raise EZDatabaseError.Create(SBookmarkWasNotFound);
end;

{**
  Performs an internal switch to the specified record.
  @param Buffer the specified row buffer.
}

procedure TZAbstractRODataset.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  GotoRow(PZRowBuffer(Buffer)^.Index);
end;

{$IFNDEF WITH_FIELDDEFLIST}
function TZAbstractRODataset.GetFieldDefListClass: TFieldDefListClass;
begin
  Result := DefaultFieldDefListClass;
end;
{$ENDIF}

{$IFNDEF WITH_VIRTUAL_DEFCHANGED}
{$IFDEF FPC} {$PUSH} {$WARN 5024 off : Parameter "$1" not used} {$ENDIF} // base class - parameter not used intentionally
procedure TZAbstractRODataset.DefChanged(Sender: TObject);
begin
end;
{$IFDEF FPC} {$POP} {$ENDIF}
{$ENDIF}

{$IFNDEF WITH_DATASETFIELD}
procedure TZAbstractRODataset.SetDataSetField(const Value: TDataSetField);
begin
  if Value <> FDataSetField then
  begin
    if (Value <> nil) and ((Value.DataSet = Self) or
       ((TZAbstractRODataset(Value.DataSet).GetDataSource <> nil) and
        (TZAbstractRODataset(Value.DataSet).GetDataSource.DataSet = Self))) then
      DatabaseError('Circular DataLink', Self);
    if Assigned(Value) and not InheritsFrom(TZAbstractRODataset(Value.DataSet).NestedDataSetClass) then
      DatabaseErrorFmt('Dataset must inherite from %s', [TZAbstractRODataset(Value.DataSet).NestedDataSetClass.ClassName], Self);
    if Active then Close;
    if Assigned(FDataSetField) then
      FDataSetField.AssignNestedDataSet(nil);
    FDataSetField := Value;
    if Assigned(Value) then
    begin
      Value.AssignNestedDataSet(Self);
      if Value.DataSet.Active then Open;
    end;
  end;
end;
{$ENDIF}

{**
  Performs an internal adding a new record.
  @param Buffer a buffer of the new adding record.
  @param Append <code>True</code> if record should be added to the end
    of the result set.
}
{$IFNDEF WITH_InternalAddRecord_TRecBuf}
procedure TZAbstractRODataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
{$ELSE}
procedure TZAbstractRODataset.InternalAddRecord(Buffer: TRecBuf; Append: Boolean);
{$ENDIF}
begin
  RaiseReadOnlyError;
end;

{**
  Performs an internal record removing.
}
procedure TZAbstractRODataset.InternalDelete;
begin
  RaiseReadOnlyError;
end;

{**
  Performs an internal post updates.
}
procedure TZAbstractRODataset.InternalPost;
  procedure Checkrequired;
  var
    I: longint;
    columnindex : integer;
  begin
    For I:=0 to Fields.Count-1 do
      With Fields[i] do
        Case State of
         dsEdit:
          if Required and not ReadOnly and (FieldKind=fkData) and IsNull then
            raise EZDatabaseError.Create(Format(SNeedField,[DisplayName]));
         dsInsert:
          if Required and not ReadOnly and (FieldKind=fkData) and IsNull then
            begin
           // allow autoincrement and defaulted fields to be null;
              columnindex := Resultset.FindColumn(Fields[i].FieldName);
              if (Columnindex = InvalidDbcIndex) or
                 (not Resultset.GetMetadata.HasDefaultValue(columnIndex) and
                  not Resultset.GetMetadata.IsAutoIncrement(columnIndex)) then
                raise EZDatabaseError.Create(Format(SNeedField,[DisplayName]));
            end;
        End;
  end;

begin
  if not ({$IFDEF FPC}Updatable{$ELSE}Self is TZAbstractDataSet{$ENDIF}) then
    RaiseReadOnlyError;

  Checkrequired;
end;

{**
  Gets a bookmark flag from the specified record.
  @param Buffer a pointer to the record buffer.
  @return a bookmark flag from the specified record.
}
function TZAbstractRODataset.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := TBookmarkFlag(PZRowBuffer(Buffer)^.BookmarkFlag);
end;

{**
  Sets a new bookmark flag to the specified record.
  @param Buffer a pointer to the record buffer.
  @param Value a new bookmark flag to the specified record.
}

procedure TZAbstractRODataset.SetBookmarkFlag(Buffer: TRecordBuffer;
  Value: TBookmarkFlag);
begin
  PZRowBuffer(Buffer)^.BookmarkFlag := Ord(Value);
end;

{**
  Gets bookmark value from the specified record.
  @param Buffer a pointer to the record buffer.
  @param Data a pointer to the bookmark value.
}

procedure TZAbstractRODataset.GetBookmarkData(
  Buffer: TRecordBuffer;
  Data: {$IFDEF WITH_BOOKMARKDATA_TBOOKMARK}TBookMark{$ELSE}Pointer{$ENDIF});
begin
  PInteger(Data)^ := PZRowBuffer(Buffer)^.Index;
end;

{**
  Sets a new bookmark value from the specified record.
  @param Buffer a pointer to the record buffer.
  @param Data a pointer to the bookmark value.
}


procedure TZAbstractRODataset.SetBookmarkData(
  Buffer: TRecordBuffer;
  Data: {$IFDEF WITH_BOOKMARKDATA_TBOOKMARK}TBookMark{$ELSE}Pointer{$ENDIF});
begin
  PZRowBuffer(Buffer)^.Index := PInteger(Data)^;
end;

{**
  Compare two specified bookmarks.
  @param Bookmark1 the first bookmark object.
  @param Bookmark2 the second bookmark object.
  @return 0 if bookmarks are equal, -1 if the first bookmark is less,
    1 if the first bookmark is greatter.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.CompareBookmarks(Bookmark1,
  Bookmark2: TBookmark): Integer;
var
  Index1, Index2: Integer;
begin
  Result := 0;
  if not Assigned(Bookmark1) or not Assigned(Bookmark2) then
    Exit;

  Index1 := CurrentRows.IndexOf({%H-}Pointer(PInteger(Bookmark1)^));
  Index2 := CurrentRows.IndexOf(Pointer(PInteger(Bookmark2)^));

  if Index1 < Index2 then Result := -1
  else if Index1 > Index2 then Result := 1;
end;
{$IFDEF FPC} {$POP} {$ENDIF}


{**
  Checks is the specified bookmark valid.
  @param Bookmark a bookmark object.
  @return <code>True</code> if the bookmark is valid.
}
{$IFDEF FPC} {$PUSH} {$WARN 4055 off : Conversion between ordinals and pointers is not portable} {$ENDIF}
function TZAbstractRODataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := False;
  if Active and Assigned(Bookmark) and (FResultSet <> nil) and (CurrentRows <> nil) then
    Result := CurrentRows.IndexOf(Pointer(PInteger(Bookmark)^)) >= 0;
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{**
  Performs an internal initialization of record buffer.
  @param Buffer a record buffer for initialization.
}

procedure TZAbstractRODataset.InternalInitRecord(Buffer: TRecordBuffer);
begin
  RowAccessor.ClearBuffer(PZRowBuffer(Buffer));
end;

{**
  Performs an internal refreshing.
}
procedure TZAbstractRODataset.InternalRefresh;
var
  RowNo: NativeInt;
  Found: Boolean;
  KeyFields: string;
  Temp: TZVariantDynArray;
  KeyValues: Variant;
  FieldRefs: TObjectDynArray;
  OnlyDataFields: Boolean;
begin
  OnlyDataFields := False;
  FieldRefs := nil;
  if Active then
  begin
    if CurrentRow > 0 then
    begin
      RowNo := {%H-}NativeInt(CurrentRows[CurrentRow - 1]);
      if ResultSet.GetRow <> RowNo then
        ResultSet.MoveAbsolute(RowNo);

      if Properties.Values['KeyFields'] <> '' then
        KeyFields := Properties.Values['KeyFields']
      else
        KeyFields := DefineKeyFields(Fields, Connection.DbcConnection.GetMetadata.GetIdentifierConvertor);
      FieldRefs := DefineFields(Self, KeyFields, OnlyDataFields, Connection.DbcConnection.GetDriver.GetTokenizer);
      SetLength(Temp, Length(FieldRefs));
      RetrieveDataFieldsFromResultSet(FieldRefs, ResultSet, Temp);
      if Length(FieldRefs) = 1 then
        KeyValues := EncodeVariant(Temp[0])
      else
        KeyValues := EncodeVariantArray(Temp);
    end
    else
    begin
      KeyFields := '';
      KeyValues := Unassigned;
    end;

    DisableControls;
    try
      try
        FRefreshInProgress := True;
        InternalClose;
        InternalOpen;
      finally
        FRefreshInProgress := False;
      end;

      DoBeforeScroll;
      if KeyFields <> '' then
        Found := Locate(KeyFields, KeyValues, [])
      else
        Found := False;
    finally
      EnableControls;
    end;

    if not Found then
    begin
      DoBeforeScroll;
      DoAfterScroll;
    end;
  end;
end;

{**
  Finds the next record in a filtered query.
  @param Restart a <code>True</code> to find from the start of the query.
  @param GoForward <code>True</code> to navigate in the forward direction.
  @return <code>True</code> if a sutisfied row was found.
}
function TZAbstractRODataset.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  Index: Integer;
  SavedFilterEnabled: Boolean;
begin
  { Checks the current state. }
  CheckBrowseMode;
  DoBeforeScroll;
  Result := False;

  { Defines an initial position position. }
  if Restart then
  begin
    if GoForward then
      Index := 1
    else
    begin
      FetchRows(0);
      Index := CurrentRows.Count;
    end
  end
  else
  begin
    Index := CurrentRow;
    if GoForward then
    begin
      Inc(Index);
      if Index > CurrentRows.Count then
        FetchOneRow;
    end
    else
      Dec(Index);
  end;

  { Finds a record. }
  SavedFilterEnabled := FilterEnabled;
  try
    FilterEnabled := True;
    while (Index >= 1) and (Index <= CurrentRows.Count) do
    begin
      if FilterRow(Index) then
      begin
        Result := True;
        Break;
      end;
      if GoForward then
      begin
        Inc(Index);
        if Index > CurrentRows.Count then
          FetchOneRow;
      end
      else
        Dec(Index)
    end
  finally
    FilterEnabled := SavedFilterEnabled;
  end;

  { Sets a new found position. }
  SetFound(Result);
  if Result then
  begin
    MoveRecNo(Index);
    DoAfterScroll;
  end;
end;

{$IFDEF FPC}
function TZAbstractRODataset.FindFirst: Boolean;
begin
  Result := FindRecord(True, True);
end;

function TZAbstractRODataset.FindLast: Boolean;
begin
  Result := FindRecord(True, False);
end;

function TZAbstractRODataset.FindNext: Boolean;
begin
  Result := FindRecord(False, True);
end;

function TZAbstractRODataset.FindPrior: Boolean;
begin
  Result := FindRecord(False, False);
end;
{$ENDIF}

{**
  Sets a filtering control flag.
  @param Value <code>True</code> to turn filtering On.
}
procedure TZAbstractRODataset.SetFiltered(Value: Boolean);
begin
  if Value <> FilterEnabled then
  begin
    FilterEnabled := Value;
    inherited SetFiltered(Value);
    RereadRows;
  end;
end;

{**
  Sets a new filter expression string.
  @param Value a new filter expression.
}
procedure TZAbstractRODataset.SetFilterText(const Value: string);
begin
  inherited SetFilterText(Value);
  FilterExpression.DefaultVariables.Clear;
  FilterExpression.Expression := Value;
  InitFilterFields := False;
  if FilterEnabled then
    RereadRows;
end;

{$IFNDEF WITH_OBJECTVIEW}
procedure TZAbstractRODataset.SetObjectView(const Value: Boolean);
begin
  CheckInactive;
  FObjectView := Value;
end;
{$ENDIF WITH_OBJECTVIEW}
{**
  Checks is the opened resultset sequensed?
  @return <code>True</code> if the opened resultset is sequenced.
}
function TZAbstractRODataset.IsSequenced: Boolean;
begin
  Result := (not FilterEnabled);
end;

function TZAbstractRODataset.NextResultSet: Boolean;
begin
  Result := False;
  if Assigned(Statement) and Statement.GetMoreResults then begin
    Result := True;
    SetAnotherResultset(Statement.GetResultSet);
  end;
end;

{**
  Processes component notifications.
  @param AComponent a changed component object.
  @param Operation a component operation code.
}
procedure TZAbstractRODataset.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (AComponent = FConnection) then
  begin
    Close;
    FConnection := nil;
  end;

  if (Operation = opRemove) and Assigned(FDataLink)
    and (AComponent = FDataLink.Datasource) then
    FDataLink.DataSource := nil;

  if (Operation = opRemove) and Assigned(FMasterLink)
    and (AComponent = FMasterLink.Datasource) then
  begin
    FMasterLink.DataSource := nil;
    RereadRows;
  end;
end;

{**
  Performs an internal record search.
  @param KeyFields a list of field names.
  @param KeyValues a list of field values.
  @param Options a search options.
  @return an index of found row or -1 if nothing was found.
}
function TZAbstractRODataset.InternalLocate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): LongInt;
var
  I, RowNo, RowCount: Integer;
  FieldRefs: TObjectDynArray;
  FieldIndices: TIntegerDynArray;
  OnlyDataFields: Boolean;
  SearchRowBuffer: PZRowBuffer;
  DecodedKeyValues: TZVariantDynArray;
  RowValues: TZVariantDynArray;
  PartialKey: Boolean;
  CaseInsensitive: Boolean;
begin
  OnlyDataFields := False;
  CheckBrowseMode;
  Result := -1;
  DecodedKeyValues := nil;

  PartialKey := loPartialKey in Options;
  CaseInsensitive := loCaseInsensitive in Options;

  FieldRefs := DefineFields(Self, KeyFields, OnlyDataFields, Connection.DbcConnection.GetDriver.GetTokenizer);
  FieldIndices := nil;
  if FieldRefs = nil then
     Exit;
  DecodedKeyValues := DecodeVariantArray(KeyValues);

  { Checks for equal field and values number }
  if Length(FieldRefs) <> Length(DecodedKeyValues) then
    raise EZDatabaseError.Create(SIncorrectSearchFieldsNumber);
  SetLength(RowValues, Length(DecodedKeyValues));

  if not OnlyDataFields then
  begin
    { Processes fields if come calculated or lookup fields are involved. }
    {$IFDEF WITH_AllocRecBuf_TRecBuf}
    SearchRowBuffer := PZRowBuffer(AllocRecBuf);
    {$ELSE}
    SearchRowBuffer := PZRowBuffer(AllocRecordBuffer);
    {$ENDIF}
    try
      I := 0;
      FieldIndices := DefineFieldIndices(FieldsLookupTable, FieldRefs);
      RowCount := CurrentRows.Count;
      while True do begin
        while (I >= RowCount) and FetchOneRow do
          RowCount := CurrentRows.Count;
        if I >= RowCount then
          Break;

        RowNo := {%H-}Integer(CurrentRows[I]);
        ResultSet.MoveAbsolute(RowNo);

        RowAccessor.RowBuffer := SearchRowBuffer;
        RowAccessor.RowBuffer^.Index := RowNo;
        FetchFromResultSet(ResultSet, FieldsLookupTable, Fields, RowAccessor);
        GetCalcFields(TGetCalcFieldsParamType(SearchRowBuffer));
        RetrieveDataFieldsFromRowAccessor(
          FieldRefs, FieldIndices, RowAccessor, RowValues);

        if CompareDataFields(DecodedKeyValues, RowValues,
          PartialKey, CaseInsensitive) then begin
          Result := I + 1;
          Break;
        end;

        Inc(I);
      end;
    finally
      if SearchRowBuffer <> nil then
        {$IFNDEF WITH_FreeRecBuf_TRecBuf}
        FreeRecordBuffer(TRecordBuffer(SearchRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
        {$ELSE}
        FreeRecBuf(TRecordBuffer(SearchRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
        {$ENDIF}
    end;
  end
  else
  begin
    PrepareValuesForComparison(FieldRefs, DecodedKeyValues,
      ResultSet, PartialKey, CaseInsensitive);

    { Processes only data fields. }
    I := 0;
    RowCount := CurrentRows.Count;
    while True do
    begin
      while (I >= RowCount) and FetchOneRow do
        RowCount := CurrentRows.Count;
      if I >= RowCount then
        Break;

      RowNo := {%H-}Integer(CurrentRows[I]);
      ResultSet.MoveAbsolute(RowNo);

      if CompareFieldsFromResultSet(FieldRefs, DecodedKeyValues,
        ResultSet, PartialKey, CaseInsensitive) then
      begin
        Result := I + 1;
        Break;
      end;

      Inc(I);
    end;
  end;
end;

{**
  Locates an interested record by specified search criteria.
  @param KeyFields a list of field names.
  @param KeyValues a list of field values.
  @param Options a search options.
  @return <code>True</code> if record was found or <code>False</code> otherwise.
}
function TZAbstractRODataset.Locate(const KeyFields: string;
  const KeyValues: Variant; Options: TLocateOptions): Boolean;
var
  Index: Integer;
begin
  DoBeforeScroll;
  if (Active) then //AVZ Check if the dataset is active before performing locate - return false otherwise
  begin
    Index := InternalLocate(KeyFields, KeyValues, Options);
    if Index > 0 then
    begin
      MoveRecNo(Index);
      DoAfterScroll;
      Result := True;
    end
    else
      Result := False;
    SetFound(Result);

  end
    else
  begin
    Result := False;
  end;
end;

{**
  Lookups specified fields from the searched record.
  @param KeyValues a list of field names to search record.
  @param KeyValues an array of field values to search record.
  @param ResultFields a list of field names to return as a result.
  @return an array of requested field values.
}
function TZAbstractRODataset.Lookup(const KeyFields: string;
  const KeyValues: Variant; const ResultFields: string): Variant;
var
  RowNo: Integer;
  FieldRefs: TObjectDynArray;
  FieldIndices: TIntegerDynArray;
  OnlyDataFields: Boolean;
  SearchRowBuffer: PZRowBuffer;
  ResultValues: TZVariantDynArray;
begin
  OnlyDataFields := False;
  Result := Null;
  RowNo := InternalLocate(KeyFields, KeyValues, []);
  FieldRefs := nil;
  FieldIndices := nil;
  if RowNo < 0 then
     Exit;

  { Fill result array }
  FieldRefs := DefineFields(Self, ResultFields, OnlyDataFields, Connection.DbcConnection.GetDriver.GetTokenizer);
  FieldIndices := DefineFieldIndices(FieldsLookupTable, FieldRefs);
  SetLength(ResultValues, Length(FieldRefs));
  {$IFDEF WITH_AllocRecBuf_TRecBuf}
  SearchRowBuffer := PZRowBuffer(AllocRecBuf);
  {$ELSE}
  SearchRowBuffer := PZRowBuffer(AllocRecordBuffer);
  {$ENDIF}
  try
    RowNo := {%H-}Integer(CurrentRows[RowNo - 1]);
    if ResultSet.GetRow <> RowNo then
      ResultSet.MoveAbsolute(RowNo);

    RowAccessor.RowBuffer := SearchRowBuffer;
    RowAccessor.RowBuffer^.Index := RowNo;
    FetchFromResultSet(ResultSet, FieldsLookupTable, Fields, RowAccessor);
    GetCalcFields(TGetCalcFieldsParamType(SearchRowBuffer));
    RetrieveDataFieldsFromRowAccessor(
      FieldRefs, FieldIndices, RowAccessor, ResultValues);
  finally
    {$IFNDEF WITH_FreeRecBuf_TRecBuf}
    FreeRecordBuffer(TRecordBuffer(SearchRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
    {$ELSE}
    FreeRecBuf(TRecordBuffer(SearchRowBuffer));   // TRecordBuffer can be both pbyte and pchar in FPC. Don't assume.
    {$ENDIF}
  end;

  if Length(FieldIndices) = 1 then
    Result := EncodeVariant(ResultValues[0])
  else
    Result := EncodeVariantArray(ResultValues);
end;

{**
  Gets the updated status for the current row.
  @return the UpdateStatus value for the current row.
}
function TZAbstractRODataset.UpdateStatus: TUpdateStatus;
var
  RowNo: Integer;
begin
  Result := usUnmodified;
  if (ResultSet <> nil) and (CurrentRows.Count > 0) then
  begin
    RowNo := {%H-}Integer(CurrentRows[CurrentRow - 1]);
    if ResultSet.GetRow <> RowNo then
      ResultSet.MoveAbsolute(RowNo);

    if ResultSet.RowInserted then
      Result := usInserted
    else if ResultSet.RowUpdated then
      Result := usModified
    else if ResultSet.RowDeleted then
      Result := usDeleted;
  end;
end;

{**
  Translates strings between ansi and oem character sets.
}
{$IFNDEF NO_TDATASET_TRANSLATE}
function TZAbstractRODataset.Translate(Src, Dest: PAnsiChar; ToOem: Boolean):
   Integer;
begin
  if (Src <> nil) then
  begin
    Result := ZFastCode.StrLen(Src);
  {$IFDEF MSWINDOWS}
    if doOemTranslate in FOptions then
    begin
      if ToOem then
        CharToOemA(Src, Dest)
      else
        OemToCharA(Src, Dest);
      Dest[Result] := #0;
    end
    else
  {$ENDIF}
    begin
      if (Src <> Dest) then
      {$IFDEF WITH_STRCOPY_DEPRECATED}AnsiStrings.{$ENDIF}StrCopy(Dest, Src);
    end;
  end
  else
    Result := 0;
end;
{$ENDIF}
{**
  Prepares the query.
  If this actually does happen at the database connection level depends on the
  specific implementation.
}
procedure TZAbstractRODataset.Prepare;
begin
  Prepared := True;
end;

{**
  Unprepares the query.
  Before the query gets executed it must be prepared again.
}
procedure TZAbstractRODataset.Unprepare;
begin
  Prepared := False;
end;

{**
  Creates a stream object for specified blob field.
  @param Field an interested field object.
  @param Mode a blob open mode.
  @return a created stream object.
}
function TZAbstractRODataset.CreateBlobStream(Field: TField;
  Mode: TBlobStreamMode): TStream;
var
  ColumnIndex: Integer;
  RowBuffer: PZRowBuffer;
  Blob: IZBlob;
  WasNull: Boolean;
begin
  WasNull := False;
  CheckActive;

  Result := nil;
  if (Field.DataType in [ftBlob, ftMemo, ftGraphic, ftFmtMemo {$IFDEF WITH_WIDEMEMO},ftWideMemo{$ENDIF}])
    and GetActiveBuffer(RowBuffer) then
  begin
    ColumnIndex := DefineFieldIndex(FieldsLookupTable, Field);
    RowAccessor.RowBuffer := RowBuffer;

    if Mode = bmRead then
    begin
      Blob := RowAccessor.GetBlob(ColumnIndex, WasNull);
      Result := TZBlobStream.Create(Field as TBlobField, Blob, Mode,
        FConnection.DbcConnection.GetConSettings);
    end
    else
    begin
      Blob := RowAccessor.GetBlob(ColumnIndex, WasNull);
      if Blob <> nil then
        Blob := Blob.Clone(Mode =  bmWrite);
        RowAccessor.SetBlob(ColumnIndex, Blob);
      Result := TZBlobStream.Create(Field as TBlobField, Blob, Mode,
        FConnection.DbcConnection.GetConSettings);
    end;
  end;
  if Result = nil then
    Result := TMemoryStream.Create;
end;

function TZAbstractRODataset.CreateNestedDataSet(DataSetField: TDataSetField): TDataSet;
begin
  {$IFDEF WITH_FTDATASETSUPPORT}
  Result := inherited CreateNestedDataSet(DataSetField);
  {$ELSE}
  Result := nil;
  {$ENDIF}
end;

{**
  Closes the specified BLOB field.
  @param a BLOB field object.
}
procedure TZAbstractRODataset.CloseBlob(Field: TField);
begin
end;

{**
  Closes the cursor-handles. Releases(not closing) the current resultset
  and opens the cursorhandles. The current statment is used further.
  @param the NewResultSet
}
procedure TZAbstractRODataset.SetAnotherResultset(const Value: IZResultSet);
begin
  {EgonHugeist: I was forced to go this stupid sequence
    first i wanted to exclude parts of InternalOpen/Close but this didn't solve
    the DataSet issues. You can't init the fields as long the Cursor is not
    closed.. Which is equal to cursor open}
  if Assigned(Value) and ( Value <> ResultSet ) then
  begin
    FDoNotCloseResultSet := True; //hint for InternalClose
    SetState(dsInactive);
    CloseCursor; //Calls InternalOpen in his sequence so InternalClose must be prepared
    FDoNotCloseResultSet := False; //reset hint for InternalClose
    ResultSet := Value; //Assign the new resultset
    if not ResultSet.IsBeforeFirst then
      ResultSet.BeforeFirst; //need this. All from dataset buffered resultsets are EOR
    FUseCurrentStatment := True; //hint for InternalOpen
    OpenCursor{$IFDEF FPC}(False){$ENDIF}; //Calls InternalOpen in his sequence so InternalOpen must be prepared
    OpenCursorComplete; //set DataSet to dsActive
    FUseCurrentStatment := False; //reset hint for InternalOpen
  end;
end;

{**
  Performs sorting of the internal rows.
}
procedure TZAbstractRODataset.InternalSort;
var
  I: Integer;
  RowNo: NativeInt;
  SavedRowBuffer: PZRowBuffer;
begin
  //if FIndexFieldNames = '' then exit; {bangfauzan addition}
  if (ResultSet <> nil) and not IsUniDirectional then
  begin
    FIndexFieldNames := Trim(FIndexFieldNames); {bangfauzan modification}
    DefineSortedFields(Self, {FSortedFields} FIndexFieldNames {bangfauzan modification},
    FSortedFieldRefs, FSortedComparsionKinds, FSortedOnlyDataFields);

    if (CurrentRow <= CurrentRows.Count) and (CurrentRows.Count > 0)
      and (CurrentRow > 0) then
      RowNo := {%H-}NativeInt(CurrentRows[CurrentRow - 1])
    else
      RowNo := -1;

    { Restores the previous order. }
    if Length(FSortedFieldRefs) = 0 then
    begin
      CurrentRows.Sort(ClearSort);
    end
    else
    begin
      FetchRows(0);
      if FSortedOnlyDataFields then
      begin
        { Converts field objects into field indices. }
        SetLength(FSortedFieldIndices, Length(FSortedFieldRefs));
        for I := 0 to High(FSortedFieldRefs) do
          FSortedFieldIndices[I] := TField(FSortedFieldRefs[I]).FieldNo{$IFDEF GENERIC_INDEX}-1{$ENDIF};
        { Performs a sorting. }
        FCompareFuncs := ResultSet.GetCompareFuncs(FSortedFieldIndices, FSortedComparsionKinds);
        CurrentRows.Sort(LowLevelSort);
      end
      else
      begin
        SavedRowBuffer := RowAccessor.RowBuffer;
        { Sorts using generic highlevel approach. }
        try
          { Allocates buffers for sorting. }
          FSortRowBuffer1 := RowAccessor.AllocBuffer;
          FSortRowBuffer2 := RowAccessor.AllocBuffer;
          { Converts field objects into field indices. }
          SetLength(FSortedFieldIndices, Length(FSortedFieldRefs));
          for I := 0 to High(FSortedFieldRefs) do
          begin
            FSortedFieldIndices[I] := DefineFieldIndex(FieldsLookupTable,
              TField(FSortedFieldRefs[I]));
          end;
          { Performs sorting. }
          FCompareFuncs := RowAccessor.GetCompareFuncs(FSortedFieldIndices, FSortedComparsionKinds);
          CurrentRows.Sort(HighLevelSort);
        finally
          { Disposed buffers for sorting. }
          RowAccessor.DisposeBuffer(FSortRowBuffer1);
          RowAccessor.DisposeBuffer(FSortRowBuffer2);
          RowAccessor.RowBuffer := SavedRowBuffer;
        end;
      end;
    end;

    CurrentRow := CurrentRows.IndexOf({%H-}Pointer(RowNo)) + 1;
    CurrentRow := Min(Max(0, CurrentRow), CurrentRows.Count);
    if not (State in [dsInactive]) then
       Resync([]);
  end;
end;

{**
  Clears list sorting and restores the previous order.
  @param Item1 a reference to the first row.
  @param Item2 a reference to the second row.
  @returns &gt;0 if Item1 &gt; Item2, &lt;0 it Item1 &lt; Item2 and 0
    if Item1 and Item2 are equal.
}
function TZAbstractRODataset.ClearSort(Item1, Item2: Pointer): Integer;
begin
  //no real pointer addresses here, just a Integer represented as Pointer! -> overflow save!
  Result := {%H-}NativeInt(Item1) - {%H-}NativeInt(Item2);
end;

{**
  Sorting list using generic approach which is slow but may be used
  with calculated fields.

  @param Item1 a reference to the first row.
  @param Item2 a reference to the second row.
  @returns &gt;0 if Item1 &gt; Item2, &lt;0 it Item1 &lt; Item2 and 0
    if Item1 and Item2 are equal.
}
function TZAbstractRODataset.HighLevelSort(Item1, Item2: Pointer): Integer;
var
  RowNo: NativeInt;
begin
  { Gets the first row. }
  RowNo := {%H-}NativeInt(Item1);
  ResultSet.MoveAbsolute(RowNo);
  RowAccessor.RowBuffer := FSortRowBuffer1;
  RowAccessor.RowBuffer^.Index := RowNo;
  FetchFromResultSet(ResultSet, FieldsLookupTable, Fields, RowAccessor);
  FRowAccessor.RowBuffer^.BookmarkFlag := Ord(bfCurrent);
  GetCalcFields(TGetCalcFieldsParamType(FSortRowBuffer1));

  { Gets the second row. }
  RowNo := {%H-}NativeInt(Item2);
  ResultSet.MoveAbsolute(RowNo);
  RowAccessor.RowBuffer := FSortRowBuffer2;
  RowAccessor.RowBuffer^.Index := RowNo;
  FetchFromResultSet(ResultSet, FieldsLookupTable, Fields, RowAccessor);
  FRowAccessor.RowBuffer^.BookmarkFlag := Ord(bfCurrent);
  GetCalcFields(TGetCalcFieldsParamType(FSortRowBuffer2));

  { Compare both records. }
  Result := RowAccessor.CompareBuffers(FSortRowBuffer1, FSortRowBuffer2,
    FSortedFieldIndices, FCompareFuncs);
end;

{**
  Sorting list using lowlevel approach which is fast but may not be used
  with calculated fields.

  @param Item1 a reference to the first row.
  @param Item2 a reference to the second row.
  @returns &gt;0 if Item1 &gt; Item2, &lt;0 it Item1 &lt; Item2 and 0
    if Item1 and Item2 are equal.
}
function TZAbstractRODataset.LowLevelSort(Item1, Item2: Pointer): Integer;
begin
  Result := ResultSet.CompareRows({%H-}Integer(Item1), {%H-}Integer(Item2),
    FSortedFieldIndices, FCompareFuncs);
end;

{**
   Sets a new dataset properties.
   @param Value a dataset properties.
}
procedure TZAbstractRODataset.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
end;

{**
  Checks if dataset can execute SQL queries?
  @returns <code>True</code> if the query can execute SQL.
}
function TZAbstractRODataset.PSIsSQLBased: Boolean;
begin
  Result := True;
end;

{$IFDEF WITH_IPROVIDER}
{**
  Starts a new transaction.
}
procedure TZAbstractRODataset.PSStartTransaction;
begin
  if Assigned(FConnection) and not FConnection.AutoCommit then
  begin
    if not FConnection.Connected then
      FConnection.Connect;
    FConnection.StartTransaction;
  end;
end;

{**
  Completes previously started transaction.
  @param Commit a commit transaction flag.
}
procedure TZAbstractRODataset.PSEndTransaction(Commit: Boolean);
begin
  if Assigned(FConnection) and FConnection.Connected
    and not FConnection.AutoCommit then
  begin
      if Commit then
         FConnection.Commit
      else
         FConnection.Rollback;
  end;
end;

{**
  Checks if this query is in transaction mode.
  @returns <code>True</code> if query in transaction.
}
function TZAbstractRODataset.PSInTransaction: Boolean;
begin
  Result := Assigned(FConnection) and FConnection.Connected
    and (FConnection.TransactIsolationLevel <> tiNone)
    and not FConnection.AutoCommit;
end;

{**
  Returns a string quote character.
  @retuns a quote character.
}
{$IFDEF WITH_IPROVIDERWIDE}
function TZAbstractRODataset.PSGetQuoteCharW: WideString;
{$ELSE}
function TZAbstractRODataset.PSGetQuoteChar: string;
{$ENDIF}
begin
  if Assigned(FConnection) then
  begin
    if not FConnection.Connected then
      FConnection.Connect;
    Result := FConnection.DbcConnection.GetMetadata.GetDatabaseInfo.GetIdentifierQuoteString;
    if Length(Result) > 1 then
      Result := Copy(Result, 1, 1);
  end
  else
    Result := '"';
end;

{**
  Checks if dataset can execute any commands?
  @returns <code>True</code> if the query can execute any commands.
}
function TZAbstractRODataset.PSIsSQLSupported: Boolean;
begin
  Result := True;
end;

{**
  Resets this dataset.
}
procedure TZAbstractRODataset.PSReset;
begin
  inherited PSReset;
  if Active then
  begin
    Refresh;
    First;
  end;
end;

{**
  Execute statement a SQL query.
}
procedure TZAbstractRODataset.PSExecute;
begin
  ExecSQL;
end;

{**
  Gets query parameters.
  @returns parameters of this query.
}
function TZAbstractRODataset.PSGetParams: TParams;
begin
  Result := Params;
end;

{**
  Set new query parameters
  @param AParams new parameters to set into this query.
}
procedure TZAbstractRODataset.PSSetParams(AParams: TParams);
begin
  if AParams.Count > 0 then
    Params.Assign(AParams);
end;

{**
  Sets a command text for this query to execute.
  @param CommandText a command text for this query.
}

{$IFDEF WITH_IPROVIDERWIDE}
procedure TZAbstractRODataset.PSSetCommandText(const CommandText: string);
begin
  SQL.Text := CommandText;
end;

procedure TZAbstractRODataset.PSSetCommandText(const CommandText: WideString);
{$ELSE}
procedure TZAbstractRODataset.PSSetCommandText(const CommandText: string);
{$ENDIF}
begin
  SQL.Text := CommandText;
end;

{**
  Updates a record in the specified dataset.
  @param UpdateKind a type of the update.
  @param Delta a dataset with updates.
}
function TZAbstractRODataset.PSUpdateRecord(UpdateKind: TUpdateKind;
  Delta: TDataSet): Boolean;
begin
  Result := False;
end;

{**
  Generates an EUpdateError object based on another exception object.
  @param E occured exception.
  @param Prev a previous update error.
  @returns a new created update error.
}
function TZAbstractRODataset.PSGetUpdateException(E: Exception;
  Prev: EUpdateError): EUpdateError;
var
  PrevErrorCode: Integer;
begin
  if E is EZSQLException then
  begin
    if Assigned(Prev) then
      PrevErrorCode := Prev.ErrorCode
    else
      PrevErrorCode := 0;

    Result := EUpdateError.Create(E.Message, '',
      EZSQLException(E).ErrorCode, PrevErrorCode, E);
  end
  else
    Result := EUpdateError.Create(E.Message, '', -1, -1, E);
end;

{**
  Gets a table name if table is only one in the SELECT SQL statement.
  @returns a table name or an empty string is SQL query is complex SELECT
    or not SELECT statement.
}
{$IFDEF WITH_IPROVIDERWIDE}
function TZAbstractRODataset.PSGetTableNameW: WideString;
{$ELSE}
function TZAbstractRODataset.PSGetTableName: string;
{$ENDIF}
var
  Driver: IZDriver;
  Tokenizer: IZTokenizer;
  StatementAnalyser: IZStatementAnalyser;
  SelectSchema: IZSelectSchema;
begin
  Result := '';
  if FConnection <> nil then
  begin
    Driver := FConnection.DbcDriver;
    Tokenizer := Driver.GetTokenizer;
    StatementAnalyser := Driver.GetStatementAnalyser;
    SelectSchema := StatementAnalyser.DefineSelectSchemaFromQuery(
      Tokenizer, SQL.Text);
    if Assigned(SelectSchema) and (SelectSchema.TableCount = 1) then
      Result := SelectSchema.Tables[0].FullName;
  end;
end;

{**
  Defines a list of query primary key fields.
  @returns a semicolon delimited list of query key fields.
}
// Silvio Clecio
{$IFDEF WITH_IPROVIDERWIDE}
function TZAbstractRODataset.PSGetKeyFieldsW: WideString;
begin
  Result := inherited PSGetKeyFieldsW;
end;
{$ELSE}
function TZAbstractRODataset.PSGetKeyFields: string;
begin
  Result := inherited PSGetKeyFields;
end;
{$ENDIF}

{**
  Executes a SQL statement with parameters.
  @param ASQL a SQL statement with parameters defined with question marks.
  @param AParams a collection of statement parameters.
  @param ResultSet a supplied result set reference (just ignored).
  @returns a number of updated rows.
}

{$IFDEF WITH_IPROVIDERWIDE}
function TZAbstractRODataset.PSExecuteStatement(const ASQL: WideString; AParams: TParams;
  ResultSet: Pointer = nil): Integer;
{$ELSE}
function TZAbstractRODataset.PSExecuteStatement(const ASQL: string;
  AParams: TParams; {$IFDEF WITH_IProviderSupportNG}var ResultSet: TDataSet
      {$ELSE}ResultSet: Pointer = nil{$ENDIF}): Integer;
{$ENDIF}
var
  I: Integer;
  Statement: IZPreparedStatement;
  ParamValue: TParam;
begin
  if Assigned(FConnection) then
  begin
    if not FConnection.Connected then
      FConnection.Connect;
    Statement := FConnection.DbcConnection.PrepareStatement(ASQL);
    if (AParams <> nil) and (AParams.Count > 0) then
      for I := 0 to AParams.Count - 1 do
      begin
        ParamValue := AParams[I];
        SetStatementParam(I{$IFNDEF GENERIC_INDEX}+1{$ENDIF}, Statement, ParamValue);
      end;
    Result := Statement.ExecuteUpdatePrepared;
  end
  else
    Result := 0;
end;

{$ENDIF}

// NB: FPC has TField.FieldDef property
procedure TZAbstractRODataset.CheckFieldCompatibility(Field: TField; AFieldDef: TFieldDef);
const
  {EH: hint all commented types are the fields the RowAccessor can't handle -> avoid stack killing moves in Get/SetFieldData()
  this Error trapping is made for User-added fields like calulateds ....}
  BaseFieldTypes: array[TFieldType] of TFieldType = (
    //generic TFieldTypes of FPC and Delphi(since D7, of course):
    ftUnknown, ftString, ftSmallint, ftInteger, ftWord, // 0..4
    ftBoolean, ftFloat, ftCurrency, ftFloat{ftBCD}, ftDate,  ftTime, ftDateTime, // 5..11
    ftBytes, ftBytes{ftVarBytes}, ftInteger{ftAutoInc}, ftBlob, ftMemo, ftBlob{ftGraphic}, ftMemo{ftFmtMemo}, // 12..18
    ftBlob{ftParadoxOle}, ftBlob{ftDBaseOle}, ftBlob{ftTypedBinary}, ftUnknown{ftCursor}, ftString{ftFixedChar}, ftWideString, // 19..24
    ftLargeint, ftUnknown{ftADT}, ftUnknown{ftArray}, ftUnknown{ftReference}, ftDataSet, ftBlob{ftOraBlob}, ftMemo{ftOraClob}, // 25..31
    ftUnknown{ftVariant}, ftUnknown{ftInterface}, ftUnknown{ftIDispatch}, ftGuid, ftTimeStamp, ftFloat{ftFMTBcd} // 32..37
{$IFDEF FPC} //addition types for FPC
    , ftWideString{ftFixedWideChar}, ftWideMemo // 38..39
{$ELSE !FPC}
{$IF CompilerVersion >= 18} //additional Types since D2006 and D2007
    , ftWideString{ftFixedWideChar}, ftWideMemo, ftDateTime{ftOraTimeStamp}, ftDateTime{ftOraInterval} // 38..41
{$IF CompilerVersion >= 20} //additional Types since D2009
    , ftLongWord, ftShortint, ftByte, ftExtended, ftUnknown{ftConnection}, ftUnknown{ftParams}, ftBlob{ftStream} //42..48
{$IF CompilerVersion >= 21} //additional Types since D2010
    , ftDateTime{ftTimeStampOffset}, ftUnknown{ftObject}, ftSingle //49..51
{$IFEND CompilerVersion >= 21}
{$IFEND CompilerVersion >= 20}
{$IFEND CompilerVersion >= 18}
{$ENDIF FPC}
  );
  CheckTypeSizes = [ftBytes, ftVarBytes, ftBCD, ftReference];
begin
  with Field do
  begin
    if (BaseFieldTypes[DataType] <> BaseFieldTypes[AFieldDef.DataType]) then
      DatabaseErrorFmt(SFieldTypeMismatch, [DisplayName,
        FieldTypeNames[DataType], FieldTypeNames[AFieldDef.DataType]], Self);
    if (DataType in CheckTypeSizes) and (Size <> AFieldDef.Size) then
        DatabaseErrorFmt(SFieldSizeMismatch, [DisplayName, Size,
          AFieldDef.Size], Self);
  end;
end;

{$IFDEF WITH_IPROVIDERSUPPORT_GUID}
type
  IProviderSupportActual = {$IF DECLARED(IProviderSupportNG)}IProviderSupportNG{$ELSE} IProviderSupport {$IFEND};
{$ENDIF}

procedure TZAbstractRODataset.CreateFields;
var
  I: Integer;

  procedure SetKeyFields;
  var
    Pos, j: Integer;
    KeyFields, FieldName: string;
    {$IFDEF WITH_IPROVIDERSUPPORT_GUID}
    PS: IProviderSupportActual;
    {$ENDIF}
  begin
    {$IFDEF WITH_IPROVIDERSUPPORT_GUID}
    if Supports(self, IProviderSupportActual, PS) then
      KeyFields := PS.PSGetKeyFields
    else
      KeyFields := IProviderSupportActual(Self).PSGetKeyFields;
    {$ELSE}
    KeyFields := self.PSGetKeyFields;
    {$ENDIF}
    Pos := 1;
    while Pos <= Length(KeyFields) do
    begin
      FieldName := ExtractFieldName(KeyFields, Pos);
      for j := 0 to FieldCount - 1 do
        if AnsiCompareText(FieldName, Fields[j].FieldName) = 0 then
        begin
          Fields[j].ProviderFlags := Fields[j].ProviderFlags + [pfInKey];
          break;
        end;
    end;
  end;

begin
  if FUseZFields then
  begin
    if ObjectView then
    begin
      for I := 0 to FieldDefs.Count - 1 do
        with FieldDefs[I] do
          if (DataType <> ftUnknown) and
            not ((faHiddenCol in Attributes) and not FIeldDefs.HiddenFields) then
            CreateField(Self);
    end else
    begin
      for I := 0 to FieldDefList.Count - 1 do
        with TZFieldDef(FieldDefList[I]) do
          if (DataType <> ftUnknown) and not (DataType in ObjectFieldTypes) and
            not ((faHiddenCol in Attributes) and not FieldDefs.HiddenFields) then
            CreateField(Self, nil, FieldDefList.Strings[I]);
    end;
    SetKeyFields;
  end
  else inherited CreateFields;
end;

{**
  Reset the calculated (includes fkLookup) fields
  @param Buffer
}
procedure TZAbstractRODataset.ClearCalcFields(Buffer: TRecordBuffer);
var
  Index: Integer;
begin
  RowAccessor.RowBuffer := PZRowBuffer(Buffer);
  for Index := 1 to Fields.Count do
    if (Fields[Index-1].FieldKind in [fkCalculated, fkLookup]) then
      RowAccessor.SetNull(DefineFieldindex(FFieldsLookupTable,Fields[Index-1]));
end;

{=======================bangfauzan addition========================}
function TZAbstractRODataset.GetSortType: TSortType;
var
  AscCount, DescCount: Integer;
  s, Fragment: String;
begin
  {pawelsel modification}
  AscCount := 0;
  DescCount := 0;
  s := UpperCase(ReplaceChar(';', ',', FIndexFieldNames));
  while s <> '' do
  begin
    BreakString(s, ',', Fragment, s);
    if ZFastCode.Pos(' DESC', Fragment) > 0 then
      Inc(DescCount)
    else
      Inc(AscCount);
  end;
  if (DescCount > 0) and (AscCount > 0) then
    Result := stIgnored
  else if (DescCount > 0) then
    Result := stDescending
  else
    Result := stAscending;
end;

procedure TZAbstractRODataset.SetSortType(Value: TSortType);
begin
  if FSortType <> Value then
  begin
    FSortType := Value;
    if (FSortType <> stIgnored) then
    begin {pawelsel modification}
      FSortedFields:=StringReplace(FSortedFields,' Desc','',[rfReplaceAll,rfIgnoreCase]);
      FSortedFields:=StringReplace(FSortedFields,' Asc','',[rfReplaceAll,rfIgnoreCase]);
    end;
    FIndexFieldNames:=GetIndexFieldNames;
    if Active then
      if (FSortedFields = '') then
        Self.InternalRefresh
      else
        InternalSort;
  end;
end;

function TZAbstractRODataset.GetIndexFieldNames : String;
begin
  Result:=FSortedFields;
  if Result <> '' then
  begin {pawelsel modification}
    if FSortType = stAscending then
    begin
       Result:=StringReplace(Result,';',' Asc;',[rfReplaceAll]);
       Result:=StringReplace(Result,',',' Asc,',[rfReplaceAll]);
       Result:=Result+' Asc';
    end;
    if FSortType = stDescending then
    begin
       Result:=StringReplace(Result,';',' Desc;',[rfReplaceAll]);
       Result:=StringReplace(Result,',',' Desc,',[rfReplaceAll]);
       Result:=Result+' Desc';
    end;
  end;
end;

procedure TZAbstractRODataset.SetIndexFieldNames(const Value: String);
var aValue: string;
begin
  aValue:=Trim(Value);
  {pawelsel modification}
  aValue:=RemoveChar('[', aValue);
  aValue:=RemoveChar(']', aValue);

  if FIndexFieldNames <> aValue then
  begin
     FIndexFieldNames := aValue;
     FSortType:=GetSortType;
     if (FSortType <> stIgnored) then
     begin {pawelsel modification}
        aValue:=StringReplace(aValue,' Desc','',[rfReplaceAll,rfIgnoreCase]);
        aValue:=StringReplace(aValue,' Asc','',[rfReplaceAll,rfIgnoreCase]);
     end;
     FSortedFields:=aValue;
  end;

  {Perform sorting}
  if Active then
     if (FSortedFields = '') then
        Self.InternalRefresh
     else
        InternalSort;
end;

{====================end of bangfauzan addition====================}

{ TZField }

constructor TZField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF WITH_TVALUEBUFFER}
  SetLength(FValueBuffer, GetDataSize);
  {$ELSE}
  GetMem(FValueBuffer, GetDataSize);
  {$ENDIF}
end;

destructor TZField.Destroy;
begin
  {$IFDEF WITH_TVALUEBUFFER}
  SetLength(FValueBuffer, 0);
  {$ELSE}
  FreeMem(FValueBuffer, GetDataSize);
  {$ENDIF}
  inherited Destroy;
end;

procedure TZField.Validate(Buffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF});
begin
  if Assigned(OnValidate) then
  begin
    { Use the already assigned FValueBuffer if set }
    if FValueBuffer = nil then
      FValueBuffer := Buffer;
    FValidating := True;
    try
      OnValidate(Self);
    finally
      FValidating := False;
    end;
  end;
end;

function TZField.GetActiveRowBuffer: Boolean;
begin
  if DataSet = nil then
    DatabaseErrorFmt({$IFDEF FPC}SNoDataset{$ELSE}SDataSetMissing{$ENDIF}, [DisplayName]);
  Result := (DataSet as TZAbstractRODataset).GetActiveBuffer(FRowBuffer);
  if Result then
    (DataSet as TZAbstractRODataset).FRowAccessor.RowBuffer := FRowBuffer;
end;

function TZField.IsFieldEditable: Boolean;
begin
  if ReadOnly and (FieldKind <> fkLookup) and not (DataSet.State in
    [dsSetKey, dsCalcFields, dsFilter, dsBlockRead, dsInternalCalc, dsOpening]) then
      DatabaseErrorFmt(SFieldReadOnly, [DisplayName]);
  if not (DataSet.State in dsWriteModes) then
    DatabaseError(SNotEditing, DataSet);
  Result := GetActiveRowBuffer;
end;

{$IFNDEF WITH_FIELD_VALIDATELOOKUPINFO}
procedure TZField.ValidateLookupInfo(All: Boolean);
begin
  if (All and ((LookupDataSet = nil) or (LookupKeyFields = '') or
     (LookupResultField = ''))) or (KeyFields = '') then
    DatabaseErrorFmt(SLookupInfoError, [DisplayName]);
  { TODO : Check FOwnedFields/FFields (private section) }
  //FFields.CheckFieldNames(KeyFields);
  if All then
  begin
    LookupDataSet.Open;
    LookupDataSet.Fields.CheckFieldNames(LookupKeyFields);
    LookupDataSet.FieldByName(LookupResultField);
  end;
end;
{$ENDIF}

{$IFNDEF WITH_TFIELD_PARENTFIELD}
procedure TZField.SetParentField(AField: TObjectField);
begin
  if AField <> FParentField then
  begin
    if DataSet <> nil then (DataSet as TZAbstractRODataset).CheckInactive;
    if AField <> nil then
    begin
      if AField.DataSet <> nil then (AField.DataSet as TZAbstractRODataset).CheckInactive;
      AField.Fields.CheckFieldName(FieldName);
      AField.Fields.Add(Self);
      if DataSet <> nil then DataSet.Fields.Remove(Self);
      DataSet := AField.DataSet;
    end
    else if DataSet <> nil then DataSet.Fields.Add(Self);
    if FParentField <> nil then FParentField.Fields.Remove(Self);
    FParentField := AField;
  end;
end;
{$ENDIF}

{$IFNDEF WITH_TFIELD_FREEBUFFERS}
procedure TZField.FreeBuffers;
begin
end;
{$ENDIF}

function TZField.GetValidationBuffer: {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ELSE}Pointer{$ENDIF};
var IsNull: Boolean;
begin
  {Active RowBuffer is already set!}
  Result := {$IFDEF WITH_TVALUEBUFFER}TValueBuffer{$ENDIF}((DataSet as TZAbstractRODataset).RowAccessor.GetColumnData(FFieldIndex, IsNull));
end;

function TZField.GetAsBoolean: Boolean;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetBoolean(FFieldIndex, IsNull)
  else
    Result := False;
end;

function TZField.GetAsDateTime: TDateTime;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetTimestamp(FFieldIndex, IsNull)
  else
    Result := 0;
end;

function TZField.GetAsDate: TDateTime;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetTimestamp(FFieldIndex, IsNull)
  else
    Result := 0;
end;

function TZField.GetAsTime: TDateTime;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetTimestamp(FFieldIndex, IsNull)
  else
    Result := 0;
end;

function TZField.GetAsCurrency: Currency;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetBigDecimal(FFieldIndex, IsNull)
  else
    Result := 0;
end;

function TZField.GetAsBCD: TBcd;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    TryStrToBcd((DataSet as TZAbstractRODataset).FRowAccessor.GetString(FFieldIndex, IsNull), Result{%H-})
  else
    Result := NullBcd;
end;


function TZField.GetAsSingle: Single;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetFloat(FFieldIndex, IsNull)
  else
    Result := 0.0;
end;

function TZField.GetAsFloat: Double;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetDouble(FFieldIndex, IsNull)
  else
    Result := 0.0;
end;

function TZField.GetAsExtended: Extended;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetBigDecimal(FFieldIndex, IsNull)
  else
    Result := 0.0;
end;

{ signed integers }
function TZField.GetAsShortInt: ShortInt;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetShort(FFieldIndex, IsNull)
  else
    Result := 0;
end;

function TZField.GetAsSmallInt: SmallInt;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetSmall(FFieldIndex, IsNull)
  else
    Result := 0;
end;

function TZField.GetAsInteger: Longint;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetInt(FFieldIndex, IsNull)
  else
    Result := 0;
end;

function TZField.GetAsLargeInt: Largeint;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetLong(FFieldIndex, IsNull)
  else
    Result := 0;
end;

{ unsigned integers }
function TZField.GetAsByte: Byte;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetByte(FFieldIndex, IsNull)
  else
    Result := 0;
end;

function TZField.GetAsWord: Word;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetInt(FFieldIndex, IsNull)
  else
    Result := 0;
end;

function TZField.GetAsLongWord: LongWord;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetUInt(FFieldIndex, IsNull)
  else
    Result := 0;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZField.GetAsUInt64: UInt64;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetLong(FFieldIndex, IsNull)
  else
    Result := 0;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

{ string types }
function TZField.GetAsString: string;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetString(FFieldIndex, IsNull)
  else
    Result := '';
end;

function TZField.GetAsWideString: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF};
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetUnicodeString(FFieldIndex, IsNull)
  else
    Result := '';
end;

{$IFNDEF NO_ANSISTRING}
function TZField.GetAsAnsiString: AnsiString;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetAnsiString(FFieldIndex, IsNull)
  else
    Result := '';
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
function TZField.GetAsUTF8String: UTF8String;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetUTF8String(FFieldIndex, IsNull)
  else
    Result := '';
end;
{$ENDIF}

function TZField.GetAsRawByteString: RawByteString;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetRawByteString(FFieldIndex, IsNull)
  else
    Result := '';
end;

function TZField.GetAsGuid: TGUID;
var IsNull: Boolean;
  Bytes: TBytes;
begin
  FillChar(Result{%H-}, SizeOf(Result), #0);
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
  begin
    Bytes := (DataSet as TZAbstractRODataset).FRowAccessor.GetBytes(FFieldIndex, IsNull);
    if not IsNull then
      Result := PGUID(Bytes)^;
  end;
end;

function TZField.GetAsBytes: TBytes;
var IsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.GetBytes(FFieldIndex, IsNull)
  else
    Result := nil;
end;

function TZField.GetAsVariant: Variant;
begin
  case  Self.DataType of
    ftUnknown: Result := Null;
    ftString: Result := {$IFNDEF NO_ANSISTRING}GetAsAnsiString{$ELSE}GetAsRawByteString{$ENDIF};
    ftSmallint: Result := GetAsSmallInt;
    ftInteger: Result := GetAsInteger;
    ftWord: Result := GetAsWord;
    ftBoolean: Result := GetAsBoolean;
    ftFloat: Result := GetAsFloat;
    ftCurrency: Result := GetAsCurrency;
    ftBCD: Result := GetAsCurrency;
    ftDate: Result := GetAsDate;
    ftTime: Result := GetAsTime;
    ftDateTime: Result := GetAsDateTime;
    ftBytes: Result := GetAsBytes;
    ftVarBytes: Result := GetAsBytes;
    ftAutoInc: Result := GetAsInteger;
    ftBlob: Result := GetAsBytes;
    ftMemo: Result := {$IFNDEF NO_ANSISTRING}GetAsAnsiString{$ELSE}GetAsRawByteString{$ENDIF};
    //ftGraphic: ;
    //ftFmtMemo: ;
    //ftParadoxOle: ;
    //ftDBaseOle: ;
    //ftTypedBinary: ;
    ftCursor: ;
    ftFixedChar: Result := {$IFNDEF NO_ANSISTRING}GetAsAnsiString{$ELSE}GetAsRawByteString{$ENDIF};
    ftWideString: Result := GetAsWideString;
    ftLargeint: Result := GetAsLargeInt;
    ftADT: ;
    ftArray: ;
    //ftReference: ;
    ftDataSet: ;
    //ftOraBlob: ;
    //ftOraClob: ;
    //ftVariant: ;
    //ftInterface: ;
    //ftIDispatch: ;
    ftGuid: Result := GetAsString;
    ftTimeStamp: ;
    ftFMTBcd: GetAsString;
    {$IFDEF WITH_FTFIXEDWIDECHAR}
    ftFixedWideChar: GetAsWideString;
    {$ENDIF}
    {$IFDEF WITH_FTWIDEMEMO}
    ftWideMemo: GetAsWideString;
    {$ENDIF}
    //ftOraTimeStamp: ;
    //ftOraInterval: ;
    {$IFDEF WITH_FTLONGWORD}
    ftLongWord: GetAsLongWord;
    {$ENDIF}
    {$IFDEF WITH_FTSHORTINT}
    ftShortint: GetAsShortInt;
    {$ENDIF}
    {$IFDEF WITH_FTBYTE}
    ftByte: GetAsByte;
    {$ENDIF}
    {$IFDEF WITH_FTEXTENDED}
    ftExtended: GetAsExtended;
    {$ENDIF}
    //ftConnection: ;
    //ftParams: ;
    //ftStream: ;
    //ftTimeStampOffset: ;
    //ftObject: ;
    {$IFDEF WITH_FTSINGLE}
    ftSingle: GetAsSingle;
    {$ENDIF}
  end;
end;

function TZField.GetIsNull: Boolean;
begin
  if GetActiveRowBuffer then //need this call to get active RowBuffer.
    Result := (DataSet as TZAbstractRODataset).FRowAccessor.IsNull(FFieldIndex)
  else
    Result := True;
end;

{$IFNDEF WITH_VIRTUAL_GETHASCONSTRAINTS}
function TZField.GetHasConstraints: Boolean;
begin
  Result := (CustomConstraint <> '') or (ImportedConstraint <> '') or
   (DefaultExpression <> '');
end;
{$ENDIF}

procedure TZField.SetAsBCD(const Value: TBcd);
begin
  if IsFieldEditable then
  begin
    if Self.FValidating then

    (DataSet as TZAbstractRODataset).FRowAccessor.SetString(FFieldIndex, BcdToStr(Value));
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsBoolean(Value: Boolean);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetBoolean(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

//procedure SetAsByteArray(const Value: Variant); virtual;
procedure TZField.SetAsDateTime(Value: TDateTime);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetTimestamp(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;
//procedure TZField.SetAsSQLTimeStamp(const Value: TSQLTimeStamp); virtual;
//procedure TZField.SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset); virtual;

{ decimal/floating values}
procedure TZField.SetAsCurrency(Value: Currency);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetBigDecimal(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsSingle(Value: Single);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetFloat(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsFloat(Value: Double);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetDouble(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsExtended(Value: Extended);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetBigDecimal(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{ signed integer values }
procedure TZField.SetAsShortInt(Value: ShortInt);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetShort(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsSmallInt(Value: SmallInt);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetSmall(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsInteger(Value: Longint);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetInt(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsLargeInt(Value: Largeint);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetLong(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{ unsigned integer values }
procedure TZField.SetAsByte(Value: Byte);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetByte(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsWord(Value: Word);
begin
  if IsFieldEditable then
  begin
    //reminder !!
    (DataSet as TZAbstractRODataset).FRowAccessor.SetWord(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsLongWord(Value: LongWord);
begin
  if IsFieldEditable then
  begin
    //reminder !!
    (DataSet as TZAbstractRODataset).FRowAccessor.SetUInt(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsUInt64(Value: UInt64);
begin
  if IsFieldEditable then
  begin
    //reminder !!
    (DataSet as TZAbstractRODataset).FRowAccessor.SetULong(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{ string values }
procedure TZField.SetAsString(const Value: string);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetString(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsWideString(const Value: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF});
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetUnicodeString(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{$IFNDEF NO_ANSISTRING}
procedure TZField.SetAsAnsiString(const Value: AnsiString);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetAnsiString(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
procedure TZField.SetAsUTF8String(const Value: UTF8String);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetUTF8String(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;
{$ENDIF}

procedure TZField.SetAsRawByteString(const Value: RawByteString);
begin
  if IsFieldEditable then
  begin
    (DataSet as TZAbstractRODataset).FRowAccessor.SetRawByteString(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsBytes(const Value: TBytes);
begin
  if IsFieldEditable then
  begin
    //reminder !!
    (DataSet as TZAbstractRODataset).FRowAccessor.SetBytes(FFieldIndex, Value);
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

procedure TZField.SetAsVariant(const Value: Variant);
begin
  if IsFieldEditable then
  begin
    case varType(Value) of
      varEmpty: (DataSet as TZAbstractRODataset).FRowAccessor.SetString(FFieldIndex, '');
      varNull: (DataSet as TZAbstractRODataset).FRowAccessor.SetNull(FFieldIndex);
      varSmallint:  (DataSet as TZAbstractRODataset).FRowAccessor.SetSmall(FFieldIndex, Value);
      varInteger:   (DataSet as TZAbstractRODataset).FRowAccessor.SetInt(FFieldIndex, Value);
      varSingle:    (DataSet as TZAbstractRODataset).FRowAccessor.SetFloat(FFieldIndex, Value);
      varDouble:    (DataSet as TZAbstractRODataset).FRowAccessor.SetDouble(FFieldIndex, Value);
      varCurrency:  (DataSet as TZAbstractRODataset).FRowAccessor.SetCurrency(FFieldIndex, Value);
      varDate:      (DataSet as TZAbstractRODataset).FRowAccessor.SetTimestamp(FFieldIndex, Value);
      varOleStr:    (DataSet as TZAbstractRODataset).FRowAccessor.SetUnicodeString(FFieldIndex, Value);
      //varDispatch:
      //varError:
      varBoolean:   (DataSet as TZAbstractRODataset).FRowAccessor.SetBoolean(FFieldIndex, Value);
      //varVariant:
      //varUnknown:
      varShortInt:  (DataSet as TZAbstractRODataset).FRowAccessor.SetShort(FFieldIndex, Value);
      varByte:      (DataSet as TZAbstractRODataset).FRowAccessor.SetByte(FFieldIndex, Value);
      varWord:      (DataSet as TZAbstractRODataset).FRowAccessor.SetWord(FFieldIndex, Value);
      varLongWord:  (DataSet as TZAbstractRODataset).FRowAccessor.SetUInt(FFieldIndex, Value);
      varInt64:     (DataSet as TZAbstractRODataset).FRowAccessor.SetLong(FFieldIndex, Value);
      {$IFDEF WITH_VARIANT_UINT64}
      varUInt64:    (DataSet as TZAbstractRODataset).FRowAccessor.SetULong(FFieldIndex, Value);
      {$ENDIF WITH_VARIANT_UINT64}
      //varStrArg:
      varString:    (DataSet as TZAbstractRODataset).FRowAccessor.SetString(FFieldIndex, Value);
      //varAny:
      {$IFDEF WITH_VARIANT_UNICODESTRING}
      varUString:   (DataSet as TZAbstractRODataset).FRowAccessor.SetUnicodeString(FFieldIndex, Value);
      {$ENDIF}
      //varTypeMask:
      //varArray:
      //varByRef:
    end;
    (DataSet as TZAbstractRODataset).DataEvent(deFieldChange, NativeInt(Self));
  end;
end;

{ TZNumericField }

constructor TZNumericField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Alignment := taRightJustify;
end;


function TZNumericField.ConvertSigned(const Value; const ValueType: TZSQLType): Int64;
begin
  case ValueType of
    stByte: Result := Byte(Value);
    stShort: Result := ShortInt(Value);
    stWord: Result := Word(Value);
    stSmall: Result := SmallInt(Value);
    stLongWord: Result := LongWord(Value);
    stInteger: Result := Integer(Value);
    stLong: Result := Int64(Value);
    stULong: Result := UInt64(Value);
    stString: Result := RawToInt64(RawByteString(Value));
    stUnicodeString: Result := UnicodeToInt64(ZWideString(Value));
    stBoolean: Result := Ord(Boolean(Value));
    stFloat: Result := Round(Single(Value));
    stDouble: Result := Round(Double(Value));
    stCurrency: Result := Round(Currency(Value));
    stBigDecimal: Result := Round(Extended(Value));
    stDate, stTime, stTimeStamp: Result := Round(TDateTime(Value));
    else
      raise EZSQLException.Create(Format(SConvertionIsNotPossible, [FFieldIndex, '','']));
  end;
end;

{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R-}{$IFEND}
function TZNumericField.ConvertUnSigned(const Value; const ValueType: TZSQLType): UInt64;
begin
  case ValueType of
    stByte: Result := Byte(Value);
    stShort: Result := ShortInt(Value);
    stWord: Result := Word(Value);
    stSmall: Result := SmallInt(Value);
    stLongWord: Result := LongWord(Value);
    stInteger: Result := Integer(Value);
    stLong: Result := Int64(Value);
    stString: Result := RawToUInt64(RawByteString(Value));
    stUnicodeString: Result := UnicodeToUInt64(ZWideString(Value));
    stBoolean: Result := Ord(Boolean(Value));
    stFloat: Result := Round(Single(Value));
    stDouble: Result := Round(Double(Value));
    stCurrency: Result := Round(Currency(Value));
    stBigDecimal: Result := Round(Extended(Value));
    stDate, stTime, stTimeStamp: Result := Round(TDateTime(Value));
    else
      raise EZSQLException.Create(Format(SConvertionIsNotPossible, [FFieldIndex, '','']));
  end;
end;
{$IF defined (RangeCheckEnabled) and defined(WITH_UINT64_C1118_ERROR)}{$R+}{$IFEND}

function TZNumericField.ConvertExtended(const Value; const ValueType: TZSQLType): Extended;
begin
  case ValueType of
    stByte: Result := Byte(Value);
    stShort: Result := ShortInt(Value);
    stWord: Result := Word(Value);
    stSmall: Result := SmallInt(Value);
    stLongWord: Result := LongWord(Value);
    stInteger: Result := Integer(Value);
    stLong: Result := Int64(Value);
    stString: Result := RawToUInt64(RawByteString(Value));
    stUnicodeString: Result := UnicodeToUInt64(ZWideString(Value));
    stBoolean: Result := Ord(Boolean(Value));
    stFloat: Result := Single(Value);
    stDouble: Result := Double(Value);
    stCurrency: Result := Currency(Value);
    stBigDecimal: Result := Extended(Value);
    stDate, stTime, stTimeStamp: Result := TDateTime(Value);
    else
      raise EZSQLException.Create(Format(SConvertionIsNotPossible, [FFieldIndex, '','']));
  end;
end;

procedure TZNumericField.SetAsBCD(const Value: TBcd);
var tmp: String;
begin
  if FRangeCheck then
  begin
    tmp := BcdToStr(Value);
    CheckRange(tmp, stString);
  end
  else
    inherited SetAsBCD(Value);
end;

procedure TZNumericField.SetAsBoolean(Value: Boolean);
begin
  if FRangeCheck then
    CheckRange(Value, stBoolean)
  else
    inherited SetAsBoolean(Value);
end;

//TZNumericField.procedure SetAsByteArray(const Value: Variant); virtual;
procedure TZNumericField.SetAsDateTime(Value: TDateTime);
begin
  if FRangeCheck then
    CheckRange(Value, stTimeStamp)
  else
    inherited SetAsDateTime(Value);
end;

//procedure TZNumericField.SetAsSQLTimeStamp(const Value: TSQLTimeStamp); virtual;
//procedure TZNumericField.SetAsSQLTimeStampOffset(const Value: TSQLTimeStampOffset); virtual;
{ decimal/floating values}
procedure TZNumericField.SetAsCurrency(Value: Currency);
begin
  if FRangeCheck then
    CheckRange(Value, stCurrency)
  else
    inherited SetAsCurrency(Value);
end;

procedure TZNumericField.SetAsSingle(Value: Single);
begin
  if FRangeCheck then
    CheckRange(Value, stFloat)
  else
    inherited SetAsSingle(Value);
end;

procedure TZNumericField.SetAsFloat(Value: Double);
begin
  if FRangeCheck then
    CheckRange(Value, stDouble)
  else
    inherited SetAsFloat(Value);
end;

procedure TZNumericField.SetAsExtended(Value: Extended);
begin
  if FRangeCheck then
    CheckRange(Value, stBigDecimal)
  else
    inherited SetAsExtended(Value);
end;

{ signed integer values }
procedure TZNumericField.SetAsShortInt(Value: ShortInt);
begin
  if FRangeCheck then
    CheckRange(Value, stShort)
  else
    inherited SetAsShortInt(Value);
end;

procedure TZNumericField.SetAsSmallInt(Value: SmallInt);
begin
  if FRangeCheck then
    CheckRange(Value, stSmall)
  else
    inherited SetAsSmallInt(Value);
end;

procedure TZNumericField.SetAsInteger(Value: Longint);
begin
  if FRangeCheck then
    CheckRange(Value, stInteger)
  else
    inherited SetAsInteger(Value);
end;

procedure TZNumericField.SetAsLargeInt(Value: Largeint);
begin
  if FRangeCheck then
    CheckRange(Value, stLong)
  else
    inherited SetAsLargeInt(Value);
end;

{ unsigned integer values }
procedure TZNumericField.SetAsByte(Value: Byte);
begin
  if FRangeCheck then
    CheckRange(Value, stByte)
  else
    inherited SetAsByte(Value);
end;

procedure TZNumericField.SetAsWord(Value: Word);
begin
  if FRangeCheck then
    CheckRange(Value, stWord)
  else
    inherited SetAsWord(Value);
end;

procedure TZNumericField.SetAsLongWord(Value: LongWord);
begin
  if FRangeCheck then
    CheckRange(Value, stLongWord)
  else
    inherited SetAsLongWord(Value);
end;

procedure TZNumericField.SetAsUInt64(Value: UInt64);
begin
  if FRangeCheck then
    CheckRange(Value, stULong)
  else
    inherited SetAsUInt64(Value);
end;

{ string values }
procedure TZNumericField.SetAsString(const Value: string);
begin
  if FRangeCheck then
    CheckRange(Value, {$IFDEF UNICODE}stUnicodeString{$ELSE}stString{$ENDIF})
  else
    inherited SetAsString(Value);
end;

procedure TZNumericField.SetAsWideString(const Value: {$IFDEF UNICODE}UnicodeString{$ELSE}WideString{$ENDIF});
begin
  if FRangeCheck then
    CheckRange(Value, stUnicodeString)
  else
    inherited SetAsWideString(Value);
end;

{$IFNDEF NO_ANSISTRING}
procedure TZNumericField.SetAsAnsiString(const Value: AnsiString);
begin
  if FRangeCheck then
    CheckRange(Value, stString)
  else
    inherited SetAsAnsiString(Value);
end;
{$ENDIF}

{$IFNDEF NO_UTF8STRING}
procedure TZNumericField.SetAsUTF8String(const Value: UTF8String);
begin
  if FRangeCheck then
    CheckRange(Value, stString)
  else
    inherited SetAsUTF8String(Value);
end;
{$ENDIF}

procedure TZNumericField.SetAsRawByteString(const Value: RawByteString);
begin
  if FRangeCheck then
    CheckRange(Value, stString)
  else
    inherited SetAsRawByteString(Value);
end;

procedure TZNumericField.RangeError(Value, Min, Max: Extended);
begin
  DatabaseErrorFmt({$IFDEF FPC}SRangeError{$ELSE}SFieldRangeError{$ENDIF}, [Value, DisplayName, Min, Max]);
end;

procedure TZNumericField.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
  begin
    FDisplayFormat := Value;
    PropertyChanged(False);
  end;
end;

procedure TZNumericField.SetEditFormat(const Value: string);
begin
  if FEditFormat <> Value then
  begin
    FEditFormat := Value;
    PropertyChanged(False);
  end;
end;

{ TZByteField }

procedure TZByteField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: UInt64;
begin
  ConvertedValue := ConvertUnSigned(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsByte(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

procedure TZByteField.SetMaxValue(Value: Byte);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZByteField.SetMinValue(Value: Byte);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

function TZByteField.GetDataSize: Integer;
begin
  Result := SizeOf({$IFDEF WITH_FTBYTE}Byte{$ELSE}Word{$ENDIF});
end;

constructor TZByteField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType({$IFDEF WITH_FTBYTE}ftByte{$ELSE}ftWord{$ENDIF});
  ValidChars := ['+', '0'..'9'];
end;

{ TZShortIntField }

procedure TZShortIntField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: Int64;
begin
  ConvertedValue := ConvertSigned(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsShortInt(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

procedure TZShortIntField.SetMaxValue(Value: ShortInt);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZShortIntField.SetMinValue(Value: ShortInt);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

function TZShortIntField.GetDataSize: Integer;
begin
  Result := SizeOf({$IFDEF WITH_FTSHORTINT}ShortInt{$ELSE}SmallInt{$ENDIF});
end;

constructor TZShortIntField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType({$IFDEF WITH_FTSHORTINT}ftShortInt{$ELSE}ftSmallInt{$ENDIF});
  ValidChars := ['+', '-', '0'..'9'];
end;

{ TZWordField }

procedure TZWordField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: UInt64;
begin
  ConvertedValue := ConvertUnSigned(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsWord(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

procedure TZWordField.SetMaxValue(Value: Word);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZWordField.SetMinValue(Value: Word);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

function TZWordField.GetDataSize: Integer;
begin
  Result := 2;
end;

constructor TZWordField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftWord);
  ValidChars := ['+', '0'..'9'];
end;

{ TZSmallIntField }

procedure TZSmallIntField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: Int64;
begin
  ConvertedValue := ConvertSigned(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsSmallInt(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

procedure TZSmallIntField.SetMaxValue(Value: SmallInt);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZSmallIntField.SetMinValue(Value: SmallInt);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

function TZSmallIntField.GetDataSize: Integer;
begin
  Result := SizeOf(SmallInt);
end;

constructor TZSmallIntField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftSmallInt);
  ValidChars := ['+', '-', '0'..'9'];
end;

{ TZIntegerField }

procedure TZIntegerField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: Int64;
begin
  ConvertedValue := ConvertSigned(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsInteger(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

procedure TZIntegerField.SetMaxValue(Value: Longint);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZIntegerField.SetMinValue(Value: Longint);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

function TZIntegerField.GetDataSize: Integer;
begin
  Result := 4;
end;

constructor TZIntegerField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftInteger);
  ValidChars := ['+', '-', '0'..'9'];
end;

{ TZLongWordField }

procedure TZLongWordField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: UInt64;
begin
  ConvertedValue := ConvertUnSigned(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsLongWord(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

procedure TZLongWordField.SetMaxValue(Value: LongWord);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZLongWordField.SetMinValue(Value: LongWord);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

function TZLongWordField.GetDataSize: Integer;
begin
  Result := SizeOf({$IFDEF WITH_FTLONGWORD}LongWord{$ELSE}Int64{$ENDIF});
end;

constructor TZLongWordField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType({$IFDEF WITH_FTLONGWORD}ftLongWord{$ELSE}ftLargeInt{$ENDIF});
  ValidChars := ['+', '0'..'9'];
end;

{ TZInt64Field }

procedure TZInt64Field.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: Int64;
begin
  ConvertedValue := ConvertSigned(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsLargeInt(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

procedure TZInt64Field.SetMaxValue(Value: Int64);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZInt64Field.SetMinValue(Value: Int64);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

function TZInt64Field.GetDataSize: Integer;
begin
  Result := 8;//SizeOf(Int64)
end;

constructor TZInt64Field.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftLargeint);
  ValidChars := ['+', '-', '0'..'9']
end;

{ TZUInt64Field }

procedure TZUInt64Field.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: UInt64;
begin
  ConvertedValue := ConvertUnSigned(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsUInt64(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

procedure TZUInt64Field.SetMaxValue(Value: UInt64);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZUInt64Field.SetMinValue(Value: UInt64);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

function TZUInt64Field.GetDataSize: Integer;
begin
  Result := 8;//SizeOf(UInt64)
end;

constructor TZUInt64Field.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftLargeint);
  ValidChars := ['+', '0'..'9']
end;

{ TZStringField }

constructor TZStringField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftString);
end;

class procedure TZStringField.CheckTypeSize(Value: Integer);
begin
  if Value < 0 then
    DatabaseError(SInvalidFieldSize);
end;

function TZStringField.GetDataSize: Integer;
begin
  Result := Size +1;
end;

{ TZWideStringField }
constructor TZWideStringField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftWideString);
end;

function TZWideStringField.GetDataSize: Integer;
begin
  Result := (Size +1) shl 1;
end;

{ TZFloatField }

constructor TZFloatField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ValidChars := [{$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}DecimalSeparator, '+', '-', '0'..'9', 'E', 'e'];
end;

procedure TZFloatField.SetCurrency(Value: Boolean);
begin
  if FCurrency <> Value then
  begin
    FCurrency := Value;
    PropertyChanged(False);
  end;
end;

procedure TZFloatField.SetPrecision(Value: Integer);
begin
  if FPrecision <> Value then
  begin
    FPrecision := Value;
    PropertyChanged(False);
  end;
end;

procedure TZFloatField.InternalGetText(Value: Extended; out Text: string; DisplayText: Boolean);
  { Note: FloatToStrF / FormatFloat that are called below use Extended type anyway.
    So there's no sense in personal GetText copies for every float type. }
var
  Format: TFloatFormat;
  FmtStr: string;
  Digits: Integer;
begin
  if DisplayText or (FEditFormat = '')
    then FmtStr := FDisplayFormat
    else FmtStr := FEditFormat;

  if FmtStr = '' then
  begin
    if FCurrency then
    begin
      if DisplayText
        then Format := ffCurrency
        else Format := ffFixed;
      Digits := {$IFDEF WITH_FORMATSETTINGS}FormatSettings.{$ENDIF}CurrencyDecimals;
    end
    else
    begin
      Format := ffGeneral;
      Digits := 0;
    end;
    Text := FloatToStrF(Value, Format, FPrecision, Digits {$IFDEF WITH_FORMATSETTINGS}, FormatSettings{$ENDIF});
  end
  else
    Text := FormatFloat(FmtStr, Value {$IFDEF WITH_FORMATSETTINGS}, FormatSettings{$ENDIF});
end;

{ TZSingleField }

procedure TZSingleField.SetMaxValue(Value: Single);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZSingleField.SetMinValue(Value: Single);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

procedure TZSingleField.SetPrecision(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if Value > 7 then Value := 7;
  inherited SetPrecision(Value);
end;

procedure TZSingleField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: Extended;
begin
  ConvertedValue := ConvertExtended(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsSingle(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

function TZSingleField.GetDataSize: Integer;
begin
  Result := 4;//SizeOf(Single);
end;

procedure TZSingleField.GetText(var Text: string; DisplayText: Boolean);
begin
  if IsNull then
    Text := ''
  else
    InternalGetText(GetAsSingle, Text, DisplayText);
end;

constructor TZSingleField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType({$IFDEF WITH_FTSINGLE}ftSingle{$ELSE}ftFloat{$ENDIF});
  FPrecision := 7;
end;

{ TZDoubleField }

procedure TZDoubleField.SetMaxValue(Value: Double);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZDoubleField.SetMinValue(Value: Double);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

procedure TZDoubleField.SetPrecision(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if Value > 15 then Value := 15;
  inherited SetPrecision(Value);
end;

procedure TZDoubleField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: Extended;
begin
  ConvertedValue := ConvertExtended(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsFloat(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

function TZDoubleField.GetDataSize: Integer;
begin
  Result := SizeOf(Double);
end;

procedure TZDoubleField.GetText(var Text: string; DisplayText: Boolean);
begin
  if IsNull then
    Text := ''
  else
    InternalGetText(GetAsFloat, Text, DisplayText);
end;

constructor TZDoubleField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftFloat);
  FPrecision := 15;
end;

{ TZCurrencyField }

procedure TZCurrencyField.SetMaxValue(Value: Currency);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZCurrencyField.SetMinValue(Value: Currency);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

procedure TZCurrencyField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: Extended;
begin
  ConvertedValue := ConvertExtended(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  //Let the IDE do the RangeCheck !
  {$R+}
  inherited SetAsCurrency(ConvertedValue);
  {$IFNDEF RangeCheckEnabled} {$R-} {$ENDIF}
end;

procedure TZCurrencyField.SetPrecision(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if Value > 15 then Value := 15;
  inherited SetPrecision(Value);
end;

function TZCurrencyField.GetDataSize: Integer;
begin
  Result := 8;//SizeOf(Currency);
end;

procedure TZCurrencyField.GetText(var Text: string; DisplayText: Boolean);
begin
  if IsNull then
    Text := ''
  else
    InternalGetText(GetAsCurrency, Text, DisplayText);
end;

constructor TZCurrencyField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftCurrency);
  FPrecision := 15;
end;

{ TZExtendedField }

procedure TZExtendedField.SetMaxValue(Value: Extended);
begin
  FRangeCheck := (Value = 0) and (FMaxValue = 0);
  FMaxValue := Value;
end;

procedure TZExtendedField.SetMinValue(Value: Extended);
begin
  FRangeCheck := (Value = 0) and (FMinValue = 0);
  FMinValue := Value;
end;

procedure TZExtendedField.CheckRange(const Value; const ValueType: TZSQLType);
var
  ConvertedValue: Extended;
begin
  ConvertedValue := ConvertExtended(Value, ValueType);

  if (ConvertedValue < FMinValue) or (ConvertedValue > FMaxValue) then
    RangeError(ConvertedValue, FMinValue, FMaxValue);
  inherited SetAsExtended(ConvertedValue);
end;

procedure TZExtendedField.SetPrecision(Value: Integer);
begin
  if Value < 2 then Value := 2;
  if Value > 19 then Value := 19;
  inherited SetPrecision(Value);
end;

function TZExtendedField.GetDataSize: Integer;
begin
  Result := {$IFDEF WITH_FTEXTENDED}10{$ELSE}8{$ENDIF};//SizeOf(Extended)/SizeOf(Double);
end;

procedure TZExtendedField.GetText(var Text: string; DisplayText: Boolean);
begin
  if IsNull then
    Text := ''
  else
    InternalGetText(GetAsExtended, Text, DisplayText);
end;

constructor TZExtendedField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType({$IFDEF WITH_FTEXTENDED}ftExtended{$ELSE}ftFloat{$ENDIF});
  FPrecision := 19;
end;

{ TZFieldDef }
{$IFNDEF TFIELDDEF_HAS_CHILDEFS}
function TZFieldDef.GetChildDefs: TFieldDefs;
begin
  //if FChildDefs = nil then
    //FChildDefs := GetChildDefsClass.Create(Self);
  Result := FChildDefs;
end;

procedure TZFieldDef.SetChildDefs(Value: TFieldDefs);
begin
  ChildDefs.Assign(Value);
end;

{$ENDIF TFIELDDEF_HAS_CHILDEFS}

type
  THackObjectField = Class(TObjectField);
function TZFieldDef.CreateFieldComponent(Owner: TComponent;
  ParentField: TObjectField = nil; FieldName: string = ''): TField;
var
  FieldClassType: TFieldClass;
begin
  if Collection is TFieldDefs then
    FieldClassType := ZSQLFieldClasses[FSQLType] else
    FieldClassType := nil;
  if FieldClassType = nil then DatabaseErrorFmt(SUnknownFieldType, [Name]);
  Result := FieldClassType.Create(Owner);
  try
    Result.Size := Size;
    if FieldName <> '' then
      Result.FieldName := FieldName else
      Result.FieldName := Name;
    Result.Required := faRequired in Attributes;
    Result.ReadOnly := faReadonly in Attributes;
    Result.SetFieldType(DataType);
    if Result is TBCDField then
      TBCDField(Result).Precision := Precision
    else if Result is TFMTBCDField then
      TFMTBCDField(Result).Precision := Precision;
    if Assigned(ParentField) then
      TZField(Result).ParentField := ParentField else
      Result.DataSet := TFieldDefs(Collection).DataSet;
    if ((faFixed in Attributes) or (DataType = ftFixedChar)) and (Result is TStringField) then
      TStringField(Result).FixedChar := True;
    if InternalCalcField then
      Result.FieldKind := fkInternalCalc;
    if (faUnNamed in Attributes) and (Result is TObjectField) then
      THackObjectField(Result).SetUnNamed(True);
  except
    Result.Free;
    raise;
  end;
end;

{$IFNDEF TFIELDDEF_HAS_CHILDEFS}
function TZFieldDef.GetChildDefsClass: TFieldDefsClass;
begin
  if Assigned(Collection) then
    Result := TFieldDefsClass(Collection.ClassType)
  else
    Result := DefaultFieldDefsClass;
end;
{$ENDIF TFIELDDEF_HAS_CHILDEFS}

constructor TZFieldDef.Create(Owner: TFieldDefs; const Name: string;
  FieldType: TFieldType; SQLType: TZSQLType; Size: Integer; Required: Boolean; FieldNo: Integer
  {$IFDEF WITH_CODEPAGE_AWARE_FIELD}; ACodePage: TSystemCodePage = CP_ACP{$ENDIF});
begin
  inherited Create(Owner, Name, FieldType, Size, Required, FieldNo
    {$IFDEF WITH_CODEPAGE_AWARE_FIELD}, ACodePage{$ENDIF});
  FSQLType := SQLType;
end;

{$IFNDEF TFIELDDEF_HAS_CHILDEFS}
destructor TZFieldDef.Destroy;
begin
  FreeAndNil(FChildDefs);
  inherited Destroy;
end;

function TZFieldDef.HasChildDefs: Boolean;
begin
  Result := (FChildDefs <> nil) and (FChildDefs.Count > 0);
end;

{$ENDIF}

function TZFieldDef.CreateField(Owner: TComponent; ParentField: TObjectField = nil;
  const FieldName: string = ''; CreateChildren: Boolean = True): TField;
var
  FieldCount, I: Integer;
begin
  Result := CreateFieldComponent(Owner, ParentField, FieldName);
  if CreateChildren and HasChildDefs then
  begin
    if (DataType = ftArray) then
    begin
      if TZAbstractRODataset(TFieldDefs(Collection).DataSet).SparseArrays then
        FieldCount := 1 else
        FieldCount := Size;
      for I := 0 to FieldCount - 1 do
        TZFieldDef(ChildDefs[0]).CreateField(nil, TObjectField(Result), Format('%s[%d]',
          [Result.FieldName, I]))
    end else
      for I := 0 to ChildDefs.Count - 1 do
        TZFieldDef(ChildDefs[I]).CreateField(nil, TObjectField(Result), '');
  end;
end;

{$IFNDEF WITH_TOBJECTFIELD}
{ TObjectField }

constructor TObjectField.Create(AOwner: TComponent);
begin
  FOwnedFields := TFields.Create(nil);
  FFields := FOwnedFields;
  inherited Create(AOwner);
end;

destructor TObjectField.Destroy;
begin
  inherited Destroy;
  FOwnedFields.Free;
end;

procedure TObjectField.ReadUnNamed(Reader: TReader);
begin
  SetUnNamed(Reader.ReadBoolean);
end;

procedure TObjectField.WriteUnNamed(Writer: TWriter);
begin
  Writer.WriteBoolean(UnNamed);
end;

procedure TObjectField.DefineProperties(Filer: TFiler);

  function UnNamedStored: Boolean;
  begin
    if Assigned(Filer.Ancestor) then
      Result := UnNamed <> TObjectField(Filer.Ancestor).UnNamed else
      Result := UnNamed;
  end;

begin
  inherited;
  Filer.DefineProperty('UnNamed', ReadUnNamed, WriteUnNamed, UnNamedStored);
end;

procedure TObjectField.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
  Field: TField;
begin
  for I := 0 to FOwnedFields.Count - 1 do
  begin
    Field := FOwnedFields[I];
    if Field.Owner = Root then Proc(Field);
  end;
end;

procedure TObjectField.SetChildOrder(Component: TComponent; Order: Integer);
var
  F: TField;
begin
  F := Component as TField;
  if FFields.IndexOf(F) >= 0 then
    F.Index := Order;
end;

function TObjectField.GetDefaultWidth: Integer;
var
  I: Integer;
begin
  Result := 10;
  if FOwnedFields.Count > 0 then
  begin
    for I := 0 to FOwnedFields.Count - 1 do
      Inc(Result, TZField(FOwnedFields[I]).GetDefaultWidth);
    Result := Result shr 1;
  end;
end;

function TObjectField.GetHasConstraints: Boolean;
var
  I: Integer;
begin
  Result := inherited GetHasConstraints;
  if not Result then
    for I := 0 to FFields.Count - 1 do
    begin
      Result := FFields[I].HasConstraints;
      if Result then Break;
    end;
end;

procedure TObjectField.SetFieldKind(Value: TFieldKind);
var
  I: Integer;
begin
  if FieldKind <> Value then
  begin
    {if (DataSet <> nil) and (DataSet.FDesigner <> nil) then
    with DataSet.Designer do
    begin
      BeginDesign;
      try
        FFieldKind := Value;
        for I := 0 to FFields.Count - 1 do
          FFields[I].FFieldKind := Value;
      finally
        EndDesign;
      end;
    end else}
    begin
      CheckInactive;
      FieldKind := Value;
      for I := 0 to FFields.Count - 1 do
        FFields[I].FieldKind := Value;
    end;
  end;
end;

procedure TObjectField.DataSetChanged;
//var
  //I: Integer;
begin
   { TODO : Check FOwnedFields/FFields (private section) }
  {FOwnedFields.DataSet := DataSet;
  for I := 0 to FOwnedFields.Count - 1 do
    FOwnedFields[I].DataSet := DataSet;}
  if (DataSet <> nil) and not TZAbstractRODataset(DataSet).ObjectView then
    TZAbstractRODataset(DataSet).ObjectView := True;
end;

procedure TObjectField.SetDataSet(ADataSet: TDataSet);
begin
  FFields := FOwnedFields;
  inherited SetDataSet(ADataSet);
  DataSetChanged;
end;

procedure TObjectField.SetParentField(AField: TObjectField);
begin
  FFields := FOwnedFields;
  inherited SetParentField(AField);
  DataSetChanged;
end;

class procedure TObjectField.CheckTypeSize(Value: Integer);
begin
  { Size is computed, no validation }
end;

{$IFNDEF WITH_VIRTUAL_TFIELD_BIND}
procedure TObjectField.Bind(Binding: Boolean);
begin
  if FieldKind = fkLookup then
    if Binding then
    begin
      if LookupCache then
        RefreshLookupList
      else
        ValidateLookupInfo(True);
   end;
end;
{$ENDIF}

procedure TObjectField.FreeBuffers;
{var
  I: Integer;}
begin
{ TODO : Check TFields.FreeBuffers for FPC how to get TFields overwritten? }
  {for I := 0 to FOwnedFields.Count - 1 do
    FOwnedFields[I].FreeBuffers;}
end;

function TObjectField.GetFieldCount: Integer;
begin
  Result := Fields.Count;
end;

function TObjectField.GetFields: TFields;
begin
  Result := FFields;
end;

function TObjectField.GetAsString: string;

  function ValueToStr(const V: Variant): string;
  var
    S: string;
    V2: Variant;
    HighBound, I: Integer;
    Sep: string;
  begin
    Result := '';
    if VarIsArray(V) then
    begin
      HighBound := VarArrayHighBound(V, 1);
      Sep := '';
      for I := 0 to HighBound do
      begin
        V2 := V[I];
        if VarIsArray(V2) then
          S := ValueToStr(V2) else
          S := VarToStr(V2);
        Result := Result + Sep + S;
        if I = 0 then Sep := FormatSettings.ListSeparator + ' ';
      end;
    end else
      Result := VarToStr(V);
    if Result <> '' then
      Result := '('+Result+')';
  end;

begin
  if (FFields = FOwnedFields) and (FFields.Count > 0) then
    Result := ValueToStr(GetAsVariant) else
    Result := inherited GetAsString;
end;

function TObjectField.GetFieldValue(Index: Integer): Variant;
begin
  Result := FFields[Index].Value;
end;

procedure TObjectField.SetFieldValue(Index: Integer; const Value: Variant);
begin
  FFields[Index].Value := Value;
end;

function TObjectField.GetAsVariant: Variant;
var
  I: Integer;
begin
  if IsNull then Result := Null else
  begin
    Result := VarArrayCreate([0, FieldCount - 1], varVariant);
    for I := 0 to FieldCount - 1 do
      Result[I] := GetFieldValue(I);
  end;
end;

procedure TObjectField.SetVarValue(const Value: Variant);
var
  Count, I: Integer;
begin
  Count := VarArrayHighBound(Value, 1) + 1;
  if Count > Size then Count := Size;
  for I := 0 to Count - 1  do
    SetFieldValue(I, Value[I]);
end;

procedure TObjectField.SetUnNamed(Value: Boolean);
begin
  FUnNamed := Value;
end;
{$ENDIF}

{ TArrayField }

{$IFNDEF WITH_TARRAYFIELD}
constructor TArrayField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftArray);
  Size := 10;
end;

procedure TArrayField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
{ TODO : Check how to get TFields.SparseArrays running with FPC? }
  {if TZAbstractRODataset(DataSet).SparseArrays then
    FFields.SparseFields := Size;}
end;

{$ENDIF !WITH_TARRAYFIELD}

{ TDataSetField }

{$IFNDEF WITH_TDATASETFIELD}
constructor TDataSetField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftDataSet);
end;

destructor TDataSetField.Destroy;
begin
  AssignNestedDataSet(nil);
  FOwnedDataSet.Free;
  inherited Destroy;
end;

procedure TDataSetField.SetIncludeObjectField(Value: Boolean);
begin
  if Assigned(FNestedDataSet) then
    TZAbstractRODataSet(FNestedDataSet).CheckInactive;
  FIncludeObjectField := Value;
end;

procedure TDataSetField.Bind(Binding: Boolean);
begin
  inherited Bind(Binding);
  if Assigned(FNestedDataSet) then
  begin
    if Binding then
    begin
      if FNestedDataSet.State = dsInActive then FNestedDataSet.Open;
    end
    else
      FNestedDataSet.Close;
  end;
end;

function TDataSetField.GetFields: TFields;
begin
  if FNestedDataSet = nil then
    GetNestedDataSet;
  Result := inherited GetFields;
end;

function TDataSetField.GetNestedDataSet: TDataSet;
begin
  if (FNestedDataSet = nil) and not (csReading in DataSet.ComponentState) then
    FNestedDataSet := TZAbstractRODataset(DataSet).CreateNestedDataSet(Self);
  Result := FNestedDataSet;
end;

procedure TDataSetField.AssignNestedDataSet(Value: TDataSet);
begin
  if Assigned(FNestedDataSet) then
  begin
    FNestedDataSet.Close;
    TZAbstractRODataset(FNestedDataSet).DataSetField := nil;
    if Assigned(DataSet) then
      TZAbstractRODataset(DataSet).NestedDataSets.Remove(FNestedDataSet);
  end;
  if Assigned(Value) then
  begin
    TZAbstractRODataset(DataSet).NestedDataSets.Add(Value);
    FFields := Value.Fields;
  end else
    FFields := FOwnedFields;
  FNestedDataSet := Value;
end;

function TDataSetField.GetCanModify: Boolean;
begin
  Result := inherited GetCanModify and Assigned(NestedDataSet) and
    FNestedDataSet.Active;
end;

procedure TDataSetField.Assign(Source: TPersistent);
var
  I: Integer;
  SourceDataset: TDataset;
  SourceField: TField;
begin
  inherited;
  if (Source is TDataSetField) then
  begin
    SourceDataset := (Source as TDataSetField).NestedDataSet;
    if not Assigned(SourceDataset) or not Assigned(NestedDataSet) then Exit;
    SourceDataset.First;
    while not SourceDataset.Eof do
    begin
      NestedDataset.Append;
      for I := 0 to NestedDataset.Fields.Count - 1 do
      begin
        SourceField := SourceDataset.FieldByName(NestedDataset.Fields[I].FieldName);
        if Assigned(SourceField) then
          NestedDataset.Fields[I].Assign(SourceField);
      end;
      NestedDataset.Post;
      SourceDataset.Next;
    end;
  end
  else
    inherited Assign(Source);
end;
{$ENDIF !WITH_TDATASETFIELD}

end.
