{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{             Core classes and interfaces                 }
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

unit ZClasses;

interface

{$I ZCore.inc}

uses
  SysUtils, Classes, SyncObjs
  {$IF defined(MSWINDOWS) and not defined(FPC)}, Windows{$IFEND} //some old comp. -> INFINITE
  {$IFDEF NO_UNIT_CONTNRS},System.Generics.Collections{$ENDIF};

const
  ZEOS_MAJOR_VERSION = 7;
  ZEOS_MINOR_VERSION = 2;
  ZEOS_SUB_VERSION = 14;
  ZEOS_STATUS = 'release';
  ZEOS_VERSION = Char(48+ZEOS_MAJOR_VERSION)+'.'+
                 {$IF ZEOS_MINOR_VERSION > 9}
                 Char(48+ZEOS_MINOR_VERSION div 10)+Char(48+ZEOS_MINOR_VERSION mod 10)+'.'+
                 {$ELSE}
                 Char(48+ZEOS_MINOR_VERSION)+'.'+
                 {$IFEND}
                 {$IF ZEOS_SUB_VERSION > 9}
                 Char(48+ZEOS_SUB_VERSION div 10)+Char(48+ZEOS_SUB_VERSION mod 10)+'-'+
                 {$ELSE}
                 Char(48+ZEOS_SUB_VERSION)+'-'+
                 {$IFEND}
                 ZEOS_STATUS;
{$IFDEF ENABLE_POOLED}
  {Pooled Protocol Prefix, including final dot}
  PooledPrefix = 'pooled.';
{$ENDIF}


type
  {$IFDEF OLDFPC}
  PDateTime = ^TDateTime;

  TAggregatedObject = class(TObject)
  private
    FController: Pointer;
    function GetController: IInterface;
  protected
    {$IFDEF FPC2_5UP}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; virtual;
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    {$ELSE}
    function QueryInterface(const iid : tguid;out obj) : longint;stdcall;
    function _AddRef : longint;stdcall;
    function _Release : longint;stdcall;
    {$ENDIF}
  public
    constructor Create(const Controller: IInterface);
    property Controller: IInterface read GetController;
  end;

  TContainedObject = class(TAggregatedObject, IInterface)
  protected
    {$IFDEF FPC2_5UP}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF}; override;
    {$ELSE}
    function QueryInterface(const iid : tguid;out obj) : longint;stdcall;
    {$ENDIF}

  end;
  {$ENDIF}

  {** Replacement for generic interface type. }
  IZInterface = IUnknown;

  {** Represents an interface for all abstract object. }
  IZObject = interface(IZInterface)
    ['{EF46E5F7-00CF-4DDA-BED0-057D6686AEE0}']
    function Equals(const Value: IZInterface): Boolean;
    function GetHashCode: LongInt;
    function Clone: IZInterface;
    function ToString: string;
    function InstanceOf(const IId: TGUID): Boolean;
  end;

  {** Represents a fake interface for coparable objects. }
  IZComparable = interface(IZObject)
    ['{04112081-F07B-4BBF-A757-817816EB67C1}']
  end;

  {** Represents an interface to clone objects. }
  IZClonnable = interface(IZObject)
    ['{ECB7F3A4-7B2E-4130-BA66-54A2D43C0149}']
  end;

  {** Represents a generic collection iterator interface. }
  IZIterator = interface(IZObject)
    ['{D964DDD0-2308-4D9B-BD36-5810632512F7}']
    function HasNext: Boolean;
    function Next: IZInterface;
  end;

  {** Represents a collection of object interfaces. }
  IZCollection = interface(IZClonnable)
    ['{51417C87-F992-4CAD-BC53-CF3925DD6E4C}']

    function Get(Index: Integer): IZInterface;
    procedure Put(Index: Integer; const Item: IZInterface);
    function IndexOf(const Item: IZInterface): Integer;
    function GetCount: Integer;
    function GetIterator: IZIterator;

    function First: IZInterface;
    function Last: IZInterface;

    function Add(const Item: IZInterface): Integer;
    procedure Insert(Index: Integer; const Item: IZInterface);
    function Remove(const Item: IZInterface): Integer;

    procedure Exchange(Index1, Index2: Integer);
    procedure Delete(Index: Integer);
    procedure Clear;

    function Contains(const Item: IZInterface): Boolean;
    function ContainsAll(const Col: IZCollection): Boolean;
    function AddAll(const Col: IZCollection): Boolean;
    function RemoveAll(const Col: IZCollection): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: IZInterface read Get write Put; default;
  end;

  {** Represents a hash map interface. }
  IZHashMap = interface(IZClonnable)
    ['{782C64F4-AD09-4F56-AF2B-E4193A05BBCE}']

    function Get(const Key: IZInterface): IZInterface;
    procedure Put(const Key: IZInterface; const Value: IZInterface);
    function GetKeys: IZCollection;
    function GetValues: IZCollection;
    function GetCount: Integer;

    function Remove(const Key: IZInterface): Boolean;
    procedure Clear;

    property Count: Integer read GetCount;
    property Keys: IZCollection read GetKeys;
    property Values: IZCollection read GetValues;
  end;

  {** Represents a stack interface. }
  IZStack = interface(IZClonnable)
    ['{8FEA0B3F-0C02-4E70-BD8D-FB0F42D4497B}']

    function Peek: IZInterface;
    function Pop: IZInterface;
    procedure Push(const Value: IZInterface);
    function GetCount: Integer;

    property Count: Integer read GetCount;
  end;

  {** Implements an abstract interfaced object. }
  // New TObject contains some methods with the same names but it has different
  // result/parameter types so we just hide the inherited methods
  TZAbstractObject = class(TInterfacedObject, IZObject)
  public
    // Parameter type differs from base (TObject)
    function Equals(const Value: IZInterface): Boolean; {$IFDEF WITH_NEWTOBJECT} reintroduce; {$ENDIF} virtual;
    // Result type differs from base (PtrInt @ FPC, Integer @ Delphi)
    function GetHashCode: LongInt; {$IFDEF WITH_NEWTOBJECT} reintroduce; {$ENDIF} virtual;
    function Clone: IZInterface; virtual;
    // Result type differs from base (ansistring/shortstring @ FPC, string @ Delphi)
    function ToString: string; {$IFDEF WITH_NEWTOBJECT} reintroduce; {$ENDIF} virtual;
    function InstanceOf(const IId: TGUID): Boolean;
  end;

  TZCharReaderStream = Class(TStream)
  private
    fEnd, fStart, fCurrent: PChar;
  protected
    function GetSize: Int64; override;
  public
    procedure SetBuffer(const Buffer: String);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  End;

// Exceptions
type
  TZExceptionSpecificData = class
  public
    function Clone: TZExceptionSpecificData; virtual; abstract;
  end;

  {** Abstract SQL exception. }
  EZSQLThrowable = class(Exception)
  private
    FErrorCode: Integer;
    FStatusCode: String;
  protected
    FSpecificData: TZExceptionSpecificData;
  public
    constructor Create(const Msg: string);
    constructor CreateWithCode(const ErrorCode: Integer; const Msg: string);
    constructor CreateWithStatus(const StatusCode: String; const Msg: string);
    constructor CreateWithCodeAndStatus(ErrorCode: Integer; const StatusCode: String; const Msg: string);
    constructor CreateClone(const E:EZSQLThrowable);
    destructor Destroy; override;

    property ErrorCode: Integer read FErrorCode;
    property StatusCode: string read FStatuscode; // The "String" Errocode // FirmOS
    property SpecificData: TZExceptionSpecificData read FSpecificData; // Engine-specific data
  end;

  {** Generic SQL exception. }
  EZSQLException = class(EZSQLThrowable);

  {** Generic connection lost exception. }
  EZSQLConnectionLost = class(EZSQLException);

  {** Generic SQL warning. }
  EZSQLWarning = class(EZSQLThrowable);

  {$IFDEF NO_UNIT_CONTNRS}
  TObjectList = class(TObjectList<TObject>);
  {$ENDIF}

  {** EH:
    implements a threaded timer which does not belong to the
    windows message queue nor VCL/FMX}
  TZThreadTimer = class(TObject)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TThreadMethod;
    FThread: TThread;
    FSignal: TEvent;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetOnTimer(Value: TThreadMethod);
  public
    constructor Create; overload;
    constructor Create(OnTimer: TThreadMethod;
      Interval: Cardinal; Enabled: Boolean); overload;
    destructor Destroy; override;
    procedure Reset;
  public
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TThreadMethod read FOnTimer write SetOnTimer;
  end;


implementation

uses ZMessages, ZCompatibility;

{$IFDEF oldFPC}

{ TAggregatedObject }

constructor TAggregatedObject.Create(const Controller: IInterface);
begin
  FController := Pointer(Controller);
end;

function TAggregatedObject.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

{$IFDEF FPC2_5UP}
function TAggregatedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;
{$ELSE}
function TAggregatedObject.QueryInterface(const iid : tguid;out obj) : longint;stdcall;
{$ENDIF}
begin
  Result := IInterface(FController).QueryInterface(IID, Obj);
end;

function TAggregatedObject._AddRef: longint;
begin
  Result := IInterface(FController)._AddRef;
end;

function TAggregatedObject._Release : longint;
begin
  Result := IInterface(FController)._Release;
end;

{ TContainedObject }

{$IFDEF FPC2_5UP}
function TContainedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
{$ELSE}
function TContainedObject.QueryInterface(const iid : tguid;out obj) : longint;stdcall;
{$ENDIF}

begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{$ENDIF}

{ TZAbstractObject }

{**
  Checks is the specified value equals to this object.
  @param Value an interface to some object.
  @return <code>True</code> if the objects are identical.
}
function TZAbstractObject.Equals(const Value: IZInterface): Boolean;
begin
  if Value <> nil then
  begin
    Result := (IZInterface(Self) = Value)
      or ((Self as IZInterface) = (Value as IZInterface));
  end else
   Result := False;
end;

{**
  Gets a unique hash for this object.
  @return a unique hash for this object.
}
function TZAbstractObject.GetHashCode: LongInt;
begin
  Result := LongInt(Self);
end;

{**
  Clones an object instance.
  @return a clonned object instance.
}
function TZAbstractObject.Clone: IZInterface;
begin
  raise Exception.Create(SClonningIsNotSupported);
  result := nil;
end;

{**
  Checks is this object implements a specified interface.
  @param IId an interface id.
  @return <code>True</code> if this object support the interface.
}
function TZAbstractObject.InstanceOf(const IId: TGUID): Boolean;
begin
  Result := GetInterfaceEntry(IId) <> nil;
end;

{**
  Converts this object into the string representation.
  @return a string representation for this object.
}
function TZAbstractObject.ToString: string;
begin
  Result := Format('%s <%p>', [ClassName, Pointer(Self)])
end;

{ TZCharReaderStream }

function TZCharReaderStream.GetSize: Int64;
begin
  Result := Int64(fEnd-fStart){%H-}-1
end;

function TZCharReaderStream.Read(var Buffer; Count: Integer): Longint;
begin
  if (Count = SizeOf(Char)) and (fCurrent < fEnd) then begin
    //just a little byte/dword inline move instead of Move()
    //skip all possible compiler magic
    {$IFDEF UNICODE}
    Word(Buffer) := PWord(fCurrent)^;
    {$ELSE}
    Byte(Buffer) := PByte(fCurrent)^;
    {$ENDIF}
    Inc(fCurrent);
    Result := SizeOf(Char);
  end else
    Result := 0
end;

function TZCharReaderStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: fCurrent := {%H-}Pointer({%H-}NativeInt(fStart)+Offset);
    soFromCurrent:   fCurrent := {%H-}Pointer({%H-}NativeInt(fCurrent)+Offset);
    soFromEnd:       fCurrent := {%H-}Pointer({%H-}NativeInt(fEnd-1)+Offset);
  end;
  Result := origin; //make compiler happy: a true positoned processing is nowhere used in our code
  //Result := LongInt(fCurrent-fStart);
end;

function TZCharReaderStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Ord(Origin) of
    soFromBeginning: fCurrent := {%H-}Pointer({%H-}NativeInt(fStart)+Offset);
    soFromCurrent:   fCurrent := {%H-}Pointer({%H-}NativeInt(fCurrent)+Offset);
    soFromEnd:       fCurrent := {%H-}Pointer({%H-}NativeInt(fEnd-1)+Offset);
  end;
  Result := Ord(origin); //make compiler happy: a true positoned processing is nowhere used in our code
  //Result := Int64(fCurrent-fStart);
end;

procedure TZCharReaderStream.SetBuffer(const Buffer: String);
begin
  fStart := Pointer(Buffer);
  fCurrent := fStart;
  fEnd := fStart+Length(Buffer);
end;

{$IFDEF FPC} // parameters not used intentionally
  {$PUSH}
  {$WARN 5033 off : Function result does not seem to be set}
  {$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
function TZCharReaderStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise Exception.Create(SUnsupportedOperation);
end;
{$IFDEF FPC} {$POP} {$ENDIF}

{ EZSQLThrowable }

constructor EZSQLThrowable.CreateClone(const E: EZSQLThrowable);
begin
  inherited Create(E.Message);
  FErrorCode:=E.ErrorCode;
  FStatusCode:=E.Statuscode;
  if E.SpecificData <> nil then
    FSpecificData := E.SpecificData.Clone;
end;

{**
  Creates an exception with message string.
  @param Msg a error description.
}
constructor EZSQLThrowable.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := -1;
end;

{**
  Creates an exception with message string.
  @param Msg a error description.
  @param ErrorCode a native server error code.
}
constructor EZSQLThrowable.CreateWithCode(const ErrorCode: Integer;
  const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
end;

{**
  Creates an exception with message string.
  @param ErrorCode a native server error code.
  @param StatusCode a server status code.
  @param Msg a error description.
}
constructor EZSQLThrowable.CreateWithCodeAndStatus(ErrorCode: Integer;
  const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := ErrorCode;
  FStatusCode := StatusCode;
end;

{**
  Creates an exception with message string.
  @param StatusCode a server status code.
  @param Msg a error description.
}
constructor EZSQLThrowable.CreateWithStatus(const StatusCode, Msg: string);
begin
  inherited Create(Msg);
  FStatusCode := StatusCode;
end;

destructor EZSQLThrowable.Destroy;
begin
  FreeAndNil(FSpecificData);
  inherited;
end;

type
  TZIntervalThread = class(TThread)
  private
    FSignal: TEvent;
    FInterval: Cardinal;
    FOnTimer: TThreadMethod;
    FActive: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(Signal: TEvent);
  end;

{ TZThreadTimer }

constructor TZThreadTimer.Create;
begin
  inherited Create;
  FSignal := TSimpleEvent.Create;
end;

constructor TZThreadTimer.Create(OnTimer: TThreadMethod;
  Interval: Cardinal; Enabled: Boolean);
begin
  Create;
  FInterval := Interval;
  FOnTimer := OnTimer;
  FEnabled := Enabled;
  FThread := TZIntervalThread.Create(FSignal);
  TZIntervalThread(FThread).FOnTimer := FOnTimer;
  TZIntervalThread(FThread).FInterval := FInterval;
  TZIntervalThread(FThread).Suspended := False; //start thread
  Reset;
end;

destructor TZThreadTimer.Destroy;
begin
  FThread.Terminate;
  FSignal.SetEvent; //signal to break the waittime
  FThread.WaitFor;
  FreeAndNil(FThread);
  FreeAndNil(FSignal);
  inherited;
end;

procedure TZThreadTimer.Reset;
  procedure SignalThread;
  begin
    if FThread <> nil then begin
      FSignal.SetEvent; //signal thread should Start now
      while FSignal.WaitFor(1) = wrSignaled do; //wait until thread confirms event
    end;
  end;
begin
  SignalThread; //change active state
  TZIntervalThread(FThread).FOnTimer := FOnTimer;
  if FEnabled and Assigned(FOnTimer) and (FInterval > 0)
  then TZIntervalThread(FThread).FInterval := FInterval
  else TZIntervalThread(FThread).FInterval := INFINITE;
  SignalThread; //change active state
end;

procedure TZThreadTimer.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then begin
    FEnabled := Value;
    Reset;
  end;
end;

procedure TZThreadTimer.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then begin
    FInterval := Value;
    Reset;
  end;
end;

procedure TZThreadTimer.SetOnTimer(Value: TThreadMethod);
begin
  if @FOnTimer <> @Value then begin
    FOnTimer := Value;
    Reset;
  end;
end;

{ TZIntervalThread }

constructor TZIntervalThread.Create(Signal: TEvent);
begin
  inherited Create(True); //suspended
  FActive := True;
  FSignal := Signal;
end;

procedure TZIntervalThread.Execute;
begin
  while not Terminated do
    case FSignal.WaitFor(FInterval) of
      wrTimeout:  if FActive and Assigned(FOnTimer) and (FInterval <> INFINITE) then
                    FOnTimer;
      wrSignaled: begin
                    FActive := not FActive;
                    FSignal.ResetEvent;
                  end;
      else        Break;
    end;
end;

end.

