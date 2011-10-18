unit dorm.UOW;

interface

uses
  rtti,
  dorm.Commons,
  dorm.Collections;

type
  TdormUOW = class
  protected
    FUOWInsert, FUOWUpdate, FUOWDelete: TdormCollection;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function AddInsertOrUpdate(Obj: TObject;
      const OIDPropertyName: String = 'ID'): TdormUOW;
    function AddDelete(Obj: TObject): TdormUOW;
    function AddInsert(Obj: TObject): TdormUOW;
    function AddUpdate(Obj: TObject): TdormUOW;
    function GetUOWInsert: TdormCollection;
    function GetUOWUpdate: TdormCollection;
    function GetUOWDelete: TdormCollection;
    procedure Clear;
    procedure Cancel;
  end;

implementation

uses
  dorm.Utils,
  SysUtils;

{ TdormUOW }

function TdormUOW.AddDelete(Obj: TObject): TdormUOW;
var
  o: TObject;
begin
  if FUOWInsert.IndexOf(Obj) > -1 then
  begin
    o := FUOWInsert.Extract(Obj);
    FreeAndNil(o);
  end
  else if FUOWUpdate.IndexOf(Obj) > -1 then
  begin
    o := FUOWUpdate.Extract(Obj);
    FUOWDelete.Add(o);
  end
  else
  begin
    FUOWDelete.Add(Obj);
  end;
  Result := Self;
end;

function TdormUOW.AddInsert(Obj: TObject): TdormUOW;
var
  v: TValue;
begin
  Result := Self;
  if (FUOWInsert.IndexOf(Obj) > -1) then
    Exit;
  FUOWInsert.Add(Obj)
end;

function TdormUOW.AddInsertOrUpdate(Obj: TObject; const OIDPropertyName: String)
  : TdormUOW;
var
  v: TValue;
begin
  if (FUOWInsert.IndexOf(Obj) > -1) or (FUOWUpdate.IndexOf(Obj) > -1) then
  begin
    Result := Self;
    Exit;
  end;

  v := TdormUtils.GetProperty(Obj, OIDPropertyName);
  if v.IsType<Int64> then
  begin
    if v.AsInt64 = 0 then
      FUOWInsert.Add(Obj)
    else
      FUOWUpdate.Add(Obj);
  end
  else if v.IsType<string> then
  begin
    if v.AsString = EmptyStr then
      FUOWInsert.Add(Obj)
    else
      FUOWUpdate.Add(Obj);
  end
  else
    raise EdormException.Create
      ('Invalid key type in TdormUOW.AddInsertOrUpdate');
  Result := Self;
end;

function TdormUOW.AddUpdate(Obj: TObject): TdormUOW;
var
  v: TValue;
begin
  Result := Self;
  if (FUOWUpdate.IndexOf(Obj) > -1) then
    Exit;
  FUOWUpdate.Add(Obj);
end;

procedure TdormUOW.Cancel;
var
  I: Integer;
begin
  for I := 0 to FUOWDelete.Count - 1 do
    FUOWDelete.Extract(I);
end;

procedure TdormUOW.Clear;
begin
  FUOWInsert.Clear;
  FUOWUpdate.Clear;
  FUOWDelete.Clear; // this MUST delete also the objects
end;

constructor TdormUOW.Create;
begin
  inherited;
  FUOWInsert := NewList(false);
  FUOWUpdate := NewList(false);
  FUOWDelete := NewList(true); // this MUST delete also the objects
end;

destructor TdormUOW.Destroy;
begin
  FUOWInsert.Free;
  FUOWUpdate.Free;
  FUOWDelete.Free;
  inherited;
end;

function TdormUOW.GetUOWDelete: TdormCollection;
begin
  Result := FUOWDelete;
end;

function TdormUOW.GetUOWInsert: TdormCollection;
begin
  Result := FUOWInsert;
end;

function TdormUOW.GetUOWUpdate: TdormCollection;
begin
  Result := FUOWUpdate;
end;

end.
