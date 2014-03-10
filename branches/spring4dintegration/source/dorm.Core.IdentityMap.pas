{ *******************************************************************************
  Copyright 2010-2013 Daniele Teti

  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
  ******************************************************************************** }

unit dorm.Core.IdentityMap;

interface

uses
  Generics.Collections,
  dorm.Commons;

type
  TIdentityMapValue = class
  public
    FObject: TObject;
    FObjectOwner: TdormObjectOwner;
    constructor Create(AObject: TObject; AObjectOwner: TdormObjectOwner);
    destructor Destroy; override;
  end;

  TIdentityMap = class
  strict private
    FDict: TDictionary<string, TIdentityMapValue>;
    function GetOIDIdentityAsMapValue(const Value: string;
      out AIdentityMapValue: TIdentityMapValue): boolean;
  public
    function GetOIDIdentity(const Value: string; out AObject: TObject): boolean;
    procedure AddIdentity(AObject: TObject; OID: string;
      AOwner: TdormObjectOwner = ooItself);
    procedure RemoveIdentity(OID: string);
    function IsLoadedObject(OID: string): boolean;
    procedure Extract(OID: string);
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  end;

implementation

uses
  SysUtils;

{ TIdentityMap }

procedure TIdentityMap.AddIdentity(AObject: TObject; OID: string;
  AOwner: TdormObjectOwner = ooItself);
begin
  FDict.Add(OID, TIdentityMapValue.Create(AObject, AOwner));
end;

procedure TIdentityMap.Clear;
var
  o, ofree: TIdentityMapValue;
begin
  for o in FDict.Values do
  begin
    ofree := o;
    FreeAndNil(ofree);
  end;
  FDict.Clear;
end;

constructor TIdentityMap.Create;
begin
  inherited;
  FDict := TDictionary<string, TIdentityMapValue>.Create;
end;

destructor TIdentityMap.Destroy;
begin
  FDict.Free;
  inherited;
end;

procedure TIdentityMap.Extract(OID: string);
begin
  RemoveIdentity(OID);
end;

function TIdentityMap.GetOIDIdentity(const Value: string;
  out AObject: TObject): boolean;
var
  imv: TIdentityMapValue;
begin
  AObject := nil;
  Result := FDict.TryGetValue(Value, imv);
  if Result then
    AObject := imv.FObject;
end;

function TIdentityMap.GetOIDIdentityAsMapValue(const Value: string;
  out AIdentityMapValue: TIdentityMapValue): boolean;
begin
  AIdentityMapValue := nil;
  Result := FDict.TryGetValue(Value, AIdentityMapValue);
end;

function TIdentityMap.IsLoadedObject(OID: string): boolean;
var
  obj: TObject;
begin
  Result := GetOIDIdentity(OID, obj);
end;

procedure TIdentityMap.RemoveIdentity(OID: string);
var
  imv: TIdentityMapValue;
begin
  if GetOIDIdentityAsMapValue(OID, imv) then
  begin
    imv.FObjectOwner := ooParent; // so the following free doesn't free it
    imv.Free;
  end;
  FDict.Remove(OID);
end;

{ TIdentityMapValue }

constructor TIdentityMapValue.Create(AObject: TObject;
  AObjectOwner: TdormObjectOwner);
begin
  inherited Create;
  FObject := AObject;
  FObjectOwner := AObjectOwner;
end;

destructor TIdentityMapValue.Destroy;
begin
  if FObjectOwner = ooItself then
    FObject.Free;
  inherited;
end;

end.
