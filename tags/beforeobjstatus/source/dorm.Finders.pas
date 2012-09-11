{ *******************************************************************************
  Copyright 2010-2012 Daniele Teti

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

unit dorm.Finders;

interface

uses
  dorm,
  dorm.Commons,
  dorm.Collections,
  dorm.Filters,
  rtti,
  Classes,
  SysUtils,
  TypInfo;

type
  TLimitedQuery = class abstract(TdormCriteria, ICustomCriteria)
  private
    FMaxRows: Integer;
    procedure SetMaxRows(const Value: Integer);
  public
    constructor Create; override;
    function GetSQL: string; virtual; abstract;
    function GetItemClassInfo: PTypeInfo; virtual; abstract;
    property MaxRows: Integer read FMaxRows write SetMaxRows;
  end;

  TLikeFilter = class(TdormCriteria, ICustomCriteria)
  private
    FFieldName: string;
    FValue: string;
  public
    function GetSQL: string; virtual; abstract;
    function GetItemClassInfo: PTypeInfo; virtual; abstract;
    property FieldName: string read FFieldName write FFieldName;
    property Value: string read FValue write FValue;
  end;

implementation

function FBEscape(const Value: string): string;
begin
  Result := StringReplace(Value, '''', '''''', [rfReplaceAll]);
end;

{ TLimitedQuery }

constructor TLimitedQuery.Create;
begin
  inherited;
  FMaxRows := 50;
end;

procedure TLimitedQuery.SetMaxRows(const Value: Integer);
begin
  FMaxRows := Value;
end;

end.
