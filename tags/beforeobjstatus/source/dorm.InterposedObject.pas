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

unit dorm.InterposedObject;

interface

uses
  Generics.Collections;

type
  TdormObject = class(TObject)
  private
    FValidationErrors: string;
  protected
    procedure AddError(const Error: string);
    procedure ClearErrors;
  public
    // Called at every validation. NEED TO BE INHERITED IN CHILD CLASSES
    function Validate: boolean; virtual;
    // Called after "Validate" only while inserting
    function InsertValidate: boolean; virtual;
    // Called after "Validate" only while Updating
    function UpdateValidate: boolean; virtual;
    // Called after "Validate" only while Deleting
    function DeleteValidate: boolean; virtual;
    // Retuns all the errors related to the last validation
    function ValidationErrors: string;

    procedure OnAfterLoad; virtual;
    procedure OnBeforeInsert; virtual;
    procedure OnBeforeUpdate; virtual;
    procedure OnBeforeDelete; virtual;
  private
    class procedure RegisterClass;
  end;

{$IF CompilerVersion = 22}
  // In DelphiXE you cannot use directly TObjectList<T> because
  // GetItem method is private. SO you have to use a specific typed list
  // as this sample show. However, you can avoid default delphi collections
  // and use whatever list you want. The only requirements are the following methods:
  // - function Add(Object: TObject)
  // - procedure Clear
  // - property Count: Integer
  // - function GetItem(Index: Integer): TObject

  TdormObjectList<T: class> = class(TObjectList<T>)
  public
    function GetElement(Index: Integer): T;
  end;
{$IFEND}


implementation

{ TdormObject }

procedure TdormObject.AddError(const Error: string);
begin
  FValidationErrors := FValidationErrors + sLineBreak + Error;
end;

procedure TdormObject.ClearErrors;
begin
  FValidationErrors := '';
end;

function TdormObject.DeleteValidate: boolean;
begin
  Result := True;
end;

function TdormObject.InsertValidate: boolean;
begin
  Result := True;
end;

procedure TdormObject.OnAfterLoad;
begin

end;

procedure TdormObject.OnBeforeDelete;
begin

end;

procedure TdormObject.OnBeforeInsert;
begin

end;

procedure TdormObject.OnBeforeUpdate;
begin

end;

class procedure TdormObject.RegisterClass;
begin
  //
end;

function TdormObject.UpdateValidate: boolean;
begin
  Result := True;
end;

function TdormObject.Validate: boolean;
begin
  ClearErrors;
  Result := True;
end;

function TdormObject.ValidationErrors: string;
begin
  Result := FValidationErrors;
end;

{ TdormObjectList<T> }
{$IF CompilerVersion = 22}


function TdormObjectList<T>.GetElement(index: Integer): T;
begin
  Result := Items[index];
end;
{$IFEND}

initialization

TdormObject.RegisterClass;

end.
