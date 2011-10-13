unit dorm.InterposedObject;

interface

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

initialization

TdormObject.RegisterClass;

end.
