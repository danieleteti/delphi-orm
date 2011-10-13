unit dorm.utils.Sequences;

interface

type
  TSequenceGUID = class
    class function NewGUID: string;
  end;

implementation

uses
  SysUtils;

//class function TSequenceGUID.NewGUID: string;
//var
//  guid: TGUID;
//begin
//  CreateGUID(guid);
//  Result := GUIDToString(guid);
//end;

class function TSequenceGUID.NewGUID: string;
var
  guid: string;
begin
  Result := IntToStr(Random(100000));
end;


initialization
  Randomize;

end.
