unit CommonsU;

interface

type
  TConfig = class sealed
  public
    class var CAPITALIZE: boolean;
    class var PROPERTYID: boolean;
    class var PROPERTYOBJVERSION: boolean;
    class var COLUMNATTRIBUTE: boolean;
    class var OUTPUTFILENAME: String;
    class var PARENTCLASS: String;
  end;

implementation

end.
