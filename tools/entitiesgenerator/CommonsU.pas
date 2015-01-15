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
    class var CLASSSUFFIX: String;
    class var TABLES: String;
    class var EXCLUDEDTABLES: String;
    class var EXCLUDEDCOLUMNS: String;
    class var INTERFACEUSES: String;
    class var TABLESCOLUMNSNULLABLE: String;
    class var FIELDSSERIALIZEASSTRING: String;
    class var CATALOGNAME: String;
    class var SCHEMANAME: String;
  end;

implementation

end.
