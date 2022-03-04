unit zdbcplatform;

interface

{$I zpackages.inc}

{$warn 5023 off : no warning about unused units}

uses
{$IFDEF ENABLE_OLEDB}
  ZDbcOleDB,
{$ENDIF}

{$IFDEF ENABLE_ADO}
  ZDbcAdo,
{$ENDIF}
  ZCompatibility;  // just to have one unit used always

implementation

end.
