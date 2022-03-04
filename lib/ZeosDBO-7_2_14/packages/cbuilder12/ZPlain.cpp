//---------------------------------------------------------------------------



#include <vcl.h>

#pragma hdrstop

USERES("ZPlain.res");

USEPACKAGE("rtl.bpi");

USEPACKAGE("ZCore.bpi");

USEUNIT("..\..\src\plain\ZPlainPostgreSqlDriver.pas");

USEUNIT("..\..\src\plain\ZPlainAdo.pas");

USEUNIT("..\..\src\plain\ZPlainAdoDriver.pas");

USEUNIT("..\..\src\plain\ZPlainDbLibDriver.pas");

USEUNIT("..\..\src\plain\ZPlainDbLibConstants.pas");

USEUNIT("..\..\src\plain\ZPlainDriver.pas");

USEUNIT("..\..\src\plain\ZPlainFirebirdDriver.pas");

USEUNIT("..\..\src\plain\ZPlainLoader.pas");

USEUNIT("..\..\src\plain\ZPlainMySqlDriver.pas");

USEUNIT("..\..\src\plain\ZPlainSqLiteDriver.pas");

USEUNIT("..\..\src\plain\ZPlainOracleConstants.pas");

USEUNIT("..\..\src\plain\ZPlainOracleDriver.pas");

USEUNIT("..\..\src\plain\ZPlainASADriver.pas");

USEUNIT("..\..\src\plain\ZPlainASAConstants.pas");

//---------------------------------------------------------------------------

#pragma package(smart_init)

//---------------------------------------------------------------------------



//   Package source.

//---------------------------------------------------------------------------



#pragma argsused

int WINAPI DllEntryPoint(HINSTANCE hinst, unsigned long reason, void*)

{

        return 1;

}

//---------------------------------------------------------------------------

