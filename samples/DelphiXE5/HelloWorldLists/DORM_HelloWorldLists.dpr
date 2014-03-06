program DORM_HelloWorldLists;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Generics.Collections,
  dorm,
  dorm.commons,
  dorm.Query,
  dorm.loggers,
  BObjectsU in '..\..\Commons\BObjectsU.pas',
  dorm.ObjectStatus,
  RandomUtilsU in '..\..\Commons\RandomUtilsU.pas',
  Console in '..\..\Commons\Console.pas',
  FindersU in 'FindersU.pas';

{$IFDEF LINK_SQLSERVERFIREDAC_ADAPTER}


const
  CONFIG_FILE = '..\..\dorm_sqlserver_firedac.conf';

const
  FILTER_WHERE = '(#TCustomerOS.Name# LIKE ''%''+?+''%'') OR (#TCustomerOS.Address# LIKE ''%''+?+''%'')';
{$ENDIF}
{$IFNDEF LINK_SQLSERVERFIREDAC_ADAPTER}


const
  CONFIG_FILE = '..\..\dorm.conf';

const
  FILTER_WHERE = '#TCustomerOS.Name# CONTAINING ? or #TCustomerOS.Address# CONTAINING ?';
{$ENDIF}


procedure Lists;

var
  dormSession: TSession;
  Customer: TCustomerOS;
  id: Integer;
  I: Integer;
  Customers: TObjectList<TCustomerOS>;

begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    CreateCustomers(dormSession, 20);
    Customers := dormSession.LoadList<TCustomerOS>;
    for Customer in Customers do
    begin
      WriteLn('|', Customer.Name.PadRight(25),
        '|', Customer.Address.PadRight(30),
        '|', DateToStr(Customer.CreatedAt).PadRight(12));
    end;
    Customers.Free;
  finally
    dormSession.Free;
  end;
end;

procedure SearchCriteriaFinder;
var
  dormSession: TSession;
  Customer: TCustomerOS;
  id: Integer;
  I: Integer;
  Customers: TObjectList<TCustomerOS>;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    CreateCustomers(dormSession, 20);
    Customers := dormSession.LoadList<TCustomerOS>(TPeopleFinder.Create);
    for Customer in Customers do
    begin
      WriteLn('|', Customer.Name.PadRight(25),
        '|', Customer.Address.PadRight(30),
        '|', DateToStr(Customer.CreatedAt).PadRight(12));
    end;
    Customers.Free;
  finally
    dormSession.Free;
  end;
end;

procedure SearchCriteria;
var
  dormSession: TSession;
  Customer: TCustomerOS;
  id: Integer;
  I: Integer;
  Customers: TObjectList<TCustomerOS>;
  Filter: String;
  Attr: Integer;
begin
  dormSession := TSession.CreateConfigured(
    TStreamReader.Create(CONFIG_FILE), TdormEnvironment.deDevelopment);
  try
    CreateCustomers(dormSession, 30);
    Filter := '';
    while True do
    begin
      Customers := dormSession.LoadListSQL<TCustomerOS>(
        Select
        .From(TCustomerOS)
        .Where(FILTER_WHERE, [Filter, Filter])
        .orderBy('#TCustomerOS.Name#')
        );
      TextColor(Red);
      TextBackground(White);
      WriteLn(Format('%4d objects found', [Customers.Count]));
      TextColor(LightGray);
      TextBackground(Black);
      for Customer in Customers do
      begin
        WriteLn('|', Customer.Name.PadRight(25),
          '|', Customer.Address.PadRight(30),
          '|', DateToStr(Customer.CreatedAt).PadRight(12));
      end;
      Customers.Free;

      // user filter
      WriteLn;
      Write('Filter ("quit" to exit): ');
      ReadLn(Filter);
      if Filter = 'quit' then
      begin
        Write('see you...');
        Break;
      end;
      ClrScr;
    end;
  finally
    dormSession.Free;
  end;
end;

begin
  // uncomments only one of the following lines

  // uncomment the following line to see a normal search
  // Lists;

  // uncomment the following line to see a DSQL criteria driven search
  SearchCriteria;
  // uncomment the following line to see a criteria driven search
  // SearchCriteriaFinder;
  ReadLn;

end.
