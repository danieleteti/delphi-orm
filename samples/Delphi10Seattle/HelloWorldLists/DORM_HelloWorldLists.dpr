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
  ClrScr;
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
  ClrScr;
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

var
  K: char;

begin
  repeat
    ClrScr;
    WriteLn('1. List of object using LoadList<T>');
    WriteLn('2. Filter sample application using DSQL');
    WriteLn('3. Filter sample using a dorm'' Finder');
    WriteLn('0. Quit');
    while True do
    begin
      K := ReadKey;
      if K in ['0', '1', '2', '3'] then
        Break;
    end;

    case K of
      '0':
        Break;
      '1':
        begin
          Lists;
          Write('Hit return to terminate this sample');
          ReadLn;
        end;
      '2':
        begin
          SearchCriteria;
        end;
      '3':
        begin
          SearchCriteriaFinder;
          Write('Hit return to terminate this sample');
          ReadLn;
        end;
    end;
  until false;
  WriteLn('See you... hit return to terminate');
  ReadLn;

end.
