unit dorm.PersistenceCreator.Intf;

interface

uses
  System.Classes;

type
  IPersistenceCreatorViewCommand = interface
    ['{C4F5071A-F9D4-4C09-AED6-E7E8E6EF0821}']
    //*--------------------------------------*
    //* Procedure of the command of the view *
    //*--------------------------------------*
    procedure NewPersistenceFileExecute;
    procedure OpenPersistenceFileExecute;
    procedure SavePersistenceFileExecute;
    procedure ChangeEnvironmentExecute(ANewEnvironment: String);
    procedure AddPersistentClassExecute;
    procedure RemovePersistentClassExecute(AClassName: String);
  end;

  IPersistenceCreatorView = interface
    ['{05FEF279-8840-424F-90F5-5E6E8A108D56}']
    //*-----------------------------------------*
    //* Procedure to set the values of the view *
    //*-----------------------------------------*
    procedure SetEnvironments(AList: TStrings);
    procedure SetPersistentClass(AList: TStrings);
    procedure SetDatabaseAdapter(AValue: String);
    procedure SetKeysGenerator(AValue: String);
    procedure SetLoggerClassName(AValue: String);
    procedure SetKeyType(AValue: String);
    procedure SetCustomAdapterConfig(AList: TStrings);
    procedure SetDatabaseConnectionString(AText: String);
    procedure SetNullKeyValue(AText: String);
    procedure SetViewCommand(AViewCommand: IPersistenceCreatorViewCommand);
    procedure SetTitleCaption(AText: String);
    procedure SetStatusBarSimpleText(AText: String);
    //*----------------------------------------*
    //* Function to get the values of the view *
    //*----------------------------------------*
    function  GetEnvironments: TStrings;
    function  GetPersistentClass: TStrings;
    function  GetDatabaseAdapter: String;
    function  GetKeysGenerator: String;
    function  GetLoggerClassName: String;
    function  GetKeyType: String;
    function  GetCustomAdapterConfig: TStrings;
    function  GetDatabaseConnectionString: String;
    function  GetNullKeyValue: String;
    function StartView: Integer;
  end;

implementation

end.
