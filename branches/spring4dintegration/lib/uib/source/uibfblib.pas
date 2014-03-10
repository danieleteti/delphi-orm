unit uibfblib;

interface
uses uibase;

const
	PluginType_YValve: Cardinal = 1;
	PluginType_Provider: Cardinal = 2;
	// leave space for may be some more super-std plugins
	PluginType_FirstNonLibPlugin: Cardinal = 11;
	PluginType_AuthServer: Cardinal = 11;
	PluginType_AuthClient: Cardinal = 12;
	PluginType_AuthUserManagement: Cardinal = 13;
	PluginType_ExternalEngine: Cardinal = 14;
	PluginType_Trace: Cardinal = 15;
	PluginType_MaxType: Cardinal = 16;	// keep in sync please



  PREPARE_PREFETCH_TYPE              = $01;
  PREPARE_PREFETCH_INPUT_PARAMETERS  = $02;
  PREPARE_PREFETCH_OUTPUT_PARAMETERS = $04;
  PREPARE_PREFETCH_LEGACY_PLAN       = $08;
  PREPARE_PREFETCH_DETAILED_PLAN     = $10;
  PREPARE_PREFETCH_AFFECTED_RECORDS  = $20; { not used yet }
  PREPARE_PREFETCH_METADATA          = PREPARE_PREFETCH_TYPE or PREPARE_PREFETCH_INPUT_PARAMETERS or PREPARE_PREFETCH_OUTPUT_PARAMETERS;
  PREPARE_PREFETCH_ALL               = PREPARE_PREFETCH_METADATA or PREPARE_PREFETCH_LEGACY_PLAN or PREPARE_PREFETCH_DETAILED_PLAN or PREPARE_PREFETCH_AFFECTED_RECORDS;

type
  TError = class
  public
  	function addCode(code: Integer): Boolean; virtual; cdecl; abstract;
	  function addString(const str: PAnsiChar; strLength: Cardinal): Boolean; virtual; cdecl; abstract;
  end;

  PUtf8 = PAnsiChar;


  TPluginModule = class;
  TMaster = class;

(*******************************************************************************
 *
 *	MODULE:			firebird/Interface.h
 *	DESCRIPTION:	Base class for all FB interfaces / plugins.
 *
 *******************************************************************************)

  TVersioned = class
  public
    function getVersion: Integer; virtual; cdecl; abstract;
    function getModule: TPluginModule; virtual; cdecl; abstract;
  end;

  TRefCounted = class(TVersioned)
  public
    procedure addRef; virtual; cdecl; abstract;
    function release: Integer; virtual; cdecl; abstract;
  end;

  TDisposable = class(TVersioned)
  public
  	procedure dispose; virtual; cdecl; abstract;
  end;

  TStatus = class(TDisposable)
  public
    procedure set1(length: Cardinal; const value: PISCStatus); virtual; cdecl; abstract;
    procedure set2(const value: PISCStatus); virtual; cdecl; abstract;
    procedure init; virtual; cdecl; abstract;

    function get: PISCStatus; virtual; cdecl; abstract;
    function isSuccess: Integer; virtual; cdecl; abstract;
  end;

  TProvider = class;
  TPluginManager = class;
  TTimerControl = class;
  TDtc = class;
  TAttachment = class;
  TTransaction = class;


  PUpgradeInfo = ^TUpgradeInfo;
  TUpgradeInfo = record
    missingFunctionClass: Pointer;
    clientModule: TPluginModule;
  end;

  TMaster = class(TVersioned)
  public
    function getStatus: TStatus; virtual; cdecl; abstract;
    function getDispatcher: TProvider; virtual; cdecl; abstract;
    function getPluginManager: TPluginManager; virtual; cdecl; abstract;
    function upgradeInterface(toUpgrade: TVersioned; desiredVersion: Integer; upgradeInfo: PUpgradeInfo): Integer; virtual; cdecl; abstract;
    function circularAlloc(const s: PAnsiChar; len: Cardinal; thr: IntPtr): PAnsiChar; virtual; cdecl; abstract;
    function getTimerControl: TTimerControl; virtual; cdecl; abstract;
    function getDtc: TDtc; virtual; cdecl; abstract;
    function registerAttachment(provider: TProvider; attachment: TAttachment): TAttachment; virtual; cdecl; abstract;
    function registerTransaction(attachment: TAttachment; transaction: TTransaction): TTransaction; virtual; cdecl; abstract;
  end;

(*
 * Firebird plugins are accessed using methods of PluginLoader interface.
 * For each plugin_module tag found, it constructs a Plugin object, reads the corresponding
 * plugin_config tag and inserts all config information in the object.
 *
 * When requested, the engine gets the attribute value of plugin_module/filename, load it as a
 * dynamic (shared) library and calls the exported function firebirdPlugin (FB_PLUGIN_ENTRY_POINT
 * definition, PluginEntrypoint prototype) passing the Plugin object as parameter.
 *
 * The plugin library may save the plugin object and call they methods later. The object and all
 * pointers returned by it are valid until the plugin is unloaded (done through OS unload of the
 * dynamic library) when Firebird is shutting down.
 *
 * Inside the plugin entry point (firebirdPlugin), the plugin may register extra functionality that
 * may be obtained by Firebird when required. Currently only External Engines may be registered
 * through Plugin::setExternalEngineFactory.
 *
 * Example plugin configuration file:
 *
 * <external_engine UDR>
 *     plugin_module UDR_engine
 * </external_engine>
 *
 * <plugin_module UDR_engine>
 *     filename $(this)/udr_engine
 *     plugin_config UDR_config
 * </plugin_module>
 *
 * <plugin_config UDR_config>
 *     path $(this)/udr
 * </plugin_config>
 *
 * Note that the external_engine tag is ignored at this stage. Only plugin_module and plugin_config
 * are read. The dynamic library extension may be ommitted, and $(this) expands to the directory of
 * the .conf file.
 *
 * Plugins may access Firebird API through the fbclient library.
 *)

  TPluginBase = class(TRefCounted)
  public
    procedure setOwner(ref: TRefCounted); virtual; cdecl; abstract;
    function getOwner: TRefCounted; virtual; cdecl; abstract;
  end;

  // TPluginSet - low level tool to access plugins according to parameter from firebird.conf
  TPluginSet = class(TRefCounted)
  public
    function getName: PAnsiChar; virtual; cdecl; abstract;
    function getModule: PAnsiChar; virtual; cdecl; abstract;
    function getPlugin: TPluginBase; virtual; cdecl; abstract;
    procedure next; virtual; cdecl; abstract;
    procedure put(const p: PAnsiChar); virtual; cdecl; abstract;
  end;

  TConfig = class;

  // Entry in configuration file
  TConfigEntry = class(TRefCounted)
  public
    function getName: PAnsiChar; virtual; cdecl; abstract;
    function getValue: PAnsiChar; virtual; cdecl; abstract;
    function getSubConfig: TConfig; virtual; cdecl; abstract;
  end;

  // Generic form of access to configuration file - find specific entry in it
  TConfig = class(TRefCounted)
  public
    function find(const name: PAnsiChar): TConfigEntry; virtual; cdecl; abstract;
    function findValue(const name: PAnsiChar; const value: PAnsiChar): TConfigEntry; virtual; cdecl; abstract;
    function findPos(const name: PAnsiChar; pos: Cardinal): TConfigEntry; virtual; cdecl; abstract;
  end;

  // Used to access config values from firebird.conf (may be DB specific)
  TFirebirdConf = class(TRefCounted)
  public
    // Get integer key by it's name
    // Value ~0 means name is invalid
    // Keys are stable: one can use once obtained key in other instances of this interface
    function getKey(const name: PAnsiChar): Cardinal; virtual; cdecl; abstract;
    // Use to access integer and boolean values
    function asInteger(key: Cardinal): Integer; virtual; cdecl; abstract;
    // Use to access string values
    function asString(key: Cardinal): PAnsiChar; virtual; cdecl; abstract;
  end;

  // This interface is passed to plugin's factory as it's single parameter
  // and contains methods to access specific plugin's configuration data
  TPluginConfig = class(TRefCounted)
  public
    function getConfigFileName: PAnsiChar; virtual; cdecl; abstract;
    function getDefaultConfig: TConfig; virtual; cdecl; abstract;
    function getFirebirdConf: TFirebirdConf; virtual; cdecl; abstract;
  end;

  // Required to creat instances of given plugin
  TPluginFactory = class(TVersioned)
  public
  	function createPlugin(factoryParameter: TPluginConfig): TPluginBase; virtual; cdecl; abstract;
  end;

  // Required to let plugins manager invoke module's cleanup routine before unloading it.
  // For some OS/compiler this may be done in dtor of global variable in module itself.
  // Others (Windows/VC) fail to create some very useful resources (threads) when module is unloading.
  TPluginModule = class(TVersioned)
  public
    procedure doClean; virtual; cdecl; abstract;
  end;

  // Interface to deal with plugins here and there, returned by master interface
  TPluginManager = class(TVersioned)
  public
    // Main function called by plugin modules in firebird_plugin()
    procedure registerPluginFactory(interfaceType: Cardinal;
      const defaultName: PAnsiChar; factory: TPluginFactory); virtual; cdecl; abstract;
    // Sets cleanup for plugin module
    // Pay attention - this should be called at plugin-register time!
    // Only at this moment manager knows, which module sets his cleanup
    procedure registerModule(cleanup: TPluginModule); virtual; cdecl; abstract;
    // Remove registered module before cleanup routine.
    // This method must be called by module which detects that it's unloaded,
    // but not notified prior to it by PluginManager via IPluginModule.
    procedure unregisterModule(cleanup: TPluginModule); virtual; cdecl; abstract;
    // Main function called to access plugins registered in plugins manager
    // Has front-end in GetPlugins.h - template GetPlugins
    // In namesList parameter comma or space separated list of names of configured plugins is passed
    // missingFunctionClass is used to add functions "notImplemented" to the end of vtable
    // in case when plugin's version is less than desired
    // If caller already has an interface for firebird.conf, it may be passed here
    // If parameter is missing, plugins will get access to default (non database specific) config
    function getPlugins(interfaceType: Cardinal; const namesList: PAnsiChar; desiredVersion: Integer;
      ui: PUpgradeInfo; firebirdConf: TFirebirdConf): TPluginSet; virtual; cdecl; abstract;
    // Get generic config interface for given file
    function getConfig(const filename: PAnsiChar): TConfig; virtual; cdecl; abstract;
    // Plugins must be released using this function - use of plugin's release()
    // will cause resources leak
    procedure releasePlugin(plugin: TPluginBase); virtual; cdecl; abstract;
  end;

  TPluginEntrypoint = procedure(masterInterface: TMaster); cdecl;

(*******************************************************************************
 *	MODULE:			YValvefirebird/Interface.h
 *	DESCRIPTION:	Interfaces, used by yValve
 *
 *******************************************************************************)

  PFbMessage = ^TFbMessage;
  TFbMessage = record
    blr: PAnsiChar;
    buffer: PAnsiChar;
    blrLength: Cardinal;
    bufferLength: Cardinal;
  end;

  TEventCallback = class(TVersioned)
  public
    procedure eventCallbackFunction(length: Cardinal; const events: PAnsiChar); virtual; cdecl; abstract;
  end;

  TBlob = class(TRefCounted)
  public
    procedure getInfo(status: TStatus; itemsLength: Cardinal; const items: PAnsiChar;
      bufferLength: Cardinal; buffer: PAnsiChar); virtual; cdecl; abstract;
    function getSegment(status: TStatus; length: Cardinal; buffer: Pointer): Cardinal; virtual; cdecl; abstract; { returns real lenght }
    procedure putSegment(status: TStatus; length: Cardinal; const buffer: Pointer); virtual; cdecl; abstract;
    procedure cancel(status: TStatus); virtual; cdecl; abstract;
    procedure close(status: TStatus); virtual; cdecl; abstract;
    function seek(status: TStatus; mode: Integer; offset: Integer): Integer; virtual; cdecl; abstract; { returns position }
  end;

  TTransaction = class(TRefCounted)
  public
    procedure getInfo(status: TStatus; itemsLength: Cardinal; const items: PAnsiChar; bufferLength: Cardinal; buffer: PAnsiChar); virtual; cdecl; abstract;
    procedure prepare(status: TStatus; msgLength: Cardinal; const msg: PAnsiChar); virtual; cdecl; abstract;
    procedure commit(status: TStatus); virtual; cdecl; abstract;
    procedure commitRetaining(status: TStatus); virtual; cdecl; abstract;
    procedure rollback(status: TStatus); virtual; cdecl; abstract;
    procedure rollbackRetaining(status: TStatus); virtual; cdecl; abstract;
    procedure disconnect(status: TStatus); virtual; cdecl; abstract;
    function join(status: TStatus; transaction: TTransaction): TTransaction; virtual; cdecl; abstract;
    function validate(status: TStatus; attachment: TAttachment): TTransaction; virtual; cdecl; abstract;
    function enterDtc(status: TStatus): TTransaction; virtual; cdecl; abstract;
  end;

  TParametersMetadata = class(TVersioned)
  public
    function getCount(status: TStatus): Cardinal; virtual; cdecl; abstract;
    function getField(status: TStatus; index: Cardinal): PAnsiChar; virtual; cdecl; abstract;
    function getRelation(status: TStatus; index: Cardinal): PAnsiChar; virtual; cdecl; abstract;
    function getOwner(status: TStatus; index: Cardinal): PAnsiChar; virtual; cdecl; abstract;
    function getAlias(status: TStatus; index: Cardinal): PAnsiChar; virtual; cdecl; abstract;
    function getType(status: TStatus; index: Cardinal): Cardinal; virtual; cdecl; abstract;
    function isNullable(status: TStatus; index: Cardinal): Integer; virtual; cdecl; abstract;
    function getSubType(status: TStatus; index: Cardinal): Cardinal; virtual; cdecl; abstract;
    function getLength(status: TStatus; index: Cardinal): Cardinal; virtual; cdecl; abstract;
    function getScale(status: TStatus; index: Cardinal): Cardinal; virtual; cdecl; abstract;
  end;

  TStatement = class(TRefCounted)
  public
    procedure prepare(status: TStatus; transaction: TTransaction;
      stmtLength: Cardinal; const sqlStmt: PAnsiChar; dialect: Cardinal;
      flags: Cardinal); virtual; cdecl; abstract;
    procedure getInfo(status: TStatus; itemsLength: Cardinal; const items: PAnsiChar;
      bufferLength: Cardinal; buffer: PAnsiChar); virtual; cdecl; abstract;
    function getType(status: TStatus): Cardinal; virtual; cdecl; abstract;
    function getPlan(status: TStatus; detailed: LongBool): PAnsiChar; virtual; cdecl; abstract;
    function getInputParameters(status: TStatus): TParametersMetadata; virtual; cdecl; abstract;
    function getOutputParameters(status: TStatus): TParametersMetadata; virtual; cdecl; abstract;
    function getAffectedRecords(status: TStatus): Int64; virtual; cdecl; abstract;
    procedure setCursorName(status: TStatus; const name: PAnsiChar); virtual; cdecl; abstract;
    function execute(status: TStatus; transaction: TTransaction; inMsgType: Cardinal;
      const inMsgBuffer: PFbMessage; const outMsgBuffer: PFbMessage): TTransaction; virtual; cdecl; abstract;
    function fetch(status: TStatus; const msgBuffer: PFbMessage): Integer; virtual; cdecl; abstract; { returns 100 if EOF, 101 if fragmented }
    procedure insert(status: TStatus; const msgBuffer: PFbMessage); virtual; cdecl; abstract;
    procedure free(status: TStatus; option: Cardinal); virtual; cdecl; abstract;
  end;

  TRequest = class(TRefCounted)
  public
    procedure receive(status: TStatus; level: Integer; msgType: Cardinal;
      length: Cardinal; const msg: PAnsiChar); virtual; cdecl; abstract;
    procedure send(status: TStatus; level: Integer; msgType: Cardinal;
      length: Cardinal; const msg: PAnsiChar); virtual; cdecl; abstract;
    procedure getInfo(status: TStatus; level: Integer; itemsLength: Cardinal;
      const items: PAnsiChar; bufferLength: Cardinal; buffer: PAnsiChar); virtual; cdecl; abstract;
    procedure start(status: TStatus; transaction: TTransaction; level: Integer); virtual; cdecl; abstract;
    procedure startAndSend(status: TStatus; transaction: TTransaction; level: Integer;
      msgType: Cardinal; length: Cardinal; const msg: PAnsiChar); virtual; cdecl; abstract;
    procedure unwind(status: TStatus; level: Integer); virtual; cdecl; abstract;
    procedure free(status: TStatus); virtual; cdecl; abstract;
  end;

  TEvents = class(TRefCounted)
  public
    procedure cancel(status: TStatus); virtual; cdecl; abstract;
  end;

  TAttachment = class(TRefCounted)
  public
    procedure getInfo(status: TStatus; itemsLength: Cardinal; const items: PAnsiChar;
      bufferLength: Cardinal; buffer: PAnsiChar); virtual; cdecl; abstract;
    function  startTransaction(status: TStatus; tpbLength: Cardinal;
      const tpb: PAnsiChar): TTransaction; virtual; cdecl; abstract;
    function reconnectTransaction(status: TStatus; length: Cardinal;
      const id: PAnsiChar): TTransaction; virtual; cdecl; abstract;
    function allocateStatement(status: TStatus): TStatement; virtual; cdecl; abstract;
    function compileRequest(status: TStatus; blrLength: Cardinal;
      const blr: PAnsiChar): TRequest; virtual; cdecl; abstract;
    procedure transactRequest(status: TStatus; transaction: TTransaction;
      blrLength: Cardinal; const blr: PAnsiChar; inMsgLength: Cardinal;
      const inMsg: PAnsiChar; outMsgLength: Cardinal; outMsg: PAnsiChar); virtual; cdecl; abstract;
    function createBlob(status: TStatus; transaction: TTransaction; id: PISCQuad;
      bpbLength: Cardinal; const bpb: PAnsiChar): TBlob; virtual; cdecl; abstract;
    function openBlob(status: TStatus; transaction: TTransaction; id: PISCQuad;
      bpbLength: Cardinal; const bpb: PAnsiChar): TBlob; virtual; cdecl; abstract;
    function getSlice(status: TStatus; transaction: TTransaction; id: PISCQuad;
      sdlLength: Cardinal; const sdl: PAnsiChar; paramLength: Cardinal;
      const param: PAnsiChar; sliceLength: Integer; slice: PAnsiChar): Integer; virtual; cdecl; abstract;
    procedure putSlice(status: TStatus; transaction: TTransaction; id: PISCQuad;
      sdlLength: Cardinal; const sdl: PAnsiChar; paramLength: Cardinal;
      const param: PAnsiChar; sliceLength: Integer; slice: PAnsiChar); virtual; cdecl; abstract;
    procedure ddl(status: TStatus; transaction: TTransaction; length: Cardinal;
      const dyn: PAnsiChar); virtual; cdecl; abstract;
    function execute(status: TStatus; transaction: TTransaction; length: Cardinal;
      const sql: PAnsiChar; dialect: Cardinal; inMsgType: Cardinal;
      const inMsgBuffer: PFbMessage; const outMsgBuffer: PFbMessage): TTransaction; virtual; cdecl; abstract;
    function queEvents(status: TStatus; callback: TEventCallback; length: Cardinal;
      const events: PAnsiChar): TEvents; virtual; cdecl; abstract;
    procedure cancelOperation(status: TStatus; option: Integer); virtual; cdecl; abstract;
    procedure ping(status: TStatus); virtual; cdecl; abstract;
    procedure detach(status: TStatus); virtual; cdecl; abstract;
    procedure drop(status: TStatus); virtual; cdecl; abstract;
  end;

  TService = class(TRefCounted)
  public
    procedure detach(status: TStatus); virtual; cdecl; abstract;
    procedure query(status: TStatus; sendLength: Cardinal; const sendItems: PAnsiChar;
       receiveLength: Cardinal; const receiveItems: PAnsiChar; bufferLength: Cardinal;
       buffer: PAnsiChar); virtual; cdecl; abstract;
    procedure start(status: TStatus; spbLength: Cardinal; const spb: PAnsiChar); virtual; cdecl; abstract;
  end;

  TProvider = class(TPluginBase)
  public
    function attachDatabase(status: TStatus; const fileName: PAnsiChar; dpbLength: Cardinal; const dpb: PAnsiChar): TAttachment; virtual; cdecl; abstract;
    function createDatabase(status: TStatus; const fileName: PAnsiChar; dpbLength: Cardinal; const dpb: PAnsiChar): TAttachment; virtual; cdecl; abstract;
    function attachServiceManager(status: TStatus; const service: PAnsiChar; spbLength: Cardinal; const spb: PAnsiChar): TService; virtual; cdecl; abstract;
    procedure shutdown(status: TStatus; timeout: Cardinal; const reason: Integer); virtual; cdecl; abstract;
  end;

  // DtcStart - structure to start transaction over >1 attachments (former TEB)
  PDtcStart = ^TDtcStart;
  TDtcStart = record
  	attachment: TAttachment;
	  tpb: PAnsiChar;
  	tpbLength: Cardinal;
  end;

  // Distributed transactions coordinator
  TDtc = class(TVersioned)
  public
	  function start(status: TStatus; cnt: Cardinal; components: PDtcStart): TTransaction; virtual; cdecl; abstract;
	  function join(status: TStatus; one, two: TTransaction): TTransaction; virtual; cdecl; abstract;
  end;

(*******************************************************************************
 *
 *	MODULE:			firebird/Timer.h
 *	DESCRIPTION:	Timer interface defnition.
 *
 *******************************************************************************)

  // Identifies particular timer.
  // Callback function is invoked when timer fires.
  TTimer = class(TRefCounted)
  public
  	procedure handler; virtual; cdecl; abstract;
  end;

  TTimerDelay = ISC_INT64;

  // Interface to set timer for particular time
  TTimerControl = class(TVersioned)
  public
    // Set timer
    procedure start(timer: TTimer; microSeconds: TTimerDelay); virtual; cdecl; abstract;
    // Stop timer
    procedure stop(timer: TTimer); virtual; cdecl; abstract;
  end;

(*******************************************************************************
 *
 *	MODULE:			firebird/ExternalEngine.h
 *	DESCRIPTION:
 *
 *******************************************************************************)

  TExternalEngine = class;

  PBlrMessage = ^TBlrMessage;
  TBlrMessage = record
  	blr: PAnsiChar;
	  blrLength: Cardinal;
  	bufferLength: Cardinal;
  end;

  // Connection to current database in external engine.
  // Context passed to ExternalEngine has SYSDBA privileges.
  // Context passed to ExternalFunction, ExternalProcedure and ExternalTrigger
  // has user privileges.
  // There is one ExternalContext per attachment. The privileges and character
  // set properties are changed during the calls.
  TExternalContext = class
  public
    // Gets the ExternalEngine associated with this context.
    function getEngine(error: TError): TExternalEngine; virtual; cdecl; abstract;

    // Gets the Attachment associated with this context.
    function getAttachment(error: TError): TAttachment; virtual; cdecl; abstract;

    // Obtained transaction is valid only before control is returned to the engine
    // or in ExternalResultSet::fetch calls of correspondent ExternalProcedure::open.
    function getTransaction(error: TError): TTransaction; virtual; cdecl; abstract;

    function getUserName: PAnsiChar; virtual; cdecl; abstract;
    function getDatabaseName: PAnsiChar; virtual; cdecl; abstract;

    // Get user attachment character set.
    function getClientCharSet: PUtf8; virtual; cdecl; abstract;

    // Misc info associated with a context. The pointers are never accessed or freed by Firebird.

    // Obtains an unique (across all contexts) code to associate plugin and/or user information.
    function obtainInfoCode: Integer; virtual; cdecl; abstract;
    // Gets a value associated with this code or FB_NULL if no value was set.
    function getInfo(code: Integer): Pointer; virtual; cdecl; abstract;
    // Sets a value associated with this code and returns the last value.
    function setInfo(code: Integer; value: Pointer): Pointer; virtual; cdecl; abstract;
  end;


  // To return set of rows in selectable procedures.
  TExternalResultSet = class(TDisposable)
  public
    function fetch(error: TError): Boolean; virtual; cdecl; abstract;
  end;


  TExternalFunction = class(TDisposable)
  public
    // This method is called just before execute and informs the engine our requested character
    // set for data exchange inside that method.
    // During this call, the context uses the character set obtained from ExternalEngine::getCharSet.
    procedure getCharSet(error: TError; context: TExternalContext;
      name: PUtf8; nameSize: Cardinal); virtual; cdecl; abstract;

    procedure execute(error: TError; context: TExternalContext;
      inMsg, outMsg: Pointer); virtual; cdecl; abstract;
  end;


  TExternalProcedure = class(TDisposable)
  public
    // This method is called just before open and informs the engine our requested character
    // set for data exchange inside that method and ExternalResultSet::fetch.
    // During this call, the context uses the character set obtained from ExternalEngine::getCharSet.
    procedure getCharSet(error: TError; context: TExternalContext;
      name: PUtf8; nameSize: Cardinal); virtual; cdecl; abstract;

    // Returns a ExternalResultSet for selectable procedures.
    // Returning NULL results in a result set of one record.
    // Procedures without output parameters should return NULL.
    function open(error: TError; context: TExternalContext;
      inMsg, outMsg: Pointer): TExternalResultSet; virtual; cdecl; abstract;
  end;

  TExternalTriggerType = (
    TYPE_BEFORE = 1,
    TYPE_AFTER,
    TYPE_DATABASE
  );

  TExternalTriggerAction = (
    ACTION_INSERT = 1,
    ACTION_UPDATE,
    ACTION_DELETE,
    ACTION_CONNECT,
    ACTION_DISCONNECT,
    ACTION_TRANS_START,
    ACTION_TRANS_COMMIT,
    ACTION_TRANS_ROLLBACK,
    ACTION_DDL
  );

  TExternalTrigger = class(TDisposable)
  public
    // This method is called just before execute and informs the engine our requested character
    // set for data exchange inside that method.
    // During this call, the context uses the character set obtained from ExternalEngine::getCharSet.
    procedure getCharSet(error: TError; context: TExternalContext;
      name: PUtf8; nameSize: Cardinal); virtual; cdecl; abstract;

    procedure execute(error: TError; context: TExternalContext;
      action: TExternalTriggerAction; oldMsg, newMsg: Pointer); virtual; cdecl; abstract;
  end;

  TRoutineMetadata = class(TVersioned)
  public
    function getPackage(status: TStatus): PAnsiChar; virtual; cdecl; abstract;
    function getName(status: TStatus): PAnsiChar; virtual; cdecl; abstract;
    function getEntryPoint(status: TStatus): PAnsiChar; virtual; cdecl; abstract;
    function getBody(status: TStatus): PAnsiChar; virtual; cdecl; abstract;
    function getInputParameters(status: TStatus): TParametersMetadata; virtual; cdecl; abstract;
    function getOutputParameters(status: TStatus): TParametersMetadata; virtual; cdecl; abstract;
    function getTriggerFields(status: TStatus): TParametersMetadata; virtual; cdecl; abstract;
    function getTriggerTable(status: TStatus): PAnsiChar; virtual; cdecl; abstract;
    function getTriggerType(status: TStatus): TExternalTriggerType; virtual; cdecl; abstract;
  end;

  // In SuperServer, shared by all attachments to one database and disposed when last (non-external)
  // user attachment to the database is closed.
  TExternalEngine = class(TPluginBase)
  public
    // This method is called once (per ExternalEngine instance) before any following methods.
    // The requested character set for data exchange inside methods of this interface should
    // be copied to charSet parameter.
    // During this call, the context uses the UTF-8 character set.
    procedure open(error: TError; context: TExternalContext;
      charSet: PUtf8; charSetSize: Cardinal); virtual; cdecl; abstract;

    // Attachment is being opened.
    procedure openAttachment(error: TError; context: TExternalContext); virtual; cdecl; abstract;

    // Attachment is being closed.
    procedure closeAttachment(error: TError; context: TExternalContext); virtual; cdecl; abstract;

    // Called when engine wants to load object in the cache. Objects are disposed when
    // going out of the cache.
    function makeFunction(error: TError; context: TExternalContext;
      const metadata: TRoutineMetadata; inBlr, outBlr: PBlrMessage): TExternalFunction; virtual; cdecl; abstract;
    function makeProcedure(error: TError; context: TExternalContext;
      const metadata: TRoutineMetadata; inBlr, outBlr: PBlrMessage): TExternalProcedure; virtual; cdecl; abstract;
    function makeTrigger(error: TError; context: TExternalContext;
      const metadata: TRoutineMetadata): TExternalTrigger; virtual; cdecl; abstract;
  end;

(*******************************************************************************
 *
 *	MODULE:			firebird/UdrEngine.h
 *	DESCRIPTION:
 *
 *******************************************************************************)


  // Factory classes. They should be singletons instances created by user's modules and
  // registered. When UDR engine is going to load a routine, it calls newItem.

  TFunctionFactory = class
  public
    procedure setup(const metadata: TRoutineMetadata; inBlr, outBlr: PBlrMessage); virtual; cdecl; abstract;
	  function newItem(const metadata: TRoutineMetadata): TExternalFunction; virtual; cdecl; abstract;
  end;

  TProcedureFactory = class
  public
    procedure setup(const metadata: TRoutineMetadata; inBlr, outBlr: PBlrMessage); virtual; cdecl; abstract;
    function newItem(const metadata: TRoutineMetadata): TExternalProcedure; virtual; cdecl; abstract;
  end;

  TTriggerFactory = class
  public
    procedure setup(const metadata: TRoutineMetadata); virtual; cdecl; abstract;
    function newItem(const metadata: TRoutineMetadata): TExternalTrigger; virtual; cdecl; abstract;
  end;


  // Routine registration functions.
  TfbUdrRegFunction = procedure(const name: PAnsiChar; factory: TFunctionFactory); cdecl;
  TfbUdrRegProcedure = procedure(const name: PAnsiChar; factory: TProcedureFactory); cdecl;
  TfbUdrRegTrigger = procedure(const name: PAnsiChar; factory: TTriggerFactory); cdecl;


  function GetClientMasterInterface: TMaster;
var
  FBLibrary: string = 'fbclient.dll';

implementation
uses Windows;

type
  TFBGetMasterInterface = function(): TMaster; {$IFDEF UNIX} cdecl; {$ELSE} stdcall; {$ENDIF}

var
  FBLib: THandle = 0;

function GetClientMasterInterface: TMaster;
var
  get: TFBGetMasterInterface;
begin
  Result := nil;
  if FBLib = 0 then
  begin
    FBLib := LoadLibrary(PChar(FBLibrary));
    if FBLib <= HINSTANCE_ERROR then Exit;
  end;
  get := GetProcAddress(FBLib, 'fb_get_master_interface');
  if not Assigned(get) then Exit;
  Result := get();
end;

initialization
finalization
  if FBLib > HINSTANCE_ERROR then
    FreeLibrary(FBLib);
end.
