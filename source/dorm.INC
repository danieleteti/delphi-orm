{ *************************************************** }
{ This file is for Global Compiler Defines for DORM }
{ *************************************************** }

{ ****************** start adapters section ********************* }
{ These defines control if a specified adapter will be
  linked into Sample Projects, Unit Tests and all your dorm projects
  To enable the linking of a specified adapter
  then remove the period before the $ that makes the
  define a comment.
}
{.$DEFINE LINK_FIREBIRDUIB_ADAPTER }
{$DEFINE LINK_FIREBIRDZEOSDBO_ADAPTER }
{.$DEFINE LINK_INTERBASEUIB_ADAPTER }
{.$DEFINE LINK_SQLITE3_ADAPTER }
{.$DEFINE LINK_SQLSERVER_ADAPTER } // require delphi ent or better
// require DevArt dbExpress driver for sqlserver
{ .$DEFINE LINK_SQLSERVERDEVART_ADAPTER }
{ . $DEFINE LINK_FIREBIRD_ADAPTER }
// require delphi ent or better (deprecated, do not use. Use LINK_FIREBIRDUIB_ADAPTER )
{ .$DEFINE LINK_INTERBASE_ADAPTER }
// require delphi ent or better (deprecated, do not use. Use LINK_INTERBASEUIB_ADAPTER)

// requires FireDac for sqlserver
{.$DEFINE LINK_SQLSERVER_FIREDAC_ADAPTER }

{ **************** end adapters section ************************* }

{ ****************** start loggers section ********************* }
{ These defines control if a specified logger will be
  linked into Sample Projects, Unit Tests and all you dorm projects.
  To enable the linking of a specified logger
  then remove the period before the $ that makes the
  define a comment.
}
{ .$DEFINE LINK_CODESITE }
{ .$DEFINE LINK_SMARTINSPECT }
{$DEFINE LINK_FILELOG }
{ **************** end loggers section ************************* }
