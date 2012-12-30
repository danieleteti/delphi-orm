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
{$DEFINE LINK_FIREBIRDUIB_ADAPTER }
{$DEFINE LINK_INTERBASEUIB_ADAPTER }
{$DEFINE LINK_SQLITE3_ADAPTER}

// requires delphi ent or better - NOT YET SUPPORTED
{ . $DEFINE LINK_SQLSERVER_ADAPTER }

// requires DevArt dbExpress driver for sqlserver - NOT YET SUPPORTED
{ . $DEFINE LINK_SQLSERVERDEVART_ADAPTER }

{ **************** end adapters section ************************* }





{ ****************** start loggers section ********************* }
{ These defines control if a specified logger will be
  linked into Sample Projects, Unit Tests and all you dorm projects.
  To enable the linking of a specified logger
  then remove the period before the $ that makes the
  define a comment.
}
{$DEFINE LINK_CODESITE }
{ .$DEFINE LINK_SMARTINSPECT }
{$DEFINE LINK_FILELOG }
{ **************** end loggers section ************************* }
