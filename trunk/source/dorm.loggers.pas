unit dorm.loggers;

interface

{$I DORM.INC}


uses
  SysUtils
{$IFDEF LINK_CODESITE}
  , dorm.loggers.CodeSite
{$ENDIF}
{$IFDEF LINK_SMARTINSPECT}
  , dorm.loggers.SmartInspect
{$ENDIF}
{$IFDEF LINK_FILELOG}
  , dorm.loggers.FileLog
{$ENDIF};

implementation

end.
