{                                                                           }
{ File:       Console.pas                                                   }
{ Function:   Console unit, similar to the Crt unit in Turbo Pascal.        }
{ Language:   Delphi 5 and above                                            }
{ Author:     Rudolph Velthuis                                              }
{ Copyright:  (c) 2006,2008 Rudy Velthuis                                   }
{ Disclaimer: This code is freeware. All rights are reserved.               }
{             This code is provided as is, expressly without a warranty     }
{             of any kind. You use it at your own risk.                     }
{                                                                           }
{             If you use this code, please credit me.                       }
{                                                                           }

unit Console;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 17.0}
    {$DEFINE INLINES}
  {$IFEND}
  {$IF RTLVersion >= 14.0}
    {$DEFINE HASERROUTPUT}
  {$IFEND}
{$ENDIF}

interface

uses Windows;

const
  // Background and foreground colors
  Black        = 0;
  Blue         = 1;
  Green        = 2;
  Cyan         = 3;
  Red          = 4;
  Magenta      = 5;
  Brown        = 6;
  LightGray    = 7;

  // Foreground colors
  DarkGray     = 8;          
  LightBlue    = 9;
  LightGreen   = 10;
  LightCyan    = 11;
  LightRed     = 12;
  LightMagenta = 13;
  Yellow       = 14;
  White        = 15;

  // Blink attribute, to be or-ed with background colors.
  Blink        = 128;

  // Text modes:
  BW40         = 0;      // 40x25 B/W on Color Adapter
  CO40         = 1;      // 40x25 Color on Color Adapter
  BW80         = 2;      // 80x25 B/W on Color Adapter
  CO80         = 3;      // 80x25 Color on Color Adapter
  Mono         = 7;      // 80x25 on Monochrome Adapter
  Font8x8      = 256;    // Add-in for ROM font

  // Mode constants for 3.0 compatibility of original CRT unit }
  C40          = CO40;
  C80          = CO80;


// Turbo/Borland Pascal Crt routines:

// Waits for keypress and returns the key pressed. If the key is not an ASCII
// key, #0 is returned, and a successive ReadKey will give the extended key
// code of the key.
function ReadKey: Char;

// Checks whether a key was pressed.
function KeyPressed: Boolean;

// Puts the cursor at the given coordinates on the screen.
procedure GotoXY(X, Y: Smallint);

// Returns the current X position of the cursor.
function WhereX: Integer;

// Returns the current Y position of the cursor.
function WhereY: Integer;

// Sets text foreground color.
procedure TextColor(Color: Byte); overload;

// Gets text forground color.
function TextColor: Byte; overload;

// Sets text background color.
procedure TextBackground(Color: Byte); overload;

// Gets text background color.
function TextBackground: Byte; overload;

// Sets text mode.
procedure TextMode(Mode: Word);

// Sets text colors to low intensity
procedure LowVideo;

// Sets text colors to high intensity
procedure HighVideo;

// Sets text attribute to value at startup.
procedure NormVideo;

// Clears the entire screen, or, if a window is set, the entire window,
// in the current background color.
procedure ClrScr;

// Clears until the end of the line, in the current background color.
procedure ClrEol;

// Inserts a line at the current cursor position.
procedure InsLine;

// Deletes the line at the current cursor position.
procedure DelLine;

// Sets a window, into which all successive output will go. You can reset the
// window to full screen by calling Window with a zero or negative value
// for Left.
procedure Window(Left, Top, Right, Bottom: Integer);

type
  // Plays a sound at the given frequency (in Herz).
  TSoundProc = procedure(Frequency: Smallint);

  // Stops the sound started with Sound.
  TNoSoundProc = procedure;

  // Delays for the given amount of milliseconds, or as close as possible.
  TDelayProc = procedure(Millisecs: Integer);

  // Plays a sound at the given frequency (in Hz) and duration (in ms).
  TBeepProc = procedure(Frequency, Duration: Smallint);

var
  Sound: TSoundProc;
  NoSound: TNoSoundProc;
  Delay: TDelayProc;
  Beep: TBeepProc;

// Additional routines:

function ScreenWidth: Smallint;
function ScreenHeight: Smallint;
function BufferWidth: Smallint;
function BufferHeight: Smallint;

var
  TextWindow: TSmallRect;
  TextAttr: Byte;
  DefaultAttr: Byte;
  ScreenMode: Byte;
  BufferSize: TCoord;
  ScreenSize: TCoord;
  StdIn, StdOut: THandle;
  StdErr: THandle;
  LastMode: Word;
  WindMin: Word;
  WindMax: Word;
  CheckBreak: Boolean;

implementation

uses SysUtils;

type
  PKey = ^TKey;
  TKey = record
    KeyCode: Smallint;
    Normal: Smallint;
    Shift: Smallint;
    Ctrl: Smallint;
    Alt: Smallint;
  end;

const
  CKeys: array[0..88] of TKey = (
    (KeyCode: VK_BACK;     Normal: $8;        Shift: $8;       Ctrl: $7F;  Alt: $10E; ),
    (KeyCode: VK_TAB;      Normal: $9;        Shift: $10F;     Ctrl: $194; Alt: $1A5; ),
    (KeyCode: VK_RETURN;   Normal: $D;        Shift: $D;       Ctrl: $A;   Alt: $1A6),
    (KeyCode: VK_ESCAPE;   Normal: $1B;       Shift: $1B;      Ctrl: $1B;  Alt: $101),
    (KeyCode: VK_SPACE;    Normal: $20;       Shift: $20;      Ctrl: $103; Alt: $20),
    (KeyCode: Ord('0');    Normal: Ord('0');  Shift: Ord(')'); Ctrl: - 1;  Alt: $181),
    (KeyCode: Ord('1');    Normal: Ord('1');  Shift: Ord('!'); Ctrl: - 1;  Alt: $178),
    (KeyCode: Ord('2');    Normal: Ord('2');  Shift: Ord('@'); Ctrl: $103; Alt: $179),
    (KeyCode: Ord('3');    Normal: Ord('3');  Shift: Ord('#'); Ctrl: - 1;  Alt: $17A),
    (KeyCode: Ord('4');    Normal: Ord('4');  Shift: Ord('$'); Ctrl: - 1;  Alt: $17B),
    (KeyCode: Ord('5');    Normal: Ord('5');  Shift: Ord('%'); Ctrl: - 1;  Alt: $17C),
    (KeyCode: Ord('6');    Normal: Ord('6');  Shift: Ord('^'); Ctrl: $1E;  Alt: $17D),
    (KeyCode: Ord('7');    Normal: Ord('7');  Shift: Ord('&'); Ctrl: - 1;  Alt: $17E),
    (KeyCode: Ord('8');    Normal: Ord('8');  Shift: Ord('*'); Ctrl: - 1;  Alt: $17F),
    (KeyCode: Ord('9');    Normal: Ord('9');  Shift: Ord('('); Ctrl: - 1;  Alt: $180),
    (KeyCode: Ord('A');    Normal: Ord('a');  Shift: Ord('A'); Ctrl: $1;   Alt: $11E),
    (KeyCode: Ord('B');    Normal: Ord('b');  Shift: Ord('B'); Ctrl: $2;   Alt: $130),
    (KeyCode: Ord('C');    Normal: Ord('c');  Shift: Ord('C'); Ctrl: $3;   Alt: $12E),
    (KeyCode: Ord('D');    Normal: Ord('d');  Shift: Ord('D'); Ctrl: $4;   Alt: $120),
    (KeyCode: Ord('E');    Normal: Ord('e');  Shift: Ord('E'); Ctrl: $5;   Alt: $112),
    (KeyCode: Ord('F');    Normal: Ord('f');  Shift: Ord('F'); Ctrl: $6;   Alt: $121),
    (KeyCode: Ord('G');    Normal: Ord('g');  Shift: Ord('G'); Ctrl: $7;   Alt: $122),
    (KeyCode: Ord('H');    Normal: Ord('h');  Shift: Ord('H'); Ctrl: $8;   Alt: $123),
    (KeyCode: Ord('I');    Normal: Ord('i');  Shift: Ord('I'); Ctrl: $9;   Alt: $117),
    (KeyCode: Ord('J');    Normal: Ord('j');  Shift: Ord('J'); Ctrl: $A;   Alt: $124),
    (KeyCode: Ord('K');    Normal: Ord('k');  Shift: Ord('K'); Ctrl: $B;   Alt: $125),
    (KeyCode: Ord('L');    Normal: Ord('l');  Shift: Ord('L'); Ctrl: $C;   Alt: $126),
    (KeyCode: Ord('M');    Normal: Ord('m');  Shift: Ord('M'); Ctrl: $D;   Alt: $132),
    (KeyCode: Ord('N');    Normal: Ord('n');  Shift: Ord('N'); Ctrl: $E;   Alt: $131),
    (KeyCode: Ord('O');    Normal: Ord('o');  Shift: Ord('O'); Ctrl: $F;   Alt: $118),
    (KeyCode: Ord('P');    Normal: Ord('p');  Shift: Ord('P'); Ctrl: $10;  Alt: $119),
    (KeyCode: Ord('Q');    Normal: Ord('q');  Shift: Ord('Q'); Ctrl: $11;  Alt: $110),
    (KeyCode: Ord('R');    Normal: Ord('r');  Shift: Ord('R'); Ctrl: $12;  Alt: $113),
    (KeyCode: Ord('S');    Normal: Ord('s');  Shift: Ord('S'); Ctrl: $13;  Alt: $11F),
    (KeyCode: Ord('T');    Normal: Ord('t');  Shift: Ord('T'); Ctrl: $14;  Alt: $114),
    (KeyCode: Ord('U');    Normal: Ord('u');  Shift: Ord('U'); Ctrl: $15;  Alt: $116),
    (KeyCode: Ord('V');    Normal: Ord('v');  Shift: Ord('V'); Ctrl: $16;  Alt: $12F),
    (KeyCode: Ord('W');    Normal: Ord('w');  Shift: Ord('W'); Ctrl: $17;  Alt: $111),
    (KeyCode: Ord('X');    Normal: Ord('x');  Shift: Ord('X'); Ctrl: $18;  Alt: $12D),
    (KeyCode: Ord('Y');    Normal: Ord('y');  Shift: Ord('Y'); Ctrl: $19;  Alt: $115),
    (KeyCode: Ord('Z');    Normal: Ord('z');  Shift: Ord('Z'); Ctrl: $1A;  Alt: $12C),
    (KeyCode: VK_PRIOR;    Normal: $149;      Shift: $149;     Ctrl: $184; Alt: $199),
    (KeyCode: VK_NEXT;     Normal: $151;      Shift: $151;     Ctrl: $176; Alt: $1A1),
    (KeyCode: VK_END;      Normal: $14F;      Shift: $14F;     Ctrl: $175; Alt: $19F),
    (KeyCode: VK_HOME;     Normal: $147;      Shift: $147;     Ctrl: $177; Alt: $197),
    (KeyCode: VK_LEFT;     Normal: $14B;      Shift: $14B;     Ctrl: $173; Alt: $19B),
    (KeyCode: VK_UP;       Normal: $148;      Shift: $148;     Ctrl: $18D; Alt: $198),
    (KeyCode: VK_RIGHT;    Normal: $14D;      Shift: $14D;     Ctrl: $174; Alt: $19D),
    (KeyCode: VK_DOWN;     Normal: $150;      Shift: $150;     Ctrl: $191; Alt: $1A0),
    (KeyCode: VK_INSERT;   Normal: $152;      Shift: $152;     Ctrl: $192; Alt: $1A2),
    (KeyCode: VK_DELETE;   Normal: $153;      Shift: $153;     Ctrl: $193; Alt: $1A3),
    (KeyCode: VK_NUMPAD0;  Normal: Ord('0');  Shift: $152;     Ctrl: $192; Alt: - 1),
    (KeyCode: VK_NUMPAD1;  Normal: Ord('1');  Shift: $14F;     Ctrl: $175; Alt: - 1),
    (KeyCode: VK_NUMPAD2;  Normal: Ord('2');  Shift: $150;     Ctrl: $191; Alt: - 1),
    (KeyCode: VK_NUMPAD3;  Normal: Ord('3');  Shift: $151;     Ctrl: $176; Alt: - 1),
    (KeyCode: VK_NUMPAD4;  Normal: Ord('4');  Shift: $14B;     Ctrl: $173; Alt: - 1),
    (KeyCode: VK_NUMPAD5;  Normal: Ord('5');  Shift: $14C;     Ctrl: $18F; Alt: - 1),
    (KeyCode: VK_NUMPAD6;  Normal: Ord('6');  Shift: $14D;     Ctrl: $174; Alt: - 1),
    (KeyCode: VK_NUMPAD7;  Normal: Ord('7');  Shift: $147;     Ctrl: $177; Alt: - 1),
    (KeyCode: VK_NUMPAD8;  Normal: Ord('8');  Shift: $148;     Ctrl: $18D; Alt: - 1),
    (KeyCode: VK_NUMPAD9;  Normal: Ord('9');  Shift: $149;     Ctrl: $184; Alt: - 1),
    (KeyCode: VK_MULTIPLY; Normal: Ord('*');  Shift: Ord('*'); Ctrl: $196; Alt: $137),
    (KeyCode: VK_ADD;      Normal: Ord('+');  Shift: Ord('+'); Ctrl: $190; Alt: $14E),
    (KeyCode: VK_SUBTRACT; Normal: Ord('-');  Shift: Ord('-'); Ctrl: $18E; Alt: $14A),
    (KeyCode: VK_DECIMAL;  Normal: Ord('.');  Shift: Ord('.'); Ctrl: $153; Alt: $193),
    (KeyCode: VK_DIVIDE;   Normal: Ord('/');  Shift: Ord('/'); Ctrl: $195; Alt: $1A4),
    (KeyCode: VK_F1;       Normal: $13B;      Shift: $154;     Ctrl: $15E; Alt: $168),
    (KeyCode: VK_F2;       Normal: $13C;      Shift: $155;     Ctrl: $15F; Alt: $169),
    (KeyCode: VK_F3;       Normal: $13D;      Shift: $156;     Ctrl: $160; Alt: $16A),
    (KeyCode: VK_F4;       Normal: $13E;      Shift: $157;     Ctrl: $161; Alt: $16B),
    (KeyCode: VK_F5;       Normal: $13F;      Shift: $158;     Ctrl: $162; Alt: $16C),
    (KeyCode: VK_F6;       Normal: $140;      Shift: $159;     Ctrl: $163; Alt: $16D),
    (KeyCode: VK_F7;       Normal: $141;      Shift: $15A;     Ctrl: $164; Alt: $16E),
    (KeyCode: VK_F8;       Normal: $142;      Shift: $15B;     Ctrl: $165; Alt: $16F),
    (KeyCode: VK_F9;       Normal: $143;      Shift: $15C;     Ctrl: $166; Alt: $170),
    (KeyCode: VK_F10;      Normal: $144;      Shift: $15D;     Ctrl: $167; Alt: $171),
    (KeyCode: VK_F11;      Normal: $185;      Shift: $187;     Ctrl: $189; Alt: $18B),
    (KeyCode: VK_F12;      Normal: $186;      Shift: $188;     Ctrl: $18A; Alt: $18C),
    (KeyCode: $DC;         Normal: Ord('\');  Shift: Ord('|'); Ctrl: $1C;  Alt: $12B),
    (KeyCode: $BF;         Normal: Ord('/');  Shift: Ord('?'); Ctrl: - 1;  Alt: $135),
    (KeyCode: $BD;         Normal: Ord('-');  Shift: Ord('_'); Ctrl: $1F;  Alt: $182),
    (KeyCode: $BB;         Normal: Ord('=');  Shift: Ord('+'); Ctrl: - 1;  Alt: $183),
    (KeyCode: $DB;         Normal: Ord('[');  Shift: Ord('{'); Ctrl: $1B;  Alt: $11A),
    (KeyCode: $DD;         Normal: Ord(']');  Shift: Ord('}'); Ctrl: $1D;  Alt: $11B),
    (KeyCode: $BA;         Normal: Ord(';');  Shift: Ord(':'); Ctrl: - 1;  Alt: $127),
    (KeyCode: $DE;         Normal: Ord(''''); Shift: Ord('"'); Ctrl: - 1;  Alt: $128),
    (KeyCode: $BC;         Normal: Ord(',');  Shift: Ord('<'); Ctrl: - 1;  Alt: $133),
    (KeyCode: $BE;         Normal: Ord('.');  Shift: Ord('>'); Ctrl: - 1;  Alt: $134),
    (KeyCode: $C0;         Normal: Ord('`');  Shift: Ord('~'); Ctrl: - 1;  Alt: $129)
  );

var
  ExtendedChar: Char = #0;

function FindKeyCode(KeyCode: Smallint): PKey; {$IFDEF INLINES}inline;{$ENDIF}
var
  I: Integer;
begin
  for I := 0 to High(CKeys) do
    if CKeys[I].KeyCode = KeyCode then
    begin
      Result := @CKeys[I];
      Exit;
    end;
  Result := nil;
end;

// This has a complexity of 11, because of the if else ladder.
// That bugs me a bit. Looking for something more elegant.
function TranslateKey(const Rec: TInputRecord; State: Integer; Key: PKey; KeyCode: Integer): Smallint;
begin
  if State and (RIGHT_ALT_PRESSED or LEFT_ALT_PRESSED) <> 0 then
    Result := Key^.Alt
  else if State and (RIGHT_CTRL_PRESSED or LEFT_CTRL_PRESSED) <> 0 then
    Result := Key^.Ctrl
  else if State and SHIFT_PRESSED <> 0 then
    Result := Key^.Shift
  else if KeyCode in [Ord('A')..Ord('Z')] then
    Result := Ord(Rec.Event.KeyEvent.AsciiChar)
  else
    Result := Key^.Normal;
end;

function ConvertKey(const Rec: TInputRecord; Key: PKey): Smallint;
  {$IFDEF INLINES}inline;{$ENDIF}
begin
  if Assigned(Key) then
    Result := TranslateKey(Rec, Rec.Event.KeyEvent.dwControlKeyState,
      Key, Rec.Event.KeyEvent.wVirtualKeyCode)
  else
    Result := -1
end;

function ReadKey: Char;
var
  InputRec: TInputRecord;
  NumRead: Cardinal;
  KeyMode: DWORD;
  KeyCode: Smallint;
begin
  if ExtendedChar <> #0 then
  begin
    Result := ExtendedChar;
    ExtendedChar := #0;
    Exit;
  end
  else
  begin
    Result := #$FF;
    GetConsoleMode(StdIn, KeyMode);
    SetConsoleMode(StdIn, 0);
    repeat
      ReadConsoleInput(StdIn, InputRec, 1, NumRead);
      if (InputRec.EventType and KEY_EVENT <> 0) and
         InputRec.Event.KeyEvent.bKeyDown then
      begin
        if InputRec.Event.KeyEvent.AsciiChar <> #0 then
        begin
          // From Delphi 2009 on, Result is WideChar
          Result := Chr(Ord(InputRec.Event.KeyEvent.AsciiChar));
          Break;
        end;
        KeyCode := ConvertKey(InputRec,
          FindKeyCode(InputRec.Event.KeyEvent.wVirtualKeyCode));
        if KeyCode > $FF then
        begin
          ExtendedChar := Chr(KeyCode and $FF);
          Result := #0;
          Break;
        end;
      end;
    until False;
    SetConsoleMode(StdIn, KeyMode);
  end;
end;

function KeyPressed: Boolean;
var
  InputRecArray: array of TInputRecord;
  NumRead: DWORD;
  NumEvents: DWORD;
  I: Integer;
  KeyCode: Word;
begin
  Result := False;
  GetNumberOfConsoleInputEvents(StdIn, NumEvents);
  if NumEvents = 0 then
    Exit;
  SetLength(InputRecArray, NumEvents);
  PeekConsoleInput(StdIn, InputRecArray[0], NumEvents, NumRead);
  for I := 0 to High(InputRecArray) do
  begin
    if (InputRecArray[I].EventType and Key_Event <> 0) and
       InputRecArray[I].Event.KeyEvent.bKeyDown then
    begin
      KeyCode := InputRecArray[I].Event.KeyEvent.wVirtualKeyCode;
      if not (KeyCode in [VK_SHIFT, VK_MENU, VK_CONTROL]) then
      begin
        if ConvertKey(InputRecArray[I], FindKeyCode(KeyCode)) <> -1 then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TextColor(Color: Byte);
begin
  LastMode := TextAttr;
  TextAttr := (TextAttr and $F0) or (Color and $0F);
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure TextBackground(Color: Byte);
begin
  LastMode := TextAttr;
  TextAttr := (TextAttr and $0F) or ((Color shl 4) and $F0);
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure LowVideo;
begin
  LastMode := TextAttr;
  TextAttr := TextAttr and $F7;
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure HighVideo;
begin
  LastMode := TextAttr;
  TextAttr := TextAttr or $08;
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

procedure NormVideo;
begin
  TextAttr := DefaultAttr;
  SetConsoleTextAttribute(StdOut, TextAttr);
end;

// The following functions are independent of TextWindow.

function GetCursorX: Integer; {$IFDEF INLINES}inline;{$ENDIF}
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(StdOut, BufferInfo);
  Result := BufferInfo.dwCursorPosition.X;
end;

function GetCursorY: Integer; {$IFDEF INLINES}inline;{$ENDIF}
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleSCreenBufferInfo(StdOut, BufferInfo);
  Result := BufferInfo.dwCursorPosition.Y;
end;

procedure SetCursorPos(X, Y: Smallint);
var
  NewPos: TCoord;
begin
  NewPos.X := X;
  NewPos.Y := Y;
  SetConsoleCursorPosition(StdOut, NewPos);
end;

// The following functions are relative to TextWindow.

procedure ClrScr;
var
  StartPos: TCoord;
  Len, NumWritten: DWORD;
  I: Integer;
begin
  if (TextWindow.Left = 0) and (TextWindow.Top = 0) and
     (TextWindow.Right = BufferSize.X - 1) and
     (TextWindow.Bottom = BufferSize.Y - 1) then
  begin
    StartPos.X := 0;
    StartPos.Y := 0;
    Len := BufferSize.X * BufferSize.Y;
    FillConsoleOutputCharacterA(StdOut, ' ', Len, StartPos, NumWritten);
    FillConsoleOutputAttribute(StdOut, TextAttr, Len, StartPos, NumWritten);
    if NumWritten < Len then
    begin
      ScreenSize.X := ScreenWidth;
      ScreenSize.Y := ScreenHeight;
    end;
  end
  else
  begin
    Len := TextWindow.Right - TextWindow.Left + 1;
    StartPos.X := TextWindow.Left;
    for I := TextWindow.Top to TextWindow.Bottom do
    begin
      StartPos.Y := I;
      FillConsoleOutputCharacterA(StdOut, ' ', Len, StartPos, NumWritten);
      FillConsoleOutputAttribute(StdOut, TextAttr, Len, StartPos, NumWritten);
    end;
  end;
  GotoXY(1, 1);
end;

procedure GotoXY(X, Y: Smallint);
begin
  Inc(X, TextWindow.Left - 1);
  Inc(Y, TextWindow.Top - 1);
  if (X >= TextWindow.Left) and (X <= TextWindow.Right) and
     (Y >= TextWindow.Top) and (Y <= TextWindow.Bottom) then
    SetCursorPos(X, Y);
end;

procedure ClrEol;
var
  Len: Integer;
  Pos: TCoord;
  NumWritten: DWORD;
begin
  Len := TextWindow.Right - GetCursorX + 1;
  Pos.X := GetCursorX;
  Pos.Y := GetCursorY;
  FillConsoleOutputCharacterA(StdOut, ' ', Len, Pos, NumWritten);
  FillConsoleOutputAttribute(StdOut, TextAttr, Len, Pos, NumWritten);
end;

procedure Scroll(Left, Top, Right, Bottom: Integer; Distance: Integer = 0);
var
  Rect: TSmallRect;
  Fill: TCharInfo;
  NewPos: TCoord;
begin
  Fill.AsciiChar := ' ';
  Fill.Attributes := TextAttr;
  if Distance = 0 then
    Distance := Bottom - Top + 1;
  Rect.Left := Left;
  Rect.Right := Right;
  Rect.Top := Top;
  Rect.Bottom := Bottom;
  NewPos.X := Left;
  NewPos.Y := Top + Distance;
  ScrollConsoleScreenBufferA(StdOut, Rect, @Rect, NewPos, Fill);
end;

procedure InsLine;
begin
  Scroll(TextWindow.Left, GetCursorY,
    TextWindow.Right, TextWindow.Bottom, 1);
end;

procedure DelLine;
begin
  Scroll(TextWindow.Left, GetCursorY,
    TextWindow.Right, TextWindow.Bottom, -1);
end;

function Validate(X1, Y1, X2, Y2: Integer): Boolean;
  {$IFDEF INLINES}inline;{$ENDIF}
begin
  Result := (X1 < X2) and (Y1 < Y2) and
            (X1 >= 0) and (X2 < BufferSize.X) and
            (Y1 >= 0) and (Y2 < BufferSize.Y);
end;

procedure WriteText(Line: PAnsiChar; Len: Integer);
var
  NumWritten: DWORD;
begin
  SetConsoleTextAttribute(StdOut, TextAttr);
  WriteConsoleA(StdOut, Line, Len, NumWritten, nil);
end;

// Replacement for TTextRec.InOutFunc and TTextRec.FlushFunc for the Output
// and ErrOutput pseudo-textfiles.
// This is generally only used if a text window is set, otherwise this is
// handled by the runtime library.
function NewTextOut(var T: TTextRec): Integer;
var
  ReadPtr, WritePtr: PAnsiChar;
  Line: AnsiString;
  DistanceToEdge: Integer;

  // Moves cursor to start of line, updates DistanceToEdge.
  procedure CarriageReturn;
  begin
    SetCursorPos(TextWindow.Left, GetCursorY);
    DistanceToEdge := TextWindow.Right - TextWindow.Left + 1;
  end;

  // Moves cursor down one line. If necessary, scrolls window.
  procedure LineFeed; {$IFDEF INLINES}inline;{$ENDIF}
  begin
    if GetCursorY < TextWindow.Bottom then
      SetCursorPos(GetCursorX, GetCursorY + 1)
    else
      Scroll(TextWindow.Left, TextWindow.Top, TextWindow.Right,
        TextWindow.Bottom, -1);
  end;

  // Store one char in write buffer.
  procedure CharToWriteBuffer(C: AnsiChar);
  begin
    WritePtr^ := C;
    Inc(WritePtr);
    Dec(DistanceToEdge);
  end;

  // True if at right edge of window.
  function WriteLine: Boolean;
  begin
    WritePtr^ := #0;
    WriteText(PAnsiChar(Line), WritePtr - PAnsiChar(Line));
    Result := DistanceToEdge = 0;
    WritePtr := PAnsiChar(Line);
    DistanceToEdge := TextWindow.Right - TextWindow.Left + 1;
  end;

  // Converts tabs to spaces, since WriteConsole will do its own tabbing when
  // it encounters a #9, which is of course independent of this unit's
  // TextWindow settings.
  procedure ProcessTab;
  var
    Num, I: Integer;
  begin
    Num := 8 - (WritePtr - PAnsiChar(Line)) mod 8;
    if Num > DistanceToEdge then
      Num := DistanceToEdge;
    for I := 1 to Num do
      CharToWriteBuffer(' ');
  end;

begin
  SetLength(Line, BufferSize.X); // Line only contains one line of windowed text.
  WritePtr := PAnsiChar(Line);
  ReadPtr := T.BufPtr;
  DistanceToEdge := TextWindow.Right - GetCursorX + 1;
  while T.BufPos > 0 do
  begin
    while (T.BufPos > 0) and (DistanceToEdge > 0) do
    begin
      case ReadPtr^ of
        #7: Windows.Beep(800, 200); // this is what my internal speaker uses.
        #8: begin
              Dec(WritePtr);
              Inc(DistanceToEdge);
            end;
        #9: ProcessTab;
        // LineFeed is not just a line feed, it takes the function of #13#10
        #10: begin
               WriteLine;
               CarriageReturn;
               LineFeed;
             end;
        #13: begin
               WriteLine;
               CarriageReturn;
             end;
        else
          CharToWriteBuffer(ReadPtr^);
      end;
      Inc(ReadPtr);
      Dec(T.BufPos);
    end;
    if WriteLine then
    begin
      CarriageReturn;
      // If TexWindow.Right is at the edge of the screen, WriteConsole will
      // already do a linefeed.
      if TextWindow.Right <> ScreenWidth - 1 then
        LineFeed;
    end;
  end;
  Result := 0;
end;

var
  OldInOutFunc: Pointer;
  OldFlushFunc: Pointer;

procedure Window(Left, Top, Right, Bottom: Integer);
begin
  Dec(Left);
  Dec(Top);
  Dec(Right);
  Dec(Bottom);
  if Validate(Left, Top, Right, Bottom) then
  begin
    TextWindow.Left := Left;
    TextWindow.Top := Top;
    TextWindow.Right := Right;
    TextWindow.Bottom := Bottom;
    if (Left > 0) or (Top > 0) or
       (Right < BufferSize.X - 1) or (Bottom < BufferSize.Y - 1) then
    // Text must be contained in window
    begin
      OldInOutFunc := TTextRec(Output).InOutFunc;
      OldFlushFunc := TTextRec(Output).FlushFunc;
      TTextRec(Output).InOutFunc := @NewTextOut;
      TTextRec(Output).FlushFunc := @NewTextOut;
      SetCursorPos(Left, Top);
    end;
  end
  else
  begin
    TextWindow.Left := 0;
    TextWindow.Right := BufferSize.X - 1;
    TextWindow.Top := 0;
    TextWindow.Bottom := BufferSize.Y - 1;
    SetCursorPos(0, 0);
    if Assigned(OldInOutFunc) then
    begin
      TTextRec(Output).InOutFunc := OldInOutFunc;
      OldInOutFunc := nil;
    end;
    if Assigned(OldFlushFunc) then
    begin
      TTextRec(Output).FlushFunc := OldFlushFunc;
      OldFlushFunc := nil;
    end;
  end;
  WindMin := (TextWindow.Left and $FF) or (TextWindow.Top and $FF) shl 8;
  WindMax := (TextWindow.Right and $FF) or (TextWindow.Bottom and $FF) shl 8;
end;

procedure HardwareSound(Frequency: Smallint);
asm
        CMP     AX,37
        JB      @@1
        MOV     CX,AX
        MOV     AL,$B6
        OUT     $43,AL
        MOV     AX,$3540
        MOV     DX,$0012
        DIV     CX
        OUT     $42,AL
        MOV     AL,AH
        OUT     $42,AL
        MOV     AL,3
        OUT     $61,AL
@@1:
end;

procedure HardwareNoSound;
asm
        MOV     AL,0
        OUT     $61,AL
end;

procedure HardwareDelay(Millisecs: Integer);
begin
  Sleep(Millisecs);
end;

procedure HardwareBeep(Frequency, Duration: Smallint);
begin
  Sound(Frequency);
  Delay(Duration);
  NoSound;
end;

type
  TSoundState = (ssPending, ssPlaying, ssFreed);

var
  CurrentFrequency: Integer;
  SoundState: TSoundState;

// On Windows NT and later, direct port access is prohibited, so there is
// no way to use HardwareSound and HardwareNoSound.
//
// Since probably every note played by Sound will be delimited by some kind
// of Delay, the playing of the note is deferred to Delay. Sound only stores
// the frequency and sets the SoundState to ssPending. Delay now knows both
// parameters, and can use Windows.Beep.
//
// Note that such code is not reentrant.

procedure SoftwareSound(Frequency: Smallint);
begin
  // $123540 div Frequency must be <= $7FFF, so Frequency must be >= 37.
  if Frequency >= 37 then
  begin
    CurrentFrequency := Frequency;
    SoundState := ssPending;
  end;
end;

procedure SoftwareDelay(Millisecs: Integer);
begin
  if SoundState = ssPending then
  begin
    SoundState := ssPlaying;
    Windows.Beep(CurrentFrequency, MilliSecs);
    SoundState := ssFreed;
  end
  else
    Sleep(MilliSecs);
end;

procedure SoftwareBeep(Frequency, Duration: Smallint);
begin
  if Frequency >= 37 then
  begin
    SoundState := ssPlaying;
    Windows.Beep(Frequency, Duration);
    SoundState := ssFreed;
  end;
end;

procedure SoftwareNoSound;
begin
  Windows.Beep(CurrentFrequency, 0);
  SoundState := ssFreed;
end;

function WhereX: Integer;
begin
  Result := GetCursorX - TextWindow.Left + 1;
end;

function WhereY: Integer;
begin
  Result := GetCursorY - TextWindow.Top + 1;
end;

procedure GetScreenSizes(var Width, Height: Smallint);
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(StdOut, BufferInfo);
  Width := BufferInfo.srWindow.Right - BufferInfo.srWindow.Left + 1;
  Height := BufferInfo.srWindow.Bottom - BufferInfo.srWindow.Top + 1;
end;

function ScreenWidth: Smallint;
var
  Height: Smallint;
begin
  GetScreenSizes(Result, Height);
end;

function ScreenHeight: Smallint;
var
  Width: Smallint;
begin
  GetScreenSizes(Width, Result);
end;

procedure GetBufferSizes(var Width, Height: Smallint);
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo(StdOut, BufferInfo);
  Width := BufferInfo.dwSize.X;
  Height := BufferInfo.dwSize.Y;
end;

function BufferWidth: Smallint;
var
  Height: Smallint;
begin
  GetBufferSizes(Result, Height);
end;

function BufferHeight: Smallint;
var
  Width: Smallint;
begin
  GetBufferSizes(Width, Result);
end;

function TextColor: Byte;
begin
  Result := TextAttr and $0F;
end;

function TextBackground: Byte;
begin
  Result := (TextAttr and $F0) shr 4;
end;

procedure TextMode(Mode: Word);
begin
  Window(0, 0, 0, 0);
  NormVideo;
end;

procedure InitScreenMode;
var
  BufferInfo: TConsoleScreenBufferInfo;
begin
  Reset(Input);
  Rewrite(Output);
  StdIn := TTextRec(Input).Handle;
  StdOut := TTextRec(Output).Handle;
{$IFDEF HASERROUTPUT}
  Rewrite(ErrOutput);
  StdErr := TTextRec(ErrOutput).Handle;
{$ELSE}
  StdErr := GetStdHandle(STD_ERROR_HANDLE);
{$ENDIF}
  if not GetConsoleScreenBufferInfo(StdOut, BufferInfo) then
  begin
    SetInOutRes(GetLastError);
    Exit;
  end;
  TextWindow.Left := 0;
  TextWindow.Top := 0;
  TextWindow.Right := BufferInfo.dwSize.X - 1;
  TextWindow.Bottom := BufferInfo.dwSize.Y - 1;
  TextAttr := BufferInfo.wAttributes and $FF;
  DefaultAttr := TextAttr;
  BufferSize := BufferInfo.dwSize;
  ScreenSize.X := BufferInfo.srWindow.Right - BufferInfo.srWindow.Left + 1;
  ScreenSize.Y := BufferInfo.srWindow.Bottom - BufferInfo.srWindow.Top + 1;
  WindMin := 0;
  WindMax := (ScreenSize.X and $FF) or (ScreenSize.Y and $FF) shl 8;
  LastMode := CO80;
  OldInOutFunc := nil;
  OldFlushFunc := nil;
  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    Sound := SoftwareSound;
    NoSound := SoftwareNoSound;
    Delay := SoftwareDelay;
    Beep := SoftwareBeep;
  end
  else
  begin
    Sound := HardwareSound;
    NoSound := HardwareNoSound;
    Delay := HardwareDelay;
    Beep := HardwareBeep;
  end;
end;

initialization
  InitScreenMode;

end.

