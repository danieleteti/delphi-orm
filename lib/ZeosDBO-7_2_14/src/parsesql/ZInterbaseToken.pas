{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{        String tokenizing classes for Interbase          }
{                                                         }
{        Originally written by Sergey Seroukhov           }
{                                                         }
{*********************************************************}

{@********************************************************}
{    Copyright (c) 1999-2020 Zeos Development Group       }
{                                                         }
{ License Agreement:                                      }
{                                                         }
{ This library is distributed in the hope that it will be }
{ useful, but WITHOUT ANY WARRANTY; without even the      }
{ implied warranty of MERCHANTABILITY or FITNESS FOR      }
{ A PARTICULAR PURPOSE.  See the GNU Lesser General       }
{ Public License for more details.                        }
{                                                         }
{ The source code of the ZEOS Libraries and packages are  }
{ distributed under the Library GNU General Public        }
{ License (see the file COPYING / COPYING.ZEOS)           }
{ with the following  modification:                       }
{ As a special exception, the copyright holders of this   }
{ library give you permission to link this library with   }
{ independent modules to produce an executable,           }
{ regardless of the license terms of these independent    }
{ modules, and to copy and distribute the resulting       }
{ executable under terms of your choice, provided that    }
{ you also meet, for each linked independent module,      }
{ the terms and conditions of the license of that module. }
{ An independent module is a module which is not derived  }
{ from or based on this library. If you modify this       }
{ library, you may extend this exception to your version  }
{ of the library, but you are not obligated to do so.     }
{ If you do not wish to do so, delete this exception      }
{ statement from your version.                            }
{                                                         }
{                                                         }
{ The project web site is located on:                     }
{   http://zeos.firmos.at  (FORUM)                        }
{   http://sourceforge.net/p/zeoslib/tickets/ (BUGTRACKER)}
{   svn://svn.code.sf.net/p/zeoslib/code-0/trunk (SVN)    }
{                                                         }
{   http://www.sourceforge.net/projects/zeoslib.          }
{                                                         }
{                                                         }
{                                 Zeos Development Group. }
{********************************************************@}

unit ZInterbaseToken;

interface

{$I ZParseSql.inc}

{$IFNDEF ZEOS_DISABLE_INTERBASE}
uses
  Classes, ZTokenizer, ZGenericSqlToken;

type

  {** Implements a Interbase-specific number state object. }
  TZInterbaseNumberState = TZGenericSQLNoHexNumberState;

  {** Implements a Interbase-specific quote string state object. }
  TZInterbaseQuoteState = TZGenericSQLQuoteState;

  {**
    This state will either delegate to a comment-handling
    state, or return a token with just a slash in it.
  }
  TZInterbaseCommentState = TZGenericSQLCommentState;

  {** Implements a symbol state object. }
  TZInterbaseSymbolState = class (TZSymbolState)
  public
    constructor Create;
  end;

  {** Implements a word state object. }
  TZInterbaseWordState = class (TZGenericSQLWordState)
  public
    constructor Create;
  end;

  {** Implements a default tokenizer object. }
  TZInterbaseTokenizer = class (TZTokenizer)
  protected
    procedure CreateTokenStates; override;
  end;

{$ENDIF ZEOS_DISABLE_INTERBASE}

implementation

{$IFNDEF ZEOS_DISABLE_INTERBASE}

{ TZInterbaseSymbolState }

{**
  Creates this Interbase-specific symbol state object.
}
constructor TZInterbaseSymbolState.Create;
begin
  inherited Create;
  Add('<=');
  Add('>=');
  Add('<>');
  Add('!=');
  Add('!<');
  Add('!>');
end;

{ TZInterbaseWordState }

{**
  Constructs this Interbase-specific word state object.
}
constructor TZInterbaseWordState.Create;
begin
  SetWordChars(#0, #255, False);
  SetWordChars('a', 'z', True);
  SetWordChars('A', 'Z', True);
  SetWordChars('0', '9', True);
  SetWordChars('_', '_', True);
  SetWordChars('$', '$', True);
end;

{ TZInterbaseTokenizer }

procedure TZInterbaseTokenizer.CreateTokenStates;
begin
  WhitespaceState := TZWhitespaceState.Create;

  SymbolState := TZInterbaseSymbolState.Create;
  NumberState := TZInterbaseNumberState.Create;
  QuoteState := TZInterbaseQuoteState.Create;
  WordState := TZInterbaseWordState.Create;
  CommentState := TZInterbaseCommentState.Create;

  SetCharacterState(#0, #32, WhitespaceState);
  SetCharacterState(#33, #191, SymbolState);
  SetCharacterState(#192, High(Char), WordState);

  SetCharacterState('a', 'z', WordState);
  SetCharacterState('A', 'Z', WordState);
  SetCharacterState('_', '_', WordState);
  SetCharacterState('$', '$', WordState);

  SetCharacterState('0', '9', NumberState);
  SetCharacterState('.', '.', NumberState);

  SetCharacterState('"', '"', QuoteState);
  SetCharacterState(#39, #39, QuoteState);

  SetCharacterState('/', '/', CommentState);
  SetCharacterState('-', '-', CommentState);
end;

{$ENDIF ZEOS_DISABLE_INTERBASE}

end.

