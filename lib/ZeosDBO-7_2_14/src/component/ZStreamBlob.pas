{*********************************************************}
{                                                         }
{                 Zeos Database Objects                   }
{                  Blob streams classes                   }
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

unit ZStreamBlob;

interface

{$I ZComponent.inc}

uses Classes, SysUtils, {$IFDEF MSEgui}mclasses, mdb{$ELSE}DB{$ENDIF},
  {$IFDEF WITH_WIDESTRUTILS}WideStrUtils, {$ENDIF}
  ZDbcIntfs, ZCompatibility;

type
  {** Implements a class for blobs stream. }
  TZBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FBlob: IZBlob;
    FMode: TBlobStreamMode;
    FConSettings: PZConSettings;
  protected
    property Blob: IZBlob read FBlob write FBlob;
    property Mode: TBlobStreamMode read FMode write FMode;
  public
    constructor Create(Field: TBlobField; Blob: IZBlob; Mode: TBlobStreamMode;
      ConSettings: PZConSettings);
    destructor Destroy; override;
  end;

implementation

uses ZFastCode, ZSysUtils, ZEncoding;

{ TZBlobStream }

{**
  Constructs this object and assignes the main properties.
  @param Blob
}
constructor TZBlobStream.Create(Field: TBlobField; Blob: IZBlob;
  Mode: TBlobStreamMode; ConSettings: PZConSettings);
var
  Buffer: Pointer;
  ASize: Integer;
begin
  inherited Create;

  FBlob := Blob;
  FMode := Mode;
  FField := Field;
  FConSettings := ConSettings;

  if (Mode in [bmRead, bmReadWrite] ) and not Blob.IsEmpty then
  begin
    if Blob.IsClob then
      case Field.DataType of
        ftMemo, ftFmtMemo:
          if FConSettings^.AutoEncode or (FConSettings.ClientCodePage.Encoding = ceUTF16) then
            Buffer := Blob.GetPAnsiChar(FConSettings^.CTRL_CP)
          else
            Buffer := Blob.GetPAnsiChar(FConSettings^.ClientCodePage^.CP);
        {$IFDEF WITH_WIDEMEMO}
        ftWideMemo:
          Buffer := Blob.GetPWideChar;
        {$ENDIF}
        else
          Buffer := Blob.GetBuffer;
      end
    else
      Buffer := Blob.GetBuffer;
    ASize := Blob.Length;
    {$IFNDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM}
    if Mode = bmReadWrite then
    begin
      WriteBuffer(Buffer^, ASize); //something courrupts the FPC-Memory-Manager here??? D7??
      Position := 0;
    end
    else
    {$ENDIF}
      SetPointer(Buffer, ASize);
  end;
end;

type THackedDataset = class(TDataset);

{**
  Destroys this object and cleanups the memory.
}
destructor TZBlobStream.Destroy;
var
  UnCachedLob: IZUnCachedLob;
  RawCP: Word;
begin
  if Mode in [bmWrite, bmReadWrite] then
  begin
    Self.Position := 0;
    {EH: speed upgrade:
     instead of moving mem from A to B i set the mem-pointer to the lobs instead.
     But we have to validate the mem if required.. }

    if Memory <> nil then begin
      case FField.DataType of
        {$IFDEF WITH_WIDEMEMO}
        { EH: i've omitted a ancoding detection for the UTF16 encoding that fails in all areas }
        ftWideMemo: Blob.SetPWideChar(Memory, Size div 2);
        {$ENDIF}
        ftMemo:
          if Blob.IsClob then
            {EH: not happy about this part. TBlobStream.LoadFromFile loads single encoded strings
            but if the Data is set by a Memo than we've got two-byte encoded strings.
            So there is NO way around to test this encoding. Acutally i've no idea about a more exact way
            than going this route...}
            if FConSettings^.AutoEncode then
              case ZDetectUTF8Encoding(Memory, Size) of  //testencoding adds one leading null bytes
                etUSASCII: //us ascii found, use faster conversion
                  Blob.SetPAnsiChar(Memory, ZEncoding.zCP_us_ascii, Size);
                etAnsi: begin
                    if (ZCompatibleCodePages(FConSettings^.ClientCodePage^.CP, zCP_UTF8)) then
                      if (ZCompatibleCodePages(FConSettings^.CTRL_CP, zCP_UTF8)) then
                        if (ZCompatibleCodePages(ZOSCodePage, zCP_UTF8)) then
                        {no idea what to do with ansiencoding, if everything if set to UTF8!}
                          RawCP := zCP_UTF8 //all convertions would fail so.. let the server raise an error!
                        else RawCP := ZOSCodePage
                      else RawCP := FConSettings^.CTRL_CP
                    else RawCP := FConSettings^.ClientCodePage^.CP;
                    Blob.SetPAnsiChar(Memory, RawCP, Size);
                  end;
                etUTF8:
                  Blob.SetPAnsiChar(Memory, ZEncoding.zCP_UTF8, Size);
              end
            else
              Blob.SetPAnsiChar(Memory, FConSettings^.ClientCodePage^.CP, Size)
          else begin
            {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
            Blob.SetBlobData(Memory, Size);
            SetPointer(nil, 0); //don't forget! Keep Lob mem alive!
            {$ELSE} //need to move data
            Blob.SetBuffer(Memory, Size);
            {$ENDIF}
          end
        else begin//ftBLOB
          {$IFDEF WITH_MM_CAN_REALLOC_EXTERNAL_MEM} //set data directly -> no move
          Blob.SetBlobData(Memory, Size);
          SetPointer(nil, 0); //don't forget! Keep Lob mem alive!
          {$ELSE} //need to move data
          Blob.SetBuffer(Memory, Size);
          {$ENDIF}
        end;
      end; {case ...}
    end else { if Memory <> nil then}
      Blob.Clear;
    //try
      if Assigned(FField.Dataset) then
        THackedDataset(FField.DataSet).DataEvent(deFieldChange, NativeInt(FField));
    //except ApplicationHandleException(Self); end; //commented see https://sourceforge.net/p/zeoslib/tickets/226/
  end else begin
    SetPointer(nil, 0); //don't forget! Keep Lob mem alive!
    if Supports(Blob, IZUnCachedLob, UnCachedLob) then
      UnCachedLob.FlushBuffer;
  end;
  inherited Destroy;
end;

end.

