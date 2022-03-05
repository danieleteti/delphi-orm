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
// Compilation directives for Lazarus

// Suppress some FPC hints that are known to be false
//{$WARN 4055 off : Conversion between ordinals and pointers is not portable} //suppress compiler buggy warning -> our Native(U)Int is Pointer size aligned
//{$WARN 4056 off : Conversion between ordinals and pointers is not portable}
//{$WARN 5093 off : function result variable of a managed type does not seem to be initialized} //suppress warning - we suppose all managed types are init'd as empty
//{$WARN 5094 off : function result variable of a managed type does not seem to be initialized}
//{$WARN 5089 off : Local variable "$1" of a managed type does not seem to be initialized}
//{$WARN 5090 off : Variable "$1" of a managed type does not seem to be initialized}
//{$WARN 5091 off : Local variable "$1" of a managed type does not seem to be initialized}

{$DEFINE PUREPASCAL}

{$DEFINE WITH_VARIANT_UINT64}         // System.TVariant supports UInt64 Values
{$DEFINE WITH_VARIANT_UNICODESTRING}  // System.TVariant supports UnicodeString Values
{$DEFINE NO_CONST_ZEROBCD}  // FmtBcd.ZeroBcd is a operator not a const
{$DEFINE WITH_TFIELD_FREEBUFFERS}     //TField has virtual FreeBuffers procedure
{$DEFINE TFIELD_HAS_ASBYTES}          // TField.AsBytes is supported
{$DEFINE TFIELD_HAS_ASLARGEINT}       // TField-baseclass hase AsLargeIntProperty
{$DEFINE HAVE_UNKNOWN_CIRCULAR_REFERENCE_ISSUES} //seems FPC behaves different to Delphi. Some weired ref issues arrive if Ancastor units using Descendants in implementation uses section??
{$DEFINE NO_PUBLISHED_RECORDS_ALLOWED} //the compiler is not able to publish records using RTTI
{$DEFINE HAVE_BCDTOSTR_FORMATSETTINGS} //(Try)BCDToStr/StrToBCD has parameter FormatSettings
{$DEFINE NEED_TYPED_UINT64_CONSTANTS}
{$DEFINE WITH_PUREPASCAL_INTPOWER}
{$DEFINE TFIELD_ASWIDESTRING_IS_WIDESTRING}
{$DEFINE HAVE_CS_TRYENTER} //TCriticalSection has method TryEnter
{$IFDEF SYSTEMINLINE} //global FPC define
  {$DEFINE WITH_INLINE}           // compiler supports inline methodes
{$ENDIF}
{$DEFINE HAVE_INSERT_BOOKMARK_BUG} //FPC does set bfEOF on Append but on Insert a row the Bookmark-Flag is bfCurrent while it should be bfInsert min v3.1
{$DEFINE WITH_UNIDIRECTIONALBUG} //still buggy with IsUnidirectional prop of TDataSet FPC still moves the recordBuffer-> visit TestReadOnlyQueryUniDirectional and TZAbstractRODataset.GetRecord
{$IFNDEF CPU386}
  {$DEFINE WITH_IE200706094} //Fatal: Internal error 200706094 see: http://zeoslib.sourceforge.net/viewtopic.php?f=40&t=11096#p34381
{$ENDIF}
// These are in a spearate file as Delphi compilers don't digest the macros stuff very well
{$MACRO ON}             // Macros needed for FPC_VERSION and FPC_RELEASE
{$MODE DELPHI}
  
{$INTERFACES COM}
{$IFDEF FPC_FULLVERSION}  // FPC_FULLVERSION was added in FPC 2.2.4
  {$IF FPC_FULLVERSION<=20204}
    {$DEFINE OLDFPC}
  {$IFEND}

  {$DEFINE WITH_PARAM_ASLARGEINT} //supportet since FPC 2.0.4 -> see http://zeoslib.sourceforge.net/viewtopic.php?p=68484#p68484
  {$DEFINE WITH_FTWIDESTRING}  // Supported from somewhere between 2.2 and 2.4

  {$IF FPC_FULLVERSION>=20500}  // Upgrade to 2.6.x asap.
    {$DEFINE FPC2_5UP} // Used in code
    {$DEFINE WITH_NEWTOBJECT}
    {$DEFINE WITH_IPROVIDER}
    {$DEFINE HAVE_UNICODESTRING}
  {$IFEND}
  {$IF FPC_FULLVERSION>=20600}  // assumes 2.5.1 pretty close to 2.6.0. Summer 2011 or newer
     {$DEFINE WITH_INC_PBYTE_SUPPORT}  // Compiler supports incrementing PByte-Pointer
     {$DEFINE WITH_WIDEMEMO}
     {$DEFINE WITH_ASLARGEINT}      // tlargeint aslargeint. Fixed 2.6.0+ or 2.6.2+ probably
     {$DEFINE WITH_ASCURRENCY}      // Has AsCurrency.
     {$DEFINE WITH_ASVARIANT}
     {$DEFINE WITH_FORMATSETTINGS}
     {$DEFINE WITH_WIDEFIELDS}      // Wide-fields are supported (common define for ftWideString/ftWideMemo)
     {$DEFINE PWIDECHAR_IS_PUNICODECHAR}
     {$DEFINE WITH_FTGUID} //TStringField descendant TGuidField is supported
     {$DEFINE FPC2_6UP}
     {$DEFINE WITH_VIRTUAL_TFIELD_ASWIDESTRING}
     {$IF FPC_FULLVERSION>=30000}
       {$DEFINE WITH_TESTCASE_ERROR_ADDRESS} //testcase methods like Fail() have error address pointer
       {$IF FPC_FULLVERSION>=30200}
         {$UNDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD} 
         {$DEFINE WITH_VAR_INIT_WARNING}
         {$DEFINE WITH_CASE_WARNING} //Since FPC 3.2 we've some weird warnigns on using case X of
         {$DEFINE WITH_CODEPAGE_AWARE_FIELD}
       {$IFEND}
     {$IFEND}
  {$IFEND}

  {$IF FPC_FULLVERSION>=30000}
       {$DEFINE FPC3_0UP}
  {$ELSE}
    {$DEFINE FPC2_6DOWN}
  {$IFEND}

      {$IF FPC_FULLVERSION>=30200}
	 {$UNDEF MISS_MATH_NATIVEUINT_MIN_MAX_OVERLOAD}
      {$ENDIF}

  {$IFDEF LCL}
    {$DEFINE WITH_LCONVENCODING} //Only available for Lazarus
  {$ENDIF}
  {$DEFINE WITH_FPC_FTTIME_BUG} //http://zeos.firmos.at/viewtopic.php?t=3534&postdays=0&postorder=asc&start=0
  {$DEFINE WITH_FPC_BOF_BUG} //http://bugs.freepascal.org/view.php?id=22377
  {$DEFINE WITH_FPC_MODIFIED_BUG} //http://bugs.freepascal.org/view.php?id=22381
  {$DEFINE WITH_PWIDECHAR_TOWIDESTRING}
  {$DEFINE WITH_CLASS_VARS}
{$ELSE}
  {$FATAL Versions Older than 2.2.4 are not supported!}
{$ENDIF}

  {$DEFINE WITH_FUNIDIRECTIONAL} // FPC has references a field instead of a setter here.  (at least in 2.6.0)

  {$IFDEF FPC_FULLVERSION>20601}
    {$UNDEF WITH_FPC_FTTIME_BUG} //seems like the never FPC2.6.2 also have fixed the NativData bug for TTimeFields+TDataSet
    {$IF FPC_FULLVERSION>20700}       // will be introduced in 2.7+
      {$ifndef MSEgui}
        {$DEFINE WITH_TBOOKMARK}        // Have TBookmark
      {.§ELSE}
        {.$DEFINE UNICODE} //16bit UTF16-Encoded see https://zeoslib.sourceforge.io/viewtopic.php?t=4688
      {$endif}
      {$DEFINE TFIELD_HAS_ASBYTES}          // TField.AsBytes is supported
      {$DEFINE TFIELD_HAS_ASLARGEINT}  // TFIeld.AsLargeInt is supported
      {$UNDEF WITH_FPC_BOF_BUG}
      {$UNDEF WITH_FPC_MODIFIED_BUG}
      {$DEFINE WITH_RAWBYTESTRING}          // Have RawByteString CP $FFFFF
      {$DEFINE WITH_FPC_STRING_CONVERSION} //After string conversions to RawByteString like AnsiToUTF8 the results are equal to original value
      {$DEFINE WITH_DEFAULTSYSTEMCODEPAGE}
      {$DEFINE MISS_RBS_SETSTRING_OVERLOAD} //SetString overload for RawByteString is missing
      {$IFNDEF FPC_HAS_BUILTIN_WIDESTR_MANAGER} //Hack for *nix systems where i couldn't locate this define )):
        {$DEFINE FPC_HAS_BUILTIN_WIDESTR_MANAGER}
      {$ENDIF}
      {$DEFINE WITH_RAWBYTESTRING_CONVERSION_BUG} //2.7 gen did bad conversions with possible data-loss here
      {$IF FPC_FULLVERSION>30000}       // will be introduced in 3.0+
        {$DEFINE WITH_tagVARIANT_UINT64} //ActiveX tagVariant supports Int64/uInt64 values not sure about the proper FPC version here
        {$DEFINE WITH_varUString} //Variant now supports refcounted UnicodeStrings
        {$UNDEF WITH_RAWBYTESTRING_CONVERSION_BUG} //issue seems to be fixed
        {$DEFINE WITH_VIRTUAL_TFIELD_BIND} //TField has procedure Bind(Binding: Boolean);
        {$IF FPC_FULLVERSION>=30100}
          {$UNDEF MISS_RBS_SETSTRING_OVERLOAD} //SetString overload for RawByteString is available now
          {$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined} //suppress warning that marks all Variant operators f.ex.

          {$DEFINE WITH_ASANSISTRING} // TField.AsAnsiString is supported
          {$DEFINE WITH_VIRTUAL_TFIELD_ASUTF8STRING} // //TField has virtual getter+Setter for the UTF8Strings
          {$DEFINE WITH_VIRTUAL_TFIELD_ASUNICODESTRING} //TField has virtual getter+Setter for the UnicodeStrings
        {$IFEND}
      {$IFEND}
    {$IFEND}
  {$ENDIF}
  // todo
  //   VIRTUALSETFIELDDATA: in nativeformat versions of SETFIELDDATA, inherited wasn't called.
  //                Afaik that methoid is virtual in FPC since December 2006, and there is no comment why
  //                it was ifdefed. We'll leave it off, and see if sb complains.
  //   LAZARUSUTF8HACK :  Places that fixed for the Lazarus convention that all strings are UTF8  are marked with this
  //                             ifdef. But probably most of them are adhoc hacks, and need constant reevaluation.
  //   OLDFPC      : IFDEF FPC's so old that I can't remember which version are put under IFDEF OLDFPC. In time these
  //                   blocks can be killed.

{$IF defined(WITH_LCONVENCODING) and (defined(MSWINDOWS) or defined(FPC_HAS_BUILTIN_WIDESTR_MANAGER))}
  {$UNDEF WITH_LCONVENCODING}
{$IFEND}

{$IFDEF WITH_LCONVENCODING}
  {$IF declared(lcl_fullversion)}
    {$if lcl_fullversion>=1000000} //Lazarus >= 1.0.0 with delphi fix.
      {$DEFINE LCONVENCODING_HAS_CP852_FUNCTIONS}
    {$ifend}
  {$ifend}
{$ENDIF}
