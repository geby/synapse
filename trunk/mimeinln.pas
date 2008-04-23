{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
|==============================================================================|
| Content: Inline MIME support procedures and functions                        |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.0 |
| (the "License"); you may not use this file except in compliance with the     |
| License. You may obtain a copy of the License at http://www.mozilla.org/MPL/ |
|                                                                              |
| Software distributed under the License is distributed on an "AS IS" basis,   |
| WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for |
| the specific language governing rights and limitations under the License.    |
|==============================================================================|
| The Original Code is Synapse Delphi Library.                                 |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c)2000.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

unit MIMEinLN;

interface

uses
  sysutils, classes, windows, MIMEchar, SynaCode, SynaUtil;

function InlineDecode(value:string;CP:TMimeChar):string;
function InlineEncode(value:string;CP,MimeP:TMimeChar):string;
Function NeedInline(value:string):boolean;
function InlineCode(value:string):string;
function InlineEmail(value:string):string;

implementation

{==============================================================================}
{InlineDecode}
function InlineDecode(value:string;CP:TMimeChar):string;
var
  s,su:string;
  x,y,z,n:integer;
  ichar:TMimeChar;
  c:char;

  function SearchEndInline(value:string;be:integer):integer;
  var
    n,q:integer;
  begin
    q:=0;
    result:=0;
    for n:=be+2 to length(value)-1 do
      if value[n]='?' then
        begin
          inc(q);
          if (q>2) and (value[n+1]='=') then
            begin
              result:=n;
              break;
            end;
        end;
  end;

begin
  result:=value;
  x:=pos('=?',result);
  y:=SearchEndInline(result,x);
  while y>x do
    begin
      s:=copy(result,x,y-x+2);
      su:=copy(s,3,length(s)-4);
      ichar:=GetCPfromID(su);
      z:=pos('?',su);
      if (length(su)>=(z+2)) and (su[z+2]='?') then
        begin
          c:=uppercase(su)[z+1];
          su:=copy(su,z+3,length(su)-z-2);
          if c='B' then
            begin
              s:=DecodeBase64(su);
              s:=DecodeChar(s,ichar,CP);
            end;
          if c='Q' then
            begin
              s:='';
              for n:=1 to length(su) do
                if su[n]='_'
                  then s:=s+' '
                  else s:=s+su[n];
              s:=DecodeQuotedprintable(s);
              s:=DecodeChar(s,ichar,CP);
            end;
        end;
      result:=copy(result,1,x-1)+s+copy(result,y+2,length(result)-y-1);
      x:=pos('=?',result);
      y:=SearchEndInline(result,x);
    end;
end;

{==============================================================================}
{InlineEncode}
function InlineEncode(value:string;CP,MimeP:TMimeChar):string;
var
  s,s1:string;
  n:integer;
begin
  s:=DecodeChar(value,CP,MimeP);
  s:=EncodeQuotedPrintable(s);
  s1:='';
  for n:=1 to length(s) do
    if s[n]=' '
      then s1:=s1+'=20'
      else s1:=s1+s[n];
  result:='=?'+GetIdFromCP(MimeP)+'?Q?'+s1+'?=';
end;

{==============================================================================}
{NeedInline}
Function NeedInline(value:string):boolean;
var
  n:integer;
begin
  result:=false;
  for n:=1 to length(value) do
    if value[n] in (SpecialChar+[char(1)..char(31),char(128)..char(255)]) then
      begin
        result:=true;
        break;
      end;
end;

{==============================================================================}
{InlineCode}
function InlineCode(value:string):string;
var
  c:TMimeChar;
begin
  if NeedInline(value)
    then
      begin
        c:=IdealCoding(value,GetCurCP,
          [ISO_8859_1, ISO_8859_2, ISO_8859_3, ISO_8859_4, ISO_8859_5,
          ISO_8859_6, ISO_8859_7, ISO_8859_8, ISO_8859_9, ISO_8859_10]);
        result:=InlineEncode(value,GetCurCP,c);
      end
    else result:=value;
end;

{==============================================================================}
{InlineEmail}
function InlineEmail(value:string):string;
var
  sd,se:string;
begin
  sd:=getEmaildesc(value);
  se:=getEmailAddr(value);
  if sd=''
    then result:=se
    else result:='"'+InlineCode(sd)+'"<'+se+'>';
end;

{==============================================================================}

begin
  exit;
  asm
    db 'Synapse Inline MIME encoding and decoding library by Lukas Gebauer',0
  end;
end.
