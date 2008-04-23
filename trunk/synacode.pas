{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
|==============================================================================|
| Content: Coding and decoding support                                         |
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

unit SynaCode;

interface

uses
  sysutils;

const
  SpecialChar:set of char
    =['=','(',')','[',']','<','>',':',';','.',',','@','/','?','\','"','_'];

  TableBase64=
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
{  TableUU=
    '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  TableXX=
    '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz';
}

function DecodeQuotedPrintable(value:string):string;
function EncodeQuotedPrintable(value:string):string;
function Decode4to3(value,table:string):string;
function DecodeBase64(value:string):string;
function EncodeBase64(value:string):string;

implementation

{==============================================================================}
{DecodeQuotedPrintable}
function DecodeQuotedPrintable(value:string):string;
var
  x:integer;
  c:char;
  s:string;
begin
  result:='';
  x:=1;
  while x<=length(value) do
    begin
      c:=value[x];
      inc(x);
      if c<>'='
        then result:=result+c
        else
          if (x+1)<length(value)
            then
              begin
                s:=copy(value,x,2);
                inc(x,2);
                result:=result+char(strtointdef('$'+s,32));
              end;
    end;
end;

{==============================================================================}
{EncodeQuotedPrintable}
function EncodeQuotedPrintable(value:string):string;
var
  n:integer;
  s:string;
begin
  result:='';
  for n:=1 to length(value) do
    begin
      s:=value[n];
      if s[1] in (SpecialChar+[char(1)..char(31),char(128)..char(255)])
        then s:='='+inttohex(ord(s[1]),2);
      result:=result+s;
    end;
end;

{==============================================================================}
{Decode4to3}
function Decode4to3(value,table:string):string;
var
  x,y,n:integer;
  d: array[0..3] of byte;
begin
  result:='';
  x:=1;
  while x<length(value) do
    begin
      for n:=0 to 3 do
        begin
          if x>length(value)
            then d[n]:=64
            else
              begin
                y:=pos(value[x],table);
                if y<1 then y:=65;
                d[n]:=y-1;
              end;
          inc(x);
        end;
      result:=result+char((D[0] and $3F) shl 2 + (D[1] and $30) shr 4);
      if d[2]<>64 then
        begin
          result:=result+char((D[1] and $0F) shl 4 + (D[2] and $3C) shr 2);
          if d[3]<>64 then
            result:=result+char((D[2] and $03) shl 6 + (D[3] and $3F));
        end;
    end;
end;

{==============================================================================}
{DecodeBase64}
function DecodeBase64(value:string):string;
begin
  result:=Decode4to3(value,TableBase64);
end;

{==============================================================================}
{EncodeBase64}
function EncodeBase64(value:string):string;
var
  c:byte;
  n:integer;
  Count:integer;
  DOut:array [0..3] of byte;
begin
  result:='';
  Count := 1;
  while count<=length(value) do
    begin
      c:=ord(value[count]);
      inc(count);
      DOut[0]:=(c and $FC) shr 2;
      DOut[1]:=(c and $03) shl 4;
      if count<=length(value)
        then
          begin
            c:=ord(value[count]);
            inc(count);
            DOut[1]:=DOut[1]+(c and $F0) shr 4;
            DOut[2]:=(c and $0F) shl 2;
            if count<=length(value)
              then
                begin
                  c:=ord(value[count]);
                  inc(count);
                  DOut[2]:=DOut[2]+(c and $C0) shr 6;
                  DOut[3]:=(c and $3F);
                end
              else
                begin
                  DOut[3] := $40;
                end;
          end
        else
          begin
            DOut[2] := $40;
            DOut[3] := $40;
          end;
      for n:=0 to 3 do
        result:=result+TableBase64[DOut[n]+1];
    end;
end;

{==============================================================================}

begin
  exit;
  asm
    db 'Synapse coding and decoding support library by Lukas Gebauer',0
  end;
end.
