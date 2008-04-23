{==============================================================================|
| Project : Delphree - Synapse                                   | 001.002.000 |
|==============================================================================|
| Content: support for ASN.1 coding and decoding                               |
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
| Portions created by Lukas Gebauer are Copyright (c) 1999, 2000.              |
| Portions created by Hernan Sanchez are Copyright (c) 2000.                   |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Hernan Sanchez (hernan.sanchez@iname.com)                                  |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

unit ASN1Util;

interface

uses
  SysUtils, SynaUtil;

const
  ASN1_INT = $02;
  ASN1_OCTSTR = $04;
  ASN1_NULL = $05;
  ASN1_OBJID = $06;
  ASN1_SEQ = $30;
  ASN1_IPADDR = $40;
  ASN1_COUNTER = $41;
  ASN1_GAUGE = $42;
  ASN1_TIMETICKS = $43;
  ASN1_OPAQUE = $44;

function ASNEncOIDitem(Value: integer): string;
function ASNDecOIDitem(var Start: integer; Buffer: string): integer;
function ASNEncLen(Len: integer): string;
function ASNDecLen(var Start: integer; Buffer: string): integer;
function ASNEncInt(Value: integer): string;
function ASNEncUInt(Value: integer): string;
function ASNObject(Data: string; ASNType: integer): string;
function ASNItem(var Start: integer; Buffer: string; var ValueType:integer): string;

implementation

function ASNEncOIDitem(Value: integer): string;
var
  x,xm:integer;
  b:boolean;
begin
  x:=value;
  b:=false;
  result:='';
  repeat
    xm:=x mod 128;
    x:=x div 128;
    if b then
      xm:=xm or $80;
    if x>0
      then b:=true;
    result:=char(xm)+result;
  until x=0;
end;

function ASNDecOIDitem(var Start: integer; Buffer: string): integer;
var
  x:integer;
  b:boolean;
begin
  result:=0;
  repeat
    result:=result*128;
    x := Ord(Buffer[Start]);
    inc(start);
    b:=x>$7f;
    x:=x and $7f;
    result:=result+x;
    if not b
      then break;
  until false
end;

function ASNEncLen(Len: integer): string;
var
  x, y: integer;
begin
  if (len<$80)
    then result:=char(len)
    else
      begin
        x:=len;
        result:='';
        repeat
          y:=x mod 256;
          x:=x div 256;
          result:=char(y)+result;
        until x=0;
        y:=length(result);
        y:=y or $80;
        result:=char(y)+result;
      end;
end;

function ASNDecLen(var Start: integer; Buffer: string): integer;
var
  x,n: integer;
begin
  x:=Ord(Buffer[Start]);
  Inc(Start);
  if (x<$80)
    then Result:=x
    else
      begin
        result:=0;
        x:=x and $7f;
        for n:=1 to x do
          begin
            result:=result*256;
            x:=Ord(Buffer[Start]);
            Inc(Start);
            result:=result+x;
          end;
      end;
end;

function ASNEncInt(Value: integer): string;
var
  x,y:cardinal;
  neg:boolean;
begin
  neg:=value<0;
  x:=abs(Value);
  if neg then
    x:=not (x-1);
  result:='';
  repeat
    y:=x mod 256;
    x:=x div 256;
    result:=char(y)+result;
  until x=0;
  if (not neg) and (result[1]>#$7F)
    then result:=#0+result;
end;

function ASNEncUInt(Value: integer): string;
var
  x,y:integer;
  neg:boolean;
begin
  neg:=value<0;
  x:=Value;
  if neg
    then x:=x and $7FFFFFFF;
  result:='';
  repeat
    y:=x mod 256;
    x:=x div 256;
    result:=char(y)+result;
  until x=0;
  if neg
    then result[1]:=char(ord(result[1]) or $80);
end;

function ASNObject(Data: string; ASNType: integer): string;
begin
  Result := Char(ASNType) + ASNEncLen(Length(Data)) + Data;
end;

function ASNItem(var Start: integer; Buffer: string; var ValueType:integer): string;
var
  ASNType: integer;
  ASNSize: integer;
  y,n: integer;
  x: byte;
  s: string;
  c: char;
  neg: boolean;
begin
  ASNType := Ord(Buffer[Start]);
  Valuetype:=ASNType;
  Inc(start);
  ASNSize := ASNDecLen(Start, Buffer);
  Result := '';
  if ((ASNType and $20) > 0) then
  begin
    Result := '$' + IntToHex(ASNType, 2);
  end
  else
    case ASNType of
      ASN1_INT:
        begin
          y := 0;
          neg:=false;
          for n := 1 to ASNSize do
          begin
            x:=Ord(Buffer[Start]);
            if (n=1) and (x>$7F)
              then neg:=true;
            if neg
              then x:=not x;
            y := y * 256 + x;
            Inc(Start);
          end;
          if neg
            then y:=-(y+1);
          Result := IntToStr(y);
        end;
      ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS:
        begin
          y := 0;
          for n := 1 to ASNSize do
          begin
            y := y * 256 + Ord(Buffer[Start]);
            Inc(Start);
          end;
          Result := IntToStr(y);
        end;
      ASN1_OCTSTR, ASN1_OPAQUE:
        begin
          for n := 1 to ASNSize do
          begin
            c := Char(Buffer[Start]);
            Inc(Start);
            s := s + c;
          end;
          Result := s;
        end;
      ASN1_OBJID:
        begin
          for n := 1 to ASNSize do
          begin
            c := Char(Buffer[Start]);
            Inc(Start);
            s := s + c;
          end;
          Result := IdToMib(s);
        end;
      ASN1_IPADDR:
        begin
          s := '';
          for n := 1 to ASNSize do
          begin
            if (n <> 1) then
              s := s + '.';
            y := Ord(Buffer[Start]);
            Inc(Start);
            s := s + IntToStr(y);
          end;
          Result := s;
        end;
    else // NULL
      begin
        Result := '';
        Inc(Start);
        Start := Start + ASNSize;
      end;
    end;
end;

begin
  exit;
  asm
    db 'Synapse ASN.1 library by Lukas Gebauer',0
  end;
end.

