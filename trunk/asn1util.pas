{==============================================================================|
| Project : Delphree - Synapse                                   | 001.003.000 |
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
  SysUtils;

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
Function MibToId(mib:string):string;
Function IdToMib(id:string):string;
Function IntMibToStr(int:string):string;
function IPToID(Host: string): string;

implementation

{==============================================================================}
{ASNEncOIDitem}
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

{==============================================================================}
{ASNDecOIDitem}
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

{==============================================================================}
{ASNEncLen}
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

{==============================================================================}
{ASNDecLen}
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

{==============================================================================}
{ASNEncInt}
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

{==============================================================================}
{ASNEncUInt}
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

{==============================================================================}
{ASNObject}
function ASNObject(Data: string; ASNType: integer): string;
begin
  Result := Char(ASNType) + ASNEncLen(Length(Data)) + Data;
end;

{==============================================================================}
{ASNItem}
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

{==============================================================================}
{MibToId}
function MibToId(mib:string):string;
var
  x:integer;

  Function walkInt(var s:string):integer;
  var
    x:integer;
    t:string;
  begin
    x:=pos('.',s);
    if x<1 then
      begin
        t:=s;
        s:='';
      end
      else
      begin
        t:=copy(s,1,x-1);
        s:=copy(s,x+1,length(s)-x);
      end;
    result:=StrToIntDef(t,0);
  end;
begin
  result:='';
  x:=walkint(mib);
  x:=x*40+walkint(mib);
  result:=ASNEncOIDItem(x);
  while mib<>'' do
    begin
      x:=walkint(mib);
      result:=result+ASNEncOIDItem(x);
    end;
end;

{==============================================================================}
{IdToMib}
Function IdToMib(id:string):string;
var
  x,y,n:integer;
begin
  result:='';
  n:=1;
  while length(id)+1>n do
    begin
      x:=ASNDecOIDItem(n,id);
      if (n-1)=1 then
        begin
          y:=x div 40;
          x:=x mod 40;
          result:=IntTostr(y);
        end;
      result:=result+'.'+IntToStr(x);
    end;
end;

{==============================================================================}
{IntMibToStr}
Function IntMibToStr(int:string):string;
Var
  n,y:integer;
begin
  y:=0;
  for n:=1 to length(int)-1 do
    y:=y*256+ord(int[n]);
  result:=IntToStr(y);
end;

{==============================================================================}
{IPToID} //Hernan Sanchez
function IPToID(Host: string): string;
var
  s, t: string;
  i, x: integer;
begin
  Result := '';
  for x:= 1 to 3 do
    begin
      t := '';
      s := StrScan(PChar(Host), '.');
      t := Copy(Host, 1, (Length(Host) - Length(s)));
      Delete(Host, 1, (Length(Host) - Length(s) + 1));
      i := StrTointDef(t, 0);
      Result := Result + Chr(i);
    end;
  i := StrTointDef(Host, 0);
  Result := Result + Chr(i);
end;

{==============================================================================}

begin
  exit;
  asm
    db 'Synapse ASN.1 library by Lukas Gebauer',0
  end;
end.

