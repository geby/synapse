{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
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
|          (Found at URL: http://www.mlp.cz/space/gebauerl/synapse/)           |
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

function ASNEncLen(Len: integer): string;
function ASNDecLen(var Start: integer; Buffer: string): integer;
function ASNEncInt(Len: integer): string;
function ASNObject(Data: string; ASNType: integer): string;
function ASNItem(var Start: integer; Buffer: string): string;

implementation

function ASNEncLen(Len: integer): string;
var
  x, y: integer;
begin
  if (Len < $80) then
    Result := Char(Len)
  else
    if (Len < $FF) then
      Result := Char($81) + Char(Len)
    else
    begin
      x := Len div $FF;
      y := Len mod $FF;
      Result := Char($82) + Char(x) + Char(y);
    end;
end;

function ASNDecLen(var Start: integer; Buffer: string): integer;
var
  x: integer;
begin
  x := Ord(Buffer[Start]);
  if (x < $80) then
  begin
    Inc(Start);
    Result := x;
  end
  else
    if (x = $81) then
    begin
      Inc(Start);
      Result := Ord(Buffer[Start]);
      Inc(Start);
    end
    else
    begin
      Inc(Start);
      x := Ord(Buffer[Start]);
      Inc(Start);
      Result := x * $FF + Ord(Buffer[Start]);
      Inc(Start);
    end;
end;

function ASNEncInt(Len: integer): string;
var
  j, y: integer;
begin
  Result := '';
  j := 0;
  y := Len div $FFFFFF;
  Len := Len - (y * $FFFFFF);
  if ((y > 0) or (j = 1)) then
  begin
    j := 1;
    Result := Result + Char(y);
  end;
  y := Len div $FFFF;
  Len := Len - (y * $FFFF);
  if ((y > 0) or (j = 1)) then
  begin
    j := 1;
    Result := Result + Char(y);
  end;
  y := Len div $FF;
  Len := Len - (y * $FF);
  if ((y > 0) or (j = 1)) then
    Result := Result + Char(y);
  Result := Result + Char(Len);
end;

function ASNObject(Data: string; ASNType: integer): string;
begin
  Result := Char(ASNType) + ASNEncLen(Length(Data)) + Data;
end;

function ASNItem(var Start: integer; Buffer: string): string;
var
  ASNType: integer;
  ASNSize: integer;
  y, n: integer;
  s: string;
  c: char;
begin
  ASNType := Ord(Buffer[Start]);
  Inc(start);
  ASNSize := ASNDecLen(Start, Buffer);
  Result := '';
  if ((ASNType and $20) > 0) then
  begin
    Result := '$' + IntToHex(ASNType, 2);
  end
  else
    case ASNType of
      ASN1_INT, ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS:
        begin
          y := 0;
          for n := 1 to ASNSize do
          begin
            y := y * 256 + Ord(Buffer[Start]);
            Inc(Start);
          end;
          Result := IntToStr(y);
        end;
      ASN1_OCTSTR, $44:
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

end.

