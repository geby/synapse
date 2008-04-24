{==============================================================================|
| Project : Delphree - Synapse                                   | 001.003.005 |
|==============================================================================|
| Content: support for ASN.1 BER coding and decoding                           |
|==============================================================================|
| Copyright (c)1999-2003, Lukas Gebauer                                        |
| All rights reserved.                                                         |
|                                                                              |
| Redistribution and use in source and binary forms, with or without           |
| modification, are permitted provided that the following conditions are met:  |
|                                                                              |
| Redistributions of source code must retain the above copyright notice, this  |
| list of conditions and the following disclaimer.                             |
|                                                                              |
| Redistributions in binary form must reproduce the above copyright notice,    |
| this list of conditions and the following disclaimer in the documentation    |
| and/or other materials provided with the distribution.                       |
|                                                                              |
| Neither the name of Lukas Gebauer nor the names of its contributors may      |
| be used to endorse or promote products derived from this software without    |
| specific prior written permission.                                           |
|                                                                              |
| THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  |
| AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    |
| IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   |
| ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR  |
| ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL       |
| DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR   |
| SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER   |
| CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT           |
| LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY    |
| OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH  |
| DAMAGE.                                                                      |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c) 1999-2003                |
| Portions created by Hernan Sanchez are Copyright (c) 2000.                   |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Hernan Sanchez (hernan.sanchez@iname.com)                                  |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$Q-}
{$WEAKPACKAGEUNIT ON}

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

function ASNEncOIDItem(Value: Integer): string;
function ASNDecOIDItem(var Start: Integer; const Buffer: string): Integer;
function ASNEncLen(Len: Integer): string;
function ASNDecLen(var Start: Integer; const Buffer: string): Integer;
function ASNEncInt(Value: Integer): string;
function ASNEncUInt(Value: Integer): string;
function ASNObject(const Data: string; ASNType: Integer): string;
function ASNItem(var Start: Integer; const Buffer: string;
  var ValueType: Integer): string;
function MibToId(Mib: string): string;
function IdToMib(const Id: string): string;
function IntMibToStr(const Value: string): string;

implementation

{==============================================================================}
function ASNEncOIDItem(Value: Integer): string;
var
  x, xm: Integer;
  b: Boolean;
begin
  x := Value;
  b := False;
  Result := '';
  repeat
    xm := x mod 128;
    x := x div 128;
    if b then
      xm := xm or $80;
    if x > 0 then
      b := True;
    Result := Char(xm) + Result;
  until x = 0;
end;

{==============================================================================}
function ASNDecOIDItem(var Start: Integer; const Buffer: string): Integer;
var
  x: Integer;
  b: Boolean;
begin
  Result := 0;
  repeat
    Result := Result * 128;
    x := Ord(Buffer[Start]);
    Inc(Start);
    b := x > $7F;
    x := x and $7F;
    Result := Result + x;
  until not b;
end;

{==============================================================================}
function ASNEncLen(Len: Integer): string;
var
  x, y: Integer;
begin
  if Len < $80 then
    Result := Char(Len)
  else
  begin
    x := Len;
    Result := '';
    repeat
      y := x mod 256;
      x := x div 256;
      Result := Char(y) + Result;
    until x = 0;
    y := Length(Result);
    y := y or $80;
    Result := Char(y) + Result;
  end;
end;

{==============================================================================}
function ASNDecLen(var Start: Integer; const Buffer: string): Integer;
var
  x, n: Integer;
begin
  x := Ord(Buffer[Start]);
  Inc(Start);
  if x < $80 then
    Result := x
  else
  begin
    Result := 0;
    x := x and $7F;
    for n := 1 to x do
    begin
      Result := Result * 256;
      x := Ord(Buffer[Start]);
      Inc(Start);
      Result := Result + x;
    end;
  end;
end;

{==============================================================================}
function ASNEncInt(Value: Integer): string;
var
  x, y: Cardinal;
  neg: Boolean;
begin
  neg := Value < 0;
  x := Abs(Value);
  if neg then
    x := not (x - 1);
  Result := '';
  repeat
    y := x mod 256;
    x := x div 256;
    Result := Char(y) + Result;
  until x = 0;
  if (not neg) and (Result[1] > #$7F) then
    Result := #0 + Result;
end;

{==============================================================================}
function ASNEncUInt(Value: Integer): string;
var
  x, y: Integer;
  neg: Boolean;
begin
  neg := Value < 0;
  x := Value;
  if neg then
    x := x and $7FFFFFFF;
  Result := '';
  repeat
    y := x mod 256;
    x := x div 256;
    Result := Char(y) + Result;
  until x = 0;
  if neg then
    Result[1] := Char(Ord(Result[1]) or $80);
end;

{==============================================================================}
function ASNObject(const Data: string; ASNType: Integer): string;
begin
  Result := Char(ASNType) + ASNEncLen(Length(Data)) + Data;
end;

{==============================================================================}
function ASNItem(var Start: Integer; const Buffer: string;
  var ValueType: Integer): string;
var
  ASNType: Integer;
  ASNSize: Integer;
  y, n: Integer;
  x: byte;
  s: string;
  c: char;
  neg: Boolean;
  l: Integer;
begin
  Result := '';
  ValueType := ASN1_NULL;
  l := Length(Buffer);
  if l < (Start + 1) then
    Exit;
  ASNType := Ord(Buffer[Start]);
  ValueType := ASNType;
  Inc(Start);
  ASNSize := ASNDecLen(Start, Buffer);
  if (Start + ASNSize - 1) > l then
    Exit;
  if (ASNType and $20) > 0 then
    Result := '$' + IntToHex(ASNType, 2)
  else
    case ASNType of
      ASN1_INT:
        begin
          y := 0;
          neg := False;
          for n := 1 to ASNSize do
          begin
            x := Ord(Buffer[Start]);
            if (n = 1) and (x > $7F) then
              neg := True;
            if neg then
              x := not x;
            y := y * 256 + x;
            Inc(Start);
          end;
          if neg then
            y := -(y + 1);
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
        Start := Start + ASNSize;
      end;
    end;
end;

{==============================================================================}
function MibToId(Mib: string): string;
var
  x: Integer;

  function WalkInt(var s: string): Integer;
  var
    x: Integer;
    t: string;
  begin
    x := Pos('.', s);
    if x < 1 then
    begin
      t := s;
      s := '';
    end
    else
    begin
      t := Copy(s, 1, x - 1);
      s := Copy(s, x + 1, Length(s) - x);
    end;
    Result := StrToIntDef(t, 0);
  end;

begin
  Result := '';
  x := WalkInt(Mib);
  x := x * 40 + WalkInt(Mib);
  Result := ASNEncOIDItem(x);
  while Mib <> '' do
  begin
    x := WalkInt(Mib);
    Result := Result + ASNEncOIDItem(x);
  end;
end;

{==============================================================================}
function IdToMib(const Id: string): string;
var
  x, y, n: Integer;
begin
  Result := '';
  n := 1;
  while Length(Id) + 1 > n do
  begin
    x := ASNDecOIDItem(n, Id);
    if (n - 1) = 1 then
    begin
      y := x div 40;
      x := x mod 40;
      Result := IntToStr(y);
    end;
    Result := Result + '.' + IntToStr(x);
  end;
end;

{==============================================================================}
function IntMibToStr(const Value: string): string;
var
  n, y: Integer;
begin
  y := 0;
  for n := 1 to Length(Value) - 1 do
    y := y * 256 + Ord(Value[n]);
  Result := IntToStr(y);
end;

{==============================================================================}

end.
