{==============================================================================|
| Project : Delphree - Synapse                                   | 002.001.000 |
|==============================================================================|
| Content: support procedures and functions                                    |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.1 |
| (the "License"); you may not use this file except in compliance with the     |
| License. You may obtain a Copy of the License at http://www.mozilla.org/MPL/ |
|                                                                              |
| Software distributed under the License is distributed on an "AS IS" basis,   |
| WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for |
| the specific language governing rights and limitations under the License.    |
|==============================================================================|
| The Original Code is Synapse Delphi Library.                                 |
|==============================================================================|
| The Initial Developer of the Original Code is Lukas Gebauer (Czech Republic).|
| Portions created by Lukas Gebauer are Copyright (c) 1999,2000,2001.          |
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

unit SynaUtil;

interface

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  Libc;
{$ELSE}
  Windows;
{$ENDIF}

function Timezone: string;
function Rfc822DateTime(t: TDateTime): string;
function CodeInt(Value: Word): string;
function DecodeInt(const Value: string; Index: Integer): Word;
function IsIP(const Value: string): Boolean;
function ReverseIP(Value: string): string;
procedure Dump(const Buffer, DumpFile: string);
function SeparateLeft(const Value, Delimiter: string): string;
function SeparateRight(const Value, Delimiter: string): string;
function GetParameter(const Value, Parameter: string): string;
function GetEmailAddr(const Value: string): string;
function GetEmailDesc(Value: string): string;
function StrToHex(const Value: string): string;
function IntToBin(Value: Integer; Digits: Byte): string;
function BinToInt(const Value: string): Integer;
function ParseURL(URL: string; var Prot, User, Pass, Host, Port, Path,
  Para: string): string;
function StringReplace(Value, Search, Replace: string): string;
function RPos(const Sub, Value: String): Integer;
function Fetch(var Value: string; const Delimiter: string): string;

implementation

{==============================================================================}

function Timezone: string;
{$IFDEF LINUX}
var
  t: TTime_T;
  UT: TUnixTime;
  bias: Integer;
  h, m: Integer;
begin
  __time(@T);
  localtime_r(@T, UT);
  bias := ut.__tm_gmtoff div 60;
  if bias >= 0 then
    Result := '+'
  else
    Result := '-';
{$ELSE}
var
  zoneinfo: TTimeZoneInformation;
  bias: Integer;
  h, m: Integer;
begin
  case GetTimeZoneInformation(Zoneinfo) of
    2:
      bias := zoneinfo.Bias + zoneinfo.DaylightBias;
    1:
      bias := zoneinfo.Bias + zoneinfo.StandardBias;
  else
    bias := zoneinfo.Bias;
  end;
  if bias <= 0 then
    Result := '+'
  else
    Result := '-';
{$ENDIF}
  bias := Abs(bias);
  h := bias div 60;
  m := bias mod 60;
  Result := Result + Format('%.2d%.2d', [h, m]);
end;

{==============================================================================}

function Rfc822DateTime(t: TDateTime): string;
var
  I: Integer;
  SaveDayNames: array[1..7] of string;
  SaveMonthNames: array[1..12] of string;
const
  MyDayNames: array[1..7] of string =
  ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MyMonthNames: array[1..12] of string =
  ('Jan', 'Feb', 'Mar', 'Apr',
    'May', 'Jun', 'Jul', 'Aug',
    'Sep', 'Oct', 'Nov', 'Dec');
begin
  if ShortDayNames[1] = MyDayNames[1] then
    Result := FormatDateTime('ddd, d mmm yyyy hh:mm:ss', t)
  else
  begin
    for I := Low(ShortDayNames) to High(ShortDayNames) do
    begin
      SaveDayNames[I] := ShortDayNames[I];
      ShortDayNames[I] := MyDayNames[I];
    end;
    for I := Low(ShortMonthNames) to High(ShortMonthNames) do
    begin
      SaveMonthNames[I] := ShortMonthNames[I];
      ShortMonthNames[I] := MyMonthNames[I];
    end;
    Result := FormatDateTime('ddd, d mmm yyyy hh:mm:ss', t);
    for I := Low(ShortDayNames) to High(ShortDayNames) do
      ShortDayNames[I] := SaveDayNames[I];
    for I := Low(ShortMonthNames) to High(ShortMonthNames) do
      ShortMonthNames[I] := SaveMonthNames[I];
  end;
  Result := Result + ' ' + Timezone;
end;

{==============================================================================}

function CodeInt(Value: Word): string;
begin
  Result := Chr(Hi(Value)) + Chr(Lo(Value))
end;

{==============================================================================}

function DecodeInt(const Value: string; Index: Integer): Word;
var
  x, y: Byte;
begin
  if Length(Value) > Index then
    x := Ord(Value[Index])
  else
    x := 0;
  if Length(Value) > (Index + 1) then
    y := Ord(Value[Index + 1])
  else
    y := 0;
  Result := x * 256 + y;
end;

{==============================================================================}

function IsIP(const Value: string): Boolean;
var
  n, x: Integer;
begin
  Result := true;
  x := 0;
  for n := 1 to Length(Value) do
    if not (Value[n] in ['0'..'9', '.']) then
    begin
      Result := False;
      Break;
    end
    else
    begin
      if Value[n] = '.' then
        Inc(x);
    end;
  if x <> 3 then
    Result := False;
end;

{==============================================================================}

function ReverseIP(Value: string): string;
var
  x: Integer;
begin
  Result := '';
  repeat
    x := LastDelimiter('.', Value);
    Result := Result + '.' + Copy(Value, x + 1, Length(Value) - x);
    Delete(Value, x, Length(Value) - x + 1);
  until x < 1;
  if Length(Result) > 0 then
    if Result[1] = '.' then
      Delete(Result, 1, 1);
end;

{==============================================================================}

procedure Dump(const Buffer, DumpFile: string);
var
  n: Integer;
  s: string;
  f: Text;
begin
  s := '';
  for n := 1 to Length(Buffer) do
    s := s + ' +#$' + IntToHex(Ord(Buffer[n]), 2);
  AssignFile(f, DumpFile);
  if FileExists(DumpFile) then
    DeleteFile(PChar(DumpFile));
  Rewrite(f);
  try
    Writeln(f, s);
  finally
    CloseFile(f);
  end;
end;

{==============================================================================}

function SeparateLeft(const Value, Delimiter: string): string;
var
  x: Integer;
begin
  x := Pos(Delimiter, Value);
  if x < 1 then
    Result := Trim(Value)
  else
    Result := Trim(Copy(Value, 1, x - 1));
end;

{==============================================================================}

function SeparateRight(const Value, Delimiter: string): string;
var
  x: Integer;
begin
  x := Pos(Delimiter, Value);
  if x > 0 then
    x := x + Length(Delimiter) - 1;
  Result := Trim(Copy(Value, x + 1, Length(Value) - x));
end;

{==============================================================================}

function GetParameter(const Value, Parameter: string): string;
var
  x, x1: Integer;
  s: string;
begin
  x := Pos(UpperCase(Parameter), UpperCase(Value));
  Result := '';
  if x > 0 then
  begin
    s := Copy(Value, x + Length(Parameter), Length(Value)
      - (x + Length(Parameter)) + 1);
    s := Trim(s);
    x1 := Length(s);
    if Length(s) > 1 then
    begin
      if s[1] = '"' then
      begin
        s := Copy(s, 2, Length(s) - 1);
        x := Pos('"', s);
        if x > 0 then
          x1 := x - 1;
      end
      else
      begin
        x := Pos(' ', s);
        if x > 0 then
          x1 := x - 1;
      end;
    end;
    Result := Copy(s, 1, x1);
  end;
end;

{==============================================================================}

function GetEmailAddr(const Value: string): string;
var
  s: string;
begin
  s := SeparateRight(Value, '<');
  s := SeparateLeft(s, '>');
  Result := Trim(s);
end;

{==============================================================================}

function GetEmailDesc(Value: string): string;
var
  s: string;
begin
  Value := Trim(Value);
  s := SeparateRight(Value, '"');
  if s <> Value then
    s := SeparateLeft(s, '"')
  else
  begin
    s := SeparateRight(Value, '(');
    if s <> Value then
      s := SeparateLeft(s, ')')
    else
    begin
      s := SeparateLeft(Value, '<');
      if s = Value then
        s := '';
    end;
  end;
  Result := Trim(s);
end;

{==============================================================================}

function StrToHex(const Value: string): string;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Value) do
    Result := Result + IntToHex(Byte(Value[n]), 2);
  Result := LowerCase(Result);
end;

{==============================================================================}

function IntToBin(Value: Integer; Digits: Byte): string;
var
  x, y, n: Integer;
begin
  Result := '';
  x := Value;
  repeat
    y := x mod 2;
    x := x div 2;
    if y > 0 then
      Result := '1' + Result
    else
      Result := '0' + Result;
  until x = 0;
  x := Length(Result);
  for n := x to Digits - 1 do
    Result := '0' + Result;
end;

{==============================================================================}

function BinToInt(const Value: string): Integer;
var
  n: Integer;
begin
  Result := 0;
  for n := 1 to Length(Value) do
  begin
    if Value[n] = '0' then
      Result := Result * 2
    else
      if Value[n] = '1' then
        Result := Result * 2 + 1
      else
        Break;
  end;
end;

{==============================================================================}

function ParseURL(URL: string; var Prot, User, Pass, Host, Port, Path,
  Para: string): string;
var
  x: Integer;
  sURL: string;
  s: string;
  s1, s2: string;
begin
  Prot := 'http';
  User := '';
  Pass := '';
  Port := '80';
  Para := '';

  x := Pos('://', URL);
  if x > 0 then
  begin
    Prot := SeparateLeft(URL, '://');
    sURL := SeparateRight(URL, '://');
  end
  else
    sURL := URL;
  x := Pos('@', sURL);
  if x > 0 then
  begin
    s := SeparateLeft(sURL, '@');
    sURL := SeparateRight(sURL, '@');
    x := Pos(':', s);
    if x > 0 then
    begin
      User := SeparateLeft(s, ':');
      Pass := SeparateRight(s, ':');
    end
    else
      User := s;
  end;
  x := Pos('/', sURL);
  if x > 0 then
  begin
    s1 := SeparateLeft(sURL, '/');
    s2 := SeparateRight(sURL, '/');
  end
  else
  begin
    s1 := sURL;
    s2 := '';
  end;
  x := Pos(':', s1);
  if x > 0 then
  begin
    Host := SeparateLeft(s1, ':');
    Port := SeparateRight(s1, ':');
  end
  else
    Host := s1;
  Result := '/' + s2;
  x := Pos('?', s2);
  if x > 0 then
  begin
    Path := '/' + SeparateLeft(s2, '?');
    Para := SeparateRight(s2, '?');
  end
  else
    Path := '/' + s2;
  if Host = '' then
    Host := 'localhost';
end;

{==============================================================================}

function StringReplace(Value, Search, Replace: string): string;
var
  x, l, ls, lr: Integer;
begin
  if (Value = '') or (Search = '') then
  begin
    Result := Value;
    Exit;
  end;
  ls := Length(Search);
  lr := Length(Replace);
  Result := '';
  x := Pos(Search, Value);
  while x > 0 do
  begin
    l := Length(Result);
    SetLength(Result, l + x - 1);
    Move(Pointer(Value)^, Pointer(@Result[l + 1])^, x - 1);
//      Result:=Result+Copy(Value,1,x-1);
    l := Length(Result);
    SetLength(Result, l + lr);
    Move(Pointer(Replace)^, Pointer(@Result[l + 1])^, lr);
//      Result:=Result+Replace;
    Delete(Value, 1, x - 1 + ls);
    x := Pos(Search, Value);
  end;
  Result := Result + Value;
end;

{==============================================================================}

function RPos(const Sub, Value: String): Integer;
var
  n: Integer;
  l: Integer;
begin
  result := 0;
  l := Length(Sub);
  for n := Length(Value) - l + 1 downto 1 do
  begin
    if Copy(Value, n, l) = Sub then
    begin
      result := n;
      break;
    end;
  end;
end;

{==============================================================================}

function Fetch(var Value: string; const Delimiter: string): string;
begin
  Result := SeparateLeft(Value, Delimiter);
  Value := SeparateRight(Value, Delimiter);
end;

end.
