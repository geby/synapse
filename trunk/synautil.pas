{==============================================================================|
| Project : Ararat Synapse                                       | 004.000.002 |
|==============================================================================|
| Content: support procedures and functions                                    |
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
| Portions created by Lukas Gebauer are Copyright (c) 1999-2003.               |
| Portions created by Hernan Sanchez are Copyright (c) 2000.                   |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Hernan Sanchez (hernan.sanchez@iname.com)                                  |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$R-}
{$H+}

unit synautil;

interface

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  Libc;
{$ELSE}
  Windows;
{$ENDIF}

function TimeZoneBias: integer;
function TimeZone: string;
function Rfc822DateTime(t: TDateTime): string;
function CDateTime(t: TDateTime): string;
function SimpleDateTime(t: TDateTime): string;
function AnsiCDateTime(t: TDateTime): string;
function GetMonthNumber(Value: string): integer;
function GetTimeFromStr(Value: string): TDateTime;
function GetDateMDYFromStr(Value: string): TDateTime;
function DecodeRfcDateTime(Value: string): TDateTime;
function GetUTTime: TDateTime;
function SetUTTime(Newdt: TDateTime): Boolean;
function GetTick: ULong;
function TickDelta(TickOld, TickNew: ULong): ULong;
function CodeInt(Value: Word): string;
function DecodeInt(const Value: string; Index: Integer): Word;
function IsIP(const Value: string): Boolean;
function IsIP6(const Value: string): Boolean;
function IPToID(Host: string): string;
function DumpStr(const Buffer: string): string;
function DumpExStr(const Buffer: string): string;
procedure Dump(const Buffer, DumpFile: string);
procedure DumpEx(const Buffer, DumpFile: string);
function SeparateLeft(const Value, Delimiter: string): string;
function SeparateRight(const Value, Delimiter: string): string;
function GetParameter(const Value, Parameter: string): string;
procedure ParseParametersEx(Value, Delimiter: string; const Parameters: TStrings);
procedure ParseParameters(Value: string; const Parameters: TStrings);
function IndexByBegin(Value: string; const List: TStrings): integer;
function GetEmailAddr(const Value: string): string;
function GetEmailDesc(Value: string): string;
function StrToHex(const Value: string): string;
function IntToBin(Value: Integer; Digits: Byte): string;
function BinToInt(const Value: string): Integer;
function ParseURL(URL: string; var Prot, User, Pass, Host, Port, Path,
  Para: string): string;
function ReplaceString(Value, Search, Replace: string): string;
function RPosEx(const Sub, Value: string; From: integer): Integer;
function RPos(const Sub, Value: String): Integer;
function Fetch(var Value: string; const Delimiter: string): string;
function FetchEx(var Value: string; const Delimiter, Quotation: string): string;
function IsBinaryString(const Value: string): Boolean;
function PosCRLF(const Value: string; var Terminator: string): integer;
Procedure StringsTrim(const value: TStrings);
function PosFrom(const SubStr, Value: String; From: integer): integer;
function IncPoint(const p: pointer; Value: integer): pointer;
function GetBetween(const PairBegin, PairEnd, Value: string): string;
function CountOfChar(const Value: string; Chr: char): integer;
function UnquoteStr(const Value: string; Quote: Char): string;

implementation

{==============================================================================}

const
  MyDayNames: array[1..7] of string =
    ('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat');
  MyMonthNames: array[1..12] of string =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
     'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');

{==============================================================================}

function TimeZoneBias: integer;
{$IFDEF LINUX}
var
  t: TTime_T;
  UT: TUnixTime;
begin
{$IFNDEF FPC}
  __time(@T);
  localtime_r(@T, UT);
{$ELSE}
  __time(T);
  localtime_r(T, UT);
{$ENDIF}
  Result := ut.__tm_gmtoff div 60;
{$ELSE}
var
  zoneinfo: TTimeZoneInformation;
  bias: Integer;
begin
  case GetTimeZoneInformation(Zoneinfo) of
    2:
      bias := zoneinfo.Bias + zoneinfo.DaylightBias;
    1:
      bias := zoneinfo.Bias + zoneinfo.StandardBias;
  else
    bias := zoneinfo.Bias;
  end;
  Result := bias * (-1);
{$ENDIF}
end;

{==============================================================================}

function TimeZone: string;
var
  bias: Integer;
  h, m: Integer;
begin
  bias := TimeZoneBias;
  if bias >= 0 then
    Result := '+'
  else
    Result := '-';
  bias := Abs(bias);
  h := bias div 60;
  m := bias mod 60;
  Result := Result + Format('%.2d%.2d', [h, m]);
end;

{==============================================================================}

function Rfc822DateTime(t: TDateTime): string;
var
  wYear, wMonth, wDay: word;
begin
  DecodeDate(t, wYear, wMonth, wDay);
  Result := Format('%s, %d %s %s %s', [MyDayNames[DayOfWeek(t)], wDay,
    MyMonthNames[wMonth], FormatDateTime('yyyy hh:nn:ss', t), TimeZone]);
end;

{==============================================================================}

function CDateTime(t: TDateTime): string;
var
  wYear, wMonth, wDay: word;
begin
  DecodeDate(t, wYear, wMonth, wDay);
  Result:= Format('%s %2d %s', [MyMonthNames[wMonth], wDay,
    FormatDateTime('hh:nn:ss', t)]);
end;

{==============================================================================}

function SimpleDateTime(t: TDateTime): string;
begin
  Result := FormatDateTime('yymmdd hhnnss', t);
end;

{==============================================================================}

function AnsiCDateTime(t: TDateTime): string;
var
  wYear, wMonth, wDay: word;
begin
  DecodeDate(t, wYear, wMonth, wDay);
  Result := Format('%s %s %d %s', [MyDayNames[DayOfWeek(t)], MyMonthNames[wMonth],
    wDay, FormatDateTime('hh:nn:ss yyyy ', t)]);
end;

{==============================================================================}

function DecodeTimeZone(Value: string; var Zone: integer): Boolean;
var
  x: integer;
  zh, zm: integer;
  s: string;
begin
  Result := false;
  s := Value;
  if (Pos('+', s) = 1) or (Pos('-',s) = 1) then
  begin
    if s = '-0000' then
      Zone := TimeZoneBias
    else
      if Length(s) > 4 then
      begin
        zh := StrToIntdef(s[2] + s[3], 0);
        zm := StrToIntdef(s[4] + s[5], 0);
        zone := zh * 60 + zm;
        if s[1] = '-' then
          zone := zone * (-1);
      end;
    Result := True;
  end
  else
  begin
    x := 32767;
    if s = 'NZDT' then x := 13;
    if s = 'IDLE' then x := 12;
    if s = 'NZST' then x := 12;
    if s = 'NZT' then x := 12;
    if s = 'EADT' then x := 11;
    if s = 'GST' then x := 10;
    if s = 'JST' then x := 9;
    if s = 'CCT' then x := 8;
    if s = 'WADT' then x := 8;
    if s = 'WAST' then x := 7;
    if s = 'ZP6' then x := 6;
    if s = 'ZP5' then x := 5;
    if s = 'ZP4' then x := 4;
    if s = 'BT' then x := 3;
    if s = 'EET' then x := 2;
    if s = 'MEST' then x := 2;
    if s = 'MESZ' then x := 2;
    if s = 'SST' then x := 2;
    if s = 'FST' then x := 2;
    if s = 'CEST' then x := 2;
    if s = 'CET' then x := 1;
    if s = 'FWT' then x := 1;
    if s = 'MET' then x := 1;
    if s = 'MEWT' then x := 1;
    if s = 'SWT' then x := 1;
    if s = 'UT' then x := 0;
    if s = 'UTC' then x := 0;
    if s = 'GMT' then x := 0;
    if s = 'WET' then x := 0;
    if s = 'WAT' then x := -1;
    if s = 'BST' then x := -1;
    if s = 'AT' then x := -2;
    if s = 'ADT' then x := -3;
    if s = 'AST' then x := -4;
    if s = 'EDT' then x := -4;
    if s = 'EST' then x := -5;
    if s = 'CDT' then x := -5;
    if s = 'CST' then x := -6;
    if s = 'MDT' then x := -6;
    if s = 'MST' then x := -7;
    if s = 'PDT' then x := -7;
    if s = 'PST' then x := -8;
    if s = 'YDT' then x := -8;
    if s = 'YST' then x := -9;
    if s = 'HDT' then x := -9;
    if s = 'AHST' then x := -10;
    if s = 'CAT' then x := -10;
    if s = 'HST' then x := -10;
    if s = 'EAST' then x := -10;
    if s = 'NT' then x := -11;
    if s = 'IDLW' then x := -12;
    if x <> 32767 then
    begin
      zone := x * 60;
      Result := True;
    end;
  end;
end;

{==============================================================================}

function GetMonthNumber(Value: string): integer;
var
  n: integer;
begin
  Result := 0;
  Value := Uppercase(Value);
  for n := 1 to 12 do
    if Value = uppercase(MyMonthNames[n]) then
    begin
      Result := n;
      Break;
    end;
end;

{==============================================================================}

function GetTimeFromStr(Value: string): TDateTime;
var
  x: integer;
begin
  x := rpos(':', Value);
  if (x > 0) and ((Length(Value) - x) > 2) then
    Value := Copy(Value, 1, x + 2);
  Value := ReplaceString(Value, ':', TimeSeparator);
  Result := 0;
  try
    Result := StrToTime(Value);
  except
    on Exception do ;
  end;
end;

{==============================================================================}

function GetDateMDYFromStr(Value: string): TDateTime;
var
  wYear, wMonth, wDay: word;
  s: string;
begin
  Result := 0;
  s := Fetch(Value, '-');
  wMonth := StrToIntDef(s, 12);
  s := Fetch(Value, '-');
  wDay := StrToIntDef(s, 30);
  wYear := StrToIntDef(Value, 1899);
  if wYear < 1000 then
    if (wYear > 99) then
      wYear := wYear + 1900
    else
      if wYear > 50 then
        wYear := wYear + 1900
      else
        wYear := wYear + 2000;
  try
    Result := EncodeDate(wYear, wMonth, wDay);
  except
    on Exception do ;
  end;
end;

{==============================================================================}

function DecodeRfcDateTime(Value: string): TDateTime;
var
  day, month, year: Word;
  zone: integer;
  x, y: integer;
  s: string;
  t: TDateTime;
begin
// ddd, d mmm yyyy hh:mm:ss
// ddd, d mmm yy hh:mm:ss
// ddd, mmm d yyyy hh:mm:ss
// ddd mmm dd hh:mm:ss yyyy
// Sun, 06 Nov 1994 08:49:37 GMT    ; RFC 822, updated by RFC 1123
// Sunday, 06-Nov-94 08:49:37 GMT   ; RFC 850, obsoleted by RFC 1036
// Sun Nov  6 08:49:37 1994         ; ANSI C's asctime() Format

  Result := 0;
  if Value = '' then
    Exit;
  day := 0;
  month := 0;
  year := 0;
  zone := 0;
  Value := ReplaceString(Value, ' -', ' #');
  Value := ReplaceString(Value, '-', ' ');
  Value := ReplaceString(Value, ' #', ' -');
  while Value <> '' do
  begin
    s := Fetch(Value, ' ');
    s := uppercase(s);
    // timezone
    if DecodetimeZone(s, x) then
    begin
      zone := x;
      continue;
    end;
    x := StrToIntDef(s, 0);
    // day or year
    if x > 0 then
      if (x < 32) and (day = 0) then
      begin
        day := x;
        continue;
      end
      else
      begin
        year := x;
        if year < 32 then
          year := year + 2000;
        if year < 1000 then
         year := year + 1900;
        continue;
      end;
    // time
    if rpos(':', s) > Pos(':', s) then
    begin
      t := GetTimeFromStr(s);
      if t <> 0 then
        Result := t;
      continue;
    end;
    //timezone daylight saving time
    if s = 'DST' then
    begin
      zone := zone + 60;
      continue;
    end;
    // month
    y := GetMonthNumber(s);
    if y > 0 then
      month := y;
  end;
  if year = 0 then
    year := 1980;
  if (month < 1) or (month > 12) then
    month := 1;
  if (day < 1) or (day > 31) then
    day := 1;
  Result := Result + Encodedate(year, month, day);
  zone := zone - TimeZoneBias;
  x := zone div 1440;
  Result := Result - x;
  zone := zone mod 1440;
  t := EncodeTime(Abs(zone) div 60, Abs(zone) mod 60, 0, 0);
  if zone < 0 then
    t := 0 - t;
  Result := Result - t;
end;

{==============================================================================}

function GetUTTime: TDateTime;
{$IFNDEF LINUX}
{$IFNDEF FPC}
var
  st: TSystemTime;
begin
  GetSystemTime(st);
  result := SystemTimeToDateTime(st);
{$ELSE}
var
  st: SysUtils.TSystemTime;
  stw: Windows.TSystemTime;
begin
  GetSystemTime(stw);
  st.Year := stw.wYear;
  st.Month := stw.wMonth;
  st.Day := stw.wDay;
  st.Hour := stw.wHour;
  st.Minute := stw.wMinute;
  st.Second := stw.wSecond;
  st.Millisecond := stw.wMilliseconds;
  result := SystemTimeToDateTime(st);
{$ENDIF}
{$ELSE}
var
  TV: TTimeVal;
  TZ: Ttimezone;
begin
  TZ.tz_minuteswest := 0;
  TZ.tz_dsttime := 0;
  gettimeofday(TV, TZ);
  Result := UnixDateDelta + (TV.tv_sec + TV.tv_usec / 1000000) / 86400;
{$ENDIF}
end;

{==============================================================================}

function SetUTTime(Newdt: TDateTime): Boolean;
{$IFNDEF LINUX}
{$IFNDEF FPC}
var
  st: TSystemTime;
begin
  DateTimeToSystemTime(newdt,st);
  Result := SetSystemTime(st);
{$ELSE}
var
  st: SysUtils.TSystemTime;
  stw: Windows.TSystemTime;
begin
  DateTimeToSystemTime(newdt,st);
  stw.wYear := st.Year;
  stw.wMonth := st.Month;
  stw.wDay := st.Day;
  stw.wHour := st.Hour;
  stw.wMinute := st.Minute;
  stw.wSecond := st.Second;
  stw.wMilliseconds := st.Millisecond;
  Result := SetSystemTime(stw);
{$ENDIF}
{$ELSE}
var
  TV: TTimeVal;
  d: double;
  TZ: Ttimezone;
begin
  Result := false;
  TZ.tz_minuteswest := 0;
  TZ.tz_dsttime := 0;
  gettimeofday(TV, TZ);
  d := (newdt - UnixDateDelta) * 86400;
  TV.tv_sec := trunc(d);
  TV.tv_usec := trunc(frac(d) * 1000000);
  Result := settimeofday(TV, TZ) <> -1;
{$ENDIF}
end;

{==============================================================================}

{$IFDEF LINUX}
function GetTick: ULong;
var
  Stamp: TTimeStamp;
begin
  Stamp := DateTimeToTimeStamp(Now);
  Result := Stamp.Time;
end;
{$ELSE}
function GetTick: ULong;
begin
  Result := Windows.GetTickCount;
end;
{$ENDIF}

{==============================================================================}

function TickDelta(TickOld, TickNew: ULong): ULong;
begin
//if DWord is signed type (older Deplhi),
// then it not work properly on differencies larger then maxint!
  Result := 0;
  if TickOld <> TickNew then
  begin
    if TickNew < TickOld then
    begin
      TickNew := TickNew + ULong(MaxInt) + 1;
      TickOld := TickOld + ULong(MaxInt) + 1;
    end;
    Result := TickNew - TickOld;
    if TickNew < TickOld then
      if Result > 0 then
        Result := 0 - Result;
  end;
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
  if Length(Value) >= (Index + 1) then
    y := Ord(Value[Index + 1])
  else
    y := 0;
  Result := x * 256 + y;
end;

{==============================================================================}

function IsIP(const Value: string): Boolean;
var
  TempIP: string;
  function ByteIsOk(const Value: string): Boolean;
  var
    x, n: integer;
  begin
    x := StrToIntDef(Value, -1);
    Result := (x >= 0) and (x < 256);
    // X may be in correct range, but value still may not be correct value!
    // i.e. "$80"
    if Result then
      for n := 1 to length(Value) do
        if not (Value[n] in ['0'..'9']) then
        begin
          Result := False;
          Break;
        end;
  end;
begin
  TempIP := Value;
  Result := False;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if not ByteIsOk(Fetch(TempIP, '.')) then
    Exit;
  if ByteIsOk(TempIP) then
    Result := True;
end;

{==============================================================================}

function IsIP6(const Value: string): Boolean;
var
  TempIP: string;
  s,t: string;
  x: integer;
  partcount: integer;
  zerocount: integer;
  First: Boolean;
begin
  TempIP := Value;
  Result := False;
  partcount := 0;
  zerocount := 0;
  First := True;
  while tempIP <> '' do
  begin
    s := fetch(TempIP, ':');
    if not(First) and (s = '') then
      Inc(zerocount);
    First := False;
    if zerocount > 1 then
      break;
    Inc(partCount);
    if s = '' then
      Continue;
    if partCount > 8 then
      break;
    if tempIP = '' then
    begin
      t := SeparateRight(s, '%');
      s := SeparateLeft(s, '%');
      x := StrToIntDef('$' + t, -1);
      if (x < 0) or (x > $ffff) then
        break;
    end;
    x := StrToIntDef('$' + s, -1);
    if (x < 0) or (x > $ffff) then
      break;
    if tempIP = '' then
      Result := True;
  end;
end;

{==============================================================================}
//Hernan Sanchez
function IPToID(Host: string): string;
var
  s, t: string;
  i, x: Integer;
begin
  Result := '';
  for x := 1 to 3 do
  begin
    t := '';
    s := StrScan(PChar(Host), '.');
    t := Copy(Host, 1, (Length(Host) - Length(s)));
    Delete(Host, 1, (Length(Host) - Length(s) + 1));
    i := StrToIntDef(t, 0);
    Result := Result + Chr(i);
  end;
  i := StrToIntDef(Host, 0);
  Result := Result + Chr(i);
end;

{==============================================================================}

function DumpStr(const Buffer: string): string;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Buffer) do
    Result := Result + ' +#$' + IntToHex(Ord(Buffer[n]), 2);
end;

{==============================================================================}

function DumpExStr(const Buffer: string): string;
var
  n: Integer;
  x: Byte;
begin
  Result := '';
  for n := 1 to Length(Buffer) do
  begin
    x := Ord(Buffer[n]);
    if x in [65..90, 97..122] then
      Result := Result + ' +''' + char(x) + ''''
    else
      Result := Result + ' +#$' + IntToHex(Ord(Buffer[n]), 2);
  end;
end;

{==============================================================================}

procedure Dump(const Buffer, DumpFile: string);
var
  f: Text;
begin
  AssignFile(f, DumpFile);
  if FileExists(DumpFile) then
    DeleteFile(PChar(DumpFile));
  Rewrite(f);
  try
    Writeln(f, DumpStr(Buffer));
  finally
    CloseFile(f);
  end;
end;

{==============================================================================}

procedure DumpEx(const Buffer, DumpFile: string);
var
  f: Text;
begin
  AssignFile(f, DumpFile);
  if FileExists(DumpFile) then
    DeleteFile(PChar(DumpFile));
  Rewrite(f);
  try
    Writeln(f, DumpExStr(Buffer));
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
  x: Integer;
  s: string;
begin
  x := Pos(UpperCase(Parameter), UpperCase(Value));
  Result := '';
  if x > 0 then
  begin
    s := Copy(Value, x + Length(Parameter), Length(Value)
      - (x + Length(Parameter)) + 1);
    s := Trim(s);
    if Length(s) > 1 then
    begin
      x := pos(';', s);
      if x > 0 then
        s := Copy(s, 1, x - 1);
      Result := UnquoteStr(s, '"');
    end;
  end;
end;

{==============================================================================}

procedure ParseParametersEx(Value, Delimiter: string; const Parameters: TStrings);
var
  s: string;
begin
  Parameters.Clear;
  while Value <> '' do
  begin
    s := Fetch(Value, Delimiter);
    Parameters.Add(s);
  end;
end;

{==============================================================================}

procedure ParseParameters(Value: string; const Parameters: TStrings);
begin
  ParseParametersEx(Value, ';', Parameters);
end;

{==============================================================================}

function IndexByBegin(Value: string; const List: TStrings): integer;
var
  n: integer;
  s: string;
begin
  Result := -1;
  Value := uppercase(Value);
  for n := 0 to List.Count -1 do
  begin
    s := UpperCase(List[n]);
    if Pos(Value, s) = 1 then
    begin
      Result := n;
      Break;
    end;
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
    s := SeparateLeft(Value, '<');
    if s = Value then
    begin
      s := SeparateRight(Value, '(');
      if s <> Value then
        s := SeparateLeft(s, ')')
      else
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
  x, y: Integer;
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
  if UpperCase(Prot) = 'HTTPS' then
    Port := '443';
  if UpperCase(Prot) = 'FTP' then
    Port := '21';
  x := Pos('@', sURL);
  y := Pos('/', sURL);
  if (x > 0) and ((x < y) or (y < 1))then
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
  if Pos('[', s1) = 1 then
  begin
    Host := Separateleft(s1, ']');
    Delete(Host, 1, 1);
    s1 := SeparateRight(s1, ']');
    if Pos(':', s1) = 1 then
      Port := SeparateRight(s1, ':');
  end
  else
  begin
    x := Pos(':', s1);
    if x > 0 then
    begin
      Host := SeparateLeft(s1, ':');
      Port := SeparateRight(s1, ':');
    end
    else
      Host := s1;
  end;
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

function ReplaceString(Value, Search, Replace: string): string;
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

function RPosEx(const Sub, Value: string; From: integer): Integer;
var
  n: Integer;
  l: Integer;
begin
  result := 0;
  l := Length(Sub);
  for n := From - l + 1 downto 1 do
  begin
    if Copy(Value, n, l) = Sub then
    begin
      result := n;
      break;
    end;
  end;
end;

{==============================================================================}

function RPos(const Sub, Value: String): Integer;
begin
  Result := RPosEx(Sub, Value, Length(Value));
end;

{==============================================================================}

function Fetch(var Value: string; const Delimiter: string): string;
var
  s: string;
begin
  Result := SeparateLeft(Value, Delimiter);
  s := SeparateRight(Value, Delimiter);
  if s = Value then
    Value := ''
  else
    Value := Trim(s);
  Result := Trim(Result);
end;

{==============================================================================}

function FetchEx(var Value: string; const Delimiter, Quotation: string): string;
var
  n: integer;
  b: Boolean;
begin
  Result := '';
  b := False;
  n := 1;
  while n <= Length(Value) do
  begin
    if b then
    begin
      if Pos(Quotation, Value) = 1 then
        b := False;
      Result := Result + Value[1];
      Delete(Value, 1, 1);
    end
    else
    begin
      if Pos(Delimiter, Value) = 1 then
      begin
        Delete(Value, 1, Length(delimiter));
        break;
      end;
      b := Pos(Quotation, Value) = 1;
      Result := Result + Value[1];
      Delete(Value, 1, 1);
    end;
  end;
  Result := Trim(Result);
end;

{==============================================================================}

function IsBinaryString(const Value: string): Boolean;
var
  n: integer;
begin
  Result := False;
  for n := 1 to Length(Value) do
    if Value[n] in [#0..#8, #10..#31] then
    begin
      Result := True;
      Break;
    end;
end;

{==============================================================================}

function PosCRLF(const Value: string; var Terminator: string): integer;
var
  p1, p2, p3, p4: integer;
const
  t1 = #$0d + #$0a;
  t2 = #$0a + #$0d;
  t3 = #$0d;
  t4 = #$0a;
begin
  Terminator := '';
  p1 := Pos(t1, Value);
  p2 := Pos(t2, Value);
  p3 := Pos(t3, Value);
  p4 := Pos(t4, Value);
  if p1 > 0 then
    Terminator := t1;
  Result := p1;
  if (p2 > 0) then
    if (Result = 0) or (p2 < Result) then
    begin
      Result := p2;
      Terminator := t2;
    end;
  if (p3 > 0) then
    if (Result = 0) or (p3 < Result) then
    begin
      Result := p3;
      Terminator := t3;
    end;
  if (p4 > 0) then
    if (Result = 0) or (p4 < Result) then
    begin
      Result := p4;
      Terminator := t4;
    end;
end;

{==============================================================================}

Procedure StringsTrim(const Value: TStrings);
var
  n: integer;
begin
  for n := Value.Count - 1 downto 0 do
    if Value[n] = '' then
      Value.Delete(n)
    else
      Break;
end;

{==============================================================================}

function PosFrom(const SubStr, Value: String; From: integer): integer;
var
  ls,lv: integer;
begin
  Result := 0;
  ls := Length(SubStr);
  lv := Length(Value);
  if (ls = 0) or (lv = 0) then
    Exit;
  if From < 1 then
    From := 1;
  while (ls + from - 1) <= (lv) do
  begin
    if CompareMem(@SubStr[1],@Value[from],ls) then
    begin
      result := from;
      break;
    end
    else
      inc(from);
  end;
end;

{==============================================================================}

function IncPoint(const p: pointer; Value: integer): pointer;
begin
  Result := pointer(integer(p) + Value);
end;

{==============================================================================}

function GetBetween(const PairBegin, PairEnd, Value: string): string;
var
  n: integer;
  x: integer;
  s: string;
begin
  Result := '';
  s := SeparateRight(Value, PairBegin);
  x := 1;
  for n := 1 to Length(s) do
  begin
    if s[n] = PairBegin then
      Inc(x);
    if s[n] = PairEnd then
    begin
      Dec(x);
      if x <= 0 then
        Break;
    end;
    Result := Result + s[n];
  end;
end;

{==============================================================================}

function CountOfChar(const Value: string; Chr: char): integer;
var
  n: integer;
begin
  Result := 0;
  for n := 1 to Length(Value) do
    if Value[n] = chr then
      Inc(Result);
end;

{==============================================================================}

function UnquoteStr(const Value: string; Quote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(Value);
  Result := AnsiExtractQuotedStr(LText, Quote);
  if Result = '' then
    Result := Value;
end;

{==============================================================================}

end.
