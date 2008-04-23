{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
|==============================================================================|
| Content: support procedures and functions                                    |
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
| Portions created by Lukas Gebauer are Copyright (c) 1999.                    |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.mlp.cz/space/gebauerl/synapse/)           |
|==============================================================================}

unit SynaUtil;

interface

uses
  Blcksock, sysutils, classes, windows;

function timezone:string;
function Rfc822DateTime(t:TDateTime):String;
function CodeInt(Value:integer):string;
function IsIP(Value:string):Boolean;
function ReverseIP(Value:string):string;
procedure Dump (Buffer:string;DumpFile:string);

implementation

{timezone}
function timezone:string;
var
  zoneinfo:TTimeZoneInformation;
  bias:integer;
  h,m:integer;
begin
  GetTimeZoneInformation(Zoneinfo);
  bias:=zoneinfo.bias;
  if bias<=0 then result:='+'
    else result:='-';
  bias:=abs(bias);
  h:=bias div 60;
  m:=bias mod 60;
  result:=result+format('%.2d%.2d',[h,m]);
end;

{Rfc822DateTime}
function Rfc822DateTime(t:TDateTime):String;
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
  if ShortDayNames[1] = MyDayNames[1]
    then Result := FormatDateTime('ddd, d mmm yyyy hh:mm:ss', t)
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
  Result:=Result+' '+Timezone;
end;

{CodeInt}
function CodeInt(Value:integer):string;
var
  x,y:Byte;
begin
  x:=Value div 256;
  y:=Value mod 256;
  Result:=char(y)+char(x);
end;

{IsIP}
function IsIP(Value:string):Boolean;
var
  n,x:integer;
begin
  Result:=true;
  x:=0;
  for n:=1 to Length(Value) do
    if not (Value[n] in ['1'..'0','.'])
      then begin
        Result:=False;
        break;
      end
      else begin
        if Value[n]='.' then Inc(x);
      end;
  if x<>3 then Result:=False;
end;

{ReverseIP}
function ReverseIP(Value:string):string;
var
  x:integer;
begin
  Result:='';
  repeat
    x:=LastDelimiter('.',Value);
    Result:=Result+Copy(Value,x,Length(Value)-x);
    Delete(Value,x,Length(Value)-x);
  until x<1;
  if Length(Result)>0 then
    if Value[1]='.' then Delete(Result, 1, 1);
end;

procedure dump (Buffer:string;DumpFile:string);
var
  n:integer;
  s:string;
  f:Text;
begin
  s:='';
  for n:=1 to Length(Buffer) do
    s:=s+' +#$'+IntToHex(Ord(Buffer[n]),2);
  Assignfile(f,DumpFile);
  if fileexists(DumpFile) then deletefile(PChar(DumpFile));
  rewrite(f);
  try
    writeln(f,s);
  finally
    closefile(f);
  end;
end;


end.
