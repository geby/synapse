{==============================================================================|
| Project : Delphree - Synapse                                   | 001.006.000 |
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

unit SynaUtil;

interface

uses
  sysutils, classes, windows;

function timezone:string;
function Rfc822DateTime(t:TDateTime):String;
function CodeInt(Value:word):string;
function DeCodeInt(Value:string;Index:integer):word;
function IsIP(Value:string):Boolean;
function ReverseIP(Value:string):string;
procedure Dump (Buffer:string;DumpFile:string);
function SeparateLeft(value,delimiter:string):string;
function SeparateRight(value,delimiter:string):string;
function getparameter(value,parameter:string):string;
function GetEmailAddr(value:string):string;
function GetEmailDesc(value:string):string;
function StrToHex(value:string):string;
function IntToBin(value:integer;digits:byte):string;
function BinToInt(value:string):integer;

implementation

{==============================================================================}
{timezone}
function timezone:string;
var
  zoneinfo:TTimeZoneInformation;
  bias:integer;
  h,m:integer;
begin
  case GetTimeZoneInformation(Zoneinfo) of
    2:  bias:=zoneinfo.bias+zoneinfo.DaylightBias;
    1:  bias:=zoneinfo.bias+zoneinfo.StandardBias;
    else
      bias:=zoneinfo.bias;
  end;
  if bias<=0 then result:='+'
    else result:='-';
  bias:=abs(bias);
  h:=bias div 60;
  m:=bias mod 60;
  result:=result+format('%.2d%.2d',[h,m]);
end;

{==============================================================================}

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

{==============================================================================}

{CodeInt}
function CodeInt(Value:word):string;
begin
  Result := Chr(Hi(Value))+ Chr(Lo(Value))
end;

{==============================================================================}

{DeCodeInt}
function DeCodeInt(Value:string;Index:integer):word;
var
  x,y:Byte;
begin
  if Length(Value)>index then x:=Ord(Value[index])
    else x:=0;
  if Length(Value)>(Index+1) then y:=Ord(Value[Index+1])
    else y:=0;
  Result:=x*256+y;
end;

{==============================================================================}

{IsIP}
function IsIP(Value:string):Boolean;
var
  n,x:integer;
begin
  Result:=true;
  x:=0;
  for n:=1 to Length(Value) do
    if not (Value[n] in ['0'..'9','.'])
      then begin
        Result:=False;
        break;
      end
      else begin
        if Value[n]='.' then Inc(x);
      end;
  if x<>3 then Result:=False;
end;

{==============================================================================}

{ReverseIP}
function ReverseIP(Value:string):string;
var
  x:integer;
begin
  Result:='';
  repeat
    x:=LastDelimiter('.',Value);
    Result:=Result+'.'+Copy(Value,x+1,Length(Value)-x);
    Delete(Value,x,Length(Value)-x+1);
  until x<1;
  if Length(Result)>0 then
    if Result[1]='.' then
      Delete(Result, 1, 1);
end;

{==============================================================================}

{dump}
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

{==============================================================================}
{SeparateLeft}
function SeparateLeft(value,delimiter:string):string;
var
  x:integer;
begin
  x:=pos(delimiter,value);
  if x<1
    then result:=trim(value)
    else result:=trim(copy(value,1,x-1));
end;

{==============================================================================}
{SeparateRight}
function SeparateRight(value,delimiter:string):string;
var
  x:integer;
begin
  x:=pos(delimiter,value);
  if x>0
    then x:=x+length(delimiter)-1;
  result:=trim(copy(value,x+1,length(value)-x));
end;

{==============================================================================}
{GetParameter}
function getparameter(value,parameter:string):string;
var
  x,x1:integer;
  s:string;
begin
  x:=pos(uppercase(parameter),uppercase(value));
  result:='';
  if x>0 then
    begin
      s:=copy(value,x+length(parameter),length(value)-(x+length(parameter))+1);
      s:=trim(s);
      x1:=length(s);
      if length(s)>1 then
        begin
          if s[1]='"'
            then
              begin
                s:=copy(s,2,length(s)-1);
                x:=pos('"',s);
                if x>0 then x1:=x-1;
              end
            else
              begin
                x:=pos(' ',s);
                if x>0 then x1:=x-1;
              end;
        end;
      result:=copy(s,1,x1);
    end;
end;

{==============================================================================}
{GetEmailAddr}
function GetEmailAddr(value:string):string;
var
  s:string;
begin
  s:=separateright(value,'<');
  s:=separateleft(s,'>');
  result:=trim(s);
end;

{==============================================================================}
{GetEmailDesc}
function GetEmailDesc(value:string):string;
var
  s:string;
begin
  value:=trim(value);
  s:=separateright(value,'"');
  if s<>value
    then s:=separateleft(s,'"')
    else
      begin
        s:=separateright(value,'(');
        if s<>value
          then s:=separateleft(s,')')
          else
            begin
              s:=separateleft(value,'<');
              if s=value
                then s:='';
            end;
      end;
  result:=trim(s);
end;

{==============================================================================}
{StrToHex}
function StrToHex(value:string):string;
var
  n:integer;
begin
  result:='';
  for n:=1 to length(value) do
    Result:=Result+IntToHex(Byte(value[n]),2);
  result:=lowercase(result);
end;

{==============================================================================}
{IntToBin}
function IntToBin(value:integer;digits:byte):string;
var
  x,y,n:integer;
begin
  result:='';
  x:=value;
  repeat
    y:=x mod 2;
    x:=x div 2;
    if y>0
      then result:='1'+result
      else result:='0'+result;
  until x=0;
  x:=length(result);
  for n:=x to digits-1 do
    result:='0'+result;
end;

{==============================================================================}
{BinToInt}
function BinToInt(value:string):integer;
var
  x,n:integer;
begin
  result:=0;
  for n:=1 to length(value) do
    begin
      if value[n]='0'
        then x:=0
        else x:=1;
      result:=result*2+x;
    end;
end;

{==============================================================================}

end.
