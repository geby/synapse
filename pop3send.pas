{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
|==============================================================================|
| Content: POP3 client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2001.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

unit POP3send;

interface
uses
  Blcksock, sysutils, classes, windows, SynaUtil, SynaCode;

const
  CRLF=#13+#10;

type
  TPOP3Send = class
  private
    Sock:TTCPBlockSocket;
    function ReadResult(full:boolean):integer;
  public
    timeout:integer;
    POP3Host:string;
    POP3Port:string;
    ResultCode:integer;
    ResultString:string;
    FullResult:TStringList;
    Username:string;
    Password:string;
    StatCount:integer;
    StatSize:integer;
    TimeStamp:string;
    Constructor Create;
    Destructor Destroy; override;
    function AuthLogin:Boolean;
    function AuthApop:Boolean;
    function Connect:Boolean;
    function login:Boolean;
    procedure logout;
    function reset:Boolean;
    function noop:Boolean;
    function stat:Boolean;
    function list(value:integer):Boolean;
    function retr(value:integer):Boolean;
    function dele(value:integer):Boolean;
    function top(value,maxlines:integer):Boolean;
    function uidl(value:integer):Boolean;
  end;

implementation

{TPOP3Send.Create}
Constructor TPOP3Send.Create;
begin
  inherited Create;
  FullResult:=TStringList.create;
  sock:=TTCPBlockSocket.create;
  sock.CreateSocket;
  timeout:=300000;
  POP3host:='localhost';
  POP3Port:='pop3';
  Username:='';
  Password:='';
  StatCount:=0;
  StatSize:=0;
end;

{TPOP3Send.Destroy}
Destructor TPOP3Send.Destroy;
begin
  Sock.free;
  FullResult.free;
  inherited destroy;
end;

{TPOP3Send.ReadResult}
function TPOP3Send.ReadResult(full:boolean):integer;
var
  s:string;
begin
  Result:=0;
  FullResult.Clear;
  s:=sock.recvstring(timeout);
  if pos('+OK',s)=1
    then result:=1;
  ResultString:=s;
  if full and (result=1)then
    repeat
      s:=sock.recvstring(timeout);
      if s='.'
        then break;
      FullResult.add(s);
    until sock.LastError<>0;
  ResultCode:=Result;
end;

{TPOP3Send.AuthLogin}
function TPOP3Send.AuthLogin:Boolean;
begin
  Result:=false;
  Sock.SendString('USER '+username+CRLF);
  if readresult(false)<>1 then Exit;
  Sock.SendString('PASS '+password+CRLF);
  if readresult(false)<>1 then Exit;
  Result:=True;
end;

{TPOP3Send.AuthAPop}
function TPOP3Send.AuthAPOP:Boolean;
var
  s:string;
begin
  Result:=false;
  s:=StrToHex(MD5(TimeStamp+PassWord));
  Sock.SendString('APOP '+username+' '+s+CRLF);
  if readresult(false)<>1 then Exit;
  Result:=True;
end;


{TPOP3Send.Connect}
function TPOP3Send.Connect:Boolean;
begin
  Result:=false;
  StatCount:=0;
  StatSize:=0;
  sock.CloseSocket;
  sock.CreateSocket;
  sock.Connect(POP3Host,POP3Port);
  if sock.lasterror<>0 then Exit;
  Result:=True;
end;

{TPOP3Send.login}
function TPOP3Send.login:Boolean;
var
  s,s1:string;
begin
  Result:=False;
  TimeStamp:='';
  if not Connect then Exit;
  if readresult(false)<>1 then Exit;
  s:=separateright(Resultstring,'<');
  if s<>Resultstring then
    begin
      s1:=separateleft(s,'>');
      if s1<>s
        then TimeStamp:='<'+s1+'>';
    end;
  result:=false;
  if TimeStamp<>''
    then result:=AuthApop;
  if not result
    then result:=AuthLogin;
end;

{TPOP3Send.logout}
procedure TPOP3Send.logout;
begin
  Sock.SendString('QUIT'+CRLF);
  readresult(false);
  Sock.CloseSocket;
end;

{TPOP3Send.reset}
function TPOP3Send.reset:Boolean;
begin
  Result:=false;
  Sock.SendString('RSET'+CRLF);
  if readresult(false)<>1 then Exit;
  Result:=True;
end;

{TPOP3Send.noop}
function TPOP3Send.noop:Boolean;
begin
  Result:=false;
  Sock.SendString('NOOP'+CRLF);
  if readresult(false)<>1 then Exit;
  Result:=True;
end;

{TPOP3Send.stat}
function TPOP3Send.stat:Boolean;
var
  s:string;
begin
  Result:=false;
  Sock.SendString('STAT'+CRLF);
  if readresult(false)<>1 then Exit;
  s:=separateright(ResultString,'+OK ');
  StatCount:=StrToIntDef(separateleft(s,' '),0);
  StatSize:=StrToIntDef(separateright(s,' '),0);
  Result:=True;
end;

{TPOP3Send.list}
function TPOP3Send.list(value:integer):Boolean;
begin
  Result:=false;
  if value=0
    then Sock.SendString('LIST'+CRLF)
    else Sock.SendString('LIST '+IntToStr(value)+CRLF);
  if readresult(true)<>1 then Exit;
  Result:=True;
end;

{TPOP3Send.retr}
function TPOP3Send.retr(value:integer):Boolean;
begin
  Result:=false;
  Sock.SendString('RETR '+IntToStr(value)+CRLF);
  if readresult(true)<>1 then Exit;
  Result:=True;
end;

{TPOP3Send.dele}
function TPOP3Send.dele(value:integer):Boolean;
begin
  Result:=false;
  Sock.SendString('DELE '+IntToStr(value)+CRLF);
  if readresult(false)<>1 then Exit;
  Result:=True;
end;

{TPOP3Send.top}
function TPOP3Send.top(value,maxlines:integer):Boolean;
begin
  Result:=false;
  Sock.SendString('TOP '+IntToStr(value)+IntToStr(maxlines)+CRLF);
  if readresult(true)<>1 then Exit;
  Result:=True;
end;

{TPOP3Send.uidl}
function TPOP3Send.uidl(value:integer):Boolean;
begin
  Result:=false;
  if value=0
    then Sock.SendString('UIDL'+CRLF)
    else Sock.SendString('UIDL '+IntToStr(value)+CRLF);
  if readresult(true)<>1 then Exit;
  Result:=True;
end;


{==============================================================================}

end.
