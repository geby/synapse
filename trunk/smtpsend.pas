{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.001|
|==============================================================================|
| Content: SMTP client                                                        |
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

unit SMTPsend;

interface
uses
  Blcksock, sysutils, classes, windows, SynaUtil;

const
  CRLF=#13+#10;

type
  TSMTPSend = class
  private
    Sock:TTCPBlockSocket;
    function ReadResult:integer;
  public
    timeout:integer;
    SMTPHost:string;
    Constructor Create;
    Destructor Destroy; override;
    function login:Boolean;
    procedure logout;
    function reset:Boolean;
    function noop:Boolean;
    function mailfrom(Value:string):Boolean;
    function mailto(Value:string):Boolean;
    function maildata(Value:Tstrings):Boolean;
  end;

function Sendto (mailfrom,mailto,subject,SMTPHost:string;maildata:TStrings):Boolean;

implementation

{TSMTPSend.Create}
Constructor TSMTPSend.Create;
begin
  inherited Create;
  sock:=TTCPBlockSocket.create;
  sock.CreateSocket;
  timeout:=300;
  SMTPhost:='localhost';
end;

{TSMTPSend.Destroy}
Destructor TSMTPSend.Destroy;
begin
  Sock.free;
  inherited destroy;
end;

{TSMTPSend.ReadResult}
function TSMTPSend.ReadResult:integer;
var
  s:string;
begin
  Result:=0;
  s:=sock.recvstring(timeout);
  if Length(s)>=3 then Result:=Strtointdef(Copy(s,1,3),0);
  while pos('-',s)=4 do
    begin
      s:=sock.recvstring(timeout);
      if sock.LastError<>0 then
        begin
          Result:=0;
          break;
        end;
    end;
end;

{TSMTPSend.login}
function TSMTPSend.login:Boolean;
begin
  Result:=False;
  sock.Connect(SMTPHost,'smtp');
  if sock.lasterror<>0 then Exit;
  if readresult<>220 then Exit;
  Sock.SendString('HELO '+sock.LocalName+CRLF);
  if readresult<>250 then Exit;
  Result:=True;
end;

{TSMTPSend.logout}
procedure TSMTPSend.logout;
begin
  Sock.SendString('QUIT'+CRLF);
  readresult;
  Sock.CloseSocket;
end;

{TSMTPSend.reset}
function TSMTPSend.reset:Boolean;
begin
  Result:=false;
  Sock.SendString('RSET'+CRLF);
  if readresult<>250 then Exit;
  Result:=True;
end;

{TSMTPSend.noop}
function TSMTPSend.noop:Boolean;
begin
  Result:=false;
  Sock.SendString('NOOP'+CRLF);
  if readresult<>250 then Exit;
  Result:=True;
end;


{TSMTPSend.mailfrom}
function TSMTPSend.mailfrom(Value:string):Boolean;
begin
  Result:=false;
  Sock.SendString('MAIL FROM:<'+Value+'>'+CRLF);
  if readresult<>250 then Exit;
  Result:=True;
end;

{TSMTPSend.mailto}
function TSMTPSend.mailto(Value:string):Boolean;
begin
  Result:=false;
  Sock.SendString('RCPT TO:<'+Value+'>'+CRLF);
  if readresult<>250 then Exit;
  Result:=True;
end;

{TSMTPSend.maildata}
function TSMTPSend.maildata(Value:Tstrings):Boolean;
var
  n:integer;
  s:string;
begin
  Result:=false;
  Sock.SendString('DATA'+CRLF);
  if readresult<>354 then Exit;
  for n:=0 to Value.Count-1 do
    begin
      s:=value[n];
      if Length(s)>1 then
        if s[1]='.' then s:='.'+s;
      Sock.SendString(s+CRLF);
    end;
  Sock.SendString('.'+CRLF);
  if readresult<>250 then Exit;
  Result:=True;
end;

{==============================================================================}


function Sendto (mailfrom,mailto,subject,SMTPHost:string;maildata:TStrings):Boolean;
var
  SMTP:TSMTPSend;
  t:TStrings;
begin
  Result:=False;
  SMTP:=TSMTPSend.Create;
  t:=TStringList.Create;
  try
    t.assign(Maildata);
    t.Insert(0,'');
    t.Insert(0,'subject: '+subject);
    t.Insert(0,'date: '+Rfc822DateTime(now));
    t.Insert(0,'to: '+mailto);
    t.Insert(0,'from: '+mailfrom);
    SMTP.SMTPHost:=SMTPHost;
    if not SMTP.login then Exit;
    if not SMTP.mailfrom(mailfrom) then Exit;
    if not SMTP.mailto(mailto) then Exit;
    if not SMTP.maildata(t) then Exit;
    SMTP.logout;
    Result:=True;
  finally
    SMTP.Free;
    t.Free;
  end;
end;


end.
