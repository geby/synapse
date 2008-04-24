{==============================================================================|
| Project : Delphree - Synapse                                   | 002.000.000 |
|==============================================================================|
| Content: SMTP client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c) 1999,2000,2001.          |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

unit SMTPsend;

interface
uses
  Blcksock, sysutils, classes, windows, SynaUtil, SynaCode;

const
  CRLF=#13+#10;

type
  TSMTPSend = class
  private
    Sock:TTCPBlockSocket;
    procedure EnhancedCode(value:string);
    function ReadResult:integer;
  public
    timeout:integer;
    SMTPHost:string;
    ResultCode:integer;
    ResultString:string;
    FullResult:TStringList;
    ESMTPcap:TStringList;
    ESMTP:boolean;
    Username:string;
    Password:string;
    AuthDone:boolean;
    ESMTPSize:boolean;
    MaxSize:integer;
    EnhCode1:integer;
    EnhCode2:integer;
    EnhCode3:integer;
    Constructor Create;
    Destructor Destroy; override;
    function AuthLogin:Boolean;
    function AuthCram:Boolean;
    function login:Boolean;
    procedure logout;
    function reset:Boolean;
    function noop:Boolean;
    function mailfrom(Value:string; size:integer):Boolean;
    function mailto(Value:string):Boolean;
    function maildata(Value:Tstrings):Boolean;
    function etrn(Value:string):Boolean;
    function verify(Value:string):Boolean;
    function EnhCodeString:string;
  end;

function SendtoRaw (mailfrom,mailto,SMTPHost:string;maildata:TStrings;Username,Password:string):Boolean;
function Sendto (mailfrom,mailto,subject,SMTPHost:string;maildata:TStrings):Boolean;
function SendtoEx (mailfrom,mailto,subject,SMTPHost:string;maildata:TStrings;Username,Password:string):Boolean;

implementation

{TSMTPSend.Create}
Constructor TSMTPSend.Create;
begin
  inherited Create;
  FullResult:=TStringList.create;
  ESMTPcap:=TStringList.create;
  sock:=TTCPBlockSocket.create;
  sock.CreateSocket;
  timeout:=300000;
  SMTPhost:='localhost';
  Username:='';
  Password:='';
end;

{TSMTPSend.Destroy}
Destructor TSMTPSend.Destroy;
begin
  Sock.free;
  ESMTPcap.free;
  FullResult.free;
  inherited destroy;
end;

{TSMTPSend.EnhancedCode}
procedure TSMTPSend.EnhancedCode (value:string);
var
  s,t:string;
  e1,e2,e3:integer;
begin
  EnhCode1:=0;
  EnhCode2:=0;
  EnhCode3:=0;
  s:=copy(value,5,length(value)-4);
  t:=separateleft(s,'.');
  s:=separateright(s,'.');
  if t='' then exit;
  if length(t)>1 then exit;
  e1:=strtointdef(t,0);
  if e1=0 then exit;
  t:=separateleft(s,'.');
  s:=separateright(s,'.');
  if t='' then exit;
  if length(t)>3 then exit;
  e2:=strtointdef(t,0);
  t:=separateleft(s,' ');
  if t='' then exit;
  if length(t)>3 then exit;
  e3:=strtointdef(t,0);
  EnhCode1:=e1;
  EnhCode2:=e2;
  EnhCode3:=e3;
end;

{TSMTPSend.ReadResult}
function TSMTPSend.ReadResult:integer;
var
  s:string;
begin
  Result:=0;
  FullResult.Clear;
  repeat
    s:=sock.recvstring(timeout);
    ResultString:=s;
    FullResult.add(s);
    if sock.LastError<>0 then
      break;
  until pos('-',s)<>4;
  s:=FullResult[0];
  if Length(s)>=3 then Result:=Strtointdef(Copy(s,1,3),0);
  ResultCode:=Result;
  EnhancedCode(s);
end;

{TSMTPSend.AuthLogin}
function TSMTPSend.AuthLogin:Boolean;
begin
  Result:=false;
  Sock.SendString('AUTH LOGIN'+CRLF);
  if readresult<>334 then Exit;
  Sock.SendString(Encodebase64(username)+CRLF);
  if readresult<>334 then Exit;
  Sock.SendString(Encodebase64(password)+CRLF);
  if readresult<>235 then Exit;
  Result:=True;
end;

{TSMTPSend.AuthCram}
function TSMTPSend.AuthCram:Boolean;
var
  s,sm:string;
  ipad,opad:string;
  n,x:integer;
begin
  Result:=false;
  Sock.SendString('AUTH CRAM-MD5'+CRLF);
  if readresult<>334 then Exit;
  s:=copy(ResultString,5,length(ResultString)-4);
  s:=DecodeBase64(s);
  s:=HMAC_MD5(s,password);
  s:=Username+' '+strtohex(s);
  Sock.SendString(Encodebase64(s)+CRLF);
  if readresult<>235 then Exit;
  Result:=True;
end;

{TSMTPSend.login}
function TSMTPSend.login:Boolean;
var
  n:integer;
  auths:string;
begin
  Result:=False;
  ESMTP:=true;
  AuthDone:=false;
  ESMTPcap.clear;
  ESMTPSize:=false;
  MaxSize:=0;
  sock.Connect(SMTPHost,'smtp');
  if sock.lasterror<>0 then Exit;
  if readresult<>220 then Exit;
  Sock.SendString('EHLO '+sock.LocalName+CRLF);
  if readresult<>250 then
    begin
      ESMTP:=false;
      Sock.SendString('HELO '+sock.LocalName+CRLF);
      if readresult<>250 then Exit;
    end;
  Result:=True;
  if ESMTP then
    begin
      for n:=1 to FullResult.count-1 do
        ESMTPcap.add(Copy(FullResult[n],5,length(Fullresult[n])-4));
      if not ((Username='') and (Password='')) then
        for n:=0 to ESMTPcap.count-1 do
          begin
            auths:=uppercase(ESMTPcap[n]);
            if pos('AUTH ',auths)=1 then
              begin
                if pos('CRAM-MD5',auths)>0 then
                  begin
                    AuthDone:=AuthCram;
                    break;
                  end;
                if pos('LOGIN',auths)>0 then
                  begin
                    AuthDone:=AuthLogin;
                    break;
                  end;
              end;
          end;
      for n:=0 to ESMTPcap.count-1 do
        if pos('SIZE',uppercase(ESMTPcap[n]))=1 then
          begin
            ESMTPsize:=true;
            MaxSize:=StrToIntDef(copy(ESMTPcap[n],6,length(ESMTPcap[n])-5),0);
            break;
          end;
    end;
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
function TSMTPSend.mailfrom(Value:string; size:integer):Boolean;
var
  s:string;
begin
  Result:=false;
  s:='MAIL FROM:<'+Value+'>';
  if ESMTPsize and (size>0)
    then s:=s+' SIZE='+IntToStr(size);
  Sock.SendString(s+CRLF);
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
      if Length(s)>=1 then
        if s[1]='.' then s:='.'+s;
      Sock.SendString(s+CRLF);
    end;
  Sock.SendString('.'+CRLF);
  if readresult<>250 then Exit;
  Result:=True;
end;

{TSMTPSend.etrn}
function TSMTPSend.etrn(Value:string):Boolean;
begin
  Result:=false;
  Sock.SendString('ETRN '+Value+CRLF);
  if (readresult<250) or (readresult>259) then Exit;
  Result:=True;
end;

{TSMTPSend.verify}
function TSMTPSend.verify(Value:string):Boolean;
begin
  Result:=false;
  Sock.SendString('VRFY '+Value+CRLF);
  if (readresult<250) or (readresult>259) then Exit;
  Result:=True;
end;

{TSMTPSend.EnhCodeString}
function TSMTPSend.EnhCodeString:string;
var
  s,t:string;
begin
  s:=inttostr(EnhCode2)+'.'+inttostr(EnhCode3);
  t:='';
  if s='0.0' then t:='Other undefined Status';
  if s='1.0' then t:='Other address status';
  if s='1.1' then t:='Bad destination mailbox address';
  if s='1.2' then t:='Bad destination system address';
  if s='1.3' then t:='Bad destination mailbox address syntax';
  if s='1.4' then t:='Destination mailbox address ambiguous';
  if s='1.5' then t:='Destination mailbox address valid';
  if s='1.6' then t:='Mailbox has moved';
  if s='1.7' then t:='Bad sender''s mailbox address syntax';
  if s='1.8' then t:='Bad sender''s system address';
  if s='2.0' then t:='Other or undefined mailbox status';
  if s='2.1' then t:='Mailbox disabled, not accepting messages';
  if s='2.2' then t:='Mailbox full';
  if s='2.3' then t:='Message length exceeds administrative limit';
  if s='2.4' then t:='Mailing list expansion problem';
  if s='3.0' then t:='Other or undefined mail system status';
  if s='3.1' then t:='Mail system full';
  if s='3.2' then t:='System not accepting network messages';
  if s='3.3' then t:='System not capable of selected features';
  if s='3.4' then t:='Message too big for system';
  if s='3.5' then t:='System incorrectly configured';
  if s='4.0' then t:='Other or undefined network or routing status';
  if s='4.1' then t:='No answer from host';
  if s='4.2' then t:='Bad connection';
  if s='4.3' then t:='Routing server failure';
  if s='4.4' then t:='Unable to route';
  if s='4.5' then t:='Network congestion';
  if s='4.6' then t:='Routing loop detected';
  if s='4.7' then t:='Delivery time expired';
  if s='5.0' then t:='Other or undefined protocol status';
  if s='5.1' then t:='Invalid command';
  if s='5.2' then t:='Syntax error';
  if s='5.3' then t:='Too many recipients';
  if s='5.4' then t:='Invalid command arguments';
  if s='5.5' then t:='Wrong protocol version';
  if s='6.0' then t:='Other or undefined media error';
  if s='6.1' then t:='Media not supported';
  if s='6.2' then t:='Conversion required and prohibited';
  if s='6.3' then t:='Conversion required but not supported';
  if s='6.4' then t:='Conversion with loss performed';
  if s='6.5' then t:='Conversion failed';
  if s='7.0' then t:='Other or undefined security status';
  if s='7.1' then t:='Delivery not authorized, message refused';
  if s='7.2' then t:='Mailing list expansion prohibited';
  if s='7.3' then t:='Security conversion required but not possible';
  if s='7.4' then t:='Security features not supported';
  if s='7.5' then t:='Cryptographic failure';
  if s='7.6' then t:='Cryptographic algorithm not supported';
  if s='7.7' then t:='Message integrity failure';
  s:='???-';
  if EnhCode1=2 then s:='Success-';
  if EnhCode1=4 then s:='Persistent Transient Failure-';
  if EnhCode1=5 then s:='Permanent Failure-';
  result:=s+t;
end;


{==============================================================================}

function SendtoRaw (mailfrom,mailto,SMTPHost:string;maildata:TStrings;
  Username,Password:string):Boolean;
var
  SMTP:TSMTPSend;
  size:integer;
begin
  Result:=False;
  SMTP:=TSMTPSend.Create;
  try
    SMTP.SMTPHost:=SMTPHost;
    SMTP.Username:=Username;
    SMTP.Password:=Password;
    if not SMTP.login then Exit;
    size:=length(maildata.text);
    if not SMTP.mailfrom(mailfrom,size) then Exit;
    if not SMTP.mailto(mailto) then Exit;
    if not SMTP.maildata(Maildata) then Exit;
    SMTP.logout;
    Result:=True;
  finally
    SMTP.Free;
  end;
end;

function SendtoEx (mailfrom,mailto,subject,SMTPHost:string;maildata:TStrings;
  Username,Password:string):Boolean;
var
  t:TStrings;
begin
  Result:=False;
  t:=TStringList.Create;
  try
    t.assign(Maildata);
    t.Insert(0,'');
    t.Insert(0,'x-mailer: Synapse - Delphi TCP/IP library by Lukas Gebauer');
    t.Insert(0,'subject: '+subject);
    t.Insert(0,'date: '+Rfc822DateTime(now));
    t.Insert(0,'to: '+mailto);
    t.Insert(0,'from: '+mailfrom);
    result:=SendToRaw(mailfrom,mailto,SMTPHost,t,Username,Password);
  finally
    t.Free;
  end;
end;

function Sendto (mailfrom,mailto,subject,SMTPHost:string;maildata:TStrings):Boolean;
begin
  result:=SendToEx(mailfrom,mailto,subject,SMTPHost,maildata,'','');
end;

end.
