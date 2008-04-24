{==============================================================================|
| Project : Delphree - Synapse                                   | 001.002.000 |
|==============================================================================|
| Content: HTTP client                                                         |
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

unit HTTPsend;

interface
uses
  Blcksock, sysutils, classes, windows, SynaUtil, SynaCode;

const
  CRLF=#13+#10;

type
  THTTPSend = class
  private
    Sock:TTCPBlockSocket;
  public
    timeout:integer;
    HTTPHost:string;
    HTTPPort:string;
    ProxyHost:string;
    ProxyPort:string;
    ProxyUser:string;
    ProxyPass:string;
    ResultCode:integer;
    Constructor Create;
    Destructor Destroy; override;
    function Request(Query,Response:TStrings):Boolean;
    function DoMethod(method,URL:string;Content,Response:TStrings):boolean;
  end;

function SimpleGet(URL:string;Response:TStrings):Boolean;
function Get(URL:string;Response:TStrings):Boolean;
function Post(URL:string;Value,Response:TStrings):Boolean;

implementation

{THTTPSend.Create}
Constructor THTTPSend.Create;
begin
  inherited Create;
  sock:=TTCPBlockSocket.create;
  sock.CreateSocket;
  timeout:=300000;
  HTTPhost:='localhost';
  HTTPPort:='80';
  ProxyHost:='';
  ProxyPort:='8080';
  ProxyUser:='';
  ProxyPass:='';
end;

{THTTPSend.Destroy}
Destructor THTTPSend.Destroy;
begin
  Sock.free;
  inherited destroy;
end;

{THTTPSend.Request}
function THTTPSend.Request(Query,Response:TStrings):Boolean;
var
  s:string;
  n:integer;
begin
  Result:=False;
  sock.Connect(HTTPHost,HTTPPort);
  if sock.lasterror<>0 then Exit;
  for n:=0 to Query.Count-1 do
    Sock.SendString(Query[n]+CRLF);
  if Query[query.Count-1]<>'' then
    Sock.SendString(CRLF);
  if sock.lasterror<>0 then Exit;
  repeat
    s:=sock.recvstring(timeout);
    Response.Add(s);
  until sock.lasterror<>0;
  Result:=True;
end;

{THTTPSend.DoMethod}
function THTTPSend.DoMethod(method,URL:string;Content,Response:TStrings):boolean;
var
  Prot,User,Pass,Host,Port,Path,Para:string;
  Query:TstringList;
  size:integer;
  s:string;
begin
  result:=false;
  Query:=TstringList.create;
  try
    parseURL(URL,Prot,User,Pass,Host,Port,Path,Para);
    if content<>nil
      then query.AddStrings(content);
    size:=length(query.text);
    query.insert(0,'');
    query.insert(0,'User-Agent: Synapse/1.1');
    query.insert(0,'Connection: close');
    query.insert(0,'Accept-Encoding: identity');
    if User<>''
      then query.insert(0,'Authorization: Basic '+EncodeBase64(user+':'+pass));
    if (proxyhost<>'') and (proxyUser<>'')
      then query.insert(0,'Proxy-Authorization: Basic '+EncodeBase64(Proxyuser+':'+Proxypass));
    if size>0
      then query.insert(0,'Content-Length: '+inttostr(size));
    query.insert(0,'Host: '+host+':'+port);

    if para=''
      then s:=''
      else s:='?'+para;
    s:=path+s;
    if proxyHost<>''
      then s:=prot+'://'+host+':'+port+s;
    query.insert(0,uppercase(method)+' '+s+' HTTP/1.0');
    if proxyhost=''
      then
        begin
          HttpHost:=host;
          HttpPort:=port;
        end
      else
        begin
          HttpHost:=Proxyhost;
          HttpPort:=Proxyport;
        end;
    result:=request(query,response);
    ResultCode:=0;
    if response.count>0
      then if pos('HTTP/',uppercase(response[0]))=1
        then
          begin
            s:=separateright(response[0],' ');
            s:=separateleft(s,' ');
            ResultCode:=StrToIntDef(s,0);
          end;
  finally
    Query.free;
  end;
end;

{==============================================================================}

{SimpleGet}
function SimpleGet(URL:string;Response:TStrings):Boolean;
var
  HTTP:THTTPSend;
  Query:TStringList;
  Prot,User,Pass,Host,Port,Path,Para:string;
begin
  parseURL(URL,Prot,User,Pass,Host,Port,Path,Para);
  if para<>''
    then path:=path+'?'+para;
  Result:=False;
  HTTP:=THTTPSend.Create;
  Query:=TStringList.create;
  try
    HTTP.HTTPhost:=Host;
    Query.Add('GET '+Path);
    if not HTTP.Request(Query,Response) then Exit;
  finally
    Query.Free;
    HTTP.Free;
  end;
  Result:=True;
end;

{get}
function Get(URL:string;Response:TStrings):Boolean;
var
  HTTP:THTTPSend;
  Prot,User,Pass,Host,Port,Path,Para:string;
begin
  Result:=False;
  HTTP:=THTTPSend.Create;
  try
    result:=HTTP.DoMethod('GET',URL,nil,Response);
  finally
    HTTP.Free;
  end;
end;

{post}
function Post(URL:string;Value,Response:TStrings):Boolean;
var
  HTTP:THTTPSend;
  Prot,User,Pass,Host,Port,Path,Para:string;
begin
  Result:=False;
  HTTP:=THTTPSend.Create;
  try
    result:=HTTP.DoMethod('POST',URL,Value,Response);
  finally
    HTTP.Free;
  end;
end;


end.
