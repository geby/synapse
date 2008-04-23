{==============================================================================|
| Project : Delphree - Synapse                                   | 001.001.000 |
|==============================================================================|
| Content: HTTP client                                                        |
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

unit HTTPsend;

interface
uses
  Blcksock, sysutils, classes, windows, SynaUtil;

const
  CRLF=#13+#10;

type
  THTTPSend = class
  private
    Sock:TTCPBlockSocket;
  public
    timeout:integer;
    HTTPHost:string;
    HTTPPort:integer;
    Constructor Create;
    Destructor Destroy; override;
    function Request(Query,Response:TStrings):Boolean;
  end;

function get(Host,URI:string;Response:TStrings):Boolean;

implementation

{THTTPSend.Create}
Constructor THTTPSend.Create;
begin
  inherited Create;
  sock:=TTCPBlockSocket.create;
  sock.CreateSocket;
  timeout:=300000;
  HTTPhost:='localhost';
  HTTPPort:=80;
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
  sock.Connect(HTTPHost,IntToStr(HTTPPort));
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

{==============================================================================}

{get}
function get(Host,URI:string;Response:TStrings):Boolean;
var
  HTTP:THTTPSend;
  Query:TStringList;
begin
  Result:=False;
  HTTP:=THTTPSend.Create;
  Query:=TStringList.create;
  try
    HTTP.HTTPhost:=Host;
    Query.Add('GET '+URI+' HTTP/0.9');
    if not HTTP.Request(Query,Response) then Exit;
  finally
    Query.Free;
    HTTP.Free;
  end;
  Result:=True;
end;

end.
