{==============================================================================|
| Project : Delphree - Synapse                                   | 002.000.000 |
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
  TTransferEncoding=(TE_UNKNOWN,
                     TE_IDENTITY,
                     TE_CHUNKED);

  THTTPSend = class
  private
    Sock:TTCPBlockSocket;
    TransferEncoding:TTransferEncoding;
    AliveHost:string;
    AlivePort:string;
    function ReadUnknown:boolean;
    function ReadIdentity(size:integer):boolean;
    function ReadChunked:boolean;
  public
    headers:TStringlist;
    Document:TMemoryStream;
    MimeType:string;
    Protocol:string;
    KeepAlive:boolean;
    Timeout:integer;
    HTTPHost:string;
    HTTPPort:string;
    ProxyHost:string;
    ProxyPort:string;
    ProxyUser:string;
    ProxyPass:string;
    ResultCode:integer;
    ResultString:string;
    Constructor Create;
    Destructor Destroy; override;
    procedure clear;
    procedure DecodeStatus(value:string);
    function HTTPmethod(method,URL:string):boolean;
  end;

function HttpGetText(URL:string;Response:TStrings):Boolean;
function HttpGetBinary(URL:string;Response:TStream):Boolean;
function HttpPostBinary(URL:string;Data:TStream):Boolean;

implementation

{THTTPSend.Create}
Constructor THTTPSend.Create;
begin
  inherited Create;
  Headers:=TStringList.create;
  Document:=TMemoryStream.Create;
  sock:=TTCPBlockSocket.create;
  sock.SizeRecvBuffer:=65536;
  sock.SizeSendBuffer:=65536;
  timeout:=300000;
  HTTPhost:='localhost';
  HTTPPort:='80';
  ProxyHost:='';
  ProxyPort:='8080';
  ProxyUser:='';
  ProxyPass:='';
  AliveHost:='';
  AlivePort:='';
  Protocol:='1.1';
  KeepAlive:=true;
  Clear;
end;

{THTTPSend.Destroy}
Destructor THTTPSend.Destroy;
begin
  Sock.free;
  Document.free;
  headers.free;
  inherited destroy;
end;

{THTTPSend.Clear}
procedure THTTPSend.Clear;
begin
  Document.Clear;
  Headers.Clear;
  MimeType:='text/html';
end;

{THTTPSend.DecodeStatus}
procedure THTTPSend.DecodeStatus(value:string);
var
  s,su:string;
begin
  s:=separateright(value,' ');
  su:=separateleft(s,' ');
  ResultCode:=StrToIntDef(su,0);
  ResultString:=separateright(s,' ');
  if ResultString=s
    then ResultString:='';
end;

{THTTPSend.HTTPmethod}
function THTTPSend.HTTPmethod(method,URL:string):boolean;
var
  sending,receiving:boolean;
  status100:boolean;
  status100error:string;
  ToClose:boolean;
  size:integer;
  Prot,User,Pass,Host,Port,Path,Para,URI:string;
  n:integer;
  s,su:string;
begin
  {initial values}
  result:=false;
  ResultCode:=500;
  ResultString:='';

  URI:=ParseURL(URL,Prot,User,Pass,Host,Port,Path,Para);
  sending:=Document.Size>0;
  {headers for sending data}
  status100:=sending and (protocol='1.1');
  if status100
    then Headers.insert(0,'Expect: 100-continue');
  if sending then
    begin
      Headers.insert(0,'Content-Length: '+inttostr(Document.size));
      if MimeType<>''
        then Headers.insert(0,'Content-Type: '+MimeType);
    end;
  {seting KeepAlives}
  if not KeepAlive
    then Headers.insert(0,'Connection: close');
  {set target servers/proxy, authorisations, etc...}
  if User<>''
    then Headers.insert(0,'Authorization: Basic '+EncodeBase64(user+':'+pass));
  if (proxyhost<>'') and (proxyUser<>'')
    then Headers.insert(0,'Proxy-Authorization: Basic '+EncodeBase64(Proxyuser+':'+Proxypass));
  Headers.insert(0,'Host: '+host+':'+port);
  if proxyHost<>''
    then URI:=prot+'://'+host+':'+port+URI;
  if URI='/*'
    then URI:='*';
  if protocol='0.9'
    then Headers.insert(0,uppercase(method)+' '+URI)
    else Headers.insert(0,uppercase(method)+' '+URI+' HTTP/'+protocol);
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
  if headers[headers.count-1]<>''
    then headers.add('');

  {connect}
  if (Alivehost<>HTTPhost) or (AlivePort<>HTTPport)
    then
      begin
        sock.CloseSocket;
        sock.CreateSocket;
        sock.Connect(HTTPHost,HTTPPort);
        if sock.lasterror<>0 then Exit;
        Alivehost:=HTTPhost;
        AlivePort:=HTTPport;
      end
    else
      begin
        if sock.canread(0) then
          begin
            sock.CloseSocket;
            sock.createsocket;
            sock.Connect(HTTPHost,HTTPPort);
            if sock.lasterror<>0 then Exit;
          end;
      end;

  {send headers}
  Sock.SendString(Headers[0]+CRLF);
  if protocol<>'0.9' then
    for n:=1 to Headers.Count-1 do
      Sock.SendString(Headers[n]+CRLF);
  if sock.lasterror<>0 then Exit;

  {reading Status}
  Status100Error:='';
  if status100 then
    begin
      repeat
        s:=sock.recvstring(timeout);
        if s<>'' then break;
      until sock.lasterror<>0;
      DecodeStatus(s);
      if (ResultCode>=100) and (ResultCode<200)
        then
          begin
            repeat
              s:=sock.recvstring(timeout);
              if s='' then break;
            until sock.lasterror<>0;
          end
        else
          begin
           sending:=false;
           Status100Error:=s;
          end;
    end;

  {send document}
  if sending then
    begin
      Sock.SendBuffer(Document.memory,Document.size);
      if sock.lasterror<>0 then Exit;
    end;

  clear;
  size:=-1;
  TransferEncoding:=TE_UNKNOWN;

  {read status}
  If Status100Error=''
    then
      begin
        repeat
          s:=sock.recvstring(timeout);
          if s<>'' then break;
        until sock.lasterror<>0;
        if pos('HTTP/',uppercase(s))=1
          then
            begin
              Headers.add(s);
              decodeStatus(s);
            end
          else
            begin
              {old HTTP 0.9 and some buggy servers not send result}
              s:=s+CRLF;
              document.Write(pointer(s)^,length(s));
              ResultCode:=0;
            end;
      end
    else Headers.add(Status100Error);

  {if need receive hedaers, receive and parse it}
  ToClose:=protocol<>'1.1';
  if Headers.count>0 then
    repeat
      s:=sock.recvstring(timeout);
      Headers.Add(s);
      if s=''
        then break;
      su:=uppercase(s);
      if pos('CONTENT-LENGTH:',su)=1 then
        begin
          size:=strtointdef(separateright(s,' '),-1);
          TransferEncoding:=TE_IDENTITY;
        end;
      if pos('CONTENT-TYPE:',su)=1 then
        MimeType:=separateright(s,' ');
      if pos('TRANSFER-ENCODING:',su)=1 then
        begin
          s:=separateright(su,' ');
          if pos('CHUNKED',s)>0 then
            TransferEncoding:=TE_CHUNKED;
        end;
      if pos('CONNECTION: CLOSE',su)=1 then
        ToClose:=true;
    until sock.lasterror<>0;

  {if need receive response body, read it}
  Receiving:=Method<>'HEAD';
  Receiving:=Receiving and (ResultCode<>204);
  Receiving:=Receiving and (ResultCode<>304);
  if Receiving then
    case TransferEncoding of
      TE_UNKNOWN : readunknown;
      TE_IDENTITY: readidentity(size);
      TE_CHUNKED : readChunked;
    end;

  Document.Seek(0,soFromBeginning);
  result:=true;
  if ToClose then
    begin
      sock.closesocket;
      Alivehost:='';
      AlivePort:='';
    end;
end;

{THTTPSend.ReadUnknown}
function THTTPSend.ReadUnknown:boolean;
var
  s:string;
begin
  result:=false;
  repeat
    s:=sock.recvstring(timeout);
    s:=s+CRLF;
    document.Write(pointer(s)^,length(s));
  until sock.lasterror<>0;
  result:=true;
end;

{THTTPSend.ReadIdentity}
function THTTPSend.ReadIdentity(size:integer):boolean;
var
  mem:TMemoryStream;
begin
  mem:=TMemoryStream.create;
  try
    mem.SetSize(size);
    sock.RecvBufferEx(mem.memory,size,timeout);
    result:=sock.lasterror=0;
    document.CopyFrom(mem,0);
  finally
    mem.free;
  end;
end;

{THTTPSend.ReadChunked}
function THTTPSend.ReadChunked:boolean;
var
  s:string;
  size:integer;
begin
  repeat
    repeat
      s:=sock.recvstring(timeout);
    until s<>'';
    if sock.lasterror<>0
      then break;
    s:=separateleft(s,' ');
    size:=strtointdef('$'+s,0);
    if size=0 then break;
    ReadIdentity(size);
  until false;
  result:=sock.lasterror=0;
end;

{==============================================================================}

{HttpGetText}
function HttpGetText(URL:string;Response:TStrings):Boolean;
var
  HTTP:THTTPSend;
begin
  Result:=False;
  HTTP:=THTTPSend.Create;
  try
    Result:=HTTP.HTTPmethod('GET',URL);
    response.LoadFromStream(HTTP.document);
  finally
    HTTP.Free;
  end;
end;

{HttpGetBinary}
function HttpGetBinary(URL:string;Response:TStream):Boolean;
var
  HTTP:THTTPSend;
begin
  Result:=False;
  HTTP:=THTTPSend.Create;
  try
    Result:=HTTP.HTTPmethod('GET',URL);
    Response.Seek(0,soFromBeginning);
    Response.CopyFrom(HTTP.document,0);
  finally
    HTTP.Free;
  end;
end;

{HttpPostBinary}
function HttpPostBinary(URL:string;Data:TStream):Boolean;
var
  HTTP:THTTPSend;
begin
  Result:=False;
  HTTP:=THTTPSend.Create;
  try
    HTTP.Document.CopyFrom(data,0);
    HTTP.MimeType:='Application/octet-stream';
    Result:=HTTP.HTTPmethod('POST',URL);
    data.Seek(0,soFromBeginning);
    data.CopyFrom(HTTP.document,0);
  finally
    HTTP.Free;
  end;
end;


end.
