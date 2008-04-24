{==============================================================================|
| Project : Ararat Synapse                                       | 003.006.007 |
|==============================================================================|
| Content: HTTP client                                                         |
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
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

//RFC-1867, RFC-1947, RFC-2388, RFC-2616 

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit httpsend;

interface

uses
  SysUtils, Classes,
  {$IFDEF STREAMSEC}
  TlsInternalServer, TlsSynaSock,
  {$ENDIF}
  blcksock, synautil, synacode;

const
  cHttpProtocol = '80';

type
  TTransferEncoding = (TE_UNKNOWN, TE_IDENTITY, TE_CHUNKED);

  THTTPSend = class(TSynaClient)
  private
    {$IFDEF STREAMSEC}
    FSock: TSsTCPBlockSocket;
    FTLSServer: TCustomTLSInternalServer;
    {$ELSE}
    FSock: TTCPBlockSocket;
    {$ENDIF}
    FTransferEncoding: TTransferEncoding;
    FAliveHost: string;
    FAlivePort: string;
    FHeaders: TStringList;
    FDocument: TMemoryStream;
    FMimeType: string;
    FProtocol: string;
    FKeepAlive: Boolean;
    FStatus100: Boolean;
    FProxyHost: string;
    FProxyPort: string;
    FProxyUser: string;
    FProxyPass: string;
    FResultCode: Integer;
    FResultString: string;
    FUserAgent: string;
    FCookies: TStringList;
    FDownloadSize: integer;
    FUploadSize: integer;
    FRangeStart: integer;
    FRangeEnd: integer;
    function ReadUnknown: Boolean;
    function ReadIdentity(Size: Integer): Boolean;
    function ReadChunked: Boolean;
    procedure ParseCookies;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DecodeStatus(const Value: string);
    function HTTPMethod(const Method, URL: string): Boolean;
    procedure Abort;
  published
    property Headers: TStringList read FHeaders;
    property Cookies: TStringList read FCookies;
    property Document: TMemoryStream read FDocument;
    property RangeStart: integer read FRangeStart Write FRangeStart;
    property RangeEnd: integer read FRangeEnd Write FRangeEnd;
    property MimeType: string read FMimeType Write FMimeType;
    property Protocol: string read FProtocol Write FProtocol;
    property KeepAlive: Boolean read FKeepAlive Write FKeepAlive;
    property Status100: Boolean read FStatus100 Write FStatus100;
    property ProxyHost: string read FProxyHost Write FProxyHost;
    property ProxyPort: string read FProxyPort Write FProxyPort;
    property ProxyUser: string read FProxyUser Write FProxyUser;
    property ProxyPass: string read FProxyPass Write FProxyPass;
    property UserAgent: string read FUserAgent Write FUserAgent;
    property ResultCode: Integer read FResultCode;
    property ResultString: string read FResultString;
    property DownloadSize: integer read FDownloadSize;
    property UploadSize: integer read FUploadSize;
{$IFDEF STREAMSEC}                            
    property Sock: TSsTCPBlockSocket read FSock;
    property TLSServer: TCustomTLSInternalServer read FTLSServer write FTLSServer;
{$ELSE}
    property Sock: TTCPBlockSocket read FSock;
{$ENDIF}
  end;

function HttpGetText(const URL: string; const Response: TStrings): Boolean;
function HttpGetBinary(const URL: string; const Response: TStream): Boolean;
function HttpPostBinary(const URL: string; const Data: TStream): Boolean;
function HttpPostURL(const URL, URLData: string; const Data: TStream): Boolean;
function HttpPostFile(const URL, FieldName, FileName: string;
  const Data: TStream; const ResultData: TStrings): Boolean;

implementation

constructor THTTPSend.Create;
begin
  inherited Create;
  FHeaders := TStringList.Create;
  FCookies := TStringList.Create;
  FDocument := TMemoryStream.Create;
{$IFDEF STREAMSEC}           
  FTLSServer := GlobalTLSInternalServer;     
  FSock := TSsTCPBlockSocket.Create;
  FSock.BlockingRead := True;
{$ELSE}
  FSock := TTCPBlockSocket.Create;
{$ENDIF}
  FSock.ConvertLineEnd := True;
  FSock.SizeRecvBuffer := 65536;
  FSock.SizeSendBuffer := 65536;
  FTimeout := 90000;
  FTargetPort := cHttpProtocol;
  FProxyHost := '';
  FProxyPort := '8080';
  FProxyUser := '';
  FProxyPass := '';
  FAliveHost := '';
  FAlivePort := '';
  FProtocol := '1.0';
  FKeepAlive := True;
  FStatus100 := False;
  FUserAgent := 'Mozilla/4.0 (compatible; Synapse)';
  FDownloadSize := 0;
  FUploadSize := 0;
  Clear;
end;

destructor THTTPSend.Destroy;
begin
  FSock.Free;
  FDocument.Free;
  FCookies.Free;
  FHeaders.Free;
  inherited Destroy;
end;

procedure THTTPSend.Clear;
begin
  FRangeStart := 0;
  FRangeEnd := 0;
  FDocument.Clear;
  FHeaders.Clear;
  FMimeType := 'text/html';
end;

procedure THTTPSend.DecodeStatus(const Value: string);
var
  s, su: string;
begin
  s := SeparateRight(Value, ' ');
  su := SeparateLeft(s, ' ');
  FResultCode := StrToIntDef(su, 0);
  FResultString := SeparateRight(s, ' ');
  if FResultString = s then
    FResultString := '';
end;

function THTTPSend.HTTPMethod(const Method, URL: string): Boolean;
var
  Sending, Receiving: Boolean;
  status100: Boolean;
  status100error: string;
  ToClose: Boolean;
  Size: Integer;
  Prot, User, Pass, Host, Port, Path, Para, URI: string;
  s, su: string;
  HttpTunnel: Boolean;
  n: integer;
begin
  {initial values}
  Result := False;
  FResultCode := 500;
  FResultString := '';
  FDownloadSize := 0;
  FUploadSize := 0;

  URI := ParseURL(URL, Prot, User, Pass, Host, Port, Path, Para);
  if UpperCase(Prot) = 'HTTPS' then
  begin
    HttpTunnel := FProxyHost <> '';
    FSock.HTTPTunnelIP := FProxyHost;
    FSock.HTTPTunnelPort := FProxyPort;
    FSock.HTTPTunnelUser := FProxyUser;
    FSock.HTTPTunnelPass := FProxyPass;
  end
  else
  begin
    HttpTunnel := False;
    FSock.HTTPTunnelIP := '';
    FSock.HTTPTunnelPort := '';
    FSock.HTTPTunnelUser := '';
    FSock.HTTPTunnelPass := '';
  end;

  Sending := Document.Size > 0;
  {Headers for Sending data}
  status100 := FStatus100 and Sending and (FProtocol = '1.1');
  if status100 then
    FHeaders.Insert(0, 'Expect: 100-continue');
  if Sending then
  begin
    FHeaders.Insert(0, 'Content-Length: ' + IntToStr(FDocument.Size));
    if FMimeType <> '' then
      FHeaders.Insert(0, 'Content-Type: ' + FMimeType);
  end;
  { setting User-agent }
  if FUserAgent <> '' then
    FHeaders.Insert(0, 'User-Agent: ' + FUserAgent);
  { setting Ranges }
  if FRangeEnd > 0 then
    FHeaders.Insert(0, 'Range: bytes=' + IntToStr(FRangeStart) + '-' + IntToStr(FRangeEnd));
  { setting Cookies }
  for n := 0 to FCookies.Count - 1 do
    FHeaders.Insert(0, 'Cookie: ' + FCookies[n]);
  { setting KeepAlives }
  if not FKeepAlive then
    FHeaders.Insert(0, 'Connection: close');
  { set target servers/proxy, authorizations, etc... }
  if User <> '' then
    FHeaders.Insert(0, 'Authorization: Basic ' + EncodeBase64(user + ':' + pass));
  if (FProxyHost <> '') and (FProxyUser <> '') and not(HttpTunnel) then
    FHeaders.Insert(0, 'Proxy-Authorization: Basic ' +
      EncodeBase64(FProxyUser + ':' + FProxyPass));
  if isIP6(Host) then
    s := '[' + Host + ']'
  else
    s := Host;
  if Port<>'80' then
     FHeaders.Insert(0, 'Host: ' + s + ':' + Port)
  else
     FHeaders.Insert(0, 'Host: ' + s);
  if (FProxyHost <> '') and not(HttpTunnel)then
    URI := Prot + '://' + s + ':' + Port + URI;
  if URI = '/*' then
    URI := '*';
  if FProtocol = '0.9' then
    FHeaders.Insert(0, UpperCase(Method) + ' ' + URI)
  else
    FHeaders.Insert(0, UpperCase(Method) + ' ' + URI + ' HTTP/' + FProtocol);
  if (FProxyHost <> '') and not(HttpTunnel) then
  begin
    FTargetHost := FProxyHost;
    FTargetPort := FProxyPort;
  end
  else
  begin
    FTargetHost := Host;
    FTargetPort := Port;
  end;
  if FHeaders[FHeaders.Count - 1] <> '' then
    FHeaders.Add('');

  { connect }
  if (FAliveHost <> FTargetHost) or (FAlivePort <> FTargetPort) then
  begin
    FSock.CloseSocket;
    FSock.Bind(FIPInterface, cAnyPort);
    if FSock.LastError <> 0 then
      Exit;
{$IFDEF STREAMSEC}
    FSock.TLSServer := nil;
    if UpperCase(Prot) = 'HTTPS' then
      if assigned(FTLSServer) then
        FSock.TLSServer := FTLSServer
      else
        exit;
{$ELSE}
    FSock.SSLEnabled := UpperCase(Prot) = 'HTTPS';
{$ENDIF}
    if FSock.LastError <> 0 then
      Exit;
    FSock.Connect(FTargetHost, FTargetPort);
    if FSock.LastError <> 0 then
      Exit;
    FAliveHost := FTargetHost;
    FAlivePort := FTargetPort;
  end
  else
  begin
    if FSock.CanRead(0) then
    begin
      FSock.CloseSocket;
      FSock.Bind(FIPInterface, cAnyPort);
      if FSock.LastError <> 0 then
        Exit;
{$IFDEF STREAMSEC}
      FSock.TLSServer := nil;
      if UpperCase(Prot) = 'HTTPS' then
        if assigned(FTLSServer) then
          FSock.TLSServer := FTLSServer
        else
          exit;
{$ELSE}
      FSock.SSLEnabled := UpperCase(Prot) = 'HTTPS';
{$ENDIF}
      if FSock.LastError <> 0 then
        Exit;
      FSock.Connect(FTargetHost, FTargetPort);
      if FSock.LastError <> 0 then
        Exit;
    end;
  end;

  { send Headers }
  if FProtocol = '0.9' then
    FSock.SendString(FHeaders[0] + CRLF)
  else
{$IFDEF LINUX}
    FSock.SendString(AdjustLineBreaks(FHeaders.Text, tlbsCRLF));
{$ELSE}
    FSock.SendString(FHeaders.Text);
{$ENDIF}
  if FSock.LastError <> 0 then
    Exit;

  { reading Status }
  Status100Error := '';
  if status100 then
  begin
    repeat
      s := FSock.RecvString(FTimeout);
      if s <> '' then
        Break;
    until FSock.LastError <> 0;
    DecodeStatus(s);
    if (FResultCode >= 100) and (FResultCode < 200) then
      repeat
        s := FSock.recvstring(FTimeout);
        if s = '' then
          Break;
      until FSock.LastError <> 0
    else
    begin
      Sending := False;
      Status100Error := s;
    end;
  end;

  { send document }
  if Sending then
  begin
    FUploadSize := FDocument.Size;
    FSock.SendBuffer(FDocument.Memory, FDocument.Size);
    if FSock.LastError <> 0 then
      Exit;
  end;

  Clear;
  Size := -1;
  FTransferEncoding := TE_UNKNOWN;

  { read status }
  if Status100Error = '' then
  begin
    repeat
      s := FSock.RecvString(FTimeout);
      if s <> '' then
        Break;
    until FSock.LastError <> 0;
    if Pos('HTTP/', UpperCase(s)) = 1 then
    begin
      FHeaders.Add(s);
      DecodeStatus(s);
    end
    else
    begin
      { old HTTP 0.9 and some buggy servers not send result }
      s := s + CRLF;
      FDocument.Write(Pointer(s)^, Length(s));
      FResultCode := 0;
    end;
  end
  else
    FHeaders.Add(Status100Error);

  { if need receive headers, receive and parse it }
  ToClose := FProtocol <> '1.1';
  if FHeaders.Count > 0 then
    repeat
      s := FSock.RecvString(FTimeout);
      FHeaders.Add(s);
      if s = '' then
        Break;
      su := UpperCase(s);
      if Pos('CONTENT-LENGTH:', su) = 1 then
      begin
        Size := StrToIntDef(SeparateRight(s, ' '), -1);
        if Size <> -1 then
          FTransferEncoding := TE_IDENTITY;
      end;
      if Pos('CONTENT-TYPE:', su) = 1 then
        FMimeType := SeparateRight(s, ' ');
      if Pos('TRANSFER-ENCODING:', su) = 1 then
      begin
        s := SeparateRight(su, ' ');
        if Pos('CHUNKED', s) > 0 then
          FTransferEncoding := TE_CHUNKED;
      end;
      if Pos('CONNECTION: CLOSE', su) = 1 then
        ToClose := True;
    until FSock.LastError <> 0;

  {if need receive response body, read it}
  Receiving := Method <> 'HEAD';
  Receiving := Receiving and (FResultCode <> 204);
  Receiving := Receiving and (FResultCode <> 304);
  if Receiving then
    case FTransferEncoding of
      TE_UNKNOWN:
        ReadUnknown;
      TE_IDENTITY:
        ReadIdentity(Size);
      TE_CHUNKED:
        ReadChunked;
    end;
  Result := True;

  FDocument.Seek(0, soFromBeginning);
  if ToClose then
  begin
    FSock.CloseSocket;
    FAliveHost := '';
    FAlivePort := '';
  end;
  ParseCookies;
end;

function THTTPSend.ReadUnknown: Boolean;
var
  s: string;
begin
  repeat
    s := FSock.RecvPacket(FTimeout);
    if FSock.LastError = 0 then
      FDocument.Write(Pointer(s)^, Length(s));
  until FSock.LastError <> 0;
  Result := True;
end;

function THTTPSend.ReadIdentity(Size: Integer): Boolean;
var
  x: integer;
begin
  FDownloadSize := Size;
  FDocument.SetSize(FDocument.Position + Size);
  x := FSock.RecvBufferEx(IncPoint(FDocument.Memory, FDocument.Position), Size, FTimeout);
  FDocument.SetSize(FDocument.Position + x);
  Result := FSock.LastError = 0;
end;

function THTTPSend.ReadChunked: Boolean;
var
  s: string;
  Size: Integer;
begin
  repeat
    repeat
      s := FSock.RecvString(FTimeout);
    until s <> '';
    if FSock.LastError <> 0 then
      Break;
    s := SeparateLeft(s, ' ');
    Size := StrToIntDef('$' + s, 0);
    if Size = 0 then
      Break;
    ReadIdentity(Size);
  until False;
  Result := FSock.LastError = 0;
end;

procedure THTTPSend.ParseCookies;
var
  n: integer;
  s: string;
  sn, sv: string;
begin
  for n := 0 to FHeaders.Count - 1 do
    if Pos('set-cookie:', lowercase(FHeaders[n])) = 1 then
    begin
      s := SeparateRight(FHeaders[n], ':');
      s := trim(SeparateLeft(s, ';'));
      sn := trim(SeparateLeft(s, '='));
      sv := trim(SeparateRight(s, '='));
      FCookies.Values[sn] := sv;
    end;
end;

procedure THTTPSend.Abort;
begin
  FSock.AbortSocket;
end;

{==============================================================================}

function HttpGetText(const URL: string; const Response: TStrings): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    Result := HTTP.HTTPMethod('GET', URL);
    Response.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;

function HttpGetBinary(const URL: string; const Response: TStream): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    Result := HTTP.HTTPMethod('GET', URL);
    Response.Seek(0, soFromBeginning);
    Response.CopyFrom(HTTP.Document, 0);
  finally
    HTTP.Free;
  end;
end;

function HttpPostBinary(const URL: string; const Data: TStream): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.Document.CopyFrom(Data, 0);
    HTTP.MimeType := 'Application/octet-stream';
    Result := HTTP.HTTPMethod('POST', URL);
    Data.Seek(0, soFromBeginning);
    Data.CopyFrom(HTTP.Document, 0);
  finally
    HTTP.Free;
  end;
end;

function HttpPostURL(const URL, URLData: string; const Data: TStream): Boolean;
var
  HTTP: THTTPSend;
begin
  HTTP := THTTPSend.Create;
  try
    HTTP.Document.Write(Pointer(URLData)^, Length(URLData));
    HTTP.MimeType := 'application/x-www-form-urlencoded';
    Result := HTTP.HTTPMethod('POST', URL);
    Data.CopyFrom(HTTP.Document, 0);
  finally
    HTTP.Free;
  end;
end;

function HttpPostFile(const URL, FieldName, FileName: string;
  const Data: TStream; const ResultData: TStrings): Boolean;
var
  HTTP: THTTPSend;
  Bound, s: string;
begin
  Bound := IntToHex(Random(MaxInt), 8) + '_Synapse_boundary';
  HTTP := THTTPSend.Create;
  try
    s := '--' + Bound + CRLF;
    s := s + 'content-disposition: form-data; name="' + FieldName + '";';
    s := s + ' filename="' + FileName +'"' + CRLF;
    s := s + 'Content-Type: Application/octet-string' + CRLF + CRLF;
    HTTP.Document.Write(Pointer(s)^, Length(s));
    HTTP.Document.CopyFrom(Data, 0);
    s := CRLF + '--' + Bound + '--' + CRLF;
    HTTP.Document.Write(Pointer(s)^, Length(s));
    HTTP.MimeType := 'multipart/form-data; boundary=' + Bound;
    Result := HTTP.HTTPMethod('POST', URL);
    ResultData.LoadFromStream(HTTP.Document);
  finally
    HTTP.Free;
  end;
end;

end.
