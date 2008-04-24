{==============================================================================|
| Project : Ararat Synapse                                       | 001.003.003 |
|==============================================================================|
| Content: NNTP client                                                         |
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

//RFC-977, RFC-2980

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit nntpsend;

interface

uses
  SysUtils, Classes,
  {$IFDEF STREAMSEC}
  TlsInternalServer, TlsSynaSock,
  {$ENDIF}
  blcksock, synautil, synacode;

const
  cNNTPProtocol = 'nntp';

type
  TNNTPSend = class(TSynaClient)
  private
    {$IFDEF STREAMSEC}
    FSock: TSsTCPBlockSocket;
    FTLSServer: TCustomTLSInternalServer;
    {$ELSE}
    FSock: TTCPBlockSocket;
    {$ENDIF}
    FResultCode: Integer;
    FResultString: string;
    FData: TStringList;
    FDataToSend: TStringList;
    FUsername: string;
    FPassword: string;
    FAutoTLS: Boolean;
    FFullSSL: Boolean;
    FNNTPcap: TStringList;
    function ReadResult: Integer;
    function ReadData: boolean;
    function SendData: boolean;
    function Connect: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Login: Boolean;
    procedure Logout;
    function DoCommand(const Command: string): boolean;
    function DoCommandRead(const Command: string): boolean;
    function DoCommandWrite(const Command: string): boolean;
    function GetArticle(const Value: string): Boolean;
    function GetBody(const Value: string): Boolean;
    function GetHead(const Value: string): Boolean;
    function GetStat(const Value: string): Boolean;
    function SelectGroup(const Value: string): Boolean;
    function IHave(const MessID: string): Boolean;
    function GotoLast: Boolean;
    function GotoNext: Boolean;
    function ListGroups: Boolean;
    function ListNewGroups(Since: TDateTime): Boolean;
    function NewArticles(const Group: string; Since: TDateTime): Boolean;
    function PostArticle: Boolean;
    function SwitchToSlave: Boolean;
    function Xover(xoStart, xoEnd: string): boolean;
    function StartTLS: Boolean;
    function FindCap(const Value: string): string;
    function ListExtensions: Boolean;
  published
    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
    property ResultCode: Integer read FResultCode;
    property ResultString: string read FResultString;
    property Data: TStringList read FData;
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;
    property FullSSL: Boolean read FFullSSL Write FFullSSL;
{$IFDEF STREAMSEC}
    property Sock: TSsTCPBlockSocket read FSock;
    property TLSServer: TCustomTLSInternalServer read FTLSServer write FTLSServer;
{$ELSE}
    property Sock: TTCPBlockSocket read FSock;
{$ENDIF}
  end;

implementation

constructor TNNTPSend.Create;
begin
  inherited Create;
{$IFDEF STREAMSEC}
  FTLSServer := GlobalTLSInternalServer;
  FSock := TSsTCPBlockSocket.Create;
  FSock.BlockingRead := True;
{$ELSE}
  FSock := TTCPBlockSocket.Create;
{$ENDIF}
  FData := TStringList.Create;
  FDataToSend := TStringList.Create;
  FNNTPcap := TStringList.Create;
  FSock.ConvertLineEnd := True;
  FTimeout := 60000;
  FTargetPort := cNNTPProtocol;
  FUsername := '';
  FPassword := '';
  FAutoTLS := False;
  FFullSSL := False;
end;

destructor TNNTPSend.Destroy;
begin
  FSock.Free;
  FDataToSend.Free;
  FData.Free;
  FNNTPcap.Free;
  inherited Destroy;
end;

function TNNTPSend.ReadResult: Integer;
var
  s: string;
begin
  Result := 0;
  FData.Clear;
  s := FSock.RecvString(FTimeout);
  FResultString := Copy(s, 5, Length(s) - 4);
  if FSock.LastError <> 0 then
    Exit;
  if Length(s) >= 3 then
    Result := StrToIntDef(Copy(s, 1, 3), 0);
  FResultCode := Result;
end;

function TNNTPSend.ReadData: boolean;
var
  s: string;
begin
  repeat
    s := FSock.RecvString(FTimeout);
    if s = '.' then
      break;
    if (s <> '') and (s[1] = '.') then
      s := Copy(s, 2, Length(s) - 1);
    FData.Add(s);
  until FSock.LastError <> 0;
  Result := FSock.LastError = 0;
end;

function TNNTPSend.SendData: boolean;
var
  s: string;
  n: integer;
begin
  for n := 0 to FDataToSend.Count - 1 do
  begin
    s := FDataToSend[n];
    if (s <> '') and (s[1] = '.') then
      s := s + '.';
    FSock.SendString(s + CRLF);
    if FSock.LastError <> 0 then
      break;
  end;
  if FDataToSend.Count = 0 then
    FSock.SendString(CRLF);
  if FSock.LastError = 0 then
    FSock.SendString('.' + CRLF);
  FDataToSend.Clear;
  Result := FSock.LastError = 0;
end;

function TNNTPSend.Connect: Boolean;
begin
  FSock.CloseSocket;
  FSock.Bind(FIPInterface, cAnyPort);
{$IFDEF STREAMSEC}
  if FFullSSL then
  begin
    if assigned(FTLSServer) then
      FSock.TLSServer := FTLSServer;
    else
    begin
      result := False;
      Exit;
    end;
  end
  else
    FSock.TLSServer := nil;
{$ELSE}
  if FFullSSL then
    FSock.SSLEnabled := True;
{$ENDIF}
  if FSock.LastError = 0 then
    FSock.Connect(FTargetHost, FTargetPort);
  Result := FSock.LastError = 0;
end;

function TNNTPSend.Login: Boolean;
begin
  Result := False;
  FNNTPcap.Clear;
  if not Connect then
    Exit;
  Result := (ReadResult div 100) = 2;
  ListExtensions;
  FNNTPcap.Assign(Fdata);
  if result then
    if (not FullSSL) and FAutoTLS and (FindCap('STARTTLS') <> '') then
      result := StartTLS;
  if (FUsername <> '') and Result then
  begin
    FSock.SendString('AUTHINFO USER ' + FUsername + CRLF);
    if (ReadResult div 100) = 3 then
    begin
      FSock.SendString('AUTHINFO PASS ' + FPassword + CRLF);
      Result := (ReadResult div 100) = 2;
    end;
  end;
end;

procedure TNNTPSend.Logout;
begin
  FSock.SendString('QUIT' + CRLF);
  ReadResult;
  FSock.CloseSocket;
end;

function TNNTPSend.DoCommand(const Command: string): Boolean;
begin
  FSock.SendString(Command + CRLF);
  Result := (ReadResult div 100) = 2;
  Result := Result and (FSock.LastError = 0);
end;

function TNNTPSend.DoCommandRead(const Command: string): Boolean;
begin
  Result := DoCommand(Command);
  if Result then
  begin
    Result := ReadData;
    Result := Result and (FSock.LastError = 0);
  end;
end;

function TNNTPSend.DoCommandWrite(const Command: string): Boolean;
var
  x: integer;
begin
  FDataToSend.Assign(FData);
  FSock.SendString(Command + CRLF);
  x := (ReadResult div 100);
  if x = 3 then
  begin
    SendData;
    x := (ReadResult div 100);
  end;
  Result := x = 2;
  Result := Result and (FSock.LastError = 0);
end;

function TNNTPSend.GetArticle(const Value: string): Boolean;
var
  s: string;
begin
  s := 'ARTICLE';
  if Value <> '' then
    s := s + ' ' + Value;
  Result := DoCommandRead(s);
end;

function TNNTPSend.GetBody(const Value: string): Boolean;
var
  s: string;
begin
  s := 'BODY';
  if Value <> '' then
    s := s + ' ' + Value;
  Result := DoCommandRead(s);
end;

function TNNTPSend.GetHead(const Value: string): Boolean;
var
  s: string;
begin
  s := 'HEAD';
  if Value <> '' then
    s := s + ' ' + Value;
  Result := DoCommandRead(s);
end;

function TNNTPSend.GetStat(const Value: string): Boolean;
var
  s: string;
begin
  s := 'STAT';
  if Value <> '' then
    s := s + ' ' + Value;
  Result := DoCommandRead(s);
end;

function TNNTPSend.SelectGroup(const Value: string): Boolean;
begin
  Result := DoCommand('GROUP ' + Value);
end;

function TNNTPSend.IHave(const MessID: string): Boolean;
begin
  Result := DoCommandWrite('IHAVE ' + MessID);
end;

function TNNTPSend.GotoLast: Boolean;
begin
  Result := DoCommand('LAST');
end;

function TNNTPSend.GotoNext: Boolean;
begin
  Result := DoCommand('NEXT');
end;

function TNNTPSend.ListGroups: Boolean;
begin
  Result := DoCommandRead('LIST');
end;

function TNNTPSend.ListNewGroups(Since: TDateTime): Boolean;
begin
  Result := DoCommandRead('NEWGROUPS ' + SimpleDateTime(Since) + ' GMT');
end;

function TNNTPSend.NewArticles(const Group: string; Since: TDateTime): Boolean;
begin
  Result := DoCommandRead('NEWNEWS ' + Group + ' ' + SimpleDateTime(Since) + ' GMT');
end;

function TNNTPSend.PostArticle: Boolean;
begin
  Result := DoCommandWrite('POST');
end;

function TNNTPSend.SwitchToSlave: Boolean;
begin
  Result := DoCommand('SLAVE');
end;

function TNNTPSend.Xover(xoStart, xoEnd: string): Boolean;
var
  s: string;
begin
  s := 'XOVER ' + xoStart;
  if xoEnd <> xoStart then
    s := s + '-' + xoEnd;
  Result := DoCommandRead(s);
end;

function TNNTPSend.StartTLS: Boolean;
begin
  Result := False;
  if FindCap('STARTTLS') <> '' then
  begin
    if DoCommand('STARTTLS') then
    begin
{$IFDEF STREAMSEC}
      if (Assigned(FTLSServer) then
      begin
        Fsock.TLSServer := FTLSServer;
        Fsock.Connect('','');
        Result := FSock.LastError = 0;
      end
      else
        Result := False;
{$ELSE}
      Fsock.SSLDoConnect;
      Result := FSock.LastError = 0;
{$ENDIF}
    end;
  end;
end;

function TNNTPSend.ListExtensions: Boolean;
begin
  Result := DoCommandRead('LIST EXTENSIONS');
end;

function TNNTPSend.FindCap(const Value: string): string;
var
  n: Integer;
  s: string;
begin
  s := UpperCase(Value);
  Result := '';
  for n := 0 to FNNTPcap.Count - 1 do
    if Pos(s, UpperCase(FNNTPcap[n])) = 1 then
    begin
      Result := FNNTPcap[n];
      Break;
    end;
end;

{==============================================================================}

end.
