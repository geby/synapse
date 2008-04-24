{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
|==============================================================================|
| Content: NNTP client                                                         |
|==============================================================================|
| The contents of this file are Subject to the Mozilla Public License Ver. 1.1 |
| (the "License"); you may not use this file except in compliance with the     |
| License. You may obtain a Copy of the License at http://www.mozilla.org/MPL/ |
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

{$WEAKPACKAGEUNIT ON}

unit NNTPsend;

interface

uses
  SysUtils, Classes,
  blcksock, SynaUtil, SynaCode;

const
  cNNTPProtocol = 'nntp';

type
  TNNTPSend = class(TObject)
  private
    FSock: TTCPBlockSocket;
    FTimeout: Integer;
    FNNTPHost: string;
    FNNTPPort: string;
    FResultCode: Integer;
    FResultString: string;
    FData: TStringList;
    function ReadResult: Integer;
    function ReadData: boolean;
    function SendData: boolean;
    function Connect: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Login: Boolean;
    procedure Logout;
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
  published
    property Timeout: Integer read FTimeout Write FTimeout;
    property NNTPHost: string read FNNTPHost Write FNNTPHost;
    property NNTPPort: string read FNNTPPort Write FNNTPPort;
    property ResultCode: Integer read FResultCode;
    property ResultString: string read FResultString;
    property Data: TStringList read FData;
    property Sock: TTCPBlockSocket read FSock;
  end;

implementation

const
  CRLF = #13#10;

constructor TNNTPSend.Create;
begin
  inherited Create;
  FData := TStringList.Create;
  FSock := TTCPBlockSocket.Create;
  FSock.CreateSocket;
  FTimeout := 300000;
  FNNTPhost := cLocalhost;
  FNNTPPort := cNNTPProtocol;
end;

destructor TNNTPSend.Destroy;
begin
  FSock.Free;
  FData.Free;
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
  Result := False;
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
  Result := False;
  for n := 0 to FData.Count -1 do
  begin
    s := FData[n];
    if (s <> '') and (s[1]='.') then
      s := s + '.';
    FSock.SendString(s + CRLF);
    if FSock.LastError <> 0 then
      break;
  end;
  Result := FSock.LastError = 0;
end;

function TNNTPSend.Connect: Boolean;
begin
  FSock.CloseSocket;
  FSock.CreateSocket;
  FSock.Connect(FNNTPHost, FNNTPPort);
  Result := FSock.LastError = 0;
end;

function TNNTPSend.Login: Boolean;
begin
  Result := False;
  if not Connect then
    Exit;
  Result := (ReadResult div 100) = 2;
end;

procedure TNNTPSend.Logout;
begin
  FSock.SendString('QUIT' + CRLF);
  ReadResult;
  FSock.CloseSocket;
end;

function TNNTPSend.GetArticle(const Value: string): Boolean;
var
  s: string;
begin
  Result := False;
  s := 'ARTICLE';
  if Value <> '' then
    s := s + ' ' + Value;
  FSock.SendString(s + CRLF);
  if (ReadResult div 100) <> 2 then
    Exit;
  Result := ReadData;
end;

function TNNTPSend.GetBody(const Value: string): Boolean;
var
  s: string;
begin
  Result := False;
  s := 'BODY';
  if Value <> '' then
    s := s + ' ' + Value;
  FSock.SendString(s + CRLF);
  if (ReadResult div 100) <> 2 then
    Exit;
  Result := ReadData;
end;

function TNNTPSend.GetHead(const Value: string): Boolean;
var
  s: string;
begin
  Result := False;
  s := 'HEAD';
  if Value <> '' then
    s := s + ' ' + Value;
  FSock.SendString(s + CRLF);
  if (ReadResult div 100) <> 2 then
    Exit;
  Result := ReadData;
end;

function TNNTPSend.GetStat(const Value: string): Boolean;
var
  s: string;
begin
  Result := False;
  s := 'STAT';
  if Value <> '' then
    s := s + ' ' + Value;
  FSock.SendString(s + CRLF);
  if (ReadResult div 100) <> 2 then
    Exit;
  Result := FSock.LastError = 0;
end;

function TNNTPSend.SelectGroup(const Value: string): Boolean;
begin
  FSock.SendString('GROUP ' + Value + CRLF);
  Result := (ReadResult div 100) = 2;
end;

function TNNTPSend.IHave(const MessID: string): Boolean;
var
  x: integer;
begin
  FSock.SendString('IHAVE ' + MessID + CRLF);
  x := (ReadResult div 100);
  if x = 3 then
  begin
    SendData;
    x := (ReadResult div 100);
  end;
  Result := x = 2;
end;

function TNNTPSend.GotoLast: Boolean;
begin
  FSock.SendString('LAST' + CRLF);
  Result := (ReadResult div 100) = 2;
end;

function TNNTPSend.GotoNext: Boolean;
begin
  FSock.SendString('NEXT' + CRLF);
  Result := (ReadResult div 100) = 2;
end;

function TNNTPSend.ListGroups: Boolean;
begin
  FSock.SendString('LIST' + CRLF);
  Result := (ReadResult div 100) = 2;
  if Result then
    Result := ReadData;
end;

function TNNTPSend.ListNewGroups(Since: TDateTime): Boolean;
begin
  FSock.SendString('NEWGROUPS ' + SimpleDateTime(Since) + ' GMT' + CRLF);
  Result := (ReadResult div 100) = 2;
  if Result then
    Result := ReadData;
end;

function TNNTPSend.NewArticles(const Group: string; Since: TDateTime): Boolean;
begin
  FSock.SendString('NEWNEWS ' + Group + ' ' + SimpleDateTime(Since) + ' GMT' + CRLF);
  Result := (ReadResult div 100) = 2;
  if Result then
    Result := ReadData;
end;

function TNNTPSend.PostArticle: Boolean;
var
  x: integer;
begin
  FSock.SendString('POST' + CRLF);
  x := (ReadResult div 100);
  if x = 3 then
  begin
    SendData;
    x := (ReadResult div 100);
  end;
  Result := x = 2;
end;

function TNNTPSend.SwitchToSlave: Boolean;
begin
  FSock.SendString('SLAVE' + CRLF);
  Result := (ReadResult div 100) = 2;
end;

{==============================================================================}

end.
