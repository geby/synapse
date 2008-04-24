{==============================================================================|
| Project : Delphree - Synapse                                   | 001.001.000 |
|==============================================================================|
| Content: NNTP client                                                         |
|==============================================================================|
| Copyright (c)1999-2002, Lukas Gebauer                                        |
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
  TNNTPSend = class(TSynaClient)
  private
    FSock: TTCPBlockSocket;
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
  FSock.ConvertLineEnd := True;
  FTimeout := 300000;
  FTargetPort := cNNTPProtocol;
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
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(FTargetHost, FTargetPort);
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
