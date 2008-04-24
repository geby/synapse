{==============================================================================|
| Project : Delphree - Synapse                                   | 002.000.000 |
|==============================================================================|
| Content: POP3 client                                                         |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.1 |
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
| Portions created by Lukas Gebauer are Copyright (c)2001-2002.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$WEAKPACKAGEUNIT ON}

//RFC-1734
//RFC-1939
//RFC-2195
//RFC-2449
//RFC-2595

unit POP3send;

interface

uses
  SysUtils, Classes,
  blcksock, SynaUtil, SynaCode;

const
  cPop3Protocol = 'pop3';

type
  TPOP3AuthType = (POP3AuthAll, POP3AuthLogin, POP3AuthAPOP);

  TPOP3Send = class(TObject)
  private
    FSock: TTCPBlockSocket;
    FTimeout: Integer;
    FPOP3Host: string;
    FPOP3Port: string;
    FResultCode: Integer;
    FResultString: string;
    FFullResult: TStringList;
    FUsername: string;
    FPassword: string;
    FStatCount: Integer;
    FStatSize: Integer;
    FTimeStamp: string;
    FAuthType: TPOP3AuthType;
    FPOP3cap: TStringList;
    FAutoTLS: Boolean;
    FFullSSL: Boolean;
    function ReadResult(Full: Boolean): Integer;
    function Connect: Boolean;
    function AuthLogin: Boolean;
    function AuthApop: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function Capability: Boolean;
    function Login: Boolean;
    procedure Logout;
    function Reset: Boolean;
    function NoOp: Boolean;
    function Stat: Boolean;
    function List(Value: Integer): Boolean;
    function Retr(Value: Integer): Boolean;
    function Dele(Value: Integer): Boolean;
    function Top(Value, Maxlines: Integer): Boolean;
    function Uidl(Value: Integer): Boolean;
    function StartTLS: Boolean;
    function FindCap(const Value: string): string;
  published
    property Timeout: Integer read FTimeout Write FTimeout;
    property POP3Host: string read FPOP3Host Write FPOP3Host;
    property POP3Port: string read FPOP3Port Write FPOP3Port;
    property ResultCode: Integer read FResultCode;
    property ResultString: string read FResultString;
    property FullResult: TStringList read FFullResult;
    property Username: string read FUsername Write FUsername;
    property Password: string read FPassword Write FPassword;
    property StatCount: Integer read FStatCount;
    property StatSize: Integer read  FStatSize;
    property TimeStamp: string read FTimeStamp;
    property AuthType: TPOP3AuthType read FAuthType Write FAuthType;
    property Sock: TTCPBlockSocket read FSock;
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;
    property FullSSL: Boolean read FFullSSL Write FFullSSL;
  end;

implementation

const
  CRLF = #13#10;

constructor TPOP3Send.Create;
begin
  inherited Create;
  FFullResult := TStringList.Create;
  FPOP3cap := TStringList.Create;
  FSock := TTCPBlockSocket.Create;
  FSock.CreateSocket;
  FTimeout := 300000;
  FPOP3host := cLocalhost;
  FPOP3Port := cPop3Protocol;
  FUsername := '';
  FPassword := '';
  FStatCount := 0;
  FStatSize := 0;
  FAuthType := POP3AuthAll;
  FAutoTLS := False;
  FFullSSL := False;
end;

destructor TPOP3Send.Destroy;
begin
  FSock.Free;
  FPOP3cap.Free;
  FullResult.Free;
  inherited Destroy;
end;

function TPOP3Send.ReadResult(Full: Boolean): Integer;
var
  s: string;
begin
  Result := 0;
  FFullResult.Clear;
  s := FSock.RecvString(FTimeout);
  if Pos('+OK', s) = 1 then
    Result := 1;
  FResultString := s;
  if Full and (Result = 1) then
    repeat
      s := FSock.RecvString(FTimeout);
      if s = '.' then
        Break;
      FFullResult.Add(s);
    until FSock.LastError <> 0;
  FResultCode := Result;
end;

function TPOP3Send.AuthLogin: Boolean;
begin
  Result := False;
  FSock.SendString('USER ' + FUserName + CRLF);
  if ReadResult(False) <> 1 then
    Exit;
  FSock.SendString('PASS ' + FPassword + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.AuthAPOP: Boolean;
var
  s: string;
begin
  s := StrToHex(MD5(FTimeStamp + FPassWord));
  FSock.SendString('APOP ' + FUserName + ' ' + s + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.Connect: Boolean;
begin
  // Do not call this function! It is calling by LOGIN method!
  FStatCount := 0;
  FStatSize := 0;
  FSock.CloseSocket;
  FSock.LineBuffer := '';
  FSock.CreateSocket;
  if FFullSSL then
    FSock.SSLEnabled := True;
  FSock.Connect(POP3Host, POP3Port);
  Result := FSock.LastError = 0;
end;

function TPOP3Send.Capability: Boolean;
begin
  FPOP3cap.Clear;
  Result := False;
  FSock.SendString('CAPA' + CRLF);
  Result := ReadResult(True) = 1;
  if Result then
    FPOP3cap.AddStrings(FFullResult);
end;

function TPOP3Send.Login: Boolean;
var
  s, s1: string;
begin
  Result := False;
  FTimeStamp := '';
  if not Connect then
    Exit;
  if ReadResult(False) <> 1 then
    Exit;
  s := SeparateRight(FResultString, '<');
  if s <> FResultString then
  begin
    s1 := SeparateLeft(s, '>');
    if s1 <> s then
      FTimeStamp := '<' + s1 + '>';
  end;
  Result := False;
  if Capability then
    if FAutoTLS and (Findcap('STLS') <> '') then
      if StartTLS then
        Capability;
  if (FTimeStamp <> '') and not (FAuthType = POP3AuthLogin) then
  begin
    Result := AuthApop;
    if not Result then
    begin
      if not Connect then
        Exit;
      if ReadResult(False) <> 1 then
        Exit;
    end;
  end;
  if not Result and not (FAuthType = POP3AuthAPOP) then
    Result := AuthLogin;
end;

procedure TPOP3Send.Logout;
begin
  FSock.SendString('QUIT' + CRLF);
  ReadResult(False);
  FSock.CloseSocket;
end;

function TPOP3Send.Reset: Boolean;
begin
  FSock.SendString('RSET' + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.NoOp: Boolean;
begin
  FSock.SendString('NOOP' + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.Stat: Boolean;
var
  s: string;
begin
  Result := False;
  FSock.SendString('STAT' + CRLF);
  if ReadResult(False) <> 1 then
    Exit;
  s := SeparateRight(ResultString, '+OK ');
  FStatCount := StrToIntDef(SeparateLeft(s, ' '), 0);
  FStatSize := StrToIntDef(SeparateRight(s, ' '), 0);
  Result := True;
end;

function TPOP3Send.List(Value: Integer): Boolean;
begin
  if Value = 0 then
    FSock.SendString('LIST' + CRLF)
  else
    FSock.SendString('LIST ' + IntToStr(Value) + CRLF);
  Result := ReadResult(Value = 0) = 1;
end;

function TPOP3Send.Retr(Value: Integer): Boolean;
begin
  FSock.SendString('RETR ' + IntToStr(Value) + CRLF);
  Result := ReadResult(True) = 1;
end;

function TPOP3Send.Dele(Value: Integer): Boolean;
begin
  FSock.SendString('DELE ' + IntToStr(Value) + CRLF);
  Result := ReadResult(False) = 1;
end;

function TPOP3Send.Top(Value, Maxlines: Integer): Boolean;
begin
  FSock.SendString('TOP ' + IntToStr(Value) + ' ' + IntToStr(Maxlines) + CRLF);
  Result := ReadResult(True) = 1;
end;

function TPOP3Send.Uidl(Value: Integer): Boolean;
begin
  if Value = 0 then
    FSock.SendString('UIDL' + CRLF)
  else
    FSock.SendString('UIDL ' + IntToStr(Value) + CRLF);
  Result := ReadResult(Value = 0) = 1;
end;

function TPOP3Send.StartTLS: Boolean;
begin
  Result := False;
  FSock.SendString('STLS' + CRLF);
  if ReadResult(False) = 1 then
  begin
    Fsock.SSLDoConnect;
    Result := FSock.LastError = 0;
  end;
end;

function TPOP3Send.FindCap(const Value: string): string;
var
  n: Integer;
  s: string;
begin
  s := UpperCase(Value);
  Result := '';
  for n := 0 to FPOP3cap.Count - 1 do
    if Pos(s, UpperCase(FPOP3cap[n])) = 1 then
    begin
      Result := FPOP3cap[n];
      Break;
    end;
end;

end.
