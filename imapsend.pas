{==============================================================================|
| Project : Delphree - Synapse                                   | 002.000.000 |
|==============================================================================|
| Content: IMAP4rev1 client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2001-2002.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$WEAKPACKAGEUNIT ON}

//RFC-2060
//RFC-2595

unit IMAPsend;

interface

uses
  SysUtils, Classes,
  blcksock, SynaUtil, SynaCode;

const
  cIMAPProtocol = '143';

type
  TIMAPSend = class(TObject)
  private
    FSock: TTCPBlockSocket;
    FTimeout: Integer;
    FIMAPHost: string;
    FIMAPPort: string;
    FTagCommand: integer;
    FResultString: string;
    FFullResult: TStringList;
    FIMAPcap: TStringList;
    FUsername: string;
    FPassword: string;
    FAuthDone: Boolean;
    FSelectedFolder: string;
    FSelectedCount: integer;
    FSelectedRecent: integer;
    FSelectedUIDvalidity: integer;
    FUID: Boolean;
    FAutoTLS: Boolean;
    FFullSSL: Boolean;
    function ReadResult: string;
    function AuthLogin: Boolean;
    function Connect: Boolean;
    procedure ParseMess(Value:TStrings);
    procedure ParseFolderList(Value:TStrings);
    procedure ParseSelect;
    procedure ParseSearch(Value:TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    function IMAPcommand(Value: string): string;
    function IMAPuploadCommand(Value: string; const Data:TStrings): string;
    function Capability: Boolean;
    function Login: Boolean;
    procedure Logout;
    function NoOp: Boolean;
    function List(FromFolder: string; const FolderList: TStrings): Boolean;
    function ListSubscribed(FromFolder: string; const FolderList: TStrings): Boolean;
    function CreateFolder(FolderName: string): Boolean;
    function DeleteFolder(FolderName: string): Boolean;
    function RenameFolder(FolderName, NewFolderName: string): Boolean;
    function SubscribeFolder(FolderName: string): Boolean;
    function UnsubscribeFolder(FolderName: string): Boolean;
    function SelectFolder(FolderName: string): Boolean;
    function SelectROFolder(FolderName: string): Boolean;
    function CloseFolder: Boolean;
    function StatusFolder(FolderName, Value: string): integer;
    function ExpungeFolder: Boolean;
    function CheckFolder: Boolean;
    function AppendMess(ToFolder: string; const Mess: TStrings): Boolean;
    function DeleteMess(MessID: integer): boolean;
    function FetchMess(MessID: integer; const Mess: TStrings): Boolean;
    function FetchHeader(MessID: integer; const Headers: TStrings): Boolean;
    function MessageSize(MessID: integer): integer;
    function CopyMess(MessID: integer; ToFolder: string): Boolean;
    function SearchMess(Criteria: string; const FoundMess: TStrings): Boolean;
    function SetFlagsMess(MessID: integer; Flags: string): Boolean;
    function GetFlagsMess(MessID: integer; var Flags: string): Boolean;
    function StartTLS: Boolean;
    function FindCap(const Value: string): string;
  published
    property Timeout: Integer read FTimeout Write FTimeout;
    property IMAPHost: string read FIMAPHost Write FIMAPHost;
    property IMAPPort: string read FIMAPPort Write FIMAPPort;
    property ResultString: string read FResultString;
    property FullResult: TStringList read FFullResult;
    property IMAPcap: TStringList read FIMAPcap;
    property Username: string read FUsername Write FUsername;
    property Password: string read FPassword Write FPassword;
    property AuthDone: Boolean read FAuthDone;
    property UID: Boolean read FUID Write FUID;
    property Sock: TTCPBlockSocket read FSock;
    property SelectedFolder: string read FSelectedFolder;
    property SelectedCount: integer read FSelectedCount;
    property SelectedRecent: integer read FSelectedRecent;
    property SelectedUIDvalidity: integer read FSelectedUIDvalidity;
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;
    property FullSSL: Boolean read FFullSSL Write FFullSSL;
  end;

implementation

const
  CRLF = #13#10;

constructor TIMAPSend.Create;
begin
  inherited Create;
  FFullResult := TStringList.Create;
  FIMAPcap := TStringList.Create;
  FSock := TTCPBlockSocket.Create;
  FSock.CreateSocket;
  FSock.SizeRecvBuffer := 32768;
  FSock.SizeSendBuffer := 32768;
  FTimeout := 300000;
  FIMAPhost := cLocalhost;
  FIMAPPort := cIMAPProtocol;
  FUsername := '';
  FPassword := '';
  FTagCommand := 0;
  FSelectedFolder := '';
  FSelectedCount := 0;
  FSelectedRecent := 0;
  FSelectedUIDvalidity := 0;
  FUID := False;
  FAutoTLS := False;
  FFullSSL := False;
end;

destructor TIMAPSend.Destroy;
begin
  FSock.Free;
  FIMAPcap.Free;
  FFullResult.Free;
  inherited Destroy;
end;


function TIMAPSend.ReadResult: string;
var
  s: string;
  x, l: integer;
begin
  Result := '';
  FFullResult.Clear;
  FResultString := '';
  repeat
    s := FSock.RecvString(FTimeout);
    if Pos('S' + IntToStr(FTagCommand) + ' ', s) = 1 then
    begin
      FResultString := s;
      break;
    end
    else
      FFullResult.Add(s);
    if (s <> '') and (s[Length(s)]='}') then
    begin
      s := Copy(s, 1, Length(s) - 1);
      x := RPos('{', s);
      s := Copy(s, x + 1, Length(s) - x);
      l := StrToIntDef(s, -1);
      if l <> -1 then
      begin
        setlength(s, l);
        x := FSock.recvbufferex(PChar(s), l, FTimeout);
        SetLength(s, x);
        FFullResult.Add(s);
      end;
    end;
  until FSock.LastError <> 0;
  s := separateright(FResultString, ' ');
  Result:=uppercase(separateleft(s, ' '));
end;

function TIMAPSend.IMAPcommand(Value: string): string;
begin
  Inc(FTagCommand);
  FSock.SendString('S' + IntToStr(FTagCommand) + ' ' + Value + CRLF);
  Result := ReadResult;
end;

function TIMAPSend.IMAPuploadCommand(Value: string; const Data:TStrings): string;
var
  l: integer;
begin
  Inc(FTagCommand);
  l := Length(Data.Text);
  FSock.SendString(IntToStr(FTagCommand) + ' ' + Value + ' {'+ IntToStr(l) + '}' + CRLF);
  FSock.SendString(Data.Text + CRLF);
  Result := ReadResult;
end;

procedure TIMAPSend.ParseMess(Value:TStrings);
var
  n: integer;
begin
  Value.Clear;
  for n := 0 to FFullResult.Count - 2 do
    if FFullResult[n][Length(FFullResult[n])] = '}' then
    begin
      Value.Text := FFullResult[n + 1];
      Break;
    end;
end;

procedure TIMAPSend.ParseFolderList(Value:TStrings);
var
  n, x: integer;
  s: string;
begin
  Value.Clear;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := FFullResult[n];
    x := RPos(' ', s);
    if (x > 0) and (Pos('NOSELECT', UpperCase(s)) = 0) then
      Value.Add(Copy(s, x + 1, Length(s) - x));
  end;
end;

procedure TIMAPSend.ParseSelect;
var
  n: integer;
  s, t: string;
begin
  FSelectedCount := 0;
  FSelectedRecent := 0;
  FSelectedUIDvalidity := 0;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := uppercase(FFullResult[n]);
    if Pos(' EXISTS', s) > 0 then
    begin
      t := separateleft(s, ' EXISTS');
      t := separateright(t, '* ');
      FSelectedCount := StrToIntDef(t, 0);
    end;
    if Pos(' RECENT', s) > 0 then
    begin
      t := separateleft(s, ' RECENT');
      t := separateright(t, '* ');
      FSelectedRecent := StrToIntDef(t, 0);
    end;
    if Pos('UIDVALIDITY', s) > 0 then
    begin
      t := separateright(s, 'UIDVALIDITY ');
      t := separateleft(t, ']');
      FSelectedUIDvalidity := StrToIntDef(t, 0);
    end;
  end;
end;

procedure TIMAPSend.ParseSearch(Value:TStrings);
var
  n: integer;
  s: string;
begin
  Value.Clear;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := uppercase(FFullResult[n]);
    if Pos('* SEARCH', s) = 1 then
    begin
      s := SeparateRight(s, '* SEARCH');
      while s <> '' do
        Value.Add(Fetch(s, ' '));
    end;
  end;
end;

function TIMAPSend.FindCap(const Value: string): string;
var
  n: Integer;
  s: string;
begin
  s := UpperCase(Value);
  Result := '';
  for n := 0 to FIMAPcap.Count - 1 do
    if Pos(s, UpperCase(FIMAPcap[n])) = 1 then
    begin
      Result := FIMAPcap[n];
      Break;
    end;
end;

function TIMAPSend.AuthLogin: Boolean;
begin
  Result := IMAPcommand('LOGIN ' + FUsername + ' ' + FPassword) = 'OK';
end;

function TIMAPSend.Connect: Boolean;
begin
  FSock.CloseSocket;
  FSock.CreateSocket;
  if FFullSSL then
    FSock.SSLEnabled := True;
  FSock.Connect(FIMAPHost, FIMAPPort);
  Result := FSock.LastError = 0;
end;

function TIMAPSend.Capability: Boolean;
var
  n: Integer;
  s, t: string;
begin
  Result := False;
  FIMAPcap.Clear;
  s := IMAPcommand('CAPABILITY');
  if s = 'OK' then
  begin
    for n := 0 to FFullResult.Count - 1 do
      if Pos('* CAPABILITY ', FFullResult[n]) = 1 then
      begin
        s := SeparateRight(FFullResult[n], '* CAPABILITY ');
        while not (s = '') do
        begin
          t := separateleft(s, ' ');
          s := separateright(s, ' ');
          if s = t then
            s := '';
          FIMAPcap.Add(t);
        end;
      end;
    Result := True;
  end;
end;

function TIMAPSend.Login: Boolean;
var
  s: string;
begin
  FSelectedFolder := '';
  FSelectedCount := 0;
  FSelectedRecent := 0;
  FSelectedUIDvalidity := 0;
  Result := False;
  FAuthDone := False;
  if not Connect then
    Exit;
  s := FSock.RecvString(FTimeout);
  if Pos('* PREAUTH', s) = 1 then
    FAuthDone := True
  else
    if Pos('* OK', s) = 1 then
      FAuthDone := False
    else
      Exit;
  if Capability then
  begin
    if Findcap('IMAP4rev1') = '' then
      Exit;
    if FAutoTLS and (Findcap('STARTTLS') <> '') then
      if StartTLS then
        Capability;
  end;
  Result := AuthLogin;
end;

procedure TIMAPSend.Logout;
begin
  IMAPcommand('LOGOUT');
  FSelectedFolder := '';
  FSock.CloseSocket;
end;

function TIMAPSend.NoOp: Boolean;
begin
  Result := IMAPcommand('NOOP') = 'OK';
end;

function TIMAPSend.List(FromFolder: string; const FolderList: TStrings): Boolean;
begin
  Result := IMAPcommand('LIST "' + FromFolder + '" *') = 'OK';
  ParseFolderList(FolderList);
end;

function TIMAPSend.ListSubscribed(FromFolder: string; const FolderList: TStrings): Boolean;
begin
  Result := IMAPcommand('LSUB "' + FromFolder + '" *') = 'OK';
  ParseFolderList(FolderList);
end;

function TIMAPSend.CreateFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('CREATE "' + FolderName + '"') = 'OK';
end;

function TIMAPSend.DeleteFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('DELETE "' + FolderName + '"') = 'OK';
end;

function TIMAPSend.RenameFolder(FolderName, NewFolderName: string): Boolean;
begin
  Result := IMAPcommand('RENAME "' + FolderName + '" "' + NewFolderName + '"') = 'OK';
end;

function TIMAPSend.SubscribeFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('SUBSCRIBE "' + FolderName + '"') = 'OK';
end;

function TIMAPSend.UnsubscribeFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('UNSUBSCRIBE "' + FolderName + '"') = 'OK';
end;

function TIMAPSend.SelectFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('SELECT "' + FolderName + '"') = 'OK';
  FSelectedFolder := FolderName;
  ParseSelect;
end;

function TIMAPSend.SelectROFolder(FolderName: string): Boolean;
begin
  Result := IMAPcommand('EXAMINE "' + FolderName + '"') = 'OK';
  FSelectedFolder := FolderName;
  ParseSelect;
end;

function TIMAPSend.CloseFolder: Boolean;
begin
  Result := IMAPcommand('CLOSE') = 'OK';
  FSelectedFolder := '';
end;

function TIMAPSend.StatusFolder(FolderName, Value: string): integer;
var
  n: integer;
  s, t: string;
begin
  Result := -1;
  Value := Uppercase(Value);
  if IMAPcommand('STATUS "' + FolderName + '" (' + Value + ')' ) = 'OK' then
    for n := 0 to FFullResult.Count - 1 do
    begin
      s := UpperCase(FFullResult[n]);
      if (Pos('* STATUS ', s) = 1) and (Pos(Value, s) > 0 ) then
      begin
        t := SeparateRight(s, Value);
        t := SeparateLeft(t, ')');
        t := trim(t);
        Result := StrToIntDef(t, -1);
        Break;
      end;
    end;
end;

function TIMAPSend.ExpungeFolder: Boolean;
begin
  Result := IMAPcommand('EXPUNGE') = 'OK';
end;

function TIMAPSend.CheckFolder: Boolean;
begin
  Result := IMAPcommand('CHECK') = 'OK';
end;

function TIMAPSend.AppendMess(ToFolder: string; const Mess: TStrings): Boolean;
begin
  Result := IMAPuploadCommand('APPEND "' + ToFolder + '"', Mess) = 'OK';
end;

function TIMAPSend.DeleteMess(MessID: integer): boolean;
var
  s: string;
begin
  s := 'STORE ' + IntToStr(MessID) + ' +FLAGS.SILENT (\Deleted)';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
end;

function TIMAPSend.FetchMess(MessID: integer; const Mess: TStrings): Boolean;
var
  s: string;
begin
  s := 'FETCH ' + IntToStr(MessID) + ' (RFC822)';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
  ParseMess(Mess);
end;

function TIMAPSend.FetchHeader(MessID: integer; const Headers: TStrings): Boolean;
var
  s: string;
begin
  s := 'FETCH ' + IntToStr(MessID) + ' (RFC822.HEADER)';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
  ParseMess(Headers);
end;

function TIMAPSend.MessageSize(MessID: integer): integer;
var
  n: integer;
  s, t: string;
begin
  Result := -1;
  s := 'FETCH ' + IntToStr(MessID) + ' (RFC822.SIZE)';
  if FUID then
    s := 'UID ' + s;
  if IMAPcommand(s) = 'OK' then
    for n := 0 to FFullResult.Count - 1 do
    begin
      s := UpperCase(FFullResult[n]);
      if (Pos('* ', s) = 1) and (Pos('RFC822.SIZE', s) > 0 ) then
      begin
        t := SeparateRight(s, 'RFC822.SIZE ');
        t := SeparateLeft(t, ')');
        t := trim(t);
        Result := StrToIntDef(t, -1);
        Break;
      end;
    end;
end;

function TIMAPSend.CopyMess(MessID: integer; ToFolder: string): Boolean;
var
  s: string;
begin
  s := 'COPY ' + IntToStr(MessID) + ' "' + ToFolder + '"';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
end;

function TIMAPSend.SearchMess(Criteria: string; const FoundMess: TStrings): Boolean;
var
  s: string;
begin
  s := 'SEARCH ' + Criteria;
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
  ParseSearch(FoundMess);
end;

function TIMAPSend.SetFlagsMess(MessID: integer; Flags: string): Boolean;
var
  s: string;
begin
  s := 'STORE ' + IntToStr(MessID) + ' FLAGS.SILENT (' + Flags + ')';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
end;

function TIMAPSend.GetFlagsMess(MessID: integer; var Flags: string): Boolean;
var
  s: string;
  n: integer;
begin
  Flags := '';
  s := 'FETCH ' + IntToStr(MessID) + ' (FLAGS)';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := uppercase(FFullResult[n]);
    if Pos('* FETCH (FLAGS', s) = 1 then
    begin
      s := SeparateRight(s, 'FLAGS');
      s := Separateright(s, '(');
      Flags := SeparateLeft(s, ')');
    end;
  end;
end;

function TIMAPSend.StartTLS: Boolean;
begin
  Result := False;
  if FindCap('STARTTLS') <> '' then
  begin
    if IMAPcommand('STARTTLS') = 'OK' then
    begin
      Fsock.SSLDoConnect;
      Result := FSock.LastError = 0;
    end;
  end;
end;

{==============================================================================}

end.
