{==============================================================================|
| Project : Ararat Synapse                                       | 002.004.002 |
|==============================================================================|
| Content: IMAP4rev1 client                                                    |
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
| Portions created by Lukas Gebauer are Copyright (c)2001-2003.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

//RFC-2060, RFC-2595

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit imapsend;

interface

uses
  SysUtils, Classes,
  {$IFDEF STREAMSEC}
  TlsInternalServer, TlsSynaSock,
  {$ENDIF}
  blcksock, synautil, synacode;

const
  cIMAPProtocol = '143';

type
  TIMAPSend = class(TSynaClient)
  private
    {$IFDEF STREAMSEC}
    FSock: TSsTCPBlockSocket;
    FTLSServer: TCustomTLSInternalServer;
    {$ELSE}
    FSock: TTCPBlockSocket;
    {$ENDIF}
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
    procedure ProcessLiterals;
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
    function AddFlagsMess(MessID: integer; Flags: string): Boolean;
    function DelFlagsMess(MessID: integer; Flags: string): Boolean;
    function StartTLS: Boolean;
    function GetUID(MessID: integer; var UID : Integer): Boolean;
    function FindCap(const Value: string): string;
  published
    property ResultString: string read FResultString;
    property FullResult: TStringList read FFullResult;
    property IMAPcap: TStringList read FIMAPcap;
    property Username: string read FUsername Write FUsername;
    property Password: string read FPassword Write FPassword;
    property AuthDone: Boolean read FAuthDone;
    property UID: Boolean read FUID Write FUID;
    property SelectedFolder: string read FSelectedFolder;
    property SelectedCount: integer read FSelectedCount;
    property SelectedRecent: integer read FSelectedRecent;
    property SelectedUIDvalidity: integer read FSelectedUIDvalidity;
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

constructor TIMAPSend.Create;
begin
  inherited Create;
  FFullResult := TStringList.Create;
  FIMAPcap := TStringList.Create;
{$IFDEF STREAMSEC}           
  FTLSServer := GlobalTLSInternalServer;     
  FSock := TSsTCPBlockSocket.Create;
  FSock.BlockingRead := True;
{$ELSE}
  FSock := TTCPBlockSocket.Create;
{$ENDIF}
  FSock.ConvertLineEnd := True;
  FSock.SizeRecvBuffer := 32768;
  FSock.SizeSendBuffer := 32768;
  FTimeout := 60000;
  FTargetPort := cIMAPProtocol;
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
        s := FSock.RecvBufferStr(l, FTimeout);
        FFullResult.Add(s);
      end;
    end;
  until FSock.LastError <> 0;
  s := separateright(FResultString, ' ');
  Result:=uppercase(separateleft(s, ' '));
end;

procedure TIMAPSend.ProcessLiterals;
var
  l: TStringList;
  n, x: integer;
  b: integer;
  s: string;
begin
  l := TStringList.Create;
  try
    l.Assign(FFullResult);
    FFullResult.Clear;
    b := 0;
    for n := 0 to l.Count - 1 do
    begin
      s := l[n];
      if b > 0 then
      begin
        FFullResult[FFullresult.Count - 1] :=
          FFullResult[FFullresult.Count - 1] + s;
        inc(b);
        if b > 2 then
          b := 0;
      end
      else
      begin
        if (s <> '') and (s[Length(s)]='}') then
        begin
          x := RPos('{', s);
          Delete(s, x, Length(s) - x + 1);
          b := 1;
        end
        else
          b := 0;
        FFullResult.Add(s);
      end;
    end;
  finally
    l.Free;
  end;
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
  FSock.SendString('S' + IntToStr(FTagCommand) + ' ' + Value + ' {'+ IntToStr(l) + '}' + CRLF);
  FSock.RecvString(FTimeout);
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
  ProcessLiterals;
  Value.Clear;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := FFullResult[n];
    if (s <> '') and (Pos('\NOSELECT', UpperCase(s)) = 0) then
    begin
      if s[Length(s)] = '"' then
      begin
        Delete(s, Length(s), 1);
        x := RPos('"', s);
      end
      else
        x := RPos(' ', s);
      if (x > 0) then
        Value.Add(Copy(s, x + 1, Length(s) - x));
    end;
  end;
end;

procedure TIMAPSend.ParseSelect;
var
  n: integer;
  s, t: string;
begin
  ProcessLiterals;
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
  ProcessLiterals;
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
  Result := IMAPcommand('LOGIN "' + FUsername + '" "' + FPassword + '"') = 'OK';
end;

function TIMAPSend.Connect: Boolean;
begin
  FSock.CloseSocket;
  FSock.Bind(FIPInterface, cAnyPort);
{$IFDEF STREAMSEC}
  if FFullSSL then
  begin
    if assigned(FTLSServer) then
      FSock.TLSServer := FTLSServer
    else
    begin
      Result := false;
      exit;
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
    ProcessLiterals;
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
  begin
    ProcessLiterals;
    for n := 0 to FFullResult.Count - 1 do
    begin
      s := FFullResult[n];
//      s := UpperCase(FFullResult[n]);
      if (Pos('* ', s) = 1) and (Pos(FolderName, s) >= 1) and (Pos(Value, s) > 0 ) then
      begin
        t := SeparateRight(s, Value);
        t := SeparateLeft(t, ')');
        t := trim(t);
        Result := StrToIntDef(t, -1);
        Break;
      end;
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
  begin
    ProcessLiterals;
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

function TIMAPSend.AddFlagsMess(MessID: integer; Flags: string): Boolean;
var
  s: string;
begin
  s := 'STORE ' + IntToStr(MessID) + ' +FLAGS.SILENT (' + Flags + ')';
  if FUID then
    s := 'UID ' + s;
  Result := IMAPcommand(s) = 'OK';
end;

function TIMAPSend.DelFlagsMess(MessID: integer; Flags: string): Boolean;
var
  s: string;
begin
  s := 'STORE ' + IntToStr(MessID) + ' -FLAGS.SILENT (' + Flags + ')';
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
  ProcessLiterals;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := uppercase(FFullResult[n]);
    if (Pos('* ', s) = 1) and (Pos('FLAGS', s) > 0 ) then
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
{$IFDEF STREAMSEC}
      if not assigned(FTLSServer) then
        Exit;
      Fsock.TLSServer := FTLSServer;
      FSock.Connect('','');
{$ELSE}
      Fsock.SSLDoConnect;
{$ENDIF}
      Result := FSock.LastError = 0;
    end;
  end;
end;

//Paul Buskermolen <p.buskermolen@pinkroccade.com>
function TIMAPSend.GetUID(MessID: integer; var UID : Integer): boolean;
var
  s, sUid: string;
  n: integer;
begin
  sUID := '';
  s := 'FETCH ' + IntToStr(MessID) + ' UID';
  Result := IMAPcommand(s) = 'OK';
  ProcessLiterals;
  for n := 0 to FFullResult.Count - 1 do
  begin
    s := uppercase(FFullResult[n]);
    if Pos('FETCH (UID', s) >= 1 then
    begin
      s := Separateright(s, '(UID ');
      sUID := SeparateLeft(s, ')');
    end;
  end;
  UID := StrToIntDef(sUID, 0);
end;

{==============================================================================}

end.
