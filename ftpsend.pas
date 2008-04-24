{==============================================================================|
| Project : Delphree - Synapse                                   | 001.002.000 |
|==============================================================================|
| Content: FTP client                                                          |
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
|   Petr Esner <petr.esner@atlas.cz>                                           |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$WEAKPACKAGEUNIT ON}

unit FTPsend;

interface

uses
  SysUtils, Classes,
  blcksock, SynaUtil, SynaCode;

const
  cFtpProtocol = 'ftp';
  cFtpDataProtocol = 'ftp-data';

  FTP_OK = 255;
  FTP_ERR = 254;

type
  TLogonActions = array [0..17] of byte;

  TFTPStatus = procedure(Sender: TObject; Response: Boolean;
    const Value: string) of object;

  TFTPSend = class(TObject)
  private
    FOnStatus: TFTPStatus;
    FSock: TTCPBlockSocket;
    FDSock: TTCPBlockSocket;
    FTimeout: Integer;
    FFTPHost: string;
    FFTPPort: string;
    FResultCode: Integer;
    FResultString: string;
    FFullResult: TStringList;
    FUsername: string;
    FPassword: string;
    FAccount: string;
    FFWHost: string;
    FFWPort: string;
    FFWUsername: string;
    FFWPassword: string;
    FFWMode: integer;
    FDataStream: TMemoryStream;
    FDataIP: string;
    FDataPort: string;
    FDirectFile: Boolean;
    FDirectFileName: string;
    FCanResume: Boolean;
    FPassiveMode: Boolean;
    FForceDefaultPort: Boolean;
    function Auth(Mode: integer): Boolean;
    function Connect: Boolean;
    function InternalStor(const Command: string; RestoreAt: integer): Boolean;
    function DataSocket: Boolean;
    function AcceptDataSocket: Boolean;
    function DataRead(const DestStream: TStream): Boolean;
    function DataWrite(const SourceStream: TStream): Boolean;
  protected
    procedure DoStatus(Response: Boolean; const Value: string);
  public
    CustomLogon: TLogonActions;
    constructor Create;
    destructor Destroy; override;
    function ReadResult: Integer;
    procedure ParseRemote(Value: string);
    function FTPCommand(const Value: string): integer;
    function Login: Boolean;
    procedure Logout;
    function List(Directory: string; NameList: Boolean): Boolean;
    function RetriveFile(const FileName: string; Restore: Boolean): Boolean;
    function StoreFile(const FileName: string; Restore: Boolean): Boolean;
    function StoreUniqueFile: Boolean;
    function AppendFile(const FileName: string): Boolean;
    function RenameFile(const OldName, NewName: string): Boolean;
    function DeleteFile(const FileName: string): Boolean;
    function FileSize(const FileName: string): integer;
    function NoOp: Boolean;
    function ChangeWorkingDir(const Directory: string): Boolean;
    function ChangeToRootDir: Boolean;
    function DeleteDir(const Directory: string): Boolean;
    function CreateDir(const Directory: string): Boolean;
    function GetCurrentDir: String;
  published
    property Timeout: Integer read FTimeout Write FTimeout;
    property FTPHost: string read FFTPHost Write FFTPHost;
    property FTPPort: string read FFTPPort Write FFTPPort;
    property ResultCode: Integer read FResultCode;
    property ResultString: string read FResultString;
    property FullResult: TStringList read FFullResult;
    property Username: string read FUsername Write FUsername;
    property Password: string read FPassword Write FPassword;
    property Account: string read FAccount Write FAccount;
    property FWHost: string read FFWHost Write FFWHost;
    property FWPort: string read FFWPort Write FFWPort;
    property FWUsername: string read FFWUsername Write FFWUsername;
    property FWPassword: string read FFWPassword Write FFWPassword;
    property FWMode: integer read FFWMode Write FFWMode;
    property Sock: TTCPBlockSocket read FSock;
    property DSock: TTCPBlockSocket read FDSock;
    property DataStream: TMemoryStream read FDataStream;
    property DataIP: string read FDataIP;
    property DataPort: string read FDataPort;
    property DirectFile: Boolean read FDirectFile Write FDirectFile;
    property DirectFileName: string read FDirectFileName Write FDirectFileName;
    property CanResume: Boolean read FCanResume;
    property PassiveMode: Boolean read FPassiveMode Write FPassiveMode;
    property ForceDefaultPort: Boolean read FForceDefaultPort Write FForceDefaultPort;
    property OnStatus: TFTPStatus read FOnStatus write FOnStatus;
  end;

function FtpGetFile(const IP, Port, FileName, LocalFile,
  User, Pass: string): Boolean;
function FtpPutFile(const IP, Port, FileName, LocalFile,
  User, Pass: string): Boolean;
function FtpInterServerTransfer(
  const FromIP, FromPort, FromFile, FromUser, FromPass: string;
  const ToIP, ToPort, ToFile, ToUser, ToPass: string): Boolean;

implementation

const
  CRLF = #13#10;

constructor TFTPSend.Create;
begin
  inherited Create;
  FFullResult := TStringList.Create;
  FDataStream := TMemoryStream.Create;
  FSock := TTCPBlockSocket.Create;
  FDSock := TTCPBlockSocket.Create;
  FTimeout := 300000;
  FFTPHost := cLocalhost;
  FFTPPort := cFtpProtocol;
  FUsername := 'anonymous';
  FPassword := 'anonymous@' + FSock.LocalName;
  FDirectFile := False;
  FPassiveMode := True;
  FForceDefaultPort := False;
  FAccount := '';
  FFWHost := '';
  FFWPort := cFtpProtocol;
  FFWUsername := '';
  FFWPassword := '';
  FFWMode := 0;
end;

destructor TFTPSend.Destroy;
begin
  FDSock.Free;
  FSock.Free;
  FDataStream.Free;
  FFullResult.Free;
  inherited Destroy;
end;

procedure TFTPSend.DoStatus(Response: Boolean; const Value: string);
begin
  if assigned(OnStatus) then
    OnStatus(Self, Response, Value);
end;

function TFTPSend.ReadResult: Integer;
var
  s,c: string;
begin
  Result := 0;
  FFullResult.Clear;
  c := '';
  repeat
    s := FSock.RecvString(FTimeout);
    if c = '' then
      c :=Copy(s, 1, 3)+' ';
    FResultString := s;
    FFullResult.Add(s);
    if FSock.LastError <> 0 then
      Break;
  until Pos(c, s) = 1;
  s := FFullResult[0];
  if Length(s) >= 3 then
    Result := StrToIntDef(Copy(s, 1, 3), 0);
  FResultCode := Result;
end;

function TFTPSend.FTPCommand(const Value: string): integer;
begin
  FSock.SendString(Value + CRLF);
  DoStatus(False, Value);
  Result := ReadResult;
  DoStatus(True, FResultString);
end;

// based on idea by Petr Esner <petr.esner@atlas.cz>
function TFTPSend.Auth(Mode: integer): Boolean;
const
  // Direct connection USER[+PASS[+ACCT]]
  Action0: TLogonActions =
    (0, FTP_OK, 3, 1, FTP_OK, 6, 2, FTP_OK, FTP_ERR, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  // SITE <hostname>
  Action1: TLogonActions =
    (3, 6, 3, 4, 6, FTP_ERR, 5, FTP_ERR, 9, 0, FTP_OK, 12, 1, FTP_OK, 15, 2,
    FTP_OK, FTP_ERR);
  // USER after logon
  Action2: TLogonActions =
    (3, 6, 3, 4, 6, FTP_ERR, 6, FTP_OK, 9, 1, FTP_OK, 12, 2, FTP_OK, FTP_ERR,
     0, 0, 0);
  // Transparent
  Action3: TLogonActions =
    (3, 6, 3, 4, 6, FTP_ERR, 0, FTP_OK, 9, 1, FTP_OK, 12, 2, FTP_OK, FTP_ERR,
     0, 0, 0);
  // proxy OPEN
  Action4: TLogonActions =
    (7, 3, 3, 0, FTP_OK, 6, 1, FTP_OK, 9, 2, FTP_OK, FTP_ERR,
     0, 0, 0, 0, 0, 0);
  // USER with no logon
  Action5: TLogonActions =
    (6, FTP_OK, 3, 1, FTP_OK, 6, 2, FTP_OK, FTP_ERR, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  // USER fireID@remotehost
  Action6: TLogonActions =
    (8, 6, 3, 4, 6, FTP_ERR, 0, FTP_OK, 9, 1, FTP_OK, 12, 2, FTP_OK, FTP_ERR,
     0, 0, 0);
  // USER remoteID@remotehost fireID
  Action7: TLogonActions =
    (9, FTP_ERR, 3, 1, FTP_OK, 6, 2, FTP_OK, FTP_ERR, 0, 0, 0, 0, 0, 0, 0, 0, 0);
  // USER remoteID@fireID@remotehost
  Action8: TLogonActions =
    (10, FTP_OK, 3, 11, FTP_OK, 6, 2, FTP_OK, FTP_ERR, 0, 0, 0, 0, 0, 0, 0, 0, 0);
var
  FTPServer: string;
  LogonActions: TLogonActions;
  i: integer;
  s: string;
  x: integer;
begin
  Result := False;
  if FFWHost = '' then
    Mode := 0;
  if (FFTPPort = cFtpProtocol) or (FFTPPort = '21') then
    FTPServer := FFTPHost
  else
    FTPServer := FFTPHost + ':' + FFTPPort;
  case Mode of
    -1:
      LogonActions := CustomLogon;
    1:
      LogonActions := Action1;
    2:
      LogonActions := Action2;
    3:
      LogonActions := Action3;
    4:
      LogonActions := Action4;
    5:
      LogonActions := Action5;
    6:
      LogonActions := Action6;
    7:
      LogonActions := Action7;
    8:
      LogonActions := Action8;
  else
    LogonActions := Action0;
  end;
  i := 0;
  repeat
    case LogonActions[i] of
      0:  s := 'USER ' + FUserName;
      1:  s := 'PASS ' + FPassword;
      2:  s := 'ACCT ' + FAccount;
      3:  s := 'USER ' + FFWUserName;
      4:  s := 'PASS ' + FFWPassword;
      5:  s := 'SITE ' + FTPServer;
      6:  s := 'USER ' + FUserName + '@' + FTPServer;
      7:  s := 'OPEN ' + FTPServer;
      8:  s := 'USER ' + FFWUserName + '@' + FTPServer;
      9:  s := 'USER ' + FUserName + '@' + FTPServer + ' ' + FFWUserName;
      10: s := 'USER ' + FUserName + '@' + FFWUserName + '@' + FTPServer;
      11: s := 'PASS ' + FPassword + '@' + FFWPassword;
    end;
    x := FTPCommand(s);
    x := x div 100;
    if (x <> 2) and (x <> 3) then
      Exit;
    i := LogonActions[i + x - 1];
    case i of
      FTP_ERR:
        Exit;
      FTP_OK:
        begin
          Result := True;
          Exit;
        end;
    end;
  until False;
end;


function TFTPSend.Connect: Boolean;
begin
  FSock.CloseSocket;
  FSock.CreateSocket;
  if FFWHost = '' then
    FSock.Connect(FFTPHost, FFTPPort)
  else
    FSock.Connect(FFWHost, FFWPort);
  Result := FSock.LastError = 0;
end;

function TFTPSend.Login: Boolean;
begin
  Result := False;
  FCanResume := False;
  if not Connect then
    Exit;
  if ReadResult <> 220 then
    Exit;
  if not Auth(FFWMode) then
    Exit;
  FTPCommand('TYPE I');
  FTPCommand('STRU F');
  FTPCommand('MODE S');
  if FTPCommand('REST 1') = 350 then
  begin
    FTPCommand('REST 0');
    FCanResume := True;
  end;
  Result := True;
end;

procedure TFTPSend.Logout;
begin
  FTPCommand('QUIT');
  FSock.CloseSocket;
end;

procedure TFTPSend.ParseRemote(Value: string);
var
  n: integer;
  nb, ne: integer;
  s: string;
  x: integer;
begin
  Value := trim(Value);
  nb := Pos('(',Value);
  ne := Pos(')',Value);
  if (nb = 0) or (ne = 0) then
  begin
    nb:=RPos(' ',Value);
    s:=Copy(Value, nb + 1, Length(Value) - nb);
  end
  else
  begin
    s:=Copy(Value,nb+1,ne-nb-1);
  end;
  for n := 1 to 4 do
    if n = 1 then
      FDataIP := Fetch(s, ',')
    else
      FDataIP := FDataIP + '.' + Fetch(s, ',');
  x := StrToIntDef(Fetch(s, ','), 0) * 256;
  x := x + StrToIntDef(Fetch(s, ','), 0);
  FDataPort := IntToStr(x);
end;

function TFTPSend.DataSocket: boolean;
var
  s: string;
begin
  Result := False;
  if FPassiveMode then
  begin
    if FTPCommand('PASV') <> 227 then
      Exit;
    ParseRemote(FResultString);
    FDSock.CloseSocket;
    FDSock.CreateSocket;
    FDSock.Connect(FDataIP, FDataPort);
    Result := FDSock.LastError = 0;
  end
  else
  begin
    FDSock.CloseSocket;
    FDSock.CreateSocket;
    if FForceDefaultPort then
      s := cFtpDataProtocol
    else
      s := '0';
    FDSock.Bind(FDSock.LocalName, s);
    if FDSock.LastError <> 0 then
      Exit;
    FDSock.Listen;
    FDSock.GetSins;
    FDataIP := FDSock.GetLocalSinIP;
    FDataIP := FDSock.ResolveName(FDataIP);
    FDataPort := IntToStr(FDSock.GetLocalSinPort);
    s := StringReplace(FDataIP, '.', ',');
    s := 'PORT ' + s + ',' + IntToStr(FDSock.GetLocalSinPort div 256)
      + ',' + IntToStr(FDSock.GetLocalSinPort mod 256);
    Result := FTPCommand(s) = 200;
  end;
end;

function TFTPSend.AcceptDataSocket: Boolean;
var
  x: integer;
begin
  if FPassiveMode then
    Result := True
  else
  begin
    Result := False;
    if FDSock.CanRead(FTimeout) then
    begin
      x := FDSock.Accept;
      if not FDSock.UsingSocks then
        FDSock.CloseSocket;
      FDSock.Socket := x;
      Result := True;
    end;
  end;
end;

function TFTPSend.DataRead(const DestStream: TStream): Boolean;
var
  x: integer;
  buf: string;
begin
  Result := False;
  try
    if not AcceptDataSocket then
      Exit;
    repeat
      buf := FDSock.RecvPacket(FTimeout);
      if FDSock.LastError = 0 then
        DestStream.Write(Pointer(buf)^, Length(buf));
    until FDSock.LastError <> 0;
    x := ReadResult;
    if (x = 226) or (x = 250) then
      Result := True;
  finally
    FDSock.CloseSocket;
  end;
end;

function TFTPSend.DataWrite(const SourceStream: TStream): Boolean;
const
  BufSize = 8192;
var
  Bytes: integer;
  bc, lb: integer;
  n, x: integer;
  Buf: string;
begin
  Result := False;
  try
    if not AcceptDataSocket then
      Exit;
    Bytes := SourceStream.Size - SourceStream.Position;
    bc := Bytes div BufSize;
    lb := Bytes mod BufSize;
    SetLength(Buf, BufSize);
    for n := 1 to bc do
    begin
      SourceStream.read(Pointer(buf)^, BufSize);
      FDSock.SendBuffer(Pchar(buf), BufSize);
      if FDSock.LastError <> 0 then
        Exit;
    end;
    SetLength(Buf, lb);
    SourceStream.read(Pointer(buf)^, lb);
    FDSock.SendBuffer(Pchar(buf), lb);
    if FDSock.LastError <> 0 then
      Exit;
    FDSock.CloseSocket;
    x := ReadResult;
    if (x = 226) or (x = 250) then
      Result := True;
  finally
    FDSock.CloseSocket;
  end;
end;

function TFTPSend.List(Directory: string; NameList: Boolean): Boolean;
begin
  Result := False;
  FDataStream.Clear;
  if Directory <> '' then
    Directory := ' ' + Directory;
  if not DataSocket then
    Exit;
  FTPCommand('TYPE A');
  if NameList then
    FTPCommand('NLST' + Directory)
  else
    FTPCommand('LIST' + Directory);
  Result := DataRead(FDataStream);
  FDataStream.Seek(0, soFromBeginning);
end;

function TFTPSend.RetriveFile(const FileName: string; Restore: Boolean): Boolean;
var
  RetrStream: TStream;
begin
  Result := False;
  if FileName = '' then
    Exit;
  Restore := Restore and FCanResume;
  if FDirectFile then
    if Restore and FileExists(FDirectFileName) then
      RetrStream := TFileStream.Create(FDirectFileName,
        fmOpenReadWrite	 or fmShareExclusive)
    else
      RetrStream := TFileStream.Create(FDirectFileName,
        fmCreate or fmShareDenyWrite)
  else
    RetrStream := FDataStream;
  try
    if not DataSocket then
      Exit;
    FTPCommand('TYPE I');
    if Restore then
    begin
      RetrStream.Seek(0, soFromEnd);
      if FTPCommand('REST ' + IntToStr(RetrStream.Size)) <> 350 then
        Exit;
    end
    else
      if RetrStream is TMemoryStream then
        TMemoryStream(RetrStream).Clear;
    if (FTPCommand('RETR ' + FileName) div 100) <> 1 then
      Exit;
    Result := DataRead(RetrStream);
    if not FDirectFile then
      RetrStream.Seek(0, soFromBeginning);
  finally
    if FDirectFile then
      RetrStream.Free;
  end;
end;

function TFTPSend.InternalStor(const Command: string; RestoreAt: integer): Boolean;
var
  SendStream: TStream;
  StorSize: integer;
begin
  Result := False;
  if FDirectFile then
    if not FileExists(FDirectFileName) then
      Exit
    else
      SendStream := TFileStream.Create(FDirectFileName,
        fmOpenRead or fmShareDenyWrite)
  else
    SendStream := FDataStream;
  try
    if not DataSocket then
      Exit;
    FTPCommand('TYPE I');
    StorSize := SendStream.Size;
    if not FCanResume then
      RestoreAt := 0;
    if RestoreAt = StorSize then
    begin
      Result := True;
      Exit;
    end;
    if RestoreAt > StorSize then
      RestoreAt := 0;
    FTPCommand('ALLO ' + IntToStr(StorSize - RestoreAt));
    if FCanResume then
      if FTPCommand('REST ' + IntToStr(RestoreAt)) <> 350 then
        Exit;
    SendStream.Seek(RestoreAt, soFromBeginning);
    if (FTPCommand(Command) div 100) <> 1 then
      Exit;
    Result := DataWrite(SendStream);
  finally
    if FDirectFile then
      SendStream.Free;
  end;
end;

function TFTPSend.StoreFile(const FileName: string; Restore: Boolean): Boolean;
var
  RestoreAt: integer;
begin
  Result := False;
  if FileName = '' then
    Exit;
  RestoreAt := 0;
  Restore := Restore and FCanResume;
  if Restore then
  begin
    RestoreAt := Self.FileSize(FileName);
    if RestoreAt < 0 then
      RestoreAt := 0;
  end;
  Result := InternalStor('STOR ' + FileName, RestoreAt);
end;

function TFTPSend.StoreUniqueFile: Boolean;
begin
  Result := InternalStor('STOU', 0);
end;

function TFTPSend.AppendFile(const FileName: string): Boolean;
begin
  Result := False;
  if FileName = '' then
    Exit;
  Result := InternalStor('APPE '+FileName, 0);
end;

function TFTPSend.NoOp: Boolean;
begin
  Result := FTPCommand('NOOP') = 250;
end;

function TFTPSend.RenameFile(const OldName, NewName: string): Boolean;
begin
  Result := False;
  if FTPCommand('RNFR ' + OldName) <> 350 then
    Exit;
  Result := FTPCommand('RNTO ' + NewName) = 250;
end;

function TFTPSend.DeleteFile(const FileName: string): Boolean;
begin
  Result := FTPCommand('DELE ' + FileName) = 250;
end;

function TFTPSend.FileSize(const FileName: string): integer;
var
  s: string;
begin
  Result := -1;
  if FTPCommand('SIZE ' + FileName) = 213 then
  begin
    s := SeparateRight(ResultString, ' ');
    s := SeparateLeft(s, ' ');
    Result := StrToIntDef(s, -1);
  end;
end;

function TFTPSend.ChangeWorkingDir(const Directory: string): Boolean;
begin
  Result := FTPCommand('CWD ' + Directory) = 250;
end;

function TFTPSend.ChangeToRootDir: Boolean;
begin
  Result := FTPCommand('CDUP') = 200;
end;

function TFTPSend.DeleteDir(const Directory: string): Boolean;
begin
  Result := FTPCommand('RMD ' + Directory) = 250;
end;

function TFTPSend.CreateDir(const Directory: string): Boolean;
begin
  Result := FTPCommand('MKD ' + Directory) = 257;
end;

function TFTPSend.GetCurrentDir: String;
begin
  Result := '';
  if FTPCommand('PWD') = 257 then
  begin
    Result := SeparateRight(FResultString, '"');
    Result := Separateleft(Result, '"');
  end;
end;

{==============================================================================}

function FtpGetFile(const IP, Port, FileName, LocalFile,
  User, Pass: string): Boolean;
begin
  Result := False;
  with TFTPSend.Create do
  try
    if User <> '' then
    begin
      Username := User;
      Password := Pass;
    end;
    FTPHost := IP;
    FTPPort := Port;
    if not Login then
      Exit;
    DirectFileName := LocalFile;
    DirectFile:=True;
    Result := RetriveFile(FileName, False);
    Logout;
  finally
    Free;
  end;
end;

function FtpPutFile(const IP, Port, FileName, LocalFile,
  User, Pass: string): Boolean;
begin
  Result := False;
  with TFTPSend.Create do
  try
    if User <> '' then
    begin
      Username := User;
      Password := Pass;
    end;
    FTPHost := IP;
    FTPPort := Port;
    if not Login then
      Exit;
    DirectFileName := LocalFile;
    DirectFile:=True;
    Result := StoreFile(FileName, False);
    Logout;
  finally
    Free;
  end;
end;

function FtpInterServerTransfer(
  const FromIP, FromPort, FromFile, FromUser, FromPass: string;
  const ToIP, ToPort, ToFile, ToUser, ToPass: string): Boolean;
var
  FromFTP, ToFTP: TFTPSend;
  s: string;
  x: integer;
begin
  Result := False;
  FromFTP := TFTPSend.Create;
  toFTP := TFTPSend.Create;
  try
    if FromUser <> '' then
    begin
      FromFTP.Username := FromUser;
      FromFTP.Password := FromPass;
    end;
    if ToUser <> '' then
    begin
      ToFTP.Username := ToUser;
      ToFTP.Password := ToPass;
    end;
    FromFTP.FTPHost := FromIP;
    FromFTP.FTPPort := FromPort;
    ToFTP.FTPHost := ToIP;
    ToFTP.FTPPort := ToPort;
    if not FromFTP.Login then
      Exit;
    if not ToFTP.Login then
      Exit;
    if FromFTP.FTPCommand('PASV') <> 227 then
      Exit;
    FromFTP.ParseRemote(FromFTP.ResultString);
    s := StringReplace(FromFTP.DataIP, '.', ',');
    s := 'PORT ' + s + ',' + IntToStr(StrToIntDef(FromFTP.DataPort, 0) div 256)
      + ',' + IntToStr(StrToIntDef(FromFTP.DataPort, 0) mod 256);
    if ToFTP.FTPCommand(s) <> 200 then
      Exit;
    x := FromFTP.FTPCommand('STOR ' + FromFile);
    if (x <> 125) and (x <> 150) then
      Exit;
    x := ToFTP.FTPCommand('RETR ' + ToFile);
    if (x <> 125) and (x <> 150) then
      Exit;
    FromFTP.Timeout := 21600000;
    x := FromFTP.ReadResult;
    if (x <> 226) and (x <> 250) then
      Exit;
    ToFTP.Timeout := 21600000;
    x := ToFTP.ReadResult;
    if (x <> 226) and (x <> 250) then
      Exit;
    Result := True;
  finally
    ToFTP.Free;
    FromFTP.Free;
  end;
end;

end.
