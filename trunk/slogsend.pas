{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
|==============================================================================|
| Content: SysLog client                                                       |
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
| Portions created by Lukas Gebauer are Copyright (c)2001.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

// RFC-3164

{$Q-}
{$WEAKPACKAGEUNIT ON}

unit SLogSend;

interface

uses
  SysUtils, Classes,
  blcksock, SynaUtil;

const
  cSysLogProtocol = '514';

  FCL_Kernel = 0;
  FCL_UserLevel = 1;
  FCL_MailSystem = 2;
  FCL_System = 3;
  FCL_Security = 4;
  FCL_Syslogd = 5;
  FCL_Printer = 6;
  FCL_News = 7;
  FCL_UUCP = 8;
  FCL_Clock = 9;
  FCL_Authorization = 10;
  FCL_FTP = 11;
  FCL_NTP = 12;
  FCL_LogAudit = 13;
  FCL_LogAlert = 14;
  FCL_Time = 15;
  FCL_Local0 = 16;
  FCL_Local1 = 17;
  FCL_Local2 = 18;
  FCL_Local3 = 19;
  FCL_Local4 = 20;
  FCL_Local5 = 21;
  FCL_Local6 = 22;
  FCL_Local7 = 23;

type
  TSyslogSeverity = (Emergency, Alert, Critical, Error, Warning, Notice, Info,
    Debug);

  TSyslogSend = class(TObject)
  private
    FSyslogHost: string;
    FSyslogPort: string;
    FSock: TUDPBlockSocket;
    FFacility: Byte;
    FSeverity: TSyslogSeverity;
    FTag: string;
    FMessage: string;
  public
    constructor Create;
    destructor Destroy; override;
    function DoIt: Boolean;
  published
    property SyslogHost: string read FSyslogHost Write FSyslogHost;
    property SyslogPort: string read FSyslogPort Write FSyslogPort;
    property Facility: Byte read FFacility Write FFacility;
    property Severity: TSyslogSeverity read FSeverity Write FSeverity;
    property Tag: string read FTag Write FTag;
    property LogMessage: string read FMessage Write FMessage;
  end;

function ToSysLog(const SyslogServer: string; Facil: Byte;
  Sever: TSyslogSeverity; const Content: string): Boolean;

implementation

constructor TSyslogSend.Create;
begin
  inherited Create;
  FSock := TUDPBlockSocket.Create;
  FSock.CreateSocket;
  FSyslogHost := cLocalhost;
  FSyslogPort := cSysLogProtocol;
  FFacility := FCL_Local0;
  FSeverity := Debug;
  FTag := ExtractFileName(ParamStr(0));
  FMessage := '';
end;

destructor TSyslogSend.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

function TSyslogSend.DoIt: Boolean;
var
  Buf: string;
  S: string;
  L: TStringList;
begin
  Result := False;
  Buf := '<' + IntToStr((FFacility * 8) + Ord(FSeverity)) + '>';
  Buf := Buf + CDateTime(now) + ' ';
  L := TStringList.Create;
  try
    FSock.ResolveNameToIP(FSock.Localname, L);
    if L.Count < 1 then
      S := '0.0.0.0'
    else
      S := L[0];
  finally
    L.Free;
  end;
  Buf := Buf + S + ' ';
  Buf := Buf + Tag + ': ' + FMessage;
  if Length(Buf) <= 1024 then
  begin
    FSock.Connect(FSyslogHost, FSyslogPort);
    FSock.SendString(Buf);
    Result := FSock.LastError = 0;
  end;
end;

{==============================================================================}

function ToSysLog(const SyslogServer: string; Facil: Byte;
  Sever: TSyslogSeverity; const Content: string): Boolean;
begin
  Result := False;
  with TSyslogSend.Create do
    try
      SyslogHost :=SyslogServer;
      Facility := Facil;
      Severity := Sever;
      LogMessage := Content;
      Result := DoIt;
    finally
      Free;
    end;
end;

end.
