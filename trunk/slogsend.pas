{==============================================================================|
| Project : Delphree - Synapse                                   | 001.001.004 |
|==============================================================================|
| Content: SysLog client                                                       |
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

// RFC-3164

{$Q-}

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

  TSyslogSend = class(TSynaClient)
  private
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
  FTargetPort := cSysLogProtocol;
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
    FSock.EnableReuse(True);
    Fsock.Bind(FIPInterface, FTargetPort);
    if FSock.LastError <> 0 then
      FSock.Bind(FIPInterface, cAnyPort);
    FSock.Connect(FTargetHost, FTargetPort);
    FSock.SendString(Buf);
    Result := FSock.LastError = 0;
  end;
end;

{==============================================================================}

function ToSysLog(const SyslogServer: string; Facil: Byte;
  Sever: TSyslogSeverity; const Content: string): Boolean;
begin
  with TSyslogSend.Create do
    try
      TargetHost :=SyslogServer;
      Facility := Facil;
      Severity := Sever;
      LogMessage := Content;
      Result := DoIt;
    finally
      Free;
    end;
end;

end.
