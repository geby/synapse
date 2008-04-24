{==============================================================================|
| Project : Delphree - Synapse                                   | 002.000.001 |
|==============================================================================|
| Content: SNTP client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2000,2001.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$Q-}
{$WEAKPACKAGEUNIT ON}

unit SNTPsend;

interface

uses
  SysUtils,
  synsock, blcksock;

const
  cNtpProtocol = 'ntp';

type
  PNtp = ^TNtp;
  TNtp = packed record
    mode: Byte;
    stratum: Byte;
    poll: Byte;
    Precision: Byte;
    RootDelay: Longint;
    RootDisperson: Longint;
    RefID: Longint;
    Ref1: Longint;
    Ref2: Longint;
    Org1: Longint;
    Org2: Longint;
    Rcv1: Longint;
    Rcv2: Longint;
    Xmit1: Longint;
    Xmit2: Longint;
  end;

  TSNTPSend = class(TObject)
  private
    FNTPReply: TNtp;
    FNTPTime: TDateTime;
    FSntpHost: string;
    FTimeout: Integer;
    FSock: TUDPBlockSocket;
    FBuffer: string;
  public
    constructor Create;
    destructor Destroy; override;
    function DecodeTs(Nsec, Nfrac: Longint): TDateTime;
    function GetNTP: Boolean;
    function GetBroadcastNTP: Boolean;
  published
    property NTPReply: TNtp read FNTPReply;
    property NTPTime: TDateTime read FNTPTime;
    property SntpHost: string read FSntpHost write FSntpHost;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

implementation

constructor TSNTPSend.Create;
begin
  inherited Create;
  FSock := TUDPBlockSocket.Create;
  FSock.CreateSocket;
  FTimeout := 5000;
  FSntpHost := cLocalhost;
end;

destructor TSNTPSend.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

function TSNTPSend.DecodeTs(Nsec, Nfrac: Longint): TDateTime;
const
  maxi = 4294967296.0;
var
  d, d1: Double;
begin
  Nsec := synsock.htonl(Nsec);
  Nfrac := synsock.htonl(Nfrac);
  d := Nsec;
  if d < 0 then
    d := maxi + d - 1;
  d1 := Nfrac;
  if d1 < 0 then
    d1 := maxi + d1 - 1;
  d1 := d1 / maxi;
  d1 := Trunc(d1 * 1000) / 1000;
  Result := (d + d1) / 86400;
  Result := Result + 2;
end;

function TSNTPSend.GetBroadcastNTP: Boolean;
var
  NtpPtr: PNtp;
  x: Integer;
begin
  Result := False;
  FSock.Bind('0.0.0.0', cNtpProtocol);
  if FSock.CanRead(Timeout) then
  begin
    x := FSock.WaitingData;
    SetLength(FBuffer, x);
    FSock.RecvBufferFrom(Pointer(FBuffer), x);
    if (SntpHost = '0.0.0.0') or (FSock.GetRemoteSinIP = SntpHost) then
      if x >= SizeOf(NTPReply) then
      begin
        NtpPtr := Pointer(FBuffer);
        FNTPReply := NtpPtr^;
        FNTPTime := DecodeTs(NTPReply.Xmit1, NTPReply.Xmit2);
        Result := True;
      end;
  end;
end;

function TSNTPSend.GetNTP: Boolean;
var
  q: TNtp;
  NtpPtr: PNtp;
  x: Integer;
begin
  Result := False;
  FSock.Connect(sntphost, cNtpProtocol);
  FillChar(q, SizeOf(q), 0);
  q.mode := $1B;
  FSock.SendBuffer(@q, SizeOf(q));
  if FSock.CanRead(Timeout) then
  begin
    x := FSock.WaitingData;
    SetLength(FBuffer, x);
    FSock.RecvBuffer(Pointer(FBuffer), x);
    if x >= SizeOf(NTPReply) then
    begin
      NtpPtr := Pointer(FBuffer);
      FNTPReply := NtpPtr^;
      FNTPTime := DecodeTs(NTPReply.Xmit1, NTPReply.Xmit2);
      Result := True;
    end;
  end;
end;

end.
