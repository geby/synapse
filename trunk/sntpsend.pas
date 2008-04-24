{==============================================================================|
| Project : Delphree - Synapse                                   | 002.000.000 |
|==============================================================================|
| Content: SNTP client                                                         |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.0 |
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
| Portions created by Lukas Gebauer are Copyright (c)2000.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$Q-}

unit SNTPsend;

interface

uses
  synsock, SysUtils, blcksock;

type

  TNtp = packed record
    mode:Byte;
    stratum:Byte;
    poll:Byte;
    Precision:Byte;
    RootDelay : longint;
    RootDisperson : longint;
    RefID : longint;
    Ref1, Ref2,
    Org1, Org2,
    Rcv1, Rcv2,
    Xmit1, Xmit2 : longint;
  end;

TSNTPSend=class(TObject)
  private
    Sock:TUDPBlockSocket;
    Buffer:string;
  public
    timeout:integer;
    SntpHost:string;
    NTPReply:TNtp;
    NTPTime:TDateTime;
    constructor Create;
    destructor Destroy; override;
    function DecodeTs(nsec,nfrac:Longint):tdatetime;
    function GetNTP:Boolean;
    function GetBroadcastNTP:Boolean;
end;

implementation

{==============================================================================}

{TSNTPSend.Create}
Constructor TSNTPSend.Create;
begin
  inherited Create;
  sock:=TUDPBlockSocket.create;
  sock.CreateSocket;
  timeout:=5000;
  sntphost:='localhost';
end;

{TSNTPSend.Destroy}
Destructor TSNTPSend.Destroy;
begin
  Sock.free;
  inherited destroy;
end;

{TSNTPSend.DecodeTs}
function TSNTPSend.DecodeTs(nsec,nfrac:Longint):tdatetime;
const
  maxi = 4294967296.0;
var
  d, d1: double;
begin
  nsec:=synsock.htonl(nsec);
  nfrac:=synsock.htonl(nfrac);
  d:=nsec;
  if d<0
    then d:=maxi+d-1;
  d1 := nfrac;
  if d1<0
    then d1:=maxi+d1-1;
  d1:=d1/maxi;
  d1:=trunc(d1*1000)/1000;
  result:=(d+d1)/86400;
  result := Result + 2;
end;


{TSNTPSend.GetBroadcastNTP}
function TSNTPSend.GetBroadcastNTP:Boolean;
var
  PNtp:^TNtp;
  x:integer;
begin
  Result:=False;
  sock.bind('0.0.0.0','ntp');
  if sock.canread(timeout)
    then begin
      x:=sock.waitingdata;
      setlength(Buffer,x);
      sock.recvbufferFrom(Pointer(Buffer),x);
      if (sntphost='0.0.0.0') or (sock.GetRemoteSinIP=sntphost) then
        if x>=SizeOf(NTPReply) then
          begin
            PNtp:=Pointer(Buffer);
            NtpReply:=PNtp^;
            NTPtime:=DeCodeTs(ntpreply.Xmit1,ntpreply.Xmit2);
            Result:=True;
          end;
    end;
end;

{TSNTPSend.GetNTP}
function TSNTPSend.GetNTP:Boolean;
var
  q:Tntp;
  PNtp:^TNtp;
  x:integer;
begin
  Result:=False;
  sock.Connect(sntphost,'ntp');
  fillchar(q,SizeOf(q),0);
  q.mode:=$1b;
  sock.SendBuffer(@q,SizeOf(q));
  if sock.canread(timeout)
    then begin
      x:=sock.waitingdata;
      setlength(Buffer,x);
      sock.recvbuffer(Pointer(Buffer),x);
      if x>=SizeOf(NTPReply) then
        begin
          PNtp:=Pointer(Buffer);
          NtpReply:=PNtp^;
          NTPtime:=DeCodeTs(ntpreply.Xmit1,ntpreply.Xmit2);
          Result:=True;
        end;
    end;
end;


{==============================================================================}


end.
