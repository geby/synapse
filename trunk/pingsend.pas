{==============================================================================|
| Project : Delphree - Synapse                                   | 001.001.000 |
|==============================================================================|
| Content: PING sender                                                         |
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
|          (Found at URL: http://www.mlp.cz/space/gebauerl/synapse/)           |
|==============================================================================}

{
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Remember, this unit work only with Winsock2! (on Win98 and WinNT 4.0 or higher)
If you must use this unit on Win95, download Wínsock2 from Microsoft
and distribute it with your application!

In spite of I use Winsock level version 1.1, RAW sockets work in this level only
if Winsock2 is installed on your computer!!!

On WinNT standardly RAW sockets work if program is running under user with
administrators provilegies. To use RAW sockets under another users, you must
create the following registry variable and set its value to DWORD 1:

HKLM\System\CurrentControlSet\Services\Afd\Parameters\DisableRawSecurity

After you change the registry, you need to restart your computer!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}

unit PINGsend;

interface

uses
  winsock, SysUtils, windows, blcksck2, Synautil, dialogs;

const
  ICMP_ECHO=8;
  ICMP_ECHOREPLY=0;

type

TIcmpEchoHeader = Record
  i_type : Byte;
  i_code : Byte;
  i_checkSum : Word;
  i_Id : Word;
  i_seq : Word;
  TimeStamp : ULong;
End;

TPINGSend=class(TObject)
  private
    Sock:TICMPBlockSocket;
    Buffer:string;
    seq:integer;
    id:integer;
    function checksum:integer;
  public
    timeout:integer;
    PacketSize:integer;
    PingTime:integer;
    function ping(host:string):Boolean;
    constructor Create;
    destructor Destroy; override;
end;

function PingHost(host:string):integer;

implementation

{==============================================================================}

{TPINGSend.Create}
Constructor TPINGSend.Create;
begin
  inherited Create;
  sock:=TICMPBlockSocket.create;
  sock.CreateSocket;
  timeout:=5000;
  packetsize:=32;
  seq:=0;
end;

{TPINGSend.Destroy}
Destructor TPINGSend.Destroy;
begin
  Sock.free;
  inherited destroy;
end;

{TPINGSend.ping}
function TPINGSend.ping(host:string):Boolean;
var
  PIPHeader:^TIPHeader;
  IpHdrLen:Integer;
  PIcmpEchoHeader:^TICMPEchoHeader;
  data:string;
  n,x:integer;
begin
  Result:=False;
  sock.connect(host,'0');
  Buffer:=StringOfChar(#0,SizeOf(TICMPEchoHeader)+packetSize);
  PIcmpEchoHeader := Pointer(Buffer);
  With PIcmpEchoHeader^ Do Begin
    i_type:=ICMP_ECHO;
    i_code:=0;
    i_CheckSum:=0;
    id:=Random(32767);
    i_Id:=id;
    TimeStamp:=GetTickcount;
    Inc(Seq);
    i_Seq:=Seq;
    for n:=Succ(SizeOf(TicmpEchoHeader)) to Length(Buffer) do
      Buffer[n]:=#$55;
    i_CheckSum:=CheckSum;
  end;
  sock.sendString(Buffer);
  if sock.canread(timeout)
    then begin
      x:=sock.waitingdata;
      setlength(Buffer,x);
      sock.recvbuffer(Pointer(Buffer),x);
      PIpHeader:=Pointer(Buffer);
      IpHdrLen:=(PIpHeader^.VerLen and $0F)*4;
      PIcmpEchoHeader:=@Buffer[IpHdrLen+1];
      if (PIcmpEchoHeader^.i_type=ICMP_ECHOREPLY) then
        if (PIcmpEchoHeader^.i_id=id) then
          begin
            PingTime:=GetTickCount-PIcmpEchoHeader^.TimeStamp;
            Result:=True;
          end;
    end;
end;

{TPINGSend.checksum}
function TPINGSend.checksum:integer;
type
  tWordArray=Array[0..0] Of Word;
var
  PWordArray:^TWordArray;
  CkSum:Dword;
  Num,Remain:Integer;
  n:Integer;
begin
  Num:=length(Buffer) div 2;
  Remain:=length(Buffer) mod 2;
  PWordArray:=Pointer(Buffer);
  CkSum := 0;
  for n:=0 to Num-1 do
    CkSum:=CkSum+PWordArray^[n];
  if Remain<>0 then
    CkSum:=CkSum+ord(Buffer[Length(Buffer)]);
  CkSum:=(CkSum shr 16)+(CkSum and $FFFF);
  CkSum:=CkSum+(CkSum shr 16);
  Result:=Word(not CkSum);
end;

{==============================================================================}

function PingHost(host:string):integer;
var
  ping:TPINGSend;
begin
  ping:=TPINGSend.Create;
  try
    if ping.ping(host)
      then Result:=ping.pingtime
      else Result:=-1;
  finally
    ping.Free;
  end;
end;

end.
