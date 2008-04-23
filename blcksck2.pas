{==============================================================================|
| Project : Delphree - Synapse                                   | 001.001.000 |
|==============================================================================|
| Content: Library base for RAW sockets                                        |
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

unit blcksck2;

interface

uses
  winsock, SysUtils, windows, blcksock;

type

{TICMPBlockSocket}
TICMPBlockSocket = class (TBlockSocket)
public
  procedure CreateSocket; override;
end;

{TRAWBlockSocket}
TRAWBlockSocket = class (TBlockSocket)
public
  procedure CreateSocket; override;
end;

TIPHeader = Record
   VerLen : Byte;
   TOS : Byte;
   TotalLen : Word;
   Identifer : Word;
   FragOffsets : Word;
   TTL : Byte;
   Protocol : Byte;
   CheckSum : Word;
   SourceIp : Dword;
   DestIp : Dword;
   Options : Dword;
 End;

function SetTimeout(Sock:TSocket;Timeout:integer):Boolean;


implementation

{======================================================================}

{TICMPBlockSocket.CreateSocket}
Procedure TICMPBlockSocket.CreateSocket;
begin
  FSocket:=winsock.socket(PF_INET,SOCK_RAW,IPPROTO_ICMP);
  FProtocol:=IPPROTO_ICMP;
  inherited createSocket;
end;


{======================================================================}

{TRAWBlockSocket.CreateSocket}
Procedure TRAWBlockSocket.CreateSocket;
begin
  FSocket:=winsock.socket(PF_INET,SOCK_RAW,IPPROTO_RAW);
  FProtocol:=IPPROTO_RAW;
  inherited createSocket;
end;


{======================================================================}

function SetTimeout(Sock:TSocket;Timeout:integer):Boolean;
var
  len,Value,res:integer;
  r1,r2:Boolean;
begin
  Result:=False;
  r1:=False;
  r2:=False;
  Value:=Timeout;
  len:=SizeOf(Value);
  Res:=Winsock.setsockopt(sock,SOL_SOCKET,SO_RCVTIMEO,@Value,len);
  r1:=res<>SOCKET_ERROR;
  Res:=Winsock.setsockopt(sock,SOL_SOCKET,SO_SNDTIMEO,@Value,len);
  r2:=res<>SOCKET_ERROR;
  Result:=r1 and r2;
end;

end.
