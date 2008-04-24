{==============================================================================|
| Project : Delphree - Synapse                                   | 002.001.000 |
|==============================================================================|
| Content: Socket Independent Platform Layer                                   |
|==============================================================================|
| Copyright (c)1999-2002, Lukas Gebauer                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)2001.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{ Comment next line if you need dynamic loading of winsock under Windows
  or any another DLL stack by CreateAlternate constructor of TBlockSocket Class.
  if next line stay uncommented, is used static mapping. This is fater method.
  Under Linx is always used static maping to Libc. }
{$DEFINE STATICWINSOCK}

unit synsock;

interface

uses
{$IFDEF LINUX}
  Libc, KernelIoctl;
{$ELSE}
  Windows, WinSock;
{$ENDIF}

{$IFDEF LINUX}
const
  WSAEINTR = EINTR;
  WSAEBADF = EBADF;
  WSAEACCES = EACCES;
  WSAEFAULT = EFAULT;
  WSAEINVAL = EINVAL;
  WSAEMFILE = EMFILE;
  WSAEWOULDBLOCK = EWOULDBLOCK;
  WSAEINPROGRESS = EINPROGRESS;
  WSAEALREADY = EALREADY;
  WSAENOTSOCK = ENOTSOCK;
  WSAEDESTADDRREQ = EDESTADDRREQ;
  WSAEMSGSIZE = EMSGSIZE;
  WSAEPROTOTYPE = EPROTOTYPE;
  WSAENOPROTOOPT = ENOPROTOOPT;
  WSAEPROTONOSUPPORT = EPROTONOSUPPORT;
  WSAESOCKTNOSUPPORT = ESOCKTNOSUPPORT;
  WSAEOPNOTSUPP = EOPNOTSUPP;
  WSAEPFNOSUPPORT = EPFNOSUPPORT;
  WSAEAFNOSUPPORT = EAFNOSUPPORT;
  WSAEADDRINUSE = EADDRINUSE;
  WSAEADDRNOTAVAIL = EADDRNOTAVAIL;
  WSAENETDOWN = ENETDOWN;
  WSAENETUNREACH = ENETUNREACH;
  WSAENETRESET = ENETRESET;
  WSAECONNABORTED = ECONNABORTED;
  WSAECONNRESET = ECONNRESET;
  WSAENOBUFS = ENOBUFS;
  WSAEISCONN = EISCONN;
  WSAENOTCONN = ENOTCONN;
  WSAESHUTDOWN = ESHUTDOWN;
  WSAETOOMANYREFS = ETOOMANYREFS;
  WSAETIMEDOUT = ETIMEDOUT;
  WSAECONNREFUSED = ECONNREFUSED;
  WSAELOOP = ELOOP;
  WSAENAMETOOLONG = ENAMETOOLONG;
  WSAEHOSTDOWN = EHOSTDOWN;
  WSAEHOSTUNREACH = EHOSTUNREACH;
  WSAENOTEMPTY = ENOTEMPTY;
  WSAEPROCLIM = -1;
  WSAEUSERS = EUSERS;
  WSAEDQUOT = EDQUOT;
  WSAESTALE = ESTALE;
  WSAEREMOTE = EREMOTE;
  WSASYSNOTREADY = -2;
  WSAVERNOTSUPPORTED = -3;
  WSANOTINITIALISED = -4;
  WSAEDISCON = -5;
  WSAHOST_NOT_FOUND = HOST_NOT_FOUND;
  WSATRY_AGAIN = TRY_AGAIN;
  WSANO_RECOVERY = NO_RECOVERY;
//  WSANO_DATA         = NO_DATA;
  WSANO_DATA = -6;

{$ELSE}

const
  DLLStackName = 'wsock32.dll';
var
  LibHandle: THandle = 0;
{$ENDIF}

{$IFDEF LINUX}
const
  WSADESCRIPTION_LEN = 256;
  WSASYS_STATUS_LEN = 128;
type
  PWSAData = ^TWSAData;
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of Char;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of Char;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PChar;
  end;
  DWORD = Integer;
  TLinger = Linger;
{$ENDIF}

type
  TWSAStartup = function(wVersionRequired: Word;
    var WSData: TWSAData): Integer; stdcall;
  TWSACleanup = function: Integer; stdcall;
  TWSAGetLastError = function: Integer; stdcall;
  TGetServByName = function(name, proto: PChar): PServEnt; stdcall;
  TGetServByPort = function(port: Integer; proto: PChar): PServEnt; stdcall;
  TGetProtoByName = function(name: PChar): PProtoEnt; stdcall;
  TGetProtoByNumber = function(proto: Integer): PProtoEnt; stdcall;
  TGetHostByName = function(name: PChar): PHostEnt; stdcall;
  TGetHostByAddr = function(addr: Pointer; len, Struc: Integer): PHostEnt; stdcall;
  TGetHostName = function(name: PChar; len: Integer): Integer; stdcall;
  TShutdown = function(s: TSocket; how: Integer): Integer; stdcall;
  TSetSockOpt = function(s: TSocket; level, optname: Integer;
    optval: PChar; optlen: Integer): Integer; stdcall;
  TGetSockOpt = function(s: TSocket; level, optname: Integer;
    optval: PChar; var optlen: Integer): Integer; stdcall;
  TSendTo = function(s: TSocket; var Buf;
    len, flags: Integer; var addrto: TSockAddr;
    tolen: Integer): Integer; stdcall;
  TSend = function(s: TSocket; var Buf;
    len, flags: Integer): Integer; stdcall;
  TRecv = function(s: TSocket;
    var Buf; len, flags: Integer): Integer; stdcall;
  TRecvFrom = function(s: TSocket;
    var Buf; len, flags: Integer; var from: TSockAddr;
    var fromlen: Integer): Integer; stdcall;
  Tntohs = function(netshort: u_short): u_short; stdcall;
  Tntohl = function(netlong: u_long): u_long; stdcall;
  TListen = function(s: TSocket; backlog: Integer): Integer; stdcall;
  TIoctlSocket = function(s: TSocket; cmd: DWORD;
    var arg: u_long): Integer; stdcall;
  TInet_ntoa = function(inaddr: TInAddr): PChar; stdcall;
  TInet_addr = function(cp: PChar): u_long; stdcall;
  Thtons = function(hostshort: u_short): u_short; stdcall;
  Thtonl = function(hostlong: u_long): u_long; stdcall;
  TGetSockName = function(s: TSocket; var name: TSockAddr;
    var namelen: Integer): Integer; stdcall;
  TGetPeerName = function(s: TSocket; var name: TSockAddr;
    var namelen: Integer): Integer; stdcall;
  TConnect = function(s: TSocket; var name: TSockAddr;
    namelen: Integer): Integer; stdcall;
  TCloseSocket = function(s: TSocket): Integer; stdcall;
  TBind = function(s: TSocket; var addr: TSockAddr;
    namelen: Integer): Integer; stdcall;
  TAccept = function(s: TSocket; addr: PSockAddr;
    addrlen: PInteger): TSocket; stdcall;
  TSocketProc = function(af, Struc, Protocol: Integer): TSocket; stdcall;
  TSelect = function(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
    timeout: PTimeVal): Longint; stdcall;

var
  WSAStartup: TWSAStartup = nil;
  WSACleanup: TWSACleanup = nil;
  WSAGetLastError: TWSAGetLastError = nil;
  GetServByName: TGetServByName = nil;
  GetServByPort: TGetServByPort = nil;
  GetProtoByName: TGetProtoByName = nil;
  GetProtoByNumber: TGetProtoByNumber = nil;
  GetHostByName: TGetHostByName = nil;
  GetHostByAddr: TGetHostByAddr = nil;
  GetHostName: TGetHostName = nil;
  Shutdown: TShutdown = nil;
  SetSockOpt: TSetSockOpt = nil;
  GetSockOpt: TGetSockOpt = nil;
  SendTo: TSendTo = nil;
  Send: TSend = nil;
  Recv: TRecv = nil;
  RecvFrom: TRecvFrom = nil;
  ntohs: Tntohs = nil;
  ntohl: Tntohl = nil;
  Listen: TListen = nil;
  IoctlSocket: TIoctlSocket = nil;
  Inet_ntoa: TInet_ntoa = nil;
  Inet_addr: TInet_addr = nil;
  htons: Thtons = nil;
  htonl: Thtonl = nil;
  GetSockName: TGetSockName = nil;
  GetPeerName: TGetPeerName = nil;
  Connect: TConnect = nil;
  CloseSocket: TCloseSocket = nil;
  Bind: TBind = nil;
  Accept: TAccept = nil;
  Socket: TSocketProc = nil;
  Select: TSelect = nil;

function InitSocketInterface(stack: string): Boolean;
function DestroySocketInterface: Boolean;

{$IFDEF LINUX}
function LSWSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer; stdcall;
function LSWSACleanup: Integer; stdcall;
function LSWSAGetLastError: Integer; stdcall;
function LSGetServByName(name, proto: PChar): PServEnt; stdcall;
function LSGetServByPort(port: Integer; proto: PChar): PServEnt; stdcall;
function LSGetProtoByName(name: PChar): PProtoEnt; stdcall;
function LSGetProtoByNumber(proto: Integer): PProtoEnt; stdcall;
function LSGetHostByName(name: PChar): PHostEnt; stdcall;
function LSGetHostByAddr(addr: Pointer; len, Struc: Integer): PHostEnt; stdcall;
function LSGetHostName(name: PChar; len: Integer): Integer; stdcall;
function LSShutdown(s: TSocket; how: Integer): Integer; stdcall;
function LSSetSockOpt(s: TSocket; level, optname: Integer; optval: PChar;
  optlen: Integer): Integer; stdcall;
function LSGetSockOpt(s: TSocket; level, optname: Integer; optval: PChar;
  var optlen: Integer): Integer; stdcall;
function LSSendTo(s: TSocket; var Buf; len, flags: Integer;
  var addrto: TSockAddr; tolen: Integer): Integer; stdcall;
function LSSend(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall;
function LSRecv(s: TSocket; var Buf; len, flags: Integer): Integer; stdcall;
function LSRecvFrom(s: TSocket; var Buf; len, flags: Integer;
  var from: TSockAddr; var fromlen: Integer): Integer; stdcall;
function LSntohs(netshort: u_short): u_short; stdcall;
function LSntohl(netlong: u_long): u_long; stdcall;
function LSListen(s: TSocket; backlog: Integer): Integer; stdcall;
function LSIoctlSocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer; stdcall;
function LSInet_ntoa(inaddr: TInAddr): PChar; stdcall;
function LSInet_addr(cp: PChar): u_long; stdcall;
function LShtons(hostshort: u_short): u_short; stdcall;
function LShtonl(hostlong: u_long): u_long; stdcall;
function LSGetSockName(s: TSocket; var name: TSockAddr;
  var namelen: Integer): Integer; stdcall;
function LSGetPeerName(s: TSocket; var name: TSockAddr;
  var namelen: Integer): Integer; stdcall;
function LSConnect(s: TSocket; var name: TSockAddr; namelen: Integer): Integer; stdcall;
function LSCloseSocket(s: TSocket): Integer; stdcall;
function LSBind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer; stdcall;
function LSAccept(s: TSocket; addr: PSockAddr; addrlen: PInteger): TSocket; stdcall;
function LSSocketProc(af, Struc, Protocol: Integer): TSocket; stdcall;
function LSSelect(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
  timeout: PTimeVal): Longint; stdcall;
{$ENDIF}

implementation

{$IFNDEF LINUX}
{$IFNDEF STATICWINSOCK}
uses syncobjs;

var
  SynSockCS: TCriticalSection;
  SynSockCount: Integer = 0;
{$ENDIF}
{$ENDIF}

{$IFDEF LINUX}

function LSWSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
begin
  with WSData do
  begin
    wVersion := wVersionRequired;
    wHighVersion := $101;
    szDescription := 'Synapse Platform Independent Socket Layer';
    szSystemStatus := 'On Linux';
    iMaxSockets := 32768;
    iMaxUdpDg := 8192;
  end;
  Result := 0;
end;

function LSWSACleanup: Integer;
begin
  Result := 0;
end;

function LSWSAGetLastError: Integer;
begin
  Result := System.GetLastError;
end;

function LSGetServByName(name, proto: PChar): PServEnt;
begin
  Result := libc.GetServByName(name, proto);
end;

function LSGetServByPort(port: Integer; proto: PChar): PServEnt;
begin
  Result := libc.GetServByPort(port, proto);
end;

function LSGetProtoByName(name: PChar): PProtoEnt;
begin
  Result := libc.GetProtoByName(Name);
end;

function LSGetProtoByNumber(proto: Integer): PProtoEnt;
begin
  Result := libc.GetProtoByNumber(proto);
end;

function LSGetHostByName(name: PChar): PHostEnt;
begin
  Result := libc.GetHostByName(Name);
end;

function LSGetHostByAddr(addr: Pointer; len, Struc: Integer): PHostEnt;
begin
  Result := libc.GetHostByAddr(Addr, len, struc);
end;

function LSGetHostName(name: PChar; len: Integer): Integer;
begin
  Result := libc.GetHostName(Name, Len);
end;

function LSShutdown(s: TSocket; how: Integer): Integer;
begin
  Result := libc.Shutdown(S, How);
end;

function LSSetSockOpt(s: TSocket; level, optname: Integer; optval: PChar;
  optlen: Integer): Integer;
begin
  Result := libc.SetSockOpt(S, Level, OptName, OptVal, OptLen);
end;

function LSGetSockOpt(s: TSocket; level, optname: Integer; optval: PChar;
  var optlen: Integer): Integer;
begin
  Result := libc.getsockopt(s, level, optname, optval, cardinal(optlen));
end;

function LSSendTo(s: TSocket; var Buf; len, flags: Integer;
  var addrto: TSockAddr; tolen: Integer): Integer;
begin
  Result := libc.SendTo(S, Buf, Len, Flags, Addrto, Tolen);
end;

function LSSend(s: TSocket; var Buf; len, flags: Integer): Integer;
begin
  Result := libc.Send(S, Buf, Len, Flags);
end;

function LSRecv(s: TSocket; var Buf; len, flags: Integer): Integer;
begin
  Result := libc.Recv(S, Buf, Len, Flags);
end;

function LSRecvFrom(s: TSocket; var Buf; len, flags: Integer;
  var from: TSockAddr; var fromlen: Integer): Integer;
begin
  Result := libc.RecvFrom(S, Buf, Len, Flags, @from, @fromlen);
end;

function LSntohs(netshort: u_short): u_short;
begin
  Result := libc.NToHS(netshort);
end;

function LSntohl(netlong: u_long): u_long;
begin
  Result := libc.NToHL(netlong);
end;

function LSListen(s: TSocket; backlog: Integer): Integer;
begin
  Result := libc.Listen(S, Backlog);
end;

function LSIoctlSocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer;
begin
  Result := libc.ioctl(s, cmd, @arg);
end;

function LSInet_ntoa(inaddr: TInAddr): PChar;
begin
  Result := libc.inet_ntoa(inaddr);
end;

function LSInet_addr(cp: PChar): u_long;
begin
  Result := libc.inet_addr(cp);
end;

function LShtons(hostshort: u_short): u_short;
begin
  Result := libc.HToNs(HostShort);
end;

function LShtonl(hostlong: u_long): u_long;
begin
  Result := libc.HToNL(HostLong);
end;

function LSGetSockName(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer;
begin
  Result := libc.GetSockName(S, Name, cardinal(namelen));
end;

function LSGetPeerName(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer;
begin
  Result := libc.GetPeerName(S, Name, cardinal(namelen));
end;

function LSConnect(s: TSocket; var name: TSockAddr; namelen: Integer): Integer;
begin
  Result := libc.Connect(S, name, namelen);
end;

function LSCloseSocket(s: TSocket): Integer;
begin
  Result := libc.__close(s);
end;

function LSBind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer;
begin
  Result := libc.Bind(S, addr, namelen);
end;

function LSAccept(s: TSocket; addr: PSockAddr; addrlen: PInteger): TSocket;
begin
  Result := libc.Accept(S, addr, psocketlength(addrlen));
end;

function LSSocketProc(af, Struc, Protocol: Integer): TSocket;
begin
  Result := libc.Socket(Af, Struc, Protocol);
end;

function LSSelect(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
  timeout: PTimeVal): Longint;
begin
  Result := libc.Select(nfds, readfds, writefds, exceptfds, timeout);
end;

{$ENDIF}

function InitSocketInterface(stack: string): Boolean;
begin
{$IFDEF LINUX}
  Accept := LSAccept;
  Bind := LSBind;
  CloseSocket := LSCloseSocket;
  Connect := LSConnect;
  GetPeerName := LSGetPeerName;
  GetSockName := LSGetSockName;
  GetSockOpt := LSGetSockOpt;
  Htonl := LShtonl;
  Htons := LShtons;
  Inet_Addr := LSinet_addr;
  Inet_Ntoa := LSinet_ntoa;
  IoctlSocket := LSioctlsocket;
  Listen := LSlisten;
  Ntohl := LSntohl;
  Ntohs := LSntohs;
  Recv := LSrecv;
  RecvFrom := LSrecvfrom;
  Select := LSselect;
  Send := LSsend;
  SendTo := LSsendto;
  SetSockOpt := LSsetsockopt;
  ShutDown := LSshutdown;
  Socket := LSsocketProc;
  GetHostByAddr := LSGetHostByAddr;
  GetHostByName := LSGetHostByName;
  GetProtoByName := LSGetProtoByName;
  GetProtoByNumber := LSGetProtoByNumber;
  GetServByName := LSGetServByName;
  GetServByPort := LSGetServByPort;
  GetHostName := LSGetHostName;
  WSAGetLastError := LSWSAGetLastError;
  WSAStartup := LSWSAStartup;
  WSACleanup := LSWSACleanup;
  Result := True;
{$ELSE}
{$IFDEF STATICWINSOCK}
  Accept := Winsock.Accept;
  Bind := Winsock.Bind;
  CloseSocket := Winsock.CloseSocket;
  Connect := Winsock.Connect;
  GetPeerName := Winsock.GetPeerName;
  GetSockName := Winsock.GetSockName;
  GetSockOpt := Winsock.GetSockOpt;
  Htonl := Winsock.htonl;
  Htons := Winsock.htons;
  Inet_Addr := Winsock.inet_addr;
  Inet_Ntoa := Winsock.inet_ntoa;
  IoctlSocket := Winsock.ioctlsocket;
  Listen := Winsock.listen;
  Ntohl := Winsock.ntohl;
  Ntohs := Winsock.ntohs;
  Recv := Winsock.recv;
  RecvFrom := Winsock.recvfrom;
  Select := Winsock.select;
  Send := Winsock.send;
  SendTo := Winsock.sendto;
  SetSockOpt := Winsock.setsockopt;
  ShutDown := Winsock.shutdown;
  Socket := Winsock.socket;
  GetHostByAddr := Winsock.GetHostByAddr;
  GetHostByName := Winsock.GetHostByName;
  GetProtoByName := Winsock.GetProtoByName;
  GetProtoByNumber := Winsock.GetProtoByNumber;
  GetServByName := Winsock.GetServByName;
  GetServByPort := Winsock.GetServByPort;
  GetHostName := Winsock.GetHostName;
  WSAGetLastError := Winsock.WSAGetLastError;
  WSAStartup := Winsock.WSAStartup;
  WSACleanup := Winsock.WSACleanup;
  Result := True;
{$ELSE}
  Result := False;
  if stack = '' then
    stack := DLLStackName;
  SynSockCS.Enter;
  try
    if SynSockCount = 0 then
    begin
      LibHandle := Windows.LoadLibrary(PChar(Stack));
      if LibHandle <> 0 then
      begin
        Accept := Windows.GetProcAddress(LibHandle, PChar('accept'));
        Bind := Windows.GetProcAddress(LibHandle, PChar('bind'));
        CloseSocket := Windows.GetProcAddress(LibHandle, PChar('closesocket'));
        Connect := Windows.GetProcAddress(LibHandle, PChar('connect'));
        GetPeerName := Windows.GetProcAddress(LibHandle, PChar('getpeername'));
        GetSockName := Windows.GetProcAddress(LibHandle, PChar('getsockname'));
        GetSockOpt := Windows.GetProcAddress(LibHandle, PChar('getsockopt'));
        Htonl := Windows.GetProcAddress(LibHandle, PChar('htonl'));
        Htons := Windows.GetProcAddress(LibHandle, PChar('htons'));
        Inet_Addr := Windows.GetProcAddress(LibHandle, PChar('inet_addr'));
        Inet_Ntoa := Windows.GetProcAddress(LibHandle, PChar('inet_ntoa'));
        IoctlSocket := Windows.GetProcAddress(LibHandle, PChar('ioctlsocket'));
        Listen := Windows.GetProcAddress(LibHandle, PChar('listen'));
        Ntohl := Windows.GetProcAddress(LibHandle, PChar('ntohl'));
        Ntohs := Windows.GetProcAddress(LibHandle, PChar('ntohs'));
        Recv := Windows.GetProcAddress(LibHandle, PChar('recv'));
        RecvFrom := Windows.GetProcAddress(LibHandle, PChar('recvfrom'));
        Select := Windows.GetProcAddress(LibHandle, PChar('select'));
        Send := Windows.GetProcAddress(LibHandle, PChar('send'));
        SendTo := Windows.GetProcAddress(LibHandle, PChar('sendto'));
        SetSockOpt := Windows.GetProcAddress(LibHandle, PChar('setsockopt'));
        ShutDown := Windows.GetProcAddress(LibHandle, PChar('shutdown'));
        Socket := Windows.GetProcAddress(LibHandle, PChar('socket'));
        GetHostByAddr := Windows.GetProcAddress(LibHandle, PChar('gethostbyaddr'));
        GetHostByName := Windows.GetProcAddress(LibHandle, PChar('gethostbyname'));
        GetProtoByName := Windows.GetProcAddress(LibHandle, PChar('getprotobyname'));
        GetProtoByNumber := Windows.GetProcAddress(LibHandle, PChar('getprotobynumber'));
        GetServByName := Windows.GetProcAddress(LibHandle, PChar('getservbyname'));
        GetServByPort := Windows.GetProcAddress(LibHandle, PChar('getservbyport'));
        GetHostName := Windows.GetProcAddress(LibHandle, PChar('gethostname'));
        WSAGetLastError := Windows.GetProcAddress(LibHandle, PChar('WSAGetLastError'));
        WSAStartup := Windows.GetProcAddress(LibHandle, PChar('WSAStartup'));
        WSACleanup := Windows.GetProcAddress(LibHandle, PChar('WSACleanup'));
        Result := True;
      end;
    end
    else Result := True;
    if Result then
      Inc(SynSockCount);
  finally
    SynSockCS.Leave;
  end;
{$ENDIF}
{$ENDIF}
end;

function DestroySocketInterface: Boolean;
begin
{$IFDEF LINUX}
{$ELSE}
{$IFNDEF STATICWINSOCK}
  SynSockCS.Enter;
  try
    Dec(SynSockCount);
    if SynSockCount < 0 then
      SynSockCount := 0;
    if SynSockCount = 0 then
      if LibHandle <> 0 then
      begin
        Windows.FreeLibrary(libHandle);
        LibHandle := 0;
      end;
  finally
    SynSockCS.Leave;
  end;
{$ENDIF}
{$ENDIF}
  Result := True;
end;

{$IFNDEF LINUX}
{$IFNDEF STATICWINSOCK}
initialization
begin
  SynSockCS:= TCriticalSection.Create;
end;

finalization
begin
  SynSockCS.Free;
end;
{$ENDIF}
{$ENDIF}

end.
