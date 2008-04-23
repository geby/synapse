{==============================================================================|
| Project : Delphree - Synapse                                   | 002.000.000 |
|==============================================================================|
| Content: Library base                                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)1999,2000.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.mlp.cz/space/gebauerl/synapse/)           |
|==============================================================================}

unit blcksock;

interface

uses
  winsock, SysUtils, windows;

type

ESynapseError = class (Exception)
Public
  ErrorCode:integer;
  ErrorMessage:string;
end;

{TBlockSocket}
TBlockSocket = class (TObject)
Protected
  FSocket:TSocket;
  FLocalSin:TSockAddrIn;
  FRemoteSin:TSockAddrIn;
  FLastError:integer;
  FProtocol:integer;
  FBuffer:string;
  FRaiseExcept:boolean;

  procedure SetSin (var sin:TSockAddrIn;ip,port:string);
  function GetSinIP (sin:TSockAddrIn):string;
  function GetSinPort (sin:TSockAddrIn):integer;
public
  FWsaData : TWSADATA;

  constructor Create;
  destructor Destroy; override;

  Procedure CreateSocket; virtual;
  Procedure CloseSocket;
  procedure Bind(ip,port:string);
  procedure Connect(ip,port:string);
  function SendBuffer(buffer:pointer;length:integer):integer; virtual;
  procedure SendByte(data:byte); virtual;
  procedure SendString(data:string); virtual;
  function RecvBuffer(buffer:pointer;length:integer):integer; virtual;
  function RecvByte(timeout:integer):byte; virtual;
  function Recvstring(timeout:integer):string; virtual;
  function PeekBuffer(buffer:pointer;length:integer):integer; virtual;
  function PeekByte(timeout:integer):byte; virtual;
  function WaitingData:integer;
  procedure SetLinger(enable:boolean;Linger:integer);
  procedure GetSins;
  function SockCheck(SockResult:integer):integer;
  procedure ExceptCheck;
  function LocalName:string;
  function GetLocalSinIP:string;
  function GetRemoteSinIP:string;
  function GetLocalSinPort:integer;
  function GetRemoteSinPort:integer;
  function CanRead(Timeout:integer):boolean;
  function CanWrite(Timeout:integer):boolean;
  function SendBufferTo(buffer:pointer;length:integer):integer;
  function RecvBufferFrom(buffer:pointer;length:integer):integer;

published
  property socket:TSocket read FSocket write FSocket;
  property LocalSin:TSockAddrIn read FLocalSin;
  property RemoteSin:TSockAddrIn read FRemoteSin;
  property LastError:integer read FLastError;
  property Protocol:integer read FProtocol;
  property RaiseExcept:boolean read FRaiseExcept write FRaiseExcept;
end;

{TUDPBlockSocket}
TUDPBlockSocket = class (TBlockSocket)
public
  procedure CreateSocket; override;
  function EnableBroadcast(Value:Boolean):Boolean;
end;

{TTCPBlockSocket}
TTCPBlockSocket = class (TBlockSocket)
public
  procedure CreateSocket; override;
  procedure Listen;
  function Accept:TSocket;
end;

function GetErrorDesc(ErrorCode:integer): string;

implementation

{TBlockSocket.Create}
constructor TBlockSocket.Create;
begin
  inherited create;
  FRaiseExcept:=false;
  FSocket:=INVALID_SOCKET;
  FProtocol:=IPPROTO_IP;
  SockCheck(winsock.WSAStartup($101, FWsaData));
  ExceptCheck;
end;

{TBlockSocket.Destroy}
destructor TBlockSocket.Destroy;
begin
  CloseSocket;
  inherited destroy;
end;

{TBlockSocket.SetSin}
procedure TBlockSocket.SetSin (var sin:TSockAddrIn;ip,port:string);
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  HostEnt: PHostEnt;
begin
  FillChar(sin,Sizeof(sin),0);
  sin.sin_family := AF_INET;
  ProtoEnt:= getprotobynumber(FProtocol);
  ServEnt:=nil;
  If ProtoEnt <> nil then
    ServEnt:= getservbyname(PChar(port), ProtoEnt^.p_name);
  if ServEnt = nil then
    Sin.sin_port:= htons(StrToIntDef(Port,0))
  else
    Sin.sin_port:= ServEnt^.s_port;
  if ip='255.255.255.255'
    then Sin.sin_addr.s_addr:=u_long(INADDR_BROADCAST)
    else
      begin
        Sin.sin_addr.s_addr:= inet_addr(PChar(ip));
        if SIn.sin_addr.s_addr = u_long(INADDR_NONE) then
          begin
            HostEnt:= gethostbyname(PChar(ip));
            if HostEnt <> nil then
              SIn.sin_addr.S_addr:= longint(plongint(HostEnt^.h_addr_list^)^);
          end;
      end;
end;

{TBlockSocket.GetSinIP}
function TBlockSocket.GetSinIP (sin:TSockAddrIn):string;
var
  p:pchar;
begin
  p:=inet_ntoa(Sin.sin_addr);
  if p=nil then result:=''
    else result:=p;
end;

{TBlockSocket.GetSinPort}
function TBlockSocket.GetSinPort (sin:TSockAddrIn):integer;
begin
  result:=ntohs(Sin.sin_port);
end;

{TBlockSocket.CreateSocket}
Procedure TBlockSocket.CreateSocket;
begin
  if FSocket=INVALID_SOCKET then FLastError:=winsock.WSAGetLastError
    else FLastError:=0;
  ExceptCheck;
end;


{TBlockSocket.CloseSocket}
Procedure TBlockSocket.CloseSocket;
begin
  winsock.CloseSocket(FSocket);
end;

{TBlockSocket.Bind}
procedure TBlockSocket.Bind(ip,port:string);
var
  sin:TSockAddrIn;
  len:integer;
begin
  SetSin(sin,ip,port);
  SockCheck(winsock.bind(FSocket,sin,sizeof(sin)));
  len:=sizeof(FLocalSin);
  Winsock.GetSockName(FSocket,FLocalSin,Len);
  ExceptCheck;
end;

{TBlockSocket.Connect}
procedure TBlockSocket.Connect(ip,port:string);
var
  sin:TSockAddrIn;
begin
  SetSin(sin,ip,port);
  SockCheck(winsock.connect(FSocket,sin,sizeof(sin)));
  GetSins;
  ExceptCheck;
end;

{TBlockSocket.GetSins}
procedure TBlockSocket.GetSins;
var
  len:integer;
begin
  len:=sizeof(FLocalSin);
  Winsock.GetSockName(FSocket,FLocalSin,Len);
  len:=sizeof(FRemoteSin);
  Winsock.GetPeerName(FSocket,FremoteSin,Len);
end;

{TBlockSocket.SendBuffer}
function TBlockSocket.SendBuffer(buffer:pointer;length:integer):integer;
begin
  result:=winsock.send(FSocket,buffer^,length,0);
  sockcheck(result);
  ExceptCheck;
end;

{TBlockSocket.SendByte}
procedure TBlockSocket.SendByte(data:byte);
begin
  sockcheck(winsock.send(FSocket,data,1,0));
  ExceptCheck;
end;

{TBlockSocket.SendString}
procedure TBlockSocket.SendString(data:string);
begin
  sockcheck(winsock.send(FSocket,pchar(data)^,length(data),0));
  ExceptCheck;
end;

{TBlockSocket.RecvBuffer}
function TBlockSocket.RecvBuffer(buffer:pointer;length:integer):integer;
begin
  result:=winsock.recv(FSocket,buffer^,length,0);
  if result=0
    then FLastError:=WSAENOTCONN
    else sockcheck(result);
  ExceptCheck;
end;

{TBlockSocket.RecvByte}
function TBlockSocket.RecvByte(timeout:integer):byte;
var
  y:integer;
  data:byte;
begin
  data:=0;
  result:=0;
  if CanRead(timeout) then
    begin
      y:=winsock.recv(FSocket,data,1,0);
      if y=0 then FLastError:=WSAENOTCONN
        else sockcheck(y);
      result:=data;
    end
    else FLastError:=WSAETIMEDOUT;
  ExceptCheck;
end;

{TBlockSocket.Recvstring}
function TBlockSocket.Recvstring(timeout:integer):string;
const
  maxbuf=1024;
var
  x:integer;
  s:string;
  bp:array[0..maxbuf] of char;
  c:char;
  pp:pchar;
  r:integer;
begin
  s:='';
  pp:=bp;
  FLastError:=0;
  c:=#0;
  repeat
    if FBuffer='' then
      begin
        x:=waitingdata;
        if x=0 then x:=1;
        if x>maxbuf then x:=maxbuf;
        if x=1 then
          begin
            c:=char(RecvByte(timeout));
            if FLastError<>0 then break;
            Fbuffer:=c;
          end
        else
          begin
            r:=Winsock.recv(FSocket,pp^,x,0);
            SockCheck(r);
            if r=0 then FLastError:=WSAENOTCONN;
            if FLastError<>0 then break;
            bp[r]:=#0;
            Fbuffer:=pp;
          end;
      end;
    x:=pos(#10,Fbuffer);
    if x<=0 then x:=length(Fbuffer);
    s:=s+copy(Fbuffer,1,x-1);
    c:=Fbuffer[x];
    delete(Fbuffer,1,x);
    s:=s+c;
  until c = #10;

  if FLastError=0 then
    begin
      s:=AdjustLineBreaks(s);
      x:=pos(#13+#10,s);
      if x>0 then s:=copy(s,1,x-1);
      result:=s;
    end
  else result:='';
  ExceptCheck;
end;

{TBlockSocket.PeekBuffer}
function TBlockSocket.PeekBuffer(buffer:pointer;length:integer):integer;
begin
  result:=winsock.recv(FSocket,buffer^,length,MSG_PEEK);
  sockcheck(result);
  ExceptCheck;
end;

{TBlockSocket.PeekByte}
function TBlockSocket.PeekByte(timeout:integer):byte;
var
  y:integer;
  data:byte;
begin
  data:=0;
  result:=0;
  if CanRead(timeout) then
    begin
      y:=winsock.recv(FSocket,data,1,MSG_PEEK);
      if y=0 then FLastError:=WSAENOTCONN;
      sockcheck(y);
      result:=data;
    end
    else FLastError:=WSAETIMEDOUT;
  ExceptCheck;
end;

{TBlockSocket.SockCheck}
function TBlockSocket.SockCheck(SockResult:integer):integer;
begin
  if SockResult=SOCKET_ERROR then result:=winsock.WSAGetLastError
    else result:=0;
  FLastError:=result;
end;

{TBlockSocket.ExceptCheck}
procedure TBlockSocket.ExceptCheck;
var
  e:ESynapseError;
  s:string;
begin
  if FRaiseExcept and (LastError<>0) then
    begin
      s:=GetErrorDesc(LastError);
      e:=ESynapseError.CreateFmt('TCP/IP socket error %d: %s',[LastError,s]);
      e.ErrorCode:=LastError;
      e.ErrorMessage:=s;
      raise e;
    end;
end;

{TBlockSocket.WaitingData}
function TBlockSocket.WaitingData:integer;
var
  x:integer;
begin
  winsock.ioctlsocket(FSocket,FIONREAD,x);
  result:=x;
end;

{TBlockSocket.SetLinger}
procedure TBlockSocket.SetLinger(enable:boolean;Linger:integer);
var
  li:TLinger;
begin
  li.l_onoff  := ord(enable);
  li.l_linger := Linger div 1000;
  SockCheck(winsock.SetSockOpt(FSocket, SOL_SOCKET, SO_LINGER, @li, SizeOf(li)));
  ExceptCheck;
end;

{TBlockSocket.LocalName}
function TBlockSocket.LocalName:string;
var
  buf: array[0..255] of char;
  Pbuf:pchar;
  RemoteHost : PHostEnt;
begin
  pbuf:=buf;
  result:='';
  winsock.gethostname(pbuf,255);
  if pbuf<>'' then
    begin
      RemoteHost:=Winsock.GetHostByName(pbuf);
      if remoteHost<>nil then result:=pchar(RemoteHost^.h_name);
    end;
  if result='' then result:='127.0.0.1';
end;


{TBlockSocket.GetLocalSinIP}
function TBlockSocket.GetLocalSinIP:string;
begin
  result:=GetSinIP(FLocalSin);
end;

{TBlockSocket.GetRemoteSinIP}
function TBlockSocket.GetRemoteSinIP:string;
begin
  result:=GetSinIP(FRemoteSin);
end;

{TBlockSocket.GetLocalSinPort}
function TBlockSocket.GetLocalSinPort:integer;
begin
  result:=GetSinPort(FLocalSin);
end;

{TBlockSocket.GetRemoteSinPort}
function TBlockSocket.GetRemoteSinPort:integer;
begin
  result:=GetSinPort(FRemoteSin);
end;

{TBlockSocket.CanRead}
function TBlockSocket.CanRead(Timeout:integer):boolean;
var
  FDSet:TFDSet;
  TimeVal:PTimeVal;
  TimeV:tTimeval;
  x:integer;
begin
  Timev.tv_usec:=(Timeout mod 1000)*1000;
  Timev.tv_sec:=Timeout div 1000;
  TimeVal:=@TimeV;
  if timeout = -1 then Timeval:=nil;
  Winsock.FD_Zero(FDSet);
  Winsock.FD_Set(FSocket,FDSet);
  x:=winsock.Select(0,@FDSet,nil,nil,TimeVal);
  SockCheck(x);
  If FLastError<>0 then x:=0;
  result:=x>0;
  ExceptCheck;
end;

{TBlockSocket.CanWrite}
function TBlockSocket.CanWrite(Timeout:integer):boolean;
var
  FDSet:TFDSet;
  TimeVal:PTimeVal;
  TimeV:tTimeval;
  x:integer;
begin
  Timev.tv_usec:=(Timeout mod 1000)*1000;
  Timev.tv_sec:=Timeout div 1000;
  TimeVal:=@TimeV;
  if timeout = -1 then Timeval:=nil;
  Winsock.FD_Zero(FDSet);
  Winsock.FD_Set(FSocket,FDSet);
  x:=winsock.Select(0,nil,@FDSet,nil,TimeVal);
  SockCheck(x);
  If FLastError<>0 then x:=0;
  result:=x>0;
  ExceptCheck;
end;

{TBlockSocket.SendBufferTo}
function TBlockSocket.SendBufferTo(buffer:pointer;length:integer):integer;
var
  len:integer;
begin
  len:=sizeof(FRemoteSin);
  result:=winsock.sendto(FSocket,buffer^,length,0,FRemoteSin,len);
  sockcheck(result);
  ExceptCheck;
end;

{TBlockSocket.RecvBufferFrom}
function TBlockSocket.RecvBufferFrom(buffer:pointer;length:integer):integer;
var
  len:integer;
begin
  len:=sizeof(FRemoteSin);
  result:=winsock.recvfrom(FSocket,buffer^,length,0,FRemoteSin,len);
  sockcheck(result);
  ExceptCheck;
end;



{======================================================================}

{TUDPBlockSocket.CreateSocket}
Procedure TUDPBlockSocket.CreateSocket;
begin
  FSocket:=winsock.socket(PF_INET,SOCK_DGRAM,IPPROTO_UDP);
  FProtocol:=IPPROTO_UDP;
  inherited createSocket;
end;

{TUDPBlockSocket.EnableBroadcast}
function TUDPBlockSocket.EnableBroadcast(Value:Boolean):Boolean;
var
  Opt:integer;
  Res:integer;
begin
  opt:=Ord(Value);
  Res:=winsock.SetSockOpt(FSocket, SOL_SOCKET, SO_BROADCAST, @opt, SizeOf(opt));
  SockCheck(Res);
  Result:=res=0;
  ExceptCheck;
end;


{======================================================================}

{TTCPBlockSocket.CreateSocket}
Procedure TTCPBlockSocket.CreateSocket;
begin
  FSocket:=winsock.socket(PF_INET,SOCK_STREAM,IPPROTO_TCP);
  FProtocol:=IPPROTO_TCP;
  inherited createSocket;
end;

{TTCPBlockSocket.Listen}
procedure TTCPBlockSocket.Listen;
begin
  SockCheck(winsock.listen(FSocket,SOMAXCONN));
  ExceptCheck;
end;

{TTCPBlockSocket.Accept}
function TTCPBlockSocket.Accept:TSocket;
var
  len:integer;
begin
  len:=sizeof(FRemoteSin);
{$IFDEF VER090}
  result:=winsock.accept(FSocket,TSockAddr(FRemoteSin),len));
{$ELSE}
  result:=winsock.accept(FSocket,@FRemoteSin,@len);
{$ENDIF}
  SockCheck(result);
  ExceptCheck;
end;


{======================================================================}

{GetErrorDesc}
function GetErrorDesc(ErrorCode:integer): string;
begin
  case ErrorCode of
    0                  : Result:= 'OK';
    WSAEINTR           : Result:= 'Interrupted system call';
    WSAEBADF           : Result:= 'Bad file number';
    WSAEACCES          : Result:= 'Permission denied';
    WSAEFAULT          : Result:= 'Bad address';
    WSAEINVAL          : Result:= 'Invalid argument';
    WSAEMFILE          : Result:= 'Too many open files';
    WSAEWOULDBLOCK     : Result:= 'Operation would block';
    WSAEINPROGRESS     : Result:= 'Operation now in progress';
    WSAEALREADY        : Result:= 'Operation already in progress';
    WSAENOTSOCK        : Result:= 'Socket operation on nonsocket';
    WSAEDESTADDRREQ    : Result:= 'Destination address required';
    WSAEMSGSIZE        : Result:= 'Message too long';
    WSAEPROTOTYPE      : Result:= 'Protocol wrong type for socket';
    WSAENOPROTOOPT     : Result:= 'Protocol not available';
    WSAEPROTONOSUPPORT : Result:= 'Protocol not supported';
    WSAESOCKTNOSUPPORT : Result:= 'Socket not supported';
    WSAEOPNOTSUPP      : Result:= 'Operation not supported on socket';
    WSAEPFNOSUPPORT    : Result:= 'Protocol family not supported';
    WSAEAFNOSUPPORT    : Result:= 'Address family not supported';
    WSAEADDRINUSE      : Result:= 'Address already in use';
    WSAEADDRNOTAVAIL   : Result:= 'Can''t assign requested address';
    WSAENETDOWN        : Result:= 'Network is down';
    WSAENETUNREACH     : Result:= 'Network is unreachable';
    WSAENETRESET       : Result:= 'Network dropped connection on reset';
    WSAECONNABORTED    : Result:= 'Software caused connection abort';
    WSAECONNRESET      : Result:= 'Connection reset by peer';
    WSAENOBUFS         : Result:= 'No buffer space available';
    WSAEISCONN         : Result:= 'Socket is already connected';
    WSAENOTCONN        : Result:= 'Socket is not connected';
    WSAESHUTDOWN       : Result:= 'Can''t send after socket shutdown';
    WSAETOOMANYREFS    : Result:= 'Too many references:can''t splice';
    WSAETIMEDOUT       : Result:= 'Connection timed out';
    WSAECONNREFUSED    : Result:= 'Connection refused';
    WSAELOOP           : Result:= 'Too many levels of symbolic links';
    WSAENAMETOOLONG    : Result:= 'File name is too long';
    WSAEHOSTDOWN       : Result:= 'Host is down';
    WSAEHOSTUNREACH    : Result:= 'No route to host';
    WSAENOTEMPTY       : Result:= 'Directory is not empty';
    WSAEPROCLIM        : Result:= 'Too many processes';
    WSAEUSERS          : Result:= 'Too many users';
    WSAEDQUOT          : Result:= 'Disk quota exceeded';
    WSAESTALE          : Result:= 'Stale NFS file handle';
    WSAEREMOTE         : Result:= 'Too many levels of remote in path';
    WSASYSNOTREADY     : Result:= 'Network subsystem is unusable';
    WSAVERNOTSUPPORTED : Result:= 'Winsock DLL cannot support this application';
    WSANOTINITIALISED  : Result:= 'Winsock not initialized';
    WSAHOST_NOT_FOUND  : Result:= 'Host not found';
    WSATRY_AGAIN       : Result:= 'Non authoritative - host not found';
    WSANO_RECOVERY     : Result:= 'Non recoverable error';
    WSANO_DATA         : Result:= 'Valid name, no data record of requested type'
  else
    Result:= 'Not a Winsock error ('+IntToStr(ErrorCode)+')';
  end;
end;

begin
  exit;
  asm
    db 'Synapse TCP/IP library by Lukas Gebauer',0
  end;
end.
