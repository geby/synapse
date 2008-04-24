{==============================================================================|
| Project : Delphree - Synapse                                   | 002.001.001 |
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
| Portions created by Lukas Gebauer are Copyright (c)1999,2000,2001.           |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
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
  function GetSizeRecvBuffer:integer;
  procedure SetSizeRecvBuffer(size:integer);
  function GetSizeSendBuffer:integer;
  procedure SetSizeSendBuffer(size:integer);
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
  function RecvBufferEx(buffer:pointer;length:integer;timeout:integer):integer; virtual;
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

  property LocalSin:TSockAddrIn read FLocalSin;
  property RemoteSin:TSockAddrIn read FRemoteSin;
published
  property socket:TSocket read FSocket write FSocket;
  property LastError:integer read FLastError;
  property Protocol:integer read FProtocol;
  property LineBuffer:string read FBuffer write FBuffer;
  property RaiseExcept:boolean read FRaiseExcept write FRaiseExcept;
  property SizeRecvBuffer:integer read GetSizeRecvBuffer write SetSizeRecvBuffer;
  property SizeSendBuffer:integer read GetSizeSendBuffer write SetSizeSendBuffer;
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
  Fbuffer:='';
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
  Fbuffer:='';
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
  Fbuffer:='';
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
  Fbuffer:='';
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

{TBlockSocket.RecvBufferEx}
function TBlockSocket.RecvBufferEx(buffer:pointer;length:integer;timeout:integer):integer;
var
  s,ss,st:string;
  x,l,lss:integer;
  fb,fs:integer;
  max:integer;
begin
  FLastError:=0;
  x:=system.length(FBuffer);
  if length<=x
    then
      begin
        fb:=length;
        fs:=0;
      end
    else
      begin
        fb:=x;
        fs:=length-x;
      end;
  ss:='';
  if fb>0 then
    begin
      s:=copy(FBuffer,1,fb);
      delete(Fbuffer,1,fb);
    end;
  if fs>0 then
    begin
      Max:=GetSizeRecvBuffer;
      ss:='';
      while system.length(ss)<fs do
        begin
          if canread(timeout) then
            begin
              l:=WaitingData;
              if l>max
                then l:=max;
              if (system.length(ss)+l)>fs
                then l:=fs-system.length(ss);
              setlength(st,l);
              x:=winsock.recv(FSocket,pointer(st)^,l,0);
              if x=0
                then FLastError:=WSAENOTCONN
                else sockcheck(x);
              if Flasterror<>0
                then break;
              lss:=system.length(ss);
              setlength(ss,lss+x);
              Move(pointer(st)^,Pointer(@ss[lss+1])^, x);
              {It is 3x faster then ss:=ss+copy(st,1,x);}
              sleep(0);
            end
            else FLastError:=WSAETIMEDOUT;
          if Flasterror<>0
            then break;
        end;
      fs:=system.length(ss);
    end;
  result:=fb+fs;
  s:=s+ss;
  move(pointer(s)^,buffer^,result);
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
  c:char;
  r:integer;
begin
  s:='';
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
            setlength(Fbuffer,x);
            r:=Winsock.recv(FSocket,pointer(FBuffer)^,x,0);
            SockCheck(r);
            if r=0 then FLastError:=WSAENOTCONN;
            if FLastError<>0 then break;
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

{TBlockSocket.GetSizeRecvBuffer}
function TBlockSocket.GetSizeRecvBuffer:integer;
var
  l:integer;
begin
  l:=SizeOf(result);
  SockCheck(winsock.getSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, @result, l));
  if Flasterror<>0
    then result:=1024;
  ExceptCheck;
end;

{TBlockSocket.SetSizeRecvBuffer}
procedure TBlockSocket.SetSizeRecvBuffer(size:integer);
begin
  SockCheck(winsock.SetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, @size, SizeOf(size)));
  ExceptCheck;
end;

{TBlockSocket.GetSizeSendBuffer}
function TBlockSocket.GetSizeSendBuffer:integer;
var
  l:integer;
begin
  l:=SizeOf(result);
  SockCheck(winsock.getSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, @result, l));
  if Flasterror<>0
    then result:=1024;
  ExceptCheck;
end;

{TBlockSocket.SetSizeSendBuffer}
procedure TBlockSocket.SetSizeSendBuffer(size:integer);
begin
  SockCheck(winsock.SetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, @size, SizeOf(size)));
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
  GetSins;
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
    WSAEINTR           :{10004} Result:= 'Interrupted system call';
    WSAEBADF           :{10009} Result:= 'Bad file number';
    WSAEACCES          :{10013} Result:= 'Permission denied';
    WSAEFAULT          :{10014} Result:= 'Bad address';
    WSAEINVAL          :{10022} Result:= 'Invalid argument';
    WSAEMFILE          :{10024} Result:= 'Too many open files';
    WSAEWOULDBLOCK     :{10035} Result:= 'Operation would block';
    WSAEINPROGRESS     :{10036} Result:= 'Operation now in progress';
    WSAEALREADY        :{10037} Result:= 'Operation already in progress';
    WSAENOTSOCK        :{10038} Result:= 'Socket operation on nonsocket';
    WSAEDESTADDRREQ    :{10039} Result:= 'Destination address required';
    WSAEMSGSIZE        :{10040} Result:= 'Message too long';
    WSAEPROTOTYPE      :{10041} Result:= 'Protocol wrong type for socket';
    WSAENOPROTOOPT     :{10042} Result:= 'Protocol not available';
    WSAEPROTONOSUPPORT :{10043} Result:= 'Protocol not supported';
    WSAESOCKTNOSUPPORT :{10044} Result:= 'Socket not supported';
    WSAEOPNOTSUPP      :{10045} Result:= 'Operation not supported on socket';
    WSAEPFNOSUPPORT    :{10046} Result:= 'Protocol family not supported';
    WSAEAFNOSUPPORT    :{10047} Result:= 'Address family not supported';
    WSAEADDRINUSE      :{10048} Result:= 'Address already in use';
    WSAEADDRNOTAVAIL   :{10049} Result:= 'Can''t assign requested address';
    WSAENETDOWN        :{10050} Result:= 'Network is down';
    WSAENETUNREACH     :{10051} Result:= 'Network is unreachable';
    WSAENETRESET       :{10052} Result:= 'Network dropped connection on reset';
    WSAECONNABORTED    :{10053} Result:= 'Software caused connection abort';
    WSAECONNRESET      :{10054} Result:= 'Connection reset by peer';
    WSAENOBUFS         :{10055} Result:= 'No buffer space available';
    WSAEISCONN         :{10056} Result:= 'Socket is already connected';
    WSAENOTCONN        :{10057} Result:= 'Socket is not connected';
    WSAESHUTDOWN       :{10058} Result:= 'Can''t send after socket shutdown';
    WSAETOOMANYREFS    :{10059} Result:= 'Too many references:can''t splice';
    WSAETIMEDOUT       :{10060} Result:= 'Connection timed out';
    WSAECONNREFUSED    :{10061} Result:= 'Connection refused';
    WSAELOOP           :{10062} Result:= 'Too many levels of symbolic links';
    WSAENAMETOOLONG    :{10063} Result:= 'File name is too long';
    WSAEHOSTDOWN       :{10064} Result:= 'Host is down';
    WSAEHOSTUNREACH    :{10065} Result:= 'No route to host';
    WSAENOTEMPTY       :{10066} Result:= 'Directory is not empty';
    WSAEPROCLIM        :{10067} Result:= 'Too many processes';
    WSAEUSERS          :{10068} Result:= 'Too many users';
    WSAEDQUOT          :{10069} Result:= 'Disk quota exceeded';
    WSAESTALE          :{10070} Result:= 'Stale NFS file handle';
    WSAEREMOTE         :{10071} Result:= 'Too many levels of remote in path';
    WSASYSNOTREADY     :{10091} Result:= 'Network subsystem is unusable';
    WSAVERNOTSUPPORTED :{10092} Result:= 'Winsock DLL cannot support this application';
    WSANOTINITIALISED  :{10093} Result:= 'Winsock not initialized';
    WSAEDISCON         :{10101} Result:= 'WSAEDISCON-10101';
    WSAHOST_NOT_FOUND  :{11001} Result:= 'Host not found';
    WSATRY_AGAIN       :{11002} Result:= 'Non authoritative - host not found';
    WSANO_RECOVERY     :{11003} Result:= 'Non recoverable error';
    WSANO_DATA         :{11004} Result:= 'Valid name, no data record of requested type'
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
