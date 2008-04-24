{==============================================================================|
| Project : Delphree - Synapse                                   | 003.002.000 |
|==============================================================================|
| Content: Library base                                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)1999,2000,2001.           |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$WEAKPACKAGEUNIT ON}

unit blcksock;

interface

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  Libc, kernelioctl,
{$ELSE}
  Windows, WinSock,
{$ENDIF}
  synsock;

const
  cLocalhost = 'localhost';

type

  ESynapseError = class(Exception)
  public
    ErrorCode: Integer;
    ErrorMessage: string;
  end;

  TBlockSocket = class(TObject)
  private
    FWsaData: TWSADATA;
    FLocalSin: TSockAddrIn;
    FRemoteSin: TSockAddrIn;
    FLastError: Integer;
    FBuffer: string;
    FRaiseExcept: Boolean;
    function GetSizeRecvBuffer: Integer;
    procedure SetSizeRecvBuffer(Size: Integer);
    function GetSizeSendBuffer: Integer;
    procedure SetSizeSendBuffer(Size: Integer);
  protected
    FSocket: TSocket;
    FProtocol: Integer;
    procedure CreateSocket; virtual;
    procedure SetSin(var Sin: TSockAddrIn; IP, Port: string);
    function GetSinIP(Sin: TSockAddrIn): string;
    function GetSinPort(Sin: TSockAddrIn): Integer;
  public
    constructor Create;
    constructor CreateAlternate(Stub: string);
    destructor Destroy; override;
    procedure CloseSocket; virtual;
    procedure Bind(IP, Port: string);
    procedure Connect(IP, Port: string);
    function SendBuffer(Buffer: Pointer; Length: Integer): Integer; virtual;
    procedure SendByte(Data: Byte); virtual;
    procedure SendString(const Data: string); virtual;
    function RecvBuffer(Buffer: Pointer; Length: Integer): Integer; virtual;
    function RecvBufferEx(Buffer: Pointer; Length: Integer;
      Timeout: Integer): Integer; virtual;
    function RecvByte(Timeout: Integer): Byte; virtual;
    function RecvString(Timeout: Integer): string; virtual;
    function PeekBuffer(Buffer: Pointer; Length: Integer): Integer; virtual;
    function PeekByte(Timeout: Integer): Byte; virtual;
    function WaitingData: Integer;
    procedure SetLinger(Enable: Boolean; Linger: Integer);
    procedure GetSins;
    function SockCheck(SockResult: Integer): Integer;
    procedure ExceptCheck;
    function LocalName: string;
    procedure ResolveNameToIP(Name: string; IPList: TStrings);
    function GetLocalSinIP: string;
    function GetRemoteSinIP: string;
    function GetLocalSinPort: Integer;
    function GetRemoteSinPort: Integer;
    function CanRead(Timeout: Integer): Boolean;
    function CanWrite(Timeout: Integer): Boolean;
    function SendBufferTo(Buffer: Pointer; Length: Integer): Integer;
    function RecvBufferFrom(Buffer: Pointer; Length: Integer): Integer;
    function GroupCanRead(const SocketList: TList; Timeout: Integer;
      const CanReadList: TList): Boolean;

    //See 'winsock2.txt' file in distribute package!
    function SetTimeout(Timeout: Integer): Boolean;

    property LocalSin: TSockAddrIn read FLocalSin;
    property RemoteSin: TSockAddrIn read FRemoteSin;
  published
    class function GetErrorDesc(ErrorCode: Integer): string;
    property Socket: TSocket read FSocket write FSocket;
    property LastError: Integer read FLastError;
    property Protocol: Integer read FProtocol;
    property LineBuffer: string read FBuffer write FBuffer;
    property RaiseExcept: Boolean read FRaiseExcept write FRaiseExcept;
    property SizeRecvBuffer: Integer read GetSizeRecvBuffer write SetSizeRecvBuffer;
    property SizeSendBuffer: Integer read GetSizeSendBuffer write SetSizeSendBuffer;
    property WSAData: TWSADATA read FWsaData;
  end;

  TUDPBlockSocket = class(TBlockSocket)
  public
    procedure CreateSocket; override;
    function EnableBroadcast(Value: Boolean): Boolean;
  end;

  TTCPBlockSocket = class(TBlockSocket)
  public
    procedure CreateSocket; override;
    procedure CloseSocket; override;
    procedure Listen;
    function Accept: TSocket;
  end;

  //See 'winsock2.txt' file in distribute package!
  TICMPBlockSocket = class(TBlockSocket)
  public
    procedure CreateSocket; override;
  end;

  //See 'winsock2.txt' file in distribute package!
  TRAWBlockSocket = class(TBlockSocket)
  public
    procedure CreateSocket; override;
  end;

  TIPHeader = record
    VerLen: Byte;
    TOS: Byte;
    TotalLen: Word;
    Identifer: Word;
    FragOffsets: Word;
    TTL: Byte;
    Protocol: Byte;
    CheckSum: Word;
    SourceIp: DWORD;
    DestIp: DWORD;
    Options: DWORD;
  end;

implementation

constructor TBlockSocket.Create;
var
  e: ESynapseError;
begin
  inherited Create;
  FRaiseExcept := False;
  FSocket := INVALID_SOCKET;
  FProtocol := IPPROTO_IP;
  FBuffer := '';
  if not InitSocketInterface('') then
  begin
    e := ESynapseError.Create('Error loading Winsock DLL!');
    e.ErrorCode := 0;
    e.ErrorMessage := 'Error loading Winsock DLL!';
    raise e;
  end;
  SockCheck(synsock.WSAStartup($101, FWsaData));
  ExceptCheck;
end;

constructor TBlockSocket.CreateAlternate(Stub: string);
var
  e: ESynapseError;
begin
  inherited Create;
  FRaiseExcept := False;
  FSocket := INVALID_SOCKET;
  FProtocol := IPPROTO_IP;
  FBuffer := '';
  if not InitSocketInterface(Stub) then
  begin
    e := ESynapseError.Create('Error loading alternate Winsock DLL (' + Stub + ')!');
    e.ErrorCode := 0;
    e.ErrorMessage := 'Error loading Winsock DLL (' + Stub + ')!';
    raise e;
  end;
  SockCheck(synsock.WSAStartup($101, FWsaData));
  ExceptCheck;
end;

destructor TBlockSocket.Destroy;
begin
  CloseSocket;
  DestroySocketInterface;
  inherited Destroy;
end;

procedure TBlockSocket.SetSin(var Sin: TSockAddrIn; IP, Port: string);
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  HostEnt: PHostEnt;
begin
  FillChar(Sin, Sizeof(Sin), 0);
  Sin.sin_family := AF_INET;
  ProtoEnt := synsock.GetProtoByNumber(FProtocol);
  ServEnt := nil;
  if ProtoEnt <> nil then
    ServEnt := synsock.GetServByName(PChar(Port), ProtoEnt^.p_name);
  if ServEnt = nil then
    Sin.sin_port := synsock.htons(StrToIntDef(Port, 0))
  else
    Sin.sin_port := ServEnt^.s_port;
  if IP = '255.255.255.255' then
    Sin.sin_addr.s_addr := u_long(INADDR_BROADCAST)
  else
  begin
    Sin.sin_addr.s_addr := synsock.inet_addr(PChar(IP));
    if SIn.sin_addr.s_addr = u_long(INADDR_NONE) then
    begin
      HostEnt := synsock.GetHostByName(PChar(IP));
      if HostEnt <> nil then
        SIn.sin_addr.S_addr := Longint(PLongint(HostEnt^.h_addr_list^)^);
    end;
  end;
end;

function TBlockSocket.GetSinIP(Sin: TSockAddrIn): string;
var
  p: PChar;
begin
  p := synsock.inet_ntoa(Sin.sin_addr);
  if p = nil then
    Result := ''
  else
    Result := p;
end;

function TBlockSocket.GetSinPort(Sin: TSockAddrIn): Integer;
begin
  Result := synsock.ntohs(Sin.sin_port);
end;

procedure TBlockSocket.CreateSocket;
begin
  FBuffer := '';
  if FSocket = INVALID_SOCKET then
    FLastError := synsock.WSAGetLastError
  else
    FLastError := 0;
  ExceptCheck;
end;

procedure TBlockSocket.CloseSocket;
begin
  synsock.CloseSocket(FSocket);
end;

procedure TBlockSocket.Bind(IP, Port: string);
var
  Sin: TSockAddrIn;
  Len: Integer;
begin
  SetSin(Sin, IP, Port);
  SockCheck(synsock.Bind(FSocket, Sin, SizeOf(Sin)));
  Len := SizeOf(FLocalSin);
  synsock.GetSockName(FSocket, FLocalSin, Len);
  FBuffer := '';
  ExceptCheck;
end;

procedure TBlockSocket.Connect(IP, Port: string);
var
  Sin: TSockAddrIn;
begin
  SetSin(Sin, IP, Port);
  SockCheck(synsock.Connect(FSocket, Sin, SizeOf(Sin)));
  GetSins;
  FBuffer := '';
  ExceptCheck;
end;

procedure TBlockSocket.GetSins;
var
  Len: Integer;
begin
  Len := SizeOf(FLocalSin);
  synsock.GetSockName(FSocket, FLocalSin, Len);
  Len := SizeOf(FRemoteSin);
  synsock.GetPeerName(FSocket, FremoteSin, Len);
end;

function TBlockSocket.SendBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := synsock.Send(FSocket, Buffer^, Length, 0);
  SockCheck(Result);
  ExceptCheck;
end;

procedure TBlockSocket.SendByte(Data: Byte);
begin
  sockcheck(synsock.Send(FSocket, Data, 1, 0));
  ExceptCheck;
end;

procedure TBlockSocket.SendString(const Data: string);
begin
  SockCheck(synsock.Send(FSocket, PChar(Data)^, Length(Data), 0));
  ExceptCheck;
end;

function TBlockSocket.RecvBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := synsock.Recv(FSocket, Buffer^, Length, 0);
  if Result = 0 then
    FLastError := WSAENOTCONN
  else
    SockCheck(Result);
  ExceptCheck;
end;

function TBlockSocket.RecvBufferEx(Buffer: Pointer; Length: Integer;
  Timeout: Integer): Integer;
var
  s, ss, st: string;
  x, l, lss: Integer;
  fb, fs: Integer;
  max: Integer;
begin
  FLastError := 0;
  x := System.Length(FBuffer);
  if Length <= x then
  begin
    fb := Length;
    fs := 0;
  end
  else
  begin
    fb := x;
    fs := Length - x;
  end;
  ss := '';
  if fb > 0 then
  begin
    s := Copy(FBuffer, 1, fb);
    Delete(FBuffer, 1, fb);
  end;
  if fs > 0 then
  begin
    Max := GetSizeRecvBuffer;
    ss := '';
    while System.Length(ss) < fs do
    begin
      if CanRead(Timeout) then
      begin
        l := WaitingData;
        if l > max then
          l := max;
        if (system.Length(ss) + l) > fs then
          l := fs - system.Length(ss);
        SetLength(st, l);
        x := synsock.Recv(FSocket, Pointer(st)^, l, 0);
        if x = 0 then
          FLastError := WSAENOTCONN
        else
          SockCheck(x);
        if FLastError <> 0 then
          Break;
        lss := system.Length(ss);
        SetLength(ss, lss + x);
        Move(Pointer(st)^, Pointer(@ss[lss + 1])^, x);
        {It is 3x faster then ss:=ss+copy(st,1,x);}
        Sleep(0);
      end
      else
        FLastError := WSAETIMEDOUT;
      if FLastError <> 0 then
        Break;
    end;
    fs := system.Length(ss);
  end;
  Result := fb + fs;
  s := s + ss;
  Move(Pointer(s)^, Buffer^, Result);
  ExceptCheck;
end;

function TBlockSocket.RecvByte(Timeout: Integer): Byte;
var
  y: Integer;
  Data: Byte;
begin
  Data := 0;
  Result := 0;
  if CanRead(Timeout) then
  begin
    y := synsock.Recv(FSocket, Data, 1, 0);
    if y = 0 then
      FLastError := WSAENOTCONN
    else
      SockCheck(y);
    Result := Data;
  end
  else
    FLastError := WSAETIMEDOUT;
  ExceptCheck;
end;

function TBlockSocket.RecvString(Timeout: Integer): string;
const
  MaxBuf = 1024;
var
  x: Integer;
  s: string;
  c: Char;
  r: Integer;
begin
  s := '';
  FLastError := 0;
  c := #0;
  repeat
    if FBuffer = '' then
    begin
      x := WaitingData;
      if x = 0 then
        x := 1;
      if x > MaxBuf then
        x := MaxBuf;
      if x = 1 then
      begin
        c := Char(RecvByte(Timeout));
        if FLastError <> 0 then
          Break;
        FBuffer := c;
      end
      else
      begin
        SetLength(FBuffer, x);
        r := synsock.Recv(FSocket, Pointer(FBuffer)^, x, 0);
        SockCheck(r);
        if r = 0 then
          FLastError := WSAENOTCONN;
        if FLastError <> 0 then
          Break;
        if r < x then
          SetLength(FBuffer, r);
      end;
    end;
    x := Pos(#10, FBuffer);
    if x < 1 then x := Length(FBuffer);
    s := s + Copy(FBuffer, 1, x - 1);
    c := FBuffer[x];
    Delete(FBuffer, 1, x);
    s := s + c;
  until c = #10;

  if FLastError = 0 then
  begin
{$IFDEF LINUX}
    s := AdjustLineBreaks(s, tlbsCRLF);
{$ELSE}
    s := AdjustLineBreaks(s);
{$ENDIF}
    x := Pos(#13 + #10, s);
    if x > 0 then
      s := Copy(s, 1, x - 1);
    Result := s;
  end
  else
    Result := '';
  ExceptCheck;
end;

function TBlockSocket.PeekBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_PEEK);
  SockCheck(Result);
  ExceptCheck;
end;

function TBlockSocket.PeekByte(Timeout: Integer): Byte;
var
  y: Integer;
  Data: Byte;
begin
  Data := 0;
  Result := 0;
  if CanRead(Timeout) then
  begin
    y := synsock.Recv(FSocket, Data, 1, MSG_PEEK);
    if y = 0 then
      FLastError := WSAENOTCONN;
    SockCheck(y);
    Result := Data;
  end
  else
    FLastError := WSAETIMEDOUT;
  ExceptCheck;
end;

function TBlockSocket.SockCheck(SockResult: Integer): Integer;
begin
  if SockResult = SOCKET_ERROR then
    Result := synsock.WSAGetLastError
  else
    Result := 0;
  FLastError := Result;
end;

procedure TBlockSocket.ExceptCheck;
var
  e: ESynapseError;
  s: string;
begin
  if FRaiseExcept and (LastError <> 0) then
  begin
    s := GetErrorDesc(LastError);
    e := ESynapseError.CreateFmt('TCP/IP Socket error %d: %s', [LastError, s]);
    e.ErrorCode := LastError;
    e.ErrorMessage := s;
    raise e;
  end;
end;

function TBlockSocket.WaitingData: Integer;
var
  x: Integer;
begin
  synsock.IoctlSocket(FSocket, FIONREAD, u_long(x));
  Result := x;
end;

procedure TBlockSocket.SetLinger(Enable: Boolean; Linger: Integer);
var
  li: TLinger;
begin
  li.l_onoff := Ord(Enable);
  li.l_linger := Linger div 1000;
  SockCheck(synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_LINGER, @li, SizeOf(li)));
  ExceptCheck;
end;

function TBlockSocket.LocalName: string;
var
  buf: array[0..255] of Char;
  BufPtr: PChar;
  RemoteHost: PHostEnt;
begin
  BufPtr := buf;
  Result := '';
  synsock.GetHostName(BufPtr, SizeOf(buf));
  if BufPtr[0] <> #0 then
  begin
    // try get Fully Qualified Domain Name
    RemoteHost := synsock.GetHostByName(BufPtr);
    if RemoteHost <> nil then
      Result := PChar(RemoteHost^.h_name);
  end;
  if Result = '' then
    Result := '127.0.0.1';
end;

procedure TBlockSocket.ResolveNameToIP(Name: string; IPList: TStrings);
type
  TaPInAddr = array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  RemoteHost: PHostEnt;
  IP: u_long;
  PAdrPtr: PaPInAddr;
  i: Integer;
  s: string;
  InAddr: TInAddr;
begin
  IPList.Clear;
  IP := synsock.inet_addr(PChar(Name));
  if IP = u_long(INADDR_NONE) then
  begin
    RemoteHost := synsock.GetHostByName(PChar(Name));
    if RemoteHost <> nil then
    begin
      PAdrPtr := PAPInAddr(RemoteHost^.h_addr_list);
      i := 0;
      while PAdrPtr^[i] <> nil do
      begin
        InAddr := PAdrPtr^[i]^;
        with InAddr.S_un_b do
          s := Format('%d.%d.%d.%d',
            [Ord(s_b1), Ord(s_b2), Ord(s_b3), Ord(s_b4)]);
        IPList.Add(s);
        Inc(i);
      end;
    end;
  end
  else
    IPList.Add(Name);
end;

function TBlockSocket.GetLocalSinIP: string;
begin
  Result := GetSinIP(FLocalSin);
end;

function TBlockSocket.GetRemoteSinIP: string;
begin
  Result := GetSinIP(FRemoteSin);
end;

function TBlockSocket.GetLocalSinPort: Integer;
begin
  Result := GetSinPort(FLocalSin);
end;

function TBlockSocket.GetRemoteSinPort: Integer;
begin
  Result := GetSinPort(FRemoteSin);
end;

function TBlockSocket.CanRead(Timeout: Integer): Boolean;
var
  FDSet: TFDSet;
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FD_ZERO(FDSet);
  FD_SET(FSocket, FDSet);
  x := synsock.Select(FSocket + 1, @FDSet, nil, nil, TimeVal);
  SockCheck(x);
  if FLastError <> 0 then
    x := 0;
  Result := x > 0;
  ExceptCheck;
end;

function TBlockSocket.CanWrite(Timeout: Integer): Boolean;
var
  FDSet: TFDSet;
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FD_ZERO(FDSet);
  FD_SET(FSocket, FDSet);
  x := synsock.Select(FSocket + 1, nil, @FDSet, nil, TimeVal);
  SockCheck(x);
  if FLastError <> 0 then
    x := 0;
  Result := x > 0;
  ExceptCheck;
end;

function TBlockSocket.SendBufferTo(Buffer: Pointer; Length: Integer): Integer;
var
  Len: Integer;
begin
  Len := SizeOf(FRemoteSin);
  Result := synsock.SendTo(FSocket, Buffer^, Length, 0, FRemoteSin, Len);
  SockCheck(Result);
  ExceptCheck;
end;

function TBlockSocket.RecvBufferFrom(Buffer: Pointer; Length: Integer): Integer;
var
  Len: Integer;
begin
  Len := SizeOf(FRemoteSin);
  Result := synsock.RecvFrom(FSocket, Buffer^, Length, 0, FRemoteSin, Len);
  SockCheck(Result);
  ExceptCheck;
end;

function TBlockSocket.GetSizeRecvBuffer: Integer;
var
  l: Integer;
begin
  l := SizeOf(Result);
  SockCheck(synsock.GetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, @Result, l));
  if FLastError <> 0 then
    Result := 1024;
  ExceptCheck;
end;

procedure TBlockSocket.SetSizeRecvBuffer(Size: Integer);
begin
  SockCheck(synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, @Size, SizeOf(Size)));
  ExceptCheck;
end;

function TBlockSocket.GetSizeSendBuffer: Integer;
var
  l: Integer;
begin
  l := SizeOf(Result);
  SockCheck(synsock.GetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, @Result, l));
  if FLastError <> 0 then
    Result := 1024;
  ExceptCheck;
end;

procedure TBlockSocket.SetSizeSendBuffer(Size: Integer);
begin
  SockCheck(synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, @Size, SizeOf(Size)));
  ExceptCheck;
end;

//See 'winsock2.txt' file in distribute package!

function TBlockSocket.SetTimeout(Timeout: Integer): Boolean;
begin
  Result := synsock.SetSockOpt(FSocket, SOL_SOCKET,
    SO_RCVTIMEO, @Timeout, SizeOf(Timeout)) <> SOCKET_ERROR;
  Result := Result and (synsock.SetSockOpt(FSocket, SOL_SOCKET,
    SO_SNDTIMEO, @Timeout, SizeOf(Timeout)) <> SOCKET_ERROR);
end;

function TBlockSocket.GroupCanRead(const SocketList: TList; Timeout: Integer;
  const CanReadList: TList): boolean;
var
  FDSet: TFDSet;
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x, n: Integer;
  Max: Integer;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FD_ZERO(FDSet);
  Max := 0;
  for n := 0 to SocketList.Count - 1 do
    if TObject(SocketList.Items[n]) is TBlockSocket then
    begin
      if TBlockSocket(SocketList.Items[n]).Socket > Max then
        Max := TBlockSocket(SocketList.Items[n]).Socket;
      FD_SET(TBlockSocket(SocketList.Items[n]).Socket, FDSet);
    end;
  x := synsock.Select(Max + 1, @FDSet, nil, nil, TimeVal);
  SockCheck(x);
  ExceptCheck;
  if FLastError <> 0 then
    x := 0;
  Result := x > 0;
  CanReadList.Clear;
  if Result then
    for n := 0 to SocketList.Count - 1 do
      if TObject(SocketList.Items[n]) is TBlockSocket then
        if FD_ISSET(TBlockSocket(SocketList.Items[n]).Socket, FDSet) then
          CanReadList.Add(TBlockSocket(SocketList.Items[n]));
end;

class function TBlockSocket.GetErrorDesc(ErrorCode: Integer): string;
begin
  case ErrorCode of
    0:
      Result := 'OK';
    WSAEINTR: {10004}
      Result := 'Interrupted system call';
    WSAEBADF: {10009}
      Result := 'Bad file number';
    WSAEACCES: {10013}
      Result := 'Permission denied';
    WSAEFAULT: {10014}
      Result := 'Bad address';
    WSAEINVAL: {10022}
      Result := 'Invalid argument';
    WSAEMFILE: {10024}
      Result := 'Too many open files';
    WSAEWOULDBLOCK: {10035}
      Result := 'Operation would block';
    WSAEINPROGRESS: {10036}
      Result := 'Operation now in progress';
    WSAEALREADY: {10037}
      Result := 'Operation already in progress';
    WSAENOTSOCK: {10038}
      Result := 'Socket operation on nonsocket';
    WSAEDESTADDRREQ: {10039}
      Result := 'Destination address required';
    WSAEMSGSIZE: {10040}
      Result := 'Message too long';
    WSAEPROTOTYPE: {10041}
      Result := 'Protocol wrong type for Socket';
    WSAENOPROTOOPT: {10042}
      Result := 'Protocol not available';
    WSAEPROTONOSUPPORT: {10043}
      Result := 'Protocol not supported';
    WSAESOCKTNOSUPPORT: {10044}
      Result := 'Socket not supported';
    WSAEOPNOTSUPP: {10045}
      Result := 'Operation not supported on Socket';
    WSAEPFNOSUPPORT: {10046}
      Result := 'Protocol family not supported';
    WSAEAFNOSUPPORT: {10047}
      Result := 'Address family not supported';
    WSAEADDRINUSE: {10048}
      Result := 'Address already in use';
    WSAEADDRNOTAVAIL: {10049}
      Result := 'Can''t assign requested address';
    WSAENETDOWN: {10050}
      Result := 'Network is down';
    WSAENETUNREACH: {10051}
      Result := 'Network is unreachable';
    WSAENETRESET: {10052}
      Result := 'Network dropped connection on reset';
    WSAECONNABORTED: {10053}
      Result := 'Software caused connection abort';
    WSAECONNRESET: {10054}
      Result := 'Connection reset by peer';
    WSAENOBUFS: {10055}
      Result := 'No Buffer space available';
    WSAEISCONN: {10056}
      Result := 'Socket is already connected';
    WSAENOTCONN: {10057}
      Result := 'Socket is not connected';
    WSAESHUTDOWN: {10058}
      Result := 'Can''t send after Socket shutdown';
    WSAETOOMANYREFS: {10059}
      Result := 'Too many references:can''t splice';
    WSAETIMEDOUT: {10060}
      Result := 'Connection timed out';
    WSAECONNREFUSED: {10061}
      Result := 'Connection refused';
    WSAELOOP: {10062}
      Result := 'Too many levels of symbolic links';
    WSAENAMETOOLONG: {10063}
      Result := 'File name is too long';
    WSAEHOSTDOWN: {10064}
      Result := 'Host is down';
    WSAEHOSTUNREACH: {10065}
      Result := 'No route to host';
    WSAENOTEMPTY: {10066}
      Result := 'Directory is not empty';
    WSAEPROCLIM: {10067}
      Result := 'Too many processes';
    WSAEUSERS: {10068}
      Result := 'Too many users';
    WSAEDQUOT: {10069}
      Result := 'Disk quota exceeded';
    WSAESTALE: {10070}
      Result := 'Stale NFS file handle';
    WSAEREMOTE: {10071}
      Result := 'Too many levels of remote in path';
    WSASYSNOTREADY: {10091}
      Result := 'Network subsystem is unusable';
    WSAVERNOTSUPPORTED: {10092}
      Result := 'Winsock DLL cannot support this application';
    WSANOTINITIALISED: {10093}
      Result := 'Winsock not initialized';
    WSAEDISCON: {10101}
      Result := 'WSAEDISCON-10101';
    WSAHOST_NOT_FOUND: {11001}
      Result := 'Host not found';
    WSATRY_AGAIN: {11002}
      Result := 'Non authoritative - host not found';
    WSANO_RECOVERY: {11003}
      Result := 'Non recoverable error';
    WSANO_DATA: {11004}
      Result := 'Valid name, no data record of requested type'
  else
    Result := 'Not a Winsock error (' + IntToStr(ErrorCode) + ')';
  end;
end;

{======================================================================}

procedure TUDPBlockSocket.CreateSocket;
begin
  FSocket := synsock.Socket(PF_INET, Integer(SOCK_DGRAM), IPPROTO_UDP);
  FProtocol := IPPROTO_UDP;
  inherited CreateSocket;
end;

function TUDPBlockSocket.EnableBroadcast(Value: Boolean): Boolean;
var
  Opt: Integer;
  Res: Integer;
begin
  opt := Ord(Value);
  Res := synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_BROADCAST, @Opt, SizeOf(opt));
  SockCheck(Res);
  Result := res = 0;
  ExceptCheck;
end;

{======================================================================}

procedure TTCPBlockSocket.CreateSocket;
begin
  FSocket := synsock.Socket(PF_INET, Integer(SOCK_STREAM), IPPROTO_TCP);
  FProtocol := IPPROTO_TCP;
  inherited CreateSocket;
end;

procedure TTCPBlockSocket.CloseSocket;
begin
  synsock.Shutdown(FSocket, 1);
  inherited CloseSocket;
end;

procedure TTCPBlockSocket.Listen;
begin
  SockCheck(synsock.Listen(FSocket, SOMAXCONN));
  GetSins;
  ExceptCheck;
end;

function TTCPBlockSocket.Accept: TSocket;
var
  Len: Integer;
begin
  Len := SizeOf(FRemoteSin);
  Result := synsock.Accept(FSocket, @FRemoteSin, @Len);
  SockCheck(Result);
  ExceptCheck;
end;

{======================================================================}

//See 'winsock2.txt' file in distribute package!

procedure TICMPBlockSocket.CreateSocket;
begin
  FSocket := synsock.Socket(PF_INET, Integer(SOCK_RAW), IPPROTO_ICMP);
  FProtocol := IPPROTO_ICMP;
  inherited CreateSocket;
end;

{======================================================================}

//See 'winsock2.txt' file in distribute package!

procedure TRAWBlockSocket.CreateSocket;
begin
  FSocket := synsock.Socket(PF_INET, Integer(SOCK_RAW), IPPROTO_RAW);
  FProtocol := IPPROTO_RAW;
  inherited CreateSocket;
end;

end.
