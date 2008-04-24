{==============================================================================|
| Project : Delphree - Synapse                                   | 005.002.000 |
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

{$Q-}
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
  synsock, SynaUtil, SynaCode, SynaSSL;

const
  cLocalhost = 'localhost';

type

  ESynapseError = class(Exception)
  public
    ErrorCode: Integer;
    ErrorMessage: string;
  end;

  THookSocketReason = (
    HR_ResolvingBegin,
    HR_ResolvingEnd,
    HR_SocketCreate,
    HR_SocketClose,
    HR_Bind,
    HR_Connect,
    HR_CanRead,
    HR_CanWrite,
    HR_Listen,
    HR_Accept,
    HR_ReadCount,
    HR_WriteCount
    );

  THookSocketStatus = procedure(Sender: TObject; Reason: THookSocketReason;
    const Value: string) of object;

  TBlockSocket = class(TObject)
  private
    FOnStatus: THookSocketStatus;
    FWsaData: TWSADATA;
    FLocalSin: TSockAddrIn;
    FRemoteSin: TSockAddrIn;
    FLastError: Integer;
    FBuffer: string;
    FRaiseExcept: Boolean;
    FNonBlockMode: Boolean;
    FMaxLineLength: Integer;
    FMaxBandwidth: Integer;
    FNextSend: Cardinal;
    function GetSizeRecvBuffer: Integer;
    procedure SetSizeRecvBuffer(Size: Integer);
    function GetSizeSendBuffer: Integer;
    procedure SetSizeSendBuffer(Size: Integer);
    procedure SetNonBlockMode(Value: Boolean);
  protected
    FSocket: TSocket;
    FProtocol: Integer;
    procedure CreateSocket; virtual;
    procedure SetSin(var Sin: TSockAddrIn; IP, Port: string);
    function GetSinIP(Sin: TSockAddrIn): string;
    function GetSinPort(Sin: TSockAddrIn): Integer;
    procedure DoStatus(Reason: THookSocketReason; const Value: string);
    procedure LimitBandwidth(Length: Integer);
  public
    constructor Create;
    constructor CreateAlternate(Stub: string);
    destructor Destroy; override;
    procedure CloseSocket; virtual;
    procedure Bind(IP, Port: string);
    procedure Connect(IP, Port: string); virtual;
    function SendBuffer(Buffer: Pointer; Length: Integer): Integer; virtual;
    procedure SendByte(Data: Byte); virtual;
    procedure SendString(const Data: string); virtual;
    function RecvBuffer(Buffer: Pointer; Length: Integer): Integer; virtual;
    function RecvBufferEx(Buffer: Pointer; Length: Integer;
      Timeout: Integer): Integer; virtual;
    function RecvByte(Timeout: Integer): Byte; virtual;
    function RecvString(Timeout: Integer): string; virtual;
    function RecvTerminated(Timeout: Integer; const Terminator: string): string; virtual;
    function RecvPacket(Timeout: Integer): string; virtual;
    function PeekBuffer(Buffer: Pointer; Length: Integer): Integer; virtual;
    function PeekByte(Timeout: Integer): Byte; virtual;
    function WaitingData: Integer;
    function WaitingDataEx: Integer;
    procedure SetLinger(Enable: Boolean; Linger: Integer);
    procedure GetSins;
    function SockCheck(SockResult: Integer): Integer;
    procedure ExceptCheck;
    function LocalName: string;
    procedure ResolveNameToIP(Name: string; IPList: TStrings);
    function ResolveName(Name: string): string;
    function ResolvePort(Port: string): Word;
    procedure SetRemoteSin(IP, Port: string);
    function GetLocalSinIP: string; virtual;
    function GetRemoteSinIP: string; virtual;
    function GetLocalSinPort: Integer; virtual;
    function GetRemoteSinPort: Integer; virtual;
    function CanRead(Timeout: Integer): Boolean;
    function CanReadEx(Timeout: Integer): Boolean;
    function CanWrite(Timeout: Integer): Boolean;
    function SendBufferTo(Buffer: Pointer; Length: Integer): Integer; virtual;
    function RecvBufferFrom(Buffer: Pointer; Length: Integer): Integer; virtual;
    function GroupCanRead(const SocketList: TList; Timeout: Integer;
      const CanReadList: TList): Boolean;
    function EnableReuse(Value: Boolean): Boolean;

    //See 'winsock2.txt' file in distribute package!
    function SetTimeout(Timeout: Integer): Boolean;
    function SetSendTimeout(Timeout: Integer): Boolean;
    function SetRecvTimeout(Timeout: Integer): Boolean;

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
    property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;
    property NonBlockMode: Boolean read FNonBlockMode Write SetNonBlockMode;
    property MaxLineLength: Integer read FMaxLineLength Write FMaxLineLength;
    property MaxBandwidth: Integer read FMaxBandwidth Write FMaxBandwidth;
  end;

  TSocksBlockSocket = class(TBlockSocket)
  protected
    FSocksIP: string;
    FSocksPort: string;
    FSocksTimeout: integer;
    FSocksUsername: string;
    FSocksPassword: string;
    FUsingSocks: Boolean;
    FSocksResolver: Boolean;
    FSocksLastError: integer;
    FSocksResponseIP: string;
    FSocksResponsePort: string;
    FSocksLocalIP: string;
    FSocksLocalPort: string;
    FSocksRemoteIP: string;
    FSocksRemotePort: string;
    FBypassFlag: Boolean;
    function SocksCode(IP, Port: string): string;
    function SocksDecode(Value: string): integer;
  public
    constructor Create;
    function SocksOpen: Boolean;
    function SocksRequest(Cmd: Byte; const IP, Port: string): Boolean;
    function SocksResponse: Boolean;
  published
    property SocksIP: string read FSocksIP write FSocksIP;
    property SocksPort: string read FSocksPort write FSocksPort;
    property SocksUsername: string read FSocksUsername write FSocksUsername;
    property SocksPassword: string read FSocksPassword write FSocksPassword;
    property UsingSocks: Boolean read FUsingSocks;
    property SocksResolver: Boolean read FSocksResolver write FSocksResolver;
    property SocksLastError: integer read FSocksLastError;
  end;

  TTCPBlockSocket = class(TSocksBlockSocket)
  protected
    FSslEnabled: Boolean;
    FSslBypass: Boolean;
    FSsl: PSSL;
    Fctx: PSSL_CTX;
    FHTTPTunnelIP: string;
    FHTTPTunnelPort: string;
    FHTTPTunnel: Boolean;
    FHTTPTunnelRemoteIP: string;
    FHTTPTunnelRemotePort: string;
    FHTTPTunnelUser: string;
    FHTTPTunnelPass: string;
    procedure SetSslEnabled(Value: Boolean);
    procedure SocksDoConnect(IP, Port: string);
    procedure HTTPTunnelDoConnect(IP, Port: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateSocket; override;
    procedure CloseSocket; override;
    procedure Listen;
    function Accept: TSocket;
    procedure Connect(IP, Port: string); override;
    procedure SSLDoConnect;
    procedure SSLDoShutdown;
    function SSLAcceptConnection(const PrivateKey, Certificate: string): Boolean;
    function GetLocalSinIP: string; override;
    function GetRemoteSinIP: string; override;
    function GetLocalSinPort: Integer; override;
    function GetRemoteSinPort: Integer; override;
    function SendBuffer(Buffer: Pointer; Length: Integer): Integer; override;
    function RecvBuffer(Buffer: Pointer; Length: Integer): Integer; override;
    function SSLGetSSLVersion: string;
    function SSLGetPeerSubject: string;
    function SSLGetPeerIssuer: string;
    function SSLGetPeerSubjectHash: Cardinal;
    function SSLGetPeerIssuerHash: Cardinal;
    function SSLGetPeerFingerprint: string;
  published
    property SSLEnabled: Boolean read FSslEnabled write SetSslEnabled;
    property SSLBypass: Boolean read FSslBypass write FSslBypass;
    property HTTPTunnelIP: string read FHTTPTunnelIP Write FHTTPTunnelIP;
    property HTTPTunnelPort: string read FHTTPTunnelPort Write FHTTPTunnelPort;
    property HTTPTunnel: Boolean read FHTTPTunnel;
    property HTTPTunnelUser: string read FHTTPTunnelUser Write FHTTPTunnelUser;
    property HTTPTunnelPass: string read FHTTPTunnelPass Write FHTTPTunnelPass;
  end;

  TUDPBlockSocket = class(TSocksBlockSocket)
  protected
    FSocksControlSock: TTCPBlockSocket;
    function UdpAssociation: Boolean;
  public
    destructor Destroy; override;
    procedure CreateSocket; override;
    function EnableBroadcast(Value: Boolean): Boolean;
    procedure Connect(IP, Port: string); override;
    function SendBuffer(Buffer: Pointer; Length: Integer): Integer; override;
    function RecvBuffer(Buffer: Pointer; Length: Integer): Integer; override;
    function SendBufferTo(Buffer: Pointer; Length: Integer): Integer; override;
    function RecvBufferFrom(Buffer: Pointer; Length: Integer): Integer; override;
    procedure AddMulticast(MCastIP:string);
    procedure DropMulticast(MCastIP:string);
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

type
  TMulticast = record
    MCastAddr : u_long;
    MCastIfc : u_long;
  end;

constructor TBlockSocket.Create;
var
  e: ESynapseError;
begin
  inherited Create;
  FRaiseExcept := False;
  FSocket := INVALID_SOCKET;
  FProtocol := IPPROTO_IP;
  FBuffer := '';
  FNonBlockMode := False;
  FMaxLineLength := 0;
  FMaxBandwidth := 0;
  FNextSend := 0;
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
  synsock.WSACleanup;
  DestroySocketInterface;
  inherited Destroy;
end;

procedure TBlockSocket.SetSin(var Sin: TSockAddrIn; IP, Port: string);
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  HostEnt: PHostEnt;
begin
  DoStatus(HR_ResolvingBegin, IP + ':' + Port);
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
  DoStatus(HR_ResolvingEnd, IP+':'+Port);
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
  DoStatus(HR_SocketCreate, '');
end;

procedure TBlockSocket.CloseSocket;
begin
  synsock.Shutdown(FSocket, 2);
  synsock.CloseSocket(FSocket);
  DoStatus(HR_SocketClose, '');
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
  DoStatus(HR_Bind, IP + ':' + Port);
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
  DoStatus(HR_Connect, IP + ':' + Port);
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

procedure TBlockSocket.LimitBandwidth(Length: Integer);
var
  x: Cardinal;
begin
  if FMaxBandwidth > 0 then
  begin
    x := FNextSend - GetTick;
    if x > 0 then
      Sleep(x);
    FNextSend := GetTick + Trunc((FMaxBandwidth / 1000) * Length);
  end;
end;

function TBlockSocket.SendBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  LimitBandwidth(Length);
  Result := synsock.Send(FSocket, Buffer^, Length, 0);
  SockCheck(Result);
  ExceptCheck;
  DoStatus(HR_WriteCount, IntToStr(Result));
end;

procedure TBlockSocket.SendByte(Data: Byte);
begin
  SendBuffer(@Data, 1);
end;

procedure TBlockSocket.SendString(const Data: string);
begin
  SendBuffer(PChar(Data), Length(Data));
end;

function TBlockSocket.RecvBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := synsock.Recv(FSocket, Buffer^, Length, 0);
  if Result = 0 then
    FLastError := WSAECONNRESET
  else
    SockCheck(Result);
  ExceptCheck;
  DoStatus(HR_ReadCount, IntToStr(Result));
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
        x := RecvBuffer(Pointer(st), l);
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

function TBlockSocket.RecvPacket(Timeout: Integer): string;
var
  x: integer;
  s: string;
begin
  Result := '';
  FLastError := 0;
  x := -1;
  if FBuffer <> '' then
  begin
    Result := FBuffer;
    FBuffer := '';
  end
  else
    if CanRead(Timeout) then
    begin
      x := WaitingData;
      if x > 0 then
      begin
        SetLength(s, x);
        x := RecvBuffer(Pointer(s), x);
        Result := Copy(s, 1, x);
      end;
    end
    else
      FLastError := WSAETIMEDOUT;
  ExceptCheck;
  if x = 0 then
    FLastError := WSAECONNRESET;
end;


function TBlockSocket.RecvByte(Timeout: Integer): Byte;
var
  s: String;
begin
  Result := 0;
  if CanRead(Timeout) then
  begin
    SetLength(s, 1);
    RecvBuffer(Pointer(s), 1);
    if s <> '' then
      Result := Ord(s[1]);
  end
  else
    FLastError := WSAETIMEDOUT;
  ExceptCheck;
end;

function TBlockSocket.RecvTerminated(Timeout: Integer; const Terminator: string): string;
const
  MaxSize = 1024;
var
  x: Integer;
  s: string;
  l: Integer;
begin
  s := '';
  l := Length(Terminator);
  Result := '';
  if l = 0 then
    Exit;
  FLastError := 0;
  repeat
    x := 0;
    if FBuffer = '' then
    begin
      FBuffer := RecvPacket(Timeout);
      if FLastError <> 0 then
        Break;
    end;
    s := s + FBuffer;
    FBuffer := '';
    x := Pos(Terminator, s);
    if x > 0 then
    begin
      FBuffer := Copy(s, x + l, Length(s) - x - l + 1);
      s := Copy(s, 1, x - 1);
    end;
    if (FMaxLineLength <> 0) and (Length(s) > FMaxLineLength) then
    begin
      FLastError := WSAENOBUFS;
      Break;
    end;
  until x > 0;
  Result := s;
  ExceptCheck;
end;

function TBlockSocket.RecvString(Timeout: Integer): string;
var
  s: string;
begin
  Result := '';
  s := RecvTerminated(Timeout, #13 + #10);
  if FLastError = 0 then
    Result := s;
end;

function TBlockSocket.PeekBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_PEEK);
  SockCheck(Result);
  ExceptCheck;
end;

function TBlockSocket.PeekByte(Timeout: Integer): Byte;
var
  s: string;
begin
  Result := 0;
  if CanRead(Timeout) then
  begin
    SetLength(s, 1);
    PeekBuffer(Pointer(s), 1);
    if s <> '' then
      Result := Ord(s[1]);
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
  if FRaiseExcept and (LastError <> 0) and (LastError <> WSAEINPROGRESS)
    and (LastError <> WSAEWOULDBLOCK) then
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

function TBlockSocket.WaitingDataEx: Integer;
begin
  if FBuffer <> '' then
    Result := Length(FBuffer)
  else
    Result := WaitingData;
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
    if IPList.Count = 0 then
      IPList.Add('0.0.0.0');
  end
  else
    IPList.Add(Name);
end;

function TBlockSocket.ResolveName(Name: string): string;
var
  l: TStringList;
begin
  l := TStringList.Create;
  try
    ResolveNameToIP(Name, l);
    Result := l[0];
  finally
    l.Free;
  end;
end;

function TBlockSocket.ResolvePort(Port: string): Word;
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
begin
  ProtoEnt := synsock.GetProtoByNumber(FProtocol);
  ServEnt := nil;
  if ProtoEnt <> nil then
    ServEnt := synsock.GetServByName(PChar(Port), ProtoEnt^.p_name);
  if ServEnt = nil then
    Result := synsock.htons(StrToIntDef(Port, 0))
  else
    Result := ServEnt^.s_port;
end;

procedure TBlockSocket.SetRemoteSin(IP, Port: string);
begin
  SetSin(FRemoteSin, IP, Port);
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
  if Result then
    DoStatus(HR_CanRead, '');
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
  if Result then
    DoStatus(HR_CanWrite, '');
end;

function TBlockSocket.CanReadEx(Timeout: Integer): Boolean;
begin
  if FBuffer <> '' then
    Result := True
  else
    Result := CanRead(Timeout);
end;

function TBlockSocket.SendBufferTo(Buffer: Pointer; Length: Integer): Integer;
var
  Len: Integer;
begin
  LimitBandwidth(Length);
  Len := SizeOf(FRemoteSin);
  Result := synsock.SendTo(FSocket, Buffer^, Length, 0, FRemoteSin, Len);
  SockCheck(Result);
  ExceptCheck;
  DoStatus(HR_WriteCount, IntToStr(Result));
end;

function TBlockSocket.RecvBufferFrom(Buffer: Pointer; Length: Integer): Integer;
var
  Len: Integer;
begin
  Len := SizeOf(FRemoteSin);
  Result := synsock.RecvFrom(FSocket, Buffer^, Length, 0, FRemoteSin, Len);
  SockCheck(Result);
  ExceptCheck;
  DoStatus(HR_ReadCount, IntToStr(Result));
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

procedure TBlockSocket.SetNonBlockMode(Value: Boolean);
var
  x: integer;
begin
  FNonBlockMode := Value;
  if Value then
    x := 1
  else
    x := 0;
  synsock.IoctlSocket(FSocket, FIONBIO, u_long(x));
end;

//See 'winsock2.txt' file in distribute package!
function TBlockSocket.SetTimeout(Timeout: Integer): Boolean;
begin
  Result := SetSendTimeout(Timeout) and SetRecvTimeout(Timeout);
end;

//See 'winsock2.txt' file in distribute package!
function TBlockSocket.SetSendTimeout(Timeout: Integer): Boolean;
begin
  Result := synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO,
    @Timeout, SizeOf(Timeout)) <> SOCKET_ERROR;
end;

//See 'winsock2.txt' file in distribute package!
function TBlockSocket.SetRecvTimeout(Timeout: Integer): Boolean;
begin
  Result := synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO,
    @Timeout, SizeOf(Timeout)) <> SOCKET_ERROR;
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

function TBlockSocket.EnableReuse(Value: Boolean): Boolean;
var
  Opt: Integer;
  Res: Integer;
begin
  opt := Ord(Value);
  Res := synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, @Opt, SizeOf(opt));
  SockCheck(Res);
  Result := res = 0;
  ExceptCheck;
end;

procedure TBlockSocket.DoStatus(Reason: THookSocketReason; const Value: string);
begin
  if assigned(OnStatus) then
    OnStatus(Self, Reason, Value);
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
      Result := 'Disconnect';
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

constructor TSocksBlockSocket.Create;
begin
  inherited Create;
  FSocksIP:= '';
  FSocksPort:= '1080';
  FSocksTimeout:= 300000;
  FSocksUsername:= '';
  FSocksPassword:= '';
  FUsingSocks := False;
  FSocksResolver := True;
  FSocksLastError := 0;
  FSocksResponseIP := '';
  FSocksResponsePort := '';
  FSocksLocalIP := '';
  FSocksLocalPort := '';
  FSocksRemoteIP := '';
  FSocksRemotePort := '';
  FBypassFlag := False;
end;

function TSocksBlockSocket.SocksOpen: boolean;
var
  Buf: string;
  n: integer;
begin
  Result := False;
  FUsingSocks := False;
  FBypassFlag := True;
  try
    if FSocksUsername = '' then
      Buf := #5 + #1 + #0
    else
      Buf := #5 + #2 + #2 +#0;
    SendString(Buf);
    Buf := RecvPacket(FSocksTimeout);
    FBuffer := Copy(Buf, 3, Length(buf) - 2);
    if Length(Buf) < 2 then
      Exit;
    if Buf[1] <> #5 then
      Exit;
    n := Ord(Buf[2]);
    case n of
      0: //not need authorisation
        ;
      2:
        begin
          Buf := #1 + char(Length(FSocksUsername)) + FSocksUsername
            + char(Length(FSocksPassword)) + FSocksPassword;
          SendString(Buf);
          Buf := RecvPacket(FSocksTimeout);
          FBuffer := Copy(Buf, 3, Length(buf) - 2);
          if Length(Buf) < 2 then
            Exit;
          if Buf[2] <> #0 then
            Exit;
        end;
    else
      Exit;
    end;
    FUsingSocks := True;
    Result := True;
  finally
    FBypassFlag := False;
  end;
end;

function TSocksBlockSocket.SocksRequest(Cmd: Byte;
  const IP, Port: string): Boolean;
var
  Buf: string;
begin
  Result := False;
  FBypassFlag := True;
  try
    Buf := #5 + char(Cmd) + #0 + SocksCode(IP, Port);
    SendString(Buf);
    Result := FLastError = 0;
  finally
    FBypassFlag := False;
  end;
end;

function TSocksBlockSocket.SocksResponse: Boolean;
var
  Buf: string;
  x: integer;
begin
  Result := False;
  FBypassFlag := True;
  try
    FSocksResponseIP := '';
    FSocksResponsePort := '';
    Buf := RecvPacket(FSocksTimeout);
    if FLastError <> 0 then
      Exit;
    if Length(Buf) < 5 then
      Exit;
    if Buf[1] <> #5 then
      Exit;
    FSocksLastError := Ord(Buf[2]);
    if FSocksLastError <> 0 then
      Exit;
    x := SocksDecode(Buf);
    FBuffer := Copy(Buf, x, Length(buf) - x + 1);
    Result := True;
  finally
    FBypassFlag := False;
  end;
end;

function TSocksBlockSocket.SocksCode(IP, Port: string): string;
begin
  if IsIP(IP) then
    Result := #1 + IPToID(IP)
  else
    if FSocksResolver then
      Result := #3 + char(Length(IP)) + IP
    else
      Result := #1 + IPToID(ResolveName(IP));
  Result := Result + CodeInt(synsock.htons(ResolvePort(Port)));
end;

function TSocksBlockSocket.SocksDecode(Value: string): integer;
var
  Atyp: Byte;
  y, n: integer;
  w: Word;
begin
  FSocksResponsePort := '0';
  Atyp := Ord(Value[4]);
  Result := 5;
  case Atyp of
    1:
      begin
        if Length(Value) < 10 then
          Exit;
        FSocksResponseIP := Format('%d.%d.%d.%d',
            [Ord(Value[5]), Ord(Value[6]), Ord(Value[7]), Ord(Value[8])]);
        Result := 9;
      end;
    3:
      begin
        y := Ord(Value[5]);
        if Length(Value) < (5 + y + 2) then
          Exit;
        for n := 6 to 6 + y do
          FSocksResponseIP := FSocksResponseIP + Value[n];
        Result := 5 + y +1;
      end;
  else
    Exit;
  end;
  w := DecodeInt(Value, Result);
  FSocksResponsePort := IntToStr(w);
  Result := Result + 2;
end;

{======================================================================}

destructor TUDPBlockSocket.Destroy;
begin
  if Assigned(FSocksControlSock) then
    FSocksControlSock.Free;
  inherited;
end;

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

procedure TUDPBlockSocket.Connect(IP, Port: string);
begin
  SetRemoteSin(IP, Port);
  FBuffer := '';
  DoStatus(HR_Connect, IP + ':' + Port);
end;

function TUDPBlockSocket.RecvBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := RecvBufferFrom(Buffer, Length);
end;

function TUDPBlockSocket.SendBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := SendBufferTo(Buffer, Length);
end;

function TUDPBlockSocket.UdpAssociation: Boolean;
var
  b: Boolean;
begin
  Result := True;
  FUsingSocks := False;
  if FSocksIP <> '' then
  begin
    Result := False;
    if not Assigned(FSocksControlSock) then
      FSocksControlSock := TTCPBlockSocket.Create;
    FSocksControlSock.CloseSocket;
    FSocksControlSock.CreateSocket;
    FSocksControlSock.Connect(FSocksIP, FSocksPort);
    if FSocksControlSock.LastError <> 0 then
      Exit;
    // if not assigned local port, assign it!
    if GetLocalSinPort = 0 then
      Bind(GetLocalSinIP, '0');
    GetSins;
    //open control TCP connection to SOCKS
    b := FSocksControlSock.SocksOpen;
    if b then
      b := FSocksControlSock.SocksRequest(3, GetLocalSinIP,
        IntToStr(GetLocalSinPort));
    if b then
      b := FSocksControlSock.SocksResponse;
    if not b and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FUsingSocks :=FSocksControlSock.UsingSocks;
    FSocksRemoteIP := FSocksControlSock.FSocksResponseIP;
    FSocksRemotePort := FSocksControlSock.FSocksResponsePort;
    Result := True;
  end;
end;

function TUDPBlockSocket.SendBufferTo(Buffer: Pointer; Length: Integer): Integer;
var
  SIp: string;
  SPort: integer;
  Buf: string;
begin
  UdpAssociation;
  if FUsingSocks then
  begin
    Sip := GetRemoteSinIp;
    SPort := GetRemoteSinPort;
    SetRemoteSin(FSocksRemoteIP, FSocksRemotePort);
    SetLength(Buf,Length);
    Move(Buffer^, PChar(Buf)^, Length);
    Buf := #0 + #0 + #0 + SocksCode(Sip, IntToStr(SPort)) + Buf;
    Result := inherited SendBufferTo(PChar(Buf), System.Length(buf));
    SetRemoteSin(Sip, IntToStr(SPort));
  end
  else
  begin
    Result := inherited SendBufferTo(Buffer, Length);
    GetSins;
  end;
end;

function TUDPBlockSocket.RecvBufferFrom(Buffer: Pointer; Length: Integer): Integer;
var
  Buf: string;
  x: integer;
begin
  Result := inherited RecvBufferFrom(Buffer, Length);
  if FUsingSocks then
  begin
    SetLength(Buf, Result);
    Move(Buffer^, PChar(Buf)^, Result);
    x := SocksDecode(Buf);
    Result := Result - x + 1;
    Buf := Copy(Buf, x, Result);
    Move(PChar(Buf)^, Buffer^, Result);
    SetRemoteSin(FSocksResponseIP, FSocksResponsePort);
  end;
end;

procedure TUDPBlockSocket.AddMulticast(MCastIP: string);
var
  Multicast: TMulticast;
begin
  Multicast.MCastAddr := synsock.inet_addr(PChar(MCastIP));
  Multicast.MCastIfc := u_long(INADDR_ANY);
  SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_ADD_MEMBERSHIP,
    pchar(@Multicast), SizeOf(Multicast)));
  ExceptCheck;
end;

procedure TUDPBlockSocket.DropMulticast(MCastIP: string);
var
  Multicast: TMulticast;
begin
  Multicast.MCastAddr := synsock.inet_addr(PChar(MCastIP));
  Multicast.MCastIfc := u_long(INADDR_ANY);
  SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_DROP_MEMBERSHIP,
    pchar(@Multicast), SizeOf(Multicast)));
  ExceptCheck;
end;

{======================================================================}

constructor TTCPBlockSocket.Create;
begin
  inherited Create;
  FSslEnabled := False;
  FSslBypass := False;
  FSsl := nil;
  Fctx := nil;
  FHTTPTunnelIP := '';
  FHTTPTunnelPort := '';
  FHTTPTunnel := False;
  FHTTPTunnelRemoteIP := '';
  FHTTPTunnelRemotePort := '';
  FHTTPTunnelUser := '';
  FHTTPTunnelPass := '';
end;

destructor TTCPBlockSocket.Destroy;
begin
  if FSslEnabled then
    SslEnabled := False;
  inherited;
end;

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
var
  b: Boolean;
  Sip,SPort: string;
begin
  if FSocksIP = '' then
  begin
    SockCheck(synsock.Listen(FSocket, SOMAXCONN));
    GetSins;
  end
  else
  begin
    Sip := GetLocalSinIP;
    if Sip = '0.0.0.0' then
      Sip := LocalName;
    SPort := IntToStr(GetLocalSinPort);
    inherited Connect(FSocksIP, FSocksPort);
    b := SocksOpen;
    if b then
      b := SocksRequest(2, Sip, SPort);
    if b then
      b := SocksResponse;
    if not b and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FSocksLocalIP := FSocksResponseIP;
    if FSocksLocalIP = '0.0.0.0' then
      FSocksLocalIP := FSocksIP;
    FSocksLocalPort := FSocksResponsePort;
    FSocksRemoteIP := '';
    FSocksRemotePort := '';
  end;
  ExceptCheck;
  DoStatus(HR_Listen, '');
end;

function TTCPBlockSocket.Accept: TSocket;
var
  Len: Integer;
begin
  if FUsingSocks then
  begin
    if not SocksResponse and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FSocksRemoteIP := FSocksResponseIP;
    FSocksRemotePort := FSocksResponsePort;
    Result := FSocket;
  end
  else
  begin
    Len := SizeOf(FRemoteSin);
    Result := synsock.Accept(FSocket, @FRemoteSin, @Len);
    SockCheck(Result);
  end;
  ExceptCheck;
  DoStatus(HR_Accept, '');
end;

procedure TTCPBlockSocket.Connect(IP, Port: string);
begin
  if FSocksIP <> '' then
    SocksDoConnect(IP, Port)
  else
    if FHTTPTunnelIP <> '' then
      HTTPTunnelDoConnect(IP, Port)
    else
      inherited Connect(IP, Port);
  if FSslEnabled then
    SSLDoConnect;
end;

procedure TTCPBlockSocket.SocksDoConnect(IP, Port: string);
var
  b: Boolean;
begin
  inherited Connect(FSocksIP, FSocksPort);
  b := SocksOpen;
  if b then
    b := SocksRequest(1, IP, Port);
  if b then
    b := SocksResponse;
  if not b and (FLastError = 0) then
    FLastError := WSASYSNOTREADY;
  FSocksLocalIP := FSocksResponseIP;
  FSocksLocalPort := FSocksResponsePort;
  FSocksRemoteIP := IP;
  FSocksRemotePort := Port;
  ExceptCheck;
  DoStatus(HR_Connect, IP + ':' + Port);
end;

procedure TTCPBlockSocket.HTTPTunnelDoConnect(IP, Port: string);
var
  s: string;
begin
  try
    FBypassFlag := True;
    inherited Connect(FHTTPTunnelIP, FHTTPTunnelPort);
    FHTTPTunnel := False;
    SendString('CONNECT ' + IP + ':' + Port + 'HTTP/1.0' + #$0d + #$0a);
    if FHTTPTunnelUser <> '' then
    Sendstring('Proxy-Authorization: Basic ' +
      EncodeBase64(FHTTPTunnelUser + ':' + FHTTPTunnelPass) + #$0d + #$0a);
    SendString(#$0d + #$0a);
    repeat
      s := RecvString(30000);
      if FLastError <> 0 then
        Break;
      if (Pos('HTTP/', s) = 1) and (Length(s) > 11) then
        FHTTPTunnel := s[10] = '2';
    until s = '';
    if (FLasterror = 0) and not FHTTPTunnel then
      FLastError := WSASYSNOTREADY;
    FHTTPTunnelRemoteIP := IP;
    FHTTPTunnelRemotePort := Port;
  finally
    FBypassFlag := False;
  end;
  ExceptCheck;
end;

procedure TTCPBlockSocket.SSLDoConnect;
begin
  FLastError := 0;
  if not FSSLEnabled then
    SSLEnabled := True;
  if sslsetfd(FSsl, FSocket) < 0 then
    FLastError := WSASYSNOTREADY;
  if (FLastError = 0) then
    if sslconnect(FSsl) < 0 then
      FLastError := WSASYSNOTREADY;
  ExceptCheck;
end;

procedure TTCPBlockSocket.SSLDoShutdown;
begin
  FLastError := 0;
  if sslshutdown(FSsl) < 0 then
    FLastError := WSASYSNOTREADY;
  ExceptCheck;
  SSLEnabled := False;
end;

function TTCPBlockSocket.GetLocalSinIP: string;
begin
  if FUsingSocks then
    Result := FSocksLocalIP
  else
    Result := inherited GetLocalSinIP;
end;

function TTCPBlockSocket.GetRemoteSinIP: string;
begin
  if FUsingSocks then
    Result := FSocksRemoteIP
  else
    if FHTTPTunnel then
      Result := FHTTPTunnelRemoteIP
    else
      Result := inherited GetRemoteSinIP;
end;

function TTCPBlockSocket.GetLocalSinPort: Integer;
begin
  if FUsingSocks then
    Result := StrToIntDef(FSocksLocalPort, 0)
  else
    Result := inherited GetLocalSinPort;
end;

function TTCPBlockSocket.GetRemoteSinPort: Integer;
begin
  if FUsingSocks then
    Result := StrToIntDef(FSocksRemotePort, 0)
  else
    if FHTTPTunnel then
      Result := StrToIntDef(FHTTPTunnelRemotePort, 0)
    else
      Result := inherited GetRemoteSinPort;
end;

procedure TTCPBlockSocket.SetSslEnabled(Value: Boolean);
begin
  if Value <> FSslEnabled then
    if Value then
    begin
      if InitSSLInterface then
      begin
        SslLoadErrorStrings;
        SslLibraryInit;
        Fctx := nil;
        Fctx := SslCtxNew(SslMethodV23);
        Fssl := nil;
        Fssl := SslNew(Fctx);
        FSslEnabled := True;
      end
      else DestroySSLInterface;
    end
    else
    begin
      sslfree(Fssl);
      SslCtxFree(Fctx);
      DestroySSLInterface;
      FSslEnabled := False;
    end;
end;

function TTCPBlockSocket.RecvBuffer(Buffer: Pointer; Length: Integer): Integer;
var
  err: integer;
begin
  if FSslEnabled and not(FSslBypass) and not(FBypassFlag) then
  begin
    FLastError := 0;
    repeat
      Result := SslRead(FSsl, Buffer, Length);
      err := SslGetError(FSsl, Result);
    until (err <> SSL_ERROR_WANT_READ) or (err <> SSL_ERROR_WANT_WRITE);
    if err = SSL_ERROR_ZERO_RETURN then
      Result := 0
    else
      if (err <> 0) then
        FLastError := WSASYSNOTREADY;
    ExceptCheck;
    DoStatus(HR_ReadCount, IntToStr(Result));
  end
  else
    Result := inherited RecvBuffer(Buffer, Length);
end;

function TTCPBlockSocket.SendBuffer(Buffer: Pointer; Length: Integer): Integer;
var
  err: integer;
begin
  if FSslEnabled and not(FSslBypass) and not(FBypassFlag) then
  begin
    FLastError := 0;
    repeat
      Result := SslWrite(FSsl, Buffer, Length);
      err := SslGetError(FSsl, Result);
    until (err <> SSL_ERROR_WANT_READ) or (err <> SSL_ERROR_WANT_WRITE);
    if err = SSL_ERROR_ZERO_RETURN then
      Result := 0
    else
      if (err <> 0) then
        FLastError := WSASYSNOTREADY;
    ExceptCheck;
    DoStatus(HR_WriteCount, IntToStr(Result));
  end
  else
    Result := inherited SendBuffer(Buffer, Length);
end;

function TTCPBlockSocket.SSLAcceptConnection(const PrivateKey, Certificate: string): Boolean;
begin
  Result := False;
  FLastError := 0;
  if SslCtxUseCertificateFile(FCtx, PChar(Certificate), 1) < 0 then
    FLastError := WSASYSNOTREADY;
  if (FLastError = 0) then
    if SslCtxUsePrivateKeyFile(FCtx, PChar(PrivateKey), 1) < 0 then
      FLastError := WSASYSNOTREADY;
  if (FLastError = 0) then
    if sslsetfd(FSsl, FSocket) < 0 then
      FLastError := WSASYSNOTREADY;
  if (FLastError = 0) then
    if sslAccept(FSsl) < 0 then
      FLastError := WSASYSNOTREADY;
  ExceptCheck;
  Result := FLastError = 0;
end;

function TTCPBlockSocket.SSLGetSSLVersion: string;
begin
  Result := SSlGetVersion(FSsl);
end;

function TTCPBlockSocket.SSLGetPeerSubject: string;
var
  cert: PX509;
  s: string;
begin
  cert := SSLGetPeerCertificate(Fssl);
  setlength(s, 4096);
  Result := SslX509NameOneline(SslX509GetSubjectName(cert), PChar(s), length(s));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerIssuer: string;
var
  cert: PX509;
  s: string;
begin
  cert := SSLGetPeerCertificate(Fssl);
  setlength(s, 4096);
  Result := SslX509NameOneline(SslX509GetIssuerName(cert), PChar(s), length(s));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerSubjectHash: Cardinal;
var
  cert: PX509;
begin
  cert := SSLGetPeerCertificate(Fssl);
  Result := SslX509NameHash(SslX509GetSubjectName(cert));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerIssuerHash: Cardinal;
var
  cert: PX509;
begin
  cert := SSLGetPeerCertificate(Fssl);
  Result := SslX509NameHash(SslX509GetIssuerName(cert));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerFingerprint: string;
var
  cert: PX509;
  x: integer;
begin
  cert := SSLGetPeerCertificate(Fssl);
  setlength(Result, EVP_MAX_MD_SIZE);
  SslX509Digest(cert, SslEvpMd5, PChar(Result), @x);
  SetLength(Result, x);
  SslX509Free(cert);
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
