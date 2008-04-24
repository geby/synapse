{==============================================================================|
| Project : Ararat Synapse                                       | 007.009.001 |
|==============================================================================|
| Content: Library base                                                        |
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
| Portions created by Lukas Gebauer are Copyright (c)1999-2003.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}
{
Special thanks to Gregor Ibic <gregor.ibic@intelicom.si>
 (Intelicom d.o.o., http://www.intelicom.si)
 for good inspiration about SSL programming.
}

{$DEFINE ONCEWINSOCK}
{Note about define ONCEWINSOCK:
If you remove this compiler directive, then socket interface is loaded and
initialized on constructor of TBlockSocket class for each socket separately.
Socket interface is used only if your need it.

If you leave this directive here, then socket interface is loaded and
initialized only once at start of your program! It boost performace on high
count of created and destroyed sockets. It eliminate possible small resource
leak on Windows systems too.
}

//{$DEFINE RAISEEXCEPT}
{When you enable this define, then is Raiseexcept property is on by default
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
{$ENDIF}
{$Q-}
{$H+}

unit blcksock;

interface

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  {$IFDEF FPC}
  synafpc,
  {$ENDIF}
  Libc,
{$ELSE}
  Windows,
{$ENDIF}
  synsock, synautil, synacode, synassl;

const

  SynapseRelease = '32';

  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';
  CR = #$0d;
  LF = #$0a;
  CRLF = CR + LF;
  c64k = 65536;


type

  ESynapseError = class(Exception)
  private
    FErrorCode: Integer;
    FErrorMessage: string;
  published
    property ErrorCode: Integer read FErrorCode Write FErrorCode;
    property ErrorMessage: string read FErrorMessage Write FErrorMessage;
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
    HR_WriteCount,
    HR_Wait,
    HR_Error
    );

  THookSocketStatus = procedure(Sender: TObject; Reason: THookSocketReason;
    const Value: string) of object;

  THookDataFilter = procedure(Sender: TObject; var Value: string) of object;

  THookCreateSocket = procedure(Sender: TObject) of object;

  TSocketFamily = (
    SF_Any,
    SF_IP4,
    SF_IP6
    );

  TSocksType = (
    ST_Socks5,
    ST_Socks4
    );

  TSSLType = (
    LT_SSLv2,
    LT_SSLv3,
    LT_TLSv1,
    LT_all
    );

  TSynaOptionType = (
    SOT_Linger,
    SOT_RecvBuff,
    SOT_SendBuff,
    SOT_NonBlock,
    SOT_RecvTimeout,
    SOT_SendTimeout,
    SOT_Reuse,
    SOT_TTL,
    SOT_Broadcast,
    SOT_MulticastTTL,
    SOT_MulticastLoop
    );

  TSynaOption = record
    Option: TSynaOptionType;
    Enabled: Boolean;
    Value: Integer;
  end;
  PSynaOption = ^TSynaOption;

  TBlockSocket = class(TObject)
  private
    FOnStatus: THookSocketStatus;
    FOnReadFilter: THookDataFilter;
    FOnWriteFilter: THookDataFilter;
    FOnCreateSocket: THookCreateSocket;
    FWsaData: TWSADATA;
    FLocalSin: TVarSin;
    FRemoteSin: TVarSin;
    FBuffer: string;
    FRaiseExcept: Boolean;
    FNonBlockMode: Boolean;
    FMaxLineLength: Integer;
    FMaxSendBandwidth: Integer;
    FNextSend: ULong;
    FMaxRecvBandwidth: Integer;
    FNextRecv: ULong;
    FConvertLineEnd: Boolean;
    FLastCR: Boolean;
    FLastLF: Boolean;
    FBinded: Boolean;
    FFamily: TSocketFamily;
    FFamilySave: TSocketFamily;
    FIP6used: Boolean;
    FPreferIP4: Boolean;
    FDelayedOptions: TList;
    FInterPacketTimeout: Boolean;
    FFDSet: TFDSet;
    FRecvCounter: Integer;
    FSendCounter: Integer;
    function GetSizeRecvBuffer: Integer;
    procedure SetSizeRecvBuffer(Size: Integer);
    function GetSizeSendBuffer: Integer;
    procedure SetSizeSendBuffer(Size: Integer);
    procedure SetNonBlockMode(Value: Boolean);
    procedure SetTTL(TTL: integer);
    function GetTTL:integer;
    function IsNewApi: Boolean;
    procedure SetFamily(Value: TSocketFamily); virtual;
    procedure SetSocket(Value: TSocket); virtual;
  protected
    FSocket: TSocket;
    FLastError: Integer;
    FLastErrorDesc: string;
    procedure SetDelayedOption(Value: TSynaOption);
    procedure DelayedOption(Value: TSynaOption);
    procedure ProcessDelayedOptions;
    procedure InternalCreateSocket(Sin: TVarSin);
    procedure SetSin(var Sin: TVarSin; IP, Port: string);
    function GetSinIP(Sin: TVarSin): string;
    function GetSinPort(Sin: TVarSin): Integer;
    procedure DoStatus(Reason: THookSocketReason; const Value: string);
    procedure DoReadFilter(Buffer: Pointer; var Length: Integer);
    procedure DoWriteFilter(Buffer: Pointer; var Length: Integer);
    procedure DoCreateSocket;
    procedure LimitBandwidth(Length: Integer; MaxB: integer; var Next: ULong);
    procedure SetBandwidth(Value: Integer);
  public
    constructor Create;
    constructor CreateAlternate(Stub: string);
    destructor Destroy; override;
    procedure CreateSocket;
    procedure CreateSocketByName(const Value: String);
    procedure CloseSocket; virtual;
    procedure AbortSocket;
    procedure Bind(IP, Port: string);
    procedure Connect(IP, Port: string); virtual;
    function SendBuffer(Buffer: Pointer; Length: Integer): Integer; virtual;
    procedure SendByte(Data: Byte); virtual;
    procedure SendString(const Data: string); virtual;
    procedure SendBlock(const Data: string); virtual;
    procedure SendStream(const Stream: TStream); virtual;
    function RecvBuffer(Buffer: Pointer; Length: Integer): Integer; virtual;
    function RecvBufferEx(Buffer: Pointer; Length: Integer;
      Timeout: Integer): Integer; virtual;
    function RecvBufferStr(Length: Integer; Timeout: Integer): String; virtual;
    function RecvByte(Timeout: Integer): Byte; virtual;
    function RecvString(Timeout: Integer): string; virtual;
    function RecvTerminated(Timeout: Integer; const Terminator: string): string; virtual;
    function RecvPacket(Timeout: Integer): string; virtual;
    function RecvBlock(Timeout: Integer): string; virtual;
    procedure RecvStream(const Stream: TStream; Timeout: Integer); virtual;
    function PeekBuffer(Buffer: Pointer; Length: Integer): Integer; virtual;
    function PeekByte(Timeout: Integer): Byte; virtual;
    function WaitingData: Integer; virtual;
    function WaitingDataEx: Integer;
    procedure Purge;
    procedure SetLinger(Enable: Boolean; Linger: Integer);
    procedure GetSinLocal;
    procedure GetSinRemote;
    procedure GetSins;
    function SockCheck(SockResult: Integer): Integer;
    procedure ExceptCheck;
    function LocalName: string;
    procedure ResolveNameToIP(Name: string; IPList: TStrings);
    function ResolveName(Name: string): string;
    function ResolveIPToName(IP: string): string;
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
    procedure EnableReuse(Value: Boolean);
    procedure SetTimeout(Timeout: Integer);
    procedure SetSendTimeout(Timeout: Integer);
    procedure SetRecvTimeout(Timeout: Integer);

    function StrToIP6(const value: string): TSockAddrIn6;
    function IP6ToStr(const value: TSockAddrIn6): string;

    function GetSocketType: integer; Virtual;
    function GetSocketProtocol: integer; Virtual;

    property WSAData: TWSADATA read FWsaData;
    property LocalSin: TVarSin read FLocalSin write FLocalSin;
    property RemoteSin: TVarSin read FRemoteSin write FRemoteSin;
  published
    class function GetErrorDesc(ErrorCode: Integer): string;
    property Socket: TSocket read FSocket write SetSocket;
    property LastError: Integer read FLastError;
    property LastErrorDesc: string read FLastErrorDesc;
    property LineBuffer: string read FBuffer write FBuffer;
    property RaiseExcept: Boolean read FRaiseExcept write FRaiseExcept;
    property SizeRecvBuffer: Integer read GetSizeRecvBuffer write SetSizeRecvBuffer;
    property SizeSendBuffer: Integer read GetSizeSendBuffer write SetSizeSendBuffer;
    property NonBlockMode: Boolean read FNonBlockMode Write SetNonBlockMode;
    property MaxLineLength: Integer read FMaxLineLength Write FMaxLineLength;
    property MaxSendBandwidth: Integer read FMaxSendBandwidth Write FMaxSendBandwidth;
    property MaxRecvBandwidth: Integer read FMaxRecvBandwidth Write FMaxRecvBandwidth;
    property MaxBandwidth: Integer Write SetBandwidth;
    property ConvertLineEnd: Boolean read FConvertLineEnd Write FConvertLineEnd;
    property TTL: Integer read GetTTL Write SetTTL;
    property Family: TSocketFamily read FFamily Write SetFamily;
    property PreferIP4: Boolean read FPreferIP4 Write FPreferIP4;
    property IP6used: Boolean read FIP6used;
    property InterPacketTimeout: Boolean read FInterPacketTimeout Write FInterPacketTimeout;
    property RecvCounter: Integer read FRecvCounter;
    property SendCounter: Integer read FSendCounter;
    property OnStatus: THookSocketStatus read FOnStatus write FOnStatus;
    property OnReadFilter: THookDataFilter read FOnReadFilter write FOnReadFilter;
    property OnWriteFilter: THookDataFilter read FOnWriteFilter write FOnWriteFilter;
    property OnCreateSocket: THookCreateSocket read FOnCreateSocket write FOnCreateSocket;
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
    FSocksType: TSocksType;
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
    property SocksTimeout: integer read FSocksTimeout write FSocksTimeout;
    property UsingSocks: Boolean read FUsingSocks;
    property SocksResolver: Boolean read FSocksResolver write FSocksResolver;
    property SocksLastError: integer read FSocksLastError;
    property SocksType: TSocksType read FSocksType write FSocksType;
  end;

  TTCPBlockSocket = class(TSocksBlockSocket)
  protected
    FSslEnabled: Boolean;
    FSslBypass: Boolean;
    FSsl: PSSL;
    Fctx: PSSL_CTX;
    FSSLPassword: string;
    FSSLCiphers: string;
    FSSLCertificateFile: string;
    FSSLPrivateKeyFile: string;
    FSSLCertCAFile: string;
    FSSLLastError: integer;
    FSSLLastErrorDesc: string;
    FSSLverifyCert: Boolean;
    FSSLType: TSSLType;
    FHTTPTunnelIP: string;
    FHTTPTunnelPort: string;
    FHTTPTunnel: Boolean;
    FHTTPTunnelRemoteIP: string;
    FHTTPTunnelRemotePort: string;
    FHTTPTunnelUser: string;
    FHTTPTunnelPass: string;
    FHTTPTunnelTimeout: integer;
    procedure SetSslEnabled(Value: Boolean);
    function SetSslKeys: boolean;
    procedure SocksDoConnect(IP, Port: string);
    procedure HTTPTunnelDoConnect(IP, Port: string);
    function GetSSLLoaded: Boolean;
  public
    constructor Create;
    procedure CloseSocket; override;
    function WaitingData: Integer; override;
    procedure Listen; virtual;
    function Accept: TSocket;
    procedure Connect(IP, Port: string); override;
    procedure SSLDoConnect;
    procedure SSLDoShutdown;
    function SSLAcceptConnection: Boolean;
    function GetLocalSinIP: string; override;
    function GetRemoteSinIP: string; override;
    function GetLocalSinPort: Integer; override;
    function GetRemoteSinPort: Integer; override;
    function SendBuffer(Buffer: Pointer; Length: Integer): Integer; override;
    function RecvBuffer(Buffer: Pointer; Length: Integer): Integer; override;
    function SSLGetSSLVersion: string;
    function SSLGetPeerSubject: string;
    function SSLGetPeerIssuer: string;
    function SSLGetPeerName: string;
    function SSLGetPeerSubjectHash: Cardinal;
    function SSLGetPeerIssuerHash: Cardinal;
    function SSLGetPeerFingerprint: string;
    function SSLGetCertInfo: string;
    function SSLGetCipherName: string;
    function SSLGetCipherBits: integer;
    function SSLGetCipherAlgBits: integer;
    function SSLGetVerifyCert: integer;
    function SSLCheck: Boolean;
    function GetSocketType: integer; override;
    function GetSocketProtocol: integer; override;
  published
    property SSLLoaded: Boolean read GetSslLoaded;
    property SSLEnabled: Boolean read FSslEnabled write SetSslEnabled;
    property SSLType: TSSLType read FSSLType write FSSLType;
    property SSLBypass: Boolean read FSslBypass write FSslBypass;
    property SSLPassword: string read FSSLPassword write FSSLPassword;
    property SSLCiphers: string read FSSLCiphers write FSSLCiphers;
    property SSLCertificateFile: string read FSSLCertificateFile write FSSLCertificateFile;
    property SSLPrivateKeyFile: string read FSSLPrivateKeyFile write FSSLPrivateKeyFile;
    property SSLCertCAFile: string read FSSLCertCAFile write FSSLCertCAFile;
    property SSLLastError: integer read FSSLLastError;
    property SSLLastErrorDesc: string read FSSLLastErrorDesc;
    property SSLverifyCert: Boolean read FSSLverifyCert write FSSLverifyCert;
    property HTTPTunnelIP: string read FHTTPTunnelIP Write FHTTPTunnelIP;
    property HTTPTunnelPort: string read FHTTPTunnelPort Write FHTTPTunnelPort;
    property HTTPTunnel: Boolean read FHTTPTunnel;
    property HTTPTunnelUser: string read FHTTPTunnelUser Write FHTTPTunnelUser;
    property HTTPTunnelPass: string read FHTTPTunnelPass Write FHTTPTunnelPass;
    property HTTPTunnelTimeout: integer read FHTTPTunnelTimeout Write FHTTPTunnelTimeout;
  end;

  TDgramBlockSocket = class(TSocksBlockSocket)
  public
    procedure Connect(IP, Port: string); override;
    function SendBuffer(Buffer: Pointer; Length: Integer): Integer; override;
    function RecvBuffer(Buffer: Pointer; Length: Integer): Integer; override;
  end;

  TUDPBlockSocket = class(TDgramBlockSocket)
  protected
    FSocksControlSock: TTCPBlockSocket;
    function UdpAssociation: Boolean;
    procedure SetMulticastTTL(TTL: integer);
    function GetMulticastTTL:integer;
  public
    destructor Destroy; override;
    procedure EnableBroadcast(Value: Boolean);
    function SendBufferTo(Buffer: Pointer; Length: Integer): Integer; override;
    function RecvBufferFrom(Buffer: Pointer; Length: Integer): Integer; override;
    procedure AddMulticast(MCastIP:string);
    procedure DropMulticast(MCastIP:string);
    procedure EnableMulticastLoop(Value: Boolean);
    function GetSocketType: integer; override;
    function GetSocketProtocol: integer; override;
  published
    property MulticastTTL: Integer read GetMulticastTTL Write SetMulticastTTL;
  end;

  TICMPBlockSocket = class(TDgramBlockSocket)
  public
    function GetSocketType: integer; override;
    function GetSocketProtocol: integer; override;
  end;

  TRAWBlockSocket = class(TBlockSocket)
  public
    function GetSocketType: integer; override;
    function GetSocketProtocol: integer; override;
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

  TSynaClient = Class(TObject)
  protected
    FTargetHost: string;
    FTargetPort: string;
    FIPInterface: string;
    FTimeout: integer;
  public
    constructor Create;
  published
    property TargetHost: string read FTargetHost Write FTargetHost;
    property TargetPort: string read FTargetPort Write FTargetPort;
    property IPInterface: string read FIPInterface Write FIPInterface;
    property Timeout: integer read FTimeout Write FTimeout;
  end;

implementation

{$IFDEF ONCEWINSOCK}
var
  WsaDataOnce: TWSADATA;
  e: ESynapseError;
{$ENDIF}


constructor TBlockSocket.Create;
begin
  CreateAlternate('');
end;

constructor TBlockSocket.CreateAlternate(Stub: string);
{$IFNDEF ONCEWINSOCK}
var
  e: ESynapseError;
{$ENDIF}
begin
  inherited Create;
  FDelayedOptions := TList.Create;
  FRaiseExcept := False;
{$IFDEF RAISEEXCEPT}
  FRaiseExcept := True;
{$ENDIF}
  FSocket := INVALID_SOCKET;
  FBuffer := '';
  FLastCR := False;
  FLastLF := False;
  FBinded := False;
  FNonBlockMode := False;
  FMaxLineLength := 0;
  FMaxSendBandwidth := 0;
  FNextSend := 0;
  FMaxRecvBandwidth := 0;
  FNextRecv := 0;
  FConvertLineEnd := False;
  FFamily := SF_Any;
  FFamilySave := SF_Any;
  FIP6used := False;
  FPreferIP4 := True;
  FInterPacketTimeout := True;
  FRecvCounter := 0;
  FSendCounter := 0;
{$IFDEF ONCEWINSOCK}
  FWsaData := WsaDataOnce;
{$ELSE}
  if Stub = '' then
    Stub := DLLStackName;
  if not InitSocketInterface(Stub) then
  begin
    e := ESynapseError.Create('Error loading Socket interface (' + Stub + ')!');
    e.ErrorCode := 0;
    e.ErrorMessage := 'Error loading Socket interface (' + Stub + ')!';
    raise e;
  end;
  SockCheck(synsock.WSAStartup(WinsockLevel, FWsaData));
  ExceptCheck;
{$ENDIF}
end;

destructor TBlockSocket.Destroy;
var
  n: integer;
  p: PSynaOption;
begin
  CloseSocket;
{$IFNDEF ONCEWINSOCK}
  synsock.WSACleanup;
  DestroySocketInterface;
{$ENDIF}
  for n := FDelayedOptions.Count - 1 downto 0 do
    begin
      p := PSynaOption(FDelayedOptions[n]);
      Dispose(p);
    end;
  FDelayedOptions.Free;
  inherited Destroy;
end;

function TBlockSocket.IsNewApi: Boolean;
begin
  Result := SockEnhancedApi;
  if not Result then
    Result := (FFamily = SF_ip6) and SockWship6Api;
end;

procedure TBlockSocket.SetDelayedOption(Value: TSynaOption);
var
  li: TLinger;
  x: integer;
begin
  case value.Option of
    SOT_Linger:
      begin
        li.l_onoff := Ord(Value.Enabled);
        li.l_linger := Value.Value div 1000;
        synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_LINGER, @li, SizeOf(li));
      end;
    SOT_RecvBuff:
      begin
        synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF,
          @Value.Value, SizeOf(Value.Value));
      end;
    SOT_SendBuff:
      begin
        synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF,
          @Value.Value, SizeOf(Value.Value));
      end;
    SOT_NonBlock:
      begin
        FNonBlockMode := Value.Enabled;
        x := Ord(FNonBlockMode);
        synsock.IoctlSocket(FSocket, FIONBIO, u_long(x));
      end;
    SOT_RecvTimeout:
      begin
        synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO,
          @Value.Value, SizeOf(Value.Value));
      end;
    SOT_SendTimeout:
      begin
        synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO,
          @Value.Value, SizeOf(Value.Value));
      end;
    SOT_Reuse:
      begin
        x := Ord(Value.Enabled);
        synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, @x, SizeOf(x));
      end;
    SOT_TTL:
      begin
        if FIP6Used then
          synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_UNICAST_HOPS,
            @Value.Value, SizeOf(Value.Value))
        else
          synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_TTL,
            @Value.Value, SizeOf(Value.Value));
      end;
    SOT_Broadcast:
      begin
//#todo1 broadcasty na IP6
        x := Ord(Value.Enabled);
        synsock.SetSockOpt(FSocket, SOL_SOCKET, SO_BROADCAST, @x, SizeOf(x));
      end;
    SOT_MulticastTTL:
      begin
        if FIP6Used then
          synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_MULTICAST_HOPS,
            @Value.Value, SizeOf(Value.Value))
        else
          synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_MULTICAST_TTL,
            @Value.Value, SizeOf(Value.Value));
      end;
    SOT_MulticastLoop:
      begin
        x := Ord(Value.Enabled);
        if FIP6Used then
          synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_MULTICAST_LOOP, @x, SizeOf(x))
        else
          synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_MULTICAST_LOOP, @x, SizeOf(x));
      end;
  end;
end;

procedure TBlockSocket.DelayedOption(Value: TSynaOption);
var
  d: PSynaOption;
begin
  if FSocket = INVALID_SOCKET then
  begin
    new(d);
    d^ := Value;
    FDelayedOptions.Insert(0, d);
  end
  else
    SetDelayedOption(Value);
end;

procedure TBlockSocket.ProcessDelayedOptions;
var
  n: integer;
  d: PSynaOption;
begin
  for n := FDelayedOptions.Count - 1 downto 0 do
  begin
    d := FDelayedOptions[n];
    SetDelayedOption(d^);
    Dispose(d);
  end;
  FDelayedOptions.Clear;
end;

procedure TBlockSocket.SetSin(var Sin: TVarSin; IP, Port: string);
type
  pu_long = ^u_long;
var
  ProtoEnt: PProtoEnt;
  ServEnt: PServEnt;
  HostEnt: PHostEnt;
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  AddrNext: PAddrInfo;
  r: integer;
  Sin4, Sin6: TVarSin;
begin
  DoStatus(HR_ResolvingBegin, IP + ':' + Port);
  FillChar(Sin, Sizeof(Sin), 0);
  //for prelimitary IP6 support try fake Family by given IP
  if SockWship6Api and (FFamily = SF_Any) then
  begin
    if IsIP(IP) then
      FFamily := SF_IP4
    else
      if IsIP6(IP) then
        FFamily := SF_IP6
      else
        if FPreferIP4 then
          FFamily := SF_IP4
        else
          FFamily := SF_IP6;
  end;
  if not IsNewApi then
  begin
    SynSockCS.Enter;
    try
      Sin.sin_family := AF_INET;
      ProtoEnt := synsock.GetProtoByNumber(GetSocketProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := synsock.GetServByName(PChar(Port), ProtoEnt^.p_name);
      if ServEnt = nil then
        Sin.sin_port := synsock.htons(StrToIntDef(Port, 0))
      else
        Sin.sin_port := ServEnt^.s_port;
      if IP = cBroadcast then
        Sin.sin_addr.s_addr := u_long(INADDR_BROADCAST)
      else
      begin
        Sin.sin_addr.s_addr := synsock.inet_addr(PChar(IP));
        if Sin.sin_addr.s_addr = u_long(INADDR_NONE) then
        begin
          HostEnt := synsock.GetHostByName(PChar(IP));
          if HostEnt <> nil then
            Sin.sin_addr.S_addr := u_long(Pu_long(HostEnt^.h_addr_list^)^);
        end;
      end;
    finally
      SynSockCS.Leave;
    end;
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Sin4, Sizeof(Sin4), 0);
      FillChar(Sin6, Sizeof(Sin6), 0);
      FillChar(Hints, Sizeof(Hints), 0);
      //if socket exists, then use their type, else use users selection
      if FSocket = INVALID_SOCKET then
        case FFamily of
          SF_Any: Hints.ai_family := AF_UNSPEC;
          SF_IP4: Hints.ai_family := AF_INET;
          SF_IP6: Hints.ai_family := AF_INET6;
        end
      else
        if FIP6Used then
          Hints.ai_family := AF_INET6
        else
          Hints.ai_family := AF_INET;
      Hints.ai_socktype := GetSocketType;
      Hints.ai_protocol := GetSocketprotocol;
      if Hints.ai_socktype = SOCK_RAW then
      begin
        Hints.ai_socktype := 0;
        Hints.ai_protocol := 0;
        r := synsock.GetAddrInfo(PChar(IP), nil, @Hints, Addr);
      end
      else
      begin
        if IP = cAnyHost then
        begin
          Hints.ai_flags := AI_PASSIVE;
          r := synsock.GetAddrInfo(nil, PChar(Port), @Hints, Addr);
        end
        else
          if IP = cLocalhost then
          begin
            r := synsock.GetAddrInfo(nil, PChar(Port), @Hints, Addr);
          end
          else
          begin
            r := synsock.GetAddrInfo(PChar(IP), PChar(Port), @Hints, Addr);
          end;
      end;
      if r = 0 then
      begin
        AddrNext := Addr;
        while not (AddrNext = nil) do
        begin
          if not(Sin4.sin_family = AF_INET) and (AddrNext^.ai_family = AF_INET) then
            Move(AddrNext^.ai_addr^, Sin4, AddrNext^.ai_addrlen);
          if not(Sin6.sin_family = AF_INET6) and (AddrNext^.ai_family = AF_INET6) then
            Move(AddrNext^.ai_addr^, Sin6, AddrNext^.ai_addrlen);
          AddrNext := AddrNext^.ai_next;
        end;
        if (Sin4.sin_family = AF_INET) and (Sin6.sin_family = AF_INET6) then
        begin
          if FPreferIP4 then
            Sin := Sin4
          else
            Sin := Sin6;
        end
        else
        begin
          sin := sin4;
          if (Sin6.sin_family = AF_INET6) then
            sin := sin6;
        end;
      end;
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;
  DoStatus(HR_ResolvingEnd, IP + ':' + Port);
end;

function TBlockSocket.GetSinIP(Sin: TVarSin): string;
var
  p: PChar;
  host, serv: string;
  hostlen, servlen: integer;
  r: integer;
begin
  Result := '';
  if not IsNewApi then
  begin
    p := synsock.inet_ntoa(Sin.sin_addr);
    if p <> nil then
      Result := p;
  end
  else
  begin
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    setlength(host, hostlen);
    setlength(serv, servlen);
    r := getnameinfo(@sin, SizeOfVarSin(sin), PChar(host), hostlen,
      PChar(serv), servlen, NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      Result := PChar(host);
  end;
end;

function TBlockSocket.GetSinPort(Sin: TVarSin): Integer;
begin
  if (Sin.sin_family = AF_INET6) then
    Result := synsock.ntohs(Sin.sin6_port)
  else
    Result := synsock.ntohs(Sin.sin_port);
end;

procedure TBlockSocket.CreateSocket;
var
  sin: TVarSin;
begin
  //dummy for SF_Any Family mode
  FLastError := 0;
  if (FFamily <> SF_Any) and (FSocket = INVALID_SOCKET) then
  begin
    FillChar(Sin, Sizeof(Sin), 0);
    if FFamily = SF_IP6 then
      sin.sin_family := AF_INET6
    else
      sin.sin_family := AF_INET;
    InternalCreateSocket(Sin);
  end;
end;

procedure TBlockSocket.CreateSocketByName(const Value: String);
var
  sin: TVarSin;
begin
  FLastError := 0;
  if FSocket = INVALID_SOCKET then
  begin
    SetSin(sin, value, '0');
    InternalCreateSocket(Sin);
  end;
end;

procedure TBlockSocket.InternalCreateSocket(Sin: TVarSin);
begin
  FRecvCounter := 0;
  FSendCounter := 0;
  FLastError := 0;
  if FSocket = INVALID_SOCKET then
  begin
    FBuffer := '';
    FBinded := False;
    FIP6Used := Sin.sin_family = AF_INET6;
    FSocket := synsock.Socket(Sin.sin_family, GetSocketType, GetSocketProtocol);
    if FSocket = INVALID_SOCKET then
      FLastError := synsock.WSAGetLastError;
    FD_ZERO(FFDSet);
    FD_SET(FSocket, FFDSet);
    ExceptCheck;
    if FIP6used then
      DoStatus(HR_SocketCreate, 'IPv6')
    else
      DoStatus(HR_SocketCreate, 'IPv4');
    ProcessDelayedOptions;
    DoCreateSocket;
  end;
end;

procedure TBlockSocket.CloseSocket;
begin
  AbortSocket;
end;

procedure TBlockSocket.AbortSocket;
var
  n: integer;
  p: PSynaOption;
begin
  synsock.CloseSocket(FSocket);
  FSocket := INVALID_SOCKET;
  for n := FDelayedOptions.Count - 1 downto 0 do
    begin
      p := PSynaOption(FDelayedOptions[n]);
      Dispose(p);
    end;
  FDelayedOptions.Clear;
  FFamily := FFamilySave;
  FLastError := 0;
  DoStatus(HR_SocketClose, '');
end;

procedure TBlockSocket.Bind(IP, Port: string);
var
  Sin: TVarSin;
begin
  FLastError := 0;
  if (FSocket <> INVALID_SOCKET)
    or not((FFamily = SF_ANY) and (IP = cAnyHost) and (Port = cAnyPort)) then
  begin
    SetSin(Sin, IP, Port);
    if FSocket = INVALID_SOCKET then
      InternalCreateSocket(Sin);
    SockCheck(synsock.Bind(FSocket, @Sin, SizeOfVarSin(Sin)));
    GetSinLocal;
    FBuffer := '';
    FBinded := True;
    ExceptCheck;
    DoStatus(HR_Bind, IP + ':' + Port);
  end;
end;

procedure TBlockSocket.Connect(IP, Port: string);
var
  Sin: TVarSin;
begin
  SetSin(Sin, IP, Port);
  if FSocket = INVALID_SOCKET then
    InternalCreateSocket(Sin);
  SockCheck(synsock.Connect(FSocket, @Sin, SizeOfVarSin(Sin)));
  GetSins;
  FBuffer := '';
  FLastCR := False;
  FLastLF := False;
  ExceptCheck;
  DoStatus(HR_Connect, IP + ':' + Port);
end;

procedure TBlockSocket.GetSinLocal;
var
  Len: Integer;
begin
  FillChar(FLocalSin, Sizeof(FLocalSin), 0);
  Len := SizeOf(FLocalSin);
  synsock.GetSockName(FSocket, @FLocalSin, Len);
end;

procedure TBlockSocket.GetSinRemote;
var
  Len: Integer;
begin
  FillChar(FRemoteSin, Sizeof(FRemoteSin), 0);
  Len := SizeOf(FRemoteSin);
  synsock.GetPeerName(FSocket, @FRemoteSin, Len);
end;

procedure TBlockSocket.GetSins;
begin
  GetSinLocal;
  GetSinRemote;
end;

procedure TBlockSocket.SetBandwidth(Value: Integer);
begin
  MaxSendBandwidth := Value;
  MaxRecvBandwidth := Value;
end;

procedure TBlockSocket.LimitBandwidth(Length: Integer; MaxB: integer; var Next: ULong);
var
  x: ULong;
  y: ULong;
begin
  if MaxB > 0 then
  begin
    y := GetTick;
    if Next > y then
    begin
      x := Next - y;
      if x > 0 then
      begin
        DoStatus(HR_Wait, IntToStr(x));
        sleep(x);
      end;
    end;
    Next := GetTick + Trunc((Length / MaxB) * 1000);
  end;
end;

function TBlockSocket.SendBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  LimitBandwidth(Length, FMaxSendBandwidth, FNextsend);
  DoWriteFilter(Buffer, Length);
  Result := synsock.Send(FSocket, Buffer^, Length, MSG_NOSIGNAL);
  SockCheck(Result);
  ExceptCheck;
  Inc(FSendCounter, Result);
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

procedure TBlockSocket.SendBlock(const Data: string);
var
  x: integer;
begin
  x := Length(Data);
  SendBuffer(@x, SizeOf(x));
  SendString(Data);
end;

procedure TBlockSocket.SendStream(const Stream: TStream);
var
  si: integer;
  x, y, yr: integer;
  s: string;
begin
  si := Stream.Size - Stream.Position;
  SendBuffer(@si, SizeOf(si));
  x := 0;
  while x < si do
  begin
    y := si - x;
    if y > c64k then
      y := c64k;
    Setlength(s, c64k);
    yr := Stream.read(s, y);
    if yr > 0 then
    begin
      SetLength(s, yr);
      SendString(s);
      Inc(x, yr);
    end
    else
      break;
  end;
end;

function TBlockSocket.RecvBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_NOSIGNAL);
  if Result = 0 then
    FLastError := WSAECONNRESET
  else
    SockCheck(Result);
  ExceptCheck;
  Inc(FRecvCounter, Result);
  DoStatus(HR_ReadCount, IntToStr(Result));
  DoReadFilter(Buffer, Result);
end;

function TBlockSocket.RecvBufferEx(Buffer: Pointer; Length: Integer;
  Timeout: Integer): Integer;
var
  s: string;
  rl, l: integer;
  ti: ULong;
begin
  FLastError := 0;
  rl := 0;
  repeat
    ti := GetTick;
    s := RecvPacket(Timeout);
    l := System.Length(s);
    if (rl + l) > Length then
      l := Length - rl;
    Move(Pointer(s)^, IncPoint(Buffer, rl)^, l);
    rl := rl + l;
    if FLastError <> 0 then
      Break;
    if rl >= Length then
      Break;
    if not FInterPacketTimeout then
    begin
      Timeout := Timeout - integer(TickDelta(ti, GetTick));
      if Timeout <= 0 then
      begin
        FLastError := WSAETIMEDOUT;
        Break;
      end;
    end;
  until False;
  delete(s, 1, l);
  FBuffer := s;
  Result := rl;
end;

function TBlockSocket.RecvBufferStr(Length: Integer; Timeout: Integer): string;
var
  x: integer;
begin
  Result := '';
  if Length > 0 then
  begin
    Setlength(Result, Length);
    x := RecvBufferEx(PChar(Result), Length , Timeout);
    if FLastError = 0 then
      SetLength(Result, x)
    else
      Result := '';
  end;
end;

function TBlockSocket.RecvPacket(Timeout: Integer): string;
var
  x: integer;
begin
  Result := '';
  FLastError := 0;
  if FBuffer <> '' then
  begin
    Result := FBuffer;
    FBuffer := '';
  end
  else
  begin
    //not drain CPU on large downloads...
    Sleep(0);
    x := WaitingData;
    if x > 0 then
    begin
      SetLength(Result, x);
      x := RecvBuffer(Pointer(Result), x);
      if x >= 0 then
        SetLength(Result, x);
    end
    else
    begin
      if CanRead(Timeout) then
      begin
        x := WaitingData;
        if x = 0 then
          FLastError := WSAECONNRESET;
        if x > 0 then
        begin
          SetLength(Result, x);
          x := RecvBuffer(Pointer(Result), x);
          if x >= 0 then
            SetLength(Result, x);
        end;
      end
      else
        FLastError := WSAETIMEDOUT;
    end;
  end;
  ExceptCheck;
end;


function TBlockSocket.RecvByte(Timeout: Integer): Byte;
begin
  Result := 0;
  FLastError := 0;
  if FBuffer = '' then
    FBuffer := RecvPacket(Timeout);
  if (FLastError = 0) and (FBuffer <> '') then
  begin
    Result := Ord(FBuffer[1]);
    System.Delete(FBuffer, 1, 1);
  end;
  ExceptCheck;
end;

function TBlockSocket.RecvTerminated(Timeout: Integer; const Terminator: string): string;
var
  x: Integer;
  s: string;
  l: Integer;
  CorCRLF: Boolean;
  t: string;
  tl: integer;
  ti: ULong;
begin
  FLastError := 0;
  Result := '';
  l := system.Length(Terminator);
  if l = 0 then
    Exit;
  tl := l;
  CorCRLF := FConvertLineEnd and (Terminator = CRLF);
  s := '';
  x := 0;
  repeat
    //get rest of FBuffer or incomming new data...
    ti := GetTick;
    s := s + RecvPacket(Timeout);
    if FLastError <> 0 then
      Break;
    x := 0;
    if Length(s) > 0 then
      if CorCRLF then
      begin
        if FLastCR and (s[1] = LF) then
          Delete(s, 1, 1);
        if FLastLF and (s[1] = CR) then
          Delete(s, 1, 1);
        FLastCR := False;
        FLastLF := False;
        t := '';
        x := PosCRLF(s, t);
        tl := system.Length(t);
        if t = CR then
          FLastCR := True;
        if t = LF then
          FLastLF := True;
      end
      else
      begin
        x := pos(Terminator, s);
        tl := l;
      end;
    if (FMaxLineLength <> 0) and (system.Length(s) > FMaxLineLength) then
    begin
      FLastError := WSAENOBUFS;
      Break;
    end;
    if x > 0 then
      Break;
    if not FInterPacketTimeout then
    begin
      Timeout := Timeout - integer(TickDelta(ti, GetTick));
      if Timeout <= 0 then
      begin
        FLastError := WSAETIMEDOUT;
        Break;
      end;
    end;
  until False;
  if x > 0 then
  begin
    Result := Copy(s, 1, x - 1);
    System.Delete(s, 1, x + tl - 1);
  end;
  FBuffer := s;
  ExceptCheck;
end;

function TBlockSocket.RecvString(Timeout: Integer): string;
var
  s: string;
begin
  Result := '';
  s := RecvTerminated(Timeout, CRLF);
  if FLastError = 0 then
    Result := s;
end;

function TBlockSocket.RecvBlock(Timeout: Integer): string;
var
  x: integer;
begin
  Result := '';
  RecvBufferEx(@x, SizeOf(x), Timeout);
  if FLastError = 0 then
    Result := RecvBufferStr(x, Timeout);
end;

procedure TBlockSocket.RecvStream(const Stream: TStream; Timeout: Integer);
var
  x: integer;
  s: string;
  n: integer;
begin
  RecvBufferEx(@x, SizeOf(x), Timeout);
  if FLastError = 0 then
  begin
    for n := 1 to (x div c64k) do
    begin
      s := RecvBufferStr(c64k, Timeout);
      if FLastError <> 0 then
        Exit;
      Stream.Write(s, c64k);
    end;
    n := x mod c64k;
    if n > 0 then
    begin
      s := RecvBufferStr(n, Timeout);
      if FLastError <> 0 then
        Exit;
      Stream.Write(s, n);
    end;
  end;
end;

function TBlockSocket.PeekBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := synsock.Recv(FSocket, Buffer^, Length, MSG_PEEK + MSG_NOSIGNAL);
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
  FLastErrorDesc := '';
  if SockResult = integer(SOCKET_ERROR) then
  begin
    Result := synsock.WSAGetLastError;
    FLastErrorDesc := GetErrorDesc(Result);
  end
  else
    Result := 0;
  FLastError := Result;
end;

procedure TBlockSocket.ExceptCheck;
var
  e: ESynapseError;
begin
  FLastErrorDesc := GetErrorDesc(FLastError);
  if (LastError <> 0) and (LastError <> WSAEINPROGRESS)
    and (LastError <> WSAEWOULDBLOCK) then
  begin
    DoStatus(HR_Error, IntToStr(FLastError) + ',' + FLastErrorDesc);
    if FRaiseExcept then
    begin
      e := ESynapseError.CreateFmt('Synapse TCP/IP Socket error %d: %s',
        [FLastError, FLastErrorDesc]);
      e.ErrorCode := FLastError;
      e.ErrorMessage := FLastErrorDesc;
      raise e;
    end;
  end;
end;

function TBlockSocket.WaitingData: Integer;
var
  x: Integer;
begin
  Result := 0;
  if synsock.IoctlSocket(FSocket, FIONREAD, u_long(x)) = 0 then
    Result := x;
end;

function TBlockSocket.WaitingDataEx: Integer;
begin
  if FBuffer <> '' then
    Result := Length(FBuffer)
  else
    Result := WaitingData;
end;

procedure TBlockSocket.Purge;
begin
  repeat
    RecvPacket(0);
  until FLastError <> 0;
  FLastError := 0;
end;

procedure TBlockSocket.SetLinger(Enable: Boolean; Linger: Integer);
var
  d: TSynaOption;
begin
  d.Option := SOT_Linger;
  d.Enabled := Enable;
  d.Value := Linger;
  DelayedOption(d);
end;

function TBlockSocket.LocalName: string;
var
  s: string;
begin
  Result := '';
  setlength(s, 255);
  synsock.GetHostName(pchar(s), Length(s) - 1);
  Result := Pchar(s);
  if Result = '' then
    Result := '127.0.0.1';
end;

procedure TBlockSocket.ResolveNameToIP(Name: string; IPList: TStrings);
type
  TaPInAddr = array[0..250] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  AddrNext: PAddrInfo;
  r: integer;
  host, serv: string;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IP: u_long;
  PAdrPtr: PaPInAddr;
  i: Integer;
  s: string;
  InAddr: TInAddr;
begin
  IPList.Clear;
  if not IsNewApi then
  begin
    IP := synsock.inet_addr(PChar(Name));
    if IP = u_long(INADDR_NONE) then
    begin
      SynSockCS.Enter;
      try
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
      finally
        SynSockCS.Leave;
      end;
    end
    else
      IPList.Add(Name);
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_UNSPEC;
      Hints.ai_socktype := GetSocketType;
      Hints.ai_protocol := GetSocketprotocol;
      Hints.ai_flags := 0;
      r := synsock.GetAddrInfo(PChar(Name), nil, @Hints, Addr);
      if r = 0 then
      begin
        AddrNext := Addr;
        while not(AddrNext = nil) do
        begin
          if not(((FFamily = SF_IP6) and (AddrNext^.ai_family = AF_INET))
            or ((FFamily = SF_IP4) and (AddrNext^.ai_family = AF_INET6))) then
          begin
            hostlen := NI_MAXHOST;
            servlen := NI_MAXSERV;
            setlength(host, hostlen);
            setlength(serv, servlen);
            r := getnameinfo(AddrNext^.ai_addr, AddrNext^.ai_addrlen,
              PChar(host), hostlen, PChar(serv), servlen,
              NI_NUMERICHOST + NI_NUMERICSERV);
            if r = 0 then
            begin
              host := PChar(host);
              IPList.Add(host);
            end;
          end;
          AddrNext := AddrNext^.ai_next;
        end;
      end;
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;
  if IPList.Count = 0 then
    IPList.Add(cAnyHost);
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
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  r: integer;
begin
  Result := 0;
  if not IsNewApi then
  begin
    SynSockCS.Enter;
    try
      ProtoEnt := synsock.GetProtoByNumber(GetSocketProtocol);
      ServEnt := nil;
      if ProtoEnt <> nil then
        ServEnt := synsock.GetServByName(PChar(Port), ProtoEnt^.p_name);
      if ServEnt = nil then
        Result := StrToIntDef(Port, 0)
      else
        Result := synsock.htons(ServEnt^.s_port);
    finally
      SynSockCS.Leave;
    end;
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_UNSPEC;
      Hints.ai_socktype := GetSocketType;
      Hints.ai_protocol := GetSocketprotocol;
      Hints.ai_flags := AI_PASSIVE;
      r := synsock.GetAddrInfo(nil, PChar(Port), @Hints, Addr);
      if r = 0 then
      begin
        if Addr^.ai_family = AF_INET then
          Result := synsock.htons(Addr^.ai_addr^.sin_port);
        if Addr^.ai_family = AF_INET6 then
          Result := synsock.htons(PSockAddrIn6(Addr^.ai_addr)^.sin6_port);
      end;
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;
end;

function TBlockSocket.ResolveIPToName(IP: string): string;
var
  Hints: TAddrInfo;
  Addr: PAddrInfo;
  r: integer;
  host, serv: string;
  hostlen, servlen: integer;
  RemoteHost: PHostEnt;
  IPn: u_long;
begin
  Result := IP;
  if not IsNewApi then
  begin
    if not IsIP(IP) then
      IP := ResolveName(IP);
    IPn := synsock.inet_addr(PChar(IP));
    if IPn <> u_long(INADDR_NONE) then
    begin
      SynSockCS.Enter;
      try
        RemoteHost := GetHostByAddr(@IPn, SizeOf(IPn), AF_INET);
        if RemoteHost <> nil then
          Result := RemoteHost^.h_name;
      finally
        SynSockCS.Leave;
      end;
    end;
  end
  else
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_UNSPEC;
      Hints.ai_socktype := GetSocketType;
      Hints.ai_protocol := GetSocketprotocol;
      Hints.ai_flags := 0;
      r := synsock.GetAddrInfo(PChar(IP), nil, @Hints, Addr);
      if r = 0 then
      begin
        hostlen := NI_MAXHOST;
        servlen := NI_MAXSERV;
        setlength(host, hostlen);
        setlength(serv, servlen);
        r := getnameinfo(Addr^.ai_addr, Addr^.ai_addrlen,
          PChar(host), hostlen, PChar(serv), servlen,
          NI_NUMERICSERV);
        if r = 0 then
          Result := PChar(host);
      end;
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;
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
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
  FDSet: TFDSet;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FDSet := FFdSet;
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
  TimeVal: PTimeVal;
  TimeV: TTimeVal;
  x: Integer;
  FDSet: TFDSet;
begin
  TimeV.tv_usec := (Timeout mod 1000) * 1000;
  TimeV.tv_sec := Timeout div 1000;
  TimeVal := @TimeV;
  if Timeout = -1 then
    TimeVal := nil;
  FDSet := FFdSet;
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
  LimitBandwidth(Length, FMaxSendBandwidth, FNextsend);
  Len := SizeOfVarSin(FRemoteSin);
  Result := synsock.SendTo(FSocket, Buffer^, Length, 0, @FRemoteSin, Len);
  SockCheck(Result);
  ExceptCheck;
  Inc(FSendCounter, Result);
  DoStatus(HR_WriteCount, IntToStr(Result));
end;

function TBlockSocket.RecvBufferFrom(Buffer: Pointer; Length: Integer): Integer;
var
  Len: Integer;
begin
  LimitBandwidth(Length, FMaxRecvBandwidth, FNextRecv);
  Len := SizeOf(FRemoteSin);
  Result := synsock.RecvFrom(FSocket, Buffer^, Length, 0, @FRemoteSin, Len);
  SockCheck(Result);
  ExceptCheck;
  Inc(FRecvCounter, Result);
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
var
  d: TSynaOption;
begin
  d.Option := SOT_RecvBuff;
  d.Value := Size;
  DelayedOption(d);
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
var
  d: TSynaOption;
begin
  d.Option := SOT_SendBuff;
  d.Value := Size;
  DelayedOption(d);
end;

procedure TBlockSocket.SetNonBlockMode(Value: Boolean);
var
  d: TSynaOption;
begin
  d.Option := SOT_nonblock;
  d.Enabled := Value;
  DelayedOption(d);
end;

procedure TBlockSocket.SetTimeout(Timeout: Integer);
begin
  SetSendTimeout(Timeout);
  SetRecvTimeout(Timeout);
end;

procedure TBlockSocket.SetSendTimeout(Timeout: Integer);
var
  d: TSynaOption;
begin
  d.Option := SOT_sendtimeout;
  d.Value := Timeout;
  DelayedOption(d);
end;

procedure TBlockSocket.SetRecvTimeout(Timeout: Integer);
var
  d: TSynaOption;
begin
  d.Option := SOT_recvtimeout;
  d.Value := Timeout;
  DelayedOption(d);
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

procedure TBlockSocket.EnableReuse(Value: Boolean);
var
  d: TSynaOption;
begin
  d.Option := SOT_reuse;
  d.Enabled := Value;
  DelayedOption(d);
end;

procedure TBlockSocket.SetTTL(TTL: integer);
var
  d: TSynaOption;
begin
  d.Option := SOT_TTL;
  d.Value := TTL;
  DelayedOption(d);
end;

function TBlockSocket.GetTTL:integer;
var
  l: Integer;
begin
  l := SizeOf(Result);
  if FIP6Used then
    synsock.GetSockOpt(FSocket, IPPROTO_IPV6, IPV6_UNICAST_HOPS, @Result, l)
  else
    synsock.GetSockOpt(FSocket, IPPROTO_IP, IP_TTL, @Result, l);
end;

procedure TBlockSocket.SetFamily(Value: TSocketFamily);
begin
  FFamily := Value;
  FFamilySave := Value;
end;

procedure TBlockSocket.SetSocket(Value: TSocket);
begin
  FRecvCounter := 0;
  FSendCounter := 0;
  FSocket := Value;
  FD_ZERO(FFDSet);
  FD_SET(FSocket, FFDSet);
  GetSins;
  FIP6Used := FRemoteSin.sin_family = AF_INET6;
end;

function TBlockSocket.StrToIP6(const value: string): TSockAddrIn6;
var
  addr: PAddrInfo;
  hints: TAddrInfo;
  r: integer;
begin
  FillChar(Result, Sizeof(Result), 0);
  if SockEnhancedApi or SockWship6Api then
  begin
    Addr := nil;
    try
      FillChar(Hints, Sizeof(Hints), 0);
      Hints.ai_family := AF_INET6;
      Hints.ai_flags := AI_NUMERICHOST;
      r := synsock.GetAddrInfo(PChar(value), nil, @Hints, Addr);
      if r = 0 then
        if (Addr^.ai_family = AF_INET6) then
            Move(Addr^.ai_addr^, Result, SizeOf(Result));
    finally
      if Assigned(Addr) then
        synsock.FreeAddrInfo(Addr);
    end;
  end;
end;

function TBlockSocket.IP6ToStr(const value: TSockAddrIn6): string;
var
  host, serv: string;
  hostlen, servlen: integer;
  r: integer;
begin
  Result := '';
  if SockEnhancedApi or SockWship6Api then
  begin
    hostlen := NI_MAXHOST;
    servlen := NI_MAXSERV;
    setlength(host, hostlen);
    setlength(serv, servlen);
    r := getnameinfo(@Value, SizeOf(value), PChar(host), hostlen,
      PChar(serv), servlen, NI_NUMERICHOST + NI_NUMERICSERV);
    if r = 0 then
      Result := PChar(host);
  end;
end;

function TBlockSocket.GetSocketType: integer;
begin
  Result := 0;
end;

function TBlockSocket.GetSocketProtocol: integer;
begin
  Result := IPPROTO_IP
end;

procedure TBlockSocket.DoStatus(Reason: THookSocketReason; const Value: string);
begin
  if assigned(OnStatus) then
    OnStatus(Self, Reason, Value);
end;

procedure TBlockSocket.DoReadFilter(Buffer: Pointer; var Length: Integer);
var
  s: string;
begin
  if assigned(OnReadFilter) then
    if Length > 0 then
      begin
        SetLength(s, Length);
        Move(Buffer^, Pointer(s)^, Length);
        OnReadFilter(Self, s);
        if System.Length(s) > Length then
          SetLength(s, Length);
        Length := System.Length(s);
        Move(Pointer(s)^, Buffer^, Length);
      end;
end;

procedure TBlockSocket.DoWriteFilter(Buffer: Pointer; var Length: Integer);
var
  s: string;
begin
  if assigned(OnWriteFilter) then
    if Length > 0 then
      begin
        SetLength(s, Length);
        Move(Buffer^, Pointer(s)^, Length);
        OnWriteFilter(Self, s);
        if System.Length(s) > Length then
          SetLength(s, Length);
        Length := System.Length(s);
        Move(Pointer(s)^, Buffer^, Length);
      end;
end;

procedure TBlockSocket.DoCreateSocket;
begin
  if assigned(OnCreateSocket) then
    OnCreateSocket(Self);
end;

class function TBlockSocket.GetErrorDesc(ErrorCode: Integer): string;
begin
  case ErrorCode of
    0:
      Result := '';
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
  FSocksTimeout:= 60000;
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
  FSocksType := ST_Socks5;
end;

function TSocksBlockSocket.SocksOpen: boolean;
var
  Buf: string;
  n: integer;
begin
  Result := False;
  FUsingSocks := False;
  if FSocksType <> ST_Socks5 then
  begin
    FUsingSocks := True;
    Result := True;
  end
  else
  begin
    FBypassFlag := True;
    try
      if FSocksUsername = '' then
        Buf := #5 + #1 + #0
      else
        Buf := #5 + #2 + #2 +#0;
      SendString(Buf);
      Buf := RecvBufferStr(2, FSocksTimeout);
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
            Buf := RecvBufferStr(2, FSocksTimeout);
            if Length(Buf) < 2 then
              Exit;
            if Buf[2] <> #0 then
              Exit;
          end;
      else
        //other authorisation is not supported!
        Exit;
      end;
      FUsingSocks := True;
      Result := True;
    finally
      FBypassFlag := False;
    end;
  end;
end;

function TSocksBlockSocket.SocksRequest(Cmd: Byte;
  const IP, Port: string): Boolean;
var
  Buf: string;
begin
  FBypassFlag := True;
  try
    if FSocksType <> ST_Socks5 then
      Buf := #4 + char(Cmd) + SocksCode(IP, Port)
    else
      Buf := #5 + char(Cmd) + #0 + SocksCode(IP, Port);
    SendString(Buf);
    Result := FLastError = 0;
  finally
    FBypassFlag := False;
  end;
end;

function TSocksBlockSocket.SocksResponse: Boolean;
var
  Buf, s: string;
  x: integer;
begin
  Result := False;
  FBypassFlag := True;
  try
    FSocksResponseIP := '';
    FSocksResponsePort := '';
    FSocksLastError := -1;
    if FSocksType <> ST_Socks5 then
    begin
      Buf := RecvBufferStr(8, FSocksTimeout);
      if FLastError <> 0 then
        Exit;
      if Buf[1] <> #0 then
        Exit;
      FSocksLastError := Ord(Buf[2]);
    end
    else
    begin
      Buf := RecvBufferStr(4, FSocksTimeout);
      if FLastError <> 0 then
        Exit;
      if Buf[1] <> #5 then
        Exit;
      case Ord(Buf[4]) of
        1:
          s := RecvBufferStr(4, FSocksTimeout);
        3:
          begin
            x := RecvByte(FSocksTimeout);
            if FLastError <> 0 then
              Exit;
            s := char(x) + RecvBufferStr(x, FSocksTimeout);
          end;
        4:
          s := RecvBufferStr(16, FSocksTimeout);
      else
        Exit;
      end;
      Buf := Buf + s + RecvBufferStr(2, FSocksTimeout);
      if FLastError <> 0 then
        Exit;
      FSocksLastError := Ord(Buf[2]);
    end;
    if ((FSocksLastError <> 0) and (FSocksLastError <> 90)) then
      Exit;
    SocksDecode(Buf);
    Result := True;
  finally
    FBypassFlag := False;
  end;
end;

function TSocksBlockSocket.SocksCode(IP, Port: string): string;
var
  s: string;
  ip6: TSockAddrIn6;
begin
  if FSocksType <> ST_Socks5 then
  begin
    Result := CodeInt(ResolvePort(Port));
    if not FSocksResolver then
      IP := ResolveName(IP);
    if IsIP(IP) then
    begin
      Result := Result + IPToID(IP);
      Result := Result + FSocksUsername + #0;
    end
    else
    begin
      Result := Result + IPToID('0.0.0.1');
      Result := Result + FSocksUsername + #0;
      Result := Result + IP + #0;
    end;
  end
  else
  begin
    if not FSocksResolver then
      IP := ResolveName(IP);
    if IsIP(IP) then
      Result := #1 + IPToID(IP)
    else
      if IsIP6(IP) then
      begin
        ip6 := StrToIP6(IP);
        setlength(s, 16);
        s[1] := ip6.sin6_addr.S_un_b.s_b1;
        s[2] := ip6.sin6_addr.S_un_b.s_b2;
        s[3] := ip6.sin6_addr.S_un_b.s_b3;
        s[4] := ip6.sin6_addr.S_un_b.s_b4;
        s[5] := ip6.sin6_addr.S_un_b.s_b5;
        s[6] := ip6.sin6_addr.S_un_b.s_b6;
        s[7] := ip6.sin6_addr.S_un_b.s_b7;
        s[8] := ip6.sin6_addr.S_un_b.s_b8;
        s[9] := ip6.sin6_addr.S_un_b.s_b9;
        s[10] := ip6.sin6_addr.S_un_b.s_b10;
        s[11] := ip6.sin6_addr.S_un_b.s_b11;
        s[12] := ip6.sin6_addr.S_un_b.s_b12;
        s[13] := ip6.sin6_addr.S_un_b.s_b13;
        s[14] := ip6.sin6_addr.S_un_b.s_b14;
        s[15] := ip6.sin6_addr.S_un_b.s_b15;
        s[16] := ip6.sin6_addr.S_un_b.s_b16;
        Result := #4 + s;
      end
      else
        Result := #3 + char(Length(IP)) + IP;
    Result := Result + CodeInt(ResolvePort(Port));
  end;
end;

function TSocksBlockSocket.SocksDecode(Value: string): integer;
var
  Atyp: Byte;
  y, n: integer;
  w: Word;
  ip6: TSockAddrIn6;
begin
  FSocksResponsePort := '0';
  Result := 0;
  if FSocksType <> ST_Socks5 then
  begin
    if Length(Value) < 8 then
      Exit;
    Result := 3;
    w := DecodeInt(Value, Result);
    FSocksResponsePort := IntToStr(w);
    FSocksResponseIP := Format('%d.%d.%d.%d',
      [Ord(Value[5]), Ord(Value[6]), Ord(Value[7]), Ord(Value[8])]);
    Result := 9;
  end
  else
  begin
    if Length(Value) < 4 then
      Exit;
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
          for n := 6 to 6 + y - 1 do
            FSocksResponseIP := FSocksResponseIP + Value[n];
          Result := 5 + y + 1;
        end;
      4:
        begin
          if Length(Value) < 22 then
            Exit;
          FillChar(ip6, SizeOf(ip6), 0);
          ip6.sin6_addr.S_un_b.s_b1 := Value[5];
          ip6.sin6_addr.S_un_b.s_b2 := Value[6];
          ip6.sin6_addr.S_un_b.s_b3 := Value[7];
          ip6.sin6_addr.S_un_b.s_b4 := Value[8];
          ip6.sin6_addr.S_un_b.s_b5 := Value[9];
          ip6.sin6_addr.S_un_b.s_b6 := Value[10];
          ip6.sin6_addr.S_un_b.s_b7 := Value[11];
          ip6.sin6_addr.S_un_b.s_b8 := Value[12];
          ip6.sin6_addr.S_un_b.s_b9 := Value[13];
          ip6.sin6_addr.S_un_b.s_b10 := Value[14];
          ip6.sin6_addr.S_un_b.s_b11 := Value[15];
          ip6.sin6_addr.S_un_b.s_b12 := Value[16];
          ip6.sin6_addr.S_un_b.s_b13 := Value[17];
          ip6.sin6_addr.S_un_b.s_b14 := Value[18];
          ip6.sin6_addr.S_un_b.s_b15 := Value[19];
          ip6.sin6_addr.S_un_b.s_b16 := Value[20];
          ip6.sin6_family := AF_INET6;
          FSocksResponseIP := IP6ToStr(ip6);
          Result := 21;
        end;
    else
      Exit;
    end;
    w := DecodeInt(Value, Result);
    FSocksResponsePort := IntToStr(w);
    Result := Result + 2;
  end;
end;

{======================================================================}

procedure TDgramBlockSocket.Connect(IP, Port: string);
begin
  SetRemoteSin(IP, Port);
  InternalCreateSocket(FRemoteSin);
  FBuffer := '';
  DoStatus(HR_Connect, IP + ':' + Port);
end;

function TDgramBlockSocket.RecvBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := RecvBufferFrom(Buffer, Length);
end;

function TDgramBlockSocket.SendBuffer(Buffer: Pointer; Length: Integer): Integer;
begin
  Result := SendBufferTo(Buffer, Length);
end;

{======================================================================}

destructor TUDPBlockSocket.Destroy;
begin
  if Assigned(FSocksControlSock) then
    FSocksControlSock.Free;
  inherited;
end;

procedure TUDPBlockSocket.EnableBroadcast(Value: Boolean);
var
  d: TSynaOption;
begin
  d.Option := SOT_Broadcast;
  d.Enabled := Value;
  DelayedOption(d);
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
    FSocksControlSock.CreateSocketByName(FSocksIP);
    FSocksControlSock.Connect(FSocksIP, FSocksPort);
    if FSocksControlSock.LastError <> 0 then
      Exit;
    // if not assigned local port, assign it!
    if not FBinded then
      Bind(cAnyHost, cAnyPort);
    //open control TCP connection to SOCKS
    FSocksControlSock.FSocksUsername := FSocksUsername;
    FSocksControlSock.FSocksPassword := FSocksPassword;
    b := FSocksControlSock.SocksOpen;
    if b then
      b := FSocksControlSock.SocksRequest(3, GetLocalSinIP, IntToStr(GetLocalSinPort));
    if b then
      b := FSocksControlSock.SocksResponse;
    if not b and (FLastError = 0) then
      FLastError := WSANO_RECOVERY;
    FUsingSocks :=FSocksControlSock.UsingSocks;
    FSocksRemoteIP := FSocksControlSock.FSocksResponseIP;
    FSocksRemotePort := FSocksControlSock.FSocksResponsePort;
    Result := b and (FLastError = 0);
  end;
end;

function TUDPBlockSocket.SendBufferTo(Buffer: Pointer; Length: Integer): Integer;
var
  SIp: string;
  SPort: integer;
  Buf: string;
begin
  Result := 0;
  FUsingSocks := False;
  if (FSocksIP <> '') and (not UdpAssociation) then
    FLastError := WSANO_RECOVERY
  else
  begin
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
  Multicast: TIP_mreq;
  Multicast6: TIPv6_mreq;
begin
  if FIP6Used then
  begin
    Multicast6.ipv6mr_multiaddr := StrToIp6(MCastIP).sin6_addr;
    Multicast6.ipv6mr_interface := 0;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_JOIN_GROUP,
      pchar(@Multicast6), SizeOf(Multicast6)));
  end
  else
  begin
    Multicast.imr_multiaddr.S_addr := synsock.inet_addr(PChar(MCastIP));
    Multicast.imr_interface.S_addr := u_long(INADDR_ANY);
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_ADD_MEMBERSHIP,
      pchar(@Multicast), SizeOf(Multicast)));
  end;
  ExceptCheck;
end;

procedure TUDPBlockSocket.DropMulticast(MCastIP: string);
var
  Multicast: TIP_mreq;
  Multicast6: TIPv6_mreq;
begin
  if FIP6Used then
  begin
    Multicast6.ipv6mr_multiaddr := StrToIp6(MCastIP).sin6_addr;
    Multicast6.ipv6mr_interface := 0;
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IPV6, IPV6_LEAVE_GROUP,
      pchar(@Multicast6), SizeOf(Multicast6)));
  end
  else
  begin
    Multicast.imr_multiaddr.S_addr := synsock.inet_addr(PChar(MCastIP));
    Multicast.imr_interface.S_addr := u_long(INADDR_ANY);
    SockCheck(synsock.SetSockOpt(FSocket, IPPROTO_IP, IP_DROP_MEMBERSHIP,
      pchar(@Multicast), SizeOf(Multicast)));
  end;
  ExceptCheck;
end;

procedure TUDPBlockSocket.SetMulticastTTL(TTL: integer);
var
  d: TSynaOption;
begin
  d.Option := SOT_MulticastTTL;
  d.Value := TTL;
  DelayedOption(d);
end;

function TUDPBlockSocket.GetMulticastTTL:integer;
var
  l: Integer;
begin
  l := SizeOf(Result);
  if FIP6Used then
    synsock.GetSockOpt(FSocket, IPPROTO_IPV6, IPV6_MULTICAST_HOPS, @Result, l)
  else
    synsock.GetSockOpt(FSocket, IPPROTO_IP, IP_MULTICAST_TTL, @Result, l);
end;

procedure TUDPBlockSocket.EnableMulticastLoop(Value: Boolean);
var
  d: TSynaOption;
begin
  d.Option := SOT_MulticastLoop;
  d.Enabled := Value;
  DelayedOption(d);
end;

function TUDPBlockSocket.GetSocketType: integer;
begin
  Result := SOCK_DGRAM;
end;

function TUDPBlockSocket.GetSocketProtocol: integer;
begin
 Result := IPPROTO_UDP;
end;

{======================================================================}

function PasswordCallback(buf:PChar; size:Integer; rwflag:Integer; userdata: Pointer):Integer; cdecl;
var
  Password: String;
begin
  Password := '';
  if TTCPBlockSocket(userdata) is TTCPBlockSocket then
    Password := TTCPBlockSocket(userdata).SSLPassword;
  if Length(Password) > (Size - 1) then
    SetLength(Password, Size - 1);
  Result := Length(Password);
  StrLCopy(buf, PChar(Password + #0), Result + 1);
end;

constructor TTCPBlockSocket.Create;
begin
  inherited Create;
  FSslEnabled := False;
  FSslBypass := False;
  FSSLCiphers := 'DEFAULT';
  FSSLCertificateFile := '';
  FSSLPrivateKeyFile := '';
  FSSLPassword  := '';
  FSsl := nil;
  Fctx := nil;
  FSSLLastError := 0;
  FSSLLastErrorDesc := '';
  FSSLverifyCert := False;
  FSSLType := LT_all;
  FHTTPTunnelIP := '';
  FHTTPTunnelPort := '';
  FHTTPTunnel := False;
  FHTTPTunnelRemoteIP := '';
  FHTTPTunnelRemotePort := '';
  FHTTPTunnelUser := '';
  FHTTPTunnelPass := '';
  FHTTPTunnelTimeout := 30000;
end;

procedure TTCPBlockSocket.CloseSocket;
begin
  if SSLEnabled then
    SSLDoShutdown;
  if FSocket <> INVALID_SOCKET then
  begin
    Synsock.Shutdown(FSocket, 1);
    Purge;
  end;
  inherited CloseSocket;
end;

function TTCPBlockSocket.WaitingData: Integer;
begin
  Result := 0;
  if FSslEnabled and not(FSslBypass) and not(FBypassFlag) then
    Result := sslpending(Fssl);
  if Result = 0 then
    Result := inherited WaitingData;
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
    if Sip = cAnyHost then
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
    if FSocksLocalIP = cAnyHost then
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
    Result := synsock.Accept(FSocket, @FRemoteSin, Len);
    SockCheck(Result);
  end;
  ExceptCheck;
  DoStatus(HR_Accept, '');
end;

procedure TTCPBlockSocket.Connect(IP, Port: string);
var
  x: integer;
begin
  if FSocksIP <> '' then
    SocksDoConnect(IP, Port)
  else
    if FHTTPTunnelIP <> '' then
      HTTPTunnelDoConnect(IP, Port)
    else
      inherited Connect(IP, Port);
  if FSslEnabled then
    if FLastError = 0 then
      SSLDoConnect
    else
    begin
      x := FLastError;
      SSLEnabled := False;
      FLastError := x;
    end;
end;

procedure TTCPBlockSocket.SocksDoConnect(IP, Port: string);
var
  b: Boolean;
begin
  inherited Connect(FSocksIP, FSocksPort);
  if FLastError = 0 then
  begin
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
  end;
  ExceptCheck;
  DoStatus(HR_Connect, IP + ':' + Port);
end;

procedure TTCPBlockSocket.HTTPTunnelDoConnect(IP, Port: string);
//bugfixed by Mike Green (mgreen@emixode.com)
var
  s: string;
begin
  try
    Port := IntToStr(ResolvePort(Port));
    FBypassFlag := True;
    inherited Connect(FHTTPTunnelIP, FHTTPTunnelPort);
    if FLastError <> 0 then
      Exit;
    FHTTPTunnel := False;
    if IsIP6(IP) then
      IP := '[' + IP + ']';
    SendString('CONNECT ' + IP + ':' + Port + ' HTTP/1.0' + CRLF);
    if FHTTPTunnelUser <> '' then
    Sendstring('Proxy-Authorization: Basic ' +
      EncodeBase64(FHTTPTunnelUser + ':' + FHTTPTunnelPass) + CRLF);
    SendString(CRLF);
    repeat
      s := RecvTerminated(FHTTPTunnelTimeout, #$0a);
      if FLastError <> 0 then
        Break;
      if (Pos('HTTP/', s) = 1) and (Length(s) > 11) then
        FHTTPTunnel := s[10] = '2';
    until (s = '') or (s = #$0d);
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
var
  x: integer;
begin
  FLastError := 0;
  if not FSSLEnabled then
    SSLEnabled := True;
  if (FLastError = 0) then
    if sslsetfd(FSsl, FSocket) < 1 then
    begin
      FLastError := WSASYSNOTREADY;
      SSLCheck;
    end;
  if (FLastError = 0) then
  begin
    x := sslconnect(FSsl);
    if x < 1 then
    begin
      FLastError := WSASYSNOTREADY;
      SSLcheck;
    end;
  end;
  if FLastError <> 0 then
  begin
    x := FLastError;
    SSLEnabled := False;
    FLastError := x;
  end;
  ExceptCheck;
end;

procedure TTCPBlockSocket.SSLDoShutdown;
var
  x: integer;
begin
  FLastError := 0;
  if assigned(FSsl) then
  begin
    x := sslshutdown(FSsl);
    if x = 0 then
      sslshutdown(FSsl);
  end;
  SSLEnabled := False;
  ExceptCheck;
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
    Result := ResolvePort(FSocksRemotePort)
  else
    if FHTTPTunnel then
      Result := StrToIntDef(FHTTPTunnelRemotePort, 0)
    else
      Result := inherited GetRemoteSinPort;
end;

function TTCPBlockSocket.SSLCheck: Boolean;
var
  ErrBuf: array[0..255] of Char;
begin
  Result := true;
  FSSLLastErrorDesc := '';
  FSSLLastError := ErrGetError;
  ErrClearError;
  if FSSLLastError <> 0 then
  begin
    Result := False;
    ErrErrorString(FSSLLastError, ErrBuf);
    FSSLLastErrorDesc := ErrBuf;
  end;
end;

function TTCPBlockSocket.GetSSLLoaded: Boolean;
begin
  Result := IsSSLLoaded;
end;

function TTCPBlockSocket.SetSslKeys: boolean;
begin
  if not assigned(FCtx) then
  begin
    Result := False;
    Exit;
  end
  else
    Result := True;
  if FSSLCertificateFile <> '' then
    if SslCtxUseCertificateChainFile(FCtx, PChar(FSSLCertificateFile)) <> 1 then
    begin
      Result := False;
      SSLCheck;
      Exit;
    end;
  if FSSLPrivateKeyFile <> '' then
    if SslCtxUsePrivateKeyFile(FCtx, PChar(FSSLPrivateKeyFile), 1) <> 1 then
    begin
      Result := False;
      SSLCheck;
      Exit;
    end;
  if FSSLCertCAFile <> '' then
    if SslCtxLoadVerifyLocations(FCtx, PChar(FSSLCertCAFile), nil) <> 1 then
    begin
      Result := False;
      SSLCheck;
    end;
end;

procedure TTCPBlockSocket.SetSslEnabled(Value: Boolean);
var
  err: Boolean;
begin
  FLastError := 0;
  if Value <> FSslEnabled then
    if Value then
    begin
      FBuffer := '';
      FSSLLastErrorDesc := '';
      FSSLLastError := 0;
      if InitSSLInterface then
      begin
        err := False;
        Fctx := nil;
        case FSSLType of
          LT_SSLv2:
            Fctx := SslCtxNew(SslMethodV2);
          LT_SSLv3:
            Fctx := SslCtxNew(SslMethodV3);
          LT_TLSv1:
            Fctx := SslCtxNew(SslMethodTLSV1);
          LT_all:
            Fctx := SslCtxNew(SslMethodV23);
        end;
        if Fctx = nil then
        begin
          SSLCheck;
          FlastError := WSAEPROTONOSUPPORT;
          err := True;
        end
        else
        begin
          SslCtxSetCipherList(Fctx, PChar(FSSLCiphers));
          if FSSLverifyCert then
            SslCtxSetVerify(FCtx, SSL_VERIFY_PEER, nil)
          else
            SslCtxSetVerify(FCtx, SSL_VERIFY_NONE, nil);
          SslCtxSetDefaultPasswdCb(FCtx, @PasswordCallback);
          SslCtxSetDefaultPasswdCbUserdata(FCtx, self);
          if not SetSSLKeys then
            FLastError := WSAEINVAL
          else
          begin
            Fssl := nil;
            Fssl := SslNew(Fctx);
            if Fssl = nil then
            begin
              SSLCheck;
              FlastError := WSAEPROTONOSUPPORT;
              err := True;
            end;
          end;
        end;
        FSslEnabled := not err;
      end
      else
        FlastError := WSAEPROTONOSUPPORT;
    end
    else
    begin
      FBuffer := '';
      sslfree(Fssl);
      Fssl := nil;
      SslCtxFree(Fctx);
      Fctx := nil;
      ErrRemoveState(0);
      FSslEnabled := False;
    end;
  ExceptCheck;
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
    until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
    if err = SSL_ERROR_ZERO_RETURN then
      Result := 0
    else
      if (err <> 0) then
        FLastError := WSASYSNOTREADY;
    ExceptCheck;
    Inc(FRecvCounter, Result);
    DoStatus(HR_ReadCount, IntToStr(Result));
    DoReadFilter(Buffer, Result);
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
    DoWriteFilter(Buffer, Length);
    repeat
      Result := SslWrite(FSsl, Buffer, Length);
      err := SslGetError(FSsl, Result);
    until (err <> SSL_ERROR_WANT_READ) and (err <> SSL_ERROR_WANT_WRITE);
    if err = SSL_ERROR_ZERO_RETURN then
      Result := 0
    else
      if (err <> 0) then
        FLastError := WSASYSNOTREADY;
    ExceptCheck;
    Inc(FSendCounter, Result);
    DoStatus(HR_WriteCount, IntToStr(Result));
  end
  else
    Result := inherited SendBuffer(Buffer, Length);
end;

function TTCPBlockSocket.SSLAcceptConnection: Boolean;
var
  x: integer;
begin
  FLastError := 0;
  if not FSSLEnabled then
    SSLEnabled := True;
  if (FLastError = 0) then
    if sslsetfd(FSsl, FSocket) < 1 then
    begin
      FLastError := WSASYSNOTREADY;
      SSLCheck;
    end;
  if (FLastError = 0) then
    if sslAccept(FSsl) < 1 then
      FLastError := WSASYSNOTREADY;
  if FLastError <> 0 then
  begin
    x := FLastError;
    SSLEnabled := False;
    FLastError := x;
  end;
  ExceptCheck;
  Result := FLastError = 0;
end;

function TTCPBlockSocket.SSLGetSSLVersion: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SSlGetVersion(FSsl);
end;

function TTCPBlockSocket.SSLGetPeerSubject: string;
var
  cert: PX509;
  s: string;
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  setlength(s, 4096);
  Result := SslX509NameOneline(SslX509GetSubjectName(cert), PChar(s), length(s));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerName: string;
var
  s: string;
begin
  s := SSLGetPeerSubject;
  s := SeparateRight(s, '/CN=');
  Result := SeparateLeft(s, '/');
end;

function TTCPBlockSocket.SSLGetPeerIssuer: string;
var
  cert: PX509;
  s: string;
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  setlength(s, 4096);
  Result := SslX509NameOneline(SslX509GetIssuerName(cert), PChar(s), length(s));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerSubjectHash: Cardinal;
var
  cert: PX509;
begin
  if not assigned(FSsl) then
  begin
    Result := 0;
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  Result := SslX509NameHash(SslX509GetSubjectName(cert));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerIssuerHash: Cardinal;
var
  cert: PX509;
begin
  if not assigned(FSsl) then
  begin
    Result := 0;
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  Result := SslX509NameHash(SslX509GetIssuerName(cert));
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetPeerFingerprint: string;
var
  cert: PX509;
  x: integer;
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  setlength(Result, EVP_MAX_MD_SIZE);
  SslX509Digest(cert, SslEvpMd5, PChar(Result), @x);
  SetLength(Result, x);
  SslX509Free(cert);
end;

function TTCPBlockSocket.SSLGetCertInfo: string;
var
  cert: PX509;
  x, y: integer;
  b: PBIO;
  s: string;
begin
  if not assigned(FSsl) then
  begin
    Result := '';
    Exit;
  end;
  cert := SSLGetPeerCertificate(Fssl);
  b := BioNew(BioSMem);
  try
    X509Print(b, cert);
    x := bioctrlpending(b);
    setlength(s,x);
    y := bioread(b,PChar(s),x);
    if y > 0 then
      setlength(s, y);
    Result := ReplaceString(s, LF, CRLF);
  finally
    BioFreeAll(b);
  end;
end;

function TTCPBlockSocket.SSLGetCipherName: string;
begin
  if not assigned(FSsl) then
    Result := ''
  else
    Result := SslCipherGetName(SslGetCurrentCipher(FSsl));
end;

function TTCPBlockSocket.SSLGetCipherBits: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    Result := SSLCipherGetBits(SslGetCurrentCipher(FSsl), nil);
end;

function TTCPBlockSocket.SSLGetCipherAlgBits: integer;
begin
  if not assigned(FSsl) then
    Result := 0
  else
    SSLCipherGetBits(SslGetCurrentCipher(FSsl), @Result);
end;

function TTCPBlockSocket.SSLGetVerifyCert: integer;
begin
  if not assigned(FSsl) then
    Result := 1
  else
    Result := SslGetVerifyResult(FSsl);
end;

function TTCPBlockSocket.GetSocketType: integer;
begin
  Result := SOCK_STREAM;
end;

function TTCPBlockSocket.GetSocketProtocol: integer;
begin
  Result := IPPROTO_TCP;
end;

{======================================================================}

function TICMPBlockSocket.GetSocketType: integer;
begin
  Result := SOCK_RAW;
end;

function TICMPBlockSocket.GetSocketProtocol: integer;
begin
  if FIP6Used then
    Result := IPPROTO_ICMPV6
  else
    Result := IPPROTO_ICMP;
end;

{======================================================================}

function TRAWBlockSocket.GetSocketType: integer;
begin
  Result := SOCK_RAW;
end;

function TRAWBlockSocket.GetSocketProtocol: integer;
begin
  Result := IPPROTO_RAW;
end;

{======================================================================}

constructor TSynaClient.Create;
begin
  inherited Create;
  FIPInterface := cAnyHost;
  FTargetHost := cLocalhost;
  FTargetPort := cAnyPort;
  FTimeout := 5000;
end;

{======================================================================}

{$IFDEF ONCEWINSOCK}
initialization
begin
  if not InitSocketInterface(DLLStackName) then
  begin
    e := ESynapseError.Create('Error loading Socket interface (' + DLLStackName + ')!');
    e.ErrorCode := 0;
    e.ErrorMessage := 'Error loading Socket interface (' + DLLStackName + ')!';
    raise e;
  end;
  synsock.WSAStartup(WinsockLevel, WsaDataOnce);
end;
{$ENDIF}

finalization
begin
{$IFDEF ONCEWINSOCK}
  synsock.WSACleanup;
  DestroySocketInterface;
{$ENDIF}
end;

end.
