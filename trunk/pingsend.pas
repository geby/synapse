{==============================================================================|
| Project : Ararat Synapse                                       | 003.001.006 |
|==============================================================================|
| Content: PING sender                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2000-2003.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@abstract(ICMP PING implementation.)
Allows create PING and TRACEROUTE. Or you can diagnose your network.

Warning: this unit using RAW sockets. On some systems you must have special
 rights for using this sort of sockets. So, it working allways when you have
 administator/root rights. Otherwise you can have problems!

Note: IPv6 not working under .NET. It is lack of Microsoft's .NET framework.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$R-}
{$H+}

unit pingsend;

interface

uses
{$IFDEF LINUX}
  Libc,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils,
  synsock, blcksock, synautil;

const
  ICMP_ECHO = 8;
  ICMP_ECHOREPLY = 0;
  ICMP_UNREACH = 3;
  ICMP_TIME_EXCEEDED = 11;
//rfc-2292
  ICMP6_ECHO = 128;
  ICMP6_ECHOREPLY = 129;
  ICMP6_UNREACH = 1;
  ICMP6_TIME_EXCEEDED = 3;

type
  {:Record for ICMP ECHO packet header.}
  TIcmpEchoHeader = record
    i_type: Byte;
    i_code: Byte;
    i_checkSum: Word;
    i_Id: Word;
    i_seq: Word;
    TimeStamp: ULong;
  end;

  {:record used internally by TPingSend for compute checksum of ICMPv6 packet
   pseudoheader.}
  TICMP6Packet = record
    in_source: TInAddr6;
    in_dest: TInAddr6;
    Length: integer;
    free0: Byte;
    free1: Byte;
    free2: Byte;
    proto: Byte;
  end;

  {:List of possible ICMP reply packet types.}
  TICMPError = (
    IE_NoError,
    IE_Other,
    IE_TTLExceed,
    IE_UnreachOther,
    IE_UnreachRoute,
    IE_UnreachAdmin,
    IE_UnreachAddr,
    IE_UnreachPort
    );

  {:@abstract(Implementation of ICMP PING and ICMPv6 PING.)

   Note: Are you missing properties for specify server address and port? Look to
   parent @link(TSynaClient) too!}
  TPINGSend = class(TSynaClient)
  private
    FSock: TICMPBlockSocket;
    FBuffer: string;
    FSeq: Integer;
    FId: Integer;
    FPacketSize: Integer;
    FPingTime: Integer;
    FIcmpEcho: Byte;
    FIcmpEchoReply: Byte;
    FIcmpUnreach: Byte;
    FReplyFrom: string;
    FReplyType: byte;
    FReplyCode: byte;
    FReplyError: TICMPError;
    FReplyErrorDesc: string;
    function Checksum(Value: string): Word;
    function Checksum6(Value: string): Word;
    function ReadPacket: Boolean;
    procedure TranslateError;
  public
    {:Send ICMP ping to host and count @link(pingtime). If ping OK, result is
     @true.}
    function Ping(const Host: string): Boolean;
    constructor Create;
    destructor Destroy; override;
  published
    {:Size of PING packet. Default size is 32 bytes.}
    property PacketSize: Integer read FPacketSize Write FPacketSize;

    {:Time between request and reply.}
    property PingTime: Integer read FPingTime;

    {:From this address is sended reply for your PING request. It maybe not your
     requested destination, when some error occured!}
    property ReplyFrom: string read FReplyFrom;

    {:ICMP type of PING reply. Each protocol using another values! For IPv4 and
     IPv6 are used different values!}
    property ReplyType: byte read FReplyType;

    {:ICMP code of PING reply. Each protocol using another values! For IPv4 and
     IPv6 are used different values! For protocol independent value look to
     @link(ReplyError)}
    property ReplyCode: byte read FReplyCode;

    {:Return type of returned ICMP message. This value is independent on used
     protocol!}
    property ReplyError: TICMPError read FReplyError;

    {:Return human readable description of returned packet type.}
    property ReplyErrorDesc: string read FReplyErrorDesc;

    {:Socket object used for TCP/IP operation. Good for seting OnStatus hook, etc.}
    property Sock: TICMPBlockSocket read FSock;
  end;

{:A very useful function and example of its use would be found in the TPINGSend
 object. Use it to ping to any host. If successful, returns the ping time in
 milliseconds.  Returns -1 if an error occurred.}
function PingHost(const Host: string): Integer;

{:A very useful function and example of its use would be found in the TPINGSend
 object. Use it to TraceRoute to any host.}
function TraceRouteHost(const Host: string): string;

implementation

{==============================================================================}

constructor TPINGSend.Create;
begin
  inherited Create;
  FSock := TICMPBlockSocket.Create;
  FTimeout := 5000;
  FPacketSize := 32;
  FSeq := 0;
  Randomize;
end;

destructor TPINGSend.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

function TPINGSend.ReadPacket: Boolean;
begin
  FBuffer := FSock.RecvPacket(Ftimeout);
  Result := FSock.LastError = 0;
end;

function TPINGSend.Ping(const Host: string): Boolean;
var
  IPHeadPtr: ^TIPHeader;
  IpHdrLen: Integer;
  IcmpEchoHeaderPtr: ^TICMPEchoHeader;
  t: Boolean;
  x: cardinal;
  IcmpReqHead: string;
begin
  Result := False;
  FPingTime := -1;
  FReplyFrom := '';
  FReplyType := 0;
  FReplyCode := 0;
  FReplyError := IE_NoError;
  FReplyErrorDesc := '';
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(Host, '0');
  if FSock.LastError <> 0 then
    Exit;
  FSock.SizeRecvBuffer := 60 * 1024;
  if FSock.IP6used then
  begin
    FIcmpEcho := ICMP6_ECHO;
    FIcmpEchoReply := ICMP6_ECHOREPLY;
    FIcmpUnreach := ICMP6_UNREACH;
  end
  else
  begin
    FIcmpEcho := ICMP_ECHO;
    FIcmpEchoReply := ICMP_ECHOREPLY;
    FIcmpUnreach := ICMP_UNREACH;
  end;
  FBuffer := StringOfChar(#55, SizeOf(TICMPEchoHeader) + FPacketSize);
  IcmpEchoHeaderPtr := Pointer(FBuffer);
  with IcmpEchoHeaderPtr^ do
  begin
    i_type := FIcmpEcho;
    i_code := 0;
    i_CheckSum := 0;
    FId := System.Random(32767);
    i_Id := FId;
    TimeStamp := GetTick;
    Inc(FSeq);
    i_Seq := FSeq;
    if fSock.IP6used then
      i_CheckSum := CheckSum6(FBuffer)
    else
      i_CheckSum := CheckSum(FBuffer);
  end;
  FSock.SendString(FBuffer);
  // remember first 8 bytes of ICMP packet
  IcmpReqHead := Copy(FBuffer, 1, 8);
  x := GetTick;
  repeat
    t := ReadPacket;
    if not t then
      break;
    if fSock.IP6used then
    begin
{$IFDEF LINUX}
      IcmpEchoHeaderPtr := Pointer(FBuffer);
{$ELSE}
//WinXP SP1 with networking update doing this think by another way ;-O
//      FBuffer := StringOfChar(#0, 4) + FBuffer;
      IcmpEchoHeaderPtr := Pointer(FBuffer);
//      IcmpEchoHeaderPtr^.i_type := FIcmpEchoReply;
{$ENDIF}
    end
    else
    begin
      IPHeadPtr := Pointer(FBuffer);
      IpHdrLen := (IPHeadPtr^.VerLen and $0F) * 4;
      IcmpEchoHeaderPtr := @FBuffer[IpHdrLen + 1];
    end;
  //it discard sometimes possible 'echoes' of previosly sended packet
  //or other unwanted ICMP packets...
  until (IcmpEchoHeaderPtr^.i_type <> FIcmpEcho)
    and ((IcmpEchoHeaderPtr^.i_id = FId)
    or (Pos(IcmpReqHead, FBuffer) > 0));
  if t then
    begin
      FPingTime := TickDelta(x, GetTick);
      FReplyFrom := FSock.GetRemoteSinIP;
      FReplyType := IcmpEchoHeaderPtr^.i_type;
      FReplyCode := IcmpEchoHeaderPtr^.i_code;
      TranslateError;
      Result := True;
    end;
end;

function TPINGSend.Checksum(Value: string): Word;
var
  CkSum: DWORD;
  Num, Remain: Integer;
  n, i: Integer;
begin
  Num := Length(Value) div 2;
  Remain := Length(Value) mod 2;
  CkSum := 0;
  i := 1;
  for n := 0 to Num - 1 do
  begin
    CkSum := CkSum + Synsock.HtoNs(DecodeInt(Value, i));
    inc(i, 2);
  end;
  if Remain <> 0 then
    CkSum := CkSum + Ord(Value[Length(Value)]);
  CkSum := (CkSum shr 16) + (CkSum and $FFFF);
  CkSum := CkSum + (CkSum shr 16);
  Result := Word(not CkSum);
end;

function TPINGSend.Checksum6(Value: string): Word;
const
  IOC_OUT = $40000000;
  IOC_IN = $80000000;
  IOC_INOUT = (IOC_IN or IOC_OUT);
  IOC_WS2 = $08000000;
  SIO_ROUTING_INTERFACE_QUERY = 20 or IOC_WS2 or IOC_INOUT;
var
  ICMP6Ptr: ^TICMP6Packet;
  s: string;
  b: integer;
  ip6: TSockAddrIn6;
  x: integer;
begin
{$IFDEF LINUX}
  Result := 0;
{$ELSE}
  s := StringOfChar(#0, SizeOf(TICMP6Packet)) + Value;
  ICMP6Ptr := Pointer(s);
  x := synsock.WSAIoctl(FSock.Socket, SIO_ROUTING_INTERFACE_QUERY,
    @FSock.RemoteSin, SizeOf(FSock.RemoteSin),
    @ip6, SizeOf(ip6), @b, nil, nil);
  if x <> -1 then
    ICMP6Ptr^.in_dest := ip6.sin6_addr
  else
    ICMP6Ptr^.in_dest := FSock.LocalSin.sin6_addr;
  ICMP6Ptr^.in_source := FSock.RemoteSin.sin6_addr;
  ICMP6Ptr^.Length := synsock.htonl(Length(Value));
  ICMP6Ptr^.proto := IPPROTO_ICMPV6;
  Result := Checksum(s);
{$ENDIF}
end;

procedure TPINGSend.TranslateError;
begin
  if fSock.IP6used then
  begin
    case FReplyType of
      ICMP6_ECHOREPLY:
        FReplyError := IE_NoError;
      ICMP6_TIME_EXCEEDED:
        FReplyError := IE_TTLExceed;
      ICMP6_UNREACH:
        case FReplyCode of
          0:
            FReplyError := IE_UnreachRoute;
          3:
            FReplyError := IE_UnreachAddr;
          4:
            FReplyError := IE_UnreachPort;
          1:
            FReplyError := IE_UnreachAdmin;
        else
          FReplyError := IE_UnreachOther;
        end;
    else
      FReplyError := IE_Other;
    end;
  end
  else
  begin
    case FReplyType of
      ICMP_ECHOREPLY:
        FReplyError := IE_NoError;
      ICMP_TIME_EXCEEDED:
        FReplyError := IE_TTLExceed;
      ICMP_UNREACH:
        case FReplyCode of
          0:
            FReplyError := IE_UnreachRoute;
          1:
            FReplyError := IE_UnreachAddr;
          3:
            FReplyError := IE_UnreachPort;
          13:
            FReplyError := IE_UnreachAdmin;
        else
          FReplyError := IE_UnreachOther;
        end;
    else
      FReplyError := IE_Other;
    end;
  end;
  case FReplyError of
    IE_NoError:
      FReplyErrorDesc := '';
    IE_Other:
      FReplyErrorDesc := 'Unknown error';
    IE_TTLExceed:
      FReplyErrorDesc := 'TTL Exceeded';
    IE_UnreachOther:
      FReplyErrorDesc := 'Unknown unreachable';
    IE_UnreachRoute:
      FReplyErrorDesc := 'No route to destination';
    IE_UnreachAdmin:
      FReplyErrorDesc := 'Administratively prohibited';
    IE_UnreachAddr:
      FReplyErrorDesc := 'Address unreachable';
    IE_UnreachPort:
      FReplyErrorDesc := 'Port unreachable';
  end;
end;

{==============================================================================}

function PingHost(const Host: string): Integer;
begin
  with TPINGSend.Create do
  try
    Result := -1;
    if Ping(Host) then
      if ReplyError = IE_NoError then
        Result := PingTime;
  finally
    Free;
  end;
end;

function TraceRouteHost(const Host: string): string;
var
  Ping: TPingSend;
  ttl : byte;
begin
  Result := '';
  Ping := TPINGSend.Create;
  try
    ttl := 1;
    repeat
      ping.Sock.TTL := ttl;
      inc(ttl);
      if ttl > 30 then
        Break;
      if not ping.Ping(Host) then
      begin
        Result := Result + cAnyHost+ ' Timeout' + CRLF;
        continue;
      end;
      if (ping.ReplyError <> IE_NoError)
        and (ping.ReplyError <> IE_TTLExceed) then
      begin
        Result := Result + Ping.ReplyFrom + ' ' + Ping.ReplyErrorDesc + CRLF;
        break;
      end;
      Result := Result + Ping.ReplyFrom + ' ' + IntToStr(Ping.PingTime) + CRLF;
    until ping.ReplyError = IE_NoError;
  finally
    Ping.Free;
  end;
end;

end.
