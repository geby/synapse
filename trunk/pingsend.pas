{==============================================================================|
| Project : Delphree - Synapse                                   | 002.003.001 |
|==============================================================================|
| Content: PING sender                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2000,2001.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
See 'winsock2.txt' file in distribute package!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
}

{$Q-}
{$WEAKPACKAGEUNIT ON}

unit PINGsend;

interface

uses
{$IFDEF LINUX}
  Libc,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils,
  synsock, blcksock, SynaUtil;

const
  ICMP_ECHO = 8;
  ICMP_ECHOREPLY = 0;

type
  TIcmpEchoHeader = record
    i_type: Byte;
    i_code: Byte;
    i_checkSum: Word;
    i_Id: Word;
    i_seq: Word;
    TimeStamp: ULONG;
  end;

  TPINGSend = class(TSynaClient)
  private
    FSock: TICMPBlockSocket;
    FBuffer: string;
    FSeq: Integer;
    FId: Integer;
    FPacketSize: Integer;
    FPingTime: Integer;
    function Checksum: Integer;
    function ReadPacket: Boolean;
  public
    function Ping(const Host: string): Boolean;
    constructor Create;
    destructor Destroy; override;
  published
    property PacketSize: Integer read FPacketSize Write FPacketSize;
    property PingTime: Integer read FPingTime;
    property Sock: TICMPBlockSocket read FSock;
  end;

function PingHost(const Host: string): Integer;

implementation

{==============================================================================}

constructor TPINGSend.Create;
begin
  inherited Create;
  FSock := TICMPBlockSocket.Create;
  FSock.CreateSocket;
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
  n: Integer;
  t: Boolean;
begin
  Result := False;
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(Host, '0');
  FBuffer := StringOfChar(#0, SizeOf(TICMPEchoHeader) + FPacketSize);
  IcmpEchoHeaderPtr := Pointer(FBuffer);
  with IcmpEchoHeaderPtr^ do
  begin
    i_type := ICMP_ECHO;
    i_code := 0;
    i_CheckSum := 0;
    FId := Random(32767);
    i_Id := FId;
    TimeStamp := GetTick;
    Inc(FSeq);
    i_Seq := FSeq;
    for n := Succ(SizeOf(TIcmpEchoHeader)) to Length(FBuffer) do
      FBuffer[n] := #$55;
    i_CheckSum := CheckSum;
  end;
  FSock.SendString(FBuffer);
  repeat
    t := ReadPacket;
    if not t then
      break;
    IPHeadPtr := Pointer(FBuffer);
    IpHdrLen := (IPHeadPtr^.VerLen and $0F) * 4;
    IcmpEchoHeaderPtr := @FBuffer[IpHdrLen + 1];
  until (IcmpEchoHeaderPtr^.i_type <> ICMP_ECHO) and (IcmpEchoHeaderPtr^.i_id = FId);
  //it discard sometimes possible 'echoes' of previosly sended packet...
  if t then
    if (IcmpEchoHeaderPtr^.i_type = ICMP_ECHOREPLY) then
      if (IcmpEchoHeaderPtr^.i_id = FId) then
      begin
        FPingTime := GetTick - IcmpEchoHeaderPtr^.TimeStamp;
        Result := True;
      end;
end;

function TPINGSend.Checksum: Integer;
type
  TWordArray = array[0..0] of Word;
var
  WordArr: ^TWordArray;
  CkSum: DWORD;
  Num, Remain: Integer;
  n: Integer;
begin
  Num := Length(FBuffer) div 2;
  Remain := Length(FBuffer) mod 2;
  WordArr := Pointer(FBuffer);
  CkSum := 0;
  for n := 0 to Num - 1 do
    CkSum := CkSum + WordArr^[n];
  if Remain <> 0 then
    CkSum := CkSum + Ord(FBuffer[Length(FBuffer)]);
  CkSum := (CkSum shr 16) + (CkSum and $FFFF);
  CkSum := CkSum + (CkSum shr 16);
  Result := Word(not CkSum);
end;

{==============================================================================}

function PingHost(const Host: string): Integer;
begin
  with TPINGSend.Create do
  try
    if Ping(Host) then
      Result := PingTime
    else
      Result := -1;
  finally
    Free;
  end;
end;

end.
