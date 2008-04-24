{==============================================================================|
| Project : Delphree - Synapse                                   | 001.001.003 |
|==============================================================================|
| Content: DNS client                                                          |
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
| Portions created by Lukas Gebauer are Copyright (c)2000,2001.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

// RFC-1035, RFC-1183, RFC1706, RFC1712, RFC2163, RFC2230

{$Q-}
{$WEAKPACKAGEUNIT ON}

unit DNSsend;

interface

uses
  SysUtils, Classes,
  blcksock, SynaUtil;

const
  cDnsProtocol = 'Domain';

  QTYPE_A = 1;
  QTYPE_NS = 2;
  QTYPE_MD = 3;
  QTYPE_MF = 4;
  QTYPE_CNAME = 5;
  QTYPE_SOA = 6;
  QTYPE_MB = 7;
  QTYPE_MG = 8;
  QTYPE_MR = 9;
  QTYPE_NULL = 10;
  QTYPE_WKS = 11; //
  QTYPE_PTR = 12;
  QTYPE_HINFO = 13;
  QTYPE_MINFO = 14;
  QTYPE_MX = 15;
  QTYPE_TXT = 16;

  QTYPE_RP = 17;
  QTYPE_AFSDB = 18;
  QTYPE_X25 = 19;
  QTYPE_ISDN = 20;
  QTYPE_RT = 21;
  QTYPE_NSAP = 22;
  QTYPE_NSAPPTR = 23;
  QTYPE_SIG = 24; // RFC-2065
  QTYPE_KEY = 25; // RFC-2065
  QTYPE_PX = 26;
  QTYPE_GPOS = 27;
  QTYPE_AAAA = 28; // IP6 Address  [Susan Thomson]
  QTYPE_LOC = 29; // RFC-1876
  QTYPE_NXT = 30; // RFC-2065

  QTYPE_SRV = 33; // RFC-2052
  QTYPE_NAPTR = 35; // RFC-2168
  QTYPE_KX = 36;

  QTYPE_AXFR = 252; //
  QTYPE_MAILB = 253; //
  QTYPE_MAILA = 254; //
  QTYPE_ALL = 255; //

type
  TDNSSend = class(TObject)
  private
    FTimeout: Integer;
    FDNSHost: string;
    FRCode: Integer;
    FBuffer: string;
    FSock: TUDPBlockSocket;
    function CompressName(const Value: string): string;
    function CodeHeader: string;
    function CodeQuery(const Name: string; QType: Integer): string;
    function DecodeLabels(var From: Integer): string;
    function DecodeResource(var i: Integer; const Name: string;
      QType: Integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    function DNSQuery(Name: string; QType: Integer;
      const Reply: TStrings): Boolean;
  published
    property Timeout: Integer read FTimeout Write FTimeout;
    property DNSHost: string read FDNSHost Write FDNSHost;
    property RCode: Integer read FRCode;
    property Sock: TUDPBlockSocket read FSock;
  end;

function GetMailServers(const DNSHost, Domain: string;
  const Servers: TStrings): Boolean;

implementation

constructor TDNSSend.Create;
begin
  inherited Create;
  FSock := TUDPBlockSocket.Create;
  FSock.CreateSocket;
  FTimeout := 5000;
  FDNSHost := cLocalhost;
end;

destructor TDNSSend.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

function TDNSSend.CompressName(const Value: string): string;
var
  n: Integer;
  s: string;
begin
  Result := '';
  if Value = '' then
    Result := #0
  else
  begin
    s := '';
    for n := 1 to Length(Value) do
      if Value[n] = '.' then
      begin
        Result := Result + Char(Length(s)) + s;
        s := '';
      end
      else
        s := s + Value[n];
    if s <> '' then
      Result := Result + Char(Length(s)) + s;
    Result := Result + #0;
  end;
end;

function TDNSSend.CodeHeader: string;
begin
  Randomize;
  Result := CodeInt(Random(32767)); // ID
  Result := Result + CodeInt($0100); // flags
  Result := Result + CodeInt(1); // QDCount
  Result := Result + CodeInt(0); // ANCount
  Result := Result + CodeInt(0); // NSCount
  Result := Result + CodeInt(0); // ARCount
end;

function TDNSSend.CodeQuery(const Name: string; QType: Integer): string;
begin
  Result := CompressName(Name);
  Result := Result + CodeInt(QType);
  Result := Result + CodeInt(1); // Type INTERNET
end;

function TDNSSend.DecodeLabels(var From: Integer): string;
var
  l, f: Integer;
begin
  Result := '';
  while True do
  begin
    l := Ord(FBuffer[From]);
    Inc(From);
    if l = 0 then
      Break;
    if Result <> '' then
      Result := Result + '.';
    if (l and $C0) = $C0 then
    begin
      f := l and $3F;
      f := f * 256 + Ord(FBuffer[From]) + 1;
      Inc(From);
      Result := Result + DecodeLabels(f);
      Break;
    end
    else
    begin
      Result := Result + Copy(FBuffer, From, l);
      Inc(From, l);
    end;
  end;
end;

function TDNSSend.DecodeResource(var i: Integer; const Name: string;
  QType: Integer): string;
var
  Rname: string;
  RType, Len, j, x, n: Integer;
begin
  Result := '';
  Rname := DecodeLabels(i);
  RType := DecodeInt(FBuffer, i);
  Inc(i, 8);
  Len := DecodeInt(FBuffer, i);
  Inc(i, 2); // i point to begin of data
  j := i;
  i := i + len; // i point to next record
  if (Name = Rname) and (QType = RType) then
  begin
    case RType of
      QTYPE_A:
        begin
          Result := IntToStr(Ord(FBuffer[j]));
          Inc(j);
          Result := Result + '.' + IntToStr(Ord(FBuffer[j]));
          Inc(j);
          Result := Result + '.' + IntToStr(Ord(FBuffer[j]));
          Inc(j);
          Result := Result + '.' + IntToStr(Ord(FBuffer[j]));
        end;
      QTYPE_NS, QTYPE_MD, QTYPE_MF, QTYPE_CNAME, QTYPE_MB,
        QTYPE_MG, QTYPE_MR, QTYPE_PTR, QTYPE_X25, QTYPE_NSAP,
        QTYPE_NSAPPTR:
        Result := DecodeLabels(j);
      QTYPE_SOA:
        begin
          Result := DecodeLabels(j);
          Result := Result + ',' + DecodeLabels(j);
          for n := 1 to 5 do
          begin
            x := DecodeInt(FBuffer, j) * 65536 + DecodeInt(FBuffer, j + 2);
            Inc(j, 4);
            Result := Result + ',' + IntToStr(x);
          end;
        end;
      QTYPE_NULL:
        begin
        end;
      QTYPE_WKS:
        begin
        end;
      QTYPE_HINFO, QTYPE_MINFO, QTYPE_RP, QTYPE_ISDN:
        begin
          Result := DecodeLabels(j);
          Result := Result + ',' + DecodeLabels(j);
        end;
      QTYPE_MX, QTYPE_AFSDB, QTYPE_RT, QTYPE_KX:
        begin
          x := DecodeInt(FBuffer, j);
          Inc(j, 2);
          Result := IntToStr(x);
          Result := Result + ',' + DecodeLabels(j);
        end;
      QTYPE_TXT:
        Result := DecodeLabels(j);
      QTYPE_GPOS:
        begin
          Result := DecodeLabels(j);
          Result := Result + ',' + DecodeLabels(j);
          Result := Result + ',' + DecodeLabels(j);
        end;
      QTYPE_PX:
        begin
          x := DecodeInt(FBuffer, j);
          Inc(j, 2);
          Result := IntToStr(x);
          Result := Result + ',' + DecodeLabels(j);
          Result := Result + ',' + DecodeLabels(j);
        end;
    end;
  end;
end;

function TDNSSend.DNSQuery(Name: string; QType: Integer;
  const Reply: TStrings): Boolean;
var
  x, n, i: Integer;
  flag, qdcount, ancount, nscount, arcount: Integer;
  s: string;
begin
  Result := False;
  Reply.Clear;
  if IsIP(Name) then
    Name := ReverseIP(Name) + '.in-addr.arpa';
  FBuffer := CodeHeader + CodeQuery(Name, QType);
  FSock.Connect(FDNSHost, cDnsProtocol);
  FSock.SendString(FBuffer);
  if FSock.CanRead(FTimeout) then
  begin
    x := FSock.WaitingData;
    SetLength(FBuffer, x);
    FSock.RecvBuffer(Pointer(FBuffer), x);
    flag := DecodeInt(FBuffer, 3);
    FRCode := Flag and $000F;
    if FRCode = 0 then
    begin
      qdcount := DecodeInt(FBuffer, 5);
      ancount := DecodeInt(FBuffer, 7);
      nscount := DecodeInt(FBuffer, 9);
      arcount := DecodeInt(FBuffer, 11);
      i := 13; //begin of body
      if qdcount > 0 then //skip questions
        for n := 1 to qdcount do
        begin
          while (FBuffer[i] <> #0) and ((Ord(FBuffer[i]) and $C0) <> $C0) do
            Inc(i);
          Inc(i, 5);
        end;
      if ancount > 0 then
        for n := 1 to ancount do
        begin
          s := DecodeResource(i, Name, QType);
          if s <> '' then
            Reply.Add(s);
        end;
      Result := True;
    end;
  end;
end;

{==============================================================================}

function GetMailServers(const DNSHost, Domain: string;
  const Servers: TStrings): Boolean;
var
  DNS: TDNSSend;
  t: TStringList;
  n, m, x: Integer;
begin
  Result := False;
  Servers.Clear;
  t := TStringList.Create;
  DNS := TDNSSend.Create;
  try
    DNS.DNSHost := DNSHost;
    if DNS.DNSQuery(Domain, QType_MX, t) then
    begin
      { normalize preference number to 5 digits }
      for n := 0 to t.Count - 1 do
      begin
        x := Pos(',', t[n]);
        if x > 0 then
          for m := 1 to 6 - x do
            t[n] := '0' + t[n];
      end;
      { sort server list }
      t.Sorted := True;
      { result is sorted list without preference numbers }
      for n := 0 to t.Count - 1 do
      begin
        x := Pos(',', t[n]);
        Servers.Add(Copy(t[n], x + 1, Length(t[n]) - x));
      end;
      Result := True;
    end;
  finally
    DNS.Free;
    t.Free;
  end;
end;

end.
