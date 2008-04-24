{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.004 |
|==============================================================================|
| Content: misc. procedures and functions                                      |
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
| Portions created by Lukas Gebauer are Copyright (c) 2002-2003.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$Q-}

unit SynaMisc;

interface

uses
  SynaUtil, blcksock, SysUtils, Classes,
{$IFDEF LINUX}
  Libc;
{$ELSE}
  Windows, Wininet;
{$ENDIF}

Type
  TProxySetting = record
    Host: string;
    Port: string;
    Bypass: string;
  end;

procedure WakeOnLan(MAC, IP: string);
function GetDNS: string;
function GetIEProxy(protocol: string): TProxySetting;

implementation

{==============================================================================}
procedure WakeOnLan(MAC, IP: string);
var
  sock: TUDPBlockSocket;
  HexMac: string;
  data: string;
  n: integer;
  b: Byte;
begin
  if MAC <> '' then
  begin
    MAC := ReplaceString(MAC, '-', '');
    MAC := ReplaceString(MAC, ':', '');
    if Length(MAC) < 12 then
      Exit;
    HexMac := '';
    for n := 0 to 5 do
    begin
      b := StrToIntDef('$' + MAC[n * 2 + 1] + MAC[n * 2 + 2], 0);
      HexMac := HexMac + char(b);
    end;
    if IP = '' then
      IP := cBroadcast;
    sock := TUDPBlockSocket.Create;
    try
      sock.CreateSocket;
      sock.EnableBroadcast(true);
      sock.Connect(IP, '9');
      data := #$FF + #$FF + #$FF + #$FF + #$FF + #$FF;
      for n := 1 to 16 do
        data := data + HexMac;
      sock.SendString(data);
    finally
      sock.Free;
    end;
  end;
end;

{==============================================================================}

{$IFNDEF LINUX}
function GetDNSbyIpHlp: string;
type
  PTIP_ADDRESS_STRING = ^TIP_ADDRESS_STRING;
  TIP_ADDRESS_STRING = array[0..15] of char;
  PTIP_ADDR_STRING = ^TIP_ADDR_STRING;
  TIP_ADDR_STRING = packed record
    Next: PTIP_ADDR_STRING;
    IpAddress: TIP_ADDRESS_STRING;
    IpMask: TIP_ADDRESS_STRING;
    Context: DWORD;
  end;
  PTFixedInfo = ^TFixedInfo;
  TFixedInfo = packed record
    HostName: array[1..128 + 4] of char;
    DomainName: array[1..128 + 4] of char;
    CurrentDNSServer: PTIP_ADDR_STRING;
    DNSServerList: TIP_ADDR_STRING;
    NodeType: UINT;
    ScopeID: array[1..256 + 4] of char;
    EnableRouting: UINT;
    EnableProxy: UINT;
    EnableDNS: UINT;
  end;
const
  IpHlpDLL = 'IPHLPAPI.DLL';
var
  IpHlpModule: THandle;
  FixedInfo: PTFixedInfo;
  InfoSize: Longint;
  PDnsServer: PTIP_ADDR_STRING;
  err: integer;
  GetNetworkParams: function(FixedInfo: PTFixedInfo; pOutPutLen: PULONG): DWORD; stdcall;
begin
  InfoSize := 0;
  Result := '...';
  IpHlpModule := LoadLibrary(IpHlpDLL);
  if IpHlpModule = 0 then
    exit;
  try
    GetNetworkParams := GetProcAddress(IpHlpModule,'GetNetworkParams');
    if @GetNetworkParams = nil then
      Exit;
    err := GetNetworkParams(Nil, @InfoSize);
    if err <> ERROR_BUFFER_OVERFLOW then
      Exit;
    Result := '';
    GetMem (FixedInfo, InfoSize);
    try
      err := GetNetworkParams(FixedInfo, @InfoSize);
      if err <> ERROR_SUCCESS then
        exit;
      with FixedInfo^ do
      begin
        Result := DnsServerList.IpAddress;
        PDnsServer := DnsServerList.Next;
        while PDnsServer <> Nil do
        begin
          if Result <> '' then
            Result := Result + ',';
          Result := Result + PDnsServer^.IPAddress;
          PDnsServer := PDnsServer.Next;
        end;
    end;
    finally
      FreeMem(FixedInfo);
    end;
  finally
    FreeLibrary(IpHlpModule);
  end;
end;

function ReadReg(SubKey, Vn: PChar): string;
var
 OpenKey: HKEY;
 DataType, DataSize: integer;
 Temp: array [0..2048] of char;
begin
  Result := '';
  if RegOpenKeyEx(HKEY_LOCAL_MACHINE, SubKey, REG_OPTION_NON_VOLATILE,
    KEY_READ, OpenKey) = ERROR_SUCCESS then
  begin
    DataType := REG_SZ;
    DataSize := SizeOf(Temp);
    if RegQueryValueEx(OpenKey, Vn, nil, @DataType, @Temp, @DataSize) = ERROR_SUCCESS then
      Result := string(Temp);
    RegCloseKey(OpenKey);
   end;
end ;
{$ENDIF}

function GetDNS: string;
{$IFDEF LINUX}
var
  l: TStringList;
  n: integer;
begin
  Result := '';
  l := TStringList.Create;
  try
    l.LoadFromFile('/etc/resolv.conf');
    for n := 0 to l.Count - 1 do
      if Pos('NAMESERVER', uppercase(l[n])) = 1 then
      begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + SeparateRight(l[n], ' ');
      end;
  finally
    l.Free;
  end;
end;
{$ELSE}
const
  NTdyn = 'System\CurrentControlSet\Services\Tcpip\Parameters\Temporary';
  NTfix = 'System\CurrentControlSet\Services\Tcpip\Parameters';
  W9xfix = 'System\CurrentControlSet\Services\MSTCP';
begin
  Result := GetDNSbyIpHlp;
  if Result = '...' then
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      Result := ReadReg(NTdyn, 'NameServer');
      if result = '' then
        Result := ReadReg(NTfix, 'NameServer');
    end
    else
      Result := ReadReg(W9xfix, 'NameServer');
end;
{$ENDIF}

{==============================================================================}

function GetIEProxy(protocol: string): TProxySetting;
{$IFDEF LINUX}
begin
  Result.Host := '';
  Result.Port := '';
  Result.Bypass := '';
end;
{$ELSE}
var
  ProxyInfo: PInternetProxyInfo;
  Err: Boolean;
  Len: DWORD;
  Proxy: string;
  DefProxy: string;
  ProxyList: TStringList;
  n: integer;
begin
  Result.Host := '';
  Result.Port := '';
  Result.Bypass := '';
  if protocol = '' then
    protocol := 'http';
  Len := 4096;
  GetMem(ProxyInfo, Len);
  ProxyList := TStringList.Create;
  try
    Err := InternetQueryOption(nil, INTERNET_OPTION_PROXY, ProxyInfo, Len);
    if Err then
      if ProxyInfo^.dwAccessType = INTERNET_OPEN_TYPE_PROXY then
      begin
        ProxyList.CommaText := ReplaceString(ProxyInfo^.lpszProxy, ' ', ',');
        Proxy := '';
        DefProxy := '';
        for n := 0 to ProxyList.Count -1 do
        begin
          if Pos(lowercase(protocol) + '=', lowercase(ProxyList[n])) = 1 then
          begin
            Proxy := SeparateRight(ProxyList[n], '=');
            break;
          end;
          if Pos('=', ProxyList[n]) < 1 then
            DefProxy := ProxyList[n];
        end;
        if Proxy = '' then
          Proxy := DefProxy;
        if Proxy <> '' then
        begin
          Result.Host := SeparateLeft(Proxy, ':');
          Result.Port := SeparateRight(Proxy, ':');
        end;
        Result.Bypass := ReplaceString(ProxyInfo^.lpszProxyBypass, ' ', ',');
      end;
  finally
    ProxyList.Free;
    FreeMem(ProxyInfo);
  end;
end;
{$ENDIF}

{==============================================================================}

end.
