{==============================================================================|
| Project : Delphree - Synapse                                   | 002.002.004 |
|==============================================================================|
| Content: SNMP traps                                                          |
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
| Portions created by Hernan Sanchez are Copyright (c)2000,2001.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Hernan Sanchez (hernan.sanchez@iname.com)                                  |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$Q-}
{$WEAKPACKAGEUNIT ON}

unit SNMPTrap;

interface

uses
  Classes, SysUtils,
  blckSock, SynaUtil, ASN1Util, SNMPSend;

const
  cSnmpTrapProtocol = '162';

  SNMP_VERSION = 0;

  PDU_GET = $A0;
  PDU_GETN = $A1;
  PDU_RESP = $A2;
  PDU_SET = $A3;
  PDU_TRAP = $A4;

type
  TTrapPDU = class(TObject)
  private
    FBuffer: string;
    FTrapPort: string;
    FVersion: Integer;
    FPDUType: Integer;
    FCommunity: string;
    FEnterprise: string;
    FTrapHost: string;
    FGenTrap: Integer;
    FSpecTrap: Integer;
    FTimeTicks: Integer;
    FSNMPMibList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure MIBAdd(const MIB, Value: string; ValueType: Integer);
    procedure MIBDelete(Index: Integer);
    function MIBGet(const MIB: string): string;
    function EncodeTrap: Integer;
    function DecodeTrap: Boolean;
  published
    property Version: Integer read FVersion Write FVersion;
    property Community: string read FCommunity Write FCommunity;
    property PDUType: Integer read FPDUType Write FPDUType;
    property TrapPort: string read FTrapPort Write FTrapPort;
    property Enterprise: string read FEnterprise Write FEnterprise;
    property TrapHost: string read FTrapHost Write FTrapHost;
    property GenTrap: Integer read FGenTrap Write FGenTrap;
    property SpecTrap: Integer read FSpecTrap Write FSpecTrap;
    property TimeTicks: Integer read FTimeTicks Write FTimeTicks;
    property SNMPMibList: TList read FSNMPMibList;
  end;

  TTrapSNMP = class(TObject)
  private
    FSock: TUDPBlockSocket;
    FTrap: TTrapPDU;
    FSNMPHost: string;
    FTimeout: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Send: Integer;
    function Recv: Integer;
  published
    property Trap: TTrapPDU read FTrap;
    property SNMPHost: string read FSNMPHost Write FSNMPHost;
    property Timeout: Integer read FTimeout Write FTimeout;
    property Sock: TUDPBlockSocket read FSock;
  end;

function SendTrap(const Dest, Source, Enterprise, Community: string;
  Generic, Specific, Seconds: Integer; const MIBName, MIBValue: string;
  MIBtype: Integer): Integer;
function RecvTrap(var Dest, Source, Enterprise, Community: string;
  var Generic, Specific, Seconds: Integer; const MIBName,
  MIBValue: TStringList): Integer;

implementation

constructor TTrapPDU.Create;
begin
  inherited Create;
  FSNMPMibList := TList.Create;
  FTrapPort := cSnmpTrapProtocol;
  FVersion := SNMP_VERSION;
  FPDUType := PDU_TRAP;
  FCommunity := 'public';
end;

destructor TTrapPDU.Destroy;
var
  i: Integer;
begin
  for i := 0 to FSNMPMibList.Count - 1 do
    TSNMPMib(FSNMPMibList[i]).Free;
  FSNMPMibList.Free;
  inherited Destroy;
end;

procedure TTrapPDU.Clear;
var
  i: Integer;
begin
  for i := 0 to FSNMPMibList.Count - 1 do
    TSNMPMib(FSNMPMibList[i]).Free;
  FSNMPMibList.Clear;
  FTrapPort := cSnmpTrapProtocol;
  FVersion := SNMP_VERSION;
  FPDUType := PDU_TRAP;
  FCommunity := 'public';
end;

procedure TTrapPDU.MIBAdd(const MIB, Value: string; ValueType: Integer);
var
  SNMPMib: TSNMPMib;
begin
  SNMPMib := TSNMPMib.Create;
  SNMPMib.OID := MIB;
  SNMPMib.Value := Value;
  SNMPMib.ValueType := ValueType;
  FSNMPMibList.Add(SNMPMib);
end;

procedure TTrapPDU.MIBDelete(Index: Integer);
begin
  if (Index >= 0) and (Index < FSNMPMibList.Count) then
  begin
    TSNMPMib(FSNMPMibList[Index]).Free;
    FSNMPMibList.Delete(Index);
  end;
end;

function TTrapPDU.MIBGet(const MIB: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to FSNMPMibList.Count - 1 do
  begin
    if TSNMPMib(FSNMPMibList[i]).OID = MIB then
    begin
      Result := TSNMPMib(FSNMPMibList[i]).Value;
      Break;
    end;
  end;
end;

function TTrapPDU.EncodeTrap: Integer;
var
  s: string;
  n: Integer;
  SNMPMib: TSNMPMib;
begin
  FBuffer := '';
  for n := 0 to FSNMPMibList.Count - 1 do
  begin
    SNMPMib := FSNMPMibList[n];
    case SNMPMib.ValueType of
      ASN1_INT:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(ASNEncInt(StrToIntDef(SNMPMib.Value, 0)), SNMPMib.ValueType);
      ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(ASNEncUInt(StrToIntDef(SNMPMib.Value, 0)), SNMPMib.ValueType);
      ASN1_OBJID:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(MibToID(SNMPMib.Value), SNMPMib.ValueType);
      ASN1_IPADDR:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject(IPToID(SNMPMib.Value), SNMPMib.ValueType);
      ASN1_NULL:
        s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
          ASNObject('', ASN1_NULL);
    else
      s := ASNObject(MibToID(SNMPMib.OID), ASN1_OBJID) +
        ASNObject(SNMPMib.Value, SNMPMib.ValueType);
    end;
    FBuffer := FBuffer + ASNObject(s, ASN1_SEQ);
  end;
  FBuffer := ASNObject(FBuffer, ASN1_SEQ);
  FBuffer := ASNObject(ASNEncInt(FGenTrap), ASN1_INT) +
    ASNObject(ASNEncInt(FSpecTrap), ASN1_INT) +
    ASNObject(ASNEncUInt(FTimeTicks), ASN1_TIMETICKS) +
    FBuffer;
  FBuffer := ASNObject(MibToID(FEnterprise), ASN1_OBJID) +
    ASNObject(IPToID(FTrapHost), ASN1_IPADDR) +
    FBuffer;
  FBuffer := ASNObject(ASNEncInt(FVersion), ASN1_INT) +
    ASNObject(FCommunity, ASN1_OCTSTR) +
    ASNObject(FBuffer, Self.FPDUType);
  FBuffer := ASNObject(FBuffer, ASN1_SEQ);
  Result := 1;
end;

function TTrapPDU.DecodeTrap: Boolean;
var
  Pos, EndPos: Integer;
  Sm, Sv: string;
  Svt: Integer;
begin
  Clear;
  Result := False;
  if Length(FBuffer) < 2 then
    Exit;
  if (Ord(FBuffer[1]) and $20) = 0 then
    Exit;
  Pos := 2;
  EndPos := ASNDecLen(Pos, FBuffer);
  FVersion := StrToIntDef(ASNItem(Pos, FBuffer, Svt), 0);
  FCommunity := ASNItem(Pos, FBuffer, Svt);
  FPDUType := StrToIntDef(ASNItem(Pos, FBuffer, Svt), PDU_TRAP);
  FEnterprise := ASNItem(Pos, FBuffer, Svt);
  FTrapHost := ASNItem(Pos, FBuffer, Svt);
  FGenTrap := StrToIntDef(ASNItem(Pos, FBuffer, Svt), 0);
  FSpecTrap := StrToIntDef(ASNItem(Pos, FBuffer, Svt), 0);
  FTimeTicks := StrToIntDef(ASNItem(Pos, FBuffer, Svt), 0);
  ASNItem(Pos, FBuffer, Svt);
  while Pos < EndPos do
  begin
    ASNItem(Pos, FBuffer, Svt);
    Sm := ASNItem(Pos, FBuffer, Svt);
    Sv := ASNItem(Pos, FBuffer, Svt);
    MIBAdd(Sm, Sv, Svt);
  end;
  Result := True;
end;

constructor TTrapSNMP.Create;
begin
  inherited Create;
  FSock := TUDPBlockSocket.Create;
  FTrap := TTrapPDU.Create;
  FTimeout := 5000;
  FSNMPHost := cLocalhost;
  FSock.CreateSocket;
end;

destructor TTrapSNMP.Destroy;
begin
  FTrap.Free;
  FSock.Free;
  inherited Destroy;
end;

function TTrapSNMP.Send: Integer;
begin
  FTrap.EncodeTrap;
  FSock.Connect(SNMPHost, FTrap.TrapPort);
  FSock.SendString(FTrap.FBuffer);
  Result := 1;
end;

function TTrapSNMP.Recv: Integer;
begin
  Result := 0;
  FSock.Bind('0.0.0.0', FTrap.TrapPort);
  FTrap.FBuffer := FSock.RecvPacket(FTimeout);
  if Fsock.Lasterror = 0 then
    if FTrap.DecodeTrap then
      Result := 1;
end;

function SendTrap(const Dest, Source, Enterprise, Community: string;
  Generic, Specific, Seconds: Integer; const MIBName, MIBValue: string;
  MIBtype: Integer): Integer;
begin
  with TTrapSNMP.Create do
  try
    SNMPHost := Dest;
    Trap.TrapHost := Source;
    Trap.Enterprise := Enterprise;
    Trap.Community := Community;
    Trap.GenTrap := Generic;
    Trap.SpecTrap := Specific;
    Trap.TimeTicks := Seconds;
    Trap.MIBAdd(MIBName, MIBValue, MIBType);
    Result := Send;
  finally
    Free;
  end;
end;

function RecvTrap(var Dest, Source, Enterprise, Community: string;
  var Generic, Specific, Seconds: Integer;
  const MIBName, MIBValue: TStringList): Integer;
var
  i: Integer;
begin
  with TTrapSNMP.Create do
  try
    SNMPHost := Dest;
    Result := Recv;
    if Result <> 0 then
    begin
      Dest := SNMPHost;
      Source := Trap.TrapHost;
      Enterprise := Trap.Enterprise;
      Community := Trap.Community;
      Generic := Trap.GenTrap;
      Specific := Trap.SpecTrap;
      Seconds := Trap.TimeTicks;
      MIBName.Clear;
      MIBValue.Clear;
      for i := 0 to Trap.SNMPMibList.Count - 1 do
      begin
        MIBName.Add(TSNMPMib(Trap.SNMPMibList[i]).OID);
        MIBValue.Add(TSNMPMib(Trap.SNMPMibList[i]).Value);
      end;
    end;
  finally
    Free;
  end;
end;

end.
