{==============================================================================|
| Project : Ararat Synapse                                       | 002.003.004 |
|==============================================================================|
| Content: SNMP traps                                                          |
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
| Portions created by Hernan Sanchez are Copyright (c)2000-2003.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Hernan Sanchez (hernan.sanchez@iname.com)                                  |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$Q-}
{$H+}

unit snmptrap;

interface

uses
  Classes, SysUtils,
  blcksock, synautil, asn1util, snmpsend;

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
    property Enterprise: string read FEnterprise Write FEnterprise;
    property TrapHost: string read FTrapHost Write FTrapHost;
    property GenTrap: Integer read FGenTrap Write FGenTrap;
    property SpecTrap: Integer read FSpecTrap Write FSpecTrap;
    property TimeTicks: Integer read FTimeTicks Write FTimeTicks;
    property SNMPMibList: TList read FSNMPMibList;
  end;

  TTrapSNMP = class(TSynaClient)
  private
    FSock: TUDPBlockSocket;
    FTrap: TTrapPDU;
  public
    constructor Create;
    destructor Destroy; override;
    function Send: Integer;
    function Recv: Integer;
  published
    property Trap: TTrapPDU read FTrap;
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
  FTargetPort := cSnmpTrapProtocol;
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
  FSock.Bind(FIPInterface, cAnyPort);
  FSock.Connect(FTargetHost, FTargetPort);
  FSock.SendString(FTrap.FBuffer);
  Result := 1;
end;

function TTrapSNMP.Recv: Integer;
begin
  Result := 0;
  FSock.Bind(FIPInterface, FTargetPort);
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
    TargetHost := Dest;
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
    TargetHost := Dest;
    Result := Recv;
    if Result <> 0 then
    begin
      Dest := TargetHost;
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
