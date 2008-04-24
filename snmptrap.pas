{==============================================================================|
| Project : Delphree - Synapse                                   | 002.002.001 |
|==============================================================================|
| Content: SNMP traps                                                          |
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

unit SNMPTrap;

interface

uses
  Classes, SysUtils, BlckSock, SynaUtil, ASN1Util, SNMPsend;

const
  TRAP_PORT      = 162;

  SNMP_VERSION   = 0;

  PDU_GET        = $A0;
  PDU_GETN       = $A1;
  PDU_RESP       = $A2;
  PDU_SET        = $A3;
  PDU_TRAP       = $A4;

type
  TTrapPDU = class(TObject)
    private
    protected
      Buffer: string;
    public
      TrapPort: integer;
      Version: integer;
      PDUType: integer;
      Community: string;
      Enterprise: string;
      TrapHost: string;
      GenTrap: integer;
      SpecTrap: integer;
      TimeTicks: integer;
      SNMPMibList: TList;
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      procedure MIBAdd(MIB, Value: string; ValueType:integer);
      procedure MIBDelete(Index: integer);
      function MIBGet(MIB: string): string;
      function EncodeTrap: integer;
      function DecodeTrap: boolean;
  end;

  TTrapSNMP = class(TObject)
  private
    sock: TUDPBlockSocket;
  public
    Trap: TTrapPDU;
    SNMPHost: string;
    Timeout: integer;
    constructor Create;
    destructor Destroy; override;
    function Send: integer;
    function Recv: integer;
  end;

function SendTrap(Dest, Source, Enterprise, Community: string;
  Generic, Specific, Seconds: integer; MIBName, MIBValue: string; MIBtype:integer): integer;
function RecvTrap(var Dest, Source, Enterprise, Community: string;
  var Generic, Specific, Seconds: integer; var MIBName, MIBValue: TStringList): integer;

implementation

constructor TTrapPDU.Create;
begin
  inherited Create;
  SNMPMibList := TList.create;
  TrapPort := TRAP_PORT;
  Version := SNMP_VERSION;
  PDUType := PDU_TRAP;
  Community := 'public';
end;

destructor TTrapPDU.Destroy;
var
  i:integer;
begin
  for i := 0 to SNMPMibList.count - 1 do
    TSNMPMib(SNMPMibList[i]).Free;
  SNMPMibList.free;
  inherited Destroy;
end;

procedure TTrapPDU.Clear;
var
  i:integer;
begin
  for i := 0 to SNMPMibList.count - 1 do
    TSNMPMib(SNMPMibList[i]).Free;
  SNMPMibList.Clear;
  TrapPort := TRAP_PORT;
  Version := SNMP_VERSION;
  PDUType := PDU_TRAP;
  Community := 'public';
end;

procedure TTrapPDU.MIBAdd(MIB, Value: string; ValueType:integer);
var
  SNMPMib: TSNMPMib;
begin
  SNMPMib := TSNMPMib.Create;
  SNMPMib.OID := MIB;
  SNMPMib.Value := Value;
  SNMPMib.ValueType := ValueType;
  SNMPMibList.Add(SNMPMib);
end;

procedure TTrapPDU.MIBDelete(Index: integer);
begin
  if (Index >= 0) and (Index < SNMPMibList.count) then
    begin
      TSNMPMib(SNMPMibList[Index]).Free;
      SNMPMibList.Delete(Index);
    end;
end;

function TTrapPDU.MIBGet(MIB: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to SNMPMibList.count - 1 do
    begin
      if ((TSNMPMib(SNMPMibList[i])).OID = MIB) then
      begin
        Result := (TSNMPMib(SNMPMibList[i])).Value;
        break;
      end;
    end;
end;

function TTrapPDU.EncodeTrap: integer;
var
  s: string;
  n: integer;
  SNMPMib: TSNMPMib;
begin
  Buffer := '';
  for n:=0 to SNMPMibList.Count-1 do
    begin
      SNMPMib := SNMPMibList[n];
      case (SNMPMib.ValueType) of
        ASN1_INT:
          begin
            s := ASNObject(MibToID(SNMPMib.OID),ASN1_OBJID)
              +ASNObject(ASNEncInt(strToIntDef(SNMPMib.Value,0)),SNMPMib.ValueType);
          end;
        ASN1_COUNTER, ASN1_GAUGE, ASN1_TIMETICKS:
          begin
            s := ASNObject(MibToID(SNMPMib.OID),ASN1_OBJID)
              +ASNObject(ASNEncUInt(strToIntDef(SNMPMib.Value,0)),SNMPMib.ValueType);
          end;
        ASN1_OBJID:
          begin
            s := ASNObject(MibToID(SNMPMib.OID),ASN1_OBJID) + ASNObject(MibToID(SNMPMib.Value),SNMPMib.ValueType);
          end;
        ASN1_IPADDR:
          begin
            s := ASNObject(MibToID(SNMPMib.OID),ASN1_OBJID) + ASNObject(IPToID(SNMPMib.Value),SNMPMib.ValueType);
          end;
        ASN1_NULL:
          begin
            s := ASNObject(MibToID(SNMPMib.OID),ASN1_OBJID) + ASNObject('',ASN1_NULL);
          end;
        else
          s := ASNObject(MibToID(SNMPMib.OID),ASN1_OBJID) + ASNObject(SNMPMib.Value,SNMPMib.ValueType);
      end;
      Buffer := Buffer + ASNObject(s, ASN1_SEQ);
    end;
  Buffer := ASNObject(Buffer, ASN1_SEQ);
  Buffer := ASNObject(ASNEncInt(GenTrap), ASN1_INT)
    + ASNObject(ASNEncInt(SpecTrap), ASN1_INT)
    + ASNObject(ASNEncUInt(TimeTicks), ASN1_TIMETICKS)
    + Buffer;
  Buffer := ASNObject(MibToID(Enterprise), ASN1_OBJID)
    + ASNObject(IPToID(TrapHost), ASN1_IPADDR)
    + Buffer;
  Buffer := ASNObject(ASNEncInt(Version), ASN1_INT)
    + ASNObject(Community, ASN1_OCTSTR)
    + ASNObject(Buffer, Self.PDUType);
  Buffer := ASNObject(Buffer, ASN1_SEQ);
  Result := 1;
end;

function TTrapPDU.DecodeTrap: boolean;
var
  Pos, EndPos: integer;
  Sm, Sv: string;
  Svt:integer;
begin
  clear;
  result:=false;
  if length(buffer)<2
    then exit;
  if (ord(buffer[1]) and $20)=0
    then exit;
  Pos := 2;
  EndPos := ASNDecLen(Pos, Buffer);
  Version := StrToIntDef(ASNItem(Pos, Buffer,svt), 0);
  Community := ASNItem(Pos, Buffer,svt);
  PDUType := StrToIntDef(ASNItem(Pos, Buffer,svt), PDU_TRAP);
  Enterprise := ASNItem(Pos, Buffer,svt);
  TrapHost := ASNItem(Pos, Buffer,svt);
  GenTrap := StrToIntDef(ASNItem(Pos, Buffer,svt), 0);
  Spectrap := StrToIntDef(ASNItem(Pos, Buffer,svt), 0);
  TimeTicks := StrToIntDef(ASNItem(Pos, Buffer,svt), 0);
  ASNItem(Pos, Buffer,svt);
  while (Pos < EndPos) do
    begin
      ASNItem(Pos, Buffer,svt);
      Sm := ASNItem(Pos, Buffer,svt);
      Sv := ASNItem(Pos, Buffer,svt);
      MIBAdd(Sm, Sv, svt);
    end;
  Result := true;
end;

constructor TTrapSNMP.Create;
begin
  inherited Create;
  Sock := TUDPBlockSocket.Create;
  Trap := TTrapPDU.Create;
  Timeout := 5000;
  SNMPHost := '127.0.0.1';
  Sock.CreateSocket;
end;

destructor TTrapSNMP.Destroy;
begin
  Trap.Free;
  Sock.Free;
  inherited Destroy;
end;

function TTrapSNMP.Send: integer;
begin
  Trap.EncodeTrap;
  Sock.Connect(SNMPHost, IntToStr(Trap.TrapPort));
  Sock.SendBuffer(PChar(Trap.Buffer), Length(Trap.Buffer));
  Result := 1;
end;

function TTrapSNMP.Recv: integer;
var
  x: integer;
begin
  Result := 0;
  Sock.Bind('0.0.0.0', IntToStr(Trap.TrapPort));
  if Sock.CanRead(Timeout) then
    begin
      x := Sock.WaitingData;
      if (x > 0) then
        begin
          SetLength(Trap.Buffer, x);
          Sock.RecvBuffer(PChar(Trap.Buffer), x);
          if Trap.DecodeTrap
            then Result:=1;
        end;
    end;
end;

function SendTrap(Dest, Source, Enterprise, Community: string;
  Generic, Specific, Seconds: integer; MIBName, MIBValue: string; MIBtype:integer): integer;
var
  SNMP: TTrapSNMP;
begin
  SNMP := TTrapSNMP.Create;
  try
    SNMP.SNMPHost := Dest;
    SNMP.Trap.TrapHost := Source;
    SNMP.Trap.Enterprise := Enterprise;
    SNMP.Trap.Community := Community;
    SNMP.Trap.GenTrap := Generic;
    SNMP.Trap.SpecTrap := Specific;
    SNMP.Trap.TimeTicks := Seconds;
    SNMP.Trap.MIBAdd(MIBName,MIBValue,MIBType);
    Result := SNMP.Send;
  finally
    SNMP.Free;
  end;
end;

function RecvTrap(var Dest, Source, Enterprise, Community: string;
  var Generic, Specific, Seconds: integer; var MIBName, MIBValue: TStringList):
integer;
var
  SNMP: TTrapSNMP;
  i: integer;
begin
  SNMP := TTrapSNMP.Create;
  try
    SNMP.SNMPHost := Dest;
    Result := SNMP.Recv;
    if (Result <> 0) then
    begin
      Dest := SNMP.SNMPHost;
      Source := SNMP.Trap.TrapHost;
      Enterprise := SNMP.Trap.Enterprise;
      Community := SNMP.Trap.Community;
      Generic := SNMP.Trap.GenTrap;
      Specific := SNMP.Trap.SpecTrap;
      Seconds := SNMP.Trap.TimeTicks;
      MIBName.Clear;
      MIBValue.Clear;
      for i:=0 to (SNMP.Trap.SNMPMibList.count - 1) do
        begin
          MIBName.Add(TSNMPMib(SNMP.Trap.SNMPMibList[i]).OID);
          MIBValue.Add(TSNMPMib(SNMP.Trap.SNMPMibList[i]).Value);
        end;
    end;
  finally
    SNMP.Free;
  end;
end;

end.

