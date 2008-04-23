{==============================================================================|
| Project : Delphree - Synapse                                   | 001.002.001 |
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
| Portions created by Hernan Sanchez are Copyright (c) 2000.                   |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Hernan Sanchez (hernan.sanchez@iname.com)                                  |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.mlp.cz/space/gebauerl/synapse/)           |
|==============================================================================}

unit SNMPTrap;

interface

uses
  Classes, SysUtils, BlckSock, SynaUtil, ASN1Util;

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
      MIBOID: TStringList;
      MIBValue: TStringList;
      constructor Create;
      destructor Destroy; override;
      procedure Clear;
      procedure MIBAdd(MIB, Value: string);
      procedure MIBDelete(Index: integer);
      function MIBGet(MIB: string): string;
      function EncodeTrap: integer;
      function DecodeTrap: integer;
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
  Generic, Specific, Seconds: integer; MIBName, MIBValue: TStringList): integer;
function RecvTrap(var Dest, Source, Enterprise, Community: string;
  var Generic, Specific, Seconds: integer; var MIBName, MIBValue: TStringList): integer;

implementation

constructor TTrapPDU.Create;
begin
  inherited Create;
  MIBOID := TStringList.Create;
  MIBValue := TStringList.Create;
  TrapPort := TRAP_PORT;
  Version := SNMP_VERSION;
  PDUType := PDU_TRAP;
  Community := 'public';
end;

destructor TTrapPDU.Destroy;
begin
  MIBValue.Free;
  MIBOID.Free;
  inherited Destroy;
end;

procedure TTrapPDU.Clear;
begin
  MIBOID.Clear;
  MIBValue.Clear;
  TrapPort := TRAP_PORT;
  Version := SNMP_VERSION;
  PDUType := PDU_TRAP;
  Community := 'public';
end;

procedure TTrapPDU.MIBAdd(MIB, Value: string);
begin
  MIBOID.Add(MIB);
  MIBValue.Add(Value);
end;

procedure TTrapPDU.MIBDelete(Index: integer);
begin
  MIBOID.Delete(Index);
  MIBValue.Delete(Index);
end;

function TTrapPDU.MIBGet(MIB: string): string;
var
  x: integer;
begin
  x := MIBOID.IndexOf(MIB);
  if (x < 0) then
    Result := ''
  else
    Result := MIBValue[x];
end;

function TTrapPDU.EncodeTrap: integer;
var
  s: string;
  n: integer;
begin
  Buffer := '';
  for n:=0 to MIBOID.Count-1 do
    begin
      s := ASNObject(MibToID(MIBOID[n]), ASN1_OBJID)
        + ASNObject(MIBValue[n], ASN1_OCTSTR);
      Buffer := Buffer + ASNObject(s, ASN1_SEQ);
    end;
  Buffer := ASNObject(Buffer, ASN1_SEQ);
  Buffer := ASNObject(ASNEncInt(GenTrap), ASN1_INT)
    + ASNObject(ASNEncInt(SpecTrap), ASN1_INT)
    + ASNObject(ASNEncInt(TimeTicks), ASN1_TIMETICKS)
    + Buffer;
  Buffer := ASNObject(MibToID(Enterprise), ASN1_OBJID)
    + ASNObject(IPToID(TrapHost), ASN1_IPADDR)
    + Buffer;
  Buffer := ASNObject(Char(Version), ASN1_INT)
    + ASNObject(Community, ASN1_OCTSTR)
    + ASNObject(Buffer, Self.PDUType);
  Buffer := ASNObject(Buffer, ASN1_SEQ);
  Result := 1;
end;

function TTrapPDU.DecodeTrap: integer;
var
  Pos, EndPos: integer;
  Sm, Sv: string;
begin
  Pos := 2;
  EndPos := ASNDecLen(Pos, Buffer);
  Version := StrToIntDef(ASNItem(Pos, Buffer), 0);
  Community := ASNItem(Pos, Buffer);
  PDUType := StrToIntDef(ASNItem(Pos, Buffer), PDU_TRAP);
  Enterprise := IdToMIB(ASNItem(Pos, Buffer));
  TrapHost := ASNItem(Pos, Buffer);
  GenTrap := StrToIntDef(ASNItem(Pos, Buffer), 0);
  Spectrap := StrToIntDef(ASNItem(Pos, Buffer), 0);
  TimeTicks := StrToIntDef(ASNItem(Pos, Buffer), 0);
  ASNItem(Pos, Buffer);
  while (Pos < EndPos) do
    begin
      ASNItem(Pos, Buffer);
      Sm := ASNItem(Pos, Buffer);
      Sv := ASNItem(Pos, Buffer);
      MIBAdd(Sm, Sv);
    end;
  Result := 1;
end;

constructor TTrapSNMP.Create;
begin
  inherited Create;
  Sock := TUDPBlockSocket.Create;
  Trap := TTrapPDU.Create;
  Timeout := 5;
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
          Trap.DecodeTrap;
          Result := 1;
        end;
    end;
end;

function SendTrap(Dest, Source, Enterprise, Community: string;
  Generic, Specific, Seconds: integer; MIBName, MIBValue: TStringList): integer;
var
  SNMP: TTrapSNMP;
  i: integer;
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
    for i:=0 to (MIBName.Count - 1) do
      SNMP.Trap.MIBAdd(MIBName[i], MIBValue[i]);
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
      for i:=0 to (SNMP.Trap.MIBOID.Count - 1) do
        begin
          MIBName.Add(SNMP.Trap.MIBOID[i]);
          MIBValue.Add(SNMP.Trap.MIBValue[i]);
        end;
    end;
  finally
    SNMP.Free;
  end;
end;

end.

