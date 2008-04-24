{==============================================================================|
| Project : Delphree - Synapse                                   | 002.003.000 |
|==============================================================================|
| Content: SNMP client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2000,2001.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|   Jean-Fabien Connault (jfconnault@mail.dotcom.fr)                           |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

unit SNMPSend;

interface

uses
  BlckSock, synautil, classes, sysutils, ASN1util;

const

//PDU type
PDUGetRequest=$a0;
PDUGetNextRequest=$a1;
PDUGetResponse=$a2;
PDUSetRequest=$a3;
PDUTrap=$a4;

//errors
ENoError=0;
ETooBig=1;
ENoSuchName=2;
EBadValue=3;
EReadOnly=4;
EGenErr=5;

type

TSNMPMib = class
  OID: string;
  Value: string;
  ValueType: integer;
end;

TSNMPRec=class(TObject)
  public
    version:integer;
    community:string;
    PDUType:integer;
    ID:integer;
    ErrorStatus:integer;
    ErrorIndex:integer;
    SNMPMibList: TList;
    constructor Create;
    destructor Destroy; override;
    function DecodeBuf(Buffer:string):boolean;
    function EncodeBuf:string;
    procedure Clear;
    procedure MIBAdd(MIB,Value:string; ValueType:integer);
    procedure MIBdelete(Index:integer);
    function MIBGet(MIB:string):string;
end;

TSNMPSend=class(TObject)
  private
    Sock:TUDPBlockSocket;
    Buffer:string;
  public
    Timeout:integer;
    Host:string;
    HostIP:string;
    Query:TSNMPrec;
    Reply:TSNMPrec;
    constructor Create;
    destructor Destroy; override;
    function DoIt:boolean;
end;

function SNMPget (Oid, Community, SNMPHost:string; var Value:string):Boolean;
function SNMPSet (Oid, Community, SNMPHost, Value: string; ValueType: integer): boolean;

implementation

{==============================================================================}

{TSNMPRec.Create}
constructor TSNMPRec.Create;
begin
  inherited create;
  SNMPMibList := TList.create;
  id:=1;
end;

{TSNMPRec.Destroy}
destructor TSNMPRec.Destroy;
var
  i:integer;
begin
  for i := 0 to SNMPMibList.count - 1 do
    TSNMPMib(SNMPMibList[i]).Free;
  SNMPMibList.free;
  inherited destroy;
end;

{TSNMPRec.DecodeBuf}
function TSNMPRec.DecodeBuf(Buffer:string):boolean;
var
  Pos:integer;
  endpos:integer;
  sm,sv:string;
  svt: integer;
begin
  result:=false;
  if length(buffer)<2
    then exit;
  if (ord(buffer[1]) and $20)=0
    then exit;
  Pos:=2;
  Endpos:=ASNDecLen(Pos,buffer);
  if length(buffer)<(Endpos+2)
    then exit;
  Self.version:=StrToIntDef(ASNItem(Pos,buffer,svt),0);
  Self.community:=ASNItem(Pos,buffer,svt);
  Self.PDUType:=StrToIntDef(ASNItem(Pos,buffer,svt),0);
  Self.ID:=StrToIntDef(ASNItem(Pos,buffer,svt),0);
  Self.ErrorStatus:=StrToIntDef(ASNItem(Pos,buffer,svt),0);
  Self.ErrorIndex:=StrToIntDef(ASNItem(Pos,buffer,svt),0);
  ASNItem(Pos,buffer,svt);
  while Pos<Endpos do
    begin
      ASNItem(Pos,buffer,svt);
      Sm:=ASNItem(Pos,buffer,svt);
      Sv:=ASNItem(Pos,buffer,svt);
      Self.MIBadd(sm,sv, svt);
    end;
  result:=true;
end;

{TSNMPRec.EncodeBuf}
function TSNMPRec.EncodeBuf:string;
var
  data,s:string;
  SNMPMib: TSNMPMib;
  n:integer;
begin
  data:='';
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
      data := data + ASNObject(s, ASN1_SEQ);
    end;
  data:=ASNObject(data,ASN1_SEQ);
  data:=ASNObject(char(Self.ID),ASN1_INT)
    +ASNObject(char(Self.ErrorStatus),ASN1_INT)
    +ASNObject(char(Self.ErrorIndex),ASN1_INT)
    +data;
  data:=ASNObject(char(Self.Version),ASN1_INT)
    +ASNObject(Self.community,ASN1_OCTSTR)
    +ASNObject(data,Self.PDUType);
  data:=ASNObject(data,ASN1_SEQ);
  Result:=data;
end;

{TSNMPRec.Clear}
procedure TSNMPRec.Clear;
var
  i:integer;
begin
  version:=0;
  community:='';
  PDUType:=0;
  ErrorStatus:=0;
  ErrorIndex:=0;
  for i := 0 to SNMPMibList.count - 1 do
    TSNMPMib(SNMPMibList[i]).Free;
  SNMPMibList.Clear;
end;

{TSNMPRec.MIBAdd}
procedure TSNMPRec.MIBAdd(MIB,Value:string; ValueType:integer);
var
  SNMPMib: TSNMPMib;
begin
  SNMPMib := TSNMPMib.Create;
  SNMPMib.OID := MIB;
  SNMPMib.Value := Value;
  SNMPMib.ValueType := ValueType;
  SNMPMibList.Add(SNMPMib);
end;

{TSNMPRec.MIBdelete}
procedure TSNMPRec.MIBdelete(Index:integer);
begin
  if (Index >= 0) and (Index < SNMPMibList.count) then
    begin
      TSNMPMib(SNMPMibList[Index]).Free;
      SNMPMibList.Delete(Index);
    end;
end;

{TSNMPRec.MIBGet}
function TSNMPRec.MIBGet(MIB:string):string;
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


{==============================================================================}

{TSNMPSend.Create}
constructor TSNMPSend.Create;
begin
  inherited create;
  Query:=TSNMPRec.Create;
  Reply:=TSNMPRec.Create;
  Query.Clear;
  Reply.Clear;
  sock:=TUDPBlockSocket.create;
  sock.createsocket;
  timeout:=5000;
  host:='localhost';
  HostIP:='';
end;

{TSNMPSend.Destroy}
destructor TSNMPSend.Destroy;
begin
  Sock.Free;
  Reply.Free;
  Query.Free;
  inherited destroy;
end;

{TSNMPSend.DoIt}
function TSNMPSend.DoIt:boolean;
var
  x:integer;
begin
  Result:=false;
  reply.clear;
  Buffer:=Query.Encodebuf;
  sock.connect(host,'161');
  HostIP:=sock.GetRemoteSinIP;
  sock.SendBuffer(PChar(Buffer),Length(Buffer));
  if sock.canread(timeout)
    then begin
      x:=sock.WaitingData;
      if x>0 then
        begin
          setlength(Buffer,x);
          sock.RecvBuffer(PChar(Buffer),x);
          result:=true;
        end;
    end;
  if Result
    then result:=reply.DecodeBuf(Buffer);
end;

{==============================================================================}

function SNMPget (Oid, Community, SNMPHost:string; var Value:string):Boolean;
var
  SNMP:TSNMPSend;
begin
  SNMP:=TSNMPSend.Create;
  try
    Snmp.Query.community:=Community;
    Snmp.Query.PDUType:=PDUGetRequest;
    Snmp.Query.MIBAdd(Oid,'',ASN1_NULL);
    Snmp.host:=SNMPHost;
    Result:=Snmp.DoIt;
    if Result then
      Value:=Snmp.Reply.MIBGet(Oid);
  finally
    SNMP.Free;
  end;
end;

function SNMPSet(Oid, Community, SNMPHost, Value: string; ValueType: integer): boolean;
var
  SNMPSend: TSNMPSend;
begin
  SNMPSend := TSNMPSend.Create;
  try
    SNMPSend.Query.community := Community;
    SNMPSend.Query.PDUType := PDUSetRequest;
    SNMPSend.Query.MIBAdd(Oid, Value, ValueType);
    SNMPSend.Host := SNMPHost;
    result:= SNMPSend.DoIt=true;
  finally
    SNMPSend.Free;
  end;
end;


end.
