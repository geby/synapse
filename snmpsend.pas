{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
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
| Portions created by Lukas Gebauer are Copyright (c)2000.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.mlp.cz/space/gebauerl/synapse/)           |
|==============================================================================}

unit SNMPSend;

interface

uses
  BlckSock, synautil, classes, sysutils;

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

TSNMPRec=class(TObject)
  private
    procedure SyncMIB;
  public
    version:integer;
    community:string;
    PDUType:integer;
    ID:integer;
    ErrorStatus:integer;
    ErrorIndex:integer;
    MIBOID:TStringList;
    MIBValue:TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure DecodeBuf(Buffer:string);
    function EncodeBuf:string;
    procedure Clear;
    procedure MIBAdd(MIB,Value:string);
    procedure MIBdelete(Index:integer);
    function MIBGet(MIB:string):string;
end;

TSNMPSend=class(TObject)
  private
    Sock:TUDPBlockSocket;
    Buffer:string;
  public
    timeout:integer;
    SNMPhost:string;
    Query:TSNMPrec;
    Reply:TSNMPrec;
    constructor Create;
    destructor Destroy; override;
    function DoIt:boolean;
end;

function SNMPget (Mib, Community, SNMPHost:string; var Value:string):Boolean;

implementation

{==============================================================================}

{TSNMPRec.Create}
constructor TSNMPRec.Create;
begin
  inherited create;
  MIBOID:=TStringList.create;
  MIBValue:=TStringList.create;
end;

{TSNMPRec.Destroy}
destructor TSNMPRec.Destroy;
begin
  MIBValue.Free;
  MIBOID.Free;
  inherited destroy;
end;

{TSNMPRec.SyncMIB}
procedure TSNMPRec.SyncMIB;
var
  n,x:integer;
begin
  x:=MIBValue.Count;
  for n:=x to MIBOID.Count-1 do
    MIBValue.Add('');
end;

{TSNMPRec.DecodeBuf}
procedure TSNMPRec.DecodeBuf(Buffer:string);
var
  Pos:integer;
  endpos:integer;
  sm,sv:string;

    function ASNlen(var start:integer):integer;
    var
      x:integer;
    begin
      x:=ord(buffer[start]);
      if x>$80 then
        begin
          inc(start);
          x:=x and $7f;
          x:=x*$80;
          x:=x+ord(buffer[start]);
        end;
      inc(start);
      result:=x;
    end;

    function ASNitem (var start:integer):string;
    var
      ASNType:integer;
      ASNSize:integer;
      y,n:integer;
      s:string;
      c:char;
    begin
      ASNType:=Ord(Buffer[start]);
      Inc(start);
      ASNSize:=ASNLen(start);
      Result:='';
      if (ASNType and $20)>0 then
        begin
          Result:='$'+IntToHex(ASNType,2);
        end
        else
          case ASNType of
            2, $41, $42, $43:  begin //integer
                  y:=0;
                  for n:=1 to ASNsize do
                    begin
                      y:=y*256+ord(buffer[start]);
                      inc(start);
                    end;
                  result:=inttostr(y);
                end;
            4, $44:  begin //string
                  for n:=1 to ASNSize do
                    begin
                      c:=char(buffer[start]);
                      inc(start);
                      s:=s+c;
                    end;
                  Result:=s;
                end;
            6:  begin //OID
                  for n:=1 to ASNsize do
                    begin
                      c:=char(buffer[start]);
                      inc(start);
                      s:=s+c;
                    end;
                  result:=IdToMib(s);
                end;
            $40:  begin //IP address
                  s:='';
                  for n:=1 to ASNsize do
                    begin
                      if n<>1 then
                        s:=s+'.';
                      y:=Ord(buffer[start]);
                      inc(start);
                      s:=s+IntToStr(y);
                    end;
                  result:=s;
                end;
            else //NULL
              begin
                Result:='';
                inc(start);
                start:=start+ASNSize;
              end;
      end;
    end;

begin
  Pos:=2;
  Endpos:=ASNLen(Pos);
  Self.version:=StrToIntDef(ASNItem(Pos),0);
  Self.community:=ASNItem(Pos);
  Self.PDUType:=StrToIntDef(ASNItem(Pos),0);
  Self.ID:=StrToIntDef(ASNItem(Pos),0);
  Self.ErrorStatus:=StrToIntDef(ASNItem(Pos),0);
  Self.ErrorIndex:=StrToIntDef(ASNItem(Pos),0);
  ASNItem(Pos);
  while Pos<Endpos do
    begin
      ASNItem(Pos);
      Sm:=ASNItem(Pos);
      Sv:=ASNItem(Pos);
      Self.MIBadd(sm,sv);
    end;
end;

{TSNMPRec.EncodeBuf}
function TSNMPRec.EncodeBuf:string;
var
  data,s:string;
  n:integer;

    function ASNEncLen (len:integer):string;
    var
      x,y:integer;
    begin
      Result:='';
      x:=len div $80;
      y:=len mod $80;
      if x>0 then Result:=char(x);
      Result:=Result+char(y);
    end;

    function ASNObject (data:string;ASNType:integer):string;
    begin
      Result:=char(ASNType)+ASNEncLen(Length(data))+data;
    end;

begin
  data:='';
  SyncMIB;
  for n:=0 to Self.MIBOID.Count-1 do
    begin
      s:=ASNObject(MibToID(Self.MIBOID[n]),6)+ASNObject(Self.MIBValue[n],4);
      data:=data+ASNObject(s,$30);
    end;
  data:=ASNObject(data,$30);
  data:=ASNObject(char(Self.ID),2)
    +ASNObject(char(Self.ErrorStatus),2)
    +ASNObject(char(Self.ErrorIndex),2)
    +data;
  data:=ASNObject(char(Self.Version),2)
    +ASNObject(Self.community,4)
    +ASNObject(data,Self.PDUType);
  data:=ASNObject(data,$30);
  Result:=data;
end;

{TSNMPRec.Clear}
procedure TSNMPRec.Clear;
begin
  version:=0;
  community:='';
  PDUType:=0;
  ID:=0;
  ErrorStatus:=0;
  ErrorIndex:=0;
  MIBOID.Clear;
  MIBValue.Clear;
end;

{TSNMPRec.MIBAdd}
procedure TSNMPRec.MIBAdd(MIB,Value:string);
var
  x:integer;
begin
  SyncMIB;
  MIBOID.Add(MIB);
  x:=MIBOID.Count;
  if MIBValue.Count>x then MIBvalue[x-1]:=value
    else MIBValue.Add(Value);
end;

{TSNMPRec.MIBdelete}
procedure TSNMPRec.MIBdelete(Index:integer);
begin
  SyncMIB;
  MIBOID.Delete(Index);
  if (MIBValue.Count-1)>= Index then MIBValue.Delete(Index);
end;

{TSNMPRec.MIBGet}
function TSNMPRec.MIBGet(MIB:string):string;
var
  x:integer;
begin
  SyncMIB;
  x:=MIBOID.IndexOf(MIB);
  if x<0 then Result:=''
    else Result:=MIBValue[x];
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
  timeout:=5;
  SNMPhost:='localhost';
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
  sock.connect(SNMPhost,'161');
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
  if Result then reply.DecodeBuf(Buffer);
end;

{==============================================================================}

function SNMPget (Mib, Community, SNMPHost:string; var Value:string):Boolean;
var
  SNMP:TSNMPSend;
begin
  Result:=False;
  SNMP:=TSNMPSend.Create;
  try
    Snmp.Query.community:=Community;
    Snmp.Query.PDUType:=PDUGetRequest;
    Snmp.Query.MIBAdd(MIB,'');
    Snmp.SNMPhost:=SNMPHost;
    Result:=Snmp.DoIt;
    if Result then
      Value:=Snmp.Reply.MIBGet(MIB);
  finally
    SNMP.Free;
  end;
end;


end.
