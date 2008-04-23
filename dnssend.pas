{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
|==============================================================================|
| Content: DNS client                                                          |
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

//RFC-1035, RFC-1183, RFC1706, RFC1712, RFC2163, RFC2230

unit DNSsend;

interface
uses
  Blcksock, sysutils, classes, SynaUtil, dialogs;

const
  Qtype_A     =1;
  Qtype_NS    =2;
  Qtype_MD    =3;
  Qtype_MF    =4;
  Qtype_CNAME =5;
  Qtype_SOA   =6;
  Qtype_MB    =7;
  Qtype_MG    =8;
  Qtype_MR    =9;
  Qtype_NULL  =10;
  Qtype_WKS   =11;  //
  Qtype_PTR   =12;
  Qtype_HINFO =13;
  Qtype_MINFO =14;
  Qtype_MX    =15;
  Qtype_TXT   =16;

  Qtype_RP    =17;
  Qtype_AFSDB =18;
  Qtype_X25   =19;
  Qtype_ISDN  =20;
  Qtype_RT    =21;
  Qtype_NSAP  =22;
  Qtype_NSAPPTR=23;
  Qtype_SIG   =24; //RFC-2065
  Qtype_KEY   =25; //RFC-2065
  Qtype_PX    =26;
  Qtype_GPOS  =27;
  Qtype_AAAA  =28; //IP6 Address                     [Susan Thomson]
  Qtype_LOC   =29; //RFC-1876
  Qtype_NXT   =30; //RFC-2065
  
  Qtype_SRV   =33; //RFC-2052
  Qtype_NAPTR =35; //RFC-2168
  Qtype_KX    =36;

  Qtype_AXFR  =252; //
  Qtype_MAILB =253; //
  Qtype_MAILA =254; //
  Qtype_ALL   =255; //

type
  TDNSSend = class
  private
    Buffer:string;
    Sock:TUDPBlockSocket;
    function CompressName(Value:string):string;
    function CodeHeader:string;
    function CodeQuery(Name:string; Qtype:integer):string;
    function DecodeLabels(var From:integer):string;
    function DecodeResource(var i:integer; Name:string; Qtype:integer):string;
  public
    timeout:integer;
    DNSHost:string;
    RCode:integer;
    Constructor Create;
    Destructor Destroy; override;
    Function DNSQuery(Name:string;Qtype:integer;Reply:TStrings):Boolean;
  end;

function GetMailServers (DNSHost, domain:string; servers:TStringList):Boolean;

implementation

{TDNSSend.Create}
Constructor TDNSSend.Create;
begin
  inherited Create;
  sock:=TUDPBlockSocket.create;
  sock.CreateSocket;
  timeout:=5;
  DNShost:='localhost';
end;

{TDNSSend.Destroy}
Destructor TDNSSend.Destroy;
begin
  Sock.free;
  inherited destroy;
end;

{TDNSSend.ComressName}
function TDNSSend.CompressName(Value:string):string;
var
  n:integer;
  s:String;
begin
  Result:='';
  if Value='' then Result:=char(0)
    else
      begin
        s:='';
        for n:=1 to Length(Value) do
          if Value[n]='.' then
            begin
              Result:=Result+char(Length(s))+s;
              s:='';
            end
            else s:=s+Value[n];
        if s<>'' then Result:=Result+char(Length(s))+s;
        Result:=Result+char(0);
      end;
end;

{TDNSSend.CodeHeader}
function TDNSSend.CodeHeader:string;
begin
  Randomize;
  Result:=Codeint(Random(32767)); //ID
  Result:=Result+Codeint($0100);  //flags
  Result:=Result+Codeint(1);   //QDCount
  Result:=Result+Codeint(0);   //ANCount
  Result:=Result+Codeint(0);   //NSCount
  Result:=Result+Codeint(0);   //ARCount
end;

{TDNSSend.CodeQuery}
function TDNSSend.CodeQuery(Name:string; Qtype:integer):string;
begin
  Result:=Compressname(Name);
  Result:=Result+Codeint(Qtype);
  Result:=Result+Codeint(1); //Type INTERNET
end;

{TDNSSend.DecodeLabels}
function TDNSSend.DecodeLabels(var From:integer):string;
var
  l,f:integer;
begin
  Result:='';
  while True do
    begin
      l:=Ord(Buffer[From]);
      Inc(From);
      if l=0 then break;
      if Result<>'' then Result:=Result+'.';
      if (l and $C0)=$C0
      then
        begin
          f:=l and $3F;
          f:=f*256+Ord(Buffer[From])+1;
          Inc(From);
          Result:=Result+Self.decodelabels(f);
          break;
        end
      else
        begin
          Result:=Result+Copy(Buffer,From,l);
          Inc(From,l);
        end;
    end;
end;

{TDNSSend.DecodeResource}
function TDNSSend.DecodeResource(var i:integer; Name:string;
Qtype:integer):string;
var
  Rname:string;
  RType,Len,j,x,n:integer;
begin
  Result:='';
  Rname:=decodelabels(i);
  Rtype:=DeCodeint(Buffer,i);
  Inc(i,8);
  Len:=DeCodeint(Buffer,i);
  Inc(i,2);   //i point to begin of data
  j:=i;
  i:=i+len;   //i point to next record
  if (Name=Rname) and (Qtype=RType) then
    begin
      case Rtype of
        Qtype_A     :
                      begin
                        Result:=IntToStr(Ord(Buffer[j]));
                        Inc(j);
                        Result:=Result+'.'+IntToStr(Ord(Buffer[j]));
                        Inc(j);
                        Result:=Result+'.'+IntToStr(Ord(Buffer[j]));
                        Inc(j);
                        Result:=Result+'.'+IntToStr(Ord(Buffer[j]));
                      end;
        Qtype_NS,
        Qtype_MD,
        Qtype_MF,
        Qtype_CNAME,
        Qtype_MB,
        Qtype_MG,
        Qtype_MR,
        Qtype_PTR,
        Qtype_X25,
        Qtype_NSAP,
        Qtype_NSAPPTR:
                      begin
                        Result:=Decodelabels(j);
                      end;
        Qtype_SOA   :
                      begin
                        Result:=Decodelabels(j);
                        Result:=Result+','+Decodelabels(j);
                        for n:=1 to 5 do
                          begin
                            x:=DecodeInt(Buffer,j)*65536+DecodeInt(Buffer,j+2);
                            Inc(j,4);
                            Result:=Result+','+IntToStr(x);
                          end;
                      end;
        Qtype_NULL  :
                      begin
                      end;
        Qtype_WKS   :
                      begin
                      end;
        Qtype_HINFO,
        Qtype_MINFO,
        Qtype_RP,
        Qtype_ISDN  :
                      begin
                        Result:=Decodelabels(j);
                        Result:=Result+','+Decodelabels(j);
                      end;
        Qtype_MX,
        Qtype_AFSDB,
        Qtype_RT,
        Qtype_KX    :
                      begin
                        x:=DecodeInt(Buffer,j);
                        Inc(j,2);
                        Result:=IntToStr(x);
                        Result:=Result+','+Decodelabels(j);
                      end;
        Qtype_TXT   :
                      begin
                        Result:=Decodelabels(j);
                      end;
        Qtype_GPOS  :
                      begin
                        Result:=Decodelabels(j);
                        Result:=Result+','+Decodelabels(j);
                        Result:=Result+','+Decodelabels(j);
                      end;
        Qtype_PX    :
                      begin
                        x:=DecodeInt(Buffer,j);
                        Inc(j,2);
                        Result:=IntToStr(x);
                        Result:=Result+','+Decodelabels(j);
                        Result:=Result+','+Decodelabels(j);
                      end;
      end;
    end;
end;

{TDNSSend.DNSQuery}
Function TDNSSend.DNSQuery(Name:string;Qtype:integer;Reply:TStrings):Boolean;
var
  x,n,i:integer;
  flag,qdcount, ancount, nscount, arcount:integer;
  s:string;
begin
  Result:=False;
  Reply.Clear;
  if IsIP(Name) then Name:=ReverseIP(Name)+'.in-addr.arpa';
  Buffer:=Codeheader+CodeQuery(Name,QType);
  sock.connect(DNSHost,'domain');
//  dump(Buffer,'c:\dnslog.Txt');
  sock.sendstring(Buffer);
  if sock.canread(timeout)
    then begin
      x:=sock.waitingdata;
      setlength(Buffer,x);
      sock.recvbuffer(Pointer(Buffer),x);
//      dump(Buffer,'c:\dnslogr.Txt');
      flag:=DeCodeint(Buffer,3);
      RCode:=Flag and $000F;
      if RCode=0 then
        begin
          qdcount:=DeCodeint(Buffer,5);
          ancount:=DeCodeint(Buffer,7);
          nscount:=DeCodeint(Buffer,9);
          arcount:=DeCodeint(Buffer,11);
          i:=13;    //begin of body
          if qdcount>0 then   //skip questions
            for n:=1 to qdcount do
              begin
                while (Buffer[i]<>#0) and ((Ord(Buffer[i]) and $C0)<>$C0) do
                  Inc(i);
                Inc(i,5);
              end;
          if ancount>0 then
            for n:=1 to ancount do
              begin
                s:=DecodeResource(i, Name, Qtype);
                if s<>'' then
                  Reply.Add(s);
              end;
          Result:=True;
        end;
    end;
end;

{==============================================================================}

function GetMailServers (DNSHost, domain:string; servers:TStringList):Boolean;
var
  DNS:TDNSSend;
  t:TStringList;
  n,m,x:integer;
begin
  Result:=False;
  servers.Clear;
  t:=TStringList.Create;
  DNS:=TDNSSend.Create;
  try
    DNS.DNSHost:=DNSHost;
    if DNS.DNSQuery(domain,QType_MX,t) then
      begin
        {normalize preference number to 5 digits}
        for n:=0 to t.Count-1 do
          begin
            x:=Pos(',',t[n]);
            if x>0 then
              for m:=1 to 6-x do
                t[n]:='0'+t[n];
          end;
        {sort server list}
        t.Sorted:=True;
        {result is sorted list without preference numbers}
        for n:=0 to t.Count-1 do
          begin
            x:=Pos(',',t[n]);
            servers.Add(Copy(t[n],x+1,Length(t[n])-x));
          end;
        Result:=True;
      end;
  finally
    DNS.Free;
    t.Free;
  end;
end;

end.


