{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.011 |
|==============================================================================|
| Content: LDAP client                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2003.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

//RFC-2251, RFC-2254, RFC-2829, RFC-2830

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit ldapsend;

interface

uses
  Classes, SysUtils,
  {$IFDEF STREAMSEC}
  TlsInternalServer, TlsSynaSock,
  {$ENDIF}
  blcksock, synautil, asn1util, synacode;

const
  cLDAPProtocol = '389';

  LDAP_ASN1_BIND_REQUEST = $60;
  LDAP_ASN1_BIND_RESPONSE = $61;
  LDAP_ASN1_UNBIND_REQUEST = $42;
  LDAP_ASN1_SEARCH_REQUEST = $63;
  LDAP_ASN1_SEARCH_ENTRY = $64;
  LDAP_ASN1_SEARCH_DONE = $65;
  LDAP_ASN1_SEARCH_REFERENCE = $73;
  LDAP_ASN1_MODIFY_REQUEST = $66;
  LDAP_ASN1_MODIFY_RESPONSE = $67;
  LDAP_ASN1_ADD_REQUEST = $68;
  LDAP_ASN1_ADD_RESPONSE = $69;
  LDAP_ASN1_DEL_REQUEST = $4A;
  LDAP_ASN1_DEL_RESPONSE = $6B;
  LDAP_ASN1_MODIFYDN_REQUEST = $6C;
  LDAP_ASN1_MODIFYDN_RESPONSE = $6D;
  LDAP_ASN1_COMPARE_REQUEST = $6E;
  LDAP_ASN1_COMPARE_RESPONSE = $6F;
  LDAP_ASN1_ABANDON_REQUEST = $70;
  LDAP_ASN1_EXT_REQUEST = $77;
  LDAP_ASN1_EXT_RESPONSE = $78;


type

  TLDAPAttribute = class(TStringList)
  private
    FAttributeName: string;
    FIsBinary: Boolean;
  protected
    function Get(Index: integer): string; override;
    procedure Put(Index: integer; const Value: string); override;
    procedure SetAttributeName(Value: string);
  published
    property AttributeName: string read FAttributeName Write SetAttributeName;
    property IsBinary: Boolean read FIsBinary;
  end;

  TLDAPAttributeList = class(TObject)
  private
    FAttributeList: TList;
    function GetAttribute(Index: integer): TLDAPAttribute;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function Add: TLDAPAttribute;
    property Items[Index: Integer]: TLDAPAttribute read GetAttribute; default;
  end;

  TLDAPResult = class(TObject)
  private
    FObjectName: string;
    FAttributes: TLDAPAttributeList;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property ObjectName: string read FObjectName write FObjectName;
    property Attributes: TLDAPAttributeList read FAttributes;
  end;

  TLDAPResultList = class(TObject)
  private
    FResultList: TList;
    function GetResult(Index: integer): TLDAPResult;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function Add: TLDAPResult;
    property Items[Index: Integer]: TLDAPResult read GetResult; default;
  end;

  TLDAPModifyOp = (
    MO_Add,
    MO_Delete,
    MO_Replace
  );

  TLDAPSearchScope = (
    SS_BaseObject,
    SS_SingleLevel,
    SS_WholeSubtree
  );

  TLDAPSearchAliases = (
    SA_NeverDeref,
    SA_InSearching,
    SA_FindingBaseObj,
    SA_Always
  );

  TLDAPSend = class(TSynaClient)
  private
    {$IFDEF STREAMSEC}
    FSock: TSsTCPBlockSocket;
    FTLSServer: TCustomTLSInternalServer;
    {$ELSE}
    FSock: TTCPBlockSocket;
    {$ENDIF}
    FResultCode: Integer;
    FResultString: string;
    FFullResult: string;
    FUsername: string;
    FPassword: string;
    FAutoTLS: Boolean;
    FFullSSL: Boolean;
    FSeq: integer;
    FResponseCode: integer;
    FResponseDN: string;
    FReferals: TStringList;
    FVersion: integer;
    FSearchScope: TLDAPSearchScope;
    FSearchAliases: TLDAPSearchAliases;
    FSearchSizeLimit: integer;
    FSearchTimeLimit: integer;
    FSearchResult: TLDAPResultList;
    FExtName: string;
    FExtValue: string;
    function Connect: Boolean;
    function BuildPacket(const Value: string): string;
    function ReceiveResponse: string;
    function DecodeResponse(const Value: string): string;
    function LdapSasl(Value: string): string;
    function TranslateFilter(Value: string): string;
    function GetErrorString(Value: integer): string;
  public
    constructor Create;
    destructor Destroy; override;
    function Login: Boolean;
    function Bind: Boolean;
    function BindSasl: Boolean;
    procedure Logout;
    function Modify(obj: string; Op: TLDAPModifyOp; const Value: TLDAPAttribute): Boolean;
    function Add(obj: string; const Value: TLDAPAttributeList): Boolean;
    function Delete(obj: string): Boolean;
    function ModifyDN(obj, newRDN, newSuperior: string; DeleteoldRDN: Boolean): Boolean;
    function Compare(obj, AttributeValue: string): Boolean;
    function Search(obj: string; TypesOnly: Boolean; Filter: string;
      const Attributes: TStrings): Boolean;
    function Extended(const Name, Value: string): Boolean;

    function StartTLS: Boolean;
  published
    property Version: integer read FVersion Write FVersion;
    property ResultCode: Integer read FResultCode;
    property ResultString: string read FResultString;
    property FullResult: string read FFullResult;
    property Username: string read FUsername Write FUsername;
    property Password: string read FPassword Write FPassword;
    property AutoTLS: Boolean read FAutoTLS Write FAutoTLS;
    property FullSSL: Boolean read FFullSSL Write FFullSSL;
    property Seq: integer read FSeq;
    property SearchScope: TLDAPSearchScope read FSearchScope Write FSearchScope;
    property SearchAliases: TLDAPSearchAliases read FSearchAliases Write FSearchAliases;
    property SearchSizeLimit: integer read FSearchSizeLimit Write FSearchSizeLimit;
    property SearchTimeLimit: integer read FSearchTimeLimit Write FSearchTimeLimit;
    property SearchResult: TLDAPResultList read FSearchResult;
    property Referals: TStringList read FReferals;
    property ExtName: string read FExtName;
    property ExtValue: string read FExtValue;
{$IFDEF STREAMSEC}
    property Sock: TSsTCPBlockSocket read FSock;
    property TLSServer: TCustomTLSInternalServer read FTLSServer write FTLSServer;
{$ELSE}
    property Sock: TTCPBlockSocket read FSock;
{$ENDIF}
  end;

function LDAPResultDump(const Value: TLDAPResultList): string;

implementation

{==============================================================================}
function TLDAPAttribute.Get(Index: integer): string;
begin
  Result := inherited Get(Index);
  if FIsbinary then
    Result := DecodeBase64(Result);
end;

procedure TLDAPAttribute.Put(Index: integer; const Value: string);
var
  s: string;
begin
  s := Value;
  if FIsbinary then
    s := EncodeBase64(Value);
  inherited Put(Index, s);
end;

procedure TLDAPAttribute.SetAttributeName(Value: string);
begin
  FAttributeName := Value;
  FIsBinary := Pos(';binary', Lowercase(value)) > 0;
end;

{==============================================================================}
constructor TLDAPAttributeList.Create;
begin
  inherited Create;
  FAttributeList := TList.Create;
end;

destructor TLDAPAttributeList.Destroy;
begin
  Clear;
  FAttributeList.Free;
  inherited Destroy;
end;

procedure TLDAPAttributeList.Clear;
var
  n: integer;
  x: TLDAPAttribute;
begin
  for n := Count - 1 downto 0 do
  begin
    x := GetAttribute(n);
    if Assigned(x) then
      x.Free;
  end;
  FAttributeList.Clear;
end;

function TLDAPAttributeList.Count: integer;
begin
  Result := FAttributeList.Count;
end;

function TLDAPAttributeList.GetAttribute(Index: integer): TLDAPAttribute;
begin
  Result := nil;
  if Index < Count then
    Result := TLDAPAttribute(FAttributeList[Index]);
end;

function TLDAPAttributeList.Add: TLDAPAttribute;
begin
  Result := TLDAPAttribute.Create;
  FAttributeList.Add(Result);
end;

{==============================================================================}
constructor TLDAPResult.Create;
begin
  inherited Create;
  FAttributes := TLDAPAttributeList.Create;
end;

destructor TLDAPResult.Destroy;
begin
  FAttributes.Free;
  inherited Destroy;
end;

{==============================================================================}
constructor TLDAPResultList.Create;
begin
  inherited Create;
  FResultList := TList.Create;
end;

destructor TLDAPResultList.Destroy;
begin
  Clear;
  FResultList.Free;
  inherited Destroy;
end;

procedure TLDAPResultList.Clear;
var
  n: integer;
  x: TLDAPResult;
begin
  for n := Count - 1 downto 0 do
  begin
    x := GetResult(n);
    if Assigned(x) then
      x.Free;
  end;
  FResultList.Clear;
end;

function TLDAPResultList.Count: integer;
begin
  Result := FResultList.Count;
end;

function TLDAPResultList.GetResult(Index: integer): TLDAPResult;
begin
  Result := nil;
  if Index < Count then
    Result := TLDAPResult(FResultList[Index]);
end;

function TLDAPResultList.Add: TLDAPResult;
begin
  Result := TLDAPResult.Create;
  FResultList.Add(Result);
end;

{==============================================================================}
constructor TLDAPSend.Create;
begin
  inherited Create;
  FReferals := TStringList.Create;
  FFullResult := '';
{$IFDEF STREAMSEC}
  FTLSServer := GlobalTLSInternalServer;
  FSock := TSsTCPBlockSocket.Create;
  FSock.BlockingRead := True;
{$ELSE}
  FSock := TTCPBlockSocket.Create;
{$ENDIF}
  FTimeout := 60000;
  FTargetPort := cLDAPProtocol;
  FUsername := '';
  FPassword := '';
  FAutoTLS := False;
  FFullSSL := False;
  FSeq := 0;
  FVersion := 3;
  FSearchScope := SS_WholeSubtree;
  FSearchAliases := SA_Always;
  FSearchSizeLimit := 0;
  FSearchTimeLimit := 0;
  FSearchResult := TLDAPResultList.Create;
end;

destructor TLDAPSend.Destroy;
begin
  FSock.Free;
  FSearchResult.Free;
  FReferals.Free;
  inherited Destroy;
end;

function TLDAPSend.GetErrorString(Value: integer): string;
begin
  case Value of
    0:
      Result := 'Success';
    1:
      Result := 'Operations error';
    2:
      Result := 'Protocol error';
    3:
      Result := 'Time limit Exceeded';
    4:
      Result := 'Size limit Exceeded';
    5:
      Result := 'Compare FALSE';
    6:
      Result := 'Compare TRUE';
    7:
      Result := 'Auth method not supported';
    8:
      Result := 'Strong auth required';
    9:
      Result := '-- reserved --';
    10:
      Result := 'Referal';
    11:
      Result := 'Admin limit exceeded';
    12:
      Result := 'Unavailable critical extension';
    13:
      Result := 'Confidentality required';
    14:
      Result := 'Sasl bind in progress';
    16:
      Result := 'No such attribute';
    17:
      Result := 'Undefined attribute type';
    18:
      Result := 'Inappropriate matching';
    19:
      Result := 'Constraint violation';
    20:
      Result := 'Attribute or value exists';
    21:
      Result := 'Invalid attribute syntax';
    32:
      Result := 'No such object';
    33:
      Result := 'Alias problem';
    34:
      Result := 'Invalid DN syntax';
    36:
      Result := 'Alias dereferencing problem';
    48:
      Result := 'Inappropriate authentication';
    49:
      Result := 'Invalid credentials';
    50:
      Result := 'Insufficient access rights';
    51:
      Result := 'Busy';
    52:
      Result := 'Unavailable';
    53:
      Result := 'Unwilling to perform';
    54:
      Result := 'Loop detect';
    64:
      Result := 'Naming violation';
    65:
      Result := 'Object class violation';
    66:
      Result := 'Not allowed on non leaf';
    67:
      Result := 'Not allowed on RDN';
    68:
      Result := 'Entry already exists';
    69:
      Result := 'Object class mods prohibited';
    71:
      Result := 'Affects multiple DSAs';
    80:
      Result := 'Other';
  else
    Result := '--unknown--';
  end;
end;

function TLDAPSend.Connect: Boolean;
begin
  // Do not call this function! It is calling by LOGIN method!
  FSock.CloseSocket;
  FSock.LineBuffer := '';
  FSeq := 0;
  FSock.Bind(FIPInterface, cAnyPort);
{$IFDEF STREAMSEC}
  if FFullSSL then
  begin
    if Assigned(FTLSServer) then
      FSock.TLSServer := FTLSServer
    else
    begin
      Result := false;
      Exit;
    end;
  end
  else
    FSock.TLSServer := nil;
{$ELSE}
  if FFullSSL then
    FSock.SSLEnabled := True;
{$ENDIF}
  if FSock.LastError = 0 then
    FSock.Connect(FTargetHost, FTargetPort);
  Result := FSock.LastError = 0;
end;

function TLDAPSend.BuildPacket(const Value: string): string;
begin
  Inc(FSeq);
  Result := ASNObject(ASNObject(ASNEncInt(FSeq), ASN1_INT) + Value,  ASN1_SEQ);
end;

function TLDAPSend.ReceiveResponse: string;
var
  x: Byte;
  i,j: integer;
begin
  Result := '';
  FFullResult := '';
  x := FSock.RecvByte(FTimeout);
  if x <> ASN1_SEQ then
    Exit;
  Result := Char(x);
  x := FSock.RecvByte(FTimeout);
  Result := Result + Char(x);
  if x < $80 then
    i := 0
  else
    i := x and $7F;
  if i > 0 then
    Result := Result + FSock.RecvBufferStr(i, Ftimeout);
  if FSock.LastError <> 0 then
  begin
    Result := '';
    Exit;
  end;
  //get length of LDAP packet
  j := 2;
  i := ASNDecLen(j, Result);
  //retreive rest of LDAP packet
  if i > 0 then
    Result := Result + FSock.RecvBufferStr(i, Ftimeout);
  if FSock.LastError <> 0 then
  begin
    Result := '';
    Exit;
  end;
  FFullResult := Result;
end;

function TLDAPSend.DecodeResponse(const Value: string): string;
var
  i, x: integer;
  Svt: Integer;
  s, t: string;
begin
  Result := '';
  FResultCode := -1;
  FResultstring := '';
  FResponseCode := -1;
  FResponseDN := '';
  FReferals.Clear;
  i := 1;
  ASNItem(i, Value, Svt);
  x := StrToIntDef(ASNItem(i, Value, Svt), 0);
  if (svt <> ASN1_INT) or (x <> FSeq) then
    Exit;
  s := ASNItem(i, Value, Svt);
  FResponseCode := svt;
  if FResponseCode in [LDAP_ASN1_BIND_RESPONSE, LDAP_ASN1_SEARCH_DONE,
    LDAP_ASN1_MODIFY_RESPONSE, LDAP_ASN1_ADD_RESPONSE, LDAP_ASN1_DEL_RESPONSE,
    LDAP_ASN1_MODIFYDN_RESPONSE, LDAP_ASN1_COMPARE_RESPONSE,
    LDAP_ASN1_EXT_RESPONSE] then
  begin
    FResultCode := StrToIntDef(ASNItem(i, Value, Svt), -1);
    FResponseDN := ASNItem(i, Value, Svt);
    FResultString := ASNItem(i, Value, Svt);
    if FResultString = '' then
      FResultString := GetErrorString(FResultCode);
    if FResultCode = 10 then
    begin
      s := ASNItem(i, Value, Svt);
      if svt = $A3 then
      begin
        x := 1;
        while x < Length(s) do
        begin
          t := ASNItem(x, s, Svt);
          FReferals.Add(t);
        end;
      end;
    end;
  end;
  Result := Copy(Value, i, Length(Value) - i + 1);
end;

function TLDAPSend.LdapSasl(Value: string): string;
var
  nonce, cnonce, nc, realm, qop, uri, response: string;
  s: string;
  a1, a2: string;
  l: TStringList;
  n: integer;
begin
  l := TStringList.Create;
  try
    nonce := '';
    realm := '';
    l.CommaText := Value;
    n := IndexByBegin('nonce=', l);
    if n >= 0 then
      nonce := UnQuoteStr(SeparateRight(l[n], 'nonce='), '"');
    n := IndexByBegin('realm=', l);
    if n >= 0 then
      realm := UnQuoteStr(SeparateRight(l[n], 'realm='), '"');
    cnonce := IntToHex(GetTick, 8);
    nc := '00000001';
    qop := 'auth';
    uri := 'ldap/' + FSock.ResolveIpToName(FSock.GetRemoteSinIP);
    a1 := md5(FUsername + ':' + realm + ':' + FPassword)
      + ':' + nonce + ':' + cnonce;
    a2 := 'AUTHENTICATE:' + uri;
    s := strtohex(md5(a1))+':' + nonce + ':' + nc + ':' + cnonce + ':'
      + qop +':'+strtohex(md5(a2));
    response := strtohex(md5(s));

    Result := 'username="' + Fusername + '",realm="' + realm + '",nonce="';
    Result := Result + nonce + '",cnonce="' + cnonce + '",nc=' + nc + ',qop=';
    Result := Result + qop + ',digest-uri="' + uri + '",response=' + response;
  finally
    l.Free;
  end;
end;

function TLDAPSend.TranslateFilter(Value: string): string;
var
  x: integer;
  s, t, l, r: string;
  c: char;
  attr, rule: string;
  dn: Boolean;
begin
  Result := '';
  if Value = '' then
    Exit;
  s := Value;
  if Value[1] = '(' then
  begin
    x := RPos(')', Value);
    s := Copy(Value, 2, x - 2);
  end;
  if s = '' then
    Exit;
  case s[1] of
    '!':
      // NOT rule (recursive call)
      begin
        Result := ASNOBject(TranslateFilter(GetBetween('(', ')', s)), $82);
      end;
    '&':
      // AND rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := SeparateRight(s, t);
          if s <> '' then
            if s[1] = ')' then
              System.Delete(s, 1, 1);
          Result := Result + TranslateFilter(t);
        until s = '';
        Result := ASNOBject(Result, $A0);
      end;
    '|':
      // OR rule (recursive call)
      begin
        repeat
          t := GetBetween('(', ')', s);
          s := SeparateRight(s, t);
          if s <> '' then
            if s[1] = ')' then
              System.Delete(s, 1, 1);
          Result := Result + TranslateFilter(t);
        until s = '';
        Result := ASNOBject(Result, $A1);
      end;
    else
      begin
        l := SeparateLeft(s, '=');
        r := SeparateRight(s, '=');
        if l <> '' then
        begin
          c := l[Length(l)];
          case c of
            ':':
              // Extensible match
              begin
                System.Delete(l, Length(l), 1);
                dn := False;
                attr := '';
                rule := '';
                if Pos(':dn', l) > 0 then
                begin
                  dn := True;
                  l := ReplaceString(l, ':dn', '');
                end;
                attr := SeparateLeft(l, ':');
                rule := SeparateRight(l, ':');
                if rule = l then
                  rule := '';
                if rule <> '' then
                  Result := ASNObject(rule, $81);
                if attr <> '' then
                  Result := Result + ASNObject(attr, $82);
                Result := Result + ASNObject(DecodeTriplet(r, '\'), $83);
                if dn then
                  Result := Result + ASNObject(AsnEncInt($ff), $84)
                else
                  Result := Result + ASNObject(AsnEncInt(0), $84);
                Result := ASNOBject(Result, $a9);
              end;
            '~':
              // Approx match
              begin
                System.Delete(l, Length(l), 1);
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                Result := ASNOBject(Result, $a8);
              end;
            '>':
              // Greater or equal match
              begin
                System.Delete(l, Length(l), 1);
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                Result := ASNOBject(Result, $a5);
              end;
            '<':
              // Less or equal match
              begin
                System.Delete(l, Length(l), 1);
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                Result := ASNOBject(Result, $a6);
              end;
          else
            // present
            if r = '*' then
              Result := ASNOBject(l, $87)
            else
              if Pos('*', r) > 0 then
              // substrings
              begin
                s := Fetch(r, '*');
                if s <> '' then
                  Result := ASNOBject(DecodeTriplet(s, '\'), $80);
                while r <> '' do
                begin
                  if Pos('*', r) <= 0 then
                    break;
                  s := Fetch(r, '*');
                  Result := Result + ASNOBject(DecodeTriplet(s, '\'), $81);
                end;
                if r <> '' then
                  Result := Result + ASNOBject(DecodeTriplet(r, '\'), $82);
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(Result, ASN1_SEQ);
                Result := ASNOBject(Result, $a4);
              end
              else
              begin
                // Equality match
                Result := ASNOBject(l, ASN1_OCTSTR)
                  + ASNOBject(DecodeTriplet(r, '\'), ASN1_OCTSTR);
                Result := ASNOBject(Result, $a3);
              end;
          end;
        end;
      end;
  end;
end;

function TLDAPSend.Login: Boolean;
begin
  Result := False;
  if not Connect then
    Exit;
  Result := True;
  if FAutoTLS then
    StartTLS;
end;

function TLDAPSend.Bind: Boolean;
var
  s: string;
begin
  s := ASNObject(ASNEncInt(FVersion), ASN1_INT)
    + ASNObject(FUsername, ASN1_OCTSTR)
    + ASNObject(FPassword, $80);
  s := ASNObject(s, LDAP_ASN1_BIND_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.BindSasl: Boolean;
var
  s, t: string;
  x, xt: integer;
  digreq: string;
begin
  Result := False;
  if FPassword = '' then
    Result := Bind
  else
  begin
    digreq := ASNObject(ASNEncInt(FVersion), ASN1_INT)
      + ASNObject('', ASN1_NULL)
      + ASNObject(ASNObject('DIGEST-MD5', ASN1_OCTSTR), $A3);
    digreq := ASNObject(digreq, LDAP_ASN1_BIND_REQUEST);
    Fsock.SendString(BuildPacket(digreq));
    s := ReceiveResponse;
    t := DecodeResponse(s);
    if FResultCode = 14 then
    begin
      s := t;
      x := 1;
      t := ASNItem(x, s, xt);
      s := ASNObject(ASNEncInt(FVersion), ASN1_INT)
        + ASNObject('', ASN1_NULL)
        + ASNObject(ASNObject('DIGEST-MD5', ASN1_OCTSTR), $A3)
        + ASNObject(LdapSasl(t), ASN1_OCTSTR);
      s := ASNObject(s, LDAP_ASN1_BIND_REQUEST);
      Fsock.SendString(BuildPacket(s));
      s := ReceiveResponse;
      DecodeResponse(s);
      if FResultCode = 14 then
      begin
        Fsock.SendString(BuildPacket(digreq));
        s := ReceiveResponse;
        DecodeResponse(s);
      end;
      Result := FResultCode = 0;
    end;
  end;
end;

procedure TLDAPSend.Logout;
begin
  Fsock.SendString(BuildPacket(ASNObject('', LDAP_ASN1_UNBIND_REQUEST)));
  FSock.CloseSocket;
end;

function TLDAPSend.Modify(obj: string; Op: TLDAPModifyOp; const Value: TLDAPAttribute): Boolean;
var
  s: string;
  n: integer;
begin
  s := '';
  for n := 0 to Value.Count -1 do
    s := s + ASNObject(Value[n], ASN1_OCTSTR);
  s := ASNObject(Value.AttributeName, ASN1_OCTSTR) + ASNObject(s, ASN1_SETOF);
  s := ASNObject(ASNEncInt(Ord(Op)), ASN1_ENUM) + ASNObject(s, ASN1_SEQ);
  s := ASNObject(s, ASN1_SEQ);
  s := ASNObject(obj, ASN1_OCTSTR) + ASNObject(s, ASN1_SEQ);
  s := ASNObject(s, LDAP_ASN1_MODIFY_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.Add(obj: string; const Value: TLDAPAttributeList): Boolean;
var
  s, t: string;
  n, m: integer;
begin
  s := '';
  for n := 0 to Value.Count - 1 do
  begin
    t := '';
    for m := 0 to Value[n].Count - 1 do
      t := t + ASNObject(Value[n][m], ASN1_OCTSTR);
    t := ASNObject(Value[n].AttributeName, ASN1_OCTSTR)
      + ASNObject(t, ASN1_SETOF);
    s := s + ASNObject(t, ASN1_SEQ);
  end;
  s := ASNObject(obj, ASN1_OCTSTR) + ASNObject(s, ASN1_SEQ);
  s := ASNObject(s, LDAP_ASN1_ADD_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.Delete(obj: string): Boolean;
var
  s: string;
begin
  s := ASNObject(obj, LDAP_ASN1_DEL_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.ModifyDN(obj, newRDN, newSuperior: string; DeleteOldRDN: Boolean): Boolean;
var
  s: string;
begin
  s := ASNObject(obj, ASN1_OCTSTR) + ASNObject(newRDN, ASN1_OCTSTR);
  if DeleteOldRDN then
    s := s + ASNObject(ASNEncInt($ff), ASN1_BOOL)
  else
    s := s + ASNObject(ASNEncInt(0), ASN1_BOOL);
  if newSuperior <> '' then
    s := s + ASNObject(newSuperior, $80);
  s := ASNObject(s, LDAP_ASN1_MODIFYDN_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.Compare(obj, AttributeValue: string): Boolean;
var
  s: string;
begin
  s := ASNObject(SeparateLeft(AttributeValue, '='), ASN1_OCTSTR)
    + ASNObject(SeparateRight(AttributeValue, '='), ASN1_OCTSTR);
  s := ASNObject(obj, ASN1_OCTSTR) + ASNObject(s, ASN1_SEQ);
  s := ASNObject(s, LDAP_ASN1_COMPARE_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  DecodeResponse(s);
  Result := FResultCode = 0;
end;

function TLDAPSend.Search(obj: string; TypesOnly: Boolean; Filter: string;
  const Attributes: TStrings): Boolean;
var
  s, t, u: string;
  n, i, x: integer;
  r: TLDAPResult;
  a: TLDAPAttribute;
begin
  FSearchResult.Clear;
  FReferals.Clear;
  s := ASNObject(obj, ASN1_OCTSTR);
  s := s + ASNObject(ASNEncInt(Ord(FSearchScope)), ASN1_ENUM);
  s := s + ASNObject(ASNEncInt(Ord(FSearchAliases)), ASN1_ENUM);
  s := s + ASNObject(ASNEncInt(FSearchSizeLimit), ASN1_INT);
  s := s + ASNObject(ASNEncInt(FSearchTimeLimit), ASN1_INT);
  if TypesOnly then
    s := s + ASNObject(ASNEncInt($ff), ASN1_BOOL)
  else
    s := s + ASNObject(ASNEncInt(0), ASN1_BOOL);
  if Filter = '' then
    Filter := '(objectclass=*)';
  t := TranslateFilter(Filter);
  if t = '' then
    s := s + ASNObject('', ASN1_NULL)
  else
    s := s + t;
  t := '';
  for n := 0 to Attributes.Count - 1 do
    t := t + ASNObject(Attributes[n], ASN1_OCTSTR);
  s := s + ASNObject(t, ASN1_SEQ);
  s := ASNObject(s, LDAP_ASN1_SEARCH_REQUEST);
  Fsock.SendString(BuildPacket(s));
  repeat
    s := ReceiveResponse;
    t := DecodeResponse(s);
    if FResponseCode = LDAP_ASN1_SEARCH_ENTRY then
    begin
      //dekoduj zaznam
      r := FSearchResult.Add;
      n := 1;
      r.ObjectName := ASNItem(n, t, x);
      ASNItem(n, t, x);
      if x = ASN1_SEQ then
      begin
        while n < Length(t) do
        begin
          s := ASNItem(n, t, x);
          if x = ASN1_SEQ then
          begin
            i := n + Length(s);
            a := r.Attributes.Add;
            u := ASNItem(n, t, x);
            a.AttributeName := u;
            ASNItem(n, t, x);
            if x = ASN1_SETOF then
              while n < i do
              begin
                u := ASNItem(n, t, x);
                a.Add(UnquoteStr(u, '"'));
              end;
          end;
        end;
      end;
    end;
    if FResponseCode = LDAP_ASN1_SEARCH_REFERENCE then
    begin
      n := 1;
      while n < Length(t) do
        FReferals.Add(ASNItem(n, t, x));
    end;
  until FResponseCode = LDAP_ASN1_SEARCH_DONE;
  Result := FResultCode = 0;
end;

function TLDAPSend.Extended(const Name, Value: string): Boolean;
var
  s, t: string;
  x, xt: integer;
begin
  s := ASNObject(Name, $80);
  if Value <> '' then
    s := s + ASNObject(Value, $81);
  s := ASNObject(s, LDAP_ASN1_EXT_REQUEST);
  Fsock.SendString(BuildPacket(s));
  s := ReceiveResponse;
  t := DecodeResponse(s);
  Result := FResultCode = 0;
  if Result then
  begin
    x := 1;
    FExtName := ASNItem(x, t, xt);
    FExtValue := ASNItem(x, t, xt);
  end;
end;


function TLDAPSend.StartTLS: Boolean;
begin
  Result := Extended('1.3.6.1.4.1.1466.20037', '');
  if Result then
  begin
{$IFDEF STREAMSEC}
    if Assigned(FTLSServer) then
    begin
      Fsock.TLSServer := FTLSServer;
      Fsock.Connect('','');
      Result := FSock.LastError = 0;
    end
    else
      Result := false;
{$ELSE}
    Fsock.SSLDoConnect;
    Result := FSock.LastError = 0;
{$ENDIF}
  end;
end;

{==============================================================================}
function LDAPResultDump(const Value: TLDAPResultList): string;
var
  n, m, o: integer;
  r: TLDAPResult;
  a: TLDAPAttribute;
begin
  Result := 'Results: ' + IntToStr(Value.Count) + CRLF +CRLF;
  for n := 0 to Value.Count - 1 do
  begin
    Result := Result + 'Result: ' + IntToStr(n) + CRLF;
    r := Value[n];
    Result := Result + '  Object: ' + r.ObjectName + CRLF;
    for m := 0 to r.Attributes.Count - 1 do
    begin
      a := r.Attributes[m];
      Result := Result + '  Attribute: ' + a.AttributeName + CRLF;
      for o := 0 to a.Count - 1 do
        Result := Result + '    ' + a[o] + CRLF;
    end;
  end;
end;

end.
