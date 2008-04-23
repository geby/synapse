{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.000 |
|==============================================================================|
| Content: MIME support procedures and functions                                    |
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
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

unit MIMEpart;

interface

uses
  sysutils, classes, windows, MIMEchar, SynaCode, SynaUtil;

type

TMimePrimary=(MP_TEXT,
              MP_MULTIPART,
              MP_MESSAGE,
              MP_BINARY);

TMimeEncoding=(ME_7BIT,
               ME_8BIT,
               ME_QUOTED_PRINTABLE,
               ME_BASE64);

TMimePart=class
  private
    FPrimary:string;
    FEncoding:string;
    FCharset:string;
    procedure Setprimary(Value:string);
    procedure SetEncoding(Value:string);
    procedure SetCharset(Value:string);
  protected
  public
    PrimaryCode:TMimePrimary;
    EncodingCode:TMimeEncoding;
    CharsetCode:TMimeChar;
    TargetCharset:TMimeChar;
    secondary:string;
    description:string;
    boundary:string;
    FileName:string;
    Lines:TStringList;
    DecodedLines:TmemoryStream;
    constructor Create;
    destructor Destroy; override;
    procedure clear;
    function ExtractPart(value:TStringList; BeginLine:integer):integer;
    procedure DecodePart;
    procedure EncodePart;
    procedure MimeTypeFromExt(value:string);
  property
    Primary:string read FPrimary Write SetPrimary;
  property
    encoding:string read FEncoding write SetEncoding;
  property
    Charset:string read FCharset write SetCharset;
end;

const
  MaxMimeType=15;
  MimeType:array [0..MaxMimeType,0..2] of string=
    (
      ('DOC','application','MSWord'),
      ('GIF','image','GIF'),
      ('JPEG','image','JPEG'),
      ('JPG','image','JPEG'),
      ('MPEG','video','MPEG'),
      ('MPG','video','MPEG'),
      ('PDF','application','PDF'),
      ('PNG','image','PNG'),
      ('PS','application','Postscript'),
      ('MOV','video','quicktime'),
      ('RTF','application','RTF'),
      ('TIF','image','TIFF'),
      ('TIFF','image','TIFF'),
      ('WAV','audio','basic'),
      ('WPD','application','Wordperfect5.1'),
      ('ZIP','application','ZIP')
    );

procedure NormalizePart(value:Tstringlist);
function InlineDecode(value:string;CP:TMimeChar):string;
function InlineEncode(value:string;CP,MimeP:TMimeChar):string;
Function NeedInline(value:string):boolean;
function InlineCode(value:string):string;
function InlineEmail(value:string):string;
function GenerateBoundary:string;

implementation

procedure NormalizePart(value:Tstringlist);
var
  t:tstringlist;
  x:integer;
  s:string;
begin
  t:=tstringlist.create;
  try
    x:=0;
    while (value.Count-1) > 0 do
      begin
        s:=value[0];
        if s=''
          then break;
        if (s[1]=' ') or (s[1]=#9)
          then
            begin
              s:=' '+trim(s);
              if t.count=0
                then t.add(s)
                else t[t.count-1]:=t[t.count-1]+s;
            end
          else
            t.add(s);
        value.Delete(0);
      end;
    t.AddStrings(value);
    value.assign(t);
  finally
    t.free;
  end;
end;

{==============================================================================}
{TMIMEPart.Create}
Constructor TMIMEPart.Create;
begin
  inherited Create;
  Lines:=TStringList.Create;
  DecodedLines:=TmemoryStream.create;
  TargetCharset:=GetCurCP;
end;

{TMIMEPart.Destroy}
Destructor TMIMEPart.Destroy;
begin
  DecodedLines.free;
  Lines.free;
  inherited destroy;
end;

{==============================================================================}
{TMIMEPart.Clear}
procedure TMIMEPart.Clear;
begin
  FPrimary:='';
  FEncoding:='';
  FCharset:='';
  PrimaryCode:=MP_TEXT;
  EncodingCode:=ME_7BIT;
  CharsetCode:=ISO_8859_1;
  TargetCharset:=GetCurCP;
  secondary:='';
  description:='';
  boundary:='';
  FileName:='';
  Lines.clear;
  DecodedLines.clear;
end;

{==============================================================================}
{TMIMEPart.ExtractPart}
function TMIMEPart.ExtractPart(value:TStringList; BeginLine:integer):integer;
var
  n,x,y,x1,x2:integer;
  t:tstringlist;
  s,su,b:string;
  st,st2:string;
  e:boolean;
begin
  t:=tstringlist.create;
  try
    {defaults}
    lines.clear;
    primary:='text';
    secondary:='plain';
    description:='';
    charset:='US-ASCII';
    FileName:='';
    encoding:='7BIT';

    x:=beginline;
    b:=boundary;
    if b<>'' then
      while value.count>x do
        begin
          s:=value[x];
          inc(x);
          if pos('--'+b,s)>0
            then break;
        end;

    {parse header}
    while value.count>x do
      begin
        s:=value[x];
        inc(x);
        if s=''
          then break;
        su:=uppercase(s);
        if pos('CONTENT-TYPE:',su)=1 then
          begin
            st:=separateright(su,':');
            st2:=separateleft(st,';');
            primary:=separateleft(st2,'/');
            secondary:=separateright(st2,'/');
            case primarycode of
              MP_TEXT:
                begin
                  charset:=uppercase(getparameter(s,'charset='));
                end;
              MP_MULTIPART:
                begin
                  boundary:=getparameter(s,'boundary=');
                end;
              MP_MESSAGE:
                begin
                end;
              MP_BINARY:
                begin
                  filename:=getparameter(s,'filename=');
                end;
            end;
          end;
        if pos('CONTENT-TRANSFER-ENCODING:',su)=1 then
          begin
            encoding:=separateright(su,':');
          end;
        if pos('CONTENT-DESCRIPTION:',su)=1 then
          begin
            description:=separateright(s,':');
          end;
      end;

    x1:=x;
    x2:=value.count-1;
    if b<>'' then
      begin
        for n:=x to value.count-1 do
          begin
            x2:=n;
            s:=value[n];
            if pos('--'+b,s)>0
              then begin
                dec(x2);
                break;
              end;
          end;
      end;
    if primarycode=MP_MULTIPART then
      begin
        for n:=x to value.count-1 do
          begin
            s:=value[n];
            if pos('--'+boundary,s)>0 then
              begin
                x1:=n;
                break;
              end;
          end;
        for n:=value.count-1 downto x do
          begin
            s:=value[n];
            if pos('--'+boundary,s)>0 then
              begin
                x2:=n;
                break;
              end;
          end;
      end;
    for n:=x1 to x2 do
      lines.add(value[n]);
    result:=x2;
    if primarycode=MP_MULTIPART then
      begin
        e:=false;
        for n:=x2+1 to value.count-1 do
          if pos('--'+boundary,value[n])>0 then
            begin
              e:=true;
              break;
            end;
        if not e
          then result:=value.count-1;
      end;
  finally
    t.free;
  end;
end;

{==============================================================================}
{TMIMEPart.DecodePart}
procedure TMIMEPart.DecodePart;
const
  CRLF=#$0D+#$0A;
var
  n:integer;
  s:string;
begin
  decodedLines.Clear;
  for n:=0 to lines.count-1 do
    begin
      s:=lines[n];
      case EncodingCode of
        ME_7BIT:
          begin
            s:=s+CRLF;
          end;
        ME_8BIT:
          begin
            s:=decodeChar(s,CharsetCode,TargetCharset);
            s:=s+CRLF;
          end;
        ME_QUOTED_PRINTABLE:
          begin
            if s=''
              then s:=CRLF
              else
                if s[length(s)]<>'='
                  then s:=s+CRLF;
            s:=DecodeQuotedPrintable(s);
            s:=decodeChar(s,CharsetCode,TargetCharset);
          end;
        ME_BASE64:
          begin
            if s<>''
              then s:=DecodeBase64(s);
            if PrimaryCode=MP_TEXT
              then s:=decodeChar(s,CharsetCode,TargetCharset);
          end;
      end;
      Decodedlines.Write(pointer(s)^,length(s));
    end;
  decodedlines.Seek(0,soFromBeginning);
end;

{==============================================================================}
{TMIMEPart.EncodePart}
procedure TMIMEPart.EncodePart;
var
  l:TStringList;
  s,buff:string;
  n,x:integer;
begin
  l:=tstringlist.create;
  Lines.clear;
  decodedlines.Seek(0,soFromBeginning);
  try
    case primarycode of
      MP_MULTIPART,
      MP_MESSAGE:
        begin
          lines.LoadFromStream(DecodedLines);
        end;
      MP_TEXT,
      MP_BINARY:
        if EncodingCode=ME_BASE64
          then
            begin
              while decodedlines.Position<decodedlines.Size do
                begin
                  Setlength(Buff,54);
                  s:='';
                  x:=Decodedlines.Read(pointer(Buff)^,54);
                  for n:=1 to x do
                    s:=s+Buff[n];
                  if PrimaryCode=MP_TEXT
                    then s:=decodeChar(s,TargetCharset,CharsetCode);
                  s:=EncodeBase64(s);
                  if x<>54
                    then s:=s+'=';
                  Lines.add(s);
                end;
            end
          else
            begin
              l.LoadFromStream(DecodedLines);
              for n:=0 to l.count-1 do
                begin
                  s:=l[n];
                  if PrimaryCode=MP_TEXT
                    then s:=decodeChar(s,TargetCharset,CharsetCode);
                  s:=EncodeQuotedPrintable(s);
                  Lines.add(s);
                end;
            end;

    end;
    Lines.add('');
    lines.insert(0,'');
    if secondary='' then
      case PrimaryCode of
        MP_TEXT:       secondary:='plain';
        MP_MULTIPART:  secondary:='mixed';
        MP_MESSAGE:    secondary:='rfc822';
        MP_BINARY:     secondary:='octet-stream';
      end;
    if description<>''
      then lines.insert(0,'Content-Description: '+Description);
    case EncodingCode of
      ME_7BIT:              s:='7bit';
      ME_8BIT:              s:='8bit';
      ME_QUOTED_PRINTABLE:  s:='Quoted-printable';
      ME_BASE64:            s:='Base64';
    end;
    case PrimaryCode of
      MP_TEXT,
      MP_BINARY:     lines.insert(0,'Content-Transfer-Encoding: '+s);
    end;
    case PrimaryCode of
      MP_TEXT:       s:=primary+'/'+secondary+'; charset='+GetIDfromCP(charsetcode);
      MP_MULTIPART:  s:=primary+'/'+secondary+'; boundary="'+boundary+'"';
      MP_MESSAGE:    s:=primary+'/'+secondary+'';
      MP_BINARY:     s:=primary+'/'+secondary+'; name="'+FileName+'"';
    end;
    lines.insert(0,'Content-type: '+s);
  finally
    l.free;
  end;
end;

{==============================================================================}
{TMIMEPart.MimeTypeFromExt}
procedure TMIMEPart.MimeTypeFromExt(value:string);
var
  s:string;
  n:integer;
begin
  primary:='';
  secondary:='';
  s:=uppercase(separateright(value,'.'));
  if s=''
    then s:=uppercase(value);
  for n:=0 to MaxMimeType do
    if MimeType[n,0]=s then
      begin
        primary:=MimeType[n,1];
        secondary:=MimeType[n,2];
        break;
      end;
  if primary=''
    then primary:='application';
  if secondary=''
    then secondary:='mixed';
end;

{==============================================================================}
{TMIMEPart.Setprimary}
procedure TMIMEPart.Setprimary(Value:string);
var
  s:string;
begin
  Fprimary:=Value;
  s:=uppercase(Value);
  PrimaryCode:=MP_BINARY;
  if Pos('TEXT',s)=1
    then PrimaryCode:=MP_TEXT;
  if Pos('MULTIPART',s)=1
    then PrimaryCode:=MP_MULTIPART;
  if Pos('MESSAGE',s)=1
    then PrimaryCode:=MP_MESSAGE;
end;

{TMIMEPart.SetEncoding}
procedure TMIMEPart.SetEncoding(Value:string);
var
  s:string;
begin
  FEncoding:=Value;
  s:=uppercase(Value);
  EncodingCode:=ME_7BIT;
  if Pos('8BIT',s)=1
    then EncodingCode:=ME_8BIT;
  if Pos('QUOTED-PRINTABLE',s)=1
    then EncodingCode:=ME_QUOTED_PRINTABLE;
  if Pos('BASE64',s)=1
    then EncodingCode:=ME_BASE64;
end;

{TMIMEPart.SetCharset}
procedure TMIMEPart.SetCharset(Value:string);
var
  s:string;
begin
  FCharset:=Value;
  CharsetCode:=GetCPfromID(value);
end;

{==============================================================================}
{InlineDecode}
function InlineDecode(value:string;CP:TMimeChar):string;
var
  s,su:string;
  x,y,z,n:integer;
  ichar:TMimeChar;
  c:char;
begin
  result:=value;
  x:=pos('=?',uppercase(value));
  y:=pos('?=',value);
  if y>x then
    begin
      s:=copy(value,x,y-x+2);
      su:=copy(s,3,length(s)-4);
      ichar:=GetCPfromID(su);
      z:=pos('?',su);
      if (length(su)>=(z+2)) and (su[z+2]='?') then
        begin
          c:=uppercase(su)[z+1];
          su:=copy(su,z+3,length(su)-z-2);
          if c='B' then
            begin
              s:=DecodeBase64(su);
              s:=DecodeChar(s,ichar,CP);
            end;
          if c='Q' then
            begin
              s:='';
              for n:=1 to length(su) do
                if su[n]='_'
                  then s:=s+' '
                  else s:=s+su[n];
              s:=DecodeQuotedprintable(s);
              s:=DecodeChar(s,ichar,CP);
            end;
        end;
      result:=copy(value,1,x-1)+s+copy(value,y+2,length(value)-y-1);
      repeat
        s:=InlineDecode(result,CP);
        if s=result
          then break;
        result:=s;
      until false;
    end;
end;

{==============================================================================}
{InlineEncode}
function InlineEncode(value:string;CP,MimeP:TMimeChar):string;
var
  s,s1:string;
  n:integer;
begin
  s:=DecodeChar(value,CP,MimeP);
  s:=EncodeQuotedPrintable(s);
  s1:='';
  for n:=1 to length(s) do
    if s[n]=' '
      then s1:=s1+'=20'
      else s1:=s1+s[n];
  result:='=?'+GetIdFromCP(MimeP)+'?Q?'+s1+'?=';
end;

{==============================================================================}
{NeedInline}
Function NeedInline(value:string):boolean;
var
  n:integer;
begin
  result:=false;
  for n:=1 to length(value) do
    if value[n] in (SpecialChar+[char(1)..char(31),char(128)..char(255)]) then
      begin
        result:=true;
        break;
      end;
end;

{==============================================================================}
{InlineCode}
function InlineCode(value:string):string;
var
  c:TMimeChar;
begin
  if NeedInline(value)
    then
      begin
        c:=IdealCoding(value,GetCurCP,
          [ISO_8859_1, ISO_8859_2, ISO_8859_3, ISO_8859_4, ISO_8859_5,
          ISO_8859_6, ISO_8859_7, ISO_8859_8, ISO_8859_9, ISO_8859_10]);
        result:=InlineEncode(value,GetCurCP,c);
      end
    else result:=value;
end;

{==============================================================================}
{InlineEmail}
function InlineEmail(value:string):string;
var
  sd,se:string;
begin
  sd:=getEmaildesc(value);
  se:=getEmailAddr(value);
  if sd=''
    then result:=se
    else result:='"'+InlineCode(sd)+'"<'+se+'>';
end;

{==============================================================================}
{GenerateBoundary}
function GenerateBoundary:string;
var
  s:string;
  x:integer;
begin
  randomize;
  x:=random(maxint);
  result:='----'+Inttohex(x,8)+'_Synapse_message_boundary';
end;

{==============================================================================}

begin
  exit;
  asm
    db 'Synapse MIME messages encoding and decoding library by Lukas Gebauer',0
  end;
end.
