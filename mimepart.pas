{==============================================================================|
| Project : Delphree - Synapse                                   | 001.001.000 |
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
  MaxMimeType=25;
  MimeType:array [0..MaxMimeType,0..2] of string=
    (
      ('AU','audio','basic'),
      ('AVI','video','x-msvideo'),
      ('BMP','image','BMP'),
      ('DOC','application','MSWord'),
      ('EPS','application','Postscript'),
      ('GIF','image','GIF'),
      ('JPEG','image','JPEG'),
      ('JPG','image','JPEG'),
      ('MID','audio','midi'),
      ('MOV','video','quicktime'),
      ('MPEG','video','MPEG'),
      ('MPG','video','MPEG'),
      ('MP2','audio','mpeg'),
      ('MP3','audio','mpeg'),
      ('PDF','application','PDF'),
      ('PNG','image','PNG'),
      ('PS','application','Postscript'),
      ('QT','video','quicktime'),
      ('RA','audio','x-realaudio'),
      ('RTF','application','RTF'),
      ('SND','audio','basic'),
      ('TIF','image','TIFF'),
      ('TIFF','image','TIFF'),
      ('WAV','audio','x-wav'),
      ('WPD','application','Wordperfect5.1'),
      ('ZIP','application','ZIP')
    );

procedure NormalizePart(value:Tstringlist);
function GenerateBoundary:string;

implementation

procedure NormalizePart(value:Tstringlist);
var
  t:tstringlist;
  s:string;
begin
  t:=tstringlist.create;
  try
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
  n,x,x1,x2:integer;
  t:tstringlist;
  s,su,b:string;
  st,st2:string;
  e:boolean;
  fn:string;
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

    fn:='';
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
            if (secondary=primary) and (pos('/',st2)<1)
              then secondary:='';
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
                  filename:=getparameter(s,'name=');
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
        if pos('CONTENT-DISPOSITION:',su)=1 then
          begin
            fn:=getparameter(s,'filename=');
          end;
      end;

    if (primarycode=MP_BINARY) and (filename='')
      then filename:=fn;

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
            if PrimaryCode=MP_TEXT
              then s:=decodeChar(s,CharsetCode,TargetCharset);
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
begin
  FCharset:=Value;
  CharsetCode:=GetCPfromID(value);
end;

{==============================================================================}
{GenerateBoundary}
function GenerateBoundary:string;
var
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
