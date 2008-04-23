{==============================================================================|
| Project : Delphree - Synapse                                   | 001.000.001 |
|==============================================================================|
| Content: MIME message object                                                 |
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


unit MIMEmess;

interface

uses
  classes, Sysutils, MimePart, MimeChar, SynaUtil, MIMEInLn;

type

TMessHeader=record
  from:string;
  ToList:tstringlist;
  subject:string;
  organization:string;
end;

TMimeMess=class(TObject)
  private
  public
    PartList:TList;
    Lines:TStringList;
    header:TMessHeader;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddPart:integer;
    procedure AddPartText(value:tstringList);
    procedure AddPartBinary(value:string);
    procedure EncodeMessage;
    procedure FinalizeHeaders;
    procedure ParseHeaders;
    procedure DecodeMessage;
end;

implementation

{==============================================================================}
{TMimeMess.Create}
Constructor TMimeMess.Create;
begin
  inherited Create;
  PartList:=TList.create;
  Lines:=TStringList.create;
  Header.ToList:=TStringList.create;
end;

{TMimeMess.Destroy}
Destructor TMimeMess.Destroy;
begin
  Header.ToList.free;
  Lines.free;
  PartList.free;
  inherited destroy;
end;

{==============================================================================}
{TMimeMess.Clear}
procedure TMimeMess.Clear;
var
  n:integer;
begin
  Lines.clear;
  for n:=0 to PartList.count-1 do
    TMimePart(PartList[n]).Free;
  PartList.Clear;
  with header do
    begin
      from:='';
      ToList.clear;
      subject:='';
      organization:='';
    end;
end;

{==============================================================================}
{TMimeMess.AddPart}
function TMimeMess.AddPart:integer;
var
  mp:TMimePart;
begin
  mp:=TMimePart.create;
  result:=PartList.Add(mp);
end;

{==============================================================================}
{TMimeMess.AddPartText}
procedure TMimeMess.AddPartText(value:tstringList);
var
  x:integer;
begin
  x:=Addpart;
  with TMimePart(PartList[x]) do
    begin
      value.SaveToStream(decodedlines);
      primary:='text';
      secondary:='plain';
      description:='message text';
      CharsetCode:=IdealCoding(value.text,targetCharset,
        [ISO_8859_1, ISO_8859_2, ISO_8859_3, ISO_8859_4, ISO_8859_5,
         ISO_8859_6, ISO_8859_7, ISO_8859_8, ISO_8859_9, ISO_8859_10]);
      EncodingCode:=ME_QUOTED_PRINTABLE;
      EncodePart;
    end;
end;

{==============================================================================}
{TMimeMess.AddPartBinary}
procedure TMimeMess.AddPartBinary(value:string);
var
  x:integer;
begin
  x:=Addpart;
  with TMimePart(PartList[x]) do
    begin
      DecodedLines.LoadFromFile(Value);
      MimeTypeFromExt(value);
      description:='attached file';
      filename:=extractFilename(value);
      EncodingCode:=ME_BASE64;
      EncodePart;
    end;
end;

{==============================================================================}
{TMimeMess.Encodemessage}
procedure TMimeMess.Encodemessage;
var
  bound:string;
  n:integer;
  m:TMimepart;
begin
  lines.clear;
  If PartList.Count=1
    then
      Lines.assign(TMimePart(PartList[0]).lines)
    else
      begin
        bound:=generateboundary;
        for n:=0 to PartList.count-1 do
          begin
            Lines.add('--'+bound);
            lines.AddStrings(TMimePart(PartList[n]).lines);
          end;
        Lines.add('--'+bound);
        m:=TMimePart.Create;
        try
          Lines.SaveToStream(m.DecodedLines);
          m.Primary:='Multipart';
          m.secondary:='mixed';
          m.description:='Multipart message';
          m.boundary:=bound;
          m.EncodePart;
          Lines.assign(m.lines);
        finally
          m.free;
        end;
      end;
end;

{==============================================================================}
{TMimeMess.FinalizeHeaders}
procedure TMimeMess.FinalizeHeaders;
var
  n:integer;
begin
  Lines.Insert(0,'x-mailer: Synapse - Delphi TCP/IP library by Lukas Gebauer');
  Lines.Insert(0,'MIME-Version: 1.0 (produced by Synapse)');
  Lines.Insert(0,'date: '+Rfc822DateTime(now));
  if header.organization<>''
    then Lines.Insert(0,'Organization: '+InlineCode(header.organization));
  if header.subject<>''
    then Lines.Insert(0,'Subject: '+InlineCode(header.subject));
  for n:=0 to Header.ToList.count-1 do
    Lines.Insert(0,'To: '+InlineEmail(header.ToList[n]));
  Lines.Insert(0,'From: '+InlineEmail(header.from));
end;

{==============================================================================}
{TMimeMess.ParseHeaders}
procedure TMimeMess.ParseHeaders;
var
  s:string;
  n:integer;
  cp:TMimeChar;
begin
  cp:=getCurCP;
  header.ToList.clear;
  for n:=0 to lines.count-1 do
    begin
      s:=lines[n];
      if s=''
        then break;
      If pos('FROM:',uppercase(s))=1
        then header.from:=InlineDecode(separateright(s,':'),cp);
      If pos('SUBJECT:',uppercase(s))=1
        then header.subject:=InlineDecode(separateright(s,':'),cp);
      If pos('ORGANIZATION:',uppercase(s))=1
        then header.organization:=InlineDecode(separateright(s,':'),cp);
      If pos('TO:',uppercase(s))=1
        then header.ToList.add(InlineDecode(separateright(s,':'),cp));
    end;
end;

{==============================================================================}
{TMimeMess.DecodeMessage}
procedure TMimeMess.DecodeMessage;
var
  l:tstringlist;
  m:tmimepart;
  x,i:integer;
  bound:string;
begin
  l:=tstringlist.create;
  m:=tmimepart.create;
  try
    l.assign(lines);
    normalizepart(l);
    with header do
      begin
        from:='';
        ToList.clear;
        subject:='';
        organization:='';
      end;
    ParseHeaders;
    m.ExtractPart(l,0);
    if m.primarycode=MP_MULTIPART
      then
        begin
          bound:=m.boundary;
          i:=0;
          repeat
            x:=AddPart;
            with TMimePart(PartList[x]) do
              begin
                boundary:=bound;
                i:=ExtractPart(l,i);
                DecodePart;
              end;
          until i>=l.count-2;
        end
      else
        begin
          x:=AddPart;
          with TMimePart(PartList[x]) do
            begin
              ExtractPart(l,0);
              DecodePart;
            end;
        end;
  finally
    m.free;
    l.free;
  end;
end;

{==============================================================================}

end.
