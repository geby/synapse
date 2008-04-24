{==============================================================================|
| Project : Delphree - Synapse                                   | 001.007.004 |
|==============================================================================|
| Content: MIME message object                                                 |
|==============================================================================|
| The contents of this file are Subject to the Mozilla Public License Ver. 1.1 |
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
| Portions created by Lukas Gebauer are Copyright (c)2000-2002.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM From distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$WEAKPACKAGEUNIT ON}

unit MIMEmess;

interface

uses
  Classes, SysUtils,
  MIMEpart, SynaChar, SynaUtil, MIMEinLn;

type
  TMessHeader = class(TObject)
  private
    FFrom: string;
    FToList: TStringList;
    FCCList: TStringList;
    FSubject: string;
    FOrganization: string;
    FCustomHeaders: TStringList;
    FDate: TDateTime;
    FXMailer: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure EncodeHeaders(const Value: TStringList);
    procedure DecodeHeaders(const Value: TStringList);
    function FindHeader(Value: string): string;
    procedure FindHeaderList(Value: string; const HeaderList: TStringList);
  published
    property From: string read FFrom Write FFrom;
    property ToList: TStringList read FToList;
    property CCList: TStringList read FCCList;
    property Subject: string read FSubject Write FSubject;
    property Organization: string read FOrganization Write FOrganization;
    property CustomHeaders: TStringList read FCustomHeaders;
    property Date: TDateTime read FDate Write FDate;
    property XMailer: string read FXMailer Write FXMailer;
  end;

  TMimeMess = class(TObject)
  private
    FPartList: TList;
    FLines: TStringList;
    FHeader: TMessHeader;
    FMultipartType: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddPart: Integer;
    procedure AddPartText(const Value: TStringList);
    procedure AddPartHTML(const Value: TStringList);
    procedure AddPartHTMLBinary(Value, Cid: string);
    procedure AddPartBinary(Value: string);
    procedure EncodeMessage;
    procedure FinalizeHeaders;
    procedure ParseHeaders;
    procedure DecodeMessage;
  published
    property PartList: TList read FPartList;
    property Lines: TStringList read FLines;
    property Header: TMessHeader read FHeader;
    property MultipartType: string read FMultipartType Write FMultipartType;
  end;

implementation

{==============================================================================}

constructor TMessHeader.Create;
begin
  inherited Create;
  FToList := TStringList.Create;
  FCCList := TStringList.Create;
  FCustomHeaders := TStringList.Create;
end;

destructor TMessHeader.Destroy;
begin
  FCustomHeaders.Free;
  FCCList.Free;
  FToList.Free;
  inherited Destroy;
end;

{==============================================================================}

procedure TMessHeader.Clear;
begin
  FFrom := '';
  FToList.Clear;
  FCCList.Clear;
  FSubject := '';
  FOrganization := '';
  FCustomHeaders.Clear;
  FDate := 0;
  FXMailer := '';
end;

procedure TMessHeader.EncodeHeaders(const Value: TStringList);
var
  n: Integer;
  s: string;
begin
  if FDate = 0 then
    FDate := Now;
  for n := FCustomHeaders.Count - 1 downto 0 do
    if FCustomHeaders[n] <> '' then
      Value.Insert(0, FCustomHeaders[n]);
  if FXMailer = '' then
    Value.Insert(0, 'x-mailer: Synapse - Delphi & Kylix TCP/IP library by Lukas Gebauer')
  else
    Value.Insert(0, 'x-mailer: ' + FXMailer);
  Value.Insert(0, 'MIME-Version: 1.0 (produced by Synapse)');
  if FOrganization <> '' then
    Value.Insert(0, 'Organization: ' + InlineCode(FOrganization));
  s := '';
  for n := 0 to FCCList.Count - 1 do
    if s = '' then
      s := InlineEmail(FCCList[n])
    else
      s := s + ' , ' + InlineEmail(FCCList[n]);
  if s <> '' then
    Value.Insert(0, 'CC: ' + s);
  Value.Insert(0, 'Date: ' + Rfc822DateTime(FDate));
  if FSubject <> '' then
    Value.Insert(0, 'Subject: ' + InlineCode(FSubject));
  s := '';
  for n := 0 to FToList.Count - 1 do
    if s = '' then
      s := InlineEmail(FToList[n])
    else
      s := s + ' , ' + InlineEmail(FToList[n]);
  if s <> '' then
    Value.Insert(0, 'To: ' + s);
  Value.Insert(0, 'From: ' + InlineEmail(FFrom));
end;

procedure TMessHeader.DecodeHeaders(const Value: TStringList);
var
  s, t: string;
  x: Integer;
  cp: TMimeChar;
begin
  cp := GetCurCP;
  Clear;
  x := 0;
  while Value.Count > x do
  begin
    s := NormalizeHeader(Value, x);
    if s = '' then
      Break;
    if Pos('X-MAILER:', UpperCase(s)) = 1 then
    begin
      FXMailer := SeparateRight(s, ':');
      continue;
    end;
    if Pos('FROM:', UpperCase(s)) = 1 then
    begin
      FFrom := InlineDecode(SeparateRight(s, ':'), cp);
      continue;
    end;
    if Pos('SUBJECT:', UpperCase(s)) = 1 then
    begin
      FSubject := InlineDecode(SeparateRight(s, ':'), cp);
      continue;
    end;
    if Pos('ORGANIZATION:', UpperCase(s)) = 1 then
    begin
      FOrganization := InlineDecode(SeparateRight(s, ':'), cp);
      continue;
    end;
    if Pos('TO:', UpperCase(s)) = 1 then
    begin
      s := SeparateRight(s, ':');
      repeat
        t := InlineDecode(fetch(s, ','), cp);
        if t <> '' then
          FToList.Add(t);
      until s = '';
      continue;
    end;
    if Pos('CC:', UpperCase(s)) = 1 then
    begin
      s := SeparateRight(s, ':');
      repeat
        t := InlineDecode(fetch(s, ','), cp);
        if t <> '' then
          FCCList.Add(t);
      until s = '';
      continue;
    end;
    if Pos('DATE:', UpperCase(s)) = 1 then
    begin
      FDate := DecodeRfcDateTime(SeparateRight(s, ':'));
      continue;
    end;
    if Pos('MIME-VERSION:', UpperCase(s)) = 1 then
      continue;
    if Pos('CONTENT-TYPE:', UpperCase(s)) = 1 then
      continue;
    if Pos('CONTENT-DESCRIPTION:', UpperCase(s)) = 1 then
      continue;
    if Pos('CONTENT-DISPOSITION:', UpperCase(s)) = 1 then
      continue;
    if Pos('CONTENT-ID:', UpperCase(s)) = 1 then
      continue;
    if Pos('CONTENT-TRANSFER-ENCODING:', UpperCase(s)) = 1 then
      continue;
    FCustomHeaders.Add(s);
  end;
end;

function TMessHeader.FindHeader(Value: string): string;
var
  n: integer;
begin
  Result := '';
  for n := 0 to FCustomHeaders.Count - 1 do
    if Pos(UpperCase(Value), UpperCase(FCustomHeaders[n])) = 1 then
    begin
      Result := SeparateRight(FCustomHeaders[n], ':');
      break;
    end;
end;

procedure TMessHeader.FindHeaderList(Value: string; const HeaderList: TStringList);
var
  n: integer;
begin
  HeaderList.Clear;
  for n := 0 to FCustomHeaders.Count - 1 do
    if Pos(UpperCase(Value), UpperCase(FCustomHeaders[n])) = 1 then
    begin
      HeaderList.Add(SeparateRight(FCustomHeaders[n], ':'));
    end;
end;

{==============================================================================}

constructor TMimeMess.Create;
begin
  inherited Create;
  FPartList := TList.Create;
  FLines := TStringList.Create;
  FHeader := TMessHeader.Create;
  FMultipartType := 'Mixed';
end;

destructor TMimeMess.Destroy;
begin
  Clear;
  FHeader.Free;
  Lines.Free;
  PartList.Free;
  inherited Destroy;
end;

{==============================================================================}

procedure TMimeMess.Clear;
var
  n: Integer;
begin
  FMultipartType := 'Mixed';
  Lines.Clear;
  for n := 0 to FPartList.Count - 1 do
    TMimePart(FPartList[n]).Free;
  FPartList.Clear;
  FHeader.Clear;
end;

{==============================================================================}

function TMimeMess.AddPart: Integer;
begin
  Result := FPartList.Add(TMimePart.Create);
end;

{==============================================================================}

procedure TMimeMess.AddPartText(const Value: TStringList);
begin
  with TMimePart(FPartList[AddPart]) do
  begin
    Value.SaveToStream(DecodedLines);
    Primary := 'text';
    Secondary := 'plain';
    Description := 'Message text';
    Disposition := 'inline';
    CharsetCode := IdealCharsetCoding(Value.Text, TargetCharset,
      [ISO_8859_1, ISO_8859_2, ISO_8859_3, ISO_8859_4, ISO_8859_5,
      ISO_8859_6, ISO_8859_7, ISO_8859_8, ISO_8859_9, ISO_8859_10]);
    EncodingCode := ME_QUOTED_PRINTABLE;
    EncodePart;
  end;
end;

{==============================================================================}

procedure TMimeMess.AddPartHTML(const Value: TStringList);
begin
  with TMimePart(FPartList[AddPart]) do
  begin
    Value.SaveToStream(DecodedLines);
    Primary := 'text';
    Secondary := 'html';
    Description := 'HTML text';
    Disposition := 'inline';
    CharsetCode := UTF_8;
    EncodingCode := ME_QUOTED_PRINTABLE;
    EncodePart;
  end;
end;

{==============================================================================}

procedure TMimeMess.AddPartBinary(Value: string);
var
  s: string;
begin
  with TMimePart(FPartList[AddPart]) do
  begin
    DecodedLines.LoadFromFile(Value);
    s := ExtractFileName(Value);
    MimeTypeFromExt(s);
    Description := 'Attached file: ' + s;
    Disposition := 'attachment';
    FileName := s;
    EncodingCode := ME_BASE64;
    EncodePart;
  end;
end;

procedure TMimeMess.AddPartHTMLBinary(Value, Cid: string);
var
  s: string;
begin
  with TMimePart(FPartList[AddPart]) do
  begin
    DecodedLines.LoadFromFile(Value);
    s := ExtractFileName(Value);
    MimeTypeFromExt(s);
    Description := 'Included file: ' + s;
    Disposition := 'inline';
    ContentID := Cid;
    FileName := s;
    EncodingCode := ME_BASE64;
    EncodePart;
  end;
end;

{==============================================================================}

procedure TMimeMess.EncodeMessage;
var
  bound: string;
  n: Integer;
  m:TMimepart;
begin
  FLines.Clear;
  if FPartList.Count = 1 then
  begin
    TMimePart(FPartList[0]).EncodePart;
    FLines.Assign(TMimePart(FPartList[0]).Lines)
  end
  else
  begin
    bound := GenerateBoundary;
    for n := 0 to FPartList.Count - 1 do
    begin
      FLines.Add('--' + bound);
      TMimePart(FPartList[n]).EncodePart;
      FLines.AddStrings(TMimePart(FPartList[n]).Lines);
    end;
    FLines.Add('--' + bound + '--');
    m := TMimePart.Create;
    try
      FLines.SaveToStream(m.DecodedLines);
      m.Primary := 'Multipart';
      m.Secondary := FMultipartType;
      m.Description := 'Multipart message';
      m.Boundary := bound;
      m.EncodePart;
      FLines.Assign(m.Lines);
    finally
      m.Free;
    end;
  end;
end;

{==============================================================================}

procedure TMimeMess.FinalizeHeaders;
begin
  FHeader.EncodeHeaders(FLines);
end;

{==============================================================================}

procedure TMimeMess.ParseHeaders;
begin
  FHeader.DecodeHeaders(FLines);
end;

{==============================================================================}

procedure TMimeMess.DecodeMessage;
var
  l: TStringList;
  m: TMimePart;
  i: Integer;
  bound: string;
begin
  l := TStringList.Create;
  m := TMimePart.Create;
  try
    l.Assign(FLines);
    FHeader.Clear;
    ParseHeaders;
    m.ExtractPart(l, 0);
    if m.PrimaryCode = MP_MULTIPART then
    begin
      bound := m.Boundary;
      i := 0;
      repeat
        with TMimePart(PartList[AddPart]) do
        begin
          Boundary := bound;
          i := ExtractPart(l, i);
          DecodePart;
        end;
      until i >= l.Count - 2;
    end
    else
    begin
      with TMimePart(PartList[AddPart]) do
      begin
        ExtractPart(l, 0);
        DecodePart;
      end;
    end;
  finally
    m.Free;
    l.Free;
  end;
end;

end.
