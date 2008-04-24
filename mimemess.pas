{==============================================================================|
| Project : Delphree - Synapse                                   | 002.001.003 |
|==============================================================================|
| Content: MIME message object                                                 |
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
| Portions created by Lukas Gebauer are Copyright (c)2000-2003.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM From distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

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
    procedure EncodeHeaders(const Value: TStrings);
    procedure DecodeHeaders(const Value: TStrings);
    function FindHeader(Value: string): string;
    procedure FindHeaderList(Value: string; const HeaderList: TStrings);
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
    FMessagePart: TMimePart;
    FLines: TStringList;
    FHeader: TMessHeader;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddPart(const PartParent: TMimePart): TMimePart;
    function AddPartMultipart(const MultipartType: String; const PartParent: TMimePart): TMimePart;
    function AddPartText(const Value: TStrings; const PartParent: TMimePart): TMimepart;
    function AddPartHTML(const Value: TStrings; const PartParent: TMimePart): TMimepart;
    function AddPartTextFromFile(const FileName: String; const PartParent: TMimePart): TMimepart;
    function AddPartHTMLFromFile(const FileName: String; const PartParent: TMimePart): TMimepart;
    function AddPartBinary(const Stream: TStream; const FileName: string; const PartParent: TMimePart): TMimepart;
    function AddPartBinaryFromFile(const FileName: string; const PartParent: TMimePart): TMimepart;
    function AddPartHTMLBinary(const Stream: TStream; const FileName, Cid: string; const PartParent: TMimePart): TMimepart;
    function AddPartHTMLBinaryFromFile(const FileName, Cid: string; const PartParent: TMimePart): TMimepart;
    procedure EncodeMessage;
    procedure DecodeMessage;
  published
    property MessagePart: TMimePart read FMessagePart;
    property Lines: TStringList read FLines;
    property Header: TMessHeader read FHeader;
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

procedure TMessHeader.EncodeHeaders(const Value: TStrings);
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
    Value.Insert(0, 'X-mailer: Synapse - Delphi & Kylix TCP/IP library by Lukas Gebauer')
  else
    Value.Insert(0, 'X-mailer: ' + FXMailer);
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

procedure TMessHeader.DecodeHeaders(const Value: TStrings);
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

procedure TMessHeader.FindHeaderList(Value: string; const HeaderList: TStrings);
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
  FMessagePart := TMimePart.Create;
  FLines := TStringList.Create;
  FHeader := TMessHeader.Create;
end;

destructor TMimeMess.Destroy;
begin
  FMessagePart.Free;
  FHeader.Free;
  FLines.Free;
  inherited Destroy;
end;

{==============================================================================}

procedure TMimeMess.Clear;
begin
  FMessagePart.Clear;
  FLines.Clear;
  FHeader.Clear;
end;

{==============================================================================}

function TMimeMess.AddPart(const PartParent: TMimePart): TMimePart;
begin
  if PartParent = nil then
    Result := FMessagePart
  else
    Result := PartParent.AddSubPart;
  Result.Clear;
end;

{==============================================================================}

function TMimeMess.AddPartMultipart(const MultipartType: String; const PartParent: TMimePart): TMimePart;
begin
  Result := AddPart(PartParent);
  with Result do
  begin
    Primary := 'Multipart';
    Secondary := MultipartType;
    Description := 'Multipart message';
    Boundary := GenerateBoundary;
    EncodePartHeader;
  end;
end;

function TMimeMess.AddPartText(const Value: TStrings; const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  with Result do
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
    EncodePartHeader;
  end;
end;

function TMimeMess.AddPartHTML(const Value: TStrings; const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  with Result do
  begin
    Value.SaveToStream(DecodedLines);
    Primary := 'text';
    Secondary := 'html';
    Description := 'HTML text';
    Disposition := 'inline';
    CharsetCode := UTF_8;
    EncodingCode := ME_QUOTED_PRINTABLE;
    EncodePart;
    EncodePartHeader;
  end;
end;

function TMimeMess.AddPartTextFromFile(const FileName: String; const PartParent: TMimePart): TMimepart;
var
  tmp: TStrings;
begin
  tmp := TStringList.Create;
  try
    tmp.LoadFromFile(FileName);
    Result := AddPartText(tmp, PartParent);
  Finally
    tmp.Free;
  end;
end;

function TMimeMess.AddPartHTMLFromFile(const FileName: String; const PartParent: TMimePart): TMimepart;
var
  tmp: TStrings;
begin
  tmp := TStringList.Create;
  try
    tmp.LoadFromFile(FileName);
    Result := AddPartHTML(tmp, PartParent);
  Finally
    tmp.Free;
  end;
end;

function TMimeMess.AddPartBinary(const Stream: TStream; const FileName: string; const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  Result.DecodedLines.LoadFromStream(Stream);
  Result.MimeTypeFromExt(FileName);
  Result.Description := 'Attached file: ' + FileName;
  Result.Disposition := 'attachment';
  Result.FileName := FileName;
  Result.EncodingCode := ME_BASE64;
  Result.EncodePart;
  Result.EncodePartHeader;
end;

function TMimeMess.AddPartBinaryFromFile(const FileName: string; const PartParent: TMimePart): TMimepart;
var
  tmp: TMemoryStream;
begin
  tmp := TMemoryStream.Create;
  try
    tmp.LoadFromFile(FileName);
    Result := AddPartBinary(tmp, ExtractFileName(FileName), PartParent);
  finally
    tmp.Free;
  end;
end;

function TMimeMess.AddPartHTMLBinary(const Stream: TStream; const FileName, Cid: string; const PartParent: TMimePart): TMimepart;
begin
  Result := AddPart(PartParent);
  Result.DecodedLines.LoadFromStream(Stream);
  Result.MimeTypeFromExt(FileName);
  Result.Description := 'Included file: ' + FileName;
  Result.Disposition := 'inline';
  Result.ContentID := Cid;
  Result.FileName := FileName;
  Result.EncodingCode := ME_BASE64;
  Result.EncodePart;
  Result.EncodePartHeader;
end;

function TMimeMess.AddPartHTMLBinaryFromFile(const FileName, Cid: string; const PartParent: TMimePart): TMimepart;
var
  tmp: TMemoryStream;
begin
  tmp := TMemoryStream.Create;
  try
    tmp.LoadFromFile(FileName);
    Result :=AddPartHTMLBinary(tmp, ExtractFileName(FileName), Cid, PartParent);
  finally
    tmp.Free;
  end;
end;

{==============================================================================}

procedure TMimeMess.EncodeMessage;
var
  l: TStringList;
  x: integer;
begin
  //merge headers from THeaders and header field from MessagePart
  l := TStringList.Create;
  try
    FHeader.EncodeHeaders(l);
    x := IndexByBegin('CONTENT-TYPE', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    x := IndexByBegin('CONTENT-DESCRIPTION', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    x := IndexByBegin('CONTENT-DISPOSITION', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    x := IndexByBegin('CONTENT-ID', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    x := IndexByBegin('CONTENT-TRANSFER-ENCODING', FMessagePart.Headers);
    if x >= 0 then
      l.add(FMessagePart.Headers[x]);
    FMessagePart.Headers.Assign(l);
  finally
    l.Free;
  end;
  FMessagePart.ComposeParts;
  FLines.Assign(FMessagePart.Lines);
end;

{==============================================================================}

procedure TMimeMess.DecodeMessage;
begin
  FHeader.Clear;
  FHeader.DecodeHeaders(FLines);
  FMessagePart.Lines.Assign(FLines);
  FMessagePart.DecomposeParts;
end;

end.
