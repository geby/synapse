{==============================================================================|
| Project : Delphree - Synapse                                   | 001.004.000 |
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
| Portions created by Lukas Gebauer are Copyright (c)2000,2001.                |
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
    FSubject: string;
    FOrganization: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
  published
    property From: string read FFrom Write FFrom;
    property ToList: TStringList read FToList Write FToList;
    property Subject: string read FSubject Write FSubject;
    property Organization: string read FOrganization Write FOrganization;
  end;

  TMimeMess = class(TObject)
  private
    FPartList: TList;
    FLines: TStringList;
    FHeader: TMessHeader;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function AddPart: Integer;
    procedure AddPartText(Value: TStringList);
    procedure AddPartHTML(Value: TStringList);
    procedure AddPartHTMLBinary(Value, Cid: string);
    procedure AddPartBinary(Value: string);
    procedure EncodeMessage;
    procedure FinalizeHeaders;
    procedure ParseHeaders;
    procedure DecodeMessage;
  published
    property PartList: TList read FPartList Write FPartList;
    property Lines: TStringList read FLines Write FLines;
    property Header: TMessHeader read FHeader Write FHeader;
  end;

implementation

{==============================================================================}

constructor TMessHeader.Create;
begin
  inherited Create;
  FToList := TStringList.Create;
end;

destructor TMessHeader.Destroy;
begin
  FToList.Free;
  inherited Destroy;
end;

{==============================================================================}

procedure TMessHeader.Clear;
begin
  FFrom := '';
  FToList.Clear;
  FSubject := '';
  FOrganization := '';
end;

{==============================================================================}

constructor TMimeMess.Create;
begin
  inherited Create;
  FPartList := TList.Create;
  FLines := TStringList.Create;
  FHeader := TMessHeader.Create;
end;

destructor TMimeMess.Destroy;
begin
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
  Lines.Clear;
  for n := 0 to PartList.Count - 1 do
    TMimePart(PartList[n]).Free;
  PartList.Clear;
  FHeader.Clear;
end;

{==============================================================================}

function TMimeMess.AddPart: Integer;
var
  mp: TMimePart;
begin
  mp := TMimePart.Create;
  Result := PartList.Add(mp);
end;

{==============================================================================}

procedure TMimeMess.AddPartText(Value: TStringList);
var
  x: Integer;
begin
  x := AddPart;
  with TMimePart(PartList[x]) do
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

procedure TMimeMess.AddPartHTML(Value: TStringList);
var
  x: Integer;
begin
  x := AddPart;
  with TMimePart(PartList[x]) do
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
  x: Integer;
  s: string;
begin
  x := AddPart;
  with TMimePart(PartList[x]) do
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
  x: Integer;
  s: string;
begin
  x := AddPart;
  with TMimePart(PartList[x]) do
  begin
    DecodedLines.LoadFromFile(Value);
    s := ExtractFileName(Value);
    MimeTypeFromExt(s);
    Description := 'Included file: ' + s;
    Disposition := 'inline';
    ContentID := cid;
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
begin
  Lines.Clear;
  if PartList.Count = 1 then
    Lines.Assign(TMimePart(PartList[0]).Lines)
  else
  begin
    bound := GenerateBoundary;
    for n := 0 to PartList.Count - 1 do
    begin
      Lines.Add('--' + bound);
      Lines.AddStrings(TMimePart(PartList[n]).Lines);
    end;
    Lines.Add('--' + bound);
    with TMimePart.Create do
    try
      Self.Lines.SaveToStream(DecodedLines);
      Primary := 'Multipart';
      Secondary := 'mixed';
      Description := 'Multipart message';
      Boundary := bound;
      EncodePart;
      Self.Lines.Assign(Lines);
    finally
      Free;
    end;
  end;
end;

{==============================================================================}

procedure TMimeMess.FinalizeHeaders;
var
  n: Integer;
begin
  Lines.Insert(0, 'x-mailer: Synapse - Delphi TCP/IP library by Lukas Gebauer');
  Lines.Insert(0, 'MIME-Version: 1.0 (produced by Synapse)');
  Lines.Insert(0, 'date: ' + Rfc822DateTime(Now));
  if FHeader.Organization <> '' then
    Lines.Insert(0, 'Organization: ' + InlineCode(Header.Organization));
  if Header.Subject <> '' then
    FLines.Insert(0, 'Subject: ' + InlineCode(Header.Subject));
  for n := 0 to FHeader.ToList.Count - 1 do
    Lines.Insert(0, 'To: ' + InlineEmail(FHeader.ToList[n]));
  Lines.Insert(0, 'From: ' + InlineEmail(FHeader.From));
end;

{==============================================================================}

procedure TMimeMess.ParseHeaders;
var
  s: string;
  x: Integer;
  cp: TMimeChar;
begin
  cp := GetCurCP;
  FHeader.Clear;
  x := 0;
  while Lines.Count > x do
  begin
    s := NormalizeHeader(Lines, x);
    if s = '' then
      Break;
    if Pos('FROM:', UpperCase(s)) = 1 then
      FHeader.From := InlineDecode(SeparateRight(s, ':'), cp);
    if Pos('SUBJECT:', UpperCase(s)) = 1 then
      FHeader.Subject := InlineDecode(SeparateRight(s, ':'), cp);
    if Pos('ORGANIZATION:', UpperCase(s)) = 1 then
      FHeader.Organization := InlineDecode(SeparateRight(s, ':'), cp);
    if Pos('TO:', UpperCase(s)) = 1 then
      FHeader.ToList.Add(InlineDecode(SeparateRight(s, ':'), cp));
  end;
end;

{==============================================================================}

procedure TMimeMess.DecodeMessage;
var
  l: TStringList;
  m: TMimePart;
  x, i: Integer;
  bound: string;
begin
  l := TStringList.Create;
  m := TMimePart.Create;
  try
    l.Assign(Lines);
    FHeader.Clear;
    ParseHeaders;
    m.ExtractPart(l, 0);
    if m.PrimaryCode = MP_MULTIPART then
    begin
      bound := m.Boundary;
      i := 0;
      repeat
        x := AddPart;
        with TMimePart(PartList[x]) do
        begin
          Boundary := bound;
          i := ExtractPart(l, i);
          DecodePart;
        end;
      until i >= l.Count - 2;
    end
    else
    begin
      x := AddPart;
      with TMimePart(PartList[x]) do
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
