{==============================================================================|
| Project : Delphree - Synapse                                   | 001.005.002 |
|==============================================================================|
| Content: MIME support procedures and functions                               |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.1 |
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
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

unit MIMEpart;

interface

uses
  SysUtils, Classes,
  SynaChar, SynaCode, SynaUtil, MIMEinLn;

type

  TMimePrimary = (MP_TEXT, MP_MULTIPART,
    MP_MESSAGE, MP_BINARY);

  TMimeEncoding = (ME_7BIT, ME_8BIT, ME_QUOTED_PRINTABLE,
    ME_BASE64, ME_UU, ME_XX);

  TMimePart = class(TObject)
  private
    FPrimary: string;
    FEncoding: string;
    FCharset: string;
    FPrimaryCode: TMimePrimary;
    FEncodingCode: TMimeEncoding;
    FCharsetCode: TMimeChar;
    FTargetCharset: TMimeChar;
    FSecondary: string;
    FDescription: string;
    FDisposition: string;
    FContentID: string;
    FBoundary: string;
    FFileName: string;
    FLines: TStringList;
    FDecodedLines: TMemoryStream;
    procedure SetPrimary(Value: string);
    procedure SetEncoding(Value: string);
    procedure SetCharset(Value: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function ExtractPart(Value: TStringList; BeginLine: Integer): Integer;
    procedure DecodePart;
    procedure EncodePart;
    procedure MimeTypeFromExt(Value: string);
  published
    property Primary: string read FPrimary write SetPrimary;
    property Encoding: string read FEncoding write SetEncoding;
    property Charset: string read FCharset write SetCharset;
    property PrimaryCode: TMimePrimary read FPrimaryCode Write FPrimaryCode;
    property EncodingCode: TMimeEncoding read FEncodingCode Write FEncodingCode;
    property CharsetCode: TMimeChar read FCharsetCode Write FCharsetCode;
    property TargetCharset: TMimeChar read FTargetCharset Write FTargetCharset;
    property Secondary: string read FSecondary Write FSecondary;
    property Description: string read FDescription Write FDescription;
    property Disposition: string read FDisposition Write FDisposition;
    property ContentID: string read FContentID Write FContentID;
    property Boundary: string read FBoundary Write FBoundary;
    property FileName: string read FFileName Write FFileName;
    property Lines: TStringList read FLines;
    property DecodedLines: TMemoryStream read FDecodedLines;
  end;

const
  MaxMimeType = 25;
  MimeType: array[0..MaxMimeType, 0..2] of string =
  (
    ('AU', 'audio', 'basic'),
    ('AVI', 'video', 'x-msvideo'),
    ('BMP', 'image', 'BMP'),
    ('DOC', 'application', 'MSWord'),
    ('EPS', 'application', 'Postscript'),
    ('GIF', 'image', 'GIF'),
    ('JPEG', 'image', 'JPEG'),
    ('JPG', 'image', 'JPEG'),
    ('MID', 'audio', 'midi'),
    ('MOV', 'video', 'quicktime'),
    ('MPEG', 'video', 'MPEG'),
    ('MPG', 'video', 'MPEG'),
    ('MP2', 'audio', 'mpeg'),
    ('MP3', 'audio', 'mpeg'),
    ('PDF', 'application', 'PDF'),
    ('PNG', 'image', 'PNG'),
    ('PS', 'application', 'Postscript'),
    ('QT', 'video', 'quicktime'),
    ('RA', 'audio', 'x-realaudio'),
    ('RTF', 'application', 'RTF'),
    ('SND', 'audio', 'basic'),
    ('TIF', 'image', 'TIFF'),
    ('TIFF', 'image', 'TIFF'),
    ('WAV', 'audio', 'x-wav'),
    ('WPD', 'application', 'Wordperfect5.1'),
    ('ZIP', 'application', 'ZIP')
    );

function NormalizeHeader(Value: TStringList; var Index: Integer): string;
function GenerateBoundary: string;

implementation

function NormalizeHeader(Value: TStringList; var Index: Integer): string;
var
  s, t: string;
  n: Integer;
begin
  s := Value[Index];
  Inc(Index);
  if s <> '' then
    while (Value.Count - 1) > Index do
    begin
      t := Value[Index];
      if t = '' then
        Break;
      for n := 1 to Length(t) do
        if t[n] = #9 then
          t[n] := ' ';
      if t[1] <> ' ' then
        Break
      else
      begin
        s := s + ' ' + Trim(t);
        Inc(Index);
      end;
    end;
  Result := s;
end;

{==============================================================================}

constructor TMIMEPart.Create;
begin
  inherited Create;
  FLines := TStringList.Create;
  FDecodedLines := TMemoryStream.Create;
  FTargetCharset := GetCurCP;
end;

destructor TMIMEPart.Destroy;
begin
  FDecodedLines.Free;
  FLines.Free;
  inherited Destroy;
end;

{==============================================================================}

procedure TMIMEPart.Clear;
begin
  FPrimary := '';
  FEncoding := '';
  FCharset := '';
  FPrimaryCode := MP_TEXT;
  FEncodingCode := ME_7BIT;
  FCharsetCode := ISO_8859_1;
  FTargetCharset := GetCurCP;
  FSecondary := '';
  FDisposition := '';
  FContentID := '';
  FDescription := '';
  FBoundary := '';
  FFileName := '';
  FLines.Clear;
  FDecodedLines.Clear;
end;

{==============================================================================}

function TMIMEPart.ExtractPart(Value: TStringList; BeginLine: Integer): Integer;
var
  n, x, x1, x2: Integer;
  t: TStringList;
  s, su, b: string;
  st, st2: string;
  e: Boolean;
  fn: string;
begin
  t := TStringlist.Create;
  try
    { defaults }
    FLines.Clear;
    Primary := 'text';
    FSecondary := 'plain';
    FDescription := '';
    Charset := 'US-ASCII';
    FFileName := '';
    Encoding := '7BIT';

    fn := '';
    x := BeginLine;
    b := FBoundary;
    { if multipart - skip pre-part }
    if b <> '' then
      while Value.Count > x do
      begin
        s := Value[x];
        Inc(x);
        if Pos('--' + b, s) = 1 then
          Break;
      end;

    { parse header }
    while Value.Count > x do
    begin
      s := NormalizeHeader(Value, x);
      if s = '' then
        Break;
      su := UpperCase(s);
      if Pos('CONTENT-TYPE:', su) = 1 then
      begin
        st := SeparateRight(su, ':');
        st2 := SeparateLeft(st, ';');
        Primary := SeparateLeft(st2, '/');
        FSecondary := SeparateRight(st2, '/');
        if (FSecondary = Primary) and (Pos('/', st2) < 1) then
          FSecondary := '';
        case FPrimaryCode of
          MP_TEXT:
            begin
              Charset := UpperCase(GetParameter(s, 'charset='));
              FFileName := GetParameter(s, 'name=');
            end;
          MP_MULTIPART:
            FBoundary := GetParameter(s, 'Boundary=');
          MP_MESSAGE:
            begin
            end;
          MP_BINARY:
            FFileName := GetParameter(s, 'name=');
        end;
      end;
      if Pos('CONTENT-TRANSFER-ENCODING:', su) = 1 then
        Encoding := SeparateRight(su, ':');
      if Pos('CONTENT-DESCRIPTION:', su) = 1 then
        FDescription := SeparateRight(s, ':');
      if Pos('CONTENT-DISPOSITION:', su) = 1 then
      begin
        FDisposition := SeparateRight(su, ':');
        FDisposition := Trim(SeparateLeft(FDisposition, ';'));
        fn := GetParameter(s, 'FileName=');
      end;
      if Pos('CONTENT-ID:', su) = 1 then
        FContentID := SeparateRight(s, ':');
    end;

    if (PrimaryCode = MP_BINARY) and (FFileName = '') then
      FFileName := fn;
    FFileName := InlineDecode(FFileName, getCurCP);
    FFileName := ExtractFileName(FFileName);

    { finding part content x1-begin x2-end }
    x1 := x;
    x2 := Value.Count - 1;
    { if multipart - end is before next boundary }
    if b <> '' then
    begin
      for n := x to Value.Count - 1 do
      begin
        x2 := n;
        s := Value[n];
        if Pos('--' + b, s) = 1 then
        begin
          Dec(x2);
          Break;
        end;
      end;
    end;
    { if content is multipart - content is delimited by their boundaries }
    if FPrimaryCode = MP_MULTIPART then
    begin
      for n := x to Value.Count - 1 do
      begin
        s := Value[n];
        if Pos('--' + FBoundary, s) = 1 then
        begin
          x1 := n;
          Break;
        end;
      end;
      for n := Value.Count - 1 downto x do
      begin
        s := Value[n];
        if Pos('--' + FBoundary, s) = 1 then
        begin
          x2 := n;
          Break;
        end;
      end;
    end;
    { copy content }
    for n := x1 to x2 do
      FLines.Add(Value[n]);
    Result := x2;
    { if content is multipart - find real end }
    if FPrimaryCode = MP_MULTIPART then
    begin
      e := False;
      for n := x2 + 1 to Value.Count - 1 do
        if Pos('--' + b, Value[n]) = 1 then
        begin
          e := True;
          Break;
        end;
      if not e then
        Result := Value.Count - 1;
    end;
    { if multipart - skip ending postpart}
    if b <> '' then
    begin
      x1 := Result;
      for n := x1 to Value.Count - 1 do
      begin
        s := Value[n];
        if Pos('--' + b, s) = 1 then
        begin
          s := TrimRight(s);
          x := Length(s);
          if x > 4 then
            if (s[x] = '-') and (S[x-1] = '-') then
              Result := Value.Count - 1;
          Break;
        end;
      end;
    end;
  finally
    t.Free;
  end;
end;

{==============================================================================}

procedure TMIMEPart.DecodePart;
const
  CRLF = #13#10;
var
  n: Integer;
  s: string;
begin
  FDecodedLines.Clear;
  for n := 0 to FLines.Count - 1 do
  begin
    s := FLines[n];
    case FEncodingCode of
      ME_7BIT:
        s := s + CRLF;
      ME_8BIT:
        begin
          s := CharsetConversion(s, FCharsetCode, FTargetCharset);
          s := s + CRLF;
        end;
      ME_QUOTED_PRINTABLE:
        begin
          if s = '' then
            s := CRLF
          else
            if s[Length(s)] <> '=' then
              s := s + CRLF;
          s := DecodeQuotedPrintable(s);
          if FPrimaryCode = MP_TEXT then
            s := CharsetConversion(s, FCharsetCode, FTargetCharset);
        end;
      ME_BASE64:
        begin
          if s <> '' then
            s := DecodeBase64(s);
          if FPrimaryCode = MP_TEXT then
            s := CharsetConversion(s, FCharsetCode, FTargetCharset);
        end;
      ME_UU:
        if s <> '' then
          s := DecodeUU(s);
      ME_XX:
        if s <> '' then
          s := DecodeXX(s);
    end;
    FDecodedLines.Write(Pointer(s)^, Length(s));
  end;
  FDecodedLines.Seek(0, soFromBeginning);
end;

{==============================================================================}

procedure TMIMEPart.EncodePart;
var
  l: TStringList;
  s, buff: string;
  n, x: Integer;
begin
  if (FEncodingCode = ME_UU) or (FEncodingCode = ME_XX) then
    Encoding := 'base64';
  l := TStringList.Create;
  FLines.Clear;
  FDecodedLines.Seek(0, soFromBeginning);
  try
    case FPrimaryCode of
      MP_MULTIPART, MP_MESSAGE:
        FLines.LoadFromStream(FDecodedLines);
      MP_TEXT, MP_BINARY:
        if FEncodingCode = ME_BASE64 then
        begin
          while FDecodedLines.Position < FDecodedLines.Size do
          begin
            Setlength(Buff, 54);
            s := '';
            x := FDecodedLines.Read(pointer(Buff)^, 54);
            for n := 1 to x do
              s := s + Buff[n];
            if FPrimaryCode = MP_TEXT then
              s := CharsetConversion(s, FTargetCharset, FCharsetCode);
            s := EncodeBase64(s);
            FLines.Add(s);
          end;
        end
        else
        begin
          l.LoadFromStream(FDecodedLines);
          for n := 0 to l.Count - 1 do
          begin
            s := l[n];
            if FPrimaryCode = MP_TEXT then
              s := CharsetConversion(s, FTargetCharset, FCharsetCode);
            s := EncodeQuotedPrintable(s);
            FLines.Add(s);
          end;
        end;

    end;
    FLines.Add('');
    FLines.Insert(0, '');
    if FSecondary = '' then
      case FPrimaryCode of
        MP_TEXT:
          FSecondary := 'plain';
        MP_MULTIPART:
          FSecondary := 'mixed';
        MP_MESSAGE:
          FSecondary := 'rfc822';
        MP_BINARY:
          FSecondary := 'octet-stream';
      end;
    if FDescription <> '' then
      FLines.Insert(0, 'Content-Description: ' + FDescription);
    if FDisposition <> '' then
    begin
      s := '';
      if FFileName <> '' then
        s := '; FileName="' + FFileName + '"';
      FLines.Insert(0, 'Content-Disposition: ' + LowerCase(FDisposition) + s);
    end;
    if FContentID <> '' then
      FLines.Insert(0, 'Content-ID: ' + FContentID);

    case FEncodingCode of
      ME_7BIT:
        s := '7bit';
      ME_8BIT:
        s := '8bit';
      ME_QUOTED_PRINTABLE:
        s := 'Quoted-printable';
      ME_BASE64:
        s := 'Base64';
    end;
    case FPrimaryCode of
      MP_TEXT,
        MP_BINARY: FLines.Insert(0, 'Content-Transfer-Encoding: ' + s);
    end;
    case FPrimaryCode of
      MP_TEXT:
        s := FPrimary + '/' + FSecondary + '; charset=' + GetIDfromCP(FCharsetCode);
      MP_MULTIPART:
        s := FPrimary + '/' + FSecondary + '; boundary="' + FBoundary + '"';
      MP_MESSAGE:
        s := FPrimary + '/' + FSecondary + '';
      MP_BINARY:
        s := FPrimary + '/' + FSecondary + '; name="' + FFileName + '"';
    end;
    FLines.Insert(0, 'Content-type: ' + s);
  finally
    l.Free;
  end;
end;

{==============================================================================}

procedure TMIMEPart.MimeTypeFromExt(Value: string);
var
  s: string;
  n: Integer;
begin
  Primary := '';
  FSecondary := '';
  s := UpperCase(ExtractFileExt(Value));
  if s = '' then
    s := UpperCase(Value);
  s := SeparateRight(s, '.');
  for n := 0 to MaxMimeType do
    if MimeType[n, 0] = s then
    begin
      Primary := MimeType[n, 1];
      FSecondary := MimeType[n, 2];
      Break;
    end;
  if Primary = '' then
    Primary := 'application';
  if FSecondary = '' then
    FSecondary := 'octet-string';
end;

{==============================================================================}

procedure TMIMEPart.SetPrimary(Value: string);
var
  s: string;
begin
  FPrimary := Value;
  s := UpperCase(Value);
  FPrimaryCode := MP_BINARY;
  if Pos('TEXT', s) = 1 then
    FPrimaryCode := MP_TEXT;
  if Pos('MULTIPART', s) = 1 then
    FPrimaryCode := MP_MULTIPART;
  if Pos('MESSAGE', s) = 1 then
    FPrimaryCode := MP_MESSAGE;
end;

procedure TMIMEPart.SetEncoding(Value: string);
var
  s: string;
begin
  FEncoding := Value;
  s := UpperCase(Value);
  FEncodingCode := ME_7BIT;
  if Pos('8BIT', s) = 1 then
    FEncodingCode := ME_8BIT;
  if Pos('QUOTED-PRINTABLE', s) = 1 then
    FEncodingCode := ME_QUOTED_PRINTABLE;
  if Pos('BASE64', s) = 1 then
    FEncodingCode := ME_BASE64;
  if Pos('X-UU', s) = 1 then
    FEncodingCode := ME_UU;
  if Pos('X-XX', s) = 1 then
    FEncodingCode := ME_XX;
end;

procedure TMIMEPart.SetCharset(Value: string);
begin
  FCharset := Value;
  FCharsetCode := GetCPFromID(Value);
end;

{==============================================================================}

function GenerateBoundary: string;
var
  x: Integer;
begin
  Randomize;
  x := Random(MaxInt);
  Result := '--' + IntToHex(x, 8) + '_Synapse_message_boundary--';
end;

end.
