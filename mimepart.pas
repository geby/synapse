{==============================================================================|
| Project : Ararat Synapse                                       | 002.004.008 |
|==============================================================================|
| Content: MIME support procedures and functions                               |
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
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit mimepart;

interface

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  {$IFDEF FPC}
  synafpc,
  {$ENDIF}
{$ELSE}
  Windows,
{$ENDIF}
  synachar, synacode, synautil, mimeinln;

type

  TMimePart = class;
  THookWalkPart = procedure(const Sender: TMimePart) of object;

  TMimePrimary = (MP_TEXT, MP_MULTIPART,
    MP_MESSAGE, MP_BINARY);

  TMimeEncoding = (ME_7BIT, ME_8BIT, ME_QUOTED_PRINTABLE,
    ME_BASE64, ME_UU, ME_XX);

  TMimePart = class(TObject)
  private
    FPrimary: string;
    FPrimaryCode: TMimePrimary;
    FSecondary: string;
    FEncoding: string;
    FEncodingCode: TMimeEncoding;
    FDefaultCharset: string;
    FCharset: string;
    FCharsetCode: TMimeChar;
    FTargetCharset: TMimeChar;
    FDescription: string;
    FDisposition: string;
    FContentID: string;
    FBoundary: string;
    FFileName: string;
    FLines: TStringList;
    FPartBody: TStringList;
    FHeaders: TStringList;
    FPrePart: TStringList;
    FPostPart: TStringList;
    FDecodedLines: TMemoryStream;
    FSubParts: TList;
    FOnWalkPart: THookWalkPart;
    FMaxLineLength: integer;
    FSubLevel: integer;
    FMaxSubLevel: integer;
    FAttachInside: boolean;
    procedure SetPrimary(Value: string);
    procedure SetEncoding(Value: string);
    procedure SetCharset(Value: string);
    function IsUUcode(Value: string): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Value: TMimePart);
    procedure AssignSubParts(Value: TMimePart);
    procedure Clear;
    procedure DecodePart;
    procedure DecodePartHeader;
    procedure EncodePart;
    procedure EncodePartHeader;
    procedure MimeTypeFromExt(Value: string);
    function GetSubPartCount: integer;
    function GetSubPart(index: integer): TMimePart;
    procedure DeleteSubPart(index: integer);
    procedure ClearSubParts;
    function AddSubPart: TMimePart;
    procedure DecomposeParts;
    procedure ComposeParts;
    procedure WalkPart;
    function CanSubPart: boolean;
  published
    property Primary: string read FPrimary write SetPrimary;
    property Encoding: string read FEncoding write SetEncoding;
    property Charset: string read FCharset write SetCharset;
    property DefaultCharset: string read FDefaultCharset write FDefaultCharset;
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
    property PartBody: TStringList read FPartBody;
    property Headers: TStringList read FHeaders;
    property PrePart: TStringList read FPrePart;
    property PostPart: TStringList read FPostPart;
    property DecodedLines: TMemoryStream read FDecodedLines;
    property SubLevel: integer read FSubLevel write FSubLevel;
    property MaxSubLevel: integer read FMaxSubLevel write FMaxSubLevel;
    property AttachInside: boolean read FAttachInside;
    property OnWalkPart: THookWalkPart read FOnWalkPart write FOnWalkPart;
    property MaxLineLength: integer read FMaxLineLength Write FMaxLineLength;
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

function NormalizeHeader(Value: TStrings; var Index: Integer): string;
function GenerateBoundary: string;

implementation

function NormalizeHeader(Value: TStrings; var Index: Integer): string;
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
  Result := TrimRight(s);
end;

{==============================================================================}

constructor TMIMEPart.Create;
begin
  inherited Create;
  FOnWalkPart := nil;
  FLines := TStringList.Create;
  FPartBody := TStringList.Create;
  FHeaders := TStringList.Create;
  FPrePart := TStringList.Create;
  FPostPart := TStringList.Create;
  FDecodedLines := TMemoryStream.Create;
  FSubParts := TList.Create;
  FTargetCharset := GetCurCP;
  FDefaultCharset := 'US-ASCII';
  FMaxLineLength := 78;
  FSubLevel := 0;
  FMaxSubLevel := -1;
  FAttachInside := false;
end;

destructor TMIMEPart.Destroy;
begin
  ClearSubParts;
  FSubParts.Free;
  FDecodedLines.Free;
  FPartBody.Free;
  FLines.Free;
  FHeaders.Free;
  FPrePart.Free;
  FPostPart.Free;
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
  FAttachInside := False;
  FPartBody.Clear;
  FHeaders.Clear;
  FPrePart.Clear;
  FPostPart.Clear;
  FDecodedLines.Clear;
  ClearSubParts;
end;

{==============================================================================}

procedure TMIMEPart.Assign(Value: TMimePart);
begin
  Primary := Value.Primary;
  Encoding := Value.Encoding;
  Charset := Value.Charset;
  DefaultCharset := Value.DefaultCharset;
  PrimaryCode := Value.PrimaryCode;
  EncodingCode := Value.EncodingCode;
  CharsetCode := Value.CharsetCode;
  TargetCharset := Value.TargetCharset;
  Secondary := Value.Secondary;
  Description := Value.Description;
  Disposition := Value.Disposition;
  ContentID := Value.ContentID;
  Boundary := Value.Boundary;
  FileName := Value.FileName;
  Lines.Assign(Value.Lines);
  PartBody.Assign(Value.PartBody);
  Headers.Assign(Value.Headers);
  PrePart.Assign(Value.PrePart);
  PostPart.Assign(Value.PostPart);
  MaxLineLength := Value.MaxLineLength;
  FAttachInside := Value.AttachInside;
end;

{==============================================================================}

procedure TMIMEPart.AssignSubParts(Value: TMimePart);
var
  n: integer;
  p: TMimePart;
begin
  Assign(Value);
  for n := 0 to Value.GetSubPartCount - 1 do
  begin
    p := AddSubPart;
    p.AssignSubParts(Value.GetSubPart(n));
  end;
end;

{==============================================================================}

function TMIMEPart.GetSubPartCount: integer;
begin
  Result :=  FSubParts.Count;
end;

{==============================================================================}

function TMIMEPart.GetSubPart(index: integer): TMimePart;
begin
  Result := nil;
  if Index < GetSubPartCount then
    Result := TMimePart(FSubParts[Index]);
end;

{==============================================================================}

procedure TMIMEPart.DeleteSubPart(index: integer);
begin
  if Index < GetSubPartCount then
  begin
    GetSubPart(Index).Free;
    FSubParts.Delete(Index);
  end;
end;

{==============================================================================}

procedure TMIMEPart.ClearSubParts;
var
  n: integer;
begin
  for n := 0 to GetSubPartCount - 1 do
    TMimePart(FSubParts[n]).Free;
  FSubParts.Clear;
end;

{==============================================================================}

function TMIMEPart.AddSubPart: TMimePart;
begin
  Result := TMimePart.Create;
  Result.DefaultCharset := FDefaultCharset;
  FSubParts.Add(Result);
  Result.SubLevel := FSubLevel + 1;
end;

{==============================================================================}

procedure TMIMEPart.DecomposeParts;
var
  x: integer;
  s: string;
  Mime: TMimePart;

  procedure SkipEmpty;
  begin
    while FLines.Count > x do
    begin
      s := TrimRight(FLines[x]);
      if s <> '' then
        Break;
      Inc(x);
    end;
  end;

begin
  x := 0;
  Clear;
  //extract headers
  while FLines.Count > x do
  begin
    s := NormalizeHeader(FLines, x);
    if s = '' then
      Break;
    FHeaders.Add(s);
  end;
  DecodePartHeader;
  //extract prepart
  if FPrimaryCode = MP_MULTIPART then
  begin
    SkipEmpty;
    while FLines.Count > x do
    begin
      s := TrimRight(FLines[x]);
      Inc(x);
      if s = '--' + FBoundary then
        Break;
      FPrePart.Add(s);
      if not FAttachInside then
        FAttachInside := IsUUcode(s);
    end;
  end;
  //extract body part
  if FPrimaryCode = MP_MULTIPART then
  begin
    repeat
      if CanSubPart then
      begin
        Mime := AddSubPart;
        while FLines.Count > x do
        begin
          s := FLines[x];
          Inc(x);
          if Pos('--' + FBoundary, s) = 1 then
            Break;
          Mime.Lines.Add(s);
        end;
        StringsTrim(Mime.Lines);
        Mime.DecomposeParts;
      end
      else
      begin
        s := FLines[x];
        Inc(x);
        FPartBody.Add(s);
      end;
      if x >= FLines.Count then
        break;
    until s = '--' + FBoundary + '--';
  end;
  if (FPrimaryCode = MP_MESSAGE) and CanSubPart then
  begin
    Mime := AddSubPart;
    SkipEmpty;
    while FLines.Count > x do
    begin
      s := TrimRight(FLines[x]);
      Inc(x);
      Mime.Lines.Add(s);
    end;
    StringsTrim(Mime.Lines);
    Mime.DecomposeParts;
  end
  else
  begin
    SkipEmpty;
    while FLines.Count > x do
    begin
      s := TrimRight(FLines[x]);
      Inc(x);
      FPartBody.Add(s);
      if not FAttachInside then
        FAttachInside := IsUUcode(s);
    end;
    StringsTrim(FPartBody);
  end;
  //extract postpart
  if FPrimaryCode = MP_MULTIPART then
  begin
    SkipEmpty;
    while FLines.Count > x do
    begin
      s := TrimRight(FLines[x]);
      Inc(x);
      FPostPart.Add(s);
      if not FAttachInside then
        FAttachInside := IsUUcode(s);
    end;
    StringsTrim(FPostPart);
  end;
end;

{==============================================================================}

procedure TMIMEPart.ComposeParts;
var
  n: integer;
  mime: TMimePart;
  s, t: string;
  d1, d2, d3: integer;
  x: integer;
begin
  FLines.Clear;
  //add headers
  for n := 0 to FHeaders.Count -1 do
  begin
    s := FHeaders[n];
    repeat
      if Length(s) < FMaxLineLength then
      begin
        t := s;
        s := '';
      end
      else
      begin
        d1 := RPosEx('; ', s, FMaxLineLength);
        d2 := RPosEx(' ', s, FMaxLineLength);
        d3 := RPosEx(', ', s, FMaxLineLength);
        if (d1 <= 1) and (d2 <= 1) and (d3 <= 1) then
        begin
          x := Pos(' ', Copy(s, 2, Length(s) - 1));
          if x < 1 then
            x := Length(s)
          else
            inc(x);
        end
        else
          if d1 > 0 then
            x := d1
          else
            if d3 > 0 then
              x := d3
            else
              x := d2 - 1;
        t := Copy(s, 1, x);
        Delete(s, 1, x);
      end;
      Flines.Add(t);
    until s = '';
  end;

  Flines.Add('');
  //add body
  //if multipart
  if FPrimaryCode = MP_MULTIPART then
  begin
    Flines.AddStrings(FPrePart);
    for n := 0 to GetSubPartCount - 1 do
    begin
      Flines.Add('--' + FBoundary);
      mime := GetSubPart(n);
      mime.ComposeParts;
      FLines.AddStrings(mime.Lines);
    end;
    Flines.Add('--' + FBoundary + '--');
    Flines.AddStrings(FPostPart);
  end;
  //if message
  if FPrimaryCode = MP_MESSAGE then
  begin
    if GetSubPartCount > 0 then
    begin
      mime := GetSubPart(0);
      mime.ComposeParts;
      FLines.AddStrings(mime.Lines);
    end;
  end
  else
  //if normal part
  begin
    FLines.AddStrings(FPartBody);
  end;
end;

{==============================================================================}

procedure TMIMEPart.DecodePart;
var
  n: Integer;
  s: string;
begin
  FDecodedLines.Clear;
  case FEncodingCode of
    ME_QUOTED_PRINTABLE:
      s := DecodeQuotedPrintable(FPartBody.Text);
    ME_BASE64:
      s := DecodeBase64(FPartBody.Text);
    ME_UU, ME_XX:
      begin
        s := '';
        for n := 0 to FPartBody.Count - 1 do
          if FEncodingCode = ME_UU then
            s := s + DecodeUU(FPartBody[n])
          else
            s := s + DecodeXX(FPartBody[n]);
      end;
  else
    s := FPartBody.Text;
  end;
  if FPrimaryCode = MP_TEXT then
    s := CharsetConversion(s, FCharsetCode, FTargetCharset);
  FDecodedLines.Write(Pointer(s)^, Length(s));
  FDecodedLines.Seek(0, soFromBeginning);
end;

{==============================================================================}

procedure TMIMEPart.DecodePartHeader;
var
  n: integer;
  s, su, fn: string;
  st, st2: string;
begin
  Primary := 'text';
  FSecondary := 'plain';
  FDescription := '';
  Charset := FDefaultCharset;
  FFileName := '';
  Encoding := '7BIT';
  FDisposition := '';
  FContentID := '';
  fn := '';
  for n := 0 to FHeaders.Count - 1 do
    if FHeaders[n] <> '' then
    begin
      s := FHeaders[n];
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
  if FFileName = '' then
    FFileName := fn;
  FFileName := InlineDecode(FFileName, FTargetCharset);
  FFileName := ExtractFileName(FFileName);
end;

{==============================================================================}

procedure TMIMEPart.EncodePart;
var
  l: TStringList;
  s, t: string;
  n, x: Integer;
  d1, d2: integer;
begin
  if (FEncodingCode = ME_UU) or (FEncodingCode = ME_XX) then
    Encoding := 'base64';
  l := TStringList.Create;
  FPartBody.Clear;
  FDecodedLines.Seek(0, soFromBeginning);
  try
    case FPrimaryCode of
      MP_MULTIPART, MP_MESSAGE:
        FPartBody.LoadFromStream(FDecodedLines);
      MP_TEXT, MP_BINARY:
        if FEncodingCode = ME_BASE64 then
        begin
          while FDecodedLines.Position < FDecodedLines.Size do
          begin
            Setlength(s, 54);
            x := FDecodedLines.Read(pointer(s)^, 54);
            Setlength(s, x);
            if FPrimaryCode = MP_TEXT then
              s := CharsetConversion(s, FTargetCharset, FCharsetCode);
            s := EncodeBase64(s);
            FPartBody.Add(s);
          end;
        end
        else
        begin
          if FPrimaryCode = MP_BINARY then
          begin
            SetLength(s, FDecodedLines.Size);
            x := FDecodedLines.Read(pointer(s)^, FDecodedLines.Size);
            Setlength(s, x);
            l.Add(s);
          end
          else
            l.LoadFromStream(FDecodedLines);
          for n := 0 to l.Count - 1 do
          begin
            s := l[n];
            if (FPrimaryCode = MP_TEXT) and (FEncodingCode <> ME_7BIT) then
              s := CharsetConversion(s, FTargetCharset, FCharsetCode);
            if FEncodingCode = ME_QUOTED_PRINTABLE then
            begin
              if FPrimaryCode = MP_BINARY then
                s := EncodeQuotedPrintable(s)
              else
                s := EncodeTriplet(s, '=', [Char(0)..Char(31), '=', Char(127)..Char(255)]);
              repeat
                if Length(s) < FMaxLineLength then
                begin
                  t := s;
                  s := '';
                end
                else
                begin
                  d1 := RPosEx('=', s, FMaxLineLength);
                  d2 := RPosEx(' ', s, FMaxLineLength);
                  if (d1 = 0) and (d2 = 0) then
                    x := FMaxLineLength
                  else
                    if d1 > d2 then
                      x := d1 - 1
                    else
                      x := d2 - 1;
                  if x = 0 then
                    x := FMaxLineLength;
                  t := Copy(s, 1, x);
                  Delete(s, 1, x);
                  if s <> '' then
                    t := t + '=';
                end;
                FPartBody.Add(t);
              until s = '';
            end
            else
              FPartBody.Add(s);
          end;
          if (FPrimaryCode = MP_BINARY)
            and (FEncodingCode = ME_QUOTED_PRINTABLE) then
            FPartBody[FPartBody.Count - 1] := FPartBody[FPartBody.Count - 1] + '=';
        end;
    end;
  finally
    l.Free;
  end;
end;

{==============================================================================}

procedure TMIMEPart.EncodePartHeader;
var
  s: string;
begin
  FHeaders.Clear;
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
    FHeaders.Insert(0, 'Content-Description: ' + FDescription);
  if FDisposition <> '' then
  begin
    s := '';
    if FFileName <> '' then
      s := '; FileName="' + InlineCodeEx(FileName, FTargetCharset) + '"';
    FHeaders.Insert(0, 'Content-Disposition: ' + LowerCase(FDisposition) + s);
  end;
  if FContentID <> '' then
    FHeaders.Insert(0, 'Content-ID: ' + FContentID);

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
      MP_BINARY: FHeaders.Insert(0, 'Content-Transfer-Encoding: ' + s);
  end;
  case FPrimaryCode of
    MP_TEXT:
      s := FPrimary + '/' + FSecondary + '; charset=' + GetIDfromCP(FCharsetCode);
    MP_MULTIPART:
      s := FPrimary + '/' + FSecondary + '; boundary="' + FBoundary + '"';
    MP_MESSAGE, MP_BINARY:
      s := FPrimary + '/' + FSecondary;
  end;
  if FFileName <> '' then
    s := s + '; name="' + InlineCodeEx(FileName, FTargetCharset) + '"';
  FHeaders.Insert(0, 'Content-type: ' + s);
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
    FSecondary := 'octet-stream';
end;

{==============================================================================}

procedure TMIMEPart.WalkPart;
var
  n: integer;
  m: TMimepart;
begin
  if assigned(OnWalkPart) then
  begin
    OnWalkPart(self);
    for n := 0 to GetSubPartCount - 1 do
    begin
      m := GetSubPart(n);
      m.OnWalkPart := OnWalkPart;
      m.WalkPart;
    end;
  end;
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

function TMIMEPart.CanSubPart: boolean;
begin
  Result := True;
  if FMaxSubLevel <> -1 then
    Result := FMaxSubLevel > FSubLevel;
end;

function TMIMEPart.IsUUcode(Value: string): boolean;
begin
  Value := UpperCase(Value);
  Result := (pos('BEGIN ', Value) = 1) and (SeparateRight(Value, ' ') <> '');
end;

{==============================================================================}

function GenerateBoundary: string;
var
  x, y: Integer;
begin
  y := GetTick;
  x := y;
  while TickDelta(y, x) = 0 do
  begin
    Sleep(1);
    x := GetTick;
  end;
  Randomize;
  y := Random(MaxInt);
  Result := IntToHex(x, 8) + '_' + IntToHex(y, 8) + '_Synapse_boundary';
end;

end.
