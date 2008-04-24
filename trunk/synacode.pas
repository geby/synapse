{==============================================================================|
| Project : Delphree - Synapse                                   | 001.005.005 |
|==============================================================================|
| Content: Coding and decoding support                                         |
|==============================================================================|
| The contents of this file are subject to the Mozilla Public License Ver. 1.1 |
| (the "License"); you may not use this file except in compliance with the     |
| License. You may obtain a Copy of the License at http://www.mozilla.org/MPL/ |
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
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$Q-}
{$WEAKPACKAGEUNIT ON}

unit SynaCode;

interface

uses
  SysUtils;

type
  TSpecials = set of Char;

const

  SpecialChar: TSpecials =
  ['=', '(', ')', '[', ']', '<', '>', ':', ';', '.', ',', '@', '/', '?', '\',
    '"', '_'];
  URLFullSpecialChar: TSpecials =
  [';', '/', '?', ':', '@', '=', '&', '#'];
  URLSpecialChar: TSpecials =
  [#$00..#$20, '_', '<', '>', '"', '%', '{', '}', '|', '\', '^', '~', '[', ']',
    '`', #$7F..#$FF];
  TableBase64 =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
  TableUU =
    '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  TableXX =
    '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_';
  ReTablebase64 =
    #$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$3E +#$40
    +#$40 +#$40 +#$3F +#$34 +#$35 +#$36 +#$37 +#$38 +#$39 +#$3A +#$3B +#$3C
    +#$3D +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$00 +#$01 +#$02 +#$03
    +#$04 +#$05 +#$06 +#$07 +#$08 +#$09 +#$0A +#$0B +#$0C +#$0D +#$0E +#$0F
    +#$10 +#$11 +#$12 +#$13 +#$14 +#$15 +#$16 +#$17 +#$18 +#$19 +#$40 +#$40
    +#$40 +#$40 +#$40 +#$40 +#$1A +#$1B +#$1C +#$1D +#$1E +#$1F +#$20 +#$21
    +#$22 +#$23 +#$24 +#$25 +#$26 +#$27 +#$28 +#$29 +#$2A +#$2B +#$2C +#$2D
    +#$2E +#$2F +#$30 +#$31 +#$32 +#$33 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40;
  ReTableUU =
    #$01 +#$02 +#$03 +#$04 +#$05 +#$06 +#$07 +#$08 +#$09 +#$0A +#$0B +#$0C
    +#$0D +#$0E +#$0F +#$10 +#$11 +#$12 +#$13 +#$14 +#$15 +#$16 +#$17 +#$18
    +#$19 +#$1A +#$1B +#$1C +#$1D +#$1E +#$1F +#$20 +#$21 +#$22 +#$23 +#$24
    +#$25 +#$26 +#$27 +#$28 +#$29 +#$2A +#$2B +#$2C +#$2D +#$2E +#$2F +#$30
    +#$31 +#$32 +#$33 +#$34 +#$35 +#$36 +#$37 +#$38 +#$39 +#$3A +#$3B +#$3C
    +#$3D +#$3E +#$3F +#$00 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40
    +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40
    +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40;
  ReTableXX =
    #$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$00 +#$40
    +#$01 +#$40 +#$40 +#$02 +#$03 +#$04 +#$05 +#$06 +#$07 +#$08 +#$09 +#$0A
    +#$0B +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$40 +#$0C +#$0D +#$0E +#$0F
    +#$10 +#$11 +#$12 +#$13 +#$14 +#$15 +#$16 +#$17 +#$18 +#$19 +#$1A +#$1B
    +#$1C +#$1D +#$1E +#$1F +#$20 +#$21 +#$22 +#$23 +#$24 +#$25 +#$40 +#$40
    +#$40 +#$40 +#$40 +#$40 +#$26 +#$27 +#$28 +#$29 +#$2A +#$2B +#$2C +#$2D
    +#$2E +#$2F +#$30 +#$31 +#$32 +#$33 +#$34 +#$35 +#$36 +#$37 +#$38 +#$39
    +#$3A +#$3B +#$3C +#$3D +#$3E +#$3F +#$40 +#$40 +#$40 +#$40 +#$40 +#$40;

function DecodeTriplet(const Value: string; Delimiter: Char): string;
function DecodeQuotedPrintable(const Value: string): string;
function DecodeURL(const Value: string): string;
function EncodeTriplet(const Value: string; Delimiter: Char;
  Specials: TSpecials): string;
function EncodeQuotedPrintable(const Value: string): string;
function EncodeURLElement(const Value: string): string;
function EncodeURL(const Value: string): string;
function Decode4to3(const Value, Table: string): string;
function Decode4to3Ex(const Value, Table: string): string;
function Encode3to4(const Value, Table: string): string;
function DecodeBase64(const Value: string): string;
function EncodeBase64(const Value: string): string;
function DecodeUU(const Value: string): string;
function DecodeXX(const Value: string): string;
function UpdateCrc32(Value: Byte; Crc32: Integer): Integer;
function Crc32(const Value: string): Integer;
function UpdateCrc16(Value: Byte; Crc16: Word): Word;
function Crc16(const Value: string): Word;
function MD5(const Value: string): string;
function HMAC_MD5(Text, Key: string): string;

implementation

const

  Crc32Tab: array[0..255] of Integer = (
    Integer($00000000), Integer($77073096), Integer($EE0E612C), Integer($990951BA),
    Integer($076DC419), Integer($706AF48F), Integer($E963A535), Integer($9E6495A3),
    Integer($0EDB8832), Integer($79DCB8A4), Integer($E0D5E91E), Integer($97D2D988),
    Integer($09B64C2B), Integer($7EB17CBD), Integer($E7B82D07), Integer($90BF1D91),
    Integer($1DB71064), Integer($6AB020F2), Integer($F3B97148), Integer($84BE41DE),
    Integer($1ADAD47D), Integer($6DDDE4EB), Integer($F4D4B551), Integer($83D385C7),
    Integer($136C9856), Integer($646BA8C0), Integer($FD62F97A), Integer($8A65C9EC),
    Integer($14015C4F), Integer($63066CD9), Integer($FA0F3D63), Integer($8D080DF5),
    Integer($3B6E20C8), Integer($4C69105E), Integer($D56041E4), Integer($A2677172),
    Integer($3C03E4D1), Integer($4B04D447), Integer($D20D85FD), Integer($A50AB56B),
    Integer($35B5A8FA), Integer($42B2986C), Integer($DBBBC9D6), Integer($ACBCF940),
    Integer($32D86CE3), Integer($45DF5C75), Integer($DCD60DCF), Integer($ABD13D59),
    Integer($26D930AC), Integer($51DE003A), Integer($C8D75180), Integer($BFD06116),
    Integer($21B4F4B5), Integer($56B3C423), Integer($CFBA9599), Integer($B8BDA50F),
    Integer($2802B89E), Integer($5F058808), Integer($C60CD9B2), Integer($B10BE924),
    Integer($2F6F7C87), Integer($58684C11), Integer($C1611DAB), Integer($B6662D3D),
    Integer($76DC4190), Integer($01DB7106), Integer($98D220BC), Integer($EFD5102A),
    Integer($71B18589), Integer($06B6B51F), Integer($9FBFE4A5), Integer($E8B8D433),
    Integer($7807C9A2), Integer($0F00F934), Integer($9609A88E), Integer($E10E9818),
    Integer($7F6A0DBB), Integer($086D3D2D), Integer($91646C97), Integer($E6635C01),
    Integer($6B6B51F4), Integer($1C6C6162), Integer($856530D8), Integer($F262004E),
    Integer($6C0695ED), Integer($1B01A57B), Integer($8208F4C1), Integer($F50FC457),
    Integer($65B0D9C6), Integer($12B7E950), Integer($8BBEB8EA), Integer($FCB9887C),
    Integer($62DD1DDF), Integer($15DA2D49), Integer($8CD37CF3), Integer($FBD44C65),
    Integer($4DB26158), Integer($3AB551CE), Integer($A3BC0074), Integer($D4BB30E2),
    Integer($4ADFA541), Integer($3DD895D7), Integer($A4D1C46D), Integer($D3D6F4FB),
    Integer($4369E96A), Integer($346ED9FC), Integer($AD678846), Integer($DA60B8D0),
    Integer($44042D73), Integer($33031DE5), Integer($AA0A4C5F), Integer($DD0D7CC9),
    Integer($5005713C), Integer($270241AA), Integer($BE0B1010), Integer($C90C2086),
    Integer($5768B525), Integer($206F85B3), Integer($B966D409), Integer($CE61E49F),
    Integer($5EDEF90E), Integer($29D9C998), Integer($B0D09822), Integer($C7D7A8B4),
    Integer($59B33D17), Integer($2EB40D81), Integer($B7BD5C3B), Integer($C0BA6CAD),
    Integer($EDB88320), Integer($9ABFB3B6), Integer($03B6E20C), Integer($74B1D29A),
    Integer($EAD54739), Integer($9DD277AF), Integer($04DB2615), Integer($73DC1683),
    Integer($E3630B12), Integer($94643B84), Integer($0D6D6A3E), Integer($7A6A5AA8),
    Integer($E40ECF0B), Integer($9309FF9D), Integer($0A00AE27), Integer($7D079EB1),
    Integer($F00F9344), Integer($8708A3D2), Integer($1E01F268), Integer($6906C2FE),
    Integer($F762575D), Integer($806567CB), Integer($196C3671), Integer($6E6B06E7),
    Integer($FED41B76), Integer($89D32BE0), Integer($10DA7A5A), Integer($67DD4ACC),
    Integer($F9B9DF6F), Integer($8EBEEFF9), Integer($17B7BE43), Integer($60B08ED5),
    Integer($D6D6A3E8), Integer($A1D1937E), Integer($38D8C2C4), Integer($4FDFF252),
    Integer($D1BB67F1), Integer($A6BC5767), Integer($3FB506DD), Integer($48B2364B),
    Integer($D80D2BDA), Integer($AF0A1B4C), Integer($36034AF6), Integer($41047A60),
    Integer($DF60EFC3), Integer($A867DF55), Integer($316E8EEF), Integer($4669BE79),
    Integer($CB61B38C), Integer($BC66831A), Integer($256FD2A0), Integer($5268E236),
    Integer($CC0C7795), Integer($BB0B4703), Integer($220216B9), Integer($5505262F),
    Integer($C5BA3BBE), Integer($B2BD0B28), Integer($2BB45A92), Integer($5CB36A04),
    Integer($C2D7FFA7), Integer($B5D0CF31), Integer($2CD99E8B), Integer($5BDEAE1D),
    Integer($9B64C2B0), Integer($EC63F226), Integer($756AA39C), Integer($026D930A),
    Integer($9C0906A9), Integer($EB0E363F), Integer($72076785), Integer($05005713),
    Integer($95BF4A82), Integer($E2B87A14), Integer($7BB12BAE), Integer($0CB61B38),
    Integer($92D28E9B), Integer($E5D5BE0D), Integer($7CDCEFB7), Integer($0BDBDF21),
    Integer($86D3D2D4), Integer($F1D4E242), Integer($68DDB3F8), Integer($1FDA836E),
    Integer($81BE16CD), Integer($F6B9265B), Integer($6FB077E1), Integer($18B74777),
    Integer($88085AE6), Integer($FF0F6A70), Integer($66063BCA), Integer($11010B5C),
    Integer($8F659EFF), Integer($F862AE69), Integer($616BFFD3), Integer($166CCF45),
    Integer($A00AE278), Integer($D70DD2EE), Integer($4E048354), Integer($3903B3C2),
    Integer($A7672661), Integer($D06016F7), Integer($4969474D), Integer($3E6E77DB),
    Integer($AED16A4A), Integer($D9D65ADC), Integer($40DF0B66), Integer($37D83BF0),
    Integer($A9BCAE53), Integer($DEBB9EC5), Integer($47B2CF7F), Integer($30B5FFE9),
    Integer($BDBDF21C), Integer($CABAC28A), Integer($53B39330), Integer($24B4A3A6),
    Integer($BAD03605), Integer($CDD70693), Integer($54DE5729), Integer($23D967BF),
    Integer($B3667A2E), Integer($C4614AB8), Integer($5D681B02), Integer($2A6F2B94),
    Integer($B40BBE37), Integer($C30C8EA1), Integer($5A05DF1B), Integer($2D02EF8D)
    );

  Crc16Tab: array[0..255] of Word = (
    $0000, $1189, $2312, $329B, $4624, $57AD, $6536, $74BF,
    $8C48, $9DC1, $AF5A, $BED3, $CA6C, $DBE5, $E97E, $F8F7,
    $1081, $0108, $3393, $221A, $56A5, $472C, $75B7, $643E,
    $9CC9, $8D40, $BFDB, $AE52, $DAED, $CB64, $F9FF, $E876,
    $2102, $308B, $0210, $1399, $6726, $76AF, $4434, $55BD,
    $AD4A, $BCC3, $8E58, $9FD1, $EB6E, $FAE7, $C87C, $D9F5,
    $3183, $200A, $1291, $0318, $77A7, $662E, $54B5, $453C,
    $BDCB, $AC42, $9ED9, $8F50, $FBEF, $EA66, $D8FD, $C974,
    $4204, $538D, $6116, $709F, $0420, $15A9, $2732, $36BB,
    $CE4C, $DFC5, $ED5E, $FCD7, $8868, $99E1, $AB7A, $BAF3,
    $5285, $430C, $7197, $601E, $14A1, $0528, $37B3, $263A,
    $DECD, $CF44, $FDDF, $EC56, $98E9, $8960, $BBFB, $AA72,
    $6306, $728F, $4014, $519D, $2522, $34AB, $0630, $17B9,
    $EF4E, $FEC7, $CC5C, $DDD5, $A96A, $B8E3, $8A78, $9BF1,
    $7387, $620E, $5095, $411C, $35A3, $242A, $16B1, $0738,
    $FFCF, $EE46, $DCDD, $CD54, $B9EB, $A862, $9AF9, $8B70,
    $8408, $9581, $A71A, $B693, $C22C, $D3A5, $E13E, $F0B7,
    $0840, $19C9, $2B52, $3ADB, $4E64, $5FED, $6D76, $7CFF,
    $9489, $8500, $B79B, $A612, $D2AD, $C324, $F1BF, $E036,
    $18C1, $0948, $3BD3, $2A5A, $5EE5, $4F6C, $7DF7, $6C7E,
    $A50A, $B483, $8618, $9791, $E32E, $F2A7, $C03C, $D1B5,
    $2942, $38CB, $0A50, $1BD9, $6F66, $7EEF, $4C74, $5DFD,
    $B58B, $A402, $9699, $8710, $F3AF, $E226, $D0BD, $C134,
    $39C3, $284A, $1AD1, $0B58, $7FE7, $6E6E, $5CF5, $4D7C,
    $C60C, $D785, $E51E, $F497, $8028, $91A1, $A33A, $B2B3,
    $4A44, $5BCD, $6956, $78DF, $0C60, $1DE9, $2F72, $3EFB,
    $D68D, $C704, $F59F, $E416, $90A9, $8120, $B3BB, $A232,
    $5AC5, $4B4C, $79D7, $685E, $1CE1, $0D68, $3FF3, $2E7A,
    $E70E, $F687, $C41C, $D595, $A12A, $B0A3, $8238, $93B1,
    $6B46, $7ACF, $4854, $59DD, $2D62, $3CEB, $0E70, $1FF9,
    $F78F, $E606, $D49D, $C514, $B1AB, $A022, $92B9, $8330,
    $7BC7, $6A4E, $58D5, $495C, $3DE3, $2C6A, $1EF1, $0F78
    );

type
  TMD5Ctx = record
    State: array[0..3] of Integer;
    Count: array[0..1] of Integer;
    case Integer of
      0: (BufChar: array[0..63] of Byte);
      1: (BufLong: array[0..15] of Integer);
  end;

{==============================================================================}

function DecodeTriplet(const Value: string; Delimiter: Char): string;
var
  x, l: Integer;
  c: Char;
  s: string;
begin
  SetLength(Result, Length(Value));
  x := 1;
  l := 1;
  while x <= Length(Value) do
  begin
    c := Value[x];
    Inc(x);
    if c <> Delimiter then
      Result[l] := c
    else
      if x < Length(Value) then
      begin
        s := Copy(Value, x, 2);
        Inc(x, 2);
        Result[l] := Char(StrToIntDef('$' + s, 32))
      end
      else
        break;
    Inc(l);
  end;
  Dec(l);
  SetLength(Result, l);
end;

{==============================================================================}

function DecodeQuotedPrintable(const Value: string): string;
begin
  Result := DecodeTriplet(Value, '=');
end;

{==============================================================================}

function DecodeURL(const Value: string): string;
begin
  Result := DecodeTriplet(Value, '%');
end;

{==============================================================================}

function EncodeTriplet(const Value: string; Delimiter: Char;
  Specials: TSpecials): string;
var
  n, l: Integer;
  s: string;
  c: char;
begin
  SetLength(Result, Length(Value) * 3);
  l := 1;
  for n := 1 to Length(Value) do
  begin
    c := Value[n];
    if c in Specials then
    begin
      Result[l] := Delimiter;
      Inc(l);
      s := IntToHex(Ord(c), 2);
      Result[l] := s[1];
      Inc(l);
      Result[l] := s[2];
      Inc(l);
    end
    else
    begin
      Result[l] := c;
      Inc(l);
    end;
  end;
  Dec(l);
  SetLength(Result, l);
end;

{==============================================================================}

function EncodeQuotedPrintable(const Value: string): string;
begin
  Result := EncodeTriplet(Value, '=', SpecialChar +
    [Char(1)..Char(31), Char(128)..Char(255)]);
end;

{==============================================================================}

function EncodeURLElement(const Value: string): string;
begin
  Result := EncodeTriplet(Value, '%', URLSpecialChar + URLFullSpecialChar);
end;

{==============================================================================}

function EncodeURL(const Value: string): string;
begin
  Result := EncodeTriplet(Value, '%', URLSpecialChar);
end;

{==============================================================================}

function Decode4to3(const Value, Table: string): string;
var
  x, y, n, l: Integer;
  d: array[0..3] of Byte;
begin
  SetLength(Result, Length(Value));
  x := 1;
  l := 1;
  while x < Length(Value) do
  begin
    for n := 0 to 3 do
    begin
      if x > Length(Value) then
        d[n] := 64
      else
      begin
        y := Pos(Value[x], Table);
        if y < 1 then
          y := 1;
        d[n] := y - 1;
      end;
      Inc(x);
    end;
    Result[l] := Char((D[0] and $3F) shl 2 + (D[1] and $30) shr 4);
    Inc(l);
    if d[2] <> 64 then
    begin
      Result[l] := Char((D[1] and $0F) shl 4 + (D[2] and $3C) shr 2);
      Inc(l);
      if d[3] <> 64 then
      begin
        Result[l] := Char((D[2] and $03) shl 6 + (D[3] and $3F));
        Inc(l);
      end;
    end;
  end;
  Dec(l);
  SetLength(Result, l);
end;

{==============================================================================}

function Decode4to3Ex(const Value, Table: string): string;
var
  x, y, n, l: Integer;
  d: array[0..3] of Byte;
begin
  SetLength(Result, Length(Value));
  x := 1;
  l := 1;
  while x < Length(Value) do
  begin
    for n := 0 to 3 do
    begin
      if x > Length(Value) then
        d[n] := 64
      else
      begin
        y := Ord(Value[x]);
        if (y < 33) or (y > 127) then
          d[n] := 64
        else
          d[n] := Ord(Table[y - 32]);
      end;
      Inc(x);
    end;
    Result[l] := Char((D[0] and $3F) shl 2 + (D[1] and $30) shr 4);
    Inc(l);
    if d[2] <> 64 then
    begin
      Result[l] := Char((D[1] and $0F) shl 4 + (D[2] and $3C) shr 2);
      Inc(l);
      if d[3] <> 64 then
      begin
        Result[l] := Char((D[2] and $03) shl 6 + (D[3] and $3F));
        Inc(l);
      end;
    end;
  end;
  Dec(l);
  SetLength(Result, l);
end;

{==============================================================================}

function Encode3to4(const Value, Table: string): string;
var
  c: Byte;
  n, l: Integer;
  Count: Integer;
  DOut: array[0..3] of Byte;
begin
  setlength(Result, ((Length(Value) + 2) div 3) * 4);
  l := 1;
  Count := 1;
  while Count <= Length(Value) do
  begin
    c := Ord(Value[Count]);
    Inc(Count);
    DOut[0] := (c and $FC) shr 2;
    DOut[1] := (c and $03) shl 4;
    if Count <= Length(Value) then
    begin
      c := Ord(Value[Count]);
      Inc(Count);
      DOut[1] := DOut[1] + (c and $F0) shr 4;
      DOut[2] := (c and $0F) shl 2;
      if Count <= Length(Value) then
      begin
        c := Ord(Value[Count]);
        Inc(Count);
        DOut[2] := DOut[2] + (c and $C0) shr 6;
        DOut[3] := (c and $3F);
      end
      else
      begin
        DOut[3] := $40;
      end;
    end
    else
    begin
      DOut[2] := $40;
      DOut[3] := $40;
    end;
    for n := 0 to 3 do
    begin
      Result[l] := Table[DOut[n] + 1];
      Inc(l);
    end;
  end;
end;

{==============================================================================}

function DecodeBase64(const Value: string): string;
begin
  Result := Decode4to3Ex(Value, ReTableBase64);
end;

{==============================================================================}

function EncodeBase64(const Value: string): string;
begin
  Result := Encode3to4(Value, TableBase64);
end;

{==============================================================================}

function DecodeUU(const Value: string): string;
var
  s: string;
  uut: string;
  x: Integer;
begin
  Result := '';
  uut := TableUU;
  s := trim(UpperCase(Value));
  if s = '' then Exit;
  if Pos('BEGIN', s) = 1 then
    Exit;
  if Pos('END', s) = 1 then
    Exit;
  if Pos('TABLE', s) = 1 then
    Exit; //ignore Table yet (set custom UUT)
  //begin decoding
  x := Pos(Value[1], uut) - 1;
  x := Round((x / 3) * 4);
  //x - lenght UU line
  s := Copy(Value, 2, x);
  if s = '' then
    Exit;
  Result := Decode4to3(s, uut);
end;

{==============================================================================}

function DecodeXX(const Value: string): string;
var
  s: string;
  x: Integer;
begin
  Result := '';
  s := trim(UpperCase(Value));
  if s = '' then
    Exit;
  if Pos('BEGIN', s) = 1 then
    Exit;
  if Pos('END', s) = 1 then
    Exit;
  //begin decoding
  x := Pos(Value[1], TableXX) - 1;
  x := Round((x / 3) * 4);
  //x - lenght XX line
  s := Copy(Value, 2, x);
  if s = '' then
    Exit;
  Result := Decode4to3(s, TableXX);
end;

{==============================================================================}

function UpdateCrc32(Value: Byte; Crc32: Integer): Integer;
begin
  Result := ((Crc32 shr 8) and Integer($00FFFFFF)) xor
    crc32tab[Byte(Crc32 xor Integer(Value)) and Integer($000000FF)];
end;

{==============================================================================}

function Crc32(const Value: string): Integer;
var
  n: Integer;
begin
  Result := Integer($FFFFFFFF);
  for n := 1 to Length(Value) do
    Result := UpdateCrc32(Ord(Value[n]), Result);
end;

{==============================================================================}

function UpdateCrc16(Value: Byte; Crc16: Word): Word;
begin
  Result := ((Crc16 shr 8) and $00FF) xor
    crc16tab[Byte(Crc16 xor (Word(Value)) and $00FF)];
end;

{==============================================================================}

function Crc16(const Value: string): Word;
var
  n: Integer;
begin
  Result := $FFFF;
  for n := 1 to Length(Value) do
    Result := UpdateCrc16(Ord(Value[n]), Result);
end;

{==============================================================================}

procedure MD5Init(var MD5Context: TMD5Ctx);
begin
  FillChar(MD5Context, SizeOf(TMD5Ctx), #0);
  with MD5Context do
  begin
    State[0] := Integer($67452301);
    State[1] := Integer($EFCDAB89);
    State[2] := Integer($98BADCFE);
    State[3] := Integer($10325476);
  end;
end;

procedure MD5Transform(var Buf: array of LongInt; const Data: array of LongInt);
var
  A, B, C, D: LongInt;

  procedure Round1(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (Z xor (X and (Y xor Z))) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X);
  end;

  procedure Round2(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (Y xor (Z and (X xor Y))) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X);
  end;

  procedure Round3(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (X xor Y xor Z) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X);
  end;

  procedure Round4(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (Y xor (X or not Z)) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X);
  end;
begin
  A := Buf[0];
  B := Buf[1];
  C := Buf[2];
  D := Buf[3];

  Round1(A, B, C, D, Data[0] + Longint($D76AA478), 7);
  Round1(D, A, B, C, Data[1] + Longint($E8C7B756), 12);
  Round1(C, D, A, B, Data[2] + Longint($242070DB), 17);
  Round1(B, C, D, A, Data[3] + Longint($C1BDCEEE), 22);
  Round1(A, B, C, D, Data[4] + Longint($F57C0FAF), 7);
  Round1(D, A, B, C, Data[5] + Longint($4787C62A), 12);
  Round1(C, D, A, B, Data[6] + Longint($A8304613), 17);
  Round1(B, C, D, A, Data[7] + Longint($FD469501), 22);
  Round1(A, B, C, D, Data[8] + Longint($698098D8), 7);
  Round1(D, A, B, C, Data[9] + Longint($8B44F7AF), 12);
  Round1(C, D, A, B, Data[10] + Longint($FFFF5BB1), 17);
  Round1(B, C, D, A, Data[11] + Longint($895CD7BE), 22);
  Round1(A, B, C, D, Data[12] + Longint($6B901122), 7);
  Round1(D, A, B, C, Data[13] + Longint($FD987193), 12);
  Round1(C, D, A, B, Data[14] + Longint($A679438E), 17);
  Round1(B, C, D, A, Data[15] + Longint($49B40821), 22);

  Round2(A, B, C, D, Data[1] + Longint($F61E2562), 5);
  Round2(D, A, B, C, Data[6] + Longint($C040B340), 9);
  Round2(C, D, A, B, Data[11] + Longint($265E5A51), 14);
  Round2(B, C, D, A, Data[0] + Longint($E9B6C7AA), 20);
  Round2(A, B, C, D, Data[5] + Longint($D62F105D), 5);
  Round2(D, A, B, C, Data[10] + Longint($02441453), 9);
  Round2(C, D, A, B, Data[15] + Longint($D8A1E681), 14);
  Round2(B, C, D, A, Data[4] + Longint($E7D3FBC8), 20);
  Round2(A, B, C, D, Data[9] + Longint($21E1CDE6), 5);
  Round2(D, A, B, C, Data[14] + Longint($C33707D6), 9);
  Round2(C, D, A, B, Data[3] + Longint($F4D50D87), 14);
  Round2(B, C, D, A, Data[8] + Longint($455A14ED), 20);
  Round2(A, B, C, D, Data[13] + Longint($A9E3E905), 5);
  Round2(D, A, B, C, Data[2] + Longint($FCEFA3F8), 9);
  Round2(C, D, A, B, Data[7] + Longint($676F02D9), 14);
  Round2(B, C, D, A, Data[12] + Longint($8D2A4C8A), 20);

  Round3(A, B, C, D, Data[5] + Longint($FFFA3942), 4);
  Round3(D, A, B, C, Data[8] + Longint($8771F681), 11);
  Round3(C, D, A, B, Data[11] + Longint($6D9D6122), 16);
  Round3(B, C, D, A, Data[14] + Longint($FDE5380C), 23);
  Round3(A, B, C, D, Data[1] + Longint($A4BEEA44), 4);
  Round3(D, A, B, C, Data[4] + Longint($4BDECFA9), 11);
  Round3(C, D, A, B, Data[7] + Longint($F6BB4B60), 16);
  Round3(B, C, D, A, Data[10] + Longint($BEBFBC70), 23);
  Round3(A, B, C, D, Data[13] + Longint($289B7EC6), 4);
  Round3(D, A, B, C, Data[0] + Longint($EAA127FA), 11);
  Round3(C, D, A, B, Data[3] + Longint($D4EF3085), 16);
  Round3(B, C, D, A, Data[6] + Longint($04881D05), 23);
  Round3(A, B, C, D, Data[9] + Longint($D9D4D039), 4);
  Round3(D, A, B, C, Data[12] + Longint($E6DB99E5), 11);
  Round3(C, D, A, B, Data[15] + Longint($1FA27CF8), 16);
  Round3(B, C, D, A, Data[2] + Longint($C4AC5665), 23);

  Round4(A, B, C, D, Data[0] + Longint($F4292244), 6);
  Round4(D, A, B, C, Data[7] + Longint($432AFF97), 10);
  Round4(C, D, A, B, Data[14] + Longint($AB9423A7), 15);
  Round4(B, C, D, A, Data[5] + Longint($FC93A039), 21);
  Round4(A, B, C, D, Data[12] + Longint($655B59C3), 6);
  Round4(D, A, B, C, Data[3] + Longint($8F0CCC92), 10);
  Round4(C, D, A, B, Data[10] + Longint($FFEFF47D), 15);
  Round4(B, C, D, A, Data[1] + Longint($85845DD1), 21);
  Round4(A, B, C, D, Data[8] + Longint($6FA87E4F), 6);
  Round4(D, A, B, C, Data[15] + Longint($FE2CE6E0), 10);
  Round4(C, D, A, B, Data[6] + Longint($A3014314), 15);
  Round4(B, C, D, A, Data[13] + Longint($4E0811A1), 21);
  Round4(A, B, C, D, Data[4] + Longint($F7537E82), 6);
  Round4(D, A, B, C, Data[11] + Longint($BD3AF235), 10);
  Round4(C, D, A, B, Data[2] + Longint($2AD7D2BB), 15);
  Round4(B, C, D, A, Data[9] + Longint($EB86D391), 21);

  Inc(Buf[0], A);
  Inc(Buf[1], B);
  Inc(Buf[2], C);
  Inc(Buf[3], D);
end;

procedure MD5Update(var MD5Context: TMD5Ctx; const Data: string);
var
  Index, t, len: Integer;
begin
  len := Length(Data);
  with MD5Context do
  begin
    T := Count[0];
    Inc(Count[0], Len shl 3);
    if Count[0] < T then
      Inc(Count[1]);
    Inc(Count[1], Len shr 29);
    T := (T shr 3) and $3F;
    Index := 0;
    if T <> 0 then
    begin
      Index := T;
      T := 64 - T;
      if Len < T then
      begin
        Move(Data, Bufchar[Index], Len);
        Exit;
      end;
      Move(Data, Bufchar[Index], T);
      MD5Transform(State, Buflong);
      Dec(Len, T);
      Index := T;
    end;
    while Len >= 64 do
    begin
      Move(Data[Index + 1], Bufchar, 64);
      MD5Transform(State, Buflong);
      Inc(Index, 64);
      Dec(Len, 64);
    end;
    Move(Data[Index + 1], Bufchar, Len);
  end
end;

function MD5Final(var MD5Context: TMD5Ctx): string;
var
  Cnt: Word;
  P: Byte;
  digest: array[0..15] of Char;
  i: Integer;
begin
  for I := 0 to 15 do
    Byte(Digest[I]) := I + 1;
  with MD5Context do
  begin
    Cnt := (Count[0] shr 3) and $3F;
    P := Cnt;
    BufChar[P] := $80;
    Inc(P);
    Cnt := 64 - 1 - Cnt;
    if Cnt < 8 then
    begin
      FillChar(BufChar[P], Cnt, #0);
      MD5Transform(State, BufLong);
      FillChar(BufChar, 56, #0);
    end
    else
      FillChar(BufChar[P], Cnt - 8, #0);
    BufLong[14] := Count[0];
    BufLong[15] := Count[1];
    MD5Transform(State, BufLong);
    Move(State, Digest, 16);
    Result := '';
    for i := 0 to 15 do
      Result := Result + Char(digest[i]);
  end;
  FillChar(MD5Context, SizeOf(TMD5Ctx), #0)
end;

{==============================================================================}

function MD5(const Value: string): string;
var
  MD5Context: TMD5Ctx;
begin
  MD5Init(MD5Context);
  MD5Update(MD5Context, Value);
  Result := MD5Final(MD5Context);
end;

{==============================================================================}

function HMAC_MD5(Text, Key: string): string;
var
  ipad, opad, s: string;
  n: Integer;
  MD5Context: TMD5Ctx;
begin
  if Length(Key) > 64 then
    Key := md5(Key);
  ipad := '';
  for n := 1 to 64 do
    ipad := ipad + #$36;
  opad := '';
  for n := 1 to 64 do
    opad := opad + #$5C;
  for n := 1 to Length(Key) do
  begin
    ipad[n] := Char(Byte(ipad[n]) xor Byte(Key[n]));
    opad[n] := Char(Byte(opad[n]) xor Byte(Key[n]));
  end;
  MD5Init(MD5Context);
  MD5Update(MD5Context, ipad);
  MD5Update(MD5Context, Text);
  s := MD5Final(MD5Context);
  MD5Init(MD5Context);
  MD5Update(MD5Context, opad);
  MD5Update(MD5Context, s);
  Result := MD5Final(MD5Context);
end;

end.
