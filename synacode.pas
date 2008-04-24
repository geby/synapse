{==============================================================================|
| Project : Delphree - Synapse                                   | 001.003.001 |
|==============================================================================|
| Content: Coding and decoding support                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2000, 2001.               |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

unit SynaCode;

interface

uses
  sysutils;

const
  SpecialChar:set of char
    =['=','(',')','[',']','<','>',':',';','.',',','@','/','?','\','"','_'];

  TableBase64=
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';
  TableUU=
    '`!"#$%&''()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_';
  TableXX=
    '+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_';


  Crc32Tab: array[0..255] of integer = (
    $00000000, $77073096, $ee0e612c, $990951ba, $076dc419, $706af48f, $e963a535, $9e6495a3,
    $0edb8832, $79dcb8a4, $e0d5e91e, $97d2d988, $09b64c2b, $7eb17cbd, $e7b82d07, $90bf1d91,
    $1db71064, $6ab020f2, $f3b97148, $84be41de, $1adad47d, $6ddde4eb, $f4d4b551, $83d385c7,
    $136c9856, $646ba8c0, $fd62f97a, $8a65c9ec, $14015c4f, $63066cd9, $fa0f3d63, $8d080df5,
    $3b6e20c8, $4c69105e, $d56041e4, $a2677172, $3c03e4d1, $4b04d447, $d20d85fd, $a50ab56b,
    $35b5a8fa, $42b2986c, $dbbbc9d6, $acbcf940, $32d86ce3, $45df5c75, $dcd60dcf, $abd13d59,
    $26d930ac, $51de003a, $c8d75180, $bfd06116, $21b4f4b5, $56b3c423, $cfba9599, $b8bda50f,
    $2802b89e, $5f058808, $c60cd9b2, $b10be924, $2f6f7c87, $58684c11, $c1611dab, $b6662d3d,
    $76dc4190, $01db7106, $98d220bc, $efd5102a, $71b18589, $06b6b51f, $9fbfe4a5, $e8b8d433,
    $7807c9a2, $0f00f934, $9609a88e, $e10e9818, $7f6a0dbb, $086d3d2d, $91646c97, $e6635c01,
    $6b6b51f4, $1c6c6162, $856530d8, $f262004e, $6c0695ed, $1b01a57b, $8208f4c1, $f50fc457,
    $65b0d9c6, $12b7e950, $8bbeb8ea, $fcb9887c, $62dd1ddf, $15da2d49, $8cd37cf3, $fbd44c65,
    $4db26158, $3ab551ce, $a3bc0074, $d4bb30e2, $4adfa541, $3dd895d7, $a4d1c46d, $d3d6f4fb,
    $4369e96a, $346ed9fc, $ad678846, $da60b8d0, $44042d73, $33031de5, $aa0a4c5f, $dd0d7cc9,
    $5005713c, $270241aa, $be0b1010, $c90c2086, $5768b525, $206f85b3, $b966d409, $ce61e49f,
    $5edef90e, $29d9c998, $b0d09822, $c7d7a8b4, $59b33d17, $2eb40d81, $b7bd5c3b, $c0ba6cad,
    $edb88320, $9abfb3b6, $03b6e20c, $74b1d29a, $ead54739, $9dd277af, $04db2615, $73dc1683,
    $e3630b12, $94643b84, $0d6d6a3e, $7a6a5aa8, $e40ecf0b, $9309ff9d, $0a00ae27, $7d079eb1,
    $f00f9344, $8708a3d2, $1e01f268, $6906c2fe, $f762575d, $806567cb, $196c3671, $6e6b06e7,
    $fed41b76, $89d32be0, $10da7a5a, $67dd4acc, $f9b9df6f, $8ebeeff9, $17b7be43, $60b08ed5,
    $d6d6a3e8, $a1d1937e, $38d8c2c4, $4fdff252, $d1bb67f1, $a6bc5767, $3fb506dd, $48b2364b,
    $d80d2bda, $af0a1b4c, $36034af6, $41047a60, $df60efc3, $a867df55, $316e8eef, $4669be79,
    $cb61b38c, $bc66831a, $256fd2a0, $5268e236, $cc0c7795, $bb0b4703, $220216b9, $5505262f,
    $c5ba3bbe, $b2bd0b28, $2bb45a92, $5cb36a04, $c2d7ffa7, $b5d0cf31, $2cd99e8b, $5bdeae1d,
    $9b64c2b0, $ec63f226, $756aa39c, $026d930a, $9c0906a9, $eb0e363f, $72076785, $05005713,
    $95bf4a82, $e2b87a14, $7bb12bae, $0cb61b38, $92d28e9b, $e5d5be0d, $7cdcefb7, $0bdbdf21,
    $86d3d2d4, $f1d4e242, $68ddb3f8, $1fda836e, $81be16cd, $f6b9265b, $6fb077e1, $18b74777,
    $88085ae6, $ff0f6a70, $66063bca, $11010b5c, $8f659eff, $f862ae69, $616bffd3, $166ccf45,
    $a00ae278, $d70dd2ee, $4e048354, $3903b3c2, $a7672661, $d06016f7, $4969474d, $3e6e77db,
    $aed16a4a, $d9d65adc, $40df0b66, $37d83bf0, $a9bcae53, $debb9ec5, $47b2cf7f, $30b5ffe9,
    $bdbdf21c, $cabac28a, $53b39330, $24b4a3a6, $bad03605, $cdd70693, $54de5729, $23d967bf,
    $b3667a2e, $c4614ab8, $5d681b02, $2a6f2b94, $b40bbe37, $c30c8ea1, $5a05df1b, $2d02ef8d
    );

  Crc16Tab: array[0..255] of word = (
      $0000, $1189, $2312, $329b, $4624, $57ad, $6536, $74bf,
      $8c48, $9dc1, $af5a, $bed3, $ca6c, $dbe5, $e97e, $f8f7,
      $1081, $0108, $3393, $221a, $56a5, $472c, $75b7, $643e,
      $9cc9, $8d40, $bfdb, $ae52, $daed, $cb64, $f9ff, $e876,
      $2102, $308b, $0210, $1399, $6726, $76af, $4434, $55bd,
      $ad4a, $bcc3, $8e58, $9fd1, $eb6e, $fae7, $c87c, $d9f5,
      $3183, $200a, $1291, $0318, $77a7, $662e, $54b5, $453c,
      $bdcb, $ac42, $9ed9, $8f50, $fbef, $ea66, $d8fd, $c974,
      $4204, $538d, $6116, $709f, $0420, $15a9, $2732, $36bb,
      $ce4c, $dfc5, $ed5e, $fcd7, $8868, $99e1, $ab7a, $baf3,
      $5285, $430c, $7197, $601e, $14a1, $0528, $37b3, $263a,
      $decd, $cf44, $fddf, $ec56, $98e9, $8960, $bbfb, $aa72,
      $6306, $728f, $4014, $519d, $2522, $34ab, $0630, $17b9,
      $ef4e, $fec7, $cc5c, $ddd5, $a96a, $b8e3, $8a78, $9bf1,
      $7387, $620e, $5095, $411c, $35a3, $242a, $16b1, $0738,
      $ffcf, $ee46, $dcdd, $cd54, $b9eb, $a862, $9af9, $8b70,
      $8408, $9581, $a71a, $b693, $c22c, $d3a5, $e13e, $f0b7,
      $0840, $19c9, $2b52, $3adb, $4e64, $5fed, $6d76, $7cff,
      $9489, $8500, $b79b, $a612, $d2ad, $c324, $f1bf, $e036,
      $18c1, $0948, $3bd3, $2a5a, $5ee5, $4f6c, $7df7, $6c7e,
      $a50a, $b483, $8618, $9791, $e32e, $f2a7, $c03c, $d1b5,
      $2942, $38cb, $0a50, $1bd9, $6f66, $7eef, $4c74, $5dfd,
      $b58b, $a402, $9699, $8710, $f3af, $e226, $d0bd, $c134,
      $39c3, $284a, $1ad1, $0b58, $7fe7, $6e6e, $5cf5, $4d7c,
      $c60c, $d785, $e51e, $f497, $8028, $91a1, $a33a, $b2b3,
      $4a44, $5bcd, $6956, $78df, $0c60, $1de9, $2f72, $3efb,
      $d68d, $c704, $f59f, $e416, $90a9, $8120, $b3bb, $a232,
      $5ac5, $4b4c, $79d7, $685e, $1ce1, $0d68, $3ff3, $2e7a,
      $e70e, $f687, $c41c, $d595, $a12a, $b0a3, $8238, $93b1,
      $6b46, $7acf, $4854, $59dd, $2d62, $3ceb, $0e70, $1ff9,
      $f78f, $e606, $d49d, $c514, $b1ab, $a022, $92b9, $8330,
      $7bc7, $6a4e, $58d5, $495c, $3de3, $2c6a, $1ef1, $0f78
   );

type
  TMD5Ctx = record
    State: array[0..3] of integer;
    Count: array[0..1] of integer;
    case Integer of
      0: (BufChar: array[0..63] of Byte);
      1: (BufLong: array[0..15] of integer);
  end;

function DecodeQuotedPrintable(value:string):string;
function EncodeQuotedPrintable(value:string):string;
function Decode4to3(value,table:string):string;
function DecodeBase64(value:string):string;
function EncodeBase64(value:string):string;
function DecodeUU(value:string):string;
function DecodeXX(value:string):string;
function UpdateCrc32(value:byte;crc32:integer):integer;
function Crc32(value:string):integer;
function UpdateCrc16(value:byte;crc16:word):word;
function Crc16(value:string):word;
function MD5(value:string):string;
function HMAC_MD5(text,key:string):string;

implementation

{==============================================================================}
{DecodeQuotedPrintable}
function DecodeQuotedPrintable(value:string):string;
var
  x:integer;
  c:char;
  s:string;
begin
  result:='';
  x:=1;
  while x<=length(value) do
    begin
      c:=value[x];
      inc(x);
      if c<>'='
        then result:=result+c
        else
          if x<length(value)
            then
              begin
                s:=copy(value,x,2);
                inc(x,2);
                result:=result+char(strtointdef('$'+s,32));
              end;
    end;
end;

{==============================================================================}
{EncodeQuotedPrintable}
function EncodeQuotedPrintable(value:string):string;
var
  n:integer;
  s:string;
begin
  result:='';
  for n:=1 to length(value) do
    begin
      s:=value[n];
      if s[1] in (SpecialChar+[char(1)..char(31),char(128)..char(255)])
        then s:='='+inttohex(ord(s[1]),2);
      result:=result+s;
    end;
end;

{==============================================================================}
{Decode4to3}
function Decode4to3(value,table:string):string;
var
  x,y,n:integer;
  d: array[0..3] of byte;
begin
  result:='';
  x:=1;
  while x<length(value) do
    begin
      for n:=0 to 3 do
        begin
          if x>length(value)
            then d[n]:=64
            else
              begin
                y:=pos(value[x],table);
                if y<1 then y:=65;
                d[n]:=y-1;
              end;
          inc(x);
        end;
      result:=result+char((D[0] and $3F) shl 2 + (D[1] and $30) shr 4);
      if d[2]<>64 then
        begin
          result:=result+char((D[1] and $0F) shl 4 + (D[2] and $3C) shr 2);
          if d[3]<>64 then
            result:=result+char((D[2] and $03) shl 6 + (D[3] and $3F));
        end;
    end;
end;

{==============================================================================}
{DecodeBase64}
function DecodeBase64(value:string):string;
begin
  result:=Decode4to3(value,TableBase64);
end;

{==============================================================================}
{EncodeBase64}
function EncodeBase64(value:string):string;
var
  c:byte;
  n:integer;
  Count:integer;
  DOut:array [0..3] of byte;
begin
  result:='';
  Count := 1;
  while count<=length(value) do
    begin
      c:=ord(value[count]);
      inc(count);
      DOut[0]:=(c and $FC) shr 2;
      DOut[1]:=(c and $03) shl 4;
      if count<=length(value)
        then
          begin
            c:=ord(value[count]);
            inc(count);
            DOut[1]:=DOut[1]+(c and $F0) shr 4;
            DOut[2]:=(c and $0F) shl 2;
            if count<=length(value)
              then
                begin
                  c:=ord(value[count]);
                  inc(count);
                  DOut[2]:=DOut[2]+(c and $C0) shr 6;
                  DOut[3]:=(c and $3F);
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
      for n:=0 to 3 do
        result:=result+TableBase64[DOut[n]+1];
    end;
end;

{==============================================================================}
{DecodeUU}
function DecodeUU(value:string):string;
var
  s:string;
  uut:string;
  x:integer;
begin
  result:='';
  uut:=TableUU;
  s:=trim(uppercase(value));
  if s='' then exit;
  if pos('BEGIN',s)=1 then exit;
  if pos('END',s)=1 then exit;
  if pos('TABLE',s)=1 then exit;  //ignore table yet (set custom UUT)
  //begin decoding
  x:=pos(value[1],uut)-1;
  x:=round((x/3)*4);
  //x - lenght UU line
  s:=copy(value,2,x);
  if s='' then exit;
  result:=Decode4to3(s,uut);
end;

{==============================================================================}
{DecodeXX}
function DecodeXX(value:string):string;
var
  s:string;
  x:integer;
begin
  result:='';
  s:=trim(uppercase(value));
  if s='' then exit;
  if pos('BEGIN',s)=1 then exit;
  if pos('END',s)=1 then exit;
  //begin decoding
  x:=pos(value[1],TableXX)-1;
  x:=round((x/3)*4);
  //x - lenght XX line
  s:=copy(value,2,x);
  if s='' then exit;
  result:=Decode4to3(s,TableXX);
end;

{==============================================================================}
{UpdateCrc32}
function UpdateCrc32(value:byte;crc32:integer):integer;
begin
   result:=((crc32 shr 8) and $00FFFFFF)
    xor crc32tab[byte(crc32 XOR integer(value)) and $000000FF];
end;

{==============================================================================}
{Crc32}
function Crc32(value:string):integer;
var
  n:integer;
begin
  result:=$FFFFFFFF;
  for n:=1 to length(value) do
    result:=UpdateCrc32(ord(value[n]), result);
end;

{==============================================================================}
{UpdateCrc16}
function UpdateCrc16(value:byte;crc16:word):word;
begin
   result:=((crc16 shr 8) and $00FF)
    xor crc16tab[byte(crc16 XOR (word(value)) and $00FF)];
end;

{==============================================================================}
{Crc16}
function Crc16(value:string):word;
var
  n:integer;
begin
  result:=$FFFF;
  for n:=1 to length(value) do
    result:=UpdateCrc16(ord(value[n]), result);
end;

{==============================================================================}
procedure MD5Init(var MD5Context: TMD5Ctx);
begin
  FillChar(MD5Context, SizeOf(TMD5Ctx), #0);
  with MD5Context do begin
    State[0] := $67452301;
    State[1] := $EFCDAB89;
    State[2] := $98BADCFE;
    State[3] := $10325476;
  end
end;

procedure MD5Transform(var Buf:array of LongInt; const Data:array of LongInt);
var
  A,B,C,D: LongInt;

  procedure Round1(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (Z xor (X and (Y xor Z))) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X)
  end;

  procedure Round2(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (Y xor (Z and (X xor Y))) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X)
  end;

  procedure Round3(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (X xor Y xor Z) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X)
  end;

  procedure Round4(var W: LongInt; X, Y, Z, Data: LongInt; S: Byte);
  begin
    Inc(W, (Y xor (X or not Z)) + Data);
    W := (W shl S) or (W shr (32 - S));
    Inc(W, X)
  end;
begin
  A:=Buf[0];
  B:=Buf[1];
  C:=Buf[2];
  D:=Buf[3];

  Round1(A,B,C,D, Data[ 0] + longint($d76aa478),  7);
  Round1(D,A,B,C, Data[ 1] + longint($e8c7b756), 12);
  Round1(C,D,A,B, Data[ 2] + longint($242070db), 17);
  Round1(B,C,D,A, Data[ 3] + longint($c1bdceee), 22);
  Round1(A,B,C,D, Data[ 4] + longint($f57c0faf),  7);
  Round1(D,A,B,C, Data[ 5] + longint($4787c62a), 12);
  Round1(C,D,A,B, Data[ 6] + longint($a8304613), 17);
  Round1(B,C,D,A, Data[ 7] + longint($fd469501), 22);
  Round1(A,B,C,D, Data[ 8] + longint($698098d8),  7);
  Round1(D,A,B,C, Data[ 9] + longint($8b44f7af), 12);
  Round1(C,D,A,B, Data[10] + longint($ffff5bb1), 17);
  Round1(B,C,D,A, Data[11] + longint($895cd7be), 22);
  Round1(A,B,C,D, Data[12] + longint($6b901122),  7);
  Round1(D,A,B,C, Data[13] + longint($fd987193), 12);
  Round1(C,D,A,B, Data[14] + longint($a679438e), 17);
  Round1(B,C,D,A, Data[15] + longint($49b40821), 22);

  Round2(A,B,C,D, Data[ 1] + longint($f61e2562),  5);
  Round2(D,A,B,C, Data[ 6] + longint($c040b340),  9);
  Round2(C,D,A,B, Data[11] + longint($265e5a51), 14);
  Round2(B,C,D,A, Data[ 0] + longint($e9b6c7aa), 20);
  Round2(A,B,C,D, Data[ 5] + longint($d62f105d),  5);
  Round2(D,A,B,C, Data[10] + longint($02441453),  9);
  Round2(C,D,A,B, Data[15] + longint($d8a1e681), 14);
  Round2(B,C,D,A, Data[ 4] + longint($e7d3fbc8), 20);
  Round2(A,B,C,D, Data[ 9] + longint($21e1cde6),  5);
  Round2(D,A,B,C, Data[14] + longint($c33707d6),  9);
  Round2(C,D,A,B, Data[ 3] + longint($f4d50d87), 14);
  Round2(B,C,D,A, Data[ 8] + longint($455a14ed), 20);
  Round2(A,B,C,D, Data[13] + longint($a9e3e905),  5);
  Round2(D,A,B,C, Data[ 2] + longint($fcefa3f8),  9);
  Round2(C,D,A,B, Data[ 7] + longint($676f02d9), 14);
  Round2(B,C,D,A, Data[12] + longint($8d2a4c8a), 20);

  Round3(A,B,C,D, Data[ 5] + longint($fffa3942),  4);
  Round3(D,A,B,C, Data[ 8] + longint($8771f681), 11);
  Round3(C,D,A,B, Data[11] + longint($6d9d6122), 16);
  Round3(B,C,D,A, Data[14] + longint($fde5380c), 23);
  Round3(A,B,C,D, Data[ 1] + longint($a4beea44),  4);
  Round3(D,A,B,C, Data[ 4] + longint($4bdecfa9), 11);
  Round3(C,D,A,B, Data[ 7] + longint($f6bb4b60), 16);
  Round3(B,C,D,A, Data[10] + longint($bebfbc70), 23);
  Round3(A,B,C,D, Data[13] + longint($289b7ec6),  4);
  Round3(D,A,B,C, Data[ 0] + longint($eaa127fa), 11);
  Round3(C,D,A,B, Data[ 3] + longint($d4ef3085), 16);
  Round3(B,C,D,A, Data[ 6] + longint($04881d05), 23);
  Round3(A,B,C,D, Data[ 9] + longint($d9d4d039),  4);
  Round3(D,A,B,C, Data[12] + longint($e6db99e5), 11);
  Round3(C,D,A,B, Data[15] + longint($1fa27cf8), 16);
  Round3(B,C,D,A, Data[ 2] + longint($c4ac5665), 23);

  Round4(A,B,C,D, Data[ 0] + longint($f4292244),  6);
  Round4(D,A,B,C, Data[ 7] + longint($432aff97), 10);
  Round4(C,D,A,B, Data[14] + longint($ab9423a7), 15);
  Round4(B,C,D,A, Data[ 5] + longint($fc93a039), 21);
  Round4(A,B,C,D, Data[12] + longint($655b59c3),  6);
  Round4(D,A,B,C, Data[ 3] + longint($8f0ccc92), 10);
  Round4(C,D,A,B, Data[10] + longint($ffeff47d), 15);
  Round4(B,C,D,A, Data[ 1] + longint($85845dd1), 21);
  Round4(A,B,C,D, Data[ 8] + longint($6fa87e4f),  6);
  Round4(D,A,B,C, Data[15] + longint($fe2ce6e0), 10);
  Round4(C,D,A,B, Data[ 6] + longint($a3014314), 15);
  Round4(B,C,D,A, Data[13] + longint($4e0811a1), 21);
  Round4(A,B,C,D, Data[ 4] + longint($f7537e82),  6);
  Round4(D,A,B,C, Data[11] + longint($bd3af235), 10);
  Round4(C,D,A,B, Data[ 2] + longint($2ad7d2bb), 15);
  Round4(B,C,D,A, Data[ 9] + longint($eb86d391), 21);

  Inc(Buf[0],A);
  Inc(Buf[1],B);
  Inc(Buf[2],C);
  Inc(Buf[3],D);
end;

procedure MD5Update(var MD5Context:TMD5Ctx; const Data:string);
var
  Index,t,len:integer;
begin
  len:=length(data);
  with MD5Context do
    begin
      T:=Count[0];
      Inc(Count[0], Len shl 3);
      if Count[0]<T then
        Inc(Count[1]);
      Inc(Count[1], Len shr 29);
      T:=(T shr 3) and $3F;
      Index:=0;
      if T<>0 then
        begin
          Index:=T;
          T:=64-T;
          if Len<T then
            begin
              Move(Data, Bufchar[Index], Len);
              Exit;
            end;
          Move(Data, Bufchar[Index], T);
          MD5Transform(State, Buflong);
          Dec(Len, T);
          Index:=T;
        end;
      while Len>=64 do
        begin
          Move(Data[Index+1], Bufchar, 64);
          MD5Transform(State, Buflong);
          Inc(Index, 64);
          Dec(Len, 64);
        end;
      Move(Data[Index+1], Bufchar, Len);
    end
end;

function MD5Final(var MD5Context: TMD5Ctx):string;
var
  Cnt : Word;
  P   : Byte;
  digest:array[0..15] of Char;
  i:integer;
begin
  for I:=0 to 15 do
    Byte(Digest[I]):=I+1;
  with MD5Context do
    begin
      Cnt:=(Count[0] shr 3) and $3F;
      P:=Cnt;
      BufChar[P]:=$80;
      Inc(P);
      Cnt:=64-1-Cnt;
      if Cnt<8 then
        begin
          FillChar(BufChar[P], Cnt, #0);
          MD5Transform(State, BufLong);
          FillChar(BufChar, 56, #0);
        end
      else fillChar(BufChar[P], Cnt-8, #0);
      BufLong[14] := Count[0];
      BufLong[15] := Count[1];
      MD5Transform(State, BufLong);
      Move(State, Digest, 16);
      result:='';
      for i:=0 to 15 do
        result:=result+char(digest[i]);
    end;
  FillChar(MD5Context, SizeOf(TMD5Ctx), #0)
end;

{==============================================================================}
{MD5}
function MD5(value:string): string;
var
  MD5Context : TMD5Ctx;
begin
  MD5Init(MD5Context);
  MD5Update(MD5Context,value);
  result:=MD5Final(MD5Context);
end;

{==============================================================================}
{HMAC_MD5}
function HMAC_MD5(text,key:string):string;
var
  ipad,opad,s:string;
  n:integer;
  MD5Context : TMD5Ctx;
begin
  if length(key)>64 then
    key:=md5(key);
  ipad:='';
  for n:=1 to 64 do
    ipad:=ipad+#$36;
  opad:='';
  for n:=1 to 64 do
    opad:=opad+#$5c;
  for n:=1 to length(key) do
    begin
      ipad[n]:=char(byte(ipad[n]) xor byte(key[n]));
      opad[n]:=char(byte(opad[n]) xor byte(key[n]));
    end;
  MD5Init(MD5Context);
  MD5Update(MD5Context,ipad);
  MD5Update(MD5Context,text);
  s:=MD5Final(MD5Context);
  MD5Init(MD5Context);
  MD5Update(MD5Context,opad);
  MD5Update(MD5Context,s);
  result:=MD5Final(MD5Context);
end;

{==============================================================================}

begin
  exit;
  asm
    db 'Synapse coding and decoding support library by Lukas Gebauer',0
  end;
end.
