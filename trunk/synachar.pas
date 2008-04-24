{==============================================================================|
| Project : Delphree - Synapse                                   | 004.000.005 |
|==============================================================================|
| Content: Charset conversion support                                          |
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

{$Q-}

unit SynaChar;

interface

type
  TMimeChar = (ISO_8859_1, ISO_8859_2, ISO_8859_3,
    ISO_8859_4, ISO_8859_5, ISO_8859_6, ISO_8859_7,
    ISO_8859_8, ISO_8859_9, ISO_8859_10, ISO_8859_13,
    ISO_8859_14, ISO_8859_15, CP1250, CP1251, CP1252,
    CP1253, CP1254, CP1255, CP1256, CP1257, CP1258,
    KOI8_R, CP895, CP852, UCS_2, UCS_4, UTF_8, UTF_7);

  TMimeSetChar = set of TMimeChar;

//character transcoding tables X to UCS-2
{
//dummy table
$0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
$0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
$0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
$0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
$00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
$00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
$00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
$00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
$00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
$00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
$00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
$00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
$00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
$00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
$00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
$00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF
}

const

{Latin-1
  Danish, Dutch, English, Faeroese, Finnish, French, German, Icelandic,
  Irish, Italian, Norwegian, Portuguese, Spanish and Swedish.
}
  CharISO_8859_1: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF
    );

{Latin-2
  Albanian, Czech, English, German, Hungarian, Polish, Rumanian,
  Serbo-Croatian, Slovak, Slovene and Swedish.
}
  CharISO_8859_2: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $02D8, $0141, $00A4, $013D, $015A, $00A7,
    $00A8, $0160, $015E, $0164, $0179, $00AD, $017D, $017B,
    $00B0, $0105, $02DB, $0142, $00B4, $013E, $015B, $02C7,
    $00B8, $0161, $015F, $0165, $017A, $02DD, $017E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
    $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7,
    $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
    $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7,
    $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
    $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7,
    $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9
    );

{Latin-3
  Afrikaans, Catalan, English, Esperanto, French, Galician,
  German, Italian, Maltese and Turkish.
}
  CharISO_8859_3: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0126, $02D8, $00A3, $00A4, $FFFD, $0124, $00A7,
    $00A8, $0130, $015E, $011E, $0134, $00AD, $FFFD, $017B,
    $00B0, $0127, $00B2, $00B3, $00B4, $00B5, $0125, $00B7,
    $00B8, $0131, $015F, $011F, $0135, $00BD, $FFFD, $017C,
    $00C0, $00C1, $00C2, $FFFD, $00C4, $010A, $0108, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $FFFD, $00D1, $00D2, $00D3, $00D4, $0120, $00D6, $00D7,
    $011C, $00D9, $00DA, $00DB, $00DC, $016C, $015C, $00DF,
    $00E0, $00E1, $00E2, $FFFD, $00E4, $010B, $0109, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $FFFD, $00F1, $00F2, $00F3, $00F4, $0121, $00F6, $00F7,
    $011D, $00F9, $00FA, $00FB, $00FC, $016D, $015D, $02D9
    );

{Latin-4
  Danish, English, Estonian, Finnish, German, Greenlandic,
  Lappish, Latvian, Lithuanian, Norwegian and Swedish.
}
  CharISO_8859_4: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0138, $0156, $00A4, $0128, $013B, $00A7,
    $00A8, $0160, $0112, $0122, $0166, $00AD, $017D, $00AF,
    $00B0, $0105, $02DB, $0157, $00B4, $0129, $013C, $02C7,
    $00B8, $0161, $0113, $0123, $0167, $014A, $017E, $014B,
    $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E,
    $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $012A,
    $0110, $0145, $014C, $0136, $00D4, $00D5, $00D6, $00D7,
    $00D8, $0172, $00DA, $00DB, $00DC, $0168, $016A, $00DF,
    $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F,
    $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $012B,
    $0111, $0146, $014D, $0137, $00F4, $00F5, $00F6, $00F7,
    $00F8, $0173, $00FA, $00FB, $00FC, $0169, $016B, $02D9
    );

{CYRILLIC
  Bulgarian, Bielorussian, English, Macedonian, Russian,
  Serbo-Croatian and Ukrainian.
}
  CharISO_8859_5: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0401, $0402, $0403, $0404, $0405, $0406, $0407,
    $0408, $0409, $040A, $040B, $040C, $00AD, $040E, $040F,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
    $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
    $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
    $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
    $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F,
    $2116, $0451, $0452, $0453, $0454, $0455, $0456, $0457,
    $0458, $0459, $045A, $045B, $045C, $00A7, $045E, $045F
    );

{ARABIC
}
  CharISO_8859_6: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $FFFD, $FFFD, $FFFD, $00A4, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $060C, $00AD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $061B, $FFFD, $FFFD, $FFFD, $061F,
    $FFFD, $0621, $0622, $0623, $0624, $0625, $0626, $0627,
    $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $0637,
    $0638, $0639, $063A, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $0640, $0641, $0642, $0643, $0644, $0645, $0646, $0647,
    $0648, $0649, $064A, $064B, $064C, $064D, $064E, $064F,
    $0650, $0651, $0652, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD
    );

{GREEK
}
  CharISO_8859_7: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $2018, $2019, $00A3, $FFFD, $FFFD, $00A6, $00A7,
    $00A8, $00A9, $FFFD, $00AB, $00AC, $00AD, $FFFD, $2015,
    $00B0, $00B1, $00B2, $00B3, $0384, $0385, $0386, $00B7,
    $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
    $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397,
    $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
    $03A0, $03A1, $FFFD, $03A3, $03A4, $03A5, $03A6, $03A7,
    $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
    $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7,
    $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
    $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7,
    $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $FFFD
    );

{HEBREW
}
  CharISO_8859_8: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $FFFD, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $2017,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7,
    $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7,
    $05E8, $05E9, $05EA, $FFFD, $FFFD, $200E, $200F, $FFFD
    );

{Latin-5
  English, Finnish, French, German, Irish, Italian, Norwegian,
  Portuguese, Spanish, Swedish and Turkish.
}
  CharISO_8859_9: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $02D8, $0141, $00A4, $013D, $015A, $00A7,
    $00A8, $0160, $015E, $0164, $0179, $00AD, $017D, $017B,
    $00B0, $0105, $02DB, $0142, $00B4, $013E, $015B, $02C7,
    $00B8, $0161, $015F, $0165, $017A, $02DD, $017E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
    $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF
    );

{Latin-6
  Danish, English, Estonian, Faeroese, Finnish, German, Greenlandic,
  Icelandic, Lappish, Latvian, Lithuanian, Norwegian and Swedish.
}
  CharISO_8859_10: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $0104, $0112, $0122, $012A, $0128, $0136, $00A7,
    $013B, $0110, $0160, $0166, $017D, $00AD, $016A, $014A,
    $00B0, $0105, $0113, $0123, $012B, $0129, $0137, $00B7,
    $013C, $0111, $0161, $0167, $017E, $2015, $016B, $014B,
    $0100, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $012E,
    $010C, $00C9, $0118, $00CB, $0116, $00CD, $00CE, $00CF,
    $00D0, $0145, $014C, $00D3, $00D4, $00D5, $00D6, $0168,
    $00D8, $0172, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $0101, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $012F,
    $010D, $00E9, $0119, $00EB, $0117, $00ED, $00EE, $00EF,
    $00F0, $0146, $014D, $00F3, $00F4, $00F5, $00F6, $0169,
    $00F8, $0173, $00FA, $00FB, $00FC, $00FD, $00FE, $0138
    );

  CharISO_8859_13: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $201D, $00A2, $00A3, $00A4, $201E, $00A6, $00A7,
    $00D8, $00A9, $0156, $00AB, $00AC, $00AD, $00AE, $00C6,
    $00B0, $00B1, $00B2, $00B3, $201C, $00B5, $00B6, $00B7,
    $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
    $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112,
    $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
    $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7,
    $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
    $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113,
    $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
    $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7,
    $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $2019
    );

  CharISO_8859_14: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $1E02, $1E03, $00A3, $010A, $010B, $1E0A, $00A7,
    $1E80, $00A9, $1E82, $1E0B, $1EF2, $00AD, $00AE, $0178,
    $1E1E, $1E1F, $0120, $0121, $1E40, $1E41, $00B6, $1E56,
    $1E81, $1E57, $1E83, $1E60, $1EF3, $1E84, $1E85, $1E61,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $0174, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $1E6A,
    $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $0176, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $0175, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $1E6B,
    $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $0177, $00FF
    );

  CharISO_8859_15: array[128..255] of Word =
  (
    $0080, $0081, $0082, $0083, $0084, $0085, $0086, $0087,
    $0088, $0089, $008A, $008B, $008C, $008D, $008E, $008F,
    $0090, $0091, $0092, $0093, $0094, $0095, $0096, $0097,
    $0098, $0099, $009A, $009B, $009C, $009D, $009E, $009F,
    $00A0, $00A1, $00A2, $00A3, $20AC, $00A5, $0160, $00A7,
    $0161, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $017D, $00B5, $00B6, $00B7,
    $017E, $00B9, $00BA, $00BB, $0152, $0153, $0178, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF
    );

{Eastern European
}
  CharCP_1250: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $FFFD, $201E, $2026, $2020, $2021,
    $FFFD, $2030, $0160, $2039, $015A, $0164, $017D, $0179,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $FFFD, $2122, $0161, $203A, $015B, $0165, $017E, $017A,
    $00A0, $02C7, $02D8, $0141, $00A4, $0104, $00A6, $00A7,
    $00A8, $00A9, $015E, $00AB, $00AC, $00AD, $00AE, $017B,
    $00B0, $00B1, $02DB, $0142, $00B4, $00B5, $00B6, $00B7,
    $00B8, $0105, $015F, $00BB, $013D, $02DD, $013E, $017C,
    $0154, $00C1, $00C2, $0102, $00C4, $0139, $0106, $00C7,
    $010C, $00C9, $0118, $00CB, $011A, $00CD, $00CE, $010E,
    $0110, $0143, $0147, $00D3, $00D4, $0150, $00D6, $00D7,
    $0158, $016E, $00DA, $0170, $00DC, $00DD, $0162, $00DF,
    $0155, $00E1, $00E2, $0103, $00E4, $013A, $0107, $00E7,
    $010D, $00E9, $0119, $00EB, $011B, $00ED, $00EE, $010F,
    $0111, $0144, $0148, $00F3, $00F4, $0151, $00F6, $00F7,
    $0159, $016F, $00FA, $0171, $00FC, $00FD, $0163, $02D9
    );

{Cyrillic
}
  CharCP_1251: array[128..255] of Word =
  (
    $0402, $0403, $201A, $0453, $201E, $2026, $2020, $2021,
    $20AC, $2030, $0409, $2039, $040A, $040C, $040B, $040F,
    $0452, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $FFFD, $2122, $0459, $203A, $045A, $045C, $045B, $045F,
    $00A0, $040E, $045E, $0408, $00A4, $0490, $00A6, $00A7,
    $0401, $00A9, $0404, $00AB, $00AC, $00AD, $00AE, $0407,
    $00B0, $00B1, $0406, $0456, $0491, $00B5, $00B6, $00B7,
    $0451, $2116, $0454, $00BB, $0458, $0405, $0455, $0457,
    $0410, $0411, $0412, $0413, $0414, $0415, $0416, $0417,
    $0418, $0419, $041A, $041B, $041C, $041D, $041E, $041F,
    $0420, $0421, $0422, $0423, $0424, $0425, $0426, $0427,
    $0428, $0429, $042A, $042B, $042C, $042D, $042E, $042F,
    $0430, $0431, $0432, $0433, $0434, $0435, $0436, $0437,
    $0438, $0439, $043A, $043B, $043C, $043D, $043E, $043F,
    $0440, $0441, $0442, $0443, $0444, $0445, $0446, $0447,
    $0448, $0449, $044A, $044B, $044C, $044D, $044E, $044F
    );

{Latin-1 (US, Western Europe)
}
  CharCP_1252: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0160, $2039, $0152, $FFFD, $017D, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $0161, $203A, $0153, $FFFD, $017E, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $00D0, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $00DD, $00DE, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $00F0, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $00FD, $00FE, $00FF
    );

{Greek
}
  CharCP_1253: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $FFFD, $2030, $FFFD, $2039, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $FFFD, $2122, $FFFD, $203A, $FFFD, $FFFD, $FFFD, $FFFD,
    $00A0, $0385, $0386, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $FFFD, $00AB, $00AC, $00AD, $00AE, $2015,
    $00B0, $00B1, $00B2, $00B3, $0384, $00B5, $00B6, $00B7,
    $0388, $0389, $038A, $00BB, $038C, $00BD, $038E, $038F,
    $0390, $0391, $0392, $0393, $0394, $0395, $0396, $0397,
    $0398, $0399, $039A, $039B, $039C, $039D, $039E, $039F,
    $03A0, $03A1, $FFFD, $03A3, $03A4, $03A5, $03A6, $03A7,
    $03A8, $03A9, $03AA, $03AB, $03AC, $03AD, $03AE, $03AF,
    $03B0, $03B1, $03B2, $03B3, $03B4, $03B5, $03B6, $03B7,
    $03B8, $03B9, $03BA, $03BB, $03BC, $03BD, $03BE, $03BF,
    $03C0, $03C1, $03C2, $03C3, $03C4, $03C5, $03C6, $03C7,
    $03C8, $03C9, $03CA, $03CB, $03CC, $03CD, $03CE, $FFFD
    );

{Turkish
}
  CharCP_1254: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0160, $2039, $0152, $FFFD, $FFFD, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $0161, $203A, $0153, $FFFD, $FFFD, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $00C3, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $00CC, $00CD, $00CE, $00CF,
    $011E, $00D1, $00D2, $00D3, $00D4, $00D5, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $0130, $015E, $00DF,
    $00E0, $00E1, $00E2, $00E3, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $00EC, $00ED, $00EE, $00EF,
    $011F, $00F1, $00F2, $00F3, $00F4, $00F5, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $0131, $015F, $00FF
    );

{Hebrew
}
  CharCP_1255: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $FFFD, $2039, $FFFD, $FFFD, $FFFD, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $FFFD, $203A, $FFFD, $FFFD, $FFFD, $FFFD,
    $00A0, $00A1, $00A2, $00A3, $20AA, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00D7, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00F7, $00BB, $00BC, $00BD, $00BE, $00BF,
    $05B0, $05B1, $05B2, $05B3, $05B4, $05B5, $05B6, $05B7,
    $05B8, $05B9, $FFFD, $05BB, $05BC, $05BD, $05BE, $05BF,
    $05C0, $05C1, $05C2, $05C3, $05F0, $05F1, $05F2, $05F3,
    $05F4, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD, $FFFD,
    $05D0, $05D1, $05D2, $05D3, $05D4, $05D5, $05D6, $05D7,
    $05D8, $05D9, $05DA, $05DB, $05DC, $05DD, $05DE, $05DF,
    $05E0, $05E1, $05E2, $05E3, $05E4, $05E5, $05E6, $05E7,
    $05E8, $05E9, $05EA, $FFFD, $FFFD, $200E, $200F, $FFFD
    );

{Arabic
}
  CharCP_1256: array[128..255] of Word =
  (
    $20AC, $067E, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $0679, $2039, $0152, $0686, $0698, $0688,
    $06AF, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $06A9, $2122, $0691, $203A, $0153, $200C, $200D, $06BA,
    $00A0, $060C, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $06BE, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $061B, $00BB, $00BC, $00BD, $00BE, $061F,
    $06C1, $0621, $0622, $0623, $0624, $0625, $0626, $0627,
    $0628, $0629, $062A, $062B, $062C, $062D, $062E, $062F,
    $0630, $0631, $0632, $0633, $0634, $0635, $0636, $00D7,
    $0637, $0638, $0639, $063A, $0640, $0641, $0642, $0643,
    $00E0, $0644, $00E2, $0645, $0646, $0647, $0648, $00E7,
    $00E8, $00E9, $00EA, $00EB, $0649, $064A, $00EE, $00EF,
    $064B, $064C, $064D, $064E, $00F4, $064F, $0650, $00F7,
    $0651, $00F9, $0652, $00FB, $00FC, $200E, $200F, $06D2
    );

{Baltic
}
  CharCP_1257: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $FFFD, $201E, $2026, $2020, $2021,
    $FFFD, $2030, $FFFD, $2039, $FFFD, $00A8, $02C7, $00B8,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $FFFD, $2122, $FFFD, $203A, $FFFD, $00AF, $02DB, $FFFD,
    $00A0, $FFFD, $00A2, $00A3, $00A4, $FFFD, $00A6, $00A7,
    $00D8, $00A9, $0156, $00AB, $00AC, $00AD, $00AE, $00C6,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00F8, $00B9, $0157, $00BB, $00BC, $00BD, $00BE, $00E6,
    $0104, $012E, $0100, $0106, $00C4, $00C5, $0118, $0112,
    $010C, $00C9, $0179, $0116, $0122, $0136, $012A, $013B,
    $0160, $0143, $0145, $00D3, $014C, $00D5, $00D6, $00D7,
    $0172, $0141, $015A, $016A, $00DC, $017B, $017D, $00DF,
    $0105, $012F, $0101, $0107, $00E4, $00E5, $0119, $0113,
    $010D, $00E9, $017A, $0117, $0123, $0137, $012B, $013C,
    $0161, $0144, $0146, $00F3, $014D, $00F5, $00F6, $00F7,
    $0173, $0142, $015B, $016B, $00FC, $017C, $017E, $02D9
    );

{??
}
  CharCP_1258: array[128..255] of Word =
  (
    $20AC, $FFFD, $201A, $0192, $201E, $2026, $2020, $2021,
    $02C6, $2030, $FFFD, $2039, $0152, $FFFD, $FFFD, $FFFD,
    $FFFD, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    $02DC, $2122, $FFFD, $203A, $0153, $FFFD, $FFFD, $0178,
    $00A0, $00A1, $00A2, $00A3, $00A4, $00A5, $00A6, $00A7,
    $00A8, $00A9, $00AA, $00AB, $00AC, $00AD, $00AE, $00AF,
    $00B0, $00B1, $00B2, $00B3, $00B4, $00B5, $00B6, $00B7,
    $00B8, $00B9, $00BA, $00BB, $00BC, $00BD, $00BE, $00BF,
    $00C0, $00C1, $00C2, $0102, $00C4, $00C5, $00C6, $00C7,
    $00C8, $00C9, $00CA, $00CB, $0300, $00CD, $00CE, $00CF,
    $0110, $00D1, $0309, $00D3, $00D4, $01A0, $00D6, $00D7,
    $00D8, $00D9, $00DA, $00DB, $00DC, $01AF, $0303, $00DF,
    $00E0, $00E1, $00E2, $0103, $00E4, $00E5, $00E6, $00E7,
    $00E8, $00E9, $00EA, $00EB, $0301, $00ED, $00EE, $00EF,
    $0111, $00F1, $0323, $00F3, $00F4, $01A1, $00F6, $00F7,
    $00F8, $00F9, $00FA, $00FB, $00FC, $01B0, $20AB, $00FF
    );

{Cyrillic
}
  CharKOI8_R: array[128..255] of Word =
  (
    $2500, $2502, $250C, $2510, $2514, $2518, $251C, $2524,
    $252C, $2534, $253C, $2580, $2584, $2588, $258C, $2590,
    $2591, $2592, $2593, $2320, $25A0, $2219, $221A, $2248,
    $2264, $2265, $00A0, $2321, $00B0, $00B2, $00B7, $00F7,
    $2550, $2551, $2552, $0451, $2553, $2554, $2555, $2556,
    $2557, $2558, $2559, $255A, $255B, $255C, $255D, $255E,
    $255F, $2560, $2561, $0401, $2562, $2563, $2564, $2565,
    $2566, $2567, $2568, $2569, $256A, $256B, $256C, $00A9,
    $044E, $0430, $0431, $0446, $0434, $0435, $0444, $0433,
    $0445, $0438, $0439, $043A, $043B, $043C, $043D, $043E,
    $043F, $044F, $0440, $0441, $0442, $0443, $0436, $0432,
    $044C, $044B, $0437, $0448, $044D, $0449, $0447, $044A,
    $042E, $0410, $0411, $0426, $0414, $0415, $0424, $0413,
    $0425, $0418, $0419, $041A, $041B, $041C, $041D, $041E,
    $041F, $042F, $0420, $0421, $0422, $0423, $0416, $0412,
    $042C, $042B, $0417, $0428, $042D, $0429, $0427, $042A
    );

{Czech (Kamenicky)
}
  CharCP_895: array[128..255] of Word =
  (
    $010C, $00FC, $00E9, $010F, $00E4, $010E, $0164, $010D,
    $011B, $011A, $0139, $00CD, $013E, $013A, $00C4, $00C1,
    $00C9, $017E, $017D, $00F4, $00F6, $00D3, $016F, $00DA,
    $00FD, $00D6, $00DC, $0160, $013D, $00DD, $0158, $0165,
    $00E1, $00ED, $00F3, $00FA, $0148, $0147, $016E, $00D4,
    $0161, $0159, $0155, $0154, $00BC, $00A7, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $2561, $2562, $2556,
    $2555, $2563, $2551, $2557, $255D, $255C, $255B, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $255E, $255F,
    $255A, $2554, $2569, $2566, $2560, $2550, $256C, $2567,
    $2568, $2564, $2565, $2559, $2558, $2552, $2553, $256B,
    $256A, $2518, $250C, $2588, $2584, $258C, $2590, $2580,
    $03B1, $03B2, $0393, $03C0, $03A3, $03C3, $03BC, $03C4,
    $03A6, $0398, $03A9, $03B4, $221E, $2205, $03B5, $2229,
    $2261, $00B1, $2265, $2264, $2320, $2321, $00F7, $2248,
    $2218, $00B7, $2219, $221A, $207F, $00B2, $25A0, $00A0
    );

{Eastern European
}
  CharCP_852: array[128..255] of Word =
  (
    $00C7, $00FC, $00E9, $00E2, $00E4, $016F, $0107, $00E7,
    $0142, $00EB, $0150, $0151, $00EE, $0179, $00C4, $0106,
    $00C9, $0139, $013A, $00F4, $00F6, $013D, $013E, $015A,
    $015B, $00D6, $00DC, $0164, $0165, $0141, $00D7, $010D,
    $00E1, $00ED, $00F3, $00FA, $0104, $0105, $017D, $017E,
    $0118, $0119, $00AC, $017A, $010C, $015F, $00AB, $00BB,
    $2591, $2592, $2593, $2502, $2524, $00C1, $00C2, $011A,
    $015E, $2563, $2551, $2557, $255D, $017B, $017C, $2510,
    $2514, $2534, $252C, $251C, $2500, $253C, $0102, $0103,
    $255A, $2554, $2569, $2566, $2560, $2550, $256C, $00A4,
    $0111, $0110, $010E, $00CB, $010F, $0147, $00CD, $00CE,
    $011B, $2518, $250C, $2588, $2584, $0162, $016E, $2580,
    $00D3, $00DF, $00D4, $0143, $0144, $0148, $0160, $0161,
    $0154, $00DA, $0155, $0170, $00FD, $00DD, $0163, $00B4,
    $00AD, $02DD, $02DB, $02C7, $02D8, $00A7, $00F7, $00B8,
    $00B0, $00A8, $02D9, $0171, $0158, $0159, $25A0, $00A0
    );

  // nothing fr replace
  Replace_None: array[0..0] of Word =
    (0);

  //remove diakritics from Czech
  Replace_Czech: array[0..55] of Word =
    (
      $00E1, $0061,
      $010D, $0063,
      $010F, $0064,
      $010E, $0044,
      $00E9, $0065,
      $011B, $0065,
      $00ED, $0069,
      $00F3, $006F,
      $0159, $0072,
      $0161, $0073,
      $0165, $0074,
      $00FA, $0075,
      $016F, $0075,
      $00FD, $0079,
      $017E, $007A,
      $00C1, $0041,
      $010C, $0043,
      $00C9, $0045,
      $011A, $0045,
      $00CD, $0049,
      $00D3, $004F,
      $0158, $0052,
      $0160, $0053,
      $0164, $0053,
      $00DA, $0055,
      $016E, $0055,
      $00DD, $0059,
      $017D, $005A
    );

{==============================================================================}
function UTF8toUCS4(const Value: string): string;
function UCS4toUTF8(const Value: string): string;
function UTF7toUCS2(const Value: string): string;
function UCS2toUTF7(const Value: string): string;
function CharsetConversion(Value: string; CharFrom: TMimeChar;
  CharTo: TMimeChar): string;
function CharsetConversionEx(Value: string; CharFrom: TMimeChar;
  CharTo: TMimeChar; const TransformTable: array of Word): string;
function GetCurCP: TMimeChar;
function GetCPFromID(Value: string): TMimeChar;
function GetIDFromCP(Value: TMimeChar): string;
function NeedCharsetConversion(const Value: string): Boolean;
function IdealCharsetCoding(const Value: string; CharFrom: TMimeChar;
  CharTo: TMimeSetChar): TMimeChar;

implementation

uses
{$IFDEF LINUX}
  Libc,
{$ELSE}
  Windows,
{$ENDIF}
  SysUtils,
  SynaUtil, SynaCode;

const
  NotFoundChar = '_';

var
  SetTwo: set of TMimeChar = [UCS_2, UTF_7];
  SetFour: set of TMimeChar = [UCS_4, UTF_8];

{==============================================================================}
function ReplaceUnicode(Value: Word; const TransformTable: array of Word): Word;
var
  n: integer;
begin
  if High(TransformTable) <> 0 then
    for n := 0 to High(TransformTable) do
      if not odd(n) then
        if TransformTable[n] = Value then
          begin
            Value := TransformTable[n+1];
            break;
          end;
  Result := Value;
end;

{==============================================================================}
procedure CopyArray(const SourceTable: array of Word;
  var TargetTable: array of Word);
var
  n: Integer;
begin
  for n := 0 to 127 do
    TargetTable[n] := SourceTable[n];
end;

{==============================================================================}
procedure GetArray(CharSet: TMimeChar; var Result: array of Word);
begin
  case CharSet of
    ISO_8859_1:
      CopyArray(CharISO_8859_1, Result);
    ISO_8859_2:
      CopyArray(CharISO_8859_2, Result);
    ISO_8859_3:
      CopyArray(CharISO_8859_3, Result);
    ISO_8859_4:
      CopyArray(CharISO_8859_4, Result);
    ISO_8859_5:
      CopyArray(CharISO_8859_5, Result);
    ISO_8859_6:
      CopyArray(CharISO_8859_6, Result);
    ISO_8859_7:
      CopyArray(CharISO_8859_7, Result);
    ISO_8859_8:
      CopyArray(CharISO_8859_8, Result);
    ISO_8859_9:
      CopyArray(CharISO_8859_9, Result);
    ISO_8859_10:
      CopyArray(CharISO_8859_10, Result);
    ISO_8859_13:
      CopyArray(CharISO_8859_13, Result);
    ISO_8859_14:
      CopyArray(CharISO_8859_14, Result);
    ISO_8859_15:
      CopyArray(CharISO_8859_15, Result);
    CP1250:
      CopyArray(CharCP_1250, Result);
    CP1251:
      CopyArray(CharCP_1251, Result);
    CP1252:
      CopyArray(CharCP_1252, Result);
    CP1253:
      CopyArray(CharCP_1253, Result);
    CP1254:
      CopyArray(CharCP_1254, Result);
    CP1255:
      CopyArray(CharCP_1255, Result);
    CP1256:
      CopyArray(CharCP_1256, Result);
    CP1257:
      CopyArray(CharCP_1257, Result);
    CP1258:
      CopyArray(CharCP_1258, Result);
    KOI8_R:
      CopyArray(CharKOI8_R, Result);
    CP895:
      CopyArray(CharCP_895, Result);
    CP852:
      CopyArray(CharCP_852, Result);
  end;
end;

{==============================================================================}
procedure ReadMulti(const Value: string; var Index: Integer; mb: Byte;
  var b1, b2, b3, b4: Byte);
Begin
  b1 := 0;
  b2 := 0;
  b3 := 0;
  b4 := 0;
  if Index < 0 then
    Index := 1;
  if mb > 4 then
    mb := 1;
  if (Index + mb - 1) <= Length(Value) then
  begin
    Case mb Of
      1:
        b1 := Ord(Value[Index]);
      2:
        Begin
          b1 := Ord(Value[Index]);
          b2 := Ord(Value[Index + 1]);
        End;
      3:
        Begin
          b1 := Ord(Value[Index]);
          b2 := Ord(Value[Index + 1]);
          b3 := Ord(Value[Index + 2]);
        End;
      4:
        Begin
          b1 := Ord(Value[Index]);
          b2 := Ord(Value[Index + 1]);
          b3 := Ord(Value[Index + 2]);
          b4 := Ord(Value[Index + 3]);
        End;
    end;
    Inc(Index, mb);
  End;
End;

{==============================================================================}
function WriteMulti(b1, b2, b3, b4: Byte; mb: Byte): string;
begin
  if mb > 4 then
    mb := 1;
  SetLength(Result, mb);
  case mb Of
    1:
      Result[1] := Char(b1);
    2:
      begin
        Result[1] := Char(b1);
        Result[2] := Char(b2);
      end;
    3:
      begin
        Result[1] := Char(b1);
        Result[2] := Char(b2);
        Result[3] := Char(b3);
      end;
    4:
      begin
        Result[1] := Char(b1);
        Result[2] := Char(b2);
        Result[3] := Char(b3);
        Result[4] := Char(b4);
      end;
  end;
end;

{==============================================================================}
function UTF8toUCS4(const Value: string): string;
var
  n, x, ul, m: Integer;
  s: string;
  w1, w2: Word;
begin
  Result := '';
  n := 1;
  while Length(Value) >= n do
  begin
    x := Ord(Value[n]);
    Inc(n);
    if x < 128 then
      Result := Result + WriteMulti(x, 0, 0, 0, 4)
    else
    begin
      m := 0;
      if (x and $E0) = $C0 then
        m := $1F;
      if (x and $F0) = $E0 then
        m := $0F;
      if (x and $F8) = $F0 then
        m := $07;
      if (x and $FC) = $F8 then
        m := $03;
      if (x and $FE) = $FC then
        m := $01;
      ul := x and m;
      s := IntToBin(ul, 0);
      while Length(Value) >= n do
      begin
        x := Ord(Value[n]);
        Inc(n);
        if (x and $C0) = $80 then
          s := s + IntToBin(x and $3F, 6)
        else
        begin
          Dec(n);
          Break;
        end;
      end;
      ul := BinToInt(s);
      w1 := ul div 65536;
      w2 := ul mod 65536;
      Result := Result + WriteMulti(Lo(w2), Hi(w2), Lo(w1), Hi(w1), 4);
    end;
  end;
end;

{==============================================================================}
function UCS4toUTF8(const Value: string): string;
var
  s, l, k: string;
  b1, b2, b3, b4: Byte;
  n, m, x, y: Integer;
  b: Byte;
begin
  Result := '';
  n := 1;
  while Length(Value) >= n do
  begin
    ReadMulti(Value, n, 4, b1, b2, b3, b4);
    if (b2 = 0) and (b3 = 0) and (b4 = 0) and (b1 < 128) then
      Result := Result + Char(b1)
    else
    begin
      x := (b1 + 256 * b2) + (b3 + 256 * b4) * 65536;
      l := IntToBin(x, 0);
      y := Length(l) div 6;
      s := '';
      for m := 1 to y do
      begin
        k := Copy(l, Length(l) - 5, 6);
        l := Copy(l, 1, Length(l) - 6);
        b := BinToInt(k) or $80;
        s := Char(b) + s;
      end;
      b := BinToInt(l);
      case y of
        5:
          b := b or $FC;
        4:
          b := b or $F8;
        3:
          b := b or $F0;
        2:
          b := b or $E0;
        1:
          b := b or $C0;
      end;
      s := Char(b) + s;
      Result := Result + s;
    end;
  end;
end;

{==============================================================================}
function UTF7toUCS2(const Value: string): string;
var
  n, i: Integer;
  c: Char;
  s, t: string;
begin
  Result := '';
  n := 1;
  while Length(Value) >= n do
  begin
    c := Value[n];
    Inc(n);
    if c <> '+' then
      Result := Result + WriteMulti(Ord(c), 0, 0, 0, 2)
    else
    begin
      s := '';
      while Length(Value) >= n do
      begin
        c := Value[n];
        Inc(n);
        if c = '-' then
          Break;
        if (c = '=') or (Pos(c, TableBase64) < 1) then
        begin
          Dec(n);
          Break;
        end;
        s := s + c;
      end;
      if s = '' then
        s := WriteMulti(Ord('+'), 0, 0, 0, 2)
      else
      begin
        t := DecodeBase64(s);
        if not odd(length(t)) then
          s := t
        else
        begin //ill-formed sequence
          t := s;
          s := WriteMulti(Ord('+'), 0, 0, 0, 2);
          for i := 1 to length(t) do
            s := s + WriteMulti(Ord(t[i]), 0, 0, 0, 2);
        end;
      end;
      Result := Result + s;
    end;
  end;
end;

{==============================================================================}
function UCS2toUTF7(const Value: string): string;
var
  s: string;
  b1, b2, b3, b4: Byte;
  n, m: Integer;
begin
  Result := '';
  n := 1;
  while Length(Value) >= n do
  begin
    ReadMulti(Value, n, 2, b1, b2, b3, b4);
    if (b2 = 0) and (b1 < 128) then
      if Char(b1) = '+' then
        Result := Result + '+-'
      else
        Result := Result + Char(b1)
    else
    begin
      s := Char(b2) + Char(b1);
      while Length(Value) >= n do
      begin
        ReadMulti(Value, n, 2, b1, b2, b3, b4);
        if (b2 = 0) and (b1 < 128) then
        begin
          Dec(n, 2);
          Break;
        end;
        s := s + Char(b2) + Char(b1);
      end;
      s := EncodeBase64(s);
      m := Pos('=', s);
      if m > 0 then
        s := Copy(s, 1, m - 1);
      Result := Result + '+' + s + '-';
    end;
  end;
end;

{==============================================================================}
function CharsetConversion(Value: string; CharFrom: TMimeChar;
  CharTo: TMimeChar): string;
begin
  Result := CharsetConversionEx(Value, CharFrom, CharTo, Replace_None);
end;

{==============================================================================}
function CharsetConversionEx(Value: string; CharFrom: TMimeChar;
  CharTo: TMimeChar; const TransformTable: array of Word): string;
var
  uni: Word;
  n, m: Integer;
  b: Byte;
  b1, b2, b3, b4: Byte;
  SourceTable, TargetTable: array[128..255] of Word;
  mbf, mbt: Byte;
begin
  GetArray(CharFrom, SourceTable);
  GetArray(CharTo, TargetTable);
  mbf := 1;
  if CharFrom in SetTwo then
    mbf := 2;
  if CharFrom in SetFour then
    mbf := 4;
  mbt := 1;
  if CharTo in SetTwo then
    mbt := 2;
  if CharTo in SetFour then
    mbt := 4;

  if CharFrom = UTF_8 then
    Value := UTF8toUCS4(Value);
  if CharFrom = UTF_7 then
    Value := UTF7toUCS2(Value);
  Result := '';

  n := 1;
  while Length(Value) >= n do
  begin
    ReadMulti(Value, n, mbf, b1, b2, b3, b4);
    if mbf = 1 then
      if b1 > 127 then
      begin
        uni := SourceTable[b1];
        uni := ReplaceUnicode(uni, TransformTable);
        b1 := Lo(uni);
        b2 := Hi(uni);
      end;
    // b1..b4 - Unicode Char
    uni := b2 * 256 + b1;
    if (b3 <> 0) or (b4 <> 0) then
    begin
      b1 := Ord(NotFoundChar);
      b2 := 0;
      b3 := 0;
      b4 := 0;
    end
    else
      if mbt = 1 then
        if uni > 127 then
        begin
          b := Ord(NotFoundChar);
          for m := 128 to 255 do
            if TargetTable[m] = uni then
            begin
              b := m;
              Break;
            end;
          b1 := b;
          b2 := 0;
        end
        else
          b1 := Lo(uni);
    Result := Result + WriteMulti(b1, b2, b3, b4, mbt)
  end;

  if CharTo = UTF_7 then
    Result := UCS2toUTF7(Result);
  if CharTo = UTF_8 then
    Result := UCS4toUTF8(Result);

end;

{==============================================================================}
{$IFDEF LINUX}

function GetCurCP: TMimeChar;
begin
  Result := GetCPFromID(nl_langinfo(_NL_CTYPE_CODESET_NAME));
end;

{$ELSE}

function GetCurCP: TMimeChar;
begin
  case GetACP of
    1250:
      Result := CP1250;
    1251:
      Result := CP1251;
    1253:
      Result := CP1253;
    1254:
      Result := CP1254;
    1255:
      Result := CP1255;
    1256:
      Result := CP1256;
    1257:
      Result := CP1257;
    1258:
      Result := CP1258;
  else
    Result := CP1252;
  end;
end;

{$ENDIF}

{==============================================================================}
function GetCPFromID(Value: string): TMimeChar;
begin
  Value := UpperCase(Value);
  Result := ISO_8859_1;
  if Pos('ISO-8859-10', Value) = 1 then
    Result := ISO_8859_10
  else
  if Pos('ISO-8859-13', Value) = 1 then
    Result := ISO_8859_13
  else
  if Pos('ISO-8859-14', Value) = 1 then
    Result := ISO_8859_14
  else
  if Pos('ISO-8859-15', Value) = 1 then
    Result := ISO_8859_15
  else
  if Pos('ISO-8859-2', Value) = 1 then
    Result := ISO_8859_2
  else
  if Pos('ISO-8859-3', Value) = 1 then
    Result := ISO_8859_3
  else
  if Pos('ISO-8859-4', Value) = 1 then
    Result := ISO_8859_4
  else
  if Pos('ISO-8859-5', Value) = 1 then
    Result := ISO_8859_5
  else
  if Pos('ISO-8859-6', Value) = 1 then
    Result := ISO_8859_6
  else
  if Pos('ISO-8859-7', Value) = 1 then
    Result := ISO_8859_7
  else
  if Pos('ISO-8859-8', Value) = 1 then
    Result := ISO_8859_8
  else
  if Pos('ISO-8859-9', Value) = 1 then
    Result := ISO_8859_9
  else
  if (Pos('WINDOWS-1250', Value) = 1) or (Pos('X-CP1250', Value) = 1) then
    Result := CP1250
  else
  if (Pos('WINDOWS-1251', Value) = 1) or (Pos('X-CP1251', Value) = 1) then
    Result := CP1251
  else
  if (Pos('WINDOWS-1252', Value) = 1) or (Pos('X-CP1252', Value) = 1) then
    Result := CP1252
  else
  if (Pos('WINDOWS-1253', Value) = 1) or (Pos('X-CP1253', Value) = 1) then
    Result := CP1253
  else
  if (Pos('WINDOWS-1254', Value) = 1) or (Pos('X-CP1254', Value) = 1) then
    Result := CP1254
  else
  if (Pos('WINDOWS-1255', Value) = 1) or (Pos('X-CP1255', Value) = 1) then
    Result := CP1255
  else
  if (Pos('WINDOWS-1256', Value) = 1) or (Pos('X-CP1256', Value) = 1) then
    Result := CP1256
  else
  if (Pos('WINDOWS-1257', Value) = 1) or (Pos('X-CP1257', Value) = 1) then
    Result := CP1257
  else
  if (Pos('WINDOWS-1258', Value) = 1) or (Pos('X-CP1258', Value) = 1) then
    Result := CP1258
  else
  if Pos('KOI8-R', Value) = 1 then
    Result := KOI8_R
  else
  if (Pos('KAMENICKY', Value) > 0) or (Pos('895', Value) > 0) then
    Result := CP895
  else
  if (Pos('LATIN-2', Value) > 0) or (Pos('852', Value) > 0) then
    Result := CP852
  else
  if Pos('UTF-7', Value) = 1 then
    Result := UTF_7
  else
  if Pos('UTF-8', Value) > 0 then
    Result := UTF_8
  else
  if Pos('UCS-4', Value) > 0 then
    Result := UCS_4
  else
  if Pos('UCS-2', Value) > 0 then
    Result := UCS_2
  else
  if Pos('UNICODE', Value) = 1 then
    Result := UCS_2
end;

{==============================================================================}
function GetIDFromCP(Value: TMimeChar): string;
begin
  case Value of
    ISO_8859_2:
      Result := 'ISO-8859-2';
    ISO_8859_3:
      Result := 'ISO-8859-3';
    ISO_8859_4:
      Result := 'ISO-8859-4';
    ISO_8859_5:
      Result := 'ISO-8859-5';
    ISO_8859_6:
      Result := 'ISO-8859-6';
    ISO_8859_7:
      Result := 'ISO-8859-7';
    ISO_8859_8:
      Result := 'ISO-8859-8';
    ISO_8859_9:
      Result := 'ISO-8859-9';
    ISO_8859_10:
      Result := 'ISO-8859-10';
    ISO_8859_13:
      Result := 'ISO-8859-13';
    ISO_8859_14:
      Result := 'ISO-8859-14';
    ISO_8859_15:
      Result := 'ISO-8859-15';
    CP1250:
      Result := 'WINDOWS-1250';
    CP1251:
      Result := 'WINDOWS-1251';
    CP1252:
      Result := 'WINDOWS-1252';
    CP1253:
      Result := 'WINDOWS-1253';
    CP1254:
      Result := 'WINDOWS-1254';
    CP1255:
      Result := 'WINDOWS-1255';
    CP1256:
      Result := 'WINDOWS-1256';
    CP1257:
      Result := 'WINDOWS-1257';
    CP1258:
      Result := 'WINDOWS-1258';
    KOI8_R:
      Result := 'KOI8-R';
    CP895:
      Result := 'CP-895';
    CP852:
      Result := 'CP-852';
    UCS_2:
      Result := 'Unicode-1-1-UCS-2';
    UCS_4:
      Result := 'Unicode-1-1-UCS-4';
    UTF_8:
      Result := 'UTF-8';
    UTF_7:
      Result := 'UTF-7';
  else
    Result := 'ISO-8859-1';
  end;
end;

{==============================================================================}
function NeedCharsetConversion(const Value: string): Boolean;
var
  n: Integer;
begin
  Result := False;
  for n := 1 to Length(Value) do
    if Ord(Value[n]) > 127 then
    begin
      Result := True;
      Break;
    end;
end;

{==============================================================================}
function IdealCharsetCoding(const Value: string; CharFrom: TMimeChar;
  CharTo: TMimeSetChar): TMimeChar;
var
  n, m: Integer;
  min, x: Integer;
  s, t: string;
begin
  Result := ISO_8859_1;
  s := '';
  for n := 1 to Length(Value) do
    if Ord(Value[n]) > 127 then
      s := s + Value[n];
  min := 128;
  for n := Ord(Low(TMimeChar)) to Ord(High(TMimeChar)) do
    if TMimeChar(n) in CharTo then
    begin
      t := CharsetConversion(s, CharFrom, TMimeChar(n));
      x := 0;
      for m := 1 to Length(t) do
        if t[m] = NotFoundChar then
          Inc(x);
      if x < min then
      begin
        min := x;
        Result := TMimeChar(n);
        if x = 0 then
          Break;
      end;
    end;
end;

end.
