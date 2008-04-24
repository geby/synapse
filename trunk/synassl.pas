{==============================================================================|
| Project : Ararat Synapse                                       | 002.001.002 |
|==============================================================================|
| Content: SSL support                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2002-2003.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}
{
Special thanks to Gregor Ibic <gregor.ibic@intelicom.si>
 (Intelicom d.o.o., http://www.intelicom.si)
 for good inspiration about begin with SSL programming.
}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}
{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
  (*$HPPEMIT 'namespace Synassl { using System::Shortint; }' *)
{$ENDIF}

unit synassl;

interface

uses
{$IFDEF LINUX}
  {$IFDEF FPC}
  synafpc,
  {$ENDIF}
  Libc, SysUtils;
{$ELSE}
  Windows;
{$ENDIF}

var
{$IFDEF LINUX}
  DLLSSLName: string = 'libssl.so';
  DLLUtilName: string = 'libcrypto.so';
{$ELSE}
  DLLSSLName: string = 'ssleay32.dll';
  DLLSSLName2: string = 'libssl32.dll';
  DLLUtilName: string = 'libeay32.dll';
{$ENDIF}

type
  PSSL_CTX = Pointer;
  PSSL = Pointer;
  PSSL_METHOD = Pointer;
  PX509 = Pointer;
  PX509_NAME = Pointer;
  PEVP_MD	= Pointer;
  PInteger = ^Integer;
  PBIO_METHOD = Pointer;
  PBIO = Pointer;

const
  EVP_MAX_MD_SIZE = 16 + 20;

  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_X509_LOOKUP = 4;
  SSL_ERROR_SYSCALL = 5; //look at error stack/return value/errno
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_ERROR_WANT_CONNECT = 7;
  SSL_ERROR_WANT_ACCEPT = 8;

  SSL_OP_NO_SSLv2 = $01000000;
  SSL_OP_NO_SSLv3 = $02000000;
  SSL_OP_NO_TLSv1 = $04000000;
  SSL_OP_ALL = $000FFFFF;
  SSL_VERIFY_NONE = $00;
  SSL_VERIFY_PEER = $01;

  X509_V_OK =	0;
  X509_V_ILLEGAL = 1;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT = 2;
  X509_V_ERR_UNABLE_TO_GET_CRL = 3;
  X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE = 4;
  X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE = 5;
  X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY = 6;
  X509_V_ERR_CERT_SIGNATURE_FAILURE = 7;
  X509_V_ERR_CRL_SIGNATURE_FAILURE = 8;
  X509_V_ERR_CERT_NOT_YET_VALID = 9;
  X509_V_ERR_CERT_HAS_EXPIRED = 10;
  X509_V_ERR_CRL_NOT_YET_VALID = 11;
  X509_V_ERR_CRL_HAS_EXPIRED = 12;
  X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD = 13;
  X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD = 14;
  X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD = 15;
  X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD = 16;
  X509_V_ERR_OUT_OF_MEM = 17;
  X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT = 18;
  X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN = 19;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY = 20;
  X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE = 21;
  X509_V_ERR_CERT_CHAIN_TOO_LONG = 22;
  X509_V_ERR_CERT_REVOKED = 23;
  X509_V_ERR_INVALID_CA = 24;
  X509_V_ERR_PATH_LENGTH_EXCEEDED = 25;
  X509_V_ERR_INVALID_PURPOSE = 26;
  X509_V_ERR_CERT_UNTRUSTED = 27;
  X509_V_ERR_CERT_REJECTED = 28;
  //These are 'informational' when looking for issuer cert
  X509_V_ERR_SUBJECT_ISSUER_MISMATCH = 29;
  X509_V_ERR_AKID_SKID_MISMATCH = 30;
  X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH = 31;
  X509_V_ERR_KEYUSAGE_NO_CERTSIGN = 32;
  X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER = 33;
  X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION = 34;
  //The application is not happy
  X509_V_ERR_APPLICATION_VERIFICATION = 50;

var
  SSLLibHandle: Integer = 0;
  SSLUtilHandle: Integer = 0;
  SSLLibFile: string = '';
  SSLUtilFile: string = '';

// libssl.dll
  function SslGetError(s: PSSL; ret_code: Integer):Integer;
  function SslLibraryInit:Integer;
  procedure SslLoadErrorStrings;
  function SslCtxSetCipherList(arg0: PSSL_CTX; str: PChar):Integer;
  function SslCtxNew(meth: PSSL_METHOD):PSSL_CTX;
  procedure SslCtxFree(arg0: PSSL_CTX);
  function SslSetFd(s: PSSL; fd: Integer):Integer;
  function SslMethodV2:PSSL_METHOD;
  function SslMethodV3:PSSL_METHOD;
  function SslMethodTLSV1:PSSL_METHOD;
  function SslMethodV23:PSSL_METHOD;
  function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: PChar; _type: Integer):Integer;
  function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: PChar):Integer;
  function SslCtxCheckPrivateKeyFile(ctx: PSSL_CTX):Integer;
  procedure SslCtxSetDefaultPasswdCb(ctx: PSSL_CTX; cb: Pointer);
  procedure SslCtxSetDefaultPasswdCbUserdata(ctx: PSSL_CTX; u: Pointer);
  function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: PChar; const CApath: PChar):Integer;
  function SslNew(ctx: PSSL_CTX):PSSL;
  procedure SslFree(ssl: PSSL);
  function SslAccept(ssl: PSSL):Integer;
  function SslConnect(ssl: PSSL):Integer;
  function SslShutdown(ssl: PSSL):Integer;
  function SslRead(ssl: PSSL; buf: PChar; num: Integer):Integer;
  function SslPeek(ssl: PSSL; buf: PChar; num: Integer):Integer;
  function SslWrite(ssl: PSSL; const buf: PChar; num: Integer):Integer;
  function SslPending(ssl: PSSL):Integer;
  function SslGetVersion(ssl: PSSL):PChar;
  function SslGetPeerCertificate(ssl: PSSL):PX509;
  procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: Integer; arg2: Pointer);
  function SSLGetCurrentCipher(s: PSSL):pointer;
  function SSLCipherGetName(c: pointer):PChar;
  function SSLCipherGetBits(c: pointer; alg_bits: PInteger):Integer;
  function SSLGetVerifyResult(ssl: PSSL):Integer;

// libeay.dll
  procedure SslX509Free(x: PX509);
  function SslX509NameOneline(a: PX509_NAME; buf: PChar; size: Integer):PChar;
  function SslX509GetSubjectName(a: PX509):PX509_NAME;
  function SslX509GetIssuerName(a: PX509):PX509_NAME;
  function SslX509NameHash(x: PX509_NAME):Cardinal;
  function SslX509Digest(data: PX509; _type: PEVP_MD; md: PChar; len: PInteger):Integer;
  function SslEvpMd5:PEVP_MD;
  function ErrErrorString(e: integer; buf: PChar): PChar;
  function ErrGetError: integer;
  procedure ErrClearError;
  procedure ErrFreeStrings;
  procedure ErrRemoveState(pid: integer);
  procedure EVPcleanup;
  procedure CRYPTOcleanupAllExData;
  procedure RandScreen;
  function BioNew(b: PBIO_METHOD): PBIO;
  procedure BioFreeAll(b: PBIO);
  function BioSMem: PBIO_METHOD;
  function BioCtrlPending(b: PBIO): integer;
  function BioRead(b: PBIO; Buf: PChar; Len: integer): integer;
  function BioWrite(b: PBIO; Buf: PChar; Len: integer): integer;
  function X509print(b: PBIO; a: PX509): integer;

function IsSSLloaded: Boolean;
function InitSSLInterface: Boolean;
function DestroySSLInterface: Boolean;

implementation

uses SyncObjs;

type
// libssl.dll
  TSslGetError = function(s: PSSL; ret_code: Integer):Integer; cdecl;
  TSslLibraryInit = function:Integer; cdecl;
  TSslLoadErrorStrings = procedure; cdecl;
  TSslCtxSetCipherList = function(arg0: PSSL_CTX; str: PChar):Integer; cdecl;
  TSslCtxNew = function(meth: PSSL_METHOD):PSSL_CTX; cdecl;
  TSslCtxFree = procedure(arg0: PSSL_CTX); cdecl;
  TSslSetFd = function(s: PSSL; fd: Integer):Integer; cdecl;
  TSslMethodV2 = function:PSSL_METHOD; cdecl;
  TSslMethodV3 = function:PSSL_METHOD; cdecl;
  TSslMethodTLSV1 = function:PSSL_METHOD; cdecl;
  TSslMethodV23 = function:PSSL_METHOD; cdecl;
  TSslCtxUsePrivateKeyFile = function(ctx: PSSL_CTX; const _file: PChar; _type: Integer):Integer; cdecl;
  TSslCtxUseCertificateChainFile = function(ctx: PSSL_CTX; const _file: PChar):Integer; cdecl;
  TSslCtxCheckPrivateKeyFile = function(ctx: PSSL_CTX):Integer; cdecl;
  TSslCtxSetDefaultPasswdCb = procedure(ctx: PSSL_CTX; cb: Pointer); cdecl;
  TSslCtxSetDefaultPasswdCbUserdata = procedure(ctx: PSSL_CTX; u: Pointer); cdecl;
  TSslCtxLoadVerifyLocations = function(ctx: PSSL_CTX; const CAfile: PChar; const CApath: PChar):Integer; cdecl;
  TSslNew = function(ctx: PSSL_CTX):PSSL; cdecl;
  TSslFree = procedure(ssl: PSSL); cdecl;
  TSslAccept = function(ssl: PSSL):Integer; cdecl;
  TSslConnect = function(ssl: PSSL):Integer; cdecl;
  TSslShutdown = function(ssl: PSSL):Integer; cdecl;
  TSslRead = function(ssl: PSSL; buf: PChar; num: Integer):Integer; cdecl;
  TSslPeek = function(ssl: PSSL; buf: PChar; num: Integer):Integer; cdecl;
  TSslWrite = function(ssl: PSSL; const buf: PChar; num: Integer):Integer; cdecl;
  TSslPending = function(ssl: PSSL):Integer; cdecl;
  TSslGetVersion = function(ssl: PSSL):PChar; cdecl;
  TSslGetPeerCertificate = function(ssl: PSSL):PX509; cdecl;
  TSslCtxSetVerify = procedure(ctx: PSSL_CTX; mode: Integer; arg2: Pointer); cdecl;
  TSSLGetCurrentCipher = function(s: PSSL):pointer; cdecl;
  TSSLCipherGetName = function(c: pointer):PChar; cdecl;
  TSSLCipherGetBits = function(c: pointer; alg_bits: PInteger):Integer; cdecl;
  TSSLGetVerifyResult = function(ssl: PSSL):Integer; cdecl;

// libeay.dll
  TSslX509Free = procedure(x: PX509); cdecl;
  TSslX509NameOneline = function(a: PX509_NAME; buf: PChar; size: Integer):PChar; cdecl;
  TSslX509GetSubjectName = function(a: PX509):PX509_NAME; cdecl;
  TSslX509GetIssuerName = function(a: PX509):PX509_NAME; cdecl;
  TSslX509NameHash = function(x: PX509_NAME):Cardinal; cdecl;
  TSslX509Digest = function(data: PX509; _type: PEVP_MD; md: PChar; len: PInteger):Integer; cdecl;
  TSslEvpMd5 = function:PEVP_MD; cdecl;
  TErrErrorString = function(e: integer; buf: PChar): PChar; cdecl;
  TErrGetError = function: integer; cdecl;
  TErrClearError = procedure; cdecl;
  TErrFreeStrings = procedure; cdecl;
  TErrRemoveState = procedure(pid: integer); cdecl;
  TEVPcleanup = procedure; cdecl;
  TCRYPTOcleanupAllExData = procedure; cdecl;
  TRandScreen = procedure; cdecl;
  TBioNew = function(b: PBIO_METHOD): PBIO; cdecl;
  TBioFreeAll = procedure(b: PBIO); cdecl;
  TBioSMem = function: PBIO_METHOD; cdecl;
  TBioCtrlPending = function(b: PBIO): integer; cdecl;
  TBioRead = function(b: PBIO; Buf: PChar; Len: integer): integer; cdecl;
  TBioWrite = function(b: PBIO; Buf: PChar; Len: integer): integer; cdecl;
  TX509print = function(b: PBIO; a: PX509): integer; cdecl;

var
// libssl.dll
  _SslGetError: TSslGetError = nil;
  _SslLibraryInit: TSslLibraryInit = nil;
  _SslLoadErrorStrings: TSslLoadErrorStrings = nil;
  _SslCtxSetCipherList: TSslCtxSetCipherList = nil;
  _SslCtxNew: TSslCtxNew = nil;
  _SslCtxFree: TSslCtxFree = nil;
  _SslSetFd: TSslSetFd = nil;
  _SslMethodV2: TSslMethodV2 = nil;
  _SslMethodV3: TSslMethodV3 = nil;
  _SslMethodTLSV1: TSslMethodTLSV1 = nil;
  _SslMethodV23: TSslMethodV23 = nil;
  _SslCtxUsePrivateKeyFile: TSslCtxUsePrivateKeyFile = nil;
  _SslCtxUseCertificateChainFile: TSslCtxUseCertificateChainFile = nil;
  _SslCtxCheckPrivateKeyFile: TSslCtxCheckPrivateKeyFile = nil;
  _SslCtxSetDefaultPasswdCb: TSslCtxSetDefaultPasswdCb = nil;
  _SslCtxSetDefaultPasswdCbUserdata: TSslCtxSetDefaultPasswdCbUserdata = nil;
  _SslCtxLoadVerifyLocations: TSslCtxLoadVerifyLocations = nil;
  _SslNew: TSslNew = nil;
  _SslFree: TSslFree = nil;
  _SslAccept: TSslAccept = nil;
  _SslConnect: TSslConnect = nil;
  _SslShutdown: TSslShutdown = nil;
  _SslRead: TSslRead = nil;
  _SslPeek: TSslPeek = nil;
  _SslWrite: TSslWrite = nil;
  _SslPending: TSslPending = nil;
  _SslGetVersion: TSslGetVersion = nil;
  _SslGetPeerCertificate: TSslGetPeerCertificate = nil;
  _SslCtxSetVerify: TSslCtxSetVerify = nil;
  _SSLGetCurrentCipher: TSSLGetCurrentCipher = nil;
  _SSLCipherGetName: TSSLCipherGetName = nil;
  _SSLCipherGetBits: TSSLCipherGetBits = nil;
  _SSLGetVerifyResult: TSSLGetVerifyResult = nil;

// libeay.dll
  _SslX509Free: TSslX509Free = nil;
  _SslX509NameOneline: TSslX509NameOneline = nil;
  _SslX509GetSubjectName: TSslX509GetSubjectName = nil;
  _SslX509GetIssuerName: TSslX509GetIssuerName = nil;
  _SslX509NameHash: TSslX509NameHash = nil;
  _SslX509Digest: TSslX509Digest = nil;
  _SslEvpMd5: TSslEvpMd5 = nil;
  _ErrErrorString: TErrErrorString = nil;
  _ErrGetError: TErrGetError = nil;
  _ErrClearError: TErrClearError = nil;
  _ErrFreeStrings: TErrFreeStrings = nil;
  _ErrRemoveState: TErrRemoveState = nil;
  _EVPcleanup: TEVPcleanup = nil;
  _CRYPTOcleanupAllExData: TCRYPTOcleanupAllExData = nil;
  _RandScreen: TRandScreen = nil;
  _BioNew: TBioNew = nil;
  _BioFreeAll: TBioFreeAll = nil;
  _BioSMem: TBioSMem = nil;
  _BioCtrlPending: TBioCtrlPending = nil;
  _BioRead: TBioRead = nil;
  _BioWrite: TBioWrite = nil;
  _X509print: TX509print = nil;

var
  SSLCS: TCriticalSection;
  SSLloaded: boolean = false;

// libssl.dll
function SslGetError(s: PSSL; ret_code: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslGetError) then
    Result := _SslGetError(s, ret_code)
  else
    Result := SSL_ERROR_SSL;
end;

function SslLibraryInit:Integer;
begin
  if InitSSLInterface and Assigned(_SslLibraryInit) then
    Result := _SslLibraryInit
  else
    Result := 1;
end;

procedure SslLoadErrorStrings;
begin
  if InitSSLInterface and Assigned(_SslLoadErrorStrings) then
    _SslLoadErrorStrings;
end;

function SslCtxSetCipherList(arg0: PSSL_CTX; str: PChar):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxSetCipherList) then
    Result := _SslCtxSetCipherList(arg0, str)
  else
    Result := 0;
end;

function SslCtxNew(meth: PSSL_METHOD):PSSL_CTX;
begin
  if InitSSLInterface and Assigned(_SslCtxNew) then
    Result := _SslCtxNew(meth)
  else
    Result := nil;
end;

procedure SslCtxFree(arg0: PSSL_CTX);
begin
  if InitSSLInterface and Assigned(_SslCtxFree) then
    _SslCtxFree(arg0);
end;

function SslSetFd(s: PSSL; fd: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslSetFd) then
    Result := _SslSetFd(s, fd)
  else
    Result := 0;
end;

function SslMethodV2:PSSL_METHOD;
begin
  if InitSSLInterface and Assigned(_SslMethodV2) then
    Result := _SslMethodV2
  else
    Result := nil;
end;

function SslMethodV3:PSSL_METHOD;
begin
  if InitSSLInterface and Assigned(_SslMethodV3) then
    Result := _SslMethodV3
  else
    Result := nil;
end;

function SslMethodTLSV1:PSSL_METHOD;
begin
  if InitSSLInterface and Assigned(_SslMethodTLSV1) then
    Result := _SslMethodTLSV1
  else
    Result := nil;
end;

function SslMethodV23:PSSL_METHOD;
begin
  if InitSSLInterface and Assigned(_SslMethodV23) then
    Result := _SslMethodV23
  else
    Result := nil;
end;

function SslCtxUsePrivateKeyFile(ctx: PSSL_CTX; const _file: PChar; _type: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxUsePrivateKeyFile) then
    Result := _SslCtxUsePrivateKeyFile(ctx, _file, _type)
  else
    Result := 0;
end;

function SslCtxUseCertificateChainFile(ctx: PSSL_CTX; const _file: PChar):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxUseCertificateChainFile) then
    Result := _SslCtxUseCertificateChainFile(ctx, _file)
  else
    Result := 0;
end;

function SslCtxCheckPrivateKeyFile(ctx: PSSL_CTX):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxCheckPrivateKeyFile) then
    Result := _SslCtxCheckPrivateKeyFile(ctx)
  else
    Result := 0;
end;

procedure SslCtxSetDefaultPasswdCb(ctx: PSSL_CTX; cb: Pointer);
begin
  if InitSSLInterface and Assigned(_SslCtxSetDefaultPasswdCb) then
    _SslCtxSetDefaultPasswdCb(ctx, cb);
end;

procedure SslCtxSetDefaultPasswdCbUserdata(ctx: PSSL_CTX; u: Pointer);
begin
  if InitSSLInterface and Assigned(_SslCtxSetDefaultPasswdCbUserdata) then
    _SslCtxSetDefaultPasswdCbUserdata(ctx, u);
end;

function SslCtxLoadVerifyLocations(ctx: PSSL_CTX; const CAfile: PChar; const CApath: PChar):Integer;
begin
  if InitSSLInterface and Assigned(_SslCtxLoadVerifyLocations) then
    Result := _SslCtxLoadVerifyLocations(ctx, CAfile, CApath)
  else
    Result := 0;
end;

function SslNew(ctx: PSSL_CTX):PSSL;
begin
  if InitSSLInterface and Assigned(_SslNew) then
    Result := _SslNew(ctx)
  else
    Result := nil;
end;

procedure SslFree(ssl: PSSL);
begin
  if InitSSLInterface and Assigned(_SslFree) then
    _SslFree(ssl);
end;

function SslAccept(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SslAccept) then
    Result := _SslAccept(ssl)
  else
    Result := -1;
end;

function SslConnect(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SslConnect) then
    Result := _SslConnect(ssl)
  else
    Result := -1;
end;

function SslShutdown(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SslShutdown) then
    Result := _SslShutdown(ssl)
  else
    Result := -1;
end;

function SslRead(ssl: PSSL; buf: PChar; num: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslRead) then
    Result := _SslRead(ssl, buf, num)
  else
    Result := -1;
end;

function SslPeek(ssl: PSSL; buf: PChar; num: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslPeek) then
    Result := _SslPeek(ssl, buf, num)
  else
    Result := -1;
end;

function SslWrite(ssl: PSSL; const buf: PChar; num: Integer):Integer;
begin
  if InitSSLInterface and Assigned(_SslWrite) then
    Result := _SslWrite(ssl, buf, num)
  else
    Result := -1;
end;

function SslPending(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SslPending) then
    Result := _SslPending(ssl)
  else
    Result := 0;
end;

function SslGetVersion(ssl: PSSL):PChar;
begin
  if InitSSLInterface and Assigned(_SslGetVersion) then
    Result := _SslGetVersion(ssl)
  else
    Result := nil;
end;

function SslGetPeerCertificate(ssl: PSSL):PX509;
begin
  if InitSSLInterface and Assigned(_SslGetPeerCertificate) then
    Result := _SslGetPeerCertificate(ssl)
  else
    Result := nil;
end;

procedure SslCtxSetVerify(ctx: PSSL_CTX; mode: Integer; arg2: Pointer);
begin
  if InitSSLInterface and Assigned(_SslCtxSetVerify) then
    _SslCtxSetVerify(ctx, mode, arg2);
end;

function SSLGetCurrentCipher(s: PSSL):pointer;
begin
  if InitSSLInterface and Assigned(_SSLGetCurrentCipher) then
    Result := _SSLGetCurrentCipher(s)
  else
    Result := nil;
end;

function SSLCipherGetName(c: pointer):PChar;
begin
  if InitSSLInterface and Assigned(_SSLCipherGetName) then
    Result := _SSLCipherGetName(c)
  else
    Result := nil;
end;

function SSLCipherGetBits(c: pointer; alg_bits: PInteger):Integer;
begin
  if InitSSLInterface and Assigned(_SSLCipherGetBits) then
    Result := _SSLCipherGetBits(c, alg_bits)
  else
    Result := 0;
end;

function SSLGetVerifyResult(ssl: PSSL):Integer;
begin
  if InitSSLInterface and Assigned(_SSLGetVerifyResult) then
    Result := _SSLGetVerifyResult(ssl)
  else
    Result := X509_V_ERR_APPLICATION_VERIFICATION;
end;

// libeay.dll
procedure SslX509Free(x: PX509);
begin
  if InitSSLInterface and Assigned(_SslX509Free) then
    _SslX509Free(x);
end;

function SslX509NameOneline(a: PX509_NAME; buf: PChar; size: Integer):PChar;
begin
  if InitSSLInterface and Assigned(_SslX509NameOneline) then
    Result := _SslX509NameOneline(a, buf,size)
  else
    Result := nil;
end;

function SslX509GetSubjectName(a: PX509):PX509_NAME;
begin
  if InitSSLInterface and Assigned(_SslX509GetSubjectName) then
    Result := _SslX509GetSubjectName(a)
  else
    Result := nil;
end;

function SslX509GetIssuerName(a: PX509):PX509_NAME;
begin
  if InitSSLInterface and Assigned(_SslX509GetIssuerName) then
    Result := _SslX509GetIssuerName(a)
  else
    Result := nil;
end;

function SslX509NameHash(x: PX509_NAME):Cardinal;
begin
  if InitSSLInterface and Assigned(_SslX509NameHash) then
    Result := _SslX509NameHash(x)
  else
    Result := 0;
end;

function SslX509Digest(data: PX509; _type: PEVP_MD; md: PChar; len: PInteger):Integer;
begin
  if InitSSLInterface and Assigned(_SslX509Digest) then
    Result := _SslX509Digest(data, _type, md, len)
  else
    Result := 0;
end;

function SslEvpMd5:PEVP_MD;
begin
  if InitSSLInterface and Assigned(_SslEvpMd5) then
    Result := _SslEvpMd5
  else
    Result := nil;
end;

function ErrErrorString(e: integer; buf: PChar): PChar;
begin
  if InitSSLInterface and Assigned(_ErrErrorString) then
    Result := _ErrErrorString(e, buf)
  else
    Result := nil;
end;

function ErrGetError: integer;
begin
  if InitSSLInterface and Assigned(_ErrGetError) then
    Result := _ErrGetError
  else
    Result := SSL_ERROR_SSL;
end;

procedure ErrClearError;
begin
  if InitSSLInterface and Assigned(_ErrClearError) then
    _ErrClearError;
end;

procedure ErrFreeStrings;
begin
  if InitSSLInterface and Assigned(_ErrFreeStrings) then
    _ErrFreeStrings;
end;

procedure ErrRemoveState(pid: integer);
begin
  if InitSSLInterface and Assigned(_ErrRemoveState) then
    _ErrRemoveState(pid);
end;

procedure EVPcleanup;
begin
  if InitSSLInterface and Assigned(_EVPcleanup) then
    _EVPcleanup;
end;

procedure CRYPTOcleanupAllExData;
begin
  if InitSSLInterface and Assigned(_CRYPTOcleanupAllExData) then
    _CRYPTOcleanupAllExData;
end;

procedure RandScreen;
begin
  if InitSSLInterface and Assigned(_RandScreen) then
    _RandScreen;
end;

function BioNew(b: PBIO_METHOD): PBIO;
begin
  if InitSSLInterface and Assigned(_BioNew) then
    Result := _BioNew(b)
  else
    Result := nil;
end;

procedure BioFreeAll(b: PBIO);
begin
  if InitSSLInterface and Assigned(_BioFreeAll) then
    _BioFreeAll(b);
end;

function BioSMem: PBIO_METHOD;
begin
  if InitSSLInterface and Assigned(_BioSMem) then
    Result := _BioSMem
  else
    Result := nil;
end;

function BioCtrlPending(b: PBIO): integer;
begin
  if InitSSLInterface and Assigned(_BioCtrlPending) then
    Result := _BioCtrlPending(b)
  else
    Result := 0;
end;

function BioRead(b: PBIO; Buf: PChar; Len: integer): integer;
begin
  if InitSSLInterface and Assigned(_BioRead) then
    Result := _BioRead(b, Buf, Len)
  else
    Result := -2;
end;

function BioWrite(b: PBIO; Buf: PChar; Len: integer): integer;
begin
  if InitSSLInterface and Assigned(_BioWrite) then
    Result := _BioWrite(b, Buf, Len)
  else
    Result := -2;
end;

function X509print(b: PBIO; a: PX509): integer;
begin
  if InitSSLInterface and Assigned(_X509print) then
    Result := _X509print(b, a)
  else
    Result := 0;
end;


function InitSSLInterface: Boolean;
var
  s: string;
  x: integer;
begin
  SSLCS.Enter;
  try
    if not IsSSLloaded then
    begin
      SSLLibHandle := LoadLibrary(PChar(DLLSSLName));
      SSLUtilHandle := LoadLibrary(PChar(DLLUtilName));
{$IFNDEF LINUX}
      if (SSLLibHandle = 0) then
        SSLLibHandle := LoadLibrary(PChar(DLLSSLName2));
{$ENDIF}
      if (SSLLibHandle <> 0) and (SSLUtilHandle <> 0) then
      begin
        _SslGetError := GetProcAddress(SSLLibHandle, PChar('SSL_get_error'));
        _SslLibraryInit := GetProcAddress(SSLLibHandle, PChar('SSL_library_init'));
        _SslLoadErrorStrings := GetProcAddress(SSLLibHandle, PChar('SSL_load_error_strings'));
        _SslCtxSetCipherList := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_set_cipher_list'));
        _SslCtxNew := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_new'));
        _SslCtxFree := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_free'));
        _SslSetFd := GetProcAddress(SSLLibHandle, PChar('SSL_set_fd'));
        _SslMethodV2 := GetProcAddress(SSLLibHandle, PChar('SSLv2_method'));
        _SslMethodV3 := GetProcAddress(SSLLibHandle, PChar('SSLv3_method'));
        _SslMethodTLSV1 := GetProcAddress(SSLLibHandle, PChar('TLSv1_method'));
        _SslMethodV23 := GetProcAddress(SSLLibHandle, PChar('SSLv23_method'));
        _SslCtxUsePrivateKeyFile := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_use_PrivateKey_file'));
        _SslCtxUseCertificateChainFile := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_use_certificate_chain_file'));
        _SslCtxCheckPrivateKeyFile := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_check_private_key'));
        _SslCtxSetDefaultPasswdCb := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_set_default_passwd_cb'));
        _SslCtxSetDefaultPasswdCbUserdata := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_set_default_passwd_cb_userdata'));
        _SslCtxLoadVerifyLocations := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_load_verify_locations'));
        _SslNew := GetProcAddress(SSLLibHandle, PChar('SSL_new'));
        _SslFree := GetProcAddress(SSLLibHandle, PChar('SSL_free'));
        _SslAccept := GetProcAddress(SSLLibHandle, PChar('SSL_accept'));
        _SslConnect := GetProcAddress(SSLLibHandle, PChar('SSL_connect'));
        _SslShutdown := GetProcAddress(SSLLibHandle, PChar('SSL_shutdown'));
        _SslRead := GetProcAddress(SSLLibHandle, PChar('SSL_read'));
        _SslPeek := GetProcAddress(SSLLibHandle, PChar('SSL_peek'));
        _SslWrite := GetProcAddress(SSLLibHandle, PChar('SSL_write'));
        _SslPending := GetProcAddress(SSLLibHandle, PChar('SSL_pending'));
        _SslGetPeerCertificate := GetProcAddress(SSLLibHandle, PChar('SSL_get_peer_certificate'));
        _SslGetVersion := GetProcAddress(SSLLibHandle, PChar('SSL_get_version'));
        _SslCtxSetVerify := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_set_verify'));
        _SslGetCurrentCipher := GetProcAddress(SSLLibHandle, PChar('SSL_get_current_cipher'));
        _SslCipherGetName := GetProcAddress(SSLLibHandle, PChar('SSL_CIPHER_get_name'));
        _SslCipherGetBits := GetProcAddress(SSLLibHandle, PChar('SSL_CIPHER_get_bits'));
        _SslGetVerifyResult := GetProcAddress(SSLLibHandle, PChar('SSL_get_verify_result'));

        _SslX509Free := GetProcAddress(SSLUtilHandle, PChar('X509_free'));
        _SslX509NameOneline := GetProcAddress(SSLUtilHandle, PChar('X509_NAME_oneline'));
        _SslX509GetSubjectName := GetProcAddress(SSLUtilHandle, PChar('X509_get_subject_name'));
        _SslX509GetIssuerName := GetProcAddress(SSLUtilHandle, PChar('X509_get_issuer_name'));
        _SslX509NameHash := GetProcAddress(SSLUtilHandle, PChar('X509_NAME_hash'));
        _SslX509Digest := GetProcAddress(SSLUtilHandle, PChar('X509_digest'));
        _SslEvpMd5 := GetProcAddress(SSLUtilHandle, PChar('EVP_md5'));
        _ErrErrorString := GetProcAddress(SSLUtilHandle, PChar('ERR_error_string'));
        _ErrGetError := GetProcAddress(SSLUtilHandle, PChar('ERR_get_error'));
        _ErrClearError := GetProcAddress(SSLUtilHandle, PChar('ERR_clear_error'));
        _ErrFreeStrings := GetProcAddress(SSLUtilHandle, PChar('ERR_free_strings'));
        _ErrRemoveState := GetProcAddress(SSLUtilHandle, PChar('ERR_remove_state'));
        _EVPCleanup := GetProcAddress(SSLUtilHandle, PChar('EVP_cleanup'));
        _CRYPTOcleanupAllExData := GetProcAddress(SSLUtilHandle, PChar('CRYPTO_cleanup_all_ex_data'));
        _RandScreen := GetProcAddress(SSLUtilHandle, PChar('RAND_screen'));
        _BioNew := GetProcAddress(SSLUtilHandle, PChar('BIO_new'));
        _BioFreeAll := GetProcAddress(SSLUtilHandle, PChar('BIO_free_all'));
        _BioSMem := GetProcAddress(SSLUtilHandle, PChar('BIO_s_mem'));
        _BioCtrlPending := GetProcAddress(SSLUtilHandle, PChar('BIO_ctrl_pending'));
        _BioRead := GetProcAddress(SSLUtilHandle, PChar('BIO_read'));
        _BioWrite := GetProcAddress(SSLUtilHandle, PChar('BIO_write'));
        _X509print := GetProcAddress(SSLUtilHandle, PChar('X509_print'));

        SetLength(s, 1024);
        x := GetModuleFilename(SSLLibHandle,PChar(s),Length(s));
        SetLength(s, x);
        SSLLibFile := s;
        SetLength(s, 1024);
        x := GetModuleFilename(SSLUtilHandle,PChar(s),Length(s));
        SetLength(s, x);
        SSLUtilFile := s;
        Result := True;
        //init library
        if assigned(_SslLibraryInit) then
          _SslLibraryInit;
        if assigned(_SslLoadErrorStrings) then
          _SslLoadErrorStrings;
        if assigned(_RandScreen) then
          _RandScreen;
        SSLloaded := True;
      end
      else
      begin
        //load failed!
        if SSLLibHandle <> 0 then
        begin
          FreeLibrary(SSLLibHandle);
          SSLLibHandle := 0;
        end;
        if SSLUtilHandle <> 0 then
        begin
          FreeLibrary(SSLUtilHandle);
          SSLLibHandle := 0;
        end;
        Result := False;
      end;
    end
    else
      //loaded before...
      Result := true;
  finally
    SSLCS.Leave;
  end;
end;

function DestroySSLInterface: Boolean;
begin
  SSLCS.Enter;
  try
    if IsSSLLoaded then
    begin
      //deinit library
      EVPCleanup;
      CRYPTOcleanupAllExData;
      ErrRemoveState(0);
    end;
    SSLloaded := false;
    if SSLLibHandle <> 0 then
    begin
      FreeLibrary(SSLLibHandle);
      SSLLibHandle := 0;
    end;
    if SSLUtilHandle <> 0 then
    begin
      FreeLibrary(SSLUtilHandle);
      SSLLibHandle := 0;
    end;

    _SslGetError := nil;
    _SslLibraryInit := nil;
    _SslLoadErrorStrings := nil;
    _SslCtxSetCipherList := nil;
    _SslCtxNew := nil;
    _SslCtxFree := nil;
    _SslSetFd := nil;
    _SslMethodV2 := nil;
    _SslMethodV3 := nil;
    _SslMethodTLSV1 := nil;
    _SslMethodV23 := nil;
    _SslCtxUsePrivateKeyFile := nil;
    _SslCtxUseCertificateChainFile := nil;
    _SslCtxCheckPrivateKeyFile := nil;
    _SslCtxSetDefaultPasswdCb := nil;
    _SslCtxSetDefaultPasswdCbUserdata := nil;
    _SslCtxLoadVerifyLocations := nil;
    _SslNew := nil;
    _SslFree := nil;
    _SslAccept := nil;
    _SslConnect := nil;
    _SslShutdown := nil;
    _SslRead := nil;
    _SslPeek := nil;
    _SslWrite := nil;
    _SslPending := nil;
    _SslGetPeerCertificate := nil;
    _SslGetVersion := nil;
    _SslCtxSetVerify := nil;
    _SslGetCurrentCipher := nil;
    _SslCipherGetName := nil;
    _SslCipherGetBits := nil;
    _SslGetVerifyResult := nil;

    _SslX509Free := nil;
    _SslX509NameOneline := nil;
    _SslX509GetSubjectName := nil;
    _SslX509GetIssuerName := nil;
    _SslX509NameHash := nil;
    _SslX509Digest := nil;
    _SslEvpMd5 := nil;
    _ErrErrorString := nil;
    _ErrGetError := nil;
    _ErrClearError := nil;
    _ErrFreeStrings := nil;
    _ErrRemoveState := nil;
    _EVPCleanup := nil;
    _CRYPTOcleanupAllExData := nil;
    _RandScreen := nil;
    _BioNew := nil;
    _BioFreeAll := nil;
    _BioSMem := nil;
    _BioCtrlPending := nil;
    _BioRead := nil;
    _BioWrite := nil;
    _X509print := nil;
  finally
    SSLCS.Leave;
  end;
  Result := True;
end;

function IsSSLloaded: Boolean;
begin
  Result := SSLLoaded;
end;

initialization
begin
  SSLCS:= TCriticalSection.Create;
end;

finalization
begin
  DestroySSLInterface;
  SSLCS.Free;
end;

end.
