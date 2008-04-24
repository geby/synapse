{==============================================================================|
| Project : Delphree - Synapse                                   | 001.007.001 |
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
 for good inspiration about SSL programming.
}

{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
  (*$HPPEMIT 'namespace Synassl { using System::Shortint; }' *)
{$ENDIF}

unit SynaSSL;

interface

uses
{$IFDEF LINUX}
  Libc, SysUtils;
{$ELSE}
  Windows;
{$ENDIF}

const
{$IFDEF LINUX}
  DLLSSLName = 'libssl.so';
  DLLUtilName = 'libcrypto.so';
{$ELSE}
  DLLSSLName = 'ssleay32.dll';
  DLLSSLName2 = 'libssl32.dll';
  DLLUtilName = 'libeay32.dll';
{$ENDIF}

type
  PSSL_CTX = Pointer;
  PSSL = Pointer;
  PSSL_METHOD = Pointer;
  PX509 = Pointer;
  PX509_NAME = Pointer;
  PEVP_MD	= Pointer;
  PInteger = ^Integer;

const
  EVP_MAX_MD_SIZE = 16+20;
  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_OP_NO_SSLv2 = $01000000;
  SSL_OP_NO_SSLv3 = $02000000;
  SSL_OP_NO_TLSv1 = $04000000;
  SSL_OP_ALL = $000FFFFF;
  SSL_VERIFY_NONE = $00;
  SSL_VERIFY_PEER = $01;

var
  SSLLibHandle: Integer = 0;
  SSLUtilHandle: Integer = 0;
  SSLLibName: string = '';

// libssl.dll
  SslGetError : function(s: PSSL; ret_code: Integer):Integer cdecl = nil;
  SslLibraryInit : function:Integer cdecl = nil;
  SslLoadErrorStrings : procedure cdecl = nil;
  SslCtxSetCipherList : function(arg0: PSSL_CTX; str: PChar):Integer cdecl = nil;
  SslCtxNew : function(meth: PSSL_METHOD):PSSL_CTX cdecl = nil;
  SslCtxFree : procedure(arg0: PSSL_CTX) cdecl = nil;
  SslSetFd : function(s: PSSL; fd: Integer):Integer cdecl = nil;
  SslMethodV23 : function:PSSL_METHOD cdecl = nil;
  SslCtxUsePrivateKeyFile : function(ctx: PSSL_CTX; const _file: PChar; _type: Integer):Integer cdecl = nil;
  SslCtxUseCertificateChainFile : function(ctx: PSSL_CTX; const _file: PChar):Integer cdecl = nil;
  SslCtxCheckPrivateKeyFile : function(ctx: PSSL_CTX):Integer cdecl = nil;
  SslCtxSetDefaultPasswdCb : procedure(ctx: PSSL_CTX; cb: Pointer) cdecl = nil;
  SslCtxSetDefaultPasswdCbUserdata : procedure(ctx: PSSL_CTX; u: Pointer) cdecl = nil;
  SslCtxLoadVerifyLocations : function(ctx: PSSL_CTX; const CAfile: PChar; const CApath: PChar):Integer cdecl = nil;
  SslNew : function(ctx: PSSL_CTX):PSSL cdecl = nil;
  SslFree : procedure(ssl: PSSL) cdecl = nil;
  SslAccept : function(ssl: PSSL):Integer cdecl = nil;
  SslConnect : function(ssl: PSSL):Integer cdecl = nil;
  SslShutdown : function(ssl: PSSL):Integer cdecl = nil;
  SslRead : function(ssl: PSSL; buf: PChar; num: Integer):Integer cdecl = nil;
  SslPeek : function(ssl: PSSL; buf: PChar; num: Integer):Integer cdecl = nil;
  SslWrite : function(ssl: PSSL; const buf: PChar; num: Integer):Integer cdecl = nil;
  SslPending : function(ssl: PSSL):Integer cdecl = nil;
  SslGetVersion : function(ssl: PSSL):PChar cdecl = nil;
  SslGetPeerCertificate : function(ssl: PSSL):PX509 cdecl = nil;
  SslCtxSetVerify : procedure(ctx: PSSL_CTX; mode: Integer; arg2: Pointer) cdecl = nil;

// libeay.dll
  SslX509Free : procedure(x: PX509) cdecl = nil;
  SslX509NameOneline : function(a: PX509_NAME; buf: PChar; size: Integer):PChar cdecl = nil;
  SslX509GetSubjectName : function(a: PX509):PX509_NAME cdecl = nil;
  SslX509GetIssuerName : function(a: PX509):PX509_NAME cdecl = nil;
  SslX509NameHash : function(x: PX509_NAME):Cardinal cdecl = nil;
  SslX509Digest : function(data: PX509; _type: PEVP_MD; md: PChar; len: PInteger):Integer cdecl = nil;
  SslEvpMd5 : function:PEVP_MD cdecl = nil;
  ErrErrorString : function(e: integer; buf: PChar): PChar cdecl = nil;
  ErrGetError : function: integer cdecl = nil;
  ErrClearError : procedure cdecl = nil;

function InitSSLInterface: Boolean;
function DestroySSLInterface: Boolean;

implementation

uses SyncObjs;

var
  SSLCS: TCriticalSection;
  SSLCount: Integer = 0;

function InitSSLInterface: Boolean;
begin
  Result := False;
  SSLCS.Enter;
  try
    if SSLCount = 0 then
    begin
{$IFDEF LINUX}
      SSLLibHandle := HMODULE(dlopen(DLLSSLName, RTLD_GLOBAL));
      SSLLibName := DLLSSLName;
      SSLUtilHandle := HMODULE(dlopen(DLLUtilName, RTLD_GLOBAL));
{$ELSE}
      SSLLibHandle := LoadLibrary(PChar(DLLSSLName));
      SSLLibName := DLLSSLName;
      if (SSLLibHandle = 0) then
      begin
        SSLLibHandle := LoadLibrary(PChar(DLLSSLName2));
        SSLLibName := DLLSSLName2;
      end;
      SSLUtilHandle := LoadLibrary(PChar(DLLUtilName));
{$ENDIF}
      if (SSLLibHandle <> 0) and (SSLUtilHandle <> 0) then
      begin
        SslGetError := GetProcAddress(SSLLibHandle, PChar('SSL_get_error'));
        SslLibraryInit := GetProcAddress(SSLLibHandle, PChar('SSL_library_init'));
        SslLoadErrorStrings := GetProcAddress(SSLLibHandle, PChar('SSL_load_error_strings'));
        SslCtxSetCipherList := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_set_cipher_list'));
        SslCtxNew := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_new'));
        SslCtxFree := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_free'));
        SslSetFd := GetProcAddress(SSLLibHandle, PChar('SSL_set_fd'));
        SslMethodV23 := GetProcAddress(SSLLibHandle, PChar('SSLv23_method'));
        SslCtxUsePrivateKeyFile := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_use_PrivateKey_file'));
        SslCtxUseCertificateChainFile := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_use_certificate_chain_file'));
        SslCtxCheckPrivateKeyFile := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_check_private_key'));
        SslCtxSetDefaultPasswdCb := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_set_default_passwd_cb'));
        SslCtxSetDefaultPasswdCbUserdata := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_set_default_passwd_cb_userdata'));
        SslCtxLoadVerifyLocations := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_load_verify_locations'));
        SslNew := GetProcAddress(SSLLibHandle, PChar('SSL_new'));
        SslFree := GetProcAddress(SSLLibHandle, PChar('SSL_free'));
        SslAccept := GetProcAddress(SSLLibHandle, PChar('SSL_accept'));
        SslConnect := GetProcAddress(SSLLibHandle, PChar('SSL_connect'));
        SslShutdown := GetProcAddress(SSLLibHandle, PChar('SSL_shutdown'));
        SslRead := GetProcAddress(SSLLibHandle, PChar('SSL_read'));
        SslPeek := GetProcAddress(SSLLibHandle, PChar('SSL_peek'));
        SslWrite := GetProcAddress(SSLLibHandle, PChar('SSL_write'));
        SslPending := GetProcAddress(SSLLibHandle, PChar('SSL_pending'));
        SslGetPeerCertificate := GetProcAddress(SSLLibHandle, PChar('SSL_get_peer_certificate'));
        SslGetVersion := GetProcAddress(SSLLibHandle, PChar('SSL_get_version'));
        SslCtxSetVerify := GetProcAddress(SSLLibHandle, PChar('SSL_CTX_set_verify'));

        SslX509Free := GetProcAddress(SSLUtilHandle, PChar('X509_free'));
        SslX509NameOneline := GetProcAddress(SSLUtilHandle, PChar('X509_NAME_oneline'));
        SslX509GetSubjectName := GetProcAddress(SSLUtilHandle, PChar('X509_get_subject_name'));
        SslX509GetIssuerName := GetProcAddress(SSLUtilHandle, PChar('X509_get_issuer_name'));
        SslX509NameHash := GetProcAddress(SSLUtilHandle, PChar('X509_NAME_hash'));
        SslX509Digest := GetProcAddress(SSLUtilHandle, PChar('X509_digest'));
        SslEvpMd5 := GetProcAddress(SSLUtilHandle, PChar('EVP_md5'));
        ErrerrorString := GetProcAddress(SSLUtilHandle, PChar('ERR_error_string'));
        ErrGetError := GetProcAddress(SSLUtilHandle, PChar('ERR_get_error'));
        ErrClearError := GetProcAddress(SSLUtilHandle, PChar('ERR_clear_error'));

        Result := True;
      end;
    end
    else Result := True;
    if Result then
      Inc(SSLCount);
  finally
    SSLCS.Leave;
  end;
end;

function DestroySSLInterface: Boolean;
begin
  SSLCS.Enter;
  try
    Dec(SSLCount);
    if SSLCount < 0 then
      SSLCount := 0;
    if SSLCount = 0 then
    begin
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
    end;
  finally
    SSLCS.Leave;
  end;
  Result := True;
end;

initialization
begin
  SSLCS:= TCriticalSection.Create;
end;

finalization
begin
  SSLCS.Free;
end;

end.
