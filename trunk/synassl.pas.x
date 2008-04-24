{==============================================================================|
| Project : Delphree - Synapse                                   | 001.004.000 |
|==============================================================================|
| Content: SSL support                                                         |
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
| Portions created by Lukas Gebauer are Copyright (c)2002.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

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
  DLLSSLName = 'libssl32.dll';
  DLLSSLName2 = 'ssleay32.dll';
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
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_OP_NO_SSLv2 = $01000000;
  SSL_OP_NO_SSLv3 = $02000000;
  SSL_OP_NO_TLSv1 = $04000000;
  SSL_OP_ALL = $000FFFFF;

var
  SSLLibHandle: Integer = 0;
  SSLUtilHandle: Integer = 0;

// ssleay.dll
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
  SslGetVersion : function(ssl: PSSL):PChar cdecl = nil;
  SslGetPeerCertificate : function(ssl: PSSL):PX509 cdecl = nil;

// libeay.dll
  SslX509Free : procedure(x: PX509) cdecl = nil;
  SslX509NameOneline : function(a: PX509_NAME; buf: PChar; size: Integer):PChar cdecl = nil;
  SslX509GetSubjectName : function(a: PX509):PX509_NAME cdecl = nil;
  SslX509GetIssuerName : function(a: PX509):PX509_NAME cdecl = nil;
  SslX509NameHash : function(x: PX509_NAME):Cardinal cdecl = nil;
  SslX509Digest : function(data: PX509; _type: PEVP_MD; md: PChar; len: PInteger):Integer cdecl = nil;
  SslEvpMd5 : function:PEVP_MD cdecl = nil;

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
      SSLUtilHandle := HMODULE(dlopen(DLLUtilName, RTLD_GLOBAL));
{$ELSE}
      SSLLibHandle := LoadLibrary(PChar(DLLSSLName));
      if (SSLLibHandle = 0) then
        SSLLibHandle := LoadLibrary(PChar(DLLSSLName2));
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
        SslGetPeerCertificate := GetProcAddress(SSLLibHandle, PChar('SSL_get_peer_certificate'));
        SslGetVersion := GetProcAddress(SSLLibHandle, PChar('SSL_get_version'));

        SslX509Free := GetProcAddress(SSLUtilHandle, PChar('X509_free'));
        SslX509NameOneline := GetProcAddress(SSLUtilHandle, PChar('X509_NAME_oneline'));
        SslX509GetSubjectName := GetProcAddress(SSLUtilHandle, PChar('X509_get_subject_name'));
        SslX509GetIssuerName := GetProcAddress(SSLUtilHandle, PChar('X509_get_issuer_name'));
        SslX509NameHash := GetProcAddress(SSLUtilHandle, PChar('X509_NAME_hash'));
        SslX509Digest := GetProcAddress(SSLUtilHandle, PChar('X509_digest'));
        SslEvpMd5 := GetProcAddress(SSLUtilHandle, PChar('EVP_md5'));

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
