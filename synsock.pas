{==============================================================================|
| Project : Delphree - Synapse                                   | 003.001.003 |
|==============================================================================|
| Content: Socket Independent Platform Layer                                   |
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
| Portions created by Lukas Gebauer are Copyright (c)2001-2003.                |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{$IFNDEF LINUX}
//{$DEFINE WINSOCK1}
{Note about define WINSOCK1:
If you activate this compiler directive, then socket interface level 1.1 is
used instead default level 2.2. Level 2.2 is not available on old W95, however
you can install update.

On Linux is level 2.2 always used!
}
{$ENDIF}

//{$DEFINE FORCEOLDAPI}
{Note about define FORCEOLDAPI:
If you activate this compiler directive, then is allways used old socket API
for name resolution. If you leave this directive inactive, then when new API
is used, when running system allows it.

For IPv6 support you must have new API!
}

{$IFDEF VER125}
  {$DEFINE BCB}
{$ENDIF}
{$IFDEF BCB}
  {$ObjExportAll On}
  (*$HPPEMIT '/* EDE 2003-02-19 */' *)
  (*$HPPEMIT 'namespace Synsock { using System::Shortint; }' *)
  (*$HPPEMIT '#undef h_addr' *)
  (*$HPPEMIT '#undef IOCPARM_MASK' *)
  (*$HPPEMIT '#undef FD_SETSIZE' *)
  (*$HPPEMIT '#undef IOC_VOID' *)
  (*$HPPEMIT '#undef IOC_OUT' *)
  (*$HPPEMIT '#undef IOC_IN' *)
  (*$HPPEMIT '#undef IOC_INOUT' *)
  (*$HPPEMIT '#undef FIONREAD' *)
  (*$HPPEMIT '#undef FIONBIO' *)
  (*$HPPEMIT '#undef FIOASYNC' *)
  (*$HPPEMIT '#undef IPPROTO_IP' *)
  (*$HPPEMIT '#undef IPPROTO_ICMP' *)
  (*$HPPEMIT '#undef IPPROTO_IGMP' *)
  (*$HPPEMIT '#undef IPPROTO_TCP' *)
  (*$HPPEMIT '#undef IPPROTO_UDP' *)
  (*$HPPEMIT '#undef IPPROTO_RAW' *)
  (*$HPPEMIT '#undef IPPROTO_MAX' *)
  (*$HPPEMIT '#undef INADDR_ANY' *)
  (*$HPPEMIT '#undef INADDR_LOOPBACK' *)
  (*$HPPEMIT '#undef INADDR_BROADCAST' *)
  (*$HPPEMIT '#undef INADDR_NONE' *)
  (*$HPPEMIT '#undef INVALID_SOCKET' *)
  (*$HPPEMIT '#undef SOCKET_ERROR' *)
  (*$HPPEMIT '#undef WSADESCRIPTION_LEN' *)
  (*$HPPEMIT '#undef WSASYS_STATUS_LEN' *)
  (*$HPPEMIT '#undef IP_OPTIONS' *)
  (*$HPPEMIT '#undef IP_TOS' *)
  (*$HPPEMIT '#undef IP_TTL' *)
  (*$HPPEMIT '#undef IP_MULTICAST_IF' *)
  (*$HPPEMIT '#undef IP_MULTICAST_TTL' *)
  (*$HPPEMIT '#undef IP_MULTICAST_LOOP' *)
  (*$HPPEMIT '#undef IP_ADD_MEMBERSHIP' *)
  (*$HPPEMIT '#undef IP_DROP_MEMBERSHIP' *)
  (*$HPPEMIT '#undef IP_DONTFRAGMENT' *)
  (*$HPPEMIT '#undef IP_DEFAULT_MULTICAST_TTL' *)
  (*$HPPEMIT '#undef IP_DEFAULT_MULTICAST_LOOP' *)
  (*$HPPEMIT '#undef IP_MAX_MEMBERSHIPS' *)
  (*$HPPEMIT '#undef SOL_SOCKET' *)
  (*$HPPEMIT '#undef SO_DEBUG' *)
  (*$HPPEMIT '#undef SO_ACCEPTCONN' *)
  (*$HPPEMIT '#undef SO_REUSEADDR' *)
  (*$HPPEMIT '#undef SO_KEEPALIVE' *)
  (*$HPPEMIT '#undef SO_DONTROUTE' *)
  (*$HPPEMIT '#undef SO_BROADCAST' *)
  (*$HPPEMIT '#undef SO_USELOOPBACK' *)
  (*$HPPEMIT '#undef SO_LINGER' *)
  (*$HPPEMIT '#undef SO_OOBINLINE' *)
  (*$HPPEMIT '#undef SO_DONTLINGER' *)
  (*$HPPEMIT '#undef SO_SNDBUF' *)
  (*$HPPEMIT '#undef SO_RCVBUF' *)
  (*$HPPEMIT '#undef SO_SNDLOWAT' *)
  (*$HPPEMIT '#undef SO_RCVLOWAT' *)
  (*$HPPEMIT '#undef SO_SNDTIMEO' *)
  (*$HPPEMIT '#undef SO_RCVTIMEO' *)
  (*$HPPEMIT '#undef SO_ERROR' *)
  (*$HPPEMIT '#undef SO_OPENTYPE' *)
  (*$HPPEMIT '#undef SO_SYNCHRONOUS_ALERT' *)
  (*$HPPEMIT '#undef SO_SYNCHRONOUS_NONALERT' *)
  (*$HPPEMIT '#undef SO_MAXDG' *)
  (*$HPPEMIT '#undef SO_MAXPATHDG' *)
  (*$HPPEMIT '#undef SO_UPDATE_ACCEPT_CONTEXT' *)
  (*$HPPEMIT '#undef SO_CONNECT_TIME' *)
  (*$HPPEMIT '#undef SO_TYPE' *)
  (*$HPPEMIT '#undef SOCK_STREAM' *)
  (*$HPPEMIT '#undef SOCK_DGRAM' *)
  (*$HPPEMIT '#undef SOCK_RAW' *)
  (*$HPPEMIT '#undef SOCK_RDM' *)
  (*$HPPEMIT '#undef SOCK_SEQPACKET' *)
  (*$HPPEMIT '#undef TCP_NODELAY' *)
  (*$HPPEMIT '#undef AF_UNSPEC' *)
  (*$HPPEMIT '#undef SOMAXCONN' *)
  (*$HPPEMIT '#undef AF_INET' *)
  (*$HPPEMIT '#undef AF_MAX' *)
  (*$HPPEMIT '#undef PF_UNSPEC' *)
  (*$HPPEMIT '#undef PF_INET' *)
  (*$HPPEMIT '#undef PF_MAX' *)
  (*$HPPEMIT '#undef MSG_OOB' *)
  (*$HPPEMIT '#undef MSG_PEEK' *)
  (*$HPPEMIT '#undef WSABASEERR' *)
  (*$HPPEMIT '#undef WSAEINTR' *)
  (*$HPPEMIT '#undef WSAEBADF' *)
  (*$HPPEMIT '#undef WSAEACCES' *)
  (*$HPPEMIT '#undef WSAEFAULT' *)
  (*$HPPEMIT '#undef WSAEINVAL' *)
  (*$HPPEMIT '#undef WSAEMFILE' *)
  (*$HPPEMIT '#undef WSAEWOULDBLOCK' *)
  (*$HPPEMIT '#undef WSAEINPROGRESS' *)
  (*$HPPEMIT '#undef WSAEALREADY' *)
  (*$HPPEMIT '#undef WSAENOTSOCK' *)
  (*$HPPEMIT '#undef WSAEDESTADDRREQ' *)
  (*$HPPEMIT '#undef WSAEMSGSIZE' *)
  (*$HPPEMIT '#undef WSAEPROTOTYPE' *)
  (*$HPPEMIT '#undef WSAENOPROTOOPT' *)
  (*$HPPEMIT '#undef WSAEPROTONOSUPPORT' *)
  (*$HPPEMIT '#undef WSAESOCKTNOSUPPORT' *)
  (*$HPPEMIT '#undef WSAEOPNOTSUPP' *)
  (*$HPPEMIT '#undef WSAEPFNOSUPPORT' *)
  (*$HPPEMIT '#undef WSAEAFNOSUPPORT' *)
  (*$HPPEMIT '#undef WSAEADDRINUSE' *)
  (*$HPPEMIT '#undef WSAEADDRNOTAVAIL' *)
  (*$HPPEMIT '#undef WSAENETDOWN' *)
  (*$HPPEMIT '#undef WSAENETUNREACH' *)
  (*$HPPEMIT '#undef WSAENETRESET' *)
  (*$HPPEMIT '#undef WSAECONNABORTED' *)
  (*$HPPEMIT '#undef WSAECONNRESET' *)
  (*$HPPEMIT '#undef WSAENOBUFS' *)
  (*$HPPEMIT '#undef WSAEISCONN' *)
  (*$HPPEMIT '#undef WSAENOTCONN' *)
  (*$HPPEMIT '#undef WSAESHUTDOWN' *)
  (*$HPPEMIT '#undef WSAETOOMANYREFS' *)
  (*$HPPEMIT '#undef WSAETIMEDOUT' *)
  (*$HPPEMIT '#undef WSAECONNREFUSED' *)
  (*$HPPEMIT '#undef WSAELOOP' *)
  (*$HPPEMIT '#undef WSAENAMETOOLONG' *)
  (*$HPPEMIT '#undef WSAEHOSTDOWN' *)
  (*$HPPEMIT '#undef WSAEHOSTUNREACH' *)
  (*$HPPEMIT '#undef WSAENOTEMPTY' *)
  (*$HPPEMIT '#undef WSAEPROCLIM' *)
  (*$HPPEMIT '#undef WSAEUSERS' *)
  (*$HPPEMIT '#undef WSAEDQUOT' *)
  (*$HPPEMIT '#undef WSAESTALE' *)
  (*$HPPEMIT '#undef WSAEREMOTE' *)
  (*$HPPEMIT '#undef WSASYSNOTREADY' *)
  (*$HPPEMIT '#undef WSAVERNOTSUPPORTED' *)
  (*$HPPEMIT '#undef WSANOTINITIALISED' *)
  (*$HPPEMIT '#undef WSAEDISCON' *)
  (*$HPPEMIT '#undef WSAENOMORE' *)
  (*$HPPEMIT '#undef WSAECANCELLED' *)
  (*$HPPEMIT '#undef WSAEEINVALIDPROCTABLE' *)
  (*$HPPEMIT '#undef WSAEINVALIDPROVIDER' *)
  (*$HPPEMIT '#undef WSAEPROVIDERFAILEDINIT' *)
  (*$HPPEMIT '#undef WSASYSCALLFAILURE' *)
  (*$HPPEMIT '#undef WSASERVICE_NOT_FOUND' *)
  (*$HPPEMIT '#undef WSATYPE_NOT_FOUND' *)
  (*$HPPEMIT '#undef WSA_E_NO_MORE' *)
  (*$HPPEMIT '#undef WSA_E_CANCELLED' *)
  (*$HPPEMIT '#undef WSAEREFUSED' *)
  (*$HPPEMIT '#undef WSAHOST_NOT_FOUND' *)
  (*$HPPEMIT '#undef HOST_NOT_FOUND' *)
  (*$HPPEMIT '#undef WSATRY_AGAIN' *)
  (*$HPPEMIT '#undef TRY_AGAIN' *)
  (*$HPPEMIT '#undef WSANO_RECOVERY' *)
  (*$HPPEMIT '#undef NO_RECOVERY' *)
  (*$HPPEMIT '#undef WSANO_DATA' *)
  (*$HPPEMIT '#undef NO_DATA' *)
  (*$HPPEMIT '#undef WSANO_ADDRESS' *)
  (*$HPPEMIT '#undef ENAMETOOLONG' *)
  (*$HPPEMIT '#undef ENOTEMPTY' *)
  (*$HPPEMIT '#undef FD_CLR' *)
  (*$HPPEMIT '#undef FD_ISSET' *)
  (*$HPPEMIT '#undef FD_SET' *)
  (*$HPPEMIT '#undef FD_ZERO' *)
  (*$HPPEMIT '#undef NO_ADDRESS' *)
{$ENDIF}


unit synsock;

{$MINENUMSIZE 4}

interface

uses
  SyncObjs, SysUtils,
{$IFDEF LINUX}
  Libc;
{$ELSE}
  Windows;
{$ENDIF}

function InitSocketInterface(stack: string): Boolean;
function DestroySocketInterface: Boolean;

const
{$IFDEF WINSOCK1}
  WinsockLevel = $0101;
{$ELSE}
  WinsockLevel = $0202;
{$ENDIF}

type
  u_char = Char;
  u_short = Word;
  u_int = Integer;
  u_long = Longint;
  pu_long = ^u_long;
  pu_short = ^u_short;
  TSocket = u_int;

{$IFDEF LINUX}
type
  DWORD = Integer;
  __fd_mask = LongWord;
const
  __FD_SETSIZE    = 1024;
  __NFDBITS       = 8 * sizeof(__fd_mask);
type
  __fd_set = {packed} record
    fds_bits: packed array[0..(__FD_SETSIZE div __NFDBITS)-1] of __fd_mask;
  end;
  TFDSet = __fd_set;
  PFDSet = ^TFDSet;

const
  FIONREAD        = $541B;
  FIONBIO         = $5421;
  FIOASYNC        = $5452;

{$ELSE}
const
  FD_SETSIZE     =   64;
type
  PFDSet = ^TFDSet;
  TFDSet = packed record
    fd_count: u_int;
    fd_array: array[0..FD_SETSIZE-1] of TSocket;
  end;

const
  IOCPARM_MASK = $7f;
  IOC_VOID     = $20000000;
  IOC_OUT      = $40000000;
  IOC_IN       = $80000000;
  IOC_INOUT    = (IOC_IN or IOC_OUT);
  FIONREAD     = IOC_OUT or { get # bytes to read }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 127;
  FIONBIO      = IOC_IN or { set/clear non-blocking i/o }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 126;
  FIOASYNC     = IOC_IN or { set/clear async i/o }
    ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
    (Longint(Byte('f')) shl 8) or 125;

{$ENDIF}

type
  PTimeVal = ^TTimeVal;
  TTimeVal = packed record
    tv_sec: Longint;
    tv_usec: Longint;
  end;

const
  IPPROTO_IP     =   0;		{ Dummy					}
  IPPROTO_ICMP   =   1;		{ Internet Control Message Protocol }
  IPPROTO_IGMP   =   2;		{ Internet Group Management Protocol}
  IPPROTO_TCP    =   6;		{ TCP           			}
  IPPROTO_UDP    =   17;	{ User Datagram Protocol		}
  IPPROTO_IPV6   =   41;
  IPPROTO_ICMPV6 =   58;

  IPPROTO_RAW    =   255;
  IPPROTO_MAX    =   256;

type
  SunB = packed record
    s_b1, s_b2, s_b3, s_b4: u_char;
  end;

  SunW = packed record
    s_w1, s_w2: u_short;
  end;

  PInAddr = ^TInAddr;
  TInAddr = packed record
    case integer of
      0: (S_un_b: SunB);
      1: (S_un_w: SunW);
      2: (S_addr: u_long);
  end;

  PSockAddrIn = ^TSockAddrIn;
  TSockAddrIn = packed record
    case Integer of
      0: (sin_family: u_short;
          sin_port: u_short;
          sin_addr: TInAddr;
          sin_zero: array[0..7] of Char);
      1: (sa_family: u_short;
          sa_data: array[0..13] of Char)
  end;

  TIP_mreq =  record
    imr_multiaddr: TInAddr;     { IP multicast address of group }
    imr_interface: TInAddr;     { local IP address of interface }
  end;

  SunB6 = packed record
    s_b1,  s_b2,  s_b3,  s_b4,
    s_b5,  s_b6,  s_b7,  s_b8,
    s_b9,  s_b10, s_b11, s_b12,
    s_b13, s_b14, s_b15, s_b16: u_char;
  end;

  SunW6 = packed record
    s_w1, s_w2, s_w3, s_w4,
    s_w5, s_w6, s_w7, s_w8: u_short;
  end;

  SunDW6 = packed record
    s_dw1, s_dw2, s_dw3, s_dw4: longint;
  end;

  S6_Bytes   = SunB6;
  S6_Words   = SunW6;
  S6_DWords  = SunDW6;
  S6_Addr    = SunB6;

  PInAddr6 = ^TInAddr6;
  TInAddr6 = packed record
    case integer of
      0: (S_un_b:  SunB6);
      1: (S_un_w:  SunW6);
      2: (S_un_dw: SunDW6);
  end;

  PSockAddrIn6 = ^TSockAddrIn6;
  TSockAddrIn6 = packed record
		sin6_family:   u_short;     // AF_INET6
		sin6_port:     u_short;     // Transport level port number
		sin6_flowinfo: u_long;	    // IPv6 flow information
		sin6_addr:     TInAddr6;    // IPv6 address
		sin6_scope_id: u_long;      // Scope Id: IF number for link-local
                                //           SITE id for site-local
  end;

  TIPv6_mreq = record
    ipv6mr_multiaddr: TInAddr6; // IPv6 multicast address.
    ipv6mr_interface: u_long;   // Interface index.
    padding: u_long;
  end;


{$IFDEF LINUX}
  hostent = record
    h_name: PChar;
    h_aliases: PPChar;
    h_addrtype: Integer;
    h_length: Cardinal;
    case Byte of
      0: (h_addr_list: PPChar);
      1: (h_addr: PPChar);
  end;

  PNetEnt = ^TNetEnt;
  TNetEnt = record
    n_name: PChar;
    n_aliases: PPChar;
    n_addrtype: Integer;
    n_net: uint32_t;
  end;

  PServEnt = ^TServEnt;
  TServEnt = record
    s_name: PChar;
    s_aliases: PPChar;
    s_port: Integer;
    s_proto: PChar;
  end;

  PProtoEnt = ^TProtoEnt;
  TProtoEnt = record
    p_name: PChar;
    p_aliases: ^PChar;
    p_proto: u_short;
  end;

{$ELSE}
  PHostEnt = ^THostEnt;
  THostEnt = packed record
    h_name: PChar;
    h_aliases: ^PChar;
    h_addrtype: Smallint;
    h_length: Smallint;
    case integer of
     0: (h_addr_list: ^PChar);
     1: (h_addr: ^PInAddr);
  end;

  PNetEnt = ^TNetEnt;
  TNetEnt = packed record
    n_name: PChar;
    n_aliases: ^PChar;
    n_addrtype: Smallint;
    n_net: u_long;
  end;

  PServEnt = ^TServEnt;
  TServEnt = packed record
    s_name: PChar;
    s_aliases: ^PChar;
    s_port: Smallint;
    s_proto: PChar;
  end;

  PProtoEnt = ^TProtoEnt;
  TProtoEnt = packed record
    p_name: PChar;
    p_aliases: ^Pchar;
    p_proto: Smallint;
  end;
{$ENDIF}

const
  INADDR_ANY       = $00000000;
  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = $FFFFFFFF;
  INADDR_NONE      = $FFFFFFFF;
  ADDR_ANY		 = INADDR_ANY;
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

  function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
  function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
  function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6):boolean;
  procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
  procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
var
  in6addr_any, in6addr_loopback : TInAddr6;

const
  WSADESCRIPTION_LEN     =   256;
  WSASYS_STATUS_LEN      =   128;
type
  PWSAData = ^TWSAData;
  TWSAData = packed record
    wVersion: Word;
    wHighVersion: Word;
    szDescription: array[0..WSADESCRIPTION_LEN] of Char;
    szSystemStatus: array[0..WSASYS_STATUS_LEN] of Char;
    iMaxSockets: Word;
    iMaxUdpDg: Word;
    lpVendorInfo: PChar;
  end;

{=============================================================================}
{$IFDEF LINUX}
Const
  IP_TOS             = 1;  { int; IP type of service and precedence.  }
  IP_TTL             = 2;  { int; IP time to live.  }
  IP_HDRINCL         = 3;  { int; Header is included with data.  }
  IP_OPTIONS         = 4;  { ip_opts; IP per-packet options.  }
  IP_ROUTER_ALERT    = 5;  { bool }
  IP_RECVOPTS        = 6;  { bool }
  IP_RETOPTS         = 7;  { bool }
  IP_PKTINFO         = 8;  { bool }
  IP_PKTOPTIONS      = 9;
  IP_PMTUDISC        = 10; { obsolete name? }
  IP_MTU_DISCOVER    = 10; { int; see below }
  IP_RECVERR         = 11; { bool }
  IP_RECVTTL         = 12; { bool }
  IP_RECVTOS         = 13; { bool }
  IP_MULTICAST_IF    = 32; { in_addr; set/get IP multicast i/f }
  IP_MULTICAST_TTL   = 33; { u_char; set/get IP multicast ttl }
  IP_MULTICAST_LOOP  = 34; { i_char; set/get IP multicast loopback }
  IP_ADD_MEMBERSHIP  = 35; { ip_mreq; add an IP group membership }
  IP_DROP_MEMBERSHIP = 36; { ip_mreq; drop an IP group membership }

  SOL_SOCKET    = 1;

  SO_DEBUG      = 1;
  SO_REUSEADDR  = 2;
  SO_TYPE       = 3;
  SO_ERROR      = 4;
  SO_DONTROUTE  = 5;
  SO_BROADCAST  = 6;
  SO_SNDBUF     = 7;
  SO_RCVBUF     = 8;
  SO_KEEPALIVE  = 9;
  SO_OOBINLINE  = 10;
  SO_NO_CHECK   = 11;
  SO_PRIORITY   = 12;
  SO_LINGER     = 13;
  SO_BSDCOMPAT  = 14;
  SO_REUSEPORT  = 15;
  SO_PASSCRED   = 16;
  SO_PEERCRED   = 17;
  SO_RCVLOWAT   = 18;
  SO_SNDLOWAT   = 19;
  SO_RCVTIMEO   = 20;
  SO_SNDTIMEO   = 21;
{ Security levels - as per NRL IPv6 - don't actually do anything }
  SO_SECURITY_AUTHENTICATION       = 22;
  SO_SECURITY_ENCRYPTION_TRANSPORT = 23;
  SO_SECURITY_ENCRYPTION_NETWORK   = 24;
  SO_BINDTODEVICE                  = 25;
{ Socket filtering }
  SO_ATTACH_FILTER = 26;
  SO_DETACH_FILTER = 27;

  SOMAXCONN       = 128;

  IPV6_UNICAST_HOPS     = 16;
  IPV6_MULTICAST_IF     = 17;
  IPV6_MULTICAST_HOPS   = 18;
  IPV6_MULTICAST_LOOP   = 19;
  IPV6_JOIN_GROUP       = 20;
  IPV6_LEAVE_GROUP      = 21;

  MSG_NOSIGNAL  = $4000;                // Do not generate SIGPIPE.

{=============================================================================}
{$ELSE}
Const
  {$IFDEF WINSOCK1}
    IP_OPTIONS          = 1;
    IP_MULTICAST_IF     = 2;           { set/get IP multicast interface   }
    IP_MULTICAST_TTL    = 3;           { set/get IP multicast timetolive  }
    IP_MULTICAST_LOOP   = 4;           { set/get IP multicast loopback    }
    IP_ADD_MEMBERSHIP   = 5;           { add  an IP group membership      }
    IP_DROP_MEMBERSHIP  = 6;           { drop an IP group membership      }
    IP_TTL              = 7;           { set/get IP Time To Live          }
    IP_TOS              = 8;           { set/get IP Type Of Service       }
    IP_DONTFRAGMENT     = 9;           { set/get IP Don't Fragment flag   }
  {$ELSE}
    IP_OPTIONS          = 1;
    IP_HDRINCL          = 2;
    IP_TOS              = 3;           { set/get IP Type Of Service       }
    IP_TTL              = 4;           { set/get IP Time To Live          }
    IP_MULTICAST_IF     = 9;           { set/get IP multicast interface   }
    IP_MULTICAST_TTL    = 10;           { set/get IP multicast timetolive  }
    IP_MULTICAST_LOOP   = 11;           { set/get IP multicast loopback    }
    IP_ADD_MEMBERSHIP   = 12;           { add  an IP group membership      }
    IP_DROP_MEMBERSHIP  = 13;           { drop an IP group membership      }
    IP_DONTFRAGMENT     = 14;           { set/get IP Don't Fragment flag   }
  {$ENDIF}

  IP_DEFAULT_MULTICAST_TTL   = 1;    { normally limit m'casts to 1 hop  }
  IP_DEFAULT_MULTICAST_LOOP  = 1;    { normally hear sends if a member  }
  IP_MAX_MEMBERSHIPS         = 20;   { per socket; must fit in one mbuf }

  SOL_SOCKET      = $ffff;          {options for socket level }
{ Option flags per-socket. }
  SO_DEBUG        = $0001;          { turn on debugging info recording }
  SO_ACCEPTCONN   = $0002;          { socket has had listen() }
  SO_REUSEADDR    = $0004;          { allow local address reuse }
  SO_KEEPALIVE    = $0008;          { keep connections alive }
  SO_DONTROUTE    = $0010;          { just use interface addresses }
  SO_BROADCAST    = $0020;          { permit sending of broadcast msgs }
  SO_USELOOPBACK  = $0040;          { bypass hardware when possible }
  SO_LINGER       = $0080;          { linger on close if data present }
  SO_OOBINLINE    = $0100;          { leave received OOB data in line }
  SO_DONTLINGER  =   $ff7f;
{ Additional options. }
  SO_SNDBUF       = $1001;          { send buffer size }
  SO_RCVBUF       = $1002;          { receive buffer size }
  SO_SNDLOWAT     = $1003;          { send low-water mark }
  SO_RCVLOWAT     = $1004;          { receive low-water mark }
  SO_SNDTIMEO     = $1005;          { send timeout }
  SO_RCVTIMEO     = $1006;          { receive timeout }
  SO_ERROR        = $1007;          { get error status and clear }
  SO_TYPE         = $1008;          { get socket type }
{ WinSock 2 extension -- new options }
  SO_GROUP_ID       = $2001; { ID of a socket group}
  SO_GROUP_PRIORITY = $2002; { the relative priority within a group}
  SO_MAX_MSG_SIZE   = $2003; { maximum message size }
  SO_PROTOCOL_INFOA = $2004; { WSAPROTOCOL_INFOA structure }
  SO_PROTOCOL_INFOW = $2005; { WSAPROTOCOL_INFOW structure }
  SO_PROTOCOL_INFO  = SO_PROTOCOL_INFOA;
  PVD_CONFIG        = $3001; {configuration info for service provider }
{ Option for opening sockets for synchronous access. }
  SO_OPENTYPE     = $7008;
  SO_SYNCHRONOUS_ALERT    = $10;
  SO_SYNCHRONOUS_NONALERT = $20;
{ Other NT-specific options. }
  SO_MAXDG        = $7009;
  SO_MAXPATHDG    = $700A;
  SO_UPDATE_ACCEPT_CONTEXT     = $700B;
  SO_CONNECT_TIME = $700C;

  SOMAXCONN       = $7fffffff;

  IPV6_UNICAST_HOPS      = 8;  // ???
  IPV6_MULTICAST_IF      = 9;  // set/get IP multicast i/f
  IPV6_MULTICAST_HOPS    = 10; // set/get IP multicast ttl
  IPV6_MULTICAST_LOOP    = 11; // set/get IP multicast loopback
  IPV6_JOIN_GROUP        = 12; // add an IP group membership
  IPV6_LEAVE_GROUP       = 13; // drop an IP group membership

  MSG_NOSIGNAL  = 0;

{$ENDIF}
{=============================================================================}

const
  SOCK_STREAM     = 1;               { stream socket }
  SOCK_DGRAM      = 2;               { datagram socket }
  SOCK_RAW        = 3;               { raw-protocol interface }
  SOCK_RDM        = 4;               { reliably-delivered message }
  SOCK_SEQPACKET  = 5;               { sequenced packet stream }

{ TCP options. }
  TCP_NODELAY     = $0001;

{ Address families. }

  AF_UNSPEC       = 0;               { unspecified }
  AF_INET         = 2;               { internetwork: UDP, TCP, etc. }
{$IFDEF LINUX}
  AF_INET6        = 10;              { Internetwork Version 6 }
{$ELSE}
  AF_INET6        = 23;              { Internetwork Version 6 }
{$ENDIF}
  AF_MAX          = 24;

{ Protocol families, same as address families for now. }
  PF_UNSPEC       = AF_UNSPEC;
  PF_INET         = AF_INET;
  PF_INET6        = AF_INET6;
  PF_MAX          = AF_MAX;

type
  { Structure used by kernel to store most addresses. }

  PSockAddr = ^TSockAddr;
  TSockAddr = TSockAddrIn;

  { Structure used by kernel to pass protocol information in raw sockets. }
  PSockProto = ^TSockProto;
  TSockProto = packed record
    sp_family: u_short;
    sp_protocol: u_short;
  end;

type
  PAddrInfo = ^TAddrInfo;
  TAddrInfo = record
                ai_flags: integer;    // AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST.
                ai_family: integer;   // PF_xxx.
                ai_socktype: integer; // SOCK_xxx.
                ai_protocol: integer; // 0 or IPPROTO_xxx for IPv4 and IPv6.
                ai_addrlen: u_int;    // Length of ai_addr.
              {$IFDEF LINUX}          // broken definition in LIBC??? :-O
                ai_addr: PSockAddr;   // Binary address.
                ai_canonname: PChar;  // Canonical name for nodename.
              {$ELSE}
                ai_canonname: PChar;  // Canonical name for nodename.
                ai_addr: PSockAddr;   // Binary address.
              {$ENDIF}
                ai_next: PAddrInfo;     // Next structure in linked list.
              end;

const
  // Flags used in "hints" argument to getaddrinfo().
  AI_PASSIVE     = $1;  // Socket address will be used in bind() call.
  AI_CANONNAME   = $2;  // Return canonical name in first ai_canonname.
  AI_NUMERICHOST = $4;  // Nodename must be a numeric address string.

  // getnameinfo constants
  NI_MAXHOST	   = 1025;
  NI_MAXSERV	   = 32;
  NI_NOFQDN 	   = $1;
  NI_NUMERICHOST = $2;
  NI_NAMEREQD	   = $4;
  NI_NUMERICSERV = $8;
  NI_DGRAM       = $10;

type
{ Structure used for manipulating linger option. }
  PLinger = ^TLinger;
  TLinger = packed record
    l_onoff: u_short;
    l_linger: u_short;
  end;

const

{ Define constant based on rfc883, used by gethostbyxxxx() calls. }

  MSG_OOB       = $01;                  // Process out-of-band data.
  MSG_PEEK      = $02;                  // Peek at incoming messages.

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);

{=============================================================================}
{$IFDEF LINUX}
const
  WSAEINTR = EINTR;
  WSAEBADF = EBADF;
  WSAEACCES = EACCES;
  WSAEFAULT = EFAULT;
  WSAEINVAL = EINVAL;
  WSAEMFILE = EMFILE;
  WSAEWOULDBLOCK = EWOULDBLOCK;
  WSAEINPROGRESS = EINPROGRESS;
  WSAEALREADY = EALREADY;
  WSAENOTSOCK = ENOTSOCK;
  WSAEDESTADDRREQ = EDESTADDRREQ;
  WSAEMSGSIZE = EMSGSIZE;
  WSAEPROTOTYPE = EPROTOTYPE;
  WSAENOPROTOOPT = ENOPROTOOPT;
  WSAEPROTONOSUPPORT = EPROTONOSUPPORT;
  WSAESOCKTNOSUPPORT = ESOCKTNOSUPPORT;
  WSAEOPNOTSUPP = EOPNOTSUPP;
  WSAEPFNOSUPPORT = EPFNOSUPPORT;
  WSAEAFNOSUPPORT = EAFNOSUPPORT;
  WSAEADDRINUSE = EADDRINUSE;
  WSAEADDRNOTAVAIL = EADDRNOTAVAIL;
  WSAENETDOWN = ENETDOWN;
  WSAENETUNREACH = ENETUNREACH;
  WSAENETRESET = ENETRESET;
  WSAECONNABORTED = ECONNABORTED;
  WSAECONNRESET = ECONNRESET;
  WSAENOBUFS = ENOBUFS;
  WSAEISCONN = EISCONN;
  WSAENOTCONN = ENOTCONN;
  WSAESHUTDOWN = ESHUTDOWN;
  WSAETOOMANYREFS = ETOOMANYREFS;
  WSAETIMEDOUT = ETIMEDOUT;
  WSAECONNREFUSED = ECONNREFUSED;
  WSAELOOP = ELOOP;
  WSAENAMETOOLONG = ENAMETOOLONG;
  WSAEHOSTDOWN = EHOSTDOWN;
  WSAEHOSTUNREACH = EHOSTUNREACH;
  WSAENOTEMPTY = ENOTEMPTY;
  WSAEPROCLIM = -1;
  WSAEUSERS = EUSERS;
  WSAEDQUOT = EDQUOT;
  WSAESTALE = ESTALE;
  WSAEREMOTE = EREMOTE;
  WSASYSNOTREADY = -2;
  WSAVERNOTSUPPORTED = -3;
  WSANOTINITIALISED = -4;
  WSAEDISCON = -5;
  WSAHOST_NOT_FOUND = HOST_NOT_FOUND;
  WSATRY_AGAIN = TRY_AGAIN;
  WSANO_RECOVERY = NO_RECOVERY;
  WSANO_DATA = -6;

  EAI_BADFLAGS    = -1;   { Invalid value for `ai_flags' field.  }
  EAI_NONAME      = -2;   { NAME or SERVICE is unknown.  }
  EAI_AGAIN       = -3;   { Temporary failure in name resolution.  }
  EAI_FAIL        = -4;   { Non-recoverable failure in name res.  }
  EAI_NODATA      = -5;   { No address associated with NAME.  }
  EAI_FAMILY      = -6;   { `ai_family' not supported.  }
  EAI_SOCKTYPE    = -7;   { `ai_socktype' not supported.  }
  EAI_SERVICE     = -8;   { SERVICE not supported for `ai_socktype'.  }
  EAI_ADDRFAMILY  = -9;   { Address family for NAME not supported.  }
  EAI_MEMORY      = -10;  { Memory allocation failure.  }
  EAI_SYSTEM      = -11;  { System error returned in `errno'.  }

{$ELSE}

const

{ All Windows Sockets error constants are biased by WSABASEERR from the "normal" }
  WSABASEERR              = 10000;

{ Windows Sockets definitions of regular Microsoft C error constants }

  WSAEINTR                = (WSABASEERR+4);
  WSAEBADF                = (WSABASEERR+9);
  WSAEACCES               = (WSABASEERR+13);
  WSAEFAULT               = (WSABASEERR+14);
  WSAEINVAL               = (WSABASEERR+22);
  WSAEMFILE               = (WSABASEERR+24);

{ Windows Sockets definitions of regular Berkeley error constants }

  WSAEWOULDBLOCK          = (WSABASEERR+35);
  WSAEINPROGRESS          = (WSABASEERR+36);
  WSAEALREADY             = (WSABASEERR+37);
  WSAENOTSOCK             = (WSABASEERR+38);
  WSAEDESTADDRREQ         = (WSABASEERR+39);
  WSAEMSGSIZE             = (WSABASEERR+40);
  WSAEPROTOTYPE           = (WSABASEERR+41);
  WSAENOPROTOOPT          = (WSABASEERR+42);
  WSAEPROTONOSUPPORT      = (WSABASEERR+43);
  WSAESOCKTNOSUPPORT      = (WSABASEERR+44);
  WSAEOPNOTSUPP           = (WSABASEERR+45);
  WSAEPFNOSUPPORT         = (WSABASEERR+46);
  WSAEAFNOSUPPORT         = (WSABASEERR+47);
  WSAEADDRINUSE           = (WSABASEERR+48);
  WSAEADDRNOTAVAIL        = (WSABASEERR+49);
  WSAENETDOWN             = (WSABASEERR+50);
  WSAENETUNREACH          = (WSABASEERR+51);
  WSAENETRESET            = (WSABASEERR+52);
  WSAECONNABORTED         = (WSABASEERR+53);
  WSAECONNRESET           = (WSABASEERR+54);
  WSAENOBUFS              = (WSABASEERR+55);
  WSAEISCONN              = (WSABASEERR+56);
  WSAENOTCONN             = (WSABASEERR+57);
  WSAESHUTDOWN            = (WSABASEERR+58);
  WSAETOOMANYREFS         = (WSABASEERR+59);
  WSAETIMEDOUT            = (WSABASEERR+60);
  WSAECONNREFUSED         = (WSABASEERR+61);
  WSAELOOP                = (WSABASEERR+62);
  WSAENAMETOOLONG         = (WSABASEERR+63);
  WSAEHOSTDOWN            = (WSABASEERR+64);
  WSAEHOSTUNREACH         = (WSABASEERR+65);
  WSAENOTEMPTY            = (WSABASEERR+66);
  WSAEPROCLIM             = (WSABASEERR+67);
  WSAEUSERS               = (WSABASEERR+68);
  WSAEDQUOT               = (WSABASEERR+69);
  WSAESTALE               = (WSABASEERR+70);
  WSAEREMOTE              = (WSABASEERR+71);

{ Extended Windows Sockets error constant definitions }

  WSASYSNOTREADY          = (WSABASEERR+91);
  WSAVERNOTSUPPORTED      = (WSABASEERR+92);
  WSANOTINITIALISED       = (WSABASEERR+93);
  WSAEDISCON              = (WSABASEERR+101);
  WSAENOMORE              = (WSABASEERR+102);
  WSAECANCELLED           = (WSABASEERR+103);
  WSAEEINVALIDPROCTABLE   = (WSABASEERR+104);
  WSAEINVALIDPROVIDER     = (WSABASEERR+105);
  WSAEPROVIDERFAILEDINIT  = (WSABASEERR+106);
  WSASYSCALLFAILURE       = (WSABASEERR+107);
  WSASERVICE_NOT_FOUND    = (WSABASEERR+108);
  WSATYPE_NOT_FOUND       = (WSABASEERR+109);
  WSA_E_NO_MORE           = (WSABASEERR+110);
  WSA_E_CANCELLED         = (WSABASEERR+111);
  WSAEREFUSED             = (WSABASEERR+112);

{ Error return codes from gethostbyname() and gethostbyaddr()
  (when using the resolver). Note that these errors are
  retrieved via WSAGetLastError() and must therefore follow
  the rules for avoiding clashes with error numbers from
  specific implementations or language run-time systems.
  For this reason the codes are based at WSABASEERR+1001.
  Note also that [WSA]NO_ADDRESS is defined only for
  compatibility purposes. }

{ Authoritative Answer: Host not found }
  WSAHOST_NOT_FOUND       = (WSABASEERR+1001);
  HOST_NOT_FOUND          = WSAHOST_NOT_FOUND;
{ Non-Authoritative: Host not found, or SERVERFAIL }
  WSATRY_AGAIN            = (WSABASEERR+1002);
  TRY_AGAIN               = WSATRY_AGAIN;
{ Non recoverable errors, FORMERR, REFUSED, NOTIMP }
  WSANO_RECOVERY          = (WSABASEERR+1003);
  NO_RECOVERY             = WSANO_RECOVERY;
{ Valid name, no data record of requested type }
  WSANO_DATA              = (WSABASEERR+1004);
  NO_DATA                 = WSANO_DATA;
{ no address, look for MX record }
  WSANO_ADDRESS           = WSANO_DATA;
  NO_ADDRESS              = WSANO_ADDRESS;

  EWOULDBLOCK        =  WSAEWOULDBLOCK;
  EINPROGRESS        =  WSAEINPROGRESS;
  EALREADY           =  WSAEALREADY;
  ENOTSOCK           =  WSAENOTSOCK;
  EDESTADDRREQ       =  WSAEDESTADDRREQ;
  EMSGSIZE           =  WSAEMSGSIZE;
  EPROTOTYPE         =  WSAEPROTOTYPE;
  ENOPROTOOPT        =  WSAENOPROTOOPT;
  EPROTONOSUPPORT    =  WSAEPROTONOSUPPORT;
  ESOCKTNOSUPPORT    =  WSAESOCKTNOSUPPORT;
  EOPNOTSUPP         =  WSAEOPNOTSUPP;
  EPFNOSUPPORT       =  WSAEPFNOSUPPORT;
  EAFNOSUPPORT       =  WSAEAFNOSUPPORT;
  EADDRINUSE         =  WSAEADDRINUSE;
  EADDRNOTAVAIL      =  WSAEADDRNOTAVAIL;
  ENETDOWN           =  WSAENETDOWN;
  ENETUNREACH        =  WSAENETUNREACH;
  ENETRESET          =  WSAENETRESET;
  ECONNABORTED       =  WSAECONNABORTED;
  ECONNRESET         =  WSAECONNRESET;
  ENOBUFS            =  WSAENOBUFS;
  EISCONN            =  WSAEISCONN;
  ENOTCONN           =  WSAENOTCONN;
  ESHUTDOWN          =  WSAESHUTDOWN;
  ETOOMANYREFS       =  WSAETOOMANYREFS;
  ETIMEDOUT          =  WSAETIMEDOUT;
  ECONNREFUSED       =  WSAECONNREFUSED;
  ELOOP              =  WSAELOOP;
  ENAMETOOLONG       =  WSAENAMETOOLONG;
  EHOSTDOWN          =  WSAEHOSTDOWN;
  EHOSTUNREACH       =  WSAEHOSTUNREACH;
  ENOTEMPTY          =  WSAENOTEMPTY;
  EPROCLIM           =  WSAEPROCLIM;
  EUSERS             =  WSAEUSERS;
  EDQUOT             =  WSAEDQUOT;
  ESTALE             =  WSAESTALE;
  EREMOTE            =  WSAEREMOTE;

  EAI_ADDRFAMILY  = 1;   // Address family for nodename not supported.
  EAI_AGAIN       = 2;   // Temporary failure in name resolution.
  EAI_BADFLAGS    = 3;   // Invalid value for ai_flags.
  EAI_FAIL        = 4;   // Non-recoverable failure in name resolution.
  EAI_FAMILY      = 5;   // Address family ai_family not supported.
  EAI_MEMORY      = 6;   // Memory allocation failure.
  EAI_NODATA      = 7;   // No address associated with nodename.
  EAI_NONAME      = 8;   // Nodename nor servname provided, or not known.
  EAI_SERVICE     = 9;   // Servname not supported for ai_socktype.
  EAI_SOCKTYPE    = 10;  // Socket type ai_socktype not supported.
  EAI_SYSTEM      = 11;  // System error returned in errno.

{$ENDIF}

{=============================================================================}
var
  WSAStartup: function(wVersionRequired: Word; var WSData: TWSAData): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  WSACleanup: function: Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  WSAGetLastError: function: Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetServByName: function(name, proto: PChar): PServEnt
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetServByPort: function(port: Integer; proto: PChar): PServEnt
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetProtoByName: function(name: PChar): PProtoEnt
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetProtoByNumber: function(proto: Integer): PProtoEnt
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetHostByName: function(name: PChar): PHostEnt
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetHostByAddr: function(addr: Pointer; len, Struc: Integer): PHostEnt
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetHostName: function(name: PChar; len: Integer): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Shutdown: function(s: TSocket; how: Integer): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  SetSockOpt: function(s: TSocket; level, optname: Integer; optval: PChar;
    optlen: Integer): Integer {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetSockOpt: function(s: TSocket; level, optname: Integer; optval: PChar;
    var optlen: Integer): Integer {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  SendTo: function(s: TSocket; var Buf; len, flags: Integer; addrto: PSockAddr;
    tolen: Integer): Integer {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Send: function(s: TSocket; var Buf; len, flags: Integer): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Recv: function(s: TSocket; var Buf; len, flags: Integer): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  RecvFrom: function(s: TSocket; var Buf; len, flags: Integer; from: PSockAddr;
    var fromlen: Integer): Integer {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  ntohs: function(netshort: u_short): u_short
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  ntohl: function(netlong: u_long): u_long
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Listen: function(s: TSocket; backlog: Integer): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  IoctlSocket: function(s: TSocket; cmd: DWORD; var arg: u_long): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Inet_ntoa: function(inaddr: TInAddr): PChar
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Inet_addr: function(cp: PChar): u_long
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  htons: function(hostshort: u_short): u_short
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  htonl: function(hostlong: u_long): u_long
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetSockName: function(s: TSocket; name: PSockAddr; var namelen: Integer): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetPeerName: function(s: TSocket; name: PSockAddr; var namelen: Integer): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Connect: function(s: TSocket; name: PSockAddr; namelen: Integer): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  CloseSocket: function(s: TSocket): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Bind: function(s: TSocket; addr: PSockAddr; namelen: Integer): Integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Accept: function(s: TSocket; addr: PSockAddr; var addrlen: Integer): TSocket
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Socket: function(af, Struc, Protocol: Integer): TSocket
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  Select: function(nfds: Integer; readfds, writefds, exceptfds: PFDSet;
    timeout: PTimeVal): Longint {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;

  GetAddrInfo: function(NodeName: PChar; ServName: PChar; Hints: PAddrInfo;
    var Addrinfo: PAddrInfo): integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  FreeAddrInfo: procedure(ai: PAddrInfo)
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;
  GetNameInfo: function( addr: PSockAddr; namelen: Integer; host: PChar;
    hostlen: DWORD; serv: PChar; servlen: DWORD; flags: integer): integer
    {$IFDEF LINUX}cdecl{$ELSE}stdcall{$ENDIF} = nil;

{$IFNDEF LINUX}
  __WSAFDIsSet: function (s: TSocket; var FDSet: TFDSet): Bool stdcall = nil;
  WSAIoctl: function (s: TSocket; dwIoControlCode: DWORD; lpvInBuffer: Pointer;
    cbInBuffer: DWORD; lpvOutBuffer: Pointer; cbOutBuffer: DWORD;
    lpcbBytesReturned: PDWORD; lpOverlapped: Pointer;
    lpCompletionRoutine: pointer): u_int stdcall = nil;
{$ENDIF}

{$IFDEF LINUX}
function LSWSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer; cdecl;
function LSWSACleanup: Integer; cdecl;
function LSWSAGetLastError: Integer; cdecl;
{$ENDIF}

var
  SynSockCS: TCriticalSection;
  SockEnhancedApi: Boolean;
  SockWship6Api: Boolean;

const
{$IFDEF LINUX}
  DLLStackName = 'libc.so.6';
{$ELSE}
  {$IFDEF WINSOCK1}
    DLLStackName = 'wsock32.dll';
  {$ELSE}
    DLLStackName = 'ws2_32.dll';
  {$ENDIF}
  DLLwship6 = 'wship6.dll';
{$ENDIF}

implementation

var
  SynSockCount: Integer = 0;
  LibHandle: THandle = 0;
  Libwship6Handle: THandle = 0;

function IN6_IS_ADDR_UNSPECIFIED(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_dw.s_dw1 = 0) and (a^.s_un_dw.s_dw2 = 0) and
             (a^.s_un_dw.s_dw3 = 0) and (a^.s_un_dw.s_dw4 = 0));
end;

function IN6_IS_ADDR_LOOPBACK(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_dw.s_dw1 = 0) and (a^.s_un_dw.s_dw2 = 0) and
             (a^.s_un_dw.s_dw3 = 0) and
             (a^.s_un_b.s_b13 = char(0)) and (a^.s_un_b.s_b14 = char(0)) and
             (a^.s_un_b.s_b15 = char(0)) and (a^.s_un_b.s_b16 = char(1)));
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_b.s_b1 = u_char($FE)) and (a^.s_un_b.s_b2 = u_char($80)));
end;

function IN6_IS_ADDR_SITELOCAL(const a: PInAddr6): boolean;
begin
  Result := ((a^.s_un_b.s_b1 = u_char($FE)) and (a^.s_un_b.s_b2 = u_char($C0)));
end;

function IN6_IS_ADDR_MULTICAST(const a: PInAddr6): boolean;
begin
  Result := (a^.s_un_b.s_b1 = char($FF));
end;

function IN6_ADDR_EQUAL(const a: PInAddr6; const b: PInAddr6): boolean;
begin
  Result := (CompareMem( a, b, sizeof(TInAddr6)));
end;

procedure SET_IN6_IF_ADDR_ANY (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
end;

procedure SET_LOOPBACK_ADDR6 (const a: PInAddr6);
begin
  FillChar(a^, sizeof(TInAddr6), 0);
  a^.s_un_b.s_b16 := char(1);
end;

{=============================================================================}
{$IFDEF LINUX}
var
  errno_loc: function: PInteger cdecl = nil;

function LSWSAStartup(wVersionRequired: Word; var WSData: TWSAData): Integer;
begin
  with WSData do
  begin
    wVersion := wVersionRequired;
    wHighVersion := $202;
    szDescription := 'Synsock - Synapse Platform Independent Socket Layer';
    szSystemStatus := 'Running on Linux';
    iMaxSockets := 32768;
    iMaxUdpDg := 8192;
  end;
  Result := 0;
end;

function LSWSACleanup: Integer;
begin
  Result := 0;
end;

function LSWSAGetLastError: Integer;
begin
  Result := errno_loc^;
end;

function __FDELT(Socket: TSocket): Integer;
begin
  Result := Socket div __NFDBITS;
end;

function __FDMASK(Socket: TSocket): __fd_mask;
begin
  Result := 1 shl (Socket mod __NFDBITS);
end;

function FD_ISSET(Socket: TSocket; var fdset: TFDSet): Boolean;
begin
  Result := (fdset.fds_bits[__FDELT(Socket)] and __FDMASK(Socket)) <> 0;
end;

procedure FD_SET(Socket: TSocket; var fdset: TFDSet);
begin
  fdset.fds_bits[__FDELT(Socket)] := fdset.fds_bits[__FDELT(Socket)] or __FDMASK(Socket);
end;

procedure FD_CLR(Socket: TSocket; var fdset: TFDSet);
begin
  fdset.fds_bits[__FDELT(Socket)] := fdset.fds_bits[__FDELT(Socket)] and (not __FDMASK(Socket));
end;

procedure FD_ZERO(var fdset: TFDSet);
var
  I: Integer;
begin
  with fdset do
    for I := Low(fds_bits) to High(fds_bits) do
      fds_bits[I] := 0;
end;

{=============================================================================}
{$ELSE}
procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
var
  I: Integer;
begin
  I := 0;
  while I < FDSet.fd_count do
  begin
    if FDSet.fd_array[I] = Socket then
    begin
      while I < FDSet.fd_count - 1 do
      begin
        FDSet.fd_array[I] := FDSet.fd_array[I + 1];
        Inc(I);
      end;
      Dec(FDSet.fd_count);
      Break;
    end;
    Inc(I);
  end;
end;

function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
begin
  Result := __WSAFDIsSet(Socket, FDSet);
end;

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
begin
  if FDSet.fd_count < FD_SETSIZE then
  begin
    FDSet.fd_array[FDSet.fd_count] := Socket;
    Inc(FDSet.fd_count);
  end;
end;

procedure FD_ZERO(var FDSet: TFDSet);
begin
  FDSet.fd_count := 0;
end;
{$ENDIF}

{=============================================================================}

function InitSocketInterface(stack: string): Boolean;
begin
  Result := False;
  SockEnhancedApi := False;
  if stack = '' then
    stack := DLLStackName;
  SynSockCS.Enter;
  try
    if SynSockCount = 0 then
    begin
      SockEnhancedApi := False;
      SockWship6Api := False;
{$IFDEF LINUX}
      Libc.Signal(Libc.SIGPIPE, TSignalHandler(Libc.SIG_IGN));
      LibHandle := HMODULE(dlopen(PChar(Stack), RTLD_GLOBAL));
{$ELSE}
      LibHandle := LoadLibrary(PChar(Stack));
{$ENDIF}
      if LibHandle <> 0 then
      begin
{$IFDEF LINUX}
        errno_loc := GetProcAddress(LibHandle, PChar('__errno_location'));
        CloseSocket := GetProcAddress(LibHandle, PChar('close'));
        IoctlSocket := GetProcAddress(LibHandle, PChar('ioctl'));
        WSAGetLastError := LSWSAGetLastError;
        WSAStartup := LSWSAStartup;
        WSACleanup := LSWSACleanup;
{$ELSE}
        WSAIoctl := GetProcAddress(LibHandle, PChar('WSAIoctl'));
        __WSAFDIsSet := GetProcAddress(LibHandle, PChar('__WSAFDIsSet'));
        CloseSocket := GetProcAddress(LibHandle, PChar('closesocket'));
        IoctlSocket := GetProcAddress(LibHandle, PChar('ioctlsocket'));
        WSAGetLastError := GetProcAddress(LibHandle, PChar('WSAGetLastError'));
        WSAStartup := GetProcAddress(LibHandle, PChar('WSAStartup'));
        WSACleanup := GetProcAddress(LibHandle, PChar('WSACleanup'));
{$ENDIF}
        Accept := GetProcAddress(LibHandle, PChar('accept'));
        Bind := GetProcAddress(LibHandle, PChar('bind'));
        Connect := GetProcAddress(LibHandle, PChar('connect'));
        GetPeerName := GetProcAddress(LibHandle, PChar('getpeername'));
        GetSockName := GetProcAddress(LibHandle, PChar('getsockname'));
        GetSockOpt := GetProcAddress(LibHandle, PChar('getsockopt'));
        Htonl := GetProcAddress(LibHandle, PChar('htonl'));
        Htons := GetProcAddress(LibHandle, PChar('htons'));
        Inet_Addr := GetProcAddress(LibHandle, PChar('inet_addr'));
        Inet_Ntoa := GetProcAddress(LibHandle, PChar('inet_ntoa'));
        Listen := GetProcAddress(LibHandle, PChar('listen'));
        Ntohl := GetProcAddress(LibHandle, PChar('ntohl'));
        Ntohs := GetProcAddress(LibHandle, PChar('ntohs'));
        Recv := GetProcAddress(LibHandle, PChar('recv'));
        RecvFrom := GetProcAddress(LibHandle, PChar('recvfrom'));
        Select := GetProcAddress(LibHandle, PChar('select'));
        Send := GetProcAddress(LibHandle, PChar('send'));
        SendTo := GetProcAddress(LibHandle, PChar('sendto'));
        SetSockOpt := GetProcAddress(LibHandle, PChar('setsockopt'));
        ShutDown := GetProcAddress(LibHandle, PChar('shutdown'));
        Socket := GetProcAddress(LibHandle, PChar('socket'));
        GetHostByAddr := GetProcAddress(LibHandle, PChar('gethostbyaddr'));
        GetHostByName := GetProcAddress(LibHandle, PChar('gethostbyname'));
        GetProtoByName := GetProcAddress(LibHandle, PChar('getprotobyname'));
        GetProtoByNumber := GetProcAddress(LibHandle, PChar('getprotobynumber'));
        GetServByName := GetProcAddress(LibHandle, PChar('getservbyname'));
        GetServByPort := GetProcAddress(LibHandle, PChar('getservbyport'));
        GetHostName := GetProcAddress(LibHandle, PChar('gethostname'));

{$IFNDEF FORCEOLDAPI}
        GetAddrInfo := GetProcAddress(LibHandle, PChar('getaddrinfo'));
        FreeAddrInfo := GetProcAddress(LibHandle, PChar('freeaddrinfo'));
        GetNameInfo := GetProcAddress(LibHandle, PChar('getnameinfo'));
        SockEnhancedApi := Assigned(GetAddrInfo) and Assigned(FreeAddrInfo)
          and Assigned(GetNameInfo);
  {$IFNDEF LINUX}
        if not SockEnhancedApi then
        begin
          LibWship6Handle := LoadLibrary(PChar(DLLWship6));
          if LibWship6Handle <> 0 then
          begin
            GetAddrInfo := GetProcAddress(LibWship6Handle, PChar('getaddrinfo'));
            FreeAddrInfo := GetProcAddress(LibWship6Handle, PChar('freeaddrinfo'));
            GetNameInfo := GetProcAddress(LibWship6Handle, PChar('getnameinfo'));
            SockWship6Api := Assigned(GetAddrInfo) and Assigned(FreeAddrInfo)
              and Assigned(GetNameInfo);
          end;
        end;
  {$ENDIF}
{$ENDIF}
        Result := True;
      end;
    end
    else Result := True;
    if Result then
      Inc(SynSockCount);
  finally
    SynSockCS.Leave;
  end;
end;

function DestroySocketInterface: Boolean;
begin
  SynSockCS.Enter;
  try
    Dec(SynSockCount);
    if SynSockCount < 0 then
      SynSockCount := 0;
    if SynSockCount = 0 then
    begin
      if LibHandle <> 0 then
      begin
        FreeLibrary(libHandle);
        LibHandle := 0;
      end;
      if LibWship6Handle <> 0 then
      begin
        FreeLibrary(LibWship6Handle);
        LibWship6Handle := 0;
      end;
    end;
  finally
    SynSockCS.Leave;
  end;
  Result := True;
end;

initialization
begin
  SynSockCS:= TCriticalSection.Create;
  SET_IN6_IF_ADDR_ANY (@in6addr_any);
  SET_LOOPBACK_ADDR6  (@in6addr_loopback);
end;

finalization
begin
  SynSockCS.Free;
end;

end.
