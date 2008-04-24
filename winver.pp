{
    $Id: header,v 1.1.2.1 2003/01/05 20:47:31 michael Exp $
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Windows Version detection functionality.
    
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
unit winver;

Interface

Uses Windows;

const  
  Win32Platform     : Integer = 0;
  Win32MajorVersion : Integer = 0;
  Win32MinorVersion : Integer = 0;
  Win32BuildNumber  : Integer = 0;
                          
  Win32CSDVersion   : string = '';
 
function CheckWin32Version(Major,Minor : Integer ): Boolean;
function CheckWin32Version(Major : Integer): Boolean;

Implementation


uses sysutils;

procedure InitVersion;

var
  Info: TOSVersionInfo;
  
begin
  Info.dwOSVersionInfoSize := SizeOf(Info);
  if GetVersionEx(Info) then
    with Info do
      begin
      Win32Platform:=dwPlatformId;
      Win32MajorVersion:=dwMajorVersion;
      Win32MinorVersion:=dwMinorVersion;
      if (Win32Platform=VER_PLATFORM_WIN32_WINDOWS) then
        Win32BuildNumber:=dwBuildNumber and $FFFF
      else
        Win32BuildNumber := dwBuildNumber;
      Win32CSDVersion := StrPas(szCSDVersion);
      end;
end;

function CheckWin32Version(Major : Integer): Boolean;

begin
  Result:=CheckWin32Version(Major,0)
end;

function CheckWin32Version(Major,Minor: Integer): Boolean;

begin
  Result := (Win32MajorVersion>Major) or
            ((Win32MajorVersion=Major) and (Win32MinorVersion>=Minor));
end;

initialization
  InitVersion;
end.