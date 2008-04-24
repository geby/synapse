{==============================================================================|
| Project : Ararat Synapse                                       | 001.000.000 |
|==============================================================================|
| Content: Utils for FreePascal compatibility                                  |
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
| Portions created by Lukas Gebauer are Copyright (c)2003.                     |
| All Rights Reserved.                                                         |
|==============================================================================|
| Contributor(s):                                                              |
|==============================================================================|
| History: see HISTORY.HTM from distribution package                           |
|          (Found at URL: http://www.ararat.cz/synapse/)                       |
|==============================================================================}

{:@exclude}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}
{$H+}

unit synafpc;

interface

{$IFDEF LINUX}
  {$IFDEF FPC}
uses
  Libc,
  dynlibs;

type
  HMODULE = Longint;

function LoadLibrary(ModuleName: PChar): HMODULE;
function FreeLibrary(Module: HMODULE): LongBool;
function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
procedure Sleep(milliseconds: Cardinal);

  {$ENDIF}
{$ENDIF}


implementation

{==============================================================================}
{$IFDEF LINUX}
  {$IFDEF FPC}
function LoadLibrary(ModuleName: PChar): HMODULE;
begin
  Result := HMODULE(dynlibs.LoadLibrary(Modulename));
end;

function FreeLibrary(Module: HMODULE): LongBool;
begin
  Result := dynlibs.UnloadLibrary(pointer(Module));
end;

function GetProcAddress(Module: HMODULE; Proc: PChar): Pointer;
begin
  Result := dynlibs.GetProcedureAddress(pointer(Module), Proc);
end;

function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
begin
  Result := 0;
end; 

procedure Sleep(milliseconds: Cardinal);
begin
  usleep(milliseconds * 1000);  // usleep is in microseconds
end;
 
  {$ENDIF}
{$ENDIF}

end.
