{
   Double commander
   -------------------------------------------------------------------------
   This unit contains Windows specific functions

   Copyright (C) 2015 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301 USA
}
// unit renamed DCWindows to lelDCWindows. Fixes #9 Units conflicting with KASToolbar from OPM
unit lelDCWindows;

{$mode objfpc}{$H+}

interface

uses
  Windows;

{en
   Converts file name in UTF-8 encoding to file name
   with UTF-16 encoding with extended-length path prefix
}
function UTF16LongName(const FileName: String): UnicodeString;

{en
   Enable a privilege
   @param(hToken Access token handle)
   @param(lpszPrivilege Name of privilege to enable)
   @returns(The function returns @true if successful, @false otherwise)
}
function EnablePrivilege(hToken: HANDLE; lpszPrivilege: LPCTSTR): Boolean;
{en
   Copy permissions specific to the NTFS file system,
   like read and write permissions, and the file owner
}
function CopyNtfsPermissions(const Source, Target: String): Boolean;

implementation

uses
  JwaAclApi, JwaWinNT, JwaAccCtrl, JwaWinBase, JwaWinType;

function UTF16LongName(const FileName: String): UnicodeString;
var
  Temp: PWideChar;
begin
  if Pos('\\', FileName) = 0 then
    Result := '\\?\' + UTF8Decode(FileName)
  else begin
    Result := '\\?\UNC\' + UTF8Decode(Copy(FileName, 3, MaxInt));
  end;
  Temp := Pointer(Result) + 4;
  while Temp^ <> #0 do
  begin
    if Temp^ = '/' then Temp^:= '\';
    Inc(Temp);
  end;
  if ((Temp - 1)^ = DriveSeparator) then Result:= Result + '\';
end;

function EnablePrivilege(hToken: HANDLE; lpszPrivilege: LPCTSTR): Boolean;
var
  tp: TTokenPrivileges;
  luid: TLuid = (LowPart: 0; HighPart: 0);
begin
  if (not LookupPrivilegeValue(nil, lpszPrivilege, luid)) then
    Exit(False);

  tp.PrivilegeCount:= 1;
  tp.Privileges[0].Luid:= luid;
  tp.Privileges[0].Attributes:= SE_PRIVILEGE_ENABLED;

  // Enable privilege in the specified access token
  if (not AdjustTokenPrivileges(hToken, False, @tp, SizeOf(TTokenPrivileges), nil, nil)) then
    Exit(False);

  // Not all privileges or groups referenced are assigned to the caller
  Result:= not (GetLastError() = ERROR_NOT_ALL_ASSIGNED);
end;

function CopyNtfsPermissions(const Source, Target: String): Boolean;
const
  DisabledPrivilege: Boolean = True;
var
  Dacl, Sacl: PACL;
  lpdwRevision: DWORD = 0;
  ProcessToken: HANDLE = 0;
  SidOwner, SidGroup: PSID;
  SecDescPtr: PSECURITY_DESCRIPTOR = nil;
  SecDescCtl: SECURITY_DESCRIPTOR_CONTROL = 0;
  SecurityInfo: SECURITY_INFORMATION = DACL_SECURITY_INFORMATION or SACL_SECURITY_INFORMATION or
                                       OWNER_SECURITY_INFORMATION or GROUP_SECURITY_INFORMATION;
begin
  if DisabledPrivilege then
  begin
    DisabledPrivilege:= False;
    Result:= OpenProcessToken(GetCurrentProcess(), TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, ProcessToken);
    if not Result then
      Exit(False)
    else begin
      EnablePrivilege(ProcessToken, SE_BACKUP_NAME);
      EnablePrivilege(ProcessToken, SE_RESTORE_NAME);
      EnablePrivilege(ProcessToken, SE_SECURITY_NAME);
      CloseHandle(ProcessToken);
    end;
  end;
  Result:= GetNamedSecurityInfoW(PWideChar(UTF8Decode(Source)), SE_FILE_OBJECT, SecurityInfo,
             @SidOwner, @SidGroup, @Dacl, @Sacl, SecDescPtr) = ERROR_SUCCESS;
  if Result then
  begin
    if GetSecurityDescriptorControl(SecDescPtr, SecDescCtl, lpdwRevision) then
    begin
      // Need to copy DACL inheritance
      if (SecDescCtl and SE_DACL_PROTECTED <> 0) then
        SecurityInfo:= SecurityInfo or PROTECTED_DACL_SECURITY_INFORMATION
      else begin
        SecurityInfo:= SecurityInfo or UNPROTECTED_DACL_SECURITY_INFORMATION;
      end;
      // Need to copy SACL inheritance
      if (SecDescCtl and SE_SACL_PROTECTED <> 0) then
        SecurityInfo:= SecurityInfo or PROTECTED_SACL_SECURITY_INFORMATION
      else begin
        SecurityInfo:= SecurityInfo or UNPROTECTED_SACL_SECURITY_INFORMATION;
      end;
      Result:= SetNamedSecurityInfoW(PWideChar(UTF8Decode(Target)), SE_FILE_OBJECT,
                 SecurityInfo, SidOwner, SidGroup, Dacl, Sacl) = ERROR_SUCCESS;
    end;
    {$PUSH}{$HINTS OFF}{$WARNINGS OFF}
    LocalFree(HLOCAL(SecDescPtr));
    {$POP}
  end;
end;

end.

