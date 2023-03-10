{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Peter Vreman

    Stabs Line Info Retriever

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
  This unit should not be compiled in objfpc mode, since this would make it
  dependent on objpas unit.
}
unit CustomLineInfo;

interface

{$S-}
{$Q-}

function GetLineInfo(addr: PtrUInt; var func, Source: ShortString; var line: LongInt): Boolean;
function StabBackTraceStr(addr: Pointer): shortstring;

implementation

uses
  exeinfo, Strings;

const
  N_Function = $24;
  N_TextLine = $44;
  N_DataLine = $46;
  N_BssLine = $48;
  N_SourceFile = $64;
  N_IncludeFile = $84;

  maxstabs = 40; { size of the stabs buffer }

var
  { GDB after 4.18 uses offset to function begin
    in text section but OS/2 version still uses 4.16 PM }
  StabsFunctionRelative: Boolean;

type
  pstab = ^tstab;

  tstab = Packed Record
    strpos: LongInt;
    ntype: Byte;
    nother: Byte;
    ndesc: Word;
    nvalue: DWord;
  end;

{ We use static variable so almost no stack is required, and is thus
  more safe when an error has occured in the program }
var
  e: TExeFile;
  stabcnt,              { amount of stabs }
  stablen, stabofs,              { absolute stab section offset in executable }
  stabstrlen, stabstrofs: LongInt; { absolute stabstr section offset in executable }
  dirlength: LongInt; { length of the dirctory part of the source file }
  stabs: array[0..maxstabs - 1] of tstab;  { buffer }
  funcstab,             { stab with current function info }
  linestab,             { stab with current line info }
  dirstab,              { stab with current directory info }
  filestab: tstab;   { stab with current file info }
  filename: ShortString;
  dbgfn: String;

var
  Crc32Tbl: array[0..255] of cardinal;

procedure MakeCRC32Tbl;
var
  crc: cardinal;
  i, n: integer;
begin
  for i := 0 to 255 do
  begin
    crc := i;
    for n := 1 to 8 do
    begin
      if (crc and 1) <> 0 then
        crc := (crc shr 1) xor Cardinal($edb88320)
      else
        crc := crc shr 1;
      Crc32Tbl[i] := crc;
    end;
  end;
end;

function UpdateCrc32(InitCrc: Cardinal; const InBuf; InLen: LongInt): Cardinal;
var
  i: LongInt;
  p: PChar;
begin
  if Crc32Tbl[1] = 0 then
    MakeCrc32Tbl;

  p := @InBuf;
  Result := not InitCrc;
  for i := 1 to InLen do
  begin
    UpdateCrc32 := Crc32Tbl[Byte(Result) xor Byte(p^)] xor (Result shr 8);
    Inc(p);
  end;
  Result := not Result;
end;

function CheckDbgFile(var e: TExeFile; const fn: string; dbgcrc: Cardinal): Boolean;
var
  c: Cardinal;
  ofm: Word;
  g: file;
begin
  CheckDbgFile := False;
  Assign(g, fn);
  {$I-}
  ofm := FileMode;
  FileMode := $40;
  reset(g, 1);
  FileMode := ofm;
  {$I+}
  if IOResult <> 0 then Exit;
  { We reuse the buffer from e here to prevent too much stack allocation }
  c := 0;
  repeat
    BlockRead(g, e.buf, e.bufsize, e.bufcnt);
    c := UpdateCrc32(c, e.buf, e.bufcnt);
  until e.bufcnt < e.bufsize;

  Close(g);
  CheckDbgFile := (dbgcrc = c);
end;

function ReadDebugLink(var e: TExeFile; var dbgfn: string): Boolean;
var
  dbglink: array[0..512] of Char;
  i, dbglinklen, dbglinkofs: LongInt;
  dbgcrc: Cardinal;
begin
  ReadDebugLink := False;
  dbglinkofs := 0;
  dbglinklen := 0;
  dbgcrc := 0;
  if not FindExeSection(e, '.gnu_debuglink', dbglinkofs, dbglinklen) then Exit;
  if dbglinklen > SizeOf(dbglink) - 1 then Exit;

  FillChar(dbglink, SizeOf(dbglink), 0);
  Seek(e.f, dbglinkofs);
  BlockRead(e.f, dbglink, dbglinklen);
  dbgfn := strpas(dbglink);

  if Length(dbgfn) = 0 then Exit;

  i := Align(Length(dbgfn) + 1, 4);

  if (i + 4) > dbglinklen then Exit;

  Move(dbglink[i], dbgcrc, 4);
  { current dir }
  if CheckDbgFile(e, dbgfn, dbgcrc) then
  begin
    ReadDebugLink := True;
    exit;
  end;
  { executable dir }
  i := Length(e.filename);
  while (i > 0) and not (e.filename[i] in AllowDirectorySeparators) do
    Dec(i);

  if i > 0 then
  begin
    dbgfn := Copy(e.filename, 1, i) + dbgfn;
    if CheckDbgFile(e, dbgfn, dbgcrc) then
    begin
      ReadDebugLink := True;
      Exit;
    end;
  end;
end;

function OpenStabs(addr: pointer): boolean;
var
  baseaddr: pointer;
begin
  OpenStabs := False;
  baseaddr := nil;
  GetModuleByAddr(addr, baseaddr, filename);
  {$IFDEF DEBUG_LINEINFO}
   WriteLn(stderr,filename,' Baseaddr: ',HexStr(PtrUInt(baseaddr),SizeOf(baseaddr)*2));
  {$ENDIF DEBUG_LINEINFO}

  if not OpenExeFile(e, filename) then Exit;

  if ReadDebugLink(e, dbgfn) then
  begin
    CloseExeFile(e);
    if not OpenExeFile(e, dbgfn) then Exit;
  end;

  if PtrUInt(BaseAddr) < e.processaddress then Exit;

  e.processaddress := PtrUInt(baseaddr) - e.processaddress;
  StabsFunctionRelative := E.FunctionRelative;

  if FindExeSection(e, '.stab', stabofs, stablen) and
    FindExeSection(e, '.stabstr', stabstrofs, stabstrlen) then
  begin
    stabcnt := stablen div SizeOf(tstab);
    OpenStabs := True;
  end
  else
  begin
    CloseExeFile(e);
    Exit;
  end;
end;

procedure CloseStabs;
begin
  CloseExeFile(e);
end;

function GetLineInfo(addr: PtrUInt; var func, Source: ShortString; var line: LongInt): Boolean;
var
  res, stabsleft, stabscnt, i: LongInt;
  found: Boolean;
  lastfunc: tstab;
begin
  GetLineInfo := False;
  {$IFDEF DEBUG_LINEINFO}
   WriteLn(stderr,'GetLineInfo called');
  {$ENDIF DEBUG_LINEINFO}
  FillChar(func, High(func) + 1, 0);
  FillChar(Source, High(Source) + 1, 0);
  line := 0;

  if not e.isopen then
  begin
    if not OpenStabs(Pointer(addr)) then Exit;
  end;

  { correct the value to the correct address in the file }
  { processaddress is set in OpenStabs                   }
  addr := dword(addr - e.processaddress);

  {$IFDEF DEBUG_LINEINFO}
   WriteLn(stderr,'Addr: ',HexStr(addr,SizeOf(addr)*2));
  {$ENDIF DEBUG_LINEINFO}

  FillChar(funcstab, SizeOf(tstab), 0);
  FillChar(filestab, SizeOf(tstab), 0);
  FillChar(dirstab, SizeOf(tstab), 0);
  FillChar(linestab, SizeOf(tstab), 0);
  FillChar(lastfunc, SizeOf(tstab), 0);
  found := False;
  Seek(e.f, stabofs);
  stabsleft := stabcnt;
  repeat
    if stabsleft > maxstabs then
      stabscnt := maxstabs
    else
      stabscnt := stabsleft;

    BlockRead(e.f, stabs, stabscnt * SizeOf(tstab), res);
    stabscnt := res div SizeOf(tstab);
    for i := 0 to stabscnt - 1 do
    begin
      case stabs[i].ntype of
        N_BssLine,
        N_DataLine,
        N_TextLine:
        begin
          if (stabs[i].ntype = N_TextLine) and StabsFunctionRelative then
            Inc(stabs[i].nvalue, lastfunc.nvalue);

          if (stabs[i].nvalue <= addr) and (stabs[i].nvalue > linestab.nvalue) then
          begin
            { if it's equal we can stop and take the last info }
            if stabs[i].nvalue = addr then
              found := True
            else
              linestab := stabs[i];
          end;
        end;
        N_Function:
        begin
          lastfunc := stabs[i];
          if (stabs[i].nvalue <= addr) and (stabs[i].nvalue > funcstab.nvalue) then
          begin
            funcstab := stabs[i];
            FillChar(linestab, SizeOf(tstab), 0);
          end;
        end;
        N_SourceFile,
        N_IncludeFile:
        begin
          if (stabs[i].nvalue <= addr) and (stabs[i].nvalue >= filestab.nvalue) then
          begin
                { if same value and type then the first one
                  contained the directory PM }
            if (stabs[i].nvalue = filestab.nvalue) and (stabs[i].ntype = filestab.ntype) then
              dirstab := filestab
            else
              FillChar(dirstab, SizeOf(tstab), 0);

            filestab := stabs[i];
            FillChar(linestab, SizeOf(tstab), 0);
            { if new file then func is not valid anymore PM }
            if stabs[i].ntype = N_SourceFile then
            begin
              FillChar(funcstab, SizeOf(tstab), 0);
              FillChar(lastfunc, SizeOf(tstab), 0);
            end;
          end;
        end;
      end;
    end;
    Dec(stabsleft, stabscnt);
  until found or (stabsleft = 0);

  { get the line,source,function info }
  line := linestab.ndesc;
  if dirstab.ntype <> 0 then
  begin
    Seek(e.f, stabstrofs + dirstab.strpos);
    BlockRead(e.f, Source[1], High(Source) - 1, res);
    dirlength := strlen(@Source[1]);
    Source[0] := Chr(dirlength);
  end
  else
    dirlength := 0;

  if filestab.ntype <> 0 then
  begin
    Seek(e.f, stabstrofs + filestab.strpos);
    BlockRead(e.f, Source[dirlength + 1], High(Source) - (dirlength + 1), res);
    Source[0] := Chr(strlen(@Source[1]));
  end;

  if funcstab.ntype <> 0 then
  begin
    Seek(e.f, stabstrofs + funcstab.strpos);
    BlockRead(e.f, func[1], High(func) - 1, res);
    func[0] := Chr(strlen(@func[1]));
    i := pos(':', func);

    if i > 0 then Delete(func, i, 255);
  end;
  //  if e.isopen then
  //    CloseStabs;
  GetLineInfo := True;
end;

function StabBackTraceStr(addr: Pointer): ShortString;
var
  func, Source: ShortString;
  hs: String[32];
  line: LongInt;
  Store: TBackTraceStrFunc;
  Success: Boolean;
begin
  {$IFDEF DEBUG_LINEINFO}
   WriteLn(stderr,'StabBackTraceStr called');
  {$ENDIF DEBUG_LINEINFO}
  { reset to prevent infinite recursion if problems inside the code PM }
  Success := False;
  Store := BackTraceStrFunc;
  BackTraceStrFunc := @SysBackTraceStr;
  Success := GetLineInfo(PtrUInt(addr), func, Source, line);
  { create string }
  {$IFDEF NETWARE}
   { we need addr relative to code start on netware }
   Dec(addr,PtrUInt(system.NWGetCodeStart));
   StabBackTraceStr:='  CodeStart + $'+HexStr(PtrUInt(addr),SizeOf(PtrUInt)*2);
  {$ELSE}
   StabBackTraceStr := '  $' + HexStr(PtrUInt(addr), SizeOf(PtrUInt) * 2);
  {$ENDIF}

  if func <> '' then  Result := Result + '  ' + func;

  if Source <> '' then
  begin
    if func <> '' then Result := Result + ', ';

    if line <> 0 then
    begin
      Str(line, hs);
      Result := Result + ' line ' + hs;
    end;
    Result := Result + ' of ' + Source;
  end;

  if Success then BackTraceStrFunc := Store;
end;

initialization
  BackTraceStrFunc := @StabBackTraceStr;

finalization
  if e.isopen then CloseStabs;
end.
