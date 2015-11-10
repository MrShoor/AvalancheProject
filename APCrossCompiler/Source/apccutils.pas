unit apccUtils;

{$Include Options.inc}

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils;

function Exec(const App, CmdLine: string; out output: string): Cardinal;
function CreateFxcProcess(Params: string; out output: string): Cardinal;
function CreateHLSLccProcess(Params: string; out output: string): Cardinal;

implementation

uses
  Windows;

function Exec(const App, CmdLine: string; out output: string): Cardinal;
const BUFSIZE = 2048;
var sinfo: TStartupInfo;
    pinfo: TProcessInformation;
    sattr: TSecurityAttributes;

    g_hChildStd_OUT_Rd, g_hChildStd_OUT_Wr: THandle;
    chBuf: array [0..BUFSIZE-1] Of AnsiChar;
    rbyte: Cardinal;
begin
    Result := 0;

    FillChar(sattr, SizeOf(sattr), 0);
    sattr.nLength := SizeOf(sattr);
    sattr.lpSecurityDescriptor := nil;
    sattr.bInheritHandle := True;
    if not CreatePipe(g_hChildStd_OUT_Rd, g_hChildStd_OUT_Wr, @sattr, 0) then Exit($FFFFFFFF);
    try
        FillChar(sinfo, SizeOf(sinfo), 0);
        sinfo.cb := SizeOf(sinfo);
        sinfo.hStdInput := GetStdHandle(STD_INPUT_HANDLE);
        sinfo.hStdOutput := g_hChildStd_OUT_Wr;
        sinfo.hStdError := g_hChildStd_OUT_Wr;
        sinfo.dwFlags := STARTF_USESTDHANDLES;

        if CreateProcess(PChar(App), PChar(CmdLine), nil, @sattr, True, 0, nil, nil, sinfo, pinfo) then
        begin
            try
                WaitForSingleObject(pinfo.hProcess, INFINITE);
                if not GetExitCodeProcess(pinfo.hProcess, Result) then Exit($FFFFFFFF);
            finally
                CloseHandle(pinfo.hProcess);
                CloseHandle(pinfo.hThread);
            end;
        end
        else
            Exit($FFFFFFFF);

        output := '';
        repeat
            FillChar(chBuf[0], BUFSIZE, 0);
            ReadFile(g_hChildStd_OUT_Rd, chBuf, BUFSIZE, rbyte, nil);
            output := output + string(chBuf);
        until rbyte < BUFSIZE;
    finally
        CloseHandle(g_hChildStd_OUT_Rd);
        CloseHandle(g_hChildStd_OUT_Wr);
    end;
end;

function CreateFxcProcess(Params: string; out output: string): Cardinal;
var App, CmdLine: string;
begin
    App     := ExtractFilePath(ParamStr(0)) + 'fxc.exe';
    CmdLine := Format('"%s" %s', [App, Params]);
    Result := Exec(App, CmdLine, output);
end;

function CreateHLSLccProcess(Params: string; out output: string): Cardinal;
var App, CmdLine: string;
begin
    {$IFDEF DEBUGHLSLCC}
    App     := ExtractFilePath(ParamStr(0)) + 'HLSLcc_d.exe';
    {$ELSE}
    App     := ExtractFilePath(ParamStr(0)) + 'HLSLcc.exe';
    {$ENDIF}
    CmdLine := Format('"%s" %s', [App, Params]);
    {$IFDEF DEBUGHLSLCC}
    WriteLn('cmd: ', CmdLine);
    {$ENDIF}
    Result := Exec(App, CmdLine, output);
end;

end.

