program ccF3DSL;

//{$DEFINE DEBUGHLSLCC}
{$APPTYPE CONSOLE}

uses
  Windows,
  SysUtils,
  Classes,
  SuperObject,
  IdHTTP,
  Math,
  D3D10,
  untFileTree in 'untFileTree.pas',
  untCompileTask in 'untCompileTask.pas';

type
    EHLSL = class(Exception);
    EGLSL = class(Exception);
    ETranslator = class(Exception);

    TShadersString = array [TShaderType] of string;

const
//    cCompileServer_GLSL = 'SHAPCDEV31';
    cCompileServer_GLSL = '127.0.0.1';

var WorkingDir: string;

function Exec(const App, CmdLine: string; out output: string): Cardinal;
const BUFSIZE = 2048;
var sinfo: TStartupInfo;
    pinfo: TProcessInformation;
    sattr: TSecurityAttributes;

    g_hChildStd_OUT_Rd, g_hChildStd_OUT_Wr: THandle;
    chBuf: array [0..BUFSIZE-1] Of AnsiChar;
    rbyte: Cardinal;
begin
    Result := $FFFFFFFF;

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
    App     := ExtractFilePath(ParamStr(0)) + 'HLSLcc.exe';
    CmdLine := Format('"%s" %s', [App, Params]);
    {$IFDEF DEBUGHLSLCC}
    WriteLn('cmd: ', CmdLine);
    {$ENDIF}
    Result := Exec(App, CmdLine, output);
end;

function ReparseLogForError_ATI(const msgstr: string; const log: string; const node: IFileNode): string;
var sl: TStringList;
    ls: string;
    i, j: Integer;
    n: Integer;
    rowInd, colInd: Integer;
    filename: string;
    dir: string;
begin
  sl := TStringList.Create;
  try
    sl.Text := log;
    for i := 0 to sl.Count - 1 do
      begin
        rowInd := -1;
        colInd := -1;

        ls := LowerCase(sl.Strings[i]);
        n := Pos(msgstr, ls);
        If n = 1 Then
        begin

          Delete(ls, 1, Length(msgstr));
          n := 0;
          for j := 1 to Length(ls) - 1 do
            if ls[j] = ' ' then Inc(n) else Break;
          if n > 0 then Delete(ls, 1, n);
          n := Pos(':', ls);
          if n > 1 then
          begin
            if not TryStrToInt(Copy(ls, 1, n - 1), colInd) then Continue;
          end
          else
            Continue;
          Delete(ls, 1, n);

          n := Pos(':', ls);
          if n > 1 then
          begin
            if not TryStrToInt(Copy(ls, 1, n - 1), rowInd) then Continue;
          end
          else
            Continue;
          Delete(ls, 1, n);

          Dec(rowInd);
          ls := Copy(sl.Strings[i], Length(sl.Strings[i]) - Length(ls) + 1, Length(ls));
          while (Length(ls) > 0) and (ls[1]=' ') do Delete(ls, 1, 1);
          dir := GetCurrentDir;
          if (dir[Length(dir)] <> '\') then dir := dir + '\';
          filename := ExtractRelativePath(dir, node.GetFileNameByLine(rowInd));
          sl.Strings[i] := Format('%s:%d:%d: ', [filename, rowInd + 1, colInd+1]) + ls;
        end;
      end;
    for i := sl.Count - 1 downto 0 do
        if sl.Strings[i] = '' then sl.Delete(i);

    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function ReparseLogForError_NV(const msgstr: string; const log: string; const node: IFileNode): string;
var sl: TStringList;
    ls: string;
    i: Integer;
    n: Integer;
    rowInd, colInd: Integer;
    filename: string;
    dir: string;
begin
  sl := TStringList.Create;
  try
    sl.Text := log;
    for i := 0 to sl.Count - 1 do
      begin
        rowInd := -1;
        colInd := -1;

        ls := LowerCase(sl.Strings[i]);
        n := Pos(msgstr, ls);
        If n <> 0 Then
        begin
          n := Pos('(', ls);
          if n = 0 then Continue;
          if n > 5 then Continue;
          if not TryStrToInt(Copy(ls, 1, n-1), colInd) then Continue;
          Delete(ls, 1, n);

          n := Pos(')', ls);
          if n = 0 then Continue;
          if n > 5 then Continue;
          if not TryStrToInt(Copy(ls, 1, n-1), rowInd) then Continue;
          Delete(ls, 1, n);

          Dec(rowInd);
          ls := Copy(sl.Strings[i], Length(sl.Strings[i]) - Length(ls) + 1, Length(ls));
          while (Length(ls) > 0) and (ls[1]=' ') do Delete(ls, 1, 1);
          dir := GetCurrentDir;
          if (dir[Length(dir)] <> '\') then dir := dir + '\';
          filename := ExtractRelativePath(dir, node.GetFileNameByLine(rowInd));
          sl.Strings[i] := Format('%s:%d:%d: ', [filename, rowInd + 1, colInd+1]) + ls;
        end;
      end;
    for i := sl.Count - 1 downto 0 do
        if sl.Strings[i] = '' then sl.Delete(i);

    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function ReparseFxcMessages(const msgstr: string; const log: string; const node: IFileNode): string;
var sl: TStringList;
    ls: string;
    i: Integer;
    n: Integer;
    rowInd, colInd: Integer;
    filename: string;
    dir: string;
begin
  sl := TStringList.Create;
  try
    sl.Text := log;
    for i := 0 to sl.Count - 1 do
      begin
        rowInd := -1;
        colInd := -1;

        ls := LowerCase(sl.Strings[i]);
        n := Pos(msgstr, ls);
        If n <> 0 Then
        begin
          dec(n);
          while (ls[n] <> '(') and (n > 0) do dec(n);
          if n <= 0 then Continue;
          Delete(ls, 1, n);

          n := Pos(',', ls);
          if n = 0 then Continue;
          if n > 5 then Continue;
          if not TryStrToInt(Copy(ls, 1, n-1), rowInd) then Continue;
          Delete(ls, 1, n);

          n := Pos(')', ls);
          if n = 0 then Continue;
          if n > 5 then Continue;
          if not TryStrToInt(Copy(ls, 1, n-1), colInd) then Continue;
          Delete(ls, 1, n);

          Dec(rowInd);
          ls := Copy(sl.Strings[i], Length(sl.Strings[i]) - Length(ls) + 1, Length(ls));
          while (Length(ls) > 0) and (ls[1]=' ') do Delete(ls, 1, 1);
          dir := WorkingDir;
          if (dir[Length(dir)] <> '\') then dir := dir + '\';
          filename := ExtractRelativePath(dir, node.GetFileNameByLine(rowInd));
          sl.Strings[i] := Format('%s:%d:%d: ', [filename, rowInd + 1, colInd+1]) + ls;
        end;
      end;
    for i := sl.Count - 1 downto 0 do
        if sl.Strings[i] = '' then sl.Delete(i);

    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

function ReparseFxcLog(const log: string; const node: IFileNode): string;
begin
    Result := log;
    Result := ReparseFxcMessages(': error ', Result, node);
    Result := ReparseFxcMessages(': warning ', Result, node);
end;

function ReparseLog(const log: string; const node: IFileNode): string;
begin
    Result := log;
    Result := ReparseLogForError_ATI('error:', Result, node);
    Result := ReparseLogForError_ATI('warning:', Result, node);
    Result := ReparseLogForError_NV('error', Result, node);
    Result := ReparseLogForError_NV('warning', Result, node);
end;

procedure RaiseTranslator(const msg: string);
begin
  raise ETranslator.Create(msg);
end;

procedure TranslateShader(const ObjFile: string; var glsl: string);
var output: string;
    params: string;
    newfile: string;
    sl: TStringList;
begin
  newfile := ObjFile+'.gobj';
  params := Format('-in=%s -out=%s -lang=%s -flags=%s', [ObjFile, newfile, HLSLcc_lang, HLSLcc_flags]);
  CreateHLSLccProcess(params, output);
  WriteLn(output);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(newfile);
    DeleteFile(newfile);
    glsl := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure RaiseGLSL(msg: string);
begin
    raise EGLSL.Create(msg);
end;

procedure CompileGLSL(const filename, source: string; stype: TShaderType);
var response: ISuperObject;
    http: TIdHTTP;
    sl: TStringList;
begin
  http := nil;
  sl := nil;
  try
    http := TIdHTTP.Create(nil);
    sl := TStringList.Create;
    case stype of
        stGeom: sl.Values['geomtarget'] := 'TRUE';
        stVert: sl.Values['verttarget'] := 'TRUE';
        stFrag: sl.Values['fragtarget'] := 'TRUE';
    end;
    sl.Values['code'] := source;
    sl.Values['filename'] := filename;
    response :=  SO(http.Post('http://'+cCompileServer_GLSL+':11580/compile', sl));
//    Writeln(ReparseLog(response.S['log'], filenode));
    Writeln(response.S['log']);
    if response.S['status'] <> 'OK' then
      RaiseGLSL(filename);
  finally
    sl.Free;
    http.Free;
  end;
end;

procedure RaiseHLSL(msg: string);
begin
    raise EHLSL.Create(msg);
end;

procedure OutputHLSLStats(filename: string);
var fs: TFileStream;
    data: TBytes;
    reflection: ID3D10ShaderReflection;
    shaderdesc: TD3D10_ShaderDesc;
begin
  fs := TFileStream.Create(filename, fmOpenRead);
  try
    SetLength(data, fs.Size);
    fs.Read(data[0], fs.Size);
    D3D10ReflectShader(@data[0], Length(data), reflection);
    reflection.GetDesc(shaderdesc);

    Writeln('    CBuffers     : ', shaderdesc.ConstantBuffers);
    Writeln('    Constants    : ', shaderdesc.DefCount);
    Writeln('    Attributes   : ', shaderdesc.DclCount);
    Writeln('    Resources    : ', shaderdesc.BoundResources);
    Writeln('    Instructions : ', shaderdesc.InstructionCount);
    Writeln('    Static  flow : ', shaderdesc.StaticFlowControlCount);
    Writeln('    Dynamic flow : ', shaderdesc.DynamicFlowControlCount);
  finally
    FreeAndNil(fs);
  end;
end;

function CompileHLSL(const filename, entry: string; stype: TShaderType; const Node: IFileNode): string;
var
    Params: string;
    target: string;
    ec: Cardinal;
    output: string;
begin
    case stype of
        stGeom: target := 'gs_4_0';
        stVert: target := 'vs_4_0';
        stFrag: target := 'ps_4_0';
    end;
    Result := filename + '.sob';
    {$IFDEF DEBUGHLSLCC}
    Params      := fxcparams + ' /E'+entry+' /T'+target+' /Fo "'+Result+'" /Fc "'+Result+'.asm" "'+filename+'"';
    {$ELSE}
    Params      := fxcparams + ' /E'+entry+' /T'+target+' /Fo "'+Result+'" '+filename+'"';
    {$ENDIF}
    ec := CreateFxcProcess(Params, output);
    if output <> '' then
    begin
        output := ReparseFxcLog(output, Node);
        Write(output);
    end;

    if ec <> 0 then
    begin
      ExitCode := ec;
      RaiseHLSL(filename);
    end;

    If OutputShaderStats Then OutputHLSLStats(Result);
end;

procedure LinkGLSL(const filename: string; const name: string; const Shaders: TShadersString);
const   ShaderCount: array [TShaderType] of string = ('vertcnt', 'geomcnt', 'fragcnt');
        ShaderParam: array [TShaderType] of string = ('vert0', 'geom0', 'frag0');
        JSONName: array [TShaderType] of string = ('vertex', 'geometry', 'fragment');
var sl: TStringList;
    http: TIdHTTP;
    response: ISuperObject;
    glsl: ISuperObject;
    i: TShaderType;
begin
  sl := nil;
  http := nil;
  try
    sl := TStringList.Create;
    sl.Values['programname'] := name;
    for i := Low(TShaderType) To High(TShaderType) Do
    begin
        if Shaders[i] = '' then Continue;
        sl.Values[ShaderCount[i]] := '1';
        sl.Values[ShaderParam[i]] := Shaders[i];
    end;

    http := TIdHTTP.Create(nil);
    response := SO(http.Post('http://'+cCompileServer_GLSL+':11580/link', sl));
    Writeln(response.S['log']);
    Writeln;
    if response.S['status'] <> 'OK' then
    begin
      ExitCode := 3;
      RaiseGLSL(filename);
    end;

    glsl := SO();
    glsl.S['name'] := Name;
    for i := Low(TShaderType) To High(TShaderType) Do
    begin
        if Shaders[i] = '' then Continue;
        glsl.S[JSONName[i]] := Shaders[i];
    end;
    glsl.SaveTo(filename, True, True);
  finally
    FreeAndNil(sl);
    FreeAndNil(http);
  end;
end;

procedure LinkHLSL(const filename: string; const name: string; const ShaderFile: TShadersString);
    procedure StreamWriteString(const stream: TStream; const str: string);
    var n: Integer;
    begin
        n := Length(str);
        Stream.WriteBuffer(n, SizeOf(n));
        stream.WriteBuffer(str[1], Length(str) * SizeOf(Char));
    end;
    procedure StreamWriteSubstream(const stream, substream: TStream);
    var n: Integer;
    begin
        if Assigned(substream) then
        begin
            substream.Position := 0;
            n := substream.Size;
            Stream.WriteBuffer(n, SizeOf(n));
            stream.CopyFrom(substream, substream.Size);
        end
        else
        begin
            n := 0;
            Stream.WriteBuffer(n, SizeOf(n));
        end;
    end;
var fs, shader: TFileStream;
    i: TShaderType;
begin
  fs := TFileStream.Create(filename, fmCreate);
  try
    try
      StreamWriteString(fs, filename);
      for i := Low(TShaderType) to High(TShaderType) do
      begin
        if FileExists(ShaderFile[i]) then
            shader := TFileStream.Create(ShaderFile[i], fmOpenRead)
        else
            shader := nil;
        StreamWriteSubstream(fs, shader);
        FreeAndNil(shader);
      end;
    finally
      FreeAndNil(fs);
    end;
  except
    DeleteFile(filename);
    raise TObject(AcquireExceptionObject);
  end;
end;

procedure CleanUpDir;
var files: TStringList;
    sr: TSearchRec;
    i: Integer;
begin
  files := TStringList.Create;
  try
    if FindFirst('*.vert', faAnyFile, sr) = 0 then
    begin
      repeat
        if sr.Attr = faDirectory then Continue;
        files.Add(sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
    if FindFirst('*.frag', faAnyFile, sr) = 0 then
    begin
      repeat
        if sr.Attr = faDirectory then Continue;
        files.Add(sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;    

    for i := 0 to files.Count - 1 do 
      DeleteFile (files.Strings[i]);
  finally
    files.Free;
  end;
end;

procedure SaveStringFile(const filename: string; const text: string);
var sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := text;
    sl.SaveToFile(filename);
  finally
    FreeAndNil(sl);
  end;
end;

procedure CompileProgram(const prog: TProgramInfo; out HLSL_Prog, GLSL_Prog: string);
var filename: string;
    oldWorkDir: string;
    NodeMan: IFileNodeManager;
    Node: array [TShaderType] of IFileNode;
    hlsl, glsl: string;
    hlslObjFile: TShadersString;
    glslObjFile: TShadersString;
    i: TShaderType;
begin
  NodeMan := GetFileNodeManager;
  for i := Low(TShaderType) to High(TShaderType) do
    Node[i] := NodeMan.GetFileNode(NodeMan.GetFullFileName(prog.Shader[i]), nil);

  oldWorkDir := GetCurrentDir;
  SetCurrentDir(prog.OutDir);
  try
    for i := Low(TShaderType) to High(TShaderType) do
    begin
      if Node[i] = nil then Continue;

      hlsl := Node[i].FullText;
      filename := prog.DXPrefix + ExtractFileName(prog.Shader[i]);
      SaveStringFile(filename, hlsl);
      hlslObjFile[i] := CompileHLSL(filename, prog.Entry[i], i, Node[i]);

      if prog.OGLTranslate then
      begin
          TranslateShader(hlslObjFile[i], glsl);
          filename := prog.OGLPrefix + ExtractFileName(prog.Shader[i]);
          SaveStringFile(filename, glsl);
          CompileGLSL(filename, glsl, i);
          glslObjFile[i] := glsl;
      end;
    end;

    HLSL_Prog := prog.DXPrefix + prog.Name + '.hlsl';
    LinkHLSL(HLSL_Prog, prog.Name, hlslObjFile);

    if prog.OGLTranslate then
    begin
        GLSL_Prog := prog.OGLPrefix + prog.Name + '.glsl';
        LinkGLSL(GLSL_Prog, prog.OGLPrefix + prog.Name, glslObjFile);
    end
    else
        GLSL_Prog := '';

    for i := Low(TShaderType) to High(TShaderType) do
    begin
      {$IFNDEF DEBUGHLSLCC}
      DeleteFile(hlslObjFile[i]);
      {$ENDIF}
    end;
  finally
    SetCurrentDir(oldWorkDir);
  end;
end;

procedure Compile(const task: ICompileTask); overload;
var i: Integer;
    pinfo: TProgramInfo;
    hlsl, glsl: string;
    RC: TStringList;
begin
    RC := nil;
    try
        RC := TStringList.Create;

        CreateFileNodeManager(task);
        for i := 0 to task.ProgramsCount - 1 do
        begin
            pinfo := task.GetProgram(i);
            task.SetLocalDomain(i);
            if not DirectoryExists(pinfo.OutDir) then ForceDirectories(pinfo.OutDir);
            CompileProgram(pinfo, hlsl, glsl);
            RC.Add(pinfo.DXPrefix  + pinfo.Name + ' RCDATA ' + '"' + pinfo.OutDir + '\' + hlsl + '"');
            if pinfo.OGLTranslate then
                RC.Add(pinfo.OGLPrefix + pinfo.Name + ' RCDATA ' + '"' + pinfo.OutDir + '\' + glsl + '"');
        end;
        RC.SaveToFile(task.ProjectName + '.rc');
    finally
        RC.Free;
    end;
end;

procedure Clean(const task: ICompileTask); overload;
//var i: Integer;
//    pinfo: TProgramInfo;
//    oldWorkDir: string;
begin
{
    for i := 0 to task.ProgramsCount - 1 do
    begin
        pinfo := task.GetProgram(i);
        task.SetLocalDomain(i);
        if not DirectoryExists(pinfo.OutDir) then Continue;
        oldWorkDir := GetCurrentDir;
        SetCurrentDir(pinfo.OutDir);
        try
          DeleteFile(pinfo.DXPrefix + pinfo.Shader);
          DeleteFile(pinfo.DXPrefix + pinfo.Fragment);
//          DeleteFile(pinfo.DXPrefix + pinfo.Name + '.hlsl.fx');
          DeleteFile(pinfo.OGLPrefix + pinfo.Vertex);
          DeleteFile(pinfo.OGLPrefix + pinfo.Fragment);
        finally
          SetCurrentDir(oldWorkDir);
        end;
    end;
}
end;

procedure DoTest;
var http: TIdHTTP;
    sl: TStringList;
begin
  http := nil;
  sl := nil;
  try
    http := TIdHTTP.Create(nil);
    sl := TStringList.Create;
    sl.Values['vert'] := 'void main() {};';
    sl.Values['frag'] := 'void somefunc(){{; void somefunc(); void main() { somefunc(); };';
    WriteLn(http.Post('http://'+cCompileServer_GLSL+':11580', sl));
    ReadLn;
  finally
    sl.Free;
    http.Free;
  end;
end;

procedure DoWork;
var src: string;
    task: ICompileTask;
begin
  If ParamCount < 1 Then Raise EInvalidOperation.Create('wrong params');
  src := ParamStr(1);
  If not FileExists(src) Then Raise EInvalidOperation.CreateFmt('file %s not found', [src]);
  task := GetCompileTask(src);
  if LowerCase(ParamStr(2)) = 'clean' then
    Clean(task)
  else
    Compile(task);
end;

begin
  try
    WorkingDir := GetCurrentDir;
    DoWork;
  except
    on E: EPlacedParseError do
    begin
      Writeln(Format('%s:%d:%d: %s', [ExtractRelativePath(GetCurrentDir, E.FileName), E.RowNum, E.ColNum, E.Message]));
    end;
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      ExitCode := 1;
    end;
  end;
end.
