unit untMain;

{$Include Options.inc}

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils, CompileTask, apccUtils,
  HLSLCompiler, GLSLCompiler,
  {$ifdef fpc}
    avTypes;
  {$else}
    davTypes;
  {$endif}

type
  ETranslator = class(Exception);

procedure DoWork;

implementation

procedure RaiseTranslator(const msg: string);
begin
  raise ETranslator.Create(msg);
end;

procedure RaiseCompileTask(const msg: string);
begin
  raise ECompileTaskError.Create(msg);
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

procedure TranslateShader(const prog: TProgramInfo; const ObjFile: string; out glsl: string);
var output: string;
    params: string;
    newfile: string;
    sl: TStringList;
begin
  WriteLn('Translating: "', ExtractFileName(ObjFile), '" ... ');
  newfile := ObjFile+'.gobj';
  params := Format('-in=%s -out=%s -lang=%d -flags=%d', [ObjFile, newfile, prog.HLSLcc_lang, prog.HLSLcc_flags]);
  if CreateHLSLccProcess(params, output) <> 0 then
    RaiseTranslator('HLSLcc.exe execution failed');
  WriteLn(output);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(newfile);
    DeleteFile(newfile);
    glsl := sl.Text;
  finally
    sl.Free;
  end;
  WriteLn('done.');
end;

procedure CompileProgram(const prog: TProgramInfo; out HLSL_Prog, GLSL_Prog: string);
var filename: string;
    glsl: string;
    hlslObjFile: TShadersString;
    glslObjFile: TShadersString;
    i: TShaderType;
begin
  for i := Low(TShaderType) to High(TShaderType) do
  begin
    if i = stUnknown then Continue;
    if prog.Shader[i] = '' then Continue;

    filename := prog.FullFileName(prog.Shader[i]);
    if filename = '' then
      RaiseCompileTask('file "'+prog.Shader[i]+'" not found');

    hlslObjFile[i] := prog.OutDir + '\' + ShaderType_Name[i] + '_' + ExtractFileName(prog.Shader[i]) + '.sob';
    CompileHLSL(prog, i, hlslObjFile[i]);

    if prog.OGLTranslate then
    begin
        TranslateShader(prog, hlslObjFile[i], glsl);
        {$IFDEF DEBUGHLSLCC}
        SaveStringFile(prog.OutDir + '\' + API_Prefix[apiOGL] + ExtractFileName(prog.Shader[i]), glsl);
        {$ENDIF}
        glslObjFile[i] := glsl;
        CompileGLSL(glsl, i);
    end;
  end;

  HLSL_Prog := prog.OutDir + '\' + API_Prefix[apiDX11] + prog.Name + API_Suffix[apiDX11];
  LinkHLSL(prog, hlslObjFile, HLSL_Prog);

  if prog.OGLTranslate then
  begin
      GLSL_Prog := prog.OutDir + '\' + API_Prefix[apiOGL] + prog.Name + API_Suffix[apiOGL];
      LinkGLSL(prog, glslObjFile, GLSL_Prog);
  end
  else
      GLSL_Prog := '';

  for i := Low(TShaderType) to High(TShaderType) do
  begin
    {$IFNDEF DEBUGHLSLCC}
    DeleteFile(hlslObjFile[i]);
    {$ENDIF}
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
        for i := 0 to task.ProgramsCount - 1 do
        begin
            pinfo := task.GetProgram(i);
            if pinfo.OutDir <> '' then
              if not DirectoryExists(pinfo.OutDir) then ForceDirectories(pinfo.OutDir);
            CompileProgram(pinfo, hlsl, glsl);

            RC.Add(API_Prefix[apiDX11]  + pinfo.Name + ' RCDATA ' + '"' + hlsl + '"');
            if pinfo.OGLTranslate then
                RC.Add(API_Prefix[apiOGL] + pinfo.Name + ' RCDATA ' + '"' + glsl + '"');

            WriteLn('--------------');
        end;
        RC.SaveToFile(task.ProjectName + '.rc');
    finally
        RC.Free;
    end;
end;

procedure DoWork;
var src: string;
    task: ICompileTask;
begin
  if ParamCount < 1 then raise EInvalidOperation.Create('wrong params');
  src := ParamStr(1);
  if not FileExists(src) then raise EInvalidOperation.CreateFmt('file %s not found', [src]);
  task := GetCompileTask(src);
  if LowerCase(ParamStr(2)) = 'clean' then
// ToDo Clean(task)
  else
    Compile(task);
end;

end.

