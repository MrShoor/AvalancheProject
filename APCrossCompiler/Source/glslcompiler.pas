unit GLSLCompiler;

{$IfDef fpc}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Windows, Messages, Classes, SysUtils, dglOpenGL, CompileTask, superobject,
  {$IfDef fpc}
    avTypes, avContnrs;
  {$Else}
    davTypes, Generics.Collections;
  {$EndIf}

type
  EGLSL = class(Exception);

procedure CompileGLSL(const code: AnsiString; st: TShaderType);
procedure LinkGLSL(const prog: TProgramInfo; code: TShadersString; const OutFile: string);

implementation
                                                    {stUnknown,    stVertex,         stGeometry,          stFragment}
const GL_ShaderType: array [TShaderType] of GLuint = (GL_ZERO, GL_VERTEX_SHADER, GL_GEOMETRY_SHADER, GL_FRAGMENT_SHADER);

type
  TGLContext = class
  private
    FWnd: HWND;
    FDC: HDC;
    FRC: HGLRC;
  public
    procedure Bind;
    procedure UnBind;

    constructor Create;
    destructor Destroy;
  end;

var
  GV_Context: TGLContext;

function AllocateDummyWindow: HWND;
const HiddenClassName = '{48A11D4A-C269-41ED-9ED9-D1A42110EB2B}';
var
  HiddenWindowClass: TWndClass;
begin
  HiddenWindowClass.hInstance := HInstance;
  if not GetClassInfo(HInstance, HiddenClassName, HiddenWindowClass) then
  begin
    HiddenWindowClass.style := 0;
    HiddenWindowClass.lpfnWndProc := @DefWindowProc;
    HiddenWindowClass.cbClsExtra := 0;
    HiddenWindowClass.cbWndExtra := 0;
    HiddenWindowClass.hInstance := 0;
    HiddenWindowClass.hIcon := 0;
    HiddenWindowClass.hCursor := 0;
    HiddenWindowClass.hbrBackground := 0;
    HiddenWindowClass.lpszMenuName := nil;
    HiddenWindowClass.lpszClassName := HiddenClassName;
    HiddenWindowClass.hInstance := HInstance;
    Windows.RegisterClass(HiddenWindowClass);
  end;
  Result := CreateWindowEx(WS_EX_TOOLWINDOW, HiddenClassName,'', WS_POPUP, 0, 0, 0, 0, 0, 0, HInstance, nil);
end;

procedure RaiseGLSL(msg: string);
begin
    raise EGLSL.Create(msg);
end;

procedure BindGlobalContext;
begin
  if GV_Context = nil then
  begin
    InitOpenGL;
    GV_Context := TGLContext.Create;
  end;
  GV_Context.Bind;
end;

procedure UnBindGlobalContext;
begin
  GV_Context.UnBind;
end;

function GetShaderCompileLog(const Shader: GLuint): string;
var Log: AnsiString;
    n, tmplen: GLint;
begin
    glGetShaderiv(Shader, GL_INFO_LOG_LENGTH, @n);
    if n>1 then
    begin
      SetLength(Log, n-1);
      glGetShaderInfoLog(Shader, n, tmplen, PAnsiChar(Log));
      Result := 'Shader compile log: ' + string(Log);
    end;
end;

procedure CompileGLSL(const code: AnsiString; st: TShaderType);
var Shader: GLuint;
    CompRes: GLint;
    codeLen: Integer;
    codePtr: PAnsiChar;
begin
  BindGlobalContext;
  Shader := glCreateShader(GL_ShaderType[st]);
  try
    codeLen := Length(code);
    codePtr := @code[1];
    glShaderSource(Shader, 1, @codePtr, @codeLen);
    glCompileShader(Shader);
    glGetShaderiv(Shader, GL_COMPILE_STATUS, @CompRes);
    if CompRes = GL_FALSE then
      RaiseGLSL(GetShaderCompileLog(Shader));
  finally
    WriteLn(GetShaderCompileLog(Shader));
    glDeleteShader(Shader);
    UnBindGlobalContext;
  end;
end;

procedure LinkGLSL(const prog: TProgramInfo; code: TShadersString; const OutFile: string);
  function CreateShader(const code: AnsiString; GLShaderType: GLUint): GLuint;
  var CompRes: GLint;
      codeLen: Integer;
      codePtr: PAnsiChar;
      errorlog: string;
  begin
    Result := glCreateShader(GLShaderType);
    codeLen := Length(code);
    codePtr := @code[1];
    glShaderSource(Result, 1, @codePtr, @codeLen);
    glCompileShader(Result);
    glGetShaderiv(Result, GL_COMPILE_STATUS, @CompRes);
    if CompRes = GL_FALSE then
    begin
      errorlog := GetShaderCompileLog(Result);
      glDeleteShader(Result);
      Result := 0;
      RaiseGLSL(errorlog);
    end;
  end;

var GLProg: GLuint;
    st: TShaderType;
    shaders: array [TShaderType] of GLuint;
    param: integer;

    sobj: ISuperObject;
begin
  BindGlobalContext;
  GLProg := glCreateProgram();
  try
    for st := Low(TShaderType) to High(TShaderType) do
    begin
      shaders[st] := 0;
      if st = stUnknown then Continue;
      if code[st] = '' then Continue;
      shaders[st] := CreateShader(code[st], GL_ShaderType[st]);
      glAttachShader(GLProg, shaders[st]);
    end;
    glLinkProgram(GLProg);
    glGetProgramiv(GLProg, GL_LINK_STATUS, @param);
    if param <> 1 then
      RaiseGLSL('Link failed');

    sobj := SO();
    for st := Low(TShaderType) to High(TShaderType) do
    begin
      if st = stUnknown then Continue;
      if code[st] = '' then Continue;
      sobj.S[ShaderType_Name[st]] := code[st];
    end;
    sobj.S['Name'] := prog.Name;
    sobj.SaveTo(OutFile, True, True);
  finally
    for st := Low(TShaderType) to High(TShaderType) do
    begin
      if shaders[st] <> 0 then
      begin
        glDetachShader(GLProg, shaders[st]);
        glDeleteShader(shaders[st]);
      end;
    end;
    glDeleteProgram(GLProg);
    UnBindGlobalContext;
  end;
end;

{ TGLContext }

procedure TGLContext.Bind;
begin
  wglMakeCurrent(FDC, FRC);
end;

constructor TGLContext.Create;
begin
  FWnd := AllocateDummyWindow;
  FDC := GetDC(FWnd);
  FRC := CreateRenderingContext(FDC, [], 32, 0, 0, 0, 0, 0);
  ActivateRenderingContext(FDC, FRC);
  DeactivateRenderingContext;
end;

destructor TGLContext.Destroy;
begin
  wglDeleteContext(FRC);
  ReleaseDC(FWnd, FDC);
  DestroyWindow(FWnd);
end;

procedure TGLContext.UnBind;
begin
  wglMakeCurrent(0,0);
end;

end.
