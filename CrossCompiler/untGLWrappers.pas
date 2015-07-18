unit untGLWrappers;

interface

uses
  dglOpenGL, SysUtils;

type
  EWrapperError = class (Exception);

type
  IShader = interface
  ['{0EDDD5DB-094A-4958-AB1E-94D8B4E8D972}']
    function Handle: GLuint;
    function CopmileShader(code: AnsiString; var log: string): Boolean;
  end;

  IProgram = interface
  ['{D79F9CE2-743C-4A9B-A002-4AD6FE18C6CD}']
    procedure ClearShaders;
    procedure AddShader(shadertype: GLuint; shadercode: AnsiString);
    function Link(var log: string): Boolean;
    function Validate: Boolean;
  end;

function CreateShader(shadertype: GLuint): IShader;
function CreateProgram: IProgram;

implementation

type
  TShader = class (TInterfacedObject, IShader)
  private
    FShader: GLuint;
  public
    function Handle: GLuint;
    function CopmileShader(code: AnsiString; var log: string): Boolean;

    constructor Create(shadertype: GLuint);
    destructor Destroy; override;
  end;

  TProgram = class (TInterfacedObject, IProgram)
  private
    FShaders: TArray<IShader>;
    FProgram: GLuint;

    function Handle: GLuint;
  public
    procedure ClearShaders;
    procedure AddShader(shadertype: GLuint; shadercode: AnsiString);
    function Link(var log: string): Boolean;
    function Validate: Boolean;

    destructor Destroy; override;
  end;

function CreateShader(shadertype: GLuint): IShader;
begin
  Result := TShader.Create(shadertype);
end;

function CreateProgram: IProgram;
begin
  Result := TProgram.Create;
end;

{ TShader }

function TShader.CopmileShader(code: AnsiString; var log: string): Boolean;
  function GetShaderInfoLog: string;
  var n, dummy: Integer;
      astr: AnsiString;
  begin
    Result := '';
    glGetShaderiv(FShader, GL_INFO_LOG_LENGTH, @n);
    if n > 1 then
    begin
      SetLength(astr, n-1);
      glGetShaderInfoLog(FShader, n, dummy, PAnsiChar(astr));
      Result := string(astr);
    end;
  end;
var n, param: Integer;
begin
  n := Length(code);
  glShaderSource(FShader, 1, @code, @n);
  glCompileShader(FShader);
  glGetShaderiv(FShader, GL_COMPILE_STATUS, @param);
  Result := param = GL_TRUE;
  log := GetShaderInfoLog;
end;

constructor TShader.Create(shadertype: GLuint);
begin
  FShader := glCreateShader(shadertype);
  if FShader = 0 then raise EWrapperError.Create('can''t create shader program');
end;

destructor TShader.Destroy;
begin
  glDeleteShader(FShader);
  FShader := 0;
  inherited;
end;

function TShader.Handle: GLuint;
begin
  Result := FShader;
end;

{ TProgram }

procedure TProgram.AddShader(shadertype: GLuint; shadercode: AnsiString);
var n: Integer;
    dummy: string;
begin
  n := Length(FShaders);
  SetLength(FShaders, n + 1);
  FShaders[n] := CreateShader(shadertype);
  if not FShaders[n].CopmileShader(shadercode, dummy) then raise EWrapperError.Create('can''t compile shader' + sLineBreak + dummy);
  glAttachShader(Handle, FShaders[n].Handle);
end;

procedure TProgram.ClearShaders;
var shaders: array of GLuint;
    i: Integer;
    cnt: Integer;
begin
  if (Handle <> 0) then
  begin
    glGetProgramiv(Handle, GL_ATTACHED_SHADERS, @cnt);
//    cnt := Length(FShaders);
    if cnt>0 then
    begin
//      SetLength(shaders, cnt);
      glGetAttachedShaders(Handle, cnt, cnt, @shaders[0]);
      for i := 0 to Length(shaders) - 1 do
//        glDetachShader(Handle, FShaders[i].Handle);
        glDetachShader(Handle, shaders[i]);
      shaders:=nil;
    end;
  end;
  SetLength(FShaders, 0);
end;

destructor TProgram.Destroy;
begin
  if FProgram <> 0 then
  begin
//    ClearShaders;
    glDeleteProgram(FProgram);
    FProgram := 0;
  end;
  inherited;
end;

function TProgram.Handle: GLuint;
begin
  if FProgram = 0 then FProgram := glCreateProgram;
  Result := FProgram;
end;

function TProgram.Link(var log: string): Boolean;
  function GetProgramInfoLog: string;
  var n, dummy: Integer;
      astr: AnsiString;
  begin
    Result := '';
    glGetProgramiv(Handle, GL_INFO_LOG_LENGTH, @n);
    if n > 1 then
    begin
      SetLength(astr, n-1);
      glGetProgramInfoLog(Handle, n, dummy, PAnsiChar(astr));
      Result := string(astr);
    end;
  end;
var param: Integer;
begin
  glLinkProgram(Handle);
  glGetProgramiv(Handle, GL_LINK_STATUS, @param);
  Result := param = GL_TRUE;
  log := GetProgramInfoLog;
end;

function TProgram.Validate: Boolean;
var param: Integer;
begin
  Result := True;
  //http://www.opengl.org/sdk/docs/man/html/glValidateProgram.xhtml
  //glValidateProgram checks to see whether the executables contained in program can execute given the current OpenGL state.
  glValidateProgram(Handle);
  glGetProgramiv(Handle, GL_VALIDATE_STATUS, @param);
  Result := param = GL_TRUE;
end;

end.
