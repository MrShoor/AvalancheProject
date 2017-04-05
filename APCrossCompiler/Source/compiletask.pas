unit CompileTask;

{$ifdef fpc}
  {$mode objfpc}{$H+}
{$endif}

interface

uses
  Classes, SysUtils,
  {$ifdef fpc}
    avTypes, avContnrs;
  {$else}
    davTypes, Generics.Collections;
  {$endif}

type
  TShadersString = array [TShaderType] of string;
{$ifdef fpc}
  TPathList = specialize TArray<string>;
  IPathList = specialize IArray<string>;
{$else}
  TPathList = TStringList;
  IPathList = TStringList;
{$endif}

  ECompileTaskError = class (Exception);

  TProgramInfo = class
      fxc_params        : string;
      OGLTranslate      : Boolean;
      HLSLcc_lang       : Integer;
      HLSLcc_flags      : Cardinal;
      HLSLFlags         : Cardinal;

      OutStats          : Boolean;

      Name              : string;
      Shader            : array [TShaderType] of string;
      Entry             : array [TShaderType] of string;
      Target            : array [TShaderType] of string;
      OutDir            : string;
      Path              : IPathList;

      DXCompileStatus    : Boolean;
      OGLTranslateStatus : Boolean;
      OGLCompileStatus   : Boolean;

      function FullFileName(const FileName: string): string;

      constructor Create;
  end;

  ICompileTask = interface
  ['{8BC6ABC8-A99A-4D7D-8293-164D721A0A84}']
      function ProjectName: string;
      function ProgramsCount: Integer;
      function GetProgram(const index: Integer): TProgramInfo;
  end;

function GetCompileTask(const filename: string): ICompileTask;

implementation

uses
  SuperObject;

type
  {$ifdef fpc}
    TProgramList = specialize TArray<TProgramInfo>;
    IProgramList = specialize IArray<TProgramInfo>;

    TFLAGS_HLSLcc = specialize THashMap<string, Cardinal>;
    IFLAGS_HLSLcc = specialize IHashMap<string, Cardinal>;

    TFLAGS_HLSL = specialize THashMap<string, Cardinal>;
    IFLAGS_HLSL = specialize IHashMap<string, Cardinal>;
  {$else}
    TProgramList = TList<TProgramInfo>;
    IProgramList = TList<TProgramInfo>;

    TFLAGS_HLSLcc = TDictionary<string, Cardinal>;
    IFLAGS_HLSLcc = TDictionary<string, Cardinal>;
  {$endif}
var
  FLAGS_HLSLcc : IFLAGS_HLSLcc;
  FLAGS_HLSL : IFLAGS_HLSL;

function GetHLSLccFlag(const s: string): Cardinal;
begin
  if FLAGS_HLSLcc = nil then
  begin
    FLAGS_HLSLcc := TFLAGS_HLSLcc.Create;
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_UNIFORM_BUFFER_OBJECT', $01);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_ORIGIN_UPPER_LEFT', $02);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_PIXEL_CENTER_INTEGER', $04);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_GLOBAL_CONSTS_NEVER_IN_UBO', $08);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_GS_ENABLED', $10);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_TESS_ENABLED', $20);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_DUAL_SOURCE_BLENDING', $40);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_INOUT_SEMANTIC_NAMES', $80);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_INOUT_APPEND_SEMANTIC_NAMES', $100);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_COMBINE_TEXTURE_SAMPLERS', $200);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_DISABLE_EXPLICIT_LOCATIONS', $400);
    FLAGS_HLSLcc.Add('HLSLCC_FLAG_DISABLE_GLOBALS_STRUCT', $800);
    FLAGS_HLSLcc.add('HLSLCC_FLAG_DISABLE_VULKAN_DUMMIES', $1000);
  end;
  if not FLAGS_HLSLcc.TryGetValue(s, Result) then Result := 0;
end;

function GetHLSLFlag(const s: string): Cardinal;
begin
  if FLAGS_HLSL = nil then
  begin
    FLAGS_HLSL := TFLAGS_HLSLcc.Create;
    FLAGS_HLSL.Add('D3DCOMPILE_DEBUG', 1 shl 0);
    FLAGS_HLSL.Add('D3DCOMPILE_SKIP_VALIDATION', 1 shl 1);
    FLAGS_HLSL.Add('D3DCOMPILE_SKIP_OPTIMIZATION', 1 shl 2);
    FLAGS_HLSL.Add('D3DCOMPILE_PACK_MATRIX_ROW_MAJOR', 1 shl 3);
    FLAGS_HLSL.Add('D3DCOMPILE_PACK_MATRIX_COLUMN_MAJOR', 1 shl 4);
    FLAGS_HLSL.Add('D3DCOMPILE_PARTIAL_PRECISION', 1 shl 5);
    FLAGS_HLSL.Add('D3DCOMPILE_FORCE_VS_SOFTWARE_NO_OPT', 1 shl 6);
    FLAGS_HLSL.Add('D3DCOMPILE_FORCE_PS_SOFTWARE_NO_OPT', 1 shl 7);
    FLAGS_HLSL.Add('D3DCOMPILE_NO_PRESHADER', 1 shl 8);
    FLAGS_HLSL.Add('D3DCOMPILE_AVOID_FLOW_CONTROL', 1 shl 9);
    FLAGS_HLSL.Add('D3DCOMPILE_PREFER_FLOW_CONTROL', 1 shl 10);
    FLAGS_HLSL.Add('D3DCOMPILE_ENABLE_STRICTNESS', 1 shl 11);
    FLAGS_HLSL.Add('D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY', 1 shl 12);
    FLAGS_HLSL.add('D3DCOMPILE_IEEE_STRICTNESS', 1 shl 13);
    FLAGS_HLSL.add('D3DCOMPILE_OPTIMIZATION_LEVEL0', 1 shl 14);
    FLAGS_HLSL.add('D3DCOMPILE_OPTIMIZATION_LEVEL1', 0);
    FLAGS_HLSL.add('D3DCOMPILE_OPTIMIZATION_LEVEL2', (1 shl 14) or (1 shl 15));
    FLAGS_HLSL.add('D3DCOMPILE_OPTIMIZATION_LEVEL3', 1 shl 15);
    FLAGS_HLSL.add('D3DCOMPILE_WARNINGS_ARE_ERRORS', 1 shl 18);
  end;
  if not FLAGS_HLSL.TryGetValue(s, Result) then Result := 0;
end;

type

  { TCompileTask }

  TCompileTask = class (TInterfacedObject, ICompileTask)
  private
    FProjectName        : string;
    FDefaultProgramInfo : TProgramInfo;

    FPrograms : IProgramList;

    procedure RaiseError(const msg: string);

    function ParseProgramFromJSON(const sobj: ISuperObject; IsDefaultProgram: Boolean): TProgramInfo;
    procedure ParseJSON(const sobj: ISuperObject);
  public
    function ProjectName: string;
    function ProgramsCount: Integer;
    function GetProgram(const index: Integer): TProgramInfo;

    constructor Create(const filename: string);
    destructor Destroy; override;
  end;

function GetCompileTask(const filename: string): ICompileTask;
begin
  Result := TCompileTask.Create(filename);
end;

{ TCompileTask }

procedure TCompileTask.RaiseError(const msg: string);
begin
  raise ECompileTaskError.Create(msg);
end;

function TCompileTask.ParseProgramFromJSON(const sobj: ISuperObject; IsDefaultProgram: Boolean): TProgramInfo;
  function TryGetBool(const sobj: ISuperObject; ValueName: string; DefValue: Boolean): Boolean;
  begin
    if sobj.O[ValueName] = nil then
      Result := DefValue
    else
      Result := sobj.B[ValueName];
  end;
  function TryGetInt(const sobj: ISuperObject; ValueName: string; DefValue: Integer): Integer;
  begin
    if sobj.O[ValueName] = nil then
      Result := DefValue
    else
      Result := sobj.I[ValueName];
  end;
  function TryGetStr(const sobj: ISuperObject; ValueName: string; DefValue: string): string;
  begin
    if sobj.O[ValueName] = nil then
      Result := DefValue
    else
      Result := sobj.S[ValueName];
  end;

var path: ISuperObject;
    flags: ISuperObject;
    I: Integer;
    sarr: TSuperArray;
    st: TShaderType;
begin
  Result := TProgramInfo.Create;
  if IsDefaultProgram then FDefaultProgramInfo := Result;

  path := sobj.O['Path'];
  if path <> nil Then
  begin
    if Result.Path = nil then Result.Path := TPathList.Create;

    case path.DataType of
      stNull    : RaiseError('null is wrong "path" type');
      stBoolean : RaiseError('boolean is wrong "path" type');
      stDouble  : RaiseError('double is wrong "path" type');
      stCurrency: RaiseError('currency is wrong "path" type');
      stInt     : RaiseError('int is wrong "path" type');
      stObject  : RaiseError('object is wrong "path" type');
      stArray   : begin
                    sarr := path.AsArray;
                    for I := 0 To sarr.Length - 1 do
                    begin
                      If sarr.O[I].DataType <> stString then RaiseError('wrong item type for ' + IntToStr(I) + ' element');
                      Result.Path.Add(sarr.S[I]);
                    end;
                  end;
      stString  : Result.Path.Add(path.AsString);
      stMethod  : RaiseError('method is wrong "path" type');
    else
      RaiseError('Wat???');
    end;
  end;

  if not IsDefaultProgram then
  begin
    if Assigned(FDefaultProgramInfo.Path) then
      for i := 0 to FDefaultProgramInfo.Path.Count - 1 do
      begin
        if Result.Path = nil then Result.Path := TPathList.Create;
        Result.Path.Add(FDefaultProgramInfo.Path[i]);
      end;
  end;

  Result.fxc_params   := TryGetStr (sobj, 'fxc_params', FDefaultProgramInfo.fxc_params);
  Result.HLSLcc_lang  := TryGetInt (sobj, 'HLSLcc_lang', FDefaultProgramInfo.HLSLcc_lang);
  Result.OGLTranslate := TryGetBool(sobj, 'OGLTranslate', FDefaultProgramInfo.OGLTranslate);
  Result.OutStats     := TryGetBool(sobj, 'OutputShaderStats', FDefaultProgramInfo.OutStats);

  Result.HLSLcc_flags := 0;
  flags := sobj.O['HLSLcc_flags'];
  if flags <> nil then
  begin
    sarr := flags.AsArray;
    for I := 0 to sarr.Length - 1 do
      Result.HLSLcc_flags :=  Result.HLSLcc_flags or GetHLSLccFlag(sarr.S[I]);
    Result.HLSLcc_flags := Result.HLSLcc_flags or $1000; //forced HLSLCC_FLAG_DISABLE_VULKAN_DUMMIES flag
  end
  else
    if not IsDefaultProgram then
      Result.HLSLcc_flags := FDefaultProgramInfo.HLSLcc_flags;

  Result.HLSLFlags := 0;
  flags := sobj.O['HLSLFlags'];
  if flags <> nil then
  begin
    sarr := flags.AsArray;
    for I := 0 to sarr.Length - 1 do
      Result.HLSLFlags :=  Result.HLSLFlags or GetHLSLFlag(sarr.S[I]);
    Result.HLSLFlags := Result.HLSLFlags or $1000; //forced HLSLCC_FLAG_DISABLE_VULKAN_DUMMIES flag
  end
  else
    if not IsDefaultProgram then
      Result.HLSLFlags := FDefaultProgramInfo.HLSLFlags
    else
      Result.HLSLFlags := GetHLSLFlag('D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY') or GetHLSLFlag('D3DCOMPILE_OPTIMIZATION_LEVEL3');

  for st := Low(TShaderType) to High(TShaderType) do
    Result.Entry[st] := TryGetStr(sobj, ShaderType_Name[st]+'Entry', FDefaultProgramInfo.Entry[st]);

  for st := Low(TShaderType) to High(TShaderType) do
    Result.Target[st] := TryGetStr(sobj, ShaderType_Name[st]+'Target', FDefaultProgramInfo.Target[st]);

  Result.OutDir := TryGetStr(sobj, 'OutDir', FDefaultProgramInfo.OutDir);

  if not IsDefaultProgram then
  begin
    if sobj.O['Name'] = nil then RaiseError('program "Name" field not found');
    Result.Name := sobj.S['Name'];
    for st := Low(TShaderType) to High(TShaderType) do
      Result.Shader[st] := TryGetStr(sobj, ShaderType_Name[st], '');
  end;
end;

procedure TCompileTask.ParseJSON(const sobj: ISuperObject);
var prog: ISuperObject;
    sarr: TSuperArray;
    I: Integer;
begin
  prog := sobj.O['Programs'];
  if prog = nil then RaiseError('no programs for compile');
  if prog.DataType <> stArray then RaiseError('wrong "programs" type');

  FDefaultProgramInfo := ParseProgramFromJSON(sobj, True);

  sarr := prog.AsArray;
  for i := 0 to sarr.Length - 1 do
  begin
    if sarr.O[I].DataType <> stObject then RaiseError('wrong item type for ' + IntToStr(I) + ' element');
    FPrograms.Add(ParseProgramFromJSON(sarr.O[I], False));
  end;
end;

function TCompileTask.ProjectName: string;
begin
  Result := FProjectName;
end;

function TCompileTask.ProgramsCount: Integer;
begin
  Result := FPrograms.Count;
end;

function TCompileTask.GetProgram(const index: Integer): TProgramInfo;
begin
  Result := FPrograms[index];
end;

constructor TCompileTask.Create(const filename: string);
var sl: TStringList;
begin
  FProjectName := ChangeFileExt(ExtractFileName(filename), '');

  FPrograms := TProgramList.Create;

  if not FileExists(filename) then
    RaiseError('File: "' + filename + '" not found');

  sl := TStringList.Create;
  try
    sl.LoadFromFile(filename);
    ParseJSON(SO(sl.Text));
  finally
    sl.Free;
  end;
end;

destructor TCompileTask.Destroy;
var
  i: Integer;
begin
  for i := 0 to FPrograms.Count - 1 do
    FPrograms[i].Free;
  FPrograms.Clear();

  FreeAndNil(FDefaultProgramInfo);
  inherited Destroy;
end;

{ TProgramInfo }

constructor TProgramInfo.Create;
begin
  HLSLcc_flags := 7296;
end;

function TProgramInfo.FullFileName(const FileName: string): string;
var I: Integer;
begin
  if FileExists(FileName) then
    Exit(ExpandFileName(FileName));

  if Assigned(Path) then
    for I := 0 to Path.Count - 1 do
    begin
      Result := Path[I] + '\' + FileName;
      if FileExists(Result) then Exit;
    end;

  Result := '';
end;

end.

