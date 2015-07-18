unit untCompileTask;

interface

uses
    SysUtils, Generics.Collections, Classes, SuperObject;

{$IFDEF WIN32}
const  DirectorySeparator = '\';
{$ELSE}
const  DirectorySeparator = '/';
{$ENDIF}

type
    EProjParseError = class (Exception);

type
    TShaderType = (stVert, stGeom, stFrag);

    TProgramInfo = record
        OGLTranslate      : Boolean;
        DXPrefix          : string;
        OGLPrefix         : string;
        Name              : string;
        Shader            : array [TShaderType] of string;
        Entry             : array [TShaderType] of string;
        OutDir            : string;
        Path              : array of string;
    end;

    ICompileTask = interface
    ['{8BC6ABC8-A99A-4D7D-8293-164D721A0A84}']
        function ProjectName: string;
        function ProgramsCount: Integer;
        function GetProgram(const index: Integer): TProgramInfo;
        procedure SetLocalDomain(const index: Integer);
        function GetFilePath(const filename: string): string;
    end;

function GetCompileTask(const filename: string): ICompileTask;
function fxcparams: string;
function HLSLcc_lang: string;
function HLSLcc_flags: string;
function OutputShaderStats : Boolean;

implementation

type
    TCompileTask = class (TInterfacedObject, ICompileTask)
    private
        FOGLTranslate  : Boolean;
        FProjectName   : string;
        FOutDir        : string;
        FDXPrefix      : string;
        FOGLPrefix     : string;
        FVertexEntry   : string;
        FFragmentEntry : string;
        FGeometryEntry : string;

        FDirectories : TStringList;
        FPrograms    : TList<TProgramInfo>;

        FLocalProgram: Integer;

        procedure RaiseError(const msg: string);

        procedure ParseProgram(const sobj: ISuperObject);
        procedure ParseJSON(const sobj: ISuperObject);
        procedure LoadFromFile(const filename: string);
    public
        function ProjectName: string;
        function ProgramsCount: Integer;
        function GetProgram(const index: Integer): TProgramInfo;
        procedure SetLocalDomain(const index: Integer);
        function GetFilePath(const filename: string): string;

        constructor Create(const filename: string);
        destructor Destroy; override;
    end;

var gvFXCParams: string;
    gvHLSLcc_lang : Integer;
    gvHLSLcc_flags: Cardinal;
    gvOutputShaderStats: Boolean;
    FLAGS_HLSLcc: TDictionary<string, Cardinal>;

function fxcparams: string;
begin
  Result := gvFXCParams;
end;

function HLSLcc_lang: string;
begin
  Result := IntToStr(gvHLSLcc_lang);
end;

function HLSLcc_flags: string;
begin
  Result := IntToStr(gvHLSLcc_flags);
end;

function OutputShaderStats : Boolean;
begin
  Result := gvOutputShaderStats;
end;

procedure InitFlags;
begin
  FLAGS_HLSLcc := TDictionary<string, Cardinal>.Create;
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
end;

function GetCompileTask(const filename: string): ICompileTask;
begin
    Result := TCompileTask.Create(filename);
end;

{ TCompileTask }

constructor TCompileTask.Create(const filename: string);
begin
    FDirectories := TStringList.Create;
    FPrograms := TList<TProgramInfo>.Create;
    LoadFromFile(filename);

    FProjectName := ExtractFileName(filename);
    FProjectName := Copy(FProjectName, 1, Length(FProjectName) - Length(ExtractFileExt(FProjectName)));
end;

destructor TCompileTask.Destroy;
begin
    FreeAndNil(FPrograms);
    FreeAndNil(FDirectories);
    inherited;
end;

Function TCompileTask.GetFilePath(const filename: string): string;
Var i: Integer;
    prog: TProgramInfo;
Begin
  If FileExists(filename) Then
  Begin
      Result := filename;
      Exit;
  End;
  If (FLocalProgram >= 0) And (FLocalProgram < FPrograms.Count) Then
  Begin
    prog := FPrograms.Items[FLocalProgram];
    For I := 0 To Length(prog.Path) - 1 Do
    Begin
      Result := ExpandFileName(prog.Path[I] + DirectorySeparator + filename);
      If FileExists(Result) Then Exit;
    End;
  End;
  For I := 0 To FDirectories.Count - 1 Do
  Begin
      Result := ExpandFileName(FDirectories.Strings[I] + DirectorySeparator + filename);
      If FileExists(Result) Then Exit;
  End;
  Result := '';
End;

function TCompileTask.GetProgram(const index: Integer): TProgramInfo;
begin
   Result := FPrograms.Items[index];
end;

procedure TCompileTask.LoadFromFile(const filename: string);
var sl: TStringList;
    sobj: ISuperObject;
begin
    sl := TStringList.Create;
    try
        sl.LoadFromFile(filename);
        sobj := SO(sl.Text);
        ParseJSON(sobj);
    finally
        sl.Free;
    end;
end;

procedure TCompileTask.ParseJSON(const sobj: ISuperObject);
var path: ISuperObject;
    prog: ISuperObject;
    flags: ISuperObject;
    flag: Cardinal;
    sarr: TSuperArray;
    I: Integer;
begin
    FDirectories.Clear;
    FPrograms.Clear;

    path := sobj.O['path'];
    if path <> nil Then
    begin
        Case path.DataType Of
            stNull    : RaiseError('null is wrong "path" type');
            stBoolean : RaiseError('boolean is wrong "path" type');
            stDouble  : RaiseError('double is wrong "path" type');
            stCurrency: RaiseError('currency is wrong "path" type');
            stInt     : RaiseError('int is wrong "path" type');
            stObject  : RaiseError('object is wrong "path" type');
            stArray   : begin
                            sarr := path.AsArray;
                            For I := 0 To sarr.Length - 1 Do
                            begin
                                If sarr.O[I].DataType <> stString Then RaiseError('wrong item type for ' + IntToStr(I) + ' element');
                                FDirectories.Add(sarr.S[I]);
                            end;
                        end;
            stString  : FDirectories.Add(path.AsString);
            stMethod  : RaiseError('method is wrong "path" type');
        End;
    end;
    gvFXCParams := sobj.S['fxc_params'];
    gvHLSLcc_lang := sobj.I['HLSLcc_lang'];
    gvOutputShaderStats := sobj.B['OutputShaderStats'];

    gvHLSLcc_flags := 0;
    flags := sobj.O['HLSLcc_flags'];
    if flags <> nil then
    begin
        sarr := flags.AsArray;
        for I := 0 to sarr.Length - 1 do
            if FLAGS_HLSLcc.TryGetValue(sarr.S[I], flag) then gvHLSLcc_flags := gvHLSLcc_flags or flag;
    end;

    FOGLTranslate := sobj.B['OGLTranslate'];
    FDXPrefix := sobj.S['DXPrefix'];
    FOGLPrefix := sobj.S['OGLPrefix'];
    FOutDir := sobj.S['outdir'];
    FVertexEntry := sobj.S['vertexEntry'];
    FFragmentEntry := sobj.S['fragmentEntry'];
    FGeometryEntry := sobj.S['geometryEntry'];

    prog := sobj.O['programs'];
    If prog = nil Then RaiseError('no programs for compile');
    If prog.DataType <> stArray Then RaiseError('wrong "programs" type');
    sarr := prog.AsArray;
    For I := 0 To sarr.Length - 1 Do
    Begin
        If sarr.O[I].DataType <> stObject Then RaiseError('wrong item type for ' + IntToStr(I) + ' element');
        ParseProgram(sarr.O[I]);
    End;
end;

procedure TCompileTask.ParseProgram(const sobj: ISuperObject);
var prog: TProgramInfo;
    sarr: TSuperArray;
    I: Integer;
begin
    FillChar(prog, SizeOf(prog), 0);
    prog.Name := sobj.S['name'];
    prog.OutDir := sobj.S['outdir'];
    if prog.OutDir = '' then prog.OutDir := FOutDir;
    prog.DXPrefix := sobj.S['DXPrefix'];
    prog.OGLPrefix := sobj.S['OGLPrefix'];
    if prog.DXPrefix = '' then prog.DXPrefix := FDXPrefix;
    if prog.OGLPrefix = '' then prog.OGLPrefix := FOGLPrefix;
    if LowerCase(prog.DXPrefix) = LowerCase(prog.OGLPrefix) then RaiseError(prog.Name + ': DXPrefix match with OGLPrefix');
    prog.Shader[stGeom] := sobj.S['geometry'];
    prog.Shader[stVert] := sobj.S['vertex'];
    if prog.Shader[stVert] = '' then RaiseError(prog.Name + ': vertex program can''t be empty');
    prog.Shader[stFrag] := sobj.S['fragment'];
    if prog.Shader[stFrag] = '' then RaiseError(prog.Name + ': fragment program can''t be empty');
    prog.Entry[stGeom] := sobj.S['geometryEntry'];
    if prog.Entry[stGeom] = '' then prog.Entry[stGeom] := FGeometryEntry;
    if (prog.Entry[stGeom] = '') and (prog.Shader[stGeom] <> '') then RaiseError(prog.Name + ': geometry entry point can''t be empty');
    prog.Entry[stVert] := sobj.S['vertexEntry'];
    if prog.Entry[stVert] = '' then prog.Entry[stVert] := FVertexEntry;
    if prog.Entry[stVert] = '' then RaiseError(prog.Name + ': vertex entry point can''t be empty');
    prog.Entry[stFrag] := sobj.S['fragmentEntry'];
    if prog.Entry[stFrag] = '' then prog.Entry[stFrag] := FFragmentEntry;
    if prog.Entry[stFrag] = '' then RaiseError(prog.Name + ': fragment entry point can''t be empty');
    if Assigned(sobj.O['OGLTranslate']) then
        prog.OGLTranslate := sobj.B['OGLTranslate']
    else
        prog.OGLTranslate := FOGLTranslate;

    if sobj.O['path'] <> nil then
    begin
        If sobj.O['path'].DataType <> stArray Then RaiseError(prog.Name + ': wrong "path" type');
        sarr := sobj.O['path'].AsArray;
        SetLength(prog.Path, sarr.Length);
        For I := 0 To sarr.Length - 1 Do
        begin
            If sarr.O[I].DataType <> stString Then RaiseError(prog.Name + ': wrong item type for ' + IntToStr(I) + ' element');
            prog.Path[I] := sarr.S[I];
        end;
    end;
    FPrograms.Add(prog);
end;

function TCompileTask.ProgramsCount: Integer;
begin
    Result := FPrograms.Count;
end;

function TCompileTask.ProjectName: string;
begin
  Result := FProjectName;
end;

procedure TCompileTask.RaiseError(const msg: string);
begin
    raise EProjParseError.Create(msg);
end;

procedure TCompileTask.SetLocalDomain(const index: Integer);
begin
  FLocalProgram := index;
end;

initialization
InitFlags;

finalization
FLAGS_HLSLcc.Free;

end.
