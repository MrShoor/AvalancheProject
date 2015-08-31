unit HLSLCompiler;

{$IfDef fpc}
  {$mode objfpc}{$H+}
  {$modeswitch advancedrecords}
{$EndIf}

interface

uses
  Windows, Classes, SysUtils, CompileTask, D3DCompiler_JSB, D3DCommon_JSB, D3D11_JSB, DXGI_JSB,
  {$IfDef fpc}
    avTypes, avContnrs, avContext;
  {$Else}
    davTypes, Generics.Collections;
  {$EndIf}

type
  EHLSL = class(Exception);

procedure RaiseHLSL(msg: string);
procedure CompileHLSL(const prog: TProgramInfo; st: TShaderType; const OutFile: string);
procedure LinkHLSL(const prog: TProgramInfo; shaders: TShadersString; const OutFile: string);

implementation

type

  { TIncludeAdapter }

  {$IfDef fpc}
  TIncludeAdapter = class(TObject, ID3DInclude)
  {$Else}
  TIncludeAdapter = class(ID3DInclude)
  {$EndIf}
  private type
    {$IfDef fpc}
    TIncludes = specialize THashMap<string, AnsiString, TMurmur2HashString>;
    IIncludes = specialize IHashMap<string, AnsiString, TMurmur2HashString>;
    {$Else}
    TIncludes = TDictionary<string, AnsiString>;
    IIncludes = TDictionary<string, AnsiString>;
    {$EndIf}
  private const
    NullData: AnsiString = ' ';
  private
    FProg: TProgramInfo;
    FIncludes: IIncludes;
  public
    function Open(IncludeType:TD3D_IncludeType;pFileName:PAnsiChar;pParentData:Pointer;ppData:PPointer;pBytes:PLongWord):HResult; {$IfNDef fpc} Override; {$EndIf} stdcall;
    function Close(pData:Pointer):HResult; {$IfNDef fpc} Override; {$EndIf} stdcall;

    constructor Create(const prog: TProgramInfo);
    destructor Destroy; override;
  end;

  { TChunkWriter }

  TChunkWriter = class
  private
    FStream: TStream;
    FPos: Int64;
  public
    constructor Create(fs: TStream; id: TFOURCC);
    destructor Destroy; override;
  end;

procedure RaiseHLSL(msg: string);
begin
    raise EHLSL.Create(msg);
end;

procedure Check3DError(hr: HRESULT);
var s: string;
begin
    if hr = 0 then Exit;
    case hr of
//      D3D10_ERROR_FILE_NOT_FOUND                : s := 'D3D10_ERROR_FILE_NOT_FOUND';
//      D3D10_ERROR_TOO_MANY_UNIQUE_STATE_OBJECTS : s := 'D3D10_ERROR_TOO_MANY_UNIQUE_STATE_OBJECTS';
      //D3DERR_INVALIDCALL                        : s := 'D3DERR_INVALIDCALL';
      //D3DERR_WASSTILLDRAWING                    : s := 'D3DERR_WASSTILLDRAWING';

      DXGI_ERROR_INVALID_CALL                 : s := 'DXGI_ERROR_INVALID_CALL';
      DXGI_ERROR_NOT_FOUND                    : s := 'DXGI_ERROR_NOT_FOUND';
      DXGI_ERROR_MORE_DATA                    : s := 'DXGI_ERROR_MORE_DATA';
      DXGI_ERROR_UNSUPPORTED                  : s := 'DXGI_ERROR_UNSUPPORTED';
      DXGI_ERROR_DEVICE_REMOVED               : s := 'DXGI_ERROR_DEVICE_REMOVED';
      DXGI_ERROR_DEVICE_HUNG                  : s := 'DXGI_ERROR_DEVICE_HUNG';
      DXGI_ERROR_DEVICE_RESET                 : s := 'DXGI_ERROR_DEVICE_RESET';
      DXGI_ERROR_WAS_STILL_DRAWING            : s := 'DXGI_ERROR_WAS_STILL_DRAWING';
      DXGI_ERROR_FRAME_STATISTICS_DISJOINT    : s := 'DXGI_ERROR_FRAME_STATISTICS_DISJOINT';
      DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE : s := 'DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE';
      DXGI_ERROR_DRIVER_INTERNAL_ERROR        : s := 'DXGI_ERROR_DRIVER_INTERNAL_ERROR';
      DXGI_ERROR_NONEXCLUSIVE                 : s := 'DXGI_ERROR_NONEXCLUSIVE';
      DXGI_ERROR_NOT_CURRENTLY_AVAILABLE      : s := 'DXGI_ERROR_NOT_CURRENTLY_AVAILABLE';
      DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED   : s := 'DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED';
      DXGI_ERROR_REMOTE_OUTOFMEMORY           : s := 'DXGI_ERROR_REMOTE_OUTOFMEMORY';
      //DXGI_ERROR_ACCESS_LOST                  : s := 'DXGI_ERROR_ACCESS_LOST';
      //DXGI_ERROR_WAIT_TIMEOUT                 : s := 'DXGI_ERROR_WAIT_TIMEOUT';
      //DXGI_ERROR_SESSION_DISCONNECTED         : s := 'DXGI_ERROR_SESSION_DISCONNECTED';
      //DXGI_ERROR_RESTRICT_TO_OUTPUT_STALE     : s := 'DXGI_ERROR_RESTRICT_TO_OUTPUT_STALE';
      //DXGI_ERROR_CANNOT_PROTECT_CONTENT       : s := 'DXGI_ERROR_CANNOT_PROTECT_CONTENT';
      //DXGI_ERROR_ACCESS_DENIED                : s := 'DXGI_ERROR_ACCESS_DENIED';
      //DXGI_ERROR_NAME_ALREADY_EXISTS          : s := 'DXGI_ERROR_NAME_ALREADY_EXISTS';

      E_FAIL        : s := 'E_FAIL';
      E_INVALIDARG  : s := 'E_INVALIDARG';
      E_OUTOFMEMORY : s := 'E_OUTOFMEMORY';
      E_NOTIMPL     : s := 'E_NOTIMPL';
      S_FALSE       : s := 'S_FALSE';
    else
      s := IntToHex(hr, 8);
    end;
    RaiseHLSL(s);
end;

function Succeeded(Status: HRESULT): Boolean;
begin
  Result := Status and HRESULT($80000000) = 0;
end;

procedure CheckHResult(hRes: HRESULT; msg: string = '');
begin
  if Succeeded(hRes) then Exit;
  if msg <> '' then msg := msg + ' ';
  msg := msg + IntToHex(hRes, 8);
  RaiseHLSL(msg);
end;

function OpenFileToString(const FileName: String; const prog: TProgramInfo = Nil): AnsiString;
var
  fn: String;
  var sl: TStringList;
begin
  Result := '';
  fn := prog.FullFileName(FileName);
  if fn = '' then RaiseHLSL('File "'+FileName+'" not found');

  sl := TStringList.Create;
  try
    sl.LoadFromFile(fn);
    Result := AnsiString(sl.Text);
  finally
    sl.Free;
  end;
end;

procedure CompileHLSL(const prog: TProgramInfo; st: TShaderType; const OutFile: string);
var astr: AnsiString;
    incl: TIncludeAdapter;
    entry: AnsiString;
    target: AnsiString;
    code: ID3DBlob;
    output: ID3DBlob;
    fs: TFileStream;
begin
  Write('Compiling(HLSL): "', prog.Shader[st], '" ... ');
  astr := OpenFileToString(prog.Shader[st], prog);
  incl := TIncludeAdapter.Create(prog);
  try
    entry := AnsiString(prog.Entry[st]);
    target := AnsiString(prog.Target[st]);
    CheckHResult( D3DCompile(@astr[1], Length(astr), nil, nil, incl, PAnsiChar(entry), PAnsiChar(target), D3DCOMPILE_ENABLE_BACKWARDS_COMPATIBILITY or D3DCOMPILE_OPTIMIZATION_LEVEL3, 0, code, output) );
    if code = nil then
      RaiseHLSL('unknown error');
    fs := TFileStream.Create(OutFile, fmCreate);
    try
      fs.WriteBuffer(code.GetBufferPointer^, code.GetBufferSize);
    finally
      fs.Free;
    end;
  finally
    // ToDo: Reparse errors
    if Assigned(output) then
      WriteLn(PAnsiChar(output.GetBufferPointer));
    incl.Free;
  end;
  WriteLn('done.');
end;

type
  TUniform = record
    Name: string;
    DataClass: TDataClass;
    ItemsCount: Integer;
    ElementType: TComponentType;
    ElementsCount: Integer;
    Offset: Integer;
    SamplerIndex: Integer;
    Data: array of Byte;

    procedure Write(const stream: TStream);
  end;

procedure LinkHLSL(const prog: TProgramInfo; shaders: TShadersString; const OutFile: string);
const
  EmptyUniform: TUniform = (
    Name          : '';
    DataClass     : dcVector;
    ItemsCount    : 0;
    ElementType   : ctFloat;
    ElementsCount : 0;
    Offset        : -1;
    SamplerIndex  : 0;
    Data          : Nil;
  );

type
  {$IfDef fpc}
  TUniformsHash = specialize THashMap<string, TUniform, TMurmur2HashString>;
  IUniformsHash = specialize IHashMap<string, TUniform, TMurmur2HashString>;
  {$Else}
  TUniformsHash = TDictionary<string, TUniform>;
  IUniformsHash = TDictionary<string, TUniform>;
  {$EndIf}
  TUniforms = array of TUniform;

var
  UniformHash : IUniformsHash;

  function GetShaderReflection(const filename: String): ID3D11ShaderReflection;
  var fs: TFileStream;
      data: TByteArr;
  begin
    fs := TFileStream.Create(filename, fmOpenRead);
    try
      SetLength(data, fs.Size);
      fs.Read(data[0], Length(data));
      Check3DError( D3DReflect(@data[0], Length(data), ID3D11ShaderReflection, Result) );
    finally
      fs.Free;
    end;
  end;

  function GetDataClass(const UniformTypeDesc: TD3D11_ShaderTypeDesc): TDataClass;
  begin
      case UniformTypeDesc._Class of
          D3D10_SVC_SCALAR: Result := dcScalar;
          D3D10_SVC_VECTOR: Result := dcVector;

          D3D10_SVC_MATRIX_ROWS,
          D3D10_SVC_MATRIX_COLUMNS: Result := dcMatrix;

          D3D10_SVC_OBJECT,
          D3D10_SVC_STRUCT,
          D3D11_SVC_INTERFACE_CLASS,
          D3D11_SVC_INTERFACE_POINTER: Result := dcSampler;
      else
          Result := dcScalar;
          Assert(False, 'Unsupported type');
      end;
  end;
  function GetComponentType(const UniformTypeDesc: TD3D11_ShaderTypeDesc): TComponentType;
  begin
      case UniformTypeDesc._Type of
          D3D10_SVT_BOOL  : Result := ctBool;
          D3D10_SVT_INT   : Result := ctInt;
          D3D10_SVT_FLOAT : Result := ctFloat;
          D3D10_SVT_UINT  : Result := ctUInt;
          D3D11_SVT_DOUBLE: Result := ctDouble;
      else
          Result := ctInt;
      end;
  end;

  procedure RaiseDiffersUniforms(const name: string);
  begin
    RaiseHLSL('Uniform "'+name+'" duplicated but differs');
  end;

  function CreateUniformByDescs(const ShaderVarDesc: TD3D11_ShaderVariableDesc; const ShaderTypeDesc: TD3D11_ShaderTypeDesc): TUniform;
  begin
    if UniformHash.TryGetValue(ShaderVarDesc.Name, Result) then
    begin
      if Result.DataClass     <> GetDataClass(ShaderTypeDesc)                 then RaiseDiffersUniforms(ShaderVarDesc.Name);
      if Result.ElementType   <> GetComponentType(ShaderTypeDesc)             then RaiseDiffersUniforms(ShaderVarDesc.Name);
      if Result.ItemsCount    <> ShaderTypeDesc.Elements                      then RaiseDiffersUniforms(ShaderVarDesc.Name);
      if Result.ElementsCount <> ShaderTypeDesc.Rows * ShaderTypeDesc.Columns then RaiseDiffersUniforms(ShaderVarDesc.Name);
    end
    else
    begin
      Result.Name := ShaderVarDesc.Name;
      Result.DataClass     := GetDataClass(ShaderTypeDesc);
      Result.ElementType   := GetComponentType(ShaderTypeDesc);
      Result.ItemsCount    := ShaderTypeDesc.Elements;
      Result.ElementsCount := ShaderTypeDesc.Rows * ShaderTypeDesc.Columns;
      Result.Offset        := ShaderVarDesc.StartOffset;
      if ShaderVarDesc.Size > 0 then
      begin
        SetLength(Result.Data, ShaderVarDesc.Size);
        if ShaderVarDesc.DefaultValue = nil then
          FillChar(Result.Data[0], SizeOf(Result.Data[0])*Length(Result.Data), 0)
        else
          Move(ShaderVarDesc.DefaultValue^, Result.Data[0], ShaderVarDesc.Size);
      end
      else
        Result.Data := nil;
      UniformHash.Add(ShaderVarDesc.Name, Result);
    end;
  end;
  function CreateUniformByDesc(const ShaderBindDesc: TD3D11_ShaderInputBindDesc): TUniform;
  begin
    if UniformHash.TryGetValue(ShaderBindDesc.Name, Result) then
    begin
      if Result.DataClass     <> dcSampler  then RaiseDiffersUniforms(ShaderBindDesc.Name);
      if Result.ElementType   <> ctInt      then RaiseDiffersUniforms(ShaderBindDesc.Name);
      if Result.ItemsCount    <> 1          then RaiseDiffersUniforms(ShaderBindDesc.Name);
      if Result.ElementsCount <> 1          then RaiseDiffersUniforms(ShaderBindDesc.Name);
    end
    else
    begin
      Result.Name := ShaderBindDesc.Name;
      Result.DataClass := dcSampler;
      Result.ElementType := ctInt;
      Result.ItemsCount := 1;
      Result.ElementsCount := 1;
      Result.Offset := ShaderBindDesc.BindPoint;
      Result.Data := nil;
      UniformHash.Add(ShaderBindDesc.Name, Result);
    end;
  end;

  function ReadUniforms(const ref: ID3D11ShaderReflection; out UBSize: Integer): TUniforms;
  var
      ShaderDesc: TD3D11_ShaderDesc;
      ShaderBufferDesc: TD3D11_ShaderBufferDesc;
      ShaderVarDesc: TD3D11_ShaderVariableDesc;
      ShaderTypeDesc: TD3D11_ShaderTypeDesc;
      ShaderBindDesc: TD3D11_ShaderInputBindDesc;
      CBufferReflect: ID3D11ShaderReflectionConstantBuffer;
      s: String;
      I, J, N: Integer;
  begin
    Result := Nil;
    UBSize := 0;
    Check3DError( ref.GetDesc(ShaderDesc) );
    if ShaderDesc.ConstantBuffers > 0 then
    begin
      Pointer(CBufferReflect) := ref.GetConstantBufferByIndex(0);
      Check3DError( CBufferReflect.GetDesc(@ShaderBufferDesc) );
      UBSize := ShaderBufferDesc.Size;
      for I := 0 to ShaderBufferDesc.Variables - 1 do
      begin
          Check3DError( ID3D11ShaderReflectionVariable(CBufferReflect.GetVariableByIndex(I)).GetDesc(ShaderVarDesc) );
          Check3DError( ID3D11ShaderReflectionType(ID3D11ShaderReflectionVariable(CBufferReflect.GetVariableByIndex(I)).GetType).GetDesc(ShaderTypeDesc) );
          if ShaderVarDesc.Flags And Cardinal(D3D10_SVF_USED) = 0 then Continue;
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result)-1] := CreateUniformByDescs(ShaderVarDesc, ShaderTypeDesc);
      end;
    end;

    for I := 0 to ShaderDesc.BoundResources - 1 do
    begin
        Check3DError(ref.GetResourceBindingDesc(I, ShaderBindDesc));
        if ShaderBindDesc._Type <> D3D10_SIT_TEXTURE then Continue;
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result)-1] := CreateUniformByDesc(ShaderBindDesc);
    end;

    //assign samplers to textures
    for I := 0 to ShaderDesc.BoundResources - 1 do
    begin
        Check3DError(ref.GetResourceBindingDesc(I, ShaderBindDesc));
        if ShaderBindDesc._Type <> D3D10_SIT_SAMPLER then Continue;
        s := string(ShaderBindDesc.Name);
        N := Pos('Sampler', s);
        if N = 0 then Continue;
        Delete(s, N, Length('Sampler'));
        for j := 0 to Length(Result) - 1 do
          if Result[j].Name = s then
            Result[j].SamplerIndex := ShaderBindDesc.BindPoint;
    end;
  end;

  procedure WriteUnifroms(stream: TStream; const UBName: AnsiString; UBSize: Integer; const Un: TUniforms);
  var i, n: Integer;
  begin
    StreamWriteString(stream, UBName);
    stream.WriteBuffer(UBSize, SizeOf(UBSize));
    n := Length(Un);
    stream.WriteBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
      un[i].Write(stream);
  end;

var st: TShaderType;
    uniforms: array [TShaderType] of TUniforms;
    UBSize: array [TShaderType] of Integer;

    ShaderChunk, UBChunk, CodeChunk: TChunkWriter;
    fs, fsCode: TFileStream;
    n: Integer;
begin
  Write('Linking(HLSL): "', prog.Name, '" ... ');
  {$IfDef fpc}
  UniformHash := TUniformsHash.Create('', EmptyUniform);
  {$Else}
  UniformHash := TUniformsHash.Create;
  {$EndIf}

  for st := Low(TShaderType) to High(TShaderType) do
  begin
    if st = stUnknown then Continue;
    if shaders[st] = '' then Continue;
    uniforms[st] := ReadUniforms( GetShaderReflection(shaders[st]), UBSize[st] );
  end;

  fs := TFileStream.Create(OutFile, fmCreate);
  try
    for st := Low(TShaderType) to High(TShaderType) do
    begin
        if st = stUnknown then Continue;
        if shaders[st] = '' then Continue;

        ShaderChunk := TChunkWriter.Create(fs, ShaderType_FourCC[st]);
        try
          CodeChunk := TChunkWriter.Create(fs, MakeFourCC('C','O','D','E'));
          try
            fsCode := TFileStream.Create(shaders[st], fmOpenRead);
            try
              n := fsCode.Size;
              fs.WriteBuffer(n, SizeOf(n));
              fs.CopyFrom(fsCode, fsCode.Size);
            finally
              FreeAndNil(fsCode);
            end;
          finally
            FreeAndNil(CodeChunk);
          end;

          UBChunk := TChunkWriter.Create(fs, MakeFourCC('U','B','L','K'));
          try
            WriteUnifroms(fs, '$Globals', UBSize[st], uniforms[st]);
          finally
            FreeAndNil(UBChunk);
          end;
        finally
          FreeAndNil(ShaderChunk);
        end;
    end;
  finally
    FreeAndNil(fs);
  end;

  {$IfNDef fpc}
  FreeAndNil(UniformHash);
  {$EndIf}
  WriteLn('done.');
end;

{ TChunkWriter }

constructor TChunkWriter.Create(fs: TStream; id: TFOURCC);
var dummy: Integer;
begin
  FStream := fs;
  FStream.WriteBuffer(id, SizeOf(id));
  dummy := 0;
  FPos := FStream.Position;
  FStream.WriteBuffer(dummy, SizeOf(dummy));
end;

destructor TChunkWriter.Destroy;
var oldPos: Int64;
    ChunkSize: Integer;
    b: Byte;
begin
  if FStream.Position mod 2 = 1 then
  begin
    b := 0;
    FStream.WriteBuffer(b, SizeOf(b));
  end;
  ChunkSize := FStream.Position - FPos - SizeOf(ChunkSize);
  oldPos := FStream.Position;
  FStream.Position := FPos;
  FStream.WriteBuffer(ChunkSize, SizeOf(ChunkSize));
  FStream.Position := oldPos;
  inherited Destroy;
end;

{ TIncludeAdapter }

function TIncludeAdapter.Open(IncludeType: TD3D_IncludeType;
  pFileName: PAnsiChar; pParentData: Pointer; ppData: PPointer;
  pBytes: PLongWord): HResult; stdcall;
var astr: AnsiString;
begin
  if pFileName = 'hlsl.h' then
  begin
    ppData^ := PAnsiChar(NullData);
    pBytes^ := 0;
    Exit(S_OK);
  end;
  if not FIncludes.TryGetValue(string(pFileName), astr) then
  begin
    astr := OpenFileToString(string(pFileName), FProg);
    FIncludes.Add(string(pFileName), astr);
  end;
  ppData^ := PAnsiChar(astr);
  pBytes^ := Length(astr);
  Exit(S_OK);
end;

function TIncludeAdapter.Close(pData: Pointer): HResult; stdcall;
begin
  Result := S_OK;
end;

constructor TIncludeAdapter.Create(const prog: TProgramInfo);
begin
  FProg := prog;
  {$IfDef fpc}
  FIncludes := TIncludes.Create('','');
  {$Else}
  FIncludes := TIncludes.Create;
  {$EndIf}
end;

destructor TIncludeAdapter.Destroy;
begin
  {$IfNDef fpc} FreeAndNil(FIncludes); {$EndIf}
  inherited Destroy;
end;

{ TUniform }

procedure TUniform.Write(const stream: TStream);
  procedure StreamWriteString(const stream: TStream; const str: AnsiString);
  var n: Integer;
  begin
    n := Length(str);
    stream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      stream.WriteBuffer(str[1], Length(str));
  end;
var b: Byte;
    n: Integer;
begin

  StreamWriteString(stream, AnsiString(Name));
  b := Ord(DataClass);
  stream.WriteBuffer(b, 1);
  b := Ord(ElementType);
  stream.WriteBuffer(b, 1);
  stream.WriteBuffer(ItemsCount, SizeOf(ItemsCount));
  stream.WriteBuffer(ElementsCount, SizeOf(ElementsCount));
  stream.WriteBuffer(Offset, SizeOf(Offset));
  stream.WriteBuffer(SamplerIndex, SizeOf(SamplerIndex));

  n := Length(Data);
  stream.WriteBuffer(n, SizeOf(n));
  if n > 0 then
    stream.WriteBuffer(Data[0], n);
end;

end.

