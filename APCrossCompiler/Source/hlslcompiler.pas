unit HLSLCompiler;

{$IfDef fpc}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, CompileTask, D3DCompiler_JSB, D3DCommon_JSB,
  {$IfDef fpc}
    avTypes, avContnrs;
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

procedure RaiseHLSL(msg: string);
begin
    raise EHLSL.Create(msg);
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
    if Assigned(output) then
      WriteLn(PAnsiChar(output.GetBufferPointer));
    incl.Free;
  end;
end;

procedure LinkHLSL(const prog: TProgramInfo; shaders: TShadersString; const OutFile: string);
begin
  // ToDo : implement it
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

end.

