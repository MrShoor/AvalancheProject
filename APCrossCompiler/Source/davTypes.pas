unit davTypes;

interface

uses
  Classes;

type
  T3DAPI = (apiOGL, apiDX11);
  TByteArr = array of Byte;
const
  API_Prefix : array [T3DAPI] of string = ('OGL_', 'DX_');
  API_Suffix : array [T3DAPI] of string = ('.glsl', '.hlsl');

type
  TShaderType = (stUnknown, stVertex, stTessControl, stTessEval, stGeometry, stFragment, stCompute);

  TComponentType = (ctBool, ctByte, ctUByte, ctShort, ctUShort, ctInt, ctUInt, ctFloat, ctDouble);
  TDataClass = (dcScalar, dcVector, dcMatrix, dcSampler, dcCubeSampler);

const
  ShaderType_Name : array [TShaderType] of string = ('Unknown', 'Vertex', 'TessControl', 'TessEval', 'Geometry', 'Fragment', 'Compute');
  ShaderType_FourCC : array [TShaderType] of Cardinal = (0, $54524556, $4E4F4354, $4C564554, $4D4F4547, $47415246, $504D4F43);

type
  TFOURCC = Cardinal;

function MakeFourCC(ch0,ch1,ch2,ch3: AnsiChar): TFOURCC;
procedure StreamWriteString(stream: TStream; const str: AnsiString);
procedure StreamReadString(stream: TStream; out str: AnsiString);

implementation

function MakeFourCC(ch0,ch1,ch2,ch3: AnsiChar): TFOURCC;
begin
  Result := Byte(ch3);
  Result := Result shl 8 or Byte(ch2);
  Result := Result shl 8 or Byte(ch1);
  Result := Result shl 8 or Byte(ch0);
end;

procedure StreamWriteString(stream: TStream; const str: AnsiString);
var n: Integer;
begin
  n := Length(str);
  stream.WriteBuffer(n, SizeOf(n));
  if n > 0 then
    stream.WriteBuffer(str[1], n);
end;

procedure StreamReadString(stream: TStream; out str: AnsiString);
var n: Integer;
begin
  n := 0;
  stream.ReadBuffer(n, SizeOf(n));
  SetLength(str, n);
  if n > 0 then
      stream.ReadBuffer(str[1], n);
end;

end.
