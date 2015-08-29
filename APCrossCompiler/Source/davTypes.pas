unit davTypes;

interface

type
  T3DAPI = (apiOGL, apiDX11);
  TByteArr = array of Byte;
const
  API_Prefix : array [T3DAPI] of string = ('OGL_', 'DX_');
  API_Suffix : array [T3DAPI] of string = ('.glsl', '.hlsl');

type
  TShaderType = (stUnknown, stVertex, stGeometry, stFragment);

  TComponentType = (ctBool, ctByte, ctUByte, ctShort, ctUShort, ctInt, ctUInt, ctFloat, ctDouble);
  TDataClass = (dcScalar, dcVector, dcMatrix, dcSampler);

const
  ShaderType_Name : array [TShaderType] of string = ('Unknown', 'Vertex', 'Geometry', 'Fragment');

type
  TFOURCC = Cardinal;

function MakeFourCC(ch0,ch1,ch2,ch3: AnsiChar): TFOURCC;

implementation

function MakeFourCC(ch0,ch1,ch2,ch3: AnsiChar): TFOURCC;
begin
  Result := Byte(ch0);
  Result := Result shl 8 or Byte(ch1);
  Result := Result shl 8 or Byte(ch2);
  Result := Result shl 8 or Byte(ch3);
end;

end.
