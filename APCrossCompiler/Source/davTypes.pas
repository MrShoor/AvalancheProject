unit davTypes;

interface

type
  T3DAPI = (apiOGL, apiDX11);
const
  API_Prefix : array [T3DAPI] of string = ('OGL_', 'DX_');
  API_Suffix : array [T3DAPI] of string = ('.glsl', '.hlsl');

type
  TShaderType = (stUnknown, stVertex, stGeometry, stFragment);

const
  ShaderType_Name : array [TShaderType] of string = ('Unknown', 'Vertex', 'Geometry', 'Fragment');

implementation

end.
