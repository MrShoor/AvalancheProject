{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Avalanche;

{$warn 5023 off : no warning about unused units}
interface

uses
  avBase, avCameraController, avCanvas, avContext, avContext_OGL, avControls, 
  avLog, avPlatform, avRes, avTess, avTimer, avTypes, dglOpenGL, avTexLoader, 
  D3D11_JSB, DXGI_JSB, DXTypes_JSB, D3DCommon_JSB, avContext_DX11, avMesh, 
  avModel, avGlyphGenerator, avMiniControls, avUtils, avGPUGlyphGenerator, 
  avTextUtils, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('Avalanche', @Register);
end.
