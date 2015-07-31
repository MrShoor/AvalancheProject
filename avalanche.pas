{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Avalanche;

interface

uses
  avBase, avCameraController, avCanvas, avContext, avContext_OGL, avContnrs, 
  avControls, avLog, avPlatform, avRes, avTess, avTimer, avTypes, dglOpenGL, 
  mutils, avTexLoader, D3D11_JSB, DXGI_JSB, DXTypes_JSB, D3DCommon_JSB, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('Avalanche', @Register);
end.
