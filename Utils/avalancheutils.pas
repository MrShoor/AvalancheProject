{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit AvalancheUtils;

{$warn 5023 off : no warning about unused units}
interface

uses
  avContnrs, mutils, intfUtils, avContnrsDefaults, avPathFinder, avPolygon, 
  avFileUtils, math_fx, superobject, avRTTIUtils, avGLU, avGLUIntf, 
  LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('AvalancheUtils', @Register);
end.
