{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit avalancheGLUTess;

{$warn 5023 off : no warning about unused units}
interface

uses
  avGLU, avGLUIntf, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('avalancheGLUTess', @Register);
end.
