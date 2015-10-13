{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Newton;

interface

uses
  NewtonIntf, NewtonImport, NewtonImport_JointLibrary, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('Newton', @Register);
end.
