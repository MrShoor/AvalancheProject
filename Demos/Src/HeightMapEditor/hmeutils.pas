unit HMEUtils;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avRes;

type

  { TavNormalMapBuilder }

  TavNormalMapBuilder = class (TavMainRenderChild)
  private
    FProg: TavProgram;
    FQuadVB: TavVB;
  public
    procedure Build(const AHeightMap: TavTexture; const ANormalMap: TavTexture; HeightScale: Single; GenerateMips: Boolean);
    procedure AfterConstruction; override;
  end;

implementation

uses
  avUtils, mutils, avTypes;

{ TavNormalMapBuilder }

procedure TavNormalMapBuilder.AfterConstruction;
begin
  inherited;
  FProg := TavProgram.Create(Self);
  FProg.Load('Build_NormalMap', True);
  FQuadVB := GenQuad_VB(Self, Vec(0, 0, 1, 1));
end;

procedure TavNormalMapBuilder.Build(const AHeightMap, ANormalMap: TavTexture; HeightScale: Single; GenerateMips: Boolean);
var fbo: TavFrameBuffer;
    wasBinded: Boolean;
begin
  wasBinded := Main.Binded;
  try
    if not wasBinded then
      if not Main.Bind then Exit;

    fbo := nil;
    try
      AHeightMap.Build;

      fbo := TavFrameBuffer.Create(Self);
      fbo.SetColor(0, ANormalMap);
      fbo.FrameRect := RectI(0, 0, Round(AHeightMap.ImageSize.x), Round(AHeightMap.ImageSize.y));
      fbo.Select();

      FProg.Select();
      FProg.SetAttributes(FQuadVB, nil, nil);
      FProg.SetUniform('HeightMap', AHeightMap, Sampler_NoFilterClamped);
      FProg.SetUniform('HeightScale', HeightScale);
      FProg.Draw();

      if GenerateMips then
        ANormalMap.GenerateMips;
    finally
      FreeAndNil(fbo);
    end;

    Main.Present;
  finally
    if not wasBinded then
      if Main.Binded then
        Main.Unbind;
  end;
end;

end.

