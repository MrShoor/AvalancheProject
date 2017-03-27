unit HMEBrush;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  mutils,
  avRes, avTypes;

type

  { TavHMEBrush }

  TavHMEBrush = class (TavMainRenderChild)
  private
    FPos: TVec2;
    FRadius: Single;
    FForce: Single;
  private
    FFBO    : TavFrameBuffer;
    FTempTex: TavTexture;
    FTempFBO: TavFrameBuffer;

    FProg   : TavProgram;
    FProgSmooth : TavProgram;

    FQuadVB : TavVB;
    FSharp  : Single;

    FHeigthTex: TavTexture;
  public
    procedure SetHeightMap(const AHeightMapTex: TavTexture);

    procedure ApplyBrush(ARaiseUp: Boolean);
    procedure ApplyBrush_Smooth();

    property Pos: TVec2 read FPos write FPos;
    property Radius: Single read FRadius write FRadius;
    property Force: Single read FForce write FForce;
    property Sharp: Single read FSharp write FSharp;

    procedure AfterConstruction; override;
  end;

implementation

uses
  avUtils;

{ TavHMEBrush }

procedure TavHMEBrush.SetHeightMap(const AHeightMapTex: TavTexture);
begin
  FFBO.SetColor(0, AHeightMapTex);
  FFBO.FrameRect := RectI(0, 0, AHeightMapTex.TexData.Width,AHeightMapTex.TexData.Height);
  FTempFBO.FrameRect := RectI(0, 0, AHeightMapTex.TexData.Width,AHeightMapTex.TexData.Height);
  FHeigthTex := AHeightMapTex;
end;

procedure TavHMEBrush.ApplyBrush(ARaiseUp: Boolean);
var oldWire: Boolean;
begin
  FFBO.Select();

  Main.States.Blending[0] := True;
  Main.States.SetBlendFunctions(TBlendFunc.bfSrcAlpha, TBlendFunc.bfOne);

  if ARaiseUp then
    Main.States.SetBlendOperation(boAdd)
  else
    Main.States.SetBlendOperation(boRevSub);

  oldWire := Main.States.Wireframe;
  Main.States.Wireframe := False;

  FProg.Select();
  FProg.SetAttributes(FQuadVB, nil, nil);
  FProg.SetUniform('BrushPos', FPos);
  FProg.SetUniform('BrushRadius', FRadius);
  FProg.SetUniform('BrushForce', FForce/100);
  FProg.SetUniform('BrushSharp', FSharp);
  FProg.SetUniform('ViewPortSize', FFBO.FrameRect.Size*1.0);
  FProg.Draw();

  Main.States.Wireframe := oldWire;

  Main.States.Blending[0] := False;
  Main.States.SetBlendOperation(boAdd);
end;

procedure TavHMEBrush.ApplyBrush_Smooth;
var oldWire: Boolean;
begin
  FTempFBO.Select();
  FTempTex.CopyFrom(FHeigthTex, 0, FTempFBO.FrameRect);

  Main.States.Blending[0] := False;

  oldWire := Main.States.Wireframe;
  Main.States.Wireframe := False;

  FProgSmooth.Select();
  FProgSmooth.SetAttributes(FQuadVB, nil, nil);
  FProgSmooth.SetUniform('BrushPos', FPos);
  FProgSmooth.SetUniform('BrushRadius', FRadius);
  FProgSmooth.SetUniform('BrushForce', FForce/100);
  FProgSmooth.SetUniform('BrushSharp', FSharp);
  FProgSmooth.SetUniform('ViewPortSize', FFBO.FrameRect.Size*1.0);
  FProgSmooth.SetUniform('HeightMap', FHeigthTex, Sampler_LinearClamped);
  FProgSmooth.Draw();

  Main.States.Wireframe := oldWire;

  FHeigthTex.CopyFrom(FTempTex, 0, FTempFBO.FrameRect);
end;

procedure TavHMEBrush.AfterConstruction;
begin
  inherited AfterConstruction;
  FRadius := 40;
  FForce := 100;
  FSharp := 1.3;

  FFBO := TavFrameBuffer.Create(Self);

  FTempTex := TavTexture.Create(Self);
  FTempTex.TargetFormat := TTextureFormat.R16;
  FTempFBO := TavFrameBuffer.Create(Self);
  FTempFBO.SetColor(0, FTempTex);

  FProg := TavProgram.Create(Self);
  FProg.Load('Brush', True);

  FProgSmooth := TavProgram.Create(Self);
  FProgSmooth.Load('BrushSmooth', True);

  FQuadVB := GenQuad_VB(Self, Vec(-1,-1, 1, 1));
end;

end.
