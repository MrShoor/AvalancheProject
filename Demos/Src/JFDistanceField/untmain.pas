unit untMain;
{$I avConfig.inc}

interface

uses
  {$IfDef FPC}
  LCLType,
  FileUtil,
  {$EndIf}
  {$IfnDef FPC}
  Windows,
  Messages,
  Vcl.AppEvnts,
  {$EndIf}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  avRes, avTypes, avTexLoader, avTess, avContnrs, mutils;

type

  { TQuadVertex }

  TQuadVertex = packed record
    vsCoord: TVec2;
    class function Layout: IDataLayout; static;
  end;
  IQuadVeritces = {$IfDef FPC}specialize{$EndIf} IArray<TQuadVertex>;
  TQuadVeritces = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TQuadVertex>;

  { TfrmMain }

  TfrmMain = class(TForm)
    Panel1: TPanel;
    btnGAPI: TSpeedButton;
    {$IfDef DCC}
    ApplicationEvents1: TApplicationEvents;
    {$EndIf}
    {$IfDef FPC}
    ApplicationProperties1: TApplicationProperties;
    {$EndIf}
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure btnGAPIClick(Sender: TObject);
  private
    FMain: TavMainRender;

    FPrepareProg: TavProgram;
    FJumpProg   : TavProgram;
    FResolveProg: TavProgram;

    FPrepareFBO: TavFrameBuffer;
    FJumpFBO   : array [0..1] of TavFrameBuffer;
    FResolveFBO: TavFrameBuffer;

    FSrcTexture: TavTexture;
    FDFTexure: array [0..1] of TavTexture;

    FDFIndex: Integer; //index of texture with current DF

    FQuad: TavVB;
  private
    FLastFPSTime: Int64;
    FFPSCounter : Int64;
  public
    procedure Init;

    procedure PrepareDistanceField;
    procedure BuildDistanceField;
    procedure ResolveDistanceField;

    procedure Sync3DAPI;
    procedure RenderScene;
  end;

var
  frmMain: TfrmMain;

implementation

uses Math;

{$IfnDef notDCC}
  {$R *.dfm}
{$EndIf}

{$IfDef FPC}
  {$R *.lfm}
  {$R 'shaders\shaders.rc'}
{$EndIf}


{ TQuadVertex }

class function TQuadVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 2).Finish(SizeOf(TQuadVertex));
end;

function GenQuadData: IQuadVeritces;
var v: TQuadVertex;
begin
  Result := TQuadVeritces.Create;
  v.vsCoord := Vec(-1, -1);
  Result.Add(v);
  v.vsCoord := Vec(-1,  1);
  Result.Add(v);
  v.vsCoord := Vec( 1, -1);
  Result.Add(v);
  v.vsCoord := Vec( 1,  1);
  Result.Add(v);
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  Init;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TfrmMain.Init;
begin
  FMain := TavMainRender.Create(nil);
  FMain.Window := Handle;

  FPrepareProg := TavProgram.Create(FMain);
  FPrepareProg.Load('prepare', True);
  FJumpProg := TavProgram.Create(FMain);
  FJumpProg.Load('JFPass', True);
  FResolveProg := TavProgram.Create(FMain);
  FResolveProg.Load('resolve', True);

  FSrcTexture := TavTexture.Create(FMain);
  FSrcTexture.AutoGenerateMips := False;
  if FileExists('TextureForDF.png') then
    FSrcTexture.TexData := LoadTexture('TextureForDF.png')
  else
    FSrcTexture.TexData := LoadTexture('..\Media\TextureForDF.png');
  ClientWidth := FSrcTexture.TexData.Width;
  ClientHeight := FSrcTexture.TexData.Height;

  FDFTexure[0] := TavTexture.Create(FMain);
  FDFTexure[0].TargetFormat := TTextureFormat.RG32f;
  FDFTexure[1] := TavTexture.Create(FMain);
  FDFTexure[1].TargetFormat := TTextureFormat.RG32f;

  FPrepareFBO := TavFrameBuffer.Create(FMain);
  FPrepareFBO.FrameRect := RectI(0, 0, FSrcTexture.TexData.Width, FSrcTexture.TexData.Height);
  FPrepareFBO.SetColor(0, FDFTexure[1]);

  FJumpFBO[0] := TavFrameBuffer.Create(FMain);
  FJumpFBO[0].SetColor(0, FDFTexure[0]);
  FJumpFBO[0].FrameRect := FPrepareFBO.FrameRect;
  FJumpFBO[1] := TavFrameBuffer.Create(FMain);
  FJumpFBO[1].SetColor(0, FDFTexure[1]);
  FJumpFBO[1].FrameRect := FPrepareFBO.FrameRect;

  FResolveFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA]);

  FQuad := TavVB.Create(FMain);
  FQuad.Vertices := GenQuadData as IVerticesData;
  FQuad.CullMode := cmNone;
  FQuad.PrimType := ptTriangleStrip;
end;

procedure TfrmMain.PrepareDistanceField;
begin
  FPrepareFBO.Select();
  FPrepareProg.Select();
  FPrepareProg.SetAttributes(FQuad, nil, nil);
  FPrepareProg.SetUniform('SrcTex', FSrcTexture, Sampler_NoFilter);
  FPrepareProg.Draw();
  FDFIndex := 1;
end;

procedure TfrmMain.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  Done := False;
  if FMain <> nil then FMain.InvalidateWindow;
end;

procedure TfrmMain.btnGAPIClick(Sender: TObject);
begin
  if btnGAPI.Down then
    btnGAPI.Caption := 'DX11'
  else
    btnGAPI.Caption := 'OGL';
end;

procedure TfrmMain.BuildDistanceField;
var fbSize: TVec2i;
    JumpDist: Integer;
    JumpStep: TVec2;
    DFNextIndex: Integer;
begin
  fbSize := FJumpFBO[FDFIndex].FrameRect.Size;

  JumpDist := max(fbSize.x, fbSize.y) div 2;
  while JumpDist > 0 do
  begin
    JumpStep.x := JumpDist / fbSize.x;
    JumpStep.y := JumpDist / fbSize.y;
    DFNextIndex := FDFIndex xor 1;

    FJumpFBO[DFNextIndex].Select();

    FJumpProg.Select();
    FJumpProg.SetAttributes(FQuad, nil, nil);
    FJumpProg.SetUniform('Aspect', fbSize.x/fbSize.y);
    FJumpProg.SetUniform('JumpStep', JumpStep);
    FJumpProg.SetUniform('SrcDistanceField', FDFTexure[FDFIndex], Sampler_NoFilter);
    FJumpProg.Draw();

    JumpDist := JumpDist div 2;
    FDFIndex := DFNextIndex;
  end;
end;

procedure TfrmMain.ResolveDistanceField;
begin
  FResolveFBO.FrameRect := RectI(0, 0, FMain.WindowSize.x, FMain.WindowSize.y);
  FResolveFBO.Select();
  FResolveFBO.Clear(0, Vec(0,0,0,0));

  FResolveProg.Select();
  FResolveProg.SetAttributes(FQuad, nil, nil);
  FPrepareProg.SetUniform('FBOFlip', Vec(1.0,1.0));
  FResolveProg.SetUniform('Time', FMain.Time64/1000.0);
  FResolveProg.SetUniform('SrcTex', FSrcTexture, Sampler_NoFilter);
  FResolveProg.SetUniform('SrcDistanceField', FDFTexure[FDFIndex], Sampler_NoFilter);
  FResolveProg.Draw();

  FResolveFBO.BlitToWindow(0);
end;

procedure TfrmMain.Sync3DAPI;
var targetAPI: T3DAPI;
begin
  if btnGAPI.Down then
    targetAPI := apiDX11
  else
    targetAPI := apiOGL;

  if FMain.Inited3D then
    if FMain.ActiveApi <> targetAPI then
      FMain.Free3D;

  if not FMain.Inited3D then
    FMain.Init3D(targetAPI);
end;

procedure TfrmMain.RenderScene;
var CurrentTime, DTime: Int64;
begin
  if FMain = nil then Exit;
  Sync3DAPI;

  if FMain.Bind then
  try
    PrepareDistanceField;
    BuildDistanceField;
    ResolveDistanceField;

    FMain.Present;

    CurrentTime := FMain.Time64;
    Inc(FFPSCounter);
    DTime := CurrentTime - FLastFPSTime;
    if DTime > 250 then
    begin
      Caption := 'FPS: ' + IntToStr(FFPSCounter*1000 div DTime);
      FFPSCounter := 0;
      FLastFPSTime := CurrentTime;
    end;
  finally
    FMain.Unbind;
  end;
end;

end.

