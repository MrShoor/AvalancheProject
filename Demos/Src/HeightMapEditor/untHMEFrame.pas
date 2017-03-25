unit untHMEFrame;

interface

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs,
  avRes, avContnrs, avTess, avTypes, avTexLoader,
  mutils, StdCtrls, ExtCtrls;

const
  HEIGHTMAP_ZSCALE = 255;

type
  TCameraState = packed record
    LookAt   : TVec2;
    YawPitch : TVec2;
    Dist     : Single;
    function ViewDir: TVec3;
  end;
  TCameraEditMode = (cemNone, cemDrag, cemRotate);

  TQuadVertex = packed record
    vsCoord: TVec2;
    class function Layout: IDataLayout; static;
  end;
  IQuadVertices = {$IfDef FPC}specialize{$EndIf}IArray<TQuadVertex>;
  TQuadVertices = {$IfDef FPC}specialize{$EndIf}TVerticesRec<TQuadVertex>;

  TGroundCell = packed record
    aiPosSize    : TVec4;
    aiBorderDelta: TVec4;
    aiQuadDelta  : TVec2;
    class function Layout: IDataLayout; static;
  end;
  IGroundCellVertices = {$IfDef FPC}specialize{$EndIf}IArray<TGroundCell>;
  TGroundCellVertices = {$IfDef FPC}specialize{$EndIf}TVerticesRec<TGroundCell>;

  TPanel = class (ExtCtrls.TPanel)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure PaintWindow(DC: HDC); override;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  TfrmHMEditor = class(TFrame)
    pnlRender: TPanel;
    Timer1: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure pnlRenderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure pnlRenderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pnlRenderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FrameMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FMain: TavMainRender;
    FFBO : TavFrameBuffer;

    FProg: TavProgram;
    FQuadPatchVB: TavVB;

    FCamera: TCameraState;
    FCameraMode: TCameraEditMode;
    FLastCursorXY: TVec2i;

    FHeightMapData: ITextureData;
    FNormalMapData: ITextureData;

    FHeightMap: TavTexture;
    FNormalMap: TavTexture;
  public
    procedure LoadMap;

    procedure Init;
    procedure RenderScene(ASender: TObject);

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

implementation

{$IfnDef FPC}
    {$R *.dfm}
{$Else}
    {$R *.lfm}
    {$R 'shaders\shaders.rc'}
{$EndIf}

{ TfrmHMEditor }

procedure TfrmHMEditor.AfterConstruction;
begin
  inherited;
  FCamera.LookAt := Vec(32,64);
  FCamera.YawPitch := Vec(Pi, Pi) * 0.25;
  FCamera.Dist := 200;

  pnlRender.OnPaint := {$IfDef FPC}@{$EndIf}RenderScene;
end;

destructor TfrmHMEditor.Destroy;
begin
  FreeAndNil(FMain);
  inherited;
end;

procedure TfrmHMEditor.FrameMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    FCamera.Dist := FCamera.Dist / 1.1
  else
    FCamera.Dist := FCamera.Dist * 1.1;
  if FMain <> nil then FMain.InvalidateWindow;
  Handled := True;
end;

procedure TfrmHMEditor.Init;

  function GenQuadVertices: IVerticesData;
  var data: IQuadVertices;
      v: TQuadVertex;
  begin
    data := TQuadVertices.Create;
    v.vsCoord := Vec(0, 0); data.Add(v);
    v.vsCoord := Vec(0, 1); data.Add(v);
    v.vsCoord := Vec(1, 0); data.Add(v);
    v.vsCoord := Vec(1, 1); data.Add(v);
    Result := data as IVerticesData;
  end;

begin
  LoadMap;

  FMain := TavMainRender.Create(nil);
//  FMain.Window := pnlRender.Handle;
//  FMain.Init3D(T3DAPI.apiDX11);
//
//  FMain.Bind;
//  FHeightMap := TavTexture.Create(FMain);
//  FHeightMap.AutoGenerateMips := True;
//  FHeightMap.TargetFormat := TTextureFormat.RGBA;
//  FHeightMap.TexData := LoadTexture('terraintiles.bmp');
//  FHeightMap.Build;
//  FMain.Unbind;

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  FQuadPatchVB := TavVB.Create(FMain);
  FQuadPatchVB.Vertices := GenQuadVertices;
  FQuadPatchVB.PrimType := ptPatches;
  FQuadPatchVB.CullMode := cmNone;

  FHeightMap := TavTexture.Create(FMain);
  FHeightMap.AutoGenerateMips := False;
  FHeightMap.TargetFormat := TTextureFormat.R16;
  FHeightMap.TexData := FHeightMapData;

  FNormalMap := TavTexture.Create(FMain);
  FNormalMap.AutoGenerateMips := False;
  FNormalMap.TargetFormat := TTextureFormat.RG;
  FNormalMap.TexData := FNormalMapData;

  FProg := TavProgram.Create(FMain);
  FProg.Load('default', True);
end;

procedure TfrmHMEditor.LoadMap;
const PICK_OFFSETS : array [0..7] of TVec2i = (
    (x:  0; y:  1),
    (x:  1; y:  1),
    (x:  1; y:  0),
    (x:  1; y: -1),
    (x:  0; y: -1),
    (x: -1; y: -1),
    (x: -1; y:  0),
    (x: -1; y:  1)
  );
var w, h, level: Integer;
    summ, j, i, k: Integer;
    z: Single;
    pts: array[0..7] of TVec3;
    pdstNorm: PVec2b;
    norm: TVec3;
    srcMip, dstMip: ITextureMip;
begin
  FHeightMapData := LoadRaw('..\Media\terrain3.r16', 2048, 2048, TTextureFormat.R16, True);
  //manual mip generation for heightmap
  level := 0;
  w := FHeightMapData.Width;
  h := FHeightMapData.Height;
  while (w mod 2 = 0) and (h mod 2 = 0) do
  begin
    Inc(level);
    w := w div 2;
    h := h div 2;
    srcMip := FHeightMapData.MipData(0, level - 1);
    dstMip := FHeightMapData.MipData(0, level);
    for j := 0 to dstMip.Height - 1 do
      for i := 0 to dstMip.Width - 1 do
      begin
        summ :=   PWord(srcMip.Pixel(i*2,   j*2))^;
        Inc(summ, PWord(srcMip.Pixel(i*2+1, j*2))^);
        Inc(summ, PWord(srcMip.Pixel(i*2,   j*2+1))^);
        Inc(summ, PWord(srcMip.Pixel(i*2+1, j*2+1))^);
        summ := summ div 4;
        PWord(dstMip.Pixel(i, j))^ := summ;
      end;
  end;

  //normals generation
  FNormalMapData := EmptyTexData(FHeightMapData.Width, FHeightMapData.Height, TTextureFormat.RG, True, True);
  for level := 0 to FHeightMapData.MipCount(0) - 1 do
  begin
    srcMip := FHeightMapData.MipData(0, level);
    dstMip := FNormalMapData.MipData(0, level);
    for j := 0 to srcMip.Height - 1 do
      for i := 0 to srcMip.Width - 1 do
      begin
        z := PWord(srcMip.Pixel(i,j))^/$FFFF*HEIGHTMAP_ZSCALE;
        for k := 0 to Length(pts) - 1 do
        begin
          pts[k].xy := PICK_OFFSETS[k];
          if (i + pts[k].x >= 0) and (i + pts[k].x < srcMip.Width) and
             (j + pts[k].y >= 0) and (j + pts[k].y < srcMip.Height) then
            pts[k].z := PWord(srcMip.Pixel(i+1,j))^/$FFFF*HEIGHTMAP_ZSCALE - z
          else
            pts[k].z := 0;
        end;

        {
        pts[1].x := 1;
        pts[1].y := 0;
        pts[1].z := PWord(srcMip.Pixel(i+1,j))^/$FFFF*HEIGHTMAP_ZSCALE - pts[0].z;

        pts[2].x := 0;
        pts[2].y := 1;
        pts[2].z := PWord(srcMip.Pixel(i,j+1))^/$FFFF*HEIGHTMAP_ZSCALE - pts[0].z;

        pts[3].x := -1;
        pts[3].y := 0;
        if i > 0 then
          pts[3].z := PWord(srcMip.Pixel(i-1,j))^/$FFFF*HEIGHTMAP_ZSCALE - pts[0].z
        else
          pts[3].z := 0;

        pts[4].x := 0;
        pts[4].y := -1;
        if j > 0 then
          pts[4].z := PWord(srcMip.Pixel(i,j-1))^/$FFFF*HEIGHTMAP_ZSCALE - pts[0].z
        else
          pts[4].z := 0;

        norm := Cross(pts[2], pts[1]) +
                Cross(pts[3], pts[2]) +
                Cross(pts[4], pts[3]) +
                Cross(pts[1], pts[4]);
        }
        norm := Vec(0,0,0);
        for k := 0 to Length(pts) - 1 do
          norm := norm + Cross(pts[(k + 1) mod Length(pts)], pts[k]);
        norm := normalize(norm);

        pdstNorm := PVec2b(dstMip.Pixel(i, j));
        //pdstNorm^.x := norm.x;
        //pdstNorm^.y := norm.y;
        pdstNorm^.x := Clamp(Round((norm.x+1.0)*0.5*255), 0, 255);
        pdstNorm^.y := Clamp(Round((norm.y+1.0)*0.5*255), 0, 255);
      end;
  end;

//  FTileMap := LoadTexture(ATileMap);
//  FTiles := LoadTextures(ATiles);
    //LoadRaw('terrain3.r16', 2048, 2048, TTextureFormat.R16);
end;

procedure TfrmHMEditor.pnlRenderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbMiddle then
    FMain.States.Wireframe := not FMain.States.Wireframe;

  if Button = mbRight then
  begin
    if ssShift in Shift then
      FCameraMode := cemRotate
    else
      FCameraMode := cemDrag;
  end;
  FLastCursorXY := Vec(X, Y);
  SetCaptureControl(pnlRender);
end;

procedure TfrmHMEditor.pnlRenderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var dir: TVec2;
begin
  if FCameraMode <> cemNone then
  begin
    case FCameraMode of
      cemNone: ;
      cemDrag: begin
        dir := FCamera.ViewDir.xy * FCamera.Dist;
        FCamera.LookAt := FCamera.LookAt + dir * (Y - FLastCursorXY.y)*2/FMain.WindowSize.y
                                         + Rotate90(dir, False) * (X - FLastCursorXY.x)*2/FMain.WindowSize.x;
        if FMain <> nil then FMain.InvalidateWindow;
      end;
      cemRotate: begin
        FCamera.YawPitch.x := FCamera.YawPitch.x + (X - FLastCursorXY.x) * 0.001;
        FCamera.YawPitch.y := FCamera.YawPitch.y + (Y - FLastCursorXY.y) * 0.001;
        FCamera.YawPitch.y := Clamp(FCamera.YawPitch.y, -0.5*Pi + 0.1, 0.5*Pi - 0.1);
        if FMain <> nil then FMain.InvalidateWindow;
      end;
    end;
  end;
  FLastCursorXY := Vec(X, Y);
end;

procedure TfrmHMEditor.pnlRenderMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FCameraMode := cemNone;
  SetCaptureControl(nil);
end;

procedure TfrmHMEditor.RenderScene(ASender: TObject);
var CellSize: Integer;
begin
  If FMain = nil Then
  begin
        Init;
  end;
  if FMain = nil then Exit;
  if not FMain.Inited3D then
  begin
    FMain.Window := pnlRender.Handle;
    FMain.Init3D(T3DAPI.apiDX11);
  end;
  if not FMain.Inited3D then Exit;

  if FMain.Bind then
  try
    FMain.Camera.At := Vec(FCamera.LookAt, 0);
    FMain.Camera.Eye := FMain.Camera.At - FCamera.ViewDir*FCamera.Dist;
    //FMain.Camera.At := Vec(0, 0, 0);
    //FMain.Camera.Eye := Vec(-1,-1,-1);//*30;
    FMain.Camera.Up := Vec(0,0,-1);

    FHeightMap.Build;

    FMain.Projection.NearPlane := 0.1;
    FMain.Projection.FarPlane := 10000;
    FMain.Projection.Fov := Pi * 0.25;

    FFBO.FrameRect := RectI(0, 0, FMain.WindowSize.x, FMain.WindowSize.y);
    FFBO.Select();
    FFBO.Clear(0, Vec(0,0.1,0,0));
    FFBO.ClearDS(1);

    FMain.States.DepthTest := True;

    CellSize := 64;

    FProg.Select(4);
    FProg.SetAttributes(FQuadPatchVB, nil, nil);
    FProg.SetUniform('fArea', Vec(0.0, 0.0, 2048.0 / CellSize - 1.0, 2048.0 / CellSize - 1.0));
    //FProg.SetUniform('fArea', Vec(0.0,0.0,2.0,2.0));
    FProg.SetUniform('CellSize', CellSize*1.0);
    FProg.SetUniform('ViewPortSize', FFBO.FrameRect.Size);
    FProg.SetUniform('HeightMap', FHeightMap, Sampler_Linear);
    FProg.SetUniform('HeightNormalMap', FNormalMap, Sampler_Linear);
    FProg.Draw((2048 div CellSize - 1)*(2048 div CellSize - 1));
    //FProg.Draw(4);

    FMain.States.DepthTest := False;

    FFBO.BlitToWindow(0);
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

procedure TfrmHMEditor.Timer1Timer(Sender: TObject);
begin
  if FMain <> nil then
    FMain.InvalidateWindow;
end;

{ TPanel }

procedure TPanel.PaintWindow(DC: HDC);
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self)
  else
    inherited;
end;

{ TQuadVertex }

class function TQuadVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 2).Finish();
end;

{ TCameraState }

function TCameraState.ViewDir: TVec3;
var v: TVec3;
begin
  v := Quat(Vec(0,1,0), YawPitch.y) * Vec(1,0,0);
  v := Quat(Vec(0,0,-1), YawPitch.x) * v;
  Result := v;
end;

{ TGroundCell }

class function TGroundCell.Layout: IDataLayout;
begin
  Result := LB.Add('aiPosSize', ctFloat, 4).
               Add('aiBorderDelta', ctFloat, 4).
               Add('aiQuadDelta', ctFloat, 2).
               Finish();
end;

end.
