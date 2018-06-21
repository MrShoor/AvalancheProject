unit untHMEFrame;

interface

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

uses
  {$IfDef FPC}
  LMessages, LCLType, LCLIntf,
  {$EndIf}
  {$IfDef DCC}
  Windows, Messages,
  {$EndIf}
  SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs,
  avRes, avContnrs, avTess, avTypes, avTexLoader,
  HMEUtils, HMEBrush,
  mutils, StdCtrls, ExtCtrls, Types;

const
  HEIGHTMAP_ZSCALE = 255;
  GROUNDCELL_SCALE = 32;

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

  { TPanel }

  TPanel = class (ExtCtrls.TPanel)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure PaintWindow(DC: HDC); override;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
  end;

  { TfrmHMEditor }

  TfrmHMEditor = class(TFrame)
    pnlRender: TPanel;
    Timer1: TTimer;
    BrushTimer: TTimer;
    procedure BrushTimerTimer(Sender: TObject);
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
    FGroundPathes: TavVB;

    FCamera: TCameraState;
    FCameraMode: TCameraEditMode;
    FLastCursorXY: TVec2i;

    FHeightMapData: ITextureData;

    FHeightMap: TavTexture;
    FNormalMap: TavTexture;

    FNMBuilder: TavNormalMapBuilder;
    FGBuilder : TavGroundQuadBuilder;

    FInBrushMode  : Boolean;
    FMapBrush     : TavHMEBrush;
    FMapBrushPlane: TPlane;
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
  if (Shift = [ssCtrl]) and (FMapBrush <> nil) then
  begin
    if WheelDelta > 0 then
      FMapBrush.Radius := FMapBrush.Radius / 1.1
    else
      FMapBrush.Radius := FMapBrush.Radius * 1.1;
    if FMain <> nil then FMain.InvalidateWindow;
  end;

  if Shift = [] then
  begin
    if WheelDelta > 0 then
      FCamera.Dist := FCamera.Dist / 1.1
    else
      FCamera.Dist := FCamera.Dist * 1.1;
    if FMain <> nil then FMain.InvalidateWindow;
  end;
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

  function GenGroundPatchesVertices: IVerticesData;
  var data: IGroundCellVertices;
      v: TGroundCell;
      i: Integer;
  begin
    data := TGroundCellVertices.Create;
    data.Capacity := (2048 div GROUNDCELL_SCALE) * (2048 div GROUNDCELL_SCALE);
    v.aiPosSize := Vec(0,1,2,3);
    v.aiBorderDelta := Vec(4,5,6,7);
    v.aiQuadDelta := Vec(8,9);
    for i := 0 to data.Capacity - 1 do
      data.Add(v);
    Result := data as IVerticesData;
  end;

begin
  LoadMap;

  FMain := TavMainRender.Create(nil);

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  FQuadPatchVB := TavVB.Create(FMain);
  FQuadPatchVB.Vertices := GenQuadVertices;
  FQuadPatchVB.PrimType := ptPatches;
  FQuadPatchVB.CullMode := cmNone;

  FGroundPathes := TavVB.Create(FMain);
  FGroundPathes.Vertices := GenGroundPatchesVertices;
  FGroundPathes.PrimType := ptPoints;
  FGroundPathes.CullMode := cmNone;

  FHeightMap := TavTexture.Create(FMain);
  FHeightMap.AutoGenerateMips := True;
  FHeightMap.TargetFormat := TTextureFormat.R16;
  FHeightMap.TexData := FHeightMapData;

  FNormalMap := TavTexture.Create(FMain);
  FNormalMap.AutoGenerateMips := True;
  FNormalMap.TargetFormat := TTextureFormat.RG;

  FProg := TavProgram.Create(FMain);
  FProg.Load('default', True);

  FNMBuilder := TavNormalMapBuilder.Create(FMain);
  FGBuilder  := TavGroundQuadBuilder.Create(FMain);

  FMapBrush  := TavHMEBrush.Create(FMain);
  FMapBrush.SetHeightMap(FHeightMap);
end;

procedure TfrmHMEditor.LoadMap;
begin
  if FileExists('..\Media\terrain3.r16') then
    FHeightMapData := LoadRaw('..\Media\terrain3.r16', 2048, 2048, TTextureFormat.R16)
  else
    FHeightMapData := LoadRaw('terrain3.r16', 2048, 2048, TTextureFormat.R16);
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

  if Button = mbLeft then
  begin
    FInBrushMode := True;
    FMapBrushPlane.Norm := Vec(0,0,-1);
    FMapBrushPlane.D := GetHeightAt(FHeightMapData, FMapBrush.Pos.x, FMapBrush.Pos.y, HEIGHTMAP_ZSCALE);
  end;

  FLastCursorXY := Vec(X, Y);
  SetCaptureControl(pnlRender);
end;

procedure TfrmHMEditor.pnlRenderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var dir: TVec2;
    IntPt: TVec3;
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

  if (FHeightMapData <> nil) then
  begin
    if FInBrushMode then
    begin
      if Intersect(FMapBrushPlane, FMain.Cursor.Ray, IntPt) then
      begin
        FMapBrush.Pos := IntPt.xy;
      end;
      BrushTimerTimer(nil);
    end
    else
      if RayCast(FHeightMapData, HEIGHTMAP_ZSCALE, FMain.Cursor.Ray, IntPt) then
        FMapBrush.Pos := IntPt.xy;
  end;

  FLastCursorXY := Vec(X, Y);
end;

procedure TfrmHMEditor.pnlRenderMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    FCameraMode := cemNone;
  if Button = mbLeft then
  begin
    if FInBrushMode then
      FHeightMap.ReadBack(FHeightMapData, 0, 0);
    FInBrushMode := False;
  end;
  SetCaptureControl(nil);
end;

procedure TfrmHMEditor.RenderScene(ASender: TObject);
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

    FNMBuilder.Build(FHeightMap, FNormalMap, HEIGHTMAP_ZSCALE, True);
    FGBuilder.Build(Round(FHeightMap.Size), GROUNDCELL_SCALE, FHeightMap, HEIGHTMAP_ZSCALE, FGroundPathes);
  end;
  if not FMain.Inited3D then Exit;

  if FMain.Bind then
  try
    FMain.Camera.At := Vec(FCamera.LookAt, 0);
    FMain.Camera.Eye := FMain.Camera.At - FCamera.ViewDir*FCamera.Dist;
    //FMain.Camera.At := Vec(0, 0, 0);
    //FMain.Camera.Eye := Vec(-1,-1,-1);//*30;
    FMain.Camera.Up := Vec(0,0,-1);

    //FHeightMap.Build;

    FMain.Projection.NearPlane := 0.1;
    FMain.Projection.FarPlane := 10000;
    FMain.Projection.Fov := Pi * 0.25;

    FFBO.FrameRect := RectI(0, 0, FMain.WindowSize.x, FMain.WindowSize.y);
    FFBO.Select();
    FFBO.Clear(0, Vec(0,0.1,0,0));
    FFBO.ClearDS(1);

    FMain.States.DepthTest := True;

    FQuadPatchVB.CullMode := cmBack;
    FProg.Select(4);
    FProg.SetAttributes(FQuadPatchVB, nil, FGroundPathes);
    //FProg.SetUniform('fArea', Vec(0.0,0.0,2.0,2.0));
    FProg.SetUniform('CellSize', GROUNDCELL_SCALE*1.0);
    FProg.SetUniform('ViewPortSize', FFBO.FrameRect.Size);
    FProg.SetUniform('HeightMap', FHeightMap, Sampler_LinearClamped);
    FProg.SetUniform('HeightNormalMap', FNormalMap, Sampler_LinearClamped);

    FProg.SetUniform('BrushPos', FMapBrush.Pos);
    FProg.SetUniform('BrushRadius', FMapBrush.Radius);
    FProg.SetUniform('BrushSharp', FMapBrush.Sharp);

    FProg.Draw(FGroundPathes.BuildedVertCount);
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

procedure TfrmHMEditor.BrushTimerTimer(Sender: TObject);
begin
  if not FInBrushMode then Exit;
  if FMain = nil then Exit;
  if not FMain.Inited3D then Exit;
  if FMapBrush = nil then Exit;

  if FMain.Bind then
  Try
    if GetKeyState(VK_SHIFT) < 0 then
      FMapBrush.ApplyBrush_Smooth()
    else
      FMapBrush.ApplyBrush(GetKeyState(VK_CONTROL) >= 0);
    FHeightMap.GenerateMips;
    FNMBuilder.Build(FHeightMap, FNormalMap, HEIGHTMAP_ZSCALE, True);
    FGBuilder.Build(Round(FHeightMap.Size), GROUNDCELL_SCALE, FHeightMap, HEIGHTMAP_ZSCALE, FGroundPathes);

    FFBO.Select();
    FMain.InvalidateWindow;
  finally
    FMain.Unbind;
  end;
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

end.
