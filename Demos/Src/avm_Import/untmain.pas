unit untMain;
{$I avConfig.inc}

interface

uses
  {$IfDef FPC}
  LCLType,
  FileUtil,
  LMessages,
  {$EndIf}
  {$IfnDef notDCC}
  Windows,
  Messages,
  Vcl.AppEvnts,
  {$EndIf}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, avRes, avTypes, mutils, avCameraController, avModel, avMesh,
  avTexLoader;

const ObjInd = 0;

type

  { TPanel }

  TPanel = class (ExtCtrls.TPanel)
  private
    FOnRepaint: TNotifyEvent;
  protected
    {$IfDef FPC}
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    {$EndIf}
    {$IfDef DCC}
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    {$EndIf}
    procedure Paint; override;
  public
    property OnRepaint: TNotifyEvent read FOnRepaint write FOnRepaint;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    {$IfDef FPC}
    ApplicationProperties1: TApplicationProperties;
    {$Else}
    ApplicationProperties1: TApplicationEvents;
    {$EndIf}
    btnLoad: TButton;
    btnClear: TButton;
    cbDirectX11: TRadioButton;
    cbOGL: TRadioButton;
    cbWireframe: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    lbAnimations: TListBox;
    lbNames: TListBox;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    RenderPanel: TPanel;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure btnClearClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure RenderPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    procedure Sync3DApi;
    procedure RenderScene;
    procedure RenderPanelRepaint(Sender: TObject);
  public
    FMain: TavMainRender;
    FFBO : TavFrameBuffer;

    FModels: TavModelCollection;

    FProg: TavProgram;

    FInstances: IavModelInstanceArr;
    FAnimC: IavAnimationController;

    FIrradiance    : TavTexture;
    FRadiance      : TavTexture;
    FHammersleyPts : TVec4Arr;

    procedure LoadModels(const AFileName: string);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Math;

{$IfnDef notDCC}
  {$R *.dfm}
{$EndIf}

{$IfDef FPC}
  {$R *.lfm}
  {$R 'MeshShader\shaders.rc'}
{$EndIf}

{ TPanel }

{$IfDef FPC}
procedure TPanel.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if Assigned(FOnRepaint) then
    Message.Result := 1
  else
    inherited;
end;
{$EndIf}
{$IfDef DCC}
procedure TPanel.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  if Assigned(FOnRepaint) then
    Message.Result := 1
  else
    inherited;
end;
{$EndIf}

procedure TPanel.Paint;
begin
  if Assigned(FOnRepaint) then
    FOnRepaint(Self)
  else
    inherited;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$IfDef FPC}
  RenderPanel.OnRepaint := @RenderPanelRepaint;
  {$Else}
  RenderPanel.OnRepaint := RenderPanelRepaint;
  {$EndIf}

  FMain := TavMainRender.Create(Nil);
  FMain.Window := RenderPanel.Handle;
//  FMain.Projection.Ortho := True;
  FMain.Projection.NearPlane := 0.1;
  FMain.Camera.At := Vec(0,0.5,0);
  FMain.Camera.Eye := Vec(-3,0.5,0);
  FMain.Projection.OrthoHeight := 4;

  FMain.Camera.At := Vec(0,10,0);
  FMain.Camera.Eye := Vec(20,18,-10);
  FMain.Projection.OrthoHeight := 4;

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  FModels := TavModelCollection.Create(FMain);

  FProg := TavProgram.Create(FMain);
  FProg.Load('avMesh', True, '');//D:\Projects\AvalancheProject\Demos\Src\avm_Import\MeshShader\!Out');

  with TavCameraController.Create(FMain) do
  begin
    CanRotate := True;
    CanMove := True;
    MovePlane := Plane(0,0,1,0);
  end;

//  LoadModels(ExtractFilePath(ParamStr(0))+'\..\Media\WhipperNude\WhipperNude.avm');
  LoadModels(ExtractFilePath(ParamStr(0))+'\..\Media\Char\char.avm');

  FIrradiance := TavTexture.Create(FMain);
  FIrradiance.TargetFormat := TTextureFormat.RGBA16f;
  FIrradiance.TexData := LoadTexture(ExtractFilePath(ParamStr(0))+'\..\Media\EnvMaps\Campus_irradiance.dds');
  //FIrradiance.TexData := LoadTexture(ExtractFilePath(ParamStr(0))+'\..\Media\EnvMaps\Grace_Cathedral_irradiance.dds');

  FRadiance := TavTexture.Create(FMain);
  FRadiance.TargetFormat := TTextureFormat.RGBA16f;
  FRadiance.TexData := LoadTexture(ExtractFilePath(ParamStr(0))+'\..\Media\EnvMaps\Campus_radiance.dds');
  //FRadiance.TexData := LoadTexture(ExtractFilePath(ParamStr(0))+'\..\Media\EnvMaps\Grace_Cathedral_radiance.dds');

  FHammersleyPts := GenerateHammersleyPts(16);
end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if Assigned(FMain) then
    FMain.InvalidateWindow;
  Done := False;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  lbNames.Clear;
  lbAnimations.Clear;
  FInstances.Clear();
  FreeAndNil(FModels);
  FModels := TavModelCollection.Create(FMain);
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  if OpenDialog.Execute then
    LoadModels(OpenDialog.FileName);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.Sync3DApi;
var selectedAPI: T3DAPI;
begin
  if cbDirectX11.Checked then
    selectedAPI := apiDX11
  else
    selectedAPI := apiOGL;
  if Assigned(FMain) then
  begin
    if FMain.Inited3D then
      if FMain.ActiveApi <> selectedAPI then
        FMain.Free3D;

    if not FMain.Inited3D then
      FMain.Init3D(selectedAPI);
  end;
end;

procedure TfrmMain.RenderScene;
  function GetVisibleInstances(const AllInstances: IavModelInstanceArr): IavModelInstanceArr;
  var i: Integer;
  begin
    if AllInstances = nil then Exit;
    Result := TavModelInstanceArr.Create();
    for i := 0 to min(AllInstances.Count, lbNames.Count) - 1 do
      if lbNames.Selected[i] then
        Result.Add(AllInstances[i]);
  end;
  procedure SyncAnimations();
  var i, j: Integer;
  begin
    if FAnimC = nil then Exit;
    FAnimC.SetTime(FMain.Time64);
    for j := 0 to lbAnimations.Count - 1 do
    begin
      if lbAnimations.Selected[j] then
        FAnimC.AnimationStart(lbAnimations.Items[j])
      else
        FAnimC.AnimationStop(lbAnimations.Items[j]);
    end;
  end;

var visInst: IavModelInstanceArr;
begin
  Sync3DApi;

  if FMain = Nil then Exit;
  if not FMain.Inited3D then Exit;

  if FMain.Bind then
  try
    SyncAnimations();
    visInst := GetVisibleInstances(FInstances);

    FMain.States.DepthTest := True;
    FMain.States.Wireframe := cbWireframe.Checked;

    FFBO.FrameRect := RectI(0,0,FMain.WindowSize.x, FMain.WindowSize.y);
    FFBO.Select();
    FFBO.Clear(0, Vec(0,0,0,0));
    FFBO.ClearDS(1);

    FProg.Select;
    FProg.SetUniform('uRadiance', FRadiance, Sampler_Linear);
    FProg.SetUniform('uIrradiance', FIrradiance, Sampler_Linear);
    FProg.SetUniform('uHammersleyPts', FHammersleyPts);
    FProg.SetUniform('uSamplesCount', Length(FHammersleyPts)*1.0);

    if assigned(visInst) and (visInst.Count > 0) then
    begin
      FModels.Select;
      FModels.Draw(visInst);
    end;

    FFBO.BlitToWindow();
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

procedure TfrmMain.RenderPanelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  RenderPanel.SetFocus;
  FProg.Invalidate;
end;

procedure TfrmMain.RenderPanelRepaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TfrmMain.LoadModels(const AFileName: string);
var newInst: IavModelInstance;
    animations: TStringList;
    i: Integer;

    meshes: IavMeshes;
    meshInstances: IavMeshInstances;
    meshInst: IavMeshInstance;
    newModelInstances: IavModelInstanceArr;
begin
  avMesh.LoadFromFile(AFileName, meshes, meshInstances);

  FAnimC := nil;
  meshInstances.Reset;
  while meshInstances.NextValue(meshInst) do
  begin
    if meshInst.Pose <> nil then
    begin
      FAnimC := Create_IavAnimationController(meshInst.Pose, FMain.Time64);
      break;
    end;
  end;

  newModelInstances := FModels.AddFromMeshInstances(meshInstances);
  FInstances := TavModelInstanceArr.Create;
  lbNames.Clear;
  lbAnimations.Clear;

  animations := TStringList.Create;
  animations.Sorted := True;
  animations.Duplicates := dupIgnore;
  if Assigned(FAnimC) then
    for i := 0 to FAnimC.Pose.Armature.AnimCount - 1 do
      animations.Add(FAnimC.Pose.Armature.Anim[i].Name);
  try
    FModels.Reset;
    while FModels.NextInstance(newInst) do
    begin
      FInstances.Add(newInst);
      lbNames.Items.Add(newInst.Name);
    end;
    lbNames.SelectAll;

    lbAnimations.Items.AddStrings(animations);
  finally
    animations.Free;
  end;
end;

end.

