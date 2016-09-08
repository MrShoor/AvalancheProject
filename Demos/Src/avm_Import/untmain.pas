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
  ExtCtrls, StdCtrls, avRes, avTypes, mutils, avCameraController, avModel, avMesh;

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
  FMain.Camera.At := Vec(0,2.5,0);
  FMain.Camera.Eye := Vec(0,2.5,-4);
  FMain.Projection.OrthoHeight := 4;

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  FModels := TavModelCollection.Create(FMain);

  FProg := TavProgram.Create(FMain);
  FProg.Load('avMesh', True);

  with TavCameraController.Create(FMain) do
  begin
    CanRotate := True;
    CanMove := True;
    MovePlane := Plane(0,0,1,0);
  end;

  LoadModels('D:\Character\Out\char.avm');
//  LoadModels(ExtractFilePath(ParamStr(0))+'\..\Media\WhipperNude\WhipperNude.avm');
//  LoadModels(ExtractFilePath(ParamStr(0))+'\..\Media\NewI\mesh.avm');
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
  //procedure SyncAnimations(const Instances: IavModelInstanceArr);
  //var i, j: Integer;
  //begin
  //  if Instances = nil then Exit;
  //  for i := 0 to Instances.Count - 1 do
  //  begin
  //    for j := 0 to lbAnimations.Count - 1 do
  //    begin
  //      if lbAnimations.Selected[j] then
  //        Instances[i].AnimationStart(lbAnimations.Items[j])
  //      else
  //        Instances[i].AnimationStop(lbAnimations.Items[j]);
  //    end;
  //  end;
  //end;

var visInst: IavModelInstanceArr;
begin
  Sync3DApi;

  if FMain = Nil then Exit;
  if not FMain.Inited3D then Exit;

  if FMain.Bind then
  try
    //SyncAnimations(FInstances);
    visInst := GetVisibleInstances(FInstances);

    FMain.States.DepthTest := True;
    FMain.States.Wireframe := cbWireframe.Checked;

    FFBO.FrameRect := RectI(0,0,FMain.WindowSize.x, FMain.WindowSize.y);
    FFBO.Select();
    FFBO.Clear(0, Vec(0,0,0,0));
    FFBO.ClearDS(1);

    FProg.Select;

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
begin
  avMesh.LoadFromFile(AFileName, meshes, meshInstances);

  FModels.AddFromMeshInstances(meshInstances);
  FInstances := TavModelInstanceArr.Create;
  lbNames.Clear;
  lbAnimations.Clear;

  animations := TStringList.Create;
  animations.Sorted := True;
  animations.Duplicates := dupIgnore;
  try
    FModels.Reset;

    while FModels.NextInstance(newInst) do
    begin
      FInstances.Add(newInst);
      lbNames.Items.Add(newInst.Name);
      //for i := 0 to newInst.AnimationCount - 1 do
      //  animations.Add(newInst.AnimationName(i));
    end;
    lbNames.SelectAll;

    lbAnimations.Items.AddStrings(animations);
  finally
    animations.Free;
  end;
end;

end.

