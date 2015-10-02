unit untMain;

{$mode objfpc}{$H+}
{$R 'MeshShader\shaders.res'}

interface

uses
  LMessages, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, avRes, avTypes, mutils, avCameraController, avModel;

const ObjInd = 0;

type

  { TPanel }

  TPanel = class (ExtCtrls.TPanel)
  private
    FOnRepaint: TNotifyEvent;
  protected
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure Paint; override;
  public
    property OnRepaint: TNotifyEvent read FOnRepaint write FOnRepaint;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    cbDirectX11: TRadioButton;
    cbOGL: TRadioButton;
    cbWireframe: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    lbAnimations: TListBox;
    lbNames: TListBox;
    Panel1: TPanel;
    RenderPanel: TPanel;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    procedure Sync3DApi;
    procedure RenderScene;
    procedure RenderPanelRepaint(Sender: TObject);
  public
    FMain: TavMainRender;
    FFBO : TavFrameBuffer;

    FModels: TavModelCollection;

    FProg: TavProgram;

    FInstances: IModelInstanceArr;

    procedure LoadModels;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Math;

{$R *.lfm}

{ TPanel }

procedure TPanel.WMEraseBkgnd(var Message: TLMEraseBkgnd);
begin
  if Assigned(FOnRepaint) then
    Message.Result := 1
  else
    inherited;
end;

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
  RenderPanel.OnRepaint := @RenderPanelRepaint;

  FMain := TavMainRender.Create(Nil);
  FMain.Window := RenderPanel.Handle;
//  FMain.Projection.Ortho := True;
  FMain.Camera.Eye := Vec(0,10,-30);
  FMain.Projection.OrthoHeight := 20;

  FFBO := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  FModels := TavModelCollection.Create(FMain);

  FProg := TavProgram.Create(FMain);
  FProg.LoadFromJSON('avMesh', True);

  with TavCameraController.Create(FMain) do
  begin
    CanRotate := True;
    CanMove := True;
    MovePlane := Plane(0,0,1,0);
  end;

  LoadModels;
end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  if Assigned(FMain) then
    FMain.InvalidateWindow;
  Done := False;
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
  function GetVisibleInstances(const AllInstances: IModelInstanceArr): IModelInstanceArr;
  var i: Integer;
  begin
    Result := TModelInstanceArr.Create(nil);
    for i := 0 to min(AllInstances.Count, lbNames.Count) - 1 do
      if lbNames.Selected[i] then
        Result.Add(AllInstances[i]);
  end;
  procedure SyncAnimations(const Instances: IModelInstanceArr);
  var i, j: Integer;
  begin
    for i := 0 to Instances.Count - 1 do
      for j := 0 to lbAnimations.Count - 1 do
      begin
        if lbAnimations.Selected[j] then
          Instances[i].AnimationStart(lbAnimations.Items[j])
        else
          Instances[i].AnimationStop(lbAnimations.Items[j]);
      end;
  end;

var visInst: IModelInstanceArr;
begin
  Sync3DApi;

  if FMain = Nil then Exit;
  if not FMain.Inited3D then Exit;

  if FMain.Bind then
  try
    SyncAnimations(FInstances);
    visInst := GetVisibleInstances(FInstances);

    FMain.States.DepthTest := True;
    FMain.States.Wireframe := cbWireframe.Checked;

    FFBO.FrameRect := RectI(0,0,FMain.WindowSize.x, FMain.WindowSize.y);
    FFBO.Select();
    FFBO.Clear(0, Vec(0,0,0,0));
    FFBO.ClearDS(1);

    FProg.Select;

    if visInst.Count > 0 then
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

procedure TfrmMain.RenderPanelRepaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TfrmMain.LoadModels;
var MName: String;
    newInst: IavModelInstance;
    animations: TStringList;
    i: Integer;
begin
  FModels.AddFromFile('test.txt');
  FInstances := TModelInstanceArr.Create(nil);
  lbNames.Clear;
  lbAnimations.Clear;

  animations := TStringList.Create;
  animations.Sorted := True;
  animations.Duplicates := dupIgnore;
  try
    FModels.Reset;
    while FModels.Next(MName) do
    begin
      newInst := FModels.CreateInstance( MName, IdentityMat4 );
      FInstances.Add(newInst);
      lbNames.Items.Add(MName);
      for i := 0 to newInst.AnimationCount - 1 do
        animations.Add(newInst.AnimationName(i));
    end;
    lbNames.SelectAll;

    lbAnimations.Items.AddStrings(animations);
  finally
    animations.Free;
  end;
end;

end.

