unit untMain;
{$I avConfig.inc}

interface

uses
  LCLType, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  avRes, avTypes, mutils, avTess, avContnrs;

type

  { TMeshVertex }

  TMeshVertex = packed record
    vsCoord : TVec3;
    vsNormal: TVec3;
    function Layout: IDataLayout;
  end;
  IMeshVertices = specialize IArray<TMeshVertex>;
  TMeshVeritecs = specialize TVerticesRec<TMeshVertex>;

  { TfrmMain }

  TfrmMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    procedure ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FMain: TavMainRender;
    FFrame: TavFrameBuffer;
    FTessProg: TavProgram;

    FMesh: TavVB;
    procedure RenderScene;
  public
    {$IfDef FPC}
    procedure EraseBackground(DC: HDC); override;
    {$EndIf}
    {$IfDef DCC}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$EndIf}
  end;

var
  frmMain: TfrmMain;

implementation

{$IfnDef notDCC}
  {$R *.dfm}
{$EndIf}

{$IfDef FPC}
  {$R *.lfm}
  {$R 'tess_shaders\shaders.rc'}
{$EndIf}

{ TMeshVertex }

function TMeshVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 3)
              .Add('vsNormal', ctFloat, 3)
              .Finish(SizeOf(TMeshVertex));
end;

function GetTriangleBuffer : IVerticesData;
var arr: IMeshVertices;
    v: TMeshVertex;
begin
  arr := TMeshVeritecs.Create;
  v.vsCoord := Vec(-1.0, 0.0, 0.0); v.vsNormal := Vec(0,0,-1); arr.Add(v);
  v.vsCoord := Vec( 0.0, 1.0, 0.0); v.vsNormal := Vec(0,0,-1); arr.Add(v);
  v.vsCoord := Vec( 1.0, 0.0, 0.0); v.vsNormal := Vec(0,0,-1); arr.Add(v);
  Result := arr As IVerticesData;
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FMain := TavMainRender.Create(nil);
  FMain.Window := Handle;
  FMain.Init3D(apiOGL);
//  FMain.Camera.Eye := Vec(-1.6, 1.4,-2.0);
  FMain.Projection.FarPlane := 10.0;
  FMain.Projection.NearPlane := 0.1;

  FFrame := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f]);

  FTessProg := TavProgram.Create(FMain);
  FTessProg.LoadFromJSON('Classic', True);

  FMesh := TavVB.Create(FMain);
  FMesh.Vertices := GetTriangleBuffer;
  FMesh.PrimType := ptPatches3;
  FMesh.CullMode := cmNone;
end;

procedure TfrmMain.ApplicationProperties1Idle(Sender: TObject; var Done: Boolean);
begin
  FMain.InvalidateWindow;
  Done := False;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TfrmMain.RenderScene;
begin
  if FMain = nil then Exit;
  if FMain.Bind then
  try
    FFrame.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FFrame.Select();
    FFrame.Clear(0,Vec(0.0, 0.2, 0.4, 1.0));
    FFrame.ClearDS(FMain.Projection.DepthRange.y);

    FMain.States.DepthTest := True;
    FMain.States.Wireframe := True;
    FTessProg.Select;
    FTessProg.SetAttributes(FMesh, nil, nil);
    FTessProg.Draw();

    FFrame.BlitToWindow(0);
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

procedure TfrmMain.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;

end.

