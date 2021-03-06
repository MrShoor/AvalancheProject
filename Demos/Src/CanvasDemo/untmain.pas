unit untmain;
{$I avConfig.inc}

interface

uses
  {$IfDef FPC}
  LCLType,
  FileUtil,
  {$Else}
  Windows,
  Messages,
  AppEvnts,
  {$EndIf}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, avRes, avTypes, avTess, mutils,
  avCanvas;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    {$IfDef DCC}
    ApplicationEvents: TApplicationEvents;
    {$EndIf}
    {$IfDef FPC}
    ApplicationProperties: TApplicationProperties;
    {$EndIf}
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure cbDXChange(Sender: TObject);
    procedure cbOGLChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    FMain: TavMainRender;
    FFrameBuffer: TavFrameBuffer;

    FCnv  : TavCanvas;
    FCnv2 : TavCanvas;
    FCnv3 : TavCanvas;
    FCnv4 : TavCanvas;

    FFPSCounter: Integer;
    FFPSMeasureTime: Integer;
  public
    procedure RenderScene;
  end;

var
  frmMain: TfrmMain;

implementation

{$IfnDef notDCC}
  {$R *.dfm}
{$EndIf}

{$IfDef FPC}
  {$R *.lfm}
{$EndIf}

function RoseFileName(): string;
begin
  Result := ExtractFilePath(ParamStr(0)) + '..\Media\rose.png';
end;

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);

  procedure BuildCanvas(const cnv: TavCanvas);
  var s: String;
      txt: ITextLines;
  begin
    cnv.Font.Size := 100;
    cnv.Font.Style := [];
    cnv.Font.Name := 'Segoe UI';

    s := '0123456789 The quick brown fox jumps over the lazy dog';
    with cnv.TextBuilder do
    begin
      Align := TLineAlign.laCenter;
      while cnv.Font.Size > 8 do
      begin
        cnv.Font.Size := cnv.Font.Size * 0.85;
        WriteLn(s);
      end;
      txt := Finish();
      txt.BoundsY := Vec(-1100, 1100);
      txt.BoundsX := Vec(-1100, 1100);
      txt.VAlign := 0.5;
      cnv.AddText(txt);
    end;
  end;

  procedure BuildCanvas2(const cnv: TavCanvas);
  var s: String;
      txt: ITextLines;
      xbounds, ybounds: TVec2;
  begin
    cnv.Font.Size := 64;
    cnv.Font.Style := [];
    cnv.Font.Name := 'Segoe UI';
    s := 'j01234567890 The quick brown fox jumps over the lazy dog';
    with cnv.TextBuilder do
    begin
      xbounds := Vec(0, 500);
      ybounds := Vec(0, 100);

      cnv.Pen.Color := Vec(1,1,1,1);
      cnv.Brush.Color := Vec(0,0,0,1);
      cnv.AddFill(Vec(xbounds.x, ybounds.x), Vec(xbounds.y, ybounds.y));
      cnv.AddRectangle(Vec(xbounds.x, ybounds.x), Vec(xbounds.y, ybounds.y));

      WriteWrapped(s);
      WriteWrappedEnd(xbounds.y-xbounds.x, True, 0, 0);
      txt := Finish();
      txt.BoundsY := ybounds;
      txt.BoundsX := xbounds;
      txt.VAlign := 0.0;
      txt.ClipWithBounds := True;
      cnv.AddText(txt);
    end;
  end;

begin
  FMain := TavMainRender.Create(Nil);
  FMain.Window := Handle;
  FMain.Init3D(apiDX11);
  FMain.Camera.Eye := Vec(-1.6, 1.4,-2.0);
  FMain.Projection.FarPlane := 10.0;
  FMain.Projection.NearPlane := 0.1;

  //FFrameBuffer := Create_FrameBufferMultiSampled(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], 8, [true, false]);
  FFrameBuffer := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], [true, false]);

  FCnv := TavCanvas.Create(FMain);
  FCnv.Font.Color := Vec(1,1,1,1);
  BuildCanvas(FCnv);

  FCnv2 := TavCanvas.Create(FMain);
  FCnv2.Font.Color := Vec(0,0,0,1);
  BuildCanvas(FCnv2);

  FCnv3 := TavCanvas.Create(FMain);
  FCnv3.Font.Name := 'Courier New';
  FCnv3.Font.Style := [gsBold];
  FCnv3.Font.Size := 35;
  FCnv3.Font.Color := Vec(1,0,0,1);
  FCnv3.Brush.Hinting := [TBrushHintingStyle.Horizontal, TBrushHintingStyle.Vertical];

  FCnv4 := TavCanvas.Create(FMain);
  FCnv4.Font.Size := 42;
  FCnv4.Font.Color := Vec(0,1,0,1);
  BuildCanvas2(FCnv4);
end;

procedure TfrmMain.cbOGLChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfrmMain.cbDXChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfrmMain.ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  if FMain <> nil then FMain.InvalidateWindow;
  Done := False;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FMain);
end;

procedure TfrmMain.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  GetCanvasCommonData(FMain).ReloadShaders;
end;

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TfrmMain.RenderScene;
  procedure UpdateFPS;
  var measureTime: Int64;
      offset: Single;
  begin
    measureTime := FMain.Time64 div 100;
    offset := FMain.Time;
    if measureTime > FFPSMeasureTime then
    begin
      FCnv3.Clear;
      FCnv3.Brush.Color := Vec(1,1,1,1);
      FCnv3.AddSprite(Vec(10 + offset, 10), Vec(266 + offset, 266), RoseFileName());

      FCnv3.Brush.Color := Vec(0,0,1,1);
      FCnv3.AddFill(Vec(10 + offset, 10+256), Vec(266 + offset, 10+256+10));
      with FCnv3.TextBuilder do
      begin
        WriteLn('FPS: ' + IntToStr(FFPSCounter*10 + Random(10)));
        FCnv3.AddText(Finish());
      end;
      FFPSMeasureTime := measureTime;
      FFPSCounter := 0;
    end
    else
      Inc(FFPSCounter);
  end;

var a: single;
begin
  if FMain = nil then Exit;

  UpdateFPS;

  if FMain.Bind then
  try
    FMain.States.DepthTest := True;

    FFrameBuffer.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FFrameBuffer.Select;

    FMain.Clear(Vec(0.0,0.2,0.4,1.0), True, FMain.Projection.DepthRange.y, True);

    FMain.States.DepthTest := False;
    FMain.States.Blending[0] := True;
    FMain.States.SetBlendFunctions(bfOne, bfInvSrcAlpha);

    a := FMain.Time * 0.25;

    if FMain.Time64 mod 2000 < 1000 then
    begin
      FMain.Clear(Vec(0.0,0.0,0.0,1.0), True, FMain.Projection.DepthRange.y, True);
      FCnv.Draw(a, Vec(ClientWidth * 0.5, ClientHeight * 0.5), 1);
    end
    else
    begin
      FMain.Clear(Vec(1.0,1.0,1.0,1.0), True, FMain.Projection.DepthRange.y, True);
      FCnv2.Draw(a, Vec(ClientWidth * 0.5, ClientHeight * 0.5), 1);
    end;
    FCnv3.Draw(0, Vec(0,0), 1);

    FCnv4.Draw(0, Vec(200,50), 1);

    FFrameBuffer.BlitToWindow;
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

end.

