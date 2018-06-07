unit untmain;
{$I avConfig.inc}

interface

uses
  {$IfDef FPC}
  LCLType,
  FileUtil,
  {$EndIf}
  {$IfDef DCC}
  Windows,
  Messages,
  {$EndIf}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, avRes, avTypes, avTess, mutils,
  avCanvas,
  ContextSwitcher;

type
  { TfrmMain }

  TfrmMain = class(TForm)
    Timer1: TTimer;
    procedure cbDXChange(Sender: TObject);
    procedure cbOGLChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FMain: TavMainRender;
    FFrameBuffer: TavFrameBuffer;

    FDummy: TavCanvas;

    FCnv  : TavCanvas;
    FCnv2 : TavCanvas;
  public
    {$IfDef FPC}
    procedure EraseBackground(DC: HDC); override;
    {$EndIf}
    {$IfDef DCC}
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    {$EndIf}
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
      Align := TLineAlign.laRight;
      while cnv.Font.Size > 8 do
      begin
        cnv.Font.Size := cnv.Font.Size * 0.85;
        WriteLn(s);
      end;
      txt := Finish();
      txt.BoundsY := Vec(0, 1100);
      txt.VAlign := 0.5;
      cnv.Text(txt);
    end;
  end;

var
  i: Integer;
begin
  FMain := TavMainRender.Create(Nil);
  FMain.Window := Handle;
  FMain.Init3D(apiDX11);
  FMain.Camera.Eye := Vec(-1.6, 1.4,-2.0);
  FMain.Projection.FarPlane := 10.0;
  FMain.Projection.NearPlane := 0.1;

  //TavContextSwitcher.Create(FMain);

  FFrameBuffer := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], [true, false]);

  {
  FDummy := TavCanvas.Create(FMain);
  FDummy.Font.Style := [];
  FDummy.Font.Name := 'Segoe UI';
  with FDummy.TextBuilder do
  begin
    for i := 0 to 255 do
      Write(Char(i));
    WriteLn('');
    FDummy.Text(Finish());
  end;
  }

  //GetCanvasCommonData(FMain).ExportGlyphs('D:\font.glyphs', 'Segoe UI', [], Char(33), Char(255));

  FCnv := TavCanvas.Create(FMain);
  FCnv.Font.Color := Vec(byte(255),byte(255),byte(255),byte(255));
  BuildCanvas(FCnv);

  FCnv2 := TavCanvas.Create(FMain);
  FCnv2.Font.Color := Vec(byte(0),byte(0),byte(0),byte(255));
  BuildCanvas(FCnv2);
end;

procedure TfrmMain.cbOGLChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TfrmMain.cbDXChange(Sender: TObject);
begin
  Invalidate;
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

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  if FMain <> nil then FMain.InvalidateWindow;
end;

{$IfDef FPC}
procedure TfrmMain.EraseBackground(DC: HDC);
begin
  //inherited EraseBackground(DC);
end;
{$EndIf}
{$IfDef DCC}
procedure TfrmMain.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;
{$EndIf}

procedure TfrmMain.RenderScene;
begin
  if FMain = nil then Exit;

  if FMain.Bind then
  try
    FMain.States.DepthTest := True;

    FFrameBuffer.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FFrameBuffer.Select;

    //FMain.Clear(Vec(0.0,0.2,0.4,1.0), True, FMain.Projection.DepthRange.y, True);

    FMain.States.DepthTest := False;
    //FMain.States.Wireframe := GetTickCount64 mod 1000 < 500;
    FMain.States.Blending[0] := True;
    FMain.States.SetBlendFunctions(bfSrcAlpha, bfInvSrcAlpha);

    if GetTickCount mod 2000 < 1000 then
    begin
      FMain.Clear(Vec(0.0,0.0,0.0,1.0), True, FMain.Projection.DepthRange.y, True);
      FCnv.Draw(MatTranslate(Vec(10,10,0)), 1);
    end
    else
    begin
      FMain.Clear(Vec(1.0,1.0,1.0,1.0), True, FMain.Projection.DepthRange.y, True);
      FCnv2.Draw(MatTranslate(Vec(10,10,0)), 1);
    end;

    FFrameBuffer.BlitToWindow;
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

end.

