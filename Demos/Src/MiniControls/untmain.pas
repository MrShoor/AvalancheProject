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
  ExtCtrls, StdCtrls, Menus, avRes, avTypes, avTess, mutils,
  avMiniControls, avCanvas;

type

  { TMyPanel }

  TMyPanel = class (TavmCustomControl)
  protected
    procedure DoValidate; override;
  end;


  { TMyButton }

  TMyButton = class(TavmCustomButton)
  protected
    procedure DoValidate; override;
  end;

  { TMyEdit }

  TMyEdit = class(TavmCustomEdit)
  private
    FLastText: ITextLines;
    FMovedRow : Integer;
    FMovedChar: Integer;
  protected
    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); override;
    function CanAddChar: Boolean; override;
    procedure DoValidate; override;
  end;

  { TfrmMain }

  TfrmMain = class(TForm)
    {$IfDef DCC}
    ApplicationEvents: TApplicationEvents;
    {$EndIf}
    {$IfDef FPC}
    ApplicationProperties: TApplicationProperties;
    {$EndIf}
    procedure ApplicationPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FMain: TavMainRender;
    FFrameBuffer: TavFrameBuffer;

    FPanel: TMyPanel;

    FTiltTime: Single;
    procedure TiltControl(ASender: TObject);
  public
    procedure RenderScene;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  math_fx;

{ TMyPanel }

procedure TMyPanel.DoValidate;
begin
  inherited DoValidate;
  Canvas.Clear;
  Canvas.Brush.Color := Vec(0.5,0.5,0.5,1);
  Canvas.AddFill(Vec(0, 0), Size);
  Canvas.AddRectangle(Vec(0, 0), Size);
end;

{ TMyEdit }

procedure TMyEdit.Notify_MouseMove(const APt: TVec2; AShifts: TShifts);
var
  charidx, rowidx: Integer;
begin
  inherited Notify_MouseMove(APt, AShifts);
  if FLastText = nil then Exit;
  FLastText.SymbolAt(APt, rowidx, charidx);
  if (rowidx <> FMovedRow) or (charidx <> FMovedChar) then
  begin
    FMovedRow := rowidx;
    FMovedChar := charidx;
    Invalidate;
  end;
end;

function TMyEdit.CanAddChar: Boolean;
begin
  if FLastText = nil then Exit(True);
  if FLastText.LinesCount = 0 then Exit(True);
  Result := FLastText.LineSize(0).x + 20 < Size.x;
end;

procedure TMyEdit.DoValidate;
var charBounds: TRectF;
    x: Single;
begin
  inherited DoValidate;

  //draw background
  Canvas.Clear;
  Canvas.Brush.Color := Vec(1,1,1,1);
  Canvas.AddFill(Vec(0,0), Size);

  Canvas.Pen.Color := Vec(0,0,0,1);
  if Focused then
    Canvas.AddRectangle(Vec(0,0), Size);

  //draw text
  if Length(Text) > 0 then
  begin
    Canvas.Font.Color := Vec(0,0,0,1);
    with Canvas.TextBuilder do
    begin
      Align := laCenter;
      Write(string(Text));
      FLastText := Finish();
      FLastText.VAlign := 0.5;
      FLastText.BoundsX := Vec(0, Size.x);
      FLastText.BoundsY := Vec(0, Size.y);
      Canvas.AddText(FLastText);
    end;
  end;

  //draw caret
  if Focused and CaretVisible then
  begin
    if (FLastText = nil) or (FLastText.LinesCount = 0) then
    begin
      charBounds.min.x := Size.x * 0.5;
      charBounds.max.x := Size.x * 0.5;
      charBounds.min.y := Size.y * 0.5 - Canvas.Font.Size*0.5;
      charBounds.max.y := Size.y * 0.5 + Canvas.Font.Size*0.5;
    end
    else
      charBounds := FLastText.GetBounds(0, Length(Text), 0);
    Canvas.AddRectangle(charBounds.min, charBounds.max);
  end;

  //draw moved character highlight
  if (FLastText <> nil) and (FMovedRow >= 0) then
  begin
    charBounds := FLastText.GetBounds(FMovedRow, FMovedChar, 1);
    Canvas.Pen.Color := Vec(1,0,0,1);
    Canvas.AddRectangle(charBounds.min, charBounds.max);
  end;
end;

{ TMyButton }

procedure TMyButton.DoValidate;
var
  txt: ITextLines;
begin
  inherited DoValidate;
  Canvas.Clear;
  if Downed then
    Canvas.Brush.Color := Vec(0.8,1.0,0.6,1.0)
  else
  begin
    if not Moved then
      Canvas.Brush.Color := Vec(0.1,0.3,0.1,1.0)
    else
      Canvas.Brush.Color := Vec(0.4,0.7,0.3,1.0);
  end;
  Canvas.AddFill(Vec(0, 0), Size);

  if Length(Text) > 0 then
  begin
    Canvas.Font.Color := Vec(0,0,0,1);
    with Canvas.TextBuilder do
    begin
      Align := laCenter;
      WriteLn(Text);
      txt := Finish();
      txt.VAlign := 0.5;
      txt.BoundsX := Vec(0, Size.x);
      txt.BoundsY := Vec(0, Size.y);
      Canvas.AddText(txt);
    end;
  end;
end;

{$IfnDef notDCC}
  {$R *.dfm}
{$EndIf}

{$IfDef FPC}
  {$R *.lfm}
{$EndIf}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FMain := TavMainRender.Create(Nil);
  FMain.Window := Handle;
  FMain.Init3D(apiDX11);

  FFrameBuffer := Create_FrameBufferMultiSampled(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], 8, [true, false]);
  //FFrameBuffer := Create_FrameBuffer(FMain, [TTextureFormat.RGBA, TTextureFormat.D32f], [true, false]);

  FPanel := TMyPanel.Create(FMain);
  FPanel.Size := Vec(200, 300);
  FPanel.Pos := Vec(202, 252);
  FPanel.Origin := Vec(0.5, 0.5);

  with TMyButton.Create(FPanel) do
  begin
    OnClick := {$IfDef FPC}@{$EndIf}TiltControl;
    Pos := Vec(20,20);
    Size := Vec(161, 30);
    Text := 'Button1';
  end;

  with TMyEdit.Create(FPanel) do
  begin
    MaxTextLength := 12;
    Pos := Vec(20, 70);
    Size := Vec(161, 30);
    Text := 'Edit1';
  end;
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

procedure TfrmMain.FormPaint(Sender: TObject);
begin
  RenderScene;
end;

procedure TfrmMain.TiltControl(ASender: TObject);
begin
  FTiltTime := FMain.Time;
end;

procedure TfrmMain.RenderScene;
var dt, tn: Single;
begin
  if FMain = nil then Exit;

  dt := FMain.Time - FTiltTime;
  tn := dt / 0.75;
  if (tn < 0) or (tn > 1) then
    FPanel.Angle := 0
  else
    FPanel.Angle := sin(sqrt(dt*1000+50)) * FallOff_Exp(tn) * 0.25;
  FPanel.Pos := Vec(ClientWidth*0.5, ClientHeight*0.5);

  if FMain.Bind then
  try
    FMain.States.DepthTest := True;

    FFrameBuffer.FrameRect := RectI(0, 0, ClientWidth, ClientHeight);
    FFrameBuffer.Select;

    FMain.Clear(Vec(0.0,0.2,0.4,1.0), True, FMain.Projection.DepthRange.y, True);

    FPanel.Draw();

    FFrameBuffer.BlitToWindow;
    FMain.Present;
  finally
    FMain.Unbind;
  end;
end;

end.

