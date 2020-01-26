unit avCameraController;
{$I avConfig.inc}

interface

uses
  {$IfDef DCC}
  Windows,
  {$EndIf}
  Classes, SysUtils, avBase, avRes, mutils, avTypes;

type
  TavCameraType = (ctFirstPerson, ctThirdPerson);

  { TavCameraController }

  TavCameraController = class (TavObject)
  private
    FAutoInvalidate: Boolean;
    FEnabled: Boolean;
    FMain: TavMainRender;

    FCaptured: boolean;

    FRotating: boolean;
    FRotatingPt: TPoint;

    FMoving  : boolean;
    FMovingPt: TVec3;

    FZoomIn: boolean;
    FZoomOut: boolean;
    procedure CaptureWindow;
    procedure ReleaseCaptureWindow;
    procedure SetEnabled(AValue: Boolean);

    function  MouseInClient: Boolean;
  protected
    procedure EMUps       (var msg: TavMessage);          message EM_UPS;
    procedure EMMouseDown (var msg: TavMouseDownMessage); message EM_MOUSEDOWN;
    procedure EMMouseUp   (var msg: TavMouseUpMessage);   message EM_MOUSEUP;
    procedure EMMouseMove (var msg: TavMouseMessage);     message EM_MOUSEMOVE;
    procedure EMMouseWheel(var msg: TavMouseMessage);     message EM_MOUSEWHEEL;
  protected
    function CanRegister(target: TavObject): boolean; override;
  public
    WhellOnlyHovered: Boolean;

    CameraType : TavCameraType;
    RotateSens : TVec2;
    WellSens   : single;
    ZoomAtXBtn : boolean;
    ZoomSpeed  : single;
    CanMove    : boolean;
    CanRotate  : boolean;
    MovePlane  : TPlane;

    MouseBtn_Move  : Integer;
    MouseBtn_Rotate: Integer;

    property AutoInvalidate: Boolean read FAutoInvalidate write FAutoInvalidate;
    property Enabled: Boolean read FEnabled write SetEnabled;

    property Main: TavMainRender read FMain;
    constructor Create(AParent: TavObject); override;
  end;

  { TavCameraControllerUI }

  TavCameraControllerUI = class (TavObject)
  private
    FMain: TavMainRender;

    FCamPos: TVec2;
    FPixelToUnit: Single;

    FCaptured: Boolean;

    FMoving: Boolean;
    FMovingPt: TVec2;

    procedure CaptureWindow;
    procedure ReleaseCaptureWindow;
  protected
    procedure EMMouseDown (var msg: TavMouseDownMessage); message EM_MOUSEDOWN;
    procedure EMMouseUp   (var msg: TavMouseUpMessage);   message EM_MOUSEUP;
    procedure EMMouseMove (var msg: TavMouseMessage);     message EM_MOUSEMOVE;
    procedure EMMouseWheel(var msg: TavMouseMessage);     message EM_MOUSEWHEEL;
    function CanRegister(target: TavObject): boolean; override;
    procedure AfterRegister; override;
  public
    WellSens   : single;
    MouseBtn_Move  : Integer;

    property PixelToUnit: Single read FPixelToUnit;

    function CanvasMat: TMat3;
    function Space_WindowToCanvas(const APt: TVec2): TVec2;
    function Space_CanvasToWindow(const APt: TVec2): TVec2;

    procedure SetState(const ACamOffset: TVec2; const APixToUnit: Single);
  end;

implementation

uses Math, avPlatform;

{ TavCameraControllerUI }

procedure TavCameraControllerUI.CaptureWindow;
begin
  if IsValidWindow(FMain.Window) then
    avPlatform.CaptureWindow(FMain.Window);
end;

procedure TavCameraControllerUI.ReleaseCaptureWindow;
begin
  avPlatform.ReleaseCaptureWindow;
end;

procedure TavCameraControllerUI.EMMouseDown(var msg: TavMouseDownMessage);
begin
  if msg.button = MouseBtn_Move then
  begin
    CaptureWindow;
    FCaptured := true;
    FMoving := true;
    FMovingPt := Space_WindowToCanvas(Vec(msg.xPos, msg.yPos));
  end;
end;

procedure TavCameraControllerUI.EMMouseUp(var msg: TavMouseUpMessage);
begin
  if msg.button = MouseBtn_Move then
    FMoving:=false;

  if FCaptured and (not FMoving) then
  begin
    FCaptured:=false;
    ReleaseCaptureWindow;
  end;
end;

procedure TavCameraControllerUI.EMMouseMove(var msg: TavMouseMessage);
var
  dVec: TVec2;
begin
  if FMoving then
  begin
    dVec := FMovingPt - Space_WindowToCanvas(Vec(msg.xPos, msg.yPos));
    FCamPos := FCamPos + dVec*FPixelToUnit;
    FMain.InvalidateWindow;
  end;
end;

procedure TavCameraControllerUI.EMMouseWheel(var msg: TavMouseMessage);
var
  dVec: TVec2;
  zoomPt: TVec2;
begin
  zoomPt := Space_WindowToCanvas(Vec(msg.xPos, msg.yPos));
  if msg.wheelShift > 0 then
    FPixelToUnit := FPixelToUnit * WellSens
  else
    FPixelToUnit := FPixelToUnit / WellSens;
  dVec := zoomPt - Space_WindowToCanvas(Vec(msg.xPos, msg.yPos));
  FCamPos := FCamPos + dVec*FPixelToUnit;
  FMain.InvalidateWindow;
end;

function TavCameraControllerUI.CanRegister(target: TavObject): boolean;
begin
  Result := inherited CanRegister(target);
  Result := Result and (target is TavMainRender);
  FMain := TavMainRender(target);
end;

procedure TavCameraControllerUI.AfterRegister;
begin
  inherited AfterRegister;
  FPixelToUnit := 1;
  WellSens := 1.1;
  FCamPos := Vec(0,0);
end;

function TavCameraControllerUI.CanvasMat: TMat3;
begin
  Result := Mat3(Vec(FPixelToUnit, FPixelToUnit), 0, -FCamPos);
end;

function TavCameraControllerUI.Space_WindowToCanvas(const APt: TVec2): TVec2;
var v: TVec3;
begin
  v := Vec(APt, 1.0) * Inv(CanvasMat);
  Result := v.xy / v.z;
end;

function TavCameraControllerUI.Space_CanvasToWindow(const APt: TVec2): TVec2;
var v: TVec3;
begin
  v := Vec(APt, 1.0) * CanvasMat;
  Result := v.xy / v.z;
end;

procedure TavCameraControllerUI.SetState(const ACamOffset: TVec2; const APixToUnit: Single);
begin
  FCamPos := ACamOffset;
  FPixelToUnit := APixToUnit;
  FMain.InvalidateWindow;
end;

{ TavCameraController }

procedure TavCameraController.CaptureWindow;
begin
  if IsValidWindow(FMain.Window) then
    avPlatform.CaptureWindow(FMain.Window);
end;

procedure TavCameraController.ReleaseCaptureWindow;
begin
  avPlatform.ReleaseCaptureWindow;
end;

procedure TavCameraController.SetEnabled(AValue: Boolean);
begin
  if FEnabled = AValue then Exit;
  if FEnabled then
    UnregisterHandler(Self, FMain.Window)
  else
    RegisterHandler(Self, FMain.Window);
  FEnabled := AValue;
end;

function TavCameraController.MouseInClient: Boolean;
var cpt: TVec2;
    wndSize: TVec2i;
begin
  Result := False;
  cpt := GetCursorPos(FMain.Window, True, False);
  if cpt.x < 0 then Exit;
  if cpt.y < 0 then Exit;
  wndSize := FMain.WindowSize;
  if cpt.x >= wndSize.x then Exit;
  if cpt.y >= wndSize.y then Exit;
  Result := True;
end;

procedure TavCameraController.EMUps(var msg: TavMessage);
begin
  if WhellOnlyHovered and not MouseInClient then Exit;

  if FZoomIn  then Main.Camera.MoveDeep( msg.param*0.001*ZoomSpeed);
  if FZoomOut then Main.Camera.MoveDeep(-msg.param*0.001*ZoomSpeed);
end;

procedure TavCameraController.EMMouseDown(var msg: TavMouseDownMessage);
begin
  if msg.button = MouseBtn_Move then
  begin
    if CanMove then
    begin
      CaptureWindow;
      FCaptured:=true;
      FMoving:=true;
      Intersect(MovePlane, Main.Cursor.Ray, FMovingPt);
    end;
  end
  else
  if msg.button = MouseBtn_Rotate then
  begin
    If CanRotate Then
    begin
      CaptureWindow;
      FCaptured:=true;
      FRotating:=true;
      FRotatingPt.x:=msg.xPos;
      FRotatingPt.y:=msg.yPos;
    end;
  end
  else
  begin
    case msg.button of
      4: begin
           if ZoomAtXBtn then
           begin
             CaptureWindow;
             FCaptured:=true;
             FZoomIn:=true;
           end;
         end;
      5: begin
           if ZoomAtXBtn then
           begin
             CaptureWindow;
             FCaptured:=true;
             FZoomOut:=true;
           end;
         end;
    end;
  end;
end;

procedure TavCameraController.EMMouseUp(var msg: TavMouseUpMessage);
begin
  if msg.button = MouseBtn_Move then
    FMoving:=false
  else
  if msg.button = MouseBtn_Rotate then
    FRotating := false
  else
  case msg.button of
    4: FZoomIn:=false;
    5: FZoomOut:=false;
  end;

  if FCaptured and (not (FRotating or FZoomIn or FZoomOut or FMoving)) then
  begin
    FCaptured:=false;
    ReleaseCaptureWindow;
  end;
end;

procedure TavCameraController.EMMouseMove(var msg: TavMouseMessage);
var dpt: TPoint;
    dVec: TVec3;
begin
  if FRotating and FMoving then
  begin
    dpt.x:=msg.xPos-FRotatingPt.x;
    dpt.y:=msg.yPos-FRotatingPt.y;
    Main.Camera.MoveForward(dpt.y*0.1);
    FRotatingPt.x:=msg.xPos;
    FRotatingPt.y:=msg.yPos;
    Intersect(MovePlane, Main.Cursor.Ray, FMovingPt);

    if FAutoInvalidate then InvalidateWindow(Main.Window, False);
  end
  else
  begin
    if FRotating then
    begin
      dpt.x:=msg.xPos-FRotatingPt.x;
      dpt.y:=msg.yPos-FRotatingPt.y;
      case CameraType of
        ctFirstPerson: begin
                         Main.Camera.BeginUpdate;
                         Main.Camera.RotateEyeHorizontal(-dpt.x*RotateSens.x);
                         Main.Camera.RotateEyeVertical(-dpt.y*RotateSens.y);
                         Main.Camera.EndUpdate;
                       end;
        ctThirdPerson: begin
                         Main.Camera.BeginUpdate;
                         Main.Camera.RotateAtHorizontal(-dpt.x*RotateSens.x);
                         Main.Camera.RotateAtVertical(-dpt.y*RotateSens.y);
                         Main.Camera.EndUpdate;
                       end;
      end;
      FRotatingPt.x:=msg.xPos;
      FRotatingPt.y:=msg.yPos;

      if FAutoInvalidate then InvalidateWindow(Main.Window, False);
    end;

    if FMoving then
    begin
      Intersect(MovePlane, Main.Cursor.Ray, dVec);
      dVec:=FMovingPt - dVec;
      Main.Camera.BeginUpdate;
      Main.Camera.At:=Main.Camera.At + dVec;
      Main.Camera.Eye:=Main.Camera.Eye + dVec;
      Main.Camera.EndUpdate;

      if FAutoInvalidate then InvalidateWindow(Main.Window, False);
    end;
  end;
end;

procedure TavCameraController.EMMouseWheel(var msg: TavMouseMessage);
begin
  if WhellOnlyHovered and not MouseInClient then Exit;

  if CameraType=ctThirdPerson then
  begin
    Main.Camera.MoveDeep(Len(Main.Camera.At - Main.Camera.Eye) * 0.05 * Sign(msg.wheelShift));
//    Main.Camera.MoveDeep(msg.wheelShift*WellSens);
    if FAutoInvalidate then InvalidateWindow(Main.Window, False);
  end;
end;

function TavCameraController.CanRegister(target: TavObject): boolean;
begin
  Result := inherited CanRegister(target);
  Result := Result and (target is TavMainRender);
  FMain := TavMainRender(target);
end;

constructor TavCameraController.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  FAutoInvalidate := True;
  CameraType := ctThirdPerson;
  RotateSens.x := 0.003;
  RotateSens.y := 0.003;
  WellSens := 0.3;
  ZoomAtXBtn := True;
  ZoomSpeed := 4;
  CanMove := false;
  MovePlane := Plane(Vec(0.0, 1.0, 0.0), Vec(0.0, 0.0, 0.0));

  MouseBtn_Move := 1;
  MouseBtn_Rotate := 2;
end;

end.

