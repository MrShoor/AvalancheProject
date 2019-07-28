unit avMiniControls;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils,
  intfUtils,
  avBase, avRes, avTypes, mutils,
  avCanvas;

type
  TavmInputConnector = class;

  TKeyEventEx = record
    shifts     : TShifts;
    Sys        : boolean;
    Dead       : boolean;
    Repeated   : boolean;
    RepeatCount: Integer;
  end;

  IUPSEvent = interface
  ['{12101264-17AA-44AC-A7E3-C921F899A2BC}']
    procedure UpdateStates();
  end;

  { TavmBaseControl }

  TavmBaseControl = class (TavMainRenderChild)
  private
    FAngle: Single;
    FOrigin: TVec2;
    FPos: TVec2;
    FSize: TVec2;

    FValid: Boolean;
    FIteratorIdx: Integer;

    FInputConnector: IWeakRef;
    FVisible: Boolean;
    FAllowFocus: Boolean;

    FUPSSubscriber: IUPSEvent;

    function GetAbsAngle: Single;
    function GetAbsPos: TVec2;
    procedure SetAbsAngle(const AValue: Single);
    procedure SetAbsPos(const AValue: TVec2);
  protected
    procedure SetAngle(const AValue: Single); virtual;
    procedure SetOrigin(const AValue: TVec2); virtual;
    procedure SetPos(const AValue: TVec2); virtual;
    procedure SetSize(const AValue: TVec2); virtual;
    procedure SetVisible(const AValue: Boolean); virtual;
  protected
    FDragThreshold: Single;
    FDragDowned: array [0..2] of Boolean;
    FDragStarted: array [0..2] of Boolean;
    FDragDownedCoord: array[0..2] of TVec2;
    procedure AfterRegister; override;
  protected
    procedure Notify_ParentSizeChanged; virtual;
    procedure Notify_ParentOriginChanged; virtual;
    procedure Notify_ParentPosChanged; virtual;
    procedure Notify_ParentAngleChanged; virtual;

    procedure DrawControl(const AMat: TMat3); virtual;
    procedure DrawRecursive(const AMat: TMat3); virtual;
  protected
    procedure Notify_MouseEnter; virtual;
    procedure Notify_MouseLeave; virtual;
    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); virtual;
    procedure Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts); virtual;
    procedure Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts); virtual;
    procedure Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts); virtual;
    procedure Notify_MouseDblClick(ABtn: Integer; const APt: TVec2; AShifts: TShifts); virtual;

    procedure Notify_DragStart(ABtn: Integer; const APt: TVec2; AShifts: TShifts); virtual;
    procedure Notify_DragMove (ABtn: Integer; const APt: TVec2; AShifts: TShifts); virtual;
    procedure Notify_DragStop (ABtn: Integer; const APt: TVec2; AShifts: TShifts); virtual;

    procedure Notify_FocusSet; virtual;
    procedure Notify_FocusLost; virtual;
    procedure Notify_KeyDown(AKey: Word; const Ex: TKeyEventEx); virtual;
    procedure Notify_Char(AChar: WideChar; const Ex: TKeyEventEx); virtual;
    procedure Notify_KeyUp(AKey: Word; const Ex: TKeyEventEx); virtual;

    procedure RedirectToParent_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts);
  protected
    procedure UPSSubscribe;
    procedure UPSUnSubscribe;
    procedure Validate;
    procedure DoValidate; virtual;
    procedure OnUPS; virtual;
  protected
    function  Space_ParentToLocal(const APt: TVec2): TVec2;
    function  Space_LocalToRootControl(const APt: TVec2): TVec2;
    function  Space_LocalToParent(const APt: TVec2): TVec2;

    procedure HitTestRecursive(const ALocalPt: TVec2; var AControl: TavmBaseControl);
    procedure HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl); virtual;
  public
    procedure Invalidate;
    procedure BringToFront;
    function LocalPtInArea(const Pt: TVec2): Boolean;
    function HitTest(const V: TVec2; ARecursive: Boolean): TavmBaseControl;
    function RootControl: TavmBaseControl;

    function  ChildControlsCount: Integer;
    procedure ResetIterator(ToLast: Boolean = false);
    function  NextChildControl(out AChild: TavmBaseControl): Boolean;
    function  PrevChildControl(out AChild: TavmBaseControl): Boolean;

    function  InputConnector: TavmInputConnector;

    property Size  : TVec2  read FSize   write SetSize;
    property Origin: TVec2  read FOrigin write SetOrigin;
    property Angle : Single read FAngle  write SetAngle;
    property Pos   : TVec2  read FPos    write SetPos;

    property DragThreshold : Single read FDragThreshold write FDragThreshold;

    property AbsAngle: Single read GetAbsAngle write SetAbsAngle;
    property AbsPos: TVec2 read GetAbsPos write SetAbsPos;

    property Visible: Boolean read FVisible write SetVisible;
    property AllowFocus: Boolean read FAllowFocus write FAllowFocus;

    function Transform   : TMat3;
    function TransformInv: TMat3;
    function AbsTransform   : TMat3;
    function AbsTransformInv: TMat3;

    procedure Draw();

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  { TavmInputConnector }

  TavmInputConnector = class (TavMainRenderChild)
  private
    FRoot: TavmBaseControl;

    FCaptured: IWeakRef;// TavmBaseControl;
    FFocused: IWeakRef; // TavmBaseControl;
    FMoved: IWeakRef; //TavmBaseControl;

    FUPSPublisher: IUPSEvent;

    function GetCaptured: TavmBaseControl;
    function GetFocused: TavmBaseControl;
    function GetMoved: TavmBaseControl;
    procedure SetCaptured(const AValue: TavmBaseControl);
    procedure SetFocused(const AValue: TavmBaseControl);
  private
    function UpdateMovedState(const Pt: TVec2i): TavmBaseControl;

    procedure EMMouseDown    (var AMsg: TavMouseDownMessage); message EM_MOUSEDOWN;
    procedure EMMouseUp      (var AMsg: TavMouseUpMessage);   message EM_MOUSEUP;
    procedure EMMouseDblClick(var AMsg: TavMouseDblClick);    message EM_MOUSEDBLCLICK;
    procedure EMMouseMove    (var AMsg: TavMouseMessage);     message EM_MOUSEMOVE;
    procedure EMMouseWheel   (var AMsg: TavMouseMessage);     message EM_MOUSEWHEEL;
  private
    procedure EMKeyDown (var AMsg: TavKeyDownMessage); message EM_KEYDOWN;
    procedure EMKeyUp   (var AMsg: TavKeyUpMessage);   message EM_KEYUP;
    procedure EMChar    (var AMsg: TavCharMessage);    message EM_CHAR;
  private
    procedure EMUps (var AMsg: TavMessage); message EM_UPS;
  public
    procedure SubscribeToUPS(const AEventHandler : IUPSEvent);

    procedure SetRootControl(const AControl: TavmBaseControl);
    property RootControl: TavmBaseControl read FRoot;

    property Captured: TavmBaseControl read GetCaptured write SetCaptured;
    property Focused : TavmBaseControl read GetFocused write SetFocused;
    property Moved   : TavmBaseControl read GetMoved;

    procedure AfterConstruction; override;
  end;

  { TavmCustomControl }

  TavmCustomControl = class (TavmBaseControl)
  private
    FCanvas : TavCanvas;
    FMoved  : Boolean;
    FFocused: Boolean;
  protected
    FInvalidateOnMove : Boolean;
    FInvalidateOnFocus: Boolean;
    procedure Notify_MouseEnter; override;
    procedure Notify_MouseLeave; override;
    procedure Notify_FocusSet; override;
    procedure Notify_FocusLost; override;
  protected
    procedure AfterRegister; override;
    procedure DrawControl(const AMat: TMat3); override;
    procedure HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl); override;
  protected
    property Canvas : TavCanvas read FCanvas;
    property Moved  : Boolean   read FMoved;
    property Focused: Boolean   read FFocused;
  public
    procedure AfterConstruction; override;
  end;

  { TavmCustomButton }

  TavmCustomButton = class (TavmCustomControl)
  private
    FDownedInternal: Boolean;
    FDowned : Boolean;
    FText   : string;
    FOnClick: TNotifyEvent;
  protected
    procedure Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;

    procedure SetText(const AValue: string); virtual;
    procedure DoOnClick; virtual;
  protected
    property Downed: Boolean read FDowned;
  public
    property Text: string read FText write SetText;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  { TavmCustomCheckbox }

  TavmCustomCheckbox = class (TavmCustomButton)
  private
    FChecked: Boolean;
    FOnCheck: TNotifyEvent;
    procedure SetChecked(const AValue: Boolean);
  protected
    procedure DoOnClick; override;
    procedure Notify_MouseDblClick(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
  public
    property Checked: Boolean read FChecked write SetChecked;
    property OnCheck: TNotifyEvent read FOnCheck write FOnCheck;
  end;

  { TavmCustomScrollBar }

  TavmCustomScrollBar = class (TavmCustomControl)
  private
    FIsVertical: Boolean;
    FOnScroll: TNotifyEvent;
    FRange: Integer;
    FViewportPos: Integer;
    FViewportWidth: Integer;

    FCapturedScrollBar: Boolean;
    FCaptureOffset: Single;

    procedure SetIsVertical(AValue: Boolean);
    procedure SetRange(AValue: Integer);
    procedure SetViewportPos(AValue: Integer);
    procedure SetViewportWidth(AValue: Integer);
  protected
    procedure Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts); override;
  protected
    function BarRect(): TRectF;
  public
    property Range: Integer read FRange write SetRange;
    property ViewportWidth: Integer read FViewportWidth write SetViewportWidth;
    property ViewportPos: Integer read FViewportPos write SetViewportPos;
    property IsVertical: Boolean read FIsVertical write SetIsVertical;

    procedure Scroll(AOffset: Integer);

    property OnScroll: TNotifyEvent read FOnScroll write FOnScroll;
  end;
  TavmCustomScrollBarClass = class of TavmCustomScrollBar;

  { TavmCustomEdit }

  TavmCustomEdit = class (TavmCustomControl)
  private
    FText: UnicodeString;
    FMaxTextLength: Integer;

    FCaretStartTime: Int64;
    FCaretState: Integer;
    FCaretInvalidateInterval: Integer;

    FOnChanged: TNotifyEvent;
    FOnComplete: TNotifyEvent;
    procedure SetText(const AValue: UnicodeString);
  protected
    function CanAddChar: Boolean; virtual;
    procedure Notify_FocusSet; override;
    procedure Notify_FocusLost; override;
    procedure Notify_KeyDown(AKey: Word; const Ex: TKeyEventEx); override;
    procedure Notify_Char(AChar: WideChar; const Ex: TKeyEventEx); override;
    procedure OnUPS; override;

    function CaretVisible: Boolean;
    property CaretInvalidateInterval: Integer read FCaretInvalidateInterval write FCaretInvalidateInterval;
  public
    property MaxTextLength: Integer read FMaxTextLength write FMaxTextLength;
    property Text: UnicodeString read FText write SetText;

    property OnChanged : TNotifyEvent read FOnChanged  write FOnChanged;
    property OnComplete: TNotifyEvent read FOnComplete write FOnComplete;

    procedure AfterConstruction; override;
  end;

  { TavmCustomScrollContainer }

  TavmCustomScrollContainer = class (TavmCustomControl)
  private
    FVScroll: TavmCustomScrollBarClass;
    FWorkArea: TVec2;
    FWorkOrigin: TVec2;
    FCapturedPt: TVec2;

    procedure SetWorkArea(AValue: TVec2);
    procedure SetWorkOrigin(AValue: TVec2);
  protected
    //procedure SetAngle(const AValue: Single); override;
    //procedure SetOrigin(const AValue: TVec2); override;
    procedure SetSize(const AValue: TVec2); override;
    procedure SetPos(const AValue: TVec2); override;
  protected
    procedure Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseMove(const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts); override;
    procedure Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts); override;

    procedure DrawRecursive(const AMat: TMat3); override;
  protected
    function ArrangeChilds(const AVertical: Boolean; const ABorder: Single): Single;
  public
    procedure SetVScrollbar(const AClass: TavmCustomScrollBarClass);

    property WorkArea  : TVec2 read FWorkArea   write SetWorkArea;
    property WorkOrigin: TVec2 read FWorkOrigin write SetWorkOrigin;
  end;

implementation

uses
  Math;

type
  TEmptyMethod = procedure () of object;

  { TUPSSubscriber }

  TUPSSubscriber = class(TWeakedInterfacedObject, IUPSEvent)
  private
    FEvent: TEmptyMethod;
    procedure UpdateStates();
  public
    constructor Create(const AEvent: TEmptyMethod);
  end;

  { TUPSPublisher }

  TUPSPublisher = class(TPublisherBase, IUPSEvent)
  private
    procedure UpdateStates();
  end;

{ TavmCustomScrollContainer }

procedure TavmCustomScrollContainer.SetWorkArea(AValue: TVec2);
begin
  if FWorkArea=AValue then Exit;
  FWorkArea:=AValue;
end;

procedure TavmCustomScrollContainer.SetWorkOrigin(AValue: TVec2);
begin
  if FWorkOrigin=AValue then Exit;
  FWorkOrigin:=AValue;
end;

procedure TavmCustomScrollContainer.SetSize(const AValue: TVec2);
begin
  inherited SetSize(AValue);
  Pos := Pos;
end;

procedure TavmCustomScrollContainer.SetPos(const AValue: TVec2);
var newPos: TVec2;
begin
  newPos := Max(AValue, WorkOrigin+WorkArea-Size);
  newPos := Min(newPos, Vec(0,0));
  inherited SetPos(newPos);
end;

procedure TavmCustomScrollContainer.Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_MouseDown(ABtn, APt, AShifts);
  if ABtn = 1 then
  begin
    FCapturedPt := APt;
    InputConnector.Captured := Self;
  end;
end;

procedure TavmCustomScrollContainer.Notify_MouseMove(const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_MouseMove(APt, AShifts);
  if InputConnector.Captured = Self then
    Pos := Pos + (APt - FCapturedPt);
end;

procedure TavmCustomScrollContainer.Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_MouseUp(ABtn, APt, AShifts);
end;

procedure TavmCustomScrollContainer.Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts);
begin
  inherited Notify_MouseWheel(APt, AWheelShift, AShifts);
  Pos := Pos + Vec(AWheelShift, AWheelShift)*120;
end;

procedure TavmCustomScrollContainer.DrawRecursive(const AMat: TMat3);
  function GetControlScissor: TRectI;
  var pts: array [0..3] of TVec3;
      i: Integer;
  begin
    pts[0] := Vec(WorkOrigin.x, WorkOrigin.y, 1.0) * AMat;
    pts[1] := Vec(WorkOrigin.x, WorkOrigin.y+WorkArea.y, 1.0) * AMat;
    pts[2] := Vec(WorkOrigin.x+WorkArea.x, WorkOrigin.y, 1.0) * AMat;
    pts[3] := Vec(WorkOrigin.x+WorkArea.x, WorkOrigin.y+WorkArea.y, 1.0) * AMat;
    Result.min.x := HUGE;
    Result.min.y := HUGE;
    Result.max.x := -HUGE;
    Result.max.y := -HUGE;
    for i := 0 to 3 do
    begin
      Result.min := Round( min(pts[i].xy, Result.min) );
      Result.max := Round( max(pts[i].xy, Result.max) );
    end;
  end;
var oldScissor: TRectI;
    oldTest: Boolean;
begin
  oldScissor := Main.States.Scissor;
  oldTest := Main.States.GetScissorTest;
  try
    Main.States.SetScissorTest(True);
    Main.States.Scissor := GetControlScissor();
    inherited DrawRecursive(AMat);
  finally
    Main.States.Scissor := oldScissor;
    Main.States.SetScissorTest(oldTest);
  end;
end;

function TavmCustomScrollContainer.ArrangeChilds(const AVertical: Boolean; const ABorder: Single): Single;
  function AutoFlip(const v: TVec2): TVec2;
  begin
    if AVertical then Result := v else Result := Vec(v.y, v.x);
  end;
var s: TVec2;
    p: TVec2;
    ch: TavmBaseControl;
begin
  s := AutoFlip(Size);
  s := Max(s - Vec(ABorder, ABorder), Vec(0,0));

  p.x := ABorder;
  p.y := 0;
  ResetIterator();
  while NextChildControl(ch) do
  begin
    p.y := p.y + ABorder;
    ch.Pos := AutoFlip(p);
    ch.Size := AutoFlip( Vec(s.x - ABorder, AutoFlip(ch.Size).y) );
    p.y := p.y + AutoFlip(ch.Size).y;
  end;
  Result := p.y + ABorder;
end;

procedure TavmCustomScrollContainer.SetVScrollbar(const AClass: TavmCustomScrollBarClass);
begin

end;

{ TavmCustomCheckbox }

procedure TavmCustomCheckbox.SetChecked(const AValue: Boolean);
begin
  if FChecked = AValue then Exit;
  FChecked := AValue;
  Invalidate;
  if Assigned(FOnCheck) then
    FOnCheck(Self);
end;

procedure TavmCustomCheckbox.DoOnClick;
begin
  inherited DoOnClick;
  Checked := not Checked;
end;

procedure TavmCustomCheckbox.Notify_MouseDblClick(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_MouseDblClick(ABtn, APt, AShifts);
  if ABtn = 1 then
    DoOnClick;
end;

{ TavmCustomScrollBar }

procedure TavmCustomScrollBar.SetRange(AValue: Integer);
begin
  AValue := Max(AValue, 1);
  if FRange = AValue then Exit;
  FRange := AValue;
  if FViewportWidth > Range then
    FViewportWidth := Range;
  if FViewportPos + FViewportWidth > Range then
    FViewportPos := Range - FViewportWidth;
  if Assigned(FOnScroll) then FOnScroll(Self);
  Invalidate;
end;

procedure TavmCustomScrollBar.SetIsVertical(AValue: Boolean);
begin
  if FIsVertical = AValue then Exit;
  FIsVertical := AValue;
  Invalidate;
end;

procedure TavmCustomScrollBar.SetViewportPos(AValue: Integer);
begin
  AValue := clamp(AValue, 0, Range - FViewportWidth);
  if FViewportPos = AValue then Exit;
  FViewportPos := AValue;
  if Assigned(FOnScroll) then FOnScroll(Self);
  Invalidate;
end;

procedure TavmCustomScrollBar.SetViewportWidth(AValue: Integer);
begin
  AValue := Clamp(AValue, 1, Range);
  if FViewportWidth = AValue then Exit;
  FViewportWidth := AValue;
  if (FViewportPos + FViewportWidth) > Range then
    FViewportPos := Range - FViewportWidth;
  if Assigned(FOnScroll) then FOnScroll(Self);
  Invalidate;
end;

procedure TavmCustomScrollBar.Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
var barRct: TRectF;
begin
  inherited Notify_MouseDown(ABtn, APt, AShifts);
  if ABtn = 1 then
  begin
    barRct := BarRect();
    if barRct.PtInRect(APt) then
    begin
      if IsVertical then
        FCaptureOffset := barRct.Center.y - APt.y
      else
        FCaptureOffset := barRct.Center.x - APt.x;
      FCapturedScrollBar := True;
      InputConnector.Captured := Self;
    end
    else
    begin
      if IsVertical then
      begin
        if APt.y >= barRct.max.y then
          Scroll(ViewportWidth)
        else
          if APt.y < barRct.min.y then
            Scroll(-ViewportWidth);
      end
      else
      begin
        if APt.x >= barRct.max.x then
          Scroll(ViewportWidth)
        else
          if APt.x < barRct.min.x then
            Scroll(-ViewportWidth);
      end;
    end;

  end;
end;

procedure TavmCustomScrollBar.Notify_MouseMove(const APt: TVec2; AShifts: TShifts);
var barRct: TRectF;
    bounds: TVec2;
    midpos: Single;
    t: Single;
begin
  inherited Notify_MouseMove(APt, AShifts);
  if FCapturedScrollBar then
  begin
    barRct := BarRect();

    if IsVertical then
    begin
      bounds := Vec(barRct.Size.y * 0.5, Size.y - barRct.Size.y * 0.5);
      midpos := APt.y + FCaptureOffset;
    end
    else
    begin
      bounds := Vec(barRct.Size.x * 0.5, Size.x - barRct.Size.x * 0.5);
      midpos := APt.x + FCaptureOffset;
    end;
    if bounds.y <= bounds.x then Exit;
    t := (midpos - bounds.x) / (bounds.y - bounds.x);
    ViewportPos := Round( (Range - ViewportWidth) * t );
  end;
end;

procedure TavmCustomScrollBar.Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  inherited Notify_MouseUp(ABtn, APt, AShifts);
  if (ABtn = 1) and FCapturedScrollBar then
  begin
    InputConnector.Captured := nil;
    FCapturedScrollBar := False;
  end;
end;

procedure TavmCustomScrollBar.Notify_MouseWheel(const APt: TVec2;
  AWheelShift: Integer; AShifts: TShifts);
begin
  //inherited Notify_MouseWheel(APt, AShifts);
  Scroll(-AWheelShift);
end;

function TavmCustomScrollBar.BarRect: TRectF;
var control_size: Single;
    bounds: TVec2;
    kWidth, kPos: Single;
begin
  if IsVertical then
    control_size := Size.y
  else
    control_size := Size.x;

  kWidth := clamp(ViewportWidth / Range, 0.0, 1.0);
  kPos   := clamp(ViewportPos / Range, 0.0, 1.0);

  bounds.x := Lerp(0, control_size, kPos);
  bounds.y := bounds.x + kWidth*control_size;

  bounds.x := clamp(bounds.x, 0, control_size);
  bounds.y := clamp(bounds.y, 0, control_size);

  if IsVertical then
    Result := RectF(0, bounds.x, Size.x, bounds.y)
  else
    Result := RectF(bounds.x, 0, bounds.y, Size.y);
end;

procedure TavmCustomScrollBar.Scroll(AOffset: Integer);
begin
  ViewportPos := ViewportPos + AOffset;
end;

{ TUPSSubscriber }

procedure TUPSSubscriber.UpdateStates;
begin
  FEvent();
end;

constructor TUPSSubscriber.Create(const AEvent: TEmptyMethod);
begin
  Assert(Assigned(AEvent));
  FEvent := AEvent;
end;

{ TUPSPublisher }

procedure TUPSPublisher.UpdateStates;
var
  subs: TSubsList;
  i: Integer;
  ev: IUPSEvent;
begin
  subs := GetSubsList;
  for i := 0 to Length(subs) - 1 do
    if Supports(subs[i], IUPSEvent, ev) then
      ev.UpdateStates();
end;

{ TavmCustomEdit }

procedure TavmCustomEdit.SetText(const AValue: UnicodeString);
begin
  if FText = AValue then Exit;
  FText := AValue;
  Invalidate;
  if Assigned(FOnChanged) then FOnChanged(Self);
end;

function TavmCustomEdit.CanAddChar: Boolean;
begin
  Result := Length(FText) < FMaxTextLength;
end;

procedure TavmCustomEdit.Notify_FocusSet;
begin
  inherited Notify_FocusSet;
  FCaretStartTime := Main.Time64;
  UPSSubscribe;
end;

procedure TavmCustomEdit.Notify_FocusLost;
begin
  inherited Notify_FocusLost;
  if Assigned(FOnComplete) then FOnComplete(Self);
  UPSUnSubscribe;
end;

procedure TavmCustomEdit.Notify_KeyDown(AKey: Word; const Ex: TKeyEventEx);
var ustr: UnicodeString;
begin
  inherited Notify_KeyDown(AKey, Ex);
  if AKey = 8 then //backspace
  begin
    ustr := Text;
    if Length(ustr) = 0 then Exit;
    Delete(ustr, Length(ustr), 1);
    Text := ustr;
  end;
  if AKey = 13 then //enter
    InputConnector.Focused := nil;
end;

procedure TavmCustomEdit.Notify_Char(AChar: WideChar; const Ex: TKeyEventEx);
begin
  inherited Notify_Char(AChar, Ex);
  if Ord(AChar) < 32 then Exit;

  FCaretStartTime := Main.Time64;

  if not CanAddChar then Exit;
  Text := Text + AChar;
end;

procedure TavmCustomEdit.OnUPS;
var newCaretState: Integer;
begin
  newCaretState := (Main.Time64 - FCaretStartTime) div FCaretInvalidateInterval;
  if newCaretState <> FCaretState then
  begin
    FCaretState := newCaretState;
    Invalidate;
  end;
end;

function TavmCustomEdit.CaretVisible: Boolean;
begin
  Result := FCaretState mod 2 = 0;
end;

procedure TavmCustomEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  FMaxTextLength := 100;
  FCaretInvalidateInterval := 500;
end;

{ TavmCustomControl }

procedure TavmCustomControl.Notify_MouseEnter;
begin
  inherited Notify_MouseEnter;
  FMoved := True;
  if FInvalidateOnMove then Invalidate;
end;

procedure TavmCustomControl.Notify_MouseLeave;
begin
  inherited Notify_MouseLeave;
  FMoved := False;
  if FInvalidateOnMove then Invalidate;
end;

procedure TavmCustomControl.Notify_FocusSet;
begin
  inherited Notify_FocusSet;
  FFocused := True;
  if FInvalidateOnFocus then Invalidate;
end;

procedure TavmCustomControl.Notify_FocusLost;
begin
  inherited Notify_FocusLost;
  FFocused := False;
  if FInvalidateOnFocus then Invalidate;
end;

procedure TavmCustomControl.AfterRegister;
begin
  inherited AfterRegister;
  FCanvas := TavCanvas.Create(Self);
end;

procedure TavmCustomControl.DrawControl(const AMat: TMat3);
begin
  inherited DrawControl(AMat);
  FCanvas.Draw(AMat);
end;

procedure TavmCustomControl.HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl);
begin
  AControl := Self;
end;

procedure TavmCustomControl.AfterConstruction;
begin
  inherited AfterConstruction;
  FInvalidateOnMove := True;
  FInvalidateOnFocus := True;
end;

{ TavmCustomButton }

procedure TavmCustomButton.SetText(const AValue: string);
begin
  if FText = AValue then Exit;
  FText := AValue;
  Invalidate;
end;

procedure TavmCustomButton.Notify_MouseDown(ABtn: Integer; const APt: TVec2;
  AShifts: TShifts);
begin
  if ABtn = 1 then
  begin
    FDownedInternal := True;
    FDowned := True;
    InputConnector.Captured := Self;
    Invalidate;
  end;
end;

procedure TavmCustomButton.Notify_MouseMove(const APt: TVec2; AShifts: TShifts);
var newDowned: Boolean;
begin
  if FDownedInternal then
  begin
    newDowned := LocalPtInArea(APt);
    if FDowned <> newDowned then
    begin
      FDowned := newDowned;
      Invalidate;
    end;
  end;
end;

procedure TavmCustomButton.Notify_MouseUp(ABtn: Integer; const APt: TVec2;
  AShifts: TShifts);
begin
  if (ABtn = 1) and FDownedInternal then
  begin
    FDownedInternal := False;
    FDowned := False;
    Invalidate;
    if LocalPtInArea(APt) then
      DoOnClick;
  end;
end;

procedure TavmCustomButton.DoOnClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

{ TavmInputConnector }

function TavmInputConnector.GetCaptured: TavmBaseControl;
begin
  if FCaptured = nil then Exit(nil);
  Result := TavmBaseControl(FCaptured.Obj);
  if Result = nil then FCaptured := nil;
end;

function TavmInputConnector.GetFocused: TavmBaseControl;
begin
  if FFocused = nil then Exit(nil);
  Result := TavmBaseControl(FFocused.Obj);
  if Result = nil then FFocused := nil;
end;

function TavmInputConnector.GetMoved: TavmBaseControl;
begin
  if FMoved = nil then Exit(nil);
  Result := TavmBaseControl(FMoved.Obj);
  if Result = nil then FMoved := nil;
end;

procedure TavmInputConnector.SetCaptured(const AValue: TavmBaseControl);
begin
  if Captured = AValue then Exit;
  if AValue = nil then
    FCaptured := nil
  else
    FCaptured := AValue.WeakRef;
end;

procedure TavmInputConnector.SetFocused(const AValue: TavmBaseControl);
var f: TavmBaseControl;
begin
  f := Focused;
  if f = AValue then Exit;
  if f <> nil then
    f.Notify_FocusLost;
  if AValue = nil then
    FFocused := nil
  else
  begin
    FFocused := AValue.WeakRef;
    AValue.Notify_FocusSet;
  end;
end;

function TavmInputConnector.UpdateMovedState(const Pt: TVec2i): TavmBaseControl;
var
  m: TavmBaseControl;
begin
  Result := Captured;
  if Result = nil then
    Result := FRoot.HitTest(Pt, true);

  m := Moved;
  if m <> Result then
  begin
    if m <> nil then
      m.Notify_MouseLeave;
    if Result = nil then
      FMoved := nil
    else
    begin
      FMoved := Result.WeakRef;
      Result.Notify_MouseEnter;
    end;
  end;
end;

procedure TavmInputConnector.EMMouseDown(var AMsg: TavMouseDownMessage);
var v: TVec2i;
    m: TavmBaseControl;
begin
  if FRoot = nil then Exit;
  v := Vec(AMsg.xPos, AMsg.yPos);
  m := UpdateMovedState(v);
  if m <> nil then
  begin
    m.Notify_MouseDown(AMsg.button, v * m.AbsTransformInv, AMsg.shifts);
    if m.AllowFocus then
      SetFocused(m);
  end
  else
    SetFocused(nil);
end;

procedure TavmInputConnector.EMMouseUp(var AMsg: TavMouseUpMessage);
var v: TVec2i;
    m: TavmBaseControl;
begin
  if FRoot = nil then Exit;
  v := Vec(AMsg.xPos, AMsg.yPos);
  m := UpdateMovedState(v);
  if m <> nil then m.Notify_MouseUp(AMsg.button, v * m.AbsTransformInv, AMsg.shifts);

  Captured := nil;
end;

procedure TavmInputConnector.EMMouseDblClick(var AMsg: TavMouseDblClick);
var v: TVec2i;
    m: TavmBaseControl;
begin
  if FRoot = nil then Exit;
  v := Vec(AMsg.xPos, AMsg.yPos);
  m := UpdateMovedState(v);
  if m <> nil then m.Notify_MouseDblClick(AMsg.button, v * m.AbsTransformInv, AMsg.shifts);
end;

procedure TavmInputConnector.EMMouseMove(var AMsg: TavMouseMessage);
var v: TVec2i;
    m: TavmBaseControl;
begin
  if FRoot = nil then Exit;
  v := Vec(AMsg.xPos, AMsg.yPos);
  m := UpdateMovedState(v);
  if m <> nil then m.Notify_MouseMove(v * m.AbsTransformInv, AMsg.shifts);
end;

procedure TavmInputConnector.EMMouseWheel(var AMsg: TavMouseMessage);
var v: TVec2i;
    m: TavmBaseControl;
begin
  if FRoot = nil then Exit;
  v := Vec(AMsg.xPos, AMsg.yPos);
  m := UpdateMovedState(v);
  if m <> nil then m.Notify_MouseWheel(v * m.AbsTransformInv, AMsg.wheelShift, AMsg.shifts);
end;

procedure TavmInputConnector.EMKeyDown(var AMsg: TavKeyDownMessage);
var f: TavmBaseControl;
    ex: TKeyEventEx;
begin
  if FRoot = nil then Exit;
  f := Focused;
  if f = nil then Exit;
  ex.shifts := AMsg.shifts;
  ex.Dead := AMsg.Dead;
  ex.Sys := AMsg.Sys;
  ex.Repeated := AMsg.Repeated;
  ex.RepeatCount := AMsg.RepeatCount;
  f.Notify_KeyDown(AMsg.Key, ex);
end;

procedure TavmInputConnector.EMKeyUp(var AMsg: TavKeyUpMessage);
var
  f: TavmBaseControl;
  ex: TKeyEventEx;
begin
  if FRoot = nil then Exit;
  f := Focused;
  if f = nil then Exit;
  ex.shifts := AMsg.shifts;
  ex.Dead := AMsg.Dead;
  ex.Sys := AMsg.Sys;
  ex.Repeated := AMsg.Repeated;
  ex.RepeatCount := AMsg.RepeatCount;
  f.Notify_KeyUp(AMsg.Key, ex);
end;

procedure TavmInputConnector.EMChar(var AMsg: TavCharMessage);
var
  f: TavmBaseControl;
  ex: TKeyEventEx;
begin
  if FRoot = nil then Exit;
  f := Focused;
  if f = nil then Exit;
  ex.shifts := AMsg.shifts;
  ex.Dead := AMsg.Dead;
  ex.Sys := AMsg.Sys;
  ex.Repeated := AMsg.Repeated;
  ex.RepeatCount := AMsg.RepeatCount;
  f.Notify_Char(AMsg.Char, ex);
end;

procedure TavmInputConnector.EMUps(var AMsg: TavMessage);
var i: Integer;
begin
  for i := 0 to AMsg.param - 1 do
    FUPSPublisher.UpdateStates();
end;

procedure TavmInputConnector.SubscribeToUPS(const AEventHandler: IUPSEvent);
begin
  (FUPSPublisher as IPublisher).Subscribe(AEventHandler as IWeakedInterface);
end;

procedure TavmInputConnector.SetRootControl(const AControl: TavmBaseControl);
begin
  Assert(FRoot = nil, 'Root Control can''t be changed');
  FRoot := AControl;
end;

procedure TavmInputConnector.AfterConstruction;
begin
  inherited AfterConstruction;
  FUPSPublisher := TUPSPublisher.Create;
end;

{ TavmBaseControl }

procedure TavmBaseControl.SetSize(const AValue: TVec2);
var
  c: TavmBaseControl;
begin
  if FSize = AValue then Exit;
  FSize := AValue;
  ResetIterator;
  while NextChildControl(c) do
    c.Notify_ParentSizeChanged;
  Invalidate;
end;

procedure TavmBaseControl.SetVisible(const AValue: Boolean);
begin
  if FVisible = AValue then Exit;
  FVisible := AValue;
end;

procedure TavmBaseControl.AfterRegister;
begin
  inherited AfterRegister;
  if Parent is TavmBaseControl then
    FInputConnector := TavmBaseControl(Parent).FInputConnector
  else
  begin
    FInputConnector := TavmInputConnector.Create(Main).WeakRef;
    InputConnector.SetRootControl(Self);
  end;
end;

procedure TavmBaseControl.Notify_ParentSizeChanged;
begin

end;

procedure TavmBaseControl.Notify_ParentOriginChanged;
begin

end;

procedure TavmBaseControl.Notify_ParentPosChanged;
begin

end;

procedure TavmBaseControl.Notify_ParentAngleChanged;
begin

end;

procedure TavmBaseControl.DrawControl(const AMat: TMat3);
begin
  Validate;
end;

procedure TavmBaseControl.DrawRecursive(const AMat: TMat3);
var m: TMat3;
    c: TavmBaseControl;
begin
  if not Visible then Exit;
  m := Transform()*AMat;
  DrawControl(m);
  ResetIterator;
  while NextChildControl(c) do
    c.DrawRecursive(m);
end;

procedure TavmBaseControl.Notify_MouseEnter;
begin
end;

procedure TavmBaseControl.Notify_MouseLeave;
begin
end;

procedure TavmBaseControl.Notify_MouseMove(const APt: TVec2; AShifts: TShifts);
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    if FDragDowned[i] then
      if LenSqr(FDragDownedCoord[i] - APt) >= (FDragThreshold * FDragThreshold) then
      begin
        Notify_DragStart(i+1, FDragDownedCoord[i], AShifts);
        FDragStarted[i] := True;
      end;

    if FDragStarted[i] then
      Notify_DragMove(i+1, APt, AShifts);
  end;
end;

procedure TavmBaseControl.Notify_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts);
begin
end;

procedure TavmBaseControl.Notify_MouseDblClick(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
end;

procedure TavmBaseControl.Notify_DragStart(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  InputConnector.Captured := Self;
end;

procedure TavmBaseControl.Notify_DragMove(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin

end;

procedure TavmBaseControl.Notify_DragStop(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
var i: Integer;
begin
  for i := 0 to 2 do
    if FDragStarted[i] then Exit;
  InputConnector.Captured := nil;
end;

procedure TavmBaseControl.Notify_FocusSet;
begin
end;

procedure TavmBaseControl.Notify_FocusLost;
begin
end;

procedure TavmBaseControl.Notify_KeyDown(AKey: Word; const Ex: TKeyEventEx);
begin
end;

procedure TavmBaseControl.Notify_Char(AChar: WideChar; const Ex: TKeyEventEx);
begin
end;

procedure TavmBaseControl.Notify_KeyUp(AKey: Word; const Ex: TKeyEventEx);
begin
end;

procedure TavmBaseControl.RedirectToParent_MouseWheel(const APt: TVec2; AWheelShift: Integer; AShifts: TShifts);
begin
  if Parent is TavmBaseControl then
    TavmBaseControl(Parent).Notify_MouseWheel(Space_LocalToParent(APt), AWheelShift, AShifts);
end;

procedure TavmBaseControl.UPSSubscribe;
begin
  FUPSSubscriber := TUPSSubscriber.Create({$IfDef FPC}@{$EndIf}OnUPS);
  InputConnector.SubscribeToUPS(FUPSSubscriber);
end;

procedure TavmBaseControl.UPSUnSubscribe;
begin
  FUPSSubscriber := nil;
end;

procedure TavmBaseControl.Notify_MouseDown(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  if (ABtn >= 1) and (ABtn <= 3) then
  begin
    FDragDowned[ABtn-1] := True;
    FDragDownedCoord[ABtn-1] := APt;
  end;
end;

procedure TavmBaseControl.Notify_MouseUp(ABtn: Integer; const APt: TVec2; AShifts: TShifts);
begin
  if (ABtn >= 1) and (ABtn <= 3) then
  begin
    if FDragStarted[ABtn-1] then
      Notify_DragStop(ABtn, APt, AShifts);
    FDragStarted[ABtn-1] := False;
    FDragDowned[ABtn-1] := False;
  end;
end;

procedure TavmBaseControl.Validate;
begin
  if not FValid then
  begin
    FValid := True;
    DoValidate;
  end;
end;

procedure TavmBaseControl.DoValidate;
begin
end;

procedure TavmBaseControl.Invalidate;
begin
  FValid := False;
end;

procedure TavmBaseControl.OnUPS;
begin

end;

function TavmBaseControl.Space_ParentToLocal(const APt: TVec2): TVec2;
var npt: TVec2;
begin
  if Parent is TavmBaseControl then
    Result := APt * TransformInv
  else
  begin
    npt := (APt/Main.States.Viewport.Size) - Vec(0.5,0.5);
    npt := npt * Vec(2.0,-2.0);
    Result := npt * Inv(GetUIMat3(Main.States.Viewport.Size));
    Result := Result * TransformInv;
  end;
end;

function TavmBaseControl.Space_LocalToRootControl(const APt: TVec2): TVec2;
begin
  if Parent is TavmBaseControl then
    Result := TavmBaseControl(Parent).Space_LocalToRootControl(APt * Transform)
  else
    Result := APt;
end;

function TavmBaseControl.Space_LocalToParent(const APt: TVec2): TVec2;
var npt: TVec2;
begin
  if Parent is TavmBaseControl then
    Result := APt * Transform
  else
  begin
    npt := APt * Transform;
    npt := npt * GetUIMat3(Main.States.Viewport.Size);
    npt := npt * Vec(0.5,-0.5);
    Result := (npt + Vec(0.5, 0.5))*Main.States.Viewport.Size;
  end;
end;

procedure TavmBaseControl.HitTestRecursive(const ALocalPt: TVec2; var AControl: TavmBaseControl);
var chld: TavmBaseControl;
begin
  if not Visible then Exit;
  if not LocalPtInArea(ALocalPt) then Exit;

  ResetIterator(True);
  while PrevChildControl(chld) do
  begin
    chld.HitTestRecursive(chld.Space_ParentToLocal(ALocalPt), AControl);
    if AControl <> nil then Exit;
  end;

  HitTestLocal(ALocalPt, AControl);
end;

procedure TavmBaseControl.HitTestLocal(const ALocalPt: TVec2; var AControl: TavmBaseControl);
begin

end;

procedure TavmBaseControl.BringToFront;
var n: Integer;
begin
  if Parent is TavmBaseControl then
  begin
    n := Parent.ChildIndex(Self);
    Assert(n >= 0);
    Parent.ChildMove(n, Parent.ChildCount-1);
  end;
end;

function TavmBaseControl.LocalPtInArea(const Pt: TVec2): Boolean;
begin
  Result := (Pt.x >= 0) and (Pt.y >= 0) and (Pt.x < FSize.x) and (Pt.y < FSize.y);
end;

function TavmBaseControl.HitTest(const V: TVec2; ARecursive: Boolean): TavmBaseControl;
var vlocal: TVec2;
begin
  Result := nil;
  if not Visible then Exit;

  vlocal := Space_ParentToLocal(v);
  if ARecursive then
    HitTestRecursive(vlocal, Result)
  else
    HitTestLocal(vlocal, Result);
end;

function TavmBaseControl.RootControl: TavmBaseControl;
begin
  if Parent is TavmBaseControl then
    Result := TavmBaseControl(Parent).RootControl
  else
    Result := Self;
end;

function TavmBaseControl.InputConnector: TavmInputConnector;
begin
  if FInputConnector = nil then
    Exit(nil);
  Result := TavmInputConnector(FInputConnector.Obj);
  if Result = nil then
    FInputConnector := nil;
end;

function TavmBaseControl.ChildControlsCount: Integer;
begin
  Result := ChildCount(TavmBaseControl);
end;

procedure TavmBaseControl.ResetIterator(ToLast: Boolean);
begin
  if ToLast then
    FIteratorIdx := ChildCount
  else
    FIteratorIdx := -1;
end;

function TavmBaseControl.NextChildControl(out AChild: TavmBaseControl): Boolean;
var obj: TavObject;
begin
  repeat
    Inc(FIteratorIdx);
    if FIteratorIdx >= ChildCount then
    begin
      AChild := nil;
      Exit(False);
    end;

    obj := Child[FIteratorIdx];
    if (obj is TavmBaseControl) then
    begin
      AChild := TavmBaseControl(obj);
      Exit(True);
    end;
  until False;
end;

function TavmBaseControl.PrevChildControl(out AChild: TavmBaseControl): Boolean;
var obj: TavObject;
begin
  repeat
    Dec(FIteratorIdx);
    if FIteratorIdx < 0 then
    begin
      AChild := nil;
      Exit(False);
    end;

    obj := Child[FIteratorIdx];
    if (obj is TavmBaseControl) then
    begin
      AChild := TavmBaseControl(obj);
      Exit(True);
    end;
  until False;
end;

function TavmBaseControl.Transform: TMat3;
var offset: TVec2;
begin
  offset := FSize * FOrigin;
  Result := Mat3Translate(-offset) * Mat3(FAngle, FPos);
end;

function TavmBaseControl.TransformInv: TMat3;
begin
  Result := Inv(Transform);
end;

function TavmBaseControl.AbsTransform: TMat3;
begin
  if Parent is TavmBaseControl then
    Result := Transform * TavmBaseControl(Parent).AbsTransform
  else
    Result := Transform;
end;

function TavmBaseControl.AbsTransformInv: TMat3;
begin
  Result := Inv(AbsTransform);
end;

procedure TavmBaseControl.Draw();
begin
  Main.States.DepthTest := False;
  Main.States.Blending[0] := True;
  Main.States.SetBlendFunctions(bfOne, bfInvSrcAlpha);

  DrawRecursive(IdentityMat3);
end;

procedure TavmBaseControl.AfterConstruction;
begin
  inherited AfterConstruction;
  FVisible := True;
  FAllowFocus := True;
  FDragThreshold := 5;
end;

destructor TavmBaseControl.Destroy;
var ic: TavmInputConnector;
begin
  inherited Destroy;
  ic := InputConnector;
  if ic <> nil then
    if ic.RootControl = Self then
      ic.Free;
end;

procedure TavmBaseControl.SetOrigin(const AValue: TVec2);
var
  c: TavmBaseControl;
begin
  if FOrigin = AValue then Exit;
  FOrigin := AValue;
  ResetIterator;
  while NextChildControl(c) do
    c.Notify_ParentOriginChanged;
  Invalidate;
end;

procedure TavmBaseControl.SetAngle(const AValue: Single);
var
  c: TavmBaseControl;
begin
  if FAngle = AValue then Exit;
  FAngle := AValue;
  ResetIterator;
  while NextChildControl(c) do
    c.Notify_ParentOriginChanged;
end;

function TavmBaseControl.GetAbsAngle: Single;
begin
  if Parent is TavmBaseControl then
    Result := FAngle + TavmBaseControl(Parent).AbsAngle
  else
    Result := FAngle;
end;

function TavmBaseControl.GetAbsPos: TVec2;
begin
  //todo
  Result := Vec(0,0);
end;

procedure TavmBaseControl.SetAbsAngle(const AValue: Single);
begin
  if Parent is TavmBaseControl then
    FAngle := AValue - TavmBaseControl(Parent).AbsAngle
  else
    FAngle := AValue;
end;

procedure TavmBaseControl.SetAbsPos(const AValue: TVec2);
begin
  //todo
end;

procedure TavmBaseControl.SetPos(const AValue: TVec2);
var
  c: TavmBaseControl;
begin
  if FPos = AValue then Exit;
  FPos := AValue;
  ResetIterator;
  while NextChildControl(c) do
    c.Notify_ParentPosChanged;
end;

end.

