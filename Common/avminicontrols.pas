unit avMiniControls;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils,
  avBase, avRes, avTypes, mutils;

type

  TavmInputConnector = class;

  { TavmBaseControl }

  TavmBaseControl = class (TavMainRenderChild)
  private
    FAngle: Single;
    FOrigin: TVec2;
    FPos: TVec2;
    FSize: TVec2;

    FValid: Boolean;
    FIteratorIdx: Integer;

    FInputConnector: TavmInputConnector;

    function GetAbsAngle: Single;
    function GetAbsPos: TVec2;
    procedure SetAbsAngle(const AValue: Single);
    procedure SetAbsPos(const AValue: TVec2);
    procedure SetAngle(const AValue: Single);
    procedure SetOrigin(const AValue: TVec2);
    procedure SetPos(const AValue: TVec2);
    procedure SetSize(const AValue: TVec2);
  protected
    procedure AfterRegister; override;
  protected
    procedure Notify_ParentSizeChanged; virtual;
    procedure Notify_ParentOriginChanged; virtual;
    procedure Notify_ParentPosChanged; virtual;
    procedure Notify_ParentAngleChanged; virtual;

    procedure DrawControl(const AMat: TMat3); virtual;
    procedure DrawRecursive(const AMat: TMat3); virtual;
  protected
    procedure Validate;
    procedure DoValidate; virtual;
    procedure Invalidate;
  protected
    function  Space_LocalToOrigin(const APt: TVec2): TVec2;
    function  Space_ParentToLocal(const APt: TVec2): TVec2;

    procedure HitTestRecursive(const ALocalPt: TVec2; var AControl: TavmBaseControl);
    procedure HitTest(const ALocalPt: TVec2; var AControl: TavmBaseControl); virtual;
  public
    function LocalPtInArea(const Pt: TVec2): Boolean;
    function HitTest(const V: TVec2; ARecursive: Boolean): TavmBaseControl;

    function  ChildControlsCount: Integer;
    procedure ResetIterator(ToLast: Boolean = false);
    function  NextChildControl(out AChild: TavmBaseControl): Boolean;
    function  PrevChildControl(out AChild: TavmBaseControl): Boolean;

    function  InputConnector: TavmInputConnector;

    property Size  : TVec2  read FSize   write SetSize;
    property Origin: TVec2  read FOrigin write SetOrigin;
    property Angle : Single read FAngle  write SetAngle;
    property Pos   : TVec2  read FPos    write SetPos;

    property AbsAngle: Single read GetAbsAngle write SetAbsAngle;
    property AbsPos: TVec2 read GetAbsPos write SetAbsPos;

    function Transform   : TMat3;
    function TransformInv: TMat3;
    function AbsTransform   : TMat3;
    function AbsTransformInv: TMat3;

    procedure Draw();

    destructor Destroy; override;
  end;

  { TavmInputConnector }

  TavmInputConnector = class (TavMainRenderChild)
  private
    FCaptured: TavmBaseControl;

    FFocused: TavmBaseControl;
    FMoved: TavmBaseControl;
    FRoot: TavmBaseControl;

    procedure SetCaptured(const AValue: TavmBaseControl);
    procedure SetFocused(const AValue: TavmBaseControl);
  private
    procedure EMMouseDown    (var AMsg: TavMouseDownMessage); message EM_MOUSEDOWN;
    procedure EMMouseUp      (var AMsg: TavMouseUpMessage);   message EM_MOUSEUP;
    procedure EMMouseDblCkick(var AMsg: TavMouseDblClick);    message EM_MOUSEDBLCLICK;
    procedure EMMouseMove    (var AMsg: TavMouseMessage);     message EM_MOUSEMOVE;
    procedure EMMouseWheel   (var AMsg: TavMouseMessage);     message EM_MOUSEWHEEL;
  private
    procedure EMKeyDown (var AMsg: TavKeyDownMessage); message EM_KEYDOWN;
    procedure EMKeyUp   (var AMsg: TavKeyUpMessage);   message EM_KEYUP;
    procedure EMChar    (var AMsg: TavCharMessage);    message EM_CHAR;
  private
    procedure EMUps (var AMsg: TavMessage); message EM_UPS;
  public
    procedure SetRootControl(const AControl: TavmBaseControl);
    property RootControl: TavmBaseControl read FRoot;

    property Captured: TavmBaseControl read FCaptured write SetCaptured;
    property Focused : TavmBaseControl read FFocused  write SetFocused;
    property Moved   : TavmBaseControl read FMoved;
  end;

  TavmPanel = class (TavmBaseControl)
  private
  public
  end;

implementation

{ TavmInputConnector }

procedure TavmInputConnector.SetCaptured(const AValue: TavmBaseControl);
begin
  if FCaptured = AValue then Exit;
  FCaptured := AValue;
end;

procedure TavmInputConnector.SetFocused(const AValue: TavmBaseControl);
begin
  if FFocused = AValue then Exit;
  FFocused := AValue;
end;

procedure TavmInputConnector.EMMouseDown(var AMsg: TavMouseDownMessage);
begin

end;

procedure TavmInputConnector.EMMouseUp(var AMsg: TavMouseUpMessage);
begin

end;

procedure TavmInputConnector.EMMouseDblCkick(var AMsg: TavMouseDblClick);
begin

end;

procedure TavmInputConnector.EMMouseMove(var AMsg: TavMouseMessage);
begin

end;

procedure TavmInputConnector.EMMouseWheel(var AMsg: TavMouseMessage);
begin

end;

procedure TavmInputConnector.EMKeyDown(var AMsg: TavKeyDownMessage);
begin

end;

procedure TavmInputConnector.EMKeyUp(var AMsg: TavKeyUpMessage);
begin

end;

procedure TavmInputConnector.EMChar(var AMsg: TavCharMessage);
begin

end;

procedure TavmInputConnector.EMUps(var AMsg: TavMessage);
begin

end;

procedure TavmInputConnector.SetRootControl(const AControl: TavmBaseControl);
begin
  Assert(FRoot = nil, 'Root Control can''t be changed');
  FRoot := AControl;
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
end;

procedure TavmBaseControl.AfterRegister;
begin
  inherited AfterRegister;
  if Parent is TavmBaseControl then
    FInputConnector := TavmBaseControl(Parent).InputConnector
  else
  begin
    FInputConnector := TavmInputConnector.Create(Main);
    FInputConnector.SetRootControl(Self);
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

end;

procedure TavmBaseControl.DrawRecursive(const AMat: TMat3);
var m: TMat3;
    c: TavmBaseControl;
begin
  m := Transform();
  DrawControl(m);
  ResetIterator;
  while NextChildControl(c) do
    c.DrawRecursive(m);
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

function TavmBaseControl.Space_LocalToOrigin(const APt: TVec2): TVec2;
begin
  Result := APt - FSize * FOrigin;
end;

function TavmBaseControl.Space_ParentToLocal(const APt: TVec2): TVec2;
begin
  Result := APt * TransformInv;
end;

procedure TavmBaseControl.HitTestRecursive(const ALocalPt: TVec2; var AControl: TavmBaseControl);
var chld: TavmBaseControl;
begin
  if not LocalPtInArea(ALocalPt) then Exit;

  ResetIterator(True);
  while PrevChildControl(chld) do
  begin
    chld.HitTestRecursive(chld.Space_ParentToLocal(ALocalPt), AControl);
    if AControl <> nil then Exit;
  end;

  HitTest(ALocalPt, AControl);
end;

procedure TavmBaseControl.HitTest(const ALocalPt: TVec2; var AControl: TavmBaseControl);
begin

end;

function TavmBaseControl.LocalPtInArea(const Pt: TVec2): Boolean;
var os: TVec2;
begin
  os := Pt - FSize * FOrigin;
  Result := (os.x >= 0) and (os.y >= 0) and (os.x < FSize.x) and (os.y < FSize.y);
end;

function TavmBaseControl.HitTest(const V: TVec2; ARecursive: Boolean): TavmBaseControl;
var vlocal: TVec2;
begin
  Result := nil;
  vlocal := Space_ParentToLocal(v);
  if ARecursive then
    HitTestRecursive(vlocal, Result)
  else
    HitTest(vlocal, Result);
end;

function TavmBaseControl.InputConnector: TavmInputConnector;
begin
  Result := FInputConnector;
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
begin
  Result := Mat3(FAngle, FPos);
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
    Result := Transform * GetUIMat3(Main.States.Viewport.Size);
end;

function TavmBaseControl.AbsTransformInv: TMat3;
begin
  Result := Inv(AbsTransform);
end;

procedure TavmBaseControl.Draw();
begin
  Validate;
  DrawRecursive(IdentityMat3);
end;

destructor TavmBaseControl.Destroy;
begin
  inherited Destroy;
  if FInputConnector.RootControl = Self then
    FInputConnector.Free;
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

