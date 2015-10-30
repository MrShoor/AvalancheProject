unit avControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, avBase, avPlatform, avTypes, avCanvas, avRes, mutils;

const
  EM_SIZECHANGED = EM_EVENTS_START_CONTROLS + 1;

type
  TavRootControl = class;
  TavControl = class;

  TFixtureType = (ftVertical, ftHorizontal);

  { TFixtureLink }

  TFixtureLink = class (TWeakedObject)
  private
    FFixtureType: TFixtureType;
    FFixType  : TFixtureType;
    FBeginItem: IWeakRef;
    FEndItem  : IWeakRef;
    FSize: Single;
    function GetAtItem(index: Integer): TavControl;
    function GetFromItem(index: Integer): TavControl;
    procedure SetSize(AValue: Single);
  public
    property FixtureType: TFixtureType read FFixtureType;
    property Size: Single read FSize write SetSize;
    function FromItemsCount: Integer;
    function AtItemsCount: Integer;
    property FromItem[index: Integer]: TavControl read GetFromItem;
    property AtItem  [index: Integer]: TavControl read GetAtItem;
  end;

  { TavControl }

  TavControl = class (TavObject)
  private
    FCanvas: TavCanvas;
    FRoot  : TavRootControl;
    FPosition: TRectF;
  protected
    function GetPosition: TRectF; virtual;
    procedure SetPosition(AValue: TRectF); virtual;

    function CanRegister(target: TavObject): boolean; override;
    procedure AfterRegister; override;
    procedure BroadcastByControls(var msg);

    procedure DoValidateCanvas; virtual;
    procedure ValidateCanvas;
  public
    function GetAbsolutePosition: TRectF;
    property Position: TRectF read GetPosition write SetPosition;

    property Canvas: TavCanvas read FCanvas;
    procedure Draw; virtual;
  end;

  { TavRootControl }

  TavRootControl = class (TavControl)
  private
    FRender: TavMainRender;
    FDPI: Single;
    function GetDPI: Single;
    function GetWindow: TWindow;
    procedure SetDPI(AValue: Single);
  protected
    function GetPosition: TRectF; override;
    function CanRegister(target: TavObject): boolean; override;
    procedure DoValidateCanvas; override;
  public
    procedure Draw; override;
    function WorkAreaSize: TVec2;

    procedure Dispatch(var message); override;
    property Window: TWindow read GetWindow;
    property Render: TavMainRender read FRender;

    property DPI: Single read GetDPI write SetDPI;

    constructor Create(AParent: TavObject); override;
  end;

  { TavDebugControl }

  TavDebugControl = class (TavControl)
  protected
    procedure DoValidateCanvas; override;
  end;

implementation

{ TFixtureLink }

function TFixtureLink.GetAtItem(index: Integer): TavControl;
begin

end;

function TFixtureLink.GetFromItem(index: Integer): TavControl;
begin

end;

procedure TFixtureLink.SetSize(AValue: Single);
begin

end;

function TFixtureLink.FromItemsCount: Integer;
begin

end;

function TFixtureLink.AtItemsCount: Integer;
begin

end;

{ TavDebugControl }

procedure TavDebugControl.DoValidateCanvas;
var rct: TRectF;
begin
  rct := GetAbsolutePosition;
  Canvas.Rectangle(rct.LeftTop, rct.RightBottom);
end;

{ TavControl }

function TavControl.GetPosition: TRectF;
begin
  Result := FPosition;
end;

procedure TavControl.SetPosition(AValue: TRectF);
begin
  if FPosition = AValue then Exit;
  FPosition := AValue;
end;

function TavControl.CanRegister(target: TavObject): boolean;
begin
  Result := inherited CanRegister(target);
  if not ((target is TavControl) or (target is TavRootControl)) then Result := False;
  if not Result then Exit;
  FRoot := TavRootControl(target.FindAtParents(TavRootControl));
  Result := Assigned(FRoot);
end;

procedure TavControl.AfterRegister;
begin
  inherited AfterRegister;
  if assigned(FRoot) then
    FCanvas := TavCanvas.Create(FRoot.Render);
end;

procedure TavControl.BroadcastByControls(var msg);
var i: Integer;
    obj: TavObject;
begin
  Dispatch(msg);
  for i := 0 to ChildCount - 1 do
    begin
      obj := Child[i];
      if obj is TavControl then
        TavControl(obj).BroadcastByControls(msg);
    end;
end;

procedure TavControl.DoValidateCanvas;
begin

end;

procedure TavControl.ValidateCanvas;
begin
  if not FCanvas.Valid then
  begin
    FCanvas.Clear;
    DoValidateCanvas;
    FCanvas.Valid := True;
  end;
end;

function TavControl.GetAbsolutePosition: TRectF;
var ParentPos: TRectF;
begin
  Result := Position;
  if Assigned(Parent) and (Parent is TavControl) then
  begin
    ParentPos := TavControl(Parent).GetAbsolutePosition;
    Result.LeftTop := ParentPos.LeftTop + Result.LeftTop;
    Result.RightBottom := ParentPos.LeftTop + Result.RightBottom;
  end;
end;

procedure TavControl.Draw;
var i: Integer;
begin
  if assigned(FCanvas) then
  begin
    ValidateCanvas;
    //FCanvas.Draw(); todo
  end;
  for i := 0 to ChildCount - 1 do
    if Child[i] is TavControl then
      TavControl(Child[i]).Draw;
end;

{ TavRootControl }

function TavRootControl.GetWindow: TWindow;
begin
  Result := FRender.Window;
end;

function TavRootControl.GetDPI: Single;
begin
  Result := FDPI;
end;

procedure TavRootControl.SetDPI(AValue: Single);
begin
  if FDPI = AValue then Exit;
  FDPI := AValue;
end;

function TavRootControl.GetPosition: TRectF;
begin
  Result.LeftTop := Vec(0.0, 0.0);
  Result.RightBottom := WorkAreaSize;
end;

function TavRootControl.CanRegister(target: TavObject): boolean;
begin
  Result := target is TavMainRender;
  if Result then
    FRender := TavMainRender(target);
end;

procedure TavRootControl.DoValidateCanvas;
var rct: TRectF;
begin
  inherited DoValidateCanvas;
  rct := GetAbsolutePosition;
  Canvas.Rectangle(rct.LeftTop, rct.RightBottom);
end;

procedure TavRootControl.Draw;
var m: TMat4;
    WorkSize: TVec2;
begin
  WorkSize := WorkAreaSize;
  m := IdentityMat4;
  m.f[0,0] := 2 / WorkSize.x;
  m.f[1,1] := -2 / WorkSize.y;
  m.f[0,3] := -1;
  m.f[1,3] := 1;
  GetCanvasCommonData(FRender).WndMatrix := m;
  inherited Draw;
end;

function TavRootControl.WorkAreaSize: TVec2;
var WndLeft, WndTop, WndWidth, WndHeight: Integer;
begin
  GetRectOfWindow(Window, WndLeft, WndTop, WndWidth, WndHeight);
  Result.x := WndWidth;
  Result.y := WndHeight;
  Result := Result * (1.0/DPI);
end;

procedure TavRootControl.Dispatch(var message);
var i: Integer;
    obj: TavObject;
begin
  inherited Dispatch(message);
  for i := 0 to ChildCount - 1 do
    begin
      obj := Child[i];
      if obj is TavControl then
        TavControl(obj).BroadcastByControls(message);
    end;
end;

constructor TavRootControl.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  FDPI := 96;
end;

end.

