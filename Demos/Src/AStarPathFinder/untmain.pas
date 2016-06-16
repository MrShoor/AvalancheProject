unit untMain;
{$I avConfig.inc}

{$Define AllowDiagonals}
//{$Define DebugOut}

interface

uses
  Windows,
  Classes, SysUtils, {FileUtil,} Forms, Controls, Graphics, Dialogs,
  avContnrs, avContnrsDefaults, avPathFinder;

const SCALE = 2;

type
  IPathFinder = {$IfDef FPC}specialize{$EndIf} IAStar<TPoint>;
  TPathFinder = {$IfDef FPC}specialize{$EndIf} TAStar<TPoint>;
  IPath = {$IfDef FPC}specialize{$EndIf} IArray<TPoint>;

  IInteractiveMap = {$IfDef FPC}specialize{$EndIf} IMap<TPoint>;
  IInteractiveMap2 = interface (IInteractiveMap)
    function Bmp: TBitmap;
  end;

  { TInteractiveMap }

  TInteractiveMap = class(TInterfacedObjectEx, IInteractiveMap, IInteractiveMap2, IEqualityComparer)
  private
    FBmp: TBitmap;

    function CellFree(const X, Y: Integer): Boolean;
  public
    function Hash(const Value): Cardinal;
    function IsEqual(const Left, Right): Boolean;

    function MaxNeighbourCount(const ANode: TPoint): Integer;
    function GetNeighbour(Index: Integer; const ACurrent, ATarget: TPoint; out ANeighbour: TPoint; out MoveWeight, DistWeight: Single): Boolean;

    function NodeComparer: IEqualityComparer;

    function Bmp: TBitmap;

    constructor Create(const AFileName: string);
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm, IDebugOut)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
  private
    FMap : IInteractiveMap2;
    FPath: IPath;

    FStartPt: TPoint;
    FEndPt  : TPoint;

    procedure FindPath;

    procedure DrawQuad(const Pt: TPoint; const AColor: TColor);
    procedure DrawWeight(const Pt: TPoint; W: Single);
    procedure OpeninigNode(const ANode; MoveWeight, AllWeight: Single);
  public

  end;

var
  Form1: TForm1;

implementation

uses
  Math;

{$IfDef FPC}
{$R *.lfm}
{$EndIf}
{$IfDef DCC}
{$R *.dfm}
{$EndIf}

{ TInteractiveMap }

function TInteractiveMap.CellFree(const X, Y: Integer): Boolean;
var pRow: PByte;
begin
  if X < 0 then Exit(False);
  if Y < 0 then Exit(False);
  if X >= FBmp.Width then Exit(False);
  if Y >= FBmp.Height then Exit(False);

  pRow := FBmp.ScanLine[y];
  Inc(pRow, x * 3);
  Result := pRow^ > 127;
end;

function TInteractiveMap.Hash(const Value): Cardinal;
begin
  Result := Murmur2(TPoint(Value), SizeOf(TPoint));
end;

function TInteractiveMap.IsEqual(const Left, Right): Boolean;
begin
  Result := (TPoint(Left).x = TPoint(Right).x) and (TPoint(Left).y = TPoint(Right).y);
end;

function TInteractiveMap.MaxNeighbourCount(const ANode: TPoint): Integer;
begin
  {$IfDef AllowDiagonals}
  Result := 8;
  {$Else}
  Result := 4;
  {$EndIf}
end;

function TInteractiveMap.GetNeighbour(Index: Integer; const ACurrent, ATarget: TPoint;
  out ANeighbour: TPoint; out MoveWeight, DistWeight: Single): Boolean;
var v: TPoint;
    minDelta: Integer;
begin
  Result := False;
  MoveWeight := 0;
  DistWeight := 0;
  ANeighbour := ACurrent;

  case Index of
    0: v := Point( 0, -1);
    1: v := Point(-1,  0);
    2: v := Point( 0,  1);
    3: v := Point( 1,  0);

    4: v := Point(-1, -1);
    5: v := Point( 1, -1);
    6: v := Point(-1,  1);
    7: v := Point( 1,  1);
  else
    Exit;
  end;

  ANeighbour.x := ANeighbour.x + v.x;
  ANeighbour.y := ANeighbour.y + v.y;
  if not CellFree(ANeighbour.x, ANeighbour.y) then Exit;

  MoveWeight := sqrt(v.x*v.x + v.y*v.y);

  v.x := abs(ATarget.x - ANeighbour.x);
  v.y := abs(ATarget.y - ANeighbour.y);
  {$IfDef AllowDiagonals}
  minDelta := Min(v.x, v.y);
  DistWeight := Max(v.x, v.y) - minDelta + minDelta*sqrt(2);
  {$Else}
  DistWeight := v.x+v.y;
  {$EndIf}

  Result := True;
end;

function TInteractiveMap.NodeComparer: IEqualityComparer;
begin
  Result := Self;
end;

function TInteractiveMap.Bmp: TBitmap;
begin
  Result := FBmp;
end;

constructor TInteractiveMap.Create(const AFileName: string);
begin
  FBmp := TBitmap.Create;
  FBmp.LoadFromFile(AFileName);
  FBmp.PixelFormat := pf24bit;
end;

destructor TInteractiveMap.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FBmp);
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMap := TInteractiveMap.Create('minimap.bmp');

  FEndPt := Point(150,90);
  FStartPt := Point(10,10);
  Invalidate;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FStartPt := Point(X div SCALE, Y div SCALE);
    FindPath;
  end;
  if Button = mbRight then
  begin
    FEndPt := Point(X div SCALE, Y div SCALE);
    FindPath;
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
var i: Integer;
begin
  Canvas.CopyRect(Rect(0, 0, FMap.Bmp.Width*SCALE, FMap.Bmp.Height*SCALE), FMap.Bmp.Canvas, Rect(0, 0, FMap.Bmp.Width, FMap.Bmp.Height));

  if Assigned(FPath) then
    for i := 0 to FPath.Count - 1 do
      DrawQuad(FPath[i], clRed);

  DrawQuad(FStartPt, clBlue);
  DrawQuad(FEndPt, clBlue);
end;

procedure TForm1.FindPath;
var pf: IPathFinder;
    startTime, endTime, Freq: Int64;
    s: string;
begin
  pf := TPathFinder.Create(FMap);

  QueryPerformanceCounter(startTime);
  {$IfDef DebugOut}
  FPath := pf.FindPath(FStartPt, FEndPt, Self);
  {$Else}
  FPath := pf.FindPath(FStartPt, FEndPt);
  {$EndIf}
  QueryPerformanceCounter(endTime);
  QueryPerformanceFrequency(Freq);
  if Assigned(FPath) then
    s := 'Found in '
  else
    s := 'Not found in ';
  s := s + IntToStr(Trunc( (endTime - startTime) / Freq * 1000 )) + 'msec';
  Caption := s;
  Invalidate;
end;

procedure TForm1.DrawQuad(const Pt: TPoint; const AColor: TColor);
var rct: TRect;
begin
  rct.Left := Pt.x * SCALE;
  rct.Top := Pt.y * SCALE;
  rct.Right := rct.Left + SCALE;
  rct.Bottom := rct.Top + SCALE;

  Canvas.Brush.Color := AColor;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(rct);
end;

procedure TForm1.DrawWeight(const Pt: TPoint; W: Single);
var rct: TRect;
    s: string;
begin
  rct.Left := Pt.x * SCALE;
  rct.Top := Pt.y * SCALE;
  rct.Right := rct.Left + SCALE;
  rct.Bottom := rct.Top + SCALE;
  s := IntToStr(Round(W));
  Canvas.TextRect(rct, rct.Left, rct.Top, s);
end;

procedure TForm1.OpeninigNode(const ANode; MoveWeight, AllWeight: Single);
begin
  DrawQuad(TPoint(ANode), clGreen);
//  DrawWeight(P, AllWeight);
end;

end.

