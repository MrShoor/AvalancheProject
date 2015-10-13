unit mutils;

{$ifdef fpc}
  {$Macro On}
  {$mode objfpc}{$H+}
  {$modeswitch advancedrecords}
  {$fputype sse2}
{$endif}

{$define NoInline}

interface {$define INTF}

uses
  Classes, SysUtils;

const
  EPS = 0.000005;
  HUGE = MaxInt;

type
  TSingleArr = array of Single;

{$define TCompType := Single}
{$define TV2 := TVec2}
{$define TV3 := TVec3}
{$define TV4 := TVec4}
{$define PV2 := PVec2}
{$define PV3 := PVec3}
{$define PV4 := PVec4}
{$define TV2Arr := TVec2Arr}
{$define TV3Arr := TVec3Arr}
{$define TV4Arr := TVec4Arr}
{$define TM2 := TMat2}
{$define TM3 := TMat3}
{$define TM4 := TMat4}
{$define PM2 := PMat2}
{$define PM3 := PMat3}
{$define PM4 := PMat4}
{$define TM2Arr := TMat2Arr}
{$define TM3Arr := TMat3Arr}
{$define TM4Arr := TMat4Arr}
{$Include mutils_v.inc}

{$define TCompType := Integer}
{$define TV2 := TVec2i}
{$define TV3 := TVec3i}
{$define TV4 := TVec4i}
{$define PV2 := PVec2i}
{$define PV3 := PVec3i}
{$define PV4 := PVec4i}
{$define TV2Arr := TVec2iArr}
{$define TV3Arr := TVec3iArr}
{$define TV4Arr := TVec4iArr}
{$define TM2 := TMat2i}
{$define TM3 := TMat3i}
{$define TM4 := TMat4i}
{$define PM2 := PMat2i}
{$define PM3 := PMat3i}
{$define PM4 := PMat4i}
{$define TM2Arr := TMat2iArr}
{$define TM3Arr := TMat3iArr}
{$define TM4Arr := TMat4iArr}
{$Include mutils_v.inc}

{$define TCompType := ShortInt}
{$define TV2 := TVec2s}
{$define TV3 := TVec3s}
{$define TV4 := TVec4s}
{$define PV2 := PVec2s}
{$define PV3 := PVec3s}
{$define PV4 := PVec4s}
{$define TV2Arr := TVec2sArr}
{$define TV3Arr := TVec3sArr}
{$define TV4Arr := TVec4sArr}
{$define TM2 := TMat2s}
{$define TM3 := TMat3s}
{$define TM4 := TMat4s}
{$define PM2 := PMat2s}
{$define PM3 := PMat3s}
{$define PM4 := PMat4s}
{$define TM2Arr := TMat2sArr}
{$define TM3Arr := TMat3sArr}
{$define TM4Arr := TMat4sArr}
{$Include mutils_v.inc}

{$define TCompType := Byte}
{$define TV2 := TVec2b}
{$define TV3 := TVec3b}
{$define TV4 := TVec4b}
{$define PV2 := PVec2b}
{$define PV3 := PVec3b}
{$define PV4 := PVec4b}
{$define TV2Arr := TVec2bArr}
{$define TV3Arr := TVec3bArr}
{$define TV4Arr := TVec4bArr}
{$define TM2 := TMat2b}
{$define TM3 := TMat3b}
{$define TM4 := TMat4b}
{$define PM2 := PMat2b}
{$define PM3 := PMat3b}
{$define PM4 := PMat4b}
{$define TM2Arr := TMat2bArr}
{$define TM3Arr := TMat3bArr}
{$define TM4Arr := TMat4bArr}
{$Include mutils_v.inc}

type

  { TQuat }

  TQuat = record
  case Byte of
    0: (x, y, z, w: Single);
    1: (v: TVec3; a: Single);
  end;

  { TLine }

  TLine = record
  case Byte of
    0: (Pnt, Dir: TVec3);
  end;

  { TLine2D }

  TLine2D = record
  case Byte of
    0: (A, B, C: Single);
    1: (Norm: TVec2; Offset: Single);
    2: (V: TVec3);
  end;

  { TSegment2D }

  TSegment2D = record
  public
    Pt1, Pt2: TVec2;
    function Line(normalized: Boolean = False): TLine2D;
  end;

  { TRectF }

  TRectF = record
    function Center  : TVec2;
    function Size    : TVec2;
    function IsEmpty : Boolean;
  case Byte of
    0: (Left, Top, Right, Bottom: Single);
    1: (LeftTop, RightBottom: TVec2);
    2: (min, max: TVec2);
    3: (f: array [0..4] of Single);
    4: (v: TVec4);
  end;

  { TRectI }

  TRectI = record
    function Center  : TVec2;
    function Size    : TVec2i;
    function IsEmpty : Boolean;
  case Byte of
    0: (Left, Top, Right, Bottom: Integer);
    1: (LeftTop, RightBottom: TVec2i);
    2: (f: array [0..4] of Integer);
    3: (v: TVec4i);
  end;

  { TAABB }

  TAABB = record
    function Center  : TVec3;
    function Size    : TVec3;
    function IsEmpty : Boolean;
    function Point(index: Integer): TVec3;
  case Byte of
    0: (min, max: TVec3);
  end;

const
  EmptyAABB: TAABB = (min: (x: HUGE; y: HUGE; z: HUGE); max: (x: -HUGE; y: -HUGE; z: -HUGE));

type
  { TPlane }

  TPlane = packed record
  case Byte of
    0: (A,B,C,D: Single);
    1: (Norm: TVec3);
  end;

const
  Black: TVec4 = (x: 0; y: 0; z: 0; w: 0);

  IdentityMat3: TMat3 = (f: ((1,0,0), (0,1,0), (0,0,1)));
  ZeroMat3: TMat3 = (f: ((0,0,0), (0,0,0), (0,0,0)));

  IdentityMat4: TMat4 = (f: ((1,0,0,0), (0,1,0,0), (0,0,1,0), (0,0,0,1)));
  ZeroMat4: TMat4 = (f: ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0)));

Operator := (const v: TVec2S)v2: TVec2I; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec3S)v2: TVec3I; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec4S)v2: TVec4I; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec2S)v2: TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec3S)v2: TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec4S)v2: TVec4; {$IFNDEF NoInline} inline; {$ENDIF}

Operator := (const v: TVec2I)v2: TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec3I)v2: TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec4I)v2: TVec4; {$IFNDEF NoInline} inline; {$ENDIF}

Operator / (const v1, v2: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
Operator / (const v1, v2: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
Operator / (const v1, v2: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
Operator / (const v: TVec2; const f: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
Operator / (const v: TVec3; const f: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
Operator / (const v: TVec4; const f: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}

Operator * (const a, b: TQuat): TQuat; {$IFNDEF NoInline} inline; {$ENDIF}
Operator * (const a: TQuat; b: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
Operator * (const a: TVec3I; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}

function Quat(const Dir: TVec3; Angle: Single): TQuat; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function Mat(const newX, newY: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat(const newX, newY, newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat(const Rotate: Single): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat(const Rotate: Single; newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function Mat(const newX, newY, newZ: TVec3): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat(const newX, newY, newZ, newPos: TVec3): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function MatTranslate(const newPos: TVec3): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function RectF(Left, Top, Right, Bottom: Single): TRectF; {$IFNDEF NoInline} inline; {$ENDIF}
function RectI(Left, Top, Right, Bottom: Integer): TRectI; {$IFNDEF NoInline} inline; {$ENDIF}
function Plane(const normal, point: TVec3): TPlane; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Plane(A,B,C,D: single): TPlane; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Plane(const pt1, pt2, pt3: TVec3): TPlane; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function Len(const v: TVec2): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Len(const v: TVec3): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Len(const v: TVec4): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Normalize(const v: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Normalize(const v: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Normalize(const v: TVec4): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function NormalizeWeights(const v: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function NormalizeWeights(const v: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function NormalizeWeights(const v: TVec4): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function SetLen(const v: TVec2; newLen: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function SetLen(const v: TVec3; newLen: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function SetLen(const v: TVec4; newLen: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: Single; s: Single): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: TVec2; s: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: TVec3; s: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: TVec4; s: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const m1, m2: TMat2; s: Single): TMat2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const m1, m2: TMat3; s: Single): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const m1, m2: TMat4; s: Single): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function Rotate(const v: TVec2; const Angle: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function Intersect(const Line1, Line2: TLine2D): TVec2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Intersect(const Seg: TSegment2D; const Line: TLine2D; out IntPoint: TVec2): Boolean; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Intersect(const Plane: TPlane; Line: TLine): TVec3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Distance(const Pt: TVec2; const Seg: TSegment2D): Single; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Projection(const Pt: TVec2; const Line: TLine2D): TVec2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Inv(const m: TMat2): TMat2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Inv(const m: TMat3): TMat3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Inv(const m: TMat4): TMat4; overload;{$IFNDEF NoInline} inline; {$ENDIF}

function Trunc(const V: TVec2): TVec2i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Trunc(const V: TVec3): TVec3i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Trunc(const V: TVec4): TVec4i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Round(const V: TVec2): TVec2i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Round(const V: TVec3): TVec3i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Round(const V: TVec4): TVec4i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Ceil(const V: TVec2): TVec2i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Ceil(const V: TVec3): TVec3i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Ceil(const V: TVec4): TVec4i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Floor(const V: TVec2): TVec2i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Floor(const V: TVec3): TVec3i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Floor(const V: TVec4): TVec4i; overload;{$IFNDEF NoInline} inline; {$ENDIF}

function IsPow2(Num: LongInt): Boolean; overload;
function IsPow2(Vec: TVec2i): Boolean; overload;
function NextPow2(Num: LongInt): LongInt; overload;
function NextPow2(Vec: TVec2i): TVec2i; overload;
function Log2Int(v: Integer): Integer;
function GetMipsCount(Width, Height: Integer): Integer;

function Bezier3(const pt1, pt2, pt3, pt4: TVec2; t: single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function SetViewMatrix(var MatrixView: TMat4; const From, At, Worldup: TVec3; leftHanded: boolean = true): HResult;

operator = (const v1, v2: TRectF): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
operator = (const v1, v2: TRectI): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}

operator + (const AABB: TAABB; v: TVec3): TAABB; {$IFNDEF NoInline} inline; {$ENDIF}

operator * (const v: TVec2i; s: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}

implementation {$undef INTF} {$define IMPL}

uses Math;

{ TAABB }

function TAABB.Center: TVec3;
begin
  Result := min*0.5 + max*0.5;
end;

function TAABB.Size: TVec3;
begin
  Result := max - min;
end;

function TAABB.IsEmpty: Boolean;
begin
  Result := (max.x<=min.x) or (max.y<=min.y) or (max.z<=min.z);
end;

function TAABB.Point(index: Integer): TVec3;
begin
  case index mod 8 of
    0: Result := Vec(min.x, min.y, min.z);
    1: Result := Vec(min.x, max.y, min.z);
    2: Result := Vec(max.x, max.y, min.z);
    3: Result := Vec(max.x, min.y, min.z);
    4: Result := Vec(min.x, min.y, max.z);
    5: Result := Vec(min.x, max.y, max.z);
    6: Result := Vec(max.x, max.y, max.z);
    7: Result := Vec(max.x, min.y, max.z);
  end;
end;

  { TRectI }

function TRectI.Center: TVec2;
begin
  Result := LeftTop*0.5 + RightBottom*0.5;
end;

function TRectI.Size: TVec2i;
begin
  Result := RightBottom - LeftTop;
end;

function TRectI.IsEmpty: Boolean;
begin
  Result := (Right<=Left) or (Bottom<=Top);
end;

{$define TCompType := Single}
{$define TV2 := TVec2}
{$define TV3 := TVec3}
{$define TV4 := TVec4}
{$define PV2 := PVec2}
{$define PV3 := PVec3}
{$define PV4 := PVec4}
{$define TV2Arr := TVec2Arr}
{$define TV3Arr := TVec4Arr}
{$define TV4Arr := TVec3Arr}
{$define TM2 := TMat2}
{$define TM3 := TMat3}
{$define TM4 := TMat4}
{$Include mutils_v.inc}

{$define TCompType := Integer}
{$define TV2 := TVec2i}
{$define TV3 := TVec3i}
{$define TV4 := TVec4i}
{$define PV2 := PVec2i}
{$define PV3 := PVec3i}
{$define PV4 := PVec4i}
{$define TV2Arr := TVec2iArr}
{$define TV3Arr := TVec4iArr}
{$define TV4Arr := TVec3iArr}
{$define TM2 := TMat2i}
{$define TM3 := TMat3i}
{$define TM4 := TMat4i}
{$Include mutils_v.inc}

{$define TCompType := ShortInt}
{$define TV2 := TVec2s}
{$define TV3 := TVec3s}
{$define TV4 := TVec4s}
{$define PV2 := PVec2s}
{$define PV3 := PVec3s}
{$define PV4 := PVec4s}
{$define TV2Arr := TVec2sArr}
{$define TV3Arr := TVec4sArr}
{$define TV4Arr := TVec3sArr}
{$define TM2 := TMat2s}
{$define TM3 := TMat3s}
{$define TM4 := TMat4s}
{$Include mutils_v.inc}

{$define TCompType := Byte}
{$define TV2 := TVec2b}
{$define TV3 := TVec3b}
{$define TV4 := TVec4b}
{$define PV2 := PVec2b}
{$define PV3 := PVec3b}
{$define PV4 := PVec4b}
{$define TV2Arr := TVec2bArr}
{$define TV3Arr := TVec4bArr}
{$define TV4Arr := TVec3bArr}
{$define TM2 := TMat2b}
{$define TM3 := TMat3b}
{$define TM4 := TMat4b}
{$Include mutils_v.inc}
operator := (const v: TVec2S)v2: TVec2I;
begin
  v2.x := v.x;
  v2.y := v.y;
end;

operator := (const v: TVec3S)v2: TVec3I;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
end;

operator := (const v: TVec4S)v2: TVec4I;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
  v2.w := v.w;
end;

operator := (const v: TVec2S)v2: TVec2;
begin
  v2.x := v.x;
  v2.y := v.y;
end;

operator := (const v: TVec3S)v2: TVec3;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
end;

operator := (const v: TVec4S)v2: TVec4;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
  v2.w := v.w;
end;

operator := (const v: TVec2I)v2: TVec2;
begin
  v2.x := v.x;
  v2.y := v.y;
end;

operator := (const v: TVec3I)v2: TVec3;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
end;

operator := (const v: TVec4I)v2: TVec4;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
  v2.w := v.w;
end;

Operator / (const v1, v2: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x / v2.x;
  Result.y := v1.y / v2.y;
end;

Operator / (const v1, v2: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x / v2.x;
  Result.y := v1.y / v2.y;
  Result.z := v1.z / v2.z;
end;

Operator / (const v1, v2: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x / v2.x;
  Result.y := v1.y / v2.y;
  Result.z := v1.z / v2.z;
  Result.z := v1.w / v2.w;
end;

Operator / (const v: TVec2; const f: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v * (1.0 / f);
end;

Operator / (const v: TVec3; const f: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v * (1.0 / f);
end;

Operator / (const v: TVec4; const f: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v * (1.0 / f);
end;

Operator * (const a, b: TQuat): TQuat; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.w:=a.w*b.w-a.x*b.x-a.y*b.y-a.z*b.z;
  Result.x:=a.w*b.x+a.x*b.w+a.y*b.z-a.z*b.y;
  Result.y:=a.w*b.y+a.y*b.w+a.z*b.x-a.x*b.z;
  Result.z:=a.w*b.z+a.z*b.w+a.x*b.y-a.y*b.x;
end;

Operator * (const a: TQuat; b: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x:=b.x-2*b.x*(sqr(a.y)+sqr(a.z))+2*b.y*(a.x*a.y+a.z*a.w)+2*b.z*(a.x*a.z-a.y*a.w);
  Result.y:=2*b.x*(a.x*a.y-a.z*a.w)+b.y-2*b.y*(sqr(a.x)+sqr(a.z))+2*b.z*(a.y*a.z+a.x*a.w);
  Result.z:=2*b.x*(a.x*a.z+a.y*a.w)+2*b.y*(a.y*a.z-a.x*a.w)+b.z-2*b.z*(sqr(a.x)+sqr(a.y));
end;

Operator * (const a: TVec3I; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := a.x*s;
  Result.y := a.y*s;
  Result.z := a.z*s;
end;

function Quat(const Dir: TVec3; Angle: Single): TQuat; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.v := Dir * sin(0.5 * angle);
  Result.a := cos(0.5 * angle);
end;

function Mat(const newX, newY: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Row[0] := Vec(newX, 0);
  Result.Row[1] := Vec(newY, 0);
  Result.Row[2] := Vec(0, 0, 1.0);
end;

function Mat(const newX, newY, newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Row[0] := Vec(newX, newPos.x);
  Result.Row[1] := Vec(newY, newPos.y);
  Result.Row[2] := Vec(0, 0, 1);
end;

function Mat(const Rotate: Single): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
var sn, cs: float;
begin
  sincos(Rotate, sn, cs);
  Result := Mat(Vec(cs,  sn), Vec(sn, -cs));
end;

function Mat(const Rotate: Single; newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
var sn, cs: float;
begin
  sincos(Rotate, sn, cs);
  Result := Mat( Vec(cs,  sn),
                 Vec(sn, -cs),
                 newPos        );
end;

function Mat(const newX, newY, newZ: TVec3): TMat4; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := Mat(NewX, NewY, NewZ, Vec(0,0,0));
end;

function Mat(const newX, newY, newZ, newPos: TVec3): TMat4; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := IdentityMat4;
  Result.Row[0] := Vec(newX, newPos.x);
  Result.Row[1] := Vec(newY, newPos.y);
  Result.Row[2] := Vec(newZ, newPos.z);
  Result.Row[3] := Vec(0, 0, 0, 1.0);
end;

function MatTranslate(const newPos: TVec3): TMat4;
begin
  Result := IdentityMat4;
  Result.Col[3] := Vec(newPos, 1);
end;

function RectF(Left, Top, Right, Bottom: Single): TRectF; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Left := Left;
  Result.Right := Right;
  Result.Top := Top;
  Result.Bottom := Bottom;
end;

function RectI(Left, Top, Right, Bottom: Integer): TRectI; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Left := Left;
  Result.Right := Right;
  Result.Top := Top;
  Result.Bottom := Bottom;
end;

function Plane(const normal, point: TVec3): TPlane; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Norm := Normalize(normal);
  Result.D := -Dot(Result.Norm, point);
end;

function Plane(A, B, C, D: single): TPlane; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.A:=A;
  Result.B:=B;
  Result.C:=C;
  Result.D:=D;
end;

function Plane(const pt1, pt2, pt3: TVec3): TPlane; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := Plane(Cross(pt2 - pt1, pt3 - pt1), pt1);
end;

function Len(const v: TVec2): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := sqrt(dot(v, v));
end;

function Len(const v: TVec3): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := sqrt(dot(v, v));
end;

function Len(const v: TVec4): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := sqrt(dot(v, v));
end;

function Normalize(const v: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v / Len(v);
end;

function Normalize(const v: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v / Len(v);
end;

function Normalize(const v: TVec4): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v / Len(v);
end;

function NormalizeWeights(const v: TVec2): TVec2;
begin
  Result := v * (1 / (v.x+v.y));
end;

function NormalizeWeights(const v: TVec3): TVec3;
begin
  Result := v * (1 / (v.x+v.y+v.z));
end;

function NormalizeWeights(const v: TVec4): TVec4;
begin
  Result := v * (1 / (v.x+v.y+v.z+v.w));
end;

function SetLen(const v: TVec2; newLen: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v / Len(v) * newLen;
end;

function SetLen(const v: TVec3; newLen: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v / Len(v) * newLen;
end;

function SetLen(const v: TVec4; newLen: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v / Len(v) * newLen;
end;

function Lerp(const v1, v2: Single; s: Single): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := v1 + s * (v2 - v1);
end;

function Lerp(const v1, v2: TVec2; s: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x + s * (v2.x-v1.x);
  Result.y := v1.y + s * (v2.y-v1.y);
end;

function Lerp(const v1, v2: TVec3; s: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x + s * (v2.x-v1.x);
  Result.y := v1.y + s * (v2.y-v1.y);
  Result.z := v1.z + s * (v2.z-v1.z);
end;

function Lerp(const v1, v2: TVec4; s: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x + s * (v2.x-v1.x);
  Result.y := v1.y + s * (v2.y-v1.y);
  Result.z := v1.z + s * (v2.z-v1.z);
  Result.w := v1.w + s * (v2.w-v1.w);
end;

function Lerp(const m1, m2: TMat2; s: Single): TMat2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Row[0] := Lerp(m1.Row[0], m2.Row[0], s);
  Result.Row[1] := Lerp(m1.Row[1], m2.Row[1], s);
end;

function Lerp(const m1, m2: TMat3; s: Single): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Row[0] := Lerp(m1.Row[0], m2.Row[0], s);
  Result.Row[1] := Lerp(m1.Row[1], m2.Row[1], s);
  Result.Row[2] := Lerp(m1.Row[2], m2.Row[2], s);
end;

function Lerp(const m1, m2: TMat4; s: Single): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Row[0] := Lerp(m1.Row[0], m2.Row[0], s);
  Result.Row[1] := Lerp(m1.Row[1], m2.Row[1], s);
  Result.Row[2] := Lerp(m1.Row[2], m2.Row[2], s);
  Result.Row[3] := Lerp(m1.Row[3], m2.Row[3], s);
end;

function Trunc(const V: TVec2): TVec2i;
begin
  Result.x := Trunc(V.x);
  Result.y := Trunc(V.y);
end;

function Trunc(const V: TVec3): TVec3i;
begin
  Result.x := Trunc(V.x);
  Result.y := Trunc(V.y);
  Result.z := Trunc(V.z);
end;

function Trunc(const V: TVec4): TVec4i;
begin
  Result.x := Trunc(V.x);
  Result.y := Trunc(V.y);
  Result.z := Trunc(V.z);
  Result.w := Trunc(V.w);
end;

function Round(const V: TVec2): TVec2i;
begin
  Result.x := Round(V.x);
  Result.y := Round(V.y);
end;

function Round(const V: TVec3): TVec3i;
begin
  Result.x := Round(V.x);
  Result.y := Round(V.y);
  Result.z := Round(V.z);
end;

function Round(const V: TVec4): TVec4i;
begin
  Result.x := Round(V.x);
  Result.y := Round(V.y);
  Result.z := Round(V.z);
  Result.w := Round(V.w);
end;

function Ceil(const V: TVec2): TVec2i;
begin
  Result.x := Ceil(V.x);
  Result.y := Ceil(V.y);
end;

function Ceil(const V: TVec3): TVec3i;
begin
  Result.x := Ceil(V.x);
  Result.y := Ceil(V.y);
  Result.z := Ceil(V.z);
end;

function Ceil(const V: TVec4): TVec4i;
begin
  Result.x := Ceil(V.x);
  Result.y := Ceil(V.y);
  Result.z := Ceil(V.z);
  Result.w := Ceil(V.w);
end;

function Floor(const V: TVec2): TVec2i;
begin
  Result.x := Floor(V.x);
  Result.y := Floor(V.y);
end;

function Floor(const V: TVec3): TVec3i;
begin
  Result.x := Floor(V.x);
  Result.y := Floor(V.y);
  Result.z := Floor(V.z);
end;

function Floor(const V: TVec4): TVec4i;
begin
  Result.x := Floor(V.x);
  Result.y := Floor(V.y);
  Result.z := Floor(V.z);
  Result.w := Floor(V.w);
end;

function IsPow2(Num: LongInt): Boolean;
begin
  Result := (Num and -Num) = Num;
end;

function IsPow2(Vec: TVec2i): Boolean;
begin
  Result := IsPow2(Vec.x) and IsPow2(Vec.y);
end;

function NextPow2(Num: LongInt): LongInt; //http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
begin
  Dec(Num);
  Num := Num Or (Num Shr 1);
  Num := Num Or (Num Shr 2);
  Num := Num Or (Num Shr 4);
  Num := Num Or (Num Shr 8);
  Num := Num Or (Num Shr 16);
  Result := Num + 1;
end;

function NextPow2(Vec: TVec2i): TVec2i;
begin
  Result.x := NextPow2(Vec.x);
  Result.y := NextPow2(Vec.y);
end;

function Log2Int(v: Integer): Integer; //http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog
begin
  if (v > $FFFF) then
  begin
    Result := 16;
    v := v shr Result;
  end
  else
    Result := 0;

  if (v > $FF) then
  begin
    v := v shr 8;
    Result := Result or 8;
  end;

  if (v > $F) then
  begin
    v := v shr 4;
    Result := Result or 4;
  end;

  if (v > $3) then
  begin
    v := v shr 2;
    Result := Result or 2;
  end;
  Result := Result or (v shr 1);
end;

function GetMipsCount(Width, Height: Integer): Integer;
begin
  Result := Log2Int(Min(Width, Height))+1;
end;

function Bezier3(const pt1, pt2, pt3, pt4: TVec2; t: single): TVec2;
var t2: single;
begin
  t2 := 1 - t;
  Result := pt1*(t2*t2*t2) + pt2*(3*t*t2*t2) + pt3*(3*t*t*t2) + pt4*(t*t*t);
end;

function SetViewMatrix(var MatrixView: TMat4; const From, At, Worldup: TVec3; leftHanded: boolean = true): HResult;
var
  View: TVec3;
  Length: Single;
  DotProduct: Single;
  Up: TVec3;
  Right: TVec3;
begin
  // Get the z basis vector, which points straight ahead. This is the
  // difference from the eyepoint to the lookat point.
  View := At - From;
  Length := Len(View);
  if (Length < EPS) then
  begin
    Result := -1;
    Exit;
  end;
  // Normalize the z basis vector
  View := View / Length;
  // Get the dot product, and calculate the projection of the z basis
  // vector onto the up vector. The projection is the y basis vector.
  DotProduct := Dot(WorldUp,View);
  Up := WorldUp - (View * DotProduct);
  // If this vector has near-zero length because the input specified a
  // bogus up vector, let's try a default up vector
  Length := Len(Up);
  if (Length < EPS) then
  begin
    Up := Vec(0.0,1.0,0.0) - (View * View.y);
    // If we still have near-zero length, resort to a different axis.
    Length := Len(Up);
    if (Length < EPS) then
    begin
      Up := Vec(0.0,0.0,1.0) - (View * View.z);
      Length := Len(Up);
      if (Length < EPS) then
      begin
        Result := -1;
        Exit;
      end;
    end;
  end;
  // Normalize the y basis vector
  Up := Up / Length;
  // The x basis vector is found simply with the cross product of the y
  // and z basis vectors
  if leftHanded then Right := Cross(Up, View) else Right := Cross(View, Up);
  // Start building the matrix. The first three rows contains the basis
  // vectors used to rotate the view to point at the lookat point
  MatrixView.f[0, 0] := Right.x;  MatrixView.f[1, 0] := Up.x;  MatrixView.f[2, 0] := View.x;  MatrixView.f[3, 0] := 0;
  MatrixView.f[0, 1] := Right.y;  MatrixView.f[1, 1] := Up.y;  MatrixView.f[2, 1] := View.y;  MatrixView.f[3, 1] := 0;
  MatrixView.f[0, 2] := Right.z;  MatrixView.f[1, 2] := Up.z;  MatrixView.f[2, 2] := View.z;  MatrixView.f[3, 2] := 0;

  // Do the translation values (rotations are still about the eyepoint)
  MatrixView.f[0, 3] := - Dot(From,Right );
  MatrixView.f[1, 3] := - Dot(From,Up );
  MatrixView.f[2, 3] := - Dot(From,View );
  MatrixView.f[3, 3] := 1;
  Result := S_OK;
end;

operator = (const v1, v2: TRectF): Boolean;
begin
  Result := (v1.LeftTop = v2.LeftTop) and (v1.RightBottom = v2.RightBottom);
end;

operator = (const v1, v2: TRectI): Boolean;
begin
  Result := (v1.LeftTop = v2.LeftTop) and (v1.RightBottom = v2.RightBottom);
end;

operator + (const AABB: TAABB; v: TVec3): TAABB;
begin
  Result.min := min(AABB.min, v);
  Result.max := max(AABB.max, v);
end;

operator * (const v: TVec2i; s: Single): TVec2;
begin
  Result.x := v.x * s;
  Result.y := v.y * s;
end;

function Rotate(const v: TVec2; const Angle: Single): TVec2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var sn, cs: Extended;
begin
  sincos(Angle, sn, cs);
  Result.x := cs * v.x - sn * v.y;
  Result.y := cs * v.y + sn * v.x;
end;

function Intersect(const Line1, Line2: TLine2D): TVec2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var m: TMat2;
    b: TVec2;
begin
  m.Row[0] := Line1.Norm;
  m.Row[1] := Line2.Norm;
  b.x := - Line1.Offset;
  b.y := - Line2.Offset;
  Result := Inv(m) * b;
end;

function Intersect(const Seg: TSegment2D; const Line: TLine2D; out
  IntPoint: TVec2): Boolean;
var dir: TVec2;
begin
  IntPoint := Intersect(Line, Seg.Line);
  dir := Seg.Pt2 - Seg.Pt1;
  Result := (Dot(IntPoint - Seg.Pt1, dir) >= 0) and (Dot(IntPoint - Seg.Pt2, dir) <= 0);
end;

function Intersect(const Plane: TPlane; Line: TLine): TVec3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var Da,Db : single;
    k     : single;
begin
  Da := -Dot(Plane.Norm, Line.Pnt);
  Db := -Dot(Plane.Norm, Line.Pnt + Line.Dir);
  if Da = Db then Exit(Vec(Infinity, Infinity, Infinity));
  k := (Plane.D - Da) / (Db - Da);
  Result := Line.Pnt + (Line.Dir * k);
end;

function Distance(const Pt: TVec2; const Seg: TSegment2D): Single;
var dir, v1, v2: TVec2;
    segline: TLine2D;
begin
  dir := Seg.Pt2 - Seg.Pt1;
  v1 := Pt - Seg.Pt1;
  v2 := Pt - Seg.Pt2;
  if (Dot(v1, dir) > 0) and (Dot(v2, dir) < 0) then
  begin
    segline := Seg.Line(True);
    Result := abs(dot(segline.Norm, Pt) + segline.C);
  end
  else
  begin
    Result := Min(LenSqr(v1), LenSqr(v2));
    Result := sqrt(Result);
  end;
end;

function Projection(const Pt: TVec2; const Line: TLine2D): TVec2;
var C: Single;
begin
  C := Dot(Pt, Line.Norm) + Line.Offset;
  Result := Pt + Line.Norm*C;
end;

function Inv(const m: TMat2): TMat2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var
  D : Single;
begin
  D := 1 / Det(m);
  Result.f[0, 0] := m.f[1, 1] * D;
  Result.f[0, 1] := -m.f[0, 1] * D;
  Result.f[1, 0] := -m.f[1, 0] * D;
  Result.f[1, 1] := m.f[0, 0] * D;
end;

function Inv(const m: TMat3): TMat3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var
  D : Single;
begin
  D := 1 / Det(m);
  Result.f[0, 0] := (m.f[1, 1] * m.f[2, 2] - m.f[1, 2] * m.f[2, 1]) * D;
  Result.f[0, 1] := (m.f[2, 1] * m.f[0, 2] - m.f[0, 1] * m.f[2, 2]) * D;
  Result.f[0, 2] := (m.f[0, 1] * m.f[1, 2] - m.f[1, 1] * m.f[0, 2]) * D;
  Result.f[1, 0] := (m.f[1, 2] * m.f[2, 0] - m.f[1, 0] * m.f[2, 2]) * D;
  Result.f[1, 1] := (m.f[0, 0] * m.f[2, 2] - m.f[2, 0] * m.f[0, 2]) * D;
  Result.f[1, 2] := (m.f[1, 0] * m.f[0, 2] - m.f[0, 0] * m.f[1, 2]) * D;
  Result.f[2, 0] := (m.f[1, 0] * m.f[2, 1] - m.f[2, 0] * m.f[1, 1]) * D;
  Result.f[2, 1] := (m.f[2, 0] * m.f[0, 1] - m.f[0, 0] * m.f[2, 1]) * D;
  Result.f[2, 2] := (m.f[0, 0] * m.f[1, 1] - m.f[0, 1] * m.f[1, 0]) * D;
end;

function Inv(const m: TMat4): TMat4; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var
  D : Single;
begin
  D := 1 / Det(m);
  Result.f[0, 0] :=  (m.f[1, 1] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) + m.f[1, 3] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1])) * D;
  Result.f[1, 0] := -(m.f[1, 0] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0])) * D;
  Result.f[2, 0] :=  (m.f[1, 0] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0])) * D;
  Result.f[3, 0] := -(m.f[1, 0] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0]) + m.f[1, 2] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0])) * D;
  Result.f[0, 1] := -(m.f[0, 1] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[0, 2] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) + m.f[0, 3] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1])) * D;
  Result.f[1, 1] :=  (m.f[0, 0] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[0, 2] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[0, 3] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0])) * D;
  Result.f[2, 1] := -(m.f[0, 0] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) - m.f[0, 1] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[0, 3] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0])) * D;
  Result.f[3, 1] :=  (m.f[0, 0] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1]) - m.f[0, 1] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0]) + m.f[0, 2] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0])) * D;
  Result.f[0, 2] :=  (m.f[0, 1] * (m.f[1, 2] * m.f[3, 3] - m.f[1, 3] * m.f[3, 2]) - m.f[0, 2] * (m.f[1, 1] * m.f[3, 3] - m.f[1, 3] * m.f[3, 1]) + m.f[0, 3] * (m.f[1, 1] * m.f[3, 2] - m.f[1, 2] * m.f[3, 1])) * D;
  Result.f[1, 2] := -(m.f[0, 0] * (m.f[1, 2] * m.f[3, 3] - m.f[1, 3] * m.f[3, 2]) - m.f[0, 2] * (m.f[1, 0] * m.f[3, 3] - m.f[1, 3] * m.f[3, 0]) + m.f[0, 3] * (m.f[1, 0] * m.f[3, 2] - m.f[1, 2] * m.f[3, 0])) * D;
  Result.f[2, 2] :=  (m.f[0, 0] * (m.f[1, 1] * m.f[3, 3] - m.f[1, 3] * m.f[3, 1]) - m.f[0, 1] * (m.f[1, 0] * m.f[3, 3] - m.f[1, 3] * m.f[3, 0]) + m.f[0, 3] * (m.f[1, 0] * m.f[3, 1] - m.f[1, 1] * m.f[3, 0])) * D;
  Result.f[3, 2] := -(m.f[0, 0] * (m.f[1, 1] * m.f[3, 2] - m.f[1, 2] * m.f[3, 1]) - m.f[0, 1] * (m.f[1, 0] * m.f[3, 2] - m.f[1, 2] * m.f[3, 0]) + m.f[0, 2] * (m.f[1, 0] * m.f[3, 1] - m.f[1, 1] * m.f[3, 0])) * D;
  Result.f[0, 3] := -(m.f[0, 1] * (m.f[1, 2] * m.f[2, 3] - m.f[1, 3] * m.f[2, 2]) - m.f[0, 2] * (m.f[1, 1] * m.f[2, 3] - m.f[1, 3] * m.f[2, 1]) + m.f[0, 3] * (m.f[1, 1] * m.f[2, 2] - m.f[1, 2] * m.f[2, 1])) * D;
  Result.f[1, 3] :=  (m.f[0, 0] * (m.f[1, 2] * m.f[2, 3] - m.f[1, 3] * m.f[2, 2]) - m.f[0, 2] * (m.f[1, 0] * m.f[2, 3] - m.f[1, 3] * m.f[2, 0]) + m.f[0, 3] * (m.f[1, 0] * m.f[2, 2] - m.f[1, 2] * m.f[2, 0])) * D;
  Result.f[2, 3] := -(m.f[0, 0] * (m.f[1, 1] * m.f[2, 3] - m.f[1, 3] * m.f[2, 1]) - m.f[0, 1] * (m.f[1, 0] * m.f[2, 3] - m.f[1, 3] * m.f[2, 0]) + m.f[0, 3] * (m.f[1, 0] * m.f[2, 1] - m.f[1, 1] * m.f[2, 0])) * D;
  Result.f[3, 3] :=  (m.f[0, 0] * (m.f[1, 1] * m.f[2, 2] - m.f[1, 2] * m.f[2, 1]) - m.f[0, 1] * (m.f[1, 0] * m.f[2, 2] - m.f[1, 2] * m.f[2, 0]) + m.f[0, 2] * (m.f[1, 0] * m.f[2, 1] - m.f[1, 1] * m.f[2, 0])) * D;
end;

{ TRectF }

function TRectF.Center: TVec2;
begin
  Result := (LeftTop + RightBottom) * 0.5;
end;

function TRectF.Size: TVec2;
begin
  Result := RightBottom - LeftTop;
end;

function TRectF.IsEmpty: Boolean;
begin
  Result := (Right<=Left) or (Bottom<=Top);
end;

{ TSegment2D }

function TSegment2D.Line(normalized: Boolean = False): TLine2D;
var v: TVec2;
begin
  v := Pt2 - Pt1;
  Result.Norm := Vec(-v.y, v.x);
  if normalized then Result.Norm := Normalize(Result.Norm);
  Result.Offset := -Dot(Pt1, Result.Norm);
end;

initialization {$undef IMPL}
    SetExceptionMask([exInvalidOp, exDenormalized, exZeroDivide, exOverflow, exUnderflow, exPrecision]);

end.

