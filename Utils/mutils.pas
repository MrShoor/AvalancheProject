unit mutils;
{$I avUtilsConfig.inc}

{$define NoInline}

interface {$define INTF}

uses
  Classes, SysUtils, Math;

const
  EPS = 0.000005;
  HUGE = MaxInt;
  deg2rad = Pi/180;
  rad2deg = 180/Pi;

type
  TSingleArr = array of Single;

{$IfDef FPC}
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

  {$define TCompType := UInt16}
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
{$EndIf}

{$IfDef DCC}
  {$I mutils_v_d_Single.inc}
  {$I mutils_v_d_Integer.inc}
  {$I mutils_v_d_ShortInt.inc}
  {$I mutils_v_d_Byte.inc}
  function Abs(const V: Single): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: Double): Double; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: SmallInt): SmallInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: ShortInt): ShortInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: Integer): Integer; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Trunc(const V: Single): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Trunc(const V: Double): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Round(const V: Single): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Round(const V: Double): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Ceil(const V: Single): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Ceil(const V: Double): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Floor(const V: Single): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Floor(const V: Double): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
{$EndIf}

type

  { TQuat }

  TQuat = record
  {$IfDef DCC}
  public
    class operator Multiply(const a, b: TQuat): TQuat;
    class operator Multiply(const a: TQuat; const b: TVec3): TVec3;
    class operator Multiply(const b: TVec3; const a: TQuat): TVec3;
    class operator Equal(const a, b: TQuat): Boolean;
  {$EndIf}
  case Byte of
    0: (x, y, z, w: Single);
    1: (v: TVec3; a: Single);
    2: (v4: TVec4);
  end;

  { TLine }

  TLine = record
  {$IfDef DCC}
  public
    class operator Multiply(const a: TLine; const b: TMat4): TLine;
  {$EndIf}
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

  TSegment2D = packed record
  public
    Pt1, Pt2: TVec2;
    function Line(normalized: Boolean = False): TLine2D;
  end;
  PSegment2D = ^TSegment2D;

  { TRectF }

  TRectF = record
    function Center  : TVec2;
    function Size    : TVec2;
    function IsEmpty : Boolean;
    function Expand(const ASize: Single) : TRectF; overload;
    function Expand(const ASize: TVec2) : TRectF; overload;
    function PtInRect(const v: TVec2) : Boolean;

    function Point(AIndex: Integer): TVec2;
  {$IfDef DCC}
  public
    class operator Equal(const r1, r2: TRectF): Boolean; overload;
    class operator Add(const r: TRectF; const v: TVec2): TRectF; overload;
    class operator Add(const r1, r2: TRectF): TRectF; overload;
    class operator Multiply(const r: TRectF; const m: TMat3): TRectF;
  {$EndIf}
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
    function PtInRect(const v: TVec2) : Boolean;
    function Expand(const ASize: Integer) : TRectI; overload;
    function Expand(const ASize: TVec2i) : TRectI; overload;
    function Point(AIndex: Integer): TVec2i;
    {$IfDef DCC}
    class operator Add(const r: TRectI; const v: TVec2i): TRectI; overload;
    class operator Add(const r1, r2: TRectI): TRectI; overload;
    class operator Equal(const v1, v2: TRectI): Boolean;
    {$EndIf}
  case Byte of
    0: (Left, Top, Right, Bottom: Integer);
    1: (LeftTop, RightBottom: TVec2i);
    2: (min, max: TVec2i);
    3: (f: array [0..4] of Integer);
    4: (v: TVec4i);
  end;

  { TAABB }

  TAABB = record
    function Center  : TVec3;
    function Size    : TVec3;
    function IsEmpty : Boolean;
    function Point(index: Integer): TVec3;
    function Edge(index: Integer): TLine;

    function InFrustum(const AViewProj: TMat4; const ADepthRange: TVec2): Boolean;
    {$IfDef DCC}
    class operator Add (const AABB: TAABB; const v: TVec3): TAABB; {$IFNDEF NoInline} inline; {$ENDIF}
    class operator Add (const Box1, Box2: TAABB): TAABB; {$IFNDEF NoInline} inline; {$ENDIF}
    class operator Multiply(const AABB: TAABB; const m: TMat4): TAABB; {$IFNDEF NoInline} inline; {$ENDIF}
    class operator Equal (const Box1, Box2: TAABB): boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
  case Byte of
    0: (min, max: TVec3);
  end;

const
  EmptyAABB: TAABB = (min: (x: HUGE; y: HUGE; z: HUGE); max: (x: -HUGE; y: -HUGE; z: -HUGE));
  EmptyRectI: TRectI = (min: (x: HUGE; y: HUGE); max: (x: -HUGE; y: -HUGE));
  EmptyRectF: TRectF = (min: (x: HUGE; y: HUGE); max: (x: -HUGE; y: -HUGE));

type
  { TPlane }

  TPlane = packed record
  case Byte of
    0: (A,B,C,D: Single);
    1: (Norm: TVec3);
  end;

  { TFrustum }

  TFrustum = record
    planes: array [0..5] of TPlane;
    pts   : array [0..7] of TVec3;
    procedure Init(const AViewProjInv: TMat4; const AProjBox: TAABB);
  end;

const
  Black: TVec4 = (x: 0; y: 0; z: 0; w: 0);
  Green: TVec4 = (x: 0; y: 0.5; z: 0; w: 1.0);

  IdentityMat3: TMat3 = (f: ((1,0,0), (0,1,0), (0,0,1)));
  ZeroMat3: TMat3 = (f: ((0,0,0), (0,0,0), (0,0,0)));

  IdentityMat4: TMat4 = (f: ((1,0,0,0), (0,1,0,0), (0,0,1,0), (0,0,0,1)));
  ZeroMat4: TMat4 = (f: ((0,0,0,0), (0,0,0,0), (0,0,0,0), (0,0,0,0)));

{$IfDef FPC}
Operator := (const v: TVec2B)v2: TVec2I; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec3B)v2: TVec3I; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec4B)v2: TVec4I; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec2B)v2: TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec3B)v2: TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
Operator := (const v: TVec4B)v2: TVec4; {$IFNDEF NoInline} inline; {$ENDIF}

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
Operator * (const a: TQuat; const b: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
Operator * (const b: TVec3; const a: TQuat): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
Operator * (const a: TVec3I; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
Operator * (const a: TVec2b; s: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
Operator * (const a: TLine; const b: TMat4): TLine; {$IFNDEF NoInline} inline; {$ENDIF}
{$EndIf}

function Quat(const Dir: TVec3; Angle: Single): TQuat; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Quat(x,y,z,w: Single): TQuat; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function Mat2(const Angle: Single): TMat2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat3(const Angle: Single): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat3(const Angle: Single; newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat3(const Scale: TVec2; Angle: Single; newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat3(const Scale: TVec2; const XDir: TVec2; const newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat4(const Q: TQuat): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat4(const Q: TQuat; newPos: TVec3): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function MatTranslate(const newPos: TVec3): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function MatScale(const Scale: TVec3): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat3Scale(const Scale: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Mat3Translate(const newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function RectF(Left, Top, Right, Bottom: Single): TRectF; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function RectF(const AMin, AMax: TVec2): TRectF; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function RectI(Left, Top, Right, Bottom: Integer): TRectI; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function RectI(LeftTop, RightBottom: TVec2I): TRectI; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Plane(const normal, point: TVec3): TPlane; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Plane(A,B,C,D: single): TPlane; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Plane(const pt1, pt2, pt3: TVec3): TPlane; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Line(const APt, ADir: TVec3): TLine; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function AABB(const AMin, AMax: TVec3): TAABB; overload;
function Line2D(const APt1, APt2: TVec2): TLine2D; {$IFNDEF NoInline} inline; {$ENDIF}
function Line2D_normalized(const APt1, APt2: TVec2): TLine2D; {$IFNDEF NoInline} inline; {$ENDIF}

function Pow(const v: TVec2; const s: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Pow(const v: TVec3; const s: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Pow(const v: TVec4; const s: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Len(const v: TVec2): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Len(const v: TVec3): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Len(const v: TVec4): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Normalize(const v: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Normalize(const v: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Normalize(const v: TVec4): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Normalize(const v: TQuat): TQuat; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function NormalizeSafe(const v: TVec2; const def: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function NormalizeSafe(const v: TVec3; const def: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function NormalizeWeights(const v: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function NormalizeWeights(const v: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function NormalizeWeights(const v: TVec4): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function SetLen(const v: TVec2; newLen: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function SetLen(const v: TVec3; newLen: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function SetLen(const v: TVec4; newLen: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: Single; s: Single): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: TVec2; s: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: TVec2; s: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: TVec3; s: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: TVec3; s: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: TVec4; s: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const v1, v2: TVec4; s: TVec4): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const m1, m2: TMat2; s: Single): TMat2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const m1, m2: TMat3; s: Single): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Lerp(const m1, m2: TMat4; s: Single): TMat4; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function Rotate(const v: TVec2; const Angle: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Rotate(const v: TVec2; const ASin, ACos: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function Rotate90(const v: TVec2; const CW: Boolean): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}

function Intersect(const Line1, Line2: TLine2D): TVec2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Intersect(const Seg: TSegment2D; const Line: TLine2D; out IntPoint: TVec2): Boolean; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Intersect(const RayOrig, RayDir: TVec3; const AABB: TAABB; out T: Single; SolidAABB, AllowBackfaces: Boolean): Boolean; overload;
function Intersect(const p1, p2, p3: TVec3; const RayOrig, RayDir: TVec3; out T: Single; out UVW: TVec3; FiniteRay: Boolean = False): Boolean; overload;
function Intersect(const Plane: TPlane; Line: TLine; out IntPt: TVec3): Boolean; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Intersect(const R1: TRectF; const R2: TRectF): Boolean; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Intersect(const R1: TRectI; const R2: TRectI): Boolean; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Intersect(const B1, B2: TAABB): Boolean; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Distance(const Pt: TVec2; const Seg: TSegment2D): Single; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Distance(const Pt: TVec2; const Seg: TSegment2D; out AClosestPt: TVec2): Single; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Projection(const Pt: TVec2; const Line: TLine2D): TVec2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Projection(const Cast: TVec3; const Recieve: TVec3): TVec3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Projection(const Pt: TVec3; const Line: TLine): TVec3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Projection(const Pt: TVec3; const Plane: TPlane): TVec3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Inv(const m: TMat2): TMat2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Inv(const m: TMat3): TMat3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Inv(const m: TMat4): TMat4; overload;{$IFNDEF NoInline} inline; {$ENDIF}
function Inv(const q: TQuat): TQuat; overload;{$IFNDEF NoInline} inline; {$ENDIF}

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
function Log2Int(v: Integer): Integer; overload;
function Log2Int(v: UInt64): Integer; overload;
function BitsCount(x: UInt64): Integer; overload;
function GetMipsCount(Width, Height: Integer): Integer; overload;
function GetMipsCount(Width, Height, Deep: Integer): Integer; overload;

function Bezier2(const pt1, pt2, pt3: TVec2; t: single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
procedure Bezier2Split(const pt1, pt2, pt3: TVec2; t: single; out lpt1, lpt2, lpt3, rpt1, rpt2, rpt3: TVec2); overload;
function Bezier2IsLine(const pt1, pt2, pt3: TVec2; ATolerance: Single): Boolean; overload;

function Bezier3(const pt1, pt2, pt3, pt4: TVec2; t: single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
function DistanceToBezier3(const APt: TVec2; const pt1, pt2, pt3, pt4: TVec2): Single; overload;
function DistanceToBezier3(const APt: TVec2; const pt1, pt2, pt3, pt4: TVec2; out ClosestPt: TVec2; out T: Single): Single; overload;
procedure Bezier3Split(const pt1, pt2, pt3, pt4: TVec2; t: single; out lpt1, lpt2, lpt3, lpt4, rpt1, rpt2, rpt3, rpt4: TVec2); overload;
function Bezier3IsLine(const pt1, pt2, pt3, pt4: TVec2; ATolerance: Single): Boolean; overload;

function SetViewMatrix(var MatrixView: TMat4; const From, At, Worldup: TVec3; leftHanded: boolean = true): HResult;
function GetUIMatrix(const AWidth, AHeight: Single; const ADepth: Single = 10000): TMat4;
function GetUIMat3(const AWidth, AHeight: Single): TMat3; overload;
function GetUIMat3(const ASize: TVec2i): TMat3; overload;

function ToStr(const v: Single): string; overload;
function ToStr(const v: TVec2): string; overload;
function ToStr(const v: TVec3): string; overload;
function ToStr(const v: TVec4): string; overload;

function NormalizeAngle(angle: single): single;
function ShortestRotation(from: single; at: single): single;

function VecSinCos(const Angle: Single): TVec2;

function FromToQuat(const AFrom, ATo: TVec3): TQuat;
function FromToMat(AFrom, ATo: TVec3): TMat4;

function RandomSphereUniformRay(): TVec3;
function HammersleyPoint(const I, N: Integer): TVec2;
function GenerateHammersleyPts(const N: Integer): TVec4Arr;
function WeightedRandom(weights: array of Integer): Integer;

function sRGBToLinear(const rgb: TVec3): TVec3;
function sRGB255ToLinear(r, g, b: Byte): TVec3; overload;
function sRGB255ToLinear(rgb: TVec3b): TVec3; overload;
function LinearTosRGB(const linear: TVec3): TVec3;

{$IfDef FPC}
operator = (const v1, v2: TRectF): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
operator = (const v1, v2: TRectI): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
operator = (const b1, b2: TAABB): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
operator = (const q1, q2: TQuat): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}

operator + (const r: TRectF; const v: TVec2): TRectF; {$IFNDEF NoInline} inline; {$ENDIF}
operator + (const r1, r2: TRectF): TRectF; {$IFNDEF NoInline} inline; {$ENDIF}
operator * (const r: TRectF; const m: TMat3): TRectF; {$IFNDEF NoInline} inline; {$ENDIF}

operator + (const b: TRectI; v: TVec2i): TRectI; {$IFNDEF NoInline} inline; {$ENDIF}
operator + (const b1, b2: TRectI): TRectI; {$IFNDEF NoInline} inline; {$ENDIF}

operator + (const AABB: TAABB; v: TVec3): TAABB; {$IFNDEF NoInline} inline; {$ENDIF}
operator + (const Box1, Box2: TAABB): TAABB; {$IFNDEF NoInline} inline; {$ENDIF}
operator * (const AABB: TAABB; const m: TMat4): TAABB; {$IFNDEF NoInline} inline; {$ENDIF}

operator * (const v: TVec2i; s: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
{$EndIf}

implementation {$undef INTF} {$define IMPL}

{$IfDef DCC}

{$EndIf}

function RandomSphereUniformRay(): TVec3;
var theta, cosphi, sinphi: Single;
begin
  theta := 2 * Pi * Random;
  cosphi := 1 - 2 * Random;
  sinphi := sqrt(1 - min(1.0, sqr(cosphi)));
  Result.x := sinphi * cos(theta);
  Result.y := sinphi * sin(theta);
  Result.z := cosphi;
end;

function HammersleyPoint(const I, N: Integer): TVec2;
  function radicalInverse_VdC(bits: Cardinal): Single;
  begin
    bits := (bits shl 16) or (bits shr 16);
    bits := ((bits and $55555555) shl 1) or ((bits and $AAAAAAAA) shr 1);
    bits := ((bits and $33333333) shl 2) or ((bits and $CCCCCCCC) shr 2);
    bits := ((bits and $0F0F0F0F) shl 4) or ((bits and $F0F0F0F0) shr 4);
    bits := ((bits and $00FF00FF) shl 8) or ((bits and $FF00FF00) shr 8);
    Result := bits * 2.3283064365386963e-10;
  end;
begin
  Result.x := I/N;
  Result.y := radicalInverse_VdC(I);
end;

function GenerateHammersleyPts(const N: Integer): TVec4Arr;
var E: TVec2;
    I: Integer;
    offset: TVec2;
begin
  offset.x := Random/N;
  offset.y := Random/N;

  SetLength(Result, N);
  for I := 0 to N-1 do
  begin
    E := HammersleyPoint(I, N);
    E := E + offset;
    if E.x > 1.0 then
      E.x := E.x - 1.0;
    if E.y > 1.0 then
      E.y := E.y - 1.0;
    Result[i].xy := E;
    Result[i].z := 0;
    Result[i].w := 0;
  end;
end;

function WeightedRandom(weights: array of Integer): Integer;
var i, n: Integer;
    summ: Integer;
begin
  summ := 0;
  for i := 0 to Length(weights) - 1 do Inc(summ, weights[i]);
  n := Random(summ);
  Result := 0;
  while n >= weights[Result] do
  begin
    n := n - weights[Result];
    Inc(Result);
  end;
end;

function sRGBToLinear(const rgb: TVec3): TVec3;
begin
  Result := Pow(rgb, 2.2);
end;

function sRGB255ToLinear(r, g, b: Byte): TVec3;
begin
  Result := sRGBToLinear(Vec(r/255,g/255,b/255));
end;

function sRGB255ToLinear(rgb: TVec3b): TVec3;
begin
  Result := sRGB255ToLinear(rgb.x, rgb.y, rgb.z);
end;

function LinearTosRGB(const linear: TVec3): TVec3;
begin
  Result := Pow(linear, 1.0/2.2);
end;

type
  TDoubleArr = array of Double;

function EvalN(const Params: TDoubleArr; X: Double): Double;
var i: Integer;
    tmpX: Double;
begin
  Result := 0;
  tmpX := 1;
  for i := Length(Params) - 1 downto 0 do
  begin
    Result := Result + Params[i]*tmpX;
    tmpX := tmpX * X;
  end;
end;

function GetDerivativeN(const Params: TDoubleArr): TDoubleArr;
var
  i: Integer;
begin
  SetLength(Result, Length(Params)-1);
  for i := 0 to Length(Params)-2 do
    Result[i] := (Length(Params)-i-1)*Params[i];
end;

function SolveN(const Params: TDoubleArr; const Steps: Integer; const EPS: Double): TDoubleArr;
const XEPS: Double = 0.0001;
  function MergeRoots(const Roots1, Roots2: TDoubleArr): TDoubleArr;
  var i, j, k, realLen: Integer;
      R1, R2: Double;
  begin
    if Roots1 = nil then Exit(Roots2);
    if Roots2 = nil then Exit(Roots1);

    SetLength(Result, Length(Roots1) + Length(Roots2));
    realLen := 0;
    i := 0;
    j := 0;
    for k := 0 to Length(Result) - 1 do
    begin
      if i = Length(Roots1) then
        R1 := Math.Infinity
      else
        R1 := Roots1[i];
      if j = Length(Roots2) then
        R2 := Math.Infinity
      else
        R2 := Roots2[j];

      if R1 < R2 then
      begin
        if (realLen=0) or (Result[realLen-1] <> R1) then
        begin
          Result[realLen] := R1;
          Inc(realLen);
        end;
        Inc(i);
      end
      else
      begin
        if (realLen=0) or (Result[realLen-1] <> R2) then
        begin
          Result[realLen] := R2;
          Inc(realLen);
        end;
        Inc(j);
      end;
    end;
    if Length(Result) <> realLen then SetLength(Result, realLen);
  end;

  procedure ResolveRange(var x1, x2: Double; var f1, f2: Double; out newX, newF: Double); //binary search
  begin
    newX := (x2+x1)*0.5;
    newF := EvalN(Params, newX);

    if Sign(NewF) = Sign(f1) then
    begin
      x1 := NewX;
      f1 := NewF;
    end
    else
    begin
      x2 := NewX;
      f2 := NewF;
    end;
  end;

  function FindOtherRange(const deriv: TDoubleArr; const x1, f1, startOffset: Double; out x2, f2: Double): Boolean;
  //var Fd: Double;
  begin
    //todo use derivative for find exact interval
    x2 := x1 + startOffset;
    f2 := EvalN(Params, x2);
    if sign(f2) <> sign(f1) then Exit(True);
    Exit(False);
    //if Sign(f2) = Sign(EvalN(deriv, x2)) then Exit(False);
    //
    //while sign(f2) = sign(f1) do
    //begin
    //  Fd := EvalN(deriv, x2);
    //  if (Fd = 0) then Break;
    //  x2 := x2 - f2 / Fd;
    //  f2 := EvalN(Params, x2);
    //end;
    //Result := True;
  end;

var deriv: TDoubleArr;
    derivRoots: TDoubleArr;
    i, j: Integer;

    //X, Fx, Fd: Double;
    RangeX: array [0..1] of Double;
    RangeF: array [0..1] of Double;
    NewX, NewF: Double;
begin
  Result := nil;
  if Length(Params) = 1 then Exit;
  if Length(Params) = 2 then
  begin
    if Params[0] <> 0 then
    begin
      SetLength(Result, 1);
      Result[0] := -Params[1]/Params[0];
    end;
    Exit;
  end;

  deriv := GetDerivativeN(Params);
  //derivRoots := MergeRoots(SolveN(deriv, Steps, EPS), SolveN( GetDerivativeN(deriv), Steps, EPS ));
  derivRoots := SolveN(deriv, Steps, EPS);
  if derivRoots = nil then
  begin
    SetLength(derivRoots, 1);
    derivRoots[0] := 0.5;
  end;

  for j := 0 to Length(derivRoots) do
  begin
    if j = 0 then
    begin
      RangeX[1] := derivRoots[0];
      RangeF[1] := EvalN(Params, RangeX[1]);
      if not FindOtherRange(deriv, RangeX[1], RangeF[1], -10.0, RangeX[0], RangeF[0]) then Continue; //no roots at this interval
    end
    else
      if j = Length(derivRoots) then
      begin
        RangeX[0] := derivRoots[j-1];
        RangeF[0] := EvalN(Params, RangeX[0]);
        if not FindOtherRange(deriv, RangeX[0], RangeF[0], +10.0, RangeX[1], RangeF[1]) then Continue; //no roots at this interval
      end
      else
      begin
        RangeX[0] := derivRoots[j-1];
        RangeX[1] := derivRoots[j];
        RangeF[0] := EvalN(Params, RangeX[0]);
        RangeF[1] := EvalN(Params, RangeX[1]);
        if Sign(RangeF[0]) = Sign(RangeF[1]) then Continue; //no roots at this interval
      end;

    for i := 0 to Steps - 1 do
    begin
      ResolveRange(RangeX[0], RangeX[1], RangeF[0], RangeF[1], NewX, NewF);

//      if (abs(NewF)<EPS) then
      if (abs(RangeX[1]-RangeX[0])<XEPS) then //root found
      begin
        if Length(Result) > 0 then
          if NewX - Result[High(Result)] < XEPS then Break; //current root already exists, skip it
        SetLength(Result, Length(Result)+1);
        Result[High(Result)] := NewX;
        Break;
      end;
    end;
  end;
end;

{ TFrustum }

procedure TFrustum.Init(const AViewProjInv: TMat4; const AProjBox: TAABB);

  function Unproject(const v: TVec3): TVec3;
  var tmp: TVec4;
  begin
    tmp := Vec(v, 1.0) * AViewProjInv;
    Result := tmp.xyz / tmp.w;
  end;

begin
  pts[0] := Unproject(AProjBox.min);
  pts[1] := Unproject(Vec(AProjBox.min.x, AProjBox.max.y, AProjBox.min.z));
  pts[2] := Unproject(Vec(AProjBox.max.x, AProjBox.min.y, AProjBox.min.z));
  pts[3] := Unproject(Vec(AProjBox.min.x, AProjBox.max.y, AProjBox.max.z));
  pts[4] := Unproject(Vec(AProjBox.max.x, AProjBox.min.y, AProjBox.max.z));
  pts[5] := Unproject(AProjBox.max);
  pts[6] := Unproject(Vec(AProjBox.max.x, AProjBox.max.y, AProjBox.min.z));
  pts[7] := Unproject(Vec(AProjBox.min.x, AProjBox.min.y, AProjBox.max.z));

  planes[0].Norm := -normalize(cross(pts[2]-pts[0], pts[1]-pts[0])); //near
  planes[1].Norm := -normalize(cross(pts[4]-pts[5], pts[3]-pts[5])); //far
  planes[2].Norm := -normalize(cross(pts[5]-pts[4], pts[2]-pts[4])); //right
  planes[3].Norm := -normalize(cross(pts[3]-pts[1], pts[0]-pts[1])); //left
  planes[4].Norm := -normalize(cross(pts[1]-pts[3], pts[5]-pts[3])); //top
  planes[5].Norm := -normalize(cross(pts[0]-pts[2], pts[4]-pts[2])); //bottom

  planes[0].D := -dot(planes[0].Norm, pts[0]); //near
  planes[1].D := -dot(planes[1].Norm, pts[5]); //far
  planes[2].D := -dot(planes[2].Norm, pts[4]); //right
  planes[3].D := -dot(planes[3].Norm, pts[1]); //left
  planes[4].D := -dot(planes[4].Norm, pts[3]); //top
  planes[5].D := -dot(planes[5].Norm, pts[2]); //bottom
end;

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
  case abs(index mod 8) of
    0: Result := Vec(min.x, min.y, min.z);
    1: Result := Vec(min.x, max.y, min.z);
    2: Result := Vec(max.x, max.y, min.z);
    3: Result := Vec(max.x, min.y, min.z);
    4: Result := Vec(min.x, min.y, max.z);
    5: Result := Vec(min.x, max.y, max.z);
    6: Result := Vec(max.x, max.y, max.z);
    7: Result := Vec(max.x, min.y, max.z);
  else
    Result := Vec(0,0,0);
  end;
end;

function TAABB.Edge(index: Integer): TLine;
begin
  case abs(index mod 12) of
    0: begin
         Result.Pnt := Vec(min.x, min.y, min.z);
         Result.Dir := Vec(min.x, max.y, min.z) - Result.Pnt;
       end;
    1: begin
         Result.Pnt := Vec(min.x, max.y, min.z);
         Result.Dir := Vec(max.x, max.y, min.z) - Result.Pnt;
       end;
    2: begin
         Result.Pnt := Vec(max.x, max.y, min.z);
         Result.Dir := Vec(max.x, min.y, min.z) - Result.Pnt;
       end;
    3: begin
         Result.Pnt := Vec(max.x, min.y, min.z);
         Result.Dir := Vec(min.x, min.y, min.z) - Result.Pnt;
       end;
    4: begin
         Result.Pnt := Vec(min.x, min.y, max.z);
         Result.Dir := Vec(min.x, max.y, max.z) - Result.Pnt;
       end;
    5: begin
         Result.Pnt := Vec(min.x, max.y, max.z);
         Result.Dir := Vec(max.x, max.y, max.z) - Result.Pnt;
       end;
    6: begin
         Result.Pnt := Vec(max.x, max.y, max.z);
         Result.Dir := Vec(max.x, min.y, max.z) - Result.Pnt;
       end;
    7: begin
         Result.Pnt := Vec(max.x, min.y, max.z);
         Result.Dir := Vec(min.x, min.y, max.z) - Result.Pnt;
       end;

    8: begin
         Result.Pnt := Vec(min.x, min.y, min.z);
         Result.Dir := Vec(min.x, min.y, max.z) - Result.Pnt;
       end;
    9: begin
         Result.Pnt := Vec(min.x, max.y, min.z);
         Result.Dir := Vec(min.x, max.y, max.z) - Result.Pnt;
       end;
    10: begin
         Result.Pnt := Vec(max.x, max.y, min.z);
         Result.Dir := Vec(max.x, max.y, max.z) - Result.Pnt;
        end;
    11: begin
         Result.Pnt := Vec(max.x, min.y, min.z);
         Result.Dir := Vec(max.x, min.y, max.z) - Result.Pnt;
        end;
  else
    Result.Dir := Vec(0,0,0);
    Result.Pnt := Vec(0,0,0);
  end;
end;

function TAABB.InFrustum(const AViewProj: TMat4; const ADepthRange: TVec2): Boolean;
var
  i: Integer;
  pts: array [0..7] of TVec4;
begin
  for i := 0 to 7 do
    pts[i] := Vec(Point(i), 1.0) * AViewProj;

  Result := False;

  i := 0;
  while (i < 8) and (pts[i].x < -abs(pts[i].w)) do inc(i);
  if i = 8 then exit;

  i := 0;
  while (i < 8) and (pts[i].x >  abs(pts[i].w)) do inc(i);
  if i = 8 then exit;

  i := 0;
  while (i < 8) and (pts[i].y < -abs(pts[i].w)) do inc(i);
  if i = 8 then exit;

  i := 0;
  while (i < 8) and (pts[i].y >  abs(pts[i].w)) do inc(i);
  if i = 8 then exit;

  if ADepthRange.y < ADepthRange.x then
  begin
    i := 0;
    while (i < 8) and (pts[i].z > pts[i].w*ADepthRange.x) do inc(i);
    if i = 8 then exit;

    i := 0;
    while (i < 8) and (pts[i].z < pts[i].w*ADepthRange.y) do inc(i);
    if i = 8 then exit;
  end
  else
  begin
    i := 0;
    while (i < 8) and (pts[i].z < pts[i].w*ADepthRange.x) do inc(i);
    if i = 8 then exit;

    i := 0;
    while (i < 8) and (pts[i].z > pts[i].w*ADepthRange.y) do inc(i);
    if i = 8 then exit;
  end;

  Result := True;
end;

{$IfDef DCC}
class operator TAABB.Add (const AABB: TAABB; const v: TVec3): TAABB;
begin
  Result.min := mutils.Min(AABB.min, v);
  Result.max := mutils.Max(AABB.max, v);
end;

class operator TAABB.Add (const Box1, Box2: TAABB): TAABB;
begin
  Result.min := mutils.Min(Box1.min, Box2.min);
  Result.max := mutils.Max(Box1.max, Box2.max);
end;

class operator TAABB.Multiply(const AABB: TAABB; const m: TMat4): TAABB;
var i: Integer;
begin
  Result := EmptyAABB;
  for i := 0 to 7 do
    Result := Result + AABB.Point(i) * m;
end;

class operator TAABB.Equal(const Box1, Box2: TAABB): boolean;
begin
  Result := (Box1.min = Box2.min) and (Box1.max = Box2.max);
end;
{$EndIf}

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

function TRectI.PtInRect(const v: TVec2): Boolean;
begin
  Result := (v.x >= min.x) and (v.y >= min.y) and
            (v.x < max.x) and (v.y < max.y);
end;

function TRectI.Expand(const ASize: Integer): TRectI;
begin
  Result.min.x := min.x - ASize;
  Result.min.y := min.y - ASize;
  Result.max.x := max.x + ASize;
  Result.max.y := max.y + ASize;
end;

function TRectI.Expand(const ASize: TVec2i): TRectI;
begin
  Result.min.x := min.x - ASize.x;
  Result.min.y := min.y - ASize.y;
  Result.max.x := max.x + ASize.x;
  Result.max.y := max.y + ASize.y;
end;

function TRectI.Point(AIndex: Integer): TVec2i;
begin
  case AIndex mod 4 of
    0: Result := min;
    1: Result := Vec(min.x, max.y);
    2: Result := max;
    3: Result := Vec(max.x, min.y);
  else
    Result := Vec(0,0);
  end;
end;

{$IfDef DCC}
class operator TRectI.Add(const r: TRectI; const v: TVec2i): TRectI;
begin
  Result.min := mutils.Min(r.min, v);
  Result.max := mutils.Max(r.max, v);
end;

class operator TRectI.Add(const r1, r2: TRectI): TRectI;
begin
  Result.min := mutils.Min(r1.min, r2.min);
  Result.max := mutils.Max(r1.max, r2.max);
end;

class operator TRectI.Equal(const v1, v2: TRectI): Boolean;
begin
  Result := (v1.Left = v2.Left) and (v1.Top = v2.Top) and (v1.Right = v2.Right) and (v1.Bottom = v2.Bottom);
end;
{$EndIf}

{$IfDef FPC}
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

  {$define TCompType := UInt16}
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
{$EndIf}

{$IfDef DCC}
  {$I mutils_v_d_Single.inc}
  {$I mutils_v_d_Integer.inc}
  {$I mutils_v_d_ShortInt.inc}
  {$I mutils_v_d_Byte.inc}

  function Abs(const V: Single): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Abs(V);
  end;
  function Abs(const V: Double): Double; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Abs(V);
  end;
  function Abs(const V: SmallInt): SmallInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Abs(V);
  end;
  function Abs(const V: ShortInt): ShortInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Abs(V);
  end;
  function Abs(const V: Integer): Integer; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Abs(V);
  end;

  function Trunc(const V: Single): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Trunc(V);
  end;
  function Trunc(const V: Double): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Trunc(V);
  end;
  function Round(const V: Single): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Round(V);
  end;
  function Round(const V: Double): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Round(V);
  end;
  function Ceil(const V: Single): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Trunc(V);
    if Frac(V) > 0 then
      Inc(Result);
  end;
  function Ceil(const V: Double): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Trunc(V);
    if Frac(V) > 0 then
      Inc(Result);
  end;
  function Floor(const V: Single): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Trunc(V);
    if Frac(V) < 0 then
      Dec(Result);
  end;
  function Floor(const V: Double): Int64; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := System.Trunc(V);
    if Frac(V) < 0 then
      Dec(Result);
  end;
{$EndIf}

{$IfDef FPC}
operator := (const v: TVec2B)v2: TVec2I;
begin
  v2.x := v.x;
  v2.y := v.y;
end;

operator := (const v: TVec3B)v2: TVec3I;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
end;

operator := (const v: TVec4B)v2: TVec4I;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
  v2.w := v.w;
end;

operator := (const v: TVec2B)v2: TVec2;
begin
  v2.x := v.x;
  v2.y := v.y;
end;

operator := (const v: TVec3B)v2: TVec3;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
end;

operator := (const v: TVec4B)v2: TVec4;
begin
  v2.x := v.x;
  v2.y := v.y;
  v2.z := v.z;
  v2.w := v.w;
end;

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

Operator * (const a: TQuat; const b: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x:=b.x-2*b.x*(sqr(a.y)+sqr(a.z))+2*b.y*(a.x*a.y+a.z*a.w)+2*b.z*(a.x*a.z-a.y*a.w);
  Result.y:=2*b.x*(a.x*a.y-a.z*a.w)+b.y-2*b.y*(sqr(a.x)+sqr(a.z))+2*b.z*(a.y*a.z+a.x*a.w);
  Result.z:=2*b.x*(a.x*a.z+a.y*a.w)+2*b.y*(a.y*a.z-a.x*a.w)+b.z-2*b.z*(sqr(a.x)+sqr(a.y));
end;

Operator * (const b: TVec3; const a: TQuat): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x:=b.x-2*b.x*(sqr(a.y)+sqr(a.z))+2*b.y*(a.x*a.y-a.z*a.w)+2*b.z*(a.x*a.z+a.y*a.w);
  Result.y:=2*b.x*(a.x*a.y+a.z*a.w)+b.y-2*b.y*(sqr(a.x)+sqr(a.z))+2*b.z*(a.y*a.z-a.x*a.w);
  Result.z:=2*b.x*(a.x*a.z-a.y*a.w)+2*b.y*(a.y*a.z+a.x*a.w)+b.z-2*b.z*(sqr(a.x)+sqr(a.y));
end;

Operator * (const a: TVec3I; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := a.x*s;
  Result.y := a.y*s;
  Result.z := a.z*s;
end;

operator * (const a: TVec2b; s: Single): TVec2;
begin
  Result.x := a.x * s;
  Result.y := a.y * s;
end;

operator*(const a: TLine; const b: TMat4): TLine;
begin
  Result.Pnt := a.Pnt * b;
  Result.Dir := (a.Pnt + a.Dir) * b - Result.Pnt;
end;
{$EndIf}

{$IfDef DCC}
class operator TQuat.Multiply(const a, b: TQuat): TQuat;
begin
  Result.w:=a.w*b.w-a.x*b.x-a.y*b.y-a.z*b.z;
  Result.x:=a.w*b.x+a.x*b.w+a.y*b.z-a.z*b.y;
  Result.y:=a.w*b.y+a.y*b.w+a.z*b.x-a.x*b.z;
  Result.z:=a.w*b.z+a.z*b.w+a.x*b.y-a.y*b.x;
end;

class operator TQuat.Multiply(const a: TQuat; const b: TVec3): TVec3;
begin
  Result.x:=b.x-2*b.x*(sqr(a.y)+sqr(a.z))+2*b.y*(a.x*a.y+a.z*a.w)+2*b.z*(a.x*a.z-a.y*a.w);
  Result.y:=2*b.x*(a.x*a.y-a.z*a.w)+b.y-2*b.y*(sqr(a.x)+sqr(a.z))+2*b.z*(a.y*a.z+a.x*a.w);
  Result.z:=2*b.x*(a.x*a.z+a.y*a.w)+2*b.y*(a.y*a.z-a.x*a.w)+b.z-2*b.z*(sqr(a.x)+sqr(a.y));
end;

class operator TQuat.Multiply(const b: TVec3; const a: TQuat): TVec3;
begin
  Result.x:=b.x-2*b.x*(sqr(a.y)+sqr(a.z))+2*b.y*(a.x*a.y-a.z*a.w)+2*b.z*(a.x*a.z+a.y*a.w);
  Result.y:=2*b.x*(a.x*a.y+a.z*a.w)+b.y-2*b.y*(sqr(a.x)+sqr(a.z))+2*b.z*(a.y*a.z-a.x*a.w);
  Result.z:=2*b.x*(a.x*a.z-a.y*a.w)+2*b.y*(a.y*a.z+a.x*a.w)+b.z-2*b.z*(sqr(a.x)+sqr(a.y));
end;

class operator TQuat.Equal(const a, b: TQuat): Boolean;
begin
  Result := a.v4 = b.v4;
end;

class operator TLine.Multiply(const a: TLine; const b: TMat4): TLine;
var p2: TVec3;
begin
  p2 := (a.Pnt + a.Dir) * b;
  Result.Pnt := a.Pnt * b;
  Result.Dir := p2 - Result.Pnt;
end;
{$EndIf}

function Quat(const Dir: TVec3; Angle: Single): TQuat; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.v := Dir * sin(0.5 * angle);
  Result.a := cos(0.5 * angle);
end;

function Quat(x,y,z,w: Single): TQuat; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function Mat2(const Angle: Single): TMat2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
var sn, cs: Extended;
begin
  sincos(Angle, sn, cs);
  Result.Row[0] := Vec(cs,  sn);
  Result.Row[1] := Vec(-sn, cs);
end;

function Mat3(const Angle: Single): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
var sn, cs: Extended;
begin
  sincos(Angle, sn, cs);
  Result.OX := Vec(cs,  sn);
  Result.OY := Vec(-sn, cs);
  Result.Pos := Vec(0, 0);
  Result.Col[2] := Vec(0,0,1);
end;

function Mat3(const Angle: Single; newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
var sn, cs: Extended;
begin
  sincos(Angle, sn, cs);
  Result.OX := Vec(cs,  sn);
  Result.OY := Vec(-sn, cs);
  Result.Pos := Vec(newPos.x, newPos.y);
  Result.Col[2] := Vec(0,0,1);
end;

function Mat3(const Scale: TVec2; Angle: Single; newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
var sn, cs: Extended;
begin
  sincos(Angle, sn, cs);
  Result.OX := Vec(cs,  sn) * Scale.x;
  Result.OY := Vec(-sn, cs) * Scale.y;
  Result.Pos := Vec(newPos.x, newPos.y);
  Result.Col[2] := Vec(0,0,1);
end;

function Mat3(const Scale: TVec2; const XDir: TVec2; const newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.OX := Vec(XDir.x,  XDir.y) * Scale.x;
  Result.OY := Vec(-XDir.y, XDir.x) * Scale.y;
  Result.Pos := Vec(newPos.x, newPos.y);
  Result.Col[2] := Vec(0,0,1);
end;

function Mat4(const Q: TQuat): TMat4;
begin
  Result := Mat4(Q, Vec(0,0,0));
end;

function Mat4(const Q: TQuat; newPos: TVec3): TMat4;
begin
  Result.f[0][0] := 1 - 2*Q.y*Q.y - 2*Q.z*Q.z;
  Result.f[0][1] :=     2*Q.x*Q.y + 2*Q.z*Q.w;
  Result.f[0][2] :=     2*Q.x*Q.z - 2*Q.y*Q.w;
  Result.f[0][3] := 0;

  Result.f[1][0] :=     2*Q.x*Q.y - 2*Q.z*Q.w;
  Result.f[1][1] := 1 - 2*Q.x*Q.x - 2*Q.z*Q.z;
  Result.f[1][2] :=     2*Q.y*Q.z + 2*Q.x*Q.w;
  Result.f[1][3] := 0;

  Result.f[2][0] :=     2*Q.x*Q.z + 2*Q.y*Q.w;
  Result.f[2][1] :=     2*Q.y*Q.z - 2*Q.x*Q.w;
  Result.f[2][2] := 1 - 2*Q.x*Q.x - 2*Q.y*Q.y;
  Result.f[2][3] := 0;

  Result.Row[3].xyz := newPos;
  Result.Row[3].w := 1;
end;

function MatTranslate(const newPos: TVec3): TMat4;
begin
  Result := IdentityMat4;
  Result.Pos := newPos;
end;

function MatScale(const Scale: TVec3): TMat4;
begin
  Result := IdentityMat4;
  Result.f[0,0] := Scale.x;
  Result.f[1,1] := Scale.y;
  Result.f[2,2] := Scale.z;
end;

function Mat3Scale(const Scale: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := IdentityMat3;
  Result.f[0,0] := Scale.x;
  Result.f[1,1] := Scale.y;
end;

function Mat3Translate(const newPos: TVec2): TMat3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := IdentityMat3;
  Result.Pos := newPos;
end;

function RectF(Left, Top, Right, Bottom: Single): TRectF; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Left := Left;
  Result.Right := Right;
  Result.Top := Top;
  Result.Bottom := Bottom;
end;

function RectF(const AMin, AMax: TVec2): TRectF;
begin
  Result.min := AMin;
  Result.max := AMax;
end;

function RectI(Left, Top, Right, Bottom: Integer): TRectI; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Left := Left;
  Result.Right := Right;
  Result.Top := Top;
  Result.Bottom := Bottom;
end;

function RectI(LeftTop, RightBottom: TVec2I): TRectI; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.LeftTop := LeftTop;
  Result.RightBottom := RightBottom;
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

function Line(const APt, ADir: TVec3): TLine;
begin
  Result.Pnt := APt;
  Result.Dir := ADir;
end;

function AABB(const AMin, AMax: TVec3): TAABB;
begin
  Result.min := AMin;
  Result.max := AMax;
end;

function Line2D(const APt1, APt2: TVec2): TLine2D; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.Norm.x := APt1.y + APt2.y;
  Result.Norm.y := APt2.x - APt1.x;
  Result.Offset := - Result.Norm.x*APt1.x - Result.Norm.y*APt1.y;
end;

function Line2D_normalized(const APt1, APt2: TVec2): TLine2D; {$IFNDEF NoInline} inline; {$ENDIF}
var nLen: Single;
begin
  Result.Norm.x := APt1.y - APt2.y;
  Result.Norm.y := APt2.x - APt1.x;
  nLen := 1.0/Len(Result.Norm);
  Result.Norm.x := Result.Norm.x*nLen;
  Result.Norm.y := Result.Norm.y*nLen;
  Result.Offset := - Result.Norm.x*APt1.x - Result.Norm.y*APt1.y;
end;

function Pow(const v: TVec2; const s: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := Power(v.x, s);
  Result.y := Power(v.y, s);
end;

function Pow(const v: TVec3; const s: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := Power(v.x, s);
  Result.y := Power(v.y, s);
  Result.z := Power(v.z, s);
end;

function Pow(const v: TVec4; const s: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := Power(v.x, s);
  Result.y := Power(v.y, s);
  Result.z := Power(v.z, s);
  Result.w := Power(v.w, s);
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

function Normalize(const v: TQuat): TQuat; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function QLen(const q: TQuat): Single; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := Sqrt( sqr(q.x) + sqr(q.y) + sqr(q.z) + sqr(q.w) );
  end;
  function fixLen(const q: TQuat): TQuat; {$IFNDEF NoInline} inline; {$ENDIF}
  var s: Single;
  begin
    s := Abs(q.x) + Abs(q.y) + Abs(q.y) + Abs(q.w);
    if s > 0 then
    begin
      s := 1/s;
      Result.x := q.x*s;
      Result.y := q.y*s;
      Result.z := q.z*s;
      Result.w := q.w*s;
    end
    else
    begin
      Result.x := 0;
      Result.y := 0;
      Result.z := 0;
      Result.w := 1;
    end;
  end;
var ql: Single;
begin
  ql := QLen(v);
  if ql < EPS then
    ql := QLen(fixLen(v));
  ql := 1.0/ql;
  Result.x := v.x * ql;
  Result.y := v.y * ql;
  Result.z := v.z * ql;
  Result.w := v.w * ql;
end;

function NormalizeSafe(const v: TVec2; const def: TVec2): TVec2;
var ls: Single;
begin
  ls := LenSqr(v);
  if ls = 0 then
    Result := def
  else
    Result := v / sqrt(ls);
end;

function NormalizeSafe(const v: TVec3; const def: TVec3): TVec3;
var ls: Single;
begin
  ls := LenSqr(v);
  if ls = 0 then
    Result := def
  else
    Result := v / sqrt(ls);
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

function Lerp(const v1, v2: TVec2; s: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x + s.x * (v2.x-v1.x);
  Result.y := v1.y + s.y * (v2.y-v1.y);
end;

function Lerp(const v1, v2: TVec3; s: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x + s * (v2.x-v1.x);
  Result.y := v1.y + s * (v2.y-v1.y);
  Result.z := v1.z + s * (v2.z-v1.z);
end;

function Lerp(const v1, v2: TVec3; s: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x + s.x * (v2.x-v1.x);
  Result.y := v1.y + s.y * (v2.y-v1.y);
  Result.z := v1.z + s.z * (v2.z-v1.z);
end;

function Lerp(const v1, v2: TVec4; s: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x + s * (v2.x-v1.x);
  Result.y := v1.y + s * (v2.y-v1.y);
  Result.z := v1.z + s * (v2.z-v1.z);
  Result.w := v1.w + s * (v2.w-v1.w);
end;

function Lerp(const v1, v2: TVec4; s: TVec4): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := v1.x + s.x * (v2.x-v1.x);
  Result.y := v1.y + s.y * (v2.y-v1.y);
  Result.z := v1.z + s.z * (v2.z-v1.z);
  Result.w := v1.w + s.w * (v2.w-v1.w);
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
  Result.x := Math.Ceil(V.x);
  Result.y := Math.Ceil(V.y);
end;

function Ceil(const V: TVec3): TVec3i;
begin
  Result.x := Math.Ceil(V.x);
  Result.y := Math.Ceil(V.y);
  Result.z := Math.Ceil(V.z);
end;

function Ceil(const V: TVec4): TVec4i;
begin
  Result.x := Math.Ceil(V.x);
  Result.y := Math.Ceil(V.y);
  Result.z := Math.Ceil(V.z);
  Result.w := Math.Ceil(V.w);
end;

function Floor(const V: TVec2): TVec2i;
begin
  Result.x := Math.Floor(V.x);
  Result.y := Math.Floor(V.y);
end;

function Floor(const V: TVec3): TVec3i;
begin
  Result.x := Math.Floor(V.x);
  Result.y := Math.Floor(V.y);
  Result.z := Math.Floor(V.z);
end;

function Floor(const V: TVec4): TVec4i;
begin
  Result.x := Math.Floor(V.x);
  Result.y := Math.Floor(V.y);
  Result.z := Math.Floor(V.z);
  Result.w := Math.Floor(V.w);
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
  if v < 0 then Exit(0);

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

function Log2Int(v: UInt64): Integer;
begin
  if (v > $FFFFFFFF) then
  begin
    Result := 32;
    v := v shr Result;
  end
  else
    Result := 0;

  if (v > $FFFF) then
  begin
    v := v shr 16;
    Result := Result or 16;
  end;

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

function BitsCount(x: UInt64): Integer;
    const m1  : UInt64 = $5555555555555555; //binary: 0101...
    const m2  : UInt64 = $3333333333333333; //binary: 00110011..
    const m4  : UInt64 = $0f0f0f0f0f0f0f0f; //binary:  4 zeros,  4 ones ...
    //const m8  : UInt64 = $00ff00ff00ff00ff; //binary:  8 zeros,  8 ones ...
    //const m16 : UInt64 = $0000ffff0000ffff; //binary: 16 zeros, 16 ones ...
    //const m32 : UInt64 = $00000000ffffffff; //binary: 32 zeros, 32 ones
    //const hff : UInt64 = $ffffffffffffffff; //binary: all ones
    const h01 : UInt64 = $0101010101010101; //the sum of 256 to the power of 0,1,2,3...
begin
  Dec(x, (x shr 1) and m1);             //put count of each 2 bits into those 2 bits
  x := (x and m2) + ((x shr 2) and m2); //put count of each 4 bits into those 4 bits
  x := (x + (x shr 4)) and m4;          //put count of each 8 bits into those 8 bits
  Result := (x * h01) shr 56;           //returns left 8 bits of x + (x<<8) + (x<<16) + (x<<24) + ...
end;

function GetMipsCount(Width, Height: Integer): Integer;
begin
  Result := Log2Int(Min(Width, Height))+1;
end;

function GetMipsCount(Width, Height, Deep: Integer): Integer;
begin
  Result := Log2Int(Min(Min(Width, Height), Deep))+1;
end;

function Bezier2(const pt1, pt2, pt3: TVec2; t: single): TVec2;
var t2: single;
begin
  t2 := 1 - t;
  Result := pt1*(t2*t2) + pt2*(2*t2*t) + pt3*(t*t);
end;

procedure Bezier2Split(const pt1, pt2, pt3: TVec2; t: single; out lpt1, lpt2, lpt3, rpt1, rpt2, rpt3: TVec2);
begin
  lpt1 := pt1;
  lpt2 := lerp(pt1, pt2, t);

  rpt3 := pt3;
  rpt2 := lerp(pt2, pt3, t);

  lpt3 := lerp(lpt2, rpt2, t);
  rpt1 := lpt3;
end;

function Bezier2IsLine(const pt1, pt2, pt3: TVec2; ATolerance: Single): Boolean;
var mainDir: TVec2;
    mainDirLen: Single;
    cDir1, cDir2: TVec2;
    s: Single;
    tolSqr: Single;
begin
  Result := False;
  mainDir := pt3 - pt1;
  cDir1 := pt2 - pt1;
  cDir2 := pt3 - pt2;
  tolSqr := ATolerance * ATolerance;
  if dot(cDir1, mainDir) <= 0 then
  begin
    if LenSqr(cDir1) > tolSqr then Exit;
  end;
  if dot(cDir2, mainDir) <= 0 then
  begin
    if LenSqr(cDir2) > tolSqr then Exit;
  end;
  mainDirLen := LenSqr(mainDir);
  s := Cross(cDir1, mainDir);
  s := s * s / mainDirLen;
  if s > tolSqr then Exit;
  Result := True;
end;

function Bezier3(const pt1, pt2, pt3, pt4: TVec2; t: single): TVec2;
var t2: single;
begin
  t2 := 1 - t;
  Result := pt1*(t2*t2*t2) + pt2*(3*t*t2*t2) + pt3*(3*t*t*t2) + pt4*(t*t*t);
end;

function DistanceToBezier3(const APt: TVec2; const pt1, pt2, pt3, pt4: TVec2): Single;
var dummy: TVec2;
    dummyT: Single;
begin
  Result := DistanceToBezier3(APt, pt1, pt2, pt3, pt4, dummy, dummyT);
end;

function DistanceToBezier3(const APt: TVec2; const pt1, pt2, pt3, pt4: TVec2; out ClosestPt: TVec2; out T: Single): Single; overload;
var A, B, C, D: TVec2;
    Params, roots: TDoubleArr;
    minPt, Pb: TVec2;
    minDist, dist: Single;
    k: Integer;
begin
  A := pt4 - pt3*3 + pt2*3 - pt1;
  B :=       pt3*3 - pt2*6 + pt1*3;
  C :=               pt2*3 - pt1*3;
  D :=                       pt1;

  SetLength(Params, 6);
  Params[0] := 3*Dot(A, A);
  Params[1] := 5*Dot(A, B);
  Params[2] := 4*Dot(A, C) + 2*Dot(B, B);
  Params[3] := 3*Dot(B, C) + 3*Dot(A, D);// - 3*Dot(A, APt);
  Params[4] := Dot(C, C) + 2*Dot(B, D);// - 2*Dot(B, APt);
  Params[5] := Dot(D, C);// - Dot(C, APt);

  Params[3] := Params[3] - 3*Dot(A, APt);
  Params[4] := Params[4] - 2*Dot(B, APt);
  Params[5] := Params[5] - Dot(C, APt);

  roots := SolveN(params, 100000, 1);

  minPt := pt1;
  minDist := LenSqr(APt-minPt);
  T := 0;
  dist := LenSqr(APt-pt4);
  if dist < minDist then
  begin
    minPt := pt4;
    minDist := dist;
    T := 1;
  end;

  for k := 0 to Length(roots) - 1 do
  begin
    if roots[k] < 0 then Continue;
    if roots[k] > 1 then Continue;
    Pb := Bezier3(pt1, pt2, pt3, pt4, roots[k]);
    dist := LenSqr(APt-Pb);
    if dist < minDist then
    begin
      minPt := Pb;
      minDist := dist;
      T := roots[k];
    end;
  end;
  ClosestPt := minPt;
  Result := sqrt(minDist);
end;

procedure Bezier3Split(const pt1, pt2, pt3, pt4: TVec2; t: single; out lpt1, lpt2, lpt3, lpt4, rpt1, rpt2, rpt3, rpt4: TVec2); overload;
var tmp: TVec2;
begin
  tmp := lerp(pt2, pt3, t);

  lpt1 := pt1;
  lpt2 := lerp(pt1, pt2, t);
  lpt3 := lerp(lpt2, tmp, t);

  rpt4 := pt4;
  rpt3 := lerp(pt3, pt4, t);
  rpt2 := lerp(tmp, rpt3, t);

  lpt4 := lerp(lpt3, rpt2, t);
  rpt1 := lpt4;
end;

function Bezier3IsLine(const pt1, pt2, pt3, pt4: TVec2; ATolerance: Single): Boolean; overload;
var mainDir: TVec2;
    mainDirLen: Single;
    cDir1, cDir2: TVec2;
    s: Single;
    tolSqr: Single;
begin
  Result := False;
  mainDir := pt4 - pt1;
  cDir1 := pt2 - pt1;
  cDir2 := pt4 - pt3;
  tolSqr := ATolerance * ATolerance;
  if dot(cDir1, mainDir) <= 0 then
  begin
    if LenSqr(cDir1) > tolSqr then Exit;
  end;
  if dot(cDir2, mainDir) <= 0 then
  begin
    if LenSqr(cDir2) > tolSqr then Exit;
  end;
  mainDirLen := LenSqr(mainDir);
  s := Cross(cDir1, mainDir);
  s := s * s / mainDirLen;
  if s > tolSqr then Exit;
  s := Cross(cDir2, mainDir);
  s := s * s / mainDirLen;
  if s > tolSqr then Exit;
  Result := True;
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
  MatrixView.f[0, 0] := Right.x;  MatrixView.f[0, 1] := Up.x;  MatrixView.f[0, 2] := View.x;  MatrixView.f[0, 3] := 0;
  MatrixView.f[1, 0] := Right.y;  MatrixView.f[1, 1] := Up.y;  MatrixView.f[1, 2] := View.y;  MatrixView.f[1, 3] := 0;
  MatrixView.f[2, 0] := Right.z;  MatrixView.f[2, 1] := Up.z;  MatrixView.f[2, 2] := View.z;  MatrixView.f[2, 3] := 0;

  // Do the translation values (rotations are still about the eyepoint)
  MatrixView.f[3, 0] := - Dot(From,Right );
  MatrixView.f[3, 1] := - Dot(From,Up );
  MatrixView.f[3, 2] := - Dot(From,View );
  MatrixView.f[3, 3] := 1;

  MatrixView := MatrixView;
  Result := S_OK;
end;

function GetUIMatrix(const AWidth, AHeight: Single; const ADepth: Single): TMat4;
begin
  Result := IdentityMat4;
  Result.OX := Vec(2/AWidth, 0, 0);
  Result.OY := Vec(0, -2/AHeight, 0);
  Result.OZ := Vec(0, 0, 1/ADepth);
  Result.Pos := Vec(-1.0, 1.0, 0);
end;

function GetUIMat3(const AWidth, AHeight: Single): TMat3;
begin
  Result := IdentityMat3;
  Result.OX := Vec(2/AWidth, 0);
  Result.OY := Vec(0, -2/AHeight);
  Result.Pos := Vec(-1.0, 1.0);
end;

function GetUIMat3(const ASize: TVec2i): TMat3;
begin
  Result := GetUIMat3(ASize.x, ASize.y);
end;

function ToStr(const v: Single): string;
begin
  Result := FormatFloat('0.000', v);
end;

function ToStr(const v: TVec2): string;
begin
  Result := '(' + ToStr(v.x) + '; ' + ToStr(v.y) + ')';
end;

function ToStr(const v: TVec3): string;
begin
  Result := '(' + ToStr(v.x) + '; ' + ToStr(v.y) + '; ' + ToStr(v.z) + ')';
end;

function ToStr(const v: TVec4): string;
begin
  Result := '(' + ToStr(v.x) + '; ' + ToStr(v.y) + '; ' + ToStr(v.z) + '; ' + ToStr(v.w) + ')';
end;

function NormalizeAngle(angle: single): single;
const PI2 = 2*Pi;
begin
  Result := frac(angle/PI2)*PI2;
  if Result < 0 then Result := Result + PI2;
end;

function ShortestRotation(from: single; at: single): single;
begin
  Result := NormalizeAngle(at-from);
  if (Result > Pi) then Result := Result - 2*Pi;
end;

function VecSinCos(const Angle: Single): TVec2;
begin
  Result.x := cos(Angle);
  Result.y := sin(Angle);
end;

function FromToQuat(const AFrom, ATo: TVec3): TQuat;
var c: TVec3;
begin
  c := Cross(AFrom, ATo);
  Result.x := c.x;
  Result.y := c.y;
  Result.z := c.z;
  Result.w := Dot(AFrom, ATo);
  Result := Normalize(Result);
  Result.w := Result.w + 1;
  if Result.w < EPS then
  begin
    if sqr(AFrom.z) > sqr(AFrom.x) then
    begin
      Result.x := 0;
      Result.y := AFrom.z;
      Result.z := - AFrom.y;
    end
    else
    begin
      Result.x := AFrom.y;
      Result.y := -AFrom.x;
      Result.z := 0;
    end;
  end;
  Result := Normalize(Result);
end;

function FromToMat(AFrom, ATo: TVec3): TMat4;
var c: TVec3;
    clen: Single;
    m1, m2: TMat4;
begin
  AFrom := Normalize(AFrom);
  ATo := Normalize(ATo);
  c := Cross(AFrom, ATo);
  clen := Len(c);
  if clen < EPS then Exit(IdentityMat4);
  c := c * (1/clen);
  m1 := IdentityMat4;
  m1.Row[0].xyz := AFrom;
  m1.Row[1].xyz := c;
  m1.Row[2].xyz := Cross(m1.Row[0].xyz, m1.Row[1].xyz);

  m2 := IdentityMat4;
  m2.Row[0].xyz := ATo;
  m2.Row[1].xyz := c;
  m2.Row[2].xyz := Cross(m2.Row[0].xyz, m2.Row[1].xyz);

  Result := Inv(m1) * m2;
end;

{$IfDef FPC}
operator = (const v1, v2: TRectF): Boolean;
begin
  Result := (v1.LeftTop = v2.LeftTop) and (v1.RightBottom = v2.RightBottom);
end;

operator = (const v1, v2: TRectI): Boolean;
begin
  Result := (v1.LeftTop = v2.LeftTop) and (v1.RightBottom = v2.RightBottom);
end;

operator = (const b1, b2: TAABB): Boolean;
begin
  Result := (b1.min = b2.min) and (b1.max = b2.max);
end;

operator = (const q1, q2: TQuat): Boolean;
begin
  Result := q1.v4 = q2.v4;
end;

operator + (const r: TRectF; const v: TVec2): TRectF;
begin
  Result.min := min(r.min, v);
  Result.max := max(r.max, v);
end;

operator + (const r1, r2: TRectF): TRectF;
begin
  Result.min := min(r1.min, r2.min);
  Result.max := max(r1.max, r2.max);
end;

operator * (const r: TRectF; const m: TMat3): TRectF;
var
  i: Integer;
begin
  Result.v := Vec(HUGE, HUGE, -HUGE, -HUGE);
  for i := 0 to 3 do
    Result := Result + r.Point(i)*m;
end;

operator + (const b: TRectI; v: TVec2i): TRectI;
begin
  Result.min := min(b.min, v);
  Result.max := max(b.max, v);
end;

operator + (const b1, b2: TRectI): TRectI;
begin
  Result.min := min(b1.min, b2.min);
  Result.max := max(b1.max, b2.max);
end;

operator + (const AABB: TAABB; v: TVec3): TAABB;
begin
  Result.min := min(AABB.min, v);
  Result.max := max(AABB.max, v);
end;

operator + (const Box1, Box2: TAABB): TAABB;
begin
  Result.min := mutils.Min(Box1.min, Box2.min);
  Result.max := mutils.Max(Box1.max, Box2.max);
end;

operator * (const AABB: TAABB; const m: TMat4): TAABB;
var i: Integer;
begin
  Result := EmptyAABB;
  for i := 0 to 7 do
    Result := Result + AABB.Point(i) * m;
end;

operator * (const v: TVec2i; s: Single): TVec2;
begin
  Result.x := v.x * s;
  Result.y := v.y * s;
end;
{$EndIf}

function Rotate(const v: TVec2; const Angle: Single): TVec2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var sn, cs: Extended;
begin
  sincos(Angle, sn, cs);
  Result.x := cs * v.x - sn * v.y;
  Result.y := cs * v.y + sn * v.x;
end;

function Rotate(const v: TVec2; const ASin, ACos: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result.x := ACos * v.x - ASin * v.y;
  Result.y := ACos * v.y + ASin * v.x;
end;

function Rotate90(const v: TVec2; const CW: Boolean): TVec2;
begin
  if CW then
  begin
    Result.x :=  v.y;
    Result.y := -v.x;
  end
  else
  begin
    Result.x := -v.y;
    Result.y :=  v.x;
  end;
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

function Intersect(const RayOrig, RayDir: TVec3; const AABB: TAABB; out T: Single; SolidAABB, AllowBackfaces: Boolean): Boolean;
var tMin, tMax: TVec3;
begin
  if (RayDir.x = 0) and (RayDir.y = 0) and (RayDir.z = 0) then Exit(False);

  if RayDir.x > 0 then
  begin
    tMin.x := (AABB.min.x - RayOrig.x) / RayDir.x;
    tMax.x := (AABB.max.x - RayOrig.x) / RayDir.x;
  end
  else
  begin
    tMax.x := (AABB.min.x - RayOrig.x) / RayDir.x;
    tMin.x := (AABB.max.x - RayOrig.x) / RayDir.x;
  end;

  if RayDir.y > 0 then
  begin
    tMin.y := (AABB.min.y - RayOrig.y) / RayDir.y;
    tMax.y := (AABB.max.y - RayOrig.y) / RayDir.y;
  end
  else
  begin
    tMax.y := (AABB.min.y - RayOrig.y) / RayDir.y;
    tMin.y := (AABB.max.y - RayOrig.y) / RayDir.y;
  end;

  if (tMin.x > tMax.y) or (tMin.y > tMax.x) then Exit(False);

  tMin.x := Max(tMin.x, tMin.y);
  tMax.x := Min(tMax.x, tMax.y);

  if RayDir.z > 0 then
  begin
    tMin.z := (AABB.min.z - RayOrig.z) / RayDir.z;
    tMax.z := (AABB.max.z - RayOrig.z) / RayDir.z;
  end
  else
  begin
    tMax.z := (AABB.min.z - RayOrig.z) / RayDir.z;
    tMin.z := (AABB.max.z - RayOrig.z) / RayDir.z;
  end;

  if (tMin.x > tMax.z) or (tMin.z > tMax.x) then Exit(False);

  tMin.x := Max(tMin.x, tMin.z);
  tMax.x := Min(tMax.x, tMax.z);

  if SolidAABB then
  begin
    tMin.x := Max(0, tMin.x);
    if tMin.x > tMax.x then Exit(False);
  end
  else
  begin
    if AllowBackfaces and (tMin.x < 0) then tMin.x := tMax.x;
    if tMin.x < 0 then Exit(False);
  end;
  T := tMin.x;
  Result := True;
end;

function Intersect(const p1, p2, p3: TVec3; const RayOrig, RayDir: TVec3; out T: Single; out UVW: TVec3; FiniteRay: Boolean): Boolean;
var a, b, p, q, n: TVec3;
    invDet, Det: Single;
begin
  T := 0;

  a := p1 - p2;
  b := p3 - p1;
  n := Cross(b, a);
  p := p1 - RayOrig;
  q := Cross(p, RayDir);

  Det := dot(RayDir, n);
  if Det = 0 then Exit(False);
  invDet := 1.0/Det;

  UVW.x := Dot(q, b) * invDet;
  UVW.y := Dot(q, a) * invDet;

  if (UVW.x<0) or (UVW.x>1) or (UVW.y<0) or (UVW.x+UVW.y>1) then Exit(False);
  UVW.z := 1.0 - UVW.x - UVW.y;

  T := Dot(n, p) * invDet;
  if T < 0 then Exit(False);

  if FiniteRay then
    if T > 1.0 then Exit(False);

  Result := True;
end;

function Intersect(const Plane: TPlane; Line: TLine; out IntPt: TVec3): Boolean; overload;
var Da,Db : single;
    k     : single;
begin
  Da := -Dot(Plane.Norm, Line.Pnt);
  Db := -Dot(Plane.Norm, Line.Pnt + Line.Dir);
  if Da = Db then
  begin
    IntPt := Vec(Infinity, Infinity, Infinity);
    Exit(False);
  end;
  k := (Plane.D - Da) / (Db - Da);
  IntPt := Line.Pnt + (Line.Dir * k);
  Result := True;
end;

function Intersect(const R1: TRectF; const R2: TRectF): Boolean;
begin
  Result := (R1.max.x >= R2.min.x) And (R1.min.x <= R2.max.x) And
            (R1.max.y >= R2.min.y) And (R1.min.y <= R2.max.y);
end;

function Intersect(const R1: TRectI; const R2: TRectI): Boolean;
begin
  Result := (R1.max.x > R2.min.x) And (R1.min.x < R2.max.x) And
            (R1.max.y > R2.min.y) And (R1.min.y < R2.max.y);
end;

function Intersect(const B1, B2: TAABB): Boolean;
begin
  Result := (B1.max.x >= B2.min.x) And (B1.min.x <= B2.max.x) And
            (B1.max.y >= B2.min.y) And (B1.min.y <= B2.max.y) And
            (B1.max.z >= B2.min.z) And (B1.min.z <= B2.max.z);
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

function Distance(const Pt: TVec2; const Seg: TSegment2D; out AClosestPt: TVec2): Single;
var dir, v1, v2: TVec2;
    segline: TLine2D;
    d1, d2: Single;
begin
  dir := Seg.Pt2 - Seg.Pt1;
  v1 := Pt - Seg.Pt1;
  v2 := Pt - Seg.Pt2;
  if (Dot(v1, dir) > 0) and (Dot(v2, dir) < 0) then
  begin
    segline := Seg.Line(True);
    Result := (dot(segline.Norm, Pt) + segline.C);
    AClosestPt := segline.Norm * Result + Pt;
    Result := abs(Result);
  end
  else
  begin
    d1 := LenSqr(v1);
    d2 := LenSqr(v2);
    if d1 < d2 then
    begin
      Result := sqrt(d1);
      AClosestPt := Seg.Pt1;
    end
    else
    begin
      Result := sqrt(d2);
      AClosestPt := Seg.Pt2;
    end;
  end;
end;

function Projection(const Pt: TVec2; const Line: TLine2D): TVec2;
var C: Single;
begin
  C := Dot(Pt, Line.Norm) + Line.Offset;
  Result := Pt + Line.Norm*C/LenSqr(Line.Norm);
end;

function Projection(const Cast: TVec3; const Recieve: TVec3): TVec3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var Recieve_LenSqr : Single;
begin
  Recieve_LenSqr := LenSqr(Recieve);
  if Recieve_LenSqr = 0 then
    Result := Vec(0,0,0)
  else
    Result := Recieve * (Dot(Cast, Recieve) / Recieve_LenSqr);
end;

function Projection(const Pt: TVec3; const Line: TLine): TVec3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var ppt: TVec3;
begin
  ppt := Projection(Pt - Line.Pnt, Line.Dir);
  Result := Line.Pnt + ppt;
end;

function Projection(const Pt: TVec3; const Plane: TPlane): TVec3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
var DPt: Single;
begin
  DPt := -Dot(Plane.Norm, Pt);
  DPt := DPt - Plane.D;
  Result := Pt + Plane.Norm * DPt / LenSqr(Plane.Norm);
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

function Inv(const q: TQuat): TQuat; overload;{$IFNDEF NoInline} inline; {$ENDIF}
begin
  Result := q;
  Result.w := -Result.w;
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

function TRectF.Expand(const ASize: Single): TRectF;
begin
  Result.min.x := min.x - ASize;
  Result.min.y := min.y - ASize;
  Result.max.x := max.x + ASize;
  Result.max.y := max.y + ASize;
end;

function TRectF.Expand(const ASize: TVec2): TRectF;
begin
  Result.min.x := min.x - ASize.x;
  Result.min.y := min.y - ASize.y;
  Result.max.x := max.x + ASize.x;
  Result.max.y := max.y + ASize.y;
end;

function TRectF.PtInRect(const v: TVec2) : Boolean;
begin
  Result := (v.x >= min.x) and (v.y >= min.y) and
            (v.x <= max.x) and (v.y <= max.y);
end;

function TRectF.Point(AIndex: Integer): TVec2;
begin
  case AIndex mod 4 of
    0: Result := min;
    1: Result := Vec(min.x, max.y);
    2: Result := max;
    3: Result := Vec(max.x, min.y);
  else
    Result := Vec(0,0);
  end;
end;

{$IfDef DCC}
class operator TRectF.Equal(const r1, r2: TRectF): Boolean;
begin
  Result := (r1.min = r2.min) and (r1.max = r2.max);
end;

class operator TRectF.Add(const r: TRectF; const v: TVec2): TRectF;
begin
  Result.min := mutils.Min(r.min, v);
  Result.max := mutils.max(r.max, v);
end;

class operator TRectF.Add(const r1, r2: TRectF): TRectF;
begin
  Result.min := mutils.Min(r1.min, r2.min);
  Result.max := mutils.Max(r1.max, r2.max);
end;

class operator TRectF.Multiply(const r: TRectF; const m: TMat3): TRectF;
var
  i: Integer;
begin
  Result.v := Vec(HUGE, HUGE, -HUGE, -HUGE);
  for i := 0 to 3 do
    Result := Result + r.Point(i)*m;
end;
{$EndIf}

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

