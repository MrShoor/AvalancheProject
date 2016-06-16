{$Define Integer}

{$IfDef Byte} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef ShortInt} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef Integer} {$Define CastToSingle} {$EndIf}

{$IFDEF INTF}
type
  { TVec2i }

  TVec2i = record
    x, y: Integer;
  public
    function yx: TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}

    Class Operator Equal (const v1, v2: TVec2i): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec2i): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec2i): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec2i): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec2i; s: Integer): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec2i): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec2i): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec2i): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec2i; s: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef Single}
    Class Operator Divide (const v1, v2: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Divide (const v: TVec2; const f: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
  end;
  PVec2i =^TVec2i;

  { TVec3i }

  TVec3i = record
  public
    Class Operator Equal (const v1, v2: TVec3i): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec3i): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec3i): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec3i): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec3i; s: Integer): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec3i): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec3i): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec3i): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec3i; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef Single}
    Class Operator Divide (const v1, v2: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Divide (const v: TVec3; const f: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
  case Byte of
    0: (f: array [0..2] of Integer);
    1: (x, y, z: Integer);
    2: (xy: TVec2i);
  end;
  PVec3i =^TVec3i;

  { TVec4i }

  TVec4i = record
  public
    Class Operator Equal (const v1, v2: TVec4i): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec4i): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec4i): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec4i): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec4i; s: Integer): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec4i): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec4i): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec4i): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec4i; s: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef Single}
    Class Operator Divide (const v1, v2: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Divide (const v: TVec4; const f: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
  case Byte of
    0: (f: array [0..3] of Integer);
    1: (x, y, z, w: Integer);
    2: (xy, zw: TVec2i);
    3: (xyz: TVec3i);
  end;
  PVec4i =^TVec4i;

  TVec2iArr = array of TVec2i;
  TVec3iArr = array of TVec3i;
  TVec4iArr = array of TVec4i;

  { TMat2i }

  TMat2i = record
  private
    function GetCol(index: Integer): TVec2i;               {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TVec2i); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator Equal (const m1, m2: TMat2i): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat2i): TMat2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat2i): TMat2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat2i): TMat2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat2i; v: TVec2i): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat2i; s: Integer): TMat2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat2i): TMat2i; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    property Col[index: Integer]: TVec2i read GetCol write SetCol;
  case Byte of
    0: (f: array [0..1,0..1] of Integer);
    1: (Row: array [0..1] of TVec2i);
  end;
  PMat2i = ^TMat2i;

  { TMat3i }

  TMat3i = record
  private
    function GetCol(index: Integer): TVec3i;               {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TVec2i;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TVec2i;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TVec2i;  {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TVec3i); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TVec2i); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TVec2i); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TVec2i); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator Equal (const m1, m2: TMat3i): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat3i): TMat3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat3i): TMat3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat3i): TMat3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec3i; const m: TMat3i): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec2i; const m: TMat3i): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat3i; s: Integer): TMat3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat3i): TMat3i; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    property Col[index: Integer]: TVec3i read GetCol write SetCol;
    property OX: TVec2i read GetOX write SetOX;
    property OY: TVec2i read GetOY write SetOY;
    property Pos: TVec2i read GetPos write SetPos;
  case Byte of
    0: (f: array [0..2,0..2] of Integer);
    1: (Row: array [0..2] of TVec3i);
  end;
  PMat3i = ^TMat3i;

  { TMat4i }

  TMat4i = record
  private
    function GetCol(index: Integer): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TVec3i;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TVec3i;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOZ: TVec3i;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TVec3i;  {$IFNDEF NoInline} inline; {$ENDIF}

    procedure SetCol(index: Integer; const Value: TVec4i); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TVec3i); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TVec3i); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOZ(const AValue: TVec3i); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TVec3i); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator Equal (const m1, m2: TMat4i): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat4i): TMat4i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat4i): TMat4i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat4i): TMat4i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec4i; const m: TMat4i): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec3i; const m: TMat4i): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat4i; s: Integer): TMat4i; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat4i): TMat4i; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    function Mat3x3: TMat3i;

    property Col[index: Integer]: TVec4i read GetCol write SetCol;
    property OX: TVec3i read GetOX write SetOX;
    property OY: TVec3i read GetOY write SetOY;
    property OZ: TVec3i read GetOZ write SetOZ;
    property Pos: TVec3i read GetPos write SetPos;
  case Byte of
    0: (f: array [0..3,0..3] of Integer);
    1: (Row: array [0..3] of TVec4i);
  end;
  PMat4i = ^TMat4i;

  TMat2iArr = array of TMat2i;
  TMat3iArr = array of TMat3i;
  TMat4iArr = array of TMat4i;

  function Abs(const V: TVec2i): TVec2i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: TVec3i): TVec3i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TVec2i): TVec2i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TVec3i): TVec3i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TVec2i): TVec2i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TVec3i): TVec3i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: Integer; minval, maxval: Integer): Integer; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TVec2i; minval, maxval: Integer): TVec2i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TVec3i; minval, maxval: Integer): TVec3i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec2i): TVec2i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec3i): TVec3i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec4i): TVec4i; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Dot(const v1, v2: TVec2i): Integer; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TVec3i): Integer; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TVec4i): Integer; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TVec2i): Integer; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TVec3i): TVec3i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TVec2i): Integer; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TVec3i): Integer; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Transpose(const m: TMat2i): TMat2i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TMat3i): TMat3i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TMat4i): TMat4i; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Det(const m: TMat2i): Integer; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TMat3i): Integer; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TMat4i): Integer; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Vec(const x, y: Integer): TVec2i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z: Integer): TVec3i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TVec2i; z: Integer): TVec3i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: Integer; const yz: TVec2i): TVec3i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z, w: Integer): TVec4i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TVec2i; const zw: TVec2i): TVec4i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xyz: TVec3i; w: Integer): TVec4i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: Integer; const yz: TVec2i; w: Integer): TVec4i; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: Integer; const yzw: TVec3i): TVec4i; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Equal (const v1, v2: TVec2i; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TVec3i; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TVec4i; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat2i; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat3i; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat4i; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;

  procedure Swap(var v1, v2: TVec2i); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TVec3i); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TVec4i); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat2i); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat3i); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat4i); overload; {$IFNDEF NoInline} inline; {$ENDIF}
{$ENDIF}

{$IFDEF IMPL}
  { TVec2i }

  function TVec2i.yx: TVec2i;
  begin
    Result.x := y;
    Result.y := x;
  end;

  { TMat2i }

  function TMat2i.GetCol(index: Integer): TVec2i;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
  end;

  procedure TMat2i.SetCol(index: Integer; const Value: TVec2i);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
  end;

  { TMat3i }

  function TMat3i.GetCol(index: Integer): TVec3i;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
  end;

  function TMat3i.GetOX: TVec2i;
  begin
    Result := Row[0].xy;
  end;

  function TMat3i.GetOY: TVec2i;
  begin
    Result := Row[1].xy;
  end;

  function TMat3i.GetPos: TVec2i;
  begin
    Result := Row[2].xy;
  end;

  procedure TMat3i.SetCol(index: Integer; const Value: TVec3i);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
  end;

  procedure TMat3i.SetOX(const AValue: TVec2i);
  begin
    Row[0].xy := AValue;
  end;

  procedure TMat3i.SetOY(const AValue: TVec2i);
  begin
    Row[1].xy := AValue;
  end;

  procedure TMat3i.SetPos(const AValue: TVec2i);
  begin
    Row[2].xy := AValue;
  end;

  { TMat4i }

  function TMat4i.GetCol(index: Integer): TVec4i;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
    Result.w := f[3, index];
  end;

  function TMat4i.GetOX: TVec3i;
  begin
    Result := Row[0].xyz;
  end;

  function TMat4i.GetOY: TVec3i;
  begin
    Result := Row[1].xyz;
  end;

  function TMat4i.GetOZ: TVec3i;
  begin
    Result := Row[2].xyz;
  end;

  function TMat4i.GetPos: TVec3i;
  begin
    Result := Row[3].xyz;
  end;

  procedure TMat4i.SetCol(index: Integer; const Value: TVec4i);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
    f[3, index] := Value.w;
  end;

  procedure TMat4i.SetOX(const AValue: TVec3i);
  begin
    Row[0].xyz := AValue;
  end;

  procedure TMat4i.SetOY(const AValue: TVec3i);
  begin
    Row[1].xyz := AValue;
  end;

  procedure TMat4i.SetOZ(const AValue: TVec3i);
  begin
    Row[2].xyz := AValue;
  end;

  procedure TMat4i.SetPos(const AValue: TVec3i);
  begin
    Row[3].xyz := AValue;
  end;

  function TMat4i.Mat3x3: TMat3i;
  begin
    Result.Row[0] := Row[0].xyz;
    Result.Row[1] := Row[1].xyz;
    Result.Row[2] := Row[2].xyz;
  end;

  Class Operator TVec2i.Equal (const v1, v2: TVec2i): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y);
  end;

  Class Operator TVec2i.Add (const v1, v2: TVec2i): TVec2i;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
  end;

  Class Operator TVec2i.Subtract (const v1, v2: TVec2i): TVec2i;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
  end;

  Class Operator TVec2i.Multiply (const v1, v2: TVec2i): TVec2i;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
  end;

  Class Operator TVec2i.Multiply (const v1: TVec2i; s: Integer): TVec2i;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
  end;

  Class Operator TVec2i.Negative (const v: TVec2i): TVec2i;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
  end;

  {$IfDef CastToInt}
  Class Operator TVec2i.Implicit (const V: TVec2i): TVec2i;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec2i.Implicit (const V: TVec2i): TVec2;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;

  Class Operator TVec2i.Multiply (const v1: TVec2i; s: Single): TVec2;
  begin
    Result.x := V1.x * s;
    Result.y := V1.y * s;
  end;
  {$EndIf}

  {$IfDef Single}
  Class Operator TVec2i.Divide (const v1, v2: TVec2): TVec2;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
  end;

  Class Operator TVec2i.Divide (const v: TVec2; const f: Single): TVec2;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
  end;
  {$EndIf}

  Class Operator TVec3i.Equal (const v1, v2: TVec3i): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
  end;

  Class Operator TVec3i.Add (const v1, v2: TVec3i): TVec3i;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
  end;

  Class Operator TVec3i.Subtract (const v1, v2: TVec3i): TVec3i;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
  end;

  Class Operator TVec3i.Multiply (const v1, v2: TVec3i): TVec3i;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
  end;

  Class Operator TVec3i.Multiply (const v1: TVec3i; s: Integer): TVec3i;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  end;

  Class Operator TVec3i.Negative (const v: TVec3i): TVec3i;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
  end;

  {$IfDef CastToInt}
  Class Operator TVec3i.Implicit (const V: TVec3i): TVec3i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec3i.Implicit (const V: TVec3i): TVec3;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;

  Class Operator TVec3i.Multiply (const v1: TVec3i; s: Single): TVec3;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  End;
  {$EndIf}

  {$IfDef Single}
  Class Operator TVec3i.Divide (const v1, v2: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
  end;

  Class Operator TVec3i.Divide (const v: TVec3; const f: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
  end;
  {$EndIf}

  Class Operator TVec4i.Equal (const v1, v2: TVec4i): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z) and (v1.w = v2.w);
  end;

  Class Operator TVec4i.Add (const v1, v2: TVec4i): TVec4i;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
    Result.w := v1.w + v2.w;
  end;

  Class Operator TVec4i.Subtract (const v1, v2: TVec4i): TVec4i;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
    Result.w := v1.w - v2.w;
  end;

  Class Operator TVec4i.Multiply (const v1, v2: TVec4i): TVec4i;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
    Result.w := v1.w * v2.w;
  end;

  Class Operator TVec4i.Multiply (const v1: TVec4i; s: Integer): TVec4i;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  end;

  Class Operator TVec4i.Negative (const v: TVec4i): TVec4i;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
    Result.w := - v.w;
  end;

  {$IfDef CastToInt}
  Class Operator TVec4i.Implicit (const V: TVec4i): TVec4i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec4i.Implicit (const V: TVec4i): TVec4;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;

  Class Operator TVec4i.Multiply (const v1: TVec4i; s: Single): TVec4;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  End;

  {$EndIf}

  {$IfDef Single}
  Class Operator TVec4i.Divide (const v1, v2: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
    Result.w := v1.w / v2.w;
  end;

  Class Operator TVec4i.Divide (const v: TVec4; const f: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
    Result.w := v.w / f;
  end;
  {$EndIf}

  Class operator TMat2i.Equal (const m1, m2: TMat2i): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]);
  end;

  Class operator TMat2i.Add (const m1, m2: TMat2i): TMat2i;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
  end;

  Class operator TMat2i.Subtract (const m1, m2: TMat2i): TMat2i;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
  end;

  Class operator TMat2i.Multiply (const m1, m2: TMat2i): TMat2i;
  var i, j: Integer;
  begin
    for j := 0 to 1 do
      for i := 0 to 1 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class operator TMat2i.Multiply (const m: TMat2i; v: TVec2i): TVec2i;
  begin
    Result.x := Dot(m.Row[0], v);
    Result.y := Dot(m.Row[1], v);
  end;

  Class operator TMat2i.Multiply (const m: TMat2i; s: Integer): TMat2i;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
  end;

  Class operator TMat2i.Negative (const m: TMat2i): TMat2i;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
  end;

  Class Operator TMat3i.Equal (const m1, m2: TMat3i): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]);
  end;

  Class Operator TMat3i.Add (const m1, m2: TMat3i): TMat3i;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
  end;

  Class Operator TMat3i.Subtract (const m1, m2: TMat3i): TMat3i;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
  end;

  Class Operator TMat3i.Multiply (const m1, m2: TMat3i): TMat3i;
  var i, j: Integer;
  begin
    for i := 0 to 2 do
      for j := 0 to 2 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TMat3i.Multiply (const v: TVec3i; const m: TMat3i): TVec3i;
  begin
    Result := m.Row[0]*v.x+
              m.Row[1]*v.y+
              m.Row[2]*v.z;
  end;

  Class Operator TMat3i.Multiply (const v: TVec2i; const m: TMat3i): TVec2i;
  begin
    Result := m.Row[0].xy*v.x+
              m.Row[1].xy*v.y+
              m.Row[2].xy;
  end;

  Class Operator TMat3i.Multiply (const m: TMat3i; s: Integer): TMat3i;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
  end;

  Class Operator TMat3i.Negative (const m: TMat3i): TMat3i;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
  end;

  Class Operator TMat4i.Equal (const m1, m2: TMat4i): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]) and (m1.Row[3] = m2.Row[3]);
  end;

  Class Operator TMat4i.Add (const m1, m2: TMat4i): TMat4i;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
    Result.Row[3] := m1.Row[3] + m2.Row[3];
  end;

  Class Operator TMat4i.Subtract (const m1, m2: TMat4i): TMat4i;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
    Result.Row[3] := m1.Row[3] - m2.Row[3];
  end;

  Class Operator TMat4i.Multiply (const m1, m2: TMat4i): TMat4i;
  var i, j: Integer;
  begin
    for i := 0 to 3 do
      for j := 0 to 3 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TMat4i.Multiply (const v: TVec4i; const m: TMat4i): TVec4i;
  begin
    Result := m.Row[0]*v.x +
              m.Row[1]*v.y +
              m.Row[2]*v.z +
              m.Row[3]*v.w;
  end;

  Class Operator TMat4i.Multiply (const v: TVec3i; const m: TMat4i): TVec3i;
  begin
    Result := m.Row[0].xyz*v.x +
              m.Row[1].xyz*v.y +
              m.Row[2].xyz*v.z +
              m.Row[3].xyz;
  end;

  Class Operator TMat4i.Multiply (const m: TMat4i; s: Integer): TMat4i;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
    Result.Row[3] := m.Row[3] * s;
  end;

  Class Operator TMat4i.Negative (const m: TMat4i): TMat4i;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
    Result.Row[3] := - m.Row[3];
  end;

  function Abs(const V: TVec2i): TVec2i;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
  end;

  function Abs(const V: TVec3i): TVec3i;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
    Result.z := System.abs(V.z);
  end;

  function Min(const A, B: TVec2i): TVec2i; overload;
  begin
    Result.x := Math.Min(A.x, B.x);
    Result.y := Math.Min(A.y, B.y);
  end;
  function Min(const A, B: TVec3i): TVec3i; overload;
  begin
    Result.x := Min(A.x, B.x);
    Result.y := Min(A.y, B.y);
    Result.z := Min(A.z, B.z);
  end;
  function Max(const A, B: TVec2i): TVec2i; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
  end;
  function Max(const A, B: TVec3i): TVec3i; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
    Result.z := Max(A.z, B.z);
  end;

  function Clamp(const v: Integer; minval, maxval: Integer): Integer; overload;
  begin
      Result := min(maxval, max(minval, v));
  end;
  function Clamp(const v: TVec2i; minval, maxval: Integer): TVec2i; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
  end;
  function Clamp(const v: TVec3i; minval, maxval: Integer): TVec3i; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
      Result.z := min(maxval, max(minval, v.z));
  end;

  function Sign(const V: TVec2i): TVec2i; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
  end;

  function Sign(const V: TVec3i): TVec3i; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
  end;

  function Sign(const V: TVec4i): TVec4i; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
    Result.w := sign(V.w);
  end;

  function Dot(const v1, v2: TVec2i): Integer; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y;
  end;

  function Dot(const v1, v2: TVec3i): Integer; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
  end;

  function Dot(const v1, v2: TVec4i): Integer; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
  end;

  function Cross(const v1, v2: TVec2i): Integer; overload;
  begin
    Result := v1.x * v2.y - v1.y * v2.x;
  end;

  function Cross(const v1, v2: TVec3i): TVec3i; overload;
  begin
    result.x := (v1.y*v2.z) - (v1.z*v2.y);
    result.y := (v1.z*v2.x) - (v1.x*v2.z);
    result.z := (v1.x*v2.y) - (v1.y*v2.x);
  end;

  function LenSqr(const v: TVec2i): Integer; overload;
  begin
    Result := dot(v, v);
  end;

  function LenSqr(const v: TVec3i): Integer; overload;
  begin
    Result := dot(v, v);
  end;

  function Vec(const x, y: Integer): TVec2i; overload;
  begin
    Result.x := x;
    Result.y := y;
  end;

  function Vec(const x, y, z: Integer): TVec3i; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
  end;

  function Vec(const xy: TVec2i; z: Integer): TVec3i; overload;
  begin
    Result.xy := xy;
    Result.z := z;
  end;

  function Vec(const x: Integer; const yz: TVec2i): TVec3i; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
  end;

  function Vec(const x, y, z, w: Integer): TVec4i; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
    Result.w := w;
  end;

  function Vec(const xy: TVec2i; const zw: TVec2i): TVec4i; overload;
  begin
    Result.xy := xy;
    Result.zw := zw;
  end;

  function Vec(const xyz: TVec3i; w: Integer): TVec4i; overload;
  begin
    Result.xyz := xyz;
    Result.w := w;
  end;

  function Vec(const x: Integer; const yz: TVec2i; w: Integer): TVec4i; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
    Result.w := w;
  end;

  function Vec(const x: Integer; const yzw: TVec3i): TVec4i; overload;
  begin
    Result.x := x;
    Result.y := yzw.x;
    Result.z := yzw.y;
    Result.w := yzw.z;
  end;

  function Equal (const v1, v2: TVec2i; const AEPS: Single = EPS): Boolean;
  var v: TVec2i;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS);
  end;

  function Equal (const v1, v2: TVec3i; const AEPS: Single = EPS): Boolean;
  var v: TVec3i;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS);
  end;

  function Equal (const v1, v2: TVec4i; const AEPS: Single = EPS): Boolean;
  var v: TVec4i;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS) and (System.abs(v.w)<AEPS);
  end;

  function Equal(const m1, m2: TMat2i; const AEPS: Single): Boolean;
  begin
     Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
               Equal(m1.Row[1], m2.Row[1], AEPS);
  end;

  function Equal(const m1, m2: TMat3i; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS);
  end;

  function Equal(const m1, m2: TMat4i; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS) and
              Equal(m1.Row[3], m2.Row[3], AEPS);
  end;

  procedure Swap(var v1, v2: TVec2i);
  var tmp: TVec2i;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TVec3i);
  var tmp: TVec3i;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TVec4i);
  var tmp: TVec4i;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat2i);
  var tmp: TMat2i;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat3i);
  var tmp: TMat3i;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat4i);
  var tmp: TMat4i;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  function Transpose(const m: TMat2i): TMat2i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
  end;

  function Transpose(const m: TMat3i): TMat3i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
  end;

  function Transpose(const m: TMat4i): TMat4i; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
    Result.Row[3] := m.Col[3];
  end;

  function Det(const m: TMat2i): Integer; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0,0]*m.f[1,1] - m.f[0,1]*m.f[1,0];
  end;

  function Det(const m: TMat3i): Integer; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := Dot(m.Row[0], Cross(m.Row[1], m.Row[2]));
  end;

  function Det(const m: TMat4i): Integer; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0, 0] * (m.f[1, 1] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) + m.f[1, 3] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1])) -
              m.f[0, 1] * (m.f[1, 0] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0])) +
              m.f[0, 2] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0])) -
              m.f[0, 3] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0]) + m.f[1, 2] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0]));
  end;
{$ENDIF}
{$UnDef Integer}
{$UnDef CastToInt} {$UnDef CastToSingle}
