{$Define ShortInt}

{$IfDef Byte} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef ShortInt} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef Integer} {$Define CastToSingle} {$EndIf}

{$IFDEF INTF}
type
  { TVec2s }

  TVec2s = record
    x, y: ShortInt;
  public
    function yx: TVec2s; {$IFNDEF NoInline} inline; {$ENDIF}

    Class Operator NotEqual (const v1, v2: TVec2s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const v1, v2: TVec2s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec2s): TVec2s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec2s): TVec2s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec2s): TVec2s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec2s; s: ShortInt): TVec2s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec2s): TVec2s; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec2s): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec2s): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec2s; s: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef Single}
    Class Operator Divide (const v1, v2: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Divide (const v: TVec2; const f: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef Integer}
    Class Operator Divide (const v1, v2: TVec2i): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Divide (const v: TVec2i; const f: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}	
  end;
  PVec2s =^TVec2s;

  { TVec3s }

  TVec3s = record
  public
    Class Operator NotEqual (const v1, v2: TVec3s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const v1, v2: TVec3s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec3s): TVec3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec3s): TVec3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec3s): TVec3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec3s; s: ShortInt): TVec3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec3s): TVec3s; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec3s): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec3s): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec3s; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef Single}
    Class Operator Divide (const v1, v2: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Divide (const v: TVec3; const f: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef Integer}
    Class Operator Divide (const v1, v2: TVec3i): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Divide (const v: TVec3i; const f: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}	
  case Byte of
    0: (f: array [0..2] of ShortInt);
    1: (x, y, z: ShortInt);
    2: (xy: TVec2s);
  end;
  PVec3s =^TVec3s;

  { TVec4s }

  TVec4s = record
  public
    Class Operator NotEqual (const v1, v2: TVec4s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const v1, v2: TVec4s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec4s): TVec4s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec4s): TVec4s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec4s): TVec4s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec4s; s: ShortInt): TVec4s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec4s): TVec4s; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec4s): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec4s): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec4s; s: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef Single}
    Class Operator Divide (const v1, v2: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Divide (const v: TVec4; const f: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef Integer}
    Class Operator Divide (const v1, v2: TVec4i): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Divide (const v: TVec4i; const f: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}	
  case Byte of
    0: (f: array [0..3] of ShortInt);
    1: (x, y, z, w: ShortInt);
    2: (xy, zw: TVec2s);
    3: (xyz: TVec3s);
  end;
  PVec4s =^TVec4s;

  TVec2sArr = array of TVec2s;
  TVec3sArr = array of TVec3s;
  TVec4sArr = array of TVec4s;

  { TMat2s }

  TMat2s = record
  private
    function GetCol(index: Integer): TVec2s;               {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TVec2s); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator NotEqual (const m1, m2: TMat2s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const m1, m2: TMat2s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat2s): TMat2s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat2s): TMat2s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat2s): TMat2s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat2s; const v: TVec2s): TVec2s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec2s; const m: TMat2s): TVec2s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat2s; s: ShortInt): TMat2s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat2s): TMat2s; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    property Col[index: Integer]: TVec2s read GetCol write SetCol;
  case Byte of
    0: (f: array [0..1,0..1] of ShortInt);
    1: (Row: array [0..1] of TVec2s);
  end;
  PMat2s = ^TMat2s;

  { TMat3s }

  TMat3s = record
  private
    function GetCol(index: Integer): TVec3s;               {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TVec2s;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TVec2s;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TVec2s;  {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TVec3s); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TVec2s); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TVec2s); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TVec2s); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator NotEqual (const m1, m2: TMat3s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const m1, m2: TMat3s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat3s): TMat3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat3s): TMat3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat3s): TMat3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec3s; const m: TMat3s): TVec3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec2s; const m: TMat3s): TVec2s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat3s; s: ShortInt): TMat3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat3s): TMat3s; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    function Mat2x2: TMat2s;
    property Col[index: Integer]: TVec3s read GetCol write SetCol;
    property OX: TVec2s read GetOX write SetOX;
    property OY: TVec2s read GetOY write SetOY;
    property Pos: TVec2s read GetPos write SetPos;
  case Byte of
    0: (f: array [0..2,0..2] of ShortInt);
    1: (Row: array [0..2] of TVec3s);
  end;
  PMat3s = ^TMat3s;

  { TMat4s }

  TMat4s = record
  private
    function GetCol(index: Integer): TVec4s; {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TVec3s;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TVec3s;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOZ: TVec3s;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TVec3s;  {$IFNDEF NoInline} inline; {$ENDIF}

    procedure SetCol(index: Integer; const Value: TVec4s); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TVec3s); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TVec3s); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOZ(const AValue: TVec3s); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TVec3s); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator NotEqual (const m1, m2: TMat4s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const m1, m2: TMat4s): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat4s): TMat4s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat4s): TMat4s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat4s): TMat4s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec4s; const m: TMat4s): TVec4s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec3s; const m: TMat4s): TVec3s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat4s; s: ShortInt): TMat4s; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat4s): TMat4s; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    function Mat3x3: TMat3s;

    property Col[index: Integer]: TVec4s read GetCol write SetCol;
    property OX: TVec3s read GetOX write SetOX;
    property OY: TVec3s read GetOY write SetOY;
    property OZ: TVec3s read GetOZ write SetOZ;
    property Pos: TVec3s read GetPos write SetPos;
  case Byte of
    0: (f: array [0..3,0..3] of ShortInt);
    1: (Row: array [0..3] of TVec4s);
  end;
  PMat4s = ^TMat4s;

  TMat2sArr = array of TMat2s;
  TMat3sArr = array of TMat3s;
  TMat4sArr = array of TMat4s;

  function Abs(const V: TVec2s): TVec2s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: TVec3s): TVec3s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TVec2s): TVec2s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TVec3s): TVec3s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TVec2s): TVec2s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TVec3s): TVec3s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: ShortInt; minval, maxval: ShortInt): ShortInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TVec2s; minval, maxval: ShortInt): TVec2s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TVec3s; minval, maxval: ShortInt): TVec3s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TVec4s; minval, maxval: ShortInt): TVec4s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec2s): TVec2s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec3s): TVec3s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec4s): TVec4s; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Dot(const v1, v2: TVec2s): ShortInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TVec3s): ShortInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TVec4s): ShortInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TVec2s): ShortInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TVec3s): TVec3s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TVec2s): ShortInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TVec3s): ShortInt; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Transpose(const m: TMat2s): TMat2s; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TMat3s): TMat3s; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TMat4s): TMat4s; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Det(const m: TMat2s): ShortInt; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TMat3s): ShortInt; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TMat4s): ShortInt; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Vec(const x, y: ShortInt): TVec2s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z: ShortInt): TVec3s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TVec2s; z: ShortInt): TVec3s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: ShortInt; const yz: TVec2s): TVec3s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z, w: ShortInt): TVec4s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TVec2s; const zw: TVec2s): TVec4s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xyz: TVec3s; w: ShortInt): TVec4s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: ShortInt; const yz: TVec2s; w: ShortInt): TVec4s; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: ShortInt; const yzw: TVec3s): TVec4s; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Equal (const v1, v2: TVec2s; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TVec3s; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TVec4s; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat2s; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat3s; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat4s; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;

  procedure Swap(var v1, v2: TVec2s); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TVec3s); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TVec4s); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat2s); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat3s); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat4s); overload; {$IFNDEF NoInline} inline; {$ENDIF}

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec2sArr); overload;
  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec3sArr); overload;
  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec4sArr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec2sArr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec3sArr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec4sArr); overload;
{$ENDIF}

{$IFDEF IMPL}
  { TVec2s }

  function TVec2s.yx: TVec2s;
  begin
    Result.x := y;
    Result.y := x;
  end;

  { TMat2s }

  function TMat2s.GetCol(index: Integer): TVec2s;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
  end;

  procedure TMat2s.SetCol(index: Integer; const Value: TVec2s);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
  end;

  { TMat3s }

  function TMat3s.Mat2x2: TMat2s;
  begin
    Result.Row[0] := Row[0].xy;
    Result.Row[1] := Row[1].xy;
  end;

  function TMat3s.GetCol(index: Integer): TVec3s;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
  end;

  function TMat3s.GetOX: TVec2s;
  begin
    Result := Row[0].xy;
  end;

  function TMat3s.GetOY: TVec2s;
  begin
    Result := Row[1].xy;
  end;

  function TMat3s.GetPos: TVec2s;
  begin
    Result := Row[2].xy;
  end;

  procedure TMat3s.SetCol(index: Integer; const Value: TVec3s);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
  end;

  procedure TMat3s.SetOX(const AValue: TVec2s);
  begin
    Row[0].xy := AValue;
  end;

  procedure TMat3s.SetOY(const AValue: TVec2s);
  begin
    Row[1].xy := AValue;
  end;

  procedure TMat3s.SetPos(const AValue: TVec2s);
  begin
    Row[2].xy := AValue;
  end;

  { TMat4s }

  function TMat4s.GetCol(index: Integer): TVec4s;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
    Result.w := f[3, index];
  end;

  function TMat4s.GetOX: TVec3s;
  begin
    Result := Row[0].xyz;
  end;

  function TMat4s.GetOY: TVec3s;
  begin
    Result := Row[1].xyz;
  end;

  function TMat4s.GetOZ: TVec3s;
  begin
    Result := Row[2].xyz;
  end;

  function TMat4s.GetPos: TVec3s;
  begin
    Result := Row[3].xyz;
  end;

  procedure TMat4s.SetCol(index: Integer; const Value: TVec4s);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
    f[3, index] := Value.w;
  end;

  procedure TMat4s.SetOX(const AValue: TVec3s);
  begin
    Row[0].xyz := AValue;
  end;

  procedure TMat4s.SetOY(const AValue: TVec3s);
  begin
    Row[1].xyz := AValue;
  end;

  procedure TMat4s.SetOZ(const AValue: TVec3s);
  begin
    Row[2].xyz := AValue;
  end;

  procedure TMat4s.SetPos(const AValue: TVec3s);
  begin
    Row[3].xyz := AValue;
  end;

  function TMat4s.Mat3x3: TMat3s;
  begin
    Result.Row[0] := Row[0].xyz;
    Result.Row[1] := Row[1].xyz;
    Result.Row[2] := Row[2].xyz;
  end;

  Class Operator TVec2s.NotEqual (const v1, v2: TVec2s): Boolean;
  begin
    Result := (v1.x <> v2.x) or (v1.y <> v2.y);
  end;

  Class Operator TVec2s.Equal (const v1, v2: TVec2s): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y);
  end;

  Class Operator TVec2s.Add (const v1, v2: TVec2s): TVec2s;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
  end;

  Class Operator TVec2s.Subtract (const v1, v2: TVec2s): TVec2s;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
  end;

  Class Operator TVec2s.Multiply (const v1, v2: TVec2s): TVec2s;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
  end;

  Class Operator TVec2s.Multiply (const v1: TVec2s; s: ShortInt): TVec2s;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
  end;

  Class Operator TVec2s.Negative (const v: TVec2s): TVec2s;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
  end;

  {$IfDef CastToInt}
  Class Operator TVec2s.Implicit (const V: TVec2s): TVec2i;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec2s.Implicit (const V: TVec2s): TVec2;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;

  Class Operator TVec2s.Multiply (const v1: TVec2s; s: Single): TVec2;
  begin
    Result.x := V1.x * s;
    Result.y := V1.y * s;
  end;
  {$EndIf}

  {$IfDef Single}
  Class Operator TVec2s.Divide (const v1, v2: TVec2): TVec2;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
  end;

  Class Operator TVec2s.Divide (const v: TVec2; const f: Single): TVec2;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TVec2s.Divide (const v1, v2: TVec2i): TVec2;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
  end;

  Class Operator TVec2s.Divide (const v: TVec2i; const f: Single): TVec2;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
  end;
  {$EndIf}  

  Class Operator TVec3s.NotEqual (const v1, v2: TVec3s): Boolean;
  begin
    Result := (v1.x <> v2.x) or (v1.y <> v2.y) or (v1.z <> v2.z);
  end;

  Class Operator TVec3s.Equal (const v1, v2: TVec3s): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
  end;

  Class Operator TVec3s.Add (const v1, v2: TVec3s): TVec3s;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
  end;

  Class Operator TVec3s.Subtract (const v1, v2: TVec3s): TVec3s;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
  end;

  Class Operator TVec3s.Multiply (const v1, v2: TVec3s): TVec3s;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
  end;

  Class Operator TVec3s.Multiply (const v1: TVec3s; s: ShortInt): TVec3s;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  end;

  Class Operator TVec3s.Negative (const v: TVec3s): TVec3s;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
  end;

  {$IfDef CastToInt}
  Class Operator TVec3s.Implicit (const V: TVec3s): TVec3i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec3s.Implicit (const V: TVec3s): TVec3;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;

  Class Operator TVec3s.Multiply (const v1: TVec3s; s: Single): TVec3;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  End;
  {$EndIf}

  {$IfDef Single}
  Class Operator TVec3s.Divide (const v1, v2: TVec3): TVec3;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
  end;

  Class Operator TVec3s.Divide (const v: TVec3; const f: Single): TVec3;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TVec3s.Divide (const v1, v2: TVec3i): TVec3;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
  end;

  Class Operator TVec3s.Divide (const v: TVec3i; const f: Single): TVec3;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
  end;
  {$EndIf}  

  Class Operator TVec4s.NotEqual (const v1, v2: TVec4s): Boolean;
  begin
    Result := (v1.x <> v2.x) or (v1.y <> v2.y) or (v1.z <> v2.z) or (v1.w <> v2.w);
  end;

  Class Operator TVec4s.Equal (const v1, v2: TVec4s): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z) and (v1.w = v2.w);
  end;

  Class Operator TVec4s.Add (const v1, v2: TVec4s): TVec4s;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
    Result.w := v1.w + v2.w;
  end;

  Class Operator TVec4s.Subtract (const v1, v2: TVec4s): TVec4s;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
    Result.w := v1.w - v2.w;
  end;

  Class Operator TVec4s.Multiply (const v1, v2: TVec4s): TVec4s;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
    Result.w := v1.w * v2.w;
  end;

  Class Operator TVec4s.Multiply (const v1: TVec4s; s: ShortInt): TVec4s;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  end;

  Class Operator TVec4s.Negative (const v: TVec4s): TVec4s;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
    Result.w := - v.w;
  end;

  {$IfDef CastToInt}
  Class Operator TVec4s.Implicit (const V: TVec4s): TVec4i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec4s.Implicit (const V: TVec4s): TVec4;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;

  Class Operator TVec4s.Multiply (const v1: TVec4s; s: Single): TVec4;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  End;

  {$EndIf}

  {$IfDef Single}
  Class Operator TVec4s.Divide (const v1, v2: TVec4): TVec4;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
    Result.w := v1.w / v2.w;
  end;

  Class Operator TVec4s.Divide (const v: TVec4; const f: Single): TVec4;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
    Result.w := v.w / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TVec4s.Divide (const v1, v2: TVec4i): TVec4;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
    Result.w := v1.w / v2.w;
  end;

  Class Operator TVec4s.Divide (const v: TVec4i; const f: Single): TVec4;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
    Result.w := v.w / f;
  end;
  {$EndIf}

  Class operator TMat2s.NotEqual (const m1, m2: TMat2s): Boolean;
  begin
    Result := (m1.Row[0] <> m2.Row[0]) or (m1.Row[1] <> m2.Row[1]);
  end;

  Class operator TMat2s.Equal (const m1, m2: TMat2s): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]);
  end;

  Class operator TMat2s.Add (const m1, m2: TMat2s): TMat2s;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
  end;

  Class operator TMat2s.Subtract (const m1, m2: TMat2s): TMat2s;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
  end;

  Class operator TMat2s.Multiply (const m1, m2: TMat2s): TMat2s;
  var i, j: Integer;
  begin
    for j := 0 to 1 do
      for i := 0 to 1 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class operator TMat2s.Multiply (const m: TMat2s; const v: TVec2s): TVec2s;
  begin
    Result.x := Dot(m.Row[0], v);
    Result.y := Dot(m.Row[1], v);
  end;
  
  Class operator TMat2s.Multiply (const v: TVec2s; const m: TMat2s): TVec2s;
  begin
    Result := m.Row[0]*v.x + m.Row[1]*v.y;
  end;  

  Class operator TMat2s.Multiply (const m: TMat2s; s: ShortInt): TMat2s;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
  end;

  Class operator TMat2s.Negative (const m: TMat2s): TMat2s;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
  end;

  Class Operator TMat3s.NotEqual (const m1, m2: TMat3s): Boolean;
  begin
    Result := (m1.Row[0] <> m2.Row[0]) or (m1.Row[1] <> m2.Row[1]) or (m1.Row[2] <> m2.Row[2]);
  end;

  Class Operator TMat3s.Equal (const m1, m2: TMat3s): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]);
  end;

  Class Operator TMat3s.Add (const m1, m2: TMat3s): TMat3s;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
  end;

  Class Operator TMat3s.Subtract (const m1, m2: TMat3s): TMat3s;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
  end;

  Class Operator TMat3s.Multiply (const m1, m2: TMat3s): TMat3s;
  var i, j: Integer;
  begin
    for i := 0 to 2 do
      for j := 0 to 2 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TMat3s.Multiply (const v: TVec3s; const m: TMat3s): TVec3s;
  begin
    Result := m.Row[0]*v.x+
              m.Row[1]*v.y+
              m.Row[2]*v.z;
  end;

  Class Operator TMat3s.Multiply (const v: TVec2s; const m: TMat3s): TVec2s;
  begin
    Result := m.Row[0].xy*v.x+
              m.Row[1].xy*v.y+
              m.Row[2].xy;
  end;

  Class Operator TMat3s.Multiply (const m: TMat3s; s: ShortInt): TMat3s;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
  end;

  Class Operator TMat3s.Negative (const m: TMat3s): TMat3s;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
  end;

  Class Operator TMat4s.NotEqual (const m1, m2: TMat4s): Boolean;
  begin
    Result := (m1.Row[0] <> m2.Row[0]) or (m1.Row[1] <> m2.Row[1]) or (m1.Row[2] <> m2.Row[2]) or (m1.Row[3] <> m2.Row[3]);
  end;

  Class Operator TMat4s.Equal (const m1, m2: TMat4s): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]) and (m1.Row[3] = m2.Row[3]);
  end;

  Class Operator TMat4s.Add (const m1, m2: TMat4s): TMat4s;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
    Result.Row[3] := m1.Row[3] + m2.Row[3];
  end;

  Class Operator TMat4s.Subtract (const m1, m2: TMat4s): TMat4s;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
    Result.Row[3] := m1.Row[3] - m2.Row[3];
  end;

  Class Operator TMat4s.Multiply (const m1, m2: TMat4s): TMat4s;
  var i, j: Integer;
  begin
    for i := 0 to 3 do
      for j := 0 to 3 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TMat4s.Multiply (const v: TVec4s; const m: TMat4s): TVec4s;
  begin
    Result := m.Row[0]*v.x +
              m.Row[1]*v.y +
              m.Row[2]*v.z +
              m.Row[3]*v.w;
  end;

  Class Operator TMat4s.Multiply (const v: TVec3s; const m: TMat4s): TVec3s;
  begin
    Result := m.Row[0].xyz*v.x +
              m.Row[1].xyz*v.y +
              m.Row[2].xyz*v.z +
              m.Row[3].xyz;
  end;

  Class Operator TMat4s.Multiply (const m: TMat4s; s: ShortInt): TMat4s;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
    Result.Row[3] := m.Row[3] * s;
  end;

  Class Operator TMat4s.Negative (const m: TMat4s): TMat4s;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
    Result.Row[3] := - m.Row[3];
  end;

  function Abs(const V: TVec2s): TVec2s;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
  end;

  function Abs(const V: TVec3s): TVec3s;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
    Result.z := System.abs(V.z);
  end;

  function Min(const A, B: TVec2s): TVec2s; overload;
  begin
    Result.x := Math.Min(A.x, B.x);
    Result.y := Math.Min(A.y, B.y);
  end;
  function Min(const A, B: TVec3s): TVec3s; overload;
  begin
    Result.x := Min(A.x, B.x);
    Result.y := Min(A.y, B.y);
    Result.z := Min(A.z, B.z);
  end;
  function Max(const A, B: TVec2s): TVec2s; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
  end;
  function Max(const A, B: TVec3s): TVec3s; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
    Result.z := Max(A.z, B.z);
  end;

  function Clamp(const v: ShortInt; minval, maxval: ShortInt): ShortInt; overload;
  begin
      Result := min(maxval, max(minval, v));
  end;
  function Clamp(const v: TVec2s; minval, maxval: ShortInt): TVec2s; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
  end;
  function Clamp(const v: TVec3s; minval, maxval: ShortInt): TVec3s; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
      Result.z := min(maxval, max(minval, v.z));
  end;
  function Clamp(const v: TVec4s; minval, maxval: ShortInt): TVec4s; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
      Result.z := min(maxval, max(minval, v.z));
	  Result.w := min(maxval, max(minval, v.w));
  end;  

  function Sign(const V: TVec2s): TVec2s; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
  end;

  function Sign(const V: TVec3s): TVec3s; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
  end;

  function Sign(const V: TVec4s): TVec4s; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
    Result.w := sign(V.w);
  end;

  function Dot(const v1, v2: TVec2s): ShortInt; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y;
  end;

  function Dot(const v1, v2: TVec3s): ShortInt; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
  end;

  function Dot(const v1, v2: TVec4s): ShortInt; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
  end;

  function Cross(const v1, v2: TVec2s): ShortInt; overload;
  begin
    Result := v1.x * v2.y - v1.y * v2.x;
  end;

  function Cross(const v1, v2: TVec3s): TVec3s; overload;
  begin
    result.x := (v1.y*v2.z) - (v1.z*v2.y);
    result.y := (v1.z*v2.x) - (v1.x*v2.z);
    result.z := (v1.x*v2.y) - (v1.y*v2.x);
  end;

  function LenSqr(const v: TVec2s): ShortInt; overload;
  begin
    Result := dot(v, v);
  end;

  function LenSqr(const v: TVec3s): ShortInt; overload;
  begin
    Result := dot(v, v);
  end;

  function Vec(const x, y: ShortInt): TVec2s; overload;
  begin
    Result.x := x;
    Result.y := y;
  end;

  function Vec(const x, y, z: ShortInt): TVec3s; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
  end;

  function Vec(const xy: TVec2s; z: ShortInt): TVec3s; overload;
  begin
    Result.xy := xy;
    Result.z := z;
  end;

  function Vec(const x: ShortInt; const yz: TVec2s): TVec3s; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
  end;

  function Vec(const x, y, z, w: ShortInt): TVec4s; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
    Result.w := w;
  end;

  function Vec(const xy: TVec2s; const zw: TVec2s): TVec4s; overload;
  begin
    Result.xy := xy;
    Result.zw := zw;
  end;

  function Vec(const xyz: TVec3s; w: ShortInt): TVec4s; overload;
  begin
    Result.xyz := xyz;
    Result.w := w;
  end;

  function Vec(const x: ShortInt; const yz: TVec2s; w: ShortInt): TVec4s; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
    Result.w := w;
  end;

  function Vec(const x: ShortInt; const yzw: TVec3s): TVec4s; overload;
  begin
    Result.x := x;
    Result.y := yzw.x;
    Result.z := yzw.y;
    Result.w := yzw.z;
  end;

  function Equal (const v1, v2: TVec2s; const AEPS: Single = EPS): Boolean;
  var v: TVec2s;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS);
  end;

  function Equal (const v1, v2: TVec3s; const AEPS: Single = EPS): Boolean;
  var v: TVec3s;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS);
  end;

  function Equal (const v1, v2: TVec4s; const AEPS: Single = EPS): Boolean;
  var v: TVec4s;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS) and (System.abs(v.w)<AEPS);
  end;

  function Equal(const m1, m2: TMat2s; const AEPS: Single): Boolean;
  begin
     Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
               Equal(m1.Row[1], m2.Row[1], AEPS);
  end;

  function Equal(const m1, m2: TMat3s; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS);
  end;

  function Equal(const m1, m2: TMat4s; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS) and
              Equal(m1.Row[3], m2.Row[3], AEPS);
  end;

  procedure Swap(var v1, v2: TVec2s);
  var tmp: TVec2s;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TVec3s);
  var tmp: TVec3s;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TVec4s);
  var tmp: TVec4s;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat2s);
  var tmp: TMat2s;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat3s);
  var tmp: TMat3s;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat4s);
  var tmp: TMat4s;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  function Transpose(const m: TMat2s): TMat2s; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
  end;

  function Transpose(const m: TMat3s): TMat3s; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
  end;

  function Transpose(const m: TMat4s): TMat4s; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
    Result.Row[3] := m.Col[3];
  end;

  function Det(const m: TMat2s): ShortInt; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0,0]*m.f[1,1] - m.f[0,1]*m.f[1,0];
  end;

  function Det(const m: TMat3s): ShortInt; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := Dot(m.Row[0], Cross(m.Row[1], m.Row[2]));
  end;

  function Det(const m: TMat4s): ShortInt; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0, 0] * (m.f[1, 1] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) + m.f[1, 3] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1])) -
              m.f[0, 1] * (m.f[1, 0] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0])) +
              m.f[0, 2] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0])) -
              m.f[0, 3] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0]) + m.f[1, 2] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec2sArr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec3sArr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec4sArr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec2sArr);
  var n: Integer;
  begin
    n := 0;
    AStream.ReadBuffer(n, SizeOf(n));
    AArr := nil;
    SetLength(AArr, n);
    if n > 0 then
      AStream.ReadBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec3sArr);
  var n: Integer;
  begin
    n := 0;
    AStream.ReadBuffer(n, SizeOf(n));
    AArr := nil;
    SetLength(AArr, n);
    if n > 0 then
      AStream.ReadBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec4sArr);
  var n: Integer;
  begin
    n := 0;
    AStream.ReadBuffer(n, SizeOf(n));
    AArr := nil;
    SetLength(AArr, n);
    if n > 0 then
      AStream.ReadBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

{$ENDIF}
{$UnDef ShortInt}
{$UnDef CastToInt} {$UnDef CastToSingle}
