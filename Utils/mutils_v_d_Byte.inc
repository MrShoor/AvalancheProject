{$Define Byte}

{$IfDef Byte} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef ShortInt} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef Integer} {$Define CastToSingle} {$EndIf}

{$IFDEF INTF}
type
  { TVec2b }

  TVec2b = record
    x, y: Byte;
  public
    function yx: TVec2b; {$IFNDEF NoInline} inline; {$ENDIF}

    Class Operator NotEqual (const v1, v2: TVec2b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const v1, v2: TVec2b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec2b): TVec2b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec2b): TVec2b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec2b): TVec2b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec2b; s: Byte): TVec2b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec2b): TVec2b; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec2b): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec2b): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec2b; s: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
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
  PVec2b =^TVec2b;

  { TVec3b }

  TVec3b = record
  public
    Class Operator NotEqual (const v1, v2: TVec3b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const v1, v2: TVec3b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec3b): TVec3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec3b): TVec3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec3b): TVec3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec3b; s: Byte): TVec3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec3b): TVec3b; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec3b): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec3b): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec3b; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
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
    0: (f: array [0..2] of Byte);
    1: (x, y, z: Byte);
    2: (xy: TVec2b);
  end;
  PVec3b =^TVec3b;

  { TVec4b }

  TVec4b = record
  public
    Class Operator NotEqual (const v1, v2: TVec4b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const v1, v2: TVec4b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec4b): TVec4b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec4b): TVec4b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec4b): TVec4b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec4b; s: Byte): TVec4b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec4b): TVec4b; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec4b): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec4b): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec4b; s: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
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
    0: (f: array [0..3] of Byte);
    1: (x, y, z, w: Byte);
    2: (xy, zw: TVec2b);
    3: (xyz: TVec3b);
  end;
  PVec4b =^TVec4b;

  TVec2bArr = array of TVec2b;
  TVec3bArr = array of TVec3b;
  TVec4bArr = array of TVec4b;

  { TMat2b }

  TMat2b = record
  private
    function GetCol(index: Integer): TVec2b;               {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TVec2b); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator NotEqual (const m1, m2: TMat2b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const m1, m2: TMat2b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat2b): TMat2b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat2b): TMat2b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat2b): TMat2b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat2b; const v: TVec2b): TVec2b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec2b; const m: TMat2b): TVec2b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat2b; s: Byte): TMat2b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat2b): TMat2b; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    property Col[index: Integer]: TVec2b read GetCol write SetCol;
  case Byte of
    0: (f: array [0..1,0..1] of Byte);
    1: (Row: array [0..1] of TVec2b);
  end;
  PMat2b = ^TMat2b;

  { TMat3b }

  TMat3b = record
  private
    function GetCol(index: Integer): TVec3b;               {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TVec2b;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TVec2b;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TVec2b;  {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TVec3b); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TVec2b); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TVec2b); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TVec2b); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator NotEqual (const m1, m2: TMat3b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const m1, m2: TMat3b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat3b): TMat3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat3b): TMat3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat3b): TMat3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec3b; const m: TMat3b): TVec3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec2b; const m: TMat3b): TVec2b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat3b; s: Byte): TMat3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat3b): TMat3b; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    function Mat2x2: TMat2b;
    property Col[index: Integer]: TVec3b read GetCol write SetCol;
    property OX: TVec2b read GetOX write SetOX;
    property OY: TVec2b read GetOY write SetOY;
    property Pos: TVec2b read GetPos write SetPos;
  case Byte of
    0: (f: array [0..2,0..2] of Byte);
    1: (Row: array [0..2] of TVec3b);
  end;
  PMat3b = ^TMat3b;

  { TMat4b }

  TMat4b = record
  private
    function GetCol(index: Integer): TVec4b; {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TVec3b;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TVec3b;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOZ: TVec3b;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TVec3b;  {$IFNDEF NoInline} inline; {$ENDIF}

    procedure SetCol(index: Integer; const Value: TVec4b); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TVec3b); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TVec3b); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOZ(const AValue: TVec3b); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TVec3b); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator NotEqual (const m1, m2: TMat4b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const m1, m2: TMat4b): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat4b): TMat4b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat4b): TMat4b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat4b): TMat4b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec4b; const m: TMat4b): TVec4b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec3b; const m: TMat4b): TVec3b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat4b; s: Byte): TMat4b; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat4b): TMat4b; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    function Mat3x3: TMat3b;

    property Col[index: Integer]: TVec4b read GetCol write SetCol;
    property OX: TVec3b read GetOX write SetOX;
    property OY: TVec3b read GetOY write SetOY;
    property OZ: TVec3b read GetOZ write SetOZ;
    property Pos: TVec3b read GetPos write SetPos;
  case Byte of
    0: (f: array [0..3,0..3] of Byte);
    1: (Row: array [0..3] of TVec4b);
  end;
  PMat4b = ^TMat4b;

  TMat2bArr = array of TMat2b;
  TMat3bArr = array of TMat3b;
  TMat4bArr = array of TMat4b;

  function Abs(const V: TVec2b): TVec2b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: TVec3b): TVec3b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TVec2b): TVec2b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TVec3b): TVec3b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TVec2b): TVec2b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TVec3b): TVec3b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: Byte; minval, maxval: Byte): Byte; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TVec2b; minval, maxval: Byte): TVec2b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TVec3b; minval, maxval: Byte): TVec3b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec2b): TVec2b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec3b): TVec3b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec4b): TVec4b; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Dot(const v1, v2: TVec2b): Byte; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TVec3b): Byte; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TVec4b): Byte; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TVec2b): Byte; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TVec3b): TVec3b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TVec2b): Byte; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TVec3b): Byte; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Transpose(const m: TMat2b): TMat2b; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TMat3b): TMat3b; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TMat4b): TMat4b; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Det(const m: TMat2b): Byte; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TMat3b): Byte; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TMat4b): Byte; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Vec(const x, y: Byte): TVec2b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z: Byte): TVec3b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TVec2b; z: Byte): TVec3b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: Byte; const yz: TVec2b): TVec3b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z, w: Byte): TVec4b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TVec2b; const zw: TVec2b): TVec4b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xyz: TVec3b; w: Byte): TVec4b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: Byte; const yz: TVec2b; w: Byte): TVec4b; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: Byte; const yzw: TVec3b): TVec4b; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Equal (const v1, v2: TVec2b; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TVec3b; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TVec4b; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat2b; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat3b; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat4b; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;

  procedure Swap(var v1, v2: TVec2b); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TVec3b); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TVec4b); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat2b); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat3b); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat4b); overload; {$IFNDEF NoInline} inline; {$ENDIF}

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec2bArr); overload;
  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec3bArr); overload;
  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec4bArr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec2bArr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec3bArr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec4bArr); overload;
{$ENDIF}

{$IFDEF IMPL}
  { TVec2b }

  function TVec2b.yx: TVec2b;
  begin
    Result.x := y;
    Result.y := x;
  end;

  { TMat2b }

  function TMat2b.GetCol(index: Integer): TVec2b;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
  end;

  procedure TMat2b.SetCol(index: Integer; const Value: TVec2b);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
  end;

  { TMat3b }

  function TMat3b.Mat2x2: TMat2b;
  begin
    Result.Row[0] := Row[0].xy;
    Result.Row[1] := Row[1].xy;
  end;

  function TMat3b.GetCol(index: Integer): TVec3b;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
  end;

  function TMat3b.GetOX: TVec2b;
  begin
    Result := Row[0].xy;
  end;

  function TMat3b.GetOY: TVec2b;
  begin
    Result := Row[1].xy;
  end;

  function TMat3b.GetPos: TVec2b;
  begin
    Result := Row[2].xy;
  end;

  procedure TMat3b.SetCol(index: Integer; const Value: TVec3b);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
  end;

  procedure TMat3b.SetOX(const AValue: TVec2b);
  begin
    Row[0].xy := AValue;
  end;

  procedure TMat3b.SetOY(const AValue: TVec2b);
  begin
    Row[1].xy := AValue;
  end;

  procedure TMat3b.SetPos(const AValue: TVec2b);
  begin
    Row[2].xy := AValue;
  end;

  { TMat4b }

  function TMat4b.GetCol(index: Integer): TVec4b;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
    Result.w := f[3, index];
  end;

  function TMat4b.GetOX: TVec3b;
  begin
    Result := Row[0].xyz;
  end;

  function TMat4b.GetOY: TVec3b;
  begin
    Result := Row[1].xyz;
  end;

  function TMat4b.GetOZ: TVec3b;
  begin
    Result := Row[2].xyz;
  end;

  function TMat4b.GetPos: TVec3b;
  begin
    Result := Row[3].xyz;
  end;

  procedure TMat4b.SetCol(index: Integer; const Value: TVec4b);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
    f[3, index] := Value.w;
  end;

  procedure TMat4b.SetOX(const AValue: TVec3b);
  begin
    Row[0].xyz := AValue;
  end;

  procedure TMat4b.SetOY(const AValue: TVec3b);
  begin
    Row[1].xyz := AValue;
  end;

  procedure TMat4b.SetOZ(const AValue: TVec3b);
  begin
    Row[2].xyz := AValue;
  end;

  procedure TMat4b.SetPos(const AValue: TVec3b);
  begin
    Row[3].xyz := AValue;
  end;

  function TMat4b.Mat3x3: TMat3b;
  begin
    Result.Row[0] := Row[0].xyz;
    Result.Row[1] := Row[1].xyz;
    Result.Row[2] := Row[2].xyz;
  end;

  Class Operator TVec2b.NotEqual (const v1, v2: TVec2b): Boolean;
  begin
    Result := (v1.x <> v2.x) or (v1.y <> v2.y);
  end;

  Class Operator TVec2b.Equal (const v1, v2: TVec2b): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y);
  end;

  Class Operator TVec2b.Add (const v1, v2: TVec2b): TVec2b;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
  end;

  Class Operator TVec2b.Subtract (const v1, v2: TVec2b): TVec2b;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
  end;

  Class Operator TVec2b.Multiply (const v1, v2: TVec2b): TVec2b;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
  end;

  Class Operator TVec2b.Multiply (const v1: TVec2b; s: Byte): TVec2b;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
  end;

  Class Operator TVec2b.Negative (const v: TVec2b): TVec2b;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
  end;

  {$IfDef CastToInt}
  Class Operator TVec2b.Implicit (const V: TVec2b): TVec2i;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec2b.Implicit (const V: TVec2b): TVec2;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;

  Class Operator TVec2b.Multiply (const v1: TVec2b; s: Single): TVec2;
  begin
    Result.x := V1.x * s;
    Result.y := V1.y * s;
  end;
  {$EndIf}

  {$IfDef Single}
  Class Operator TVec2b.Divide (const v1, v2: TVec2): TVec2;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
  end;

  Class Operator TVec2b.Divide (const v: TVec2; const f: Single): TVec2;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TVec2b.Divide (const v1, v2: TVec2i): TVec2;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
  end;

  Class Operator TVec2b.Divide (const v: TVec2i; const f: Single): TVec2;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
  end;
  {$EndIf}  

  Class Operator TVec3b.NotEqual (const v1, v2: TVec3b): Boolean;
  begin
    Result := (v1.x <> v2.x) or (v1.y <> v2.y) or (v1.z <> v2.z);
  end;

  Class Operator TVec3b.Equal (const v1, v2: TVec3b): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
  end;

  Class Operator TVec3b.Add (const v1, v2: TVec3b): TVec3b;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
  end;

  Class Operator TVec3b.Subtract (const v1, v2: TVec3b): TVec3b;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
  end;

  Class Operator TVec3b.Multiply (const v1, v2: TVec3b): TVec3b;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
  end;

  Class Operator TVec3b.Multiply (const v1: TVec3b; s: Byte): TVec3b;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  end;

  Class Operator TVec3b.Negative (const v: TVec3b): TVec3b;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
  end;

  {$IfDef CastToInt}
  Class Operator TVec3b.Implicit (const V: TVec3b): TVec3i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec3b.Implicit (const V: TVec3b): TVec3;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;

  Class Operator TVec3b.Multiply (const v1: TVec3b; s: Single): TVec3;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  End;
  {$EndIf}

  {$IfDef Single}
  Class Operator TVec3b.Divide (const v1, v2: TVec3): TVec3;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
  end;

  Class Operator TVec3b.Divide (const v: TVec3; const f: Single): TVec3;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TVec3b.Divide (const v1, v2: TVec3i): TVec3;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
  end;

  Class Operator TVec3b.Divide (const v: TVec3i; const f: Single): TVec3;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
  end;
  {$EndIf}  

  Class Operator TVec4b.NotEqual (const v1, v2: TVec4b): Boolean;
  begin
    Result := (v1.x <> v2.x) or (v1.y <> v2.y) or (v1.z <> v2.z) or (v1.w <> v2.w);
  end;

  Class Operator TVec4b.Equal (const v1, v2: TVec4b): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z) and (v1.w = v2.w);
  end;

  Class Operator TVec4b.Add (const v1, v2: TVec4b): TVec4b;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
    Result.w := v1.w + v2.w;
  end;

  Class Operator TVec4b.Subtract (const v1, v2: TVec4b): TVec4b;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
    Result.w := v1.w - v2.w;
  end;

  Class Operator TVec4b.Multiply (const v1, v2: TVec4b): TVec4b;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
    Result.w := v1.w * v2.w;
  end;

  Class Operator TVec4b.Multiply (const v1: TVec4b; s: Byte): TVec4b;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  end;

  Class Operator TVec4b.Negative (const v: TVec4b): TVec4b;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
    Result.w := - v.w;
  end;

  {$IfDef CastToInt}
  Class Operator TVec4b.Implicit (const V: TVec4b): TVec4i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec4b.Implicit (const V: TVec4b): TVec4;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;

  Class Operator TVec4b.Multiply (const v1: TVec4b; s: Single): TVec4;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  End;

  {$EndIf}

  {$IfDef Single}
  Class Operator TVec4b.Divide (const v1, v2: TVec4): TVec4;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
    Result.w := v1.w / v2.w;
  end;

  Class Operator TVec4b.Divide (const v: TVec4; const f: Single): TVec4;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
    Result.w := v.w / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TVec4b.Divide (const v1, v2: TVec4i): TVec4;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
    Result.w := v1.w / v2.w;
  end;

  Class Operator TVec4b.Divide (const v: TVec4i; const f: Single): TVec4;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
    Result.w := v.w / f;
  end;
  {$EndIf}

  Class operator TMat2b.NotEqual (const m1, m2: TMat2b): Boolean;
  begin
    Result := (m1.Row[0] <> m2.Row[0]) or (m1.Row[1] <> m2.Row[1]);
  end;

  Class operator TMat2b.Equal (const m1, m2: TMat2b): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]);
  end;

  Class operator TMat2b.Add (const m1, m2: TMat2b): TMat2b;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
  end;

  Class operator TMat2b.Subtract (const m1, m2: TMat2b): TMat2b;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
  end;

  Class operator TMat2b.Multiply (const m1, m2: TMat2b): TMat2b;
  var i, j: Integer;
  begin
    for j := 0 to 1 do
      for i := 0 to 1 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class operator TMat2b.Multiply (const m: TMat2b; const v: TVec2b): TVec2b;
  begin
    Result.x := Dot(m.Row[0], v);
    Result.y := Dot(m.Row[1], v);
  end;
  
  Class operator TMat2b.Multiply (const v: TVec2b; const m: TMat2b): TVec2b;
  begin
    Result := m.Row[0]*v.x + m.Row[1]*v.y;
  end;  

  Class operator TMat2b.Multiply (const m: TMat2b; s: Byte): TMat2b;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
  end;

  Class operator TMat2b.Negative (const m: TMat2b): TMat2b;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
  end;

  Class Operator TMat3b.NotEqual (const m1, m2: TMat3b): Boolean;
  begin
    Result := (m1.Row[0] <> m2.Row[0]) or (m1.Row[1] <> m2.Row[1]) or (m1.Row[2] <> m2.Row[2]);
  end;

  Class Operator TMat3b.Equal (const m1, m2: TMat3b): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]);
  end;

  Class Operator TMat3b.Add (const m1, m2: TMat3b): TMat3b;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
  end;

  Class Operator TMat3b.Subtract (const m1, m2: TMat3b): TMat3b;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
  end;

  Class Operator TMat3b.Multiply (const m1, m2: TMat3b): TMat3b;
  var i, j: Integer;
  begin
    for i := 0 to 2 do
      for j := 0 to 2 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TMat3b.Multiply (const v: TVec3b; const m: TMat3b): TVec3b;
  begin
    Result := m.Row[0]*v.x+
              m.Row[1]*v.y+
              m.Row[2]*v.z;
  end;

  Class Operator TMat3b.Multiply (const v: TVec2b; const m: TMat3b): TVec2b;
  begin
    Result := m.Row[0].xy*v.x+
              m.Row[1].xy*v.y+
              m.Row[2].xy;
  end;

  Class Operator TMat3b.Multiply (const m: TMat3b; s: Byte): TMat3b;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
  end;

  Class Operator TMat3b.Negative (const m: TMat3b): TMat3b;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
  end;

  Class Operator TMat4b.NotEqual (const m1, m2: TMat4b): Boolean;
  begin
    Result := (m1.Row[0] <> m2.Row[0]) or (m1.Row[1] <> m2.Row[1]) or (m1.Row[2] <> m2.Row[2]) or (m1.Row[3] <> m2.Row[3]);
  end;

  Class Operator TMat4b.Equal (const m1, m2: TMat4b): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]) and (m1.Row[3] = m2.Row[3]);
  end;

  Class Operator TMat4b.Add (const m1, m2: TMat4b): TMat4b;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
    Result.Row[3] := m1.Row[3] + m2.Row[3];
  end;

  Class Operator TMat4b.Subtract (const m1, m2: TMat4b): TMat4b;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
    Result.Row[3] := m1.Row[3] - m2.Row[3];
  end;

  Class Operator TMat4b.Multiply (const m1, m2: TMat4b): TMat4b;
  var i, j: Integer;
  begin
    for i := 0 to 3 do
      for j := 0 to 3 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TMat4b.Multiply (const v: TVec4b; const m: TMat4b): TVec4b;
  begin
    Result := m.Row[0]*v.x +
              m.Row[1]*v.y +
              m.Row[2]*v.z +
              m.Row[3]*v.w;
  end;

  Class Operator TMat4b.Multiply (const v: TVec3b; const m: TMat4b): TVec3b;
  begin
    Result := m.Row[0].xyz*v.x +
              m.Row[1].xyz*v.y +
              m.Row[2].xyz*v.z +
              m.Row[3].xyz;
  end;

  Class Operator TMat4b.Multiply (const m: TMat4b; s: Byte): TMat4b;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
    Result.Row[3] := m.Row[3] * s;
  end;

  Class Operator TMat4b.Negative (const m: TMat4b): TMat4b;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
    Result.Row[3] := - m.Row[3];
  end;

  function Abs(const V: TVec2b): TVec2b;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
  end;

  function Abs(const V: TVec3b): TVec3b;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
    Result.z := System.abs(V.z);
  end;

  function Min(const A, B: TVec2b): TVec2b; overload;
  begin
    Result.x := Math.Min(A.x, B.x);
    Result.y := Math.Min(A.y, B.y);
  end;
  function Min(const A, B: TVec3b): TVec3b; overload;
  begin
    Result.x := Min(A.x, B.x);
    Result.y := Min(A.y, B.y);
    Result.z := Min(A.z, B.z);
  end;
  function Max(const A, B: TVec2b): TVec2b; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
  end;
  function Max(const A, B: TVec3b): TVec3b; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
    Result.z := Max(A.z, B.z);
  end;

  function Clamp(const v: Byte; minval, maxval: Byte): Byte; overload;
  begin
      Result := min(maxval, max(minval, v));
  end;
  function Clamp(const v: TVec2b; minval, maxval: Byte): TVec2b; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
  end;
  function Clamp(const v: TVec3b; minval, maxval: Byte): TVec3b; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
      Result.z := min(maxval, max(minval, v.z));
  end;

  function Sign(const V: TVec2b): TVec2b; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
  end;

  function Sign(const V: TVec3b): TVec3b; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
  end;

  function Sign(const V: TVec4b): TVec4b; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
    Result.w := sign(V.w);
  end;

  function Dot(const v1, v2: TVec2b): Byte; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y;
  end;

  function Dot(const v1, v2: TVec3b): Byte; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
  end;

  function Dot(const v1, v2: TVec4b): Byte; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
  end;

  function Cross(const v1, v2: TVec2b): Byte; overload;
  begin
    Result := v1.x * v2.y - v1.y * v2.x;
  end;

  function Cross(const v1, v2: TVec3b): TVec3b; overload;
  begin
    result.x := (v1.y*v2.z) - (v1.z*v2.y);
    result.y := (v1.z*v2.x) - (v1.x*v2.z);
    result.z := (v1.x*v2.y) - (v1.y*v2.x);
  end;

  function LenSqr(const v: TVec2b): Byte; overload;
  begin
    Result := dot(v, v);
  end;

  function LenSqr(const v: TVec3b): Byte; overload;
  begin
    Result := dot(v, v);
  end;

  function Vec(const x, y: Byte): TVec2b; overload;
  begin
    Result.x := x;
    Result.y := y;
  end;

  function Vec(const x, y, z: Byte): TVec3b; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
  end;

  function Vec(const xy: TVec2b; z: Byte): TVec3b; overload;
  begin
    Result.xy := xy;
    Result.z := z;
  end;

  function Vec(const x: Byte; const yz: TVec2b): TVec3b; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
  end;

  function Vec(const x, y, z, w: Byte): TVec4b; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
    Result.w := w;
  end;

  function Vec(const xy: TVec2b; const zw: TVec2b): TVec4b; overload;
  begin
    Result.xy := xy;
    Result.zw := zw;
  end;

  function Vec(const xyz: TVec3b; w: Byte): TVec4b; overload;
  begin
    Result.xyz := xyz;
    Result.w := w;
  end;

  function Vec(const x: Byte; const yz: TVec2b; w: Byte): TVec4b; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
    Result.w := w;
  end;

  function Vec(const x: Byte; const yzw: TVec3b): TVec4b; overload;
  begin
    Result.x := x;
    Result.y := yzw.x;
    Result.z := yzw.y;
    Result.w := yzw.z;
  end;

  function Equal (const v1, v2: TVec2b; const AEPS: Single = EPS): Boolean;
  var v: TVec2b;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS);
  end;

  function Equal (const v1, v2: TVec3b; const AEPS: Single = EPS): Boolean;
  var v: TVec3b;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS);
  end;

  function Equal (const v1, v2: TVec4b; const AEPS: Single = EPS): Boolean;
  var v: TVec4b;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS) and (System.abs(v.w)<AEPS);
  end;

  function Equal(const m1, m2: TMat2b; const AEPS: Single): Boolean;
  begin
     Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
               Equal(m1.Row[1], m2.Row[1], AEPS);
  end;

  function Equal(const m1, m2: TMat3b; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS);
  end;

  function Equal(const m1, m2: TMat4b; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS) and
              Equal(m1.Row[3], m2.Row[3], AEPS);
  end;

  procedure Swap(var v1, v2: TVec2b);
  var tmp: TVec2b;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TVec3b);
  var tmp: TVec3b;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TVec4b);
  var tmp: TVec4b;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat2b);
  var tmp: TMat2b;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat3b);
  var tmp: TMat3b;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat4b);
  var tmp: TMat4b;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  function Transpose(const m: TMat2b): TMat2b; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
  end;

  function Transpose(const m: TMat3b): TMat3b; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
  end;

  function Transpose(const m: TMat4b): TMat4b; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
    Result.Row[3] := m.Col[3];
  end;

  function Det(const m: TMat2b): Byte; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0,0]*m.f[1,1] - m.f[0,1]*m.f[1,0];
  end;

  function Det(const m: TMat3b): Byte; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := Dot(m.Row[0], Cross(m.Row[1], m.Row[2]));
  end;

  function Det(const m: TMat4b): Byte; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0, 0] * (m.f[1, 1] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) + m.f[1, 3] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1])) -
              m.f[0, 1] * (m.f[1, 0] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0])) +
              m.f[0, 2] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0])) -
              m.f[0, 3] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0]) + m.f[1, 2] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec2bArr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec3bArr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec4bArr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec2bArr);
  var n: Integer;
  begin
    n := 0;
    AStream.ReadBuffer(n, SizeOf(n));
    AArr := nil;
    SetLength(AArr, n);
    if n > 0 then
      AStream.ReadBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec3bArr);
  var n: Integer;
  begin
    n := 0;
    AStream.ReadBuffer(n, SizeOf(n));
    AArr := nil;
    SetLength(AArr, n);
    if n > 0 then
      AStream.ReadBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec4bArr);
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
{$UnDef Byte}
{$UnDef CastToInt} {$UnDef CastToSingle}
