{$Define Single}

{$IfDef Byte} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef ShortInt} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef Integer} {$Define CastToSingle} {$EndIf}

{$IFDEF INTF}
type
  { TVec2 }

  TVec2 = record
    x, y: Single;
  public
    function yx: TVec2; {$IFNDEF NoInline} inline; {$ENDIF}

    Class Operator Equal (const v1, v2: TVec2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec2; s: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec2): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec2; s: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
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
  PVec2 =^TVec2;

  { TVec3 }

  TVec3 = record
  public
    Class Operator Equal (const v1, v2: TVec3): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec3; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec3): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec3; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
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
    0: (f: array [0..2] of Single);
    1: (x, y, z: Single);
    2: (xy: TVec2);
  end;
  PVec3 =^TVec3;

  { TVec4 }

  TVec4 = record
  public
    Class Operator Equal (const v1, v2: TVec4): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec4; s: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TVec4): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TVec4; s: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
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
    0: (f: array [0..3] of Single);
    1: (x, y, z, w: Single);
    2: (xy, zw: TVec2);
    3: (xyz: TVec3);
  end;
  PVec4 =^TVec4;

  TVec2Arr = array of TVec2;
  TVec3Arr = array of TVec3;
  TVec4Arr = array of TVec4;

  { TMat2 }

  TMat2 = record
  private
    function GetCol(index: Integer): TVec2;               {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TVec2); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator Equal (const m1, m2: TMat2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat2): TMat2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat2): TMat2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat2): TMat2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat2; v: TVec2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat2; s: Single): TMat2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat2): TMat2; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    property Col[index: Integer]: TVec2 read GetCol write SetCol;
  case Byte of
    0: (f: array [0..1,0..1] of Single);
    1: (Row: array [0..1] of TVec2);
  end;
  PMat2 = ^TMat2;

  { TMat3 }

  TMat3 = record
  private
    function GetCol(index: Integer): TVec3;               {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TVec2;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TVec2;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TVec2;  {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TVec3); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TVec2); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TVec2); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TVec2); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator Equal (const m1, m2: TMat3): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat3): TMat3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat3): TMat3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat3): TMat3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec3; const m: TMat3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec2; const m: TMat3): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat3; s: Single): TMat3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat3): TMat3; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    property Col[index: Integer]: TVec3 read GetCol write SetCol;
    property OX: TVec2 read GetOX write SetOX;
    property OY: TVec2 read GetOY write SetOY;
    property Pos: TVec2 read GetPos write SetPos;
  case Byte of
    0: (f: array [0..2,0..2] of Single);
    1: (Row: array [0..2] of TVec3);
  end;
  PMat3 = ^TMat3;

  { TMat4 }

  TMat4 = record
  private
    function GetCol(index: Integer): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TVec3;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TVec3;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOZ: TVec3;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TVec3;  {$IFNDEF NoInline} inline; {$ENDIF}

    procedure SetCol(index: Integer; const Value: TVec4); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TVec3); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TVec3); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOZ(const AValue: TVec3); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TVec3); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator Equal (const m1, m2: TMat4): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TMat4): TMat4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TMat4): TMat4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TMat4): TMat4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec4; const m: TMat4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TVec3; const m: TMat4): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TMat4; s: Single): TMat4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TMat4): TMat4; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    function Mat3x3: TMat3;

    property Col[index: Integer]: TVec4 read GetCol write SetCol;
    property OX: TVec3 read GetOX write SetOX;
    property OY: TVec3 read GetOY write SetOY;
    property OZ: TVec3 read GetOZ write SetOZ;
    property Pos: TVec3 read GetPos write SetPos;
  case Byte of
    0: (f: array [0..3,0..3] of Single);
    1: (Row: array [0..3] of TVec4);
  end;
  PMat4 = ^TMat4;

  TMat2Arr = array of TMat2;
  TMat3Arr = array of TMat3;
  TMat4Arr = array of TMat4;

  function Abs(const V: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: Single; minval, maxval: Single): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TVec2; minval, maxval: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TVec3; minval, maxval: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec2): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TVec4): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Dot(const v1, v2: TVec2): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TVec3): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TVec4): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TVec2): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TVec3): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TVec2): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TVec3): Single; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Transpose(const m: TMat2): TMat2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TMat3): TMat3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TMat4): TMat4; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Det(const m: TMat2): Single; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TMat3): Single; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TMat4): Single; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Vec(const x, y: Single): TVec2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TVec2; z: Single): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: Single; const yz: TVec2): TVec3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z, w: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TVec2; const zw: TVec2): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xyz: TVec3; w: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: Single; const yz: TVec2; w: Single): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: Single; const yzw: TVec3): TVec4; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Equal (const v1, v2: TVec2; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TVec3; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TVec4; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat2; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat3; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TMat4; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;

  procedure Swap(var v1, v2: TVec2); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TVec3); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TVec4); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat2); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat3); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TMat4); overload; {$IFNDEF NoInline} inline; {$ENDIF}

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec2Arr); overload;
  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec3Arr); overload;
  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec4Arr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec2Arr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec3Arr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec4Arr); overload;
{$ENDIF}

{$IFDEF IMPL}
  { TVec2 }

  function TVec2.yx: TVec2;
  begin
    Result.x := y;
    Result.y := x;
  end;

  { TMat2 }

  function TMat2.GetCol(index: Integer): TVec2;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
  end;

  procedure TMat2.SetCol(index: Integer; const Value: TVec2);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
  end;

  { TMat3 }

  function TMat3.GetCol(index: Integer): TVec3;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
  end;

  function TMat3.GetOX: TVec2;
  begin
    Result := Row[0].xy;
  end;

  function TMat3.GetOY: TVec2;
  begin
    Result := Row[1].xy;
  end;

  function TMat3.GetPos: TVec2;
  begin
    Result := Row[2].xy;
  end;

  procedure TMat3.SetCol(index: Integer; const Value: TVec3);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
  end;

  procedure TMat3.SetOX(const AValue: TVec2);
  begin
    Row[0].xy := AValue;
  end;

  procedure TMat3.SetOY(const AValue: TVec2);
  begin
    Row[1].xy := AValue;
  end;

  procedure TMat3.SetPos(const AValue: TVec2);
  begin
    Row[2].xy := AValue;
  end;

  { TMat4 }

  function TMat4.GetCol(index: Integer): TVec4;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
    Result.w := f[3, index];
  end;

  function TMat4.GetOX: TVec3;
  begin
    Result := Row[0].xyz;
  end;

  function TMat4.GetOY: TVec3;
  begin
    Result := Row[1].xyz;
  end;

  function TMat4.GetOZ: TVec3;
  begin
    Result := Row[2].xyz;
  end;

  function TMat4.GetPos: TVec3;
  begin
    Result := Row[3].xyz;
  end;

  procedure TMat4.SetCol(index: Integer; const Value: TVec4);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
    f[3, index] := Value.w;
  end;

  procedure TMat4.SetOX(const AValue: TVec3);
  begin
    Row[0].xyz := AValue;
  end;

  procedure TMat4.SetOY(const AValue: TVec3);
  begin
    Row[1].xyz := AValue;
  end;

  procedure TMat4.SetOZ(const AValue: TVec3);
  begin
    Row[2].xyz := AValue;
  end;

  procedure TMat4.SetPos(const AValue: TVec3);
  begin
    Row[3].xyz := AValue;
  end;

  function TMat4.Mat3x3: TMat3;
  begin
    Result.Row[0] := Row[0].xyz;
    Result.Row[1] := Row[1].xyz;
    Result.Row[2] := Row[2].xyz;
  end;

  Class Operator TVec2.Equal (const v1, v2: TVec2): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y);
  end;

  Class Operator TVec2.Add (const v1, v2: TVec2): TVec2;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
  end;

  Class Operator TVec2.Subtract (const v1, v2: TVec2): TVec2;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
  end;

  Class Operator TVec2.Multiply (const v1, v2: TVec2): TVec2;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
  end;

  Class Operator TVec2.Multiply (const v1: TVec2; s: Single): TVec2;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
  end;

  Class Operator TVec2.Negative (const v: TVec2): TVec2;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
  end;

  {$IfDef CastToInt}
  Class Operator TVec2.Implicit (const V: TVec2): TVec2i;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec2.Implicit (const V: TVec2): TVec2;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;

  Class Operator TVec2.Multiply (const v1: TVec2; s: Single): TVec2;
  begin
    Result.x := V1.x * s;
    Result.y := V1.y * s;
  end;
  {$EndIf}

  {$IfDef Single}
  Class Operator TVec2.Divide (const v1, v2: TVec2): TVec2;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
  end;

  Class Operator TVec2.Divide (const v: TVec2; const f: Single): TVec2;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TVec2.Divide (const v1, v2: TVec2i): TVec2;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
  end;

  Class Operator TVec2.Divide (const v: TVec2i; const f: Single): TVec2;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
  end;
  {$EndIf}  

  Class Operator TVec3.Equal (const v1, v2: TVec3): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
  end;

  Class Operator TVec3.Add (const v1, v2: TVec3): TVec3;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
  end;

  Class Operator TVec3.Subtract (const v1, v2: TVec3): TVec3;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
  end;

  Class Operator TVec3.Multiply (const v1, v2: TVec3): TVec3;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
  end;

  Class Operator TVec3.Multiply (const v1: TVec3; s: Single): TVec3;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  end;

  Class Operator TVec3.Negative (const v: TVec3): TVec3;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
  end;

  {$IfDef CastToInt}
  Class Operator TVec3.Implicit (const V: TVec3): TVec3i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec3.Implicit (const V: TVec3): TVec3;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;

  Class Operator TVec3.Multiply (const v1: TVec3; s: Single): TVec3;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  End;
  {$EndIf}

  {$IfDef Single}
  Class Operator TVec3.Divide (const v1, v2: TVec3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
  end;

  Class Operator TVec3.Divide (const v: TVec3; const f: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TVec3.Divide (const v1, v2: TVec3i): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
  end;

  Class Operator TVec3.Divide (const v: TVec3i; const f: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
  end;
  {$EndIf}  

  Class Operator TVec4.Equal (const v1, v2: TVec4): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z) and (v1.w = v2.w);
  end;

  Class Operator TVec4.Add (const v1, v2: TVec4): TVec4;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
    Result.w := v1.w + v2.w;
  end;

  Class Operator TVec4.Subtract (const v1, v2: TVec4): TVec4;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
    Result.w := v1.w - v2.w;
  end;

  Class Operator TVec4.Multiply (const v1, v2: TVec4): TVec4;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
    Result.w := v1.w * v2.w;
  end;

  Class Operator TVec4.Multiply (const v1: TVec4; s: Single): TVec4;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  end;

  Class Operator TVec4.Negative (const v: TVec4): TVec4;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
    Result.w := - v.w;
  end;

  {$IfDef CastToInt}
  Class Operator TVec4.Implicit (const V: TVec4): TVec4i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TVec4.Implicit (const V: TVec4): TVec4;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;

  Class Operator TVec4.Multiply (const v1: TVec4; s: Single): TVec4;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  End;

  {$EndIf}

  {$IfDef Single}
  Class Operator TVec4.Divide (const v1, v2: TVec4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
    Result.w := v1.w / v2.w;
  end;

  Class Operator TVec4.Divide (const v: TVec4; const f: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
    Result.w := v.w / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TVec4.Divide (const v1, v2: TVec4i): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
    Result.w := v1.w / v2.w;
  end;

  Class Operator TVec4.Divide (const v: TVec4i; const f: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
    Result.w := v.w / f;
  end;
  {$EndIf}

  Class operator TMat2.Equal (const m1, m2: TMat2): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]);
  end;

  Class operator TMat2.Add (const m1, m2: TMat2): TMat2;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
  end;

  Class operator TMat2.Subtract (const m1, m2: TMat2): TMat2;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
  end;

  Class operator TMat2.Multiply (const m1, m2: TMat2): TMat2;
  var i, j: Integer;
  begin
    for j := 0 to 1 do
      for i := 0 to 1 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class operator TMat2.Multiply (const m: TMat2; v: TVec2): TVec2;
  begin
    Result.x := Dot(m.Row[0], v);
    Result.y := Dot(m.Row[1], v);
  end;

  Class operator TMat2.Multiply (const m: TMat2; s: Single): TMat2;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
  end;

  Class operator TMat2.Negative (const m: TMat2): TMat2;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
  end;

  Class Operator TMat3.Equal (const m1, m2: TMat3): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]);
  end;

  Class Operator TMat3.Add (const m1, m2: TMat3): TMat3;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
  end;

  Class Operator TMat3.Subtract (const m1, m2: TMat3): TMat3;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
  end;

  Class Operator TMat3.Multiply (const m1, m2: TMat3): TMat3;
  var i, j: Integer;
  begin
    for i := 0 to 2 do
      for j := 0 to 2 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TMat3.Multiply (const v: TVec3; const m: TMat3): TVec3;
  begin
    Result := m.Row[0]*v.x+
              m.Row[1]*v.y+
              m.Row[2]*v.z;
  end;

  Class Operator TMat3.Multiply (const v: TVec2; const m: TMat3): TVec2;
  begin
    Result := m.Row[0].xy*v.x+
              m.Row[1].xy*v.y+
              m.Row[2].xy;
  end;

  Class Operator TMat3.Multiply (const m: TMat3; s: Single): TMat3;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
  end;

  Class Operator TMat3.Negative (const m: TMat3): TMat3;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
  end;

  Class Operator TMat4.Equal (const m1, m2: TMat4): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]) and (m1.Row[3] = m2.Row[3]);
  end;

  Class Operator TMat4.Add (const m1, m2: TMat4): TMat4;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
    Result.Row[3] := m1.Row[3] + m2.Row[3];
  end;

  Class Operator TMat4.Subtract (const m1, m2: TMat4): TMat4;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
    Result.Row[3] := m1.Row[3] - m2.Row[3];
  end;

  Class Operator TMat4.Multiply (const m1, m2: TMat4): TMat4;
  var i, j: Integer;
  begin
    for i := 0 to 3 do
      for j := 0 to 3 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TMat4.Multiply (const v: TVec4; const m: TMat4): TVec4;
  begin
    Result := m.Row[0]*v.x +
              m.Row[1]*v.y +
              m.Row[2]*v.z +
              m.Row[3]*v.w;
  end;

  Class Operator TMat4.Multiply (const v: TVec3; const m: TMat4): TVec3;
  begin
    Result := m.Row[0].xyz*v.x +
              m.Row[1].xyz*v.y +
              m.Row[2].xyz*v.z +
              m.Row[3].xyz;
  end;

  Class Operator TMat4.Multiply (const m: TMat4; s: Single): TMat4;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
    Result.Row[3] := m.Row[3] * s;
  end;

  Class Operator TMat4.Negative (const m: TMat4): TMat4;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
    Result.Row[3] := - m.Row[3];
  end;

  function Abs(const V: TVec2): TVec2;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
  end;

  function Abs(const V: TVec3): TVec3;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
    Result.z := System.abs(V.z);
  end;

  function Min(const A, B: TVec2): TVec2; overload;
  begin
    Result.x := Math.Min(A.x, B.x);
    Result.y := Math.Min(A.y, B.y);
  end;
  function Min(const A, B: TVec3): TVec3; overload;
  begin
    Result.x := Min(A.x, B.x);
    Result.y := Min(A.y, B.y);
    Result.z := Min(A.z, B.z);
  end;
  function Max(const A, B: TVec2): TVec2; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
  end;
  function Max(const A, B: TVec3): TVec3; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
    Result.z := Max(A.z, B.z);
  end;

  function Clamp(const v: Single; minval, maxval: Single): Single; overload;
  begin
      Result := min(maxval, max(minval, v));
  end;
  function Clamp(const v: TVec2; minval, maxval: Single): TVec2; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
  end;
  function Clamp(const v: TVec3; minval, maxval: Single): TVec3; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
      Result.z := min(maxval, max(minval, v.z));
  end;

  function Sign(const V: TVec2): TVec2; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
  end;

  function Sign(const V: TVec3): TVec3; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
  end;

  function Sign(const V: TVec4): TVec4; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
    Result.w := sign(V.w);
  end;

  function Dot(const v1, v2: TVec2): Single; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y;
  end;

  function Dot(const v1, v2: TVec3): Single; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
  end;

  function Dot(const v1, v2: TVec4): Single; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
  end;

  function Cross(const v1, v2: TVec2): Single; overload;
  begin
    Result := v1.x * v2.y - v1.y * v2.x;
  end;

  function Cross(const v1, v2: TVec3): TVec3; overload;
  begin
    result.x := (v1.y*v2.z) - (v1.z*v2.y);
    result.y := (v1.z*v2.x) - (v1.x*v2.z);
    result.z := (v1.x*v2.y) - (v1.y*v2.x);
  end;

  function LenSqr(const v: TVec2): Single; overload;
  begin
    Result := dot(v, v);
  end;

  function LenSqr(const v: TVec3): Single; overload;
  begin
    Result := dot(v, v);
  end;

  function Vec(const x, y: Single): TVec2; overload;
  begin
    Result.x := x;
    Result.y := y;
  end;

  function Vec(const x, y, z: Single): TVec3; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
  end;

  function Vec(const xy: TVec2; z: Single): TVec3; overload;
  begin
    Result.xy := xy;
    Result.z := z;
  end;

  function Vec(const x: Single; const yz: TVec2): TVec3; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
  end;

  function Vec(const x, y, z, w: Single): TVec4; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
    Result.w := w;
  end;

  function Vec(const xy: TVec2; const zw: TVec2): TVec4; overload;
  begin
    Result.xy := xy;
    Result.zw := zw;
  end;

  function Vec(const xyz: TVec3; w: Single): TVec4; overload;
  begin
    Result.xyz := xyz;
    Result.w := w;
  end;

  function Vec(const x: Single; const yz: TVec2; w: Single): TVec4; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
    Result.w := w;
  end;

  function Vec(const x: Single; const yzw: TVec3): TVec4; overload;
  begin
    Result.x := x;
    Result.y := yzw.x;
    Result.z := yzw.y;
    Result.w := yzw.z;
  end;

  function Equal (const v1, v2: TVec2; const AEPS: Single = EPS): Boolean;
  var v: TVec2;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS);
  end;

  function Equal (const v1, v2: TVec3; const AEPS: Single = EPS): Boolean;
  var v: TVec3;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS);
  end;

  function Equal (const v1, v2: TVec4; const AEPS: Single = EPS): Boolean;
  var v: TVec4;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS) and (System.abs(v.w)<AEPS);
  end;

  function Equal(const m1, m2: TMat2; const AEPS: Single): Boolean;
  begin
     Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
               Equal(m1.Row[1], m2.Row[1], AEPS);
  end;

  function Equal(const m1, m2: TMat3; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS);
  end;

  function Equal(const m1, m2: TMat4; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS) and
              Equal(m1.Row[3], m2.Row[3], AEPS);
  end;

  procedure Swap(var v1, v2: TVec2);
  var tmp: TVec2;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TVec3);
  var tmp: TVec3;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TVec4);
  var tmp: TVec4;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat2);
  var tmp: TMat2;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat3);
  var tmp: TMat3;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TMat4);
  var tmp: TMat4;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  function Transpose(const m: TMat2): TMat2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
  end;

  function Transpose(const m: TMat3): TMat3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
  end;

  function Transpose(const m: TMat4): TMat4; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
    Result.Row[3] := m.Col[3];
  end;

  function Det(const m: TMat2): Single; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0,0]*m.f[1,1] - m.f[0,1]*m.f[1,0];
  end;

  function Det(const m: TMat3): Single; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := Dot(m.Row[0], Cross(m.Row[1], m.Row[2]));
  end;

  function Det(const m: TMat4): Single; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0, 0] * (m.f[1, 1] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) + m.f[1, 3] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1])) -
              m.f[0, 1] * (m.f[1, 0] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0])) +
              m.f[0, 2] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0])) -
              m.f[0, 3] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0]) + m.f[1, 2] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec2Arr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec3Arr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TVec4Arr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec2Arr);
  var n: Integer;
  begin
    n := 0;
    AStream.ReadBuffer(n, SizeOf(n));
    AArr := nil;
    SetLength(AArr, n);
    if n > 0 then
      AStream.ReadBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec3Arr);
  var n: Integer;
  begin
    n := 0;
    AStream.ReadBuffer(n, SizeOf(n));
    AArr := nil;
    SetLength(AArr, n);
    if n > 0 then
      AStream.ReadBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TVec4Arr);
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
{$UnDef Single}
{$UnDef CastToInt} {$UnDef CastToSingle}
