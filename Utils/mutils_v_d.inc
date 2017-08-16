{$Define TCompType}

{$IfDef Byte} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef ShortInt} {$Define CastToInt} {$Define CastToSingle} {$EndIf}
{$IfDef Integer} {$Define CastToSingle} {$EndIf}

{$IFDEF INTF}
type
  { TV2 }

  TV2 = record
    x, y: TCompType;
  public
    function yx: TV2; {$IFNDEF NoInline} inline; {$ENDIF}

    Class Operator NotEqual (const v1, v2: TV2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const v1, v2: TV2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TV2; s: TCompType): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TV2): TVec2i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TV2): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TV2; s: Single): TVec2; {$IFNDEF NoInline} inline; {$ENDIF}
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
  PV2 =^TV2;

  { TV3 }

  TV3 = record
  public
    Class Operator NotEqual (const v1, v2: TV3): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const v1, v2: TV3): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TV3; s: TCompType): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TV3): TVec3i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TV3): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TV3; s: Single): TVec3; {$IFNDEF NoInline} inline; {$ENDIF}
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
    0: (f: array [0..2] of TCompType);
    1: (x, y, z: TCompType);
    2: (xy: TV2);
  end;
  PV3 =^TV3;

  { TV4 }

  TV4 = record
  public
    Class Operator NotEqual (const v1, v2: TV4): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const v1, v2: TV4): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const v1, v2: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const v1, v2: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1, v2: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TV4; s: TCompType): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const v: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}

    {$IfDef CastToInt}
    Class Operator Implicit (const V: TV4): TVec4i; {$IFNDEF NoInline} inline; {$ENDIF}
    {$EndIf}
    {$IfDef CastToSingle}
    Class Operator Implicit (const V: TV4): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v1: TV4; s: Single): TVec4; {$IFNDEF NoInline} inline; {$ENDIF}
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
    0: (f: array [0..3] of TCompType);
    1: (x, y, z, w: TCompType);
    2: (xy, zw: TV2);
    3: (xyz: TV3);
  end;
  PV4 =^TV4;

  TV2Arr = array of TV2;
  TV3Arr = array of TV3;
  TV4Arr = array of TV4;

  { TM2 }

  TM2 = record
  private
    function GetCol(index: Integer): TV2;               {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TV2); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator NotEqual (const m1, m2: TM2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const m1, m2: TM2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TM2): TM2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TM2): TM2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TM2): TM2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TM2; const v: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TV2; const m: TM2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TM2; s: TCompType): TM2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TM2): TM2; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    property Col[index: Integer]: TV2 read GetCol write SetCol;
  case Byte of
    0: (f: array [0..1,0..1] of TCompType);
    1: (Row: array [0..1] of TV2);
  end;
  PM2 = ^TM2;

  { TM3 }

  TM3 = record
  private
    function GetCol(index: Integer): TV3;               {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TV2;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TV2;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TV2;  {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TV3); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TV2); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TV2); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TV2); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator NotEqual (const m1, m2: TM3): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const m1, m2: TM3): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TV3; const m: TM3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TV2; const m: TM3): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TM3; s: TCompType): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    function Mat2x2: TM2;
    property Col[index: Integer]: TV3 read GetCol write SetCol;
    property OX: TV2 read GetOX write SetOX;
    property OY: TV2 read GetOY write SetOY;
    property Pos: TV2 read GetPos write SetPos;
  case Byte of
    0: (f: array [0..2,0..2] of TCompType);
    1: (Row: array [0..2] of TV3);
  end;
  PM3 = ^TM3;

  { TM4 }

  TM4 = record
  private
    function GetCol(index: Integer): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOX: TV3;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOY: TV3;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetOZ: TV3;  {$IFNDEF NoInline} inline; {$ENDIF}
    function GetPos: TV3;  {$IFNDEF NoInline} inline; {$ENDIF}

    procedure SetCol(index: Integer; const Value: TV4); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOX(const AValue: TV3); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOY(const AValue: TV3); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetOZ(const AValue: TV3); {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetPos(const AValue: TV3); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    Class Operator NotEqual (const m1, m2: TM4): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Equal (const m1, m2: TM4): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Add (const m1, m2: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Subtract (const m1, m2: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m1, m2: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TV4; const m: TM4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const v: TV3; const m: TM4): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Multiply (const m: TM4; s: TCompType): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
    Class Operator Negative (const m: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    function Mat3x3: TM3;

    property Col[index: Integer]: TV4 read GetCol write SetCol;
    property OX: TV3 read GetOX write SetOX;
    property OY: TV3 read GetOY write SetOY;
    property OZ: TV3 read GetOZ write SetOZ;
    property Pos: TV3 read GetPos write SetPos;
  case Byte of
    0: (f: array [0..3,0..3] of TCompType);
    1: (Row: array [0..3] of TV4);
  end;
  PM4 = ^TM4;

  TM2Arr = array of TM2;
  TM3Arr = array of TM3;
  TM4Arr = array of TM4;

  function Abs(const V: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TCompType; minval, maxval: TCompType): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TV2; minval, maxval: TCompType): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TV3; minval, maxval: TCompType): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TV4; minval, maxval: TCompType): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Sign(const V: TV4): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Dot(const v1, v2: TV2): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TV3): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Dot(const v1, v2: TV4): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TV2): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Cross(const v1, v2: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TV2): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function LenSqr(const v: TV3): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Transpose(const m: TM2): TM2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TM3): TM3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Transpose(const m: TM4): TM4; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Det(const m: TM2): TCompType; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TM3): TCompType; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  function Det(const m: TM4): TCompType; overload;{$IFNDEF NoInline} inline; {$ENDIF}

  function Vec(const x, y: TCompType): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z: TCompType): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TV2; z: TCompType): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: TCompType; const yz: TV2): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x, y, z, w: TCompType): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xy: TV2; const zw: TV2): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const xyz: TV3; w: TCompType): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: TCompType; const yz: TV2; w: TCompType): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Vec(const x: TCompType; const yzw: TV3): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}

  function Equal (const v1, v2: TV2; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TV3; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const v1, v2: TV4; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TM2; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TM3; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;
  function Equal (const m1, m2: TM4; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF} overload;

  procedure Swap(var v1, v2: TV2); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TV3); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TV4); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TM2); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TM3); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TM4); overload; {$IFNDEF NoInline} inline; {$ENDIF}

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TV2Arr); overload;
  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TV3Arr); overload;
  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TV4Arr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TV2Arr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TV3Arr); overload;
  procedure StreamReadVecArr(const AStream: TStream; out AArr: TV4Arr); overload;
{$ENDIF}

{$IFDEF IMPL}
  { TV2 }

  function TV2.yx: TV2;
  begin
    Result.x := y;
    Result.y := x;
  end;

  { TM2 }

  function TM2.GetCol(index: Integer): TV2;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
  end;

  procedure TM2.SetCol(index: Integer; const Value: TV2);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
  end;

  { TM3 }

  function TM3.Mat2x2: TM2;
  begin
    Result.Row[0] := Row[0].xy;
    Result.Row[1] := Row[1].xy;
  end;

  function TM3.GetCol(index: Integer): TV3;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
  end;

  function TM3.GetOX: TV2;
  begin
    Result := Row[0].xy;
  end;

  function TM3.GetOY: TV2;
  begin
    Result := Row[1].xy;
  end;

  function TM3.GetPos: TV2;
  begin
    Result := Row[2].xy;
  end;

  procedure TM3.SetCol(index: Integer; const Value: TV3);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
  end;

  procedure TM3.SetOX(const AValue: TV2);
  begin
    Row[0].xy := AValue;
  end;

  procedure TM3.SetOY(const AValue: TV2);
  begin
    Row[1].xy := AValue;
  end;

  procedure TM3.SetPos(const AValue: TV2);
  begin
    Row[2].xy := AValue;
  end;

  { TM4 }

  function TM4.GetCol(index: Integer): TV4;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
    Result.w := f[3, index];
  end;

  function TM4.GetOX: TV3;
  begin
    Result := Row[0].xyz;
  end;

  function TM4.GetOY: TV3;
  begin
    Result := Row[1].xyz;
  end;

  function TM4.GetOZ: TV3;
  begin
    Result := Row[2].xyz;
  end;

  function TM4.GetPos: TV3;
  begin
    Result := Row[3].xyz;
  end;

  procedure TM4.SetCol(index: Integer; const Value: TV4);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
    f[3, index] := Value.w;
  end;

  procedure TM4.SetOX(const AValue: TV3);
  begin
    Row[0].xyz := AValue;
  end;

  procedure TM4.SetOY(const AValue: TV3);
  begin
    Row[1].xyz := AValue;
  end;

  procedure TM4.SetOZ(const AValue: TV3);
  begin
    Row[2].xyz := AValue;
  end;

  procedure TM4.SetPos(const AValue: TV3);
  begin
    Row[3].xyz := AValue;
  end;

  function TM4.Mat3x3: TM3;
  begin
    Result.Row[0] := Row[0].xyz;
    Result.Row[1] := Row[1].xyz;
    Result.Row[2] := Row[2].xyz;
  end;

  Class Operator TV2.NotEqual (const v1, v2: TV2): Boolean;
  begin
    Result := (v1.x <> v2.x) or (v1.y <> v2.y);
  end;

  Class Operator TV2.Equal (const v1, v2: TV2): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y);
  end;

  Class Operator TV2.Add (const v1, v2: TV2): TV2;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
  end;

  Class Operator TV2.Subtract (const v1, v2: TV2): TV2;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
  end;

  Class Operator TV2.Multiply (const v1, v2: TV2): TV2;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
  end;

  Class Operator TV2.Multiply (const v1: TV2; s: TCompType): TV2;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
  end;

  Class Operator TV2.Negative (const v: TV2): TV2;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
  end;

  {$IfDef CastToInt}
  Class Operator TV2.Implicit (const V: TV2): TVec2i;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TV2.Implicit (const V: TV2): TVec2;
  begin
    Result.x := V.x;
    Result.y := V.y;
  end;

  Class Operator TV2.Multiply (const v1: TV2; s: Single): TVec2;
  begin
    Result.x := V1.x * s;
    Result.y := V1.y * s;
  end;
  {$EndIf}

  {$IfDef Single}
  Class Operator TV2.Divide (const v1, v2: TVec2): TVec2;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
  end;

  Class Operator TV2.Divide (const v: TVec2; const f: Single): TVec2;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TV2.Divide (const v1, v2: TVec2i): TVec2;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
  end;

  Class Operator TV2.Divide (const v: TVec2i; const f: Single): TVec2;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
  end;
  {$EndIf}  

  Class Operator TV3.NotEqual (const v1, v2: TV3): Boolean;
  begin
    Result := (v1.x <> v2.x) or (v1.y <> v2.y) or (v1.z <> v2.z);
  end;

  Class Operator TV3.Equal (const v1, v2: TV3): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
  end;

  Class Operator TV3.Add (const v1, v2: TV3): TV3;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
  end;

  Class Operator TV3.Subtract (const v1, v2: TV3): TV3;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
  end;

  Class Operator TV3.Multiply (const v1, v2: TV3): TV3;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
  end;

  Class Operator TV3.Multiply (const v1: TV3; s: TCompType): TV3;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  end;

  Class Operator TV3.Negative (const v: TV3): TV3;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
  end;

  {$IfDef CastToInt}
  Class Operator TV3.Implicit (const V: TV3): TVec3i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TV3.Implicit (const V: TV3): TVec3;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
  end;

  Class Operator TV3.Multiply (const v1: TV3; s: Single): TVec3;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  End;
  {$EndIf}

  {$IfDef Single}
  Class Operator TV3.Divide (const v1, v2: TVec3): TVec3;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
  end;

  Class Operator TV3.Divide (const v: TVec3; const f: Single): TVec3;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TV3.Divide (const v1, v2: TVec3i): TVec3;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
  end;

  Class Operator TV3.Divide (const v: TVec3i; const f: Single): TVec3;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
  end;
  {$EndIf}  

  Class Operator TV4.NotEqual (const v1, v2: TV4): Boolean;
  begin
    Result := (v1.x <> v2.x) or (v1.y <> v2.y) or (v1.z <> v2.z) or (v1.w <> v2.w);
  end;

  Class Operator TV4.Equal (const v1, v2: TV4): Boolean;
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z) and (v1.w = v2.w);
  end;

  Class Operator TV4.Add (const v1, v2: TV4): TV4;
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
    Result.w := v1.w + v2.w;
  end;

  Class Operator TV4.Subtract (const v1, v2: TV4): TV4;
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
    Result.w := v1.w - v2.w;
  end;

  Class Operator TV4.Multiply (const v1, v2: TV4): TV4;
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
    Result.w := v1.w * v2.w;
  end;

  Class Operator TV4.Multiply (const v1: TV4; s: TCompType): TV4;
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  end;

  Class Operator TV4.Negative (const v: TV4): TV4;
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
    Result.w := - v.w;
  end;

  {$IfDef CastToInt}
  Class Operator TV4.Implicit (const V: TV4): TVec4i;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;
  {$EndIf}

  {$IfDef CastToSingle}
  Class Operator TV4.Implicit (const V: TV4): TVec4;
  begin
    Result.x := V.x;
    Result.y := V.y;
    Result.z := V.z;
    Result.w := V.w;
  end;

  Class Operator TV4.Multiply (const v1: TV4; s: Single): TVec4;
  Begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  End;

  {$EndIf}

  {$IfDef Single}
  Class Operator TV4.Divide (const v1, v2: TVec4): TVec4;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
    Result.w := v1.w / v2.w;
  end;

  Class Operator TV4.Divide (const v: TVec4; const f: Single): TVec4;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
    Result.w := v.w / f;
  end;
  {$EndIf}
  
  {$IfDef Integer}
  Class Operator TV4.Divide (const v1, v2: TVec4i): TVec4;
  begin
    Result.x := v1.x / v2.x;
    Result.y := v1.y / v2.y;
    Result.z := v1.z / v2.z;
    Result.w := v1.w / v2.w;
  end;

  Class Operator TV4.Divide (const v: TVec4i; const f: Single): TVec4;
  begin
    Result.x := v.x / f;
    Result.y := v.y / f;
    Result.z := v.z / f;
    Result.w := v.w / f;
  end;
  {$EndIf}

  Class operator TM2.NotEqual (const m1, m2: TM2): Boolean;
  begin
    Result := (m1.Row[0] <> m2.Row[0]) or (m1.Row[1] <> m2.Row[1]);
  end;

  Class operator TM2.Equal (const m1, m2: TM2): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]);
  end;

  Class operator TM2.Add (const m1, m2: TM2): TM2;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
  end;

  Class operator TM2.Subtract (const m1, m2: TM2): TM2;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
  end;

  Class operator TM2.Multiply (const m1, m2: TM2): TM2;
  var i, j: Integer;
  begin
    for j := 0 to 1 do
      for i := 0 to 1 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class operator TM2.Multiply (const m: TM2; const v: TV2): TV2;
  begin
    Result.x := Dot(m.Row[0], v);
    Result.y := Dot(m.Row[1], v);
  end;
  
  Class operator TM2.Multiply (const v: TV2; const m: TM2): TV2;
  begin
    Result := m.Row[0]*v.x + m.Row[1]*v.y;
  end;  

  Class operator TM2.Multiply (const m: TM2; s: TCompType): TM2;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
  end;

  Class operator TM2.Negative (const m: TM2): TM2;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
  end;

  Class Operator TM3.NotEqual (const m1, m2: TM3): Boolean;
  begin
    Result := (m1.Row[0] <> m2.Row[0]) or (m1.Row[1] <> m2.Row[1]) or (m1.Row[2] <> m2.Row[2]);
  end;

  Class Operator TM3.Equal (const m1, m2: TM3): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]);
  end;

  Class Operator TM3.Add (const m1, m2: TM3): TM3;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
  end;

  Class Operator TM3.Subtract (const m1, m2: TM3): TM3;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
  end;

  Class Operator TM3.Multiply (const m1, m2: TM3): TM3;
  var i, j: Integer;
  begin
    for i := 0 to 2 do
      for j := 0 to 2 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TM3.Multiply (const v: TV3; const m: TM3): TV3;
  begin
    Result := m.Row[0]*v.x+
              m.Row[1]*v.y+
              m.Row[2]*v.z;
  end;

  Class Operator TM3.Multiply (const v: TV2; const m: TM3): TV2;
  begin
    Result := m.Row[0].xy*v.x+
              m.Row[1].xy*v.y+
              m.Row[2].xy;
  end;

  Class Operator TM3.Multiply (const m: TM3; s: TCompType): TM3;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
  end;

  Class Operator TM3.Negative (const m: TM3): TM3;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
  end;

  Class Operator TM4.NotEqual (const m1, m2: TM4): Boolean;
  begin
    Result := (m1.Row[0] <> m2.Row[0]) or (m1.Row[1] <> m2.Row[1]) or (m1.Row[2] <> m2.Row[2]) or (m1.Row[3] <> m2.Row[3]);
  end;

  Class Operator TM4.Equal (const m1, m2: TM4): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]) and (m1.Row[3] = m2.Row[3]);
  end;

  Class Operator TM4.Add (const m1, m2: TM4): TM4;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
    Result.Row[3] := m1.Row[3] + m2.Row[3];
  end;

  Class Operator TM4.Subtract (const m1, m2: TM4): TM4;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
    Result.Row[3] := m1.Row[3] - m2.Row[3];
  end;

  Class Operator TM4.Multiply (const m1, m2: TM4): TM4;
  var i, j: Integer;
  begin
    for i := 0 to 3 do
      for j := 0 to 3 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Class Operator TM4.Multiply (const v: TV4; const m: TM4): TV4;
  begin
    Result := m.Row[0]*v.x +
              m.Row[1]*v.y +
              m.Row[2]*v.z +
              m.Row[3]*v.w;
  end;

  Class Operator TM4.Multiply (const v: TV3; const m: TM4): TV3;
  begin
    Result := m.Row[0].xyz*v.x +
              m.Row[1].xyz*v.y +
              m.Row[2].xyz*v.z +
              m.Row[3].xyz;
  end;

  Class Operator TM4.Multiply (const m: TM4; s: TCompType): TM4;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
    Result.Row[3] := m.Row[3] * s;
  end;

  Class Operator TM4.Negative (const m: TM4): TM4;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
    Result.Row[3] := - m.Row[3];
  end;

  function Abs(const V: TV2): TV2;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
  end;

  function Abs(const V: TV3): TV3;
  begin
    Result.x := System.abs(V.x);
    Result.y := System.abs(V.y);
    Result.z := System.abs(V.z);
  end;

  function Min(const A, B: TV2): TV2; overload;
  begin
    Result.x := Math.Min(A.x, B.x);
    Result.y := Math.Min(A.y, B.y);
  end;
  function Min(const A, B: TV3): TV3; overload;
  begin
    Result.x := Min(A.x, B.x);
    Result.y := Min(A.y, B.y);
    Result.z := Min(A.z, B.z);
  end;
  function Max(const A, B: TV2): TV2; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
  end;
  function Max(const A, B: TV3): TV3; overload;
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
    Result.z := Max(A.z, B.z);
  end;

  function Clamp(const v: TCompType; minval, maxval: TCompType): TCompType; overload;
  begin
      Result := min(maxval, max(minval, v));
  end;
  function Clamp(const v: TV2; minval, maxval: TCompType): TV2; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
  end;
  function Clamp(const v: TV3; minval, maxval: TCompType): TV3; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
      Result.z := min(maxval, max(minval, v.z));
  end;
  function Clamp(const v: TV4; minval, maxval: TCompType): TV4; overload;
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
      Result.z := min(maxval, max(minval, v.z));
	  Result.w := min(maxval, max(minval, v.w));
  end;  

  function Sign(const V: TV2): TV2; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
  end;

  function Sign(const V: TV3): TV3; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
  end;

  function Sign(const V: TV4): TV4; overload;
  begin
    Result.x := sign(V.x);
    Result.y := sign(V.y);
    Result.z := sign(V.z);
    Result.w := sign(V.w);
  end;

  function Dot(const v1, v2: TV2): TCompType; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y;
  end;

  function Dot(const v1, v2: TV3): TCompType; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
  end;

  function Dot(const v1, v2: TV4): TCompType; overload;
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
  end;

  function Cross(const v1, v2: TV2): TCompType; overload;
  begin
    Result := v1.x * v2.y - v1.y * v2.x;
  end;

  function Cross(const v1, v2: TV3): TV3; overload;
  begin
    result.x := (v1.y*v2.z) - (v1.z*v2.y);
    result.y := (v1.z*v2.x) - (v1.x*v2.z);
    result.z := (v1.x*v2.y) - (v1.y*v2.x);
  end;

  function LenSqr(const v: TV2): TCompType; overload;
  begin
    Result := dot(v, v);
  end;

  function LenSqr(const v: TV3): TCompType; overload;
  begin
    Result := dot(v, v);
  end;

  function Vec(const x, y: TCompType): TV2; overload;
  begin
    Result.x := x;
    Result.y := y;
  end;

  function Vec(const x, y, z: TCompType): TV3; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
  end;

  function Vec(const xy: TV2; z: TCompType): TV3; overload;
  begin
    Result.xy := xy;
    Result.z := z;
  end;

  function Vec(const x: TCompType; const yz: TV2): TV3; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
  end;

  function Vec(const x, y, z, w: TCompType): TV4; overload;
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
    Result.w := w;
  end;

  function Vec(const xy: TV2; const zw: TV2): TV4; overload;
  begin
    Result.xy := xy;
    Result.zw := zw;
  end;

  function Vec(const xyz: TV3; w: TCompType): TV4; overload;
  begin
    Result.xyz := xyz;
    Result.w := w;
  end;

  function Vec(const x: TCompType; const yz: TV2; w: TCompType): TV4; overload;
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
    Result.w := w;
  end;

  function Vec(const x: TCompType; const yzw: TV3): TV4; overload;
  begin
    Result.x := x;
    Result.y := yzw.x;
    Result.z := yzw.y;
    Result.w := yzw.z;
  end;

  function Equal (const v1, v2: TV2; const AEPS: Single = EPS): Boolean;
  var v: TV2;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS);
  end;

  function Equal (const v1, v2: TV3; const AEPS: Single = EPS): Boolean;
  var v: TV3;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS);
  end;

  function Equal (const v1, v2: TV4; const AEPS: Single = EPS): Boolean;
  var v: TV4;
  begin
    v := v2 - v1;
    Result := (System.abs(v.x)<AEPS) and (System.abs(v.y)<AEPS) and (System.abs(v.z)<AEPS) and (System.abs(v.w)<AEPS);
  end;

  function Equal(const m1, m2: TM2; const AEPS: Single): Boolean;
  begin
     Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
               Equal(m1.Row[1], m2.Row[1], AEPS);
  end;

  function Equal(const m1, m2: TM3; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS);
  end;

  function Equal(const m1, m2: TM4; const AEPS: Single): Boolean;
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS) and
              Equal(m1.Row[3], m2.Row[3], AEPS);
  end;

  procedure Swap(var v1, v2: TV2);
  var tmp: TV2;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TV3);
  var tmp: TV3;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TV4);
  var tmp: TV4;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TM2);
  var tmp: TM2;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TM3);
  var tmp: TM3;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  procedure Swap(var v1, v2: TM4);
  var tmp: TM4;
  begin
    tmp := v2;
    v2 := v1;
    v1 := tmp;
  end;

  function Transpose(const m: TM2): TM2; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
  end;

  function Transpose(const m: TM3): TM3; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
  end;

  function Transpose(const m: TM4): TM4; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Col[0];
    Result.Row[1] := m.Col[1];
    Result.Row[2] := m.Col[2];
    Result.Row[3] := m.Col[3];
  end;

  function Det(const m: TM2): TCompType; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0,0]*m.f[1,1] - m.f[0,1]*m.f[1,0];
  end;

  function Det(const m: TM3): TCompType; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := Dot(m.Row[0], Cross(m.Row[1], m.Row[2]));
  end;

  function Det(const m: TM4): TCompType; overload;{$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := m.f[0, 0] * (m.f[1, 1] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) + m.f[1, 3] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1])) -
              m.f[0, 1] * (m.f[1, 0] * (m.f[2, 2] * m.f[3, 3] - m.f[2, 3] * m.f[3, 2]) - m.f[1, 2] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0])) +
              m.f[0, 2] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 3] - m.f[2, 3] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 3] - m.f[2, 3] * m.f[3, 0]) + m.f[1, 3] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0])) -
              m.f[0, 3] * (m.f[1, 0] * (m.f[2, 1] * m.f[3, 2] - m.f[2, 2] * m.f[3, 1]) - m.f[1, 1] * (m.f[2, 0] * m.f[3, 2] - m.f[2, 2] * m.f[3, 0]) + m.f[1, 2] * (m.f[2, 0] * m.f[3, 1] - m.f[2, 1] * m.f[3, 0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TV2Arr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TV3Arr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamWriteVecArr(const AStream: TStream; const AArr: TV4Arr);
  var n: Integer;
  begin
    n := Length(AArr);
    AStream.WriteBuffer(n, SizeOf(n));
    if n > 0 then
      AStream.WriteBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TV2Arr);
  var n: Integer;
  begin
    n := 0;
    AStream.ReadBuffer(n, SizeOf(n));
    AArr := nil;
    SetLength(AArr, n);
    if n > 0 then
      AStream.ReadBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TV3Arr);
  var n: Integer;
  begin
    n := 0;
    AStream.ReadBuffer(n, SizeOf(n));
    AArr := nil;
    SetLength(AArr, n);
    if n > 0 then
      AStream.ReadBuffer(AArr[0], n*SizeOf(AArr[0]));
  end;

  procedure StreamReadVecArr(const AStream: TStream; out AArr: TV4Arr);
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
{$UnDef TCompType}
{$UnDef CastToInt} {$UnDef CastToSingle}
