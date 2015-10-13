{$IFDEF INTF}
type
  { TV2 }

  TV2 = record
    x, y: TCompType;
  public
    function yx: TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  end;
  PV2 =^TV2;

  { TV3 }

  TV3 = record
  case Byte of
    0: (f: array [0..2] of Single);
    1: (x, y, z: TCompType);
    2: (xy: TV2);
  end;
  PV3 =^TV3;

  { TV4 }

  TV4 = record
  case Byte of
    0: (f: array [0..3] of Single);
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
    procedure SetCol(index: Integer; const Value: TV3); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    property Col[index: Integer]: TV3 read GetCol write SetCol;
  case Byte of
    0: (f: array [0..2,0..2] of TCompType);
    1: (Row: array [0..2] of TV3);
  end;
  PM3 = ^TM3;

  { TM4 }

  TM4 = record
  private
    function GetCol(index: Integer): TV4;               {$IFNDEF NoInline} inline; {$ENDIF}
    procedure SetCol(index: Integer; const Value: TV4); {$IFNDEF NoInline} inline; {$ENDIF}
  public
    property Col[index: Integer]: TV4 read GetCol write SetCol;
  case Byte of
    0: (f: array [0..3,0..3] of TCompType);
    1: (Row: array [0..3] of TV4);
  end;
  PM4 = ^TM4;

  TM2Arr = array of TM2;
  TM3Arr = array of TM3;
  TM4Arr = array of TM4;

  Operator = (const v1, v2: TV2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator + (const v1, v2: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const v1, v2: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const v1, v2: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const v1: TV2; s: TCompType): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const v: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}

  Operator = (const v1, v2: TV3): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator + (const v1, v2: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const v1, v2: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const v1, v2: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const v1: TV3; s: TCompType): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const v: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}

  Operator = (const v1, v2: TV4): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator + (const v1, v2: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const v1, v2: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const v1, v2: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const v1: TV4; s: TCompType): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const v: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}

  Operator = (const m1, m2: TM2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator + (const m1, m2: TM2): TM2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const m1, m2: TM2): TM2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m1, m2: TM2): TM2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m: TM2; v: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m: TM2; s: TCompType): TM2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const m: TM2): TM2; {$IFNDEF NoInline} inline; {$ENDIF}

  Operator = (const m1, m2: TM3): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator + (const m1, m2: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const m1, m2: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m1, m2: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m: TM3; v: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m: TM3; v: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m: TM3; s: TCompType): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const m: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}

  Operator = (const m1, m2: TM4): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator + (const m1, m2: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const m1, m2: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m1, m2: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m: TM4; v: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m: TM4; v: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator * (const m: TM4; s: TCompType): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  Operator - (const m: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}

  function Abs(const V: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Abs(const V: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Min(const A, B: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Max(const A, B: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TCompType; minval, maxval: TCompType): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TV2; minval, maxval: TCompType): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  function Clamp(const v: TV3; minval, maxval: TCompType): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}

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

  function Equal (const v1, v2: TV2; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  function Equal (const v1, v2: TV3; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  function Equal (const v1, v2: TV4; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  function Equal (const m1, m2: TM2; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  function Equal (const m1, m2: TM3; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  function Equal (const m1, m2: TM4; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}

  procedure Swap(var v1, v2: TV2); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TV3); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TV4); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TM2); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TM3); overload; {$IFNDEF NoInline} inline; {$ENDIF}
  procedure Swap(var v1, v2: TM4); overload; {$IFNDEF NoInline} inline; {$ENDIF}
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

  function TM3.GetCol(index: Integer): TV3;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
  end;

  procedure TM3.SetCol(index: Integer; const Value: TV3);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
  end;

  { TM4 }

  function TM4.GetCol(index: Integer): TV4;
  begin
    Result.x := f[0, index];
    Result.y := f[1, index];
    Result.z := f[2, index];
    Result.w := f[3, index];
  end;

  procedure TM4.SetCol(index: Integer; const Value: TV4);
  begin
    f[0, index] := Value.x;
    f[1, index] := Value.y;
    f[2, index] := Value.z;
    f[3, index] := Value.w;
  end;

  Operator = (const v1, v2: TV2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y);
  end;

  Operator + (const v1, v2: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
  end;

  Operator - (const v1, v2: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
  end;

  Operator * (const v1, v2: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
  end;

  Operator * (const v1: TV2; s: TCompType): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
  end;

  Operator - (const v: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := - v.x;
    Result.y := - v.y;
  end;

  Operator = (const v1, v2: TV3): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z);
  end;

  Operator + (const v1, v2: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
  end;

  Operator - (const v1, v2: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
  end;

  Operator * (const v1, v2: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
  end;

  Operator * (const v1: TV3; s: TCompType): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
  end;

  Operator - (const v: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
  end;

  Operator = (const v1, v2: TV4): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := (v1.x = v2.x) and (v1.y = v2.y) and (v1.z = v2.z) and (v1.w = v2.w);
  end;

  Operator + (const v1, v2: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x + v2.x;
    Result.y := v1.y + v2.y;
    Result.z := v1.z + v2.z;
    Result.w := v1.w + v2.w;
  end;

  Operator - (const v1, v2: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x - v2.x;
    Result.y := v1.y - v2.y;
    Result.z := v1.z - v2.z;
    Result.w := v1.w - v2.w;
  end;

  Operator * (const v1, v2: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x * v2.x;
    Result.y := v1.y * v2.y;
    Result.z := v1.z * v2.z;
    Result.w := v1.w * v2.w;
  end;

  Operator * (const v1: TV4; s: TCompType): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := v1.x * s;
    Result.y := v1.y * s;
    Result.z := v1.z * s;
    Result.w := v1.w * s;
  end;

  Operator - (const v: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := - v.x;
    Result.y := - v.y;
    Result.z := - v.z;
    Result.w := - v.w;
  end;

  operator = (const m1, m2: TM2): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]);
  end;

  operator+(const m1, m2: TM2): TM2;
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
  end;

  operator-(const m1, m2: TM2): TM2;
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
  end;

  operator*(const m1, m2: TM2): TM2;
  var i, j: Integer;
  begin
    for j := 0 to 1 do
      for i := 0 to 1 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  operator*(const m: TM2; v: TV2): TV2;
  begin
    Result.x := Dot(m.Row[0], v);
    Result.y := Dot(m.Row[1], v);
  end;

  operator*(const m: TM2; s: TCompType): TM2;
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
  end;

  operator-(const m: TM2): TM2;
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
  end;

  Operator = (const m1, m2: TM3): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]);
  end;

  Operator + (const m1, m2: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
  end;

  Operator - (const m1, m2: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
  end;

  Operator * (const m1, m2: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  var i, j: Integer;
  begin
    for j := 0 to 2 do
      for i := 0 to 2 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Operator * (const m: TM3; v: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := Dot(m.Row[0], v);
    Result.y := Dot(m.Row[1], v);
    Result.z := Dot(m.Row[2], v);
  end;

  Operator * (const m: TM3; v: TV2): TV2; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := Dot(m.Row[0].xy, v);
    Result.y := Dot(m.Row[1].xy, v);
    Result := Result + m.Col[2].xy;
  end;

  Operator * (const m: TM3; s: TCompType): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
  end;

  Operator - (const m: TM3): TM3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
  end;

  Operator = (const m1, m2: TM4): Boolean;
  begin
    Result := (m1.Row[0] = m2.Row[0]) and (m1.Row[1] = m2.Row[1]) and (m1.Row[2] = m2.Row[2]) and (m1.Row[3] = m2.Row[3]);
  end;

  Operator + (const m1, m2: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m1.Row[0] + m2.Row[0];
    Result.Row[1] := m1.Row[1] + m2.Row[1];
    Result.Row[2] := m1.Row[2] + m2.Row[2];
    Result.Row[3] := m1.Row[3] + m2.Row[3];
  end;

  Operator - (const m1, m2: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m1.Row[0] - m2.Row[0];
    Result.Row[1] := m1.Row[1] - m2.Row[1];
    Result.Row[2] := m1.Row[2] - m2.Row[2];
    Result.Row[3] := m1.Row[3] - m2.Row[3];
  end;

  Operator * (const m1, m2: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  var i, j: Integer;
  begin
    for j := 0 to 3 do
      for i := 0 to 3 do
        Result.f[i, j] := Dot(m1.Row[i], m2.Col[j]);
  end;

  Operator * (const m: TM4; v: TV4): TV4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := Dot(m.Row[0], v);
    Result.y := Dot(m.Row[1], v);
    Result.z := Dot(m.Row[2], v);
    Result.w := Dot(m.Row[3], v);
  end;

  Operator * (const m: TM4; v: TV3): TV3; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := Dot(m.Row[0].xyz, v);
    Result.y := Dot(m.Row[1].xyz, v);
    Result.z := Dot(m.Row[2].xyz, v);
    Result := Result + m.Col[3].xyz;
  end;

  Operator * (const m: TM4; s: TCompType): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := m.Row[0] * s;
    Result.Row[1] := m.Row[1] * s;
    Result.Row[2] := m.Row[2] * s;
    Result.Row[3] := m.Row[3] * s;
  end;

  Operator - (const m: TM4): TM4; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.Row[0] := - m.Row[0];
    Result.Row[1] := - m.Row[1];
    Result.Row[2] := - m.Row[2];
    Result.Row[3] := - m.Row[3];
  end;

  function Abs(const V: TV2): TV2;
  begin
    Result.x := abs(V.x);
    Result.y := abs(V.y);
  end;

  function Abs(const V: TV3): TV3;
  begin
    Result.x := abs(V.x);
    Result.y := abs(V.y);
    Result.z := abs(V.z);
  end;

  function Min(const A, B: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := Math.Min(A.x, B.x);
    Result.y := Math.Min(A.y, B.y);
  end;
  function Min(const A, B: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := Min(A.x, B.x);
    Result.y := Min(A.y, B.y);
    Result.z := Min(A.z, B.z);
  end;
  function Max(const A, B: TV2): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
  end;
  function Max(const A, B: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := Max(A.x, B.x);
    Result.y := Max(A.y, B.y);
    Result.z := Max(A.z, B.z);
  end;

  function Clamp(const v: TCompType; minval, maxval: TCompType): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
      Result := min(maxval, max(minval, v));
  end;
  function Clamp(const v: TV2; minval, maxval: TCompType): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
  end;
  function Clamp(const v: TV3; minval, maxval: TCompType): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
      Result.x := min(maxval, max(minval, v.x));
      Result.y := min(maxval, max(minval, v.y));
      Result.z := min(maxval, max(minval, v.z));
  end;

  function Dot(const v1, v2: TV2): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := v1.x * v2.x + v1.y * v2.y;
  end;

  function Dot(const v1, v2: TV3): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z;
  end;

  function Dot(const v1, v2: TV4): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := v1.x * v2.x + v1.y * v2.y + v1.z * v2.z + v1.w * v2.w;
  end;

  function Cross(const v1, v2: TV2): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := v1.x * v2.y - v1.y * v2.x;
  end;

  function Cross(const v1, v2: TV3): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    result.x := (v1.y*v2.z) - (v1.z*v2.y);
    result.y := (v1.z*v2.x) - (v1.x*v2.z);
    result.z := (v1.x*v2.y) - (v1.y*v2.x);
  end;

  function LenSqr(const v: TV2): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := dot(v, v);
  end;

  function LenSqr(const v: TV3): TCompType; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := dot(v, v);
  end;

  function Vec(const x, y: TCompType): TV2; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := x;
    Result.y := y;
  end;

  function Vec(const x, y, z: TCompType): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
  end;

  function Vec(const xy: TV2; z: TCompType): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.xy := xy;
    Result.z := z;
  end;

  function Vec(const x: TCompType; const yz: TV2): TV3; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
  end;

  function Vec(const x, y, z, w: TCompType): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := x;
    Result.y := y;
    Result.z := z;
    Result.w := w;
  end;

  function Vec(const xy: TV2; const zw: TV2): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.xy := xy;
    Result.zw := zw;
  end;

  function Vec(const xyz: TV3; w: TCompType): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.xyz := xyz;
    Result.w := w;
  end;

  function Vec(const x: TCompType; const yz: TV2; w: TCompType): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := x;
    Result.y := yz.x;
    Result.z := yz.y;
    Result.w := w;
  end;

  function Vec(const x: TCompType; const yzw: TV3): TV4; overload; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result.x := x;
    Result.y := yzw.x;
    Result.z := yzw.y;
    Result.w := yzw.z;
  end;

  function Equal (const v1, v2: TV2; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  var v: TV2;
  begin
    v := v2 - v1;
    Result := (abs(v.x)<AEPS) and (abs(v.y)<AEPS);
  end;

  function Equal (const v1, v2: TV3; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  var v: TV3;
  begin
    v := v2 - v1;
    Result := (abs(v.x)<AEPS) and (abs(v.y)<AEPS) and (abs(v.z)<AEPS);
  end;

  function Equal (const v1, v2: TV4; const AEPS: Single = EPS): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  var v: TV4;
  begin
    v := v2 - v1;
    Result := (abs(v.x)<AEPS) and (abs(v.y)<AEPS) and (abs(v.z)<AEPS) and (abs(v.w)<AEPS);
  end;

  function Equal(const m1, m2: TM2; const AEPS: Single): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
     Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
               Equal(m1.Row[1], m2.Row[1], AEPS);
  end;

  function Equal(const m1, m2: TM3; const AEPS: Single): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
  begin
    Result := Equal(m1.Row[0], m2.Row[0], AEPS) and
              Equal(m1.Row[1], m2.Row[1], AEPS) and
              Equal(m1.Row[2], m2.Row[2], AEPS);
  end;

  function Equal(const m1, m2: TM4; const AEPS: Single): Boolean; {$IFNDEF NoInline} inline; {$ENDIF}
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
{$ENDIF}
