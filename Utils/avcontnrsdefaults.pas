unit avContnrsDefaults;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  EMPTY_HASH = 0;

type
  THashFunction = function (const SrcData; len: Cardinal): Cardinal;

  TInterfacedObjectEx = TInterfacedObject;

  { IComparer }

  generic IComparer<T> = interface
    function Compare(const Left, Right: T): Integer;
  end;

  { IEqualityComparer }

  generic IEqualityComparer<T> = interface
    function Hash(const Value: T): Integer;
    function IsEqual(const Left, Right: T): Boolean;
  end;

  { TEqualityComparer_UString }

  generic TEqualityComparer_UString<T> = class (TInterfacedObjectEx, specialize IEqualityComparer<T>)
  public
    function Hash(const Value: T): Integer;
    function IsEqual(const Left, Right: T): Boolean;
  end;

  { TEqualityComparer_AString }

  generic TEqualityComparer_AString<T> = class (TInterfacedObjectEx, specialize IEqualityComparer<T>)
  public
    function Hash(const Value: T): Integer;
    function IsEqual(const Left, Right: T): Boolean;
  end;

  { TEqualityComparer_WString }

  generic TEqualityComparer_WString<T> = class (TInterfacedObjectEx, specialize IEqualityComparer<T>)
  public
    function Hash(const Value: T): Integer;
    function IsEqual(const Left, Right: T): Boolean;
  end;

  { TEqualityComparer_Data }

  generic TEqualityComparer_Data<T> = class (TInterfacedObjectEx, specialize IEqualityComparer<T>)
  public
    function Hash(const Value: T): Integer;
    function IsEqual(const Left, Right: T): Boolean;
  end;

  { TEqualityComparer_Array }

  generic TEqualityComparer_Array<T> = class (TInterfacedObjectEx, specialize IEqualityComparer<T>)
  private
    class var ElementSize: Integer;
  public
    function Hash(const Value: T): Integer;
    function IsEqual(const Left, Right: T): Boolean;

    class constructor Create;
  end;

  { TComparer_UString }

  generic TComparer_UString<T> = class (TInterfacedObjectEx, specialize IComparer<T>)
  public
    function Compare(const Left, Right: T): Integer;
  end;

  { TComparer_AString }

  generic TComparer_AString<T> = class (TInterfacedObjectEx, specialize IComparer<T>)
  public
    function Compare(const Left, Right: T): Integer;
  end;

  { TComparer_WString }

  generic TComparer_WString<T> = class (TInterfacedObjectEx, specialize IComparer<T>)
  public
    function Compare(const Left, Right: T): Integer;
  end;

  { TComparer_Data }

  generic TComparer_Data<T> = class (TInterfacedObjectEx, specialize IComparer<T>)
  public
    function Compare(const Left, Right: T): Integer;
  end;

  { TComparer_Array }

  generic TComparer_Array<T> = class (TInterfacedObjectEx, specialize IComparer<T>)
  private
    class var ElementSize: Integer;
  public
    function Compare(const Left, Right: T): Integer;

    class constructor Create;
  end;

var gvDefaultHash: THashFunction;

function Murmur2DefSeed(const SrcData; len: LongWord): Cardinal;
function Murmur2(const SrcData; len: LongWord; const Seed: LongWord = $9747b28c): Cardinal;

implementation

uses TypInfo, Math;

function Murmur2DefSeed(const SrcData; len: LongWord): Cardinal;
begin
  Result := Murmur2(SrcData, len);
  if Result = EMPTY_HASH then Inc(Result);
end;

function Murmur2(const SrcData; len: LongWord; const Seed: LongWord = $9747B28C): Cardinal;
var
  S: array [0 .. 0] of Byte absolute SrcData;
  h: LongWord;
  k: LongWord;
  data: Integer;
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5BD1E995;
  r = 24;
begin
  // The default seed, $9747b28c, is from the original C library

  // Initialize the hash to a 'random' value
  h := Seed xor len;

  // Mix 4 bytes at a time into the hash
  data := 0;

  while (len >= 4) do
  begin
    k := PLongWord(@S[data])^;

    k := k * m;
    k := k xor (k shr r);
    k := k * m;

    h := h * m;
    h := h xor k;

    data := data + 4;
    len := len - 4;
  end;

  { Handle the last few bytes of the input array
    S: ... $69 $18 $2f
    }
  Assert(len <= 3);
  if len = 3 then
    h := h xor (LongWord(S[data + 2]) shl 16);
  if len >= 2 then
    h := h xor (LongWord(S[data + 1]) shl 8);
  if len >= 1 then
  begin
    h := h xor (LongWord(S[data]));

    h := h * m;
  end;

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.
  h := h xor (h shr 13);

  h := h * m;
  h := h xor (h shr 15);

  Result := h;
end;

{ TComparer_Array }

function TComparer_Array.Compare(const Left, Right: T): Integer;
var arrLeft  : TBytes absolute Left;
    arrRight : TBytes absolute Right;
    diff : Integer;
    i : Integer;
begin
  if Length(arrLeft) = Length(arrRight) then
    Result := 0
  else
  if Length(arrLeft) < Length(arrRight) then
    Result := -1
  else
    Result := 1;

  for i := 0 to min(Length(arrLeft), Length(arrRight))*ElementSize - 1 do
  begin
    diff := arrLeft[i] - arrRight[i];
    if diff <> 0 then Exit(diff);
  end;
end;

class constructor TComparer_Array.Create;
var p: PTypeInfo;
begin
  p := TypeInfo(T);
  Assert(p^.Kind = tkDynArray);
  ElementSize := GetTypeData(p)^.elSize;
end;

{ TComparer_Data }

function TComparer_Data.Compare(const Left, Right: T): Integer;
var arrLeft  : array [0..0] of Byte absolute Left;
    arrRight : array [0..0] of Byte absolute Right;
    diff : Integer;
    i : Integer;
begin
  Result := 0;
  for i := 0 to SizeOf(T) - 1 do
  begin
    diff := arrLeft[i] - arrRight[i];
    if diff <> 0 then Exit(diff);
  end;
end;

{ TComparer_WString }

function TComparer_WString.Compare(const Left, Right: T): Integer;
var
  strLeft: WideString absolute Left;
  strRight: WideString absolute Right;
begin
  Result := CompareStr(strLeft, strRight);
end;

{ TComparer_AString }

function TComparer_AString.Compare(const Left, Right: T): Integer;
var
  strLeft: AnsiString absolute Left;
  strRight: AnsiString absolute Right;
begin
  Result := CompareStr(strLeft, strRight);
end;

{ TComparer_UString }

function TComparer_UString.Compare(const Left, Right: T): Integer;
var
  strLeft: UnicodeString absolute Left;
  strRight: UnicodeString absolute Right;
begin
  Result := CompareStr(strLeft, strRight);
end;

{ TEqualityComparer_WString }

function TEqualityComparer_WString.Hash(const Value: T): Integer;
var str: WideString absolute Value;
begin
  if Length(str) = 0 then
    Result := 42
  else
    Result := gvDefaultHash(str[1], Length(str));
end;

function TEqualityComparer_WString.IsEqual(const Left, Right: T): Boolean;
var
  strLeft: WideString absolute Left;
  strRight: WideString absolute Right;
begin
  Result := strLeft = strRight;
end;

{ TEqualityComparer_Array }

function TEqualityComparer_Array.Hash(const Value: T): Integer;
var arr : TBytes absolute Value;
begin
  if Length(arr) = 0 then
    Result := 42
  else
    Result := gvDefaultHash(arr[0], Length(arr)*ElementSize);
end;

function TEqualityComparer_Array.IsEqual(const Left, Right: T): Boolean;
var arrLeft  : TBytes absolute Left;
    arrRight : TBytes absolute Right;
begin
  if Length(arrLeft) <> Length(arrRight) then Exit(False);
  if Length(arrLeft) = 0 then Exit(True);
  Result := CompareMem(Pointer(arrLeft), Pointer(arrRight), Length(arrLeft)*ElementSize);
end;

class constructor TEqualityComparer_Array.Create;
var p: PTypeInfo;
begin
  p := TypeInfo(T);
  Assert(p^.Kind = tkDynArray);
  ElementSize := GetTypeData(p)^.elSize;
end;

{ TEqualityComparer_UString }

function TEqualityComparer_UString.Hash(const Value: T): Integer;
var str: UnicodeString absolute Value;
begin
  if Length(str) = 0 then
    Result := 42
  else
    Result := gvDefaultHash(str[1], Length(str));
end;

function TEqualityComparer_UString.IsEqual(const Left, Right: T): Boolean;
var
  strLeft: UnicodeString absolute Left;
  strRight: UnicodeString absolute Right;
begin
  Result := strLeft = strRight;
end;

{ TEqualityComparer_AString }

function TEqualityComparer_AString.Hash(const Value: T): Integer;
var str: AnsiString absolute Value;
begin
  if Length(str) = 0 then
    Result := 42
  else
    Result := gvDefaultHash(str[1], Length(str));
end;

function TEqualityComparer_AString.IsEqual(const Left, Right: T): Boolean;
var
  strLeft: AnsiString absolute Left;
  strRight: AnsiString absolute Right;
begin
  Result := strLeft = strRight;
end;

{ TDataEqualityComparer }

function TEqualityComparer_Data.Hash(const Value: T): Integer;
begin
  Result := gvDefaultHash(Value, SizeOf(T));
end;

function TEqualityComparer_Data.IsEqual(const Left, Right: T): Boolean;
begin
  Result := CompareMem(@Left, @Right, SizeOf(T));
end;

initialization
  gvDefaultHash := @Murmur2DefSeed;

end.

