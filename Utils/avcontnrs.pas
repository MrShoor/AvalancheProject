unit avContnrs;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$macro ON}

interface {$DEFINE INTERFACE}

uses
  Classes, SysUtils, mutils, FGL, typinfo, Windows;

type
  TInterfacedObjectEx = TInterfacedObject;
  TCompareMethod = function (item1, item2: Pointer; dataSize: Integer): Boolean of object;
  TCompareMethodStatic = function (item1, item2: Pointer; dataSize: Integer; userData: Pointer): Integer;

  EContnrsError = class(Exception);

  { IArray }

  generic IArray<TValue> = interface
    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);
    function GetItem(const index: Integer): TValue;
    procedure SetItem(const index: Integer; const AValue: TValue);
    function GetPItem(const index: Integer): Pointer;

    function Count: Integer;

    function  Add(const item: TValue): Integer; //index of new added element
    procedure Delete(const index: Integer);
    function  IndexOf(const item: TValue): Integer;

    procedure Clear(const TrimCapacity: Boolean = False);

    procedure Sort(comparator: TCompareMethodStatic; userData: Pointer);

    property Item[index: Integer]: TValue read GetItem write SetItem; default;
    property PItem[index: Integer]: Pointer read GetPItem;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { TArray }

  generic TArray<TValue> = class (TInterfacedObjectEx, specialize IArray<TValue>)
  private
    FData : array of TValue;
    FCount: Integer;
    FCleanWithEmpty: Boolean;
    FEmpty: TValue;

    FItemComparer: TCompareMethod;
    function CmpUString(item1, item2: Pointer; dataSize: Integer): Boolean;
    function CmpAString(item1, item2: Pointer; dataSize: Integer): Boolean;
    function CmpRecord(item1, item2: Pointer; dataSize: Integer): Boolean;
    function CmpArray(item1, item2: Pointer; dataSize: Integer): Boolean;
    function GetCompareMethod(tinfo: PTypeInfo): TCompareMethod;
  private
    procedure QuickSort(L, R : Longint; Compare: TCompareMethodStatic; userData: Pointer);
  protected
    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);
    function GetItem(const index: Integer): TValue;
    procedure SetItem(const index: Integer; const AValue: TValue);
    function GetPItem(const index: Integer): Pointer;

    function Count: Integer;

    function Add(const item: TValue): Integer; //return index of new added element
    procedure Delete(const index: Integer);
    function IndexOf(const item: TValue): Integer;

    procedure Clear(const TrimCapacity: Boolean = False);

    procedure Sort(comparator: TCompareMethodStatic; userData: Pointer);

    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    constructor Create; overload;
    constructor Create(const AEmptyValue: TValue); overload;
  end;

  IObjArr = specialize IArray<TObject>;
  TObjArr = specialize TArray<TObject>;

  { IHashMap }

  //THash must contain Hash method like this:
  //TMyHash = record
  //  function Hash(AKey: TKey): Cardinal;
  //end;
  generic IHashMap<TKey, TValue, THash> = interface
    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);
    function GetItem(const AKey: TKey): TValue;
    procedure SetItem(const AKey: TKey; const AValue: TValue);

    function Count: Integer;
    procedure Add(const AKey: TKey; const AValue: TValue);
    procedure AddOrSet(const AKey: TKey; const AValue: TValue);
    procedure Delete(const AKey: TKey);
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    function Contains(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean; overload;
    function ContainsValue(const AValue: TValue; out AKey: TKey): Boolean; overload;
    procedure Clear;

    procedure Reset;
    function Next(out AKey: TKey; out AValue: TValue): Boolean;
    function NextKey(out AKey: TKey): Boolean;
    function NextValue(out AValue: TValue): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Item[AKey: TKey]: TValue read GetItem write SetItem;
  end;

  { THashMap }

  //THash must contain Hash method like this:
  //TMyHash = record
  //  function Hash(AKey: TKey): Cardinal;
  //end;
  generic THashMap<TKey, TValue, THash> = class (TInterfacedObjectEx, specialize IHashMap<TKey, TValue, THash>)
  private
    type
      TItem = packed record
        Hash : Cardinal;
        Key  : TKey;
        Value: TValue;
      end;
      TItems = array of TItem;
  strict private
    FEnumIndex: Integer;
    FData: TItems;
    FGrowLimit: Integer;
    FCount: Integer;

    FCleanWithEmpty: Boolean;
    FEmptyKey: TKey;
    FEmptyValue: TValue;

    FKeyComparer: TCompareMethod;
    FValueComparer: TCompareMethod;
    function CmpUString(item1, item2: Pointer; dataSize: Integer): Boolean;
    function CmpAString(item1, item2: Pointer; dataSize: Integer): Boolean;
    function CmpRecord(item1, item2: Pointer; dataSize: Integer): Boolean;
    function CmpArray(item1, item2: Pointer; dataSize: Integer): Boolean;
    function GetCompareMethod(tinfo: PTypeInfo): TCompareMethod;

    function PrevIndex(const index: Integer): Integer; inline;
    function Wrap(const index: Integer): Integer; inline;
    function CalcBucketIndex(const AKey: TKey; AHash: Cardinal; out AIndex: Integer): Boolean; inline;
    procedure DoAddOrSet(BucketIndex, AHash: Integer; const AKey: TKey; const AValue: TValue); inline;
    procedure GrowIfNeeded; inline;

    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);
    function GetItem(const AKey: TKey): TValue;
    procedure SetItem(const AKey: TKey; const AValue: TValue);

    function Count: Integer;
    procedure Add(const AKey: TKey; const AValue: TValue);
    procedure AddOrSet(const AKey: TKey; const AValue: TValue);
    procedure Delete(const AKey: TKey);
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    function Contains(const AKey: TKey): Boolean;
    function ContainsValue(const AValue: TValue): Boolean; overload;
    function ContainsValue(const AValue: TValue; out AKey: TKey): Boolean; overload;
    procedure Clear;

    function WrapedIndexIsBetween(Left, Index, Right: Integer): Boolean;

    procedure Reset;
    function Next(out AKey: TKey; out AValue: TValue): Boolean;
    function NextKey(out AKey: TKey): Boolean;
    function NextValue(out AValue: TValue): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    constructor Create; overload;
    constructor Create(const AEmptyKey: TKey; const AEmptyValue: TValue); overload;
  end;

  { TMurmur2Hash }

  generic TMurmur2Hash<TKey> = record
    function Hash(const AKey: TKey): Cardinal;
  end;

  { TMurmur2HashString }

  TMurmur2HashString = record
    function Hash(AKey: string): Cardinal;
  end;

//{$DEFINE DIM := 2}
//{$DEFINE TCompType := Single}
//{$DEFINE TBox := TBox2f}
//{$DEFINE TLooseTree := TLooseQuadTree_f}
//{$INCLUDE LooseTree.inc}

function Murmur2(const SrcData; len: LongWord; const Seed: LongWord=$9747b28c): Cardinal;

implementation {$DEFINE IMPLEMENTATION} {$UNDEF INTERFACE}

uses
  rtlconsts, math;

{$R-}
{$Q-}
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

{ TMurmur2HashString }

function TMurmur2HashString.Hash(AKey: string): Cardinal;
begin
  if AKey = '' then Exit(1);
  Result := Murmur2(AKey[1], (Length(AKey)+1)*SizeOf(char));
  if Result = 0 then Result := 1;
end;

{ TMurmur2Hash }

function TMurmur2Hash.Hash(const AKey: TKey): Cardinal;
var p: PTypeInfo;
    arr: TBytes absolute AKey;
begin
  p := TypeInfo(TKey);
  if p^.Kind = tkDynArray then
    Result := Murmur2(arr[0], Length(arr)*Integer(GetTypeData(p)^.elSize))
  else
    Result := Murmur2(AKey, SizeOf(AKey));
  if Result = 0 then Result := 1;
end;

{ THashMap }

function THashMap.CmpUString(item1, item2: Pointer; dataSize: Integer): Boolean;
begin
  Result := string(item1^) = string(item2^);
end;

function THashMap.CmpAString(item1, item2: Pointer; dataSize: Integer): Boolean;
begin
  Result := AnsiString(item1^) = AnsiString(item2^);
end;

function THashMap.CmpRecord(item1, item2: Pointer; dataSize: Integer): Boolean;
begin
  Result := CompareMem(item1, item2, dataSize);
end;

function THashMap.CmpArray(item1, item2: Pointer; dataSize: Integer): Boolean;
var b1: TBytes absolute item1;
    b2: TBytes absolute item2;
begin
  Result := False;
  if item1 = item2 then Exit;
  if Length(b1) <> Length(b2) then Exit;
  dataSize := Length(b1)*Integer(GetTypeData(TypeInfo(TKey))^.elsize);
  if dataSize = 0 then Exit(True);
  Result := CompareMem(@b1[0], @b2[0], dataSize);
end;

function THashMap.GetCompareMethod(tinfo: PTypeInfo): TCompareMethod;
begin
  Result := Nil;
  case tinfo^.Kind of
  tkUnknown: Assert(False, 'Not implemented yet');
  tkInteger: Result := @CmpRecord;
  tkChar: Result := @CmpRecord;
  tkEnumeration: Result := @CmpRecord;
  tkFloat: Result := @CmpRecord;
  tkSet: Result := @CmpRecord;
  tkMethod: Assert(False, 'Not implemented yet');
  tkSString: Result := @CmpRecord;
  tkLString: ;
  tkAString: Result := @CmpAString;
  tkWString: ;
  tkVariant: ;
  tkArray: ;
  tkRecord: Result := @CmpRecord;
  tkInterface: Result := @CmpRecord;
  tkClass: Result := @CmpRecord;
  tkObject: Result := @CmpRecord;
  tkWChar: Result := @CmpRecord;
  tkBool: Result := @CmpRecord;
  tkInt64: Result := @CmpRecord;
  tkQWord: Result := @CmpRecord;
  tkDynArray: Result := @CmpArray;
  tkInterfaceRaw: Result := @CmpRecord;
  tkProcVar: ;
  tkUString: Result := @CmpUString;
  tkUChar: ;
  tkHelper: ;
  end;
  if Result = nil then Assert(False, 'Not implemented yet');
end;

function THashMap.PrevIndex(const index: Integer): Integer;
begin
  Result := Wrap(index-1+Length(FData));
end;

function THashMap.Wrap(const index: Integer): Integer;
begin
  //used And operation instead Mod because Length(FData) always power of two
  Result := index And (Length(FData)-1);
end;

function THashMap.CalcBucketIndex(const AKey: TKey; AHash: Cardinal; out AIndex: Integer): Boolean;
begin
  if Length(FData) = 0 then
  begin
    AIndex := 0;
    Exit(False);
  end;

  AIndex := Wrap(AHash);
  while True do
  begin
    if FData[AIndex].Hash = 0 then Exit(False);
    if (FData[AIndex].Hash = AHash) And FKeyComparer(@FData[AIndex].Key, @AKey, SizeOf(AKey)) then Exit(True);
    Inc(AIndex);
    if AIndex = Length(FData) then AIndex := 0;
  end;
end;

procedure THashMap.DoAddOrSet(BucketIndex, AHash: Integer; const AKey: TKey;
  const AValue: TValue);
begin
  if FData[BucketIndex].Hash = 0 then Inc(FCount);
  FData[BucketIndex].Hash := AHash;
  FData[BucketIndex].Key := AKey;
  FData[BucketIndex].Value := AValue;
end;

procedure THashMap.GrowIfNeeded;
begin
  if FCount >= FGrowLimit then
    Capacity := Max(4, Capacity) * 2;
end;

function THashMap.GetCapacity: Integer;
begin
  Result := Length(FData);
end;

procedure THashMap.SetCapacity(const cap: Integer);
var OldItems: TItems;
    i, bIndex: Integer;
begin
  OldItems := FData;
  FData := nil;
  FCount := 0;
  SetLength(FData, cap);
  for i := 0 to Length(OldItems)-1 do
    if OldItems[i].Hash <> 0 then
    begin
      CalcBucketIndex(OldItems[i].Key, OldItems[i].Hash, bIndex);
      DoAddOrSet(bIndex, OldItems[i].Hash, OldItems[i].Key, OldItems[i].Value);
    end;
  FGrowLimit := cap div 2 + cap div 4;
end;

function THashMap.GetItem(const AKey: TKey): TValue;
var bIndex: Integer = 0;
begin
  if not CalcBucketIndex(AKey, THash.Hash(AKey), bIndex) then
    raise EContnrsError.CreateFmt(SListIndexError, [bIndex]);
  Result := FData[bIndex].Value;
end;

procedure THashMap.SetItem(const AKey: TKey; const AValue: TValue);
var bIndex: Integer = 0;
begin
  if not CalcBucketIndex(AKey, THash.Hash(AKey), bIndex) then
    raise EContnrsError.CreateFmt(SListIndexError, [bIndex]);
  FData[bIndex].Value := AValue;
end;

function THashMap.Count: Integer;
begin
  Result := FCount;
end;

procedure THashMap.Add(const AKey: TKey; const AValue: TValue);
var hash: Cardinal;
    bIndex: Integer;
begin
  GrowIfNeeded;
  hash := THash.Hash(AKey);
  if CalcBucketIndex(AKey, hash, bIndex) then
      raise EListError.CreateFmt(SDuplicateItem, [bIndex]);
  DoAddOrSet(bIndex, hash, AKey, AValue);
end;

procedure THashMap.AddOrSet(const AKey: TKey; const AValue: TValue);
var hash: Cardinal;
    bIndex: Integer;
begin
  GrowIfNeeded;
  hash := THash.Hash(AKey);
  CalcBucketIndex(AKey, hash, bIndex);
  DoAddOrSet(bIndex, hash, AKey, AValue);
end;

procedure THashMap.Delete(const AKey: TKey);
var hash: Cardinal;
    bIndex, curInd: Integer;
begin
  hash := THash.Hash(AKey);
  if not CalcBucketIndex(AKey, hash, bIndex) then
    Exit;

  curInd := bIndex;
  while True do
  begin
    Inc(curInd);
    if curInd = Length(FData) then curInd := 0;
    hash := FData[curInd].Hash;
    if hash = 0 then Break;
    if not WrapedIndexIsBetween(bIndex, Wrap(hash), curInd) then
    begin
      FData[bIndex] := FData[curInd];
      bIndex := curInd;
      FData[bIndex].Hash := 0;
    end;
  end;
  Dec(FCount);
  FData[bIndex].Hash := 0;
  if FCleanWithEmpty then
  begin
    FData[bIndex].Key := FEmptyKey;
    FData[bIndex].Value := FEmptyValue;
  end;
end;

function THashMap.TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
var bIndex: Integer = 0;
begin
  if not CalcBucketIndex(AKey, THash.Hash(AKey), bIndex) then
    Exit(False);
  Result := True;
  AValue := FData[bIndex].Value;
end;

function THashMap.Contains(const AKey: TKey): Boolean;
var bIndex: Integer;
begin
  Result := CalcBucketIndex(AKey, THash.Hash(AKey), bIndex);
end;

function THashMap.ContainsValue(const AValue: TValue): Boolean;
var dummyKey: TKey;
begin
  Result := ContainsValue(AValue, dummyKey);
end;

function THashMap.ContainsValue(const AValue: TValue; out AKey: TKey): Boolean;
var i: Integer;
begin
  for i := 0 to Length(FData)-1 do
    if FValueComparer(@FData[i].Value, @AValue, SizeOf(AValue)) then
    begin
      Result := True;
      AKey := FData[i].Key;
      Exit;
    end;
  Result := False;
end;

procedure THashMap.Clear;
begin
  FData := nil;
  FGrowLimit := 0;
  FCount := 0;
end;

function THashMap.WrapedIndexIsBetween(Left, Index, Right: Integer): Boolean;
begin
  if Left < Right then
    Result := (Left < Index) and (Index <= Right) // not wrapped range case
  else
    Result := (Index > Left) or (Index <= Right); // range is wraped, check outside
end;

procedure THashMap.Reset;
begin
  FEnumIndex := -1;
end;

function THashMap.Next(out AKey: TKey; out AValue: TValue): Boolean;
begin
  Inc(FEnumIndex);
  while FEnumIndex < Length(FData) do
  begin
    if FData[FEnumIndex].Hash <> 0 then
    begin
      AKey   := FData[FEnumIndex].Key;
      AValue := FData[FEnumIndex].Value;
      Exit(True);
    end;
    Inc(FEnumIndex);
  end;
  Result := False;
end;

function THashMap.NextKey(out AKey: TKey): Boolean;
begin
 Inc(FEnumIndex);
 while FEnumIndex < Length(FData) do
 begin
   if FData[FEnumIndex].Hash <> 0 then
   begin
     AKey   := FData[FEnumIndex].Key;
     Exit(True);
   end;
   Inc(FEnumIndex);
 end;
 Result := False;
end;

function THashMap.NextValue(out AValue: TValue): Boolean;
begin
 Inc(FEnumIndex);
 while FEnumIndex < Length(FData) do
 begin
   if FData[FEnumIndex].Hash <> 0 then
   begin
     AValue := FData[FEnumIndex].Value;
     Exit(True);
   end;
   Inc(FEnumIndex);
 end;
 Result := False;
end;

constructor THashMap.Create;
begin
  FCleanWithEmpty := False;
  FKeyComparer := GetCompareMethod(TypeInfo(TKey));
  FValueComparer := GetCompareMethod(TypeInfo(TValue));
end;

constructor THashMap.Create(const AEmptyKey: TKey; const AEmptyValue: TValue);
begin
  FCleanWithEmpty := True;
  FEmptyKey := AEmptyKey;
  FEmptyValue := AEmptyValue;
  FKeyComparer := GetCompareMethod(TypeInfo(TKey));
  FValueComparer := GetCompareMethod(TypeInfo(TValue));
end;

{ TArray }

function TArray.CmpUString(item1, item2: Pointer; dataSize: Integer): Boolean;
begin
  Result := string(item1) = string(item2);
end;

function TArray.CmpAString(item1, item2: Pointer; dataSize: Integer): Boolean;
begin
  Result := AnsiString(item1) = AnsiString(item2);
end;

function TArray.CmpRecord(item1, item2: Pointer; dataSize: Integer): Boolean;
begin
  Result := CompareMem(item1, item2, dataSize);
end;

function TArray.CmpArray(item1, item2: Pointer; dataSize: Integer): Boolean;
var b1: TBytes absolute item1;
    b2: TBytes absolute item2;
begin
  Result := False;
  if item1 = item2 then Exit;
  if Length(b1) <> Length(b2) then Exit;
  dataSize := Length(b1)*Integer(GetTypeData(TypeInfo(TValue))^.elsize);
  if dataSize = 0 then Exit(True);
  Result := CompareMem(@b1[0], @b2[0], dataSize);
end;

function TArray.GetCompareMethod(tinfo: PTypeInfo): TCompareMethod;
begin
  case tinfo^.Kind of
  tkUnknown: Assert(False, 'Not implemented yet');
  tkInteger: Result := @CmpRecord;
  tkChar: Result := @CmpRecord;
  tkEnumeration: Result := @CmpRecord;
  tkFloat: Result := @CmpRecord;
  tkSet: Result := @CmpRecord;
  tkMethod: Assert(False, 'Not implemented yet');
  tkSString: Result := @CmpRecord;
  tkLString: ;
  tkAString: Result := @CmpAString;
  tkWString: ;
  tkVariant: ;
  tkArray: ;
  tkRecord: Result := @CmpRecord;
  tkInterface: Result := @CmpRecord;
  tkClass: Result := @CmpRecord;
  tkObject: Result := @CmpRecord;
  tkWChar: Result := @CmpRecord;
  tkBool: Result := @CmpRecord;
  tkInt64: Result := @CmpRecord;
  tkQWord: Result := @CmpRecord;
  tkDynArray: Result := @CmpArray;
  tkInterfaceRaw: Result := @CmpRecord;
  tkProcVar: ;
  tkUString: Result := @CmpUString;
  tkUChar: ;
  tkHelper: ;
  end;
end;

procedure TArray.QuickSort(L, R: Longint; Compare: TCompareMethodStatic;
  userData: Pointer);
var
  I, J : Longint;
  P, Q : TValue;
begin
 repeat
   I := L;
   J := R;
   P := FData[ (L + R) div 2 ];
   repeat
     while Compare(@P, @FData[i], SizeOf(TValue), userData) > 0 do
       I := I + 1;
     while Compare(@P, @FData[J], SizeOf(TValue), userData) < 0 do
       J := J - 1;
     If I <= J then
     begin
       Q := FData[I];
       FData[I] := FData[J];
       FData[J] := Q;
       I := I + 1;
       J := J - 1;
     end;
   until I > J;
   // sort the smaller range recursively
   // sort the bigger range via the loop
   // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
   if J - L < R - I then
   begin
     if L < J then
       QuickSort(L, J, Compare, userData);
     L := I;
   end
   else
   begin
     if I < R then
       QuickSort(I, R, Compare, userData);
     R := J;
   end;
 until L >= R;
end;

function TArray.GetCapacity: Integer;
begin
  Result := Length(FData);
end;

procedure TArray.SetCapacity(const cap: Integer);
begin
  if cap <> Length(FData) then
    SetLength(FData, cap);
end;

function TArray.GetItem(const index: Integer): TValue;
begin
  Result := FData[index];
end;

procedure TArray.SetItem(const index: Integer; const AValue: TValue);
begin
  FData[index] := AValue;
end;

function TArray.GetPItem(const index: Integer): Pointer;
begin
  Result := @FData[index];
end;

function TArray.Count: Integer;
begin
  Result := FCount;
end;

function TArray.Add(const item: TValue): Integer;
begin
  Result := FCount;
  if Capacity < 8 then
    Capacity := 8
  else
    if Capacity <= FCount then
      Capacity := Capacity * 2;
  FData[FCount] := item;
  Inc(FCount);
end;

procedure TArray.Delete(const index: Integer);
var I: Integer;
begin
  for I := index to FCount - 2 do
    FData[I] := FData[I+1];
  Dec(FCount);
  if FCleanWithEmpty then
    FData[FCount] := FEmpty;
end;

function TArray.IndexOf(const item: TValue): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if FItemComparer(@FData[I], @item, SizeOf(item)) then
      Exit(I);
end;

procedure TArray.Clear(const TrimCapacity: Boolean);
var I: Integer;
begin
  if FCleanWithEmpty then
    for I := 0 to FCount - 1 do
      FData[I] := FEmpty;
  FCount := 0;
  if TrimCapacity then
    SetLength(FData, 0);
end;

procedure TArray.Sort(comparator: TCompareMethodStatic; userData: Pointer);
begin
  if FCount < 2 then Exit;
  QuickSort(0, FCount-1, comparator, userData);
end;

constructor TArray.Create;
begin
  FCleanWithEmpty := False;
  FItemComparer := GetCompareMethod(TypeInfo(TValue));
end;

constructor TArray.Create(const AEmptyValue: TValue);
begin
  FCleanWithEmpty := True;
  FEmpty := AEmptyValue;
  FItemComparer := GetCompareMethod(TypeInfo(TValue));
end;

//{$DEFINE DIM := 2}
//{$DEFINE TCompType := Single}
//{$DEFINE TBox := TBox2f}
//{$DEFINE TLooseTree := TLooseQuadTree_f}
//{$INCLUDE LooseTree.inc}

end. {$UNDEF IMPLEMENTATION}
