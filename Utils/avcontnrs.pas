unit avContnrs;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, mutils, FGL, typinfo, avContnrsDefaults;

type
  TInterfacedObjectEx = avContnrsDefaults.TInterfacedObjectEx;

  EContnrsError = class(Exception);

  generic TArrData<T> = array of T;
type

  { IHeap }

  generic IHeap<T> = interface
    function Count: Integer;
    function PeekTop(): T;
    function ExtractTop(): T;
    procedure Insert(const Value: T);

    procedure Trim;
    procedure Clear(const NewCapacity: Integer);
  end;

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
    procedure DeleteWithSwap(const index: Integer);
    function  IndexOf(const item: TValue): Integer;

    procedure Clear(const TrimCapacity: Boolean = False);

    procedure Sort(const comparator: IComparer = nil); overload;
    procedure HeapSort(const comparator: IComparer = nil); overload;

    property Item[index: Integer]: TValue read GetItem write SetItem; default;
    property PItem[index: Integer]: Pointer read GetPItem;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { THeap }

  generic THeap<T> = class (TInterfacedObjectEx, specialize IHeap<T>)
  private type
    TData = specialize TArrData<T>;
  private
    FData  : TData;
    FCount : Integer;

    FComparer: IComparer;

    FEnumIndex: Integer;

    procedure Grow;

    function GetMinIndex(const i1, i2: Integer): Integer; inline;
    procedure Swap(const i1, i2: Integer);
    procedure SiftDown(Index: Integer);
    procedure SiftUp(Index: Integer);

    procedure AutoSelectComparer;
  public
    function Count: Integer;
    function PeekTop(): T;
    function ExtractTop(): T;
    function ExtractTop_NoClean_NoTrim(): T;
    procedure Insert(const Value: T);

    procedure Reset;
    function Next(out AItem: T): Boolean;

    procedure Trim;
    procedure Clear(const NewCapacity: Integer);

    constructor Create(const ArrData: TData; const ACount: Integer = -1; const AComparer: IComparer = nil); overload;
    constructor Create(const ACapacity: Integer); overload;
    constructor Create(const ACapacity: Integer; const AComparer: IComparer); overload;
  end;

  { TArray }

  generic TArray<TValue> = class (TInterfacedObjectEx, specialize IArray<TValue>)
  private type
    THeapSrt = specialize THeap<TValue>;
  private
    FData : array of TValue;
    FCount: Integer;
    FCleanWithEmpty: Boolean;
    FEmpty: TValue;

    FComparator: IComparer;

    //FItemComparer: TCompareMethod;
  private
    procedure QuickSort(L, R : Longint; const comparator: IComparer);
  protected
    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);
    function GetItem(const index: Integer): TValue;
    procedure SetItem(const index: Integer; const AValue: TValue);
    function GetPItem(const index: Integer): Pointer;

    function Count: Integer;

    function Add(const item: TValue): Integer; //return index of new added element
    procedure Delete(const index: Integer);
    procedure DeleteWithSwap(const index: Integer);
    function IndexOf(const item: TValue): Integer;

    procedure Clear(const TrimCapacity: Boolean = False);

    procedure Sort(const comparator: IComparer = nil); overload;
    procedure HeapSort(const comparator: IComparer = nil); overload;

    property Capacity: Integer read GetCapacity write SetCapacity;

    procedure AutoSelectComparer;
  public
    constructor Create(const AComparator: IComparer = nil); overload;
  end;

  IObjArr = specialize IArray<TObject>;
  TObjArr = specialize TArray<TObject>;

  { IHashMap }

  generic IHashMap<TKey, TValue> = interface
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
    procedure Clear;

    procedure Reset;
    function Next(out AKey: TKey; out AValue: TValue): Boolean;
    function NextKey(out AKey: TKey): Boolean;
    function NextValue(out AValue: TValue): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Item[AKey: TKey]: TValue read GetItem write SetItem; default;
  end;

  { IHashSet }

  generic IHashSet<TKey> = interface
    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);

    function Count: Integer;
    procedure Add(const AKey: TKey); overload;
    //procedure Add(const Arr : specialize IArray<TKey>); overload;
    procedure AddOrSet(const AKey: TKey);
    procedure Delete(const AKey: TKey);
    function Contains(const AKey: TKey): Boolean;
    procedure Clear;

    procedure Reset;
    function Next(out AKey: TKey): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { THashMap }

  generic THashMap<TKey, TValue> = class (TInterfacedObjectEx, specialize IHashMap<TKey, TValue>)
  private
    type
      TItem = packed record
        Hash : Cardinal;
        Key  : TKey;
        Value: TValue;
      end;
      TItems = array of TItem;
  public type
    TPair = record
      Key  : TKey;
      Value: TValue;
    end;
  strict private
    FEnumIndex: Integer;
    FData: TItems;
    FGrowLimit: Integer;
    FCount: Integer;

    FComparer: IEqualityComparer;
    FEmptyKey: TKey;
    FEmptyValue: TValue;

    function PrevIndex(const index: Integer): Integer; {$IfNDef NoInline}inline;{$EndIf}
    function Wrap(const index: Integer): Integer; {$IfNDef NoInline}inline;{$EndIf}
    function CalcBucketIndex(const AKey: TKey; AHash: Cardinal; out AIndex: Integer): Boolean; {$IfNDef NoInline}inline;{$EndIf}
    procedure DoAddOrSet(BucketIndex, AHash: Integer; const AKey: TKey; const AValue: TValue); {$IfNDef NoInline}inline;{$EndIf}
    procedure GrowIfNeeded; {$IfNDef NoInline}inline;{$EndIf}

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
    procedure Clear;

    function WrapedIndexIsBetween(Left, Index, Right: Integer): Boolean;

    procedure Reset;
    function Next(out AKey: TKey; out AValue: TValue): Boolean;
    function NextKey(out AKey: TKey): Boolean;
    function NextValue(out AValue: TValue): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;

    procedure AutoSelectComparer;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IEqualityComparer); overload;
  end;

  { THashSet }

  generic THashSet<TKey> = class (TInterfacedObjectEx, specialize IHashSet<TKey>)
  private type
    TMap = specialize THashMap<TKey, Boolean>;
    IMap = specialize IHashMap<TKey, Boolean>;
  strict private
    FHash : IMap;

    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);

    function Count: Integer;
    procedure Add(const AKey: TKey); overload;
    procedure AddOrSet(const AKey: TKey);
    procedure Delete(const AKey: TKey);
    function Contains(const AKey: TKey): Boolean;
    procedure Clear;

    procedure Reset;
    function Next(out AKey: TKey): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    constructor Create; overload;
    constructor Create(const AComparer: IEqualityComparer); overload;
  end;

implementation

uses
  rtlconsts, math;

procedure THeap.Grow;
begin
  SetLength(FData, NextPow2(FCount + 1));
end;

procedure THeap.Trim;
begin
  SetLength(FData, FCount);
end;

procedure THeap.Clear(const NewCapacity: Integer);
begin
  FData := nil;
  FCount := 0;
  SetLength(FData, NewCapacity);
end;

function THeap.GetMinIndex(const i1, i2: Integer): Integer;
begin
  if FComparer.Compare(FData[i1], FData[i2]) < 0 then
    Result := i1
  else
    Result := i2;
end;

procedure THeap.Swap(const i1, i2: Integer);
var tmp: T;
begin
  tmp := FData[i2];
  FData[i2] := FData[i1];
  FData[i1] := tmp;
end;

procedure THeap.SiftDown(Index: Integer);
var leftIdx, rightIdx: Integer;
    minChild: Integer;
begin
  while True do
  begin
    leftIdx := 2 * Index + 1;
    if leftIdx >= FCount then Exit; // no left child

    rightIdx := 2 * Index + 2;
    if rightIdx < FCount then
      minChild := GetMinIndex(leftIdx, rightIdx)
    else
      minChild := leftIdx;

    if FComparer.Compare(FData[Index], FData[minChild]) <= 0 then Exit; // sift completed
    Swap(Index, minChild);
    Index := minChild;
  end;
end;

procedure THeap.SiftUp(Index: Integer);
var parentIdx: Integer;
begin
  while True do
  begin
    if Index = 0 then Exit; // at Root
    parentIdx := (Index - 1) div 2;
    if FComparer.Compare(FData[parentIdx], FData[Index]) <= 0 then Exit; // sift completed
    Swap(parentIdx, Index);
    Index := parentIdx;
  end;
end;

procedure THeap.AutoSelectComparer;
begin
  if FComparer = nil then
    FComparer := avContnrsDefaults.AutoSelectComparer(TypeInfo(T), SizeOf(T));
end;

function THeap.Count: Integer;
begin
  Result := FCount;
end;

function THeap.PeekTop: T;
begin
  if FCount = 0 then raise ERangeError.Create('Index 0 out of bound');
  Result := FData[0];
end;

function THeap.ExtractTop: T;
begin
  if FCount = 0 then raise ERangeError.Create('Index 0 out of bound');
  Result := FData[0];
  FData[0] := FData[FCount - 1];
  FData[FCount - 1] := Default(T);
  Dec(FCount);
  SiftDown(0);

  if Length(FData) div 4 >= FCount then Trim;
end;

function THeap.ExtractTop_NoClean_NoTrim: T;
begin
  if FCount = 0 then raise ERangeError.Create('Index 0 out of bound');
  Result := FData[0];
  Swap(0, FCount - 1);
  Dec(FCount);
  SiftDown(0);
end;

procedure THeap.Insert(const Value: T);
begin
  if FCount = Length(FData) then Grow;
  FData[FCount] := Value;
  Inc(FCount);
  SiftUp(FCount - 1);
end;

procedure THeap.Reset;
begin
  FEnumIndex := -1;
end;

function THeap.Next(out AItem: T): Boolean;
begin
  Inc(FEnumIndex);
  if FEnumIndex >= FCount then
  begin
    AItem := Default(T);
    Result := False;
  end
  else
  begin
    AItem := FData[FEnumIndex];
    Result := True;
  end;
end;

constructor THeap.Create(const ArrData: TData; const ACount: Integer; const AComparer: IComparer);
var i: Integer;
begin
  FData := ArrData;
  FComparer := AComparer;
  AutoSelectComparer;
  if ACount < 0 then
    FCount := Length(ArrData)
  else
    FCount := ACount;

  //heapify
  for i := FCount div 2 - 1 downto 0 do
    siftDown(i);
end;

constructor THeap.Create(const ACapacity: Integer);
begin
  Create(ACapacity, nil);
end;

constructor THeap.Create(const ACapacity: Integer; const AComparer: IComparer);
begin
  SetLength(FData, ACapacity);
  FComparer := AComparer;
  AutoSelectComparer;
  FCount := 0;
end;

{ THashSet }

function THashSet.GetCapacity: Integer;
begin
  Result := FHash.Capacity;
end;

procedure THashSet.SetCapacity(const cap: Integer);
begin
  FHash.Capacity := cap;
end;

function THashSet.Count: Integer;
begin
  Result := FHash.Count;
end;

procedure THashSet.Add(const AKey: TKey);
begin
  FHash.Add(AKey, False);
end;

procedure THashSet.AddOrSet(const AKey: TKey);
begin
 FHash.AddOrSet(AKey, False);
end;

procedure THashSet.Delete(const AKey: TKey);
begin
 FHash.Delete(AKey);
end;

function THashSet.Contains(const AKey: TKey): Boolean;
begin
 Result := FHash.Contains(AKey);
end;

procedure THashSet.Clear;
begin
  FHash.Clear;
end;

procedure THashSet.Reset;
begin
  FHash.Reset;
end;

function THashSet.Next(out AKey: TKey): Boolean;
begin
  Result := FHash.NextKey(AKey);
end;

constructor THashSet.Create;
begin
  Create(nil);
end;

constructor THashSet.Create(const AComparer: IEqualityComparer);
begin
  FHash := TMap.Create(AComparer);
end;

{ THashMap }

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
    if (FData[AIndex].Hash = AHash) And FComparer.IsEqual(FData[AIndex].Key, AKey) then Exit(True);
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
  FGrowLimit := cap div 2;
end;

function THashMap.GetItem(const AKey: TKey): TValue;
var bIndex: Integer = 0;
begin
  if not CalcBucketIndex(AKey, FComparer.Hash(AKey), bIndex) then
    raise EContnrsError.CreateFmt(SListIndexError, [bIndex]);
  Result := FData[bIndex].Value;
end;

procedure THashMap.SetItem(const AKey: TKey; const AValue: TValue);
var bIndex: Integer = 0;
begin
  if not CalcBucketIndex(AKey, FComparer.Hash(AKey), bIndex) then
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
  hash := FComparer.Hash(AKey);
  if CalcBucketIndex(AKey, hash, bIndex) then
      raise EListError.CreateFmt(SDuplicateItem, [bIndex]);
  DoAddOrSet(bIndex, hash, AKey, AValue);
end;

procedure THashMap.AddOrSet(const AKey: TKey; const AValue: TValue);
var hash: Cardinal;
    bIndex: Integer;
begin
  GrowIfNeeded;
  hash := FComparer.Hash(AKey);
  CalcBucketIndex(AKey, hash, bIndex);
  DoAddOrSet(bIndex, hash, AKey, AValue);
end;

procedure THashMap.Delete(const AKey: TKey);
var hash: Cardinal;
    bIndex, curInd: Integer;
begin
  hash := FComparer.Hash(AKey);
  if not CalcBucketIndex(AKey, hash, bIndex) then
    Exit;

  curInd := bIndex;
  while True do
  begin
    Inc(curInd);
    if curInd = Length(FData) then curInd := 0;
    hash := FData[curInd].Hash;
    if hash = EMPTY_HASH then Break;
    if not WrapedIndexIsBetween(bIndex, Wrap(hash), curInd) then
    begin
      FData[bIndex] := FData[curInd];
      bIndex := curInd;
      FData[bIndex].Hash := EMPTY_HASH;
    end;
  end;
  Dec(FCount);
  FData[bIndex].Hash := EMPTY_HASH;
  FData[bIndex].Key := FEmptyKey;
  FData[bIndex].Value := FEmptyValue;
end;

function THashMap.TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
var bIndex: Integer = 0;
    hash: Integer;
begin
  hash := FComparer.Hash(AKey);
  if not CalcBucketIndex(AKey, hash, bIndex) then
    Exit(False);
  Result := True;
  AValue := FData[bIndex].Value;
end;

function THashMap.Contains(const AKey: TKey): Boolean;
var bIndex: Integer;
begin
  Result := CalcBucketIndex(AKey, FComparer.Hash(AKey), bIndex);
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

procedure THashMap.AutoSelectComparer;
begin
  if FComparer = nil then
    FComparer := AutoSelectEqualityComparer(TypeInfo(TKey), SizeOf(TKey));
end;

constructor THashMap.Create;
begin
  Create(nil);
end;

constructor THashMap.Create(const AComparer: IEqualityComparer);
begin
  FComparer := AComparer;
  AutoSelectComparer;
  FEmptyKey := Default(TKey);
  FEmptyValue := Default(TValue);
end;

{ TArray }

procedure TArray.QuickSort(L, R: Longint; const comparator: IComparer);
var
  I, J : Longint;
  P, Q : TValue;
begin
 repeat
   I := L;
   J := R;
   P := FData[ (L + R) div 2 ];
   repeat
     while comparator.Compare(P, FData[i]) > 0 do
       I := I + 1;
     while comparator.Compare(P, FData[J]) < 0 do
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
       QuickSort(L, J, comparator);
     L := I;
   end
   else
   begin
     if I < R then
       QuickSort(I, R, comparator);
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

procedure TArray.DeleteWithSwap(const index: Integer);
var last: Integer;
begin
  last := Count - 1;
  if index <> last then
    FData[index] := FData[last];
  Dec(FCount);
  if FCleanWithEmpty then
    FData[last] := FEmpty;
end;

function TArray.IndexOf(const item: TValue): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if FComparator.Compare(FData[I], item)=0 then
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

procedure TArray.Sort(const comparator: IComparer);
begin
  if FCount < 2 then Exit;
  if comparator = nil then
    QuickSort(0, FCount-1, FComparator)
  else
    QuickSort(0, FCount-1, comparator);
end;

procedure TArray.HeapSort(const comparator: IComparer);
var Heap: THeapSrt;
begin
  Heap := nil;
  try
    if comparator = nil then
      Heap := THeapSrt.Create(FData, FCount, TInvertedComparer.Create(FComparator) )
    else
      Heap := THeapSrt.Create(FData, FCount, TInvertedComparer.Create(comparator) );
    while Heap.Count > 0 do
      Heap.ExtractTop_NoClean_NoTrim;
  finally
    FreeAndNil(Heap);
  end;
end;

procedure TArray.AutoSelectComparer;
begin
  if FComparator = nil then
    FComparator := avContnrsDefaults.AutoSelectComparer(TypeInfo(TValue), SizeOf(TValue));
end;

constructor TArray.Create(const AComparator: IComparer = nil);
begin
  FCleanWithEmpty := IsAutoReferenceCounterType(TypeInfo(TValue));
  if FCleanWithEmpty then FEmpty := Default(TValue);
  FComparator := AComparator;
  AutoSelectComparer;

  //FItemComparer := GetCompareMethod(TypeInfo(TValue));
end;

end. {$UNDEF IMPLEMENTATION}
