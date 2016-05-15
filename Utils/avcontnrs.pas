unit avContnrs;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$macro ON}

interface {$DEFINE INTERFACE}

uses
  Classes, SysUtils, mutils, FGL, typinfo, avContnrsDefaults;

type
  TInterfacedObjectEx = avContnrsDefaults.TInterfacedObjectEx;

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
    procedure DeleteWithSwap(const index: Integer);
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
    procedure DeleteWithSwap(const index: Integer);
    function IndexOf(const item: TValue): Integer;

    procedure Clear(const TrimCapacity: Boolean = False);

    procedure Sort(comparator: TCompareMethodStatic; userData: Pointer);

    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    constructor Create; overload;
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
    procedure Add(const AKey: TKey);
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

      IEqComparer  = specialize IEqualityComparer<TKey>;
      TEQC_UString = specialize TEqualityComparer_UString<TKey>;
      TEQC_WString = specialize TEqualityComparer_WString<TKey>;
      TEQC_AString = specialize TEqualityComparer_AString<TKey>;
      TEQC_Data    = specialize TEqualityComparer_Data<TKey>;
      TEQC_Array   = specialize TEqualityComparer_Array<TKey>;
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

    FComparer: IEqComparer;
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
    constructor Create; virtual;
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
    procedure Add(const AKey: TKey);
    procedure AddOrSet(const AKey: TKey);
    procedure Delete(const AKey: TKey);
    function Contains(const AKey: TKey): Boolean;
    procedure Clear;

    procedure Reset;
    function Next(out AKey: TKey): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    constructor Create; virtual;
  end;

function IsAutoReferenceCounterType(const AType: PTypeInfo): Boolean;

implementation {$DEFINE IMPLEMENTATION} {$UNDEF INTERFACE}

uses
  rtlconsts, math;

{$R-}
{$Q-}

function IsAutoReferenceCounterType(const AType: PTypeInfo): Boolean;
var td: PTypeData;
    mf: PManagedField;
    n, i: Integer;
begin
  case AType^.Kind of
  tkUnknown     : Assert(False, 'What???');
  tkInteger     : Result := False;
  tkChar        : Result := False;
  tkEnumeration : Result := False;
  tkFloat       : Result := False;
  tkSet         : Result := False;
  tkMethod      : Result := False;
  tkSString     : Result := False;
  tkLString     : Result := True;
  tkAString     : Result := True;
  tkWString     : Result := True;
  tkVariant     : Result := True;
  tkArray       : Result := False;
  tkRecord      : begin
                    Result := False;
                    td := GetTypeData(AType);
                    n := td^.ManagedFldCount;
                    mf := PManagedField(@td^.ManagedFldCount);
                    Inc(PByte(mf), SizeOf(td^.ManagedFldCount));
                    for i := 0 to n-1 do
                    begin
                      if IsAutoReferenceCounterType(mf^.TypeRef) then
                      begin
                        Result := True;
                        Exit;
                      end;
                      Inc(mf);
                    end;
                  end;
  tkInterface   : Result := True;
  tkClass       : Result := False;
  tkObject      : Result := False;
  tkWChar       : Result := False;
  tkBool        : Result := False;
  tkInt64       : Result := False;
  tkQWord       : Result := False;
  tkDynArray    : Result := True;
  tkInterfaceRaw: Result := False;
  tkProcVar     : Result := False;
  tkUString     : Result := True;
  tkUChar       : Result := False;
  tkHelper      : Result := False;
  tkFile        : Result := False;
  tkClassRef    : Result := False;
  tkPointer     : Result := False;
  end;
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
  FHash.NextKey(AKey);
end;

constructor THashSet.Create;
begin
  FHash := TMap.Create;
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
var pInfo: PTypeInfo;
begin
  if FComparer = nil then
  begin
    pInfo := TypeInfo(TKey);
    case pInfo^.Kind of
      tkUnknown     : Assert(False, 'What?');
      tkInteger     : FComparer := TEQC_Data.Create;
      tkChar        : FComparer := TEQC_Data.Create;
      tkEnumeration : FComparer := TEQC_Data.Create;
      tkFloat       : FComparer := TEQC_Data.Create;
      tkSet         : FComparer := TEQC_Data.Create;
      tkMethod      : FComparer := TEQC_Data.Create;
      tkSString     : FComparer := TEQC_Data.Create;
      tkLString     : Assert(False, 'Todooo');
      tkAString     : FComparer := TEQC_AString.Create;
      tkWString     : FComparer := TEQC_WString.Create;
      tkVariant     : Assert(False, 'Todooo');
      tkArray       : FComparer := TEQC_Data.Create;
      tkRecord      : FComparer := TEQC_Data.Create;
      tkInterface   : FComparer := TEQC_Data.Create;
      tkClass       : FComparer := TEQC_Data.Create;
      tkObject      : FComparer := TEQC_Data.Create;
      tkWChar       : FComparer := TEQC_Data.Create;
      tkBool        : FComparer := TEQC_Data.Create;
      tkInt64       : FComparer := TEQC_Data.Create;
      tkQWord       : FComparer := TEQC_Data.Create;
      tkDynArray    : FComparer := TEQC_Array.Create;
      tkInterfaceRaw: FComparer := TEQC_Data.Create;
      tkProcVar     : FComparer := TEQC_Data.Create;
      tkUString     : FComparer := TEQC_UString.Create;
      tkUChar       : FComparer := TEQC_Data.Create;
      tkHelper      : FComparer := TEQC_Data.Create;
      tkFile        : FComparer := TEQC_Data.Create;
      tkClassRef    : FComparer := TEQC_Data.Create;
      tkPointer     : FComparer := TEQC_Data.Create;
    else
      Assert(False, 'Usupported type of key');
    end;
  end;
end;

constructor THashMap.Create;
begin
  AutoSelectComparer;
  FEmptyKey := Default(TKey);
  FEmptyValue := Default(TValue);
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
  tkFile: ;
  tkClassRef: Result := @CmpRecord;
  tkPointer: Result := @CmpRecord;
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
  FCleanWithEmpty := IsAutoReferenceCounterType(TypeInfo(TValue));
  if FCleanWithEmpty then FEmpty := Default(TValue);
  FItemComparer := GetCompareMethod(TypeInfo(TValue));
end;

end. {$UNDEF IMPLEMENTATION}
