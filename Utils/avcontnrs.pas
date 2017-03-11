unit avContnrs;
{$I avConfig.inc}

interface

uses
  Classes, SysUtils, mutils, typinfo, avContnrsDefaults;

type
  TInterfacedObjectEx = avContnrsDefaults.TInterfacedObjectEx;

  EContnrsError = class(Exception);

  {$IfDef FPC}generic{$EndIf}
  TArrData<T> = array of T;
type

  { IHeap }

  {$IfDef FPC}generic{$EndIf}
  IHeap<T> = interface
    function GetOnDuplicate: IDuplicateResolver;
    procedure SetOnDuplicate(const AValue: IDuplicateResolver);
    property OnDuplicate: IDuplicateResolver read GetOnDuplicate write SetOnDuplicate;

    function Count: Integer;
    function PeekTop(): T;
    function ExtractTop(): T;
    procedure Insert(const Value: T);

    procedure Trim;
    procedure Clear(const NewCapacity: Integer);
  end;

  { IArray }

  {$IfDef FPC}generic{$EndIf}
  IArray<TValue> = interface
    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);
    function GetItem(index: Integer): TValue;
    procedure SetItem(index: Integer; const AValue: TValue);
    function GetPItem(index: Integer): Pointer;

    function Count: Integer;

    function  Add(const item: TValue): Integer; //index of new added element
    procedure AddArray(const arr: IArray{$IfDef DCC}<TValue>{$EndIf});
    procedure Delete(const index: Integer);
    procedure DeleteWithSwap(const index: Integer);
    function  IndexOf(const item: TValue): Integer;
    function  Sub(const arr: IArray{$IfDef DCC}<TValue>{$EndIf}): {$IfDef FPC}specialize{$EndIf}IArray<TValue>;

    procedure Swap(const I1, I2: Integer);

    procedure Clear(const TrimCapacity: Boolean = False);

    procedure Sort(const comparator: IComparer = nil); overload;
    procedure HeapSort(const comparator: IComparer = nil); overload;

    property Item[index: Integer]: TValue read GetItem write SetItem; default;
    property PItem[index: Integer]: Pointer read GetPItem;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { THeap }

  {$IfDef FPC}generic{$EndIf}
  THeap<T> = class (TInterfacedObjectEx, {$IfDef FPC}specialize{$EndIf} IHeap<T>)
  protected type
    TData = {$IfDef FPC}specialize{$EndIf} TArrData<T>;
  protected
    FData  : TData;
    FCount : Integer;

    FComparer: IComparer;

    FEnumIndex: Integer;
    FOnDuplicate: IDuplicateResolver;

    procedure Grow;

    function GetMinIndex(const i1, i2: Integer): Integer; inline;
    procedure Swap(const i1, i2: Integer);
    procedure SiftDown(Index: Integer);
    procedure SiftUp(Index: Integer);

    procedure AutoSelectComparer;
  public
    function GetOnDuplicate: IDuplicateResolver;
    procedure SetOnDuplicate(const AValue: IDuplicateResolver);
    property OnDuplicate: IDuplicateResolver read GetOnDuplicate write SetOnDuplicate;

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

  {$IfDef FPC}generic{$EndIf}
  TArray<TValue> = class (TInterfacedObjectEx, {$IfDef FPC}specialize{$EndIf} IArray<TValue>)
  private type
    THeapSrt = {$IfDef FPC}specialize{$EndIf} THeap<TValue>;
    TData = {$IfDef FPC}specialize{$EndIf} TArrData<TValue>;
    IArr = {$IfDef FPC}specialize{$EndIf} IArray<TValue>;
    TArr = {$IfDef FPC}specialize{$EndIf} TArray<TValue>;
  private
    FData : TData;
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
    function GetItem(index: Integer): TValue;
    procedure SetItem(index: Integer; const AValue: TValue);
    function GetPItem(index: Integer): Pointer;

    function Count: Integer;

    function  Add(const item: TValue): Integer; //return index of new added element
    procedure AddArray(const arr: IArr); //index of new added element
    procedure Delete(const index: Integer);
    procedure DeleteWithSwap(const index: Integer);
    function  IndexOf(const item: TValue): Integer;

    function  Sub (const arr: IArr): {$IfDef FPC}specialize{$EndIf}IArray<TValue>;

    procedure Swap(const I1, I2: Integer);

    procedure Clear(const TrimCapacity: Boolean = False);

    procedure Sort(const comparator: IComparer = nil); overload;
    procedure HeapSort(const comparator: IComparer = nil); overload;

    property Capacity: Integer read GetCapacity write SetCapacity;

    procedure AutoSelectComparer;
  public
    constructor Create(const AComparator: IComparer = nil); overload;
  end;

  IObjArr = {$IfDef FPC}specialize{$EndIf} IArray<TObject>;
  TObjArr = {$IfDef FPC}specialize{$EndIf} TArray<TObject>;

  { IQueue }

  {$IfDef FPC}generic{$EndIf}
  IQueue<TValue> = interface
    function GetCapacity: Integer;
    procedure SetCapacity(cap: Integer);

    function Count: Integer;

    procedure Push(const item: TValue); overload;
    //procedure Push(const items: specialize IArray<TValue>); overload;
    function Pop: TValue;
    function Peek: TValue;

    procedure Clear(const TrimCapacity: Boolean = False);

    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { TQueue }

  {$IfDef FPC}generic{$EndIf}
  TQueue<TValue> = class (TInterfacedObjectEx, {$IfDef FPC}specialize{$EndIf} IQueue<TValue>)
  private type
    TDataArr = array of TValue;
  private
    FData: TDataArr;
    FSeek: Cardinal;
    FEnd : Cardinal;
  private
    function GetCapacity: Integer;
    procedure SetCapacity(cap: Integer);

    function Count: Integer;

    procedure Push(const item: TValue); overload;
    //procedure Push(const items: specialize IArray<TValue>); overload;
    function Pop: TValue;
    function Peek: TValue;

    procedure Clear(const TrimCapacity: Boolean = False);

    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    constructor Create;
  end;

  { IHashMap }

  {$IfDef FPC}generic{$EndIf}
  IHashMap<TKey, TValue> = interface
    function GetOnDuplicate: IDuplicateResolver;
    procedure SetOnDuplicate(const AValue: IDuplicateResolver);

    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);
    function GetItem({$IfDef FPC}const{$EndIf} AKey: TKey): TValue;
    function GetPItem({$IfDef FPC}const{$EndIf} AKey: TKey): Pointer;
    procedure SetItem({$IfDef FPC}const{$EndIf} AKey: TKey; const AValue: TValue);

    function Count: Integer;
    function Add(const AKey: TKey; const AValue: TValue): Boolean;
    function AddIfNotContains(const AKey: TKey; const AValue: TValue): Boolean;
    procedure AddOrSet(const AKey: TKey; const AValue: TValue);
    function Delete(const AKey: TKey): Boolean;
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    function TryGetPValue(const AKey: TKey; out Value: Pointer): Boolean;
    function Contains(const AKey: TKey): Boolean;
    procedure Clear;

    procedure Reset;
    function Next(out AKey: TKey; out AValue: TValue): Boolean;
    function NextKey(out AKey: TKey): Boolean;
    function NextValue(out AValue: TValue): Boolean;

    property OnDuplicate: IDuplicateResolver read GetOnDuplicate write SetOnDuplicate;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Item[AKey: TKey]: TValue read GetItem write SetItem; default;
    property PItem[AKey: TKey]: Pointer read GetPItem;
  end;

  { IHashSet }

  {$IfDef FPC}generic{$EndIf}
  IHashSet<TKey> = interface
    function GetOnDuplicate: IDuplicateResolver;
    procedure SetOnDuplicate(const AValue: IDuplicateResolver);

    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);

    function Count: Integer;
    function Add(const AKey: TKey): Boolean; overload; //Return True if new item added, False if item already in set
    //procedure Add(const Arr : {$IfDef FPC}specialize{$EndIf} IArray<TKey>); overload;
    procedure AddOrSet(const AKey: TKey);
    function Delete(const AKey: TKey): Boolean;
    function Contains(const AKey: TKey): Boolean;
    procedure Clear;

    function Diff        (const ASet: IHashSet{$IfDef DCC}<TKey>{$EndIf}): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
    function Union       (const ASet: IHashSet{$IfDef DCC}<TKey>{$EndIf}): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
    function Intersection(const ASet: IHashSet{$IfDef DCC}<TKey>{$EndIf}): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
    function SymetricDiff(const ASet: IHashSet{$IfDef DCC}<TKey>{$EndIf}): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;

    procedure Reset;
    function Next(out AKey: TKey): Boolean;

    property OnDuplicate: IDuplicateResolver read GetOnDuplicate write SetOnDuplicate;
    property Capacity: Integer read GetCapacity write SetCapacity;
  end;

  { THashMap }

  {$IfDef FPC}generic{$EndIf}
  THashMap<TKey, TValue> = class (TInterfacedObjectEx, {$IfDef FPC}specialize{$EndIf} IHashMap<TKey, TValue>)
  {$IfDef FPC}
  protected
  {$Else}
  public
  {$EndIf}
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
  protected
    procedure MoveWithAddingGap(var bIndex, curInd: Integer); virtual;
  protected
    FEnumIndex: Integer;
    FData: TItems;
    FGrowLimit: Integer;
    FCount: Integer;

    FComparer: IEqualityComparer;
    FEmptyKey: TKey;
    FEmptyValue: TValue;

    FOnDuplicate: IDuplicateResolver;

    function PrevIndex(const index: Integer): Integer; {$IfNDef NoInline}inline;{$EndIf}
    function Wrap(const index: Cardinal): Integer; {$IfNDef NoInline}inline;{$EndIf}
    function CalcBucketIndex(const AKey: TKey; AHash: Cardinal; out AIndex: Integer): Boolean; {$IfNDef NoInline}inline;{$EndIf}
    procedure DoAddOrSet(BucketIndex, AHash: Cardinal; const AKey: TKey; const AValue: TValue); {$IfNDef NoInline}inline;{$EndIf}
    procedure GrowIfNeeded; {$IfNDef NoInline}inline;{$EndIf}

    function GetOnDuplicate: IDuplicateResolver;
    procedure SetOnDuplicate(const AValue: IDuplicateResolver);

    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer); virtual;
    function GetItem({$IfDef FPC}const{$EndIf} AKey: TKey): TValue;
    function GetPItem({$IfDef FPC}const{$EndIf} AKey: TKey): Pointer;
    procedure SetItem({$IfDef FPC}const{$EndIf} AKey: TKey; const AValue: TValue);

    function Count: Integer;
    function Add(const AKey: TKey; const AValue: TValue): Boolean;
    function AddIfNotContains(const AKey: TKey; const AValue: TValue): Boolean;
    procedure AddOrSet(const AKey: TKey; const AValue: TValue);
    function Delete(const AKey: TKey): Boolean;
    function TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
    function TryGetPValue(const AKey: TKey; out AValue: Pointer): Boolean;
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
    property OnDuplicate: IDuplicateResolver read GetOnDuplicate write SetOnDuplicate;

    constructor Create; overload;
    constructor Create(const AComparer: IEqualityComparer); overload;
  end;

  { THashSet }

  {$IfDef FPC}generic{$EndIf}
  THashSet<TKey> = class (TInterfacedObjectEx, {$IfDef FPC}specialize{$EndIf} IHashSet<TKey>)
  private type
    TMap = {$IfDef FPC}specialize{$EndIf} THashMap<TKey, Boolean>;
    IMap = {$IfDef FPC}specialize{$EndIf} IHashMap<TKey, Boolean>;
    IHSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TKey>;
    THSet = {$IfDef FPC}specialize{$EndIf} THashSet<TKey>;
  strict private
    FHash : IMap;
    FComparer : IEqualityComparer;

    function GetOnDuplicate: IDuplicateResolver;
    procedure SetOnDuplicate(const AValue: IDuplicateResolver);

    function GetCapacity: Integer;
    procedure SetCapacity(const cap: Integer);

    function Count: Integer;
    function Add(const AKey: TKey): Boolean; overload;
    procedure AddOrSet(const AKey: TKey);
    function Delete(const AKey: TKey): Boolean;
    function Contains(const AKey: TKey): Boolean;
    procedure Clear;

    function  Diff         (const ASet: IHSet): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
    function  Union        (const ASet: IHSet): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
    function  Intersection (const ASet: IHSet): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
    function  SymetricDiff (const ASet: IHSet): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;

    procedure Reset;
    function Next(out AKey: TKey): Boolean;

    property Capacity: Integer read GetCapacity write SetCapacity;
  public
    property OnDuplicate: IDuplicateResolver read GetOnDuplicate write SetOnDuplicate;

    constructor Create; overload;
    constructor Create(const AComparer: IEqualityComparer); overload;
  end;

  { IHashMapWithBuckets }

  TOnBucketIndexChange = procedure (const Key, Value; OldBucketIndex, NewBucketIndex: Integer) of object;

  {$IfDef FPC}generic{$EndIf} IHashMapWithBuckets<TKey, TValue> = interface ({$IfDef FPC}specialize{$EndIf} IHashMap<TKey, TValue>)
    function AddOrSetWithBucketIndex(const AKey: TKey; const AValue: TValue): Integer; //return bucket index
    function AddOrGetBucketIndex(const AKey: TKey; const AValue: TValue): Integer; //return bucket index
    function GetPKeyByBucketIndex(ABucketIndex: Integer): Pointer;
    function GetPValueByBucketIndex(ABucketIndex: Integer): Pointer;

    function GetOnBucketIndexChange: TOnBucketIndexChange;
    procedure SetOnBucketIndexChange(const AValue: TOnBucketIndexChange);

    property OnBucketIndexChange: TOnBucketIndexChange read GetOnBucketIndexChange write SetOnBucketIndexChange;
  end;

  { THashMapWithBuckets }

  {$IfDef FPC}generic{$EndIf} THashMapWithBuckets<TKey, TValue> = class ({$IfDef FPC}specialize{$EndIf} THashMap<TKey, TValue>, {$IfDef FPC}specialize{$EndIf} IHashMapWithBuckets<TKey, TValue>)
  protected
    procedure SetCapacity(const cap: Integer); override;
    procedure MoveWithAddingGap(var bIndex, curInd: Integer); override;
  private
    FOnBucketIndexChange: TOnBucketIndexChange;

    function AddOrSetWithBucketIndex(const AKey: TKey; const AValue: TValue): Integer; //return bucket index
    function AddOrGetBucketIndex(const AKey: TKey; const AValue: TValue): Integer; //return bucket index
    function GetPKeyByBucketIndex(ABucketIndex: Integer): Pointer;
    function GetPValueByBucketIndex(ABucketIndex: Integer): Pointer;

    function GetOnBucketIndexChange: TOnBucketIndexChange;
    procedure SetOnBucketIndexChange(const AValue: TOnBucketIndexChange);
  end;

  {$IfDef FPC}generic{$EndIf}
  IBase_LooseTreeNode<TItem, TBox> = interface
    function Level: Integer;
    function Child(const AIndex: Integer): {$IfDef FPC}specialize{$EndIf} IBase_LooseTreeNode<TItem, TBox>;

    function TotalCount: Integer;
    function ItemsCount: Integer;
    function Item(const AIndex: Integer): TItem;

    function UsedMemory: Integer;
  end;

  ILooseNodeCallBackIterator = interface
    procedure OnEnumNode(const ASender: IInterface; const ANode: IInterface; var EnumChilds: Boolean);
  end;

  {$IfDef FPC}generic{$EndIf}
  IBase_LooseTree<TItem, TBox> = interface
    function Add(const AItem: TItem; Const ABox: TBox): Boolean;
    function Contains(const AItem: TItem): Boolean;
    function Delete(const AItem: TItem): Boolean;

    function AABB(const ANode: IInterface; const APlaceArea: Boolean = False): TBox;

    function RootNode: {$IfDef FPC}specialize{$EndIf} IBase_LooseTreeNode<TItem, TBox>;

    procedure EnumNodes(const ACallbackIterator: ILooseNodeCallBackIterator);
    procedure CleanUnused;

    function UsedMemory: Integer;
  end;

  {$IfDef FPC}generic{$EndIf}
  ILooseOctTreeNode<TItem> = interface ({$IfDef FPC}specialize{$EndIf} IBase_LooseTreeNode<TItem, TAABB>)
  end;

  {$IfDef FPC}generic{$EndIf}
  IOctNodeInternal<TItem> = interface ({$IfDef FPC}specialize{$EndIf} ILooseOctTreeNode<TItem>)
    function Place: TVec3i;
    function Parent: {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>;

    function InNode(const AMinBound, AMaxBound: TVec3i): Boolean; overload;
    function InNode(const APoint: TVec3): Boolean; overload;
    function CreateParent(const AMinBound: TVec3): {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>;
    function CalcChildIndex(const AChildPlace: TVec3): Integer; overload;
    function ObtainChild(const AIndex: Integer): {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>;

    procedure AddItem(const AItem: TItem);
    procedure DelItem(const AItem: TItem);

    procedure IncTotalCounter;
    procedure DecTotalCounter;

    procedure ClearParentLink;
  end;

  { TLooseOctTreeNode }

  {$IfDef FPC}generic{$EndIf}
  TLooseOctTreeNode<TItem> = class (TInterfacedObjectEx, {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>)
  private type
    IItems = {$IfDef FPC}specialize{$EndIf} IArray<TItem>;
    TItems = {$IfDef FPC}specialize{$EndIf} TArray<TItem>;

    INode = {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>;
    TNode = {$IfDef FPC}specialize{$EndIf} TLooseOctTreeNode<TItem>;
  private
    FLevel       : Integer;
    FPlace       : TVec3i;  //left bottom of bound
    FParent      : TNode;
    FTotalCounter: Integer;
    FChilds      : array [0..7] of INode;
    FItems       : IItems;
  private
    function UsedMemory: Integer;

    function Level: Integer;
    function Child(const AIndex: Integer): {$IfDef FPC}specialize{$EndIf} IBase_LooseTreeNode<TItem, TAABB>;

    function TotalCount: Integer;
    function ItemsCount: Integer;
    function Item(const AIndex: Integer): TItem;

    function CalcChildIndex(const AParentPlace: TVec3i; const AParentLevel: Integer; const AChildPlace: TVec3): Integer; overload;
    function InNode(const AMinBound, AMaxBound: TVec3i): Boolean; overload;
    function InNode(const APoint: TVec3): Boolean; overload;

    procedure TryDetachFromParent;

    procedure AddItem(const AItem: TItem);
    procedure DelItem(const AItem: TItem);

    procedure IncTotalCounter;
    procedure DecTotalCounter;

    procedure ClearParentLink;

    function Place: TVec3i;
    function Parent: INode;
  public
    function CalcChildIndex(const AChildPlace: TVec3): Integer; overload;
    function ObtainChild(const AIndex: Integer): {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>;
    function CreateParent(const AMinBound: TVec3): {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>;
    constructor Create(const AParent: TNode; const ALevel: Integer; const APlace: TVec3i);
  end;

  {$IfDef FPC}generic{$EndIf}
  ILooseOctTree<TItem> = interface ({$IfDef FPC}specialize{$EndIf} IBase_LooseTree<TItem, TAABB>)
  end;

  { TLooseOctTree }

  {$IfDef FPC}generic{$EndIf}
  TLooseOctTree<TItem> = class (TInterfacedObjectEx, {$IfDef FPC}specialize{$EndIf} ILooseOctTree<TItem>)
  private type
    ITree = {$IfDef FPC}specialize{$EndIf} ILooseOctTree<TItem>;

    IBaseNode = {$IfDef FPC}specialize{$EndIf} IBase_LooseTreeNode<TItem, TAABB>;

    INode = {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>;
    TNode = {$IfDef FPC}specialize{$EndIf} TLooseOctTreeNode<TItem>;

    IItemToNodeHash = {$IfDef FPC}specialize{$EndIf} IHashMap<TItem, INode>;
    TItemToNodeHash = {$IfDef FPC}specialize{$EndIf} THashMap<TItem, INode>;
  private
    FMinNodeSize: TVec3;
    FMinNodeSizeInv: TVec3;
    FRoot: INode;
    FItemToNode: IItemToNodeHash;
  protected
    function UsedMemory: Integer;

    function Add(const AItem: TItem; Const ABox: TAABB): Boolean;
    function Contains(const AItem: TItem): Boolean;
    function Delete(const AItem: TItem): Boolean;

    function AABB(const ANode: IInterface; const APlaceArea: Boolean = False): TAABB;

    function RootNode: {$IfDef FPC}specialize{$EndIf} IBase_LooseTreeNode<TItem, TAABB>;

    procedure EnumNodesRecursive(const ANode: IBaseNode; const ACallbackIterator: ILooseNodeCallBackIterator);
    procedure EnumNodes(const ACallbackIterator: ILooseNodeCallBackIterator);

    function GetNewRoot(out NewRoot: INode): boolean;
    procedure CleanUnused;
  public
    constructor Create(const AMinNodeSize: TVec3);
  end;

procedure StreamAutoWrite(const AStream: TStream; const AData; ATypeInfo: PTypeInfo);
procedure StreamAutoRead (const AStream: TStream; var   AData; ATypeInfo: PTypeInfo);

implementation

uses
  rtlconsts, math;

function IsManagedRecord(td: PTypeData): Boolean;
{$IfDef FPC}
var i : Integer;
    mf: PManagedField;
{$EndIf}
begin
  {$IfDef DCC}
    Result := td^.ManagedFldCount > 0;
  {$Else}
    {$IfDef FPC}
      mf := PManagedField(@td^.ManagedFldCount);
      Inc(PByte(mf), SizeOf(td^.ManagedFldCount));

      Result := False;
      for i := 0 to td^.ManagedFldCount - 1 do
      begin
        case mf^.TypeRef^.Kind of
          tkLString,tkAString,tkWString,tkVariant,tkInterface,tkDynArray,tkUString: Exit(True);
          tkRecord:
            if IsManagedRecord(GetTypeData(mf^.TypeRef)) then Exit(True);
        end;
        Inc(mf);
      end;
    {$Else}
      error
    {$EndIf}
  {$EndIf}
end;

procedure StreamRawAutoWrite(const AStream: TStream; const AData; AType: PTypeInfo);
  procedure WriteManagedRecord();
  var td: PTypeData;
      i, n, offset, size : Integer;
      mf: PManagedField;
  begin
     td := GetTypeData(AType);
     n := td^.ManagedFldCount;
     mf := PManagedField(@td^.ManagedFldCount);
     Inc(PByte(mf), SizeOf(td^.ManagedFldCount));

     offset := 0;
     for i := 0 to n-1 do
     begin
       size := mf^.FldOffset - offset;
       if size > 0 then
       begin
         AStream.WriteBuffer(Pointer(NativeInt(@AData)+offset)^, size);
         Inc(offset, size);
       end;
       {$IfDef FPC}
       StreamRawAutoWrite(AStream, Pointer(NativeInt(@AData)+mf^.FldOffset)^, mf^.TypeRef);
       {$EndIf}
       {$IfDef DCC}
       StreamRawAutoWrite(AStream, Pointer(NativeInt(@AData)+mf^.FldOffset)^, mf^.TypeRef^);
       {$EndIf}
       Inc(offset, SizeOf(Pointer));
       Inc(mf);
     end;
  end;
var type2: PTypeInfo;
    td: PTypeData;
    n, i: Integer;
    pB: PByte;
begin
   td := GetTypeData(AType);
   case AType^.Kind of
    tkInteger,tkChar,tkEnumeration,{$IfDef FPC}tkBool,{$EndIf}tkWChar,tkSet:
      begin
        case td^.OrdType of
          otSByte,otUByte : AStream.WriteBuffer(AData, 1);
          otSWord,otUWord : AStream.WriteBuffer(AData, 2);
          otSLong,otULong : AStream.WriteBuffer(AData, 4);
        else
          raise EContnrsError.Create('Can''t be streamed');
        end;
      end;

    tkFloat:
      begin
        case td^.FloatType of
          ftSingle : AStream.WriteBuffer(AData, SizeOf(Single));
          ftDouble : AStream.WriteBuffer(AData, SizeOf(Double));
        else
          raise EContnrsError.Create('Can''t be streamed');
        end;
      end;

    tkInt64{$IfDef FPC}, tkQWord{$EndIf}: AStream.WriteBuffer(AData, SizeOf(Int64));

    {$IfDef FPC}
    tkAString:
    {$Else}
    tkLString:
    {$EndIf}
      begin
        n := Length(AnsiString(AData));
        AStream.WriteBuffer(n, SizeOf(n));
        if n > 0 then
          AStream.WriteBuffer(AnsiString(AData)[1], n);
      end;
    tkUString:
      begin
        n := Length(UnicodeString(AData));
        AStream.WriteBuffer(n, SizeOf(n));
        if n > 0 then
          AStream.WriteBuffer(UnicodeString(AData)[1], n*2);
      end;
    tkWString:
      begin
        n := Length(WideString(AData));
        AStream.WriteBuffer(n, SizeOf(n));
        if n > 0 then
          AStream.WriteBuffer(WideString(AData)[1], n*2);
      end;

    tkRecord:
     begin
       if IsManagedRecord(GetTypeData(AType)) then
         WriteManagedRecord()
       else
         AStream.WriteBuffer(AData, td^.RecSize);
     end;

    tkArray:
     begin
       if td^.ArrayData.ElType^.Kind in [tkInteger,tkChar,tkEnumeration,{$IfDef FPC}tkBool,tkQWord,{$EndIf}tkWChar,tkSet,tkFloat,tkInt64] then
         AStream.WriteBuffer(AData, td^.ArrayData.Size)
       else
         raise EContnrsError.Create('Not implemented yet');
     end;

    tkDynArray:
     begin
       pB := PByte(AData);
       n := DynArraySize(pB);
       AStream.WriteBuffer(n, SizeOf(n));
       if n > 0 then
         case td^.elType2^.Kind of
           tkInteger,tkChar,tkEnumeration,{$IfDef FPC}tkBool,tkQWord,{$EndIf}tkWChar,tkSet,tkFloat,tkInt64:
             begin
               AStream.WriteBuffer(PInteger(AData)^, td^.elSize * DynArraySize(Pointer(AData)));
             end;

           tkRecord:
             begin
               {$IfDef FPC}
               type2 := td^.elType2;
               {$Else}
               type2 := td^.elType2^;
               {$EndIf}
               if IsManagedRecord(GetTypeData(type2)) then
               begin
                 n := DynArraySize(pB);
                 for i := 0 to n - 1 do
                 begin
                   StreamRawAutoWrite(AStream, pB^, type2);
                   Inc(pB^, td^.elSize);
                 end;
               end
               else
               begin
                 AStream.WriteBuffer(PInteger(AData)^, td^.elSize * DynArraySize(Pointer(AData)));
               end;
             end;
         else
           raise EContnrsError.Create('Not implemented yet');
         end;
     end;
   else
     raise EContnrsError.Create('Can''t be streamed');
   end;
end;

procedure StreamRawAutoRead(const AStream: TStream; var AData; AType: PTypeInfo);
  procedure ReadManagedRecord();
  var td: PTypeData;
      i, n, offset, size : Integer;
      mf: PManagedField;
  begin
     td := GetTypeData(AType);
     n := td^.ManagedFldCount;
     mf := PManagedField(@td^.ManagedFldCount);
     Inc(PByte(mf), SizeOf(td^.ManagedFldCount));

     offset := 0;
     for i := 0 to n-1 do
     begin
       size := mf^.FldOffset - offset;
       if size > 0 then
       begin
         AStream.ReadBuffer(Pointer(NativeInt(@AData)+offset)^, size);
         Inc(offset, size);
       end;
       {$IfDef FPC}
       StreamRawAutoRead(AStream, Pointer(NativeInt(@AData)+mf^.FldOffset)^, mf^.TypeRef);
       {$EndIf}
       {$IfDef DCC}
       StreamRawAutoRead(AStream, Pointer(NativeInt(@AData)+mf^.FldOffset)^, mf^.TypeRef^);
       {$EndIf}
       Inc(offset, SizeOf(Pointer));
       Inc(mf);
     end;
  end;
var type2: PTypeInfo;
    td: PTypeData;
    n, i: Integer;
    nNative: NativeInt;
    pB: PByte;
begin
   td := GetTypeData(AType);
   case AType^.Kind of
    tkInteger,tkChar,tkEnumeration,{$IfDef FPC}tkBool,{$EndIf}tkWChar,tkSet:
      begin
        case td^.OrdType of
          otSByte,otUByte : AStream.ReadBuffer(AData, 1);
          otSWord,otUWord : AStream.ReadBuffer(AData, 2);
          otSLong,otULong : AStream.ReadBuffer(AData, 4);
        else
          raise EContnrsError.Create('Can''t be streamed');
        end;
      end;

    tkFloat:
      begin
        case td^.FloatType of
          ftSingle : AStream.ReadBuffer(AData, SizeOf(Single));
          ftDouble : AStream.ReadBuffer(AData, SizeOf(Double));
        else
          raise EContnrsError.Create('Can''t be streamed');
        end;
      end;

    tkInt64{$IfDef FPC},tkQWord{$EndIf}: AStream.ReadBuffer(AData, SizeOf(Int64));

    {$IfDef FPC}
    tkAString:
    {$Else}
    tkLString:
    {$EndIf}
      begin
        n := 0;
        AStream.ReadBuffer(n, SizeOf(n));
        SetLength(AnsiString(AData), n);
        if n > 0 then
          AStream.ReadBuffer(AnsiString(AData)[1], n);
      end;
    tkUString:
      begin
        n := 0;
        AStream.ReadBuffer(n, SizeOf(n));
        SetLength(UnicodeString(AData), n);
        if n > 0 then
          AStream.ReadBuffer(UnicodeString(AData)[1], n*2);
      end;
    tkWString:
      begin
        n := 0;
        AStream.ReadBuffer(n, SizeOf(n));
        SetLength(WideString(AData), n);
        if n > 0 then
          AStream.ReadBuffer(WideString(AData)[1], n*2);
      end;

    tkRecord:
     begin
       if IsManagedRecord(GetTypeData(AType)) then
         ReadManagedRecord()
       else
         AStream.ReadBuffer(AData, td^.RecSize);
     end;

    tkArray:
     begin
       if td^.ArrayData.ElType^.Kind in [tkInteger,tkChar,tkEnumeration,{$IfDef FPC}tkBool,tkQWord,{$EndIf}tkWChar,tkSet,tkFloat,tkInt64] then
         AStream.ReadBuffer(AData, td^.ArrayData.Size)
       else
         raise EContnrsError.Create('Not implemented yet');
     end;

    tkDynArray:
     begin
       n := 0;
       AStream.ReadBuffer(n, SizeOf(n));
       nNative := n;
       DynArraySetLength(Pointer(AData), AType, 1, @nNative);
       pB := PByte(AData);
       if n > 0 then
         case td^.elType2^.Kind of
           tkInteger,tkChar,tkEnumeration,{$IfDef FPC}tkBool,tkQWord,{$EndIf}tkWChar,tkSet,tkFloat,tkInt64:
             begin
               AStream.ReadBuffer(PInteger(AData)^, td^.elSize * DynArraySize(Pointer(AData)));
             end;

           tkRecord:
             begin
               {$IfDef FPC}
               type2 := td^.elType2;
               {$Else}
               type2 := td^.elType2^;
               {$EndIf}
               if IsManagedRecord(GetTypeData(type2)) then
               begin
                 n := DynArraySize(pB);
                 for i := 0 to n - 1 do
                 begin
                   StreamRawAutoRead(AStream, pB^, type2);
                   Inc(pB^, td^.elSize);
                 end;
               end
               else
               begin
                 AStream.ReadBuffer(PInteger(AData)^, td^.elSize * DynArraySize(Pointer(AData)));
               end;
             end;
         else
           raise EContnrsError.Create('Not implemented yet');
         end;
     end;
   else
     raise EContnrsError.Create('Can''t be streamed');
   end;
end;

procedure StreamAutoWrite(const AStream: TStream; const AData; ATypeInfo: PTypeInfo);
begin
  StreamRawAutoWrite(AStream, AData, ATypeInfo);
end;

procedure StreamAutoRead (const AStream: TStream; var AData; ATypeInfo: PTypeInfo);
begin
  StreamRawAutoRead(AStream, AData, ATypeInfo);
end;

{ TQueue }

{$IFOPT R+}
  {$DEFINE RANGEON}
  {$R-}
{$ENDIF}
function TQueue{$IfDef DCC}<TValue>{$EndIf}.GetCapacity: Integer;
begin
  Result := Length(FData);
end;

procedure TQueue{$IfDef DCC}<TValue>{$EndIf}.SetCapacity(cap: Integer);
var data: TDataArr;
    j, i: Integer;
begin
  Assert(cap >= Count);
  SetLength(data, cap);
  j := 0;
  if Length(FData) > 0 then
  begin
    for i := FSeek to FEnd-1 do
    begin
      data[j] := FData[i mod Length(FData)];
      Inc(j);
    end;
  end;
  FEnd := j;
  FSeek := 0;
  FData := data;
end;

function TQueue{$IfDef DCC}<TValue>{$EndIf}.Count: Integer;
begin
  Result := FEnd - FSeek;
end;

procedure TQueue{$IfDef DCC}<TValue>{$EndIf}.Push(const item: TValue);
var n: Cardinal;
begin
  if Count = Length(FData) then
    Capacity := Max(16, Capacity*2);
  n := Length(FData);
  if FEnd >= n then
    FData[FEnd - n] := item
  else
    FData[FEnd] := item;
  Inc(FEnd);
end;

function TQueue{$IfDef DCC}<TValue>{$EndIf}.Pop: TValue;
begin
  if FSeek = FEnd then
    raise EListError.Create('No items in queue.');

  Result := FData[FSeek];
  FData[FSeek] := Default(TValue);
  Inc(FSeek);
  if FSeek >= Length(FData) then
  begin
    Dec(FSeek, Length(FData));
    Dec(FEnd, Length(FData));
  end;
end;

function TQueue{$IfDef DCC}<TValue>{$EndIf}.Peek: TValue;
begin
  if FSeek = FEnd then
    raise EListError.Create('No items in queue.');
  Result := FData[FSeek];
end;

procedure TQueue{$IfDef DCC}<TValue>{$EndIf}.Clear(const TrimCapacity: Boolean);
var
  i: Integer;
begin
  if TrimCapacity then
    FData := nil
  else
    for i := FSeek to FEnd-1 do
      FData[i mod Length(FData)] := Default(TValue);
  FSeek := 0;
  FEnd := 0;
end;

constructor TQueue{$IfDef DCC}<TValue>{$EndIf}.Create;
begin
  FSeek := 0;
  FEnd := 0;
end;
{$IFDEF RANGEON}
  {$R+}
  {$UNDEF RANGEON}
{$ENDIF}

{ THashMapWithBuckets }

procedure THashMapWithBuckets{$IfDef DCC}<TKey, TValue>{$EndIf}.SetCapacity(const cap: Integer);
var OldItems: {$IfDef DCC}THashMap<TKey, TValue>.{$EndIf}TItems;
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

      if Assigned(FOnBucketIndexChange) then
        FOnBucketIndexChange(OldItems[i].Key, OldItems[i].Value, i, bIndex);
    end;
  FGrowLimit := cap div 2;
end;

procedure THashMapWithBuckets{$IfDef DCC}<TKey, TValue>{$EndIf}.MoveWithAddingGap(var bIndex, curInd: Integer);
var oldInd: Integer;
    oldKey: TKey;
    oldVal: TValue;
begin
  oldInd := bIndex;
  inherited MoveWithAddingGap(bIndex, curInd);
  if Assigned(FOnBucketIndexChange) then FOnBucketIndexChange(FData[oldInd].Key, FData[oldInd].Value, curInd, oldInd);
end;

function THashMapWithBuckets{$IfDef DCC}<TKey, TValue>{$EndIf}.AddOrSetWithBucketIndex(const AKey: TKey; const AValue: TValue): Integer;
var hash: Cardinal;
begin
  GrowIfNeeded;
  hash := FComparer.Hash(AKey);
  CalcBucketIndex(AKey, hash, Result);
  DoAddOrSet(Result, hash, AKey, AValue);
end;

function THashMapWithBuckets{$IfDef DCC}<TKey, TValue>{$EndIf}.AddOrGetBucketIndex(const AKey: TKey; const AValue: TValue): Integer;
var hash: Cardinal;
begin
  GrowIfNeeded;
  hash := FComparer.Hash(AKey);
  if not CalcBucketIndex(AKey, hash, Result) then
    DoAddOrSet(Result, hash, AKey, AValue);
end;

function THashMapWithBuckets{$IfDef DCC}<TKey, TValue>{$EndIf}.GetPKeyByBucketIndex(ABucketIndex: Integer): Pointer;
begin
  Result := @FData[ABucketIndex].Key;
end;

function THashMapWithBuckets{$IfDef DCC}<TKey, TValue>{$EndIf}.GetPValueByBucketIndex(ABucketIndex: Integer): Pointer;
begin
  Result := @FData[ABucketIndex].Value;
end;

function THashMapWithBuckets{$IfDef DCC}<TKey, TValue>{$EndIf}.GetOnBucketIndexChange: TOnBucketIndexChange;
begin
  Result := FOnBucketIndexChange;
end;

procedure THashMapWithBuckets{$IfDef DCC}<TKey, TValue>{$EndIf}.SetOnBucketIndexChange(
  const AValue: TOnBucketIndexChange);
begin
  FOnBucketIndexChange := AValue;
end;

{ TLooseOctTreeNode }

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.UsedMemory: Integer;
var
  i: Integer;
begin
  Result := InstanceSize;
  Inc(Result, FItems.Capacity * SizeOf(TItem));
  for i := 0 to 7 do
    if FChilds[i] <> nil then
      Inc(Result, FChilds[i].UsedMemory);
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.Level: Integer;
begin
  Result := FLevel;
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.Child(const AIndex: Integer): {$IfDef FPC}specialize{$EndIf} IBase_LooseTreeNode<TItem, TAABB>;
begin
  Result := FChilds[AIndex];
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.TotalCount: Integer;
begin
  Result := FTotalCounter;
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.ItemsCount: Integer;
begin
  if FItems = nil then
    Result := 0
  else
    Result := FItems.Count;
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.Item(const AIndex: Integer): TItem;
begin
  Result := FItems[AIndex];
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.CalcChildIndex(const AParentPlace: TVec3i; const AParentLevel: Integer; const AChildPlace: TVec3): Integer;
var offset: Integer;
begin
  Assert(AParentLevel > 0);
  Result := 0;
  offset := 1 shl AParentLevel shr 1;
  if AChildPlace.x >= AParentPlace.x+offset then Result := Result or 1;
  if AChildPlace.y >= AParentPlace.y+offset then Result := Result or 2;
  if AChildPlace.z >= AParentPlace.z+offset then Result := Result or 4;
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.InNode(const AMinBound, AMaxBound: TVec3i): Boolean;
var offset: Integer;
begin
  Result := False;
  if AMinBound.x < FPlace.x then Exit;
  if AMinBound.y < FPlace.y then Exit;
  if AMinBound.z < FPlace.z then Exit;
  offset := 1 shl FLevel;
  if AMaxBound.x >= FPlace.x+offset then Exit;
  if AMaxBound.y >= FPlace.y+offset then Exit;
  if AMaxBound.z >= FPlace.z+offset then Exit;
  Result := True;
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.InNode(const APoint: TVec3): Boolean;
var offset: Integer;
begin
  Result := False;
  if APoint.x < FPlace.x then Exit;
  if APoint.y < FPlace.y then Exit;
  if APoint.z < FPlace.z then Exit;
  offset := 1 shl FLevel;
  if APoint.x >= FPlace.x+offset then Exit;
  if APoint.y >= FPlace.y+offset then Exit;
  if APoint.z >= FPlace.z+offset then Exit;
  Result := True;
end;

procedure TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.TryDetachFromParent;
var selfIntf: INode;
    i: Integer;
begin
  if (FTotalCounter = 0) and Assigned(FParent) then
  begin
    selfIntf := Self;
    for i := 0 to 7 do
      if FParent.FChilds[i] = selfIntf then
        begin
          FParent.FChilds[i] := nil;
          FParent := nil;
          Exit;
        end;
  end;
end;

procedure TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.AddItem(const AItem: TItem);
begin
  FItems.Add(AItem);
  IncTotalCounter;
end;

procedure TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.DelItem(const AItem: TItem);
begin
  DecTotalCounter;
  FItems.Delete(FItems.IndexOf(AItem));
end;

procedure TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.IncTotalCounter;
begin
  Inc(FTotalCounter);
end;

procedure TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.DecTotalCounter;
begin
  Dec(FTotalCounter);
  TryDetachFromParent;
end;

procedure TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.ClearParentLink;
begin
  FParent := nil;
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.Place: TVec3i;
begin
  Result := FPlace;
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.Parent: INode;
begin
  Result := FParent;
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.CalcChildIndex(const AChildPlace: TVec3): Integer;
begin
  Result := CalcChildIndex(FPlace, FLevel, AChildPlace);
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.ObtainChild(const AIndex: Integer): {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>;
var childPlace: TVec3i;
    offset: Integer;
begin
  Assert(FLevel <> 0);
  Result := FChilds[AIndex];
  if Result <> nil then Exit;
  Assert(FChilds[AIndex] = nil);

  offset := 1 shl FLevel shr 1;
  childPlace.x := FPlace.x + (AIndex and 1)*offset;
  childPlace.y := FPlace.y + ((AIndex and 2) shr 1)*offset;
  childPlace.z := FPlace.z + ((AIndex and 4) shr 2)*offset;

  FChilds[AIndex] := TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.Create(Self, FLevel - 1, childPlace);
  Result := FChilds[AIndex];
end;

function TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.CreateParent(const AMinBound: TVec3): {$IfDef FPC}specialize{$EndIf} IOctNodeInternal<TItem>;
var parentLevel: Integer;
    parentPlace: TVec3i;
    LevelSize: Integer;
begin
  Assert(FParent = Nil);

  LevelSize := 1 shl FLevel;
  parentPlace := FPlace;
  if AMinBound.x < FPlace.x then parentPlace.x := parentPlace.x - LevelSize;
  if AMinBound.y < FPlace.y then parentPlace.y := parentPlace.y - LevelSize;
  if AMinBound.z < FPlace.z then parentPlace.z := parentPlace.z - LevelSize;
  parentLevel := FLevel + 1;

  FParent := TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.Create(nil, parentLevel, parentPlace);
  FParent.FTotalCounter := FTotalCounter;
  Result := FParent;
  FParent.FChilds[CalcChildIndex(parentPlace, parentLevel, FPlace)] := Self;
end;

constructor TLooseOctTreeNode{$IfDef DCC}<TItem>{$EndIf}.Create(const AParent: TNode; const ALevel: Integer; const APlace: TVec3i);
begin
  FParent := AParent;
  FPlace := APlace;
  FLevel := ALevel;
  FItems := TItems.Create();
  FTotalCounter := 0;
end;

{ TLooseOctTree }
function TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.UsedMemory: Integer;
begin
  Result := InstanceSize;
  Inc(Result, FItemToNode.Capacity*(SizeOf(TItem)+SizeOf(Pointer)+SizeOf(Integer)) );
  Inc(Result, FRoot.UsedMemory);
end;

function TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.Add(const AItem: TItem; const ABox: TAABB): Boolean;
//{$Define TreeCheck}
var KMin, KMax: TVec3i;
    Level, LevelSize: Integer;
    Place: TVec3;
    node: INode;
    nodeBox: TAABB;
    k: Single;
    v: TVec3;
    size1, size2: Single;
begin
  Delete(AItem);
  Place := ABox.Center * FMinNodeSizeInv * 2;
  SetRoundMode(rmUp);
  KMax := mutils.Ceil( (ABox.max - ABox.min) * FMinNodeSizeInv )*2;
  Level := Max(Max(Log2Int(Integer(KMax.x-1)), Log2Int(Integer(KMax.y-1))), Log2Int(Integer(KMax.z-1))) + 1;
  {$IfDef TreeCheck}
  Assert(Level < 1000);
  {$EndIf}
  KMax  := mutils.Ceil (ABox.max * FMinNodeSizeInv * 2);
  SetRoundMode(rmDown);
  KMin  := mutils.Floor(ABox.min * FMinNodeSizeInv * 2);
  SetRoundMode(rmNearest);

  {$ifdef TreeCheck}
  SetRoundMode(rmDown);
  v := KMin * FMinNodeSize * 0.5;
  Assert(v.x <= ABox.min.x);
  Assert(v.y <= ABox.min.y);
  Assert(v.z <= ABox.min.z);
  SetRoundMode(rmUp);
  v := KMax * FMinNodeSize * 0.5;
  Assert(v.x >= ABox.max.x);
  Assert(v.y >= ABox.max.y);
  Assert(v.z >= ABox.max.z);
  SetRoundMode(rmNearest);
  {$EndIf}

  if FRoot = nil then
    FRoot := TNode.Create(nil, Level, KMin);

  while (not FRoot.InNode(Place)) or (FRoot.Level < Level) do
    FRoot := INode(FRoot.CreateParent(Place));

  node := FRoot;
  while node.Level <> Level do
  begin
    node.IncTotalCounter;
    node := INode(node.ObtainChild(node.CalcChildIndex(Place)));
  end;

  {$IfDef TreeCheck}
  nodeBox := AABB(node, false);
  if ABox.min.x < nodeBox.min.x then raise Exception.Create('bad');
  if ABox.min.y < nodeBox.min.y then raise Exception.Create('bad');
  if ABox.min.z < nodeBox.min.z then raise Exception.Create('bad');
  if ABox.max.x > nodeBox.max.x then raise Exception.Create('bad');
  if ABox.max.y > nodeBox.max.y then raise Exception.Create('bad');
  if ABox.max.z > nodeBox.max.z then raise Exception.Create('bad');

  v := AABB(node, true).Size;
  size1 := Max(Max(v.x, v.y), v.z);
  v := ABox.Size;
  size2 := Max(Max(v.x, v.y), v.z);
  k := size1/size2;
  If (level > 1) and (k > 2) Then raise Exception.Create('bad');
  {$EndIf}

  node.AddItem(AItem);
  FItemToNode.Add(AItem, node);

  Result := True;
end;

function TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.Contains(const AItem: TItem): Boolean;
begin
  Result := FItemToNode.Contains(AItem);
end;

function TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.Delete(const AItem: TItem): Boolean;
var node, parent, nextParent: INode;
begin
  Result := False;
  if FItemToNode.TryGetValue(AItem, node) then
  begin
    parent := node.Parent;
    node.DelItem(AItem);
    FItemToNode.Delete(AItem);
    while parent <> nil do
    begin
      nextParent := parent.Parent;
      parent.DecTotalCounter;
      parent := nextParent;
    end;

    CleanUnused;
    Result := True;
  end;
end;

function TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.AABB(const ANode: IInterface; const APlaceArea: Boolean): TAABB;
var size: Integer;
    offset: Single;
begin
  Result.min := INode(ANode).Place;
  size := 1 shl INode(ANode).Level;
  if not APlaceArea then
  begin
    offset := -size * 0.5;
    Result.min.x := Result.min.x + offset;
    Result.min.y := Result.min.y + offset;
    Result.min.z := Result.min.z + offset;
    size := size shl 1;
  end;
  Result.max.x := Result.min.x + size;
  Result.max.y := Result.min.y + size;
  Result.max.z := Result.min.z + size;

  Result.min := Result.min * FMinNodeSize * 0.5;
  Result.max := Result.max * FMinNodeSize * 0.5;
end;

function TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.RootNode: {$IfDef FPC}specialize{$EndIf} IBase_LooseTreeNode<TItem, TAABB>;
begin
  Result := FRoot;
end;

procedure TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.EnumNodesRecursive(const ANode: IBaseNode; const ACallbackIterator: ILooseNodeCallBackIterator);
var EnumChilds: Boolean;
    Child: IBaseNode;
    i: Integer;
    sender: ITree;
begin
  EnumChilds := True;

  sender := Self;
  ACallbackIterator.OnEnumNode(sender, ANode, EnumChilds);
  if EnumChilds then
    for i := 0 to 7 do
    begin
      Child := ANode.Child(i);
      if Assigned(Child) then
        EnumNodesRecursive(Child, ACallbackIterator);
    end;
end;

procedure TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.EnumNodes(const ACallbackIterator: ILooseNodeCallBackIterator);
begin
  if FRoot = nil then Exit;
  EnumNodesRecursive(FRoot, ACallbackIterator);
end;

function TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.GetNewRoot(out NewRoot: INode): boolean;
var i: Integer;
    chld: INode;
begin
  NewRoot := nil;
  for i := 0 to 7 do
  begin
    chld := INode(FRoot.Child(i));
    if (chld <> nil) then
    begin
      if NewRoot <> nil then Exit(False);
      NewRoot := chld;
    end;
  end;
  Result := NewRoot <> nil;
end;

procedure TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.CleanUnused;
var newRoot: INode;
begin
  if FRoot = nil then Exit;
  if FRoot.TotalCount = 0 then
  begin
    FRoot := nil;
    Exit;
  end;

  while True do
  begin
    if not GetNewRoot(newRoot) then Break;
    FRoot := newRoot;
    FRoot.ClearParentLink;
    if FRoot.Level = 0 then Break;
  end;
end;

constructor TLooseOctTree{$IfDef DCC}<TItem>{$EndIf}.Create(const AMinNodeSize: TVec3);
begin
  FMinNodeSize := AMinNodeSize;
  FMinNodeSizeInv.x := 1.0/FMinNodeSize.x;
  FMinNodeSizeInv.y := 1.0/FMinNodeSize.y;
  FMinNodeSizeInv.z := 1.0/FMinNodeSize.z;
  FItemToNode := TItemToNodeHash.Create;
end;

procedure THeap{$IfDef DCC}<T>{$EndIf}.Grow;
begin
  SetLength(FData, NextPow2(FCount + 1));
end;

function THeap{$IfDef DCC}<T>{$EndIf}.GetOnDuplicate: IDuplicateResolver;
begin
  Result := FOnDuplicate;
end;

procedure THeap{$IfDef DCC}<T>{$EndIf}.Trim;
begin
  SetLength(FData, FCount);
end;

procedure THeap{$IfDef DCC}<T>{$EndIf}.Clear(const NewCapacity: Integer);
begin
  FData := nil;
  FCount := 0;
  SetLength(FData, NewCapacity);
end;

function THeap{$IfDef DCC}<T>{$EndIf}.GetMinIndex(const i1, i2: Integer): Integer;
begin
  if FComparer.Compare(FData[i1], FData[i2]) < 0 then
    Result := i1
  else
    Result := i2;
end;

procedure THeap{$IfDef DCC}<T>{$EndIf}.SetOnDuplicate(const AValue: IDuplicateResolver);
begin
  if FOnDuplicate = AValue then Exit;
  FOnDuplicate := AValue;
end;

procedure THeap{$IfDef DCC}<T>{$EndIf}.Swap(const i1, i2: Integer);
var tmp: T;
begin
  tmp := FData[i2];
  FData[i2] := FData[i1];
  FData[i1] := tmp;
end;

procedure THeap{$IfDef DCC}<T>{$EndIf}.SiftDown(Index: Integer);
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

procedure THeap{$IfDef DCC}<T>{$EndIf}.SiftUp(Index: Integer);
var parentIdx: Integer;
    cmpResult: Integer;
begin
  while True do
  begin
    if Index = 0 then Exit; // at Root
    parentIdx := (Index - 1) div 2;

    cmpResult := FComparer.Compare(FData[parentIdx], FData[Index]);
    if cmpResult = 0 then
    begin
      if FOnDuplicate = nil then Exit;
      case FOnDuplicate.DuplicateResolve(FData[Index], FData[parentIdx]) of
        dupAddNew: Exit;
        dupOverwrite: FData[parentIdx] := FData[Index];
        dupSkip: ;
      end;
      FData[Index] := FData[FCount - 1];
      Dec(FCount);
      SiftDown(Index);
      Exit;
    end;
    if cmpResult < 0 then Exit; // sift completed
    Swap(parentIdx, Index);
    Index := parentIdx;
  end;
end;

procedure THeap{$IfDef DCC}<T>{$EndIf}.AutoSelectComparer;
begin
  if FComparer = nil then
    FComparer := avContnrsDefaults.AutoSelectComparer(TypeInfo(T), SizeOf(T));
end;

function THeap{$IfDef DCC}<T>{$EndIf}.Count: Integer;
begin
  Result := FCount;
end;

function THeap{$IfDef DCC}<T>{$EndIf}.PeekTop: T;
begin
  if FCount = 0 then raise ERangeError.Create('Index 0 out of bound');
  Result := FData[0];
end;

function THeap{$IfDef DCC}<T>{$EndIf}.ExtractTop: T;
begin
  if FCount = 0 then raise ERangeError.Create('Index 0 out of bound');
  Result := FData[0];
  FData[0] := FData[FCount - 1];
  FData[FCount - 1] := Default(T);
  Dec(FCount);
  SiftDown(0);

  if Length(FData) div 4 >= FCount then Trim;
end;

function THeap{$IfDef DCC}<T>{$EndIf}.ExtractTop_NoClean_NoTrim: T;
begin
  if FCount = 0 then raise ERangeError.Create('Index 0 out of bound');
  Result := FData[0];
  Swap(0, FCount - 1);
  Dec(FCount);
  SiftDown(0);
end;

procedure THeap{$IfDef DCC}<T>{$EndIf}.Insert(const Value: T);
begin
  if FCount = Length(FData) then Grow;
  FData[FCount] := Value;
  Inc(FCount);
  SiftUp(FCount - 1);
end;

procedure THeap{$IfDef DCC}<T>{$EndIf}.Reset;
begin
  FEnumIndex := -1;
end;

function THeap{$IfDef DCC}<T>{$EndIf}.Next(out AItem: T): Boolean;
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

constructor THeap{$IfDef DCC}<T>{$EndIf}.Create(const ArrData: TData; const ACount: Integer; const AComparer: IComparer);
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

constructor THeap{$IfDef DCC}<T>{$EndIf}.Create(const ACapacity: Integer);
begin
  Create(ACapacity, nil);
end;

constructor THeap{$IfDef DCC}<T>{$EndIf}.Create(const ACapacity: Integer; const AComparer: IComparer);
begin
  SetLength(FData, ACapacity);
  FComparer := AComparer;
  AutoSelectComparer;
  FCount := 0;
end;

{ THashSet }

function THashSet{$IfDef DCC}<TKey>{$EndIf}.GetOnDuplicate: IDuplicateResolver;
begin
  Result := FHash.OnDuplicate;
end;

procedure THashSet{$IfDef DCC}<TKey>{$EndIf}.SetOnDuplicate(const AValue: IDuplicateResolver);
begin
  FHash.OnDuplicate := AValue;
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.GetCapacity: Integer;
begin
  Result := FHash.Capacity;
end;

procedure THashSet{$IfDef DCC}<TKey>{$EndIf}.SetCapacity(const cap: Integer);
begin
  FHash.Capacity := cap;
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.Count: Integer;
begin
  Result := FHash.Count;
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.Add(const AKey: TKey): Boolean;
begin
  Result := FHash.AddIfNotContains(AKey, False);
end;

procedure THashSet{$IfDef DCC}<TKey>{$EndIf}.AddOrSet(const AKey: TKey);
begin
 FHash.AddOrSet(AKey, False);
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.Delete(const AKey: TKey): Boolean;
begin
 Result := FHash.Delete(AKey);
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.Contains(const AKey: TKey): Boolean;
begin
 Result := FHash.Contains(AKey);
end;

procedure THashSet{$IfDef DCC}<TKey>{$EndIf}.Clear;
begin
  FHash.Clear;
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.Diff(const ASet: IHSet): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
var k: TKey;
begin
  Result := THSet.Create(FComparer);
  Reset;
  while Next(k) do
    if not ASet.Contains(k) then
      Result.Add(k);
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.Union(const ASet: IHSet): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
var k: TKey;
begin
  Result := THSet.Create(FComparer);
  Result.Capacity := Capacity + ASet.Capacity;
  Reset;
  while Next(k) do Result.Add(k);
  ASet.Reset;
  while Next(k) do Result.Add(k);
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.Intersection(const ASet: IHSet): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
var k: TKey;
begin
  Result := THSet.Create(FComparer);
  Reset;
  while Next(k) do
    if ASet.Contains(k) then
      Result.Add(k);
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.SymetricDiff(const ASet: IHSet): {$IfDef FPC}specialize{$EndIf}IHashSet<TKey>;
var k: TKey;
begin
  Result := THSet.Create(FComparer);
  Reset;
  while Next(k) do
    if not ASet.Contains(k) then
      Result.Add(k);
  ASet.Reset;
  while ASet.Next(k) do
    if not Contains(k) then
      Result.Add(k);
end;

procedure THashSet{$IfDef DCC}<TKey>{$EndIf}.Reset;
begin
  FHash.Reset;
end;

function THashSet{$IfDef DCC}<TKey>{$EndIf}.Next(out AKey: TKey): Boolean;
begin
  Result := FHash.NextKey(AKey);
end;

constructor THashSet{$IfDef DCC}<TKey>{$EndIf}.Create;
begin
  Create(nil);
end;

constructor THashSet{$IfDef DCC}<TKey>{$EndIf}.Create(const AComparer: IEqualityComparer);
begin
  FComparer := AComparer;
  FHash := TMap.Create(AComparer);
end;

{ THashMap }

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.PrevIndex(const index: Integer): Integer;
begin
  Result := Wrap(index-1+Length(FData));
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Wrap(const index: Cardinal): Integer;
begin
  Result := index Mod Length(FData);
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.CalcBucketIndex(const AKey: TKey; AHash: Cardinal; out AIndex: Integer): Boolean;
begin
  if Length(FData) = 0 then
  begin
    AIndex := 0;
    Exit(False);
  end;
  AIndex := Wrap(AHash);
  while True do
  begin
    if FData[AIndex].Hash = EMPTY_HASH then Exit(False);
    if (FData[AIndex].Hash = AHash) And FComparer.IsEqual(FData[AIndex].Key, AKey) then Exit(True);
    Inc(AIndex);
    if AIndex = Length(FData) then AIndex := 0;
  end;
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.DoAddOrSet(BucketIndex, AHash: Cardinal; const AKey: TKey;
  const AValue: TValue);
begin
  if FData[BucketIndex].Hash = EMPTY_HASH then Inc(FCount);
  FData[BucketIndex].Hash := AHash;
  FData[BucketIndex].Key := AKey;
  FData[BucketIndex].Value := AValue;
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.GrowIfNeeded;
begin
  if FCount >= FGrowLimit then
    Capacity := Max(4, Capacity) * 2;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.GetOnDuplicate: IDuplicateResolver;
begin
  Result := FOnDuplicate;
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.SetOnDuplicate(const AValue: IDuplicateResolver);
begin
  if FOnDuplicate = AValue then Exit;
  FOnDuplicate := AValue;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.GetCapacity: Integer;
begin
  Result := Length(FData);
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.SetCapacity(const cap: Integer);
var OldItems: TItems;
    i, bIndex: Integer;
begin
  OldItems := FData;
  FData := nil;
  FCount := 0;
  SetLength(FData, cap);
  for i := 0 to Length(OldItems)-1 do
    if OldItems[i].Hash <> EMPTY_HASH then
    begin
      CalcBucketIndex(OldItems[i].Key, OldItems[i].Hash, bIndex);
      DoAddOrSet(bIndex, OldItems[i].Hash, OldItems[i].Key, OldItems[i].Value);
    end;
  FGrowLimit := cap div 2;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.GetItem({$IfDef FPC}const{$EndIf} AKey: TKey): TValue;
var bIndex: Integer;
begin
  bIndex := 0;
  if not CalcBucketIndex(AKey, FComparer.Hash(AKey), bIndex) then
    raise EContnrsError.CreateFmt(SItemNotFound, [bIndex]);
  Result := FData[bIndex].Value;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.GetPItem({$IfDef FPC}const{$EndIf} AKey: TKey): Pointer;
var bIndex: Integer;
begin
  bIndex := 0;
  if not CalcBucketIndex(AKey, FComparer.Hash(AKey), bIndex) then
    raise EContnrsError.CreateFmt(SItemNotFound, [bIndex]);
  Result := @FData[bIndex].Value;
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.SetItem({$IfDef FPC}const{$EndIf} AKey: TKey; const AValue: TValue);
var bIndex: Integer;
begin
  bIndex := 0;
  if not CalcBucketIndex(AKey, FComparer.Hash(AKey), bIndex) then
    raise EContnrsError.CreateFmt(SItemNotFound, [bIndex]);
  FData[bIndex].Value := AValue;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Count: Integer;
begin
  Result := FCount;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Add(const AKey: TKey; const AValue: TValue): Boolean;
var hash: Cardinal;
    bIndex: Integer;
begin
  bIndex := 0;
  Result := False;
  GrowIfNeeded;
  hash := FComparer.Hash(AKey);

  if CalcBucketIndex(AKey, hash, bIndex) then
  begin
    if FOnDuplicate = nil then
      raise EListError.CreateFmt(SDuplicateItem, [bIndex])
    else
      case FOnDuplicate.DuplicateResolve(FData[bIndex].Key, AKey) of
        dupAddNew: raise EListError.CreateFmt(SDuplicateItem, [bIndex]);
        dupOverwrite: ;
        dupSkip: Exit;
      end;
  end;

  DoAddOrSet(bIndex, hash, AKey, AValue);
  Result := True;
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.AddOrSet(const AKey: TKey; const AValue: TValue);
var hash: Cardinal;
    bIndex: Integer;
begin
  GrowIfNeeded;
  hash := FComparer.Hash(AKey);
  CalcBucketIndex(AKey, hash, bIndex);
  DoAddOrSet(bIndex, hash, AKey, AValue);
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.AddIfNotContains(const AKey: TKey; const AValue: TValue): Boolean;
var hash: Cardinal;
    bIndex: Integer;
begin
  bIndex := 0;
  Result := False;
  GrowIfNeeded;
  hash := FComparer.Hash(AKey);

  if CalcBucketIndex(AKey, hash, bIndex) then
    Exit;

  DoAddOrSet(bIndex, hash, AKey, AValue);
  Result := True;
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.MoveWithAddingGap(var bIndex, curInd: Integer);
begin
  FData[bIndex] := FData[curInd];
  bIndex := curInd;
  FData[bIndex].Hash := EMPTY_HASH;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Delete(const AKey: TKey): Boolean;
var hash: Cardinal;
    bIndex, curInd: Integer;
begin
  Result := False;
  bIndex := 0;
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
      MoveWithAddingGap(bIndex, curInd);
  end;
  Dec(FCount);
  FData[bIndex].Hash := EMPTY_HASH;
  FData[bIndex].Key := FEmptyKey;
  FData[bIndex].Value := FEmptyValue;
  Result := True;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.TryGetValue(const AKey: TKey; out AValue: TValue): Boolean;
var bIndex: Integer;
    hash: Cardinal;
begin
  bIndex := 0;
  hash := FComparer.Hash(AKey);
  if not CalcBucketIndex(AKey, hash, bIndex) then
    Exit(False);
  Result := True;
  AValue := FData[bIndex].Value;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.TryGetPValue(const AKey: TKey; out AValue: Pointer): Boolean;
var bIndex: Integer;
    hash: Cardinal;
begin
  bIndex := 0;
  hash := FComparer.Hash(AKey);
  if not CalcBucketIndex(AKey, hash, bIndex) then
  begin
    AValue := nil;
    Exit(False);
  end;
  Result := True;
  AValue := @FData[bIndex].Value;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Contains(const AKey: TKey): Boolean;
var bIndex: Integer;
begin
  bIndex := 0;
  Result := CalcBucketIndex(AKey, FComparer.Hash(AKey), bIndex);
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Clear;
begin
  FData := nil;
  FGrowLimit := 0;
  FCount := 0;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.WrapedIndexIsBetween(Left, Index, Right: Integer): Boolean;
begin
  if Left < Right then
    Result := (Left < Index) and (Index <= Right) // not wrapped range case
  else
    Result := (Index > Left) or (Index <= Right); // range is wraped, check outside
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Reset;
begin
  FEnumIndex := -1;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Next(out AKey: TKey; out AValue: TValue): Boolean;
begin
  Inc(FEnumIndex);
  while FEnumIndex < Length(FData) do
  begin
    if FData[FEnumIndex].Hash <> EMPTY_HASH then
    begin
      AKey   := FData[FEnumIndex].Key;
      AValue := FData[FEnumIndex].Value;
      Exit(True);
    end;
    Inc(FEnumIndex);
  end;
  Result := False;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.NextKey(out AKey: TKey): Boolean;
begin
 Inc(FEnumIndex);
 while FEnumIndex < Length(FData) do
 begin
   if FData[FEnumIndex].Hash <> EMPTY_HASH then
   begin
     AKey   := FData[FEnumIndex].Key;
     Exit(True);
   end;
   Inc(FEnumIndex);
 end;
 Result := False;
end;

function THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.NextValue(out AValue: TValue): Boolean;
begin
 Inc(FEnumIndex);
 while FEnumIndex < Length(FData) do
 begin
   if FData[FEnumIndex].Hash <> EMPTY_HASH then
   begin
     AValue := FData[FEnumIndex].Value;
     Exit(True);
   end;
   Inc(FEnumIndex);
 end;
 Result := False;
end;

procedure THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.AutoSelectComparer;
begin
  if FComparer = nil then
    FComparer := AutoSelectEqualityComparer(TypeInfo(TKey), SizeOf(TKey));
end;

constructor THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Create;
begin
  Create(nil);
end;

constructor THashMap{$IfDef DCC}<TKey, TValue>{$EndIf}.Create(const AComparer: IEqualityComparer);
begin
  FComparer := AComparer;
  AutoSelectComparer;
  FEmptyKey := Default(TKey);
  FEmptyValue := Default(TValue);
end;

{ TArray }

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.QuickSort(L, R: Longint; const comparator: IComparer);
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

function TArray{$IfDef DCC}<TValue>{$EndIf}.GetCapacity: Integer;
begin
  Result := Length(FData);
end;

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.SetCapacity(const cap: Integer);
begin
  if cap <> Length(FData) then
    SetLength(FData, cap);
end;

function TArray{$IfDef DCC}<TValue>{$EndIf}.GetItem(index: Integer): TValue;
begin
  Assert(index >= 0);
  Result := FData[index];
end;

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.SetItem(index: Integer; const AValue: TValue);
begin
  FData[index] := AValue;
end;

function TArray{$IfDef DCC}<TValue>{$EndIf}.GetPItem(index: Integer): Pointer;
begin
  Assert(index >= 0);
  Result := @FData[index];
end;

function TArray{$IfDef DCC}<TValue>{$EndIf}.Count: Integer;
begin
  Result := FCount;
end;

function TArray{$IfDef DCC}<TValue>{$EndIf}.Add(const item: TValue): Integer;
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

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.AddArray(const arr: IArr);
var i: Integer;
begin
  if Capacity < FCount + arr.Count then
    Capacity := NextPow2(FCount + arr.Count);
  for i := 0 to arr.Count - 1 do
    FData[FCount+i] := arr[i];
  Inc(FCount, arr.Count);
end;

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.Delete(const index: Integer);
var I: Integer;
begin
  for I := index to FCount - 2 do
    FData[I] := FData[I+1];
  Dec(FCount);
  if FCleanWithEmpty then
    FData[FCount] := FEmpty;
end;

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.DeleteWithSwap(const index: Integer);
var last: Integer;
begin
  last := Count - 1;
  if index <> last then
    FData[index] := FData[last];
  Dec(FCount);
  if FCleanWithEmpty then
    FData[last] := FEmpty;
end;

function TArray{$IfDef DCC}<TValue>{$EndIf}.IndexOf(const item: TValue): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 0 to FCount - 1 do
    if FComparator.Compare(FData[I], item)=0 then
      Exit(I);
end;

function TArray{$IfDef DCC}<TValue>{$EndIf}.Sub(const arr: IArr): {$IfDef FPC}specialize{$EndIf}IArray<TValue>;
var i: Integer;
begin
  Result := TArr.Create(FComparator);
  for i := 0 to Count - 1 do
    if arr.IndexOf(FData[i]) < 0 then Result.Add(FData[i]);
end;

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.Swap(const I1, I2: Integer);
var tmp: TValue;
begin
  tmp := FData[I1];
  FData[I1] := FData[I2];
  FData[I2] := tmp;
end;

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.Clear(const TrimCapacity: Boolean);
var I: Integer;
begin
  if FCleanWithEmpty then
    for I := 0 to FCount - 1 do
      FData[I] := FEmpty;
  FCount := 0;
  if TrimCapacity then
    SetLength(FData, 0);
end;

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.Sort(const comparator: IComparer);
begin
  if FCount < 2 then Exit;
  if comparator = nil then
    QuickSort(0, FCount-1, FComparator)
  else
    QuickSort(0, FCount-1, comparator);
end;

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.HeapSort(const comparator: IComparer);
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

procedure TArray{$IfDef DCC}<TValue>{$EndIf}.AutoSelectComparer;
begin
  if FComparator = nil then
    FComparator := avContnrsDefaults.AutoSelectComparer(TypeInfo(TValue), SizeOf(TValue));
end;

constructor TArray{$IfDef DCC}<TValue>{$EndIf}.Create(const AComparator: IComparer = nil);
begin
  FCleanWithEmpty := IsAutoReferenceCounterType(TypeInfo(TValue));
  if FCleanWithEmpty then FEmpty := Default(TValue);
  FComparator := AComparator;
  AutoSelectComparer;

  //FItemComparer := GetCompareMethod(TypeInfo(TValue));
end;

{$UNDEF IMPLEMENTATION}
end.
