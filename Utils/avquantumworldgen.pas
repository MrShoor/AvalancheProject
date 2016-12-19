unit avQuantumWorldGen;

{$I avConfig.inc}

interface

uses
  Classes, SysUtils, avContnrs, avContnrsDefaults, Math, avTypes;

type
  TTilesSPosition = array of Integer;

  { TSuperPosition }

  TSuperPosition = packed record
    count: Integer; //-1 = any value; -2 = no value; count>=0 = superposition counts or single state
    data : TBoolArr; //nil = single state or spec values, <>nil = superposition
    function InSuperPosition: Boolean; inline;
    function InSingleState: Boolean; inline;
    function InAnyPosition: Boolean; inline;
    function InNoPosition: Boolean; inline;
  end;

const
  AnySuperPosition: TSuperPosition = (count: -1; data: nil);
  NoSuperPosition : TSuperPosition = (count: -2; data: nil);

type
  {$IfDef FPC}generic{$EndIf} IQMap<TNode> = interface
    function TilesCount      : Integer;
    function DirectionsCount : Integer;
    function GetPossibleTiles(const ADirection, FromTile: Integer): TSuperPosition;

    function GetQNeighbour(const ADirection: Integer; const ANode: TNode; out ANeighbour: TNode): Boolean;

    function NodeComparer: IEqualityComparer;
  end;

  {$IfDef FPC}generic{$EndIf} IQGen<TNode> = interface
    function GetOnReduce: TNotifyEvent;
    procedure SetOnReduce(const AValue: TNotifyEvent);

    procedure Resolve(const ANodes: array of TNode);
    function  Get(const ANode: TNode) : TSuperPosition;
    procedure Reduce(const ANode: TNode; const ATiles: TSuperPosition); overload;

    property OnReduce: TNotifyEvent read GetOnReduce write SetOnReduce;
  end;


  { TQGen }

  {$IfDef FPC}generic{$EndIf} TQGen<TNode> = class(TInterfacedObjectEx, {$IfDef FPC}specialize{$EndIf} IQGen<TNode>)
  private type
    IMap = {$IfDef FPC}specialize{$EndIf} IQMap<TNode>;

    IOpenHeap = {$IfDef FPC}specialize{$EndIf} IArray<Integer>;
    TOpenHeap = {$IfDef FPC}specialize{$EndIf} TArray<Integer>;

    TWaveFrontInfo = packed record
      HeapIndex: Integer;
      SP : TSuperPosition;
    end;
    PWaveFrontInfo = ^TWaveFrontInfo;

    IWaveFront = {$IfDef FPC}specialize{$EndIf} IHashMapWithBuckets<TNode, TWaveFrontInfo>;
    TWaveFront = {$IfDef FPC}specialize{$EndIf} THashMapWithBuckets<TNode, TWaveFrontInfo>;
  private
    FMap: IMap;
    FMaxTilesCount  : Integer;
    FDirectionsCount: Integer;
    FIndexToSP : array of array of TSuperPosition; //[direction][fromtile]

    FFront: IWaveFront;
    FOpenHeap: IOpenHeap;

    FOnReduce: TNotifyEvent;

    function GetOnReduce: TNotifyEvent;
    procedure SetOnReduce(const AValue: TNotifyEvent);

    //heap stuff
    function StatesCountInHeap(I: Integer): Integer;
    procedure Swap(I1, I2: Integer);
    procedure SiftDown(Index: Integer);
    procedure SiftUp(Index: Integer);
    procedure ExtractTop(out ANode: TNode; out ASP: TSuperPosition);
    procedure BucketIndexChange(const Key, Value; OldBucketIndex, NewBucketIndex: Integer);
    //end heap stuff

    function ExtendSuperPosition(const Direction: Integer; const ASP: TSuperPosition): TSuperPosition;
    function IntersectSuperPosition(const SP1, SP2: TSuperPosition): TSuperPosition;
    function IsLessSuperPosition(const Left, Right: TSuperPosition): Boolean;
    function StatesCount(const SP: TSuperPosition): Integer;

    function Collapse(const ANode: TNode; const SP: TSuperPosition): TSuperPosition;
  public
    procedure Resolve(const ANodes: array of TNode);
    function Get(const ANode: TNode) : TSuperPosition;
    procedure Reduce(const ANode: TNode; const ATiles: TSuperPosition); overload;

    property OnReduce: TNotifyEvent read GetOnReduce write SetOnReduce;

    constructor Create(const AMap: IMap);
  end;

implementation

{ TSuperPosition }

function TSuperPosition.InSuperPosition: Boolean;
begin
  Result := (count >= 0) and (data <> nil);
end;

function TSuperPosition.InSingleState: Boolean;
begin
  Result := (count >= 0) and (data = nil);
end;

function TSuperPosition.InAnyPosition: Boolean;
begin
  Result := (count = -1);
end;

function TSuperPosition.InNoPosition: Boolean;
begin
  Result := (count = -2);
end;

{ TQGen }

function TQGen{$IfDef DCC}<TNode>{$EndIf}.GetOnReduce: TNotifyEvent;
begin
  Result := FOnReduce;
end;

procedure TQGen{$IfDef DCC}<TNode>{$EndIf}.SetOnReduce(const AValue: TNotifyEvent);
begin
  FOnReduce := AValue;
end;

function TQGen{$IfDef DCC}<TNode>{$EndIf}.StatesCountInHeap(I: Integer): Integer;
begin
  Result := StatesCount(PWaveFrontInfo(FFront.GetPValueByBucketIndex(FOpenHeap[I]))^.SP);
end;

procedure TQGen{$IfDef DCC}<TNode>{$EndIf}.Swap(I1, I2: Integer);
var bucketIndex: Integer;
    wInfo: PWaveFrontInfo;
begin
  FOpenHeap.Swap(I1, I2);

  bucketIndex := FOpenHeap[I1];
  wInfo := FFront.GetPValueByBucketIndex(bucketIndex);
  wInfo^.HeapIndex := I1;

  bucketIndex := FOpenHeap[I2];
  wInfo := FFront.GetPValueByBucketIndex(bucketIndex);
  wInfo^.HeapIndex := I2;
end;

procedure TQGen{$IfDef DCC}<TNode>{$EndIf}.SiftDown(Index: Integer);
var leftIdx, rightIdx: Integer;
    minChild: Integer;
begin
  while True do
  begin
    leftIdx := 2 * Index + 1;
    if leftIdx >= FOpenHeap.Count then Exit; // no left child

    rightIdx := 2 * Index + 2;
    if rightIdx < FOpenHeap.Count then
    begin
      if StatesCountInHeap(leftIdx) < StatesCountInHeap(rightIdx) {FOpenHeap[leftIdx].AllWeight - FOpenHeap[rightIdx].AllWeight < 0} then
        minChild := leftIdx
      else
        minChild := rightIdx;
    end
    else
      minChild := leftIdx;

    if StatesCountInHeap(Index) <= StatesCountInHeap(minChild) {(FOpenHeap[Index].AllWeight - FOpenHeap[minChild].AllWeight) <= 0} then Exit; // sift completed
    Swap(Index, minChild);
    Index := minChild;
  end;
end;

procedure TQGen{$IfDef DCC}<TNode>{$EndIf}.SiftUp(Index: Integer);
var parentIdx: Integer;
//    cmpResult: Single;
begin
  while True do
  begin
    if Index = 0 then Exit; // at Root
    parentIdx := (Index - 1) div 2;
//    cmpResult := FOpenHeap[parentIdx].AllWeight - FOpenHeap[Index].AllWeight;
//    if cmpResult <= 0 then Exit; // sift completed
    if StatesCountInHeap(parentIdx) <= StatesCountInHeap(Index) then Exit; // sift completed
    Swap(parentIdx, Index);
    Index := parentIdx;
  end;
end;

procedure TQGen{$IfDef DCC}<TNode>{$EndIf}.ExtractTop(out ANode: TNode; out ASP: TSuperPosition);
var bIndex: Integer;
    pInfo: PWaveFrontInfo;
begin
  bIndex := FOpenHeap[0];
  ANode := TNode(FFront.GetPKeyByBucketIndex(bIndex)^);
  pInfo := FFront.GetPValueByBucketIndex(bIndex);
  pInfo^.HeapIndex := -1;
  ASP := pInfo^.SP;

  FOpenHeap.DeleteWithSwap(0);

  if FOpenHeap.Count > 0 then
  begin
    pInfo := FFront.GetPValueByBucketIndex(FOpenHeap[0]);
    pInfo^.HeapIndex := 0;
    SiftDown(0);
  end;
end;

procedure TQGen{$IfDef DCC}<TNode>{$EndIf}.BucketIndexChange(const Key, Value; OldBucketIndex, NewBucketIndex: Integer);
var pInfo: TWaveFrontInfo absolute Value;
begin
  if FOpenHeap = nil then Exit;
  if pInfo.HeapIndex < 0 then Exit;
  FOpenHeap.Item[pInfo.HeapIndex] := NewBucketIndex;
end;

function TQGen{$IfDef DCC}<TNode>{$EndIf}.ExtendSuperPosition(const Direction: Integer; const ASP: TSuperPosition): TSuperPosition;
var j, i: Integer;
begin
  if ASP.InSuperPosition then
  begin
    Assert(ASP.count > 0);

    SetLength(Result.data, FMaxTilesCount);
    FillChar(Result.data[0], SizeOf(Result.data[0])*FMaxTilesCount, 0);

    for i := 0 to High(ASP.data) do
      if ASP.data[i] then
        for j := 0 to High(FIndexToSP[Direction][i].data) do
          Result.data[j] := Result.data[j] or FIndexToSP[Direction][i].data[j];

    Result.count := 0;
    for i := 0 to High(Result.data) do
      if Result.data[i] then Inc(Result.count);

    if Result.count = FMaxTilesCount then
      Exit(AnySuperPosition);

    if Result.count = 1 then
    begin
      for i := 0 to High(Result.data) do
        if Result.data[i] then
        begin
          Result.count := i;
          Result.data := nil;
        end;
    end;

    if Result.count = 0 then
      Exit(NoSuperPosition);
  end
  else
  begin
    if ASP.InSingleState then Exit(FIndexToSP[Direction][ASP.count]);
    if ASP.InAnyPosition then Exit(AnySuperPosition);
    if ASP.InNoPosition  then Exit(NoSuperPosition);
    Assert(False);
  end;
end;

function TQGen{$IfDef DCC}<TNode>{$EndIf}.IntersectSuperPosition(const SP1, SP2: TSuperPosition): TSuperPosition;
var
  i: Integer;
begin
  if SP1.InAnyPosition then Exit(SP2);
  if SP1.InNoPosition then Exit(SP1);
  if SP2.InAnyPosition then Exit(SP1);
  if SP2.InNoPosition then Exit(SP2);

  if SP1.InSingleState then
  begin
    if SP2.InSingleState then
    begin
      if SP1.count = SP2.count then
        Exit(SP1)
      else
        Exit(NoSuperPosition);
    end
    else //SP2 in SuperPositionState
    begin
      if SP2.data[SP1.count] then
        Exit(SP1)
      else
        Exit(NoSuperPosition);
    end;
  end;

  //SP1 in SuperPositionState
  if SP2.InSingleState then
  begin
    if SP1.data[SP2.count] then
      Exit(SP2)
    else
      Exit(NoSuperPosition);
  end
  else //SP2 in SuperPositionState too
  begin
    Result.count := 0;
    SetLength(Result.data, FMaxTilesCount);
    FillChar(Result.data[0], SizeOf(Result.data[0])*FMaxTilesCount, 0);
    for i := 0 to High(SP1.data) do
    begin
      Result.data[i] := SP1.data[i] and SP2.data[i];
      if Result.data[i] then Inc(Result.count);
    end;
    case Result.count of
      0: Result := NoSuperPosition;
      1: begin
           for i := 0 to High(Result.data) - 1 do
             if Result.data[i] then
             begin
               Result.count := i;
               Result.data := nil;
               Break;
             end;
         end;
    end;
    Exit;
  end;
end;

function TQGen{$IfDef DCC}<TNode>{$EndIf}.IsLessSuperPosition(const Left, Right: TSuperPosition): Boolean;
begin
  if Left.InAnyPosition then
  begin
    Exit(False);
  end;
  if Left.InNoPosition then
  begin
    Exit(Not Right.InNoPosition);
  end;
  if Left.InSingleState then
  begin
    Exit(Right.InSuperPosition or Right.InAnyPosition);
  end;
  if Left.InSuperPosition then
  begin
    if Right.InAnyPosition then Exit(True);
    if Right.InNoPosition then Exit(False);
    if Right.InSingleState then Exit(False);
    Exit(Left.count < Right.count);
  end;
end;

function TQGen{$IfDef DCC}<TNode>{$EndIf}.StatesCount(const SP: TSuperPosition): Integer;
begin
  if SP.InAnyPosition then Exit(FMaxTilesCount);
  if SP.InNoPosition then Exit(0);
  if SP.InSingleState then Exit(1);
  Result := SP.count;
end;

function TQGen{$IfDef DCC}<TNode>{$EndIf}.Collapse(const ANode: TNode; const SP: TSuperPosition): TSuperPosition;
var i, n: Integer;
begin
  if SP.InAnyPosition then
  begin
    Result.count := Random(FMaxTilesCount);
    Result.data := nil;
    Exit;
  end;
  if SP.InSingleState then Exit(SP);
  if SP.InNoPosition then Exit(SP);

  n := Random(SP.count);
  for i := 0 to High(SP.data) do
    if SP.data[i] then
    begin
      if n = 0 then
      begin
        Result.count := i;
        Result.data := nil;
      end;
      Dec(n);
    end;
end;

procedure TQGen{$IfDef DCC}<TNode>{$EndIf}.Resolve(const ANodes: array of TNode);
var i: Integer;
    currNode: TNode;
    waveInfo: TWaveFrontInfo;
    sp: TSuperPosition;
begin
  FOpenHeap := TOpenHeap.Create();
  for i := Low(ANodes) to High(ANodes) do
  begin
    waveInfo.HeapIndex := FOpenHeap.Count;
    waveInfo.SP := AnySuperPosition;
    FOpenHeap.Add(FFront.AddOrGetBucketIndex(ANodes[i], waveInfo));
  end;

  for i := FOpenHeap.Count div 2 - 1 downto 0 do
      SiftDown(i);

  while FOpenHeap.Count > 0 do
  begin
    ExtractTop(currNode, sp);
    Reduce(currNode, Collapse(currNode, sp));
  end;
end;

function TQGen{$IfDef DCC}<TNode>{$EndIf}.Get(const ANode: TNode): TSuperPosition;
var wfInfo: PWaveFrontInfo;
begin
  if not FFront.TryGetPValue(ANode, Pointer(wfInfo)) then
    Exit(AnySuperPosition)
  else
    Result := wfInfo.SP;
end;

procedure TQGen{$IfDef DCC}<TNode>{$EndIf}.Reduce(const ANode: TNode; const ATiles: TSuperPosition);
var waveInfo: PWaveFrontInfo;
    waveInfo_new: TWaveFrontInfo;
    newSP : TSuperPosition;
    NNode : TNode;
    direction: Integer;
begin
  if FFront.TryGetPValue(ANode, Pointer(waveInfo)) then
  begin
    newSP := IntersectSuperPosition(ATiles, waveInfo^.SP);
    if IsLessSuperPosition(newSP, waveInfo^.SP) then
    begin
      waveInfo^.SP := newSP;
      if waveInfo^.HeapIndex >= 0 then
        SiftUp(waveInfo^.HeapIndex);

      if not newSP.InNoPosition then
      begin
          for direction := 0 to FDirectionsCount - 1 do
            if FMap.GetQNeighbour(direction, ANode, NNode) then
              Reduce(NNode, ExtendSuperPosition(direction, newSP));
      end;

      If assigned(FOnReduce) Then FOnReduce(Self);
    end;
  end
  else
  begin
    waveInfo_new.HeapIndex := -1;
    waveInfo_new.SP := ATiles;
    FFront.Add(ANode, waveInfo_new);
  end;
end;

constructor TQGen{$IfDef DCC}<TNode>{$EndIf}.Create(const AMap: IMap);
var
  direction, tile: Integer;
begin
  FMap := AMap;
  FDirectionsCount := AMap.DirectionsCount;
  FMaxTilesCount := AMap.TilesCount;
  SetLength(FIndexToSP, FDirectionsCount);
  for direction := 0 to FDirectionsCount - 1 do
  begin
    SetLength(FIndexToSP[direction], FMaxTilesCount);
    for tile := 0 to FMaxTilesCount - 1 do
      FIndexToSP[direction][tile] := FMap.GetPossibleTiles(direction, tile);
  end;

  FFront := TWaveFront.Create(FMap.NodeComparer);
  FFront.OnBucketIndexChange := {$IfDef FPC}@{$EndIf}BucketIndexChange;
end;

end.

