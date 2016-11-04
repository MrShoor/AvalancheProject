unit avPathFinder;
{$I avConfig.inc}

//{$Define DebugOut}

interface

uses
  Classes, SysUtils, avContnrs, avContnrsDefaults, Math;

type
  {$IfDef FPC}generic{$EndIf} IMap<TNode> = interface
    function MaxNeighbourCount(const ANode: TNode): Integer;
    function GetNeighbour(Index: Integer; const ACurrent, ATarget: TNode; out ANeighbour: TNode; out MoveWeight, DistWeight: Single): Boolean;

    function NodeComparer: IEqualityComparer;
  end;

  IDebugOut = interface
    procedure OpeninigNode(const ANode; MoveWeight, AllWeight: Single);
  end;

  { IAStar }

  {$IfDef FPC}generic{$EndIf} IAStar<TNode> = interface
    function FindPath(const AStartNode, AEndNode: TNode; {$IfDef DebugOut}const ADebugOut: IDebugOut;{$EndIf} AStopWeigth: Single = Infinity; ConstructClosestPath: Boolean = True): {$IfDef FPC}specialize{$EndIf} IArray<TNode>;
  end;

  { TAStar }

  {$IfDef FPC}generic{$EndIf} TAStar<TNode> = class (TInterfacedObjectEx, {$IfDef FPC}specialize{$EndIf} IAStar<TNode>)
  private type
    TPath = {$IfDef FPC}specialize{$EndIf} TArray<TNode>;

    IFindMap = {$IfDef FPC}specialize{$EndIf} IMap<TNode>;

    TOpenHeapInfo = record
      NodeBucket: Integer;
      AllWeight : Single;
    end;
    POpenHeapInfo = ^TOpenHeapInfo;
    IOpenHeap = {$IfDef FPC}specialize{$EndIf} IArray<TOpenHeapInfo>;
    TOpenHeap = {$IfDef FPC}specialize{$EndIf} TArray<TOpenHeapInfo>;

    TOpenSetInfo = record
      HeapIndex  : Integer;
      From       : TNode;
      MoveWeight : Single;
      AllWeight  : Single;
    end;
    POpenSetInfo = ^TOpenSetInfo;
    TOpenSet = {$IfDef FPC}specialize{$EndIf} THashMapWithBuckets<TNode, TOpenSetInfo>;
    IOpenSet = {$IfDef FPC}specialize{$EndIf} IHashMapWithBuckets<TNode, TOpenSetInfo>;
  private
    FMap: IFindMap;

    FOpenSet : IOpenSet;
    FOpenHeap: IOpenHeap;

    //heap stuff
    procedure Swap(I1, I2: Integer);
    procedure SiftDown(Index: Integer);
    procedure SiftUp(Index: Integer);
    //end heap stuff

    procedure BucketIndexChange(const Key, Value; OldBucketIndex, NewBucketIndex: Integer);

    procedure AddToOpen(const ANode, AFrom: TNode; const moveW, distW: Single);
    procedure ExtractTop(out ANode: TNode; out AStepInfo: TOpenSetInfo);

    function ConstructPath(const ATarget, AStartNode: TNode): {$IfDef FPC}specialize{$EndIf} IArray<TNode>;
    function FindPath(const AStartNode, AEndNode: TNode; {$IfDef DebugOut}const ADebugOut: IDebugOut;{$EndIf} AStopWeigth: Single = Infinity; ConstructClosestPath: Boolean = True): {$IfDef FPC}specialize{$EndIf} IArray<TNode>;
  public
    constructor Create(const AMap: IFindMap);
    destructor Destroy; override;
  end;

implementation

{ TAStar }

procedure TAStar{$IfDef DCC}<TNode>{$EndIf}.Swap(I1, I2: Integer);
var psInfo: POpenSetInfo;
    hInfo: TOpenHeapInfo;
begin
  FOpenHeap.Swap(I1, I2);

  hInfo := FOpenHeap[I1];
  psInfo := FOpenSet.GetPValueByBucketIndex(hInfo.NodeBucket);
  psInfo^.HeapIndex := I1;

  hInfo := FOpenHeap[I2];
  psInfo := FOpenSet.GetPValueByBucketIndex(hInfo.NodeBucket);
  psInfo^.HeapIndex := I2;
end;

procedure TAStar{$IfDef DCC}<TNode>{$EndIf}.SiftDown(Index: Integer);
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
      if FOpenHeap[leftIdx].AllWeight - FOpenHeap[rightIdx].AllWeight < 0 then
        minChild := leftIdx
      else
        minChild := rightIdx;
    end
    else
      minChild := leftIdx;

    if (FOpenHeap[Index].AllWeight - FOpenHeap[minChild].AllWeight) <= 0 then Exit; // sift completed
    Swap(Index, minChild);
    Index := minChild;
  end;
end;

procedure TAStar{$IfDef DCC}<TNode>{$EndIf}.SiftUp(Index: Integer);
var parentIdx: Integer;
    cmpResult: Single;
begin
  while True do
  begin
    if Index = 0 then Exit; // at Root
    parentIdx := (Index - 1) div 2;

    cmpResult := FOpenHeap[parentIdx].AllWeight - FOpenHeap[Index].AllWeight;
    if cmpResult <= 0 then Exit; // sift completed
    Swap(parentIdx, Index);
    Index := parentIdx;
  end;
end;

procedure TAStar{$IfDef DCC}<TNode>{$EndIf}.BucketIndexChange(const Key, Value; OldBucketIndex,
  NewBucketIndex: Integer);
var sInfo: TOpenSetInfo absolute Value;
    hInfo: POpenHeapInfo;
begin
  if sInfo.HeapIndex < 0 then Exit;
  hInfo := FOpenHeap.PItem[sInfo.HeapIndex];
  hInfo^.NodeBucket := NewBucketIndex;
end;

procedure TAStar{$IfDef DCC}<TNode>{$EndIf}.AddToOpen(const ANode, AFrom: TNode; const moveW, distW: Single);
var psInfo: POpenSetInfo;
    phInfo: POpenHeapInfo;
    sInfo: TOpenSetInfo;
    hInfo: TOpenHeapInfo;
begin
  if FOpenSet.TryGetPValue(ANode, Pointer(psInfo)) then
  begin
    if psInfo^.HeapIndex < 0 then Exit; //closed vertex
    if psInfo^.MoveWeight < moveW then Exit;
    phInfo := FOpenHeap.PItem[psInfo^.HeapIndex];
    phInfo^.AllWeight := moveW + distW;

    psInfo^.MoveWeight := moveW;
    psInfo^.AllWeight := phInfo^.AllWeight;
    psInfo^.From := AFrom;

    SiftUp(psInfo^.HeapIndex);
  end
  else
  begin
    sInfo.HeapIndex := FOpenHeap.Count;
    sInfo.MoveWeight := moveW;
    sInfo.AllWeight := moveW + distW;
    sInfo.From := AFrom;

    hInfo.NodeBucket := FOpenSet.AddOrSetWithBucketIndex(ANode, sInfo);
    hInfo.AllWeight := sInfo.AllWeight;
    FOpenHeap.Add(hInfo);

    SiftUp(sInfo.HeapIndex);
  end;
end;

procedure TAStar{$IfDef DCC}<TNode>{$EndIf}.ExtractTop(out ANode: TNode; out AStepInfo: TOpenSetInfo);
var bIndex: Integer;
    sInfo: POpenSetInfo;
begin
  bIndex := FOpenHeap[0].NodeBucket;
  ANode := TNode(FOpenSet.GetPKeyByBucketIndex(bIndex)^);
  sInfo := FOpenSet.GetPValueByBucketIndex(bIndex);
  sInfo^.HeapIndex := -1;
  AStepInfo := sInfo^;

  FOpenHeap.DeleteWithSwap(0);

  if FOpenHeap.Count > 0 then
  begin
    sInfo := FOpenSet.GetPValueByBucketIndex(FOpenHeap[0].NodeBucket);
    sInfo^.HeapIndex := 0;
    SiftDown(0);
  end;
end;

function TAStar{$IfDef DCC}<TNode>{$EndIf}.ConstructPath(const ATarget, AStartNode: TNode): {$IfDef FPC}specialize{$EndIf} IArray<TNode>;
var nextNode: TNode;
    i: Integer;
    startTime, endTime, freq: Int64;
begin
  Result := TPath.Create;
  nextNode := ATarget;
  while not FMap.NodeComparer.IsEqual(nextNode, AStartNode) do
  begin
    Result.Add(nextNode);
    nextNode := FOpenSet[nextNode].From;
  end;
  for i := 0 to Result.Count div 2 - 1 do
    Result.Swap(i, Result.Count-1-i);
end;

function TAStar{$IfDef DCC}<TNode>{$EndIf}.FindPath(const AStartNode, AEndNode: TNode;
  {$IfDef DebugOut}const ADebugOut: IDebugOut;{$EndIf}
  AStopWeigth: Single; ConstructClosestPath: Boolean): {$IfDef FPC}specialize{$EndIf} IArray<TNode>;
var currStepInfo: TOpenSetInfo;
    currNode: TNode;
    nextNode: TNode;
    i: Integer;
    moveW, distW: Single;

    minNode: TNode;
    minDistW: Single;
begin
  Result := nil;
  if FMap.NodeComparer.IsEqual(AEndNode, AStartNode) then
  begin
    Result := TPath.Create;
    Exit;
  end;

  AddToOpen(AStartNode, Default(TNode), 0, Infinity);

  while FOpenHeap.Count > 0 do
  begin
    ExtractTop(currNode, currStepInfo);
    {$IfDef DebugOut}
    if Assigned(ADebugOut) then
      ADebugOut.OpeninigNode(currNode, currStepInfo.MoveWeight, currStepInfo.AllWeight);
    {$EndIf}

    if FMap.NodeComparer.IsEqual(currNode, AEndNode) then // path found, reconstuct
    begin
      Result := ConstructPath(currStepInfo.From, AStartNode);
      Exit;
    end;

    if currStepInfo.MoveWeight >= AStopWeigth then break;

    i := FMap.MaxNeighbourCount(currNode);
    while i > 0 do
    begin
      Dec(i);
      if FMap.GetNeighbour(i, currNode, AEndNode, nextNode, moveW, distW) then
        AddToOpen(nextNode, currNode, currStepInfo.MoveWeight+moveW, distW);
    end;
  end;

  if ConstructClosestPath then
  begin
    minDistW := Infinity;
    minNode := AStartNode;
    FOpenSet.Reset;
    while FOpenSet.Next(nextNode, currStepInfo) do
    begin
      distW := currStepInfo.AllWeight - currStepInfo.MoveWeight;
      if distW < minDistW then
      begin
        minDistW := distW;
        minNode := nextNode;
      end;
    end;
    Result := ConstructPath(minNode, AStartNode);
  end;
end;

constructor TAStar{$IfDef DCC}<TNode>{$EndIf}.Create(const AMap: IFindMap);
begin
  FMap := AMap;

  FOpenSet := TOpenSet.Create(FMap.NodeComparer);
  {$IfDef FPC}
  FOpenSet.OnBucketIndexChange := @BucketIndexChange;
  {$Else}
  FOpenSet.OnBucketIndexChange := BucketIndexChange;
  {$EndIf}
  FOpenHeap := TOpenHeap.Create;
end;

destructor TAStar{$IfDef DCC}<TNode>{$EndIf}.Destroy;
begin
  inherited Destroy;
end;

end.

