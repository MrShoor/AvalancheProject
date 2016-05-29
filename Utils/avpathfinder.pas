unit avPathFinder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, avContnrs, avContnrsDefaults;

type
  generic IMap<TNode> = interface
    function MaxNeighbourCount(const ANode: TNode): Integer;
    function GetNeighbour(Index: Integer; const ACurrent, ATarget: TNode; out ANeighbour: TNode; out MoveWeight, DistWeight: Single): Boolean;

    function NodeComparer: IEqualityComparer;
  end;

  IDebugOut = interface
    procedure OpeninigNode(const ANode; MoveWeight, AllWeight: Single);
  end;

  { IAStar }

  generic IAStar<TNode> = interface
    function FindPath(const AStartNode, AEndNode: TNode; const ConstructClosestPath: Boolean = True): specialize IArray<TNode>;
    function FindPathWithDebugOut(const AStartNode, AEndNode: TNode; const ADebugOut: IDebugOut; const ConstructClosestPath: Boolean = True): specialize IArray<TNode>;
  end;

  { TAStar }

  generic TAStar<TNode> = class (TInterfacedObjectEx, specialize IAStar<TNode>)
  private type
    TPath = specialize TArray<TNode>;

    IFindMap = specialize IMap<TNode>;

    TNodeExtraInfo = record
      From      : TNode;
      MoveWeight: Single;
      AllWeight : Single;
    end;
    IClosedSet = specialize IHashMap<TNode, TNodeExtraInfo>;
    TClosedSet = specialize THashMap<TNode, TNodeExtraInfo>;

    TOpenHeapInfo = record
      Node      : TNode;
      From      : TNode;
      MoveWeight: Single;
      AllWeight : Single;
    end;
    IOpenHeap = specialize IArray<TOpenHeapInfo>;
    TOpenHeap = specialize TArray<TOpenHeapInfo>;

    TOpenSetInfo = record
      HeapIndex  : Integer;
      MoveWeight : Single;
    end;
    TOpenSet = specialize THashMap<TNode, TOpenSetInfo>;
    IOpenSet = specialize IHashMap<TNode, TOpenSetInfo>;
  private
    FMap: IFindMap;

    FClosed : IClosedSet;

    FOpenSet : IOpenSet;
    FOpenHeap: IOpenHeap;

    //heap stuff
    procedure Swap(I1, I2: Integer);
    procedure SiftDown(Index: Integer);
    procedure SiftUp(Index: Integer);
    //end heap stuff

    procedure AddToOpen(const ANode, AFrom: TNode; const moveW, distW: Single);
    function ExtractTop: TOpenHeapInfo;

    function ConstructPath(const ATarget, AStartNode: TNode): specialize IArray<TNode>;
    function FindPath(const AStartNode, AEndNode: TNode; const ConstructClosestPath: Boolean = True): specialize IArray<TNode>;
    function FindPathWithDebugOut(const AStartNode, AEndNode: TNode; const ADebugOut: IDebugOut; const ConstructClosestPath: Boolean = True): specialize IArray<TNode>;
  public
    constructor Create(const AMap: IFindMap);
    destructor Destroy; override;
  end;

implementation

uses Math;

{ TAStar }

procedure TAStar.Swap(I1, I2: Integer);
var sInfo: TOpenSetInfo;
    hInfo: TOpenHeapInfo;
begin
  FOpenHeap.Swap(I1, I2);

  hInfo := FOpenHeap[I1];
  sInfo := FOpenSet[hInfo.Node];
  sInfo.HeapIndex := I1;
  FOpenSet[hInfo.Node] := sInfo;

  hInfo := FOpenHeap[I2];
  sInfo := FOpenSet[hInfo.Node];
  sInfo.HeapIndex := I2;
  FOpenSet[hInfo.Node] := sInfo;
end;

procedure TAStar.SiftDown(Index: Integer);
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

procedure TAStar.SiftUp(Index: Integer);
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

procedure TAStar.AddToOpen(const ANode, AFrom: TNode; const moveW, distW: Single);
var sInfo: TOpenSetInfo;
    hInfo: TOpenHeapInfo;
begin
  if FOpenSet.TryGetValue(ANode, sInfo) then
  begin
    if sInfo.MoveWeight < moveW then Exit;
    hInfo := FOpenHeap[sInfo.HeapIndex];
    hInfo.From := AFrom;
    hInfo.MoveWeight := moveW;
    hInfo.AllWeight := moveW + distW;
    FOpenHeap[sInfo.HeapIndex] := hInfo;

    sInfo.MoveWeight := moveW;
  end
  else
  begin
    hInfo.Node := ANode;
    hInfo.From := AFrom;
    hInfo.MoveWeight := moveW;
    hInfo.AllWeight := moveW + distW;

    sInfo.HeapIndex := FOpenHeap.Add(hInfo);
    sInfo.MoveWeight := moveW;
  end;
  FOpenSet.AddOrSet(ANode, sInfo);
  SiftUp(sInfo.HeapIndex);
end;

function TAStar.ExtractTop: TOpenHeapInfo;
var sInfo: TOpenSetInfo;
begin
  Result := FOpenHeap[0];
  FOpenHeap.DeleteWithSwap(0);

  FOpenSet.Delete(Result.Node);
  if FOpenHeap.Count > 0 then
  begin
    sInfo.HeapIndex := 0;
    sInfo.MoveWeight := FOpenHeap[0].MoveWeight;
    FOpenSet.AddOrSet(FOpenHeap[0].Node, sInfo);
    SiftDown(0);
  end;
end;

function TAStar.ConstructPath(const ATarget, AStartNode: TNode): specialize IArray<TNode>;
var nextNode: TNode;
    i: Integer;
begin
  Result := TPath.Create;
  nextNode := ATarget;
  while not FMap.NodeComparer.IsEqual(nextNode, AStartNode) do
  begin
    Result.Add(nextNode);
    nextNode := FClosed[nextNode].From;
  end;
  for i := 0 to Result.Count div 2 - 1 do
    Result.Swap(i, Result.Count-1-i);
end;

function TAStar.FindPath(const AStartNode, AEndNode: TNode; const ConstructClosestPath: Boolean): specialize IArray<TNode>;
var currNodeInfo: TOpenHeapInfo;
    nextNode: TNode;
    i: Integer;
    moveW, distW: Single;

    pathStep: TNodeExtraInfo;
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
  pathStep.From := AStartNode;
  pathStep.MoveWeight := 0;
  FClosed.AddOrSet(AStartNode, pathStep);

  while FOpenSet.Count > 0 do
  begin
    currNodeInfo := ExtractTop;

    if FMap.NodeComparer.IsEqual(currNodeInfo.Node, AEndNode) then // path found, reconstuct
    begin
      Result := ConstructPath(currNodeInfo.From, AStartNode);
      Exit;
    end;

    pathStep.From := currNodeInfo.From;
    pathStep.MoveWeight := currNodeInfo.MoveWeight;
    pathStep.AllWeight := currNodeInfo.AllWeight;
    FClosed.AddOrSet(currNodeInfo.Node, pathStep);

    i := FMap.MaxNeighbourCount(currNodeInfo.Node);
    while i > 0 do
    begin
      Dec(i);
      if FMap.GetNeighbour(i, currNodeInfo.Node, AEndNode, nextNode, moveW, distW) then
      begin
        if FClosed.Contains(nextNode) then Continue;
        AddToOpen(nextNode, currNodeInfo.Node, FClosed[currNodeInfo.Node].MoveWeight+moveW, distW);
      end;
    end;
  end;

  if ConstructClosestPath then
  begin
    minDistW := Infinity;
    minNode := AStartNode;
    FClosed.Reset;
    while FClosed.Next(nextNode, pathStep) do
    begin
      distW := pathStep.AllWeight - pathStep.MoveWeight;
      if distW < minDistW then
      begin
        minDistW := distW;
        minNode := nextNode;
      end;
    end;
    Result := ConstructPath(minNode, AStartNode);
  end;
end;

function TAStar.FindPathWithDebugOut(const AStartNode, AEndNode: TNode;
  const ADebugOut: IDebugOut; const ConstructClosestPath: Boolean): specialize IArray<TNode>;
var currNodeInfo: TOpenHeapInfo;
    nextNode: TNode;
    i: Integer;
    moveW, distW: Single;

    pathStep: TNodeExtraInfo;
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
  pathStep.From := AStartNode;
  pathStep.MoveWeight := 0;
  FClosed.AddOrSet(AStartNode, pathStep);

  while FOpenSet.Count > 0 do
  begin
    currNodeInfo := ExtractTop;
    ADebugOut.OpeninigNode(currNodeInfo.Node, currNodeInfo.MoveWeight, currNodeInfo.AllWeight);

    if FMap.NodeComparer.IsEqual(currNodeInfo.Node, AEndNode) then // path found, reconstuct
    begin
      Result := ConstructPath(currNodeInfo.From, AStartNode);
      Exit;
    end;

    pathStep.From := currNodeInfo.From;
    pathStep.MoveWeight := currNodeInfo.MoveWeight;
    pathStep.AllWeight := currNodeInfo.AllWeight;
    FClosed.AddOrSet(currNodeInfo.Node, pathStep);

    i := FMap.MaxNeighbourCount(currNodeInfo.Node);
    while i > 0 do
    begin
      Dec(i);
      if FMap.GetNeighbour(i, currNodeInfo.Node, AEndNode, nextNode, moveW, distW) then
      begin
        if FClosed.Contains(nextNode) then Continue;
        AddToOpen(nextNode, currNodeInfo.Node, FClosed[currNodeInfo.Node].MoveWeight+moveW, distW);
      end;
    end;
  end;

  if ConstructClosestPath then
  begin
    minDistW := Infinity;
    minNode := AStartNode;
    FClosed.Reset;
    while FClosed.Next(nextNode, pathStep) do
    begin
      distW := pathStep.AllWeight - pathStep.MoveWeight;
      if distW < minDistW then
      begin
        minDistW := distW;
        minNode := nextNode;
      end;
    end;
    Result := ConstructPath(minNode, AStartNode);
  end;
end;

constructor TAStar.Create(const AMap: IFindMap);
begin
  FMap := AMap;

  FClosed := TClosedSet.Create(FMap.NodeComparer);
  FOpenSet := TOpenSet.Create(FMap.NodeComparer);
  FOpenHeap := TOpenHeap.Create;
end;

destructor TAStar.Destroy;
begin
  inherited Destroy;
end;

end.

