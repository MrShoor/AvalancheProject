//must be defined
// DIM - dimension count of tree
// TCompType - component type of box (and space)

{$IFDEF INTERFACE}
  TBox = packed record
    min: array [0..DIM-1] of TCompType;
    max: array [0..DIM-1] of TCompType;
  end;

  { TLooseQuadTree_f }

  generic TLooseTree<TValue> = class
  private
    type
      TItemList = specialize TArray<TValue>;
      TBoxList =  specialize TArray<TBox>;
      THashList = specialize THashMap<TValue, Integer>;
      IItemList = specialize IArray<TValue>;
      IBoxList =  specialize IArray<TBox>;
      IHashList = specialize IHashMap<TValue, Integer>;
  private
    FEmptyValue: TValue;

    FItems: IItemList;
    FBoxes: IBoxList;
    FHash : IHashList;

    FSearchBox: TBox;
    FSearchIndex: Integer;

    function IntersectBoxes(const a,b: TBox): Boolean;
  public
    function Count: Integer;

    procedure Clear;
    function Contains(Const AItem: TValue): Boolean;
    procedure Add(Const ABox: TBox; Const AItem: TValue);
    procedure Del(Const AItem: TValue);

    procedure StartSearch(ASearchBox: TBox);
    function Next(out AItem: TValue): Boolean;

    constructor Create(const AEmptyValue: TValue);
  end;
{$ENDIF}

{$IFDEF IMPLEMENTATION}

{ TLooseTree }

function TLooseTree.IntersectBoxes(const a, b: TBox2f): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to DIM-1 do
    Result := Result or (a.min[i]>b.max[i]) or (b.min[i]>a.max[i]);
  Result := not Result;
end;

function TLooseTree.Count: Integer;
begin
  Result := FItems.Count;
end;

procedure TLooseTree.Clear;
begin
  FItems.Clear;
  FBoxes.Clear;
  FHash.Clear;
end;

function TLooseTree.Contains(const AItem: TValue): Boolean;
begin
  Result := FHash.Contains(AItem);
end;

procedure TLooseTree.Add(const ABox: TBox; const AItem: TValue);
var n: Integer;
begin
  if FHash.TryGetValue(AItem, n) then
  begin
    FBoxes[n] := ABox;
  end
  else
  begin
    FHash.Add(AItem, FItems.Count);
    FItems.Add(AItem);
    FBoxes.Add(ABox);
  end;
end;

procedure TLooseTree.Del(const AItem: TValue);
var n: Integer;
begin
  if FHash.TryGetValue(AItem, n) then
  begin
    FHash.Delete(AItem);
    FItems.Delete(n);
    FBoxes.Delete(n);
  end;
end;

procedure TLooseTree.StartSearch(ASearchBox: TBox);
begin
  FSearchBox := ASearchBox;
  FSearchIndex := 0;
end;

function TLooseTree.Next(out AItem: TValue): Boolean;
begin
  while (FSearchIndex < FItems.Count) do
  begin
    if IntersectBoxes(FSearchBox, FBoxes[FSearchIndex]) then
    begin
      Result := True;
      AItem := FItems[FSearchIndex];
      Inc(FSearchIndex);
      Exit;
    end;
    Inc(FSearchIndex);
  end;
  Result := False;
end;

constructor TLooseTree.Create(const AEmptyValue: TValue);
begin
  FEmptyValue := AEmptyValue;

  FItems := TItemList.Create(FEmptyValue);
  FBoxes := TBoxList.Create;
  FHash  := THashList.Create(FEmptyValue, 0);
end;

{$ENDIF}
