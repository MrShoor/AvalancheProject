unit avTess;
{$I avConfig.inc}
//{$Define SlowAssertions}

interface

uses
  Classes, SysUtils, avTypes, mutils, avContnrs, avContnrsDefaults;

type
  { TVerticesRec }


//  TRec should contain Layout class function
//  TRec = packed record
//    class function Layout: IDataLayout; static;
//  end;
  {$IfDef FPC}generic{$EndIf} TVerticesRec<TRec> = class ({$IfDef FPC}specialize{$EndIf} TArray<TRec>, IVerticesData)
  strict private
    FLayout: IDataLayout;
    function VerticesCount: Integer;
    function Layout: IDataLayout;
    function Data: TPointerData;
  public
    constructor Create; overload;
    constructor Create(const ALayout: IDataLayout); overload;
  end;

  { ILayoutBuilder }

  ILayoutBuilder = interface
    procedure Reset;
    function Add(Name: string; CompType: TComponentType; CompCount: Integer; DoNorm: Boolean = False; Offset: Integer = -1): ILayoutBuilder;
    function Finish(TotalSize: Integer = -1): IDataLayout;
  end;

  { IIndices }

  IIndices = interface (IIndicesData)
  ['{DF1D969A-9D96-4885-9012-340ECA017BA3}']
  //*properties implementation
    function GetCapacity: Integer;
    function GetIndex(index: Integer): Integer;
    function GetIsDWord: Boolean;
    function GetLine(index: Integer): TVec2i;
    function GetTriangle(index: Integer): TVec3i;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetIndex(index: Integer; const Value: Integer);
    procedure SetIsDWord(const Value: Boolean);
    procedure SetLine(index: Integer; const Value: TVec2i);
    procedure SetPrimitiveType(const Value: TPrimitiveType);
    procedure SetTriangle(index: Integer; const Value: TVec3i);
    procedure SetPrimitiveCount(const Value: Integer);
  //*end of properties implementation

    function Clone: IIndices;
    procedure Clear;

    property IsDWord: Boolean read GetIsDWord write SetIsDWord;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count   : Integer read IndicesCount write SetCount;
    procedure Add(NewIndex: Integer); overload;

    property Ind[index: Integer]: Integer read GetIndex write SetIndex;

    property PrimitiveCount: Integer read PrimCount write SetPrimitiveCount;
    property PrimitiveType: TPrimitiveType read PrimType write SetPrimitiveType;
    property Line[index: Integer]: TVec2i read GetLine write SetLine;
    property Triangle[index: Integer]: TVec3i read GetTriangle write SetTriangle;
  end;

  TPolyTris = record
    Verts  : IVerticesData;
    Ind    : IIndices;
    Outline: IIndices;
  end;

  TClipOperation = (coUnion, coDiff, coInt);

  IPoly = interface
  ['{46EA3D19-C9F0-4902-A491-05C08EF9566A}']
    function CoordLayoutIndex: Integer;

    function BoundingRect: TRectF;
    function PointIn(const V: TVec2): Boolean;
    function Distance(const V: TVec2): Boolean;
    procedure Transform(const M: TMat3);
    procedure Expand(Expansion: Single);

    function Clip(operation: TClipOperation; const SecondPoly: IPoly): IPoly;

    function Tris: TPolyTris;
  end;

  IMemRange = interface
    function Size  : Integer;
    function Offset: Integer;
  end;

  IRangeManager = interface
    function GetName: string;
    procedure SetName(const AName: string);

    function Alloc(const ASize: Integer): IMemRange;

    function Size     : Integer;
    function Allocated: Integer;
    function FreeSpace: Integer;

    procedure AddSpace(const ASpace: Integer);
    procedure Defrag();
    procedure Trim;

    property Name: string read GetName write SetName;
  end;

  EQuadRangeOutOfSpace = class (Exception);
  IQuadManager = interface;

  IQuadRange = interface
    function MinXY : TVec2i;
    function MaxXY : TVec2i;
    function Rect  : TRectI;
    function Size  : TVec2i;
    function Width : Integer;
    function Height: Integer;
    function Area  : Int64;

    function UserData: Pointer;
  end;

  TOnMoveQuad = procedure (const Sender: IQuadManager; const Quad: IQuadRange; const OldRect: TRectI) of object;

  IQuadManager = interface
    function Alloc(const AWidth, AHeight: Integer; const AUserData: Pointer = nil): IQuadRange;

    function Width : Integer;
    function Height: Integer;

    function AllocatedArea : Int64;
    function FreeArea      : Int64;
    function AllocatedCount: Integer;

    function SetSize(const AWidth, AHeight: Integer; const ACallBack: TOnMoveQuad = nil): Boolean;
    function Repack(const ACallBack: TOnMoveQuad = nil): Boolean;
  end;

function Create_IIndices : IIndices;
function LB: ILayoutBuilder;
function Create_IRangeManager(const InitialSpace: Integer = 0): IRangeManager;
function Create_IQuadManager(const InitialWidth: Integer = 0; const InitialHeight: Integer = 0): IQuadManager;

implementation

uses Math, {$IfDef FPC}AVL_Tree{$EndIf}{$IfDef DCC}AVL_Tree_d, RTTI{$EndIf};

threadvar GV_LB: ILayoutBuilder;

type
  { TLayoutBuilder }

  TLayoutBuilder = class(TInterfacedObject, ILayoutBuilder)
  private
    FFields: array [0..63] of TFieldInfo;
    FCount: Integer;
    FTotalSize: Integer;
  public
    procedure Reset;
    function Add(Name: string; CompType: TComponentType; CompCount: Integer; DoNorm: Boolean = False; Offset: Integer = -1): ILayoutBuilder;
    function Finish(TotalSize: Integer = -1): IDataLayout;
  end;

  { TIndices }

  TIndices = class (TInterfacedObject, IIndicesData, IIndices)
  private
    FData: array of Byte;
    FCount: Integer;

    FIsDWord: Boolean;
    FPrimType: TPrimitiveType;

    function StrideSize: Integer;

    procedure Grow(Size: Integer);
  private
    function IndexSize: Integer;
    function IndicesCount: Integer;
    function GetCapacity: Integer;
    function GetIndex(index: Integer): Integer;
    function GetIsDWord: Boolean;
    function GetLine(index: Integer): TVec2i;
    function PrimType: TPrimitiveType;
    function GetTriangle(index: Integer): TVec3i;
    function PrimCount: Integer;
    procedure SetCapacity(const Value: Integer);
    procedure SetCount(const Value: Integer);
    procedure SetIndex(index: Integer; const Value: Integer);
    procedure SetIsDWord(const Value: Boolean);
    procedure SetLine(index: Integer; const Value: TVec2i);
    procedure SetPrimitiveType(const Value: TPrimitiveType);
    procedure SetTriangle(index: Integer; const Value: TVec3i);
    procedure SetPrimitiveCount(const Value: Integer);
  public
    function Clone: IIndices;
    procedure Clear;

    property IsDWord: Boolean read GetIsDWord write SetIsDWord;

    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count  : Integer read IndicesCount write SetCount;
    procedure Add(NewIndex: Integer); overload;

    property Ind[index: Integer]: Integer read GetIndex write SetIndex;

    property PrimitiveCount: Integer read PrimCount write SetPrimitiveCount;
    property PrimitiveType: TPrimitiveType read PrimType write SetPrimitiveType;
    property Line[index: Integer]: TVec2i read GetLine write SetLine;
    property Triangle[index: Integer]: TVec3i read GetTriangle write SetTriangle;

    function Data: TPointerData;
  end;
{
  TPoly = class (TInterfacedObject, IPoly)
  public
    function CoordLayoutIndex: Integer;

    function BoundingRect: TRectF;
    function PointIn(const V: TVec2): Boolean;
    function Distance(const V: TVec2): Boolean;
    procedure Transform(const M: TMat3);
    procedure Expand(Expansion: Single);

    function Clip(operation: TClipOperation; const SecondPoly: IPoly): IPoly;

    function Tris: TPolyTris;
  end;
}

  { TMemRangeEx }
  TRangeManager = class;

  TMemRangeEx = class (TObject, IUnknown, IMemRange)
  private
    FManager : TRangeManager;
    FRefCount: Integer;
    FOffset  : Integer;
    FSize    : Integer;

    FPrev    : TMemRangeEx;
    FNext    : TMemRangeEx;
  public
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

    function Size  : Integer;
    function Offset: Integer;
  end;

  { TRangeManager }

  TRangeManager = class (TInterfacedObject, IRangeManager)
  private type
    TAVLTreeInternal = class
    private type
      TRangeManagerAVLData = class
      public
        Size: Integer;
        List: TList;
        constructor Create(ASize: Integer; AList: TList);
      end;
    {strict} private
      FData: TAVLTree;
      FDummy: TRangeManagerAVLData;
      function FindMaxNode(const data: TRangeManagerAVLData): TAVLTreeNode;
    public
      function TryGet(const ASize: Integer; out AValue: TList): Boolean;
      function TryGetMax(const ASize: Integer; out AValue: TList): Boolean;
      function Add(const ASize: Integer; const AValue: TList): Boolean;
      function Del(const ASize: Integer): boolean;
      procedure Clear;

      constructor Create;
      destructor Destroy; override;
    end;
  private
    FName : string;

    FFreeRanges: TAVLTreeInternal;
    FLastRange : TMemRangeEx;

    FTotalSize    : Integer;
    FAllocatedSize: Integer;

    procedure PutAtFree(const Range: TMemRangeEx);
    procedure PopFromFree(const Range: TMemRangeEx);

    procedure Dispose(Range: TMemRangeEx);

    function CompareSize(const Left, Right: Integer): Integer;
  protected
    procedure CheckNodeInvariants(const ANode: TAVLTreeNode);
    procedure CheckTreeInvariants;
    procedure CheckInvariants;
  public
    function GetName: string;
    procedure SetName(const AName: string);

    function Alloc(const ASize: Integer): IMemRange;

    function Size     : Integer;
    function Allocated: Integer;
    function FreeSpace: Integer;

    procedure AddSpace(const ASpace: Integer);
    procedure Defrag();
    procedure Trim;

    constructor Create;
    destructor Destroy; override;
  end;

  { TQuadManager }

  TQuadManager = class(TInterfacedObject, IQuadManager)
  private type
    TQuadComparer = class(TInterfacedObjectEx, IComparer)
    private
      function Compare(const Left, Right): Integer;
    end;

    { TQuadRange }

    TQuadRange = class(TObject, IUnknown, IQuadRange)
    private
      FOwner   : TQuadManager;
      FRefCount: Integer;
      FRect    : TRectI;
      FUserData: Pointer;
    protected
      function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
      function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
      function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    protected
      function MinXY : TVec2i;
      function MaxXY : TVec2i;
      function Rect  : TRectI;
      function Size  : TVec2i;
      function Width : Integer;
      function Height: Integer;
      function Area  : Int64;

      function UserData: Pointer;
    public
      constructor Create(AOwner: TQuadManager; const ARect: TRectI);
      destructor Destroy; override;
    end;

    IQuadList = {$IfDef FPC}specialize{$EndIf} IArray<TQuadRange>;
    TQuadList = {$IfDef FPC}specialize{$EndIf} TArray<TQuadRange>;

    TRemapPair = record
      OldQuad: TQuadRange;
      NewQuad: TQuadRange;
    end;
    IRemapList = {$IfDef FPC}specialize{$EndIf} IArray<TRemapPair>;
    TRemapList = {$IfDef FPC}specialize{$EndIf} TArray<TRemapPair>;
  private
    FWidth : Integer;
    FHeight: Integer;
    FAllocatedArea : Int64;

    FAllocatedQuads: IQuadList;
    FFreeQuads     : IQuadList;

    procedure OnQuadReleased(const AQuad: TQuadRange);
    function TryRepackToNewSize(const AWidth, AHeight: Integer; const ACallBack: TOnMoveQuad): Boolean;
    function AllocRange(const AWidth, AHeight: Integer; const AUserData: Pointer): TQuadRange;
  protected
    function Alloc(const AWidth, AHeight: Integer; const AUserData: Pointer): IQuadRange;

    function Width : Integer;
    function Height: Integer;

    function AllocatedArea : Int64;
    function FreeArea      : Int64;
    function AllocatedCount: Integer;

    function SetSize(const AWidth, AHeight: Integer; const ACallBack: TOnMoveQuad = nil): Boolean;
    function Repack(const ACallBack: TOnMoveQuad = nil): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function Create_IIndices: IIndices;
begin
  Result := TIndices.Create;
end;

function LB: ILayoutBuilder;
begin
  if GV_LB = nil then GV_LB := TLayoutBuilder.Create;
  GV_LB.Reset;
  Result := GV_LB;
end;

function Create_IRangeManager(const InitialSpace: Integer): IRangeManager;
begin
  Result := TRangeManager.Create;
  if InitialSpace > 0 then;
    Result.AddSpace(InitialSpace);
end;

function Create_IQuadManager(const InitialWidth, InitialHeight: Integer): IQuadManager;
begin
  Result := TQuadManager.Create;
  Result.SetSize(InitialWidth, InitialHeight);
end;

{ TQuadManager.TQuadComparer }

function TQuadManager.TQuadComparer.Compare(const Left, Right): Integer;
var L: TQuadRange absolute Left;
    R: TQuadRange absolute Right;
begin
  Result := sign(R.Area - L.Area);
end;

{ TQuadManager }

procedure TQuadManager.OnQuadReleased(const AQuad: TQuadRange);
begin
  FAllocatedQuads.DeleteWithSwap(FAllocatedQuads.IndexOf(AQuad));
  FFreeQuads.Add(AQuad);
end;

function TQuadManager.TryRepackToNewSize(const AWidth, AHeight: Integer; const ACallBack: TOnMoveQuad): Boolean;
var
  RemapList: IRemapList;
  RemapPair: TRemapPair;
  OldFreeQuads: IQuadList;
  OldSize: TVec2I;
  OldRect: TRectI;
  i: Integer;
  cmp: IComparer;
begin
  Result := False;

  cmp := TQuadComparer.Create();
  FAllocatedQuads.Sort(cmp);

  OldFreeQuads := FFreeQuads;
  OldSize.x := FWidth;
  OldSize.y := FHeight;

  try
    FWidth := AWidth;
    FHeight := AHeight;
    FFreeQuads := TQuadList.Create();
    FFreeQuads.Capacity := FAllocatedQuads.Count * 2;
    FFreeQuads.Add(TQuadRange.Create(Self, RectI(0,0,FWidth,FHeight)));

    RemapList := TRemapList.Create();
    RemapList.Capacity := FAllocatedQuads.Count;
    for i := 0 to FAllocatedQuads.Count - 1 do
    begin
      RemapPair.OldQuad := FAllocatedQuads[i];
      RemapPair.NewQuad := AllocRange(RemapPair.OldQuad.Width, RemapPair.OldQuad.Height, RemapPair.OldQuad.UserData);
      RemapList.Add(RemapPair);
    end;

    for i := 0 to RemapList.Count - 1 do
    begin
      RemapPair := RemapList[i];
      OldRect := RemapPair.OldQuad.FRect;
      RemapPair.OldQuad.FRect := RemapPair.NewQuad.FRect;
      RemapPair.NewQuad.Free;
      if Assigned(ACallBack) Then ACallBack(Self, RemapPair.OldQuad, OldRect);
    end;

    for i := 0 to OldFreeQuads.Count - 1 do OldFreeQuads[i].Free;
    OldFreeQuads := nil;
    Result := True;
  except
    on e: EQuadRangeOutOfSpace do
    begin
      FWidth  := OldSize.x;
      FHeight := OldSize.y;
      for i := 0 to FFreeQuads.Count - 1 do FFreeQuads[i].Free;
      FFreeQuads := OldFreeQuads;
    end;
  end;
end;

function TQuadManager.AllocRange(const AWidth, AHeight: Integer;
  const AUserData: Pointer): TQuadRange;

  function FindBestQuadForSplit(): Integer;
  var bestArea, currArea: Int64;
      range: TQuadRange;
      i: Integer;
  begin
    bestArea :=  $7FFFFFFFFFFFFFFF;
    Result := -1;
    for i := 0 to FFreeQuads.Count - 1 do
    begin
      range := FFreeQuads[i];
      if (AWidth > range.Width) then Continue;
      if (AHeight > range.Height) then Continue;

      currArea := range.Area;
      if currArea <= bestArea then
      begin
        Result := i;
        bestArea := currArea;
      end;
    end;

    if Result = -1 then raise EQuadRangeOutOfSpace.CreateFmt('No enought space for %d*%d quad.', [AWidth, AHeight]);
  end;

  procedure Split(const SrcQuad: TQuadRange; const W,H: Integer; out NewQuad, BottomQuad, RightQuad: TQuadRange);
  var SrcW, SrcH: Integer;
      rct: TRectI;
  begin
    SrcW := SrcQuad.Width;
    SrcH := SrcQuad.Height;
    NewQuad := TQuadRange.Create(Self, RectI(SrcQuad.MinXY, SrcQuad.MinXY + Vec(W, H)));
    if (SrcW - W) < (SrcH - H) then
    begin
        if H = SrcH then
          BottomQuad := nil
        else
        begin
          rct := SrcQuad.FRect;
          Inc(rct.Top, H);
          BottomQuad := TQuadRange.Create(Self, rct);
        end;
        if W = SrcW then
          RightQuad := nil
        else
        begin
          rct := SrcQuad.FRect;
          Inc(rct.Left, W);
          rct.Bottom := rct.Top + H;
          RightQuad  := TQuadRange.Create(Self, rct);
        end;
    end
    else
    begin
        if W = SrcQuad.Width then
          RightQuad := nil
        else
        begin
          rct := SrcQuad.FRect;
          Inc(rct.Left, W);
          RightQuad  := TQuadRange.Create(Self, rct);
        end;
        if H = SrcQuad.Height then
          BottomQuad := nil
        else
        begin
          rct := SrcQuad.FRect;
          Inc(rct.Top, H);
          rct.Right := rct.Left + W;
          BottomQuad := TQuadRange.Create(Self, rct);
        end;
    end;
  end;
var bestQuadIndex: Integer;
    bestQuad, newQuad, rightQuad, bottomQuad: TQuadRange;
begin
  Result := nil;
  bestQuadIndex := FindBestQuadForSplit();
  bestQuad := FFreeQuads[bestQuadIndex];
  FFreeQuads.DeleteWithSwap(bestQuadIndex);
  if (bestQuad.Width = AWidth) and (bestQuad.Height = AHeight) then
  begin
    bestQuad.FUserData := AUserData;
    Result := bestQuad;
  end
  else
  begin
    Split(bestQuad, AWidth, AHeight, newQuad, bottomQuad, rightQuad);
    newQuad.FUserData := AUserData;
    Result := newQuad;
    if bottomQuad <> nil then FFreeQuads.Add(bottomQuad);
    if rightQuad <> nil then FFreeQuads.Add(rightQuad);
    bestQuad.Free;
  end;
end;

function TQuadManager.Alloc(const AWidth, AHeight: Integer; const AUserData: Pointer): IQuadRange;
var quad: TQuadRange;
begin
  quad := AllocRange(AWidth, AHeight, AUserData);
  FAllocatedQuads.Add(quad);
  Inc(FAllocatedArea, quad.Area);
  Result := quad;
end;

function TQuadManager.Width: Integer;
begin
  Result := FWidth;
end;

function TQuadManager.Height: Integer;
begin
  Result := FHeight;
end;

function TQuadManager.AllocatedArea: Int64;
begin
  Result := FAllocatedArea;
end;

function TQuadManager.FreeArea: Int64;
begin
  Result := FWidth*FHeight - FAllocatedArea;
end;

function TQuadManager.AllocatedCount: Integer;
begin
  Result := FAllocatedQuads.Count;
end;

function TQuadManager.SetSize(const AWidth, AHeight: Integer; const ACallBack: TOnMoveQuad): Boolean;
begin
  if (FWidth = AWidth) and (FHeight = AHeight) then Exit(False);
  Result := TryRepackToNewSize(AWidth, AHeight, ACallBack);
end;

function TQuadManager.Repack(const ACallBack: TOnMoveQuad): Boolean;
begin
  Result := TryRepackToNewSize(FWidth, FHeight, ACallBack);
end;

constructor TQuadManager.Create;
begin
  FAllocatedQuads := TQuadList.Create();
  FFreeQuads      := TQuadList.Create();
end;

destructor TQuadManager.Destroy;
var i: Integer;
begin
  inherited Destroy;
  Assert(FAllocatedQuads.Count = 0);
  for i := 0 to FFreeQuads.Count - 1 do
    FFreeQuads[i].Free;
  FFreeQuads.Clear();
end;

{ TQuadManager.TQuadRange }

function TQuadManager.TQuadRange.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj): longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TQuadManager.TQuadRange._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  {$IfDef FPC}
  Result := InterLockedIncrement(FRefCount);
  {$Else}
  Result := AtomicIncrement(FRefCount);
  {$EndIf}
end;

function TQuadManager.TQuadRange._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  {$IfDef FPC}
  Result := InterLockedDecrement(FRefCount);
  {$Else}
  Result := AtomicDecrement(FRefCount);
  {$EndIf}
  if Result = 0 then
    FOwner.OnQuadReleased(Self);
end;

function TQuadManager.TQuadRange.MinXY: TVec2i;
begin
  Result := FRect.LeftTop;
end;

function TQuadManager.TQuadRange.MaxXY: TVec2i;
begin
  Result := FRect.RightBottom;
end;

function TQuadManager.TQuadRange.Rect: TRectI;
begin
  Result := FRect;
end;

function TQuadManager.TQuadRange.Size: TVec2i;
begin
  Result := FRect.Size;
end;

function TQuadManager.TQuadRange.Width: Integer;
begin
  Result := FRect.Right - FRect.Left;
end;

function TQuadManager.TQuadRange.Height: Integer;
begin
  Result := FRect.Bottom - FRect.Top;
end;

function TQuadManager.TQuadRange.Area: Int64;
begin
  Result := (FRect.Right - FRect.Left) * (FRect.Bottom - FRect.Top);
end;

function TQuadManager.TQuadRange.UserData: Pointer;
begin
  Result := FUserData;
end;

constructor TQuadManager.TQuadRange.Create(AOwner: TQuadManager;
  const ARect: TRectI);
begin
  FOwner := AOwner;
  FRect := ARect;
end;

destructor TQuadManager.TQuadRange.Destroy;
begin
  inherited Destroy;
end;

{ TRangeManager.TAVLTreeInternal.TMyData }

constructor TRangeManager.TAVLTreeInternal.TRangeManagerAVLData.Create(ASize: Integer; AList: TList);
begin
  Size := ASize;
  List := AList;
end;

{ TRangeManager.TAVLTreeInternal }

function TRangeManager.TAVLTreeInternal.FindMaxNode(const data: TRangeManagerAVLData): TAVLTreeNode;
var c: Integer;
begin
  Result := FData.Root;
  if Result = nil then Exit;
  c := FData.OnCompare(Result.Data, data);
  while True do
  begin
      if c < 0 then
      begin
        if Assigned(Result.Right) then
        begin
          Result := Result.Right;
          c := FData.OnCompare(Result.Data, data);
        end
        else
          Exit(nil); //max not found
      end;
      if c > 0 then
      begin
        if Assigned(Result.Left) then
        begin
          c := FData.OnCompare(Result.Left.Data, data);
          if c >= 0 Then
            Result := Result.Left
          else
            Exit; //return current node
        end
        else
            Exit; //return current node
      end;
      if c = 0 then Exit; //return current node
  end;
end;

function TRangeManager.TAVLTreeInternal.TryGet(const ASize: Integer; out AValue: TList): Boolean;
var node: TAVLTreeNode;
begin
  Result := False;
  FDummy.Size := ASize;
  node := FData.Find( FDummy );
  if assigned(node) then
  begin
    AValue := TRangeManagerAVLData(node.Data).List;
    Result := True;
  end;
end;

function TRangeManager.TAVLTreeInternal.TryGetMax(const ASize: Integer; out AValue: TList): Boolean;
var
  node: TAVLTreeNode;
begin
  FDummy.Size := ASize;
  node := FindMaxNode(FDummy);
  if Assigned(node) then
  begin
    Result := True;
    AValue := TRangeManagerAVLData(node.Data).List;
  end
  else
    Result := False;
end;

function TRangeManager.TAVLTreeInternal.Add(const ASize: Integer; const AValue: TList): Boolean;
begin
  Result := Assigned( FData.Add(TRangeManagerAVLData.Create(ASize, AValue)) );
end;

function TRangeManager.TAVLTreeInternal.Del(const ASize: Integer): boolean;
var node: TAVLTreeNode;
begin
  FDummy.Size := ASize;
  node := FData.Find(FDummy);
  if Assigned(node) then
  begin
    TRangeManagerAVLData(node.Data).Free;
    FData.Delete(node);
    Result := True
  end
  else
    Result := False
end;

procedure TRangeManager.TAVLTreeInternal.Clear;
  procedure FreeLists(ANode: TAVLTreeNode);
  begin
    if ANode=nil then exit;
    FreeLists(ANode.Left);
    FreeLists(ANode.Right);
    if ANode.Data<>nil then
    begin
      TRangeManagerAVLData(ANode.Data).List.Free;
      TRangeManagerAVLData(ANode.Data).Free;
    end;
    ANode.Data:=nil;
  end;
begin
  FreeLists(FData.Root);
  FData.Clear;
end;

function CompareRangeManagerAVLData(Data1, Data2: Pointer): Integer;
var Size1, Size2: Integer;
begin
  Size1 := TRangeManager.TAVLTreeInternal.TRangeManagerAVLData(Data1).Size;
  Size2 := TRangeManager.TAVLTreeInternal.TRangeManagerAVLData(Data2).Size;
  Result := Size1 - Size2;
end;

constructor TRangeManager.TAVLTreeInternal.Create;
begin
  FData  := TAVLTree.Create(@CompareRangeManagerAVLData);
  FDummy := TRangeManagerAVLData.Create(0, nil);
end;

destructor TRangeManager.TAVLTreeInternal.Destroy;
begin
  Clear;
  FreeAndNil(FData);
  FreeAndNil(FDummy);
  inherited Destroy;
end;

{ TRangeManager }

procedure TRangeManager.PutAtFree(const Range: TMemRangeEx);
var lst: TList;
begin
  if FFreeRanges.TryGet(Range.Size, lst) then
  begin
    lst.Add(Range);
  end
  else
  begin
    lst := TList.Create;
    lst.Add(Range);
    FFreeRanges.Add(Range.Size, lst);
  end;
end;

procedure TRangeManager.PopFromFree(const Range: TMemRangeEx);
var lst: TList;
begin
  if FFreeRanges.TryGet(Range.Size, lst) then
  begin
    lst.Remove(Range);
    if lst.Count = 0 then
    begin
        FFreeRanges.Del(Range.Size);
        lst.Free;
    end;
  end
  else
    Assert(False);
end;

procedure TRangeManager.Dispose(Range: TMemRangeEx);
var tmpRange: TMemRangeEx;
begin
  Dec(FAllocatedSize, Range.Size);

  if assigned(Range.FPrev) then
      if Range.FPrev.FRefCount = 0 then
      begin
        PopFromFree(Range.FPrev);
        Inc(Range.FPrev.FSize, Range.Size);
        Range.FPrev.FNext := Range.FNext;
        if assigned(Range.FNext) then
            Range.FNext.FPrev := Range.FPrev
        else
            FLastRange := Range.FPrev;
        tmpRange := Range;
        Range := Range.FPrev;
        tmpRange.Free;
      end;

  if assigned(Range.FNext) then
      if Range.FNext.FRefCount = 0 then
      begin
        PopFromFree(Range.FNext);
        Inc(Range.FSize, Range.FNext.FSize);
        tmpRange := Range.FNext;
        Range.FNext := Range.FNext.FNext;
        if assigned(Range.FNext) then
            Range.FNext.FPrev := Range
        else
            FLastRange := Range;
        tmpRange.Free;
      end;

  PutAtFree(Range);

  {$IFDEF SlowAssertions}
  CheckInvariants;
  {$ENDIF}
end;

function TRangeManager.CompareSize(const Left, Right: Integer): Integer;
begin
  Result := Left - Right;
end;

procedure TRangeManager.CheckNodeInvariants(const ANode: TAVLTreeNode);
begin
  if ANode = nil then Exit;
  Assert(ANode.Data <> nil);
  CheckNodeInvariants(ANode.Left);
  CheckNodeInvariants(ANode.Right);
end;

procedure TRangeManager.CheckTreeInvariants;
begin
  CheckNodeInvariants(FFreeRanges.FData.Root);
end;

procedure TRangeManager.CheckInvariants;
var Range: TMemRangeEx;
    AllSize: Integer;
    AllocSize: Integer;
    FreeCount: Integer;
begin
  Range := FLastRange;
  AllSize := 0;
  AllocSize := 0;
  FreeCount := 0;
  while Assigned(Range) do
  begin
      if Assigned(Range.FPrev) then
      begin
        Assert(Range = Range.FPrev.FNext);
        Assert(Range.FOffset = Range.FPrev.FOffset + Range.FPrev.FSize);
      end;

      Assert(Range.FRefCount >= 0);
      if FTotalSize > 0 then
      begin
        Assert(Range.FOffset < FTotalSize);
        Assert(Range.FSize > 0);
      end;
      if Range.FRefCount > 0 then
        Inc(AllocSize, Range.FSize)
      else
        Inc(FreeCount);
      Inc(AllSize, Range.FSize);

      Range := Range.FPrev;
  end;
  Assert(AllSize = FTotalSize);
  Assert(AllocSize = FAllocatedSize);

  CheckTreeInvariants;
//  Assert(FreeCount >= FFreeRanges.GetNodeCount);
end;

function TRangeManager.GetName: string;
begin
  Result := FName;
end;

procedure TRangeManager.SetName(const AName: string);
begin
  FName := AName;
end;

function TRangeManager.Alloc(const ASize: Integer): IMemRange;
var lst: TList;
    srcRange, newRange: TMemRangeEx;
begin
  if FFreeRanges.TryGetMax(ASize, lst) then
  begin
    Inc(FAllocatedSize, ASize);

    srcRange := TMemRangeEx(lst.Items[lst.Count-1]);
    lst.Delete(lst.Count-1);
    if lst.Count = 0 then
    begin
      FFreeRanges.Del(srcRange.Size);
      lst.Free;
    end;

    if srcRange.Size = ASize then
    begin
      Result := srcRange;
      Exit;
    end
    else
    begin
      newRange := TMemRangeEx.Create;
      newRange.FManager := Self;
      newRange.FOffset := srcRange.FOffset;
      newRange.FSize := ASize;
      newRange.FPrev := srcRange.FPrev;
      if Assigned(newRange.FPrev) then newRange.FPrev.FNext := newRange;
      newRange.FNext := srcRange;
      srcRange.FPrev := newRange;
      Dec(srcRange.FSize, ASize);
      Inc(srcRange.FOffset, ASize);
      PutAtFree(srcRange);
      Result := newRange;
    end;
  end
  else
    Result := nil;

  {$IFDEF SlowAssertions}
  CheckInvariants;
  {$ENDIF}
end;

function TRangeManager.Size: Integer;
begin
  Result := FTotalSize;
end;

function TRangeManager.Allocated: Integer;
begin
  Result := FAllocatedSize;
end;

function TRangeManager.FreeSpace: Integer;
begin
  Result := FTotalSize - FAllocatedSize;
end;

procedure TRangeManager.AddSpace(const ASpace: Integer);
var newLast: TMemRangeEx;
begin
  if Assigned(FLastRange) then
  begin
    if FLastRange.FRefCount = 0 then
    begin
      //add new space at last range
      PopFromFree(FLastRange);
      Inc(FLastRange.FSize, ASpace);
      PutAtFree(FLastRange);
    end
    else
    begin
      //FLastRange already allocated
      newLast := TMemRangeEx.Create;
      newLast.FManager := Self;
      newLast.FSize := ASpace;
      newLast.FPrev := FLastRange;
      newLast.FOffset := FTotalSize;
      FLastRange.FNext := newLast;
      FLastRange := newLast;
      PutAtFree(FLastRange);
    end;
  end
  else
  begin
    //no any space, first initialization
    FLastRange := TMemRangeEx.Create;
    FLastRange.FManager := Self;
    FLastRange.FSize := ASpace;
    PutAtFree(FLastRange);
  end;

  Inc(FTotalSize, ASpace);
end;

procedure TRangeManager.Defrag;
var curr, tmpCurr: TMemRangeEx;
    {OldOffset, }GOffet: Integer;
begin
  if FreeSpace = 0 then Exit;
  if (FLastRange.FRefCount = 0) and (FreeSpace = FLastRange.FSize) then Exit;
  FFreeRanges.Clear;
  curr := FLastRange;
  FLastRange := nil;
  GOffet := FAllocatedSize;
  while assigned(curr) do
  begin
    if (FLastRange = nil) and (curr.FRefCount > 0) then FLastRange := curr;
    if curr.FRefCount = 0 then
    begin
      if Assigned(curr.FPrev) then curr.FPrev.FNext := curr.FNext;
      if Assigned(curr.FNext) then curr.FNext.FPrev := curr.FPrev;
      tmpCurr := curr;
      curr := curr.FPrev;
      tmpCurr.Free;
    end
    else
    begin
      Dec(GOffet, curr.Size);
      //if Assigned(CallBack) then
      //begin
      //  OldOffset := curr.FOffset;
      //  curr.FOffset := GOffet;
      //  CallBack(Self, curr, OldOffset);
      //end
      //else
        curr.FOffset := GOffet;

      curr := curr.FPrev;
    end;
  end;

  curr := TMemRangeEx.Create;
  curr.FManager := Self;
  curr.FOffset := FAllocatedSize;
  curr.FSize := FreeSpace;
  curr.FPrev := FLastRange;
  FLastRange.FNext := curr;
  FLastRange := curr;

  PutAtFree(FLastRange);

  {$IFDEF SlowAssertions}
  CheckInvariants;
  {$ENDIF}
end;

procedure TRangeManager.Trim;
var tmp: TMemRangeEx;
begin
  if FLastRange = nil then Exit;
  if FLastRange.FRefCount > 0 then Exit;
  PopFromFree(FLastRange);
  tmp := FLastRange;
  Dec(FTotalSize, FLastRange.FSize);
  FLastRange := FLastRange.FPrev;
  if assigned(FLastRange) then
    FLastRange.FNext := nil;
  tmp.Free;

  {$IFDEF SlowAssertions}
  CheckInvariants;
  {$ENDIF}
end;

constructor TRangeManager.Create;
begin
  FFreeRanges := TAVLTreeInternal.Create;
end;

destructor TRangeManager.Destroy;
begin
  Assert(FAllocatedSize = 0, FName);
  FreeAndNil(FFreeRanges);
  FreeAndNil(FLastRange);
  inherited Destroy;
end;

{ TMemRangeEx }

function TMemRangeEx.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TMemRangeEx._AddRef: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Inc(FRefCount);
  Result := FRefCount;
end;

function TMemRangeEx._Release: longint; {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := FRefCount;
  Dec(FRefCount);
  if FRefCount = 0 then
    FManager.Dispose(Self);
end;

function TMemRangeEx.Size: Integer;
begin
  Result := FSize;
end;

function TMemRangeEx.Offset: Integer;
begin
  Result := FOffset;
end;

{ TLayoutBuilder }

procedure TLayoutBuilder.Reset;
begin
  FCount := 0;
  FTotalSize := 0;
end;

function TLayoutBuilder.Add(Name: string; CompType: TComponentType; CompCount: Integer; DoNorm: Boolean; Offset: Integer): ILayoutBuilder;
begin
  Assert(FCount <= High(FFields), 'To huge record');

  FFields[FCount].Name := Name;
  FFields[FCount].CompCount := CompCount;
  FFields[FCount].CompType := CompType;
  FFields[FCount].DoNorm := DoNorm;
  if Offset < 0 then
    FFields[FCount].Offset := FTotalSize
  else
    FFields[FCount].Offset := Offset;
  FFields[FCount].Size := ComponentTypeSize(CompType)*CompCount;

  FTotalSize := Max(FTotalSize, FFields[FCount].Offset + FFields[FCount].Size); //to do align here
  Inc(FCount);

  Result := Self;
end;

function TLayoutBuilder.Finish(TotalSize: Integer): IDataLayout;
var i: Integer;
    newfields: TFieldInfoArr;
begin
  if TotalSize > 0 then FTotalSize := TotalSize;
  SetLength(newfields, FCount);
  for i := 0 to FCount - 1 do
    newfields[i] := FFields[i];
  Result := Create_DataLayout(newfields, FTotalSize);
end;

{ TIndices }

function TIndices.StrideSize: Integer;
begin
  if FIsDWord then
    Result := 4
  else
    Result := 2;
end;

procedure TIndices.Grow(Size: Integer);
begin
  if size > Length(FData) then
    SetLength(FData, Trunc(size * 2));
end;

function TIndices.IndexSize: Integer;
begin
  if IsDWord then Result := 4 else Result := 2;
end;

function TIndices.IndicesCount: Integer;
begin
  Result := FCount;
end;

function TIndices.GetCapacity: Integer;
begin
  Result := Length(FData) div StrideSize;
end;

function TIndices.GetIndex(index: Integer): Integer;
begin
  if StrideSize = 4 then
    Result := TIntArr(FData)[index]
  else
    Result := TWordArr(FData)[index];
end;

function TIndices.GetIsDWord: Boolean;
begin
  Result := FIsDWord;
end;

function TIndices.GetLine(index: Integer): TVec2i;
begin
  case FPrimType of
      ptLines    : Result := Vec(Ind[index div 2], Ind[index div 2 + 1]);
      ptLineStrip: Result := Vec(Ind[index], Ind[index + 1]);
  else
      Assert(False, 'wrong primitive type');
  end;
end;

function TIndices.PrimType: TPrimitiveType;
begin
  Result := FPrimType;
end;

function TIndices.GetTriangle(index: Integer): TVec3i;
begin
  case FPrimType of
      ptTriangles    : Result := Vec(Ind[index * 3], Ind[index * 3 + 1], Ind[index * 3 + 2]);
      ptTriangleStrip: Result := Vec(Ind[index], Ind[index + 1], Ind[index + 2]);
  else
      Assert(False, 'wrong primitive type');
  end;
end;

function TIndices.PrimCount: Integer;
begin
  case FPrimType of
    ptPoints       : Result := Count;
    ptLines        : Result := Count div 2;
    ptLineStrip    : Result := Count - 1;
    ptTriangles    : Result := Count div 3;
    ptTriangleStrip: Result := Count - 2;
  else
    Result := 0;
  end;
end;

procedure TIndices.SetCapacity(const Value: Integer);
begin
  SetLength(FData, Value * StrideSize);
end;

procedure TIndices.SetCount(const Value: Integer);
begin
  Grow(Value * StrideSize);
  FCount := Value;
end;

procedure TIndices.SetIndex(index: Integer; const Value: Integer);
begin
  if Value >= $ffff then IsDWord := True;

  if StrideSize = 4 then
    TIntArr(FData)[index] := Value
  else
    TWordArr(FData)[index] := Value;
end;

procedure TIndices.SetIsDWord(const Value: Boolean);
var old: IIndices;
    i: Integer;
begin
  if FIsDWord = Value then Exit;
  old := Clone;
  FIsDWord := Value;
  Count := old.Count;
  for i := 0 to Count - 1 do
    Ind[i] := old.Ind[i];
end;

procedure TIndices.SetLine(index: Integer; const Value: TVec2i);
begin
  case FPrimType of
      ptLines    : begin
                     Ind[index div 2] := Value.x;
                     Ind[index div 2 + 1] := Value.y;
                   end;
      ptLineStrip: begin
                     Ind[index] := Value.x;
                     Ind[index + 1] := Value.y;
                   end;
  else
    Assert(False, 'wrong primitive type');
  end;
end;

procedure TIndices.SetPrimitiveType(const Value: TPrimitiveType);
begin
  FPrimType := Value;
end;

procedure TIndices.SetTriangle(index: Integer; const Value: TVec3i);
begin
  case FPrimType of
      ptTriangles    : begin
                         Ind[index * 3] := Value.x;
                         Ind[index * 3 + 1] := Value.y;
                         Ind[index * 3 + 2] := Value.z;
                       end;
      ptTriangleStrip: begin
                         Ind[index] := Value.x;
                         Ind[index + 1] := Value.y;
                         Ind[index + 2] := Value.z;
                       end;
  else
      Assert(False, 'wrong primitive type');
  end;
end;

procedure TIndices.SetPrimitiveCount(const Value: Integer);
begin
  case FPrimType of
    ptPoints       : Count := Value;
    ptLines        : Count := Value * 2;
    ptLineStrip    : Count := Value + 1;
    ptTriangles    : Count := Value * 3;
    ptTriangleStrip: Count := Value + 2;
  else
    Assert(False, 'primitive type not defined');
  end;
end;

function TIndices.Clone: IIndices;
var obj: TIndices;
begin
  obj := TIndices.Create;
  Result := obj;
  Result.IsDWord := IsDWord;
  Result.PrimitiveType := PrimitiveType;
  Result.Count := Count;
  Move(FData[0], obj.FData[0], Min(Length(FData), Length(obj.FData)));
end;

procedure TIndices.Clear;
begin
  Count := 0;
end;

procedure TIndices.Add(NewIndex: Integer);
begin
  Count := Count + 1;
  Ind[Count - 1] := NewIndex;
end;

function TIndices.Data: TPointerData;
begin
  Result.data := Pointer(FData);
  Result.size := Count * StrideSize;
end;

{ TVerticesRec }

function TVerticesRec{$IfDef DCC}<TRec>{$EndIf}.VerticesCount: Integer;
begin
  Result := Count;
end;

function TVerticesRec{$IfDef DCC}<TRec>{$EndIf}.Layout: IDataLayout;
begin
  Result := FLayout;
end;

function TVerticesRec{$IfDef DCC}<TRec>{$EndIf}.Data: TPointerData;
begin
  Result.data := GetPItem(0);
  Result.size := Count * SizeOf(TRec);
end;

{$IfDef FPC}
constructor TVerticesRec.Create;
begin
  FLayout := TRec.Layout;
end;
{$EndIf}
{$IfDef DCC}
constructor TVerticesRec<TRec>.Create;
var ctx: TRttiContext;
    dataType: TRttiType;
    recType: TRttiRecordType;
    methods: System.TArray<TRttiMethod>;
    method: TRttiMethod;
begin
    ctx := TRttiContext.Create;
    try
        dataType := ctx.GetType(TypeInfo(TRec));
        Assert(dataType.TypeKind = tkRecord);
        recType := dataType as TRttiRecordType;
        method := recType.GetMethod('Layout');
        Assert(method <> nil);
        Assert(Length(method.GetParameters) = 0);
        Assert(method.ReturnType.Handle = TypeInfo(IDataLayout));
        FLayout := System.Rtti.Invoke(method.CodeAddress, nil, method.CallingConvention, method.ReturnType.Handle, method.IsStatic).AsInterface As IDataLayout;
    finally
        ctx.Free;
    end;
end;
{$EndIf}

constructor TVerticesRec{$IfDef DCC}<TRec>{$EndIf}.Create(Const ALayout: IDataLayout);
begin
  FLayout := ALayout;
end;

end.

