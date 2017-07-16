unit avPolygon;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils,
  mutils,
  superobject,
  intfUtils,
  avContnrs;

type
  TPathEdgeType = (etLine, erArc, etBezier2, etBezier3);

  IPathEdge = interface;

  { IPathVertex }

  IPathVertex = interface
  ['{F8310B64-F98D-460C-A7F3-E0DA7EB07326}']
    function GetCoord: TVec2;
    function GetLeftEdge: IPathEdge;
    function GetRightEdge: IPathEdge;
    procedure SetCoord(const AValue: TVec2);

    property Coord: TVec2 read GetCoord write SetCoord;

    property LeftEdge : IPathEdge read GetLeftEdge;
    property RightEdge: IPathEdge read GetRightEdge;
  end;

  { IPathEdge }

  IPathEdge = interface
  ['{791120DA-026D-4884-BA1B-2D743151A1BA}']
    function GetVEnd: IPathVertex;
    function GetVStart: IPathVertex;

    function EdgeType: TPathEdgeType;
    function SubPt(t: Single): TVec2;

    procedure LoadJSON(const AObj: ISuperObject);
    function  SaveJSON: ISuperObject;

    property VStart: IPathVertex read GetVStart;
    property VEnd  : IPathVertex read GetVEnd;
  end;

  { IPathEdge_Arc }

  IPathEdge_Arc = interface (IPathEdge)
  ['{F0A60BF6-934E-461B-9237-9934C1A5CB68}']
    function GetH: Single;
    procedure SetH(AValue: Single);

    property H: Single read GetH write SetH;
  end;

  { IPathEdge_Bezier2 }

  IPathEdge_Bezier2 = interface (IPathEdge)
  ['{9EDBC337-AF26-4204-8027-AEAF36AEE67E}']
    function GetCPt: TVec2;
    procedure SetCPt(AValue: TVec2);

    property CPt: TVec2 read GetCPt write SetCPt;
  end;

  { IPathEdge_Bezier3 }

  IPathEdge_Bezier3 = interface (IPathEdge)
  ['{9EDBC337-AF26-4204-8027-AEAF36AEE67E}']
    function GetCPt1: TVec2;
    function GetCPt2: TVec2;
    procedure SetCPt1(const AValue: TVec2);
    procedure SetCPt2(const AValue: TVec2);

    property CPt1: TVec2 read GetCPt1 write SetCPt1;
    property CPt2: TVec2 read GetCPt2 write SetCPt2;
  end;

  IVec2Arr = {$IfDef FPC}specialize{$EndIf}IArray<TVec2>;
  TIVec2Arr = {$IfDef FPC}specialize{$EndIf}TArray<TVec2>;

  { IPath }

  IPath = interface
  ['{05079DAB-2343-4794-B257-08278E578A3A}']
    function VertexCount: Integer;
    function Vertex(Index: Integer): IPathVertex;

    function AddEdge(NewType: TPathEdgeType): IPathEdge;
    function EdgeCount: Integer;
    function Edge(Index: Integer): IPathEdge;
    function IndexOfEdge(const AEdge: IPathEdge): Integer;
    procedure ChangeEdgeType(EdgeIndex: Integer; NewType: TPathEdgeType);
    procedure SplitEdge(AEdgeIndex: Integer; ASplitPoint: Single);
    procedure DelEdge(Index: Integer);

    function  Replicate: IPath;

    function  Tesselate(const ATolerance: Single): IVec2Arr;

    procedure LoadJSON(const AObj: ISuperObject);
    function  SaveJSON: ISuperObject;
  end;
  IPathArr = {$IfDef FPC}specialize{$EndIf}IArray<IPath>;
  TPathArr = {$IfDef FPC}specialize{$EndIf}TArray<IPath>;

function Create_IPath: IPath;

implementation

const
  cPolyEdgeTypeNames: array [TPathEdgeType] of string = (
    'Line',
    'Arc',
    'Bezier2',
    'Bezier3'
  );

type
  { TPath }

  TPath = class(TInterfacedObject, IPath)
  private type
    TPolyEdge = class;

    IPathEdge_Internal = interface(IPathEdge)
    ['{114DB96C-1147-434B-8B7F-116DE252529A}']
      procedure SetVEnd(const AValue: IPathVertex);
      procedure SetVStart(const AValue: IPathVertex);

      procedure TesselateTo(const APts: IVec2Arr; ATolerance: Single);

      function Obj: TPolyEdge;
      property VStart: IPathVertex read GetVStart write SetVStart;
      property VEnd  : IPathVertex read GetVEnd write SetVEnd;
    end;
    IEdgeArr = {$IfDef FPC}specialize{$EndIf}IArray<IPathEdge_Internal>;
    TEdgeArr = {$IfDef FPC}specialize{$EndIf}TArray<IPathEdge_Internal>;

    TPolyVertex = class(TInterfacedObject, IPathVertex)
    private
      FCoord : TVec2;
      FLeft  : IWeakRefIntf; //IPathEdge
      FRight : IWeakRefIntf; //IPathEdge
    protected
      function GetCoord: TVec2;
      function GetLeftEdge: IPathEdge;
      function GetRightEdge: IPathEdge;
      procedure SetCoord(const AValue: TVec2);

      property Coord: TVec2 read GetCoord write SetCoord;

      property LeftEdge : IPathEdge read GetLeftEdge;
      property RightEdge: IPathEdge read GetRightEdge;
    public
    end;

    { TPolyEdge }

    TPolyEdge = class(TInterfacedObject, IPathEdge, IPathEdge_Internal)
    private
      FOwner : TPath;
      FVStart: IPathVertex;
      FVEnd  : IPathVertex;
    private
      function GetVEnd: IPathVertex;
      function GetVStart: IPathVertex;
      procedure SetVEnd(const AValue: IPathVertex);
      procedure SetVStart(const AValue: IPathVertex);

      procedure TesselateTo(const APts: IVec2Arr; ATolerance: Single); virtual;

      function EdgeType: TPathEdgeType; virtual;
      function SubPt(t: Single): TVec2;

      procedure LoadJSON(const AObj: ISuperObject);
      function  SaveJSON: ISuperObject;

      property VStart: IPathVertex read GetVStart;
      property VEnd  : IPathVertex read GetVEnd;

      function Obj: TPolyEdge;
    protected
    public
      constructor Create(const AOwner: TPath; const AVStart, AVEnd: IPathVertex);
      destructor Destroy; override;
    end;

    { TPolyEdge_Bezier3 }

    TPolyEdge_Bezier3 = class(TPolyEdge, IPathEdge_Bezier3)
    private
      FCPt1: TVec2;
      FCPt2: TVec2;
      function GetCPt1: TVec2;
      function GetCPt2: TVec2;
      procedure SetCPt1(const AValue: TVec2);
      procedure SetCPt2(const AValue: TVec2);

      procedure TesselateTo(const APts: IVec2Arr; ATolerance: Single); override;

      property CPt1: TVec2 read GetCPt1 write SetCPt1;
      property CPt2: TVec2 read GetCPt2 write SetCPt2;
    end;

  private
    FEdges: IEdgeArr;

    function CreateEdge(NewType: TPathEdgeType; const VStart, VEnd: IPathVertex): IPathEdge_Internal;
  private
    function VertexCount: Integer;
    function Vertex(Index: Integer): IPathVertex;

    function AddEdge(NewType: TPathEdgeType): IPathEdge;
    function EdgeCount: Integer;
    function Edge(Index: Integer): IPathEdge;
    function IndexOfEdge(const AEdge: IPathEdge): Integer;
    procedure ChangeEdgeType(EdgeIndex: Integer; NewType: TPathEdgeType);
    procedure SplitEdge(AEdgeIndex: Integer; ASplitPoint: Single);
    procedure DelEdge(Index: Integer);

    function  Replicate: IPath;

    function  Tesselate(const ATolerance: Single): IVec2Arr;

    procedure LoadJSON(const AObj: ISuperObject);
    function  SaveJSON: ISuperObject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function Create_IPath: IPath;
begin
  Result := TPath.Create;
end;

function Vec2ToSO(const APt: TVec2): ISuperObject;
var arr: TSuperArray;
begin
  Result := TSuperObject.Create(stArray);
  arr := Result.AsArray;
  arr.D[0] := APt.x;
  arr.D[1] := APt.y;
end;

function SOToVec2_Def(const ASO: ISuperObject; const ADefValue: TVec2): TVec2;
var arr: TSuperArray;
begin
  if ASO = nil then Exit(ADefValue);
  if ASO.DataType <> stArray then Exit(ADefValue);
  arr := ASO.AsArray;
  if arr.Length <> 2 then Exit(ADefValue);
  Result.x := arr.D[0];
  Result.y := arr.D[1];
end;

function SOGetEdgeType_Def(const ASO: ISuperObject; const AName: string; const ADefValue: TPathEdgeType): TPathEdgeType;
var o: ISuperObject;
    s: string;
begin
  o := ASO.O[AName];
  if o = nil then Exit(ADefValue);
  s := o.AsString;
  for Result := Low(TPathEdgeType) to High(TPathEdgeType) do
    if cPolyEdgeTypeNames[Result] = s then Exit;
  Result := ADefValue;
end;

{ TPath.TPolyEdge_Bezier3 }

function TPath.TPolyEdge_Bezier3.GetCPt1: TVec2;
begin
  Result := FCPt1;
end;

function TPath.TPolyEdge_Bezier3.GetCPt2: TVec2;
begin
  Result := FCPt2;
end;

procedure TPath.TPolyEdge_Bezier3.SetCPt1(const AValue: TVec2);
begin
  FCPt1 := AValue;
end;

procedure TPath.TPolyEdge_Bezier3.SetCPt2(const AValue: TVec2);
begin
  FCPt2 := AValue;
end;

procedure TPath.TPolyEdge_Bezier3.TesselateTo(const APts: IVec2Arr; ATolerance: Single);

  procedure Split(const Pt1, Pt2, Pt3, Pt4: TVec2);

    function IsStraight: boolean;
    var seg: TSegment2D;
    begin
      seg.Pt1 := Pt1;
      seg.Pt2 := Pt4;
      Result := (Distance(Pt2, seg) < ATolerance) and (Distance(Pt3, seg) < ATolerance);
    end;
  var
      lpt2, lpt3: TVec2;
      rpt2, rpt3: TVec2;
      tmppt: TVec2;
      splitpt: TVec2;
  begin
    if IsStraight then
    begin
      APts.Add(Pt1);
      Exit;
    end;
    lpt2 := (Pt1 + Pt2) * 0.5;
    rpt3 := (Pt3 + Pt4) * 0.5;
    tmppt := (Pt2 + Pt3) * 0.5;
    lpt3 := (lpt2 + tmppt) * 0.5;
    rpt2 := (tmppt + rpt3) * 0.5;
    splitpt := (lpt3 + rpt2) * 0.5;
    Split(Pt1, lpt2, lpt3, splitpt);
    Split(splitpt, rpt2, rpt3, Pt4);
  end;

var s, e: TVec2;
begin
  s := VStart.Coord;
  e := VEnd.Coord;
  Split(s, s+FCPt1, e+FCPt2, e);
end;

{ TPolyLine }

function TPath.CreateEdge(NewType: TPathEdgeType; const VStart, VEnd: IPathVertex): IPathEdge_Internal;
begin
  case NewType of
    etLine:
        Result := TPolyEdge.Create(Self, Vstart, Vend);
    etBezier3:
        Result := TPolyEdge_Bezier3.Create(Self, VStart, VEnd);
  else
    Assert(False);
    Result := nil;
  end;
end;

function TPath.VertexCount: Integer;
begin
  Result := FEdges.Count + 1;
  if Result = 1 then Result := 0;
end;

function TPath.Vertex(Index: Integer): IPathVertex;
var n: Integer;
begin
  n := VertexCount;
  Index := Index mod n;
  if Index < 0 then Inc(Index, n);
  if Index = n - 1 then
    Result := FEdges[Index-1].VEnd
  else
    Result := FEdges[Index].VStart;
end;

function TPath.AddEdge(NewType: TPathEdgeType): IPathEdge;
var Vstart: IPathVertex;
    Vend  : IPathVertex;
    newEdge: IPathEdge_Internal;
begin
  if FEdges.Count = 0 then
    Vstart := TPolyVertex.Create
  else
    Vstart := FEdges.Last.VEnd;
  Vend := TPolyVertex.Create;
  Vend.Coord := Vstart.Coord;

  newEdge := CreateEdge(NewType, Vstart, Vend);
  Assert(newEdge <> nil);
  FEdges.Add(newEdge);
  Result := newEdge;
end;

function TPath.EdgeCount: Integer;
begin
  Result := FEdges.Count;
end;

function TPath.Edge(Index: Integer): IPathEdge;
begin
  Result := FEdges[Index];
end;

function TPath.IndexOfEdge(const AEdge: IPathEdge): Integer;
begin
  Result := FEdges.IndexOf((AEdge as IPathEdge_Internal));
end;

procedure TPath.ChangeEdgeType(EdgeIndex: Integer; NewType: TPathEdgeType);
var repEdge: IPathEdge;
begin
  repEdge := FEdges[EdgeIndex];
  FEdges[EdgeIndex] := CreateEdge(NewType, repEdge.VStart, repEdge.VEnd);
end;

procedure TPath.SplitEdge(AEdgeIndex: Integer; ASplitPoint: Single);
begin
  Assert(False, 'todo');
end;

procedure TPath.DelEdge(Index: Integer);
var currEdge: IPathEdge;
    nextEdge: IPathEdge;
begin
  currEdge := FEdges[Index];
  if Index < FEdges.Count-1 then
  begin
    nextEdge := FEdges[Index+1];
    (nextEdge as IPathEdge_Internal).VStart := currEdge.VStart;
  end;
  (currEdge as IPathEdge_Internal).Obj.FOwner := nil;
  FEdges.Delete(Index);
end;

function TPath.Replicate: IPath;
begin
  Result := TPath.Create;
  Result.LoadJSON(SaveJSON());
end;

function TPath.Tesselate(const ATolerance: Single): IVec2Arr;
var
  i: Integer;
begin
  Result := TIVec2Arr.Create();
  for i := 0 to FEdges.Count - 1 do
  begin
    Result.Add(FEdges[i].VStart.Coord);
    FEdges[i].TesselateTo(Result, ATolerance);
  end;
  if FEdges.Count > 0 then
    Result.Add(FEdges.Last.VEnd.Coord);
end;

procedure TPath.LoadJSON(const AObj: ISuperObject);
var i: Integer;
    edges: ISuperObject;
    edgesArr: TSuperArray;
    edgeType : TPathEdgeType;
begin
  FEdges.Clear();
  edges := AObj.O['Edges'];
  if edges = nil then Exit;
  if edges.DataType <> stArray then Exit;
  edgesArr := edges.AsArray;
  for i := 0 to edgesArr.Length - 1 do
  begin
    edgeType := SOGetEdgeType_Def(edgesArr.O[i], 'EType', etLine);
    AddEdge(edgeType).LoadJSON(edgesArr.O[i]);
  end;
end;

function TPath.SaveJSON: ISuperObject;
var edges: ISuperObject;
    edgesArr: TSuperArray;
    i: Integer;
begin
  Result := SO('{}');
  edges := TSuperObject.Create(stArray);
  Result.O['Edges'] := edges;
  edgesArr := edges.AsArray;
  for i := 0 to FEdges.Count - 1 do
    edgesArr.O[i] := FEdges[i].SaveJSON;
end;

constructor TPath.Create;
begin
  FEdges := TEdgeArr.Create;
end;

destructor TPath.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  for i := 0 to FEdges.Count - 1 do
    FEdges[i].Obj.FOwner := nil;
  FEdges.Clear();
end;

{ TPolyLine.TPolyVertex }

function TPath.TPolyVertex.GetCoord: TVec2;
begin
  Result := FCoord;
end;

function TPath.TPolyVertex.GetLeftEdge: IPathEdge;
begin
  Result := nil;
  if FLeft = nil then Exit;
  Result := FLeft.Intf as IPathEdge;
end;

function TPath.TPolyVertex.GetRightEdge: IPathEdge;
begin
  Result := nil;
  if FRight = nil then Exit;
  Result := FRight.Intf as IPathEdge;
end;

procedure TPath.TPolyVertex.SetCoord(const AValue: TVec2);
begin
  FCoord := AValue;
end;

{ TPolyLine.TPolyEdge }

function TPath.TPolyEdge.GetVEnd: IPathVertex;
begin
  Result := FVEnd;
end;

function TPath.TPolyEdge.GetVStart: IPathVertex;
begin
  Result := FVStart;
end;

procedure TPath.TPolyEdge.SetVEnd(const AValue: IPathVertex);
begin
  FVEnd := AValue;
end;

procedure TPath.TPolyEdge.SetVStart(const AValue: IPathVertex);
begin
  FVStart := AValue;
end;

procedure TPath.TPolyEdge.TesselateTo(const APts: IVec2Arr; ATolerance: Single);
begin
  //
end;

function TPath.TPolyEdge.EdgeType: TPathEdgeType;
begin
  Result := TPathEdgeType.etLine;
end;

function TPath.TPolyEdge.SubPt(t: Single): TVec2;
begin
  if t < 0.5 then
    Result := FVStart.Coord
  else
    Result := FVEnd.Coord;
end;

procedure TPath.TPolyEdge.LoadJSON(const AObj: ISuperObject);
begin
  VStart.Coord := SOToVec2_Def(AObj.O['Start'], Vec(0,0));
  VEnd.Coord := SOToVec2_Def(AObj.O['End'], Vec(0,0));
end;

function TPath.TPolyEdge.SaveJSON: ISuperObject;
begin
  Result := SO('{}');
  Result.S['EType'] := cPolyEdgeTypeNames[EdgeType];
  Result.O['Start'] := Vec2ToSO(FVStart.Coord);
  Result.O['End'] := Vec2ToSO(FVStart.Coord);
end;

function TPath.TPolyEdge.Obj: TPolyEdge;
begin
  Result := Self;
end;

constructor TPath.TPolyEdge.Create(const AOwner: TPath; const AVStart, AVEnd: IPathVertex);
begin
  FOwner := AOwner;
  FVStart := AVStart;
  FVEnd := AVEnd;
end;

destructor TPath.TPolyEdge.Destroy;
begin
  inherited Destroy;
end;

end.

