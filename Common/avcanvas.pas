unit avCanvas;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, intfUtils, avBase, avRes, mutils, avTypes, avContnrs, avTess;

type
  TPolyEdgeType = (etLine, erArc, etBezier2, etBezier3);

  IPolyEdge = interface;

  { IPolyVertex }

  IPolyVertex = interface
  ['{F8310B64-F98D-460C-A7F3-E0DA7EB07326}']
    function GetCoord: TVec2;
    function GetLeftEdge: IPolyEdge;
    function GetRightEdge: IPolyEdge;
    procedure SetCoord(AValue: TVec2);

    property Coord: TVec2 read GetCoord write SetCoord;

    property LeftEdge : IPolyEdge read GetLeftEdge;
    property RightEdge: IPolyEdge read GetRightEdge;
  end;

  { IPolyEdge }

  IPolyEdge = interface
  ['{791120DA-026D-4884-BA1B-2D743151A1BA}']
    function GetVEnd: IPolyVertex;
    function GetVStart: IPolyVertex;
    procedure SetVEnd(AValue: IPolyVertex);
    procedure SetVStart(AValue: IPolyVertex);

    function EdgeType: TPolyEdgeType;
    function SubPt(t: Single): TVec2;

    property VStart: IPolyVertex read GetVStart write SetVStart;
    property VEnd  : IPolyVertex read GetVEnd write SetVEnd;
  end;

  { IPolyEdge_Arc }

  IPolyEdge_Arc = interface (IPolyEdge)
  ['{F0A60BF6-934E-461B-9237-9934C1A5CB68}']
    function GetH: Single;
    procedure SetH(AValue: Single);

    property H: Single read GetH write SetH;
  end;

  { IPolyEdge_Bezier2 }

  IPolyEdge_Bezier2 = interface (IPolyEdge)
  ['{9EDBC337-AF26-4204-8027-AEAF36AEE67E}']
    function GetCPt: TVec2;
    procedure SetCPt(AValue: TVec2);

    property CPt: TVec2 read GetCPt write SetCPt;
  end;

  { IPolyEdge_Bezier3 }

  IPolyEdge_Bezier3 = interface (IPolyEdge)
  ['{9EDBC337-AF26-4204-8027-AEAF36AEE67E}']
    function GetCPt1: TVec2;
    function GetCPt2: TVec2;
    procedure SetCPt1(AValue: TVec2);
    procedure SetCPt2(AValue: TVec2);

    property CPt1: TVec2 read GetCPt1 write SetCPt1;
    property CPt2: TVec2 read GetCPt2 write SetCPt2;
  end;

  { IPolyLine }

  IPolyLine = interface
  ['{05079DAB-2343-4794-B257-08278E578A3A}']
    function VertexCount: Integer;
    function Vertex(Index: Integer): IPolyVertex;
    function IndexOfVertex(const AVertex: IPolyVertex): Integer;
    procedure AddVertex(const pt: TVec2; NewEdgeType: TPolyEdgeType = etLine);
    procedure DelVertex(Index: Integer; NewEdgeType: TPolyEdgeType = etLine);
    procedure InsertVertex(const pt: TVec2; Index: Integer); overload;
    procedure InsertVertex(const pt: TVec2; Index: Integer; PrevEdgeType, NextEdgeType: TPolyEdgeType); overload;

    function EdgeCount: Integer;
    function Edge(Index: Integer): IPolyEdge;
    function IndexOfEdge(const AEdge: IPolyEdge): Integer;
    procedure ChangeEdgeType(EdgeIndex: Integer; NewType: TPolyEdgeType);
    procedure DelEdge(Index: Integer);
  end;

  { TLinePointVertex }

  TLinePointVertex = packed record
    Coords      : TVec4; //xy - start point, zw - end point
    Normals     : TVec4; //xy - normal at start point, zw - normal at end point
    Width       : TVec2; //x - real width, y - minimal width in pixels
    HintingAlign: TVec4;
    class function Layout: IDataLayout; static;
  end;
  TLinePointVertices = specialize TVerticesRec<TLinePointVertex>;
  ILinePointVertices = specialize IArray<TLinePointVertex>;

  {$SCOPEDENUMS ON}
  TPenAlign = (Center, Left, Right);
  TPenHintingStyle = (Vertical, Horizontal, PostHinting);
  TPenHinting = set of TPenHintingStyle;
  {$SCOPEDENUMS OFF}

  { TPenStyle }

  TPenStyle = class (TPersistent)
  private
    FAlign: TPenAlign;
    FHinting: TPenHinting;
    FMinPixWidth: Integer;
    FWidth: Single;
    procedure SetAlign(AValue: TPenAlign);
    procedure SetHinting(AValue: TPenHinting);
    procedure SetMinPixWidth(AValue: Integer);
    procedure SetWidth(AValue: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Width: Single read FWidth write SetWidth;
    property Hinting: TPenHinting read FHinting write SetHinting;
    property Align: TPenAlign read FAlign write SetAlign;

    property MinPixWidth: Integer read FMinPixWidth write SetMinPixWidth;
  end;

  { TavCanvasCommonData }

  TavCanvasCommonData = class (TavRes)
  private
    FLineQuad: TavVB;
    FLineProg: TavProgram;
    FWndMatrix: TMat4;
    procedure SetWndMatrix(AValue: TMat4);
  protected
    procedure AfterInit3D; override;
    procedure BeforeFree3D; override;
  public
    property WndMatrix: TMat4 read FWndMatrix write SetWndMatrix;

    procedure ReloadShaders;
    property LineQuad: TavVB      read FLineQuad;
    property LineProg: TavProgram read FLineProg;

    constructor Create(AParent: TavObject); overload; override;
  end;

  { TavCanvas }

  TavCanvas = class(TavMainRenderChild)
  private
    FCommonData: IWeakRef;
    function GetCommonData: TavCanvasCommonData;
    property CommonData: TavCanvasCommonData read GetCommonData;
  private
    FPen: TPenStyle;

    FLineData: ILinePointVertices;
    FValid: Boolean;
    FVBLines : TavVB;

    procedure SetPen(AValue: TPenStyle);
    procedure AddLineSegment(Coords, Normals: TVec4);
    procedure FillSegmentByPen(var Seg: TLinePointVertex);

    procedure SetValid(AValue: Boolean);
  public
    property Pen: TPenStyle read FPen write SetPen;
    property Valid: Boolean read FValid write SetValid;

    procedure Clear;
    procedure Rectangle(Left, Top, Right, Bottom: Single); overload;
    procedure Rectangle(LeftTop, RightBottom: TVec2); overload;
    procedure Draw(const ATransform: TMat4; const APixelToUnit: Single);

    constructor Create(AParent: TavObject); overload; override;
    destructor Destroy; override;
  end;

function GetCanvasCommonData(const RenderMain: TavMainRender): TavCanvasCommonData;

implementation

//{$R '..\Canvas_Shaders\Canvas_Shaders.rc'}

uses Math;

const
  NAME_TavCanvasCommonData = 'TavCanvasCommonData';

type
  { TLineQuadVertex }

  TLineQuadVertex = packed record
    quadCoord: TVec2;
    class function Layout: IDataLayout; static;
  end;
  TLineQuadVertices = specialize TVerticesRec<TLineQuadVertex>;
  ILineQuadVertices = specialize IArray<TLineQuadVertex>;

function GetCanvasCommonData(const RenderMain: TavMainRender): TavCanvasCommonData;
begin
  Result := TavCanvasCommonData(RenderMain.FindChild(NAME_TavCanvasCommonData));
  if Result = nil then
  begin
    Result := TavCanvasCommonData.Create(RenderMain);
    Result.Name := NAME_TavCanvasCommonData;
  end;
end;

{ TLinePointVertex }

class function TLinePointVertex.Layout: IDataLayout;
begin
  Result := LB.Add('Coords', ctFloat, 4).
               Add('Normals', ctFloat, 4).
               Add('Width', ctFloat, 2).
               Add('HintingAlign', ctFloat, 4).
               Finish(SizeOf(TLinePointVertex));
end;

{ TLineQuadVertex }

class function TLineQuadVertex.Layout: IDataLayout;
begin
  Result := LB.Add('quadCoord', ctFloat, 2).Finish(SizeOf(TLineQuadVertex));
end;

{ TavCanvasCommonData }

procedure TavCanvasCommonData.SetWndMatrix(AValue: TMat4);
begin
  if FWndMatrix = AValue then Exit;
  FWndMatrix := AValue;
end;

procedure TavCanvasCommonData.AfterInit3D;
begin
  inherited AfterInit3D;
  ReloadShaders;
end;

procedure TavCanvasCommonData.BeforeFree3D;
begin
  inherited BeforeFree3D;
end;

procedure TavCanvasCommonData.ReloadShaders;
const LOADFROMRES = False;
      DIR = 'E:\Projects\AvalancheProject\Canvas_Shaders\!Out\';
begin
  FLineProg.Load('CanvasLine', LOADFROMRES, DIR);
end;

constructor TavCanvasCommonData.Create(AParent: TavObject);
var Vert: ILineQuadVertices;
    V: TLineQuadVertex;
begin
  inherited Create(AParent);
  FLineQuad := TavVB.Create(Self);
  FLineProg := TavProgram.Create(Self);

  Vert := TLineQuadVertices.Create;
  V.quadCoord := Vec(0.0, -1.0); Vert.Add(V);
  V.quadCoord := Vec(0.0,  1.0); Vert.Add(V);
  V.quadCoord := Vec(1.0, -1.0); Vert.Add(V);
  V.quadCoord := Vec(1.0,  1.0); Vert.Add(V);
  FLineQuad.Vertices := Vert as IVerticesData;
  FLineQuad.PrimType := ptTriangleStrip;
end;

{ TavCanvas }

function TavCanvas.GetCommonData: TavCanvasCommonData;
begin
  if FCommonData = nil then
      FCommonData := GetCanvasCommonData(Main).WeakRef;
  Result := TavCanvasCommonData(FCommonData.Obj);
end;

procedure TavCanvas.SetPen(AValue: TPenStyle);
begin
  if FPen = AValue then Exit;
  FPen.Assign(AValue);
end;

procedure TavCanvas.AddLineSegment(Coords, Normals: TVec4);
var Seg: TLinePointVertex;
begin
  FillSegmentByPen(Seg);
  Seg.Coords := Coords;
  Seg.Normals := Normals;
  FLineData.Add(Seg);
  FVBLines.Invalidate;
end;

procedure TavCanvas.FillSegmentByPen(var Seg: TLinePointVertex);
begin
  Seg.Width := Vec(Pen.Width, Pen.MinPixWidth);
  if TPenHintingStyle.Horizontal in Pen.Hinting then
    Seg.HintingAlign.x := 1
  else
    Seg.HintingAlign.x := 0;

  if TPenHintingStyle.Vertical in Pen.Hinting then
    Seg.HintingAlign.y := 1
  else
    Seg.HintingAlign.y := 0;

  if TPenHintingStyle.PostHinting in Pen.Hinting then
    Seg.HintingAlign.z := 1
  else
    Seg.HintingAlign.z := 0;

  case Pen.Align of
    TPenAlign.Center: Seg.HintingAlign.w := 0;
    TPenAlign.Left  : Seg.HintingAlign.w := 1;
    TPenAlign.Right : Seg.HintingAlign.w := -1;
  end;
end;

procedure TavCanvas.SetValid(AValue: Boolean);
begin
  if FValid = AValue then Exit;
  FValid := AValue;
end;

procedure TavCanvas.Clear;
begin
  FLineData.Clear();
  FVBLines.Invalidate;
end;

procedure TavCanvas.Rectangle(Left, Top, Right, Bottom: Single);
var Seg: TLinePointVertex;
begin
  FillSegmentByPen(Seg);

  Seg.Coords.xy := Vec(Left, Top);
  Seg.Coords.zw := Vec(Right, Top);
  Seg.Normals.xy := (Vec(1.0, 1.0));
  Seg.Normals.zw := (Vec(-1.0, 1.0));
  FLineData.Add(Seg);

  Seg.Coords.xy := Seg.Coords.zw;
  Seg.Normals.xy := Seg.Normals.zw;
  Seg.Coords.zw := Vec(Right, Bottom);
  Seg.Normals.zw := (Vec(-1.0, -1.0));
  FLineData.Add(Seg);

  Seg.Coords.xy := Seg.Coords.zw;
  Seg.Normals.xy := Seg.Normals.zw;
  Seg.Coords.zw := Vec(Left, Bottom);
  Seg.Normals.zw := (Vec(1.0, -1.0));
  FLineData.Add(Seg);

  Seg.Coords.xy := Seg.Coords.zw;
  Seg.Normals.xy := Seg.Normals.zw;
  Seg.Coords.zw := Vec(Left, Top);
  Seg.Normals.zw := (Vec(1.0, 1.0));
  FLineData.Add(Seg);

  FVBLines.Invalidate;
end;

procedure TavCanvas.Rectangle(LeftTop, RightBottom: TVec2);
begin
  Rectangle(LeftTop.x, LeftTop.y, RightBottom.x, RightBottom.y);
end;

procedure TavCanvas.Draw(const ATransform: TMat4; const APixelToUnit: Single);
var prog: TavProgram;
begin
  prog := CommonData.LineProg;
  prog.Select;
  prog.SetAttributes(CommonData.LineQuad, nil, FVBLines);
  prog.SetUniform('UIMatrix', ATransform);
  prog.SetUniform('UIMatrixInverse', Inv(ATransform) );
  prog.SetUniform('ViewPortSize', Main.States.Viewport.Size);
  prog.SetUniform('PixelToUnit', APixelToUnit);
  prog.Draw(FVBLines.Vertices.VerticesCount);
end;

constructor TavCanvas.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  FPen := TPenStyle.Create;
  FPen.Width := 0.1;

  FLineData := TLinePointVertices.Create;
  FVBLines := TavVB.Create(CommonData);
  FVBLines.Vertices := FLineData as IVerticesData;
end;

destructor TavCanvas.Destroy;
begin
  FreeAndNil(FPen);
  inherited Destroy;
end;

{ TPenStyle }

procedure TPenStyle.SetWidth(AValue: Single);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
end;

procedure TPenStyle.AssignTo(Dest: TPersistent);
var PenDest: TPenStyle absolute Dest;
begin
  Assert(Dest is TPenStyle);
  PenDest.FAlign := FAlign;
  PenDest.FHinting := FHinting;
  PenDest.FMinPixWidth := FMinPixWidth;
  PenDest.FWidth := FWidth;
end;

procedure TPenStyle.SetHinting(AValue: TPenHinting);
begin
  if FHinting = AValue then Exit;
  FHinting := AValue;
end;

procedure TPenStyle.SetMinPixWidth(AValue: Integer);
begin
  if FMinPixWidth = AValue then Exit;
  FMinPixWidth := AValue;
end;

procedure TPenStyle.SetAlign(AValue: TPenAlign);
begin
  if FAlign = AValue then Exit;
  FAlign := AValue;
end;

end.
