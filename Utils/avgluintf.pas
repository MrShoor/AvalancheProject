unit avGLUIntf;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avGLU, mutils;

type
  TContours = array of TVec2Arr;

function PolyToConvex(const APolyLine: TVec2Arr): TContours; overload;
function RemoveSelfIntersection(const APolyLine: TVec2Arr): TContours;

implementation

uses avContnrs;

const
  PolyPartitionLib = 'PolyPartitionLib.dll';

type
  TOnNewConvex = procedure (Convex: PVec2; ConvexLen: Integer; UserData: Pointer);

procedure ConvexPartitionHM(Contour: PVec2; ContourLen: Integer; CallBack: TOnNewConvex; UserData: Pointer); external PolyPartitionLib;

type
  ETessError = class(Exception)
  end;

type
  PVec2_MixPairs = array [0..3] of PVec2;

  { TTess }

  TTess = class
  private type
    IV2Arr = {$IfDef FPC}specialize{$EndIf}IArray<TVec2>;
    TV2Arr = {$IfDef FPC}specialize{$EndIf}TArray<TVec2>;
  private
    FTess: PGLUTesselator;
    FMode: TGLenum;
    FBuildBuffer: IV2Arr;
    FNewVert: IV2Arr;
    FOutFans: TContours;
    FOutContours: TContours;

    procedure cbBegin(AMode: TGLenum);
    procedure cbEdgeFlag(AEdge: TGLboolean);
    procedure cbVertex(AUserData: PVec2);
    procedure cbEnd();
    procedure cbCombine(newPt: TGLVectord3; vertexData: PVec2_MixPairs; weights: TVec4; out NewVertex: PVec2);
    procedure cbError(errorNo: TGLenum);

    procedure CustomExecute(const ALine: TVec2Arr);
  public
    function Execute(const ALine: TVec2Arr): TContours;
    function ExecuteToLoops(const APolyLine: TVec2Arr): TContours;

    constructor Create();
    destructor Destroy; override;
  end;

  { TConvexPartitioning }

  TConvexPartitioning = class
  private
    FOutData: TContours;
    procedure OnContour(Convex: PVec2; ConvexLen: Integer);
  public
    function GetData: TContours;
    function Execute(const ALine: TVec2Arr): TContours;
  end;

procedure ConvexPartitioning_CB(Convex: PVec2; ConvexLen: Integer; UserData: Pointer);
begin
  TConvexPartitioning(UserData).OnContour(Convex, ConvexLen);
end;

procedure TTess_cbBegin(AMode: TGLenum; UserData: TTess);
begin
  UserData.cbBegin(AMode);
end;

procedure TTess_cbEdgeFlag(AEdge: TGLboolean; UserData: TTess);
begin
  UserData.cbEdgeFlag(AEdge);
end;

procedure TTess_cbVertex(UserData1: PVec2; UserData2: TTess);
begin
  UserData2.cbVertex(UserData1);
end;

procedure TTess_cbEnd(UserData: TTess);
begin
  UserData.cbEnd();
end;

procedure TTess_cbCombine(newPt: TGLVectord3; vertexData: PVec2_MixPairs; weights: TVec4; out NewVertex: PVec2; UserData: TTess);
begin
  UserData.cbCombine(newPt, vertexData, weights, NewVertex);
end;

procedure TTess_cbError(errorNo: TGLenum; UserData: TTess);
begin
  UserData.cbError(errorNo);
end;

function PolyToConvex(const ALine: array of TVec2Arr): TContours; overload;
var ttf: TConvexPartitioning;
    i: Integer;
begin
  ttf := TConvexPartitioning.Create();
  try
    for i := 0 to Length(ALine) - 1 do
      ttf.Execute(ALine[i]);
    Result := ttf.GetData;
  finally
    ttf.Free;
  end;
end;

function PolyToConvex(const APolyLine: TVec2Arr): TContours;
begin
  Result := PolyToConvex(RemoveSelfIntersection(APolyLine));
end;

function RemoveSelfIntersection(const APolyLine: TVec2Arr): TContours;
var looper: TTess;
begin
  looper := TTess.Create();
  try
    Result := looper.ExecuteToLoops(APolyLine);
  finally
    looper.Free;
  end;
end;

{ TConvexPartitioning }

procedure TConvexPartitioning.OnContour(Convex: PVec2; ConvexLen: Integer);
var NewConvex: TVec2Arr;
begin
  if ConvexLen < 2 then Exit;
  SetLength(NewConvex, ConvexLen);
  Move(Convex^, NewConvex[0], ConvexLen*SizeOf(TVec2));
  SetLength(FOutData, Length(FOutData) + 1);
  FOutData[High(FOutData)] := NewConvex;
end;

function TConvexPartitioning.GetData: TContours;
begin
  Result := FOutData;
end;

function TConvexPartitioning.Execute(const ALine: TVec2Arr): TContours;
begin
  ConvexPartitionHM(@ALine[0], Length(ALine), @ConvexPartitioning_CB, Self);
  Result := FOutData;
end;

{ TTessToFans }

procedure TTess.cbBegin(AMode: TGLenum);
begin
  FMode := AMode;
  FBuildBuffer.Clear();
end;

procedure TTess.cbEdgeFlag(AEdge: TGLboolean);
begin
  Assert(not AEdge);
end;

procedure TTess.cbVertex(AUserData: PVec2);
begin
  FBuildBuffer.Add(AUserData^);
end;

procedure TTess.cbEnd;
var newFan, newCntr: TVec2Arr;
    i, j: Integer;
begin
  case FMode of
    GLU_TRIANGLE_FAN: begin
      SetLength(newFan, FBuildBuffer.Count);
      for i := 0 to FBuildBuffer.Count - 1 do
        newFan[i] := FBuildBuffer.Item[i];
      SetLength(FOutFans, Length(FOutFans)+1);
      FOutFans[High(FOutFans)] := newFan;
    end;
    GLU_TRIANGLE_STRIP: begin
      j := Length(FOutFans);
      SetLength(FOutFans, Length(FOutFans) + FBuildBuffer.Count - 2);
      for i := 2 to FBuildBuffer.Count - 1 do
      begin
        SetLength(newFan, 3);
        newFan[0] := FBuildBuffer.Item[i-2];
        newFan[1] := FBuildBuffer.Item[i-1];
        newFan[2] := FBuildBuffer.Item[i];
        FOutFans[j] := newFan;
        Inc(j);
      end;
    end;
    GLU_TRIANGLES: begin
      j := Length(FOutFans);
      SetLength(FOutFans, Length(FOutFans) + FBuildBuffer.Count div 3);
      for i := 0 to (FBuildBuffer.Count div 3) - 1 do
      begin
        SetLength(newFan, 3);
        newFan[0] := FBuildBuffer.Item[i*3];
        newFan[1] := FBuildBuffer.Item[i*3+1];
        newFan[2] := FBuildBuffer.Item[i*3+2];
        FOutFans[j] := newFan;
        Inc(j);
      end;
    end;
    GLU_LINE_LOOP: begin
      SetLength(newCntr, FBuildBuffer.Count);
      for i := 0 to FBuildBuffer.Count - 1 do
        newCntr[i] := FBuildBuffer[i];
      SetLength(FOutContours, Length(FOutContours)+1);
      FOutContours[High(FOutContours)] := newCntr;
    end
  else
    Assert(False, 'not supported yet');
  end;
end;

procedure TTess.cbCombine(newPt: TGLVectord3; vertexData: PVec2_MixPairs; weights: TVec4; out NewVertex: PVec2);
var n: Integer;
begin
  n := FNewVert.Add(Vec(newPt[0], newPt[1]));
  NewVertex := FNewVert.PItem[n];
end;

procedure TTess.cbError(errorNo: TGLenum);
begin
  raise ETessError.Create('Tess error #'+IntToStr(errorNo));
end;

procedure TTess.CustomExecute(const ALine: TVec2Arr);
var d: TGLArrayd3;
    i: Integer;
begin
  d[2] := 0;
  gluTessBeginPolygon(FTess, Self);
  gluTessBeginContour(FTess);
  for i := 0 to Length(ALine) - 1 do
  begin
    d[0] := ALine[i].x;
    d[1] := ALine[i].y;
    gluTessVertex(FTess, d, @ALine[i]);
  end;
  gluTessEndContour(FTess);
  gluTessEndPolygon(FTess);
end;

function TTess.Execute(const ALine: TVec2Arr): TContours;
begin
  if FBuildBuffer.Capacity < Length(ALine) then FBuildBuffer.Capacity := Length(ALine);

  gluTessProperty(FTess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_NONZERO);
  gluTessProperty(FTess, GLU_TESS_BOUNDARY_ONLY, GL_FALSE);

  CustomExecute(ALine);

  Result := FOutFans;
end;

function TTess.ExecuteToLoops(const APolyLine: TVec2Arr): TContours;
begin
  if FBuildBuffer.Capacity < Length(APolyLine) then FBuildBuffer.Capacity := Length(APolyLine);
  gluTessProperty(FTess, GLU_TESS_BOUNDARY_ONLY, GL_TRUE);
  CustomExecute(APolyLine);
  Result := FOutContours;
end;

constructor TTess.Create;
begin
  FBuildBuffer := TV2Arr.Create;
  FNewVert := TV2Arr.Create;

  FTess := gluNewTess;

  gluTessCallback(FTess, GLU_TESS_BEGIN_DATA, @TTess_cbBegin);
  gluTessCallback(FTess, GLU_TESS_END_DATA,   @TTess_cbEnd);
  gluTessCallback(FTess, GLU_TESS_VERTEX_DATA,  @TTess_cbVertex);
  gluTessCallback(FTess, GLU_TESS_COMBINE_DATA, @TTess_cbCombine);
  gluTessCallback(FTess, GLU_TESS_ERROR_DATA,   @TTess_cbError);
  //gluTessCallback(FTess, GLU_TESS_EDGE_FLAG_DATA,   @TTess_cbEdgeFlag);
end;

destructor TTess.Destroy;
begin
  if FTess <> nil then
    gluDeleteTess(FTess);
  inherited Destroy;
end;

end.

