unit avGLUIntf;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avGLU, mutils;

type
  TContours = array of TVec2Arr;

  { TPolyContour }

  TPolyContour = record
    cntr: TVec2Arr;
    hole: Boolean;
    procedure SetPoints(const APoints: array of TVec2);
  end;
  TPoly = array of TPolyContour;

function PolyToConvex(const APolyLine: TVec2Arr): TContours; overload;
function PolyToConvex(const APoly: array of TPolyContour): TContours; overload;
function RemoveSelfIntersection(const APolyLine: TVec2Arr): TContours; overload;
function RemoveSelfIntersection(const APoly: array of TPolyContour): TPoly; overload;
function SignedArea(const AContour: TVec2Arr): Single;

implementation

uses avContnrs, Math;

const
  PolyPartitionLib = 'PolyPartitionLib.dll';

type
  TVec2Double = record
    x : Double;
    y : Double;
    id: Integer;
  end;
  PVec2Double = ^TVec2Double;

  TPolyContour_rawptr = record
    points: PVec2;
    numPoints: Integer;
    hole: Integer;
    procedure Init(const cntr: TPolyContour);
  end;
  PPolyContour = ^TPolyContour_rawptr;

  TOnNewConvex = procedure (Convex: PVec2Double; ConvexLen: Integer; UserData: Pointer);

procedure ConvexPartitionHM_f(Contour: PVec2; ContourLen: Integer; CallBack: TOnNewConvex; UserData: Pointer); external PolyPartitionLib;
procedure ConvexPartitionHM_withHoles_f(Contours: PPolyContour; CountoursCount: Integer; CallBack: TOnNewConvex; UserData: Pointer); external PolyPartitionLib;

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

    procedure CustomExecute(const ALine: TVec2Arr); overload;
    procedure CustomExecute(const APoly: array of TPolyContour); overload;

    procedure DebugOut(cntrs: TContours);
  public
    function Execute(const ALine: TVec2Arr): TContours;
    function ExecuteToLoops(const APolyLine: TVec2Arr): TContours;
    function ExecuteToLoops(const APoly: array of TPolyContour): TPoly;

    constructor Create();
    destructor Destroy; override;
  end;

  { TConvexPartitioning }

  TConvexPartitioning = class
  private
    FOutData: TContours;
    procedure OnContour(Convex: PVec2Double; ConvexLen: Integer);
  public
    function GetData: TContours;
    function Execute(const ALine: TVec2Arr): TContours; overload;
    function Execute(const APoly: array of TPolyContour): TContours; overload;
  end;

procedure ConvexPartitioning_CB(Convex: PVec2Double; ConvexLen: Integer; UserData: Pointer);
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

function PolyToConvex(const APoly: array of TPolyContour): TContours;
var ttf: TConvexPartitioning;
    fixedPoly: TPoly;
begin
  ttf := TConvexPartitioning.Create();
  try
    fixedPoly := RemoveSelfIntersection(APoly);
    ttf.Execute(fixedPoly);
    //ttf.Execute(APoly);
    Result := ttf.GetData;
  finally
    ttf.Free;
  end;
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

function RemoveSelfIntersection(const APoly: array of TPolyContour): TPoly;
var looper: TTess;
begin
  looper := TTess.Create();
  try
    Result := looper.ExecuteToLoops(APoly);
  finally
    looper.Free;
  end;
end;

function SignedArea(const AContour: TVec2Arr): Single;
var summ: Double;
    i, ii, n: Integer;
begin
  summ := 0;
  n := Length(AContour);
  for i := 0 to n - 1 do
  begin
    ii := (i + 1) mod n;
    summ := summ + (AContour[ii].x - AContour[i].x)*(AContour[ii].y + AContour[i].y);
  end;
  Result := summ * 0.5;
end;

{ TPolyContour }

procedure TPolyContour.SetPoints(const APoints: array of TVec2);
var i: Integer;
begin
  SetLength(cntr, Length(APoints));
  for i := 0 to Length(APoints) - 1 do
    cntr[i] := APoints[i];
end;

{ TPolyContour_rawptr }

procedure TPolyContour_rawptr.Init(const cntr: TPolyContour);
begin
  numPoints := Length(cntr.cntr);
  if numPoints < 3 then numPoints := 0;
  if cntr.hole then hole := 1 else hole := 0;
  if numPoints = 0 then points := nil else points := @cntr.cntr[0];
end;

{ TConvexPartitioning }

procedure TConvexPartitioning.OnContour(Convex: PVec2Double; ConvexLen: Integer);
var NewConvex: TVec2Arr;
    i: Integer;
begin
  if ConvexLen < 2 then Exit;
  SetLength(NewConvex, ConvexLen);
  for i := 0 to ConvexLen - 1 do
  begin
    NewConvex[i].x := Convex^.x;
    NewConvex[i].y := Convex^.y;
    Inc(Convex);
  end;
  SetLength(FOutData, Length(FOutData) + 1);
  FOutData[High(FOutData)] := NewConvex;
end;

function TConvexPartitioning.GetData: TContours;
begin
  Result := FOutData;
end;

function TConvexPartitioning.Execute(const ALine: TVec2Arr): TContours;
begin
  ConvexPartitionHM_f(@ALine[0], Length(ALine), @ConvexPartitioning_CB, Self);
  Result := FOutData;
end;

function TConvexPartitioning.Execute(const APoly: array of TPolyContour): TContours;
var rawPoly: array of TPolyContour_rawptr;
    i: Integer;
begin
  if Length(APoly) = 0 then Exit(nil);
  SetLength(rawPoly, Length(APoly));
  for i := 0 to Length(APoly) - 1 do
    rawPoly[i].Init(APoly[i]);
  ConvexPartitionHM_withHoles_f(@rawPoly[0], Length(rawPoly), @ConvexPartitioning_CB, Self);
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

procedure TTess.cbEnd();
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

procedure TTess.CustomExecute(const APoly: array of TPolyContour);
var d: TGLArrayd3;
    i, j: Integer;
    ptSrc: PVec2;
    areaSign: Integer;
begin
  d[2] := 0;
  gluTessBeginPolygon(FTess, Self);
  for j := 0 to Length(APoly) - 1 do
  begin
    if Length(APoly[j].cntr) > 2 then
    begin
      areaSign := Sign(SignedArea(APoly[j].cntr));
      if areaSign = 0 then Continue;
      if not APoly[j].hole then
        areaSign := -areaSign;

      gluTessBeginContour(FTess);
      if areaSign < 0 then
        ptSrc := @APoly[j].cntr[Length(APoly[j].cntr)-1]
      else
        ptSrc := @APoly[j].cntr[0];
      for i := 0 to Length(APoly[j].cntr) - 1 do
      begin
        d[0] := ptSrc^.x;
        d[1] := ptSrc^.y;
        gluTessVertex(FTess, d, ptSrc);
        Inc(ptSrc, areaSign);
      end;
      gluTessEndContour(FTess);
    end;
  end;
  gluTessEndPolygon(FTess);
end;

procedure TTess.DebugOut(cntrs: TContours);
var j, i, ii: Integer;
    c: TVec2Arr;
    summ: Double;
    ccw: string;
begin
  WriteLn('TTess.DebugOut >>');
  WriteLn('  contours count: ', Length(cntrs));
  for j := 0 to Length(cntrs) - 1 do
  begin
    c := cntrs[j];
    summ := 0;
    for i := 0 to Length(c) - 1 do
    begin
      ii := (i + 1) mod Length(c);
      summ := summ + (c[ii].x - c[i].x)*(c[ii].y + c[i].y);
    end;
    if summ < 0 then
      ccw := 'CCW'
    else
      ccw := 'CW';
    WriteLn('    len: ', Length(c), ' wise: ', ccw);
  end;
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
  gluTessProperty(FTess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_ODD);
  gluTessNormal(FTess, 0, 0, 1);
  gluTessProperty(FTess, GLU_TESS_BOUNDARY_ONLY, GL_TRUE);
  CustomExecute(APolyLine);
  Result := FOutContours;
//  DebugOut(Result);
end;

function TTess.ExecuteToLoops(const APoly: array of TPolyContour): TPoly;
var i: Integer;
begin
  gluTessProperty(FTess, GLU_TESS_WINDING_RULE, GLU_TESS_WINDING_POSITIVE);
  gluTessNormal(FTess, 0, 0, 1);
  gluTessProperty(FTess, GLU_TESS_BOUNDARY_ONLY, GL_TRUE);
  CustomExecute(APoly);

  SetLength(Result, Length(FOutContours));
  for i := 0 to Length(FOutContours) - 1 do
  begin
    Result[i].cntr := FOutContours[i];
    Result[i].hole := SignedArea(FOutContours[i]) > 0;
  end;
end;

constructor TTess.Create();
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

