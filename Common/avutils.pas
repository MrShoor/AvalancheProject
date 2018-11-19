unit avUtils;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils,
  avBase, avRes, avTypes, mutils;

function GenQuad(const ABounds: TVec4): IVerticesData;
function GenQuad_VB(const AOwner: TavObject; const ABounds: TVec4): TavVB;

implementation

uses
  avContnrs, avTess;

type
  TQuadVertex = packed record
    vsCoord: TVec2;
    class function Layout: IDataLayout; static;
  end;
  IQuadVertices = {$IfDef FPC}specialize{$EndIf}IArray<TQuadVertex>;
  TQuadVertices = {$IfDef FPC}specialize{$EndIf}TVerticesRec<TQuadVertex>;


function GenQuad(const ABounds: TVec4): IVerticesData;
var data: IQuadVertices;
    v: TQuadVertex;
begin
  data := TQuadVertices.Create;
  v.vsCoord := Vec(ABounds.x, ABounds.y); data.Add(v);
  v.vsCoord := Vec(ABounds.x, ABounds.w); data.Add(v);
  v.vsCoord := Vec(ABounds.z, ABounds.y); data.Add(v);
  v.vsCoord := Vec(ABounds.z, ABounds.w); data.Add(v);
  Result := data as IVerticesData;
end;

function GenQuad_VB(const AOwner: TavObject; const ABounds: TVec4): TavVB;
begin
  Result := TavVB.Create(AOwner);
  Result.CullMode := cmNone;
  Result.PrimType := ptTriangleStrip;
  Result.Vertices := GenQuad(ABounds);
end;

{ TQuadVertex }

class function TQuadVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 2).Finish();
end;

end.

