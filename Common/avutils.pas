unit avUtils;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils,
  intfUtils,
  avBase, avRes, avTypes, mutils, avContnrs;

type
  ISystemBase = interface (IWeakedInterface)
  ['{DC83A43B-A305-49E3-8764-16E2B8626AD9}']
  end;

  IComponentBase = interface (IWeakedInterface)
  ['{9F2E6A54-4CF3-4904-948D-EDA326D42473}']
  end;

  { TSystemBase }

  TSystemBase = class(TWeakedInterfacedObject, ISystemBase)
  public type

    { TComponentBase }

    TComponentBase = class(TWeakedInterfacedObject, IComponentBase)
    private
      FIndex : Integer;
    protected
      FSystem: TSystemBase;
      procedure Detach; virtual;
    public
      constructor Create(ASystem: TSystemBase); virtual;
      destructor Destroy; override;
    end;
    ITComponentBaseArr = {$IfDef FPC}specialize{$EndIf} IArray<TComponentBase>;
    TTComponentBaseArr = {$IfDef FPC}specialize{$EndIf} TArray<TComponentBase>;
  protected
    FComps: ITComponentBaseArr;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

function GenQuad(const ABounds: TVec4): IVerticesData;
function GenQuad_VB(const AOwner: TavObject; const ABounds: TVec4): TavVB;

implementation

uses
  avTess;

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

{ TSystemBase }

procedure TSystemBase.AfterConstruction;
begin
  inherited AfterConstruction;
  FComps := TTComponentBaseArr.Create();
end;

destructor TSystemBase.Destroy;
var i: Integer;
begin
  for i := 0 to FComps.Count - 1 do
    FComps[i].Detach;
  inherited Destroy;
end;

{ TSystemBase.TComponentBase }

procedure TSystemBase.TComponentBase.Detach;
begin
  FSystem := nil;
end;

constructor TSystemBase.TComponentBase.Create(ASystem: TSystemBase);
begin
  FSystem := ASystem;
  FIndex := FSystem.FComps.Add(Self);
end;

destructor TSystemBase.TComponentBase.Destroy;
begin
  if FSystem <> nil then
  begin
    FSystem.FComps.DeleteWithSwap(FIndex);
    if FSystem.FComps.Count > FIndex then FSystem.FComps[FIndex].FIndex := FIndex;
    FSystem := nil;
  end;
  inherited Destroy;
end;

{ TQuadVertex }

class function TQuadVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 2).Finish();
end;

end.

