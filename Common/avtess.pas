unit avTess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, avTypes, mutils, avContnrs;

type
  { TVerticesRec }


//  TRec should contain Layout class function
//  TRec = packed record
//    class function Layout: IDataLayout; static;
//  end;
  generic TVerticesRec<TRec> = class (specialize TArray<TRec>, IVerticesData)
  strict private
    FLayout: IDataLayout;
    function VerticesCount: Integer;
    function Layout: IDataLayout;
    function Data: TPointerData;
  public
    constructor Create;
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
    function IndicesCount: Integer;
    function PrimCount: Integer;
    function PrimType: TPrimitiveType;
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

function Create_IIndices : IIndices;
function LB: ILayoutBuilder;

implementation

uses Math;

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
  Inc(FTotalSize, FFields[FCount].Size); //to do align here
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

function TVerticesRec.VerticesCount: Integer;
begin
  Result := Count;
end;

function TVerticesRec.Layout: IDataLayout;
begin
  Result := FLayout;
end;

function TVerticesRec.Data: TPointerData;
begin
  Result.data := GetPItem(0);
  Result.size := Count * SizeOf(TRec);
end;

constructor TVerticesRec.Create;
begin
  FLayout := TRec.Layout;
end;

end.

