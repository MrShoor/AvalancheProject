unit avMesh;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  avRes, avTypes, avTess, avContnrs, mutils;

type
  IavArmature = interface;

  { TMeshVertex }

  TMeshVertex = packed record
    vsCoord: TVec3;
    vsNormal: TVec3;
    vsMatIndex: Single;
    vsWIndex: TVec4;
    vsWeight: TVec4;
    class function Layout: IDataLayout; static;
  end;
  PMeshVertex = ^TMeshVertex;
  IMeshVertices = specialize IArray<TMeshVertex>;
  TMeshVertices = specialize TVerticesRec<TMeshVertex>;

  { IavMesh }

  IavMesh = interface
    function GetArmature: IavArmature;
    function GetBBox: TAABB;
    function GetInd: IIndices;
    function GetName: String;
    function GetVert: IMeshVertices;

    property Name: String read GetName;
    property BBox: TAABB read GetBBox;
    property Vert: IMeshVertices read GetVert;
    property Ind : IIndices read GetInd;

    property Armature: IavArmature read GetArmature;
  end;

  TavMeshes = array of IavMesh;

  { IavBone }

  IavBone = interface
    function GetChild(index: Integer): IavBone;
    function GetChildsCount: Integer;
    function GetIndex: Integer;
    function GetName: String;
    function GetParent: IavBone;
    function GetTransform: TMat4;
    procedure SetTransform(AValue: TMat4);

    property Name: String read GetName;
    property Index: Integer read GetIndex;
    property Transform: TMat4 read GetTransform write SetTransform;

    property Parent: IavBone read GetParent;
    property ChildsCount: Integer read GetChildsCount;
    property Child[ChildIndex: Integer]: IavBone read GetChild;

    function AbsTransform: TMat4;
  end;

  IavAnimation = interface
  end;

  { IavArmature }

  IavArmature = interface
    function GetAnim(index: Integer): IavAnimation;
    function GetAnimCount: Integer;
    function GetBone(index: Integer): IavBone;
    function GetBonesCount: Integer;
    function GetName: AnsiString;

    property BonesCount: Integer read GetBonesCount;
    property Bone[index: Integer]: IavBone read GetBone;

    property AnimCount: Integer read GetAnimCount;
    property Anim[index: Integer]: IavAnimation read GetAnim;

    property Name: AnsiString read GetName;

    function FindBone(const AName: string): IavBone;

    function BoneTransformData: ITextureData;
  end;

procedure LoadFromStream(const stream: TStream; out meshes: TavMeshes);
procedure LoadFromFile(const FileName: string; out meshes: TavMeshes);

implementation

uses
  Math, avTexLoader;

type
  { IavBoneInternal }

  IavBoneInternal = interface (IavBone)
    procedure SetIndex(AValue: Integer);
    procedure SetName(AValue: String);
    procedure SetParent(AValue: IavBone);

    property Name: String read GetName write SetName;
    property Index: Integer read GetIndex write SetIndex;
    property Parent: IavBone read GetParent write SetParent;

    procedure AddChild(const Bone: IavBoneInternal);
  end;

  { IavArmatureInternal }

  IavArmatureInternal = interface (IavArmature)
    procedure SetName(const AValue: AnsiString);
    property Name: AnsiString read GetName write SetName;

    procedure AddBone(const ABone: IavBoneInternal);
  end;

  { IavMeshInternal }

  IavMeshInternal = interface (IavMesh)
    procedure SetArmature(const AValue: IavArmature);
    procedure SetBBox(const AValue: TAABB);
    procedure SetName(const AValue: String);

    property Name: String read GetName write SetName;
    property BBox: TAABB read GetBBox write SetBBox;
    property Armature: IavArmature read GetArmature write SetArmature;

    procedure AddDefaultVertexGroup(const GroupName: String; const GroupIndex: Integer);
  end;

  { TavBone }

  TavBone = class (TInterfacedObjectEx, IavBone, IavBoneInternal)
  private
    FParent   : Pointer;
    FChilds   : Array Of IavBone;
    FName     : AnsiString;
    FIndex    : Integer;
    FTransform: TMat4;

    function GetChild(index: Integer): IavBone;
    function GetChildsCount: Integer;
    function GetIndex: Integer;
    function GetName: String;
    function GetParent: IavBone;
    function GetTransform: TMat4;
    procedure SetIndex(AValue: Integer);
    procedure SetName(AValue: String);
    procedure SetParent(AValue: IavBone);
    procedure SetTransform(AValue: TMat4);
  public
    property Name: String read GetName write SetName;
    property Index: Integer read GetIndex write SetIndex;
    property Transform: TMat4 read GetTransform write SetTransform;

    property Parent: IavBone read GetParent write SetParent;
    property ChildsCount: Integer read GetChildsCount;
    property Child[ChildIndex: Integer]: IavBone read GetChild;
    procedure AddChild(const Bone: IavBoneInternal);

    function AbsTransform: TMat4;
  end;

  { TavArmature }

  TavArmature = class (TInterfacedObjectEx, IavArmature, IavArmatureInternal)
  private type
    IBoneHash = specialize IHashMap<string, Integer, TMurmur2HashString>;
    TBoneHash = specialize THashMap<string, Integer, TMurmur2HashString>;
  private
    FName: AnsiString;
    FBones: array of IavBone;
    FBoneIndex: IBoneHash;

    FAnim: array of IavAnimation;

    FTransfromData: ITextureData;

    function GetAnim(index: Integer): IavAnimation;
    function GetAnimCount: Integer;
    function GetBone(index: Integer): IavBone;
    function GetBonesCount: Integer;
    function GetName: AnsiString;
    procedure SetName(const AValue: AnsiString);
  public
    property BonesCount: Integer read GetBonesCount;
    property Bone[index: Integer]: IavBone read GetBone;

    property AnimCount: Integer read GetAnimCount;
    property Anim[index: Integer]: IavAnimation read GetAnim;

    property Name: AnsiString read GetName write SetName;

    function FindBone(const AName: string): IavBone;
    procedure AddBone(const ABone: IavBoneInternal);

    procedure UpdateTransformData;
    function BoneTransformData: ITextureData;

    procedure AfterConstruction; override;
  end;

  { TavMesh }

  TavMesh = class (TInterfacedObjectEx, IavMesh, IavMeshInternal)
  private type
    IVGroupHash = specialize IHashMap<String, Integer, TMurmur2HashString>;
    TVGroupHash = specialize THashMap<String, Integer, TMurmur2HashString>;
  private
    FVert: IMeshVertices;
    FInd : IIndices;
    FBBox: TAABB;
    FArm : IavArmature;
    FName: String;

    FDefVGroups: IVGroupHash;

    function GetArmature: IavArmature;
    function GetBBox: TAABB;
    function GetInd: IIndices;
    function GetName: String;
    function GetVert: IMeshVertices;
    procedure SetArmature(const AValue: IavArmature);
    procedure SetBBox(const AValue: TAABB);
    procedure SetName(const AValue: String);
  public
    property Name: String read GetName write SetName;
    property BBox: TAABB read GetBBox write SetBBox;
    property Vert: IMeshVertices read GetVert;
    property Ind : IIndices read GetInd;

    property Armature: IavArmature read GetArmature write SetArmature;

    procedure AddDefaultVertexGroup(const GroupName: String; const GroupIndex: Integer);

    procedure AfterConstruction; override;
  end;

  { TPNWVertex }

  TPNWVertex = packed record
    vsCoord: TVec3;
    vsNormal: TVec3;
    vsWIndex: TVec4;
    vsWeight: TVec4;
    class function Layout: IDataLayout; static;
  end;

  IPNWVertices = specialize IArray<TPNWVertex>;
  TPNWVertices = specialize TVerticesRec<TPNWVertex>;

  TMeshVertexFunc = specialize TMurmur2Hash<TMeshVertex>;
  IMeshVerticesHash = specialize IHashMap<TMeshVertex, Integer, TMeshVertexFunc>;
  TMeshVerticesHash = specialize THashMap<TMeshVertex, Integer, TMeshVertexFunc>;

const
  EmptyPNWVertex: TPNWVertex = (
    vsCoord: (x: 0; y: 0; z: 0);
    vsNormal: (x: 0; y: 0; z: 0);
    vsWIndex: (x: 0; y: 0; z: 0; w: 0);
    vsWeight: (x: 0; y: 0; z: 0; w: 0)
  );

procedure LoadFromStream(const stream: TStream; out meshes: TavMeshes);
  procedure LoadMeshFromStream(const stream: TStream; out mesh: IavMeshInternal);
  type
    TWeightInfo = packed record
      index: Integer;
      weight: Single;
    end;
  var defV: IPNWVertices;
      v: TPNWVertex;
      i, j, n: Integer;
      wCount, wReadCount: Integer;
      w: array [0..3] of TWeightInfo;
      s: AnsiString;

      mv: TMeshVertex;
      mvHash: IMeshVerticesHash;
      faceNorm: TVec3;
      faceMaterial: Integer;
      faceSmooth: Byte;
      ind: Integer;
  begin
    //prevent compile warning by initialization
    v := EmptyPNWVertex;
    i := 0;
    j := 0;
    n := 0;
    faceMaterial := 0;
    faceSmooth := 0;
    ind := 0;
    wCount := 0;
    ZeroClear(w, SizeOf(w));
    ZeroClear(faceNorm, SizeOf(faceNorm));
    //
    mesh := TavMesh.Create;
    StreamReadString(stream, s);
    mesh.Name := String(s);

    stream.ReadBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
    begin
      StreamReadString(stream, s);
      stream.ReadBuffer(j, SizeOf(j));
      mesh.AddDefaultVertexGroup(String(s), j);
    end;

    stream.ReadBuffer(n, SizeOf(n));
    defV := TPNWVertices.Create;
    defV.Capacity := n;
    for i := 0 to n - 1 do
    begin
      stream.ReadBuffer(v, SizeOf(v.vsCoord)+SizeOf(v.vsNormal));

      stream.ReadBuffer(wCount, SizeOf(wCount));
      w[0].index := -1; w[0].weight := 0;
      w[1].index := -1; w[0].weight := 0;
      w[2].index := -1; w[0].weight := 0;
      w[3].index := -1; w[0].weight := 0;
      wReadCount := min(4, wCount);
      stream.ReadBuffer(w[0], wReadCount*SizeOf(TWeightInfo));
      stream.Position := stream.Position + (wCount-wReadCount)*SizeOf(TWeightInfo);
      v.vsWIndex := Vec(w[0].index, w[1].index, w[2].index, w[3].index);
      v.vsWeight := Vec(w[0].weight, w[1].weight, w[2].weight, w[3].weight);
      case wReadCount of
        0: ;
        1: v.vsWeight.x   := 1;
        2: v.vsWeight.xy  := NormalizeWeights(v.vsWeight.xy);
        3: v.vsWeight.xyz := NormalizeWeights(v.vsWeight.xyz);
        4: v.vsWeight     := NormalizeWeights(v.vsWeight);
      else
        Assert(False, 'wtf?');
      end;

      defV.Add(v);
    end;

    mvHash := TMeshVerticesHash.Create;

    stream.ReadBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
    begin
      stream.ReadBuffer(faceMaterial, SizeOf(faceMaterial));
      stream.ReadBuffer(faceSmooth, SizeOf(faceSmooth));
      stream.ReadBuffer(faceNorm, SizeOf(faceNorm));
      for j := 0 to 2 do
      begin
        stream.ReadBuffer(ind, SizeOf(ind));
        v := defV.Item[ind];
        mv.vsCoord := v.vsCoord;
        if faceSmooth <> 0 then
          mv.vsNormal := v.vsNormal
        else
          mv.vsNormal := faceNorm;
        mv.vsMatIndex := faceMaterial;
        mv.vsWeight := v.vsWeight;
        mv.vsWIndex := v.vsWIndex;

        if not mvHash.TryGetValue(mv, ind) then
        begin
          ind := mesh.vert.Add(mv);
          mesh.BBox := mesh.BBox + mv.vsCoord;
          mvHash.Add(mv, ind);
        end;
        mesh.ind.Add(ind);
      end;
    end;
  end;

  procedure LoadArmatureFromStream(const stream: TStream; out arm: IavArmatureInternal);
    procedure LinkArmToMesh(const arm: IavArmatureInternal; const MeshName: AnsiString);
    var i: Integer;
    begin
      for i := 0 to Length(meshes) - 1 do
        if meshes[i].name = MeshName then
          IavMeshInternal(meshes[i]).Armature := arm;
    end;
    procedure LinkBones(const child, parent: IavBoneInternal);
    begin
      if parent = nil then Exit;
      if child = parent then Exit;
      parent.AddChild(child);
      child.Parent := parent;
    end;
    procedure LoadBoneFromStream(const stream: TStream; out bone: IavBoneInternal; out parent: String);
    var s: AnsiString;
        m: TMat4;
    begin
      ZeroClear(m, SizeOf(m));

      bone := TavBone.Create;

      StreamReadString(stream, s);
      bone.Name := String(s);

      StreamReadString(stream, s);
      parent := String(s);

      stream.ReadBuffer(m, SizeOf(m));
      bone.Transform := m;
    end;

  var s: AnsiString;
      i: Integer;
      n: Integer = 0;

      bones: array of IavBoneInternal;
      boneParent: array of String;
  begin
    arm := TavArmature.Create;

    StreamReadString(stream, s);
    arm.Name := s;

    stream.ReadBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
    begin
      StreamReadString(stream, s);
      LinkArmToMesh(arm, s);
    end;

     stream.ReadBuffer(n, SizeOf(n));
     SetLength(bones, n);
     SetLength(boneParent, n);
     for i := 0 to n - 1 do
     begin
       LoadBoneFromStream(stream, bones[i], boneParent[i]);
       arm.AddBone(bones[i]);
     end;
     for i := 0 to n - 1 do
       LinkBones(bones[i], IavBoneInternal(arm.FindBone(boneParent[i])));
  end;

var i, n: Integer;
    arm: array of IavArmatureInternal;
begin
  n := 0;

  stream.ReadBuffer(n, SizeOf(n));
  SetLength(meshes, n);
  for i := 0 to n - 1 do
    LoadMeshFromStream(stream, IavMeshInternal(meshes[i]));

  stream.ReadBuffer(n, SizeOf(n));
  SetLength(arm, n);
  for i := 0 to n - 1 do
    LoadArmatureFromStream(stream, arm[i]);
end;

procedure LoadFromFile(const FileName: string; out meshes: TavMeshes);
var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(fs, meshes);
  finally
    FreeAndNil(fs);
  end;
end;

{ TavMesh }

function TavMesh.GetArmature: IavArmature;
begin
  Result := FArm;
end;

function TavMesh.GetBBox: TAABB;
begin
  Result := FBBox;
end;

function TavMesh.GetInd: IIndices;
begin
  Result := FInd;
end;

function TavMesh.GetName: String;
begin
  Result := FName;
end;

function TavMesh.GetVert: IMeshVertices;
begin
  Result := FVert;
end;

procedure TavMesh.SetArmature(const AValue: IavArmature);
var b: IavBone;
    wind: Integer;
    i: Integer;
begin
  FArm := AValue;

  //todo remap weights indices in vertices
  for i := 0 to AValue.BonesCount - 1 do
  begin
    b := AValue.Bone[i];
    wind := FDefVGroups.Item[b.Name];
    Assert(b.Index=wind, 'todo remap weights indices in vertices');
  end;
end;

procedure TavMesh.SetBBox(const AValue: TAABB);
begin
  FBBox := AValue;
end;

procedure TavMesh.SetName(const AValue: String);
begin
  FName := AValue;
end;

procedure TavMesh.AddDefaultVertexGroup(const GroupName: String; const GroupIndex: Integer);
begin
  if FDefVGroups = nil then
    FDefVGroups := TVGroupHash.Create;
  FDefVGroups.Add(GroupName, GroupIndex);
end;

procedure TavMesh.AfterConstruction;
begin
  inherited AfterConstruction;
  FInd := Create_IIndices;
  FInd.PrimitiveType := ptTriangles;
  FInd.IsDWord := True;
  FVert := TMeshVertices.Create;
  FBBox := EmptyAABB;
end;

{ TavArmature }

function TavArmature.GetAnim(index: Integer): IavAnimation;
begin
  Result := FAnim[index];
end;

function TavArmature.GetAnimCount: Integer;
begin
  Result := Length(FAnim);
end;

function TavArmature.GetBone(index: Integer): IavBone;
begin
  Result := FBones[index];
end;

function TavArmature.GetBonesCount: Integer;
begin
  Result := Length(FBones);
end;

function TavArmature.GetName: AnsiString;
begin
  Result := FName;
end;

procedure TavArmature.SetName(const AValue: AnsiString);
begin
  FName := AValue;
end;

function TavArmature.FindBone(const AName: string): IavBone;
var i: Integer;
begin
  if FBoneIndex.TryGetValue(AName, i) then
    Result := FBones[i]
  else
    Result := Nil;
end;

procedure TavArmature.AddBone(const ABone: IavBoneInternal);
begin
  ABone.Index := Length(FBones);
  FBoneIndex.Add(ABone.Name, ABone.Index);
  SetLength(FBones, Length(FBones) + 1);
  FBones[ABone.Index] := ABone;
end;

procedure TavArmature.UpdateTransformData;
var i: Integer;
    m: TMat4;
    mip: TTextureMipInfo;
begin
  mip := FTransfromData.Data(0,0);
  for i := 0 to Length(FBones) - 1 do
  begin
    m := FBones[i].AbsTransform;
    m := IdentityMat4;
    PVec4(mip.Pixel(0, i))^ := m.Row[0];
    PVec4(mip.Pixel(1, i))^ := m.Row[1];
    PVec4(mip.Pixel(2, i))^ := m.Row[2];
    PVec4(mip.Pixel(3, i))^ := m.Row[3];
  end;
end;

function TavArmature.BoneTransformData: ITextureData;
begin
  if FTransfromData = nil then
  begin
    FTransfromData := EmptyTexData(4, Length(FBones), TTextureFormat.RGBA32f, False, True);
    UpdateTransformData;
  end;
  Result := FTransfromData;
end;

procedure TavArmature.AfterConstruction;
begin
  inherited AfterConstruction;
  FBoneIndex := TBoneHash.Create;
end;

{ TavBone }

function TavBone.GetChild(index: Integer): IavBone;
begin
  Result := FChilds[index];
end;

function TavBone.GetChildsCount: Integer;
begin
  Result := Length(FChilds);
end;

function TavBone.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TavBone.GetName: String;
begin
  Result := FName;
end;

function TavBone.GetParent: IavBone;
begin
  Result := IavBone(FParent);
end;

function TavBone.GetTransform: TMat4;
begin
  Result := FTransform;
end;

procedure TavBone.SetIndex(AValue: Integer);
begin
  FIndex := AValue;
end;

procedure TavBone.SetName(AValue: String);
begin
  FName := AValue;
end;

procedure TavBone.SetParent(AValue: IavBone);
begin
  FParent := Pointer(AValue);
end;

procedure TavBone.SetTransform(AValue: TMat4);
begin
  FTransform := AValue;
end;

procedure TavBone.AddChild(const Bone: IavBoneInternal);
var n: Integer;
begin
  n := Length(FChilds);
  SetLength(FChilds, n+1);
  FChilds[n] := Bone;
end;

function TavBone.AbsTransform: TMat4;
begin
  Result := FTransform;
end;

{ TPNWVertex }

class function TPNWVertex.Layout: IDataLayout;
begin
  Result := Nil;
end;

{ TMeshVertex }

class function TMeshVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 3).
               Add('vsNormal', ctFloat, 3).
               Add('vsMatIndex', ctFloat, 1).
               Add('vsWIndex', ctFloat, 4).
               Add('vsWeight', ctFloat, 4).Finish();
end;

end.

