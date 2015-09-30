unit avMesh;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  avTypes, avTess, avContnrs, mutils, avTexLoader;

type
  IavArmature = interface;

  { TMeshVertex }

  TMeshVertex = packed record
    vsCoord: TVec3;
    vsNormal: TVec3;
    vsTex: TVec2;
    vsMatIndex: Single;
    vsWIndex: TVec4;
    vsWeight: TVec4;
    class function Layout: IDataLayout; static;
  end;
  PMeshVertex = ^TMeshVertex;
  IMeshVertices = specialize IArray<TMeshVertex>;
  TMeshVertices = specialize TVerticesRec<TMeshVertex>;

  { TMeshMaterial }

  TMeshMaterial = packed record
    matDiff: TVec4;
    matSpec: TVec4;
    matDiffMapFactor: Single;
    matSpecPow: Single;
  end;

  { TMeshMaterialMaps }

  TMeshMaterialMaps = packed record
    matDiffMap : ITextureData;
    matNormalMap: ITextureData;
  end;

  { TMeshAnimationState }

  TMeshAnimationState = packed record
    Index : Integer;
    Frame : Single;
    Weight: Single;
  end;

  { IavMesh }

  IavMesh = interface
    function GetMaterial(const AIndex: Integer): TMeshMaterial;
    function GetMaterialMaps(const AIndex: Integer): TMeshMaterialMaps;
    function GetArmature: IavArmature;
    function GetBBox: TAABB;
    function GetInd: IIndices;
    function GetName: String;
    function GetVert: IMeshVertices;

    function MaterialsCount: Integer;
    property Material    [const AIndex: Integer]: TMeshMaterial read GetMaterial;
    property MaterialMaps[const AIndex: Integer]: TMeshMaterialMaps read GetMaterialMaps;

    property Name: String read GetName;
    property BBox: TAABB read GetBBox;
    property Vert: IMeshVertices read GetVert;
    property Ind : IIndices read GetInd;

    property Armature: IavArmature read GetArmature;
    procedure GetPoseData(var Matrices: TMat4Arr; const AnimState: array of TMeshAnimationState);
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

  { IavAnimation }

  IavAnimation = interface
    function GetArmature: IavArmature;
    function GetName: String;
    function GetEnabled: Boolean;
    function GetFrame: Single;
    function GetFrameCount: Integer;
    procedure SetEnabled(AValue: Boolean);
    procedure SetFrame(AValue: Single);

    function Index: Integer;

    function BonesCount: Integer;
    procedure GetBoneTransform(const AIndex: Integer; const AFrame: Single; out BoneIndex: Integer; out Transform: TMat4);

    property Armature: IavArmature read GetArmature;
    property Name: String read GetName;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FrameCount: Integer read GetFrameCount;
    property Frame: Single read GetFrame write SetFrame;
  end;

  { IavArmature }

  IavArmature = interface
    function GetAnim(index: Integer): IavAnimation;
    function GetAnimCount: Integer;
    function GetBone(index: Integer): IavBone;
    function GetBonesCount: Integer;
    function GetName: String;

    property BonesCount: Integer read GetBonesCount;
    property Bone[index: Integer]: IavBone read GetBone;

    property AnimCount: Integer read GetAnimCount;
    property Anim[index: Integer]: IavAnimation read GetAnim;
    function FindAnim(const AName: string): IavAnimation;

    property Name: String read GetName;

    function FindBone(const AName: string): IavBone;

    procedure GetPoseData(var Matrices: TMat4Arr; const AnimState: array of TMeshAnimationState);
  end;

procedure LoadFromStream(const stream: TStream; out meshes: TavMeshes; TexManager: ITextureManager = Nil);
procedure LoadFromFile(const FileName: string; out meshes: TavMeshes; const TexManager: ITextureManager = Nil);

implementation

uses
  Math;

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

  { IavAnimationInternal }

  IavAnimationInternal = interface (IavAnimation)
    procedure SetArmature(const AValue: IavArmature);
    procedure SetName(const AValue: String);

    procedure SetBones(const indices: TIntArr);
    procedure AddFrame(const transforms: TMat4Arr);

    procedure SetAnimationIndex(const AAnimIndex: Integer);

    property Armature: IavArmature read GetArmature write SetArmature;
    property Name: String read GetName write SetName;
  end;

  { IavArmatureInternal }

  IavArmatureInternal = interface (IavArmature)
    procedure SetName(const AValue: String);
    property Name: String read GetName write SetName;

    procedure AddBone(const ABone: IavBoneInternal);
    procedure AddAnimation(const AAnim: IavAnimationInternal);
  end;

  { IavMeshInternal }

  IavMeshInternal = interface (IavMesh)
    procedure SetArmature(const AValue: IavArmature);
    procedure SetBBox(const AValue: TAABB);
    procedure SetName(const AValue: String);

    property Name: String read GetName write SetName;
    property BBox: TAABB read GetBBox write SetBBox;
    property Armature: IavArmature read GetArmature write SetArmature;

    procedure AddMaterial(const AMat: TMeshMaterial; const AMatMap: TMeshMaterialMaps);
  end;

  { TavBone }

  TavBone = class (TInterfacedObjectEx, IavBone, IavBoneInternal)
  private
    FParent   : Pointer;
    FChilds   : Array Of IavBone;
    FName     : String;
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
    destructor Destroy; override;
  end;

  { TavArmature }

  TavArmature = class (TInterfacedObjectEx, IavArmature, IavArmatureInternal)
  private type
    TavBoneArr = array of IavBone;
    IBoneHash = specialize IHashMap<string, Integer, TMurmur2HashString>;
    TBoneHash = specialize THashMap<string, Integer, TMurmur2HashString>;
  private
    FName: String;
    FBones: TavBoneArr;
    FRootBones: TavBoneArr;
    FBoneIndex: IBoneHash;

    FAnim: array of IavAnimationInternal;

    function GetAnim(index: Integer): IavAnimation;
    function GetAnimCount: Integer;
    function GetBone(index: Integer): IavBone;
    function GetBonesCount: Integer;
    function GetName: AnsiString;
    procedure SetName(const AValue: String);

    function GetRootBones: TavBoneArr;

    property RootBones: TavBoneArr read GetRootBones;
  public
    property BonesCount: Integer read GetBonesCount;
    property Bone[index: Integer]: IavBone read GetBone;

    property AnimCount: Integer read GetAnimCount;
    property Anim[index: Integer]: IavAnimation read GetAnim;

    property Name: String read GetName write SetName;

    function FindBone(const AName: string): IavBone;
    procedure AddBone(const ABone: IavBoneInternal);
    procedure AddAnimation(const AAnim: IavAnimationInternal);
    function FindAnim(const AName: string): IavAnimation;

    procedure GetPoseData(var Matrices: TMat4Arr; const AnimState: array of TMeshAnimationState);

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  { TavMesh }

  TavMesh = class (TInterfacedObjectEx, IavMesh, IavMeshInternal)
  private
    FVert: IMeshVertices;
    FInd : IIndices;
    FBBox: TAABB;
    FArm : IavArmature;
    FName: String;

    FMat: array of TMeshMaterial;
    FMatMaps: array of TMeshMaterialMaps;

    function GetMaterial(const AIndex: Integer): TMeshMaterial;
    function GetMaterialMaps(const AIndex: Integer): TMeshMaterialMaps;
    function GetArmature: IavArmature;
    function GetBBox: TAABB;
    function GetInd: IIndices;
    function GetName: String;
    function GetVert: IMeshVertices;
    procedure SetArmature(const AValue: IavArmature);
    procedure SetBBox(const AValue: TAABB);
    procedure SetName(const AValue: String);
  public
    function MaterialsCount: Integer;

    property Name: String read GetName write SetName;
    property BBox: TAABB read GetBBox write SetBBox;
    property Vert: IMeshVertices read GetVert;
    property Ind : IIndices read GetInd;

    property Armature: IavArmature read GetArmature write SetArmature;
    procedure GetPoseData(var Matrices: TMat4Arr; const AnimState: array of TMeshAnimationState);

    procedure AddMaterial(const AMat: TMeshMaterial; const AMatMap: TMeshMaterialMaps);

    procedure AfterConstruction; override;
  end;

  { TavAnimation }

  TavAnimation = class (TInterfacedObjectEx, IavAnimation, IavAnimationInternal)
  private
    FName: String;
    FEnabled: Boolean;
    FFrame: Single;

    FBones : TIntArr;
    FFrames: array of TMat4Arr;

    FArmature: Pointer;
    FAnimationIndex: Integer;

    function GetArmature: IavArmature;
    function GetName: String;
    function GetEnabled: Boolean;
    function GetFrame: Single;
    function GetFrameCount: Integer;
    procedure SetArmature(const AValue: IavArmature);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFrame(AValue: Single);

    procedure SetName(const AValue: String);
  public
    procedure SetBones(const indices: TIntArr);
    procedure AddFrame(const transforms: TMat4Arr);

    function Index: Integer;
    procedure SetAnimationIndex(const AAnimIndex: Integer);

    function BonesCount: Integer;
    procedure GetBoneTransform(const AIndex: Integer; const AFrame: Single; out BoneIndex: Integer; out Transform: TMat4);

    property Armature: IavArmature read GetArmature write SetArmature;
    property Name: String read GetName;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FrameCount: Integer read GetFrameCount;
    property Frame: Single read GetFrame write SetFrame;
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

procedure LoadFromStream(const stream: TStream; out meshes: TavMeshes; TexManager: ITextureManager = Nil);
  procedure LoadMeshFromStream(const stream: TStream; out mesh: IavMeshInternal; out parent: String);
  type
    TWeightInfo = packed record
      index: Integer;
      weight: Single;
    end;
    TWeightInfoArr = array of TWeightInfo;

  var defV: IPNWVertices;
      v: TPNWVertex;
      i, j, n: Integer;
      wCount: Integer;
      w: TWeightInfoArr;
      s: AnsiString;

      mv: TMeshVertex;
      mvHash: IMeshVerticesHash;
      faceNorm: TVec3;
      faceMaterial: Integer;
      faceSmooth: Byte;
      ind: Integer;

      mat: TMeshMaterial;
      matMap: TMeshMaterialMaps;

      texWidth, texHeight: Integer;
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
    ZeroClear(mv, SizeOf(mv));
    ZeroClear(mat, SizeOf(mat));
    //
    mesh := TavMesh.Create;
    StreamReadString(stream, s);
    mesh.Name := String(s);

    StreamReadString(stream, s);
    parent := String(s);

    texWidth := SIZE_DEFAULT;
    texHeight := SIZE_DEFAULT;
    stream.ReadBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
    begin
      stream.ReadBuffer(mat.matDiff, SizeOf(mat.matDiff));
      stream.ReadBuffer(mat.matDiffMapFactor, SizeOf(mat.matDiffMapFactor));
      stream.ReadBuffer(mat.matSpec, SizeOf(mat.matSpec));
      stream.ReadBuffer(mat.matSpecPow, SizeOf(mat.matSpecPow));

      StreamReadString(stream, s);
      if s = '' then
        matMap.matDiffMap := nil
      else
      begin
        matMap.matDiffMap := TexManager.LoadTexture(String(s), texWidth, texHeight, TImageFormat.A8R8G8B8);
        texWidth := matMap.matDiffMap.Width;
        texHeight := matMap.matDiffMap.Height;
      end;

      StreamReadString(stream, s);
      if s = '' then
        matMap.matNormalMap := nil
      else
      begin
        matMap.matNormalMap := TexManager.LoadTexture(String(s), texWidth, texHeight, TImageFormat.A8R8G8B8);
        texWidth := matMap.matNormalMap.Width;
        texHeight := matMap.matNormalMap.Height;
      end;

      mesh.AddMaterial(mat, matMap);
    end;

    stream.ReadBuffer(n, SizeOf(n));
    defV := TPNWVertices.Create;
    defV.Capacity := n;
    for i := 0 to n - 1 do
    begin
      stream.ReadBuffer(v, SizeOf(v.vsCoord)+SizeOf(v.vsNormal));

      stream.ReadBuffer(wCount, SizeOf(wCount));
      SetLength(w, wCount);
      stream.ReadBuffer(w[0], wCount*SizeOf(TWeightInfo));
      v.vsWIndex := Vec(-1,-1,-1,-1);
      v.vsWeight := Vec(0,0,0,0);
      Assert(Length(w)<=4);
      for j := 0 to Length(w) - 1 do
      begin
        v.vsWIndex.f[j] := w[j].index;
        v.vsWeight.f[j] := w[j].weight;
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
        stream.ReadBuffer(mv.vsTex, SizeOf(mv.vsTex));
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
        n: Integer = 0;
    begin
      ZeroClear(m, SizeOf(m));

      bone := TavBone.Create;

      StreamReadString(stream, s);
      bone.Name := String(s);

      StreamReadString(stream, s);
      parent := String(s);

      stream.ReadBuffer(n, SizeOf(n));
      bone.Index := n;

      stream.ReadBuffer(m, SizeOf(m));
      bone.Transform := m;
    end;
    procedure LoadAnimationFromStream(const stream: TStream; out anim: IavAnimationInternal);
    var s: AnsiString;
        i: Integer;
        n: Integer = 0;
        bones: TIntArr;
        frame: TMat4Arr;
        frameStart: Integer = 0;
        frameEnd: Integer = 0;
    begin
      anim := TavAnimation.Create;
      StreamReadString(stream, s);
      anim.Name := String(s);

      stream.ReadBuffer(n, SizeOf(n));
      SetLength(bones, n);
      if n > 0 then stream.ReadBuffer(bones[0], n*SizeOf(Integer));
      anim.SetBones(bones);

      stream.ReadBuffer(frameStart, SizeOf(frameStart));
      stream.ReadBuffer(frameEnd, SizeOf(frameEnd));
      if Length(bones) > 0 then
        for i := frameStart to frameEnd - 1 do
        begin
          SetLength(frame, length(bones));
          stream.ReadBuffer(frame[0], Length(frame)*SizeOf(TMat4));
          anim.AddFrame(frame);
          frame := nil;
        end;
    end;
  var s: AnsiString;
      i: Integer;
      n: Integer = 0;

      bones: array of IavBoneInternal;
      boneParent: array of String;
      anim: IavAnimationInternal;
  begin
    arm := TavArmature.Create;

    StreamReadString(stream, s);
    arm.Name := String(s);

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

    stream.ReadBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
    begin
      LoadAnimationFromStream(stream, anim);
      anim.Armature := arm;
      arm.AddAnimation(anim);
    end;
  end;

var i, j, n: Integer;
    arm: array of IavArmatureInternal;
    MeshParent: String;
begin
  if TexManager = nil then TexManager := Create_ITextureManager;

  n := 0;

  stream.ReadBuffer(n, SizeOf(n));
  SetLength(arm, n);
  for i := 0 to n - 1 do
    LoadArmatureFromStream(stream, arm[i]);

  stream.ReadBuffer(n, SizeOf(n));
  SetLength(meshes, n);
  for i := 0 to n - 1 do
  begin
    LoadMeshFromStream(stream, IavMeshInternal(meshes[i]), MeshParent);
    for j := 0 to Length(arm) - 1 do
      if arm[j].Name = MeshParent then
      begin
        IavMeshInternal(meshes[i]).Armature := arm[j];
        Break;
      end;
  end;
end;

procedure LoadFromFile(const FileName: string; out meshes: TavMeshes; const TexManager: ITextureManager = Nil);
var fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(fs, meshes, TexManager);
  finally
    FreeAndNil(fs);
  end;
end;

{ TavAnimation }

function TavAnimation.GetName: String;
begin
  Result := FName;
end;

function TavAnimation.GetArmature: IavArmature;
begin
  Result := IavArmature(FArmature);
end;

function TavAnimation.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TavAnimation.GetFrame: Single;
begin
  Result := FFrame;
end;

function TavAnimation.GetFrameCount: Integer;
begin
  Result := Length(FFrames);
end;

procedure TavAnimation.SetArmature(const AValue: IavArmature);
begin
  FArmature := Pointer(AValue);
end;

procedure TavAnimation.SetEnabled(AValue: Boolean);
begin
  FEnabled := AValue;
end;

procedure TavAnimation.SetFrame(AValue: Single);
begin
  FFrame := AValue;
end;

procedure TavAnimation.SetName(const AValue: String);
begin
  FName := AValue;
end;

procedure TavAnimation.SetBones(const indices: TIntArr);
begin
  FBones := indices;
  FFrames := nil;
end;

procedure TavAnimation.AddFrame(const transforms: TMat4Arr);
var n: Integer;
begin
  n := Length(FFrames);
  SetLength(FFrames, n+1);
  FFrames[n] := transforms;
end;

function TavAnimation.Index: Integer;
begin
  Result := FAnimationIndex;
end;

procedure TavAnimation.SetAnimationIndex(const AAnimIndex: Integer);
begin
  FAnimationIndex := AAnimIndex;
end;

function TavAnimation.BonesCount: Integer;
begin
  Result := Length(FBones);
end;

procedure TavAnimation.GetBoneTransform(const AIndex: Integer; const AFrame: Single; out BoneIndex: Integer; out Transform: TMat4);
var Frame1, Frame2, FrameCnt: Integer;
    FrameWeight: Single;
begin
  BoneIndex := FBones[AIndex];

  Frame1 := Floor(AFrame);
  Frame2 := Ceil(AFrame);
  FrameWeight := AFrame - Frame1;

  FrameCnt := FrameCount;
  Frame1 := Frame1 mod FrameCnt;
  Frame2 := Frame2 mod FrameCnt;
  if Frame1 < 0 then Inc(Frame1, FrameCnt);
  if Frame2 < 0 then Inc(Frame2, FrameCnt);

  Transform := Lerp(FFrames[Frame1][AIndex], FFrames[Frame2][AIndex], FrameWeight);
end;

{ TavMesh }

function TavMesh.GetMaterial(const AIndex: Integer): TMeshMaterial;
begin
  Result := FMat[AIndex];
end;

function TavMesh.GetMaterialMaps(const AIndex: Integer): TMeshMaterialMaps;
begin
  Result := FMatMaps[AIndex];
end;

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
begin
  FArm := AValue;
end;

procedure TavMesh.SetBBox(const AValue: TAABB);
begin
  FBBox := AValue;
end;

procedure TavMesh.SetName(const AValue: String);
begin
  FName := AValue;
end;

function TavMesh.MaterialsCount: Integer;
begin
  Result := Length(FMat);
end;

procedure TavMesh.GetPoseData(var Matrices: TMat4Arr; const AnimState: array of TMeshAnimationState);
begin
  if Assigned(FArm) then
    FArm.GetPoseData(Matrices, AnimState)
  else
  begin
    SetLength(Matrices, 1);
    Matrices[0] := IdentityMat4;
  end;
end;

procedure TavMesh.AddMaterial(const AMat: TMeshMaterial; const AMatMap: TMeshMaterialMaps);
var n: Integer;
begin
  n := MaterialsCount;
  SetLength(FMat, n + 1);
  FMat[n] := AMat;
  SetLength(FMatMaps, n + 1);
  FMatMaps[n] := AMatMap;
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

procedure TavArmature.SetName(const AValue: String);
begin
  FName := AValue;
end;

function TavArmature.GetRootBones: TavBoneArr;
var i, n: Integer;
begin
  if FRootBones = nil then
  begin
    for i := 0 to Length(FBones) - 1 do
      if FBones[i].Parent = nil then
      begin
        n := Length(FRootBones);
        SetLength(FRootBones, n+1);
        FRootBones[n] := FBones[i];
      end;
  end;
  Result := FRootBones;
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
  FBoneIndex.Add(ABone.Name, ABone.Index);
  if ABone.Index >= 0 then
  if Length(FBones) <= ABone.Index then
    SetLength(FBones, ABone.Index+1);
  FBones[ABone.Index] := ABone;
end;

procedure TavArmature.AddAnimation(const AAnim: IavAnimationInternal);
var n: Integer;
begin
  n := Length(FAnim);
  SetLength(FAnim, n+1);
  FAnim[n] := AAnim;
  AAnim.SetAnimationIndex(n);
end;

function TavArmature.FindAnim(const AName: string): IavAnimation;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Length(FAnim) - 1 do
    if FAnim[i].Name = AName then
      Exit(FAnim[i]);
end;

procedure TavArmature.GetPoseData(var Matrices: TMat4Arr; const AnimState: array of TMeshAnimationState);
  procedure FillMipData_Recursive(var AbsTransform: TMat4Arr; const animTransform: TMat4Arr; const bone: IavBone; const parentTransform: TMat4);
  var i: Integer;
  begin
    AbsTransform[bone.Index] := animTransform[bone.Index]*parentTransform;
    for i := 0 to bone.ChildsCount - 1 do
      FillMipData_Recursive(AbsTransform, animTransform, bone.Child[i], AbsTransform[bone.Index]);
  end;

var i, j, BoneInd: Integer;
    m: TMat4;

    boneWeight: TSingleArr;
    boneTransform: TMat4Arr;

    rBones: TavBoneArr;
    currAnim: IavAnimation;
begin
  if Length(FBones) = 0 then Exit;

  SetLength(boneWeight, Length(FBones));
  ZeroClear(boneWeight[0], Length(boneWeight)*SizeOf(boneWeight[0]));
  SetLength(boneTransform, Length(FBones));
  ZeroClear(boneTransform[0], Length(boneTransform)*SizeOf(boneTransform[0]));

  for i := 0 to Length(FBones) - 1 do
    boneTransform[i] := FBones[i].Transform;

  for i := 0 to Length(AnimState) - 1 do
  begin
    currAnim := FAnim[AnimState[i].Index];
    for j := 0 to currAnim.BonesCount - 1 do
    begin
      currAnim.GetBoneTransform(j, AnimState[i].Frame, BoneInd, m);

      if boneWeight[BoneInd] = 0 then
        boneTransform[BoneInd] := m*AnimState[i].Weight
      else
        boneTransform[BoneInd] := boneTransform[BoneInd] + m*AnimState[i].Weight;

      boneWeight[BoneInd] := boneWeight[BoneInd] + AnimState[i].Weight;
    end;
  end;

  for i := 0 to Length(boneTransform) - 1 do
    if (boneWeight[i] > 0) then
      boneTransform[i] := boneTransform[i] * (1 / boneWeight[i]);

  if Length(Matrices) <> Length(boneTransform) then
    SetLength(Matrices, Length(boneTransform));

  rBones := RootBones;
  for i := 0 to Length(rBones) - 1 do
    FillMipData_Recursive(Matrices, boneTransform, rBones[i], IdentityMat4);
end;

procedure TavArmature.AfterConstruction;
begin
  inherited AfterConstruction;
  FBoneIndex := TBoneHash.Create;
end;

destructor TavArmature.Destroy;
var i: Integer;
begin
  for i := 0 to Length(FAnim) - 1 do
    FAnim[i].Armature := nil;
  inherited Destroy;
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

destructor TavBone.Destroy;
var i: Integer;
begin
  for i := 0 to Length(FChilds) - 1 do
    IavBoneInternal(FChilds[i]).Parent := nil;
  inherited Destroy;
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
               Add('vsTex', ctFloat, 2).
               Add('vsMatIndex', ctFloat, 1).
               Add('vsWIndex', ctFloat, 4).
               Add('vsWeight', ctFloat, 4).Finish();
end;

end.

