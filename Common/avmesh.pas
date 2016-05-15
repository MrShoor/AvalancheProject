unit avMesh;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  avTypes, avTess, avContnrs, mutils, avTexLoader;

type
  IavArmature = interface;
  IavMeshInstance = interface;

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

    function CreateInstance(const AInstanceName: string): IavMeshInstance;
  end;

  IavMeshes = specialize IHashMap<string, IavMesh>;
  TavMeshes = specialize THashMap<string, IavMesh>;

  { IavMeshInstance }

  IavMeshInstance = interface
    function GetArmature: IavArmature;
    function GetChild(const AIndex: Integer): IavMeshInstance;
    function GetMesh: IavMesh;
    function GetName: String;
    function GetParent: IavMeshInstance;
    function GetTransform: TMat4;
    procedure SetTransform(const AValue: TMat4);
    //--------------------------

    property Name     : string      read GetName;
    property Transform: TMat4       read GetTransform write SetTransform;
    property Mesh     : IavMesh     read GetMesh;
    property Armature : IavArmature read GetArmature;

    property Parent: IavMeshInstance read GetParent;

    function ChildsCount: Integer;
    property Child[const AIndex: Integer]: IavMeshInstance read GetChild;
    procedure DelChild(const AIndex: Integer);
    procedure AddChild(const AChild: IavMeshInstance);
    function IndexOf(const AChild: IavMeshInstance): Integer;

    function  IndexOfBone(const ABoneName: string): Integer;
    procedure SetAnimationPose(const AnimState: array of TMeshAnimationState);
    function  LocalPose: TMat4Arr;
    function  AbsPose: TMat4Arr;

    function Clone(const NewInstanceName: string): IavMeshInstance;
  end;

  IavMeshInstances = specialize IHashMap<string, IavMeshInstance>;
  TavMeshInstances = specialize THashMap<string, IavMeshInstance>;

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

    function IndexOfBone(const AName: string): Integer;
    function FindBone(const AName: string): IavBone;

    procedure GetPoseData(var Matrices: TMat4Arr; const RemapIndices: TIntArr; const AnimState: array of TMeshAnimationState);
  end;

  IavArmatures = specialize IHashMap<string, IavArmature>;
  TavArmatures = specialize THashMap<string, IavArmature>;

procedure LoadFromStream(const stream: TStream; out meshes: IavMeshes; out meshInst: IavMeshInstances; TexManager: ITextureManager = Nil);
procedure LoadFromFile(const FileName: string; out meshes: IavMeshes; out meshInst: IavMeshInstances; const TexManager: ITextureManager = Nil);

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
    procedure SetBBox(const AValue: TAABB);
    procedure SetName(const AValue: String);

    property Name: String read GetName write SetName;
    property BBox: TAABB read GetBBox write SetBBox;

    procedure AddMaterial(const AMat: TMeshMaterial; const AMatMap: TMeshMaterialMaps);
  end;

  { IavMeshInstanceInternal }

  IavMeshInstanceInternal = interface (IavMeshInstance)
    procedure SetParent(const AValue: IavMeshInstance);
    procedure SetMesh(const AValue: IavMesh);
    procedure SetName(const AValue: string);
    //--------------------------
    procedure SetArmature(const AValue: IavArmature; const AGroupNames: array of string);

    property Parent   : IavMeshInstance read GetParent   write SetParent;
    property Name     : string          read GetName     write SetName;
    property Mesh     : IavMesh         read GetMesh     write SetMesh;
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
    IBoneHash = specialize IHashMap<string, Integer>;
    TBoneHash = specialize THashMap<string, Integer>;
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

    function IndexOfBone(const AName: string): Integer;
    function FindBone(const AName: string): IavBone;
    procedure AddBone(const ABone: IavBoneInternal);
    procedure AddAnimation(const AAnim: IavAnimationInternal);
    function FindAnim(const AName: string): IavAnimation;

    procedure GetPoseData(var Matrices: TMat4Arr; const RemapIndices: TIntArr; const AnimState: array of TMeshAnimationState);

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  { TavMesh }

  TavMesh = class (TInterfacedObjectEx, IavMesh, IavMeshInternal)
  private
    FVert: IMeshVertices;
    FInd : IIndices;
    FBBox: TAABB;
    FName: String;

    FMat: array of TMeshMaterial;
    FMatMaps: array of TMeshMaterialMaps;

    function GetMaterial(const AIndex: Integer): TMeshMaterial;
    function GetMaterialMaps(const AIndex: Integer): TMeshMaterialMaps;
    function GetBBox: TAABB;
    function GetInd: IIndices;
    function GetName: String;
    function GetVert: IMeshVertices;
    procedure SetBBox(const AValue: TAABB);
    procedure SetName(const AValue: String);
  public
    function MaterialsCount: Integer;

    property Name: String read GetName write SetName;
    property BBox: TAABB read GetBBox write SetBBox;
    property Vert: IMeshVertices read GetVert;
    property Ind : IIndices read GetInd;

    function CreateInstance(const AInstanceName: string): IavMeshInstance;

    procedure AddMaterial(const AMat: TMeshMaterial; const AMatMap: TMeshMaterialMaps);

    procedure AfterConstruction; override;
  end;

  { TavMeshInstance }

  TavMeshInstance = class (TInterfacedObjectEx, IavMeshInstance, IavMeshInstanceInternal)
  private type
    IChildList = specialize IArray<IavMeshInstance>;
    TChildList = specialize TArray<IavMeshInstance>;
  private
    FParent: Pointer;
    FArm: IavArmature;
    FBoneRemap: TIntArr;
    FMesh: IavMesh;

    FChilds: IChildList;

    FName: string;
    FLocalTransform: TMat4;
    FBonePretransform: TMat4;
    FPoseTransform: TMat4Arr;

    FPoseAbsValid: Boolean;
    FPoseAbs: TMat4Arr;
  public
    function GetArmature: IavArmature;
    function GetChild(const AIndex: Integer): IavMeshInstance;
    function GetMesh: IavMesh;
    function GetName: String;
    function GetParent: IavMeshInstance;
    function GetTransform: TMat4;
    procedure SetMesh(const AValue: IavMesh);
    procedure SetName(const AValue: string);
    procedure SetParent(const AValue: IavMeshInstance);
    procedure SetTransform(const AValue: TMat4);
    //--------------------------
    procedure SetArmature(const AValue: IavArmature; const AGroupNames: array of string);

    property Name     : string      read GetName;
    property Transform: TMat4       read GetTransform write SetTransform;
    property Mesh     : IavMesh     read GetMesh;
    property Armature : IavArmature read GetArmature;

    property Parent: IavMeshInstance read GetParent write SetParent;

    function ChildsCount: Integer;
    property Child[const AIndex: Integer]: IavMeshInstance read GetChild;
    procedure DelChild(const AIndex: Integer);
    procedure AddChild(const AChild: IavMeshInstance);
    function IndexOf(const AChild: IavMeshInstance): Integer;

    function IndexOfBone(const ABoneName: string): Integer;
    procedure SetAnimationPose(const AnimState: array of TMeshAnimationState);
    function LocalPose: TMat4Arr;
    function AbsPose: TMat4Arr;

    function Clone(const NewInstanceName: string): IavMeshInstance;
  public
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

  IMeshVerticesHash = specialize IHashMap<TMeshVertex, Integer>;
  TMeshVerticesHash = specialize THashMap<TMeshVertex, Integer>;

const
  EmptyPNWVertex: TPNWVertex = (
    vsCoord: (x: 0; y: 0; z: 0);
    vsNormal: (x: 0; y: 0; z: 0);
    vsWIndex: (x: 0; y: 0; z: 0; w: 0);
    vsWeight: (x: 0; y: 0; z: 0; w: 0)
  );

procedure LoadFromStream(const stream: TStream; out meshes: IavMeshes; out meshInst: IavMeshInstances; TexManager: ITextureManager);
type
  TMeshInstInfo = packed record
    Inst: IavMeshInstanceInternal;
    Parent: string;
    Mesh: string;
    GroupNames: array of string;
  end;

  procedure LoadMeshFromStream(const stream: TStream; out mesh: IavMeshInternal);
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
      v.vsWIndex := Vec(0,-1,-1,-1);
      v.vsWeight := Vec(1,0,0,0);
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

  procedure LoadMeshInstanceFromStream(const stream: TStream; out instInfo: TMeshInstInfo);
  var s: AnsiString;
      m: TMat4;
      i, n: Integer;
  begin
    //prevent warnings
    m := IdentityMat4;
    n := 0;
    //

    instInfo.Inst := TavMeshInstance.Create;
    StreamReadString(stream, s);
    instInfo.Inst.Name := s;
    StreamReadString(stream, s);
    instInfo.Parent := s;
    stream.ReadBuffer(m, SizeOf(m));
    instInfo.Inst.Transform := m;
    StreamReadString(stream, s);
    instInfo.Mesh := s;
    stream.ReadBuffer(n, SizeOf(n));
    SetLength(instInfo.GroupNames, n);
    for i := 0 to n - 1 do
    begin
      StreamReadString(stream, s);
      instInfo.GroupNames[i] := s;
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

var i, n: Integer;

    armatures: IavArmatures;

    arm: IavArmatureInternal;
    mesh: IavMeshInternal;
    inst: IavMeshInstance;
    instArr: array of TMeshInstInfo;
begin
  if TexManager = nil then TexManager := Create_ITextureManager;

  n := 0;

  armatures := TavArmatures.Create;
  stream.ReadBuffer(n, SizeOf(n));
  for i := 0 to n - 1 do
  begin
    LoadArmatureFromStream(stream, arm);
    armatures.Add(arm.Name, arm);
  end;

  meshes := TavMeshes.Create;
  stream.ReadBuffer(n, SizeOf(n));
  for i := 0 to n - 1 do
  begin
    LoadMeshFromStream(stream, mesh);
    meshes.Add(mesh.Name, mesh);
  end;

  meshInst := TavMeshInstances.Create;
  stream.ReadBuffer(n, SizeOf(n));
  SetLength(instArr, n);
  for i := 0 to n - 1 do
  begin
    LoadMeshInstanceFromStream(stream, instArr[i]);
    meshInst.Add(instArr[i].Inst.Name, instArr[i].Inst);
  end;

  for i := 0 to Length(instArr) - 1 do
  begin
    if instArr[i].Parent <> '' then
    begin
      if meshInst.TryGetValue(instArr[i].Parent, inst) then
        inst.AddChild(instArr[i].Inst)
      else
        if armatures.TryGetValue(instArr[i].Parent, IavArmature(arm)) then
          instArr[i].Inst.SetArmature(arm, instArr[i].GroupNames)
        else
          Assert(False, 'Parent "'+instArr[i].Parent+'" not found for object "'+instArr[i].Inst.Name+'"');
    end;

    if meshes.TryGetValue(instArr[i].Mesh, IavMesh(mesh)) then
      instArr[i].Inst.Mesh := mesh
    else
      Assert(False, 'Mesh "'+instArr[i].Mesh+'" not found for object "'+instArr[i].Inst.Name+'"');
  end;
end;

procedure LoadFromFile(const FileName: string; out meshes: IavMeshes; out
  meshInst: IavMeshInstances; const TexManager: ITextureManager);
var fs: TFileStream;
    oldDir: string;
begin
  fs := TFileStream.Create(FileName, fmOpenRead);
  oldDir := GetCurrentDir;
  try
    SetCurrentDir(ExtractFilePath(FileName));
    LoadFromStream(fs, meshes, meshInst, TexManager);
  finally
    SetCurrentDir(oldDir);
    FreeAndNil(fs);
  end;
end;

{ TavMeshInstance }

function TavMeshInstance.GetArmature: IavArmature;
begin
  Result := FArm;
end;

function TavMeshInstance.GetChild(const AIndex: Integer): IavMeshInstance;
begin
  Result := FChilds[AIndex];
end;

function TavMeshInstance.GetMesh: IavMesh;
begin
  Result := FMesh;
end;

function TavMeshInstance.GetName: String;
begin
  Result := FName;
end;

function TavMeshInstance.GetParent: IavMeshInstance;
begin
  Result := IavMeshInstance(FParent);
end;

function TavMeshInstance.GetTransform: TMat4;
begin
  Result := FLocalTransform;
end;

procedure TavMeshInstance.SetArmature(const AValue: IavArmature; const AGroupNames: array of string);
var
  i: Integer;
begin
  FArm := AValue;
  FBonePretransform := FLocalTransform;
  FLocalTransform := IdentityMat4;
  SetLength(FBoneRemap, Length(AGroupNames));
  for i := 0 to Length(FBoneRemap) - 1 do
    FBoneRemap[i] := FArm.IndexOfBone(AGroupNames[i]);
end;

procedure TavMeshInstance.SetMesh(const AValue: IavMesh);
begin
  FMesh := AValue;
end;

procedure TavMeshInstance.SetName(const AValue: string);
begin
  FName := AValue;
end;

procedure TavMeshInstance.SetParent(const AValue: IavMeshInstance);
begin
  FParent := Pointer(AValue);
end;

procedure TavMeshInstance.SetTransform(const AValue: TMat4);
begin
  if FLocalTransform = AValue then Exit;
  FLocalTransform := AValue;
  FPoseAbsValid := False;
end;

function TavMeshInstance.ChildsCount: Integer;
begin
  Result := FChilds.Count;
end;

procedure TavMeshInstance.DelChild(const AIndex: Integer);
var n: Integer;
begin
  n := FChilds.Count-1;
  FChilds[AIndex] := FChilds[n];
  FChilds.Delete(n);
end;

procedure TavMeshInstance.AddChild(const AChild: IavMeshInstance);
begin
  if AChild = nil then Exit;
  FChilds.Add(AChild);
  IavMeshInstanceInternal(AChild).Parent := Self;
end;

function TavMeshInstance.IndexOf(const AChild: IavMeshInstance): Integer;
begin
  Result := FChilds.IndexOf(AChild);
end;

procedure TavMeshInstance.SetAnimationPose(const AnimState: array of TMeshAnimationState);
begin
  if Assigned(FArm) then
  begin
    FArm.GetPoseData(FPoseTransform, FBoneRemap, AnimState);
    FPoseAbsValid := False;
  end
  else
  begin
    SetLength(FPoseTransform, 1);
    FPoseTransform[0] := IdentityMat4;
  end;
end;

function TavMeshInstance.LocalPose: TMat4Arr;
begin
  if Length(FPoseTransform) = 0 then
    SetAnimationPose([]);
  Result := FPoseTransform;
end;

function TavMeshInstance.AbsPose: TMat4Arr;
var lPose: TMat4Arr;
  i: Integer;
begin
  if not FPoseAbsValid then
  begin
    lPose := LocalPose;
    if Length(lPose) <> Length(FPoseAbs) then
      SetLength(FPoseAbs, Length(lPose));
    for i := 0 to Length(lPose) - 1 do
      FPoseAbs[i] := FBonePretransform*lPose[i]*FLocalTransform;
    FPoseAbsValid := True;
  end;
  Result := FPoseAbs;
end;

function TavMeshInstance.IndexOfBone(const ABoneName: string): Integer;
var i: Integer;
begin
  Result := -1;
  if Armature = nil then Exit;
  Result := Armature.IndexOfBone(ABoneName);
  if Result < 0 then Exit;
  for i := 0 to Length(FBoneRemap) - 1 do
  begin
    if FBoneRemap[i] = Result then
    begin
      Result := i;
      Exit;
    end;
  end;
  Result := -1;
end;

function TavMeshInstance.Clone(const NewInstanceName: string): IavMeshInstance;
var inst: TavMeshInstance;
    intf: IavMeshInstance;
begin
  inst := TavMeshInstance.Create;
  intf := inst;
  inst.SetMesh(Mesh);
  inst.SetName(NewInstanceName);
  if Assigned(Parent) then
    Parent.AddChild(inst);
  inst.FArm := Armature;
  inst.FBoneRemap := FBoneRemap;
  inst.Transform := Transform;
  inst.FBonePretransform := FBonePretransform;
  Result := intf;
end;

procedure TavMeshInstance.AfterConstruction;
begin
  inherited AfterConstruction;
  FChilds := TChildList.Create;
  FBonePretransform := IdentityMat4;
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

function TavMesh.CreateInstance(const AInstanceName: string): IavMeshInstance;
var instInt: IavMeshInstanceInternal;
begin
  instInt := TavMeshInstance.Create;
  instInt.Mesh := self;
  instInt.Name := AInstanceName;
  Result := instInt;
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

function TavArmature.IndexOfBone(const AName: string): Integer;
begin
  if not FBoneIndex.TryGetValue(AName, Result) then
    Result := -1;
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

procedure TavArmature.GetPoseData(var Matrices: TMat4Arr; const RemapIndices: TIntArr; const AnimState: array of TMeshAnimationState);
  procedure FillBoneData_Recursive(var AbsTransform: TMat4Arr; const animTransform: TMat4Arr; const bone: IavBone; const parentTransform: TMat4);
  var i: Integer;
  begin
    AbsTransform[bone.Index] := animTransform[bone.Index]*parentTransform;
    for i := 0 to bone.ChildsCount - 1 do
      FillBoneData_Recursive(AbsTransform, animTransform, bone.Child[i], AbsTransform[bone.Index]);
  end;

var i, j, BoneInd: Integer;
    m: TMat4;

    boneWeight: TSingleArr;
    boneTransform: TMat4Arr;

    rBones: TavBoneArr;
    currAnim: IavAnimation;
    w: Single;
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
      w := AnimState[i].Weight;
      if w < 0.01 then Continue;

      if boneWeight[BoneInd] = 0 then
        boneTransform[BoneInd] := m*AnimState[i].Weight
      else
        boneTransform[BoneInd] := boneTransform[BoneInd] + m*w;

      boneWeight[BoneInd] := boneWeight[BoneInd] + w;
    end;
  end;

  for i := 0 to Length(boneTransform) - 1 do
    if (boneWeight[i] > 0.001) then
      boneTransform[i] := boneTransform[i] * (1 / boneWeight[i]);

  rBones := RootBones;
  for i := 0 to Length(rBones) - 1 do
    FillBoneData_Recursive(boneTransform, boneTransform, rBones[i], IdentityMat4);

  if Length(Matrices) <> Length(RemapIndices) then
    SetLength(Matrices, Length(RemapIndices));
  for i := 0 to Length(Matrices) - 1 do
  begin
    j := RemapIndices[i];
    if j < 0 then
      Matrices[i] := IdentityMat4
    else
      Matrices[i] := boneTransform[j];
  end;
{
  if Length(Matrices) <> Length(boneTransform) then
    SetLength(Matrices, Length(boneTransform));

  for i := 0 to Length(Matrices) - 1 do
    Matrices[i] := Matrices[i]*MeshTransform;
}
  currAnim := nil;
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
  if Assigned(FParent) then
    Result := FTransform * Parent.AbsTransform
  else
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

