unit avMesh;
{$I avConfig.inc}

interface

uses
  Classes, SysUtils, intfUtils,
  avTypes, avTess, avContnrs, mutils, avTexLoader;

const
  KeyFramePerMSec = 1/30;
  Default_GrowSpeed = 70;
  Default_FadeSpeed = 70;

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
  IMeshVertices = {$IfDef FPC}specialize{$EndIf} IArray<TMeshVertex>;
  TMeshVertices = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TMeshVertex>;

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
  TMeshAnimationStateArr = array of TMeshAnimationState;

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

    function BonesCount: Integer;
    function GetBoneName(const AIndex: Integer): string;
    function FindBone(const ABoneRemapedName: string): Integer;

    property Name: String read GetName;
    property BBox: TAABB read GetBBox;
    property Vert: IMeshVertices read GetVert;
    property Ind : IIndices read GetInd;

    function CreateInstance(const AInstanceName: string): IavMeshInstance;
  end;
  IavMeshes = {$IfDef FPC}specialize{$EndIf} IHashMap<string, IavMesh>;
  TavMeshes = {$IfDef FPC}specialize{$EndIf} THashMap<string, IavMesh>;

  { IavPose }

  IavPose = interface
    function Armature : IavArmature;

    procedure SetAnimationState(const AnimState: array of TMeshAnimationState);

    procedure BumpPoseStateID;
    function  PoseStateID : Int64;
    function  AbsPose: TMat4Arr;
  end;

  { IavMeshInstance }

  IavMeshInstance = interface (IWeakedInterface)
    function GetChild(const AIndex: Integer): IavMeshInstance;
    function GetMesh: IavMesh;
    function GetName: String;
    function GetParent: IavMeshInstance;
    function GetPose: IavPose;
    function GetTransform: TMat4;
    procedure SetPose(const AValue: IavPose);
    procedure SetTransform(const AValue: TMat4);
    //--------------------------
    property Name     : string      read GetName;
    property Transform: TMat4       read GetTransform write SetTransform;
    property Mesh     : IavMesh     read GetMesh;

    property Parent: IavMeshInstance read GetParent;

    function ChildsCount: Integer;
    property Child[const AIndex: Integer]: IavMeshInstance read GetChild;
    procedure DelChild(const AIndex: Integer);
    procedure AddChild(const AChild: IavMeshInstance);
    function IndexOf(const AChild: IavMeshInstance): Integer;

    function GetSingleBoneTransform(const ABoneRemapedIndex: Integer): TMat4;

    function PoseStateID: Int64;
    property Pose: IavPose read GetPose write SetPose;
    function PoseArray: TMat4Arr;

    function Clone(const NewInstanceName: string): IavMeshInstance;
  end;
  IavMeshInstances = {$IfDef FPC}specialize{$EndIf} IHashMap<string, IavMeshInstance>;
  TavMeshInstances = {$IfDef FPC}specialize{$EndIf} THashMap<string, IavMeshInstance>;
  IavMeshInstanceArray = {$IfDef FPC}specialize{$EndIf} IArray<IavMeshInstance>;
  TavMeshInstanceArray = {$IfDef FPC}specialize{$EndIf} TArray<IavMeshInstance>;

  { IavBone }

  IavBone = interface
    function GetChild(index: Integer): IavBone;
    function GetChildsCount: Integer;
    function GetIndex: Integer;
    function GetName: String;
    function GetParent: IavBone;
    function GetHead: TVec3;
    function GetTail: TVec3;
    function GetTransform: TMat4;
    procedure SetTransform(AValue: TMat4);

    property Name: String read GetName;
    property Index: Integer read GetIndex;
    property Head: TVec3 read GetHead;
    property Tail: TVec3 read GetTail;
    property Transform: TMat4 read GetTransform write SetTransform;

    property Parent: IavBone read GetParent;
    property ChildsCount: Integer read GetChildsCount;
    property Child[ChildIndex: Integer]: IavBone read GetChild;

    function AbsTransform: TMat4;
  end;
  TavBoneArr = array of IavBone;

  { IavSingleBoneAnimation }

  IavSingleBoneAnimation = interface
    function BoneName       : string;
    function AnimationName  : string;
    function Frames: TMat4Arr;
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
    function GetLocalBoneIndex(const AArmatureBoneIndex: Integer): Integer;
    procedure GetBoneTransform(const ALocalBoneIndex: Integer; const AFrame: Single; out ArmatureBoneIndex: Integer; out Transform: TMat4);

    function ExtractAnimation(const ABoneName: string): IavSingleBoneAnimation;

    property Armature: IavArmature read GetArmature;
    property Name: String read GetName;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FrameCount: Integer read GetFrameCount;
    property Frame: Single read GetFrame write SetFrame;
  end;

  { IavArmature }

  IavArmature = interface (IWeakedInterface)
    function GetAnim(index: Integer): IavAnimation;
    function GetAnimCount: Integer;
    function GetBone(index: Integer): IavBone;
    function GetBonesCount: Integer;
    function GetName: String;

    property BonesCount: Integer read GetBonesCount;
    property Bone[index: Integer]: IavBone read GetBone;
    function RootBones: TavBoneArr;

    property AnimCount: Integer read GetAnimCount;
    property Anim[index: Integer]: IavAnimation read GetAnim;
    function FindAnim(const AName: string): IavAnimation;

    property Name: String read GetName;

    function IndexOfBone(const AName: string): Integer;
    function FindBone(const AName: string): IavBone;
  end;

  { IavAnimationController }

  IavAnimationController = interface
    function GetPose: IavPose;
    procedure SetPose(const AValue: IavPose);
    property Pose: IavPose read GetPose write SetPose;

    function AnimationState: TMeshAnimationStateArr;

    procedure SetTime(const ATime: Int64);
    procedure AnimationStartStop(const AAnimationName: string; DoStart: Boolean; GrowSpeed: Single = Default_GrowSpeed);
    procedure AnimationStart(const AAnimationName: string; GrowSpeed: Single = Default_GrowSpeed);
    procedure AnimationStop (const AAnimationName: string; FadeSpeed: Single = Default_FadeSpeed);
    procedure AnimationStartAndStopOther(const AAnimationNames: array of string; GrowFadeSpeed: Single = Default_FadeSpeed);
  end;

  IavArmatures = {$IfDef FPC}specialize{$EndIf} IHashMap<string, IavArmature>;
  TavArmatures = {$IfDef FPC}specialize{$EndIf} THashMap<string, IavArmature>;

procedure LoadFromStream(const stream: TStream; out meshes: IavMeshes; out meshInst: IavMeshInstances; TexManager: ITextureManager = Nil);
procedure LoadFromFile(const FileName: string; out meshes: IavMeshes; out meshInst: IavMeshInstances; const TexManager: ITextureManager = Nil);

function Create_IavAnimationController(const APose: IavPose; const ATime: Int64): IavAnimationController;

implementation

uses
  Math;

type
  { IavBoneInternal }

  IavBoneInternal = interface (IavBone)
    procedure SetIndex(AValue: Integer);
    procedure SetName(AValue: String);
    procedure SetParent(AValue: IavBone);
    procedure SetTail(const AValue: TVec3);
    procedure SetHead(const AValue: TVec3);

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
    procedure SetBones(const ABones: TStringArr);

    property Name: String read GetName write SetName;
    property BBox: TAABB read GetBBox write SetBBox;

    procedure AddMaterial(const AMat: TMeshMaterial; const AMatMap: TMeshMaterialMaps);
  end;

  { IavMeshInstanceInternal }

  IavMeshInstanceInternal = interface (IavMeshInstance)
    procedure SetParent(const AValue: IavMeshInstance);
    procedure SetMesh(const AValue: IavMesh);
    procedure SetName(const AValue: string);

    property Parent   : IavMeshInstance read GetParent   write SetParent;
    property Name     : string          read GetName     write SetName;
    property Mesh     : IavMesh         read GetMesh     write SetMesh;
  end;

  IavPoseInternal = interface (IavPose)
    function Clone(): IavPose;
  end;

  { TavBone }

  TavBone = class (TInterfacedObjectEx, IavBone, IavBoneInternal)
  private
    FParent   : Pointer;
    FChilds   : Array Of IavBone;
    FName     : String;
    FIndex    : Integer;
    FHead     : TVec3;
    FTail     : TVec3;
    FTransform: TMat4;

    function GetChild(index: Integer): IavBone;
    function GetChildsCount: Integer;
    function GetIndex: Integer;
    function GetName: String;
    function GetParent: IavBone;
    function GetHead: TVec3;
    function GetTail: TVec3;
    function GetTransform: TMat4;
    procedure SetIndex(AValue: Integer);
    procedure SetName(AValue: String);
    procedure SetParent(AValue: IavBone);
    procedure SetTail(const AValue: TVec3);
    procedure SetHead(const AValue: TVec3);
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

  TavArmature = class (TWeakedInterfacedObject, IavArmature, IavArmatureInternal)
  private type
    IBoneHash = {$IfDef FPC}specialize{$EndIf} IHashMap<string, Integer>;
    TBoneHash = {$IfDef FPC}specialize{$EndIf} THashMap<string, Integer>;
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
    function GetName: string;
    procedure SetName(const AValue: String);
  public
    property BonesCount: Integer read GetBonesCount;
    property Bone[index: Integer]: IavBone read GetBone;
    function RootBones: TavBoneArr;

    property AnimCount: Integer read GetAnimCount;
    property Anim[index: Integer]: IavAnimation read GetAnim;

    property Name: String read GetName write SetName;

    function IndexOfBone(const AName: string): Integer;
    function FindBone(const AName: string): IavBone;
    procedure AddBone(const ABone: IavBoneInternal);
    procedure AddAnimation(const AAnim: IavAnimationInternal);
    function FindAnim(const AName: string): IavAnimation;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  { TavPose }

  TavPose = class (TInterfacedObjectEx, IavPose, IavPoseInternal)
  private
    FArmature : IavArmature;

    FBoneTransform : TMat4Arr;
    FPoseStateID   : Int64;

    function Armature : IavArmature;

    procedure SetAnimationState(const AnimState: array of TMeshAnimationState);

    procedure BumpPoseStateID;
    function  PoseStateID : Int64;
    function  AbsPose     : TMat4Arr;
  public
    constructor Create(const AArmature: IavArmature);
    function Clone(): IavPose;
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
    FBones: TStringArr;

    function GetMaterial(const AIndex: Integer): TMeshMaterial;
    function GetMaterialMaps(const AIndex: Integer): TMeshMaterialMaps;
    function GetBBox: TAABB;
    function GetInd: IIndices;
    function GetName: String;
    function GetVert: IMeshVertices;
    procedure SetBBox(const AValue: TAABB);
    procedure SetName(const AValue: String);

    function BonesCount: Integer;
    function GetBoneName(const AIndex: Integer): string;
    function FindBone(const ABoneRemapedName: string): Integer;
    procedure SetBones(const ABones: TStringArr);
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

  TavMeshInstance = class (TWeakedInterfacedObject, IavMeshInstance, IavMeshInstanceInternal)
  private type
    IChildList = {$IfDef FPC}specialize{$EndIf} IArray<IavMeshInstance>;
    TChildList = {$IfDef FPC}specialize{$EndIf} TArray<IavMeshInstance>;
  private
    FParent: Pointer;
    FMesh: IavMesh;
    FChilds: IChildList;
    FName: string;

    FTransform: TMat4;
    FPoseArray: TMat4Arr;
    FPoseStateID : Int64;

    FPose : IavPose;
    FSavedPoseStateID: Int64;
    FBoneRemap: TIntArr;
    FBoneBindTransform: TMat4;

    procedure BumpPoseStateID;
    function PoseStateID: Int64;
    function PoseArray: TMat4Arr;
  public
    function GetChild(const AIndex: Integer): IavMeshInstance;
    function GetMesh: IavMesh;
    function GetName: String;
    function GetParent: IavMeshInstance;
    function GetPose: IavPose;
    function GetTransform: TMat4;
    procedure SetPose(const AValue: IavPose);
    procedure SetMesh(const AValue: IavMesh);
    procedure SetName(const AValue: string);
    procedure SetParent(const AValue: IavMeshInstance);
    procedure SetTransform(const AValue: TMat4);
    //--------------------------

    property Name     : string      read GetName;
    property Transform: TMat4       read GetTransform write SetTransform;
    property Mesh     : IavMesh     read GetMesh;

    property Parent: IavMeshInstance read GetParent write SetParent;

    function ChildsCount: Integer;
    property Child[const AIndex: Integer]: IavMeshInstance read GetChild;
    procedure DelChild(const AIndex: Integer);
    procedure AddChild(const AChild: IavMeshInstance);
    function IndexOf(const AChild: IavMeshInstance): Integer;

    function GetSingleBoneTransform(const ABoneRemapedIndex: Integer): TMat4;

    function Clone(const NewInstanceName: string): IavMeshInstance;
  public
    procedure AfterConstruction; override;
  end;

  { TavSingleBoneAnimation }

  TavSingleBoneAnimation = class (TInterfacedObjectEx, IavSingleBoneAnimation)
  private
    FBoneName: string;
    FAnimationName: string;
    FFrames: TMat4Arr;
    function BoneName: string;
    function AnimationName: string;
    function Frames: TMat4Arr;
  public
    constructor Create(const ABoneName, AAnimationName: string; const AFrames: TMat4Arr);
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
    function GetLocalBoneIndex(const AArmatureBoneIndex: Integer): Integer;
    procedure GetBoneTransform(const ALocalBoneIndex: Integer; const AFrame: Single; out ArmatureBoneIndex: Integer; out Transform: TMat4);

    function ExtractAnimation(const ABoneName: string): IavSingleBoneAnimation;

    property Armature: IavArmature read GetArmature write SetArmature;
    property Name: String read GetName;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property FrameCount: Integer read GetFrameCount;
    property Frame: Single read GetFrame write SetFrame;
  end;

  { TavAnimationController }

  TavAnimationController = class (TInterfacedObjectEx, IavAnimationController)
  private type
    TAnimationPlayState = packed record
      StartTime: Int64;
      StopTime : Int64;
      GrowSpeed: Single;
      FadeSpeed: Single;
      procedure Calc(const ATime: Int64; out AFrame: Single; out AWeight: Single; out AComplete: Boolean);
    end;
  private
    FPose: IavPose;

    FTime : Int64;
    FAnimationStates   : TMeshAnimationStateArr;
    FAnimationPlayState: array of TAnimationPlayState;

    procedure UpdateAnimationState;
  private
    function GetPose: IavPose;
    procedure SetPose(const AValue: IavPose);
    property Pose: IavPose read GetPose write SetPose;

    function AnimationState: TMeshAnimationStateArr;

    procedure SetTime(const ATime: Int64);
    procedure AnimationStartStop(const AAnimationName: string; DoStart: Boolean; GrowSpeed: Single = Default_GrowSpeed);
    procedure AnimationStart(const AAnimationName: string; GrowSpeed: Single = Default_GrowSpeed);
    procedure AnimationStop (const AAnimationName: string; FadeSpeed: Single = Default_FadeSpeed);
    procedure AnimationStartAndStopOther(const AAnimationNames: array of string; GrowFadeSpeed: Single = Default_FadeSpeed);
  end;

  { TPNWVertex }

  TPNWVertex = packed record
    vsCoord: TVec3;
    vsNormal: TVec3;
    vsWIndex: TVec4;
    vsWeight: TVec4;
    class function Layout: IDataLayout; static;
  end;

  IPNWVertices = {$IfDef FPC}specialize{$EndIf} IArray<TPNWVertex>;
  TPNWVertices = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TPNWVertex>;

  IMeshVerticesHash = {$IfDef FPC}specialize{$EndIf} IHashMap<TMeshVertex, Integer>;
  TMeshVerticesHash = {$IfDef FPC}specialize{$EndIf} THashMap<TMeshVertex, Integer>;

const
  EmptyPNWVertex: TPNWVertex = (
    vsCoord: (x: 0; y: 0; z: 0);
    vsNormal: (x: 0; y: 0; z: 0);
    vsWIndex: (x: 0; y: 0; z: 0; w: 0);
    vsWeight: (x: 0; y: 0; z: 0; w: 0)
  );

procedure LoadFromStream(const stream: TStream; out meshes: IavMeshes; out meshInst: IavMeshInstances; TexManager: ITextureManager);
type
  IavPoses = {$IfDef FPC}specialize{$EndIf} IHashMap<string, IavPose>; //armatureName -> pose
  TavPoses = {$IfDef FPC}specialize{$EndIf} THashMap<string, IavPose>; //armatureName -> pose

  TMeshInstInfo = packed record
    Inst: IavMeshInstanceInternal;
    Parent: string;
    Mesh: string;
    //Transform: TMat4;
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
      vertexGroups: TStringArr;
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
    SetLength(vertexGroups, n);
    for i := 0 to n - 1 do
      StreamReadString(stream, vertexGroups[i]);
    mesh.SetBones(vertexGroups);

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
      i: Integer;
  begin
    //prevent warnings
    m := IdentityMat4;
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
        v: TVec3;
        m: TMat4;
        n: Integer;
    begin
      n := 0;
      ZeroClear(m, SizeOf(m));
      ZeroClear(v, SizeOf(v));

      bone := TavBone.Create;

      StreamReadString(stream, s);
      bone.Name := String(s);

      StreamReadString(stream, s);
      parent := String(s);

      stream.ReadBuffer(n, SizeOf(n));
      bone.Index := n;

      stream.ReadBuffer(m, SizeOf(m));
      bone.Transform := m;

      stream.ReadBuffer(v, SizeOf(v));
      bone.SetHead(v);

      stream.ReadBuffer(v, SizeOf(v));
      bone.SetTail(v);
    end;
    procedure LoadAnimationFromStream(const stream: TStream; out anim: IavAnimationInternal);
    var s: AnsiString;
        i: Integer;
        n: Integer;
        bones: TIntArr;
        frame: TMat4Arr;
        frameStart: Integer;
        frameEnd: Integer;
    begin
      n := 0;
      frameStart := 0;
      frameEnd := 0;
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
      n: Integer;

      bones: array of IavBoneInternal;
      boneParent: array of String;
      anim: IavAnimationInternal;
  begin
    n := 0;
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
    poses: IavPoses;
    pose: IavPose;

    arm: IavArmatureInternal;
    mesh: IavMeshInternal;
    inst: IavMeshInstance;
    instArr: array of TMeshInstInfo;
begin
  if TexManager = nil then TexManager := Create_ITextureManager;

  n := 0;

  armatures := TavArmatures.Create;
  poses := TavPoses.Create();
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
    if meshes.TryGetValue(instArr[i].Mesh, IavMesh(mesh)) then
      instArr[i].Inst.Mesh := mesh
    else
      Assert(False, 'Mesh "'+instArr[i].Mesh+'" not found for object "'+instArr[i].Inst.Name+'"');

    if instArr[i].Parent <> '' then
    begin
      if meshInst.TryGetValue(instArr[i].Parent, inst) then
        inst.AddChild(instArr[i].Inst)
      else
        if armatures.TryGetValue(instArr[i].Parent, IavArmature(arm)) then
        begin
          if not poses.TryGetValue(instArr[i].Parent, pose) then
          begin
            pose := TavPose.Create(arm);
            poses.Add(instArr[i].Parent, pose);
          end;
          instArr[i].Inst.SetPose(pose)
        end
        else
          Assert(False, 'Parent "'+instArr[i].Parent+'" not found for object "'+instArr[i].Inst.Name+'"');
    end;
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

function Create_IavAnimationController(const APose: IavPose; const ATime: Int64
  ): IavAnimationController;
begin
  Result := TavAnimationController.Create;
  Result.Pose := APose;
  Result.SetTime(ATime);
end;

{ TavSingleBoneAnimation }

function TavSingleBoneAnimation.BoneName: string;
begin
  Result := FBoneName;
end;

function TavSingleBoneAnimation.AnimationName: string;
begin
  Result := FAnimationName;
end;

function TavSingleBoneAnimation.Frames: TMat4Arr;
begin
  Result := FFrames;
end;

constructor TavSingleBoneAnimation.Create(const ABoneName,
  AAnimationName: string; const AFrames: TMat4Arr);
begin
  FBoneName := ABoneName;
  FAnimationName := AAnimationName;
  FFrames := AFrames;
end;

{ TavPose }

function TavPose.Armature: IavArmature;
begin
  Result := FArmature;
end;

procedure TavPose.SetAnimationState(const AnimState: array of TMeshAnimationState);
  procedure FillBoneData_Recursive(var AbsTransform: TMat4Arr; const animTransform: TMat4Arr; const bone: IavBone; const parentTransform: TMat4);
  var i: Integer;
  begin
    AbsTransform[bone.Index] := animTransform[bone.Index]*parentTransform;
    for i := 0 to bone.ChildsCount - 1 do
      FillBoneData_Recursive(AbsTransform, animTransform, bone.Child[i], AbsTransform[bone.Index]);
  end;
var ArmBoneCnt: Integer;
    i, j, BoneInd: Integer;
    m: TMat4;

    boneWeight: TSingleArr;
    rootBones : TavBoneArr;

    currAnim: IavAnimation;
    w: Single;
begin
  ArmBoneCnt := FArmature.BonesCount;
  if ArmBoneCnt = 0 then Exit;

  SetLength(boneWeight, ArmBoneCnt);
  ZeroClear(boneWeight[0], Length(boneWeight)*SizeOf(boneWeight[0]));
  SetLength(FBoneTransform, ArmBoneCnt);
  ZeroClear(FBoneTransform[0], Length(FBoneTransform)*SizeOf(FBoneTransform[0]));

  for i := 0 to ArmBoneCnt - 1 do
    FBoneTransform[i] := FArmature.Bone[i].Transform;

  for i := 0 to Length(AnimState) - 1 do
  begin
    currAnim := FArmature.Anim[AnimState[i].Index];
    w := AnimState[i].Weight;
    if w < 0.01 then Continue;

    for j := 0 to currAnim.BonesCount - 1 do
    begin
      currAnim.GetBoneTransform(j, AnimState[i].Frame, BoneInd, m);

      if boneWeight[BoneInd] = 0 then
        FBoneTransform[BoneInd] := m*w
      else
        FBoneTransform[BoneInd] := FBoneTransform[BoneInd] + m*w;

      boneWeight[BoneInd] := boneWeight[BoneInd] + w;
    end;
  end;

  for i := 0 to Length(FBoneTransform) - 1 do
    if (boneWeight[i] > 0.001) then
      FBoneTransform[i] := FBoneTransform[i] * (1 / boneWeight[i]);

  rootBones := FArmature.RootBones;
  for i := 0 to Length(rootBones) - 1 do
    FillBoneData_Recursive(FBoneTransform, FBoneTransform, rootBones[i], IdentityMat4);

  BumpPoseStateID;
end;

procedure TavPose.BumpPoseStateID;
begin
  Inc(FPoseStateID);
end;

function TavPose.PoseStateID: Int64;
begin
  Result := FPoseStateID;
end;

function TavPose.AbsPose: TMat4Arr;
begin
  Result := FBoneTransform;
end;

constructor TavPose.Create(const AArmature: IavArmature);
begin
  FArmature := AArmature;
  SetAnimationState([]);
end;

function TavPose.Clone(): IavPose;
var pObj: TavPose;
begin
  pObj := TavPose.Create(FArmature);
  Result := pObj;

  pObj.FBoneTransform := Copy(FBoneTransform, 1, Length(FBoneTransform));
  pObj.FPoseStateID := 0;
end;

{ TavAnimationController.TAnimationPlayState }

procedure TavAnimationController.TAnimationPlayState.Calc(const ATime: Int64;
  out AFrame: Single; out AWeight: Single; out AComplete: Boolean);
var FadeValue: Single;
begin
  AComplete := False;
  if ATime < StartTime then
  begin
    AWeight := 0;
    AFrame := 0;
    Exit;
  end;

  AFrame := (ATime - StartTime) * KeyFramePerMSec;
  AWeight := Min(1, (ATime - StartTime) / GrowSpeed);

  if StopTime > 0 then
  begin
    if ATime > StopTime then
      FadeValue := 1
    else
      FadeValue := Min(1, (ATime - StopTime) / FadeSpeed);
    AWeight := Max(0, AWeight - FadeValue);
    if AWeight = 0 then AComplete := True;
  end;
end;

{ TavAnimationController }

procedure TavAnimationController.UpdateAnimationState;
var i, n: Integer;
    currTime: Int64;
    Complete: Boolean;
    inst: IavMeshInstance;
begin
  if Length(FAnimationStates) = 0 then Exit;

  //update
  currTime := FTime;
  for i := 0 to Length(FAnimationStates) - 1 do
  begin
    FAnimationPlayState[i].Calc(currTime, FAnimationStates[i].Frame, FAnimationStates[i].Weight, Complete);
    if Complete then
      FAnimationPlayState[i].StartTime := -1;
  end;

  //remove completed animations
  n := 0;
  for i := 0 to Length(FAnimationStates) - 1 do
  begin
    if FAnimationPlayState[i].StartTime < 0 then
    begin
      Inc(n);
      Continue;
    end;
    if n > 0 then
    begin
      FAnimationPlayState[i-n] := FAnimationPlayState[i];
      FAnimationStates[i-n] := FAnimationStates[i];
    end;
  end;
  if n > 0 then
  begin
    SetLength(FAnimationPlayState, Length(FAnimationPlayState) - n);
    SetLength(FAnimationStates, Length(FAnimationStates) - n);
  end;
end;

function TavAnimationController.GetPose: IavPose;
begin
  Result := FPose;
end;

procedure TavAnimationController.SetPose(const AValue: IavPose);
begin
  if FPose = AValue then Exit;
  FPose := AValue;
  FAnimationStates   := nil;
  FAnimationPlayState:= nil;
end;

function TavAnimationController.AnimationState: TMeshAnimationStateArr;
begin
  Result := FAnimationStates;
end;

procedure TavAnimationController.SetTime(const ATime: Int64);
begin
  if FTime = ATime then Exit;
  FTime := ATime;
  UpdateAnimationState;
  FPose.SetAnimationState(FAnimationStates);
end;

procedure TavAnimationController.AnimationStartStop(const AAnimationName: string; DoStart: Boolean; GrowSpeed: Single);
begin
  if DoStart then
    AnimationStart(AAnimationName, GrowSpeed)
  else
    AnimationStop(AAnimationName, GrowSpeed);
end;

procedure TavAnimationController.AnimationStart(const AAnimationName: string; GrowSpeed: Single);
var inst: IavMeshInstance;
    arm : IavArmature;
    anim: IavAnimation;
    animIndex: Integer;
    n: Integer;
    i: Integer;
begin
  arm := Pose.Armature;
  if arm = nil then Exit;
  anim := arm.FindAnim(AAnimationName);
  if anim = nil then Exit;
  animIndex := anim.Index;

  for i := 0 to Length(FAnimationStates) - 1 do
    if FAnimationStates[i].Index = animIndex then Exit;

  n := Length(FAnimationStates);

  SetLength(FAnimationStates, n+1);
  FAnimationStates[n].Frame := 0;
  FAnimationStates[n].Index := animIndex;
  FAnimationStates[n].Weight := 0;

  SetLength(FAnimationPlayState, n+1);
  FAnimationPlayState[n].StartTime := FTime;
  FAnimationPlayState[n].GrowSpeed := GrowSpeed;
  FAnimationPlayState[n].StopTime := -1;
  FAnimationPlayState[n].FadeSpeed := Default_FadeSpeed;
end;

procedure TavAnimationController.AnimationStop(const AAnimationName: string; FadeSpeed: Single);
var anim: IavAnimation;
    animIndex: Integer;
    i: Integer;
    arm : IavArmature;
begin
  arm := Pose.Armature;
  if arm = nil then Exit;
  anim := arm.FindAnim(AAnimationName);
  Assert(Assigned(anim), 'Animation with name ' + AAnimationName + ' not found');
  animIndex := anim.Index;

  for i := 0 to Length(FAnimationStates) - 1 do
    if FAnimationStates[i].Index = animIndex then
    begin
      if FAnimationPlayState[i].StopTime >= 0 then Exit;
      FAnimationPlayState[i].StopTime := FTime + Math.Ceil(FadeSpeed);
      FAnimationPlayState[i].FadeSpeed := FadeSpeed;
      Break;
    end;
end;

procedure TavAnimationController.AnimationStartAndStopOther(const AAnimationNames: array of string; GrowFadeSpeed: Single);
  function InCurrentAnimations(const AAnimName: string): Boolean;
  var i: Integer;
  begin
    Result := False;
    for i := 0 to Length(AAnimationNames) - 1 do
      if AAnimationNames[i] = AAnimName then Exit(True);
  end;
var i: Integer;
    s: String;
    arm: IavArmature;
begin
  arm := Pose.Armature;
  if arm = nil then Exit;

  for i := 0 to Length(FAnimationStates) - 1 do
  begin
    s := arm.Anim[FAnimationStates[i].Index].Name;
    if not InCurrentAnimations(s) then
      AnimationStop(s, GrowFadeSpeed);
  end;

  for i := 0 to Length(AAnimationNames) - 1 do
    AnimationStart(AAnimationNames[i], GrowFadeSpeed);
end;

{ TavMeshInstance }

function TavMeshInstance.PoseStateID: Int64;
begin
  Result := FPoseStateID;
  if Assigned(FPose) then Inc(Result, FPose.PoseStateID);
end;

function TavMeshInstance.PoseArray: TMat4Arr;
var newID: Int64;
    poseArr: TMat4Arr;
    i, boneIndex: Integer;
begin
  if FPose = nil then
  begin
    if Length(FPoseArray) <> 1 then SetLength(FPoseArray, 1);
    FPoseArray[0] := FTransform;
  end
  else
  begin
    newID := FPose.PoseStateID;
    if newID <> FSavedPoseStateID then
    begin
      FSavedPoseStateID := newID;
      if Length(FPoseArray) <> Length(FBoneRemap) then SetLength(FPoseArray, Length(FBoneRemap));
      poseArr := FPose.AbsPose;
      for i := 0 to Length(FPoseArray) - 1 do
      begin
        boneIndex := FBoneRemap[i];
        if boneIndex >= 0 then
          FPoseArray[i] := FBoneBindTransform * poseArr[boneIndex] * FTransform
        else
          FPoseArray[i] := FBoneBindTransform * FTransform;
      end;
    end;
  end;
  Result := FPoseArray;
end;

procedure TavMeshInstance.BumpPoseStateID;
begin
  Inc(FPoseStateID);
  if FPose <> nil then
    Dec(FSavedPoseStateID);
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

function TavMeshInstance.GetPose: IavPose;
begin
  Result := FPose;
end;

function TavMeshInstance.GetTransform: TMat4;
begin
  Result := FTransform;
end;

procedure TavMeshInstance.SetPose(const AValue: IavPose);
var
  i: Integer;
begin
  if FPose = AValue then Exit;
  FPose := AValue;
  if Length(FBoneRemap) <> FMesh.BonesCount then
    SetLength(FBoneRemap, FMesh.BonesCount);
  for i := 0 to Length(FBoneRemap) - 1 do
    FBoneRemap[i] := FPose.Armature.IndexOfBone(FMesh.GetBoneName(i));
  FBoneBindTransform := FTransform;
  FTransform := IdentityMat4;

  FSavedPoseStateID := FPose.PoseStateID - 1;
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
  FPose.BumpPoseStateID;
end;

procedure TavMeshInstance.SetTransform(const AValue: TMat4);
begin
  if FTransform = AValue then Exit;
  FTransform := AValue;
  BumpPoseStateID;
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

function TavMeshInstance.GetSingleBoneTransform(const ABoneRemapedIndex: Integer): TMat4;
begin
  Result := FBoneBindTransform * FPose.AbsPose[ABoneRemapedIndex];
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
  if Assigned(FPose) then
    inst.SetPose(IavPoseInternal(FPose).Clone());
  inst.FTransform := FTransform;
  Result := intf;
end;

procedure TavMeshInstance.AfterConstruction;
begin
  inherited AfterConstruction;
  FChilds := TChildList.Create;
  FTransform := IdentityMat4;
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

function TavAnimation.GetLocalBoneIndex(const AArmatureBoneIndex: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FBones) - 1 do
    if FBones[i] = AArmatureBoneIndex then Exit(i);
end;

procedure TavAnimation.GetBoneTransform(const ALocalBoneIndex: Integer; const AFrame: Single; out ArmatureBoneIndex: Integer; out Transform: TMat4);
var Frame1, Frame2, FrameCnt: Integer;
    FrameWeight: Single;
begin
  ArmatureBoneIndex := FBones[ALocalBoneIndex];

  Frame1 := Math.Floor(AFrame);
  Frame2 := Math.Ceil(AFrame);
  FrameWeight := AFrame - Frame1;

  FrameCnt := FrameCount;
  Frame1 := Frame1 mod FrameCnt;
  Frame2 := Frame2 mod FrameCnt;
  if Frame1 < 0 then Inc(Frame1, FrameCnt);
  if Frame2 < 0 then Inc(Frame2, FrameCnt);

  Transform := Lerp(FFrames[Frame1][ALocalBoneIndex], FFrames[Frame2][ALocalBoneIndex], FrameWeight);
end;

function TavAnimation.ExtractAnimation(const ABoneName: string): IavSingleBoneAnimation;
var idx, idx2, i: Integer;
    frms: TMat4Arr;
begin
  Result := nil;
  idx := Armature.IndexOfBone(ABoneName);
  if idx < 0 then Exit;
  idx2 := -1;
  for i := 0 to Length(FBones) - 1 do
    if FBones[i] = idx then
    begin
      idx2 := i;
      break;
    end;
  if idx2 < 0 then Exit;
  SetLength(frms, Length(FFrames));
  for i := 0 to Length(FFrames) - 1 do
    frms[i] := FFrames[i][idx2];
  Result := TavSingleBoneAnimation.Create(ABoneName, FName, frms);
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

function TavMesh.BonesCount: Integer;
begin
  Result := Length(FBones);
end;

function TavMesh.GetBoneName(const AIndex: Integer): string;
begin
  Result := FBones[AIndex];
end;

function TavMesh.FindBone(const ABoneRemapedName: string): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Length(FBones) - 1 do
    if FBones[i] = ABoneRemapedName then Exit(i);;
end;

procedure TavMesh.SetBones(const ABones: TStringArr);
begin
  FBones := ABones;
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

function TavArmature.GetName: string;
begin
  Result := FName;
end;

procedure TavArmature.SetName(const AValue: String);
begin
  FName := AValue;
end;

function TavArmature.RootBones: TavBoneArr;
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

function TavBone.GetHead: TVec3;
begin
  Result := FHead;
end;

function TavBone.GetTail: TVec3;
begin
  Result := FTail;
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

procedure TavBone.SetTail(const AValue: TVec3);
begin
  FTail := AValue;
end;

procedure TavBone.SetHead(const AValue: TVec3);
begin
  FHead := AValue;
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

