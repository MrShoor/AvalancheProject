unit avModel;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  mutils,
  avBase, avRes, avTess, avTypes, avContnrs, avMesh, avTexLoader;

const
  KeyFramePerMSec = 1/30;
  Default_GrowSpeed = 70;
  Default_FadeSpeed = 70;

type
  TavModelCollection = class;

  { TModelInstanceGPUData_Base }

  TModelInstanceGPUData_Base = class (TInterfacedObjectEx, IVerticesData)
  protected
    function GetBasicPtr: Pointer;
    function GetOffset(const FieldPtr: Pointer): Integer;

    function AddLayoutFields(const LBuilder: ILayoutBuilder): ILayoutBuilder; virtual; abstract;
  public
    function VerticesCount: Integer;
    function Layout: IDataLayout; virtual; abstract;
    function Data: TPointerData;
  end;

  { TModelInstanceGPUData }

  TModelInstanceGPUData = class (TModelInstanceGPUData_Base)
  protected
    class var Layout_ModelInstanceGPUData: IDataLayout;
    function AddLayoutFields(const LBuilder: ILayoutBuilder): ILayoutBuilder; override;
  public
    aiBoneMatDifNormOffset: TVec4;
    function Layout: IDataLayout; override;
  end;


  { IavModelInstance }

  IavModelInstance = interface
    function GetAutoUpdateAnimation: Boolean;
    function GetBoneTransform(const AName: string): TMat4;
    function GetTransform: TMat4;
    procedure SetAutoUpdateAnimation(AValue: Boolean);
    procedure SetBoneTransform(const AName: string; const AValue: TMat4);
    procedure SetTransform(const AValue: TMat4);

    procedure UpdateAnimationStates(const ATime: Int64);

    function AnimationCount: Integer;
    function AnimationName(const AIndex: Integer): string;
    procedure AnimationStartStop(const AAnimationName: string; DoStart: Boolean; GrowSpeed: Single = Default_GrowSpeed);
    procedure AnimationStart(const AAnimationName: string; GrowSpeed: Single = Default_GrowSpeed);
    procedure AnimationStop (const AAnimationName: string; FadeSpeed: Single = Default_FadeSpeed);
    procedure AnimationStartOnly(const AAnimationNames: array of string; GrowFadeSpeed: Single = Default_FadeSpeed);

    function Collection: TavModelCollection;
    function ModelName: String;

    property Transform: TMat4 read GetTransform write SetTransform;
    property BoneTransform[const AName: string]: TMat4 read GetBoneTransform write SetBoneTransform;

    property AutoUpdateAnimation: Boolean read GetAutoUpdateAnimation write SetAutoUpdateAnimation;
  end;

  IModelInstanceArr = specialize IArray<IavModelInstance>;
  TModelInstanceArr = specialize TArray<IavModelInstance>;

  TBoneTransformHandle = Pointer;

  { TavModelCollection }

  TavModelCollection = class (TavMainRenderChild)
  private type
    { TModel }

    TModel = class
    private
      Owner: TavModelCollection;
      VBHandle: TVBManagedHandle;
      IBHandle: TIBManagedHandle;
      FMapTex : TavTexture;
      FInstances: TList;

      MaterialOffset: Single;
      DiffuseOffset : Single;
      NormalsOffset : Single;

      procedure UnlinkInstance(const Obj: TObject);
    public
      Mesh : IavMesh;
      function CreateInstance(const ModelTransform: TMat4): IavModelInstance;
      constructor Create;
      destructor Destroy; override;
    end;

    { TavMaterialMap }

    TavMaterialMap = class(TavTexture)
    private type
      TMaterialRow = packed record
        Material: TMeshMaterial;
        Dummy   : TVec2;         //align to whole pixels count
      end;
    private
      FMaterials: array of TMaterialRow;
    protected
      function DoBuild: Boolean; override;
    public
      procedure AddMaterial(const AMaterial: TMeshMaterial);
      function GetMaterialCount: Integer;
    end;

    { TavBoneTransformMap }

    TavBoneTransformMap = class(TavTexture)
    private type
      TTransformNode = class
        Range     : IMemRange;
        Mat       : TMat4Arr;
        DirtyIndex: Integer;
        function Size: Integer; Inline;
      end;
      TNodes = specialize TNodeManager<TTransformNode>;
    private
      FNodes : TNodes;
    protected
      function TexWidth: Integer;

      function DoBuild: Boolean; override;
    public
      function  AddMatrices(const m: TMat4Arr): TBoneTransformHandle;
      procedure DeleteMatrices(var AHandle: TBoneTransformHandle);
      procedure UpdateMatrices(const AHandle: TBoneTransformHandle; const m: TMat4Arr);
      function  GetOffset(const AHandle: TBoneTransformHandle): Integer;

      procedure AfterConstruction; override;
      destructor Destroy; override;
    end;

    IModelHash = specialize IHashMap<String, TModel, TMurmur2HashString>;
    TModelHash = specialize THashMap<String, TModel, TMurmur2HashString>;

    TTextureKey = packed record
      Width : Integer;
      Height: Integer;
      Mips  : Integer;
    end;
  private const
    EmptyTexureKey: TTextureKey = (Width:0;Height:0;Mips:0);
  private type
    TTextureHashFunc = specialize TMurmur2Hash<TTextureKey>;
    ITextureHash = specialize IHashMap<TTextureKey, TavTexture, TTextureHashFunc>;
    TTextureHash = specialize THashMap<TTextureKey, TavTexture, TTextureHashFunc>;

    IDummyTexDataHash = specialize IHashMap<TTextureKey, ITextureData, TTextureHashFunc>;
    TDummyTexDataHash = specialize THashMap<TTextureKey, ITextureData, TTextureHashFunc>;
  private
    FVB: TavVBManaged;
    FIB: TavIBManaged;
    FInstVB: TavVBManaged;
    FMaterials: TavMaterialMap;
    FMaps: ITextureHash;
    FBoneTransform: TavBoneTransformMap;

    FDummyTexData: IDummyTexDataHash; //empty tex data for not completed materials

    FModels: IModelHash;

    procedure AddModel(const AModel: TModel);

    function ObtainDummyTextureData(const Key: TTextureKey): ITextureData;
    function ObtainMap(const Key: TTextureKey): TavTexture;

    procedure Draw(const Intances: IObjArr; SortByMaterial: Boolean); overload;
  public
    function ModelsCount: Integer;
    procedure Reset;
    function Next(out ModelName: string): Boolean;

    function CreateInstance(const ModelName: String; const ModelTransform: TMat4): IavModelInstance;

    procedure Select;
    procedure Draw(const Instances: array of IavModelInstance; SortByMaterial: Boolean = True); overload;
    procedure Draw(const Instances: array of IModelInstanceArr; SortByMaterial: Boolean = True); overload;

    procedure AddFromFile(const FileName: String; const TexManager: ITextureManager = nil);
    procedure AddFromMeshes(const AMeshes: TavMeshes);

    constructor Create(AParent: TavObject); override;
    destructor Destroy; override;
  end;

implementation

uses
  Math;

type
  TavModelInstance = class;

  { IavModelInstanceInternal }

  IavModelInstanceInternal = interface (IavModelInstance)
    function GetObj: TavModelInstance;
  end;

  { TavModelInstance }

  TavModelInstance = class(TInterfacedObject, IavModelInstance, IavModelInstanceInternal)
  private type
    TAnimationPlayState = packed record
      StartTime: Int64;
      StopTime : Int64;
      GrowSpeed: Single;
      FadeSpeed: Single;
      procedure Calc(const ATime: Int64; out AFrame: Single; out AWeight: Single; out AComplete: Boolean);
    end;
  private
    FModel: TavModelCollection.TModel;
    FInstanceIndex: Integer;
    FInstGPUData: TVBManagedHandle;

    FBoneTransformIndex: TBoneTransformHandle;
    FBoneTransform: TMat4Arr;
    FBoneTransformDirty: Boolean;

    FTransform: TMat4;
    FAutoUpdateAnimation: Boolean;

    FAnimationStates   : array of TMeshAnimationState;
    FAnimationPlayState: array of TAnimationPlayState;

    procedure OnUnlink;
    procedure OnLink;

    procedure UpdateBoneTransform;
  public
    function GetTexMap: TavTexture;

    function GetObj: TavModelInstance;

    function GetAutoUpdateAnimation: Boolean;
    function GetBoneTransform(const AName: string): TMat4;
    function GetTransform: TMat4;
    procedure SetAutoUpdateAnimation(AValue: Boolean);
    procedure SetBoneTransform(const AName: string; const AValue: TMat4);
    procedure SetTransform(const AValue: TMat4);

    procedure UpdateAnimationStates(const ATime: Int64);

    function AnimationCount: Integer;
    function AnimationName(const AIndex: Integer): string;
    procedure AnimationStartStop(const AAnimationName: string; DoStart: Boolean; GrowSpeed: Single = Default_GrowSpeed);
    procedure AnimationStart(const AAnimationName: string; GrowSpeed: Single);
    procedure AnimationStop (const AAnimationName: string; FadeSpeed: Single);
    procedure AnimationStartOnly(const AAnimationNames: array of string; GrowFadeSpeed: Single = Default_FadeSpeed);

    function Collection: TavModelCollection;
    function ModelName: String;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

{ TavModelCollection.TavBoneTransformMap.TTransformNode }

function TavModelCollection.TavBoneTransformMap.TTransformNode.Size: Integer;
begin
  Result := Length(Mat);
end;

{ TavModelCollection.TavBoneTransformMap }

function TavModelCollection.TavBoneTransformMap.TexWidth: Integer;
begin
  Result := 512;
end;

function TavModelCollection.TavBoneTransformMap.DoBuild: Boolean;
  procedure SetNodeData(const ANode: TTransformNode);
  var start, size, rowsize, stop: Integer;
      x, y: Integer;
  begin
    start := ANode.Range.Offset;
    size  := ANode.Range.Size;
    stop  := start + size;
    while start < stop do
    begin
      y := start div TexWidth;
      x := (start mod TexWidth);
      rowsize := min(stop - start, TexWidth - x);
      FTexH.SetMipImage(x*4, y, rowsize*4, 1, 0, 0, TImageFormat.R32G32B32A32F, @ANode.Mat[0]);
      Inc(start, rowsize);
    end;
  end;
var NewW, NewH: Integer;
    node: TTransformNode;
begin
  if FTexH = nil then
  begin
    FTexH := Main.Context.CreateTexture;
    FTexH.TargetFormat := TTextureFormat.RGBA32f;
  end;

  NewW := TexWidth * 4;
  NewH := (FNodes.RangeManSize + TexWidth - 1) div TexWidth;
  if (FTexH.Width <> NewW) or (FTexH.Height <> NewH) then
  begin
    FTexH.AllocMem(NewW, NewH, 1, False);
    FNodes.InvalidateAll;
  end;

  FNodes.Reset;
  while FNodes.NextDirty(node) do
    SetNodeData(node);
  FNodes.ValidateAll;

  Result := True;
end;

function TavModelCollection.TavBoneTransformMap.AddMatrices(const m: TMat4Arr): TBoneTransformHandle;
var node: TTransformNode;
begin
  Result := nil;
  node := TTransformNode.Create;
  node.Mat := m;
  FNodes.Add(node);
  if FNodes.DirtyCount > 0 then
    Invalidate;
  Result := Pointer(node);
end;

procedure TavModelCollection.TavBoneTransformMap.DeleteMatrices(var AHandle: TBoneTransformHandle);
var node: TTransformNode absolute AHandle;
begin
  if AHandle = nil then Exit;

  if TObject(AHandle) is TTransformNode then
    if FNodes.Del(node) then
    begin
      node.Free;
      node := nil;
      AHandle := nil;
    end;
end;

procedure TavModelCollection.TavBoneTransformMap.UpdateMatrices(const AHandle: TBoneTransformHandle; const m: TMat4Arr);
var node: TTransformNode absolute AHandle;
begin
  if AHandle = nil then Exit;
  node.Mat := m;
  FNodes.Invalidate(node);
  Invalidate;
end;

function TavModelCollection.TavBoneTransformMap.GetOffset(const AHandle: TBoneTransformHandle): Integer;
var node: TTransformNode absolute AHandle;
begin
  if AHandle = nil then Exit(0);
  if node.Range = nil then Exit(0);
  Result := node.Range.Offset;
end;

procedure TavModelCollection.TavBoneTransformMap.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TNodes.Create;
end;

destructor TavModelCollection.TavBoneTransformMap.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FNodes);
end;

{ TavModelCollection.TavMaterialMap }

function TavModelCollection.TavMaterialMap.DoBuild: Boolean;
begin
  if FTexH = nil then FTexH := Main.Context.CreateTexture;
  FTexH.TargetFormat := TTextureFormat.RGBA32f;
  FTexH.AllocMem(3, Length(FMaterials), 1, False);
  FTexH.SetMipImage(0, 0, 3, Length(FMaterials), 0, 0, TImageFormat.R32G32B32A32F, @FMaterials[0]);
  Result := True;
end;

procedure TavModelCollection.TavMaterialMap.AddMaterial(const AMaterial: TMeshMaterial);
var n : Integer;
begin
  n := Length(FMaterials);
  SetLength(FMaterials, Length(FMaterials)+1);
  FMaterials[n].Material := AMaterial;
  Invalidate;
end;

function TavModelCollection.TavMaterialMap.GetMaterialCount: Integer;
begin
  Result := Length(FMaterials);
end;

{ TModelInstanceGPUData_Base }

function TModelInstanceGPUData_Base.GetBasicPtr: Pointer;
begin
  Result := Pointer(Self);
  Inc(PByte(Result), ClassParent.InstanceSize);
end;

function TModelInstanceGPUData_Base.GetOffset(const FieldPtr: Pointer): Integer;
begin
  {$Hints Off}
  {$Warnings Off}
  Result := NativeInt(FieldPtr) - NativeInt(GetBasicPtr);
  {$Warnings On}
  {$Hints On}
end;

function TModelInstanceGPUData_Base.VerticesCount: Integer;
begin
  Result := 1;
end;

function TModelInstanceGPUData_Base.Data: TPointerData;
begin
  Result.data := GetBasicPtr;
  Result.size := Layout.Size;
end;

function TModelInstanceGPUData.AddLayoutFields(const LBuilder: ILayoutBuilder): ILayoutBuilder;
begin
  Result := LBuilder.Add('aiBoneMatDifNormOffset', ctFloat, 4, False, GetOffset(@aiBoneMatDifNormOffset));
end;

function TModelInstanceGPUData.Layout: IDataLayout;
begin
  if not Assigned(Layout_ModelInstanceGPUData) then
    Layout_ModelInstanceGPUData := AddLayoutFields(LB).Finish();
  Result := Layout_ModelInstanceGPUData;
end;

{ TavModelInstance.TAnimationPlayState }

procedure TavModelInstance.TAnimationPlayState.Calc(const ATime: Int64; out AFrame: Single; out AWeight: Single; out AComplete: Boolean);
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

{ TavModelInstance }

procedure TavModelInstance.OnUnlink;
begin

end;

procedure TavModelInstance.OnLink;
begin

end;

procedure TavModelInstance.UpdateAnimationStates(const ATime: Int64);
var i, n: Integer;
    currTime: Int64;
    Complete: Boolean;
begin
  if Length(FAnimationStates) = 0 then Exit;
  FBoneTransformDirty := True;

  //update
  currTime := ATime;
  for i := 0 to Length(FAnimationStates) - 1 do
  begin
    FAnimationPlayState[i].Calc(currTime, FAnimationStates[i].Frame, FAnimationStates[i].Weight, Complete);
    if Complete then
      FAnimationPlayState[i].StartTime := -1;
  end;

  //cleanup completed
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

function TavModelInstance.AnimationCount: Integer;
begin
  Result := 0;
  if Assigned(FModel) and Assigned(FModel.Mesh) and Assigned(FModel.Mesh.Armature) then
    Result := FModel.Mesh.Armature.AnimCount;
end;

function TavModelInstance.AnimationName(const AIndex: Integer): string;
begin
  Result := FModel.Mesh.Armature.Anim[AIndex].Name;
end;

procedure TavModelInstance.AnimationStartStop(const AAnimationName: string;
  DoStart: Boolean; GrowSpeed: Single);
begin
  if DoStart then
    AnimationStart(AAnimationName, GrowSpeed)
  else
    AnimationStop(AAnimationName, GrowSpeed);
end;

procedure TavModelInstance.UpdateBoneTransform;
begin
  if FAutoUpdateAnimation then
    UpdateAnimationStates(Collection.Main.BindTime64);

  if FBoneTransformDirty then
  begin
    FBoneTransformDirty := False;
//    if Length(FAnimationStates) > 0 then
    FModel.Mesh.GetPoseData(FBoneTransform, FTransform, FAnimationStates);
    Collection.FBoneTransform.UpdateMatrices(FBoneTransformIndex, FBoneTransform);
  end;
end;

function TavModelInstance.GetTexMap: TavTexture;
begin
  Result := nil;
  if FModel = nil then Exit;
  Result := FModel.FMapTex;
end;

function TavModelInstance.GetObj: TavModelInstance;
begin
  Result := Self;
end;

function TavModelInstance.GetAutoUpdateAnimation: Boolean;
begin
  Result := FAutoUpdateAnimation;
end;

function TavModelInstance.GetBoneTransform(const AName: string): TMat4;
var n: Integer;
begin
  if FModel = nil then Exit(IdentityMat4);
  if FModel.Mesh = nil then Exit(IdentityMat4);
  if FModel.Mesh.Armature = nil then Exit(IdentityMat4);
  n := FModel.Mesh.Armature.IndexOfBone(AName);
  if n < 0 then Exit(IdentityMat4);
  Result := FBoneTransform[n];
end;

function TavModelInstance.GetTransform: TMat4;
begin
  Result := FTransform;
end;

procedure TavModelInstance.SetAutoUpdateAnimation(AValue: Boolean);
begin
  FAutoUpdateAnimation := AValue;
end;

procedure TavModelInstance.SetBoneTransform(const AName: string; const AValue: TMat4);
var n: Integer;
begin
  if FModel = nil then Exit;
  if FModel.Mesh = nil then Exit;
  if FModel.Mesh.Armature = nil then Exit;
  n := FModel.Mesh.Armature.IndexOfBone(AName);
  if n < 0 then Exit;
  FBoneTransform[n] := AValue;
  FBoneTransformDirty := True;
end;

procedure TavModelInstance.SetTransform(const AValue: TMat4);
begin
  if FTransform = AValue then Exit;
  FTransform := AValue;
  FBoneTransformDirty := True;
end;

procedure TavModelInstance.AnimationStart(const AAnimationName: string; GrowSpeed: Single);
var anim: IavAnimation;
    animIndex: Integer;
    n: Integer;
    i: Integer;
begin
  if FModel = nil then Exit;
  if FModel.Mesh.Armature = nil then Exit;
  anim := FModel.Mesh.Armature.FindAnim(AAnimationName);
  animIndex := anim.Index;

  for i := 0 to Length(FAnimationStates) - 1 do
    if FAnimationStates[i].Index = animIndex then Exit;

  n := Length(FAnimationStates);

  SetLength(FAnimationStates, n+1);
  FAnimationStates[n].Frame := 0;
  FAnimationStates[n].Index := animIndex;
  FAnimationStates[n].Weight := 0;

  SetLength(FAnimationPlayState, n+1);
  FAnimationPlayState[n].StartTime := Collection.Main.Time64;
  FAnimationPlayState[n].GrowSpeed := GrowSpeed;
  FAnimationPlayState[n].StopTime := -1;
  FAnimationPlayState[n].FadeSpeed := Default_FadeSpeed;
end;

procedure TavModelInstance.AnimationStop(const AAnimationName: string; FadeSpeed: Single);
var anim: IavAnimation;
    animIndex: Integer;
    i: Integer;
begin
  if FModel = nil then Exit;
  if FModel.Mesh.Armature = nil then Exit;
  anim := FModel.Mesh.Armature.FindAnim(AAnimationName);
  Assert(Assigned(anim), 'Animation with name ' + AAnimationName + ' not found');
  animIndex := anim.Index;

  for i := 0 to Length(FAnimationStates) - 1 do
    if FAnimationStates[i].Index = animIndex then
    begin
      if FAnimationPlayState[i].StopTime >= 0 then Exit;
      FAnimationPlayState[i].StopTime := Collection.Main.Time64 + Ceil(FadeSpeed);
      FAnimationPlayState[i].FadeSpeed := FadeSpeed;
      Break;
    end;
end;

procedure TavModelInstance.AnimationStartOnly(const AAnimationNames: array of string; GrowFadeSpeed: Single);
  function InCurrentAnimations(const AAnimName: string): Boolean;
  var i: Integer;
  begin
    Result := False;
    for i := 0 to Length(AAnimationNames) - 1 do
      if AAnimationNames[i] = AAnimName then Exit(True);
  end;
var i: Integer;
    s: String;
begin
  for i := 0 to Length(FAnimationStates) - 1 do
  begin
    s := AnimationName(FAnimationStates[i].Index);
    if not InCurrentAnimations(s) then
      AnimationStop(s, GrowFadeSpeed);
  end;

  for i := 0 to Length(AAnimationNames) - 1 do
    AnimationStart(AAnimationNames[i], GrowFadeSpeed);
end;

function TavModelInstance.Collection: TavModelCollection;
begin
  if FModel = nil then Exit(nil);
  Result := FModel.Owner;
end;

function TavModelInstance.ModelName: String;
begin
  if (FModel = nil) or (FModel.Mesh = nil) then
    Result := ''
  else
    Result := FModel.Mesh.Name;
end;

procedure TavModelInstance.AfterConstruction;
begin
  inherited AfterConstruction;
  FTransform := IdentityMat4;
  FAutoUpdateAnimation := True;
end;

destructor TavModelInstance.Destroy;
begin
  if Assigned(FModel) then
    FModel.UnlinkInstance(Self);
  inherited Destroy;
end;

{ TModel }

procedure TavModelCollection.TModel.UnlinkInstance(const Obj: TObject);
var inst: TavModelInstance absolute Obj;
    n: Integer;
begin
  if Obj = nil then Exit;
  if inst.FInstanceIndex < 0 then Exit;

  n := FInstances.Count-1;
  if inst.FInstanceIndex <> n then
  begin
    FInstances[inst.FInstanceIndex] := FInstances[n];
    TavModelInstance(FInstances[inst.FInstanceIndex]).FInstanceIndex := inst.FInstanceIndex;
  end;
  Owner.FBoneTransform.DeleteMatrices(inst.FBoneTransformIndex);
  Owner.FInstVB.Del(inst.FInstGPUData);
  inst.OnUnlink;
  inst.FInstanceIndex := -1;
  inst.FModel := nil;
  inst.FInstGPUData := nil;
  FInstances.Delete(n);
end;

function TavModelCollection.TModel.CreateInstance(const ModelTransform: TMat4): IavModelInstance;
var inst: TavModelInstance;
    instGPU: TModelInstanceGPUData;
begin
  inst := TavModelInstance.Create;
  inst.FTransform := ModelTransform;

  Mesh.GetPoseData(inst.FBoneTransform, ModelTransform, []);
  inst.FBoneTransformIndex := Owner.FBoneTransform.AddMatrices(inst.FBoneTransform);

  instGPU := TModelInstanceGPUData.Create;

  instGPU.aiBoneMatDifNormOffset := Vec(Owner.FBoneTransform.GetOffset(inst.FBoneTransformIndex),
                                        MaterialOffset,
                                        DiffuseOffset,
                                        NormalsOffset);
  inst.FModel := Self;
  inst.FInstGPUData := Owner.FInstVB.Add(instGPU as IVerticesData);
  inst.FInstanceIndex := FInstances.Count;
  FInstances.Add(inst);
  inst.OnLink;
  Result := inst;
end;

constructor TavModelCollection.TModel.Create;
begin
  FInstances := TList.Create;
  MaterialOffset := -1000000;
  DiffuseOffset  := -1000000;
  NormalsOffset  := -1000000;
end;

destructor TavModelCollection.TModel.Destroy;
var
  i: Integer;
begin
  for i := FInstances.Count - 1 downto 0 do
    UnlinkInstance(TavModelInstance(FInstances[i]));
  FreeAndNil(FInstances);
  inherited Destroy;
end;

{ TavModelCollection }

procedure TavModelCollection.AddModel(const AModel: TModel);
begin
  FModels.AddOrSet(AModel.Mesh.Name, AModel);
end;

function TavModelCollection.ObtainDummyTextureData(const Key: TTextureKey): ITextureData;
begin
  if not FDummyTexData.TryGetValue(Key, Result) then
  begin
    Result := EmptyTexData(Key.Width, Key.Height, TImageFormat.A8R8G8B8, Key.Mips > 1, True);
    FDummyTexData.Add(Key, Result);
  end;
end;

function TavModelCollection.ObtainMap(const Key: TTextureKey): TavTexture;
begin
  if not FMaps.TryGetValue(Key, Result) then
  begin
    Result := TavTexture.Create(Self);
    Result.TargetFormat := TTextureFormat.RGBA;
    Result.ForcedArray := True;
    Result.AutoGenerateMips := True;
    FMaps.Add(Key, Result);
  end;
end;

function SortByTexture(item1, item2: Pointer; dataSize: Integer; userData: Pointer): Integer;
var m1: ^TavModelInstance absolute item1;
    m2: ^TavModelInstance absolute item2;
begin
  Assert(m1^ <> nil);
  Assert(m2^ <> nil);
  Result := NativeInt(m1^.GetTexMap) - NativeInt(m2^.GetTexMap);
end;

procedure TavModelCollection.Draw(const Intances: IObjArr; SortByMaterial: Boolean);
var mInst: TavModelInstance;
    lastDiffuse: TavTexture;
    prog: TavProgram;
    i: Integer;
begin
  prog := Main.ActiveProgram;
  if prog = nil then Exit;

  if SortByMaterial then
    Intances.Sort(@SortByTexture, nil);

  lastDiffuse := nil;
  for i := 0 to Intances.Count - 1 do
  begin
    mInst := TavModelInstance(Intances[i]);
    mInst.UpdateBoneTransform;
    if lastDiffuse <> mInst.GetTexMap then
    begin
      lastDiffuse := mInst.GetTexMap;
      prog.SetUniform('Maps', lastDiffuse, Sampler_Linear);
    end;
    DrawManaged(prog, mInst.FModel.VBHandle, mInst.FModel.IBHandle, mInst.FInstGPUData, ptTriangles, cmBack);
  end;
end;

function TavModelCollection.ModelsCount: Integer;
begin
  Result := FModels.Count;
end;

procedure TavModelCollection.Reset;
begin
  FModels.Reset;
end;

function TavModelCollection.Next(out ModelName: string): Boolean;
var Dummy: TModel;
begin
  Result := FModels.Next(ModelName, Dummy);
end;

function TavModelCollection.CreateInstance(const ModelName: String; const ModelTransform: TMat4): IavModelInstance;
var m: TModel;
begin
  if FModels.TryGetValue(ModelName, m) then
    Result := m.CreateInstance(ModelTransform)
  else
    Result := nil;
end;

procedure TavModelCollection.Select;
begin
  Assert(Main.ActiveProgram <> Nil);
  if not FVB.HasData then Exit;
  if not FIB.HasData then Exit;
  if not FInstVB.HasData then Exit;
  Main.ActiveProgram.SetAttributes(FVB, FIB, FInstVB);
  Main.ActiveProgram.SetUniform('Materials', FMaterials, Sampler_NoFilter);
  Main.ActiveProgram.SetUniform('BoneTransform', FBoneTransform, Sampler_NoFilter);
end;

procedure TavModelCollection.Draw(const Instances: array of IavModelInstance; SortByMaterial: Boolean);
var inst: IObjArr;
    i: Integer;
    obj: TavModelInstance;
begin
  if Length(Instances) = 0 then Exit;

  inst := TObjArr.Create;
  inst.Capacity := Length(Instances);
  for i := 0 to Length(Instances) - 1 do
  begin
    obj := IavModelInstanceInternal(Instances[i]).GetObj;
    Assert(obj.Collection = Self);
    inst.Add(obj);
  end;

  Draw(inst, SortByMaterial);
end;

procedure TavModelCollection.Draw(const Instances: array of IModelInstanceArr; SortByMaterial: Boolean);
var inst: IObjArr;
    i, j, cap: Integer;
    obj: TavModelInstance;
begin
  if Length(Instances) = 0 then Exit;

  cap := 0;
  for i := 0 to Length(Instances) - 1 do
    Inc(cap, Instances[i].Count);
  if cap = 0 then Exit;

  inst := TObjArr.Create;
  inst.Capacity := cap;

  for i := 0 to Length(Instances) - 1 do
  begin
    for j := 0 to Instances[i].Count - 1 do
    begin
      obj := IavModelInstanceInternal(Instances[i][j]).GetObj;
      Assert(obj.Collection = Self);
      inst.Add(obj);
    end;
  end;

  Draw(inst, SortByMaterial);
end;

procedure TavModelCollection.AddFromFile(const FileName: String; const TexManager: ITextureManager);
var meshes: TavMeshes;
begin
  LoadFromFile(FileName, meshes, TexManager);
  AddFromMeshes(meshes);
end;

procedure TavModelCollection.AddFromMeshes(const AMeshes: TavMeshes);
  procedure AddTexData(const TexKey: TTextureKey; TexData: ITextureData);
  var Tex: TavTexture;
  begin
    if TexData = nil then TexData := ObtainDummyTextureData(TexKey);
    Tex := ObtainMap(TexKey);
    if Tex.TexData = nil then
      Tex.TexData := EmptyTexData;
    Tex.TexData.Merge([TexData]);
    Tex.Invalidate;
  end;
  function GetMapOffset(const TexKey: TTextureKey): Single;
  var Tex: TavTexture;
  begin
    Result := 0;
    Tex := ObtainMap(TexKey);
    if Assigned(Tex.TexData) then
      Result := Tex.TexData.ItemCount;
  end;
var
  material: TMeshMaterial;
  model: TModel;
  i, j: Integer;
  tKey: TTextureKey;
  ContainDiffuseMap, ContainNormalMap: Boolean;
begin
  for i := 0 to Length(AMeshes) - 1 do
  begin
    model := TModel.Create;
    model.Owner := Self;
    model.Mesh := AMeshes[i];
    model.VBHandle := FVB.Add(model.Mesh.Vert as IVerticesData);
    model.IBHandle := FIB.Add(model.Mesh.Ind as IIndicesData);

    //prepare map size
    tKey.Width := -1;
    tKey.Height := -1;
    tKey.Mips := -1;
    ContainDiffuseMap := False;
    ContainNormalMap := False;
    for j := 0 to model.Mesh.MaterialsCount-1 do
    begin
      if Assigned(model.Mesh.MaterialMaps[j].matDiffMap) then
      begin
        tKey.Width  := model.Mesh.MaterialMaps[j].matDiffMap.Width;
        tKey.Height := model.Mesh.MaterialMaps[j].matDiffMap.Height;
        tKey.Mips   := model.Mesh.MaterialMaps[j].matDiffMap.MipsCount;
        ContainDiffuseMap := True;
      end;
      if Assigned(model.Mesh.MaterialMaps[j].matNormalMap) then
      begin
        tKey.Width  := model.Mesh.MaterialMaps[j].matNormalMap.Width;
        tKey.Height := model.Mesh.MaterialMaps[j].matNormalMap.Height;
        tKey.Mips   := model.Mesh.MaterialMaps[j].matNormalMap.MipsCount;
        ContainNormalMap := True;
      end;
    end;
    if tKey.Width < 0 then
    begin
      tKey.Width := 1;
      tKey.Height := 1;
      tKey.Mips := 1;
    end;

    if ContainDiffuseMap then
    begin
      model.DiffuseOffset := GetMapOffset(tKey);
      for j := 0 to model.Mesh.MaterialsCount-1 do
        AddTexData(tKey, model.Mesh.MaterialMaps[j].matDiffMap);
    end;
    if ContainNormalMap then
    begin
      model.NormalsOffset := GetMapOffset(tKey);
      for j := 0 to model.Mesh.MaterialsCount-1 do
        AddTexData(tKey, model.Mesh.MaterialMaps[j].matNormalMap);
    end;

    model.MaterialOffset := FMaterials.GetMaterialCount;
    for j := 0 to model.Mesh.MaterialsCount - 1 do
    begin
      material := model.Mesh.Material[j];
      if model.Mesh.MaterialMaps[j].matDiffMap = nil then material.matDiffMapFactor := 0;
      FMaterials.AddMaterial(material);
    end;

    model.FMapTex := ObtainMap(tKey);

    AddModel(model);
  end;
end;

constructor TavModelCollection.Create(AParent: TavObject);
begin
  inherited;
  FModels := TModelHash.Create('', nil);

  FVB := TavVBManaged.Create(Self);
  FVB.CullMode := cmBack;
  FIB := TavIBManaged.Create(Self);
  FInstVB := TavVBManaged.Create(Self);
  FMaterials := TavMaterialMap.Create(Self);
  FBoneTransform := TavBoneTransformMap.Create(Self);
  FMaps := TTextureHash.Create;

  FDummyTexData := TDummyTexDataHash.Create(EmptyTexureKey, nil);
end;

destructor TavModelCollection.Destroy;
var m: TModel;
    mName: String;
begin
  FModels.Reset;
  while FModels.Next(mName, m) do
  begin
    FVB.Del(m.VBHandle);
    FIB.Del(m.IBHandle);
    m.Mesh := nil;
    m.Free;
  end;
  FModels.Clear;
  inherited Destroy;
end;

end.

