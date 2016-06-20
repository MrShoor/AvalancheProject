unit avModel;
{$I avConfig.inc}

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
    dummy: Boolean;
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
    function Name: String;

    property Transform: TMat4 read GetTransform write SetTransform;
    property BoneTransform[const AName: string]: TMat4 read GetBoneTransform write SetBoneTransform;
    function Clone: IavModelInstance;

    property AutoUpdateAnimation: Boolean read GetAutoUpdateAnimation write SetAutoUpdateAnimation;
  end;

  IModelInstanceArr = {$IfDef FPC}specialize{$EndIf} IArray<IavModelInstance>;
  TModelInstanceArr = {$IfDef FPC}specialize{$EndIf} TArray<IavModelInstance>;

  { IBoneTransformHandle }

  IBoneTransformHandle = interface (IUnknown)
    procedure Invalidate;
    function Offset: Integer;
    function GetMatrices: TMat4Arr;
    procedure SetMatrices(const AValue: TMat4Arr);
    property Matrices: TMat4Arr read GetMatrices write SetMatrices;
  end;

  { TavModelCollection }

  TavModelCollection = class (TavMainRenderChild)
  private type
    { TModel }

    TModel = class
    private
      Owner: TavModelCollection;
      VBHandle: IVBManagedHandle;
      IBHandle: IIBManagedHandle;
      FMapTex : TavTexture;
      FInstances: TList;

      MaterialOffset: Single;
      DiffuseOffset : Single;
      NormalsOffset : Single;

      procedure UnlinkInstance(const Obj: TObject);
    public
      Mesh : IavMesh;
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
      {$IfDef FPC}
      TTransformNode = class (TInterfacedObject, IBoneTransformHandle)
        Owner     : TavBoneTransformMap;
        Range     : IMemRange;
        Mat       : TMat4Arr;
        DirtyIndex: Integer;

        procedure Invalidate;
        function Offset: Integer;
        function GetMatrices: TMat4Arr;
        procedure SetMatrices(const AValue: TMat4Arr);
        property Matrices: TMat4Arr read GetMatrices write SetMatrices;

        function Size: Integer; Inline;
        destructor Destroy; override;
      end;
      {$EndIf}
      {$IfDef DCC}
      TTransformNode = class (TDefaultNode, IBoneTransformHandle)
        Owner     : TavBoneTransformMap;
        Mat       : TMat4Arr;

        procedure Invalidate;
        function Offset: Integer;
        function GetMatrices: TMat4Arr;
        procedure SetMatrices(const AValue: TMat4Arr);
        property Matrices: TMat4Arr read GetMatrices write SetMatrices;

        function Size: Integer; override;
        destructor Destroy; override;
      end;
      {$EndIf}
      TNodes = {$IfDef FPC}specialize{$EndIf} TNodeManager<TTransformNode>;
    private
      FNodes : TNodes;
    protected
      function TexWidth: Integer;

      function DoBuild: Boolean; override;
    public
      function  AddMatrices(const m: TMat4Arr): IBoneTransformHandle;
      procedure UpdateMatrices(const AHandle: IBoneTransformHandle; const m: TMat4Arr);

      procedure AfterConstruction; override;
      destructor Destroy; override;
    end;

    { IavModelInstanceInternal }

    TavModelInstance = class;

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
      FModel: TModel;
      FInstanceIndex: Integer;
      FInstGPUData: IVBManagedHandle;

      FBoneTransformHandle: IBoneTransformHandle;
      FBoneTransformDirty: Boolean;

      FMeshInst: IavMeshInstance;

      FName: string;
      FAutoUpdateAnimation: Boolean;

      FAnimationStates   : array of TMeshAnimationState;
      FAnimationPlayState: array of TAnimationPlayState;

      procedure OnUnlink;
      procedure OnLink;

      function GetPoseArr: TMat4Arr;
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
      function Name: String;

      function Clone: IavModelInstance;

      procedure AfterConstruction; override;
      destructor Destroy; override;
    end;

    IModelHash = {$IfDef FPC}specialize{$EndIf} IHashMap<String, TModel>;
    TModelHash = {$IfDef FPC}specialize{$EndIf} THashMap<String, TModel>;

    IModelInstHash = {$IfDef FPC}specialize{$EndIf} IHashMap<String, IavModelInstance>;
    TModelInstHash = {$IfDef FPC}specialize{$EndIf} THashMap<String, IavModelInstance>;

    TTextureKey = packed record
      Width : Integer;
      Height: Integer;
      Mips  : Integer;
    end;
  private const
    EmptyTexureKey: TTextureKey = (Width:0;Height:0;Mips:0);
  private type
    ITextureHash = {$IfDef FPC}specialize{$EndIf} IHashMap<TTextureKey, TavTexture>;
    TTextureHash = {$IfDef FPC}specialize{$EndIf} THashMap<TTextureKey, TavTexture>;

    IDummyTexDataHash = {$IfDef FPC}specialize{$EndIf} IHashMap<TTextureKey, ITextureData>;
    TDummyTexDataHash = {$IfDef FPC}specialize{$EndIf} THashMap<TTextureKey, ITextureData>;
  private
    FVB: TavVBManaged;
    FIB: TavIBManaged;
    FInstVB: TavVBManaged;
    FMaterials: TavMaterialMap;
    FMaps: ITextureHash;
    FBoneTransform: TavBoneTransformMap;

    FDummyTexData: IDummyTexDataHash; //empty tex data for not completed materials

    FModels: IModelHash;
    FModelInstances: IModelInstHash;

    function ObtainDummyTextureData(const Key: TTextureKey): ITextureData;
    function ObtainMap(const Key: TTextureKey): TavTexture;

    procedure Draw(const Intances: IObjArr; SortByMaterial: Boolean); overload;
  public
    function ModelsCount: Integer;
    procedure Reset;
    function NextModel(out ModelName: string): Boolean;
    function NextInstance(out Instance: IavModelInstance): Boolean;
    function FindInstance(const InstanceName: string): IavModelInstance;
    procedure DeleteInstance(const InstanceName: string);

    procedure Select;
    procedure Draw(const Instances: array of IavModelInstance; SortByMaterial: Boolean = True); overload;
    procedure Draw(const Instances: array of IModelInstanceArr; SortByMaterial: Boolean = True); overload;

    procedure AddFromFile(const FileName: String; const TexManager: ITextureManager = nil);
    function  AddFromMeshInstance(const AMeshInstance: IavMeshInstance): IavModelInstance;
    procedure AddFromMeshInstances(const AMeshInstances: IavMeshInstances);

    constructor Create(AParent: TavObject); override;
    destructor Destroy; override;
  end;

implementation

uses
  Math;

{ TavModelCollection.TavBoneTransformMap.TTransformNode }

procedure TavModelCollection.TavBoneTransformMap.TTransformNode.Invalidate;
begin
  Owner.FNodes.Invalidate(Self);
  Owner.Invalidate;
end;

function TavModelCollection.TavBoneTransformMap.TTransformNode.Offset: Integer;
begin
  Result := Range.Offset;
end;

function TavModelCollection.TavBoneTransformMap.TTransformNode.GetMatrices: TMat4Arr;
begin
  Result := Mat;
end;

procedure TavModelCollection.TavBoneTransformMap.TTransformNode.SetMatrices(const AValue: TMat4Arr);
begin
  Assert(Length(Mat) = Length(AValue));
  Mat := AValue;
  Owner.FNodes.Invalidate(Self);
end;

function TavModelCollection.TavBoneTransformMap.TTransformNode.Size: Integer;
begin
  Result := Length(Mat);
end;

destructor TavModelCollection.TavBoneTransformMap.TTransformNode.Destroy;
begin
  inherited Destroy;
  Owner.FNodes.Del(Self);
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

function TavModelCollection.TavBoneTransformMap.AddMatrices(const m: TMat4Arr): IBoneTransformHandle;
var node: TTransformNode;
begin
  Result := nil;
  node := TTransformNode.Create;
  node.Owner := Self;
  node.Mat := m;
  FNodes.Add(node);
  if FNodes.DirtyCount > 0 then
    Invalidate;
  Result := node;
end;

procedure TavModelCollection.TavBoneTransformMap.UpdateMatrices(const AHandle: IBoneTransformHandle; const m: TMat4Arr);
var node: TTransformNode absolute AHandle;
begin
  if AHandle = nil then Exit;
  node.Mat := m;
  FNodes.Invalidate(node);
  Invalidate;
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
  Result := @dummy;
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
var offset: Integer;
begin
  offset := GetOffset(@aiBoneMatDifNormOffset);
  Result := LBuilder.Add('aiBoneMatDifNormOffset', ctFloat, 4, False, offset);
end;

function TModelInstanceGPUData.Layout: IDataLayout;
begin
  if not Assigned(Layout_ModelInstanceGPUData) then
    Layout_ModelInstanceGPUData := AddLayoutFields(LB).Finish();
  Result := Layout_ModelInstanceGPUData;
end;

{ TavModelCollection.TavModelInstance.TAnimationPlayState }

procedure TavModelCollection.TavModelInstance.TAnimationPlayState.Calc(const ATime: Int64; out AFrame: Single; out AWeight: Single; out AComplete: Boolean);
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

{ TavModelCollection.TavModelInstance }

procedure TavModelCollection.TavModelInstance.OnUnlink;
begin

end;

procedure TavModelCollection.TavModelInstance.OnLink;
begin

end;

function TavModelCollection.TavModelInstance.GetPoseArr: TMat4Arr;
begin
  Result := FBoneTransformHandle.Matrices;
end;

procedure TavModelCollection.TavModelInstance.UpdateAnimationStates(const ATime: Int64);
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

function TavModelCollection.TavModelInstance.AnimationCount: Integer;
begin
  Result := 0;
  if Assigned(FMeshInst) and Assigned(FMeshInst.Armature) then
    Result := FMeshInst.Armature.AnimCount;
end;

function TavModelCollection.TavModelInstance.AnimationName(const AIndex: Integer): string;
begin
  Result := FMeshInst.Armature.Anim[AIndex].Name;
end;

procedure TavModelCollection.TavModelInstance.AnimationStartStop(const AAnimationName: string;
  DoStart: Boolean; GrowSpeed: Single);
begin
  if DoStart then
    AnimationStart(AAnimationName, GrowSpeed)
  else
    AnimationStop(AAnimationName, GrowSpeed);
end;

procedure TavModelCollection.TavModelInstance.UpdateBoneTransform;
begin
  if FAutoUpdateAnimation then
    UpdateAnimationStates(Collection.Main.BindTime64);

  if FBoneTransformDirty then
  begin
    FBoneTransformDirty := False;
    FMeshInst.SetAnimationPose(FAnimationStates);
    FBoneTransformHandle.Matrices := FMeshInst.AbsPose;
//    m := FBoneTransformHandle.Matrices;
//    FMeshInst.GetPoseData(m, FAnimationStates);
    FBoneTransformHandle.Invalidate;
  end;
end;

function TavModelCollection.TavModelInstance.GetTexMap: TavTexture;
begin
  Result := nil;
  if FModel = nil then Exit;
  Result := FModel.FMapTex;
end;

function TavModelCollection.TavModelInstance.GetObj: TavModelInstance;
begin
  Result := Self;
end;

function TavModelCollection.TavModelInstance.GetAutoUpdateAnimation: Boolean;
begin
  Result := FAutoUpdateAnimation;
end;

function TavModelCollection.TavModelInstance.GetBoneTransform(const AName: string): TMat4;
begin
  if FModel = nil then Exit(IdentityMat4);
  if FModel.Mesh = nil then Exit(IdentityMat4);
end;

function TavModelCollection.TavModelInstance.GetTransform: TMat4;
begin
  Result := FMeshInst.Transform;
end;

procedure TavModelCollection.TavModelInstance.SetAutoUpdateAnimation(AValue: Boolean);
begin
  FAutoUpdateAnimation := AValue;
end;

procedure TavModelCollection.TavModelInstance.SetBoneTransform(const AName: string; const AValue: TMat4);
begin
  if FModel = nil then Exit;
  if FModel.Mesh = nil then Exit;
  FMeshInst.IndexOfBone(AName);
end;

procedure TavModelCollection.TavModelInstance.SetTransform(const AValue: TMat4);
begin
  if FMeshInst.Transform = AValue then Exit;
  FMeshInst.Transform := AValue;
  FBoneTransformDirty := True;
end;

procedure TavModelCollection.TavModelInstance.AnimationStart(const AAnimationName: string; GrowSpeed: Single);
var anim: IavAnimation;
    animIndex: Integer;
    n: Integer;
    i: Integer;
begin
  if FModel = nil then Exit;
  if FMeshInst.Armature = nil then Exit;
  anim := FMeshInst.Armature.FindAnim(AAnimationName);
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
  FAnimationPlayState[n].StartTime := Collection.Main.Time64;
  FAnimationPlayState[n].GrowSpeed := GrowSpeed;
  FAnimationPlayState[n].StopTime := -1;
  FAnimationPlayState[n].FadeSpeed := Default_FadeSpeed;
end;

procedure TavModelCollection.TavModelInstance.AnimationStop(const AAnimationName: string; FadeSpeed: Single);
var anim: IavAnimation;
    animIndex: Integer;
    i: Integer;
begin
  if FModel = nil then Exit;
  if FMeshInst.Armature = nil then Exit;
  anim := FMeshInst.Armature.FindAnim(AAnimationName);
  Assert(Assigned(anim), 'Animation with name ' + AAnimationName + ' not found');
  animIndex := anim.Index;

  for i := 0 to Length(FAnimationStates) - 1 do
    if FAnimationStates[i].Index = animIndex then
    begin
      if FAnimationPlayState[i].StopTime >= 0 then Exit;
      FAnimationPlayState[i].StopTime := Collection.Main.Time64 + Math.Ceil(FadeSpeed);
      FAnimationPlayState[i].FadeSpeed := FadeSpeed;
      Break;
    end;
end;

procedure TavModelCollection.TavModelInstance.AnimationStartOnly(const AAnimationNames: array of string; GrowFadeSpeed: Single);
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

function TavModelCollection.TavModelInstance.Collection: TavModelCollection;
begin
  if FModel = nil then Exit(nil);
  Result := FModel.Owner;
end;

function TavModelCollection.TavModelInstance.ModelName: String;
begin
  if (FModel = nil) or (FModel.Mesh = nil) then
    Result := ''
  else
    Result := FModel.Mesh.Name;
end;

function TavModelCollection.TavModelInstance.Name: String;
begin
  Result := FName;
end;

function TavModelCollection.TavModelInstance.Clone: IavModelInstance;
var NewInstName: string;
    i: Integer;
begin
  Result := nil;
  if FModel = nil then Exit;

  i := 0;
  repeat
    NewInstName := Name + 'Clone' + IntToStr(i);
    Inc(i);
  until not FModel.Owner.FModelInstances.Contains(NewInstName);
  Result := FModel.Owner.AddFromMeshInstance(FMeshInst.Clone(NewInstName));
end;

procedure TavModelCollection.TavModelInstance.AfterConstruction;
begin
  inherited AfterConstruction;
  FAutoUpdateAnimation := True;
end;

destructor TavModelCollection.TavModelInstance.Destroy;
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
  inst.FBoneTransformHandle := nil;
  inst.FInstGPUData := nil;
  inst.OnUnlink;
  inst.FInstanceIndex := -1;
  inst.FModel := nil;
  inst.FInstGPUData := nil;
  inst.FMeshInst := nil;
  FInstances.Delete(n);
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

procedure TavModelCollection.Draw(const Intances: IObjArr; SortByMaterial: Boolean);
var mInst: TavModelInstance;
    lastDiffuse: TavTexture;
    prog: TavProgram;
    i: Integer;
begin
  prog := Main.ActiveProgram;
  if prog = nil then Exit;

  if SortByMaterial then
    Intances.Sort;

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
  FModelInstances.Reset;
end;

function TavModelCollection.NextModel(out ModelName: string): Boolean;
var Dummy: TModel;
begin
  Result := FModels.Next(ModelName, Dummy);
end;

function TavModelCollection.NextInstance(out Instance: IavModelInstance): Boolean;
begin
  Result := FModelInstances.NextValue(Instance);
end;

function TavModelCollection.FindInstance(const InstanceName: string): IavModelInstance;
begin
  if not FModelInstances.TryGetValue(InstanceName, Result) then
    Result := nil;
end;

procedure TavModelCollection.DeleteInstance(const InstanceName: string);
begin
  FModelInstances.Delete(InstanceName);
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
var meshes: IavMeshes;
    inst  : IavMeshInstances;
begin
  LoadFromFile(FileName, meshes, inst, TexManager);
  AddFromMeshInstances(inst);
end;

function TavModelCollection.AddFromMeshInstance(const AMeshInstance: IavMeshInstance): IavModelInstance;
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
  j: Integer;
  tKey: TTextureKey;
  ContainDiffuseMap, ContainNormalMap: Boolean;

  modelInst: TavModelInstance;
  instGPU : TModelInstanceGPUData;
  m: TMat4Arr;
begin
  Assert(not FModelInstances.Contains(AMeshInstance.Name), 'Instance with name "'+AMeshInstance.Name+'" already in set');

  if not FModels.TryGetValue(AMeshInstance.Mesh.Name, model) then
  begin
    model := TModel.Create;
    model.Owner := Self;
    model.Mesh := AMeshInstance.Mesh;
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

    FModels.AddOrSet(model.Mesh.Name, model);
  end;

  modelInst := TavModelInstance.Create;
  modelInst.FMeshInst := AMeshInstance;
  modelInst.FMeshInst.SetAnimationPose([]);
  m := modelInst.FMeshInst.AbsPose;
  modelInst.FBoneTransformHandle := FBoneTransform.AddMatrices(m);
  instGPU := TModelInstanceGPUData.Create;
  instGPU.aiBoneMatDifNormOffset := Vec(modelInst.FBoneTransformHandle.Offset,
                                        model.MaterialOffset,
                                        model.DiffuseOffset,
                                        model.NormalsOffset);
  modelInst.FModel := model;
  modelInst.FInstGPUData := FInstVB.Add(instGPU as IVerticesData);
  modelInst.FInstanceIndex := model.FInstances.Count;
  modelInst.FName := AMeshInstance.Name;
  model.FInstances.Add(modelInst);
  modelInst.OnLink;
  FModelInstances.Add(modelInst.FName, modelInst);

  Result := modelInst;
end;

procedure TavModelCollection.AddFromMeshInstances(const AMeshInstances: IavMeshInstances);
var inst: IavMeshInstance;
begin
  AMeshInstances.Reset;
  while AMeshInstances.NextValue(inst) do
    AddFromMeshInstance(inst);
end;

constructor TavModelCollection.Create(AParent: TavObject);
begin
  inherited;
  FModels := TModelHash.Create;
  FModelInstances := TModelInstHash.Create;

  FVB := TavVBManaged.Create(Self);
  FVB.CullMode := cmBack;
  FIB := TavIBManaged.Create(Self);
  FInstVB := TavVBManaged.Create(Self);
  FMaterials := TavMaterialMap.Create(Self);
  FBoneTransform := TavBoneTransformMap.Create(Self);
  FMaps := TTextureHash.Create;

  FDummyTexData := TDummyTexDataHash.Create;
end;

destructor TavModelCollection.Destroy;
var m: TModel;
    mName: String;
begin
  FModelInstances.Clear;
  FModels.Reset;
  while FModels.Next(mName, m) do
    m.Free;
  FModels.Clear;
  inherited Destroy;
end;

end.

