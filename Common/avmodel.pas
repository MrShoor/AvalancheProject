unit avModel;
{$I avConfig.inc}

interface

uses
  Classes, SysUtils,
  mutils,
  avBase, avRes, avTess, avTypes, avContnrs, avMesh, avTexLoader;

type
  TavModelCollection = class;

  { TModelInstanceGPUData }

  TModelInstanceGPUData = record
    aiBoneMatOffset: TVec2;
    class function Layout: IDataLayout; static;
  end;
  TModelInstanceGPUDataArr = {$IfDef FPC}specialize{$EndIf}TVerticesRec<TModelInstanceGPUData>;
  IModelInstanceGPUDataArr = {$IfDef FPC}specialize{$EndIf}IArray<TModelInstanceGPUData>;

  { IavModelInstance }

  IavModelInstance = interface
    function GetStatic: Boolean;
    function GetTransform: TMat4;
    procedure SetStatic(const AValue: Boolean);
    procedure SetTransform(const AValue: TMat4);

    function Mesh: IavMeshInstance;
    function MultiMesh: IavMeshInstanceArray;
    function CustomProps: ICustomProps;
    function Collection: TavModelCollection;
    function ModelName: String;
    function Name: String;

    property Static   : Boolean read GetStatic    write SetStatic;
    property Transform: TMat4   read GetTransform write SetTransform;
  end;

  IavModelInstanceArr = {$IfDef FPC}specialize{$EndIf} IArray<IavModelInstance>;
  TavModelInstanceArr = {$IfDef FPC}specialize{$EndIf} TArray<IavModelInstance>;
  IavModelInstanceSet = {$IfDef FPC}specialize{$EndIf} IHashSet<IavModelInstance>;
  TavModelInstanceSet = {$IfDef FPC}specialize{$EndIf} THashSet<IavModelInstance>;

  { IBoneTransformHandle }

  IBoneTransformHandle = interface (IUnknown)
    procedure Invalidate;
    function Offset: Integer;
    function GetMatrices: TMat4Arr;
    property Matrices: TMat4Arr read GetMatrices;
  end;

  { TavModelCollection }

  TavModelCollection = class (TavMainRenderChild)
  private type
    IMaterialMapHandle = IManagedHandle;

    { TModel }

    TModel = class
    private
      Owner: TavModelCollection;
      VBHandle: IVBManagedHandle;
      IBHandle: IIBManagedHandle;
      FMapTex : TavMultiTexture;
      FInstances: TList;

      MaterialHandle: IMaterialMapHandle;

      FInDestroy: Boolean;
      procedure UnlinkInstance(const Obj: TObject);
    public
      Mesh : IavMesh;
      constructor Create;
      destructor Destroy; override;
    end;

    TModelMaterialGPU = record
      matDiff        : TVec4;
      matSpec        : TVec4;
      matSpecHardness: Single;
      matSpecIOR     : Single;
      matEmitFactor  : Single;
      Dummy          : Single;

      mapDiffuse_Intensity_Color              : TVec4; //pairs of index/factor
      mapDiffuse_Alpha_Translucency           : TVec4; //pairs of index/factor
      mapShading_Ambient_Emit                 : TVec4; //pairs of index/factor
      mapShading_Mirror_RayMirror             : TVec4; //pairs of index/factor
      mapSpecular_Intensity_Color             : TVec4; //pairs of index/factor
      mapSpecular_Hardness_mapGeometry_Normal : TVec4; //pairs of index/factor
      mapGeometry_Warp_Displace               : TVec4; //pairs of index/factor
    end;

    { TModelMaterial }

    TModelMaterial = record
      Material: TModelMaterialGPU;

      mapDiffuse_Intensity    : IMTManagedHandle;
      mapDiffuse_Color        : IMTManagedHandle;
      mapDiffuse_Alpha        : IMTManagedHandle;
      mapDiffuse_Translucency : IMTManagedHandle;
      mapShading_Ambient      : IMTManagedHandle;
      mapShading_Emit         : IMTManagedHandle;
      mapShading_Mirror       : IMTManagedHandle;
      mapShading_RayMirror    : IMTManagedHandle;
      mapSpecular_Intensity   : IMTManagedHandle;
      mapSpecular_Color       : IMTManagedHandle;
      mapSpecular_Hardness    : IMTManagedHandle;
      mapGeometry_Normal      : IMTManagedHandle;
      mapGeometry_Warp        : IMTManagedHandle;
      mapGeometry_Displace    : IMTManagedHandle;

      procedure UpdateMapIndices;
    end;

    IMaterialArr = {$IfDef FPC}specialize{$EndIf} IArray<TModelMaterial>;
    TMaterialArr = {$IfDef FPC}specialize{$EndIf} TArray<TModelMaterial>;

    { TavMaterialMap }
    TavMaterialMap = class(TavTexture)
    private type
      {$IfDef FPC}
      TMaterialNode = class (TInterfacedObject, IMaterialMapHandle)
        Owner     : TavMaterialMap;
        Range     : IMemRange;
        Data      : IMaterialArr;
        DirtyIndex: Integer;
        function HandleData: Pointer;
        function Size: Integer; Inline;
        function Offset: Integer;
        destructor Destroy; override;
      end;
      {$EndIf}
      {$IfDef DCC}
      TMaterialNode = class (TDefaultNode, IMaterialMapHandle)
        Owner     : TavMaterialMap;
        Data      : IMaterialArr;
        function HandleData: Pointer;
        function Size: Integer; override;
        function Offset: Integer;
        destructor Destroy; override;
      end;
      {$EndIf}
      TNodes = {$IfDef FPC}specialize{$EndIf} TNodeManager<TMaterialNode>;
    private
      FNodes: TNodes;
    protected
      function DoBuild: Boolean; override;
    public
      function AddMaterials(const AMaterials: IMaterialArr): IMaterialMapHandle;
      procedure AfterConstruction; override;
      destructor Destroy; override;
    end;

    { TavBoneTransformMap }

    TavBoneTransformMap = class(TavTexture)
    private type
      {$IfDef FPC}
      TTransformNode = class (TInterfacedObject, IBoneTransformHandle)
        Owner     : TavBoneTransformMap;
        Range     : IMemRange;
        Instance  : IavMeshInstance;
        DirtyIndex: Integer;

        procedure Invalidate;
        function Offset: Integer;
        function GetMatrices: TMat4Arr;
        property Matrices: TMat4Arr read GetMatrices;

        function Size: Integer; Inline;
        destructor Destroy; override;
      end;
      {$EndIf}
      {$IfDef DCC}
      TTransformNode = class (TDefaultNode, IBoneTransformHandle)
        Owner     : TavBoneTransformMap;
        Instance  : IavMeshInstance;

        procedure Invalidate;
        function Offset: Integer;
        function GetMatrices: TMat4Arr;
        property Matrices: TMat4Arr read GetMatrices;

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
      function  AddPose(const AInstance: IavMeshInstance): IBoneTransformHandle;

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
    private
      FOwner: TavModelCollection;

      FModel: TModel;
      FInstanceIndex: Integer;

      FInstGPUData: IVBManagedHandle;

      FBoneTransformHandles: array of IBoneTransformHandle;
      FBoneTransformID:array of Int64;

      FMeshInst: IavMeshInstance;
      FMultiMesh: IavMeshInstanceArray;
      FMultiMeshPoseValid: Boolean;

      FName: string;

      FStatic: Boolean;

      procedure OnUnlink;
      procedure OnLink;

      procedure UpdateBoneTransform;
    public
      function GetTexMap: TavMultiTexture;

      function GetObj: TavModelInstance;

      function GetStatic: Boolean;
      function GetTransform: TMat4;
      procedure SetStatic(const AValue: Boolean);
      procedure SetTransform(const AValue: TMat4);

      function Mesh: IavMeshInstance;
      function MultiMesh: IavMeshInstanceArray;
      function CustomProps: ICustomProps;
      function Collection: TavModelCollection;
      function ModelName: String;
      function Name: String;

      property Static: Boolean read GetStatic write SetStatic;

      constructor Create(const AOwner: TavModelCollection);
      destructor Destroy; override;
    end;

    IModelHash = {$IfDef FPC}specialize{$EndIf} IHashMap<IavMesh, TModel>;
    TModelHash = {$IfDef FPC}specialize{$EndIf} THashMap<IavMesh, TModel>;

    IModelInstHash = {$IfDef FPC}specialize{$EndIf} IHashMap<IavMeshInstance, TavModelInstance>;
    TModelInstHash = {$IfDef FPC}specialize{$EndIf} THashMap<IavMeshInstance, TavModelInstance>;

    IModelInstSet = {$IfDef FPC}specialize{$EndIf} IHashSet<TavModelInstance>;
    TModelInstSet = {$IfDef FPC}specialize{$EndIf} THashSet<TavModelInstance>;

    TTextureKey = packed record
      Width : Integer;
      Height: Integer;
      Mips  : Integer;
    end;
  private const
    EmptyTexureKey: TTextureKey = (Width:0;Height:0;Mips:0);
  private type
    ITextureHash = {$IfDef FPC}specialize{$EndIf} IHashMap<TTextureKey, TavMultiTexture>;
    TTextureHash = {$IfDef FPC}specialize{$EndIf} THashMap<TTextureKey, TavMultiTexture>;

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
    FModelMultiInstances: IModelInstSet;

    FStaticForUpdateSet: IModelInstSet;

    function ObtainDummyTextureData(const Key: TTextureKey): ITextureData;
    function ObtainMap(const Key: TTextureKey): TavMultiTexture;

    procedure Draw(const Intances: IObjArr; SortByMaterial: Boolean); overload;

    function BuildModelMaterial(const AMaps: TavMultiTexture; const AMesh: IavMesh; const AMaterialIndex: Integer): TModelMaterial;
    function ObtainTModel(const AMesh: IavMesh): TModel;
  public
    function ModelsCount: Integer;
    procedure Reset;
    function NextModel(out AMesh: IavMesh): Boolean;
    function NextInstance(out AInstance: IavModelInstance): Boolean;

    procedure Select;
    procedure Draw(const Instances: array of IavModelInstance; SortByMaterial: Boolean = True); overload;
    procedure Draw(const Instances: array of IavModelInstanceArr; SortByMaterial: Boolean = True); overload;

    function  AddFromMesh(const AMesh: IavMesh; const AMeshCount: Integer): IavModelInstance;

    function  AddFromMeshInstance(const AMeshInstance: IavMeshInstance): IavModelInstance;
    function  AddFromMeshInstances(const AMeshInstances: IavMeshInstances): IavModelInstanceArr; overload;
    function  AddFromMeshInstances(const AMeshInstances: IavMeshInstanceArray): IavModelInstanceArr; overload;

    function  ObtainModel(const AMeshInstance: IavMeshInstance): IavModelInstance;
    function  ObtainModels(const AMeshInstances: IavMeshInstances): IavModelInstanceArr; overload;
    function  ObtainModels(const AMeshInstances: IavMeshInstanceArray): IavModelInstanceArr; overload;

    constructor Create(AParent: TavObject); override;
    destructor Destroy; override;
  end;

implementation

uses
  Math;

{ TModelInstanceGPUData }

class function TModelInstanceGPUData.Layout: IDataLayout;
begin
  Result := LB.Add('aiBoneMatOffset', ctFloat, 2).Finish();
end;

{ TavModelCollection.TavMaterialMap.TMaterialNode }

function TavModelCollection.TavMaterialMap.TMaterialNode.HandleData: Pointer;
begin
  Result := Self;
end;

function TavModelCollection.TavMaterialMap.TMaterialNode.Size: Integer;
begin
  Result := Data.Count;
end;

function TavModelCollection.TavMaterialMap.TMaterialNode.Offset: Integer;
begin
  Result := Range.Offset;
end;

destructor TavModelCollection.TavMaterialMap.TMaterialNode.Destroy;
begin
  inherited Destroy;
  Owner.FNodes.Del(Self);
end;

{ TavModelCollection.TModelMaterial }
procedure TavModelCollection.TModelMaterial.UpdateMapIndices;
  function GetOffset(const AHandle : IMTManagedHandle): Integer;
  begin
    if AHandle = nil then Result := -1 else Result := AHandle.Offset;
  end;
begin
  Material.mapDiffuse_Alpha_Translucency.x := GetOffset(mapDiffuse_Alpha);
  Material.mapDiffuse_Alpha_Translucency.z := GetOffset(mapDiffuse_Translucency);
  Material.mapDiffuse_Intensity_Color.x := GetOffset(mapDiffuse_Intensity);
  Material.mapDiffuse_Intensity_Color.z := GetOffset(mapDiffuse_Color);
  Material.mapShading_Ambient_Emit.x := GetOffset(mapShading_Ambient);
  Material.mapShading_Ambient_Emit.z := GetOffset(mapShading_Emit);
  Material.mapShading_Mirror_RayMirror.x := GetOffset(mapShading_Mirror);
  Material.mapShading_Mirror_RayMirror.z := GetOffset(mapShading_RayMirror);
  Material.mapSpecular_Intensity_Color.x := GetOffset(mapSpecular_Intensity);
  Material.mapSpecular_Intensity_Color.z := GetOffset(mapSpecular_Color);
  Material.mapSpecular_Hardness_mapGeometry_Normal.x := GetOffset(mapSpecular_Hardness);
  Material.mapSpecular_Hardness_mapGeometry_Normal.z := GetOffset(mapGeometry_Normal);
  Material.mapGeometry_Warp_Displace.x := GetOffset(mapGeometry_Warp);
  Material.mapGeometry_Warp_Displace.z := GetOffset(mapGeometry_Displace);
end;

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
  Result := Instance.PoseArray;
end;

function TavModelCollection.TavBoneTransformMap.TTransformNode.Size: Integer;
begin
  Result := Length(Matrices);
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
      x, y, i: Integer;
  begin
    start := ANode.Range.Offset;
    size  := ANode.Range.Size;
    stop  := start + size;
    i := 0;
    while start < stop do
    begin
      y := start div TexWidth;
      x := (start mod TexWidth);
      rowsize := min(stop - start, TexWidth - x);

      FTexH.SetMipImage(x*4, y, rowsize*4, 1, 0, 0, TImageFormat.R32G32B32A32F, @ANode.Matrices[i]);
      Inc(start, rowsize);
      Inc(i, rowsize);
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

function TavModelCollection.TavBoneTransformMap.AddPose(
  const AInstance: IavMeshInstance): IBoneTransformHandle;
var node: TTransformNode;
begin
  Result := nil;
  node := TTransformNode.Create;
  node.Owner := Self;
  node.Instance := AInstance;
  FNodes.Add(node);
  if FNodes.DirtyCount > 0 then
    Invalidate;
  Result := node;
end;

procedure TavModelCollection.TavBoneTransformMap.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TNodes.Create;
end;

destructor TavModelCollection.TavBoneTransformMap.Destroy;
begin
  FreeAndNil(FNodes);
  inherited Destroy;
end;

{ TavModelCollection.TavMaterialMap }

function TavModelCollection.TavMaterialMap.DoBuild: Boolean;
var matPixSize, i: Integer;
    GPUData: array of TModelMaterialGPU;
    node: TMaterialNode;
    n: Integer;
begin
  matPixSize := SizeOf(TModelMaterialGPU) div SizeOf(TVec4);
  Assert(SizeOf(TModelMaterialGPU) mod SizeOf(TVec4) = 0);
  if FTexH = nil then FTexH := Main.Context.CreateTexture;

  SetLength(GPUData, FNodes.RangeManSize);
  FNodes.Reset;
  while FNodes.Next(node) do
  begin
    n := node.Offset;
    for i := 0 to node.Size - 1 do
      GPUData[n+i] := node.Data[i].Material;
  end;

  FTexH.TargetFormat := TTextureFormat.RGBA32f;
  FTexH.AllocMem(matPixSize, Length(GPUData), 1, False);
  FTexH.SetMipImage(0, 0, matPixSize, Length(GPUData), 0, 0, TImageFormat.R32G32B32A32F, @GPUData[0]);
  Result := True;
end;

function TavModelCollection.TavMaterialMap.AddMaterials(const AMaterials: IMaterialArr): IMaterialMapHandle;
var node: TMaterialNode;
begin
  Result := nil;
  if AMaterials = nil then Exit;
  if AMaterials.Count = 0 then Exit;

  node := TMaterialNode.Create;
  node.Owner := Self;
  node.Data := AMaterials;
  FNodes.Add(node);
  if FNodes.DirtyCount > 0 then Invalidate;
  Result := node;
end;

procedure TavModelCollection.TavMaterialMap.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TNodes.Create;
  FNodes.Name := 'MaterialMap';
end;

destructor TavModelCollection.TavMaterialMap.Destroy;
begin
  FreeAndNil(FNodes);
  inherited Destroy;
end;

{ TavModelCollection.TavModelInstance }

procedure TavModelCollection.TavModelInstance.OnUnlink;
begin

end;

procedure TavModelCollection.TavModelInstance.OnLink;
begin

end;

procedure TavModelCollection.TavModelInstance.UpdateBoneTransform;
var newID: Int64;
    i: Integer;
begin
  if FMeshInst <> nil then
  begin
    newID := FMeshInst.PoseStateID;
    if FBoneTransformID[0] <> newID then
    begin
      FBoneTransformID[0] := newID;
      FBoneTransformHandles[0].Invalidate;
    end;
  end;

  if FMultiMesh <> nil then
  begin
    if not FMultiMeshPoseValid then
      for i := 0 to FMultiMesh.Count - 1 do
      begin
        newID := FMultiMesh[i].PoseStateID;
        if FBoneTransformID[i] <> newID then
        begin
          FBoneTransformID[i] := newID;
          FBoneTransformHandles[i].Invalidate;
        end;
        FMultiMeshPoseValid := True;
      end;
  end;
end;

function TavModelCollection.TavModelInstance.GetTexMap: TavMultiTexture;
begin
  Result := nil;
  if FModel = nil then Exit;
  Result := FModel.FMapTex;
end;

function TavModelCollection.TavModelInstance.GetObj: TavModelInstance;
begin
  Result := Self;
end;

function TavModelCollection.TavModelInstance.GetStatic: Boolean;
begin
  Result := FStatic;
end;

function TavModelCollection.TavModelInstance.GetTransform: TMat4;
begin
  if FMeshInst = nil then
    Exit(IdentityMat4)
  else
    Result := FMeshInst.Transform;
end;

procedure TavModelCollection.TavModelInstance.SetStatic(const AValue: Boolean);
begin
  if FStatic = AValue then Exit;
  FStatic := AValue;
  if FStatic then
    FOwner.FStaticForUpdateSet.AddOrSet(Self)
  else
    FOwner.FStaticForUpdateSet.Delete(Self);
end;

procedure TavModelCollection.TavModelInstance.SetTransform(const AValue: TMat4);
begin
  if FMeshInst = nil then Exit;
  FMeshInst.Transform := AValue;
  if FStatic then
    FOwner.FStaticForUpdateSet.AddOrSet(Self);
end;

function TavModelCollection.TavModelInstance.Mesh: IavMeshInstance;
begin
  Result := FMeshInst;
end;

function TavModelCollection.TavModelInstance.MultiMesh: IavMeshInstanceArray;
begin
  Result := FMultiMesh;
end;

function TavModelCollection.TavModelInstance.CustomProps: ICustomProps;
begin
  Result := FMeshInst.CustomProps;
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

constructor TavModelCollection.TavModelInstance.Create(const AOwner: TavModelCollection);
begin
  FOwner := AOwner;
end;

destructor TavModelCollection.TavModelInstance.Destroy;
begin
  if (FOwner <> nil) then
  begin
    if (FMeshInst <> nil) then
      FOwner.FModelInstances.Delete(FMeshInst);
    FOwner.FModelMultiInstances.Delete(Self);
    FOwner.FStaticForUpdateSet.Delete(Self);
  end;
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
  inst.FBoneTransformHandles := nil;
  inst.FInstGPUData := nil;
  inst.OnUnlink;
  inst.FInstanceIndex := -1;
  inst.FModel := nil;
  inst.FInstGPUData := nil;
  inst.FMeshInst := nil;
  if inst.FMultiMesh <> nil then
    inst.FMultiMesh.Clear(True);
  FInstances.Delete(n);

  if (not FInDestroy) and (FInstances.Count = 0) then
    Free;
end;

constructor TavModelCollection.TModel.Create;
begin
  FInstances := TList.Create;
end;

destructor TavModelCollection.TModel.Destroy;
var
  i: Integer;
begin
  FInDestroy := True;
  for i := FInstances.Count - 1 downto 0 do
    UnlinkInstance(TavModelInstance(FInstances[i]));
  FreeAndNil(FInstances);
  if Owner <> nil then
    Owner.FModels.Delete(Mesh);
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

function TavModelCollection.ObtainMap(const Key: TTextureKey): TavMultiTexture;
begin
  if not FMaps.TryGetValue(Key, Result) then
  begin
    Result := TavMultiTexture.Create(Self);
    Result.Name := 'ModelsMultiTexture';
    Result.TargetFormat := TTextureFormat.RGBA;
    Result.AutoGenerateMips := True;
    FMaps.Add(Key, Result);
  end;
end;

procedure TavModelCollection.Draw(const Intances: IObjArr; SortByMaterial: Boolean);
var mInst: TavModelInstance;
    lastDiffuse: TavMultiTexture;
    prog: TavProgram;
    i: Integer;
begin
  prog := Main.ActiveProgram;
  if prog = nil then Exit;

  if SortByMaterial then
    Intances.Sort;

  for i := 0 to Intances.Count - 1 do
    if not TavModelInstance(Intances[i]).Static then
      TavModelInstance(Intances[i]).UpdateBoneTransform;
  FStaticForUpdateSet.Reset;
  while FStaticForUpdateSet.Next(mInst) do
    mInst.UpdateBoneTransform;
  FStaticForUpdateSet.Clear;

  //for i := 0 to Intances.Count - 1 do
  //  TavModelInstance(Intances[i]).UpdateBoneTransform;
  Main.ActiveProgram.SetUniform('BoneTransform', FBoneTransform, Sampler_NoFilter);

  lastDiffuse := nil;
  for i := 0 to Intances.Count - 1 do
  begin
    mInst := TavModelInstance(Intances[i]);
    if lastDiffuse <> mInst.GetTexMap then
    begin
      lastDiffuse := mInst.GetTexMap;
      prog.SetUniform('Maps', lastDiffuse, Sampler_Linear);
    end;
    DrawManaged(prog, mInst.FModel.VBHandle, mInst.FModel.IBHandle, mInst.FInstGPUData, ptTriangles, Main.States.CullMode);
  end;
end;

function TavModelCollection.BuildModelMaterial(const AMaps: TavMultiTexture; const AMesh: IavMesh; const AMaterialIndex: Integer): TModelMaterial;
  function GetTexPair(const ATexData: ITextureData; const AFactor: Single;
                      out AHandle: IMTManagedHandle): TVec2;
  var NewAdded: Boolean;
  begin
    if ATexData = nil then
    begin
      AHandle := nil;
      Result := Vec(-1, 0);
      Exit;
    end;
    AHandle := AMaps.Add(ATexData, NewAdded);
    if NewAdded then FMaterials.Invalidate;
    Result.x := AHandle.Offset;
    Result.y := AFactor;
  end;

var meshMat: TMeshMaterial;
    meshMap: TMeshMaterialMaps;
begin
  meshMat := AMesh.Material[AMaterialIndex];
  meshMap := AMesh.MaterialMaps[AMaterialIndex];

  Result.Material.matDiff        := meshMat.matDiff;
  Result.Material.matSpec        := meshMat.matSpec;
  Result.Material.matSpecHardness:= meshMat.matSpecHardness;
  Result.Material.matSpecIOR     := meshMat.matSpecIOR;
  Result.Material.matEmitFactor  := meshMat.matEmitFactor;

  Result.Material.mapDiffuse_Alpha_Translucency.xy := GetTexPair(meshMap.Textures[texkDiffuse_Alpha].map, meshMap.Textures[texkDiffuse_Alpha].factor, Result.mapDiffuse_Alpha);
  Result.Material.mapDiffuse_Alpha_Translucency.zw := GetTexPair(meshMap.Textures[texkDiffuse_Translucency].map, meshMap.Textures[texkDiffuse_Translucency].factor, Result.mapDiffuse_Translucency);
  Result.Material.mapDiffuse_Intensity_Color.xy := GetTexPair(meshMap.Textures[texkDiffuse_Intensity].map, meshMap.Textures[texkDiffuse_Intensity].factor, Result.mapDiffuse_Intensity);
  Result.Material.mapDiffuse_Intensity_Color.zw := GetTexPair(meshMap.Textures[texkDiffuse_Color].map, meshMap.Textures[texkDiffuse_Color].factor, Result.mapDiffuse_Color);
  Result.Material.mapShading_Ambient_Emit.xy := GetTexPair(meshMap.Textures[texkShading_Ambient].map, meshMap.Textures[texkShading_Ambient].factor, Result.mapShading_Ambient);
  Result.Material.mapShading_Ambient_Emit.zw := GetTexPair(meshMap.Textures[texkShading_Emit].map, meshMap.Textures[texkShading_Emit].factor, Result.mapShading_Emit);
  Result.Material.mapShading_Mirror_RayMirror.xy := GetTexPair(meshMap.Textures[texkShading_Mirror].map, meshMap.Textures[texkShading_Mirror].factor, Result.mapShading_Mirror);
  Result.Material.mapShading_Mirror_RayMirror.zw := GetTexPair(meshMap.Textures[texkShading_RayMirror].map, meshMap.Textures[texkShading_RayMirror].factor, Result.mapShading_RayMirror);
  Result.Material.mapSpecular_Intensity_Color.xy := GetTexPair(meshMap.Textures[texkSpecular_Intensity].map, meshMap.Textures[texkSpecular_Intensity].factor, Result.mapSpecular_Intensity);
  Result.Material.mapSpecular_Intensity_Color.zw := GetTexPair(meshMap.Textures[texkSpecular_Color].map, meshMap.Textures[texkSpecular_Color].factor, Result.mapSpecular_Color);
  Result.Material.mapSpecular_Hardness_mapGeometry_Normal.xy := GetTexPair(meshMap.Textures[texkSpecular_Hardness].map, meshMap.Textures[texkSpecular_Hardness].factor, Result.mapSpecular_Hardness);
  Result.Material.mapSpecular_Hardness_mapGeometry_Normal.zw := GetTexPair(meshMap.Textures[texkGeometry_Normal].map, meshMap.Textures[texkGeometry_Normal].factor, Result.mapGeometry_Normal);
  Result.Material.mapGeometry_Warp_Displace.xy := GetTexPair(meshMap.Textures[texkGeometry_Warp].map, meshMap.Textures[texkGeometry_Warp].factor, Result.mapGeometry_Warp);
  Result.Material.mapGeometry_Warp_Displace.zw := GetTexPair(meshMap.Textures[texkGeometry_Displace].map, meshMap.Textures[texkGeometry_Displace].factor, Result.mapGeometry_Displace);
end;

function TavModelCollection.ObtainTModel(const AMesh: IavMesh): TModel;
var tKey: TTextureKey;
    mapTex: TavMultiTexture;
    materials: IMaterialArr;
    j: Integer;
begin
  if not FModels.TryGetValue(AMesh, Result) then
  begin
    Result := TModel.Create;
    Result.Owner := Self;
    Result.Mesh := AMesh;
    Result.VBHandle := FVB.Add(Result.Mesh.Vert as IVerticesData);
    Result.IBHandle := FIB.Add(Result.Mesh.Ind as IIndicesData);

    //prepare map size
    tKey.Width := -1;
    tKey.Height := -1;
    tKey.Mips := -1;
    for j := 0 to Result.Mesh.MaterialsCount-1 do
    begin
      tKey.Width := Result.Mesh.MaterialMaps[j].Width;
      tKey.Height := Result.Mesh.MaterialMaps[j].Height;
      tKey.Mips := Result.Mesh.MaterialMaps[j].MipCount;
      if tKey.Width > 0 then Break;
    end;
    if tKey.Width < 0 then
    begin
      tKey.Width := 1;
      tKey.Height := 1;
      tKey.Mips := 1;
    end;

    mapTex := ObtainMap(tKey);
    materials := TMaterialArr.Create;
    materials.Capacity := Result.Mesh.MaterialsCount;
    for j := 0 to Result.Mesh.MaterialsCount - 1 do
      materials.Add(BuildModelMaterial(mapTex, Result.Mesh, j));
    Result.MaterialHandle := FMaterials.AddMaterials(materials);
    Result.FMapTex := mapTex;
    FModels.AddOrSet(Result.Mesh, Result);
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

function TavModelCollection.NextModel(out AMesh: IavMesh): Boolean;
var Dummy: TModel;
begin
  Result := FModels.Next(AMesh, Dummy);
end;

function TavModelCollection.NextInstance(out AInstance: IavModelInstance): Boolean;
var inst: TavModelInstance;
begin
  inst := nil;
  Result := FModelInstances.NextValue(inst);
  AInstance := inst;
end;

procedure TavModelCollection.Select;
begin
  Assert(Main.ActiveProgram <> Nil);
  if not FVB.HasData then Exit;
  if not FIB.HasData then Exit;
  if not FInstVB.HasData then Exit;
  Main.ActiveProgram.SetAttributes(FVB, FIB, FInstVB);
  Main.ActiveProgram.SetUniform('Materials', FMaterials, Sampler_NoFilter);
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

procedure TavModelCollection.Draw(const Instances: array of IavModelInstanceArr; SortByMaterial: Boolean);
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

function TavModelCollection.AddFromMesh(const AMesh: IavMesh; const AMeshCount: Integer): IavModelInstance;
var instances: IavMeshInstanceArray;
    i: Integer;
    modelInst: TavModelInstance;
    model: TModel;
    instGPU: IModelInstanceGPUDataArr;
    instGPU_one: TModelInstanceGPUData;
begin
  instances := TavMeshInstanceArray.Create();
  instances.Capacity := AMeshCount;
  for i := 0 to AMeshCount - 1 do
    instances.Add(AMesh.CreateInstance(''));

  model := ObtainTModel(AMesh);

  modelInst := TavModelInstance.Create(Self);
  modelInst.FMeshInst := nil;
  modelInst.FMultiMesh := instances;

  instGPU := TModelInstanceGPUDataArr.Create;
  SetLength(modelInst.FBoneTransformHandles, instances.Count);
  SetLength(modelInst.FBoneTransformID, instances.Count);
  for i := 0 to instances.Count - 1 do
  begin
    modelInst.FBoneTransformHandles[i] := FBoneTransform.AddPose(instances[i]);
    instGPU_one.aiBoneMatOffset := Vec(modelInst.FBoneTransformHandles[i].Offset, model.MaterialHandle.Offset);
    instGPU.Add(instGPU_one);
  end;
  modelInst.FModel := model;
  modelInst.FInstGPUData := FInstVB.Add(instGPU as IVerticesData);
  modelInst.FInstanceIndex := model.FInstances.Count;
  modelInst.FName := '';
  model.FInstances.Add(modelInst);
  modelInst.OnLink;
  FModelMultiInstances.Add(modelInst);
  modelInst.Static := True;

  Result := modelInst;
end;

function TavModelCollection.AddFromMeshInstance(const AMeshInstance: IavMeshInstance): IavModelInstance;
{
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
}
var
  model: TModel;

  modelInst: TavModelInstance;
  instGPU : IModelInstanceGPUDataArr;
  instGPU_one: TModelInstanceGPUData;
begin
  Assert(not FModelInstances.Contains(AMeshInstance), 'Instance "'+AMeshInstance.Name+'" already in set');

  model := ObtainTModel(AMeshInstance.Mesh);

  modelInst := TavModelInstance.Create(Self);
  modelInst.FMeshInst := AMeshInstance;
  if modelInst.FMeshInst.Pose <> nil then
    modelInst.FMeshInst.Pose.SetAnimationState([]);
  SetLength(modelInst.FBoneTransformHandles, 1);
  SetLength(modelInst.FBoneTransformID, 1);
  modelInst.FBoneTransformHandles[0] := FBoneTransform.AddPose(modelInst.FMeshInst);

  instGPU := TModelInstanceGPUDataArr.Create;
  instGPU_one.aiBoneMatOffset := Vec(modelInst.FBoneTransformHandles[0].Offset, model.MaterialHandle.Offset);
  instGPU.Add(instGPU_one);
  modelInst.FModel := model;
  modelInst.FInstGPUData := FInstVB.Add(instGPU as IVerticesData);
  modelInst.FInstanceIndex := model.FInstances.Count;
  modelInst.FName := AMeshInstance.Name;
  model.FInstances.Add(modelInst);
  modelInst.OnLink;
  FModelInstances.Add(AMeshInstance, modelInst);
  modelInst.Static := not ( (modelInst.FMeshInst.Pose <> nil) and (modelInst.FMeshInst.Pose.Armature <> nil) );

  Result := modelInst;
end;

function TavModelCollection.AddFromMeshInstances(const AMeshInstances: IavMeshInstances): IavModelInstanceArr;
var inst: IavMeshInstance;
begin
  Result := TavModelInstanceArr.Create;
  AMeshInstances.Reset;
  while AMeshInstances.NextValue(inst) do
    Result.Add(AddFromMeshInstance(inst));
end;

function TavModelCollection.AddFromMeshInstances(const AMeshInstances: IavMeshInstanceArray): IavModelInstanceArr;
var i: Integer;
begin
  Result := TavModelInstanceArr.Create;
  for i := 0 to AMeshInstances.Count - 1 do
    Result.Add(AddFromMeshInstance(AMeshInstances[i]));
end;

function TavModelCollection.ObtainModel(const AMeshInstance: IavMeshInstance): IavModelInstance;
var inst: TavModelInstance;
begin
  if not FModelInstances.TryGetValue(AMeshInstance, inst) then
    Result := AddFromMeshInstance(AMeshInstance)
  else
    Result := inst;
end;

function TavModelCollection.ObtainModels(const AMeshInstances: IavMeshInstances): IavModelInstanceArr;
var inst: IavMeshInstance;
begin
  Result := TavModelInstanceArr.Create;
  AMeshInstances.Reset;
  while AMeshInstances.NextValue(inst) do
    Result.Add(ObtainModel(inst));
end;

function TavModelCollection.ObtainModels(const AMeshInstances: IavMeshInstanceArray): IavModelInstanceArr;
var i: Integer;
begin
  Result := TavModelInstanceArr.Create;
  for i := 0 to AMeshInstances.Count - 1 do
    Result.Add(ObtainModel(AMeshInstances[i]));
end;

constructor TavModelCollection.Create(AParent: TavObject);
begin
  inherited;
  FModels := TModelHash.Create;
  FModelInstances := TModelInstHash.Create;
  FModelMultiInstances := TModelInstSet.Create;
  FStaticForUpdateSet := TModelInstSet.Create;

  FVB := TavVBManaged.Create(Self);
  FVB.CullMode := cmBack;
  FVB.Name := 'ModelsVB';
  FIB := TavIBManaged.Create(Self);
  FIB.Name := 'ModelsIB';
  FInstVB := TavVBManaged.Create(Self);
  FInstVB.Name := 'ModelsInstVB';
  FMaterials := TavMaterialMap.Create(Self);
  FBoneTransform := TavBoneTransformMap.Create(Self);
  FMaps := TTextureHash.Create;

  FDummyTexData := TDummyTexDataHash.Create;
end;

destructor TavModelCollection.Destroy;
var m: TModel;
    mesh: IavMesh;
    inst: TavModelInstance;
begin
  FModelInstances.Reset;
  while FModelInstances.NextValue(inst) do
  begin
    inst.FOwner := nil;
    if Assigned(inst.FModel) then
      inst.FModel.UnlinkInstance(inst);
  end;
  FModelInstances.Clear;

  FModelMultiInstances.Reset;
  while FModelMultiInstances.Next(inst) do
  begin
    inst.FOwner := nil;
    if Assigned(inst.FModel) then
      inst.FModel.UnlinkInstance(inst);
  end;
  FModelMultiInstances.Clear;

  FModels.Reset;
  while FModels.Next(mesh, m) do
    m.Free;
  FModels.Clear;

  inherited Destroy;
end;

end.

