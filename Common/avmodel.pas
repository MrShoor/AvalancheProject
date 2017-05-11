unit avModel;
{$I avConfig.inc}

interface

uses
  Classes, SysUtils,
  mutils,
  avBase, avRes, avTess, avTypes, avContnrs, avMesh, avTexLoader;

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
    aiBoneMatOffset: TVec2;
    function Layout: IDataLayout; override;
  end;


  { IavModelInstance }

  IavModelInstance = interface
    function Mesh: IavMeshInstance;
    function Collection: TavModelCollection;
    function ModelName: String;
    function Name: String;
  end;

  IavModelInstanceArr = {$IfDef FPC}specialize{$EndIf} IArray<IavModelInstance>;
  TavModelInstanceArr = {$IfDef FPC}specialize{$EndIf} TArray<IavModelInstance>;

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

      FBoneTransformHandle: IBoneTransformHandle;
      FBoneTransformID: Int64;

      FMeshInst: IavMeshInstance;

      FName: string;

      procedure OnUnlink;
      procedure OnLink;

      function GetPoseArr: TMat4Arr;
      procedure UpdateBoneTransform;
    public
      function GetTexMap: TavMultiTexture;

      function GetObj: TavModelInstance;

      function Mesh: IavMeshInstance;
      function Collection: TavModelCollection;
      function ModelName: String;
      function Name: String;

      constructor Create(const AOwner: TavModelCollection);
      destructor Destroy; override;
    end;

    IModelHash = {$IfDef FPC}specialize{$EndIf} IHashMap<IavMesh, TModel>;
    TModelHash = {$IfDef FPC}specialize{$EndIf} THashMap<IavMesh, TModel>;

    IModelInstHash = {$IfDef FPC}specialize{$EndIf} IHashMap<IavMeshInstance, TavModelInstance>;
    TModelInstHash = {$IfDef FPC}specialize{$EndIf} THashMap<IavMeshInstance, TavModelInstance>;

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

    function ObtainDummyTextureData(const Key: TTextureKey): ITextureData;
    function ObtainMap(const Key: TTextureKey): TavMultiTexture;

    procedure Draw(const Intances: IObjArr; SortByMaterial: Boolean); overload;
  public
    function ModelsCount: Integer;
    procedure Reset;
    function NextModel(out AMesh: IavMesh): Boolean;
    function NextInstance(out AInstance: IavModelInstance): Boolean;

    procedure Select;
    procedure Draw(const Instances: array of IavModelInstance; SortByMaterial: Boolean = True); overload;
    procedure Draw(const Instances: array of IavModelInstanceArr; SortByMaterial: Boolean = True); overload;

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
  offset := GetOffset(@aiBoneMatOffset);
  Result := LBuilder.Add('aiBoneMatOffset', ctFloat, 2, False, offset);
end;

function TModelInstanceGPUData.Layout: IDataLayout;
begin
  if not Assigned(Layout_ModelInstanceGPUData) then
    Layout_ModelInstanceGPUData := AddLayoutFields(LB).Finish();
  Result := Layout_ModelInstanceGPUData;
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

procedure TavModelCollection.TavModelInstance.UpdateBoneTransform;
var newID: Int64;
begin
  newID := FMeshInst.PoseStateID;
  if FBoneTransformID <> newID then
  begin
    FBoneTransformID := newID;
    FBoneTransformHandle.Invalidate;
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

function TavModelCollection.TavModelInstance.Mesh: IavMeshInstance;
begin
  Result := FMeshInst;
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
  if FOwner <> nil then
    FOwner.FModelInstances.Delete(FMeshInst);
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
    DrawManaged(prog, mInst.FModel.VBHandle, mInst.FModel.IBHandle, mInst.FInstGPUData, ptTriangles, Main.States.CullMode);
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
  function BuildModelMaterial(const AMaps: TavMultiTexture; const AMesh: IavMesh; const AMaterialIndex: Integer): TModelMaterial;
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

    Result.Material.mapDiffuse_Alpha_Translucency.xy := GetTexPair(meshMap.mapDiffuse_Alpha, meshMap.mapDiffuse_AlphaFactor, Result.mapDiffuse_Alpha);
    Result.Material.mapDiffuse_Alpha_Translucency.zw := GetTexPair(meshMap.mapDiffuse_Translucency, meshMap.mapDiffuse_TranslucencyFactor, Result.mapDiffuse_Translucency);
    Result.Material.mapDiffuse_Intensity_Color.xy := GetTexPair(meshMap.mapDiffuse_Intensity, meshMap.mapDiffuse_IntensityFactor, Result.mapDiffuse_Intensity);
    Result.Material.mapDiffuse_Intensity_Color.zw := GetTexPair(meshMap.mapDiffuse_Color, meshMap.mapDiffuse_ColorFactor, Result.mapDiffuse_Color);
    Result.Material.mapShading_Ambient_Emit.xy := GetTexPair(meshMap.mapShading_Ambient, meshMap.mapShading_AmbientFactor, Result.mapShading_Ambient);
    Result.Material.mapShading_Ambient_Emit.zw := GetTexPair(meshMap.mapShading_Emit, meshMap.mapShading_EmitFactor, Result.mapShading_Emit);
    Result.Material.mapShading_Mirror_RayMirror.xy := GetTexPair(meshMap.mapShading_Mirror, meshMap.mapShading_MirrorFactor, Result.mapShading_Mirror);
    Result.Material.mapShading_Mirror_RayMirror.zw := GetTexPair(meshMap.mapShading_RayMirror, meshMap.mapShading_RayMirrorFactor, Result.mapShading_RayMirror);
    Result.Material.mapSpecular_Intensity_Color.xy := GetTexPair(meshMap.mapSpecular_Intensity, meshMap.mapSpecular_IntensityFactor, Result.mapSpecular_Intensity);
    Result.Material.mapSpecular_Intensity_Color.zw := GetTexPair(meshMap.mapSpecular_Color, meshMap.mapSpecular_ColorFactor, Result.mapSpecular_Color);
    Result.Material.mapSpecular_Hardness_mapGeometry_Normal.xy := GetTexPair(meshMap.mapSpecular_Hardness, meshMap.mapSpecular_HardnessFactor, Result.mapSpecular_Hardness);
    Result.Material.mapSpecular_Hardness_mapGeometry_Normal.zw := GetTexPair(meshMap.mapGeometry_Normal, meshMap.mapGeometry_NormalFactor, Result.mapGeometry_Normal);
    Result.Material.mapGeometry_Warp_Displace.xy := GetTexPair(meshMap.mapGeometry_Warp, meshMap.mapGeometry_WarpFactor, Result.mapGeometry_Warp);
    Result.Material.mapGeometry_Warp_Displace.zw := GetTexPair(meshMap.mapGeometry_Displace, meshMap.mapGeometry_DisplaceFactor, Result.mapGeometry_Displace);
  end;

var
  model: TModel;
  j: Integer;
  tKey: TTextureKey;
  mapTex: TavMultiTexture;

  modelInst: TavModelInstance;
  materials: IMaterialArr;
  instGPU : TModelInstanceGPUData;
begin
  Assert(not FModelInstances.Contains(AMeshInstance), 'Instance "'+AMeshInstance.Name+'" already in set');

  if not FModels.TryGetValue(AMeshInstance.Mesh, model) then
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
    for j := 0 to model.Mesh.MaterialsCount-1 do
    begin
      tKey.Width := model.Mesh.MaterialMaps[j].Width;
      tKey.Height := model.Mesh.MaterialMaps[j].Height;
      tKey.Mips := model.Mesh.MaterialMaps[j].MipCount;
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
    materials.Capacity := model.Mesh.MaterialsCount;
    for j := 0 to model.Mesh.MaterialsCount - 1 do
      materials.Add(BuildModelMaterial(mapTex, model.Mesh, j));
    model.MaterialHandle := FMaterials.AddMaterials(materials);
    model.FMapTex := mapTex;
    FModels.AddOrSet(model.Mesh, model);
  end;

  modelInst := TavModelInstance.Create(Self);
  modelInst.FMeshInst := AMeshInstance;
  if modelInst.FMeshInst.Pose <> nil then
    modelInst.FMeshInst.Pose.SetAnimationState([]);
  modelInst.FBoneTransformHandle := FBoneTransform.AddPose(modelInst.FMeshInst);
  instGPU := TModelInstanceGPUData.Create;
  instGPU.aiBoneMatOffset := Vec(modelInst.FBoneTransformHandle.Offset, model.MaterialHandle.Offset);
  modelInst.FModel := model;
  modelInst.FInstGPUData := FInstVB.Add(instGPU as IVerticesData);
  modelInst.FInstanceIndex := model.FInstances.Count;
  modelInst.FName := AMeshInstance.Name;
  model.FInstances.Add(modelInst);
  modelInst.OnLink;
  FModelInstances.Add(AMeshInstance, modelInst);

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
  FModels.Reset;
  while FModels.Next(mesh, m) do
    m.Free;
  FModels.Clear;

  inherited Destroy;
end;

end.

