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
    aiBoneMatDifNormOffset: TVec4;
    function Layout: IDataLayout; override;
  end;


  { IavModelInstance }

  IavModelInstance = interface
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
      function GetTexMap: TavTexture;

      function GetObj: TavModelInstance;

      function Collection: TavModelCollection;
      function ModelName: String;
      function Name: String;

      destructor Destroy; override;
    end;

    IModelHash = {$IfDef FPC}specialize{$EndIf} IHashMap<IavMesh, TModel>;
    TModelHash = {$IfDef FPC}specialize{$EndIf} THashMap<IavMesh, TModel>;

    IModelInstHash = {$IfDef FPC}specialize{$EndIf} IHashMap<IavMeshInstance, IavModelInstance>;
    TModelInstHash = {$IfDef FPC}specialize{$EndIf} THashMap<IavMeshInstance, IavModelInstance>;

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
    function NextModel(out AMesh: IavMesh): Boolean;
    function NextInstance(out AInstance: IavModelInstance): Boolean;
    procedure DeleteInstance(const AInstance: IavMeshInstance);

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

      FTexH.SetMipImage(x*4, y, rowsize*4, 1, 0, 0, TImageFormat.R32G32B32A32F, @ANode.Matrices[0]);
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

function TavModelCollection.NextModel(out AMesh: IavMesh): Boolean;
var Dummy: TModel;
begin
  Result := FModels.Next(AMesh, Dummy);
end;

function TavModelCollection.NextInstance(out AInstance: IavModelInstance
  ): Boolean;
begin
  Result := FModelInstances.NextValue(AInstance);
end;

procedure TavModelCollection.DeleteInstance(const AInstance: IavMeshInstance);
begin
  FModelInstances.Delete(AInstance);
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

    FModels.AddOrSet(model.Mesh, model);
  end;

  modelInst := TavModelInstance.Create;
  modelInst.FMeshInst := AMeshInstance;
  modelInst.FMeshInst.Pose.SetAnimationState([]);
  modelInst.FBoneTransformHandle := FBoneTransform.AddPose(modelInst.FMeshInst);
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
begin
  if not FModelInstances.TryGetValue(AMeshInstance, Result) then
    Result := AddFromMeshInstance(AMeshInstance);
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
  FIB := TavIBManaged.Create(Self);
  FInstVB := TavVBManaged.Create(Self);
  FMaterials := TavMaterialMap.Create(Self);
  FBoneTransform := TavBoneTransformMap.Create(Self);
  FMaps := TTextureHash.Create;

  FDummyTexData := TDummyTexDataHash.Create;
end;

destructor TavModelCollection.Destroy;
var m: TModel;
    mesh: IavMesh;
begin
  FModelInstances.Clear;
  FModels.Reset;
  while FModels.Next(mesh, m) do
    m.Free;
  FModels.Clear;
  inherited Destroy;
end;

end.

