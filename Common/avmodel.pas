unit avModel;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils,
  avBase, avRes, avTypes, avContnrs, avMesh, avTexLoader;

const
  KeyFramePerMSec = 1/40;
  Default_GrowSpeed = 10000;
  Default_FadeSpeed = 10000;

type
  TavModelCollection = class;

  IavModelInstance = interface
    procedure AnimationStart(const AnimationName: string; GrowSpeed: Single = Default_GrowSpeed);
    procedure AnimationStop (const AnimationName: string; FadeSpeed: Single = Default_FadeSpeed);

    function Collection: TavModelCollection;
    procedure Draw;
  end;

  { TModel }

  TModel = class
  private
    Owner: TavModelCollection;
    VBHandle: TVBManagedHandle;
    IBHandle: TIBManagedHandle;
    FInstances: TList;

    procedure UnlinkInstance(const Obj: TObject);
  public
    Mesh : IavMesh;
    function CreateInstance: IavModelInstance;
    constructor Create;
    destructor Destroy; override;
  end;

  { TavModelCollection }

  TavModelCollection = class (TavMainRenderChild)
  private type
    IModelHash = specialize IHashMap<String, TModel, TMurmur2HashString>;
    TModelHash = specialize THashMap<String, TModel, TMurmur2HashString>;
  private
    FVB: TavVBManaged;
    FIB: TavIBManaged;

    FModels: IModelHash;

    procedure AddModel(const AModel: TModel);
  public
    function ModelsCount: Integer;
    procedure Reset;
    function Next(out ModelName: string): Boolean;

    function CreateInstance(const ModelName: String): IavModelInstance;

    procedure Select;

    procedure AddFromFile(const FileName: String; const TexManager: ITextureManager = nil);

    constructor Create(AParent: TavObject); override;
    destructor Destroy; override;
  end;

implementation

uses
  Math;

type
    { TavModelInstance }

    TavModelInstance = class(TInterfacedObject, IavModelInstance)
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

      FPose: TavTexture;
      FDiffuse: TavTexture;

      FAnimationStates   : array of TMeshAnimationState;
      FAnimationPlayState: array of TAnimationPlayState;

      procedure OnUnlink;
      procedure OnLink;

      procedure UpdateAnimationStates;
    public
      procedure AnimationStart(const AnimationName: string; GrowSpeed: Single);
      procedure AnimationStop (const AnimationName: string; FadeSpeed: Single);

      function Collection: TavModelCollection;
      procedure Draw;

      destructor Destroy; override;
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
      FadeValue := 0
    else
      FadeValue := Min(1, (ATime - StopTime) / FadeSpeed);
    AWeight := Max(0, AWeight - FadeValue);
    if AWeight = 0 then AComplete := True;
  end;

end;

{ TavModelInstance }

procedure TavModelInstance.OnUnlink;
begin
  FreeAndNil(FPose);
  FreeAndNil(FDiffuse);
end;

procedure TavModelInstance.OnLink;
begin
  FPose := TavTexture.Create(Collection);
  FPose.TargetFormat := TTextureFormat.RGBA32f;
  if Assigned(FModel.Mesh.Armature) then
    FPose.TexData := FModel.Mesh.Armature.BoneTransformData;

  FDiffuse := TavTexture.Create(Collection);
  FDiffuse.AutoGenerateMips := True;
  if FModel.Mesh.MaterialsCount > 0 then
    FDiffuse.TexData := FModel.Mesh.GetMaterialMaps(0).matDiffMap;
end;

procedure TavModelInstance.UpdateAnimationStates;
var i, n: Integer;
    currTime: Int64;
    Complete: Boolean;
begin
  if Length(FAnimationStates) = 0 then Exit;

  //update
  currTime := Collection.Main.Time64;
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

procedure TavModelInstance.AnimationStart(const AnimationName: string; GrowSpeed: Single);
var anim: IavAnimation;
    animIndex: Integer;
    n: Integer;
    i: Integer;
begin
  if FModel = nil then Exit;
  if FModel.Mesh.Armature = nil then Exit;
  anim := FModel.Mesh.Armature.FindAnim(AnimationName);
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

procedure TavModelInstance.AnimationStop(const AnimationName: string; FadeSpeed: Single);
var anim: IavAnimation;
    animIndex: Integer;
    i: Integer;
begin
  if FModel = nil then Exit;
  if FModel.Mesh.Armature = nil then Exit;
  anim := FModel.Mesh.Armature.FindAnim(AnimationName);
  animIndex := anim.Index;

  for i := 0 to Length(FAnimationStates) - 1 do
    if FAnimationStates[i].Index = animIndex then
    begin
      if FAnimationPlayState[i].StopTime >= 0 then Exit;
      FAnimationPlayState[i].StopTime := Collection.Main.Time64;
      FAnimationPlayState[i].FadeSpeed := FadeSpeed;
      Break;
    end;
end;

function TavModelInstance.Collection: TavModelCollection;
begin
  if FModel = nil then Exit(nil);
  Result := FModel.Owner;
end;

procedure TavModelInstance.Draw;
var prog: TavProgram;
begin
  if FModel = nil then Exit;
  prog := Collection.Main.ActiveProgram;
  if prog = nil then Exit;

  UpdateAnimationStates;
  if Assigned(FModel.Mesh.Armature) and (Length(FAnimationStates)>0) then
  begin
    FModel.Mesh.Armature.UpdatePoseData(FPose.TexData, FAnimationStates);
    FPose.Invalidate;
  end;

  if Assigned(FPose.TexData) then
  begin
    prog.SetUniform('BonePixelHeight', 1/FPose.Height);
    prog.SetUniform('BoneTransform', FPose, Sampler_NoFilter);
  end
  else
    prog.SetUniform('BonePixelHeight', 0.0);

  if Assigned(FDiffuse.TexData) then
    prog.SetUniform('DiffuseMap', FDiffuse, Sampler_Linear);

  DrawManaged(prog, FModel.VBHandle, FModel.IBHandle, nil);
end;

destructor TavModelInstance.Destroy;
begin
  if Assigned(FModel) then
    FModel.UnlinkInstance(Self);
  inherited Destroy;
end;

{ TModel }

procedure TModel.UnlinkInstance(const Obj: TObject);
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
  inst.OnUnlink;
  inst.FInstanceIndex := -1;
  inst.FModel := nil;
  FInstances.Delete(n);
end;

function TModel.CreateInstance: IavModelInstance;
var inst: TavModelInstance;
begin
  inst := TavModelInstance.Create;
  inst.FModel := Self;
  inst.FInstanceIndex := FInstances.Count;
  FInstances.Add(inst);
  inst.OnLink;
  Result := inst;
end;

constructor TModel.Create;
begin
  FInstances := TList.Create;
end;

destructor TModel.Destroy;
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

function TavModelCollection.CreateInstance(const ModelName: String): IavModelInstance;
var m: TModel;
begin
  if FModels.TryGetValue(ModelName, m) then
    Result := m.CreateInstance
  else
    Result := nil;
end;

procedure TavModelCollection.Select;
begin
  Assert(Main.ActiveProgram <> Nil);
  Main.ActiveProgram.SetAttributes(FVB, FIB, nil);
end;

procedure TavModelCollection.AddFromFile(const FileName: String; const TexManager: ITextureManager);
var meshes: TavMeshes;
    model: TModel;
    i: Integer;
begin
  LoadFromFile(FileName, meshes, TexManager);
  for i := 0 to Length(meshes) - 1 do
  begin
    model := TModel.Create;
    model.Owner := Self;
    model.Mesh := meshes[i];
    model.VBHandle := FVB.Add(model.Mesh.Vert as IVerticesData);
    model.IBHandle := FIB.Add(model.Mesh.Ind as IIndicesData);
    AddModel(model);
  end;
end;

constructor TavModelCollection.Create(AParent: TavObject);
begin
  inherited;
  FModels := TModelHash.Create('', nil);
  FVB := TavVBManaged.Create(Self);
  FIB := TavIBManaged.Create(Self);
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

