unit SpineIntf;

interface

//some help: http://ru.esotericsoftware.com/spine-c

uses
  SpineH,
  intfUtils,
  mutils,
  avRes,
  avContnrs,
  avTess,
  avTypes;

type
  TSpineVertex = packed record
    vsCoord    : TVec3;
    vsTexCrd   : TVec2;
    vsColor    : TVec4;
    vsAtlasRef : Single;
    class function Layout: IDataLayout; static;
  end;
  ISpineVertices = {$IfDef FPC}specialize{$EndIf} IArray<TSpineVertex>;
  TSpineVertices = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TSpineVertex>;

  IspAtlas = interface
    function Handle: PspAtlas;
  end;

  IspSkeletonData = interface
    function Handle: PspSkeletonData;
  end;

  IspSkeleton = interface
    function Handle : PspSkeleton;
    function Data : IspSkeletonData;
    function Texture: TavAtlasArrayReferenced;

    function WriteVertices(const AVert: ISpineVertices; const AZ: Single; const DoUpdateWorldTransform: Boolean = True): Integer;

    function  GetPos: TVec2;
    function  GetFlipX: Boolean;
    function  GetFlipY: Boolean;
    procedure SetPos(const Value: TVec2);
    procedure SetFlipX(const Value: Boolean);
    procedure SetFlipY(const Value: Boolean);

    property Pos: TVec2 read GetPos write SetPos;
    property FlipX: Boolean read GetFlipX write SetFlipX;
    property FlipY: Boolean read GetFlipY write SetFlipY;
  end;

  IspAnimationStateData = interface
    function Handle : PspAnimationStateData;
    function SkeletonData : IspSkeletonData;

    function  GetDefaultMix: Single;
    procedure SetDefaultMix(const Value: Single);

    property DefaultMix: Single read GetDefaultMix write SetDefaultMix;
  end;

  IspAnimationState = interface
    function Handle : PspAnimationState;
    function Data : IspAnimationStateData;

    procedure Update(const ADelta: Single);
    procedure Apply(const ASkeleton: IspSkeleton);

    procedure ClearTracks;
    procedure ClearTrack(const AIndex: Integer);

    procedure SetAnimationByName(ATrackIndex: Integer; const AnimationName: string; loop: Boolean);

    function  GetTimeScale: Single;
    procedure SetTimeScale(const Value: Single);
    property  TimeScale: Single read GetTimeScale write SetTimeScale;
  end;

function Create_IspAtlas(const AFileName: string): IspAtlas;
function Create_IspSkeletonData(const ASkelFileName: string; const AAtlas: IspAtlas; const AScale: Single = 1): IspSkeletonData;

function Create_IspSkeleton(const AData: IspSkeletonData; const ATexture: TavAtlasArrayReferenced): IspSkeleton; overload;
function Create_IspSkeleton(const AAtlasFileName, ASkelFileName: string; const ATexture: TavAtlasArrayReferenced; const AScale: Single = 1): IspSkeleton; overload;

function Create_IspAnimationStateData(const ASkeletonData: IspSkeletonData): IspAnimationStateData;
function Create_IspAnimationState(const AData: IspAnimationStateData): IspAnimationState; overload;
function Create_IspAnimationState(const ASkel: IspSkeleton; const ADefaultMix: Single): IspAnimationState; overload;

procedure ClearCache;

implementation

uses
  avTexLoader;

type
  TAtlas = class(TInterfacedObject, IspAtlas)
  private
    FspAtlas : PspAtlas;
  public
    function Handle: PspAtlas;
    constructor Create(const AFileName: string);
    destructor Destroy; override;
  end;

  TSkeletonData = class(TInterfacedObject, IspSkeletonData)
  private
    FspSkeletonData : PspSkeletonData;
    FAtlas : IspAtlas;
  public
    function Handle: PspSkeletonData;
    constructor Create(const ASkelFileName: string; const AAtlas: IspAtlas; const AScale: Single = 1);
    destructor Destroy; override;
  end;

  TSkeleton = class(TInterfacedObject, IspSkeleton)
  private type
    ISpritesSet = {$IfDef FPC}specialize{$EndIf} IHashSet<ISpriteIndex>;
    TSpritesSet = {$IfDef FPC}specialize{$EndIf} THashSet<ISpriteIndex>;
  private
    FspSkeleton : PspSkeleton;
    FData       : IspSkeletonData;
    FTexture    : IWeakRef; //TavAtlasArrayReferenced

    FSprites    : ISpritesSet;
    function  GetPos: TVec2;
    function  GetFlipX: Boolean;
    function  GetFlipY: Boolean;
    procedure SetPos(const Value: TVec2);
    procedure SetFlipX(const Value: Boolean);
    procedure SetFlipY(const Value: Boolean);
  public
    function Handle : PspSkeleton;
    function Data : IspSkeletonData;
    function Texture: TavAtlasArrayReferenced;

    function WriteVertices(const AVert: ISpineVertices; const AZ: Single; const DoUpdateWorldTransform: Boolean = True): Integer;

    property Pos: TVec2 read GetPos write SetPos;
    property FlipX: Boolean read GetFlipX write SetFlipX;
    property FlipY: Boolean read GetFlipY write SetFlipY;

    constructor Create(const AData: IspSkeletonData; const ATexture: TavAtlasArrayReferenced);
    destructor Destroy; override;
  end;

  TAnimationStateData = class(TInterfacedObject, IspAnimationStateData)
  private
    FData : PspAnimationStateData;
    FSkeleton : IspSkeletonData;
    function  GetDefaultMix: Single;
    procedure SetDefaultMix(const Value: Single);
  public
    function Handle: PspAnimationStateData;
    function SkeletonData : IspSkeletonData;

    property DefaultMix: Single read GetDefaultMix write SetDefaultMix;

    constructor Create(const ASkeleton: IspSkeletonData);
    destructor Destroy; override;
  end;

  TAnimationState = class(TInterfacedObject, IspAnimationState)
  private
    FData: IspAnimationStateData;
    FspAnimationState : PspAnimationState;
  public
    function Handle : PspAnimationState;
    function Data : IspAnimationStateData;

    procedure Update(const ADelta: Single);
    procedure Apply(const ASkeleton: IspSkeleton);

    procedure ClearTracks;
    procedure ClearTrack(const AIndex: Integer);

    procedure SetAnimationByName(ATrackIndex: Integer; const AnimationName: string; loop: Boolean);
    //procedure SetAnimationByName(ATrackIndex: Integer; const AnimationName: string; loop: Boolean);

    function  GetTimeScale: Single;
    procedure SetTimeScale(const Value: Single);
    property  TimeScale: Single read GetTimeScale write SetTimeScale;

    constructor Create(const AData: IspAnimationStateData);
    destructor Destroy; override;
  end;

//threadvar GV_BuildBuffer: TVec2Arr;
var GV_BuildBuffer: TVec2Arr;

procedure ClearCache;
begin
  Default_ITextureManager.DropCache;
end;

function Create_IspAtlas(const AFileName: string): IspAtlas;
begin
  Result := TAtlas.Create(AFileName);
end;

function Create_IspSkeletonData(const ASkelFileName: string; const AAtlas: IspAtlas; const AScale: Single = 1): IspSkeletonData;
begin
  Result := TSkeletonData.Create(ASkelFileName, AAtlas, AScale);
end;

function Create_IspSkeleton(const AData: IspSkeletonData; const ATexture: TavAtlasArrayReferenced): IspSkeleton;
begin
  Result := TSkeleton.Create(AData, ATexture);
end;

function Create_IspSkeleton(const AAtlasFileName, ASkelFileName: string; const ATexture: TavAtlasArrayReferenced; const AScale: Single = 1): IspSkeleton; overload;
begin
  Result := Create_IspSkeleton( Create_IspSkeletonData(ASkelFileName, Create_IspAtlas(AAtlasFileName), AScale), ATexture );
end;

function Create_IspAnimationStateData(const ASkeletonData: IspSkeletonData): IspAnimationStateData;
begin
  Result := TAnimationStateData.Create(ASkeletonData);
end;

function Create_IspAnimationState(const AData: IspAnimationStateData): IspAnimationState;
begin
  Result := TAnimationState.Create(AData);
end;

function Create_IspAnimationState(const ASkel: IspSkeleton; const ADefaultMix: Single): IspAnimationState; overload;
var asd: IspAnimationStateData;
begin
  asd := Create_IspAnimationStateData(ASkel.Data);
  asd.DefaultMix := ADefaultMix;
  Result := Create_IspAnimationState(asd);
end;

{ TAtlas }

constructor TAtlas.Create(const AFileName: string);
begin
  FspAtlas := spAtlas_createFromFile(PspPChar(AnsiString(AFileName)), nil);
end;

destructor TAtlas.Destroy;
begin
  spAtlas_dispose(FspAtlas);
  inherited;
end;

function TAtlas.Handle: PspAtlas;
begin
  Result := FspAtlas;
end;

{ TSkeletonData }

constructor TSkeletonData.Create(const ASkelFileName: string; const AAtlas: IspAtlas; const AScale: Single = 1);
var bd: PspSkeletonBinary;
begin
  FAtlas := AAtlas;
  bd := nil;
  try
    bd := spSkeletonBinary_create(FAtlas.Handle);
    bd.scale := AScale;//1.8/686;
    FspSkeletonData := spSkeletonBinary_readSkeletonDataFile(bd, PspPChar(AnsiString(ASkelFileName)));
  finally
    spSkeletonBinary_dispose(bd);
  end;
end;

destructor TSkeletonData.Destroy;
begin
  spSkeletonData_dispose(FspSkeletonData);
  inherited;
end;

function TSkeletonData.Handle: PspSkeletonData;
begin
  Result := FspSkeletonData;
end;

{ TSkeleton }

constructor TSkeleton.Create(const AData: IspSkeletonData; const ATexture: TavAtlasArrayReferenced);
begin
  FData := AData;
  FspSkeleton := spSkeleton_create(FData.Handle);
  FTexture := ATexture.WeakRef;
  FSprites := TSpritesSet.Create;
end;

function TSkeleton.Data: IspSkeletonData;
begin
  Result := FData;
end;

destructor TSkeleton.Destroy;
begin
  spSkeleton_dispose(FspSkeleton);
  inherited;
end;

function TSkeleton.GetFlipX: Boolean;
begin
  Result := FspSkeleton.flip.x <> 0;
end;

function TSkeleton.GetFlipY: Boolean;
begin
  Result := FspSkeleton.flip.y <> 0;
end;

function TSkeleton.GetPos: TVec2;
begin
  Result := FspSkeleton^.pos;
end;

function TSkeleton.Handle: PspSkeleton;
begin
  Result := FspSkeleton;
end;

procedure TSkeleton.SetFlipX(const Value: Boolean);
begin
  if Value then
    FspSkeleton.flip.x := 1
  else
    FspSkeleton.flip.x := 0;
end;

procedure TSkeleton.SetFlipY(const Value: Boolean);
begin
  if Value then
    FspSkeleton.flip.y := 1
  else
    FspSkeleton.flip.y := 0;
end;

procedure TSkeleton.SetPos(const Value: TVec2);
begin
  FspSkeleton^.pos := Value;
end;

function TSkeleton.Texture: TavAtlasArrayReferenced;
begin
  Result := nil;
  if FTexture = nil then Exit;
  if FTexture.Obj = nil then
  begin
    FTexture := nil;
    FSprites.Clear;
    Exit;
  end;
  Result := FTexture.Obj as TavAtlasArrayReferenced;
end;

function TSkeleton.WriteVertices(const AVert: ISpineVertices; const AZ: Single; const DoUpdateWorldTransform: Boolean): Integer;
  type TWordArr = array of Word;
var slot: PPspSlot;
    i, j, n: Integer;
    regionAtt: PspRegionAttachment;
    meshAtt: PspMeshAttachment;

    mip: ITextureMip;
    sprite: ISpriteIndex;

    v: TSpineVertex;

    regionWorldPos: array [0..3] of TVec2;

    tex: TavAtlasArrayReferenced;
begin
  tex := Texture;
  if tex = nil then Exit(0);

  if DoUpdateWorldTransform then
    spSkeleton_updateWorldTransform(FspSkeleton);

  slot := FspSkeleton^.drawOrder;
  for i := 0 to FspSkeleton^.slotsCount - 1 do
  try
    if slot^ = nil then Continue;
    if slot^.attachment = nil then Continue;

    v.vsColor := FspSkeleton^.color * slot^.color;
    v.vsCoord.z := AZ;

    case slot^.attachment^.atype of
      SP_ATTACHMENT_REGION:
      begin
        regionAtt := PspRegionAttachment(slot^.attachment);
        mip := ITextureMip(PspAtlasRegion(regionAtt.userData).page.UserData);
        sprite := tex.ObtainSprite(mip);
        FSprites.Add(sprite);
        v.vsAtlasRef := sprite.Index;

        ZeroClear(regionWorldPos, SizeOf(regionWorldPos));
        spRegionAttachment_computeWorldVertices(regionAtt, slot^.bone, @regionWorldPos[0], 0, 2);

        v.vsCoord.xy := regionWorldPos[0];
        v.vsTexCrd := Vec(regionAtt^.uvs[0], regionAtt^.uvs[1]);
        AVert.Add(v);

        v.vsCoord.xy := regionWorldPos[1];
        v.vsTexCrd := Vec(regionAtt^.uvs[2], regionAtt^.uvs[3]);
        AVert.Add(v);

        v.vsCoord.xy := regionWorldPos[2];
        v.vsTexCrd := Vec(regionAtt^.uvs[4], regionAtt^.uvs[5]);
        AVert.Add(v);

        v.vsCoord.xy := regionWorldPos[0];
        v.vsTexCrd := Vec(regionAtt^.uvs[0], regionAtt^.uvs[1]);
        AVert.Add(v);

        v.vsCoord.xy := regionWorldPos[2];
        v.vsTexCrd := Vec(regionAtt^.uvs[4], regionAtt^.uvs[5]);
        AVert.Add(v);

        v.vsCoord.xy := regionWorldPos[3];
        v.vsTexCrd := Vec(regionAtt^.uvs[6], regionAtt^.uvs[7]);
        AVert.Add(v);
      end;

      SP_ATTACHMENT_MESH:
      begin
        meshAtt := PspMeshAttachment(slot^.attachment);
        if meshAtt^.super.worldVerticesLength = 0 then Continue;

        mip := ITextureMip(PspAtlasRegion(meshAtt.userData).page.UserData);
        sprite := tex.ObtainSprite(mip);
        FSprites.Add(sprite);
        v.vsAtlasRef := sprite.Index;

        if Length(GV_BuildBuffer) < meshAtt^.super.worldVerticesLength then
          SetLength(GV_BuildBuffer, meshAtt^.super.worldVerticesLength*2);

        spVertexAttachment_computeWorldVertices(@meshAtt^.super, slot^, 0, meshAtt^.super.worldVerticesLength, @GV_BuildBuffer[0], 0, 2);

        for j := 0 to meshAtt^.trianglesCount-1 do
        begin
          n := TWordArr(meshAtt^.triangles)[j];
          v.vsCoord.xy := GV_BuildBuffer[n];
          v.vsTexCrd := TVec2Arr(meshAtt^.uvs)[n];
          AVert.Add(v);
        end;
      end;

    end;
  finally
    Inc(slot);
  end;
end;

procedure avCreateTexturePage(const APath: PspPChar; var Width, Height: Integer; var UserData: Pointer);
var texData: ITextureData;
begin
  texData := Default_ITextureManager.LoadTexture(string(APath));
  Width := texData.Width;
  Height := texData.Height;
  ITextureMip(UserData) := texData.MipData(0,0);
end;

procedure avDestroyTexturePage(var UserData: Pointer);
begin
  ITextureMip(UserData) := nil;
end;

procedure Init;
begin
  SetTextureAllocator(avCreateTexturePage, avDestroyTexturePage);
end;

{ TSpineVertex }

class function TSpineVertex.Layout: IDataLayout;
begin
  Result := LB.Add('vsCoord', ctFloat, 3)
              .Add('vsTexCrd', ctFloat, 2)
              .Add('vsColor', ctFloat, 4)
              .Add('vsAtlasRef', ctFloat, 1)
              .Finish();
end;

{ TAnimationState }

procedure TAnimationState.Apply(const ASkeleton: IspSkeleton);
begin
  spAnimationState_apply(FspAnimationState, ASkeleton.Handle);
end;

procedure TAnimationState.ClearTrack(const AIndex: Integer);
begin
  spAnimationState_clearTrack(FspAnimationState, AIndex);
end;

procedure TAnimationState.ClearTracks;
begin
  spAnimationState_clearTracks(FspAnimationState);
end;

constructor TAnimationState.Create(const AData: IspAnimationStateData);
begin
  FData := AData;
  FspAnimationState := spAnimationState_create(FData.Handle);
end;

function TAnimationState.Data: IspAnimationStateData;
begin
  Result := FData;
end;

destructor TAnimationState.Destroy;
begin
  spAnimationState_dispose(FspAnimationState);
  inherited;
end;

function TAnimationState.GetTimeScale: Single;
begin
  Result := FspAnimationState.timeScale;
end;

function TAnimationState.Handle: PspAnimationState;
begin
  Result := FspAnimationState;
end;

procedure TAnimationState.SetAnimationByName(ATrackIndex: Integer; const AnimationName: string; loop: Boolean);
var n: Integer;
begin
  if loop then n := 1 else n := 0;
  spAnimationState_setAnimationByName(FspAnimationState, ATrackIndex, PspPChar(AnsiString(AnimationName)), n);
end;

procedure TAnimationState.SetTimeScale(const Value: Single);
begin
  FspAnimationState.timeScale := Value;
end;

procedure TAnimationState.Update(const ADelta: Single);
begin
  spAnimationState_update(FspAnimationState, ADelta);
end;

{ TAnimationStateData }

constructor TAnimationStateData.Create(const ASkeleton: IspSkeletonData);
begin
  FSkeleton := ASkeleton;
  FData := spAnimationStateData_create(FSkeleton.Handle);
end;

destructor TAnimationStateData.Destroy;
begin
  spAnimationStateData_dispose(FData);
  inherited;
end;

function TAnimationStateData.GetDefaultMix: Single;
begin
  Result := FData^.defaultMix;
end;

function TAnimationStateData.Handle: PspAnimationStateData;
begin
  Result := FData;
end;

procedure TAnimationStateData.SetDefaultMix(const Value: Single);
begin
  FData^.defaultMix := Value;
end;

function TAnimationStateData.SkeletonData: IspSkeletonData;
begin
  Result := FSkeleton;
end;

initialization

Init;

finalization

end.
