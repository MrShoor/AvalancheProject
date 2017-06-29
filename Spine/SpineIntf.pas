unit SpineIntf;

interface

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
    function Texture: TavAtlasArrayReferenced;

    function WriteVertices(const AVert: ISpineVertices; const DoUpdateWorldTransform: Boolean = True): Integer;
  end;

function Create_IspAtlas(const AFileName: string): IspAtlas;
function Create_IspSkeletonData(const ASkelFileName: string; const AAtlas: IspAtlas): IspSkeletonData;

function Create_IspSkeleton(const AData: IspSkeletonData; const ATexture: TavAtlasArrayReferenced): IspSkeleton; overload;
function Create_IspSkeleton(const AAtlasFileName, ASkelFileName: string; const ATexture: TavAtlasArrayReferenced): IspSkeleton; overload;

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
    constructor Create(const ASkelFileName: string; const AAtlas: IspAtlas);
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

    FSprites   : ISpritesSet;
  public
    function Handle : PspSkeleton;
    function Texture: TavAtlasArrayReferenced;

    function WriteVertices(const AVert: ISpineVertices; const DoUpdateWorldTransform: Boolean = True): Integer;

    constructor Create(const AData: IspSkeletonData; const ATexture: TavAtlasArrayReferenced);
    destructor Destroy; override;
  end;

var GV_TexMan: ITextureManager;
//threadvar GV_BuildBuffer: TVec2Arr;
var GV_BuildBuffer: TVec2Arr;

procedure ClearCache;
begin
  GV_TexMan.DropCache;
end;

function Create_IspAtlas(const AFileName: string): IspAtlas;
begin
  Result := TAtlas.Create(AFileName);
end;

function Create_IspSkeletonData(const ASkelFileName: string; const AAtlas: IspAtlas): IspSkeletonData;
begin
  Result := TSkeletonData.Create(ASkelFileName, AAtlas);
end;

function Create_IspSkeleton(const AData: IspSkeletonData; const ATexture: TavAtlasArrayReferenced): IspSkeleton;
begin
  Result := TSkeleton.Create(AData, ATexture);
end;

function Create_IspSkeleton(const AAtlasFileName, ASkelFileName: string; const ATexture: TavAtlasArrayReferenced): IspSkeleton; overload;
begin
  Result := Create_IspSkeleton( Create_IspSkeletonData(ASkelFileName, Create_IspAtlas(AAtlasFileName)), ATexture );
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

constructor TSkeletonData.Create(const ASkelFileName: string; const AAtlas: IspAtlas);
var bd: PspSkeletonBinary;
begin
  FAtlas := AAtlas;
  bd := nil;
  try
    bd := spSkeletonBinary_create(FAtlas.Handle);
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

destructor TSkeleton.Destroy;
begin
  spSkeleton_dispose(FspSkeleton);
  inherited;
end;

function TSkeleton.Handle: PspSkeleton;
begin
  Result := FspSkeleton;
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

function TSkeleton.WriteVertices(const AVert: ISpineVertices; const DoUpdateWorldTransform: Boolean): Integer;
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
    v.vsCoord.z := 0;

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
  texData := GV_TexMan.LoadTexture(string(APath));
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
  GV_TexMan := Create_ITextureManager;
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

initialization

Init;

finalization

end.
