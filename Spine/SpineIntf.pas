unit SpineIntf;

interface

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

//some help: http://ru.esotericsoftware.com/spine-c

uses
  SysUtils,
  SpineH,
  intfUtils,
  mutils,
  avRes,
  avContnrs,
  avContnrsDefaults,
  avTess,
  avTypes;

type
  PspAnimation = SpineH.PspAnimation;
  PspTrackEntry = SpineH.PspTrackEntry;
  TspEventType = SpineH.TspEventType;

  ESpineError = class (Exception);

  TSpineVertex = packed record
    vsCoord    : TVec3;
    vsTexCrd   : TVec2;
    vsColor    : TVec4;
    vsAtlasRef : IUnknown;
  end;

  ISpineAddVertexCallback = interface
    procedure AddVertex(const AVert: TSpineVertex);
  end;

  IspAtlas = interface
    function Handle: PspAtlas;
  end;

  IspAtlasPageCustomLoader = interface
    procedure LoadPage(const APath: PspPChar; var AWidth, AHeight: Integer; var APage: IUnknown);
  end;

  IspSkeletonData = interface
    function Handle: PspSkeletonData;
  end;

  IspTextureToRefIdx = interface
    function GetPageRef(const UserDataAltasPage: Pointer): IUnknown;
  end;

  TOnWriteVertices = procedure (const ASlot: PspSlot; const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single) of object;

  IspSkeletonSlotRenderSubscriber = interface
  ['{B262DA7D-8790-47B0-BA80-1A47C798710E}']
    procedure OnWriteVertices(const ASlot: PspSlot; const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single);
  end;

  { IspSkeleton }

  IspSkeleton = interface
    function Handle : PspSkeleton;
    function Data : IspSkeletonData;
    function Texture: IspTextureToRefIdx;

    function  SubscribeOnSlotRender(const ASlotName: string; const AOnWriteVertices: TOnWriteVertices): IspSkeletonSlotRenderSubscriber;
    procedure SubscribeOnSlotRender(const ASlotName: string; const ASubscriber: IspSkeletonSlotRenderSubscriber);
    function WriteVertices(const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single; const DoUpdateWorldTransform: Boolean = True): Integer;
    procedure UpdateWorldTransform;

    function FindBone(const ABoneName: string): PspBone;
    function SetSkinByName(const ASkinName: string): Integer;

    //function FindSlotIndex(const ASlotName): Integer;

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

  IspAnimationState = interface;

  TspOnUserEvent = procedure(const AAnimState: IspAnimationState; const AName: string; AState: PspAnimation; AEntry: PspTrackEntry) of object;
  TspOnAnimEvent = procedure(const AAnimState: IspAnimationState; event_type: TspEventType; AState: PspAnimation; AEntry: PspTrackEntry) of object;

  IspEventSubscriber = interface
  ['{3E8EC925-80BF-4504-8A25-694E8AB5BF0F}']
    procedure Event_OnAnimEvent(const AAnimState: IspAnimationState; state: PspAnimation; entry: PspTrackEntry; event_type: TspEventType);
    procedure Event_OnUserEvent(const AAnimState: IspAnimationState; const AName: string; AState: PspAnimation; AEntry: PspTrackEntry);
  end;

  IspAnimationState = interface
    function Handle : PspAnimationState;
    function Data : IspAnimationStateData;

    procedure Update(const ADelta: Single);
    procedure Apply(const ASkeleton: IspSkeleton);

    procedure ClearTracks;
    procedure ClearTrack(const AIndex: Integer);

    procedure SetAnimationByName(ATrackIndex: Integer; const AnimationName: string; loop: Boolean);

    function  Subscribe_UserEvent(const AEvent: TspOnUserEvent; const AAnimEvent: TspOnAnimEvent; const AAnimation: string = ''; const AUserEventName: string = ''): IspEventSubscriber;
    procedure Subscribe_UserEventIntf(const AEvent: IspEventSubscriber);

    function  GetTimeScale: Single;
    procedure SetTimeScale(const Value: Single);
    property  TimeScale: Single read GetTimeScale write SetTimeScale;
  end;

  IspSkeletonCache = interface
    function ObtainSkeleton(const AAtlasFileName, ASkelFileName: string; const ATexture: IspTextureToRefIdx; const AScale: Single = 1): IspSkeleton;
    procedure ClearCache(const ASkeletons: Boolean = True; const AAtlases: Boolean = True);
  end;

function Create_IspAtlas(const AFileName: string; const ACustomLoader: IspAtlasPageCustomLoader = nil): IspAtlas;
function Create_IspSkeletonData(const ASkelFileName: string; const AAtlas: IspAtlas; const AScale: Single = 1): IspSkeletonData;

function Create_IspSkeleton(const AData: IspSkeletonData; const ATexture: IspTextureToRefIdx): IspSkeleton; overload;
function Create_IspSkeleton(const AAtlasFileName, ASkelFileName: string; const ATexture: IspTextureToRefIdx; const AScale: Single = 1): IspSkeleton; overload;

function Create_IspAnimationStateData(const ASkeletonData: IspSkeletonData): IspAnimationStateData;
function Create_IspAnimationState(const AData: IspAnimationStateData): IspAnimationState; overload;
function Create_IspAnimationState(const ASkel: IspSkeleton; const ADefaultMix: Single): IspAnimationState; overload;

function SkeletonCache: IspSkeletonCache;
procedure ClearCache;

function EvalAbsBoneTransform(const ABone: PspBone): TMat3;

function WriteAttachementVertices(const ASkeleton: IspSkeleton; const ASlot: PspSlot; const AAttachment: PspAttachment; const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single): Integer;

implementation

uses
  avTexLoader;

type
  TAtlas = class(TInterfacedObject, IspAtlas)
  private
    FspAtlas : PspAtlas;
  public
    function Handle: PspAtlas;
    constructor Create(const AFileName: string; const ACustomLoader: IspAtlasPageCustomLoader);
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

  { TSkeletonSlotRenderSubscriber }

  TSkeletonSlotRenderSubscriber = class(TWeakedInterfacedObject, IspSkeletonSlotRenderSubscriber)
  private
    FOnWriteVertices: TOnWriteVertices;
    procedure OnWriteVertices(const ASlot: PspSlot; const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single);
  public
    constructor Create(const AOnWriteVertices: TOnWriteVertices);
  end;

  { TSkeletonSlotRenderPublisher }

  TSkeletonSlotRenderPublisher = class(TPublisherBase, IspSkeletonSlotRenderSubscriber)
    procedure OnWriteVertices(const ASlot: PspSlot; const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single);
  end;

  { TSkeleton }

  TSkeleton = class(TInterfacedObject, IspSkeleton)
  private type
    TSlotPublisher = packed record
      slot: PspSlot;
      publisher: IspSkeletonSlotRenderSubscriber;
    end;
    PSlotPublisher = ^TSlotPublisher;
  private
    FspSkeleton : PspSkeleton;
    FData       : IspSkeletonData;
    FTexture    : IspTextureToRefIdx;

    FSubscribers: array of TSlotPublisher;
    function  GetPos: TVec2;
    function  GetFlipX: Boolean;
    function  GetFlipY: Boolean;
    procedure SetPos(const Value: TVec2);
    procedure SetFlipX(const Value: Boolean);
    procedure SetFlipY(const Value: Boolean);

    function FindSubscribers(const ASlot: PspSlot): PSlotPublisher;
  public
    function Handle : PspSkeleton;
    function Data : IspSkeletonData;
    function Texture: IspTextureToRefIdx;

    function  SubscribeOnSlotRender(const ASlotName: string; const AOnWriteVertices: TOnWriteVertices): IspSkeletonSlotRenderSubscriber;
    procedure SubscribeOnSlotRender(const ASlotName: string; const ASubscriber: IspSkeletonSlotRenderSubscriber);
    function  WriteVertices(const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single; const DoUpdateWorldTransform: Boolean = True): Integer;
    procedure UpdateWorldTransform;

    function FindBone(const ABoneName: string): PspBone;
    function SetSkinByName(const ASkinName: string): Integer;

    property Pos: TVec2 read GetPos write SetPos;
    property FlipX: Boolean read GetFlipX write SetFlipX;
    property FlipY: Boolean read GetFlipY write SetFlipY;

    constructor Create(const AData: IspSkeletonData; const ATexture: IspTextureToRefIdx);
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

  TspUserEventPublisher = class(TPublisherBase, IspEventSubscriber)
  private
    procedure Event_OnAnimEvent(const AAnimState: IspAnimationState; state: PspAnimation; entry: PspTrackEntry; event_type: TspEventType);
    procedure Event_OnUserEvent(const AAnimState: IspAnimationState; const AName: string; state: PspAnimation; entry: PspTrackEntry);
  end;

  TspUserEventSubscriber = class(TWeakedInterfacedObject, IspEventSubscriber)
  private
    FAnimListener: TspOnAnimEvent;
    FUserListener: TspOnUserEvent;
    FAnimFilter: AnsiString;
    FEventFilter: string;
    procedure Event_OnAnimEvent(const AAnimState: IspAnimationState; state: PspAnimation; entry: PspTrackEntry; event_type: TspEventType);
    procedure Event_OnUserEvent(const AAnimState: IspAnimationState; const AName: string; state: PspAnimation; entry: PspTrackEntry);
  public
    constructor Create(const AUserListener: TspOnUserEvent; const AAnimListener: TspOnAnimEvent; const AAnimFilter: string; const AEventFilter: string);
  end;

  TAnimationState = class(TInterfacedObject, IspAnimationState)
  private
    FData: IspAnimationStateData;
    FspAnimationState : PspAnimationState;

    FUserEventPublisher : IspEventSubscriber;

    procedure DoNotifyAnimEvent(state: PspAnimation; entry: PspTrackEntry; event_type: TspEventType); inline;
    procedure DoNotifyUserEvent(state: PspAnimation; entry: PspTrackEntry; const EventName: string); inline;
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

    function  Subscribe_UserEvent(const AUserEvent: TspOnUserEvent; const AAnimEvent: TspOnAnimEvent; const AAnimation: string = ''; const AUserEventName: string = ''): IspEventSubscriber;
    procedure Subscribe_UserEventIntf(const AEvent: IspEventSubscriber);

    procedure CallBackEvent(state: PspAnimation; event_type: TspEventType; entry: PspTrackEntry; event: PspEvent);

    constructor Create(const AData: IspAnimationStateData);
    destructor Destroy; override;
  end;

  TspSkeletonCache = class(TInterfacedObject, IspSkeletonCache)
  private type
    TSkelDataKey = packed record
      Skel : string;
      Scale: single;
    end;
    TKeyEQComparer = class(TInterfacedObject, IEqualityComparer)
    private
      function Hash(const Value): Cardinal;
      function IsEqual(const Left, Right): Boolean;
    end;
    TSkelSet = {$IfDef FPC}specialize{$EndIf} THashMap<TSkelDataKey, IspSkeletonData>;
    ISkelSet = {$IfDef FPC}specialize{$EndIf} IHashMap<TSkelDataKey, IspSkeletonData>;
    TAtlasSet = {$IfDef FPC}specialize{$EndIf} THashMap<string, IspAtlas>;
    IAtlasSet = {$IfDef FPC}specialize{$EndIf} IHashMap<string, IspAtlas>;
  private
    FAtlases  : IAtlasSet;
    FSkeletons: ISkelSet;
    function ObtainAtlas(const AAtlasFileName: string): IspAtlas;
  public
    function ObtainSkeleton(const AAtlasFileName, ASkelFileName: string; const ATexture: IspTextureToRefIdx; const AScale: Single = 1): IspSkeleton;
    procedure ClearCache(const ASkeletons: Boolean = True; const AAtlases: Boolean = True);
    procedure AfterConstruction; override;
  end;

//threadvar GV_BuildBuffer: TVec2Arr;
var GV_BuildBuffer: TVec2Arr;
    GV_SkeletonCache: IspSkeletonCache;

procedure AnimationCallBacks(state: PspAnimationState; event_type: TspEventType; entry: PspTrackEntry; event: PspEvent); //sometime state comes corrupted?
begin
  if entry^.userData = nil then Exit;
  TAnimationState(entry^.userData).CallBackEvent(entry^.animation, event_type, entry, event);
end;

function SkeletonCache: IspSkeletonCache;
begin
  if GV_SkeletonCache = nil then
    GV_SkeletonCache := TspSkeletonCache.Create;
  Result := GV_SkeletonCache;
end;

procedure ClearCache;
begin
  if GV_SkeletonCache <> nil then
    GV_SkeletonCache.ClearCache();
  Default_ITextureManager.DropCache;
end;

function EvalAbsBoneTransform(const ABone: PspBone): TMat3;
begin
end;

function WriteAttachementVertices(const ASkeleton: IspSkeleton; const ASlot: PspSlot; const AAttachment: PspAttachment; const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single): Integer;
  type TWordArr = array of Word;
var i, j, n: Integer;
    regionAtt: PspRegionAttachment;
    meshAtt: PspMeshAttachment;

    mip: ITextureMip;
    sprite: ISpriteIndex;

    v: TSpineVertex;

    regionWorldPos: array [0..3] of TVec2;

    tex: IspTextureToRefIdx;
begin
  Result := 0;

  tex := ASkeleton.Texture;

  v.vsColor := ASkeleton.Handle^.color * ASlot^.color;
  v.vsCoord.z := AZ;

  case AAttachment^.atype of
    SP_ATTACHMENT_REGION:
    begin
      regionAtt := PspRegionAttachment(AAttachment);

      v.vsAtlasRef := tex.GetPageRef(PspAtlasRegion(regionAtt^.userData)^.page^.UserData);

      ZeroClear(regionWorldPos, SizeOf(regionWorldPos));
      spRegionAttachment_computeWorldVertices(regionAtt, ASlot^.bone, @regionWorldPos[0], 0, 2);

      v.vsCoord.xy := regionWorldPos[0];
      v.vsTexCrd := Vec(regionAtt^.uvs[0], regionAtt^.uvs[1]);
      AVertAddCallback.AddVertex(v);

      v.vsCoord.xy := regionWorldPos[1];
      v.vsTexCrd := Vec(regionAtt^.uvs[2], regionAtt^.uvs[3]);
      AVertAddCallback.AddVertex(v);

      v.vsCoord.xy := regionWorldPos[2];
      v.vsTexCrd := Vec(regionAtt^.uvs[4], regionAtt^.uvs[5]);
      AVertAddCallback.AddVertex(v);

      v.vsCoord.xy := regionWorldPos[0];
      v.vsTexCrd := Vec(regionAtt^.uvs[0], regionAtt^.uvs[1]);
      AVertAddCallback.AddVertex(v);

      v.vsCoord.xy := regionWorldPos[2];
      v.vsTexCrd := Vec(regionAtt^.uvs[4], regionAtt^.uvs[5]);
      AVertAddCallback.AddVertex(v);

      v.vsCoord.xy := regionWorldPos[3];
      v.vsTexCrd := Vec(regionAtt^.uvs[6], regionAtt^.uvs[7]);
      AVertAddCallback.AddVertex(v);

      Inc(Result, 6);
    end;

    SP_ATTACHMENT_MESH:
    begin
      meshAtt := PspMeshAttachment(ASlot^.attachment);
      if meshAtt^.super.worldVerticesLength = 0 then Exit;

      v.vsAtlasRef := tex.GetPageRef(PspAtlasRegion(meshAtt^.userData)^.page^.UserData);

      if Length(GV_BuildBuffer) < meshAtt^.super.worldVerticesLength then
        SetLength(GV_BuildBuffer, meshAtt^.super.worldVerticesLength*2);

      spVertexAttachment_computeWorldVertices(@meshAtt^.super, ASlot, 0, meshAtt^.super.worldVerticesLength, @GV_BuildBuffer[0], 0, 2);

      for j := 0 to meshAtt^.trianglesCount-1 do
      begin
        n := TWordArr(meshAtt^.triangles)[j];
        v.vsCoord.xy := GV_BuildBuffer[n];
        v.vsTexCrd := TVec2Arr(meshAtt^.uvs)[n];
        AVertAddCallback.AddVertex(v);
      end;
      Inc(Result, meshAtt^.trianglesCount);
    end;

  end;
end;

function Create_IspAtlas(const AFileName: string; const ACustomLoader: IspAtlasPageCustomLoader): IspAtlas;
begin
  Result := TAtlas.Create(AFileName, ACustomLoader);
end;

function Create_IspSkeletonData(const ASkelFileName: string; const AAtlas: IspAtlas; const AScale: Single = 1): IspSkeletonData;
begin
  Result := TSkeletonData.Create(ASkelFileName, AAtlas, AScale);
end;

function Create_IspSkeleton(const AData: IspSkeletonData; const ATexture: IspTextureToRefIdx): IspSkeleton;
begin
  Result := TSkeleton.Create(AData, ATexture);
end;

function Create_IspSkeleton(const AAtlasFileName, ASkelFileName: string; const ATexture: IspTextureToRefIdx; const AScale: Single = 1): IspSkeleton; overload;
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

{ TSkeletonSlotRenderSubscriber }

procedure TSkeletonSlotRenderSubscriber.OnWriteVertices(const ASlot: PspSlot; const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single);
begin
  FOnWriteVertices(ASlot, AVertAddCallback, AZ);
end;

constructor TSkeletonSlotRenderSubscriber.Create(const AOnWriteVertices: TOnWriteVertices);
begin
  Assert(Assigned(AOnWriteVertices));
  FOnWriteVertices := AOnWriteVertices;
end;

{ TSkeletonSlotRenderPublisher }

procedure TSkeletonSlotRenderPublisher.OnWriteVertices(const ASlot: PspSlot; const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single);
var subs: TSubsList;
    i: Integer;
begin
  subs := GetSubsList;
  for i := 0 to Length(subs) - 1 do
    (subs[i] as IspSkeletonSlotRenderSubscriber).OnWriteVertices(ASlot, AVertAddCallback, AZ);
end;

{ TAtlas }

constructor TAtlas.Create(const AFileName: string; const ACustomLoader: IspAtlasPageCustomLoader);
begin
  FspAtlas := spAtlas_createFromFile(PspPChar(AnsiString(AFileName)), ACustomLoader);
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
    bd^.scale := AScale;//1.8/686;
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

constructor TSkeleton.Create(const AData: IspSkeletonData; const ATexture: IspTextureToRefIdx);
begin
  FData := AData;
  FspSkeleton := spSkeleton_create(FData.Handle);
  FTexture := ATexture;
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

function TSkeleton.FindBone(const ABoneName: string): PspBone;
var bn: AnsiString;
begin
  bn := AnsiString(ABoneName);
  Result := spSkeleton_findBone(FspSkeleton, PAnsiChar(bn));
end;

function TSkeleton.SetSkinByName(const ASkinName: string): Integer;
var sn: AnsiString;
begin
  sn := AnsiString(ASkinName);
  Result := spSkeleton_setSkinByName(FspSkeleton, PAnsiChar(sn));
end;

function TSkeleton.GetFlipX: Boolean;
begin
  Result := FspSkeleton^.flip.x <> 0;
end;

function TSkeleton.GetFlipY: Boolean;
begin
  Result := FspSkeleton^.flip.y <> 0;
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
    FspSkeleton^.flip.x := 1
  else
    FspSkeleton^.flip.x := 0;
end;

procedure TSkeleton.SetFlipY(const Value: Boolean);
begin
  if Value then
    FspSkeleton^.flip.y := 1
  else
    FspSkeleton^.flip.y := 0;
end;

function TSkeleton.FindSubscribers(const ASlot: PspSlot): PSlotPublisher;
var i: Integer;
begin
  for i := 0 to Length(FSubscribers) - 1 do
    if FSubscribers[i].slot = ASlot then
      Exit(@FSubscribers[i]);
  Result := nil;
end;

procedure TSkeleton.SetPos(const Value: TVec2);
begin
  FspSkeleton^.pos := Value;
end;

function TSkeleton.Texture: IspTextureToRefIdx;
begin
  Result := FTexture;
end;

function TSkeleton.SubscribeOnSlotRender(const ASlotName: string; const AOnWriteVertices: TOnWriteVertices): IspSkeletonSlotRenderSubscriber;
begin
  Result := TSkeletonSlotRenderSubscriber.Create(AOnWriteVertices);
  SubscribeOnSlotRender(ASlotName, Result);
end;

procedure TSkeleton.SubscribeOnSlotRender(const ASlotName: string; const ASubscriber: IspSkeletonSlotRenderSubscriber);
var psubs: PSlotPublisher;
    subs : TSlotPublisher;
    slot: PspSlot;
begin
  slot := spSkeleton_findSlot(FspSkeleton, PspPChar(ASlotName));
  Assert(slot <> nil, 'Slot "' + ASlotName + '" not found');
  psubs := FindSubscribers(slot);
  if psubs = nil then
  begin
    subs.slot := slot;
    subs.publisher := TSkeletonSlotRenderPublisher.Create;
    psubs := @subs;
    SetLength(FSubscribers, Length(FSubscribers) + 1);
    FSubscribers[Length(FSubscribers) - 1] := subs;
  end;
  (psubs^.publisher as IPublisher).Subscribe(ASubscriber as IWeakedInterface);
end;

procedure TSkeleton.UpdateWorldTransform;
begin
  spSkeleton_updateWorldTransform(FspSkeleton);
end;

function TSkeleton.WriteVertices(const AVertAddCallback: ISpineAddVertexCallback; const AZ: Single; const DoUpdateWorldTransform: Boolean): Integer;
  type TWordArr = array of Word;
var slot: PPspSlot;
    i, j, n: Integer;
    regionAtt: PspRegionAttachment;
    meshAtt: PspMeshAttachment;

    mip: ITextureMip;
    sprite: ISpriteIndex;
    subs: PSlotPublisher;

    v: TSpineVertex;

    regionWorldPos: array [0..3] of TVec2;
begin
  Result := 0;

  if DoUpdateWorldTransform then
    spSkeleton_updateWorldTransform(FspSkeleton);

  slot := FspSkeleton^.drawOrder;
  for i := 0 to FspSkeleton^.slotsCount - 1 do
  try
    if slot^ = nil then Continue;
    subs := FindSubscribers(slot^);
    if (subs <> nil) and (subs^.publisher <> nil) then
      subs^.publisher.OnWriteVertices(slot^, AVertAddCallback, AZ);
    if slot^^.attachment = nil then Continue;

    Inc(Result, WriteAttachementVertices(Self, slot^, slot^^.attachment, AVertAddCallback, AZ));
  finally
    Inc(slot);
  end;
end;

procedure avCreateTexturePage(const APage: PspAtlasPage; const APath: PspPChar; var Width, Height: Integer; var UserData: Pointer);
var texData: ITextureData;
begin
  if APage^.atlas^.UserData = nil then
  begin
    texData := Default_ITextureManager.LoadTexture(string(APath));
    Width := texData.Width;
    Height := texData.Height;
    ITextureMip(UserData) := texData.MipData(0,0);
  end
  else
  begin
    IspAtlasPageCustomLoader(APage^.atlas^.UserData).LoadPage(APath, Width, Height, IUnknown(UserData));
  end;
end;

procedure avDestroyTexturePage(var UserData: Pointer);
begin
  IUnknown(UserData) := nil;
end;

procedure Init;
begin
  SetTextureAllocator({$IfDef FPC}@{$EndIf}avCreateTexturePage, {$IfDef FPC}@{$EndIf}avDestroyTexturePage);
end;

{ TAnimationState }

procedure TAnimationState.Apply(const ASkeleton: IspSkeleton);
begin
  spAnimationState_apply(FspAnimationState, ASkeleton.Handle);
end;

procedure TAnimationState.CallBackEvent(state: PspAnimation; event_type: TspEventType; entry: PspTrackEntry; event: PspEvent);
begin
  case event_type of
    SP_ANIMATION_START: DoNotifyAnimEvent(state, entry, event_type);
    SP_ANIMATION_INTERRUPT: DoNotifyAnimEvent(state, entry, event_type);
    SP_ANIMATION_END: DoNotifyAnimEvent(state, entry, event_type);
    SP_ANIMATION_COMPLETE: DoNotifyAnimEvent(state, entry, event_type);
    SP_ANIMATION_DISPOSE: DoNotifyAnimEvent(state, entry, event_type);
    SP_ANIMATION_EVENT: DoNotifyUserEvent(state, entry, event^.data^.name);
  end;
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
  FspAnimationState^.UserData := Self;
  FspAnimationState^.listener := @AnimationCallBacks;

  FUserEventPublisher := TspUserEventPublisher.Create;
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

procedure TAnimationState.DoNotifyAnimEvent(state: PspAnimation; entry: PspTrackEntry; event_type: TspEventType);
begin
//  WriteLn(AnsiString(state^.name));
  FUserEventPublisher.Event_OnAnimEvent(Self, state, entry, event_type);
end;

procedure TAnimationState.DoNotifyUserEvent(state: PspAnimation; entry: PspTrackEntry; const EventName: string);
begin
  FUserEventPublisher.Event_OnUserEvent(Self, EventName, state, entry);
end;

function TAnimationState.GetTimeScale: Single;
begin
  Result := FspAnimationState^.timeScale;
end;

function TAnimationState.Handle: PspAnimationState;
begin
  Result := FspAnimationState;
end;

procedure TAnimationState.SetAnimationByName(ATrackIndex: Integer; const AnimationName: string; loop: Boolean);
  function TestAnimationExists(): Boolean;
  var i: Integer;
      skel: PspSkeletonData;
      anim: PspAnimation;
  begin
    skel := FspAnimationState^.data^.skeletonData;
    anim := skel^.animations^;
    for i := 0 to  skel^.animationsCount - 1 do
    begin
      if AnsiString(anim^.name) = AnimationName then Exit(true);
      Inc(anim);
    end;
    Result := false;
  end;
var n: Integer;
    track: PPspTrackEntry;
begin
  //Assert(TestAnimationExists());
  if loop then n := 1 else n := 0;
  spAnimationState_setAnimationByName(FspAnimationState, ATrackIndex, PspPChar(AnsiString(AnimationName)), n);

  track := FspAnimationState^.tracks;
  Inc(track, ATrackIndex);
  track^^.userData := Self;
end;

procedure TAnimationState.SetTimeScale(const Value: Single);
begin
  FspAnimationState^.timeScale := Value;
end;

function TAnimationState.Subscribe_UserEvent(const AUserEvent: TspOnUserEvent; const AAnimEvent: TspOnAnimEvent; const AAnimation: string; const AUserEventName: string): IspEventSubscriber;
begin
  Result := TspUserEventSubscriber.Create(AUserEvent, AAnimEvent, AAnimation, AUserEventName);
  Subscribe_UserEventIntf(Result);
end;

procedure TAnimationState.Subscribe_UserEventIntf(const AEvent: IspEventSubscriber);
begin
  (FUserEventPublisher as IPublisher).Subscribe(AEvent as IWeakedInterface);
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

{ TspSkeletonCache }

procedure TspSkeletonCache.AfterConstruction;
var eqc: IEqualityComparer;
begin
  inherited;
  eqc := TKeyEQComparer.Create;
  FSkeletons := TSkelSet.Create(eqc);
  FAtlases := TAtlasSet.Create;
end;

procedure TspSkeletonCache.ClearCache(const ASkeletons, AAtlases: Boolean);
begin
  if AAtlases then FAtlases.Clear;
  if ASkeletons then FSkeletons.Clear;
end;

function TspSkeletonCache.ObtainAtlas(const AAtlasFileName: string): IspAtlas;
begin
  if not FAtlases.TryGetValue(AAtlasFileName, Result) then
  begin
    if not FileExists(AAtlasFileName) then
      raise ESpineError.Create('File: "' + AAtlasFileName + '" not found."');
    Result := Create_IspAtlas(AAtlasFileName);
    FAtlases.AddOrSet(AAtlasFileName, Result);
  end;
end;

function TspSkeletonCache.ObtainSkeleton(const AAtlasFileName,
  ASkelFileName: string; const ATexture: IspTextureToRefIdx;
  const AScale: Single): IspSkeleton;
var key: TSkelDataKey;
    skelData: IspSkeletonData;
begin
  key.Skel := ASkelFileName;
  key.Scale := AScale;
  if not FSkeletons.TryGetValue(key, skelData) then
  begin
    if not FileExists(key.Skel) then
      raise ESpineError.Create('File: "' + key.Skel + '" not found."');
    skelData := Create_IspSkeletonData(key.Skel, ObtainAtlas(AAtlasFileName), key.Scale);
    FSkeletons.AddOrSet(key, skelData);
  end;
  Result := Create_IspSkeleton(skelData, ATexture);
end;

{ TspSkeletonCache.TKeyEQComparer }

function TspSkeletonCache.TKeyEQComparer.Hash(const Value): Cardinal;
var K: TSkelDataKey absolute Value;
begin
  Result := 0;
  if Length(K.Skel) > 0 then
    Result := Result xor Murmur2DefSeed(K.Skel[1], Length(K.Skel)*SizeOf(Char));
  Result := Result xor Murmur2DefSeed(K.Scale, SizeOf(K.Scale));
end;

function TspSkeletonCache.TKeyEQComparer.IsEqual(const Left, Right): Boolean;
var L: TSkelDataKey absolute Left;
    R: TSkelDataKey absolute Right;
begin
  Result := (L.Skel = R.Skel) and (L.Scale = R.Scale);
end;

{ TspUserEventSubscriber }

constructor TspUserEventSubscriber.Create(const AUserListener: TspOnUserEvent; const AAnimListener: TspOnAnimEvent; const AAnimFilter: string; const AEventFilter: string);
begin
  FUserListener := AUserListener;
  FAnimListener := AAnimListener;
  FAnimFilter := AnsiString(AAnimFilter);
  FEventFilter := AEventFilter;
end;

procedure TspUserEventSubscriber.Event_OnAnimEvent(const AAnimState: IspAnimationState; state: PspAnimation; entry: PspTrackEntry; event_type: TspEventType);
begin
  if not Assigned(FAnimListener) then Exit;
  if FAnimFilter <> '' then
    if FAnimFilter <> state^.name then
      Exit;
  FAnimListener(AAnimState, event_type, state, entry);
end;

procedure TspUserEventSubscriber.Event_OnUserEvent(const AAnimState: IspAnimationState; const AName: string; state: PspAnimation; entry: PspTrackEntry);
begin
  if not Assigned(FUserListener) then Exit;
  if FAnimFilter <> '' then
    if FAnimFilter <> state^.name then
      Exit;
  if FEventFilter <> '' then
    if FEventFilter <> AName then
      Exit;
  FUserListener(AAnimState, AName, state, entry);
end;

{ TspUserEventPublisher }

procedure TspUserEventPublisher.Event_OnAnimEvent(const AAnimState: IspAnimationState; state: PspAnimation; entry: PspTrackEntry; event_type: TspEventType);
var subs: TSubsList;
    i: Integer;
begin
  subs := GetSubsList;
  for i := 0 to Length(subs) - 1 do
    (subs[i] as IspEventSubscriber).Event_OnAnimEvent(AAnimState, state, entry, event_type);
end;

procedure TspUserEventPublisher.Event_OnUserEvent(const AAnimState: IspAnimationState; const AName: string; state: PspAnimation; entry: PspTrackEntry);
var subs: TSubsList;
    i: Integer;
begin
  subs := GetSubsList;
  for i := 0 to Length(subs) - 1 do
    (subs[i] as IspEventSubscriber).Event_OnUserEvent(AAnimState, AName, state, entry);
end;

initialization

Init;

finalization

end.
