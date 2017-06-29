unit SpineH;

interface

uses
  mutils;

const
  //cSpineWrapperDll = 'd:\Projects\DSpineWrap\x64\Debug\DSpineWrap.dll';
  cSpineWrapperDll = 'DSpineWrap.dll';

type
  PspPChar = PAnsiChar;

//***Atlas***
  TEngineCreateTexturePage = procedure (const APath: PspPChar; var Width, Height: Integer; var UserData: Pointer);
  TEngineDestroyTexturePage = procedure (var UserData: Pointer);

  {$Z4}
  TspAtlasFormat = (
    SP_ATLAS_UNKNOWN_FORMAT,
    SP_ATLAS_ALPHA,
    SP_ATLAS_INTENSITY,
    SP_ATLAS_LUMINANCE_ALPHA,
    SP_ATLAS_RGB565,
    SP_ATLAS_RGBA4444,
    SP_ATLAS_RGB888,
    SP_ATLAS_RGBA8888
  );

  TspAtlasFilter = (
    SP_ATLAS_UNKNOWN_FILTER,
    SP_ATLAS_NEAREST,
    SP_ATLAS_LINEAR,
    SP_ATLAS_MIPMAP,
    SP_ATLAS_MIPMAP_NEAREST_NEAREST,
    SP_ATLAS_MIPMAP_LINEAR_NEAREST,
    SP_ATLAS_MIPMAP_NEAREST_LINEAR,
    SP_ATLAS_MIPMAP_LINEAR_LINEAR
  );

  TspAtlasWrap = (
    SP_ATLAS_MIRROREDREPEAT,
    SP_ATLAS_CLAMPTOEDGE,
    SP_ATLAS_REPEAT
  );

  PspAtlas = ^TspAtlas;
  PspAtlasPage = ^TspAtlasPage;
  PspAtlasRegion = ^TspAtlasRegion;

  TspAtlasPage = record
    atlas     : PspAtlas;//const spAtlas* atlas;
    name      : PspPChar;
    format    : TspAtlasFormat;
    minFilter : TspAtlasFilter;
    magFilter : TspAtlasFilter;
    uWrap     : TspAtlasWrap;
    vWrap     : TspAtlasWrap;

    UserData  : Pointer;
    Size      : TVec2i;

    next      : PspAtlasPage;
  end;

  TspAtlasRegion = record
    name         : PspPChar;
    pos          : TVec2i;
    size         : TVec2i;
    uv           : TVec4;
    offset       : TVec2i;
    originalSize : TVec2i;
    index        : Integer;
    rotate       : Integer;
    flip         : Integer;
    splits       : PInteger;
    pads         : PInteger;

    page         : PspAtlasPage;

    next         : PspAtlasRegion;
  end;

  TspAtlas = record
    pages    : PspAtlasPage;
    regions  : PspAtlasRegion;

    UserData : Pointer;
  end;

//***SkeletonBinary***
  TspSkeletonBinary = record
    scale : Single;
    attachmentLoader : Pointer; //todo add PspAttachmentLoader
    error : PspPChar;
  end;
  PspSkeletonBinary = ^TspSkeletonBinary;

//***BoneData***
  TspTransformMode = (
    SP_TRANSFORMMODE_NORMAL,
    SP_TRANSFORMMODE_ONLYTRANSLATION,
    SP_TRANSFORMMODE_NOROTATIONORREFLECTION,
    SP_TRANSFORMMODE_NOSCALE,
    SP_TRANSFORMMODE_NOSCALEORREFLECTION
  );

  TspBoneData = record
    index         : Integer;
    name          : PspPChar;
    parent        : PspPChar;
    length        : Single;
    pos           : TVec2;
    rotation      : Single;
    scale         : TVec2;
    shear         : TVec2;
    transformMode : TspTransformMode;
  end;
  PspBoneData = ^TspBoneData;
  PPspBoneData = ^PspBoneData;

//***SlotData***
  TspBlendMode = (
    SP_BLEND_MODE_NORMAL, SP_BLEND_MODE_ADDITIVE, SP_BLEND_MODE_MULTIPLY, SP_BLEND_MODE_SCREEN
  );

  TspSlotData = record
    index         : Integer;
    name          : PspPChar;
    boneData      : PspBoneData;
    attachmentName: PspPChar;
    color         : TVec4;
    darkColor     : PVec4;
    blendMode     : TspBlendMode;
  end;
  PspSlotData = ^TspSlotData;
  PPspSlotData = ^PspSlotData;

//***Skin***
  TspSkin = record
    name : PspPChar;
  end;
  PspSkin = ^TspSkin;
  PPspSkin = ^PspSkin;

//***Events***
  TspEventData = record
    name       : PspPChar;
    intValue   : Integer;
    floatValue : Single;
    stringValue: PspPChar;
  end;
  PspEventData = ^TspEventData;
  PPspEventData = ^PspEventData;

//***Timeline***
  PspTimeline = Pointer; //todo add timeline stuff
  PPspTimeline = ^PspTimeline;

//***Animation***
  TspMixPose = (
    SP_MIX_POSE_SETUP,
    SP_MIX_POSE_CURRENT,
    SP_MIX_POSE_CURRENT_LAYERED
  );

  TspMixDirection = (
    SP_MIX_DIRECTION_IN,
    SP_MIX_DIRECTION_OUT
  );

  TspAnimation = record
    name     : PspPChar;
    duration : Single;

    timelinesCount: Integer;
    timelines     : PPspTimeline;
  end;
  PspAnimation = ^TspAnimation;
  PPspAnimation = ^PspAnimation;

//***IkContraintData***
  TspIkConstraintData = record
    name : PspPChar;
    order: Integer;

    bonesCount: Integer;
    bones     : PPspBoneData;

    target       : PspBoneData;
    bendDirection: Integer;
    mix          : Single;
  end;
  PspIkConstraintData = ^TspIkConstraintData;
  PPspIkConstraintData = ^PspIkConstraintData;

//***TransformConstraintData***
  TspTransformConstraintData = record
    name : PspPChar;
    order: Integer;

    bonesCount: Integer;
    bones     : PPspBoneData;

    target : PspBoneData;

    rotateMix   : Single;
    translateMix: Single;
    scaleMix    : Single;
    shearMix    : Single;

    offsetRotation: Single;
    offset        : TVec2;
    offsetScale   : TVec2;
    offsetShearY  : Single;

    realtive: Integer;
    local   : Integer;
  end;
  PspTransformConstraintData = ^TspTransformConstraintData;
  PPspTransformConstraintData = ^PspTransformConstraintData;

//***PathConstraintData***
  TspPositionMode = (
    SP_POSITION_MODE_FIXED, SP_POSITION_MODE_PERCENT
  );

  TspSpacingMode = (
    SP_SPACING_MODE_LENGTH, SP_SPACING_MODE_FIXED, SP_SPACING_MODE_PERCENT
  );

  TspRotateMode = (
    SP_ROTATE_MODE_TANGENT, SP_ROTATE_MODE_CHAIN, SP_ROTATE_MODE_CHAIN_SCALE
  );

  TspPathConstraintData = record
    name : PspPChar;
    order: Integer;

    bonesCount: Integer;
    bones     : PPspBoneData;

    target : PspBoneData;

    positionMode: TspPositionMode;
    spacingMode : TspSpacingMode;
    rotateMode  : TspRotateMode;

    offsetRotation: Single;
    position      : Single;
    spacing       : Single;
    rotateMix     : Single;
    translateMix  : Single;
  end;
  PspPathConstraintData = ^TspPathConstraintData;
  PPspPathConstraintData = ^PspPathConstraintData;

//***SkeletonData***
  TspSkeletonData = record
    version: PspPChar;
    hash   : PspPChar;
    size   : TVec2;

    bonesCount: Integer;
    bones: PPspBoneData;

    slotsCount: Integer;
    slots: PPspSlotData;

    skinsCount: Integer;
    skins: PPspSkin;
    defaultSkin: PspSkin;

    eventsCount: Integer;
    events: PPspEventData;

    animationsCount: Integer;
    animations: PPspAnimation;

    ikConstraintsCount: Integer;
    ikConstraints: PPspIkConstraintData;

    transformConstraintsCount: Integer;
    transformConstraints: PPspTransformConstraintData;

    pathConstraintsCount: Integer;
    pathConstraints: PPspPathConstraintData;
  end;
  PspSkeletonData = ^TspSkeletonData;

//***Bone***
  PspSkeleton = ^TspSkeleton;
  PspBone = ^TspBone;
  PPspBone = ^PspBone;

  TspBone = record
    data         : PspBoneData;
    skeleton     : PspSkeleton;
    parent       : PspBone;
    childrenCount: Integer;
    children     : PPspBone;

    Pos     : TVec2;
    Rotation: Single;
    Scale   : TVec2;
    Shear   : TVec2;

    AbsPos     : TVec2;
    AbsRotation: Single;
    AbsScale   : TVec2;
    AbsShear   : TVec2;

    appliedValid: Integer;//bool

    a: Single;
    b: Single;
    WorldX: Single;
    c: Single;
    d: Single;
    WorldY: Single;

    sorted: Integer;//bool
  end;

//***Attachment***
  TspAttachmentType = (
    SP_ATTACHMENT_REGION,
    SP_ATTACHMENT_BOUNDING_BOX,
    SP_ATTACHMENT_MESH,
    SP_ATTACHMENT_LINKED_MESH,
    SP_ATTACHMENT_PATH,
    SP_ATTACHMENT_POINT,
    SP_ATTACHMENT_CLIPPING
  );

  TspAttachment = record
    name  : PspPChar;
    atype : TspAttachmentType;

    vtable: Pointer;
    attachmentLoader: Pointer;
  end;
  PspAttachment = ^TspAttachment;

//***RegionAttachment***
  TspRegionAttachment = record
    super   : TspAttachment;
    path    : PspPChar;
    pos     : TVec2;
    scale   : TVec2;
    rotation: Single;
    size    : TVec2;
    color   : TVec4;

    userData          : Pointer;
    regionOffset      : TVec2i;
    regionSize        : TVec2i;
    regionOriginalSize: TVec2i;

    offset: array[0..7] of Single;
    uvs   : array[0..7] of Single;
  end;
  PspRegionAttachment = ^TspRegionAttachment;

//***VertexAttachment***
  TspVertexAttachment = record
    super : TspAttachment;

    bonesCount : Integer;
    bones      : PInteger;

    verticesCount: Integer;
    vertices     : PSingle;

    worldVerticesLength: Integer;

    id: Integer;
  end;
  PspVertexAttachment = ^TspVertexAttachment;

//***MeshAttachment***
  PspMeshAttachment = ^TspMeshAttachment;
  TspMeshAttachment = record
    super : TspVertexAttachment;

    UserData          : Pointer;
    regionOffset      : TVec2i;
    regionSize        : TVec2i;
    regionOriginalSize: TVec2i;

    regionUV : TVec2;
    regionUV2: TVec2;

    regionRotate: Integer; {bool}

    path: PspPChar;

    regionUVs: PSingle;
    uvs: PSingle;

    trianglesCount: Integer;
    triangles: PWord;

    color: TVec4;

    hullLength: Integer;

    parentMesh: PspMeshAttachment;
    inheritDeform: Integer; {bool}

    {Nonessential}
    edgesCount: Integer;
    edges: PInteger;
    size: TVec2;
  end;

//***Slot***
  TspSlot = record
    data: PspSlotData;
    bone: PspBone;
    color     : TVec4;
    darkColor : PVec4;
    attachment: PspAttachment;

    attachmentVerticesCapacity: Integer;
    attachmentVerticesCount   : Integer;
    attachmentVertices        : PSingle;
  end;
  PspSlot = ^TspSlot;
  PPspSlot = ^PspSlot;

//***IkConstraint***
  TspIkConstraint = record
    data: PspIkConstraintData;

    bonesCount: Integer;
    bones     : PPspBone;

    target    : PspBone;
    bendDirection: Integer;
    mix: Single;
  end;
  PspIkConstraint = ^TspIkConstraint;
  PPspIkConstraint = ^PspIkConstraint;

//***TransformConstraint***
  TspTransformConstraint = record
    data: PspTransformConstraintData;

    bonesCount: Integer;
    bones     : PPspBone;

    target    : PspBone;

    rotateMix   : Single;
    translateMix: Single;
    scaleMix    : Single;
    shearMix    : Single;
  end;
  PspTransformConstraint = ^TspTransformConstraint;
  PPspTransformConstraint = ^PspTransformConstraint;

//***PathConstraint***
  TspPathConstraint = record
    data: PspPathConstraintData;

    bonesCount: Integer;
    bones     : PPspBone;

    target    : PspBone;

    position    : Single;
    spacing     : Single;
    rotateMix   : Single;
    transalteMix: Single;

    spacesCount : Integer;
    spaces: PSingle;

    positionsCount: Integer;
    positions: PSingle;

    worldCount: Integer;
    world: PSingle;

    curvesCount: Integer;
    curves: PSingle;

    lengthsCount: Integer;
    lengths: PSingle;

    segments: array [0..9] of Single;
  end;
  PspPathConstraint = ^TspPathConstraint;
  PPspPathConstraint = ^PspPathConstraint;

//***Event***
  TspEvent = record
    data       : PspEventData;
    time       : Single;
    intValue   : Integer;
    floatValue : Integer;
    stringValue: PspPChar;
  end;
  PspEvent = ^TspEvent;
  PPspEvent = ^PspEvent;

//***Skeleton***
  TspSkeleton = record
    data: PspSkeletonData;

    bonesCount: Integer;
    bones     : PPspBone;
    root      : PspBone;

    slotsCount: Integer;
    slots     : PPspSlot;
    drawOrder : PPspSlot;

    ikConstraintsCount: Integer;
    ikConstraints     : PPspIkConstraint;

    transformConstraintsCount: Integer;
    transformConstraints     : PPspTransformConstraint;

    pathConstraintsCount: Integer;
    pathConstraints     : PPspPathConstraint;

    skin : PspSkin;
    color: TVec4;

    time : Single;
    flip : TVec2i;
    pos  : TVec2;
  end;

procedure Init;

function  spAtlas_create(const data: PspPChar; len: Integer; const dir: PspPChar; UserData: Pointer): PspAtlas; external cSpineWrapperDll;
function  spAtlas_createFromFile(const path: PspPChar; UserData: Pointer): PspAtlas; external cSpineWrapperDll;
procedure spAtlas_dispose(self: PspAtlas); external cSpineWrapperDll;
function  spAtlas_findRegion(atlas: PspAtlas): PspAtlasRegion; external cSpineWrapperDll; //Returns 0 if the region was not found.


//function  spSkeletonBinary_createWithLoader(.....): PspSkeletonBinary
function  spSkeletonBinary_create(const atlas: PspAtlas): PspSkeletonBinary; external cSpineWrapperDll;
procedure spSkeletonBinary_dispose(self: PspSkeletonBinary); external cSpineWrapperDll;


function  spSkeletonBinary_readSkeletonData(SBinary: PspSkeletonBinary; binary: PByte; len: Integer): PspSkeletonData; external cSpineWrapperDll;
function  spSkeletonBinary_readSkeletonDataFile (SBinary: PspSkeletonBinary; path: PspPChar): PspSkeletonData; external cSpineWrapperDll;


function  spSkeletonData_create (): PspSkeletonData; external cSpineWrapperDll;
procedure spSkeletonData_dispose (self: PspSkeletonData); external cSpineWrapperDll;


function  spBoneData_create (index: Integer; name: PspPChar; parent: PspBoneData): PspBoneData; external cSpineWrapperDll;
procedure spBoneData_dispose (self: PspBoneData); external cSpineWrapperDll;


function  spSlotData_create (index: Integer; name: PspPChar; boneData: PspBoneData): PspSlotData; external cSpineWrapperDll;
procedure spSlotData_dispose (self: PspSlotData); external cSpineWrapperDll;
procedure spSlotData_setAttachmentName (self: PspSlotData; attachmentName: PspPChar); external cSpineWrapperDll;

//todo add skin functions
//spSkin* spSkin_create (const char* name);
//void spSkin_dispose (spSkin* self);
//void spSkin_addAttachment (spSkin* self, int slotIndex, const char* name, spAttachment* attachment);
//spAttachment* spSkin_getAttachment (const spSkin* self, int slotIndex, const char* name);
//const char* spSkin_getAttachmentName (const spSkin* self, int slotIndex, int attachmentIndex);
//void spSkin_attachAll (const spSkin* self, struct spSkeleton* skeleton, const spSkin* oldspSkin);


function  spEventData_create  (name: PspPChar): PspEventData; external cSpineWrapperDll;
procedure spEventData_dispose (self: PspEventData); external cSpineWrapperDll;


function  spAnimation_create (name: PspPChar; timelinesCount: Integer): PspAnimation; external cSpineWrapperDll;
procedure spAnimation_dispose (self: PspAnimation); external cSpineWrapperDll;

procedure spAnimation_apply (self: PspAnimation; skeleton: PspSkeleton; lastTime: Single; time: Single; loop: Integer;
		events: PPspEvent; out eventsCount: Integer; alpha: Single; pose: TspMixPose; direction: TspMixDirection); external cSpineWrapperDll;


function  spIkConstraintData_create (name: PspPChar): PspIkConstraintData; external cSpineWrapperDll;
procedure spIkConstraintData_dispose (self: PspIkConstraintData); external cSpineWrapperDll;


function  spTransformConstraintData_create (name: PspPChar): PspTransformConstraintData; external cSpineWrapperDll;
procedure spTransformConstraintData_dispose (self: PspTransformConstraintData); external cSpineWrapperDll;


function  spPathConstraintData_create (name: PspPChar): PspPathConstraintData; external cSpineWrapperDll;
procedure spPathConstraintData_dispose (self: PspPathConstraintData); external cSpineWrapperDll;


procedure spBone_setYDown (yDown: Integer{bool}); external cSpineWrapperDll;
function  spBone_isYDown(): Integer{bool}; external cSpineWrapperDll;

function  spBone_create (data: PspBoneData; skeleton: PspSkeleton; parent: PspBone): PspBone; external cSpineWrapperDll;
procedure spBone_dispose (self: PspBone); external cSpineWrapperDll;

procedure spBone_setToSetupPose (self: PspBone); external cSpineWrapperDll;

procedure spBone_updateWorldTransform (self: PspBone); external cSpineWrapperDll;
procedure spBone_updateWorldTransformWith (self: PspBone; x, y, rotation, scaleX, scaleY, shearX, shearY: Single); external cSpineWrapperDll;

function spBone_getWorldRotationX (self: PspBone): Single; external cSpineWrapperDll;
function spBone_getWorldRotationY (self: PspBone): Single; external cSpineWrapperDll;
function spBone_getWorldScaleX (self: PspBone): Single; external cSpineWrapperDll;
function spBone_getWorldScaleY (self: PspBone): Single; external cSpineWrapperDll;

procedure spBone_updateAppliedTransform (self: PspBone); external cSpineWrapperDll;

procedure spBone_worldToLocal(self: PspBone; worldX, worldY: Single; out localX, localY: Single); external cSpineWrapperDll;
procedure spBone_localToWorld(self: PspBone; localX, localY: Single; out worldX, worldY: Single); external cSpineWrapperDll;

function spBone_worldToLocalRotation (self: PspBone; worldRotation: Single): Single; external cSpineWrapperDll;
function spBone_localToWorldRotation (self: PspBone; localRotation: Single): Single; external cSpineWrapperDll;

procedure spBone_rotateWorld (self: PspBone; degrees: Single); external cSpineWrapperDll;


procedure spRegionAttachment_setUVs (self: PspRegionAttachment; u,v,u2,v2: Single; rotate: Integer {bool}); external cSpineWrapperDll;
procedure spRegionAttachment_updateOffset (self: PspRegionAttachment); external cSpineWrapperDll;
procedure spRegionAttachment_computeWorldVertices (self: PspRegionAttachment; bone: PspBone; vertices: PSingle; offset: Integer; stride: Integer); external cSpineWrapperDll;

procedure spVertexAttachment_computeWorldVertices (self: PspVertexAttachment; slot: PspSlot; start, count: Integer; worldVertices: PSingle; offset: Integer; stride: Integer); external cSpineWrapperDll;

procedure spMeshAttachment_updateUVs (self: PspMeshAttachment); external cSpineWrapperDll;
procedure spMeshAttachment_setParentMesh (self: PspMeshAttachment; parentMesh: PspMeshAttachment); external cSpineWrapperDll;

function spSlot_create (data: PspSlotData; bone: PspBone): PspSlot; external cSpineWrapperDll;
procedure spSlot_dispose (self: PspSlot); external cSpineWrapperDll;

//void spSlot_setAttachment (spSlot* self, spAttachment* attachment); todo

procedure spSlot_setAttachmentTime (self: PspSlot; time: Single); external cSpineWrapperDll;
function  spSlot_getAttachmentTime (self: PspSlot): Single; external cSpineWrapperDll;

procedure spSlot_setToSetupPose (self: PspSlot); external cSpineWrapperDll;


function  spIkConstraint_create (data: PspIkConstraintData; skeleton: PspSkeleton): PspIkConstraint; external cSpineWrapperDll;
procedure spIkConstraint_dispose (self: PspIkConstraint); external cSpineWrapperDll;

procedure spIkConstraint_apply (self: PspIkConstraint); external cSpineWrapperDll;

procedure spIkConstraint_apply1 (bone: PspBone; targetX: Single; targetY: Single; alpha: Single); external cSpineWrapperDll;
procedure spIkConstraint_apply2 (parent: PspBone; child: PspBone; targetX: Single; targetY: Single; bendDirection: Integer; alpha: Single); external cSpineWrapperDll;


function  spTransformConstraint_create (data: PspTransformConstraintData; skeleton: PspSkeleton): PspTransformConstraint; external cSpineWrapperDll;
procedure spTransformConstraint_dispose (self: PspTransformConstraint); external cSpineWrapperDll;

procedure spTransformConstraint_apply (self: PspTransformConstraint); external cSpineWrapperDll;


function spPathConstraint_create (data: PspPathConstraintData; skeleton: PspSkeleton): PspPathConstraint; external cSpineWrapperDll;
procedure spPathConstraint_dispose (self: PspPathConstraint); external cSpineWrapperDll;

procedure spPathConstraint_apply (self: PspPathConstraint); external cSpineWrapperDll;
//float* spPathConstraint_computeWorldPositions(spPathConstraint* self, spPathAttachment* path, int spacesCount, int/*bool*/ tangents, int/*bool*/percentPosition, int/**/percentSpacing); todo

function spEvent_create (time: Single; data: PspEventData): PspEvent; external cSpineWrapperDll;
procedure spEvent_dispose (self: PspEvent); external cSpineWrapperDll;

function  spSkeleton_create (data: PspSkeletonData): PspSkeleton; external cSpineWrapperDll;
procedure spSkeleton_dispose (self: PspSkeleton); external cSpineWrapperDll;

//Caches information about bones and constraints. Must be called if bones or constraints, or weighted path attachments are added or removed.
procedure spSkeleton_updateCache (self: PspSkeleton); external cSpineWrapperDll;
procedure spSkeleton_updateWorldTransform (self: PspSkeleton); external cSpineWrapperDll;

// Sets the bones, constraints, and slots to their setup pose values
procedure spSkeleton_setToSetupPose (self: PspSkeleton); external cSpineWrapperDll;

// Sets the bones and constraints to their setup pose values
procedure spSkeleton_setBonesToSetupPose (self: PspSkeleton); external cSpineWrapperDll;
procedure spSkeleton_setSlotsToSetupPose (self: PspSkeleton); external cSpineWrapperDll;

// Returns Nil if the bone was not found
function spSkeleton_findBone (self: PspSkeleton; boneName: PspPChar): PspBone; external cSpineWrapperDll;
// Returns -1 if the bone was not found
function spSkeleton_findBoneIndex (self: PspSkeleton; boneName: PspPChar): Integer; external cSpineWrapperDll;

// Returns Nil if the slot was not found
function spSkeleton_findSlot (self: PspSkeleton; slotName: PspPChar): PspSlot; external cSpineWrapperDll;
// Returns -1 if the slot was not found
function spSkeleton_findSlotIndex (self: PspSkeleton; slotName: PspPChar): Integer; external cSpineWrapperDll;

{* Sets the skin used to look up attachments before looking in the SkeletonData defaultSkin. Attachments from the new skin are
 * attached if the corresponding attachment from the old skin was attached. If there was no old skin, each slot's setup mode
 * attachment is attached from the new skin.
 * @param skin May be 0.}
procedure spSkeleton_setSkin (self: PspSkeleton; skin: PspSkin); external cSpineWrapperDll;

{* Returns 0 if the skin was not found. See spSkeleton_setSkin.
 * @param skinName May be 0. }
function spSkeleton_setSkinByName (self: PspSkeleton; skinName: PspPChar): Integer; external cSpineWrapperDll;

//todo implemetn attachment stuff
//function spSkeleton_getAttachmentForSlotName (self: PspSkeleton; slotName: PspPChar; attachmentName: PspPChar): PspAttachment;
//spAttachment* spSkeleton_getAttachmentForSlotIndex (const spSkeleton* self, int slotIndex, const char* attachmentName);
//int spSkeleton_setAttachment (spSkeleton* self, const char* slotName, const char* attachmentName);

function spSkeleton_findIkConstraint (self: PspSkeleton; constraintName: PspPChar): PspIkConstraint; external cSpineWrapperDll;
function spSkeleton_findTransformConstraint (self: PspSkeleton; constraintName: PspPChar): PspTransformConstraint; external cSpineWrapperDll;
function spSkeleton_findPathConstraint (self: PspSkeleton; constraintName: PspPChar): PspPathConstraint; external cSpineWrapperDll;

procedure spSkeleton_update (self: PspSkeleton; deltaTime: Single); external cSpineWrapperDll;

procedure SetTextureAllocator(ACreate: TEngineCreateTexturePage; ADestroy: TEngineDestroyTexturePage); cdecl; external cSpineWrapperDll;

implementation

uses
  avTexLoader, avTypes;

procedure avCreateTexturePage(const APath: PspPChar; var Width, Height: Integer; var UserData: Pointer);
var texData: ITextureData;
begin
  texData := LoadTexture(string(APath));
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

initialization

finalization

end.
