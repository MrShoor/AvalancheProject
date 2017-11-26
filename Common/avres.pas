unit avRes;
{$I avConfig.inc}

interface

uses
  Classes, SysUtils, intfUtils, avBase, avContext, mutils, avTypes, avTess, avPlatform, avContnrs;

type
  TavProgram = class;
  TavCamera = class;
  TavProjection = class;
  TavCursor = class;
  TavFrameBuffer = class;

  { TavMainRender }

  TavMainRender = class (TavObject)
  private
    FFrameID   : Int64;
    FActiveAPI : T3DAPI;
    FContext   : IRenderContext;
    FWindow    : TWindow;
    FActiveProgram: TavProgram;
    FActiveFrameBuffer: TavFrameBuffer;

    FCamera: TavCamera;
    FProjection: TavProjection;
    FCursor: TavCursor;

    FUPS: Integer;
    FBindTime: Int64;
    FLastTime: Int64;

    function GetActiveProgram: TavProgram;
    function GetWindow: TWindow;
    function GetWindowSize: TVec2i;
    procedure SetActiveProgram(AValue: TavProgram);
    procedure SetWindow(AValue: TWindow);
    function SetActiveFrameBuffer(const AFBO: TavFrameBuffer): TavFrameBuffer;

    procedure AfterInit3D_Broadcast;
    procedure BeforeFree3D_Broadcast;
    procedure AfterFree3D_Broadcast;
  protected
    procedure EMWindowDestroy(var msg: TavMessage); message EM_WINDOWDESTROY;
  public
    procedure ProcessTimerEvents;
    property UpdateStatesInterval: Integer read FUPS write FUPS;
    function Time64: Int64;
    function Time: Double;
    function BindTime64: Int64;

    property FrameID: Int64 read FFrameID;
    property Camera: TavCamera read FCamera;
    property Projection: TavProjection read FProjection;
    property Cursor: TavCursor read FCursor;

    function ActiveApi: T3DAPI;
    function Inited3D: Boolean;
    procedure Init3D(api: T3DAPI = apiOGL);
    procedure Free3D;
    function Context: IRenderContext;

    function  States: IRenderStates;
    function  Bind: boolean;
    function  Binded: boolean;
    procedure Unbind;

    procedure Clear(const color  : TVec4;      doColor  : Boolean = True;
                          depth  : Single = 1; doDepth  : Boolean = False;
                          stencil: Byte   = 0; doStencil: Boolean = False);
    procedure Flush;
    procedure Present;
    procedure InvalidateWindow;

    property Window: TWindow read GetWindow write SetWindow;
    property WindowSize: TVec2i read GetWindowSize;
    property ActiveProgram: TavProgram read GetActiveProgram write SetActiveProgram;
    function ActiveFrameBuffer: TavFrameBuffer;

    procedure Dispatch(var message); override;
    constructor Create(AParent: TavObject); override;
    destructor Destroy; override;
  end;

  { TavMainRenderChild }

  TavMainRenderChild = class (TavObject)
  strict private
    FMain : TavMainRender;
  protected
    function CanRegister(target: TavObject): boolean; override;
  public
    function Main: TavMainRender;
  end;

  { TavCamera }

  TavCamera = class (TavObject)
  private
    FUpdating: Integer;

    FEye: TVec3;
    FAt : TVec3;
    FUp : TVec3;

    FTargetEye: TVec3;
    FTargetAt : TVec3;
    FTargetUp : TVec3;
    FPlaying  : boolean;
    FPlayType : byte;
    FPlaySpeed: single;

    FMatrix  : TMat4;
    FuMatrix : TMat4;

    procedure StopPlaying;

    procedure SetEye(const Value: TVec3);
    procedure SetAt(const Value: TVec3);
    procedure SetUp(const Value: TVec3);

    procedure UpdateMatrix;
    procedure SetMatrix(AValue: TMat4);
    function  GetViewDir: TLine;
  protected
    procedure EMUps(var msg: TavMessage); message EM_UPS;
  public
    property Eye: TVec3 read FEye write SetEye;
    property At : TVec3 read FAt  write SetAt;
    property Up : TVec3 read FUp  write SetUp;
    property ViewDir: TLine read GetViewDir;
    property Playing: boolean read FPlaying;

    property Matrix : TMat4 read FMatrix write SetMatrix;
    property uMatrix: TMat4 read FuMatrix;

    procedure SetVectors(const Aeye, Aat, Aup: TVec3);

    procedure MoveForward(step:single);
    procedure MoveBack   (step:single);
    procedure MoveLeft   (step:single);
    procedure MoveRight  (step:single);
    procedure MoveUp     (step:single);
    procedure MoveDown   (step:single);
    procedure MoveDeep   (step:single);
    procedure RotateEyeHorizontal(angle:single);
    procedure RotateEyeVertical  (angle:single);
    procedure RotateAtHorizontal (angle:single);
    procedure RotateAtVertical   (angle:single);

    procedure Play(targetEye, targetAt, targetUp: TVec3; speedN: single; PlayType: byte = 0);

    procedure BeginUpdate;
    procedure EndUpdate;

    constructor Create(AParent: TavObject); override;
  end;

  { TavProjection }

  TavProjection = class (TavObject)
  private
    FDepthRange: TVec2;
    FOrtho: Boolean;
    FUpdating: Integer;

    FFov        : single;
    FAspect     : single;
    FNearPlane  : single;
    FFarPlane   : single;
    FOrthoHeight: single;

    FMatrix : TMat4;
    FuMatrix: TMat4;

    procedure SetAspect(AValue: single);
    procedure SetDepthRange(const AValue: TVec2);
    procedure SetFarPlane(AValue: single);
    procedure SetFov(AValue: single);
    procedure SetMatrix(const AValue: TMat4);
    procedure SetNearPlane(AValue: single);
    procedure SetOrtho(AValue: Boolean);
    procedure SetOrthoHeight(AValue: single);

    procedure UpdateMatrix;
  public
    function DepthRangeMinMax: TVec2;

    property Fov        : single read FFov         write SetFov;
    property Aspect     : single read FAspect      write SetAspect;
    property NearPlane  : single read FNearPlane   write SetNearPlane;
    property FarPlane   : single read FFarPlane    write SetFarPlane;
    property OrthoHeight: single read FOrthoHeight write SetOrthoHeight;
    property DepthRange : TVec2  read FDepthRange  write SetDepthRange;

    property Ortho: Boolean read FOrtho write SetOrtho;

    property Matrix    : TMat4 read FMatrix write SetMatrix;
    property uMatrix   : TMat4 read FuMatrix;

    procedure BeginUpdate;
    procedure EndUpdate;

    constructor Create(AParent: TavObject); override;
  end;

  { TavCursor }

  TavCursor = class (TavObject)
  private
    FMain: TavMainRender;

    FCameraUpdateID    : Integer;
    FProjectionUpdateID: Integer;
    FFrameID           : Int64;

    FWindowCur: TVec2;
    FFrom, FAt: TVec3;
    FRay: TLine;
    procedure UpdateCursor;

    function GetWindowCur: TVec2;
    function GetAt: TVec3;
    function GetFrom: TVec3;
    function GetRay: TLine;
  protected
    function CanRegister(target: TavObject): boolean; override;
  public
    property WindowCur: TVec2 read GetWindowCur;
    property From: TVec3 read GetFrom;
    property At  : TVec3 read GetAt;
    property Ray : TLine read GetRay;
  end;

  { TavRes }

  TavRes = class(TavMainRenderChild)
  private
    FDirty   : Boolean;
  protected
    procedure AfterInit3D; virtual;
    procedure BeforeFree3D; virtual;
    procedure AfterFree3D; virtual;

    function DoBuild: Boolean; virtual;

    procedure EM3DAfterFree(var msg: TavMessage); message EM_3D_AFTER_FREE;
    procedure EM3DBeforeFree(var msg: TavMessage); message EM_3D_BEFORE_FREE;
    procedure EM3DAfterInit(var msg: TavMessage); message EM_3D_AFTER_INIT;
  public
    procedure Build;
    procedure Invalidate;
    function Valid: Boolean;
    procedure AfterConstruction; override;

    constructor Create(AParent: TavObject); overload; override;
  end;

  { TavVerticesBase }

  TavVerticesBase = class(TavRes)
  private
    FCullMode: TCullingMode;
    FPrimType: TPrimitiveType;
  protected
    FbufH: IctxVetexBuffer;
    FBuildedPrimType: TPrimitiveType;
    procedure BeforeFree3D; override;
  public
    function BuildedVertCount: Integer;
    property BuildedPrimType : TPrimitiveType read FBuildedPrimType;

    property CullMode: TCullingMode read FCullMode write FCullMode;
    property PrimType: TPrimitiveType read FPrimType write FPrimType;
  end;

  { TavIndicesBase }

  TavIndicesBase = class(TavRes)
  private
    FCullMode: TCullingMode;
    FPrimType: TPrimitiveType;
  protected
    FbufH: IctxIndexBuffer;
    procedure BeforeFree3D; override;
  public
    function BuildedIndCount: Integer;

    property CullMode: TCullingMode read FCullMode write FCullMode;
    property PrimType: TPrimitiveType read FPrimType write FPrimType;
  end;

  { TavStructuredBase }

  TavStructuredBase = class(TavRes)
  private
  protected
    FbufH: IctxStructuredBuffer;
    procedure BeforeFree3D; override;
  public
  end;

  { TavVB }

  TavVB = class(TavVerticesBase)
  private
    FDropLocalAfterBuild: Boolean;
    FVert: IVerticesData;
    procedure SetVert(AValue: IVerticesData);
  protected
    function DoBuild: Boolean; override;
  public
    property DropLocalAfterBuild: Boolean read FDropLocalAfterBuild write FDropLocalAfterBuild;
    property Vertices: IVerticesData read FVert write SetVert;
  end;

  { TavIB }

  TavIB = class(TavIndicesBase)
  private
    FDropLocalAfterBuild: Boolean;
    FInd: IIndicesData;
    procedure SetIndices(const AValue: IIndicesData);
  protected
    function DoBuild: Boolean; override;
  public
    property DropLocalAfterBuild: Boolean read FDropLocalAfterBuild write FDropLocalAfterBuild;
    property Indices: IIndicesData read FInd write SetIndices;
  end;

  { TavSB }

  TavSB = class(TavStructuredBase)
  private
    FDropLocalAfterBuild: Boolean;
    FVert: IVerticesData;
    procedure SetVert(AValue: IVerticesData);
  protected
    function DoBuild: Boolean; override;
  public
    property DropLocalAfterBuild: Boolean read FDropLocalAfterBuild write FDropLocalAfterBuild;
    property Vertices: IVerticesData read FVert write SetVert;
  end;

  { TNodeManager }
  //TNode class should contain next fields:
  //  TNode.DirtyIndex : Integer
  //  TNode.Range      : IMemRange
  //  TNode.Size       : Integer
  {$IfDef DCC}
    TDefaultNode = class (TInterfacedObject)
      DirtyIndex : Integer;
      Range      : IMemRange;
      function Size: Integer; virtual; abstract;
    end;
  {$EndIf}

  {$IfDef FPC}generic TNodeManager<TNode> = class {$EndIf}
  {$IfDef DCC} TNodeManager<TNode: TDefaultNode> = class {$EndIf}
  strict private type
    IGroupHash = {$IfDef FPC}specialize{$EndIf} IHashMap<TNode, Integer>;
    TGroupHash = {$IfDef FPC}specialize{$EndIf} THashMap<TNode, Integer>;
    IGroupList = {$IfDef FPC}specialize{$EndIf} IArray<TNode>;
    TGroupList = {$IfDef FPC}specialize{$EndIf} TArray<TNode>;
  strict private
    FRangeMan  : IRangeManager;
    FNodes     : IGroupHash;

    FDirtyNodes : IGroupList;
    FDirtyAll   : Boolean;

    FEnumIndex : Integer;
    function GetName: string;
    procedure SetName(const AValue: string);
  public
    property Name: string read GetName write SetName;

    function RangeManSize: Integer;

    procedure Add(const ANode: TNode);
    function  Del(const ANode: TNode): Boolean;
    function  Invalidate(const ANode: TNode): Boolean;
    procedure InvalidateAll;
    procedure Validate(const ANode: TNode);
    procedure ValidateAll;

    function DirtyCount: Integer;
    function TotalCount: Integer;
    procedure Reset;
    function NextDirty(out ANode: TNode): Boolean;
    function Next(out ANode: TNode): Boolean;

    procedure AfterConstruction; override;
  end;

  IManagedHandle = interface (IUnknown)
    function HandleData: Pointer;
    function Offset: Integer;
    function Size  : Integer;
  end;

  IVBManagedHandle = IManagedHandle;
  { TavVBManaged }

  TavVBManaged = class(TavVerticesBase)
  private type
    {$IfDef FPC}
    TVBNode = class (TInterfacedObject, IManagedHandle)
      Owner     : TavVBManaged;
      Range     : IMemRange;
      Vert      : IVerticesData;
      DirtyIndex: Integer;
      function HandleData: Pointer;
      function Size: Integer; Inline;
      function Offset: Integer;
      destructor Destroy; override;
    end;
    {$EndIf}
    {$IfDef DCC}
    TVBNode = class (TDefaultNode, IManagedHandle)
      Owner     : TavVBManaged;
      Vert      : IVerticesData;
      function HandleData: Pointer;
      function Size: Integer; override;
      function Offset: Integer;
      destructor Destroy; override;
    end;
    {$EndIf}
    TNodes = {$IfDef FPC}specialize{$EndIf} TNodeManager<TVBNode>;
  private
    FNodes : TNodes;
  protected
    function DoBuild: Boolean; override;
    procedure SetName(const Value: string); override;
  public
    function HasData: Boolean;

    function Add(const AVert: IVerticesData): IVBManagedHandle;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  IIBManagedHandle = IManagedHandle;
  { TavIBManaged }

  TavIBManaged = class(TavIndicesBase)
  private type
    {$IfDef FPC}
    TIBNode = class (TInterfacedObject, IManagedHandle)
      Owner     : TavIBManaged;
      Range     : IMemRange;
      Ind       : IIndicesData;
      DirtyIndex: Integer;
      function HandleData: Pointer;
      function Size: Integer; Inline;
      function Offset: Integer;
      destructor Destroy; override;
    end;
    {$EndIf}
    {$IfDef DCC}
    TIBNode = class (TDefaultNode, IManagedHandle)
      Owner     : TavIBManaged;
      Ind       : IIndicesData;
      function HandleData: Pointer;
      function Size: Integer; override;
      function Offset: Integer;
      destructor Destroy; override;
    end;
    {$EndIf}
    TNodes = {$IfDef FPC}specialize{$EndIf} TNodeManager<TIBNode>;
  private
    FNodes : TNodes;
  protected
    function DoBuild: Boolean; override;
    procedure SetName(const Value: string); override;
  public
    function HasData: Boolean;
    function Add(const AInd: IIndicesData): IIBManagedHandle;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  { TavTextureBase }

  TavTextureBase = class(TavRes)
  protected
    FTexH: IctxTexture;
    FTargetFormat: TTextureFormat;
    FsRGB: Boolean;
    procedure BeforeFree3D; override;
  public
    function Width: Integer;
    function Height: Integer;
    function Size: TVec2;

    procedure CopyFrom(const ASrc: TavTextureBase; SrcMipLevel: Integer; const ASrcRect: TRectI);
    procedure GenerateMips();

    procedure ReadBack(var ATexData: ITextureData; ASlice: Integer; const AMipLevel: Integer); //-1 for all mip levels

    property TargetFormat: TTextureFormat read FTargetFormat write FTargetFormat;
    property sRGB: Boolean read FsRGB write FsRGB;
  end;

  { TavTexture }

  TavTexture = class(TavTextureBase)
  private
    FForcedArray: Boolean;
    FForcedPOT: Boolean;
    FAutoGenerateMips: Boolean;
    FDropLocalAfterBuild: Boolean;
    FTexData: ITextureData;
    FImageSize: TVec2;
    procedure SetAutoGenerateMips(AValue: Boolean);
    procedure SetForcedArray(AValue: Boolean);
    procedure SetTexData(AValue: ITextureData);
  protected
    function DoBuild: Boolean; override;
  public
    property ForcedArray: Boolean read FForcedArray write SetForcedArray;
    property DropLocalAfterBuild: Boolean read FDropLocalAfterBuild write FDropLocalAfterBuild;
    property TexData: ITextureData read FTexData write SetTexData;

    function ImageSize: TVec2;

    property AutoGenerateMips: Boolean read FAutoGenerateMips write SetAutoGenerateMips;
    property ForcedPOT: Boolean read FForcedPOT write FForcedPOT;

    procedure AfterConstruction; override;
  end;

  IMTManagedHandle = IManagedHandle;

  { TavMultiTexture }

  TavMultiTexture = class(TavTextureBase)
  private type
    {$IfDef FPC}
    TMTNode = class (TInterfacedObject, IMTManagedHandle)
      Owner     : TavMultiTexture;
      Range     : IMemRange;
      TexData   : ITextureData;
      DirtyIndex: Integer;
      function HandleData: Pointer;
      function Size: Integer; Inline;
      function Offset: Integer;
      destructor Destroy; override;
    end;
    {$EndIf}
    {$IfDef DCC}
    TMTNode = class (TDefaultNode, IMTManagedHandle)
      Owner     : TavMultiTexture;
      TexData   : ITextureData;
      function HandleData: Pointer;
      function Size: Integer; override;
      function Offset: Integer;
      destructor Destroy; override;
    end;
    {$EndIf}
    TNodes = {$IfDef FPC}specialize{$EndIf} TNodeManager<TMTNode>;
  private
    FAutoGenerateMips: Boolean;
    FNodes: TNodes;
    FInitSize: TVec2i;
    procedure SetAutoGenerateMips(const AValue: Boolean);
  protected
    function DoBuild: Boolean; override;
    procedure SetName(const Value: string); override;
  public
    function Add(const ATexData: ITextureData): IMTManagedHandle; overload;
    function Add(const ATexData: ITextureData; out NewAdded: Boolean): IMTManagedHandle; overload;

    property AutoGenerateMips: Boolean read FAutoGenerateMips write SetAutoGenerateMips;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  ISpriteData = interface
    function  Image: ITextureMip;
    procedure OnSetRect(const ANewPos: TRectI);
  end;

  { TavAtlasBase }

  TavAtlasBase = class(TavTextureBase)
  private type
    IQuadMap  = {$IfDef FPC}specialize{$EndIf} IHashMap<Pointer, IQuadRange>;
    TQuadMap  = {$IfDef FPC}specialize{$EndIf} THashMap<Pointer, IQuadRange>;
  private
    FAutoGrow : Boolean;
    FQuadManager: IQuadManager;

    FQuads : IQuadMap;
    FInvalidQuads : IQuadMap;
  protected
    function  DoBuild: Boolean; override;
    procedure OnQuadMove(const Sender: IQuadManager; const Quad: IQuadRange; const OldRect: TRectI);
    procedure InvalidateAll;
  public
    function AtlasSize: TVec2i;
    property AutoGrow: Boolean read FAutoGrow write FAutoGrow;

    procedure AddSprite(const ASprite: ISpriteData); //or invalidate
    procedure DelSprite(const ASprite: ISpriteData);

    procedure AfterConstruction; override;
  end;

  TavAtlasArrayReferenced = class;

  ISpriteIndex = interface
    function Atlas: TavAtlasArrayReferenced;
    function Index: Integer;
    function Data : ITextureMip;
  end;
  ISpriteIndexArr = {$IfDef FPC}specialize{$EndIf} IArray<ISpriteIndex>;
  TSpriteIndexArr = {$IfDef FPC}specialize{$EndIf} TArray<ISpriteIndex>;
  ISpriteIndexSet = {$IfDef FPC}specialize{$EndIf} IHashSet<ISpriteIndex>;
  TSpriteIndexSet = {$IfDef FPC}specialize{$EndIf} THashSet<ISpriteIndex>;

  { TavAtlasArrayReferenced }

  TavAtlasArrayReferenced  = class(TavTextureBase)
  private type

    TSpriteIndex = class(TInterfacedObject, ISpriteIndex)
    private
      FOwner: TavAtlasArrayReferenced;
      FIndex: Integer;
      FData : ITextureMip;
      FSlice: Integer;
      FQuad : IQuadRange;
      function Atlas: TavAtlasArrayReferenced;
      function Index: Integer;
      function Data : ITextureMip;
    public
      constructor Create(const AOwner: TavAtlasArrayReferenced; const AIndex: Integer; const AData: ITextureMip; const ASlice: Integer; const AQuad: IQuadRange);
      destructor Destroy; override;
    end;

    TPageInfo = record
      QManager: IQuadManager;
      Valid   : Boolean;
    end;
    PPageInfo = ^TPageInfo;

    TSpriteRegion = packed record
      Rect  : TVec4;
      Slice : Single;
      class function Layout(): IDataLayout; static;
    end;

    ISpriteMap = {$IfDef FPC}specialize{$EndIf} IHashMap<ITextureMip, TSpriteIndex>;
    TSpriteMap = {$IfDef FPC}specialize{$EndIf} THashMap<ITextureMip, TSpriteIndex>;

    ISpriteList = {$IfDef FPC}specialize{$EndIf} IArray<TSpriteIndex>;
    TSpriteList = {$IfDef FPC}specialize{$EndIf} TArray<TSpriteIndex>;

    IPages = {$IfDef FPC}specialize{$EndIf} IArray<TPageInfo>;
    TPages = {$IfDef FPC}specialize{$EndIf} TArray<TPageInfo>;

    IRegions = {$IfDef FPC}specialize{$EndIf} IArray<TSpriteRegion>;
    TRegions = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TSpriteRegion>;

    IFreeIndices = {$IfDef FPC}specialize{$EndIf} IArray<Integer>;
    TFreeIndices = {$IfDef FPC}specialize{$EndIf} TArray<Integer>;
  private
    FAutoGenerateMips: Boolean;
    FSprites    : ISpriteMap;
    FSpriteList : ISpriteList;
    FFreeIndices: IFreeIndices;

    FPages : IPages;
    FInvalidPagesCount : Boolean;

    FRegions: IRegions;
    FRegionsBuffer: TavSB;

    FTargetSize: TVec2i;

    function  AllocIndex: Integer;
    procedure AllocQuad(const ASize: TVec2i; out ARange: IQuadRange; out ASlice: Integer);
    procedure SetAutoGenerateMips(const AValue: Boolean);
    procedure SetTargetSize(const AValue: TVec2i);

    procedure FullRebuild;
  protected
    function DoBuild: Boolean; override;
  public
    property RegionsVB: TavSB read FRegionsBuffer;

    procedure CleanUnused;

    property AutoGenerateMips: Boolean read FAutoGenerateMips write SetAutoGenerateMips;
    property TargetSize: TVec2i read FTargetSize write SetTargetSize;

    function ObtainSprite(const ASprite: ITextureMip): ISpriteIndex;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

  { TavMultiSampleTexture }

  TavMultiSampleTexture = class(TavTextureBase)
  private
    FTargetWidth      : Integer;
    FTargetHeight     : Integer;
    FTargetSampleCount: Integer;
  protected
    function DoBuild: Boolean; override;
  public
    procedure SetSize(const AWidth, AHeight: Integer);
    property TargetSampleCount: Integer read FTargetSampleCount write FTargetSampleCount;
  end;

  { TavUAV }

  TavUAV = class(TavStructuredBase)
  private
    FElementsCount : Integer;
    FStrideSize    : Integer;
    FAppendable    : Boolean;
    FUAVBufH       : IctxUAV;
    FInitialElement: TByteArr;
  private
    FDropLocalAfterBuild: Boolean;
    FVert: IVerticesData;
    procedure SetVert(AValue: IVerticesData);
  protected
    procedure BeforeFree3D; override;
    function DoBuild: Boolean; override;
  public
    property DropLocalAfterBuild: Boolean read FDropLocalAfterBuild write FDropLocalAfterBuild;
    property Vertices: IVerticesData read FVert write SetVert;

    function ElementsCount: Cardinal;
    function StrideSize: Cardinal;

    function ReadCounter: Cardinal;
    function ReadRAWData(const AElementsCount: Integer = -1): TByteArr;

    procedure SetSize(const AElementsCount, AStrideSize: Integer; const AAppendable: Boolean; AInitialElement: Pointer = Nil);
  end;

  { TavProgram }

  TavProgram = class(TavRes)
  private type
    TUniformMatrices = (VP_Matrix,
                        VP_InverseMatrix,
                        V_Matrix,
                        V_InverseMatrix,
                        P_Matrix,
                        P_InverseMatrix,
                        FBOFlip);
  private
    FSrc: string;
    FSrcPath: string;
    FFromRes: Boolean;
    FOutLayout: IDataLayout;
    FProgram : IctxProgram;

    FUniformsMatrices     : array [TUniformMatrices] of TUniformField;
    FCameraUpdateID       : Int64;
    FProjectionUpdateID   : Int64;
    FFlipUpdated          : Boolean;

    FDefaultPrimType: TPrimitiveType;
    FDefaultCullMode: TCullingMode;
    FIsIndexedBinded: Boolean;
    FSelectedVertexCount: Integer;
    FSelectedIndexCount : Integer;
  protected
    procedure BeforeFree3D; override;
    function DoBuild: Boolean; override;

    procedure UpdateMatrices;
  public
    procedure Select(const APatchSize: Integer = 0); virtual;
    procedure SetAttributes(Model: TavVerticesBase;
                            ModelIndices: TavIndicesBase;
                            Instance: TavVerticesBase;
                            InstanceStepRate: Integer = 1);

    function  GetUniformField  (const AName: string): TUniformField;

    procedure SetUniform (const Field: TUniformField; const value: integer);     overload;
    procedure SetUniform (const Field: TUniformField; const value: single);      overload;
    procedure SetUniform (const Field: TUniformField; const v: TVec2);           overload;
    procedure SetUniform (const Field: TUniformField; const v: TVec3);           overload;
    procedure SetUniform (const Field: TUniformField; const v: TVec4);           overload;
    procedure SetUniform (const Field: TUniformField; const values: TSingleArr); overload;
    procedure SetUniform (const Field: TUniformField; const v: TVec4arr);        overload;
    procedure SetUniform (const Field: TUniformField; const m: TMat4);           overload;
    procedure SetUniform (const Field: TUniformField; const tex: TavTextureBase;  const sampler: TSamplerInfo); overload;
    procedure SetUniform (const Field: TUniformField; const buf: TavStructuredBase); overload;

    procedure SetUniform (const AName: string; const value: integer);     overload;
    procedure SetUniform (const AName: string; const value: single);      overload;
    procedure SetUniform (const AName: string; const v: TVec2);           overload;
    procedure SetUniform (const AName: string; const v: TVec3);           overload;
    procedure SetUniform (const AName: string; const v: TVec4);           overload;
    procedure SetUniform (const AName: string; const values: TSingleArr); overload;
    procedure SetUniform (const AName: string; const v: TVec4arr);        overload;
    procedure SetUniform (const AName: string; const m: TMat4);           overload;
    procedure SetUniform (const AName: string; const tex: TavTextureBase; const sampler: TSamplerInfo); overload;
    procedure SetUniform (const AName: string; const buf: TavStructuredBase); overload;

    procedure SetComputeUAV(const Index: Integer; const uav: TavUAV; const initial: Integer = 0);

    procedure Load(const AProgram: string; FromResource: boolean = false; const AProgramPath: string = ''); overload;
    procedure Load(const AProgram: string; const AStremOutputLayout: IDataLayout; FromResource: boolean = false; const AProgramPath: string = ''); overload;

    procedure Draw(InstanceCount: Integer = 0;
                   Start: integer = 0; Count: integer = - 1;
                   BaseVertex: integer = 0; BaseInstance: Integer = 0); overload;
    procedure Draw(PrimTopology: TPrimitiveType; CullMode: TCullingMode; IndexedGeometry: Boolean;
                   InstanceCount: Integer = 0;
                   Start: integer = 0; Count: integer = - 1;
                   BaseVertex: integer = 0; BaseInstance: Integer = 0); overload;
    procedure Dispatch(GroupDims: TVec3i);

    destructor Destroy; override;
  end;

  { TavFrameBuffer }

  TavFrameBuffer = class(TavRes)
  private type
    TAttachInfo = record
      tex: IWeakRef;
      mipLevel: Integer;
    end;
    TStreamAttachInfo = record
      buffer: IWeakRef;
      offset: Integer;
    end;
  private const
    EmptyAttachInfo: TAttachInfo = (tex : nil; mipLevel : 0);
  private type
    TColorsList = {$IfDef FPC}specialize{$EndIf} TArray<TAttachInfo>;
    IColorsList = {$IfDef FPC}specialize{$EndIf} IArray<TAttachInfo>;
    TUAVList = {$IfDef FPC}specialize{$EndIf} TArray<IWeakRef>;
    IUAVList = {$IfDef FPC}specialize{$EndIf} IArray<IWeakRef>;
    TStreamsList = {$IfDef FPC}specialize{$EndIf} TArray<TStreamAttachInfo>;
    IStreamsList = {$IfDef FPC}specialize{$EndIf} IArray<TStreamAttachInfo>;
  private
    FFrameBuf: IctxFrameBuffer;
    FColors  : IColorsList;
    FUAVs    : IUAVList;
    FStreams : IStreamsList;
    FDepth   : TAttachInfo;
    FFrameRect: TRectI;
    FForcedPOT: Boolean;
    procedure SetFrameRect(AValue: TRectI);
  protected
    procedure BeforeFree3D; override;
    function DoBuild: Boolean; override;
  public
    function Select(UpdateProjMatrix: Boolean = True): TavFrameBuffer;

    property FrameRect: TRectI read FFrameRect write SetFrameRect;
    property ForcedPOT: Boolean read FForcedPOT write FForcedPOT;

    procedure ClearColorList;
    procedure ClearUAVList;

    function GetColor(Index: Integer): TavTextureBase;
    function GetColorMipLevel(Index: Integer): Integer;
    procedure SetColor(Index: Integer; AValue: TavTextureBase; mipLevel: Integer = 0);
    procedure SetUAV(Index: Integer; AValue: TavUAV); overload;
    procedure SetUAV(Index: Integer; AValue: TavTextureBase); overload;
    procedure SetStreamOut(Index: Integer; ABuffer: TavVerticesBase; Offset: Integer);

    function  GetDepth: TavTextureBase;
    procedure SetDepth(AValue: TavTextureBase; mipLevel: Integer);

    procedure Clear(index: Integer; color: TVec4);
    procedure ClearDS(depth: Single; clearDepth: Boolean = True; stencil: Integer = 0; clearStencil: Boolean = False);
    procedure ClearUAV(index: Integer; color: TVec4i);

    procedure BlitToWindow(index: Integer = 0);

    procedure AfterConstruction; override;
  end;

function Create_FrameBuffer(Parent: TavObject; textures: array of TTextureFormat): TavFrameBuffer; overload;
function Create_FrameBuffer(Parent: TavObject; textures: array of TTextureFormat; srgb: array of Boolean): TavFrameBuffer; overload;
function Create_FrameBufferMultiSampled(Parent: TavObject; textures: array of TTextureFormat; const ASampleCount: Integer): TavFrameBuffer;

procedure DrawManaged(const AProg: TavProgram;
                      const Vert: IVBManagedHandle; const Ind: IIBManagedHandle; const Inst: IVBManagedHandle); overload;
procedure DrawManaged(const AProg: TavProgram;
                      const Vert: IVBManagedHandle; const Ind: IIBManagedHandle; const Inst: IVBManagedHandle;
                      PrimTopology: TPrimitiveType; CullMode: TCullingMode); overload;

implementation

uses
  TypInfo,
  Math,
  avLog,
  avContext_OGL,
  avContext_DX11,
  avTexLoader;

function Create_FrameBuffer(Parent: TavObject; textures: array of TTextureFormat; srgb: array of Boolean): TavFrameBuffer;
var i, colorIndex: Integer;
    tex: TavTexture;
    hasDepth: Boolean;
begin
  Result := TavFrameBuffer.Create(Parent);
  colorIndex := 0;
  hasDepth := False;
  for i := Low(textures) to High(textures) do
  begin
    if IsDepthTexture[textures[i]] and hasDepth then Continue;
    tex := TavTexture.Create(Result);
    tex.TargetFormat := textures[i];
    tex.AutoGenerateMips := False;
    tex.sRGB := srgb[i];
    if IsDepthTexture[textures[i]] then
    begin
        Result.SetDepth(tex, 0);
        hasDepth := True;
    end
    else
    begin
        Result.SetColor(colorIndex, tex, 0);
        Inc(colorIndex);
    end;
  end;
end;

function Create_FrameBuffer(Parent: TavObject; textures: array of TTextureFormat): TavFrameBuffer; overload;
var srgb: array of Boolean;
    i: Integer;
begin
  SetLength(srgb, Length(textures));
  for i := 0 to High(srgb) do srgb[i] := false;
  Result := Create_FrameBuffer(Parent, textures, srgb);
end;

function Create_FrameBufferMultiSampled(Parent: TavObject; textures: array of TTextureFormat; const ASampleCount: Integer): TavFrameBuffer;
var i, colorIndex: Integer;
    tex: TavMultiSampleTexture;
    hasDepth: Boolean;
begin
  Result := TavFrameBuffer.Create(Parent);
  colorIndex := 0;
  hasDepth := False;
  for i := Low(textures) to High(textures) do
  begin
    if IsDepthTexture[textures[i]] and hasDepth then Continue;
    tex := TavMultiSampleTexture.Create(Result);
    tex.TargetFormat := textures[i];
    tex.TargetSampleCount := ASampleCount;
    if IsDepthTexture[textures[i]] then
    begin
        Result.SetDepth(tex, 0);
        hasDepth := True;
    end
    else
    begin
        Result.SetColor(colorIndex, tex, 0);
        Inc(colorIndex);
    end;
  end;
end;

procedure DrawManaged(const AProg: TavProgram; const Vert: IVBManagedHandle;
  const Ind: IIBManagedHandle; const Inst: IVBManagedHandle);
begin
  DrawManaged(AProg, Vert, Ind, Inst, AProg.FDefaultPrimType, AProg.FDefaultCullMode);
end;

procedure DrawManaged(const AProg: TavProgram; const Vert: IVBManagedHandle;
  const Ind: IIBManagedHandle; const Inst: IVBManagedHandle;
  PrimTopology: TPrimitiveType; CullMode: TCullingMode);
var
    VertNode : TavVBManaged.TVBNode;
    IndNode  : TavIBManaged.TIBNode;
    InstNode : TavVBManaged.TVBNode;

    BaseInst, InstCount: Integer;
    Start, Count, BaseVertex: Integer;
begin
  IndNode := nil;
  InstNode := nil;
  if Vert = nil then Exit;
  VertNode := TavVBManaged.TVBNode(Vert.HandleData);
  Assert(Assigned(VertNode));
  if Assigned(Ind)  then IndNode := TavIBManaged.TIBNode(Ind.HandleData);
  if Assigned(Inst) then InstNode := TavVBManaged.TVBNode(Inst.HandleData);

  if Assigned(InstNode) then
  begin
    BaseInst  := InstNode.Range.Offset;
    InstCount := InstNode.Range.Size;
  end
  else
  begin
    BaseInst := 0;
    InstCount := 0;
  end;

  if Assigned(IndNode) then
  begin
    Start := IndNode.Range.Offset;
    Count := IndNode.Range.Size;
    BaseVertex := VertNode.Range.Offset;
  end
  else
  Begin
    Start := VertNode.Range.Offset;
    Count := VertNode.Range.Size;
    BaseVertex := 0;
  end;

  AProg.Draw(PrimTopology, CullMode, Assigned(IndNode), InstCount, Start, Count, BaseVertex, BaseInst);
end;

{ TavSB }

procedure TavSB.SetVert(AValue: IVerticesData);
begin
  if FVert = AValue then Exit;
  FVert := AValue;
  Invalidate;
end;

function TavSB.DoBuild: Boolean;
begin
  if Assigned(FVert) then
  begin
    if FbufH = nil then FbufH := Main.Context.CreateStructBuffer;
    FbufH.ElementSize := FVert.Layout.Size;
    FbufH.AllocMem(Max(FVert.Data.size, FVert.Layout.Size), FVert.Data.data);
  end;
  if FDropLocalAfterBuild then FVert := nil;
  Result := True;
end;

{ TavStructuredBase }

procedure TavStructuredBase.BeforeFree3D;
begin
  inherited BeforeFree3D;
  if Assigned(FbufH) then Invalidate;
  FbufH := nil;
end;

{ TSpriteRegion }

class function TavAtlasArrayReferenced.TSpriteRegion.Layout: IDataLayout;
begin
  Result := LB.Add('Rect', ctFloat, 4)
              .Add('Slice', ctFloat, 1)
              .Finish();
end;

{ TavAtlasArrayReferenced }

function TavAtlasArrayReferenced.AllocIndex: Integer;
var dummy: TSpriteRegion;
begin
  if FFreeIndices.Count = 0 then
  begin
    Result := FSpriteList.Add(nil);
    dummy.Rect := Vec(0,0,0,0);
    dummy.Slice := 0;
    FRegions.Add(dummy);
  end
  else
  begin
    Result := FFreeIndices.Last;
    FFreeIndices.Delete(FFreeIndices.Count-1);
  end;
end;

procedure TavAtlasArrayReferenced.AllocQuad(const ASize: TVec2i; out ARange: IQuadRange; out ASlice: Integer);
var Page: PPageInfo;
    NewPage: TPageInfo;
begin
  Assert(ASize.x <= FTargetSize.x);
  Assert(ASize.y <= FTargetSize.y);
  ASlice := 0;
  while ASlice < FPages.Count do
  begin
    Page := PPageInfo(FPages.PItem[ASlice]);
    try
      ARange := Page^.QManager.Alloc(ASize.x, ASize.y);
      Page^.Valid := False;
      Invalidate;
      Exit;
    except
      on e: EQuadRangeOutOfSpace do
        Inc(ASlice);
    end;
  end;

  FInvalidPagesCount := True;
  NewPage.QManager := Create_IQuadManager(FTargetSize.x, FTargetSize.y);
  NewPage.Valid := False;
  FPages.Add(NewPage);
  AllocQuad(ASize, ARange, ASlice);
end;

procedure TavAtlasArrayReferenced.SetAutoGenerateMips(const AValue: Boolean);
begin
  if FAutoGenerateMips = AValue then Exit;
  FAutoGenerateMips := AValue;
  FInvalidPagesCount := True;
  Invalidate;
end;

procedure TavAtlasArrayReferenced.SetTargetSize(const AValue: TVec2i);
begin
  if FTargetSize = AValue then Exit;
  FTargetSize := AValue;
  FullRebuild;
end;

procedure TavAtlasArrayReferenced.FullRebuild;
var i : Integer;
    sprite: TSpriteIndex;
    region: TSpriteRegion;
begin
  for i := 0 to FSpriteList.Count - 1 do
  begin
    sprite := FSpriteList[i];
    if sprite = nil then Continue;
    sprite.FQuad := nil;
    sprite.FSlice := 0;
  end;

  FRegions.Clear();

  //todo sort by size
  for i := 0 to FSpriteList.Count - 1 do
  begin
    sprite := FSpriteList[i];
    if sprite = nil then Continue;
    AllocQuad(Vec(sprite.FData.Width,sprite.FData.Height), sprite.FQuad, sprite.FSlice);
    region.Rect := sprite.FQuad.Rect.v;
    region.Slice := sprite.FSlice;
    region.Rect.xy := region.Rect.xy + Vec(1,1);
    region.Rect.zw := region.Rect.zw - Vec(1,1);
    FRegions[sprite.FIndex] := region;
  end;
  FRegionsBuffer.Invalidate;
  FInvalidPagesCount := True;
end;

function TavAtlasArrayReferenced.DoBuild: Boolean;
var page: PPageInfo;
    sprite: TSpriteIndex;
    rct: TRectI;
    i, j: Integer;
    col: TByteArr;
    w,h,slice,px: Integer;
    pf: TImageFormat;
begin
  Result := True;
  if FPages.Count = 0 then Exit;

  if FInvalidPagesCount then
  begin
    FTexH := Main.Context.CreateTexture;
    FTexH.TargetFormat := FTargetFormat;
    FTexH.sRGB := FsRGB;
    FTexH.AllocMem(FTargetSize.x, FTargetSize.y, FPages.Count, FAutoGenerateMips, True);
    for i := 0 to FPages.Count - 1 do
      PPageInfo(FPages.PItem[i])^.Valid := False;
  end;

  for i := 0 to FSpriteList.Count - 1 do
  begin
    sprite := FSpriteList[i];
    if sprite = nil then Continue;
    page := PPageInfo(FPages.PItem[sprite.FSlice]);
    if page^.Valid then Continue;
    rct := sprite.FQuad.Rect;

    w := sprite.FData.Width;
    h := sprite.FData.Height;
    pf:= sprite.FData.PixelFormat;
    px:= ImagePixelSize[pf];
    slice := sprite.FSlice;

    FTexH.SetMipImage(rct.Left+1, rct.Top,      rct.Right-rct.Left-2, 1,                    0, slice, pf, sprite.FData.Pixel(0, h-1));
    FTexH.SetMipImage(rct.Left+1, rct.Top+1,    rct.Right-rct.Left-2, rct.Bottom-rct.Top-2, 0, slice, pf, sprite.FData.Data);
    FTexH.SetMipImage(rct.Left+1, rct.Bottom-1, rct.Right-rct.Left-2, 1,                    0, slice, pf, sprite.FData.Data);

    if Length(col) <> (h + 2)*px then
      SetLength(col, (h + 2)*px);

    Move(sprite.FData.Pixel(w-1,h-1)^, col[0], px);
    for j := 0 to h - 1 do
      Move(sprite.FData.Pixel(w-1,j)^, col[(j+1)*px], px);
    Move(sprite.FData.Pixel(w-1,0)^, col[(h+1)*px], px);
    FTexH.SetMipImage(rct.Left, rct.Top, 1, rct.Bottom-rct.Top, 0, slice, pf, @col[0]);

    Move(sprite.FData.Pixel(0,h-1)^, col[0], px);
    for j := 0 to h - 1 do
      Move(sprite.FData.Pixel(0,j)^, col[(j+1)*px], px);
    Move(sprite.FData.Pixel(0,0)^, col[(h+1)*px], px);
    FTexH.SetMipImage(rct.Right-1, rct.Top, 1, rct.Bottom-rct.Top, 0, slice, pf, @col[0]);
  end;

  for i := 0 to FPages.Count - 1 do
    PPageInfo(FPages.PItem[i])^.Valid := True;

  if AutoGenerateMips then
    FTexH.GenerateMips;
end;

procedure TavAtlasArrayReferenced.CleanUnused;
begin
  FullRebuild;
end;

function TavAtlasArrayReferenced.ObtainSprite(const ASprite: ITextureMip): ISpriteIndex;
var spriteObj: TSpriteIndex;
    range: IQuadRange;
    slice: Integer;
    freeIndex: Integer;
    region: TSpriteRegion;
    allocSize: TVec2i;
begin
  if not FSprites.TryGetValue(ASprite, spriteObj) then
  begin
    allocSize.x := ASprite.Width+2;
    allocSize.y := ASprite.Height+2;
    AllocQuad(allocSize, range, slice);
    freeIndex := AllocIndex();
    spriteObj := TSpriteIndex.Create(Self, freeIndex, ASprite, slice, range);
    FSpriteList[freeIndex] := spriteObj;
    FSprites.Add(ASprite, spriteObj);
    region.Slice := spriteObj.FSlice;
    region.Rect := spriteObj.FQuad.Rect.v;
    region.Rect.xy := region.Rect.xy + Vec(1,1);
    region.Rect.zw := region.Rect.zw - Vec(1,1);
    FRegions[freeIndex] := region;
    FRegionsBuffer.Invalidate;
  end;
  Result := spriteObj;
end;

procedure TavAtlasArrayReferenced.AfterConstruction;
begin
  inherited AfterConstruction;
  FSprites := TSpriteMap.Create();
  FSpriteList := TSpriteList.Create();
  FFreeIndices := TFreeIndices.Create();
  FPages := TPages.Create();
  FRegions := TRegions.Create();

  FRegionsBuffer := TavSB.Create(Self);
  FRegionsBuffer.Vertices := FRegions as IVerticesData;

  FTargetSize := Vec(1024, 1024);
end;

destructor TavAtlasArrayReferenced.Destroy;
var i: Integer;
begin
  for i := 0 to FSpriteList.Count - 1 do
  begin
    if FSpriteList[i] = nil then Continue;
    FSpriteList[i].FQuad  := nil;
    FSpriteList[i].FOwner := nil;
  end;
  inherited Destroy;
end;

{ TavAtlasArrayReferenced.TSpriteIndex }

function TavAtlasArrayReferenced.TSpriteIndex.Atlas: TavAtlasArrayReferenced;
begin
  Result := FOwner;
end;

function TavAtlasArrayReferenced.TSpriteIndex.Index: Integer;
begin
  Result := FIndex;
end;

function TavAtlasArrayReferenced.TSpriteIndex.Data: ITextureMip;
begin
  Result := FData;
end;

constructor TavAtlasArrayReferenced.TSpriteIndex.Create(
  const AOwner: TavAtlasArrayReferenced; const AIndex: Integer;
  const AData: ITextureMip; const ASlice: Integer; const AQuad: IQuadRange);
begin
  FOwner := AOwner;
  FIndex := AIndex;
  FData  := AData;
  FQuad  := AQuad;
  FSlice := ASlice;
end;

destructor TavAtlasArrayReferenced.TSpriteIndex.Destroy;
begin
  if FOwner <> nil then
  begin
    FOwner.FSprites.Delete(FData);
    FOwner.FSpriteList[FIndex] := nil;
    FOwner.FFreeIndices.Add(FIndex);
  end;
  inherited Destroy;
end;

{ TavMultiTexture.TMTNode }

function TavMultiTexture.TMTNode.HandleData: Pointer;
begin
  Result := Self;
end;

function TavMultiTexture.TMTNode.Size: Integer;
begin
  Result := TexData.ItemCount;
end;

function TavMultiTexture.TMTNode.Offset: Integer;
begin
  Result := Range.Offset;
end;

destructor TavMultiTexture.TMTNode.Destroy;
begin
  inherited Destroy;
  Owner.FNodes.Del(Self);
end;

{ TavMultiTexture }

procedure TavMultiTexture.SetAutoGenerateMips(const AValue: Boolean);
begin
  if FAutoGenerateMips = AValue then Exit;
  FAutoGenerateMips := AValue;
  Invalidate;
end;

function TavMultiTexture.DoBuild: Boolean;
var node: TMTNode;
    i: Integer;
begin
  Result := True;
  if FNodes.TotalCount = 0 then Exit;
  if FTexH = nil then
  begin
    FTexH := Main.Context.CreateTexture;
    FTexH.TargetFormat := FTargetFormat;
    FTexH.AllocMem(FInitSize.x, FInitSize.y, FNodes.RangeManSize, FAutoGenerateMips, True);
  end;

  if FTexH.Deep <> FNodes.RangeManSize then
  begin
    FTexH.AllocMem(FInitSize.x, FInitSize.y, FNodes.RangeManSize, FAutoGenerateMips, True);
    FNodes.InvalidateAll;
  end;

  FNodes.Reset;
  while FNodes.NextDirty(node) do
    for i := 0 to node.TexData.ItemCount - 1 do
      FTexH.SetMipImage(0, 0, FInitSize.x, FInitSize.y, 0, node.Offset+i, node.TexData.Format, node.TexData.MipData(i, 0).Data);

  if FAutoGenerateMips then
    FTexH.GenerateMips;
end;

procedure TavMultiTexture.SetName(const Value: string);
begin
  inherited SetName(Value);
  if FNodes <> nil then
    FNodes.Name := Value;
end;

function TavMultiTexture.Add(const ATexData: ITextureData): IMTManagedHandle;
var dummy: Boolean;
begin
  Result := Add(ATexData, dummy);
end;

function TavMultiTexture.Add(const ATexData: ITextureData; out NewAdded: Boolean): IMTManagedHandle;
var node: TMTNode;
begin
  Result := nil;
  NewAdded := False;
  if ATexData = nil then Exit;
  if ATexData.ItemCount = 0 then Exit;

  FNodes.Reset;
  while FNodes.Next(node) do
    if node.TexData = ATexData then
      Exit(node);

  if FInitSize.x < 0 then FInitSize.x := ATexData.Width else Assert(FInitSize.x = ATexData.Width);
  if FInitSize.y < 0 then FInitSize.y := ATexData.Height else Assert(FInitSize.y = ATexData.Height);

  node := TMTNode.Create;
  node.Owner := Self;
  node.TexData := ATexData;
  FNodes.Add(node);
  if FNodes.DirtyCount > 0 then
    Invalidate;

  NewAdded := True;
  Result := node;
end;

procedure TavMultiTexture.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TNodes.Create;
  FInitSize.x := -1;
  FInitSize.y := -1;
end;

destructor TavMultiTexture.Destroy;
begin
  FreeAndNil(FNodes);
  inherited Destroy;
end;

{ TavAtlasBase }

function TavAtlasBase.DoBuild: Boolean;
var pSprite: Pointer;
    sprite: ISpriteData;
    range : IQuadRange;
    img   : ITextureMip;
    rct   : TRectI;
begin
  Result := inherited DoBuild;
  if FTexH = nil then
  begin
    FTexH := Main.Context.CreateTexture;
    FTexH.TargetFormat := FTargetFormat;
    FTexH.sRGB := FsRGB;
  end;
  if (FTexH.Width <> FQuadManager.Width) or (FTexH.Height <> FQuadManager.Height) then
    FTexH.AllocMem(FQuadManager.Width, FQuadManager.Height, 1, False);

  FInvalidQuads.Reset;
  while FInvalidQuads.Next(pSprite, range) do
  begin
    sprite := ISpriteData(pSprite);
    img := sprite.Image;
    rct := range.Rect;
    FTexH.SetMipImage(rct.Left, rct.Top, rct.Right-rct.Left, rct.Bottom-rct.Top, 0, 0, img.PixelFormat, img.Data);
  end;
  FInvalidQuads.Clear;
  Result := True;
end;

procedure TavAtlasBase.OnQuadMove(const Sender: IQuadManager; const Quad: IQuadRange; const OldRect: TRectI);
begin
  ISpriteData(Quad.UserData).OnSetRect(Quad.Rect);
  if FInvalidQuads.AddIfNotContains(Quad.UserData, Quad) then
    Invalidate;
end;

procedure TavAtlasBase.InvalidateAll;
var pSprite: Pointer;
    range : IQuadRange;
begin
  FQuads.Reset;
  while FQuads.Next(pSprite, range) do
    FInvalidQuads.AddIfNotContains(pSprite, range);
end;

function TavAtlasBase.AtlasSize: TVec2i;
begin
  Result.x := FQuadManager.Width;
  Result.y := FQuadManager.Height;
end;

procedure TavAtlasBase.AddSprite(const ASprite: ISpriteData);
var img     : ITextureMip;
    range   : IQuadRange;
    repacked: Boolean;
begin
  if not FQuads.TryGetValue(Pointer(ASprite), range) then
  begin
    img := ASprite.Image;

    repacked := False;
    repeat
      try
        range := FQuadManager.Alloc(img.Width, img.Height, Pointer(ASprite));
      except
        on e: EQuadRangeOutOfSpace do
        begin
          if FAutoGrow then
          begin
            if not repacked and (FQuadManager.FreeArea > img.Width*img.Height * 4) then
              FQuadManager.Repack({$IfDef FPC}@{$EndIf}OnQuadMove)
            else
            begin
              if (FQuadManager.Width >= 8192) and (FQuadManager.Height >= 8192) then raise EQuadRangeOutOfSpace.Create('Atlas texture size reach limits');
              FQuadManager.SetSize(FQuadManager.Width*2, FQuadManager.Height*2, {$IfDef FPC}@{$EndIf}OnQuadMove);
            end;
            repacked := True;
            InvalidateAll;
          end
          else
            raise ;
        end;
      end;
    until range <> nil;

    FQuads.AddOrSet(Pointer(ASprite), range);
    ASprite.OnSetRect(range.Rect);
    FInvalidQuads.AddIfNotContains(Pointer(ASprite), range);
    Invalidate;
  end
  else
  begin
    img := ASprite.Image;
    if (img.Width <> range.Width) or (img.Height <> range.Height) then
    begin
      range := nil;
      DelSprite(ASprite);
      AddSprite(ASprite);
    end;
  end;
end;

procedure TavAtlasBase.DelSprite(const ASprite: ISpriteData);
begin
  FQuads.Delete(Pointer(ASprite));
  FInvalidQuads.Delete(Pointer(ASprite));
end;

procedure TavAtlasBase.AfterConstruction;
begin
  inherited AfterConstruction;
  FAutoGrow := True;
  FQuadManager := Create_IQuadManager(1024, 1024);
  FQuads := TQuadMap.Create;
  FInvalidQuads := TQuadMap.Create;
end;

{ TavUAV }

procedure TavUAV.SetVert(AValue: IVerticesData);
begin
  if FVert = AValue then Exit;
  FVert := AValue;
  FElementsCount := AValue.VerticesCount;
  FStrideSize := AValue.Layout.Size;
  FInitialElement := nil;
  FAppendable := False;
  Invalidate;
end;

procedure TavUAV.BeforeFree3D;
begin
  inherited;
  if FUAVBufH <> nil then Invalidate;
  FUAVBufH := nil;
end;

function TavUAV.DoBuild: Boolean;
var initData: TByteArr;
    initDataPtr: Pointer;
    i: Integer;
begin
  Result := inherited DoBuild;
  if FVert = nil then
  begin
    initDataPtr := nil;
    if FInitialElement <> nil then
    begin
      SetLength(initData, FElementsCount*FStrideSize);
      for i := 0 to FElementsCount - 1 do
        Move(FInitialElement[0], initData[i*FStrideSize], FStrideSize);
      initDataPtr := @initData[0];
    end;
    FUAVBufH := Main.Context.CreateUAV(FElementsCount, FStrideSize, FAppendable, initDataPtr);
  end
  else
    FUAVBufH := Main.Context.CreateUAV(FElementsCount, FStrideSize, FAppendable, FVert.Data.data);
  FbufH := FUAVBufH;
end;

function TavUAV.ElementsCount: Cardinal;
begin
  Result := FElementsCount;
end;

function TavUAV.StrideSize: Cardinal;
begin
  Result := FStrideSize;
end;

function TavUAV.ReadCounter: Cardinal;
begin
  if FBufH = nil then
    Result := 0
  else
    Result := FUAVBufH.ReadCounter;
end;

function TavUAV.ReadRAWData(const AElementsCount: Integer = -1): TByteArr;
begin
  if FBufH = nil then Exit(Nil);
  Result := FUAVBufH.ReadRAWData(AElementsCount);
end;

procedure TavUAV.SetSize(const AElementsCount, AStrideSize: Integer; const AAppendable: Boolean; AInitialElement: Pointer);
begin
  FVert := nil;
  if FElementsCount <> AElementsCount then
  begin
    FElementsCount := AElementsCount;
    Invalidate;
  end;
  if FStrideSize <> AStrideSize then
  begin
    FStrideSize := AStrideSize;
    Invalidate;
  end;
  if FAppendable <> AAppendable then
  begin
    FAppendable := AAppendable;
    Invalidate;
  end;

  if AInitialElement = nil then
    FInitialElement := nil
  else
  begin
    if (Length(FInitialElement)=AStrideSize) then
        if CompareMem(AInitialElement, @FInitialElement[0], AStrideSize) then Exit;
    SetLength(FInitialElement, AStrideSize);
    Move(AInitialElement^, FInitialElement[0], AStrideSize);
    Invalidate;
  end;
end;


{ TavMultiSampleTexture }

function TavMultiSampleTexture.DoBuild: Boolean;
begin
  inherited DoBuild;
  if FTexH = nil then FTexH := Main.Context.CreateTexture;
  FTexH.TargetFormat := FTargetFormat;
  FTexH.sRGB := FsRGB;
  FTexH.AllocMultiSampled(FTargetWidth, FTargetHeight, FTargetSampleCount);
  Result := True;
end;

procedure TavMultiSampleTexture.SetSize(const AWidth, AHeight: Integer);
begin
  if (FTargetHeight <> AHeight) or
     (FTargetWidth <> AWidth) then
  begin
    FTargetWidth := AWidth;
    FTargetHeight := AHeight;
    Invalidate;
  end;
end;

{ TavTextureBase }

procedure TavTextureBase.BeforeFree3D;
begin
  inherited BeforeFree3D;
  if Assigned(FTexH) then Invalidate;
  FTexH := nil;
end;

function TavTextureBase.Width: Integer;
begin
  if Assigned(FTexH) then
    Result := FTexH.Width
  else
    Result := 0;
end;

function TavTextureBase.Height: Integer;
begin
  if Assigned(FTexH) then
    Result := FTexH.Height
  else
    Result := 0;
end;

function TavTextureBase.Size: TVec2;
begin
  if Assigned(FTexH) then
  begin
    Result.x := FTexH.Width;
    Result.y := FTexH.Height;
  end
  else
  begin
    Result.x := 0;
    Result.y := 0;
  end;
end;

procedure TavTextureBase.CopyFrom(const ASrc: TavTextureBase; SrcMipLevel: Integer; const ASrcRect: TRectI);
var w, h: Integer;
begin
  if ASrc.FTexH = nil then Exit;
  if FTexH = Nil then FTexH := Main.Context.CreateTexture;
  FTexH.TargetFormat := FTargetFormat;
  FTexH.sRGB := FsRGB;

  w := Abs(ASrcRect.Right - ASrcRect.Left);
  h := Abs(ASrcRect.Bottom - ASrcRect.Top);
  if ( FTexH.Width  <> w ) or
     ( FTexH.Height <> h ) or
     ( FTexH.Format <> ASrc.FTexH.Format ) then
  begin
    FTexH.AllocMem(w, h, 1, False);
  end;
  FTexH.CopyFrom(0, Vec(0,0), ASrc.FTexH, SrcMipLevel, ASrcRect);
  FDirty := False;
end;

procedure TavTextureBase.GenerateMips();
begin
  if FTexH = nil then Exit;
  FTexH.GenerateMips;
end;

procedure TavTextureBase.ReadBack(var ATexData: ITextureData; ASlice: Integer; const AMipLevel: Integer);
var
  i: Integer;
begin
  if FTexH = nil then Exit;
  if (ATexData.Width <> Width) or (ATexData.Height <> Height) or
     (ATexData.ItemCount <> 1) or (ATexData.MipsCount < AMipLevel) or
     (ImagePixelSize[ATexData.Format] <> TexturePixelSize[FTexH.Format]) then
    ATexData := EmptyTexData(FTexH.Width, FTexH.Height, FTexH.Format, FTexH.MipsCount > 0, True);
  if AMipLevel < 0 then
    for i := 0 to FTexH.MipsCount do
      FTexH.ReadBack(ATexData, ASlice, i)
  else
    FTexH.ReadBack(ATexData, ASlice, AMipLevel);
end;


{ TavIBManaged.TIBNode }

function TavIBManaged.TIBNode.HandleData: Pointer;
begin
  Result := Self;
end;

function TavIBManaged.TIBNode.Size: Integer;
begin
  Result := Ind.IndicesCount;
end;

function TavIBManaged.TIBNode.Offset: Integer;
begin
  Result := Range.Offset;
end;

destructor TavIBManaged.TIBNode.Destroy;
begin
  inherited Destroy;
  Owner.FNodes.Del(Self);
end;

{ TavIBManaged }

function TavIBManaged.DoBuild: Boolean;
  function GetFirstIndexSize: Integer;
  var node: TIBNode;
  begin
    Result := 0;
    FNodes.Reset;
    if FNodes.Next(node) then
    begin
      Result := node.Ind.IndexSize;
      FPrimType := node.Ind.PrimType;
    end;
  end;
  procedure SetNodeData(const node: TIBNode; const IndexSize: Integer);
  begin
    Assert(node.Ind.IndexSize = IndexSize);
    Assert(node.Ind.Data.data <> nil);
    Assert(node.Ind.Data.size = node.Range.Size*IndexSize);
    FbufH.SetSubData(node.Range.Offset*IndexSize, node.Range.Size*IndexSize, node.Ind.Data.data);
  end;
var node  : TIBNode;
    IndexSize: Integer;
begin
  Result := True;
  if Assigned(FbufH) and (FNodes.DirtyCount = 0) then Exit;

  if FbufH = nil then
  begin
    FbufH := Main.Context.CreateIndexBuffer;
    case GetFirstIndexSize of
      2: FbufH.IndexSize := TIndexSize.Word;
      4: FbufH.IndexSize := TIndexSize.DWord;
    else
      Assert(False);
    end;
  end;
  IndexSize := IndexSizeInBytes[FbufH.IndexSize];
  if FbufH.Size <> FNodes.RangeManSize*IndexSize then
  begin
    FNodes.InvalidateAll;
    FbufH.AllocMem(FNodes.RangeManSize*IndexSize, nil);
  end;
  FNodes.Reset;
  while FNodes.NextDirty(node) do
    SetNodeData(node, IndexSize);
  FNodes.ValidateAll;
end;

procedure TavIBManaged.SetName(const Value: string);
begin
  inherited SetName(Value);
  if FNodes <> nil then
    FNodes.Name := Value;
end;

function TavIBManaged.HasData: Boolean;
begin
  Result := FNodes.RangeManSize > 0;
end;

function TavIBManaged.Add(const AInd: IIndicesData): IIBManagedHandle;
var node: TIBNode;
begin
  Result := nil;
  if AInd = nil then Exit;
  if AInd.IndicesCount = 0 then Exit;

  node := TIBNode.Create;
  node.Owner := Self;
  node.Ind := AInd;
  FNodes.Add(node);
  if FNodes.DirtyCount > 0 then
    Invalidate;

  Result := node;
end;

procedure TavIBManaged.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TNodes.Create;
end;

destructor TavIBManaged.Destroy;
begin
  FreeAndNil(FNodes);
  inherited Destroy;
end;

{ TavVBManaged.TVBNode }

function TavVBManaged.TVBNode.HandleData: Pointer;
begin
  Result := Self;
end;

function TavVBManaged.TVBNode.Size: Integer;
begin
  Result := Vert.VerticesCount;
end;

function TavVBManaged.TVBNode.Offset: Integer;
begin
  Result := Range.Offset;
end;

destructor TavVBManaged.TVBNode.Destroy;
begin
  inherited Destroy;
  Owner.FNodes.Del(Self);
end;

{ TNodeManager }

function TNodeManager{$IfDef DCC}<TNode>{$EndIf}.GetName: string;
begin
  Result := FRangeMan.Name;
end;

procedure TNodeManager{$IfDef DCC}<TNode>{$EndIf}.SetName(const AValue: string);
begin
  FRangeMan.Name := AValue;
end;

function TNodeManager{$IfDef DCC}<TNode>{$EndIf}.RangeManSize: Integer;
begin
  Result := FRangeMan.Size;
end;

procedure TNodeManager{$IfDef DCC}<TNode>{$EndIf}.Add(const ANode: TNode);
begin
  if ANode = nil then Exit;
  ANode.Range := FRangeMan.Alloc(ANode.Size);
  ANode.DirtyIndex := -1;
  if ANode.Range = nil then
  begin
      FRangeMan.AddSpace( Max(ANode.Size, Ceil(FRangeMan.Size*1.5)) );
      FRangeMan.Defrag;
      ANode.Range := FRangeMan.Alloc(ANode.Size);
      FDirtyAll := True;
  end
  else
    Invalidate(ANode);
  Assert(ANode.Range <> nil);
  FNodes.Add(ANode, 0);
end;

function TNodeManager{$IfDef DCC}<TNode>{$EndIf}.Del(const ANode: TNode): Boolean;
begin
  Result := False;
  if ANode = nil then Exit;
  Validate(ANode);
  Result := FNodes.Contains(ANode);
  if Result then
    FNodes.Delete(ANode);
end;

function TNodeManager{$IfDef DCC}<TNode>{$EndIf}.Invalidate(const ANode: TNode): Boolean;
begin
  Result := False;
  if ANode = nil then Exit;
  if FDirtyAll then Exit;
  if ANode.DirtyIndex >= 0 then Exit;
  ANode.DirtyIndex := FDirtyNodes.Count;
  FDirtyNodes.Add(ANode);
  Result := True;
end;

procedure TNodeManager{$IfDef DCC}<TNode>{$EndIf}.InvalidateAll;
var i: Integer;
begin
  if FDirtyAll then Exit;
  FDirtyAll := True;
  for i := 0 to FDirtyNodes.Count - 1 do
    FDirtyNodes.Item[i].DirtyIndex := -1;
  FDirtyNodes.Clear;
end;

procedure TNodeManager{$IfDef DCC}<TNode>{$EndIf}.Validate(const ANode: TNode);
var LastIndex: Integer;
begin
  if ANode.DirtyIndex < 0 then Exit;
  LastIndex := FDirtyNodes.Count - 1;
  if ANode.DirtyIndex <> LastIndex then
  begin
    FDirtyNodes.Item[ANode.DirtyIndex] := FDirtyNodes.Item[LastIndex];
    FDirtyNodes.Item[ANode.DirtyIndex].DirtyIndex := ANode.DirtyIndex;
  end;
  ANode.DirtyIndex := -1;
  FDirtyNodes.Delete(LastIndex);
end;

procedure TNodeManager{$IfDef DCC}<TNode>{$EndIf}.ValidateAll;
var
  i: Integer;
begin
  if FDirtyAll then
    FDirtyAll := False
  else
  begin
    for i := 0 to FDirtyNodes.Count - 1 do
      FDirtyNodes.Item[i].DirtyIndex := -1;
    FDirtyNodes.Clear;
  end;
end;

function TNodeManager{$IfDef DCC}<TNode>{$EndIf}.DirtyCount: Integer;
begin
  if FDirtyAll then
    Result := FNodes.Count
  else
    Result := FDirtyNodes.Count;
end;

function TNodeManager{$IfDef DCC}<TNode>{$EndIf}.TotalCount: Integer;
begin
  Result := FNodes.Count;
end;

procedure TNodeManager{$IfDef DCC}<TNode>{$EndIf}.Reset;
begin
  FEnumIndex := 0;
  FNodes.Reset;
end;

function TNodeManager{$IfDef DCC}<TNode>{$EndIf}.NextDirty(out ANode: TNode): Boolean;
var Dummy: Integer;
begin
  if FDirtyAll then
    Result := FNodes.Next(ANode, Dummy)
  else
  begin
    Result := FEnumIndex < FDirtyNodes.Count;
    if Result then
    begin
      ANode := FDirtyNodes[FEnumIndex];
      Inc(FEnumIndex);
    end;
  end;
end;

function TNodeManager{$IfDef DCC}<TNode>{$EndIf}.Next(out ANode: TNode): Boolean;
var Dummy: Integer;
begin
  Result := FNodes.Next(ANode, Dummy);
end;

procedure TNodeManager{$IfDef DCC}<TNode>{$EndIf}.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TGroupHash.Create;
  FDirtyNodes := TGroupList.Create;
  FRangeMan := Create_IRangeManager();
end;

{ TavVBManaged }

function TavVBManaged.DoBuild: Boolean;
  function GetFirstLayout: IDataLayout;
  var node: TVBNode;
  begin
    Result := nil;
    FNodes.Reset;
    if FNodes.Next(node) then
      Result := node.Vert.Layout;
  end;
  procedure SetNodeData(const node: TVBNode; const StrideSize: Integer);
  begin
    Assert(node.Vert.Layout.Size = StrideSize);
    Assert(node.Vert.Data.data <> nil);
    Assert(node.Vert.Data.size = node.Range.Size*StrideSize);
    FbufH.SetSubData(node.Range.Offset*StrideSize, node.Range.Size*StrideSize, node.Vert.Data.data);
  end;
var node  : TVBNode;
    StrideSize: Integer;
begin
  Result := True;
  if Assigned(FbufH) and (FNodes.DirtyCount = 0) then Exit;

  if FbufH = nil then
  begin
    FbufH := Main.Context.CreateVertexBuffer;
    FbufH.Layout := GetFirstLayout;
  end;
  StrideSize := FbufH.Layout.Size;
  if FbufH.Size <> FNodes.RangeManSize*StrideSize then
  begin
    FNodes.InvalidateAll;
    FbufH.AllocMem(FNodes.RangeManSize*StrideSize, nil);
  end;

  FNodes.Reset;
  while FNodes.NextDirty(node) do
    SetNodeData(node, StrideSize);
  FNodes.ValidateAll;
end;

procedure TavVBManaged.SetName(const Value: string);
begin
  inherited SetName(Value);
  if FNodes <> nil then
    FNodes.Name := Value;
end;

function TavVBManaged.HasData: Boolean;
begin
  Result := FNodes.RangeManSize > 0;
end;

function TavVBManaged.Add(const AVert: IVerticesData): IVBManagedHandle;
var node: TVBNode;
begin
  Result := nil;
  if AVert = nil then Exit;
  if AVert.VerticesCount = 0 then Exit;
  if AVert.Layout = nil then Exit;

  node := TVBNode.Create;
  node.Owner := Self;
  node.Vert := AVert;
  FNodes.Add(node);
  if FNodes.DirtyCount > 0 then
    Invalidate;

  Result := node;
end;

procedure TavVBManaged.AfterConstruction;
begin
  inherited AfterConstruction;
  FNodes := TNodes.Create;
end;

destructor TavVBManaged.Destroy;
begin
  FreeAndNil(FNodes);
  inherited Destroy;
end;

{ TavMainRenderChild }

function TavMainRenderChild.CanRegister(target: TavObject): boolean;
begin
  Result := inherited CanRegister(target);
  if not Result then Exit;
  FMain := TavMainRender(target.FindAtParents(TavMainRender));
  Result := Assigned(FMain);
end;

function TavMainRenderChild.Main: TavMainRender;
begin
  Result := FMain;
end;

{ TavFrameBuffer }

function TavFrameBuffer.GetColor(Index: Integer): TavTextureBase;
var ref: IWeakRef;
begin
  Result := nil;
  if (Index >= 0) and (Index < FColors.Count) then
    ref := FColors.Item[Index].tex
  else
    ref := nil;
  if Assigned(ref) then
      Result := TavTextureBase(ref.Obj);
end;

function TavFrameBuffer.GetColorMipLevel(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FColors.Count) then
    Result := FColors.Item[Index].mipLevel
  else
    Result := 0;
end;

function TavFrameBuffer.GetDepth: TavTextureBase;
begin
  Result := nil;
  if Assigned(FDepth.tex) then
    Result := TavTextureBase(FDepth.tex.Obj);
end;

procedure TavFrameBuffer.SetColor(Index: Integer; AValue: TavTextureBase; mipLevel: Integer = 0);
var oldCount: Integer;
    ainfo: TAttachInfo;
    i: Integer;
begin
  oldCount := FColors.Count;
  for i := oldCount to Index do
    FColors.Add(EmptyAttachInfo);
  if Assigned(AValue) then
  begin
    ainfo.tex := AValue.WeakRef;
    ainfo.mipLevel := mipLevel;
    FColors.Item[index] := ainfo;
  end;
  Invalidate;
end;

procedure TavFrameBuffer.SetUAV(Index: Integer; AValue: TavUAV);
var oldCount: Integer;
    i: Integer;
begin
  oldCount := FUAVs.Count;
  for i := oldCount to Index do
    FUAVs.Add(nil);
  if Assigned(AValue) then
    FUAVs.Item[index] := AValue.WeakRef;
  Invalidate;
end;

procedure TavFrameBuffer.SetUAV(Index: Integer; AValue: TavTextureBase);
var oldCount: Integer;
    i: Integer;
begin
  oldCount := FUAVs.Count;
  for i := oldCount to Index do
    FUAVs.Add(nil);
  if Assigned(AValue) then
    FUAVs.Item[index] := AValue.WeakRef;
  Invalidate;
end;

procedure TavFrameBuffer.SetStreamOut(Index: Integer; ABuffer: TavVerticesBase; Offset: Integer);
var oldCount: Integer;
    i: Integer;
    info: TStreamAttachInfo;
begin
  info.buffer := nil;
  info.offset := 0;
  oldCount := FStreams.Count;
  for i := oldCount to Index do
    FStreams.Add(info);
  if Assigned(ABuffer) then
    info.buffer := ABuffer.WeakRef;
  info.offset := Offset;
  FStreams.Item[index] := info;
  Invalidate;
end;

procedure TavFrameBuffer.SetDepth(AValue: TavTextureBase; mipLevel: Integer);
begin
  if Assigned(AValue) then
    FDepth.tex := AValue.WeakRef
  else
    FDepth.tex := nil;
  FDepth.mipLevel := mipLevel;
  Invalidate;
end;

procedure TavFrameBuffer.Clear(index: Integer; color: TVec4);
begin
  FFrameBuf.Clear(index, color);
end;

procedure TavFrameBuffer.ClearDS(depth: Single; clearDepth: Boolean;
  stencil: Integer; clearStencil: Boolean);
begin
  FFrameBuf.ClearDS(depth, clearDepth, stencil, clearStencil);
end;

procedure TavFrameBuffer.ClearUAV(index: Integer; color: TVec4i);
begin
  FFrameBuf.ClearUAV(index, color);
end;

procedure TavFrameBuffer.BlitToWindow(index: Integer);
begin
  FFrameBuf.BlitToWindow(index, FFrameRect, GetRectOfWindow(Main.Window), tfNearest);
end;

procedure TavFrameBuffer.SetFrameRect(AValue: TRectI);
begin
  if (FFrameRect = AValue) then Exit;
  FFrameRect := AValue;
  Invalidate;
end;

procedure TavFrameBuffer.BeforeFree3D;
begin
  inherited BeforeFree3D;
  if Assigned(FFrameBuf) then Invalidate;
  FFrameBuf := nil;
end;

function TavFrameBuffer.DoBuild: Boolean;
  function GetTex(const ainfo: TAttachInfo): TavTextureBase; inline;
  begin
    if Assigned(ainfo.tex) then
      Result := TavTexture(ainfo.tex.Obj)
    else
      Result := nil;
  end;

  function GetStream(const sinfo: TStreamAttachInfo): TavVerticesBase; inline;
  begin
    if Assigned(sinfo.buffer) then
      Result := TavVerticesBase(sinfo.buffer.Obj)
    else
      Result := nil;
  end;

  procedure ResizeTex(tex: TavTextureBase; FrameSize: TVec2i; mipLevel: Integer); //inline;
  var NewTexSize: TVec2i;
      SimpleTex: TavTexture absolute tex;
      MultiTex : TavMultiSampleTexture absolute tex;
  begin
    if tex is TavTexture then
    begin
      if ForcedPOT then
        NewTexSize := NextPow2(FrameSize) * (1 shl mipLevel)
      else
        NewTexSize := FrameSize * (1 shl mipLevel);

      if (SimpleTex.Size.x <> NewTexSize.x) or (SimpleTex.Size.y <> NewTexSize.y) then
        SimpleTex.TexData := EmptyTexData(NewTexSize.x, NewTexSize.y, SimpleTex.TargetFormat, mipLevel > 0);
      SimpleTex.Build;
      Exit;
    end;

    if tex is TavMultiSampleTexture then
    begin
      MultiTex.SetSize(FrameSize.x, FrameSize.y);
      MultiTex.Build;
      Exit;
    end;

    Assert(False, 'Unknown type of texture: '+tex.ClassName);
  end;

var ainfo: TAttachInfo;
    tex: TavTextureBase;
    buf: TavVerticesBase;
    uav: TavUAV;
    sinfo: TStreamAttachInfo;
    i: Integer;
    FrameSize: TVec2i;
begin
  Result := inherited DoBuild;
  if FFrameBuf = nil then
    FFrameBuf := Main.Context.CreateFrameBuffer
  else
    FFrameBuf.ClearColorList;

  FrameSize := Max(Vec(FFrameRect.Left, FFrameRect.Top), Vec(FFrameRect.Right, FFrameRect.Bottom));

  for i := 0 to FColors.Count - 1 do
  begin
    ainfo := FColors[i];
    tex := GetTex(ainfo);
    if Assigned(tex) then
    begin
      ResizeTex(tex, FrameSize, ainfo.mipLevel);
      FFrameBuf.SetColor(i, tex.FTexH, ainfo.mipLevel);
    end
    else
      FFrameBuf.SetColor(i, nil, ainfo.mipLevel);
  end;

  for i := 0 to FUAVs.Count - 1 do
  begin
    if FUAVs[i].Obj is TavUAV then
    begin
      uav := TavUAV(FUAVs[i].Obj);
      uav.Build;
      if Assigned(uav) then
        FFrameBuf.SetUAV(i, uav.FUAVBufH)
      else
        FFrameBuf.SetUAV(i, nil);
    end
    else
    if FUAVs[i].Obj is TavTextureBase then
    begin
      tex := TavTextureBase(FUAVs[i].Obj);
      tex.Build;
      if Assigned(tex) then
        FFrameBuf.SetUAVTex(i, tex.FTexH)
      else
        FFrameBuf.SetUAVTex(i, nil);
    end
    else
      Assert(False);
  end;

  for i := 0 to FStreams.Count - 1 do
  begin
    sinfo := FStreams[i];
    buf := GetStream(sinfo);
    if buf <> nil then
      buf.Build;
    FFrameBuf.SetStreamOut(i, buf.FbufH, sinfo.offset);
  end;

  tex := GetTex(FDepth);
  if Assigned(tex) then
  begin
    ResizeTex(tex, FrameSize, FDepth.mipLevel);
    FFrameBuf.SetDepthStencil(tex.FTexH, FDepth.mipLevel);
  end
  else
    FFrameBuf.SetDepthStencil(nil, FDepth.mipLevel);
end;

function TavFrameBuffer.Select(UpdateProjMatrix: Boolean): TavFrameBuffer;
begin
  Build;
  Main.Context.States.ViewPort := FFrameRect;
  if UpdateProjMatrix then
    Main.Projection.Aspect := (FFrameRect.Bottom - FFrameRect.Top)/(FFrameRect.Right - FFrameRect.Left);
  FFrameBuf.Select;
  Result := Main.SetActiveFrameBuffer(Self);
end;

procedure TavFrameBuffer.ClearColorList;
begin
  FColors.Clear;
  Invalidate;
end;

procedure TavFrameBuffer.ClearUAVList;
begin
  FUAVs.Clear;
  Invalidate;
end;

procedure TavFrameBuffer.AfterConstruction;
begin
  inherited AfterConstruction;
  FColors := TColorsList.Create;
  FUAVs := TUAVList.Create;
  FStreams := TStreamsList.Create;
end;

{ TavTexture }

procedure TavTexture.SetTexData(AValue: ITextureData);
begin
  if FTexData = AValue then Exit;
  FTexData := AValue;
  Invalidate;
end;

procedure TavTexture.SetAutoGenerateMips(AValue: Boolean);
begin
  if FAutoGenerateMips = AValue then Exit;
  FAutoGenerateMips := AValue;
  Invalidate;
end;

procedure TavTexture.SetForcedArray(AValue: Boolean);
begin
  if FForcedArray = AValue then Exit;
  if Assigned(FTexH) then Invalidate;
  FForcedArray := AValue;
end;

function TavTexture.DoBuild: Boolean;
var MipInfo : ITextureMip;
    i, j, n: Integer;
begin
  if Assigned(FTexData) then
  begin
    if FTexH = nil then FTexH := Main.Context.CreateTexture;
    FTexH.TargetFormat := FTargetFormat;
    FTexH.sRGB := FsRGB;
    FImageSize.x := FTexData.Width;
    FImageSize.y := FTexData.Height;

    if FForcedPOT then
      FTexH.AllocMem(NextPow2(FTexData.Width), NextPow2(FTexData.Height), FTexData.ItemCount, (FTexData.MipsCount>1) or FAutoGenerateMips, FForcedArray)
    else
      FTexH.AllocMem(FTexData.Width, FTexData.Height, FTexData.ItemCount, (FTexData.MipsCount>1) or FAutoGenerateMips, FForcedArray);

    for i := 0 to FTexData.ItemCount - 1 do
    begin
      n := FTexData.MipCount(i);
      if FAutoGenerateMips then
        n := 1;
      for j := 0 to n - 1 do
      begin
        MipInfo := FTexData.MipData(i, j);
        FTexH.SetMipImage(0, 0, MipInfo.Width, MipInfo.Height, j, i, FTexData.Format, MipInfo.Data);
      end;
    end;
    if FAutoGenerateMips then
      FTexH.GenerateMips;
  end;
  if FDropLocalAfterBuild then FTexData := nil;
  Result := True;
end;

function TavTexture.ImageSize: TVec2;
begin
  Result := FImageSize;
end;

procedure TavTexture.AfterConstruction;
begin
  inherited AfterConstruction;
end;

{ TavCursor }

function TavCursor.GetWindowCur: TVec2;
begin
  UpdateCursor;
  Result := FWindowCur;
end;

procedure TavCursor.UpdateCursor;
var vCur: TVec2;
    v: TVec4;
    m: TMat4;
begin
  vCur := GetCursorPos(FMain.Window, true, true);
  if (vCur = FWindowCur) and (FMain.Camera.UpdateID = FCameraUpdateID) and (FMain.Projection.UpdateID = FProjectionUpdateID) then Exit;
  FWindowCur := vCur;

  m := FMain.Projection.uMatrix * FMain.Camera.uMatrix;
  v := Vec(vCur.x, vCur.y, FMain.Projection.DepthRange.x, 1);
  v := v * m;
  FFrom := v.xyz / v.w;
  v := Vec(vCur.x, vCur.y, FMain.Projection.DepthRange.y, 1);
  v := v * m;
  FAt := v.xyz / v.w;

  FRay.Pnt := FFrom;
  FRay.Dir := FAt - FFrom;

  FCameraUpdateID := FMain.Camera.UpdateID;
  FProjectionUpdateID := FMain.Projection.UpdateID;
end;

function TavCursor.GetAt: TVec3;
begin
  UpdateCursor;
  Result := FAt;
end;

function TavCursor.GetFrom: TVec3;
begin
  UpdateCursor;
  Result := FFrom;
end;

function TavCursor.GetRay: TLine;
begin
  UpdateCursor;
  Result := FRay;
end;

function TavCursor.CanRegister(target: TavObject): boolean;
begin
  Result := inherited CanRegister(target);
  Result := Result and (target is TavMainRender);
  FMain := TavMainRender(target);
end;

{ TavProjection }

procedure TavProjection.SetMatrix(const AValue: TMat4);
begin
  if FMatrix = AValue then Exit;
  FMatrix := AValue;
  FuMatrix := Inv(FMatrix);
  Inc(FUpdateID);
end;

procedure TavProjection.SetAspect(AValue: single);
begin
  if FAspect = AValue then Exit;
  FAspect := AValue;
  UpdateMatrix;
end;

procedure TavProjection.SetDepthRange(const AValue: TVec2);
begin
  if FDepthRange = AValue then Exit;
  FDepthRange := AValue;
  UpdateMatrix;
end;

procedure TavProjection.SetFarPlane(AValue: single);
begin
  if FFarPlane = AValue then Exit;
  FFarPlane := AValue;
  UpdateMatrix;
end;

procedure TavProjection.SetFov(AValue: single);
begin
  if FFov = AValue then Exit;
  FFov := AValue;
  UpdateMatrix;
end;

procedure TavProjection.SetNearPlane(AValue: single);
begin
  if FNearPlane = AValue then Exit;
  FNearPlane := AValue;
  UpdateMatrix;
end;

procedure TavProjection.SetOrtho(AValue: Boolean);
begin
  if FOrtho = AValue then Exit;
  FOrtho := AValue;
  UpdateMatrix;
end;

procedure TavProjection.SetOrthoHeight(AValue: single);
begin
  if FOrthoHeight = AValue then Exit;
  FOrthoHeight := AValue;
  UpdateMatrix;
end;

procedure TavProjection.UpdateMatrix;

  function CalcPerspectiveMatrix: TMat4;
  var w, h, Q: Single;
      DepthSize: Single;
  begin
    h := (cos(fFOV/2)/sin(fFOV/2));
    w := fAspect * h;
    Q := 1.0/(NearPlane - FarPlane);
    DepthSize := DepthRange.y - DepthRange.x;

    ZeroClear(Result, SizeOf(Result));
    Result.f[0, 0] := w;
    Result.f[1, 1] := h;
    Result.f[2, 2] := DepthRange.x - DepthSize * FarPlane * Q;
    Result.f[2, 3] := 1.0;
    Result.f[3, 2] := DepthSize * NearPlane * FarPlane * Q;
  end;

  function CalcOrthoMatrix: TMat4;
  var w, h, Q: Single;
      DepthSize: Single;
  begin
    h := 2 / FOrthoHeight;
    w := h * FAspect;
    Q := 1.0 / (FarPlane - NearPlane);
    DepthSize := DepthRange.y - DepthRange.x;
    Result := IdentityMat4;
    Result.f[0, 0] := w;
    Result.f[1, 1] := h;
    Result.f[2, 2] := DepthSize * Q;
    Result.f[3, 2] := DepthRange.x  - DepthSize * NearPlane * Q;
  end;

begin
  if FOrtho then
    FMatrix := CalcOrthoMatrix
  else
    FMatrix := CalcPerspectiveMatrix;
  FuMatrix := Inv(FMatrix);
  Inc(FUpdateID);
end;

function TavProjection.DepthRangeMinMax: TVec2;
const APIToRange: array [T3DAPI] of TVec2 = ( {OGL}(x: -1; y: 1), {DX11} (x: 0; y: 1), {apiDX11_WARP} (X: 0; y: 1) );
begin
  Result := Vec(0.0, 0.0);
  if Parent is TavMainRender then
    Result := APIToRange[TavMainRender(Parent).ActiveApi];
end;

procedure TavProjection.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TavProjection.EndUpdate;
begin
  Dec(FUpdating);
  If FUpdating = 0 Then UpdateMatrix;
end;

constructor TavProjection.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  FFov := PI/2;
  FAspect := 3/4;
  FNearPlane := 1;
  FFarPlane := 703.62;
  FOrthoHeight := 100;
  FDepthRange := DepthRangeMinMax;
  UpdateMatrix;
end;

{ TavCamera }

procedure TavCamera.SetMatrix(AValue: TMat4);
begin
  FMatrix := AValue;
  FuMatrix := Inv(FMatrix);
  Inc(FUpdateID);
end;

procedure TavCamera.StopPlaying;
begin
  FPlaying := False;
end;

procedure TavCamera.SetEye(const Value: TVec3);
begin
  if not Equal(FEye, Value) then
  begin
    FEye := Value;
    UpdateMatrix;
    StopPlaying;
  end;
end;

procedure TavCamera.SetAt(const Value: TVec3);
begin
  if not Equal(FAt, Value) then
  begin
    FAt := Value;
    UpdateMatrix;
    StopPlaying;
  end;
end;

procedure TavCamera.SetUp(const Value: TVec3);
begin
  if not Equal(FUp, Value) then
  begin
    FUp := Value;
    UpdateMatrix;
    StopPlaying;
  end;
end;

procedure TavCamera.UpdateMatrix;
begin
  SetViewMatrix(FMatrix, FEye, FAt, FUp, True);
  FuMatrix := Inv(FMatrix);
  Inc(FUpdateID);
end;

function TavCamera.GetViewDir: TLine;
begin
  Result.Pnt := At;
  Result.Dir := Eye - At;
end;

procedure TavCamera.EMUps(var msg: TavMessage);
  function Cmp(const V1, V2: TVec3; const k: Single): Boolean;
  var lbase: Single;
      e: Single;
  begin
      lbase := Len(V2);
      e := k * EPS;
      If Abs(Len(V2)-Len(V1)) < lbase * e Then
          Result := True
      Else
          Result := False;
  end;
var spd: Single;
    k: Single;
    i: Integer;
begin
  for i := 0 to msg.param - 1 do
  begin
    if not FPlaying then Exit;
    spd := Min(1, abs(FPlaySpeed));
    FEye := FEye + (FTargetEye - FEye) * spd;
    FAt := FAt + (FTargetAt - FAt) * spd;
    FUp := FUp + (FTargetUp - FUp) * spd;
    k := 1/spd;
    UpdateMatrix;
    if Cmp(FEye, FTargetEye, k) and Cmp(FAt, FTargetAt, k) and Cmp(FUp, FTargetUp, k) then StopPlaying;
  end;
end;

procedure TavCamera.SetVectors(const Aeye, Aat, Aup: TVec3);
begin
  FEye := Aeye;
  FAt := Aat;
  FUp := Aup;
  UpdateMatrix;
  StopPlaying;
end;

procedure TavCamera.MoveForward(step: single);
var v:TVec3;
begin
  if abs(step) < EPS then Exit;
  StopPlaying;
  v := SetLen(FAt - FEye, step);
  FEye := FEye + v;
  FAt := FAt + v;
  UpdateMatrix;
end;

procedure TavCamera.MoveBack(step: single);
var v:TVec3;
begin
  if abs(step) < EPS then Exit;
  StopPlaying;
  v := SetLen(FAt - FEye, -step);
  FEye := FEye + v;
  FAt := FAt + v;
  UpdateMatrix;
end;

procedure TavCamera.MoveLeft(step: single);
var v:TVec3;
begin
  if abs(step) < EPS then Exit;
  StopPlaying;
  v := SetLen(Cross(FUp, FAt - FEye), -step);
  FEye := FEye + v;
  FAt := FAt + v;
  UpdateMatrix;
end;

procedure TavCamera.MoveRight(step: single);
var v:TVec3;
begin
  if abs(step) < EPS then Exit;
  StopPlaying;
  v := SetLen(Cross(FUp, FAt - FEye), step);
  FEye := FEye + v;
  FAt := FAt + v;
  UpdateMatrix;
end;

procedure TavCamera.MoveUp(step: single);
var v: TVec3;
begin
  if abs(step) < EPS then Exit;
  StopPlaying;
  v := SetLen(FUp, step);
  FEye := FEye + v;
  FAt := FAt + v;
  UpdateMatrix;
end;

procedure TavCamera.MoveDown(step: single);
var v: TVec3;
begin
  if abs(step) < EPS then Exit;
  StopPlaying;
  v := SetLen(FUp, -step);
  FEye := FEye + v;
  FAt := FAt + v;
  UpdateMatrix;
end;

procedure TavCamera.MoveDeep(step: single);
var v: TVec3;
begin
  if abs(step) < EPS then Exit;
  StopPlaying;
  v := SetLen(FAt - FEye, step);
  FEye := FEye + v;
  UpdateMatrix;
end;

procedure TavCamera.RotateEyeHorizontal(angle: single);
var rotor: TQuat;
    dirvector: TVec3;
begin
  if abs(angle) < EPS then exit;
  StopPlaying;
  rotor := Quat(Normalize(FUp), angle);
  dirvector := FAt - FEye;
  dirvector := rotor * dirvector;
  FAt := FEye + dirvector;
  UpdateMatrix;
end;

procedure TavCamera.RotateEyeVertical(angle: single);
var dirvector,right,right2:TVec3;
    rotor: TQuat;
begin
  if abs(angle) < EPS then exit;
  StopPlaying;
  dirvector := FAt - FEye;
  right := Cross(dirvector, FUp);
  if Len(right) = 0 then Exit else right := Normalize(right);
  rotor := Quat(right, angle);
  dirvector := rotor * dirvector;
  right2 := Cross(dirvector, FUp);

  if Dot(right, right2)<0 then
    begin
      if Dot(dirvector, FUp)>0 then
        dirvector := Normalize(FUp) * Len(dirvector)
      else
        dirvector := Normalize(FUp) * (-Len(dirvector));
      rotor := Quat(right, -sign(angle)*EPS*2);
      dirvector := rotor * dirvector;
    end;
  FAt := FEye + dirvector;

  UpdateMatrix;
end;

procedure TavCamera.RotateAtHorizontal(angle: single);
var rotor: TQuat;
    dirvector: TVec3;
begin
  if abs(angle) < EPS then exit;
  StopPlaying;
  rotor := Quat(Normalize(FUp), angle);
  dirvector := FEye - FAt;
  dirvector := rotor * dirvector;
  FEye := FAt + dirvector;
  UpdateMatrix;
end;

procedure TavCamera.RotateAtVertical(angle: single);
var dirvector, right, right2: TVec3;
    rotor: TQuat;
begin
  if abs(angle) < EPS then exit;
  StopPlaying;
  dirvector := FEye - FAt;
  right := Cross(dirvector, FUp);
  if Len(right) = 0 then exit else right := Normalize(right);
  rotor := Quat(right, angle);
  dirvector := rotor * dirvector;
  right2 := Cross(dirvector, FUp);

  if Dot(right, right2)<0 then
    begin
      if Dot(dirvector, FUp)>0 then
        dirvector := Normalize(FUp) * Len(dirvector)
      else
        dirvector := Normalize(FUp) * (-Len(dirvector));
      rotor := Quat(right, -sign(angle)*EPS*5);
      dirvector := rotor * dirvector;
    end;
  FEye := FAt + dirvector;

  UpdateMatrix;
end;

procedure TavCamera.Play(targetEye, targetAt, targetUp: TVec3; speedN: single;
  PlayType: byte);
begin
  FPlaying := true;
  FTargetEye := targetEye;
  FTargetAt := targetAt;
  FTargetUp := targetUp;
  FPlaySpeed := max(EPS, speedN);
  FPlayType := PlayType;
end;

procedure TavCamera.BeginUpdate;
begin
  Inc(FUpdating);
end;

procedure TavCamera.EndUpdate;
begin
  Dec(FUpdating);
  If FUpdating = 0 Then UpdateMatrix;
end;

constructor TavCamera.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  SetVectors(Vec(0.0, 0.0, -3.62131), Vec(0.0, 0.0, 0.0), Vec(0.0, 1.0, 0.0));
end;

{ TavMainRender }

procedure TavMainRender.SetWindow(AValue: TWindow);
begin
  if FWindow = AValue then Exit;
  if IsValidWindow(FWindow) then
    UnregisterHandler(Self, FWindow);
  FWindow := AValue;
  if IsValidWindow(FWindow) then
    RegisterHandler(Self, FWindow);
  if Inited3D then
  begin
    Free3D;
    if IsValidWindow(FWindow) then
      Init3D(FActiveAPI);
  end;
end;

procedure TavMainRender.AfterInit3D_Broadcast;
var msg: TavMessage;
begin
  msg.msg := EM_3D_AFTER_INIT;
  msg.param := 0;
  msg.result := False;
  msg.sender := Self;
  BroadcastRecursive(msg);
end;

procedure TavMainRender.BeforeFree3D_Broadcast;
var msg: TavMessage;
begin
  msg.msg := EM_3D_BEFORE_FREE;
  msg.param := 0;
  msg.result := False;
  msg.sender := Self;
  BroadcastRecursive(msg);
end;

procedure TavMainRender.AfterFree3D_Broadcast;
var msg: TavMessage;
begin
  msg.msg := EM_3D_AFTER_FREE;
  msg.param := 0;
  msg.result := False;
  msg.sender := Self;
  BroadcastRecursive(msg);
end;

procedure TavMainRender.EMWindowDestroy(var msg: TavMessage);
begin
  Window := NOTWINDOW;
end;

procedure TavMainRender.ProcessTimerEvents;
var UpdateCount: Integer;
    msg: TavMessage;
begin
  UpdateCount := (GetTime64 - FLastTime) div FUPS;
  if UpdateCount = 0 then Exit;
  FLastTime := FLastTime + Int64(FUPS) * UpdateCount;
  msg.msg := EM_UPS;
  msg.param := UpdateCount;
  msg.sender := Self;
  msg.result := False;
  Broadcast(msg);
end;

function TavMainRender.Time64: Int64;
begin
  Result := GetTime64;
end;

function TavMainRender.Time: Double;
begin
  Result := GetTime;
end;

function TavMainRender.BindTime64: Int64;
begin
  Result := FBindTime;
end;

function TavMainRender.GetWindow: TWindow;
begin
  Result := FWindow;
end;

function TavMainRender.GetWindowSize: TVec2i;
var rct: TRectI;
begin
  if IsValidWindow(FWindow) then
  begin
    avPlatform.GetRectOfWindow(FWindow, rct.Left, rct.Top, rct.Right, rct.Bottom);
    Result := rct.Size;
  end
  else
    Result := Vec(0,0);
end;

function TavMainRender.GetActiveProgram: TavProgram;
begin
  Result := FActiveProgram;
end;

function TavMainRender.ActiveFrameBuffer: TavFrameBuffer;
begin
  Result := FActiveFrameBuffer;
end;

procedure TavMainRender.SetActiveProgram(AValue: TavProgram);
begin
  if FActiveProgram <> AValue then
  begin
    FActiveProgram := AValue;
    if Assigned(FActiveProgram) then
      FActiveProgram.Select()
    else
      if Assigned(FContext) then
        FContext.ActiveProgram := nil;
  end;
end;

function TavMainRender.SetActiveFrameBuffer(const AFBO: TavFrameBuffer): TavFrameBuffer;
begin
  Result := FActiveFrameBuffer;
  FActiveFrameBuffer := AFBO;
end;

function TavMainRender.ActiveApi: T3DAPI;
begin
  Result := FActiveAPI;
end;

function TavMainRender.Inited3D: Boolean;
begin
  Result := Assigned(FContext);
end;

procedure TavMainRender.Init3D(api: T3DAPI);
begin
  try
      if not Inited3D then
      begin
        case api of
            apiOGL :
              begin
                FContext := TContext_OGL.Create(FWindow);
                FProjection.DepthRange := Vec(-1.0, 1.0);
              end;
            apiDX11, apiDX11_WARP:
              begin
                FContext := TContext_DX11.Create(FWindow, api = apiDX11_WARP);
                FProjection.DepthRange := Vec(0, 1.0);
              end;
        end;
        FActiveApi := api;

        if assigned(FContext) then
        begin
          if Bind then
          begin
//            States.Viewport := Rect(0, 0, FWidth, FHeight);
            AfterInit3D_Broadcast;
            Unbind;
          end;
          LogLn('Render context created');
        end
        else
        begin
          LogLn('Creating render context failed');
        end;
      end;
  except
      on e: ECreateContextFailed do
        raise EavError.Create('ECreateContextFailed with message: ' + e.Message);
  end;
end;

procedure TavMainRender.Free3D;
begin
  if Inited3D then
  begin
    if FContext.Bind then
    begin
      ActiveProgram := nil;
      BeforeFree3D_Broadcast;
      Unbind;
    end;
    FContext := nil;
    LogLn('Render context released');
    AfterFree3D_Broadcast;
  end;
end;

function TavMainRender.Context: IRenderContext;
begin
  Result := FContext;
end;

function TavMainRender.States: IRenderStates;
begin
  Result := FContext.States;
end;

function TavMainRender.Bind: boolean;
begin
  Result := FContext.Bind;
  FCursor.UpdateCursor;
  ProcessTimerEvents;
  FBindTime := Time64;
end;

function TavMainRender.Binded: boolean;
begin
  Result := FContext.Binded;
end;

procedure TavMainRender.Unbind;
begin
  FContext.Unbind;
  FActiveProgram := nil;
end;

procedure TavMainRender.Clear(const color  : TVec4;      doColor  : Boolean = True;
                              depth  : Single = 1; doDepth  : Boolean = False;
                              stencil: Byte   = 0; doStencil: Boolean = False);
begin
  FContext.Clear(color, doColor, depth, doDepth, stencil, doStencil);
end;

procedure TavMainRender.Flush;
begin
  FContext.Flush;
end;

procedure TavMainRender.Present;
begin
  FContext.Present;
  Inc(FFrameID);
end;

procedure TavMainRender.InvalidateWindow;
begin
  avPlatform.InvalidateWindow(FWindow, False);
end;

procedure TavMainRender.Dispatch(var message);
begin
  inherited Dispatch(message);
  Broadcast(message);
end;

constructor TavMainRender.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  FWindow := NOTWINDOW;
  FCamera := TavCamera.Create(Self);
  FProjection := TavProjection.Create(Self);
  FCursor := TavCursor.Create(Self);
  FUPS := 20;
end;

destructor TavMainRender.Destroy;
begin
  Free3D;
  Window := NOTWINDOW;
  inherited Destroy;
end;

{ TavIB }

procedure TavIB.SetIndices(const AValue: IIndicesData);
begin
  if FInd = AValue then Exit;
  FInd := AValue;
  PrimType := AValue.PrimType;
  Invalidate;
end;

function TavIB.DoBuild: Boolean;
begin
  if Assigned(FInd) then
  begin
    if FbufH = nil then FbufH := Main.Context.CreateIndexBuffer;
    FbufH.AllocMem(FInd.Data.size, FInd.Data.data);
    case FInd.IndexSize of
      2: FbufH.IndexSize := TIndexSize.Word;
      4: FbufH.IndexSize := TIndexSize.DWord;
    else
      Assert(False,'Incorrect index size');
    end;
  end;
  if FDropLocalAfterBuild then FInd := nil;
  Result := True;
end;

{ TavVB }

procedure TavVB.SetVert(AValue: IVerticesData);
begin
  if FVert = AValue then Exit;
  FVert := AValue;
  Invalidate;
end;

function TavVB.DoBuild: Boolean;
begin
  if Assigned(FVert) then
  begin
    if FbufH = nil then FbufH := Main.Context.CreateVertexBuffer;
    FbufH.AllocMem(FVert.Data.size, FVert.Data.data);
    FbufH.Layout := FVert.Layout;
  end;
  FBuildedPrimType := FPrimType;
  if FDropLocalAfterBuild then FVert := nil;
  Result := True;
end;

{ TavIndicesBase }

procedure TavIndicesBase.BeforeFree3D;
begin
  inherited BeforeFree3D;
  if Assigned(FbufH) then Invalidate;
  FbufH := nil;
end;

function TavIndicesBase.BuildedIndCount: Integer;
begin
  if FbufH = nil then
    Exit(0)
  else
    Result := FbufH.IndicesCount;
end;

{ TavVerticesBase }

procedure TavVerticesBase.BeforeFree3D;
begin
  inherited BeforeFree3D;
  if Assigned(FbufH) then Invalidate;
  FbufH := nil;
end;

function TavVerticesBase.BuildedVertCount: Integer;
begin
  if FbufH = nil then
    Exit(0)
  else
    Result := FbufH.VertexCount;
end;

{ TavRes }

procedure TavRes.AfterInit3D;
begin

end;

procedure TavRes.BeforeFree3D;
begin

end;

procedure TavRes.AfterFree3D;
begin

end;

function TavRes.DoBuild: Boolean;
begin
  Result := True;
end;

procedure TavRes.EM3DAfterFree(var msg: TavMessage);
begin
  AfterFree3D;
end;

procedure TavRes.EM3DBeforeFree(var msg: TavMessage);
begin
  BeforeFree3D;
end;

procedure TavRes.EM3DAfterInit(var msg: TavMessage);
begin
  AfterInit3D;
end;

procedure TavRes.Build;
begin
  if not FDirty then Exit;
  FDirty := not DoBuild;
end;

procedure TavRes.Invalidate;
begin
  FDirty := True;
end;

function TavRes.Valid: Boolean;
begin
  Result := not FDirty;
end;

procedure TavRes.AfterConstruction;
begin
  inherited AfterConstruction;
  if Main.Inited3D then
    AfterInit3D;
end;

constructor TavRes.Create(AParent: TavObject);
begin
  inherited Create(AParent);
end;

{ TavProgram }

procedure TavProgram.BeforeFree3D;
begin
  inherited BeforeFree3D;
  if Assigned(FProgram) then Invalidate;
  FProgram := nil;
  FCameraUpdateID := -1;
  FProjectionUpdateID := -1;
  FFlipUpdated := False;
end;

function TavProgram.DoBuild: Boolean;
var
  i: TUniformMatrices;
  pInfo: PTypeInfo;
  newSrc: String;
begin
  if not Assigned(FProgram) then
    FProgram := Main.Context.CreateProgram;

  if FFromRes then
    newSrc := API_Prefix[Main.ActiveApi] + FSrc
  else
    newSrc := FSrcPath+'\'+API_Prefix[Main.ActiveApi]+FSrc+API_Suffix[Main.ActiveApi];

  FProgram.Load(newSrc, FFromRes, FOutLayout);

  pInfo := TypeInfo(TUniformMatrices);
  for i := Low(TUniformMatrices) to High(TUniformMatrices) do
    FUniformsMatrices[i] := GetUniformField(GetEnumName(pInfo, Integer(I)));

  FCameraUpdateID := -1;
  FProjectionUpdateID := -1;
  Result := True;
end;

procedure TavProgram.UpdateMatrices;
var i: TUniformMatrices;
    MValid: array [TUniformMatrices] of Boolean;
begin
  MValid[V_Matrix]         := FCameraUpdateID = Main.Camera.UpdateID;
  MValid[V_InverseMatrix]  := MValid[V_Matrix];
  MValid[P_Matrix]         := FProjectionUpdateID = Main.Projection.UpdateID;
  MValid[P_InverseMatrix]  := MValid[P_Matrix];
  MValid[VP_Matrix]        := MValid[V_Matrix] and MValid[P_Matrix];
  MValid[VP_InverseMatrix] := MValid[VP_Matrix];
  MValid[FBOFlip] := FFlipUpdated;

  for i := Low(TUniformMatrices) to High(TUniformMatrices) do
    if (not MValid[i]) and Assigned(FUniformsMatrices[i]) then
    case i of
      VP_Matrix       : SetUniform(FUniformsMatrices[i], Main.Camera.Matrix * Main.Projection.Matrix);
      VP_InverseMatrix: SetUniform(FUniformsMatrices[i], Main.Projection.uMatrix * Main.Camera.uMatrix);
      V_Matrix        : SetUniform(FUniformsMatrices[i], Main.Camera.Matrix);
      V_InverseMatrix : SetUniform(FUniformsMatrices[i], Main.Camera.uMatrix);
      P_Matrix        : SetUniform(FUniformsMatrices[i], Main.Projection.Matrix);
      P_InverseMatrix : SetUniform(FUniformsMatrices[i], Main.Projection.uMatrix);
      FBOFlip         : if Main.ActiveApi = apiOGL then
                          SetUniform(FUniformsMatrices[i], Vec(1.0,-1.0))
                        else
                          SetUniform(FUniformsMatrices[i], Vec(1.0, 1.0));
    end;
  FCameraUpdateID := Main.Camera.UpdateID;
  FProjectionUpdateID := Main.Projection.UpdateID;
  FFlipUpdated := True;
end;

procedure TavProgram.Select(const APatchSize: Integer);
begin
  if Main.ActiveProgram = Self then Exit;
  Build;
  Main.ActiveProgram := Self;
  FProgram.Select(APatchSize);
  UpdateMatrices;
end;

procedure TavProgram.SetAttributes(Model: TavVerticesBase; ModelIndices: TavIndicesBase; Instance: TavVerticesBase; InstanceStepRate: Integer);
var SelectedVertices, SelectedInstances: IctxVetexBuffer;
    SelectedIndices: IctxIndexBuffer;
begin
  if Assigned(Model) then
  begin
    Model.Build;
    SelectedVertices := Model.FbufH;
    FDefaultCullMode := Model.CullMode;
    FDefaultPrimType := Model.PrimType;
    FSelectedVertexCount := Model.BuildedVertCount;
  end
  else
    SelectedVertices := nil;

  if Assigned(Instance) then
  begin
    Instance.Build;
    SelectedInstances := Instance.FbufH
  end
  else
    SelectedInstances := nil;

  if Assigned(ModelIndices) then
  begin
    ModelIndices.Build;
    SelectedIndices := ModelIndices.FbufH;
    if SelectedIndices <> nil then
    begin
      FDefaultCullMode := ModelIndices.CullMode;
      FDefaultPrimType := ModelIndices.PrimType;
    end;
    FSelectedIndexCount := ModelIndices.BuildedIndCount;
  end
  else
    SelectedIndices := nil;

  FIsIndexedBinded := Assigned(SelectedIndices);

  FProgram.SetAttributes(SelectedVertices, SelectedInstances, SelectedIndices, InstanceStepRate);
end;

function TavProgram.GetUniformField(const AName: string): TUniformField;
begin
  Result := FProgram.GetUniformField(AName);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const value: integer);
begin
  FProgram.SetUniform(Field, value);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const value: single);
begin
  FProgram.SetUniform(Field, value);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const v: TVec2);
begin
  FProgram.SetUniform(Field, v);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const v: TVec3);
begin
  FProgram.SetUniform(Field, v);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const v: TVec4);
begin
  FProgram.SetUniform(Field, v);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const values: TSingleArr);
begin
  FProgram.SetUniform(Field, values);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const v: TVec4arr);
begin
  FProgram.SetUniform(Field, v);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const m: TMat4);
begin
  FProgram.SetUniform(Field, m);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const tex: TavTextureBase; const sampler: TSamplerInfo);
begin
  if tex = nil then Exit;
  tex.Build;
  FProgram.SetUniform(Field, tex.FTexH, sampler);
end;

procedure TavProgram.SetUniform(const Field: TUniformField; const buf: TavStructuredBase);
begin
  if buf = nil then Exit;
  buf.Build;
  FProgram.SetUniform(Field, buf.FbufH);
end;

procedure TavProgram.SetUniform(const AName: string; const value: integer);
begin
  FProgram.SetUniform(GetUniformField(AName), integer(value));
end;

procedure TavProgram.SetUniform(const AName: string; const value: single);
begin
  FProgram.SetUniform(GetUniformField(AName), value);
end;

procedure TavProgram.SetUniform(const AName: string; const v: TVec2);
begin
  FProgram.SetUniform(GetUniformField(AName), v);
end;

procedure TavProgram.SetUniform(const AName: string; const v: TVec3);
begin
  FProgram.SetUniform(GetUniformField(AName), v);
end;

procedure TavProgram.SetUniform(const AName: string; const v: TVec4);
begin
  FProgram.SetUniform(GetUniformField(AName), v);
end;

procedure TavProgram.SetUniform(const AName: string; const values: TSingleArr);
begin
  FProgram.SetUniform(GetUniformField(AName), values);
end;

procedure TavProgram.SetUniform(const AName: string; const v: TVec4arr);
begin
  FProgram.SetUniform(GetUniformField(AName), v);
end;

procedure TavProgram.SetUniform(const AName: string; const m: TMat4);
begin
  FProgram.SetUniform(GetUniformField(AName), m);
end;

procedure TavProgram.SetUniform(const AName: string; const tex: TavTextureBase; const sampler: TSamplerInfo);
begin
  if tex = nil then Exit;
  tex.Build;
  FProgram.SetUniform(GetUniformField(AName), tex.FTexH, sampler);
end;

procedure TavProgram.SetUniform(const AName: string; const buf: TavStructuredBase);
begin
  if buf = nil then Exit;
  buf.Build;
  FProgram.SetUniform(GetUniformField(AName), buf.FbufH);
end;

procedure TavProgram.SetComputeUAV(const Index: Integer; const uav: TavUAV; const initial: Integer);
begin
  if uav = nil then
  begin
    FProgram.SetComputeUAV(Index, nil, initial);
  end
  else
  begin
    uav.Build;
    FProgram.SetComputeUAV(Index, uav.FUAVBufH, initial);
  end;
end;

procedure TavProgram.Load(const AProgram: string; FromResource: boolean = false; const AProgramPath: string = '');
begin
  FSrc := AProgram;
  FFromRes := FromResource;
  FSrcPath := AProgramPath;
  FOutLayout := Nil;
  FCameraUpdateID := -1;
  FProjectionUpdateID := -1;
  FFlipUpdated := False;
  Invalidate;
end;

procedure TavProgram.Load(const AProgram: string; const AStremOutputLayout: IDataLayout; FromResource: boolean = false; const AProgramPath: string = '');
begin
  FSrc := AProgram;
  FFromRes := FromResource;
  FSrcPath := AProgramPath;
  FOutLayout := AStremOutputLayout;
  FCameraUpdateID := -1;
  FProjectionUpdateID := -1;
  FFlipUpdated := False;
  Invalidate;
end;

procedure TavProgram.Draw(InstanceCount: Integer; Start: integer;
  Count: integer; BaseVertex: integer; BaseInstance: Integer);
begin
  Draw(FDefaultPrimType, FDefaultCullMode, FIsIndexedBinded, InstanceCount, Start, Count, BaseVertex, BaseInstance);
end;

procedure TavProgram.Draw(PrimTopology: TPrimitiveType; CullMode: TCullingMode;
  IndexedGeometry: Boolean; InstanceCount: Integer; Start: integer;
  Count: integer; BaseVertex: integer; BaseInstance: Integer);
var ICount: Integer;
begin
  if Count = -1 then
  begin
    if IndexedGeometry then
      ICount := FSelectedIndexCount - Start
    else
      ICount := FSelectedVertexCount - Start;
  end
  else
  begin
      ICount := Count;
  end;

  FProgram.Draw(PrimTopology, CullMode, IndexedGeometry, InstanceCount, Start, ICount, BaseVertex, BaseInstance);
end;

procedure TavProgram.Dispatch(GroupDims: TVec3i);
begin
  FProgram.Dispatch(GroupDims);
end;

destructor TavProgram.Destroy;
begin
  FProgram := nil;
  if Main.ActiveProgram = Self then Main.ActiveProgram := nil;
  inherited Destroy;
end;

end.

