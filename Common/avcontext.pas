unit avContext;
{$I avConfig.inc}

interface

uses
  Classes, SysUtils, avTypes, mutils
  {$IfDef Windows}, Windows{$EndIf};

type
  E3DError = Class(Exception);

  {$SCOPEDENUMS ON}
  TBufferPoolType = (StaticDraw, DynamicDraw, StreamDraw);
  {$SCOPEDENUMS OFF}
  { IctxBuffer }

  IctxBuffer = interface
  ['{1EBE4AB0-AABD-44E5-B12C-29D0A3034D95}']
    //********
    function GetTargetPoolType : TBufferPoolType;
    procedure SetTargetPoolType(Value : TBufferPoolType);
    //********
    Property TargetPoolType : TBufferPoolType Read GetTargetPoolType Write SetTargetPoolType;

    Function Size : Integer;

    Function Map(usage: TMapingUsage): PByte;
    Function Unmap: Boolean;
    Procedure AllocMem(ASize : Integer; Data : PByte); Overload;
    Procedure SetSubData(AOffset, ASize : Integer; Data : PByte); Overload;
  end;

  { IctxVetexBuffer }

  IctxVetexBuffer = interface (IctxBuffer)
  ['{17615C55-CD2E-43BC-A48D-8542B7D64E0C}']
    //*******
    function GetLayout: IDataLayout;
    procedure SetLayout(const AValue: IDataLayout);
    //*******
    function VertexCount: Integer;
    property Layout: IDataLayout read GetLayout write SetLayout;
  end;

  { IctxStructuredBuffer }

  IctxStructuredBuffer = interface (IctxBuffer)
  ['{F3E8B713-AB48-4A07-9D38-2C924821A001}']
    //*******
    function GetElementSize: Integer;
    procedure SetElementSize(const AValue: Integer);
    //*******

    property ElementSize: Integer read GetElementSize write SetElementSize;
  end;

  { IctxIndexBuffer }

  IctxIndexBuffer = interface (IctxBuffer)
  ['{40E44974-8EAD-477F-9588-340A7EB1510F}']
    //*******
    function GetIndexSize: TIndexSize;
    procedure SetIndexSize(AValue: TIndexSize);
    //*******
    function IndicesCount: Integer;
    Property IndexSize: TIndexSize Read GetIndexSize Write SetIndexSize;
  end;

  { IctxTexture }

  IctxTexture = interface
  ['{FEEC2DC3-16AC-48D6-9AE1-6E358ECD4C48}']
    //*******
    function GetTargetFormat: TTextureFormat;
    function Get_sRGB: Boolean;
    procedure SetTargetFormat(Value: TTextureFormat);
    procedure Set_sRGB(Value: Boolean);
    //*******
    property TargetFormat: TTextureFormat read GetTargetFormat write SetTargetFormat;
    property sRGB: Boolean read Get_sRGB write Set_sRGB;

    function Width : Integer;
    function Height: Integer;
    function Deep  : Integer;
    function MipsCount: Integer;
    function SampleCount: Integer;

    function Format: TTextureFormat;

    procedure AllocMultiSampled(AWidth, AHeight, ASampleCount: Integer); overload;

    procedure AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean; ForcedArray: Boolean = False); overload;
    procedure AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean; DataFormat: TImageFormat; Data: PByte; ForcedArray: Boolean = False); overload;

    procedure SetMipImage(X, Y, ImageWidth, ImageHeight, MipLevel, ZSlice: Integer; DataFormat: TImageFormat; Data: PByte); overload;
    procedure SetMipImage(DestRect: TRect; MipLevel, ZSlice: Integer; DataFormat: TImageFormat; Data: PByte); overload;

    procedure GenerateMips;

    procedure ReadBack(const ATexData: ITextureData; const ASlice: Integer; const AMipLevel: Integer = -1); //-1 for all mip levels

    procedure CopyFrom(const DstMipLevel, DstSlice: Integer; const DstPos: TVec2I;
                       const ASrcRes: IctxTexture; const SrcMipLevel, SrcSlice: Integer; const SrcRect: TRectI);
  end;

  IctxTexture3D = interface
  ['{43B96A4F-0010-4C0F-9836-902E4F45A1DD}']
    //*******
    function GetTargetFormat: TTextureFormat;
    function Get_sRGB: Boolean;
    procedure SetTargetFormat(Value: TTextureFormat);
    procedure Set_sRGB(Value: Boolean);
    //*******
    property TargetFormat: TTextureFormat read GetTargetFormat write SetTargetFormat;
    property sRGB: Boolean read Get_sRGB write Set_sRGB;

    function Width : Integer;
    function Height: Integer;
    function Deep  : Integer;
    function MipsCount: Integer;

    function Format: TTextureFormat;

    procedure AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean); overload;

    procedure GenerateMips;
  end;

  IctxUAV = interface (IctxStructuredBuffer)
  ['{8CA32553-D7C1-4A87-9AFA-3D348CAC64AB}']
    function ElementsCount: Cardinal;
    function StrideSize: Cardinal;

    function ReadCounter: Cardinal;
    function ReadRAWData(AElementsCount: Integer = -1): TByteArr;
//    function Appendable
  end;

  TDataClass = (dcScalar, dcVector, dcMatrix, dcSampler, dcCubeSampler);

  { TUniformField }

  TUniformField = class
    Name : string;
    DataClass : TDataClass;
    ItemsCount : Integer;
    ElementType : TComponentType;
    ElementsCount : Integer;
    Data : PByte;
    DataSize : Integer;
  end;

  { IctxProgram }

  IctxProgram = interface
  ['{C17B35BB-A577-4149-BB9E-3F9C9CDEA3D4}']
    procedure Select(const APatchSize: Integer = 0);
    procedure Load(const AProgram: string; FromResource: Boolean = false; const AStreamOutLayout: IDataLayout = nil);

    procedure SetAttributes(const AModel, AInstances : IctxVetexBuffer; const AModelIndices: IctxIndexBuffer; InstanceStepRate: Integer = 1);

    function GetUniformField(const Name: string): TUniformField;
    procedure SetUniform(const Field: TUniformField; const Value: integer); overload;
    procedure SetUniform(const Field: TUniformField; const Value: single); overload;
    procedure SetUniform(const Field: TUniformField; const v: TVec2); overload;
    procedure SetUniform(const Field: TUniformField; const v: TVec3); overload;
    procedure SetUniform(const Field: TUniformField; const v: TVec4); overload;
    procedure SetUniform(const Field: TUniformField; const values: TSingleArr); overload;
    procedure SetUniform(const Field: TUniformField; const v: TVec4arr); overload;
    procedure SetUniform(const Field: TUniformField; const m: TMat4); overload;
    procedure SetUniform(const Field: TUniformField; const m: PMat4; const mCount: Integer); overload;
    procedure SetUniform(const Field: TUniformField; const tex: IctxTexture; const Sampler: TSamplerInfo); overload;
    procedure SetUniform(const Field: TUniformField; const tex: IctxTexture; const Sampler: TSamplerInfo; const ASliceMip: TVec2i); overload;
    procedure SetUniform(const Field: TUniformField; const tex: IctxTexture3D; const Sampler: TSamplerInfo); overload;
    procedure SetUniform(const Field: TUniformField; const buf: IctxStructuredBuffer); overload;

    procedure SetComputeUAV(const Index: Integer; const uav: IctxUAV; const initial: Integer);
    procedure SetComputeTex3D(const Index: Integer; const uav: IctxTexture3D);
    procedure SetComputeTex2D(const Index: Integer; const uav: IctxTexture);

    procedure Draw(PrimTopology: TPrimitiveType; CullMode: TCullingMode; IndexedGeometry: Boolean;
                   InstanceCount: Integer;
                   Start: integer; Count: integer;
                   BaseVertex: integer; BaseInstance: Integer);

    procedure DispatchDraw(GroupDims: TVec3i);
    procedure ClearComputeUAV(const Index: Integer; const color: TVec4i);
    procedure ResetUAVCounter(const Index: Integer);
  end;

  { IctxFrameBuffer }

  IctxFrameBuffer = interface
  ['{E2A4C25A-B9E2-40B4-B2F8-3E1EF8C93B8E}']
    procedure Select;

    procedure ClearColorList;
    procedure EnableColorTarget(index: Integer; Enabled: Boolean);
    procedure SetColor(index: Integer; tex: IctxTexture; mipLevel: Integer = 0; sliceStart: Integer = -1; sliceCount: Integer = 0);
    procedure SetColor3D(index: Integer; tex: IctxTexture3D; mipLevel: Integer = 0; sliceStart: Integer = -1; sliceCount: Integer = 0);
    procedure SetDepthStencil(tex: IctxTexture; mipLevel: Integer = 0; sliceStart: Integer = -1; sliceCount: Integer = 0);
    procedure SetUAVTex(index: Integer; UAV: IctxTexture);
    procedure SetUAVTex3D(index: Integer; UAV: IctxTexture3D);
    procedure SetUAV(index: Integer; UAV: IctxUAV);
    procedure SetStreamOut(index: Integer; buffer: IctxVetexBuffer; Offset: Integer);

    procedure Clear(index: Integer; color: TVec4);
    procedure ClearDS(depth: Single; clearDepth: Boolean = True; stencil: Integer = 0; clearStencil: Boolean = False);
    procedure ClearUAV(index: Integer; color: TVec4i);
    procedure ResetUAVCounters;

    procedure BlitToWindow(index: Integer; const srcRect, dstRect: TRectI; const Filter: TTextureFilter);
  end;

  { IRenderStates }

  IRenderStates = interface
  ['{3695F390-25CC-441B-9603-F14952631C2E}']
    // getters/setters
    function GetBlendSrc (RenderTargetIndex: Integer = AllTargets) : TBlendFunc;
    function GetBlendDest(RenderTargetIndex: Integer = AllTargets) : TBlendFunc;
    function GetBlending (RenderTargetIndex: Integer = AllTargets) : Boolean;
    function GetColorMask(RenderTargetIndex: Integer = AllTargets) : TColorMask;
    function GetCullMode               : TCullingMode;
    function GetDepthFunc              : TCompareFunc;
    function GetDepthTest              : Boolean;
    function GetDepthWrite             : Boolean;
    function GetDepthBias              : Single;
    function GetSlopeScaledDepthBias   : Single;
    function GetNearFarClamp           : Boolean;
    function GetViewport               : TRectI;
    function GetScissor                : TRectI;
    function GetScissorTest            : Boolean;
    function GetWireframe              : Boolean;
    procedure SetCullMode              (const Value : TCullingMode);
    procedure SetDepthTest             (const Value : Boolean);
    procedure SetDepthWrite            (const Value : Boolean);
    procedure SetDepthFunc             (const Value : TCompareFunc);
    procedure SetDepthBias             (const Value : Single);
    procedure SetSlopeScaledDepthBias  (const Value : Single);
    procedure SetNearFarClamp          (const Value : Boolean);
    procedure SetBlending              (RenderTargetIndex: Integer; const Value : Boolean);
    procedure SetColorMask             (RenderTargetIndex: Integer; const Value : TColorMask);
    procedure SetViewport              (const Value : TRectI);
    procedure SetScissor               (const Value : TRectI);
    procedure SetScissorTest           (const Value : Boolean);
    procedure SetWireframe             (const Value : Boolean);
    procedure SetStencil(Enabled : Boolean; StencilFunc : TCompareFunc; Ref : Integer; Mask : Byte; sFail, dFail, dPass : TStencilAction);
    // getters/setters

    property Viewport               : TRectI       read GetViewport               write SetViewport;
    property Scissor                : TRectI       read GetScissor                write SetScissor;
    property SicssorTest            : Boolean      read GetScissorTest            write SetScissorTest;

    property Wireframe              : Boolean      read GetWireframe              write SetWireframe;
    property CullMode               : TCullingMode read GetCullMode               write SetCullMode;

    property ColorMask[RenderTargetIndex: Integer] : TColorMask   read GetColorMask write SetColorMask;
    property Blending [RenderTargetIndex: Integer] : Boolean      read GetBlending  write SetBlending;
    property BlendSrc [RenderTargetIndex: Integer] : TBlendFunc   read GetBlendSrc;  //use SetBlendFunctions for set this parametrs
    property BlendDest[RenderTargetIndex: Integer] : TBlendFunc   read GetBlendDest; //use SetBlendFunctions for set this parametrs
    procedure SetBlendFunctions(Src, Dest : TBlendFunc; RenderTargetIndex: Integer = AllTargets);
    procedure SetBlendFunctions_SeparateAlpha(Src, Dest, AlphaSrc, AlphaDest : TBlendFunc; RenderTargetIndex: Integer = AllTargets);
    procedure SetBlendOperation(BlendOp : TBlendOp; RenderTargetIndex: Integer = AllTargets);
    procedure SetBlendOperation_SeparateAlpha(BlendOp, AlphaBlendOp : TBlendOp; RenderTargetIndex: Integer = AllTargets);

    property DepthTest              : Boolean      read GetDepthTest              write SetDepthTest;
    property DepthFunc              : TCompareFunc read GetDepthFunc              write SetDepthFunc;
    property DepthWrite             : Boolean      read GetDepthWrite             write SetDepthWrite;
    property NearFarClamp           : Boolean      read GetNearFarClamp           write SetNearFarClamp;
  end;

  { IRenderContext }

  IRenderContext = interface
  ['{732AEEF4-530C-45A2-874D-8CDF9EEAE33C}']
    function CreateVertexBuffer: IctxVetexBuffer;
    function CreateIndexBuffer : IctxIndexBuffer;
    function CreateStructBuffer: IctxStructuredBuffer;
    function CreateProgram     : IctxProgram;
    function CreateTexture     : IctxTexture;
    function CreateTexture3D   : IctxTexture3D;
    function CreateFrameBuffer : IctxFrameBuffer;
    function CreateUAV(const AElementsCount, AStrideSize: Cardinal; const Appendable: Boolean; const AInitialData: Pointer) : IctxUAV;

    function States: IRenderStates;
    function GetActiveProgram: IctxProgram;
    procedure SetActiveProgram(AValue: IctxProgram);

    function Binded: Boolean;
    function Bind: Boolean;
    function Unbind: Boolean;

    procedure Clear(const color  : TVec4;      doColor  : Boolean = True;
                          depth  : Single = 1; doDepth  : Boolean = False;
                          stencil: Byte   = 0; doStencil: Boolean = False);
    procedure Flush;
    procedure Present;
    property ActiveProgram: IctxProgram read GetActiveProgram write SetActiveProgram;
  end;

implementation

end.

