unit avContext;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, avTypes, mutils;

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
    procedure SetTargetFormat(Value: TTextureFormat);
    //*******
    property TargetFormat: TTextureFormat read GetTargetFormat write SetTargetFormat;

    function Width : Integer;
    function Height: Integer;
    function Format: TTextureFormat;

    procedure AllocMem(AWidth, AHeight: Integer; WithMips: Boolean); overload;
    procedure AllocMem(AWidth, AHeight: Integer; WithMips: Boolean; DataFormat: TImageFormat; Data: PByte); overload;

    procedure SetImage(ImageWidth, ImageHeight: Integer; DataFormat: TImageFormat; Data: PByte; GenMipmaps: Boolean); overload;
    procedure SetImage(X, Y, ImageWidth, ImageHeight: Integer; DataFormat: TImageFormat; Data: PByte; GenMipmaps: Boolean); overload;

    procedure SetMipImage(X, Y, ImageWidth, ImageHeight, MipLevel: Integer; DataFormat: TImageFormat; Data: PByte); overload;
    procedure SetMipImage(DestRect: TRect; MipLevel: Integer; DataFormat: TImageFormat; Data: PByte); overload;
  end;

  TDataClass = (dcScalar, dcVector, dcMatrix, dcSampler);

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
    procedure Select;
    procedure Load(const AProgram: string; FromResource: Boolean = false);

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
    procedure SetUniform(const Field: TUniformField; const tex: IctxTexture; const Sampler: TSamplerInfo); overload;

    procedure Draw(PrimTopology: TPrimitiveType; CullMode: TCullingMode; IndexedGeometry: Boolean;
                   InstanceCount: Integer;
                   Start: integer; Count: integer;
                   BaseVertex: integer; BaseInstance: Integer);
  end;

  { IctxFrameBuffer }

  IctxFrameBuffer = interface
  ['{E2A4C25A-B9E2-40B4-B2F8-3E1EF8C93B8E}']
    procedure Select;

    procedure Clear;
    procedure EnableColorTarget(index: Integer; Enabled: Boolean);
    procedure SetColor(index: Integer; tex: IctxTexture; mipLevel: Integer = 0);
    procedure SetDepthStencil(tex: IctxTexture; mipLevel: Integer = 0);

    procedure BlitToWindow(const srcRect, dstRect: TRectI; const Filter: TTextureFilter);
  end;

  IRenderStates = interface
  ['{3695F390-25CC-441B-9603-F14952631C2E}']
    // getters/setters
    function GetBlendDest              : TBlendFunc;
    function GetBlending               : Boolean;
    function GetBlendSrc               : TBlendFunc;
    function GetColorWrite             : Boolean;
    function GetCullMode               : TCullingMode;
    function GetDepthFunc              : TCompareFunc;
    function GetDepthTest              : Boolean;
    function GetDepthWrite             : Boolean;
    function GetLineWidth              : Single;
    function GetNearFarClamp           : Boolean;
    function GetVertexProgramPointSize : Boolean;
    function GetViewport               : TRectI;
    function GetWireframe              : Boolean;
    procedure SetCullMode              (const Value : TCullingMode);
    procedure SetLineWidth             (const Value : Single);
    procedure SetVertexProgramPointSize(const Value : Boolean);
    procedure SetColorWrite            (const Value : Boolean);
    procedure SetDepthTest             (const Value : Boolean);
    procedure SetDepthWrite            (const Value : Boolean);
    procedure SetDepthFunc             (const Value : TCompareFunc);
    procedure SetNearFarClamp          (const Value : Boolean);
    procedure SetBlending              (const Value : Boolean);
    procedure SetViewport              (const Value : TRectI);
    procedure SetWireframe             (const Value : Boolean);
    procedure SetScissor(Enabled : Boolean; const Value : TRect);
    procedure SetStencil(Enabled : Boolean; StencilFunc : TCompareFunc; Ref : Integer; Mask : Byte; sFail, dFail, dPass : TStencilAction);

    procedure SetBlendFunctions(Src, Dest : TBlendFunc);

    // getters/setters
    property Viewport               : TRectI       read GetViewport               write SetViewport;

    property Wireframe              : Boolean      read GetWireframe              write SetWireframe;
    property CullMode               : TCullingMode read GetCullMode               write SetCullMode;
    property LineWidth              : Single       read GetLineWidth              write SetLineWidth;
    property VertexProgramPointSize : Boolean      read GetVertexProgramPointSize write SetVertexProgramPointSize;

    property Blending               : Boolean      read GetBlending               write SetBlending;
    property BlendSrc               : TBlendFunc   read GetBlendSrc;
    property BlendDest              : TBlendFunc   read GetBlendDest;

    property DepthTest              : Boolean      read GetDepthTest              write SetDepthTest;
    property DepthFunc              : TCompareFunc read GetDepthFunc              write SetDepthFunc;
    property DepthWrite             : Boolean      read GetDepthWrite             write SetDepthWrite;
    property ColorWrite             : Boolean      read GetColorWrite             write SetColorWrite;

    property NearFarClamp           : Boolean      read GetNearFarClamp           write SetNearFarClamp;
  end;

  { IRenderContext }

  IRenderContext = interface
  ['{732AEEF4-530C-45A2-874D-8CDF9EEAE33C}']
    function CreateVertexBuffer: IctxVetexBuffer;
    function CreateIndexBuffer : IctxIndexBuffer;
    function CreateProgram     : IctxProgram;
    function CreateTexture     : IctxTexture;
    function CreateFrameBuffer : IctxFrameBuffer;

    function States: IRenderStates;
    function GetActiveProgram: IctxProgram;
    procedure SetActiveProgram(AValue: IctxProgram);

    function Binded: Boolean;
    function Bind: Boolean;
    function Unbind: Boolean;

    procedure Clear(const color  : TVec4;      doColor  : Boolean = True;
                          depth  : Single = 1; doDepth  : Boolean = False;
                          stencil: Byte   = 0; doStencil: Boolean = False);
    procedure Present;
    property ActiveProgram: IctxProgram read GetActiveProgram write SetActiveProgram;
  end;

implementation

end.

