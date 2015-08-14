unit avContext_DX11;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, avTypes, avPlatform, avContext, avContnrs, mutils, contnrs,
  D3D11_JSB, DXGI_JSB, DXTypes_JSB, D3DCommon_JSB, D3DCompiler_JSB;

type
  TavInterfacedObject = TInterfacedObject;

  { TContext_DX11 }

  TContext_DX11 = class (TavInterfacedObject, IRenderContext)
  private type
    TSamplerMap_Hash = specialize TMurmur2Hash<TSamplerInfo>;
    TSamplerMap = specialize THashMap<TSamplerInfo, ID3D11SamplerState, TSamplerMap_Hash>;
    ISamplerMap = specialize IHashMap<TSamplerInfo, ID3D11SamplerState, TSamplerMap_Hash>;
  private
    FStates: TObject;
    FStatesIntf: IRenderStates;

    FBindCount: Integer;
    FActiveProgram: IctxProgram;
    FActiveFrameBuffer: IctxFrameBuffer;

    FSwapChain: IDXGISwapChain;
    FBackBuffer: ID3D11Texture2D;
    FRenderTarget: ID3D11RenderTargetView;
    FDevice: ID3D11Device;
    FDeviceContext: ID3D11DeviceContext;
    FWnd: TWindow;

    FSamplerMap: ISamplerMap;

    FPrimTopology : TPrimitiveType;

    function GetActiveProgram: IctxProgram;
    procedure SetActiveProgram(AValue: IctxProgram);

    procedure RebuildViews(const AWidth, AHeight: Cardinal);
    procedure SetFrameBuffer(const AObject: TObject); //TFrameBuffer
    function ObtainSamplerState(const ASampler: TSamplerInfo): ID3D11SamplerState;
  public
    procedure UpdateStates; // ToDo: impl UpdateStates

    function CreateVertexBuffer : IctxVetexBuffer;
    function CreateIndexBuffer : IctxIndexBuffer;
    function CreateProgram : IctxProgram;
    function CreateTexture : IctxTexture;
    function CreateFrameBuffer : IctxFrameBuffer;

    function States : IRenderStates;
    property ActiveProgram: IctxProgram read GetActiveProgram write SetActiveProgram;

    function Binded: Boolean;
    function Bind: Boolean;
    function Unbind: Boolean;

    procedure Clear(const color  : TVec4;      doColor  : Boolean = True;
                          depth  : Single = 1; doDepth  : Boolean = False;
                          stencil: Byte   = 0; doStencil: Boolean = False);
    procedure Present;

    constructor Create(Const Wnd: TWindow);
    destructor Destroy; override;
  end;

implementation

uses Windows, Math, typinfo;

const
  D3D11TextureFormat: array [TTextureFormat] of TDXGI_Format = (
  {RGBA} DXGI_FORMAT_R8G8B8A8_UNORM,
  {RGBA16} DXGI_FORMAT_R16G16B16A16_UNORM,
  {RGBA16f} DXGI_FORMAT_R16G16B16A16_FLOAT,
  {RGBA32} DXGI_FORMAT_R32G32B32A32_SINT,
  {RGBA32f} DXGI_FORMAT_R32G32B32A32_FLOAT,
  {RGB} DXGI_FORMAT_UNKNOWN,
  {RGB16} DXGI_FORMAT_UNKNOWN,
  {RGB16f} DXGI_FORMAT_UNKNOWN,
  {RGB32} DXGI_FORMAT_R32G32B32_SINT,
  {RGB32f} DXGI_FORMAT_R32G32B32_FLOAT,
  {RG} DXGI_FORMAT_R8G8_UNORM,
  {RG16} DXGI_FORMAT_R16G16_UNORM,
  {RG16f} DXGI_FORMAT_R16G16_FLOAT,
  {RG32} DXGI_FORMAT_R32G32_SINT,
  {RG32f} DXGI_FORMAT_R32G32_FLOAT,
  {R} DXGI_FORMAT_R8_UNORM,
  {R16} DXGI_FORMAT_R16_UNORM,
  {R16f} DXGI_FORMAT_R16_FLOAT,
  {R32} DXGI_FORMAT_R32_SINT,
  {R32f} DXGI_FORMAT_R32_FLOAT,
  {DXT1} DXGI_FORMAT_UNKNOWN,
  {DXT3} DXGI_FORMAT_UNKNOWN,
  {DXT5} DXGI_FORMAT_UNKNOWN,
  {D24_S8} DXGI_FORMAT_R24G8_TYPELESS,
  {D32f_S8} DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS,
  {D16} DXGI_FORMAT_R16_TYPELESS,
  {D24} DXGI_FORMAT_R24G8_TYPELESS,
  {D32} DXGI_FORMAT_R32_TYPELESS,
  {D32f} DXGI_FORMAT_R32_TYPELESS
  );

  D3D11ViewFormat: array [TTextureFormat] of TDXGI_Format = (
  {RGBA} DXGI_FORMAT_R8G8B8A8_UNORM,
  {RGBA16} DXGI_FORMAT_R16G16B16A16_UNORM,
  {RGBA16f} DXGI_FORMAT_R16G16B16A16_FLOAT,
  {RGBA32} DXGI_FORMAT_R32G32B32A32_SINT,
  {RGBA32f} DXGI_FORMAT_R32G32B32A32_FLOAT,
  {RGB} DXGI_FORMAT_UNKNOWN,
  {RGB16} DXGI_FORMAT_UNKNOWN,
  {RGB16f} DXGI_FORMAT_UNKNOWN,
  {RGB32} DXGI_FORMAT_R32G32B32_SINT,
  {RGB32f} DXGI_FORMAT_R32G32B32_FLOAT,
  {RG} DXGI_FORMAT_R8G8_UNORM,
  {RG16} DXGI_FORMAT_R16G16_UNORM,
  {RG16f} DXGI_FORMAT_R16G16_FLOAT,
  {RG32} DXGI_FORMAT_R32G32_SINT,
  {RG32f} DXGI_FORMAT_R32G32_FLOAT,
  {R} DXGI_FORMAT_R8_UNORM,
  {R16} DXGI_FORMAT_R16_UNORM,
  {R16f} DXGI_FORMAT_R16_FLOAT,
  {R32} DXGI_FORMAT_R32_SINT,
  {R32f} DXGI_FORMAT_R32_FLOAT,
  {DXT1} DXGI_FORMAT_UNKNOWN,
  {DXT3} DXGI_FORMAT_UNKNOWN,
  {DXT5} DXGI_FORMAT_UNKNOWN,
  {D24_S8} DXGI_FORMAT_D24_UNORM_S8_UINT,
  {D32f_S8} DXGI_FORMAT_D32_FLOAT_S8X24_UINT,
  {D16} DXGI_FORMAT_D16_UNORM,
  {D24} DXGI_FORMAT_D24_UNORM_S8_UINT,
  {D32} DXGI_FORMAT_UNKNOWN,
  {D32f} DXGI_FORMAT_D32_FLOAT
  );

  DXPrimitiveType: array [TPrimitiveType] of TD3D11_PrimitiveTopology = ( {ptPoints}            D3D11_PRIMITIVE_TOPOLOGY_POINTLIST,
                                                                          {ptLines}             D3D11_PRIMITIVE_TOPOLOGY_LINELIST,
                                                                          {ptLineStrip}         D3D11_PRIMITIVE_TOPOLOGY_LINESTRIP,
                                                                          {ptTriangles}         D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST,
                                                                          {ptTriangleStrip}     D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP,
                                                                          {ptLines_Adj}         D3D11_PRIMITIVE_TOPOLOGY_LINELIST_ADJ,
                                                                          {ptLineStrip_Adj}     D3D11_PRIMITIVE_TOPOLOGY_LINESTRIP_ADJ,
                                                                          {ptTriangles_Adj}     D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST_ADJ,
                                                                          {ptTriangleStrip_Adj} D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP_ADJ);

procedure Check3DError(hr: HRESULT);
var s: string;
begin
    if hr = 0 then Exit;
    case hr of
//      D3D10_ERROR_FILE_NOT_FOUND                : s := 'D3D10_ERROR_FILE_NOT_FOUND';
//      D3D10_ERROR_TOO_MANY_UNIQUE_STATE_OBJECTS : s := 'D3D10_ERROR_TOO_MANY_UNIQUE_STATE_OBJECTS';
      //D3DERR_INVALIDCALL                        : s := 'D3DERR_INVALIDCALL';
      //D3DERR_WASSTILLDRAWING                    : s := 'D3DERR_WASSTILLDRAWING';

      DXGI_ERROR_INVALID_CALL                 : s := 'DXGI_ERROR_INVALID_CALL';
      DXGI_ERROR_NOT_FOUND                    : s := 'DXGI_ERROR_NOT_FOUND';
      DXGI_ERROR_MORE_DATA                    : s := 'DXGI_ERROR_MORE_DATA';
      DXGI_ERROR_UNSUPPORTED                  : s := 'DXGI_ERROR_UNSUPPORTED';
      DXGI_ERROR_DEVICE_REMOVED               : s := 'DXGI_ERROR_DEVICE_REMOVED';
      DXGI_ERROR_DEVICE_HUNG                  : s := 'DXGI_ERROR_DEVICE_HUNG';
      DXGI_ERROR_DEVICE_RESET                 : s := 'DXGI_ERROR_DEVICE_RESET';
      DXGI_ERROR_WAS_STILL_DRAWING            : s := 'DXGI_ERROR_WAS_STILL_DRAWING';
      DXGI_ERROR_FRAME_STATISTICS_DISJOINT    : s := 'DXGI_ERROR_FRAME_STATISTICS_DISJOINT';
      DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE : s := 'DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE';
      DXGI_ERROR_DRIVER_INTERNAL_ERROR        : s := 'DXGI_ERROR_DRIVER_INTERNAL_ERROR';
      DXGI_ERROR_NONEXCLUSIVE                 : s := 'DXGI_ERROR_NONEXCLUSIVE';
      DXGI_ERROR_NOT_CURRENTLY_AVAILABLE      : s := 'DXGI_ERROR_NOT_CURRENTLY_AVAILABLE';
      DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED   : s := 'DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED';
      DXGI_ERROR_REMOTE_OUTOFMEMORY           : s := 'DXGI_ERROR_REMOTE_OUTOFMEMORY';
      //DXGI_ERROR_ACCESS_LOST                  : s := 'DXGI_ERROR_ACCESS_LOST';
      //DXGI_ERROR_WAIT_TIMEOUT                 : s := 'DXGI_ERROR_WAIT_TIMEOUT';
      //DXGI_ERROR_SESSION_DISCONNECTED         : s := 'DXGI_ERROR_SESSION_DISCONNECTED';
      //DXGI_ERROR_RESTRICT_TO_OUTPUT_STALE     : s := 'DXGI_ERROR_RESTRICT_TO_OUTPUT_STALE';
      //DXGI_ERROR_CANNOT_PROTECT_CONTENT       : s := 'DXGI_ERROR_CANNOT_PROTECT_CONTENT';
      //DXGI_ERROR_ACCESS_DENIED                : s := 'DXGI_ERROR_ACCESS_DENIED';
      //DXGI_ERROR_NAME_ALREADY_EXISTS          : s := 'DXGI_ERROR_NAME_ALREADY_EXISTS';

      E_FAIL        : s := 'E_FAIL';
      E_INVALIDARG  : s := 'E_INVALIDARG';
      E_OUTOFMEMORY : s := 'E_OUTOFMEMORY';
      E_NOTIMPL     : s := 'E_NOTIMPL';
      S_FALSE       : s := 'S_FALSE';
    else
      s := IntToHex(hr, 8);
    end;
    raise E3DError.Create(s);
end;

function GetDXGIFormat(AType: TComponentType; ACount: Integer; Normalized: Boolean): TDXGI_Format;
begin
    Result := DXGI_FORMAT_UNKNOWN;
    if Normalized then
    begin
        case AType of
            ctBool: Assert(False, 'Unsupported format');
            ctByte  : case ACount of
                        1: Result := DXGI_FORMAT_R8_SNORM;
                        2: Result := DXGI_FORMAT_R8G8_SNORM;
                        3: Assert(False, 'Unsupported format');
                        4: Result := DXGI_FORMAT_R8G8B8A8_SNORM;
                      end;
            ctUByte : case ACount of
                        1: Result := DXGI_FORMAT_R8_UNORM;
                        2: Result := DXGI_FORMAT_R8G8_UNORM;
                        3: Assert(False, 'Unsupported format');
                        4: Result := DXGI_FORMAT_R8G8B8A8_UNORM;
                      end;
            ctShort : case ACount of
                        1: Result := DXGI_FORMAT_R16_SNORM;
                        2: Result := DXGI_FORMAT_R16G16_SNORM;
                        3: Assert(False, 'Unsupported format');
                        4: Result := DXGI_FORMAT_R16G16B16A16_SNORM;
                      end;
            ctUShort: case ACount of
                        1: Result := DXGI_FORMAT_R16_UNORM;
                        2: Result := DXGI_FORMAT_R16G16_UNORM;
                        3: Assert(False, 'Unsupported format');
                        4: Result := DXGI_FORMAT_R16G16B16A16_UNORM;
                      end;
            ctInt   : Assert(False, 'Unsupported format');
            ctUInt  : Assert(False, 'Unsupported format');
            ctFloat : case ACount of
                        1: Result := DXGI_FORMAT_R32_FLOAT;
                        2: Result := DXGI_FORMAT_R32G32_FLOAT;
                        3: Result := DXGI_FORMAT_R32G32B32_FLOAT;
                        4: Result := DXGI_FORMAT_R32G32B32A32_FLOAT;
                      end;
            ctDouble: Assert(False, 'Unsupported format');
        end;
    end
    else
    begin
        case AType of
            ctBool: Assert(False, 'Unsupported format');
            ctByte  : case ACount of
                        1: Result := DXGI_FORMAT_R8_SINT;
                        2: Result := DXGI_FORMAT_R8G8_SINT;
                        3: Assert(False, 'Unsupported format');
                        4: Result := DXGI_FORMAT_R8G8B8A8_SINT;
                      end;
            ctUByte : case ACount of
                        1: Result := DXGI_FORMAT_R8_UINT;
                        2: Result := DXGI_FORMAT_R8G8_UINT;
                        3: Assert(False, 'Unsupported format');
                        4: Result := DXGI_FORMAT_R8G8B8A8_UINT;
                      end;
            ctShort : case ACount of
                        1: Result := DXGI_FORMAT_R16_SINT;
                        2: Result := DXGI_FORMAT_R16G16_SINT;
                        3: Assert(False, 'Unsupported format');
                        4: Result := DXGI_FORMAT_R16G16B16A16_SINT;
                      end;
            ctUShort: case ACount of
                        1: Result := DXGI_FORMAT_R16_UINT;
                        2: Result := DXGI_FORMAT_R16G16_UINT;
                        3: Assert(False, 'Unsupported format');
                        4: Result := DXGI_FORMAT_R16G16B16A16_UINT;
                      end;
            ctInt   : case ACount of
                        1: Result := DXGI_FORMAT_R32_SINT;
                        2: Result := DXGI_FORMAT_R32G32_SINT;
                        3: Result := DXGI_FORMAT_R32G32B32_SINT;
                        4: Result := DXGI_FORMAT_R32G32B32A32_SINT;
                      end;
            ctUInt  : case ACount of
                        1: Result := DXGI_FORMAT_R32_UINT;
                        2: Result := DXGI_FORMAT_R32G32_UINT;
                        3: Result := DXGI_FORMAT_R32G32B32_UINT;
                        4: Result := DXGI_FORMAT_R32G32B32A32_UINT;
                      end;
            ctFloat : case ACount of
                        1: Result := DXGI_FORMAT_R32_FLOAT;
                        2: Result := DXGI_FORMAT_R32G32_FLOAT;
                        3: Result := DXGI_FORMAT_R32G32B32_FLOAT;
                        4: Result := DXGI_FORMAT_R32G32B32A32_FLOAT;
                      end;
            ctDouble: Assert(False, 'Unsupported format');
        end;
    end;
end;

type

  { TStates }

  TStates = class (TNoRefObject, IRenderStates)
  private
    const DXBlend : array [TBlendFunc] of D3D11_BLEND = (
      D3D11_BLEND_ZERO,         // bfZero
      D3D11_BLEND_ONE,          // bfOne
      D3D11_BLEND_SRC_ALPHA,    // bfSrcAlpha
      D3D11_BLEND_INV_SRC_ALPHA,// bfInvSrcAlpha
      D3D11_BLEND_SRC_COLOR,    // bfSrcColor
      D3D11_BLEND_DEST_COLOR    // bfDstColor
    );

    const DXDepthMask : array [Boolean] of D3D11_DEPTH_WRITE_MASK = (
      D3D11_DEPTH_WRITE_MASK_ZERO,
      D3D11_DEPTH_WRITE_MASK_ALL
    );

    const DXCompareFunc : array [TCompareFunc] of TD3D11_ComparisonFunc = (
      D3D11_COMPARISON_NEVER,         // cfNever
      D3D11_COMPARISON_LESS,          // cfLess
      D3D11_COMPARISON_EQUAL,         // cfEqual
      D3D11_COMPARISON_NOT_EQUAL,     // cfNotEqual
      D3D11_COMPARISON_LESS_EQUAL,    // cfLessEqual
      D3D11_COMPARISON_GREATER,       // cfGreater
      D3D11_COMPARISON_GREATER_EQUAL, // cfGreaterEqual
      D3D11_COMPARISON_ALWAYS         // cfAlways
    );

    const F3DDepthFunc : array [TD3D11_ComparisonFunc] of TCompareFunc = (
      cfNever,
      cfNever,        // D3D11_COMPARISON_NEVER
      cfLess,         // D3D11_COMPARISON_LESS
      cfEqual,        // D3D11_COMPARISON_EQUAL
      cfLessEqual,    // D3D11_COMPARISON_LESS_EQUAL
      cfGreater,      // D3D11_COMPARISON_GREATER
      cfNotEqual,     // D3D11_COMPARISON_NOT_EQUAL
      cfGreaterEqual, // D3D11_COMPARISON_GREATER_EQUAL
      cfAlways        // D3D11_COMPARISON_ALWAYS
    );

    const DXStencilAction : array [TStencilAction] of TD3D11_StencilOp = (
      D3D11_STENCIL_OP_KEEP,     //saKeep
      D3D11_STENCIL_OP_REPLACE,  //saSet
      D3D11_STENCIL_OP_ZERO,     //saZero
      D3D11_STENCIL_OP_INVERT,   //saInvert
      D3D11_STENCIL_OP_INCR,     //saInc
      D3D11_STENCIL_OP_DECR,     //saDec
      D3D11_STENCIL_OP_INCR_SAT, //saIncWrap
      D3D11_STENCIL_OP_DECR_SAT  //saDecWrap
    );
  private type
    TBlendStateHash_func = specialize TMurmur2Hash<TD3D11_BlendDesc>;
    TBlendStateMap = specialize THashMap<TD3D11_BlendDesc, ID3D11BlendState, TBlendStateHash_func>;
    IBlendStateMap = specialize IHashMap<TD3D11_BlendDesc, ID3D11BlendState, TBlendStateHash_func>;

    TRasterStateHash_func = specialize TMurmur2Hash<TD3D11_RasterizerDesc>;
    TRasterStateMap = specialize THashMap<TD3D11_RasterizerDesc, ID3D11RasterizerState, TRasterStateHash_func>;
    IRasterStateMap = specialize IHashMap<TD3D11_RasterizerDesc, ID3D11RasterizerState, TRasterStateHash_func>;

    TDepthStateHash_func = specialize TMurmur2Hash<TD3D11_DepthStencilDesc>;
    TDepthStateMap = specialize THashMap<TD3D11_DepthStencilDesc, ID3D11DepthStencilState, TDepthStateHash_func>;
    IDepthStateMap = specialize IHashMap<TD3D11_DepthStencilDesc, ID3D11DepthStencilState, TDepthStateHash_func>;
  private
    FContext: TContext_DX11;

    FViewport: TRectI;

    FBDesc: TD3D11_BlendDesc;
    FBDescDirty: Boolean;
    FBlendStates: IBlendStateMap;
    FBlendStateLast: ID3D11BlendState;

    FRDesc: TD3D11_RasterizerDesc;
    FRDescDirty: Boolean;
    FRasterStates: IRasterStateMap;
    FRasterStateLast: ID3D11RasterizerState;

    FDDesc: TD3D11_DepthStencilDesc;
    FDDescDirty: Boolean;
    FDepthStates: IDepthStateMap;
    FDepthStateLast: ID3D11DepthStencilState;
    FDStencilRefLast: Integer;
    FDStencilRef: Integer;
  protected
    procedure LoadDefaultState;

    procedure InvalidateAllStates;
    procedure UpdateBlendState;
    procedure UpdateRasterState;
    procedure UpdateDepthState;
  protected
    // getters/setters
    function GetBlendSrc (RenderTargetIndex: Integer = 0) : TBlendFunc;
    function GetBlendDest(RenderTargetIndex: Integer = 0) : TBlendFunc;
    function GetBlending               : Boolean;
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

    procedure SetBlendFunctions(Src, Dest : TBlendFunc; RenderTargetIndex: Integer = 0);

    // getters/setters
    property Viewport               : TRectI       read GetViewport               write SetViewport;

    property Wireframe              : Boolean      read GetWireframe              write SetWireframe;
    property CullMode               : TCullingMode read GetCullMode               write SetCullMode;
    property LineWidth              : Single       read GetLineWidth              write SetLineWidth;
    property VertexProgramPointSize : Boolean      read GetVertexProgramPointSize write SetVertexProgramPointSize;

    property Blending               : Boolean      read GetBlending               write SetBlending;
    property BlendSrc [RenderTargetIndex: Integer] : TBlendFunc   read GetBlendSrc;
    property BlendDest[RenderTargetIndex: Integer] : TBlendFunc   read GetBlendDest;

    property DepthTest              : Boolean      read GetDepthTest              write SetDepthTest;
    property DepthFunc              : TCompareFunc read GetDepthFunc              write SetDepthFunc;
    property DepthWrite             : Boolean      read GetDepthWrite             write SetDepthWrite;
    property ColorWrite             : Boolean      read GetColorWrite             write SetColorWrite;

    property NearFarClamp           : Boolean      read GetNearFarClamp           write SetNearFarClamp;
  public
    constructor Create(AContext: TContext_DX11);
  end;


  { TColorSpaceConverter }

  TColorSpaceConverter = class
  public
    class function Convert(ASrc: PByte; ASrcSize: Integer; ASrcFormat: TImageFormat; ADstFormat: TTextureFormat; out ADst: PByte; out ADstSize: Integer): Boolean;
  end;


  { THandleObject }

  THandleObject = class (TavInterfacedObject)
  private
  protected
    FContext: TContext_DX11;
  public
    constructor Create(const AContext: TContext_DX11); virtual;
    destructor Destroy; override;
  end;

  IctxTexture_DX11 = interface (IctxTexture)
  ['{32A32DF0-5E08-4BDC-98AC-79144BAF3BCB}']
    function GetHandle : ID3D11Texture2D;
    function GetResView: ID3D11ShaderResourceView;
  end;

  { TTexture }

  TTexture = class (THandleObject, IctxTexture, IctxTexture_DX11)
  private
    FTargetFormat: TTextureFormat;
    FWidth: Integer;
    FHeight: Integer;
    FFormat: TTextureFormat;
    FWithMips: Boolean;

    FTexture: ID3D11Texture2D;
    FResView: ID3D11ShaderResourceView;

    function BuildDesc(AWidth, AHeight: Integer; WithMips: Boolean): TD3D11_Texture2DDesc;

    function GetHandle : ID3D11Texture2D;
    function GetResView: ID3D11ShaderResourceView;
  public
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

  TFrameBuffer = class;

  IctxFrameBuffer_DX11 = interface
  ['{7CF255BF-6B18-49E9-927A-8C2D59D36A0C}']
    function GetObj: TFrameBuffer;
  end;

  { TFrameBuffer }

  TFrameBuffer = class (THandleObject, IctxFrameBuffer, IctxFrameBuffer_DX11)
  private type
    TTexInfo = record
      Tex    : IctxTexture;
      Mip    : Integer;
      Enabled: Boolean;
    end;
  private
    FTex   : array of TTexInfo;
    FViews : array of ID3D11RenderTargetView;

    FDepthView: ID3D11DepthStencilView;
    FDepthTex : IctxTexture;
    FDepthMip : Integer;

    FValid: Boolean;
    function GetObj: TFrameBuffer;
  public
    procedure Select;

    procedure ClearColorList;
    procedure EnableColorTarget(index: Integer; Enabled: Boolean);
    procedure SetColor(index: Integer; tex: IctxTexture; mipLevel: Integer = 0);
    procedure SetDepthStencil(tex: IctxTexture; mipLevel: Integer = 0);

    procedure Clear(index: Integer; color: TVec4);
    procedure ClearDS(depth: Single; clearDepth: Boolean = True; stencil: Integer = 0; clearStencil: Boolean = False);

    procedure BlitToWindow(index: Integer; const srcRect, dstRect: TRectI; const Filter: TTextureFilter);
  end;

  { TBufferBase }

  TBufferBase = class(THandleObject, IctxBuffer)
  protected
    const
        DXPoolType: array [TBufferPoolType] of TD3D11_Usage = ( {StaticDraw }  D3D11_USAGE_DEFAULT,
                                                                {DynamicDraw}  D3D11_USAGE_DYNAMIC,
                                                                {StreamDraw }  D3D11_USAGE_STAGING);
  protected
    FSize: Integer;
    FTargetPool: TBufferPoolType;
    FBuffer: ID3D11Buffer;

    function GetBufferBindFlag: Cardinal; virtual; abstract;
  public
    function GetTargetPoolType: TBufferPoolType;
    procedure SetTargetPoolType(Value: TBufferPoolType);
    property TargetPoolType: TBufferPoolType read GetTargetPoolType write SetTargetPoolType;

    function Size: Integer;

    function Map(usage: TMapingUsage): PByte;
    function Unmap: Boolean;

    procedure AllocMem(ASize: Integer; Data: PByte); virtual; overload;
    procedure SetSubData(AOffset, ASize: Integer; Data: PByte); overload;

    procedure AfterConstruction; override;
  end;

  { IctxVetexBuffer_DX }

  IctxVetexBuffer_DX = interface(IctxVetexBuffer)
  ['{87083775-37DB-41A6-8DA8-FC3189E6060F}']
    procedure Select(Slot: Integer = 0);
  end;

  { TVertexBuffer }

  TVertexBuffer = class(TBufferBase, IctxVetexBuffer, IctxVetexBuffer_DX)
  private
    FLayout: IDataLayout;
    function GetBufferBindFlag: Cardinal; override;
  public
    //*******
    function GetLayout: IDataLayout;
    procedure SetLayout(const AValue: IDataLayout);
    //*******
    function VertexCount: Integer;
    property Layout: IDataLayout read GetLayout write SetLayout;

    procedure Select(Slot: Integer = 0);
  end;

  { IctxIndexBuffer_DX }

  IctxIndexBuffer_DX = interface(IctxIndexBuffer)
  ['{E720002F-3E37-40F7-AED3-43AE79734E5B}']
    procedure Select();
  end;

  { TIndexBuffer }

  TIndexBuffer = class(TBufferBase, IctxIndexBuffer, IctxIndexBuffer_DX)
  private
    FIndexSize: TIndexSize;
    function GetBufferBindFlag: Cardinal; override;
  public
    //*******
    function GetIndexSize: TIndexSize;
    procedure SetIndexSize(AValue: TIndexSize);
    //*******
    function IndicesCount: Integer;
    Property IndexSize: TIndexSize Read GetIndexSize Write SetIndexSize;

    procedure Select();
  end;

  { TConstantBuffer }

  TConstantBuffer = class(TBufferBase)
  private
    FData: array of Byte;
    FDirty: Boolean;
    function GetBufferBindFlag: Cardinal; override;
    procedure SyncToGPU; {$IFNDEF NoInline} inline; {$ENDIF}
  public
    function Ptr: PByte;
    procedure AllocMem(ASize: Integer; Data: PByte); override; overload;
//      procedure Select(Shader: TShaderType; Slot: Integer = 0);
    procedure AfterConstruction; override;
    procedure Invalidate;
    procedure LoadFromReflect(const Reflect: ID3D11ShaderReflectionConstantBuffer);
  end;

  { TUniformField_DX }

  TUniformField_DX = class(TUniformField)
  public
    FCB           : array [TShaderType] of TConstantBuffer;
    FData         : array [TShaderType] of PByte;

    FResChanged   : Boolean;
    FResourceIndex: array [TShaderType] of Integer;
    FSamplerIndex : array [TShaderType] of Integer;
    FSamplerState : ID3D11SamplerState;
    FResourceView : ID3D11ShaderResourceView;
    procedure BindResource(Const Device : ID3D11DeviceContext); Inline;
    procedure SetResource(Const ResourceView : ID3D11ShaderResourceView; Const SamplerState : ID3D11SamplerState); Inline;
    procedure AfterConstruction; override;
  end;

  { TProgram }

  TProgram = class(THandleObject, IctxProgram)
  private type
    TILKey = record
      ModelRI    : IDataLayout;
      InstanceRI : IDataLayout;
    end;
    TILInfo = record
      IL: ID3D11InputLayout;
      BindTime  : Cardinal;
    end;

    TILKeyHash_func = specialize TMurmur2Hash<TILKey>;
    TILMap = specialize THashMap<TILKey, TILInfo, TILKeyHash_func>;
    IILMap = specialize IHashMap<TILKey, TILInfo, TILKeyHash_func>;

    TUniformMap = specialize THashMap<string, TUniformField, TMurmur2HashString>;
    IUniformMap = specialize IHashMap<string, TUniformField, TMurmur2HashString>;
  private
    FILMap: IILMap;

    FData  : array [TShaderType] of TByteArr;
    FShader: array [TShaderType] of ID3D11DeviceChild;
    FCB    : array [TShaderType] of TConstantBuffer;

    FResUniforms: array of TUniformField_DX;
    FUniforms: IUniformMap;

    function ObtainUniformField(const name: string; out WasCreated: Boolean): TUniformField_DX;
    procedure ClearUniformList;
    procedure ClearILList;

    procedure SetIL(const AKey: TILKey);

    procedure SetUniform(const DXField: TUniformField_DX; const data; const datasize: Integer); overload; //inline;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure SyncCB;
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

{ TUniformField_DX }

procedure TUniformField_DX.BindResource(const Device: ID3D11DeviceContext);
begin
  if not FResChanged then Exit;

  if FResourceIndex[stVertex]>=0 then
    Device.VSSetShaderResources(FResourceIndex[stVertex], 1, @FResourceView);
  if FSamplerIndex[stVertex]>=0 then
    Device.VSSetSamplers(FSamplerIndex[stVertex], 1, @FSamplerState);

  if FResourceIndex[stGeometry]>=0 then
    Device.GSSetShaderResources(FResourceIndex[stGeometry], 1, @FResourceView);
  if FSamplerIndex[stGeometry]>=0 then
    Device.GSSetSamplers(FSamplerIndex[stGeometry], 1, @FSamplerState);

  if FResourceIndex[stFragment]>=0 then
    Device.PSSetShaderResources(FResourceIndex[stFragment], 1, @FResourceView);
  if FSamplerIndex[stFragment]>=0 then
    Device.PSSetSamplers(FSamplerIndex[stFragment], 1, @FSamplerState);

  FResChanged := False;
end;

procedure TUniformField_DX.SetResource(const ResourceView: ID3D11ShaderResourceView;
  const SamplerState: ID3D11SamplerState);
begin
  If (ResourceView = FResourceView) And (SamplerState = FSamplerState) Then Exit;
  FResourceView := ResourceView;
  FSamplerState := SamplerState;
  FResChanged := True;
end;

procedure TUniformField_DX.AfterConstruction;
var st: TShaderType;
begin
  inherited;
  for st := Low(TShaderType) to High(TShaderType) do
  begin
    FSamplerIndex[st] := -1;
    FResourceIndex[st] := -1;
  end;
end;

{ TConstantBuffer }

function TConstantBuffer.GetBufferBindFlag: Cardinal;
begin
  Result := DWord(D3D11_BIND_CONSTANT_BUFFER);
end;

procedure TConstantBuffer.SyncToGPU;
var PB: PByte;
begin
  If FDirty Then
  Begin
      PB := Map(muWriteOnly);
      Move(FData[0], PB^, Length(FData));
      Unmap;
      FDirty := False;
  End;
end;

function TConstantBuffer.Ptr: PByte;
begin
  Result := @FData[0];
end;

procedure TConstantBuffer.AllocMem(ASize: Integer; Data: PByte);
begin
  inherited;
  SetLength(FData, ASize);
  if Assigned(Data) then
    Move(Data^, FData[0], ASize);
end;

procedure TConstantBuffer.AfterConstruction;
begin
  inherited AfterConstruction;
  FTargetPool := TBufferPoolType.DynamicDraw;
end;

procedure TConstantBuffer.Invalidate;
begin
  FDirty := True;
end;

procedure TConstantBuffer.LoadFromReflect(const Reflect: ID3D11ShaderReflectionConstantBuffer);
begin

end;

{ TIndexBuffer }

function TIndexBuffer.GetBufferBindFlag: Cardinal;
begin
  Result := DWord(D3D11_BIND_INDEX_BUFFER);
end;

function TIndexBuffer.GetIndexSize: TIndexSize;
begin
  Result := FIndexSize;
end;

procedure TIndexBuffer.SetIndexSize(AValue: TIndexSize);
begin
  FIndexSize := AValue;
end;

function TIndexBuffer.IndicesCount: Integer;
const IndexSizeInBytes : array [TIndexSize] of Integer = (2, 4);
begin
  Result := Size div IndexSizeInBytes[FIndexSize];
end;

procedure TIndexBuffer.Select;
begin
  case FIndexSize of
      TIndexSize.Word : FContext.FDeviceContext.IASetIndexBuffer(FBuffer, DXGI_FORMAT_R16_UINT, 0);
      TIndexSize.DWord: FContext.FDeviceContext.IASetIndexBuffer(FBuffer, DXGI_FORMAT_R32_UINT, 0);
  end;
end;

{ TVertexBuffer }

function TVertexBuffer.GetBufferBindFlag: Cardinal;
begin
  Result := DWord(D3D11_BIND_VERTEX_BUFFER);
end;

function TVertexBuffer.GetLayout: IDataLayout;
begin
  Result := FLayout;
end;

procedure TVertexBuffer.SetLayout(const AValue: IDataLayout);
begin
  FLayout := AValue;
end;

function TVertexBuffer.VertexCount: Integer;
begin
  if FLayout = nil then Exit(0);
  if FLayout.Size = 0 then Exit(0);
  Result := Size div FLayout.Size;
end;

procedure TVertexBuffer.Select(Slot: Integer);
var stride, offset: Integer;
begin
  if Assigned(FBuffer) and (FSize>0) and assigned(FLayout) then
  begin
    offset := 0;
    stride := FLayout.Size;
    FContext.FDeviceContext.IASetVertexBuffers(Slot, 1, @FBuffer, @stride, @offset);
  end;
end;

{ TBufferHandleBase }

function TBufferBase.GetTargetPoolType: TBufferPoolType;
begin
  Result := FTargetPool;
end;

procedure TBufferBase.SetTargetPoolType(Value: TBufferPoolType);
begin
  FTargetPool := Value;
end;

function TBufferBase.Size: Integer;
begin
  Result := FSize;
end;

function TBufferBase.Map(usage: TMapingUsage): PByte;
const DXMapType: array [TMapingUsage] of TD3D11_Map = (D3D11_MAP_WRITE_DISCARD, D3D11_MAP_READ, D3D11_MAP_READ_WRITE);
var MappedSubres: TD3D11_MappedSubresource;
begin
  Check3DError(FContext.FDeviceContext.Map(FBuffer, 0, DXMapType[usage], 0, MappedSubres));
  Result := MappedSubres.pData;
end;

function TBufferBase.Unmap: Boolean;
begin
  FContext.FDeviceContext.Unmap(FBuffer, 0);
  Result := True;
end;

procedure TBufferBase.AllocMem(ASize: Integer; Data: PByte);
const
  DXCpuAccess: array [TBufferPoolType] of DWord = ( {StaticDraw }  0,
                                                    {DynamicDraw}  DWord(D3D11_CPU_ACCESS_WRITE),
                                                    {StreamDraw }  DWord(D3D11_CPU_ACCESS_WRITE) or DWord(D3D11_CPU_ACCESS_READ));
var Desc: TD3D11_BufferDesc;
    BufData: TD3D11_SubresourceData;
begin
  if ASize = 0 then Exit;
  Desc.ByteWidth := ASize;
  Desc.Usage := DXPoolType[FTargetPool];
  Desc.BindFlags := GetBufferBindFlag;
  Desc.CPUAccessFlags := DXCpuAccess[FTargetPool];
  Desc.MiscFlags := 0;

  FBuffer := nil;
  if assigned(Data) then
  begin
      BufData.pSysMem := Data;
      BufData.SysMemPitch := 0;
      BufData.SysMemSlicePitch := 0;
      Check3DError(FContext.FDevice.CreateBuffer(Desc, @BufData, FBuffer));
  end
  else
    Check3DError(FContext.FDevice.CreateBuffer(Desc, nil, FBuffer));
  FSize := ASize;
end;

procedure TBufferBase.SetSubData(AOffset, ASize: Integer; Data: PByte);
var box: TD3D11_Box;
begin
  box.left := AOffset;
  box.right := AOffset + ASize;
  box.top := 0;
  box.bottom := 1;
  box.front := 0;
  box.back := 1;
  FContext.FDeviceContext.UpdateSubresource(FBuffer, 0, @box, Data, ASize, 0);
end;

procedure TBufferBase.AfterConstruction;
begin
  inherited AfterConstruction;
  FTargetPool := TBufferPoolType.StaticDraw;
end;

{ TProgram }

function TProgram.ObtainUniformField(const name: string; out WasCreated: Boolean): TUniformField_DX;
begin
  if not FUniforms.TryGetValue(name, TUniformField(Result)) then
  begin
    Result := TUniformField_DX.Create;
    Result.Name := name;
    FUniforms.Add(name, Result);
    WasCreated := True;
  end
  else
    WasCreated := False;
end;

procedure TProgram.ClearUniformList;
var Value: TUniformField;
    name : string;
begin
  FUniforms.Reset;
  while FUniforms.Next(name, Value) do Value.Free;
  FUniforms.Clear;
end;

procedure TProgram.ClearILList;
begin
  FILMap.Clear;
end;

procedure TProgram.SetIL(const AKey: TILKey);
    procedure ParseAttributeName(const AttrName: string; out SemanticName: AnsiString; out SemanticIndex: Cardinal);
    const Digits: array ['0'..'9'] of Cardinal = (0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
    var StrN, I: Integer;
        N: Cardinal;
    begin
      SemanticIndex := 0;
      N := 1;
      StrN := Length(AttrName);
      for I := Length(AttrName) downto 1 do
      begin
        if AttrName[I] < '0' then Break;
        if AttrName[I] > '9' then Break;
        Dec(StrN);
        SemanticIndex := SemanticIndex + N * Digits[AttrName[I]];
        N := N * 10;
      end;
      SemanticName := AnsiString(Copy(AttrName, 1, StrN));
    end;
var Info: TILInfo;
    ElDesc: array of TD3D11_InputElementDesc;
    Names: array of AnsiString;
    I, N: Integer;
    field: TFieldInfo;
begin
  if not FILMap.TryGetValue(AKey, Info) then
  begin
    N := 0;
    if assigned(AKey.ModelRI) then Inc(N, AKey.ModelRI.Count);
    if assigned(AKey.InstanceRI) then Inc(N, AKey.InstanceRI.Count);
    if N = 0 then Exit;

    SetLength(ElDesc, N);
    SetLength(Names, N);
    N := 0;
    if assigned(AKey.ModelRI) then
    begin
        for I := 0 to AKey.ModelRI.Count - 1 do
        begin
          field := AKey.ModelRI.Item[I];
          ParseAttributeName(field.Name, Names[I], ElDesc[I].SemanticIndex);
          ElDesc[I].SemanticName := PAnsiChar(Names[I]);
          ElDesc[I].Format := GetDXGIFormat(field.CompType, field.CompCount, field.DoNorm);
          ElDesc[I].InputSlot := 0;
          ElDesc[I].AlignedByteOffset := field.Offset;
          ElDesc[I].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
          ElDesc[I].InstanceDataStepRate := 0;
        end;
        N := AKey.ModelRI.Count;
    end;
    if assigned(AKey.InstanceRI) then
    begin
        for I := 0 to AKey.InstanceRI.Count - 1 do
        begin
          field := AKey.InstanceRI.Item[I];
          ParseAttributeName(field.Name, Names[I+N], ElDesc[I+N].SemanticIndex);
          ElDesc[I+N].SemanticName := PAnsiChar(Names[I+N]);
          ElDesc[I+N].Format := GetDXGIFormat(field.CompType, field.CompCount, field.DoNorm);
          ElDesc[I+N].InputSlot := 1;
          ElDesc[I+N].AlignedByteOffset := field.Offset;
          ElDesc[I+N].InputSlotClass := D3D11_INPUT_PER_INSTANCE_DATA;
          ElDesc[I+N].InstanceDataStepRate := 1;
        end;
    end;
    Check3DError(FContext.FDevice.CreateInputLayout(@ElDesc[0], Length(ElDesc), @FData[stVertex][0], Length(FData[stVertex]), Info.IL));
    FILMap.AddOrSet(AKey, Info);
  end;
  FContext.FDeviceContext.IASetInputLayout(Info.IL);
end;

procedure TProgram.SetUniform(const DXField: TUniformField_DX; const data; const datasize: Integer);
var st: TShaderType;
begin
  if DXField = nil then Exit;
  if {$IFDEF NOCACHE_UNIFORMS}True{$ELSE}not CompareMem(DXField.Data, @data, datasize){$ENDIF} then
  begin
    for st := stVertex to stFragment do
      if assigned(DXField.FCB[st]) then
      begin
        Move(data, DXField.FData[st]^, datasize);
        DXField.FCB[st].Invalidate;
      end;
  end;
end;

procedure TProgram.AfterConstruction;
var ILKey : TILKey;
    ILInfo: TILInfo;
begin
  inherited AfterConstruction;
  ILKey.InstanceRI := nil;
  ILKey.ModelRI := nil;
  ILInfo.IL := nil;
  ILInfo.BindTime := 0;
  FILMap := TILMap.Create(ILKey, ILInfo);
  FUniforms := TUniformMap.Create;
end;

destructor TProgram.Destroy;
var st: TShaderType;
begin
  ClearUniformList;
  ClearILList;
  for st := Low(TShaderType) to High(TShaderType) do
    FreeAndNil(FCB[st]);
  inherited Destroy;
end;

procedure TProgram.SyncCB;
var st: TShaderType;
    i: Integer;
begin
  for st := stVertex to stFragment do
    if Assigned(FCB[st]) then
        FCB[st].SyncToGPU;
  for i := 0 to Length(FResUniforms) - 1 do
    FResUniforms[i].BindResource(FContext.FDeviceContext);
end;

procedure TProgram.Select;
var st: TShaderType;
    i: Integer;
begin
  FContext.FActiveProgram := Self;
  FContext.FDeviceContext.VSSetShader(ID3D11VertexShader(FShader[stVertex]), nil, 0);
  FContext.FDeviceContext.GSSetShader(ID3D11GeometryShader(FShader[stGeometry]), nil, 0);
  FContext.FDeviceContext.PSSetShader(ID3D11PixelShader(FShader[stFragment]), nil, 0);
  for st := stVertex to stFragment do
    if Assigned(FCB[st]) then
    begin
      FCB[st].SyncToGPU;
      case st of
        stUnknown : ;
        stVertex  : FContext.FDeviceContext.VSSetConstantBuffers(0, 1, @FCB[st].FBuffer);
        stGeometry: FContext.FDeviceContext.GSSetConstantBuffers(0, 1, @FCB[st].FBuffer);
        stFragment: FContext.FDeviceContext.PSSetConstantBuffers(0, 1, @FCB[st].FBuffer);
      end;
    end;
  for i := 0 to Length(FResUniforms) - 1 do
    FResUniforms[i].FResChanged := True;
end;

procedure TProgram.Load(const AProgram: string; FromResource: Boolean);
    function StreamReadString(const stream: TStream): WideString;
    var n: Integer;
    begin
        stream.ReadBuffer(n, SizeOf(n));
        SetLength(Result, n);
        stream.ReadBuffer(Result[1], Length(Result) * SizeOf(WideChar));
    end;
    function StreamReadShaderData(const stream: TStream): TByteArr;
    var n: Integer;
    begin
        stream.ReadBuffer(n, SizeOf(n));
        SetLength(Result, n);
        if n > 0 then
            stream.ReadBuffer(Result[0], n);
    end;
    function GetDataClass(const UniformTypeDesc: TD3D11_ShaderTypeDesc): TDataClass;
    begin
        case UniformTypeDesc._Class of
            D3D10_SVC_SCALAR: Result := dcScalar;
            D3D10_SVC_VECTOR: Result := dcVector;

            D3D10_SVC_MATRIX_ROWS,
            D3D10_SVC_MATRIX_COLUMNS: Result := dcMatrix;

            D3D10_SVC_OBJECT,
            D3D10_SVC_STRUCT,
            D3D11_SVC_INTERFACE_CLASS,
            D3D11_SVC_INTERFACE_POINTER: Result := dcSampler;
        else
            Result := dcScalar;
            Assert(False, 'Unsupported type');
        end;
    end;
    function GetComponentType(const UniformTypeDesc: TD3D11_ShaderTypeDesc): TComponentType;
    begin
        case UniformTypeDesc._Type of
            D3D10_SVT_BOOL  : Result := ctBool;
            D3D10_SVT_INT   : Result := ctInt;
            D3D10_SVT_FLOAT : Result := ctFloat;
            D3D10_SVT_UINT  : Result := ctUInt;
            D3D11_SVT_DOUBLE: Result := ctDouble;
        else
            Result := ctInt;
        end;
    end;
    function AddUniformByDescs(const ShaderVarDesc: TD3D11_ShaderVariableDesc; const ShaderTypeDesc: TD3D11_ShaderTypeDesc): TUniformField_DX;
    var WasCreated: Boolean;
    begin
        Result := ObtainUniformField(string(ShaderVarDesc.Name), WasCreated);
        If WasCreated Then
        begin
            Result.DataClass     := GetDataClass(ShaderTypeDesc);
            Result.ElementType   := GetComponentType(ShaderTypeDesc);
            Result.ItemsCount    := ShaderTypeDesc.Elements;
            Result.ElementsCount := ShaderTypeDesc.Rows * ShaderTypeDesc.Columns;
        end
        else
        begin
            Assert(Result.DataClass     = GetDataClass(ShaderTypeDesc),
                   'Uniform ' + Result.Name + ' at program ' + AProgram + ' differs between shaders in DataClass');

            Assert(Result.ElementType   = GetComponentType(ShaderTypeDesc),
                   'Uniform ' + Result.Name + ' at program ' + AProgram + ' differs between shaders in ElementType');

            Assert(Cardinal(Result.ItemsCount) = ShaderTypeDesc.Elements,
                   'Uniform ' + Result.Name + ' at program ' + AProgram + ' differs between shaders in ItemsCount');

            Assert(Cardinal(Result.ElementsCount) = ShaderTypeDesc.Rows * ShaderTypeDesc.Columns,
                   'Uniform ' + Result.Name + ' at program ' + AProgram + ' differs between shaders in ElementsCount');

        end;
    end;
    function AddUniformByDesc(const ShaderBindDesc: TD3D11_ShaderInputBindDesc): TUniformField_DX;
    var WasCreated: Boolean;
    begin
        Result := ObtainUniformField(string(ShaderBindDesc.Name), WasCreated);
        If WasCreated Then
        begin
            Result.DataClass := dcSampler;
            Result.ElementType := ctInt;
            Result.ItemsCount := 1;
            Result.ElementsCount := 1;
        end
        else
        begin
            Assert(Result.DataClass = dcSampler,
                   'Uniform ' + Result.Name + ' at program ' + AProgram + ' differs between shaders in DataClass');

            Assert(Result.ElementType = ctInt,
                   'Uniform ' + Result.Name + ' at program ' + AProgram + ' differs between shaders in ElementType');

            Assert(Result.ItemsCount = 1,
                   'Uniform ' + Result.Name + ' at program ' + AProgram + ' differs between shaders in ItemsCount');

            Assert(Result.ElementsCount = 1,
                   'Uniform ' + Result.Name + ' at program ' + AProgram + ' differs between shaders in ElementsCount');
        end;
    end;
    function LoadUniformsFromConstantReflection(st: TShaderType; Reflection: ID3D11ShaderReflection): TConstantBuffer;
    var I, N: Integer;
        s: String;
        UField: TUniformField_DX;
        UF : TUniformField;
        ShaderDesc: TD3D11_ShaderDesc;
        ShaderBufferDesc: TD3D11_ShaderBufferDesc;
        ShaderVarDesc: TD3D11_ShaderVariableDesc;
        ShaderTypeDesc: TD3D11_ShaderTypeDesc;
        ShaderBindDesc: TD3D11_ShaderInputBindDesc;
        CBufferReflect: ID3D11ShaderReflectionConstantBuffer;
    begin
      Result := nil;
      if Reflection = nil then Exit;
      Check3DError(Reflection.GetDesc(ShaderDesc));
      if ShaderDesc.ConstantBuffers > 0 then
      begin
        CBufferReflect := Reflection.GetConstantBufferByIndex(0);
        Check3DError(CBufferReflect.GetDesc(@ShaderBufferDesc));
        Result := TConstantBuffer.Create(FContext);
        Result.AllocMem(ShaderBufferDesc.Size, nil);
        for I := 0 to ShaderBufferDesc.Variables - 1 do
        begin
          Check3DError(CBufferReflect.GetVariableByIndex(I).GetDesc(ShaderVarDesc));
          Check3DError(CBufferReflect.GetVariableByIndex(I).GetType.GetDesc(ShaderTypeDesc));
          if ShaderVarDesc.Flags And Cardinal(D3D10_SVF_USED) = 0 then Continue;

          UField := AddUniformByDescs(ShaderVarDesc, ShaderTypeDesc);
          UField.FCB[st] := Result;
          UField.FData[st] := Result.Ptr;
          Inc(UField.FData[st], ShaderVarDesc.StartOffset);
          if Assigned(ShaderVarDesc.DefaultValue) then
              Move(ShaderVarDesc.DefaultValue^, UField.FData[st]^, ShaderVarDesc.Size)
          else
              FillChar(UField.FData[st]^, ShaderVarDesc.Size, 0);
          UField.Data := UField.FData[st];
          UField.DataSize := ShaderVarDesc.Size;
        end;
      end;

      for I := 0 to ShaderDesc.BoundResources - 1 do
      begin
          Check3DError(Reflection.GetResourceBindingDesc(I, ShaderBindDesc));
          if ShaderBindDesc._Type <> D3D10_SIT_TEXTURE then Continue;
          UField := AddUniformByDesc(ShaderBindDesc);
          UField.FResourceIndex[st] := ShaderBindDesc.BindPoint;
          UField.FSamplerIndex[st] := -1;
      end;

      //assign samplers to textures
      for I := 0 to ShaderDesc.BoundResources - 1 do
      begin
          Check3DError(Reflection.GetResourceBindingDesc(I, ShaderBindDesc));
          if ShaderBindDesc._Type <> D3D10_SIT_SAMPLER then Continue;
          s := string(ShaderBindDesc.Name);
          N := Pos('Sampler', s);
          if N = 0 then Continue;
          Delete(s, N, Length('Sampler'));
          UField := TUniformField_DX(GetUniformField(s));
          if UField = nil then Continue;
          UField.FSamplerIndex[st] := ShaderBindDesc.BindPoint;
      end;

      N := 0;
      FUniforms.Reset;
      while FUniforms.Next(s, UF) do
        if UF.DataClass = dcSampler then
          Inc(N);
      SetLength(FResUniforms, N);
      N := 0;
      FUniforms.Reset;
      while FUniforms.Next(s, UF) do
        if UF.DataClass = dcSampler then
        begin
          FResUniforms[N] := UF as TUniformField_DX;
          Inc(N);
        end;
    end;
var stream: TStream;
    ShaderRef: array [TShaderType] of ID3D11ShaderReflection;
    st: TShaderType;

    vShader: ID3D11VertexShader;
    gShader: ID3D11GeometryShader;
    pShader: ID3D11PixelShader;
begin
  stream := nil;
  ClearUniformList;
  for st := Low(TShaderType) to High(TShaderType) do
  begin
    FreeAndNil(FCB[st]);
    FShader[st] := nil;
  end;

  try
    if FromResource then
        stream := TResourceStream.Create(HInstance, AProgram, RT_RCDATA)
    else
        stream := TFileStream.Create(AProgram, fmOpenRead);
    StreamReadString(stream);//reading name

    for st := stVertex to stFragment do
    begin
      FData[st] := StreamReadShaderData(stream);
      if Length(FData[st]) > 0 then
      begin
          case st of
              stUnknown : ;
              stVertex  : begin
                            Check3DError(FContext.FDevice.CreateVertexShader(@FData[st][0], Length(FData[st]), nil, vShader));
                            FShader[st] := vShader;
                          end;
              stGeometry: begin
                            Check3DError(FContext.FDevice.CreateGeometryShader(@FData[st][0], Length(FData[st]), nil, gShader));
                            FShader[st] := gShader;
                          end;
              stFragment: begin
                            Check3DError(FContext.FDevice.CreatePixelShader(@FData[st][0], Length(FData[st]), nil, pShader));
                            FShader[st] := pShader;
                          end;
          end;
          Check3DError(D3DReflect(@FData[st][0], Length(FData[st]), ID3D11ShaderReflection, ShaderRef[st]));
      end;
    end;
  finally
    stream.Free;
  end;

  for st := stVertex to stFragment do
  begin
    FCB[st] := LoadUniformsFromConstantReflection(st, ShaderRef[st]);
    if Assigned(FCB[st]) then FCB[st].Invalidate;
  end;
end;

procedure TProgram.SetAttributes(const AModel, AInstances: IctxVetexBuffer;
  const AModelIndices: IctxIndexBuffer; InstanceStepRate: Integer);
var ILKey: TILKey;
begin
  if assigned(AModel) then
    ILKey.ModelRI := AModel.Layout
  else
    ILKey.ModelRI := nil;
  if assigned(AInstances) then
    ILKey.InstanceRI := AInstances.Layout
  else
    ILKey.InstanceRI := nil;
  SetIL(ILKey);

  if assigned(AInstances) then IctxVetexBuffer_DX(AInstances).Select(1);
  if assigned(AModel) then IctxVetexBuffer_DX(AModel).Select;
  if assigned(AModelIndices) then
    IctxIndexBuffer_DX(AModelIndices).Select
  else
    FContext.FDeviceContext.IASetIndexBuffer(nil, DXGI_FORMAT_UNKNOWN, 0);
end;

function TProgram.GetUniformField(const Name: string): TUniformField;
begin
  if not FUniforms.TryGetValue(Name, Result) then
    Result := nil;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const Value: integer);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform(DXField, Value, SizeOf(Value));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const Value: single);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform(DXField, Value, SizeOf(Value));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec2);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform(DXField, v, SizeOf(v));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec3);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform(DXField, v, SizeOf(v));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec4);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform(DXField, v, SizeOf(v));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const values: TSingleArr);
var DXField: TUniformField_DX absolute Field;
    PV: PVec4;
    Same: Boolean;
    i: Integer;
    st: TShaderType;
begin
  if Field = nil then Exit;

  Same := True;
  PV := PVec4(DXField.Data);
  for i := 0 to Length(values) - 1 do
  begin
      if PV^.x <> values[i] then
      begin
          Same := False;
          Break;
      end;
      Inc(PV);
  end;

  if Same then Exit;

  for st := stVertex to stFragment do
    if Assigned(DXField.FCB[st]) then
    begin
      PV := PVec4(DXField.FData[st]);
      for i := 0 to Length(values) - 1 do
      begin
          PV^.x := values[i];
          Inc(PV);
      end;
      DXField.FCB[st].Invalidate;
    end;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec4arr);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform(DXField, v[0], SizeOf(v[0]) * Length(v));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const m: TMat4);
var DXField: TUniformField_DX absolute Field;
    mt: TMat4;
begin
  mt := Transpose(m);
  SetUniform(DXField, mt, SizeOf(m));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const tex: IctxTexture; const Sampler: TSamplerInfo);
var DXField: TUniformField_DX absolute Field;
    dxtex: IctxTexture_DX11;
    resview: ID3D11ShaderResourceView;
    SamplerState: ID3D11SamplerState;
begin
  if Field = nil then Exit;
  if not Supports(tex, IctxTexture_DX11, dxtex) then Exit;
  resview := dxtex.GetResView;
  SamplerState := FContext.ObtainSamplerState(Sampler);
  DXField.SetResource(resview, SamplerState);
end;

procedure TProgram.Draw(PrimTopology: TPrimitiveType; CullMode: TCullingMode;
  IndexedGeometry: Boolean; InstanceCount: Integer; Start: integer;
  Count: integer; BaseVertex: integer; BaseInstance: Integer);
begin
  SyncCB;
  FContext.States.CullMode := CullMode;

  FContext.UpdateStates;
  if FContext.FPrimTopology <> PrimTopology then
      FContext.FDeviceContext.IASetPrimitiveTopology(DXPrimitiveType[PrimTopology]);
  FContext.FPrimTopology := PrimTopology;

  if IndexedGeometry then
  begin
    if InstanceCount = 0 then
      FContext.FDeviceContext.DrawIndexed(Count, Start, BaseVertex)
    else
      FContext.FDeviceContext.DrawIndexedInstanced(Count, InstanceCount, Start, BaseVertex, BaseInstance);
  end
  else
  begin
    if InstanceCount = 0 then
      FContext.FDeviceContext.Draw(Count, Start)
    else
      FContext.FDeviceContext.DrawInstanced(Count, InstanceCount, Start, BaseInstance);
  end;
end;

{ TStates }

procedure TStates.LoadDefaultState;
var i: Integer;
begin
  FBDescDirty := True;
  FBDesc.AlphaToCoverageEnable := False;
  FBDesc.IndependentBlendEnable := True;
  for i := 0 to 7 do
  begin
    FBDesc.RenderTarget[i].BlendEnable := False;
    FBDesc.RenderTarget[i].SrcBlend := D3D11_BLEND_ONE;
    FBDesc.RenderTarget[i].DestBlend := D3D11_BLEND_ZERO;
    FBDesc.RenderTarget[i].BlendOp := D3D11_BLEND_OP_ADD;
    FBDesc.RenderTarget[i].SrcBlendAlpha := D3D11_BLEND_ONE;
    FBDesc.RenderTarget[i].DestBlendAlpha := D3D11_BLEND_ZERO;
    FBDesc.RenderTarget[i].BlendOpAlpha := D3D11_BLEND_OP_ADD;
    FBDesc.RenderTarget[i].RenderTargetWriteMask := Byte(D3D11_COLOR_WRITE_ENABLE_ALL);
  end;

  FRDescDirty := True;
  FRDesc.FillMode := D3D11_FILL_SOLID;
  FRDesc.CullMode := D3D11_CULL_BACK;
  FRDesc.FrontCounterClockwise := False;
  FRDesc.DepthBias := 0;
  FRDesc.DepthBiasClamp := 0;
  FRDesc.SlopeScaledDepthBias := 0;
  FRDesc.DepthClipEnable := True;
  FRDesc.ScissorEnable := False;
  FRDesc.MultisampleEnable := False;
  FRDesc.AntialiasedLineEnable := False;

  FDDescDirty := True;
  // Depth test parameters
  FDDesc.DepthEnable := True;
  FDDesc.DepthWriteMask := D3D11_DEPTH_WRITE_MASK_ALL;
  FDDesc.DepthFunc := D3D11_COMPARISON_LESS;
  // Stencil test parameters
  FDDesc.StencilEnable := False;
  FDDesc.StencilReadMask := $FF;
  FDDesc.StencilWriteMask := $FF;
  // Stencil operations if pixel is front-facing
  FDDesc.FrontFace.StencilFailOp := D3D11_STENCIL_OP_KEEP;
  FDDesc.FrontFace.StencilDepthFailOp := D3D11_STENCIL_OP_INCR;
  FDDesc.FrontFace.StencilPassOp := D3D11_STENCIL_OP_KEEP;
  FDDesc.FrontFace.StencilFunc := D3D11_COMPARISON_ALWAYS;
  // Stencil operations if pixel is back-facing
  FDDesc.BackFace.StencilFailOp := D3D11_STENCIL_OP_KEEP;
  FDDesc.BackFace.StencilDepthFailOp := D3D11_STENCIL_OP_DECR;
  FDDesc.BackFace.StencilPassOp := D3D11_STENCIL_OP_KEEP;
  FDDesc.BackFace.StencilFunc := D3D11_COMPARISON_ALWAYS;
end;

procedure TStates.InvalidateAllStates;
begin
  FBDescDirty := True;
  FRDescDirty := True;
  FDDescDirty := True;
  FRasterStateLast := nil;
  FDepthStateLast := nil;
  FBlendStateLast := nil;
end;

procedure TStates.UpdateBlendState;
const DefBlendFactor: TColorArray = (1.0, 1.0, 1.0, 1.0);
var State: ID3D11BlendState;
begin
  if FBDescDirty then
  begin
    if not FBlendStates.TryGetValue(FBDesc, State) then
    begin
      FContext.FDevice.CreateBlendState(FBDesc, State);
      FBlendStates.Add(FBDesc, State);
    end;
    if FBlendStateLast <> State then
    begin
      FBlendStateLast := State;
      FContext.FDeviceContext.OMSetBlendState(State, DefBlendFactor, $FFFFFFFF);
    end;
    FBDescDirty := False;
  end;
end;

procedure TStates.UpdateRasterState;
var State: ID3D11RasterizerState;
begin
  if FRDescDirty then
  begin
    if not FRasterStates.TryGetValue(FRDesc, State) then
    begin
      FContext.FDevice.CreateRasterizerState(FRDesc, State);
      FRasterStates.Add(FRDesc, State);
    end;
    if FRasterStateLast <> State then
    begin
      FRasterStateLast := State;
      FContext.FDeviceContext.RSSetState(State);
    end;
    FRDescDirty := False;
  end;
end;

procedure TStates.UpdateDepthState;
var State: ID3D11DepthStencilState;
begin
  if FDDescDirty then
  begin
    if not FDepthStates.TryGetValue(FDDesc, State) then
    begin
      FContext.FDevice.CreateDepthStencilState(FDDesc, State);
      FDepthStates.Add(FDDesc, State);
    end;
    if (FDepthStateLast <> State) Or (FDStencilRefLast <> FDStencilRef) then
    begin
      FDepthStateLast := State;
      FDStencilRefLast := FDStencilRef;
      FContext.FDeviceContext.OMSetDepthStencilState(State, FDStencilRef);
    end;
    FDDescDirty := False;
  end;
end;

function TStates.GetBlendSrc(RenderTargetIndex: Integer): TBlendFunc;
begin

end;

function TStates.GetBlendDest(RenderTargetIndex: Integer): TBlendFunc;
begin

end;

function TStates.GetBlending: Boolean;
begin

end;

function TStates.GetColorWrite: Boolean;
begin

end;

function TStates.GetCullMode: TCullingMode;
begin

end;

function TStates.GetDepthFunc: TCompareFunc;
begin

end;

function TStates.GetDepthTest: Boolean;
begin

end;

function TStates.GetDepthWrite: Boolean;
begin

end;

function TStates.GetLineWidth: Single;
begin

end;

function TStates.GetNearFarClamp: Boolean;
begin

end;

function TStates.GetVertexProgramPointSize: Boolean;
begin

end;

function TStates.GetViewport: TRectI;
begin

end;

function TStates.GetWireframe: Boolean;
begin

end;

procedure TStates.SetCullMode(const Value: TCullingMode);
begin

end;

procedure TStates.SetLineWidth(const Value: Single);
begin

end;

procedure TStates.SetVertexProgramPointSize(const Value: Boolean);
begin

end;

procedure TStates.SetColorWrite(const Value: Boolean);
begin

end;

procedure TStates.SetDepthTest(const Value: Boolean);
begin

end;

procedure TStates.SetDepthWrite(const Value: Boolean);
begin

end;

procedure TStates.SetDepthFunc(const Value: TCompareFunc);
begin

end;

procedure TStates.SetNearFarClamp(const Value: Boolean);
begin

end;

procedure TStates.SetBlending(const Value: Boolean);
begin

end;

procedure TStates.SetViewport(const Value: TRectI);
begin

end;

procedure TStates.SetWireframe(const Value: Boolean);
begin

end;

procedure TStates.SetScissor(Enabled: Boolean; const Value: TRect);
begin

end;

procedure TStates.SetStencil(Enabled: Boolean; StencilFunc: TCompareFunc;
  Ref: Integer; Mask: Byte; sFail, dFail, dPass: TStencilAction);
begin

end;

procedure TStates.SetBlendFunctions(Src, Dest: TBlendFunc;
  RenderTargetIndex: Integer);
begin

end;

constructor TStates.Create(AContext: TContext_DX11);
begin
  FContext := AContext;

  FillChar(FBDesc, SizeOf(FBDesc), 0);
  FillChar(FRDesc, SizeOf(FRDesc), 0);
  FillChar(FDDesc, SizeOf(FDDesc), 0);

  FBlendStates  := TBlendStateMap.Create(FBDesc, nil);
  FRasterStates := TRasterStateMap.Create(FRDesc, nil);
  FDepthStates  := TDepthStateMap.Create(FDDesc, nil);

  LoadDefaultState;
end;

{ TColorSpaceConverter }

class function TColorSpaceConverter.Convert(ASrc: PByte; ASrcSize: Integer; ASrcFormat: TImageFormat; ADstFormat: TTextureFormat; out ADst: PByte; out ADstSize: Integer): Boolean;
type
  TComponentInfo = record
    DWordOffset : Integer;
    BitsOffset  : Integer;
    BitsCount   : Integer;
    CompType    : TComponentType;
  end;

  TImageFormatDesc = record
    RGBA : array [0..3] of TComponentInfo;
    StrideSize : Integer;
  end;

  function GetComponentValue(Data: PByte; Const Desc: TImageFormatDesc; CompIndex: Integer): Integer;
  var mask: Integer;
  begin
      Result := 0;
      if Desc.RGBA[CompIndex].CompType = ctBool then Exit;
      Inc(Data, Desc.RGBA[CompIndex].DWordOffset * 4);
      Move(Data^, Result, min(Desc.StrideSize, 4));
      Result := Result shr Desc.RGBA[CompIndex].BitsOffset;
      mask := (1 shl Desc.RGBA[CompIndex].BitsCount) - 1;
      Result := Result and mask;
  end;

  procedure SetComponentValue(Data: PByte; Const Desc: TImageFormatDesc; CompIndex: Integer; Value: Integer);
  var bSrc: PByte;
      i: Integer;
  begin
    Value := Value shl Desc.RGBA[CompIndex].BitsOffset;
    Inc(Data, Desc.RGBA[CompIndex].DWordOffset * 4);
    bSrc := PByte(Pointer(@Value));
    for i := 0 to min(Desc.StrideSize, 4) - 1 do
    begin
      Data^ := Data^ or bSrc^;
      Inc(Data);
      Inc(bSrc);
    end;
  end;

  function CompCharToIndex(ch: Char): Integer;
  begin
      case LowerCase(ch) of
        'r' : Result := 0;
        'g' : Result := 1;
        'b' : Result := 2;
        'a' : Result := 3;
      else
        Result := -1;
      end;
  end;

  function DecodeBitsCount(const s: string; var charindex: Integer): Integer;
  const Digits = ['0'..'9'];
  begin
      Result := 0;
      while (s[charindex] in Digits) do
      begin
        Result := Result * 10 + (Ord(s[charindex]) - Ord('0'));
        Inc(charindex);
      end;
  end;

  function DecodeImageFormat(const Format: TImageFormat): TImageFormatDesc;
  var Name: string;
      TotalOffset, CharIndex: Integer;
      CompIndex, BitsCount: Integer;
      i: Integer;
  begin
      Name := GetEnumName(TypeInfo(TImageFormat), Ord(Format));

      FillChar(Result, SizeOf(Result), 0);
      Result.RGBA[0].CompType := ctBool;
      Result.RGBA[1].CompType := ctBool;
      Result.RGBA[2].CompType := ctBool;
      Result.RGBA[3].CompType := ctBool;

      //standard RGBA decode
      TotalOffset := 0;
      CharIndex := 1;
      for i := 0 to 3 do
      begin
        CompIndex := CompCharToIndex(Name[CharIndex]);
        if CompIndex = -1 then Break;
        Inc(CharIndex);

        BitsCount := DecodeBitsCount(Name, CharIndex);
        if BitsCount <= 0 then Break;

        Result.RGBA[CompIndex].DWordOffset := TotalOffset div 32;
        Result.RGBA[CompIndex].BitsOffset := 32 - (TotalOffset mod 32) - BitsCount;
        Result.RGBA[CompIndex].BitsCount := BitsCount;
        Result.RGBA[CompIndex].CompType := ctInt;
        Inc(TotalOffset, BitsCount);
      end;
      Result.StrideSize := (TotalOffset + 7) div 8;

      if LowerCase(Name[Length(Name)-1]) = 'f' then
        for i := 0 to 3 do
          if Result.RGBA[i].CompType = ctInt then Result.RGBA[i].CompType := ctFloat;
  end;

  function DecodeTextureFormat(const Format: TTextureFormat): TImageFormatDesc;
  var Name: string;
      CharIndex: Integer;
      CompIndex, BitsCount: Integer;
      CompCount: Integer;
      i: Integer;
  begin
      Name := GetEnumName(TypeInfo(TTextureFormat), Ord(Format));

      FillChar(Result, SizeOf(Result), 0);
      Result.RGBA[0].CompType := ctBool;
      Result.RGBA[1].CompType := ctBool;
      Result.RGBA[2].CompType := ctBool;
      Result.RGBA[3].CompType := ctBool;

      CharIndex := 1;
      CompCount := 0;
      //standard RGBA decode
      for i := 0 to 3 do
      begin
        CompIndex := CompCharToIndex(Name[CharIndex]);
        if CompIndex = -1 then Break;
        Inc(CharIndex);
        Result.RGBA[CompIndex].CompType := ctInt;
        Inc(CompCount);
      end;
      if CompCount = 0 then Exit;

      BitsCount := DecodeBitsCount(Name, CharIndex);
      if BitsCount = 0 then BitsCount := 8;
      for i := 0 to 3 do
      begin
        Result.RGBA[i].DWordOffset := (BitsCount * i) div 32;
        if BitsCount = 8 then //todo check this condition for other bitcount images
          Result.RGBA[i].BitsOffset := i * BitsCount
        else
          Result.RGBA[i].BitsOffset := 32 - ((BitsCount * i) mod 32) - BitsCount;
        Result.RGBA[i].BitsCount := BitsCount;
      end;

      if LowerCase(Name[Length(Name)-1]) = 'f' then
        for i := 0 to 3 do
          if Result.RGBA[i].CompType = ctInt then Result.RGBA[i].CompType := ctFloat;

      Result.StrideSize := (CompCount * BitsCount + 7) div 8;
  end;

  function MatchFormat(const F1, F2: TImageFormatDesc): Boolean;
  var i: Integer;
  begin
      Result := True;
      if F1.StrideSize <> F2.StrideSize then Exit(False);
      for i := 0 to 3 do
      begin
        if (F1.RGBA[i].CompType = ctBool) <> (F2.RGBA[i].CompType = ctBool) then Exit(False);
        if F1.RGBA[i].CompType = ctBool then Continue;
        if F1.RGBA[i].BitsCount   <> F2.RGBA[i].BitsCount   then Exit(False);
        if F1.RGBA[i].DWordOffset <> F2.RGBA[i].DWordOffset then Exit(False);
        if F1.RGBA[i].BitsOffset  <> F2.RGBA[i].BitsOffset  then Exit(False);
      end;
  end;

var ImgFormat: TImageFormatDesc;
    TexFormat: TImageFormatDesc;
    PixelCount: Integer;
    SrcPixel, DstPixel: PByte;
    i, j: Integer;
begin
  ImgFormat := DecodeImageFormat(ASrcFormat);
  TexFormat := DecodeTextureFormat(ADstFormat);

  Assert(ImgFormat.StrideSize > 0);
  Assert(TexFormat.StrideSize > 0);

  if MatchFormat(ImgFormat, TexFormat) then
  begin
    ADst := ASrc;
    ADstSize := ASrcSize;
    Exit(False);
  end;

  PixelCount := ASrcSize div ImgFormat.StrideSize;
  ADstSize := PixelCount * TexFormat.StrideSize;
  GetMem(ADst, ADstSize);
  ZeroMemory(ADst, ADstSize);

  SrcPixel := ASrc;
  DstPixel := ADst;
  for j := 0 to PixelCount - 1 do
  begin
    for i := 0 to 3 do
    begin
      if TexFormat.RGBA[i].CompType = ctBool then Continue;
      SetComponentValue(DstPixel, TexFormat, i, GetComponentValue(SrcPixel, ImgFormat, i));
    end;
    Inc(SrcPixel, ImgFormat.StrideSize);
    Inc(DstPixel, TexFormat.StrideSize);
  end;
  Result := True;
end;

{ TTexture }

function TTexture.BuildDesc(AWidth, AHeight: Integer; WithMips: Boolean): TD3D11_Texture2DDesc;
begin
  Result.Width  := NextPow2(AWidth);
  Result.Height := NextPow2(AHeight);
  FWithMips     := WithMips;
  if WithMips then
    Result.MipLevels := GetMipsCount(Result.Width, Result.Height)
  else
    Result.MipLevels := 1;

  case FTargetFormat of
    TTextureFormat.D24_S8,
    TTextureFormat.D32f_S8,
    TTextureFormat.D16,
    TTextureFormat.D24,
    TTextureFormat.D32,
    TTextureFormat.D32f:
      Result.BindFlags := DWord(D3D11_BIND_SHADER_RESOURCE) or DWord(D3D11_BIND_DEPTH_STENCIL);
  else
    Result.BindFlags := DWord(D3D11_BIND_SHADER_RESOURCE) or DWord(D3D11_BIND_RENDER_TARGET);
  end;

  Result.ArraySize := 1;
  Result.Format := D3D11TextureFormat[FTargetFormat];
  Result.SampleDesc.Count := 1;
  Result.SampleDesc.Quality := 0;
  Result.Usage := D3D11_USAGE_DEFAULT;
  Result.CPUAccessFlags := 0;
  Result.MiscFlags := 0;
  FFormat := FTargetFormat;
end;

function TTexture.GetHandle: ID3D11Texture2D;
begin
  Result := FTexture;
end;

function TTexture.GetResView: ID3D11ShaderResourceView;
var desc: TD3D11_ShaderResourceViewDesc;
begin
  if FResView = nil then
  begin
    desc.Format := D3D11TextureFormat[FFormat];
    desc.ViewDimension := D3D10_SRV_DIMENSION_TEXTURE2D;
    desc.Texture2D.MostDetailedMip := 0;
    if FWithMips then
        desc.Texture2D.MipLevels := GetMipsCount(FWidth, FHeight)
    else
        desc.Texture2D.MipLevels := 1;
    Check3DError(FContext.FDevice.CreateShaderResourceView(FTexture, @desc, FResView));
  end;
  Result := FResView;
end;

function TTexture.GetTargetFormat: TTextureFormat;
begin
  Result := FTargetFormat;
end;

procedure TTexture.SetTargetFormat(Value: TTextureFormat);
begin
  FTargetFormat := Value;
end;

function TTexture.Width: Integer;
begin
  Result := FWidth;
end;

function TTexture.Height: Integer;
begin
  Result := FHeight;
end;

function TTexture.Format: TTextureFormat;
begin
  Result := FFormat;
end;

procedure TTexture.AllocMem(AWidth, AHeight: Integer; WithMips: Boolean);
var desc: TD3D11_Texture2DDesc;
begin
  FResView := nil;
  desc := BuildDesc(AWidth, AHeight, WithMips);
  Check3DError(FContext.FDevice.CreateTexture2D(desc, nil, FTexture));
  FWithMips := WithMips;
  FWidth := desc.Width;
  FHeight := desc.Height;
end;

procedure TTexture.AllocMem(AWidth, AHeight: Integer; WithMips: Boolean;
  DataFormat: TImageFormat; Data: PByte);
var desc: TD3D11_Texture2DDesc;
begin
  FResView := nil;
  desc := BuildDesc(AWidth, AHeight, WithMips);
  //todo: initialization with data
  Check3DError(FContext.FDevice.CreateTexture2D(desc, nil, FTexture));
  FWithMips := WithMips;
  FWidth := desc.Width;
  FHeight := desc.Height;
end;

procedure TTexture.SetImage(ImageWidth, ImageHeight: Integer; DataFormat: TImageFormat; Data: PByte; GenMipmaps: Boolean);
begin
  SetImage(0, 0, ImageWidth, ImageHeight, DataFormat, Data, GenMipmaps);
end;

procedure TTexture.SetImage(X, Y, ImageWidth, ImageHeight: Integer;
  DataFormat: TImageFormat; Data: PByte; GenMipmaps: Boolean);
var desc: TD3D11_Texture2DDesc;
begin
  FResView := nil;
  desc := BuildDesc(ImageWidth, ImageHeight, GenMipmaps);
  If GenMipmaps Then
    desc.MiscFlags := DWord(D3D11_RESOURCE_MISC_GENERATE_MIPS);

  Check3DError(FContext.FDevice.CreateTexture2D(desc, nil, FTexture));
  FWithMips := GenMipmaps;
  FWidth := desc.Width;
  FHeight := desc.Height;

  SetMipImage(X, Y, ImageWidth, ImageHeight, 0, DataFormat, Data);

  if GenMipmaps and Assigned(Data) then
    FContext.FDeviceContext.GenerateMips(GetResView);
end;

procedure TTexture.SetMipImage(X, Y, ImageWidth, ImageHeight, MipLevel: Integer; DataFormat: TImageFormat; Data: PByte);
var v: TVec4i;
begin
  v := Vec(X, Y, X + ImageWidth, Y + ImageHeight);
  SetMipImage(Rect(v), MipLevel, DataFormat, Data);
end;

procedure TTexture.SetMipImage(DestRect: TRect; MipLevel: Integer; DataFormat: TImageFormat; Data: PByte);
var tex  : PByte;
    imgWidth, imgHeight: Integer;
    texSize: Integer;
    texShouldFree: Boolean;
    Box: TD3D11_Box;
begin
    if Data = nil then Exit;

    imgWidth := (DestRect.Right - DestRect.Left);
    imgHeight := (DestRect.Bottom - DestRect.Top);
    if imgWidth <= 0 then Exit;
    if imgHeight <= 0 then Exit;

    Box.left := DestRect.Left;
    Box.top := DestRect.Top;
    Box.right := DestRect.Right;
    Box.bottom := DestRect.Bottom;
    Box.front := 0;
    Box.back := 1;

    texShouldFree := TColorSpaceConverter.Convert(Data, imgWidth * imgHeight * ImagePixelSize[DataFormat], DataFormat, TargetFormat, tex, texSize);
    try
      FContext.FDeviceContext.UpdateSubresource(FTexture, MipLevel, @Box, tex, imgWidth * ImagePixelSize[DataFormat], 0);
    finally
      if texShouldFree then FreeMem(tex);
    end;
end;

{ TFrameBuffer }

function TFrameBuffer.GetObj: TFrameBuffer;
begin
  Result := Self;
end;

procedure TFrameBuffer.Select;
var i: Integer;
    RTDesc: TD3D11_RenderTargetViewDesc;
    DSDesc: TD3D11_DepthStencilViewDesc;
begin
  if not FValid then
  begin
    for i := 0 to Length(FTex) - 1 do
    begin
      if FTex[i].Tex = nil then
      begin
        FViews[i] := nil;
        Continue;
      end;
      FillChar(RTDesc, SizeOf(RTDesc), 0);
      RTDesc.Format := D3D11ViewFormat[FTex[i].Tex.Format];
      RTDesc.ViewDimension := D3D11_RTV_DIMENSION_TEXTURE2D;
      RTDesc.Texture2D.MipSlice := FTex[i].Mip;

      Check3DError(FContext.FDevice.CreateRenderTargetView((FTex[i].Tex as IctxTexture_DX11).GetHandle, @RTDesc, FViews[i]));
    end;

    if Assigned(FDepthTex) then
    begin
      FillChar(DSDesc, SizeOf(DSDesc), 0);
      DSDesc.Format := D3D11ViewFormat[FDepthTex.Format];
      DSDesc.ViewDimension := D3D11_DSV_DIMENSION_TEXTURE2D;
      DSDesc.Texture2D.MipSlice := FDepthMip;

      Check3DError(FContext.FDevice.CreateDepthStencilView((FDepthTex as IctxTexture_DX11).GetHandle, @DSDesc, FDepthView));
    end
    else
      FDepthView := nil;

    FValid := True;
  end;
  FContext.SetFrameBuffer(Self);
end;

procedure TFrameBuffer.ClearColorList;
begin
  FViews := nil;
  FTex := nil;
  FValid := False;
end;

procedure TFrameBuffer.EnableColorTarget(index: Integer; Enabled: Boolean);
begin
  Assert(index >= 0);
  Assert(index < Length(FTex));
  FTex[index].Enabled := Enabled;
end;

procedure TFrameBuffer.SetColor(index: Integer; tex: IctxTexture; mipLevel: Integer);
begin
  if index <= Length(FTex) then
  begin
    SetLength(FViews, index + 1);
    SetLength(FTex, index + 1);
  end;

  if FTex[index].Tex = tex then Exit;

  FTex[index].Tex := tex;
  FTex[index].Mip := mipLevel;
  FTex[index].Enabled := True;
  FViews[index] := nil;

  FValid := False;
end;

procedure TFrameBuffer.SetDepthStencil(tex: IctxTexture; mipLevel: Integer);
begin
  if FDepthTex = tex then Exit;

  FDepthTex := tex;
  FDepthView := nil;
  FDepthMip := mipLevel;

  FValid := False;
end;

procedure TFrameBuffer.Clear(index: Integer; color: TVec4);
begin
  Assert(FViews[index] <> nil);
  FContext.FDeviceContext.ClearRenderTargetView(FViews[index], TColorArray(color));
end;

procedure TFrameBuffer.ClearDS(depth: Single; clearDepth: Boolean;
  stencil: Integer; clearStencil: Boolean);
var flags: DWord;
begin
  Assert(FDepthView <> nil);
  flags := 0;
  if clearDepth then flags := flags or DWord(D3D11_CLEAR_DEPTH);
  if clearStencil then flags := flags or DWord(D3D11_CLEAR_STENCIL);
  if flags = 0 then Exit;
  FContext.FDeviceContext.ClearDepthStencilView(FDepthView, flags, depth, stencil);
end;

procedure TFrameBuffer.BlitToWindow(index: Integer; const srcRect,
  dstRect: TRectI; const Filter: TTextureFilter);
var SrcBox: TD3D11_Box;
begin
  //todo blitting with stretch
  if FTex[index].Tex = nil then Exit;
  SrcBox.left := srcRect.Left;
  SrcBox.top := srcRect.Top;
  SrcBox.right := srcRect.Right;
  SrcBox.bottom := srcRect.Bottom;
  SrcBox.front := 0;
  SrcBox.back := 1;
  FContext.FDeviceContext.CopySubresourceRegion(FContext.FBackBuffer, 0, dstRect.Left, dstRect.Top, 0,
                                                (FTex[index].Tex as IctxTexture_DX11).GetHandle, FTex[index].Mip, @SrcBox);
end;

{ THandleObject }

constructor THandleObject.Create(const AContext: TContext_DX11);
begin
  Assert(AContext<>nil, 'AContext = nil');
  FContext := AContext;
end;

destructor THandleObject.Destroy;
begin
  FContext := nil;
  inherited Destroy;
end;

{ TContext_DX11 }

function TContext_DX11.GetActiveProgram: IctxProgram;
begin

end;

procedure TContext_DX11.SetActiveProgram(AValue: IctxProgram);
begin

end;

procedure TContext_DX11.RebuildViews(const AWidth, AHeight: Cardinal);
var SwapChainDesc: TDXGI_SwapChainDesc;
    ViewPort: TD3D11_Viewport;
begin
  FSwapChain.GetDesc(SwapChainDesc);
  if (SwapChainDesc.BufferDesc.Width = AWidth) and
     (SwapChainDesc.BufferDesc.Height = AHeight) and
     Assigned(FRenderTarget) then Exit;
  FRenderTarget := nil;
  FBackBuffer := nil;

  FDeviceContext.ClearState;
  //TD3D10States(FStatesObj).InvalidateAllStates;
  FSwapChain.ResizeBuffers(SwapChainDesc.BufferCount, AWidth, AHeight, SwapChainDesc.BufferDesc.Format, SwapChainDesc.Flags);

  Check3DError(FSwapChain.GetBuffer(0, ID3D11Texture2D, FBackBuffer));
  Check3DError(FDevice.CreateRenderTargetView(FBackBuffer, nil, FRenderTarget));

  FDeviceContext.OMSetRenderTargets(1, @FRenderTarget, nil);
  //FDevice.IASetPrimitiveTopology(D3D10PrimitiveType[FPrimTopology]);
  ViewPort.TopLeftX := 0;
  ViewPort.TopLeftY := 0;
  ViewPort.Width := AWidth;
  ViewPort.Height := AHeight;
  ViewPort.MinDepth := 0;
  ViewPort.MaxDepth := 1;
  FDeviceContext.RSSetViewports(1, @ViewPort);
end;

procedure TContext_DX11.SetFrameBuffer(const AObject: TObject);
  procedure SetFBORenderTargets(const FBO: TFrameBuffer);
  var
      dummy: ID3D11RenderTargetView;
  begin
      dummy := nil;
      FDeviceContext.OMSetRenderTargets(1, @dummy, nil);
      FDeviceContext.OMSetRenderTargets(Length(FBO.FViews), @FBO.FViews[0], FBO.FDepthView);
  end;
  procedure SetDefaultRenderTargets;
  var
      dummy: ID3D11RenderTargetView;
  begin
      dummy := nil;
      FDeviceContext.OMSetRenderTargets(1, @dummy, nil);
      FDeviceContext.OMSetRenderTargets(1, @FRenderTarget, nil);
  end;
var FBO: TFrameBuffer absolute AObject;
begin
  FActiveFrameBuffer := FBO;
  if assigned(FBO) then
  begin
      SetFBORenderTargets(FBO);
  end
  else
  begin
      SetDefaultRenderTargets;
  end;
end;

function TContext_DX11.ObtainSamplerState(const ASampler: TSamplerInfo): ID3D11SamplerState;
                            //min           //mag           //mip
  const DX_Filter: array [TTextureFilter, TTextureFilter, TTextureFilter] of TD3D11_Filter =
    (
       //min
       ( //mag
         (D3D11_FILTER_MIN_MAG_MIP_POINT             , D3D11_FILTER_MIN_MAG_MIP_POINT             , D3D11_FILTER_MIN_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_MIN_MAG_MIP_POINT             , D3D11_FILTER_MIN_MAG_MIP_POINT             , D3D11_FILTER_MIN_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT, D3D11_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT, D3D11_FILTER_MIN_POINT_MAG_MIP_LINEAR)
       ),
       ( //mag
         (D3D11_FILTER_MIN_MAG_MIP_POINT             , D3D11_FILTER_MIN_MAG_MIP_POINT             , D3D11_FILTER_MIN_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_MIN_MAG_MIP_POINT             , D3D11_FILTER_MIN_MAG_MIP_POINT             , D3D11_FILTER_MIN_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT, D3D11_FILTER_MIN_POINT_MAG_LINEAR_MIP_POINT, D3D11_FILTER_MIN_POINT_MAG_MIP_LINEAR)
       ),
       ( //mag
         (D3D11_FILTER_MIN_LINEAR_MAG_MIP_POINT      , D3D11_FILTER_MIN_LINEAR_MAG_MIP_POINT      , D3D11_FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_MIN_LINEAR_MAG_MIP_POINT      , D3D11_FILTER_MIN_LINEAR_MAG_MIP_POINT      , D3D11_FILTER_MIN_LINEAR_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_MIN_MAG_LINEAR_MIP_POINT      , D3D11_FILTER_MIN_MAG_LINEAR_MIP_POINT      , D3D11_FILTER_MIN_MAG_MIP_LINEAR)
       )
    );
  const D3D11_Wrap: array [TTextureWrap] of TD3D11_TextureAddressMode = (D3D11_TEXTURE_ADDRESS_WRAP,   //twRepeat
                                                                         D3D11_TEXTURE_ADDRESS_MIRROR, //twMirror
                                                                         D3D11_TEXTURE_ADDRESS_CLAMP,  //twClamp
                                                                         D3D11_TEXTURE_ADDRESS_BORDER);//twClampToEdge
var Desc: TD3D11_SamplerDesc;
begin
  if not FSamplerMap.TryGetValue(ASampler, Result) then
  begin
    Desc.Filter := DX_Filter[ASampler.MinFilter, ASampler.MagFilter, ASampler.MipFilter];
    Desc.AddressU := D3D11_Wrap[ASampler.Wrap_X];
    Desc.AddressV := D3D11_Wrap[ASampler.Wrap_Y];
    Desc.AddressW := D3D11_TEXTURE_ADDRESS_WRAP;
    Desc.MipLODBias := 0;
    Desc.MaxAnisotropy := Min(16, Max(1, ASampler.Anisotropy));
    Desc.ComparisonFunc := D3D11_COMPARISON_ALWAYS;
    Desc.MinLOD := 0;
    Desc.MaxLOD := D3D11_FLOAT32_MAX;
    Desc.BorderColor[0] := ASampler.Border.x;
    Desc.BorderColor[1] := ASampler.Border.y;
    Desc.BorderColor[2] := ASampler.Border.z;
    Desc.BorderColor[3] := ASampler.Border.w;
    Result := nil;
    Check3DError(FDevice.CreateSamplerState(Desc, Result));
    FSamplerMap.Add(ASampler, Result);
  end;
end;

procedure TContext_DX11.UpdateStates;
begin

end;

function TContext_DX11.CreateVertexBuffer: IctxVetexBuffer;
begin
  Result := TVertexBuffer.Create(Self);
end;

function TContext_DX11.CreateIndexBuffer: IctxIndexBuffer;
begin
  Result := TIndexBuffer.Create(Self);
end;

function TContext_DX11.CreateProgram: IctxProgram;
begin
  Result := TProgram.Create(Self);
end;

function TContext_DX11.CreateTexture: IctxTexture;
begin
  Result := TTexture.Create(Self);
end;

function TContext_DX11.CreateFrameBuffer: IctxFrameBuffer;
begin
  Result := TFrameBuffer.Create(Self);
end;

function TContext_DX11.States: IRenderStates;
begin
  Result := FStatesIntf;
end;

function TContext_DX11.Binded: Boolean;
begin
  Result := FBindCount > 0;
end;

function TContext_DX11.Bind: Boolean;
var WndRect: TRectI;
    WndSize: TVec2i;
begin
  Result := True;
  if FBindCount > 0 then
  begin
    Inc(FBindCount);
    Exit;
  end;
  WndRect := GetRectOfWindow(FWnd);
  WndSize := WndRect.Size;
  if (WndSize.x <= 0) or (WndSize.y <= 0) then Exit(False);
  RebuildViews(WndSize.x, WndSize.y);
  Inc(FBindCount);
end;

function TContext_DX11.Unbind: Boolean;
begin
  Dec(FBindCount);
  Assert(FBindCount = 0, '???');
  Result := True;
end;

procedure TContext_DX11.Clear(const color: TVec4; doColor: Boolean;
  depth: Single; doDepth: Boolean; stencil: Byte; doStencil: Boolean);
var FBO: TFrameBuffer;
    i: Integer;
begin
  if Assigned(FActiveFrameBuffer) then
    FBO := (FActiveFrameBuffer as IctxFrameBuffer_DX11).GetObj;
  if Assigned(FBO) then
  begin
    for i := 0 to Length(FBO.FViews) - 1 do
      FBO.Clear(i, color);
    FBO.ClearDS(depth, doDepth, stencil, doStencil);
  end
  Else
  begin
    FDeviceContext.ClearRenderTargetView(FRenderTarget, TColorArray(color));
    //FDeviceContext.ClearDepthStencilView();
  end;
end;

procedure TContext_DX11.Present;
begin
  Check3DError(FSwapChain.Present(0, 0));
end;

constructor TContext_DX11.Create(const Wnd: TWindow);
var SwapChainDesc: TDXGI_SwapChainDesc;
    EmptySampler: TSamplerInfo;
begin
  FStates := TStates.Create(Self);
  FStatesIntf := TStates(FStates);

  FillChar(EmptySampler, SizeOf(EmptySampler), 0);
  FSamplerMap := TSamplerMap.Create(EmptySampler, nil);

  FWnd := Wnd;

  SwapChainDesc.BufferCount := 1;
  SwapChainDesc.BufferDesc.Width := 0;
  SwapChainDesc.BufferDesc.Height := 0;
  SwapChainDesc.BufferDesc.Format := DXGI_FORMAT_R8G8B8A8_UNORM;
  SwapChainDesc.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
  SwapChainDesc.SampleDesc.Count := 1;//Max(1, MultiSampleLevel);
  SwapChainDesc.SampleDesc.Quality := 0;
  SwapChainDesc.OutputWindow := Wnd;
  SwapChainDesc.Windowed := True;
  SwapChainDesc.SwapEffect := DXGI_SWAP_EFFECT_DISCARD;

  Check3DError(
    D3D11CreateDeviceAndSwapChain(nil,
                                  D3D_DRIVER_TYPE_HARDWARE, 0, LongWord(D3D11_CREATE_DEVICE_SINGLETHREADED), nil, 0, D3D11_SDK_VERSION,
                                  @SwapChainDesc, FSwapChain, FDevice, nil, FDeviceContext)
  );
end;

destructor TContext_DX11.Destroy;
begin
  FDeviceContext.ClearState;
  FRenderTarget := nil;
  FBackBuffer := nil;
  FSwapChain := nil;
  FDevice := nil;
  FDeviceContext := nil;
  FStatesIntf := nil;
  FreeAndNil(FStates);
  inherited Destroy;
end;

end.

