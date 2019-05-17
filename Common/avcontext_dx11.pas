unit avContext_DX11;
{$I avConfig.inc}

interface

uses
  Classes, SysUtils, avTypes, avPlatform, avContext, avContnrs, mutils,
  D3D11_JSB, DXGI_JSB, DXTypes_JSB, D3DCommon_JSB;

type
  TavInterfacedObject = TInterfacedObject;

  IRenderContext_DX11 = interface
  ['{E8D0F003-32BD-4F7E-9336-5338770F193E}']
    function GetDevice: ID3D11Device;
    function GetDeviceContext: ID3D11DeviceContext;
    function GetSwapChain: IDXGISwapChain;
  end;

  { TContext_DX11 }

  TContext_DX11 = class (TavInterfacedObject, IRenderContext, IRenderContext_DX11)
  private type
    TSamplerMap = {$IfDef FPC}specialize{$EndIf} THashMap<TSamplerInfo, ID3D11SamplerState>;
    ISamplerMap = {$IfDef FPC}specialize{$EndIf} IHashMap<TSamplerInfo, ID3D11SamplerState>;
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

    FPrimTopology : TD3D11_PrimitiveTopology;

    function GetActiveProgram: IctxProgram;
    procedure SetActiveProgram(AValue: IctxProgram);

    procedure RebuildViews(const AWidth, AHeight: Cardinal);
    procedure SetFrameBuffer(const AObject: TObject); //TFrameBuffer
    function ObtainSamplerState(const ASampler: TSamplerInfo): ID3D11SamplerState;
  public
    function GetDevice: ID3D11Device;
    function GetDeviceContext: ID3D11DeviceContext;
    function GetSwapChain: IDXGISwapChain;

    procedure UpdateStates;

    function CreateVertexBuffer : IctxVetexBuffer;
    function CreateIndexBuffer : IctxIndexBuffer;
    function CreateStructBuffer: IctxStructuredBuffer;
    function CreateProgram : IctxProgram;
    function CreateTexture : IctxTexture;
    function CreateTexture3D : IctxTexture3D;
    function CreateFrameBuffer : IctxFrameBuffer;
    function CreateUAV(const AElementsCount, AStrideSize: Cardinal; const Appendable: Boolean; const AInitialData: Pointer): IctxUAV;

    function States : IRenderStates;
    property ActiveProgram: IctxProgram read GetActiveProgram write SetActiveProgram;

    function Binded: Boolean;
    function Bind: Boolean;
    function Unbind: Boolean;

    procedure Clear(const color  : TVec4;      doColor  : Boolean = True;
                          depth  : Single = 1; doDepth  : Boolean = False;
                          stencil: Byte   = 0; doStencil: Boolean = False);
    procedure Flush;
    procedure Present;

    constructor Create(Const Wnd: TWindow; Const WARP: Boolean = False);
    destructor Destroy; override;
  end;

procedure Check3DError(hr: HRESULT);

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
  {R32} DXGI_FORMAT_R32_UINT,
  {R32f} DXGI_FORMAT_R32_FLOAT,
  {DXT1} DXGI_FORMAT_BC1_UNORM,
  {DXT3} DXGI_FORMAT_BC2_UNORM,
  {DXT5} DXGI_FORMAT_BC3_UNORM,
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
  {R32} DXGI_FORMAT_R32_UINT,
  {R32f} DXGI_FORMAT_R32_FLOAT,
  {DXT1} DXGI_FORMAT_BC1_UNORM,
  {DXT3} DXGI_FORMAT_BC2_UNORM,
  {DXT5} DXGI_FORMAT_BC3_UNORM,
  {D24_S8} DXGI_FORMAT_D24_UNORM_S8_UINT,
  {D32f_S8} DXGI_FORMAT_D32_FLOAT_S8X24_UINT,
  {D16} DXGI_FORMAT_D16_UNORM,
  {D24} DXGI_FORMAT_D24_UNORM_S8_UINT,
  {D32} DXGI_FORMAT_UNKNOWN,
  {D32f} DXGI_FORMAT_D32_FLOAT
  );

  D3D11ShaderViewFormat: array [TTextureFormat] of TDXGI_Format = (
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
  {R32} DXGI_FORMAT_R32_UINT,
  {R32f} DXGI_FORMAT_R32_FLOAT,
  {DXT1} DXGI_FORMAT_UNKNOWN,
  {DXT3} DXGI_FORMAT_UNKNOWN,
  {DXT5} DXGI_FORMAT_UNKNOWN,
  {D24_S8} DXGI_FORMAT_UNKNOWN,
  {D32f_S8} DXGI_FORMAT_R32_FLOAT_X8X24_TYPELESS,
  {D16} DXGI_FORMAT_R16_FLOAT,
  {D24} DXGI_FORMAT_R24_UNORM_X8_TYPELESS,
  {D32} DXGI_FORMAT_R32_SINT,
  {D32f} DXGI_FORMAT_R32_FLOAT
  );

  DXPrimitiveType: array [TPrimitiveType] of TD3D11_PrimitiveTopology = ( {ptPoints}            D3D11_PRIMITIVE_TOPOLOGY_POINTLIST,
                                                                          {ptLines}             D3D11_PRIMITIVE_TOPOLOGY_LINELIST,
                                                                          {ptLineStrip}         D3D11_PRIMITIVE_TOPOLOGY_LINESTRIP,
                                                                          {ptTriangles}         D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST,
                                                                          {ptTriangleStrip}     D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP,
                                                                          {ptLines_Adj}         D3D11_PRIMITIVE_TOPOLOGY_LINELIST_ADJ,
                                                                          {ptLineStrip_Adj}     D3D11_PRIMITIVE_TOPOLOGY_LINESTRIP_ADJ,
                                                                          {ptTriangles_Adj}     D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST_ADJ,
                                                                          {ptTriangleStrip_Adj} D3D11_PRIMITIVE_TOPOLOGY_TRIANGLESTRIP_ADJ,
                                                                          {ptPatches}           D3D11_PRIMITIVE_TOPOLOGY_1_CONTROL_POINT_PATCHLIST);

  DXCompareFunc : array [TCompareFunc] of TD3D11_ComparisonFunc = (
    D3D11_COMPARISON_NEVER,         // cfNever
    D3D11_COMPARISON_LESS,          // cfLess
    D3D11_COMPARISON_EQUAL,         // cfEqual
    D3D11_COMPARISON_NOT_EQUAL,     // cfNotEqual
    D3D11_COMPARISON_LESS_EQUAL,    // cfLessEqual
    D3D11_COMPARISON_GREATER,       // cfGreater
    D3D11_COMPARISON_GREATER_EQUAL, // cfGreaterEqual
    D3D11_COMPARISON_ALWAYS         // cfAlways
  );

function Add_sRGB(const AFormat: TDXGI_Format): TDXGI_Format;
begin
  case AFormat of
    DXGI_FORMAT_R8G8B8A8_UNORM : Result := DXGI_FORMAT_R8G8B8A8_UNORM_SRGB;
    DXGI_FORMAT_BC1_UNORM : Result := DXGI_FORMAT_BC1_UNORM_SRGB;
    DXGI_FORMAT_BC2_UNORM : Result := DXGI_FORMAT_BC2_UNORM_SRGB;
    DXGI_FORMAT_BC3_UNORM : Result := DXGI_FORMAT_BC3_UNORM_SRGB;
  else
    Result := AFormat;
  end;
end;

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
    const DXCullMode : array [TCullingMode] of D3D11_CULL_MODE = (D3D11_CULL_NONE, D3D11_CULL_BACK, D3D11_CULL_FRONT);

    const DXBlend : array [TBlendFunc] of D3D11_BLEND = (
      D3D11_BLEND_ZERO,         // bfZero
      D3D11_BLEND_ONE,          // bfOne
      D3D11_BLEND_SRC_ALPHA,    // bfSrcAlpha
      D3D11_BLEND_INV_SRC_ALPHA,// bfInvSrcAlpha
      D3D11_BLEND_DEST_ALPHA,    // bfDstAlpha
      D3D11_BLEND_INV_DEST_ALPHA,// bfInvDstAlpha
      D3D11_BLEND_SRC_COLOR,    // bfSrcColor
      D3D11_BLEND_DEST_COLOR    // bfDstColor
    );

    const DXBlendOp : array [TBlendOp] of D3D11_BLEND_OP = (
      D3D11_BLEND_OP_ADD,          // boAdd
      D3D11_BLEND_OP_SUBTRACT,     // boSub
      D3D11_BLEND_OP_REV_SUBTRACT, // boRevSub
      D3D11_BLEND_OP_MIN,          // boMin
      D3D11_BLEND_OP_MAX           // boMax
    );

    const DXDepthMask : array [Boolean] of D3D11_DEPTH_WRITE_MASK = (
      D3D11_DEPTH_WRITE_MASK_ZERO,
      D3D11_DEPTH_WRITE_MASK_ALL
    );

    const AVDepthFunc : array [TD3D11_ComparisonFunc] of TCompareFunc = (
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
    TBlendStateMap = {$IfDef FPC}specialize{$EndIf} THashMap<TD3D11_BlendDesc, ID3D11BlendState>;
    IBlendStateMap = {$IfDef FPC}specialize{$EndIf} IHashMap<TD3D11_BlendDesc, ID3D11BlendState>;

    TRasterStateMap = {$IfDef FPC}specialize{$EndIf} THashMap<TD3D11_RasterizerDesc, ID3D11RasterizerState>;
    IRasterStateMap = {$IfDef FPC}specialize{$EndIf} IHashMap<TD3D11_RasterizerDesc, ID3D11RasterizerState>;

    TDepthStateMap = {$IfDef FPC}specialize{$EndIf} THashMap<TD3D11_DepthStencilDesc, ID3D11DepthStencilState>;
    IDepthStateMap = {$IfDef FPC}specialize{$EndIf} IHashMap<TD3D11_DepthStencilDesc, ID3D11DepthStencilState>;
  private
    FContext: TContext_DX11;

    FViewport: TRectI;
    FScissor : TRectI;

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

    procedure SetupViewPort();
  protected
    // getters/setters
    function GetBlendSrc (RenderTargetIndex: Integer = AllTargets): TBlendFunc;
    function GetBlendDest(RenderTargetIndex: Integer = AllTargets): TBlendFunc;
    function GetBlending (RenderTargetIndex: Integer = AllTargets): Boolean;
    function GetColorMask(RenderTargetIndex: Integer = AllTargets): TColorMask;
    function GetCullMode               : TCullingMode;
    function GetDepthFunc              : TCompareFunc;
    function GetDepthTest              : Boolean;
    function GetDepthWrite             : Boolean;
    function GetNearFarClamp           : Boolean;
    function GetViewport               : TRectI;
    function GetScissor                : TRectI;
    function GetScissorTest            : Boolean;
    function GetWireframe              : Boolean;
    procedure SetCullMode              (const Value : TCullingMode);
    procedure SetDepthTest             (const Value : Boolean);
    procedure SetDepthWrite            (const Value : Boolean);
    procedure SetDepthFunc             (const Value : TCompareFunc);
    procedure SetNearFarClamp          (const Value : Boolean);
    procedure SetBlending              (RenderTargetIndex: Integer; const Value : Boolean);
    procedure SetColorMask             (RenderTargetIndex: Integer; const Value : TColorMask);
    procedure SetViewport              (const Value : TRectI);
    procedure SetScissor               (const Value : TRectI);
    procedure SetScissorTest           (const Value : Boolean);
    procedure SetWireframe             (const Value : Boolean);

    procedure SetStencil(Enabled : Boolean; StencilFunc : TCompareFunc; Ref : Integer; Mask : Byte; sFail, dFail, dPass : TStencilAction);
    procedure SetBlendFunctions(Src, Dest : TBlendFunc; RenderTargetIndex: Integer = AllTargets);
    procedure SetBlendFunctions_SeparateAlpha(Src, Dest, AlphaSrc, AlphaDest : TBlendFunc; RenderTargetIndex: Integer = AllTargets);
    procedure SetBlendOperation(BlendOp : TBlendOp; RenderTargetIndex: Integer = AllTargets);
    procedure SetBlendOperation_SeparateAlpha(BlendOp, AlphaBlendOp : TBlendOp; RenderTargetIndex: Integer = AllTargets);
    // getters/setters

    property Scissor                : TRectI       read GetScissor                write SetScissor;
    property SicssorTest            : Boolean      read GetScissorTest            write SetScissorTest;
  public
    constructor Create(AContext: TContext_DX11);
  end;


  { TColorSpaceConverter }

  TColorSpaceConverter = class
  private type
    TConvertMethod = function (ASrc: PByte; ASrcSize: Integer; ASrcFormat: TImageFormat; ADstFormat: TTextureFormat; out ADst: PByte; out ADstSize: Integer): Boolean of object;
    class var FSpecConvertes : array [TImageFormat] of array [TTextureFormat] of TConvertMethod;
  public
    class function Convert_A8R8G8B8_RGBA(ASrc: PByte; ASrcSize: Integer; ASrcFormat: TImageFormat; ADstFormat: TTextureFormat; out ADst: PByte; out ADstSize: Integer): Boolean;
    class function Convert(ASrc: PByte; ASrcSize: Integer; ASrcFormat: TImageFormat; ADstFormat: TTextureFormat; out ADst: PByte; out ADstSize: Integer): Boolean;
    class constructor Create;
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
    function GetResCubeView: ID3D11ShaderResourceView;
  end;

  IctxUAV_DX11 = interface
  ['{07D7CCE8-A382-4667-8A3A-BEF73928C2A5}']
    procedure InvalidateCounter;
    function GetView: ID3D11UnorderedAccessView;
  end;

  IctxTexture3D_DX11 = interface(IctxTexture3D)
  ['{C0A913F6-36E9-43B4-A289-2357D31A3B8A}']
    function GetHandle : ID3D11Texture3D;
    function GetResView: ID3D11ShaderResourceView;
  end;

  { TTexture }

  TTexture = class (THandleObject, IctxTexture, IctxTexture_DX11, IctxUAV_DX11)
  private
    FTargetFormat: TTextureFormat;
    FWidth: Integer;
    FHeight: Integer;
    FDeep: Integer;
    FForcedArray: Boolean;
    FFormat: TTextureFormat;
    FsRGB: Boolean;
    FMipsCount: Integer;
    FSampleCount: Integer;

    FTexture: ID3D11Texture2D;
    FResView: ID3D11ShaderResourceView;
    FCubeResView: ID3D11ShaderResourceView;

    FUAVView: ID3D11UnorderedAccessView;

    function BuildDesc(AWidth, AHeight, ADeep: Integer; WithMips: Boolean): TD3D11_Texture2DDesc;
  private //IctxTexture_DX11
    function GetHandle : ID3D11Texture2D;
    function GetResView: ID3D11ShaderResourceView;
    function GetResCubeView: ID3D11ShaderResourceView;
  private //IctxUAV_DX11
    procedure InvalidateCounter;
    function GetView: ID3D11UnorderedAccessView;
  public
    //*******
    function GetTargetFormat: TTextureFormat;
    function Get_sRGB: Boolean;
    procedure SetTargetFormat(Value: TTextureFormat);
    procedure Set_sRGB(Value: Boolean);
    //*******
    property TargetFormat: TTextureFormat read GetTargetFormat write SetTargetFormat;

    function Width : Integer;
    function Height: Integer;
    function Deep  : Integer;
    function MipsCount: Integer;
    function SampleCount: Integer;
    function Format: TTextureFormat;

    procedure AllocMultiSampled(AWidth, AHeight, ASampleCount: Integer); overload;

    procedure AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean; ForcedArray: Boolean); overload;
    procedure AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean; DataFormat: TImageFormat; Data: PByte; ForcedArray: Boolean); overload;

    procedure SetMipImage(X, Y, ImageWidth, ImageHeight, MipLevel, ZSlice: Integer; DataFormat: TImageFormat; Data: PByte); overload;
    procedure SetMipImage(DestRect: TRect; MipLevel, ZSlice: Integer; DataFormat: TImageFormat; Data: PByte); overload;

    procedure CopyFrom(const DstMipLevel: Integer; const DstPos: TVec2I;
                       const ASrcRes: IctxTexture; const SrcMipLevel: Integer; const SrcRect: TRectI);

    procedure GenerateMips;

    procedure ReadBack(const ATexData: ITextureData; const ASlice: Integer; const AMipLevel: Integer); //-1 for all mip levels
  end;

  { TTexture3D }

  TTexture3D = class(THandleObject, IctxTexture3D, IctxTexture3D_DX11, IctxUAV_DX11)
  private
    FTargetFormat: TTextureFormat;
    FSize: TVec3i;
    FFormat: TTextureFormat;
    FsRGB: Boolean;
    FMipsCount: Integer;

    FTexture: ID3D11Texture3D;
    FResView: ID3D11ShaderResourceView;
    FUAVView: ID3D11UnorderedAccessView;
  private
    function BuildDesc(AWidth, AHeight, ADeep: Integer; WithMips: Boolean): TD3D11_Texture3DDesc;
  private //IctxTexture3D_DX11
    function GetHandle : ID3D11Texture3D;
    function GetResView: ID3D11ShaderResourceView;
  private //IctxUAV_DX11
    procedure InvalidateCounter;
    function GetView: ID3D11UnorderedAccessView;
  public
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

  TFrameBuffer = class;

  IctxFrameBuffer_DX11 = interface
  ['{7CF255BF-6B18-49E9-927A-8C2D59D36A0C}']
    function GetObj: TFrameBuffer;
  end;

  { TFrameBuffer }

  TFrameBuffer = class (THandleObject, IctxFrameBuffer, IctxFrameBuffer_DX11)
  private type
    TTexInfo = record
      Tex2D     : IctxTexture;
      Tex3D     : IctxTexture3D;
      Mip       : Integer;
      SliceStart: Integer;
      SliceCount: Integer;
      Enabled   : Boolean;
    end;
    TUAVInfo = record
      UAV : IctxUAV_DX11;
    end;
    TSOInfo = record
      SO : IctxVetexBuffer;
    end;
  private
    FTex   : array of TTexInfo;
    FViews : array of ID3D11RenderTargetView;

    FUAV : array of TUAVInfo;
    FUAVViews : array of ID3D11UnorderedAccessView;
    FUAVInitials : array of Cardinal;

    FStreams: array of TSOInfo;
    FStreamBuffers: array of ID3D11Buffer;
    FStreamOffsets: array of Integer;

    FDepthView : ID3D11DepthStencilView;
    FDepthTex  : IctxTexture;
    FDepthMip  : Integer;
    FDepthSliceStart: Integer;
    FDepthSliceCount: Integer;

    FValid: Boolean;
    function GetObj: TFrameBuffer;

    procedure SetUAV_Internal(index: Integer; const UAV: IctxUAV_DX11);
  public
    procedure Select;

    procedure ClearColorList;
    procedure EnableColorTarget(index: Integer; Enabled: Boolean);
    procedure SetColor(index: Integer; tex: IctxTexture; mipLevel: Integer = 0; sliceStart: Integer = -1; sliceCount: Integer = 0);
    procedure SetColor3D(index: Integer; tex: IctxTexture3D; mipLevel: Integer = 0; sliceStart: Integer = -1; sliceCount: Integer = 0);
    procedure SetUAVTex(index: Integer; UAV: IctxTexture);
    procedure SetUAVTex3D(index: Integer; UAV: IctxTexture3D);
    procedure SetUAV(index: Integer; UAV: IctxUAV);
    procedure SetDepthStencil(tex: IctxTexture; mipLevel: Integer = 0; sliceStart: Integer = -1; sliceCount: Integer = 0);
    procedure SetStreamOut(index: Integer; buffer: IctxVetexBuffer; Offset: Integer);

    procedure Clear(index: Integer; color: TVec4);
    procedure ClearDS(depth: Single; clearDepth: Boolean = True; stencil: Integer = 0; clearStencil: Boolean = False);
    procedure ClearUAV(index: Integer; color: TVec4i);
    procedure ResetUAVCounters;

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
    function GetBufferMiscFlag: Cardinal; virtual;
    function GetStructuredByteStride: Cardinal; virtual;
  public
    function GetTargetPoolType: TBufferPoolType;
    procedure SetTargetPoolType(Value: TBufferPoolType);
    property TargetPoolType: TBufferPoolType read GetTargetPoolType write SetTargetPoolType;

    function Size: Integer;

    function Map(usage: TMapingUsage): PByte;
    function Unmap: Boolean;

    procedure AllocMem(ASize: Integer; Data: PByte); overload; virtual;
    procedure SetSubData(AOffset, ASize: Integer; Data: PByte); overload;

    procedure AfterConstruction; override;
  end;

  { IctxVetexBuffer_DX }

  IctxVetexBuffer_DX = interface(IctxVetexBuffer)
  ['{87083775-37DB-41A6-8DA8-FC3189E6060F}']
    procedure Select(Slot: Integer = 0);
    function Handle: ID3D11Buffer;
  end;

  { TVertexBuffer }

  TVertexBuffer = class(TBufferBase, IctxVetexBuffer, IctxVetexBuffer_DX)
  private
    FLayout: IDataLayout;
  protected
    function GetBufferBindFlag: Cardinal; override;
  public
    //*******
    function GetLayout: IDataLayout;
    procedure SetLayout(const AValue: IDataLayout);
    //*******
    function VertexCount: Integer;
    property Layout: IDataLayout read GetLayout write SetLayout;

    procedure Select(Slot: Integer = 0);
    function Handle: ID3D11Buffer;
  end;

  { IctxStructuredBuffer_DX }

  IctxStructuredBuffer_DX = interface(IctxStructuredBuffer)
  ['{64DB6CFF-DE71-451F-BE3E-4BD71C8C3473}']
    function Handle: ID3D11Buffer;
    function View  : ID3D11ShaderResourceView;
  end;

  { TStructuredBuffer }

  TStructuredBuffer = class(TBufferBase, IctxStructuredBuffer, IctxStructuredBuffer_DX)
  private
    FView  : ID3D11ShaderResourceView;
    FElSize: Integer;

    function  GetElementSize: Integer;
    procedure SetElementSize(const AValue: Integer);
  protected
    function GetBufferBindFlag: Cardinal; override;
    function GetBufferMiscFlag: Cardinal; override;
    function GetStructuredByteStride: Cardinal; override;
  public
    procedure AllocMem(ASize: Integer; Data: PByte); overload; override;

    function Handle: ID3D11Buffer;
    function View: ID3D11ShaderResourceView;
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
  protected
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

  { TUAV }

  TUAV = class(THandleObject, IctxUAV, IctxUAV_DX11, IctxStructuredBuffer_DX)
  private
    FElementsCount: Cardinal;
    FStrideSize: Cardinal;
    FBuffer: ID3D11Buffer;
    FView: ID3D11UnorderedAccessView;

    FShaderView: ID3D11ShaderResourceView;

    FLastCounterValid: Boolean;
    FLastCounter: Cardinal;

    function ElementsCount: Cardinal;
    function StrideSize: Cardinal;

    procedure InvalidateCounter;
    function ReadCounter: Cardinal;
    function ReadRAWData(AElementsCount: Integer = -1): TByteArr;

    function GetView: ID3D11UnorderedAccessView;
  private //IctxBuffer
    function GetTargetPoolType : TBufferPoolType;
    procedure SetTargetPoolType(Value : TBufferPoolType);

    function Size : Integer;

    function Map(usage: TMapingUsage): PByte;
    function Unmap: Boolean;
    procedure AllocMem(ASize : Integer; Data : PByte); Overload;
    procedure SetSubData(AOffset, ASize : Integer; Data : PByte); Overload;
  private //IctxStructuredBuffer
    function  GetElementSize: Integer;
    procedure SetElementSize(const AValue: Integer);
  private //IctxStructuredBuffer_DX
    function Handle: ID3D11Buffer;
    function View  : ID3D11ShaderResourceView;
  public
    constructor Create(const AContext: TContext_DX11; const AElementsCount, AStrideSize: Cardinal; const Appendable: Boolean; const AInitialData: Pointer); reintroduce;
    //destructor Destroy; override;
  end;

  { TConstantBuffer }

  TConstantBuffer = class(TBufferBase)
  private
    FData: array of Byte;
    FDirty: Boolean;
    procedure SyncToGPU; {$IFNDEF NoInline} inline; {$ENDIF}
  protected
    function GetBufferBindFlag: Cardinal; override;
  public
    function Ptr: PByte;
    procedure AllocMem(ASize: Integer; Data: PByte); overload; override;
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

    TILMap = {$IfDef FPC}specialize{$EndIf} THashMap<TILKey, TILInfo>;
    IILMap = {$IfDef FPC}specialize{$EndIf} IHashMap<TILKey, TILInfo>;

    TUniformMap = {$IfDef FPC}specialize{$EndIf} THashMap<string, TUniformField>;
    IUniformMap = {$IfDef FPC}specialize{$EndIf} IHashMap<string, TUniformField>;
  private
    FILMap: IILMap;

    FData  : array [TShaderType] of TByteArr;
    FShader: array [TShaderType] of ID3D11DeviceChild;
    FCB    : array [TShaderType] of TConstantBuffer;

    FResUniforms: array of TUniformField_DX;
    FUniforms: IUniformMap;

    FSelectedPathSize: Integer;

    FUAVList: array of IctxUAV_DX11;

    function ObtainUniformField(const name: string; out WasCreated: Boolean): TUniformField_DX;
    procedure ClearUniformList;
    procedure ClearILList;

    procedure SetIL(const AKey: TILKey);

    procedure SetUniform_internal(const DXField: TUniformField_DX; const data; const datasize: Integer); overload; //inline;
    procedure SetUniform_internal(const DXField: TUniformField_DX; const tex: ID3D11ShaderResourceView; const Sampler: TSamplerInfo); overload;
  public
    procedure AfterConstruction; override;
    destructor Destroy; override;

    procedure SyncCB;
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
    procedure SetUniform(const Field: TUniformField; const tex: IctxTexture3D; const Sampler: TSamplerInfo); overload;
    procedure SetUniform(const Field: TUniformField; const buf: IctxStructuredBuffer); overload;

    procedure SetComputeUAV(const Index: Integer; const uav: IctxUAV; const initial: Integer);
    procedure SetComputeTex3D(const Index: Integer; const uav: IctxTexture3D);

    procedure Draw(PrimTopology: TPrimitiveType; CullMode: TCullingMode; IndexedGeometry: Boolean;
                   InstanceCount: Integer;
                   Start: integer; Count: integer;
                   BaseVertex: integer; BaseInstance: Integer);
    procedure DispatchDraw(GroupDims: TVec3i);
    procedure ClearComputeUAV(const Index: Integer; const color: TVec4i);
    procedure ResetUAVCounter(const Index: Integer);
  end;

{ TTexture3D }

function TTexture3D.BuildDesc(AWidth, AHeight, ADeep: Integer; WithMips: Boolean): TD3D11_Texture3DDesc;
begin
  Result.Width  := AWidth;
  Result.Height := AHeight;
  Result.Depth := ADeep;

  if WithMips then
    FMipsCount := GetMipsCount(Result.Width, Result.Height, Result.Depth)
  else
    FMipsCount := 1;
  Result.MipLevels := FMipsCount;

  Result.BindFlags := DWord(D3D11_BIND_SHADER_RESOURCE) or DWord(D3D11_BIND_RENDER_TARGET);

  case FTargetFormat of
    TTextureFormat.R32f,
    TTextureFormat.R32:
      Result.BindFlags := Result.BindFlags or DWord(D3D11_BIND_UNORDERED_ACCESS);
  end;

  Result.Format := D3D11TextureFormat[FTargetFormat];
  if FsRGB then Result.Format := Add_sRGB(Result.Format);
  Result.Usage := D3D11_USAGE_DEFAULT;

  Result.CPUAccessFlags := 0;
  Result.MiscFlags := 0;
  if WithMips and (Result.BindFlags and DWord(D3D11_BIND_RENDER_TARGET) = DWord(D3D11_BIND_RENDER_TARGET))  then
    Result.MiscFlags := Result.MiscFlags or DWord(D3D11_RESOURCE_MISC_GENERATE_MIPS);
  FFormat := FTargetFormat;
end;

function TTexture3D.GetHandle: ID3D11Texture3D;
begin
  Result := FTexture;
end;

function TTexture3D.GetResView: ID3D11ShaderResourceView;
var desc: TD3D11_ShaderResourceViewDesc;
begin
  if FResView = nil then
  begin
    desc.Format := D3D11ShaderViewFormat[FFormat];
    if FsRGB then desc.Format := Add_sRGB(desc.Format);
    desc.ViewDimension := D3D11_SRV_DIMENSION_TEXTURE3D;
    desc.Texture3D.MostDetailedMip := 0;
    desc.Texture3D.MipLevels := FMipsCount;
    Check3DError(FContext.FDevice.CreateShaderResourceView(FTexture, @desc, FResView));
  end;
  Result := FResView;
end;

procedure TTexture3D.InvalidateCounter;
begin

end;

function TTexture3D.GetView: ID3D11UnorderedAccessView;
var
  uavdesc: TD3D11_UnorderedAccessViewDesc;
begin
  if FUAVView = nil then
  begin
    uavdesc.Format := D3D11ShaderViewFormat[FFormat];

    uavdesc.ViewDimension := D3D11_UAV_DIMENSION_TEXTURE3D;
    uavdesc.Texture3D.MipSlice := 0;
    uavdesc.Texture3D.FirstWSlice := 0;
    uavdesc.Texture3D.WSize := FSize.z;

    Check3DError(FContext.FDevice.CreateUnorderedAccessView(FTexture, @uavdesc, FUAVView));
  end;
  Result := FUAVView;
end;

function TTexture3D.GetTargetFormat: TTextureFormat;
begin
  Result := FTargetFormat;
end;

function TTexture3D.Get_sRGB: Boolean;
begin
  Result := FsRGB;
end;

procedure TTexture3D.SetTargetFormat(Value: TTextureFormat);
begin
  FTargetFormat := Value;
end;

procedure TTexture3D.Set_sRGB(Value: Boolean);
begin

end;

function TTexture3D.Width: Integer;
begin
  Result := FSize.x;
end;

function TTexture3D.Height: Integer;
begin
  Result := FSize.y;
end;

function TTexture3D.Deep: Integer;
begin
  Result := FSize.z;
end;

function TTexture3D.MipsCount: Integer;
begin
  Result := FMipsCount;
end;

function TTexture3D.Format: TTextureFormat;
begin
  Result := FFormat;
end;

procedure TTexture3D.AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean);
var desc: TD3D11_Texture3DDesc;
begin
  FResView := nil;
  FUAVView := nil;
  desc := BuildDesc(AWidth, AHeight, ADeep, WithMips);
  Check3DError(FContext.FDevice.CreateTexture3D(desc, nil, FTexture));
  FSize := Vec(AWidth, AHeight, ADeep);
end;

procedure TTexture3D.GenerateMips;
begin
  FContext.FDeviceContext.GenerateMips(GetResView);
end;

{ TUniformField_DX }

procedure TUniformField_DX.BindResource(const Device: ID3D11DeviceContext);
begin
  if not FResChanged then Exit;

  if FResourceIndex[stVertex]>=0 then
    Device.VSSetShaderResources(FResourceIndex[stVertex], 1, @FResourceView);
  if FSamplerIndex[stVertex]>=0 then
    Device.VSSetSamplers(FSamplerIndex[stVertex], 1, @FSamplerState);

  if FResourceIndex[stTessControl]>=0 then
    Device.HSSetShaderResources(FResourceIndex[stTessControl], 1, @FResourceView);
  if FSamplerIndex[stTessControl]>=0 then
    Device.HSSetSamplers(FSamplerIndex[stTessControl], 1, @FSamplerState);

  if FResourceIndex[stTessEval]>=0 then
    Device.DSSetShaderResources(FResourceIndex[stTessEval], 1, @FResourceView);
  if FSamplerIndex[stTessEval]>=0 then
    Device.DSSetSamplers(FSamplerIndex[stTessEval], 1, @FSamplerState);

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
  if (FTargetPool = TBufferPoolType.StaticDraw) then
    Result := Result or DWord(D3D11_BIND_STREAM_OUTPUT);
end;

function TVertexBuffer.GetLayout: IDataLayout;
begin
  Result := FLayout;
end;

function TVertexBuffer.Handle: ID3D11Buffer;
begin
  Result := FBuffer;
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

function TBufferBase.GetBufferMiscFlag: Cardinal;
begin
  Result := 0;
end;

function TBufferBase.GetStructuredByteStride: Cardinal;
begin
  Result := 0;
end;

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
  Desc.MiscFlags := GetBufferMiscFlag;
  Desc.StructureByteStride := GetStructuredByteStride;

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
    if N = 0 then
    begin
      FContext.FDeviceContext.IASetInputLayout(nil);
      Exit;
    end;

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

procedure TProgram.SetUniform_internal(const DXField: TUniformField_DX; const data; const datasize: Integer);
var st: TShaderType;
begin
  if DXField = nil then Exit;
  if {$IFDEF NOCACHE_UNIFORMS}True{$ELSE}not CompareMem(DXField.Data, @data, datasize){$ENDIF} then
  begin
    for st := stVertex to High(TShaderType) do
      if assigned(DXField.FCB[st]) then
      begin
        Move(data, DXField.FData[st]^, datasize);
        DXField.FCB[st].Invalidate;
      end;
  end;
end;

procedure TProgram.SetUniform_internal(const DXField: TUniformField_DX; const tex: ID3D11ShaderResourceView; const Sampler: TSamplerInfo);
var SamplerState: ID3D11SamplerState;
begin
  SamplerState := FContext.ObtainSamplerState(Sampler);
  DXField.SetResource(tex, SamplerState);

  if DXField.FResourceIndex[stVertex]>=0 then
    FContext.FDeviceContext.VSSetShaderResources(DXField.FResourceIndex[stVertex], 1, @tex);
  if DXField.FSamplerIndex[stVertex]>=0 then
    FContext.FDeviceContext.VSSetSamplers(DXField.FSamplerIndex[stVertex], 1, @SamplerState);

  if DXField.FResourceIndex[stTessControl]>=0 then
      FContext.FDeviceContext.HSSetShaderResources(DXField.FResourceIndex[stTessControl], 1, @tex);
  if DXField.FSamplerIndex[stTessControl]>=0 then
      FContext.FDeviceContext.HSSetSamplers(DXField.FSamplerIndex[stTessControl], 1, @SamplerState);

  if DXField.FResourceIndex[stTessEval]>=0 then
      FContext.FDeviceContext.DSSetShaderResources(DXField.FResourceIndex[stTessEval], 1, @tex);
  if DXField.FSamplerIndex[stTessEval]>=0 then
      FContext.FDeviceContext.DSSetSamplers(DXField.FSamplerIndex[stTessEval], 1, @SamplerState);

  if DXField.FResourceIndex[stGeometry]>=0 then
      FContext.FDeviceContext.GSSetShaderResources(DXField.FResourceIndex[stGeometry], 1, @tex);
  if DXField.FSamplerIndex[stGeometry]>=0 then
      FContext.FDeviceContext.GSSetSamplers(DXField.FSamplerIndex[stGeometry], 1, @SamplerState);

  if DXField.FResourceIndex[stFragment]>=0 then
    FContext.FDeviceContext.PSSetShaderResources(DXField.FResourceIndex[stFragment], 1, @tex);
  if DXField.FSamplerIndex[stFragment]>=0 then
    FContext.FDeviceContext.PSSetSamplers(DXField.FSamplerIndex[stFragment], 1, @SamplerState);

  if DXField.FResourceIndex[stCompute]>=0 then
    FContext.FDeviceContext.CSSetShaderResources(DXField.FResourceIndex[stCompute], 1, @tex);
  if DXField.FSamplerIndex[stCompute]>=0 then
    FContext.FDeviceContext.CSSetSamplers(DXField.FSamplerIndex[stCompute], 1, @SamplerState);
end;

procedure TProgram.AfterConstruction;
begin
  inherited AfterConstruction;
  FILMap := TILMap.Create;
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
  for st := stVertex to High(TShaderType) do
    if Assigned(FCB[st]) then
        FCB[st].SyncToGPU;
  for i := 0 to Length(FResUniforms) - 1 do
    FResUniforms[i].BindResource(FContext.FDeviceContext);
end;

procedure TProgram.Select(const APatchSize: Integer);
var st: TShaderType;
    i: Integer;
begin
  FContext.FActiveProgram := Self;
  FContext.FDeviceContext.VSSetShader(ID3D11VertexShader(FShader[stVertex]), nil, 0);
  FContext.FDeviceContext.HSSetShader(ID3D11HullShader(FShader[stTessControl]), nil, 0);
  FContext.FDeviceContext.DSSetShader(ID3D11DomainShader(FShader[stTessEval]), nil, 0);
  FContext.FDeviceContext.GSSetShader(ID3D11GeometryShader(FShader[stGeometry]), nil, 0);
  FContext.FDeviceContext.PSSetShader(ID3D11PixelShader(FShader[stFragment]), nil, 0);
  FContext.FDeviceContext.CSSetShader(ID3D11ComputeShader(FShader[stCompute]), nil, 0);
  if FShader[stCompute] <> nil then
  begin
    if Assigned(FCB[stCompute]) then
    begin
      FCB[stCompute].SyncToGPU;
      FContext.FDeviceContext.CSSetConstantBuffers(0, 1, @FCB[stCompute].FBuffer);
    end;
  end
  else
  begin
    for st := stVertex to stFragment do
      if Assigned(FCB[st]) then
      begin
        FCB[st].SyncToGPU;
        case st of
          stVertex     : FContext.FDeviceContext.VSSetConstantBuffers(0, 1, @FCB[st].FBuffer);
          stTessControl: FContext.FDeviceContext.HSSetConstantBuffers(0, 1, @FCB[st].FBuffer);
          stTessEval   : FContext.FDeviceContext.DSSetConstantBuffers(0, 1, @FCB[st].FBuffer);
          stGeometry   : FContext.FDeviceContext.GSSetConstantBuffers(0, 1, @FCB[st].FBuffer);
          stFragment   : FContext.FDeviceContext.PSSetConstantBuffers(0, 1, @FCB[st].FBuffer);
        end;
      end;
    if APatchSize > 0 then
      FSelectedPathSize := APatchSize;
  end;
  for i := 0 to Length(FResUniforms) - 1 do
    FResUniforms[i].FResChanged := True;
end;

procedure TProgram.Load(const AProgram: string; FromResource: Boolean; const AStreamOutLayout: IDataLayout);

  type
    TD3D11_SoDeclarationEntryArr = array of TD3D11_SoDeclarationEntry;
    TOutDeclNames = array of AnsiString;

    procedure ReadCodeData(stream: TStream; var Data: TByteArr);
    var n: Integer;
    begin
      {$IfDef FPC}
      n := 0;
      {$EndIf}
      stream.ReadBuffer(n, SizeOf(n));
      SetLength(Data, n);
      stream.ReadBuffer(Data[0], n);
    end;

    function ReadUniformChunk(Stream: TStream; st: TShaderType): TConstantBuffer;
    var blockName: AnsiString;
        blockSize: Integer;
        i, n, n2: Integer;
        UName: AnsiString;
        UField: TUniformField_DX;
        WasCreated: Boolean;
        b: Byte;
        Offset: Integer;
    begin
      {$IfDef FPC}
      n2 := 0;
      Offset := 0;
      b := 0;
      n := 0;
      blockSize := 0;
      {$EndIf}

      Result := Nil;
      StreamReadString(Stream, blockName);
      Stream.ReadBuffer(blockSize, SizeOf(blockSize));
      if blockSize > 0 then
      begin
        Result := TConstantBuffer.Create(FContext);
        Result.AllocMem(Ceil(blockSize/16)*16, nil);
      end;

      Stream.ReadBuffer(n, SizeOf(n));
      for i := 0 to n-1 do
      begin
        StreamReadString(Stream, UName);
        UField := ObtainUniformField(UName, WasCreated);
        UField.FCB[st] := Result;
        UField.FData[st] := nil;

        Stream.ReadBuffer(b, 1);
        UField.DataClass := TDataClass(b);
        Stream.ReadBuffer(b, 1);
        UField.ElementType := TComponentType(b);
        Stream.ReadBuffer(UField.ItemsCount, SizeOf(UField.ItemsCount));
        Stream.ReadBuffer(UField.ElementsCount, SizeOf(UField.ElementsCount));
        Stream.ReadBuffer(Offset, SizeOf(Offset));
        case UField.DataClass of
          dcSampler, dcCubeSampler:
            begin
              UField.FCB[st] := nil;
              UField.FResourceIndex[st] := Offset;
            end
        else
          UField.FData[st] := Result.Ptr;
          if Offset > 0 then
            Inc(UField.FData[st], Offset);
        end;
        Stream.ReadBuffer(UField.FSamplerIndex[st], SizeOf(UField.FSamplerIndex[st]));
        Stream.ReadBuffer(n2, SizeOf(n2));
        if n2 > 0 then
          Stream.ReadBuffer(UField.FData[st]^, n2);

        UField.Data := UField.FData[st];
        UField.DataSize := n2;
      end;
    end;

    function GetShaderTypeByChunk(const AChunkID: TFOURCC): TShaderType;
    var i: TShaderType;
    begin
      for i := Low(TShaderType) to High(TShaderType) do
        if AChunkID = ShaderType_FourCC[i] then Exit(i);
      Result := stUnknown;
    end;

    function GetOutDeclaration(const ALayout: IDataLayout; var ADeclNames: TOutDeclNames): TD3D11_SoDeclarationEntryArr;
    var
      i: Integer;
    begin
      SetLength(Result, ALayout.Count);
      SetLength(ADeclNames, ALayout.Count);
      for i := 0 to ALayout.Count - 1 do
      begin
        Assert(ALayout.Item[i].CompType = ctFloat, 'Output layout can contains only float components');
        ADeclNames[i] := ALayout.Item[i].Name;
        Result[i].Stream := 0;
        Result[i].SemanticName := PAnsiChar(ADeclNames[i]);
        Result[i].StartComponent := 0;
        Result[i].ComponentCount := ALayout.Item[i].CompCount;
        Result[i].OutputSlot := 0;
      end;
    end;

var stream: TStream;
    st: TShaderType;

    ShaderIntf: ID3D11DeviceChild;
    OutDecl: TD3D11_SoDeclarationEntryArr;
    OutDeclNames: TOutDeclNames;
    OutStrideSize: Cardinal;
    ShaderData: TByteArr;

    ChunkID: TFOURCC;
    ChunkSize: Cardinal;
    NewPos, NewPos2 : Int64;
    i: Integer;
begin
  ChunkID := 0;
  ChunkSize := 0;
  OutDeclNames := nil;

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

    while Stream.Position <> Stream.Size do
    begin
      stream.ReadBuffer(ChunkID, SizeOf(ChunkID));
      stream.ReadBuffer(ChunkSize, SizeOf(ChunkSize));
      NewPos := stream.Position + ChunkSize;

      try
        st := GetShaderTypeByChunk(ChunkID);
        if st = stUnknown then Continue;

        for i := 0 to 1 do
        begin
          stream.ReadBuffer(ChunkID, SizeOf(ChunkID));
          stream.ReadBuffer(ChunkSize, SizeOf(ChunkSize));
          NewPos2 := stream.Position + ChunkSize;
          try
            if ChunkID = MakeFourCC('C','O','D','E') then
            begin
              ReadCodeData(stream, FData[st]);
              case st of
                stVertex     : Check3DError(FContext.FDevice.CreateVertexShader  (@FData[st][0], Length(FData[st]), nil, ID3D11VertexShader  (ShaderIntf)));
                stTessControl: Check3DError(FContext.FDevice.CreateHullShader    (@FData[st][0], Length(FData[st]), nil, ID3D11HullShader    (ShaderIntf)));
                stTessEval   : Check3DError(FContext.FDevice.CreateDomainShader  (@FData[st][0], Length(FData[st]), nil, ID3D11DomainShader  (ShaderIntf)));
                stGeometry   : begin
                                 if AStreamOutLayout = nil then
                                   Check3DError(FContext.FDevice.CreateGeometryShader(@FData[st][0], Length(FData[st]), nil, ID3D11GeometryShader(ShaderIntf)))
                                 else
                                 begin
                                   OutDecl := GetOutDeclaration(AStreamOutLayout, OutDeclNames);
                                   OutStrideSize := AStreamOutLayout.Size;
                                   Check3DError( FContext.FDevice.CreateGeometryShaderWithStreamOutput(
                                      @FData[st][0], Length(FData[st]),
                                      @OutDecl[0], Length(OutDecl),
                                      @OutStrideSize, 1,
                                      D3D11_SO_NO_RASTERIZED_STREAM,
                                      nil,
                                      ID3D11GeometryShader(ShaderIntf)
                                   ));
                                 end;
                               end;
                stFragment   : Check3DError(FContext.FDevice.CreatePixelShader   (@FData[st][0], Length(FData[st]), nil, ID3D11PixelShader   (ShaderIntf)));
                stCompute    : Check3DError(FContext.FDevice.CreateComputeShader (@FData[st][0], Length(FData[st]), nil, ID3D11ComputeShader (ShaderIntf)));
              end;
              FShader[st] := ShaderIntf;
              Continue;
            end;

            if ChunkID = MakeFourCC('U','B','L','K') then
            begin
              FCB[st] := ReadUniformChunk(stream, st);
              Continue;
            end;

            Assert(False, 'Only 2 type of chunk availble');
          finally
            stream.Position := NewPos2;
          end;
        end;

      finally
        stream.Position := NewPos;
      end;
    end;
  finally
    stream.Free;
  end;

  if (AStreamOutLayout <> nil) and (FShader[stGeometry] = nil) then
  begin
    if FShader[stTessEval] <> nil then
      ShaderData := FData[stTessEval];
    if (ShaderData = nil) and (FShader[stVertex] <> nil) then
      ShaderData := FData[stVertex];
    if ShaderData <> nil then
    begin
       ShaderIntf := nil;
       OutDecl := GetOutDeclaration(AStreamOutLayout, OutDeclNames);
       OutStrideSize := AStreamOutLayout.Size;
       Check3DError( FContext.FDevice.CreateGeometryShaderWithStreamOutput(
          @ShaderData[0], Length(ShaderData),
          @OutDecl[0], Length(OutDecl),
          @OutStrideSize, 1,
          D3D11_SO_NO_RASTERIZED_STREAM,
          nil,
          ID3D11GeometryShader(ShaderIntf)
       ));
       FShader[stGeometry] := ShaderIntf;
    end;
  end;

  for st := stVertex to High(TShaderType) do
    if Assigned(FCB[st]) then FCB[st].Invalidate;
end;

procedure TProgram.SetAttributes(const AModel, AInstances: IctxVetexBuffer;
  const AModelIndices: IctxIndexBuffer; InstanceStepRate: Integer);
var ILKey: TILKey;
begin
  if assigned(AModel) then
  begin
    Assert(AModel.Size > 0);
    ILKey.ModelRI := AModel.Layout
  end
  else
    ILKey.ModelRI := nil;
  if assigned(AInstances) then
  begin
    Assert(AInstances.Size > 0);
    ILKey.InstanceRI := AInstances.Layout
  end
  else
    ILKey.InstanceRI := nil;
  SetIL(ILKey);

  if assigned(AInstances) then IctxVetexBuffer_DX(AInstances).Select(1);
  if assigned(AModel) then IctxVetexBuffer_DX(AModel).Select;
  if assigned(AModelIndices) then
  begin
    Assert(AModelIndices.Size > 0);
    IctxIndexBuffer_DX(AModelIndices).Select
  end
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
  SetUniform_internal(DXField, Value, SizeOf(Value));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const Value: single);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform_internal(DXField, Value, SizeOf(Value));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec2);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform_internal(DXField, v, SizeOf(v));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec3);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform_internal(DXField, v, SizeOf(v));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec4);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform_internal(DXField, v, SizeOf(v));
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

  for st := stVertex to High(TShaderType) do
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
  SetUniform_internal(DXField, v[0], SizeOf(v[0]) * Length(v));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const m: TMat4);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform_internal(DXField, m, SizeOf(m));
end;

procedure TProgram.SetUniform(const Field: TUniformField; const m: PMat4; const mCount: Integer);
var DXField: TUniformField_DX absolute Field;
begin
  SetUniform_internal(DXField, m^, SizeOf(TMat4)*mCount);
end;

procedure TProgram.SetUniform(const Field: TUniformField; const tex: IctxTexture; const Sampler: TSamplerInfo);
var DXField: TUniformField_DX absolute Field;
    dxtex: IctxTexture_DX11;
    resview: ID3D11ShaderResourceView;
begin
  if Field = nil then Exit;
  if not Supports(tex, IctxTexture_DX11, dxtex) then Exit;
  if Field.DataClass = dcCubeSampler then
    resview := dxtex.GetResCubeView
  else
    resview := dxtex.GetResView;
  SetUniform_internal(DXField, resview, Sampler);
end;

procedure TProgram.SetUniform(const Field: TUniformField; const tex: IctxTexture3D; const Sampler: TSamplerInfo);
var DXField: TUniformField_DX absolute Field;
    dxtex: IctxTexture3D_DX11;
begin
  if Field = nil then Exit;
  if not Supports(tex, IctxTexture3D_DX11, dxtex) then Exit;
  SetUniform_internal(DXField, dxtex.GetResView, Sampler);
end;

procedure TProgram.SetUniform(const Field: TUniformField; const buf: IctxStructuredBuffer);
var DXField: TUniformField_DX absolute Field;
    resview: ID3D11ShaderResourceView;
    dxbuf  : IctxStructuredBuffer_DX;
begin
  if Field = nil then Exit;
  if not Supports(buf, IctxStructuredBuffer_DX, dxbuf) then Exit;
  resview := dxbuf.View;

  if DXField.FResourceIndex[stVertex]>=0 then
    FContext.FDeviceContext.VSSetShaderResources(DXField.FResourceIndex[stVertex], 1, @resview);

  if DXField.FResourceIndex[stTessControl]>=0 then
      FContext.FDeviceContext.HSSetShaderResources(DXField.FResourceIndex[stTessControl], 1, @resview);

  if DXField.FResourceIndex[stTessEval]>=0 then
      FContext.FDeviceContext.DSSetShaderResources(DXField.FResourceIndex[stTessEval], 1, @resview);

  if DXField.FResourceIndex[stGeometry]>=0 then
      FContext.FDeviceContext.GSSetShaderResources(DXField.FResourceIndex[stGeometry], 1, @resview);

  if DXField.FResourceIndex[stFragment]>=0 then
    FContext.FDeviceContext.PSSetShaderResources(DXField.FResourceIndex[stFragment], 1, @resview);

  if DXField.FResourceIndex[stCompute]>=0 then
    FContext.FDeviceContext.CSSetShaderResources(DXField.FResourceIndex[stCompute], 1, @resview);
end;

procedure TProgram.SetComputeUAV(const Index: Integer; const uav: IctxUAV; const initial: Integer);
var view: ID3D11UnorderedAccessView;
begin
  Assert(FShader[stCompute] <> nil);

  if Length(FUAVList) < Index+1 then
    SetLength(FUAVList, Index + 1);

  if uav = nil then
  begin
    view := nil;
    FUAVList[Index] := nil;
  end
  else
  begin
    FUAVList[Index] := uav as IctxUAV_DX11;
    view := FUAVList[Index].GetView;
  end;

  FContext.FDeviceContext.CSSetUnorderedAccessViews(Index, 1, @view, @initial);
end;

procedure TProgram.SetComputeTex3D(const Index: Integer; const uav: IctxTexture3D);
var view: ID3D11UnorderedAccessView;
    initial: LongWord;
begin
  initial := 0;
  Assert(FShader[stCompute] <> nil);
  if uav = nil then
    view := nil
  else
    view := (uav as IctxUAV_DX11).GetView;
  FContext.FDeviceContext.CSSetUnorderedAccessViews(Index, 1, @view, @initial);
end;

procedure TProgram.Draw(PrimTopology: TPrimitiveType; CullMode: TCullingMode;
  IndexedGeometry: Boolean; InstanceCount: Integer; Start: integer;
  Count: integer; BaseVertex: integer; BaseInstance: Integer);
var newTopology: TD3D11_PrimitiveTopology;
begin
  SyncCB;
  FContext.States.CullMode := CullMode;

  FContext.UpdateStates;
  newTopology := DXPrimitiveType[PrimTopology];
  if newTopology = D3D11_PRIMITIVE_TOPOLOGY_1_CONTROL_POINT_PATCHLIST then
    newTopology := TD3D11_PrimitiveTopology(Integer(newTopology) + FSelectedPathSize - 1);
  if FContext.FPrimTopology <> newTopology then
  begin
      FContext.FDeviceContext.IASetPrimitiveTopology(newTopology);
      FContext.FPrimTopology := newTopology;
  end;

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

procedure TProgram.DispatchDraw(GroupDims: TVec3i);
begin
  SyncCB;
  FContext.FDeviceContext.Dispatch(GroupDims.x, GroupDims.y, GroupDims.z);
end;

procedure TProgram.ClearComputeUAV(const Index: Integer; const color: TVec4i);
var
  view: ID3D11UnorderedAccessView;
begin
  FContext.FDeviceContext.CSGetUnorderedAccessViews(Index, 1, @view);
  Assert(view <> nil);
  FContext.FDeviceContext.ClearUnorderedAccessViewUint(view, TQuadArray_UInt(color));
end;

procedure TProgram.ResetUAVCounter(const Index: Integer);
begin
  if Length(FUAVList) <= Index then Exit;
  if FUAVList[Index] = nil then Exit;
  FUAVList[Index].InvalidateCounter;
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
  FRDesc.FrontCounterClockwise := True;
  FRDesc.DepthBias := 0;
  FRDesc.DepthBiasClamp := 0;
  FRDesc.SlopeScaledDepthBias := 0;
  FRDesc.DepthClipEnable := True;
  FRDesc.ScissorEnable := False;
  FRDesc.MultisampleEnable := False;
  FRDesc.AntialiasedLineEnable := False;

  FDDescDirty := True;
  // Depth test parameters
  FDDesc.DepthEnable := False;
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
      Check3DError( FContext.FDevice.CreateBlendState(FBDesc, State) );
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
      Check3DError( FContext.FDevice.CreateRasterizerState(FRDesc, State) );
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
      Check3DError( FContext.FDevice.CreateDepthStencilState(FDDesc, State) );
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
  case FBDesc.RenderTarget[max(0, RenderTargetIndex)].SrcBlend of
    D3D11_BLEND_ZERO: Result := bfZero;
    D3D11_BLEND_ONE: Result := bfOne;
    D3D11_BLEND_SRC_COLOR: Result := bfSrcColor;
    D3D11_BLEND_SRC_ALPHA: Result := bfSrcAlpha;
    D3D11_BLEND_INV_SRC_ALPHA: Result := bfInvSrcAlpha;
    D3D11_BLEND_DEST_COLOR: Result := bfDstColor;
  else
    Assert(False, 'not implemented yet');
    Result := bfZero;
  end;
end;

function TStates.GetBlendDest(RenderTargetIndex: Integer): TBlendFunc;
begin
  case FBDesc.RenderTarget[max(0, RenderTargetIndex)].DestBlend of
    D3D11_BLEND_ZERO: Result := bfZero;
    D3D11_BLEND_ONE: Result := bfOne;
    D3D11_BLEND_SRC_COLOR: Result := bfSrcColor;
    D3D11_BLEND_SRC_ALPHA: Result := bfSrcAlpha;
    D3D11_BLEND_INV_SRC_ALPHA: Result := bfInvSrcAlpha;
    D3D11_BLEND_DEST_COLOR: Result := bfDstColor;
  else
    Assert(False, 'not implemented yet');
    Result := bfZero;
  end;
end;

function TStates.GetBlending(RenderTargetIndex: Integer = AllTargets): Boolean;
begin
  Result := FBDesc.RenderTarget[max(0, RenderTargetIndex)].BlendEnable;
end;

function TStates.GetColorMask(RenderTargetIndex: Integer): TColorMask;
var mask: Byte;
begin
  RenderTargetIndex := max(0, RenderTargetIndex);
  mask := FBDesc.RenderTarget[RenderTargetIndex].RenderTargetWriteMask;
  Result := [];
  if mask and Byte(D3D11_COLOR_WRITE_ENABLE_RED)   <> 0 then Result := Result + [cmRed];
  if mask and Byte(D3D11_COLOR_WRITE_ENABLE_GREEN) <> 0 then Result := Result + [cmGreen];
  if mask and Byte(D3D11_COLOR_WRITE_ENABLE_BLUE)  <> 0 then Result := Result + [cmBlue];
  if mask and Byte(D3D11_COLOR_WRITE_ENABLE_ALPHA) <> 0 then Result := Result + [cmAlpha];
end;

function TStates.GetCullMode: TCullingMode;
begin
  case FRDesc.CullMode of
    D3D11_CULL_NONE: Result := cmNone;
    D3D11_CULL_FRONT: Result := cmFront;
    D3D11_CULL_BACK: Result := cmBack;
  else
    Assert(False, 'Not implemented yet');
    Result := cmNone;
  end;
end;

function TStates.GetDepthFunc: TCompareFunc;
begin
  Result := AVDepthFunc[FDDesc.DepthFunc];
end;

function TStates.GetDepthTest: Boolean;
begin
  Result := FDDesc.DepthEnable;
end;

function TStates.GetDepthWrite: Boolean;
begin
  Result := FDDesc.DepthWriteMask = D3D11_DEPTH_WRITE_MASK_ALL;
end;

function TStates.GetNearFarClamp: Boolean;
begin
  Result := not FRDesc.DepthClipEnable;
end;

function TStates.GetViewport: TRectI;
begin
  Result := FViewport;
end;

function TStates.GetScissor: TRectI;
begin
  Result := FScissor;
end;

function TStates.GetScissorTest: Boolean;
begin
  Result := FRDesc.ScissorEnable;
end;

function TStates.GetWireframe: Boolean;
begin
  case FRDesc.FillMode of
    D3D11_FILL_WIREFRAME: Result := True;
    D3D11_FILL_SOLID    : Result := False;
  else
    Assert(False, 'WTF??');
    Result := False;
  end;
end;

procedure TStates.SetCullMode(const Value: TCullingMode);
begin
  FRDescDirty := FRDescDirty or (FRDesc.CullMode <> DXCullMode[Value]);
  FRDesc.CullMode := DXCullMode[Value];
end;

procedure TStates.SetColorMask(RenderTargetIndex: Integer; const Value: TColorMask);
var i: Integer;
    dxMask: Byte;
begin
  dxMask := 0;
  if cmRed   in Value then dxMask := dxMask or Byte(D3D11_COLOR_WRITE_ENABLE_RED);
  if cmGreen in Value then dxMask := dxMask or Byte(D3D11_COLOR_WRITE_ENABLE_GREEN);
  if cmBlue  in Value then dxMask := dxMask or Byte(D3D11_COLOR_WRITE_ENABLE_BLUE);
  if cmAlpha in Value then dxMask := dxMask or Byte(D3D11_COLOR_WRITE_ENABLE_ALPHA);

  if RenderTargetIndex = AllTargets then
  begin
    for i := 0 to Length(FBDesc.RenderTarget) - 1 do
    begin
      FBDescDirty := FBDescDirty or (FBDesc.RenderTarget[i].RenderTargetWriteMask <> dxMask);
      FBDesc.RenderTarget[i].RenderTargetWriteMask := dxMask;
    end;
  end
  else
  begin
    FBDescDirty := FBDescDirty or (FBDesc.RenderTarget[RenderTargetIndex].RenderTargetWriteMask <> dxMask);
    FBDesc.RenderTarget[RenderTargetIndex].RenderTargetWriteMask := dxMask;
  end;
end;

procedure TStates.SetDepthTest(const Value: Boolean);
begin
  FDDescDirty := FDDescDirty or (FDDesc.DepthEnable <> Value);
  FDDesc.DepthEnable := Value;
end;

procedure TStates.SetDepthWrite(const Value: Boolean);
begin
  FDDescDirty := FDDescDirty or (FDDesc.DepthWriteMask <> DXDepthMask[Value]);
  FDDesc.DepthWriteMask := DXDepthMask[Value];
end;

procedure TStates.SetDepthFunc(const Value: TCompareFunc);
begin
  FDDescDirty := FDDescDirty or (FDDesc.DepthFunc <> DXCompareFunc[Value]);
  FDDesc.DepthFunc := DXCompareFunc[Value];
end;

procedure TStates.SetNearFarClamp(const Value: Boolean);
begin
  FRDescDirty := FRDescDirty or (FRDesc.DepthClipEnable=Value);
  FRDesc.DepthClipEnable := not Value;
end;

procedure TStates.SetBlending(RenderTargetIndex: Integer; const Value : Boolean);
var i: Integer;
begin
  if RenderTargetIndex = AllTargets then
  begin
    for i := 0 to Length(FBDesc.RenderTarget) - 1 do
    begin
      FBDescDirty := FBDescDirty or (FBDesc.RenderTarget[i].BlendEnable <> Value);
      FBDesc.RenderTarget[i].BlendEnable := Value;
    end;
  end
  else
  begin
    FBDescDirty := FBDescDirty or (FBDesc.RenderTarget[RenderTargetIndex].BlendEnable <> Value);
    FBDesc.RenderTarget[RenderTargetIndex].BlendEnable := Value;
  end;
end;

procedure TStates.SetBlendOperation(BlendOp: TBlendOp; RenderTargetIndex: Integer);
begin
  SetBlendOperation_SeparateAlpha(BlendOp, BlendOp, RenderTargetIndex);
end;

procedure TStates.SetBlendOperation_SeparateAlpha(BlendOp, AlphaBlendOp: TBlendOp; RenderTargetIndex: Integer);
var i: Integer;
begin
  if RenderTargetIndex = AllTargets then
  begin
    for i := 0 to Length(FBDesc.RenderTarget) - 1 do
    begin
      FBDescDirty := FBDescDirty or
                     (FBDesc.RenderTarget[i].BlendOp <> DXBlendOp[BlendOp]) or
                     (FBDesc.RenderTarget[i].BlendOpAlpha <> DXBlendOp[AlphaBlendOp]);
      FBDesc.RenderTarget[i].BlendOp := DXBlendOp[BlendOp];
      FBDesc.RenderTarget[i].BlendOpAlpha := DXBlendOp[AlphaBlendOp];
    end;
  end
  else
  begin
      FBDescDirty := FBDescDirty or
                     (FBDesc.RenderTarget[RenderTargetIndex].BlendOp <> DXBlendOp[BlendOp]) or
                     (FBDesc.RenderTarget[RenderTargetIndex].BlendOpAlpha <> DXBlendOp[AlphaBlendOp]);
      FBDesc.RenderTarget[RenderTargetIndex].BlendOp := DXBlendOp[BlendOp];
      FBDesc.RenderTarget[RenderTargetIndex].BlendOpAlpha := DXBlendOp[AlphaBlendOp];
  end;
end;

procedure TStates.SetViewport(const Value: TRectI);
begin
  if (FViewport.Left <> Value.Left) or
     (FViewport.Top <> Value.Top) or
     (FViewport.Right <> Value.Right) or
     (FViewport.Bottom <> Value.Bottom) then
  begin
    FViewport := Value;
    SetupViewPort;
  end;
end;

procedure TStates.SetScissor(const Value: TRectI);
var sr: TD3D11_Rect;
begin
  if (FScissor.Left <> Value.Left) or
     (FScissor.Top <> Value.Top) or
     (FScissor.Right <> Value.Right) or
     (FScissor.Bottom <> Value.Bottom) then
  begin
    FScissor := Value;
    sr.Left := Value.Left;
    sr.Top := Value.Top;
    sr.Right := Value.Right;
    sr.Bottom := Value.Bottom;
    FContext.FDeviceContext.RSSetScissorRects(1, @sr);
  end;
end;

procedure TStates.SetScissorTest(const Value: Boolean);
begin
  FRDescDirty := FRDescDirty or (FRDesc.ScissorEnable <> Value);
  FRDesc.ScissorEnable := Value;
end;

procedure TStates.SetWireframe(const Value: Boolean);
const D3DWire: array [Boolean] of TD3D11_FillMode = (D3D11_FILL_SOLID, D3D11_FILL_WIREFRAME);
begin
  FRDescDirty := FRDescDirty or (FRDesc.FillMode <> D3DWire[Value]);
  FRDesc.FillMode:= D3DWire[Value];
end;

procedure TStates.SetStencil(Enabled: Boolean; StencilFunc: TCompareFunc;
  Ref: Integer; Mask: Byte; sFail, dFail, dPass: TStencilAction);
begin
  FDDesc.StencilEnable := Enabled;
  FDDesc.StencilReadMask := Mask;
  FDDesc.StencilWriteMask := Mask;
  FDDesc.FrontFace.StencilFailOp := DXStencilAction[sFail];
  FDDesc.FrontFace.StencilDepthFailOp := DXStencilAction[dFail];
  FDDesc.FrontFace.StencilPassOp := DXStencilAction[dPass];
  FDDesc.FrontFace.StencilFunc := DXCompareFunc[StencilFunc];
  FDDesc.BackFace.StencilFailOp := DXStencilAction[sFail];
  FDDesc.BackFace.StencilDepthFailOp := DXStencilAction[dFail];
  FDDesc.BackFace.StencilPassOp := DXStencilAction[dPass];
  FDDesc.BackFace.StencilFunc := DXCompareFunc[StencilFunc];
  FDStencilRef := Ref;
  FDDescDirty := True;
end;

procedure TStates.SetupViewPort();
var vp: TD3D11_Viewport;
begin
  vp.TopLeftX := FViewport.Left;
  vp.TopLeftY := FViewport.Top;
  vp.Width := FViewport.Right - FViewport.Left;
  vp.Height := FViewport.Bottom - FViewport.Top;
  vp.MinDepth := 0;
  vp.MaxDepth := 1;
  FContext.FDeviceContext.RSSetViewports(1, @vp);
end;

procedure TStates.SetBlendFunctions(Src, Dest: TBlendFunc; RenderTargetIndex: Integer);
begin
  SetBlendFunctions_SeparateAlpha(Src, Dest, Src, Dest, RenderTargetIndex);
end;

procedure TStates.SetBlendFunctions_SeparateAlpha(Src, Dest, AlphaSrc, AlphaDest: TBlendFunc; RenderTargetIndex: Integer);
var i: Integer;
begin
  if RenderTargetIndex = AllTargets then
  begin
    for i := 0 to Length(FBDesc.RenderTarget) - 1 do
    begin
      FBDescDirty := FBDescDirty or
                     (FBDesc.RenderTarget[i].SrcBlend <> DXBlend[Src]) or
                     (FBDesc.RenderTarget[i].DestBlend <> DXBlend[Dest]) or
                     (FBDesc.RenderTarget[i].SrcBlendAlpha <> DXBlend[AlphaSrc]) or
                     (FBDesc.RenderTarget[i].DestBlendAlpha <> DXBlend[AlphaDest]);
      FBDesc.RenderTarget[i].SrcBlend := DXBlend[Src];
      FBDesc.RenderTarget[i].DestBlend := DXBlend[Dest];

      case AlphaSrc of
        bfDstColor : FBDesc.RenderTarget[i].SrcBlendAlpha := DXBlend[bfDstAlpha];
        bfSrcColor : FBDesc.RenderTarget[i].SrcBlendAlpha := DXBlend[bfSrcAlpha];
      else
        FBDesc.RenderTarget[i].SrcBlendAlpha := DXBlend[AlphaSrc];
      end;

      case AlphaDest of
        bfDstColor : FBDesc.RenderTarget[i].DestBlendAlpha := DXBlend[bfDstAlpha];
        bfSrcColor : FBDesc.RenderTarget[i].DestBlendAlpha := DXBlend[bfSrcAlpha];
      else
        FBDesc.RenderTarget[i].DestBlendAlpha := DXBlend[AlphaDest];
      end;
    end;
  end
  else
  begin
    FBDescDirty := FBDescDirty or
                   (FBDesc.RenderTarget[RenderTargetIndex].SrcBlend <> DXBlend[Src]) or
                   (FBDesc.RenderTarget[RenderTargetIndex].DestBlend <> DXBlend[Dest]) or
                   (FBDesc.RenderTarget[RenderTargetIndex].SrcBlendAlpha <> DXBlend[AlphaSrc]) or
                   (FBDesc.RenderTarget[RenderTargetIndex].DestBlendAlpha <> DXBlend[AlphaDest]);
    FBDesc.RenderTarget[RenderTargetIndex].SrcBlend := DXBlend[Src];
    FBDesc.RenderTarget[RenderTargetIndex].DestBlend := DXBlend[Dest];

    case AlphaSrc of
      bfDstColor : FBDesc.RenderTarget[RenderTargetIndex].SrcBlendAlpha := DXBlend[bfDstAlpha];
      bfSrcColor : FBDesc.RenderTarget[RenderTargetIndex].SrcBlendAlpha := DXBlend[bfSrcAlpha];
    else
      FBDesc.RenderTarget[RenderTargetIndex].SrcBlendAlpha := DXBlend[AlphaSrc];
    end;

    case AlphaDest of
      bfDstColor : FBDesc.RenderTarget[RenderTargetIndex].DestBlendAlpha := DXBlend[bfDstAlpha];
      bfSrcColor : FBDesc.RenderTarget[RenderTargetIndex].DestBlendAlpha := DXBlend[bfSrcAlpha];
    else
      FBDesc.RenderTarget[RenderTargetIndex].DestBlendAlpha := DXBlend[AlphaDest];
    end;
  end;
end;

constructor TStates.Create(AContext: TContext_DX11);
begin
  FContext := AContext;

  FillChar(FBDesc, SizeOf(FBDesc), 0);
  FillChar(FRDesc, SizeOf(FRDesc), 0);
  FillChar(FDDesc, SizeOf(FDDesc), 0);

  FBlendStates  := TBlendStateMap.Create();
  FRasterStates := TRasterStateMap.Create();
  FDepthStates  := TDepthStateMap.Create();

  LoadDefaultState;
end;

{ TColorSpaceConverter }

class function TColorSpaceConverter.Convert_A8R8G8B8_RGBA(ASrc: PByte;
  ASrcSize: Integer; ASrcFormat: TImageFormat; ADstFormat: TTextureFormat; out
  ADst: PByte; out ADstSize: Integer): Boolean;
var pSrc, pDst: PVec4b;
    PixelCount: Integer;
    i: Integer;
begin
  Result := True;
  PixelCount := ASrcSize div 4;
  ADstSize := ASrcSize;
  GetMem(ADst, ADstSize);
  ZeroMemory(ADst, ADstSize);

  pSrc := PVec4b(ASrc);
  pDst := PVec4b(ADst);
  for i := 0 to PixelCount - 1 do
  begin
    pDst^.x := pSrc^.z;
    pDst^.y := pSrc^.y;
    pDst^.z := pSrc^.x;
    pDst^.w := pSrc^.w;
    Inc(pDst);
    Inc(pSrc);
  end;
end;

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
      case ch of
        'r', 'R' : Result := 0;
        'g', 'G' : Result := 1;
        'b', 'B' : Result := 2;
        'a', 'A' : Result := 3;
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
      i, n: Integer;
  begin
      Name := GetEnumName(TypeInfo(TImageFormat), Ord(Format));

      ZeroClear(Result, SizeOf(Result));
      Result.RGBA[0].CompType := ctBool;
      Result.RGBA[1].CompType := ctBool;
      Result.RGBA[2].CompType := ctBool;
      Result.RGBA[3].CompType := ctBool;

      n := Min(32, ImagePixelSize[Format] * 8);

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

        Result.RGBA[CompIndex].DWordOffset := TotalOffset div n;
        Result.RGBA[CompIndex].BitsOffset := n - (TotalOffset mod n) - BitsCount;
        Result.RGBA[CompIndex].BitsCount := BitsCount;
        Result.RGBA[CompIndex].CompType := ctInt;
        Inc(TotalOffset, BitsCount);
      end;
      Result.StrideSize := (TotalOffset + 7) div 8;

      if LowerCase(Name[Length(Name)]) = 'f' then
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

      ZeroClear(Result, SizeOf(Result));
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

      if LowerCase(Name[Length(Name)]) = 'f' then
        for i := 0 to 3 do
          if Result.RGBA[i].CompType = ctInt then Result.RGBA[i].CompType := ctFloat;

      BitsCount := DecodeBitsCount(Name, CharIndex);
      if BitsCount = 0 then BitsCount := 8;
      for i := 0 to 3 do
      begin
        Result.RGBA[i].DWordOffset := (BitsCount * i) div 32;
        if (BitsCount = 8) or (Result.RGBA[i].CompType = ctFloat) then //todo check this condition for other bitcount images
          Result.RGBA[i].BitsOffset := i * BitsCount
        else
        if TexturePixelSize[Format] < 4 then
          Result.RGBA[i].BitsOffset := i * BitsCount
        else
          Result.RGBA[i].BitsOffset := 32 - ((BitsCount * i) mod 32) - BitsCount;
        Result.RGBA[i].BitsCount := BitsCount;
      end;

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

  function IsEqualsFormat(const imgF: TImageFormat; const texF: TTextureFormat): Boolean;
  begin
    case imgF of
      TImageFormat.Gray8: Result := texF = TTextureFormat.R;
      TImageFormat.R8G8: Result := texF = TTextureFormat.RG;
      TImageFormat.R8G8B8: Result := texF = TTextureFormat.RGB;
      TImageFormat.A8B8G8R8: Result := texF = TTextureFormat.RGBA;
      TImageFormat.R16F: Result := texF = TTextureFormat.R16f;
      TImageFormat.R16G16F: Result := texF = TTextureFormat.RG16f;
      TImageFormat.B16G16R16F: Result := texF = TTextureFormat.RGB16f;
      TImageFormat.A16B16G16R16F: Result := texF = TTextureFormat.RGBA16f;
      TImageFormat.R32F: Result := texF = TTextureFormat.R32f;
      TImageFormat.R32G32F: Result := texF = TTextureFormat.RG32f;
      TImageFormat.R32G32B32F: Result := texF = TTextureFormat.RGB32f;
      TImageFormat.R32G32B32A32F: Result := texF = TTextureFormat.RGBA32f;
      TImageFormat.DXT1: Result := texF = TTextureFormat.DXT1;
      TImageFormat.DXT3: Result := texF = TTextureFormat.DXT3;
      TImageFormat.DXT5: Result := texF = TTextureFormat.DXT5;
      TImageFormat.R16: Result := texF = TTextureFormat.R16;
      TImageFormat.R16G16: Result := texF = TTextureFormat.RG;
      TImageFormat.R16G16B16: Result := texF = TTextureFormat.RGB;
      TImageFormat.A16B16G16R16: Result := texF = TTextureFormat.RGBA16;
      TImageFormat.R32: Result := texF = TTextureFormat.R32;
      TImageFormat.R32G32: Result := texF = TTextureFormat.RG32;
      TImageFormat.R32G32B32: Result := texF = TTextureFormat.RGB32;
      TImageFormat.A32B32G32R32: Result := texF = TTextureFormat.RGBA32;
    else
      Result := False;
    end;

  end;

var SpecMethod: TConvertMethod;
    ImgFormat: TImageFormatDesc;
    TexFormat: TImageFormatDesc;
    PixelCount: Integer;
    SrcPixel, DstPixel: PByte;
    i, j: Integer;
begin
  SpecMethod := FSpecConvertes[ASrcFormat][ADstFormat];
  if IsEqualsFormat(ASrcFormat, ADstFormat) then
  begin
    ADst := ASrc;
    ADstSize := ASrcSize;
    Exit(False);
  end;

  if Assigned(SpecMethod) then Exit(SpecMethod(ASrc, ASrcSize, ASrcFormat, ADstFormat, ADst, ADstSize));

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

class constructor TColorSpaceConverter.Create;
var i: TImageFormat;
    j: TTextureFormat;
begin
  for i := Low(TImageFormat) to High(TImageFormat) do
    for j := Low(TTextureFormat) to High(TTextureFormat) do
      FSpecConvertes[i][j] := nil;
  FSpecConvertes[TImageFormat.A8R8G8B8][TTextureFormat.RGBA] := {$IfDef FPC}@{$EndIf}Convert_A8R8G8B8_RGBA;
end;

{ TTexture }

function TTexture.BuildDesc(AWidth, AHeight, ADeep: Integer; WithMips: Boolean): TD3D11_Texture2DDesc;
begin
  Result.Width  := AWidth;
  Result.Height := AHeight;
  if WithMips then
    FMipsCount := GetMipsCount(Result.Width, Result.Height)
  else
    FMipsCount := 1;
  Result.MipLevels := FMipsCount;

  case FTargetFormat of
    TTextureFormat.D24_S8,
    TTextureFormat.D32f_S8,
    TTextureFormat.D16,
    TTextureFormat.D24,
    TTextureFormat.D32,
    TTextureFormat.D32f:
      Result.BindFlags := DWord(D3D11_BIND_SHADER_RESOURCE) or DWord(D3D11_BIND_DEPTH_STENCIL);
  else
    Result.BindFlags := DWord(D3D11_BIND_SHADER_RESOURCE);
    if not (FTargetFormat in [TTextureFormat.DXT1, TTextureFormat.DXT3, TTextureFormat.DXT5]) then
      Result.BindFlags := Result.BindFlags or DWord(D3D11_BIND_RENDER_TARGET);
  end;

  case FTargetFormat of
    TTextureFormat.R32f,
    TTextureFormat.R32:
      Result.BindFlags := Result.BindFlags or DWord(D3D11_BIND_UNORDERED_ACCESS);
  end;

  Result.ArraySize := ADeep;
  Result.Format := D3D11TextureFormat[FTargetFormat];
  if FsRGB then Result.Format := Add_sRGB(Result.Format);
  Result.SampleDesc.Count := 1;
  Result.SampleDesc.Quality := 0;
  Result.Usage := D3D11_USAGE_DEFAULT;
  Result.CPUAccessFlags := 0;
  Result.MiscFlags := 0;
  if WithMips and (Result.BindFlags and DWord(D3D11_BIND_RENDER_TARGET) = DWord(D3D11_BIND_RENDER_TARGET))  then
    Result.MiscFlags := Result.MiscFlags or DWord(D3D11_RESOURCE_MISC_GENERATE_MIPS);
  if (ADeep mod 6 = 0) and (AWidth = AHeight) then
    Result.MiscFlags := Result.MiscFlags or DWord(D3D11_RESOURCE_MISC_TEXTURECUBE);
  FFormat := FTargetFormat;
end;

function TTexture.GetHandle: ID3D11Texture2D;
begin
  Result := FTexture;
end;

function TTexture.GetResCubeView: ID3D11ShaderResourceView;
var desc: TD3D11_ShaderResourceViewDesc;
begin
  if FCubeResView = nil then
  begin
    if FDeep < 6 then Exit(nil);
    desc.Format := D3D11ShaderViewFormat[FFormat];
    if FsRGB then desc.Format := Add_sRGB(desc.Format);
    if (FDeep >= 12) or FForcedArray then
    begin
      desc.ViewDimension := D3D11_SRV_DIMENSION_TEXTURECUBEARRAY;
      desc.TextureCubeArray.NumCubes := FDeep div 6;
      desc.TextureCubeArray.First2DArrayFace := 0;
      desc.TextureCubeArray.MostDetailedMip := 0;
      desc.TextureCubeArray.MipLevels := FMipsCount;
    end
    else
    begin
      desc.ViewDimension := D3D11_SRV_DIMENSION_TEXTURECUBE;
      desc.TextureCube.MostDetailedMip := 0;
      desc.TextureCube.MipLevels := FMipsCount;
    end;
    Check3DError(FContext.FDevice.CreateShaderResourceView(FTexture, @desc, FCubeResView));
  end;
  Result := FCubeResView;
end;

function TTexture.GetResView: ID3D11ShaderResourceView;
var desc: TD3D11_ShaderResourceViewDesc;
begin
  if FResView = nil then
  begin
    desc.Format := D3D11ShaderViewFormat[FFormat];
    if FsRGB then desc.Format := Add_sRGB(desc.Format);
    if (FDeep > 1) or FForcedArray then
    begin
      desc.ViewDimension := D3D10_SRV_DIMENSION_TEXTURE2DARRAY;
      desc.Texture2DArray.ArraySize := FDeep;
      desc.Texture2DArray.FirstArraySlice := 0;
      desc.Texture2DArray.MostDetailedMip := 0;
      desc.Texture2DArray.MipLevels := FMipsCount;
    end
    else
    begin
      desc.ViewDimension := D3D10_SRV_DIMENSION_TEXTURE2D;
      desc.Texture2D.MostDetailedMip := 0;
      desc.Texture2D.MipLevels := FMipsCount;
    end;
    Check3DError(FContext.FDevice.CreateShaderResourceView(FTexture, @desc, FResView));
  end;
  Result := FResView;
end;

function TTexture.GetTargetFormat: TTextureFormat;
begin
  Result := FTargetFormat;
end;

function TTexture.GetView: ID3D11UnorderedAccessView;
var
  uavdesc: TD3D11_UnorderedAccessViewDesc;
begin
  if FUAVView = nil then
  begin
    uavdesc.Format := D3D11ShaderViewFormat[FFormat];

    if (FDeep > 1) or FForcedArray then
    begin
      uavdesc.ViewDimension := D3D11_UAV_DIMENSION_TEXTURE2DARRAY;
      uavdesc.Texture2DArray.ArraySize := FDeep;
      uavdesc.Texture2DArray.MipSlice := 0;
    end
    else
    begin
      uavdesc.ViewDimension := D3D11_UAV_DIMENSION_TEXTURE2D;
      uavdesc.Texture2D.MipSlice := 0;
    end;

    Check3DError(FContext.FDevice.CreateUnorderedAccessView(FTexture, @uavdesc, FUAVView));
  end;
  Result := FUAVView;
end;

function TTexture.Get_sRGB: Boolean;
begin
  Result := FsRGB;
end;

procedure TTexture.SetTargetFormat(Value: TTextureFormat);
begin
  FTargetFormat := Value;
end;

procedure TTexture.Set_sRGB(Value: Boolean);
begin
  FsRGB := Value;
end;

function TTexture.Width: Integer;
begin
  Result := FWidth;
end;

function TTexture.Height: Integer;
begin
  Result := FHeight;
end;

procedure TTexture.InvalidateCounter;
begin

end;

function TTexture.Deep: Integer;
begin
  Result := FDeep;
end;

function TTexture.MipsCount: Integer;
begin
  Result := FMipsCount;
end;

function TTexture.SampleCount: Integer;
begin
  Result := FSampleCount;
end;

function TTexture.Format: TTextureFormat;
begin
  Result := FFormat;
end;

procedure TTexture.AllocMultiSampled(AWidth, AHeight, ASampleCount: Integer);
var desc: TD3D11_Texture2DDesc;
    Quality: LongWord;
begin
  desc.Width  := AWidth;
  desc.Height := AHeight;
  desc.MipLevels := 1;

  case FTargetFormat of
    TTextureFormat.D24_S8,
    TTextureFormat.D32f_S8,
    TTextureFormat.D16,
    TTextureFormat.D24,
    TTextureFormat.D32,
    TTextureFormat.D32f:
      desc.BindFlags := DWord(D3D11_BIND_DEPTH_STENCIL);
  else
    desc.BindFlags := DWord(D3D11_BIND_RENDER_TARGET);
  end;

  desc.Format := D3D11TextureFormat[FTargetFormat];
  if FsRGB then desc.Format := Add_sRGB(desc.Format);

  Check3DError(FContext.FDevice.CheckMultisampleQualityLevels(desc.Format, ASampleCount, Quality));
  Dec(Quality);

  desc.ArraySize := 1;
  desc.SampleDesc.Count := ASampleCount;
  desc.SampleDesc.Quality := Quality;
  desc.Usage := D3D11_USAGE_DEFAULT;
  desc.CPUAccessFlags := 0;
  desc.MiscFlags := 0;

  FResView := nil;
  FCubeResView := nil;
  Check3DError(FContext.FDevice.CreateTexture2D(desc, nil, FTexture));
  FSampleCount := ASampleCount;
  FWidth := AWidth;
  FHeight := AHeight;
  FDeep := 1;
  FForcedArray := False;
  FFormat := FTargetFormat;
end;

procedure TTexture.AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean; ForcedArray: Boolean);
var desc: TD3D11_Texture2DDesc;
begin
  FResView := nil;
  FCubeResView := nil;
  FUAVView := nil;
  desc := BuildDesc(AWidth, AHeight, ADeep, WithMips);
  Check3DError(FContext.FDevice.CreateTexture2D(desc, nil, FTexture));
  FSampleCount := 1;
  FWidth := desc.Width;
  FHeight := desc.Height;
  FDeep := ADeep;
  FForcedArray := ForcedArray;
end;

procedure TTexture.AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean;
  DataFormat: TImageFormat; Data: PByte; ForcedArray: Boolean);
var desc: TD3D11_Texture2DDesc;
begin
  FResView := nil;
  FCubeResView := nil;
  FUAVView := nil;
  desc := BuildDesc(AWidth, AHeight, ADeep, WithMips);
  //todo: initialization with data
  Check3DError(FContext.FDevice.CreateTexture2D(desc, nil, FTexture));
  FSampleCount := 1;
  FWidth := desc.Width;
  FHeight := desc.Height;
  FDeep := ADeep;
  FForcedArray := ForcedArray;
end;

procedure TTexture.SetMipImage(X, Y, ImageWidth, ImageHeight, MipLevel, ZSlice: Integer; DataFormat: TImageFormat; Data: PByte);
var v: TVec4i;
begin
  v := Vec(X, Y, X + ImageWidth, Y + ImageHeight);
  SetMipImage(Rect(v), MipLevel, ZSlice, DataFormat, Data);
end;

procedure TTexture.SetMipImage(DestRect: TRect; MipLevel, ZSlice: Integer; DataFormat: TImageFormat; Data: PByte);
var tex  : PByte;
    imgWidth, imgHeight: Integer;
    texSize: Integer;
    texShouldFree: Boolean;
    Box: TD3D11_Box;
    rowPitch, depthPitch: Integer;
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
      case DataFormat of
        TImageFormat.DXT1:
        begin
          rowPitch := ((imgWidth + 3) div 4) * 8;
          depthPitch := 0; //to do fix it
        end;
        TImageFormat.DXT3, TImageFormat.DXT5:
        begin
          rowPitch := ((imgWidth + 3) div 4) * 16;
          depthPitch := 0; //to do fix it
        end;
      else
        rowPitch := imgWidth * TexturePixelSize[TargetFormat];
        depthPitch := rowPitch * imgHeight;
      end;
      FContext.FDeviceContext.UpdateSubresource(FTexture, D3D11CalcSubresource(MipLevel, ZSlice, FMipsCount), @Box, tex, rowPitch, depthPitch);
    finally
      if texShouldFree then FreeMem(tex);
    end;
end;

procedure TTexture.CopyFrom(const DstMipLevel: Integer; const DstPos: TVec2I; const ASrcRes: IctxTexture;
  const SrcMipLevel: Integer; const SrcRect: TRectI);
var box: TD3D11_Box;
    wholeCopy: Boolean;
begin
  case IctxTexture_DX11(ASrcRes).Format of
    TTextureFormat.D24_S8,
    TTextureFormat.D32f_S8,
    TTextureFormat.D16,
    TTextureFormat.D24,
    TTextureFormat.D32,
    TTextureFormat.D32f: wholeCopy := True;
  else
    wholeCopy := False;
  end;

  if wholeCopy then
  begin
    FContext.FDeviceContext.CopySubresourceRegion(
        FTexture, D3D11CalcSubresource(DstMipLevel, 0, 1), 0, 0, 0,
        IctxTexture_DX11(ASrcRes).GetHandle, D3D11CalcSubresource(SrcMipLevel, 0, 1), nil);
  end
  else
  begin
    box.Left := SrcRect.Left;
    box.Top := SrcRect.Top;
    box.Right := SrcRect.Right;
    box.Bottom := SrcRect.Bottom;
    box.Front := 0;
    box.Back := 1;
    FContext.FDeviceContext.CopySubresourceRegion(
        FTexture, D3D11CalcSubresource(DstMipLevel, 0, 1), DstPos.x, DstPos.y, 0,
        IctxTexture_DX11(ASrcRes).GetHandle, D3D11CalcSubresource(SrcMipLevel, 0, 1), @box);
  end;
end;

procedure TTexture.GenerateMips;
begin
  FContext.FDeviceContext.GenerateMips(GetResView);
end;

procedure TTexture.ReadBack(const ATexData: ITextureData; const ASlice: Integer; const AMipLevel: Integer);
var texdesc: TD3D11_Texture2DDesc;
    copybox: TD3D11_Box;
    mapdata: TD3D11_MappedSubresource;
    cputex : ID3D11Texture2D;
    mip: ITextureMip;
    pSrc, pDst: PByte;
    j, w: Integer;
begin
  mip := ATexData.MipData(0, AMipLevel);

  texdesc.Width := mip.Width;
  texdesc.Height := mip.Height;
  texdesc.MipLevels := 1;
  texdesc.ArraySize := 1;
  texdesc.Format := D3D11TextureFormat[FFormat];
  if FsRGB then texdesc.Format := Add_sRGB(texdesc.Format);
  texdesc.SampleDesc.Count := 1;
  texdesc.SampleDesc.Quality := 0;
  texdesc.Usage := D3D11_USAGE_STAGING;
  texdesc.BindFlags := 0;
  texdesc.CPUAccessFlags := Cardinal(D3D11_CPU_ACCESS_READ);
  texdesc.MiscFlags := 0;
  Check3DError( FContext.GetDevice.CreateTexture2D(texdesc, nil, cputex) );

  copybox.Left := 0;
  copybox.Right := ATexData.Width;
  copybox.Top := 0;
  copybox.Bottom := ATexData.Height;
  copybox.Front := 0;
  copybox.Back := 1;
  FContext.GetDeviceContext.CopySubresourceRegion(cputex, D3D11CalcSubresource(AMipLevel, ASlice, 1), 0, 0, 0,
                                                  FTexture, D3D11CalcSubresource(0, 0, 1), @copybox);

  Check3DError( FContext.GetDeviceContext.Map(cputex, D3D11CalcSubresource(0, 0, 1), D3D11_MAP_READ, 0, mapdata) );
  w := mip.Width * ImagePixelSize[ATexData.Format];
  for j := 0 to mip.Height - 1 do
  begin
    pSrc := mapdata.pData;
    Inc(pSrc, mapdata.RowPitch * j);
    pDst := ATexData.MipData(0, AMipLevel).Data;
    Inc(pDst, j * w);
    Move(pSrc^, pDst^, w);
  end;
  FContext.GetDeviceContext.Unmap(cputex, D3D11CalcSubresource(0, 0, 1));
end;

{ TFrameBuffer }

function TFrameBuffer.GetObj: TFrameBuffer;
begin
  Result := Self;
end;

procedure TFrameBuffer.ResetUAVCounters;
var i: Integer;
begin
  for i := 0 to Length(FUAV) - 1 do FUAV[i].UAV.InvalidateCounter;
  FContext.SetFrameBuffer(Self);
end;

procedure TFrameBuffer.Select;
var i: Integer;
    RTDesc: TD3D11_RenderTargetViewDesc;
    DSDesc: TD3D11_DepthStencilViewDesc;
begin
  if (not FValid) and ((Length(FTex) > 0) or (FDepthTex <> nil)) then
  begin
    for i := 0 to Length(FTex) - 1 do
    begin
      if FTex[i].Tex2D <> nil then
      begin
        ZeroClear(RTDesc, SizeOf(RTDesc));
        RTDesc.Format := D3D11ViewFormat[FTex[i].Tex2D.Format];
        if FTex[i].Tex2D.sRGB then RTDesc.Format := Add_sRGB(RTDesc.Format);
        if FTex[i].Tex2D.SampleCount > 1 then
        begin
          if FTex[i].SliceStart < 0 then
          begin
            RTDesc.ViewDimension := D3D11_RTV_DIMENSION_TEXTURE2DMS;
          end
          else
          begin
            RTDesc.ViewDimension := D3D11_RTV_DIMENSION_TEXTURE2DMSARRAY;
            RTDesc.Texture2DMSArray.ArraySize := FTex[i].SliceCount;
            RTDesc.Texture2DMSArray.FirstArraySlice := FTex[i].SliceStart;
          end;
        end
        else
        begin
          if FTex[i].SliceStart < 0 then
          begin
            RTDesc.ViewDimension := D3D11_RTV_DIMENSION_TEXTURE2D;
            RTDesc.Texture2D.MipSlice := FTex[i].Mip;
          end
          else
          begin
            RTDesc.ViewDimension := D3D11_RTV_DIMENSION_TEXTURE2DARRAY;
            RTDesc.Texture2DArray.MipSlice := FTex[i].Mip;
            RTDesc.Texture2DArray.ArraySize := FTex[i].SliceCount;
            RTDesc.Texture2DArray.FirstArraySlice := FTex[i].SliceStart;
          end;
        end;

        Check3DError(FContext.FDevice.CreateRenderTargetView((FTex[i].Tex2D as IctxTexture_DX11).GetHandle, @RTDesc, FViews[i]));
        Continue;
      end;

      if FTex[i].Tex3D <> nil then
      begin
        ZeroClear(RTDesc, SizeOf(RTDesc));
        RTDesc.Format := D3D11ViewFormat[FTex[i].Tex3D.Format];
        if FTex[i].Tex3D.sRGB then RTDesc.Format := Add_sRGB(RTDesc.Format);

        RTDesc.ViewDimension := D3D11_RTV_DIMENSION_TEXTURE3D;
        RTDesc.Texture3D.MipSlice := FTex[i].Mip;
        RTDesc.Texture3D.FirstWSlice := FTex[i].SliceStart;
        RTDesc.Texture3D.WSize := FTex[i].SliceCount;

        Check3DError(FContext.FDevice.CreateRenderTargetView((FTex[i].Tex3D as IctxTexture3D_DX11).GetHandle, @RTDesc, FViews[i]));
        Continue;
      end;

      FViews[i] := nil;
    end;

    if Assigned(FDepthTex) then
    begin
      ZeroClear(DSDesc, SizeOf(DSDesc));
      DSDesc.Format := D3D11ViewFormat[FDepthTex.Format];
      if FDepthTex.SampleCount > 1 then
      begin
        if FDepthSliceStart < 0 then
        begin
          DSDesc.ViewDimension := D3D11_DSV_DIMENSION_TEXTURE2DMS;
        end
        else
        begin
          DSDesc.ViewDimension := D3D11_DSV_DIMENSION_TEXTURE2DMSARRAY;
          DSDesc.Texture2DMSArray.ArraySize := FDepthSliceCount;
          DSDesc.Texture2DMSArray.FirstArraySlice := FDepthSliceStart;
        end;
      end
      else
      begin
        if FDepthSliceStart < 0 then
        begin
          DSDesc.ViewDimension := D3D11_DSV_DIMENSION_TEXTURE2D;
          DSDesc.Texture2D.MipSlice := FDepthMip;
        end
        else
        begin
          DSDesc.ViewDimension := D3D11_DSV_DIMENSION_TEXTURE2DARRAY;
          DSDesc.Texture2DArray.MipSlice := FDepthMip;
          DSDesc.Texture2DArray.ArraySize := FDepthSliceCount;
          DSDesc.Texture2DArray.FirstArraySlice := FDepthSliceStart;
        end;
      end;

      Check3DError(FContext.FDevice.CreateDepthStencilView((FDepthTex as IctxTexture_DX11).GetHandle, @DSDesc, FDepthView));
    end
    else
      FDepthView := nil;

    FValid := True;
  end;
  for i := 0 to Length(FUAV) - 1 do FUAV[i].UAV.InvalidateCounter;
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

procedure TFrameBuffer.SetColor(index: Integer; tex: IctxTexture; mipLevel: Integer; sliceStart: Integer; sliceCount: Integer);
begin
  Assert(Length(FStreams)=0, 'You can''t mix stream output with render to textures');
  if index <= Length(FTex) then
  begin
    SetLength(FViews, index + 1);
    SetLength(FTex, index + 1);
  end;

  if FTex[index].Tex2D = tex then Exit;

  FTex[index].Tex2D := tex;
  FTex[index].Tex3D := nil;
  FTex[index].Mip := mipLevel;
  FTex[index].SliceStart := sliceStart;
  FTex[index].SliceCount := sliceCount;
  FTex[index].Enabled := True;
  FViews[index] := nil;

  FValid := False;
end;

procedure TFrameBuffer.SetColor3D(index: Integer; tex: IctxTexture3D; mipLevel: Integer; sliceStart: Integer; sliceCount: Integer);
begin
  Assert(Length(FStreams)=0, 'You can''t mix stream output with render to textures');
  if index <= Length(FTex) then
  begin
    SetLength(FViews, index + 1);
    SetLength(FTex, index + 1);
  end;

  if FTex[index].Tex3D = tex then Exit;

  FTex[index].Tex2D := nil;
  FTex[index].Tex3D := tex;
  FTex[index].Mip := mipLevel;
  FTex[index].SliceStart := sliceStart;
  FTex[index].SliceCount := sliceCount;
  FTex[index].Enabled := True;
  FViews[index] := nil;

  FValid := False;

end;

procedure TFrameBuffer.SetDepthStencil(tex: IctxTexture; mipLevel: Integer; sliceStart: Integer; sliceCount: Integer);
begin
  Assert((tex = nil) or (Length(FStreams)=0), 'You can''t mix stream output with render to textures');
  if FDepthTex = tex then Exit;

  FDepthTex := tex;
  FDepthView := nil;
  FDepthMip := mipLevel;
  FDepthSliceStart := sliceStart;
  FDepthSliceCount := sliceCount;

  FValid := False;
end;

procedure TFrameBuffer.SetStreamOut(index: Integer; buffer: IctxVetexBuffer; Offset: Integer);
begin
  Assert(Length(FTex)=0, 'You can''t mix stream output with render to textures');
  Assert(Length(FUAV)=0, 'You can''t mix stream output with render to UAV');
  if index <= Length(FStreams) then
  begin
    SetLength(FStreams, index + 1);
    SetLength(FStreamOffsets, index + 1);
    SetLength(FStreamBuffers, index + 1);
  end;

  if FStreams[index].SO = buffer then Exit;

  FStreams[index].SO := buffer;
  FStreamOffsets[index] := Offset;
  FStreamBuffers[index] := (buffer as IctxVetexBuffer_DX).Handle;

  FValid := False;
end;

procedure TFrameBuffer.SetUAV_Internal(index: Integer; const UAV: IctxUAV_DX11);
begin
  Assert(Length(FStreams)=0, 'You can''t mix stream output with render to UAV');
  if index <= Length(FUAV) then
  begin
    SetLength(FUAV, index + 1);
    SetLength(FUAVViews, index + 1);
    SetLength(FUAVInitials, index + 1);
  end;

  if FUAV[index].UAV = UAV then Exit;

  FUAV[index].UAV := UAV;
  FUAVViews[index] := UAV.GetView;
  FUAVInitials[index] := 0;
end;

procedure TFrameBuffer.SetUAVTex(index: Integer; UAV: IctxTexture);
begin
  Assert(Length(FStreams)=0, 'You can''t mix stream output with render to UAV');
  SetUAV_Internal(index, UAV as IctxUAV_DX11);
end;

procedure TFrameBuffer.SetUAVTex3D(index: Integer; UAV: IctxTexture3D);
begin
  Assert(Length(FStreams)=0, 'You can''t mix stream output with render to UAV');
  SetUAV_Internal(index, UAV as IctxUAV_DX11);
end;

procedure TFrameBuffer.SetUAV(index: Integer; UAV: IctxUAV);
begin
  Assert(Length(FStreams)=0, 'You can''t mix stream output with render to UAV');
  SetUAV_Internal(index, UAV as IctxUAV_DX11);
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

procedure TFrameBuffer.ClearUAV(index: Integer; color: TVec4i);
begin
  Assert(FUAVViews[index] <> nil);
  FContext.FDeviceContext.ClearUnorderedAccessViewUint(FUAVViews[index], TQuadArray_UInt(color));
end;

procedure TFrameBuffer.BlitToWindow(index: Integer; const srcRect,
  dstRect: TRectI; const Filter: TTextureFilter);
var SrcBox: TD3D11_Box;
    desc: TD3D11_Texture2DDesc;
begin
  //todo blitting with stretch
  if FTex[index].Tex2D = nil then Exit;
  SrcBox.left := srcRect.Left;
  SrcBox.top := srcRect.Top;
  SrcBox.right := srcRect.Right;
  SrcBox.bottom := srcRect.Bottom;
  SrcBox.front := 0;
  SrcBox.back := 1;
  if FTex[index].Tex2D.SampleCount > 1 then
  begin
    FContext.FBackBuffer.GetDesc(desc);
    FContext.FDeviceContext.ResolveSubresource(FContext.FBackBuffer, 0, (FTex[index].Tex2D as IctxTexture_DX11).GetHandle, 0, desc.Format);
  end
  else
  begin
    FContext.FDeviceContext.CopySubresourceRegion(FContext.FBackBuffer, 0, dstRect.Left, dstRect.Top, 0,
                                                  (FTex[index].Tex2D as IctxTexture_DX11).GetHandle, FTex[index].Mip, @SrcBox);
  end;
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
  Result := Nil;
end;

procedure TContext_DX11.SetActiveProgram(AValue: IctxProgram);
begin

end;

procedure TContext_DX11.RebuildViews(const AWidth, AHeight: Cardinal);
var SwapChainDesc: TDXGI_SwapChainDesc;
begin
  FSwapChain.GetDesc(SwapChainDesc);
  if (SwapChainDesc.BufferDesc.Width = AWidth) and
     (SwapChainDesc.BufferDesc.Height = AHeight) and
     Assigned(FRenderTarget) then Exit;
  FRenderTarget := nil;
  FBackBuffer := nil;

  FDeviceContext.ClearState;
  TStates(FStates).SetupViewPort;
  TStates(FStates).InvalidateAllStates;
  FSwapChain.ResizeBuffers(SwapChainDesc.BufferCount, AWidth, AHeight, SwapChainDesc.BufferDesc.Format, SwapChainDesc.Flags);

  Check3DError(FSwapChain.GetBuffer(0, ID3D11Texture2D, FBackBuffer));
  Check3DError(FDevice.CreateRenderTargetView(FBackBuffer, nil, FRenderTarget));

  FDeviceContext.OMSetRenderTargets(1, @FRenderTarget, nil);
  FDeviceContext.IASetPrimitiveTopology(FPrimTopology);
end;

procedure TContext_DX11.SetFrameBuffer(const AObject: TObject);
  procedure SetFBORenderTargets(const FBO: TFrameBuffer);
  var
      dummy: ID3D11RenderTargetView;
      dummyNull: PID3D11Buffer;
      dummyOffset: Integer;

      pViews: PID3D11RenderTargetView;
      pUAV  : PID3D11UnorderedAccessView;
      pUAVInit: PLongWord;
  begin
      dummy := nil;
      dummyNull := nil;
      dummyOffset := 0;
      FDeviceContext.OMSetRenderTargets(1, @dummy, nil);
      if Length(FBO.FStreams) > 0 then
      begin
        FDeviceContext.SOSetTargets(Length(FBO.FStreams), @FBO.FStreamBuffers[0], @FBO.FStreamOffsets[0]);
      end
      else
      begin
        if Length(FBO.FUAVViews) = 0 then
        begin
            if Length(FBO.FViews) = 0 then
              FDeviceContext.OMSetRenderTargets(0, nil, FBO.FDepthView)
            else
              FDeviceContext.OMSetRenderTargets(Length(FBO.FViews), @FBO.FViews[0], FBO.FDepthView);
            FDeviceContext.SOSetTargets(1, @dummyNull, @dummyOffset);
        end
        else
        begin
            pViews := nil;
            pUAV := nil;
            pUAVInit := nil;
            if Length(FBO.FViews) > 0 then pViews := @FBO.FViews[0];
            if Length(FBO.FUAVViews) > 0 then
            begin
              pUAV := @FBO.FUAVViews[0];
              pUAVInit := @FBO.FUAVInitials[0];
            end;

        //FDeviceContext.OMSetRenderTargetsAndUnorderedAccessViews(Length(FBO.FViews), @FBO.FViews[0], FBO.FDepthView);
            FDeviceContext.OMSetRenderTargetsAndUnorderedAccessViews(
              Length(FBO.FViews), pViews, FBO.FDepthView,
              Length(FBO.FViews), Length(FBO.FUAVViews), pUAV, pUAVInit
            );
            FDeviceContext.SOSetTargets(1, @dummyNull, @dummyOffset);
        end;
      end;
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
  const DX_CmpFilter: array [TTextureFilter, TTextureFilter, TTextureFilter] of TD3D11_Filter =
    (
       //min
       ( //mag
         (D3D11_FILTER_COMPARISON_MIN_MAG_MIP_POINT             , D3D11_FILTER_COMPARISON_MIN_MAG_MIP_POINT             , D3D11_FILTER_COMPARISON_MIN_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_COMPARISON_MIN_MAG_MIP_POINT             , D3D11_FILTER_COMPARISON_MIN_MAG_MIP_POINT             , D3D11_FILTER_COMPARISON_MIN_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_COMPARISON_MIN_POINT_MAG_LINEAR_MIP_POINT, D3D11_FILTER_COMPARISON_MIN_POINT_MAG_LINEAR_MIP_POINT, D3D11_FILTER_COMPARISON_MIN_POINT_MAG_MIP_LINEAR)
       ),
       ( //mag
         (D3D11_FILTER_COMPARISON_MIN_MAG_MIP_POINT             , D3D11_FILTER_COMPARISON_MIN_MAG_MIP_POINT             , D3D11_FILTER_COMPARISON_MIN_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_COMPARISON_MIN_MAG_MIP_POINT             , D3D11_FILTER_COMPARISON_MIN_MAG_MIP_POINT             , D3D11_FILTER_COMPARISON_MIN_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_COMPARISON_MIN_POINT_MAG_LINEAR_MIP_POINT, D3D11_FILTER_COMPARISON_MIN_POINT_MAG_LINEAR_MIP_POINT, D3D11_FILTER_COMPARISON_MIN_POINT_MAG_MIP_LINEAR)
       ),
       ( //mag
         (D3D11_FILTER_COMPARISON_MIN_LINEAR_MAG_MIP_POINT      , D3D11_FILTER_COMPARISON_MIN_LINEAR_MAG_MIP_POINT      , D3D11_FILTER_COMPARISON_MIN_LINEAR_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_COMPARISON_MIN_LINEAR_MAG_MIP_POINT      , D3D11_FILTER_COMPARISON_MIN_LINEAR_MAG_MIP_POINT      , D3D11_FILTER_COMPARISON_MIN_LINEAR_MAG_POINT_MIP_LINEAR),
         (D3D11_FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT      , D3D11_FILTER_COMPARISON_MIN_MAG_LINEAR_MIP_POINT      , D3D11_FILTER_COMPARISON_MIN_MAG_MIP_LINEAR)
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
    if ASampler.Comparison <> cfNever then
      Desc.Filter := DX_CmpFilter[ASampler.MinFilter, ASampler.MagFilter, ASampler.MipFilter]
    else
    begin
      if ASampler.Anisotropy > 0 then
        Desc.Filter := D3D11_FILTER_ANISOTROPIC
      else
        Desc.Filter := DX_Filter[ASampler.MinFilter, ASampler.MagFilter, ASampler.MipFilter];
    end;
    Desc.AddressU := D3D11_Wrap[ASampler.Wrap_X];
    Desc.AddressV := D3D11_Wrap[ASampler.Wrap_Y];
    Desc.AddressW := D3D11_Wrap[ASampler.Wrap_Z];
    Desc.MipLODBias := 0;
    Desc.MaxAnisotropy := Min(16, Max(1, ASampler.Anisotropy));
    Desc.MinLOD := 0;
    Desc.MaxLOD := D3D11_FLOAT32_MAX;
    Desc.BorderColor[0] := ASampler.Border.x;
    Desc.BorderColor[1] := ASampler.Border.y;
    Desc.BorderColor[2] := ASampler.Border.z;
    Desc.BorderColor[3] := ASampler.Border.w;
    Desc.ComparisonFunc := DXCompareFunc[ASampler.Comparison];
    Result := nil;
    Check3DError(FDevice.CreateSamplerState(Desc, Result));
    FSamplerMap.Add(ASampler, Result);
  end;
end;

function TContext_DX11.GetDevice: ID3D11Device;
begin
  Result := FDevice;
end;

function TContext_DX11.GetDeviceContext: ID3D11DeviceContext;
begin
  Result := FDeviceContext;
end;

function TContext_DX11.GetSwapChain: IDXGISwapChain;
begin
  Result := FSwapChain;
end;

procedure TContext_DX11.UpdateStates;
begin
  TStates(FStates).UpdateBlendState;
  TStates(FStates).UpdateRasterState;
  TStates(FStates).UpdateDepthState;
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

function TContext_DX11.CreateStructBuffer: IctxStructuredBuffer;
begin
  Result := TStructuredBuffer.Create(Self);
end;

function TContext_DX11.CreateTexture: IctxTexture;
begin
  Result := TTexture.Create(Self);
end;

function TContext_DX11.CreateTexture3D: IctxTexture3D;
begin
  Result := TTexture3D.Create(Self);
end;

function TContext_DX11.CreateUAV(const AElementsCount, AStrideSize: Cardinal; const Appendable: Boolean; const AInitialData: Pointer): IctxUAV;
begin
  Result := TUAV.Create(Self, AElementsCount, AStrideSize, Appendable, AInitialData);
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
  FBO := nil;
  if Assigned(FActiveFrameBuffer) then
    FBO := (FActiveFrameBuffer as IctxFrameBuffer_DX11).GetObj;
  if Assigned(FBO) then
  begin
    for i := 0 to Length(FBO.FViews) - 1 do
      FBO.Clear(i, color);
    if doDepth or doStencil then
      FBO.ClearDS(depth, doDepth, stencil, doStencil);
  end
  Else
  begin
    FDeviceContext.ClearRenderTargetView(FRenderTarget, TColorArray(color));
    //FDeviceContext.ClearDepthStencilView();
  end;
end;

procedure TContext_DX11.Flush;
begin
  FDeviceContext.Flush;
end;

procedure TContext_DX11.Present;
var hres, devRemove: HRESULT;
begin
  hres := FSwapChain.Present(0, 0);
  if (hres = DXGI_ERROR_DEVICE_REMOVED) then
  begin
    devRemove := FDevice.GetDeviceRemovedReason;
    if devRemove = S_OK then
      raise E3DError.Create('DXGI_ERROR_DEVICE_REMOVED occurs but GetDeviceRemovedReason==S_OK')
    else
      Check3DError(devRemove);
  end;
end;

constructor TContext_DX11.Create(const Wnd: TWindow; const WARP: Boolean);
var SwapChainDesc: TDXGI_SwapChainDesc;
    DriverType : TD3D_DriverType;
    dxgidev: IDXGIDevice1;
    featureLevel: TD3D_FeatureLevel;
begin
  FStates := TStates.Create(Self);
  FStatesIntf := TStates(FStates);

  FSamplerMap := TSamplerMap.Create;

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
  SwapChainDesc.Flags := 0;

  if WARP then
    DriverType := D3D_DRIVER_TYPE_WARP
  else
    DriverType := D3D_DRIVER_TYPE_HARDWARE;

  featureLevel := D3D_FEATURE_LEVEL_11_0;
  try
    Check3DError(
      D3D11CreateDeviceAndSwapChain(nil,
                                    DriverType, 0, 0{LongWord(D3D11_CREATE_DEVICE_SINGLETHREADED)} {Or LongWord(D3D11_CREATE_DEVICE_DEBUG)}, @featureLevel, 1, D3D11_SDK_VERSION,
                                    @SwapChainDesc, FSwapChain, FDevice, nil, FDeviceContext)
    );
	except
    on E: E3DError do
    begin
      raise ECreateContextFailed.Create('Can''t create D3D11 context');
  	end;
	end;
	if Supports(FDevice, IDXGIDevice1, dxgidev) then
    dxgidev.SetMaximumFrameLatency(1);
end;

destructor TContext_DX11.Destroy;
begin
  if FDeviceContext <> nil then
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

{ TUAV }

procedure TUAV.AllocMem(ASize: Integer; Data: PByte);
begin
  Assert(False, 'not available for UAV');
end;

constructor TUAV.Create(const AContext: TContext_DX11; const AElementsCount, AStrideSize: Cardinal; const Appendable: Boolean; const AInitialData: Pointer);
var bufdesc: TD3D11_BufferDesc;
    uavdesc: TD3D11_UnorderedAccessViewDesc;
    initData: TD3D11_SubresourceData;
begin
  inherited Create(AContext);
  FElementsCount := AElementsCount;
  FStrideSize := AStrideSize;

  // create sbuf + view
  bufdesc.ByteWidth := FElementsCount*FStrideSize;
  bufdesc.Usage := D3D11_USAGE_DEFAULT;
  bufdesc.BindFlags := Cardinal(D3D11_BIND_UNORDERED_ACCESS) Or Cardinal(D3D11_BIND_SHADER_RESOURCE);
  bufdesc.CPUAccessFlags := 0;
  bufdesc.MiscFlags := Cardinal(D3D11_RESOURCE_MISC_BUFFER_STRUCTURED);
  bufdesc.StructureByteStride := AStrideSize;

  if AInitialData = nil then
  begin
    initData.pSysMem := nil;
    initData.SysMemPitch := 0;
    initData.SysMemSlicePitch := 0;
    Check3DError( FContext.GetDevice.CreateBuffer(bufdesc, nil, FBuffer) )
  end
  else
  begin
    initData.pSysMem := AInitialData;
    initData.SysMemPitch := AElementsCount * AStrideSize;
    initData.SysMemSlicePitch := AElementsCount * AStrideSize;
    Check3DError( FContext.GetDevice.CreateBuffer(bufdesc, @initData, FBuffer) );
  end;

  uavdesc.Format := DXGI_FORMAT_UNKNOWN;
  uavdesc.ViewDimension := D3D11_UAV_DIMENSION_BUFFER;
  uavdesc.Buffer.FirstElement := 0;
  uavdesc.Buffer.NumElements := FElementsCount;
  if Appendable then
    uavdesc.Buffer.Flags := Cardinal(D3D11_BUFFER_UAV_FLAG_APPEND)
  else
    uavdesc.Buffer.Flags := Cardinal(D3D11_BUFFER_UAV_FLAG_COUNTER);
  Check3DError( FContext.GetDevice.CreateUnorderedAccessView(FBuffer, @uavdesc, FView) );
  //FContext.GetDeviceContext.
end;

function TUAV.ElementsCount: Cardinal;
begin
  Result := FElementsCount;
end;

function TUAV.GetElementSize: Integer;
begin
  Result := FStrideSize;
end;

function TUAV.GetTargetPoolType: TBufferPoolType;
begin
  Result := TBufferPoolType.StaticDraw;
end;

function TUAV.GetView: ID3D11UnorderedAccessView;
begin
  Result := FView;
end;

function TUAV.Handle: ID3D11Buffer;
begin
  Result := FBuffer;
end;

procedure TUAV.InvalidateCounter;
begin
  FLastCounterValid := False;
end;

function TUAV.Map(usage: TMapingUsage): PByte;
begin
  Result := nil;
  Assert(False, 'not available for UAV');
end;

function TUAV.ReadCounter: Cardinal;
var bufdesc: TD3D11_BufferDesc;
    cpubuf: ID3D11Buffer;
    mapdata: TD3D11_MappedSubresource;
begin
  if FLastCounterValid then Exit(FLastCounter);

  bufdesc.ByteWidth := 4;
  bufdesc.Usage := D3D11_USAGE_STAGING;
  bufdesc.BindFlags := 0;
  bufdesc.CPUAccessFlags := Cardinal(D3D11_CPU_ACCESS_READ);
  bufdesc.MiscFlags := 0;
  bufdesc.StructureByteStride := 0;
  Check3DError( FContext.GetDevice.CreateBuffer(bufdesc, nil, cpubuf) );

  FContext.GetDeviceContext.CopyStructureCount(cpubuf, 0, FView);

  Check3DError( FContext.GetDeviceContext.Map(cpubuf, 0, D3D11_MAP_READ, 0, mapdata) );
  FLastCounterValid := True;
  FLastCounter := PCardinal(mapdata.pData)^;
  FContext.GetDeviceContext.Unmap(cpubuf, 0);

  Result := FLastCounter;
end;

function TUAV.ReadRAWData(AElementsCount: Integer = -1): TByteArr;
var bufdesc: TD3D11_BufferDesc;
    copybox: TD3D11_Box;
    mapdata: TD3D11_MappedSubresource;
    cpubuf: ID3D11Buffer;
begin
  if AElementsCount < 0 then
      AElementsCount := ReadCounter;
  if AElementsCount = 0 then Exit(nil);
  SetLength(Result, AElementsCount*FStrideSize);

  bufdesc.ByteWidth := Length(Result);
  bufdesc.Usage := D3D11_USAGE_STAGING;
  bufdesc.BindFlags := 0;
  bufdesc.CPUAccessFlags := Cardinal(D3D11_CPU_ACCESS_READ);
  bufdesc.MiscFlags := 0;
  bufdesc.StructureByteStride := 0;
  Check3DError( FContext.GetDevice.CreateBuffer(bufdesc, nil, cpubuf) );

  copybox.Left := 0;
  copybox.Right := Length(Result);
  copybox.Top := 0;
  copybox.Bottom := 1;
  copybox.Front := 0;
  copybox.Back := 1;
  FContext.GetDeviceContext.CopySubresourceRegion(cpubuf, 0, 0, 0, 0, FBuffer, 0, @copybox);

  Check3DError( FContext.GetDeviceContext.Map(cpubuf, 0, D3D11_MAP_READ, 0, mapdata) );
  Move(mapdata.pData^, Result[0], Length(Result));
  FContext.GetDeviceContext.Unmap(cpubuf, 0);
end;

procedure TUAV.SetElementSize(const AValue: Integer);
begin
  Assert(False, 'not available for UAV');
end;

procedure TUAV.SetSubData(AOffset, ASize: Integer; Data: PByte);
begin
  Assert(False, 'not available for UAV');
end;

procedure TUAV.SetTargetPoolType(Value: TBufferPoolType);
begin
  Assert(False, 'not available for UAV');
end;

function TUAV.Size: Integer;
begin
  Result := FElementsCount * FStrideSize;
end;

function TUAV.StrideSize: Cardinal;
begin
  Result := FStrideSize;
end;

function TUAV.Unmap: Boolean;
begin
  Result := False;
  Assert(False, 'not available for UAV');
end;

function TUAV.View: ID3D11ShaderResourceView;
var desc: TD3D11_ShaderResourceViewDesc;
begin
  if FShaderView = nil then
  begin
    desc.Format := DXGI_FORMAT_UNKNOWN;
    desc.ViewDimension := D3D11_SRV_DIMENSION_BUFFER;
    desc.Buffer.FirstElement := 0;
    desc.Buffer.NumElements :=  FElementsCount;
    Check3DError( FContext.FDevice.CreateShaderResourceView(FBuffer, @desc, FShaderView) );
  end;
  Result := FShaderView;
end;

{ TStructuredBuffer }

procedure TStructuredBuffer.AllocMem(ASize: Integer; Data: PByte);
var desc: TD3D11_ShaderResourceViewDesc;
begin
  inherited;
  desc.Format := DXGI_FORMAT_UNKNOWN;
  desc.ViewDimension := D3D11_SRV_DIMENSION_BUFFER;
  desc.Buffer.FirstElement := 0;
  desc.Buffer.NumElements :=  ASize div FElSize;
  Check3DError( FContext.FDevice.CreateShaderResourceView(FBuffer, @desc, FView) );
end;

function TStructuredBuffer.GetBufferBindFlag: Cardinal;
begin
  Result := DWord(D3D11_BIND_SHADER_RESOURCE);
end;

function TStructuredBuffer.GetBufferMiscFlag: Cardinal;
begin
  Result := DWord(D3D11_RESOURCE_MISC_BUFFER_STRUCTURED);
end;

function TStructuredBuffer.GetElementSize: Integer;
begin
  Result := FElSize;
end;

function TStructuredBuffer.GetStructuredByteStride: Cardinal;
begin
  Result := FElSize;
end;

function TStructuredBuffer.Handle: ID3D11Buffer;
begin
  Result := FBuffer;
end;

procedure TStructuredBuffer.SetElementSize(const AValue: Integer);
begin
  FElSize := AValue;
end;

function TStructuredBuffer.View: ID3D11ShaderResourceView;
begin
  Result := FView;
end;

end.

