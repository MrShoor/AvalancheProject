unit avContext_OGL;
{$I avConfig.inc}
//{$DEFINE NOVAO}
//{$DEFINE NOglNamed}

interface

uses
  Classes, SysUtils, avTypes, avPlatform, dglOpenGL, Windows, mutils, avContext, avContnrs;

const
  MAX_RENDER_TARGET_COUNT = 8;

type
  TavInterfacedObject = TInterfacedObject;

  TObjListHash = {$IfDef FPC}specialize{$EndIf} THashMap<TObject, Boolean>;
  IObjListHash = {$IfDef FPC}specialize{$EndIf} IHashMap<TObject, Boolean>;

  { TContext_OGL }

  TContext_OGL = class (TavInterfacedObject, IRenderContext)
  private
    FWnd : TWindow;
    FDC  : HDC;
    FRC  : HGLRC;

    FStates: TObject;
    FStatesIntf: IRenderStates;

    FBindCount: Integer;

    FHandles: IObjListHash;
    FPrograms: TList;
    FDeletedHandles: TList;

    FActiveProgram: IctxProgram;

    procedure AddHandle(const HandleObject: TObject);
    procedure RemoveHandle(const HandleObject: TObject);
    procedure AddHandlesForCleanup(const HandleObject: TObject);

    procedure CleanUpHandles;
    function GetActiveProgram: IctxProgram;
    procedure SetActiveProgram(AValue: IctxProgram);
  public
    function CreateVertexBuffer : IctxVetexBuffer;
    function CreateIndexBuffer : IctxIndexBuffer;
    function CreateProgram : IctxProgram;
    function CreateTexture : IctxTexture;
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

    constructor Create(Const Wnd: TWindow);
    destructor Destroy; override;
  end;

  TVAOKey = record
    ModelVertex   : IctxVetexBuffer;
    ModelIndex    : IctxIndexBuffer;
    InstanceVertex: IctxVetexBuffer;
    InstanceStepRate: Integer;
    {$IfDef DCC}
    class operator Equal(a, b: TVAOKey): Boolean;
    {$EndIf}
  end;
  TVAOInfo = record
    VAO: Cardinal;
    BindTime  : Cardinal;
    Model     : IDataLayout;
    Instance  : IDataLayout;
    HasIndices: Boolean;
    {$IfDef DCC}
    class operator Equal(a, b: TVAOInfo): Boolean;
    {$EndIf}
  end;

{$IfDef FPC}
operator = (const a, b: TVAOKey): Boolean;
operator = (const a, b: TVAOInfo): Boolean;
{$EndIf}

implementation

uses SuperObject, avLog, Math;

const
  DEFAULT_BackBuffer: Boolean = False;

const
  GLPoolType: array [TBufferPoolType] of Cardinal = ( {StaticDraw }  GL_STATIC_DRAW,
                                                      {DynamicDraw}  GL_DYNAMIC_DRAW,
                                                      {StreamDraw }  GL_STREAM_DRAW
                                                    );
  GLPrimitiveType: array [TPrimitiveType] of Cardinal = ( {ptPoints}            GL_POINTS,
                                                          {ptLines}             GL_LINES,
                                                          {ptLineStrip}         GL_LINE_STRIP,
                                                          {ptTriangles}         GL_TRIANGLES,
                                                          {ptTriangleStrip}     GL_TRIANGLE_STRIP,
                                                          {ptLines_Adj}         GL_LINES_ADJACENCY,
                                                          {ptLineStrip_Adj}     GL_LINE_STRIP_ADJACENCY,
                                                          {ptTriangles_Adj}     GL_TRIANGLES_ADJACENCY,
                                                          {ptTriangleStrip_Adj} GL_TRIANGLE_STRIP_ADJACENCY,
                                                          {ptPatches}           GL_PATCHES);
  GLIndexSize: array [TIndexSize] of Cardinal = ( {Word}  GL_UNSIGNED_SHORT,
                                                  {DWord} GL_UNSIGNED_INT);
  GLTextureFormat: array [TTextureFormat] of Cardinal = (  {RGBA   } GL_RGBA,
                                                           {RGBA16 } GL_RGBA16,
                                                           {RGBA16f} GL_RGBA16F,
                                                           {RGBA32 } GL_RGBA32UI,
                                                           {RGBA32f} GL_RGBA32F,
                                                           {RGB    } GL_RGB,
                                                           {RGB16  } GL_RGB16,
                                                           {RGB16f } GL_RGB16F,
                                                           {RGB32  } GL_RGB32UI,
                                                           {RGB32f } GL_RGB32F,
                                                           {RG     } GL_RG,
                                                           {RG16   } GL_RG16,
                                                           {RG16f  } GL_RG16F,
                                                           {RG32   } GL_RG32UI,
                                                           {RG32f  } GL_RG32F,
                                                           {R      } GL_RED,
                                                           {R16F    } GL_R16,
                                                           {R16f   } GL_R16F,
                                                           {R32F    } GL_R32UI,
                                                           {R32f   } GL_R32F,
                                                           {DXT1   } GL_COMPRESSED_RGBA_S3TC_DXT1_EXT,
                                                           {DXT3   } GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,
                                                           {DXT5   } GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,
                                                           {D24_S8 } GL_DEPTH24_STENCIL8,
                                                           {D32f_S8} GL_DEPTH32F_STENCIL8,
                                                           {D16    } GL_DEPTH_COMPONENT16,
                                                           {D24    } GL_DEPTH_COMPONENT24,
                                                           {D32    } GL_DEPTH_COMPONENT32,
                                                           {D32f   } GL_DEPTH_COMPONENT32F
                                                       );
  GLAttachmetType: array [TTextureFormat] of Cardinal = (  {RGBA   } GL_COLOR_ATTACHMENT0,
                                                           {RGBA16 } GL_COLOR_ATTACHMENT0,
                                                           {RGBA16f} GL_COLOR_ATTACHMENT0,
                                                           {RGBA32 } GL_COLOR_ATTACHMENT0,
                                                           {RGBA32f} GL_COLOR_ATTACHMENT0,
                                                           {RGB    } GL_COLOR_ATTACHMENT0,
                                                           {RGB16  } GL_COLOR_ATTACHMENT0,
                                                           {RGB16f } GL_COLOR_ATTACHMENT0,
                                                           {RGB32  } GL_COLOR_ATTACHMENT0,
                                                           {RGB32f } GL_COLOR_ATTACHMENT0,
                                                           {RG     } GL_COLOR_ATTACHMENT0,
                                                           {RG16   } GL_COLOR_ATTACHMENT0,
                                                           {RG16f  } GL_COLOR_ATTACHMENT0,
                                                           {RG32   } GL_COLOR_ATTACHMENT0,
                                                           {RG32f  } GL_COLOR_ATTACHMENT0,
                                                           {R      } GL_COLOR_ATTACHMENT0,
                                                           {R16F    } GL_COLOR_ATTACHMENT0,
                                                           {R16f   } GL_COLOR_ATTACHMENT0,
                                                           {R32F    } GL_COLOR_ATTACHMENT0,
                                                           {R32f   } GL_COLOR_ATTACHMENT0,
                                                           {DXT1   } GL_COLOR_ATTACHMENT0,
                                                           {DXT3   } GL_COLOR_ATTACHMENT0,
                                                           {DXT5   } GL_COLOR_ATTACHMENT0,
                                                           {D24_S8 } GL_DEPTH_STENCIL_ATTACHMENT,
                                                           {D32f_S8} GL_DEPTH_STENCIL_ATTACHMENT,
                                                           {D16    } GL_DEPTH_ATTACHMENT,
                                                           {D24    } GL_DEPTH_ATTACHMENT,
                                                           {D32    } GL_DEPTH_ATTACHMENT,
                                                           {D32f   } GL_DEPTH_ATTACHMENT
                                                       );
  GLMapAccess: array [TMapingUsage] of Cardinal = ({muWriteOnly}GL_WRITE_ONLY,
                                                   {muReadOnly} GL_READ_ONLY,
                                                   {muReadWrite}GL_READ_WRITE);

const
  GLCompTypeSize: array [TComponentType] of Integer = ({ctBool}  1,
                                                       {ctByte}  1,
                                                       {ctUByte} 1,
                                                       {ctShort} 2,
                                                       {ctUShort}2,
                                                       {ctInt}   4,
                                                       {ctUInt}  4,
                                                       {ctFloat} 4,
                                                       {ctDouble}8);


{$IfDef FPC}
operator = (const a, b: TVAOKey): Boolean;
begin
  Result := CompareMem(@a, @b, SizeOf(TVAOKey));
end;
operator = (const a, b: TVAOInfo): Boolean;
begin
  Result := CompareMem(@a, @b, SizeOf(TVAOInfo));
end;
{$EndIf}

{$IfDef DCC}
class operator TVAOKey.Equal(a, b: TVAOKey): Boolean;
begin
  Result := CompareMem(@a, @b, SizeOf(TVAOKey));
end;

class operator TVAOInfo.Equal(a, b: TVAOInfo): Boolean;
begin
  Result := CompareMem(@a, @b, SizeOf(TVAOInfo));
end;
{$EndIf}

procedure VectorInfoOfDataType(datatype: Cardinal; out ElementClass: TDataClass;
                                                   out ElementType: TComponentType;
                                                   out ElementsCount: Integer);
begin
  case datatype of
    GL_FLOAT,
    GL_FLOAT_VEC2,
    GL_FLOAT_VEC3,
    GL_FLOAT_VEC4,
    GL_FLOAT_MAT2,
    GL_FLOAT_MAT3,
    GL_FLOAT_MAT4,
    GL_FLOAT_MAT2x3,
    GL_FLOAT_MAT2x4,
    GL_FLOAT_MAT3x2,
    GL_FLOAT_MAT3x4,
    GL_FLOAT_MAT4x2,
    GL_FLOAT_MAT4x3: ElementType := ctFloat;

    GL_DOUBLE,
    GL_DOUBLE_VEC2,
    GL_DOUBLE_VEC3,
    GL_DOUBLE_VEC4,
    GL_DOUBLE_MAT2,
    GL_DOUBLE_MAT3,
    GL_DOUBLE_MAT4,
    GL_DOUBLE_MAT2x3,
    GL_DOUBLE_MAT2x4,
    GL_DOUBLE_MAT3x2,
    GL_DOUBLE_MAT3x4,
    GL_DOUBLE_MAT4x2,
    GL_DOUBLE_MAT4x3: ElementType := ctDouble;

    GL_INT,
    GL_INT_VEC2,
    GL_INT_VEC3,
    GL_INT_VEC4,
    GL_SAMPLER_1D,
    GL_SAMPLER_2D,
    GL_SAMPLER_3D,
    GL_SAMPLER_CUBE,
    GL_SAMPLER_1D_SHADOW,
    GL_SAMPLER_2D_SHADOW,
    GL_SAMPLER_1D_ARRAY,
    GL_SAMPLER_2D_ARRAY,
    GL_SAMPLER_1D_ARRAY_SHADOW,
    GL_SAMPLER_2D_ARRAY_SHADOW,
    GL_SAMPLER_2D_MULTISAMPLE,
    GL_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_SAMPLER_CUBE_SHADOW,
    GL_SAMPLER_BUFFER,
    GL_SAMPLER_2D_RECT,
    GL_SAMPLER_2D_RECT_SHADOW,
    GL_INT_SAMPLER_1D,
    GL_INT_SAMPLER_2D,
    GL_INT_SAMPLER_3D,
    GL_INT_SAMPLER_CUBE,
    GL_INT_SAMPLER_1D_ARRAY,
    GL_INT_SAMPLER_2D_ARRAY,
    GL_INT_SAMPLER_2D_MULTISAMPLE,
    GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_INT_SAMPLER_BUFFER,
    GL_INT_SAMPLER_2D_RECT,
    GL_UNSIGNED_INT_SAMPLER_1D,
    GL_UNSIGNED_INT_SAMPLER_2D,
    GL_UNSIGNED_INT_SAMPLER_3D,
    GL_UNSIGNED_INT_SAMPLER_CUBE,
    GL_UNSIGNED_INT_SAMPLER_1D_ARRAY,
    GL_UNSIGNED_INT_SAMPLER_2D_ARRAY,
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE,
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_UNSIGNED_INT_SAMPLER_BUFFER,
    GL_UNSIGNED_INT_SAMPLER_2D_RECT : ElementType := ctInt;

    GL_UNSIGNED_INT,
    GL_UNSIGNED_INT_VEC2,
    GL_UNSIGNED_INT_VEC3,
    GL_UNSIGNED_INT_VEC4: ElementType := ctUInt;

    GL_BOOL,
    GL_BOOL_VEC2,
    GL_BOOL_VEC3,
    GL_BOOL_VEC4: ElementType := ctBool;
  else
    Assert(False, 'Uknown element type '+IntToHex(datatype, 0));
  end;

  case datatype of
    GL_FLOAT,
    GL_DOUBLE,
    GL_INT,
    GL_UNSIGNED_INT,
    GL_BOOL : ElementClass := dcScalar;

    GL_FLOAT_VEC2,
    GL_FLOAT_VEC3,
    GL_FLOAT_VEC4,
    GL_DOUBLE_VEC2,
    GL_DOUBLE_VEC3,
    GL_DOUBLE_VEC4,
    GL_INT_VEC2,
    GL_INT_VEC3,
    GL_INT_VEC4,
    GL_UNSIGNED_INT_VEC2,
    GL_UNSIGNED_INT_VEC3,
    GL_UNSIGNED_INT_VEC4,
    GL_BOOL_VEC2,
    GL_BOOL_VEC3,
    GL_BOOL_VEC4 : ElementClass := dcVector;


    GL_FLOAT_MAT2,
    GL_FLOAT_MAT3,
    GL_FLOAT_MAT4,
    GL_FLOAT_MAT2x3,
    GL_FLOAT_MAT2x4,
    GL_FLOAT_MAT3x2,
    GL_FLOAT_MAT3x4,
    GL_FLOAT_MAT4x2,
    GL_FLOAT_MAT4x3,
    GL_DOUBLE_MAT2,
    GL_DOUBLE_MAT3,
    GL_DOUBLE_MAT4,
    GL_DOUBLE_MAT2x3,
    GL_DOUBLE_MAT2x4,
    GL_DOUBLE_MAT3x2,
    GL_DOUBLE_MAT3x4,
    GL_DOUBLE_MAT4x2,
    GL_DOUBLE_MAT4x3: ElementClass := dcMatrix;

    GL_SAMPLER_1D,
    GL_SAMPLER_2D,
    GL_SAMPLER_3D,
    GL_SAMPLER_CUBE,
    GL_SAMPLER_1D_SHADOW,
    GL_SAMPLER_2D_SHADOW,
    GL_SAMPLER_1D_ARRAY,
    GL_SAMPLER_2D_ARRAY,
    GL_SAMPLER_1D_ARRAY_SHADOW,
    GL_SAMPLER_2D_ARRAY_SHADOW,
    GL_SAMPLER_2D_MULTISAMPLE,
    GL_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_SAMPLER_CUBE_SHADOW,
    GL_SAMPLER_BUFFER,
    GL_SAMPLER_2D_RECT,
    GL_SAMPLER_2D_RECT_SHADOW,
    GL_INT_SAMPLER_1D,
    GL_INT_SAMPLER_2D,
    GL_INT_SAMPLER_3D,
    GL_INT_SAMPLER_CUBE,
    GL_INT_SAMPLER_1D_ARRAY,
    GL_INT_SAMPLER_2D_ARRAY,
    GL_INT_SAMPLER_2D_MULTISAMPLE,
    GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_INT_SAMPLER_BUFFER,
    GL_INT_SAMPLER_2D_RECT,
    GL_UNSIGNED_INT_SAMPLER_1D,
    GL_UNSIGNED_INT_SAMPLER_2D,
    GL_UNSIGNED_INT_SAMPLER_3D,
    GL_UNSIGNED_INT_SAMPLER_CUBE,
    GL_UNSIGNED_INT_SAMPLER_1D_ARRAY,
    GL_UNSIGNED_INT_SAMPLER_2D_ARRAY,
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE,
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_UNSIGNED_INT_SAMPLER_BUFFER,
    GL_UNSIGNED_INT_SAMPLER_2D_RECT : ElementClass := dcSampler;
  else
    Assert(False, 'Uknown element type '+IntToHex(datatype, 0));
  end;

  case datatype of
    GL_FLOAT,
    GL_DOUBLE,
    GL_INT,
    GL_UNSIGNED_INT,
    GL_BOOL: ElementsCount := 1;

    GL_FLOAT_VEC2,
    GL_DOUBLE_VEC2,
    GL_INT_VEC2,
    GL_UNSIGNED_INT_VEC2,
    GL_BOOL_VEC2: ElementsCount := 2;

    GL_FLOAT_VEC3,
    GL_DOUBLE_VEC3,
    GL_INT_VEC3,
    GL_UNSIGNED_INT_VEC3,
    GL_BOOL_VEC3: ElementsCount := 3;

    GL_FLOAT_VEC4,
    GL_DOUBLE_VEC4,
    GL_INT_VEC4,
    GL_UNSIGNED_INT_VEC4,
    GL_BOOL_VEC4: ElementsCount := 4;

    GL_FLOAT_MAT2,
    GL_DOUBLE_MAT2: ElementsCount := 4;


    GL_FLOAT_MAT3,
    GL_DOUBLE_MAT3: ElementsCount := 9;

    GL_FLOAT_MAT4,
    GL_DOUBLE_MAT4: ElementsCount := 16;

    GL_FLOAT_MAT2x3,
    GL_FLOAT_MAT3x2,
    GL_DOUBLE_MAT2x3,
    GL_DOUBLE_MAT3x2: ElementsCount := 6;

    GL_FLOAT_MAT2x4,
    GL_FLOAT_MAT4x2,
    GL_DOUBLE_MAT2x4,
    GL_DOUBLE_MAT4x2: ElementsCount := 8;


    GL_FLOAT_MAT3x4,
    GL_FLOAT_MAT4x3,
    GL_DOUBLE_MAT3x4,
    GL_DOUBLE_MAT4x3: ElementsCount := 12;

    GL_SAMPLER_1D,
    GL_SAMPLER_2D,
    GL_SAMPLER_3D,
    GL_SAMPLER_CUBE,
    GL_SAMPLER_1D_SHADOW,
    GL_SAMPLER_2D_SHADOW,
    GL_SAMPLER_1D_ARRAY,
    GL_SAMPLER_2D_ARRAY,
    GL_SAMPLER_1D_ARRAY_SHADOW,
    GL_SAMPLER_2D_ARRAY_SHADOW,
    GL_SAMPLER_2D_MULTISAMPLE,
    GL_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_SAMPLER_CUBE_SHADOW,
    GL_SAMPLER_BUFFER,
    GL_SAMPLER_2D_RECT,
    GL_SAMPLER_2D_RECT_SHADOW,
    GL_INT_SAMPLER_1D,
    GL_INT_SAMPLER_2D,
    GL_INT_SAMPLER_3D,
    GL_INT_SAMPLER_CUBE,
    GL_INT_SAMPLER_1D_ARRAY,
    GL_INT_SAMPLER_2D_ARRAY,
    GL_INT_SAMPLER_2D_MULTISAMPLE,
    GL_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_INT_SAMPLER_BUFFER,
    GL_INT_SAMPLER_2D_RECT,
    GL_UNSIGNED_INT_SAMPLER_1D,
    GL_UNSIGNED_INT_SAMPLER_2D,
    GL_UNSIGNED_INT_SAMPLER_3D,
    GL_UNSIGNED_INT_SAMPLER_CUBE,
    GL_UNSIGNED_INT_SAMPLER_1D_ARRAY,
    GL_UNSIGNED_INT_SAMPLER_2D_ARRAY,
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE,
    GL_UNSIGNED_INT_SAMPLER_2D_MULTISAMPLE_ARRAY,
    GL_UNSIGNED_INT_SAMPLER_BUFFER,
    GL_UNSIGNED_INT_SAMPLER_2D_RECT : ElementsCount := 1;
  else
    ElementsCount := 0;
  end;
end;

function SizeOfDataType(datatype: Cardinal): Integer;
var ElClass: TDataClass;
    ElType: TComponentType;
    ElCount: Integer;
begin
  VectorInfoOfDataType(datatype, ElClass, ElType, ElCount);

  Result := GLCompTypeSize[ElType] * ElCount;
end;

function ReduceName(const AName: string): string;
var n: Integer;
begin
    n := Pos('[', AName);
    if n > 1 then
    begin
        Result := Copy(AName, 1, n - 1);
        Exit;
    end;
    n := Pos('.', AName);
    if n > 1 then
    begin
        Result := Copy(AName, 1, n - 1);
        Exit;
    end;
    Result := AName;
end;

function GetErrorStr(errorcode: Cardinal): string;
begin
  Result := '('+IntToHex(errorcode, 8)+')';
end;

function GetGLErrorStr(errorcode: Cardinal): string;
begin
  case ErrorCode of
    GL_NO_ERROR                      : Result := 'GL_NO_ERROR';
    GL_INVALID_ENUM                  : Result := 'GL_INVALID_ENUM';
    GL_INVALID_VALUE                 : Result := 'GL_INVALID_VALUE';
    GL_INVALID_OPERATION             : Result := 'GL_INVALID_OPERATION';
    GL_INVALID_FRAMEBUFFER_OPERATION : Result := 'GL_INVALID_FRAMEBUFFER_OPERATION';
    GL_OUT_OF_MEMORY                 : Result := 'GL_OUT_OF_MEMORY';
    GL_STACK_UNDERFLOW               : Result := 'GL_STACK_UNDERFLOW';
    GL_STACK_OVERFLOW                : Result := 'GL_STACK_OVERFLOW';
  else
    Result := GetErrorStr(errorcode);
  end;
end;

procedure Raise3DError(const msg: string);
begin
  raise E3DError.Create(msg);
end;

procedure RaiseLast3DError(const prefix: string);
begin
  Raise3DError(prefix + GetGLErrorStr(glGetError()));
end;

type

  { TStates_OGL }

  TStates_OGL = class (TNoRefObject, IRenderStates)
  private
    const
      GLCompareFunction : array [TCompareFunc] of Cardinal = (
        GL_NEVER,    // cfNever
        GL_LESS,     // cfLess
        GL_EQUAL,    // cfEqual
        GL_NOTEQUAL, // cfNotEqual
        GL_LEQUAL,   // cfLessEqual
        GL_GREATER,  // cfGreater
        GL_GEQUAL,   // cfGreaterEqual
        GL_ALWAYS    // cfAlways
      );

      GLStencilAction : array [TStencilAction] of Cardinal = (
        GL_KEEP,      // saKeep
        GL_REPLACE,   // saSet
        GL_ZERO,      // saZero
        GL_INVERT,    // saInvert
        GL_INCR,      // saInc
        GL_DECR,      // saDec
        GL_INCR_WRAP, // saIncWrap
        GL_DECR_WRAP  // saDecWrap
      );

      GLBlendFunction : array [TBlendFunc] of Cardinal = (
        GL_ZERO,                // bfZero
        GL_ONE,                 // bfOne
        GL_SRC_ALPHA,           // bfSrcAlpha
        GL_ONE_MINUS_SRC_ALPHA, // bfInvSrcAlpha
        GL_DST_ALPHA,           // bfDstAlpha
        GL_ONE_MINUS_DST_ALPHA, // bfInvDstAlpha
        GL_SRC_COLOR,           // bfSrcColor
        GL_DST_COLOR            // bfDstColor
      );
  private
    FContext      : TContext_OGL;
    FCullMode     : TCullingMode;
    FDepthWrite   : Boolean;
    FDepthTest    : Boolean;
    FDepthFunc    : TCompareFunc;
    FNearFarClamp : Boolean;
    FBlendSrc     : array [0..MAX_RENDER_TARGET_COUNT-1] of TBlendFunc;
    FBlendDest    : array [0..MAX_RENDER_TARGET_COUNT-1] of TBlendFunc;
    FBlending     : array [0..MAX_RENDER_TARGET_COUNT-1] of Boolean;
    FColorMask    : array [0..MAX_RENDER_TARGET_COUNT-1] of TColorMask;
    FViewport     : TRectI;
    FScissor      : TRectI;
    FScissorTest  : Boolean;
    FWireframe    : Boolean;
    FStencil      : Boolean;
    function GetBlendSrc (RenderTargetIndex: Integer = AllTargets): TBlendFunc;
    function GetBlendDest(RenderTargetIndex: Integer = AllTargets): TBlendFunc;
    function GetBlending (RenderTargetIndex: Integer = AllTargets): Boolean;
    function GetColorMask(RenderTargetIndex: Integer = AllTargets): TColorMask;
    function GetCullMode: TCullingMode;
    function GetDepthFunc: TCompareFunc;
    function GetDepthTest: Boolean;
    function GetDepthWrite: Boolean;
    function GetNearFarClamp: Boolean;
    function GetViewport: TRectI;
    function GetScissor: TRectI;
    function GetScissorTest: Boolean;
    function GetWireframe: Boolean;
    procedure SetCullMode(const Value: TCullingMode);
    procedure SetDepthTest(const Value: Boolean);
    procedure SetDepthWrite(const Value: Boolean);
    procedure SetDepthFunc(const Value: TCompareFunc);
    procedure SetNearFarClamp(const Value: Boolean);
    procedure SetColorMask(RenderTargetIndex: Integer; const Value: TColorMask);
    procedure SetBlending(RenderTargetIndex: Integer; const Value : Boolean);
    procedure SetViewport(const Value: TRectI);
    procedure SetScissor(const Value : TRectI);
    procedure SetScissorTest(const Value : Boolean);
    procedure SetWireframe(const Value: Boolean);
  public
    procedure SetBlendFunctions(Src, Dest: TBlendFunc; RenderTargetIndex: Integer = AllTargets);

    constructor Create(AContext: TContext_OGL);

    procedure ReadDefaultStates;

    procedure SetStencil(Enabled: Boolean; StencilFunc: TCompareFunc; Ref: Integer; Mask: Byte; sFail, dFail, dPass: TStencilAction);
  end;

  { IHandle }

  IHandle = interface
  ['{F2CF8FAC-F7E7-49AF-846F-439DBFFB289A}']
    function Handle: Cardinal;
  end;

  { THandle }

  THandle = class(TObject, IUnknown, IHandle)
  private
    FRefCount: Integer;

    function Handle: Cardinal;
    function QueryInterface({$IfDef FPC_HAS_CONSTREF}constref{$Else}const{$EndIf}iid: tguid; out obj): longint; stdcall;
    function _AddRef: longint; stdcall;
    function _Release: longint; stdcall;
  protected
    FContext: TContext_OGL;
    FHandle: Cardinal;

    procedure AllocHandle; virtual; abstract;
    procedure FreeHandle; virtual; abstract;
  public
    procedure AfterConstruction; override;
    constructor Create(AContext: TContext_OGL); overload; virtual;
    class function NewInstance: TObject; override;
    destructor Destroy; override;
  end;

  { TBufferBase }

  TBufferBase = class(THandle, IctxBuffer)
  protected
    const
        GLPoolType: array [TBufferPoolType] of Cardinal = ( {StaticDraw }  GL_STATIC_DRAW,
                                                            {DynamicDraw}  GL_DYNAMIC_DRAW,
                                                            {StreamDraw }  GL_STREAM_DRAW
                                                          );
        GLIndexSize: array [TIndexSize] of Cardinal = ( {Word}  GL_UNSIGNED_SHORT,
                                                        {DWord} GL_UNSIGNED_INT);
  private
    FSize: Integer;
    FTargetPool: TBufferPoolType;
    function GetTargetPoolType: TBufferPoolType;
    procedure SetTargetPoolType(Value: TBufferPoolType);
  protected
    procedure AllocHandle; override;
    procedure FreeHandle; override;

    property TargetPoolType: TBufferPoolType read GetTargetPoolType write SetTargetPoolType;

    function Size: Integer;

    function Map(usage: TMapingUsage): PByte; virtual; abstract;
    function Unmap: Boolean; virtual; abstract;
    procedure AllocMem(ASize: Integer; Data: PByte); overload;
    procedure SetSubData(AOffset, ASize: Integer; Data: PByte); overload;
  end;

  { TVertexBuffer }

  TVertexBuffer = class(TBufferBase, IctxVetexBuffer)
  private
    FLayout: IDataLayout;
  public
    function Map(usage: TMapingUsage): PByte; override;
    function Unmap: Boolean; override;
  public
    function GetLayout: IDataLayout;
    procedure SetLayout(const Value: IDataLayout);

    function VertexCount: Integer;
    property Layout: IDataLayout read GetLayout write SetLayout;
  end;

  { TIndexBuffer }

  TIndexBuffer = class(TBufferBase, IctxIndexBuffer)
  private
    FIndexSize: TIndexSize;
    FPrimType: TPrimitiveType;
  public
    function Map(usage: TMapingUsage): PByte; override;
    function Unmap: Boolean; override;
  public
    //*******
    function GetIndexSize: TIndexSize;
    function GetPrimType: TPrimitiveType;
    procedure SetIndexSize(AValue: TIndexSize);
    procedure SetPrimType(AValue: TPrimitiveType);
    //*******
    function IndicesCount: Integer;
    function PrimCount: Integer;
    Property IndexSize: TIndexSize Read GetIndexSize Write SetIndexSize;
    property PrimType: TPrimitiveType read GetPrimType write SetPrimType;
  end;

  { TTexture }

  TTexture = class(THandle, IctxTexture)
  private const
    GLTexBinding   : array [Boolean] of GLuint = (GL_TEXTURE_BINDING_2D, GL_TEXTURE_BINDING_2D_ARRAY);
    GLImagePixelFormat: array [TImageFormat] of Cardinal = ( {Unknown      }  GL_NONE,
                                                             {Gray8        }  GL_ALPHA,
                                                             {R3G3B2       }  GL_RGB,
                                                             {R8G8         }  GL_RG,
                                                             {R5G6B5       }  GL_RGB,
                                                             {A1R5G5B5     }  GL_BGRA,
                                                             {A4R4G4B4     }  GL_BGRA,
                                                             {R8G8B8       }  GL_BGR,
                                                             {A8R8G8B8     }  GL_BGRA,
                                                             {A8B8G8R8     }  GL_RGBA,
                                                             {R16F         }  GL_RED,
                                                             {R16G16F      }  GL_RG,
                                                             {R16G16B16F   }  GL_BGR,  //??
                                                             {A16R16G16B16F}  GL_BGRA, //??
                                                             {B16G16R16F   }  GL_RGB,  //??
                                                             {A16B16G16R16F}  GL_RGBA, //??
                                                             {R32F         }  GL_RED,
                                                             {R32G32F      }  GL_RG,
                                                             {R32G32B32F   }  GL_RGB,
                                                             {A32R32G32B32F}  GL_RGBA,
                                                             {R32G32B32A32F}  GL_RGBA, //??
                                                             {DXT1         }  GL_COMPRESSED_RGBA_S3TC_DXT1_EXT,
                                                             {DXT3         }  GL_COMPRESSED_RGBA_S3TC_DXT3_EXT,
                                                             {DXT5         }  GL_COMPRESSED_RGBA_S3TC_DXT5_EXT,
                                                             {D24_S8       }  GL_DEPTH_STENCIL,
                                                             {D32f_S8      }  GL_DEPTH_STENCIL,
                                                             {D16          }  GL_DEPTH_COMPONENT,
                                                             {D24          }  GL_DEPTH_COMPONENT,
                                                             {D32          }  GL_DEPTH_COMPONENT,
                                                             {D32f         }  GL_DEPTH_COMPONENT,
                                                             {R16          }  GL_RED,
                                                             {R16G16       }  GL_RG,
                                                             {R16G16B16    }  GL_RGB,
                                                             {A16R16G16B16 }  GL_BGRA,
                                                             {B16G16R16    }  GL_BGR,
                                                             {A16B16G16R16 }  GL_RGBA,
                                                             {R32          }  GL_RED,
                                                             {R32G32       }  GL_RG,
                                                             {R32G32B32    }  GL_RGB,
                                                             {A32R32G32B32 }  GL_BGRA,
                                                             {A32B32G32R32 }  GL_RGBA
                                                           );
    GLImageComponentFormat: array [TImageFormat] of Cardinal = ( {Unknown      }  GL_NONE,
                                                                 {Gray8        }  GL_UNSIGNED_BYTE,
                                                                 {R3G3B2       }  GL_UNSIGNED_BYTE_2_3_3_REV,
                                                                 {R8G8         }  GL_UNSIGNED_BYTE,
                                                                 {R5G6B5       }  GL_UNSIGNED_SHORT_5_6_5,
                                                                 {A1R5G5B5     }  GL_UNSIGNED_SHORT_1_5_5_5_REV,
                                                                 {A4R4G4B4     }  GL_UNSIGNED_SHORT_4_4_4_4,
                                                                 {R8G8B8       }  GL_UNSIGNED_BYTE,
                                                                 {A8R8G8B8     }  GL_UNSIGNED_INT_8_8_8_8_REV,
                                                                 {A8B8G8R8     }  GL_UNSIGNED_BYTE,
                                                                 {R16F         }  GL_HALF_FLOAT,
                                                                 {R16G16F      }  GL_HALF_FLOAT,
                                                                 {R16G16B16F   }  GL_HALF_FLOAT,
                                                                 {A16R16G16B16F}  GL_HALF_FLOAT,
                                                                 {B16G16R16F   }  GL_HALF_FLOAT,
                                                                 {A16B16G16R16F}  GL_HALF_FLOAT,
                                                                 {R32F         }  GL_FLOAT,
                                                                 {R32G32F      }  GL_FLOAT,
                                                                 {R32G32B32F   }  GL_FLOAT,
                                                                 {A32R32G32B32F}  GL_FLOAT,
                                                                 {R32G32B32A32F}  GL_FLOAT,
                                                                 {DXT1         }  GL_NONE,
                                                                 {DXT3         }  GL_NONE,
                                                                 {DXT5         }  GL_NONE,
                                                                 {D24_S8       }  GL_UNSIGNED_INT_24_8,
                                                                 {D32f_S8      }  GL_FLOAT_32_UNSIGNED_INT_24_8_REV,
                                                                 {D16          }  GL_UNSIGNED_SHORT,
                                                                 {D24          }  GL_UNSIGNED_INT_24_8,
                                                                 {D32          }  GL_UNSIGNED_INT,
                                                                 {D32f         }  GL_FLOAT,
                                                                 {R16          }  GL_UNSIGNED_SHORT,
                                                                 {R16G16       }  GL_UNSIGNED_SHORT,
                                                                 {R16G16B16    }  GL_UNSIGNED_SHORT,
                                                                 {A16R16G16B16 }  GL_NONE,
                                                                 {B16G16R16    }  GL_UNSIGNED_SHORT,
                                                                 {A16B16G16R16 }  GL_NONE,
                                                                 {R32          }  GL_UNSIGNED_INT,
                                                                 {R32G32       }  GL_UNSIGNED_INT,
                                                                 {R32G32B32    }  GL_UNSIGNED_INT,
                                                                 {A32R32G32B32 }  GL_NONE,
                                                                 {A32B32G32R32 }  GL_NONE
                                                               );
  private
    FFormat : TTextureFormat;
    FWidth  : Integer;
    FHeight : Integer;
    FDeep   : Integer;
    FIsArray : Boolean;
    FMipsCount: Integer;
    FTargetFormat : TTextureFormat;
    FSampleCount: Integer;

    FGLTexTarget: GLuint;

    function GetTargetFormat: TTextureFormat;
    procedure SetTargetFormat(Value: TTextureFormat);

    procedure AllocMem(AWidth, AHeight, ADeep: Integer; glFormat, exFormat, compFormat: Cardinal; Data: PByte; GenMipmaps: Boolean; ForcedArray: Boolean); overload;
  public
    procedure AllocHandle; override;
    procedure FreeHandle; override;

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

    procedure GenerateMips;

    procedure CopyFrom(const DstMipLevel: Integer; const DstPos: TVec2I;
                       const ASrcRes: IctxTexture; const SrcMipLevel: Integer; const SrcRect: TRectI);
  end;

  { TProgram }

  TProgram = class(THandle, IctxProgram)
  private
    type
      TUniformField_OGL = class (TUniformField)
      public
        FData: array of Byte;
        ID: Cardinal;
        OGLType: Cardinal;
      end;

      TAttributeField_OGL = class
          ID           : Integer;
          Name         : String;
          DataType     : Cardinal;
          DataClass    : TDataClass;
          ElementType  : TComponentType;
          ElementsCount: Integer;
      end;
    const
      GLComponentType: array [TComponentType] of Cardinal = ({ctBool}   GL_BOOL,
                                                             {ctByte}   GL_BYTE,
                                                             {ctUByte}  GL_UNSIGNED_BYTE,
                                                             {ctShort}  GL_SHORT,
                                                             {ctUShort} GL_UNSIGNED_SHORT,
                                                             {ctInt}    GL_INT,
                                                             {ctUInt}   GL_UNSIGNED_INT,
                                                             {ctFloat}  GL_FLOAT,
                                                             {ctDouble} GL_DOUBLE);
      GLMagTextureFilter : array [TTextureFilter] of Cardinal = (
        GL_NEAREST,
        GL_NEAREST,
        GL_LINEAR
      );
                                 //mip           //min
      GLMinTextureFilter: array [TTextureFilter, TTextureFilter] of Cardinal = ((GL_NEAREST,                GL_NEAREST,                GL_LINEAR),                     //no mips
                                                                                (GL_NEAREST_MIPMAP_NEAREST, GL_NEAREST_MIPMAP_NEAREST, GL_LINEAR_MIPMAP_NEAREST),      //nearest mip
                                                                                (GL_NEAREST_MIPMAP_LINEAR , GL_NEAREST_MIPMAP_LINEAR , GL_LINEAR_MIPMAP_LINEAR ) );    //linear mips
      GLWrap : array [TTextureWrap] of Cardinal = (
        GL_REPEAT,          // twRepeat
        GL_MIRRORED_REPEAT, // twMirror
        GL_CLAMP_TO_EDGE,   // twClamp
        GL_CLAMP_TO_BORDER  // twClampToEdge
      );
    type
      TVaoHash = {$IfDef FPC}specialize{$EndIf} THashMap<TVAOKey, TVAOInfo>;
      IVaoHash = {$IfDef FPC}specialize{$EndIf} IHashMap<TVAOKey, TVAOInfo>;

      TUniformHash = {$IfDef FPC}specialize{$EndIf} THashMap<string, TUniformField_OGL>;
      IUniformHash = {$IfDef FPC}specialize{$EndIf} IHashMap<string, TUniformField_OGL>;
      TAttributeHash = {$IfDef FPC}specialize{$EndIf} THashMap<string, TAttributeField_OGL>;
      IAttributeHash = {$IfDef FPC}specialize{$EndIf} IHashMap<string, TAttributeField_OGL>;
  private
    FUniformList: IUniformHash;
    FAttrList: IAttributeHash;
    FVAOList : IVaoHash;
    FValidated: Boolean;

    FSelectedVAOKey: TVAOKey;
    FSelectedVAOBinded: Boolean;
  protected
    procedure ClearUniformList;
    procedure ClearAttrList;
    procedure ClearVAOList;

    function CreateShader(ACode: AnsiString; AType: TShaderType): Cardinal;
    procedure DetachAllShaders;
    procedure ReadUniforms;
    procedure ReadAttributes;

    procedure BindVAO(const AKey: TVAOKey);

    function GetAttributeField(const name: string): TAttributeField_OGL;
    procedure SetAttribute(AttrField: TAttributeField_OGL; componentsCount, componentsType: integer; stride, offset: integer; divisor: Integer; normalized: Boolean = False); overload;
    procedure SetAttributes(const ALayout: IDataLayout; divisor: Integer); overload;

    function GetProgramInfoLog: string;

    procedure ValidateProgram;
    procedure CleanUpUselessVAO;
  protected
    procedure AllocHandle; override;
    procedure FreeHandle; override;
  public //IctxProgram
    procedure Select(const APatchSize: Integer = 0);
    procedure Load(const AProgram: string; FromResource: Boolean = false);

    procedure SetAttributes(const AModel, AInstances : IctxVetexBuffer; const AModelIndices: IctxIndexBuffer; InstanceStepRate: Integer = 1); overload;

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
  public
    constructor Create(AContext: TContext_OGL); override;
    destructor Destroy; override;
  end;

  { TFrameBuffer }

  TFrameBuffer = class(THandle, IctxFrameBuffer)
  private const

  private
    FMaxColorAttacments : Integer;

    FEnabledColorTargets: array of Boolean;
    FGLEnabled          : array of GLenum;
    FValid: Boolean;
    procedure Invalidate;
  protected
    procedure AllocHandle; override;
    procedure FreeHandle; override;
  public
    procedure Select;

    procedure ClearColorList;
    procedure EnableColorTarget(index: Integer; Enabled: Boolean);
    procedure SetColor(index: Integer; tex: IctxTexture; mipLevel: Integer);
    procedure SetDepthStencil(tex: IctxTexture; mipLevel: Integer);
    procedure SetUAV(index: Integer; UAV: IctxUAV);

    procedure Clear(index: Integer; color: TVec4);
    procedure ClearDS(depth: Single; clearDepth: Boolean = True; stencil: Integer = 0; clearStencil: Boolean = False);
    procedure ResetUAVCounters;

    procedure BlitToWindow(index: Integer; const srcRect, dstRect: TRectI; const Filter: TTextureFilter);
  public
    constructor Create(AContext: TContext_OGL); override;
//    destructor Destroy; override;
  end;

procedure TFrameBuffer.Invalidate;
begin
  FValid := False;
end;

procedure TFrameBuffer.ResetUAVCounters;
begin
  Assert(False, 'qwe not implemented');
end;

procedure TFrameBuffer.AllocHandle;
begin
  glGenFramebuffers(1, @FHandle);
end;

procedure TFrameBuffer.FreeHandle;
begin
  glDeleteFramebuffers(1, @FHandle);
  FHandle := 0;
end;

procedure TFrameBuffer.Select;
var i, n: Integer;
    status: Integer;
    err: String;
begin
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FHandle);
  n := 0;
  for i := 0 to Length(FEnabledColorTargets) - 1 do
    if FEnabledColorTargets[i] then
    begin
      FGLEnabled[n] := GL_COLOR_ATTACHMENT0 + i;
      Inc(n);
    end;

  if n = 1 then
    glDrawBuffer(FGLEnabled[0])
  else
    glDrawBuffers(n, @FGLEnabled[0]);

  if not FValid then
  begin
    status := glCheckFramebufferStatus(GL_DRAW_FRAMEBUFFER);
    err:='error at glCheckFramebufferStatus: ';
    case status of
      GL_FRAMEBUFFER_UNDEFINED: err:=err+': GL_FRAMEBUFFER_UNDEFINED';
      GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT: err:=err+': GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT';
      GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT: err:=err+': GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT';
      GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER: err:=err+': GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER';
      GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER: err:=err+': GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER';
      GL_FRAMEBUFFER_UNSUPPORTED: err:=err+': GL_FRAMEBUFFER_UNSUPPORTED';
      GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE: err:=err+': GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE';
      GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS: err:=err+': GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS';
    end;
    LogLn(err);
    FValid := True;
  end;
end;

procedure TFrameBuffer.ClearColorList;
var i: Integer;
    ActiveObject: GLuint;
begin
  {$IFNDEF NOglNamed}
  if Assigned(glNamedFramebufferTexture2DEXT) then
  begin
    for i := 0 to FMaxColorAttacments - 1 do
      glNamedFramebufferTexture2DEXT(FHandle, GL_COLOR_ATTACHMENT0+i, GL_TEXTURE_2D, 0, 0);
  end
  else
  {$ENDIF}
  begin
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, @ActiveObject);
    if ActiveObject <> FHandle then glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FHandle);
    for i := 0 to FMaxColorAttacments - 1 do
      glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0+i, GL_TEXTURE_2D, 0, 0);
    if ActiveObject <> FHandle then glBindFramebuffer(GL_DRAW_FRAMEBUFFER, ActiveObject);
  end;
  for i := 0 to Length(FEnabledColorTargets) - 1 do
    FEnabledColorTargets[i] := False;
  Invalidate;
end;

procedure TFrameBuffer.EnableColorTarget(index: Integer; Enabled: Boolean);
begin
  FEnabledColorTargets[index] := Enabled;
end;

procedure TFrameBuffer.SetColor(index: Integer; tex: IctxTexture; mipLevel: Integer);
var ActiveObject: GLuint;
    TexH: GLuint;
begin
  if Assigned(tex) then
    TexH := (tex as IHandle).Handle
  else
    TexH := 0;
  {$IFNDEF NOglNamed}
  if Assigned(glNamedFramebufferTexture2DEXT) then
  begin
      glNamedFramebufferTexture2DEXT(FHandle, GL_COLOR_ATTACHMENT0+index, GL_TEXTURE_2D, TexH, mipLevel);
  end
  else
  {$ENDIF}
  begin
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, @ActiveObject);
    if ActiveObject <> FHandle then glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FHandle);
    glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_COLOR_ATTACHMENT0+index, GL_TEXTURE_2D, TexH, mipLevel);
    if ActiveObject <> FHandle then glBindFramebuffer(GL_DRAW_FRAMEBUFFER, ActiveObject);
  end;
  FEnabledColorTargets[index] := Assigned(tex);
  Invalidate;
end;

procedure TFrameBuffer.SetDepthStencil(tex: IctxTexture; mipLevel: Integer);
var ActiveObject: GLuint;
    GLAttType: GLuint;
    TexH: GLuint;
begin
  if Assigned(tex) then
  begin
    TexH := (tex as IHandle).Handle;
    GLAttType := GLAttachmetType[tex.Format];
  end
  else
  begin
    TexH := 0;
    GLAttType := GL_DEPTH_STENCIL_ATTACHMENT;
  end;
  Assert(GLAttType <> GL_COLOR_ATTACHMENT0);
  {$IFNDEF NOglNamed}
  if Assigned(glNamedFramebufferTexture2DEXT) then
  begin
      glNamedFramebufferTexture2DEXT(FHandle, GLAttType, GL_TEXTURE_2D, TexH, mipLevel);
      if TexH = 0 then
        glNamedFramebufferTexture2DEXT(FHandle, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, TexH, mipLevel);
  end
  else
  {$ENDIF}
  begin
    glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, @ActiveObject);
    if ActiveObject <> FHandle then glBindFramebuffer(GL_DRAW_FRAMEBUFFER, FHandle);
    glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GLAttType, GL_TEXTURE_2D, TexH, mipLevel);
    if TexH = 0 then
      glFramebufferTexture2D(GL_DRAW_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_TEXTURE_2D, TexH, mipLevel);
    if ActiveObject <> FHandle then glBindFramebuffer(GL_DRAW_FRAMEBUFFER, ActiveObject);
  end;
  Invalidate;
end;

procedure TFrameBuffer.SetUAV(index: Integer; UAV: IctxUAV);
begin
  Assert(False, 'qwe not implemented');
end;

procedure TFrameBuffer.Clear(index: Integer; color: TVec4);
begin
  glClearBufferfv(GL_COLOR, index, @color);
end;

procedure TFrameBuffer.ClearDS(depth: Single; clearDepth: Boolean; stencil: Integer; clearStencil: Boolean);
begin
  if clearDepth and clearStencil then
    glClearBufferfi(GL_DEPTH_STENCIL, 0, depth, stencil)
  else
  if clearDepth then
    glClearBufferfv(GL_DEPTH, 0, @depth)
  else
  if clearStencil then
    glClearBufferiv(GL_STENCIL, 0, @stencil);
end;

procedure TFrameBuffer.BlitToWindow(index: Integer; const srcRect, dstRect: TRectI; const Filter: TTextureFilter);
const GLFilter: array [TTextureFilter] of GLuint = (GL_NEAREST, GL_NEAREST,  GL_LINEAR);
      GLTargetBuffer: array [Boolean] of GLuint = (GL_FRONT, GL_BACK);
begin
  glBindFramebuffer(GL_READ_FRAMEBUFFER, FHandle);
  glReadBuffer(GL_COLOR_ATTACHMENT0+index);
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  glDrawBuffer(GLTargetBuffer[DEFAULT_BackBuffer]);
  glBlitFramebuffer(srcRect.Left, srcRect.Top, srcRect.Right, srcRect.Bottom, dstRect.Left, dstRect.Top, dstRect.Right, dstRect.Bottom, GL_COLOR_BUFFER_BIT, GLFilter[Filter]);
end;

constructor TFrameBuffer.Create(AContext: TContext_OGL);
begin
  inherited Create(AContext);
  glGetIntegerv(GL_MAX_COLOR_ATTACHMENTS, @FMaxColorAttacments);
  SetLength(FEnabledColorTargets, FMaxColorAttacments);
  SetLength(FGLEnabled, FMaxColorAttacments);
end;

{ TTexture }

function TTexture.GetTargetFormat: TTextureFormat;
begin
  Result := FTargetFormat;
end;

procedure TTexture.SetTargetFormat(Value: TTextureFormat);
begin
  FTargetFormat := Value;
end;

procedure TTexture.AllocMem(AWidth, AHeight, ADeep: Integer; glFormat, exFormat, compFormat: Cardinal; Data: PByte; GenMipmaps: Boolean; ForcedArray: Boolean);
var i: Integer;
begin
  //todo: initialization with data
  FWidth := AWidth;
  FHeight := AHeight;
  FDeep := ADeep;
  FFormat := FTargetFormat;
  FSampleCount := 1;

  if GenMipmaps then
    FMipsCount := GetMipsCount(AWidth, AHeight)
  else
    FMipsCount := 1;

  FIsArray := (ADeep > 1) Or ForcedArray;
  if FIsArray then
  begin
    FGLTexTarget := GL_TEXTURE_2D_ARRAY;

    glActiveTexture(GL_TEXTURE31);
    glBindTexture(FGLTexTarget, FHandle);
    for i := 0 to FMipsCount - 1 do
    begin
      glTexImage3D(FGLTexTarget, i, glFormat, AWidth, AHeight, ADeep, 0, exFormat, compFormat, Data);
      AWidth := AWidth div 2;
      AHeight := AHeight div 2;
    end;
  end
  else
  begin
    FGLTexTarget := GL_TEXTURE_2D;

    glActiveTexture(GL_TEXTURE31);
    glBindTexture(FGLTexTarget, FHandle);
    for i := 0 to FMipsCount - 1 do
    begin
      glTexImage2D(FGLTexTarget, i, glFormat, AWidth, AHeight, 0, exFormat, compFormat, Data);
      AWidth := AWidth div 2;
      AHeight := AHeight div 2;
    end;
  end;
end;

procedure TTexture.AllocHandle;
begin
  glGenTextures(1, @FHandle);
end;

procedure TTexture.FreeHandle;
begin
  glDeleteTextures(1, @FHandle);
  FHandle := 0;
end;

function TTexture.Width: Integer;
begin
  Result := FWidth;
end;

function TTexture.Height: Integer;
begin
  Result := FHeight;
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
var maxSamples: Integer;
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FDeep := 1;
  FFormat := FTargetFormat;
  FMipsCount := 1;
  FIsArray := False;
  FSampleCount := ASampleCount;

  FGLTexTarget := GL_TEXTURE_2D_MULTISAMPLE;

  glGetIntegerv(GL_MAX_SAMPLES, @maxSamples);

  glActiveTexture(GL_TEXTURE31);
  glBindTexture(FGLTexTarget, FHandle);
  glTexImage2DMultisample(FGLTexTarget, max(0, min(maxSamples, FSampleCount))-1, GLTextureFormat[FTargetFormat], AWidth, AHeight, False);
end;

procedure TTexture.AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean; ForcedArray: Boolean);
var exFormat: Cardinal;
    compType: Cardinal;
begin
  case TargetFormat of
    TTextureFormat.D32f_S8 : begin exFormat:=GL_DEPTH_STENCIL;   compType:=GL_FLOAT_32_UNSIGNED_INT_24_8_REV; end;
    TTextureFormat.D24_S8  : begin exFormat:=GL_DEPTH_STENCIL;   compType:=GL_UNSIGNED_INT_24_8;              end;
    TTextureFormat.D16     : begin exFormat:=GL_DEPTH_COMPONENT; compType:=GL_UNSIGNED_SHORT;                 end;
    TTextureFormat.D24     : begin exFormat:=GL_DEPTH_COMPONENT; compType:=GL_UNSIGNED_BYTE;                  end;
    TTextureFormat.D32     : begin exFormat:=GL_DEPTH_COMPONENT; compType:=GL_INT;                            end;
    TTextureFormat.D32f    : begin exFormat:=GL_DEPTH_COMPONENT; compType:=GL_FLOAT;                          end;
    TTextureFormat.RGBA16f : begin exFormat:=GL_RGBA;            compType:=GL_HALF_FLOAT;                     end;
  else
    exFormat:=GL_RGBA; compType:=GL_UNSIGNED_BYTE;
  end;
  AllocMem(AWidth, AHeight, ADeep, GLTextureFormat[FTargetFormat], exFormat, compType, nil, WithMips, ForcedArray);
end;

procedure TTexture.AllocMem(AWidth, AHeight, ADeep: Integer; WithMips: Boolean; DataFormat: TImageFormat; Data: PByte; ForcedArray: Boolean);
begin
  AllocMem(AWidth, AHeight, ADeep, GLTextureFormat[FTargetFormat], GLImagePixelFormat[DataFormat], GLImageComponentFormat[DataFormat], Data, WithMips, ForcedArray);
end;

procedure TTexture.SetMipImage(X, Y, ImageWidth, ImageHeight, MipLevel, ZSlice: Integer; DataFormat: TImageFormat; Data: PByte);
begin
  if Data = nil then Exit;
  glActiveTexture(GL_TEXTURE31);
  glBindTexture(FGLTexTarget, FHandle);
  if FIsArray then
    glTexSubImage3D(FGLTexTarget, MipLevel, X, Y, ZSlice, ImageWidth, ImageHeight, 1, GLImagePixelFormat[DataFormat], GLImageComponentFormat[DataFormat], Data)
  else
    glTexSubImage2D(FGLTexTarget, MipLevel, X, Y, ImageWidth, ImageHeight, GLImagePixelFormat[DataFormat], GLImageComponentFormat[DataFormat], Data);
end;

procedure TTexture.SetMipImage(DestRect: TRect; MipLevel, ZSlice: Integer; DataFormat: TImageFormat; Data: PByte);
begin
  SetMipImage(DestRect.Left, DestRect.Top, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top, MipLevel, ZSlice, DataFormat, Data);
end;

procedure TTexture.GenerateMips;
var oldH: GLuint;
begin
  glGetIntegerv(GLTexBinding[FIsArray], @oldH);
  glBindTexture(FGLTexTarget, FHandle);
  glGenerateMipmap(FGLTexTarget);
  glBindTexture(FGLTexTarget, oldH);
end;

procedure TTexture.CopyFrom(const DstMipLevel: Integer; const DstPos: TVec2I; const ASrcRes: IctxTexture;
  const SrcMipLevel: Integer; const SrcRect: TRectI);
begin
  Assert(False, 'Not implemented yet');
end;

{ TOGLStates }

function TStates_OGL.GetBlendSrc(RenderTargetIndex: Integer): TBlendFunc;
begin
  if RenderTargetIndex = AllTargets then RenderTargetIndex := 0;
  Result := FBlendSrc[RenderTargetIndex];
end;

function TStates_OGL.GetBlendDest(RenderTargetIndex: Integer): TBlendFunc;
begin
  if RenderTargetIndex = AllTargets then RenderTargetIndex := 0;
  Result := FBlendDest[RenderTargetIndex];
end;

function TStates_OGL.GetBlending(RenderTargetIndex: Integer = AllTargets): Boolean;
begin
  Result := FBlending[max(0, RenderTargetIndex)];
end;

function TStates_OGL.GetColorMask(RenderTargetIndex: Integer = AllTargets): TColorMask;
begin
  Result := FColorMask[Max(0, RenderTargetIndex)];
end;

function TStates_OGL.GetCullMode: TCullingMode;
begin
  Result := FCullMode;
end;

function TStates_OGL.GetDepthFunc: TCompareFunc;
begin
  Result := FDepthFunc;
end;

function TStates_OGL.GetDepthTest: Boolean;
begin
  Result := FDepthTest;
end;

function TStates_OGL.GetDepthWrite: Boolean;
begin
  Result := FDepthWrite;
end;

function TStates_OGL.GetNearFarClamp: Boolean;
begin
  Result := FNearFarClamp;
end;

function TStates_OGL.GetViewport: TRectI;
begin
  Result := FViewport;
end;

function TStates_OGL.GetScissor: TRectI;
begin
  Result := FScissor;
end;

function TStates_OGL.GetScissorTest: Boolean;
begin
  Result := FScissorTest;
end;

function TStates_OGL.GetWireframe: Boolean;
begin
  Result := FWireframe;
end;

procedure TStates_OGL.SetCullMode(const Value: TCullingMode);
begin
  if (FCullMode <> Value) and FContext.Binded then
  begin
    case Value of
      cmNone: glDisable(GL_CULL_FACE);
      cmBack: begin
                glEnable(GL_CULL_FACE);
                glCullFace(GL_BACK);
              end;
      cmFront: begin
                glEnable(GL_CULL_FACE);
                glCullFace(GL_FRONT);
              end;
    end;
    FCullMode := Value;
  end;
end;

procedure TStates_OGL.SetColorMask(RenderTargetIndex: Integer; const Value: TColorMask);
var at: Boolean;
begin
  if RenderTargetIndex = AllTargets then
  begin
    at := True;
    RenderTargetIndex := 0;
  end
  else
    at := False;

  if (Value <> FColorMask[RenderTargetIndex]) and FContext.Binded then
  begin
    FColorMask[RenderTargetIndex] := Value;
    if at then
      glColorMask(cmRed in Value, cmGreen in Value, cmBlue in Value, cmAlpha in Value)
    else
      glColorMaski(RenderTargetIndex, cmRed in Value, cmGreen in Value, cmBlue in Value, cmAlpha in Value);
  end;
end;

procedure TStates_OGL.SetDepthTest(const Value: Boolean);
begin
  if (FDepthTest <> Value) and FContext.Binded then
  begin
      FDepthTest := Value;
      if FDepthTest then
        glEnable(GL_DEPTH_TEST)
      else
        glDisable(GL_DEPTH_TEST);
  end;
end;

procedure TStates_OGL.SetDepthWrite(const Value: Boolean);
begin
  if (FDepthWrite <> Value) and FContext.Binded then
  begin
    FDepthWrite := Value;
    glDepthMask(FDepthWrite);
  end;
end;

procedure TStates_OGL.SetDepthFunc(const Value: TCompareFunc);
begin
  if (FDepthFunc <> Value) and FContext.Binded then
  begin
    FDepthFunc := Value;
    glDepthFunc(GLCompareFunction[FDepthFunc]);
  end;
end;

procedure TStates_OGL.SetNearFarClamp(const Value: Boolean);
begin
  if (FNearFarClamp <> Value) and FContext.Binded Then
  begin
    FNearFarClamp := Value;
    if FNearFarClamp then
      glEnable(GL_DEPTH_CLAMP)
    else
      glDisable(GL_DEPTH_CLAMP);
  end;
end;

procedure TStates_OGL.SetBlending(RenderTargetIndex: Integer; const Value : Boolean);
var at: Boolean;
begin
  if RenderTargetIndex = AllTargets then
  begin
    at := True;
    RenderTargetIndex := 0;
  end
  else
    at := False;

  if (Value <> FBlending[RenderTargetIndex]) and FContext.Binded then
  begin
    FBlending[RenderTargetIndex] := Value;
    if at then
      if Value then
        glEnable(GL_BLEND)
      else
        glDisable(GL_BLEND)
    else
      if Value then
        glEnablei(RenderTargetIndex, GL_BLEND)
      else
        glDisablei(RenderTargetIndex, GL_BLEND);
  end;
end;

procedure TStates_OGL.SetViewport(const Value: TRectI);
begin
  if ((FViewport.Left   <> Value.Left  ) or
      (FViewport.Top    <> Value.Top   ) or
      (FViewport.Right  <> Value.Right ) or
      (FViewport.Bottom <> Value.Bottom)) and FContext.Binded then
  begin
    FViewport := Value;
    glViewport(FViewport.Left, FViewport.Top, FViewport.Right - FViewport.Left, FViewport.Bottom - FViewport.Top);
  end;
end;

procedure TStates_OGL.SetScissor(const Value: TRectI);
begin
  if ((FScissor.Left   <> Value.Left  ) or
      (FScissor.Top    <> Value.Top   ) or
      (FScissor.Right  <> Value.Right ) or
      (FScissor.Bottom <> Value.Bottom)) and FContext.Binded then
  begin
    FScissor := Value;
    glScissor(FScissor.Left, FScissor.Top, FScissor.Right - FScissor.Left, FScissor.Bottom - FScissor.Top);
  end;
end;

procedure TStates_OGL.SetScissorTest(const Value: Boolean);
begin
  if (FScissorTest <> Value) and FContext.Binded then
  begin
    FScissorTest := Value;
    if Value then
      glEnable(GL_SCISSOR_TEST)
    else
      glDisable(GL_SCISSOR_TEST);
  end;
end;

procedure TStates_OGL.SetWireframe(const Value: Boolean);
begin
  if Value <> FWireframe then
  begin
    FWireframe := Value;
    if FWireframe then
      glPolygonMode(GL_FRONT_AND_BACK, GL_LINE)
    else
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
  end;
end;

procedure TStates_OGL.SetBlendFunctions(Src, Dest: TBlendFunc; RenderTargetIndex: Integer);
var at: Boolean;
begin
  if RenderTargetIndex = AllTargets then
  begin
    at := True;
    RenderTargetIndex := 0;
  end
  else
    at := False;

  if ( (Src <> FBlendSrc[RenderTargetIndex]) or (Dest <> FBlendDest[RenderTargetIndex]) ) and FContext.Binded then
  begin
    FBlendSrc[RenderTargetIndex] := Src;
    FBlendDest[RenderTargetIndex] := Dest;
    if at then
      glBlendFunc( GLBlendFunction[Src], GLBlendFunction[Dest] )
    else
      glBlendFunci( RenderTargetIndex, GLBlendFunction[Src], GLBlendFunction[Dest] );
  end;
end;

constructor TStates_OGL.Create(AContext: TContext_OGL);
begin
  FContext := AContext;
end;

procedure TStates_OGL.ReadDefaultStates;
var gb: GLboolean;
    gi: GLint;
    colorwritemask: array [0..3] of GLboolean;
    i: Integer;
begin
  glGetBooleanv(GL_CULL_FACE, @gb);
  glGetIntegerv(GL_CULL_FACE_MODE, @gi);
  if gb then
    if (gi=GL_BACK) then
      FCullMode := cmBack
    else
      FCullMode := cmFront
  else
    FCullMode := cmNone;

  glGetBooleanv(GL_COLOR_WRITEMASK, @colorwritemask[0]);
  for i := 0 to MAX_RENDER_TARGET_COUNT - 1 do
  begin
    FColorMask[i] := [];
    if colorwritemask[0] then
      FColorMask[i] := FColorMask[i] + [cmRed];
    if colorwritemask[1] then
      FColorMask[i] := FColorMask[i] + [cmGreen];
    if colorwritemask[2] then
      FColorMask[i] := FColorMask[i] + [cmBlue];
    if colorwritemask[3] then
      FColorMask[i] := FColorMask[i] + [cmAlpha];
  end;

  glGetBooleanv(GL_DEPTH_TEST, @gb);
  FDepthTest := gb;

  glGetBooleanv(GL_DEPTH_WRITEMASK, @gb);
  FDepthWrite := gb;

  glGetIntegerv(GL_DEPTH_FUNC, @gi);
  case gi of
    GL_NEVER   : FDepthFunc := cfNever;
    GL_LESS    : FDepthFunc := cfLess;
    GL_EQUAL   : FDepthFunc := cfEqual;
    GL_LEQUAL  : FDepthFunc := cfLessEqual;
    GL_GREATER : FDepthFunc := cfGreater;
    GL_GEQUAL  : FDepthFunc := cfGreaterEqual;
    GL_ALWAYS  : FDepthFunc := cfAlways;
  else
    FDepthFunc := cfLess;
  end;

  glGetBooleanv(GL_SCISSOR_TEST, @gb);
  FScissorTest := gb;

  //GL_DEPTH_CLAMP    not defined at glGet??
  FNearFarClamp := False;

  glGetBooleanv(GL_BLEND, @gb);
  for i := 0 to MAX_RENDER_TARGET_COUNT - 1 do
    FBlending[i] := gb;

  //GL_BLEND_FUNC    not defined at glGet??
  for i := 0 to MAX_RENDER_TARGET_COUNT - 1 do
  begin
    FBlendSrc[i] := bfOne;
    FBlendDest[i] := bfZero;
  end;

  glGetIntegerv(GL_VIEWPORT, @FViewport);
  FViewport.Right := FViewport.Right + FViewport.Left;
  FViewport.Bottom := FViewport.Bottom + FViewport.Top;

  glGetIntegerv(GL_SCISSOR_BOX, @FScissor);
  FScissor.Right := FScissor.Right + FScissor.Left;
  FScissor.Bottom := FScissor.Bottom + FScissor.Top;

  glGetIntegerv(GL_POLYGON_MODE, @gi);
  if gi = GL_LINE then
    FWireframe := True
  else
    FWireframe := False;
end;

procedure TStates_OGL.SetStencil(Enabled: Boolean; StencilFunc: TCompareFunc;
  Ref: Integer; Mask: Byte; sFail, dFail, dPass: TStencilAction);
begin
  if FStencil <> Enabled then
  begin
    FStencil := Enabled;
    if Enabled then
      glEnable(GL_STENCIL_TEST)
    else
      glDisable(GL_STENCIL_TEST);
  end;

  if FStencil then
  begin
    glStencilFunc(GLCompareFunction[StencilFunc], Ref, Mask);
    glStencilOp(GLStencilAction[sFail], GLStencilAction[dFail], GLStencilAction[dPass]);
  end;
end;

{ TIndexBuffer }

function TIndexBuffer.Map(usage: TMapingUsage): PByte;
begin
  Assert(FContext.Binded);
  glBindVertexArray(0);
  glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, FHandle);
  Result := glMapBuffer(GL_ELEMENT_ARRAY_BUFFER, GLMapAccess[usage]);
end;

function TIndexBuffer.Unmap: Boolean;
begin
  Assert(FContext.Binded);
  Result := glUnmapBuffer(GL_ELEMENT_ARRAY_BUFFER);
end;

function TIndexBuffer.GetIndexSize: TIndexSize;
begin
  Result := FIndexSize;
end;

function TIndexBuffer.GetPrimType: TPrimitiveType;
begin
  Result := FPrimType;
end;

function TIndexBuffer.IndicesCount: Integer;
begin
  case FIndexSize of
    TIndexSize.Word  : Result := FSize div 2;
    TIndexSize.DWord : Result := FSize div 2;
  else
    Assert(False, 'Not implemented yet');
    Result := 0;
  end;
end;

function TIndexBuffer.PrimCount: Integer;
begin
  Result := CalcPrimCount(IndicesCount, FPrimType);
end;

procedure TIndexBuffer.SetIndexSize(AValue: TIndexSize);
begin
  FIndexSize := AValue;
end;

procedure TIndexBuffer.SetPrimType(AValue: TPrimitiveType);
begin
  FPrimType := AValue;
end;

{ TVertexBufferHandle }

function TVertexBuffer.GetLayout: IDataLayout;
begin
  Result := FLayout;
end;

procedure TVertexBuffer.SetLayout(const Value: IDataLayout);
begin
  FLayout := Value;
end;

function TVertexBuffer.VertexCount: Integer;
begin
  Result := FSize div FLayout.Size;
end;

function TVertexBuffer.Map(usage: TMapingUsage): PByte;
begin
  Assert(FContext.Binded);
  glBindVertexArray(0);
  glBindBuffer(GL_ARRAY_BUFFER, FHandle);
  Result := glMapBuffer(GL_ARRAY_BUFFER, GLMapAccess[usage]);
end;

function TVertexBuffer.Unmap: Boolean;
begin
  Assert(FContext.Binded);
  Result := glUnmapBuffer(GL_ARRAY_BUFFER);
end;

{ TBufferHandle }

function TBufferBase.GetTargetPoolType: TBufferPoolType;
begin
  Result := FTargetPool;
end;

procedure TBufferBase.SetTargetPoolType(Value: TBufferPoolType);
begin
  FTargetPool := Value;
end;

procedure TBufferBase.AllocHandle;
begin
  glGenBuffers(1, @FHandle);
end;

procedure TBufferBase.FreeHandle;
begin
  glDeleteBuffers(1, @FHandle);
  FHandle := 0;
end;

function TBufferBase.Size: Integer;
begin
  Result := FSize;
end;

procedure TBufferBase.AllocMem(ASize: Integer; Data: PByte);
var ActiveObject: GLuint;
begin
  Assert(FContext.Binded);
  FSize := ASize;
  {$IFNDEF NOglNamed}
  if Assigned(glNamedBufferDataEXT) then
  begin
      glNamedBufferDataEXT(FHandle, ASize, Data, GLPoolType[FTargetPool]);
  end
  else
  {$ENDIF}
  begin
      {$IFDEF NOVAO}
      glGetIntegerv(GL_ARRAY_BUFFER_BINDING, @ActiveObject);
      if FHandle <> ActiveObject then glBindBuffer(GL_ARRAY_BUFFER, FHandle);
      glBufferData(GL_ARRAY_BUFFER, ASize, Data, GLPoolType[FTargetPool]);
      if FHandle <> ActiveObject then glBindBuffer(GL_ARRAY_BUFFER, ActiveObject);
      {$ELSE}
      glGetIntegerv(GL_VERTEX_ARRAY_BINDING, @ActiveObject);
      if ActiveObject <> 0 then glBindVertexArray(0);
      glBindBuffer(GL_ARRAY_BUFFER, FHandle);
      glBufferData(GL_ARRAY_BUFFER, ASize, Data, GLPoolType[FTargetPool]);
      glBindBuffer(GL_ARRAY_BUFFER, 0);
      if ActiveObject <> 0 then glBindVertexArray(ActiveObject);
      {$ENDIF}
  end;
end;

procedure TBufferBase.SetSubData(AOffset, ASize: Integer; Data: PByte);
var ActiveObject: GLuint;
begin
  Assert(FContext.Binded);
  {$IFNDEF NOglNamed}
  if Assigned(glNamedBufferSubDataEXT) then
  begin
      glNamedBufferSubDataEXT(FHandle, AOffset, ASize, Data);
  end
  else
  {$ENDIF}
  begin
      {$IFDEF NOVAO}
      glGetIntegerv(GL_ARRAY_BUFFER_BINDING, @ActiveObject);
      if FHandle <> ActiveObject then glBindBuffer(GL_ARRAY_BUFFER, FHandle);
      glBufferSubData(GL_ARRAY_BUFFER, AOffset, ASize, Data);
      if FHandle <> ActiveObject then glBindBuffer(GL_ARRAY_BUFFER, ActiveObject);
      {$ELSE}
      glGetIntegerv(GL_VERTEX_ARRAY_BINDING, @ActiveObject);
      if ActiveObject <> 0 then glBindVertexArray(0);
      glBindBuffer(GL_ARRAY_BUFFER, FHandle);
      glBufferSubData(GL_ARRAY_BUFFER, AOffset, ASize, Data);
      glBindBuffer(GL_ARRAY_BUFFER, 0);
      if ActiveObject <> 0 then glBindVertexArray(ActiveObject);
      {$ENDIF}
  end;
end;

{ TProgram }

procedure TProgram.ClearUniformList;
var Key: String;
    Value: TUniformField_OGL;
begin
  FUniformList.Reset;
  while FUniformList.Next(Key, Value) do Value.Free;
  FUniformList.Clear;
end;

procedure TProgram.ClearAttrList;
var Key: String;
    Value: TAttributeField_OGL;
begin
  FAttrList.Reset;
  while FAttrList.Next(Key, Value) do Value.Free;
  FAttrList.Clear;
end;

procedure TProgram.ClearVAOList;
begin
  FVAOList.Clear;
end;

function TProgram.CreateShader(ACode: AnsiString; AType: TShaderType): Cardinal;
  function GetShaderCompileLog(const Shader: GLuint): string;
  var Log: AnsiString;
      n, tmplen: GLint;
  begin
      glGetShaderiv(Shader, GL_INFO_LOG_LENGTH, @n);
      if n>1 then
      begin
        SetLength(Log, n-1);
        glGetShaderInfoLog(Shader, n, tmplen, PAnsiChar(Log));
        Result := 'Shader compile log: ' + string(Log);
      end;
  end;
var n: Integer;
    CompRes: GLint;
    pstr: PAnsiChar;
    Log: string;
begin
  Result := 0;
  if ACode = '' then Exit;

  case AType of
      stUnknown    : Exit;
      stVertex     : Result := glCreateShader(GL_VERTEX_SHADER);
      stTessControl: Result := glCreateShader(GL_TESS_CONTROL_SHADER);
      stTessEval   : Result := glCreateShader(GL_TESS_EVALUATION_SHADER);
      stGeometry   : Result := glCreateShader(GL_GEOMETRY_SHADER);
      stFragment   : Result := glCreateShader(GL_FRAGMENT_SHADER);
  else
    Assert(False, 'unknown shader type');
  end;
  if Result = 0 then RaiseLast3DError('TProgram.glCreateShader: ');

  n := Length(ACode);
  pstr := @ACode[1];
  glShaderSource(Result, 1, @pstr, @n);
  glCompileShader(Result);
  glGetShaderiv(Result, GL_COMPILE_STATUS, @CompRes);
  if CompRes = GL_FALSE then
  begin
    glDeleteShader(Result);
    Raise3DError('TProgram.glCompileShader: ' + GetShaderCompileLog(Result));
  end;

  Log := GetShaderCompileLog(Result);
  if Log <> '' then LogLn(Log);
end;

procedure TProgram.DetachAllShaders;
var i, cnt: Integer;
    shaders: array of Cardinal;
begin
  if FHandle <> 0 then
  begin
    glGetProgramiv(FHandle, GL_ATTACHED_SHADERS, @cnt);
    if cnt>0 then
    begin
      SetLength(shaders, cnt);
      glGetAttachedShaders(FHandle, cnt, cnt, @shaders[0]);
      for i := 0 to Length(shaders) - 1 do
        glDetachShader(FHandle, shaders[i]);
      for i := 0 to Length(shaders) - 1 do
        glDeleteShader(shaders[i]);
    end;
  end;
end;

procedure TProgram.ReadUniforms;
var uniform : TUniformField_OGL;
    I, N: Integer;
    namebuf_ans: AnsiString;
    namebuf: string;
    writenlen: Integer;
    datasize: integer;
    datatype: Cardinal;

    texIndex: Integer;
begin
  glGetProgramiv(FHandle, GL_ACTIVE_UNIFORM_MAX_LENGTH, @N);
  texIndex := 0;
  if N > 0 then
  begin
    glUseProgram(FHandle);

    SetLength(namebuf_ans, N - 1);
    glGetProgramiv(FHandle, GL_ACTIVE_UNIFORMS, @N);
    for I := 0 to N - 1 do
    begin
      uniform := TUniformField_OGL.Create;

      FillChar(namebuf_ans[1], Length(namebuf_ans), 0);
      glGetActiveUniform(FHandle, I, Length(namebuf_ans)+1, writenlen, datasize, datatype, @namebuf_ans[1]);
      namebuf := string(PAnsiChar(namebuf_ans));
      if Pos('gl_', namebuf) <> 1 then
      begin
        uniform.Name := ReduceName(namebuf);
        uniform.OGLType := datatype;
        VectorInfoOfDataType(datatype, uniform.DataClass, uniform.ElementType, uniform.ElementsCount);
        uniform.ItemsCount := datasize div (uniform.ElementsCount * GLCompTypeSize[uniform.ElementType]);
        SetLength(uniform.FData, datasize * SizeOfDataType(datatype));
        uniform.Data := @uniform.FData[0];
        uniform.DataSize := Length(uniform.FData);
        FillChar(uniform.FData[0], Length(uniform.FData), 0);
        uniform.ID := glGetUniformLocation(FHandle, @namebuf_ans[1]);

        case uniform.ElementType of
            ctBool  : glGetUniformfv (FHandle, uniform.ID, @uniform.Data[0]);
            ctByte  : glGetUniformiv (FHandle, uniform.ID, @uniform.Data[0]);
            ctUByte : glGetUniformuiv(FHandle, uniform.ID, @uniform.Data[0]);
            ctShort : glGetUniformiv (FHandle, uniform.ID, @uniform.Data[0]);
            ctUShort: glGetUniformuiv(FHandle, uniform.ID, @uniform.Data[0]);
            ctInt   : glGetUniformiv (FHandle, uniform.ID, @uniform.Data[0]);
            ctUInt  : glGetUniformuiv(FHandle, uniform.ID, @uniform.Data[0]);
            ctFloat : glGetUniformfv (FHandle, uniform.ID, @uniform.Data[0]);
            ctDouble: glGetUniformdv (FHandle, uniform.ID, @uniform.Data[0]);
        end;
        Begin
//          AllocConsole();
//          WriteLn(uniform.Name);
        end;
        FUniformList.Add(uniform.Name, uniform);
//        If FUniformList.Contains(uniform.Name) Then


        if uniform.DataClass = dcSampler then
        begin
          PInteger(@uniform.Data[0])^ := texIndex;
          glUniform1i(uniform.ID, texIndex);
          Inc(texIndex);
        end;
      end;
    end;

    glUseProgram(0);
  end;
end;

procedure TProgram.ReadAttributes;
var I, N: Integer;
    namebuf_ans: AnsiString;
    namebuf: string;
    attr : TAttributeField_OGL;
    writenlen: Integer;
    datasize: integer;
    datatype: Cardinal;
begin
  glGetProgramiv(FHandle, GL_ACTIVE_ATTRIBUTE_MAX_LENGTH, @N);
  if N > 0 then
  begin
    SetLength(namebuf_ans, N - 1);
    glGetProgramiv(FHandle, GL_ACTIVE_ATTRIBUTES, @N);
    for I := 0 to N - 1 do
    begin
      attr := TAttributeField_OGL.Create;

      FillChar(namebuf_ans[1], Length(namebuf_ans), 0);
      glGetActiveAttrib(FHandle, I, Length(namebuf_ans)+1, writenlen, datasize, datatype, @namebuf_ans[1]);
      namebuf := string(PAnsiChar(namebuf_ans));
      attr.Name := ReduceName(namebuf);
      attr.DataType := datatype;
      VectorInfoOfDataType(datatype, attr.DataClass, attr.ElementType, attr.ElementsCount);
      attr.ID := glGetAttribLocation(FHandle, @namebuf_ans[1]);

      FAttrList.Add(namebuf, attr);
    end;
  end;
end;

procedure TProgram.BindVAO(const AKey: TVAOKey);
var VAOInfo: TVAOInfo;
  function UpdateVAOBinding(Const AVAO: Cardinal): TVAOInfo;
  begin
    Result.VAO := AVAO;
    If Assigned(AKey.InstanceVertex) Then
    Begin
      glBindBuffer(GL_ARRAY_BUFFER, (AKey.InstanceVertex as IHandle).Handle);
      Result.Instance := AKey.InstanceVertex.Layout;
      SetAttributes(Result.Instance, AKey.InstanceStepRate);
    End
    Else
      Result.Instance := nil;

    If Assigned(AKey.ModelVertex) Then
    Begin
      glBindBuffer(GL_ARRAY_BUFFER, (AKey.ModelVertex as IHandle).Handle);
      Result.Model := AKey.ModelVertex.Layout;
      SetAttributes(Result.Model, 0);
    End
    Else
      Result.Model := nil;

    If Assigned(AKey.ModelIndex) Then
    Begin
      glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, (AKey.ModelIndex as IHandle).Handle);
      Result.HasIndices := True;
    End
    Else
      Result.HasIndices := False;
  end;
  function VAOInvalid(const VAOInfo: TVAOInfo): Boolean;
  begin
    if assigned(AKey.InstanceVertex) then
    begin
       Result := (VAOInfo.Instance <> AKey.InstanceVertex.Layout);
       if Result then Exit;
    end
    else
    begin
       Result := assigned(VAOInfo.Instance);
       if Result then Exit;
    end;

    if assigned(AKey.ModelVertex) then
    begin
       Result := (VAOInfo.Model <> AKey.ModelVertex.Layout);
       if Result then Exit;
    end
    else
    begin
       Result := assigned(VAOInfo.Model);
       if Result then Exit;
    end;

    if assigned(AKey.ModelIndex) then
    begin
      Result := Not VAOInfo.HasIndices;
      if Result then Exit;
    end
    else
      Result := VAOInfo.HasIndices;
  end;
begin
  {$IFDEF NOVAO}
  UpdateVAOBinding(0);
  {$ELSE}
  If not FVAOList.TryGetValue(AKey, VAOInfo) Then
  Begin
      glGenVertexArrays(1, @VAOInfo.VAO);
      glBindVertexArray(VAOInfo.VAO);
      VAOInfo := UpdateVAOBinding(VAOInfo.VAO);
      VAOInfo.BindTime := GetTickCount;
      FVAOList.Add(AKey, VAOInfo);
  End
  Else
  Begin
      glBindVertexArray(VAOInfo.VAO);
      If VAOInvalid(VAOInfo) Then
        VAOInfo := UpdateVAOBinding(VAOInfo.VAO);
      VAOInfo.BindTime := GetTickCount;
      FVAOList.Item[AKey] := VAOInfo;
  End;
  {$ENDIF}
end;

function TProgram.GetAttributeField(const name: string): TAttributeField_OGL;
begin
  if not FAttrList.TryGetValue(name, Result) then
    if not FAttrList.TryGetValue('in_'+name, Result) then
       if not FAttrList.TryGetValue('in_'+name+'0', Result) then
          Result := nil;
end;

procedure TProgram.SetAttribute(AttrField: TAttributeField_OGL;
  componentsCount, componentsType: integer; stride, offset: integer;
  divisor: Integer; normalized: Boolean);
begin
  if AttrField = nil then Exit;
  glEnableVertexAttribArray(AttrField.ID);
  glVertexAttribPointer(AttrField.ID, componentsCount, componentsType, normalized, stride, Pointer(offset));
  glVertexAttribDivisor(AttrField.ID, divisor);
end;

procedure TProgram.SetAttributes(const ALayout: IDataLayout; divisor: Integer);
var I: Integer;
begin
  if ALayout = nil then Exit;
  for I := 0 to ALayout.Count - 1 do
    SetAttribute(GetAttributeField(ALayout[I].Name),
                 ALayout[I].CompCount,
                 GLComponentType[ALayout[I].CompType],
                 ALayout.Size,
                 ALayout[I].Offset,
                 divisor,
                 ALayout[I].DoNorm);
end;

function TProgram.GetProgramInfoLog: string;
var n, dummy: Integer;
    astr: AnsiString;
begin
  Result := '';
  glGetProgramiv(Handle, GL_INFO_LOG_LENGTH, @n);
  if n > 1 then
  begin
    SetLength(astr, n-1);
    glGetProgramInfoLog(Handle, n, dummy, PAnsiChar(astr));
    Result := string(astr);
  end;
end;

procedure TProgram.ValidateProgram;
var param: GLuint;
begin
  if not FValidated then
  begin
    glValidateProgram(FHandle);
    glGetProgramiv(FHandle, GL_VALIDATE_STATUS, @param);
    if param<>1 then Raise3DError('Program validation failed: '+GetProgramInfoLog);
    FValidated := True;
  end;
end;

procedure TProgram.CleanUpUselessVAO;
var CurrTime, DTime: Cardinal;
    Key: TVAOKey;
    Value: TVAOInfo;
begin
  CurrTime := GetTickCount;
  FVAOList.Reset;
  while FVAOList.Next(Key, Value) do
  begin
    DTime := CurrTime - Value.BindTime;
    if DTime > 3000 then
    begin
      glDeleteVertexArrays(1, @Value.VAO);
      FVAOList.Delete(Key);
    end;
  end;
end;

procedure TProgram.AllocHandle;
begin
  FHandle := glCreateProgram();
end;

procedure TProgram.FreeHandle;
begin
  DetachAllShaders;
  glDeleteProgram(FHandle);
  FHandle := 0;
end;

procedure TProgram.Select(const APatchSize: Integer = 0);
begin
  glUseProgram(Handle);
  if APatchSize > 0 then
    glPatchParameteri(GL_PATCH_VERTICES, APatchSize);
end;

procedure TProgram.Load(const AProgram: string; FromResource: Boolean);
var stream: TStream;
    s: AnsiString;
    obj: ISuperObject;
    GLShader: Cardinal;
    param: integer;
    st: TShaderType;
begin
  stream := nil;
  try
    if FromResource then
    begin
      stream := TResourceStream.Create(HInstance, AProgram, RT_RCDATA);
    end
    else
    begin
      if FileExists(AProgram) then
        stream := TFileStream.Create(AProgram, fmOpenRead)
      else
      begin
        LogLn('File not found: ' + AProgram);
        Exit;
      end;
    end;
    SetLength(s, stream.Size);
    stream.Read(s[1], stream.Size);
  finally
    FreeAndNil(stream);
  end;

  obj := SO(s);
  DetachAllShaders;
  ClearUniformList;
  ClearAttrList;
  ClearVAOList;

  for st := Low(TShaderType) to High(TShaderType) do
  begin
    GLShader := CreateShader(AnsiString(obj.S[ShaderType_Name[st]]), st);
    if GLShader <> 0 then
      glAttachShader(FHandle, GLShader);
  end;

  glLinkProgram(FHandle);
  glGetProgramiv(FHandle, GL_LINK_STATUS, @param);
  if param=1 then
  begin
    ReadUniforms;
    ReadAttributes;
  end
  else
  begin
    Raise3DError('Program linking failed: '+GetProgramInfoLog);
  end;
end;

procedure TProgram.SetAttributes(const AModel, AInstances: IctxVetexBuffer;
  const AModelIndices: IctxIndexBuffer; InstanceStepRate: Integer = 1);
begin;
  FSelectedVAOKey.ModelVertex := AModel;
  FSelectedVAOKey.ModelIndex := AModelIndices;
  FSelectedVAOKey.InstanceVertex := AInstances;
  FSelectedVAOKey.InstanceStepRate :=InstanceStepRate;
  FSelectedVAOBinded := False;
end;

function TProgram.GetUniformField(const Name: string): TUniformField;
var value: TUniformField_OGL;
begin
  if FUniformList.TryGetValue(name, value) then
    Result := value
  else
    Result := nil;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const Value: integer);
begin
  if Field = nil then Exit;
  if Field.DataClass = dcSampler then Exit;
  If value <> PInteger(Field.Data)^ then
  begin
    PInteger(Field.Data)^ := value;
    glUniform1i(TUniformField_OGL(Field).ID, value);
  end;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const Value: single);
begin
  if Field = nil then Exit;
  if Field.DataClass = dcSampler then Exit;
  If PSingle(Field.Data)^ <> value then
  begin
    PSingle(Field.Data)^ := value;
    glUniform1f(TUniformField_OGL(Field).ID, value);
  end;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec2);
begin
  if Field = nil then Exit;
  if Field.DataClass = dcSampler then Exit;
  If not (PVec2(Field.Data)^ = v) Then
  begin
    PVec2(Field.Data)^ := v;
    glUniform2fv(TUniformField_OGL(Field).ID, 1, @v);
  end;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec3);
begin
  if Field = nil then Exit;
  if Field.DataClass = dcSampler then Exit;
  If not (PVec3(Field.Data)^ = v) Then
  begin
    PVec3(Field.Data)^ := v;
    glUniform3fv(TUniformField_OGL(Field).ID, 1, @v);
  end;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec4);
begin
  if Field = nil then Exit;
  if Field.DataClass = dcSampler then Exit;
  If not (PVec4(Field.Data)^ = v) Then
  begin
    PVec4(Field.Data)^ := v;
    glUniform4fv(TUniformField_OGL(Field).ID, 1, @v);
  end;
end;

procedure TProgram.SetUniform(const Field: TUniformField;
  const values: TSingleArr);
begin
  if Field = nil then Exit;
  if Field.DataClass = dcSampler then Exit;
  if Length(values) > 0 then
    if not CompareMem(@values[0], Field.Data, Length(values) * SizeOf(single)) then
    begin
      Move(values[0], Field.Data^, Length(values) * SizeOf(single));
      glUniform1fv(TUniformField_OGL(Field).ID, Length(values), @values[0]);
    end;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const v: TVec4arr);
begin
  if Field = nil then Exit;
  if Field.DataClass = dcSampler then Exit;
  if Length(v) > 0 then
    if not CompareMem(@v[0], Field.Data, Length(v) * SizeOf(TVec4)) then
    begin
      Move(v[0], Field.Data^, Length(v) * SizeOf(TVec4));
      glUniform4fv(TUniformField_OGL(Field).ID, Length(v), @v[0]);
    end;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const m: TMat4);
begin
  if Field = nil then Exit;
  if Field.DataClass = dcSampler then Exit;
  if not CompareMem(@m, Field.Data, SizeOf(m)) then
  begin
    Move(m, Field.Data^, SizeOf(m));
    glUniformMatrix4fv(TUniformField_OGL(Field).ID, 1, false, @m);
  end;
end;

procedure TProgram.SetUniform(const Field: TUniformField; const tex: IctxTexture; const Sampler: TSamplerInfo);
var TexH: GLuint;
    TexTarget: GLuint;
    MipFilter: TTextureFilter;
begin
  if Field = nil then Exit;
  if Field.DataClass <> dcSampler then Exit;
  if tex = nil then Exit;

  TexH := (tex as IHandle).Handle;
  if tex.Deep > 1 then
    TexTarget := GL_TEXTURE_2D_ARRAY
  else
    TexTarget := GL_TEXTURE_2D;

  glActiveTexture(GL_TEXTURE0 + PInteger(Field.Data)^);
  glBindTexture(TexTarget, TexH);

  if tex.MipsCount > 1 then
    MipFilter := Sampler.MipFilter
  else
    MipFilter := tfNone;

  glUniform1i(TUniformField_OGL(Field).ID, PInteger(Field.Data)^);
  glTexParameteri(TexTarget, GL_TEXTURE_MAG_FILTER, GLMagTextureFilter[Sampler.MagFilter]);
  glTexParameteri(TexTarget, GL_TEXTURE_MIN_FILTER, GLMinTextureFilter[MipFilter, Sampler.MinFilter]);
  glTexParameteri(TexTarget, GL_TEXTURE_WRAP_S, GLWrap[Sampler.Wrap_X]);
  glTexParameteri(TexTarget, GL_TEXTURE_WRAP_T, GLWrap[Sampler.Wrap_Y]);
  glTexParameterf(TexTarget, GL_TEXTURE_MAX_ANISOTROPY_EXT, Max(Sampler.Anisotropy, 1.0));
  glTexParameterfv(TexTarget, GL_TEXTURE_BORDER_COLOR, @Sampler.Border);
end;

procedure TProgram.Draw(PrimTopology: TPrimitiveType; CullMode: TCullingMode;
  IndexedGeometry: Boolean; InstanceCount: Integer; Start: integer;
  Count: integer; BaseVertex: integer; BaseInstance: Integer);
var StrideSize: Integer;
    IndexSize: TIndexSize;
begin
  if not FSelectedVAOBinded then
  begin
    BindVAO(FSelectedVAOKey);
    FSelectedVAOBinded := True;
  end;

  ValidateProgram;
  FContext.States.CullMode := CullMode;

  if IndexedGeometry then
  begin
    IndexSize := FSelectedVAOKey.ModelIndex.IndexSize;
    if IndexSize = TIndexSize.DWord then
      StrideSize := 4
    else
      StrideSize := 2;
    Start := Start * StrideSize;
    Count := Count;
    if InstanceCount = 0 then
    begin
      if BaseVertex > 0 then
          glDrawElementsBaseVertex(GLPrimitiveType[PrimTopology], Count, GLIndexSize[IndexSize], Pointer(Start), BaseVertex)
      else
          glDrawElements(GLPrimitiveType[PrimTopology], Count, GLIndexSize[IndexSize], Pointer(Start));
    end
    else
    begin
      if BaseVertex > 0 then
      begin
          if BaseInstance > 0 then
              glDrawElementsInstancedBaseVertexBaseInstance(GLPrimitiveType[PrimTopology], Count, GLIndexSize[IndexSize], Pointer(Start), InstanceCount, BaseVertex, BaseInstance)
          else
              glDrawElementsInstancedBaseVertex(GLPrimitiveType[PrimTopology], Count, GLIndexSize[IndexSize], Pointer(Start), InstanceCount, BaseVertex);
      end
      else
      begin
          if BaseInstance > 0 then
              glDrawElementsInstancedBaseInstance(GLPrimitiveType[PrimTopology], Count, GLIndexSize[IndexSize], Pointer(Start), InstanceCount, BaseInstance)
          else
              glDrawElementsInstanced(GLPrimitiveType[PrimTopology], Count, GLIndexSize[IndexSize], Pointer(Start), InstanceCount);
      end;
    end;
  end
  else
  begin
    //StrideSize := FSelectedVAOKey.ModelVertex.Layout.Size;
    Start := Start;
    Count := Count;
    if InstanceCount = 0 then
      glDrawArrays(GLPrimitiveType[PrimTopology], Start, Count)
    else
      if BaseInstance > 0 then
        glDrawArraysInstancedBaseInstance(GLPrimitiveType[PrimTopology], Start, Count, InstanceCount, BaseInstance)
      else
        glDrawArraysInstanced(GLPrimitiveType[PrimTopology], Start, Count, InstanceCount);
  end;
end;

constructor TProgram.Create(AContext: TContext_OGL);
begin
  inherited Create(AContext);
  FUniformList := TUniformHash.Create;
  FAttrList := TAttributeHash.Create;

  FVAOList := TVaoHash.Create;
end;

destructor TProgram.Destroy;
begin
  ClearUniformList;
  ClearAttrList;
  ClearVAOList;
  DetachAllShaders;
  inherited Destroy;
end;

{ THandle }

function THandle.Handle: Cardinal;
begin
  Result := FHandle;
end;

function THandle.QueryInterface({$IfDef FPC_HAS_CONSTREF}constref{$Else}const{$EndIf} iid: tguid; out obj): longint; stdcall;
begin
  if GetInterface(IID, obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function THandle._AddRef: longint; stdcall;
begin
  Result := InterLockedIncrement(FRefCount);
end;

function THandle._Release: longint; stdcall;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    if FContext.Binded then
      Free
    else
      FContext.AddHandlesForCleanup(Self);
end;

procedure THandle.AfterConstruction;
begin
  inherited AfterConstruction;
  InterLockedDecrement(FRefCount);
end;

constructor THandle.Create(AContext: TContext_OGL);
begin
  Assert(Assigned(AContext), 'Context can''t to be nil');
  Assert(AContext.Binded, 'Context can''t to be unbinded');
  FContext := AContext;
  FContext.AddHandle(Self);
  AllocHandle;
end;

class function THandle.NewInstance: TObject;
begin
  Result:=inherited NewInstance;
  THandle(Result).FRefCount := 1;
end;

destructor THandle.Destroy;
begin
  if Assigned(FContext) then
  begin
    FContext.RemoveHandle(Self);
    FreeHandle;
  end;
  inherited Destroy;
end;

{ TContext_OGL }

procedure TContext_OGL.AddHandle(const HandleObject: TObject);
begin
  FHandles.Add(HandleObject, True);
  if HandleObject is TProgram then
    FPrograms.Add(HandleObject);
end;

function TContext_OGL.GetActiveProgram: IctxProgram;
begin
  Result := FActiveProgram;
end;

procedure TContext_OGL.RemoveHandle(const HandleObject: TObject);
begin
  FHandles.Delete(HandleObject);
  if HandleObject is TProgram then
    FPrograms.Remove(HandleObject);
end;

procedure TContext_OGL.AddHandlesForCleanup(const HandleObject: TObject);
begin
  FDeletedHandles.Add(HandleObject);
end;

procedure TContext_OGL.CleanUpHandles;
var i: Integer;
begin
  for i := 0 to FDeletedHandles.Count - 1 do
    TObject(FDeletedHandles.Items[i]).Free;
  FDeletedHandles.Clear;
end;

procedure TContext_OGL.SetActiveProgram(AValue: IctxProgram);
begin
  if FActiveProgram = AValue then Exit;
  FActiveProgram := AValue;
end;

function TContext_OGL.CreateVertexBuffer: IctxVetexBuffer;
begin
  Result := TVertexBuffer.Create(Self);
end;

function TContext_OGL.CreateIndexBuffer: IctxIndexBuffer;
begin
  Result := TIndexBuffer.Create(Self);
end;

function TContext_OGL.CreateProgram: IctxProgram;
begin
  Result := TProgram.Create(Self);
end;

function TContext_OGL.CreateTexture: IctxTexture;
begin
  Result := TTexture.Create(Self);
end;

function TContext_OGL.CreateUAV(const AElementsCount, AStrideSize: Cardinal; const Appendable: Boolean; const AInitialData: Pointer): IctxUAV;
begin
  Result := nil;
  Assert(False, 'qwe not implemented');
end;

function TContext_OGL.CreateFrameBuffer: IctxFrameBuffer;
begin
  Result := TFrameBuffer.Create(Self);
end;

function TContext_OGL.States: IRenderStates;
begin
  Result := FStatesIntf;
end;

function TContext_OGL.Binded: Boolean;
begin
  Result := FBindCount > 0;
end;

function TContext_OGL.Bind: Boolean;
begin
  if FBindCount = 0 then
  begin
    wglMakeCurrent(FDC, FRC);
    CleanUpHandles;
    if not DEFAULT_BackBuffer then
      glDrawBuffer(GL_FRONT);
  end;
  Inc(FBindCount);
  Result := True;
end;

function TContext_OGL.Unbind: Boolean;
begin
  Dec(FBindCount);
  if FBindCount = 0 then
  begin
    glUseProgram(0);
    wglMakeCurrent(0, 0);
    FActiveProgram := nil;
  end;
  Result := True;
end;
procedure TContext_OGL.Clear(const color  : TVec4;      doColor  : Boolean = True;
                                   depth  : Single = 1; doDepth  : Boolean = False;
                                   stencil: Byte   = 0; doStencil: Boolean = False);
var GLClearBits: GLuint;
begin
  GLClearBits := 0;
  if doColor then
  begin
    glClearColor(color.x, color.y, color.z, color.w);
    GLClearBits := GL_COLOR_BUFFER_BIT;
  end;

  if doDepth then
  begin
    glClearDepthf(depth);
    GLClearBits := GLClearBits or GL_DEPTH_BUFFER_BIT;
  end;

  if doStencil then
  begin
    glClearStencil(stencil);
    GLClearBits := GLClearBits or GL_STENCIL_BUFFER_BIT;
  end;
  glClear(GLClearBits);
end;

procedure TContext_OGL.Flush;
begin
  glFlush();
end;

procedure TContext_OGL.Present;
begin
  glBindFramebuffer(GL_DRAW_FRAMEBUFFER, 0);
  if DEFAULT_BackBuffer then
    SwapBuffers(FDC)
  else
    glFlush;
end;

constructor TContext_OGL.Create(const Wnd: TWindow);
begin
  FWnd := Wnd;
  FDC := GetDC(FWnd);
  if DEFAULT_BackBuffer then
    FRC := CreateRenderingContext(FDC, [opDoubleBuffered], 32, 0, 0, 0, 0, 0)
  else
    FRC := CreateRenderingContext(FDC, [], 32, 0, 0, 0, 0, 0);
  ActivateRenderingContext(FDC, FRC);
  DeactivateRenderingContext;

  FHandles := TObjListHash.Create;
  FPrograms := TList.Create;
  FDeletedHandles := TList.Create;

  FStates := TStates_OGL.Create(Self);
  FStatesIntf := TStates_OGL(FStates);
end;

destructor TContext_OGL.Destroy;
begin
  FreeAndNil(FDeletedHandles);
  FreeAndNil(FPrograms);
  FStatesIntf := nil;
  FreeAndNil(FStates);
  DestroyRenderingContext(FRC);
  ReleaseDC(FWnd, FDC);
  inherited Destroy;
end;

end.

