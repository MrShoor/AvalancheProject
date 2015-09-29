unit avTypes;

{$ifdef fpc}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$endif}

interface

uses
  Classes, SysUtils, mutils;

const
  EM_NONE              = $0000;

  EM_UPS               = $0010;

  EM_MOUSEDOWN         = $0020;
  EM_MOUSEUP           = $0021;
  EM_MOUSEDBLCLICK     = $0022;
  EM_MOUSEMOVE         = $0023;
  EM_MOUSEWHEEL        = $0024;

  EM_KEYDOWN           = $0030;
  EM_KEYUP             = $0031;
  EM_CHAR              = $0032;

  EM_WINDOWDESTROY     = $0100;
  EM_3D_AFTER_INIT     = $0101;
  EM_3D_BEFORE_FREE    = $0102;
  EM_3D_AFTER_FREE     = $0103;

  EM_EVENTS_START_CONTROLS = $0200;
  EM_EVENTS_END_CONTROLS   = $02FF;

  SETSIZE_IN_BYTES = 4;

const
  AllTargets = -1;

type
  TavMessage = packed record
    msg    : Cardinal;
    param  : Integer;
    sender : TObject;
    result : boolean;      //setup at true if message is processing (broadcasting will stop)
  end;

  TShifts = set of (sShift, sAlt, sCtrl, sLeft, sRight, sMiddle, sDouble, sXMButton1, sXMButton2, sLast = SETSIZE_IN_BYTES * 8 - 1);
  TavMouseBtnMessage = packed record
    msg       : Cardinal;
    button    : integer; // 0=nothing 1=left 2=right 3=middle 4=xbutton1 5=xbutton2
    xPos      : integer;
    yPos      : integer;
    wheelShift: integer;
    shifts    : TShifts;
    result    : boolean; //setup at true if message is processing (broadcasting will be stopped)
  end;
  TavMouseMessage     = TavMouseBtnMessage;
  TavMouseDownMessage = TavMouseBtnMessage;
  TavMouseUpMessage   = TavMouseBtnMessage;
  TavMouseDblClick    = TavMouseBtnMessage;

  TavKeyMessage = packed record
    msg      : Cardinal;
    shifts   : TShifts;
    Sys      : boolean;
    Dead     : boolean;
    Result   : boolean;
    case Cardinal of
      0: (Key : Cardinal);
      1: (Char: WideChar);
  end;
  TavKeyDownMessage = TavKeyMessage;
  TavKeyUpMessage   = TavKeyMessage;
  TavCharMessage    = TavKeyMessage;

type
  ECreateContextFailed = Class(Exception);

  TByteArr = array of Byte;
  TIntArr = array of Integer;
  TWordArr = array of Word;

  T3DAPI = (apiOGL, apiDX11);
const
  API_Prefix : array [T3DAPI] of string = ('OGL_', 'DX_');
  API_Suffix : array [T3DAPI] of string = ('.glsl', '.hlsl');

type
  TShaderType = (stUnknown, stVertex, stTessControl, stTessEval, stGeometry, stFragment);

const
  ShaderType_Name : array [TShaderType] of string = ('Unknown', 'Vertex', 'TessControl', 'TessEval', 'Geometry', 'Fragment');
  ShaderType_FourCC : array [TShaderType] of Cardinal = (0, $54524556, $4E4F4354, $4C564554, $4D4F4547, $47415246);

type
  TCullingMode = (cmNone, cmBack, cmFront);
  TMapingUsage = (muWriteOnly, muReadOnly, muReadWrite);

  TPrimitiveType = (ptPoints,
                    ptLines, ptLineStrip,
                    ptTriangles, ptTriangleStrip,
                    ptLines_Adj, ptLineStrip_Adj,
                    ptTriangles_Adj, ptTriangleStrip_Adj);
  {$SCOPEDENUMS ON}
  TIndexSize = (Word, DWord);
  TTextureFormat = (RGBA, RGBA16, RGBA16f, RGBA32, RGBA32f, RGB, RGB16, RGB16f, RGB32, RGB32f, RG, RG16, RG16f, RG32, RG32f, R, R16, R16f, R32,
                    R32f, DXT1, DXT3, DXT5, D24_S8, D32f_S8, D16, D24, D32, D32f);
  TImageFormat = (Unknown, Gray8, R3G3B2, R8G8, R5G6B5, A1R5G5B5, A4R4G4B4, R8G8B8, A8R8G8B8, A8B8G8R8, R16F, R16G16F, R16G16B16F, A16R16G16B16F, B16G16R16F,
                  A16B16G16R16F, R32F, R32G32F, R32G32B32F, A32R32G32B32F, R32G32B32A32F, DXT1, DXT3, DXT5, D24_S8, D32f_S8, D16, D24, D32, D32f,
                  R16, R16G16, R16G16B16, A16R16G16B16, B16G16R16, A16B16G16R16, R32, R32G32, R32G32B32, A32R32G32B32, A32B32G32R32);
  {$SCOPEDENUMS OFF}
  TTextureFilter = (tfNone, tfNearest, tfLinear);
  TTextureWrap   = (twRepeat, twMirror, twClamp, twClampToEdge);
  TBlendFunc     = (bfZero, bfOne, bfSrcAlpha, bfInvSrcAlpha, bfDstAlpha, bfInfDstAlpha, bfSrcColor, bfDstColor);
  TCompareFunc   = (cfNever,
                    cfLess,
                    cfEqual,
                    cfNotEqual,
                    cfLessEqual,
                    cfGreater,
                    cfGreaterEqual,
                    cfAlways);
  TStencilAction = (saKeep,
                    saSet,
                    saZero,
                    saInvert,
                    saInc,
                    saDec,
                    saIncWrap,
                    saDecWrap);
  TColorMaskComponent = (cmRed, cmGreen, cmBlue, cmAlpha);
  TColorMask = set of TColorMaskComponent;
Const
  AllChanells : TColorMask = [cmRed, cmGreen, cmBlue, cmAlpha];

Type
  TSamplerInfo = record
    MinFilter  : TTextureFilter;
    MagFilter  : TTextureFilter;
    MipFilter  : TTextureFilter;
    Anisotropy : Integer;
    Wrap_X     : TTextureWrap;
    Wrap_Y     : TTextureWrap;
    Border     : TVec4;
  end;

  TComponentType = (ctBool, ctByte, ctUByte, ctShort, ctUShort, ctInt, ctUInt, ctFloat, ctDouble);
  TFieldInfo = packed record
      Name     : string;
      Offset   : Integer;
      Size     : Integer;
      CompCount: Integer;
      CompType : TComponentType;
      DoNorm   : Boolean;
  end;
  TFieldInfoArr = array of TFieldInfo;

  IDataLayout = interface //must be immutable
  ['{6D87F9A5-A459-4A05-AFC9-7B8B6EE89A4E}']
  //*properties implementation
    function GetItem(index: Integer): TFieldInfo;
    function GetSize: Integer;
  //*end of properties implementation
    function Count: Integer;

    property Item[index: Integer]: TFieldInfo read GetItem; default;
    property Size: Integer read GetSize;
  end;

  TPointerData = packed record
    data: Pointer;
    size: Integer;
  end;

  IVerticesData = interface
  ['{49A05144-F599-46DF-8A5A-5E12A08BA44C}']
    function VerticesCount: Integer;
    function Layout: IDataLayout;
    function Data: TPointerData;
  end;

  IIndicesData = interface
  ['{4D744FC8-513E-422F-B169-F7FE0988DA9C}']
    function IndicesCount: Integer;
    function IndexSize: Integer;
    function PrimCount: Integer;
    function PrimType : TPrimitiveType;
    function Data: TPointerData;
  end;

  { ITextureMip }

  ITextureMip = interface
    function Width      : Integer;
    function Height     : Integer;
    function Data       : PByte;
    function PixelFormat: TImageFormat;
    function Replicate  : ITextureMip;
    function Pixel(const x,y: Integer): PByte;
  end;

  { ITextureData }

  ITextureData = interface
  ['{3643BDFB-36C1-4A64-BEA6-04108FDB8F1E}']
    function Width: Integer;
    function Height: Integer;
    function MipsCount: Integer;

    function Format: TImageFormat;
    function ItemCount: Integer;
    function MipCount(const Index: Integer): Integer;
    function MipData(const Index, MipLevel: Integer): ITextureMip;

    procedure Merge(const TexData: array of ITextureData);
  end;

const
    IndexSizeInBytes: array [TIndexSize] of Integer = (
      { TIndexSize.Word } 2,
      { TIndexSize.DWord } 4
    );
    ImagePixelSize : array [TImageFormat] of Integer = (
    { Unknown       } 0,
    { Gray8         } 1,
    { R3G3B2        } 1,
    { R8G8          } 2,
    { R5G6B5        } 2,
    { A1R5G5B5      } 2,
    { A4R4G4B4      } 2,
    { R8G8B8        } 3,
    { A8R8G8B8      } 4,
    { A8B8G8R8      } 4,
    { R16F          } 2,
    { R16G16F       } 4,
    { R16G16B16F    } 6,
    { A16R16G16B16F } 8,
    { B16G16R16F    } 6,
    { A16B16G16R16  } 8,
    { R32F          } 4,
    { R32G32F       } 8,
    { R32G32B32F    } 12,
    { A32R32G32B32F } 16,
    { R32G32B32A32F } 16,
    { DXT1          } 8,
    { DXT3          } 16,
    { DXT5          } 16,
    { D24_S8        } 4,
    { D32f_S8       } 8,
    { D16           } 2,
    { D24           } 4,
    { D32           } 4,
    { D32f          } 4,
    { R16           } 2,
    { R16G16        } 4,
    { R16G16B16     } 6,
    { A16R16G16B16  } 8,
    { B16G16R16     } 6,
    { A16B16G16R16  } 8,
    { R32           } 4,
    { R32G32        } 8,
    { R32G32B32     } 12,
    { A32R32G32B32  } 16,
    { A32B32G32R32  } 16);
    TexturePixelSize : array [TTextureFormat] of Integer = (
    { RGBA    } 4,
    { RGBA16  } 8,
    { RGBA16f } 8,
    { RGBA32  } 16,
    { RGBA32f } 16,
    { RGB     } 3,
    { RGB16   } 6,
    { RGB16f  } 6,
    { RGB32   } 12,
    { RGB32f  } 12,
    { RG      } 2,
    { RG16    } 4,
    { RG16f   } 4,
    { RG32    } 8,
    { RG32f   } 8,
    { R       } 1,
    { R16F    } 2,
    { R16f    } 2,
    { R32F    } 4,
    { R32f    } 4,
    { DXT1    } 8,
    { DXT3    } 16,
    { DXT5    } 16,
    { D24_S8  } 4,
    { D32f_S8 } 8,
    { D16     } 2,
    { D24     } 4,
    { D32     } 4,
    { D32f    } 4);
    IsDepthTexture : array [TTextureFormat] of Boolean = (
    { RGBA    } false,
    { RGBA16  } false,
    { RGBA16f } false,
    { RGBA32  } false,
    { RGBA32f } false,
    { RGB     } false,
    { RGB16   } false,
    { RGB16f  } false,
    { RGB32   } false,
    { RGB32f  } false,
    { RG      } false,
    { RG16    } false,
    { RG16f   } false,
    { RG32    } false,
    { RG32f   } false,
    { R       } false,
    { R16F    } false,
    { R16f    } false,
    { R32F    } false,
    { R32f    } false,
    { DXT1    } false,
    { DXT3    } false,
    { DXT5    } false,
    { D24_S8  } true,
    { D32f_S8 } true,
    { D16     } true,
    { D24     } true,
    { D32     } true,
    { D32f    } true);
    Sampler_NoFilter : TSamplerInfo = (
      MinFilter  : tfNearest;
      MagFilter  : tfNearest;
      MipFilter  : tfNone;
      Anisotropy : 16;
      Wrap_X     : twRepeat;
      Wrap_Y     : twRepeat;
      Border     : (x: 0; y: 0; z: 0; w: 0);
    );
    Sampler_LinearNoMips : TSamplerInfo = (
      MinFilter  : tfLinear;
      MagFilter  : tfLinear;
      MipFilter  : tfNone;
      Anisotropy : 16;
      Wrap_X     : twRepeat;
      Wrap_Y     : twRepeat;
      Border     : (x: 0; y: 0; z: 0; w: 0);
    );
    Sampler_Linear : TSamplerInfo = (
      MinFilter  : tfLinear;
      MagFilter  : tfLinear;
      MipFilter  : tfLinear;
      Anisotropy : 16;
      Wrap_X     : twRepeat;
      Wrap_Y     : twRepeat;
      Border     : (x: 0; y: 0; z: 0; w: 0);
    );

    Sampler_LinearClamped : TSamplerInfo = (
      MinFilter  : tfLinear;
      MagFilter  : tfLinear;
      MipFilter  : tfLinear;
      Anisotropy : 16;
      Wrap_X     : twClamp;
      Wrap_Y     : twClamp;
      Border     : (x: 0; y: 0; z: 0; w: 0);
    );

type
  TFOURCC = Cardinal;

  { TNoRefObject }

  TNoRefObject = class (TObject, IUnknown)
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

function Create_DataLayout(const AFields: TFieldInfoArr; AStrideSize: Integer = 0): IDataLayout;
function CalcPrimCount(const ItemsCount: Integer; const PrimType: TPrimitiveType): Integer;
function ComponentTypeSize(const AType: TComponentType): Integer;

function MakeFourCC(ch0,ch1,ch2,ch3: AnsiChar): TFOURCC;
procedure StreamWriteString(stream: TStream; const str: AnsiString);
procedure StreamReadString(stream: TStream; out str: AnsiString);

procedure ZeroClear(out data; const DataSize: Integer);

implementation

type

  { TDataLayout }

  TDataLayout = class(TInterfacedObject, IDataLayout)
  private
    FFields: TFieldInfoArr;
    FTotalSize: Integer;
  public
    //*properties implementation
    function GetItem(index: Integer): TFieldInfo;
    function GetSize: Integer;
    //*end of properties implementation
    function Count: Integer;

    property Item[index: Integer]: TFieldInfo read GetItem; default;
    property Size: Integer read GetSize;

    constructor Create(const AFields: TFieldInfoArr; AStrideSize: Integer = 0);
  end;

function Create_DataLayout(const AFields: TFieldInfoArr; AStrideSize: Integer = 0): IDataLayout;
begin
  Result := TDataLayout.Create(AFields, AStrideSize);
end;

function CalcPrimCount(const ItemsCount: Integer; const PrimType: TPrimitiveType): Integer;
begin
  case PrimType of
      ptPoints           : Result := ItemsCount;
      ptLines            : Result := ItemsCount div 2;
      ptLineStrip        : Result := ItemsCount - 1;
      ptTriangles        : Result := ItemsCount div 3;
      ptTriangleStrip    : Result := ItemsCount - 2;
      ptLines_Adj        : Assert(False, 'Not implemented yet');
      ptLineStrip_Adj    : Assert(False, 'Not implemented yet');
      ptTriangles_Adj    : Assert(False, 'Not implemented yet');
      ptTriangleStrip_Adj: Assert(False, 'Not implemented yet');
  end;
end;

function ComponentTypeSize(const AType: TComponentType): Integer;
begin
  case AType of
    ctBool  : Assert(False, 'Not implemented yet');
    ctByte  : Result := 1;
    ctUByte : Result := 1;
    ctShort : Result := 2;
    ctUShort: Result := 2;
    ctInt   : Result := 4;
    ctUInt  : Result := 4;
    ctFloat : Result := 4;
    ctDouble: Result := 8;
  else
    Assert(False, 'Not implemented yet')
  end;
end;

function MakeFourCC(ch0,ch1,ch2,ch3: AnsiChar): TFOURCC;
begin
  Result := Byte(ch3);
  Result := Result shl 8 or Byte(ch2);
  Result := Result shl 8 or Byte(ch1);
  Result := Result shl 8 or Byte(ch0);
end;

procedure StreamWriteString(stream: TStream; const str: AnsiString);
var n: Integer;
begin
  n := Length(str);
  stream.WriteBuffer(n, SizeOf(n));
  if n > 0 then
    stream.WriteBuffer(str[1], n);
end;

procedure StreamReadString(stream: TStream; out str: AnsiString);
var n: Integer;
begin
  n := 0;
  stream.ReadBuffer(n, SizeOf(n));
  SetLength(str, n);
  if n > 0 then
      stream.ReadBuffer(str[1], n);
end;

procedure ZeroClear(out data; const DataSize: Integer);
begin
  {$Hints OFF}
  FillChar(data, DataSize, 0);
  {$Hints ON}
end;

{ TNoRefObject }

function TNoRefObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TNoRefObject._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TNoRefObject._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

{ TDataLayout }

function TDataLayout.GetItem(index: Integer): TFieldInfo;
begin
  Result := FFields[index];
end;

function TDataLayout.GetSize: Integer;
begin
  Result := FTotalSize;
end;

function TDataLayout.Count: Integer;
begin
  Result := Length(FFields);
end;

constructor TDataLayout.Create(const AFields: TFieldInfoArr; AStrideSize: Integer = 0);
var
  i: Integer;
begin
  FFields := AFields;
  if AStrideSize = 0 then
  begin
    FTotalSize := 0;
    for i := 0 to Length(FFields) - 1 do
      Inc(FTotalSize, AFields[i].Size);
  end
  else
    FTotalSize := AStrideSize;
end;

end.
