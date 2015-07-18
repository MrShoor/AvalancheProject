unit avTypes;

{$mode objfpc}{$H+}

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

  TIntArr = array of Integer;
  TWordArr = array of Word;

  T3DAPI = (apiOGL);

  TShaderType = (stUnknown, stVertex, stGeometry, stFragment);
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
  TImageFormat = (Unknown, Gray8, R3G3B2, R8G8, R5G6B5, A1R5G5B5, A4R4G4B4, R8G8B8, B8G8R8A8, R8G8B8A8, R16, R16G16, R16G16B16, A16R16G16B16, B16G16R16,
                  A16B16G16R16, R32, R32G32, R32G32B32, A32R32G32B32F, A32B32G32R32F, DXT1, DXT3, DXT5);
  {$SCOPEDENUMS OFF}
  TTextureFilter = (tfNone, tfNearest, tfLinear);
  TTextureWrap   = (twRepeat, twMirror, twClamp, twClampToEdge);
  TBlendFunc     = (bfZero, bfOne, bfSrcAlpha, bfInvSrcAlpha);
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

  TTextureMipInfo = packed record
    Width : Integer;
    Height: Integer;
    Level : Integer;
    Data  : PByte;
  end;

  ITextureData = interface
  ['{3643BDFB-36C1-4A64-BEA6-04108FDB8F1E}']
    function Format: TImageFormat;
    function ItemCount: Integer;
    function MipCount(const Index: Integer): Integer;
    function Data(const Index, MipLevel: Integer): TTextureMipInfo;
  end;

function Create_DataLayout(const AFields: TFieldInfoArr; AStrideSize: Integer = 0): IDataLayout;
function CalcPrimCount(const ItemsCount: Integer; const PrimType: TPrimitiveType): Integer;
function ComponentTypeSize(const AType: TComponentType): Integer;

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
