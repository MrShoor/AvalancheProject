unit avContext_DX11;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils, avTypes, avPlatform, avContext, avContnrs, mutils, contnrs,
  D3D11_JSB, DXGI_JSB, DXTypes_JSB, D3DCommon_JSB;

type
  TavInterfacedObject = TInterfacedObject;

  { TContext_DX11 }

  TContext_DX11 = class (TavInterfacedObject, IRenderContext)
  private
    FStates: TObject;
    FStatesIntf: IRenderStates;
    FBindCount: Integer;
    FActiveProgram: IctxProgram;

    FSwapChain: IDXGISwapChain;
    FRenderTarget: ID3D11RenderTargetView;
    FDevice: ID3D11Device;
    FDeviceContext: ID3D11DeviceContext;
    FWnd: TWindow;

    function GetActiveProgram: IctxProgram;
    procedure SetActiveProgram(AValue: IctxProgram);

    procedure RebuildViews(const AWidth, AHeight: Cardinal);
    procedure SetFrameBuffer(const AObject: TObject); //TFrameBuffer
  public
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

procedure Check3DError(hr: HRESULT);
var s: string;
begin
    if hr = 0 then Exit;
    case hr of
      //D3D10_ERROR_FILE_NOT_FOUND                : s := 'D3D10_ERROR_FILE_NOT_FOUND';
      //D3D10_ERROR_TOO_MANY_UNIQUE_STATE_OBJECTS : s := 'D3D10_ERROR_TOO_MANY_UNIQUE_STATE_OBJECTS';
      ////D3DERR_INVALIDCALL                        : s := 'D3DERR_INVALIDCALL';
      ////D3DERR_WASSTILLDRAWING                    : s := 'D3DERR_WASSTILLDRAWING';
      //
      //DXGI_ERROR_INVALID_CALL                 : s := 'DXGI_ERROR_INVALID_CALL';
      //DXGI_ERROR_NOT_FOUND                    : s := 'DXGI_ERROR_NOT_FOUND';
      //DXGI_ERROR_MORE_DATA                    : s := 'DXGI_ERROR_MORE_DATA';
      //DXGI_ERROR_UNSUPPORTED                  : s := 'DXGI_ERROR_UNSUPPORTED';
      //DXGI_ERROR_DEVICE_REMOVED               : s := 'DXGI_ERROR_DEVICE_REMOVED';
      //DXGI_ERROR_DEVICE_HUNG                  : s := 'DXGI_ERROR_DEVICE_HUNG';
      //DXGI_ERROR_DEVICE_RESET                 : s := 'DXGI_ERROR_DEVICE_RESET';
      //DXGI_ERROR_WAS_STILL_DRAWING            : s := 'DXGI_ERROR_WAS_STILL_DRAWING';
      //DXGI_ERROR_FRAME_STATISTICS_DISJOINT    : s := 'DXGI_ERROR_FRAME_STATISTICS_DISJOINT';
      //DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE : s := 'DXGI_ERROR_GRAPHICS_VIDPN_SOURCE_IN_USE';
      //DXGI_ERROR_DRIVER_INTERNAL_ERROR        : s := 'DXGI_ERROR_DRIVER_INTERNAL_ERROR';
      //DXGI_ERROR_NONEXCLUSIVE                 : s := 'DXGI_ERROR_NONEXCLUSIVE';
      //DXGI_ERROR_NOT_CURRENTLY_AVAILABLE      : s := 'DXGI_ERROR_NOT_CURRENTLY_AVAILABLE';
      //DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED   : s := 'DXGI_ERROR_REMOTE_CLIENT_DISCONNECTED';
      //DXGI_ERROR_REMOTE_OUTOFMEMORY           : s := 'DXGI_ERROR_REMOTE_OUTOFMEMORY';
      ////DXGI_ERROR_ACCESS_LOST                  : s := 'DXGI_ERROR_ACCESS_LOST';
      ////DXGI_ERROR_WAIT_TIMEOUT                 : s := 'DXGI_ERROR_WAIT_TIMEOUT';
      ////DXGI_ERROR_SESSION_DISCONNECTED         : s := 'DXGI_ERROR_SESSION_DISCONNECTED';
      ////DXGI_ERROR_RESTRICT_TO_OUTPUT_STALE     : s := 'DXGI_ERROR_RESTRICT_TO_OUTPUT_STALE';
      ////DXGI_ERROR_CANNOT_PROTECT_CONTENT       : s := 'DXGI_ERROR_CANNOT_PROTECT_CONTENT';
      ////DXGI_ERROR_ACCESS_DENIED                : s := 'DXGI_ERROR_ACCESS_DENIED';
      ////DXGI_ERROR_NAME_ALREADY_EXISTS          : s := 'DXGI_ERROR_NAME_ALREADY_EXISTS';
      //
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

type

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

  { TTexture }

  TTexture = class (THandleObject, IctxTexture)
  private
    FTargetFormat: TTextureFormat;
    FWidth: Integer;
    FHeight: Integer;
    FFormat: TTextureFormat;
    FWithMips: Boolean;

    FTexture: ID3D11Texture2D;
    FResView: ID3D11ShaderResourceView;

    function BuildDesc(AWidth, AHeight: Integer; WithMips: Boolean): TD3D11_Texture2DDesc;
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

  { TFrameBuffer }

  TFrameBuffer = class (THandleObject, IctxFrameBuffer)
  private
    FViews: array of ID3D11RenderTargetView;
    FDepthStencil: ID3D11DepthStencilView;
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
    Result := False;
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

procedure TFrameBuffer.Select;
begin
  FContext.SetFrameBuffer(Self);
end;

procedure TFrameBuffer.ClearColorList;
begin

end;

procedure TFrameBuffer.EnableColorTarget(index: Integer; Enabled: Boolean);
begin

end;

procedure TFrameBuffer.SetColor(index: Integer; tex: IctxTexture;
  mipLevel: Integer);
begin

end;

procedure TFrameBuffer.SetDepthStencil(tex: IctxTexture; mipLevel: Integer);
begin

end;

procedure TFrameBuffer.Clear(index: Integer; color: TVec4);
begin

end;

procedure TFrameBuffer.ClearDS(depth: Single; clearDepth: Boolean;
  stencil: Integer; clearStencil: Boolean);
begin

end;

procedure TFrameBuffer.BlitToWindow(index: Integer; const srcRect,
  dstRect: TRectI; const Filter: TTextureFilter);
begin

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
    BackBuffer: ID3D11Texture2D;
    ViewPort: TD3D11_Viewport;
begin
  FSwapChain.GetDesc(SwapChainDesc);
  if (SwapChainDesc.BufferDesc.Width = AWidth) and
     (SwapChainDesc.BufferDesc.Height = AHeight) and
     Assigned(FRenderTarget) then Exit;
  FRenderTarget := nil;
  BackBuffer := nil;

  FDeviceContext.ClearState;
  //TD3D10States(FStatesObj).InvalidateAllStates;
  FSwapChain.ResizeBuffers(SwapChainDesc.BufferCount, AWidth, AHeight, SwapChainDesc.BufferDesc.Format, SwapChainDesc.Flags);

  Check3DError(FSwapChain.GetBuffer(0, ID3D11Texture2D, BackBuffer));
  Check3DError(FDevice.CreateRenderTargetView(BackBuffer, nil, FRenderTarget));

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
      FDeviceContext.OMSetRenderTargets(Length(FBO.FViews), @FBO.FViews[0], FBO.FDepthStencil);
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
  if assigned(FBO) then
  begin
      SetFBORenderTargets(FBO);
  end
  else
  begin
      SetDefaultRenderTargets;
  end;
end;

function TContext_DX11.CreateVertexBuffer: IctxVetexBuffer;
begin

end;

function TContext_DX11.CreateIndexBuffer: IctxIndexBuffer;
begin

end;

function TContext_DX11.CreateProgram: IctxProgram;
begin

end;

function TContext_DX11.CreateTexture: IctxTexture;
begin
  Result := TTexture.Create(Self);
end;

function TContext_DX11.CreateFrameBuffer: IctxFrameBuffer;
begin

end;

function TContext_DX11.States: IRenderStates;
begin

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
begin
  FDeviceContext.ClearRenderTargetView(FRenderTarget, TColorArray(color));
//  FDeviceContext.ClearRenderTargetView();
//  FDeviceContext.ClearDepthStencilView();
end;

procedure TContext_DX11.Present;
begin
  Check3DError(FSwapChain.Present(0, 0));
end;

constructor TContext_DX11.Create(const Wnd: TWindow);
var SwapChainDesc: TDXGI_SwapChainDesc;
begin
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
  FSwapChain := nil;
  FDevice := nil;
  FDeviceContext := nil;
  inherited Destroy;
end;

end.

