unit avContext_DX11;

{$mode objfpc}{$H+}

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

uses Windows, Math;

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

{ TTexture }

function TTexture.GetTargetFormat: TTextureFormat;
begin

end;

procedure TTexture.SetTargetFormat(Value: TTextureFormat);
begin

end;

function TTexture.Width: Integer;
begin

end;

function TTexture.Height: Integer;
begin

end;

function TTexture.Format: TTextureFormat;
begin

end;

procedure TTexture.AllocMem(AWidth, AHeight: Integer; WithMips: Boolean);
begin

end;

procedure TTexture.AllocMem(AWidth, AHeight: Integer; WithMips: Boolean;
  DataFormat: TImageFormat; Data: PByte);
begin

end;

procedure TTexture.SetImage(ImageWidth, ImageHeight: Integer;
  DataFormat: TImageFormat; Data: PByte; GenMipmaps: Boolean);
begin

end;

procedure TTexture.SetImage(X, Y, ImageWidth, ImageHeight: Integer;
  DataFormat: TImageFormat; Data: PByte; GenMipmaps: Boolean);
begin

end;

procedure TTexture.SetMipImage(X, Y, ImageWidth, ImageHeight,
  MipLevel: Integer; DataFormat: TImageFormat; Data: PByte);
begin

end;

procedure TTexture.SetMipImage(DestRect: TRect; MipLevel: Integer;
  DataFormat: TImageFormat; Data: PByte);
begin

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
  Assert(FBindCount = 0, '???');
  Dec(FBindCount);
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

