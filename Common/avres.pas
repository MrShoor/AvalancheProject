unit avRes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, avBase, avContext, mutils, avTypes, avPlatform, avContnrs;

type
  TavProgram = class;
  TavCamera = class;
  TavProjection = class;
  TavCursor = class;

  { TavMainRender }

  TavMainRender = class (TavObject)
  private
    FFrameID   : Int64;
    FActiveAPI : T3DAPI;
    FContext   : IRenderContext;
    FWindow    : TWindow;
    FActiveProgram: TavProgram;

    FCamera: TavCamera;
    FProjection: TavProjection;
    FCursor: TavCursor;

    FUPS: Integer;
    FLastTime: Int64;

    function GetActiveProgram: TavProgram;
    function GetWindow: TWindow;
    procedure SetActiveProgram(AValue: TavProgram);
    procedure SetWindow(AValue: TWindow);

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
    procedure Present;
    procedure InvalidateWindow;

    property Window: TWindow read GetWindow write SetWindow;
    property ActiveProgram: TavProgram read GetActiveProgram write SetActiveProgram;

    procedure Dispatch(var message); override;
    constructor Create(AParent: TavObject); override;
    destructor Destroy; override;
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

  TavRes = class(TavObject)
  strict private
    FDirty   : Boolean;
    FMain    : TavMainRender;
  protected
    procedure AfterInit3D; virtual;
    procedure BeforeFree3D; virtual;
    procedure AfterFree3D; virtual;
    function CanRegister(target: TavObject): boolean; override;

    function DoBuild: Boolean; virtual;
    function Main: TavMainRender;

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

  { TavTexture }

  TavTexture = class(TavRes)
  private
    FAutoGenerateMips: Boolean;
    FDropLocalAfterBuild: Boolean;
    FImageIndex: Integer;
    FTargetFormat: TTextureFormat;
    FTexData: ITextureData;
    FImageSize: TVec2;
    procedure SetAutoGenerateMips(AValue: Boolean);
    procedure SetImageIndex(AValue: Integer);
    procedure SetTexData(AValue: ITextureData);
  protected
    FTexH: IctxTexture;
    procedure BeforeFree3D; override;
    function DoBuild: Boolean; override;
  public
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property DropLocalAfterBuild: Boolean read FDropLocalAfterBuild write FDropLocalAfterBuild;
    property TexData: ITextureData read FTexData write SetTexData;

    function Width: Integer;
    function Height: Integer;
    function Size: TVec2;
    function ImageSize: TVec2;

    property TargetFormat: TTextureFormat read FTargetFormat write FTargetFormat;
    property AutoGenerateMips: Boolean read FAutoGenerateMips write SetAutoGenerateMips;

    procedure AfterConstruction; override;
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
    procedure Select; virtual;
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
    procedure SetUniform (const Field: TUniformField; const tex: TavTexture; const sampler: TSamplerInfo); overload;

    procedure SetUniform (const AName: string; const value: integer);     overload;
    procedure SetUniform (const AName: string; const value: single);      overload;
    procedure SetUniform (const AName: string; const v: TVec2);           overload;
    procedure SetUniform (const AName: string; const v: TVec3);           overload;
    procedure SetUniform (const AName: string; const v: TVec4);           overload;
    procedure SetUniform (const AName: string; const values: TSingleArr); overload;
    procedure SetUniform (const AName: string; const v: TVec4arr);        overload;
    procedure SetUniform (const AName: string; const m: TMat4);           overload;
    procedure SetUniform (const AName: string; const tex: TavTexture; const sampler: TSamplerInfo); overload;

    procedure LoadFromJSON(const AProgram: string; FromResource: boolean = false; const AProgramPath: string = ''); overload;

    procedure Draw(InstanceCount: Integer = 0;
                   Start: integer = 0; Count: integer = - 1;
                   BaseVertex: integer = 0; BaseInstance: Integer = 0); overload;
    procedure Draw(PrimTopology: TPrimitiveType; CullMode: TCullingMode; IndexedGeometry: Boolean;
                   InstanceCount: Integer = 0;
                   Start: integer = 0; Count: integer = - 1;
                   BaseVertex: integer = 0; BaseInstance: Integer = 0); overload;

    destructor Destroy; override;
  end;

  { TavFrameBuffer }

  TavFrameBuffer = class(TavRes)
  private type
    TAttachInfo = record
      tex: IWeakRef;
      mipLevel: Integer;
    end;
  private const
    EmptyAttachInfo: TAttachInfo = (tex : nil; mipLevel : 0);
  private type
    TColorsList = specialize TArray<TAttachInfo>;
    IColorsList = specialize IArray<TAttachInfo>;
  private
    FFrameBuf: IctxFrameBuffer;
    FColors  : IColorsList;
    FDepth   : TAttachInfo;
    FFrameRect: TRectI;
    procedure SetFrameRect(AValue: TRectI);
  protected
    procedure BeforeFree3D; override;
    function DoBuild: Boolean; override;
  public
    procedure Select(UpdateProjMatrix: Boolean = True);

    property FrameRect: TRectI read FFrameRect write SetFrameRect;

    procedure ClearColorList;

    function GetColor(Index: Integer): TavTexture;
    function GetColorMipLevel(Index: Integer): Integer;
    procedure SetColor(Index: Integer; AValue: TavTexture; mipLevel: Integer = 0);

    function GetDepth: TavTexture;
    procedure SetDepth(AValue: TavTexture; mipLevel: Integer);

    procedure Clear(index: Integer; color: TVec4);
    procedure ClearDS(depth: Single; clearDepth: Boolean = True; stencil: Integer = 0; clearStencil: Boolean = False);

    procedure BlitToWindow(index: Integer = 0);

    procedure AfterConstruction; override;
  end;

function Create_FrameBuffer(Parent: TavObject; textures: array of TTextureFormat): TavFrameBuffer;

implementation

uses
  TypInfo,
  Math,
  avLog,
  avContext_OGL,
  avContext_DX11,
  avTexLoader;

function Create_FrameBuffer(Parent: TavObject; textures: array of TTextureFormat): TavFrameBuffer;
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

{ TavFrameBuffer }

function TavFrameBuffer.GetColor(Index: Integer): TavTexture;
var ref: IWeakRef;
begin
  if (Index >= 0) and (Index < FColors.Count) then
    ref := FColors.Item[Index].tex
  else
    ref := nil;
  if Assigned(ref) then
      Result := TavTexture(ref.Obj);
end;

function TavFrameBuffer.GetColorMipLevel(Index: Integer): Integer;
begin
  if (Index >= 0) and (Index < FColors.Count) then
    Result := FColors.Item[Index].mipLevel
  else
    Result := 0;
end;

function TavFrameBuffer.GetDepth: TavTexture;
begin
  Result := nil;
  if Assigned(FDepth.tex) then
    Result := TavTexture(FDepth.tex.Obj);
end;

procedure TavFrameBuffer.SetColor(Index: Integer; AValue: TavTexture; mipLevel: Integer = 0);
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

procedure TavFrameBuffer.SetDepth(AValue: TavTexture; mipLevel: Integer);
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
  function GetTex(const ainfo: TAttachInfo): TavTexture; inline;
  begin
    if Assigned(ainfo.tex) then
      Result := TavTexture(ainfo.tex.Obj)
    else
      Result := nil;
  end;

  procedure ResizeTex(tex: TavTexture; FrameSize: TVec2i; mipLevel: Integer); //inline;
  var NewTexSize: TVec2i;
  begin
    NewTexSize := NextPow2(FrameSize) * (1 shl mipLevel);
    if (tex.Size.x <> NewTexSize.x) or (tex.Size.y <> NewTexSize.y) then
      tex.TexData := EmptyTexData(NewTexSize.x, NewTexSize.y, tex.TargetFormat, mipLevel > 0);
    tex.Build;
  end;

var ainfo: TAttachInfo;
    tex: TavTexture;
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

  tex := GetTex(FDepth);
  if Assigned(tex) then
  begin
    ResizeTex(tex, FrameSize, FDepth.mipLevel);
    FFrameBuf.SetDepthStencil(tex.FTexH, FDepth.mipLevel);
  end
  else
    FFrameBuf.SetDepthStencil(nil, FDepth.mipLevel);
end;

procedure TavFrameBuffer.Select(UpdateProjMatrix: Boolean);
begin
  Build;
  Main.Context.States.ViewPort := FFrameRect;
  if UpdateProjMatrix then
    Main.Projection.Aspect := (FFrameRect.Bottom - FFrameRect.Top)/(FFrameRect.Right - FFrameRect.Left);
  FFrameBuf.Select;
end;

procedure TavFrameBuffer.ClearColorList;
begin
  FColors.Clear;
  Invalidate;
end;

procedure TavFrameBuffer.AfterConstruction;
begin
  inherited AfterConstruction;
  FColors := TColorsList.Create(EmptyAttachInfo);
end;

{ TavTexture }

procedure TavTexture.SetTexData(AValue: ITextureData);
begin
  if FTexData = AValue then Exit;
  FTexData := AValue;
  Invalidate;
end;

procedure TavTexture.SetImageIndex(AValue: Integer);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  Invalidate;
end;

procedure TavTexture.SetAutoGenerateMips(AValue: Boolean);
begin
  if FAutoGenerateMips = AValue then Exit;
  FAutoGenerateMips := AValue;
  Invalidate;
end;

procedure TavTexture.BeforeFree3D;
begin
  inherited BeforeFree3D;
  if Assigned(FTexH) then Invalidate;
  FTexH := nil;
end;

function TavTexture.DoBuild: Boolean;
var MipCount: Integer;
    MipInfo : TTextureMipInfo;
    i: Integer;
begin
  if Assigned(FTexData) and (FTexData.ItemCount > FImageIndex) then
  begin
    MipCount := FTexData.MipCount(FImageIndex);
    if MipCount > 0 then
    begin
      if FTexH = nil then FTexH := Main.Context.CreateTexture;
      FTexH.TargetFormat := FTargetFormat;
      MipInfo := FTexData.Data(FImageIndex, 0);
      FImageSize.x := MipInfo.Width;
      FImageSize.y := MipInfo.Height;
      if MipCount = 1 then
        FTexH.SetImage(MipInfo.Width, MipInfo.Height, FTexData.Format, MipInfo.Data, FAutoGenerateMips)
      else
      begin
        FTexH.AllocMem(NextPow2(MipInfo.Width), NextPow2(MipInfo.Height), True);
        for i := 0 to MipCount - 1 do
        begin
          MipInfo := FTexData.Data(FImageIndex, i);
          FTexH.SetMipImage(0, 0, MipInfo.Width, MipInfo.Height, i, FTexData.Format, MipInfo.Data);
        end;
      end;
    end;
  end;
  if FDropLocalAfterBuild then FTexData := nil;
  Result := True;
end;

function TavTexture.Width: Integer;
begin
  if Assigned(FTexH) then
    Result := FTexH.Width
  else
    Result := 0;
end;

function TavTexture.Height: Integer;
begin
  if Assigned(FTexH) then
    Result := FTexH.Height
  else
    Result := 0;
end;

function TavTexture.Size: TVec2;
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

  m := FMain.Camera.uMatrix * FMain.Projection.uMatrix;
  v := Vec(vCur.x, vCur.y, FMain.Projection.DepthRange.x, 1);
  v := m * v;
  FFrom := v.xyz / v.w;
  v := Vec(vCur.x, vCur.y, FMain.Projection.DepthRange.y, 1);
  v := m * v;
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

    FillChar(Result, SizeOf(Result), 0);
    Result.f[0, 0] := w;
    Result.f[1, 1] := h;
    Result.f[2, 2] := DepthRange.x - DepthSize * FarPlane * Q;
    Result.f[3, 2] := 1.0;
    Result.f[2, 3] := DepthSize * fNearPlane * fFarPlane * Q;
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
    Result.f[2, 3] := DepthRange.x - DepthSize * NearPlane * Q;
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
const APIToRange: array [T3DAPI] of TVec2 = ( {OGL}(x: -1; y: 1), {DX11} (x: 0; y: 1) );
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

function TavMainRender.GetWindow: TWindow;
begin
  Result := FWindow;
end;

function TavMainRender.GetActiveProgram: TavProgram;
begin
  Result := FActiveProgram;
end;

procedure TavMainRender.SetActiveProgram(AValue: TavProgram);
begin
  if FActiveProgram <> AValue then
  begin
    FActiveProgram := AValue;
    if Assigned(FActiveProgram) then
      FActiveProgram.Select
    else
      if Assigned(FContext) then
        FContext.ActiveProgram := nil;
  end;
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
            apiDX11:
              begin
                FContext := TContext_DX11.Create(FWindow);
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

function TavRes.CanRegister(target: TavObject): boolean;
begin
  Result := inherited CanRegister(target);
  if not Result then Exit;
  FMain := TavMainRender(target.FindAtParents(TavMainRender));
  Result := Assigned(FMain);
end;

function TavRes.DoBuild: Boolean;
begin
  Result := True;
end;

function TavRes.Main: TavMainRender;
begin
  Result := FMain;
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
  if FMain.Inited3D then
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

  FProgram.Load(newSrc, FFromRes);

  pInfo := TypeInfo(TUniformMatrices);
  for i := Low(TUniformMatrices) to High(TUniformMatrices) do
    FUniformsMatrices[i] := GetUniformField(GetEnumName(pInfo, Integer(I)));

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
      VP_Matrix       : SetUniform(FUniformsMatrices[i], Main.Projection.Matrix  * Main.Camera.Matrix);
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

procedure TavProgram.Select;
begin
  if Main.ActiveProgram = Self then Exit;
  Build;
  Main.ActiveProgram := Self;
  FProgram.Select;
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

procedure TavProgram.SetUniform(const Field: TUniformField; const tex: TavTexture; const sampler: TSamplerInfo);
begin
  if tex = nil then Exit;
  tex.Build;
  FProgram.SetUniform(Field, tex.FTexH, sampler);
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

procedure TavProgram.SetUniform(const AName: string; const tex: TavTexture; const sampler: TSamplerInfo);
begin
  if tex = nil then Exit;
  tex.Build;
  FProgram.SetUniform(GetUniformField(AName), tex.FTexH, sampler);
end;

procedure TavProgram.LoadFromJSON(const AProgram: string; FromResource: boolean; const AProgramPath: string = '');
begin
  FSrc := AProgram;
  FFromRes := FromResource;
  FSrcPath := AProgramPath;
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

destructor TavProgram.Destroy;
begin
  FProgram := nil;
  if Main.ActiveProgram = Self then Main.ActiveProgram := nil;
  inherited Destroy;
end;

end.

