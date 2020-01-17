unit avGPUGlyphGenerator;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avRes, avContext, avContnrs, mutils, intfUtils, avTypes, avTess, avGlyphGenerator;

type
  { TSegmentVertex }

  TSegmentVertex = record
    seg: TVec4;
    class function Layout: IDataLayout; static;
  end;
  TSegmentVertexArr = {$IfDef FPC}specialize{$EndIf}TVerticesRec<TSegmentVertex>;
  ISegmentVertexArr = {$IfDef FPC}specialize{$EndIf}IArray<TSegmentVertex>;

  IGPUGlyphGenerator = interface
    procedure BeginGeneration;
    procedure DrawToTexture(const AGlyph: IGlyphPoly; const ASize: TVec2i; const ADstTexture: IctxTexture; const ADstPos: TVec2i; ADstSlice, ADstMip: Integer);
    procedure DrawToTexture(const AGlyph: IGlyphPoly; const ASize: TVec2i; const ADstTexture: TavTextureBase; const ADstPos: TVec2i; ADstSlice, ADstMip: Integer);
    procedure EndGeneration;
  end;

function GetGlyphGenerator(const AMain: TavMainRender): IGPUGlyphGenerator;

implementation

//{$R '..\GlyphGen_Shaders\GlyphGen_Shaders.rc'}
{$R '..\GlyphGen_Shaders\GlyphGen_Shaders.res'}

uses
  Math;

type
  { TavGlyphGenerator }

  TavGlyphGenerator = class(TavMainRenderChild, IUnknown, IGPUGlyphGenerator)
  private
    FFBO: TavFrameBuffer;
    FProgram: TavProgram;
    FSBGlyph: TavSB;
    FSBDataGlyph: ISegmentVertexArr;

    FInGeneration: Boolean;
    FPrevFBO  : TavFrameBuffer;
    FPrevProg : TavProgram;
    FPrevBlend: Boolean;
    FPrevScissor: Boolean;
    FPrevViewport: TRectI;
    FPrevMask: TColorMask;

    procedure PrepareBuffers(const AGlyph: IGlyphPoly); overload;
    procedure RenderGlyph(const ATexSize: TVec2i);
  private
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  protected
    procedure AfterRegister; override;
  public
    procedure BeginGeneration;
    procedure DrawToTexture(const AGlyph: IGlyphPoly; const ASize: TVec2i; const ADstTexture: IctxTexture; const ADstPos: TVec2i; ADstSlice, ADstMip: Integer);
    procedure DrawToTexture(const AGlyph: IGlyphPoly; const ASize: TVec2i; const ADstTexture: TavTextureBase; const ADstPos: TVec2i; ADstSlice, ADstMip: Integer);
    procedure EndGeneration;
  end;

function GetGlyphGenerator(const AMain: TavMainRender): IGPUGlyphGenerator;
const cGLYPH_GEN = 'GPU_GLYPH_GENERATOR';
var gen: TavGlyphGenerator;
begin
  gen := TavGlyphGenerator( AMain.FindChild(cGLYPH_GEN, TavGlyphGenerator) );
  if gen = nil then
  begin
    gen := TavGlyphGenerator.Create(AMain);
    gen.Name := cGLYPH_GEN;
  end;
  Result := gen;
end;

{ TSegmentVertex }

class function TSegmentVertex.Layout: IDataLayout;
begin
  Result := LB.Add('seg', ctFloat, 4).Finish();
end;

{ TavGlyphGenerator }

procedure TavGlyphGenerator.PrepareBuffers(const AGlyph: IGlyphPoly);
var i, j: Integer;
    seg: TSegmentVertex;
    cntr: IGlyphContour;
    cntrLen: Integer;
begin
  FSBDataGlyph.Clear();
  for j := 0 to AGlyph.Count - 1 do
  begin
    cntr := AGlyph[j];
    cntrLen := cntr.Count;
    for i := 0 to cntrLen-1 do
    begin
      seg.seg.xy := cntr[i];
      seg.seg.zw := cntr[(i+1) mod cntrLen];
      FSBDataGlyph.Add(seg);
    end;
  end;
  FSBGlyph.Invalidate;
end;

procedure TavGlyphGenerator.RenderGlyph(const ATexSize: TVec2i);
begin
  FFBO.FrameRect := RectI(Vec(0,0), Max(ATexSize, FFBO.FrameRect.max));
  FFBO.Select(False);
  Main.States.Viewport := RectI(Vec(0,0), ATexSize);
  FProgram.Select();
  FProgram.SetUniform('GlyphSize', Single(FSBDataGlyph.Count));
  FProgram.SetUniform('Glyph', FSBGlyph);
  FProgram.Draw(ptTriangleStrip, cmNone, False, 0, 0, 3);
end;

function TavGlyphGenerator.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getinterface(iid,obj) then
    result:=S_OK
  else
    result:=longint(E_NOINTERFACE);
end;

function TavGlyphGenerator._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TavGlyphGenerator._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

procedure TavGlyphGenerator.AfterRegister;
begin
  inherited AfterRegister;
  FFBO := Create_FrameBuffer(Self, [TTextureFormat.R16f]);
  FFBO.FrameRect := RectI(0,0,1,1);

  FProgram := TavProgram.Create(Self);
  FProgram.Load('GPUSDFGlyphGenerator', True, '');

  FSBGlyph := TavSB.Create(Self);
  FSBDataGlyph := TSegmentVertexArr.Create();
  FSBGlyph.Vertices := FSBDataGlyph as IVerticesData;
end;

procedure TavGlyphGenerator.BeginGeneration;
begin
  Assert(not FInGeneration);
  FInGeneration := True;
  FPrevProg := Main.ActiveProgram;
  FPrevFBO := Main.ActiveFrameBuffer;
  FPrevBlend := Main.States.Blending[0];
  FPrevScissor := Main.States.GetScissorTest;
  FPrevViewport := Main.States.Viewport;
  FPrevMask := Main.States.ColorMask[0];

  Main.States.Blending[0] := False;
  Main.States.SetScissorTest(False);
  Main.States.ColorMask[0] := [cmRed]
end;

procedure TavGlyphGenerator.DrawToTexture(const AGlyph: IGlyphPoly; const ASize: TVec2i; const ADstTexture: IctxTexture; const ADstPos: TVec2i; ADstSlice, ADstMip: Integer);
begin
  Assert(FInGeneration);
  PrepareBuffers(AGlyph);
  RenderGlyph(ASize);
  ADstTexture.CopyFrom(ADstMip, ADstSlice, ADstPos, FFBO.GetColor(0).Handle, 0, 0, RectI(Vec(0,0), ASize));
end;

procedure TavGlyphGenerator.DrawToTexture(const AGlyph: IGlyphPoly; const ASize: TVec2i; const ADstTexture: TavTextureBase; const ADstPos: TVec2i; ADstSlice, ADstMip: Integer);
begin
  ADstTexture.Build;
  DrawToTexture(AGlyph, ASize, ADstTexture.Handle, ADstPos, ADstSlice, ADstMip);
end;

procedure TavGlyphGenerator.EndGeneration;
begin
  Assert(FInGeneration);
  FInGeneration := False;
  Main.States.Blending[0] := FPrevBlend;
  Main.States.SetScissorTest(FPrevScissor);
  if (FPrevFBO <> nil) then FPrevFBO.Select(False);
  if (FPrevProg <> nil) then FPrevProg.Select();
  Main.States.Viewport := FPrevViewport;
  Main.States.ColorMask[0] := FPrevMask;
end;

end.

