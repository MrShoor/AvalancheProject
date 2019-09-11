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
    procedure InvalidateProg;
    function GlyphSegments: TavSB;
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

    procedure PrepareBuffers(const AGlyph: IGlyphPoly; AScale, AOffset: Single); overload;
    procedure PrepareBuffers(const AGlyph: IGlyphPoly); overload;
    procedure RenderGlyph(const ATexSize: TVec2i);
  private
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  protected
    procedure AfterRegister; override;
  public
    function PrepareGlyph(const AFontName  : string;
                          const AChar      : WideChar;
                          const ASize      : Integer;
                          const ABorder    : Integer;
                          const AItalic    : Boolean;
                          const ABold      : Boolean;
                          const AUnderLine : Boolean;
                          out   XXX        : TVec3;
                          out   YYYY       : TVec4): TVec2i;
    procedure InvalidateProg;
    function GlyphSegments: TavSB;
    function GlyphTexture: TavTextureBase;
    procedure DrawToTexture(const ATexture: IctxTexture; const ADstPos: TVec2i); overload;
    procedure DrawToTexture(const ATexture: TavTextureBase; const ADstPos: TVec2i); overload;

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

procedure TavGlyphGenerator.PrepareBuffers(const AGlyph: IGlyphPoly; AScale, AOffset: Single);
var i, j: Integer;
    seg: TSegmentVertex;
    vOffset: TVec2;
    cntr: IGlyphContour;
begin
  vOffset := Vec(AOffset, AOffset);

  FSBDataGlyph.Clear();

  for j := 0 to AGlyph.Count - 1 do
  begin
    cntr := AGlyph[j];
    for i := 0 to cntr.Count - 1 do
    begin
      seg.seg.xy := cntr[i]*AScale+vOffset;
      seg.seg.zw := cntr[(i+1) mod cntr.Count]*AScale+vOffset;
      FSBDataGlyph.Add(seg);
    end;
  end;

  FSBGlyph.Invalidate;
end;

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
  FFBO.FrameRect := RectI(Vec(0,0), ATexSize);
  FFBO.Select(False);
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

  FProgram := TavProgram.Create(Self);
  FProgram.Load('GPUSDFGlyphGenerator', True, '');

  FSBGlyph := TavSB.Create(Self);
  FSBDataGlyph := TSegmentVertexArr.Create();
  FSBGlyph.Vertices := FSBDataGlyph as IVerticesData;
end;

function TavGlyphGenerator.PrepareGlyph(const AFontName: string;
  const AChar: WideChar; const ASize: Integer; const ABorder: Integer;
  const AItalic: Boolean; const ABold: Boolean; const AUnderLine: Boolean; out
  XXX: TVec3; out YYYY: TVec4): TVec2i;
var
  glyph: IGlyphPoly;
  YSize: Single;
  scale: Single;
begin
  glyph := GenerateGlyphOutline(AFontName, AChar, AItalic, ABold, AUnderLine, XXX, YYYY);
  YSize := YYYY.x+YYYY.y+YYYY.z+YYYY.w;
  scale := ASize/YSize;
  Result.x := Ceil(XXX.y*scale) + ABorder*2;
  Result.y := Ceil((YYYY.y+YYYY.z)*scale) + ABorder*2;

  XXX := XXX * scale;
  YYYY := YYYY * scale;

  XXX.x := XXX.x - ABorder;
  XXX.y := XXX.y + 2*ABorder;
  XXX.z := XXX.z - ABorder;
  YYYY.x := YYYY.x - ABorder;
  YYYY.y := YYYY.y + ABorder;
  YYYY.z := YYYY.z + ABorder;
  YYYY.w := YYYY.w - ABorder;

  PrepareBuffers(glyph, scale, ABorder);
  RenderGlyph(Result);
end;

procedure TavGlyphGenerator.InvalidateProg;
begin
  FProgram.Invalidate;
end;

function TavGlyphGenerator.GlyphSegments: TavSB;
begin
  Result := FSBGlyph;
end;

function TavGlyphGenerator.GlyphTexture: TavTextureBase;
begin
  Result := FFBO.GetColor(0);
end;

procedure TavGlyphGenerator.DrawToTexture(const ATexture: IctxTexture; const ADstPos: TVec2i);
begin
//  ATexture.CopyFrom(0, ADstPos, ATexture, 0, FFBO.FrameRect);
end;

procedure TavGlyphGenerator.DrawToTexture(const ATexture: TavTextureBase; const ADstPos: TVec2i);
begin
//  ATexture.CopyFrom(FFBO.GetColor(0), ADstPos, 0, FFBO.FrameRect);
end;

procedure TavGlyphGenerator.BeginGeneration;
begin
  Assert(not FInGeneration);
  FInGeneration := True;
  FPrevProg := Main.ActiveProgram;
  FPrevFBO := Main.ActiveFrameBuffer;
  FPrevBlend := Main.States.Blending[0];
  FPrevScissor := Main.States.GetScissorTest;

  Main.States.Blending[0] := False;
  Main.States.SetScissorTest(False);
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
end;

end.

