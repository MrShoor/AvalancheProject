unit avCanvas;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, intfUtils, avBase, avRes, mutils, avTypes, avContnrs, avTess, avContext, avGlyphGenerator;

type
  { TLinePointVertex }

  TLinePointVertex = packed record
    Coords      : TVec4; //xy - start point, zw - end point
    Normals     : TVec4; //xy - normal at start point, zw - normal at end point
    Width       : TVec2; //x - real width, y - minimal width in pixels
    HintingAlign: TVec4;
    class function Layout: IDataLayout; static;
  end;
  TLinePointVertices = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TLinePointVertex>;
  ILinePointVertices = {$IfDef FPC}specialize{$EndIf} IArray<TLinePointVertex>;

  { TGlyphVertex }

  TGlyphVertex = packed record
  private
    procedure SetGlyph(const AValue: ISpriteIndex);
  public
    Pos       : TVec2;
    Align     : Single;
    Size      : TVec2;
    SDFOffset : Single;
    Color     : TVec4b;
    GlyphID   : Integer;

    _Glyph    : ISpriteIndex;
    class function Layout: IDataLayout; static;

    property Glyph: ISpriteIndex read _Glyph write SetGlyph;
  end;
  PGlyphVertex = ^TGlyphVertex;
  TGlyphVertices = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TGlyphVertex>;
  IGlyphVertices = {$IfDef FPC}specialize{$EndIf} IArray<TGlyphVertex>;

  TLineAlign = (laLeft, laCenter, laRight);
  TLineInfo = packed record
    align: TLineAlign;
    yymetrics: TVec2;
    width: Single;
  end;
  PLineInfo = ^TLineInfo;
  TLineInfoArr = {$IfDef FPC}specialize{$EndIf} TArray<TLineInfo>;
  ILineInfoArr = {$IfDef FPC}specialize{$EndIf} IArray<TLineInfo>;

  {$SCOPEDENUMS ON}
  TPenAlign = (Center, Left, Right);
  TPenHintingStyle = (Vertical, Horizontal, PostHinting);
  TPenHinting = set of TPenHintingStyle;
  {$SCOPEDENUMS OFF}

  { TPenStyle }

  TPenStyle = class (TPersistent)
  private
    FAlign: TPenAlign;
    FHinting: TPenHinting;
    FMinPixWidth: Integer;
    FWidth: Single;
    procedure SetAlign(AValue: TPenAlign);
    procedure SetHinting(AValue: TPenHinting);
    procedure SetMinPixWidth(AValue: Integer);
    procedure SetWidth(AValue: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Width: Single read FWidth write SetWidth;
    property Hinting: TPenHinting read FHinting write SetHinting;
    property Align: TPenAlign read FAlign write SetAlign;

    property MinPixWidth: Integer read FMinPixWidth write SetMinPixWidth;
  end;

  { TFontStyle }
  TGlyphStyle = (gsBold, gsItalic, gsUnderline, gsStroke);
  TGlyphStyles = set of TGlyphStyle;

  TFontStyle = class (TPersistent)
  private
    FColor: TVec4b;
    FName: string;
    FSDFOffset: Single;
    FSize: Single;
    FStyle: TGlyphStyles;
    procedure SetColor(const AValue: TVec4b);
    procedure SetName(const AValue: string);
    procedure SetSDFOffset(const AValue: Single);
    procedure SetSize(const AValue: Single);
    procedure SetStyle(const AValue: TGlyphStyles);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Name     : string       read FName      write SetName;
    property Style    : TGlyphStyles read FStyle     write SetStyle;
    property Color    : TVec4b       read FColor     write SetColor;
    property Size     : Single       read FSize      write SetSize;
    property SDFOffset: Single       read FSDFOffset write SetSDFOffset;
  end;

  { ITextLines }

  ITextLines = interface
    function GetBoundsX: TVec2;
    function GetVAlign: Single;
    procedure SetBoundsX(const AValue: TVec2);
    procedure SetVAlign(const AValue: Single);

    function LinesCount: Integer;
    function LineSize(const i: Integer): TVec2;
    function MaxLineWidth(): Single;
    function TotalHeight() : Single;

    function AllGlyphs(): IGlyphVertices;

    property BoundsX: TVec2  read GetBoundsX write SetBoundsX;
    property VAlign : Single read GetVAlign  write SetVAlign;
  end;
  TTextLinesArr = {$IfDef FPC}specialize{$EndIf} TArray<ITextLines>;
  ITextLinesArr = {$IfDef FPC}specialize{$EndIf} IArray<ITextLines>;

  { ITextBuilder }

  ITextBuilder = interface
    function  GetAlign: TLineAlign;
    function  GetFont: TFontStyle;
    procedure SetAlign(const AValue: TLineAlign);
    procedure SetFont(const AValue: TFontStyle);

    procedure Write(const AStr: string);
    procedure WriteLn(const AStr: string);

    function Finish(): ITextLines;

    property Font : TFontStyle read GetFont write SetFont;
    property Align: TLineAlign read GetAlign write SetAlign;
  end;

  { TavUIProgram }

  TavUIProgram = class (TavProgram)
  private
    FUniform_UIMatrix: TUniformField;
    FUniform_UIMatrixInverse: TUniformField;
    FUniform_ViewportSize: TUniformField;

    FUniform_Transform: TUniformField;
    FUniform_PixelToUnit: TUniformField;

    FLastViewportSize: TVec2i;
  protected
    procedure BeforeFree3D; override;
    function  DoBuild: Boolean; override;

    procedure UpdateUniforms; override;
  public
    procedure SetTransform  (const ATransform: TMat4);
    procedure SetPixelToUnit(const AScale: single);
  end;

  { TavCanvasCommonData }
  TavCanvasCommonData = class (TavRes)
  private type
    TGlyphKey = packed record
      font : string[255];
      glyph: WideChar;
      style: TGlyphStyles;
    end;
    TGlyphData = packed record
      img         : ITextureMip;
      XXXMetrics  : TVec3;
      YYYYMetrics : TVec4;
    end;
    PGlyphData = ^TGlyphData;

    TGlyphsMap = {$IfDef FPC}specialize{$EndIf}THashMap<TGlyphKey, TGlyphData>;
    IGlyphsMap = {$IfDef FPC}specialize{$EndIf}IHashMap<TGlyphKey, TGlyphData>;
  private
    FLineQuad : TavVB;
    FLineProg : TavUIProgram;
    FFontProg : TavUIProgram;

    FWndMatrix: TMat4;

    FGlyphsAtlas: TavAtlasArrayReferenced;

    FGlyphs : IGlyphsMap;

    procedure SetWndMatrix(AValue: TMat4);
  protected
    procedure AfterInit3D; override;
    procedure BeforeFree3D; override;
  public
    function GetGlyphImage(const AFontName: string; AChar: WideChar; AStyle: TGlyphStyles; out XXXMetrics: TVec3; out YYYYMetrics: TVec4): ITextureMip;
    function GetGlyphSprite(const AFontName: string; AChar: WideChar; AStyle: TGlyphStyles; out XXXMetrics: TVec3; out YYYYMetrics: TVec4): ISpriteIndex;

    property WndMatrix: TMat4 read FWndMatrix write SetWndMatrix;

    procedure ReloadShaders;
    function GlyphsAtlas: TavAtlasArrayReferenced;
    property LineQuad: TavVB        read FLineQuad;
    property LineProg: TavUIProgram read FLineProg;
    property FontProg: TavUIProgram read FFontProg;

    procedure ExportGlyphs(const AFileName: string; const AFontName: string; AStyle: TGlyphStyles; const ACharFirst, ACharLast: WideChar);

    constructor Create(AParent: TavObject); overload; override;
  end;

  { TavFontProgram }

  TavFontProgram = class (TavUIProgram)
  private
    FUniform_Atlas       : TUniformField;
    FUniform_AtlasRegions: TUniformField;
    FUniform_AtlasSize   : TUniformField;
    FCommon              : TavCanvasCommonData;
  protected
    procedure AfterInit3D; override;
    procedure BeforeFree3D; override;
    function  DoBuild: Boolean; override;

    procedure UpdateUniforms; override;
  end;

  { TavCanvas }

  TavCanvas = class(TavMainRenderChild)
  private type
    TGeometryKind = (gkUnknown, gkLines, gkFont);
    TGeometryBatch = packed record
      kind  : TGeometryKind;
      ranges: TVec2i;
    end;
    TGeometry = {$IfDef FPC}specialize{$EndIf}TArray<TGeometryBatch>;
    IGeometry = {$IfDef FPC}specialize{$EndIf}IArray<TGeometryBatch>;

    TVec2iArray = {$IfDef FPC}specialize{$EndIf}TArray<TVec2i>;
    IVec2iArray = {$IfDef FPC}specialize{$EndIf}IArray<TVec2i>;
  private
    FCommonData: IWeakRef;
    function GetCommonData: TavCanvasCommonData;
    property CommonData: TavCanvasCommonData read GetCommonData;
  private
    FPen : TPenStyle;
    FFont: TFontStyle;

    FLineData: ILinePointVertices;
    FValid   : Boolean;
    FVBLines : TavVB;

    FTextLines      : ITextLinesArr;
    FTextLineRanges : IVec2iArray;
    FGlyphsData     : IGlyphVertices;
    FVBGlyphs       : TavVB;

    FGeometryBatches: IGeometry;
    FCurrentBatch: TGeometryBatch;

    procedure SetFont(const AValue: TFontStyle);
    procedure SetPen(AValue: TPenStyle);
    procedure AddLineSegment(Coords, Normals: TVec4);
    procedure FillSegmentByPen(out Seg: TLinePointVertex);

    procedure SetValid(AValue: Boolean);
    procedure SelectGeometryKind(const AKind: TGeometryKind);
  public
    property Pen  : TPenStyle  read FPen   write SetPen;
    property Font : TFontStyle read FFont  write SetFont;
    property Valid: Boolean    read FValid write SetValid;

    function TextBuilder: ITextBuilder;

    //drawing functions
    procedure Clear;
    procedure Rectangle(Left, Top, Right, Bottom: Single); overload;
    procedure Rectangle(LeftTop, RightBottom: TVec2); overload;
    procedure Text(const AText: ITextLines);

    procedure Draw(const ATransform: TMat4; const APixelToUnit: Single);

    constructor Create(AParent: TavObject); overload; override;
    destructor Destroy; override;
  end;

function GetCanvasCommonData(const RenderMain: TavMainRender): TavCanvasCommonData;

implementation

//{$R '..\Canvas_Shaders\Canvas_Shaders.rc'}

uses Math;

const
  NAME_TavCanvasCommonData = 'TavCanvasCommonData';

  GLYPH_DefaultSize = 32;
  GLYPH_DFSize = GLYPH_DefaultSize;
  GLYPH_DFSizeInv = 1.0/GLYPH_DFSize;

type
  { TTextLines }

  TTextLines = class(TInterfacedObject, ITextLines)
  private
    FBoundsX     : TVec2;
    FGlyphs      : IGlyphVertices;
    FLines       : ILineInfoArr;
    FMaxLineWidth: Single;
    FTotalHeight : Single;
    FVAlign      : Single;
  public
    function  GetBoundsX: TVec2;
    function  GetVAlign: Single;
    procedure SetBoundsX(const AValue: TVec2);
    procedure SetVAlign(const AValue: Single);

    function LinesCount: Integer;
    function LineSize(const i: Integer): TVec2;
    function MaxLineWidth(): Single;
    function TotalHeight() : Single;

    function AllGlyphs(): IGlyphVertices;

    property BoundsX: TVec2 read GetBoundsX write SetBoundsX;
  public
    constructor Create(const AGlyphs: IGlyphVertices; const ALines: ILineInfoArr);
  end;

  { TTextBuilder }

  TTextBuilder = class (TInterfacedObject, ITextBuilder)
  private type
    TVec4Arr = {$IfDef FPC}specialize{$EndIf} TArray<TVec4>;
    IVec4Arr = {$IfDef FPC}specialize{$EndIf} IArray<TVec4>;
  private
    FCommon: TavCanvasCommonData;
    FFont  : TFontStyle;
    FAlign : TLineAlign;

    FGlyphs: IGlyphVertices;
    FLines : ILineInfoArr;

    FLineInited     : Boolean;
    FPos            : TVec2;
    FLineStart      : Integer;
    FLineInfo       : TLineInfo;
    FLineYYYYMetrics  : IVec4Arr;

    procedure WriteInternal(const AStr: UnicodeString);
    procedure WriteLnInternal;
  private
    function  GetAlign: TLineAlign;
    function  GetFont: TFontStyle;
    procedure SetAlign(const AValue: TLineAlign);
    procedure SetFont(const AValue: TFontStyle);

    procedure Write(const AStr: string);
    procedure WriteLn(const AStr: string);

    function Finish(): ITextLines;
  public
    constructor Create(const AFont: TFontStyle; const ACommon: TavCanvasCommonData);
    destructor  Destroy; override;
  end;

  { TLineQuadVertex }

  TLineQuadVertex = packed record
    quadCoord: TVec2;
    class function Layout: IDataLayout; static;
  end;
  TLineQuadVertices = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TLineQuadVertex>;
  ILineQuadVertices = {$IfDef FPC}specialize{$EndIf} IArray<TLineQuadVertex>;

function GetCanvasCommonData(const RenderMain: TavMainRender): TavCanvasCommonData;
begin
  Result := TavCanvasCommonData(RenderMain.FindChild(NAME_TavCanvasCommonData));
  if Result = nil then
  begin
    Result := TavCanvasCommonData.Create(RenderMain);
    Result.Name := NAME_TavCanvasCommonData;
  end;
end;

{ TavFontProgram }

procedure TavFontProgram.AfterInit3D;
begin
  inherited AfterInit3D;
  FCommon := GetCanvasCommonData(Main);
end;

procedure TavFontProgram.BeforeFree3D;
begin
  inherited BeforeFree3D;
  FUniform_Atlas       := nil;
  FUniform_AtlasRegions:= nil;
  FUniform_AtlasSize:= nil;
end;

function TavFontProgram.DoBuild: Boolean;
begin
  Result := inherited DoBuild;
  if Result then
  begin
    FUniform_Atlas := GetUniformField('Atlas');
    FUniform_AtlasRegions := GetUniformField('AtlasRegions');
    FUniform_AtlasSize := GetUniformField('AtlasSize');
  end;
end;

procedure TavFontProgram.UpdateUniforms;
begin
  inherited UpdateUniforms;
  SetUniform(FUniform_Atlas, FCommon.GlyphsAtlas, Sampler_Linear);
  SetUniform(FUniform_AtlasRegions, FCommon.GlyphsAtlas.RegionsVB);
  SetUniform(FUniform_AtlasSize, FCommon.GlyphsAtlas.Size);
end;

{ TavUIProgram }

procedure TavUIProgram.BeforeFree3D;
begin
  inherited BeforeFree3D;
  FUniform_UIMatrix := nil;
  FUniform_UIMatrixInverse := nil;
  FUniform_ViewportSize := nil;
  FUniform_PixelToUnit := nil;
end;

function TavUIProgram.DoBuild: Boolean;
begin
  Result := inherited DoBuild;
  if Result then
  begin
    FUniform_UIMatrix := GetUniformField('UIMatrix');
    FUniform_UIMatrixInverse := GetUniformField('UIMatrixInverse');
    FUniform_ViewportSize := GetUniformField('ViewPortSize');
    FUniform_PixelToUnit := GetUniformField('PixelToUnit');
    FUniform_Transform := GetUniformField('Transform');
    FLastViewportSize := Vec(0,0);
  end;
end;

procedure TavUIProgram.UpdateUniforms;
var currViewportSize: TVec2i;
    m: TMat4;
begin
  currViewportSize := Main.States.Viewport.Size;
  if FLastViewportSize <> currViewportSize then
  begin
    FLastViewportSize := currViewportSize;
    m := GetUIMatrix(FLastViewportSize.x, FLastViewportSize.y);
    SetUniform(FUniform_UIMatrix, m);
    SetUniform(FUniform_UIMatrixInverse, Inv(m));
    SetUniform(FUniform_ViewportSize, FLastViewportSize*1.0);
  end;
end;

procedure TavUIProgram.SetTransform(const ATransform: TMat4);
begin
  SetUniform(FUniform_Transform, ATransform);
end;

procedure TavUIProgram.SetPixelToUnit(const AScale: single);
begin
  SetUniform(FUniform_PixelToUnit, AScale);
end;

{ TGlyphVertexGPU }

procedure TGlyphVertex.SetGlyph(const AValue: ISpriteIndex);
begin
  if _Glyph = AValue then Exit;
  _Glyph := AValue;
  GlyphID := _Glyph.Index;
end;

class function TGlyphVertex.Layout: IDataLayout;
begin
  Result := LB.Add('Pos', ctFloat, 2)
              .Add('Align', ctFloat, 1)
              .Add('Size', ctFloat, 2)
              .Add('SDFOffset', ctFloat, 1)
              .Add('Color', ctUByte, 4, true)
              .Add('GlyphID', ctUInt, 1)
              .Finish(SizeOf(TGlyphVertex));
end;

{ TTextBuilder }

procedure TTextBuilder.WriteInternal(const AStr: UnicodeString);
  procedure ScaleMetrics(var xxx: TVec3; var yyyy: TVec4);
  begin
    xxx := xxx * GLYPH_DFSizeInv * FFont.Size;
    yyyy := yyyy * GLYPH_DFSizeInv * FFont.Size;
  end;
var ch: WideChar;
    xxx: TVec3;
    yyyy: TVec4;
    glyph: PGlyphVertex;
    dummy: TGlyphVertex;
    i: Integer;
begin
  if not FLineInited then
  begin
    FLineInited := True;
    FLineStart := FGlyphs.Count;
    FLineInfo.align := FAlign;
    FLineInfo.yymetrics := Vec(0,0);
    FLineInfo.width := 0;
  end;

  for i := 1 to Length(AStr) do
  begin
    ch := AStr[i];
    glyph := PGlyphVertex( FGlyphs.PItem[FGlyphs.Add(dummy)] );
    glyph^.Glyph := FCommon.GetGlyphSprite(FFont.Name, ch, FFont.Style, xxx, yyyy);
    ScaleMetrics(xxx, yyyy);

    FLineYYYYMetrics.Add(yyyy);
    FLineInfo.width := FLineInfo.width + xxx.x + xxx.y + xxx.z;

    FPos.x := FPos.x + xxx.x + xxx.y*0.5;
    glyph^.Pos.x := FPos.x;
    FPos.x := FPos.x + xxx.y*0.5 + xxx.z;

    glyph^.Size.x := xxx.y;
    glyph^.Size.y := yyyy.y + yyyy.z;

    glyph^.SDFOffset := 0;
    glyph^.Color := FFont.Color;

    FLineInfo.yymetrics := Max(FLineInfo.yymetrics, Vec(yyyy.x + yyyy.y, yyyy.z + yyyy.w));
  end;
end;

procedure TTextBuilder.WriteLnInternal;
var glyph: PGlyphVertex;
    i: Integer;
begin
  if not FLineInited then
  begin
    FPos.y := FPos.y + FFont.Size;
  end
  else
  begin
    glyph := PGlyphVertex( FGlyphs.PItem[FLineStart] );
    for i := 0 to FLineYYYYMetrics.Count - 1 do
    begin
      case FLineInfo.align of
        laLeft : glyph^.Align := 0;
        laCenter :
          begin
            glyph^.Pos.x := glyph^.Pos.x - FLineInfo.width * 0.5;
            glyph^.Align := 0.5;
          end;
        laRight  :
          begin
            glyph^.Pos.x := glyph^.Pos.x - FLineInfo.width;
            glyph^.Align := 1;
          end;
      end;
      glyph^.Pos.y := FPos.y + FLineInfo.yymetrics.x - FLineYYYYMetrics[i].y + glyph^.Size.y * 0.5;
      Inc(glyph);
    end;
    FLines.Add(FLineInfo);
    FLineYYYYMetrics.Clear();
  end;
  FPos.x := 0;
  FPos.y := FPos.y + FLineInfo.yymetrics.x + FLineInfo.yymetrics.y;
  FLineInited := False;
end;

function TTextBuilder.GetAlign: TLineAlign;
begin
  Result := FAlign;
end;

function TTextBuilder.GetFont: TFontStyle;
begin
  Result := FFont;
end;

procedure TTextBuilder.SetAlign(const AValue: TLineAlign);
begin
  FAlign := AValue;
end;

procedure TTextBuilder.SetFont(const AValue: TFontStyle);
begin
  FFont.Assign(AValue);
end;

procedure TTextBuilder.Write(const AStr: string);
begin
  WriteInternal(UnicodeString(AStr));
end;

procedure TTextBuilder.WriteLn(const AStr: string);
begin
  WriteInternal(UnicodeString(AStr));
  WriteLnInternal;
end;

function TTextBuilder.Finish: ITextLines;
begin
  Result := TTextLines.Create(FGlyphs, FLines);

  FGlyphs := TGlyphVertices.Create();
  FLines := TLineInfoArr.Create();
  FLineInited := False;
  FLineStart := 0;
  FLineYYYYMetrics := TVec4Arr.Create();
end;

constructor TTextBuilder.Create(const AFont: TFontStyle; const ACommon: TavCanvasCommonData);
begin
  FCommon := ACommon;
  FFont := AFont;
  FGlyphs := TGlyphVertices.Create();
  FLines := TLineInfoArr.Create();
  FLineInited := False;
  FLineStart := 0;
  FLineYYYYMetrics := TVec4Arr.Create();
end;

destructor TTextBuilder.Destroy;
begin
  inherited Destroy;

end;

{ TTextLines }

function TTextLines.GetBoundsX: TVec2;
begin
  Result := FBoundsX;
end;

function TTextLines.GetVAlign: Single;
begin
  Result := FVAlign;
end;

procedure TTextLines.SetBoundsX(const AValue: TVec2);
begin
  FBoundsX := AValue;
end;

procedure TTextLines.SetVAlign(const AValue: Single);
begin
  FVAlign := AValue;
end;

function TTextLines.LinesCount: Integer;
begin
  Result := FLines.Count;
end;

function TTextLines.LineSize(const i: Integer): TVec2;
var pl: PLineInfo;
begin
  pl := PLineInfo( FLines.PItem[i] );
  Result.x := pl^.width;
  Result.y := pl^.yymetrics.x + pl^.yymetrics.y;
end;

function TTextLines.MaxLineWidth: Single;
begin
  Result := FMaxLineWidth;
end;

function TTextLines.TotalHeight: Single;
begin
  Result := FTotalHeight;
end;

function TTextLines.AllGlyphs: IGlyphVertices;
begin
  Result := FGlyphs;
end;

constructor TTextLines.Create(const AGlyphs: IGlyphVertices; const ALines: ILineInfoArr);
var i: Integer;
    pl: PLineInfo;
begin
  FGlyphs := AGlyphs;
  FLines := ALines;

  FMaxLineWidth := 0;
  FTotalHeight := 0;
  for i := 0 to FLines.Count - 1 do
  begin
    pl := FLines.PItem[i];
    FMaxLineWidth := Max(FMaxLineWidth, pl^.width);
    FTotalHeight := FTotalHeight + pl^.yymetrics.x + pl^.yymetrics.y;
  end;
  FBoundsX.x := -0.5 * FMaxLineWidth;
  FBoundsX.y :=  0.5 * FMaxLineWidth;
end;

{ TFontStyle }

procedure TFontStyle.SetName(const AValue: string);
begin
  if FName = AValue then Exit;
  FName := AValue;
end;

procedure TFontStyle.SetSDFOffset(const AValue: Single);
begin
  if FSDFOffset = AValue then Exit;
  FSDFOffset := AValue;
end;

procedure TFontStyle.SetSize(const AValue: Single);
begin
  if FSize = AValue then Exit;
  FSize := AValue;
end;

procedure TFontStyle.SetColor(const AValue: TVec4b);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
end;

procedure TFontStyle.SetStyle(const AValue: TGlyphStyles);
begin
  if FStyle = AValue then Exit;
  FStyle := AValue;
end;

procedure TFontStyle.AssignTo(Dest: TPersistent);
var FontDest: TFontStyle absolute Dest;
begin
  Assert(Dest is TFontStyle);
  FontDest.FColor := FColor;
  FontDest.FName := FName;
  FontDest.FSDFOffset := FSDFOffset;
  FontDest.FSize := FSize;
  FontDest.FStyle := FStyle;
end;

{ TLinePointVertex }

class function TLinePointVertex.Layout: IDataLayout;
begin
  Result := LB.Add('Coords', ctFloat, 4).
               Add('Normals', ctFloat, 4).
               Add('Width', ctFloat, 2).
               Add('HintingAlign', ctFloat, 4).
               Finish(SizeOf(TLinePointVertex));
end;

{ TLineQuadVertex }

class function TLineQuadVertex.Layout: IDataLayout;
begin
  Result := LB.Add('quadCoord', ctFloat, 2).Finish(SizeOf(TLineQuadVertex));
end;

{ TavCanvasCommonData }

procedure TavCanvasCommonData.SetWndMatrix(AValue: TMat4);
begin
  if FWndMatrix = AValue then Exit;
  FWndMatrix := AValue;
end;

procedure TavCanvasCommonData.AfterInit3D;
begin
  inherited AfterInit3D;
  ReloadShaders;
end;

procedure TavCanvasCommonData.BeforeFree3D;
begin
  inherited BeforeFree3D;
end;

function TavCanvasCommonData.GetGlyphImage(const AFontName: string;
  AChar: WideChar; AStyle: TGlyphStyles; out XXXMetrics: TVec3; out
  YYYYMetrics: TVec4): ITextureMip;
var key  : TGlyphKey;
    pdata: PGlyphData;
    data : TGlyphData;
begin
  ZeroClear(key, SizeOf(key));
  key.font := AFontName;
  key.glyph := AChar;
  key.style := AStyle;
  if not FGlyphs.TryGetPValue(key, Pointer(pdata)) then
  begin
    data.img := GenerateGlyphSDF(AFontName, AChar, GLYPH_DefaultSize, GLYPH_DefaultSize div 4, gsItalic in AStyle, gsBold in AStyle, gsUnderline in AStyle, data.XXXMetrics, data.YYYYMetrics);
    FGlyphs.Add(key, data);
    pdata := @data;
  end;
  Result := pdata^.img;
  XXXMetrics := pdata^.XXXMetrics;
  YYYYMetrics := pdata^.YYYYMetrics;
end;

function TavCanvasCommonData.GetGlyphSprite(const AFontName: string;
  AChar: WideChar; AStyle: TGlyphStyles; out XXXMetrics: TVec3; out
  YYYYMetrics: TVec4): ISpriteIndex;
begin
  Result := FGlyphsAtlas.ObtainSprite(GetGlyphImage(AFontName, AChar, AStyle, XXXMetrics, YYYYMetrics));
end;

procedure TavCanvasCommonData.ReloadShaders;
const LOADFROMRES = False;
      DIR = 'D:\Projects\AvalancheProject\Canvas_Shaders\!Out\';
      //DIR = 'C:\MyProj\AvalancheProject\Canvas_Shaders\!Out\';
begin
  FLineProg.Load('CanvasLine', LOADFROMRES, DIR);
  FFontProg.Load('CanvasFont', LOADFROMRES, DIR);
end;

function TavCanvasCommonData.GlyphsAtlas: TavAtlasArrayReferenced;
begin
  Result := FGlyphsAtlas;
end;

procedure TavCanvasCommonData.ExportGlyphs(const AFileName: string; const AFontName: string; AStyle: TGlyphStyles; const ACharFirst, ACharLast: WideChar);
  type
    TExportInformation = record
      ch    : WideChar;
      XXX   : TVec3;
      YYYY  : TVec4;
      sprite: ISpriteIndex;
    end;
    procedure WriteGlyph(const AExp: TExportInformation; const AStream: TStream);
    var region : TSpriteRegion;
        picture: ITextureMip;
    begin
      AStream.WriteBuffer(AExp.ch, SizeOf(AExp.ch));
      AStream.WriteBuffer(AExp.XXX, SizeOf(AExp.XXX));
      AStream.WriteBuffer(AExp.YYYY, SizeOf(AExp.YYYY));
      region := GlyphsAtlas.GetRegion(AExp.sprite.Index);
      AStream.WriteBuffer(region, SizeOf(region));
      picture := AExp.sprite.Data;
      Assert(picture.Width = region.Rect.z - region.Rect.x);
      Assert(picture.Height = region.Rect.w - region.Rect.y);
      if picture.Width * picture.Height > 0 then
        AStream.WriteBuffer(picture.Data^, picture.Width*picture.Height*SizeOf(Single));
    end;

var fs    : TFileStream;
    char  : WideChar;
    n, i  : Integer;
    exp   : array of TExportInformation;

begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    n := Ord(ACharLast) - Ord(ACharFirst) + 1;
    SetLength(exp, n);
    n := 0;
    for char := ACharLast downto ACharFirst do
    begin
      exp[n].ch := char;
      exp[n].sprite := GetGlyphSprite(AFontName, char, AStyle, exp[n].XXX, exp[n].YYYY);
      Inc(n);
    end;

    fs.WriteBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
      WriteGlyph(exp[i], fs);
  finally
    FreeAndNil(fs);
  end;
end;

constructor TavCanvasCommonData.Create(AParent: TavObject);
var Vert: ILineQuadVertices;
    V: TLineQuadVertex;
begin
  inherited Create(AParent);
  FLineQuad := TavVB.Create(Self);
  FLineProg := TavUIProgram.Create(Self);
  FFontProg := TavFontProgram.Create(Self);

  Vert := TLineQuadVertices.Create;
  V.quadCoord := Vec(0.0, -1.0); Vert.Add(V);
  V.quadCoord := Vec(0.0,  1.0); Vert.Add(V);
  V.quadCoord := Vec(1.0, -1.0); Vert.Add(V);
  V.quadCoord := Vec(1.0,  1.0); Vert.Add(V);
  FLineQuad.Vertices := Vert as IVerticesData;
  FLineQuad.PrimType := ptTriangleStrip;

  FGlyphs := TGlyphsMap.Create();
  FGlyphsAtlas := TavAtlasArrayReferenced.Create(Self);
  FGlyphsAtlas.TargetFormat := TTextureFormat.R32f;
end;

{ TavCanvas }

function TavCanvas.GetCommonData: TavCanvasCommonData;
begin
  if FCommonData = nil then
      FCommonData := GetCanvasCommonData(Main).WeakRef;
  Result := TavCanvasCommonData(FCommonData.Obj);
end;

procedure TavCanvas.SetPen(AValue: TPenStyle);
begin
  if FPen = AValue then Exit;
  FPen.Assign(AValue);
end;

procedure TavCanvas.SetFont(const AValue: TFontStyle);
begin
  if FFont = AValue then Exit;
  FFont.Assign(AValue);
end;

procedure TavCanvas.AddLineSegment(Coords, Normals: TVec4);
var Seg: TLinePointVertex;
begin
  FillSegmentByPen(Seg);
  Seg.Coords := Coords;
  Seg.Normals := Normals;
  FLineData.Add(Seg);
  FVBLines.Invalidate;
end;

procedure TavCanvas.FillSegmentByPen(out Seg: TLinePointVertex);
begin
  Seg.Width := Vec(Pen.Width, Pen.MinPixWidth);
  if TPenHintingStyle.Horizontal in Pen.Hinting then
    Seg.HintingAlign.x := 1
  else
    Seg.HintingAlign.x := 0;

  if TPenHintingStyle.Vertical in Pen.Hinting then
    Seg.HintingAlign.y := 1
  else
    Seg.HintingAlign.y := 0;

  if TPenHintingStyle.PostHinting in Pen.Hinting then
    Seg.HintingAlign.z := 1
  else
    Seg.HintingAlign.z := 0;

  case Pen.Align of
    TPenAlign.Center: Seg.HintingAlign.w := 0;
    TPenAlign.Left  : Seg.HintingAlign.w := 1;
    TPenAlign.Right : Seg.HintingAlign.w := -1;
  end;
end;

procedure TavCanvas.SetValid(AValue: Boolean);
begin
  if FValid = AValue then Exit;
  FValid := AValue;
end;

procedure TavCanvas.SelectGeometryKind(const AKind: TGeometryKind);

  function GetRangeEnd(const AKind: TGeometryKind): Integer;
  begin
    case AKind of
      gkUnknown: Exit(0);
      gkLines  : Exit(FLineData.Count);
      gkFont   : Exit(FTextLines.Count);
    else
      Result := 0;
    end;
  end;

begin
  if AKind = FCurrentBatch.kind then Exit;

  if FCurrentBatch.kind <> gkUnknown then
  begin
    FCurrentBatch.ranges.y := GetRangeEnd(FCurrentBatch.kind) - FCurrentBatch.ranges.x;
    FGeometryBatches.Add(FCurrentBatch);
  end;
  FCurrentBatch.kind := AKind;
  FCurrentBatch.ranges.x := GetRangeEnd(FCurrentBatch.kind);
  FCurrentBatch.ranges.y := 0;
end;

function TavCanvas.TextBuilder: ITextBuilder;
begin
  Result := TTextBuilder.Create(FFont, CommonData);
end;

procedure TavCanvas.Clear;
begin
  FLineData.Clear();
  FVBLines.Invalidate;

  FTextLineRanges.Clear();
  FGlyphsData.Clear();
  FVBGlyphs.Invalidate;

  SelectGeometryKind(gkUnknown);
  FGeometryBatches.Clear();
end;

procedure TavCanvas.Rectangle(Left, Top, Right, Bottom: Single);
var Seg: TLinePointVertex;
begin
  SelectGeometryKind(gkLines);

  FillSegmentByPen(Seg);

  Seg.Coords.xy := Vec(Left, Top);
  Seg.Coords.zw := Vec(Right, Top);
  Seg.Normals.xy := (Vec(1.0, 1.0));
  Seg.Normals.zw := (Vec(-1.0, 1.0));
  FLineData.Add(Seg);

  Seg.Coords.xy := Seg.Coords.zw;
  Seg.Normals.xy := Seg.Normals.zw;
  Seg.Coords.zw := Vec(Right, Bottom);
  Seg.Normals.zw := (Vec(-1.0, -1.0));
  FLineData.Add(Seg);

  Seg.Coords.xy := Seg.Coords.zw;
  Seg.Normals.xy := Seg.Normals.zw;
  Seg.Coords.zw := Vec(Left, Bottom);
  Seg.Normals.zw := (Vec(1.0, -1.0));
  FLineData.Add(Seg);

  Seg.Coords.xy := Seg.Coords.zw;
  Seg.Normals.xy := Seg.Normals.zw;
  Seg.Coords.zw := Vec(Left, Top);
  Seg.Normals.zw := (Vec(1.0, 1.0));
  FLineData.Add(Seg);

  FVBLines.Invalidate;
end;

procedure TavCanvas.Rectangle(LeftTop, RightBottom: TVec2);
begin
  Rectangle(LeftTop.x, LeftTop.y, RightBottom.x, RightBottom.y);
end;

procedure TavCanvas.Text(const AText: ITextLines);
var range: TVec2i;
begin
  SelectGeometryKind(gkFont);

  FTextLines.Add(AText);
  range.x := FGlyphsData.Count;
  FGlyphsData.AddArray(AText.AllGlyphs());
  range.y := FGlyphsData.Count - range.x;
  FTextLineRanges.Add(range);
end;

procedure TavCanvas.Draw(const ATransform: TMat4; const APixelToUnit: Single);

  function InitProg(const AProg: TavUIProgram): Boolean;
  begin
    AProg.SetTransform(ATransform);
    AProg.SetPixelToUnit(APixelToUnit);
    Result := True;
  end;

var prog: TavUIProgram;
    batch: TGeometryBatch;
    gk: TGeometryKind;
    progInited: array [TGeometryKind] of Boolean;
    i, j: Integer;
    textRange: TVec2i;
begin
  SelectGeometryKind(gkUnknown);

  for gk := Low(TGeometryKind) to High(TGeometryKind) do progInited[gk] := False;

  for i := 0 to FGeometryBatches.Count - 1 do
  begin
    batch := FGeometryBatches[i];
    if batch.kind = gkUnknown then Continue;
    if batch.ranges.y = 0 then Continue;

    case batch.kind of
      gkLines:
        begin
          prog := CommonData.LineProg;
          prog.Select;
          prog.SetAttributes(CommonData.LineQuad, nil, FVBLines);
          if not progInited[batch.kind] then
            progInited[batch.kind] := InitProg(prog);
          prog.Draw(batch.ranges.y, 0, -1, 0, batch.ranges.x);
        end;
      gkFont:
        begin
          if FVBGlyphs.Vertices.VerticesCount = 0 then Continue;

          prog := CommonData.FontProg;
          prog.Select;
          prog.SetAttributes(nil, nil, FVBGlyphs);
          if not progInited[batch.kind] then
            progInited[batch.kind] := InitProg(prog);

          for j := batch.ranges.x to batch.ranges.x + batch.ranges.y - 1 do
          begin
            textRange := FTextLineRanges[j];
            prog.Draw(ptTriangleStrip, cmNone, false, textRange.y, 0, 4, 0, textRange.x);
          end;
        end;
    end;
  end;
end;

constructor TavCanvas.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  FGeometryBatches := TGeometry.Create;

  FPen := TPenStyle.Create;
  FPen.Width := 0.1;

  FFont:= TFontStyle.Create;
  FFont.Name := 'Segoe UI';
  FFont.Size := 14;
  FFont.Color := Vec(255, 255, 255, 255);

  FLineData := TLinePointVertices.Create;
  FVBLines := TavVB.Create(CommonData);
  FVBLines.Vertices := FLineData as IVerticesData;

  FTextLines := TTextLinesArr.Create();
  FGlyphsData:= TGlyphVertices.Create();
  FTextLineRanges := TVec2iArray.Create();
  FVBGlyphs := TavVB.Create(CommonData);
  FVBGlyphs.Vertices := FGlyphsData as IVerticesData;

  FCurrentBatch.kind := gkUnknown;
end;

destructor TavCanvas.Destroy;
begin
  FreeAndNil(FPen);
  FreeAndNil(FFont);
  inherited Destroy;
end;

{ TPenStyle }

procedure TPenStyle.SetWidth(AValue: Single);
begin
  if FWidth = AValue then Exit;
  FWidth := AValue;
end;

procedure TPenStyle.AssignTo(Dest: TPersistent);
var PenDest: TPenStyle absolute Dest;
begin
  Assert(Dest is TPenStyle);
  PenDest.FAlign := FAlign;
  PenDest.FHinting := FHinting;
  PenDest.FMinPixWidth := FMinPixWidth;
  PenDest.FWidth := FWidth;
end;

procedure TPenStyle.SetHinting(AValue: TPenHinting);
begin
  if FHinting = AValue then Exit;
  FHinting := AValue;
end;

procedure TPenStyle.SetMinPixWidth(AValue: Integer);
begin
  if FMinPixWidth = AValue then Exit;
  FMinPixWidth := AValue;
end;

procedure TPenStyle.SetAlign(AValue: TPenAlign);
begin
  if FAlign = AValue then Exit;
  FAlign := AValue;
end;

end.
