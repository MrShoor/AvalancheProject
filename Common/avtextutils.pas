unit avTextUtils;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils,
  mutils, intfUtils,
  avBase, avRes, avTess, avTypes, avContnrs, avContext, avGlyphGenerator, avGPUGlyphGenerator;

type
  IGlyphIndex = interface;

  { TGlyphVertex }

  TGlyphVertex = packed record
  private
    procedure SetGlyph(const AValue: IGlyphIndex);
  public
    Pos       : TVec2;
    Align     : Single;
    Size      : TVec2;
    SDFOffset : Single;
    Color     : TVec4;
    GlyphID   : Integer;

    _Glyph    : IGlyphIndex;
    class function Layout: IDataLayout; static;

    property Glyph: IGlyphIndex read _Glyph write SetGlyph;
  end;
  PGlyphVertex = ^TGlyphVertex;
  TGlyphVertices = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TGlyphVertex>;
  IGlyphVertices = {$IfDef FPC}specialize{$EndIf} IArray<TGlyphVertex>;

  TXXXMetrics = {$IfDef FPC}specialize{$EndIf} TArray<TVec3>;
  IXXXMetrics = {$IfDef FPC}specialize{$EndIf} IArray<TVec3>;
  TLineAlign = (laLeft, laCenter, laRight);
  TLineInfo = packed record
    align: TLineAlign;
    yymetrics: TVec2;
    width: Single;
    glyphs: TVec2i;
    XXXMetrics: IXXXMetrics;
  end;
  PLineInfo = ^TLineInfo;
  TLineInfoArr = {$IfDef FPC}specialize{$EndIf} TArray<TLineInfo>;
  ILineInfoArr = {$IfDef FPC}specialize{$EndIf} IArray<TLineInfo>;

  { TFontStyle }
  TGlyphStyle = (gsBold, gsItalic, gsUnderline, gsStroke);
  TGlyphStyles = set of TGlyphStyle;

  TFontStyle = class (TPersistent)
  private
    FColor: TVec4;
    FName: string;
    FSDFOffset: Single;
    FSize: Single;
    FStyle: TGlyphStyles;
    procedure SetColor(const AValue: TVec4);
    procedure SetName(const AValue: string);
    procedure SetSDFOffset(const AValue: Single);
    procedure SetSize(const AValue: Single);
    procedure SetStyle(const AValue: TGlyphStyles);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Name     : string       read FName      write SetName;
    property Style    : TGlyphStyles read FStyle     write SetStyle;
    property Color    : TVec4        read FColor     write SetColor;
    property Size     : Single       read FSize      write SetSize;
    property SDFOffset: Single       read FSDFOffset write SetSDFOffset;

    procedure AfterConstruction; override;
  end;

  { ITextLines }

  ITextLines = interface
    function GetBoundsX: TVec2;
    function GetBoundsY: TVec2;
    function GetClipWithBounds: Boolean;
    function GetVAlign: Single;
    function GetVScroll: Single;
    procedure SetBoundsX(const AValue: TVec2);
    procedure SetBoundsY(const AValue: TVec2);
    procedure SetClipWithBounds(AValue: Boolean);
    procedure SetVAlign(const AValue: Single);
    procedure SetVScroll(AValue: Single);

    function  GetBounds(ARowIdx: Integer; FromChar: Integer; CharCounts: Integer): TRectF;
    procedure SymbolAt(const ALocalCoord: TVec2; out ARowIdx, ACharIdx: Integer);

    function LinesCount: Integer;
    function LineSize(const i: Integer): TVec2;
    function LineGlyphs(const i: Integer): TVec2i;
    function MaxLineWidth(): Single;
    function TotalHeight() : Single;

    function AllGlyphs(): IGlyphVertices;
    function BoundsXY(): TVec4;

    property BoundsX: TVec2  read GetBoundsX write SetBoundsX;
    property BoundsY: TVec2  read GetBoundsY write SetBoundsY;
    property VAlign : Single read GetVAlign  write SetVAlign;
    property VScroll: Single read GetVScroll write SetVScroll;
    property ClipWithBounds: Boolean read GetClipWithBounds write SetClipWithBounds;
  end;
  TTextLinesArr = {$IfDef FPC}specialize{$EndIf} TArray<ITextLines>;
  ITextLinesArr = {$IfDef FPC}specialize{$EndIf} IArray<ITextLines>;

  { ITextBuilder }

  ITextBuilder = interface
    function  GetAlign: TLineAlign;
    function  GetFont: TFontStyle;
    procedure SetAlign(const AValue: TLineAlign);
    procedure SetFont(const AValue: TFontStyle);

    procedure WriteSpace(ASpace: Single);
    procedure Write(const AStr: string);
    procedure WriteLn(const AStr: string);
    procedure WriteMultiline(const AStr: string);
    procedure WriteWrapped(const AStr: string);
    procedure WriteWrappedEnd(const AMaxWidth: Single; AJustifyAlign: Boolean = False; AFirstRowOffset: Single = 0; ANextRowsOffset: Single = 0);
    procedure WriteWrappedMultiline(const AStr: string; const AMaxWidth: Single; AJustifyAlign: Boolean = False; AFirstRowOffset: Single = 0; ANextRowsOffset: Single = 0);

    function Finish(): ITextLines;

    property Font : TFontStyle read GetFont write SetFont;
    property Align: TLineAlign read GetAlign write SetAlign;
  end;

  TavGlyphAtlas = class;

  IGlyphIndex = interface
    function Atlas: TavGlyphAtlas;
    function Index: Integer;
    function Size : TVec2i;
    function XXXMetrics: TVec3;
    function YYYYMetrics: TVec4;
    function XXXMetricsScaled(AFontSize: Single): TVec3;
    function YYYYMetricsScaled(AFontSize: Single): TVec4;
  end;
  IGlyphIndexArr = {$IfDef FPC}specialize{$EndIf} IArray<IGlyphIndex>;
  TGlyphIndexArr = {$IfDef FPC}specialize{$EndIf} TArray<IGlyphIndex>;
  IGlyphIndexSet = {$IfDef FPC}specialize{$EndIf} IHashSet<IGlyphIndex>;
  TGlyphIndexSet = {$IfDef FPC}specialize{$EndIf} THashSet<IGlyphIndex>;

  { TavGlyphAtlas }

  TavGlyphAtlas = class(TavTextureBase)
  private type
    TGlyphData = packed record
      poly   : IGlyphPoly;
      ImgSize: TVec2i;
      Border : Integer;
      XXX    : TVec3;
      YYYY   : TVec4;
    end;

    TGlyphKey = packed record
      font  : string[255];
      glyph : WideChar;
      style : TGlyphStyles;
      size  : Integer;
      border: Integer;
      function GenData: TGlyphData;
    end;

    { TGlyphIndex }

    TGlyphIndex = class(TInterfacedObject, IGlyphIndex)
    private
      FOwner: TavGlyphAtlas;
      FKey  : TGlyphKey;
      FData : TGlyphData;
      FIndex: Integer;
      FSlice: Integer;
      FQuad : IQuadRange;
      function Atlas: TavGlyphAtlas;
      function Index: Integer;
      function Size : TVec2i;
      function XXXMetrics: TVec3;
      function YYYYMetrics: TVec4;
      function XXXMetricsScaled(AFontSize: Single): TVec3;
      function YYYYMetricsScaled(AFontSize: Single): TVec4;
    public
      constructor Create(const AOwner: TavGlyphAtlas; const AKey: TGlyphKey; const AData: TGlyphData; const AIndex: Integer; const ASlice: Integer; const AQuad: IQuadRange);
      destructor Destroy; override;
    end;

    ISpriteMap = {$IfDef FPC}specialize{$EndIf} IHashMap<TGlyphKey, TGlyphIndex>;
    TSpriteMap = {$IfDef FPC}specialize{$EndIf} THashMap<TGlyphKey, TGlyphIndex>;

    ISpriteList = {$IfDef FPC}specialize{$EndIf} IArray<TGlyphIndex>;
    TSpriteList = {$IfDef FPC}specialize{$EndIf} TArray<TGlyphIndex>;

    TPageInfo = record
      QManager: IQuadManager;
      InvalidSprites: ISpriteList;
    end;
    PPageInfo = ^TPageInfo;

    IPages = {$IfDef FPC}specialize{$EndIf} IArray<TPageInfo>;
    TPages = {$IfDef FPC}specialize{$EndIf} TArray<TPageInfo>;

    IRegions = {$IfDef FPC}specialize{$EndIf} IArray<TSpriteRegion>;
    TRegions = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TSpriteRegion>;

    IFreeIndices = {$IfDef FPC}specialize{$EndIf} IArray<Integer>;
    TFreeIndices = {$IfDef FPC}specialize{$EndIf} TArray<Integer>;
  private
    FSprites    : ISpriteMap;
    FSpriteList : ISpriteList;
    FFreeIndices: IFreeIndices;

    FPages : IPages;
    FInvalidPagesCount : Boolean;

    FRegions: IRegions;
    FRegionsBuffer: TavSB;

    FTargetSize: TVec2i;

    function  AllocIndex: Integer;
    procedure AllocQuad(const ASize: TVec2i; out ARange: IQuadRange; out ASlice: Integer);
    procedure SetTargetSize(const AValue: TVec2i);
    procedure FullRebuild;
  protected
    function DoBuild: Boolean; override;
    procedure DoOnFrameStart; override;
  public
    procedure Build; override;
    procedure Invalidate; override;
    property RegionsVB: TavSB read FRegionsBuffer;

    procedure CleanUnused;

    property TargetSize: TVec2i read FTargetSize write SetTargetSize;

    function ObtainGlyph(const AFontName: string; AChar: WideChar; AStyle: TGlyphStyles): IGlyphIndex;
    function GetRegion(AIndex: Integer): TSpriteRegion;

    procedure AfterConstruction; override;
    destructor Destroy; override;
  end;

function Create_ITextBuilder(const AFont: TFontStyle; const AGlyphs: TavGlyphAtlas): ITextBuilder;

implementation

uses Math;

type
  { TTextLines }

  TTextLines = class(TInterfacedObject, ITextLines)
  private
    FBoundsX       : TVec2;
    FBoundsY       : TVec2;
    FGlyphs        : IGlyphVertices;
    FLines         : ILineInfoArr;
    FMaxLineWidth  : Single;
    FTotalHeight   : Single;
    FVAlign        : Single;
    FVScroll       : Single;
    FClipWithBounds: Boolean;
  public
    function GetBoundsX: TVec2;
    function GetBoundsY: TVec2;
    function GetClipWithBounds: Boolean;
    function GetVAlign: Single;
    function GetVScroll: Single;
    procedure SetBoundsX(const AValue: TVec2);
    procedure SetBoundsY(const AValue: TVec2);
    procedure SetClipWithBounds(AValue: Boolean);
    procedure SetVAlign(const AValue: Single);
    procedure SetVScroll(AValue: Single);

    function  GetBounds(ARowIdx: Integer; FromChar: Integer; CharCounts: Integer): TRectF;
    procedure SymbolAt(const ALocalCoord: TVec2; out ARowIdx, ACharIdx: Integer);

    function LinesCount: Integer;
    function LineSize(const i: Integer): TVec2;
    function LineGlyphs(const i: Integer): TVec2i;
    function MaxLineWidth(): Single;
    function TotalHeight() : Single;

    function AllGlyphs(): IGlyphVertices;
    function BoundsXY(): TVec4;

    property BoundsX: TVec2 read GetBoundsX write SetBoundsX;
    property BoundsY: TVec2 read GetBoundsY write SetBoundsY;
  public
    constructor Create(const AGlyphs: IGlyphVertices; const ALines: ILineInfoArr);
  end;

  { TTextBuilder }

  TTextBuilder = class (TInterfacedObject, ITextBuilder)
  private type
    TVec4Arr = {$IfDef FPC}specialize{$EndIf} TArray<TVec4>;
    IVec4Arr = {$IfDef FPC}specialize{$EndIf} IArray<TVec4>;

    TWordInfo = record
      Glyphs     : IGlyphVertices;
      XXXMetrics : IXXXMetrics;
      YYYYMetrics: IVec4Arr;
      XXXSpace   : TVec3;

      Width      : Single;
      YYMetrics  : TVec2;
    end;
    PWordInfo = ^TWordInfo;

    IWordsArr = {$IfDef FPC}specialize{$EndIf} IArray<TWordInfo>;
    TWordsArr = {$IfDef FPC}specialize{$EndIf} TArray<TWordInfo>;
  private
    FAtlas : TavGlyphAtlas;
    FFont  : TFontStyle;
    FAlign : TLineAlign;

    FGlyphs: IGlyphVertices;
    FLines : ILineInfoArr;

    FLineInited     : Boolean;
    FPos            : TVec2;
    FLineStart      : Integer;
    FLineInfo       : TLineInfo;
    FLineYYYYMetrics: IVec4Arr;

    FWrappedWords: IWordsArr;

    procedure InitLine;
    function  CalcBounds(const AStr: UnicodeString): TVec2;
    procedure WriteWordInternal(const AStr: UnicodeString);
    procedure WriteInternal(const AStr: UnicodeString);
    procedure WriteLnInternal;
  private
    function  GetAlign: TLineAlign;
    function  GetFont: TFontStyle;
    procedure SetAlign(const AValue: TLineAlign);
    procedure SetFont(const AValue: TFontStyle);

    procedure WriteSpace(ASpace: Single);
    procedure Write(const AStr: string);
    procedure WriteLn(const AStr: string);
    procedure WriteMultiline(const AStr: string);
    procedure WriteWrapped(const AStr: string);
    procedure WriteWrappedEnd(const AMaxWidth: Single; AJustifyAlign: Boolean = False; AFirstRowOffset: Single = 0; ANextRowsOffset: Single = 0);
    procedure WriteWrappedMultiline(const AStr: string; const AMaxWidth: Single; AJustifyAlign: Boolean = False; AFirstRowOffset: Single = 0; ANextRowsOffset: Single = 0);

    function Finish(): ITextLines;
  public
    constructor Create(const AFont: TFontStyle; const AAtlas: TavGlyphAtlas);
    destructor  Destroy; override;
  end;

function Create_ITextBuilder(const AFont: TFontStyle; const AGlyphs: TavGlyphAtlas): ITextBuilder;
begin
  Result := TTextBuilder.Create(AFont, AGlyphs);
end;

{ TTextLines }

function TTextLines.GetBoundsX: TVec2;
begin
  Result := FBoundsX;
end;

function TTextLines.GetBoundsY: TVec2;
begin
  Result := FBoundsY;
end;

function TTextLines.GetClipWithBounds: Boolean;
begin
  Result := FClipWithBounds;
end;

function TTextLines.GetVAlign: Single;
begin
  Result := FVAlign;
end;

function TTextLines.GetVScroll: Single;
begin
  Result := FVScroll;
end;

procedure TTextLines.SetBoundsX(const AValue: TVec2);
begin
  FBoundsX := AValue;
end;

procedure TTextLines.SetBoundsY(const AValue: TVec2);
begin
  FBoundsY := AValue;
end;

procedure TTextLines.SetClipWithBounds(AValue: Boolean);
begin
  FClipWithBounds := AValue;
end;

procedure TTextLines.SetVAlign(const AValue: Single);
begin
  FVAlign := AValue;
end;

procedure TTextLines.SetVScroll(AValue: Single);
begin
  FVScroll := AValue;
end;

function TTextLines.GetBounds(ARowIdx: Integer; FromChar: Integer; CharCounts: Integer): TRectF;
var line: PLineInfo;
    i, n: Integer;
    HAlign: Single;
    xxx: TVec3;
    xlen: Single;
begin
  line := nil;

  Assert(ARowIdx >= 0, 'ARowIdx out of range');
  Assert(ARowIdx < FLines.Count, 'ARowIdx out of range');

  Result.min.y := Lerp(0, -FTotalHeight, FVAlign) + Lerp(BoundsY.x, BoundsY.y, FVAlign);
  for i := 0 to ARowIdx do
  begin
    line := FLines.PItem[i];
    Result.max.y := Result.min.y + line^.yymetrics.x + line^.yymetrics.y;
    if i <> ARowIdx then Result.min.y := Result.max.y;
  end;

  case line^.align of
    laLeft : HAlign := 0;
    laCenter : HAlign := 0.5;
    laRight : HAlign := 1.0;
  else
    HAlign := 0;
  end;

  Result.min.x := Lerp(0, -line^.width, HAlign) + Lerp(BoundsX.x, BoundsX.y, HAlign);
  Result.max.x := Result.min.x;
  n := 0;
  for i := line^.glyphs.x to Clamp(line^.glyphs.x + FromChar + CharCounts, line^.glyphs.x, line^.glyphs.y) - 1 do
  begin
    xxx := line^.XXXMetrics[n];
    xlen := xxx.x + xxx.y + xxx.z;
    if n < FromChar then Result.min.x := Result.min.x + xlen;
    Result.max.x := Result.max.x + xlen;
    Inc(n);
  end;
end;

procedure TTextLines.SymbolAt(const ALocalCoord: TVec2; out ARowIdx, ACharIdx: Integer);

  function FindLineIdx(): Integer;
  var ypos, ynext: Single;
      i: Integer;
      line: PLineInfo;
  begin
    ypos := Lerp(0, -FTotalHeight, FVAlign) + Lerp(BoundsY.x, BoundsY.y, FVAlign);
    for i := 0 to FLines.Count - 1 do
    begin
      line := FLines.PItem[i];
      ynext := ypos + line^.yymetrics.x + line^.yymetrics.y;
      if (ALocalCoord.y > ypos) and (ALocalCoord.y <= ynext) then Exit(i);
      ypos := ynext;
    end;
    Result := -1;
  end;

  function FindCharPos(const ALineIdx: Integer): Integer;
  var xpos: Single;
      i: Integer;
      line: PLineInfo;
      HAlign: Single;
  begin
    line := FLines.PItem[ALineIdx];

    case line^.align of
      laLeft : HAlign := 0;
      laCenter : HAlign := 0.5;
      laRight : HAlign := 1.0;
    else
      HAlign := 0;
    end;

    Result := 0;
    xpos := ALocalCoord.x - Lerp(BoundsX.x, BoundsX.y, HAlign);
    for i := line^.glyphs.x to line^.glyphs.y - 1 do
    begin
      if FGlyphs[i].Pos.x > xpos then Exit;
      Inc(Result);
    end;
  end;

begin
  ARowIdx := FindLineIdx();
  if ARowIdx < 0 then
  begin
    ACharIdx := -1;
    Exit;
  end;

  ACharIdx := FindCharPos(ARowIdx);
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

function TTextLines.LineGlyphs(const i: Integer): TVec2i;
var pl: PLineInfo;
begin
  pl := PLineInfo( FLines.PItem[i] );
  Result := pl^.glyphs;
end;

function TTextLines.MaxLineWidth(): Single;
begin
  Result := FMaxLineWidth;
end;

function TTextLines.TotalHeight(): Single;
begin
  Result := FTotalHeight;
end;

function TTextLines.AllGlyphs(): IGlyphVertices;
begin
  Result := FGlyphs;
end;

function TTextLines.BoundsXY(): TVec4;
begin
  Result := Vec(FBoundsX, FBoundsY);
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
  FBoundsX.x := 0;
  FBoundsX.y := FMaxLineWidth;
  FBoundsY.x := 0;
  FBoundsY.y := FTotalHeight;
end;

{ TTextBuilder }

procedure TTextBuilder.InitLine;
begin
  if not FLineInited then
  begin
    FLineInited := True;
    FLineStart := FGlyphs.Count;
    FLineInfo.align := FAlign;
    FLineInfo.yymetrics := Vec(0,0);
    FLineInfo.width := 0;
    FLineInfo.glyphs.x := FGlyphs.Count;
    FLineInfo.glyphs.y := FLineInfo.glyphs.x;
    FLineInfo.XXXMetrics := TXXXMetrics.Create();
  end;
end;

function TTextBuilder.CalcBounds(const AStr: UnicodeString): TVec2;
var
  i: Integer;
  glyph: TGlyphVertex;
  yy: TVec2;
  ch: WideChar;
  xxx: TVec3;
  yyyy: TVec4;
begin
  Result := Vec(0,0);
  yy := Vec(0,0);
  for i := 1 to Length(AStr) do
  begin
    ch := AStr[i];
    glyph.Glyph := FAtlas.ObtainGlyph(FFont.Name, ch, FFont.Style);
    xxx := glyph.Glyph.XXXMetricsScaled(FFont.Size);
    yyyy := glyph.Glyph.YYYYMetricsScaled(FFont.Size);
    yy := Max(yy, Vec(yyyy.x + yyyy.y, yyyy.z + yyyy.w));
    Result.x := Result.x + xxx.x + xxx.y + xxx.z;
  end;
  Result.y := yy.x + yy.y;
end;

procedure TTextBuilder.WriteWordInternal(const AStr: UnicodeString);
var
  pword: PWordInfo;
  dummyword: TWordInfo;
  ch: WideChar;
  xxx: TVec3;
  yyyy: TVec4;
  glyph: PGlyphVertex;
  dummyglyph: TGlyphVertex;
  i: Integer;
  posx: Single;
  dummy: IGlyphIndex;
begin
  {$IfDef FPC}
  ZeroClear(dummyglyph, SizeOf(dummyglyph));
  ZeroClear(dummyword, SizeOf(dummyword));
  {$EndIf}

  if FWrappedWords = nil then FWrappedWords := TWordsArr.Create();
  pword := PWordInfo( FWrappedWords.PItem[FWrappedWords.Add(dummyword)] );
  pword^.Glyphs := TGlyphVertices.Create();
  pword^.XXXMetrics := TXXXMetrics.Create();
  pword^.YYYYMetrics := TVec4Arr.Create();
  pword^.YYMetrics := Vec(0, 0);

  dummy := FAtlas.ObtainGlyph(FFont.Name, ' ', FFont.Style);
  xxx := dummy.XXXMetricsScaled(FFont.Size);
  yyyy := dummy.YYYYMetricsScaled(FFont.Size);
  pword^.XXXSpace := xxx;

  posx := 0;
  for i := 1 to Length(AStr) do
  begin
    ch := AStr[i];
    glyph := PGlyphVertex( pword^.Glyphs.PItem[pword^.Glyphs.Add(dummyglyph)] );
    glyph^.Glyph := FAtlas.ObtainGlyph(FFont.Name, ch, FFont.Style);
    xxx := glyph^.Glyph.XXXMetricsScaled(FFont.Size);
    yyyy := glyph^.Glyph.YYYYMetricsScaled(FFont.Size);

    pword^.YYYYMetrics.Add(yyyy);
    pword^.Width := pword^.Width + xxx.x + xxx.y + xxx.z;
    pword^.XXXMetrics.Add(xxx);

    posx := posx + xxx.x + xxx.y*0.5;
    glyph^.Pos.x := posx;
    posx := posx + xxx.y*0.5 + xxx.z;

    glyph^.Size.x := xxx.y;
    glyph^.Size.y := yyyy.y + yyyy.z;

    glyph^.SDFOffset := FFont.SDFOffset;
    glyph^.Color := FFont.Color;

    pword^.YYMetrics := Max(pword^.YYMetrics, Vec(yyyy.x + yyyy.y, yyyy.z + yyyy.w));
  end;
end;

procedure TTextBuilder.WriteInternal(const AStr: UnicodeString);
var ch: WideChar;
    xxx: TVec3;
    yyyy: TVec4;
    glyph: PGlyphVertex;
    dummy: TGlyphVertex;
    i: Integer;
begin
  {$IfDef FPC}
  ZeroClear(dummy, SizeOf(dummy));
  {$EndIf}

  InitLine;

  for i := 1 to Length(AStr) do
  begin
    ch := AStr[i];
    glyph := PGlyphVertex( FGlyphs.PItem[FGlyphs.Add(dummy)] );
    glyph^.Glyph := FAtlas.ObtainGlyph(FFont.Name, ch, FFont.Style);
    xxx := glyph^.Glyph.XXXMetricsScaled(FFont.Size);
    yyyy := glyph^.Glyph.YYYYMetricsScaled(FFont.Size);

    FLineYYYYMetrics.Add(yyyy);
    FLineInfo.width := FLineInfo.width + xxx.x + xxx.y + xxx.z;
    FLineInfo.XXXMetrics.Add(xxx);

    FPos.x := FPos.x + xxx.x + xxx.y*0.5;
    glyph^.Pos.x := FPos.x;
    FPos.x := FPos.x + xxx.y*0.5 + xxx.z;

    glyph^.Size.x := xxx.y;
    glyph^.Size.y := yyyy.y + yyyy.z;

    glyph^.SDFOffset := FFont.SDFOffset;
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
    FLineInfo.glyphs.y := FGlyphs.Count;
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

procedure TTextBuilder.WriteSpace(ASpace: Single);
begin
  InitLine;
  FLineInfo.width := FLineInfo.width + ASpace;
  FPos.x := FPos.x + ASpace;
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

procedure TTextBuilder.WriteMultiline(const AStr: string);
var sl: TStringList;
    i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Text := AStr;
    for i := 0 to sl.Count - 1 do
      WriteLn(sl[i]);
  finally
    sl.Free;
  end;
end;

procedure TTextBuilder.WriteWrapped(const AStr: string);
  function FirstSpaces(): UnicodeString;
  var i: Integer;
  begin
    for i := 1 to Length(AStr) do
      if AStr[i] <> ' ' then
      begin
        Result := UnicodeString(Copy(AStr, 1, i-1));
        Exit;
      end;
    Result := '';
  end;
var sl: TStringList;
    i: Integer;
    fs: UnicodeString;
begin
  if FWrappedWords = nil then FWrappedWords := TWordsArr.Create();
  fs := FirstSpaces();
  sl := TStringList.Create;
  try
    sl.Delimiter := ' ';
    sl.DelimitedText := AStr;
    for i := 0 to sl.Count - 1 do
      if i = 0 then
        WriteWordInternal(fs + UnicodeString(sl.Strings[i]))
      else
        WriteWordInternal(UnicodeString(sl.Strings[i]));
  finally
    FreeAndNil(sl);
  end;
end;

procedure TTextBuilder.WriteWrappedMultiline(const AStr: string; const AMaxWidth: Single; AJustifyAlign: Boolean = False; AFirstRowOffset: Single = 0; ANextRowsOffset: Single = 0);
var sl: TStringList;
    i: Integer;
begin
  sl := TStringList.Create;
  try
    sl.Text := AStr;
    for i := 0 to sl.Count - 1 do
    begin
      WriteWrapped(sl[i]);
      WriteWrappedEnd(AMaxWidth, AJustifyAlign, AFirstRowOffset, ANextRowsOffset);
    end;
  finally
    sl.Free;
  end;
end;

procedure TTextBuilder.WriteWrappedEnd(const AMaxWidth: Single;
  AJustifyAlign: Boolean; AFirstRowOffset: Single; ANextRowsOffset: Single);

type
  IVec2iArr = {$IfDef FPC}specialize{$EndIf} IArray<TVec2i>;
  TVec2iArr = {$IfDef FPC}specialize{$EndIf} TArray<TVec2i>;

var i, j, k: Integer;
    remainWidth, offset, JustifySpace, CurrLineWidth: Single;
    pw: PWordInfo;
    lines: IVec2iArr;
    yyyy: TVec4;
    xxx: TVec3;
    xOffset: Single;
    glyph: PGlyphVertex;
    isNewLine: Boolean;
begin
  if FLineInited then
    WriteLnInternal;

  if FAlign <> laLeft then
  begin
    AFirstRowOffset := 0;
    ANextRowsOffset := 0;
  end;

  lines := TVec2iArr.Create();
  remainWidth := 0;
  for i := 0 to FWrappedWords.Count - 1 do
  begin
    isNewLine := False;
    pw := PWordInfo(FWrappedWords.PItem[i]);
    if remainWidth < (pw^.Width + pw^.XXXSpace.x + pw^.XXXSpace.y*0.5) then //make new line
    begin
      if i = 0 then
        remainWidth := AMaxWidth - AFirstRowOffset
      else
        remainWidth := AMaxWidth - ANextRowsOffset;
      lines.Add(Vec(i, 0));
      isNewLine := True;
    end;
    if isNewLine then
      remainWidth := remainWidth - (pw^.Width + pw^.XXXSpace.y*0.5 + pw^.XXXSpace.z)
    else
      remainWidth := remainWidth - (pw^.Width + pw^.XXXSpace.x + pw^.XXXSpace.y + pw^.XXXSpace.z);
    lines.Last := lines.Last + Vec(0, 1);
  end;

  for i := 0 to lines.Count - 1 do
  begin
    if i = 0 then
      offset := AFirstRowOffset
    else
      offset := ANextRowsOffset;

    if i = lines.Count - 1 then AJustifyAlign := False;

    JustifySpace := 0;
    if AJustifyAlign then
    begin
      CurrLineWidth := 0;
      for j := 0 to lines[i].y - 1 do
        CurrLineWidth := CurrLineWidth + FWrappedWords[lines[i].x + j].Width;
      JustifySpace := (AMaxWidth - offset - CurrLineWidth) / (lines[i].y - 1) * 0.5;
    end;

    pw := nil;
    WriteSpace(offset);
    for j := 0 to lines[i].y - 1 do
    begin
      if pw <> nil then
      begin
        if AJustifyAlign then
          WriteSpace(JustifySpace)
        else
          WriteSpace(pw^.XXXSpace.x + pw^.XXXSpace.y*0.5);
      end;

      pw := PWordInfo(FWrappedWords.PItem[lines[i].x + j]);
      xOffset := FPos.x;
      for k := 0 to pw^.Glyphs.Count - 1 do
      begin
        glyph := PGlyphVertex( FGlyphs.PItem[FGlyphs.Add(pw^.Glyphs[k])] );
        glyph^.Pos.x := glyph^.Pos.x + xOffset;
        //FGlyphs.Add(pw^.Glyphs[k]);
        yyyy := pw^.YYYYMetrics[k];
        xxx := pw^.XXXMetrics[k];

        FLineYYYYMetrics.Add(yyyy);
        FLineInfo.width := FLineInfo.width + xxx.x + xxx.y + xxx.z;
        FLineInfo.XXXMetrics.Add(xxx);
        FPos.x := FPos.x + xxx.x + xxx.y + xxx.z;
        FLineInfo.yymetrics := Max(FLineInfo.yymetrics, Vec(yyyy.x + yyyy.y, yyyy.z + yyyy.w));
      end;

      if j <> lines[i].y - 1 then
      begin
        if AJustifyAlign then
          WriteSpace(JustifySpace)
        else
          WriteSpace(pw^.XXXSpace.y*0.5 + pw^.XXXSpace.z);
      end;
    end;
    WriteLnInternal;
  end;
  FWrappedWords.Clear();
end;

function TTextBuilder.Finish(): ITextLines;
begin
  if FLineInited then WriteLnInternal;

  Result := TTextLines.Create(FGlyphs, FLines);

  FGlyphs := TGlyphVertices.Create();
  FLines := TLineInfoArr.Create();
  FLineInited := False;
  FLineStart := 0;
  FLineYYYYMetrics := TVec4Arr.Create();
end;

constructor TTextBuilder.Create(const AFont: TFontStyle; const AAtlas: TavGlyphAtlas);
begin
  FAtlas := AAtlas;
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

{ TGlyphVertexGPU }

procedure TGlyphVertex.SetGlyph(const AValue: IGlyphIndex);
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
              .Add('Color', ctFloat, 4)
              .Add('GlyphID', ctUInt, 1)
              .Finish(SizeOf(TGlyphVertex));
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

procedure TFontStyle.SetColor(const AValue: TVec4);
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

procedure TFontStyle.AfterConstruction;
begin
  inherited AfterConstruction;
  FName := 'Segoe UI';
  FSize := 18;
  FColor := Vec(1,1,1,1);
end;

{ TavGlyphAtlas.TGlyphKey }

function TavGlyphAtlas.TGlyphKey.GenData: TGlyphData;
var scale: Single;
    i, j: Integer;
    vOffset: TVec2;
    cntr: IGlyphContour;
begin
  Result.poly := GenerateGlyphOutline(font, glyph, gsItalic in style, gsBold in style, gsUnderline in style, Result.XXX, Result.YYYY);
  Result.Border := border;

  scale := size/(Result.YYYY.x+Result.YYYY.y+Result.YYYY.z+Result.YYYY.w);

  Result.XXX := Result.XXX * scale;
  Result.YYYY := Result.YYYY * scale;

  Result.ImgSize.x := Math.Ceil(Result.XXX.y) + Result.Border*2;
  Result.ImgSize.y := Math.Ceil(Result.YYYY.y+Result.YYYY.z) + Result.Border*2;

  Result.XXX.x := Result.XXX.x - Result.Border;
  Result.XXX.y := Result.XXX.y + 2*Result.Border;
  Result.XXX.z := Result.XXX.z - Result.Border;
  Result.YYYY.x := Result.YYYY.x - Result.Border;
  Result.YYYY.y := Result.YYYY.y + Result.Border;
  Result.YYYY.z := Result.YYYY.z + Result.Border;
  Result.YYYY.w := Result.YYYY.w - Result.Border;

  vOffset := Vec(border, border);
  for j := 0 to Result.poly.Count - 1 do
  begin
    cntr := Result.poly[j];
    for i := 0 to cntr.Count - 1 do
      PVec2(cntr.PItem[i])^ := cntr.Item[i] * scale + vOffset;
  end;
end;

{ TavGlyphAtlas }

function TavGlyphAtlas.AllocIndex: Integer;
var dummy: TSpriteRegion;
begin
  if FFreeIndices.Count = 0 then
  begin
    Result := FSpriteList.Add(nil);
    dummy.Rect := Vec(0,0,0,0);
    dummy.Slice := 0;
    FRegions.Add(dummy);
  end
  else
  begin
    Result := FFreeIndices.Last;
    FFreeIndices.Delete(FFreeIndices.Count-1);
  end;
end;

procedure TavGlyphAtlas.AllocQuad(const ASize: TVec2i; out ARange: IQuadRange; out ASlice: Integer);
var Page: PPageInfo;
    NewPage: TPageInfo;
begin
  Assert(ASize.x <= FTargetSize.x);
  Assert(ASize.y <= FTargetSize.y);
  ASlice := 0;
  while ASlice < FPages.Count do
  begin
    Page := PPageInfo(FPages.PItem[ASlice]);
    try
      ARange := Page^.QManager.Alloc(ASize.x, ASize.y);
      Invalidate;
      Exit;
    except
      on e: EQuadRangeOutOfSpace do
        Inc(ASlice);
    end;
  end;

  FInvalidPagesCount := True;
  NewPage.QManager := Create_IQuadManager(FTargetSize.x, FTargetSize.y);
  NewPage.InvalidSprites := TSpriteList.Create();
  FPages.Add(NewPage);
  AllocQuad(ASize, ARange, ASlice);
end;

procedure TavGlyphAtlas.SetTargetSize(const AValue: TVec2i);
begin
  if FTargetSize = AValue then Exit;
  FTargetSize := AValue;
  FullRebuild;
end;

procedure TavGlyphAtlas.FullRebuild;
var i : Integer;
    sprite: TGlyphIndex;
    region: TSpriteRegion;
begin
  for i := 0 to FSpriteList.Count - 1 do
  begin
    sprite := FSpriteList[i];
    if sprite = nil then Continue;
    sprite.FQuad := nil;
    sprite.FSlice := 0;
  end;

  FRegions.Clear();

  //todo sort by size
  for i := 0 to FSpriteList.Count - 1 do
  begin
    sprite := FSpriteList[i];
    if sprite = nil then Continue;
    AllocQuad(sprite.FData.ImgSize, sprite.FQuad, sprite.FSlice);
    region.Rect := sprite.FQuad.Rect.v;
    region.Slice := sprite.FSlice;
    FRegions[sprite.FIndex] := region;
  end;
  FRegionsBuffer.Invalidate;
  FInvalidPagesCount := True;
end;

function TavGlyphAtlas.DoBuild: Boolean;
var page: PPageInfo;
    sprite: TGlyphIndex;
    i, j: Integer;
    glyphGen: IGPUGlyphGenerator;
begin
  glyphGen := GetGlyphGenerator(Main);

  Result := True;
  if FPages.Count = 0 then Exit;

  if FTexH = nil then
  begin
    FTexH := Main.Context.CreateTexture;
    FTexH.TargetFormat := FTargetFormat;
    FTexH.sRGB := FsRGB;
  end;

  if FInvalidPagesCount then
  begin
    FInvalidPagesCount := False;
    FTexH.AllocMem(FTargetSize.x, FTargetSize.y, FPages.Count, False, True);

    for i := 0 to FPages.Count - 1 do
    begin
      page := PPageInfo(FPages.PItem[i]);
      page^.InvalidSprites.Clear();
    end;

    for i := 0 to FSpriteList.Count - 1 do
    begin
      sprite := FSpriteList[i];
      if sprite = nil then Continue;
      PPageInfo(FPages.PItem[sprite.FSlice])^.InvalidSprites.Add(sprite);
    end;
  end;

  for i := 0 to FPages.Count - 1 do
  begin
    page := PPageInfo(FPages.PItem[i]);
    for j := 0 to page^.InvalidSprites.Count - 1 do
    begin
      sprite := page^.InvalidSprites[j];
      glyphGen.DrawToTexture(sprite.FData.poly, sprite.FData.ImgSize, FTexH, sprite.FQuad.Rect.min, sprite.FSlice, 0);
    end;
    page^.InvalidSprites.Clear();
  end;
end;

procedure TavGlyphAtlas.DoOnFrameStart;
begin
  inherited DoOnFrameStart;
  Build;
end;

procedure TavGlyphAtlas.Build;
var
  glyphGen: IGPUGlyphGenerator;
begin
  glyphGen := GetGlyphGenerator(Main);
  if not Valid then
  begin
    glyphGen.BeginGeneration;
    inherited Build;
    glyphGen.EndGeneration;
  end;
end;

procedure TavGlyphAtlas.Invalidate;
begin
  inherited Invalidate;
//  SubscribeForNextFrame;
end;

procedure TavGlyphAtlas.CleanUnused;
begin
  FullRebuild;
end;

function TavGlyphAtlas.ObtainGlyph(const AFontName: string; AChar: WideChar; AStyle: TGlyphStyles): IGlyphIndex;
var spriteObj: TGlyphIndex;
    key: TGlyphKey;
    data: TGlyphData;
    range: IQuadRange;
    slice: Integer;
    freeIndex: Integer;
    region: TSpriteRegion;
begin
  FillChar(key, SizeOf(key), 0);
  key.font  := AFontName;
  key.glyph := AChar;
  key.style := AStyle;
  key.size := 32;
  key.border := 8;
  if not FSprites.TryGetValue(key, spriteObj) then
  begin
    data := key.GenData;
    AllocQuad(data.ImgSize, range, slice);
    freeIndex := AllocIndex();
    spriteObj := TGlyphIndex.Create(Self, key, data, freeIndex, slice, range);
    FSpriteList[freeIndex] := spriteObj;
    FSprites.Add(key, spriteObj);
    region.Slice := spriteObj.FSlice;
    region.Rect := spriteObj.FQuad.Rect.v;
    FRegions[freeIndex] := region;

    FPages[slice].InvalidSprites.Add(spriteObj);
    FRegionsBuffer.Invalidate;
  end;
  Result := spriteObj;
end;

function TavGlyphAtlas.GetRegion(AIndex: Integer): TSpriteRegion;
begin
  Result := FRegions[AIndex];
end;

procedure TavGlyphAtlas.AfterConstruction;
begin
  inherited AfterConstruction;
  FSprites := TSpriteMap.Create();
  FSpriteList := TSpriteList.Create();
  FFreeIndices := TFreeIndices.Create();
  FPages := TPages.Create();
  FRegions := TRegions.Create();

  FRegionsBuffer := TavSB.Create(Self);
  FRegionsBuffer.Vertices := FRegions as IVerticesData;

  FTargetSize := Vec(4096, 4096);
end;

destructor TavGlyphAtlas.Destroy;
var i: Integer;
begin
  for i := 0 to FSpriteList.Count - 1 do
  begin
    if FSpriteList[i] = nil then Continue;
    FSpriteList[i].FQuad  := nil;
    FSpriteList[i].FOwner := nil;
  end;
  inherited Destroy;
end;

{ TavGlyphAtlas.TGlyphIndex }

function TavGlyphAtlas.TGlyphIndex.Atlas: TavGlyphAtlas;
begin
  Result := FOwner;
end;

function TavGlyphAtlas.TGlyphIndex.Index: Integer;
begin
  Result := FIndex;
end;

function TavGlyphAtlas.TGlyphIndex.Size: TVec2i;
begin
  if FQuad = nil then
    Result := Vec(1,1)
  else
    Result := FQuad.Size;
end;

function TavGlyphAtlas.TGlyphIndex.XXXMetrics: TVec3;
begin
  Result := FData.XXX;
end;

function TavGlyphAtlas.TGlyphIndex.YYYYMetrics: TVec4;
begin
  Result := FData.YYYY;
end;

function TavGlyphAtlas.TGlyphIndex.XXXMetricsScaled(AFontSize: Single): TVec3;
begin
  Result := FData.XXX * (AFontSize / FKey.size);
end;

function TavGlyphAtlas.TGlyphIndex.YYYYMetricsScaled(AFontSize: Single): TVec4;
begin
  Result := FData.YYYY * (AFontSize / FKey.size);
end;

constructor TavGlyphAtlas.TGlyphIndex.Create(const AOwner: TavGlyphAtlas;
  const AKey: TGlyphKey; const AData: TGlyphData; const AIndex: Integer;
  const ASlice: Integer; const AQuad: IQuadRange);
begin
  FOwner := AOwner;
  FKey := AKey;
  FData := AData;
  FIndex := AIndex;
  FSlice := ASlice;
  FQuad := AQuad;
end;

destructor TavGlyphAtlas.TGlyphIndex.Destroy;
var n: Integer;
begin
  if FOwner <> nil then
  begin
    FOwner.FSprites.Delete(FKey);
    FOwner.FSpriteList[FIndex] := nil;
    FOwner.FFreeIndices.Add(FIndex);
    n := FOwner.FPages[FSlice].InvalidSprites.IndexOf(Self);
    if n >= 0 then FOwner.FPages[FSlice].InvalidSprites.Delete(n);
  end;
  inherited Destroy;
end;

end.

