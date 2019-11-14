unit avCanvas;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, intfUtils,
  avBase, avRes, mutils, avTypes, avContnrs, avTess, avContext, avGlyphGenerator, avGPUGlyphGenerator, avGLUIntf;

type
  IGlyphIndex = interface;

  { TLinePointVertex }

  TLinePointVertex = packed record
    Coords      : TVec4; //xy - start point, zw - end point
    Normals     : TVec4; //xy - normal at start point, zw - normal at end point
    Width       : TVec2; //x - real width, y - minimal width in pixels
    HintingAlign: TVec4;
    Color       : TVec4;
    class function Layout: IDataLayout; static;
  end;
  TLinePointVertices = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TLinePointVertex>;
  ILinePointVertices = {$IfDef FPC}specialize{$EndIf} IArray<TLinePointVertex>;

  { TCanvasTriangleVertex }

  TCanvasTriangleVertex = packed record
  private
    procedure SetSprite(const AValue: ISpriteIndex);
  public
    Coords   : TVec2;
    Hinting  : TVec2;
    Color    : TVec4;
    TexCoord : TVec2;
    SpriteID : Integer;

    _Sprite     : ISpriteIndex;
    class function Layout: IDataLayout; static;

    property Sprite: ISpriteIndex read _Sprite write SetSprite;
  end;
  TCanvasTriangleVertices = {$IfDef FPC}specialize{$EndIf} TVerticesRec<TCanvasTriangleVertex>;
  ICanvasTriangleVertices = {$IfDef FPC}specialize{$EndIf} IArray<TCanvasTriangleVertex>;

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

  {$SCOPEDENUMS ON}
  TPenAlign = (Center, Left, Right);
  TPenHintingStyle = (Vertical, Horizontal, PostHinting);
  TPenHinting = set of TPenHintingStyle;
  {$SCOPEDENUMS OFF}

const
  cPenHintingAll = [TPenHintingStyle.Vertical, TPenHintingStyle.Horizontal, TPenHintingStyle.PostHinting];

type

  { TPenStyle }

  TPenStyle = class (TPersistent)
  private
    FAlign: TPenAlign;
    FColor: TVec4;
    FHinting: TPenHinting;
    FJointsBreakAngle: Single;
    FMinPixWidth: Integer;
    FPattern: ITextureMip;
    FPatternTransform: TMat3;
    FWidth: Single;
    procedure SetAlign(AValue: TPenAlign);
    procedure SetColor(const AValue: TVec4);
    procedure SetHinting(AValue: TPenHinting);
    procedure SetJointsBreakAngle(AValue: Single);
    procedure SetMinPixWidth(AValue: Integer);
    procedure SetPattern(const AValue: ITextureMip);
    procedure SetPatternTransform(const AValue: TMat3);
    procedure SetWidth(AValue: Single);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Color: TVec4 read FColor write SetColor;
    property Width: Single read FWidth write SetWidth;
    property Hinting: TPenHinting read FHinting write SetHinting;
    property Align: TPenAlign read FAlign write SetAlign;

    property MinPixWidth: Integer read FMinPixWidth write SetMinPixWidth;

    property JointsBreakAngle: Single read FJointsBreakAngle write SetJointsBreakAngle;

    property Pattern: ITextureMip read FPattern write SetPattern; //todo
    property PatternTransform: TMat3 read FPatternTransform write SetPatternTransform; //todo

    procedure AfterConstruction; override;
  end;

  {$SCOPEDENUMS ON}
  TBrushHintingStyle = (Vertical, Horizontal);
  TBrushHinting = set of TBrushHintingStyle;
  {$SCOPEDENUMS OFF}

const
  cBrushHintingAll = [TBrushHintingStyle.Vertical, TBrushHintingStyle.Horizontal];

type

  { TBrushStyle }

  TBrushStyle = class (TPersistent)
  private
    FColor: TVec4;
    FHinting: TBrushHinting;
    FPattern: ITextureMip;
    FPatternTransform: TMat3;
    procedure SetColor(const AValue: TVec4);
    procedure SetHinting(const AValue: TBrushHinting);
    procedure SetPattern(const AValue: ITextureMip);
    procedure SetPatternTransform(const AValue: TMat3);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Color: TVec4 read FColor write SetColor;
    property Hinting: TBrushHinting read FHinting write SetHinting;

    property Pattern: ITextureMip read FPattern write SetPattern; //todo
    property PatternTransform: TMat3 read FPatternTransform write SetPatternTransform; //todo

    procedure AfterConstruction; override;
  end;

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

  { TavUIProgram }

  TavUIProgram = class (TavProgram)
  private
    FUniform_UIMatrix: TUniformField;
    FUniform_UIMatrixInverse: TUniformField;
    FUniform_ViewportSize: TUniformField;

    FUniform_CanvasTransform: TUniformField;
    FUniform_PixelToUnit: TUniformField;

    FUniform_ZValue: TUniformField;

    FUniform_ColorFilter: TUniformField;

    FLastViewportSize: TVec2i;
  protected
    procedure BeforeFree3D; override;
    function  DoBuild: Boolean; override;

    procedure UpdateUniforms; override;
  public
    procedure SetCanvasTransform(const ATransform: TMat3);
    procedure SetCustomUIMatrix(const ATransform: TMat4);
    procedure SetPixelToUnit(const AScale: single);
    procedure SetZValue(const AZVal: single);
    procedure SetColorFilters(const AColorFilter: TMat4);
  end;
  TavFontProgram = class;

  TavGlyphAtlas = class;

  IGlyphIndex = interface
    function Atlas: TavGlyphAtlas;
    function Index: Integer;
    function Size : TVec2i;
    function XXXMetrics: TVec3;
    function YYYYMetrics: TVec4;
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
      font : string[255];
      glyph: WideChar;
      style: TGlyphStyles;
      function GenData(ASize, ABorder: Integer): TGlyphData;
    end;

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

  { TavCanvasCommonData }
  TavCanvasCommonData = class (TavRes)
  private
    FLineQuad : TavVB;
    FLineProg : TavUIProgram;
    FFontProg : TavFontProgram;
    FTrisProg : TavUIProgram;

    FGlyphsAtlas : TavGlyphAtlas;

    FSpritesAtlas: TavAtlasArrayReferenced;
  private
    FGlyphGenerationSize: Integer;
    FGlyphGenerationSize_Inv: Double;
    FGlyphGenerationBorderSize: Integer;
  protected
    procedure AfterInit3D; override;
    procedure BeforeFree3D; override;
  public
    //function GetGlyphImage(const AFontName: string; AChar: WideChar; AStyle: TGlyphStyles; out XXXMetrics: TVec3; out YYYYMetrics: TVec4): ITextureMip;
    function GetGlyphSprite(const AFontName: string; AChar: WideChar; AStyle: TGlyphStyles; out XXXMetrics: TVec3; out YYYYMetrics: TVec4): IGlyphIndex;
    function GetImageSprite(const AFileName: string): ISpriteIndex;
    function GetTextureSprite(const ATexture: ITextureMip): ISpriteIndex;

    property GlyphsAtlas : TavGlyphAtlas read FGlyphsAtlas;
    property SpritesAtlas: TavAtlasArrayReferenced read FSpritesAtlas;
    property LineQuad: TavVB          read FLineQuad;

    procedure ReloadShaders;
    property LineProg: TavUIProgram   read FLineProg;
    property FontProg: TavFontProgram read FFontProg;
    property TrisProg: TavUIProgram   read FTrisProg;

    procedure SetGlyphAtlasSize(const ATargetAtlasSize: TVec2i);
    procedure SetGlyphGenerationSize(AGlyphSize: Integer = 32; ABorderSize: Integer = 8);
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

    FUniform_BoundsXY : TUniformField;
    FUniform_YPos_ClipWithBounds : TUniformField;
  protected
    procedure AfterInit3D; override;
    procedure BeforeFree3D; override;
    function  DoBuild: Boolean; override;

    procedure UpdateUniforms; override;
  public
    procedure SetBoundsXY(const BoundsX, BoundsY: TVec2);
    procedure SetYPos_ClipWithBounds(YPos: Single; Clip: Boolean);
  end;

  { TavTrisProgram }

  TavTrisProgram = class (TavUIProgram)
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
  public
  end;

  { TavCanvas }

  TavCanvas = class(TavMainRenderChild)
  private type
    TGeometryKind = (gkUnknown, gkLines, gkFont, gkTris);
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
    FUseCustomUIMatrix: Boolean;
    FCustomUIMatrix: TMat4;

    FZValue: Single;
    FPen   : TPenStyle;
    FBrush : TBrushStyle;
    FFont  : TFontStyle;

    FLineData: ILinePointVertices;
    FValid   : Boolean;
    FVBLines : TavVB;

    FTextLines      : ITextLinesArr;
    FTextLineRanges : IVec2iArray;
    FGlyphsData     : IGlyphVertices;
    FVBGlyphs       : TavVB;

    FTrisData : ICanvasTriangleVertices;
    FVBTris   : TavVB;

    FGeometryBatches: IGeometry;
    FCurrentBatch: TGeometryBatch;

    procedure SetFont(const AValue: TFontStyle);
    procedure SetBrush(const AValue: TBrushStyle);
    procedure SetPen(AValue: TPenStyle);
    procedure AddLineSegment(Coords, Normals: TVec4);
    procedure FillSegmentByPen(out Seg: TLinePointVertex);
    procedure FillVertexWithBrush(out V: TCanvasTriangleVertex);

    procedure SetValid(AValue: Boolean);
    procedure SelectGeometryKind(const AKind: TGeometryKind);

    procedure AddQuad(const LeftTop, RightBottom: TVec2; const ASprite: ISpriteIndex);
    procedure AddQuadClipped(const LeftTop, RightBottom: TVec2; const ASprite: ISpriteIndex; const ASpriteRect: TRectI);
  public
    property UseCustomUIMatrix: Boolean read FUseCustomUIMatrix write FUseCustomUIMatrix;
    property CustomUIMatrix: TMat4 read FCustomUIMatrix write FCustomUIMatrix;

    property ZValue: Single      read FZValue write FZValue;
    property Pen   : TPenStyle   read FPen    write SetPen;
    property Brush : TBrushStyle read FBrush  write SetBrush;
    property Font  : TFontStyle  read FFont   write SetFont;

    property Valid: Boolean    read FValid write SetValid;

    function TextBuilder: ITextBuilder;
    function GetSprite(const AFileName: string): ISpriteIndex; overload;
    function GetSprite(const ATexture: ITextureMip): ISpriteIndex; overload;

    //drawing functions
    procedure Clear;
    procedure AddLine(const Start, Stop: TVec2); overload;
    procedure AddPolyline(const APts: array of TVec2; AClosed: Boolean = false); overload;
    procedure AddRectangle(Left, Top, Right, Bottom: Single); overload;
    procedure AddRectangle(LeftTop, RightBottom: TVec2); overload;
    procedure AddText(const AText: ITextLines);
    procedure AddFill(const LeftTop, RightBottom: TVec2); overload;
    procedure AddPolyFill(const APoly: array of TPolyContour); overload;
    procedure AddTriangle(const V1,V2,V3: TVec2); overload;
    procedure AddSprite(const LeftTop, RightBottom: TVec2; const AFileName: string); overload;
    procedure AddSprite(const LeftTop, RightBottom: TVec2; const ASprite: ISpriteIndex); overload;
    procedure AddSpriteClipped(const LeftTop, RightBottom: TVec2; const AFileName: string; const AClipRect: TRectI); overload;
    procedure AddSpriteClipped(const LeftTop, RightBottom: TVec2; const ASprite: ISpriteIndex; const AClipRect: TRectI); overload;

    procedure Draw(const ARotation: Single; const AOffset: TVec2; const APixelToUnit: Single); overload;
    procedure Draw(const ATransform: TMat3); overload;
    procedure Draw(const ARotation: Single; const AOffset: TVec2; const APixelToUnit: Single; const AColorFilter: TMat4); overload;
    procedure Draw(const ATransform: TMat3; const AColorFilter: TMat4); overload;

    constructor Create(AParent: TavObject); overload; override;
    destructor Destroy; override;
  end;

function GetCanvasCommonData(const RenderMain: TavMainRender): TavCanvasCommonData;

implementation

//{$R '..\Canvas_Shaders\Canvas_Shaders.rc'}
{$R '..\Canvas_Shaders\Canvas_Shaders.res'}

uses Math, avTexLoader;

const
  NAME_TavCanvasCommonData = 'TavCanvasCommonData';

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
    FCommon: TavCanvasCommonData;
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

    procedure ScaleMetrics(var xxx: TVec3; var yyyy: TVec4);

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

{ TavGlyphAtlas.TGlyphKey }

function TavGlyphAtlas.TGlyphKey.GenData(ASize, ABorder: Integer): TGlyphData;
var scale: Single;
    i, j: Integer;
    vOffset: TVec2;
    cntr: IGlyphContour;
begin
  Result.poly := GenerateGlyphOutline(font, glyph, gsItalic in style, gsBold in style, gsUnderline in style, Result.XXX, Result.YYYY);
  Result.Border := ABorder;

  scale := ASize/(Result.YYYY.x+Result.YYYY.y+Result.YYYY.z+Result.YYYY.w);

  Result.XXX := Result.XXX * scale;
  Result.YYYY := Result.YYYY * scale;

  Result.ImgSize.x := Ceil(Result.XXX.y) + Result.Border*2;
  Result.ImgSize.y := Ceil(Result.YYYY.y+Result.YYYY.z) + Result.Border*2;

  Result.XXX.x := Result.XXX.x - Result.Border;
  Result.XXX.y := Result.XXX.y + 2*Result.Border;
  Result.XXX.z := Result.XXX.z - Result.Border;
  Result.YYYY.x := Result.YYYY.x - Result.Border;
  Result.YYYY.y := Result.YYYY.y + Result.Border;
  Result.YYYY.z := Result.YYYY.z + Result.Border;
  Result.YYYY.w := Result.YYYY.w - Result.Border;

  vOffset := Vec(ABorder, ABorder);
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
  if not FSprites.TryGetValue(key, spriteObj) then
  begin
    data := key.GenData(32, 8);
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

{ TavTrisProgram }

procedure TavTrisProgram.AfterInit3D;
begin
  inherited AfterInit3D;
  FCommon := GetCanvasCommonData(Main);
end;

procedure TavTrisProgram.BeforeFree3D;
begin
  inherited BeforeFree3D;
  FUniform_Atlas       := nil;
  FUniform_AtlasRegions:= nil;
  FUniform_AtlasSize   := nil;
end;

function TavTrisProgram.DoBuild: Boolean;
begin
  Result := inherited DoBuild;
  if Result then
  begin
    FUniform_Atlas := GetUniformField('Atlas');
    FUniform_AtlasRegions := GetUniformField('AtlasRegions');
    FUniform_AtlasSize := GetUniformField('AtlasSize');
  end;
end;

procedure TavTrisProgram.UpdateUniforms;
begin
  inherited UpdateUniforms;
  SetUniform(FUniform_Atlas, FCommon.SpritesAtlas, Sampler_Linear);
  SetUniform(FUniform_AtlasRegions, FCommon.SpritesAtlas.RegionsVB);
  SetUniform(FUniform_AtlasSize, FCommon.SpritesAtlas.Size);
end;

{ TBrushStyle }

procedure TBrushStyle.SetColor(const AValue: TVec4);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
end;

procedure TBrushStyle.SetHinting(const AValue: TBrushHinting);
begin
  if FHinting = AValue then Exit;
  FHinting := AValue;
end;

procedure TBrushStyle.SetPattern(const AValue: ITextureMip);
begin
  if FPattern = AValue then Exit;
  FPattern := AValue;
end;

procedure TBrushStyle.SetPatternTransform(const AValue: TMat3);
begin
  if FPatternTransform = AValue then Exit;
  FPatternTransform := AValue;
end;

procedure TBrushStyle.AssignTo(Dest: TPersistent);
var BrushDest: TBrushStyle absolute Dest;
begin
  Assert(Dest is TBrushStyle);
  BrushDest.FColor := FColor;
  BrushDest.FHinting := FHinting;
  BrushDest.FPattern := FPattern;
  BrushDest.FPatternTransform := FPatternTransform;
end;

procedure TBrushStyle.AfterConstruction;
begin
  inherited AfterConstruction;
  FColor := Vec(0.5,0.5,0.5,1.0);
  FHinting := cBrushHintingAll;
end;

{ TCanvasTriangleVertex }

procedure TCanvasTriangleVertex.SetSprite(const AValue: ISpriteIndex);
begin
  _Sprite := AValue;
  if Assigned(_Sprite) then
    SpriteID := Sprite.Index
  else
    SpriteID := -1;
end;

class function TCanvasTriangleVertex.Layout: IDataLayout;
begin
  Result := LB.Add('Coords', ctFloat, 2).
               Add('Hinting', ctFloat, 2).
               Add('Color', ctFloat, 4).
               Add('TexCoord', ctFloat, 2).
               Add('SpriteID', ctInt, 1).
               Finish(SizeOf(TCanvasTriangleVertex));
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
  FUniform_AtlasSize   := nil;
  FUniform_BoundsXY := nil;
  FUniform_YPos_ClipWithBounds := nil;
end;

function TavFontProgram.DoBuild: Boolean;
begin
  Result := inherited DoBuild;
  if Result then
  begin
    FUniform_Atlas := GetUniformField('Atlas');
    FUniform_AtlasRegions := GetUniformField('AtlasRegions');
    FUniform_AtlasSize := GetUniformField('AtlasSize');
    FUniform_BoundsXY := GetUniformField('BoundsXY');
    FUniform_YPos_ClipWithBounds := GetUniformField('YPos_ClipWithBounds');
  end;
end;

procedure TavFontProgram.UpdateUniforms;
begin
  inherited UpdateUniforms;
  SetUniform(FUniform_Atlas, FCommon.GlyphsAtlas, Sampler_Linear);
  SetUniform(FUniform_AtlasRegions, FCommon.GlyphsAtlas.RegionsVB);
  SetUniform(FUniform_AtlasSize, FCommon.GlyphsAtlas.Size);
end;

procedure TavFontProgram.SetBoundsXY(const BoundsX, BoundsY: TVec2);
begin
  SetUniform(FUniform_BoundsXY, Vec(BoundsX, BoundsY));
end;

procedure TavFontProgram.SetYPos_ClipWithBounds(YPos: Single; Clip: Boolean);
var clipF: Single;
begin
  if Clip then clipF := 1 else clipF := 0;
  SetUniform(FUniform_YPos_ClipWithBounds, Vec(YPos, clipF));
end;

{ TavUIProgram }

procedure TavUIProgram.BeforeFree3D;
begin
  inherited BeforeFree3D;
  FUniform_UIMatrix := nil;
  FUniform_UIMatrixInverse := nil;
  FUniform_ViewportSize := nil;
  FUniform_PixelToUnit := nil;
  FUniform_CanvasTransform := nil;
  FUniform_ZValue := nil;
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
    FUniform_CanvasTransform := GetUniformField('_CanvasTransform');
    FUniform_ZValue := GetUniformField('ZValue');
    FUniform_ColorFilter := GetUniformField('UIColorFilter');
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

procedure TavUIProgram.SetCanvasTransform(const ATransform: TMat3);
var m: TMat4;
begin
  m.Row[0] := Vec(ATransform.Row[0], 0);
  m.Row[1] := Vec(ATransform.Row[1], 0);
  m.Row[2] := Vec(ATransform.Row[2], 0);
  m.Row[3] := Vec(0, 0, 0, 0);
  SetUniform(FUniform_CanvasTransform, m);
end;

procedure TavUIProgram.SetCustomUIMatrix(const ATransform: TMat4);
begin
  SetUniform(FUniform_UIMatrix, ATransform);
  SetUniform(FUniform_UIMatrixInverse, Inv(ATransform));
end;

procedure TavUIProgram.SetColorFilters(const AColorFilter: TMat4);
begin
  SetUniform(FUniform_ColorFilter, AColorFilter);
end;

procedure TavUIProgram.SetPixelToUnit(const AScale: single);
begin
  SetUniform(FUniform_PixelToUnit, AScale);
end;

procedure TavUIProgram.SetZValue(const AZVal: single);
begin
  SetUniform(FUniform_ZValue, AZVal);
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

{ TTextBuilder }

procedure TTextBuilder.ScaleMetrics(var xxx: TVec3; var yyyy: TVec4);
begin
  xxx := xxx * FCommon.FGlyphGenerationSize_Inv * FFont.Size;
  yyyy := yyyy * FCommon.FGlyphGenerationSize_Inv * FFont.Size;
end;

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
    glyph.Glyph := FCommon.GetGlyphSprite(FFont.Name, ch, FFont.Style, xxx, yyyy);
    ScaleMetrics(xxx, yyyy);
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

  FCommon.GetGlyphSprite(FFont.Name, ' ', FFont.Style, xxx, yyyy);
  ScaleMetrics(xxx, yyyy);
  pword^.XXXSpace := xxx;

  posx := 0;
  for i := 1 to Length(AStr) do
  begin
    ch := AStr[i];
    glyph := PGlyphVertex( pword^.Glyphs.PItem[pword^.Glyphs.Add(dummyglyph)] );
    glyph^.Glyph := FCommon.GetGlyphSprite(FFont.Name, ch, FFont.Style, xxx, yyyy);
    ScaleMetrics(xxx, yyyy);

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
    glyph^.Glyph := FCommon.GetGlyphSprite(FFont.Name, ch, FFont.Style, xxx, yyyy);
    ScaleMetrics(xxx, yyyy);

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

{ TLinePointVertex }

class function TLinePointVertex.Layout: IDataLayout;
begin
  Result := LB.Add('Coords', ctFloat, 4).
               Add('Normals', ctFloat, 4).
               Add('Width', ctFloat, 2).
               Add('HintingAlign', ctFloat, 4).
               Add('Color', ctFloat, 4).
               Finish(SizeOf(TLinePointVertex));
end;

{ TLineQuadVertex }

class function TLineQuadVertex.Layout: IDataLayout;
begin
  Result := LB.Add('quadCoord', ctFloat, 2).Finish(SizeOf(TLineQuadVertex));
end;

{ TavCanvasCommonData }

procedure TavCanvasCommonData.AfterInit3D;
begin
  inherited AfterInit3D;
  ReloadShaders;
end;

procedure TavCanvasCommonData.BeforeFree3D;
begin
  inherited BeforeFree3D;
end;

//function TavCanvasCommonData.GetGlyphImage(const AFontName: string;
//  AChar: WideChar; AStyle: TGlyphStyles; out XXXMetrics: TVec3; out
//  YYYYMetrics: TVec4): ITextureMip;
//var key  : TGlyphKey;
//    pdata: PGlyphData;
//    data : TGlyphData;
//begin
//  ZeroClear(key, SizeOf(key));
//  key.font := AFontName;
//  key.glyph := AChar;
//  key.style := AStyle;
//  if not FGlyphs.TryGetPValue(key, Pointer(pdata)) then
//  begin
//    data.img := GenerateGlyphSDF(AFontName, AChar, FGlyphGenerationSize, FGlyphGenerationBorderSize, gsItalic in AStyle, gsBold in AStyle, gsUnderline in AStyle, data.XXXMetrics, data.YYYYMetrics);
//    FGlyphs.Add(key, data);
//    pdata := @data;
//  end;
//  Result := pdata^.img;
//  XXXMetrics := pdata^.XXXMetrics;
//  YYYYMetrics := pdata^.YYYYMetrics;
//end;

function TavCanvasCommonData.GetGlyphSprite(const AFontName: string;
  AChar: WideChar; AStyle: TGlyphStyles; out XXXMetrics: TVec3; out
  YYYYMetrics: TVec4): IGlyphIndex;
begin
//  GetGlyphImage(AFontName, AChar, AStyle, XXXMetrics, YYYYMetrics)
  Result := FGlyphsAtlas.ObtainGlyph(AFontName, AChar, AStyle);
  XXXMetrics := Result.XXXMetrics;
  YYYYMetrics := Result.YYYYMetrics;
end;

function TavCanvasCommonData.GetImageSprite(const AFileName: string): ISpriteIndex;
begin
  Result := GetTextureSprite(Default_ITextureManager.LoadTexture(AFileName, SIZE_DEFAULT, SIZE_DEFAULT, TImageFormat.A8R8G8B8, True).MipData(0,0));
end;

function TavCanvasCommonData.GetTextureSprite(const ATexture: ITextureMip): ISpriteIndex;
begin
  Result := FSpritesAtlas.ObtainSprite(ATexture);
end;

procedure TavCanvasCommonData.ReloadShaders;
const LOADFROMRES = True;
      DIR = 'D:\Projects\AvalancheProject\Canvas_Shaders\!Out\';
begin
  FLineProg.Load('CanvasLine', LOADFROMRES, DIR);
  FFontProg.Load('CanvasFont', LOADFROMRES, DIR);
  FTrisProg.Load('CanvasTris', LOADFROMRES, DIR);
end;

procedure TavCanvasCommonData.SetGlyphAtlasSize(const ATargetAtlasSize: TVec2i);
begin
    FGlyphsAtlas.TargetSize := ATargetAtlasSize;
end;

procedure TavCanvasCommonData.SetGlyphGenerationSize(AGlyphSize: Integer;
  ABorderSize: Integer);
begin
    FGlyphGenerationSize := AGlyphSize;
    FGlyphGenerationSize_Inv := 1 / FGlyphGenerationSize;
    FGlyphGenerationBorderSize := ABorderSize;
end;

procedure TavCanvasCommonData.ExportGlyphs(const AFileName: string; const AFontName: string; AStyle: TGlyphStyles; const ACharFirst, ACharLast: WideChar);
  type
    TExportInformation = record
      ch    : WideChar;
      XXX   : TVec3;
      YYYY  : TVec4;
      sprite: IGlyphIndex;
    end;
    procedure WriteGlyph(const AExp: TExportInformation; const AStream: TStream);   ///todo fixit
    var region : TSpriteRegion;
        picture: ITextureMip;
    begin
      //AStream.WriteBuffer(AExp.ch, SizeOf(AExp.ch));
      //AStream.WriteBuffer(AExp.XXX, SizeOf(AExp.XXX));
      //AStream.WriteBuffer(AExp.YYYY, SizeOf(AExp.YYYY));
      //region := GlyphsAtlas.GetRegion(AExp.sprite.Index);
      //AStream.WriteBuffer(region, SizeOf(region));
      //picture := AExp.sprite.Data;
      //Assert(picture.Width = region.Rect.z - region.Rect.x);
      //Assert(picture.Height = region.Rect.w - region.Rect.y);
      //if picture.Width * picture.Height > 0 then
      //  AStream.WriteBuffer(picture.Data^, picture.Width*picture.Height*SizeOf(Single));
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
    //for char := ACharLast downto ACharFirst do
    for char := ACharFirst to ACharLast do
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
  SetGlyphGenerationSize();

  FLineQuad := TavVB.Create(Self);
  FLineProg := TavUIProgram.Create(Self);
  FFontProg := TavFontProgram.Create(Self);
  FTrisProg := TavTrisProgram.Create(Self);

  Vert := TLineQuadVertices.Create;
  V.quadCoord := Vec(0.0, -1.0); Vert.Add(V);
  V.quadCoord := Vec(0.0,  1.0); Vert.Add(V);
  V.quadCoord := Vec(1.0, -1.0); Vert.Add(V);
  V.quadCoord := Vec(1.0,  1.0); Vert.Add(V);
  FLineQuad.Vertices := Vert as IVerticesData;
  FLineQuad.PrimType := ptTriangleStrip;

  FGlyphsAtlas := TavGlyphAtlas.Create(Self);
  FGlyphsAtlas.TargetFormat := TTextureFormat.R16f;

  FSpritesAtlas := TavAtlasArrayReferenced.Create(Self);
  FSpritesAtlas.TargetFormat := TTextureFormat.RGBA;
  FSpritesAtlas.sRGB := True;;
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

procedure TavCanvas.SetBrush(const AValue: TBrushStyle);
begin
  if FBrush = AValue then Exit;
  FBrush.Assign(AValue);
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
  Seg.Color := Pen.Color;
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

procedure TavCanvas.FillVertexWithBrush(out V: TCanvasTriangleVertex);
begin
  V.Color := Brush.Color;
  if TBrushHintingStyle.Horizontal in Brush.Hinting then
    V.Hinting.x := 1
  else
    V.Hinting.x := 0;

  if TBrushHintingStyle.Vertical in Brush.Hinting then
    V.Hinting.y := 1
  else
    V.Hinting.y := 0;
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
      gkTris   : Exit(FTrisData.Count);
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

procedure TavCanvas.AddQuad(const LeftTop, RightBottom: TVec2; const ASprite: ISpriteIndex);
var
  rct: TRectI;
begin
  if ASprite = nil then
    rct := RectI(0,0,1,1)
  else
    rct := RectI(Vec(0,0), ASprite.Size);
  AddQuadClipped(LeftTop, RightBottom, ASprite, rct);
end;

procedure TavCanvas.AddQuadClipped(const LeftTop, RightBottom: TVec2; const ASprite: ISpriteIndex; const ASpriteRect: TRectI);
var v: array [0..3] of TCanvasTriangleVertex;
    sInv: TVec2;
begin
  if ASprite <> nil then
    sInv := Vec(1,1)/ASprite.Size
  else
    sInv := Vec(1,1);

  SelectGeometryKind(gkTris);

  FillVertexWithBrush(v[0]);
  FillVertexWithBrush(v[1]);
  FillVertexWithBrush(v[2]);
  FillVertexWithBrush(v[3]);

  v[0].Coords := LeftTop;
  v[0].TexCoord := Vec(ASpriteRect.Left,ASpriteRect.Top) * sInv;
  v[0].Sprite := ASprite;

  v[1].Coords := Vec(LeftTop.x, RightBottom.y);
  v[1].TexCoord := Vec(ASpriteRect.Left, ASpriteRect.Bottom) * sInv;
  v[1].Sprite := ASprite;

  v[2].Coords := Vec(RightBottom.x, LeftTop.y);
  v[2].TexCoord := Vec(ASpriteRect.Right, ASpriteRect.Top) * sInv;
  v[2].Sprite := ASprite;

  v[3].Coords := RightBottom;
  v[3].TexCoord := Vec(ASpriteRect.Right, ASpriteRect.Bottom) * sInv;
  v[3].Sprite := ASprite;

  FTrisData.Add(v[0]);
  FTrisData.Add(v[1]);
  FTrisData.Add(v[2]);

  FTrisData.Add(v[2]);
  FTrisData.Add(v[1]);
  FTrisData.Add(v[3]);
end;

function TavCanvas.TextBuilder: ITextBuilder;
begin
  Result := TTextBuilder.Create(FFont, CommonData);
end;

function TavCanvas.GetSprite(const AFileName: string): ISpriteIndex;
begin
  Result := CommonData.GetImageSprite(AFileName);
end;

function TavCanvas.GetSprite(const ATexture: ITextureMip): ISpriteIndex;
begin
  Result := CommonData.GetTextureSprite(ATexture);
end;

procedure TavCanvas.Clear;
begin
  SelectGeometryKind(gkUnknown);

  FLineData.Clear();
  FVBLines.Invalidate;

  FTextLines.Clear();
  FTextLineRanges.Clear();
  FGlyphsData.Clear();
  FVBGlyphs.Invalidate;

  FTrisData.Clear();
  FVBTris.Invalidate;

  FGeometryBatches.Clear();
end;

procedure TavCanvas.AddLine(const Start, Stop: TVec2);
var Seg: TLinePointVertex;
begin
  SelectGeometryKind(gkLines);

  FillSegmentByPen(Seg);

  Seg.Coords.xy := Start;
  Seg.Coords.zw := Stop;
  Seg.Normals.xy := Normalize(Rotate90(Stop - Start, False));
  Seg.Normals.zw := Seg.Normals.xy;
  FLineData.Add(Seg);
end;

procedure TavCanvas.AddPolyline(const APts: array of TVec2; AClosed: Boolean);

  procedure EvalNormals(const p1, p2, p3, p4: TVec2; CosBreakTolerance: Single; out n1, n2: TVec2);
  var d1, d2, d3: TVec2;
      D, C: Single;
  begin
    d1 := NormalizeSafe(p2 - p1, Vec(0,0));
    d2 := NormalizeSafe(p3 - p2, Vec(0,0));
    d3 := NormalizeSafe(p4 - p3, Vec(0,0));

    C := Cross(d1, d2);
    D := Dot(d1, -d2);
    if (D > CosBreakTolerance) or (abs(C) < EPS) then
      n1 := Rotate90(d2, True)
    else
    begin
      D := sqrt(Max(0.5 - D*0.5, 0))*Sign(C);
      n1 := Normalize(d1 - d2) / D;
    end;

    C := Cross(d2, d3);
    D := Dot(d2, -d3);
    if (D > CosBreakTolerance) or (abs(C) < EPS) then
      n2 := Rotate90(d2, True)
    else
    begin
      D := sqrt(Max(0.5 - D*0.5, 0))*Sign(C);
      n2 := Normalize(d2 - d3) / D;
    end;
  end;

var Seg: TLinePointVertex;
    i, n: Integer;
    cosBreak: Single;
begin
  if Length(APts) < 2 then Exit;
  cosBreak := Cos(FPen.JointsBreakAngle);

  SelectGeometryKind(gkLines);

  FillSegmentByPen(Seg);

  if AClosed then
  begin
    n := Length(APts);
    for i := 0 to n - 1 do
    begin
      Seg.Coords.xy := APts[i];
      Seg.Coords.zw := APts[(i+1) mod n];
      EvalNormals(APts[(i-1+n) mod n], APts[i], APts[(i+1) mod n], APts[(i+2) mod n], cosBreak, Seg.Normals.xy, Seg.Normals.zw);
      FLineData.Add(Seg);
    end;
  end
  else
  begin
    n := Length(APts) - 1;
    for i := 0 to n - 1 do
    begin
      Seg.Coords.xy := APts[i];
      Seg.Coords.zw := APts[i+1];
      EvalNormals(APts[Max(i-1, 0)], APts[i], APts[i+1], APts[Min(i+2, n)], cosBreak, Seg.Normals.xy, Seg.Normals.zw);
      FLineData.Add(Seg);
    end;
  end;
end;

procedure TavCanvas.AddRectangle(Left, Top, Right, Bottom: Single);
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

procedure TavCanvas.AddRectangle(LeftTop, RightBottom: TVec2);
begin
  AddRectangle(LeftTop.x, LeftTop.y, RightBottom.x, RightBottom.y);
end;

procedure TavCanvas.AddText(const AText: ITextLines);
var range: TVec2i;
begin
  if AText = nil then Exit;
  SelectGeometryKind(gkFont);

  FTextLines.Add(AText);
  range.x := FGlyphsData.Count;
  FGlyphsData.AddArray(AText.AllGlyphs());
  range.y := FGlyphsData.Count - range.x;
  FTextLineRanges.Add(range);
end;

procedure TavCanvas.AddFill(const LeftTop, RightBottom: TVec2);
begin
  AddQuad(LeftTop, RightBottom, nil);
end;

procedure TavCanvas.AddPolyFill(const APoly: array of TPolyContour);
var convex: TContours;
    c: TVec2Arr;
    i, j: Integer;
    v: array [0..2] of TCanvasTriangleVertex;
    vidx: Integer;
begin
  if Length(APoly) = 0 then Exit;
  convex := PolyToConvex(APoly);
  if Length(convex) = 0 then Exit;

  SelectGeometryKind(gkTris);

  FillVertexWithBrush(v[0]);
  FillVertexWithBrush(v[1]);
  FillVertexWithBrush(v[2]);
  v[0].TexCoord := Vec(0,0);
  v[1].TexCoord := Vec(0,0);
  v[2].TexCoord := Vec(0,0);
  v[0].Sprite := nil;
  v[1].Sprite := nil;
  v[2].Sprite := nil;
  for j := 0 to Length(convex) - 1 do
  begin
    c := convex[j];
    if Length(c) < 3 then Continue;
    v[2].Coords := c[0];
    v[0].Coords := c[1];
    vidx := 0;
    for i := 2 to Length(c) - 1 do
    begin
      vidx := vidx xor 1;
      v[vidx].Coords := c[i];
      FTrisData.Add(v[2]);
      FTrisData.Add(v[vidx xor 1]);
      FTrisData.Add(v[vidx]);
    end;
  end;
end;

procedure TavCanvas.AddTriangle(const V1, V2, V3: TVec2);
var v: TCanvasTriangleVertex;
begin
  SelectGeometryKind(gkTris);

  FillVertexWithBrush(v);
  v.TexCoord := Vec(0,0);
  v.Sprite := nil;

  v.Coords := V1;
  FTrisData.Add(v);
  v.Coords := V2;
  FTrisData.Add(v);
  v.Coords := V3;
  FTrisData.Add(v);
end;

procedure TavCanvas.AddSprite(const LeftTop, RightBottom: TVec2; const AFileName: string);
begin
  AddQuad(LeftTop, RightBottom, CommonData.GetImageSprite(AFileName));
end;

procedure TavCanvas.AddSprite(const LeftTop, RightBottom: TVec2; const ASprite: ISpriteIndex);
begin
  AddQuad(LeftTop, RightBottom, ASprite);
end;

procedure TavCanvas.AddSpriteClipped(const LeftTop, RightBottom: TVec2; const AFileName: string; const AClipRect: TRectI);
begin
  AddQuadClipped(LeftTop, RightBottom, CommonData.GetImageSprite(AFileName), AClipRect);
end;

procedure TavCanvas.AddSpriteClipped(const LeftTop, RightBottom: TVec2; const ASprite: ISpriteIndex; const AClipRect: TRectI);
begin
  AddQuadClipped(LeftTop, RightBottom, ASprite, AClipRect);
end;

procedure TavCanvas.Draw(const ARotation: Single; const AOffset: TVec2; const APixelToUnit: Single);
begin
  Draw(ARotation, AOffset, APixelToUnit, IdentityMat4);
end;

procedure TavCanvas.Draw(const ATransform: TMat3);
begin
  Draw(ATransform, IdentityMat4);
end;

procedure TavCanvas.Draw(const ARotation: Single; const AOffset: TVec2; const APixelToUnit: Single; const AColorFilter: TMat4);
begin
  Draw( Mat3(Vec(APixelToUnit, APixelToUnit), ARotation, AOffset), AColorFilter );
end;

procedure TavCanvas.Draw(const ATransform: TMat3; const AColorFilter: TMat4);

  function InitProg(const AProg: TavUIProgram): Boolean;
  begin
    if UseCustomUIMatrix then
      AProg.SetCustomUIMatrix(FCustomUIMatrix);
    AProg.SetCanvasTransform(ATransform);
    AProg.SetPixelToUnit(Len(ATransform.OX));
    AProg.SetZValue(FZValue);
    AProg.SetColorFilters(AColorFilter);
    Result := True;
  end;

var prog: TavUIProgram;
    fontprog: TavFontProgram;
    batch: TGeometryBatch;
    gk: TGeometryKind;
    progInited: array [TGeometryKind] of Boolean;
    i, j: Integer;
    textRange: TVec2i;

    BoundsY : TVec2;
    VAlign, YPos: Single;
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

          fontprog := CommonData.FontProg;
          fontprog.Select;
          fontprog.SetAttributes(nil, nil, FVBGlyphs);
          if not progInited[batch.kind] then
            progInited[batch.kind] := InitProg(fontprog);

          for j := batch.ranges.x to batch.ranges.x + batch.ranges.y - 1 do
          begin
            VAlign := FTextLines[j].VAlign;
            BoundsY := FTextLines[j].BoundsY;
            YPos := Lerp(0, -FTextLines[j].TotalHeight(), VAlign) + Lerp(BoundsY.x, BoundsY.y, VAlign) + FTextLines[j].VScroll;
            fontprog.SetBoundsXY(FTextLines[j].BoundsX, FTextLines[j].BoundsY);
            fontprog.SetYPos_ClipWithBounds(YPos, FTextLines[j].ClipWithBounds);
            textRange := FTextLineRanges[j];
            if textRange.y = 0 then Continue;
            fontprog.Draw(ptTriangleStrip, cmNone, false, textRange.y, 0, 4, 0, textRange.x);
          end;
        end;
      gkTris:
        begin
          prog := CommonData.TrisProg;
          prog.Select;
          prog.SetAttributes(FVBTris, nil, nil);
          if not progInited[batch.kind] then
            progInited[batch.kind] := InitProg(prog);
          prog.Draw(0, batch.ranges.x, batch.ranges.y);
        end;
    end;
  end;
end;

constructor TavCanvas.Create(AParent: TavObject);
begin
  inherited Create(AParent);
  FZValue := 0.5;
  FGeometryBatches := TGeometry.Create;

  FPen := TPenStyle.Create;
  FBrush := TBrushStyle.Create;
  FFont:= TFontStyle.Create;

  FLineData := TLinePointVertices.Create;
  FVBLines := TavVB.Create(Self);
  FVBLines.Vertices := FLineData as IVerticesData;

  FTextLines := TTextLinesArr.Create();
  FGlyphsData:= TGlyphVertices.Create();
  FTextLineRanges := TVec2iArray.Create();
  FVBGlyphs := TavVB.Create(Self);
  FVBGlyphs.Vertices := FGlyphsData as IVerticesData;

  FTrisData := TCanvasTriangleVertices.Create;
  FVBTris   := TavVB.Create(Self);
  FVBTris.Vertices := FTrisData as IVerticesData;
  FVBTris.CullMode := cmNone;
  FVBTris.PrimType := ptTriangles;

  FCurrentBatch.kind := gkUnknown;
end;

destructor TavCanvas.Destroy;
begin
  FreeAndNil(FPen);
  FreeAndNil(FBrush);
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
  PenDest.FColor := FColor;
  PenDest.FAlign := FAlign;
  PenDest.FHinting := FHinting;
  PenDest.JointsBreakAngle := FJointsBreakAngle;
  PenDest.FMinPixWidth := FMinPixWidth;
  PenDest.FWidth := FWidth;
  PenDest.FPattern := FPattern;
  PenDest.FPatternTransform := FPatternTransform;
end;

procedure TPenStyle.AfterConstruction;
begin
  inherited AfterConstruction;
  FWidth := 1;
  FColor := Vec(0,0,0,1);
  FHinting := cPenHintingAll;
  FJointsBreakAngle := Pi/8;
end;

procedure TPenStyle.SetHinting(AValue: TPenHinting);
begin
  if FHinting = AValue then Exit;
  FHinting := AValue;
end;

procedure TPenStyle.SetJointsBreakAngle(AValue: Single);
begin
  if FJointsBreakAngle = AValue then Exit;
  FJointsBreakAngle := AValue;
end;

procedure TPenStyle.SetMinPixWidth(AValue: Integer);
begin
  if FMinPixWidth = AValue then Exit;
  FMinPixWidth := AValue;
end;

procedure TPenStyle.SetPattern(const AValue: ITextureMip);
begin
  if FPattern = AValue then Exit;
  FPattern := AValue;
end;

procedure TPenStyle.SetPatternTransform(const AValue: TMat3);
begin
  if FPatternTransform = AValue then Exit;
  FPatternTransform := AValue;
end;

procedure TPenStyle.SetAlign(AValue: TPenAlign);
begin
  if FAlign = AValue then Exit;
  FAlign := AValue;
end;

procedure TPenStyle.SetColor(const AValue: TVec4);
begin
  if FColor = AValue then Exit;
  FColor := AValue;
end;

end.
