unit avGlyphGenerator;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avTypes, mutils, avContnrs;

const
  GLYPH_DFOverscale = 3;

type
  TGlyphContour = {$IfDef FPC}specialize{$EndIf}TArray<TVec2>;
  IGlyphContour = {$IfDef FPC}specialize{$EndIf}IArray<TVec2>;
  TGlyphPoly = {$IfDef FPC}specialize{$EndIf}TArray<IGlyphContour>;
  IGlyphPoly = {$IfDef FPC}specialize{$EndIf}IArray<IGlyphContour>;

function GenerateGlyphImage(const AFontName  : string;
                            const AChar      : WideChar;
                            const ASize      : Integer;
                            const AItalic    : Boolean;
                            const ABold      : Boolean;
                            const AUnderLine : Boolean;
                            out   ABCMetrics : TVec3I): ITextureMip;

function GenerateGlyphSDF(const AFontName  : string;
                          const AChar      : WideChar;
                          const ASize      : Integer;
                          const ABorder    : Integer;
                          const AItalic    : Boolean;
                          const ABold      : Boolean;
                          const AUnderLine : Boolean;
                          out   XXX        : TVec3;
                          out   YYYY       : TVec4): ITextureMip;

function GenerateGlyphOutline(const AFontName  : string;
                              const AChar      : WideChar;
                              const AItalic    : Boolean;
                              const ABold      : Boolean;
                              const AUnderLine : Boolean;
                              out   XXX        : TVec3;
                              out   YYYY       : TVec4): IGlyphPoly;

implementation

uses Math, Windows, Graphics{$IfDef DCC}, Types{$EndIf};

type

  { TGlyphImage }

  TGlyphImage = class (TInterfacedObject, ITextureMip)
  private
    FData  : TVec4bArr;
    FWidth : Integer;
  public
    function Width      : Integer;
    function Height     : Integer;
    function Data       : PByte;
    function PixelFormat: TImageFormat;
    function Replicate  : ITextureMip;
    function Pixel(const x,y: Integer): PByte;
    constructor Create(const AWidth: Integer; const AData: TVec4bArr);
  end;

  { TGlyphMonochrome }

  TGlyphMonochrome = record
    buf    : TByteArr;
    YY     : TVec2i;
    metrics: TGlyphMetrics;

    procedure SaveToFile(const AFileName: string);
  end;

  IVec2Field = interface
    function GetSize: TVec2i;
    function GetData: TVec2Arr;
  end;

  { TVec2Field }

  TVec2Field = class (TInterfacedObject, IVec2Field)
  private
    const
      DIST_INFINITY : TVec2 = (x: Infinity; y: Infinity);
      DIST_ZERO     : TVec2 = (x: 0; y: 0);
  private
    FWidth : Integer;
    FHeight: Integer;
    FData  : TVec2Arr;

    function  GetPix(const x, y: Integer): TVec2;
    procedure SetPix(const x, y: Integer; const v: TVec2);

    procedure ReBuild(const AInverted: Boolean);

    function GetSize: TVec2i;
    function GetData: TVec2Arr;
  public
    procedure SaveToFile(const AFileName: string);

    constructor Create(const ABmp: TBitmap; const AInverted: Boolean); overload;
    constructor Create(const AGlyph: TGlyphMonochrome; const ABorder: Integer; const AInverted: Boolean); overload;
  end;

  { TDField }

  TDField = class (TInterfacedObject, ITextureMip)
  private
    FData  : TSingleArr;
    FHeight: Integer;
    FWidth : Integer;
    procedure DownscaleOnce;
  private
    function Width      : Integer;
    function Height     : Integer;
    function Data       : PByte;
    function PixelFormat: TImageFormat;
    function Replicate  : ITextureMip;
    function Pixel(const x,y: Integer): PByte;
  public
    procedure Downscale(NTimes: Integer);
    constructor Create(const ACopyFrom: TDField); overload;
    constructor Create(const PositiveField, NegativeField: IVec2Field); overload;
  end;

{$IfDef FPC}
procedure GetTextSize(const Canvas: TCanvas; AChar: WideChar; out w, h: Integer);
begin
  w := 0;
  h := 0;
  Canvas.GetTextSize(AChar, w, h);
end;
{$EndIf}

{$IfDef DCC}
procedure GetTextSize(const Canvas: TCanvas; AChar: WideChar; out w, h: Integer);
var size: TSize;
begin
  size := Canvas.TextExtent(AChar);
  w := size.cx;
  h := size.cy;
end;
{$EndIf}

function GetGlyph(const Canvas: TCanvas; AChar: WideChar): TGlyphMonochrome;
const
  F_IDENTITY : Windows.TMAT2 = (eM11: (fract: 0; value: 1); eM12: (fract: 0; value: 0); eM21: (fract: 0; value: 0); eM22: (fract: 0; value: 1));
var ch: Integer;
    textM: TTextMetricW;
    bufSize: Cardinal;
begin
  if not GetTextMetricsW(Canvas.Handle, {$IfDef FPC}@{$EndIF}textM) then
    RaiseLastOSError;
  Result.YY.x := textM.tmAscent;
  Result.YY.y := textM.tmDescent;

  ch := Ord(AChar);
  bufSize := GetGlyphOutlineW(Canvas.Handle, ch, GGO_BITMAP, Result.metrics, 0, nil, F_IDENTITY);
  if bufSize = GDI_ERROR then
    RaiseLastOSError;

  if bufSize = 0 then
  begin
    Result.buf := nil;
    Exit;
  end;

  SetLength(Result.buf, bufSize);
  if GetGlyphOutlineW(Canvas.Handle, ch, GGO_BITMAP, Result.metrics, Length(Result.buf), @Result.buf[0], F_IDENTITY) = GDI_ERROR then
    RaiseLastOSError;
end;

function GetGlyphMetrics(const Canvas: TCanvas; AChar: WideChar): TVec3I;
var ch: Integer;
begin
  ch := Ord(AChar);
  {$IfDef FPC}
  if not GetCharABCWidthsW(Canvas.Handle, ch, ch, LPABC(@Result)) then
  {$Else}
  if not GetCharABCWidthsW(Canvas.Handle, ch, ch, Result) then
  {$EndIf}
    RaiseLastOSError;
end;

{$IfDef FPC}
  {$WARN 5044 off : Symbol "$1" is not portable}
{$EndIf}
function GenerateGlyphImage(const AFontName: string; const AChar: WideChar;
  const ASize: Integer; const AItalic: Boolean; const ABold: Boolean;
  const AUnderLine: Boolean; out ABCMetrics: TVec3I): ITextureMip;

type
  TRGB = packed record
    r, g, b: Byte;
  end;
  PRGB = ^TRGB;

var bmp: TBitmap;
    fstyle: TFontStyles;
    i, j, w, h: Integer;

    dstData: TVec4bArr;
    dstPix : PVec4b;
    srcPix : PRGB;
begin
  bmp := nil;
  try
    fstyle := [];
    if ABold then fstyle := fstyle + [fsBold];
    if AItalic then fstyle := fstyle + [fsItalic];
    if AUnderLine then fstyle := fstyle + [fsUnderline];

    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24bit;
    bmp.Canvas.Font.Name := AFontName;
    bmp.Canvas.Font.Height := ASize;
    bmp.Canvas.Font.Color := clWhite;
    bmp.Canvas.Font.Style := fstyle;
    bmp.Canvas.Font.Quality := fqAntialiased;
    bmp.Canvas.Brush.Color := clBlack;
    bmp.Canvas.Brush.Style := bsSolid;

    w := 0; h := 0;
    GetTextSize(bmp.Canvas, AChar, w, h);
    bmp.Width := w;
    bmp.Height := h;

    bmp.Canvas.TextOut(0, 0, string(AChar));

    SetLength(dstData, w*h);
    dstPix := @dstData[0];
    for j := 0 to h - 1 do
    begin
      srcPix := bmp.ScanLine[j];
      for i := 0 to w - 1 do
      begin
        dstPix^.x := 255;//srcPix^.r;
        dstPix^.y := 255;//srcPix^.g;
        dstPix^.z := 255;//srcPix^.b;
        dstPix^.w := srcPix^.r;
//        dstPix^.x := srcPix^.r;//srcPix^.r;
//        dstPix^.y := 255;//srcPix^.g;
//        dstPix^.z := 255;//srcPix^.b;
//        dstPix^.w := 255;
        Inc(srcPix);
        Inc(dstPix);
      end;
    end;

    Result := TGlyphImage.Create(w, dstData);

    ABCMetrics := GetGlyphMetrics(bmp.Canvas, AChar);
  finally
    FreeAndNil(bmp);
  end;
end;
{$IfDef FPC}
  {$WARN 5044 on : Symbol "$1" is not portable}
{$EndIf}

function GenerateGlyphSDF(const AFontName: string; const AChar: WideChar; const ASize: Integer; const ABorder: Integer; const AItalic: Boolean;
  const ABold: Boolean; const AUnderLine: Boolean; out XXX: TVec3; out YYYY: TVec4): ITextureMip;
var bmp: TBitmap;
    fstyle: TFontStyles;

    pos, neg: IVec2Field;
    df: TDField;
    fontH: Integer;
    scaledBorder, w, h: Integer;
    glyph: TGlyphMonochrome;
begin
  bmp := nil;
  try
    fstyle := [];
    if ABold then fstyle := fstyle + [fsBold];
    if AItalic then fstyle := fstyle + [fsItalic];
    if AUnderLine then fstyle := fstyle + [fsUnderline];

    fontH := ASize * (1 shl GLYPH_DFOverscale);
    scaledBorder := ABorder * (1 shl GLYPH_DFOverscale);

    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24bit;
    bmp.Canvas.Font.Name := AFontName;
    bmp.Canvas.Font.Height := fontH;
    bmp.Canvas.Font.Color := clWhite;
    bmp.Canvas.Font.Style := fstyle;
    bmp.Canvas.Font.Quality := fqNonAntialiased;
    bmp.Canvas.Brush.Color := clBlack;
    bmp.Canvas.Brush.Style := bsSolid;

    w := 0; h := 0;
    GetTextSize(bmp.Canvas, AChar, w, h);

    glyph := GetGlyph(bmp.Canvas, AChar);
    pos := TVec2Field.Create(glyph, scaledBorder, false);
    neg := TVec2Field.Create(glyph, scaledBorder, true);
    df := TDField.Create(pos, neg);
    df.Downscale(GLYPH_DFOverscale);
    Result := df;

    YYYY.x := glyph.YY.x - glyph.metrics.gmptGlyphOrigin.Y;
    YYYY.y := glyph.metrics.gmptGlyphOrigin.Y;
    YYYY.z := glyph.metrics.gmBlackBoxY - YYYY.y;
    YYYY.w := glyph.YY.x + glyph.YY.y - YYYY.x - YYYY.y - YYYY.z;

    Assert(YYYY.x + YYYY.y + YYYY.z + YYYY.w - fontH < 1);

    YYYY.x := YYYY.x - scaledBorder;
    YYYY.y := YYYY.y + scaledBorder;
    YYYY.z := YYYY.z + scaledBorder;
    YYYY.w := YYYY.w - scaledBorder;

    XXX.x := glyph.metrics.gmptGlyphOrigin.X;
    XXX.y := glyph.metrics.gmBlackBoxX;
    XXX.z := Integer(glyph.metrics.gmCellIncX) - Integer(glyph.metrics.gmBlackBoxX) - glyph.metrics.gmptGlyphOrigin.X;

    XXX.x := XXX.x - scaledBorder;
    XXX.y := XXX.y + 2 * scaledBorder;
    XXX.z := XXX.z - scaledBorder;

    XXX  := XXX  * (1.0 / (1 shl GLYPH_DFOverscale));
    YYYY := YYYY * (1.0 / (1 shl GLYPH_DFOverscale));
  finally
    FreeAndNil(bmp);
  end;
end;

function GenerateGlyphOutline(const AFontName: string; const AChar: WideChar; const AItalic: Boolean; const ABold: Boolean;
  const AUnderLine: Boolean; out XXX: TVec3; out YYYY: TVec4): IGlyphPoly;

  function ToCoord(const AFixed: Windows.FIXED): Single; inline;
  begin
    Result := Integer(AFixed) / 65536.0
  end;

  function ToVec(const APoint: TPOINTFX): TVec2; inline;
  begin
    Result.x := ToCoord(APoint.x);
    Result.y := ToCoord(APoint.y);
  end;

  procedure ApproxBezier2(const APt1, APt2, APt3: TVec2; ATolerance: Single; const APath: IGlyphContour);
  var lpt1, lpt2, lpt3, rpt1, rpt2, rpt3: TVec2;
  begin
    if Bezier2IsLine(APt1, APt2, APt3, ATolerance) then
    begin
      if APath.Last <> APt3 then
        APath.Add(APt3);
    end
    else
    begin
      Bezier2Split(APt1, APt2, APt3, 0.5, lpt1, lpt2, lpt3, rpt1, rpt2, rpt3);
      ApproxBezier2(lpt1, lpt2, lpt3, ATolerance, APath);
      ApproxBezier2(rpt1, rpt2, rpt3, ATolerance, APath);
    end;
  end;

  procedure ApproxBezier3(const APt1, APt2, APt3, APt4: TVec2; ATolerance: Single; const APath: IGlyphContour);
  var lpt1, lpt2, lpt3, lpt4, rpt1, rpt2, rpt3, rpt4: TVec2;
  begin
    if Bezier3IsLine(APt1, APt2, APt3, APt4, ATolerance) then
    begin
      if APath.Last <> APt4 then
        APath.Add(APt4);
    end
    else
    begin
      Bezier3Split(APt1, APt2, APt3, APt4, 0.5, lpt1, lpt2, lpt3, lpt4, rpt1, rpt2, rpt3, rpt4);
      ApproxBezier3(lpt1, lpt2, lpt3, lpt4, ATolerance, APath);
      ApproxBezier3(rpt1, rpt2, rpt3, rpt4, ATolerance, APath);
    end;
  end;

const
  cToleranceScale = 0.0025;
const
  F_IDENTITY : Windows.TMAT2 = (eM11: (fract: 0; value: 1); eM12: (fract: 0; value: 0); eM21: (fract: 0; value: 0); eM22: (fract: 0; value: 1));
const GGO_BEZIER = 3;
      GGO_UNHINTED = $0100;
      TT_PRIM_CSPLINE = 3;
const GGO_FLAGS = GGO_NATIVE or GGO_UNHINTED;// or GGO_BEZIER;
var i, j: Integer;
    metrics: TGLYPHMETRICS;
    textM: TTextMetricW;
    bufSize: DWORD;
    Buf: array of Byte;
    polyHeader: PTTPOLYGONHEADER;
    polyCurve : PTTPOLYCURVE;
    path: IGlyphContour;
    pB, pC, pD: TVec2;
    startoffset, curveoffset: Integer;
    cTol: Single;

var bmp: TBitmap;
    cnv: TCanvas;
    fstyle: TFontStyles;
begin
  Result := TGlyphPoly.Create();
  bmp := nil;
  cnv := nil;
  try
    fstyle := [];
    if ABold then fstyle := fstyle + [fsBold];
    if AItalic then fstyle := fstyle + [fsItalic];
    if AUnderLine then fstyle := fstyle + [fsUnderline];

    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24bit;
    cnv := bmp.Canvas;
    cnv.Font.Name := AFontName;
    cnv.Font.Height := 32*(1 shl GLYPH_DFOverscale);
    cnv.Font.Style := fstyle;

    bufSize := GetGlyphOutlineW(cnv.Handle, Ord(AChar), GGO_FLAGS, metrics, 0, nil, F_IDENTITY);
    if bufSize = GDI_ERROR then
      RaiseLastOSError;
    SetLength(Buf, bufSize);
    GetGlyphOutlineW(cnv.Handle, Ord(AChar), GGO_FLAGS, metrics, bufSize, @Buf[0], F_IDENTITY);
    cTol := max(metrics.gmBlackBoxX, metrics.gmBlackBoxY)*cToleranceScale;

    polyHeader := nil;
    curveoffset := 0;
    startoffset := 0;
    while curveoffset < bufSize do
    begin
      if (polyHeader = nil) or (curveoffset-startoffset = polyHeader^.cb) then
      begin
        polyHeader := @Buf[curveoffset];
        Assert(polyHeader^.dwType = TT_POLYGON_TYPE);
        startoffset := curveoffset;
        Inc(curveoffset, SizeOf(TTPOLYGONHEADER));
        path := TGlyphContour.Create();
        Result.Add(path);
        path.Add(ToVec(polyHeader^.pfxStart));
      end;

      polyCurve := @Buf[curveoffset];
      case polyCurve^.wType of
        TT_PRIM_LINE :
          begin
            for i := 0 to polyCurve^.cpfx - 1 do
            begin
              pB := ToVec(polyCurve^.apfx[i]);
              if path.Last <> pB then path.Add(pB);
            end;
          end;
        TT_PRIM_QSPLINE :
          begin
            for i := 0 to polyCurve^.cpfx - 2 do
            begin
              pB := ToVec(polyCurve^.apfx[i]);
              pC := ToVec(polyCurve^.apfx[i+1]);
              if i < polyCurve^.cpfx - 2 then
                pC := (pB + pC)*0.5;
              ApproxBezier2(path.Last, pB, pC, cTol, path);
            end;
          end;
        TT_PRIM_CSPLINE :
          begin
            for i := 0 to polyCurve^.cpfx - 3 do
            begin
              if i = 0 then
              begin
                pB := ToVec(polyCurve^.apfx[i]);
              end
              else
              begin
                if i = polyCurve^.cpfx - 3 then
                  pB := (ToVec(polyCurve^.apfx[i]) + ToVec(polyCurve^.apfx[i+1]))*0.5
                else
                  pB := lerp(ToVec(polyCurve^.apfx[i]), ToVec(polyCurve^.apfx[i+1]), 1/3);
              end;

              if i = polyCurve^.cpfx - 3 then
              begin
                pC := ToVec(polyCurve^.apfx[i+1]);
              end
              else
              begin
                if i = 0 then
                  pC := (ToVec(polyCurve^.apfx[i]) + ToVec(polyCurve^.apfx[i+1]))*0.5
                else
                  pC := lerp(ToVec(polyCurve^.apfx[i]), ToVec(polyCurve^.apfx[i+1]), 2/3);
              end;

              if i = polyCurve^.cpfx - 3 then
                pD := ToVec(polyCurve^.apfx[i+2])
              else
              begin
                if i+1 = polyCurve^.cpfx - 3 then
                  pD := (ToVec(polyCurve^.apfx[i+1]) + ToVec(polyCurve^.apfx[i+2]))*0.5
                else
                  pD := lerp(ToVec(polyCurve^.apfx[i+1]), ToVec(polyCurve^.apfx[i+2]), 1/3);
                pD := (pD + pC) * 0.5;
              end;
              ApproxBezier3(path.Last, pB, pC, pD, cTol, path);
            end;
          end;
      end;
      curveoffset := curveoffset + SizeOf(polyCurve^.wType) + SizeOf(polyCurve^.cpfx) + SizeOf(POINTFX)*polyCurve^.cpfx;
    end;

    for i := Result.Count - 1 downto 0 do
    begin
      path := Result[i];
      if path.Count < 3 then
      begin
        Result.Delete(i);
        Continue;
      end;
      if path.Last = path[0] then
        path.Delete(path.Count - 1);
    end;

    if not GetTextMetricsW(cnv.Handle, {$IfDef FPC}@{$EndIF}textM) then
      RaiseLastOSError;
    YYYY.x := textM.tmAscent - metrics.gmptGlyphOrigin.Y;
    YYYY.y := metrics.gmptGlyphOrigin.Y;
    YYYY.z := metrics.gmBlackBoxY - YYYY.y;
    YYYY.w := textM.tmAscent + textM.tmDescent - YYYY.x - YYYY.y - YYYY.z;

    XXX.x := metrics.gmptGlyphOrigin.X;
    XXX.y := metrics.gmBlackBoxX;
    XXX.z := metrics.gmCellIncX - metrics.gmBlackBoxX - metrics.gmptGlyphOrigin.X;

    for i := 0 to Result.Count - 1 do
    begin
      path := Result[i];
      for j := 0 to path.Count - 1 do
      begin
        PVec2( path.PItem[j] )^.x := path[j].x-XXX.x;
        PVec2( path.PItem[j] )^.y := YYYY.y-path[j].y;
      end;
    end;
  finally
    FreeAndNil(bmp);
  end;
end;

{ TGlyphMonochrome }

{$IfDef FPC}
  {$WARN 5044 off : Symbol "$1" is not portable}
{$EndIf}
procedure TGlyphMonochrome.SaveToFile(const AFileName: string);

  procedure SetPix(bmp: TBitmap; x, y: Integer; b: Byte);
  var pdest: PVec3b;
  begin
    pdest := bmp.ScanLine[y];
    Inc(pdest, x);
    pdest^.x := b;
    pdest^.y := b;
    pdest^.z := b;
  end;

var bmp: TBitmap;
  rowsize: UINT;
  j, i: Integer;
  row: PByte;
begin
  bmp := nil;
  try
    bmp := TBitmap.Create;
    bmp.PixelFormat := pf24bit;
    bmp.Width := metrics.gmBlackBoxX;
    bmp.Height := metrics.gmBlackBoxY;

    if Length(buf) > 0 then
    begin

      rowsize := ((metrics.gmBlackBoxX + 31) div 32) * 4;
      for j := 0 to metrics.gmBlackBoxY - 1 do
      begin
        row := @buf[Cardinal(j) * rowsize];
        for i := 0 to metrics.gmBlackBoxX - 1 do
        begin
          if ((TByteArr(row)[i div 8] shr (7 - i mod 8)) and 1 = 1) then
            SetPix(bmp, i, j, 255)
          else
            SetPix(bmp, i, j, 0);
        end;
      end;

    end;

    bmp.SaveToFile(AFileName);
  finally
    FreeAndNil(bmp);
  end;
end;
{$IfDef FPC}
  {$WARN 5044 on: Symbol "$1" is not portable}
{$EndIf}

{ TDField }

procedure TDField.DownscaleOnce;

  function GetPixel(const x, y: Integer): Single; {$IfDef FPC}inline;{$EndIf}
  begin
    Result := FData[y * FWidth + x];
  end;

var newWidth : Integer;
    newHeight: Integer;
    newData  : TSingleArr;
    i, j, x, y: Integer;
    pix: Single;
begin
  newWidth := FWidth div 2;
  newHeight := FHeight div 2;
  SetLength(newData, newWidth * newHeight);
  for j := 0 to newHeight - 1 do
    for i := 0 to newWidth - 1 do
    begin
      x := i * 2;
      y := j * 2;
      pix := GetPixel(x, y) + GetPixel(x + 1, y) + GetPixel(x, y + 1) + GetPixel(x + 1, y + 1);
      newData[j * newWidth + i] := pix*0.25*0.5;
    end;
  FWidth := newWidth;
  FHeight := newHeight;
  FData := newData;
end;

function TDField.Width: Integer;
begin
  Result := FWidth;
end;

function TDField.Height: Integer;
begin
  Result := FHeight;
end;

function TDField.Data: PByte;
begin
  Result := @FData[0];
end;

function TDField.PixelFormat: TImageFormat;
begin
  Result := TImageFormat.R32F;
end;

function TDField.Replicate: ITextureMip;
begin
  Result := TDField.Create(Self);
end;

function TDField.Pixel(const x, y: Integer): PByte;
begin
  Result := PByte(@FData[y*FWidth + x]);
end;

constructor TDField.Create(const ACopyFrom: TDField);
begin
  FWidth := ACopyFrom.FWidth;
  FHeight := ACopyFrom.FHeight;
  FData := Copy(ACopyFrom.FData, 0, Length(ACopyFrom.FData));
end;

procedure TDField.Downscale(NTimes: Integer);
begin
  while NTimes > 0 do
  begin
    DownscaleOnce;
    Dec(NTimes);
  end;
end;

constructor TDField.Create(const PositiveField, NegativeField: IVec2Field);
var size: TVec2i;
    srcPos: TVec2Arr;
    srcNeg: TVec2Arr;
    i: Integer;
begin
  size := PositiveField.GetSize;
  Assert(size.x > 0);
  Assert(size.y > 0);
  Assert(size = NegativeField.GetSize());

  FWidth := size.x;
  FHeight := size.y;
  SetLength(FData, size.x * size.y);

  srcPos := PositiveField.GetData;
  srcNeg := NegativeField.GetData;
  for i := 0 to Length(FData) - 1 do
    FData[i] := Len(srcPos[i]) - Len(srcNeg[i]);
end;

{ TVec2Field }

function TVec2Field.GetPix(const x, y: Integer): TVec2;
begin
  Result := FData[y * FWidth + x];
end;

procedure TVec2Field.SetPix(const x, y: Integer; const v: TVec2);
begin
  FData[y * FWidth + x] := v;
end;

// MEIJSTER distance transform alghoritm
// http://fab.cba.mit.edu/classes/S62.12/docs/Meijster_distance.pdf
procedure TVec2Field.ReBuild(const AInverted: Boolean);
type
  TEnv = record
    StartPos : Integer;
    IntPos   : Integer;
  end;

  function GetPixelY(const data: TVec2Arr; const x, y: Integer): Single; {$IfDef FPC}inline;{$EndIf}
  begin
    Result := data[y * FWidth + x].y;
  end;

  procedure SetPixel(const data: TVec2Arr; const x, y: Integer; const v: TVec2); {$IfDef FPC}inline;{$EndIf}
  begin
    data[y * FWidth + x] := v;
  end;

  function GetPixelBordered(const data: TVec2Arr; const x, y: Integer): TVec2; {$IfDef FPC}inline;{$EndIf}
  begin
    if (x < 0) or (y < 0) or (x >= FWidth) or (y >= FHeight) then
    begin
      if AInverted then
        Result := DIST_ZERO
      else
        Result := DIST_INFINITY;
    end
    else
      Result := data[y * FWidth + x];
  end;

  function DistSqr(const dx, dy: Single): Single;
  begin
    Result := dx*dx + dy*dy;
  end;

  function Sep(const x1, x2, dy1, dy2: Single): Integer;
  begin
    Result := Floor((x2*x2 - x1*x1 + dy2*dy2 - dy1*dy1)/(2*(x2-x1)));
  end;

var
    env: array of TEnv;
    envIdx: Integer;
    copy: TVec2Arr;
    i, j, w: Integer;
    Pix1, Pix2: TVec2;
begin
  SetLength(copy, Length(FData));
  Move(FData[0], copy[0], Length(FData) * SizeOf(FData[0]));

  //two vertical passes
  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
    begin
      Pix1 := GetPixelBordered(copy, i, j-1);
      Pix1.x := 0;
      Pix1.y := Pix1.y-1;
      if Abs(Pix1.y) < Abs(GetPixelY(copy, i, j)) then SetPixel(copy, i, j, Pix1);
    end;

  for j := FHeight - 1 downto 0 do
    for i := 0 to FWidth - 1 do
    begin
      Pix1 := GetPixelBordered(copy, i, j+1);
      Pix1.x := 0;
      Pix1.y := Pix1.y+1;
      if Abs(Pix1.y) < Abs(GetPixelY(copy, i, j)) then SetPixel(copy, i, j, Pix1);
    end;

  //horizontal pass with envelopes
  SetLength(env, FWidth+2);
  for j := 0 to FHeight - 1 do
  begin
    envIdx := 0;
    env[0].StartPos := -1;
    env[0].IntPos := -1;
    for i := 0 to FWidth do
    begin
      Pix1 := GetPixelBordered(copy, i, j);
      Pix2 := GetPixelBordered(copy, env[envIdx].StartPos, j);
      if IsInfinite(Pix1.y) then Continue;

      while (envIdx>=0) and (DistSqr(env[envIdx].IntPos - env[envIdx].StartPos, Pix2.y) > DistSqr(env[envIdx].IntPos - i, Pix1.y)) do
      begin
        Dec(envIdx);
        if (envIdx >= 0) then
            Pix2 := GetPixelBordered(copy, env[envIdx].StartPos, j);
      end;

      if envIdx < 0 then
      begin
        envIdx := 0;
        env[envIdx].StartPos := i;
      end
      else
      begin
        w := 1 + Sep(env[envIdx].StartPos, i, Pix2.y, Pix1.y);
        if w < FWidth then
        begin
          Inc(envIdx);
          env[envIdx].StartPos := i;
          env[envIdx].IntPos := w;
        end;
      end;
    end;

    for i := FWidth - 1 downto 0 do
    begin
      Pix1 := GetPixelBordered(copy, env[envIdx].StartPos, j);
      Pix1.x := i - env[envIdx].StartPos;
      SetPixel(FData, i, j, Pix1);
      if i = env[envIdx].IntPos then Dec(envIdx);
    end;
  end;
end;

function TVec2Field.GetSize: TVec2i;
begin
  Result := Vec(FWidth, FHeight)
end;

function TVec2Field.GetData: TVec2Arr;
begin
  Result := FData;
end;

{$IfDef FPC}
  {$WARN 5044 off : Symbol "$1" is not portable}
{$EndIf}
procedure TVec2Field.SaveToFile(const AFileName: string);
var bmp: TBitmap;
    j, i: Integer;
    pdst: PVec3b;
    v: TVec2;
begin
  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf24bit;
    bmp.Width := FWidth;
    bmp.Height := FHeight;

    for j := 0 to FHeight - 1 do
    begin
      pdst := bmp.ScanLine[j];
      for i := 0 to FWidth - 1 do
      begin
        v := GetPix(i, j);
        pdst^.x := Clamp(Round(abs(v.x) / 10 * 255), 0, 255);
        pdst^.y := Clamp(Round(abs(v.y) / 10 * 255), 0, 255);
        pdst^.z := 0;
        Inc(pdst);
      end;
    end;

    bmp.SaveToFile(AFileName);
  finally
    FreeAndNil(bmp);
  end;
end;

constructor TVec2Field.Create(const ABmp: TBitmap; const AInverted: Boolean);
var src: PVec3b;
    dst: PVec2;
    j, i: Integer;
begin
  Assert(ABmp.PixelFormat = pf24bit);
  Assert(ABmp.Width > 0);
  Assert(ABmp.Height > 0);
  FWidth := ABmp.Width;
  FHeight := ABmp.Height;
  SetLength(FData, ABmp.Width * ABmp.Height);
  dst := @FData[0];
  for j := 0 to ABmp.Height - 1 do
  begin
    src := ABmp.ScanLine[j];
    for i := 0 to ABmp.Width - 1 do
    begin
      if (src^.x = 0) xor AInverted then
        dst^ := DIST_INFINITY
      else
        dst^ := DIST_ZERO;
      Inc(dst);
      Inc(src);
    end;
  end;

  ReBuild(AInverted);
end;
{$IfDef FPC}
  {$WARN 5044 on : Symbol "$1" is not portable}
{$EndIf}

constructor TVec2Field.Create(const AGlyph: TGlyphMonochrome; const ABorder: Integer; const AInverted: Boolean);
var i, j: Integer;
    rowsize: integer;
    row: PByte;
    v: TVec2;
begin
  FWidth := Integer(AGlyph.metrics.gmBlackBoxX) + 2 * ABorder;
  FHeight := Integer(AGlyph.metrics.gmBlackBoxY) + 2 * ABorder;
  SetLength(FData, FWidth * FHeight);

  if not AInverted then
    v := DIST_INFINITY
  else
    v := DIST_ZERO;
  for i := 0 to Length(FData) - 1 do
    FData[i] := v;

  if Length(AGlyph.buf) = 0 then Exit;

  rowsize := ((AGlyph.metrics.gmBlackBoxX + 31) div 32) * 4;
  for j := 0 to AGlyph.metrics.gmBlackBoxY - 1 do
  begin
    row := @AGlyph.buf[j * rowsize];
    for i := 0 to AGlyph.metrics.gmBlackBoxX - 1 do
    begin
      if ((TByteArr(row)[i div 8] shr (7 - i mod 8)) and 1 = 1) xor AInverted then
        SetPix(i + ABorder, j + ABorder, DIST_ZERO)
      else
        SetPix(i + ABorder, j + ABorder, DIST_INFINITY);
    end;
  end;

  ReBuild(AInverted);
end;

{ TGlyphImage }

function TGlyphImage.Width: Integer;
begin
  Result := FWidth;
end;

function TGlyphImage.Height: Integer;
begin
  Result := Length(FData);
  if Result > 0 then Result := Result div FWidth;
end;

function TGlyphImage.Data: PByte;
begin
  if Length(FData) > 0 then
    Result := PByte(@FData[0])
  else
    Result := nil;
end;

function TGlyphImage.PixelFormat: TImageFormat;
begin
  Result := TImageFormat.A8B8G8R8;
end;

function TGlyphImage.Replicate: ITextureMip;
begin
  Result := TGlyphImage.Create(FWidth, Copy(FData, 0, Length(FData)));
end;

function TGlyphImage.Pixel(const x, y: Integer): PByte;
begin
  Result := PByte(@FData[y*FWidth+x]);
end;

constructor TGlyphImage.Create(const AWidth: Integer; const AData: TVec4bArr);
begin
  FWidth := AWidth;
  FData := AData;
  Assert(Length(FData) mod FWidth = 0);
end;

end.

