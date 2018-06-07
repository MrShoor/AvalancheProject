unit avGlyphGenerator;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  Classes, SysUtils, avTypes, mutils;

const
  GLYPH_DFOverscale = 4;

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

    bmp.Canvas.TextOut(0, 0, AChar);

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
    glyph.SaveToFile('D:\debug.bmp');

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
    XXX.z := glyph.metrics.gmCellIncX - glyph.metrics.gmBlackBoxX - glyph.metrics.gmptGlyphOrigin.X;

    XXX.x := XXX.x - scaledBorder;
    XXX.y := XXX.y + 2 * scaledBorder;
    XXX.z := XXX.z - scaledBorder;

    XXX  := XXX  * (1.0 / (1 shl GLYPH_DFOverscale));
    YYYY := YYYY * (1.0 / (1 shl GLYPH_DFOverscale));
  finally
    FreeAndNil(bmp);
  end;
end;

{ TGlyphMonochrome }

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
        row := @buf[j * rowsize];
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

constructor TVec2Field.Create(const AGlyph: TGlyphMonochrome; const ABorder: Integer; const AInverted: Boolean);
var i, j: Integer;
    rowsize: integer;
    row: PByte;
    v: TVec2;
begin
  FWidth := AGlyph.metrics.gmBlackBoxX + 2 * ABorder;
  FHeight := AGlyph.metrics.gmBlackBoxY + 2 * ABorder;
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

