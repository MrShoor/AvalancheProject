unit avGlyphGenerator;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  Classes, SysUtils, avTypes, mutils;

function GenerateGlyphImage(const AFontName  : string;
                            const AChar      : WideChar;
                            const ASize      : Integer;
                            const AItalic    : Boolean;
                            const ABold      : Boolean;
                            const AUnderLine : Boolean;
                            out   ABCMetrics : TVec3I): ITextureMip;

implementation

uses Windows, Graphics{$IfDef DCC}, Types{$EndIf};

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

function GetGlyphMetrics(const Canvas: TCanvas; AChar: WideChar): TVec3I;
var ch: Integer;
begin
  ch := Ord(AChar);
  {$IfDef FPC}
  if not GetCharABCWidthsW(Canvas.Handle, ch, ch, LPABC(@Result)) then
  {$Else}
  if not GetCharABCWidthsW(Canvas.Handle, ch, ch, Result) then
  {$EndIf}
    RaiseLastWin32Error;
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
    bmp.Canvas.Font.Size := ASize;
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

