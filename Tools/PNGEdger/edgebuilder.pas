unit EdgeBuilder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Imaging, ImagingTypes, ImagingFormats, ImagingUtility;

type
  ELoadError = class (Exception);
  EConvertError = class (Exception);

procedure CreatePngEdges(const SrcImage, DstImage: string; Tolerance: Integer);

implementation

uses
  Math, mutils;

type
  { TDistField }

  TDistField = class (TObject)
  private
    FData  : TVec3Arr;
    FWidth : Integer;
    FHeight: Integer;

    procedure WrapCoords(var x, y: Integer);

    procedure SetPixel(x, y: Integer; const AValue: TVec3);
    function GetPixel(x, y: Integer): TVec3;
  public
    procedure GetNearestPixel(x,y: Integer; out nx, ny: Integer);
    constructor Create(const Img: TImageData; Tolerance: Single);
  end;

procedure CreatePngEdges(const SrcImage, DstImage: string; Tolerance: Integer);
var src, dst: TImageData;
    df: TDistField;
    i, j, nearX, nearY: Integer;
    Color: TColorFPRec;
begin
  InitImage(src);
  InitImage(dst);
  try
    if not LoadImageFromFile(SrcImage, src) then
      raise ELoadError.Create('Can''t load image "'+SrcImage+'"');

    if (src.Format <> ifA8R8G8B8) and (src.Format <> ifA16R16G16B16) then
      raise EConvertError.Create('Source image "'+SrcImage+'" should be RGBA8 or RGBA16 format');

    if not NewImage(src.Width, src.Height, src.Format, dst) then
      raise EConvertError.Create('Can''t create dst image');

    df := TDistField.Create(src, Clamp(Tolerance, 0, 100)/100);
    for j := 0 to src.Height - 1 do
      for i := 0 to src.Width - 1 do
      begin
        df.GetNearestPixel(i, j, nearX, nearY);
        Color := GetPixelFP(src, nearX, nearY);
        Color.A := GetPixelFP(src, i, j).A;
        SetPixelFP(dst, i, j, Color);
      end;

    if not SaveImageToFile(DstImage, dst) then
      raise EConvertError.Create('Can''t save dst image at "'+DstImage+'"');
  finally
    FreeImage(src);
    FreeImage(dst);
  end;
end;

{ TDistField }

procedure TDistField.WrapCoords(var x, y: Integer);
begin
  x := x mod FWidth;
  if x < 0 then Inc(x, FWidth);
  y := y mod FHeight;
  if y < 0 then Inc(y, FHeight);
end;

procedure TDistField.SetPixel(x, y: Integer; const AValue: TVec3);
begin
  WrapCoords(x, y);
  FData[y*FWidth+x] := AValue;
end;

function TDistField.GetPixel(x, y: Integer): TVec3;
begin
  WrapCoords(x, y);
  Result := FData[y*FWidth+x];
end;

procedure TDistField.GetNearestPixel(x, y: Integer; out nx, ny: Integer);
var v: TVec2;
begin
  v := Vec(x,y) + GetPixel(x,y).xy;
  nx := Round(v.x);
  ny := Round(v.y);
end;

constructor TDistField.Create(const Img: TImageData; Tolerance: Single);

  procedure UpdateMinDistance(x, y: Integer; offsetX, offsetY: Integer; var CurrentDist: Single);
  var v: TVec3;
  begin
    v.xy := GetPixel(x+offsetX, y+offsetY).xy + Vec(offsetX, offsetY);
    v.z := Len(v.xy);
    if v.z < CurrentDist then
    begin
      CurrentDist := v.z;
      SetPixel(x, y, v);
    end;
  end;

var
  i, j: Integer;
  MinSrc: Single;
  AA: Single;
begin
  FWidth := Img.Width;
  FHeight := Img.Height;
  SetLength(FData, FWidth * FHeight);
  if FHeight*FWidth = 0 then Exit;

  for j := 0 to Img.Height - 1 do
    for i := 0 to Img.Width - 1 do
      begin
        AA := GetPixelFP(Img, i, j).A;
        if AA < Tolerance then
          SetPixel(i,j, Vec(Infinity,Infinity,Infinity))
        else
          SetPixel(i,j, Vec(0,0,0));
      end;

  for j := 0 to FHeight - 1 do
    for i := 0 to FWidth - 1 do
    begin
      MinSrc := GetPixel(i, j).z;
      UpdateMinDistance(i, j, -1, -1, MinSrc);
      UpdateMinDistance(i, j,  0, -1, MinSrc);
      UpdateMinDistance(i, j,  1, -1, MinSrc);
      UpdateMinDistance(i, j, -1,  0, MinSrc);
    end;

  for j := FHeight - 1 downto 0 do
    for i := FWidth - 1 downto 0 do
    begin
      MinSrc := GetPixel(i, j).z;
      UpdateMinDistance(i, j,  1,  0, MinSrc);
      UpdateMinDistance(i, j, -1,  1, MinSrc);
      UpdateMinDistance(i, j,  0,  1, MinSrc);
      UpdateMinDistance(i, j,  1,  1, MinSrc);
    end;
end;

end.

