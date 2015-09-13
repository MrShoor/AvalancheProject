unit avTexLoader;

{$Define VAMPYRE}

interface

uses SysUtils,
     {$IfDef VAMPYRE}
     ImagingTypes,
     ImagingUtility,
     Imaging,
     {$EndIf}
     avTypes,
     mutils;

type
     ETextureFormatError = class (Exception);

const
    SIZE_DEFAULT   = -1;
    SIZE_NEXTPOW2  = -2;
    FORMAT_DEFAULT = TImageFormat.Unknown;

function EmptyTexData(Width, Height: Integer; Format: TTextureFormat; withMips: Boolean = False): ITextureData;

function LoadTexture(const data        : TByteArr;
                     const targetWidth : Integer = SIZE_DEFAULT;
                     const targetHeight: Integer = SIZE_DEFAULT;
                     const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;
function LoadTexture(const FileName    : string;
                     const targetWidth : Integer = SIZE_DEFAULT;
                     const targetHeight: Integer = SIZE_DEFAULT;
                     const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;
function LoadTextures(const Files       : array of string;
                      const targetWidth : Integer = SIZE_DEFAULT;
                      const targetHeight: Integer = SIZE_DEFAULT;
                      const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;

implementation

uses Classes;

const
BestImageFormat : array [TTextureFormat] of TImageFormat = (
    { RGBA    } TImageFormat.A8B8G8R8,
    { RGBA16  } TImageFormat.A16B16G16R16,
    { RGBA16f } TImageFormat.A16B16G16R16F,
    { RGBA32  } TImageFormat.A32B32G32R32,
    { RGBA32f } TImageFormat.A32B32G32R32F,
    { RGB     } TImageFormat.R8G8B8,
    { RGB16   } TImageFormat.R16G16B16,
    { RGB16f  } TImageFormat.R16G16B16F,
    { RGB32   } TImageFormat.R32G32B32,
    { RGB32f  } TImageFormat.R32G32B32F,
    { RG      } TImageFormat.R8G8,
    { RG16    } TImageFormat.R16G16,
    { RG16f   } TImageFormat.R16G16F,
    { RG32    } TImageFormat.R32G32,
    { RG32f   } TImageFormat.R32G32F,
    { R       } TImageFormat.R32,
    { R16     } TImageFormat.R16,
    { R16f    } TImageFormat.R16F,
    { R32F    } TImageFormat.Unknown,
    { R32f    } TImageFormat.R32F,
    { DXT1    } TImageFormat.DXT1,
    { DXT3    } TImageFormat.DXT3,
    { DXT5    } TImageFormat.DXT5,
    { D24_S8  } TImageFormat.D24_S8,
    { D32f_S8 } TImageFormat.D32f_S8,
    { D16     } TImageFormat.D16,
    { D24     } TImageFormat.D24,
    { D32     } TImageFormat.D32,
    { D32f    } TImageFormat.D32f);
BestTextureFormat : array [TImageFormat] of TTextureFormat = (
    { Unknown       } TTextureFormat.RGBA,
    { Gray8         } TTextureFormat.R,
    { R3G3B2        } TTextureFormat.RGB,
    { R8G8          } TTextureFormat.RG,
    { R5G6B5        } TTextureFormat.RGB,
    { A1R5G5B5      } TTextureFormat.RGBA,
    { A4R4G4B4      } TTextureFormat.RGBA,
    { R8G8B8        } TTextureFormat.RGB,
    { A8R8G8B8      } TTextureFormat.RGBA,
    { A8B8G8R8      } TTextureFormat.RGBA,
    { R16F          } TTextureFormat.R16f,
    { R16G16F       } TTextureFormat.RG16f,
    { R16G16B16F    } TTextureFormat.RGB16f,
    { A16R16G16B16F } TTextureFormat.RGBA16f,
    { B16G16R16F    } TTextureFormat.RGB16f,
    { A16B16G16R16F } TTextureFormat.RGBA16f,
    { R32F          } TTextureFormat.R32f,
    { R32G32F       } TTextureFormat.RG32f,
    { R32G32B32F    } TTextureFormat.RGB32f,
    { A32R32G32B32F } TTextureFormat.RGBA32f,
    { A32B32G32R32F } TTextureFormat.RGBA32f,
    { DXT1          } TTextureFormat.DXT1,
    { DXT3          } TTextureFormat.DXT3,
    { DXT5          } TTextureFormat.DXT5,
    { D24_S8        } TTextureFormat.D24_S8,
    { D32f_S8       } TTextureFormat.D32f_S8,
    { D16           } TTextureFormat.D16,
    { D24           } TTextureFormat.D24,
    { D32           } TTextureFormat.D32,
    { D32f          } TTextureFormat.D32f,
    { R16           } TTextureFormat.R16,
    { R16G16        } TTextureFormat.RG16,
    { R16G16B16     } TTextureFormat.RGB16,
    { A16R16G16B16  } TTextureFormat.RGBA16,
    { B16G16R16     } TTextureFormat.RGB16,
    { A16B16G16R16  } TTextureFormat.RGBA16,
    { R32           } TTextureFormat.R32,
    { R32G32        } TTextureFormat.RG32,
    { R32G32B32     } TTextureFormat.RGB32,
    { A32R32G32B32  } TTextureFormat.RGBA32,
    { A32B32G32R32  } TTextureFormat.RGBA32);

type

  { TTextureData }

  TTextureData = class (TInterfacedObject, ITextureData)
  private type

    { TMip }

    TMip = class
    private
        FData  : TByteArr;
        FWidth : Integer;
        FHeight: Integer;
        FLevel : Integer;
    public
        function Replicate: TMip;
        constructor Create(AMipLevel: Integer;
                           AWidth   : Integer;
                           AHeight  : Integer;
                           AData    : PByte;
                           ASize    : Integer);
    end;
  private
    FFormat: TImageFormat;
    FMips: array of array of TMip;
    FWidth: Integer;
    FHeight: Integer;
    FMipsCount: Integer;
    procedure Clear;
  public
    function Width: Integer;
    function Height: Integer;
    function MipsCount: Integer;

    function Format: TImageFormat;
    function ItemCount: Integer;
    function MipCount(const Index: Integer): Integer;
    function Data(const Index, MipLevel: Integer): TTextureMipInfo;

    {$IfDef VAMPYRE}
    constructor Create(ImgData: TDynImageDataArray;
                       targetWidth, targetHeight : Integer;
                       targetFormat              : TImageFormat); overload;
    constructor Create(ImgData: array of TDynImageDataArray;
                       targetWidth, targetHeight : Integer;
                       targetFormat              : TImageFormat); overload;
    {$EndIf}
    constructor CreateEmpty(AWidth, AHeight: Integer; AFormat: TImageFormat; withMips: Boolean = False);
    destructor Destroy; override;
  end;

procedure RaiseUnsupported;
begin
  raise ETextureFormatError.Create('Unsupported format');
end;


function EmptyTexData(Width, Height: Integer; Format: TTextureFormat; withMips: Boolean): ITextureData;
begin
  Result := TTextureData.CreateEmpty(Width, Height, BestImageFormat[Format], withMips);
end;

{$IfDef VAMPYRE}
function VampToAv(srcFormat: ImagingTypes.TImageFormat): TImageFormat;
begin
   case srcFormat of
     ifGray8        : Result := TImageFormat.Gray8;
     ifA8Gray8      : Result := TImageFormat.R8G8;
     ifGray16       : Result := TImageFormat.R16F;
     ifGray32       : Result := TImageFormat.R32F;
     ifA16Gray16    : Result := TImageFormat.R16G16F;
     ifR5G6B5       : Result := TImageFormat.R5G6B5;
     ifR8G8B8       : Result := TImageFormat.R8G8B8;
     ifA8R8G8B8     : Result := TImageFormat.A8R8G8B8;
     ifX8R8G8B8     : Result := TImageFormat.A8R8G8B8;
     ifR32F         : Result := TImageFormat.R32F;
     ifA32R32G32B32F: Result := TImageFormat.A32R32G32B32F;
     ifA32B32G32R32F: Result := TImageFormat.A32B32G32R32F;
     ifR16F         : Result := TImageFormat.R16F;
     ifA16R16G16B16F: Result := TImageFormat.A16R16G16B16F;
     ifA16B16G16R16F: Result := TImageFormat.A16B16G16R16F;
     ifDXT1         : Result := TImageFormat.DXT1;
     ifDXT3         : Result := TImageFormat.DXT3;
     ifDXT5         : Result := TImageFormat.DXT5;
     ifR3G3B2       : Result := TImageFormat.R3G3B2;
     ifA1R5G5B5     : Result := TImageFormat.A1R5G5B5;
     ifA4R4G4B4     : Result := TImageFormat.A4R4G4B4;
   else
     Result := TImageFormat.Unknown;
   end;
end;

function AvToVamp(srcFormat: TImageFormat): ImagingTypes.TImageFormat;
begin
  case srcFormat of
    TImageFormat.Unknown       : Result := ifDefault;
    TImageFormat.Gray8         : Result := ifGray8;
    TImageFormat.R3G3B2        : Result := ifR3G3B2;
    TImageFormat.R5G6B5        : Result := ifR5G6B5;
    TImageFormat.A1R5G5B5      : Result := ifA1R5G5B5;
    TImageFormat.A4R4G4B4      : Result := ifA4R4G4B4;
    TImageFormat.R8G8B8        : Result := ifR8G8B8;
    TImageFormat.A8R8G8B8      : Result := ifA8R8G8B8;
    TImageFormat.R16F           : Result := ifR16F;
    TImageFormat.A16R16G16B16F  : Result := ifA16R16G16B16F;
    TImageFormat.A16B16G16R16F : Result := ifA16B16G16R16F;
    TImageFormat.R32F           : Result := ifR32F;
    TImageFormat.A32R32G32B32F : Result := ifA32R32G32B32F;
    TImageFormat.A32B32G32R32F : Result := ifA32B32G32R32F;
    TImageFormat.DXT1          : Result := ifDXT1;
    TImageFormat.DXT3          : Result := ifDXT3;
    TImageFormat.DXT5          : Result := ifDXT5;
  else
    Result := ifUnknown;
  end;
end;
{$EndIf}

{$IfDef VAMPYRE}
function LoadTexture(const data                      : TByteArr;
                     const targetWidth, targetHeight : Integer;
                     const targetFormat              : TImageFormat): ITextureData;
var imgs: TDynImageDataArray;
begin
  imgs := nil;
  if not LoadMultiImageFromMemory(@data[0], Length(data), imgs) then Exit(nil);
  try
    Result := TTextureData.Create(imgs, targetWidth, targetHeight, targetFormat);
  finally
    FreeImagesInArray(imgs);
  end;
end;
{$EndIf}

{$IfDef VAMPYRE}
function LoadTexture(const FileName: string; const targetWidth: Integer;
  const targetHeight: Integer; const targetFormat: TImageFormat): ITextureData;
var imgs: TDynImageDataArray;
begin
  imgs := nil;
  if not LoadMultiImageFromFile(FileName, imgs) then Exit(nil);
  try
    Result := TTextureData.Create(imgs, targetWidth, targetHeight, targetFormat);
  finally
    FreeImagesInArray(imgs);
  end;
end;

function LoadTextures(const Files: array of string; const targetWidth: Integer;
  const targetHeight: Integer; const targetFormat: TImageFormat): ITextureData;
var imgs: array of TDynImageDataArray;
    i: Integer;
begin
  SetLength(imgs, Length(Files));
  try
    for i := 0 to Length(imgs) - 1 do
      if not LoadMultiImageFromFile(Files[i], imgs[i]) then Exit(nil);
    Result := TTextureData.Create(imgs, targetWidth, targetHeight, targetFormat);
  finally
    for i := 0 to Length(imgs) - 1 do
      FreeImagesInArray(imgs[i]);
  end;
end;

{$EndIf}

{ TTextureData.TMip }

function TTextureData.TMip.Replicate: TMip;
begin
  Result := TMip.Create(FLevel, FWidth, FHeight, nil, 0);
  Result.FData := FData;
end;

constructor TTextureData.TMip.Create(AMipLevel: Integer; AWidth: Integer;
  AHeight: Integer; AData: PByte; ASize: Integer);
begin
  FLevel := AMipLevel;
  FWidth := AWidth;
  FHeight := AHeight;
  SetLength(FData, ASize);
  if ASize > 0 then Move(AData^, FData[0], ASize);
end;

{ TTextureData }

procedure TTextureData.Clear;
var i, j: Integer;
begin
  for i := 0 to Length(FMips) - 1 do
    for j := 0 to Length(FMips[i]) - 1 do
      FreeAndNil(FMips[i][j]);
  FMips := nil;
end;

function TTextureData.Width: Integer;
begin
  Result := FWidth;
end;

function TTextureData.Height: Integer;
begin
  Result := FHeight;
end;

function TTextureData.MipsCount: Integer;
begin
  Result := FMipsCount;
end;

function TTextureData.Format: TImageFormat;
begin
  Result := FFormat;
end;

function TTextureData.ItemCount: Integer;
begin
  Result := Length(FMips);
end;

function TTextureData.MipCount(const Index: Integer): Integer;
begin
  Result := Length(FMips[Index]);
end;

function TTextureData.Data(const Index, MipLevel: Integer): TTextureMipInfo;
begin
  Result.Data   := PByte(FMips[Index][MipLevel].FData);
  Result.Height := FMips[Index][MipLevel].FHeight;
  Result.Width  := FMips[Index][MipLevel].FWidth;
  Result.Level  := FMips[Index][MipLevel].FLevel;
end;

{$IfDef VAMPYRE}
constructor TTextureData.Create(ImgData: TDynImageDataArray;
                                targetWidth, targetHeight : Integer;
                                targetFormat              : TImageFormat);
  type
      TTextureType = (ttSingleImage, ttMipLeveling, ttTextureArray);

var NewVamp: ImagingTypes.TImageFormat;
    Img0: TImageData;
    i: Integer;
    TexType: TTextureType;
begin
  Assert(Length(ImgData)>0);
  Img0 := ImgData[0];

  if targetFormat = TImageFormat.Unknown then
  begin
    targetFormat := VampToAv(Img0.Format);
    if targetFormat = TImageFormat.Unknown then RaiseUnsupported;
  end;
  NewVamp := AvToVamp(targetFormat);
  if NewVamp = ifUnknown then RaiseUnsupported;
  FFormat := targetFormat;

  TexType := ttSingleImage;
  if Length(ImgData) > 1 then
  begin
    if (ImgData[0].Width = ImgData[1].Width) and (ImgData[0].Height = ImgData[1].Height) then
      TexType := ttTextureArray
    else
    begin
      if IsPow2(ImgData[0].Width) and IsPow2(ImgData[0].Height) and
         (ImgData[0].Width shr 2 = ImgData[1].Width) and (ImgData[0].Height shr 2 = ImgData[1].Height) then
         TexType := ttMipLeveling;
    end;
  end;

  if targetWidth = SIZE_DEFAULT then targetWidth := ImgData[0].Width;
  if targetHeight = SIZE_DEFAULT then targetHeight := ImgData[0].Height;
  if targetWidth = SIZE_NEXTPOW2 then targetWidth := NextPow2(ImgData[0].Width);
  if targetHeight = SIZE_NEXTPOW2 then targetHeight := NextPow2(ImgData[0].Height);

  FWidth := targetWidth;
  FHeight := targetHeight;

  if (TexType = ttMipLeveling) then
    if not (IsPow2(targetWidth) and IsPow2(targetHeight)) then
      TexType := ttSingleImage;

  case TexType of
    ttSingleImage:
        SetLength(FMips, 1, 1);
    ttMipLeveling:
        SetLength(FMips, 1, Length(ImgData));
    ttTextureArray:
        SetLength(FMips, Length(ImgData), 1);
  end;
  FMipsCount := Length(FMips[0]);

  for i := 0 to Length(ImgData) - 1 do
  begin
    if NewVamp <> ImgData[i].Format then ConvertImage(ImgData[i], NewVamp);

    if (targetWidth <> ImgData[i].Width) or (targetHeight <> ImgData[i].Height) then
      ResizeImage(ImgData[i], targetWidth, targetHeight, rfBicubic);

    case TexType of
      ttSingleImage:
          begin
            FMips[0][0] := TMip.Create(0, ImgData[i].Width, ImgData[i].Height, ImgData[i].Bits, ImgData[i].Size);
            Break;
          end;
      ttMipLeveling:
          begin
            FMips[0][i] := TMip.Create(i, ImgData[i].Width, ImgData[i].Height, ImgData[i].Bits, ImgData[i].Size);
            targetWidth := targetWidth shr 2;
            targetHeight := targetHeight shr 2;
          end;
      ttTextureArray:
          begin
            FMips[i][0] := TMip.Create(0, ImgData[i].Width, ImgData[i].Height, ImgData[i].Bits, ImgData[i].Size);
          end;
    end;
  end;
end;

constructor TTextureData.Create(ImgData: array of TDynImageDataArray;
  targetWidth, targetHeight: Integer; targetFormat: TImageFormat);
var NewVamp: ImagingTypes.TImageFormat;
    PImg: PImageData;
    j, i: Integer;
    w,h: Integer;
begin
  Assert(Length(ImgData)>0);
  Assert(Length(ImgData[0])>0);

  if targetFormat = TImageFormat.Unknown then
  begin
    targetFormat := VampToAv(ImgData[0][0].Format);
    if targetFormat = TImageFormat.Unknown then RaiseUnsupported;
  end;
  NewVamp := AvToVamp(targetFormat);
  if NewVamp = ifUnknown then RaiseUnsupported;
  FFormat := targetFormat;

  if targetWidth = SIZE_DEFAULT then targetWidth := ImgData[0][0].Width;
  if targetHeight = SIZE_DEFAULT then targetHeight := ImgData[0][0].Height;
  if targetWidth = SIZE_NEXTPOW2 then targetWidth := NextPow2(ImgData[0][0].Width);
  if targetHeight = SIZE_NEXTPOW2 then targetHeight := NextPow2(ImgData[0][0].Height);
  FWidth := targetWidth;
  FHeight := targetHeight;

  FMipsCount := 1;
  for j := 0 to Length(ImgData) - 1 do
    if Length(ImgData[j])>1 then
    begin
      FMipsCount := GetMipsCount(FWidth, FHeight);
      Break;
    end;

  SetLength(FMips, Length(ImgData), FMipsCount);
  for j := 0 to Length(ImgData) - 1 do
  begin
    Assert(Length(ImgData[j]) > 0, 'No data for texture #'+IntToStr(j));
    w := targetWidth;
    h := targetHeight;
    for i := 0 to FMipsCount - 1 do
    begin
      if i < Length(ImgData[j]) then
        PImg := @ImgData[j][i];

      if NewVamp <> PImg^.Format then ConvertImage(PImg^, NewVamp);
      if (w <> PImg^.Width) or (h <> PImg^.Height) then
        ResizeImage(PImg^, w, h, rfBicubic);
      FMips[j][i] := TMip.Create(i, PImg^.Width, PImg^.Height, PImg^.Bits, PImg^.Size);
      w := w shr 2;
      h := h shr 2;
    end;
  end;
end;

constructor TTextureData.CreateEmpty(AWidth, AHeight: Integer; AFormat: TImageFormat; withMips: Boolean);
var i: Integer;
begin
  FWidth := AWidth;
  FHeight := AHeight;

  FFormat := AFormat;
  withMips := withMips and IsPow2(Vec(AWidth, AHeight));
  if withMips then
    FMipsCount := Log2Int(Min(AWidth, AHeight))
  else
    FMipsCount := 1;
  SetLength(FMips, 1, FMipsCount);
  for i := 0 to FMipsCount - 1 do
  begin
    FMips[0][i] := TMip.Create(i, AWidth, AHeight, Nil, 0);
    AWidth := AWidth shr 1;
    AHeight := AHeight shr 1;
  end;
end;

{$EndIf}

destructor TTextureData.Destroy;
begin
  inherited Destroy;
  Clear;
end;

end.


