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

function LoadTexture(const data        : TByteArr;
                     const targetWidth : Integer = SIZE_DEFAULT;
                     const targetHeight: Integer = SIZE_DEFAULT;
                     const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;
function LoadTexture(const FileName    : string;
                     const targetWidth : Integer = SIZE_DEFAULT;
                     const targetHeight: Integer = SIZE_DEFAULT;
                     const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;

implementation

uses Classes;

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
    procedure Clear;
  public
    function Format: TImageFormat;
    function ItemCount: Integer;
    function MipCount(const Index: Integer): Integer;
    function Data(const Index, MipLevel: Integer): TTextureMipInfo;

    {$IfDef VAMPYRE}
    constructor Create(ImgData: TDynImageDataArray;
                       targetWidth, targetHeight : Integer;
                       targetFormat              : TImageFormat);
    {$EndIf}
    destructor Destroy; override;
  end;

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
  Result.Data   := @FMips[Index][MipLevel].FData[0];
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

  procedure RaiseUnsupported;
  begin
    raise ETextureFormatError.Create('Unsupported format');
  end;

  function VampToAv(srcFormat: ImagingTypes.TImageFormat): TImageFormat;
  begin
     case srcFormat of
       ifGray8        : Result := TImageFormat.Gray8;
       ifA8Gray8      : Result := TImageFormat.R8G8;
       ifGray16       : Result := TImageFormat.R16;
       ifGray32       : Result := TImageFormat.R32;
       ifA16Gray16    : Result := TImageFormat.R16G16;
       ifR5G6B5       : Result := TImageFormat.R5G6B5;
       ifR8G8B8       : Result := TImageFormat.R8G8B8;
       ifA8R8G8B8     : Result := TImageFormat.B8G8R8A8;
       ifX8R8G8B8     : Result := TImageFormat.B8G8R8A8;
       ifR32F         : Result := TImageFormat.R32;
       ifA32R32G32B32F: Result := TImageFormat.A32R32G32B32F;
       ifA32B32G32R32F: Result := TImageFormat.A32B32G32R32F;
       ifR16F         : Result := TImageFormat.R16;
       ifA16R16G16B16F: Result := TImageFormat.A16R16G16B16;
       ifA16B16G16R16F: Result := TImageFormat.A16B16G16R16;
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
      TImageFormat.Unknown      : Result := ifDefault;
      TImageFormat.Gray8        : Result := ifGray8;
      TImageFormat.R3G3B2       : Result := ifR3G3B2;
      TImageFormat.R5G6B5       : Result := ifR5G6B5;
      TImageFormat.A1R5G5B5     : Result := ifA1R5G5B5;
      TImageFormat.A4R4G4B4     : Result := ifA4R4G4B4;
      TImageFormat.R8G8B8       : Result := ifR8G8B8;
      TImageFormat.B8G8R8A8     : Result := ifA8R8G8B8;
      TImageFormat.R16          : Result := ifR16F;
      TImageFormat.A16R16G16B16 : Result := ifA16R16G16B16F;
      TImageFormat.A16B16G16R16 : Result := ifA16B16G16R16F;
      TImageFormat.R32          : Result := ifR32F;
      TImageFormat.A32R32G32B32F: Result := ifA32R32G32B32F;
      TImageFormat.A32B32G32R32F: Result := ifA32B32G32R32F;
      TImageFormat.DXT1         : Result := ifDXT1;
      TImageFormat.DXT3         : Result := ifDXT3;
      TImageFormat.DXT5         : Result := ifDXT5;
    else
      Result := ifUnknown;
    end;
  end;

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
{$EndIf}

destructor TTextureData.Destroy;
begin
  inherited Destroy;
  Clear;
end;

end.


