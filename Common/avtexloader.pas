unit avTexLoader;

{$DEFINE VAMPYRE}

interface

uses SysUtils,
     {$IFDEF VAMPYRE}
     ImagingTypes,
     ImagingUtility,
     Imaging,
     {$ENDIF}
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
                     const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData;

implementation

uses Classes, Math;

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

    constructor Create(var ImgData: TImageData;
                       targetWidth, targetHeight : Integer;
                       targetFormat              : TImageFormat);
    destructor Destroy; override;
  end;

function LoadTexture(const data                      : TByteArr;
                     const targetWidth, targetHeight : Integer;
                     const targetFormat              : TImageFormat): ITextureData;
var img: TImageData;
begin
    if not LoadImageFromMemory(@data[0], Length(data), img) then Exit(nil);
    Result := TTextureData.Create(img, targetWidth, targetHeight, targetFormat);
end;

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

constructor TTextureData.Create(var ImgData: TImageData;
                                targetWidth, targetHeight : Integer;
                                targetFormat              : TImageFormat);
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
begin
  if targetFormat = TImageFormat.Unknown then
  begin
    targetFormat := VampToAv(ImgData.Format);
    if targetFormat = TImageFormat.Unknown then RaiseUnsupported;
  end;
  NewVamp := AvToVamp(targetFormat);
  if NewVamp = ifUnknown then RaiseUnsupported;
  if NewVamp <> ImgData.Format then ConvertImage(ImgData, NewVamp);

  if targetWidth = SIZE_DEFAULT then targetWidth := ImgData.Width;
  if targetHeight = SIZE_DEFAULT then targetHeight := ImgData.Height;
  if targetWidth = SIZE_NEXTPOW2 then targetWidth := NextPow2(ImgData.Width);
  if targetHeight = SIZE_NEXTPOW2 then targetHeight := NextPow2(ImgData.Height);

  if (targetWidth <> ImgData.Width) or (targetHeight <> ImgData.Height) then
    ResizeImage(ImgData, targetWidth, targetHeight, rfBicubic);

  SetLength(FMips, 1);
  SetLength(FMips[0], 1);
  FMips[0][0] := TMip.Create(0, ImgData.Width, ImgData.Height, ImgData.Bits, ImgData.Size);
end;

destructor TTextureData.Destroy;
begin
  inherited Destroy;
  Clear;
end;

end.


