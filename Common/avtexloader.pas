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
     avContnrs,
     mutils;

const
  SIZE_DEFAULT   = -1;
  SIZE_NEXTPOW2  = -2;
  FORMAT_DEFAULT = TImageFormat.Unknown;

type
  ETextureFormatError = class (Exception);

  { ITextureManager }

  ITextureManager = interface
    procedure DropCache;
    function LoadTexture(const FileData    : TByteArr;
                         const targetWidth : Integer = SIZE_DEFAULT;
                         const targetHeight: Integer = SIZE_DEFAULT;
                         const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;
    function LoadTexture(const FileName    : string;
                         const targetWidth : Integer = SIZE_DEFAULT;
                         const targetHeight: Integer = SIZE_DEFAULT;
                         const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;
    function LoadTexture(const Files       : array of string;
                         const targetWidth : Integer = SIZE_DEFAULT;
                         const targetHeight: Integer = SIZE_DEFAULT;
                         const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;
  end;

function EmptyTexData(Width, Height: Integer; Format: TTextureFormat; withMips: Boolean = False; AllocateTextureMemory: Boolean = False): ITextureData; overload;
function EmptyTexData(Width, Height: Integer; Format: TImageFormat; withMips: Boolean = False; AllocateTextureMemory: Boolean = False): ITextureData; overload;
function EmptyTexData: ITextureData; overload;

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

function Create_ITextureManager: ITextureManager;

implementation

uses Classes;

const
BestImageFormat : array [TTextureFormat] of TImageFormat = (
    { RGBA    } TImageFormat.A8B8G8R8,
    { RGBA16  } TImageFormat.A16B16G16R16,
    { RGBA16f } TImageFormat.A16B16G16R16F,
    { RGBA32  } TImageFormat.A32B32G32R32,
    { RGBA32f } TImageFormat.R32G32B32A32F,
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
    { R32G32B32A32F } TTextureFormat.RGBA32f,
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

  { TTextureManager }

  TTextureManager = class (TInterfacedObject, ITextureManager)
  private type
    TTexHashFunc = specialize TMurmur2Hash<TByteArr>;
    ITexHash = specialize IHashMap<TByteArr, ITextureData, TTexHashFunc>;
    TTexHash = specialize THashMap<TByteArr, ITextureData, TTexHashFunc>;
  private
    FTexHash: ITexHash;

    function StreamToKey(const AStream: TStream): TByteArr;
  public
    procedure DropCache;
    function LoadTexture(const FileData    : TByteArr;
                         const targetWidth : Integer = SIZE_DEFAULT;
                         const targetHeight: Integer = SIZE_DEFAULT;
                         const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;
    function LoadTexture(const FileName    : string;
                         const targetWidth : Integer = SIZE_DEFAULT;
                         const targetHeight: Integer = SIZE_DEFAULT;
                         const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;
    function LoadTexture(const Files       : array of string;
                         const targetWidth : Integer = SIZE_DEFAULT;
                         const targetHeight: Integer = SIZE_DEFAULT;
                         const targetFormat: TImageFormat = FORMAT_DEFAULT): ITextureData; overload;
    procedure AfterConstruction; override;
  end;

  { TMipImage }

  TMipImage = class (TInterfacedObjectEx, ITextureMip)
  private
    FData       : TByteArr;
    FWidth      : Integer;
    FHeight     : Integer;
    FPixelFormat: TImageFormat;
  public
    function Width      : Integer;
    function Height     : Integer;
    function Data       : PByte;
    function PixelFormat: TImageFormat;
    function Replicate  : ITextureMip;
    function Pixel(const x,y: Integer): PByte;
    constructor Create(const AWidth, AHeight: Integer; const AData: PByte; const ADataSize: Integer; const APixelFormat: TImageFormat);
  end;

  { TTextureData }

  TTextureData = class (TInterfacedObject, ITextureData)
  private
    FFormat: TImageFormat;
    FMips: array of array of ITextureMip;
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
    function MipData(const Index, MipLevel: Integer): ITextureMip;

    procedure SetMip(Index, MipLevel: Integer; const AImage: ITextureMip; AutoResize: Boolean; const FillColor: TVec4);
    procedure Drop(Index, MipLevel: Integer); overload;
    procedure Drop(Index: Integer); overload;

    procedure Merge(const TexData: array of ITextureData); overload;
    procedure Merge(const TexData: array of ITextureData; AutoResize: Boolean; const FillColor: TVec4); overload;

    {$IfDef VAMPYRE}
    constructor Create(ImgData: TDynImageDataArray;
                       targetWidth, targetHeight : Integer;
                       targetFormat              : TImageFormat); overload;
    constructor Create(ImgData: array of TDynImageDataArray;
                       targetWidth, targetHeight : Integer;
                       targetFormat              : TImageFormat); overload;
    {$EndIf}
    constructor CreateEmpty(AWidth, AHeight: Integer; AFormat: TImageFormat; withMips: Boolean = False; AllocateTextureMemory: Boolean = False);
    destructor Destroy; override;
  end;

procedure RaiseUnsupported;
begin
  raise ETextureFormatError.Create('Unsupported format');
end;


function EmptyTexData(Width, Height: Integer; Format: TTextureFormat; withMips: Boolean; AllocateTextureMemory: Boolean): ITextureData;
begin
  Result := TTextureData.CreateEmpty(Width, Height, BestImageFormat[Format], withMips, AllocateTextureMemory);
end;

function EmptyTexData(Width, Height: Integer; Format: TImageFormat; withMips: Boolean; AllocateTextureMemory: Boolean): ITextureData;
begin
  Result := TTextureData.CreateEmpty(Width, Height, Format, withMips, AllocateTextureMemory);
end;

function EmptyTexData: ITextureData;
begin
  Result := TTextureData.Create;
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
     ifA32B32G32R32F: Result := TImageFormat.R32G32B32A32F;
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
    TImageFormat.R16F          : Result := ifR16F;
    TImageFormat.A16R16G16B16F : Result := ifA16R16G16B16F;
    TImageFormat.A16B16G16R16F : Result := ifA16B16G16R16F;
    TImageFormat.R32F          : Result := ifR32F;
    TImageFormat.A32R32G32B32F : Result := ifA32R32G32B32F;
    TImageFormat.R32G32B32A32F : Result := ifA32B32G32R32F;
    TImageFormat.DXT1          : Result := ifDXT1;
    TImageFormat.DXT3          : Result := ifDXT3;
    TImageFormat.DXT5          : Result := ifDXT5;
  else
    Result := ifUnknown;
  end;
end;
{$EndIf}

{$IfDef VAMPYRE}
function ConvertMip(const srcMip: ITextureMip; const w, h: Integer; const format: TImageFormat; AutoStretch: Boolean; Const FillColor: TVec4): ITextureMip;
  function MipToImg(const AMip: ITextureMip): TImageData;
  begin
    Result.Format := AvToVamp(AMip.PixelFormat);
    Result.Height := AMip.Height;
    Result.Width := AMip.Width;
    Result.Palette := nil;
    Result.Size := AMip.Width*AMip.Height*ImagePixelSize[AMip.PixelFormat];
    Result.Bits := AMip.Data;
  end;
var img, img2: TImageData;
    col: TColorFPRec;
    pcol: Pointer;
begin
  if (srcMip.Width = w) and (srcMip.Height=h) and (srcMip.PixelFormat = format) then
    Exit(srcMip);

  ZeroClear(img, SizeOf(img));
  ZeroClear(img2, SizeOf(img2));
  try
    if AutoStretch then
    begin
      img2 := MipToImg(srcMip);
      CloneImage(img2, img);
      ResizeImage(img, w, h, rfBicubic);
      ConvertImage(img, AvToVamp(format));
    end
    else
    begin
      img2 := MipToImg(srcMip);

      NewImage(w, h, AvToVamp(format), img);
      col.R := FillColor.x;
      col.G := FillColor.y;
      col.B := FillColor.z;
      col.A := FillColor.w;
      SetPixelFP(img, 0, 0, col);
      GetPixelDirect(img, 0, 0, pcol);
      FillRect(img, 0, 0, img.Width, img.Height, pcol);

      CopyRect(img2, 0, 0, srcMip.Width, srcMip.Height, img, 0, 0);
    end;
    Result := TMipImage.Create(img.Width, img.Height, img.Bits, img.Size, format);
  finally
    FreeImage(img);
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

function Create_ITextureManager: ITextureManager;
begin
  Result := TTextureManager.Create;
end;

{ TMipImage }

function TMipImage.Width: Integer;
begin
  Result := FWidth;
end;

function TMipImage.Height: Integer;
begin
  Result := FHeight;
end;

function TMipImage.Data: PByte;
begin
  Result := PByte(FData);
end;

function TMipImage.PixelFormat: TImageFormat;
begin
  Result := FPixelFormat;
end;

function TMipImage.Replicate: ITextureMip;
begin
  Result := TMipImage.Create(FWidth, FHeight, PByte(FData), Length(FData), FPixelFormat);
end;

function TMipImage.Pixel(const x, y: Integer): PByte;
begin
  Result := PByte(FData);
  Inc(Result, (y*Width+x)*ImagePixelSize[PixelFormat]);
end;

constructor TMipImage.Create(const AWidth, AHeight: Integer; const AData: PByte; const ADataSize: Integer; const APixelFormat: TImageFormat);
begin
  Assert((ADataSize=AWidth*AHeight*ImagePixelSize[APixelFormat]) or (ADataSize=0));
  FWidth := AWidth;
  FHeight := AHeight;
  FPixelFormat := APixelFormat;
  SetLength(FData, ADataSize);
  if Assigned(AData) and (ADataSize > 0) then Move(AData^, FData[0], ADataSize);
end;

{ TTextureManager }

function TTextureManager.StreamToKey(const AStream: TStream): TByteArr;
begin
  SetLength(Result, AStream.Size);
  AStream.Position := 0;
  AStream.ReadBuffer(Result[0], Length(Result));
end;

procedure TTextureManager.DropCache;
begin
  FTexHash.Clear;
end;

function TTextureManager.LoadTexture(const FileData: TByteArr;
  const targetWidth: Integer; const targetHeight: Integer;
  const targetFormat: TImageFormat): ITextureData;
const
  MethodID: Integer = 0;
var ms: TMemoryStream;
    key: TByteArr;
begin
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(MethodID, SizeOf(MethodID));
    ms.WriteBuffer(Pointer(FileData), SizeOf(Pointer));
    ms.WriteBuffer(targetWidth, SizeOf(targetWidth));
    ms.WriteBuffer(targetHeight, SizeOf(targetHeight));
    ms.WriteBuffer(targetFormat, SizeOf(targetFormat));
    key := StreamToKey(ms);
    if not FTexHash.TryGetValue(key, Result) then
    begin
      Result := avTexLoader.LoadTexture(FileData, targetWidth, targetHeight, targetFormat);
      FTexHash.Add(key, Result);
    end;
  finally
    FreeAndNil(ms);
  end;
end;

function TTextureManager.LoadTexture(const FileName: string;
  const targetWidth: Integer; const targetHeight: Integer;
  const targetFormat: TImageFormat): ITextureData;
const
  MethodID: Integer = 1;
var ms: TMemoryStream;
    key: TByteArr;
    fullName: string;
begin
  ms := TMemoryStream.Create;
  try
    fullName := ExpandFileName(FileName);

    ms.WriteBuffer(MethodID, SizeOf(MethodID));
    StreamWriteString(ms, fullName);
    ms.WriteBuffer(targetWidth, SizeOf(targetWidth));
    ms.WriteBuffer(targetHeight, SizeOf(targetHeight));
    ms.WriteBuffer(targetFormat, SizeOf(targetFormat));
    key := StreamToKey(ms);
    if not FTexHash.TryGetValue(key, Result) then
    begin
      Result := avTexLoader.LoadTexture(fullName, targetWidth, targetHeight, targetFormat);
      FTexHash.Add(key, Result);
    end;
  finally
    FreeAndNil(ms);
  end;
end;

function TTextureManager.LoadTexture(const Files: array of string;
  const targetWidth: Integer; const targetHeight: Integer;
  const targetFormat: TImageFormat): ITextureData;
const
  MethodID: Integer = 2;
var ms: TMemoryStream;
    key: TByteArr;
    n: Integer;
    i: Integer;
begin
  ms := TMemoryStream.Create;
  try
    ms.WriteBuffer(MethodID, SizeOf(MethodID));
    n := Length(Files);
    ms.WriteBuffer(n, SizeOf(n));
    for i := 0 to n - 1 do
      StreamWriteString(ms, Files[i]);
    ms.WriteBuffer(targetWidth, SizeOf(targetWidth));
    ms.WriteBuffer(targetHeight, SizeOf(targetHeight));
    ms.WriteBuffer(targetFormat, SizeOf(targetFormat));
    key := StreamToKey(ms);
    if not FTexHash.TryGetValue(key, Result) then
    begin
      Result := avTexLoader.LoadTextures(Files, targetWidth, targetHeight, targetFormat);
      FTexHash.Add(key, Result);
    end;
  finally
    FreeAndNil(ms);
  end;
end;

procedure TTextureManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FTexHash := TTexHash.Create(nil, nil);
end;

{$EndIf}

{ TTextureData }

procedure TTextureData.Clear;
begin
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

function TTextureData.MipData(const Index, MipLevel: Integer): ITextureMip;
begin
  Result := FMips[Index][MipLevel];
end;

procedure TTextureData.SetMip(Index, MipLevel: Integer; const AImage: ITextureMip; AutoResize: Boolean; const FillColor: TVec4);
var w, h: Integer;
begin
  if (MipLevel < 0) or (MipLevel >= FMipsCount) then Exit;

  if (Index < 0) or (Index > Length(FMips)) then
    Index := Length(FMips);

  if Index = Length(FMips) then
  begin
    SetLength(FMips, Length(FMips) + 1);
    SetLength(FMips[Index], FMipsCount);
  end;

  w := Width shr MipLevel;
  h := Height shr MipLevel;
  FMips[Index][MipLevel] := ConvertMip(AImage, w, h, Format, AutoResize, FillColor);
end;

procedure TTextureData.Drop(Index, MipLevel: Integer);
var I: Integer;
begin
  if index < 0 then Exit;
  if MipLevel < 0 then Exit;
  if index >= Length(FMips) then Exit;
  if MipLevel >= Length(FMips[Index]) then Exit;
  FMips[Index][MipLevel] := nil;
  for I := 0 to Length(FMips[Index]) - 1 do
    if Assigned(FMips[Index]) then Exit;
  Drop(Index);
end;

procedure TTextureData.Drop(Index: Integer);
var i: Integer;
begin
  if index < 0 then Exit;
  if index >= Length(FMips) then Exit;

  for i := Index to Length(FMips) - 2 do
    FMips[i] := FMips[i+1];
  SetLength(FMips, Length(FMips)-1);
end;

procedure TTextureData.Merge(const TexData: array of ITextureData);
begin
  Merge(TexData, True, Vec(0,0,0,0));
end;

procedure TTextureData.Merge(const TexData: array of ITextureData; AutoResize: Boolean; const FillColor: TVec4);
var i, j, k: Integer;
    sliceStart, sliceOffset: Integer;
    w, h: Integer;
begin
  if Length(TexData) = 0 then Exit;

  if Length(FMips) = 0 then
  begin
    for i := Low(TexData) to High(TexData) do
      if TexData[i].ItemCount > 0 then
      begin
        FWidth := TexData[i].Width;
        FHeight := TexData[i].Height;
        FFormat := TexData[i].Format;
        FMipsCount := TexData[i].MipsCount;
        Break;
      end;
    if FMipsCount = 0 then Exit;
  end;

  sliceOffset := 0;
  for i := Low(TexData) to High(TexData) do
    Inc(sliceOffset, TexData[i].ItemCount);

  sliceStart := Length(FMips);
  SetLength(FMips, sliceStart+sliceOffset, FMipsCount);

  sliceOffset := sliceStart;
  for i := Low(TexData) to High(TexData) do
    for j := 0 to TexData[i].ItemCount - 1 do
    begin
      w := FWidth;
      h := FHeight;
      for k := 0 to FMipsCount - 1 do
      begin
        if k < TexData[i].MipsCount then
          FMips[sliceOffset][k] := ConvertMip(TexData[i].MipData(j, k), w, h, FFormat, AutoResize, FillColor)
        else
          FMips[sliceOffset][k] := ConvertMip(FMips[sliceOffset][k-1], w, h, FFormat, AutoResize, FillColor);
        w := w div 2;
        h := h div 2;
      end;
      Inc(sliceOffset);
    end;
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
            FMips[0][0] := TMipImage.Create(targetWidth, targetHeight, ImgData[i].Bits, ImgData[i].Size, FFormat);
            Break;
          end;
      ttMipLeveling:
          begin
            FMips[0][i] := TMipImage.Create(targetWidth, targetHeight, ImgData[i].Bits, ImgData[i].Size, FFormat);
            targetWidth := targetWidth shr 2;
            targetHeight := targetHeight shr 2;
          end;
      ttTextureArray:
          begin
            FMips[i][0] := TMipImage.Create(targetWidth, targetHeight, ImgData[i].Bits, ImgData[i].Size, FFormat);
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
      FMips[j][i] := TMipImage.Create(PImg^.Width, PImg^.Height, PImg^.Bits, PImg^.Size, FFormat);
      w := w shr 2;
      h := h shr 2;
    end;
  end;
end;

constructor TTextureData.CreateEmpty(AWidth, AHeight: Integer; AFormat: TImageFormat; withMips: Boolean; AllocateTextureMemory: Boolean);
var i: Integer;
    AllocSize: Integer;
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
    if AllocateTextureMemory then
      AllocSize := AWidth * AHeight * ImagePixelSize[FFormat]
    else
      AllocSize := 0;
    FMips[0][i] := TMipImage.Create(AWidth, AHeight, Nil, AllocSize, FFormat);
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


