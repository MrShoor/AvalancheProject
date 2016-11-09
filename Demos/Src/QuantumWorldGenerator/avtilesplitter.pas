unit avTileSplitter;

{$I avConfig.inc}

interface

uses
  Classes, SysUtils, avTypes;

type
  TRGBA = packed record
    r, g, b, a: Byte;
    {$IfDef DCC}
    class operator Equal(const a, b: TRGBA): Boolean;
    class operator NotEqual(const a, b: TRGBA): Boolean;
    {$EndIf}
  end;
  TRGBA_array = array of TRGBA;
  PRGBA = ^TRGBA;

  { ITileImage }

  ITileImage = interface
    function Width : Integer;
    function Height: Integer;
    function Data  : PRGBA;

    function GetPixel(const X, Y: Integer): TRGBA;
    function GetPPixel(const X, Y: Integer): PRGBA;
  end;

  TTileRotate = (rot0, rot90, rot180, rot270);
  TDirection = (dTop, dRight, dBottom, dLeft);

  { TTileDesc }

  TTileDesc = packed record
    rotation: TTileRotate;
    flipped : Boolean;
    image   : ITileImage;
    function Pixel(x, y: Integer): TRGBA;
  end;

  ITileSet = interface
    function ImagesCount: Integer;
    function GetTileImage(const AIndex: Integer): ITileImage;

    function TilesCount: Integer;
    function GetTileDesc(const AIndex: Integer): TTileDesc;
    function PossibleNeighbours(const ASrcTile: Integer; const ADirection: TDirection): TIntArr;
  end;

  TTileGenOptions = set of (tgoAllowRotate, tgoAllowFlipH, tgoAllowFlipV);

function SplitTilesFromFile(const AFileName: string; ATileSize: Integer; ASplitStep: Integer = 1; AGenOptions: TTileGenOptions = [tgoAllowRotate, tgoAllowFlipH]): ITileSet;

implementation

uses
  avContnrs, avContnrsDefaults, avTexLoader;

type

  ITileImageArr = {$IfDef FPC}specialize{$EndIf} IArray<ITileImage>;
  TTileImageArr = {$IfDef FPC}specialize{$EndIf} TArray<ITileImage>;

  ITileArr = {$IfDef FPC}specialize{$EndIf} IArray<TTileDesc>;
  TTileArr = {$IfDef FPC}specialize{$EndIf} TArray<TTileDesc>;

  { TTileImage }

  TTileImage = class (TInterfacedObject, ITileImage)
  private
    FWidth : Integer;
    FHeight: Integer;
    FData  : TRGBA_array;
  public
    function Width : Integer;
    function Height: Integer;
    function Data  : PRGBA;
    function GetPixel(const X, Y: Integer): TRGBA;
    function GetPPixel(const X, Y: Integer): PRGBA;
    constructor Create(const AWidth, AHeight: Integer; const AData: TRGBA_array);
  end;

  { TTileSet }

  TTileSet = class (TInterfacedObject, ITileSet)
  private
    FImages: ITileImageArr;
    FTiles : ITileArr;

    FLinks : array of array [TDirection] of TIntArr; // [TileIndex][Direction]

    function ImagesCount: Integer;
    function GetTileImage(const AIndex: Integer): ITileImage;

    function TilesCount: Integer;
    function GetTileDesc(const AIndex: Integer): TTileDesc;
    function PossibleNeighbours(const ASrcTile: Integer; const ADirection: TDirection): TIntArr;
  public
    constructor Create(const AFileName: string; ATileSize, ASplitStep: Integer; AGenOptions: TTileGenOptions);
  end;

{$IfDef FPC}
operator = (const a, b: TRGBA): Boolean;
var ia: Cardinal absolute a;
    ib: Cardinal absolute b;
begin
  Result := ia = ib;
end;
{$EndIf}

{$IfDef DCC}
class operator TRGBA.Equal(const a, b: TRGBA): Boolean;
var ia: Cardinal absolute a;
    ib: Cardinal absolute b;
begin
  Result := ia = ib;
end;

class operator TRGBA.NotEqual(const a, b: TRGBA): Boolean;
var ia: Cardinal absolute a;
    ib: Cardinal absolute b;
begin
  Result := ia <> ib;
end;
{$EndIf}

function SplitTilesFromFile(const AFileName: string; ATileSize, ASplitStep: Integer; AGenOptions: TTileGenOptions): ITileSet;
begin
  Result := TTileSet.Create(AFileName, ATileSize, ASplitStep, AGenOptions);
end;

{ TTileDesc }

function TTileDesc.Pixel(x, y: Integer): TRGBA;
var newX, newY: Integer;
begin
  if flipped then
    x := image.Width - x;
  case rotation of
    rot0 : begin
      newX := x;
      newY := y;
    end;
    rot90 : begin
      newX := y;
      newY := image.Height - 1 - x;
    end;
    rot180: begin
      newX := image.Width - 1 - x;
      newY := image.Height - 1 - y;
    end;
    rot270: begin
      newX := image.Width - 1 - y;
      newY := x;
    end;
  end;
  Result := image.GetPixel(newX, newY);
end;

{ TTileImage }

function TTileImage.Width: Integer;
begin
  Result := FWidth;
end;

function TTileImage.Height: Integer;
begin
  Result := FHeight;
end;

function TTileImage.Data: PRGBA;
begin
  Result := @FData[0];
end;

function TTileImage.GetPixel(const X, Y: Integer): TRGBA;
begin
  Result := FData[Y*FWidth+X];
end;

function TTileImage.GetPPixel(const X, Y: Integer): PRGBA;
begin
  Result := @FData[Y*FWidth+X];
end;

constructor TTileImage.Create(const AWidth, AHeight: Integer; const AData: TRGBA_array);
begin
  FWidth := AWidth;
  FHeight := AHeight;
  FData := AData;
  Assert(FWidth*FHeight <= Length(FData));
end;

function TTileSet.ImagesCount: Integer;
begin
  Result := FImages.Count;
end;

function TTileSet.GetTileImage(const AIndex: Integer): ITileImage;
begin
  Result := FImages[AIndex];
end;

function TTileSet.TilesCount: Integer;
begin
  Result := FTiles.Count;
end;

function TTileSet.GetTileDesc(const AIndex: Integer): TTileDesc;
begin
  Result := FTiles[AIndex];
end;

function TTileSet.PossibleNeighbours(const ASrcTile: Integer; const ADirection: TDirection): TIntArr;
begin
  Result := FLinks[ASrcTile][ADirection];
end;

constructor TTileSet.Create(const AFileName: string; ATileSize, ASplitStep: Integer; AGenOptions: TTileGenOptions);

  function CreateTileAtXY(const img: ITextureMip; x, y: Integer): ITileImage;
  var data: TRGBA_array;
      j, i: Integer;
  begin
    SetLength(data, ATileSize*ATileSize);
    Result := TTileImage.Create(ATileSize, ATileSize, data);
    for j := 0 to ATileSize - 1 do
      for i := 0 to ATileSize - 1 do
        Result.GetPPixel(i, j)^ := PRGBA(img.Pixel(i+x, j+y))^;
  end;

  function SideMatch(const SrcTile: TTileDesc; const DestTile: TTileDesc; const Dir: TDirection): Boolean;
  var i: Integer;
  begin
    Result := False;
    case Dir of
      dTop: begin
        for i := 0 to ATileSize - 1 do
          if SrcTile.Pixel(i, 0) <> DestTile.Pixel(i, ATileSize-1) then Exit;
      end;
      dRight: begin
        for i := 0 to ATileSize - 1 do
          if SrcTile.Pixel(ATileSize-1, i) <> DestTile.Pixel(0, i) then Exit;
      end;
      dBottom: begin
        for i := 0 to ATileSize - 1 do
          if SrcTile.Pixel(i, ATileSize-1) <> DestTile.Pixel(i, 0) then Exit;
      end;
      dLeft: begin
        for i := 0 to ATileSize - 1 do
          if SrcTile.Pixel(0, i) <> DestTile.Pixel(ATileSize-1, i) then Exit;
      end;
    end;
    Result := True;
  end;

var img: ITextureMip;
    tile: TTileDesc;
    i, j: Integer;
    dir: TDirection;
begin
  FImages := TTileImageArr.Create();
  FTiles := TTileArr.Create();

  //prepare images
  img := LoadTexture(AFileName, SIZE_DEFAULT, SIZE_DEFAULT, TImageFormat.A8R8G8B8).MipData(0, 0);
  j := 0;
  while j < img.Width - ATileSize - 1 do
  begin
    i := 0;
    while i < img.Height - ATileSize - 1 do
    begin
      FImages.Add(CreateTileAtXY(img, i, j));
      Inc(i, ASplitStep);
      Inc(j, ASplitStep);
    end;
  end;

  //prepare tiles
  for i := 0 to FImages.Count - 1 do
  begin
    tile.flipped := False;
    tile.rotation := rot0;
    tile.image := FImages[i];
    FTiles.Add(tile);

    if tgoAllowRotate in AGenOptions then
    begin
      tile.rotation := rot90;
      FTiles.Add(tile);
      tile.rotation := rot180;
      FTiles.Add(tile);
      tile.rotation := rot270;
      FTiles.Add(tile);
    end;
  end;

  for i := 0 to FTiles.Count - 1 do
  begin
    tile := FTiles[i];
    if tgoAllowFlipH in AGenOptions then
    begin
      tile.flipped := true;
      FTiles.Add(tile);
    end;
  end;
  //todo tgoAllowFlipV

  //build links
  FLinks := nil;
  SetLength(FLinks, FTiles.Count);
  for i := 0 to FTiles.Count - 1 do
    for j := 0 to FTiles.Count - 1 do
      for dir := Low(TDirection) to High(TDirection) do
      begin
        if SideMatch(FTiles[i], FTiles[j], dir) then
        begin
          SetLength(FLinks[i][dir], Length(FLinks[i][dir])+1);
          FLinks[i][dir][High(FLinks[i][dir])] := j;
        end;
      end;
end;

end.

