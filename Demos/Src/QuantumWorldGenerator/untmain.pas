unit untmain;
{$I avConfig.inc}

interface

uses
    Windows,
  Classes, SysUtils, {FileUtil,} Forms, Controls, Graphics, Dialogs, mutils,
  avTileSplitter, avQuantumWorldGen, avTypes, avContnrsDefaults;

const
  WORLD_SIZE_X = 800;
  WORLD_SIZE_Y = 400;
  TILE_SIZE = 9;
  TILE_STEP = 9;
//  TILE_SIZE = 3;
//  TILE_STEP = 1;

type
  IMap = {$IfDef FPC}specialize{$EndIf} IQMap<TVec2i>;
  ITiledMapDesc = interface(IMap)
    function GetTile(const AIndex: Integer): TTileDesc;
  end;

  { TMap }

  TMap = class(TInterfacedObject, IMap, ITiledMapDesc)
  private
    FTilesCount: Integer;
    FTileSet: ITileSet;
    FAreaSize: TVec2i;
    FWrappedX: Boolean;
    FWrappedY: Boolean;

    function TilesCount      : Integer;
    function DirectionsCount : Integer;
    function GetPossibleTiles(const ADirection, FromTile: Integer): TSuperPosition2;

    function MaxNeighbourCount(const ANode: TVec2i): Integer;
    function GetNeighbour(ADirection: Integer; const ANode: TVec2i; out ANeighbour: TVec2i): Boolean;

    function NodeComparer: IEqualityComparer;

    function GetTile(const AIndex: Integer): TTileDesc;
  public
    constructor Create(const AFileName: string; const AreaSize: TVec2i; wrappedX, wrappedY: Boolean);
  end;

  IWorld = {$IfDef FPC}specialize{$EndIf} IQGen<TVec2i>;
  TWorld = {$IfDef FPC}specialize{$EndIf} TQGen<TVec2i>;

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    FBackBmp : TBitmap;
    FMapDesc: ITiledMapDesc;
    FWorld  : IWorld;
    procedure ReduceEvent(ASender: TObject);
    procedure ResolveWorld;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$IfnDef notDCC}
  {$R *.dfm}
{$EndIf}

{$IfDef FPC}
  {$R *.lfm}
{$EndIf}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FMapDesc := TMap.Create('pattern.bmp', Vec(WORLD_SIZE_X, WORLD_SIZE_Y), True, True);
  FWorld := TWorld.Create(FMapDesc);
  FWorld.OnReduce := ReduceEvent;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ResolveWorld;
  Invalidate;
end;

procedure TForm1.FormPaint(Sender: TObject);
  type
    TRGB = packed record
      r, g, b: Byte;
    end;
    PRGB = ^TRGB;
  function GetPixel(bmp: TBitmap; x,y: Integer): PRGB;
  begin
    Result := bmp.ScanLine[y];
    Inc(Result, x);
  end;

var
  anyData   : TBoolArr;
  singleData: TBoolArr;

  procedure DrawTile(const bmp: TBitmap; const X, Y: Integer; SP: TSuperPosition2);
  var k, i, j, offset: Integer;
      pCol: PRGB;
      pSrc: TRGBA;
      data: TTilesSPosition;
      r,g,b,n: Integer;
      tile: TTileDesc;
  begin
    offset := 0;
//    if SP.InSingleState then
//    begin
//      data := singleData;
//      offset := SP.count;
//    end
//    else
//      if SP.InAnyPosition then
//        data := anyData
//      else
//        if SP.InNoPosition then
//          data := nil
//        else
//          data := SP.data;
    data := SP.AsBitsArray;

    for j := 0 to TILE_SIZE - 1 do
      for i := 0 to TILE_SIZE - 1 do
      begin
        r := 0;
        g := 0;
        b := 0;
        n := 0;
        for k := 0 to Length(data) - 1 do
        begin
          //if not data[k] then Continue;
          tile := FMapDesc.GetTile(data[k]);
          pSrc := tile.Pixel(i, TILE_SIZE-1-j);
          Inc(r, pSrc.r);
          Inc(g, pSrc.g);
          Inc(b, pSrc.b);
          Inc(n);
        end;
        if n > 0 then
        begin
          r := r div n;
          g := g div n;
          b := b div n;
        end;
        pCol := GetPixel(bmp, X*TILE_SIZE+i, Y*TILE_SIZE+j);
        pCol^.r := r;
        pCol^.g := g;
        pCol^.b := b;
      end;
  end;

var
//  FBackBmp: TBitmap;
  i, j: Integer;
begin
  SetLength(anyData, FMapDesc.TilesCount);
  SetLength(singleData, 1);
  singleData[0] := True;
  for i := 0 to Length(anyData) - 1 do anyData[i] := True;

  if FBackBmp = nil then
  begin
      FBackBmp := TBitmap.Create;
      FBackBmp.Width := WORLD_SIZE_X*TILE_SIZE;
      FBackBmp.Height := WORLD_SIZE_Y*TILE_SIZE;
      FBackBmp.PixelFormat := pf24bit;
  end;
  for j := 0 to WORLD_SIZE_Y - 1 do
    for i := 0 to WORLD_SIZE_X - 1 do
      DrawTile(FBackBmp, i, j, FWorld.Get(Vec(i,j)));

  Canvas.StretchDraw(Rect(0,0,FBackBmp.Width,FBackBmp.Height), FBackBmp);
end;

procedure TForm1.ReduceEvent(ASender: TObject);
begin
    Exit;
  if Random(3) = 0 then
  begin
      Invalidate;
      UpdateWindow(Handle);
  end;
end;

procedure TForm1.ResolveWorld;
var i, j: Integer;
    area: TVec2iArr;
begin
  SetLength(area, WORLD_SIZE_X*WORLD_SIZE_Y);
  for j := 0 to WORLD_SIZE_Y - 1 do
    for i := 0 to WORLD_SIZE_X - 1 do
      area[j*WORLD_SIZE_X+i] := Vec(i, j);
  FWorld.Resolve(area);
end;

{ TMap }

function TMap.TilesCount: Integer;
begin
  Result := FTileSet.TilesCount;
end;

function TMap.DirectionsCount: Integer;
begin
  Result := 4;
end;

function TMap.GetPossibleTiles(const ADirection, FromTile: Integer): TSuperPosition2;
var arr: TIntArr;
    dir: TDirection;
    i: Integer;
begin
  case ADirection of
    0 : dir := dTop;
    1 : dir := dRight;
    2 : dir := dBottom;
    3 : dir := dLeft;
  else
    dir := dTop;
  end;
  arr := FTileSet.PossibleNeighbours(FromTile, dir);

  Result.Init(FTilesCount, arr);
  {
  Result.count := Length(arr);
  case Result.count of
    0 : Exit(NoSuperPosition);
    1 : begin
      Result.count := arr[0];
      Result.data := nil;
      Exit;
    end;
  else
    if Result.count = FTilesCount then
      Exit(AnySuperPosition)
    else
    begin
      Result.data := nil;
      SetLength(Result.data, FTilesCount);
      FillChar(Result.data[0], Length(Result.data), 0);
      for i := 0 to Length(arr) - 1 do
        Result.data[arr[i]] := True;
    end;
  end;
  }
end;

function TMap.GetNeighbour(ADirection: Integer; const ANode: TVec2i; out ANeighbour: TVec2i): Boolean;
begin
  case ADirection of
    0 : ANeighbour := Vec(ANode.x, ANode.y + 1);
    1 : ANeighbour := Vec(ANode.x + 1, ANode.y);
    2 : ANeighbour := Vec(ANode.x, ANode.y - 1);
    3 : ANeighbour := Vec(ANode.x - 1, ANode.y);
  else
    Exit(False);
  end;

  if ANeighbour.x < 0 then
    if FWrappedX then
      ANeighbour.x := ANeighbour.x + FAreaSize.x
    else
      Exit(False);

  if ANeighbour.y < 0 then
    if FWrappedX then
      ANeighbour.y := ANeighbour.y + FAreaSize.y
    else
      Exit(False);

  if ANeighbour.x >= FAreaSize.x then
    if FWrappedX then
      ANeighbour.x := ANeighbour.x - FAreaSize.x
    else
      Exit(False);

  if ANeighbour.y >= FAreaSize.y then
    if FWrappedY then
      ANeighbour.y := ANeighbour.y - FAreaSize.y
    else
      Exit(False);

  Result := True;
end;

function TMap.NodeComparer: IEqualityComparer;
begin
  Result := AutoSelectEqualityComparer(TypeInfo(TVec2i), SizeOf(TVec2i))
end;

function TMap.GetTile(const AIndex: Integer): TTileDesc;
begin
  Result := FTileSet.GetTileDesc(AIndex);
end;

function TMap.MaxNeighbourCount(const ANode: TVec2i): Integer;
begin
  Result := 4;
end;

constructor TMap.Create(const AFileName: string; const AreaSize: TVec2i;
  wrappedX, wrappedY: Boolean);
begin
  FTileSet := SplitTilesFromFile(AFileName, TILE_SIZE, TILE_STEP, [tgoAllowRotate]);
  FTilesCount := FTileSet.TilesCount;
  FWrappedX := wrappedX;
  FWrappedY := wrappedY;
  FAreaSize := AreaSize;
end;

end.

