unit CubeMapConverter;

{$IfDef FPC}
  {$mode objfpc}{$H+}
{$EndIf}

interface

uses
  SysUtils,
  mutils,
  ImagingUtility,
  Imaging,
  ImagingTypes,
  ImagingDDS;

type
  TConvertParams = record
    Input     : string;
    Output    : string;
    OutputFmt : TImageFormat;
    OutputSize: Integer;
    Mult      : Single;
    GenMips   : Boolean;
  end;

  EConvertError = class (Exception);

procedure DoConvert(const AParams: TConvertParams);
function ParseParams: TConvertParams;

implementation

uses Math;

procedure BuildCube(const src: TImageData; const dst: TDynImageDataArray; const Mult: Single);
  function GetLerpPixel(const src: TImageData; crd: TVec2): TColorFPRec;
  var p1, p2, p3, p4: TColorFPRec;
      t: TVec2;
      i: Integer;
  begin
    t.x := crd.x - Floor(crd.x);
    t.y := crd.y - Floor(crd.y);

    p1 := GetPixelFP(src, Floor(crd.x), Floor(crd.y));
    p2 := GetPixelFP(src, Ceil(crd.x), Floor(crd.y));

    p3 := GetPixelFP(src, Floor(crd.x), Ceil(crd.y));
    p4 := GetPixelFP(src, Ceil(crd.x), Ceil(crd.y));

    for i := 0 to 3 do
    begin
      Result.Channels[i] := Lerp(Lerp(p1.Channels[i], p2.Channels[i], t.x), Lerp(p3.Channels[i], p4.Channels[i], t.x), t.y) * Mult;
      Result.Channels[i] := Max(0, Result.Channels[i]);
    end;
  end;
var i, j, slice: Integer;
    ray: TVec3;
    srcPix: TVec2;
    SideSize: Integer;
    r: Single;
begin
  SideSize := dst[0].Height;
  for slice := 0 to Length(dst) - 1 do
  begin
    for j := 0 to SideSize - 1 do
      for i := 0 to SideSize - 1 do
      begin
        case slice of
          0: begin //positiveX
            ray.x := SideSize*0.5;
            ray.z := - i + SideSize*0.5;
            ray.y :=   j - SideSize*0.5;
          end;
          1: begin //negativeX
            ray.x := -SideSize*0.5;
            ray.z := i - SideSize*0.5;
            ray.y := j - SideSize*0.5;
          end;
          2: begin //positiveY
            ray.x := i - SideSize*0.5;
            ray.z := j - SideSize*0.5;
            ray.y := -SideSize*0.5;
          end;
          3: begin //negativeY
            ray.x :=   i - SideSize*0.5;
            ray.z := - j + SideSize*0.5;
            ray.y := SideSize*0.5;
          end;
          4: begin //positiveZ
            ray.x := i - SideSize*0.5;
            ray.z := SideSize*0.5;
            ray.y := j - SideSize*0.5;
          end;
          5: begin //negativeZ
            ray.x := -i + SideSize*0.5;
            ray.z := -SideSize*0.5;
            ray.y := j - SideSize*0.5;
          end;
        end;
        ray := Clamp(Normalize(ray), -1, 1);

        if src.Width <> src.Height then
        begin
          srcPix.x := (ArcTan2(ray.z, ray.x)+PI)/(2*PI);
          srcPix.y := ArcCos(-ray.y)/PI;
        end
        else
        begin
          //(Dx*r,Dy*r) where r=(1/pi)*acos(Dz)/sqrt(Dx^2 + Dy^2).
          r := (1/PI)*ArcCos(ray.z)/sqrt(sqr(ray.x) + sqr(ray.y));
          srcPix.x := (ray.x * r + 1.0)*0.5;
          srcPix.y := (ray.y * r + 1.0)*0.5;
        end;

        srcPix.x := srcPix.x * src.Width;
        srcPix.y := srcPix.y * src.Height;
        SetPixelFP(dst[slice], i, j, GetLerpPixel(src, srcPix));
      end;
  end;
end;

procedure GenCubeMips(var dst: TDynImageDataArray);
var i,j,k: Integer;
    mips: TDynImageDataArray;
    newArray: TDynImageDataArray;
begin
  newArray := nil;
  for i := 0 to Length(dst) - 1 do
  begin    
    mips := nil;
    GenerateMipMaps(dst[i], GetMipsCount(dst[i].Width, dst[i].Height), mips);

    k := Length(newArray);
    SetLength(newArray, k + Length(mips));
    for j := 0 to Length(mips)-1 do
      newArray[k+j] := mips[j];

    FreeImage(dst[i]);
  end;
  dst := newArray;
end;

procedure DoConvert(const AParams: TConvertParams);
var src: TImageData;
    dst: TDynImageDataArray;

    ddsFormat: TDDSFileFormat;
    i: Integer;
begin
  InitImage(src);
  SetLength(dst, 6);
  for i := 0 to 5 do InitImage(dst[i]);
  try                                                     
    if not LoadImageFromFile(AParams.Input, src) then
      raise EConvertError.Create('Can''t open file: ' + ExpandFileName(AParams.Input));
    SaveImageToFile('tmp.png', src);

    for i := 0 to 5 do
      if not NewImage(AParams.OutputSize, AParams.OutputSize, AParams.OutputFmt, dst[i]) then
        raise EConvertError.Create('Can''t allocate output image');

    BuildCube(src, dst, AParams.Mult);

    if AParams.GenMips then
      GenCubeMips(dst);

    ddsFormat := TDDSFileFormat.Create();
    try
      ddsFormat.SaveVolume := False;
      ddsFormat.SaveCubeMap := True;
      ddsFormat.SaveDepth := 6;      
      if AParams.GenMips then
        ddsFormat.SaveMipMapCount := GetMipsCount(AParams.OutputSize, AParams.OutputSize)
      else
        ddsFormat.SaveMipMapCount := 1;
      if not ddsFormat.SaveToFile(AParams.Output, dst) then
        raise EConvertError.Create('Can''t save file: ' + ExpandFileName(AParams.Output));
    finally
      FreeAndNil(ddsFormat);
    end;
  finally
    FreeImage(src);
    for i := 0 to 5 do FreeImage(dst[i]);
  end;
end;

procedure PrintHelp;
begin
  WriteLn('Usage: HDR2CubeMap.exe <input file> <params>');
  WriteLn('  <params>:');
  WriteLn('    -o<filename> - set output filename');
  WriteLn('    -s<int size> - output cube face size');
  WriteLn('    -f<string pixelformat> - output pixel format:');
  WriteLn('                             RGBA');
  WriteLn('                             RGBA16f');
  WriteLn('                             RGBA32f');
  WriteLn('    -m<float mult> - color multiplier');
  WriteLn('    -g - generate mips');
end;

procedure InvalidParam(const s: string);
begin
  WriteLn('Invalid param: "', s, '"');
end;

function ParseParams: TConvertParams;
var
  i: Integer;
  s, pVal: String;
  n: Integer;
  f: Single;
begin
  if (ParamCount = 0) or ( (ParamCount=1) and ((ParamStr(1) = '/?') or (ParamStr(1) = '-?')) ) then
  begin
    PrintHelp;
    Halt(1);
  end;

  Result.Input := ParamStr(1);
  Result.Output := Result.Input + '_out.dds';
  Result.OutputFmt := TImageFormat.ifA8R8G8B8;
  Result.OutputSize := 512;
  Result.Mult := 1;
  Result.GenMips := False;

  for i := 2 to ParamCount do
  begin
    s := ParamStr(i);
    if Length(s) < 2 then 
    begin
      InvalidParam(s);
      Continue;
    end;
    if s[1] <> '-' then
    begin
      InvalidParam(s);
      Continue;    
    end;
    pVal := Copy(s, 3, Length(s) - 2);
    
    if s[2] = 'o' then
    begin
      Result.Output := pVal;
      Continue;
    end;
    
    if s[2] = 's' then
    begin
      if not TryStrToInt(pVal, n) then
        InvalidParam(s)
      else
        Result.OutputSize := n;
      Continue;
    end;      

    if s[2] = 'f' then
    begin
      if pVal = 'RGBA' then
        Result.OutputFmt := TImageFormat.ifA8R8G8B8
      else if pVal = 'RGBA16f' then
        Result.OutputFmt := TImageFormat.ifA16B16G16R16F
      else if pVal = 'RGBA32f' then
        Result.OutputFmt := TImageFormat.ifA32B32G32R32F
      else
        InvalidParam(s);        
      Continue;
    end;     

    if s[2] = 'm' then
    begin
      if not TryStrToFloat(pVal, f) then
        InvalidParam(s)
      else
        Result.Mult := f;
      Continue;
    end;

    if s[2] = 'g' then
    begin
      if pVal = '' then
        Result.GenMips := True
      else
        InvalidParam(s);        
      Continue;
    end;

    InvalidParam(s);
  end;
end;

end.

