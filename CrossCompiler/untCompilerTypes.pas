unit untCompilerTypes;

interface

uses
  Windows, Classes, SysUtils, untGLWrappers;

type
  ECompileError = class (Exception);

  TStatus = (csNone, csSuccessed, csFailed);
  TShaderTarget = (stGeometry, stVertex, stFragment);
  TBytes = TArray<Byte>;
  TGeomBins = TArray<string>;
  TVertBins = TArray<string>;
  TFragBins = TArray<string>;

  IInfo = interface
  ['{35AA7EE0-66ED-4F17-9B06-37C3850F96C8}']
    function GetLog: WideString; stdcall;
    function GetStatus: TStatus; stdcall;
    procedure SetLog(const Value: WideString); stdcall;
    procedure SetStatus(const Value: TStatus); stdcall;

    property Status: TStatus read GetStatus write SetStatus;
    property Log: WideString read GetLog write SetLog;
  end;

  ICompileData = interface (IInfo)
  ['{53A71D46-8C62-4F25-B567-64DB888DED9B}']
    function GetCode: WideString; stdcall;
    function GetBin: TBytes; stdcall;
    function GetTarget: TShaderTarget; stdcall;
    procedure SetCode(const Value: WideString); stdcall;
    procedure SetBin(const Value: TBytes); stdcall;
    procedure SetTarget(const Value: TShaderTarget); stdcall;

    procedure Compile; stdcall;
    property Target: TShaderTarget read GetTarget write SetTarget;
    property Code: WideString read GetCode write SetCode;
    property Bin: TBytes read GetBin write SetBin;
  end;

  ILinkData = interface (IInfo)
  ['{679EE921-F924-444D-A2CB-8B2C0F47CF93}']
    function GetGeom: TGeomBins; stdcall;
    function GetFrag: TFragBins; stdcall;
    function GetVert: TVertBins; stdcall;
    function GetName: string; stdcall;
    procedure SetGeom(const Value: TGeomBins); stdcall;
    procedure SetVert(const Value: TVertBins); stdcall;
    procedure SetFrag(const Value: TFragBins); stdcall;
    procedure SetName(const Value: string); stdcall;

    procedure Link(AProgram: IProgram); overload; stdcall;
    procedure Link; overload; stdcall;

    property Geom: TGeomBins read GetGeom write SetGeom;
    property Vert: TVertBins read GetVert write SetVert;
    property Frag: TFragBins read GetFrag write SetFrag;
    property Name: string    read GetName write SetName;
  end;

function CreateCompileData(target: TShaderTarget; code: string = ''): ICompileData;
function CreateLinkData(const name: string; geom: TGeomBins; vert: TVertBins; frag: TFragBins): ILinkData;

implementation

uses
  dglOpenGL;

type
  TInfo = class(TInterfacedObject, IInfo)
  private
    FLog: string;
    FStatus: TStatus;
    function GetLog: WideString; stdcall;
    function GetStatus: TStatus; stdcall;
    procedure SetLog(const Value: WideString); stdcall;
    procedure SetStatus(const Value: TStatus); stdcall;
  public
    property Status: TStatus read GetStatus write SetStatus;
    property Log: WideString read GetLog write SetLog;
  end;

  TCompileData = class (TInfo, ICompileData)
  private
    FCode: string;
    FTarget: TShaderTarget;
    FBin: TBytes;
    function GetCode: WideString; stdcall;
    function GetBin: TBytes; stdcall;
    function GetTarget: TShaderTarget; stdcall;
    procedure SetCode(const Value: WideString); stdcall;
    procedure SetBin(const Value: TBytes); stdcall;
    procedure SetTarget(const Value: TShaderTarget); stdcall;
  public
    procedure Compile; stdcall;
    property Target: TShaderTarget read GetTarget write SetTarget;
    property Code: WideString read GetCode write SetCode;
    property Bin: TBytes read GetBin write SetBin;
  end;

  TLinkData = class (TInfo, ILinkData)
  private
    FGeom: TGeomBins;
    FVert: TVertBins;
    FFrag: TFragBins;
    FName: string;

    function GetGeom: TGeomBins; stdcall;
    function GetFrag: TFragBins; stdcall;
    function GetVert: TVertBins; stdcall;
    function GetName: string; stdcall;
    procedure SetGeom(const Value: TGeomBins); stdcall;
    procedure SetVert(const Value: TVertBins); stdcall;
    procedure SetFrag(const Value: TFragBins); stdcall;
    procedure SetName(const Value: string); stdcall;
  public
    procedure Link(AProgram: IProgram); overload; stdcall;
    procedure Link; overload; stdcall;

    property Geom: TGeomBins read GetGeom write SetGeom;
    property Vert: TVertBins read GetVert write SetVert;
    property Frag: TFragBins read GetFrag write SetFrag;
    property Name: string    read GetName write SetName;
  end;

function CreateCompileData(target: TShaderTarget; code: string = ''): ICompileData;
begin
  Result := TCompileData.Create;
  Result.Code := code;
  Result.Target := target;
end;

function CreateLinkData(const name: string; geom: TGeomBins; vert: TVertBins; frag: TFragBins): ILinkData;
begin
  Result := TLinkData.Create;
  Result.Geom := geom;
  Result.Vert := vert;
  Result.Frag := frag;
  Result.Name := name;
end;

{ TCompileData }

procedure TCompileData.Compile;
var shader: IShader;
begin
  case Target of
    stGeometry: shader := CreateShader(GL_GEOMETRY_SHADER);
    stVertex  : shader := CreateShader(GL_VERTEX_SHADER);
    stFragment: shader := CreateShader(GL_FRAGMENT_SHADER);
  end;
  Assert(assigned(shader), 'shader is nil');
  if shader.CopmileShader(AnsiString(FCode), FLog) then
    Status := csSuccessed
  else
    Status := csFailed;
end;

function TCompileData.GetBin: TBytes;
begin
  Result := FBin;
end;

function TCompileData.GetCode: WideString;
begin
  Result := FCode;
end;

function TCompileData.GetTarget: TShaderTarget;
begin
  Result := FTarget;
end;

procedure TCompileData.SetBin(const Value: TBytes);
begin
  FBin := Value;
end;

procedure TCompileData.SetCode(const Value: WideString);
begin
  FCode := Value;
end;

procedure TCompileData.SetTarget(const Value: TShaderTarget);
begin
  FTarget := Value;
end;

{ TInfo }

function TInfo.GetLog: WideString;
begin
  Result := FLog;
end;

function TInfo.GetStatus: TStatus;
begin
  Result := FStatus;
end;

procedure TInfo.SetLog(const Value: WideString);
begin
  FLog := Value;
end;

procedure TInfo.SetStatus(const Value: TStatus);
begin
  FStatus := Value;
end;

{ TLinkData }

function TLinkData.GetFrag: TFragBins;
begin
  Result := FFrag;
end;

function TLinkData.GetGeom: TGeomBins;
begin
  Result := FGeom;
end;

function TLinkData.GetName: string;
begin
  Result := FName;
end;

function TLinkData.GetVert: TVertBins;
begin
  Result := FVert;
end;

procedure TLinkData.Link(AProgram: IProgram);
var i: Integer;
begin
  for i := 0 to Length(FGeom) - 1 do
    AProgram.AddShader(GL_GEOMETRY_SHADER, AnsiString(FGeom[i]));

  for i := 0 to Length(FVert) - 1 do
    AProgram.AddShader(GL_VERTEX_SHADER, AnsiString(FVert[i]));

  for i := 0 to Length(FFrag) - 1 do
    AProgram.AddShader(GL_FRAGMENT_SHADER, AnsiString(FFrag[i]));

  if AProgram.Link(FLog) then
    Status := csSuccessed
  else
    Status := csFailed;
end;

procedure TLinkData.Link;
var prog: IProgram;
    totallog: string;
begin
  prog := CreateProgram;

  totallog := 'Linking ''' + Name + ''' program... ';
  Link(prog);
  case Status of
    csNone: totallog := totallog + 'unknown';
    csSuccessed: totallog := totallog + 'done';
    csFailed: totallog := totallog + 'failed';
  end;
  totallog := totallog + sLineBreak + Log;
  Log := totallog;
  if Status <> csSuccessed then Exit;
end;

procedure TLinkData.SetFrag(const Value: TFragBins);
begin
  FFrag := Value;
end;

procedure TLinkData.SetGeom(const Value: TGeomBins);
begin
  FGeom := Value;
end;

procedure TLinkData.SetName(const Value: string);
begin
  FName := Value;
end;

procedure TLinkData.SetVert(const Value: TVertBins);
begin
  FVert := Value;
end;

//initialization
//InitOpenGL;

end.
