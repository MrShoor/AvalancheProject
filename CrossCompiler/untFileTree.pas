unit untFileTree;

interface

uses
  SysUtils, Classes, untCompileTask;

type
  EParseError = class (Exception);
  EPlacedParseError = class (EParseError)
  private
    FFileName: string;
    FColNum: Integer;
    FRowNum: Integer;
  public
    property FileName: string read FFileName;
    property RowNum: Integer read FRowNum;
    property ColNum: Integer read FColNum;

    constructor Create(const AFileName: string; ARowNum, AColNum: Integer; const Msg: string);
  end;

  IFileNode = interface
  ['{18FF9A17-DE83-4DD6-80E8-2299766BCD6C}']
    function GetFileName: WideString; stdcall;
    function GetChild(index: Integer): IFileNode; stdcall;
    function GetLine(index: integer): string; stdcall;
    function GetFullLine(index: integer): string; stdcall;

    property FileName: WideString read GetFileName;
//    property RelativeFileName: WideString read GetRelativeFileName;

    function LinesCount: integer; stdcall;
    property Lines[index: integer]: string read GetLine;
    function FullLinesCount: integer; stdcall;
    property FullLines[index: integer]: string read GetFullLine;
    function FullText: string; stdcall;

    function ChildCount: Integer; stdcall;
    property Child[index: Integer]: IFileNode read GetChild;

    function GetFileNameByLine(var LineIndex: Integer): string;

    function ConsistFile(const AFileName: WideString): Boolean;
    function Loaded: Boolean; stdcall;
  end;

  IFileNodeManager = interface
  ['{1BE639E7-8D80-4059-BB98-CC769C81B074}']
    function GetFullFileName(const FileName: string): string; stdcall;
    function GetFileNode(const FileName: string; const requestby: IFileNode): IFileNode; stdcall;
  end;

procedure CreateFileNodeManager(const CompileTask: ICompileTask);
function GetFileNodeManager: IFileNodeManager;

implementation

uses
  Generics.Collections;

type
  TFileNode = class;
  TNodeManager = class;

  TChildInfo = record
    LinePos: Integer;
    Node: IFileNode;
  end;

  TFileNode = class (TInterfacedObject, IFileNode)
  private
    FChilds: TList<TChildInfo>;
    FFileName: string;
    FFullFileName: string;
    FFullText: TStringList;
    FRealText: TStringList;
    FLoaded: Boolean;

    function GetLine(index: integer): string; stdcall;
    function GetFullLine(index: integer): string; stdcall;
    function GetChild(index: Integer): IFileNode; stdcall;
    function GetFileName: WideString; stdcall;
  public
    function LinesCount: integer; stdcall;
    property Lines[index: integer]: string read GetLine;
    function FullLinesCount: integer; stdcall;
    property FullLines[index: integer]: string read GetFullLine;

    function FullText: string; stdcall;
    function ConsistFile(const AFileName: WideString): Boolean;
    function Loaded: Boolean; stdcall;

    function ChildCount: Integer; stdcall;
    property Child[index: Integer]: IFileNode read GetChild;

    function GetFileNameByLine(var LineIndex: Integer): string;

    property FileName: WideString read GetFileName;
    property FullFileName: string read FFullFileName;

    procedure LoadFromFile(const AFileName: string);

    constructor Create;
    destructor Destroy; override;
  end;

  TNodeManager = class (TInterfacedObject, IFileNodeManager)
  private
    FNodes: TDictionary<string, IFileNode>;
    FTask : ICompileTask;
  public
    function GetFullFileName(const FileName: string): string; stdcall;
    constructor Create(const CompileTask: ICompileTask);
    function GetFileNode(const FileName: string; const requestby: IFileNode): IFileNode; stdcall;
    destructor Destroy; override;
  end;

var
  Global_manager: IFileNodeManager;

procedure CreateFileNodeManager(const CompileTask: ICompileTask);
begin
  if Global_manager = nil then Global_manager := TNodeManager.Create(CompileTask);
end;

function GetFileNodeManager: IFileNodeManager;
begin
  Result := Global_manager;
end;

function IsDirective(const str: string): Boolean;
var i: Integer;
begin
  Result := False;
  for i := 1 to Length(str) do
    begin
      if str[i] = '#' then
      begin
        Result := True;
        Exit;
      end;
      if (str[i] <> ' ') and (str[i] <> #8) then Exit;
    end;
end;

{ TNodeManager }

constructor TNodeManager.Create(const CompileTask: ICompileTask);
begin
    FTask := CompileTask;
end;

destructor TNodeManager.Destroy;
begin
  FreeAndNil(FNodes);
  inherited;
end;

function TNodeManager.GetFileNode(const FileName: string; const requestby: IFileNode): IFileNode;
var fullfilename: string;
    node: TFileNode;
begin
  Result := nil;
  if FileName = '' then Exit;
  if FNodes = nil then FNodes := TDictionary<string, IFileNode>.Create;
  fullfilename := ExpandFileName(FileName);
  if not FNodes.TryGetValue(fullfilename, Result) then
  begin
    node := TFileNode.Create;
    Result := node;
    FNodes.Add(fullfilename, Result);
    node.LoadFromFile(FileName);
  end
  else
  begin
    if assigned(requestby) then
      if not Result.Loaded then
        raise EParseError.Create('Circular references!!!')
      else
        if Result.ConsistFile(requestby.FileName) then
          raise EParseError.Create('Circular references!!!');
  end;
end;

function TNodeManager.GetFullFileName(const FileName: string): string;
begin
    Result := FTask.GetFilePath(FileName);
end;

{ TFileNode }

function TFileNode.ChildCount: Integer;
begin
  Result := FChilds.Count;
end;

function TFileNode.ConsistFile(const AFileName: WideString): Boolean;
var i: Integer;
begin
  Result := False;
  if WideLowerCase(FFileName) = WideLowerCase(AFileName) then
  begin
    Result := True;
    Exit;
  end;

  for i := 0 to FChilds.Count - 1 do
    if FChilds.Items[i].Node.ConsistFile(AFileName) then
    begin
      Result := True;
      Exit;
    end;
end;

constructor TFileNode.Create;
begin
  FFullText := TStringList.Create;
  FRealText := TStringList.Create;
  FChilds := TList<TChildInfo>.Create;
end;

destructor TFileNode.Destroy;
begin
  FreeAndNil(FChilds);
  FreeAndNil(FFullText);
  FreeAndNil(FRealText);
  inherited;
end;

function TFileNode.FullLinesCount: integer;
begin
  Result := FFullText.Count;
end;

function TFileNode.FullText: string;
begin
  Result := FFullText.Text;
end;

function TFileNode.GetChild(index: Integer): IFileNode;
begin
  Result := FChilds.Items[index].Node;
end;

function TFileNode.GetFileName: WideString;
begin
  Result := FFileName;
end;

function TFileNode.GetFileNameByLine(var LineIndex: Integer): string;
var
  i: Integer;
  offset: Integer;
begin
  Result := '';
  offset := 0;
  for i := 0 to FChilds.Count - 1 do
  begin
    if (LineIndex >= FChilds[i].LinePos) and (LineIndex < FChilds[i].LinePos + FChilds[i].Node.FullLinesCount) then
    begin
      LineIndex := LineIndex - FChilds[i].LinePos;
      Result := FChilds[i].Node.GetFileNameByLine(LineIndex);
    end
    else
        Inc(offset, FChilds[i].Node.FullLinesCount-1);
  end;
  if Result = '' then
  begin
      Result := FFileName;
      Dec(LineIndex, offset);
  end;
end;

function TFileNode.GetFullLine(index: integer): string;
begin
  Result := FFullText.Strings[index];
end;

function TFileNode.GetLine(index: integer): string;
begin
  Result := FRealText.Strings[index];
end;

function TFileNode.LinesCount: integer;
begin
  Result := FRealText.Count;
end;

function TFileNode.Loaded: Boolean;
begin
  Result := FLoaded;
end;

procedure TFileNode.LoadFromFile(const AFileName: string);
const INCLUDE = '#include';
var i, j, n: Integer;
    s, subfile, newsubfile: string;
    ch: IFileNode;
    chinfo: TChildInfo;
begin
  FFileName := AFileName;
  FFullFileName := ExpandFileName(AFileName);
  FLoaded := False;

  If 'hlsl.h' = AFileName Then
  Begin
      FLoaded := True;
      Exit;
  End;

  FRealText.LoadFromFile(AFileName);
  for i := 0 to FRealText.Count - 1 do
    begin
      s := FRealText.Strings[i];
      if IsDirective(s) then
      begin
        n := Pos(INCLUDE, s);
        if n > 0 then
        begin
          Delete(s, 1, (n - 1) + Length(INCLUDE));
          n := Pos('"', s);
          if n = 0 then raise EPlacedParseError.Create(FFileName, i + 1, 0, 'include wrong');
          Delete(s, 1, (n - 1) + Length('"'));
          n := Pos('"', s);
          if n = 0 then raise EPlacedParseError.Create(FFileName, i + 1, 0, 'include wrong');
          subfile := Copy(s, 1, n - 1);
          if subfile <> 'hlsl.h' then
          begin
            newsubfile := GetFileNodeManager.GetFullFileName(subfile);
            if newsubfile = '' then raise EPlacedParseError.Create(FFileName, i + 1, 0, Format('include wrong. File %s not found', [subfile]));
            subfile := newsubfile;
          end;
          ch := GetFileNodeManager.GetFileNode(subfile, Self);
          chinfo.LinePos := FFullText.Count;
          chinfo.Node := ch;
          FChilds.Add(chinfo);
          for j := 0 to ch.FullLinesCount - 1 do
            FFullText.Add(ch.FullLines[j]);
          Continue;
        end;
      end;
      FFullText.Add(s);
    end;
  FLoaded := True;
end;

{ EPlacedParseError }

constructor EPlacedParseError.Create(const AFileName: string; ARowNum,
  AColNum: Integer; const Msg: string);
begin
  Message := Msg;
  FFileName := AFileName;
  FRowNum := ARowNum;
  FColNum := AColNum;
end;

end.
