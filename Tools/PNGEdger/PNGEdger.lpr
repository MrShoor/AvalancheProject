program PNGEdger;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, vampyreimagingpackage, CustApp, EdgeBuilder, FileUtil
  { you can add units after this };

type

  { TMyApplication }

  TMyApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMyApplication }

procedure TMyApplication.DoRun;
var
  InFile : String;
  OutFile: String;
  Tolerance: Integer;
begin
  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if ParamCount = 0 then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not FileExists(ParamStr(1)) then
  begin
    WriteLn('File "'+ParamStr(1)+'" not found.');
    Terminate;
    Exit;
  end;
  InFile := ParamStr(1);

  if HasOption('o') then
    OutFile := GetOptionValue('o')
  else
    OutFile := ExtractFileNameWithoutExt(InFile) + '_noedges.png';

  if HasOption('t') then
    Tolerance := StrToIntDef(GetOptionValue('t'), 95)
  else
    Tolerance := 95;

  CreatePngEdges(InFile, OutFile, Tolerance);
  WriteLn('Done!');

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMyApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: PNGEdger <input png file> <parameters>');
  writeln('parameters list:');
  writeln('  -o <filename> - Output filename');
  writeln('  -t N - N is alpha tolerance [0,100]. Default value = 95');
end;

var
  Application: TMyApplication;
begin
  Application:=TMyApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.

