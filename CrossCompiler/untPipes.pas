unit untPipes;

interface

uses
  Windows, Classes, SysUtils;

type
  TPipeMode = (pmClient, pmCreateDuplex, pmCreateInbound, pmCreateOutbound);

  TPipeServer = class (TComponent)
  private
//    FHandle:
    function GetHandle: Cardinal;
  public                                  //CreateNamedPipe
    procedure OpenPipe(name: string; mode: TPipeMode);
    procedure ClosePipe;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ TPipeServer }

procedure TPipeServer.ClosePipe;
begin

end;

constructor TPipeServer.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TPipeServer.Destroy;
begin

  inherited;
end;

function TPipeServer.GetHandle: Cardinal;
begin
//  CreateNamedPipe('\\.\pipe\cGLSL', PIPE_ACCESS_DUPLEX,
end;

procedure TPipeServer.OpenPipe(name: string; mode: TPipeMode);
var pipename: string;
    openmode: Cardinal;
begin
  pipename := '\\.\pipe\' + name;

  if mode = pmClient then
  begin
    Exit;
  end;

  openmode := 0;
  case mode of
    pmCreateDuplex: openmode := PIPE_ACCESS_DUPLEX;
    pmCreateInbound: openmode := PIPE_ACCESS_INBOUND;
    pmCreateOutbound: openmode := PIPE_ACCESS_OUTBOUND;
  else
    Assert(false, 'wrong mode value');
  end;
  CreateNamedPipe(PChar(pipename), openmode,
end;

end.
