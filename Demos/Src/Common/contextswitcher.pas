unit ContextSwitcher;

{$mode objfpc}{$H+}

interface

uses
  avBase, avRes, avTypes;

type

  { TavContextSwitcher }

  TavContextSwitcher = class (TavObject)
  protected
    FOutHint : Boolean;
    procedure EMKeyDown(var msg: TavKeyDownMessage); message EM_KEYDOWN;
  public
    procedure SwitchContext;
    constructor Create(AParent: TavObject; OutHint : Boolean = True); reintroduce;
  end;

implementation

{ TavContextSwitcher }

procedure TavContextSwitcher.EMKeyDown(var msg: TavKeyDownMessage);
begin
  if msg.shifts <> [sCtrl] then Exit;
  if msg.Key <> Ord('A') then Exit;
  SwitchContext;
end;

procedure TavContextSwitcher.SwitchContext;
var render: TavMainRender;
    targetAPI: T3DAPI;
begin
    if not Assigned(Parent) then Exit;
  render := Parent as TavMainRender;

  targetAPI := apiOGL;
  if render.Inited3D then
  begin
    case render.ActiveApi of
      apiOGL  : targetAPI := apiDX11;
      apiDX11 : targetAPI := apiOGL;
    end;
    render.Free3D;
  end;

  render.Init3D(targetAPI);
  render.InvalidateWindow;
  if FOutHint then
    case targetAPI of
      apiOGL : WriteLn('Current gapi OpenGL');
      apiDX11: WriteLn('Current gapi DirectX11');
    end;
end;

constructor TavContextSwitcher.Create(AParent: TavObject; OutHint: Boolean);
begin
  inherited Create(AParent);
  FOutHint := OutHint;
  if FOutHint then
    WriteLn('press Ctrl + A for switch 3D api');
end;

end.

