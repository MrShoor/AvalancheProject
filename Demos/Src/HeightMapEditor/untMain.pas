unit untMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, untHMEFrame, avTexLoader, avTypes;

type
  TfrmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private
    FHMFrame : TfrmHMEditor;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$IfnDef FPC}
    {$R *.dfm}
{$Else}
    {$R *.lfm}
{$EndIf}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FHMFrame := TfrmHMEditor.Create(Self);
  FHMFrame.ControlState := FHMFrame.ControlState + [csCustomPaint];
  FHMFrame.Align := alClient;
  FHMFrame.Visible := True;
  FHMFrame.Parent := Self;
end;

procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FHMFrame.FrameMouseWheel(Sender, Shift, WheelDelta, MousePos, Handled);
end;

end.
