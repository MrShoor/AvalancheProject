unit HMEBrush;

{$IfDef FPC}
  {$mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$EndIf}

interface

uses
  mutils,
  avRes, avBase, avTypes;

type

  { TavHMEBrush }

  TavHMEBrush = class (TavMainRenderChild)
  private
    FRadius: Single;
  public
    property Radius: Single read FRadius write FRadius;
    procedure AfterConstruction; override;
  end;

implementation

{ TavHMEBrush }

procedure TavHMEBrush.AfterConstruction;
begin
  inherited AfterConstruction;
  FRadius := 20;
end;

end.
