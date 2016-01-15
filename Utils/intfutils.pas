unit intfUtils;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils;

type
  IWeakRef_Obj = interface
    procedure _Clear;
    function Obj : TObject;
  end;

  { TWeaklyObject }

  TWeaklyObject = class
  private
    FWeakRef : IWeakRef_Obj;
  public
    function WeakRef : IWeakRef_Obj;
    destructor Destroy; override;
  end;

implementation

type

  { TWeakRef_Obj }

  TWeakRef_Obj = class (TInterfacedObject, IWeakRef_Obj)
  private
    FObj : TObject;
  public
    procedure _Clear;
    function Obj : TObject;
    constructor Create(const AObj : TObject);
  end;

{ TWeakRef_Obj }

procedure TWeakRef_Obj._Clear;
begin
  FObj := nil;
end;

function TWeakRef_Obj.Obj: TObject;
begin
  Result := FObj;
end;

constructor TWeakRef_Obj.Create(const AObj: TObject);
begin
  FObj := AObj;
end;

{ TWeaklyObject }

function TWeaklyObject.WeakRef: IWeakRef_Obj;
begin
  if FWeakRef = nil then
    FWeakRef := TWeakRef_Obj.Create(Self);
  Result := FWeakRef;
end;

destructor TWeaklyObject.Destroy;
begin
  if Assigned(FWeakRef) then
     FWeakRef._Clear;
  inherited Destroy;
end;

end.

