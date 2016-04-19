unit intfUtils;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

interface

uses
  Classes, SysUtils;

type
  { IWeakRef }

  IWeakRef = interface(IUnknown)
    function Obj: TObject;
  end;

  { IWeakRefInternal }

  IWeakRefInternal = interface(IWeakRef)
    procedure CleanUp;
  end;

  { TWeakedObject }

  TWeakedObject = class (TObject)
  private
    FWeakRef: IWeakRefInternal;
  public
    function WeakRef: IWeakRef;
    destructor Destroy; override;
  end;

  IWeakedObject = interface
  ['{08E0422B-0726-444B-90AA-BDF4D85D5668}']
    function WeakRef: IWeakRef;
  end;

  { TWeakedInterfacedObject }

  TWeakedInterfacedObject = class (TInterfacedObject, IWeakedObject)
  private
    FWeakRef: IWeakRefInternal;
  public
    function WeakRef: IWeakRef;
    destructor Destroy; override;
  end;

implementation

type
  { TWeakRef }

  TWeakRef = class (TInterfacedObject, IWeakRef, IWeakRefInternal)
  private
    FObj: TObject;
  public
    function Obj: TObject;
    procedure CleanUp;
    constructor Create(AInstance: TObject);
  end;

{ TWeakRef }

function TWeakRef.Obj: TObject;
begin
  Result := FObj;
end;

procedure TWeakRef.CleanUp;
begin
  FObj := nil;
end;

constructor TWeakRef.Create(AInstance: TObject);
begin
  FObj := AInstance;
end;

{ TWeakedObject }

function TWeakedObject.WeakRef: IWeakRef;
begin
  if FWeakRef = nil then
    FWeakRef := TWeakRef.Create(Self);
  Result := FWeakRef;
end;

destructor TWeakedObject.Destroy;
begin
  if Assigned(FWeakRef) then
    FWeakRef.CleanUp;
  inherited Destroy;
end;

{ TWeakedInterfacedObject }

function TWeakedInterfacedObject.WeakRef: IWeakRef;
begin
  if FWeakRef = nil then
    FWeakRef := TWeakRef.Create(Self);
  Result := FWeakRef;
end;

destructor TWeakedInterfacedObject.Destroy;
begin
  if Assigned(FWeakRef) then
    FWeakRef.CleanUp;
  inherited Destroy;
end;

end.

