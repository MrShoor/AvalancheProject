unit NewtonIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, NewtonImport, mutils;

type
  INewtonWorld = interface;

  INewtonWorldChild = interface
    function Valid: Boolean;
    function World: INewtonWorld;
  end;

  INewtonCollision = interface (INewtonWorldChild)

  end;

  INewtonWorld = interface
    procedure UpdateWorld(const ADeltaTime: Integer); //ADeltaTime in seconds
  end;

function Create_NewtonWorld(const AWorldSize: TAABB): INewtonWorld;

implementation

type
  TNewtonInterfacedObject = TInterfacedObject;

  { TNWorld }

  TNWorld = class (TNewtonInterfacedObject, INewtonWorld)
  private
    FWorld : PNewtonWorld;
    FChilds: TList;
  public
    function AddChild(const AObject: TObject): Integer;
    procedure DelChild(const AObjectIndex: Integer);

    procedure UpdateWorld(const ADeltaTime: Integer);

    constructor Create(const AWorldSize: TAABB);
    destructor Destroy; override;
  end;

  { TNWorldChild }

  TNWorldChild = class (TNewtonInterfacedObject, INewtonWorldChild)
  private
    FWorld: TNWorld;
    FChildIndex: Integer;
  public
    function Valid: Boolean;
    function World: INewtonWorld;

    constructor Create(const AWorld: TNWorld);
    destructor Destroy; override;
  end;

//  TNewtonCollision = interface

function Create_NewtonWorld(const AWorldSize: TAABB): INewtonWorld;
begin
  Result := TNWorld.Create(AWorldSize);
end;

function NewtonAlloc ( sizeInBytes : int ) : Pointer; cdecl;
begin
  Result := GetMem(sizeInBytes);
end;

procedure NewtonFree ( ptr : Pointer; sizeInBytes : int ); cdecl;
begin
  FreeMem(ptr, sizeInBytes);
end;

{ TNewtonWorldChild }

function TNWorldChild.Valid: Boolean;
begin
  Result := Assigned(FWorld);
end;

function TNWorldChild.World: INewtonWorld;
begin
  Result := FWorld;
end;

constructor TNWorldChild.Create(const AWorld: TNWorld);
begin
  FWorld := AWorld;
end;

destructor TNWorldChild.Destroy;
begin
  inherited Destroy;
end;

{ TNewtonWorld }

function TNWorld.AddChild(const AObject: TObject): Integer;
begin
  Assert(AObject is TNWorldChild, 'AObject not is TNWorldChild');
  Result := FChilds.Count;
  FChilds.Add(AObject);
end;

procedure TNWorld.DelChild(const AObjectIndex: Integer);
var LastIndex: Integer;
begin
  LastIndex := FChilds.Count - 1;
  if AObjectIndex <> LastIndex then
  begin
    TNWorldChild(FChilds[LastIndex]).FChildIndex := AObjectIndex;
    FChilds.Exchange(AObjectIndex, LastIndex);
  end;
  FChilds.Delete(LastIndex);
end;

procedure TNWorld.UpdateWorld(const ADeltaTime: Integer);
begin
  NewtonUpdate(FWorld, ADeltaTime);
end;

constructor TNWorld.Create(const AWorldSize: TAABB);
begin
  FWorld := NewtonCreate(@NewtonAlloc, @NewtonFree);

  // use the standard x87 floating point model
  NewtonSetPlatformArchitecture(FWorld, 0);

  // set a fix world size
  NewtonSetWorldSize(FWorld, @AWorldSize.min.f[0], @AWorldSize.min.f[1]);

	// configure the Newton world to use iterative solve mode 0
	// this is the most efficient but the less accurate mode
  NewtonSetSolverModel(FWorld, 1);
end;

destructor TNWorld.Destroy;
begin
	//// destroy all rigid bodies, this is no necessary because Newton Destroy world will also destroy all bodies
	//// but if you want to change level and restart you can call this function to clean the world without destroying the world.
	//NewtonDestroyAllBodies (g_world);

  NewtonDestroy(FWorld);
  inherited Destroy;
end;

initialization
  NewtonSetMemorySystem(@NewtonAlloc, @NewtonFree);

finalization

end.

