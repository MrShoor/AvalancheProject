unit NewtonIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, NewtonImport, mutils;

type
  INewtonWorld = interface;
  INewtonBody = interface;

  TOnApplyForceAndTorque = procedure (const Sender: INewtonBody; timestep : Single; threadIndex : Integer) of object;
  TOnTransformCallback = procedure (const Sender: INewtonBody; const AMatrix : TMat4; threadIndex : Integer) of object;

  { INewtonWorldChild }

  INewtonWorldChild = interface
    function Valid: Boolean;
    function World: INewtonWorld;
    function NewtonObject: Pointer;
  end;

  { INewtonCollision }

  INewtonCollision = interface (INewtonWorldChild)
  end;

  { INewtonBody }

  INewtonBody = interface (INewtonWorldChild)
    function GetMatrix: TMat4;
    function GetOnApplyForce: TOnApplyForceAndTorque;
    function GetOnTransform: TOnTransformCallback;
    procedure SetMatrix(const AValue: TMat4);
    procedure SetOnApplyForce(const AValue: TOnApplyForceAndTorque);
    procedure SetOnTransform(const AValue: TOnTransformCallback);

    procedure CalcDefaultInertia(const AMass: Single);
    procedure GetMassMatrix(out AMass: Single; out AInert: TVec3);
    procedure SetForce(const AForce: TVec3);

    property Matrix: TMat4 read GetMatrix write SetMatrix;

    property OnApplyForce: TOnApplyForceAndTorque read GetOnApplyForce write SetOnApplyForce;
    property OnTransform: TOnTransformCallback read GetOnTransform write SetOnTransform;
  end;

  { INewtonWorld }

  INewtonWorld = interface
    function GetOnDefaultApplyForce: TOnApplyForceAndTorque;
    function GetOnDefaultTransform: TOnTransformCallback;
    procedure SetOnDefaultApplyForce(const AValue: TOnApplyForceAndTorque);
    procedure SetOnDefaultTransform(const AValue: TOnTransformCallback);

    function CreateBox(const ASize: TVec3; const ATransform: TMat4): INewtonCollision;
    function CreateBody(const ACollision: INewtonCollision; const ATransform: TMat4): INewtonBody;

    procedure SetWorldSize(const AValue: TAABB);
    procedure UpdateWorld(const ADeltaTime: Single); //ADeltaTime in seconds

    property OnDefaultApplyForce: TOnApplyForceAndTorque read GetOnDefaultApplyForce write SetOnDefaultApplyForce;
    property OnDefaultTransform: TOnTransformCallback read GetOnDefaultTransform write SetOnDefaultTransform;
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

    FOnDefaultApplyForce: TOnApplyForceAndTorque;
    FOnDefaultTransform : TOnTransformCallback;

    procedure CleanUpChilds;
    function AddChild(const AObject: TObject): Integer;
    procedure DelChild(const AObjectIndex: Integer);
  public
    function GetOnDefaultApplyForce: TOnApplyForceAndTorque;
    function GetOnDefaultTransform: TOnTransformCallback;
    procedure SetOnDefaultApplyForce(const AValue: TOnApplyForceAndTorque);
    procedure SetOnDefaultTransform(const AValue: TOnTransformCallback);

    function CreateBox(const ASize: TVec3; const ATransform: TMat4): INewtonCollision;
    function CreateBody(const ACollision: INewtonCollision; const ATransform: TMat4): INewtonBody;

    procedure SetWorldSize(const AValue: TAABB);
    procedure UpdateWorld(const ADeltaTime: Single);
  public
    constructor Create(const AWorldSize: TAABB);
    destructor Destroy; override;
  end;

  { TNWorldChild }

  TNWorldChild = class (TNewtonInterfacedObject, INewtonWorldChild)
  private
    FWorld: TNWorld;
    FChildIndex: Integer;
  protected
    FObject: Pointer;
  public
    procedure CleanLinks; virtual;

    function Valid: Boolean;
    function World: INewtonWorld;
    function NewtonObject: Pointer;

    constructor Create(const AWorld: TNWorld; const AObject: Pointer);
    destructor Destroy; override;
  end;

  { TNCollision }

  TNCollision = class (TNWorldChild, INewtonCollision)
  public
    procedure CleanLinks; override;
  end;

  { TNBody }

  TNBody = class (TNWorldChild, INewtonBody)
  public
    FOnApplyForce: TOnApplyForceAndTorque;
    FOnTransform : TOnTransformCallback;

    function GetMatrix: TMat4;
    function GetOnApplyForce: TOnApplyForceAndTorque;
    function GetOnTransform: TOnTransformCallback;
    procedure SetMatrix(const AValue: TMat4);
    procedure SetOnApplyForce(const AValue: TOnApplyForceAndTorque);
    procedure SetOnTransform(const AValue: TOnTransformCallback);

    procedure CalcDefaultInertia(const AMass: Single);
    procedure GetMassMatrix(out AMass: Single; out AInert: TVec3);
    procedure SetForce(const AForce: TVec3);

    property Matrix: TMat4 read GetMatrix write SetMatrix;
  public
    procedure DoApplyForceAndTorqueCallback(const timestep : Single; threadIndex : Integer);
    procedure DoTransformCallback(const AMatrix : TMat4; threadIndex : Integer);

    procedure CleanLinks; override;
    procedure AfterConstruction; override;
  end;

procedure BodyApplyForceAndTorqueCallback ( const body : PNewtonBody; timestep : Float; threadIndex : int ); cdecl;
begin
  TNBody(NewtonBodyGetUserData(body)).DoApplyForceAndTorqueCallback(timestep, threadIndex);
end;

procedure BodyTransformCallback ( const body : PNewtonBody; const matrix : PFloat; threadIndex : int ); cdecl;
begin
  TNBody(NewtonBodyGetUserData(body)).DoTransformCallback(PMat4(matrix)^, threadIndex);
end;

procedure BodyDestroyCallback ( const body : PNewtonBody ); cdecl;
begin
  //nothing do here
end;

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

{ TNBody }

function TNBody.GetMatrix: TMat4;
begin
  if FObject = nil then
    Result := IdentityMat4
  else
    NewtonBodyGetMatrix(FObject, @Result);
end;

function TNBody.GetOnApplyForce: TOnApplyForceAndTorque;
begin
  Result := FOnApplyForce;
end;

function TNBody.GetOnTransform: TOnTransformCallback;
begin
  Result := FOnTransform;
end;

procedure TNBody.SetMatrix(const AValue: TMat4);
begin
  if FObject = nil then Exit;
  NewtonBodySetMatrix(FObject, @AValue);
end;

procedure TNBody.SetOnApplyForce(const AValue: TOnApplyForceAndTorque);
begin
  FOnApplyForce := AValue;
end;

procedure TNBody.SetOnTransform(const AValue: TOnTransformCallback);
begin
  FOnTransform := AValue;
end;

procedure TNBody.CalcDefaultInertia(const AMass: Single);
var inertia: TVec3;
    origin: TVec3;
begin
  if FObject = nil then Exit;
  NewtonConvexCollisionCalculateInertialMatrix(NewtonBodyGetCollision(FObject), @inertia, @origin);
  NewtonBodySetMassMatrix(FObject, AMass, AMass * inertia.x, AMass * inertia.y, AMass * inertia.z);
  NewtonBodySetCentreOfMass(FObject, @origin);
end;

procedure TNBody.GetMassMatrix(out AMass: Single; out AInert: TVec3);
begin
  if FObject = nil then
  begin
    AMass := 0;
    AInert := Vec(0,0,0);
  end
  else
    NewtonBodyGetMassMatrix(FObject, @AMass, @AInert.x, @AInert.y, @AInert.z);
end;

procedure TNBody.SetForce(const AForce: TVec3);
begin
  if Assigned(FObject) then
    NewtonBodySetForce(FObject, @AForce);
end;

procedure TNBody.DoApplyForceAndTorqueCallback(const timestep: Single; threadIndex: Integer);
begin
  if Assigned(FOnApplyForce) then
    FOnApplyForce(Self, timestep, threadIndex)
  else
    if Assigned(FWorld.FOnDefaultApplyForce) then
      FWorld.FOnDefaultApplyForce(Self, timestep, threadIndex)
end;

procedure TNBody.DoTransformCallback(const AMatrix: TMat4; threadIndex: Integer);
begin
  if Assigned(FOnTransform) then
    FOnTransform(Self, AMatrix, threadIndex)
  else
    if Assigned(FWorld.FOnDefaultTransform) then
      FWorld.FOnDefaultTransform(Self, AMatrix, threadIndex);
end;

procedure TNBody.CleanLinks;
begin
  NewtonDestroyBody(FWorld.FWorld, FObject);
  inherited CleanLinks;
end;

procedure TNBody.AfterConstruction;
begin
  inherited AfterConstruction;
  NewtonBodySetUserData(FObject, Pointer(Self));
  NewtonBodySetForceAndTorqueCallback(FObject, @BodyApplyForceAndTorqueCallback);
  NewtonBodySetTransformCallback(FObject, @BodyTransformCallback);
  NewtonBodySetDestructorCallback(FObject, @BodyDestroyCallback);
end;

{ TNCollision }

procedure TNCollision.CleanLinks;
begin
  NewtonReleaseCollision(FWorld.FWorld, FObject);
  inherited CleanLinks;
end;

{ TNewtonWorldChild }

procedure TNWorldChild.CleanLinks;
begin
  FObject := nil;
  FWorld := nil;
end;

function TNWorldChild.NewtonObject: Pointer;
begin
  Result := FObject;
end;

function TNWorldChild.Valid: Boolean;
begin
  Result := Assigned(FWorld);
end;

function TNWorldChild.World: INewtonWorld;
begin
  Result := FWorld;
end;

constructor TNWorldChild.Create(const AWorld: TNWorld; const AObject: Pointer);
begin
  FWorld := AWorld;
  FChildIndex := FWorld.AddChild(Self);
  FObject := AObject;
end;

destructor TNWorldChild.Destroy;
begin
  inherited Destroy;
  if Assigned(FWorld) then
  begin
    FWorld.DelChild(FChildIndex);
    CleanLinks;
  end;
end;

{ TNewtonWorld }

procedure TNWorld.CleanUpChilds;
var
  i: Integer;
begin
  for i := 0 to FChilds.Count - 1 do
    TNWorldChild(FChilds[i]).CleanLinks;
  FChilds.Clear;
end;

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
    FChilds[AObjectIndex] := FChilds[LastIndex];
    TNWorldChild(FChilds[AObjectIndex]).FChildIndex := AObjectIndex;
  end;
  FChilds.Delete(LastIndex);
end;

function TNWorld.GetOnDefaultApplyForce: TOnApplyForceAndTorque;
begin
  Result := FOnDefaultApplyForce;
end;

function TNWorld.GetOnDefaultTransform: TOnTransformCallback;
begin
  Result := FOnDefaultTransform;
end;

procedure TNWorld.SetOnDefaultApplyForce(const AValue: TOnApplyForceAndTorque);
begin
  FOnDefaultApplyForce := AValue;
end;

procedure TNWorld.SetOnDefaultTransform(const AValue: TOnTransformCallback);
begin
  FOnDefaultTransform := AValue;
end;

procedure TNWorld.SetWorldSize(const AValue: TAABB);
begin
  NewtonSetWorldSize(FWorld, @AValue.min, @AValue.max);
end;

function TNWorld.CreateBox(const ASize: TVec3; const ATransform: TMat4): INewtonCollision;
begin
  Result := TNCollision.Create(Self, NewtonCreateBox(FWorld, ASize.x, ASize.y, ASize.z, 0, @ATransform));
end;

function TNWorld.CreateBody(const ACollision: INewtonCollision; const ATransform: TMat4): INewtonBody;
begin
  Result := TNBody.Create(Self, NewtonCreateBody(FWorld, ACollision.NewtonObject, @ATransform));
end;

procedure TNWorld.UpdateWorld(const ADeltaTime: Single);
begin
  NewtonUpdate(FWorld, ADeltaTime);
end;

constructor TNWorld.Create(const AWorldSize: TAABB);
begin
  FChilds := TList.Create;

  FWorld := NewtonCreate(@NewtonAlloc, @NewtonFree);

  // use the standard x87 floating point model
  NewtonSetPlatformArchitecture(FWorld, 0);

  // set a fix world size
  SetWorldSize(AWorldSize);

	// configure the Newton world to use iterative solve mode 0
	// this is the most efficient but the less accurate mode
  NewtonSetSolverModel(FWorld, 1);
end;

destructor TNWorld.Destroy;
begin
	//// destroy all rigid bodies, this is no necessary because Newton Destroy world will also destroy all bodies
	//// but if you want to change level and restart you can call this function to clean the world without destroying the world.
	//NewtonDestroyAllBodies (g_world);
  CleanUpChilds;

  FreeAndNil(FChilds);
  NewtonDestroy(FWorld);
  inherited Destroy;
end;

initialization
  NewtonSetMemorySystem(@NewtonAlloc, @NewtonFree);

finalization

end.

