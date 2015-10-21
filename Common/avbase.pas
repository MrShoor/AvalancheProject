unit avBase;

{$mode objfpc}{$H+}
{$DEFINE AssertOnRegFailed}

interface

uses
  Classes, SysUtils,
  avContnrs;

type
  EavError = class (EAbort);

  TavObject = class;
  TavObjectClass = class of TavObject;
  TavObjectArr = array of TavObject;

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

  { TavObject }

  TavObject = class (TWeakedObject)
  private
    FObjects: TList;
    FParent : TavObject;

    FName : string;

    function GetChild(index: integer): TavObject;

    procedure UnRegister;
    procedure RegisterTo(obj: TavObject);

    procedure SetParent(const Value: TavObject);
  protected
    FUpdateID: Integer;

    procedure BeforeRegister; virtual;
    function  QueryRegister(AChild: TavObject): boolean; virtual; //return TRUE if child can to be register at self
    function  CanRegister(target: TavObject): boolean; virtual;  //return TRUE if self allow register at target
    procedure AfterRegister; virtual;
  public
    property UpdateID: Integer read FUpdateID;
    property Name: string read FName write FName;

    property  Parent: TavObject read FParent write SetParent;

    function  ChildCount: integer; overload;
    function  ChildCount(aClass: TavObjectClass): integer; overload;
    property  Child[index: integer]: TavObject read GetChild;
    function  ChildArray(const aClass: TavObjectClass): TavObjectArr;
    procedure ChildMove(oldindex, newindex: integer);
    procedure ChildSwap(index1, index2: integer);
    function  ChildIndex(AChild: TavObject): integer;
    function  ChildExists(obj: TavObject): boolean;
    function  ChildExistsRecursive(obj: TavObject): boolean;
    function  FindChild(const AName: string): TavObject; overload;
    function  FindChild(const AName: string; objClass: TavObjectClass): TavObject; overload;
    function  FindChildRecursive(const AName: string): TavObject; overload;
    function  FindChildRecursive(const AName: string; objClass: TavObjectClass): TavObject; overload;
    function  FindAtParents(objClass: TavObjectClass): TavObject;

    function  RootObject: TavObject;

    procedure Broadcast(var Message);
    procedure BroadcastRecursive(var Message);

    constructor Create(AParent: TavObject); virtual;

    destructor Destroy; override;
  end;

implementation

uses
  avLog;

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

{ TavObject }

procedure TavObject.SetParent(const Value: TavObject);
begin
  if Parent <> Value then
  begin
    if not CanRegister(Value) then
    begin
      {$ifdef AssertOnRegFailed}
      Assert(False, 'error('+FName+'): can''t register at this parent('+Value.Name+')');
      {$endif}
      LogLn('error('+FName+'): can''t register at this parent('+Value.Name+')');
      exit;
    end;
    if assigned(Value) then
      if not Value.QueryRegister(self) then
      begin
        {$ifdef AssertOnRegFailed}
        Assert(False, 'error('+FName+'): parent not allow register');
        {$endif}
        LogLn('error('+FName+'): parent not allow register');
        exit;
      end;
    BeforeRegister;
    UnRegister;
    RegisterTo(Value);
    AfterRegister;
  end;
end;

procedure TavObject.BeforeRegister;
begin

end;

function TavObject.GetChild(index: integer): TavObject;
begin
  Result:=nil;
  if (index>=0) and (index<FObjects.Count) then  Result:=TavObject(FObjects.Items[index]);
end;

procedure TavObject.UnRegister;
begin
  if assigned(FParent) then
    FParent.FObjects.Delete(FParent.FObjects.IndexOf(self));
end;

procedure TavObject.RegisterTo(obj: TavObject);
begin
  if assigned(obj) then
    obj.FObjects.Add(self);
  FParent:=obj;
end;

function TavObject.QueryRegister(AChild: TavObject): boolean;
begin
  Result:=Assigned(AChild);
end;

function TavObject.CanRegister(target: TavObject): boolean;
begin
  Result:=not ChildExists(target);
end;

procedure TavObject.AfterRegister;
begin

end;

function TavObject.FindAtParents(objClass: TavObjectClass): TavObject;
var obj: TavObject;
begin
  Result := nil;
  obj := Self;
  while Assigned(obj) do
    if obj is objClass then
      Exit(obj)
    else
      obj := obj.Parent;
end;

function TavObject.ChildCount: integer;
begin
  Result := FObjects.Count;
end;

function TavObject.ChildCount(aClass: TavObjectClass): integer;
var i: integer;
begin
  Result:=0;
  for i := 0 to ChildCount - 1 do
    if Child[i] is aClass then inc(Result);
end;

function TavObject.ChildArray(const aClass: TavObjectClass): TavObjectArr;
var n, i: Integer;
begin
  SetLength(Result, ChildCount(aClass));
  if Length(Result)>0 then
    FillChar(Result[0], SizeOf(Result[0])*Length(Result), 0);
  n:=0;
  for i := 0 to ChildCount - 1 do
    if Child[i] is aClass then
    begin
      Result[n]:=Child[i];
      inc(n);
    end;
end;

procedure TavObject.ChildMove(oldindex, newindex: integer);
begin
  FObjects.Move(newindex, oldindex);
end;

procedure TavObject.ChildSwap(index1, index2: integer);
var p: Pointer;
begin
  p:=FObjects.Items[index1];
  FObjects.Items[index1]:=FObjects.Items[index2];
  FObjects.Items[index2]:=p;
end;

function TavObject.ChildIndex(AChild: TavObject): integer;
begin
  Result:=FObjects.IndexOf(AChild);
end;

function TavObject.ChildExists(obj: TavObject): boolean;
begin
  Result := FObjects.IndexOf(obj) >= 0;
end;

function TavObject.ChildExistsRecursive(obj: TavObject): boolean;
var i: integer;
begin
  Result := FObjects.IndexOf(obj) >= 0;
  if not Result then
  begin
    for i := 0 to ChildCount - 1 do
    begin
      Result := Child[i].ChildExistsRecursive(obj);
      if Result then Exit;
    end;
  end;
end;

function TavObject.FindChild(const AName: string): TavObject;
var i: Integer;
begin
  Result := nil;
  for i := 0 to ChildCount - 1 do
    if Child[i].Name = AName then Exit(Child[i]);
end;

function TavObject.FindChild(const AName: string; objClass: TavObjectClass): TavObject;
var i: Integer;
begin
  Result := nil;
  for i := 0 to ChildCount - 1 do
    if (Child[i] is objClass) and (Child[i].Name = AName) then Exit(Child[i]);
end;

function TavObject.FindChildRecursive(const AName: string): TavObject;
var i: Integer;
begin
  Result := FindChild(AName);
  if Result = nil then
  begin
    for i := 0 to ChildCount - 1 do
    begin
      Result := Child[i].FindChildRecursive(AName);
      if Assigned(Result) then Exit;
    end;
  end;
end;

function TavObject.FindChildRecursive(const AName: string; objClass: TavObjectClass): TavObject;
var i: Integer;
begin
  Result := FindChild(AName, objClass);
  if Result = nil then
  begin
    for i := 0 to ChildCount - 1 do
    begin
      Result := Child[i].FindChildRecursive(AName, objClass);
      if Assigned(Result) then Exit;
    end;
  end;
end;

function TavObject.RootObject: TavObject;
begin
  Result := Parent;
  if Result = nil then Exit(Self);
  while Result.Parent <> nil do
    Result := Result.Parent;
end;

procedure TavObject.Broadcast(var Message);
var i: integer;
begin
  for i := 0 to FObjects.Count - 1 do TavObject(FObjects.Items[i]).Dispatch(Message);
end;

procedure TavObject.BroadcastRecursive(var Message);
var i: Integer;
begin
  Broadcast(Message);
  for i := 0 to FObjects.Count - 1 do
    TavObject(FObjects[i]).BroadcastRecursive(Message);
end;

constructor TavObject.Create(AParent: TavObject);
begin
  FObjects := TList.Create;
  Name := ClassName;
  Parent := AParent;
end;

destructor TavObject.Destroy;
begin
  inherited Destroy;
  while FObjects.Count>0 do
    TavObject(FObjects.Items[FObjects.Count-1]).Free;

  UnRegister;

  FreeAndNil(FObjects);
end;

end.

