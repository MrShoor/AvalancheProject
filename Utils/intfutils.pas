unit intfUtils;
{$I avConfig.inc}

interface

uses
  Classes, SysUtils
  {$IfDef DCC}
    {$IfDef Windows}
    ,Windows
    {$EndIf}
  {$EndIf};

type
{$IfDef FPC}
  HRes = longint;
{$Else}
  HRes = HRESULT;
{$EndIf}

  { IWeakRef }

  IWeakRef = interface(IUnknown)
    function Obj: TObject;
  end;

  { IWeakRefIntf }

  IWeakRefIntf = interface(IUnknown)
    function Intf: IUnknown;
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

  IWeakedInterface = interface
  ['{08E0422B-0726-444B-90AA-BDF4D85D5668}']
    function WeakRef: IWeakRefIntf;
  end;

  { TWeakedInterfacedObject }

  TWeakedInterfacedObject = class (TObject, IUnknown, IWeakedInterface)
  private
    FWeakRef: TObject;
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    function WeakRef: IWeakRefIntf;
    class function NewInstance : TObject; override;
    destructor Destroy; override;
  end;

  { TNoRefObject }

  TNoRefObject = class (TObject)
  public
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
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

  { TWeakRefIntf }

  TWeakRefIntf = class (TInterfacedObject, IWeakRefIntf)
  private
    FInstance: IUnknown;
    FInstanceRefCount: Integer;
  public
    function Intf: IUnknown;
    constructor Create(const AInstance: IUnknown);
  end;

{ TWeakRefIntf }

function TWeakRefIntf.Intf: IUnknown;
begin
  Result := FInstance;
end;

constructor TWeakRefIntf.Create(const AInstance: IUnknown);
begin
  FInstance := AInstance;
end;

{ TNoRefObject }

function TNoRefObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getinterface(iid,obj) then
    result:=S_OK
  else
    result:=longint(E_NOINTERFACE);
end;

function TNoRefObject._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
end;

function TNoRefObject._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := -1;
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

function TWeakedInterfacedObject.QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getinterface(iid,obj) then
    result:=S_OK
  else
    result:=longint(E_NOINTERFACE);
end;

function TWeakedInterfacedObject._AddRef: longint; stdcall;
begin
  Result := InterLockedIncrement(TWeakRefIntf(FWeakRef).FInstanceRefCount);
end;

function TWeakedInterfacedObject._Release: longint; stdcall;
begin
  Result := InterLockedDecrement(TWeakRefIntf(FWeakRef).FInstanceRefCount);
  if Result = 0 then
    Destroy
  else
    if Result = 1 then
      TWeakRefIntf(FWeakRef).FInstance := nil;
end;

function TWeakedInterfacedObject.WeakRef: IWeakRefIntf;
begin
  Result := TWeakRefIntf(FWeakRef);
end;

class function TWeakedInterfacedObject.NewInstance: Tobject;
begin
  Result := inherited newinstance;
  TWeakedInterfacedObject(Result).FWeakRef := TWeakRefIntf.Create(TWeakedInterfacedObject(Result));
  TWeakRefIntf(TWeakedInterfacedObject(Result).FWeakRef)._AddRef;
end;

destructor TWeakedInterfacedObject.Destroy;
begin
  inherited Destroy;
  TWeakRefIntf(FWeakRef)._Release;
end;

end.

