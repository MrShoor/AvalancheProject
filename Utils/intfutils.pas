unit intfUtils;
{$I avUtilsConfig.inc}

interface

uses
  Classes, SysUtils, syncobjs
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
  protected
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : HRes;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  public
    procedure AfterConstruction; override;
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

  { TWeakRef }

  TWeakRef = class (TInterfacedObject, IWeakRef, IWeakRefInternal)
  private
    FObj: TObject;
  public
    function Obj: TObject;
    procedure CleanUp;
    constructor Create(AInstance: TObject);
  end;

  IPublisher = interface
  ['{598D29AC-0EB3-4589-898A-C67069616720}']
    procedure Subscribe  (const ASubscriber: IWeakedInterface);
    procedure UnSubscribe(const ASubscriber: IWeakedInterface);
  end;

  { TPublisherBase }

  TPublisherBase = class (TWeakedInterfacedObject, IPublisher)
  protected type
    TSubsList = array of IWeakedInterface;
  private
    FCS   : TCriticalSection;
    FSubs : array of IWeakRefIntf;
    function IndexOf(const ASubscriber: IWeakedInterface): Integer;
  protected
    function GetSubsList: TSubsList;
  public
    procedure Subscribe  (const ASubscriber: IWeakedInterface); virtual;
    procedure UnSubscribe(const ASubscriber: IWeakedInterface); virtual;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

function WRef(const AObject: TWeakedObject): IWeakRef; overload;
function WRef(const AIntf: IWeakedInterface): IWeakRefIntf; overload;
function DeRef(var ARef: IWeakRef): TObject; overload;
function DeRef(var ARef: IWeakRefIntf): IUnknown; overload;

implementation

type

  { TWeakRefIntf }

  TWeakRefIntf = class (TInterfacedObject, IWeakRefIntf)
  private
    FInstance: TWeakedInterfacedObject;
    FInstanceRefCount: Integer;
  public
    function Intf: IUnknown;
    constructor Create(const AInstance: TWeakedInterfacedObject);
  end;

function WRef(const AObject: TWeakedObject): IWeakRef;
begin
  if AObject = nil then
    Result := nil
  else
    Result := AObject.WeakRef;
end;

function WRef(const AIntf: IWeakedInterface): IWeakRefIntf;
begin
  if AIntf = nil then
    Result := nil
  else
    Result := AIntf.WeakRef;
end;

function DeRef(var ARef: IWeakRef): TObject;
begin
  if ARef = nil then Exit(nil);
  Result := ARef.Obj;
  if Result = nil then ARef := nil;
end;

function DeRef(var ARef: IWeakRefIntf): IUnknown;
begin
  if ARef = nil then Exit(nil);
  Result := ARef.Intf;
  if Result = nil then ARef := nil;
end;

{ TPublisherBase }

function TPublisherBase.IndexOf(const ASubscriber: IWeakedInterface): Integer;
var
  i: Integer;
begin
  FCS.Enter;
  try
    Result := -1;
    for i := 0 to Length(FSubs) - 1 do
      if FSubs[i] = ASubscriber.WeakRef then Exit(i);
  finally
    FCS.Leave;
  end;
end;

function TPublisherBase.GetSubsList: TSubsList;
var i, j : integer;
    intf : IUnknown;
begin
  FCS.Enter;
  try
    j := 0;
    SetLength(Result, Length(FSubs));
    for i := 0 to Length(FSubs) - 1 do
    begin
      intf := FSubs[i].Intf;
      if intf <> nil then
      begin
        Result[j] := intf as IWeakedInterface;
        if j < i then
          FSubs[j] := FSubs[i];
        Inc(j);
      end
    end;

    if j < Length(FSubs) then
    begin
      SetLength(FSubs, j);
      SetLength(Result, j);
    end;
  finally
    FCS.Leave;
  end;
end;

procedure TPublisherBase.Subscribe(const ASubscriber: IWeakedInterface);
begin
  FCS.Enter;
  try
    Assert(IndexOf(ASubscriber) < 0);
    SetLength(FSubs, Length(FSubs) + 1);
    FSubs[High(FSubs)] := ASubscriber.WeakRef;
  finally
    FCS.Leave;
  end;
end;

procedure TPublisherBase.UnSubscribe(const ASubscriber: IWeakedInterface);
var n, last: Integer;
begin
  n := IndexOf(ASubscriber);
  if n < 0 then Exit;
  last := High(FSubs);
  if n <> last then FSubs[n] := FSubs[last];
  FSubs[last] := nil;
  SetLength(FSubs, last);
end;

procedure TPublisherBase.AfterConstruction;
begin
  inherited AfterConstruction;
  FCS := TCriticalSection.Create;
end;

procedure TPublisherBase.BeforeDestruction;
begin
  FreeAndNil(FCS);
  inherited BeforeDestruction;
end;

{ TWeakRefIntf }

function TWeakRefIntf.Intf: IUnknown;
begin
  Result := FInstance;
end;

constructor TWeakRefIntf.Create(const AInstance: TWeakedInterfacedObject);
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

function TWeakedInterfacedObject._AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedIncrement(TWeakRefIntf(FWeakRef).FInstanceRefCount);
end;

function TWeakedInterfacedObject._Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterLockedDecrement(TWeakRefIntf(FWeakRef).FInstanceRefCount);
  if Result = 0 then
  begin
    TWeakRefIntf(FWeakRef).FInstance := nil;
    Destroy;
  end;
end;

procedure TWeakedInterfacedObject.AfterConstruction;
begin
  inherited AfterConstruction;
  Dec( TWeakRefIntf(FWeakRef).FInstanceRefCount );
end;

function TWeakedInterfacedObject.WeakRef: IWeakRefIntf;
begin
  Result := TWeakRefIntf(FWeakRef);
end;

class function TWeakedInterfacedObject.NewInstance: TObject;
begin
  Result := inherited newinstance;
  TWeakedInterfacedObject(Result).FWeakRef := TWeakRefIntf.Create(TWeakedInterfacedObject(Result));
  TWeakRefIntf(TWeakedInterfacedObject(Result).FWeakRef)._AddRef;
  TWeakRefIntf(TWeakedInterfacedObject(Result).FWeakRef).FInstanceRefCount := 1;
end;

destructor TWeakedInterfacedObject.Destroy;
begin
  inherited Destroy;
  TWeakRefIntf(FWeakRef)._Release;
end;

end.

