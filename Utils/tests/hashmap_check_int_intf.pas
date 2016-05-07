unit hashmap_Check_Int_Intf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, avContnrs;

type

  { TMyInterfacedObject }

  TMyInterfacedObject = class(TObject,IUnknown)
  protected
     FRefCount : longint;
     function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
     function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
     function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
  end;

  { TCheck_Str_Intf }

  TCheck_Str_Intf = class(TTestCase)
  published
    procedure Test;
    procedure CheckInterfaceCleaned;
  end;

implementation

var gvCounter: Integer;

{ TMyInterfacedObject }

function TMyInterfacedObject.QueryInterface(constref iid: tguid; out obj): longint; stdcall;
begin
  if GetInterface(iid,obj) then
    Result:=S_OK
  else
    Result:=LongInt(E_NOINTERFACE);
end;

function TMyInterfacedObject._AddRef: longint; stdcall;
begin
  InterLockedIncrement(gvCounter);
  Result := InterLockedIncrement(FRefCount);
end;

function TMyInterfacedObject._Release: longint; stdcall;
begin
  InterLockedDecrement(gvCounter);
  Result := InterLockedDecrement(FRefCount);
  if Result = 0 then
    Self.Destroy;
end;

procedure TCheck_Str_Intf.Test;
const FILL_COUNT = 10000;
type
  IHash = specialize IHashMap<String, IUnknown>;
  THash = specialize THashMap<String, IUnknown>;
var hash: IHash;
    i: Integer;
    key: string;

    counter: Integer;
begin
  hash := THash.Create;
  for i := 0 to FILL_COUNT - 1 do
    hash.Add(IntToStr(i), TMyInterfacedObject.Create);

  Check(hash.Count = FILL_COUNT, 'Incorrect count value');

  for i := 0 to FILL_COUNT - 1 do
    Check(Assigned(hash[IntToStr(i)]), 'Incorrect filling');

  counter := hash.Count;
  for i := 0 to FILL_COUNT - 1 do
  begin
    key := IntToStr(Random(FILL_COUNT));
    if hash.Contains(key) then
    begin
      hash.Delete(key);
      Dec(counter);
      Check(not hash.Contains(key), 'Contains after delete?');
    end;
  end;
  Check(hash.Count = counter, 'Incorrect count value after delete');

  hash.Clear;
  Check(hash.Count = 0, 'Incorrect count value after clear');
end;

procedure TCheck_Str_Intf.CheckInterfaceCleaned;
begin
  Check(gvCounter = 0, 'Interfaces not realeased');
end;



initialization

  RegisterTest(TCheck_Str_Intf);
end.

