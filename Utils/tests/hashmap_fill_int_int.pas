unit hashmap_Fill_Int_Int;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, avContnrs;

type
  TFill_Int_Int= class(TTestCase)
  published
    procedure Test;
  end;

implementation

procedure TFill_Int_Int.Test;
const FILL_COUNT = 10000;
type
  IHash = specialize IHashMap<Integer, Integer>;
  THash = specialize THashMap<Integer, Integer>;
var hash: IHash;
    i: Integer;
begin
  hash := THash.Create;
  for i := 0 to FILL_COUNT - 1 do
    hash.Add(i, i * 3);

  Check(hash.Count = FILL_COUNT, 'Incorrect count value');

  for i := 0 to FILL_COUNT - 1 do
    Check(hash[i] = i * 3, 'Incorrect filling');
end;


initialization

  RegisterTest(TFill_Int_Int);
end.

