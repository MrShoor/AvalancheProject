unit Heap_Int;

{$mode objfpc}{$H+}

interface

uses
  Dialogs,
  Classes, SysUtils, fpcunit, testutils, testregistry,
  avContnrs;

type

  { TIntHeapTest }

  TIntHeapTest = class(TTestCase)
  published
    procedure HeapifyFromArray;
    procedure HeapSortForIArray;
  end;

implementation

type
  IHeapInt = specialize IHeap<Integer>;
  THeapInt = specialize THeap<Integer>;

procedure TIntHeapTest.HeapifyFromArray;
var Heap: IHeapInt;
    Data: specialize TArrData<Integer>;
    i, Top, NextTop: Integer;
begin
  SetLength(Data, 1000);
  for i := 0 to Length(Data) - 1 do
    Data[i] := Random(500);

  Heap := THeapInt.Create(Data);
  Check(Length(Data) = Heap.Count);

  Top := Heap.PeekTop;
  while Heap.Count > 0 do
  begin
    NextTop := Heap.ExtractTop;
    Check(Top <= NextTop, 'Heap inconsistent: '+IntToStr(Top)+'>'+IntToStr(NextTop));
  end;
end;

procedure TIntHeapTest.HeapSortForIArray;
type
  IIntArr = specialize IArray<Integer>;
  TIntArr = specialize TArray<Integer>;
var Data: IIntArr;
    i, n: Integer;
begin
  Data := TIntArr.Create;
  Data.Capacity := 1000;
  for i := 0 to 1000 - 1 do
    Data.Add(Random(500));

  Data.HeapSort();
  n := Data[0];
  for i := 1 to Data.Count - 1 do
  begin
    Check(n <= Data[i], 'Not sorted: '+IntToStr(n)+'>'+IntToStr(Data[i]));
    n := Data[i];
  end;
end;



initialization

  RegisterTest(TIntHeapTest);
end.

