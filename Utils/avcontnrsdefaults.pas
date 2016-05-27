unit avContnrsDefaults;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TypInfo;

const
  EMPTY_HASH = 0;

type
  THashFunction = function (const SrcData; len: Cardinal): Cardinal;

  TInterfacedObjectEx = TInterfacedObject;

  { IComparer }

  IComparer = interface
    function Compare(const Left, Right): Integer;
  end;

  { IEqualityComparer }

  IEqualityComparer = interface
    function Hash(const Value): Integer;
    function IsEqual(const Left, Right): Boolean;
  end;

  { TInvertedComparer }

  TInvertedComparer = class(TInterfacedObjectEx, IComparer)
  private
    FComp: IComparer;
    function Compare(const Left, Right): Integer;
  public
    constructor Create(const AComparer: IComparer);
  end;


var gvDefaultHash: THashFunction;

function Murmur2DefSeed(const SrcData; len: LongWord): Cardinal;
function Murmur2(const SrcData; len: LongWord; const Seed: LongWord = $9747b28c): Cardinal;

function IsAutoReferenceCounterType(const AType: PTypeInfo): Boolean;

function AutoSelectComparer(const pInfo: PTypeInfo; const TypeSize: Integer): IComparer;
function AutoSelectEqualityComparer(const pInfo: PTypeInfo; const TypeSize: Integer): IEqualityComparer;

implementation

uses Math;

type
  { TEqualityComparer_UString }

  TEqualityComparer_UString = class (TInterfacedObjectEx, IEqualityComparer)
  public
    function Hash(const Value): Integer;
    function IsEqual(const Left, Right): Boolean;
  end;

  { TEqualityComparer_AString }

  TEqualityComparer_AString = class (TInterfacedObjectEx, IEqualityComparer)
  public
    function Hash(const Value): Integer;
    function IsEqual(const Left, Right): Boolean;
  end;

  { TEqualityComparer_WString }

  TEqualityComparer_WString = class (TInterfacedObjectEx, IEqualityComparer)
  public
    function Hash(const Value): Integer;
    function IsEqual(const Left, Right): Boolean;
  end;

  { TEqualityComparer_Data }

  TEqualityComparer_Data = class (TInterfacedObjectEx, IEqualityComparer)
  private
    ElementSize: Integer;
  public
    function Hash(const Value): Integer;
    function IsEqual(const Left, Right): Boolean;

    constructor Create(const ATypeSize: Integer);
  end;

  { TEqualityComparer_Array }

  TEqualityComparer_Array = class (TInterfacedObjectEx, IEqualityComparer)
  private
    ElementSize: Integer;
  public
    function Hash(const Value): Integer;
    function IsEqual(const Left, Right): Boolean;

    constructor Create(const p: PTypeInfo);
  end;

  { TComparer_UString }

  TComparer_UString = class (TInterfacedObjectEx, IComparer)
  public
    function Compare(const Left, Right): Integer;
  end;

  { TComparer_AString }

  TComparer_AString = class (TInterfacedObjectEx, IComparer)
  public
    function Compare(const Left, Right): Integer;
  end;

  { TComparer_WString }

  TComparer_WString = class (TInterfacedObjectEx, IComparer)
  public
    function Compare(const Left, Right): Integer;
  end;

  { TComparer_Data }

  TComparer_Data = class (TInterfacedObjectEx, IComparer)
  private
    ElementSize: Integer;
  public
    function Compare(const Left, Right): Integer;

    constructor Create(const ATypeSize: Integer);
  end;

  { TComparer_Array }

  TComparer_Array = class (TInterfacedObjectEx, IComparer)
  private
    ElementSize: Integer;
  public
    function Compare(const Left, Right): Integer;

    constructor Create(const p: PTypeInfo);
  end;

  { TComparer_OrdValue }

  generic TComparer_OrdValue<T> = class (TInterfacedObjectEx, IComparer)
  public
    function Compare(const Left, Right): Integer;
  end;

function Murmur2DefSeed(const SrcData; len: LongWord): Cardinal;
begin
  Result := Murmur2(SrcData, len);
  if Result = EMPTY_HASH then Inc(Result);
end;

{$R-}
{$Q-}
function Murmur2(const SrcData; len: LongWord; const Seed: LongWord = $9747B28C): Cardinal;
var
  S: array [0 .. 0] of Byte absolute SrcData;
  h: LongWord;
  k: LongWord;
  data: Integer;
const
  // 'm' and 'r' are mixing constants generated offline.
  // They're not really 'magic', they just happen to work well.
  m = $5BD1E995;
  r = 24;
begin
  // The default seed, $9747b28c, is from the original C library

  // Initialize the hash to a 'random' value
  h := Seed xor len;

  // Mix 4 bytes at a time into the hash
  data := 0;

  while (len >= 4) do
  begin
    k := PLongWord(@S[data])^;

    k := k * m;
    k := k xor (k shr r);
    k := k * m;

    h := h * m;
    h := h xor k;

    data := data + 4;
    len := len - 4;
  end;

  { Handle the last few bytes of the input array
    S: ... $69 $18 $2f
    }
  Assert(len <= 3);
  if len = 3 then
    h := h xor (LongWord(S[data + 2]) shl 16);
  if len >= 2 then
    h := h xor (LongWord(S[data + 1]) shl 8);
  if len >= 1 then
  begin
    h := h xor (LongWord(S[data]));

    h := h * m;
  end;

  // Do a few final mixes of the hash to ensure the last few
  // bytes are well-incorporated.
  h := h xor (h shr 13);

  h := h * m;
  h := h xor (h shr 15);

  Result := h;
end;

function AutoSelectComparer(const pInfo: PTypeInfo; const TypeSize: Integer): IComparer;
type
  TComparer_Int8   = specialize TComparer_OrdValue<ShortInt>;
  TComparer_UInt8  = specialize TComparer_OrdValue<Byte>;
  TComparer_Int16  = specialize TComparer_OrdValue<SmallInt>;
  TComparer_UInt16 = specialize TComparer_OrdValue<Word>;
  TComparer_Int32  = specialize TComparer_OrdValue<LongInt>;
  TComparer_UInt32 = specialize TComparer_OrdValue<Cardinal>;
  TComparer_Int64  = specialize TComparer_OrdValue<Int64>;
  TComparer_UInt64 = specialize TComparer_OrdValue<UInt64>;

  TComparer_Single   = specialize TComparer_OrdValue<Single>;
  TComparer_Double   = specialize TComparer_OrdValue<Double>;
  TComparer_Extended = specialize TComparer_OrdValue<Extended>;
  TComparer_Currency = specialize TComparer_OrdValue<Currency>;
  TComparer_Comp     = specialize TComparer_OrdValue<Comp>;
var pData: PTypeData;
begin
  Result := nil;
  case pInfo^.Kind of
    tkUnknown     : Assert(False, 'What?');
    tkInteger     :
      begin
        pData := GetTypeData(pInfo);
        case pData^.OrdType of
          otSByte: Result := TComparer_Int8.Create;
          otUByte: Result := TComparer_UInt8.Create;
          otSWord: Result := TComparer_Int16.Create;
          otUWord: Result := TComparer_UInt16.Create;
          otSLong: Result := TComparer_Int32.Create;
          otULong: Result := TComparer_UInt32.Create;
        end;
      end;
    tkChar        : Result := TComparer_Data.Create(TypeSize);
    tkEnumeration : Result := TComparer_Data.Create(TypeSize);
    tkFloat       :
      begin
        pData := GetTypeData(pInfo);
        case pData^.FloatType of
          ftSingle  : Result := TComparer_Single.Create;
          ftDouble  : Result := TComparer_Double.Create;
          ftExtended: Result := TComparer_Extended.Create;
          ftComp    : Result := TComparer_Comp.Create;
          ftCurr    : Result := TComparer_Currency.Create;
        end;
      end;
    tkSet         : Result := TComparer_Data.Create(TypeSize);
    tkMethod      : Result := TComparer_Data.Create(TypeSize);
    tkSString     : Result := TComparer_Data.Create(TypeSize);
    tkLString     : Assert(False, 'Todooo');
    tkAString     : Result := TComparer_AString.Create;
    tkWString     : Result := TComparer_WString.Create;
    tkVariant     : Assert(False, 'Todooo');
    tkArray       : Result := TComparer_Data.Create(TypeSize);
    tkRecord      : Result := TComparer_Data.Create(TypeSize);
    tkInterface   : Result := TComparer_Data.Create(TypeSize);
    tkClass       : Result := TComparer_Data.Create(TypeSize);
    tkObject      : Result := TComparer_Data.Create(TypeSize);
    tkWChar       : Result := TComparer_Data.Create(TypeSize);
    tkBool        : Result := TComparer_Data.Create(TypeSize);
    tkInt64       : Result := TComparer_Int64.Create;
    tkQWord       : Result := TComparer_UInt64.Create;
    tkDynArray    : Result := TComparer_Array.Create(pInfo);
    tkInterfaceRaw: Result := TComparer_Data.Create(TypeSize);
    tkProcVar     : Result := TComparer_Data.Create(TypeSize);
    tkUString     : Result := TComparer_UString.Create;
    tkUChar       : Result := TComparer_Data.Create(TypeSize);
    tkHelper      : Result := TComparer_Data.Create(TypeSize);
    tkFile        : Result := TComparer_Data.Create(TypeSize);
    tkClassRef    : Result := TComparer_Data.Create(TypeSize);
    tkPointer     : Result := TComparer_Data.Create(TypeSize);
  end;
end;

function IsAutoReferenceCounterType(const AType: PTypeInfo): Boolean;
var td: PTypeData;
    mf: PManagedField;
    n, i: Integer;
begin
  case AType^.Kind of
  tkUnknown     : Assert(False, 'What???');
  tkInteger     : Result := False;
  tkChar        : Result := False;
  tkEnumeration : Result := False;
  tkFloat       : Result := False;
  tkSet         : Result := False;
  tkMethod      : Result := False;
  tkSString     : Result := False;
  tkLString     : Result := True;
  tkAString     : Result := True;
  tkWString     : Result := True;
  tkVariant     : Result := True;
  tkArray       : Result := False;
  tkRecord      : begin
                    Result := False;
                    td := GetTypeData(AType);
                    n := td^.ManagedFldCount;
                    mf := PManagedField(@td^.ManagedFldCount);
                    Inc(PByte(mf), SizeOf(td^.ManagedFldCount));
                    for i := 0 to n-1 do
                    begin
                      if IsAutoReferenceCounterType(mf^.TypeRef) then
                      begin
                        Result := True;
                        Exit;
                      end;
                      Inc(mf);
                    end;
                  end;
  tkInterface   : Result := True;
  tkClass       : Result := False;
  tkObject      : Result := False;
  tkWChar       : Result := False;
  tkBool        : Result := False;
  tkInt64       : Result := False;
  tkQWord       : Result := False;
  tkDynArray    : Result := True;
  tkInterfaceRaw: Result := False;
  tkProcVar     : Result := False;
  tkUString     : Result := True;
  tkUChar       : Result := False;
  tkHelper      : Result := False;
  tkFile        : Result := False;
  tkClassRef    : Result := False;
  tkPointer     : Result := False;
  end;
end;

function AutoSelectEqualityComparer(const pInfo: PTypeInfo; const TypeSize: Integer): IEqualityComparer;
begin
  Result := nil;
  case pInfo^.Kind of
    tkUnknown     : Assert(False, 'What?');
    tkInteger     : Result := TEqualityComparer_Data.Create(TypeSize);
    tkChar        : Result := TEqualityComparer_Data.Create(TypeSize);
    tkEnumeration : Result := TEqualityComparer_Data.Create(TypeSize);
    tkFloat       : Result := TEqualityComparer_Data.Create(TypeSize);
    tkSet         : Result := TEqualityComparer_Data.Create(TypeSize);
    tkMethod      : Result := TEqualityComparer_Data.Create(TypeSize);
    tkSString     : Result := TEqualityComparer_Data.Create(TypeSize);
    tkLString     : Assert(False, 'Todooo');
    tkAString     : Result := TEqualityComparer_AString.Create;
    tkWString     : Result := TEqualityComparer_WString.Create;
    tkVariant     : Assert(False, 'Todooo');
    tkArray       : Result := TEqualityComparer_Data.Create(TypeSize);
    tkRecord      : Result := TEqualityComparer_Data.Create(TypeSize);
    tkInterface   : Result := TEqualityComparer_Data.Create(TypeSize);
    tkClass       : Result := TEqualityComparer_Data.Create(TypeSize);
    tkObject      : Result := TEqualityComparer_Data.Create(TypeSize);
    tkWChar       : Result := TEqualityComparer_Data.Create(TypeSize);
    tkBool        : Result := TEqualityComparer_Data.Create(TypeSize);
    tkInt64       : Result := TEqualityComparer_Data.Create(TypeSize);
    tkQWord       : Result := TEqualityComparer_Data.Create(TypeSize);
    tkDynArray    : Result := TEqualityComparer_Array.Create(pInfo);
    tkInterfaceRaw: Result := TEqualityComparer_Data.Create(TypeSize);
    tkProcVar     : Result := TEqualityComparer_Data.Create(TypeSize);
    tkUString     : Result := TEqualityComparer_UString.Create;
    tkUChar       : Result := TEqualityComparer_Data.Create(TypeSize);
    tkHelper      : Result := TEqualityComparer_Data.Create(TypeSize);
    tkFile        : Result := TEqualityComparer_Data.Create(TypeSize);
    tkClassRef    : Result := TEqualityComparer_Data.Create(TypeSize);
    tkPointer     : Result := TEqualityComparer_Data.Create(TypeSize);
  end;
end;

{ TInvertedComparer }

function TInvertedComparer.Compare(const Left, Right): Integer;
begin
  Result := FComp.Compare(Right, Left);
end;

constructor TInvertedComparer.Create(const AComparer: IComparer);
begin
  Assert(AComparer <> nil);
  FComp := AComparer;
end;

{ TComparer_OrdValue }

function TComparer_OrdValue.Compare(const Left, Right): Integer;
var ILeft  : T absolute Left;
    IRight : T absolute Right;
begin
  if ILeft < IRight then
    Result := -1
  else
    if ILeft = IRight then
      Result := 0
    else
      Result := 1;
end;

{ TComparer_Array }

function TComparer_Array.Compare(const Left, Right): Integer;
var arrLeft  : TBytes absolute Left;
    arrRight : TBytes absolute Right;
    diff : Integer;
    i : Integer;
begin
  if Length(arrLeft) = Length(arrRight) then
    Result := 0
  else
  if Length(arrLeft) < Length(arrRight) then
    Result := -1
  else
    Result := 1;

  for i := 0 to min(Length(arrLeft), Length(arrRight))*ElementSize - 1 do
  begin
    diff := arrLeft[i] - arrRight[i];
    if diff <> 0 then Exit(diff);
  end;
end;

constructor TComparer_Array.Create(const p: PTypeInfo);
begin
  Assert(p^.Kind = tkDynArray);
  ElementSize := GetTypeData(p)^.elSize;
end;

{ TComparer_Data }

function TComparer_Data.Compare(const Left, Right): Integer;
var arrLeft  : array [0..0] of Byte absolute Left;
    arrRight : array [0..0] of Byte absolute Right;
    diff : Integer;
    i : Integer;
begin
  Result := 0;
  for i := ElementSize - 1 downto 0 do
  begin
    diff := arrLeft[i] - arrRight[i];
    if diff <> 0 then Exit(diff);
  end;
end;

constructor TComparer_Data.Create(const ATypeSize: Integer);
begin
  ElementSize := ATypeSize;
end;

{ TComparer_WString }

function TComparer_WString.Compare(const Left, Right): Integer;
var
  strLeft: WideString absolute Left;
  strRight: WideString absolute Right;
begin
  Result := CompareStr(strLeft, strRight);
end;

{ TComparer_AString }

function TComparer_AString.Compare(const Left, Right): Integer;
var
  strLeft: AnsiString absolute Left;
  strRight: AnsiString absolute Right;
begin
  Result := CompareStr(strLeft, strRight);
end;

{ TComparer_UString }

function TComparer_UString.Compare(const Left, Right): Integer;
var
  strLeft: UnicodeString absolute Left;
  strRight: UnicodeString absolute Right;
begin
  Result := CompareStr(strLeft, strRight);
end;

{ TEqualityComparer_WString }

function TEqualityComparer_WString.Hash(const Value): Integer;
var str: WideString absolute Value;
begin
  if Length(str) = 0 then
    Result := 42
  else
    Result := gvDefaultHash(str[1], Length(str));
end;

function TEqualityComparer_WString.IsEqual(const Left, Right): Boolean;
var
  strLeft: WideString absolute Left;
  strRight: WideString absolute Right;
begin
  Result := strLeft = strRight;
end;

{ TEqualityComparer_Array }

function TEqualityComparer_Array.Hash(const Value): Integer;
var arr : TBytes absolute Value;
begin
  if Length(arr) = 0 then
    Result := 42
  else
    Result := gvDefaultHash(arr[0], Length(arr)*ElementSize);
end;

function TEqualityComparer_Array.IsEqual(const Left, Right): Boolean;
var arrLeft  : TBytes absolute Left;
    arrRight : TBytes absolute Right;
begin
  if Length(arrLeft) <> Length(arrRight) then Exit(False);
  if Length(arrLeft) = 0 then Exit(True);
  Result := CompareMem(Pointer(arrLeft), Pointer(arrRight), Length(arrLeft)*ElementSize);
end;

constructor TEqualityComparer_Array.Create(const p: PTypeInfo);
begin
  Assert(p^.Kind = tkDynArray);
  ElementSize := GetTypeData(p)^.elSize;
end;

{ TEqualityComparer_UString }

function TEqualityComparer_UString.Hash(const Value): Integer;
var str: UnicodeString absolute Value;
begin
  if Length(str) = 0 then
    Result := 42
  else
    Result := gvDefaultHash(str[1], Length(str));
end;

function TEqualityComparer_UString.IsEqual(const Left, Right): Boolean;
var
  strLeft: UnicodeString absolute Left;
  strRight: UnicodeString absolute Right;
begin
  Result := strLeft = strRight;
end;

{ TEqualityComparer_AString }

function TEqualityComparer_AString.Hash(const Value): Integer;
var str: AnsiString absolute Value;
begin
  if Length(str) = 0 then
    Result := 42
  else
    Result := gvDefaultHash(str[1], Length(str));
end;

function TEqualityComparer_AString.IsEqual(const Left, Right): Boolean;
var
  strLeft: AnsiString absolute Left;
  strRight: AnsiString absolute Right;
begin
  Result := strLeft = strRight;
end;

{ TDataEqualityComparer }

function TEqualityComparer_Data.Hash(const Value): Integer;
begin
  Result := gvDefaultHash(Value, ElementSize);
end;

function TEqualityComparer_Data.IsEqual(const Left, Right): Boolean;
begin
  Result := CompareMem(@Left, @Right, ElementSize);
end;

constructor TEqualityComparer_Data.Create(const ATypeSize: Integer);
begin
  ElementSize := ATypeSize;
end;

initialization
  gvDefaultHash := @Murmur2DefSeed;

end.

