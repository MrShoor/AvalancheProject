unit avPlatform;
{$I avConfig.inc}

interface

uses
  Classes, SysUtils, Windows, mutils, Messages;

const
  WM_UPS = WM_USER + 106;
  NOTWINDOW = INVALID_HANDLE_VALUE;

type
  TWindow = HWND;

procedure CaptureWindow(Window: TWindow);
procedure ReleaseCaptureWindow;
procedure GetRectOfWindow(Window: TWindow; out Left, Top, Right, Bottom: integer); overload;
function GetRectOfWindow(Window: TWindow): TRectI; overload;
procedure InvalidateWindow(Window: TWindow; EraseBackground: Boolean);
function  IsValidWindow(Window: TWindow): Boolean;

function GetCursorPos(Window: TWindow; isLocal: boolean; isAbsolute: boolean): TVec2;
function GetTime64: Int64;
function GetTime: Double;

procedure RegisterHandler(Obj: TObject; Wnd: TWindow);
procedure UnregisterHandler(Obj: TObject; Wnd: TWindow);

implementation

uses
  avTypes, syncobjs;

type
  TwndSubclassInfo = class
    handle     : TWindow;
    PrevWndProc: TFNWndProc;
    mains      : TList;
  end;

const
  MK_XBUTTON1      = $20;
  MK_XBUTTON2      = $40;
  WM_XBUTTONDOWN   = $020B;
  WM_XBUTTONUP     = $020C;
  WM_XBUTTONDBLCLK = $020D;
  WM_MOUSELAST     = $020D;
  XBUTTON1 = $0001;
  XBUTTON2 = $0002;

var StartTime64, Freq: Int64;
    SubClassingList: TThreadList;

function MouseShifts(Keys: Word): TShifts;
begin
  Result := [];
  if Keys and MK_SHIFT    <> 0 then Include(Result, sShift);
  if Keys and MK_CONTROL  <> 0 then Include(Result, sCtrl);
  if Keys and MK_LBUTTON  <> 0 then Include(Result, sLeft);
  if Keys and MK_RBUTTON  <> 0 then Include(Result, sRight);
  if Keys and MK_MBUTTON  <> 0 then Include(Result, sMiddle);
  if Keys and MK_XBUTTON1 <> 0 then Include(Result, sXMButton1);
  if Keys and MK_XBUTTON2 <> 0 then Include(Result, sXMButton2);
  if GetKeyState(VK_MENU) <  0 then Include(Result, sAlt);
end;

function KeyShifts(KeyData: Longint): TShifts;
const
  AltMask = $20000000;
begin
  Result := [];
  if GetKeyState(VK_SHIFT) < 0 then Include(Result, sShift);
  if GetKeyState(VK_CONTROL) < 0 then Include(Result, sCtrl);
  if KeyData and AltMask <> 0 then Include(Result, sAlt);
end;

function FindSubclassInfo(handle: TWindow): TwndSubclassInfo;
var i: Integer;
    lst: TList;
    sc: TwndSubclassInfo;
begin
  lst := SubClassingList.LockList;
  try
    for i := 0 to lst.Count - 1 do
    begin
      Result := TwndSubclassInfo(lst.Items[i]);
      if Result.handle = handle then Exit;
    end;
    Result := nil;
  finally
    SubClassingList.UnlockList;
  end;
end;

function CommonWndProc(handle: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT stdcall;
var sc: TwndSubclassInfo;
    proc: TFNWndProc;
    i: integer;
    msg     : TavMessage;
    MouseMsg: TavMouseBtnMessage;
    KeyMsg  : TavKeyMessage;
    msgptr  : PDWord;
begin
  {$IFOPT R+}
    {$DEFINE RANGEON}
    {$R-}
  {$ELSE}
    {$UNDEF RANGEON}
  {$ENDIF}
  sc := FindSubclassInfo(handle);
  if assigned(sc) then
  begin
    msgptr:=nil;
    proc := sc.PrevWndProc;
    case uMsg of
      WM_UPS:
        begin
          msg.msg:=EM_UPS;
          msg.param:=wParam;
          msg.sender:=nil;
          msg.result:=false;
          msgptr:=@msg;
        end;
      WM_MOUSEFIRST..WM_MOUSELAST:
        begin
          case uMsg of
            WM_LBUTTONDOWN, WM_LBUTTONUP, WM_LBUTTONDBLCLK: MouseMsg.button:=1;
            WM_RBUTTONDOWN, WM_RBUTTONUP, WM_RBUTTONDBLCLK: MouseMsg.button:=2;
            WM_MBUTTONDOWN, WM_MBUTTONUP, WM_MBUTTONDBLCLK: MouseMsg.button:=3;
            WM_XBUTTONDOWN, WM_XBUTTONUP, WM_XBUTTONDBLCLK:
              begin
                if HiWord(wParam)=1 then MouseMsg.button:=4;
                if HiWord(wParam)=2 then MouseMsg.button:=5;
              end;
          else
            MouseMsg.button:=0;
          end;
          case uMsg of
            WM_MOUSEMOVE: MouseMsg.msg:=EM_MOUSEMOVE;
            WM_LBUTTONDOWN, WM_RBUTTONDOWN, WM_MBUTTONDOWN, WM_XBUTTONDOWN: MouseMsg.msg:=EM_MOUSEDOWN;
            WM_LBUTTONUP, WM_RBUTTONUP, WM_MBUTTONUP, WM_XBUTTONUP: MouseMsg.msg:=EM_MOUSEUP;
            WM_LBUTTONDBLCLK, WM_RBUTTONDBLCLK, WM_MBUTTONDBLCLK, WM_XBUTTONDBLCLK: MouseMsg.msg:=EM_MOUSEDBLCLICK;
            WM_MOUSEWHEEL: MouseMsg.msg:=EM_MOUSEWHEEL;
          else
            MouseMsg.msg:=0;
          end;
          if uMsg = WM_MOUSEWHEEL then MouseMsg.wheelShift:=SmallInt(HiWord(wParam)) div WHEEL_DELTA else MouseMsg.wheelShift:=0;
          MouseMsg.xPos:=SmallInt(LoWord(lParam));
          MouseMsg.yPos:=SmallInt(HiWord(lParam));
          MouseMsg.shifts:=MouseShifts(wParam);
          MouseMsg.result:=false;

          msgptr:=@MouseMsg;
        end;
      WM_MOUSEHOVER: begin end;
      WM_MOUSELEAVE: begin end;

      WM_KEYFIRST..WM_KEYLAST:
        begin
          case uMsg of
            WM_KEYDOWN, WM_KEYUP, WM_SYSKEYDOWN, WM_SYSKEYUP: KeyMsg.Key:=wParam;
            WM_CHAR, WM_SYSCHAR, WM_DEADCHAR, WM_SYSDEADCHAR: KeyMsg.Char:=WideChar(wParam);
          else
            KeyMsg.Key:=0;
          end;
          case uMsg of
            WM_SYSKEYDOWN, WM_SYSKEYUP, WM_SYSCHAR, WM_SYSDEADCHAR: KeyMsg.Sys:=true;
          else
            KeyMsg.Sys:=false;
          end;
          case uMsg of
            WM_KEYDOWN, WM_SYSKEYDOWN: KeyMsg.msg:=EM_KEYDOWN;
            WM_KEYUP, WM_SYSKEYUP: KeyMsg.msg:=EM_KEYUP;
            WM_CHAR, WM_SYSCHAR, WM_DEADCHAR, WM_SYSDEADCHAR: KeyMsg.msg:=EM_CHAR;
          else
            KeyMsg.msg:=0;
          end;
          case uMsg of
            WM_DEADCHAR, WM_SYSDEADCHAR: KeyMsg.Dead:=true;
          else
            KeyMsg.Dead:=false;
          end;
          KeyMsg.shifts:=KeyShifts(lParam);
          KeyMsg.Result:=false;

          msgptr:=@KeyMsg;
        end;

      WM_DESTROY: begin
                    msg.msg := EM_WINDOWDESTROY;
                    msg.param := wParam;
                    msg.sender := nil;
                    msg.result := False;
                    msgptr := @msg;
                  end;
    end;
    if assigned(msgptr) then
      if msgptr^ <> 0 then
        for i := sc.mains.Count - 1 downto 0 do
          TObject(sc.mains.Items[i]).Dispatch(msgptr^);
    {$IfDef FPC}
    Result := CallWindowProc(WNDPROC(proc), handle, uMsg, wParam, lParam);
    {$Else}
    Result := CallWindowProc(TFNWndProc(proc), handle, uMsg, wParam, lParam);
    {$EndIf}
  end
  else
    Result := DefWindowProc(handle, uMsg, wParam, lParam);
  {$IFDEF RANGEON}
    {$R+}
    {$UNDEF RANGEON}
  {$ENDIF}
end;

procedure CaptureWindow(Window: TWindow);
begin
  SetCapture(Window);
end;

procedure ReleaseCaptureWindow;
begin
  ReleaseCapture;
end;

procedure GetRectOfWindow(Window: TWindow; out Left, Top, Right, Bottom: integer);
var rct: TRect;
begin
  GetClientRect(Window, rct);
  Left  :=rct.Left;
  Top   :=rct.Top;
  Right :=rct.Right;
  Bottom:=rct.Bottom;
end;

function GetRectOfWindow(Window: TWindow): TRectI;
var rct: TRect;
begin
  GetClientRect(Window, rct);
  Result.Left  :=rct.Left;
  Result.Top   :=rct.Top;
  Result.Right :=rct.Right;
  Result.Bottom:=rct.Bottom;
end;

procedure InvalidateWindow(Window: TWindow; EraseBackground: Boolean);
begin
  InvalidateRect(Window, nil, EraseBackground);
end;

function IsValidWindow(Window: TWindow): Boolean;
begin
  Result := IsWindow(Window);
end;

function GetCursorPos(Window: TWindow; isLocal: boolean; isAbsolute: boolean): TVec2;
var pt : Windows.TPoint;
    rct: Windows.TRect;
begin
  Windows.GetCursorPos(pt);
  if not isLocal then
  begin
    Result.X:=pt.X;
    Result.Y:=pt.Y;
    exit;
  end;

  Windows.ScreenToClient(Window, pt);
  Result.X:=pt.X;
  Result.Y:=pt.Y;
  if not isAbsolute then exit;

  Windows.GetClientRect(Window, rct);
  Result.x:=(Result.x/(rct.Right - rct.Left) - 0.5)*2;
  Result.y:=-(Result.y/(rct.Bottom - rct.Top) - 0.5)*2;
end;

function GetTime64: Int64;
var currtime: Int64;
begin
  QueryPerformanceCounter(currtime);
  Result := (1000 * (currtime - StartTime64)) div Freq;
end;

function GetTime: Double;
var currtime: Int64;
begin
  QueryPerformanceCounter(currtime);
  Result := (currtime - StartTime64) / Freq;
end;

procedure RegisterHandler(Obj: TObject; Wnd: TWindow);
var lst: TList;
    i: Integer;
    sc: TwndSubclassInfo;
begin
  Assert(Assigned(SubClassingList));
  try
    lst := SubClassingList.LockList;
    for i := 0 to lst.Count - 1 do
    begin
      sc := TwndSubclassInfo(lst.Items[i]);
      if sc.handle = Wnd then
      begin
        Assert(sc.mains.IndexOf(Obj)=-1);
        sc.mains.Add(Obj);
        Exit;
      end;
    end;

    sc := TwndSubclassInfo.Create;
    sc.handle := Wnd;
    sc.PrevWndProc := TFNWndProc(SetWindowLongPtr(Wnd, GWLP_WNDPROC, LONG_PTR(@CommonWndProc)));;
    sc.mains := TList.Create;
    sc.mains.Add(obj);
    lst.Add(sc);
  finally
    SubClassingList.UnlockList;
  end;
end;

procedure UnregisterHandler(Obj: TObject; Wnd: TWindow);
var lst: TList;
    i, n: Integer;
    sc: TwndSubclassInfo;
begin
  Assert(Assigned(SubClassingList));
  try
    lst := SubClassingList.LockList;
    for i := 0 to lst.Count - 1 do
    begin
      sc := TwndSubclassInfo(lst.Items[i]);
      if sc.handle = Wnd then
      begin
        n := sc.mains.IndexOf(Obj);
        Assert(n>=0);
        sc.mains.Delete(n);

        if sc.mains.Count = 0 then
        begin
          sc.mains.Free;
          SetWindowLongPtr(sc.handle, GWLP_WNDPROC, LONG_PTR(sc.PrevWndProc));
          sc.Free;
          lst.Delete(i);
        end;

        Exit;
      end;
    end;
    Assert(False);
  finally
    SubClassingList.UnlockList;
  end;
end;

procedure InitSubClassingList;
begin
  SubClassingList := TThreadList.Create;
end;

procedure FreeSubClassingList;
var n: Integer;
    lst: TList;
begin
  if assigned(SubClassingList) then
  begin
    lst := SubClassingList.LockList;
    try
      n := lst.Count;
    finally
      SubClassingList.UnlockList;
    end;
    if n > 0 then
      MessageBox(0, 'SubClassingList not empty!', 'Warning.', MB_OK);
    FreeAndNil(SubClassingList);
  end;
end;

initialization
  QueryPerformanceCounter(StartTime64);
  QueryPerformanceFrequency(Freq);
  InitSubClassingList;

finalization
  FreeSubClassingList;

end.

