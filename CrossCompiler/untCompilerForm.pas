unit untCompilerForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ExtCtrls, IdContext, IdBaseComponent,
  IdComponent, IdCustomTCPServer, IdTCPServer, IdCustomHTTPServer,
  IdHTTPServer, untCompilerTypes, SuperObject, dglOpenGL,
  StdCtrls, ComCtrls, Menus;

const
  WM_COMPILESHADER = WM_USER + 101;  
  WM_LINKPROGRAM = WM_USER + 102;

type
  TfrmcGLSL = class(TForm)
    TrayIcon: TTrayIcon;
    imgTrayIcons: TImageList;
    idleTimer: TTimer;
    mmLog: TMemo;
    btnClearLog: TButton;
    cbUseLog: TCheckBox;
    cbIdleClear: TCheckBox;
    tbLogLevel: TTrackBar;
    Label1: TLabel;
    pmTray: TPopupMenu;
    Exit1: TMenuItem;
    btnCopyLog: TButton;
    procedure FormCreate(Sender: TObject);
    procedure IdHTTPServerCommandOther(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
    procedure idleTimerTimer(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure TrayIconClick(Sender: TObject);
    procedure btnCopyLogClick(Sender: TObject);
  private
    FisExitNow : Boolean;
    FDC: HDC;
    FRC: HGLRC;

    FHTTP: TIdHTTPServer;

    procedure WMCompileShader(var msg: TMessage); message WM_COMPILESHADER;
    procedure WMLinkProgram(var msg: TMessage); message WM_LINKPROGRAM;

    procedure AddToLog(const str: string; level: Integer = 0);
  public
    { Public declarations }
  end;

var
  frmcGLSL: TfrmcGLSL;

implementation

uses
  Clipbrd;

{$R *.dfm}

procedure TfrmcGLSL.AddToLog(const str: string; level: Integer);
begin
  if tbLogLevel.Position < level then Exit;
  if not cbUseLog.Checked then Exit;
  mmLog.Lines.Add(str);
end;

procedure TfrmcGLSL.btnClearLogClick(Sender: TObject);
begin
  mmLog.Clear;
end;

procedure TfrmcGLSL.btnCopyLogClick(Sender: TObject);
begin
  Clipboard.SetTextBuf(PChar(mmLog.Text));
end;

procedure TfrmcGLSL.Exit1Click(Sender: TObject);
begin
  FisExitNow := True;
  Close;
end;

procedure TfrmcGLSL.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := FisExitNow;
  if not FisExitNow then Hide;
end;

procedure TfrmcGLSL.FormCreate(Sender: TObject);
begin
  Left := Screen.WorkAreaRect.Right - Width - 5;
  Top := Screen.WorkAreaRect.Bottom - Height - 5;

  FDC := GetDC(Handle);
  FRC := CreateRenderingContext(FDC, [opDoubleBuffered], 32, 24, 8, 0, 0, 0);
  ActivateRenderingContext(FDC, FRC);

  mmLog.Clear;

  FHTTP := TIdHTTPServer.Create(Self);
  FHTTP.OnCommandOther := IdHTTPServerCommandOther;
  FHTTP.OnCommandGet := IdHTTPServerCommandOther;
  FHTTP.DefaultPort := 11580;
  FHTTP.Active := True;
end;

procedure TfrmcGLSL.FormDestroy(Sender: TObject);
begin
  wglDeleteContext(FRC);
  ReleaseDC(Handle, FDC);
end;

procedure TfrmcGLSL.IdHTTPServerCommandOther(AContext: TIdContext; ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
  function ProcessOutputLog(log: string): string;
  var sl: TStringList;
  begin
    sl := TStringList.Create;
    try
        sl.Text := log;
        while (sl.Count > 1) do
        begin
          if (sl.Strings[0]='') then
            sl.Delete(0)
          else
            Break;
        end;

        while (sl.Count > 1) do
        begin
          if sl.Strings[sl.Count - 1]='' then
            sl.Delete(sl.Count - 1)
          else
            Break;
        end;
        Result := sl.Text;
    finally
        sl.Free;
    end;
  end;
  function CompileAddLog(const shadername: string; target: TShaderTarget; code: string; var log: string): Boolean;
  var shader: ICompileData;
  begin
    case target of
      stGeometry: log := log + 'Compiling ''' + shadername + ''' as geometry target... ';
      stVertex  : log := log + 'Compiling ''' + shadername + ''' as vertex target... ';
      stFragment: log := log + 'Compiling ''' + shadername + ''' as fragment target... ';
    end;
    shader := CreateCompileData(target, code);
    SendMessage(Handle, WM_COMPILESHADER, WPARAM(shader), 0);
    if shader.Status = csSuccessed then
    begin
      log := log + 'done';
      Result := True;
    end
    else
    begin
      log := log + 'failed';
      Result := False;
    end;
    log := log + ProcessOutputLog(shader.Log);
  end;

  function LinkAddLog(const name: string; const geoms: TGeomBins; const verts: TVertBins; const frags: TFragBins; var log: string): Boolean;
  var ldata: ILinkData;
  begin
    ldata := CreateLinkData(name, geoms, verts, frags);
    SendMessage(Handle, WM_LINKPROGRAM, WPARAM(ldata), 0);
    log := ldata.Log;
    Result := ldata.Status = csSuccessed;  
  end;
var sname: string;
    code: string;
    log: string;
    response: ISuperObject; 
    wastarget: Boolean;

    geoms: TGeomBins;
    verts: TVertBins;
    frags: TFragBins;
    i: Integer;
begin
  AddToLog(Format('%s from %s', [ARequestInfo.URI, AContext.Binding.PeerIP]));
  if ARequestInfo.CommandType = hcPOST then
  begin
    if ARequestInfo.URI = '/compile' then
    begin
      code := ARequestInfo.Params.Values['code'];
      AddToLog('code: ' + sLineBreak + code, 1);

      log := '';
      response := SO();
      try
        response.S['status'] := 'ERROR';
        wastarget := False;
        sname := ARequestInfo.Params.Values['filename'];
        if ARequestInfo.Params.Values['geomtarget'] = 'TRUE' then
        begin
          wastarget := True;
          if not CompileAddLog(sname, stGeometry, code, log) then Exit;
          response.S['geom'] := code;
          AddToLog('geom_result: ' + sLineBreak + code, 1);
        end;
        if ARequestInfo.Params.Values['verttarget'] = 'TRUE' then
        begin
          wastarget := True;
          if not CompileAddLog(sname, stVertex, code, log) then Exit;
          response.S['vert'] := code;
          AddToLog('vert_result: ' + sLineBreak + code, 1);
        end;
        if ARequestInfo.Params.Values['fragtarget'] = 'TRUE' then
        begin
          wastarget := True;
          if not CompileAddLog(sname, stFragment, code, log) then Exit;
          response.S['frag'] := code;
          AddToLog('frag_result: ' + sLineBreak + code, 1);
        end;  
        if not wastarget then
        begin
          log := 'Compile target not found';
        end
        else
        begin        
          response.S['status'] := 'OK';
        end;
      finally
        response.S['log'] := log;
        AResponseInfo.ContentText := response.AsJSon(false, false);
      end;
      AddToLog('status: ' + response.S['status']);
      AddToLog('log: ' + response.S['log']);
      Exit;
    end;
    
    if ARequestInfo.URI = '/link' then
    begin      
      log := '';
      response := SO();
      try
        response.S['status'] := 'ERROR';
        SetLength(geoms, StrToIntDef(ARequestInfo.Params.Values['geomcnt'], 0));
        SetLength(verts, StrToInt(ARequestInfo.Params.Values['vertcnt']));
        SetLength(frags, StrToInt(ARequestInfo.Params.Values['fragcnt']));
        AddToLog('geom count: ' + IntToStr(Length(geoms)), 1);
        AddToLog('vert count: ' + IntToStr(Length(verts)), 1);
        AddToLog('frag count: ' + IntToStr(Length(frags)), 1);
        for i := 0 to Length(geoms) - 1 do
        begin
          geoms[i] := ARequestInfo.Params.Values['geom' + IntToStr(i)];
          AddToLog('geom' + IntToStr(i) + ': ' + ARequestInfo.Params.Values['geom' + IntToStr(i)], 1);
        end;
        for i := 0 to Length(verts) - 1 do
        begin
          verts[i] := ARequestInfo.Params.Values['vert' + IntToStr(i)];
          AddToLog('vert' + IntToStr(i) + ': ' + ARequestInfo.Params.Values['vert' + IntToStr(i)], 1);
        end;
        for i := 0 to Length(frags) - 1 do
        begin
          frags[i] := ARequestInfo.Params.Values['frag' + IntToStr(i)];
          AddToLog('frag' + IntToStr(i) + ': ' + ARequestInfo.Params.Values['frag' + IntToStr(i)], 1);
        end;
        log := '';
        if LinkAddLog(ARequestInfo.Params.Values['programname'], geoms, verts, frags, log) then response.S['status'] := 'OK';
      finally
        response.S['log'] := log;
        AResponseInfo.ContentText := response.AsJSon(false, false);      
      end;
      AddToLog('status: ' + response.S['status']);
      AddToLog('log: ' + response.S['log']);
      Exit;
    end;    
    AResponseInfo.ResponseNo := 404;
  end
  else
    AResponseInfo.ResponseNo := 405;
end;

procedure TfrmcGLSL.idleTimerTimer(Sender: TObject);
begin
  TrayIcon.IconIndex := 0;
  idleTimer.Enabled := False;
  If cbIdleClear.Checked Then mmLog.Clear;
end;

procedure TfrmcGLSL.TrayIconClick(Sender: TObject);
begin
  Show;
  SetActiveWindow(Handle);
  SetForegroundWindow(Handle);
end;

procedure TfrmcGLSL.WMCompileShader(var msg: TMessage);
var info: ICompileData;
begin
  TrayIcon.IconIndex := 3;
  info := ICompileData(msg.WParam);
  info.Compile;
  case info.Status of
      csSuccessed: TrayIcon.IconIndex := 1;
      csFailed   : TrayIcon.IconIndex := 2;
  end;
  idleTimer.Enabled := True;
end;

procedure TfrmcGLSL.WMLinkProgram(var msg: TMessage);
var info: ILinkData;
begin
  TrayIcon.IconIndex := 3;
  info := ILinkData(msg.WParam);
  info.Link;
  case info.Status of
      csSuccessed: TrayIcon.IconIndex := 1;
      csFailed   : TrayIcon.IconIndex := 2;
  end;
  idleTimer.Enabled := True;
end;

end.
