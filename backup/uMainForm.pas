unit uMainForm;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ExtCtrls, ComCtrls, StdCtrls, Buttons, IniFiles,
  uWVBrowser, uWVWinControl, uWVWindowParent, uWVTypes, uWVConstants, uWVTypeLibrary,
  uWVLibFunctions, uWVLoader, uWVInterfaces, uWVCoreWebView2Args,
  uWVBrowserBase, uWVCoreWebView2SharedBuffer,
  Windows;

type

  { TMainForm }

  TMainForm = class(TForm)
    btExit: TBitBtn;
    btReload: TBitBtn;
    btHide: TBitBtn;
    Panel1: TPanel;
    pnStatut: TPanel;
    tiHide: TTimer;
    timerIdle: TTimer;
    tiMinimize: TTimer;
    WVWindowParent1: TWVWindowParent;
    Timer1: TTimer;
    WVBrowser1: TWVBrowser;

    procedure btExitClick(Sender: TObject);
    procedure btHideClick(Sender: TObject);
    procedure btReloadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure pnStatutMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pnStatutMouseEnter(Sender: TObject);
    procedure pnStatutMouseLeave(Sender: TObject);
    procedure pnStatutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure pnStatutMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure tiHideTimer(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SendMsgBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure timerIdleTimer(Sender: TObject);
    procedure tiMinimizeTimer(Sender: TObject);

    procedure WVBrowser1AfterCreated(Sender: TObject);
    procedure WVBrowser1WebMessageReceived(Sender: TObject;
      const aWebView: ICoreWebView2;
      const aArgs: ICoreWebView2WebMessageReceivedEventArgs);
    procedure WVBrowser1InitializationError(Sender: TObject;
      aErrorCode: HRESULT; const aErrorMessage: wvstring);
    procedure PostSharedBufferBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure loadPage();
    procedure stopAppli();
    procedure setSize(Width, Height: integer; bShowButton: boolean);

  protected
    FSharedBuffer: TCoreWebView2SharedBuffer;
    procedure WMMove(var aMessage: TWMMove); message WM_MOVE;
    procedure WMMoving(var aMessage: TMessage); message WM_MOVING;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;
  urlIntranetTimer: string;
  MouseIsDown: boolean;
  PX, PY: integer;
  curWidth, curHeight: integer;
  canMinimize: boolean;
  startX, startY: integer;
  isMinimized: boolean;
  isShown: boolean;
  canReload: boolean;
  lastX, lastY: integer;
  bSizing:boolean;
  logData:TStringList;
  logFile:string;

implementation

{$R *.lfm}

// This demo shows how to send and receive messages between the web browser and the web page.

// JavaScript can send messages with "window.chrome.webview.postMessage"
// The web browser will receive them in the TWVBrowser.OnWebMessageReceived event.

// The web browser can send messages with the TWVBrowser.PostWebMessageAsString function
// JavaScript will receive them in the "message" event listener declared with
// window.chrome.webview.addEventListener

// This demos also shows how to share a buffer between the browser and Delphi.

// Use TWVBrowserBase.CreateSharedBuffer to create a shared buffer :
// https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/icorewebview2environment12?view=webview2-1.0.1661.34#createsharedbuffer

// Use TWVBrowserBase.PostSharedBufferToScript to share a buffer with the browser :
// https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/icorewebview2_17?view=webview2-1.0.1661.34#postsharedbuffertoscript

// For all the information about shared buffers read this document :
// https://learn.microsoft.com/en-us/microsoft-edge/webview2/reference/win32/icorewebview2sharedbuffer

const
  CUSTOM_SHARED_BUFFER_SIZE = 1024;


procedure log(const Msg: string);
begin
  logData.Add(msg);
  logData.SaveToFile(logFile);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  mode: string;
  token: string;
  MyUUID: TGUID;
  idleTime:integer;
begin
  logData:=TStringList.Create;
  logFile:= ExtractFileDir(Application.ExeName)+'\log.txt';
  if (FileExists(logFile) then
    logData.LoadFromFile(logFile);
  isMinimized := False;
  isShown := False;
  lastx := -1;
  lastY := -1;


  FSharedBuffer := nil;
  ini := TIniFile.Create(StringReplace(Application.exename, '.exe', '', []) + '.ini');
  mode := ini.ReadString('main', 'mode', 'prod');

  if (mode = 'dev') then
  begin
    urlIntranetTimer := 'http://127.0.0.1:2001/cohda/root/cohda_root.php';
  end
  else
  begin
    urlIntranetTimer := 'https://intracohda.cohda.fr/cohda/root/cohda_root.php';
  end;

  token := ini.ReadString('main', 'token', '');
  if (token = '') then
  begin
    CreateGUID(MyUUID);
    token := GUIDToString(MyUUID);
    ini.writeString('main', 'token', token);

  end;

  startX := ini.readInteger('main', 'x', -1);
  startY := ini.readInteger('main', 'y', -1);

  canReload := ini.ReadBool('main', 'reload', False);

  idleTime:= ini.readInteger('main', 'idle', 10);
  timerIdle.Interval:=idleTime*1000*60;

  ini.Free;
  urlIntranetTimer := urlIntranetTimer + '?timer=' + token;

  //  urlIntranetTimer

end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  if assigned(FSharedBuffer) then
    FreeAndNil(FSharedBuffer);


  logData.Free;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin

  SetWindowPos(MainForm.Handle, HWND_TOPMOST, 0, 0, 0, 0,
    SWP_NoMove or SWP_NoSize);
  btReload.Visible := False;
  btExit.Visible := False;





  if GlobalWebView2Loader.InitializationError then
    ShowMessage(GlobalWebView2Loader.ErrorMessage)
  else
  if GlobalWebView2Loader.Initialized then
    WVBrowser1.CreateBrowser(WVWindowParent1.Handle)
  else
    Timer1.Enabled := True;


  if ((startx = -1) or (starty = -1)) then
  begin

  end
  else
  begin
    MainForm.Left := startx;
    MainForm.top := startY;
  end;

  curWidth := pnStatut.Width*4;
  curHeight := pnStatut.Width*4;
  setSize(-1, -1, false);

end;

procedure TMainForm.timerIdleTimer(Sender: TObject);
var
  cX, cY: integer;
begin
  if (isMinimized) then
  begin
    cx := Mouse.CursorPos.x;
    cY := Mouse.CursorPos.y;
    if (cX = lastX) and (cY = lastY) then
    begin
      WVBrowser1.PostWebMessageAsString('IDLE');
    end
    else
    begin
      lastX := cX;
      lastY := cY;
    end;

  end;
end;

procedure TMainForm.tiMinimizeTimer(Sender: TObject);
var
  cX, cY: integer;
  pt: TPoint;
begin

  pt := Mouse.CursorPos;

  //  cx:=Mouse.CursorPos.x;
  //cY:=Mouse.CursorPos.y;

  //  if (not isShown) then
  //  if not PtInRect(mainForm.BoundsRect, pt) then
  if not PtInRect(mainForm.BoundsRect, pt) then
  begin

    setSize(pnStatut.Width, pnStatut.Width, False);
    isMinimized := True;

  end;
end;

procedure TMainForm.PostSharedBufferBtnClick(Sender: TObject);
var
  TempDstBuffer: pbyte;
  TempSharedBufferItf: ICoreWebView2SharedBuffer;
  TempText: string;
begin

  (*
  if not (assigned(FSharedBuffer)) and WVBrowser1.CreateSharedBuffer(
    CUSTOM_SHARED_BUFFER_SIZE, TempSharedBufferItf) then
  try
    FSharedBuffer := TCoreWebView2SharedBuffer.Create(TempSharedBufferItf);
  finally
    TempSharedBufferItf := nil;
  end;

  TempText := trim(SharedBufferEdt.Text);
  TempDstBuffer := FSharedBuffer.Buffer;
  FillChar(TempDstBuffer^, CUSTOM_SHARED_BUFFER_SIZE, 0);
  // Copy the UTF8 data to the shared buffer
  move(TempText[1], TempDstBuffer^, length(TempText));
  // Post the shared buffer. The document will receive a "sharedbufferreceived" message.
  WVBrowser1.PostSharedBufferToScript(FSharedBuffer.BaseIntf,
    COREWEBVIEW2_SHARED_BUFFER_ACCESS_READ_WRITE, '');
    *)
end;

procedure TMainForm.SendMsgBtnClick(Sender: TObject);
begin
  //  WVBrowser1.PostWebMessageAsString(Edit1.Text);
end;

procedure TMainForm.WVBrowser1AfterCreated(Sender: TObject);
begin
  loadPage();
end;

procedure TMainForm.loadPage();
var
  TempContent: string;
  uid: string;
begin

  uid := FormatDateTime('yyyy-mm-dd hh:nn:ss', Now);
  WVWindowParent1.UpdateSize;
  Caption := 'Browser Host App Communication example';

  WVBrowser1.Navigate(urlIntranetTimer + '&id=' + uid);
end;

procedure TMainForm.WVBrowser1InitializationError(Sender: TObject;
  aErrorCode: HRESULT; const aErrorMessage: wvstring);
begin
  ShowMessage(aErrorMessage);
end;

function HexToColor(Hex: string): TColor;
var
  R, G, B: byte;
begin
  // Remove the '#' if it exists
  if Hex[1] = '#' then
    Delete(Hex, 1, 1);

  // Convert hex string to RGB values
  R := StrToInt('$' + Copy(Hex, 1, 2));
  G := StrToInt('$' + Copy(Hex, 3, 2));
  B := StrToInt('$' + Copy(Hex, 5, 2));

  // Combine RGB into TColor
  Result := RGB(R, G, B);
end;

procedure SetObjectColor(SomeObject: TShape; HexColor: string);
begin
  SomeObject.Brush.Color := HexToColor(HexColor);
end;

procedure TMainForm.WVBrowser1WebMessageReceived(Sender: TObject;
  const aWebView: ICoreWebView2; const aArgs: ICoreWebView2WebMessageReceivedEventArgs);
var
  TempArgs: TCoreWebView2WebMessageReceivedEventArgs;
  TempMsg: string;
  TempNewBuffer: array[0..CUSTOM_SHARED_BUFFER_SIZE - 1] of widechar;
  statut, sWidth, sHeight, sShow, sColor: string;
  tMessage: array of string;
  color: integer;

begin
  TempArgs := TCoreWebView2WebMessageReceivedEventArgs.Create(aArgs);
  TempMsg := TempArgs.WebMessageAsString;

  log(TempMsg);
  //  MessagesMem.Lines.Add('Message received : ' + TempMsg);

  // The "SharedBufferDataUpdated" message is used to notify the main process that the buffer has been updated.
  if (TempMsg = 'SharedBufferDataUpdated') and assigned(FSharedBuffer) then
  begin
    FillChar(TempNewBuffer, CUSTOM_SHARED_BUFFER_SIZE, 0);
    // Convert the buffer encoded in UTF8 to a unicode string
    UTF8ToUnicode(TempNewBuffer, pansichar(FSharedBuffer.Buffer),
      CUSTOM_SHARED_BUFFER_SIZE);
    //    MessagesMem.Lines.Add('New shared buffer contents : ' + TempNewBuffer);
  end;




  tMessage := TempMsg.split('|');
  statut := tMessage[0];
  if (Length(tMessage) > 1) then
  begin
    sWidth := tMessage[1];
  end;
  if (Length(tMessage) > 2) then
  begin
    sHeight := tMessage[2];
  end;
  if (Length(tMessage) > 3) then
  begin
    sShow := tMessage[3];
  end;

  if (Length(tMessage) > 4) then
  begin
    sColor := tMessage[4];
  end;


  canMinimize := (sShow = '0');

  if (sColor <> '') then
  begin
    pnStatut.Color := HexToColor(sColor);
  end
  else
  begin
    pnStatut.Color := clWhite;
  end;


  if (sWidth <> '') then
  begin
    curWidth := StrToInt(sWidth);
    curHeight := StrToInt(sHeight);
    setSize(curWidth, curHeight, True);
  end;



  if (statut = 'LOGIN') then
  begin

  end;

  if (statut = 'NEW') then
  begin

  end;

  if (statut = 'RUN') then
  begin

  end;

  if (statut = 'IDLE') then
  begin

  end;

  WVWindowParent1.SetFocus;
  TempArgs.Free;

  tiMinimize.Enabled := canMinimize;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := False;

  if GlobalWebView2Loader.Initialized then
    WVBrowser1.CreateBrowser(WVWindowParent1.Handle)
  else
    Timer1.Enabled := True;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  stopAppli();
end;

procedure TMainForm.btExitClick(Sender: TObject);
begin
  stopAppli();
end;

procedure TMainForm.btHideClick(Sender: TObject);
begin
  tiHide.Enabled:=true;
end;

procedure TMainForm.btReloadClick(Sender: TObject);
begin
loadPage();

end;

procedure TMainForm.Button2Click(Sender: TObject);
begin
  loadPage();
end;

procedure TMainForm.pnStatutMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    MouseIsDown := True;
    PX := X;
    PY := Y;
  end;
end;

procedure TMainForm.pnStatutMouseEnter(Sender: TObject);
begin
  if not bsizing then
  begin
  isMinimized := False;
  isShown := True;
  setSize(-1, -1, True);

  end;
end;

procedure TMainForm.pnStatutMouseLeave(Sender: TObject);
begin
  //  isShown := False;
end;

procedure TMainForm.pnStatutMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if MouseIsDown then
  begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
  end;
end;

procedure TMainForm.pnStatutMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  ini: TIniFile;
begin

  startX := MainForm.Left;
  startY := MainForm.top;
  if (startX < 0) then
  begin
    startX := 0;
    MainForm.Left := 0;

  end;
  if (startY < 0) then
  begin
    startY := 0;
    MainForm.top := 0;
  end;

   if (startX + pnStatut.Width> Screen.Width) then
  begin
    startX := Screen.Width-pnStatut.Width;
    MainForm.Left := startX;

  end;
  if (startY + pnStatut.Width> screen.Height) then
  begin
    startY := screen.Height-              pnStatut.Width;
    MainForm.top := startY;
  end;





  ini := TIniFile.Create(StringReplace(Application.exename, '.exe', '', []) + '.ini');
  ini.WriteInteger('main', 'x', startX);
  ini.WriteInteger('main', 'y', startY);
  ini.Free;
  MouseIsDown := False;
end;

procedure TMainForm.tiHideTimer(Sender: TObject);
begin
  tiHide.Enabled:=false;
  isMinimized:=true;
  setSize(pnStatut.Width, pnStatut.Width, False);
end;

procedure TMainForm.WMMove(var aMessage: TWMMove);
begin
  inherited;

  if (WVBrowser1 <> nil) then
    WVBrowser1.NotifyParentWindowPositionChanged;
end;

procedure TMainForm.WMMoving(var aMessage: TMessage);
begin
  inherited;

  if (WVBrowser1 <> nil) then
    WVBrowser1.NotifyParentWindowPositionChanged;
end;

procedure TMainForm.stopAppli();
begin
  Application.Terminate;
end;

procedure TMainForm.setSize(Width, Height: integer; bShowButton: boolean);
begin
  bSizing:=true;
  MainForm.Left := startX;
  MainForm.top := startY;
  if (Width = -1) then
  begin
    MainForm.Width := curWidth;
    MainForm.Height := curHeight;
  end
  else
  begin
    MainForm.Width := Width;
    MainForm.Height := Height;

  end;


  btReload.Visible := (bShowButton and canReload);
  btHide.visible:=bShowButton;
  btExit.Visible := bShowButton;

  //pnButton.Visible:=bShowButton;

  if (not bShowButton) then
  begin
    MainForm.left := startX;
    MainForm.top := startY;
  end
  else
  begin
    if (MainForm.Left + MainForm.Width > Screen.Width) then
    begin
      MainForm.Left := Screen.Width - MainForm.Width;
    end;

    if (MainForm.top + MainForm.Height > Screen.Height) then
    begin
      MainForm.top := Screen.Height - MainForm.Height;
    end;

  end;
  bSizing:=false;

end;



initialization
  GlobalWebView2Loader := TWVLoader.Create(nil);
  GlobalWebView2Loader.UserDataFolder :=
    ExtractFileDir(Application.ExeName) + '\CustomCache';
  GlobalWebView2Loader.StartWebView2;

end.
