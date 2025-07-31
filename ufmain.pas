unit ufmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls, Windows,
  Buttons, StdCtrls, MaskEdit, Grids, DBCtrls, IdHTTP, IdSSLOpenSSL,
  IniFiles
  , fpjson, jsonparser, fpjsondataset, DB;

type

  { TfMain }

  TfMain = class(TForm)
    BitBtn1: TBitBtn;
    btStop: TBitBtn;
    btStopAt: TBitBtn;
    btContinue: TBitBtn;
    btBookmarkImputation: TBitBtn;
    btBookmarkActivity: TBitBtn;
    btStart: TBitBtn;
    btRetry: TBitBtn;
    btMinimize: TBitBtn;
    btExit: TBitBtn;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    Label6: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    lblCurImputation: TLabel;
    lblCurActivity: TLabel;
    lblCurStart: TLabel;
    Label8: TLabel;
    lblCurDuree: TLabel;
    mCurCommentaire: TMemo;
    pnCurImputation: TPanel;
    pcNew: TPageControl;
    pnCurActivity: TPanel;
    pnCurClient: TPanel;
    pnCurProjet: TPanel;
    pnStop: TPanel;
    pnRun: TPanel;
    tiMinimize: TTimer;
    timerIdle: TTimer;
    txtpassword: TEdit;
    vsHistory: TScrollBox;
    tsCreate: TTabSheet;
    tsHistory: TTabSheet;
    txtSearch: TEdit;
    lstImputation: TComboBox;
    lstActivity: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    pnSearch: TPanel;
    Panel3: TPanel;
    sbList: TScrollBox;
    txtLogin: TEdit;
    idIntranet: TIdHTTP;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel8: TPanel;
    pnSizeError: TPanel;
    pnBorder: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    pnSizeNew: TPanel;
    pnSizeTask: TPanel;
    pnSizeLogin: TPanel;
    pnBtn: TPanel;
    pnMove: TPanel;
    pcMain: TPageControl;
    pnSizeStart: TPanel;
    pnTop: TPanel;
    tiPhase: TTimer;
    tsStart: TTabSheet;
    tsTask: TTabSheet;
    tsNew: TTabSheet;
    tsLogin: TTabSheet;
    tsError: TTabSheet;
    procedure BitBtn1Click(Sender: TObject);
    procedure btContinueClick(Sender: TObject);
    procedure btMinimizeClick(Sender: TObject);
    procedure btStopAtClick(Sender: TObject);
    procedure btStopClick(Sender: TObject);
    procedure btBookmarkActivityClick(Sender: TObject);
    procedure btBookmarkImputationClick(Sender: TObject);
    procedure btStartClick(Sender: TObject);
    procedure btRetryClick(Sender: TObject);
    procedure btExitClick(Sender: TObject);

    procedure setMinimized();
    procedure applicationStart();
    procedure IdleTimer1Timer(Sender: TObject);
    procedure pcNewChange(Sender: TObject);
    procedure pnMoveMouseEnter(Sender: TObject);
    procedure pnMoveMouseLeave(Sender: TObject);
    procedure pnSizeNewClick(Sender: TObject);
    procedure setPhase(setphase: string; delai: integer = 10);
    procedure applicationPhase();
    procedure applicationStop();
    procedure FormCreate(Sender: TObject);

    procedure FormShow(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure loadConfig();
    procedure lstImputationChange(Sender: TObject);
    procedure Panel5Click(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: boolean);
    procedure pnMoveMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pnMoveMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure pnMoveMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure setSize();
    procedure timerIdleTimer(Sender: TObject);
    procedure tiMinimizeTimer(Sender: TObject);
    procedure tiPhaseTimer(Sender: TObject);
    procedure txtSearchChange(Sender: TObject);
    procedure txtSearchKeyPress(Sender: TObject; var Key: char);
    procedure txtSearchMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private

  public

  end;

type
  tConfig = record
    token: string;
    url: string;
    idleInterval: integer;
    imputation: integer;
    activity: string;
    startX: integer;
    startY: integer;
  end;

var
  fMain: TfMain;
  config: tConfig;
  MouseIsDown: boolean;
  PX, PY: integer;

  ini: TIniFile;
  phase: string;

  tabActivity: TStringList;
  tabSearch: TStringList;
  idObjet: integer;
  idCurTask: integer;

  lastX, lastY: integer;
  lastTime: TDateTime;



const
  refreshInterval = 30000;
  keyInterval = 300;




implementation

{$R *.lfm}

{ TfMain }

procedure TfMain.btExitClick(Sender: TObject);
begin
  if (phase='refresh') then
  begin
       ShowMessage('Avant de fermer vous devez terminer la tâche en cours');
  end
  else
  begin
       applicationStop();

  end;
end;

procedure TfMain.btRetryClick(Sender: TObject);
begin
  pcMain.ActivePage := tsStart;
  setSize();
  tiPhase.Enabled := True;
end;

procedure TfMain.BitBtn1Click(Sender: TObject);
begin
  phase := 'login';
  tiPhase.Enabled := True;
end;

procedure TfMain.btContinueClick(Sender: TObject);
begin
  setPhase('refresh', refreshInterval);
end;

procedure TfMain.btMinimizeClick(Sender: TObject);
begin
  setminimized();
end;

procedure tfmain.setMinimized();
begin
  Self.Width := pnTop.Height;
  Self.Height := pnTop.Height;
  self.Left := config.startX;
  self.Top := config.startY;
  pnBtn.Visible := False;
end;

procedure TfMain.btStopAtClick(Sender: TObject);
begin
  setPhase('stopAt');
end;

procedure TfMain.btStopClick(Sender: TObject);
begin
  setPhase('stop');
end;

procedure TfMain.btBookmarkActivityClick(Sender: TObject);
begin
  config.activity := tabActivity.ValueFromIndex[lstActivity.ItemIndex];
  ini := TIniFile.Create(StringReplace(Application.exename, '.exe', '', []) + '.ini');
  ini.WriteString('main', 'activity', config.activity);

  ini.Free;
end;

procedure TfMain.btBookmarkImputationClick(Sender: TObject);
begin
  config.imputation := lstImputation.ItemIndex + 1;

  ini := TIniFile.Create(StringReplace(Application.exename, '.exe', '', []) + '.ini');
  ini.WriteInteger('main', 'imputation', config.imputation);

  ini.Free;
end;

procedure TfMain.btStartClick(Sender: TObject);
var
  bt: TButton;
  nom: string;
  tab: TStringArray;
begin

  nom := TButton(Sender).Name;
  if (nom = 'btStart') then
  begin
    idObjet := -1;
    setPhase('create');
  end
  else
  begin
    tab := nom.Split('_');
    if (tab[0] = 'clone') then
    begin
      idObjet := StrToInt(tab[1]);
      setPhase('clone');
    end
    else
    begin
      idObjet := StrToInt(tab[1]);
      setPhase('create');
    end;
  end;

end;

procedure TfMain.applicationStart();
begin

  tabActivity := TStringList.Create;
  tabSearch := TStringList.Create;
  lastx := -1;
  lastY := -1;
  timerIdle.Enabled := False;



  loadConfig();
  setPhase('new');

end;

procedure TfMain.IdleTimer1Timer(Sender: TObject);
begin

end;

procedure TfMain.pcNewChange(Sender: TObject);
begin
  if pcNew.ActivePage = tsHistory then
  begin
    setPhase('history');
  end;
end;

procedure TfMain.pnMoveMouseEnter(Sender: TObject);
begin
  if (not MouseIsDown) then
  begin
    setSize();
  end;
end;

procedure TfMain.pnMoveMouseLeave(Sender: TObject);
begin
  tiMinimize.Enabled := True;
end;

procedure TfMain.pnSizeNewClick(Sender: TObject);
begin

end;


procedure TfMain.setphase(setphase: string; delai: integer = 10);
begin

  phase := setphase;
  tiPhase.Interval := delai;
  tiPhase.Enabled := True;

  if (phase = 'create') then
  begin
    timerIdle.Enabled := True;
  end
  else if (timerIdle.Enabled) and (phase <> 'refresh') then
  begin
    timerIdle.Enabled := False;
  end
  else if (not timerIdle.Enabled) and (phase = 'refresh') then
  begin
    timerIdle.Enabled := True;
  end;

  if (phase = 'new') then
  begin
    pcNew.ActivePage := tsCreate;
    txtSearch.Text := '';
    lstImputation.ItemIndex := config.imputation - 1;
    lstImputationChange(lstImputation);

  end;

  if (phase = 'refresh') then
  begin
    btStop.Visible := True;
    btStopAt.Visible := False;
    btContinue.Visible := False;

  end;

end;

procedure TfMain.applicationPhase();
var
  PostData: TStringList;
  res: string;
  J, TMPJSON, TMPJSON1: TJSONData;
  l, q, r: integer;
  code: integer;
  message: string;
  id: integer;
  Lib: string;
  panel: TPanel;
  btn: TBitBtn;
  jsInt: integer;
  jsString: string;
  idxActivity: integer;
begin

  PostData := TStringList.Create;
  PostData.Add('token=' + config.token);
  PostData.Add('action=' + phase);
  if (phase = 'start') then
  begin

  end;

  if (phase = 'login') then
  begin
    PostData.Add('login=' + txtLogin.Text);
    PostData.Add('password=' + txtPassword.Text);

  end;


  if (phase = 'create') then
  begin
    PostData.Add('idObjet=' + IntToStr(idObjet));
    PostData.Add('imputation=' + IntToStr(lstImputation.ItemIndex + 1));

    if (lstActivity.ItemIndex <> -1) then
    begin
      PostData.Add('idActivity=' + tabActivity.ValueFromIndex[lstActivity.ItemIndex]);

    end
    else
    begin
      ShowMessage('Le type de tâche est obligatoire');
      exit;
    end;

  end;


  if (phase = 'clone') then
  begin
    PostData.Add('idTask=' + IntToStr(idObjet));
    PostData.Add('imputation=' + IntToStr(lstImputation.ItemIndex + 1));

    if (lstActivity.ItemIndex <> -1) then
    begin
      PostData.Add('idActivity=' + tabActivity.ValueFromIndex[lstActivity.ItemIndex]);

    end
    else
    begin
      ShowMessage('Le type de tâche est obligatoire');
      exit;
    end;

  end;

  if (phase = 'search') then
  begin

    if (lstImputation.ItemIndex = 1) then
    begin
      PostData.Add('mode=entreprise');
    end
    else
    begin
      PostData.Add('mode=projet');
    end;

    PostData.Add('search=' + txtSearch.Text);

  end;

  if ((phase = 'refresh') or (phase = 'stop')) then
  begin

    PostData.Add('idTask=' + IntToStr(idCurTask));
    PostData.Add('commentaire=' + mCurCommentaire.Text);

  end;


  if (phase = 'stopAt') then
  begin

    PostData.Add('idTask=' + IntToStr(idCurTask));
    PostData.Add('hFin=' + FormatDateTime('hh:nn', lastTime));
    PostData.Add('commentaire=' + mCurCommentaire.Text);

  end;



  try

    res := idIntranet.Post(config.url, PostData);
  except
    on E: Exception do
    begin
      pcMain.ActivePage := tsError;
      setSize();
      Exit;

    end;
  end;

  try
    J := GetJSON(res);
  except
    on E: Exception do
    begin
      ShowMessage(e.Message);
      ShowMessage(res);

    end;
  end;

  code := J.FindPath('error.code').AsInteger;
  message := J.FindPath('error.message').AsString;
  if (code <> 0) then
  begin
    //ShowMessage(message);
  end;

  case Code of
    1, 2, 3:
    begin
      pcMain.ActivePage := tsLogin;
      setSize();
      exit;
    end;
  end;

  if (phase = 'login') then
  begin
    setPhase('new');
    exit;
  end;


  if (phase = 'new') then
  begin
    pcMain.ActivePage := tsNew;
    sbList.Visible := False;
    for l := sbList.ComponentCount - 1 downto 0 do
    begin
      if sbList.Components[l] is TPanel then
        sbList.Components[l].Free;
    end;
    sbList.Visible := True;
    setSize();


    TMPJSON := J.FindPath('activity');
    //    JSONDataSet1.load;

    l := TMPJSON.Count - 1;
    q := 0;
    lstActivity.Clear;
    tabActivity.Clear;
    idxActivity := -1;
    for r := 0 to l do
    begin
      if (TMPJSON.Items[r].FindPath('code').AsString = config.activity) then
      begin
        idxActivity := r;
      end;

      tabActivity.Values[IntToStr(r)] := TMPJSON.Items[r].FindPath('code').AsString;
      Lib := TMPJSON.Items[r].FindPath('lib').AsString;

      lstActivity.Items.Add(lib);
    end;
    lstActivity.ItemIndex := idxActivity;

  end;




  if (phase = 'search') then
  begin

    sbList.Visible := False;

    TMPJSON := J.FindPath('list');
    l := TMPJSON.Count - 1;
    q := 0;




    for r := l downto 0 do
    begin

      Lib := TMPJSON.Items[r].FindPath('affichage').AsString;

      panel := TPanel.Create(sbList);
      panel.Parent := sbList;
      panel.Caption := StringReplace(lib, '<br>', chr(13) + chr(10), [rfReplaceAll]);
      panel.Align := alTop;
      panel.Alignment := taLeftJustify;
      panel.VerticalAlignment := taAlignTop;
      panel.WordWrap := True;
      panel.Height := 50;

      btn := TBitBtn.Create(panel);
      btn.Parent := panel;
      btn.Align := alRight;
      btn.Kind := btStart.Kind;

      btn.Width := 38;
      btn.onClick := btStart.OnClick;
      btn.Name := 'list_' + TMPJSON.Items[r].FindPath('id').AsString;
      btn.Caption := '';
      sbList.Visible := True;
      //      mCurCommentaire.Lines.Add(lib);
    end;

  end;

  if (phase = 'history') then
  begin

    vsHistory.Visible := False;
    for l := vsHistory.ComponentCount - 1 downto 0 do
    begin
      if vsHistory.Components[l] is TPanel then
        vsHistory.Components[l].Free;
    end;


    TMPJSON := J.FindPath('list');
    l := TMPJSON.Count - 1;
    q := 0;




    for r := l downto 0 do
    begin

      Lib := TMPJSON.Items[r].FindPath('affichage').AsString;

      panel := TPanel.Create(vsHistory);
      panel.Parent := vsHistory;
      panel.Caption := StringReplace(lib, '<br>', chr(13) + chr(10), [rfReplaceAll]);
      panel.Align := alTop;
      panel.Alignment := taLeftJustify;
      panel.VerticalAlignment := taAlignTop;
      panel.WordWrap := True;
      panel.Height := 50;

      btn := TBitBtn.Create(panel);
      btn.Parent := panel;
      btn.Align := alRight;
      btn.Kind := btStart.Kind;

      btn.Width := 38;
      btn.onClick := btStart.OnClick;
      btn.Name := 'clone_' + TMPJSON.Items[r].FindPath('id').AsString;
      btn.Caption := '';

      //      mCurCommentaire.Lines.Add(lib);
      vsHistory.Visible := True;
    end;

  end;

  if ((phase = 'create') or (phase = 'clone')) then
  begin
    pcMain.ActivePage := tsTask;
    setSize();
    idCurTask := J.FindPath('task.id').AsInteger;
    lblCurImputation.Caption :=
      lstImputation.Items.ValueFromIndex[J.FindPath('task.imputation').AsInteger - 1];
    lblCurActivity.Caption := J.FindPath('task.libtask').AsString;
    pnCurClient.Caption := J.FindPath('task.entreprise').AsString;
    pnCurProjet.Caption := J.FindPath('task.proposition').AsString;
    lblCurStart.Caption := J.FindPath('task.hdebut').AsString;
    lblCurDuree.Caption := J.FindPath('task.duree').AsString;
    mCurCommentaire.Text := J.FindPath('task.commentaire').AsString;
    setPhase('refresh', refreshInterval);

  end;


  if ((phase = 'stop') or (phase = 'stopAt')) then
  begin

    setPhase('new');
  end;


  if (phase = 'refresh') then
  begin
    lblCurDuree.Caption := J.FindPath('task.duree').AsString;
    setPhase('refresh', refreshInterval);
  end;

  PostData.Free;

end;

procedure TfMain.applicationStop();
begin
  tabActivity.Free;
  tabSearch.Free;


  Application.Terminate;
end;

procedure TfMain.FormCreate(Sender: TObject);
begin

end;


procedure TfMain.FormShow(Sender: TObject);
begin
  SetWindowPos(self.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NoMove or SWP_NoSize);
  pcNew.OwnerDraw := True;
  //  pcNew.IsParentColor :=true;
  pcMain.ShowTabs := False;
  pcMain.ActivePage := tsStart;
  setSize();
  applicationStart();
end;

procedure TfMain.LabeledEdit1Change(Sender: TObject);
begin

end;

procedure TfMain.loadConfig();
var

  MyUUID: TGUID;
  isDev: boolean;
begin
  ini := TIniFile.Create(StringReplace(Application.exename, '.exe', '', []) + '.ini');
  config.token := ini.ReadString('main', 'token', '');
  config.idleInterval := ini.ReadInteger('main', 'idle', 1000 * 60 * 10);
  timerIdle.Interval := config.idleInterval;
  config.imputation := ini.ReadInteger('main', 'imputation', 1);

  config.startX := ini.ReadInteger('main', 'x', 0);
  config.startY := ini.ReadInteger('main', 'y', 0);
  self.Left := config.startX;
  self.top := config.startY;

  config.activity := ini.ReadString('main', 'activity', '-1');


  isDev := ini.ReadBool('main', 'dev', False);
  if (isDev) then
  begin
    config.url := 'http://127.0.0.1:2001/cohda/ext/cohdaTimer/api.php';

  end
  else
  begin
    config.url := 'https://intracohda.cohda.fr/cohda/ext/cohdaTimer/api.php';

  end;

  if (config.token = '') then
  begin
    CreateGUID(MyUUID);
    config.token := GUIDToString(MyUUID);
    ini.writeString('main', 'token', config.token);

  end;

  ini.Free;

end;

procedure TfMain.lstImputationChange(Sender: TObject);
begin
  if (lstImputation.ItemIndex = 0) then
  begin
    pnSearch.Visible := False;
    pnRun.Visible := True;
    sbList.Visible := False;
  end;

  if (lstImputation.ItemIndex = 1) then
  begin
    pnRun.Visible := False;
    pnSearch.Visible := True;
    txtSearch.TextHint := 'Chercher un client';
    txtSearch.Text := '';
    sbList.Visible := True;
  end;
  if (lstImputation.ItemIndex = 2) then
  begin
    pnRun.Visible := False;
    pnSearch.Visible := True;
    txtSearch.TextHint := 'Chercher un projet';
    txtSearch.Text := '';
    sbList.Visible := True;
  end;
end;

procedure TfMain.Panel5Click(Sender: TObject);
begin

end;

procedure TfMain.pcMainChange(Sender: TObject);
begin

  setSize();
end;

procedure TfMain.pcMainChanging(Sender: TObject; var AllowChange: boolean);
begin
  ShowMessage('aaaaa');
end;

procedure TfMain.pnMoveMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    MouseIsDown := True;
    PX := X;
    PY := Y;
  end;
end;

procedure TfMain.pnMoveMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin

  if MouseIsDown then
  begin
    SetBounds(Left + (X - PX), Top + (Y - PY), Width, Height);
  end;
end;

procedure TfMain.pnMoveMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin

  config.startX := self.Left;
  config.startY := self.top;
  if (config.startX < 0) then
  begin
    config.startX := 0;
  end;
  if (config.startX + pnTop.Height > Screen.Width) then
  begin
    config.startX := Screen.Width - pnTop.Height;
  end;
  if (config.startY < 0) then
  begin
    config.startY := 0;
  end;

  if (config.startY + pntop.Height > Screen.Height) then
  begin
    config.startY := Screen.Height - pntop.Height;
  end;

  self.Left := config.startX;
  self.top := config.startY;

  ini := TIniFile.Create(StringReplace(Application.exename, '.exe', '', []) + '.ini');
  ini.WriteInteger('main', 'x', config.startX);
  ini.WriteInteger('main', 'y', config.startY);
  ini.Free;
  MouseIsDown := False;

  if (self.left + self.Width) > Screen.Width then
  begin
    self.Left := Screen.Width - self.Width;
  end;
  if (self.top + self.Height) > Screen.Height then
  begin
    self.top := Screen.Height - self.Height;
  end;

end;

procedure TfMain.setSize();
var
  sizer: TPanel;
begin
  pnBtn.Visible := True;
  if (pcMain.ActivePage = tsStart) then
  begin
    pnMove.Caption := 'Démarrage en cours';
    sizer := pnSizeStart;
  end;
  if (pcMain.ActivePage = tsError) then
  begin
    pnMove.Caption := 'Erreur';
    sizer := pnSizeError;
  end;
  if (pcMain.ActivePage = tsLogin) then
  begin
    pnMove.Caption := 'Connexion à l''intranet';
    sizer := pnSizeLogin;
  end;
  if (pcMain.ActivePage = tsNew) then
  begin
    pnMove.Caption := 'Aucune tâche en cours';
    sizer := pnSizeNew;
  end;
  if (pcMain.ActivePage = tsTask) then
  begin
    sizer := pnSizeTask;
    if phase = 'idle' then
    begin
      pnMove.Caption := 'Tâche en cours, mais poste inactif';

      pnMove.Color := $000080FF;
    end
    else
    begin
      pnMove.Caption := '';
      pnMove.Color := clLime;
    end;
  end
  else
  begin
    pnMove.Color := clRed;
  end;


  //a
  fMain.Height := pnTop.Height + sizer.Height + pnBorder.BorderWidth *
    2 + pcMain.BorderSpacing.Around * 2 + 4;
  fMain.Width := sizer.Width + pnBorder.BorderWidth * 2 +
    pcMain.BorderSpacing.Around * 2 + 4;
  application.ProcessMessages;


  if (self.left + self.Width) > Screen.Width then
  begin
    self.Left := Screen.Width - self.Width;
  end;
  if (self.top + self.Height) > Screen.Height then
  begin
    self.top := Screen.Height - self.Height;
  end;
end;

procedure TfMain.timerIdleTimer(Sender: TObject);
var
  cX, cY: integer;
begin

  cx := Mouse.CursorPos.x;
  cY := Mouse.CursorPos.y;
  if (cX = lastX) and (cY = lastY) then
  begin
    tiPhase.Enabled := False;
    timerIdle.Enabled := False;
    lastTime := Now;
    btStopAt.Caption := 'Arrêter à : ' + FormatDateTime('hh:nn', lastTime);
    //    setPhase('idle');
    phase := 'idle';
    btStop.Visible := False;
    btStopAt.Visible := True;
    btContinue.Visible := True;
    setSize();
  end
  else
  begin
    lastX := cX;
    lastY := cY;
  end;

end;

procedure TfMain.tiMinimizeTimer(Sender: TObject);
var

  pt: TPoint;
begin
  if (phase = 'refresh') then
  begin
    pt := Mouse.CursorPos;

    if not PtInRect(self.BoundsRect, pt) then
    begin

      setMinimized();

    end;

  end;
end;

procedure TfMain.tiPhaseTimer(Sender: TObject);
begin
  tiPhase.Enabled := False;
  applicationPhase();
end;

procedure TfMain.txtSearchChange(Sender: TObject);
begin

end;

procedure TfMain.txtSearchKeyPress(Sender: TObject; var Key: char);
begin
  if (txtSearch.Text <> '') then
  begin
    tiPhase.Enabled := False;
    setPhase('search', keyInterval);
  end;
end;

procedure TfMain.txtSearchMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin

end;

end.
