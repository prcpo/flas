unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  PairSplitter, StdCtrls, Menus, ActnList, StdActns, ExtCtrls, fpjson,
  jsonparser, pqconnection, sqldb;

type

  { TMainForm }

  TMainForm = class(TForm)
    Memo1: TMemo;
    PageSaveClose: TAction;
    PageClose: TAction;
    DBConn: TPQConnection;
    Images: TImageList;
    PageSave: TAction;
    MainMenu: TMainMenu;
    MenuItemApplication: TMenuItem;
    MenuItemShowConnectProperties: TMenuItem;
    MenuItemFileExit: TMenuItem;
    Pager: TPageControl;
    ShowConnectProperties: TAction;
    ActionList: TActionList;
    FileExit: TFileExit;
    Notes: TMemo;
    SplitterVertical: TPairSplitter;
    SplitterHorizontal: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    SideBottom: TPairSplitterSide;
    SideLeft: TPairSplitterSide;
    SideCenter: TPairSplitterSide;
    StatusBar: TStatusBar;
    PageToolBar: TToolBar;
    TabSheet1: TTabSheet;
    ToolButtonOK: TToolButton;
    ToolButtonClose: TToolButton;
    ToolButtonSave: TToolButton;
    Navigator: TTreeView;
    procedure DBConnAfterConnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure PageCloseExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PageSaveCloseExecute(Sender: TObject);
    procedure PageSaveExecute(Sender: TObject);
    procedure ShowConnectPropertiesExecute(Sender: TObject);
    procedure PageToolBarClick(Sender: TObject);
  private
    { private declarations }
    function GetParamObjectFromJSON(J: TJSONData; S: String) : String;
    function CreateFromJSONLabeledEdit(ParentObj: TObject;
             LastControl: TControl; Nm: String; J: TJSONData): TControl;
    procedure ShowStatus(S: String);
    function StringToJSON(S: String): TJSONData;
    function GetJSONFromDB(SQL: String): TJSONData;
    procedure CreateNodeFromJSON(Tree: TTreeNodes; Node: TTreeNode; J: TJSONData);
  public
    { public declarations }
    procedure ShowPage(N, C, JSON: String);
    procedure ClosePage(Page: TCustomPage);
    procedure ShowNote(L: Integer; C: String);
    procedure CloseAllPages();
    procedure RefreshNavigator();
    function FindPagerTab(N: String): TTabSheet;
    procedure CreatePageControls(Sheet: TTabsheet; JSON: String);
   end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

const
  PageCfgName: String = 'CfgConnectionsPage';
  PageCfgCaption: String = 'Настройка подключения';
  PageCfgJSON: String = '{"login":{"Type":"string","Text":"Имя пользователя","Value":"postgres"}, ' +
    '"password":{"Type":"string","Text":"Пароль","Value":"postgres"}, ' +
    '"DBhost":{"Type":"string","Text":"Сервер БД","Value":"localhost"}, '+
    '"DBName":{"Type":"string","Text":"Имя БД","Value":"flas"}}';
  PageCfgActionsJSON: String = '{"Actions":{"Save":"internal"},{"OK","internal"}}';
  ConnectStatusOff: String = 'Нет подключения';

function TMainForm.GetParamObjectFromJSON(J: TJSONData; S: String) : String;
var
  JR: TJSONData;
begin
  try
    JR :=  J.Items[TJSONObject(J).IndexOfName(S)];
    try
      Result := JR.AsString;
    except
      on EJSON do Result := JR.AsJSON;
    end;
  except
    on EListError do Result := '';
  end;
end;

procedure TMainForm.ShowConnectPropertiesExecute(Sender: TObject);
begin
  ShowPage(PageCfgName, PageCfgCaption, PageCfgJSON);
end;

procedure TMainForm.PageToolBarClick(Sender: TObject);
begin

end;

function TMainForm.CreateFromJSONLabeledEdit(ParentObj: TObject;
  LastControl: TControl; Nm: String; J: TJSONData
  ): TControl;
var
  El: TLabeledEdit;
begin
  try
    El := TLabeledEdit.Create(Self);
    El.Parent := TWinControl(ParentObj);
    El.Name := Nm;
    El.Text:='';
    El.EditLabel.Caption := GetParamObjectFromJSON(J, 'Text');
    El.Text:=GetParamObjectFromJSON(J, 'Value');
    El.LabelPosition:=lpRight;
    El.AnchorToNeighbour(akTop, 0, LastControl);
    Result := El;
  finally
  end;
end;

procedure TMainForm.ShowStatus(S: String);
begin
  StatusBar.Panels.Items[0].Text:=S;
end;

function TMainForm.StringToJSON(S: String): TJSONData;
  function DoParse(P: TJSONParser): TJSONData;
  var
    J, El: TJSONData;
  begin
    Try
      J:=P.Parse;
        ShowNote(20,'Parse succesful. Dumping JSON data : ');
        If Assigned(J) then
          begin
            ShowNote(20,'Returned JSON structure has class : ' + J.ClassName);
            Result := J;
          end
        else
          ShowNote(20,'No JSON data available');
    except
      On E : Exception do
        ShowNote(20,'An Exception occurred when parsing : ' + E.Message);
    end;
  end;

Var
  P : TJSONParser;
begin
  // Create parser with Stream as source.
  P:=TJSONParser.Create(S);
  try
    Result := DoParse(P);
  finally
    FreeAndNil(P);
  end;
end;

function TMainForm.GetJSONFromDB(SQL: String): TJSONData;
var
  S: String;
  J: TJSONData;
  Qry: TSQLQuery;
  Trans: TSQLTransaction;
begin
  ShowNote(20,'Запрос к БД: '+ SQL);
  try
    Qry := TSQLQuery.Create(Self);
    Trans := TSQLTransaction.Create(Self);
    Qry.SQL.Text:=SQL + ' as J;';
    Trans.DataBase:=DBConn;
    Qry.Transaction:= Trans;
    Trans.StartTransaction;
    Qry.Active:=True;
    S := String(Qry.FieldValues['J']);
    ShowNote(20,'... вернул: ' + S);
    Result := StringToJSON(S);
  except
      On E : Exception do
        ShowNote(20,'An Exception occurred when parsing : ' + E.Message);
  end;
end;

procedure TMainForm.CreateNodeFromJSON(Tree: TTreeNodes; Node: TTreeNode;
  J: TJSONData);
var
  i: Integer;
  ModesItem: TJSONData;
  CurNode: TTreeNode;
begin
  for i := 0 to J.Count-1 do
  begin
    //ShowNote(20, 'JSON: ' + GetParamObjectFromJSON(J.Items[i], 'Text'));
    ModesItem.Create;
    ModesItem := J.Items[i];
    if  LowerCase(GetParamObjectFromJSON(J.Items[i], 'Type')) = 'mode' then
      begin
        CurNode := Tree.AddChildObject(Node, GetParamObjectFromJSON(J.Items[i], 'Text'),
          Pointer(ModesItem));
        //if GetParamObjectFromJSON(J.Items[i], 'Actions') > ' ' then
        //  CurNode.ImageIndex:=12;

        CreateNodeFromJSON(Tree, CurNode,
          GetJSONFromDB('select modes_node_children_get(''' + TJSONObject(J).Names[i] + ''')'));
      end;
  end;
end;

procedure TMainForm.ShowPage(N, C, JSON: String);
var
  NewTab: TTabSheet;
begin
  try
    NewTab := TTabSheet.Create(Pager);
    NewTab.Parent:=Pager;
    NewTab.Name:=N;
    CreatePageControls(NewTab, JSON);
    NewTab.Caption:=C;
    Pager.ActivePage:=NewTab;
  except
    on EComponentError do
    begin
      NewTab.Destroy;
      Pager.ActivePage := TTabSheet(Pager.FindChildControl('N'));
      ShowNote(10, 'Лист не создан. Возврат на существующий: ' + N)
    end;
   end;
end;

procedure TMainForm.ClosePage(Page: TCustomPage);
var
  i: Integer;
begin
  for i:= Page.ControlCount-1 downto 0 do
      Page.Controls[i].Destroy;
  Page.Destroy;
end;

procedure TMainForm.ShowNote(L: Integer; C: String);
begin
  if Notes.Tag >= L then
    Notes.Lines.Add(C);
end;

procedure TMainForm.CloseAllPages();
var
  i: Integer;
begin
  if Pager.PageList.Count > 0 then
    begin
      ShowNote(20, 'Подключаюсь заново. Будет закрыто листов: ' + IntToStr(Pager.PageList.Count));
      for i := Pager.PageList.Count-1 downto 0 do
      begin
        ClosePage(Pager.Page[i]);
      end;
    end;
end;

procedure TMainForm.RefreshNavigator();
var
  S: String;
  J: TJSONData;
begin
  Navigator.Items.Clear;
  S := 'select modes_node_children_get()';
  ShowNote(20,'Запрос навигатора: ' + S);
  J := GetJSONFromDB(S);
  CreateNodeFromJSON(Navigator.Items, nil, J);
end;

function TMainForm.FindPagerTab(N: String): TTabSheet;
begin

end;

procedure TMainForm.CreatePageControls(Sheet: TTabSheet; JSON: String);
  procedure ObjectFromJSON(ParentObj: TObject; J: TJSONData);
  var
    i: Integer;
    Oname, Otype: String;
    LastControl: TControl;
  begin
    LastControl := nil;
    try
      for i:=0 to J.Count-1 do
      begin
        OName := TJSONObject(J).Names[i];
        OType := GetParamObjectFromJSON(J.Items[i], 'Type');
        ShowNote(20,'Item #' + IntToStr(i) + ': ' + Oname
          + ' = ' + J.Items[i].AsJSON);

        if not (OType = '') then
        begin
          ShowNote(20, 'Type: ' + Otype);
          if LowerCase(OType) = 'string' then
            LastControl := CreateFromJSONLabeledEdit(ParentObj, LastControl, OName, J.Items[i]);
        end;
      end;
    except
      On E : Exception do
        ShowNote(20,'An Exception occurred when parsing : ' + E.Message);
    end;
  end;


  Procedure ParseString(S : String);
  begin
    ObjectFromJSON(Sheet, StringToJSON(S));
  end;

begin
  ShowNote(20,'JSON: ' + JSON);
  ParseString(JSON);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin

end;

procedure TMainForm.PageSaveCloseExecute(Sender: TObject);
begin
  PageSaveExecute(Sender);
  PageCloseExecute(Sender);
end;

procedure TMainForm.PageSaveExecute(Sender: TObject);
begin
  if Pager.ActivePage.Name = PageCfgName then
    begin
      try
        with DBConn do begin
          Connected:=false;
          UserName:=TEdit(Pager.ActivePage.FindChildControl('login')).Text;
          Password:=TEdit(Pager.ActivePage.FindChildControl('password')).Text;
          HostName:=TEdit(Pager.ActivePage.FindChildControl('DBhost')).Text;
          DatabaseName:=TEdit(Pager.ActivePage.FindChildControl('DBName')).Text;
          Connected:=True;
        end;
        ShowNote(10,'Подключено к БД');
      except
        On E : Exception do begin
          ShowNote(1,'An Exception occurred when parsing : ' + E.Message);
          ShowStatus(ConnectStatusOff);
        end;
      end;
    end;
end;

procedure TMainForm.PageCloseExecute(Sender: TObject);
begin
    if Pager.PageCount > 0 then
       ClosePage(Pager.ActivePage);
end;

procedure TMainForm.DBConnAfterConnect(Sender: TObject);
begin
  ShowStatus('Подключено: ' + DBConn.UserName
          + '@' + DBConn.HostName + '/' + DBConn.DatabaseName);
  CloseAllPages();
  RefreshNavigator();
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ShowConnectPropertiesExecute(Sender);
end;

procedure TMainForm.Memo1Change(Sender: TObject);
begin

end;

end.

