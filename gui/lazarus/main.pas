{%RunWorkingDir /mnt/data/projects/flas.work}
unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  PairSplitter, StdCtrls, Menus, ActnList, StdActns, ExtCtrls, Buttons, EditBtn,
  Grids, DBGrids, fpjson, jsonparser, pqconnection, sqldb, db, BufDataset,
  SdfData, memds, types;

type

  { TMainForm }

  TMainForm = class(TForm)
    NotesHide: TAction;
    Memo1: TMemo;
    Notes: TMemo;
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
    DebugTrackBar: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure NavigatorDblClick(Sender: TObject);
    procedure NotesHideExecute(Sender: TObject);
    procedure PageCloseExecute(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PagerCloseTabClicked(Sender: TObject);
    procedure PageSaveCloseExecute(Sender: TObject);
    procedure PageSaveExecute(Sender: TObject);
    procedure PageSheetDBContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure ShowConnectPropertiesExecute(Sender: TObject);
    procedure PageToolBarClick(Sender: TObject);
    procedure CreateNewDB(Sender: TObject);
    procedure OpenMode(Mode: TTreeNode);
  private
    { private declarations }
    function GetParamObjectFromJSON(J: TJSONData; S: String) : TJSONData;
    function CreateFromJSONLabeledEdit(ParentObj: TObject;
             LastControl: TControl; Nm: String; J: TJSONData): TControl;
    function CreateFromJSONCheckBox(ParentObj: TObject;
             LastControl: TControl; Nm: String; J: TJSONData): TControl;
    function CreateFromJSONTable(ParentObj: TObject;
             LastControl: TControl; Nm: String; J: TJSONData): TControl;
    procedure PopulateTable(El: TDBGrid);
    procedure ShowStatus(S: String);
    function StringToJSON(S: String): TJSONData;
    function GetJSONFromDB(SQL: String): TJSONData;
    procedure CreateNodeFromJSON(Tree: TTreeNodes; Node: TTreeNode; J: TJSONData);
    procedure AfterDBConnect(Sender: TObject);
  public
    { public declarations }
    procedure ShowPage(N, C, JSON: String);
    procedure ClosePage(Page: TCustomPage);
    procedure ShowNote(L: Integer; C: String);
    procedure CloseAllPages();
    procedure RefreshNavigator();
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
    '"DBName":{"Type":"string","Text":"Имя БД","Value":"flas"},' +
    '"DBCreateNew":{"Type":"check","Text":"Создать новую базу данных","Value":false}}';
  PageCfgActionsJSON: String = '{"Actions":{"Save":"internal"},{"OK","internal"}}';
  ConnectStatusOff: String = 'Нет подключения';

function TMainForm.GetParamObjectFromJSON(J: TJSONData; S: String) : TJSONData;
var
  JR: TJSONData;
begin
  try
    JR :=  J.Items[TJSONObject(J).IndexOfName(S)];
    try
      Result := JR;
    except
      on EJSON do Result := JR;
    end;
  except
    on EListError do Result := nil;
  end;
end;

procedure TMainForm.ShowConnectPropertiesExecute(Sender: TObject);
begin
  ShowPage(PageCfgName, PageCfgCaption, PageCfgJSON);
end;

procedure TMainForm.PageToolBarClick(Sender: TObject);
begin

end;

procedure TMainForm.CreateNewDB(Sender: TObject);
var
  Script: TStringList;
  Trans: TSQLTransaction;
begin
  Script:=TStringList.Create;
  try
    DBConn.CreateDB;
    ShowNote(0,'База данных создана успешно.');
    Script.LoadFromFile('db_create.sql');
   except
     On E : Exception do
       ShowNote(0,'An Exception occurred when parsing : ' + E.Message);
   end;
   Trans := TSQLTransaction.Create(Self);
   DBConn.Transaction := Trans;
  DBConn.Connected:=true;
  ShowNote(1,'Подключено к новой БД.');
  ShowNote(1,'Создаю структуру базы данных.');
  DBConn.ExecuteDirect(Script.Text);
  DBConn.Transaction.CommitRetaining;
  Script.Text:='alter database ' + DBConn.DatabaseName + ' set search_path to public, ext;';
  DBConn.ExecuteDirect(Script.Text);
  DBConn.Transaction.CommitRetaining;

  Script.LoadFromFile('init.sql');
  ShowNote(1,'Заполняю базу первичными данными.');
  DBConn.ExecuteDirect(Script.Text);
  DBConn.Transaction.CommitRetaining;
  ShowNote(1,'Структура базы данных создана успешно.');
  DBConn.Transaction.EndTransaction;
  DBConn.Transaction := nil;
  FreeAndNil(Trans);
  FreeAndNil(Script);
end;

procedure TMainForm.OpenMode(Mode: TTreeNode);
var
  J: TJSONObject;
  Ja, Jao: TJSONData;
  i: Integer;
begin
  J := TJSONObject(Mode.Data);
  ShowNote(10,'JSON выбранного: ' + J.AsJSON);
  Ja := GetParamObjectFromJSON(J.Objects[J.Names[0]] , 'Actions');
  ShowNote(10,'Действия: ' + Ja.AsJSON);
  Jao := GetParamObjectFromJSON(Ja, 'open');
  if not (Jao = nil) then
     ShowPage(J.Names[0], Mode.Text,
        GetJSONFromDB('select ' + GetParamObjectFromJSON(Jao, 'Function').AsString
              + '(''' + J.Names[0] + ''')').AsJSON);
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
    El.EditLabel.Caption := GetParamObjectFromJSON(J, 'Text').AsString;
    El.Text:=GetParamObjectFromJSON(J, 'Value').AsString;
    El.LabelPosition:=lpRight;
    El.AnchorToNeighbour(akTop, 0, LastControl);
    Result := El;
  finally
  end;
end;

function TMainForm.CreateFromJSONCheckBox(ParentObj: TObject;
  LastControl: TControl; Nm: String; J: TJSONData): TControl;
var
  El: TCheckBox;
begin
  try
    El := TCheckBox.Create(Self);
    El.Parent := TWinControl(ParentObj);
    El.Name := Nm;
    El.Caption := GetParamObjectFromJSON(J, 'Text').AsString;
    El.Checked := GetParamObjectFromJSON(J, 'Value').AsBoolean;
    El.AnchorToNeighbour(akTop, 0, LastControl);
    Result := El;
  finally
  end;

end;

function TMainForm.CreateFromJSONTable(ParentObj: TObject;
  LastControl: TControl; Nm: String; J: TJSONData): TControl;
var
  El: TDBGrid;
  ElSource: TDataSource;
  ElDataSet: TMemDataset;
  i: Integer;
  Jc: TJSONData;
  colSize: Integer;
  CurField: TFieldDefs;
begin
  try
    El := TDBGrid.Create(Self);
    ElSource := TDataSource.Create(El);
    ElDataSet := TMemDataSet.Create(El);
    El.Parent := TWinControl(ParentObj);
    El.Name := Nm;
    El.AnchorToNeighbour(akTop, 0, LastControl);
    El.DataSource:=ElSource;
    El.Align := alClient;
    Jc:=GetParamObjectFromJSON(J,'Columns');
    ShowNote(10,'Столбцы: ' + Jc.AsJSON);
    colSize := 12;
    for i := 0 to Jc.Count-1 do begin
      ShowNote(10,'Столбец #' + IntToStr(i) + ': ' + TJSONObject(Jc).Names[i] + ' = ' +
      GetParamObjectFromJSON(Jc.Items[i], 'Text').AsString);
//      if  TJSONObject(Jc).Names[i] = 'ID' then colSize := 0 else colSize := 10;
      ElDataSet.FieldDefs.Add(GetParamObjectFromJSON(Jc.Items[i], 'Text').AsString, ftString, colSize);
//      ElDataSet.FieldDefs[ElDataSet.FieldDefs.Count-1].DisplayName :=GetParamObjectFromJSON(Jc.Items[i], 'Text').AsString;
      ElDataSet.FieldDefs[ElDataSet.FieldDefs.Count-1].Name :=TJSONObject(Jc).Names[i];
    end;
//    ElDataSet.CreateTable;
    ElSource.DataSet:= ElDataSet;
    ElDataSet.Open;
    PopulateTable(El);
    El.ScrollBars:=ssAutoBoth;
    Result := El;
  finally
    FreeAndNil(Jc);
  end;
end;

procedure TMainForm.PopulateTable(El: TDBGrid);
var
  col, row: Integer;
  J: TJSONData;
begin
  J := GetJSONFromDB('select doc_list_get(''doc.pay'')');
  for row := 0 to J.Count -1 do begin
    El.DataSource.DataSet.Append;
    for col:=0 to El.Columns.Count-1 do begin
      TBufDataSet(El.DataSource.DataSet).Fields[col].AsString :=
        GetParamObjectFromJSON(J.Items[row], TBufDataSet(El.DataSource.DataSet).Fields[col].FieldName).AsString;
    end;
    TBufDataSet(El.DataSource.DataSet).Post;
    ShowNote(10,'Field: ' + TBufDataSet(El.DataSource.DataSet).Fields[0].FieldName +
                        ' has value ' + TBufDataSet(El.DataSource.DataSet).Fields[0].AsString);
  end;
  El.DataSource.DataSet.First;
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
        ShowNote(10,'Parse succesful. Dumping JSON data : ' + J.AsJSON);
        If Assigned(J) then
          begin
            ShowNote(10,'Returned JSON structure has class : ' + J.ClassName);
            Result := J;
          end
        else
          ShowNote(10,'No JSON data available');
    except
      On E : Exception do
        ShowNote(10,'An Exception occurred when parsing : ' + E.Message);
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
  ShowNote(10,'Запрос к БД: '+ SQL);
  Qry := TSQLQuery.Create(Self);
  Trans := TSQLTransaction.Create(Self);
  try
    Qry.SQL.Text:=SQL + ' as J;';
    Trans.DataBase:=DBConn;
    Qry.Transaction:= Trans;
    Trans.StartTransaction;
    Qry.Active:=True;
    S := String(Qry.FieldValues['J']);
    ShowNote(10,'... вернул: ' + S);
    Result := StringToJSON(S);
  except
      On E : Exception do
        ShowNote(10,'An Exception occurred when parsing : ' + E.Message);
  end;
  Trans.Free;
  Qry.Free;
end;

procedure TMainForm.CreateNodeFromJSON(Tree: TTreeNodes; Node: TTreeNode;
  J: TJSONData);
var
  i: Integer;
  ModesItem: TJSONObject;
  CurNode: TTreeNode;
begin
  ShowNote(10,'Count: ' + IntToStr(J.Count));
  if J.Count > 0 then
  for i := 0 to J.Count-1 do
  begin
    ModesItem := TJSONObject.Create();
    ModesItem.Add(TJSONObject(J).Names[i],J.Items[i]);
    try
        if  LowerCase(GetParamObjectFromJSON(J.Items[i], 'Type').AsString) = 'mode' then
          begin
            CurNode := Tree.AddChild(Node, GetParamObjectFromJSON(J.Items[i], 'Text').AsString);
            CurNode.Data :=  ModesItem;
            ShowNote(10,'Создана ветвь: ' + ModesItem.AsJSON);
            //if GetParamObjectFromJSON(J.Items[i], 'Actions') > ' ' then
            //  CurNode.ImageIndex:=12;

            CreateNodeFromJSON(Tree, CurNode,
              GetJSONFromDB('select modes_node_children_get(''' + TJSONObject(J).Names[i] + ''')'));
          end;
    except
      On E : Exception do
        ShowNote(10,'An Exception occurred when parsing : ' + E.Message);
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
    //В БД код объекта может включать точку. Заменяем на два подчеркивания.
    NewTab.Name:=StringReplace(N,'.','__',[rfReplaceAll]);
    CreatePageControls(NewTab, JSON);
    NewTab.Caption:=C;
    Pager.ActivePage:=NewTab;
  except
    on EComponentError do
    begin
      NewTab.Destroy;
      Pager.ActivePage := TTabSheet(Pager.FindChildControl('N'));
      ShowNote(9, 'Лист не создан. Возврат на существующий: ' + N)
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
  if DebugTrackBar.Position >= L then begin
    Notes.Lines.Add(':: '+FormatDateTime('hh:mm:ss.zzz',Now()) + ' : ' + C);
    //Notes.Lines.Add(C);
    Notes.Refresh;
  end;
end;

procedure TMainForm.CloseAllPages();
var
  i: Integer;
begin
  if Pager.PageList.Count > 0 then
    begin
      ShowNote(10, 'Подключаюсь заново. Будет закрыто листов: ' + IntToStr(Pager.PageList.Count));
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
  ShowNote(10,'Запрос навигатора: ' + S);
  J := GetJSONFromDB(S);
  CreateNodeFromJSON(Navigator.Items, nil, J);
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
        OType := GetParamObjectFromJSON(J.Items[i], 'Type').AsString;
        ShowNote(10,'Item #' + IntToStr(i) + ': ' + Oname
          + ' = ' + J.Items[i].AsJSON);

        if not (OType = '') then
        begin
          ShowNote(10, 'Type: ' + OType);
          if LowerCase(OType) = 'string' then
            LastControl := CreateFromJSONLabeledEdit(ParentObj, LastControl, OName, J.Items[i]);
          if LowerCase(OType) = 'check' then
            LastControl := CreateFromJSONCheckBox(ParentObj, LastControl, OName, J.Items[i]);
          if LowerCase(OType) = 'table' then
            LastControl := CreateFromJSONTable(ParentObj, LastControl, OName, J.Items[i]);
        end;
      end;
    except
      On E : Exception do
        ShowNote(10,'An Exception occurred when parsing : ' + E.Message);
    end;
  end;


  Procedure ParseString(S : String);
  begin
    ObjectFromJSON(Sheet, StringToJSON(S));
  end;

begin
  ShowNote(10,'JSON: ' + JSON);
  ParseString(JSON);
end;

procedure TMainForm.FormResize(Sender: TObject);
begin

end;

procedure TMainForm.PagerCloseTabClicked(Sender: TObject);
begin
  ClosePage(TCustomPage(Sender));
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
      with DBConn do begin
          Connected:=false;
          UserName:=TEdit(Pager.ActivePage.FindChildControl('login')).Text;
          Password:=TEdit(Pager.ActivePage.FindChildControl('password')).Text;
          HostName:=TEdit(Pager.ActivePage.FindChildControl('DBhost')).Text;
          DatabaseName:=TEdit(Pager.ActivePage.FindChildControl('DBName')).Text;
      end;
      if TCheckBox(Pager.ActivePage.FindChildControl('DBCreateNew')).Checked then
        CreateNewDB(Sender);
      try
        DBConn.Connected:=True;
        ShowNote(9,'Подключено к БД');
        AfterDBConnect(Sender);
      except
        On E : Exception do begin
          ShowNote(1,'An Exception occurred when parsing : ' + E.Message);
          ShowStatus(ConnectStatusOff);
        end;
      end;
    end;
end;

procedure TMainForm.PageSheetDBContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TMainForm.PageCloseExecute(Sender: TObject);
begin
    if Pager.PageCount > 0 then
       ClosePage(Pager.ActivePage);
end;

procedure TMainForm.AfterDBConnect(Sender: TObject);
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

procedure TMainForm.Label1Click(Sender: TObject);
begin

end;

procedure TMainForm.Memo1Change(Sender: TObject);
begin

end;

procedure TMainForm.NavigatorDblClick(Sender: TObject);
begin
  OpenMode(Navigator.Selected);
end;

procedure TMainForm.NotesHideExecute(Sender: TObject);
begin
  SplitterVertical.Position := SplitterVertical.Height;
  SplitterVertical.UpdatePosition;
end;

end.

