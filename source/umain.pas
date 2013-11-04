unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, ExtCtrls, Buttons, ActnList,libusb,Utils,LCLProc
  , XMLPropStorage, Menus, StdCtrls, uUsb, uLibUSbDevice;

type

  { TfMain }

  TfMain = class(TForm)
    acRefresh: TAction;
    acInfo: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    ImageList1: TImageList;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    miInfo: TMenuItem;
    miLanguage: TMenuItem;
    pSelect: TPanel;
    pActiveChain: TPanel;
    Properties: TXMLPropStorage;
    Splitter1: TSplitter;
    tvMain: TTreeView;
    procedure acInfoExecute(Sender: TObject);
    procedure acRefreshExecute(Sender: TObject);
    procedure ControllerGetDeviceClass(VendorID, DeviceID: word;
      var aClass: TUSBDeviceClass);
    procedure ControllerUSBArrival(Sender: TObject);
    procedure ControllerUSBRemove(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure NewMItemClick(Sender: TObject);
    procedure tvMainSelectionChanged(Sender: TObject);
  private
    { private declarations }
    Language: String;
    Controller: TUSBController;
    procedure SetLanguage(Lang : string);
  public
    { public declarations }
    procedure Enumerate;
  end;

const
  FUNC_TYPE             = $FE;
  FUNC_START_BOOTLOADER = 30;

var
  fMain: TfMain;

resourcestring
  strErrorConnectingToDevice = 'Fehler beim verbinden zum Gerät';
  strInfo                               = 'www:  http://www.ullihome.de'+lineending
                                        +'mail: christian@ullihome.de'+lineending
                                        +lineending
                                        +'Lizenz:'+lineending
                                        +'Die Software und ihre Dokumentation wird wie sie ist zur'+lineending
                                        +'Verfuegung gestellt. Da Fehlfunktionen auch bei ausfuehrlich'+lineending
                                        +'getesteter Software durch die Vielzahl an verschiedenen'+lineending
                                        +'Rechnerkonfigurationen niemals ausgeschlossen werden koennen,'+lineending
                                        +'uebernimmt der Autor keinerlei Haftung fuer jedwede Folgeschaeden,'+lineending
                                        +'die sich durch direkten oder indirekten Einsatz der Software'+lineending
                                        +'oder der Dokumentation ergeben. Uneingeschraenkt ausgeschlossen'+lineending
                                        +'ist vor allem die Haftung fuer Schaeden aus entgangenem Gewinn,'+lineending
                                        +'Betriebsunterbrechung, Verlust von Informationen und Daten und'+lineending
                                        +'Schaeden an anderer Software, auch wenn diese dem Autor bekannt'+lineending
                                        +'sein sollten. Ausschliesslich der Benutzer haftet fuer Folgen der'+lineending
                                        +'Benutzung dieser Software.'+lineending
                                        +lineending
                                        +'erstellt mit Freepascal + Lazarus'+lineending
                                        +'http://www.freepascal.org, http://lazarus.freepascal.org'+lineending
                                        +'Iconset von:'+lineending
                                        +'http://www.famfamfam.com/lab/icons/silk/'+lineending;

implementation

uses uScanChain,uInfo;

{ TfMain }

procedure TfMain.FormCreate(Sender: TObject);
var
  NewMItem: TMenuItem;
  sl: TStringList;
  i: Integer;
begin
  Language := 'Deutsch';
  ForceDirectories(GetConfigDir('usbavrlab'));
  Properties.FileName := GetConfigDir('usbavrlab')+'boundaryscantool.xml';
  Properties.Restore;
  Controller := TUSBController.Create(nil);
  Controller.OnGetDeviceClass:=@ControllerGetDeviceClass;
  Controller.OnUSBArrival:=@ControllerUSBArrival;
  Controller.OnUSBRemove:=@ControllerUSBRemove;
  Top := StrToIntDef(Properties.StoredValue['TOP'],Top);
  Left := StrToIntDef(Properties.StoredValue['LEFT'],Left);
  Height := StrToIntDef(Properties.StoredValue['HEIGHT'],Height);
  Width := StrToIntDef(Properties.StoredValue['WIDTH'],Width);
  pSelect.Width := StrToIntDef(Properties.StoredValue['HDIVIDER'],pSelect.Width);
  sl := TStringList.Create;
  if FileExistsUTF8(AppendPathDelim(AppendPathDelim(ProgramDirectory) + 'languages')+'languages.txt') then
    sl.LoadFromFile(UTF8ToSys(AppendPathDelim(AppendPathDelim(ProgramDirectory) + 'languages')+'languages.txt'));
  for i := 0 to sl.Count-1 do
    begin
      NewMItem := TMenuItem.Create(nil);
      NewMItem.Caption := sl[i];
      NewMItem.AutoCheck := True;
      NewMItem.OnClick :=@NewMItemClick;
      NewMItem.GroupIndex := 11;
      miLanguage.Add(NewMItem);
      if UTF8UpperCase(NewMItem.Caption) = UTF8UpperCase(Properties.StoredValue['LANGUAGE']) then
        begin
          NewMItem.Checked := True;
          Language := Properties.StoredValue['LANGUAGE'];
        end;
    end;
  sl.Free;
  SetLanguage(Language);
  fInfo := TfInfo.Create(Self);
  with fInfo do
    begin
      Version := {$I version.inc};
      Version := Version+{$I revision.inc} / 100;
      ProgramName := 'USB AVR Lab Boundary Scan Tool';
      Copyright := '2009 C.Ulrich';
      InfoText := strInfo;
    end;
  fInfo.SetLanguage;
end;

procedure TfMain.acRefreshExecute(Sender: TObject);
begin
  if not Assigned(tvMain.Selected) then exit;
  if TObject(tvMain.Selected.Data) is TfScanChain then
    with TObject(tvMain.Selected.Data) as TfScanChain do
      begin
        Enumerate;
      end;
end;

procedure TfMain.ControllerGetDeviceClass(VendorID, DeviceID: word;
  var aClass: TUSBDeviceClass);
begin
  if  ((VendorID = $16C0)
  and
      ((DeviceID = $05dc)
      ))
  then
    begin
      aClass := TLibUSBDevice;
    end
end;

procedure TfMain.ControllerUSBArrival(Sender: TObject);
var
  aNode: TTreeNode = nil;
  Typ : byte;
  aChannel: TfScanChain;
begin
  if  ((TUSBDevice(Sender).VendorID = $16C0)
  and
      ((TUSBDevice(Sender).DeviceID = $05dc)))
    then
    begin
      if TlibUSBDevice(Sender).OpenDevice then
        begin
          if TlibUSBDevice(Sender).SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,FUNC_TYPE, 0, 0,@typ, 1, 5000) = 1 then
            begin
              TlibUSBDevice(Sender).Tag := Typ;
              if Typ = 10 then
                begin
                  aChannel := TfScanChain.Create(Self);
                  aChannel.JTAGInterface := TUSBLabJTAGInterface.Create(TlibUSBDevice(Sender));
                  aNode := tvMain.Items.Add(nil,'USB AVR-Lab JTAG Interface');
                  aNode.ImageIndex:=1;
                  aNode.SelectedIndex:=1;
                  aNode.Data:=aChannel;
                  aChannel.BorderStyle:=bsNone;
                  aChannel.Parent := pActiveChain;
                  aChannel.Node := aNode;
                  aChannel.Align:=alClient;
                  aChannel.Show;
                  aChannel.Enumerate;
                end;
            end;
          TlibUSBDevice(Sender).CloseDevice;
        end;
    end;
end;

procedure TfMain.ControllerUSBRemove(Sender: TObject);
var
  aNode: TTreeNode;
begin
  if tvMain.Items.Count = 0 then exit;
  aNode := tvMain.Items[0];
  while Assigned(aNode) do
    begin
      if TUSBLAbJTAGInterface(TfScanChain(aNode.Data).JTAGInterface).Device = Sender then
        begin
          pActiveChain.RemoveControl(TfScanChain(aNode.Data));
          TfScanChain(aNode.Data).Free;
          tvMain.Items.Delete(aNode);
          exit;
        end;
      aNode := aNode.GetNext;
    end;
end;

procedure TfMain.acInfoExecute(Sender: TObject);
begin
  fInfo.Showmodal;
end;

procedure TfMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Properties.StoredValue['TOP'] := IntToStr(Top);
  Properties.StoredValue['LEFT'] := IntToStr(Left);
  Properties.StoredValue['HEIGHT'] := IntToStr(Height);
  Properties.StoredValue['WIDTH'] := IntToStr(Width);
  Properties.StoredValue['HDIVIDER'] := IntToStr(pSelect.Width);
end;

procedure TfMain.NewMItemClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to miLanguage.Count-1 do
    if miLanguage[i].Caption = Language then
      miLanguage[i].Checked := false;
  TmenuItem(Sender).Checked := True;
  Language := TmenuItem(Sender).Caption;
  SetLanguage(Language);
  Properties.StoredValue['LANGUAGE'] := Language;
end;

procedure TfMain.tvMainSelectionChanged(Sender: TObject);
begin
  acRefresh.Enabled:=False;
  if not Assigned(tvMain.Selected) then exit;
  if TObject(tvMain.Selected.Data) is TfScanChain then
    begin
      acRefresh.Enabled := True;
    end
  else if TObject(tvMain.Selected.Parent.Data) is TfScanChain then
    begin
      TfScanChain(tvMain.Selected.Parent.Data).cbDevices.ItemIndex:=tvMain.Selected.Index;
      TfScanChain(tvMain.Selected.Parent.Data).cbDevices.OnChange(TfScanChain(tvMain.Selected.Parent.Data).cbDevices);
    end;
end;

procedure TfMain.SetLanguage(Lang: string);
begin

end;

procedure TfMain.Enumerate;
var
  busses : PUSBBus;
  usb_bus: PUSBBus;
  dev :    PUSBDevice;
  DeviceHandle: PUSBDevHandle;
  typ,res : byte;
  Found: Boolean;
  aChannel: TfScanChain = nil;
  aNode: TTreeNode = nil;
  oNode: TTreeNode;
begin
  {
  usb_init();
  usb_find_busses();
  usb_find_devices();

  if tvMain.Items.Count > 0 then
    aNode := tvMain.Items[0];
  while Assigned(aNode) do
    begin
      TfScanChain(aNode.Data).Tag:=-1;
      aNode := aNode.GetNextSibling;
    end;

  busses := usb_get_busses();
  usb_bus := busses;
  while Assigned(usb_bus) do
    begin
      dev := usb_bus^.devices;
      while Assigned(dev) do
        begin
          if  (dev^.descriptor.idVendor = $16C0)
          and (dev^.descriptor.idProduct = $05dc) then
            begin
              DeviceHandle := usb_open(dev);
              res := usb_control_msg(DeviceHandle,USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,FUNC_TYPE, 0, 0,@typ, 1, 5000);
              usb_close(DeviceHandle);
              if res <> 0 then
                if typ = 10 then
                  begin
                    Found := False;
                    if tvMain.Items.Count > 0 then
                      aNode := tvMain.Items[0]
                    else
                      aNode := nil;
                    while Assigned(aNode) do
                      begin
                        if TUSBLAbJTAGInterface(TfScanChain(aNode.Data).JTAGInterface).Device = dev then
                          begin
                            TfScanChain(aNode.Data).Tag := 0;
                            Found := True;
                          end;
                        aNode := aNode.GetNextSibling;
                      end;
                    if not Found then
                      begin
                        aChannel := TfScanChain.Create(Self);
                        aChannel.JTAGInterface := TUSBLabJTAGInterface.Create(dev);
                        aNode := tvMain.Items.Add(nil,'USB AVR-Lab JTAG Interface');
                        aNode.ImageIndex:=1;
                        aNode.SelectedIndex:=1;
                        aNode.Data:=aChannel;
                        aChannel.BorderStyle:=bsNone;
                        aChannel.Parent := pActiveChain;
                        aChannel.Name:='Cannel'+IntToStr(integer(dev));
                        aChannel.Node := aNode;
                        aChannel.Align:=alClient;
                        aChannel.Show;
                        aChannel.Enumerate;
                      end;
                  end;
            end;
          dev := dev^.next;
        end;
      usb_bus := usb_bus^.next
    end;
  if tvMain.Items.Count > 0 then
    aNode := tvMain.Items[0]
  else
    aNode := nil;
  while Assigned(aNode) do
    begin
      oNode := aNode;
      aNode := aNode.GetNextSibling;
      if TfScanChain(oNode.Data).Tag = -1 then
        begin
          pActiveChain.RemoveControl(TfScanChain(oNode.Data));
          TfScanChain(oNode.Data).Free;
          tvMain.Items.Delete(oNode);
        end
    end;
    }
end;

initialization
  {$I umain.lrs}

end.

