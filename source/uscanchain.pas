unit uScanChain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, libusb, StdCtrls, ComCtrls,
  Dialogs, uBSDL, Utils, Controls, Buttons, ActnList, ExtCtrls, uDrawIC, Graphics,
  uLibUSBDevice;

type
  TTAPState  = (TAP_STATE_UNKNOWN=0,
                TAP_STATE_RESET=1,
                TAP_STATE_IDLE=2,
                TAP_STATE_SELECT_DR_SCAN=3,
                TAP_STATE_CAPTURE_DR=4,
                TAP_STATE_SHIFT_DR=5,
                TAP_STATE_EXIT1_DR=6,
                TAP_STATE_PAUSE_DR=7,
                TAP_STATE_EXIT2_DR=8,
                TAP_STATE_UPDATE_DR=9,
                TAP_STATE_SELECT_IR_SCAN=10,
                TAP_STATE_CAPTURE_IR=11,
                TAP_STATE_SHIFT_IR=12,
                TAP_STATE_EXIT1_IR=13,
                TAP_STATE_PAUSE_IR=14,
                TAP_STATE_EXIT2_IR=15,
                TAP_STATE_UPDATE_IR=16);

  TBoundaryCellFunction = (bfInternal,bfInput,bfControl,bfControlR,bfOutput2,bfOutput3,bfBiDir,bfClock,bfObserveOnly);
  TBoundaryCellRslt = (brZ,brPull1,brPull0,brWeak1,brWeak0,brKeeper);

  TJTAGInterface = class
  public
    procedure Open;virtual;abstract;
    procedure Close;virtual;abstract;
    function SetTapState(NewState : TTapState) : Boolean;virtual;abstract;
    function ShiftInData(Data : Pointer;length : Integer) : Boolean;virtual;abstract;
    function GetChainSize : Integer;virtual;abstract;
    function ShiftOutData(Data: Pointer; length: Integer): Boolean;virtual;abstract;
  end;

  { TUSBLAbJTAGInterface }

  TUSBLAbJTAGInterface = class(TJTAGInterface)
  private
    FDev : TLibUSBDevice;
    FTapState : TTapState;
  public
    constructor Create(Dev : TLibUSBDevice);
    destructor Destroy;override;
    procedure Open;override;
    procedure Close;override;
    function SetTapState(NewState : TTapState) : Boolean;override;
    function ShiftInData(Data : Pointer;length : Integer) : Boolean;override;
    function GetChainSize : Integer;override;
    function ShiftOutData(Data: Pointer; length: Integer): Boolean;override;
    property Device : TLibUSBDevice read FDev;
  end;

  TJTAGBoundaryCell = record
    CellNumber : Integer;
    Cell : Integer;                            //Celltype
    Port : string;
    CellFunction : TBoundaryCellFunction;   //cell function
    safe : char;
    control_cell : Integer;
    disable_value : char;
    disable_result : TBoundaryCellRslt;
    value : Boolean;
    invalue : Boolean;
    read : Boolean;
  end;
  PJTAGBoundaryCell = ^TJTAGBoundaryCell;

  TJTAGBoundaryRegister = array of TJTAGBoundaryCell;

  TJTAGPins = class;

  TJTAGPinState = (jsUnknown,jsTristate,jsOn,jsOff);

  { TJTAGPin }

  TJTAGPin = class(TList)
  private
    FDrawable: TDrawableICPin;
    FName: string;
    FParent: TJTAGPins;
    FPin: Integer;
    function GetCells(idx : Integer): TJTAGBoundaryCell;
    function GetInState: TJTAGPinState;
    function GetType: TBoundaryCellFunction;
    procedure SetName(const AValue: string);
    function GetState: TJTAGPinState;
    procedure SetState(const AValue: TJTAGPinState);
  public
    constructor Create(AOwner : TJTAGPins);
    property Name : string read FName write SetName;
    property Pin : Integer read FPin write FPin;
    property Parent : TJTAGPins read FParent;
    property Cells[idx : Integer] : TJTAGBoundaryCell read GetCells;
    property OutState : TJTAGPinState read GetState write SetState;
    property Typ : TBoundaryCellFunction read GetType;
    property InState : TJTAGPinState read GetInState;
    property Drawable : TDrawableICPin read FDrawable write FDrawable;
  end;

  TJTAGDevice = class;

  { TJTAGPins }

  TJTAGPins = class(TList)
  private
    FBoundaryLength: Integer;
    FBoundaryRegister: TJTAGBoundaryRegister;
    FParent: TJTAGDevice;
    FPinout: String;
    function GetPin(name : string): TJTAGPin;
    procedure SetPinout(const AValue: String);
  public
    constructor Create(AOwner : TJTAGDevice);
    procedure Clear; override;
    property BoundaryRegister : TJTAGBoundaryRegister read FBoundaryRegister;
    property BoundaryLength : Integer read FBoundaryLength;
    procedure SetBoundaryRegister(bsdlinfo : TStrings);
    property Parent : TJTAGDevice read FParent;
    property Pinout : String read FPinout write SetPinout;
    property Pins[name : string] : TJTAGPin read GetPin;
  end;

  TJTAGInstruction = record
    Name : string;
    Value : string;
  end;

  TJTAGInstructions = array of TJTAGInstruction;

  { TJTAGDevice }

  TJTAGDevice = class
  private
    FBSDLDevice: TEntityParameter;
    FBSDLFile: TBSDLFile;
    FDefaultPinout: string;
    FInstruction: string;
    FInstructions: TJTAGInstructions;
    FJTAGInterface: TJTAGInterface;
    FManufacturerID: Integer;
    FName: string;
    FPartNumber: Integer;
    FPins: TJTAGPins;
    FVendorName: string;
    FVersion: Integer;
    procedure SetBSDLFile(const AValue: TBSDLFile);
    procedure SetInstruction(const AValue: string);
    procedure SetJTAGInterface(const AValue: TJTAGInterface);
  public
    constructor Create(mID,PartNum,Vers : Integer);
    property ManufacturerID : Integer read FManufacturerID;
    property VendorName : string read FVendorName;
    property PartNumber : Integer read FPartNumber;
    property Version : Integer read FVersion;
    property Name : string read FName;
    property BSDLFile : TBSDLFile read FBSDLFile write SetBSDLFile;
    property BSDLDevice : TEntityParameter read FBSDLDevice;
    property Pins : TJTAGPins read FPins;
    property DefaultPinout : string read FDefaultPinout;
    property Instruction : string read FInstruction write SetInstruction;
    property Instructions : TJTAGInstructions read FInstructions;
    property JTAGInterface : TJTAGInterface read FJTAGInterface write SetJTAGInterface;
    procedure ShiftInOut;
    destructor Destroy;override;
  end;

  { TfScanChain }

  TfScanChain = class(TFrame)
    acSingleShift: TAction;
    acReset: TAction;
    ActionList1: TActionList;
    cbDevices: TComboBox;
    cbPinout: TComboBox;
    cbOnlyJTAGablePins: TCheckBox;
    cbInstruction: TComboBox;
    ilStates: TImageList;
    ilActions: TImageList;
    ilPinTypes: TImageList;
    lInstruction: TLabel;
    lPinout: TLabel;
    lFManufacturer: TLabel;
    lFDeviceName: TLabel;
    lManufacturer: TLabel;
    lDeviceName: TLabel;
    pbPins: TPaintBox;
    pView: TPanel;
    SpeedButton1: TSpeedButton;
    sbContinousShift: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Splitter1: TSplitter;
    tvPins: TTreeView;
    procedure acResetExecute(Sender: TObject);
    procedure acSingleShiftExecute(Sender: TObject);
    procedure cbDevicesChange(Sender: TObject);
    procedure cbInstructionChange(Sender: TObject);
    procedure cbOnlyJTAGablePinsChange(Sender: TObject);
    procedure cbPinoutChange(Sender: TObject);
    procedure pbPinsDblClick(Sender: TObject);
    procedure pbPinsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbPinsPaint(Sender: TObject);
    procedure pbPinsResize(Sender: TObject);
    procedure sbContinousShiftClick(Sender: TObject);
    procedure tvPinsDblClick(Sender: TObject);
  private
    { private declarations }
    FDevices : TList;
    function GetDevices(idx : Integer): TJTAGDevice;
  public
    { public declarations }
    DeviceCount : Integer;
    Node : TTreeNode;
    JTAGInterface : TJTAGInterface;
    SelectedDev : TJTAGDevice;
    SelectedPinout : TDrawableICPinout;
    SelectedPinoutBitmap : TBitmap;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
    procedure Enumerate;
    procedure DisplayData;
    property Devices[idx : Integer] : TJTAGDevice read GetDevices;
  end;

const
  FUNC_READ_CHAIN       = 1;
  FUNC_WRITE_CHAIN      = 2;
  FUNC_TAP_STATE        = 3;
  FUNC_GET_TAP_STATE    = 4;
  FUNC_OPEN             = 5;
  FUNC_CLOSE            = 6;
  FUNC_SHIFTBITS        = 7;
  FUNC_CHAINSIZE        = 8;
  FUNC_EXECCHAIN        = 9;


implementation

uses BinUtils;

resourcestring
  strUnknown              = 'Unbekannt';
  strUnknownIC            = 'Unbekanntes IC Herst.:%.2x Part.: %.4x Version: %.1x';
  strUnknownCellType      = 'Unkebannter Boundary Scan Cell Type %s';
  strUnknownRsltType      = 'Unkebannter Boundary Scan Rslt Typ %s';

{ TfScanChain }

procedure TfScanChain.cbDevicesChange(Sender: TObject);
var
  aDev: TJTAGDevice;
  i: Integer;
begin
  acSingleShift.Enabled:=False;
  sbContinousShift.Enabled:=False;
  if not Assigned(Devices[cbDevices.ItemIndex]) then
    begin
      lFManufacturer.Caption:='-';
      lfDeviceName.Caption := '-';
      SelectedDev := nil;
      cbPinout.OnChange(cbPinout);
      exit;
    end;
  aDev := Devices[cbDevices.ItemIndex];
  lFManufacturer.Caption := aDev.VendorName;
  SelectedDev := aDev;
  lfDeviceName.Caption := Devices[cbDevices.ItemIndex].Name;
  cbPinout.Clear;
  if not Assigned(aDev.BSDLFile) then exit;
  for i := 0 to aDev.BSDLFile.DevicesByIdx[0].Count-1 do
    if TObject(aDev.BSDLFile.DevicesByIdx[0][i]) is TConstant then
      if TConstant(aDev.BSDLFile.DevicesByIdx[0][i]).Typ = 'PIN_MAP_STRING' then
        cbPinout.Items.Add(TConstant(aDev.BSDLFile.DevicesByIdx[0][i]).Name);
  cbPinout.Text:=trim(aDev.DefaultPinout);
  cbPinout.OnChange(cbPinout);
  cbInstruction.Clear;
  for i := 0 to length(aDev.Instructions)-1 do
    if (aDev.Instructions[i].Name = 'SAMPLE')
    or (aDev.Instructions[i].Name = 'BYPASS')
    or (aDev.Instructions[i].Name = 'EXTEST')
    or (aDev.Instructions[i].Name = 'INTEST') then
      cbInstruction.Items.Add(aDev.Instructions[i].Name);
  cbInstruction.Text:=aDev.Instruction;
end;

procedure TfScanChain.acSingleShiftExecute(Sender: TObject);
begin
  SelectedDev.ShiftInOut;
  DisplayData;
end;

procedure TfScanChain.acResetExecute(Sender: TObject);
var
  abyte : byte;
  i: Integer;
begin
  JTAGInterface.SetTapState(TAP_STATE_RESET);  //All devices that support ID Code have now IDCODE instruction loaded
  JTAGInterface.SetTapState(TAP_STATE_SHIFT_DR); //Shift register is now connected to IDCODE Register
  for i := 0 to DeviceCount-1 do
    Devices[i].Instruction:='IDCODE';
  cbInstruction.Text:='';
end;

procedure TfScanChain.cbInstructionChange(Sender: TObject);
begin
  SelectedDev.Instruction:=cbInstruction.Text;
  cbInstruction.Text:=SelectedDev.Instruction;
  acSingleShift.Enabled:=cbInstruction.ItemIndex > -1;
  sbContinousShift.Enabled:=cbInstruction.ItemIndex > -1;
end;

procedure TfScanChain.cbOnlyJTAGablePinsChange(Sender: TObject);
begin
  cbPinoutChange(nil);
end;

procedure TfScanChain.cbPinoutChange(Sender: TObject);
var
  i: Integer;
  a: Integer;
  tmp : string;
  aItem: TTreeNode;
  PinNum : Integer;
  aSubItem: TTreeNode;

  procedure ConnectPin(aItem : TTreeNode;PortName : string;Drawable : TDrawableICPin = nil);
  begin
    aItem.Data:=SelectedDev.Pins.Pins[Portname];
    if Assigned(SelectedDev.Pins.Pins[Portname]) then
      begin
        if Assigned(Drawable) then SelectedDev.Pins.Pins[Portname].Drawable := Drawable;
        if SelectedDev.Pins.Pins[Portname].Count = 0 then
          aItem.ImageIndex := 0
        else
          begin
            case SelectedDev.Pins.Pins[Portname].Cells[0].CellFunction of
            bfInput:
              begin
                aItem.ImageIndex := 1;
                aItem.StateIndex:=3;
              end;
            bfControl,bfControlR:aItem.ImageIndex := 4;
            bfOutput2,bfOutput3:aItem.ImageIndex := 2;
            bfBiDir:
              begin
                aItem.ImageIndex := 3;
                aItem.StateIndex:=3;
              end;
            bfClock:aItem.ImageIndex := 5;
            bfObserveOnly:
              begin
                aItem.ImageIndex := 6;
                aItem.StateIndex:=3;
              end;
            end;
          end;
      end;
      aItem.SelectedIndex:=aItem.ImageIndex;
  end;
begin
  tvPins.BeginUpdate;
  tvPins.Items.Clear;
  FreeAndNil(SelectedPinout);
  SelectedPinout := TDrawableICPinout.Create(cbPinout.Text);
  if Assigned(SelectedDev) and Assigned(SelectedDev.BSDLFile) then
    for i := 0 to SelectedDev.BSDLFile.DevicesByIdx[0].Count-1 do
      if TObject(SelectedDev.BSDLFile.DevicesByIdx[0][i]) is TConstant then
        if TConstant(SelectedDev.BSDLFile.DevicesByIdx[0][i]).Name = cbPinout.Text then
          begin
            SelectedDev.Pins.Pinout:=cbPinout.Text;
            with TObject(SelectedDev.BSDLFile.DevicesByIdx[0][i]) as TConstant do
              for a := 0 to Parameter.Count-1 do
                begin
                  tmp := Parameter[a];
                  aItem := tvPins.Items.Add(nil,trim(copy(tmp,0,pos(':',tmp)-1)));
                  tmp := copy(tmp,pos(':',tmp)+1,length(tmp));
                  if rpos(',',tmp) > 0 then
                    tmp := copy(tmp,0,rpos(',',tmp)-1);
                  if pos('(',tmp) = 0 then
                    begin
                      SelectedPinout.Add(TDrawableICPin.Create(StrToInt(trim(tmp)),aItem.Text));
                      ConnectPin(aItem,aItem.text,TDrawableICPin(SelectedPinout.Items[SelectedPinout.Count-1]));
                    end
                  else
                    ConnectPin(aItem,aItem.text);
                  aItem.Text := aItem.Text + ' ['+trim(tmp)+']';
                  if pos('(',tmp) > 0 then
                    begin
                      tmp := copy(tmp,pos('(',tmp)+1,length(tmp));
                      PinNum := 0;
                      while pos(',',tmp) > 0 do
                        begin
                          aSubItem := tvPins.Items.AddChild(aItem,copy(aItem.Text,0,pos(' ',aItem.Text)-1)+IntToStr(PinNum));
                          SelectedPinout.Add(TDrawableICPin.Create(StrToInt(copy(tmp,0,pos(',',tmp)-1)),aSubItem.Text));
                          ConnectPin(aSubItem,copy(aItem.Text,0,pos(' ',aItem.Text)-1)+'('+IntToStr(PinNum)+')',TDrawableICPin(SelectedPinout.Items[SelectedPinout.Count-1]));
                          aSubItem.Text := aSubItem.Text + ' ['+copy(tmp,0,pos(',',tmp)-1)+']';
                          inc(PinNum);
                          tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
                        end;
                      aSubItem := tvPins.Items.AddChild(aItem,copy(aItem.Text,0,pos(' ',aItem.Text)-1)+IntToStr(PinNum));
                      SelectedPinout.Add(TDrawableICPin.Create(StrToInt(copy(tmp,0,pos(')',tmp)-1)),aSubItem.Text));
                      ConnectPin(aSubItem,copy(aSubItem.Text,0,pos(' ',aItem.Text)-1)+'('+IntToStr(PinNum)+')',TDrawableICPin(SelectedPinout.Items[SelectedPinout.Count-1]));
                      aSubItem.Text := aSubItem.Text + ' ['+copy(tmp,0,pos(')',tmp)-1)+']';
                    end;
                  aItem.Expanded:=True;
                  if cbOnlyJTAGablePins.Checked and ((aItem.Data = nil) or (TJTAGPin(aItem.Data).Count = 0)) and (aItem.Count = 0) then
                    tvPins.Items.Delete(aItem);
                end;
          end;
  SelectedPinout.Draw(SelectedPinoutBitmap.Canvas,Rect(0,0,SelectedPinoutBitmap.Width,SelectedPinoutBitmap.Height));
  pbPins.Invalidate;
  tvPins.EndUpdate;
end;

procedure TfScanChain.pbPinsDblClick(Sender: TObject);
begin
  if Assigned(tvPins.Selected) then
    tvPins.OnDblClick(Self);
end;

procedure TfScanChain.pbPinsMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  aNode: TTreeNode;
  Pin: TDrawableICPin;
begin
  if not Assigned(SelectedPinout) then exit;
  Pin := SelectedPinOut.GetPinAt(x,y);
  if not Assigned(Pin) then exit;
  aNode := tvPins.Items[0];
  while aNode <> nil do
    begin
      if Assigned(aNode.Data) and (TObject(aNode.Data) is TJTAGPin) and (Assigned(TJTAGPin(aNode.Data).Drawable)) and (TJTAGPin(aNode.Data).Drawable = Pin) then
        begin
          tvPins.Selected := aNode;
          break;
        end;
      aNode := aNode.GetNext;
    end;
end;

procedure TfScanChain.pbPinsPaint(Sender: TObject);
begin
  pbPins.Canvas.Draw(0,0,SelectedPinoutBitmap);
end;

procedure TfScanChain.pbPinsResize(Sender: TObject);
begin
  SelectedPinoutBitmap.Height:=pbPins.Height;
  SelectedPinoutBitmap.Width:=pbPins.Width;
  if Assigned(SelectedPinout) then
    SelectedPinout.Draw(SelectedPinoutBitmap.Canvas,Rect(0,0,SelectedPinoutBitmap.Width,SelectedPinoutBitmap.Height));
end;

procedure TfScanChain.sbContinousShiftClick(Sender: TObject);
begin
  while sbContinousShift.Down do
    begin
      SelectedDev.ShiftInOut;
      DisplayData;
      Application.ProcessMessages;
    end;
end;

procedure TfScanChain.tvPinsDblClick(Sender: TObject);
begin
  if not Assigned(tvPins.Selected) then exit;
  if not Assigned(tvPins.Selected.Data) then exit;
  case TJTAGPin(tvPins.Selected.Data).OutState of
  jsUnknown,jsTriState:TJTAGPin(tvPins.Selected.Data).OutState := jsOn;
  jsOn:TJTAGPin(tvPins.Selected.Data).OutState := jsOff;
  jsOff:TJTAGPin(tvPins.Selected.Data).OutState := jsTristate;
  end;
  DisplayData;
end;

function TfScanChain.GetDevices(idx : Integer): TJTAGDevice;
begin
  Result := nil;
  if (FDevices.Count >= idx) and (idx >= 0) then
    Result := TJTAGDevice(FDevices[idx]);
end;

constructor TfScanChain.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDevices := TList.Create;
  SelectedPinoutBitmap := TBitmap.Create;
end;

destructor TfScanChain.Destroy;
var
  i: Integer;
begin
  for i := 0 to DeviceCount-1 do
    Devices[i].Free;
  FDevices.Clear;
  FDevices.Free;
  JTAGInterface.Free;
  inherited Destroy;
end;

procedure TfScanChain.Enumerate;
var
  i: Integer;
  data : array[0..255] of byte;
  tmpdevcount: Integer;
  aDev: TJTAGDevice;
  aNode: TTreeNode;
begin
  JTAGInterface.Open;
  for i := 0 to DeviceCount-1 do
    Devices[i].Free;
  FDevices.Clear;
  cbDevices.Clear;
  cbDevices.OnChange(cbDevices);
  Node.DeleteChildren;
  if JTAGInterface.SetTapState(TAP_STATE_SHIFT_IR) then
    begin
      //All Devices to Bypass
      Fillchar(Data[0],255,$FF);
      JTAGInterface.ShiftInData(@Data[0],255);
      JTAGInterface.SetTapState(TAP_STATE_SHIFT_DR);
      //Clear DataRegister
      Fillchar(Data[0],255,$00);
      JTAGInterface.ShiftInData(@Data[0],255);
      //Shift in 1 until we receive one back
      DeviceCount := -1;
      data[0] := $FF;
      tmpdevcount := 0;
      for i := 0 to 1 do
        begin
          JTAGInterface.SetTapState(TAP_STATE_SHIFT_DR);
          if not JTAGInterface.ShiftInData(@data[0],1) then break;
          if not JTAGInterface.ShiftOutData(@data[1],2) then break;
          if data[1] = 0 then
            inc(tmpdevcount,8)
          else
            begin
              DeviceCount := tmpdevcount;
              while ((data[1] and 1) = 0) do
                begin
                  data[1] := data[1] shr 1;
                  inc(DeviceCount);
                end;
              break;
            end;
        end;
      if not JTAGInterface.SetTapState(TAP_STATE_RESET) then exit;  //All devices that support ID Code have now IDCODE instruction loaded
      for i := 0 to DeviceCount-1 do
        begin
          if not JTAGInterface.SetTapState(TAP_STATE_SHIFT_DR) then exit; //Shift register is now connected to IDCODE Register
          Fillchar(Data[0],255,$FF);
//          Showmessage('Achtung!');
          JTAGInterface.ShiftInData(@Data[0],4);
          JTAGInterface.ShiftOutData(@Data[0],4);
//          Showmessage(IntToHex(Data[0],2)+' '+IntToHex(Data[1],2)+' '+IntToHex(Data[2],2)+' '+IntToHex(Data[3],2)+' ');
          aDev := TJTAGDevice.Create(((Data[2] and $F) shl 7)+(Data[3] shr 1),((Data[0] and $F) shl 12)+(Data[1] shl 4)+(Data[2] shr 4),Data[0] shr 4);
          aDev.JTAGInterface := JTAGInterface;
          FDevices.Add(aDev);
          aDev.FInstruction:='IDCODE';
        end;
      if not JTAGInterface.SetTapState(TAP_STATE_SHIFT_DR) then exit;
      for i := 0 to DeviceCount-1 do
        begin
          cbDevices.Items.Add(Devices[i].Name);
          aNode := Node.Owner.AddChild(Node,Devices[i].Name);
          aNode.ImageIndex := 2;
          aNode.SelectedIndex := 2;
          Node.Expanded:=True;
        end;
    end;
end;

procedure TfScanChain.DisplayData;
var
  aNode: TTreeNode;
begin
  tvPins.BeginUpdate;
  if not Assigned(SelectedDev) then exit;
  aNode := tvPins.Items[0];
  while Assigned(aNode) do
    begin
      if Assigned(aNode.Data) then
        begin
          if (TJTAGPin(aNode.Data).Typ = bfOutput2)
          or (TJTAGPin(aNode.Data).Typ = bfOutput3)
          or (TJTAGPin(aNode.Data).Typ = bfBiDir) then
            case TJTAGPin(aNode.Data).OutState of
            jsUnknown,
            jsTristate:aNode.ImageIndex := 9;
            jsOn:aNode.ImageIndex := 7;
            jsOff:aNode.ImageIndex := 8;
            end;
          aNode.SelectedIndex:=aNode.ImageIndex;
          case TJTAGPin(aNode.Data).InState of
          jsOn:aNode.StateIndex := 0;
          jsOff:aNode.StateIndex := 1;
          end;
        end;
      aNode := aNode.GetNext;
    end;
  SelectedPinout.Draw(SelectedPinoutBitmap.Canvas,Rect(0,0,SelectedPinoutBitmap.Width,SelectedPinoutBitmap.Height));
  tvPins.EndUpdate;
  pbPins.Invalidate;
end;

{ TJTAGDevice }

procedure TJTAGDevice.SetInstruction(const AValue: string);
var
  aBuffer : array of Byte;
  Value : string = '';
  i: Integer;
  idx: Integer;
  aChar: Char;
begin
  if FInstruction = AValue then exit;
  for i := 0 to length(Instructions)-1 do
    if Instructions[i].Name = AValue then Value := Instructions[i].Value;
  //Instruction als String
  if Value = '' then exit;
  if (length(Value) mod 8) > 0 then
    Setlength(aBuffer,(length(Value) div 8)+1)
  else
    Setlength(aBuffer,(length(Value) div 8));
  //auf 8 bit länge aufsstocken
  while length(value) < (length(aBuffer)*8) do
    Value := Value+'0';
  idx := 0;
  //binär
  while length(Value) > 0 do
    begin
      aBuffer[idx] := BinToInt(copy(Value,0,8));
      inc(idx);
      Value := copy(Value,9,length(value));
    end;
  //senden
  if not JTAGInterface.SetTapState(TAP_STATE_SHIFT_IR) then exit;
  if not JTAGInterface.ShiftInData(@aBuffer[0],length(aBuffer)) then exit;
  if not JTAGInterface.SetTapState(TAP_STATE_SHIFT_DR) then exit;
  FInstruction := AValue;
end;

procedure TJTAGDevice.SetJTAGInterface(const AValue: TJTAGInterface);
begin
  FJTAGInterface := AValue;
  FJTAGInterface.Open;
end;

procedure TJTAGDevice.SetBSDLFile(const AValue: TBSDLFile);
var
  i: Integer;
  tmp : string;
begin
  if AValue = nil then
    begin
      FBSDLDevice := nil;
      exit;
    end;
  if not Assigned(FBSDLDevice) then exit;//TODO: find device ourself
  FBSDLFile := AValue;
  if Assigned(FBSDLDevice.Parameters['BOUNDARY_REGISTER']) then
    FPins.SetBoundaryRegister(TAttribute(FBSDLDevice.Parameters['BOUNDARY_REGISTER']).Parameter);
  if Assigned(FBSDLDevice.Parameters['PHYSICAL_PIN_MAP']) then
    FDefaultPinout := TGenericParameter(FBSDLDevice.Parameters['PHYSICAL_PIN_MAP']).Parameter.Text;
  if Assigned(FBSDLDevice.Parameters['INSTRUCTION_OPCODE']) then
    with FBSDLDevice.Parameters['INSTRUCTION_OPCODE'] as TAttribute do
      begin
        Setlength(FInstructions,Parameter.Count);
        for i := 0 to Parameter.Count-1 do
          begin
            Instructions[i].Name:=trim(copy(Parameter[i],0,pos('(',Parameter[i])-1));
            tmp := copy(Parameter[i],pos('(',Parameter[i])+1,length(Parameter[i]));
            Instructions[i].Value:=trim(copy(tmp,0,pos(')',tmp)-1));
          end;
      end;
end;

constructor TJTAGDevice.Create(mID, PartNum, Vers: Integer);
var
  Info: TSearchRec;
  aFile : TBSDLFile;
  i: Integer;
  idcode,idcode1 : Longint;
  sl: TStringList;
begin
  FManufacturerID := mID;
  FPartNumber := PartNum;
  FVersion := Vers;
  FBSDLFile := nil;
  FPins := TJTAGPins.Create(Self);
  if FileExists(AppendPathDelim(ExtractFileDir(Application.Exename))+'data'+DirectorySeparator+'idcodes.dat') then
    begin
      sl := TStringList.Create;
      sl.LoadFromFile(AppendPathDelim(ExtractFileDir(Application.Exename))+'data'+DirectorySeparator+'idcodes.dat');
      for i := 0 to sl.Count-1 do
        if IntToBin(BinToInt(copy(sl[i],0,pos(';',sl[i])-1))) = IntToBin(mID) then
          FVendorName := copy(sl[i],pos(';',sl[i])+1,length(sl[i]));
      sl.Free;
    end;
  FName := Format(strUnknownIC,[mID,PartNum,Vers]);
  if FindFirst (AppendPathDelim(ExtractFileDir(Application.Exename))+'data'+DirectorySeparator+'*.bsd',faAnyFile,Info)=0 then
    repeat
      aFile := TBSDLFile.Create;
      aFile.LoadFromFile(AppendPathDelim(ExtractFileDir(Application.Exename))+'data'+DirectorySeparator+Info.Name);
      for i := 0 to aFile.DeviceCount-1 do
        if Assigned(aFile.DevicesByIdx[i].Parameters['IDCODE_REGISTER']) then
          begin
            idcode := BinToInt(copy(StringReplace(StringReplace(TAttribute(aFile.DevicesByIdx[i].Parameters['IDCODE_REGISTER']).Parameter.Text,#13,'',[rfReplaceAll]),#10,'',[rfReplaceAll]),5,100));
            idcode1 := ((0 shl 28) + (PartNum shl 12) + (mId shl 1) + 1);
            if idcode = idcode1 then
              begin
                FName := aFile.DevicesByIdx[i].Name;
                FBSDLDevice := aFile.DevicesByIdx[i];
                BSDLFile := aFile;
                break;
              end;
          end;
      if not Assigned(FBSDLfile) then
        aFile.Free
      else break;
    until FindNext(info)<>0;
  FindClose(Info);
end;

procedure TJTAGDevice.ShiftInOut;
var
  i: Integer;
  aData : array of byte;
  idx: Integer;
  cell: Integer;
  j: Integer;
  a: Integer;
  aByte: Integer;
  aBit: Integer;
begin
  if (Pins.BoundaryLength mod 8) > 0 then
    Setlength(aData,(Pins.BoundaryLength div 8)+1)
  else
    Setlength(aData,Pins.BoundaryLength div 8);
  FillChar(aData[0],length(aData),$00);
  for i := 0 to length(Pins.BoundaryRegister)-1 do
    if Pins.BoundaryRegister[i].value then
      begin
        aByte := (Pins.BoundaryRegister[i].CellNumber+1) div 8;
        aBit := ((Pins.BoundaryRegister[i].CellNumber+1) mod 8);
        aData[aByte] := aData[aByte] + (1 shl aBit);
      end;
  JTAGInterface.ShiftInData(@aData[0],length(aData));
  JTAGInterface.ShiftOutData(@aData[0],length(aData));
  cell := 0;
  for i := length(aData)-1 downto 0 do
    for j := 0 to 7 do
      begin
        for a := 0 to length(Pins.BoundaryRegister)-1 do
          if Pins.BoundaryRegister[a].CellNumber = cell then
            begin
              Pins.BoundaryRegister[a].invalue := aData[i] and 1 = 1;
              Pins.BoundaryRegister[a].read := True;
            end;
        aData[i] := aData[i] shr 1;
        inc(cell);
      end;
  JTAGInterface.SetTapState(TAP_STATE_SHIFT_DR);
end;

destructor TJTAGDevice.Destroy;
begin
  FBSDLFile.Free;
  FPins.Free;
  inherited Destroy;
end;

{ TJTAGPins }

procedure TJTAGPins.SetPinout(const AValue: String);
var
  i: Integer;
  a: Integer;
  tmp : string;
  aPin: TJTAGPin;
  Parname: String;
  PinNum: Integer;
begin
  if (FPinout=AValue) or (not Assigned(Parent)) or (not Assigned(Parent.BSDLFile)) then exit;
  FPinout:=AValue;
  Clear;
  for i := 0 to Parent.BSDLFile.DevicesByIdx[0].Count-1 do
    if TObject(Parent.BSDLFile.DevicesByIdx[0][i]) is TConstant then
      if TConstant(Parent.BSDLFile.DevicesByIdx[0][i]).Name = AValue then
        begin
          with TObject(Parent.BSDLFile.DevicesByIdx[0][i]) as TConstant do
            for a := 0 to Parameter.Count-1 do
              begin
                tmp := Parameter[a];
                aPin := TJTAGPin.Create(Self);
                aPin.Name:=trim(copy(tmp,0,pos(':',tmp)-1));
                Parname := aPin.Name;
                tmp := copy(tmp,pos(':',tmp)+1,length(tmp));
                if rpos(',',tmp) > rpos(')',tmp) then
                  tmp := copy(tmp,0,rpos(',',tmp)-1);
                if IsNumeric(trim(tmp)) then aPin.Pin := StrToInt(trim(tmp))
                else if pos('(',tmp) > 0 then
                  begin
                    tmp := copy(tmp,pos('(',tmp)+1,length(tmp));
                    PinNum := 0;
                    while pos(',',tmp) > 0 do
                      begin
                        aPin.Name:=ParName+'('+IntToStr(PinNum)+')';
                        aPin.Pin:=StrToInt(copy(tmp,0,pos(',',tmp)-1));
                        inc(PinNum);
                        tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
                        aPin := TJTAGPin.Create(Self)
                      end;
                    aPin.Name := ParName+'('+IntToStr(PinNum)+')';
                    aPin.Pin:=StrToInt(copy(tmp,0,pos(')',tmp)-1));
                  end;
              end;
        end;
end;

function TJTAGPins.GetPin(name : string): TJTAGPin;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if TJTAGPin(Items[i]).Name = name then
      begin
        Result := TJTAGPin(Items[i]);
        exit;
      end;
end;

constructor TJTAGPins.Create(AOwner: TJTAGDevice);
begin
  FParent := AOwner;
  inherited Create;
end;

procedure TJTAGPins.Clear;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    TObject(Items[i]).Free;
  inherited Clear;
end;

procedure TJTAGPins.SetBoundaryRegister(bsdlinfo: TStrings);
var
  i: Integer;
  tmp: string;
  HighestBit : Integer;
begin
  HighestBit := 0;
  Setlength(FBoundaryRegister,bsdlInfo.Count);
  for i := bsdlInfo.Count-1 downto 0 do
    begin
      FBoundaryRegister[i].read := False;
      tmp := bsdlInfo[bsdlInfo.Count-i-1];
      if IsNumeric(copy(tmp,0,pos(' ',tmp)-1)) then
        FBoundaryRegister[i].CellNumber :=StrToInt(copy(tmp,0,pos(' ',tmp)-1));
      if FBoundaryRegister[i].CellNumber > HighestBit then HighestBit := FBoundaryRegister[i].CellNumber;
      tmp := copy(tmp,pos('(',tmp)+1,length(tmp));
      if copy(trim(copy(tmp,0,pos(',',tmp)-1)),0,3) = 'BC_' then
        FBoundaryRegister[i].Cell := StrToInt(copy(trim(copy(tmp,0,pos(',',tmp)-1)),4,1));
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
      FBoundaryRegister[i].Port:=trim(copy(tmp,0,pos(',',tmp)-1));
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
      if lowercase(trim(copy(tmp,0,pos(',',tmp)-1))) = 'internal' then
        FBoundaryRegister[i].CellFunction := bfInternal
      else if lowercase(trim(copy(tmp,0,pos(',',tmp)-1))) = 'input' then
        FBoundaryRegister[i].CellFunction := bfInput
      else if lowercase(trim(copy(tmp,0,pos(',',tmp)-1))) = 'control' then
        FBoundaryRegister[i].CellFunction := bfControl
      else if lowercase(trim(copy(tmp,0,pos(',',tmp)-1))) = 'controlr' then
        FBoundaryRegister[i].CellFunction := bfControlR
      else if lowercase(trim(copy(tmp,0,pos(',',tmp)-1))) = 'output2' then
        FBoundaryRegister[i].CellFunction := bfOutput2
      else if lowercase(trim(copy(tmp,0,pos(',',tmp)-1))) = 'output3' then
        FBoundaryRegister[i].CellFunction := bfOutput3
      else if lowercase(trim(copy(tmp,0,pos(',',tmp)-1))) = 'bidir' then
        FBoundaryRegister[i].CellFunction := bfBiDir
      else if lowercase(trim(copy(tmp,0,pos(',',tmp)-1))) = 'clock' then
        FBoundaryRegister[i].CellFunction := bfClock
      else if lowercase(trim(copy(tmp,0,pos(',',tmp)-1))) = 'observe_only' then
        FBoundaryRegister[i].CellFunction := bfObserveOnly
      else raise Exception.Create(Format(strUnknownCellType,[lowercase(trim(copy(tmp,0,pos(',',tmp)-1)))]));
      tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
      FBoundaryRegister[i].safe:=copy(trim(tmp),0,1)[1];
      if FBoundaryRegister[i].safe = '1' then FBoundaryRegister[i].value := True;
      if (pos(',',tmp) < pos(')',tmp)) and (pos(',',tmp) > 0) then
        begin //has ccell
          tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
          FBoundaryRegister[i].control_cell:=StrToInt(trim(copy(tmp,0,pos(',',tmp)-1)));
          tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
          FBoundaryRegister[i].disable_value:=copy(trim(tmp),0,1)[1];
          tmp := copy(tmp,pos(',',tmp)+1,length(tmp));
          if lowercase(trim(copy(tmp,0,pos(')',tmp)-1))) = 'z' then
            FBoundaryRegister[i].disable_result:=brZ
          else if lowercase(trim(copy(tmp,0,pos(')',tmp)-1))) = 'pull1' then
            FBoundaryRegister[i].disable_result:=brPull1
          else if lowercase(trim(copy(tmp,0,pos(')',tmp)-1))) = 'pull0' then
            FBoundaryRegister[i].disable_result:=brPull0
          else if lowercase(trim(copy(tmp,0,pos(')',tmp)-1))) = 'weak1' then
            FBoundaryRegister[i].disable_result:=brWeak1
          else if lowercase(trim(copy(tmp,0,pos(')',tmp)-1))) = 'weak0' then
            FBoundaryRegister[i].disable_result:=brWeak0
          else if lowercase(trim(copy(tmp,0,pos(')',tmp)-1))) = 'keeper' then
            FBoundaryRegister[i].disable_result:=brKeeper
          else raise Exception.Create(Format(strUnknownRsltType,[lowercase(trim(copy(tmp,0,pos(')',tmp)-1)))]));
        end
      else
        begin //no ccell
          FBoundaryRegister[i].control_cell:=-1;
        end;
    end;
  FBoundaryLength := HighestBit;
end;

{ TJTAGPin }

function TJTAGPin.GetCells(idx : Integer): TJTAGBoundaryCell;
begin
  if idx < count then
    Result := PJTAGBoundarycell(Items[idx])^;
end;

function TJTAGPin.GetInState: TJTAGPinState;
begin
  Result := jsUnknown;
  if Count = 0 then exit;
  if not PJTAGBoundaryCell(Items[0])^.read then exit;
  if PJTAGBoundaryCell(Items[0])^.invalue then Result := jsOn
  else Result := jsOff;
  if Assigned(FDrawable) then
    begin
      case Result of
      jsUnknown: FDrawable.State := isUnknown;
      jsTriState: FDrawable.State := isTriState;
      jsOn: FDrawable.State := isOn;
      jsOff: FDrawable.State := isOff;
      end;
    end;
end;

function TJTAGPin.GetType: TBoundaryCellFunction;
begin
  if Count > 0 then Result := PJTAGBoundaryCell(Items[0])^.CellFunction;
end;

procedure TJTAGPin.SetName(const AValue: string);
var
  i: Integer;
begin
  if AValue = FName then exit
  else FName := AValue;
  Clear; //Clear all Cells
  for i := 0 to length(Parent.BoundaryRegister)-1 do
    if Parent.BoundaryRegister[i].Port = AValue then
      begin
        Add(@Parent.BoundaryRegister[i]);
        if Parent.BoundaryRegister[i].control_cell > -1 then
          begin
            Add(@Parent.BoundaryRegister[Parent.BoundaryRegister[i].control_cell]);
            //default disable control
            if Parent.BoundaryRegister[Parent.BoundaryRegister[i].control_cell].disable_value = '0' then
              Parent.BoundaryRegister[Parent.BoundaryRegister[i].control_cell].value := false
            else if Parent.BoundaryRegister[Parent.BoundaryRegister[i].control_cell].disable_value = '1' then
              Parent.BoundaryRegister[Parent.BoundaryRegister[i].control_cell].value :=true;
          end;
        exit;
      end;
end;

function TJTAGPin.GetState: TJTAGPinState;
begin
  Result := jsUnknown;
  if Count = 0 then exit;
  if not PJTAGBoundaryCell(Items[0])^.read then exit;
  if Count > 1 then
    begin
      if PJTAGBoundaryCell(Items[1])^.value = ((PJTAGBoundaryCell(Items[1])^.disable_value = '0')) then
        begin
          Result := jsTriState;
          exit;
        end;
    end;
  if PJTAGBoundaryCell(Items[0])^.value then Result := jsOn
  else Result := jsOff;
end;

procedure TJTAGPin.SetState(const AValue: TJTAGPinState);
begin
  if (AValue = jsUnknown) or (AValue = jsTriState) then
    begin
      if Count >=2 then
        PJTAGBoundarycell(Items[1])^.value := (PJTAGBoundarycell(Items[1])^.disable_value = '0');
      PJTAGBoundarycell(Items[0])^.value:=false;
    end
  else
    begin
      if Count >=2 then
        PJTAGBoundarycell(Items[1])^.value := not (PJTAGBoundarycell(Items[1])^.disable_value = '0');
      PJTAGBoundarycell(Items[0])^.value:=(AValue = jsOn);
    end;
end;

constructor TJTAGPin.Create(AOwner: TJTAGPins);
begin
  FParent := AOwner;
  aOwner.Add(Self);
  inherited Create;
end;

{ TUSBLAbJTAGInterface }

constructor TUSBLAbJTAGInterface.Create(Dev : TLibUSBDevice);
begin
  FDev := Dev;
end;

destructor TUSBLAbJTAGInterface.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TUSBLAbJTAGInterface.Open;
var
  res: LongInt;
begin
  if FDev.OpenDevice then
    FDev.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,FUNC_OPEN, 0, 0,nil, 0, 5000);
end;

procedure TUSBLAbJTAGInterface.Close;
var
  res: LongInt;
begin
  if FDev.OpenDevice then
    FDev.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,FUNC_CLOSE, 0, 0,nil, 0, 5000);
  FDev.CloseDevice;
end;

function TUSBLAbJTAGInterface.SetTapState(NewState: TTapState): Boolean;
var
  aTapState : Byte;
  aTapStateIn : Byte;
  res: LongInt;
begin
  if FDev.OpenDevice then
    begin
      aTapState := byte(NewState);
      res := FDev.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,FUNC_TAP_STATE, aTapState, 0,nil, 0, 5000);
      res := FDev.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,FUNC_GET_TAP_STATE, 0, 0,@aTapStateIn, 1, 5000);
      FTapState := TTapState(aTapStateIn);
      Result := aTapState = aTapStateIn;
    end;
end;

function TUSBLAbJTAGInterface.ShiftInData(Data: Pointer; length: Integer
  ): Boolean;
var
  res: LongInt;
begin
  if FDev.OpenDevice then
    begin
      res := FDev.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_OUT,FUNC_WRITE_CHAIN, 0, 0,Data, length, 5000);
      Result := res = length;
      if Result then
        Result := FDev.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_OUT,FUNC_EXECCHAIN, 0, 0,nil, 0, 5000) = 0;
    end;
end;

function TUSBLAbJTAGInterface.GetChainSize: Integer;
var
  ChainSize: Word;
begin
  if FDev.OpenDevice then
    begin
      FDev.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,FUNC_CHAINSIZE, 0, 0,@ChainSize, 2, 5000);
      Result := ChainSize;
    end;
end;

function TUSBLAbJTAGInterface.ShiftOutData(Data: Pointer; length: Integer
  ): Boolean;
var
  res: LongInt;
begin
  if FDev.OpenDevice then
    begin
      res := FDev.SendControlMsg(USB_TYPE_VENDOR or USB_RECIP_DEVICE or USB_ENDPOINT_IN,FUNC_READ_CHAIN, 0, 0,Data, length, 5000);
      Result := res = length;
    end;
end;

initialization
  {$I uscanchain.lrs}

end.

