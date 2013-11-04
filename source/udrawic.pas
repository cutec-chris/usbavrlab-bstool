unit uDrawIC;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, lcltype, lclintf;

type
  TDrawableICPinState = (isUnknown,isTriState,isOn,isOff);

  { TDrawableICPin }

  TDrawableICPin = class(TObject)
  private
    FName: string;
    FNumber: Integer;
    FRect: TRect;
    FState: TDrawableICPinState;
  public
    property Number : Integer read FNumber;
    property Name : string read FName;
    property State : TDrawableICPinState read FState write FState;
    property Rect : TRect read FRect write FRect;
    constructor Create(aNumber : Integer;aName : string);
  end;

  { TDrawableICPinout }

  TDrawableICPinout = class(TList)
  private
    FName : string;
    function GetPins(Idx : Integer): TDrawableICPin;
    procedure DrawDIP(Canvas : TCanvas;aRect : TRect);
    procedure DrawTQFP(Canvas : TCanvas;aRect : TRect);
    procedure DrawMLF(Canvas : TCanvas;aRect : TRect);
  public
    constructor Create(aName : string);
    property Pins[Idx : Integer] : TDrawableICPin read GetPins;
    procedure Draw(Canvas : TCanvas;aRect : TRect);
    function GetPinAt(x,y : Integer) : TDrawableICPin;
  end;

implementation

{ TDrawableICPin }

constructor TDrawableICPin.Create(aNumber: Integer; aName: string);
begin
  FName := aName;
  FNumber := aNumber;
  fState := isUnknown;
end;

{ TDrawableICPinout }

function TDrawableICPinout.GetPins(Idx : Integer): TDrawableICPin;
var
  i: Integer;
begin
  Result := nil;
  if Idx <= Count then
    for i := 0 to Count-1 do if TDrawableICPin(Items[i]).Number = Idx then
      begin
        Result := TDrawableICPin(Items[i]);
      end;
end;

const
  clTriState = $0086CBFF;
  clOn = $007E86F8;
  clOff = $00F8C77E;

procedure TDrawableICPinout.DrawDIP(Canvas: TCanvas; aRect: TRect);
var
  ICWidth: Integer;
  PinWidth: Integer;
  ICHeight: Integer;
  WholeWidth: Integer;
  MiddleX: Integer;
  MiddleY: Integer;
  PinHeight: LongInt;
  aX: Integer;
  aY: Integer;
  i: Integer;
  FontWidth: Integer;
  Pin: TDrawableICPin;
  label WidthChange;
begin
  Canvas.Brush.Color:=clWindow;
  Canvas.Pen.Color:=Canvas.Brush.Color;
  Canvas.Rectangle(aRect);
  if Count = 0 then exit;
  MiddleX := (aRect.Right-aRect.Left) div 2;
  MiddleY := (arect.Bottom-aRect.Top) div 2;
  ICHeight := (aRect.Bottom-aRect.Top);
WidthChange:
  PinHeight := round(ICHeight / ((Count / 2)+(Count / 4)));
  Canvas.Font.Height:=PinHeight;
  FontWidth := 0;
  for i := 0 to Count-1 do
    if Canvas.TextExtent(TDrawableICPin(items[i]).Name).cx > FontWidth then FontWidth := Canvas.TextExtent(TDrawableICPin(items[i]).Name).cx;
  ICWidth := round(ICHeight / 2.5);
  PinWidth := (ICWidth div 8);
  WholeWidth := ICWidth+(2*PinWidth)+(2*FontWidth);
  if WholeWidth > aRect.Right-aRect.Left then
    begin
      ICHeight := ICHeight-5;
      goto WidthChange;
    end;
  Canvas.Pen.Color:=clBlack;
  Canvas.Rectangle(aRect.Left+(MiddleX-ICWidth div 2),aRect.Top+(MiddleY-ICHeight div 2),aRect.Left+(MiddleX+ICWidth div 2),aRect.Top+(MiddleY+ICHeight div 2));
  Canvas.Arc(aRect.Left+MiddleX-(ICWidth div 8),aRect.Top+(MiddleY-ICHeight div 2)-(ICWidth div 8),aRect.Left+MiddleX+(ICWidth div 8),aRect.Top+(MiddleY-ICHeight div 2)+(ICWidth div 8),16*180,16*180);
  aX := aRect.Left+(MiddleX-ICWidth div 2);
  aY := aRect.Top+(MiddleY-ICHeight div 2)+(PinHeight div 2);
  for i := 1 to Count div 2 do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          Canvas.TextOut(aX-Canvas.TextExtent(Pin.Name).cx-PinHeight-4,aY,Pin.Name);
          Canvas.TextOut(aX+3,aY,IntToStr(Pin.Number));
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(aX-PinHeight,aY,aX+1,aY+PinHeight);
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(aX-PinHeight,aY,aX+1,aY+PinHeight);
      aY := aY+PinHeight+(PinHeight div 2);
    end;
  aX := aRect.Left+(MiddleX+ICWidth div 2);
  aY := aRect.Top+(MiddleY-ICHeight div 2)+(PinHeight div 2);
  for i := Count downto (Count div 2)+1 do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          Canvas.TextOut(aX+PinHeight+4,aY,Pins[i].Name);
          Canvas.TextOut(aX-Canvas.TextExtent(IntToStr(Pins[i].Number)).cx-1,aY,IntToStr(Pins[i].Number));
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(aX-1,aY,aX+Pinheight,aY+PinHeight);
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(aX-1,aY,aX+Pinheight,aY+PinHeight);
      aY := aY+PinHeight+(PinHeight div 2);
    end;
end;

procedure DrawRotatedText(Canvas : TCanvas;x,y : Integer;Text : string;Angle : real);
var
   lf : TLogFont;
   BaFont : TFont;
begin
  BaFont := TFont.Create;
  BaFont.Assign(Canvas.Font);
   with Canvas do
     begin
       GetObject(Canvas.Font.Handle, SizeOf(LogFont), @lf);
       lf.lfEscapement := round(Angle*10);
       lf.lfOrientation := round(Angle*10);
       lf.lfHeight:=Font.Height;
       lf.lfOutPrecision := OUT_TT_ONLY_PRECIS;
       Font.Handle := CreateFontIndirect(lf) ;
       TextOut(x,y,Text);
     end;
  Canvas.Font.Assign(BaFont);
  BaFont.Free;
end;

procedure TDrawableICPinout.DrawTQFP(Canvas: TCanvas; aRect: TRect);
var
  MiddleX: Integer;
  MiddleY: Integer;
  FontWidth: Integer;
  i: Integer;
  ICWidth: Integer;
  ICHeight: LongInt;
  aX,aY: real;
  PinWidth: Integer;
  Pin: TDrawableICPin;
  PinHeight: real;
begin
  Canvas.Brush.Color:=clWindow;
  Canvas.Pen.Color:=Canvas.Brush.Color;
  Canvas.Rectangle(aRect);
  if Count = 0 then exit;
  MiddleX := (aRect.Right-aRect.Left) div 2;
  MiddleY := (arect.Bottom-aRect.Top) div 2;
  Canvas.Font.Height:=MiddleY div (Count div 4);
  FontWidth := 0;
  for i := 0 to Count-1 do
    if Canvas.TextExtent(TDrawableICPin(items[i]).Name).cx > FontWidth then FontWidth := Canvas.TextExtent(TDrawableICPin(items[i]).Name).cx;
  if (aRect.Bottom-aRect.Top) > (aRect.Right-aRect.Left) then
    begin
      ICWidth := (aRect.Right-aRect.Left)-(FontWidth*2+4);
      ICWidth := ICWidth-(ICWidth div 4);
      ICHeight := ICWidth;
    end
  else
    begin
      ICWidth := (aRect.Bottom-aRect.Top)-(FontWidth*2+4);
      ICWidth := ICWidth-(ICWidth div 4);
      ICHeight := ICWidth;
    end;
  Canvas.Pen.Color:=clBlack;
  Canvas.Rectangle(aRect.Left+(MiddleX-ICWidth div 2),aRect.Top+(MiddleY-ICHeight div 2),aRect.Left+(MiddleX+ICWidth div 2),aRect.Top+(MiddleY+ICHeight div 2));
  Canvas.Arc(aRect.Left+(MiddleX-ICWidth div 2)+(ICWidth div 10),aRect.Top+(MiddleY-ICHeight div 2)+(ICWidth div 10),aRect.Left+(MiddleX-ICWidth div 2)+2*(ICWidth div 30)+(ICWidth div 10),aRect.Top+(MiddleY-ICHeight div 2)+2*(ICWidth div 30)+(ICWidth div 10),0,16*360);
  PinHeight := ICHeight / ((Count / 2)+2);
  Canvas.Font.Height:=round(PinHeight*1.5);
  aX := aRect.Left+(MiddleX-ICWidth div 2);
  aY := aRect.Top+(MiddleY-ICHeight / 2)+(PinHeight*1.5);
  for i := 1 to Count div 4 do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          Canvas.TextOut(round(aX-Canvas.TextExtent(Pin.Name).cx-PinHeight*2-4),round(aY-PinHeight / 3),Pin.Name);
          Canvas.TextOut(round(aX+3),round(aY-(PinHeight / 3)),IntToStr(Pin.Number));
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(round(aX-PinHeight-(PinHeight / 3)-PinHeight),round(aY),round(aX+1),round(aY+PinHeight));
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(round(aX-PinHeight),round(aY),round(aX+1),round(aY+PinHeight));
      Canvas.Rectangle(round(aX-PinHeight-(PinHeight / 3)),round(aY),round(aX-PinHeight+1),round(aY+PinHeight));
      Canvas.Rectangle(round(aX-PinHeight-(PinHeight / 3)-PinHeight),round(aY),round(aX-PinHeight-(PinHeight / 3)+1),round(aY+PinHeight));
      aY := aY+PinHeight*2;
    end;
  aX := aRect.Left+(MiddleX-ICWidth / 2)+(PinHeight*1.5);
  aY := aRect.Top+(MiddleY+ICHeight div 2);
  Canvas.Font.Height:=round(PinHeight*1.5);
  for i := (Count div 4)+1 to ((Count div 4)*2) do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          DrawRotatedText(Canvas,round(aX-(PinHeight / 3)),round(aY+Canvas.TextExtent(Pin.Name).cx+PinHeight*2+4),Pin.Name,90);
          DrawRotatedText(Canvas,round(aX-(PinHeight / 3)),round(aY-3),IntToStr(Pin.Number),90);
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(round(aX),round(aY-1),round(aX+PinHeight),round(aY-1+PinHeight+(PinHeight / 3)));
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(round(aX),round(aY-1),round(aX+PinHeight),round(aY+PinHeight));
      Canvas.Rectangle(round(aX),round(aY+PinHeight-1),round(aX+PinHeight),round(aY+PinHeight+(PinHeight / 3)));
      Canvas.Rectangle(round(aX),round(aY-1+PinHeight+(PinHeight / 3)),round(aX+PinHeight),round(aY+PinHeight*2+(PinHeight / 3)));
      aX := aX+PinHeight*2;
    end;
  aX := aRect.Left+(MiddleX+ICWidth div 2)+(PinHeight*2+(PinHeight / 3))-1;
  aY := aRect.Top+(MiddleY-ICHeight / 2)+(PinHeight*1.5);
  for i := ((Count div 4)*3) downto ((Count div 4)*2)+1 do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          Canvas.TextOut(round(aX+4),round(aY-PinHeight / 3),Pin.Name);
          Canvas.TextOut(round(aX-(PinHeight*2+(PinHeight / 3))-3-Canvas.TextExtent(IntToStr(Pin.Number)).cx),round(aY-(PinHeight / 3)),IntToStr(Pin.Number));
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(round(aX-PinHeight-(PinHeight / 3)-PinHeight),round(aY),round(aX+1),round(aY+PinHeight));
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(round(aX-PinHeight),round(aY),round(aX+1),round(aY+PinHeight));
      Canvas.Rectangle(round(aX-PinHeight-(PinHeight / 3)),round(aY),round(aX-PinHeight+1),round(aY+PinHeight));
      Canvas.Rectangle(round(aX-PinHeight-(PinHeight / 3)-PinHeight),round(aY),round(aX-PinHeight-(PinHeight / 3)+1),round(aY+PinHeight));
      aY := aY+PinHeight*2;
    end;
  aX := aRect.Left+(MiddleX-ICWidth / 2)+(PinHeight*1.5);
  aY := aRect.Top+(MiddleY-ICHeight div 2)-(PinHeight*2+(PinHeight / 3))+1;
  Canvas.Font.Height:=round(PinHeight*1.5);
  for i := ((Count div 4)*4) downto ((Count div 4)*3)+1 do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          DrawRotatedText(Canvas,round(aX-(PinHeight / 3)),round(aY-3),Pin.Name,90);
          DrawRotatedText(Canvas,round(aX-(PinHeight / 3)),round(aY+Canvas.TextExtent(IntToStr(Pin.Number)).cx+PinHeight*2+6),IntToStr(Pin.Number),90);
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(round(aX),round(aY-1),round(aX+PinHeight),round(aY-1+PinHeight+(PinHeight / 3)));
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(round(aX),round(aY-1),round(aX+PinHeight),round(aY+PinHeight));
      Canvas.Rectangle(round(aX),round(aY+PinHeight-1),round(aX+PinHeight),round(aY+PinHeight+(PinHeight / 3)));
      Canvas.Rectangle(round(aX),round(aY-1+PinHeight+(PinHeight / 3)),round(aX+PinHeight),round(aY+PinHeight*2+(PinHeight / 3)));
      aX := aX+PinHeight*2;
    end;
end;

procedure TDrawableICPinout.DrawMLF(Canvas: TCanvas; aRect: TRect);
var
  MiddleX: Integer;
  MiddleY: Integer;
  FontWidth: Integer;
  i: Integer;
  ICWidth: Integer;
  ICHeight: LongInt;
  aX,aY: real;
  PinWidth: Integer;
  Pin: TDrawableICPin;
  PinHeight: real;
begin
  Canvas.Brush.Color:=clWindow;
  Canvas.Pen.Color:=Canvas.Brush.Color;
  Canvas.Rectangle(aRect);
  if Count = 0 then exit;
  MiddleX := (aRect.Right-aRect.Left) div 2;
  MiddleY := (arect.Bottom-aRect.Top) div 2;
  Canvas.Font.Height:=MiddleY div (Count div 4);
  FontWidth := 0;
  for i := 0 to Count-1 do
    if Canvas.TextExtent(TDrawableICPin(items[i]).Name+' ('+IntToStr(TDrawableICPin(items[i]).Number)+')').cx > FontWidth then FontWidth := Canvas.TextExtent(TDrawableICPin(items[i]).Name+' ('+IntToStr(TDrawableICPin(items[i]).Number)+')').cx;
  if (aRect.Bottom-aRect.Top) > (aRect.Right-aRect.Left) then
    begin
      ICWidth := (aRect.Right-aRect.Left)-(FontWidth*2+4);
      ICHeight := ICWidth;
    end
  else
    begin
      ICWidth := (aRect.Bottom-aRect.Top)-(FontWidth*2+4);
      ICHeight := ICWidth;
    end;
  Canvas.Pen.Color:=clBlack;
  Canvas.Rectangle(aRect.Left+(MiddleX-ICWidth div 2),aRect.Top+(MiddleY-ICHeight div 2),aRect.Left+(MiddleX+ICWidth div 2),aRect.Top+(MiddleY+ICHeight div 2));
  Canvas.Arc(aRect.Left+(MiddleX-ICWidth div 2)+(ICWidth div 10),aRect.Top+(MiddleY-ICHeight div 2)+(ICWidth div 10),aRect.Left+(MiddleX-ICWidth div 2)+2*(ICWidth div 30)+(ICWidth div 10),aRect.Top+(MiddleY-ICHeight div 2)+2*(ICWidth div 30)+(ICWidth div 10),0,16*360);
  PinHeight := ICHeight / ((Count / 2)+2);
  Canvas.Font.Height:=round(PinHeight*1.5);
  aX := aRect.Left+(MiddleX-ICWidth div 2);
  aY := aRect.Top+(MiddleY-ICHeight / 2)+(PinHeight*1.5);
  for i := 1 to Count div 4 do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          Canvas.TextOut(round(aX-Canvas.TextExtent(Pin.Name+' ('+IntToStr(Pin.Number)+')').cx-4),round(aY-PinHeight / 3),Pin.Name+' ('+IntToStr(Pin.Number)+')');
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(round(aX-PinHeight / 3),round(aY),round(aX+(PinHeight / 3)*2),round(aY+PinHeight));
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(round(aX-PinHeight / 3),round(aY),round(aX+(PinHeight / 3)*2),round(aY+PinHeight));
      aY := aY+PinHeight*2;
    end;
  aX := aRect.Left+(MiddleX-ICWidth / 2)+(PinHeight*1.5);
  aY := aRect.Top+(MiddleY+ICHeight div 2)-(PinHeight / 3)*2;
  Canvas.Font.Height:=round(PinHeight*1.5);
  for i := (Count div 4)+1 to ((Count div 4)*2) do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          DrawRotatedText(Canvas,round(aX-(PinHeight / 3)),round(aY+Canvas.TextExtent(Pin.Name+' ('+IntToStr(Pin.Number)+')').cx+PinHeight+4),Pin.Name+' ('+IntToStr(Pin.Number)+')',90);
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(round(aX),round(aY-1),round(aX+PinHeight),round(aY+PinHeight));
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(round(aX),round(aY-1),round(aX+PinHeight),round(aY+PinHeight));
      aX := aX+PinHeight*2;
    end;
  aX := aRect.Left+(MiddleX+ICWidth div 2)+(Pinheight / 3);
  aY := aRect.Top+(MiddleY-ICHeight / 2)+(PinHeight*1.5);
  for i := ((Count div 4)*3) downto ((Count div 4)*2)+1 do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          Canvas.TextOut(round(aX+4),round(aY-PinHeight / 3),'('+IntToStr(Pin.Number)+') '+Pin.Name);
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(round(aX-PinHeight),round(aY),round(aX+1),round(aY+PinHeight));
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(round(aX-PinHeight),round(aY),round(aX+1),round(aY+PinHeight));
      aY := aY+PinHeight*2;
    end;
  aX := aRect.Left+(MiddleX-ICWidth / 2)+(PinHeight*1.5);
  aY := aRect.Top+(MiddleY-ICHeight div 2)-(PinHeight / 3);
  Canvas.Font.Height:=round(PinHeight*1.5);
  for i := ((Count div 4)*4) downto ((Count div 4)*3)+1 do
    begin
      Pin := Pins[i];
      if Assigned(Pin) then
        begin
          Canvas.Brush.Color := clWindow;
          DrawRotatedText(Canvas,round(aX-(PinHeight / 3)),round(aY-3),'('+IntToStr(Pin.Number)+') '+Pin.Name,90);
          case Pin.State of
          isUnknown:Canvas.Brush.Color := clWindow;
          isTriState:Canvas.Brush.Color := clTriState;
          isOn:Canvas.Brush.Color := clOn;
          isOff:Canvas.Brush.Color := clOff;
          end;
          Pin.Rect := Rect(round(aX),round(aY-1),round(aX+PinHeight),round(aY+PinHeight));
        end
      else Canvas.Brush.Color := clWindow;
      Canvas.Rectangle(round(aX),round(aY-1),round(aX+PinHeight),round(aY+PinHeight));
      aX := aX+PinHeight*2;
    end;
end;

constructor TDrawableICPinout.Create(aName: string);
begin
  inherited Create;
  FName := aName;
end;

procedure TDrawableICPinout.Draw(Canvas: TCanvas; aRect: TRect);
begin
  if pos('DIP',FName) > 0 then
    DrawDIP(Canvas,aRect)
  else if (pos('TQFP',FName) > 0) then
    DrawTQFP(Canvas,aRect)
  else if (pos('MLF',FName) > 0) then
    DrawMLF(Canvas,aRect)
  else
    begin
      Canvas.Brush.Color:=clWindow;
      Canvas.Pen.Color:=Canvas.Brush.Color;
      Canvas.Rectangle(aRect);
    end;
end;

function TDrawableICPinout.GetPinAt(x, y: Integer): TDrawableICPin;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    with TDrawableICPin(Items[i]) do
      if  (x > Rect.Left)
      and (y > Rect.Top)
      and (x < Rect.Right)
      and (y < Rect.Bottom) then
        begin
          Result := TDrawableICPin(Items[i]);
          exit;
        end;
end;

end.

