unit uBSDL;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type

  TEntityParameter = class;

  { TBSDLFile }

  TBSDLFile = class(TList)
  private
    function GetDeviceByIndex(Idx : Integer): TEntityParameter;
    function GetDeviceCount: Integer;
    function GetDevices(Name : string): TEntityParameter;
  public
    procedure LoadFromStream(Stream : TStream);
    procedure LoadFromFile(Filename : string);
    property  Devices[Name : string] : TEntityParameter read GetDevices;
    property  DevicesByIdx[Idx : Integer] : TEntityParameter read GetDeviceByIndex;
    property  DeviceCount : Integer read GetDeviceCount;
    procedure Clear; override;
  end;

  { TBSDLParameter }

  TBSDLParameter = class(TList)
  private
    FName: string;
  public
    constructor Create(aName : string);
    property Name : string read FName;
    procedure Clear; override;
  end;

  { TEntityParameter }

  TEntityParameter = class(TBSDLParameter)
  private
    function GetParameter(aName : string): TBSDLParameter;
  public
    property Parameters[aName : string] : TBSDLParameter read GetParameter;
  end;

  { TGenericParameter }

  TGenericParameter = class(TBSDLParameter)
  private
    FParameter: TStrings;
    FType: String;
  public
    constructor Create(aName,aType : string);
    destructor Destroy;override;
    property Parameter : TStrings read FParameter;
    property Typ : String read FType;
  end;

  { TAttribute }

  TAttribute = class(TBSDLParameter)
  private
    FParameter: TStrings;
  public
    constructor Create(aName : string);
    destructor Destroy;override;
    property Parameter : TStrings read FParameter;
  end;

  { TConstant }

  TConstant = class(TBSDLParameter)
  private
    FParameter: TStrings;
    FType: String;
  public
    constructor Create(aName,aType : string);
    destructor Destroy;override;
    property Parameter : TStrings read FParameter;
    property Typ : String read FType;
  end;

implementation

{ TBSDLFile }

function TBSDLFile.GetDeviceByIndex(Idx : Integer): TEntityParameter;
begin
  Result := nil;
  if Count > Idx then
    Result := TEntityParameter(Items[idx]);
end;

function TBSDLFile.GetDeviceCount: Integer;
begin
  Result := Count;
end;

function TBSDLFile.GetDevices(Name : string): TEntityParameter;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if  Assigned (Items[i])
    and (TObject(Items[i]) is TEntityParameter)
    and (TEntityParameter(Items[i]).Name = Name) then
      begin
        Result := TEntityParameter(Items[i]);
        exit;
      end;
end;

procedure TBSDLFile.LoadFromStream(Stream: TStream);
var
  sl: TStringList;
  tmp: String;
  aParent : TBSDLParameter = nil;
  aEntry: TBSDLParameter;

  function IsToken(tokenname :  string) : Boolean;
  begin
    Result := copy(Uppercase(tmp),0,length(tokenname)) = Uppercase(tokenname);
  end;
begin
  sl := TStringList.Create;
  sl.LoadFromStream(Stream);
  while sl.Count > 0 do
    begin
      tmp := trim(sl[0]);
      if IsToken('entity') then
        begin
          tmp := copy(tmp,7,length(tmp));
          aParent := TBSDLParameter(Items[Add(TEntityParameter.Create(trim(copy(tmp,0,pos('IS',uppercase(tmp))-1))))]);
        end
      else if IsToken('end') then
        aParent := nil
      else if Assigned(aParent) then
        begin
          if ISToken('attribute') then
            begin
              tmp := copy(tmp,10,length(tmp));
              aEntry := TBSDLParameter(aParent.Items[aParent.Add(TAttribute.Create(trim(copy(tmp,0,pos(' OF',uppercase(tmp))-1))))]);
              with aEntry as TAttribute do
                begin
                  tmp := copy(tmp,pos(' OF',Uppercase(tmp))+4,length(tmp));
                  tmp := copy(tmp,pos(' IS',Uppercase(tmp))+4,length(tmp));
                  while pos(';',tmp) = 0 do
                    begin
                      if copy(trim(tmp),0,2) = '--' then
                        begin
                          sl.Delete(0);
                          tmp := trim(sl[0]);
                          continue;
                        end;
                      if copy(trim(tmp),0,1) = '"' then
                        begin
                          tmp := copy(trim(tmp),2,length(tmp));
                          tmp := copy(tmp,0,pos('"',tmp)-1);
                        end;
                      if trim(tmp) <> '' then
                        Parameter.Add(trim(tmp));
                      sl.Delete(0);
                      tmp := trim(sl[0]);
                    end;
                  tmp := copy(tmp,0,pos(';',tmp)-1);
                  if copy(trim(tmp),0,1) = '"' then
                    begin
                      tmp := copy(trim(tmp),2,length(tmp));
                      tmp := copy(tmp,0,pos('"',tmp)-1);
                    end;
                  Parameter.Add(trim(tmp));
                end;
            end
          else if ISToken('constant') then
            begin
              tmp := copy(tmp,9,length(tmp));
              aEntry := TBSDLParameter(aParent.Items[aParent.Add(TConstant.Create(trim(copy(tmp,0,pos(':',tmp)-1)),copy(copy(tmp,pos(':',tmp)+1,length(tmp)),0,pos(':=',copy(tmp,pos(':',tmp)+1,length(tmp)))-1)))]);
              tmp := copy(tmp,pos(':=',Uppercase(tmp))+3,length(tmp));
              with aEntry as TConstant do
                begin
                  while pos(';',tmp) = 0 do
                    begin
                      if copy(trim(tmp),0,2) = '--' then
                        begin
                          sl.Delete(0);
                          tmp := trim(sl[0]);
                          continue;
                        end;
                      if copy(trim(tmp),0,1) = '"' then
                        begin
                          tmp := copy(trim(tmp),2,length(tmp));
                          tmp := copy(tmp,0,pos('"',tmp)-1);
                        end;
                      if trim(tmp) <> '' then
                        Parameter.Add(trim(tmp));
                      sl.Delete(0);
                      tmp := trim(sl[0]);
                    end;
                  tmp := copy(tmp,0,pos(';',tmp)-1);
                  if copy(trim(tmp),0,1) = '"' then
                    begin
                      tmp := copy(trim(tmp),2,length(tmp));
                      tmp := copy(tmp,0,pos('"',tmp)-1);
                    end;
                  Parameter.Add(trim(tmp));
                end;
            end
          else if ISToken('generic') then
            begin
              tmp := copy(tmp,pos('(',tmp)+1,length(tmp));
              aEntry := TBSDLParameter(aParent.Items[aParent.Add(TGenericParameter.Create(trim(copy(tmp,0,pos(':',tmp)-1)),copy(copy(tmp,pos(':',tmp)+1,length(tmp)),0,pos(':=',copy(tmp,pos(':',tmp)+1,length(tmp)))-1)))]);
              with aEntry as TGenericParameter do
                begin
                  tmp := copy(tmp,pos(':=',tmp)+2,length(tmp));
                  Parameter.Add(trim(StringReplace(copy(tmp,0,pos(')',tmp)-1),'"','',[rfReplaceAll])));
                end;
            end;
        end;
      sl.Delete(0);
    end;
  sl.Free;
end;

procedure TBSDLFile.LoadFromFile(Filename: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(Filename,fmOpenRead);
  LoadFromStream(f);
  f.Free;
end;

procedure TBSDLFile.Clear;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if Assigned(Items[i]) then TObject(Items[i]).Free;
  inherited Clear;
end;

{ TEntityParameter }

function TEntityParameter.GetParameter(aName : string): TBSDLParameter;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if  Assigned (Items[i])
    and (TObject(Items[i]) is TBSDLParameter)
    and (TBSDLParameter(Items[i]).Name = aName) then
      begin
        Result := TBSDLParameter(Items[i]);
        exit;
      end;
end;

{ TBSDLParameter }

constructor TBSDLParameter.Create(aName: string);
begin
  inherited Create;
  FName := aName;
end;

procedure TBSDLParameter.Clear;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if Assigned(Items[i]) then TObject(Items[i]).Free;
  inherited Clear;
end;

{ TAttribute }

constructor TAttribute.Create(aName: string);
begin
  inherited Create(aName);
  FParameter := TStringList.Create;
end;

destructor TAttribute.Destroy;
begin
  FParameter.Free;
  inherited Destroy;
end;

{ TConstant }

constructor TConstant.Create(aName, aType: string);
begin
  inherited Create(aName);
  FParameter := TStringList.Create;
  FType := trim(aType);
end;

destructor TConstant.Destroy;
begin
  FParameter.Free;
  inherited Destroy;
end;

{ TGenericParameter }

constructor TGenericParameter.Create(aName, aType: string);
begin
  inherited Create(aName);
  FParameter := TStringList.Create;
  FType := aType;
end;

destructor TGenericParameter.Destroy;
begin
  FParameter.Free;
  inherited Destroy;
end;

end.

