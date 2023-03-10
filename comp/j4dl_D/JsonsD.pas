{****************************************************************************
Copyright (c) 2014 Randolph

mail: rilyu@sina.com
https://github.com/rilyu/json4delphi

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

201804 - Fiy - VGS - Refactor FixedFloatToStr (best use case and optimization)
201805 - Add - VGS - Add OBjectToJson and JsonToObject, rtti based, cross platform Delphi10+ and FPC 3+Refactor
201807 - Fix - VGS - String unicode (\uxxx) encoding and decoding.

****************************************************************************}

unit JsonsD;

interface

uses Classes, SysUtils, JsonsUtilsExD;

type
  TJsonValueType = (jvNone, jvNull, jvString, jvNumber, jvBoolean, jvObject, jvArray, jvDateTime, jvInt64);
  TJsonStructType = (jsNone, jsArray, jsObject);
  TJsonNull = (null);
  TJsonEmpty = (empty);

type

  { TJsonBase }

  TJsonBase = class(TObject)
  private
    FOwner: TJsonBase;
    function GetOwner: TJsonBase;

  protected
    function GetOwnerName: string;
    procedure RaiseError(const Msg: string);
    procedure RaiseParseError(const JsonString: string);
    procedure RaiseAssignError(Source: TJsonBase);

  public
    constructor Create(AOwner: TJsonBase);
    destructor Destroy; override;

    procedure Parse(JsonString: string); virtual; abstract;
    function Stringify: string; virtual; abstract;

    procedure Assign(Source: TJsonBase); virtual; abstract;

    function Encode(const S: string): string;
    function Decode(const S: string): string;

    procedure Split(const S: string; const Delimiter: char; Strings: TStrings);

    function IsJsonObject(const S: string): boolean;
    function IsJsonArray(const S: string): boolean;
    function IsJsonString(const S: string): boolean;
    function IsJsonNumber(const S: string): boolean;
    function IsJsonDateTime(const S: string): boolean;
    function IsJsonInt64(const S: string): boolean;
    function IsJsonBoolean(const S: string): boolean;
    function IsJsonNull(const S: string): boolean;

    function AnalyzeJsonValueType(const S: string): TJsonValueType;

  public
    property Owner: TJsonBase read GetOwner;

  end;

  TJsonObject = class;
  TJsonArray = class;

  { TJsonValue }

  TJsonValue = class(TJsonBase)
  private
    FValueType: TJsonValueType;
    FStringValue: string;
    FNumberValue: extended;
    FDateTimeValue: TDateTime;
    FInt64Value: int64;
    FBooleanValue: boolean;
    FObjectValue: TJsonObject;
    FArrayValue: TJsonArray;

    function GetAsArray: TJsonArray;
    function GetAsBoolean: boolean;
    function GetAsDateTime: TDateTime;
    function GetAsInteger: int64;
    function GetAsNumber: extended;
    function GetAsObject: TJsonObject;
    function GetAsString: string;
    function GetIsNull: boolean;
    procedure SetAsBoolean(const Value: boolean);
    procedure SetAsDateTime(Value: TDateTime);
    procedure SetAsInteger(const Value: int64);
    procedure SetAsNumber(const Value: extended);
    procedure SetAsString(const Value: string);
    procedure SetIsNull(const Value: boolean);
    procedure SetAsArray(const Value: TJsonArray);
    procedure SetAsObject(const Value: TJsonObject);
    function GetIsEmpty: boolean;
    procedure SetIsEmpty(const Value: boolean);

  protected
    procedure RaiseValueTypeError(const AsValueType: TJsonValueType);

  public
    constructor Create(AOwner: TJsonBase);
    destructor Destroy; override;

    procedure Parse(JsonString: string); override;
    function Stringify: string; override;

    procedure Assign(Source: TJsonBase); override;

    procedure Clear;

  public
    property ValueType: TJsonValueType read FValueType;
    property AsString: string read GetAsString write SetAsString;
    property AsNumber: extended read GetAsNumber write SetAsNumber;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsInteger: int64 read GetAsInteger write SetAsInteger;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsObject: TJsonObject read GetAsObject write SetAsObject;
    property AsArray: TJsonArray read GetAsArray write SetAsArray;
    property IsNull: boolean read GetIsNull write SetIsNull;
    property IsEmpty: boolean read GetIsEmpty write SetIsEmpty;

  end;

  { TJsonArray }

  TJsonArray = class(TJsonBase)
  private
    FList: TList;
    function GetItems(Index: integer): TJsonValue;
    function GetCount: integer;
  public
    constructor Create(AOwner: TJsonBase = nil);
    destructor Destroy; override;

    procedure Parse(JsonString: string); override;
    function Stringify: string; override;

    procedure Assign(Source: TJsonBase); override;
    procedure Merge(Addition: TJsonArray);

    function Add: TJsonValue;
    function Insert(const Index: integer): TJsonValue;

    function Put(const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Value: TJsonNull): TJsonValue; overload;
    function Put(const Value: boolean): TJsonValue; overload;
    function Put(const Value: int64): TJsonValue; overload;
    function Put(const Value: extended): TJsonValue; overload;
    function Put(const Value: TDateTime): TJsonValue; overload;
    function Put(const Value: string): TJsonValue; overload;
    function Put(const Value: TJsonArray): TJsonValue; overload;
    function Put(const Value: TJsonObject): TJsonValue; overload;
    function Put(const Value: TJsonValue): TJsonValue; overload;

    procedure Delete(const Index: integer);
    procedure Clear;

  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TJsonValue read GetItems; default;

  end;

  TJsonPair = class(TJsonBase)
  private
    FName: string;
    FValue: TJsonValue;

    procedure SetName(const Value: string);

  public
    constructor Create(AOwner: TJsonBase; const AName: string = '');
    destructor Destroy; override;

    procedure Parse(JsonString: string); override;
    function Stringify: string; override;

    procedure Assign(Source: TJsonBase); override;

  public
    property Name: string read FName write SetName;
    property Value: TJsonValue read FValue;

  end;

  { TJsonObject }

  TJsonObject = class(TJsonBase)
  private
    FList: TList;
    FAutoAdd: boolean;
    function GetCount: integer;
    function GetItems(Index: integer): TJsonPair;
    function GetValues(Name: string): TJsonValue;
  public
    constructor Create(AOwner: TJsonBase = nil);
    destructor Destroy; override;

    procedure Parse(JsonString: string); override;
    function Stringify: string; override;

    procedure Assign(Source: TJsonBase); override;
    procedure Merge(Addition: TJsonObject);

    function Add(const Name: string = ''): TJsonPair;
    function Insert(const Index: integer; const Name: string = ''): TJsonPair;

    function Put(const Name: string; const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Name: string; const Value: TJsonNull): TJsonValue; overload;
    function Put(const Name: string; const Value: boolean): TJsonValue; overload;
    function Put(const Name: string; const Value: int64): TJsonValue; overload;
    function Put(const Name: string; const Value: extended): TJsonValue; overload;
    function Put(const Name: string; const Value: TDateTime): TJsonValue; overload;
    function Put(const Name: string; const Value: string): TJsonValue; overload;
    function Put(const Name: string; const Value: TJsonArray): TJsonValue; overload;
    function Put(const Name: string; const Value: TJsonObject): TJsonValue; overload;
    function Put(const Name: string; const Value: TJsonValue): TJsonValue; overload;
    function Put(const Value: TJsonPair): TJsonValue; overload;

    function Find(const Name: string): integer;

    procedure Delete(const Index: integer); overload;
    procedure Delete(const Name: string); overload;

    procedure Clear;

  public
    property Count: integer read GetCount;
    property Items[Index: integer]: TJsonPair read GetItems;
    property Values[Name: string]: TJsonValue read GetValues; default;
    property AutoAdd: boolean read FAutoAdd write FAutoAdd;

  end;

  { TJson }

  TJson = class(TJsonBase)
  private
    FStructType: TJsonStructType;
    FJsonArray: TJsonArray;
    FJsonObject: TJsonObject;

    function GetCount: integer;
    function GetJsonArray: TJsonArray;
    function GetJsonObject: TJsonObject;
    function GetValues(Name: string): TJsonValue;
  protected
    procedure CreateArrayIfNone;
    procedure CreateObjectIfNone;

    procedure RaiseIfNone;
    procedure RaiseIfNotArray;
    procedure RaiseIfNotObject;

    procedure CheckJsonArray;
    procedure CheckJsonObject;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Parse(JsonString: string); override;
    function Stringify: string; override;

    procedure Assign(Source: TJsonBase); override;

    procedure Delete(const Index: integer); overload;
    procedure Delete(const Name: string); overload;

    procedure Clear;

    function Get(const Index: integer): TJsonValue; overload; //for both
    function Get(const Name: string): TJsonValue; overload; //for JsonObject

    //for JsonArray
    function Put(const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Value: TJsonNull): TJsonValue; overload;
    function Put(const Value: boolean): TJsonValue; overload;
    function Put(const Value: int64): TJsonValue; overload;
    function Put(const Value: extended): TJsonValue; overload;
    function Put(const Value: TDateTime): TJsonValue; overload;
    function Put(const Value: string): TJsonValue; overload;
    function Put(const Value: TJsonArray): TJsonValue; overload;
    function Put(const Value: TJsonObject): TJsonValue; overload;
    function Put(const Value: TJsonValue): TJsonValue; overload;
    function Put(const Value: TJson): TJsonValue; overload;

    //for JsonObject
    function Put(const Name: string; const Value: TJsonEmpty): TJsonValue; overload;
    function Put(const Name: string; const Value: TJsonNull): TJsonValue; overload;
    function Put(const Name: string; const Value: boolean): TJsonValue; overload;
    function Put(const Name: string; const Value: int64): TJsonValue; overload;
    function Put(const Name: string; const Value: extended): TJsonValue; overload;
    function Put(const Name: string; const Value: TDatetime): TJsonValue; overload;
    function Put(const Name: string; const Value: string): TJsonValue; overload;
    function Put(const Name: string; const Value: TJsonArray): TJsonValue; overload;
    function Put(const Name: string; const Value: TJsonObject): TJsonValue; overload;
    function Put(const Name: string; const Value: TJsonValue): TJsonValue; overload;
    function Put(const Name: string; const Value: TJson): TJsonValue; overload;
    function Put(const Value: TJsonPair): TJsonValue; overload;

  public
    property StructType: TJsonStructType read FStructType;
    property JsonObject: TJsonObject read GetJsonObject;
    property JsonArray: TJsonArray read GetJsonArray;

    property Count: integer read GetCount;
    property Values[Name: string]: TJsonValue read GetValues; default; //for JsonObject

  end;

implementation

{ TJsonBase }

function TJsonBase.AnalyzeJsonValueType(const S: string): TJsonValueType;
var
  Len: integer;
  Number: extended;
  i: int64;
begin
  Result := jvNone;
  Len := Length(S);
  if Len >= 2 then
  begin
    if (S[1] = '{') and (S[Len] = '}') then Result := jvObject
    else if (S[1] = '[') and (S[Len] = ']') then Result := jvArray
    else if (S[1] = '"') and (S[Len] = '"') then Result := jvString
    else if SameText(S, 'null') then Result := jvNull
    else if SameText(S, 'true') or SameText(S, 'false') then Result := jvBoolean
    else if JSONStringIsCompatibleDate(S) then Result := jvDateTime
    else if TryStrToInt64(S, i) then Result := jvInt64
    else if FixedTryStrToFloat(S, Number) then Result := jvNumber;
  end
  else if FixedTryStrToFloat(S, Number) then Result := jvNumber;
end;

constructor TJsonBase.Create(AOwner: TJsonBase);
begin
  FOwner := AOwner;
end;

function TJsonBase.Decode(const S: string): string;

  function HexValue(C: char): byte;
  begin
    case C of
      '0'..'9': Result := byte(C) - byte('0');
      'a'..'f': Result := (byte(C) - byte('a')) + 10;
      'A'..'F': Result := (byte(C) - byte('A')) + 10;
      else
        raise Exception.Create('Illegal hexadecimal characters "' + C + '"');
    end;
  end;

var
  I: integer;
  C: char;
  ubuf: integer;
begin
  Result := '';
  I := 1;
  while I <= Length(S) do
  begin
    C := S[I];
    Inc(I);
    if C = '\' then
    begin
      C := S[I];
      Inc(I);
      case C of
        'b': Result := Result + #8;
        't': Result := Result + #9;
        'n': Result := Result + #10;
        'f': Result := Result + #12;
        'r': Result := Result + #13;
        'u':
        begin
          if not TryStrToInt('$' + Copy(S, I, 4), ubuf) then
            raise Exception.Create(format('Invalid unicode \u%s', [Copy(S, I, 4)]));
          Result := Result + Chr(ubuf);
          Inc(I, 4);
        end;
        else
          Result := Result + C;
      end;
    end
    else
      Result := Result + C;
  end;
end;

destructor TJsonBase.Destroy;
begin
  inherited Destroy;
end;

function TJsonBase.Encode(const S: string): string;
var
  I, UnicodeValue: integer;
  C: char;
begin
  Result := '';
  for I := 1 to Length(S) do
  begin
    C := S[I];
    case C of
      '"': Result := Result + '\' + C;
      '\': Result := Result + '\' + C;
      '/': Result := Result + '\' + C;
      #8: Result := Result + '\b';
      #9: Result := Result + '\t';
      #10: Result := Result + '\n';
      #12: Result := Result + '\f';
      #13: Result := Result + '\r';
      else
        if (Ord(C) < 32) or (Ord(C) > 127) then
        begin
          Result := Result + '\u';
          UnicodeValue := Ord(C);
          Result := Result + lowercase(IntToHex((UnicodeValue and 61440) shr 12, 1));
          Result := Result + lowercase(IntToHex((UnicodeValue and 3840) shr 8, 1));
          Result := Result + lowercase(IntToHex((UnicodeValue and 240) shr 4, 1));
          Result := Result + lowercase(IntToHex((UnicodeValue and 15), 1));
        end
        else
          Result := Result + C;

    end;
  end;
end;

function TJsonBase.GetOwner: TJsonBase;
begin
  Result := FOwner;
end;

function TJsonBase.GetOwnerName: string;
var
  TheOwner: TJsonBase;
begin
  Result := '';
  TheOwner := Owner;
  while True do
  begin
    if not Assigned(TheOwner) then Break
    else if TheOwner is TJsonPair then
    begin
      Result := (TheOwner as TJsonPair).Name;
      Break;
    end
    else
      TheOwner := TheOwner.Owner;
  end;
end;

function TJsonBase.IsJsonArray(const S: string): boolean;
var
  Len: integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '[') and (S[Len] = ']');
end;

function TJsonBase.IsJsonBoolean(const S: string): boolean;
begin
  Result := SameText(lowercase(S), 'true') or SameText(lowercase(S), 'false');
end;

function TJsonBase.IsJsonNull(const S: string): boolean;
begin
  Result := SameText(S, 'null');
end;

function TJsonBase.IsJsonNumber(const S: string): boolean;
var
  Number: extended;
begin
  Result := FixedTryStrToFloat(S, Number);
end;

function TJsonBase.IsJsonDateTime(const S: string): boolean;
begin
  Result := JSONStringIsCompatibleDate(S);
end;

function TJsonBase.IsJsonInt64(const S: string): boolean;
var
  i: int64;
begin
  Result := TryStrToInt64(S, i);
end;

function TJsonBase.IsJsonObject(const S: string): boolean;
var
  Len: integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '{') and (S[Len] = '}');
end;

function TJsonBase.IsJsonString(const S: string): boolean;
var
  Len: integer;
begin
  Len := Length(S);
  Result := (Len >= 2) and (S[1] = '"') and (S[Len] = '"');
end;

procedure TJsonBase.RaiseAssignError(Source: TJsonBase);
var
  SourceClassName: string;
begin
  if Source is TObject then SourceClassName := Source.ClassName
  else
    SourceClassName := 'nil';
  RaiseError(Format('assign error: %s to %s', [SourceClassName, ClassName]));
end;

procedure TJsonBase.RaiseError(const Msg: string);
var
  S: string;
begin
  S := Format('<%s>%s', [ClassName, Msg]);
  raise Exception.Create(S);
end;

procedure TJsonBase.RaiseParseError(const JsonString: string);
begin
  RaiseError(Format('"%s" parse error: %s', [GetOwnerName, JsonString]));
end;

procedure TJsonBase.Split(const S: string; const Delimiter: char; Strings: TStrings);

  function IsPairBegin(C: char): boolean;
  begin
    Result := (C = '{') or (C = '[') or (C = '"');
  end;

  function GetPairEnd(C: char): char;
  begin
    case C of
      '{': Result := '}';
      '[': Result := ']';
      '"': Result := '"';
      else
        Result := #0;
    end;
  end;

  function MoveToPair(P: PChar): PChar;
  var
    PairBegin, PairEnd: char;
    C: char;
  begin
    PairBegin := P^;
    PairEnd := GetPairEnd(PairBegin);
    Result := P;
    while Result^ <> #0 do
    begin
      Inc(Result);
      C := Result^;
      if C = PairEnd then Break
      else if (PairBegin = '"') and (C = '\') then Inc(Result)
      else if (PairBegin <> '"') and IsPairBegin(C) then Result := MoveToPair(Result);
    end;
  end;

var
  PtrBegin, PtrEnd: PChar;
  C: char;
  StrItem: string;
begin
  PtrBegin := PChar(S);
  PtrEnd := PtrBegin;
  while PtrEnd^ <> #0 do
  begin
    C := PtrEnd^;
    if C = Delimiter then
    begin
      StrItem := Trim(Copy(PtrBegin, 1, PtrEnd - PtrBegin));
      Strings.Add(StrItem);
      PtrBegin := PtrEnd + 1;
      PtrEnd := PtrBegin;
      Continue;
    end
    else if IsPairBegin(C) then PtrEnd := MoveToPair(PtrEnd);
    Inc(PtrEnd);
  end;
  StrItem := Trim(Copy(PtrBegin, 1, PtrEnd - PtrBegin));
  if StrItem <> '' then Strings.Add(StrItem);
end;

{ TJsonValue }

procedure TJsonValue.Assign(Source: TJsonBase);
var
  Src: TJsonValue;
begin
  Clear;
  if not (Source is TJsonValue) and not (Source is TJsonObject) and not (Source is TJsonArray) then
    RaiseAssignError(Source);
  if Source is TJsonObject then
  begin
    FValueType := jvObject;
    FObjectValue := TJsonObject.Create(Self);
    FObjectValue.Assign(Source);
  end
  else if Source is TJsonArray then
  begin
    FValueType := jvArray;
    FArrayValue := TJsonArray.Create(Self);
    FArrayValue.Assign(Source);
  end
  else if Source is TJsonValue then
  begin
    Src := Source as TJsonValue;
    FValueType := Src.FValueType;
    case FValueType of
      jvNone, jvNull: ;
      jvString: FStringValue := Src.FStringValue;
      jvInt64: FInt64Value := src.FInt64Value;
      jvNumber: FNumberValue := Src.FNumberValue;
      jvDateTime: FDateTimeValue := src.FDateTimeValue;
      jvBoolean: FBooleanValue := Src.FBooleanValue;
      jvObject:
      begin
        FObjectValue := TJsonObject.Create(Self);
        FObjectValue.Assign(Src.FObjectValue);
      end;
      jvArray:
      begin
        FArrayValue := TJsonArray.Create(Self);
        FArrayValue.Assign(Src.FArrayValue);
      end;
    end;
  end;
end;

procedure TJsonValue.Clear;
begin
  case FValueType of
    jvNone, jvNull: ;
    jvString: FStringValue := '';
    jvInt64: FInt64Value := 0;
    jvNumber: FNumberValue := 0;
    jvDateTime: FDateTimeValue := 0;
    jvBoolean: FBooleanValue := False;
    jvObject:
    begin
      FObjectValue.Free;
      FObjectValue := nil;
    end;
    jvArray:
    begin
      FArrayValue.Free;
      FArrayValue := nil;
    end;
  end;
  FValueType := jvNone;
end;

constructor TJsonValue.Create(AOwner: TJsonBase);
begin
  inherited Create(AOwner);
  FStringValue := '';
  FNumberValue := 0;
  FInt64Value := 0;
  FDateTimeValue := 0;
  FBooleanValue := False;
  FObjectValue := nil;
  FArrayValue := nil;
  FValueType := jvNone;
end;

destructor TJsonValue.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJsonValue.GetAsArray: TJsonArray;
begin
  if IsEmpty then
  begin
    FValueType := jvArray;
    FArrayValue := TJsonArray.Create(Self);
  end;
  if FValueType <> jvArray then RaiseValueTypeError(jvArray);
  Result := FArrayValue;
end;

function TJsonValue.GetAsBoolean: boolean;
begin
  Result := False;
  case FValueType of
    jvNone, jvNull: Result := False;
    jvString: Result := SameText(lowercase(FStringValue), 'true');
    jvInt64: Result := (FInt64Value <> 0);
    jvNumber: Result := (FNumberValue <> 0);
    jvDateTime: Result := (FDateTimeValue > 0);
    jvBoolean: Result := FBooleanValue;
    jvObject, jvArray: RaiseValueTypeError(jvBoolean);
  end;
end;

function TJsonValue.GetAsDateTime: TDateTime;
begin
  Result := 0;
  case FValueType of
    jvNone, jvNull: Result := 0;
    jvString: Result := JSONStringToDate(FStringValue);
    jvInt64: Result := FInt64Value;
    jvNumber: Result := FNumberValue;
    jvDateTime: Result := FDateTimeValue;
    jvBoolean: Result := 0;
    jvObject, jvArray: RaiseValueTypeError(jvDateTime);
  end;
end;

function TJsonValue.GetAsInteger: int64;
begin
  Result := 0;
  case FValueType of
    jvNone, jvNull: Result := 0;
    jvString: Result := Trunc(FixedStrToFloat(FStringValue));
    jvInt64: Result := FInt64Value;
    jvNumber: Result := Trunc(FNumberValue);
    jvDateTime: Result := Trunc(FDateTimeValue);
    jvBoolean: Result := Ord(FBooleanValue);
    jvObject, jvArray: RaiseValueTypeError(jvInt64);
  end;
end;

function TJsonValue.GetAsNumber: extended;
begin
  Result := 0;
  case FValueType of
    jvNone, jvNull: Result := 0;
    jvString: Result := FixedStrToFloat(FStringValue);
    jvInt64: Result := FInt64Value;
    jvNumber: Result := FNumberValue;
    jvDateTime: Result := FDateTimeValue;
    jvBoolean: Result := Ord(FBooleanValue);
    jvObject, jvArray: RaiseValueTypeError(jvNumber);
  end;
end;

function TJsonValue.GetAsObject: TJsonObject;
begin
  if IsEmpty then
  begin
    FValueType := jvObject;
    FObjectValue := TJsonObject.Create(Self);
  end;
  if FValueType <> jvObject then RaiseValueTypeError(jvObject);
  Result := FObjectValue;
end;

function TJsonValue.GetAsString: string;
const
  BooleanStr: array[boolean] of string = ('false', 'true');
begin
  Result := '';
  case FValueType of
    jvNone, jvNull: Result := '';
    jvString: Result := FStringValue;
    jvInt64: Result := FInt64Value.ToString;
    jvNumber: Result := FixedFloatToStr(FNumberValue);
    jvDateTime: Result := JSONDateToString(FDateTimeValue);
    jvBoolean: Result := BooleanStr[FBooleanValue];
    jvObject, jvArray: RaiseValueTypeError(jvString);
  end;
end;

function TJsonValue.GetIsEmpty: boolean;
begin
  Result := (FValueType = jvNone);
end;

function TJsonValue.GetIsNull: boolean;
begin
  Result := (FValueType = jvNull);
end;

procedure TJsonValue.Parse(JsonString: string);
begin
  Clear;
  FValueType := AnalyzeJsonValueType(JsonString);
  case FValueType of
    jvNone: RaiseParseError(JsonString);
    jvNull: ;
    jvString: FStringValue := Decode(Copy(JsonString, 2, Length(JsonString) - 2));
    jvInt64: FInt64Value := StrToInt64Def(JsonString, 0);
    jvNumber: FNumberValue := FixedStrToFloat(JsonString);
    jvDateTime: FDateTimeValue := JSONStringToDate(JsonString);
    jvBoolean: FBooleanValue := SameText(JsonString, 'true');
    jvObject:
    begin
      FObjectValue := TJsonObject.Create(Self);
      FObjectValue.Parse(JsonString);
    end;
    jvArray:
    begin
      FArrayValue := TJsonArray.Create(Self);
      FArrayValue.Parse(JsonString);
    end;
  end;
end;

procedure TJsonValue.RaiseValueTypeError(const AsValueType: TJsonValueType);
const
  StrJsonValueType: array[TJsonValueType] of string = ('jvNone', 'jvNull', 'jvString', 'jvNumber', 'jvBoolean', 'jvObject', 'jvArray', 'jvDateTime', 'jvInt64');
var
  S: string;
begin
  S := Format('"%s" value type error: %s to %s', [GetOwnerName, StrJsonValueType[FValueType], StrJsonValueType[AsValueType]]);
  RaiseError(S);
end;

procedure TJsonValue.SetAsArray(const Value: TJsonArray);
begin
  if FValueType <> jvArray then
  begin
    Clear;
    FValueType := jvArray;
    FArrayValue := TJsonArray.Create(Self);
  end;
  FArrayValue.Assign(Value);
end;

procedure TJsonValue.SetAsBoolean(const Value: boolean);
begin
  if FValueType <> jvBoolean then
  begin
    Clear;
    FValueType := jvBoolean;
  end;
  FBooleanValue := Value;
end;

procedure TJsonValue.SetAsDateTime(Value: TDateTime);
begin
  if FValueType <> jvDateTime then
  begin
    Clear;
    FValueType := jvDateTime;
  end;
  FDateTimeValue := Value;
end;

procedure TJsonValue.SetAsInteger(const Value: int64);
begin
  if FValueType <> jvInt64 then
  begin
    Clear;
    FValueType := jvInt64;
  end;
  FInt64Value := Value;
end;

procedure TJsonValue.SetAsNumber(const Value: extended);
begin
  if FValueType <> jvNumber then
  begin
    Clear;
    FValueType := jvNumber;
  end;
  FNumberValue := Value;
end;

procedure TJsonValue.SetAsObject(const Value: TJsonObject);
begin
  if FValueType <> jvObject then
  begin
    Clear;
    FValueType := jvObject;
    FObjectValue := TJsonObject.Create(Self);
  end;
  FObjectValue.Assign(Value);
end;

procedure TJsonValue.SetAsString(const Value: string);
begin
  if FValueType <> jvString then
  begin
    Clear;
    FValueType := jvString;
  end;
  FStringValue := Value;
end;

procedure TJsonValue.SetIsEmpty(const Value: boolean);
const
  EmptyValueType: array[boolean] of TJsonValueType = (jvNull, jvNone);
begin
  if FValueType <> EmptyValueType[Value] then
  begin
    Clear;
    FValueType := EmptyValueType[Value];
  end;
end;

procedure TJsonValue.SetIsNull(const Value: boolean);
const
  NullValueType: array[boolean] of TJsonValueType = (jvNone, jvNull);
begin
  if FValueType <> NullValueType[Value] then
  begin
    Clear;
    FValueType := NullValueType[Value];
  end;
end;

function TJsonValue.Stringify: string;
const
  StrBoolean: array[boolean] of string = ('false', 'true');
begin
  Result := '';
  case FValueType of
    jvNone, jvNull: Result := 'null';
    jvString: Result := '"' + Encode(FStringValue) + '"';
    jvInt64: Result := FInt64Value.ToString;
    jvNumber: Result := FixedFloatToStr(FNumberValue);
    jvDateTime: Result := JSONDateToString(FDateTimeValue);
    jvBoolean: Result := StrBoolean[FBooleanValue];
    jvObject: Result := FObjectValue.Stringify;
    jvArray: Result := FArrayValue.Stringify;
  end;
end;

{ TJsonArray }

function TJsonArray.Add: TJsonValue;
begin
  Result := TJsonValue.Create(Self);
  FList.Add(Result);
end;

procedure TJsonArray.Assign(Source: TJsonBase);
var
  Src: TJsonArray;
  I: integer;
begin
  Clear;
  if not (Source is TJsonArray) then RaiseAssignError(Source);
  Src := Source as TJsonArray;
  for I := 0 to Src.Count - 1 do Add.Assign(Src[I]);
end;

procedure TJsonArray.Clear;
var
  I: integer;
  Item: TJsonValue;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Item := TJsonValue(FList[I]);
    Item.Free;
  end;
  FList.Clear;
end;

constructor TJsonArray.Create(AOwner: TJsonBase);
begin
  inherited Create(AOwner);
  FList := TList.Create;
end;

procedure TJsonArray.Delete(const Index: integer);
var
  Item: TJsonValue;
begin
  Item := TJsonValue(FList[Index]);
  Item.Free;
  FList.Delete(Index);
end;

destructor TJsonArray.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TJsonArray.GetCount: integer;
begin
  Result := FList.Count;
end;

function TJsonArray.GetItems(Index: integer): TJsonValue;
begin
  Result := TJsonValue(FList[Index]);
end;

function TJsonArray.Insert(const Index: integer): TJsonValue;
begin
  Result := TJsonValue.Create(Self);
  FList.Insert(Index, Result);
end;

procedure TJsonArray.Merge(Addition: TJsonArray);
var
  I: integer;
begin
  for I := 0 to Addition.Count - 1 do Add.Assign(Addition[I]);
end;

procedure TJsonArray.Parse(JsonString: string);
var
  I: integer;
  S: string;
  List: TStringList;
  Item: TJsonValue;
begin
  Clear;
  JsonString := Trim(JsonString);
  if not IsJsonArray(JsonString) then RaiseParseError(JsonString);
  S := Trim(Copy(JsonString, 2, Length(JsonString) - 2));
  List := TStringList.Create;
  try
    Split(S, ',', List);
    for I := 0 to List.Count - 1 do
    begin
      Item := Add;
      Item.Parse(List[I]);
    end;
  finally
    List.Free;
  end;
end;

function TJsonArray.Put(const Value: boolean): TJsonValue;
begin
  Result := Add;
  Result.AsBoolean := Value;
end;

function TJsonArray.Put(const Value: int64): TJsonValue;
begin
  Result := Add;
  Result.AsInteger := Value;
end;

function TJsonArray.Put(const Value: TJsonEmpty): TJsonValue;
begin
  Result := Add;
  Result.IsEmpty := True;
end;

function TJsonArray.Put(const Value: TJsonNull): TJsonValue;
begin
  Result := Add;
  Result.IsNull := True;
end;

function TJsonArray.Put(const Value: extended): TJsonValue;
begin
  Result := Add;
  Result.AsNumber := Value;
end;

function TJsonArray.Put(const Value: TDateTime): TJsonValue;
begin
  Result := Add;
  Result.AsDateTime := Value;
end;

function TJsonArray.Put(const Value: TJsonObject): TJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TJsonArray.Put(const Value: TJsonValue): TJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TJsonArray.Put(const Value: string): TJsonValue;
begin
  Result := Add;
  Result.AsString := Value;
end;

function TJsonArray.Put(const Value: TJsonArray): TJsonValue;
begin
  Result := Add;
  Result.Assign(Value);
end;

function TJsonArray.Stringify: string;
var
  I: integer;
  Item: TJsonValue;
begin
  Result := '[';
  for I := 0 to FList.Count - 1 do
  begin
    Item := TJsonValue(FList[I]);
    if I > 0 then Result := Result + ',';
    Result := Result + Item.Stringify;
  end;
  Result := Result + ']';
end;

{ TJsonPair }

procedure TJsonPair.Assign(Source: TJsonBase);
var
  Src: TJsonPair;
begin
  if not (Source is TJsonPair) then RaiseAssignError(Source);
  Src := Source as TJsonPair;
  FName := Src.FName;
  FValue.Assign(Src.FValue);
end;

constructor TJsonPair.Create(AOwner: TJsonBase; const AName: string);
begin
  inherited Create(AOwner);
  FName := AName;
  FValue := TJsonValue.Create(Self);
end;

destructor TJsonPair.Destroy;
begin
  FValue.Free;
  inherited Destroy;
end;

procedure TJsonPair.Parse(JsonString: string);
var
  List: TStringList;
  StrName: string;
begin
  List := TStringList.Create;
  try
    Split(JsonString, ':', List);
    if List.Count <> 2 then RaiseParseError(JsonString);
    StrName := List[0];
    if not IsJsonString(StrName) then RaiseParseError(StrName);
    FName := Decode(Copy(StrName, 2, Length(StrName) - 2));
    FValue.Parse(List[1]);
  finally
    List.Free;
  end;
end;

procedure TJsonPair.SetName(const Value: string);
begin
  FName := Value;
end;

function TJsonPair.Stringify: string;
begin
  Result := Format('"%s":%s', [Encode(FName), FValue.Stringify]);
end;

{ TJsonObject }

function TJsonObject.Add(const Name: string): TJsonPair;
begin
  Result := TJsonPair.Create(Self, Name);
  FList.Add(Result);
end;

procedure TJsonObject.Assign(Source: TJsonBase);
var
  Src: TJsonObject;
  I: integer;
begin
  Clear;
  if not (Source is TJsonObject) then RaiseAssignError(Source);
  Src := Source as TJsonObject;
  for I := 0 to Src.Count - 1 do Add.Assign(Src.Items[I]);
end;

procedure TJsonObject.Clear;
var
  I: integer;
  Item: TJsonPair;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Item := TJsonPair(FList[I]);
    Item.Free;
  end;
  FList.Clear;
end;

constructor TJsonObject.Create(AOwner: TJsonBase);
begin
  inherited Create(AOwner);
  FList := TList.Create;
  FAutoAdd := True;
end;

procedure TJsonObject.Delete(const Index: integer);
var
  Item: TJsonPair;
begin
  Item := TJsonPair(FList[Index]);
  Item.Free;
  FList.Delete(Index);
end;

procedure TJsonObject.Delete(const Name: string);
var
  Index: integer;
begin
  Index := Find(Name);
  if Index < 0 then RaiseError(Format('"%s" not found', [Name]));
  Delete(Index);
end;

destructor TJsonObject.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

function TJsonObject.Find(const Name: string): integer;
var
  I: integer;
  Pair: TJsonPair;
begin
  Result := -1;
  for I := 0 to FList.Count - 1 do
  begin
    Pair := TJsonPair(FList[I]);
    if SameText(Name, Pair.Name) then
    begin
      Result := I;
      Break;
    end;
  end;
end;

function TJsonObject.GetCount: integer;
begin
  Result := FList.Count;
end;

function TJsonObject.GetItems(Index: integer): TJsonPair;
begin
  Result := TJsonPair(FList[Index]);
end;

function TJsonObject.GetValues(Name: string): TJsonValue;
var
  Index: integer;
  Pair: TJsonPair;
begin
  Index := Find(Name);
  if Index < 0 then
  begin
    if not FAutoAdd then RaiseError(Format('%s not found', [Name]));
    Pair := Add(Name);
  end
  else
    Pair := TJsonPair(FList[Index]);
  Result := Pair.Value;
end;

function TJsonObject.Insert(const Index: integer; const Name: string): TJsonPair;
begin
  Result := TJsonPair.Create(Self, Name);
  FList.Insert(Index, Result);
end;

procedure TJsonObject.Merge(Addition: TJsonObject);
var
  I: integer;
begin
  for I := 0 to Addition.Count - 1 do Add.Assign(Addition.Items[I]);
end;

procedure TJsonObject.Parse(JsonString: string);
var
  I: integer;
  S: string;
  List: TStringList;
  Item: TJsonPair;
begin
  Clear;
  JsonString := Trim(JsonString);
  if not IsJsonObject(JsonString) then RaiseParseError(JsonString);
  S := Trim(Copy(JsonString, 2, Length(JsonString) - 2));
  List := TStringList.Create;
  try
    Split(S, ',', List);
    for I := 0 to List.Count - 1 do
    begin
      Item := Add;
      Item.Parse(List[I]);
    end;
  finally
    List.Free;
  end;
end;

function TJsonObject.Put(const Name: string; const Value: int64): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsInteger := Value;
end;

function TJsonObject.Put(const Name: string; const Value: extended): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsNumber := Value;
end;

function TJsonObject.Put(const Name: string; const Value: TDateTime): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsDateTime := Value;
end;

function TJsonObject.Put(const Name: string; const Value: boolean): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsBoolean := Value;
end;

function TJsonObject.Put(const Name: string; const Value: TJsonEmpty): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.IsEmpty := True;
end;

function TJsonObject.Put(const Name: string; const Value: TJsonNull): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.IsNull := True;
end;

function TJsonObject.Put(const Name: string; const Value: TJsonValue): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TJsonObject.Put(const Value: TJsonPair): TJsonValue;
var
  Pair: TJsonPair;
begin
  Pair := Add;
  Pair.Assign(Value);
  Result := Pair.Value;
end;

function TJsonObject.Put(const Name: string; const Value: TJsonObject): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TJsonObject.Put(const Name: string; const Value: string): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.AsString := Value;
end;

function TJsonObject.Put(const Name: string; const Value: TJsonArray): TJsonValue;
begin
  Result := Add(Name).Value;
  Result.Assign(Value);
end;

function TJsonObject.Stringify: string;
var
  I: integer;
  Item: TJsonPair;
begin
  Result := '{';
  for I := 0 to FList.Count - 1 do
  begin
    Item := TJsonPair(FList[I]);
    if I > 0 then Result := Result + ',';
    Result := Result + Item.Stringify;
  end;
  Result := Result + '}';
end;

{ TJson }

procedure TJson.Assign(Source: TJsonBase);
begin
  Clear;
  if Source is TJson then
  begin
    case (Source as TJson).FStructType of
      jsNone: ;
      jsArray:
      begin
        CreateArrayIfNone;
        FJsonArray.Assign((Source as TJson).FJsonArray);
      end;
      jsObject:
      begin
        CreateObjectIfNone;
        FJsonObject.Assign((Source as TJson).FJsonObject);
      end;
    end;
  end
  else if Source is TJsonArray then
  begin
    CreateArrayIfNone;
    FJsonArray.Assign(Source);
  end
  else if Source is TJsonObject then
  begin
    CreateObjectIfNone;
    FJsonObject.Assign(Source);
  end
  else if Source is TJsonValue then
  begin
    if (Source as TJsonValue).ValueType = jvArray then
    begin
      CreateArrayIfNone;
      FJsonArray.Assign((Source as TJsonValue).AsArray);
    end
    else if (Source as TJsonValue).ValueType = jvObject then
    begin
      CreateObjectIfNone;
      FJsonObject.Assign((Source as TJsonValue).AsObject);
    end
    else
      RaiseAssignError(Source);
  end
  else
    RaiseAssignError(Source);
end;

procedure TJson.CheckJsonArray;
begin
  CreateArrayIfNone;
  RaiseIfNotArray;
end;

procedure TJson.CheckJsonObject;
begin
  CreateObjectIfNone;
  RaiseIfNotObject;
end;

procedure TJson.Clear;
begin
  case FStructType of
    jsNone: ;
    jsArray:
    begin
      FJsonArray.Free;
      FJsonArray := nil;
    end;
    jsObject:
    begin
      FJsonObject.Free;
      FJsonObject := nil;
    end;
  end;
  FStructType := jsNone;
end;

constructor TJson.Create;
begin
  inherited Create(nil);
  FStructType := jsNone;
  FJsonArray := nil;
  FJsonObject := nil;
end;

procedure TJson.CreateArrayIfNone;
begin
  if FStructType = jsNone then
  begin
    FStructType := jsArray;
    FJsonArray := TJsonArray.Create(Self);
  end;
end;

procedure TJson.CreateObjectIfNone;
begin
  if FStructType = jsNone then
  begin
    FStructType := jsObject;
    FJsonObject := TJsonObject.Create(Self);
  end;
end;

procedure TJson.Delete(const Index: integer);
begin
  RaiseIfNone;
  case FStructType of
    jsArray: FJsonArray.Delete(Index);
    jsObject: FJsonObject.Delete(Index);
  end;
end;

procedure TJson.Delete(const Name: string);
begin
  RaiseIfNotObject;
  FJsonObject.Delete(Name);
end;

destructor TJson.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TJson.Get(const Index: integer): TJsonValue;
begin
  Result := nil;
  RaiseIfNone;
  case FStructType of
    jsArray: Result := FJsonArray.Items[Index];
    jsObject: Result := FJsonObject.Items[Index].Value;
  end;
end;

function TJson.Get(const Name: string): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Values[Name];
end;

function TJson.GetCount: integer;
begin
  case FStructType of
    jsArray: Result := FJsonArray.Count;
    jsObject: Result := FJsonObject.Count;
    else
      Result := 0;
  end;
end;

function TJson.GetJsonArray: TJsonArray;
begin
  CheckJsonArray;
  Result := FJsonArray;
end;

function TJson.GetJsonObject: TJsonObject;
begin
  CheckJsonObject;
  Result := FJsonObject;
end;

function TJson.GetValues(Name: string): TJsonValue;
begin
  Result := Get(Name);
end;

procedure TJson.Parse(JsonString: string);
begin
  Clear;
  JsonString := Trim(JsonString);
  if IsJsonArray(JsonString) then
  begin
    CreateArrayIfNone;
    FJsonArray.Parse(JsonString);
  end
  else if IsJsonObject(JsonString) then
  begin
    CreateObjectIfNone;
    FJsonObject.Parse(JsonString);
  end
  else
    RaiseParseError(JsonString);
end;

function TJson.Put(const Value: int64): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: extended): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: boolean): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonEmpty): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonNull): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: string): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonValue): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonObject): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Value: TJsonArray): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Name: string; const Value: int64): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: string; const Value: extended): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Value: TDateTime): TJsonValue;
begin
  CheckJsonArray;
  Result := FJsonArray.Put(Value);
end;

function TJson.Put(const Name: string; const Value: boolean): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: string; const Value: TJsonEmpty): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: string; const Value: TJsonNull): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: string; const Value: TJsonValue): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Value: TJsonPair): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Value);
end;

function TJson.Put(const Name: string; const Value: TJsonObject): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: string; const Value: TJsonArray): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Value: TJson): TJsonValue;
begin
  CheckJsonArray;
  case Value.FStructType of
    jsArray: Result := Put(Value.FJsonArray);
    jsObject: Result := Put(Value.FJsonObject);
    else
      Result := nil;
  end;
end;

function TJson.Put(const Name: string; const Value: TJson): TJsonValue;
begin
  CheckJsonObject;
  case Value.FStructType of
    jsArray: Result := Put(Name, Value.FJsonArray);
    jsObject: Result := Put(Name, Value.FJsonObject);
    else
      Result := nil;
  end;
end;

function TJson.Put(const Name: string; const Value: TDatetime): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

function TJson.Put(const Name: string; const Value: string): TJsonValue;
begin
  CheckJsonObject;
  Result := FJsonObject.Put(Name, Value);
end;

procedure TJson.RaiseIfNone;
begin
  if FStructType = jsNone then RaiseError('json struct type is jsNone');
end;

procedure TJson.RaiseIfNotArray;
begin
  if FStructType <> jsArray then RaiseError('json struct type is not jsArray');
end;

procedure TJson.RaiseIfNotObject;
begin
  if FStructType <> jsObject then RaiseError('json struct type is not jsObject');
end;

function TJson.Stringify: string;
begin
  case FStructType of
    jsArray: Result := FJsonArray.Stringify;
    jsObject: Result := FJsonObject.Stringify;
    else
      Result := '';
  end;
end;

end.
