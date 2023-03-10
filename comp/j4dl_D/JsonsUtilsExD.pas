unit JsonsUtilsExD;

interface

{$DEFINE LINEBREAKJSONFORMAT}//Desactivate for a non "minimal better human-readable format".

uses SysUtils;

function FixedFloatToStr(const Value: extended): string;
function FixedTryStrToFloat(const S: string; out Value: extended): boolean;
function FixedStrToFloat(const S: string): extended;
function JSONStringIsCompatibleDate(aJSONDate: string): boolean;
function JSONDateToString(aDate: TDateTime): string;
function JSONStringToDate(aDate: string): TDateTime;

const
  GLB_JSON_STD_DECIMALSEPARATOR = '.';

var
  JsonsUtils_GLB_DECIMALSEPARATOR: char;

implementation

uses
  DateUtils;

function ZeroFillStr(Number, Size: integer): string;
begin
  Result := IntToStr(Number);
  while length(Result) < Size do
    Result := '0' + Result;
end;

function JSONDateToString(aDate: TDateTime): string;
begin
  Result := '"' + ZeroFillStr(YearOf(aDate), 4) + '-' +
    ZeroFillStr(MonthOf(aDate), 2) + '-' +
    ZeroFillStr(DayOf(aDate), 2) + 'T' +
    ZeroFillStr(HourOf(aDate), 2) + ':' +
    ZeroFillStr(MinuteOf(aDate), 2) + ':' +
    ZeroFillStr(SecondOf(aDate), 2) + '.' +
    ZeroFillStr(SecondOf(aDate), 3) + 'Z"';
end;

function JSONStringToDate(aDate: string): TDateTime;
begin
  if JSONStringIsCompatibleDate(aDate) then
    Result :=
      EncodeDateTime(
      StrToInt(Copy(aDate, 01, 4)),
      StrToInt(Copy(aDate, 06, 2)),
      StrToInt(Copy(aDate, 09, 2)),
      StrToInt(Copy(aDate, 12, 2)),
      StrToInt(Copy(aDate, 15, 2)),
      StrToInt(Copy(aDate, 18, 2)),
      StrToInt(Copy(aDate, 21, 3)))
  else
    Result := 0;
end;

function JSONStringIsCompatibleDate(aJSONDate: string): boolean;
var
  ldummy: integer;
  lval, lnum: boolean;
begin
  lval := TryStrToInt(Copy(aJSONDate, 1, 4), ldummy) and TryStrToInt(Copy(aJSONDate, 6, 2), ldummy) and
    TryStrToInt(Copy(aJSONDate, 9, 2), ldummy) and TryStrToInt(Copy(aJSONDate, 12, 2), ldummy) and
    TryStrToInt(Copy(aJSONDate, 15, 2), ldummy) and TryStrToInt(Copy(aJSONDate, 18, 2), ldummy) and
    TryStrToInt(Copy(aJSONDate, 21, 3), ldummy);

  lnum := (Length(aJSONDate) = 24) and
    (aJSONDate[5] = '-') and
    (aJSONDate[8] = '-') and
    (aJSONDate[11] = 'T') and
    (aJSONDate[14] = ':') and
    (aJSONDate[17] = ':') and
    (aJSONDate[20] = '.') and
    (aJSONDate[24] = 'Z');

  Result := lval and lNum;
end;


{**
 * Fixed FloatToStr to convert DecimalSeparator to dot (.) decimal separator, FloatToStr returns
 * DecimalSeparator as decimal separator, but JSON uses dot (.) as decimal separator.
 *}
function GetDecimalSeparator: char;
  {$IFDEF FPC}
var
  LFormatSettings: TFormatSettings;
  {$ENDIF}
begin
  {$IFNDEF FPC}
  {$IFDEF DELPHIXE_UP}
  Result :=  FormatSettings.DecimalSeparator;
  {$ELSE}
  Result := FormatSettings.DecimalSeparator;
  {$ENDIF}
  {$ELSE}
  LFormatSettings := DefaultFormatSettings;
  Result :=  LFormatSettings.DecimalSeparator;
  {$ENDIF}
end;


function FixedFloatToStr(const Value: extended): string;
var
  lS: string;
begin
  lS := FloatToStr(Value);
  if JsonsUtils_GLB_DECIMALSEPARATOR = GLB_JSON_STD_DECIMALSEPARATOR then
  begin
    Result := LS;
  end
  else
  begin
    Result := StringReplace(lS,
      JsonsUtils_GLB_DECIMALSEPARATOR,
      GLB_JSON_STD_DECIMALSEPARATOR,
      [rfReplaceAll]);
  end;
end;

{**
 * Fixed TryStrToFloat to convert dot (.) decimal separator to DecimalSeparator, TryStrToFloat expects
 * decimal separator to be DecimalSeparator, but JSON uses dot (.) as decimal separator.
 *}
function FixedTryStrToFloat(const S: string; out Value: extended): boolean;
var
  FixedS: string;
begin
  if JsonsUtils_GLB_DECIMALSEPARATOR = GLB_JSON_STD_DECIMALSEPARATOR then
  begin
    Result := TryStrToFloat(S, Value);
  end
  else
  begin
    FixedS := StringReplace(S,
      GLB_JSON_STD_DECIMALSEPARATOR,
      JsonsUtils_GLB_DECIMALSEPARATOR,
      [rfReplaceAll]);
    Result := TryStrToFloat(FixedS, Value);
  end;
end;

{**
 * Fixed StrToFloat to convert dot (.) decimal separator to DecimalSeparator, StrToFloat expects
 * decimal separator to be DecimalSeparator, but JSON uses dot (.) as decimal separator.
 *}
function FixedStrToFloat(const S: string): extended;
var
  FixedS: string;
begin
  if JsonsUtils_GLB_DECIMALSEPARATOR = GLB_JSON_STD_DECIMALSEPARATOR then
  begin
    Result := StrToFloat(S);
  end
  else
  begin
    FixedS := StringReplace(S,
      GLB_JSON_STD_DECIMALSEPARATOR,
      JsonsUtils_GLB_DECIMALSEPARATOR,
      [rfReplaceAll]);
    Result := StrToFloat(FixedS);
  end;
end;

initialization
  JsonsUtils_GLB_DECIMALSEPARATOR := GetDecimalSeparator;

finalization

end.
