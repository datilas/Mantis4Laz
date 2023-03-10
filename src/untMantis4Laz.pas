Unit untMantis4Laz;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

Interface

Uses
  Classes, SysUtils, untIssues;

Type

  { TMantis4Laz }

  { TRet_http }

  TRet_http = Class(TObject)
  private
    FBody: String;
    FExecuteTime: Int64;
    FLastError: Integer;
    FLastErrorDesc: String;
    FResultCode: Integer;
    FResultString: String;

    Procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
    //*
    property ResultCode: Integer Read FResultCode Write FResultCode;
    property ResultString: String Read FResultString Write FResultString;
    property LastError: Integer Read FLastError Write FLastError;
    property LastErrorDesc: String Read FLastErrorDesc Write FLastErrorDesc;
    property Body: String Read FBody Write FBody;
    property ExecuteTime: Int64 Read FExecuteTime Write FExecuteTime;
  end;

  { TMantis4Laz }

  TMantis4Laz = Class(TObject)
  private
    FApiToken: String;
    FIssues: TIssues;
    FRet_http: TRet_http;
    FUrlBase: String;
    Function GetUrlBase: String;
    Procedure SetUrlBase(AValue: String);
    //*
    Procedure PostHttp(Const aEndPoint, aBody: String);
  public
    constructor Create;
    destructor Destroy; override;
    //*
    Procedure PostIssues;
    //*
    property UrlBase: String Read GetUrlBase Write SetUrlBase;
    property ApiToken: String Read FApiToken Write FApiToken;
    property Issues: TIssues Read FIssues Write FIssues;
    property RetHttp: TRet_http Read FRet_http;
  End;

Implementation

Uses
  httpsend, synautil, blcksock, ssl_openssl3, {%H-}ssl_openssl, ssl_openssl_lib;

{ TRet_http }

Procedure TRet_http.Clear;
Begin
  FResultCode := 0;
  FResultString := '';
  FLastError := 0;
  FLastErrorDesc := '';
  FBody := '';
  FExecuteTime := 0;
End;

Constructor TRet_http.Create;
Begin
  Clear;
End;

Destructor TRet_http.Destroy;
Begin
  Inherited Destroy;
End;

{ TMantis4Laz }

Procedure TMantis4Laz.PostHttp(Const aEndPoint, aBody: String);
Var
  lHTTP: THTTPSend;
  lTime: Int64;
Begin
  lTime := GetTickCount64;
  RetHttp.Clear;

  lHTTP := THTTPSend.Create;
  Try
    lHTTP.Clear;
    lHTTP.Protocol := '1.1';
    lHTTP.KeepAlive := True;
    lHTTP.UserAgent := 'Mozilla/5.0 (Windows NT 6.1; Win632; x86; rv:47.0) Gecko/20100101 Firefox/47.0';
    lHTTP.Sock.SSL.SSLType := LT_all;
    lHTTP.Headers.Add('Authorization: ' + ApiToken);
    lHTTP.MimeType := 'application/json';

    If aBody <> '' Then
      WriteStrToStream(lHTTP.Document, aBody);

    lHTTP.HTTPMethod('POST', UrlBase + aEndPoint);

    RetHttp.ResultCode := lHTTP.ResultCode;
    RetHttp.ResultString := lHTTP.ResultString;
    RetHttp.LastError := lHTTP.Sock.LastError;
    RetHttp.LastErrorDesc := lHTTP.Sock.LastErrorDesc;
    RetHttp.Body := String(ReadStrFromStream(lHTTP.Document, lHTTP.Document.Size));
    RetHttp.ExecuteTime := GetTickCount64 - lTime;
  Finally
    lHTTP.Free;
  End;
End;

Function TMantis4Laz.GetUrlBase: String;
Begin
  If FUrlBase[Length( FUrlBase)] = '/' Then
    SetLength(FUrlBase, (Length(FUrlBase) - 1));

  Result := FUrlBase;
end;

Procedure TMantis4Laz.SetUrlBase(AValue: String);
Begin
  FUrlBase := AValue;
end;

Constructor TMantis4Laz.Create;
Begin
  FApiToken := '';
  FUrlBase := '';
  FIssues := TIssues.Create;
  FRet_http := TRet_http.Create;
End;

Destructor TMantis4Laz.Destroy;
Begin
  FreeAndNil(FIssues);
  FreeAndNil(FRet_http);
  Inherited Destroy;
End;

Procedure TMantis4Laz.PostIssues;
Begin
  PostHttp('/api/rest/issues', Issues.Json_Issues);
End;

End.
