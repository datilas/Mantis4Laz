Unit Unit1;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

Uses untMantis4Laz;

{$R *.lfm}

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Var
  M4L: TMantis4Laz;
Begin
  M4L := TMantis4Laz.Create;
  try
    M4L.UrlBase := 'https://example.com/mantis/';
    M4L.ApiToken := 'aaaa-bbbbb-cccccc';

    M4L.Issues.Summary := 'teste Summary 1';
    M4L.Issues.Description := 'teste Description 1';

    M4L.Issues.Project.Id := 1;
    M4L.Issues.Project.Name := 'teste_nome';

    M4L.Issues.Category.Id := 1;
    M4L.Issues.Category.Name := 'General';

    M4L.Issues.Handler.Id := 1;
    M4L.Issues.Handler.Name := 'vboctor';

    M4L.Issues.View_state.Id := 10;
    M4L.Issues.View_state.Name := 'public';

    M4L.Issues.Priority.Id := 30;
    M4L.Issues.Priority.Name := 'normal';

    M4L.Issues.Severity.Id := 50;
    M4L.Issues.Severity.Name := 'minor';

    M4L.Issues.Reproducibility.Id := 70;
    M4L.Issues.Reproducibility.Name := 'have not tried';

    M4L.Issues.Sticky := False;

    with M4L.Issues.Tags.New Do
    Begin
      Id := 1;
      Name := 'mantishub';
    end;

    with M4L.Issues.Custom_fields.New Do
    Begin
      Id := 1;
      Name := 'controle';
      Value := '000999';
    end;

    with M4L.Issues.Custom_fields.New Do
    Begin
      Id := 2;
      Name := 'versao';
      Value := '1090';
    end;

    with M4L.Issues.Files.New Do
    Begin
      Name := 'test.txt';
      Content := 'VGhpcyBpcyBhIFRFU1QuDQpUaGlzIGlzIGEgVEVTVC4NClRoaXMgaXMgYSBURVNULg0KVGhpcyBpcyBhIFRFU1QuDQpUaGlzIGlzIGEgVEVTVC4=';
    end;

    with M4L.Issues.Files.New Do
    Begin
      Name := 'test2.txt';
      Content := 'VGhpcyBpcyBhIFRFU1QuDQpUaGlzIGlzIGEgVEVTVC4NClRoaXMgaXMgYSBURVNULg0KVGhpcyBpcyBhIFRFU1QuDQpUaGlzIGlzIGEgVEVTVC4=';
    end;

    with M4L.Issues.Files.New Do
    Begin
      Name := 'test3.txt';
      Content := 'VGhpcyBpcyBhIFRFU1QuDQpUaGlzIGlzIGEgVEVTVC4NClRoaXMgaXMgYSBURVNULg0KVGhpcyBpcyBhIFRFU1QuDQpUaGlzIGlzIGEgVEVTVC4=';
    end;

    with M4L.Issues.Files.New Do
    Begin
      Name := 'test4.txt';
      Content := 'VGhpcyBpcyBhIFRFU1QuDQpUaGlzIGlzIGEgVEVTVC4NClRoaXMgaXMgYSBURVNULg0KVGhpcyBpcyBhIFRFU1QuDQpUaGlzIGlzIGEgVEVTVC4=';
    end;

    M4L.PostIssues;

    Memo1.Text := '';
    Memo1.Lines.Add('ExecuteTime:' + IntToStr(M4L.RetHttp.ExecuteTime) + ' ms');
    Memo1.Lines.Add('ResultCode:' + IntToStr(M4L.RetHttp.ResultCode));
    Memo1.Lines.Add('ResultString:' + M4L.RetHttp.ResultString);
    Memo1.Lines.Add('LastError:' + IntToStr(M4L.RetHttp.LastError));
    Memo1.Lines.Add('LastErrorDesc:' + M4L.RetHttp.LastErrorDesc);
    Memo1.Lines.Add('Body:' + M4L.RetHttp.Body);
  Finally
    M4L.Free;
  End;
end;

End.
