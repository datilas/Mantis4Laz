Program demo;

{$mode objfpc}{$H+}

Uses
 {$IFDEF UNIX}
  cthreads,
 {$ENDIF}
 {$IFDEF HASAMIGA}
  athreads,
 {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Unit1,
  untIssues,
  UExceptionLogger, untMantis4Laz;

{$R *.res}

Begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  exceptionLogger := TExceptionLogger.Create(Application);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
End.
