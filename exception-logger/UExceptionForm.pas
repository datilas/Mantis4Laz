unit UExceptionForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CustomLineInfo, ComCtrls, ExtCtrls, Buttons, UStackTrace, UExceptionLogger;

type

  { TExceptionForm }

  TExceptionForm = class(TForm)
    ButtonDetails: TBitBtn;
    ButtonKill: TBitBtn;
    ButtonClose: TBitBtn;
    Image1: TImage;
    lblErrorHeader: TLabel;
    lblLoggerInternalError: TLabel;
    lblErrorText: TLabel;
    ListView1: TListView;
    MemoExceptionInfo: TMemo;
    PageControl1: TPageControl;
    PanelBasic: TPanel;
    PanelDescription: TPanel;
    PanelButtons: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonDetailsClick(Sender: TObject);
    procedure ButtonKillClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    Logger: TExceptionLogger;
    procedure SetBasicInfo(ADataList: TStringList);
    procedure LoadStackTraceToListView(StackTrace: TStackTrace);
    procedure SetLoggerError(const AMsg: string);
  end;


implementation

{$R *.lfm}

procedure TExceptionForm.FormShow(Sender: TObject);
begin
  Caption := SExceptionInfo;
  PageControl1.Pages[0].Caption := SGeneral;
  PageControl1.Pages[1].Caption := SCallStack;
  lblErrorHeader.Caption := SErrorOccured;
  ButtonClose.Caption := SClose;
  ButtonDetails.Caption := SDetails;
  ButtonKill.Caption := STerminate;
  //CheckBoxIgnore.Caption := SIgnoreNextTime;
  ListView1.Column[0].Caption := SIndex;
  ListView1.Column[1].Caption := SAddress;
  ListView1.Column[2].Caption := SLine;
  ListView1.Column[3].Caption := SClass;
  ListView1.Column[4].Caption := SProcedureMethod;
  ListView1.Column[5].Caption := SUnit;

  Height := PanelBasic.Height + PanelButtons.Height;
  PageControl1.ActivePageIndex := 0;
  //CheckBoxIgnore.Checked := False;
end;

procedure TExceptionForm.SetBasicInfo(ADataList: TStringList);
begin
  MemoExceptionInfo.Lines.Assign(ADataList);
end;

procedure TExceptionForm.FormCreate(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
   MemoExceptionInfo.Font.Name := 'Courier New';
   MemoExceptionInfo.Font.Size := 9;
  {$ENDIF}
end;

procedure TExceptionForm.ButtonCloseClick(Sender: TObject);
begin
  //if CheckBoxIgnore.Checked then
  //  Logger.SkipExceptionNextTime();
  Close;
end;

procedure TExceptionForm.ButtonDetailsClick(Sender: TObject);
begin
  if PanelDescription.Height <= 1 then
    Height := PanelBasic.Height + PanelButtons.Height + 200
  else
    Height := PanelBasic.Height + PanelButtons.Height;

  Application.ProcessMessages;
end;

procedure TExceptionForm.ButtonKillClick(Sender: TObject);
begin
  //Halt;
  Application.Terminate;
end;

procedure TExceptionForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TExceptionForm.LoadStackTraceToListView(StackTrace: TStackTrace);
var
  I: integer;
  NewItem: TListItem;
begin
  with ListView1, Items do
  begin
    try
      BeginUpdate;
      Clear;
      for I := 0 to StackTrace.Count - 1 do
      begin
        with TStackFrameInfo(StackTrace[I]) do
        begin
          NewItem := Add;
          with NewItem do
          begin
            Caption := IntToStr(Index);
            SubItems.Add(IntToHex(Address, 8));
            SubItems.Add(IntToStr(LineNumber));
            SubItems.Add(FunctionClassName);
            SubItems.Add(FunctionName);
            SubItems.Add(Source);
          end;
        end;
      end;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TExceptionForm.SetLoggerError(const AMsg: string);
begin
  if AMsg = EmptyStr then Exit;

  lblLoggerInternalError.Caption := AMsg;
  lblLoggerInternalError.Show;
end;

end.
