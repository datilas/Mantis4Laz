Unit untIssues;

{$IF DEFINED(FPC)}
  {$MODE DELPHI}{$H+}
{$ENDIF}

Interface

Uses
  Classes, SysUtils, Contnrs, JsonsD;

Type

  { TProject }

  TProject = Class(TObject)
  private
    FId: Integer;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;

    property Id: Integer Read FId Write FId;
    property Name: String Read FName Write FName;
  End;

  { TCategory }

  TCategory = Class(TObject)
  private
    FId: Integer;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;
    //*
    property Id: Integer Read FId Write FId;
    property Name: String Read FName Write FName;
  End;

  { THandler }

  THandler = Class(TObject)
  private
    FId: Integer;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;
    //*
    property Id: Integer Read FId Write FId;
    property Name: String Read FName Write FName;
  End;

  { TView_state }

  TView_state = Class(TObject)
  private
    FId: Integer;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;
    //*
    property Id: Integer Read FId Write FId;
    property Name: String Read FName Write FName;
  End;

  { TPriority }

  TPriority = Class(TObject)
  private
    FId: Integer;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;
    //*
    property Id: Integer Read FId Write FId;
    property Name: String Read FName Write FName;
  End;

  { TSeverity }

  TSeverity = Class(TObject)
  private
    FId: Integer;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;
    //*
    property Id: Integer Read FId Write FId;
    property Name: String Read FName Write FName;
  End;

  { TReproducibility }

  TReproducibility = Class(TObject)
  private
    FId: Integer;
    FName: String;
  public
    constructor Create;
    destructor Destroy; override;
    //*
    property Id: Integer Read FId Write FId;
    property Name: String Read FName Write FName;
  End;

  { TTag }

  TTag = Class
  private
    FId: Integer;
    FName: String;
  public
    constructor Create;
    //*
    property Id: Integer Read FId Write FId;
    property Name: String Read FName Write FName;
  End;

  { TTags }

  TTags = class(TObjectList)
  protected
    procedure SetObject(Index: Integer; Item: TTag);
    function GetObject(Index: Integer): TTag;
    procedure Insert(Index: Integer; Obj: TTag);
  public
    function Add(Obj: TTag): Integer;
    function New: TTag;
    property Objects[Index: Integer]: TTag Read GetObject Write SetObject; default;
  end;


  { TCustom_field }

  TCustom_field = Class
  private
    FId: Integer;
    FName: String;
    FValue: String;
  public
    constructor Create;
    //*
    property Id: Integer Read FId Write FId;
    property Name: String Read FName Write FName;
    property Value: String Read FValue Write FValue;
  End;


  { TCustom_fields }

  TCustom_fields = class(TObjectList)
  protected
    procedure SetObject(Index: Integer; Item: TCustom_field);
    function GetObject(Index: Integer): TCustom_field;
    procedure Insert(Index: Integer; Obj: TCustom_field);
  public
    function Add(Obj: TCustom_field): Integer;
    function New: TCustom_field;
    property Objects[Index: Integer]: TCustom_field Read GetObject Write SetObject; default;
  end;


  { TFile }

  TFile = Class
  private
    FContent: String;
    FName: String;
  public
    constructor Create;
    //*
    property Name: String Read FName Write FName;
    property Content: String Read FContent Write FContent;
  End;

  { TFiles }

  TFiles = class(TObjectList)
  protected
    procedure SetObject(Index: Integer; Item: TFile);
    function GetObject(Index: Integer): TFile;
    procedure Insert(Index: Integer; Obj: TFile);
  public
    function Add(Obj: TFile): Integer;
    function New: TFile;
    property Objects[Index: Integer]: TFile Read GetObject Write SetObject; default;
  end;


  { TIssues }
  TIssues = Class(TObject)
  private
    FCategory: TCategory;
    FCustom_fields: TCustom_fields;
    FDescription: String;
    FFiles: TFiles;
    FHandler: THandler;
    FPriority: TPriority;
    FProject: TProject;
    FReproducibility: TReproducibility;
    FSeverity: TSeverity;
    FSticky: Boolean;
    FSummary: String;
    FTags: TTags;
    FView_state: TView_state;
    Function GetJson_Issues: String;
  public
    constructor Create;
    destructor Destroy; override;
    //*
    property Summary: String Read FSummary Write FSummary;
    property Description: String Read FDescription Write FDescription;
    property Project: TProject Read FProject Write FProject;
    property Category: TCategory Read FCategory Write FCategory;
    property Handler: THandler Read FHandler Write FHandler;
    property View_state: TView_state Read FView_state Write FView_state;
    property Priority: TPriority Read FPriority Write FPriority;
    property Severity: TSeverity Read FSeverity Write FSeverity;
    property Reproducibility: TReproducibility Read FReproducibility Write FReproducibility;
    property Sticky: Boolean Read FSticky Write FSticky;
    property Tags: TTags Read FTags Write FTags;
    property Custom_fields: TCustom_fields Read FCustom_fields Write FCustom_fields;
    property Files: TFiles Read FFiles Write FFiles;
    //*
    property Json_Issues: String Read GetJson_Issues;
  End;

Implementation

{ TTags }

Procedure TTags.SetObject(Index: Integer; Item: TTag);
Begin
  inherited Items[Index] := Item;
End;

Function TTags.GetObject(Index: Integer): TTag;
Begin
  Result := TTag(inherited Items[Index]);
End;

Procedure TTags.Insert(Index: Integer; Obj: TTag);
Begin
  inherited Insert(Index, Obj);
End;

Function TTags.Add(Obj: TTag): Integer;
Begin
  Result := inherited Add(Obj);
End;

Function TTags.New: TTag;
Begin
  Result := TTag.Create;
  Self.Add(Result);
End;

{ TTag }

Constructor TTag.Create;
Begin
  inherited;

  FId := 0;
  FName := '';
End;

{ TFiles }

Procedure TFiles.SetObject(Index: Integer; Item: TFile);
Begin
  inherited Items[Index] := Item;
End;

Function TFiles.GetObject(Index: Integer): TFile;
Begin
  Result := TFile(inherited Items[Index]);
End;

Procedure TFiles.Insert(Index: Integer; Obj: TFile);
Begin
  inherited Insert(Index, Obj);
End;

Function TFiles.Add(Obj: TFile): Integer;
Begin
  Result := inherited Add(Obj);
End;

Function TFiles.New: TFile;
Begin
  Result := TFile.Create;
  Self.Add(Result);
End;

{ TFile }

Constructor TFile.Create;
Begin
  inherited;
  FContent := '';
  FName := '';
End;

{ TCustom_fields }

Procedure TCustom_fields.SetObject(Index: Integer; Item: TCustom_field);
Begin
  inherited Items[Index] := Item;
End;

Function TCustom_fields.GetObject(Index: Integer): TCustom_field;
Begin
  Result := TCustom_field(inherited Items[Index]);
End;

Procedure TCustom_fields.Insert(Index: Integer; Obj: TCustom_field);
Begin
  inherited Insert(Index, Obj);
End;

Function TCustom_fields.Add(Obj: TCustom_field): Integer;
Begin
  Result := inherited Add(Obj);
End;

Function TCustom_fields.New: TCustom_field;
Begin
  Result := TCustom_field.Create;
  Self.Add(Result);
End;

{ TCustom_field }

Constructor TCustom_field.Create;
Begin
  inherited;

  FId := 0;
  FName := '';
  FValue := '';
End;

{ TSeverity }

Constructor TSeverity.Create;
Begin
  FId := 0;
  FName := '';
End;

Destructor TSeverity.Destroy;
Begin
  Inherited Destroy;
End;

{ TPriority }

Constructor TPriority.Create;
Begin
  FId := 0;
  FName := '';
End;

Destructor TPriority.Destroy;
Begin
  Inherited Destroy;
End;

{ TView_state }

Constructor TView_state.Create;
Begin
  FId := 0;
  FName := '';
End;

Destructor TView_state.Destroy;
Begin
  Inherited Destroy;
End;

{ THandler }

Constructor THandler.Create;
Begin
  FId := 0;
  FName := '';
End;

Destructor THandler.Destroy;
Begin
  Inherited Destroy;
End;

{ TCategory }

Constructor TCategory.Create;
Begin
  FId := 0;
  FName := '';
End;

Destructor TCategory.Destroy;
Begin
  Inherited Destroy;
End;

{ TProject }

Constructor TProject.Create;
Begin
  FId := 0;
  FName := '';
End;

Destructor TProject.Destroy;
Begin
  Inherited Destroy;
End;

{ TReproducibility }

Constructor TReproducibility.Create;
Begin
  FId := 0;
  FName := '';
End;

Destructor TReproducibility.Destroy;
Begin
  Inherited Destroy;
End;

{ TIssues }

Function TIssues.GetJson_Issues: String;
Var
  js: TJson;
  i: Integer;
Begin
  Result := '';
  js := TJson.Create;
  Try
    js.Values['summary'].AsString := Summary;
    js.Values['description'].AsString := Description;

    if Project.Id > 0 Then
      js.Values['project'].AsObject.Values['id'].AsInteger := Project.Id;
    js.Values['project'].AsObject.Values['name'].AsString := Project.Name;

    if Category.Id > 0 Then
      js.Values['category'].AsObject.Values['id'].AsInteger := Category.Id;
    js.Values['category'].AsObject.Values['name'].AsString := Category.Name;

    If Handler.Id > 0 Then
      js.Values['handler'].AsObject.Values['id'].AsInteger := Handler.Id;

    If Handler.Name <> '' Then
      js.Values['handler'].AsObject.Values['name'].AsString := Handler.Name;

    If View_state.Id > 0 Then
      js.Values['view_state'].AsObject.Values['id'].AsInteger := View_state.Id;

    If View_state.Name <> '' Then
      js.Values['view_state'].AsObject.Values['name'].AsString := View_state.Name;

    If Priority.Id > 0 Then
      js.Values['priority'].AsObject.Values['id'].AsInteger := Priority.Id;

    If Priority.Name <> '' Then
      js.Values['priority'].AsObject.Values['name'].AsString := Priority.Name;

    If Severity.Id > 0 Then
      js.Values['severity'].AsObject.Values['id'].AsInteger := Severity.Id;

    If Severity.Name <> '' Then
      js.Values['severity'].AsObject.Values['name'].AsString := Severity.Name;

    If Reproducibility.Id > 0 Then
      js.Values['reproducibility'].AsObject.Values['id'].AsInteger := Reproducibility.Id;

    If Reproducibility.Name <> '' Then
      js.Values['reproducibility'].AsObject.Values['name'].AsString := Reproducibility.Name;

    js.Values['sticky'].AsBoolean := Sticky;

    for i := 0 to Pred(Tags.Count) Do
    Begin
      With js.Values['tags'].AsArray.Add Do
      Begin
        If Tags[i].Id > 0 Then
          AsObject.Values['id'].AsInteger := Tags[i].Id;
        AsObject.Values['name'].AsString := Tags[i].Name;
      End;
    End;

    for i := 0 to Pred(Custom_fields.Count) Do
    Begin
      With js.Values['custom_fields'].AsArray.Add Do
      Begin
        If Custom_fields[i].Id > 0 Then
          AsObject.Values['field'].AsObject.Values['id'].AsInteger := Custom_fields[i].Id;
        AsObject.Values['field'].AsObject.Values['name'].AsString := Custom_fields[i].Name;
        AsObject.Values['value'].AsString := Custom_fields[i].Value;
      End;
    End;

    for i := 0 to Pred(Files.Count) Do
    Begin
      With js.Values['files'].AsArray.Add Do
      Begin
        AsObject.Values['name'].AsString := Files[i].Name;
        AsObject.Values['content'].AsString := Files[i].Content;
      End;
    End;
  Finally
    Begin
      Result := js.Stringify;
      js.Free;
    End;
  End;
end;

Constructor TIssues.Create;
Begin
  FSummary := '';
  FDescription := '';
  FProject := TProject.Create;
  FCategory := TCategory.Create;
  FHandler := THandler.Create;
  FView_state := TView_state.Create;
  FPriority := TPriority.Create;
  FSeverity := TSeverity.Create;
  FReproducibility := TReproducibility.Create;
  FSticky := False;
  FTags := TTags.Create;
  FCustom_fields := TCustom_fields.Create;
  FFiles := TFiles.Create;
End;

Destructor TIssues.Destroy;
Begin
  FreeAndNil(FProject);
  FreeAndNil(FCategory);
  FreeAndNil(FHandler);
  FreeAndNil(FView_state);
  FreeAndNil(FPriority);
  FreeAndNil(FSeverity);
  FreeAndNil(FReproducibility);
  FreeAndNil(FCustom_fields);
  FreeAndNil(FTags);
  FreeAndNil(FFiles);
  Inherited Destroy;
End;

End.
