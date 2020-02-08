unit simba.codeinsight;

{$modeswitch nestedprocvars}

interface

uses
  sysutils, classes,
  castaliapaslex, castaliapaslextypes,
  simba.codeparser, simba.generics, simba.parser_misc, simba.eventhandlerlist,
  simba.ci_includebuffer;

type
  TCodeInsight = class;
  TCodeInsightArray = array of TCodeInsight;

  TOnFindInclude = function(Sender: TObject; var FileName: string): Boolean of object;
  TOnLoadLibrary = function(Sender: TObject; var FileName: string; out ci: TCodeInsight): Boolean of object;

  TCodeInsight = class(TCodeParser)
  protected
    FPosition: Integer;

    FOnFindInclude: TOnFindInclude;
    FOnLoadLibrary: TOnLoadLibrary;

    FGlobals: TDeclarationMap;
    FLocals: TDeclarationMap;
    FCachedIncludes: TCachedIncludeMap;

    function GetGlobalsArray: TDeclarationArray;
    function GetLocalsArray: TDeclarationArray;

    procedure SetPos(Value: Integer);

    procedure Reset;
  public
    function GetMembersOfType(Declaration: TDeclaration): TDeclarationArray; overload;
    function GetMembersOfType(Declaration: TDeclaration; Name: String): TDeclarationArray; overload;

    function _ParseExpression(Expressions: TExpressionArray): TDeclaration;
    function _ParseExpression(Expr: String): TDeclaration;

    function FindDeclaration(Expr: String): TDeclarationArray;

    // GetType
    // GetArrayType
    function GetType(Declaration: TDeclaration): TDeclaration;
    function getArrayKind(Declaration: TDeclaration; Dimensions: Int32): TDeclaration;

    procedure Assign(From: TObject); override;


    procedure Run; override;

    property OnFindInclude: TOnFindInclude read FOnFindInclude write FOnFindInclude;
    property OnLoadLibrary: TOnLoadLibrary read FOnLoadLibrary write FOnLoadLibrary;
    property Position: Integer read FPosition write SetPos;


    property Globals: TDeclarationMap read FGlobals;
    property GlobalsArray: TDeclarationArray read GetGlobalsArray;
    property Locals: TDeclarationMap read FLocals;
    property LocalsArray: TDeclarationArray read GetLocalsArray;

    constructor Create(AFileName: String = ''); override;
    destructor Destroy; override;
  end;

var
  InternalParsers: array of TCodeParser;
  CoreDefines: TStringList;

implementation

uses
  simba.misc;

function TCodeInsight.GetGlobalsArray: TDeclarationArray;
begin
  Result := FGlobals.ExportToArrays.Items;
end;

function TCodeInsight.GetLocalsArray: TDeclarationArray;
begin
  Result := FLocals.ExportToArrays.Items;
end;

procedure TCodeInsight.SetPos(Value: Integer);

  procedure GetMethodLocals(Method: TciProcedureDeclaration);
  var
    Declaration: TDeclaration;
    Declarations: TDeclarationArray;
    i: Int32;
  begin
    Declarations := nil;

    while (Method <> nil) do
    begin
      if (tokStatic in Method.Directives) then
        Break;

      Declarations := Declarations + Method.Items.GetItemsOfClass(TciVarDeclaration);
      Declarations := Declarations + Method.Items.GetItemsOfClass(TciTypeDeclaration);
      Declarations := Declarations + Method.Items.GetItemsOfClass(TciProcedureDeclaration);
      Declarations := Declarations + Method.Items.GetItemsOfClass(TciReturnType);
      Declarations := Declarations + Method.GetParamDeclarations();

      Declaration := Method.Items.GetFirstItemOfClass(TciProcedureClassName);

      if Declaration <> nil then
      begin
        Declarations := Declarations + Declaration;
        Declarations := Declarations + GetMembersOfType(Declaration);
      end;

      Method.HasOwnerClass(TciProcedureDeclaration, TDeclaration(Method));
    end;

    for i := 0 to High(Declarations) do
      FLocals.Add(Declarations[i].Name, Declarations[i]);
  end;

  procedure GetWithVariables(Declarations: TDeclarationArray);
  var
    Declaration: TDeclaration;
    i: Int32;
  begin
    for i := 0 to High(Declarations) do
    begin

      Declaration := _ParseExpression(Declarations[i].RawText);
      Writeln(Declarations[i].RawText, ' :: ', Declaration.CleanText);
      if Declaration <> nil then
        FLocals.Add(GetMembersOfType(Declaration));
    end;
  end;

  function GetDeclaration(Declaration: TDeclaration): TDeclaration;
  var
    i: Int32;
  begin
    Result := nil;

    if (FPosition >= Declaration.StartPos) and (FPosition <= Declaration.EndPos) then
    begin
      Result := Declaration;
      Exit;
    end;

    for i := 0 to Declaration.Items.Count - 1 do
    begin
      Result := GetDeclaration(Declaration.Items[i]);
      if (Result <> nil) then
        Exit;
    end;
  end;

var
  Declaration: TDeclaration;
  Declarations: TDeclarationArray;
  Method: TciProcedureDeclaration;
  WithVars: TDeclarationArray;
  i: Int32;
begin
  FPosition := Value;
  FLocals.Clear();

  if (FPosition > -1) then
  begin
    Declaration := nil;

    for i := 0 to FItems.Count - 1 do
    begin
      Declaration := GetDeclaration(FItems[i]);
      if Declaration <> nil then
        Break;
    end;

    if (Declaration <> nil) then
    begin
      Declaration.HasOwnerClass(TciProcedureDeclaration, TDeclaration(Method), True);
      if (Method <> nil) then
        GetMethodLocals(Method as TciProcedureDeclaration);

      Declarations := Declaration.GetOwnersOfClass(TciWithStatement);
      if Declaration is TciWithStatement then
        Declarations := Declarations + Declaration;

      for i := 0 to High(Declarations) do
        WithVars := WithVars + Declarations[i].Items.GetItemsOfClass(TciVariable);

      GetWithVariables(WithVars);
    end;
  end;
end;

procedure TCodeInsight.Reset;
var
  i: Int32;
begin
  Lexer.Init();

  for i := 0 to FCachedIncludes.Count - 1 do
    FCachedIncludes.ItemsI[i].DecRef();

  FCachedIncludes.Clear();
  FGlobals.Clear();
  FLocals.Clear();
end;

function TCodeInsight._ParseExpression(Expressions: TExpressionArray): TDeclaration;
var
  i, j: Int32;
  s: String;
  Base: TExpressionItem;
  Members: TDeclarationArray;
  Declaration: TDeclaration;
begin
  Result := nil;

  Base := Expressions.PopLeft();

  Declaration := FLocals[Base.Identifier];
  if Declaration = nil then
    Declaration := FGlobals[Base.Identifier];

  if (Declaration <> nil) then
  begin
    Declaration := GetType(Declaration);
    if Declaration = nil then
      Exit;

    if Base.Dimensions > 0 then
    begin
      Declaration := getArrayKind(Declaration, Base.Dimensions);
      if Declaration = nil then
        Exit;
    end;

    for i := 0 to High(Expressions) do
    begin
      Members := GetMembersOfType(Declaration, Expressions[i].Identifier);
      if Length(Members) = 0 then
        Exit;

      Declaration := Members[0];
      Declaration := GetType(Declaration);
      if Declaration = nil then
        Exit;

      if Expressions[i].Dimensions > 0 then
      begin
        Declaration := getArrayKind(Declaration, Expressions[i].Dimensions);
        if Declaration = nil then
          Exit;
      end;
    end;

    Result := Declaration;
  end;
end;

function TCodeInsight._ParseExpression(Expr: String): TDeclaration;
begin
  Result := _ParseExpression(GetExpressionArray(Expr));
end;

function TCodeInsight.GetMembersOfType(Declaration: TDeclaration): TDeclarationArray;

  procedure GetFields(Declaration: TciRecordType);
  begin
    Result := Result + Declaration.Items.GetItemsOfClass(TciClassField);
  end;

  procedure GetMethods(Declaration: TciTypeDeclaration);
  var
    Declarations: TDeclarationArray;
    i: Int32;
  begin
    while Declaration <> nil do
    begin
      Declarations := FGlobals.ItemsOfKey(Declaration.Name);
      for i := 0 to High(Declarations) do
        if Declarations[i] is TciProcedureDeclaration then
          Result := Result + Declarations[i];

      if Declaration.RecordType <> nil then
        GetFields(Declaration.RecordType);

      Declaration := FGlobals[Declaration.GetParent] as TciTypeDeclaration;
    end;
  end;

begin
  Result := nil;
  if Declaration = nil then
    Exit;

  if Declaration is TciProcedureClassName then
  begin
    Declaration := FGlobals[Declaration.RawText];
    if Declaration is TciTypeDeclaration then
      GetMethods(Declaration as TciTypeDeclaration);
  end else
  if Declaration is TciTypeKind then
  begin
    with Declaration as TciTypeKind do
    begin
      if RecordType <> nil then
        GetFields(RecordType);

      if IdentifierType <> nil then
      begin
        Declaration := FGlobals[IdentifierType.RawText];
        if Declaration is TciTypeDeclaration then
          GetMethods(Declaration as TciTypeDeclaration);
      end;
    end;
  end else
  if Declaration is TciTypeDeclaration then
    GetMethods(Declaration as TciTypeDeclaration)
  else
  if Declaration is TciRecordType then
    GetFields(Declaration as TciRecordType)
  else
    WriteLn('GetMembersOfType: Unexpected type "', Declaration.ClassName, '"');
end;

function TCodeInsight.GetMembersOfType(Declaration: TDeclaration; Name: String): TDeclarationArray;
var
  Declarations: TDeclarationArray;
  i: Int32;
begin
  Result := nil;

  Declarations := GetMembersOfType(Declaration);
  for i := 0 to High(Declarations) do
    if Declarations[i].IsName(Name) then
      Result := Result + Declarations[i];
end;

function TCodeInsight.FindDeclaration(Expr: String): TDeclarationArray;
var
  Expressions: TExpressionArray;
  Name: String;
  Declaration: TDeclaration;
begin
  Result := nil;

  Expressions := GetExpressionArray(Expr);
  Name := Expressions.Pop.Identifier;

  if Length(Expressions) > 0 then
  begin
    Declaration := _ParseExpression(Expressions);
    if Declaration <> nil then
      Result := GetMembersOfType(Declaration, Name);
  end else
    Result := FLocals.ItemsOfKey(Name) + FGlobals.ItemsOfKey(Name);
end;

function TCodeInsight.GetType(Declaration: TDeclaration): TDeclaration;
begin
  Result := nil;

  if Declaration is TciProcedureClassName then
  begin
    Result := FGlobals[Declaration.RawText];
    Exit;
  end;
  if Declaration is TciTypeDeclaration then
    Exit(Declaration);

  if Declaration is TciProcedureDeclaration then
    Declaration := TciProcedureDeclaration(Declaration).ReturnType
  else
  if Declaration is TciVarDeclaration then
    Declaration := TciVarDeclaration(Declaration).VarType;

  if Declaration is TciTypeKind then
  begin
    with Declaration as TciTypeKind do
    begin
      if IdentifierType <> nil then
        Result := FGlobals[IdentifierType.Name]
      else
        Result := GetType;
    end;
  end else
  begin
    if Declaration = nil then
      WriteLN('GetType: Nil')
    else
      WriteLn('GetType: Unexpected type "', Declaration.ClassName, '"');
  end;
end;

function TCodeInsight.getArrayKind(Declaration: TDeclaration; Dimensions: Int32): TDeclaration;
begin
  Result := nil;

  while (Declaration <> nil) and (Dimensions > 0) do
  begin
    if Declaration is TciTypeDeclaration then
      Declaration := TciTypeDeclaration(Declaration).ArrayType;

    if (Declaration is TciArrayType) then
    begin
      with Declaration as TciArrayType do
      begin
        Dec(Dimensions, GetDimensionCount());
        Declaration := Self.GetType(GetType());
      end;
    end else
      Declaration := nil;
  end;

  Result := Declaration;
end;


constructor TCodeInsight.Create(AFileName: String);
begin
  inherited Create(AFileName);

  FOnFindInclude := nil;
  FOnLoadLibrary := nil;

  FGlobals := TDeclarationMap.Create(nil, dupAccept, False);
  FLocals := TDeclarationMap.Create(nil, dupAccept, False);
  FCachedIncludes := TCachedIncludeMap.Create(nil, dupIgnore, False);


  FPosition := -1;

  Reset();

  Lexer.Defines.AddStrings(CoreDefines);
end;

destructor TCodeInsight.Destroy;
begin
  Reset();

  FGlobals.Free();
  FLocals.Free();
  FCachedIncludes.Free();

  inherited;
end;

procedure TCodeInsight.Assign(From: TObject);
begin
  if (From is TCodeInsight) then
    with From as TCodeInsight do
    begin
      Self.OnFindInclude := OnFindInclude;
      Self.OnLoadLibrary := OnLoadLibrary;
    end;

  inherited;
end;


procedure TCodeInsight.Run;

  procedure Build(Parser: TCodeParser);

    procedure AddInclude(Declaration: TciInclude);
    var
      Include: TCodeInsight_CachedInclude;
    begin
      Include := IncludeCache.GetInclude(Parser, Declaration.RawText);

      if FCachedIncludes.Add(Include.FileName, Include) > -1 then // Dont add the same include multiple times
      begin
        Include.IncRef();

        Build(Include);
      end;
    end;

    procedure AddMethod(Method: TciProcedureDeclaration);
    begin
      if Method.IsMethodOfType then
        FGlobals.Add(MEthod.ObjectName, Method)
      else
        FGlobals.Add(Method.Name, Method);
    end;

    procedure AddType(TypeDeclaration: TciTypeDeclaration);
    var
      Declaration: TDeclaration;
    begin
      if TypeDeclaration.EnumType <> nil then
        for Declaration in TypeDeclaration.EnumType.Elements do
          FGlobals.Add(Declaration.Name, Declaration);

      FGlobals.Add(TypeDeclaration.Name, TypeDeclaration);
    end;

  var
    i: Int32;
    Declaration: TDeclaration;
  begin
    for i := 0 to Parser.Items.Count - 1 do
    begin
      Declaration := Parser.Items[i];

      if Declaration.ClassType = TciInclude then
        AddInclude(Declaration as TciInclude)
      else
      if Declaration.ClassType = TciProcedureDeclaration then
        AddMethod(Declaration as TciProcedureDeclaration)
      else
      if Declaration.ClassType = TciTypeDeclaration then
        AddType(Declaration as TciTypeDeclaration)
      else
      if Declaration.Name <> '' then
        FGlobals.Add(Declaration.Name, Declaration);
    end;
  end;

var
  Parser: TCodeParser;
begin
  inherited Run();

  for Parser in InternalParsers do
    Build(Parser);

  Build(Self);
end;

initialization
  CoreDefines := TStringList.Create();

finalization
  CoreDefines.Free();

end.
