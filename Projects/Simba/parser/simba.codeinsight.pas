unit simba.codeinsight;

{$modeswitch nestedprocvars}

interface

uses
  sysutils, classes,
  castaliapaslex, castaliapaslextypes,
  simba.codeparser, simba.parser_misc,
  simba.ci_includecache;

type
  TCodeInsight = class(TCodeParser)
  protected
  class var
    FIncludeCache: TCodeInsight_IncludeCache;
    FBaseIncludes: TCodeInsight_IncludeArray;
  protected
    FPosition: Integer;
    FIncludeParsers: TCodeInsight_IncludeArray;

    procedure DoInclude(Sender: TObject; FileName: String; var Handled: Boolean);
    procedure DoLibrary(Sender: TObject; FileName: String; var Handled: Boolean);

    procedure SetPosition(Value: Integer);

    procedure Reset;

    function GetIncludes: TCodeInsight_IncludeArray;
    function GetGlobal(Name: String): TDeclaration;
    function GetGlobals(Name: String): TDeclarationArray;
    function GetLocal(Name: String): TDeclaration;
    function GetLocals(Name: String): TDeclarationArray;
  public
    class procedure CreateClassVariables;
    class procedure DestroyClassVariables;
    class procedure AddBaseInclude(Include: TCodeInsight_Include);

    function GetDeclarationAtPosition(APosition: Int32): TDeclaration;

    function GetMembersOfType(Declaration: TDeclaration): TDeclarationArray; overload;
    function GetMembersOfType(Declaration: TDeclaration; Name: String): TDeclarationArray; overload;

    function ParseExpression(Expressions: TExpressionArray): TDeclaration;
    function ParseExpression(Expr: String): TDeclaration;

    function FindDeclarations(Expr: String): TDeclarationArray;
    function FindMethods(Expr: String): TDeclarationArray;

    function ResolveType(Declaration: TDeclaration): TDeclaration;
    function ResolveArrayType(Declaration: TDeclaration; Dimensions: Int32): TDeclaration;

    property Position: Integer read FPosition write SetPosition;

    property Globals[Name: String]: TDeclarationArray read GetGlobals;
    property Global[Name: String]: TDeclaration read GetGlobal;

    property Locals[Name: String]: TDeclarationArray read GetLocals;
    property Local[Name: String]: TDeclaration read GetLocal;

    property Includes: TCodeInsight_IncludeArray read GetIncludes;

    constructor Create; override;
    destructor Destroy; override;
  end;

implementation

class procedure TCodeInsight.CreateClassVariables;
begin
  FIncludeCache := TCodeInsight_IncludeCache.Create();
end;

class procedure TCodeInsight.DestroyClassVariables;
var
  I: Int32;
begin
  for I := 0 to High(FBaseIncludes) do
    FBaseIncludes[I].Free();

  FIncludeCache.Free();
  FIncludeCache := nil;
end;

class procedure TCodeInsight.AddBaseInclude(Include: TCodeInsight_Include);
begin
  FBaseIncludes := FBaseIncludes + Include;
end;

function TCodeInsight.GetGlobals(Name: String): TDeclarationArray;
var
  I: Int32;
begin
  Result := nil;

  if Name = '' then
  begin
    for I := 0 to High(FIncludeParsers) do
      Result := Result + FIncludeParsers[I].Globals.ExportToArrays().Items;
    Result := Result + FGlobals.ExportToArrays().Items;
  end else
  begin
    for I := 0 to High(FIncludeParsers) do
      Result := Result + FIncludeParsers[I].Globals.ItemsOfKey(Name);
    Result := Result + FGlobals.ItemsOfKey(Name);
  end;
end;

function TCodeInsight.GetGlobal(Name: String): TDeclaration;
var
  I: Int32;
begin
  for I := 0 to High(FIncludeParsers) do
  begin
    Result := FIncludeParsers[i].Globals[Name];
    if Result <> nil then
      Exit;
  end;

  Result := FGlobals[Name];
end;

function TCodeInsight.GetLocal(Name: String): TDeclaration;
begin
  Result := FLocals[Name];
end;

function TCodeInsight.GetLocals(Name: String): TDeclarationArray;
begin
  if Name = '' then
    Result := FLocals.ExportToArrays.Items
  else
    Result := FLocals.ItemsOfKey(Name);
end;

procedure TCodeInsight.DoInclude(Sender: TObject; FileName: String; var Handled: Boolean);
var
  Include: TCodeInsight_Include;
begin
  if (FStack.Top = nil) then // Only global includes are cached
  begin
    Include := FIncludeCache.GetInclude(Self, FileName);
    if (Include <> nil) then
      FIncludeParsers := FIncludeParsers + Include;

    Handled := True;
  end;
end;

procedure TCodeInsight.DoLibrary(Sender: TObject; FileName: String; var Handled: Boolean);
var
  Include: TCodeInsight_Include;
begin
  if (OnLoadLibrary <> nil) then
  begin
    Include := FIncludeCache.GetLibrary(Self, FileName);
    if (Include <> nil) then
      FIncludeParsers := FIncludeParsers + Include;
  end;

  Handled := True;
end;

procedure TCodeInsight.SetPosition(Value: Integer);

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
      Declaration := ParseExpression(Declarations[i].RawText);
      if Declaration <> nil then
        FLocals.Add(GetMembersOfType(Declaration));
    end;
  end;

var
  Declaration: TDeclaration;
  Declarations: TDeclarationArray;
  Method: TDeclaration;
  i: Int32;
begin
  FPosition := Value;

  FLocals.Clear();

  if (FPosition > -1) then
  begin
    Declaration := GetDeclarationAtPosition(FPosition);

    if (Declaration <> nil) then
    begin
      Method := Declaration;
      if (Method is TciProcedureDeclaration) or Declaration.HasOwnerClass(TciProcedureDeclaration, Method, True) then
        GetMethodLocals(Method as TciProcedureDeclaration);

      Declarations := Declaration.GetOwnersOfClass(TciWithStatement);
      if Declaration is TciWithStatement then
        Declarations := Declarations + Declaration;

      for i := 0 to High(Declarations) do
        GetWithVariables(Declarations[i].Items.GetItemsOfClass(TciVariable));
    end;
  end;
end;

procedure TCodeInsight.Reset;
var
  i: Int32;
begin
  Lexer.Init();

  for i := Length(FBaseIncludes) to High(FIncludeParsers) do
    FIncludeCache.Release(FIncludeParsers[i]);
  FIncludeParsers := FBaseIncludes;

  FGlobals.Clear();
  FLocals.Clear();
end;

function TCodeInsight.GetIncludes: TCodeInsight_IncludeArray;
var
  i: Int32;
begin
  Result := nil;
  for i := Length(FBaseIncludes) to High(FIncludeParsers) do
    Result := Result + FIncludeParsers[i];
end;

function TCodeInsight.GetDeclarationAtPosition(APosition: Int32): TDeclaration;

  function Find(Declaration: TDeclaration): TDeclaration;
  var
    i: Int32;
  begin
    Result := nil;

    if (FPosition >= Declaration.StartPos) and (FPosition <= Declaration.EndPos) then
      Result := Declaration
    else
    begin
      for i := 0 to Declaration.Items.Count - 1 do
      begin
        Result := Find(Declaration.Items[i]);
        if (Result <> nil) then
          Exit;
      end;
    end;
  end;

var
  i: Int32;
begin
  Result := nil;

  for i := 0 to FItems.Count - 1 do
  begin
    Result := Find(FItems[i]);
    if (Result <> nil) then
      Exit;
  end;
end;

function TCodeInsight.ParseExpression(Expressions: TExpressionArray): TDeclaration;
var
  i, j: Int32;
  s: String;
  Base: TExpressionItem;
  Members: TDeclarationArray;
  Declaration: TDeclaration;
begin
  Result := nil;

  Base := Expressions.PopLeft();

  Declaration := Local[Base.Identifier];
  if Declaration = nil then
    Declaration := Global[Base.Identifier];

  if (Declaration <> nil) then
  begin
    Declaration := ResolveType(Declaration);
    if Declaration = nil then
      Exit;

    if Base.Dimensions > 0 then
    begin
      Declaration := ResolveArrayType(Declaration, Base.Dimensions);
      if Declaration = nil then
        Exit;
    end;

    for i := 0 to High(Expressions) do
    begin
      Members := GetMembersOfType(Declaration, Expressions[i].Identifier);
      if Length(Members) = 0 then
        Exit;

      Declaration := Members[0];
      Declaration := ResolveType(Declaration);
      if Declaration = nil then
        Exit;

      if Expressions[i].Dimensions > 0 then
      begin
        Declaration := ResolveArrayType(Declaration, Expressions[i].Dimensions);
        if Declaration = nil then
          Exit;
      end;
    end;

    Result := Declaration;
  end;
end;

function TCodeInsight.ParseExpression(Expr: String): TDeclaration;
begin
  Result := ParseExpression(GetExpressionArray(Expr));
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
      Declarations := Globals[Declaration.Name];
      for i := 0 to High(Declarations) do
      begin
        if Declarations[i] is TciProcedureDeclaration then
          Result := Result + Declarations[i];
      end;

      if Declaration.RecordType <> nil then
        GetFields(Declaration.RecordType);

      Declaration := Global[Declaration.GetParent] as TciTypeDeclaration; // fix
    end;
  end;

begin
  Result := nil;
  if Declaration = nil then
    Exit;

  if Declaration is TciProcedureClassName then
  begin
    Declaration := Global[Declaration.RawText];
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
        Declaration := Global[IdentifierType.RawText];
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

function TCodeInsight.FindDeclarations(Expr: String): TDeclarationArray;
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
    Declaration := ParseExpression(Expressions);
    if Declaration <> nil then
      Result := GetMembersOfType(Declaration, Name);
  end else
    Result := Locals[Name] + Globals[Name];
end;

function TCodeInsight.FindMethods(Expr: String): TDeclarationArray;

  function Resolve(Declaration: TDeclaration): TciProcedureDeclaration;
  var
    NativeMethod: TciNativeType;
    Parent: TDeclaration;
  begin
    Result := nil;

    if not (Declaration is TciProcedureDeclaration) then
    begin
      Declaration := ResolveType(Declaration);

      if Declaration is TciTypeDeclaration then
      begin
        NativeMethod := TciTypeDeclaration(Declaration).NativeMethodType;

        if NativeMethod <> nil then
        begin
          Parent := Global[NativeMethod.Parent];
          if Parent is TciTypeDeclaration then
            Declaration := Parent as TciTypeDeclaration;
        end;

        if Declaration is TciTypeDeclaration then
          Declaration := TciTypeDeclaration(Declaration).MethodType;
      end;
    end;

    if Declaration is TciProcedureDeclaration then
      Result := Declaration as TciProcedureDeclaration;
  end;

var
  Declaration: TciProcedureDeclaration;
  Declarations: TDeclarationArray;
  i: Int32;
  Invoke: Boolean;
begin
  Result := nil;

  Invoke := Expr.EndsWith('()');
  Declarations := FindDeclarations(Expr);

  for i := 0 to High(Declarations) do
  begin
    Declaration := Resolve(Declarations[i]);

    if (Declaration <> nil) then
    begin
      if Invoke then
      begin
        if (Declaration.ReturnType <> nil) then
          Declaration := Resolve(Declaration.ReturnType)
        else
          Declaration := nil;
      end;

      if (Declaration <> nil) then
        Result := Result + Declaration;
    end;
  end;
end;

function TCodeInsight.ResolveType(Declaration: TDeclaration): TDeclaration;
begin
  Result := nil;

  if Declaration is TciProcedureClassName then
  begin
    Result := Global[Declaration.RawText];
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
        Result := Global[IdentifierType.Name]
      else
        Result := GetType;
    end;
  end else
  begin
    if Declaration = nil then
      WriteLN('ResolveType: Nil')
    else
      WriteLn('ResolveType: Unexpected type "', Declaration.ClassName, '"');
  end;
end;

function TCodeInsight.ResolveArrayType(Declaration: TDeclaration; Dimensions: Int32): TDeclaration;
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
        Declaration := ResolveType(GetType());
      end;
    end else
      Declaration := nil;
  end;

  Result := Declaration;
end;

constructor TCodeInsight.Create;
begin
  inherited Create();

  OnInclude := @DoInclude;
  OnLibrary := @DoLibrary;

  Reset();
end;

destructor TCodeInsight.Destroy;
begin
  Reset();

  inherited Destroy();
end;

initialization
  TCodeInsight.CreateClassVariables();

finalization
  TCodeInsight.DestroyClassVariables();

end.
