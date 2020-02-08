unit simba.ci_includebuffer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, syncobjs,
  simba.generics, simba.codeparser;

type
  TCodeInsight_CachedInclude = class(TCodeParser)
  protected
    FRefCount: Int32;
    FAge: Int32;
    FLock: TCriticalSection;
  public
    property RefCount: Int32 read FRefCount;
    property Age: Int32 read FAge;
    property Lock: TCriticalSection read FLock write FLock;

    procedure IncRef;
    procedure DecRef;

    constructor Create(AFileName: String = ''); override;
  end;

  TCachedIncludeMap = specialize TSimbaStringMap<TCodeInsight_CachedInclude>;

  TCodeInsight_IncludeCache = class
  protected
    FLock: TCriticalSection;
    FCachedIncludes: TCachedIncludeMap;

    procedure Purge;

    function FindInclude(Sender: TCodeParser; var FileName: String): Boolean;
  public
    function GetInclude(Sender: TCodeParser; FileName: String): TCodeInsight_CachedInclude;

    constructor Create;
    destructor Destroy; override;
  end;

var
  IncludeCache: TCodeInsight_IncludeCache;

implementation

uses
  simba.files, simba.settings, forms;

procedure TCodeInsight_CachedInclude.IncRef;
begin
  FLock.Enter(); Inc(FRefCount);
  FLock.Leave();
end;

procedure TCodeInsight_CachedInclude.DecRef;
begin
  FLock.Enter(); Dec(FRefCount);
  FLock.Leave();
end;

constructor TCodeInsight_CachedInclude.Create(AFileName: String);
begin
  inherited Create(AFileName);

  FAge := FileAge(AFileName);
end;

procedure TCodeInsight_IncludeCache.Purge;
var
  i: Int32;
  Include: TCodeInsight_CachedInclude;
begin
  for i := FCachedIncludes.Count - 1 downto 0 do
  begin
    Include := FCachedIncludes.ItemsI[i];

    if (Include.Age = FileAge(Include.FileName)) then
      Continue;
    if (Include.RefCount > 0) then
      Continue;

    WriteLn('Removing Include "', Include.FileName, '"');

    FCachedIncludes.Delete(i).Free();
  end;
end;

function TCodeInsight_IncludeCache.FindInclude(Sender: TCodeParser; var FileName: String): Boolean;
begin
  Result := FindFile(FileName, [ExtractFileDir(Sender.FileName), SimbaSettings.Environment.IncludePath.Value, Application.Location]);
end;

function TCodeInsight_IncludeCache.GetInclude(Sender: TCodeParser; FileName: String): TCodeInsight_CachedInclude;
var
  Include: TCodeInsight_CachedInclude;
begin
  Result := nil;

  FLock.Enter();

  try
    Purge();

    if FindInclude(Sender, FileName) then
    begin
      for Include in FCachedIncludes.ItemsOfKey(FileName) do
      begin
        if (Include.Age <> FileAge(Include.FileName)) then
          Continue;

        Exit(Include);
      end;

      WriteLn('Parsing Include "' + FileName + '"');

      Result := TCodeInsight_CachedInclude.Create(FileName);
      Result.Lock := FLock;
      Result.OnMessage := nil;
      Result.Run();

      FCachedIncludes.Add(FileName, Result);
    end;
  finally
    FLock.Leave();
  end;
end;

constructor TCodeInsight_IncludeCache.Create;
begin
  inherited Create();

  FLock := TCriticalSection.Create();
  FCachedIncludes := TCachedIncludeMap.Create(nil, dupAccept, False);
end;

destructor TCodeInsight_IncludeCache.Destroy;
begin
  FLock.Free();
  FCachedIncludes.Free();

  inherited Destroy;
end;

initialization
  IncludeCache := TCodeInsight_IncludeCache.Create();

finalization;
  IncludeCache.Free();

end.

