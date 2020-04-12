unit simba.scriptinstance;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, process,
  simba.ipc, simba.script_common;

type
  TSimbaScriptInstance = class;

  TSimbaScriptProcess = class(TProcess)
  public
    OnExecute: TNotifyEvent;
    OnDestroy: TNotifyEvent;

    procedure Execute; override;

    constructor Create; reintroduce;
    destructor Destroy; override;
  end;

  TSimbaScriptOutputThread = class(TThread)
  protected
    FScript: TSimbaScriptInstance;
    FServer: TSimbaIPC_Server;

    procedure Execute; override;
  public
    constructor Create(Script: TSimbaScriptInstance); reintroduce;
  end;

  TSimbaScriptStateThread = class(TThread)
  protected
    FScript: TSimbaScriptInstance;
    FServer: TSimbaIPC_Server;
    FState: ESimbaScriptState;

    procedure Execute; override;
  public
    property State: ESimbaScriptState read FState;

    constructor Create(Script: TSimbaScriptInstance; Server: TSimbaIPC_Server); reintroduce;
  end;

  TSimbaScriptMethodThread = class(TThread)
  protected
    FServer: TSimbaIPC_Server;
    FScript: TSimbaScriptInstance;

    procedure Execute; override;
  public
    constructor Create(Script: TSimbaScriptInstance; Server: TSimbaIPC_Server); reintroduce;
  end;

  TSimbaScriptInstance = class
  protected
    FProcess: TSimbaScriptProcess;

    FStateServer: TSimbaIPC_Server;
    FMethodServer: TSimbaIPC_Server;

    FStateThread: TSimbaScriptStateThread;
    FOutputThread: TSimbaScriptOutputThread;
    FMethodThread: TSimbaScriptMethodThread;

    FManageOutput: Boolean;
    FOutput: TStringList;
    FTarget: THandle;

    FStartTime: UInt64;
    FScript: String;
    FScriptFile: String;
    FScriptName: String;

    procedure OnExecuteProcess(Sender: TObject);
    procedure OnDestroyProcess(Sender: TObject);

    function GetTimeRunning: UInt64;
    function GetIsPaused: Boolean;
    function GetIsStopping: Boolean;
    function GetIsRunning: Boolean;
    function GetExitCode: Int32;

    procedure SetScript(Value: String);
  public
    property Process: TSimbaScriptProcess read FProcess;

    // Parameters to pass to script
    property Script: String write FScript;
    property ScriptName: String write FScriptName;
    property ScriptFile: String write FScriptFile;
    property Target: THandle write FTarget;

    // Stats
    property TimeRunning: UInt64 read GetTimeRunning;
    property ExitCode: Int32 read GetExitCode;

    // Get state
    property IsPaused: Boolean read GetIsPaused;
    property IsStopping: Boolean read GetIsStopping;
    property IsRunning: Boolean read GetIsRunning;

    // Start
    procedure Run;
    procedure Compile;

    // Change the state
    procedure Resume;
    procedure Pause;
    procedure Stop;
    procedure Kill;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  forms, dialogs,
  simba.script_simbamethod, simba.debugform, simba.settings;

procedure TSimbaScriptMethodThread.Execute;
var
  Method: TSimbaMethod;
  Message: Int32;
begin
  Method.Script := FScript;
  Method.Params := TMemoryStream.Create();
  Method.Result := TMemoryStream.Create();

  try
    while True do
    begin
      Method.Params.Clear();
      Method.Result.Clear();

      if not FServer.ReadMessage(Message, Method.Params) then
        Break;

      case ESimbaMethod(Message) of
        SIMBA_METHOD_DEBUG_IMAGE:         TThread.Synchronize(nil, @Method._DebugImage);
        SIMBA_METHOD_DEBUG_IMAGE_DRAW:    TThread.Synchronize(nil, @Method._DebugImageDraw);
        SIMBA_METHOD_SCRIPT_ERROR:        TThread.Synchronize(nil, @Method._ScriptError);
        SIMBA_METHOD_CLEAR_DEBUG:         TThread.Synchronize(nil, @Method._ClearDebug);
        SIMBA_METHOD_DEBUG_IMAGE_CLEAR:   TThread.Synchronize(nil, @Method._DebugImageClear);
        SIMBA_METHOD_DEBUG_IMAGE_DISPLAY: TThread.Synchronize(nil, @Method._DebugImageDisplay);
        SIMBA_METHOD_GET_PID:             TThread.Synchronize(nil, @Method._GetPID);
        SIMBA_METHOD_DEBUG_IMAGE_GET:     TThread.Synchronize(nil, @Method._DebugImageGetImage);
        SIMBA_METHOD_BALLOON_HINT:        TThread.Synchronize(nil, @Method._ShowBalloonHint);
        SIMBA_METHOD_DISGUISE:            TThread.Synchronize(nil, @Method._Disguise);
        SIMBA_METHOD_STATUS:              TThread.Synchronize(nil, @Method._Status);
        SIMBA_METHOD_GET_TARGET_PID:      TThread.Synchronize(nil, @Method._GetSimbaTargetPID);
        SIMBA_METHOD_GET_TARGET_WINDOW:   TThread.Synchronize(nil, @Method._GetSimbaTargetWindow)
        else
          raise Exception.CreateFmt('Invalid method %d', [Method.Method]);
      end;

      FServer.WriteMessage(Message, Method.Result);
    end;
  except
    on E: Exception do
      WriteLn('Method Server Exception: ' + E.Message);
  end;

  Method.Params.Free();
  Method.Result.Free();

  FServer.Free();
end;

constructor TSimbaScriptMethodThread.Create(Script: TSimbaScriptInstance; Server: TSimbaIPC_Server);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FServer := Server;
  FScript := Script;
end;

procedure TSimbaScriptStateThread.Execute;
var
  Count: Int32;
begin
  try
    while True do
    begin
      Count := FServer.Read(FState, SizeOf(Int32));
      if Count = PIPE_TERMINATED then
        Break;
    end;
  except
    on E: Exception do
      WriteLn('State Server Exception: ' + E.Message);
  end;

  FServer.Free();
end;

constructor TSimbaScriptStateThread.Create(Script: TSimbaScriptInstance; Server: TSimbaIPC_Server);
begin
  inherited Create(False, 512*512);

  FreeOnTerminate := True;

  FScript := Script;
  FServer := Server;
  FState := SCRIPT_RUNNING;
end;

procedure TSimbaScriptOutputThread.Execute;
var
  Output: TStringList;

  function Process(constref Data: String): String;
  var
    I: Int32;
    Lines: TStringArray;
  begin
    Result := '';

    Lines := Data.Split([LineEnding]);
    for I := 0 to High(Lines) - 1 do
      Output.Add(Lines[I]);

    // Pipe buffer is full, this is not a complete line.
    if not Data.EndsWith(LineEnding) then
      Exit(Lines[High(Lines)]);

    Output.Add(Lines[High(Lines)]);
  end;

var
  Buffer: array[1..16 * 1024] of Char;
  Remaining: String;
  Count: Int32;
begin
  Output := TStringList.Create();

  Remaining := '';

  try
    while True do
    begin
      Output.Clear();

      Count := FScript.Process.Output.Read(Buffer[1], Length(Buffer));
      if (Count = 0) then
        Break;

      Remaining := Process(Remaining + Copy(Buffer, 1, Count));

      if (Output.Count > 0) then
        SimbaDebugForm.Add(Output);
    end;
  except
    on E: Exception do
      WriteLn('Output Thread Exception: ' + E.Message);
  end;

  if (Length(Remaining) > 0) then
    SimbaDebugForm.Add(Remaining);

  Output.Free();
end;

constructor TSimbaScriptOutputThread.Create(Script: TSimbaScriptInstance);
begin
  inherited Create(False, 512*512);

  FScript := Script;
end;

procedure TSimbaScriptProcess.Execute;
begin
  inherited Execute();

  if (OnExecute <> nil) then
    OnExecute(Self);
end;

constructor TSimbaScriptProcess.Create;
begin
  inherited Create(Application);
end;

destructor TSimbaScriptProcess.Destroy;
begin
  if Running then
    Terminate(0);

  if (OnDestroy <> nil) then
    OnDestroy(Self);

  inherited Destroy();
end;

function TSimbaScriptInstance.GetIsRunning: Boolean;
begin
  Result := FProcess.Running;
end;

function TSimbaScriptInstance.GetIsPaused: Boolean;
begin
  Result := FProcess.Running and (FStateThread.State = SCRIPT_PAUSED);
end;

function TSimbaScriptInstance.GetIsStopping: Boolean;
begin
  Result := FProcess.Running and (FStateThread.State = SCRIPT_STOPPING);
end;

procedure TSimbaScriptInstance.SetScript(Value: String);
var
  FileName: String;
begin
  FileName := GetTempFileName(SimbaSettings.Environment.DataPath.Value, '.script');

  with TStringList.Create() do
  try
    Text := Value;

    SaveToFile(FileName);
  finally
    Free();
  end;

  FProcess.Parameters.Add('--scriptfile=' + FileName);
end;

procedure TSimbaScriptInstance.OnExecuteProcess(Sender: TObject);
begin
  FOutputThread := TSimbaScriptOutputThread.Create(Self);
end;

procedure TSimbaScriptInstance.OnDestroyProcess(Sender: TObject);
begin
  FOutputThread.WaitFor();
  FOutputThread.Free();
end;

function TSimbaScriptInstance.GetTimeRunning: UInt64;
begin
  Result := GetTickCount64() - FStartTime;
end;

function TSimbaScriptInstance.GetExitCode: Int32;
begin
  Result := FProcess.ExitCode;
end;

procedure TSimbaScriptInstance.Run;
begin
  if (FScript <> '') then
  begin
    FScriptFile := GetTempFileName(SimbaSettings.Environment.DataPath.Value, '.script');

    with TStringList.Create() do
    try
      Text := FScript;

      SaveToFile(FScriptFile);
    finally
      Free();
    end;

    FProcess.Parameters.Add('--scriptname=' + FScriptName);
  end;

  FProcess.Parameters.Add('--target=' + IntToStr(FTarget));
  FProcess.Parameters.Add('--run');
  FProcess.Parameters.Add(FScriptFile);
  FProcess.Execute();

  FStartTime := GetTickCount64();
end;

procedure TSimbaScriptInstance.Compile;
begin
  if (FScript <> '') then
  begin
    FScriptFile := GetTempFileName(SimbaSettings.Environment.DataPath.Value, '.script');

    with TStringList.Create() do
    try
      Text := FScript;

      SaveToFile(FScriptFile);
    finally
      Free();
    end;

    FProcess.Parameters.Add('--scriptname=' + FScriptName);
  end;

  FProcess.Parameters.Add('--target=' + IntToStr(FTarget));
  FProcess.Parameters.Add('--compile');
  FProcess.Parameters.Add(FScriptFile);
  FProcess.Execute();

  FStartTime := GetTickCount64();
end;

procedure TSimbaScriptInstance.Resume;
var
  Message: Int32 = Ord(SCRIPT_RUNNING);
begin
  FStateServer.Write(Message, SizeOf(Int32));
end;

procedure TSimbaScriptInstance.Pause;
var
  Message: Int32 = Ord(SCRIPT_PAUSED);
begin
  FStateServer.Write(Message, SizeOf(Int32));
end;

procedure TSimbaScriptInstance.Stop;
var
  Message: Int32 = Ord(SCRIPT_STOPPING);
begin
  FStateServer.Write(Message, SizeOf(Int32));
end;

constructor TSimbaScriptInstance.Create;
begin
  FMethodServer := TSimbaIPC_Server.Create();
  FStateServer := TSimbaIPC_Server.Create();

  FOutput := TStringList.Create();

  FProcess := TSimbaScriptProcess.Create();
  FProcess.PipeBufferSize := 16 * 1024;
  FProcess.OnExecute := @Self.OnExecuteProcess;
  FProcess.OnDestroy := @Self.OnDestroyProcess;
  FProcess.InheritHandles := True;
  FProcess.CurrentDirectory := Application.Location;
  FProcess.Options := FProcess.Options + [poUsePipes];

  FProcess.Parameters.Add('--simba-method-server=%s', [FMethodServer.Client]);
  FProcess.Parameters.Add('--simba-state-server=%s', [FStateServer.Client]);

  FProcess.Parameters.Add('--apppath=%s', [Application.Location]);
  FProcess.Parameters.Add('--datapath=%s', [SimbaSettings.Environment.DataPath.Value]);
  FProcess.Parameters.Add('--includepath=%s', [SimbaSettings.Environment.IncludePath.Value]);
  FProcess.Parameters.Add('--pluginpath=%s', [SimbaSettings.Environment.PluginPath.Value]);
  FProcess.Parameters.Add('--fontpath=%s', [SimbaSettings.Environment.FontPath.Value]);

  FProcess.Executable := SimbaSettings.Environment.ScriptExecutablePath.Value;
  if (not FileExists(FProcess.Executable)) then
    raise Exception.Create('SimbaScript executable not found: ' + FProcess.Executable);

  FMethodThread := TSimbaScriptMethodThread.Create(Self, FMethodServer);
  FStateThread := TSimbaScriptStateThread.Create(Self, FStateServer);
end;

procedure TSimbaScriptInstance.Kill;
begin
  if (FProcess <> nil) then
    FProcess.Terminate(0);
end;

destructor TSimbaScriptInstance.Destroy;
begin
  if (FProcess <> nil) then
  begin
    if FProcess.Running then
      FProcess.Terminate(0);

    FProcess.Free();
  end;

  if (FMethodServer <> nil) then FMethodServer.Terminate();
  if (FStateServer <> nil)  then FStateServer.Terminate();

  inherited Destroy();
end;

end.

