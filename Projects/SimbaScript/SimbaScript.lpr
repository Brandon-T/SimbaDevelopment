program SimbaScript;

{$mode objfpc}{$H+}

{$IFDEF DARWIN}
  {$modeswitch objectivec2}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads, cmem,
  {$ENDIF}
  {$IFDEF DARWIN}
  cocoaint,
  {$ENDIF}
  sysutils, classes, interfaces, forms,
  simbascript.script, simba.script_common, simba.ipc;

{$R *.res}

type
  TApplicationHelper = class helper for TApplication
  public
    procedure Execute(Data: PtrInt);
    procedure Terminate(Sender: TObject);
  end;

procedure TApplicationHelper.Execute(Data: PtrInt);
begin
  Script := TSimbaScript.Create(True);
  Script.OnTerminate := @Self.Terminate;
  Script.CompileOnly := Application.HasOption('compile');
  Script.Dump := Application.HasOption('dump');

  if not Script.Dump then
  begin
    Script.ScriptFile := Application.Params[Application.ParamCount];

    with TStringList.Create() do
    try
      LoadFromFile(Script.ScriptFile);

      Script.Script := Text;
    finally
      Free();
    end;
  end;

  if Application.HasOption('scriptname') then
  begin
    if ExtractFileExt(Script.ScriptFile) = '.tmp' then
      DeleteFile(Script.ScriptFile);

    Script.ScriptFile := Application.GetOptionValue('scriptname');
  end;

  Script.AppPath := Application.GetOptionValue('apppath');
  if Script.AppPath = '' then
    Script.AppPath := IncludeTrailingPathDelimiter(Application.Location);

  Script.DataPath := Application.GetOptionValue('datapath');
  if Script.DataPath = '' then
    Script.DataPath := IncludeTrailingPathDelimiter(Application.Location) + 'Data';

  Script.PluginPath := Application.GetOptionValue('pluginpath');
  if Script.PluginPath = '' then
    Script.PluginPath := IncludeTrailingPathDelimiter(Application.Location) + 'Plugins';

  Script.FontPath := Application.GetOptionValue('fontpath');
  if Script.FontPath = '' then
    Script.FontPath := IncludeTrailingPathDelimiter(Application.Location) + 'Fonts';

  Script.IncludePath := Application.GetOptionValue('includepath');
  if Script.IncludePath = '' then
    Script.IncludePath := IncludeTrailingPathDelimiter(Application.Location) + 'Includes';

  if Application.HasOption('targetwindow') then
    Script.TargetWindow := Application.GetOptionValue('targetwindow').ToInt64();

  if Application.HasOption('output-client') then
    Script.OutputServer := TSimbaIPC_Client.Create(Application.GetOptionValue('output-client'));

  if Application.HasOption('method-client') then
    Script.MethodServer := TSimbaIPC_Client.Create(Application.GetOptionValue('method-client'));

  if Application.HasOption('state-client') then
    Script.StateServer := TSimbaIPC_Client.Create(Application.GetOptionValue('state-client'));

  Script.Start();
end;

procedure TApplicationHelper.Terminate(Sender: TObject);
begin
  inherited Terminate();

  {$IFDEF DARWIN}
  CocoaWidgetSet.NSApp.Terminate(nil); // Lazarus doesn't exit the message loop
  {$ENDIF}

  if (WakeMainThread <> nil) then
    WakeMainThread(nil);
end;

var
  I: Int32;

begin
  ExitCode := SCRIPT_EXIT_CODE_INITIALIZE;

  try
    if not Application.HasOption('dump') then
    begin
      if Application.HasOption('help') or (Application.ParamCount < 2) or (not Application.HasOption('compile') and (not Application.HasOption('run'))) then
      begin
        WriteLn(
          'Options:'                                                       + LineEnding +
          '  --run:          Runs the given script'                        + LineEnding +
          '  --compile:      Compiles the given script'                    + LineEnding +
          ''                                                               + LineEnding +
          '  --targetwindow: Window handle to target. Defaults to Desktop' + LineEnding +
          '  --apppath:      Defaults to SimbaScript.exe location'         + LineEnding +
          '  --datapath:     Defaults to AppPath/Data'                     + LineEnding +
          '  --pluginpath:   Defaults to AppPath/Plugins'                  + LineEnding +
          '  --fontpath:     Defaults to AppPath/Fonts'                    + LineEnding +
          '  --includepath:  Defaults to AppPath/Includes'                 + LineEnding +
          ''                                                               + LineEnding +
          'Example:'                                                       + LineEnding +
          '  SimbaScript.exe --run script.simba'                           + LineEnding +
          ''
        );

        Exit;
      end;

      if (not FileExists(Application.Params[Application.ParamCount])) then
      begin
        WriteLn('Script not found: ', Application.Params[Application.ParamCount]);

        Exit;
      end;
    end;

    Application.Title := 'SimbaScript';
    Application.CaptureExceptions := False;
    Application.Scaled := True;
    Application.Initialize();

    Application.QueueAsyncCall(@Application.Execute, 0);
    Application.Run();
  except
    on E: Exception do
    begin
      WriteLn(StringOfChar('-', 80));
      WriteLn('');
      WriteLn('Exception: ', E.Message);
      WriteLn('Exception Class: ', E.ClassName);
      WriteLn('');
      WriteLn('Parameters:');
      for i := 1 to Application.ParamCount + 1 do
        WriteLn('  ' + Application.Params[i]);

      WriteLn(StringOfChar('-', 80));
    end;
  end;

  if (Script <> nil) then
    Script.Free();
end.

