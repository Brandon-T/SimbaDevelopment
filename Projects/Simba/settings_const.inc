ssSettings = 'Settings/';
ssLastConfig = 'LastConfig/';
<<<<<<< ours
ssScriptManager = 'ScriptManager/';
=======
>>>>>>> theirs

ssUpdater = 'Updater/';
ssFonts = 'Fonts/';
ssIncludes = 'Includes/';
<<<<<<< ours
ssInterpreter = 'Interpreter/';
=======
>>>>>>> theirs
ssTabs = 'Tabs/';
ssColourPicker = 'ColourPicker/';
ssScripts = 'Scripts/';
ssExtensions = 'Extensions/';
ssMainForm = 'MainForm/';
ssSourceEditor = 'SourceEditor/';
ssTray = 'Tray/';
ssConsole = 'Console/';
ssNews = 'News/';
ssPlugins = 'Plugins/';
ssNotes = 'Notes/';
ssMisc = 'Misc/';
ssCodeInsight = 'CodeInsight/';
ssFunctionList = 'FunctionList/';

ssCheckUpdate = ssSettings + ssUpdater + 'CheckForUpdates';
ssCheckUpdateMinutes = ssSettings + ssUpdater + 'CheckEveryXMinutes';
ssUpdaterLink = ssSettings + ssUpdater + 'RemoteLink';
ssUpdaterVersionLink = ssSettings + ssUpdater + 'RemoteVersionLink';
ssAutomaticallyUpdate = ssSettings + ssUpdater + 'AutomaticallyUpdate';
<<<<<<< ours
ssUpdaterReleaseChannel = ssSettings + ssUpdater + 'ReleaseChannel';

ssLoadFontsOnStart = ssSettings + ssFonts + 'LoadOnStartUp';
ssFontsCheckForUpdates = ssSettings + ssFonts + 'CheckForUpdates';
ssFontsVersion = ssSettings + ssFonts + 'Version';
ssFontsLink = ssSettings + ssFonts + 'UpdateLink';
ssFontsVersionLink = ssSettings + ssFonts + 'VersionLink';
ssFontsPath = ssSettings + ssFonts + 'Path';

ssIncludesPath = ssSettings + ssIncludes + 'Path';
ssInterpreterType = ssSettings + ssInterpreter + 'Type';
ssInterpreterAllowSysCalls = ssSettings + ssInterpreter + 'AllowSysCalls';
=======

ssFontsPath = ssSettings + ssFonts + 'Path';
ssIncludesPath = ssSettings + ssIncludes + 'Path';
ssPluginsPath = ssSettings + ssPlugins + 'Path';
>>>>>>> theirs

ssTabsOpenNextOnClose = ssSettings + ssTabs + 'OpenNextOnClose';
ssTabsOpenScriptInNewTab = ssSettings + ssTabs + 'OpenScriptInNewTab';
ssTabsCheckBeforeOpen = ssSettings + ssTabs + 'CheckTabsBeforeOpen';

ssColourPickerShowHistoryOnPick = ssSettings + ssColourPicker + 'ShowHistoryOnPick';
ssColourPickerAddToHistoryOnPick = ssSettings + ssColourPicker + 'AddToHistoryOnPick';
ssColourPickerAddCoordinateToClipBoard = ssSettings + ssColourPicker + 'AddCoordinateToClipBoard';
ssColourPickerAddColourToClipBoard = ssSettings + ssColourPicker + 'AddColourToClipBoard';

ssScriptsPath = ssSettings + ssScripts + 'Path';

<<<<<<< ours
=======
ssScriptExectuable = ssSettings + 'ScriptExectuable/';
ssScriptExectuablePath = ssScriptExectuable + 'Path';
ssScriptExectuableExtractOnStartup = ssScriptExectuable + 'ExtractOnStartup';

ssOpenSSL = ssSettings + 'OpenSSL/';
ssOpenSSLExtractOnStartup = ssOpenSSL + 'ExtractOnStartup';
ssOpenSSLInitalizeOnStartup = ssOpenSSL + 'InitalizeOnStartup';

>>>>>>> theirs
ssCodeHintsShowAutomatically = ssSettings + ssCodeInsight + 'CodeHints/ShowAutomatically';
ssCodeCompletionShowAutomatically = ssSettings + ssCodeInsight + 'CodeCompletion/ShowAutomatically';

ssShowBalloonHints = ssSettings + 'ShowBalloonHints';

ssSourceEditorLazColors = ssSettings + ssSourceEditor + 'LazColors';
ssSourceEditorCaretPastEOL = ssSettings + ssSourceEditor + 'CaretPastEOL';
ssSourceEditorFont = ssSettings + ssSourceEditor + 'Font/';
ssSourceEditorDefScriptPath = ssSettings + ssSourceEditor + 'DefScriptPath';
<<<<<<< ours

ssNewsLink = ssSettings + ssNews + 'URL';
ssPluginsPath = ssSettings + ssPlugins + 'Path';

ssExtensionsExtensionN = ssExtensions + 'Extension';
ssExtensionsPath = ssSettings + ssExtensions + 'Path';
ssExtensionsCount = ssExtensions + 'ExtensionCount';
ssExtensionsFileExtension = ssSettings + ssExtensions + 'FileExtension';
=======
ssSourceEditorHighlighterPath = ssSettings + ssSourceEditor + 'Highlighter';

ssNewsLink = ssSettings + ssNews + 'URL';
>>>>>>> theirs

ssTrayAlwaysVisible = ssSettings + ssTray + 'AlwaysVisible';

ssConsoleVisible = ssLastConfig + ssConsole + 'Visible';

<<<<<<< ours
ssMainFormPosition = ssLastConfig + ssMainForm + 'Position';
=======
ssMainFormLeft = ssLastConfig + ssMainForm + 'Left';
ssMainFormTop = ssLastConfig + ssMainForm + 'Top';
ssMainFormWidth = ssLastConfig + ssMainForm + 'Width';
ssMainFormHeight = ssLastConfig + ssMainForm + 'Height';
>>>>>>> theirs
ssMainFormState = ssLastConfig + ssMainForm + 'State';
ssMainFormNormalSize = ssSettings + ssMainForm + 'NormalSize';
ssFunctionListShown = ssLastConfig + ssMainForm + 'FunctionListShown';

ssCodeInsightShowHidden = ssSettings + ssCodeInsight + 'ShowHidden';
ssFunctionListShowOnStart = ssSettings + ssCodeInsight + ssFunctionList + 'ShowOnStart';

<<<<<<< ours
ssMaxRecentFiles = ssSettings + 'General/MaxRecentFiles';
ssRecentFiles = ssLastConfig + ssMainForm + 'RecentFiles';
ssRecentFilesCount = ssLastConfig + ssMainForm + 'RecentFiles/Count';
ssRecentFileN = ssLastConfig + ssMainForm + 'RecentFiles/File';
=======
ssRecentFiles = ssLastConfig + ssMainForm + 'RecentFiles';
>>>>>>> theirs

ssNotesContent = ssSettings + ssNotes + 'Content';
ssNotesVisible = ssSettings + ssNotes + 'Visible';

<<<<<<< ours
ssMiscRestartScriptIfStarted = ssSettings + ssMisc + 'RestartScriptIfStarted';
ssMiscWarnIfRunning = ssSettings + ssMisc + 'WarnIfRunning';
ssMiscWarnIfModified = ssSettings + ssMisc + 'WarnIfModified';

ssSMURL = ssScriptManager + 'URL';
ssSMPath = ssScriptManager + 'Path';
ssSMFile = ssScriptManager + 'File';
ssFRun = ssScriptManager + 'FirstRun';
=======
ssFileBrowserVisible = ssSettings + 'FileBrowser/Visible';

ssMiscRestartScriptIfStarted = ssSettings + ssMisc + 'RestartScriptIfStarted';
ssMiscWarnIfRunning = ssSettings + ssMisc + 'WarnIfRunning';
ssMiscWarnIfModified = ssSettings + ssMisc + 'WarnIfModified';
ssSaveScriptOnCompile = ssSettings + ssMisc + 'SaveScriptOnCompile';
>>>>>>> theirs
