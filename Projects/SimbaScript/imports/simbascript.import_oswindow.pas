unit simbascript.import_oswindow;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_OSWindow(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.oswindow;

procedure Lape_OSWindow_Activate(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[1])^.Activate();
end;

procedure Lape_OSWindow_IsVaild(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[1])^.IsValid();
end;

procedure Lape_OSWindow_IsActive(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[1])^.IsActive();
end;

procedure Lape_OSWindow_IsActiveEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[1])^.IsActive(PInt32(Params^[2])^);
end;

procedure Lape_OSWindow_IsVisible(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindow(Params^[1])^.IsVisible();
end;

procedure Lape_OSWindow_GetPID(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PUInt32(Result)^ := POSWindow(Params^[1])^.GetPID();
end;

procedure Lape_OSWindow_GetRootWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := POSWindow(Params^[1])^.GetRootWindow();
end;

procedure Lape_OSWindow_GetTitle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := POSWindow(Params^[1])^.GetTitle();
end;

procedure Lape_OSWindow_GetClassName(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PWideString(Result)^ := POSWindow(Params^[1])^.GetClassName();
end;

procedure Lape_OSWindow_GetBounds(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBox(Result)^ := POSWindow(Params^[1])^.GetBounds();
end;

procedure Lape_OSWindow_GetChildren(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindow(Params^[1])^.GetChildren(PBoolean(Params^[2])^);
end;

procedure Lape_OSWindow_SetBounds(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Params^[1])^.SetBounds(PBox(Params^[2])^);
end;

procedure Lape_OSWindow_Kill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Params^[1])^.Kill();
end;

procedure Lape_OSWindowArray_GetByTitle(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindowArray(Params^[1])^.GetByTitle(PString(Params^[2])^, POSWindow(Params^[3])^);
end;

procedure Lape_OSWindowArray_GetByTitleEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindowArray(Params^[1])^.GetByTitle(PString(Params^[2])^);
end;

procedure Lape_OSWindowArray_GetByClass(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindowArray(Params^[1])^.GetByClass(PString(Params^[2])^, POSWindow(Params^[3])^);
end;

procedure Lape_OSWindowArray_GetByClassEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindowArray(Params^[1])^.GetByClass(PString(Params^[2])^);
end;

procedure Lape_OSWindowArray_GetByTitleAndClass(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := POSWindowArray(Params^[1])^.GetByTitleAndClass(PString(Params^[2])^, PString(Params^[3])^, POSWindow(Params^[4])^);
end;

procedure Lape_OSWindowArray_GetByTitleAndClassEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := POSWindowArray(Params^[1])^.GetByTitleAndClass(PString(Params^[2])^, PString(Params^[3])^);
end;

procedure Lape_OSWindowArray_ToString(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PString(Result)^ := POSWindowArray(Params^[1])^.ToString();
end;

procedure Lape_GetTopWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := GetTopWindows();
end;

procedure Lape_GetVisibleWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := GetVisibleWindows();
end;

procedure Lape_GetWindows(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := GetWindows();
end;

procedure Lape_GetActiveWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := GetActiveWindow();
end;

procedure Lape_GetDesktopWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindow(Result)^ := GetDesktopWindow();
end;

procedure Lape_FindWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := FindWindow(PString(Params^[1])^);
end;

procedure Lape_FindWindowEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindWindow(PString(Params^[1])^, POSWindow(Params^[2])^);
end;

procedure Lape_FindChildWindow(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  POSWindowArray(Result)^ := FindChildWindow(PString(Params^[1])^, PString(Params^[2])^);
end;

procedure Lape_FindChildWindowEx(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  PBoolean(Result)^ := FindChildWindow(PString(Params^[1])^, PString(Params^[2])^, POSWindow(Params^[3])^);
end;

procedure Lape_Import_OSWindow(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'OSWindow';

    addGlobalType('type PtrUInt', 'TOSWindow');
    addGlobalType('array of TOSWindow', 'TOSWindowArray');

    addGlobalMethod('function TOSWindow.Activate: Boolean; constref;', @Lape_OSWindow_Activate, Data);
    addGlobalMethod('function TOSWindow.IsVaild: Boolean; constref;', @Lape_OSWindow_IsVaild, Data);
    addGlobalMethod('function TOSWindow.IsActive: Boolean; constref; overload;', @Lape_OSWindow_IsActive, Data);
    addGlobalMethod('function TOSWindow.IsActive(Time: Int32): Boolean; constref; overload;', @Lape_OSWindow_IsActiveEx, Data);
    addGlobalMethod('function TOSWindow.IsVisible: Boolean; constref;', @Lape_OSWindow_IsVisible, Data);
    addGlobalMethod('function TOSWindow.GetPID: UInt32; constref;', @Lape_OSWindow_GetPID, Data);
    addGlobalMethod('function TOSWindow.GetRootWindow: TOSWindow; constref; ', @Lape_OSWindow_GetRootWindow, Data);
    addGlobalMethod('function TOSWindow.GetClassName: WideString; constref;', @Lape_OSWindow_GetClassName, Data);
    addGlobalMethod('function TOSWindow.GetTitle: WideString; constref;', @Lape_OSWindow_GetTitle, Data);
    addGlobalMethod('function TOSWindow.GetBounds: TBox; constref;', @Lape_OSWindow_GetBounds, Data);
    addGlobalMethod('function TOSWindow.GetChildren(Recursive: Boolean = True): TOSWindowArray; constref;', @Lape_OSWindow_GetChildren, Data);
    addGlobalMethod('procedure TOSWindow.SetBounds(Bounds: TBox); constref;', @Lape_OSWindow_SetBounds, Data);
    addGlobalMethod('procedure TOSWindow.Kill; constref;', @Lape_OSWindow_Kill, Data);

    addGlobalMethod('function TOSWindowArray.GetByTitle(Title: String; out Window: TOSWindow): Boolean; constref; overload;', @Lape_OSWindowArray_GetByTitle, Data);
    addGlobalMethod('function TOSWindowArray.GetByTitle(Title: String): TOSWindowArray; constref; overload;', @Lape_OSWindowArray_GetByTitleEx, Data);

    addGlobalMethod('function TOSWindowArray.GetByClass(ClassName: String; out Window: TOSWindow): Boolean; constref; overload;', @Lape_OSWindowArray_GetByClass, Data);
    addGlobalMethod('function TOSWindowArray.GetByClass(ClassName: String): TOSWindowArray; constref; overload;', @Lape_OSWindowArray_GetByClassEx, Data);

    addGlobalMethod('function TOSWindowArray.GetByTitleAndClass(Title, ClassName: String; out Window: TOSWindow): Boolean; overload;', @Lape_OSWindowArray_GetByTitleAndClass, Data);
    addGlobalMethod('function TOSWindowArray.GetByTitleAndClass(Title, ClassName: String): TOSWindowArray; overload;', @Lape_OSWindowArray_GetByTitleAndClassEx, Data);

    addGlobalMethod('function TOSWindowArray.ToString: String; constref;', @Lape_OSWindowArray_ToString, Data);

    addGlobalMethod('function GetTopWindows: TOSWindowArray', @Lape_GetTopWindows, Data);
    addGlobalMethod('function GetVisibleWindows: TOSWindowArray;', @Lape_GetVisibleWindows, Data);
    addGlobalMethod('function GetWindows: TOSWindowArray;', @Lape_GetWindows, Data);
    addGlobalMethod('function GetActiveWindow: TOSWindow;', @Lape_GetActiveWindow, Data);
    addGlobalMethod('function GetDesktopWindow: TOSWindow;', @Lape_GetDesktopWindow, Data);

    addGlobalMethod('function FindWindow(Title: String): TOSWindowArray; overload;', @Lape_FindWindow, Data);
    addGlobalMethod('function FindWindow(Title: String; out Window: TOSWindow): Boolean; overload;', @Lape_FindWindowEx, Data);
    addGlobalMethod('function FindChildWindow(Title: String; ClassName: String): TOSWindowArray; overload;', @Lape_FindChildWindow, Data);
    addGlobalMethod('function FindChildWindow(Title: String; ClassName: String; out Child: TOSWindow): Boolean; overload;', @Lape_FindChildWindowEx, Data);
  end;
end;

end.

