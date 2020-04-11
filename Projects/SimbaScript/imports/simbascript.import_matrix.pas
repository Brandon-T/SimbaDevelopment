unit simbascript.import_matrix;

{$mode objfpc}{$H+}

interface

{$i import_uses.inc}

procedure Lape_Import_Matrix(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);

implementation

uses
  simba.matrix;

//procedure SetSize(a: TSingleMatrix; Width, Height: Int32);
procedure Lape_MatrixSetSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixSetSize(TSingleMatrix(Params^[1]^), Int32(Params^[2]^), Int32(Params^[3]^));
end;

//procedure Size(a: TSingleMatrix; out Width, Height: Int32);
procedure Lape_MatrixSize(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixSize(TSingleMatrix(Params^[1]^), Int32(Params^[2]^), Int32(Params^[3]^));
end;

//function Width(a: TSingleMatrix): Int32;
procedure Lape_MatrixWidth(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Int32(Result^) := MatrixWidth(TSingleMatrix(Params^[1]^));
end;

//function Height(a: TSingleMatrix): Int32;
procedure Lape_MatrixHeight(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Int32(Result^) := MatrixHeight(TSingleMatrix(Params^[1]^));
end;

//function ToInt(a: TSingleMatrix): T2DIntArray;
procedure Lape_MatrixToInt(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  T2DIntArray(Result^) := MatrixToInt(TSingleMatrix(Params^[1]^));
end;

//function Mean(a: TSingleMatrix): Single;
procedure Lape_MatrixMean(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  Single(Result^) := MatrixMean(TSingleMatrix(Params^[1]^));
end;

//procedure MeanStdev(a: TSingleMatrix; out Mean, Stdev: Double);
procedure Lape_MatrixMeanStdev(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixMeanStdev(TSingleMatrix(Params^[1]^), Single(Params^[2]^), Single(Params^[3]^));
end;

//procedure MinMax(a: TSingleMatrix; out vMin,vMax: Single);
procedure Lape_MatrixMinMax(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixMinMax(TSingleMatrix(Params^[1]^), Single(Params^[2]^), Single(Params^[3]^));
end;

procedure Lape_MatrixMin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var _:Single;
begin
  MatrixMinMax(TSingleMatrix(Params^[1]^), Single(Result^), _);
end;

procedure Lape_MatrixMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
var _:Single;
begin
  MatrixMinMax(TSingleMatrix(Params^[1]^), _, Single(Result^));
end;

//function ArgMax(a: TSingleMatrix): TPoint;
procedure Lape_MatrixArgMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPoint(Result^) := MatrixArgMax(TSingleMatrix(Params^[1]^));
end;

//function ArgMin(a: TSingleMatrix): TPoint;
procedure Lape_MatrixArgMin(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPoint(Result^) := MatrixArgMin(TSingleMatrix(Params^[1]^));
end;

//function NormMinMax(a: TSingleMatrix; Alpha, Beta: Single): TSingleMatrix;
procedure Lape_NormMinMax(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSingleMatrix(Result^) := MatrixNormMinMax(TSingleMatrix(Params^[1]^), Single(Params^[2]^), Single(Params^[3]^));
end;

//function Indices(a: TSingleMatrix; Value: Single; Comparator: EComparator): TPointArray;  
procedure Lape_MatrixIndices(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TPointArray(Result^) := MatrixIndices(TSingleMatrix(Params^[1]^), Single(Params^[2]^), EComparator(Params^[3]^));
end;

//function Extract(a: TSingleMatrix; Indices: TPointArray): TSingleArray;
procedure Lape_MatrixExtract(const Params: PParamArray; const Result: Pointer); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  TSingleArray(Result^) := MatrixExtract(TSingleMatrix(Params^[1]^), TPointArray(Params^[2]^));
end;

//procedure Fill(a: TSingleMatrix; Indices: TPointArray; Values: TSingleArray);
procedure Lape_MatrixFill(const Params: PParamArray); {$IFDEF Lape_CDECL}cdecl;{$ENDIF}
begin
  MatrixFill(TSingleMatrix(Params^[1]^), TPointArray(Params^[2]^), TSingleArray(Params^[3]^));
end;

procedure Lape_Import_Matrix(Compiler: TSimbaScript_Compiler; Data: Pointer = nil);
begin
  with Compiler do
  begin
    Section := 'Matrix';

    addGlobalMethod('procedure TSingleMatrix.SetSize(Width, Height: Int32);', @Lape_MatrixSetSize, Data);
    addGlobalMethod('procedure TSingleMatrix.Size(out Width, Height: Int32); constref;', @Lape_MatrixSize, Data);
    addGlobalMethod('function TSingleMatrix.Width(): Int32; constref;', @Lape_MatrixWidth, Data);
    addGlobalMethod('function TSingleMatrix.Height(): Int32; constref;', @Lape_MatrixHeight, Data);
    addGlobalMethod('function TSingleMatrix.ToInt(): T2DIntArray; constref;', @Lape_MatrixToInt, Data);
    addGlobalMethod('function TSingleMatrix.Mean(): Single; constref;', @Lape_MatrixMean, Data);
    addGlobalMethod('procedure TSingleMatrix.MeanStdev(out Mean, Stdev: Double); constref;', @Lape_MatrixMeanStdev, Data);
    addGlobalMethod('procedure TSingleMatrix.MinMax(out vMin,vMax: Single); constref;', @Lape_MatrixMinMax, Data);
    addGlobalMethod('function TSingleMatrix.Min(): Single; constref;', @Lape_MatrixMin, Data);
    addGlobalMethod('function TSingleMatrix.Max(): Single; constref;', @Lape_MatrixMax, Data);
    addGlobalMethod('function TSingleMatrix.ArgMax(): TPoint; constref;', @Lape_MatrixArgMax, Data);
    addGlobalMethod('function TSingleMatrix.ArgMin(): TPoint; constref;', @Lape_MatrixArgMin, Data);
    addGlobalMethod('function TSingleMatrix.NormMinMax(Alpha, Beta: Single): TSingleMatrix; constref;', @Lape_NormMinMax, Data);
    addGlobalMethod('function TSingleMatrix.Indices(Value: Single; Comparator: EComparator): TPointArray; constref;', @Lape_MatrixIndices, Data);
    addGlobalMethod('function TSingleMatrix.Extract(Indices: TPointArray): TSingleArray; constref;', @Lape_MatrixExtract, Data);
    addGlobalMethod('procedure TSingleMatrix.Fill(Indices: TPointArray; Values: TSingleArray); constref;', @Lape_MatrixFill, Data);
  end;
end;

end.

