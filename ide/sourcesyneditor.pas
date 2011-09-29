{
/***************************************************************************
                               SourceSynEditor
                             -------------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Abstract:
    SynEdit extensions for the IDE
    - DebugMarks: Mark lines with debug info
}
unit SourceSynEditor;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
  Classes, SysUtils, LCLProc, Graphics, Menus, math, LazarusIDEStrConsts,
  SynEdit, SynEditMiscClasses, SynGutter, SynGutterBase, SynEditMarks,
  SynGutterLineNumber, SynGutterCodeFolding, SynGutterMarks, SynGutterChanges,
  SynGutterLineOverview, SynEditMarkup, SynEditMarkupGutterMark,
  SynEditTextBuffer, SynEditFoldedView, SynTextDrawer, SynEditTextBase,
  SynPluginTemplateEdit, SynPluginSyncroEdit,
  SynEditHighlighter, SynEditHighlighterFoldBase, SynHighlighterPas;

type

  TIDESynGutterMarks = class;

  { TIDESynEditor }

  TIDESynEditor = class(TSynEdit)
  private
    FSyncroEdit: TSynPluginSyncroEdit;
    FTemplateEdit: TSynPluginTemplateEdit;
    FMarkupForGutterMark: TSynEditMarkupGutterMark;
    function GetIDEGutterMarks: TIDESynGutterMarks;
  protected
    function CreateGutter(AOwner : TSynEditBase; ASide: TSynGutterSide;
                          ATextDrawer: TheTextDrawer): TSynGutter; override;
  public
    constructor Create(AOwner: TComponent); override;
    function TextIndexToViewPos(aTextIndex : Integer) : Integer;
    property IDEGutterMarks: TIDESynGutterMarks read GetIDEGutterMarks;
    property TopView;
    property TextBuffer;
    property TemplateEdit: TSynPluginTemplateEdit read FTemplateEdit;
    property SyncroEdit: TSynPluginSyncroEdit read FSyncroEdit;
  end;

  TIDESynHighlighterPasRangeList = class(TSynHighlighterPasRangeList)
  protected
    FInterfaceLine, FImplementationLine,
    FInitializationLine, FFinalizationLine: Integer;
  end;

  { TIDESynPasSyn }

  TIDESynPasSyn = class(TSynPasSyn)
  private
    function GetFinalizationLine: Integer;
    function GetImplementationLine: Integer;
    function GetInitializationLine: Integer;
    function GetInterfaceLine: Integer;
  protected
    function CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList; override;
    function StartCodeFoldBlock(ABlockType: Pointer;
              IncreaseLevel: Boolean = true): TSynCustomCodeFoldBlock; override;
  public
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string;
      LineNumber: Integer); override;
    property InterfaceLine: Integer read GetInterfaceLine;
    property ImplementationLine: Integer read GetImplementationLine;
    property InitializationLine: Integer read GetInitializationLine;
    property FinalizationLine: Integer read GetFinalizationLine;
  end;

  { TIDESynFreePasSyn }

  TIDESynFreePasSyn = class(TIDESynPasSyn)
  public
    constructor Create(AOwner: TComponent); override;
    procedure ResetRange; override;
  end;

  { TIDESynGutterLOvProviderPascal }

  TIDESynGutterLOvProviderPascal = class(TSynGutterLineOverviewProvider)
  private
    FColor2: TColor;
    FInterfaceLine, FImplementationLine,
    FInitializationLine, FFinalizationLine: Integer;
    FPixInterfaceLine, FPixImplementationLine,
    FPixInitializationLine, FPixFinalizationLine: Integer;
    FPixEndInterfaceLine, FPixEndImplementationLine,
    FPixEndInitializationLine, FPixEndFinalizationLine: Integer;
    FSingleLine: Boolean;
    FRGBColor2: TColor;
    procedure SetColor2(const AValue: TColor);
    procedure SetSingleLine(const AValue: Boolean);
  protected
    procedure BufferChanged(Sender: TObject);
    procedure HighlightChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    procedure ReCalc; override;

    procedure Paint(Canvas: TCanvas; AClip: TRect; TopOffset: integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
  published
    property SingleLine: Boolean read FSingleLine write SetSingleLine;
    property Color2: TColor read FColor2 write SetColor2;
  end;

  { TIDESynGutterLOvProviderIDEMarks }

  TIDESynGutterLOvProviderIDEMarks = class(TSynGutterLOvProviderBookmarks)
  // Bookmarsk and breakpoints
  private
    FBreakColor: TColor;
    FRGBBreakColor: TColor;
    procedure SetBreakColor(const AValue: TColor);
  protected
    procedure AdjustColorForMark(AMark: TSynEditMark; var AColor: TColor; var APriority: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BreakColor: TColor read FBreakColor write SetBreakColor;
  end;

  { TIDESynGutter }

  TIDESynGutter = class(TSynGutter)
  protected
    procedure CreateDefaultGutterParts; override;
  end;

  { TIDESynDebugMarkInfo }

  TIDESynDebugMarkInfo = class(TSynManagedStorageMem)
  private
    FRefCount: Integer;
    function GetSrcLineToMarkLine(SrcIndex: Integer): Integer;
    procedure SetSrcLineToMarkLine(SrcIndex: Integer; const AValue: Integer);
  public
    constructor Create;
    procedure IncRefCount;
    procedure DecRefCount;
    // Index is the Current line-index (0 based) in editor (including source modification)
    // Result is the original Line-pos (1 based) as known by the debugger
    property SrcLineToMarkLine[SrcIndex: Integer]: Integer
             read GetSrcLineToMarkLine write SetSrcLineToMarkLine; default;
    property RefCount: Integer read FRefCount;
  end;

  { TIDESynGutterMarks }

  TIDESynGutterMarks = class(TSynGutterMarks)
  private
    FDebugMarkInfo: TIDESynDebugMarkInfo;
    FMarkInfoTextBuffer: TSynEditStrings;
  protected
    procedure CheckTextBuffer;       // Todo: Add a notification, when TextBuffer Changes
    Procedure PaintLine(aScreenLine: Integer; Canvas : TCanvas; AClip : TRect); override;
  public
    destructor Destroy; override;
    procedure SetDebugMarks(AFirstLinePos, ALastLinePos: Integer);
    procedure ClearDebugMarks;
    function HasDebugMarks: Boolean;
    function DebugLineToSourceLine(aLinePos: Integer): Integer;
    function SourceLineToDebugLine(aLinePos: Integer; AdjustOnError: Boolean = False): Integer;
  end;

  { TIDESynGutterCodeFolding }

  TIDESynGutterCodeFolding = class(TSynGutterCodeFolding)
  protected
    procedure PopClickedUnfoldAll(Sender: TObject);
    procedure PopClickedUnfoldComment(Sender: TObject);
    procedure PopClickedFoldComment(Sender: TObject);
    procedure PopClickedHideComment(Sender: TObject);
    procedure CreatePopUpMenuEntries(APopUp: TPopupMenu; ALine: Integer); override;
  end;

implementation

{ TIDESynEditor }

function TIDESynEditor.GetIDEGutterMarks: TIDESynGutterMarks;
begin
  Result := TIDESynGutterMarks(Gutter.Parts.ByClass[TIDESynGutterMarks, 0]);
end;

function TIDESynEditor.CreateGutter(AOwner: TSynEditBase; ASide: TSynGutterSide;
  ATextDrawer: TheTextDrawer): TSynGutter;
begin
  Result := TIDESynGutter.Create(AOwner, ASide, ATextDrawer);
end;

constructor TIDESynEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTemplateEdit:=TSynPluginTemplateEdit.Create(Self);
  FSyncroEdit := TSynPluginSyncroEdit.Create(Self);
  FMarkupForGutterMark := TSynEditMarkupGutterMark.Create(Self, FWordBreaker);
  TSynEditMarkupManager(MarkupMgr).AddMarkUp(FMarkupForGutterMark);
end;

function TIDESynEditor.TextIndexToViewPos(aTextIndex: Integer): Integer;
begin
  Result := TextView.TextIndexToViewPos(aTextIndex - 1);
end;

{ TIDESynPasSyn }

function TIDESynPasSyn.GetFinalizationLine: Integer;
begin
  Result := TIDESynHighlighterPasRangeList(CurrentRanges).FFinalizationLine;
end;

function TIDESynPasSyn.GetImplementationLine: Integer;
begin
  Result := TIDESynHighlighterPasRangeList(CurrentRanges).FImplementationLine;
end;

function TIDESynPasSyn.GetInitializationLine: Integer;
begin
  Result := TIDESynHighlighterPasRangeList(CurrentRanges).FInitializationLine;
end;

function TIDESynPasSyn.GetInterfaceLine: Integer;
begin
  Result := TIDESynHighlighterPasRangeList(CurrentRanges).FInterfaceLine;
end;

function TIDESynPasSyn.CreateRangeList(ALines: TSynEditStringsBase): TSynHighlighterRangeList;
begin
  Result := TIDESynHighlighterPasRangeList.Create;
  TIDESynHighlighterPasRangeList(Result).FInterfaceLine := -1;
  TIDESynHighlighterPasRangeList(Result).FImplementationLine := -1;
  TIDESynHighlighterPasRangeList(Result).FInitializationLine := -1;
  TIDESynHighlighterPasRangeList(Result).FFinalizationLine := -1;
end;

function TIDESynPasSyn.StartCodeFoldBlock(ABlockType: Pointer;
  IncreaseLevel: Boolean): TSynCustomCodeFoldBlock;
begin
  if (ABlockType = Pointer(PtrInt(cfbtUnitSection))) or
     (ABlockType = Pointer(PtrInt(cfbtUnitSection)) + PtrUInt(CountPascalCodeFoldBlockOffset))
  then begin
    if KeyComp('Interface') then
      TIDESynHighlighterPasRangeList(CurrentRanges).FInterfaceLine := LineIndex  + 1;
    if KeyComp('Implementation') then
      TIDESynHighlighterPasRangeList(CurrentRanges).FImplementationLine := LineIndex  + 1;
    if KeyComp('Initialization') then
      TIDESynHighlighterPasRangeList(CurrentRanges).FInitializationLine := LineIndex  + 1;
    if KeyComp('Finalization') then
      TIDESynHighlighterPasRangeList(CurrentRanges).FFinalizationLine := LineIndex  + 1;
  end;
  Result := inherited;
end;

procedure TIDESynPasSyn.SetLine(const NewValue: string; LineNumber: Integer);
begin
  if assigned(CurrentRanges) then begin
    if TIDESynHighlighterPasRangeList(CurrentRanges).FInterfaceLine = LineNumber + 1 then
      TIDESynHighlighterPasRangeList(CurrentRanges).FInterfaceLine := -1;
    if TIDESynHighlighterPasRangeList(CurrentRanges).FImplementationLine = LineNumber + 1 then
      TIDESynHighlighterPasRangeList(CurrentRanges).FImplementationLine := -1;
    if TIDESynHighlighterPasRangeList(CurrentRanges).FInitializationLine = LineNumber + 1 then
      TIDESynHighlighterPasRangeList(CurrentRanges).FInitializationLine := -1;
    if TIDESynHighlighterPasRangeList(CurrentRanges).FFinalizationLine = LineNumber + 1 then
      TIDESynHighlighterPasRangeList(CurrentRanges).FFinalizationLine := -1;
  end;
  inherited SetLine(NewValue, LineNumber);
end;

{ TIDESynFreePasSyn }

constructor TIDESynFreePasSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CompilerMode:=pcmObjFPC;
end;

procedure TIDESynFreePasSyn.ResetRange;
begin
  inherited ResetRange;
  CompilerMode:=pcmObjFPC;
end;

{ TIDESynGutterLOvProviderPascal }

procedure TIDESynGutterLOvProviderPascal.SetSingleLine(const AValue: Boolean);
begin
  if FSingleLine = AValue then exit;
  FSingleLine := AValue;
  InvalidatePixelLines(0, Height);
end;

procedure TIDESynGutterLOvProviderPascal.SetColor2(const AValue: TColor);
begin
  if FColor2 = AValue then exit;
  FColor2 := AValue;
  FRGBColor2 := ColorToRGB(AValue);
  DoChange(Self);
end;

procedure TIDESynGutterLOvProviderPascal.BufferChanged(Sender: TObject);
begin
  TSynEditStringList(Sender).RemoveHanlders(self);
  TSynEditStringList(TextBuffer).AddGenericHandler(senrHighlightChanged,
    TMethod({$IFDEF FPC}@{$ENDIF}HighlightChanged));
  TSynEditStringList(TextBuffer).AddGenericHandler(senrTextBufferChanged,
    TMethod({$IFDEF FPC}@{$ENDIF}BufferChanged));
  //LineCountChanged(nil, 0, 0);
end;

procedure TIDESynGutterLOvProviderPascal.HighlightChanged(Sender: TSynEditStrings; AIndex,
  ACount: Integer);
var
  hl: TIDESynPasSyn;
  procedure Update(var TheVal: Integer; NewVal: Integer);
  begin
    if TheVal = NewVal then exit;
    if FSingleLine then begin
      InvalidatePixelLines(TheVal, TheVal);
      InvalidatePixelLines(NewVal, NewVal);
    end else begin
      InvalidatePixelLines(Min(TheVal, NewVal), Height);
    end;

    TheVal := NewVal;
  end;
var i1,i1e,i2,i2e,i3,i3e,i4,i4e: Integer;
begin
  i1  := FPixInterfaceLine;
  i1e := FPixEndInterfaceLine;
  i2  := FPixImplementationLine;
  i2e := FPixEndImplementationLine;
  i3  := FPixInitializationLine;
  i3e := FPixEndInitializationLine;
  i4  := FPixFinalizationLine;
  i4e := FPixEndFinalizationLine;
  if not(TSynEdit(SynEdit).Highlighter is TIDESynPasSyn) then begin
    FInterfaceLine := -1;
    FInterfaceLine := -1;
    FInitializationLine := -1;
    FFinalizationLine := -1;
  end else begin
    hl := TSynEdit(SynEdit).Highlighter as TIDESynPasSyn;
    if hl.CurrentLines = nil then exit;
    FInterfaceLine :=      hl.InterfaceLine;
    FImplementationLine := hl.ImplementationLine;
    FInitializationLine := hl.InitializationLine;
    FFinalizationLine :=   hl.FinalizationLine;
  end;

  ReCalc;

  if (i1 <> FPixInterfaceLine) or (i1e <> FPixEndInterfaceLine) then begin
    InvalidatePixelLines(i1,i1e);
    InvalidatePixelLines(FPixInterfaceLine, FPixEndInterfaceLine);
  end;
  if (i2 <> FPixImplementationLine) or (i2e <> FPixEndImplementationLine) then begin
    InvalidatePixelLines(i2,i2e);
    InvalidatePixelLines(FPixImplementationLine, FPixEndImplementationLine);
  end;
  if (i3 <> FPixInitializationLine) or (i3e <> FPixEndInitializationLine) then begin
    InvalidatePixelLines(i3,i3e);
    InvalidatePixelLines(FPixInitializationLine, FPixEndInitializationLine);
  end;
  if (i4 <> FPixFinalizationLine) or (i4e <> FPixEndFinalizationLine) then begin
    InvalidatePixelLines(i4,i4e);
    InvalidatePixelLines(FPixFinalizationLine, FPixEndFinalizationLine);
  end;
end;

procedure TIDESynGutterLOvProviderPascal.ReCalc;
begin
  FPixInterfaceLine      := TextLineToPixel(FInterfaceLine);
  FPixImplementationLine := TextLineToPixel(FImplementationLine);
  FPixInitializationLine := TextLineToPixel(FInitializationLine);
  FPixFinalizationLine   := TextLineToPixel(FFinalizationLine);

  if SingleLine then begin
    if FPixInterfaceLine < 0 then
      FPixEndInterfaceLine := -1
    else
      FPixEndInterfaceLine      := TextLineToPixelEnd(FInterfaceLine) + 1;

    if FPixImplementationLine < 0 then
      FPixEndImplementationLine := -1
    else
      FPixEndImplementationLine := TextLineToPixelEnd(FImplementationLine) + 1;

    if FPixInitializationLine < 0 then
      FPixEndInitializationLine := -1
    else
      FPixEndInitializationLine := TextLineToPixelEnd(FInitializationLine) + 1;

    if FPixFinalizationLine < 0 then
      FPixEndFinalizationLine := -1
    else
      FPixEndFinalizationLine   := TextLineToPixelEnd(FFinalizationLine) + 1;
  end else begin
    if FPixInterfaceLine < 0 then
      FPixEndInterfaceLine := -1
    else if FPixImplementationLine >= 0 then
      FPixEndInterfaceLine := FPixImplementationLine - 1
    else if FPixInitializationLine >= 0 then
      FPixEndInterfaceLine := FPixInitializationLine - 1
    else if FPixFinalizationLine >= 0 then
      FPixEndInterfaceLine := FPixFinalizationLine - 1
    else
      FPixEndInterfaceLine := Height - 1;

    if FPixImplementationLine < 0 then
      FPixEndImplementationLine := -1
    else if FPixInitializationLine >= 0 then
      FPixEndImplementationLine := FPixInitializationLine - 1
    else if FPixFinalizationLine >= 0 then
      FPixEndImplementationLine := FPixFinalizationLine - 1
    else
      FPixEndImplementationLine := Height - 1;

    if FPixInitializationLine < 0 then
      FPixEndInitializationLine := -1
    else if FPixFinalizationLine >= 0 then
      FPixEndInitializationLine := FPixFinalizationLine - 1
    else
      FPixEndInitializationLine := Height - 1;

    if FPixFinalizationLine < 0 then
      FPixEndFinalizationLine := -1
    else
      FPixEndFinalizationLine := Height - 1;
  end;
end;

procedure TIDESynGutterLOvProviderPascal.Paint(Canvas: TCanvas; AClip: TRect;
  TopOffset: integer);
  procedure DrawArea(AStartLine, AEndLine: Integer; C: TColor);
  var r: TRect;
  begin
    if (C = clNone) and SingleLine then
      c := Color;
    if (C = clNone) then
      exit;

    if (AStartLine + TopOffset > AClip.Bottom) or
       (AEndLine + TopOffset < AClip.Top)
    then
      exit;
    r := AClip;
    r.Top    := Max(r.Top, AStartLine + TopOffset);
    r.Bottom := Min(r.Bottom, AEndLine + 1 + TopOffset);
    Canvas.Brush.Color := C;
    Canvas.FillRect(r);
  end;
var
  C2, C3: TColor;
begin
  if FPixInterfaceLine >= 0 then
    DrawArea(FPixInterfaceLine, FPixEndInterfaceLine, Color);

  if FPixImplementationLine >= 0 then
    DrawArea(FPixImplementationLine, FPixEndImplementationLine, Color2);

  C2 := Color;
  C3 := Color2;
  if FPixImplementationLine < 0 then begin
    C2 := Color2;
    if FPixInitializationLine >= 0 then
      C3 := Color;
  end;

  if FPixInitializationLine >= 0 then
    DrawArea(FPixInitializationLine, FPixEndInitializationLine, C2);

  if FPixFinalizationLine >= 0 then
    DrawArea(FPixFinalizationLine, FPixEndFinalizationLine, C3);
end;

constructor TIDESynGutterLOvProviderPascal.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SingleLine := False;
  Color  := $D4D4D4;
  Color2 := $E8E8E8;
  TSynEditStringList(TextBuffer).AddGenericHandler(senrHighlightChanged,
    TMethod({$IFDEF FPC}@{$ENDIF}HighlightChanged));
  TSynEditStringList(TextBuffer).AddGenericHandler(senrTextBufferChanged,
    TMethod({$IFDEF FPC}@{$ENDIF}BufferChanged));
end;

destructor TIDESynGutterLOvProviderPascal.Destroy;
begin
  TSynEditStringList(TextBuffer).RemoveHanlders(self);
  inherited Destroy;
end;

{ TIDESynGutterLOvProviderIDEMarks }

procedure TIDESynGutterLOvProviderIDEMarks.SetBreakColor(const AValue: TColor);
begin
  if FBreakColor = AValue then exit;
  FBreakColor := AValue;
  FRGBBreakColor := ColorToRGB(AValue);
  DoChange(Self);
end;

procedure TIDESynGutterLOvProviderIDEMarks.AdjustColorForMark(AMark: TSynEditMark;
  var AColor: TColor; var APriority: Integer);
begin
  if not AMark.IsBookmark then begin
    AColor := FRGBBreakColor;
    inc(APriority);
  end;
  inherited AdjustColorForMark(AMark, AColor, APriority);
end;

constructor TIDESynGutterLOvProviderIDEMarks.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BreakColor := $0080C8;
end;

{ TIDESynGutter }

procedure TIDESynGutter.CreateDefaultGutterParts;
begin
  if Side = gsLeft then begin
    with TIDESynGutterMarks.Create(Parts) do
      Name := 'SynGutterMarks1';
    with TSynGutterLineNumber.Create(Parts) do
      Name := 'SynGutterLineNumber1';
    with TSynGutterChanges.Create(Parts) do
      Name := 'SynGutterChanges1';
    with TSynGutterSeparator.Create(Parts) do
      Name := 'SynGutterSeparator1';
    with TIDESynGutterCodeFolding.Create(Parts) do
      Name := 'SynGutterCodeFolding1';
  {$IFDEF WithSynOverviewGutter}
  end
  else begin
    with TSynGutterSeparator.Create(Parts) do
      Name := 'SynGutterSeparatorR';
    with TSynGutterLineOverview.Create(Parts) do begin
      Name := 'SynGutterLineOverview1';
      with TIDESynGutterLOvProviderIDEMarks.Create(Providers) do
        Priority := 20;
      with TSynGutterLOvProviderModifiedLines.Create(Providers) do
        Priority := 9;
      with TSynGutterLOvProviderCurrentPage.Create(Providers) do
        Priority := 1;
      with TIDESynGutterLOvProviderPascal.Create(Providers) do
        Priority := 0;
    end;
    with TSynGutterSeparator.Create(Parts) do begin
      Name := 'SynGutterSeparatorR2';
      AutoSize := False;
      Width := 1;
      LineWidth := 0;
    end;
  {$ENDIF}
  end;
end;

{ TIDESynGutterMarks }

procedure TIDESynGutterMarks.CheckTextBuffer;
begin
  if (FMarkInfoTextBuffer <> nil) and
     (FMarkInfoTextBuffer <> TIDESynEditor(SynEdit).TextBuffer)
  then begin
    FMarkInfoTextBuffer := nil;
    if FDebugMarkInfo <> nil then FDebugMarkInfo.DecRefCount;
    if (FDebugMarkInfo <> nil) and (FDebugMarkInfo.RefCount = 0) then
      FreeAndNil(FDebugMarkInfo);
  end;
end;

procedure TIDESynGutterMarks.PaintLine(aScreenLine: Integer; Canvas: TCanvas; AClip: TRect);
var
  aGutterOffs, TxtIdx: Integer;
  HasAnyMark: Boolean;

  procedure DrawDebugMark(Line: Integer);
  var
    itop : Longint;
    LineHeight: LongInt;
  begin
    if Line < 0 then Exit;
    if Assigned(FBookMarkOpt.BookmarkImages) and
       (DebugMarksImageIndex <= FBookMarkOpt.BookmarkImages.Count) and
       (DebugMarksImageIndex >= 0) then
    begin
      LineHeight := TSynEdit(SynEdit).LineHeight;
      iTop := 0;
      if LineHeight > FBookMarkOpt.BookmarkImages.Height then
        iTop := (LineHeight - FBookMarkOpt.BookmarkImages.Height) div 2;

      FBookMarkOpt.BookmarkImages.Draw
        (Canvas, FBookMarkOpt.LeftMargin + aGutterOffs * ColumnWidth,
         iTop + Line * LineHeight, DebugMarksImageIndex, True);
    end
  end;

begin
  CheckTextBuffer;
  aGutterOffs := 0;
  HasAnyMark := PaintMarks(aScreenLine, Canvas, AClip, aGutterOffs);
  TxtIdx := FoldView.TextIndex[aScreenLine];
  if (not HasAnyMark) and (HasDebugMarks) and (TxtIdx < FDebugMarkInfo.Count) and
     (FDebugMarkInfo.SrcLineToMarkLine[TxtIdx] > 0)
  then
    DrawDebugMark(aScreenLine);
end;

destructor TIDESynGutterMarks.Destroy;
begin
  ClearDebugMarks;
  inherited;
end;

procedure TIDESynGutterMarks.SetDebugMarks(AFirstLinePos, ALastLinePos: Integer);
var
  i: LongInt;
begin
  CheckTextBuffer;

  if FDebugMarkInfo = nil then begin
    FDebugMarkInfo := TIDESynDebugMarkInfo(TIDESynEditor(SynEdit).TextBuffer.Ranges[ClassType]);
    if FDebugMarkInfo = nil then begin
      FDebugMarkInfo := TIDESynDebugMarkInfo.Create;
      // Todo: Add a notification, when TextBuffer Changes
      FMarkInfoTextBuffer := TIDESynEditor(SynEdit).TextBuffer;
      TIDESynEditor(SynEdit).TextBuffer.Ranges[ClassType] := FDebugMarkInfo;
    end
    else
      FDebugMarkInfo.IncRefCount;
  end;

  if ALastLinePos > FDebugMarkInfo.Count then begin
    //debugln(['Request to set debug-mark out of range: max-count=',FDebugMarkInfo.Count,' Marks=',AFirstLinePos,' to=',ALastLinePos]);
    ALastLinePos := FDebugMarkInfo.Count;
  end;
  if AFirstLinePos < 1 then begin
    //debugln(['Request to set debug-mark out of range: max-count=',FDebugMarkInfo.Count,' Marks=',AFirstLinePos,' to=',ALastLinePos]);
    AFirstLinePos := 1;
  end;
  for i := AFirstLinePos - 1 to ALastLinePos - 1 do
    FDebugMarkInfo[i] := i + 1;
  TSynEdit(SynEdit).InvalidateGutter;
end;

procedure TIDESynGutterMarks.ClearDebugMarks;
begin
  CheckTextBuffer;

  if FDebugMarkInfo = nil then exit;
  FDebugMarkInfo.DecRefCount;
  if FDebugMarkInfo.RefCount = 0 then begin
    TIDESynEditor(SynEdit).TextBuffer.Ranges[ClassType] := nil;
    FreeAndNil(FDebugMarkInfo);
  end;
  FDebugMarkInfo := nil;
  FMarkInfoTextBuffer := nil;
  TSynEdit(SynEdit).InvalidateGutter;
end;

function TIDESynGutterMarks.HasDebugMarks: Boolean;
begin
  CheckTextBuffer;
  if FDebugMarkInfo = nil then begin
    FDebugMarkInfo := TIDESynDebugMarkInfo(TIDESynEditor(SynEdit).TextBuffer.Ranges[ClassType]);
    if FDebugMarkInfo <> nil then begin
      FDebugMarkInfo.IncRefCount;
      TSynEdit(SynEdit).InvalidateGutter;
    end;
  end;
  Result := FDebugMarkInfo <> nil;
end;

function TIDESynGutterMarks.DebugLineToSourceLine(aLinePos: Integer): Integer;
var
  i, c: LongInt;
begin
  CheckTextBuffer;
  if (aLinePos < 1) or (not HasDebugMarks) then exit(aLinePos);
  Result := aLinePos - 1; // 0 based
  if (FDebugMarkInfo[Result] = 0) or (FDebugMarkInfo[Result] > aLinePos) then begin
    i := Result;
    repeat
      dec(i);
      while (i >= 0) and (FDebugMarkInfo[i] = 0) do dec(i);
      if (i < 0) or (FDebugMarkInfo[i] < aLinePos) then break;
      Result := i;
    until FDebugMarkInfo[Result] = aLinePos;
    if (FDebugMarkInfo[Result] > aLinePos) and // line not found
       (Result > 0) and (FDebugMarkInfo[Result - 1] = 0)
    then
      dec(Result);
  end;
  if (FDebugMarkInfo[Result] = 0) or (FDebugMarkInfo[Result] < aLinePos) then begin
    c := FDebugMarkInfo.Count;
    i := Result;
    repeat
      inc(i);
      while (i < c) and (FDebugMarkInfo[i] = 0) do inc(i);
      if (i >= c) or (FDebugMarkInfo[i] > aLinePos) then break;
      Result := i;
    until FDebugMarkInfo[Result] = aLinePos;
    if (FDebugMarkInfo[Result] < aLinePos) and // line not found
       (Result < c-1) and (FDebugMarkInfo[Result + 1] = 0)
    then
      inc(Result);
  end;
  inc(Result); // 1 based
end;

function TIDESynGutterMarks.SourceLineToDebugLine(aLinePos: Integer;
  AdjustOnError: Boolean): Integer;
begin
  CheckTextBuffer;
  if (aLinePos < 1) or (not HasDebugMarks) or (aLinePos >= FDebugMarkInfo.Count) then
    exit(aLinePos);
  Result := FDebugMarkInfo[aLinePos - 1];
  while (Result = 0) and AdjustOnError and (aLinePos < FDebugMarkInfo.Count-1) do begin
    inc(aLinePos);
    Result := FDebugMarkInfo[aLinePos - 1];
  end;
end;

{ TIDESynDebugMarkInfo }

function TIDESynDebugMarkInfo.GetSrcLineToMarkLine(SrcIndex: Integer): Integer;
begin
  Result := Integer(ItemPointer[SrcIndex]^);
end;

procedure TIDESynDebugMarkInfo.SetSrcLineToMarkLine(SrcIndex: Integer; const AValue: Integer);
begin
  Integer(ItemPointer[SrcIndex]^) := AValue;
end;

constructor TIDESynDebugMarkInfo.Create;
begin
  Inherited;
  ItemSize := SizeOf(Integer);
  FRefCount := 1;
end;

procedure TIDESynDebugMarkInfo.IncRefCount;
begin
  inc(FRefCount);
end;

procedure TIDESynDebugMarkInfo.DecRefCount;
begin
  dec(FRefCount);
end;

{ TIDESynGutterCodeFolding }

procedure TIDESynGutterCodeFolding.PopClickedUnfoldAll(Sender: TObject);
var
  i, y1, y2: Integer;
begin
  if not TSynEdit(SynEdit).SelAvail then exit;
  y1 := TSynEdit(SynEdit).BlockBegin.Y;
  y2 := TSynEdit(SynEdit).BlockEnd.Y;
  if TSynEdit(SynEdit).BlockEnd.X = 1 then dec(y2);
  for i := y1-1 to y2-1 do
    FoldView.UnFoldAtTextIndex(i);
end;

procedure TIDESynGutterCodeFolding.PopClickedUnfoldComment(Sender: TObject);
var
  i, j, y1, y2: Integer;
  FldInf: TSynFoldNodeInfo;
begin
  if not TSynEdit(SynEdit).SelAvail then exit;
  y1 := TSynEdit(SynEdit).BlockBegin.Y;
  y2 := TSynEdit(SynEdit).BlockEnd.Y;
  if TSynEdit(SynEdit).BlockEnd.X = 1 then dec(y2);
  for i := y1-1 to y2-1 do begin
    j := FoldView.FoldProvider.FoldOpenCount(i);
    while j > 0 do begin
      dec(j);
      if FoldView.IsFoldedAtTextIndex(i,j) then begin
        FldInf := FoldView.FoldProvider.FoldOpenInfo(i, j);
        if TPascalCodeFoldBlockType(PtrUInt(FldInf.FoldType)) in
           [cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment]
        then begin
          FoldView.UnFoldAtTextIndex(i, j, 1, False, 0);
          FoldView.UnFoldAtTextIndex(i, j, 1, False, 1);
        end;
      end;
    end;
  end;
end;

procedure TIDESynGutterCodeFolding.PopClickedFoldComment(Sender: TObject);
var
  i, j, y1, y2: Integer;
  FldInf: TSynFoldNodeInfo;
begin
  if not TSynEdit(SynEdit).SelAvail then exit;
  y1 := TSynEdit(SynEdit).BlockBegin.Y;
  y2 := TSynEdit(SynEdit).BlockEnd.Y;
  if TSynEdit(SynEdit).BlockEnd.X = 1 then dec(y2);
  for i := y1-1 to y2-1 do begin
    j := FoldView.FoldProvider.FoldOpenCount(i);
    while j > 0 do begin
      dec(j);
      FldInf := FoldView.FoldProvider.FoldOpenInfo(i, j);
      if (TPascalCodeFoldBlockType(PtrUInt(FldInf.FoldType)) in
          [cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment]) and
         (sfaFoldFold in FldInf.FoldAction)
      then begin
        FoldView.FoldAtTextIndex(i, j, 1, False, 1);
      end;
    end;
  end;
end;

procedure TIDESynGutterCodeFolding.PopClickedHideComment(Sender: TObject);
var
  i, j, y1, y2: Integer;
  FldInf: TSynFoldNodeInfo;
begin
  if not TSynEdit(SynEdit).SelAvail then exit;
  y1 := TSynEdit(SynEdit).BlockBegin.Y;
  y2 := TSynEdit(SynEdit).BlockEnd.Y;
  if TSynEdit(SynEdit).BlockEnd.X = 1 then dec(y2);
  for i := y1-1 to y2-1 do begin
    j := FoldView.FoldProvider.FoldOpenCount(i);
    while j > 0 do begin
      dec(j);
      FldInf := FoldView.FoldProvider.FoldOpenInfo(i, j);
      if (TPascalCodeFoldBlockType(PtrUInt(FldInf.FoldType)) in
          [cfbtAnsiComment, cfbtBorCommand, cfbtSlashComment]) and
         (sfaFoldHide in FldInf.FoldAction)
      then begin
        FoldView.FoldAtTextIndex(i, j, 1, False, 0);
      end;
    end;
  end;
end;

procedure TIDESynGutterCodeFolding.CreatePopUpMenuEntries(APopUp: TPopupMenu; ALine: Integer);
var
  i, j, y1, y2: Integer;
  HasFolds, HasHideableComments, HasFoldableComments, HasCollapsedComments: Boolean;
  ft: TPascalCodeFoldBlockType;
  Foldable, HideAble: TPascalCodeFoldBlockTypes;
  lc: TSynEditFoldLineCapabilities;

  procedure CheckFoldConf(Val: TPascalCodeFoldBlockType);
  begin
    if not TSynPasSyn(FoldView.HighLighter).FoldConfig[ord(Val)].Enabled then
      exit;
    if fmFold in TSynPasSyn(FoldView.HighLighter).FoldConfig[ord(Val)].Modes then
      include(Foldable, Val);
    if fmHide in TSynPasSyn(FoldView.HighLighter).FoldConfig[ord(Val)].Modes then
      include(HideAble, Val);
  end;

  function AddPopUpItem(const ACaption: String): TMenuItem;
  begin
    Result := TMenuItem.Create(APopUp);
    Result.Caption := ACaption;
    APopUp.Items.Add(Result);
  end;


begin
  inherited CreatePopUpMenuEntries(APopUp, ALine);
  if not TSynEdit(SynEdit).SelAvail then exit;

  y1 := TSynEdit(SynEdit).BlockBegin.Y;
  y2 := TSynEdit(SynEdit).BlockEnd.Y;
  if TSynEdit(SynEdit).BlockEnd.X = 1 then dec(y2);

  HasFolds := FoldView.TextIndexToViewPos(y2) - FoldView.TextIndexToViewPos(y1) <> y2 - y1;
  //debugln(['*** HasFolds=', HasFolds, ' y1=',y1, ' y2=',y2, ' VP1=',FoldView.TextIndexToViewPos(y1), ' VP2=',FoldView.TextIndexToViewPos(y2)]);

  HasHideableComments := False;
  HasFoldableComments := False;
  HasCollapsedComments := False;
  if FoldView.HighLighter is TSynPasSyn then begin
    Foldable := [];
    HideAble := [];
    CheckFoldConf(cfbtAnsiComment);
    CheckFoldConf(cfbtBorCommand);
    CheckFoldConf(cfbtSlashComment);
    if (Foldable <> []) or (HideAble <> []) then begin
      i := y1-1;
      while i < y2 do begin
        lc := FoldView.FoldProvider.LineCapabilities[i];
        j := FoldView.FoldProvider.FoldOpenCount(i);
        while j > 0 do begin
          dec(j);
          ft := TPascalCodeFoldBlockType(PtrUInt(FoldView.FoldProvider.FoldOpenInfo(i, j).FoldType));
          if ((ft in Foldable) or (ft in HideAble)) and FoldView.IsFoldedAtTextIndex(i,j) then
            HasCollapsedComments := True
          else begin
            if (ft in Foldable) and (cfFoldStart in lc) then
              HasFoldableComments := True;
            if (ft in HideAble) and (cfHideStart in lc) then
              HasHideableComments := True;
          end;
        end;
        if HasFoldableComments and HasHideableComments and
           (HasCollapsedComments or not HasFolds)
        then
          break;
        inc(i);
      end;
    end;
  end;

  if (HasFolds or HasCollapsedComments or HasFoldableComments or HasHideableComments) and
     (APopUp.Items.Count > 0)
  then
    AddPopUpItem(cLineCaption);

  If HasFolds then
    AddPopUpItem(synfUnfoldAllInSelection).OnClick := {$IFDEF FPC}@{$ENDIF}PopClickedUnfoldAll;
  If HasCollapsedComments then
    AddPopUpItem(synfUnfoldCommentsInSelection).OnClick := {$IFDEF FPC}@{$ENDIF}PopClickedUnfoldComment;
  If HasFoldableComments then
    AddPopUpItem(synfFoldCommentsInSelection).OnClick := {$IFDEF FPC}@{$ENDIF}PopClickedFoldComment;
  If HasHideableComments then
    AddPopUpItem(synfHideCommentsInSelection).OnClick := {$IFDEF FPC}@{$ENDIF}PopClickedHideComment;
end;

end.

