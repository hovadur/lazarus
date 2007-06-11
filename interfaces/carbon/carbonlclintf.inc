{%MainUnit carbonint.pas}

{ $Id$ }
{******************************************************************************
  All Carbon interface communication implementations.
  This are the implementation of the overrides of the Carbon Interface for the
  methods defined in the
  lcl/include/lclintf.inc


  !! Keep alphabetical !!

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

{------------------------------------------------------------------------------
  Method:  CreateStandardCursor
  Params:  ACursor - Cursor type
  Returns: Cursor object in Carbon for the specified cursor type
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.CreateStandardCursor(ACursor: SmallInt): hCursor;
var
  AThemeCursor: ThemeCursor;
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.CreateStandardCursor ACursor: ' + DbgS(ACursor));
  {$ENDIF}
  
  Result := 0;
  if (ACursor >= crLow) and (ACursor <= crHigh) then
  begin
    AThemeCursor := CursorToThemeCursor[TCursor(ACursor)];
    if AThemeCursor <> kThemeUndefCursor then
      Result := hCursor(TCarbonCursor.CreateThemed(AThemeCursor));
  end;
  
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.CreateStandardCursor Result: ' + DbgS(Result));
  {$ENDIF}
end;

function TCarbonWidgetSet.ExtUTF8Out(DC: HDC; X, Y: Integer; Options: Longint;
  Rect: PRect; Str: PChar; Count: Longint; Dx: PInteger): Boolean;
begin
  Result:=inherited ExtUTF8Out(DC, X, Y, Options, Rect, Str, Count, Dx);
end;

function TCarbonWidgetSet.FontCanUTF8(Font: HFont): boolean;
begin
  Result:=inherited FontCanUTF8(Font);
end;

function TCarbonWidgetSet.GetAcceleratorString(const AVKey: Byte;
  const AShiftState: TShiftState): String;
begin
  Result:=inherited GetAcceleratorString(AVKey, AShiftState);
end;

function TCarbonWidgetSet.GetControlConstraints(Constraints: TObject): boolean;
begin
  Result:=inherited GetControlConstraints(Constraints);
end;

{------------------------------------------------------------------------------
  Method:  GetLCLOwnerObject
  Params:  Handle - Handle of window
  Returns: LCL control which has the specified widget
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.GetLCLOwnerObject(Handle: HWnd): TObject;
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.GetLCLOwnerObject Handle: ' + DbgS(Handle));
  {$ENDIF}
  
  Result := nil;
  if not CheckWidget(Handle, 'GetLCLOwnerObject') then Exit;
  
  Result := TCarbonWidget(Handle).LCLObject;
  
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.GetLCLOwnerObject Result: ' + DbgS(Result));
  {$ENDIF}
end;

function TCarbonWidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer
  ): integer;
begin
  Result:=inherited GetListBoxIndexAtY(ListBox, y);
end;

function TCarbonWidgetSet.GetListBoxItemRect(ListBox: TComponent;
  Index: integer; var ARect: TRect): boolean;
begin
  Result:=inherited GetListBoxItemRect(ListBox, Index, ARect);
end;

{------------------------------------------------------------------------------
  Method:  IntfSendsUTF8KeyPress
  Returns: If the interface sends UTF-8 key press events
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.IntfSendsUTF8KeyPress: boolean;
begin
  Result := True;
end;

{------------------------------------------------------------------------------
  Method:  PromptUser
  Params:  DialogCaption - Dialog caption
           DialogMessage - Dialog message text
           DialogType    - Type of dialog
           Buttons       - Pointer to button types
           ButtonCount   - Count of passed buttons
           DefaultIndex  - Index of default button
           EscapeResult  - Result value of escape
  Returns: The result value of pushed button
  
  Shows modal dialog with the specified caption, message and buttons and prompts
  user to push one.
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.PromptUser(const DialogCaption : string;
                                     const DialogMessage : string;
                                           DialogType    : LongInt;
                                           Buttons       : PLongInt;
                                           ButtonCount   : LongInt;
                                           DefaultIndex  : LongInt;
                                           EscapeResult  : LongInt) : LongInt;
 {Implements MessageDlg.
  Carbon's standard alert box only supports 3 buttons (plus optional
  help button).
  Note that alert's help button is not supported at this time since no help context
  is passed to this method.}
  
  // returns first found button ID from passed buttons in dialog buttons
  function FindButton(AButtons: Array of LongInt): LongInt;
  var
    I, J: Integer;
  begin
    for I := Low(AButtons) to High(AButtons) do
      for J := 0 to ButtonCount -1 do
      begin
        if AButtons[I] = Buttons[J] then
        begin
          Result := AButtons[I];
          Exit;
        end;
      end;
  end;
  
const
  ButtonCaption: Array [idButtonOk..idButtonNoToAll] of String =
    ('OK', 'Cancel', ''{Help}, 'Yes', 'No', 'Close', 'Abort', 'Retry', 'Ignore',
     'All', 'Yes To All', 'No To All');
{ Note: Not using Pointer(kAlertDefaultOKText) or Pointer(kAlertDefaultCancelText)
  since this just passes in -1, which tells button to use its normal text and
  we need to override with Yes and No. If Localizable.strings file is in app 
  bundle's .lproj folder, will use localized strings for above keys if they
  are defined in .strings file.}
var
  ParamRec    : AlertStdCFStringAlertParamRec;
  CFString    : CFStringRef;
  ButtonID    : Integer;
  RightBtnID  : LongInt;
  MiddleBtnID : LongInt;
  LeftBtnID   : LongInt;
  CaptionStr  : CFStringRef;
  MessageStr  : CFStringRef;
  AlertCode   : AlertType;
  AlertRef    : DialogRef;
  AlertBtnIdx : DialogItemIndex;
  I: Integer;

const SName = 'PromptUser';
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.PromptUser DialogCaption: ' + DialogCaption +
      ' DialogMessage: ' + DialogMessage + ' DialogType: ' + DbgS(DialogType) +
      ' ButtonCount: ' + DbgS(ButtonCount) + ' DefaultIndex: ' +
      DbgS(DefaultIndex) + ' EscapeResult: ' + DbgS(EscapeResult));
  {$ENDIF}
  
  Result := -1;
  
  if (ButtonCount > 4) or ((ButtonCount = 4) and
    not (FindButton([idButtonHelp]) > 0)) then
  begin
    // if the button count is bigger than 3 + help button we can not use
    // native alert
    
    {$IFDEF VerboseLCLIntf}
      DebugLn('TCarbonWidgetSet.PromptUser Use LCL standard one.');
    {$ENDIF}
    
    Result := inherited;

    {$IFDEF VerboseLCLIntf}
      DebugLn('TCarbonWidgetSet.PromptUser LCL Result: ' + DbgS(Result));
    {$ENDIF}
    
    Exit;
  end;

  {Initialize record}
  ParamRec.version := kStdCFStringAlertVersionOne;
  ParamRec.movable := True;
  ParamRec.helpButton := False;
  ParamRec.defaultText := nil;
  ParamRec.cancelText := nil;
  ParamRec.otherText := nil;
  ParamRec.defaultButton := kAlertStdAlertOKButton;  {Right button}
  ParamRec.cancelButton := kAlertStdAlertCancelButton;
  ParamRec.position := kWindowDefaultPosition;
  ParamRec.flags := 0; 

  {English defaults to use if no Localizable.strings translations to use}
  {Convert LCL "id" button values to Carbon values}

  ButtonID := 0;
  for I := 0 to ButtonCount - 1 do
  begin
    if Buttons[I] = idButtonHelp then ParamRec.helpButton := True
    else
    begin
      if (Buttons[I] < Low(ButtonCaption)) or (Buttons[I] > High(ButtonCaption)) then
      begin
        DebugLn('TCarbonWidgetSet.PromptUser Invalid button ID: ' + DbgS(Buttons[I]));
        Continue;
      end;
      
      CreateCFString(ButtonCaption[Buttons[I]], CFString);
      try
        case ButtonID of
          0: // set right button caption and result
            begin
              ParamRec.defaultText := CFCopyLocalizedString(CFString, nil);
              RightBtnID := Buttons[I];
            end;
          1: // set middle button caption and result
            begin
              ParamRec.cancelText := CFCopyLocalizedString(CFString, nil);
              MiddleBtnID := Buttons[I];
            end;
          2: // set left button caption and result
            begin
              ParamRec.otherText := CFCopyLocalizedString(CFString, nil);
              LeftBtnID := Buttons[I];
              
              // set cancel to left button if exists
              ParamRec.cancelButton := kAlertStdAlertOtherButton;
            end;
        end;
      finally
        FreeCFString(CFString);
      end;
      
      Inc(ButtonID);
    end;
  end;

  CreateCFString(DialogCaption, CaptionStr);
  CreateCFString(DialogMessage, MessageStr);

  {Note: kAlertCautionAlert displays alert icon and app's icon.
         kAlertStopAlert and kAlertNoteAlert only display app's icon.
         kAlertPlainAlert doesn't display any icon.}
  case DialogType of
    idDialogWarning : AlertCode := kAlertCautionAlert;
    idDialogError   : AlertCode := kAlertCautionAlert;
    idDialogInfo    : AlertCode := kAlertNoteAlert;
    idDialogConfirm : AlertCode := kAlertNoteAlert;
  else
    AlertCode := kAlertNoteAlert;
  end;

  try
    if OSError(CreateStandardAlert(AlertCode, CaptionStr, MessageStr, @ParamRec, AlertRef),
      Self, SName, 'CreateStandardAlert') then Exit;
      
    if OSError(RunStandardAlert(AlertRef, nil, AlertBtnIdx), Self, SName,
      'RunStandardAlert') then Exit;

    {Convert Carbon result to LCL "id" dialog result}
    case AlertBtnIdx of
      kAlertStdAlertOKButton     : Result := RightBtnID;
      kAlertStdAlertCancelButton : Result := MiddleBtnID;
      kAlertStdAlertOtherButton  : Result := LeftBtnID;
    end;

  finally
    FreeCFString(ParamRec.defaultText);
    FreeCFString(ParamRec.cancelText);
    FreeCFString(ParamRec.otherText);
    FreeCFString(CaptionStr);
    FreeCFString(MessageStr);
  end;
  
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.PromptUser Result: ' + DbgS(Result));
  {$ENDIF}
end;  {TCarbonWidgetSet.PromptUser}

{------------------------------------------------------------------------------
  Function: RawImage_CreateBitmaps
  Params: ARawImage: Source raw image
          ABitmap:   Destination bitmap object
          AMask:     Destination mask object
          ASkipMask: When set, no mask is created
  Returns:

 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_CreateBitmaps(const ARawImage: TRawImage; out ABitmap, AMask: HBitmap; ASkipMask: Boolean): Boolean;
var
  ADesc: TRawImageDescription absolute ARawImage.Description;
begin
  Result := False;

  ABitmap := HBITMAP(TCarbonBitmap.Create(ADesc.Width, ADesc.Height, ADesc.Depth, ARawImage.Data));

  if ASkipMask
  then AMaskBitmap := 0
  else AMaskBitmap := HBITMAP(TCarbonBitmap.Create(ADesc.Width, ADesc.Height, 1, ARawImage.Mask))

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromBitmap
  Params: ABitmap:
          ADesc:
  Returns:

  Describes the inner format utilized by Carbon and specific information
  for the specified bitmap
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_DescriptionFromBitmap(ABitmap: HBITMAP; out ADesc: TRawImageDescription): Boolean;
var
  ACBitmap: TCarbonBitmap absolute ABitmap;
  Prec, Shift, BPR: Byte;
  AlphaInfo: CGImageAlphaInfo;
begin
  Result := False;

  if not CheckBitmap(Bitmap, 'RawImage_DescriptionFromBitmap') then Exit;

  FillChar(ADesc, SizeOf(Desc), 0);

  ADesc.Format := ricfRGBA;
  ADesc.Width := ACBitmap.Width;
  ADesc.Height := ACBitmap.Height;

  //ADesc.PaletteColorCount := 0;

  ADesc.BitOrder := riboReversedBits;
  ADesc.ByteOrder := riboMSBFirst;

  BPR := CGImageGetBytesPerRow(ACBitmap.CGImage);
  if BPR and $F = 0 then ADesc.LineEnd := rileDQWordBoundary     // 128bit aligned
  else if BPR and $7 = 0 then ADesc.LineEnd := rileQWordBoundary //  64bit aligned
  else if BPR and $3 = 0 then ADesc.LineEnd := rileWordBoundary  //  32bit aligned
  else if BPR and $1 = 0 then ADesc.LineEnd := rileByteBoundary  //   8bit aligned
  else ADesc.LineEnd := rileTight;

  ADesc.LineOrder := riloTopToBottom;
  ADesc.BitsPerPixel := CGImageGetBitsPerPixel(ACBitmap.CGImage);

  ADesc.MaskBitOrder := riboReversedBits;
  ADesc.MaskBitsPerPixel := 1;
  ADesc.MaskLineEnd := rileDQWordBoundary; // 128bit aligned ??? is this needed for mone bitmaps ??;
  // ADesc.MaskShift := 0;

  Prec := CGImageGetBitsPerComponent(ACBitmap.CGImage);
  AlphaInfo := CGImageGetAlphaInfo(ACBitmap.CGImage);
  
  if AlphaInfo <> kCGImageAlphaOnly
  then begin
    ADesc.RedPrec := Prec;
    ADesc.GreenPrec := Prec;
    ADesc.BluePrec := Prec;
  end;

  // alpha
  case AlphaInfo of
    kCGImageAlphaNone,
    kCGImageAlphaNoneSkipLast,
    kCGImageAlphaNoneSkipFirst: begin
      ADesc.Depth := Prec * 3;
      // ADesc.AlphaPrec := 0;
    end;
  else
    ADesc.Depth := Prec * 4;
    ADesc.AlphaPrec := Prec;
  end;

  case AlphaInfo of
    kCGImageAlphaNone,
    kCGImageAlphaNoneSkipLast: begin
      // RGBx
      Shift := 32 - Prec;
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
    end;
    kCGImageAlphaNoneSkipFirst: begin
      // xRGB
      Shift := 0;
      ADesc.BlueShift := Shift;
      Inc(Shift, Prec);
      ADesc.GreenShift := Shift;
      Inc(Shift, Prec);
      ADesc.RedShift := Shift;
    end;
    kCGImageAlphaPremultipliedFirst,
    kCGImageAlphaFirst: begin
      // ARGB
      Shift := 32 - Prec;
      ADesc.AlphaShift := Shift;
      Dec(Shift, Prec);
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
    end;
    kCGImageAlphaPremultipliedLast,
    kCGImageAlphaLast: begin
      // RGBA
      Shift := 32 - Prec;
      ADesc.RedShift := Shift;
      Dec(Shift, Prec);
      ADesc.GreenShift := Shift;
      Dec(Shift, Prec);
      ADesc.BlueShift := Shift;
      Dec(Shift, Prec);
      ADesc.AlphaShift := Shift;
    end;
    kCGImageAlphaOnly: begin
      // A
      //ADesc.AlphaShift := 0;
    end;
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: RawImage_DescriptionFromDevice
  Params: ADC:   - Handle to device context
          ADesc: - Pointer to raw image description
  Returns: True if success

  Retrieves the standard image format utilized by Carbon
 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_DescriptionFromDevice(ADC: HDC; out ADesc: TRawImageDescription): Boolean;
var
  P: TPoint;
begin
  Result := False;

  FillStandardDescription(ADesc);
  if CheckDC(ADC, 'GetDeviceSize')
  then begin
    P := TCarbonDeviceContext(DC).Size;
    ADesc.Width := P.X;
    ADesc.Height := P.Y
  end;

  Result := True;
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromBitmap
  Params: ABitmap:
          AMask:
          ARect:
          ARawImage:
  Returns:

 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_FromBitmap(ABitmap, AMask: HBITMAP; const ARect: TRect; var ARawImage: TRawImage): Boolean;
begin
  // Your code here
end;

{------------------------------------------------------------------------------
  Function: RawImage_FromDevice
  Params: ADC:
          ARect:
          ARawImage:
  Returns:

 ------------------------------------------------------------------------------}
function TCarbonWidgetSet.RawImage_FromDevice(ADC: HDC; const ARect: TRect; var ARawImage: TRawImage): Boolean;
begin
  // Your code here
end;

{------------------------------------------------------------------------------
  Method:  SetMainMenuEnabled
  Params:  AEnabled

  Enables/disables main menu
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SetMainMenuEnabled(AEnabled: Boolean);
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.SetMainMenuEnabled AEnabled: ' + DbgS(AEnabled));
  {$ENDIF}
  
  if FMainMenu <> nil then
  begin
    if FMainMenu.Items.HandleAllocated then
    begin
      TCarbonMenu(FMainMenu.Items.Handle).SetEnable(AEnabled);
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method:  SetCaptureWidget
  Params:  AWidget - Carbon widget to capture

  Sets captured Carbon widget
 ------------------------------------------------------------------------------}
procedure TCarbonWidgetSet.SetCaptureWidget(const AWidget: HWND);
begin
  {$IFDEF VerboseLCLIntf}
    DebugLn('TCarbonWidgetSet.SetCaptureWidget AWidget: ' + DbgS(AWidget));
  {$ENDIF}
  
  if AWidget <> FCaptureWidget then
  begin
    FCaptureWidget := AWidget;
    
    if FCaptureWidget <> 0 then
      LCLSendCaptureChangedMsg(TCarbonWidget(FCaptureWidget).LCLObject);
  end;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

// included by carbonint.pas
