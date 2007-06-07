{  $Id$  }
{
 /***************************************************************************
                                graphtype.pp
                                ------------
                    Graphic related platform independent types
                    and utility functions.
                    Initial Revision  : Sat Feb 02 0:02:58 2002

 ***************************************************************************/

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
unit GraphType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLProc;

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

type
  TGraphicsColor = -$7FFFFFFF-1..$7FFFFFFF;
  TGraphicsFillStyle = (
    fsSurface, // fill till the color (it fills all execpt this color)
    fsBorder   // fill this color (it fills only conneted pixels of this color)
    );
  TGraphicsBevelCut = (bvNone, bvLowered, bvRaised, bvSpace);

//------------------------------------------------------------------------------
// raw image data
type
  { Colorformat: Higher values means higher intensity.
    For example: Red=0 means no red, Alpha=0 means transparent }
  TRawImageColorFormat = (
    ricfRGBA,   // one pixel contains red, green, blue and alpha
                // If AlphaPrec=0 then there is no alpha.
                // Same for RedPrec, GreenPrec and BluePrec.
    ricfGray    // R=G=B. The Red stores the Gray. AlphaPrec can be >0.
    );

  TRawImageByteOrder = (
    riboLSBFirst, // least significant byte first
    riboMSBFirst  // most significant byte first
    );
    
  TRawImageBitOrder = (
    riboBitsInOrder, // Bit 0 is pixel 0
    riboReversedBits // Bit 0 is pixel 7 (Bit 1 is pixel 6, ...)
    );

  TRawImageLineEnd = (
    rileTight,         // no gap at end of lines
    rileByteBoundary,  // each line starts at byte boundary. For example:
                       // If BitsPerPixel=3 and Width=1, each line has a gap
                       // of 5 unused bits at the end.
    rileWordBoundary,  // each line starts at word (16bit) boundary
    rileDWordBoundary, // each line starts at double word (32bit) boundary
    rileQWordBoundary, // each line starts at quad word (64bit) boundary
    rileDQWordBoundary // each line starts at double quad word (128bit) boundary
    );

  TRawImageLineOrder = (
    riloTopToBottom, // The line 0 is the top line
    riloBottomToTop  // The line 0 is the bottom line
    );

  TRawImageDescription = record
    Format: TRawImageColorFormat;
    Width: cardinal;
    Height: cardinal;
    Depth: Byte; // used bits per pixel
    BitOrder: TRawImageBitOrder;
    ByteOrder: TRawImageByteOrder;
    LineOrder: TRawImageLineOrder;
    LineEnd: TRawImageLineEnd;
    BitsPerPixel: Byte; // bits per pixel. can be greater than Depth.
    RedPrec: Byte; // red precision. bits for red
    RedShift: Byte;
    GreenPrec: Byte;
    GreenShift: Byte; // bitshift. Direction: from least to most significant
    BluePrec: Byte;
    BlueShift: Byte;
    AlphaPrec: Byte;
    AlphaShift: Byte;

    // The next values are only valid, if there is a mask (MaskBitsPerPixel > 0)
    // Masks are always separate with a depth of 1 bpp. One pixel can occupy
    // one byte at most
    // a value of 1 means that pixel is masked
    // a value of 0 means the pixel value is shown
    MaskBitsPerPixel: Byte; // bits per mask pixel, usually 1, 0 when no mask
    MaskShift: Byte;        // the shift (=position) of the mask bit
    MaskLineEnd: TRawImageLineEnd;
    MaskBitOrder: TRawImageBitOrder;

    // The next values are only valid, if there is a palette (PaletteColorCount > 0)
    PaletteColorCount: Word;   // entries in color palette. 0 when no palette.
    PaletteBitsPerIndex: Byte; // bits per palette index, this can be larger than the colors used
    PaletteShift: Byte;        // bitshift. Direction: from least to most significant
    PaletteLineEnd: TRawImageLineEnd;
    PaletteBitOrder: TRawImageBitOrder;
    PaletteByteOrder: TRawImageByteOrder;
  end;
  PRawImageDescription = ^TRawImageDescription;
  
  // Note: not all devices/images have all parts at any time. But if a part can
  // be applied to the device/image, the 'Description' describes its structure.
  TRawImage = record
    Description: TRawImageDescription;
    Data: PByte;
    DataSize: PtrUInt;
    Mask: PByte;
    MaskSize: PtrUInt;
    Palette: PByte;
    PaletteSize: PtrUInt;
  end;
  PRawImage = ^TRawImage;

  TRawImagePosition = record
    Byte: PtrUInt;
    Bit: cardinal;
  end;
  PRawImagePosition = ^TRawImagePosition;

const
  RawImageColorFormatNames: array[TRawImageColorFormat] of string = (
    'ricfRGBA',
    'ricfGray'
    );

  RawImageByteOrderNames: array[TRawImageByteOrder] of string = (
    'riboLSBFirst',
    'riboMSBFirst'
    );

  RawImageBitOrderNames: array[TRawImageBitOrder] of string = (
    'riboBitsInOrder',
    'riboReversedBits'
    );

  RawImageLineEndNames: array[TRawImageLineEnd] of string = (
    'rileTight',
    'rileByteBoundary',
    'rileWordBoundary',
    'rileDWordBoundary',
    'rileQWordBoundary',
    'rileDQWordBoundary'
    );

  RawImageLineOrderNames: array[TRawImageLineOrder] of string = (
    'riloTopToBottom',
    'riloBottomToTop'
    );
    
  DefaultByteOrder = {$IFDEF Endian_Little}riboLSBFirst{$ELSE}riboMSBFirst{$ENDIF};


function  RawImage_IsMasked(const ARawImage: TRawImage; ATestPixels: Boolean): Boolean;
function  RawImage_IsTransparent(const ARawImage: TRawImage; ATestPixels: Boolean): Boolean;

procedure RawImage_CreateData(var ARawImage: TRawImage; AZeroMem: Boolean);
procedure RawImage_CreateLineStarts(AWidth, AHeight: cardinal; ABitsPerPixel: Byte;
                                   ALineEnd: TRawImageLineEnd;
                                   var ALineStarts: PRawImagePosition);

procedure RawImage_FreeData(var ARawImage: TRawImage);
procedure RawImage_ReleaseData(var ARawImage: TRawImage);
procedure RawImage_ExtractRect(const ASrc: TRawImage; const ARect: TRect; out ADst: TRawImage);


function  RawImageDescription_AsString(const ADesc: TRawImageDescription): string;

procedure RawImage_GetXYPosition(ALineOrder: TRawImageLineOrder; ABitsPerPixel: Byte;
                                 AHeight: Cardinal; ALineStarts: PRawImagePosition;
                                 x, y: cardinal; var APosition: TRawImagePosition);

procedure RawImage_ReadBits(AData: PByte; const APosition: TRawImagePosition;
                       ABitsPerPixel, APrec, AShift: Byte;
                       ABitOrder: TRawImageBitOrder; out ABits: Word);
procedure RawImage_WriteBits(AData: PByte; const APosition: TRawImagePosition;
                       ABitsPerPixel, APrec, AShift: Byte;
                       ABitOrder: TRawImageBitOrder; ABits: Word);

function GetBytesPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd): PtrUInt;
function GetBitsPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd): PtrUInt;

// some default rawimage descriptions

function GetDescriptionFromMask(const ASrc: TRawImageDescription): TRawImageDescription;
function GetDescriptionFromAlpha(const ASrc: TRawImageDescription): TRawImageDescription;
function GetDescription_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;
function GetDescription_BPP24_B8G8R8_M1_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;
function GetDescription_BPP32_B8G8R8_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;
function GetDescription_BPP32_B8G8R8_M1_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;
function GetDescription_BPP32_A8B8G8R8_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;


// TODO: remove
{$ifdef OldRawImageProcs}
function RawImageMaskIsEmpty(RawImage: PRawImage; TestPixels: boolean): boolean;
function RawImageDescriptionAsString(Desc: PRawImageDescription): string;
procedure FreeRawImageData(RawImage: PRawImage);
procedure ReleaseRawImageData(RawImage: PRawImage);

procedure CreateRawImageData(Width, Height, BitsPerPixel: cardinal;
                             LineEnd: TRawImageLineEnd;
                             var Data: Pointer; var DataSize: PtrUInt);
procedure CreateRawImageLineStarts(Width, Height, BitsPerPixel: cardinal;
                                   LineEnd: TRawImageLineEnd;
                                   var LineStarts: PRawImagePosition);
procedure CreateRawImageDescFromMask(SrcRawImageDesc,
                                     DestRawImageDesc: PRawImageDescription);
procedure GetRawImageXYPosition(RawImageDesc: PRawImageDescription;
                                LineStarts: PRawImagePosition; x, y: cardinal;
                                var Position: TRawImagePosition);
procedure ExtractRawImageRect(SrcRawImage: PRawImage; const SrcRect: TRect;
                              DestRawImage: PRawImage);


procedure ReadRawImageBits(TheData: PByte; const Position: TRawImagePosition;
                       BitsPerPixel, Prec, Shift: cardinal;
                       BitOrder: TRawImageBitOrder; var Bits: word);
procedure WriteRawImageBits(TheData: PByte; const Position: TRawImagePosition;
                       BitsPerPixel, Prec, Shift: cardinal;
                       BitOrder: TRawImageBitOrder; Bits: word);

procedure ReAlignRawImageLines(var Data: Pointer; var Size: PtrUInt;
  Width, Height, BitsPerPixel: cardinal;
  var OldLineEnd: TRawImageLineEnd; NewLineEnd: TRawImageLineEnd);

{$endif}

var
  MissingBits: array[0..15] of array[0..7] of word;

implementation

uses Math;


{------------------------------------------------------------------------------
  Function: IntersectRect
  Params:  var DestRect: TRect; const SrcRect1, SrcRect2: TRect
  Returns: Boolean

  Intersects SrcRect1 and SrcRect2 into DestRect.
  Intersecting means that DestRect will be the overlapping area of SrcRect1 and
  SrcRect2. If SrcRect1 and SrcRect2 do not overlapp the Result is false, else
  true.
 ------------------------------------------------------------------------------}
function IntersectRect(var DestRect: TRect;
  const SrcRect1, SrcRect2: TRect): Boolean;
begin
  Result := False;

  // test if rectangles intersects
  Result:=(SrcRect2.Left < SrcRect1.Right)
      and (SrcRect2.Right > SrcRect1.Left)
      and (SrcRect2.Top < SrcRect1.Bottom)
      and (SrcRect2.Bottom > SrcRect1.Top);

  if Result then begin
    DestRect.Left:=Max(SrcRect1.Left,SrcRect2.Left);
    DestRect.Top:=Max(SrcRect1.Top,SrcRect2.Top);
    DestRect.Right:=Min(SrcRect1.Right,SrcRect2.Right);
    DestRect.Bottom:=Min(SrcRect1.Bottom,SrcRect2.Bottom);
  end else begin
    FillChar(DestRect,SizeOf(DestRect),0);
  end;
end;

{$ifdef OldRawImageProcs}
function RawImageMaskIsEmpty(RawImage: PRawImage; TestPixels: boolean): boolean;
begin
  Result := not RawImage_IsMasked(RawImage^, TestPixels);
end;
{$endif}

function RawImage_IsMasked(const ARawImage: TRawImage; ATestPixels: Boolean): Boolean;

  function CheckMask: boolean;
  var
    Width: cardinal;
    Height: cardinal;
    UsedBitsPerLine: cardinal;
    TotalBits: Cardinal;
    TotalBitsPerLine: cardinal;
    TotalBytesPerLine: cardinal;
    UnusedBitsAtEnd: Byte;
    UnusedBytesAtEnd: Byte;
    P: PCardinal;
    LinePtr: PByte;
    x, y, xEnd: Integer;
    EndMask: Cardinal; // No mask bits should be set. The Cardinal at line end
                       // can contain some unused bits. This mask AND cardinal
                       // at line end makes the unsused bits all 0.
    UsedBytesPerLine: integer;
    
    procedure CreateEndMask;
    begin
      if ARawImage.Description.MaskBitOrder = riboBitsInOrder
      then EndMask := ($FF shr UnusedBitsAtEnd)
      else EndMask := ($FF shl UnusedBitsAtEnd) and $FF;
      // add unused bytes
      {$ifdef endian_big}
      // read in memory -> [??][eM][uu][uu]
      EndMask := ($FFFFFF00 or EndMask) shl (UnusedBytesAtEnd shl 3);
      {$else}
      // read in memory -> [uu][uu][eM][??]
      EndMask := ((EndMask shl 24) or $00FFFFFF) shr (UnusedBytesAtEnd shl 3);
      {$endif}
    end;
    
    // separate dump procs to avoid code flow cluttering
    // added here in case somone want to debug
    {$IFDEF VerboseRawImage}
    procedure DumpFull;
    begin
      DebugLn('RawImageMaskIsEmpty FullByte y=',dbgs(y),' x=',dbgs(x),' Byte=',DbgS(p^));
    end;

    procedure DumpEdge;
    begin
      DebugLn('RawImageMaskIsEmpty EdgeByte y=',dbgs(y),' x=',dbgs(x),
        ' Byte=',HexStr(Cardinal(p^),2),
        ' UnusedMask=',HexStr(Cardinal(UnusedMask),2),
        ' OR='+dbgs(p^ or UnusedMask),
        ' UnusedBitsAtEnd='+dbgs(UnusedBitsAtEnd),
        ' UsedBitsPerLine='+dbgs(UsedBitsPerLine),
        ' Width='+dbgs(Width),
        ' ARawImage.Description.AlphaBitsPerPixel='+dbgs(ARawImage.Description.AlphaBitsPerPixel));
    end;
    {$endif}

  begin
    Result := True;
    
    Width := ARawImage.Description.Width;
    Height := ARawImage.Description.Height;

    TotalBitsPerLine := GetBitsPerLine(Width,
                          ARawImage.Description.MaskBitsPerPixel,
                          ARawImage.Description.MaskLineEnd);
    TotalBits := Height * TotalBitsPerLine;
    if ARawImage.MaskSize < PtrUInt((TotalBits + 7) shr 3)
    then raise Exception.Create('RawImage_IsMasked - Invalid MaskSize');

    UsedBitsPerLine := Width * ARawImage.Description.MaskBitsPerPixel;
    UnusedBitsAtEnd := TotalBitsPerLine - UsedBitsPerLine;

    if UnusedBitsAtEnd = 0
    then begin
      // the next line follows the previous one, so we can compare the whole
      // memblock in one go

      P := PCardinal(ARawImage.Mask);
      for x := 1 to TotalBits shr 5 do
      begin
        if p^ <> 0 then Exit;
        Inc(p);
      end;

      // redefine UnusedBitsAtEnd as the bits at the end of the block
      UnusedBitsAtEnd := TotalBits and $1F;
      if UnusedBitsAtEnd <> 0
      then begin
        // check last piece
        UnusedBytesAtEnd := UnusedBitsAtEnd shr 3;
        // adjust to byte bounds
        UnusedBitsAtEnd := UnusedBitsAtEnd and 7;
        CreateEndMask;

        if p^ and EndMask <> 0 then Exit;
      end;
    end
    else begin
      // scan each line
      TotalBytesPerLine := TotalBitsPerLine shr 3;
      UnusedBytesAtEnd := UnusedBitsAtEnd shr 3;

      // Number of cardinals to check
      xEnd := (TotalBytesPerLine - UnusedBytesAtEnd) shr 2;
      
      // Adjust unused to only the last checked
      UnusedBytesAtEnd := UnusedBytesAtEnd and 3;
      UnusedBitsAtEnd := UnusedBitsAtEnd and 7;

      // create mask for the last bits
      CreateEndMask;

      LinePtr := ARawImage.Mask;
      for y := 0 to Height - 1 do
      begin
        p := PCardinal(LinePtr);
        for x := 0 to xEnd - 1 do
        begin
          if p^ <> 0 then Exit;
          Inc(p);
        end;
        // check last end
        if (EndMask <> 0) and (p^ and EndMask <> 0) then Exit;

        Inc(LinePtr, TotalBytesPerLine);
      end;
    end;
    Result := False;
  end;
  
begin
  Result := False;
  //DebugLn('RawImageMaskIsEmpty Quicktest: empty ',dbgs(RawImage^.Description.Width),'x',dbgs(RawImage^.Description.Height));

  // quick test
  if (ARawImage.Mask = nil)
  or (ARawImage.MaskSize = 0)
  or (ARawImage.Description.MaskBitsPerPixel = 0)
  or (ARawImage.Description.Width = 0)
  or (ARawImage.Description.Height = 0)
  then begin
    {$IFDEF VerboseRawImage}
    DebugLn('RawImageMaskIsEmpty Quicktest: empty');
    {$ENDIF}
    exit;
  end;

  if ATestPixels
  then begin
    Result := CheckMask;

    {$IFDEF VerboseRawImage}
    DebugLn('RawImageMaskIsEmpty Empty=',dbgs(not Result));
    {$ENDIF}
  end
  else begin
    Result := True;
    {$IFDEF VerboseRawImage}
    DebugLn('RawImageMaskIsEmpty NoPixelTest: not empty');
    {$ENDIF}
    Exit;
  end;
end;

function RawImage_IsTransparent(const ARawImage: TRawImage; ATestPixels: Boolean): Boolean;
  function CheckAlpha: Boolean;
  begin
    {$note TODO: implement CheckAlpha}
    Result := True;
  end;
begin
  Result :=
    (ARawImage.Data <> nil) and
    (ARawImage.DataSize <> 0) and
    (ARawImage.Description.AlphaPrec <> 0) and
    (ARawImage.Description.Width = 0) and
    (ARawImage.Description.Height = 0);

  if Result and ATestPixels then
    Result := CheckAlpha;
end;

{$ifdef OldRawImageProcs}
function RawImageDescriptionAsString(Desc: PRawImageDescription): string;
begin
  Result := RawImageDescription_AsString(Desc^);
end;
{$endif}

function RawImageDescription_AsString(const ADesc: TRawImageDescription): string;

  function BoolStr(b: boolean): string;
  begin
    if b then
      Result:='true'
    else
      Result:='false';
  end;

begin
  with ADesc do
  begin
    Result:=
       ' Format='+RawImageColorFormatNames[Format]
      +' HasPalette->'+BoolStr(PaletteColorCount <> 0)
      +' HasMask->'+BoolStr(PaletteColorCount <> 0)
      +' Depth='+IntToStr(Depth)
      +' Width='+IntToStr(Width)
      +' Height='+IntToStr(Height)
      +' BitOrder='+RawImageBitOrderNames[BitOrder]
      +' ByteOrder='+RawImageByteOrderNames[ByteOrder]
      +' LineOrder='+RawImageLineOrderNames[LineOrder]
//      +' ColorCount='+IntToStr(ColorCount)
      +' LineEnd='+RawImageLineEndNames[LineEnd]
      +' BitsPerPixel='+IntToStr(BitsPerPixel)
      +' BytesPerLine->'+IntToStr(GetBytesPerLine(Width,BitsPerPixel,LineEnd))
      +' RedPrec='+IntToStr(RedPrec)
      +' RedShift='+IntToStr(RedShift)
      +' GreenPrec='+IntToStr(GreenPrec)
      +' GreenShift='+IntToStr(GreenShift)
      +' BluePrec='+IntToStr(BluePrec)
      +' BlueShift='+IntToStr(BlueShift)
      +' AlphaPrec='+IntToStr(AlphaPrec)
      +' AlphaShift='+IntToStr(AlphaShift)
      +' ~~~mask~~~'
      +' MaskBitsPerPixel='+IntToStr(MaskBitsPerPixel)
      +' MaskShift='+IntToStr(MaskShift)
      +' MaskLineEnd='+RawImageLineEndNames[MaskLineEnd]
      +' MaskBitOrder='+RawImageBitOrderNames[MaskBitOrder]
      +' MaskBytesPerLine->'+IntToStr(GetBytesPerLine(Width,MaskBitsPerPixel,MaskLineEnd))
      +' ~~~palette~~~'
      +' PaletteColorCount='+IntToStr(PaletteColorCount)
      +' PaletteBitsPerIndex='+IntToStr(PaletteBitsPerIndex)
      +' PaletteShift='+IntToStr(PaletteShift)
      +' PaletteLineEnd='+RawImageLineEndNames[PaletteLineEnd]
      +' PaletteBitOrder='+RawImageBitOrderNames[PaletteBitOrder]
      +' PaletteByteOrder='+RawImageByteOrderNames[PaletteByteOrder]
      +' PaletteBytesPerLine->'+IntToStr(GetBytesPerLine(Width,PaletteBitsPerIndex,PaletteLineEnd))
      +'';
  end;
end;

{$ifdef OldRawImageProcs}
procedure FreeRawImageData(RawImage: PRawImage);
begin
  RawImage_FreeData(RawImage^);
end;
{$endif}

procedure RawImage_FreeData(var ARawImage: TRawImage);
begin
  FreeMem(ARawImage.Data);
  ARawImage.Data := nil;
  ARawImage.DataSize:=0;

  FreeMem(ARawImage.Mask);
  ARawImage.Mask := nil;
  ARawImage.MaskSize := 0;
  
  FreeMem(ARawImage.Palette);
  ARawImage.Palette := nil;
  ARawImage.PaletteSize:=0;
end;

{$ifdef OldRawImageProcs}
procedure ReleaseRawImageData(RawImage: PRawImage);
begin
  RawImage_ReleaseData(RawImage^);
end;
{$endif}

procedure RawImage_ReleaseData(var ARawImage: TRawImage);
begin
  ARawImage.Data:=nil;
  ARawImage.DataSize:=0;
  ARawImage.Mask:=nil;
  ARawImage.MaskSize:=0;
  ARawImage.Palette:=nil;
  ARawImage.PaletteSize:=0;
end;

{-------------------------------------------------------------------------------
  Beware: Data is used in ReallocMem

-------------------------------------------------------------------------------}
{$ifdef OldRawImageProcs}
procedure CreateRawImageData(Width, Height, BitsPerPixel: cardinal;
  LineEnd: TRawImageLineEnd; var Data: Pointer; var DataSize: PtrUInt);
var
  PixelCount: PtrUInt;
  BitsPerLine: PtrUInt;
  DataBits: QWord;
begin
  // get current size
  PixelCount:=Width*Height;
  if PixelCount=0 then exit;

  // calculate BitsPerLine
  BitsPerLine:=GetBitsPerLine(Width,BitsPerPixel,LineEnd);

  // create pixels
  DataBits:=QWord(BitsPerLine)*Height;
  DataSize:=cardinal((DataBits+7) shr 3);
  ReAllocMem(Data,DataSize);
  FillChar(Data^,DataSize,0);
end;
{$endif}

procedure RawImage_CreateData(var ARawImage: TRawImage; AZeroMem: Boolean);
var
  Size: QWord;
begin
  // get current size
  if ARawImage.Description.Width = 0 then Exit;
  if ARawImage.Description.Height = 0 then Exit;

  // calculate size
  with ARawImage.Description do
    Size := GetBitsPerLine(Width, BitsPerPixel, LineEnd);
  Size := (Size * ARawImage.Description.Height) shr 3;
  
  if Size <= High(ARawImage.DataSize)
  then ARawImage.DataSize := Size
  else ARawImage.DataSize := High(ARawImage.DataSize);

  ReAllocMem(ARawImage.Data, ARawImage.DataSize);

  if AZeroMem
  then FillChar(ARawImage.Data^, ARawImage.DataSize, 0);
  
  // Setup mask if needed
  if ARawImage.Description.MaskBitsPerPixel = 0 then Exit;
  
  // calculate mask size
  with ARawImage.Description do
    Size := GetBitsPerLine(Width, MaskBitsPerPixel, MaskLineEnd);
  Size := (Size * ARawImage.Description.Height) shr 3;

  if Size <= High(ARawImage.MaskSize)
  then ARawImage.MaskSize := Size
  else ARawImage.MaskSize := High(ARawImage.MaskSize);

  ReAllocMem(ARawImage.Mask, ARawImage.MaskSize);

  if AZeroMem
  then FillChar(ARawImage.Mask^, ARawImage.MaskSize, 0);
end;


{$ifdef OldRawImageProcs}
procedure CreateRawImageDescFromMask(SrcRawImageDesc,
  DestRawImageDesc: PRawImageDescription);
begin
  // original code raises an exception, imo it is perfectly valid
  // to create a black image (MWE)
  if (SrcRawImageDesc^.MaskBitsPerPixel = 0) then
    RaiseGDBException('CreateRawImageFromMask Alpha not separate');
  RawImageDescription_CreateFromMask(SrcRawImageDesc^, DestRawImageDesc^);
end;
{$endif}

function GetDescription_BPP24_B8G8R8_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;
{ pf24bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=24 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
}
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 24bit depth format
  FillChar(Result, SizeOf(Result),0);
  with Result do
  begin
    Format:=ricfRGBA;
    Depth:=24; // used bits per pixel
    Width:=AWidth;
    Height:=AHeight;
    BitOrder:=riboBitsInOrder;
    ByteOrder:=DefaultByteOrder;
    LineOrder:=riloTopToBottom;
    BitsPerPixel:=24; // bits per pixel. can be greater than Depth.
    LineEnd:=rileDWordBoundary;
    RedPrec:=8; // red precision. bits for red
    RedShift:=16;
    GreenPrec:=8;
    GreenShift:=8; // bitshift. Direction: from least to most significant
    BluePrec:=8;
//    BlueShift:=0;
//    AlphaPrec:=0;
//    MaskBitsPerPixel:=0;
  end;
end;

function GetDescription_BPP24_B8G8R8_M1_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;
{ pf24bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=24 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 Masked
}
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 24bit depth format
  FillChar(Result, SizeOf(Result),0);
  with Result do
  begin
    Format:=ricfRGBA;
    Depth:=24; // used bits per pixel
    Width:=AWidth;
    Height:=AHeight;
    BitOrder:=riboBitsInOrder;
    ByteOrder:=DefaultByteOrder;
    LineOrder:=riloTopToBottom;
    BitsPerPixel:=24; // bits per pixel. can be greater than Depth.
    LineEnd:=rileDWordBoundary;
    RedPrec:=8; // red precision. bits for red
    RedShift:=16;
    GreenPrec:=8;
    GreenShift:=8; // bitshift. Direction: from least to most significant
    BluePrec:=8;
//    BlueShift:=0;
//    AlphaPrec:=0;
    MaskBitsPerPixel:=1;
    MaskBitOrder:=riboBitsInOrder;
//    MaskShift:=0;        // the shift (=position) of the mask bit
    MaskLineEnd:=rileDWordBoundary;
  end;
end;

function GetDescription_BPP32_B8G8R8_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;
{ pf32bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=32 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 No alpha
 No mask
}
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 32bit depth format
  FillChar(Result, SizeOf(Result),0);
  with Result do
  begin
    Format:=ricfRGBA;
    Depth:=24; // used bits per pixel
    Width:=AWidth;
    Height:=AHeight;
    BitOrder:=riboBitsInOrder;
    ByteOrder:=DefaultByteOrder;
    LineOrder:=riloTopToBottom;
    BitsPerPixel:=32; // bits per pixel. can be greater than Depth.
    LineEnd:=rileDWordBoundary;
    RedPrec:=8; // red precision. bits for red
    RedShift:=16;
    GreenPrec:=8;
    GreenShift:=8; // bitshift. Direction: from least to most signifikant
    BluePrec:=8;
//    BlueShift:=0;
//    AlphaPrec:=0;
//    MaskBitsPerPixel:=0;
  end;
end;

function GetDescription_BPP32_B8G8R8_M1_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;
{ pf32bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=32 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 no alpha
 with mask
}
begin
  // setup an artificial ScanLineImage with format RGB 24 bit, 32bit depth format
  FillChar(Result, SizeOf(Result),0);
  with Result do
  begin
    Format:=ricfRGBA;
    Depth:=24; // used bits per pixel
    Width:=AWidth;
    Height:=AHeight;
    BitOrder:=riboBitsInOrder;
    ByteOrder:=DefaultByteOrder;
    LineOrder:=riloTopToBottom;
    BitsPerPixel:=32; // bits per pixel. can be greater than Depth.
    LineEnd:=rileDWordBoundary;
    RedPrec:=8; // red precision. bits for red
    RedShift:=16;
    GreenPrec:=8;
    GreenShift:=8; // bitshift. Direction: from least to most signifikant
    BluePrec:=8;
//    BlueShift:=0;
//    AlphaPrec:=0;
    MaskBitsPerPixel:=1;
    MaskBitOrder:=riboBitsInOrder;
//    MaskShift:=0;        // the shift (=position) of the mask bit
    MaskLineEnd:=rileDWordBoundary;
  end;
end;

function GetDescription_BPP32_A8B8G8R8_BIO_TTB(AWidth, AHeight: integer): TRawImageDescription;
{ pf32bit:

 Format=ricfRGBA HasPalette=false Depth=24 PaletteColorCount=0
 BitOrder=riboBitsInOrder ByteOrder=DefaultByteOrder
 LineOrder=riloTopToBottom
 BitsPerPixel=32 LineEnd=rileDWordBoundary
 RedPrec=8 RedShift=16 GreenPrec=8 GreenShift=8 BluePrec=8 BlueShift=0
 alpha
 no mask
}
var
  ADesc: TRawImageDescription;
begin
  // setup an artificial ScanLineImage with format RGB 32 bit, 32bit depth format
  FillChar(Result, SizeOf(Result),0);
  with Result do
  begin
    Format:=ricfRGBA;
    Depth:=32; // used bits per pixel
    Width:=AWidth;
    Height:=AHeight;
    BitOrder:=riboBitsInOrder;
    ByteOrder:=DefaultByteOrder;
    LineOrder:=riloTopToBottom;
    BitsPerPixel:=32; // bits per pixel. can be greater than Depth.
    LineEnd:=rileDWordBoundary;
    RedPrec:=8; // red precision. bits for red
    RedShift:=16;
    GreenPrec:=8;
    GreenShift:=8; // bitshift. Direction: from least to most signifikant
    BluePrec:=8;
    BlueShift:=0;
    AlphaPrec:=8;
    AlphaShift:=24;
//    MaskBitsPerPixel:=0;
  end;
end;


function GetDescriptionFromMask(const ASrc: TRawImageDescription): TRawImageDescription;
begin
  FillByte(Result, SizeOf(Result), 0);
  
  with Result do
  begin
    Format       := ricfGray;
    Width        := ASrc.Width;
    Height       := ASrc.Height;
    Depth        := 1; // per def
    BitOrder     := ASrc.MaskBitOrder;
    ByteOrder    := DefaultByteOrder;
    LineOrder    := ASrc.LineOrder;
    LineEnd      := ASrc.MaskLineEnd;
    BitsPerPixel := ASrc.MaskBitsPerPixel;
    RedPrec      := 1;
    RedShift     := ASrc.MaskShift;
  end;
end;

function GetDescriptionFromAlpha(const ASrc: TRawImageDescription): TRawImageDescription;
begin
  FillByte(Result, SizeOf(Result), 0);

  with Result do
  begin
    Format       := ricfGray;
    Width        := ASrc.Width;
    Height       := ASrc.Height;
    Depth        := ASrc.AlphaPrec;
    BitOrder     := ASrc.BitOrder;
    ByteOrder    := ASrc.ByteOrder;
    LineOrder    := ASrc.LineOrder;
    LineEnd      := ASrc.LineEnd;
    BitsPerPixel := ASrc.BitsPerPixel;
    RedPrec      := ASrc.AlphaPrec;
    RedShift     := ASrc.AlphaShift;
  end;
end;

{$ifdef OldRawImageProcs}
procedure GetRawImageXYPosition(RawImageDesc: PRawImageDescription;
  LineStarts: PRawImagePosition; x, y: cardinal;
  var Position: TRawImagePosition);
begin
  RawImage_GetXYPosition(RawImageDesc^.LineOrder, RawImageDesc^.BitsPerPixel,
                         RawImageDesc^.Height, LineStarts, x, y, Position);
end;
{$endif}

procedure RawImage_GetXYPosition(ALineOrder: TRawImageLineOrder; ABitsPerPixel: Byte;
                                 AHeight: Cardinal; ALineStarts: PRawImagePosition;
                                 x, y: cardinal; var APosition: TRawImagePosition);
var
  BitOffset: Cardinal;
begin
  if ALineOrder = riloBottomToTop then
    y := AHeight - y;
  APosition := ALineStarts[y];
  BitOffset := x * ABitsPerPixel + APosition.Bit;
  APosition.Bit := BitOffset and 7;
  Inc(APosition.Byte, BitOffset shr 3);
end;

{$ifdef OldRawImageProcs}
procedure ExtractRawImageRect(SrcRawImage: PRawImage; const SrcRect: TRect;
  DestRawImage: PRawImage);
begin
  RawImage_ExtractRect(SrcRawImage^, SrcRect, DestRawImage^);
end;
{$endif}
  
procedure RawImage_ExtractRect(const ASrc: TRawImage; const ARect: TRect; out ADst: TRawImage);
  procedure ExtractData(AData: PByte; ADataSize: PtrUInt; ABitsPerPixel: Byte;
                        ABitOrder: TRawImageBitOrder; ALineEnd: TRawImageLineEnd;
                        ADest: PByte; ADestSize: PtrUInt);
  var
    SrcWidth, SrcHeight: cardinal;
    DstWidth, DstHeight: cardinal;
    x, y: Integer;
    LineOrder: TRawImageLineOrder;
    SrcLineStarts, DstLineStarts: PRawImagePosition;
    SrcStartPos, SrcEndPos, DstStartPos: TRawImagePosition;
    Shift0, Shift1: Byte;
    SrcPos: PByte;
    DstPos: PByte;
    ByteCount: PtrUInt;
  begin
    SrcWidth := ASrc.Description.Width;
    DstWidth := ADst.Description.Width;

    //DebugLn'ExtractRawImageDataRect data=',DbgS(DestData),' Size=',DestDataSize);
    if SrcWidth = DstWidth
    then begin
      if ASrc.Description.LineOrder = riloTopToBottom
      then // copy whole source from beginning
        System.Move(AData[0], ADest[0], ADestSize)
      else // copy remainder
        System.Move(AData[ADataSize - ADestSize], ADest[0], ADestSize);
      Exit;
    end;
    
    LineOrder := ASrc.Description.LineOrder;

    SrcHeight := ASrc.Description.Height;
    DstHeight := ADst.Description.Height;


    // calculate line starts
    SrcLineStarts := nil;
    DstLineStarts := nil;
    if ASrc.Description.LineOrder = riloTopToBottom
    then // we only need the first part from start
      RawImage_CreateLineStarts(SrcWidth, ARect.Top + DstHeight, ABitsPerPixel, ALineEnd, SrcLineStarts)
    else
      RawImage_CreateLineStarts(SrcWidth, SrcHeight - ARect.Top, ABitsPerPixel, ALineEnd, SrcLineStarts);
    RawImage_CreateLineStarts(DstWidth, DstHeight, ABitsPerPixel, ALineEnd, DstLineStarts);

    // copy
    for y := 0 to DstHeight - 1 do
    begin
      RawImage_GetXYPosition(LineOrder, ABitsPerPixel, SrcHeight, SrcLineStarts, ARect.Left, y + ARect.Top, SrcStartPos);
      RawImage_GetXYPosition(LineOrder, ABitsPerPixel, SrcHeight, SrcLineStarts, ARect.Right, y + ARect.Top, SrcEndPos);
      RawImage_GetXYPosition(LineOrder, ABitsPerPixel, DstHeight, DstLineStarts, 0, y, DstStartPos);
      
      //DebugLn'ExtractRawImageDataRect A y=',y,' SrcByte=',SrcLineStartPosition.Byte,' SrcBit=',SrcLineStartPosition.Bit,
      //' DestByte=',DestLineStartPosition.Byte,' DestBit=',DestLineStartPosition.Bit);

      if  (SrcStartPos.Bit = 0) and (DstStartPos.Bit = 0)
      then begin
        // copy bytes
        ByteCount := SrcEndPos.Byte - SrcStartPos.Byte;
        if SrcEndPos.Bit > 0
        then Inc(ByteCount);
          
        //DebugLn'ExtractRawImageDataRect B ByteCount=',ByteCount);
        System.Move(AData[SrcStartPos.Byte], ADest[DstStartPos.Byte], ByteCount);
      end
      else if DstStartPos.Bit = 0
      then begin
        // copy and move bits
        ByteCount := (DstWidth * ABitsPerPixel + 7) shr 3;
        SrcPos := @AData[SrcStartPos.Byte];
        DstPos := @ADest[DstStartPos.Byte];
        Shift0 := SrcStartPos.Bit;
        Shift1 := 8 - Shift0;

        if ABitOrder = riboBitsInOrder
        then begin
          // src[byte|bit]: 07 06 05 04 03 02 01 00 :: 17 16 15 14 13 12 11 10 :
          // imagine startbit = 3 ->
          // dst[byte|bit]: 12 11 10 07 06 05 04 03 :
          for x := 0 to ByteCount - 1 do
          begin
            DstPos^ := (SrcPos[0] shr Shift0) or (SrcPos[1] shl Shift1);
            inc(SrcPos);
            inc(DstPos);
          end;
        end
        else begin
          // src[byte|bit]: 07 06 05 04 03 02 01 00 :: 17 16 15 14 13 12 11 10 :
          // imagine startbit = 3 ->
          // dst[byte|bit]: 04 03 02 01 00 17 16 15 :
          for x := 0 to ByteCount - 1 do
          begin
            DstPos^ := (SrcPos[0] shl Shift0) or (SrcPos[1] shr Shift1);
            inc(SrcPos);
            inc(DstPos);
          end;
        end;
      end
      else begin
        DebugLn('ToDo: ExtractRawImageRect DestLineStartPosition.Bit>0');
        break;
      end;
    end;
    // clean up
    FreeMem(SrcLineStarts);
    FreeMem(DstLineStarts);
  end;

var
  SrcMaskDesc, DestMaskDesc: TRawImageDescription;
  R: TRect;
begin
  //DebugLn'ExtractRawImageRect SrcRawImage=',RawImageDescriptionAsString(@SrcRawImage^.Description),
  //  ' SrcRect=',SrcRect.Left,',',SrcRect.Top,',',SrcRect.Right,',',SrcRect.Bottom);

  // copy description
  ADst.Description := ASrc.Description;
  RawImage_ReleaseData(ADst);

  // get intersection
  IntersectRect(R, Rect(0, 0, ASrc.Description.Width, ASrc.Description.Height), ARect);
  ADst.Description.Width := R.Right - R.Left;
  ADst.Description.Height := R.Bottom - R.Top;
  if (ADst.Description.Width <= 0)
  or (ADst.Description.Height <= 0)
  then begin
    ADst.Description.Width := 0;
    ADst.Description.Height := 0;
    Exit;
  end;
  
  // allocate some space
  RawImage_CreateData(ADst, False);

  // extract rectangle from Data
  ExtractData(ASrc.Data, ASrc.DataSize,
              ASrc.Description.BitsPerPixel, ASrc.Description.BitOrder,
              ASrc.Description.LineEnd, ADst.Data, ADst.DataSize);

  // extract rectangle from MAsk

  if ASrc.Description.MaskBitsPerPixel = 0 then Exit;
  if ASrc.Mask = nil then Exit;
  if ASrc.MaskSize = 0 then Exit;


  //DebugLn'ExtractRawImageRect Mask SrcRawImage=',RawImageDescriptionAsString(@SrcMaskDesc));
  ExtractData(ASrc.Mask, ASrc.MaskSize,
              ASrc.Description.MaskBitsPerPixel, ASrc.Description.MaskBitOrder,
              ASrc.Description.MaskLineEnd, ADst.Mask, ADst.MaskSize);
end;

{$ifdef OldRawImageProcs}
procedure CreateRawImageLineStarts(Width, Height, BitsPerPixel: cardinal;
  LineEnd: TRawImageLineEnd; var LineStarts: PRawImagePosition);
begin
  RawImage_CreateLineStarts(Width, Height, BitsPerPixel, LineEnd, LineStarts);
end;
{$endif}

procedure RawImage_CreateLineStarts(AWidth, AHeight: cardinal; ABitsPerPixel: Byte;
  ALineEnd: TRawImageLineEnd; var ALineStarts: PRawImagePosition);
// LineStarts is recreated, so make sure it is nil or a valid mem
var
  PixelCount: cardinal;
  BitsPerLine: cardinal;
  CurLine: cardinal;
  BytesPerLine: cardinal;
  ExtraBitsPerLine: Byte;
  CurBitOffset: Byte;
  LoopBit: Byte;
  LoopByte: PtrUInt;
begin
  // get current size
  PixelCount := AWidth * AHeight;
  if PixelCount = 0 then exit;

  // calculate BitsPerLine, BytesPerLine and ExtraBitsPerLine
  BitsPerLine := GetBitsPerLine(AWidth, ABitsPerPixel, ALineEnd);
  BytesPerLine := BitsPerLine shr 3;
  ExtraBitsPerLine := BitsPerLine and 7;

  // create line start array
  ReAllocMem(ALineStarts, AHeight * SizeOf(TRawImagePosition));
  ALineStarts[0].Byte := 0;
  ALineStarts[0].Bit := 0;
  LoopBit := 0;
  LoopByte := 0;
  for CurLine := 1 to AHeight-1 do
  begin
    CurBitOffset := LoopBit + ExtraBitsPerLine;
    LoopByte := LoopByte + BytesPerLine + (CurBitOffset shr 3);
    LoopBit := CurBitOffset and 7;
    ALineStarts[CurLine].Byte := LoopByte;
    ALineStarts[CurLine].Bit := LoopBit;
  end;
end;

function GetBytesPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd): PtrUInt;
begin
  Result := (GetBitsPerLine(AWidth, ABitsPerPixel, ALineEnd) + 7) shr 3;
end;

function GetBitsPerLine(AWidth: Cardinal; ABitsPerPixel: Byte; ALineEnd: TRawImageLineEnd): PtrUInt;
begin
  Result := AWidth * ABitsPerPixel;
  case ALineEnd of
    rileTight: ;
    rileByteBoundary:   Result := (Result +  7) and not PtrUInt(7);
    rileWordBoundary:   Result := (Result + 15) and not PtrUInt(15);
    rileDWordBoundary:  Result := (Result + 31) and not PtrUInt(31);
    rileQWordBoundary:  Result := (Result + 63) and not PtrUInt(63);
    rileDQWordBoundary: Result := (Result +127) and not PtrUInt(127);
  end;
end;

{$ifdef OldRawImageProcs}
procedure ReadRawImageBits(TheData: PByte;
  const Position: TRawImagePosition;
  BitsPerPixel, Prec, Shift: cardinal; BitOrder: TRawImageBitOrder;
  var Bits: word);
begin
  RawImage_ReadBits(TheData, Position, BitsPerPixel, Prec, Shift, BitOrder, Bits);
end;
{$endif}
  
procedure RawImage_ReadBits(AData: PByte; const APosition: TRawImagePosition;
                       ABitsPerPixel, APrec, AShift: Byte;
                       ABitOrder: TRawImageBitOrder; out ABits: Word);
var
  PB: PByte;
  PW: PWord  absolute PB;
  PC: PCardinal absolute PB;
  PrecMask: Word;
begin
  PrecMask := (Word(1) shl APrec) - 1;
  PB := @AData[APosition.Byte];
  case ABitsPerPixel of
  1,2,4:
      begin
        if ABitOrder = riboBitsInOrder then
          ABits := (PB^ shr (AShift + APosition.Bit)) and PrecMask
        else
          ABits := (PB^ shr (AShift + 7 - APosition.Bit)) and PrecMask;
      end;
  8:  begin
        ABits := (PB^ shr AShift) and PrecMask;
      end;
  16: begin
        {$note check endian and/or source byte order}
        ABits := (PW^ shr AShift) and PrecMask;
      end;
  32: begin
        {$note check endian and/or source byte order}
        ABits := (PC^ shr AShift) and PrecMask;
      end;
  else
    ABits:=0;
  end;
  
  if APrec<16
  then begin
    // add missing bits
    ABits := ABits shl (16 - APrec);
    ABits := ABits or MissingBits[APrec, ABits shr 13];
  end;
end;

{$ifdef OldRawImageProcs}
procedure WriteRawImageBits(TheData: PByte;
  const Position: TRawImagePosition;
  BitsPerPixel, Prec, Shift: cardinal; BitOrder: TRawImageBitOrder; Bits: word);
begin
  RawImage_WriteBits(TheData, Position, BitsPerPixel, Prec, Shift, BitOrder, Bits);
end;
{$endif}

procedure RawImage_WriteBits(AData: PByte; const APosition: TRawImagePosition;
                       ABitsPerPixel, APrec, AShift: Byte;
                       ABitOrder: TRawImageBitOrder; ABits: Word);
var
  PB: PByte;
  PW: PWord absolute PB;
  PC: PCardinal absolute PB;
  PrecMask: Cardinal;
  BitShift: Integer;
begin
  PB := @AData[APosition.Byte];
  PrecMask := (Cardinal(1) shl APrec) - 1;
  ABits := ABits shr (16 - APrec);
  
  case ABitsPerPixel of
  1,2,4:
      begin
        if ABitOrder = riboBitsInOrder
        then BitShift := AShift + APosition.Bit
        else BitShift := AShift + 7 - APosition.Bit;
        
        PrecMask := not(PrecMask shl BitShift);
        PB^ := (PB^ and PrecMask) or (ABits shl BitShift);
      end;
  8:  begin
        PrecMask := not(PrecMask shl aShift);
        PB^ := (PB^ and PrecMask) or (ABits shl AShift);
      end;
  16: begin
        {$note check endian and/or source byte order}
        PrecMask := not(PrecMask shl AShift);
        PW^ := (PW^ and PrecMask) or (ABits shl AShift);
      end;
  32: begin
        {$note check endian and/or source byte order}
        PrecMask := not(PrecMask shl AShift);
        PC^ := (PC^ and PrecMask) or (ABits shl AShift);
      end;
  end;
end;

{$ifdef OldRawImageProcs}
procedure ReAlignRawImageLines(var Data: Pointer; var Size: PtrUInt;
  Width, Height, BitsPerPixel: cardinal;
  var OldLineEnd: TRawImageLineEnd; NewLineEnd: TRawImageLineEnd);
var
  OldBytesPerLine: PtrUInt;
  OldSize: PtrUInt;
  NewBytesPerLine: PtrUInt;
  NewSize: PtrUInt;
  y: Integer;
  OldPos: Pointer;
  NewPos: Pointer;
begin
  if OldLineEnd=NewLineEnd then exit;
  if (Width=0) or (Height=0) then exit;
  OldBytesPerLine:=GetBytesPerLine(Width,BitsPerPixel,OldLineEnd);
  OldSize:=OldBytesPerLine*PtrUInt(Height);
  if OldSize<>Size then
    RaiseGDBException('ReAlignRawImageLines OldSize<>Size');
  NewBytesPerLine:=GetBytesPerLine(Width,BitsPerPixel,NewLineEnd);
  NewSize:=NewBytesPerLine*PtrUInt(Height);
  //DebugLn(['ReAlignRawImageLines OldBytesPerLine=',OldBytesPerLine,' NewBytesPerLine=',NewBytesPerLine]);
  
  // enlarge before
  if OldSize<NewSize then
    ReAllocMem(Data,NewSize);

  // move data
  OldPos:=Data;
  NewPos:=Data;
  if OldBytesPerLine>NewBytesPerLine then begin
    // compress
    for y:=0 to Height-1 do begin
      System.Move(OldPos^,NewPos^,NewBytesPerLine);
      inc(OldPos,OldBytesPerLine);
      inc(NewPos,NewBytesPerLine);
    end;
  end else begin
    // expand
    inc(OldPos,OldSize);
    inc(NewPos,NewSize);
    for y:=Height-1 downto 0 do begin
      dec(OldPos,OldBytesPerLine);
      dec(NewPos,NewBytesPerLine);
      System.Move(OldPos^,NewPos^,OldBytesPerLine);
    end;
  end;
      
  // shrink after
  if OldSize>NewSize then
    ReAllocMem(Data,NewSize);
    
  Size:=NewSize;
  OldLineEnd:=NewLineEnd;
end;
{$endif}


//------------------------------------------------------------------------------
procedure InternalInit;
var
  Prec: Integer;
  HighValue: word;
  Bits: word;
  CurShift, DShift: Integer;
begin
  for Prec := 0 to 15 do
  begin
    for HighValue := 0 to 7 do
    begin
      // HighValue represents the three highest bits
      // For example:
      //   Prec=5 and the read value is %10110
      //   => HighValue=%101
      // copy the HighValue till all missing bits are set
      // For example:
      //   Prec=5, HighValue=%110
      // => MissingBits[5,6]:=%0000011011011011
      //   because 00000 110 110 110 11
      MissingBits[Prec, HighValue] := 0;
      if Prec = 0 then Continue;

      if Prec>=3 then begin
        DShift := 3;
        Bits := HighValue;
      end else begin
        DShift := Prec;
        Bits := HighValue shr (3-Prec);
      end;
      
      CurShift := 16 - Prec;
      while CurShift > 0 do
      begin
        //DebugLn(['InternalInit CurShift=',CurShift,' DShift=',DShift]);
        if CurShift >= DShift then
          MissingBits[Prec, HighValue] :=
            MissingBits[Prec, HighValue] or (Bits shl (CurShift - DShift))
        else
          MissingBits[Prec, HighValue] :=
            MissingBits[Prec, HighValue] or (Bits shr (DShift - CurShift));
        Dec(CurShift, DShift);
      end;
    end;
  end;
end;

initialization
  InternalInit;

end.
