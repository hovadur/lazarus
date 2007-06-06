{ $Id$ }
{
                      ------------------------------------
                      win32debug.pp  -  graphic dump utils 
                      ------------------------------------
 
 @created(Fri Jun 1th WET 2007)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)                       

 This unit contains utility functions to show the contents of graphics
 
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

unit Win32Debug;

{$mode objfpc}{$H+}

interface 

uses
  windows, ctypes, sysutils;

procedure DbgDumpBitmap(ABitmap: HBITMAP; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
procedure DbgDumpDC(ADC: HDC; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);

implementation

type
  PDbgDumpInfo = ^TDbgDumpInfo;
  TDbgDumpInfo = record                            
    Width, Height: Integer;
    Bitmap: HBITMAP;
  end;

function DbgWindowProc(Wnd: HWnd; Msg: UINT; WParam: WPAram; LParam: LPARAM): LRESULT; stdcall;
var
  Info: PDbgDumpInfo;
  PS: TPaintStruct;
  DC: HDC;
  OldBmp: HBITMAP;
begin
  Result := 0;
  case Msg of
    WM_PAINT: begin
      // grrr.... this function isn't mapped to GetWindowLong
      {$ifdef CPU64}
      Info := Pointer(GetWindowLongPtr(wnd, GWL_USERDATA));
      {$else}
      Info := Pointer(GetWindowLong(wnd, GWL_USERDATA));
      {$endif}
      BeginPaint(Wnd, PS);
      DC := CreateCompatibleDC(PS.hdc);
      OldBmp := SelectObject(DC, Info^.Bitmap);
      BitBlt(PS.hDC, 0, 0, Info^.Width, Info^.Height, DC, 0,0, SRCCOPY);
      SelectObject(DC, OldBmp);
      DeleteDC(DC);
      EndPaint(Wnd, PS);
    end;
    WM_DESTROY: begin
      {$ifdef CPU64}
      Info := Pointer(GetWindowLongPtr(wnd, GWL_USERDATA));
      {$else}
      Info := Pointer(GetWindowLong(wnd, GWL_USERDATA));
      {$endif}
      DeleteObject(Info^.Bitmap);
      Dispose(Info);
    end;
  else
    Result := DefWindowProc(wnd, Msg, WParam, LParam);
  end;
end;

var
  MDbgClassCreated: Boolean = False;

procedure DbgCreateClass;
var
  wc: TWndClass;
begin
  if MDbgClassCreated then Exit;

  FillByte(wc, SizeOf(wc), 0);
  wc.style := CS_HREDRAW or CS_VREDRAW;
  wc.lpfnWndProc := @DbgWindowProc;
  wc.hInstance := hinstance;
  wc.hbrBackground := GetStockObject(WHITE_BRUSH);
  wc.lpszClassName := 'LazDbgWindow';
  RegisterClass(wc);

  MDbgClassCreated := True;
end;

procedure DbgCreateWindow(AInfo: PDbgDumpInfo; const ATitle: String);
var
  window: HWND;  
  w, h: Integer;
begin
  DbgCreateClass;
  if AInfo^.Width < 50 then W := 50 else w := AInfo^.Width;
  if AInfo^.Height < 25 then H := 25 else H := AInfo^.Height;
  window := CreateWindowEx(WS_EX_TOOLWINDOW, 'LazDbgWindow', PChar(ATitle), WS_OVERLAPPEDWINDOW, CW_USEDEFAULT, CW_USEDEFAULT, W + 8, H+ 25, 0, 0, HINSTANCE, nil);
  {$ifdef CPU64}
  SetWindowLongPtr(window, GWL_USERDATA, PtrInt(AInfo));
  {$else}
  SetWindowLong(window, GWL_USERDATA, PtrInt(AInfo));
  {$endif}

  ShowWindow(window, SW_SHOWNOACTIVATE); 
end;

procedure InternalDumpBitmap(ABitmap: HBITMAP; ADesc, ATitle: String; AWidth: Integer; AHeight: Integer);
var
  Info: PDbgDumpInfo;
  h,w,d: Integer;
  WinBmp: Windows.TBitmap;
begin
  New(Info);
  if (ABitmap = 0)
  or (Windows.GetObject(ABitmap, SizeOf(WinBmp), @WinBmp) = 0)
  then begin
    w := 0; h:= 0; d := 0;
    Info^.Bitmap := 0;
    if AWidth = -1 then AWidth := 0;
    if AHeight = -1 then AHeight := 0;
  end
  else begin
    w := WinBmp.bmWidth;
    h := WinBmp.bmHeight;
    d := WinBmp.bmBitsPixel;
    if AWidth = -1 then AWidth := W;
    if AHeight = -1 then AHeight := H;
    Info^.Bitmap := CopyImage(ABitmap, IMAGE_BITMAP, AWidth, AHeight, 0);
  end;
  
  Info^.Width := AWidth;
  Info^.Height := AHeight;

  ATitle := ATitle + Format(' (%s W:%d H:%d D:%d)', [ADesc, w, h, d]);
  DbgCreateWindow(Info, ATitle);
end;

procedure DbgDumpBitmap(ABitmap: HBITMAP; ATitle: String = ''; AWidth: Integer = -1; AHeight: Integer = -1);
begin
  InternalDumpBitmap(ABitmap, Format('Bitmap:$%x', [ABitmap]), ATitle, AWidth, AHeight);
end;

procedure DbgDumpDC(ADC: HDC; ATitle: String; AWidth, AHeight: Integer);
var
  bmp: HBITMAP;
begin
  bmp := CreateBitmap(1,1,1,1,nil);
  // select dummy to get selected bitmap
  bmp := SelectObject(ADC, bmp);
  InternalDumpBitmap(bmp, Format('DC:$%x', [ADC]), ATitle, AWidth, AHeight);
  // restore bitmap and delete dummy
  DeleteObject(SelectObject(ADC, bmp));
end;

end.
