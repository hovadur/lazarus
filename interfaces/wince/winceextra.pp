{
 *****************************************************************************
 *                            WinCEWinApiEmu.pp                              *
 *                            -----------------                              *
 * Extra WinCE code that's not in the RTL or present on all WinCE versions.  *
 *                                                                           *
 *****************************************************************************

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

  Author: Roozbeh GHolizadeh
          Marc Weustink

  Abstract:
    Missing and usefull windows api are defined and emulated here.
    Not all functionalities are present,but only those neccessary for lcl to function.
}
unit WinCEExtra;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, Maps, GraphType;

type
  DrawStateProc = function(
    dc:HDC;         // handle to device context
    ldata: LPARAM;  // image information
    wData: WPARAM;  // more image information
    cx: integer;    // image width
    cy: integer     // image height  
  ) : boolean;

const
  { State type }
  DSS_NORMAL    = $0000;
  DSS_UNION     = $0010;  { Gray string appearance }
  DSS_DISABLED  = $0020;
  DSS_MONO      = $0080;
  DSS_RIGHT     = $8000;
  DSS_DEFAULT   =$0040;  { Make it bold }
  DSS_HIDEPREFIX=$0200;
  DSS_PREFIXONLY=$0400;
  
  { missing progress bar styles }
  PBS_SMOOTH=01;
  PBS_VERTICAL=04;
  PBM_SETRANGE32=WM_USER+6;



function DrawState(dc:HDC ; hbr : HBRUSH ; func: DRAWSTATEPROC ; lp:LPARAM; wp:WPARAM;x,y,cx,cy:integer;flags:UINT) : boolean;
function GetTopWindow(hWnd:HWND):HWND;

function SetProp(Wnd: HWND; {lpString:LPCSTR;} hData:HANDLE):WINBOOL;
function GetProp(Wnd: HWND{ lpString:LPCSTR}):HANDLE;
function RemoveProp(Wnd: HWND{; lpString:LPCSTR}):HANDLE;
function EnumProps(Wnd: HWND;lpEnumFunc:PROPENUMPROC) : integer;

const
  // BlendOp flags
  AC_SRC_OVER = $00;
  // AlphaFormat flags
  AC_SRC_ALPHA = $01;

// AlphaBlend is only defined for win98&2k and up 
// load dynamic and use ownfunction if not defined
var
  AlphaBlend: function(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;



implementation

const
  wPattern55AA: array[1..8] of word = ($5555, $aaaa, $5555, $aaaa, $5555, $aaaa, $5555, $aaaa);

var
  MPropertyLists: TMap;

function SetProp(Wnd:HWND; {lpString:LPCSTR;} hData: HANDLE):WINBOOL;
begin
  Result := true;
  if not MPropertyLists.SetData(Wnd, hData)
  then MPropertyLists.Add(Wnd, hData);
end;


function GetProp(Wnd:HWND{; lpString:LPCSTR}):HANDLE;
begin
  Result := 0;
  MPropertyLists.GetData(Wnd, Result)
end;


function RemoveProp(Wnd:HWND{; lpString:LPCSTR}):HANDLE;
begin
  Result := 0;
  if MPropertyLists.GetData(Wnd, Result)
  then MPropertyLists.Delete(Wnd);
end;

//well we only have one property for each window handle so just find and call that
// return -1 if none found!
function EnumProps(Wnd:HWND;lpEnumFunc:PROPENUMPROC) : integer;
var
  h:HANDLE;
begin
  h := GetProp(Wnd);
  if h <> 0
  then Result := integer(lpEnumFunc(Wnd,'',h))
  else Result := -1;
end;

function GetTopWindow(hWnd:HWND):HWND;
begin
  Result := GetWindow(hWnd,GW_CHILD);
end;


{ Wine sources - www.winehq.com - mostly used for emulating DrawState functions }
function DrawStateJam(dc:HDC; opcode:UINT; func: DrawStateProc; lp:LPARAM; wp: WPARAM; rc:LPRECT; dtflags: UINT): boolean;
var
  memdc: HDC;
  hbmsave: HBITMAP;
  cx,cy: integer;
begin
  cx := rc^.Right - rc^.left;
  cy := rc^.bottom - rc^.top;
  
  case opcode of
    DST_TEXT, DST_PREFIXTEXT:
      Result := DrawText(dc, PWideChar(lp), wp, rc, dtflags) <> 0;
  
    DST_ICON:
      Result := DrawIcon(dc, rc^.left, rc^.top, lp);
  
    DST_BITMAP: begin
      memdc := CreateCompatibleDC(dc);
      if memdc = 0 then Exit(False);
      
      hbmsave := SelectObject(memdc, lp);
      if hbmsave = 0 then
      begin
        DeleteDC(memdc);
        Exit(False);
      end;
      
      Result := BitBlt(dc, rc^.left, rc^.top, cx, cy, memdc, 0, 0, SRCCOPY);
      SelectObject(memdc, hbmsave);
      DeleteDC(memdc);
    end;
  
    DST_COMPLEX: begin
      if func <> nil then
      begin
        { DRAWSTATEPROC assumes that it draws at the center of coordinates  }
        //OffsetViewportOrgEx(dc, rc^.left, rc^.top, nil);
        Result := func(dc, lp, wp, cx, cy);
        
        { Restore origin }
        //OffsetViewportOrgEx(dc, -rc^.left, -rc^.top, nil);
      end 
      else Result := False;
    end;        
  else  
    Result := False;
  end;
end;


{$goto on} // TODO: remove goto

function DrawState(dc:HDC ; hbr : HBRUSH ; func: DRAWSTATEPROC ; lp:LPARAM; wp:WPARAM;x,y,cx,cy:integer;flags:UINT) : boolean;
label
  cleanup;
var
  hbm,hbmsave :HBITMAP;
  hfsave : HFONT;
  hbsave,hbrtmp : HBRUSH;
  memdc : HDC;
  rc:TRECT;
  dtflags:UINT;
  opcode:UINT;
  len:integer;
  tmp : boolean;
  s:SIZE;
  //ici:^CURSORICONINFO;
  bm:BITMAP;
  fg, bg : COLORREF;
  
  h55AABrush : HBRUSH;
  h55AABitmap: HBITMAP;
  
begin
  Result := False; 
  hbrtmp := 0;
  dtflags := DT_NOCLIP;
  opcode := flags and $f;
  len := wp;

  if  ((opcode = DST_TEXT) or (opcode = DST_PREFIXTEXT)) and (len=0) 
  then len := length(widestring(PWideChar(lp))); // The string is '\0' terminated 

  { Find out what size the image has if not given by caller }
  if (cx=0) or (cy=0) then
  begin
    case opcode of
      DST_TEXT,DST_PREFIXTEXT:
        begin
          if not GetTextExtentPoint32(dc, pwidechar(lp), len, @s) 
          then Exit;
        end;

      {DST_ICON:
        begin
          ici = (CURSORICONINFO *)GlobalLock16((HGLOBAL16)lp);
          if(!ici) then return false;
          s.cx = ici->nWidth;
          s.cy = ici->nHeight;
          GlobalUnlock16((HGLOBAL16)lp);
        end;}

      DST_BITMAP:
        begin
          if GetObject(lp, sizeof(bm), @bm) = 0 
          then Exit;
          s.cx := bm.bmWidth;
          s.cy := bm.bmHeight;
        end;

      DST_COMPLEX: {/* cx and cy must be set in this mode */}
        Exit;
    end;

    if cx = 0 then cx := s.cx;
    if cy = 0 then cy := s.cy;
  end;

  rc.left   := x;
  rc.top    := y;
  rc.right  := x + cx;
  rc.bottom := y + cy;

  if (flags and DSS_RIGHT) <> 0 { This one is not documented in the win32.hlp file }
  then dtflags := dtflags or DT_RIGHT;
  
  if opcode = DST_TEXT 
  then dtflags := dtflags or DT_NOPREFIX;

  { For DSS_NORMAL we just jam in the image and return }
  if (flags and $7ff0) = DSS_NORMAL 
  then Exit(DrawStateJam(dc, opcode, func, lp, len, @rc, dtflags));

  { For all other states we need to convert the image to B/W in a local bitmap
    before it is displayed }
  fg := SetTextColor(dc, RGB(0, 0, 0));
  bg := SetBkColor(dc, RGB(255, 255, 255));
  hbm := 0;
  hbmsave := 0;
  memdc := 0;
  hbsave := 0;

  { From here on we must use "goto cleanup" when something goes wrong }
  // MWE: you can also use an exception block for this.

  hbm := CreateBitmap(cx, cy, 1, 1, nil);
  if hbm = 0 then goto cleanup;
  
  memdc := CreateCompatibleDC(dc);
  if memdc = 0 then goto cleanup;
  
  hbmsave := SelectObject(memdc, hbm);
  if hbmsave = 0 then goto cleanup;
  
  rc.top := 0;
  rc.left := 0;
  rc.right := cx;
  rc.bottom := cy;
  if FillRect(memdc, rc, GetStockObject(WHITE_BRUSH)) = 0 then goto cleanup;
  
  SetBkColor(memdc, RGB(255, 255, 255));
  SetTextColor(memdc, RGB(0, 0, 0));
  hfsave := SelectObject(memdc, GetCurrentObject(dc, OBJ_FONT));

  { DST_COMPLEX may draw text as well,
    so we must be sure that correct font is selected }
  if (hfsave = 0) and (opcode <= DST_PREFIXTEXT) then goto cleanup;
  tmp := DrawStateJam(memdc, opcode, func, lp, len, @rc, dtflags);
  if hfsave <> 0 then SelectObject(memdc, hfsave);
  if not tmp then goto cleanup;

  { This state cause the image to be dithered }
  if (flags and DSS_UNION) <> 0 then
  begin
    h55AABitmap := CreateBitmap( 8, 8, 1, 1, @wPattern55AA);
    h55AABrush := CreatePatternBrush(h55AABitmap);
    hbsave := SelectObject(memdc, h55AABrush);
    if hbsave = 0 
    then begin
      DeleteObject(h55AABrush);
      DeleteObject(h55AABitmap);
      goto cleanup;
    end;
    
    tmp := PatBlt(memdc, 0, 0, cx, cy, $00FA0089);
    SelectObject(memdc, hbsave);
    DeleteObject(h55AABrush);
    DeleteObject(h55AABitmap);
    if not tmp then goto cleanup;
  end;

  if (flags and DSS_DISABLED) <> 0 
  then
     hbrtmp := CreateSolidBrush(GetSysColor(COLOR_3DHILIGHT))
  else if (flags and DSS_DEFAULT) <> 0 
  then
     hbrtmp := CreateSolidBrush(GetSysColor(COLOR_3DSHADOW));

  { Draw light or dark shadow }
  if (flags and (DSS_DISABLED or DSS_DEFAULT)) <> 0 then
  begin
    if hbrtmp = 0 then goto cleanup;
    hbsave := SelectObject(dc, hbrtmp);
    if hbsave = 0 then goto cleanup;
    if not BitBlt(dc, x+1, y+1, cx, cy, memdc, 0, 0, $00B8074A)  then goto cleanup;
    SelectObject(dc, hbsave);
    DeleteObject(hbrtmp);
    hbrtmp := 0;
  end;

  if (flags and DSS_DISABLED) <> 0 then
  begin
    hbrtmp := CreateSolidBrush(GetSysColor(COLOR_3DSHADOW));
    hbr := hbrtmp;
    if hbrtmp = 0 then goto cleanup;
  end
  else begin
    if hbr = 0 
    then hbr := GetStockObject(BLACK_BRUSH);
  end;

  hbsave := SelectObject(dc, hbr);

  if not BitBlt(dc, x, y, cx, cy, memdc, 0, 0, $00B8074A) then goto cleanup;

  Result := True;

cleanup:
  SetTextColor(dc, fg);
  SetBkColor(dc, bg);

  if(hbsave<>0)  then SelectObject(dc, hbsave);
  if(hbmsave<>0) then SelectObject(memdc, hbmsave);
  if(hbrtmp<>0)  then DeleteObject(hbrtmp);
  if(hbm<>0)     then DeleteObject(hbm);
  if(memdc<>0)   then DeleteDC(memdc);
end;

function _AlphaBlend(hdcDest: HDC; nXOriginDest, nYOriginDest, nWidthDest, nHeightDest: Integer; hdcSrc: HDC; nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc: Integer; blendFunction: TBlendFunction): BOOL; stdcall;
var
  R: TRect;
  SrcImage, DstImage: TRawImage;
  SrcDC: HDC;
  bmp: HBITMAP;
  X, Y: Integer;
begin
  if blendFunction.AlphaFormat = 0
  then begin
    Result := True;
    case blendFunction.SourceConstantAlpha of
      0: begin
        Exit;
      end;
      255: begin
        // simple strechblt
        StretchBlt(hdcDest, nXOriginDest, nYOriginDest, nWidthDest, nHeightDest, hdcSrc, nXOriginSrc, nYOriginSrc, nWidthSrc, nHeightSrc, SRCCOPY);
      end;
    end;
  end;

  // TODO: implement someday for older versions

(*
  // get source by replacing it with a dummy
  R := Rect(nXOriginSrc, nYOriginSrc, nXOriginSrc + nWidthSrc, nYOriginSrc + nHeightSrc);
  bmp := CreateBitmap(1,1,1,1,nil);
  bmp := SelectObject(hdcSrc, bmp);
  Result := Widgetset.RawImage_FromBitmap(SrcImage, bmp, 0, R);
  // Restore source
  bmp := SelectObject(hdcSrc, bmp);

  // Get destination
  bmp := SelectObject(hdcDest, bmp);
  // check if destination is 32bit, copy to temp 32bit if not so
  // ...
  // create dstimage
  Result := Widgetset.RawImage_FromBitmap(DstImage, bmp, 0, R);
  // Restore destination
  bmp := SelectObject(hdcDest, bmp);

  // check if resized
  if (nWidthDest <> nWidthSrc) or (nHeightDest <> nHeightSrc)
  then begin
    // separate image and alpha, scale the resulting image and recreate Src Rawimage

  end;
  
  // loop through pixels
  
  // create and bitblt destination bitmap
  
  // cleanup
  
*)
end;

var
  kerneldllhandle: THandle = 0;

procedure Initialize;
var
  p: Pointer;
begin                
  AlphaBlend := @_AlphaBlend;
  kerneldllhandle := LoadLibrary(KernelDLL);
  if kerneldllhandle <> 0
  then begin 
    p := GetProcAddress(kerneldllhandle, 'AlphaBlend');
    if p <> nil
    then Pointer(AlphaBlend) := p;
  end;
  
  {$if SizeOf(THandle) = 4}
  MPropertyLists := TMap.Create(itu4, 4);
  {$else}
  MPropertyLists := TMap.Create(itu8, 8);
  {$endif}
end;

procedure Finalize;
begin
  AlphaBlend := @_AlphaBlend;
  if kerneldllhandle <> 0
  then FreeLibrary(kerneldllhandle);
  kerneldllhandle := 0;
  
  FreeAndNil(MPropertyLists);
end;

initialization
  Initialize;

finalization
  Finalize;
  
end.




