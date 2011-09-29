{ $Id: $}
{                  --------------------------------------------
                  cocoaprivate.pp  -  Cocoa internal classes
                  --------------------------------------------

 This unit contains the private classhierarchy for the Cocoa implemetations

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CocoaPrivate;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils;

type
  { LCLObjectExtension }

  LCLObjectExtension = objccategory(NSObject)
    function lclIsEnabled: Boolean; message 'lclIsEnabled';
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:';
    function lclIsVisible: Boolean; message 'lclIsVisible';

    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:';
    procedure lclInvalidate; message 'lclInvalidate';
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::';
    procedure lclLocalToScreen(var X,Y: Integer); message 'lclLocalToScreen::';
    function lclParent: id; message 'lclParent';
    function lclFrame: TRect; message 'lclFrame';
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:';
    function lclClientFrame: TRect; message 'lclClientFrame';
  end;

  { LCLViewExtension }

  LCLViewExtension = objccategory(NSView)
    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclLocalToScreen(var X,Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    function lclParent: id; message 'lclParent'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
  end;

  { LCLControlExtension }

  LCLControlExtension = objccategory(NSControl)
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;
  end;

  { LCLWindowExtension }

  LCLWindowExtension = objccategory(NSWindow)
    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclLocalToScreen(var X,Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
  end;

  { TCommonCallback }

  TCommonCallback = class(TObject)
  public
    Owner : NSObject;
    constructor Create(AOwner: NSObject);
    procedure MouseDown(x,y: Integer); virtual; abstract;
    procedure MouseUp(x,y: Integer); virtual; abstract;
    procedure MouseClick(ClickCount: Integer); virtual; abstract;
    procedure MouseMove(x,y: Integer); virtual; abstract;
    procedure Draw(ctx: NSGraphicsContext; const bounds, dirty: NSRect); virtual; abstract;
  end;

  { TWindowCallback }

  TWindowCallback = class(TObject)
  public
    Owner : NSWindow;
    constructor Create(AOwner: NSWindow);
    procedure Activate; virtual; abstract;
    procedure Deactivate; virtual; abstract;
    procedure CloseQuery(var CanClose: Boolean); virtual; abstract;
    procedure Close; virtual; abstract;
    procedure Resize; virtual; abstract;
  end;

  { TCocoaMenu }

  TCocoaMenu = objcclass(NSMenu)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

  { TCocoaMenuItem }

  TCocoaMenuItem = objcclass(NSMenuItem)
  public
    procedure lclItemSelected(sender: id); message 'lclItemSelected:';
  end;

  { TCocoaButton }

  TCocoaButton = objcclass(NSButton)
  protected
    procedure actionButtonClick(sender: NSObject); message 'actionButtonClick:';
  public
    callback  : TCommonCallback;
    function initWithFrame(frameRect: NSRect): id; override;
    function acceptsFirstResponder: Boolean; override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
  end;

  TCocoaTextField = objcclass(NSTextField)
    callback  : TCommonCallback;
    function acceptsFirstResponder: Boolean; override;
  end;

  { TCocoaSecureTextField }

  TCocoaSecureTextField = objcclass(NSSecureTextField)
    callback  : TCommonCallback;
    function acceptsFirstResponder: Boolean; override;
  end;


  TCocoaTextView = objcclass(NSTextView)
    callback  : TCommonCallback;
    function acceptsFirstResponder: Boolean; override;
  end;

  { TCocoaWindow }

  TCocoaWindow = objcclass(NSWindow)
  protected
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
  public
    callback      : TCommonCallback;
    wincallback   : TWindowCallback;
    function acceptsFirstResponder: Boolean; override;
    procedure mouseUp(event: NSEvent); override;
    procedure mouseDown(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
  end;

  { TCocoaCustomControl }

  TCocoaCustomControl = objcclass(NSControl)
    callback  : TCommonCallback;
    procedure drawRect(dirtyRect: NSRect); override;
  end;

  { TCocoaScrollView }

  TCocoaScrollView = objcclass(NSScrollView)
    callback  : TCommonCallback;
  end;


  TCocoaComboBox = objcclass;

  { TCocoaComboBoxList }

  TCocoaComboBoxList = class(TStringList)
  private
    fOwner  : TCocoaComboBox;
  protected
    procedure Changed; override;
  public
    constructor Create(AOwner: TCocoaComboBox);
    property Owner: TCocoaComboBox read fOwner;
  end;

  { TCocoaComboBox }

  TCocoaComboBox = objcclass(NSComboBox)
    callback  : TCommonCallback;
    list      : TCocoaComboBoxList;
    function comboBox_objectValueForItemAtIndex_(combo: TCocoaComboBox; row: NSInteger): id;
      message 'comboBox:objectValueForItemAtIndex:';
    function numberOfItemsInComboBox(combo: TCocoaComboBox): NSInteger;
      message 'numberOfItemsInComboBox:';
    procedure dealloc; override;
  end;

  { TCocoaScrollBar }

  TCocoaScrollBar = objcclass(NSScroller)
    callback  : TCommonCallback;
  end;

  TCocoaListView = objcclass;

  { TCocoaStringList }

  TCocoaStringList = class(TStringList)
  protected
    procedure Changed; override;
  public
    Owner : TCocoaListView;
    constructor Create(AOwner: TCocoaListView);
  end;

  { TCocoaListView }

  TCocoaListView = objcclass(NSTableView)
    callback  : TCommonCallback;
    list      : TCocoaStringList;
    function numberOfRowsInTableView(aTableView: NSTableView): NSInteger; message 'numberOfRowsInTableView:';
    function tableView_objectValueForTableColumn_row(tableView: NSTableView;
      objectValueForTableColumn: NSTableColumn; row: NSInteger):id;
      message 'tableView:objectValueForTableColumn:row:';
  end;

  { TCocoaGroupBox }

  TCocoaGroupBox = objcclass(NSBox)
    callback  : TCommonCallback;
  end;

implementation

{ TCocoaButton }

procedure TCocoaButton.actionButtonClick(sender: NSObject);
begin
  callback.MouseClick(1);
  //todo: simulate MouseUp
end;

function TCocoaButton.initWithFrame(frameRect: NSRect): id;
begin
  Result:=inherited initWithFrame(frameRect);
  if Assigned(Result) then begin
    setTarget(Self);
    setAction(objcselector('actionButtonClick:'));
  end;
end;

function TCocoaButton.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

procedure TCocoaButton.mouseUp(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  callback.MouseUp(round(mp.x), round(mp.y));
  inherited mouseUp(event);
end;

procedure TCocoaButton.mouseDown(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  callback.MouseDown(round(mp.x), round(mp.y));
  inherited mouseDown(event);
end;

procedure TCocoaButton.mouseDragged(event: NSEvent);
begin
  inherited mouseDragged(event);
end;

procedure TCocoaButton.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaButton.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaButton.mouseMoved(event: NSEvent);
begin
  inherited mouseMoved(event);
end;

{ TCocoaTextField }

function TCocoaTextField.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

{ TCocoaTextView }

function TCocoaTextView.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

{ TCocoaWindow }

function TCocoaWindow.windowShouldClose(sender: id): LongBool;
var
  canClose : Boolean;
begin
  canClose:=true;
  wincallback.CloseQuery(canClose);
  Result:=canClose;
end;

procedure TCocoaWindow.windowWillClose(notification: NSNotification);
begin
  wincallback.Close;
end;

procedure TCocoaWindow.windowDidBecomeKey(notification: NSNotification);
begin
  wincallback.Activate;
end;

procedure TCocoaWindow.windowDidResignKey(notification: NSNotification);
begin
  wincallback.Deactivate;
end;

procedure TCocoaWindow.windowDidResize(notification: NSNotification);
begin
  wincallback.Resize;
end;

function TCocoaWindow.acceptsFirstResponder: Boolean;
begin
  Result:=true;
end;

procedure TCocoaWindow.mouseUp(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callback.MouseUp(round(mp.x), round(mp.y));
  inherited mouseUp(event);
end;

procedure TCocoaWindow.mouseDown(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callback.MouseDown(round(mp.x), round(mp.y));
  inherited mouseDown(event);
end;

procedure TCocoaWindow.mouseDragged(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callback.MouseMove(round(mp.x), round(mp.y));
  inherited mouseMoved(event);
end;

procedure TCocoaWindow.mouseMoved(event: NSEvent);
var
  mp : NSPoint;
begin
  mp:=event.locationInWindow;
  mp.y:=NSView(event.window.contentView).bounds.size.height-mp.y;
  callback.MouseMove(round(mp.x), round(mp.y));
  inherited mouseMoved(event);
end;

procedure TCocoaWindow.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaWindow.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

{ TCommonCallback }

constructor TCommonCallback.Create(AOwner: NSObject);
begin
  Owner:=AOwner;
end;

{ TCocoaSecureTextField }

function TCocoaSecureTextField.acceptsFirstResponder: Boolean;
begin
  Result:=True;
end;

{ TWindowCallback }

constructor TWindowCallback.Create(AOwner: NSWindow);
begin
  Owner:=AOwner;
end;

{ TCocoaCustomControl }

procedure TCocoaCustomControl.drawRect(dirtyRect:NSRect);
begin
  inherited drawRect(dirtyRect);
  callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
end;

{ LCLObjectExtension }

function LCLObjectExtension.lclIsEnabled:Boolean;
begin
  Result:=False;
end;

procedure LCLObjectExtension.lclSetEnabled(AEnabled:Boolean);
begin
end;

function LCLObjectExtension.lclIsVisible:Boolean;
begin
  Result:=False;
end;

procedure LCLObjectExtension.lclInvalidateRect(const r:TRect);
begin

end;

procedure LCLObjectExtension.lclInvalidate;
begin

end;

procedure LCLObjectExtension.lclRelativePos(var Left,Top:Integer);
begin

end;

procedure LCLObjectExtension.lclLocalToScreen(var X,Y:Integer);
begin

end;

function LCLObjectExtension.lclParent:id;
begin
  Result:=nil;
end;

function LCLObjectExtension.lclFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

procedure LCLObjectExtension.lclSetFrame(const r:TRect);
begin

end;

function LCLObjectExtension.lclClientFrame:TRect;
begin
  FillChar(Result, sizeof(Result), 0);
end;

{ LCLControlExtension }

function RectToViewCoord(view: NSView; const r: TRect): NSRect;
var
  b: NSRect;
begin
  if not Assigned(view) then Exit;
  b:=view.bounds;
  Result.origin.x:=r.Left;
  Result.origin.y:=b.size.height-r.Top;
  Result.size.width:=r.Right-r.Left;
  Result.size.height:=r.Bottom-r.Top;
end;

function LCLControlExtension.lclIsEnabled:Boolean;
begin
  Result:=IsEnabled;
end;

procedure LCLControlExtension.lclSetEnabled(AEnabled:Boolean);
begin
  SetEnabled(AEnabled);
end;

function LCLViewExtension.lclIsVisible:Boolean;
begin
  Result:=not isHidden;
end;

procedure LCLViewExtension.lclInvalidateRect(const r:TRect);
begin
  setNeedsDisplayInRect(RectToViewCoord(Self, r));
end;

procedure LCLViewExtension.lclInvalidate;
begin
  setNeedsDisplay_(True);
end;

procedure LCLViewExtension.lclLocalToScreen(var X,Y:Integer);
begin

end;

function LCLViewExtension.lclParent:id;
begin
  Result:=superView;
end;

function LCLViewExtension.lclFrame: TRect;
var
  v : NSView;
begin
  v:=superview;
  if Assigned(v)
    then NSToLCLRect(frame, v.frame.size.height, Result)
    else NSToLCLRect(frame, Result);
end;

procedure LCLViewExtension.lclSetFrame(const r:TRect);
var
  ns : NSRect;
begin
  if Assigned(superview)
    then LCLToNSRect(r, superview.frame.size.height, ns)
    else LCLToNSRect(r, ns);
  setFrame(ns);
end;

function LCLViewExtension.lclClientFrame:TRect;
var
  r: NSRect;
begin
  r:=bounds;
  Result.Left:=0;
  Result.Top:=0;
  Result.Right:=Round(r.size.width);
  Result.Bottom:=Round(r.size.height);
end;

{ LCLWindowExtension }

function LCLWindowExtension.lclIsVisible:Boolean;
begin
  Result:=isVisible;
end;

procedure LCLWindowExtension.lclInvalidateRect(const r:TRect);
begin
  contentView.lclInvalidateRect(r);
end;

procedure LCLWindowExtension.lclInvalidate;
begin
  contentView.lclInvalidate;
end;

procedure LCLWindowExtension.lclLocalToScreen(var X,Y:Integer);
var
  f   : NSRect;
begin
  if Assigned(screen) then begin
    f:=frame;
    x:=Round(f.origin.x+x);
    y:=Round(screen.frame.size.height-f.size.height-f.origin.y);
  end;
end;

function LCLWindowExtension.lclFrame:TRect;
begin
  if Assigned(screen)
    then NSToLCLRect(frame, screen.frame.size.height, Result)
    else NSToLCLRect(frame, Result);
end;

procedure LCLWindowExtension.lclSetFrame(const r:TRect);
var
  ns : NSREct;
begin
  if Assigned(screen)
    then LCLToNSRect(r, screen.frame.size.height, ns)
    else LCLToNSRect(r, ns);
  setFrame_display(ns, isVisible);
end;

function LCLWindowExtension.lclClientFrame:TRect;
var
  wr  : NSRect;
  b   : NSRect;
begin
  wr:=frame;
  b:=contentView.frame;
  Result.Left:=Round(b.origin.x);
  Result.Top:=Round(wr.size.height-b.origin.y);
  Result.Right:=Round(b.origin.x+b.size.width);
  Result.Bottom:=Round(Result.Top+b.size.height);
end;

{ TCocoaListView }

function TCocoaListView.numberOfRowsInTableView(aTableView:NSTableView): NSInteger;
begin
  if Assigned(list)
    then Result:=list.Count
    else Result:=0;
end;

function TCocoaListView.tableView_objectValueForTableColumn_row(tableView: NSTableView;
  objectValueForTableColumn: NSTableColumn; row: NSInteger):id;
begin
  if not Assigned(list) then
    Result:=nil
  else begin
    if row>=list.count then Result:=nil
    else Result:=NSStringUtf8(list[row]);
  end;
end;

{ TCocoaStringList }

procedure TCocoaStringList.Changed;
begin
  inherited Changed;
  Owner.reloadData;
end;

constructor TCocoaStringList.Create(AOwner:TCocoaListView);
begin
  Owner:=AOwner;
  inherited Create;
end;

{ TCocoaComboBoxList }

procedure TCocoaComboBoxList.Changed;
begin
  fOwner.reloadData;
  inherited Changed;
end;

constructor TCocoaComboBoxList.Create(AOwner:TCocoaComboBox);
begin
  fOwner:=AOwner;
end;

{ TCocoaComboBox }

function TCocoaComboBox.comboBox_objectValueForItemAtIndex_(combo:TCocoaComboBox;
  row: NSInteger):id;
begin
  if not Assigned(list) or (row<0) or (row>=list.Count)
    then Result:=nil
    else Result:=NSStringUtf8(list[row]);
end;

function TCocoaComboBox.numberOfItemsInComboBox(combo:TCocoaComboBox):NSInteger;
begin
  if not Assigned(list) then Result:=0
  else Result:=list.Count;
end;

procedure TCocoaComboBox.dealloc;
begin
  if Assigned(list) then begin
    list.Free;
    list:=nil;
  end;
  inherited release;
end;

{ TCocoaMenu }

procedure TCocoaMenu.lclItemSelected(sender:id);
begin

end;

{ TCocoaMenuITem }

procedure TCocoaMenuItem.lclItemSelected(sender:id);
begin

end;

end.

