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
{$interfaces corba}

interface

uses
  // rtl+ftl
  Types, Classes, SysUtils,
  CGGeometry,
  // Libs
  MacOSAll, CocoaAll, CocoaUtils, CocoaGDIObjects,
  // LCL
  LCLType, Controls;

type
  { ICommonCallback }

  ICommonCallback = interface
    // mouse events
    function MouseUpDownEvent(Event: NSEvent): Boolean;
    procedure MouseClick;
    function MouseMove(Event: NSEvent): Boolean;
    function KeyEvent(Event: NSEvent): Boolean;
    // size,pos events
    procedure frameDidChange;
    procedure boundsDidChange;
    // misc events
    procedure Draw(ctx: NSGraphicsContext; const bounds, dirty: NSRect);
    function ResetCursorRects: Boolean;
    procedure BecomeFirstResponder;
    procedure ResignFirstResponder;
    // non event methods
    function DeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
    function GetPropStorage: TStringList;
    function GetContext: TCocoaContext;
    function GetTarget: TObject;
    function GetHasCaret: Boolean;
    procedure SetHasCaret(AValue: Boolean);

    // properties
    property HasCaret: Boolean read GetHasCaret write SetHasCaret;
  end;

  { LCLObjectExtension }

  LCLObjectExtension = objccategory(NSObject)
    function lclIsEnabled: Boolean; message 'lclIsEnabled';
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:';
    function lclIsVisible: Boolean; message 'lclIsVisible';
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:';
    function lclWindowState: Integer; message 'lclWindowState';

    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:';
    procedure lclInvalidate; message 'lclInvalidate';
    procedure lclUpdate; message 'lclUpdate';
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::';
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::';
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::';
    function lclParent: id; message 'lclParent';
    function lclFrame: TRect; message 'lclFrame';
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:';
    function lclClientFrame: TRect; message 'lclClientFrame';
    function lclGetCallback: ICommonCallback; message 'lclGetCallback';
    function lclGetPropStorage: TStringList; message 'lclGetPropStorage';
    function lclGetTarget: TObject; message 'lclGetTarget';
    function lclDeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult; message 'lclDeliverMessage:::';
  end;

  { LCLViewExtension }

  LCLViewExtension = objccategory(NSView)
    function lclInitWithCreateParams(const AParams: TCreateParams): id; message 'lclInitWithCreateParams:';

    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:'; reintroduce;
    function lclIsPainting: Boolean; message 'lclIsPainting';
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclUpdate; message 'lclUpdate'; reintroduce;
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::'; reintroduce;
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::'; reintroduce;
    function lclParent: id; message 'lclParent'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
  end;

  NSViewFix = objccategory external (NSView)
    function fittingSize: NSSize; message 'fittingSize';
  end;

  { LCLControlExtension }

  LCLControlExtension = objccategory(NSControl)
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;
  end;

  { LCLWindowExtension }

  LCLWindowExtension = objccategory(NSWindow)
    function lclIsVisible: Boolean; message 'lclIsVisible'; reintroduce;
    procedure lclSetVisible(AVisible: Boolean); message 'lclSetVisible:'; reintroduce;
    function lclIsEnabled: Boolean; message 'lclIsEnabled'; reintroduce;
    procedure lclSetEnabled(AEnabled: Boolean); message 'lclSetEnabled:'; reintroduce;

    function lclWindowState: Integer; message 'lclWindowState'; reintroduce;
    procedure lclInvalidateRect(const r: TRect); message 'lclInvalidateRect:'; reintroduce;
    procedure lclInvalidate; message 'lclInvalidate'; reintroduce;
    procedure lclUpdate; message 'lclUpdate'; reintroduce;
    procedure lclRelativePos(var Left, Top: Integer); message 'lclRelativePos::'; reintroduce;
    procedure lclLocalToScreen(var X, Y: Integer); message 'lclLocalToScreen::'; reintroduce;
    procedure lclScreenToLocal(var X, Y: Integer); message 'lclScreenToLocal::'; reintroduce;
    function lclFrame: TRect; message 'lclFrame'; reintroduce;
    procedure lclSetFrame(const r: TRect); message 'lclSetFrame:'; reintroduce;
    function lclClientFrame: TRect; message 'lclClientFrame'; reintroduce;
  end;

  { IButtonCallback }

  IButtonCallback = interface(ICommonCallback)
    procedure ButtonClick;
  end;

  { IWindowCallback }

  IWindowCallback = interface(ICommonCallBack)
    function CanActivate: Boolean;
    procedure Activate;
    procedure Deactivate;
    procedure CloseQuery(var CanClose: Boolean);
    procedure Close;
    procedure Resize;
    procedure Move;

    function GetEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);

    property Enabled: Boolean read GetEnabled write SetEnabled;
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
    procedure boundsDidChange(sender: NSNotification); message 'boundsDidChange:';
    procedure frameDidChange(sender: NSNotification); message 'frameDidChange:';
  public
    callback: IButtonCallback;
    function initWithFrame(frameRect: NSRect): id; override;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;

    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure resetCursorRects; override;
  end;

  { TCocoaTextField }

  TCocoaTextField = objcclass(NSTextField)
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure resetCursorRects; override;
  end;

  { TCocoaSecureTextField }

  TCocoaSecureTextField = objcclass(NSSecureTextField)
  public
    callback  : ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    procedure resetCursorRects; override;
  end;


  { TCocoaTextView }

  TCocoaTextView = objcclass(NSTextView)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure resetCursorRects; override;
  end;

  { TCocoaPanel }

  TCocoaPanel = objcclass(NSPanel, NSWindowDelegateProtocol)
  protected
    function windowShouldClose(sender : id): LongBool; message 'windowShouldClose:';
    procedure windowWillClose(notification: NSNotification); message 'windowWillClose:';
    procedure windowDidBecomeKey(notification: NSNotification); message 'windowDidBecomeKey:';
    procedure windowDidResignKey(notification: NSNotification); message 'windowDidResignKey:';
    procedure windowDidResize(notification: NSNotification); message 'windowDidResize:';
    procedure windowDidMove(notification: NSNotification); message 'windowDidMove:';
  public
    callback: IWindowCallback;
    function acceptsFirstResponder: Boolean; override;
    function canBecomeKeyWindow: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    procedure sendEvent(event: NSEvent); override;
  end;

  { TCocoaCustomControl }

  TCocoaCustomControl = objcclass(NSControl)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    procedure drawRect(dirtyRect: NSRect); override;
    function lclGetCallback: ICommonCallback; override;
    // mouse
    procedure mouseDown(event: NSEvent); override;
    procedure mouseUp(event: NSEvent); override;
    procedure rightMouseDown(event: NSEvent); override;
    procedure rightMouseUp(event: NSEvent); override;
    procedure otherMouseDown(event: NSEvent); override;
    procedure otherMouseUp(event: NSEvent); override;
    procedure mouseDragged(event: NSEvent); override;
    procedure mouseEntered(event: NSEvent); override;
    procedure mouseExited(event: NSEvent); override;
    procedure mouseMoved(event: NSEvent); override;
    // key
    procedure keyDown(event: NSEvent); override;
    procedure keyUp(event: NSEvent); override;
    procedure flagsChanged(event: NSEvent); override;
    // other
    procedure resetCursorRects; override;
  end;

  { TCocoaScrollView }

  TCocoaScrollView = objcclass(NSScrollView)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure resetCursorRects; override;
  end;


  TCocoaComboBox = objcclass;

  { TCocoaComboBoxList }

  TCocoaComboBoxList = class(TStringList)
  private
    FOwner: TCocoaComboBox;
  protected
    procedure Changed; override;
  public
    constructor Create(AOwner: TCocoaComboBox);
    property Owner: TCocoaComboBox read fOwner;
  end;

  IComboboxCallBack = interface(ICommonCallBack)
    procedure ComboBoxWillPopUp;
    procedure ComboBoxWillDismiss;
    procedure ComboBoxSelectionDidChange;
    procedure ComboBoxSelectionIsChanging;
  end;

  { TCocoaComboBox }

  TCocoaComboBox = objcclass(NSComboBox, NSComboBoxDataSourceProtocol, NSComboBoxDelegateProtocol)
  public
    callback: IComboboxCallBack;
    list: TCocoaComboBoxList;
    resultNS: NSString;  //use to return values to combo
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function comboBox_objectValueForItemAtIndex_(combo: TCocoaComboBox; row: NSInteger): id; message 'comboBox:objectValueForItemAtIndex:';
    function numberOfItemsInComboBox(combo: TCocoaComboBox): NSInteger; message 'numberOfItemsInComboBox:';
    procedure dealloc; override;
    function lclGetCallback: ICommonCallback; override;
    procedure resetCursorRects; override;
    procedure comboBoxWillPopUp(notification: NSNotification); message 'comboBoxWillPopUp:';
    procedure comboBoxWillDismiss(notification: NSNotification); message 'comboBoxWillDismiss:';
    procedure comboBoxSelectionDidChange(notification: NSNotification); message 'comboBoxSelectionDidChange:';
    procedure comboBoxSelectionIsChanging(notification: NSNotification); message 'comboBoxSelectionIsChanging:';
  end;

  { TCocoaScrollBar }

  TCocoaScrollBar = objcclass(NSScroller)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure resetCursorRects; override;
  end;

  TCocoaListView = objcclass;

  { TCocoaStringList }

  TCocoaStringList = class(TStringList)
  protected
    procedure Changed; override;
  public
    Owner: TCocoaListView;
    constructor Create(AOwner: TCocoaListView);
  end;

  { TCocoaListView }

  TCocoaListView = objcclass(NSTableView, NSTableViewDataSourceProtocol)
  public
    callback: ICommonCallback;
    list: TCocoaStringList;
    resultNS: NSString;  //use to return values to combo
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    function numberOfRowsInTableView(aTableView: NSTableView): NSInteger; message 'numberOfRowsInTableView:';
    function tableView_objectValueForTableColumn_row(tableView: NSTableView;
      objectValueForTableColumn: NSTableColumn; row: NSInteger):id;
      message 'tableView:objectValueForTableColumn:row:';
    procedure dealloc; override;
    procedure resetCursorRects; override;
  end;

  { TCocoaGroupBox }

  TCocoaGroupBox = objcclass(NSBox)
  public
    callback: ICommonCallback;
    function acceptsFirstResponder: Boolean; override;
    function becomeFirstResponder: Boolean; override;
    function resignFirstResponder: Boolean; override;
    function lclGetCallback: ICommonCallback; override;
    procedure resetCursorRects; override;
  end;

procedure SetViewDefaults(AView: NSView);

implementation

procedure SetViewDefaults(AView: NSView);
begin
  if not Assigned(AView) then Exit;
  AView.setAutoresizingMask(NSViewMinYMargin or NSViewMaxXMargin);
end;

{ TCocoaPanel }

function TCocoaPanel.windowShouldClose(sender: id): LongBool;
var
  canClose: Boolean;
begin
  canClose := True;
  callback.CloseQuery(canClose);
  Result := canClose;
end;

procedure TCocoaPanel.windowWillClose(notification: NSNotification);
begin
  callback.Close;
end;

procedure TCocoaPanel.windowDidBecomeKey(notification: NSNotification);
begin
  callback.Activate;
end;

procedure TCocoaPanel.windowDidResignKey(notification: NSNotification);
begin
  callback.Deactivate;
end;

procedure TCocoaPanel.windowDidResize(notification: NSNotification);
begin
  callback.Resize;
end;

procedure TCocoaPanel.windowDidMove(notification: NSNotification);
begin
  callback.Move;
end;

function TCocoaPanel.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaPanel.canBecomeKeyWindow: Boolean;
begin
  Result := callback.CanActivate;
end;

function TCocoaPanel.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaPanel.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaPanel.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaPanel.mouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaPanel.mouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaPanel.rightMouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaPanel.rightMouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaPanel.otherMouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaPanel.otherMouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaPanel.mouseDragged(event: NSEvent);
begin
  if not callback.MouseMove(event) then
    inherited mouseDragged(event);
end;

procedure TCocoaPanel.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaPanel.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaPanel.mouseMoved(event: NSEvent);
begin
  if not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

procedure TCocoaPanel.sendEvent(event: NSEvent);
var
  Message: NSMutableDictionary;
  Handle: HWND;
  Msg: Cardinal;
  WP: WParam;
  LP: LParam;
  Result: NSNumber;
  Obj: NSObject;
begin
  if event.type_ = NSApplicationDefined then
  begin
    // event which we get through PostMessage or SendMessage
    if event.subtype = LCLEventSubTypeMessage then
    begin
      // extract message data
      Message := NSMutableDictionary(event.data1);
      Handle := NSNumber(Message.objectForKey(NSMessageWnd)).unsignedIntegerValue;
      Msg := NSNumber(Message.objectForKey(NSMessageMsg)).unsignedLongValue;
      WP := NSNumber(Message.objectForKey(NSMessageWParam)).integerValue;
      LP := NSNumber(Message.objectForKey(NSMessageLParam)).integerValue;
      // deliver message and set result
      Obj := NSObject(Handle);
      // todo: check that Obj is still a valid NSView/NSWindow
      Result := NSNumber.numberWithInteger(Obj.lclDeliverMessage(Msg, WP, LP));
      Message.setObject_forKey(Result, NSMessageResult);
      Result.release;
    end;
  end
  else
    inherited sendEvent(event);
end;

{ TCocoaScrollView }

function TCocoaScrollView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaScrollView.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaScrollView.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaScrollView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaScrollView.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaScrollBar }

function TCocoaScrollBar.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaScrollBar.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaScrollBar.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaScrollBar.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaScrollBar.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaGroupBox }

function TCocoaGroupBox.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaGroupBox.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaGroupBox.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaGroupBox.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaGroupBox.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaButton }

procedure TCocoaButton.actionButtonClick(sender: NSObject);
begin
  // this is the action handler of button
  callback.ButtonClick;
end;

procedure TCocoaButton.boundsDidChange(sender: NSNotification);
begin
  callback.boundsDidChange;
end;

procedure TCocoaButton.frameDidChange(sender: NSNotification);
begin
  callback.frameDidChange;
end;

function TCocoaButton.initWithFrame(frameRect: NSRect): id;
begin
  Result := inherited initWithFrame(frameRect);
  if Assigned(Result) then
  begin
    setTarget(Self);
    setAction(objcselector('actionButtonClick:'));
    NSNotificationCenter.defaultCenter.addObserver_selector_name_object(Self, objcselector('boundsDidChange:'), NSViewBoundsDidChangeNotification, Result);
    NSNotificationCenter.defaultCenter.addObserver_selector_name_object(Self, objcselector('frameDidChange:'), NSViewFrameDidChangeNotification, Result);
    Result.setPostsBoundsChangedNotifications(True);
    Result.setPostsFrameChangedNotifications(True);
  end;
end;

function TCocoaButton.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaButton.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaButton.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaButton.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaButton.mouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaButton.rightMouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaButton.rightMouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaButton.otherMouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaButton.otherMouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaButton.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaButton.mouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaButton.mouseDragged(event: NSEvent);
begin
  if not callback.MouseMove(event) then
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
  if not callback.MouseMove(event) then
    inherited mouseMoved(event);
end;

{ TCocoaTextField }

function TCocoaTextField.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaTextField.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaTextField.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaTextField.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextField.resetCursorRects;
begin
  // this will not work well because
  // cocoa replaced TextField and TextView cursors in
  // mouseEntered, mouseMoved and CursorUpdate
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaTextView }

function TCocoaTextView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaTextView.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaTextView.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaTextView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaTextView.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaSecureTextField }

function TCocoaSecureTextField.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaSecureTextField.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaSecureTextField.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

procedure TCocoaSecureTextField.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ TCocoaCustomControl }

function TCocoaCustomControl.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaCustomControl.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaCustomControl.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

procedure TCocoaCustomControl.drawRect(dirtyRect: NSRect);
begin
  inherited drawRect(dirtyRect);
  callback.Draw(NSGraphicsContext.currentContext, bounds, dirtyRect);
end;

function TCocoaCustomControl.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaCustomControl.mouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited mouseDown(event);
end;

procedure TCocoaCustomControl.mouseDragged(event: NSEvent);
begin
  inherited mouseDragged(event);
end;

procedure TCocoaCustomControl.mouseEntered(event: NSEvent);
begin
  inherited mouseEntered(event);
end;

procedure TCocoaCustomControl.mouseExited(event: NSEvent);
begin
  inherited mouseExited(event);
end;

procedure TCocoaCustomControl.mouseMoved(event: NSEvent);
begin
  inherited mouseMoved(event);
end;

procedure TCocoaCustomControl.keyDown(event: NSEvent);
begin
  if not callback.KeyEvent(event) then
    inherited keyDown(event);
end;

procedure TCocoaCustomControl.keyUp(event: NSEvent);
begin
  if not callback.KeyEvent(event) then
    inherited keyUp(event);
end;

procedure TCocoaCustomControl.flagsChanged(event: NSEvent);
begin
  if not callback.KeyEvent(event) then
    inherited flagsChanged(event);
end;

procedure TCocoaCustomControl.mouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited mouseUp(event);
end;

procedure TCocoaCustomControl.rightMouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited rightMouseDown(event);
end;

procedure TCocoaCustomControl.rightMouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited rightMouseUp(event);
end;

procedure TCocoaCustomControl.otherMouseDown(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited otherMouseDown(event);
end;

procedure TCocoaCustomControl.otherMouseUp(event: NSEvent);
begin
  if not callback.MouseUpDownEvent(event) then
    inherited otherMouseUp(event);
end;

procedure TCocoaCustomControl.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

{ LCLObjectExtension }

function LCLObjectExtension.lclIsEnabled: Boolean;
begin
  Result := False;
end;

procedure LCLObjectExtension.lclSetEnabled(AEnabled: Boolean);
begin
end;

function LCLObjectExtension.lclIsVisible: Boolean;
begin
  Result := False;
end;

procedure LCLObjectExtension.lclSetVisible(AVisible: Boolean);
begin
end;

function LCLObjectExtension.lclWindowState: Integer;
begin
  Result := SIZE_RESTORED;
end;

procedure LCLObjectExtension.lclInvalidateRect(const r: TRect);
begin
end;

procedure LCLObjectExtension.lclInvalidate;
begin
end;

procedure LCLObjectExtension.lclUpdate;
begin
end;

procedure LCLObjectExtension.lclRelativePos(var Left,Top: Integer);
begin
end;

procedure LCLObjectExtension.lclLocalToScreen(var X,Y: Integer);
begin
end;

procedure LCLObjectExtension.lclScreenToLocal(var X, Y: Integer);
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

function LCLObjectExtension.lclGetCallback: ICommonCallback;
begin
  Result := nil;
end;

function LCLObjectExtension.lclGetPropStorage: TStringList;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.GetPropStorage
  else
    Result := nil;
end;

function LCLObjectExtension.lclGetTarget: TObject;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.GetTarget
  else
    Result := nil;
end;

function LCLObjectExtension.lclDeliverMessage(Msg: Cardinal; WParam: WParam; LParam: LParam): LResult;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := Callback.DeliverMessage(Msg, WParam, LParam)
  else
    Result := 0;
end;

{ LCLControlExtension }

function RectToViewCoord(view: NSView; const r: TRect): NSRect;
var
  b: NSRect;
begin
  if not Assigned(view) then Exit;
  b := view.bounds;
  with r do
  begin
    Result.origin.x := Left;
    Result.origin.y := b.size.height - Bottom;
    Result.size.width := Right - Left;
    Result.size.height := Bottom - Top;
  end;
end;

function LCLControlExtension.lclIsEnabled:Boolean;
begin
  Result := IsEnabled;
end;

procedure LCLControlExtension.lclSetEnabled(AEnabled:Boolean);
begin
  SetEnabled(AEnabled);
end;

function LCLViewExtension.lclInitWithCreateParams(const AParams: TCreateParams): id;
var
  p: NSView;
  ns: NSRect;
begin
  p := nil;
  setHidden(AParams.Style and WS_VISIBLE = 0);
  if (AParams.WndParent <> 0) then
  begin
    if (NSObject(AParams.WndParent).isKindOfClass_(NSView)) then
      p := NSView(AParams.WndParent)
    else
    if (NSObject(AParams.WndParent).isKindOfClass_(NSWindow)) then
      p := NSWindow(AParams.WndParent).contentView;
  end;
  with AParams do
    if Assigned(p) then
      LCLToNSRect(Types.Bounds(X,Y,Width, Height), p.frame.size.height, ns)
    else
      LCLToNSRect(Types.Bounds(X,Y,Width, Height), ns);

  Result := initWithFrame(ns);
  if not Assigned(Result) then
    Exit;

  if Assigned(p) then
    p.addSubview(Self);
  SetViewDefaults(Self);
end;

function LCLViewExtension.lclIsVisible: Boolean;
begin
  Result := not isHidden;
end;

procedure LCLViewExtension.lclSetVisible(AVisible: Boolean);
begin
  setHidden(not AVisible);
end;

function LCLViewExtension.lclIsPainting: Boolean;
begin
  Result := Assigned(lclGetCallback) and Assigned(lclGetCallback.GetContext);
end;

procedure LCLViewExtension.lclInvalidateRect(const r:TRect);
begin
  setNeedsDisplayInRect(RectToViewCoord(Self, r));
end;

procedure LCLViewExtension.lclInvalidate;
begin
  setNeedsDisplay_(True);
end;

procedure LCLViewExtension.lclUpdate;
begin
  display;
end;

procedure LCLViewExtension.lclRelativePos(var Left, Top: Integer);
begin
  with frame.origin do
  begin
    Left := Round(x);
    Top := Round(y);
  end;
end;

procedure LCLViewExtension.lclLocalToScreen(var X, Y:Integer);
var
  P: NSPoint;
begin
  // 1. convert to window base
  P.x := X;
  P.y := Y;
  P := convertPoint_ToView(P, nil);
  // 2. convert to screen
  with window.frame.origin do
  begin
    P.x := P.x + x;
    P.y := P.y + y;
  end;
  X := Round(P.x);
  Y := Round(window.screen.frame.size.height - P.y);
end;

procedure LCLViewExtension.lclScreenToLocal(var X, Y: Integer);
var
  P: NSPoint;
begin
  // 1. convert from screen to window
  P.x := X;
  P.y := window.screen.frame.size.height - Y;
  with window.frame.origin do
  begin
    P.x := P.x - x;
    P.y := P.y - y;
  end;
  // 2. convert from window to local
  P := convertPoint_FromView(P, nil);
  X := Round(P.x);
  Y := Round(P.y);
end;

function LCLViewExtension.lclParent:id;
begin
  Result := superView;
end;

function LCLViewExtension.lclFrame: TRect;
var
  v : NSView;
begin
  v := superview;
  if Assigned(v) then
    NSToLCLRect(frame, v.frame.size.height, Result)
  else
    NSToLCLRect(frame, Result);
end;

procedure LCLViewExtension.lclSetFrame(const r: TRect);
var
  ns : NSRect;
begin
  if Assigned(superview)  then
    LCLToNSRect(r, superview.frame.size.height, ns)
  else
    LCLToNSRect(r, ns);
  setFrame(ns);
end;

function LCLViewExtension.lclClientFrame: TRect;
var
  r: NSRect;
begin
  r := bounds;
  with Result do
  begin
    Left := 0;
    Top := 0;
    Right := Round(r.size.width);
    Bottom := Round(r.size.height);
  end;
end;

{ LCLWindowExtension }

function LCLWindowExtension.lclIsVisible: Boolean;
begin
  Result := isVisible;
end;

procedure LCLWindowExtension.lclSetVisible(AVisible: Boolean);
begin
  if AVisible then
    orderFrontRegardless
  else
    orderOut(nil);
end;

function LCLWindowExtension.lclIsEnabled: Boolean;
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    Result := IWindowCallback(Callback).Enabled
  else
    Result := contentView.lclIsEnabled;
end;

procedure LCLWindowExtension.lclSetEnabled(AEnabled: Boolean);
var
  Callback: ICommonCallback;
begin
  Callback := lclGetCallback;
  if Assigned(Callback) then
    IWindowCallback(Callback).Enabled := AEnabled
  else
    contentView.lclSetEnabled(AEnabled);
end;

function LCLWindowExtension.lclWindowState: Integer;
const
  NSFullScreenWindowMask = 1 shl 14;
begin
  if isMiniaturized then
    Result := SIZE_MINIMIZED
  else
  if (styleMask and NSFullScreenWindowMask) <> 0 then
    Result := SIZE_FULLSCREEN
  else
  if isZoomed then
    Result := SIZE_MAXIMIZED
  else
    Result := SIZE_RESTORED;
end;

procedure LCLWindowExtension.lclInvalidateRect(const r: TRect);
begin
  contentView.lclInvalidateRect(r);
end;

procedure LCLWindowExtension.lclInvalidate;
begin
  contentView.lclInvalidate;
end;

procedure LCLWindowExtension.lclUpdate;
begin
  contentView.lclUpdate;
end;

procedure LCLWindowExtension.lclRelativePos(var Left, Top: Integer);
begin
  with frame.origin do
  begin
    Left := Round(x);
    Top := Round(y);
  end;
end;

procedure LCLWindowExtension.lclLocalToScreen(var X, Y:Integer);
var
  f: NSRect;
begin
  if Assigned(screen) then
  begin
    f := frame;
    inc(X, Round(f.origin.x));
    inc(Y, Round(screen.frame.size.height - f.size.height - f.origin.y));
  end;
end;

procedure LCLWindowExtension.lclScreenToLocal(var X, Y: Integer);
var
  f: NSRect;
begin
  if Assigned(screen) then
  begin
    f := frame;
    dec(X, Round(f.origin.x));
    dec(Y, Round(screen.frame.size.height - f.size.height - f.origin.y));
  end;
end;

function LCLWindowExtension.lclFrame:TRect;
begin
  if Assigned(screen) then
    NSToLCLRect(frame, screen.frame.size.height, Result)
  else
    NSToLCLRect(frame, Result);
end;

procedure LCLWindowExtension.lclSetFrame(const r:TRect);
var
  ns: NSREct;
begin
  if Assigned(screen) then
    LCLToNSRect(r, screen.frame.size.height, ns)
  else
    LCLToNSRect(r, ns);
  setFrame_display(ns, isVisible);
end;

function LCLWindowExtension.lclClientFrame: TRect;
var
  wFrame, cFrame: NSRect;
begin
  wFrame := frame;
  cFrame := contentRectForFrameRect(wFrame);
  with Result do
  begin
    Left := Round(cFrame.origin.x - wFrame.origin.x);
    Top := Round(wFrame.origin.y + wFrame.size.height - cFrame.origin.y - cFrame.size.height);
    Right := Left + Round(cFrame.size.width);
    Bottom := Top + Round(cFrame.size.height);
  end;
end;

{ TCocoaListView }

function TCocoaListView.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaListView.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaListView.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

function TCocoaListView.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

function TCocoaListView.numberOfRowsInTableView(aTableView:NSTableView): NSInteger;
begin
  if Assigned(list) then
    Result := list.Count
  else
    Result := 0;
end;

function TCocoaListView.tableView_objectValueForTableColumn_row(tableView: NSTableView;
  objectValueForTableColumn: NSTableColumn; row: NSInteger):id;
begin
  if not Assigned(list) then
    Result:=nil
  else begin
    if row>=list.count then Result:=nil
    else
    begin
      resultNS.release;  //so we can reuse it
      resultNS := NSStringUtf8(list[row]);
      Result:= ResultNS;
    end;
  end;
end;

procedure TCocoaListView.dealloc;
begin
  FreeAndNil(list);
  resultNS.release;
  inherited dealloc;
end;

procedure TCocoaListView.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
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

function TCocoaComboBox.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TCocoaComboBox.becomeFirstResponder: Boolean;
begin
  Result := inherited becomeFirstResponder;
  callback.BecomeFirstResponder;
end;

function TCocoaComboBox.resignFirstResponder: Boolean;
begin
  Result := inherited resignFirstResponder;
  callback.ResignFirstResponder;
end;

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
  if Assigned(list) then
  begin
    list.Free;
    list:=nil;
  end;
  resultNS.release;
  inherited dealloc;
end;

function TCocoaComboBox.lclGetCallback: ICommonCallback;
begin
  Result := callback;
end;

procedure TCocoaComboBox.resetCursorRects;
begin
  if not callback.resetCursorRects then
    inherited resetCursorRects;
end;

procedure TCocoaComboBox.comboBoxWillPopUp(notification: NSNotification);
begin
  callback.ComboBoxWillPopUp;
end;

procedure TCocoaComboBox.comboBoxWillDismiss(notification: NSNotification);
begin
  callback.ComboBoxWillDismiss;
end;

procedure TCocoaComboBox.comboboxSelectionDidChange(notification: NSNotification);
begin
  callback.ComboBoxSelectionDidChange;
end;

procedure TCocoaComboBox.comboBoxSelectionIsChanging(notification: NSNotification);
begin
  callback.ComboBoxSelectionIsChanging;
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

