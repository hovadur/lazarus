{ ToDo: move to lazcontrols }
unit GenericCheckList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel, CheckLst, LazarusIDEStrConsts, Buttons;

type

  { TGenericCheckListForm }

  TGenericCheckListForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
    procedure FormCreate(Sender: TObject);
  private

  public

  end; 

var
  GenericCheckListForm: TGenericCheckListForm;

implementation

{$R *.lfm}

{ TGenericCheckListForm }

procedure TGenericCheckListForm.FormCreate(Sender: TObject);
var
  BitButtonYes: TBitBtn;
begin
  ButtonPanel1.OKButton.Caption:=lisMenuOk;
  ButtonPanel1.CancelButton.Caption:=lisCancel;

  // save and compile
  BitButtonYes:=TBitBtn.Create(ButtonPanel1);
  BitButtonYes.Kind:=bkCustom;
  BitButtonYes.ModalResult:=mrYes;
  BitButtonYes.Caption:=lisBuild;
  BitButtonYes.Align:=alRight;
  BitButtonYes.Parent:=ButtonPanel1;
end;

end.

