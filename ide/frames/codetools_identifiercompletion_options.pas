{
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
}
unit codetools_identifiercompletion_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, StdCtrls,
  CodeToolsOptions, LazarusIDEStrConsts, IDEOptionsIntf;

type

  { TCodetoolsIndentifierComplietionOptionsFrame }

  TCodetoolsIndentifierComplietionOptionsFrame = class(TAbstractIDEOptionsEditor)
    ICAutoAddParameterBrackets: TCheckBox;
    ICAutoStartAfterPointCheckBox: TCheckBox;
    ICAddAssignOperatorCheckBox: TCheckBox;
    ICAddSemicolonCheckBox: TCheckBox;
  private
    { private declarations }
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TCodetoolsIndentifierComplietionOptionsFrame }

function TCodetoolsIndentifierComplietionOptionsFrame.GetTitle: String;
begin
  Result := dlgIdentifierCompletion;
end;

procedure TCodetoolsIndentifierComplietionOptionsFrame.Setup(
  ADialog: TAbstractOptionsEditorDialog);
begin
  ICAddSemicolonCheckBox.Caption:=dlgAddSemicolon;
  ICAddAssignOperatorCheckBox.Caption:=dlgAddAssignmentOperator;
  ICAutoStartAfterPointCheckBox.Caption:=lisAutomaticallyInvokeAfterPoint;
  ICAutoAddParameterBrackets.Caption:=lisAddParameterBrackets;
end;

procedure TCodetoolsIndentifierComplietionOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeToolsOptions do
  begin
    ICAddSemicolonCheckBox.Checked := IdentComplAddSemicolon;
    ICAddAssignOperatorCheckBox.Checked := IdentComplAddAssignOperator;
    ICAutoStartAfterPointCheckBox.Checked := IdentComplAutoStartAfterPoint;
    ICAutoAddParameterBrackets.Checked:=IdentComplAddParameterBrackets;
  end;
end;

procedure TCodetoolsIndentifierComplietionOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TCodeToolsOptions do
  begin
    IdentComplAddSemicolon := ICAddSemicolonCheckBox.Checked;
    IdentComplAddAssignOperator := ICAddAssignOperatorCheckBox.Checked;
    IdentComplAutoStartAfterPoint := ICAutoStartAfterPointCheckBox.Checked;
    IdentComplAddParameterBrackets:=ICAutoAddParameterBrackets.Checked;
  end;
end;

class function TCodetoolsIndentifierComplietionOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TCodeToolsOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCodetools, TCodetoolsIndentifierComplietionOptionsFrame, CdtOptionsIdentCompletion);
end.

