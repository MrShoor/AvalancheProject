object frmMain: TfrmMain
  Left = 585
  Top = 234
  Caption = 'OpenGL'
  ClientHeight = 510
  ClientWidth = 771
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 8
    Top = 32
    Width = 88
    Height = 25
    Caption = 'Switch GAPI'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 8
    Top = 1
    Width = 89
    Height = 25
    ParentBackground = False
    TabOrder = 1
    object cbSorted: TCheckBox
      Left = 9
      Top = 2
      Width = 67
      Height = 19
      Caption = 'cbSorted'
      TabOrder = 0
      OnClick = cbSortedChange
    end
  end
end
