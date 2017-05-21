object frmMain: TfrmMain
  Left = 446
  Top = 96
  Caption = 'frmMain'
  ClientHeight = 728
  ClientWidth = 1044
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1044
    728)
  PixelsPerInch = 96
  TextHeight = 13
  object RenderPanel: TPanel
    Left = 186
    Top = 0
    Width = 858
    Height = 728
    Anchors = [akLeft, akTop, akRight, akBottom]
    BevelOuter = bvNone
    TabOrder = 0
    OnMouseDown = RenderPanelMouseDown
    OnMouseMove = RenderPanelMouseMove
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 186
    Height = 728
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 1
    DesignSize = (
      186
      728)
    object Label1: TLabel
      Left = 8
      Top = 96
      Width = 41
      Height = 13
      Caption = 'Objects:'
      Color = clBtnFace
      ParentColor = False
    end
    object Label2: TLabel
      Left = 10
      Top = 344
      Width = 52
      Height = 13
      Caption = 'Animations'
      Color = clBtnFace
      ParentColor = False
    end
    object cbDirectX11: TRadioButton
      Left = 8
      Top = 8
      Width = 73
      Height = 19
      Caption = 'DirectX 11'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object cbOGL: TRadioButton
      Left = 8
      Top = 32
      Width = 63
      Height = 19
      Caption = 'OpenGL'
      Enabled = False
      TabOrder = 1
    end
    object cbWireframe: TCheckBox
      Left = 8
      Top = 64
      Width = 75
      Height = 19
      Caption = 'Wireframe'
      TabOrder = 2
    end
    object lbNames: TListBox
      Left = 0
      Top = 120
      Width = 184
      Height = 216
      ItemHeight = 13
      MultiSelect = True
      TabOrder = 3
    end
    object lbAnimations: TListBox
      Left = 0
      Top = 368
      Width = 184
      Height = 352
      Anchors = [akLeft, akTop, akBottom]
      ItemHeight = 13
      TabOrder = 4
    end
    object btnLoad: TButton
      Left = 88
      Top = 2
      Width = 96
      Height = 25
      Caption = 'Add from file'
      TabOrder = 5
      OnClick = btnLoadClick
    end
    object btnClear: TButton
      Left = 88
      Top = 32
      Width = 96
      Height = 25
      Caption = 'Clear'
      TabOrder = 6
      OnClick = btnClearClick
    end
    object btnFit: TButton
      Left = 89
      Top = 61
      Width = 96
      Height = 25
      Caption = 'Fit scene'
      TabOrder = 7
      OnClick = btnFitClick
    end
  end
  object ApplicationProperties1: TApplicationEvents
    OnIdle = ApplicationProperties1Idle
    Left = 72
    Top = 184
  end
  object OpenDialog: TOpenDialog
    Filter = '*.dat;*.avm|*.dat;*.avm'
    Left = 64
    Top = 128
  end
end
