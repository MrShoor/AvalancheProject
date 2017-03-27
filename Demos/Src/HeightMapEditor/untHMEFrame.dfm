object frmHMEditor: TfrmHMEditor
  Left = 0
  Top = 0
  Width = 869
  Height = 711
  TabOrder = 0
  OnMouseWheel = FrameMouseWheel
  object pnlRender: TPanel
    Left = 112
    Top = 3
    Width = 757
    Height = 705
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnMouseDown = pnlRenderMouseDown
    OnMouseMove = pnlRenderMouseMove
    OnMouseUp = pnlRenderMouseUp
  end
  object Timer1: TTimer
    Interval = 1
    OnTimer = Timer1Timer
    Left = 32
    Top = 56
  end
end
