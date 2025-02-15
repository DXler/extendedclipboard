object Form1: TForm1
  Left = 0
  Top = 0
  Caption = ' ExtendedClipboard component demo.'
  ClientHeight = 701
  ClientWidth = 851
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 835
    Height = 181
    AutoSize = False
    Caption = 
      'The "ClipboardExt" component can auto handle text and different ' +
      'graphic formats (e.g. Bitmap, PNG).'#13#10#13#10'With the "Start monitor c' +
      'lipboard" you can use 2 modes:'#13#10#13#10'Mode #1: if you click on "Star' +
      't monitor clipboard" button with an empty clipboard the componen' +
      't monitor the clipboard and if data available in the clipboard t' +
      'hen autodetect the type of data,'#13#10#13#10'Mode #2: if data in clipboar' +
      'd available and you click on "Start monitor clipboard" button th' +
      'e component autodetect the type of the available data.'#13#10#13#10'The de' +
      'veloper can directly assign the data to right control (see "OnDe' +
      'tectClipboardData" event in the demo).'#13#10#13#10'You can see the Mode #' +
      '1 as a kind of "Auto Wait" or "Auto Import" and Mode #2 is a "Ma' +
      'nual Insert" mode.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label4: TLabel
    Left = 8
    Top = 274
    Width = 835
    Height = 35
    AutoSize = False
    Caption = 
      'To use the demo program you can try to copy text or graphics fro' +
      'm different sources (e.g. web browser, different tools that can ' +
      'show pictures and so on).      Let your imagine free!    ;-)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label2: TLabel
    Left = 222
    Top = 230
    Width = 314
    Height = 16
    Caption = 'ATTENTION: the clipboard is immediately cleared!'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label3: TLabel
    Left = 335
    Top = 204
    Width = 200
    Height = 16
    Caption = '( Normally this is not needed! )'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label5: TLabel
    Left = 385
    Top = 335
    Width = 259
    Height = 64
    Caption = 
      '1) Insert picture from clipboard'#13#10'2) Clear clipboard'#13#10'3) Click o' +
      'n button to copy picture to clipboard'#13#10'4) Insert picture in othe' +
      'r application'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 8
    Top = 402
    Width = 367
    Height = 368
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 201
    Width = 141
    Height = 25
    Caption = 'Start monitor clipboard'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Panel1: TPanel
    Left = 385
    Top = 402
    Width = 458
    Height = 368
    Caption = 'Pictures will show here.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object Image1: TImage
      Left = 0
      Top = 0
      Width = 458
      Height = 368
      Proportional = True
    end
  end
  object Button2: TButton
    Left = 8
    Top = 374
    Width = 179
    Height = 25
    Caption = 'Copy the bottom text to clipboard'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 671
    Top = 371
    Width = 141
    Height = 25
    Caption = 'Copy picture to clipboard'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 222
    Top = 201
    Width = 105
    Height = 25
    Caption = 'Clear clipboard'
    TabOrder = 5
    OnClick = Button4Click
  end
  object ClipboardExt1: TClipboardExt
    Left = 4
    Top = 324
  end
end
