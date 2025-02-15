unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.UITypes, System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, ExtendedClipboard;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Button2: TButton;
    Button3: TButton;
    Label5: TLabel;
    Button4: TButton;
    ClipboardExt1: TClipboardExt;
    procedure Button1Click(Sender: TObject);
    procedure ClipboardExt1MonitorClipboardStatus(Sender: TObject;
      var monitorStatus: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure ClipboardExt1DetectClipboardData(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ClipboardExt1EmptyClipboardNotify(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  Form1               :TForm1;

  clipbrdExtension    :TClipbrdExtension;

implementation

{$R *.dfm}

// #############################################################################
//
// Enable clipboard monitor
//
// #############################################################################
procedure TForm1.Button1Click(Sender: TObject);
begin
 // Set "command" to enable clipboard monitor
 clipbrdExtension.ecbCommand := EnableMonitor;

 // "Send" command to "ExtendedClipboard" component
 ClipboardExt1.ClipboardExtension := clipbrdExtension;
end;

// #############################################################################
//
// Copy data to clipboard
//
// #############################################################################
procedure TForm1.Button2Click(Sender: TObject);
begin
 If Memo1.Lines.Count > 0 Then
  Begin
   // ecbCommand: set "command" to copy data to the clipboard
   // ClipboardType: set to "Unicodetext"
   // cbText: the text
   clipbrdExtension.ecbCommand := CopyToClipboard;
   clipbrdExtension.ClipboardType := CF_UNICODETEXT;
   clipbrdExtension.cbText := Memo1.Text;

   // "Send" command to "ExtendedClipboard" component
   ClipboardExt1.ClipboardExtension := clipbrdExtension;
  End;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
 If Image1.Picture <> NIL Then
  Begin
   // ecbCommand: set "command" to copy data to the clipboard
   // ClipboardType: set to "Unicodetext"
   // cbPicture: the picture
   clipbrdExtension.ecbCommand := CopyToClipboard;
   clipbrdExtension.ClipboardType := CF_BITMAP;
   clipbrdExtension.cbPicture := Image1.Picture;

   ClipboardExt1.ClipboardExtension := clipbrdExtension;
  End;
end;

// #############################################################################
//
// Clear the clipboard
//
// #############################################################################
procedure TForm1.Button4Click(Sender: TObject);
begin
 // Set "command" to clear the clipboard
 clipbrdExtension.ecbCommand := ClearClipboard;

 // "Send" command to "ExtendedClipboard" component
 ClipboardExt1.ClipboardExtension := clipbrdExtension;
end;

// #############################################################################
//
// Event to detect data in the clipboard
//
// #############################################################################
procedure TForm1.ClipboardExt1DetectClipboardData(Sender: TObject);
begin
 // IMPORTANT: you MUST(!) set "clipbrdExtension" to "ClipboardExtension"
 // to get the current results.
 clipbrdExtension := ClipboardExt1.ClipboardExtension;

 // Check if "ClipboardType" is an Unicodetext
 If clipbrdExtension.ClipboardType = CF_UNICODETEXT Then
  Begin
   // Add the text to the "Memo" control
   Memo1.Lines.Add(clipbrdExtension.cbText);
  End;

 // Check if "ClipboardType" is a bitmap
 If clipbrdExtension.ClipboardType = CF_BITMAP Then
  Begin
   // Set the bitmap to the "Image1" control
   Image1.Picture := clipbrdExtension.cbPicture;
  End;
end;

// #############################################################################
//
// Event to detect if clipboard is clear (after "Button4" is clicked)
//
// #############################################################################
procedure TForm1.ClipboardExt1EmptyClipboardNotify(Sender: TObject);
begin
 MessageDlg('Clipboard is empty',mtInformation,[mbOK],0);
end;

// #############################################################################
//
// Event to return status of clipboard monitoring
//
// #############################################################################
procedure TForm1.ClipboardExt1MonitorClipboardStatus(Sender: TObject;
  var monitorStatus: Boolean);
begin
 If monitorStatus = False Then
  Begin
   Memo1.Lines.Add('Monitor clipboard is off.');
  End;
end;

end.
