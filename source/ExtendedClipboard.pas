unit ExtendedClipboard;

interface

Uses
  Winapi.Windows, System.SysUtils, System.StrUtils, System.Classes, System.TypInfo,
  System.UITypes, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Graphics, Vcl.Imaging.pngimage,
  Vcl.Imaging.pnglang, Vcl.Clipbrd;


// #############################################################################
// Für die Erstellung der Anleitung auskommentieren!

Const
  clipbrdTypes :Array[1..18] Of String = ('CF_TEXT','CF_BITMAP','CF_METAFILEPICT',
                                          'CF_SYLK','CF_DIF','CF_TIFF','CF_OEMTEXT',
                                          'CF_DIB','CF_PALETTE','CF_PENDATA',
                                          'CF_RIFF','CF_WAVE','CF_UNICODETEXT',
                                          'CF_ENHMETAFILE','CF_HDROP','CF_LOCALE',
                                          'CF_DIBV5','CF_MAX');

// #############################################################################


Type
  /// <summary>
  ///   Enumeration of all possible clipboard types (declared in the
  ///   "Winapi.Windows" unit). <br /><br />Notice: not all clipboard types are
  ///   available (this can changed in future versions).
  /// </summary>
  TClipboardTypes = (CF_TEXT,CF_BITMAP,CF_METAFILEPICT,CF_SYLK,CF_DIF,CF_TIFF,
                     CF_OEMTEXT,CF_DIB,CF_PALETTE,CF_PENDATA,CF_RIFF,CF_WAVE,
                     CF_UNICODETEXT,CF_ENHMETAFILE,CF_HDROP,CF_LOCALE,CF_DIBV5,
                     CF_MAX);


// #############################################################################
// Für die Erstellung der Anleitung auskommentieren!

Type
  TClprdCommands = (ClearClipboard,CopyToClipboard,EnableMonitor,DisableMonitor);

// #############################################################################


Type
// #############################################################################
// Für die Erstellung der Komponente auskommentieren!

 /// <summary>
 ///   This function tell the "Extended Clipboard" component which command
 ///   should be executed.
 /// </summary>
 /// <remarks>
 ///   CopyToClipboard: <br /><br />The property
 ///   "ClipboardType"(TClipboardTypes) tells the component which type the data
 ///   have to write to clipboard. <br /><br />This property is in
 ///   TClipbrdExtension. <br /><br />The variables "cbText" (string) und
 ///   "cbPicture" (TPicture) which are declared in TClipbrdExtension contains
 ///   the data which should be write to clipboard. <br />
 /// </remarks>
 /// <example>
 ///   <para>
 ///     // The component (TClipboardExt) is in this example declared as
 ///     "ClipboardExt1".
 ///   </para>
 ///   <para>
 ///     Var myClipbrdExtension :TClipbrdExtension;
 ///   </para>
 ///   <para>
 ///     ....
 ///   </para>
 ///   <para>
 ///     myClipbrdExtension.ecbCommand := CopyToClipboard;
 ///   </para>
 ///   <para>
 ///     myClipbrdExtension.ClipboardType := CF_UNICODETEXT;
 ///   </para>
 ///   <para>
 ///     myClipbrdExtension.cbText := 'This is a little clipbard test.';
 ///   </para>
 ///   <para>
 ///     ClipbardExt1.ClipboardExtension := myClipbrdExtension;
 ///   </para>
 /// </example>

// #############################################################################
// Für die Erstellung der Komponente auskommentieren!
{
 ecbCommand =(
   /// <summary>
   ///   Clears the clipboard.
   /// </summary>
   ClearClipboard,
   /// <summary>
   ///   Copies the given data to the clipboard.
   /// </summary>
   CopyToClipboard,
   /// <summary>
   ///   Enabled the clipboard monitor.
   /// </summary>
   EnableMonitor,
   /// <summary>
   ///   Disabling the clipboard monitor.
   /// </summary>
   DisableMonitor)
}
// #############################################################################


// #############################################################################
// Für die Erstellung der Anleitung auskommentieren!

  TClipbrdExtension = Record
   Public
    ecbCommand         :TClprdCommands;

    // Properties
    ClipboardType      :TClipboardTypes;

    // Results
    cbText             :String;
    cbPicture          :TPicture;
  End;

// #############################################################################


// #############################################################################
// Für die Erstellung der Anleitung auskommentieren!

Type
  TDetectClipboardData = Procedure(Sender :TObject) Of Object;
  TMonitorClipboardStatus = Procedure(Sender :TObject; Var monitorStatus :Boolean) Of Object;
  TEmptyClipboardNotify = Procedure(Sender :TObject) Of Object;

// #############################################################################


Type
  /// <summary>
  ///   The "ExtendedClipboard" component help to handle the clipboard at a
  ///   more effective way with useful addition features and to make it easier.
  /// </summary>
  TClipboardExt = Class(TComponent)
    Private
     FInternTimer                :TTimer;

     // Record intern variables
     indexValueResult                          :String;
     searchIndexResult                         :Integer;

     Procedure InternTimerEvent(Sender :TObject);

     Function GetIndexFromStringList(searchNameForIndex :String) :Integer;
     Procedure GetClipbrdData(formatTypeIndex :Integer; Var clipbrdDataType :String);
     Function GetClipboardType(clipboardType :String) :TClipboardTypes;
    Protected
     FMonitorClipBoard           :Boolean;
     FClipboardExtension         :TClipbrdExtension;

     FOnDetectClipbrdData        :TDetectClipboardData;
     FOnMonitorClipboardStatus   :TMonitorClipboardStatus;
     FOnEmptyClipboardNotify      :TEmptyClipboardNotify;

     Procedure SetClipboardExtension(Const Value :TClipbrdExtension);

     Procedure ClipboardDataDetected;
     Procedure ClipboardMonitorStatus(Var monitorStatus :Boolean);
     Procedure ClipboardEmptyNotify;
    Public
     /// <summary>
     ///   This is the "gateway" between the component and the developer in
     ///   which the developer can simply set commands, possible parameters and
     ///   can get at an easy way the results. <br /><br />For detailed
     ///   information: please read the "EasyCommands_and_more_with_records"
     ///   article. <br />
     /// </summary>
     /// <remarks>
     ///   To set a command and (possible) parameters simply assign the needed
     ///   values to the corresponding variables in a local declaration of the
     ///   TClipbrdExtension and assign at last the local variable to the
     ///   "ClipboardExtension" property. <br /><br />To reading the result (or
     ///   more results) after setting and "execution" a command simply assign
     ///   the "ClipboardExtension" property to the local declaration of the
     ///   TClipbrdExtension.
     /// </remarks>
     Property ClipboardExtension :TClipbrdExtension Read FClipboardExtension Write SetClipboardExtension;
    Published
// #############################################################################
// Für die Erstellung der Anleitung auskommentieren!

     Constructor Create(AOwner :TComponent); Override;

// #############################################################################

     /// <summary>
     ///   <para>
     ///     This is the "OnDetectClipboardData" event.
     ///   </para>
     ///   <para>
     ///     It is fired if the component detects data in the clipboard.
     ///   </para>
     ///   <para>
     ///     To read the data: see the above example.
     ///   </para>
     /// </summary>
     /// <example>
     ///   <code lang="Delphi">// The component (TClipboardExt) is in this example declared as "ClipboardExt1".
     ///
     /// Var clipbrdExtension :TClipbrdExtension;
     /// ....
     /// // #############################################################################
     /// //
     /// // Event to detect data in the clipboard
     /// //
     /// // #############################################################################
     /// procedure TForm1.ClipboardExt1DetectClipboardData(Sender: TObject);
     /// begin
     ///  // IMPORTANT: you MUST(!) set "clipbrdExtension" to "ClipboardExtension" to get the current results.
     ///  clipbrdExtension := ClipboardExt1.ClipboardExtension;
     ///
     ///  // Check if "ClipboardType" is an Unicodetext
     ///  If clipbrdExtension.ClipboardType = CF_UNICODETEXT Then
     ///   Begin
     ///    // Add the text to the "Memo" control
     ///    Memo1.Lines.Add(clipbrdExtension.cbText);
     ///   End;
     ///
     ///  // Check if "ClipboardType" is a bitmap
     ///  If clipbrdExtension.ClipboardType = CF_BITMAP Then
     ///   Begin
     ///    // Set the bitmap to the "Image1" control
     ///    Image1.Picture := clipbrdExtension.cbPicture;
     ///   End;
     /// end;
     ///   </code>
     /// </example>
     Property OnDetectClipboardData :TDetectClipboardData Read FOnDetectClipbrdData Write FOnDetectClipbrdData;
     /// <summary>
     ///   This is the "OnMonitorClipboardStatus" event. <br /><br />It is
     ///   fired if monitoring of the clipboard is finished and the
     ///   "monitorStatus" was set to "False".
     /// </summary>
     /// <value>
     /// <value>
     /// </value>
     ///   monitorStatus = Boolean
     /// </value>
     Property OnMonitorClipboardStatus :TMonitorClipboardStatus Read FOnMonitorClipboardStatus Write FOnMonitorClipboardStatus;
     /// <summary>
     ///   <para>
     ///     This is the "OnEmptyClipboardNotify" event. <br /><br />It is
     ///     fired if the clipboard is cleared (by the user) and if a other
     ///     program clear the clipboard while the "ExtendedClipboard"
     ///     component will be monitoring it. <br />
     ///   </para>
     ///   <para>
     ///     This event does not return a value. <br />
     ///   </para>
     /// </summary>
     Property OnEmptyClipboardNotify :TEmptyClipboardNotify Read FOnEmptyClipboardNotify Write FOnEmptyClipboardNotify;
  end;

// #############################################################################
// Für die Erstellung der Anleitung auskommentieren!

Procedure Register;

// #############################################################################

implementation

Var actualClipboardFormts                     :TStringList;

    clipbrdHandle                             :THandle;

    actualClipBrdString,clipbrdFormatName     :String;

    bmpPalette                                :HPALETTE;

    dataFormat                                :Word;

 //{$DEFINE demo}

 {$IFDEF demo}
    CRLF                                      :String;
 {$ENDIF}

Constructor TClipboardExt.Create(AOwner :TComponent);
begin
 Inherited Create(AOwner);

 FInternTimer := TTimer.Create(Self);

 FInternTimer.OnTimer := InternTimerEvent;

 {$IFDEF VER310}
 FInternTimer.Interval := 50;
 {$ENDIF}

 {$IFDEF VER320}
 FInternTimer.Interval := 100;
 {$ENDIF}

 {$IFDEF VER330}
 FInternTimer.Interval := 100;
 {$ENDIF}

 {$IFDEF VER340}
 FInternTimer.Interval := 100;
 {$ENDIF}

 FInternTimer.Enabled := False;

 actualClipboardFormts := TStringList.Create;

 {$IFDEF demo}
 CRLF := Chr(13) + Chr(10);

 MessageDlg('This version is for demonstration purposes only!' + CRLF +
            'Distribution is not allowed.' + CRLF + CRLF +
            'Development and (c) by neXt generation Software in 2021 - 2025.',mtInformation,[mbOK],0);
 {$ENDIF}
end;

Procedure TClipboardExt.SetClipboardExtension(Const Value :TClipbrdExtension);
begin
 FClipboardExtension := Value;

 If FClipboardExtension.ecbCommand = ClearClipboard Then
  Begin
   Clipboard.Open;
    Clipboard.Clear;
   Clipboard.Close;

   ClipboardEmptyNotify;
  End;

 If FClipboardExtension.ecbCommand = CopyToClipboard Then
  Begin
   If FClipboardExtension.ClipboardType = CF_UNICODETEXT Then
    Begin
     Clipboard.Open;
      Clipboard.SetTextBuf(PChar(FClipboardExtension.cbText));
     Clipboard.Close;
    End;

   If FClipboardExtension.ClipboardType = CF_BITMAP Then
    Begin
     Clipboard.Open;
      FClipboardExtension.cbPicture.SaveToClipboardFormat(dataFormat,clipbrdHandle,bmpPalette);
      Clipboard.SetAsHandle(dataFormat,clipbrdHandle);
     Clipboard.Close;
    End;
  End;

 If FClipboardExtension.ecbCommand = EnableMonitor Then
  Begin
   FInternTimer.Enabled := True;
  End;

 If FClipboardExtension.ecbCommand = DisableMonitor Then
  Begin
   FInternTimer.Enabled := False;
  End;
end;

Procedure TClipboardExt.ClipboardDataDetected;
begin
 If Assigned(FOnDetectClipbrdData) = True Then
  Begin
   FOnDetectClipbrdData(Self);
  End;
end;

Procedure TClipboardExt.ClipboardMonitorStatus(Var monitorStatus :Boolean);
begin
 If Assigned(FOnMonitorClipboardStatus) = True Then
  Begin
   FOnMonitorClipboardStatus(Self,monitorStatus);
  End;
end;

Procedure TClipboardExt.ClipboardEmptyNotify;
begin
 If Assigned(FOnEmptyClipboardNotify) = True Then
  Begin
   FOnEmptyClipboardNotify(Self);
  End;
end;

Procedure TClipboardExt.InternTimerEvent(Sender: TObject);

Label QuitClipboardMonitor;

Var clipbrdPNGImage                 :TPNGImage;

    clipbrdType                     :String;

    clipboardFormat                 :Cardinal;

    formatNameLength,foundIndex     :Integer;

    clipbrdStatus                   :Boolean;

begin
 Clipboard.Open;

 clipboardFormat := 0;

 actualClipboardFormts.Clear;

 While EnumClipboardFormats(clipboardFormat) > 0 Do
  Begin
   clipboardFormat := EnumClipboardFormats(clipboardFormat);

   If clipboardFormat <= 18 Then
    Begin
     actualClipboardFormts.AddPair(clipbrdTypes[clipboardFormat],clipboardFormat.ToString);
    End
   Else
    Begin
     clipbrdFormatName := DupeString(' ',MAX_PATH);

     formatNameLength := GetClipboardFormatName(clipboardFormat,PChar(clipbrdFormatName),MAX_PATH);

     clipbrdFormatName := LeftStr(clipbrdFormatName,formatNameLength);

     If formatNameLength > 0 Then
      Begin
       actualClipboardFormts.AddPair(clipbrdFormatName,clipboardFormat.ToString);
      End;
    End;
  End;

 If clipboardFormat = 0 Then
  Begin
   If GetLastError <> ERROR_SUCCESS Then
    Begin
     MessageDlg(SysErrorMessage(GetLastError),mtError,[mbOK],0);

     Clipboard.Close;
     Exit;
    End;
 End;

 If actualClipboardFormts.Count = 0 Then
  Begin
   Clipboard.Close;
   Exit;
  End;

 If GetIndexFromStringList('CF_UNICODETEXT') >= 0 Then
  Begin
   foundIndex := GetIndexFromStringList('CF_UNICODETEXT');

   GetClipbrdData(foundIndex,clipbrdType);

   FClipboardExtension.cbText := PChar(GlobalLock(clipbrdHandle));

   FClipboardExtension.ClipboardType := GetClipboardType(clipbrdType);

   If actualClipBrdString <> FClipboardExtension.cbText Then
    Begin
     actualClipBrdString := FClipboardExtension.cbText;

     ClipboardDataDetected;

     FInternTimer.Enabled := False;
     clipbrdStatus := False;

     ClipboardMonitorStatus(clipbrdStatus);

     GoTo QuitClipboardMonitor;
    End
   Else
    Begin
     // Do nothing.
    End;
  End;

 If GetIndexFromStringList('CF_BITMAP') >= 0 Then
  Begin
   FClipboardExtension.cbPicture := TPicture.Create;

   foundIndex := GetIndexFromStringList('CF_BITMAP');

   GetClipbrdData(foundIndex,clipbrdType);

   FClipboardExtension.cbPicture.Bitmap.LoadFromClipboardFormat(2,clipbrdHandle,0);
   FClipboardExtension.ClipboardType := GetClipboardType(clipbrdType);

   ClipboardDataDetected;

   FInternTimer.Enabled := False;
   clipbrdStatus := False;

   ClipboardMonitorStatus(clipbrdStatus);

   FClipboardExtension.cbPicture.Free;

   GoTo QuitClipboardMonitor;
  End
 Else
  Begin
   // Do nothing.
  End;

 If GetIndexFromStringList('PNG') >= 0 Then
  Begin
   FClipboardExtension.cbPicture := TPicture.Create;

   foundIndex := GetIndexFromStringList('PNG');

   GetClipbrdData(foundIndex,clipbrdType);

   clipbrdPNGImage := TPngImage.Create;
   clipbrdPNGImage.LoadFromClipboardFormat(2,clipbrdHandle,0);

   FClipboardExtension.cbPicture.Assign(clipbrdPNGImage);
   FClipboardExtension.ClipboardType := GetClipboardType(clipbrdType);

   ClipboardDataDetected;

   FInternTimer.Enabled := False;
   clipbrdStatus := False;

   ClipboardMonitorStatus(clipbrdStatus);

   FClipboardExtension.cbPicture.Free;
   clipbrdPNGImage.Free;

   GoTo QuitClipboardMonitor;
  End
 Else
  Begin
   // Do nothing.
  End;

 If GetIndexFromStringList('CF_DIB') >= 0 Then
  Begin
   FClipboardExtension.cbPicture := TPicture.Create;

   foundIndex := GetIndexFromStringList('CF_DIB');

   GetClipbrdData(foundIndex,clipbrdType);

   clipbrdPNGImage := TPngImage.Create;
   clipbrdPNGImage.LoadFromClipboardFormat(2,clipbrdHandle,0);

   FClipboardExtension.cbPicture.Assign(clipbrdPNGImage);
   FClipboardExtension.ClipboardType := GetClipboardType(clipbrdType);

   ClipboardDataDetected;

   FInternTimer.Enabled := False;
   clipbrdStatus := False;

   ClipboardMonitorStatus(clipbrdStatus);

   FClipboardExtension.cbPicture.Free;
   clipbrdPNGImage.Free;

   GoTo QuitClipboardMonitor;
  End
 Else
  Begin
   // Do nothing.
  End;

 QuitClipboardMonitor:
 GlobalUnlock(clipbrdHandle);

 Clipboard.Close;
end;

Function TClipboardExt.GetIndexFromStringList(searchNameForIndex :String) :Integer;
begin
 searchIndexResult := actualClipboardFormts.IndexOfName(searchNameForIndex);
 indexValueResult := actualClipboardFormts.ValueFromIndex[searchIndexResult];

 Result := searchIndexResult;
end;

Procedure TClipboardExt.GetClipbrdData(formatTypeIndex :Integer; Var clipbrdDataType :String);

Var clipbrdFormat :Integer;

begin
 clipbrdDataType := actualClipboardFormts.Names[formatTypeIndex];
 clipbrdFormat := actualClipboardFormts.ValueFromIndex[formatTypeIndex].ToInteger;

 clipbrdHandle := GetClipboardData(clipbrdFormat);
end;

Function TClipboardExt.GetClipboardType(clipboardType :String) :TClipboardTypes;
Begin
 Result := TClipboardTypes(GetEnumValue(TypeInfo(TClipboardTypes),clipboardType));
End;

procedure Register;
begin
  RegisterComponents('ClipboardExt', [TClipboardExt]);
end;

end.
