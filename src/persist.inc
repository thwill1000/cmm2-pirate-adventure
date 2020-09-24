' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Sub do_save()
  Local _ = zsave(0)
End Sub

Sub do_load()
  Local _ = zsave(1)
End Sub

' Save/Restore game
'
' NOTE: Use a single subroutine to cover both because of the 50 subroutine
'       limit in MMBasic 4.5 on the CMM1
'
' @param  res  set 0 to save and 1 to restore
' @return      1 on success, 0 on failure or if cancelled
Function zsave(res)
  Local exists(10), i, old_dir$, s$, s2$(2) Length 40

  fil.mkdir(SAVE_DIR$)

  If res Then
    con.out("Select game to restore:") : con.endl()
  Else
    con.out("Select save game slot:") : con.endl()
  EndIf

'  old_dir$ = Cwd$
'  ChDir SAVE_DIR$
  For i = 1 To 10
    s$ = Dir$(SAVE_DIR$ + "/game" + Str$(i) + ".sav")
    con.out("  [" + Format$(i, "%2g") + "] ")
    If s$ = "" Then
      con.out("Empty") : con.endl()
    Else
      exists(i) = 1
      Open (SAVE_DIR$ + "/game" + Str$(i) + ".sav") For Input As #1
      Line Input #1, s2$(0) ' header
      Line Input #1, s2$(1) ' version
      Line Input #1, s2$(2) ' date/time
      Line Input #1, s$     ' game name
      ' TODO: verify header / version
      con.out(s2$(2) + " - " + s$) : con.endl()
      Close #1
    EndIf
  Next i
'  ChDir old_dir$

  s$ = con.in$("Game number? ")
  i = Val(s$)
  If i < 1 Or i > 10 Then i = 0

  If i > 0 And res And Not exists(i) Then i = 0

  If i > 0 And Not res And exists(i) Then
    s$ = con.in$("Overwrite game " + Str$(i) + " [y|N]? ")
    If LCase$(s$) <> "y" Then i = 0
  EndIf

  If i > 0 And Not res Then
    s$ = con.in$("Save game name? ")
    If s$ = "" Then i = 0
  EndIf

  If i = 0 Then con.out("Cancelled") : con.endl() : Exit Function

  s2$(0) = SAVE_DIR$ + "\game" + Str$(i) + ".sav"

  If res Then
    Open s2$(0) For Input As #1

    ' Read text header
    Line Input #1, s2$(0)
    Line Input #1, s2$(1)
    Line Input #1, s2$(2)
    Line Input #1, s$
    con.out("Restoring '" + s$ + "' ...") : con.endl()

    Local err$, values(4 + il), tmp$
    For i = 0 To 4 + il
      Line Input #1, tmp$
      If tmp$ = "" Then
        err$ = "missing data"
        Exit For
      Else
        values(i) = Val(tmp$)
      End If
    Next i
    If err$ = "" And Not Eof(#1) Then err$ = "unexpected data"

    If err$ <> "" Then
      con.out("Save file is invalid: " + err$) : con.endl()
      Close #1
      Exit Function
    EndIf

    ' Update game state with values read from file.
    sf = values(0)
    df = values(1)
    r  = values(2)
    lx = values(3)
    For i = 4 To il + 4 : ia(i - 4) = values(i) : Next i

  Else
    con.out("Saving '" + s$ + "' ...") : con.endl()
    Open s2$(0) For Output As #1

    ' Write text header
    Print #1, "SAINT save file"
    Print #1, "1"
    Print #1, Date$ + " " + Time$
    Print #1, s$

    ' Write state
    Print #1, "&b" Bin$(sf, 32) ' status flags
    Print #1, Str$(df)          ' dark flag
    Print #1, Str$(r)           ' current room
    Print #1, Str$(lx)          ' light duration
    For i = 0 To il : Print #1, Str$(ia(i)) : Next i ' object locations

  EndIf

  Close #1
  zsave = 1

End Function
