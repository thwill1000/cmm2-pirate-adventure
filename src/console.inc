' Copyright (c) 2020 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Dim script    ' bit 1 is set for writing (via #2),
              ' bit 2 is set for reading (via #3)
Dim con.buf$  ' console write buffer
Dim con.space ' was the last character written to con.buf$ a space?
Dim con.lines ' number of lines that have been output since the last prompt
Dim con.count ' number of consecutive calls to con.endl() without an
              ' intervening call to con.out()
Dim con.x = 1 ' current horizontal cursor position, indexed from 1
Dim con.spin  ' is the spinning cursor currently displayed?

'!comment_if INLINE_CONSTANTS
Const S_WRITE = &b01
Const S_READ  = &b10

Dim CON.HEIGHT = 50
Dim CON.WIDTH = 100
'!endif

' @param  p$  prompt
' @param  r   if 1 then write input to the script
Function con.in$(p$, r)
  Local s$

  con.out(p$)
  con.flush()
  con.lines = 0

  ' If configured read input from the script
  If script And S_READ Then
    Line Input #3, s$
    If s$ = "" Then
      script = script And S_WRITE
      Close #3
    Else
      con.out(s$) : con.endl()
    EndIf
  EndIf

  ' Otherwise read input from the console
  If Not(script And S_READ) Then Line Input s$ : con.x = 1

  ' If configured write input to the script
  If (r = 1) And (script AND S_WRITE) And (s$ <> "") Then Print #2, s$

  con.in$ = s$
End Function

' Outputs s$.
'
' s$ is appended to con.buf$ one character at a time. Only when we switch from
' non-spaces to spaces or vice versa do we flush the contents of con.buf$ to the
' console. As a result calling this function will probably not print the
' entirety of s$; ' the last 'word' or part thereof may be left in con.buf$.
Sub con.out(s$)
  con.count = 0

  If Len(s$) = 1 Then
    If (s$ = " ") Xor con.space Then con.flush() : con.space = (s$ = " ")
    con.buf$ = con.buf$ + s$
  Else
    Local c$, i
    For i = 1 To Len(s$)
      c$ = Mid$(s$, i, 1)
      If (c$ = " ") Xor con.space Then con.flush() : con.space = (c$ = " ")
      con.buf$ = con.buf$ + c$
    Next i
  EndIf
End Sub

' Writes con.buf$ to the console and clears it.
'
' If con.buf$ does not fit on the current line then first write CRLF and then if
' con.buf$ contains non-spaces write con.buf$ to the console.
Sub con.flush()
  If con.spin Then Print Chr$(8); " "; Chr$(8); : con.spin = 0

  Do
    If con.x = 1 And con.lines > CON.HEIGHT - 2 Then
      Print "[MORE] ";
      Do While Inkey$ <> "" : Loop
      Do While Inkey$ = "" : Loop
      Print
      con.lines = 0
    EndIf

    If con.x + Len(con.buf$) > CON.WIDTH Then
      Print
      con.lines = con.lines + 1
      con.x = 1
      If con.space Then con.buf$ = "" : Exit Sub
    Else
      Print con.buf$;
      con.x = con.x + Len(con.buf$)
      con.buf$ = ""
      Exit Sub
    EndIf
  Loop
End Sub

' Flushes con.buf$ to the console and then writes CRLF.
Sub con.endl()
  con.flush()

  If con.count < 0 Then
    Exit Sub
  ElseIf con.count >= 10 Then
    ' If the story outputs 10 or more empty lines then we assume it was trying
    ' to clear the screen
    Local i
    For i = 0 To CON.HEIGHT - con.count - 1 : Print : Next i
    con.count = -999 ' to avoid any unnecessary additional empty lines
    con.lines = 0
    Exit Sub
  EndIf

  Print
  con.count = con.count + 1
  con.lines = con.lines + 1
  con.x = 1
End Sub

' Echos the contents of a (hopefully) text file to the console.
Sub con.echo(f$)
  Local s$

  Open f$ For Input As #1
  Do
    Line Input #1, s$
    con.out(s$)
    con.endl()
  Loop While Not Eof(#1)
  Close #1
End Sub
