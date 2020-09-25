' Copyright (c) 2020 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Dim con.buf$   ' console write buffer
Dim con.space  ' was the last character written to con.buf$ a space?
Dim con.lines  ' number of lines that have been output since the last prompt
Dim con.count  ' number of consecutive calls to con.endl() without an
               ' intervening call to con.out()
Dim con.x = 1  ' current horizontal cursor position, indexed from 1
Dim con.spin   ' is the spinning cursor currently displayed?
Dim con.fd_in  ' file descriptor to read input from
Dim con.fd_out ' file descriptor to echo input to
Dim con.echo   ' if 1 then echo non-empty input to 'con.fd_out'

'!comment_if INLINE_CONSTANTS
Dim CON.HEIGHT = 50
Dim CON.WIDTH = 100
'!endif

' @param  p$  prompt
Function con.in$(p$)
  Local s$

  con.out(p$)
  con.flush()
  con.lines = 0

  Line Input #con.fd_in, s$
  If con.fd_in > 0 Then con.out(s$) : con.endl()
  con.x = 1

  If con.echo And con.fd_out > 0 And s$ <> "" Then Print #con.fd_out, s$

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
Sub con.print_file(f$)
  Local s$

  Open f$ For Input As #1
  Do
    Line Input #1, s$
    con.out(s$)
    con.endl()
  Loop While Not Eof(#1)
  Close #1
End Sub

Sub con.close()
  If con.fd_in <> 0 Then Close #con.fd_in
  If con.fd_out <> 0 Then Close #con.fd_out
  con.echo = 0
End Sub

Sub con.open_in(fd, f$)
  If con.fd_in <> 0 Then Close #con.fd_in
  Open f$ For Input As #fd
  con.fd_in = fd
End Sub

Sub con.open_out(fd, f$)
  If con.fd_out <> 0 Then Close #con.fd_out
  Open f$ For Output As #fd
  con.fd_out = fd
  con.echo = 1
End Sub