' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Sub dbg.dump_action(x)
  Local cmd(3), noun, verb

  verb = Int(ca(x, 0) / 150) ' action - verb
  noun = ca(x, 0) - verb * 150  ' action - noun
  cmd(0) = Int(ca(x, 6) / 150)
  cmd(1) = ca(x, 6) - cmd(0) * 150
  cmd(2) = Int(ca(x, 7) / 150)
  cmd(3) = ca(x, 7) - cmd(2) * 150

  Local n$ = Str$(noun)
  Local v$ = Str$(verb)
  If verb > 0 Then
    n$ = dbg.get_noun$(noun)
    v$ = dbg.get_verb$(verb)
  End If

  Print Str$(x) ":" Tab(6) v$ Tab(12) n$ Tab(18);
  Print dbg.get_cmd$(cmd(0)) Tab(28) dbg.get_cmd$(cmd(1)) Tab(38) dbg.get_cmd$(cmd(2)) Tab(48) dbg.get_cmd$(cmd(3))
End Sub

Function dbg.get_verb$(v)
  dbg.get_verb$ = nv_str$(v, 0)
End Function

Function dbg.get_noun$(n)
  dbg.get_noun$ = nv_str$(n, 1)
End Function

Function dbg.get_cmd$(c)
  Local s$
  Select Case c
    Case 0 : s$ = "0"
    Case 1 To 51 : s$ = "MSG:" + Str$(c)
    Case 52: s$ = "GETx"
    Case 53: s$ = "DROPx"
    Case 54: s$ = "GOTOy"
    Case 55: s$ = "x->RM0"
    Case 56: s$ = "NIGHT"
    Case 57: s$ = "DAY"
    Case 58: s$ = "SETz"
    Case 59: s$ = "x->RM0" ' same as 55
    Case 60: s$ = "CLRz"
    Case 61: s$ = "DEAD"
    Case 62: s$ = "x->y"
    Case 63: s$ = "FINI"
    Case 64: s$ = "DspRM"
    Case 65: s$ = "SCORE"
    Case 66: s$ = "INV"
    Case 67: s$ = "SET0"
    Case 68: s$ = "CLR0"
    Case 69: s$ = "FILL"
    Case 70: s$ = "CLS"
    Case 71: s$ = "SAVEz"
    Case 72: s$ = "EXx,x"
    Case 102 To 149 : s$ = "MSG:" + Str$(c - 50)
    Case Else: s$ = "Huh?"
  End Select
  dbg.get_cmd$ = s$
End Function

Sub dbg.dump_vocab()
  Local i
  For i = 0 To nl
    Print i, nv_str$(i, 0), nv_str$(i, 1)
  Next i
End Sub
