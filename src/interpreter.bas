' Scott Adams Adventure Game Interpreter for Maximite
' MMBasic port (c) Thomas Hugo Williams 2020
' Original TRS-80 code (c) Scott Adams 1978

Option Explicit On
Option Default Integer

#Include "data.inc"
#Include "debug.inc"

Const ACTION_SUCCESS = 0
Const ACTION_UNKNOWN = -1
Const ACTION_NOT_YET = -2

' These global variables hold the current game state
Dim lx ' light duration
Dim df ' dark flag
Dim r  ' current room
Dim sf ' status flags
' TODO: move the object location array here

' TODO: This shouldn't be global
Dim ip ' action parameter pointer

Cls

dat.read("pirate.dat")
'show_intro()
reset_state()

'INPUT "USE OLD 'SAVED' GAME? ", S$
'IF LEFT$(S$,1)<>"Y" THEN Goto 130
'110 IFD<>-1THENCLOSE:OPEN"I",D,SV$ELSEINPUT"READY SAVED TAPE "K$:PRINT;INT(IL*5/60)+1;" MINUTES"
'120 d=OPENIN("SAV"):INPUT#d,SF,LX,DF,R:FORX=0TOIL:INPUT#d,IA(X):NEXT:CLOSE#d:IFD<>-1CLOSE

main_loop()
End

Sub show_intro()
  Local s$

  Cls
' Print'"*** WELCOME TO ADVENTURE LAND.(#4.6) ***":PRINT:PRINT" UNLESS TOLD DIFFERENTLY YOU MUST FIND"'"*TREASURES* AND RETURN THEM TO THEIR"'"PROPER PLACE!"
  Print "I'M YOUR PUPPET. GIVE ME ENGLISH COMMANDS THAT ";
  Print "CONSIST OF A NOUN AND VERB. SOME EXAMPLES..."
  Print
  Print "TO FIND OUT WHAT YOU'RE CARRYING YOU MIGHT SAY: TAKE INVENTORY"
  Print "TO GO INTO A HOLE YOU MIGHT SAY: GO HOLE"

  Print "TO SAVE CURRENT GAME: SAVE GAME"
  Print
  Print "YOU WILL AT TIMES NEED SPECIAL ITEMS TO DO THINGS, BUT I'M SURE YOU'LL BE A GOOD ADVENTURER AND FIGURE THESE THINGS OUT."
  Print
  Input "HAPPY ADVENTURING... HIT ENTER TO START", s$
  Cls
End Sub

Sub reset_state()
  r = ar  ' current room = starting room
  lx = lt ' light source starts full
  df = 0  ' dark flag is unset
  sf = 0  ' status flags are clear
End Sub

Sub main_loop()
  Local noun, nstr$, verb

  describe_room()
  Do
    do_actions(0, 0) ' automatic actions
    prompt_for_input(verb, noun, nstr$)
    Print
    do_actions(verb, noun, nstr$)
    update_light()
  Loop
End Sub

Sub describe_room()
  Local i, k, p

  If df Then
    ' Object 9 is the torch.
    If ia(9) <> -1 And ia(9) <> r Then
      Print "I can't see, its too dark!"
      Exit Sub
    EndIf
  EndIf

  If Left$(rs$(r), 1) = "*" Then
    Print Mid$(rs$(r), 2)
  Else
    Print "I'm in a " + rs$(r)
  EndIf

  Print "Obvious exits: ";
  k = 0
  For i = 0 To 5
    If rm(r, i) <> 0 Then
      Print nv_str$(i + 1, 1) " ";
      k = k + 1
    EndIf
  Next i
  If k = 0 Then Print "NONE" Else Print

  k = 0
  For i = 0 To il
    If ia(i) = r Then
      If k = 0 Then Print "Visible items here: ";
      k = k + 1
      If k > 1 Then Print ", ";
      p = InStr(ia_str$(i), "/")
      If p < 1 Then
        Print ia_str$(i);
      Else
        Print Left$(ia_str$(i), p - 1);
      EndIf
    EndIf
  Next i
  Print
End Sub

Sub do_actions(verb, noun, nstr$)
  Local a, an, av, process_action, state

  ' Handle "go <direction>"
  If verb = 1 And noun < 7 Then
    go_direction(noun)
    Exit Sub
  EndIf

  state = ACTION_UNKNOWN

  For a = 0 to cl
    av = Int(ca(a, 0) / 150) ' action - verb
    an = ca(a, 0) - av * 150 ' action - noun

    ' Stop processing automatic actions (verb == 0) when we reach the first
    ' non-zero action verb.
    If verb = 0 And av <> 0 Then Exit Sub

    If av = 0 And verb = 0 Then
      ' Automatic action, 'an' is the probability
      ' TODO: am I correctly generating numbers in range 1..100 ?
      process_action = (1 + 100 * Rnd()) <= an
    Else If av = verb And (an = noun Or an = 0) Then
      ' Verb and noun match, or action noun is 'ANY'
      process_action = 1
    Else
      process_action = 0
    EndIf

    If process_action Then
      If process_conditions(a) Then
        do_commands(a)
        state = ACTION_SUCCESS
      Else
        state = ACTION_NOT_YET
      EndIf
    EndIf

    ' Stop processing actions when a non-automatic action succeeds.
    If state = ACTION_SUCCESS And verb <> 0 Then Exit For

  Next a

  If state = ACTION_UNKNOWN Then
    If verb = 10 Then
      do_get(noun, nstr$)
      state = ACTION_SUCCESS
    Else If verb = 18 Then
      do_drop(noun, nstr$)
      state = ACTION_SUCCESS
    End If
  End If

  Select Case state
    Case ACTION_UNKNOWN : Print "I don't understand your command."
    Case ACTION_NOT_YET : Print "I can't do that yet."
  End Select

End Sub

' @param  a  current action index
Function process_conditions(a)
  Local code, i, ok, value

  ok = 1
  For i = 1 To 5
    value = Int(ca(a, i) / 20)
    code = ca(a, i) - value * 20
    ok = ok And evaluate_condition(code, value)
    If Not ok Then Exit For
  Next i

  process_conditions = ok
End Function

' @param  a  current action index
Sub do_commands(a)
  Local cmd(3)

  ip = 0 ' reset parameter pointer
  cmd(0) = Int(ca(a, 6) / 150)
  cmd(1) = ca(a, 6) - cmd(0) * 150
  cmd(2) = Int(ca(a, 7) / 150)
  cmd(3) = ca(a, 7) - cmd(2) * 150

  do_command(a, cmd(0))
  do_command(a, cmd(1))
  do_command(a, cmd(2))
  do_command(a, cmd(3))
End Sub

Sub go_direction(noun)
  Local l = df
  If l Then l = df And ia(9) <> R and ia(9) <> - 1
  If l Then Print "Dangerous to move in the dark!"
  If noun < 1 Then Print "Give me a direction too." : Exit Sub
  Local k = rm(r, noun - 1)
  If k < 1 Then
    If l Then
      Print "I fell down and broke my neck."
      k = rl
      df = 0
    Else
      Print "I can't go in that direction."
      Exit Sub
    EndIf
  EndIf
  If Not l Then Cls
  r = k
  describe_room()
End Sub

Function evaluate_condition(code, value)
  Local i, pass
  Select Case code
    Case 0
      pass = 1
    Case 1
      ' Passes if the player is carrying object <value>.
      pass = (ia(value) = -1)
    Case 2
      ' Passes if the player is in the same room (but not carrying) object <value>.
      pass = (ia(value) = r)
    Case 3
      ' Passes if object <value> is available; i.e. carried or in the current room
      pass = (ia(value) = -1) Or (ia(value) = r)
    Case 4
      ' Passes if the player is in room <value>.
      pass = (r = value)
    Case 5
      ' Passes if the player is carrying object <value> or it is in a different room.
      pass = (ia(value) <> r)
    Case 6
      ' Passes if the player is not carrying object <value>.
      pass = (ia(value) <> -1)
    Case 7
      ' Passes if the player is not in room <value>.
      pass = (r <> value)
    Case 8
      ' Passes if numbered flag-bit set.
      pass = (sf And Int(2^value + 0.5)) <> 0
    Case 9
      ' Passes if numbered flag-bit clear.
      pass = (sf And Int(2^value + 0.5)) = 0
    Case 10
      ' Passes if the player is carrying anything.
      For i = 0 To il
        If ia(i) = -1 Then pass = 1 : Exit For
      Next i
    Case 11
      ' Passes if the player is carrying nothing.
      pass = 1
      For i = 0 To il
        If ia(i) = -1 Then pass = 0 : Exit For
      Next i
    Case 12
      ' Passes if object <number> is not available; i.e. not carried or in the current room.
      pass = (ia(value) <> -1) And (ia(value) <> r)
    Case 13
      ' Passes if object <value> is not in the store room (0)
      pass = (ia(value) <> 0)
    Case 14
      ' Passes if object <value> is in the store room (0)
      pass = (ia(value) = 0)
    Case Else
      Error "Unknown condition: " + Str$(code)
  End Select

  evaluate_condition = pass
End Function

' @param  a  current action index
Sub do_command(a, cmd)
  Local i, p, x, y

  Select Case cmd
    Case 0
      ' Do nothing ? Or should it display message 0 which is null ?

    Case 1 To 51
      ' Display corresponding message.
      Print ms$(cmd)

    Case 52
      ' GETx
      ' Pick up the Par #1 object unless player already carrying the limit.
      ' The object may be in this room, or in any other room.
      x = 0
      For i = 1 To il
        If ia(i) = -1 Then x = x + 1
      Next i
      If x > mx Then Print "I'VE TOO MUCH TOO CARRY. TRY -TAKE INVENTORY-"
      p = get_parameter(a)
      ia(p) = -1

    Case 53
      ' DROPx
      ' Drop the Par #1 object in the current room.
      ' The object may be carried or in any other room
      p = get_parameter(a)
      ia(p) = r

    Case 54
      ' GOTOy
      ' Move the player to the Par #1 room.
      ' This command should be followed by a DspRM (64) command.
      ' Also it may need to be followed by a NIGHT (56) or DAY (57) command.
      p = get_parameter(a)
      r = p

    Case 55, 59
      ' x->RM0
      ' Move the Par #1 object to room 0 (the storeroom).
      p = get_parameter(a)
      ia(p) = 0

    Case 56
      ' NIGHT
      ' Set the darkness flag-bit (15).
      ' It will be dark if the artificial light source is not available,
      ' so this should be followed by a DspRM (64) command.
      df = 1 ' TODO: this isn't a flag bit ! difference between interpreter versions ?

    Case 57
      ' DAY
      ' Clear the darkness flag-bit (15).
      ' This should be followed by a DspRM (64) command.
      df = 0

    Case 58
      ' SETz
      ' Set the Par #1 flag-bit.
      p = get_parameter(a)
      sf = sf Or Int(0.5 + 2^p)

    Case 60
      ' CLRz
      ' Clear the Par #1 flag-bit.
      p = get_parameter(a)
      sf = sf And Not Int(0.5 + 2^p)

    Case 61
      ' DEAD
      ' Tell the player they are dead,
      ' Goto the last room (usually some form of limbo),
      ' make it DAY and display the room.
      Print "I'M DEAD..."
      r = rl
      df = 0
      do_command(64)

    Case 62
      ' x->y
      ' Move the Par #1 object to the Par #2 room.
      ' This will automatically display the room if the object came from,
      ' or went to the current room.
      x = get_parameter(a)
      ia(x) = get_parameter(a)
      ' TODO: This isn't automatically displaying the room ?

    Case 63
      ' FINI
      ' Tell the player the game is over and ask if they want to play again.
      Print "The game is now over."
      Print "Another game?";
      Local s$
      Input s$
      If LCase$(Left$(s$, 1)) = "n" Then
        End
      Else
        For i = 0 To il
          ia(i) = i2(i)
        Next i
        Goto 100 ' TODO: Restart
      EndIf

    Case 64
      ' DspRM
      ' Display the current room.
      ' This checks if the darkness flag-bit (15) is set and the artificial
      ' light (object 9) is not available.
      ' If there is light, it displays the room description, the objects in
      ' the room and any obvious exits.
      describe_room()

    Case 65
      ' SCORE
      ' Tells the player how many treasures they have collected by getting
      ' them to the treasure room and what their percentage of the total is.
      x = 0
      For i = 1 To il
        If ia(i) = tr And Left$(ia_str$(i), 1) = "*" Then x = x + 1
      Next i
      Print "I've stored " Str$(x) " treasures. On a scale of 0 to 100 that rates a ";
      Print Str$(Int(x/tt*100)) "."
      If x = tt Then
        Print "Well done."
        Goto 850
      EndIf

    Case 66
      ' INV
      ' Tells the player what objects they are carrying.
      Print "I'm carrying: ";
      print_object_list(-1)

    Case 67
      ' SET0
      ' Sets the flag-bit numbered 0 (this may be convenient because no parameter is used).
      sf = sf Or Int(0.5)

    Case 68
      ' CLR0
      ' Clears the flag-bit numbered 0 (this may be convenient because no parameter is used).
      sf = sf And Not Int(0.5)

    Case 69
      ' FILL
      ' Re-fill the artifical light source and clear flag-bit 16 which
      ' indicates that it was empty. This also picks up the artifical light
      ' source (object 9). This command should be followed by a x->RM0 to store
      ' the unlighted light source (these are two different objects).
      lx = lt
      ia(9) = -1

    Case 70
      ' CLS
      Cls

    Case 71
      ' SAVEz
      ' This command saves the game to tape or disk, depending on which version
      ' is used. It writes some user variables such as time limit and the
      ' current room and the current locations of all objects out as a saved
      ' game.
      '710 PRINT"SAVING GAME":IFD=-1 INPUT"READY OUTPUT TAPE "K$:PRINT;INT(IL*5/60)+1;" MINUTES"ELSE OPEN"O",D,SV$
      '720 d=OPENOUT("SAV"):PRINT#d,SF,LX,DF,R:FORW=0TOIL:PRINT#d,IA(W):NEXT:CLOSE#d:IFD<>-1CLOSE
      '730 GOTO960

    Case 72
      ' EXx,x
      ' This command exchanges the room locations of the Par #1 object and the
      ' Par #2 object. If the objects in the current room change, the new
      ' description will be displayed.
      x = get_parameter(a) ' x = object 1
      y = get_parameter(a) ' y = object 2
      p = ia(x)           ' p = location of object 1
      ia(x) = ia(y)
      ia(y) = p

    Case 102 To 149
      ' Display corresponding message.
      Print ms$(cmd - 50)

    Case Else
      Error "Unknown command: " + Str$(cmd)

  End Select

End Sub

' @param   a   current action index
' @global  ip  parameter pointer
Function get_parameter(a)
 Local code, value

 Do
   ip = ip + 1
   value = Int(ca(a, ip) / 20)
   code = ca(a, ip) - value * 20
 Loop While code <> 0

 get_parameter = value
End Function

Sub print_object_list(rm)
  Local count, i, p

  For i = 0 To il
    If ia(i) = rm Then
      count = count + 1
      If count > 1 Then Print ", ";
      p = InStr(ia_str$(i), "/")
      If p < 1 Then
        Print ia_str$(i);
      Else
        Print Left$(ia_str$(i), p - 1);
      EndIf
    EndIf
  Next i

  If count = 0 Then Print "Nothing" Else Print
End Sub

Sub prompt_for_input(verb, noun, nstr$)
  Local s$

  Do
    Input "Tell me what to do ? ", s$
    parse(s$, verb, noun, nstr$)
    If verb <> 0 Then Exit Do
    Print "You use word(s) I don't know!"
  Loop

End Sub

Sub parse(s$, verb, noun, nstr$)
  Local p, vstr$

  vstr$ = ""
  nstr$ = ""

  p = InStr(s$, " ")
  If p < 1 Then p = Len(s$) + 1
  vstr$ = Left$(s$, p - 1)
  Do While Mid$(s$, p, 1) = " " : p = p + 1 : Loop
  nstr$= Mid$(s$, p, Len(s$) - p + 1)

  vstr$ = LCase$(Left$(vstr$, ln))
  nstr$ = LCase$(Left$(nstr$, ln))

  verb = lookup_word(vstr$, 0)
  noun = lookup_word(nstr$, 1)

  ' Hack to allow use of common abbreviations, and avoid typing 'go'.
  If verb = 0 Then
    Select Case vstr$
      Case "n", "north" : verb = 1 : noun = 1
      Case "s", "south" : verb = 1 : noun = 2
      Case "e", "east"  : verb = 1 : noun = 3
      Case "w", "west"  : verb = 1 : noun = 4
      Case "u", "up"    : verb = 1 : noun = 5
      Case "d", "down"  : verb = 1 : noun = 6
      Case "i"          : verb = 25
    End Select
  End If

  If noun <> 0 Then nstr$ = LCase$(nv_str$(noun, 1)) ' to use correct synonym

'  Print "verb =" verb ", noun =" noun ", nstr$ = " nstr$
End Sub

Function lookup_word(word$, dict)
  Local i, s$

  lookup_word = 0

  If word$ = "" Then Exit Function

  For i = 0 To nl
    s$ = nv_str$(i, dict)
    If Left$(s$, 1) = "*" Then s$ = Mid$(s$, 2)
    If dict = 1 And i < 7 Then s$ = Left$(s$, ln)
    If word$ = LCase$(s$) Then
      ' Word found, if it's a synonym then use previous word
      lookup_word = i
      Do While Left$(nv_str$(lookup_word, dict), 1) = "*"
        lookup_word = lookup_word - 1
      Loop
      Exit For
    EndIf
  Next i
End Function

Sub do_get(noun, nstr$)
  Local carried = 0, i, k

  If nstr$ = "" Then Print "What?" : Exit Sub

  For i = 0 To il
    If ia(i) = -1 Then carried = carried + 1
  Next i
  If carried >= mx Then Print "I've too much to carry!" : Exit Sub

  For i = 0 To il
    If LCase$(obj_noun$(i)) = nstr$ Then
      If ia(i) = r Then
        ia(i) = -1
        k = 3
        Exit For
      Else
        k = 2
      End If
    End If
  Next i

  If k = 2 Then
    Print "I don't see it here."
  Else If k = 0 Then
    Print "It's beyond my power to do that."
  Else
    Print "OK, ";
  End If
End Sub

' Gets the noun for referring to the given object.
Function obj_noun$(i)
  Local en, st

  st = InStr(ia_str$(i), "/")
  If st > 1 Then
    en = InStr(st + 1, ia_str$(i), "/")
    If en < st + 1 Then Error "Missing trailing '/'"
    obj_noun$ = Mid$(ia_str$(i), st + 1, en - st - 1)
  End If

  If Len(obj_noun$) > ln Then Error "Object noun too long: " + obj_noun$
End Function

Sub do_drop(noun, nstr$)
  Local i, k = 0

  If nstr$ = "" Then Print "What?" : Exit Sub

  For i = 0 To il
    If LCase$(obj_noun$(i)) = nstr$ Then
      If ia(i) = -1 Then
        ia(i) = r
        k = 3
        Exit For
      Else
        k = 1
      End If
    End If
  Next i

  If k = 1 Then
    Print "I'm not carrying it!"
  Else If k = 0 Then
    Print "It's beyond my power to do that."
  Else
    Print "OK, ";
  End If

End Sub

Sub update_light()
  ' If carrying the lit light source ...
  If ia(9) = -1 Then
    lx = lx - 1 ' decrement its duration
    If lx < 0 Then
      Print "Light has run out!"
      ia(9) = 0
    ElseIf lx < 25 Then
      Print "Light runs out in " Str$(lx) " turns!"
    EndIf
  EndIf
End Sub
