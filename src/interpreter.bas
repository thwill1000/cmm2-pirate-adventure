' Scott Adams Adventure Game Interpreter for Maximite
' MMBasic port (c) Thomas Hugo Williams 2020
' Original TRS-80 code (c) Scott Adams 1978

Option Explicit On
Option Default Integer

#Include "debug.inc"

Dim ac
Dim k, s$, z$
Dim d = -1
Dim il ' Highest numbered object 0..il
Dim cl ' Highest action number 0..cl
Dim nl ' Highest vocabulary number 0..nl
Dim rl ' Highest room number 0..rl
Dim mx ' Maximum number of objects carried
Dim ar ' Starting room
Dim tt ' Number of treasures
Dim ln ' Word length
Dim lt ' Time limit
Dim ml ' Highest message number
Dim tr ' Treasure room
Dim lx ' Light duration
Dim nv(1) ' verb & noun of current command
Dim df ' Dark flag
Dim r  ' Current room
Dim sf ' Status flags
Dim f, f1, f2, f3
Dim ip
Dim x  ' 1st loop index
Dim y  ' 2nd loop index

Cls

read_data("pirate.dat")
show_intro()

r = ar  ' current room = starting room
lx = lt ' light source starts full
df = 0  ' dark flag is unset
sf = 0  ' status flags are clear

'INPUT "USE OLD 'SAVED' GAME? ", S$
'IF LEFT$(S$,1)<>"Y" THEN Goto 130
'110 IFD<>-1THENCLOSE:OPEN"I",D,SV$ELSEINPUT"READY SAVED TAPE "K$:PRINT;INT(IL*5/60)+1;" MINUTES"
'120 d=OPENIN("SAV"):INPUT#d,SF,LX,DF,R:FORX=0TOIL:INPUT#d,IA(X):NEXT:CLOSE#d:IFD<>-1CLOSE

main_loop()
End

Sub read_data(f$)
  f$ = "pirate.dat"
  Open f$ For Input AS 1
  Input #1, il, cl, nl, rl, mx, ar, tt, ln, lt, ml, tr
  Dim ca(cl, 7)      ' action table
  Dim nv_str$(nl, 1) ' vocabulary table - verbs at index 0, nouns at index 1
  Dim ia_str$(il)    ' object descriptions
  Dim ia(il)         ' object locations
  Dim rs$(rl)        ' room descriptions
  Dim rm(rl, 5)      ' room exits: N, S, E, W, U, D
  Dim ms$(ml)        ' messages table

  Local i, j

  ' Read action table.
  For i = 0 To cl Step 2
    j = i + 1
    Input #1,ca(i,0),ca(i,1),ca(i,2),ca(i,3),ca(i,4),ca(i,5),ca(i,6),ca(i,7),ca(j,0),ca(j,1),ca(j,2),ca(j,3),ca(j,4),ca(j,5),ca(j,6),ca(j,7)
  Next i

  ' Read vocabulary table.
  For i = 0 To nl Step 10
    For j = 0 TO 1
      Input #1,NV_STR$(i,j),NV_STR$(i+1,j),NV_STR$(i+2,j),NV_STR$(i+3,j),NV_STR$(i+4,j),NV_STR$(i+5,j),NV_STR$(i+6,j),NV_STR$(i+7,j),NV_STR$(i+8,j),NV_STR$(i+9,j)
    Next j
  Next i

  ' Read rooms.
  For i = 0 TO rl : INPUT #1, rm(i,0),rm(i,1),rm(i,2),rm(i,3),rm(i,4),rm(i,5),rs$(i) : Next i

  ' Read messages.
  For i = 0 TO ml : Input #1, ms$(i) : Next i

  ' Read objects.
  Dim i2(il)
  For i = 0 TO il : Input #1, ia_str$(i),ia(i):i2(i)=ia(i) : Next i

  Close #1
End Sub

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

Sub main_loop()
  Local _
  Do
    Print
    describe_room() ' TODO: shouldn't be describing this every time through the loop
    nv(0) = 0 ' no verb
    _ = do_actions() ' automatic actions
    Print
    prompt_for_input()
    Print
    Select Case do_actions() ' non-automatic actions
      Case -1 : Print "I don't understand your command."
      Case -2 : Print "I can't do that yet."
    End Select
    handle_light()
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
      Print nv$(i + 1, 1) " ";
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

Function do_actions()
  Local ok, n, v

  f = 1 : f2 = 1 : f3 = 0

  ' Handle "go <direction>"
  If nv(0) = 1 And nv(1) < 7 Then
    go_direction()
    Exit Sub
  EndIf

  For x = 0 to cl
    dbg.dump_action(x)
    ok = 0
    v = Int(ca(x, 0) / 150) ' action - verb
    n = ca(x, 0) - v * 150  ' action - noun
    If v = 0 Then
      ' Automatic action, n is the probability
      ' TODO: Am I correctly generating numbers in range 1..100
      f = 0
      ok = (1 + 100 * Rnd()) <= n
    EndIf
    If nv(0) = v And nv(1) = n Then ok = 1
    If nv(0) = v And n = 0 Then ok = 1

    If ok Then ok = process_conditions(x)
    If ok Then do_commands(x)
  Next x
End Function

Function process_conditions(x)
  Local k, ll, w, y

  f = 0 : f2 = 1 : f3 = 1
  For y = 1 To 5
    W = CA(X,Y)
    LL = INT(W/20) ' ll = <number>
    K = W -LL*20   ' k = condition code
    f2 = f2 And evaluate_condition(k, ll)
    If Not f2 Then Exit For
  Next y

  process_conditions = f2
End Function

Sub do_commands(x)
  Local cmd(3)

  IP=0
  cmd(0) = Int(ca(x, 6) / 150)
  cmd(1) = ca(x, 6) - cmd(0) * 150
  cmd(2) = Int(ca(x, 7) / 150)
  cmd(3) = ca(x, 7) - cmd(2) * 150

  do_command(cmd(0))
  do_command(cmd(1))
  do_command(cmd(2))
  do_command(cmd(3))
End Sub

REM Stop processing non-automatic actions
970 IF NV(0)<>0 THEN x=X:X=CL:NEXT X:X=x:GOTO 990

REM Next automatic action
980 NEXT X
990 :
1000 IF NV(0)=0 THEN Goto 1040
1010 GOSUB 1060
1020 IF F Then PRINT "I DON'T UNDERSTAND YOUR COMMAND": GOTO 1040
1030 IF NOT F2 Then PRINT "I CAN'T DO THAT YET" : GOTO 1040

REM Return from action-matching routine
1040 RETURN

REM Get param from condition list
1050 IP=IP+1:W=CA(X,IP):P=INT(W/20):M=W-P*20:IF M<>0 THEN1050 ELSE RETURN

REM Automatically GET or DROP
1060 IF NV(0)<>10 AND NV(0)<>18 OR F3 THEN 1230
1070 IF NV(1)=0 PRINT"WHAT?":GOTO1180
1080 IF NV(0)<>10 THEN 1110
1090 L=0:FORZ=0TOIL:IFIA(Z)=-1THENL=L+1
1100 NEXT:IF L>=MX PRINTZ$:GOTO1180
1110 K=0:FOR X=0TOIL:IF RIGHT$(IA$(X),1)<>"/"THEN1190 ELSE LL=LEN(IA$(X))-1:TP$=MID$(IA$(X),1,LL):FORY=LL TO2 STEP-1:IFMID$(TP$,Y,1)<>"/"THEN NEXTY:GOTO1190 ELSE y=Y:Y=2:NEXTY:Y=y
1120 TP$=LEFT$(MID$(TP$,Y+1),ln)
1130 IFTP$<>NV$(NV(1),1)THEN1190
1140 IFNV(0)=10THEN1160
1150 IFIA(X)<>-1THENK=1:GOTO1190 ELSE IA(X)=R:K=3:GOTO1170
1160 IFIA(X)<>R THEN K=2:GOTO1190 ELSE IA(X)=-1:K=3
1170 PRINT"OK, ";:x=X:X=IL:NEXTX:X=x
1180 F=0:RETURN
1190 NEXTX
1200 IF K=1 THEN PRINT"I'M NOT CARRYING IT" ELSE IFK=2 PRINT"I DON'T SEE IT HERE"
1210 IF K=0 IF NOT F3 PRINT"ITS BEYOND MY POWER TO DO THAT":F=0
1220 IF K<>0 THEN F=0
1230 RETURN

End Sub

Sub go_direction()
  Local l = df
  If l Then l = df And ia(9) <> R and ia(9) <> - 1
  If l Then Print "Dangerous to move in the dark!"
  If nv(1) < 1 Then Print "Give me a direction too." : Exit Sub
  Local k = rm(r, nv(1) - 1)
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
  describe_current_location()
End Sub

Function evaluate_condition(code, number)
  Local i, pass
  Select Case code
    Case 0
      pass = 1
    Case 1
      ' Passes if the player is carrying object <number>.
      pass = (ia(number) = -1)
    Case 2
      ' Passes if the player is in the same room (but not carrying) object <number>.
      pass = (ia(number) = r)
    Case 3
      ' Passes if object<number> is available; i.e. carried or in the current room
      pass = (ia(number) = -1) Or (ia(number) = r)
    Case 4
      ' Passes if the player is in room <number>.
      pass = (r = number)
    Case 5
      ' Passes if the player is carrying object <number> or it is in a different room.
      pass = (ia(number) <> r)
    Case 6
      ' Passes if the player is not carrying object <number>.
      pass = (ia(number) <> -1)
    Case 7
      ' Passes if the player is not in room <number>.
      pass = (r <> number)
    Case 8
      ' Passes if numbered flag-bit set.
      pass = (sf And Int(2^number + 0.5)) <> 0
    Case 9
      ' Passes if numbered flag-bit clear.
      pass = (sf And Int(2^number + 0.5)) = 0
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
      pass = (ia(number) <> -1) And (ia(number) <> r)
    Case 13
      ' Passes if object <number> is not in the store room (0)
      pass = (ia(number) <> 0)
    Case 14
      ' Passes if object <number> is in the store room (0)
      pass = (ia(number) = 0)
    Case Else
      Error "Unknown condition: " + Str$(code)
  End Select

  evaluate_condition = pass
End Function

Sub do_command(cmd)
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
      Goto 970 ' TODO: stop processing non-automatic actions
      p = get_parameter()
      ia(p) = -1

    Case 53
      ' DROPx
      ' Drop the Par #1 object in the current room.
      ' The object may be carried or in any other room
      p = get_parameter()
      ia(p) = r

    Case 54
      ' GOTOy
      ' Move the player to the Par #1 room.
      ' This command should be followed by a DspRM (64) command.
      ' Also it may need to be followed by a NIGHT (56) or DAY (57) command.
      p = get_parameter()
      r = p

    Case 55, 59
      ' x->RM0
      ' Move the Par #1 object to room 0 (the storeroom).
      p = get_parameter()
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
      p = get_parameter()
      sf = sf Or Int(0.5 + 2^p)

    Case 60
      ' CLRz
      ' Clear the Par #1 flag-bit.
      p = get_parameter()
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
      x = get_parameter()
      ia(x) = get_parameter()
      ' TODO: This isn't automatically displaying the room ?

    Case 63
      ' FINI
      ' Tell the player the game is over and ask if they want to play again.
      Print "The game is now over."
      Print "Another game?";
      Input s$
      If LCase$(Left$(s$, 1)) = "n" Then
        End
      Else
        For i = 0 To il
          ia(i) = i2(i)
        Next i
        Goto 100 ' Restart ?
      EndIf

    Case 64
      ' DspRM
      ' Display the current room.
      ' This checks if the darkness flag-bit (15) is set and the artificial
      ' light (object 9) is not available.
      ' If there is light, it displays the room description, the obejcts in
      ' the room and any obvious exits.
      describe_current_location()

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
      x = get_parameter() ' x = object 1
      y = get_parameter() ' y = object 2
      p = ia(x)           ' p = location of object 1
      ia(x) = ia(y)
      ia(y) = p

    Case Else
      Error "Unknown command:" cmd

  End Select

End Sub

Function get_parameter()
 Local m, p, w

 Do
   ip = ip + 1
   w = ca(x, ip)
   p = Int(w / 20)
   m = w - p * 20
 Loop While m <> 0

 get_parameter = p
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

Sub prompt_for_input()
  Local s$

  Do
    Input "Tell me what to do ? ", s$
    parse(s$)
    If Not f Then Exit Do
    Print "You use word(s) I don't know!"
  Loop

End Sub

Sub parse(s$)
  Local i, j, k, nt$(2), w$
  nt$(0) = "" ' current verb
  nt$(1) = "" ' current noun

  For i = 1 To Len(s$)
    If Mid$(s$, i, 1) = " " Then
      k = 1
    Else
      nt$(k) = nt$(k) + Mid$(s$, i, 1)
    EndIf
  Next i

  nt$(0) = LCase$(Left$(nt$(0), ln))
  nt$(1) = LCase$(Left$(nt$(1), ln))

  For i = 0 To 1
    ' i = 0 for verb, 1 for noun.
    nv(i) = 0
    If nt$(i) <> "" Then
      For j = 0 To nl
        w$ = nv_str$(j, i)
        If Left$(w$, 1) = "*" Then w$ = Mid$(w$, 2)
        If i = 1 And j < 7 Then w$ = Left$(w$, ln)
        If nt$(i) = LCase$(w$) Then
          ' Word found, if it's a synonym then use previous word
          nv(i) = j
          Do While Left$(nv_str$(nv(i), i), 1) = "*"
            nv(i) = nv(i) - 1
          Loop
          Exit For
        EndIf
      Next j
    EndIf
  Next i

  Print "verb =" nv(0) ", noun =" nv(1)

  f = nv(0) < 1 Or Len(nt$(1)) > 0 And nv(1) < 1
End Sub

Sub handle_light()
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
