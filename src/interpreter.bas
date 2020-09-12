REM COPYRIGHT SCOTT ADAMS. 1978
REM converted to BBC BASIC in 2020

Option Explicit On
Option Default Integer

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
Dim r
Dim sf ' Status flag
Dim zi
Dim tp$
Dim v, w
Dim f, f1, f2, f3
Dim n, ll, ip
Dim kk$
Dim x ' 1st loop index
Dim y ' 2nd loop index
Dim l

Cls

REM L% = screen width in chars
'20 MODE7:VDU23,0,10,8;0;0;0;23,0,11,12;0;0;0;:PRINT:D=-1:DIM S% 255:L%=40:*FX11,0

REM X=Y=Z:K=R=V:N=LL=F:TP$=K$:W=IP=P:

40 K=0:Z$="I'VE TOO MUCH TOO CARRY. TRY -TAKE INVENTORY-"

read_data("pirate.dat")
intro()

100 R=AR:LX=LT:DF=0:SF=0

'INPUT "USE OLD 'SAVED' GAME? ", S$
'IF LEFT$(S$,1)<>"Y" THEN Goto 130
'110 IFD<>-1THENCLOSE:OPEN"I",D,SV$ELSEINPUT"READY SAVED TAPE "K$:PRINT;INT(IL*5/60)+1;" MINUTES"
'120 d=OPENIN("SAV"):INPUT#d,SF,LX,DF,R:FORX=0TOIL:INPUT#d,IA(X):NEXT:CLOSE#d:IFD<>-1CLOSE

describe_current_location()
GOTO 160

140
INPUT "TELL ME WHAT TO DO? ", TP$
PRINT
parse(tp$)
'GOSUB 170 ' Parse TP$
IF F Then PRINT "YOU USE WORD(S) I DON'T KNOW" : GOTO 140
150 GOSUB 360
If lx < 0 Then Print "LIGHT HAS RUN OUT!" : ia(9) = 0 : Goto 160
If lx < 25 Then Print "LIGHT RUNS OUT IN " Str$(lx) "TURNS!"
160 NV(0)=0 : GOSUB 360 : GOTO 140

REM *** Search for matching actions ***

REM Find a matching action
360 F2=-1:F=-1:F3=0

' Go <Direction>
IF NV(0) = 1 AND NV(1) < 7 THEN Goto 610

FOR x = 0 to cl
  v = Int(ca(x, 0) / 150) ' action - verb
  n = ca(x, 0) - v * 150  ' action - noun
  If v = 0 Then
    ' Automatic action, n is the probability
    f = 0
    If rnd(100) <= n Then Goto 400
  EndIf
  If nv(0) = v And nv(1) = n Then Goto 400
  If nv(0) = v And n = 0 Then Goto 400
Next x

Goto 990

REM Process the conditions                        K=condition code, LL="number"
400 Print "DEBUG: Process condition:"; x
F2 = 1 : F = 0 : F3 = 1
FOR y = 1 TO 5
  W = CA(X,Y)
  LL = INT(W/20) ' ll = <number>
  K = W -LL*20   ' k = condition code
  f2 = f2 And evaluate_condition(k, ll)
  If Not f2 Then Exit For
Next y

If Not f2 Then Next x

REM Process the commands
560 IP=0:FOR Y=1 TO 4:K=INT((Y-1)/2)+6:ON Y GOTO 570,580,570,580
570 AC=INT(CA(X,K)/150):GOTO 590
580 AC=CA(X,K)-INT(CA(X,K)/150)*150
590 IF AC>101 THEN Goto 600 ELSE IF AC=0 THEN Goto 960 ELSE IF AC<52 THEN Print MS$(AC):GOTO 960:ELSE ON AC-51 GOTO 660,700,740,760,770,780,790,760,810,830,840,850,860,870,890,920,930,940,950,710,750
600 Print MS$(AC-50) : GOTO 960

' Go <Direction>
610
action_go()
Goto 1040

REM 52. GETx
660 L=0:FORZ=1TOIL:IFIA(Z)=-1LETL=L+1
670 NEXTZ
680 IF L>=MX PRINT Z$:GOTO970
690 GOSUB1050:IA(P)=-1:GOTO960

REM 53. DROPx
700 GOSUB1050:IA(P)=R:GOTO960

REM 71. SAVE
710 PRINT"SAVING GAME":IFD=-1 INPUT"READY OUTPUT TAPE "K$:PRINT;INT(IL*5/60)+1;" MINUTES"ELSE OPEN"O",D,SV$
720 d=OPENOUT("SAV"):PRINT#d,SF,LX,DF,R:FORW=0TOIL:PRINT#d,IA(W):NEXT:CLOSE#d:IFD<>-1CLOSE
730 GOTO960

REM 54. GOTOy
740 GOSUB1050:R=P:GOTO960

REM 72. EXx,x (Exchange the room locations of the Par #1 object and the Par #2 object)
750 GOSUB1050:L=P:GOSUB1050:Z=IA(P):IA(P)=IA(L):IA(L)=Z:GOTO960

REM 55/59. x->RM0
760 GOSUB1050:IA(P)=0:GOTO960

REM 56. NIGHT
770 DF=-1:GOTO960

REM 57. DAY
780 DF=0:GOTO960

REM 58. SETz
790 GOSUB1050
800 SF=SF OR INT(.5+2^P):GOTO960

REM 60. CLRz
810 GOSUB1050

820 SF=SF AND NOT INT(.5+2^P):GOTO960

REM 61. DEAD
830 PRINT"I'M DEAD...":R=RL:DF=0:GOTO860

REM 62. x->y (Move the Par #1 object to the Par #2 room)
840 GOSUB1050:L=P:GOSUB1050:IA(L)=P:GOTO960

REM 63. FINI
850 INPUT"THE GAME IS NOW OVER"'"ANOTHER GAME? "K$:IFLEFT$(K$,1)="N"THENEND ELSE FORX=0TOIL:IA(X)=I2(X):NEXT:GOTO100

REM 64. DspRM
860 GOSUB240:GOTO960

REM 65. SCORE
870 L=0:FORZ=1TOIL:IFIA(Z)=TR IFLEFT$(IA$(Z),1)="*"LET L=L+1
880 NEXTZ:PRINT"I'VE STORED ";L;" TREASURES."'"ON A SCALE OF 0 TO 100 THAT RATES A ";INT(L/TT*100):IFL=TT THENPRINT"WELL DONE.":GOTO850 ELSE960

REM 66. INV
REM 890 PRINT"I'M CARRYING:":K$="NOTHING":PROCd3("890 FOR"):FORZ=0TOIL:IFIA(Z)<>-1THEN910 ELSEGOSUB280:IFLEN(TP$)+POS>63PRINT
890 PRINT"I'M CARRYING:":K$="NOTHING":FORZ=0TOIL:IFIA(Z)<>-1THEN910 ELSEGOSUB280
900 Print CHR$(32*-(POS>0) + TP$ + "." : K$=""
910 NEXT:PRINTK$:IFPOS PRINT':GOTO960 ELSEPRINT:GOTO960

REM 67. SET0 (Sets the flag-bit numbered 0)
920 P=0:GOTO 800

REM 68. CLR0 (Clears the flag-bit numbered 0)
930 P=0:GOTO 820

REM 69. FILL (Re-fill the artificial light source (obj 9) and pick it up)
940 LX=LT:IA(9)=-1:GOTO 960

REM 70. CLS
950 Cls : GOTO 960

REM Next command
960 NEXT Y

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

Sub read_data(f$)
  f$ = "pirate.dat"
  Open f$ For Input AS 1
  Input #1, il, cl, nl, rl, mx, ar, tt, ln, lt, ml, tr
'1270 DIM NV(1)
  Dim ca(cl, 7)      ' action table
  Dim nv_str$(nl, 1) ' vocabulary table - verbs at index 0, nouns at index 1
  Dim ia_str$(il)    ' object descriptions
  Dim ia(il)         ' object locations
  Dim rs$(rl)        ' room descriptions
  Dim rm(rl, 5)      ' room exits: N, S, E, W, U, D
  Dim ms$(ml)        ' messages table
'Dim NT$(1)
'Dim I2(IL)
'Dim x, y

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
REM 1295 P.NV$(X,Y),NV$(X+1,Y),NV$(X+2,Y),NV$(X+3,Y),NV$(X+4,Y),NV$(X+5,Y),NV$(X+6,Y),NV$(X+7,Y),NV$(X+8,Y),NV$(X+9,Y)
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

Sub intro()
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

Sub describe_current_location()
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

Sub action_go()
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

Sub dump_vocab()
  Local i
  For i = 0 To nl
    Print i, nv_str$(i, 0), nv_str$(i, 1)
  Next i
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
