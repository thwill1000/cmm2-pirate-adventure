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
Dim r, sf
Dim zi
Dim tp$
Dim v, w
Dim f, f1, f2, f3
Dim n, ll, ip
Dim kk$
Dim x ' 1st loop index
Dim y ' 2nd loop index

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
PROCb
INPUT "TELL ME WHAT TO DO? ", TP$
PRINT
GOSUB 170 ' Parse TP$
IF F Then PRINT "YOU USE WORD(S) I DON'T KNOW" : GOTO 140
150 GOSUB 360
If lx < 0 Then Print "LIGHT HAS RUN OUT!" : ia(9) = 0 : Goto 160
If lx < 25 Then Print "LIGHT RUNS OUT IN " Str$(lx) "TURNS!"
160 NV(0)=0 : GOSUB 360 : GOTO 140

REM Parse input 
170 K=0:NT$(0)="":NT$(1)=""
180
FOR X=1 TO LEN(TP$)
kk$=MID$(TP$,X,1)
IF kk$=" " THEN K=1 ELSE NT$(K) = LEFT$(NT$(K)+kk$,ln)
190 NEXT 
FOR X=0 TO 1
  NV(X)=0
  IF NT$(X)="" THEN
    Goto 230
  ELSE
    FOR Y=0 TO NL
    kk$=NV_STR$(Y,X)
    IF LEFT$(kk$,1) = "*" THEN kk$=MID$(kk$,2)
  ENDIF
200
  IF X = 1 Then
    IF Y < 7 Then kk$=LEFT$(kk$,ln)
  ENDIF
210
  IF NT$(X)=kk$ THEN
    NV(X)=Y
    y=Y
    Y=NL
    NEXT Y
    Y=y
  ELSE
    NEXT Y
    GOTO 230
  ENDIF
220 IF LEFT$(NV_STR$(NV(X),X),1) = "*" THEN NV(X)=NV(X)-1 : GOTO 220
230
NEXT X
F = NV(0) < 1 OR LEN(NT$(1)) > 0 AND NV(1) < 1
RETURN

REM *** Search for matching actions ***

REM Find a matching action
360 F2=-1:F=-1:F3=0
Print "DEBUG: foo"
IF NV(0) = 1 AND NV(1) < 7 THEN Goto 610
FOR X=0 TO CL
  V=INT(CA(X,0)/150)
  IF NV(0)=0 Then If V<>0 Then Return
370 IF NV(0)<>V THEN NEXT X : GOTO 990
N=CA(X,0)-V*150
380 IF NV(0) = 0 Then F = 0 : GOTO 385
GOTO 390
385 IF RND(100)<=N THEN Goto 400
NEXT X : GOTO 990
390 IF N<>NV(1) AND N<>0 THEN NEXT X : GOTO 990

REM Process the conditions                        K=condition code, LL="number"
400 F2=-1:F=0:F3=-1
FOR Y=1 TO 5
  W=CA(X,Y)
  LL=INT(W/20)
  K=W-LL*20
  F1=-1
'  Print K+1
  ON K+1 GOTO 550,430,450,470,490,500,510,520,530,540,410,420,440,460,480

REM 10. Is player carrying anything at all?
410 F1=-1:FOR Z=0 TO IL:IF IA(Z)=-1 THEN z=Z:Z=IL:NEXT Z:Z=z:GOTO 550 ELSE NEXT Z:F1=0:GOTO 550

REM 11. Is player carrying nothing?
420 F1=0:FOR Z=0 TO IL:IF IA(Z)=-1 THEN z=Z:Z=IL:NEXT Z:Z=z:GOTO 550 ELSE NEXT Z:F1=-1:GOTO 550

REM 1. Is player carrying object LL?
430 F1=IA(LL)=-1:GOTO 550

REM 12. Is obj LL not being carried by player and not in current room?
440 F1=IA(LL)<>-1 AND IA(LL)<>R:GOTO 550

REM 2. Is player in room with object LL?
450 F1=IA(LL)=R:GOTO 550

REM 13. Is obj LL not in the storeroom (room zero)?
460 F1=IA(LL)<>0:GOTO 550

REM 3. Is player in room with obj LL or carrying it?
470 F1=IA(LL)=R OR IA(LL)=-1:GOTO 550

REM 14. Is obj LL in storeroom (room zero)?
480 F1=IA(LL)=0:GOTO 550

REM 4. Is player in room LL?
490 F1=R=LL:GOTO 550

REM 5. Is player carrying obj LL, or obj LL isn't in current room?
500 F1=IA(LL)<>R:GOTO 550

REM 6. Is player not carrying obj LL?
510 F1=IA(LL)<>-1:GOTO 550

REM 7. Is player not in room LL?
520 F1=R<>LL:GOTO 550

REM 8. Is numbered flag-bit set?
530 F1=SF AND INT(2^LL+.5):F1=F1<>0:GOTO 550

REM 9. Is numbered flag-bit clear?
540 F1=SF AND INT(2^LL+.5):F1=F1=0:GOTO 550

550 F2=F2 AND F1
'IF F2 THEN NEXT Y ELSE y=Y:Y=5:NEXT Y:Y=y:NEXT X:GOTO 990
If f2 Then Next Y : Goto 560
Next X : Goto 990

REM Process the commands
560 IP=0:FOR Y=1 TO 4:K=INT((Y-1)/2)+6:ON Y GOTO 570,580,570,580
570 AC=INT(CA(X,K)/150):GOTO 590
580 AC=CA(X,K)-INT(CA(X,K)/150)*150
590 IF AC>101 THEN Goto 600 ELSE IF AC=0 THEN Goto 960 ELSE IF AC<52 THEN PROCp(MS$(AC)):GOTO 960:ELSE ON AC-51 GOTO 660,700,740,760,770,780,790,760,810,830,840,850,860,870,890,920,930,940,950,710,750
600 PROCp(MS$(AC-50)):GOTO 960

610 L=DF:IFL THEN L=DF AND IA(9)<>R AND IA(9)<>-1:IF L PRINT"DANGEROUS TO MOVE IN THE DARK!"
620 IFNV(1)<1PRINT"GIVE ME A DIRECTION TOO.":GOTO1040
630 K=RM(R,NV(1)-1):IFK>=1 ELSE IFL THENPRINT"I FELL DOWN AND BROKE MY NECK.":K=RL:DF=0:ELSE PRINT"I CAN'T GO IN THAT DIRECTION":GOTO1040
640 IF NOT L PROCcls
650 R=K:GOSUB240:GOTO1040

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
900 PROCp(CHR$(32*-(POS>0))+TP$+"."):PROCj:K$=""
910 NEXT:PRINTK$:IFPOS PRINT':GOTO960 ELSEPRINT:GOTO960

REM 67. SET0 (Sets the flag-bit numbered 0)
920 P=0:GOTO 800

REM 68. CLR0 (Clears the flag-bit numbered 0)
930 P=0:GOTO 820

REM 69. FILL (Re-fill the artificial light source (obj 9) and pick it up)
940 LX=LT:IA(9)=-1:GOTO 960

REM 70. CLS
950 PROCcls:GOTO 960

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
  If df Then
    ' Object 9 is the torch.
    If ia(9) <> -1 And ia(9) <> r Then
      Print "I CAN'T SEE, IT'S TOO DARK."
      Exit Sub
    EndIf
  EndIf

  If Left$(rs$(r), 1) = "*" Then
    Print Mid$(rs$(r), 2) + "."
  Else
    Print "I'm in a " + rs$(r) + "."
  EndIf

  250 K=-1
'  IF LEFT$(RS$(R),1)="*" THEN PROCp(MID$(RS$(R),2)+".") ELSE PROCp("I'M IN A "+RS$(R)+".")
  260 FOR zi=0 TO IL
    IF K Then IF IA(zi)=R Then PRINT "VISIBLE ITEMS HERE:" : K=0
  270 GOTO 300

  280 TP$=IA_STR$(zi)
  IF RIGHT$(TP$,1)<>"/" THEN Return
  FOR W=LEN(TP$)-1 TO 1 STEP-1
    IF MID$(TP$,W,1)="/" Then TP$=LEFT$(TP$,W-1) : Return
  NEXT W
  290 RETURN

  REM 300 IF IA(Z)<>R THEN320 ELSE GOSUB280:IFPOS+LEN(TP$)+3>63THENPRINT
  300 IF IA(zi)<>R THEN Goto 320 ELSE GOSUB 280
  '310 PROCp(CHR$(32*-(POS>0))+TP$+"."):PROCj
  PROCp(TP$+".") : PROCj
  320 NEXT : IF POS Then PRINT' ELSEPRINT
  330 K=-1
  FOR zi = 0 TO 5
    If K Then If RM(R,zi) <> 0 Then PRINT "OBVIOUS EXITS: "; : K=0
  340 IF RM(R,zi)<>0 Then PRINT NV$(zi+1,1);" ";
  350 NEXT zi
  IF POS Then Print
End Sub

Sub PROCcls()
  Cls
  Print
End Sub

REM Word-wrap
'2060 DEFPROCp($S%):LOCALA%,Z%,C%,N%,T%:N%=LEN$S%:A%=0:Z%=L%+1-POS:REPEATIFZ%>N%Z%=N%ELSEREPEATZ%=Z%-1:C%=S%?Z%:UNTILC%=32:IFZ%<A%Z%=A%+L%
'2070 T%=S%?Z%:S%?Z%=13:PRINT$(S%+A%);:S%?Z%=T%:VDU32,-8*(POS=1):IFZ%-A%<=L%ANDPOS PRINTELSEIFC%=32A%=A%+1ELSEZ%=Z%-1
'2080 A%=Z%+1:Z%=A%+L%+1:UNTILA%>=N%:IFPOS PRINT:ENDPROC ELSEENDPROC
Sub PROCp(s$)
  Print s$
End Sub

REM Backtrack to previous non-space char 
'2090 DEFPROCj:LOCALA%,C%:A%=&87:REPEATVDU8:C%=(USR(&FFF4)AND&FF00)DIV256:UNTILC%<>32:VDU9:ENDPROC
Sub PROCj()
End Sub

REM Is the previous line blank?
2100 DEFFNb:LOCALA%,C%,I%:VDU11,8:A%=&87:FORI%=1TOL%:VDU9:C%=(USR(&FFF4)AND&FF00)DIV256:IFC%=32NEXT:VDU10,13:=TRUE ELSEI%=L%:NEXT:VDU10,13:=FALSE
REM If at start of line, then print a blank line if prev line isn't blank.

'2120 DEFPROCb:IFPOS ENDPROC ELSE IFNOT(FNb)PRINT:ENDPROC ELSEENDPROC
Sub PROCb()
End Sub
