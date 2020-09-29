' Copyright (c) 2019-20 Thomas Hugo Williams
' For Colour Maximite 2, MMBasic 5.05

Function str.rpad$(s$, x%)
  If Len(s$) < x% Then
    str.rpad$ = s$ + Space$(x% - Len(s$))
  Else
    str.rpad$ = s$
  EndIf
End Function
