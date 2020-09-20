Option Explicit On
Option Default Integer

#Include "data.inc"
#Include "debug.inc"

Cls

Dim in$ = "pirate.dat"
Dim out$ = "pirate.dmp"
Dim fd = 1

dat.read(in$)

Open out$ For Output As #fd
Print #fd, "Data dump for '" in$ "'"
Print #fd
dbg.dump(fd)
Close #fd

End




