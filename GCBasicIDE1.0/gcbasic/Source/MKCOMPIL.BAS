DECLARE SUB Replace (VAR$, FIND$, REP$)
Programmer = 0

ID$ = LTRIM$(RTRIM$(UCASE$(COMMAND$)))
IF ID$ = "" THEN END

Assembler = 0 'gputils

ReadCommand:
IF LEFT$(ID$, 2) = "EL" THEN Programmer = 2: ID$ = MID$(ID$, 3): GOTO ReadCommand
IF LEFT$(ID$, 2) = "IC" THEN Programmer = 1: ID$ = MID$(ID$, 3): GOTO ReadCommand
IF LEFT$(ID$, 2) = "MP" THEN Assembler = 1: ID$ = MID$(ID$, 3): GOTO ReadCommand
IF RIGHT$(ID$, 1) <> "\" THEN ID$ = ID$ + "\"

'Find long directories
ReplaceLFNs:

IF INSTR(ID$, "\") <> 0 THEN
 T% = INSTR(ID$, "\")
 IF INSTR(T% + 1, ID$, "\") - T% > 8 THEN
  T$ = MID$(ID$, T% + 1)
  T$ = LEFT$(T$, INSTR(T$, "\") - 1)
  TN$ = LEFT$(T$, 6) + "~1"
  Replace ID$, T$, TN$
 END IF

 Replace ID$, "\", "#"
 GOTO ReplaceLFNs
END IF

DO WHILE INSTR(ID$, "#") <> 0: Replace ID$, "#", "\": LOOP
IF RIGHT$(ID$, 1) = "\" THEN ID$ = LEFT$(ID$, LEN(ID$) - 1)

OPEN ID$ + "\compile.bat" FOR OUTPUT AS #1

PRINT #1, "@ECHO OFF"
PRINT #1, LEFT$(ID$, 2)
PRINT #1, "cd " + MID$(ID$, 3)
PRINT #1, ""
'PRINT #1, "copy %1 compiled.txt>nul"
'PRINT #1, "GCBASIC /NC compiled.txt /A:makeasm.bat"
'PRINT #1, "del compiled.txt"
PRINT #1, "GCBASIC /NC %1 /O:compiled.asm /A:makeasm.bat"
PRINT #1, ""

CLOSE #1

OPEN ID$ + "\makeasm.bat" FOR OUTPUT AS #1

PRINT #1, "@ECHO OFF"
IF Assembler = 0 THEN PRINT #1, "C:\progra~1\gputils\bin\gpasm -i -w2 compiled.asm"
IF Assembler = 1 THEN PRINT #1, "C:\progra~1\microc~1\mpasms~1\MPASMWIN /c- /o- /q+ /l- /x- /w1 compiled.asm"
PRINT #1, "del compiled.cod>nul"
PRINT #1, ""
IF Programmer = 0 THEN
 PRINT #1, "REM Add the command to call your programmer application here"
 PRINT #1, 'REM (The file to download will be called COMPILED.HEX)"
END IF
IF Programmer = 1 THEN PRINT #1, "icprog -lcompiled.hex"
IF Programmer = 2 THEN PRINT #1, "usb -TPIC16F819 -Lcompiled.hex"

CLOSE #1

SUB Replace (VAR$, FIND$, REP$)

 VART$ = UCASE$(VAR$): FINDT$ = UCASE$(FIND$)
 IF INSTR(VART$, FINDT$) = 0 THEN VAR$ = VAR$ + REP$: EXIT SUB

 ND$ = LEFT$(VAR$, INSTR(VART$, FINDT$) - 1)
 ND$ = ND$ + REP$
 ND$ = ND$ + MID$(VAR$, INSTR(VART$, FINDT$) + LEN(FIND$))

 VAR$ = ND$
END SUB

