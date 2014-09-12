FI$ = COMMAND$

CLS
PRINT "NQC > GCBASIC music importer"
PRINT

IF FI$ = "" THEN
 PRINT "Please drag and drop a .nqc file produced by Brick's Music Studio onto"
 PRINT "this program."
 END
END IF

OPEN FI$ FOR INPUT AS #1
OPEN "song.txt" FOR OUTPUT AS #2

DO WHILE NOT EOF(1)
 LINE INPUT #1, DS$
 IF INSTR(DS$, ";") <> 0 THEN DS$ = LEFT$(DS$, INSTR(DS$, ";") - 1)
 IF INSTR(DS$, "PlayTone") <> 0 THEN
  Tone% = VAL(MID$(DS$, INSTR(DS$, "(") + 1))
  Length% = VAL(MID$(DS$, INSTR(DS$, ",") + 1))
  PRINT #2, "Tone " + MID$(STR$(Tone%), 2) + "," + STR$(Length%)
 END IF
 IF INSTR(DS$, "Wait") <> 0 THEN
  Length% = VAL(MID$(DS$, INSTR(DS$, "(") + 1))
  PRINT #2, "Wait" + STR$(Length%) + " 10ms"
 END IF
LOOP

CLOSE
PRINT "Done!"
PRINT
PRINT "Output in " + CURDIR$ + "\song.txt"

