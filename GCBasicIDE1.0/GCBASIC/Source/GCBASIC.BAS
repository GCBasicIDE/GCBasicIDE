DECLARE SUB FindCalcSubs (DS$)
DECLARE FUNCTION SubListLocation (SN$)
DECLARE SUB FindSubs (DS$)
DECLARE FUNCTION LocationOfSub (SubName$)
DECLARE FUNCTION Message$ (DS$)
DECLARE FUNCTION ShortName$ (DS$)
DECLARE FUNCTION IsDefined (Var$)
DECLARE SUB ProcessWords ()
DECLARE SUB CompileReadTable ()
DECLARE FUNCTION IsWord (DS$)
DECLARE FUNCTION GetByte$ (DS$, BS%)
DECLARE SUB InitCompiler (C$)
DECLARE SUB CompilePot ()
DECLARE FUNCTION AsmTidy$ (DS$)
DECLARE FUNCTION TypeOfVar$ (VarName$)
DECLARE SUB FindLargeVars ()
DECLARE FUNCTION WholeINSTR (DS$, N$)
DECLARE FUNCTION MakeDec (DS$)
DECLARE SUB ProcessArrays ()
DECLARE SUB TranslateArrays ()
DECLARE SUB FindArrays ()
DECLARE SUB DelSysVars ()
DECLARE SUB FindFreeRAM ()
DECLARE SUB CompilePulseOut ()
DECLARE SUB CompileConditions (Condition$, IfTrue$)
DECLARE FUNCTION CountOccur (Source$, Search$)
DECLARE SUB FixFunctions ()
DECLARE FUNCTION IsCalc (DS$)
DECLARE SUB TranslateFunctions ()
DECLARE SUB ReplaceConstantsLine (DS$)
DECLARE SUB Calculate (SUM$)
DECLARE SUB DelSubLine (T%)
DECLARE SUB RunScripts ()
DECLARE SUB AddSubLine (DS$, T%)
DECLARE FUNCTION CheckLine (DS$)
DECLARE SUB AddBankCommands ()
DECLARE SUB AddLine (DS$, T%)
DECLARE SUB CalcConfig ()
DECLARE SUB CompileCalc (SUM$)
DECLARE SUB CompileDir ()
DECLARE SUB CompileDo ()
DECLARE SUB CompileFor ()
DECLARE SUB CompileIF ()
DECLARE SUB CompileRotate ()
DECLARE SUB CompileSet ()
DECLARE SUB CompileVars ()
DECLARE SUB CompileWait ()
DECLARE SUB DelLine (T%)
DECLARE SUB FindRequiredSubs ()
DECLARE FUNCTION IsConst (T$)
DECLARE SUB OptimiseIF ()
DECLARE SUB RemIfDefs ()
DECLARE SUB Replace (Var$, Find$, REP$)
DECLARE SUB ReplaceConstants ()
DECLARE SUB SortVarList ()
DECLARE SUB SplitLines ()
DECLARE SUB SubParams ()
DECLARE SUB PreProcessor ()
DECLARE SUB PrepareBuiltIn ()
DECLARE SUB ShowBlock (DS$)
DECLARE FUNCTION CompleteINSTR ()
DECLARE SUB SCICONV (ST$)
DECLARE SUB Add18FBanks
DECLARE SUB AddSysVarBits
DECLARE SUB CompileRepeat
DECLARE SUB CompileSelect
DECLARE SUB BuildMemoryMap
DECLARE SUB ReadChipData

'    GCBASIC - A BASIC Compiler for PIC microcontrollers
'    Copyright (C) 2006-2007 Hugh Considine
'
'    This program is free software; you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation; either version 2 of the License, or
'    (at your option) any later version.
'
'    This program is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with this program; if not, write to the Free Software
'    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
'
'If you have any questions, please email me: hconsidine@bigpond.com

'Initialise
'Misc Vars
COMMON SHARED APC%, ICC%, DFC%, VLC%, SBC%, SDC%, SLC%, IFC%, ERC%, WSC%, FLC%, DLC%, ARC%, SSC%, SASC%, POC%, COC%, DTC%, LVC%, BVC%
COMMON SHARED CSC%, CV%, COSC%, MemSize%, FreeRAM%, FoundCount%, PotFound%, ThisCalc%, LongPresent%, DivMultPresent%
COMMON SHARED ChipName$, ChipMhz, OSC$, CONFIG$, CONFIG2$, Intrpt$, ChipRam%, ConfWords%, DataPass%, ChipFamily%, PSP%
COMMON SHARED FI$, OFI$, ID$, VBS%, Version$, SVC%, SVBC%, ProgDir$, CLD$, MakeASM$, ProgWillFail%, MSGC%, SendToPIC$
COMMON SHARED UserInt%, PauseOnErr%, OldVLC%, USDC%, MRC%, GCGB%

COMMON SHARED TempData$(), CheckTemp$(), ConfigOp$(), ConfigLoc%(), gcERR$(), FILE$()
COMMON SHARED VARLIST$(), VarType$()
COMMON SHARED StringStore$(), DataTables$(), DataTable%()

COMMON SHARED PROG$()
COMMON SHARED Messages$()
COMMON SHARED SUBCODE$()

CLOSE
IF DIR$("ERRORS.TXT") <> "" THEN KILL "ERRORS.TXT"

'Code Array
'$DYNAMIC
DIM SHARED PROG$(20000): APC% = 0

'Sub arrays
'$DYNAMIC
DIM SHARED SUBLIST$(500, 1 TO 2): SLC% = 0
DIM SHARED SUBCODE$(10000), SUBDATA$(500, 1 TO 4): SBC% = 0: SDC% = 0

'Processing Arrays
'$DYNAMIC
DIM SHARED CallEvery$(50): CEC% = 0
DIM SHARED gcDEF$(400, 1 TO 3): DFC% = 0
DIM SHARED gcINC$(25, 1 TO 2): ICC% = 0
DIM SHARED VARLIST$(4000): VLC% = 0
DIM SHARED VarType$(1000)
DIM SHARED ArrayData$(50, 1 TO 4): ARC% = 0 '1 - name, 2 - start, 3 - size, 4 - dim/sub
DIM SHARED TempData$(300)
DIM SHARED CheckTemp$(300)
DIM SHARED gcERR$(50): ERC% = 0
DIM SHARED SysVars$(1000, 1 TO 2)
DIM SHARED SysVarBits$(8000, 1 TO 3) '1 - name, 2 - location, 3 - var
DIM SHARED FILE$(300)
DIM SHARED FreeMem(1) AS INTEGER
DIM SHARED VarLoc(1) AS INTEGER
DIM SHARED MemRanges$(20): MRC% = 0
DIM SHARED StringStore$(0 To 500): SSC% = 0: SASC% = 0
DIM SHARED ConfigOp$(200): COC% = 0
DIM SHARED ConfigLoc%(200)
DIM SHARED DataTables$(50): DTC% = 0
DIM SHARED DataTable%(50, 255)
DIM SHARED DataTableUsed%(50)
DIM SHARED LargeVars$(1000, 1 TO 2): LVC% = 0
DIM SHARED Messages$(1 TO 2, 200): MSGC% = 0

WSC% = 0
Star80$ = ";********************************************************************************"
LongPresent% = 0
DivMultPresent% = 0

'Set version
Version$ = "0.9 10/2/2007"

'Clear program failure warning
ProgWillFail% = 0

'Get file to compile from COMMAND$
C$ = COMMAND$

'Show startup messages, and read COMMAND$
StartTime! = TIMER
InitCompiler COMMAND$

'Load files, and tidy them up
PreProcessor
MainProgramSize% = APC%

IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("FindLargeVars")
FindLargeVars

'Convert functions to subs with values returned
IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("TranslateFunctions")
TranslateFunctions

IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("FindRequiredSubs")
FindRequiredSubs

'Add code to prevent program continuing into subs
APC% = APC% + 1: PROG$(APC%) = "BASPROGRAMEND"
APC% = APC% + 1: PROG$(APC%) = " sleep"
APC% = APC% + 1: PROG$(APC%) = " goto $"
APC% = APC% + 1: PROG$(APC%) = ""

'Add interrupt routine
If UserInt% = 1 THEN
 APC% = APC% + 1: PROG$(APC%) = Star80$
 APC% = APC% + 1: PROG$(APC%) = ""
 APC% = APC% + 1: PROG$(APC%) = "; User interrupt routine"
 APC% = APC% + 1: PROG$(APC%) = "INTERRUPT"
 T% = LocationOfSub("INTERRUPT")
 S% = VAL(SUBDATA$(T%, 2)): E% = VAL(SUBDATA$(T%, 3))
 FOR PD% = S% TO E%
  T$ = SUBCODE$(PD%)
  IF LEFT$(T$, 8) = "EXIT SUB" THEN T$ = " retfie"
  APC% = APC% + 1: PROG$(APC%) = T$
 NEXT
 APC% = APC% + 1: PROG$(APC%) = " retfie"
 APC% = APC% + 1: PROG$(APC%) = ""
END IF

'Add string lookup tables
IF SSC% <> 0 THEN
 IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("StringTable")
 VLC% = VLC% + 1: VARLIST$(VLC%) = "StringPointer"
 APC% = APC% + 1: PROG$(APC%) = Star80$
 APC% = APC% + 1: PROG$(APC%) = ""
 APC% = APC% + 1: PROG$(APC%) = "; String Lookup Tables"
 FOR PD% = 1 TO SSC%

  TL$ = "StringTable" + STR$(PD%)
  DS$ = StringStore$(PD%)
  APC% = APC% + 1: PROG$(APC%) = "StringLookup" + STR$(PD%)
  IF ChipFamily% = 12 THEN
   APC% = APC% + 1: PROG$(APC%) = " movf StringPointer, W"
   APC% = APC% + 1: PROG$(APC%) = " addwf PCL, F"
  END IF
  IF ChipFamily% = 14 THEN
   VLC% = VLC% + 1: VARLIST$(VLC%) = "DataPointer"
   APC% = APC% + 1: PROG$(APC%) = " bcf STATUS, C"
   APC% = APC% + 1: PROG$(APC%) = " movf StringPointer, W"
   APC% = APC% + 1: PROG$(APC%) = " addlw 1"
   APC% = APC% + 1: PROG$(APC%) = " addlw low " + TL$
   APC% = APC% + 1: PROG$(APC%) = " movwf DataPointer"
   APC% = APC% + 1: PROG$(APC%) = " movlw high " + TL$
   APC% = APC% + 1: PROG$(APC%) = " btfsc STATUS, C"
   APC% = APC% + 1: PROG$(APC%) = " addlw 1"
   APC% = APC% + 1: PROG$(APC%) = " movwf PCLATH"
   APC% = APC% + 1: PROG$(APC%) = " movf DataPointer, W"
   APC% = APC% + 1: PROG$(APC%) = TL$
   APC% = APC% + 1: PROG$(APC%) = " movwf PCL"
  END IF
  IF ChipFamily% = 16 THEN
   APC% = APC% + 1: PROG$(APC%) = " bcf STATUS, C"
   APC% = APC% + 1: PROG$(APC%) = " movlw high " + TL$
   APC% = APC% + 1: PROG$(APC%) = " movwf TBLPTRH"
   APC% = APC% + 1: PROG$(APC%) = " movlw low " + TL$
   APC% = APC% + 1: PROG$(APC%) = " addlw 1"
   APC% = APC% + 1: PROG$(APC%) = " addwf StringPointer, W"
   APC% = APC% + 1: PROG$(APC%) = " movwf TBLPTRL"
   APC% = APC% + 1: PROG$(APC%) = " btfsc STATUS, C"
   APC% = APC% + 1: PROG$(APC%) = " incf TBLPTRH, F"
   APC% = APC% + 1: PROG$(APC%) = " tblrd*"
   APC% = APC% + 1: PROG$(APC%) = " movf TABLAT, W"
   APC% = APC% + 1: PROG$(APC%) = " return"
   APC% = APC% + 1: PROG$(APC%) = TL$
   APC% = APC% + 1: PROG$(APC%) = " dw " + STR$(LEN(DS$)*256 + LEN(DS$)) + "," + CHR$(34) + DS$ + CHR$(34)
   APC% = APC% + 1: PROG$(APC%) = ""
  END IF
  
  If ChipFamily% <> 16 THEN
   APC% = APC% + 1: PROG$(APC%) = " retlw " + STR$(LEN(DS$))
   FOR SP% = 1 TO LEN(DS$)
    APC% = APC% + 1
    T$ = " retlw " + STR$(ASC(MID$(DS$, SP%, 1)))
    IF MID$(DS$, SP%, 1) <> "=" THEN T$ = T$ + CHR$(9) + ";" + MID$(DS$, SP%, 1)
    IF MID$(DS$, SP%, 1) = "=" THEN T$ = T$ + CHR$(9) + "; (equals)"
    PROG$(APC%) = T$
   NEXT SP%
   APC% = APC% + 1: PROG$(APC%) = " return"
   APC% = APC% + 1: PROG$(APC%) = ""
  END IF
 NEXT PD%
END IF

'Add data tables
IF DTC% > 0 THEN
 IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("DataTable")
 APC% = APC% + 1: PROG$(APC%) = Star80$
 APC% = APC% + 1: PROG$(APC%) = ""
 APC% = APC% + 1: PROG$(APC%) = "; Data Lookup Tables"
 FOR PD% = 1 TO DTC%
  If DataTableUsed%(PD%) = 1 THEN   
   TL$ = "Table" + DataTables$(PD%)
   APC% = APC% + 1: PROG$(APC%) = DataTables$(PD%)
   VLC% = VLC% + 1: VARLIST$(VLC%) = "DataPointer"
   IF ChipFamily% = 12 THEN
    APC% = APC% + 1: PROG$(APC%) = " movf DataPointer, W"
    APC% = APC% + 1: PROG$(APC%) = " addwf PCL, F"
   END IF 
   IF ChipFamily% = 14 THEN
    APC% = APC% + 1: PROG$(APC%) = " movlw " + STR$(DataTable%(PD%, 0) + 1)
    APC% = APC% + 1: PROG$(APC%) = " subwf DataPointer, W"
    APC% = APC% + 1: PROG$(APC%) = " btfsc STATUS, C"
    APC% = APC% + 1: PROG$(APC%) = " retlw 0"
    APC% = APC% + 1: PROG$(APC%) = " movf DataPointer, W"
    APC% = APC% + 1: PROG$(APC%) = " addlw 1"
    APC% = APC% + 1: PROG$(APC%) = " bcf STATUS, C"
    APC% = APC% + 1: PROG$(APC%) = " addlw low " + TL$
    APC% = APC% + 1: PROG$(APC%) = " movwf DataPointer"
    APC% = APC% + 1: PROG$(APC%) = " movlw high " + TL$
    APC% = APC% + 1: PROG$(APC%) = " btfsc STATUS, C"
    APC% = APC% + 1: PROG$(APC%) = " addlw 1"
    APC% = APC% + 1: PROG$(APC%) = " movwf PCLATH"
    APC% = APC% + 1: PROG$(APC%) = " movf DataPointer, W"
    APC% = APC% + 1: PROG$(APC%) = TL$
    APC% = APC% + 1: PROG$(APC%) = " movwf PCL"
   END IF
   IF ChipFamily% = 16 THEN
    APC% = APC% + 1: PROG$(APC%) = " movlw " + STR$(DataTable%(PD%, 0) + 1)
    APC% = APC% + 1: PROG$(APC%) = " cpfslt DataPointer"
    APC% = APC% + 1: PROG$(APC%) = " retlw 0"
    
    APC% = APC% + 1: PROG$(APC%) = " movf DataPointer, W"
    APC% = APC% + 1: PROG$(APC%) = " bcf STATUS, C"
    APC% = APC% + 1: PROG$(APC%) = " addlw low " + TL$
    APC% = APC% + 1: PROG$(APC%) = " movwf TBLPTRL"
    APC% = APC% + 1: PROG$(APC%) = " movlw high " + TL$
    APC% = APC% + 1: PROG$(APC%) = " movwf TBLPTRH"
    APC% = APC% + 1: PROG$(APC%) = " btfsc STATUS, C"
    APC% = APC% + 1: PROG$(APC%) = " incf TBLPTRH, F"
    APC% = APC% + 1: PROG$(APC%) = " tblrd*"
    APC% = APC% + 1: PROG$(APC%) = " movf TABLAT, W"
    APC% = APC% + 1: PROG$(APC%) = " return"
    APC% = APC% + 1: PROG$(APC%) = TL$
    T$ = ""
    FOR SP% = 0 TO DataTable%(PD%, 0)
    T$ = T$ + "," + STR$(DataTable%(PD%, SP%))
    NEXT SP%
    APC% = APC% + 1: PROG$(APC%) = " db " + MID$(T$, 2)
    APC% = APC% + 1: PROG$(APC%) = ""
   END IF
   If ChipFamily% <> 16 Then
    FOR SP% = 0 TO DataTable%(PD%, 0)
     APC% = APC% + 1: PROG$(APC%) = " retlw " + STR$(DataTable%(PD%, SP%))
    NEXT SP%
    APC% = APC% + 1: PROG$(APC%) = " return"
    APC% = APC% + 1: PROG$(APC%) = ""
   END IF
  END IF
  
 NEXT PD%
END IF

'Add required subroutines
IF SLC% > 0 THEN
 IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("AddingSubs")
 APC% = APC% + 1: PROG$(APC%) = Star80$
 APC% = APC% + 1: PROG$(APC%) = ";Subroutines included in program"
 APC% = APC% + 1: PROG$(APC%) = Star80$
 FOR PD% = 1 TO SLC%
  IF VBS% = 1 THEN PRINT SPC(10); SUBLIST$(PD%, 1)
  APC% = APC% + 1: PROG$(APC%) = ""
  APC% = APC% + 1: PROG$(APC%) = SUBLIST$(PD%, 1)
  T% = VAL(SUBLIST$(PD%, 2))
  S% = VAL(SUBDATA$(T%, 2)): E% = VAL(SUBDATA$(T%, 3))
  FOR CD% = S% TO E%
   IF SUBCODE$(CD%) <> "" THEN APC% = APC% + 1: PROG$(APC%) = SUBCODE$(CD%)
   SUBCODE$(CD%) = ""
  NEXT CD%
  APC% = APC% + 1: PROG$(APC%) = " return"
  APC% = APC% + 1: PROG$(APC%) = ""
  APC% = APC% + 1: PROG$(APC%) = Star80$

 NEXT PD%
END IF

'Compile FOR commands
IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("CompileFor")
CompileFor

IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("FindArrays")
FindArrays

'Compile Sub parameters
IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("SubParams")
SubParams
SortVarList

IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("ProcessArrays")
ProcessArrays

'Replace +=, -=
FOR PD% = 1 TO APC%
 DS$ = PROG$(PD%)
 IF INSTR(DS$, "+=") <> 0 THEN
  V$ = LEFT$(DS$, INSTR(DS$, "+=") - 1)
  T$ = MID$(DS$, INSTR(DS$, "+=") + 2)
  PROG$(PD%) = V$ + "=" + V$ + "+" + T$
 END IF
 IF INSTR(DS$, "-=") <> 0 THEN
  V$ = LEFT$(DS$, INSTR(DS$, "-=") - 1)
  T$ = MID$(DS$, INSTR(DS$, "-=") + 2)
  PROG$(PD%) = V$ + "=" + V$ + "-" + T$
 END IF
NEXT PD%

IF VBS% = 1 THEN PRINT: PRINT SPC(5); Message$("AddBits")
AddSysVarBits

IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("CompComs")

IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileReadTable");
CompileReadTable
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

IF VBS% = 1 THEN PRINT SPC(10); Message$("CompilePot");
CompilePot
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

'Compile DO commands
IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileDO");
CompileDo
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

'Compile misc. commands
IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileDir");
CompileDir
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

IF VBS% = 1 THEN PRINT SPC(10); Message$("CompilePulseOut");
CompilePulseOut
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileWait");
CompileWait
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileSet");
CompileSet
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileRotate");
CompileRotate
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileRepeat");
CompileRepeat
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileSelect");
CompileSelect
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

'Compile IF commands
IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileIF");
CompileIF
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

'Compile variable commands
IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileVars");
CompileVars
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileExitSub");
FoundCount% = 0
FOR PD% = 1 TO APC%
 IF LEFT$(UCASE$(PROG$(PD%)), 8) = "EXIT SUB" THEN PROG$(PD%) = " return": FoundCount% = FoundCount% + 1
 IF LEFT$(UCASE$(PROG$(PD%)), 13) = "EXIT FUNCTION" THEN PROG$(PD%) = " return": FoundCount% = FoundCount% + 1
 IF LEFT$(UCASE$(PROG$(PD%)), 4) = "END " THEN 
  T$ = PROG$(PD%)
  IF INSTR(T$, ";") <> 0 THEN T$ = TRIM$(LEFT$(T$, INSTR(T$, ";") - 1))
  IF T$ = "END" THEN PROG$(PD%) = " goto BASPROGRAMEND": FoundCount% = FoundCount% + 1
 END IF
NEXT PD%
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"
SortVarList

'Compile GOSUBs and RETURNs
FoundCount% = 0
IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileGOSUB");
FOR CD% = 1 TO APC%
 T% = 0
 IF LEFT$(UCASE$(PROG$(CD%)), 6) = "GOSUB " THEN T% = 1: Replace PROG$(CD%), "GOSUB", " call": FoundCount% = FoundCount% + 1
 IF LEFT$(UCASE$(PROG$(CD%)), 6) = "RETURN" THEN T% = 1: Replace PROG$(CD%), "RETURN", " return": FoundCount% = FoundCount% + 1
 IF T% = 1 AND INSTR(PROG$(CD%), ";?F") <> 0 THEN PROG$(CD%) = RTRIM$(LEFT$(PROG$(CD%), INSTR(PROG$(CD%), ";?F") - 1))
NEXT CD%
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"

'Compile GOTOs and labels
FoundCount% = 0
IF VBS% = 1 THEN PRINT SPC(10); Message$("CompileGOTO");
FOR CD% = 1 TO APC%
 T% = 0
 IF LEFT$(UCASE$(PROG$(CD%)), 4) = "GOTO" THEN T% = 1: Replace PROG$(CD%), "GOTO", " goto": FoundCount% = FoundCount% + 1
 IF T% = 1 AND INSTR(PROG$(CD%), ";?F") <> 0 THEN PROG$(CD%) = RTRIM$(LEFT$(PROG$(CD%), INSTR(PROG$(CD%), ";?F") - 1))
NEXT CD%
FOR CD% = 1 TO APC%
 IF RIGHT$(PROG$(CD%), 1) = ":" THEN Replace PROG$(CD%), ":", "": FoundCount% = FoundCount% + 1
NEXT CD%
IF VBS% = 1 THEN PRINT Message$("Found2") + STR$(FoundCount%) + ")"

IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("TidyCode")

'Delete variables that are defined by the assembler from the list of
'variables to define (PORTA, STATUS, etc.)
DelSysVars

'Check for invalid variable names
FOR PD% = 1 TO VLC%
 DS$ = VARLIST$(PD%)
 T% = 0
 IF INSTR(DS$, " ") <> 0 THEN T% = 1
 'IF LEN(DS$) = 1 THEN T% = 1
 IF DS$ = "W" THEN T% = 1
 IF DS$ = "F" THEN T% = 1
 IF DS$ = "B" THEN T% = 1
 IF T% = 1 THEN
  ERC% = ERC% + 1
  T$ = Message$("BadVarName")
  Replace T$, "%var%", DS$
  gcERR$(ERC%) = T$
 END IF
NEXT PD%

'Check that there is enough RAM
IF VLC% > ChipRAM% THEN ERC% = ERC% + 1: gcERR$(ERC%) = Message$("ExcessVars")

'Determine the correct setting for the CONFIG directive
IF VBS% = 1 THEN PRINT SPC(10); Message$("CalcConfig")
CalcConfig

'Set Bank
IF VBS% = 1 THEN PRINT SPC(10); Message$("AddBankCommands");
AddBankCommands
IF VBS% = 1 THEN PRINT

'Tidy up code
IF VBS% = 1 THEN PRINT SPC(10); Message$("OptimiseVars")
PD% = 0
TidyVarClear:
 PD% = PD% + 1
 IF PROG$(PD%) = " movlw 0" AND LEFT$(PROG$(PD% + 1), 7) = " movwf " THEN
  PROG$(PD%) = " clrf" + MID$(PROG$(PD% + 1), 7)
  DelLine PD% + 1
  PD% = PD% - 1
 END IF
IF PD% < APC% THEN GOTO TidyVarClear
IF ChipFamily% = 12 THEN
 FOR PD% = 1 to APC%
  IF INSTR(LCASE$(PROG$(PD%)), "return") <> 0 THEN PROG$(PD%) = " retlw 0"
 NEXT
END IF
IF ChipFamily% = 16 THEN
 FOR PD% = 1 to APC%
  IF INSTR(LCASE$(PROG$(PD%)), "clrw") <> 0 THEN PROG$(PD%) = " clrf WREG"
 NEXT
END IF

IF VBS% = 1 THEN PRINT SPC(10); Message$("OptimiseIf")
OptimiseIF

IF VBS% = 1 THEN PRINT SPC(10); Message$("FixFunctions")
FixFunctions

'Add banking mode to PIC16 commands
IF ChipFamily% = 16 THEN
 IF VBS% = 1 THEN PRINT SPC(10); Message$("Add18FBanks")
 Add18FBanks
END IF

'Check for uncompiled lines
FOR PD% = 1 to APC%
 IF INSTR(PROG$(PD%), ";?F") <> 0 THEN
  ERC% = ERC% + 1
  Origin$ = MID$(PROG$(PD%), INSTR(PROG$(PD%), ";?F"))
  gcERR$(ERC%) = Origin$ + Message$("SynErr")
 END IF
NEXT

'Final tidy of varlist 
OldVLC% = 0
SortVarList

IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("WritingASM")
'Write .ASM program
OPEN OFI$ FOR OUTPUT AS #1
PRINT #1, ";Program compiled by Great Cow BASIC (" + Version$ + ")"
IF GCGB% = 0 THEN
    PRINT #1, ";Need help? See the GCBASIC forums at http://sourceforge.net/forum/?group_id=169286,"
    PRINT #1, ";check the documentation or email hconsidine@bigpond.com."
END IF
PRINT #1, ""
PRINT #1, Star80$
PRINT #1, ""
PRINT #1, ";Set up the assembler options (Chip type, clock source, other bits and pieces)"
PRINT #1, " LIST p=" + ChipName$ + ", r=DEC"
PRINT #1, "#include <P" + ChipName$ + ".inc>"
IF ChipFamily% <> 16 THEN
 IF ConfWords% < 2 THEN PRINT #1, " __CONFIG " + CONFIG$
 IF ConfWords% > 1 THEN 
     PRINT #1, " __CONFIG _CONFIG1, " + CONFIG$
     IF CONFIG2$ <> "" THEN PRINT #1, " __CONFIG _CONFIG2, " + CONFIG2$
 END IF
END IF
IF ChipFamily% = 16 THEN PRINT #1, " CONFIG " + CONFIG$
PRINT #1, ""
IF VLC% > 0 THEN
 PRINT #1, Star80$
 PRINT #1, ""
 PRINT #1, ";Set aside memory locations for variables"
 FOR PD% = 1 TO VLC%
  VL% = VarLoc(PD%)
  'PRINT #1, VARLIST$(PD%) + " equ " + STR$(VL%)
  T$ = VARLIST$(PD%) + " equ " + STR$(VL%)
  PRINT #1, AsmTidy$(T$)
 NEXT PD%
 PRINT #1, ""
END IF
PRINT #1, Star80$
PRINT #1, ""
PRINT #1, ";Jump to initialisation code when PIC is reset"
PRINT #1, AsmTidy$(" ORG 0")
PRINT #1, AsmTidy$(" call INITSYS")
PRINT #1, AsmTidy$(" goto SystemInitialise")
PRINT #1, ""
PRINT #1, Star80$
PRINT #1, ""

'Check if interrupts are supported
T$ = "INTCON"
IntSupport% = 0
FOR PD% = 1 TO SVC%
 IF UCASE$(SysVars$(PD%, 1)) = UCASE$(T$) THEN IntSupport% = 1: EXIT FOR
NEXT PD%

IF IntSupport% = 0 THEN Intrpt$ = ""
IF IntSupport% = 1 THEN
 PRINT #1, ";Interrupt vector"
IF ChipFamily% <> 16 THEN PRINT #1, AsmTidy$(" ORG 4")
IF ChipFamily% = 16 THEN PRINT #1, AsmTidy$(" ORG 8")
  
 'Add ON ... GOSUB code
 
 'Call user interrupt
 IF UserInt% = 1 THEN PRINT #1, AsmTidy$(" goto INTERRUPT")
 IF UserInt% = 0 THEN
  IF ChipFamily% <> 16 THEN PRINT #1, AsmTidy$(" retfie")
  IF ChipFamily% = 16 THEN PRINT #1, AsmTidy$(" retfie 0")
 END IF
 PRINT #1, ""
 PRINT #1, Star80$
 PRINT #1, ""
END IF

PRINT #1, ";Various initialisation routines, automatically called by GCBASIC"
PRINT #1, "SystemInitialise"
FOR PD% = 1 TO ICC%
 T$ = gcINC$(PD%, 2)
 T$ = LTRIM$(RTRIM$(T$))
 IF INSTR(T$, ";") = 0 AND T$ <> "" THEN PRINT #1, CHR$(9) + "call" + CHR$(9) + T$
NEXT PD%
PRINT #1, ""
IF Intrpt$ <> "" THEN
 PRINT #1, ";Initialise interrupt"
 PRINT #1, AsmTidy$(" clrf INTCON")
 PRINT #1, AsmTidy$(" clrf TMR0")
 PRINT #1, AsmTidy$(" banksel OPTION_REG")
 PRINT #1, AsmTidy$(" bcf OPTION_REG, T0CS")
 PRINT #1, AsmTidy$(" bcf OPTION_REG, PSA")

 'Set prescaler here

 PRINT #1, AsmTidy$(" banksel INTCON")
 PRINT #1, AsmTidy$(" bsf INTCON, T0IE")
 PRINT #1, AsmTidy$(" bsf INTCON, GIE")
 PRINT #1, ""
END IF
PRINT #1, Star80$
PRINT #1, ""
PRINT #1, ";Start of the main program"
FOR PD% = 1 TO APC%
 PRINT #1, AsmTidy$(PROG$(PD%))
NEXT PD%
PRINT #1, ""
PRINT #1, " END"
CLOSE #1

IF ERC% = 0 THEN
 PRINT Message$("Success");
 IF VBS% = 0 THEN PRINT
 IF VBS% = 1 THEN
  PRINT Message$("CompTime") + STR$(INT((TIMER - StartTime!)*1000)/1000) + Message$("CompSecs")
  PRINT
  PRINT Message$("Summary")
  PRINT SPC(5); Message$("DataRead")
  PRINT SPC(10); Message$("InLines") + STR$(SDC% + MainProgramSize%) + " (10000 max)"
  PRINT SPC(10); Message$("Vars") + STR$(VLC%) + " (500 max)"
  PRINT SPC(10); Message$("Consts") + STR$(DFC%) + " (400 max)"
  PRINT SPC(10); Message$("Subs") + STR$(SBC%) + " (500 max)"
  PRINT SPC(5); Message$("AssemblyWritten") + STR$(APC%) + " (6000 max)"
 END IF
END IF

'Sort error list
SortErrorList:
L% = 0
FOR PD% = 1 TO ERC% - 1
 IF INSTR(gcERR$(PD%), ";?F") <> 0 THEN
  T$ = gcERR$(PD%)
  F1% = VAL(MID$(T$, INSTR(T$, "F")+1))
  L1% = VAL(MID$(T$, INSTR(T$, "L")+1))
 END IF
 IF INSTR(gcERR$(PD% + 1), ";?F") <> 0 THEN
  T$ = gcERR$(PD% + 1)
  F2% = VAL(MID$(T$, INSTR(T$, "F")+1))
  L2% = VAL(MID$(T$, INSTR(T$, "L")+1))
 END IF
 
 IF F1% > F2% THEN
  T$ = gcERR$(PD%)
  gcERR$(PD%) = gcERR$(PD% + 1)
  gcERR$(PD% + 1) = T$
  L% = 1
 END IF
 IF F1% <= F2% AND L1% > L2% THEN
  T$ = gcERR$(PD%)
  gcERR$(PD%) = gcERR$(PD% + 1)
  gcERR$(PD% + 1) = T$
  L% = 1
 END IF
 
NEXT PD%
IF L% = 1 THEN GOTO SortErrorList

'Generate file:line references
FOR PD% = 1 to ERC%
 IF INSTR(gcERR$(PD%), ";?F") <> 0 THEN
  T$ = gcERR$(PD%)
  Replace T$, "?", ""
  M$ = TRIM$(MID$(T$, INSTR(T$, "?")+1))
  F% = VAL(MID$(T$, INSTR(T$, "F")+1))
  L% = VAL(MID$(T$, INSTR(T$, "L")+1))
  F$ = gcINC$(F%, 1)
  FOR T% = LEN(F$) to 1 step -1
   IF MID$(F$, T%, 1) = "\" THEN F$ = MID$(F$, T%+1): exit for
  NEXT
  gcERR$(PD%) = F$+" ("+STR$(L%)+"): " + M$
 END IF
NEXT

IF ProgWillFail% = 1 THEN COLOR 14, 4
'T$ = " has": IF ERC% > 1 THEN T$ = "s have"
'IF ProgWillFail% = 0 THEN T$ = "Error" + T$
'IF ProgWillFail% = 1 THEN T$ = "Serious error" + T$
IF ProgWillFail% = 0 AND ERC% = 1 THEN T$ = Message$("Error")
IF ProgWillFail% = 0 AND ERC% > 1 THEN T$ = Message$("Errors")
IF ProgWillFail% = 1 AND ERC% = 1 THEN T$ = Message$("BadError")
IF ProgWillFail% = 1 AND ERC% > 1 THEN T$ = Message$("BadErrors")

IF ERC% > 0 THEN
 PRINT T$
 PRINT
 FOR PD% = 1 TO ERC%
  DS$ = gcERR$(PD%)
ShowError:
  IF LEN(DS$) <= 77 THEN PRINT SPC(1); DS$
  IF LEN(DS$) > 77 THEN
   FOR T% = 77 TO 1 STEP -1
    T$ = MID$(DS$, T%, 1)
    IF T$ = " " THEN
     PRINT SPC(1); LEFT$(DS$, T%)
     DS$ = MID$(DS$, T% + 1)
     GOTO ShowError
    END IF
   NEXT T%
  END IF

 NEXT PD%
 PRINT
 PRINT Message$("ErrorLogged") + ID$ + "\ERRORS.TXT."
 OPEN ID$ + "\ERRORS.TXT" FOR OUTPUT AS #1
 PRINT #1, "GCBASIC error log for " + FI$ + " and included files."
 PRINT #1, "GCBASIC version: " + Version$
 PRINT #1, ""
 FOR PD% = 1 TO ERC%
  PRINT #1, gcERR$(PD%)
 NEXT PD%
 CLOSE
 PRINT
 IF PauseOnErr% = 1 THEN
  PRINT Message$("AnyKey")
  DO WHILE INKEY$ = "": SLEEP 10: LOOP
 END IF

END IF

COLOR 7, 0

'Assemble program
IF MakeASM$ <> "" AND ERC% = 0 THEN
 PRINT
 PRINT Message$("MakeASM")
 IF INSTR(UCASE$(MakeASM$), "%FILENAME%") <> 0 THEN Replace MakeASM$, "%FILENAME%", FI$
 IF INSTR(UCASE$(MakeASM$), "%CHIPMODEL%") <> 0 THEN Replace MakeASM$, "%CHIPMODEL%", ChipName$
 IF VBS% = 1 THEN PRINT SPC(5); Message$("Calling") + MakeASM$
 SHELL MakeASM$

 'Check that the assembly was successful
 T% = 0
 IF DIR$("compiled.err") <> "" THEN
   OPEN "compiled.err" for input as #1
   DO WHILE NOT EOF(1)
   LINE INPUT #1, DS$
   IF INSTR(UCASE$(DS$), "ERROR") <> 0 THEN 
    T% = T% + 1
    IF T% = 1 THEN PRINT Message$("AssemblyFailed"): PRINT
    PRINT DS$
   END IF
  LOOP
  CLOSE
  
  IF T% <> 0 AND PauseOnErr% = 1 THEN
   PRINT
   PRINT Message$("AnyKey")
   T$ = INKEY$
   DO WHILE INKEY$ = "": SLEEP 10: LOOP
  END IF
 END IF

END IF

'Download program
IF SendToPIC$ <> "" AND MakeASM$ <> "" AND ERC% = 0 AND T% = 0 THEN
 PRINT
 PRINT Message$("SendToPIC")
 IF INSTR(UCASE$(SendToPIC$), "%FILENAME%") <> 0 THEN Replace SendToPIC$, "%FILENAME%", FI$
 IF INSTR(UCASE$(SendToPIC$), "%CHIPMODEL%") <> 0 THEN Replace SendToPIC$, "%CHIPMODEL%", ChipName$
 IF VBS% = 1 THEN PRINT SPC(5); Message$("Calling") + SendToPIC$
 SHELL SendToPIC$
END IF

END

REM $STATIC
SUB Add18FBanks
  FOR PD% = 1 to APC%
  T$ = LCASE$(PROG$(PD%))
  
  IF INSTR(T$, "movff") = 0 AND INSTR(T$, ",access") = 0 AND INSTR(T$, ", access") = 0 AND T$ <> "" AND LEFT$(T$, 1) <> ";" THEN
   T$ = TRIM$(UCASE$(PROG$(PD%)))
   V$ = MID$(T$, INSTR(T$, " ") + 1)
   IF INSTR(V$, ",") <> 0 THEN V$ = LEFT$(V$, INSTR(V$, ",") - 1)
   V$ = TRIM$(V$)
   T$ = LEFT$(T$, INSTR(T$, " ")-1)
   IF INSTR(T$, "F") <> 0 THEN
    'Check if the variable being accessed is a SFR
    ConstFound% = 0
    FOR FC% = 1 TO SVC%
     IF SysVars$(FC%, 1) = V$ THEN ConstFound% = 1: EXIT FOR
    NEXT FC%
    'Add banking mode
    IF ConstFound% = 0 THEN PROG$(PD%) = PROG$(PD%) + ",BANKED"
    IF ConstFound% = 1 THEN PROG$(PD%) = PROG$(PD%) + ",ACCESS"
   END IF
  END IF
 NEXT
 
END SUB

REM $STATIC
SUB AddBankCommands

PD% = 0
Bank% = 0
BankMask% = 384: IF ChipFamily% = 16 THEN BankMask% = 65280

DetectBank:
PD% = PD% + 1
 T$ = PROG$(PD%)

 IF VBS% = 1 THEN
  LOCATE , 60: PRINT STR$(INT(PD% / APC% * 100)); "%";
 END IF

 IF T$ = " return" AND Bank% <> 0 THEN
  AddLine " banksel STATUS", PD%
  PD% = PD% + 1
  Bank% = 0
  GOTO NextBankSet
 END IF

 IF LEFT$(T$, 6) = " call " AND Bank% <> 0 THEN
  AddLine " banksel STATUS", PD%
  PD% = PD% + 1
  Bank% = 0
  GOTO NextBankSet
 END IF

 T2$ = UCASE$(TRIM$(T$))
 IF INSTR(T2$, " ") <> 0 THEN T2$ = LEFT$(T2$, INSTR(T2$, " ") - 1)

 IF INSTR(T2$, "F") <> 0 THEN
     
 'Check for SFRs outside of Bank 0
 'Not needed for 18F
 IF ChipFamily% <> 16 THEN
  FOR CD% = 1 TO SVC%
   IF WholeINSTR(T$, SysVars$(CD%, 1)) = 2 THEN
    NewBank% = VAL(SysVars$(CD%, 2)) AND BankMask%
    IF NewBank% <> Bank% THEN
     AddLine " banksel " + SysVars$(CD%, 1), PD%
     PD% = PD% + 1
     Bank% = NewBank%
     GOTO NextBankSet
    END IF
   END IF
  NEXT CD%
 END IF

 FOR CD% = 1 TO VLC%
  IF WholeINSTR(T$, VARLIST$(CD%)) = 2 THEN
   NewBank% = VarLoc(CD%) AND BankMask%
   IF NewBank% <> Bank% THEN
    AddLine " banksel " + VARLIST$(CD%), PD%
    PD% = PD% + 1
    Bank% = NewBank%
    GOTO NextBankSet
   END IF
  END IF
 NEXT CD%

 END IF

NextBankSet:

IF PD% < APC% THEN GOTO DetectBank

END SUB

SUB AddLine (DS$, T%)
FOR PD% = APC% TO T% STEP -1
 PROG$(PD% + 1) = PROG$(PD%)
NEXT PD%
APC% = APC% + 1
PROG$(T%) = DS$
END SUB

SUB AddSubLine (DS$, T%)

'Add line to list
FOR PD% = SDC% TO T% STEP -1
 SUBCODE$(PD% + 1) = SUBCODE$(PD%)
NEXT PD%
SDC% = SDC% + 1
SUBCODE$(T%) = DS$

T2% = T% - 1

'Adjust index
FOR PD% = 1 TO SBC%
 'IF VAL(SUBDATA$(PD%, 2)) > T% THEN SUBDATA$(PD%, 2) = STR$(VAL(SUBDATA$(PD%, 2)) + 1)
 'IF VAL(SUBDATA$(PD%, 3)) >= T2% THEN SUBDATA$(PD%, 3) = STR$(VAL(SUBDATA$(PD%, 3)) + 1)
 IF VAL(SUBDATA$(PD%, 2)) > T% THEN SUBDATA$(PD%, 2) = STR$(VAL(SUBDATA$(PD%, 2)) + 1)
 IF VAL(SUBDATA$(PD%, 3)) > T2% THEN SUBDATA$(PD%, 3) = STR$(VAL(SUBDATA$(PD%, 3)) + 1)
NEXT PD%

END SUB

SUB AddSysVarBits
 
 FOR PD% = 1 to APC%

  DS$ = PROG$(PD%)
  IF INSTR(DS$, ".") <> 0 THEN GOTO AddNextLineBits
  IF LEFT$(DS$, 1) = " " THEN GOTO AddNextLineBits

  'Only use for some commands
  T% = 0
  IF LEFT$(DS$, 4) = "SET " THEN T% = 1
  IF LEFT$(DS$, 4) = "DIR " THEN T% = 1
  IF LEFT$(DS$, 3) = "IF " THEN T% = 1
  IF LEFT$(DS$, 3) = "DO " THEN T% = 1
  IF LEFT$(DS$, 5) = "LOOP " THEN T% = 1
  IF INSTR(DS$, "=") <> 0 THEN T% = 1
  IF LEFT$(DS$, 11) = "WAIT UNTIL " THEN T% = 1
  IF LEFT$(DS$, 11) = "WAIT WHILE " THEN T% = 1
  IF T% = 0 THEN GOTO AddNextLineBits

  FOR SV% = 1 TO SVBC%
   T$ = SysVarBits$(SV%, 1)
   IF WholeINSTR(UCASE$(DS$), UCASE$(T$)) = 2 THEN
       'PRINT DS$
       IF UCASE$(T$) <> "DIR" AND (LEFT$(DS$, 4) <> "DIR " AND T$ <> "C") THEN
        Replace PROG$(PD%), T$, SysVarBits$(SV%, 3) + "." + T$
        GOTO AddNextLineBits
       END IF
   END IF
  NEXT
  
AddNextLineBits:
    
 NEXT
 
END SUB

FUNCTION AsmTidy$ (DS$)

T$ = DS$

IF LEFT$(T$, 1) = ";" THEN AsmTidy$ = T$: EXIT FUNCTION

IF LEFT$(T$, 13) = " movwf TRISIO" THEN
   
 S$ = "TRISIO"
 TRISSupport% = 0
 FOR PD% = 1 TO SVC%
  IF UCASE$(SysVars$(PD%, 1)) = UCASE$(S$) THEN TRISSupport% = 1: EXIT FOR
 NEXT PD%
 
 IF TRISSupport% = 0 THEN T$ = " tris GPIO"    

END IF

Replace T$, " ", CHR$(9)
Replace T$, " ", CHR$(9)

AsmTidy$ = T$

END FUNCTION

SUB BuildMemoryMap

'Dimension arrays
REDIM FreeMem(MemSize%+10) AS INTEGER
REDIM VarLoc(MemSize%+10) AS INTEGER

'Mark all locations as used
FOR PD% = 0 to MemSize% + 10
    FreeMem(PD%) = 1
NEXT

'Mark locations as free if specified by a range
FOR Range% = 1 to MRC%
    T$ = MemRanges$(Range%)
    Min% = VAL("&h" + LEFT$(T$, INSTR(T$, ":") - 1))
    Max% = VAL("&h" + MID$(T$, INSTR(T$, ":") + 1))
    FOR L% = Min% to Max%
        FreeMem(L%) = 0
    NEXT
NEXT

'Produce list of free memory locations
T% = 0
FOR PD% = 1 TO MemSize%
 IF FreeMem(PD%) = 0 THEN
  T% = T% + 1
  VarLoc(T%) = PD%
  IF T% = ChipRam% THEN EXIT FOR
 END IF
NEXT PD%

FreeRAM% = T%

END SUB

SUB CompileIF

'Compile IF Commands
COMPILEIFS:
T% = 0
IL% = 0
CD% = 0
REPIF:
 CD% = CD% + 1
 IF UCASE$(LEFT$(PROG$(CD%), 3)) = "IF " THEN
     
  IL% = IL% + 1
  IF IL% = 1 THEN
   ILC% = ILC% + 1
         
   Origin$ = ""
   IF INSTR(PROG$(CD%), ";?F") <> 0 THEN 
    T$ = PROG$(CD%)
    Origin$ = MID$(T$, INSTR(T$, ";?F"))
    PROG$(CD%) = RTRIM$(LEFT$(PROG$(CD%), INSTR(PROG$(CD%), ";?F") - 1))
   END IF
     
   DS$ = PROG$(CD%)
   C$ = MID$(DS$, 4)
   C$ = LEFT$(C$, INSTR(UCASE$(C$), "THEN") - 1)
   CompileConditions C$, "FALSE"
   COSC% = COSC% + 1: CheckTemp$(COSC%) = " goto ENDIF" + STR$(ILC%)
   PROG$(CD%) = CheckTemp$(1)
   FOR WD% = 2 TO COSC%
    AddLine CheckTemp$(WD%), CD% + WD% - 1
   NEXT WD%
   CD% = CD% + COSC% - 1
   T% = 1
  END IF
 END IF
 
 IF UCASE$(LEFT$(PROG$(CD%), 6)) = "END IF" THEN
  Origin$ = ""
  IF INSTR(PROG$(CD%), ";?F") <> 0 THEN 
   T$ = PROG$(CD%)
   Origin$ = MID$(T$, INSTR(T$, ";?F"))
   PROG$(CD%) = RTRIM$(LEFT$(PROG$(CD%), INSTR(PROG$(CD%), ";?F") - 1))
  END IF
  
  IL% = IL% - 1  
  IF IL% = 0 THEN PROG$(CD%) = "ENDIF" + STR$(ILC%)

  IF IL% < 0 THEN
   IF Origin$ <> "" THEN
    ERC% = ERC% + 1
    gcERR$(ERC%) = Origin$ + Message$("ExtraENDIF")
   END IF
   IL% = 0
  END IF

 END IF
IF CD% < APC% THEN GOTO REPIF

IF T% = 1 THEN GOTO COMPILEIFS

FoundCount% = ILC%

END SUB

SUB CompileReadTable
 FoundCount% = 0

 PD% = 0
lbCompileTables:
 PD% = PD% + 1
 DS$ = PROG$(PD%)
 IF LEFT$(DS$, 10) = "READTABLE " THEN

  Origin$ = ""
  IF INSTR(DS$, ";?F") <> 0 THEN 
   Origin$ = MID$(DS$, INSTR(DS$, ";?F"))
   DS$ = RTRIM$(LEFT$(DS$, INSTR(DS$, ";?F") - 1))
  END IF

  'Get table name
  DS$ = LTRIM$(MID$(DS$, 11))
  TN$ = LTRIM$(RTRIM$(LEFT$(DS$, INSTR(DS$, ",") - 1)))
  DS$ = MID$(DS$, INSTR(DS$, ",") + 1)

  'Get table location
  TL$ = LTRIM$(RTRIM$(LEFT$(DS$, INSTR(DS$, ",") - 1)))
  DS$ = MID$(DS$, INSTR(DS$, ",") + 1)
  
  'Get output variable
  OV$ = LTRIM$(RTRIM$(DS$))
  VLC% = VLC% + 1: VARLIST$(VLC%) = OV$: VarType$(VLC%) = ""

  'Check that table exists
  T% = 0
  FOR CD% = 1 to DTC%
   IF UCASE$(TRIM$(DataTables$(CD%))) = UCASE$(TRIM$(TN$)) THEN T% = 1: EXIT FOR
  NEXT
  IF T% = 0 THEN
   T$ = Message$("TableNotFound")
   Replace T$, "%Table%", TN$
   ERC% = ERC% + 1
   gcERR$(ERC%) = Origin$ + T$
  END IF
    
  'Write assembly
  'Pseudo code:
  ' movf/movlw TL$
  ' call TN$
  ' movwf OV$

  'PROG$(PD%) = ""
  'IF IsCalc(TL$) THEN PROG$(PD%) = "DataPointer=" + TL$
  'IF IsConst(TL$) THEN PROG$(PD%) = " movlw " + TL$
  'IF PROG$(PD%) = "" THEN
  ' PROG$(PD%) = " movf " + TL$ + ", W"
  ' VLC% = VLC% + 1: VARLIST$(VLC%) = TL$
  'END IF
  PROG$(PD%) = "DataPointer=" + TL$
  AddLine " call " + TN$, PD% + 1
  AddLine " movwf " + OV$, PD% + 2
  PD% = PD% + 1
  
 END IF

IF PD% < APC% THEN GOTO lbCompileTables
  

END SUB

SUB CompileVars
CD% = 0
FoundCount% = 0
CompVars:
 CD% = CD% + 1
 DS$ = PROG$(CD%)

 IF INSTR(DS$, "=") <> 0 THEN
  IF INSTR(LCASE$(DS$), " dw ") <> 0 THEN GOTO CompNextVar
  IF INSTR(LCASE$(DS$), " retlw ") <> 0 THEN GOTO CompNextVar

  Origin$ = ""
  IF INSTR(DS$, ";?F") <> 0 THEN 
   Origin$ = MID$(DS$, INSTR(DS$, ";?F"))
   DS$ = RTRIM$(LEFT$(DS$, INSTR(DS$, ";?F") - 1))
   PROG$(CD%) = DS$
  END IF
  
  'Check if command involves moving a string into an array
  IF INSTR(DS$, "()") <> 0 AND INSTR(DS$, "%STRING") <> 0 THEN
   'Get string to copy
   StringToCopy$ = LTRIM$(RTRIM$(MID$(DS$, INSTR(DS$, "=") + 1)))
   StringToCopy% = VAL(MID$(StringToCopy$, 8))
   StringToCopy$ = StringStore$(StringToCopy%)
   
   'Get original line
   OL% = CD%
   
   'Find array address
   ArrayName$ = RTRIM$(LEFT$(DS$, INSTR(DS$, "(") - 1))
   AF% = 0
   FOR FA% = 1 TO ARC%
    IF ArrayName$ = ArrayData$(FA%, 1) THEN AF% = FA%: EXIT FOR
   NEXT FA%
   'If the array has not been declared, show error and compile next line
   IF AF% = 0 THEN
    ERC% = ERC% + 1
    gcERR$(ERC%) = Origin$ + "Array " + ArrayName$ + " has not been declared. Please use DIM to declare the size of this array before using it!"
    ProgWillFail% = 1
   END IF

   'Find array type
   IF ArrayData$(AF%, 4) = "DIM" THEN
    T% = VAL(ArrayData$(AF%, 2))
    T% = T% AND 255
    ArrayHandler$ = STR$(T%)
    ArrayType% = 0
   END IF
   IF ArrayData$(AF%, 4) = "SUB" THEN
    ArrayHandler$ = "Sys" + ArrayName$ + "Handler"
    'Was ArrayHandler$ = "Sys" + N$ + "Handler"
    VLC% = VLC% + 1
    VARLIST$(VLC%) = ArrayHandler$
    ArrayType% = 1
   END IF

   'Add code to set FSR to correct starting location
   If ChipFamily% <> 16 THEN PROG$(CD%) = "FSR=" + ArrayHandler$
   If ChipFamily% = 16 THEN PROG$(CD%) = "FSR0L=" + ArrayHandler$

   If ChipFamily% <> 16 THEN
    IF ArrayType% = 0 THEN
     IF VAL(ArrayData$(AF%, 2)) < 256 THEN T$ = " bcf STATUS, IRP"
     IF VAL(ArrayData$(AF%, 2)) > 255 THEN T$ = " bsf STATUS, IRP"
     AddLine T$, CD%
    END IF
    IF ArrayType% = 1 THEN
     AddLine " bcf STATUS, IRP", CD%
     AddLine " btfsc " + ArrayHandler$ + "_H,0", CD% + 1
     AddLine " bsf STATUS, IRP", CD% + 2
     CD% = CD% + 2
    END IF
   END IF
   If ChipFamily% = 16 THEN
    IF ArrayType% = 0 THEN AddLine "FSR0H=" + STR$(INT(VAL(ArrayData$(AF%, 2)) / 256)), CD%
    IF ArrayType% = 1 THEN AddLine "FSR0H=" + ArrayHandler$ + "_H", CD%
   END IF
   'Add calls to lookup table
   'ASM code to produce:
   '    clrf StringPointer
   'Label
   '    Call StringLookupN
   '    movwf INDF
   '    incf StringPointer, F
   '    incf FSR, F
   '    movf StringPointer, W
   '    sublw [StringLength + 1]
   '    btfss STATUS, Z
   '    goto Label
   
   FSR$ = "FSR": INDF$ = "INDF"
   IF ChipFamily% = 16 THEN FSR$ = "FSR0L": INDF$ = "INDF0"

   SASC% = SASC% + 1
   AddLine " clrf StringPointer", CD% + 2
   AddLine "SysReadString" + STR$(SASC%), CD% + 3
   AddLine " call StringLookup" + STR$(StringToCopy%), CD% + 4
   AddLine " movwf " + INDF$, CD% + 5
   AddLine " incf StringPointer, F", CD% + 6
   AddLine " incf " + FSR$ + ", F", CD% + 7
   AddLine " movf StringPointer, W", CD% + 8
   AddLine " sublw " + STR$(LEN(StringToCopy$) + 1), CD% + 9
   AddLine " btfss STATUS, Z", CD% + 10
   AddLine " goto SysReadString" + STR$(SASC%), CD% + 11
   
   CD% = OL% - 1
   
   GOTO CompNextVar
  END IF

  'Remove ALL spaces
  DO WHILE INSTR(DS$, " ") <> 0: Replace DS$, " ", "": LOOP
  
  'Check if an individual bit is being set
  IF INSTR(DS$, ".") <> 0 THEN
   V$ = LEFT$(DS$, INSTR(DS$, "=") - 1)
   T$ = LEFT$(V$, INSTR(V$, ".") - 1)
   VLC% = VLC% + 1: VARLIST$(VLC%) = T$: VarType$(VLC%) = ""
   S$ = MID$(DS$, INSTR(DS$, "=") + 1)
   ST$ = LEFT$(S$, INSTR(S$, "." ) - 1)
   IF NOT IsConst(ST$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = ST$: VarType$(VLC%) = ""
   'Set or clear?
   IF S$ = "0" THEN
    Replace V$, ".", ", "
    PROG$(CD%) = " bcf " + V$
    GOTO CompNextVar
   END IF
   IF S$ = "1" OR S$ = "255" THEN
    Replace V$, ".", ", "
    PROG$(CD%) = " bsf " + V$
    GOTO CompNextVar
   END IF
   
   'Set one bit to the value of another?
   T$ = LEFT$(S$, INSTR(S$, ".") - 1)
   VLC% = VLC% + 1: VARLIST$(VLC%) = T$: VarType$(VLC%) = ""
   Replace V$, ".", ", "
   Replace S$, ".", ", "
   
   PROG$(CD%) = " bcf " + V$
   AddLine " btfsc " + S$, CD% + 1
   AddLine " bsf " + V$, CD% + 2
   CD% = CD% + 2
   
   GOTO CompNextVar   
  END IF
  
   'Check if command is simple setting to a constant
  T$ = MID$(DS$, INSTR(DS$, "=") + 1)
  IF IsConst(T$) THEN
   T% = 1: IF T$ = "0" THEN T% = 0
   V$ = LEFT$(DS$, INSTR(DS$, "=") - 1)

   VarFound% = 0
   FOR CheckDup% = 1 TO VLC%
    IF UCASE$(VARLIST$(CheckDup%)) = UCASE$(V$) THEN VarFound% = 1: EXIT FOR
   NEXT CheckDup%
   IF VarFound% = 0 THEN
    VLC% = VLC% + 1
    VARLIST$(VLC%) = V$
    VarType$(VLC%) = ""
    VT$ = "BYTE"
   END IF
   IF VarFound% <> 0 THEN VT$ = TypeOfVar$(V$)

   IF VT$ = "BYTE" THEN
    FoundCount% = FoundCount% + 1
    IF T% = 0 THEN PROG$(CD%) = " clrf " + V$
    IF T% <> 0 THEN
     PROG$(CD%) = " movlw " + T$
     AddLine " movwf " + V$, CD% + 1
     CD% = CD% + 1
    END IF
    GOTO CompNextVar
   END IF

   IF VT$ = "WORD" THEN
    HB% = VAL(GetByte$(T$, 1))
    LB% = VAL(GetByte$(T$, 0))
    IF HB% = 0 THEN PROG$(CD%) = " clrf " + V$ + "_H"
    IF HB% <> 0 THEN
     PROG$(CD%) = " movlw " + STR$(HB%)
     AddLine " movwf " + V$ + "_H", CD% + 1
     CD% = CD% + 1
    END IF
    IF LB% = 0 THEN AddLine " clrf " + V$, CD% + 1
    IF LB% <> 0 THEN
     AddLine " movlw " + STR$(LB%), CD% + 1
     AddLine " movwf " + V$, CD% + 2
     CD% = CD% + 1
    END IF
    CD% = CD% + 1
   END IF
   GOTO CompNextVar
  END IF

  'Check if command is increment or decrement
  T$ = MID$(DS$, INSTR(DS$, "=") + 1)
  V$ = LEFT$(DS$, INSTR(DS$, "=") - 1)
  VT$ = TypeOfVar$(V$)
  IF VT$ = "BYTE" THEN
   IF V$ + "+1" = T$ OR V$ + "-1" = T$ THEN
    T% = 1: IF INSTR(T$, "-1") <> 0 THEN T% = 0
    VLC% = VLC% + 1: VARLIST$(VLC%) = V$
    FoundCount% = FoundCount% + 1
    IF T% = 0 THEN PROG$(CD%) = " decf " + V$ + ",F"
    IF T% = 1 THEN PROG$(CD%) = " incf " + V$ + ",F"
    GOTO CompNextVar
   END IF
  END IF

  'Prepare for calculation
  FOR T% = 1 TO 100
   TempData$(T%) = ""
  NEXT T%
  V$ = LEFT$(DS$, INSTR(DS$, "=") - 1)
  VarFound% = 0
  FOR CheckDup% = 1 TO VLC%
   IF UCASE$(VARLIST$(CheckDup%)) = UCASE$(V$) THEN VarFound% = 1: EXIT FOR
  NEXT CheckDup%
  IF VarFound% = 0 THEN
   VLC% = VLC% + 1
   VARLIST$(VLC%) = V$
   VT$ = "BYTE"
  END IF
  IF VarFound% <> 0 THEN VT$ = TypeOfVar$(V$)
  IF VLC% > 325 THEN SortVarList

  S$ = MID$(DS$, INSTR(DS$, "=") + 1)
  CSC% = 0: CV% = 0
  
  'Decide what output to request from CompileCalc
  ThisCalc% = 0 'Byte output
  WordForced% = 0
  IF VT$ = "WORD" THEN ThisCalc% = 1 'Word output
  IF INSTR(S$, "[WORD]") <> 0 THEN
   ThisCalc% = 1
   Replace S$, "[WORD]", ""
   WordForced% = 1
  END IF
 
  SO$ = S$
  CompileCalc S$
  
  'Finish Calculation
  IF CSC% <> 0 THEN
   IF ThisCalc% = 0 THEN
    IF INSTR(TempData$(CSC%), " movff ") = 0 THEN TempData$(CSC%) = " movwf " + V$
    IF INSTR(TempData$(CSC%), " movff ") <> 0 THEN TempData$(CSC%) = LEFT$(TempData$(CSC%), INSTR(TempData$(CSC%), ",")) + V$
   END IF 

   DNR% = WholeINSTR(SO$, V$)
   'IF WordForced% = 1 THEN DNR% = 2
   IF ThisCalc% = 1 AND DNR% <> 2 THEN
    FOR RN% = 1 TO CSC%
     IF WholeINSTR(TempData$(RN%), S$) = 2 THEN Replace TempData$(RN%), S$, V$
     IF WholeINSTR(TempData$(RN%), S$ + "_H") = 2 THEN
      IF WordForced% = 0 THEN Replace TempData$(RN%), S$, V$
      IF WordForced% = 1 THEN
       TempData$(RN%) = ""
       IF LEFT$(TempData$(RN% - 1), 6) = " movf " AND RIGHT$(TempData$(RN% - 1), 1) = "W" THEN TempData$(RN% - 1) = ""
      END IF
     END IF
    NEXT RN%

    T% = 0
    DV% = 0
DelDupCompVars:
     DV% = DV% + 1
     T$ = VARLIST$(DV%)
     IF T$ = S$ OR T$ = S$ + "_H" THEN
      T% = T% + 1
      VARLIST$(DV%) = VARLIST$(VLC%)
      VarType$(DV%) = VarType$(VLC%)
      VARLIST$(VLC%) = ""
      VarType$(VLC%) = ""
      VLC% = VLC% - 1
      DV% = DV% - 1
      IF T% = 2 THEN GOTO VarsTidied
     END IF
    IF DV% < VLC% THEN GOTO DelDupCompVars

VarsTidied:
   END IF

   IF ThisCalc% = 1 AND DNR% = 2 THEN
    CSC% = CSC% + 1: TempData$(CSC%) = " movf " + S$ + ",W"
    CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + V$
    IF WordForced% = 0 THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " movf " + S$ + "_H,W"
     CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + V$ + "_H"
    END IF
   END IF
  END IF
  
  IF CSC% = 0 THEN
   CSC% = CSC% + 1
   IF NOT IsConst(S$) THEN
    TempData$(CSC%) = " movf " + S$ + ",W"
    CSC% = CSC% + 1
    VLC% = VLC% + 1: VARLIST$(VLC%) = S$
   END IF
   IF IsConst(S$) THEN
    TempData$(CSC%) = " movlw " + STR$(VAL(S$) AND 255)
    CSC% = CSC% + 1
   END IF
   TempData$(CSC%) = " movwf " + V$

   IF ThisCalc% = 1 AND WordForced% = 0 THEN
    CSC% = CSC% + 1

    IF NOT IsConst(S$) THEN
     DT$ = TypeOfVar$(S$)
     IF DT$ = "WORD" THEN
      TempData$(CSC%) = " movf " + S$ + "_H,W"
      CSC% = CSC% + 1
      VLC% = VLC% + 1: VARLIST$(VLC%) = S$
      TempData$(CSC%) = " movwf " + V$ + "_H"
     END IF
     IF DT$ <> "WORD" THEN
      TempData$(CSC%) = " clrw"
      CSC% = CSC% + 1
      TempData$(CSC%) = " movwf " + V$ + "_H"
     END IF
    END IF
    
    IF IsConst(S$) THEN
     TempData$(CSC%) = " movlw " + STR$((VAL(S$) AND 65280) / 256)
     CSC% = CSC% + 1
     TempData$(CSC%) = " movwf " + V$ + "_H"
    END IF
    
   END IF
  END IF
  
  T$ = TempData$(CSC% - 1)
  IF INSTR(T$, V$) <> 0 AND INSTR(T$, "wf") <> 0 THEN
   TempData$(CSC%) = ""
   CSC% = CSC% - 1
   Replace TempData$(CSC%), ",W", ",F"
  END IF
  
  'Write the assembly code
  PROG$(CD%) = TempData$(1)
  LC% = 0
  FOR PD% = 2 TO CSC%
   IF TempData$(PD%) <> "" THEN
    LC% = LC% + 1
    AddLine TempData$(PD%), CD% + LC% 'PD% - 1
   END IF
  NEXT PD%
  FoundCount% = FoundCount% + 1
 END IF

CompNextVar:
IF CD% < APC% THEN GOTO CompVars

END SUB

SUB DelLine (T%)
 FOR PD% = T% TO APC% - 1
  PROG$(PD%) = PROG$(PD% + 1)
 NEXT PD%
 PROG$(APC%) = ""
 APC% = APC% - 1
END SUB

SUB DelSubLine (T%)

'Delete line from list
SDC% = SDC% - 1
FOR PD% = T% TO SDC%
 SUBCODE$(PD%) = SUBCODE$(PD% + 1)
NEXT PD%
SUBCODE$(SDC% + 1) = ""

'Adjust index
FOR PD% = 1 TO SBC%
 IF VAL(SUBDATA$(PD%, 2)) > T% THEN SUBDATA$(PD%, 2) = STR$(VAL(SUBDATA$(PD%, 2)) - 1)
 IF VAL(SUBDATA$(PD%, 3)) >= T% THEN SUBDATA$(PD%, 3) = STR$(VAL(SUBDATA$(PD%, 3)) - 1)
NEXT PD%

END SUB

SUB DelSysVars

DSV% = 0
DelSysVarsStart:
DSV% = DSV% + 1
 T$ = UCASE$(VARLIST$(DSV%))
 
 IF T$ = "TRISIO" THEN
  VLC% = VLC% - 1
  FOR DD% = DSV% TO VLC%
   VARLIST$(DD%) = VARLIST$(DD% + 1)
   VarType$(DD%) = VarType$(DD% + 1)
  NEXT DD%
  DSV% = DSV% - 1   
 END IF
 
 FOR PD% = 1 TO SVC%
  IF T$ = SysVars$(PD%, 1) THEN
   VLC% = VLC% - 1
   FOR DD% = DSV% TO VLC%
    VARLIST$(DD%) = VARLIST$(DD% + 1)
    VarType$(DD%) = VarType$(DD% + 1)
   NEXT DD%
   DSV% = DSV% - 1
  END IF
 NEXT PD%

IF DSV% < VLC% THEN GOTO DelSysVarsStart

END SUB

SUB FindArrays

'Determine whether or not a temporary array is required
TempArrayNeeded% = 0
FOR PD% = 1 TO APC%
 DS$ = PROG$(PD%)
 IF INSTR(DS$, "%STRING") <> 0 AND INSTR(DS$, "=") = 0 THEN TempArrayNeeded% = 1
NEXT PD%

IF TempArrayNeeded% = 1 THEN AddLine "DIM SYSTEMPARRAY(20)", 1

'Search through program to find arrays
PD% = 0
FindArrayStart:
PD% = PD% + 1
 IF LEFT$(PROG$(PD%), 4) = "DIM " AND INSTR(PROG$(PD%), " AS ") = 0 THEN
  'Read array name and size
  N$ = MID$(PROG$(PD%), 5)
  S% = VAL(MID$(N$, INSTR(N$, "(") + 1))
  N$ = LEFT$(N$, INSTR(N$, "(") - 1)
  DelLine PD%
  PD% = PD% - 1

  'Add to list
  T% = 0
  FOR CD% = 1 TO ARC%
   IF N$ = ArrayData$(CD%, 1) THEN
    'Note VAL(V$) replaced with S% 20/11/2006
    IF S% <= VAL(ArrayData$(CD%, 3)) THEN T% = 1: EXIT FOR
    IF S% > VAL(ArrayData$(CD%, 3)) THEN T% = 2: EXIT FOR
   END IF
  NEXT CD%
  IF T% = 0 THEN
   ARC% = ARC% + 1
   ArrayData$(ARC%, 1) = N$
   ArrayData$(ARC%, 3) = STR$(S%)
   ArrayData$(ARC%, 4) = "DIM"
  END IF
  IF T% = 2 THEN
   ArrayData$(CD%, 3) = STR$(S%)
  END IF
 END IF

IF PD% < APC% THEN GOTO FindArrayStart

'Sort arrays in order of size
SortArrays:
T% = 0
FOR PD% = 1 TO ARC% - 1
 IF VAL(ArrayData$(PD%, 3)) < VAL(ArrayData$(PD% + 1, 3)) THEN
  T$ = ArrayData$(PD%, 3)
  ArrayData$(PD%, 3) = ArrayData$(PD% + 1, 3)
  ArrayData$(PD% + 1, 3) = T$
  T$ = ArrayData$(PD%, 1)
  ArrayData$(PD%, 1) = ArrayData$(PD% + 1, 1)
  ArrayData$(PD% + 1, 1) = T$
  T$ = ArrayData$(PD%, 4)
  ArrayData$(PD%, 4) = ArrayData$(PD% + 1, 4)
  ArrayData$(PD% + 1, 4) = T$
  T% = 1
 END IF
NEXT PD%
IF T% = 1 THEN GOTO SortArrays

'Allocate RAM to arrays
FOR PD% = 1 TO ARC%

 IF ArrayData$(PD%, 4) <> "DIM" THEN GOTO NextArray

 FL% = 0
 ArraySize% = VAL(ArrayData$(PD%, 3))

 FOR SR% = FreeRAM% - ArraySize% - 1 TO 1 STEP -1

  IF VarLoc(SR% + ArraySize%) - VarLoc(SR%) = ArraySize% THEN
   FL% = 1
   ArrayData$(PD%, 2) = STR$(VarLoc(SR%))
   FreeRAM% = FreeRAM% - ArraySize%
   FOR CD% = SR% TO FreeRAM%
    VarLoc(CD%) = VarLoc(CD% + ArraySize%)
   NEXT CD%
   GOTO NextArray
  END IF
 NEXT SR%

 IF FL% = 0 THEN
  ERC% = ERC% + 1
  T$ = Message$("ArrayTooBig")
  Replace T$, "%array%", ArrayData$(PD%, 1)
  gcERR$(ERC%) = T$
 END IF

NextArray:
NEXT PD%

END SUB

SUB FindCalcSubs (DS$)
 
 DataPass% = 0

 'FOR
 'Uses LessThan, MoreThan
 IF LEFT$(DS$, 4) = "FOR " AND IsWord(DS$) THEN
  ST% = 1
  IF INSTR(DS$, " STEP ") <> 0 THEN ST% = VAL(MID$(DS$, INSTR(DS$, " STEP ") + 5))
  IF ST% > 0 THEN T$ = "SysCompLessThan"
  IF ST% < 0 THEN T$ = "SysCompMoreThan"
  IF IsWord(DS$) THEN T$ = T$ + "16"
  DataPass% = DataPass% + 1
  TempData$(DataPass%) = T$
 END IF
 
 'Multiply
 IF INSTR(DS$, "*") <> 0 THEN
  T$ = "SYSMULTSUB"
  IF IsWord(DS$) THEN T$ = T$ + "16"
  DataPass% = DataPass% + 1
  TempData$(DataPass%) = T$
 END IF

 'Divide
 IF INSTR(DS$, "/") <> 0 THEN
  T$ = "SYSDIVSUB"
  IF IsWord(DS$) THEN T$ = T$ + "16"
  DataPass% = DataPass% + 1
  TempData$(DataPass%) = T$
 END IF

 'Condition checking
 T$ = DS$
 'If a var is being set, remove the first "=" sign
 IF INSTR(T$, "+=") <> 0 THEN Replace T$, "+=", ""
 IF INSTR(T$, "-=") <> 0 THEN Replace T$, "-=", ""
 
 IF INSTR(UCASE$(T$), "FOR") <> 0 THEN Replace T$, "=", ""
 
 DO WHILE INSTR(T$, "(") <> 0
  FOR T% = INSTR(T$, "(") TO INSTR(T$, ")")
   IF MID$(T$, T%, 1) = " " THEN MID$(T$, T%) = "_"
  NEXT
  Replace T$, "(", "%1"
  Replace T$, ")", "%2"
 LOOP
 DO WHILE INSTR(T$, "%1") <> 0
  Replace T$, "%1", "("
  Replace T$, "%2", ")"
 LOOP
  
 IF INSTR(T$, "=") <> 0 THEN
  SC% = 0
  For SS% = INSTR(T$, "=") to 1 step -1
   IF MID$(T$, SS%, 1) = " " THEN SC% = SC% + 1   
  next
  IF SC% < 2 THEN Replace T$, "=", ""
 end if

 IF (CountOccur(T$, "';=~<>{}") > 0 AND CountOccur(T$, "';=~<>{}+-*/&|#!") >= 2) OR IsWord(T$) THEN
  ST$ = "": IF IsWord(T$) THEN ST$ = "16"
  IF INSTR(T$, "=") <> 0 THEN
   DataPass% = DataPass% + 1
   TempData$(DataPass%) = "SysCompEqual" + ST$
  END IF
  IF INSTR(T$, "~") <> 0 THEN
   DataPass% = DataPass% + 1
   TempData$(DataPass%) = "SysCompNotEqual" + ST$
  END IF
  IF INSTR(T$, "<") <> 0 THEN
   DataPass% = DataPass% + 1
   TempData$(DataPass%) = "SysCompLessThan" + ST$
  END IF
  IF INSTR(T$, ">") <> 0 THEN
   DataPass% = DataPass% + 1
   TempData$(DataPass%) = "SysCompMoreThan" + ST$
  END IF
  IF INSTR(T$, "{") <> 0 THEN
   DataPass% = DataPass% + 1
   TempData$(DataPass%) = "SysCompLessOrEqual" + ST$
  END IF
  IF INSTR(T$, "}") <> 0 THEN
   DataPass% = DataPass% + 1
   TempData$(DataPass%) = "SysCompMoreOrEqual" + ST$
  END IF
 END IF

END SUB

SUB FindLargeVars

'Find large variables defined using DIM AS
PD% = 0
FindDIMs:
 PD% = PD% + 1
 DS$ = PROG$(PD%)

 IF LEFT$(DS$, 4) = "DIM " AND INSTR(DS$, " AS ") <> 0 THEN
  DelLine PD%
  PD% = PD% - 1

  'Get origin
  Origin$ = ""
  IF INSTR(DS$, ";?F") <> 0 THEN 
   Origin$ = MID$(DS$, INSTR(DS$, ";?F"))
   DS$ = RTRIM$(LEFT$(DS$, INSTR(DS$, ";?F") - 1))
  END IF

  'Check that a large var, and not an array is being dimensioned
  IF INSTR(DS$, "(") <> 0 THEN
   ERC% = ERC% + 1
   gcERR$(ERC%) = Origin$ + Message$("ArrayTypeInvalid")
  END IF
  
  'Get variable name and type
  V$ = LTRIM$(RTRIM$(MID$(DS$, 5)))
  V$ = LTRIM$(RTRIM$(LEFT$(V$, INSTR(V$, " ") - 1)))
  Ty$ = LTRIM$(RTRIM$(MID$(DS$, INSTR(DS$, " AS ") + 4)))

  'Code for strings
  IF LEFT$(Ty$, 6) = "STRING" THEN
   StLen% = 20
   IF INSTR(Ty$, "*") <> 0 THEN
    StLen% = VAL(MID$(Ty$, INSTR(Ty$, "*") + 1))
   END IF
   AddLine "DIM " + V$ + "(" + STR$(StLen%) + ")", 1
   PD% = PD% + 1
  END IF

  'Search variable list, and show error if duplicate definition occurs
  VarFound% = 0
  FOR SL% = 1 TO VLC%
   IF VARLIST$(SL%) = V$ THEN VarFound% = SL%: EXIT FOR
  NEXT SL%

  IF Ty$ = "BYTE" THEN Ty$ = ""

  IF VarFound% = 0 THEN
   IF Ty$ = "WORD" THEN LongPresent% = 1
   VLC% = VLC% + 1: VARLIST$(VLC%) = V$
   VarType$(VLC%) = Ty$
   IF Ty$ = "WORD" THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V$ + "_H"
  END IF
  IF VarFound% <> 0 AND UCASE$(TRIM$(VarType$(VarFound%))) <> "" AND UCASE$(Ty$) <> "" AND UCASE$(TRIM$(VarType$(VarFound%))) <> UCASE$(Ty$) THEN
   ERC% = ERC% + 1
   T$ = Message$("DupDef")
   Replace T$, "%var%", V$
   gcERR$(ERC%) = Origin$ + T$
  END IF

 END IF

NextDIM:
IF PD% < APC% THEN GOTO FindDIMs

'Search in subs, and add to possibly needed list
PD% = 0
SearchSubsLarge:
 PD% = PD% + 1
 DS$ = SUBCODE$(PD%)
 IF LEFT$(DS$, 4) = "DIM " AND INSTR(DS$, " AS ") <> 0 THEN
  FOR FS% = 1 TO SBC%
   IF VAL(SUBDATA$(FS%, 2)) <= PD% AND VAL(SUBDATA$(FS%, 3)) >= PD% THEN EXIT FOR
  NEXT FS%

  S$ = SUBDATA$(FS%, 1)
  IF INSTR(S$, "(") <> 0 THEN S$ = LEFT$(S$, INSTR(S$, "(") - 1)
  IF INSTR(S$, " ") <> 0 THEN S$ = LEFT$(S$, INSTR(S$, " ") - 1)
  S$ = RTRIM$(S$)
  
  LVC% = LVC% + 1
  LargeVars$(LVC%, 1) = DS$
  LargeVars$(LVC%, 2) = S$

  DelSubLine PD%
  PD% = PD% - 1
 END IF
IF PD% < SDC% THEN GOTO SearchSubsLarge

END SUB

SUB FindRequiredSubs

CalcSubs% = 0
LO% = 2

'System initialisation sub
SLC% = SLC% + 1
SUBLIST$(SLC%, 1) = "INITSYS"
SUBLIST$(SLC%, 2) = STR$(LocationOfSub("INITSYS"))

'Interrupt sub
T% = LocationOfSub("INTERRUPT")
IF T% <> 0 THEN
 UserInt% = 1
 SLC% = SLC% + 1
 SUBLIST$(SLC%, 1) = "INTERRUPT"    
 SUBLIST$(SLC%, 2) = STR$(T%)
END IF

'Find high level subs in main program
FOR PD% = 1 TO APC%
 FindSubs PROG$(PD%)
 IF TempData$(0) = " call " THEN PROG$(PD%) = " call " + PROG$(PD%)
 FOR AdS% = 1 TO DataPass%
  IF SubListLocation(TempData$(AdS%)) = 0 THEN
   SLC% = SLC% + 1
   SUBLIST$(SLC%, 1) = TempData$(AdS%)
   SUBLIST$(SLC%, 2) = STR$(LocationOfSub(TempData$(AdS%)))
  END IF
 NEXT
NEXT

'Find high level subs in subs
O% = 1
GETSUBCALLS:
SA% = 0
 OO% = O%
 O% = SLC%
 FOR FS% = OO% TO O%
  CS% = VAL(SUBLIST$(FS%, 2))
  S% = VAL(SUBDATA$(CS%, 2)): E% = VAL(SUBDATA$(CS%, 3))
  
  'FOR CD% = S% TO E%
  CD% = S% - 1
  SearchInSubs:
  CD% = CD% + 1
  IF CD% > E% THEN GOTO DontLookInSub

  'Replace Constants
  RepConstsSubs:
  T$ = SUBCODE$(CD%)
  ReplaceConstantsLine SUBCODE$(CD%)
  IF T$ <> SUBCODE$(CD%) THEN GOTO RepConstsSubs

  'Split lines
  IF INSTR(SUBCODE$(CD%), ":") <> 0 AND RIGHT$(SUBCODE$(CD%), 1) <> ":" THEN
   T$ = SUBCODE$(CD%)
   T2$ = LTRIM$(RTRIM$(MID$(T$, INSTR(T$, ":") + 1)))
   SUBCODE$(CD%) = LTRIM$(RTRIM$(LEFT$(T$, INSTR(T$, ":") - 1)))
   AddSubLine T2$, CD% + 1
   E% = E% + 1
  END IF

   FindSubs SUBCODE$(CD%)
   IF TempData$(0) = " call " THEN SUBCODE$(CD%) = " call " + SUBCODE$(CD%)
   FOR AdS% = 1 TO DataPass%
    IF SubListLocation(TempData$(AdS%)) = 0 THEN
     SA% = 1
     SLC% = SLC% + 1
     SUBLIST$(SLC%, 1) = UCASE$(TempData$(AdS%))
     SUBLIST$(SLC%, 2) = STR$(LocationOfSub(TempData$(AdS%)))
    END IF
   NEXT

  'NEXT CD%
  IF CD% < E% THEN GOTO SearchInSubs
  DontLookInSub:
  
 NEXT FS%

 'Find automatic initialisation routines for subs recently added
 FOR FS% = 1 TO ICC%
  T$ = LTRIM$(RTRIM$(gcINC$(FS%, 2)))
  IF INSTR(T$, ";") = 0 AND T$ <> "" AND SubListLocation(T$) = 0 THEN
   SLC% = SLC% + 1
   SUBLIST$(SLC%, 1) = T$
   SUBLIST$(SLC%, 2) = STR$(LocationOfSub(T$))
  END IF
 NEXT FS%
 
IF SA% = 1 THEN GOTO GETSUBCALLS

'Complete list of needed large (>byte) variables
'Large vars in main would already have been found
FindLargeVarsInSubs:
FOR FS% = 1 TO SLC%
 FOR CD% = 1 TO LVC%
  IF LargeVars$(CD%, 2) = SUBLIST$(FS%, 1) THEN
   DS$ = LargeVars$(CD%, 1)
      
   'Get origin
   Origin$ = ""
   IF INSTR(DS$, ";?F") <> 0 THEN 
    Origin$ = MID$(DS$, INSTR(DS$, ";?F"))
    DS$ = RTRIM$(LEFT$(DS$, INSTR(DS$, ";?F") - 1))
   END IF
 
   'Get variable name and type
   V$ = TRIM$(MID$(DS$, 5))
   V$ = TRIM$(LEFT$(V$, INSTR(V$, " ") - 1))
   Ty$ = UCASE$(TRIM$(MID$(DS$, INSTR(DS$, " AS ") + 4)))
   IF Ty$ = "BYTE" THEN Ty$ = ""
   
   'Search variable list, and show error if duplicate definition occurs
   VarFound% = 0
   FOR SL% = 1 TO VLC%
    IF VARLIST$(SL%) = V$ THEN VarFound% = SL%: EXIT FOR
   NEXT SL%
     
   IF VarFound% = 0 THEN
    IF Ty$ = "WORD" THEN LongPresent% = 1
    VLC% = VLC% + 1: VARLIST$(VLC%) = V$
    VarType$(VLC%) = Ty$
    IF Ty$ = "WORD" THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V$ + "_H"
    LO% = 2
   END IF
   IF VarFound <> 0 AND UCASE$(TRIM$(VarType$(VarFound%))) = "" AND UCASE$(Ty$) <> "" THEN
    IF Ty$ = "WORD" THEN LongPresent% = 1
    VLC% = VLC% + 1: VARLIST$(VLC%) = V$
    VarType$(VLC%) = Ty$
    IF Ty$ = "WORD" THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V$ + "_H"
    LO% = 2
   END IF
   IF VarFound% <> 0 AND UCASE$(TRIM$(VarType$(VarFound%))) <> "" AND UCASE$(Ty$) <> "" AND UCASE$(TRIM$(VarType$(VarFound%))) <> UCASE$(Ty$) THEN
    ERC% = ERC% + 1
    T$ = Message$("DupDef")
    Replace T$, "%var%", V$
    gcERR$(ERC%) = Origin$ + T$
   END IF 
  END IF
 NEXT
NEXT

SortVarList

'Find low level subs in main program
RC% = 0
FOR PD% = 1 TO APC%
 FindCalcSubs PROG$(PD%)
 FOR AdS% = 1 TO DataPass%
  IF SubListLocation(TempData$(AdS%)) = 0 THEN
   SLC% = SLC% + 1
   SUBLIST$(SLC%, 1) = TempData$(AdS%)
   SUBLIST$(SLC%, 2) = STR$(LocationOfSub(TempData$(AdS%)))
   RC% = 1
  END IF
 NEXT
NEXT

'Find low level subs in subs
OLO% = LO% - 1
LO% = SLC%
FOR FS% = OLO% TO LO%
 CS% = VAL(SUBLIST$(FS%, 2))
 S% = VAL(SUBDATA$(CS%, 2)): E% = VAL(SUBDATA$(CS%, 3))
 FOR CD% = S% TO E%
  FindCalcSubs SUBCODE$(CD%)
  FOR AdS% = 1 TO DataPass%
   IF SubListLocation(TempData$(AdS%)) = 0 THEN
    RC% = 1
    SLC% = SLC% + 1
    SUBLIST$(SLC%, 1) = UCASE$(TempData$(AdS%))
    SUBLIST$(SLC%, 2) = STR$(LocationOfSub(TempData$(AdS%)))
   END IF
  NEXT
 NEXT CD%
NEXT FS%

IF RC% = 0 THEN CalcSubs% = CalcSubs% + 1
'IF CalcSubs% < 3 THEN GOTO GETSUBCALLS
IF RC% = 1 OR CalcSubs% < 2 THEN GOTO GETSUBCALLS '2 is lowest known safe value

'Remove interrupt sub from list, as it is added inline
IF UserInt% <> 0 THEN
 FOR SC% = 1 to SLC%
  IF UCASE$(SUBLIST$(SC%, 1)) = "INTERRUPT" THEN
   SUBLIST$(SC%, 1) = SUBLIST$(SLC%, 1)
   SUBLIST$(SC%, 2) = SUBLIST$(SLC%, 2)
   SLC% = SLC% - 1
   EXIT FOR
  END IF
 NEXT
END IF

'Sort list of subs
SORTSUBS:
T% = 0
FOR SC% = 1 TO SLC% - 1
 IF SUBLIST$(SC%, 1) > SUBLIST$(SC% + 1, 1) THEN
  N$ = SUBLIST$(SC% + 1, 1): V$ = SUBLIST$(SC% + 1, 2)
  SUBLIST$(SC% + 1, 1) = SUBLIST$(SC%, 1): SUBLIST$(SC% + 1, 2) = SUBLIST$(SC%, 2)
  SUBLIST$(SC%, 1) = N$: SUBLIST$(SC%, 2) = V$
  T% = 1
 END IF
NEXT SC%
IF T% = 1 THEN GOTO SORTSUBS

'Delete duplicate entries in list
CD% = 0
DELDUP:
 CD% = CD% + 1
 IF SUBLIST$(CD%, 1) = SUBLIST$(CD% + 1, 1) THEN
  FOR DL% = CD% TO SLC%
   SUBLIST$(DL%, 1) = SUBLIST$(DL% + 1, 1)
   SUBLIST$(DL%, 2) = SUBLIST$(DL% + 1, 2)
  NEXT DL%
  SLC% = SLC% - 1
  CD% = CD% - 1
 END IF
IF CD% < SLC% THEN GOTO DELDUP

END SUB

SUB FindSubs (DS$)

 'DS$ = UCASE$(LineTemp$)
 'IF INSTR(DS$, ";?F") <> 0 THEN DS$ = TRIM$(LEFT$(DS$, INSTR(DS$, ";?F") - 1))
  
 TempData$(0) = ""
 TempData$(1) = ""
 DataPass% = 0

 'Check if startup sub is needed for the line because of a constant
 DO WHILE INSTR(UCASE$(DS$), ";STARTUP") <> 0
  T% = VAL(MID$(DS$, INSTR(UCASE$(DS$), ";STARTUP") + 8))
  T$ = ";STARTUP" + LTRIM$(STR$(T%))
  Replace DS$, T$, ""
  Replace gcINC$(T%, 2), ";", ""
 LOOP
 
 'Check for data table use
 IF LEFT$(DS$, 10) = "READTABLE " THEN
  'Get name
  T$ = MID$(DS$, INSTR(DS$, " ") + 1)
  T$ = UCASE$(TRIM$(LEFT$(T$, INSTR(T$, ",") - 1)))
  'Find index, and mark as used
  FOR T% = 1 TO DTC%
   IF UCASE$(TRIM$(DataTables$(T%))) = T$ THEN 
    DataTableUsed%(T%) = 1
    EXIT FOR
   END IF
  NEXT
 END IF
  
 IF LEFT$(DS$, 4) = "POT " THEN
  'Add Delay_MS and Delay_10US
  DataPass% = DataPass% + 1: TempData$(DataPass%) = "Delay_MS"
  DataPass% = DataPass% + 1: TempData$(DataPass%) = "Delay_10US"
  EXIT SUB
 END IF

 IF LEFT$(DS$, 5) = "WAIT " OR LEFT$(DS$, 9) = "PULSEOUT " THEN
  V$ = MID$(DS$, 6)
  IF LEFT$(DS$, 9) = "PULSEOUT " THEN V$ = LTRIM$(MID$(DS$, INSTR(DS$, ",") + 1))
  
  Origin$ = ""
  IF INSTR(V$, ";?F") <> 0 THEN 
   Origin$ = MID$(V$, INSTR(V$, ";?F")) 
   V$ = RTRIM$(LEFT$(V$, INSTR(V$, ";?F") - 1))
  END IF
  
  U$ = LCASE$(MID$(V$, INSTR(V$, " ") + 1))
  V$ = LEFT$(V$, INSTR(V$, " ") - 1)
  
  T% = 0
  IF (U$ = "us" OR U$ = "usec") And (NOT IsConst(V$) OR ChipMHz% >= 20) THEN U$ = "Delay_US": T% = 1
  IF U$ = "10us" OR U$ = "10usec" THEN U$ = "Delay_10US": T% = 1
  IF U$ = "ms" OR U$ = "msec" THEN U$ = "Delay_MS": T% = 1
  IF U$ = "10ms" OR U$ = "10msec" THEN U$ = "Delay_10MS": T% = 1
  IF U$ = "s" OR U$ = "sec" THEN U$ = "Delay_S": T% = 1
  IF U$ = "m" OR U$ = "min" THEN U$ = "Delay_M": T% = 1
  IF U$ = "h" OR U$ = "hour" THEN U$ = "Delay_H": T% = 1
  IF T% = 1 THEN DataPass% = DataPass% + 1: TempData$(DataPass%) = U$
  EXIT SUB
 END IF

'Check for sub call
 T$ = LTRIM$(DS$)
 GSF% = 0
 IF INSTR(T$, "GOSUB ") <> 0 THEN T$ = TRIM$(MID$(T$, INSTR(T$, "GOSUB ") + 5)): GSF% = 1
 IF INSTR(T$, "(") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, "(") - 1)
 IF INSTR(T$, " ") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, " ") - 1)

 LO% = LocationOfSub(T$)
 IF LO% <> 0 THEN
  DataPass% = DataPass% + 1: TempData$(DataPass%) = T$
  IF GSF% = 0 THEN TempData$(0) = " call "
  Replace gcINC$(VAL(SUBDATA$(LO%, 4)), 2), ";", ""
 END IF

END SUB

SUB FixFunctions

'Fix functions that are used by DO and WAIT, so they are updated inside
'the loops

FOR PD% = 1 TO APC%
 IF LEFT$(PROG$(PD%), 9) = " call FN_" THEN
  IF LEFT$(PROG$(PD% + 1), 11) = "SysWaitLoop" OR LEFT$(PROG$(PD% + 1), 11) = "SysDoLoop_S" THEN
  
  SN$ = MID$(PROG$(PD%), 10)
  FOR T% = PD% - 1 TO 1 STEP -1
   IF PROG$(T%) = ";FNSTART," + SN$ THEN
    PROG$(T%) = PROG$(PD% + 1)
    PROG$(PD% + 1) = ";IAmAnEmoLineDeleteMe"
    EXIT FOR
   END IF
  NEXT
    
 '  T$ = PROG$(PD%)
 '  PROG$(PD%) = PROG$(PD% + 1)
 '  PROG$(PD% + 1) = T$
  END IF
 END IF
NEXT PD%

CD% = 0
DelEmoLines:
 CD% = CD% + 1
 
 IF PROG$(CD%) = ";IAmAnEmoLineDeleteMe" THEN DelLine CD%: CD% = CD% - 1
 IF LEFT$(PROG$(CD%), 9) = ";FNSTART," THEN DelLine CD%: CD% = CD% - 1
 
IF CD% < APC% THEN GOTO DelEmoLines


END SUB

FUNCTION LocationOfSub (SubName$)

DS$ = UCASE$(LTRIM$(SubName$))
LocationOfSub = 0

'If a var is being set, don't bother checking to see if the var is a sub
IF MID$(DS$, INSTR(DS$, " ") + 1, 1) = "=" THEN EXIT FUNCTION

'Remove any parameters from the name
IF INSTR(DS$, "(") <> 0 THEN DS$ = LEFT$(DS$, INSTR(DS$, "(") - 1)
IF INSTR(DS$, " ") <> 0 THEN DS$ = LEFT$(DS$, INSTR(DS$, " ") - 1)

'Exit if SubName is a known, common command
'This code is meant to save time, so no point in checking for common commands if they are not subs
T$ = LEFT$(DS$, 3)
IF T$ = "IF " THEN EXIT FUNCTION
IF T$ = "DO " THEN EXIT FUNCTION
T$ = LEFT$(DS$, 4)
IF T$ = "SET " THEN EXIT FUNCTION
IF T$ = "DIR " THEN EXIT FUNCTION
IF T$ = "FOR " THEN EXIT FUNCTION
IF T$ = "DIM " THEN EXIT FUNCTION
IF T$ = "END " THEN EXIT FUNCTION
T$ = LEFT$(DS$, 5)
IF T$ = "LOOP " THEN EXIT FUNCTION
IF T$ = "NEXT " THEN EXIT FUNCTION
IF T$ = "WAIT " THEN EXIT FUNCTION

'Returns the position of DS$ in SUBDATA$()
FOR T% = 1 TO SBC%
 T$ = UCASE$(LTRIM$(SUBDATA$(T%, 1)))
 IF INSTR(T$, "(") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, "(") - 1)
 IF INSTR(T$, " ") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, " ") - 1)
 IF TRIM$(T$) = TRIM$(DS$) THEN 
  LocationOfSub = T%
  EXIT FUNCTION
 END IF
NEXT T%

END FUNCTION

SUB OptimiseIF
PD% = 0

IFOptimise:
PD% = PD% + 1
 T$ = PROG$(PD%)
 IF LEFT$(T$, 5) = "ENDIF" THEN
  IF PROG$(PD% - 2) = " goto " + T$ THEN
   DS$ = LEFT$(PROG$(PD% - 3), 6)
   IF DS$ = " btfsc" THEN T% = 0
   IF DS$ = " btfss" THEN T% = 1
   IF T% = 0 THEN Replace PROG$(PD% - 3), "btfsc", "btfss"
   IF T% = 1 THEN Replace PROG$(PD% - 3), "btfss", "btfsc"
   DelLine PD%
   DelLine PD% - 2
   PD% = PD% - 2
  END IF
 END IF
IF PD% < APC% THEN GOTO IFOptimise

END SUB

SUB PrepareBuiltIn
'Read built-in subs and constants

'Delay subs
'Time Intervals: us, 10us, ms, 10ms, sec, min, hour

'Delay_US
SBC% = SBC% + 1
SUBDATA$(SBC%, 1) = "Delay_US"
SUBDATA$(SBC%, 2) = STR$(SDC% + 1)

'Each nop takes .2 us on 20 MHz chip
L% = ChipMhz / 4 - 3
IF L% < 0 THEN L% = 0
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DUS_START"
FOR CD% = 1 TO L%
 SDC% = SDC% + 1: SUBCODE$(SDC%) = " nop"
NEXT CD%
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz SysWaitTempUS, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto DUS_START"
SUBDATA$(SBC%, 3) = STR$(SDC%)

'Delay_10US
SBC% = SBC% + 1
SUBDATA$(SBC%, 1) = "Delay_10US"
SUBDATA$(SBC%, 2) = STR$(SDC% + 1)
L% = 10 * ChipMhz / 12 - .5
IF L% < 1 THEN L% = 1
D$ = STR$(L%)
SDC% = SDC% + 1: SUBCODE$(SDC%) = "D10US_START"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DELAYTEMP = " + D$
SDC% = SDC% + 1: SUBCODE$(SDC%) = "D10US_LOOP"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz DELAYTEMP, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto D10US_LOOP"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz SysWaitTemp10US, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto D10US_START"
SUBDATA$(SBC%, 3) = STR$(SDC%)

'Delay_MS
'Repeat 20(wait 50)
SBC% = SBC% + 1
SUBDATA$(SBC%, 1) = "Delay_MS"
SUBDATA$(SBC%, 2) = STR$(SDC% + 1)
IF ChipMhz < 31 THEN
 L% = 100 * ChipMhz / 12 - .5
 IF L% < 1 THEN L% = 1
 IL$ = STR$(L%)
 OL$ = "10"
END IF
IF ChipMhz > 30 THEN
 L% = 50 * ChipMhz / 12 - .5 - 2
 IF L% < 1 THEN L% = 1
 IL$ = STR$(L%)
 OL$ = "20"
END IF

SDC% = SDC% + 1: SUBCODE$(SDC%) = "DMS_START"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DELAYTEMP2 = " + OL$
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DMS_OUTER"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DELAYTEMP = " + IL$
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DMS_INNER"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz DELAYTEMP, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto DMS_INNER"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz DELAYTEMP2, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto DMS_OUTER"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz SysWaitTempMS, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto DMS_START"
SUBDATA$(SBC%, 3) = STR$(SDC%)

'Delay_10MS
SBC% = SBC% + 1
SUBDATA$(SBC%, 1) = "Delay_10MS"
SUBDATA$(SBC%, 2) = STR$(SDC% + 1)
SDC% = SDC% + 1: SUBCODE$(SDC%) = "D10MS_START"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "SysWaitTempMS = 10"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "Delay_MS"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz SysWaitTemp10MS, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto D10MS_START"
SUBDATA$(SBC%, 3) = STR$(SDC%)

'Delay_Sec
SBC% = SBC% + 1
SUBDATA$(SBC%, 1) = "Delay_S"
SUBDATA$(SBC%, 2) = STR$(SDC% + 1)
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DS_START"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DELAYTEMP4 = 10"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DS_OUTER"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "SysWaitTempMS = 100"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "Delay_MS"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz DELAYTEMP4, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto DS_OUTER"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz SysWaitTempS, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto DS_START"
SUBDATA$(SBC%, 3) = STR$(SDC%)


'Delay_Min
SBC% = SBC% + 1
SUBDATA$(SBC%, 1) = "Delay_M"
SUBDATA$(SBC%, 2) = STR$(SDC% + 1)
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DMIN_START"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "SysWaitTempS = 60"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "Delay_S"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz SysWaitTempM, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto DMIN_START"
SUBDATA$(SBC%, 3) = STR$(SDC%)


'Delay_Hour
SBC% = SBC% + 1
SUBDATA$(SBC%, 1) = "Delay_H"
SUBDATA$(SBC%, 2) = STR$(SDC% + 1)
SDC% = SDC% + 1: SUBCODE$(SDC%) = "DHOUR_START"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "SysWaitTempM = 60"
SDC% = SDC% + 1: SUBCODE$(SDC%) = "Delay_M"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " decfsz SysWaitTempH, F"
SDC% = SDC% + 1: SUBCODE$(SDC%) = " goto DHOUR_START"
SUBDATA$(SBC%, 3) = STR$(SDC%)

END SUB

SUB PreProcessor

'Find required files
IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("FindSource")
ICC% = 1: gcINC$(1, 1) = ShortName$(FI$)
T% = 1

FindIncludeFiles:
T2% = T%
ICCO% = ICC%
FOR T% = T2% TO ICC%

 IF VBS% = 1 THEN PRINT SPC(10); gcINC$(T%, 1);
 IF DIR$(gcINC$(T%, 1)) = "" THEN
  IF VBS% = 0 THEN PRINT Message$("NoFile")
  IF VBS% = 1 THEN PRINT ": " + Message$("NotFound")
 END IF
 IF DIR$(gcINC$(T%, 1)) <> "" THEN
  OPEN gcINC$(T%, 1) FOR INPUT AS #1
  DO WHILE NOT EOF(1)
   LINE INPUT #1, T$
   T$ = TRIM$(T$)
   IF LEFT$(UCASE$(T$), 8) = "#INCLUDE" THEN
    IF INSTR(T$, CHR$(34)) <> 0 THEN
     T$ = MID$(T$, INSTR(T$, CHR$(34)) + 1)
     T$ = TRIM$(LEFT$(T$, INSTR(T$, CHR$(34)) - 1))
     IF LEFT$(T$, 1) = "\" THEN T$ = MID$(T$, 2)
     IF INSTR(T$, ":\") = 0 THEN T$ = ProgDir$ + "\" + T$
    END IF
    IF INSTR(T$, "<") <> 0 THEN
     T$ = MID$(T$, INSTR(T$, "<") + 1)
     T$ = LEFT$(T$, INSTR(T$, ">") - 1)
     T$ = ID$ + "\include\" + T$
    END IF
    T$ = ShortName$(T$)
    CE% = 0
    FOR PD% = 1 TO ICC%
     IF UCASE$(gcINC$(PD%, 1)) = UCASE$(T$) THEN CE% = 1: EXIT FOR
    NEXT PD%
    IF CE% = 0 THEN ICC% = ICC% + 1: gcINC$(ICC%, 1) = T$
   END IF
  LOOP

  CLOSE #1
  IF VBS% = 1 THEN PRINT ": " + Message$("found")
 END IF
NEXT T%
IF ICCO% < ICC% THEN GOTO FindIncludeFiles

'Add standard include files to list
#IFDEF __FB_LINUX__
OPEN ID$ + "/include/lowlevel.dat" FOR INPUT AS #1
#ELSE
OPEN ID$ + "\include\lowlevel.dat" FOR INPUT AS #1
#ENDIF

DO WHILE NOT EOF(1)
 LINE INPUT #1, DS$
 DS$ = LTRIM$(RTRIM$(DS$))
 IF LEFT$(DS$, 1) <> "'" THEN
#IFDEF __FB_LINUX__
  DS$ = ID$ + "/include/lowlevel/" + DS$
#ELSE
  DS$ = ID$ + "\INCLUDE\LOWLEVEL\" + DS$
#ENDIF
  CE% = 0
  FOR PD% = 1 TO ICC%
   IF UCASE$(gcINC$(PD%, 1)) = UCASE$(DS$) THEN CE% = 1: EXIT FOR
  NEXT PD%
  IF CE% = 0 THEN
   ICC% = ICC% + 1
   gcINC$(ICC%, 1) = DS$
   T$ = DIR$(DS$)
   IF T$ <> "" THEN T$ = ": " + Message$("found")
   IF T$ = "" THEN T$ = ": " + Message$("NotFound")
   IF VBS% = 1 THEN PRINT SPC(10); DS$; T$
  END IF
 END IF
LOOP
CLOSE

'Read all required files
IF VBS% = 1 THEN PRINT SPC(5); Message$("LoadSource");
FOR RF% = 1 TO ICC%
 IF DIR$(gcINC$(RF%, 1)) = "" THEN GOTO LoadNextFile
 OPEN gcINC$(RF%, 1) FOR INPUT AS #1
 
 S% = 0
 LC% = 0
 
 DO WHILE NOT EOF(1)
LoadFileData:
  LINE INPUT #1, DS$
  LC% = LC% + 1
  
  'Remove comments and leading and trailing spaces, and capitalise line
  DSO$ = DS$
  DO WHILE INSTR(DS$, CHR$(9)) <> 0: Replace DS$, CHR$(9), " ": LOOP
  DS$ = UCASE$(LTRIM$(RTRIM$(DS$)))

  'Put strings back to how they were
  IF INSTR(DS$, CHR$(34)) <> 0 THEN
   ST$ = MID$(DS$, INSTR(DS$, CHR$(34)))
   ST$ = LEFT$(ST$, INSTR(2, ST$, CHR$(34)))
   STO$ = MID$(DSO$, INSTR(DSO$, CHR$(34)))
   STO$ = LEFT$(STO$, INSTR(2, STO$, CHR$(34)))
   Replace DS$, ST$, STO$
  END IF

  'Remove comments
  IF INSTR(DS$, "B'") <> 0 THEN
   Replace DS$, "B'", "%B%"
   Replace DS$, "'", "%S%"
  END IF
  IF INSTR(DS$, "H'") <> 0 THEN
   Replace DS$, "H'", "%H%"
   Replace DS$, "'", "%S%"
  END IF
  IF INSTR(DS$, ";") > 1 THEN DS$ = LEFT$(DS$, INSTR(DS$, ";") - 1)
  IF INSTR(DS$, "'") > 1 THEN DS$ = LEFT$(DS$, INSTR(DS$, "'") - 1)
  IF INSTR(DS$, "%B%") <> 0 THEN
   Replace DS$, "%B%", "B'"
   Replace DS$, "%S%", "'"
  END IF
  IF INSTR(DS$, "%H%") <> 0 THEN
   Replace DS$, "%H%", "H'"
   Replace DS$, "%S%", "'"
  END IF
  DS$ = RTRIM$(DS$)

  'Only load line if it is valid
  T% = 0
  IF LEFT$(DS$, 1) = ";" THEN T% = 1
  IF LEFT$(DS$, 1) = "'" THEN T% = 1
  IF LEFT$(DS$, 4) = "REM " THEN T% = 1
  IF DS$ = "REM" THEN T% = 1
  IF DS$ = "" THEN T% = 1
  IF LEFT$(DS$, 8) = "#INCLUDE" THEN T% = 1
  IF T% = 0 THEN T% = CheckLine(DS$)

  'Remove any tabs and double spaces
  DO WHILE INSTR(DS$, CHR$(9)) <> 0: Replace DS$, CHR$(9), " ": LOOP
  DO WHILE INSTR(DS$, "  ") <> 0: Replace DS$, "  ", " ": LOOP

  'Load line
  IF T% = 0 THEN

MultiCommand:

   'Make adjustments to line if needed

   'Tokenise strings
   IF INSTR(DS$, CHR$(34)) <> 0 THEN
    StringTemp$ = MID$(DS$, INSTR(DS$, CHR$(34)) + 1)
    StringTemp$ = LEFT$(StringTemp$, INSTR(StringTemp$, CHR$(34)) - 1)
    SSC% = SSC% + 1
    StringStore$(SSC%) = StringTemp$
    Replace DS$, CHR$(34) + StringTemp$ + CHR$(34), "%STRING" + STR$(SSC%) + "%"
   END IF

   'Remove LET and CALL commands
   IF LEFT$(DS$, 4) = "LET " THEN Replace DS$, "LET ", ""
   IF LEFT$(DS$, 5) = "CALL " THEN Replace DS$, "CALL ", ""

   'Convert single-line IFs to multiple line
   IF INSTR(DS$, "IF") <> 0 AND INSTR(DS$, "THEN") <> 0 AND LEN(DS$) > INSTR(DS$, "THEN") + 3 THEN
    Replace DS$, "THEN", "THEN:"
    DS$ = DS$ + ": END IF"
   END IF
   
   'Check that the IF has a THEN
   IF LEFT$(DS$, 3) = "IF " AND INSTR(DS$, "THEN") = 0 THEN
    ERC% = ERC% + 1
    gcERR$(ERC%) = ";?F" + STR$(RF%) + "L" + STR$(LC%)+"?" + Message$("NoThen")
   END IF
   
   'Replace <> with ~ (not equal)
   IF INSTR(DS$, "<>") <> 0 THEN Replace DS$, "<>", "~"
   'Replace => with } (equal or greater)
   IF INSTR(DS$, "=>") <> 0 THEN Replace DS$, "=>", "}"
   IF INSTR(DS$, ">=") <> 0 THEN Replace DS$, ">=", "}"
   'Replace =< with { (less or equal)
   IF INSTR(DS$, "=<") <> 0 THEN Replace DS$, "=<", "{"
   IF INSTR(DS$, "<=") <> 0 THEN Replace DS$, "<=", "{"
   
   'Convert DIM var AS BIT to #define var BitTemp.BVC%
   IF LEFT$(DS$, 4) = "DIM " AND INSTR(DS$, "AS BIT") <> 0 THEN
    V$ = MID$(DS$, INSTR(DS$, " ") + 1)
    V$ = TRIM$(LEFT$(V$, INSTR(V$, " ") - 1))
    DS$ = "#DEFINE " + V$ + " SYSBITVAR" + STR$(INT(BVC% / 8)) + "." + STR$(BVC%-INT(BVC%/8)*8) 
    BVC% = BVC% + 1
   END IF
      
   'Convert Function f(x) to sub FN_f(x)
   IF LEFT$(DS$, 9) = "FUNCTION " THEN
    T$ = "FN_" + LTRIM$(RTRIM$(MID$(DS$, 10)))
    DS$ = "SUB " + T$
   END IF
   IF LEFT$(DS$, 14) = "WORD FUNCTION " THEN
    T$ = "FN_" + LTRIM$(RTRIM$(MID$(DS$, 15)))
    N$ = T$
    IF INSTR(N$, "(") <> 0 THEN N$ = LEFT$(N$, INSTR(N$, "(") - 1)
    IF INSTR(N$, " ") <> 0 THEN N$ = LEFT$(N$, INSTR(N$, " ") - 1)
    Replace N$, "FN_", ""
    N$ = LTRIM$(RTRIM$(N$))
    
    DS$ = "SUB " + T$
    LVC% = LVC% + 1
    LargeVars$(LVC%, 1) = "DIM " + N$ + " AS WORD"
    LargeVars$(LVC%, 2) = "FN_" + N$

   END IF
   
   'Also convert END FUNCTION to END SUB
   IF DS$ = "END FUNCTION" THEN DS$ = "END SUB"
   IF DS$ = "EXIT FUNCTION" THEN DS$ = "EXIT SUB"

   'If [WORD] is included, ensure it is at the end of the line
   IF INSTR(DS$, "[WORD]") <> 0 THEN
    IF INSTR(DS$, "[WORD]") + 6 < LEN(DS$) THEN
     Replace DS$, "[WORD]", ""
     DS$ = LTRIM$(DS$) + " [WORD]"
    END IF
   END IF

   'Convert WHILE and WEND to DO WHILE and LOOP
   IF LEFT$(DS$, 6) = "WHILE " THEN DS$ = "DO " + DS$
   IF DS$ = "WEND" THEN DS$ = "LOOP"
   
   'Check/fix binary and hex notation
   'Convert H' ' to 0x
   IF INSTR(DS$, "H'") <> 0 THEN
    Replace DS$, "H'", "0x"
    Replace DS$, "'", ""
   END IF
   'Convert 0b to B' '
   IF WholeINSTR(DS$, "0B") > 0 Then
    Replace DS$, "0B", "B'"
    T% = LEN(DS$)
    For SL% = INSTR(DS$, "B'") + 2 TO LEN(DS$)
     T$ = MID$(DS$, SL%, 1)   
     IF T$ = " " THEN T% = 1
     IF T$ = "(" THEN T% = 1
     IF T$ = ")" THEN T% = 1
     IF T$ = "," THEN T% = 1
     IF T$ = "." THEN T% = 1
     IF T$ = ":" THEN T% = 1
     IF T$ = "+" THEN T% = 1
     IF T$ = "-" THEN T% = 1
     IF T$ = "*" THEN T% = 1
     IF T$ = "/" THEN T% = 1
     IF T$ = "=" THEN T% = 1
     IF T$ = "!" THEN T% = 1
     IF T$ = "<" THEN T% = 1
     IF T$ = ">" THEN T% = 1
     IF T$ = "{" THEN T% = 1
     IF T$ = "}" THEN T% = 1
     IF T$ = "~" THEN T% = 1
     IF T$ = "&" THEN T% = 1
     IF T$ = "|" THEN T% = 1
     IF T$ = "#" THEN T% = 1
     IF T% = 1 THEN T% = SL%: Exit For
    Next    
    LT$ = LEFT$(DS$, T% - 1)
    RT$ = ""
    IF T% < LEN(DS$) Then RT$ = MID$(DS$, T%)
    DS$ = LT$ + "'" + RT$
   END IF
      
   'Replace any string variable references with arrays, after adding string to list
   IF WholeINSTR(DS$, "$") = 1 AND LEFT$(DS$, 1) <> " " THEN
    DO WHILE INSTR(DS$, "$") <> 0
     Replace DS$, "$", "()"
    LOOP
   END IF
   
   'Remove any tabs and double spaces (again)
   DO WHILE INSTR(DS$, CHR$(9)) <> 0: Replace DS$, CHR$(9), " ": LOOP
   DO WHILE INSTR(DS$, "  ") <> 0: Replace DS$, "  ", " ": LOOP

   'Decide if the line read is part of a sub or not
   IF LEFT$(DS$, 4) = "SUB " THEN
    IF S% = 1 THEN
     SUBDATA$(SBC%, 3) = STR$(SDC%)
    END IF
    S% = 1

    'Find any large vars used as parameters
    DO WHILE INSTR(DS$, " AS ") <> 0
     'Get sub name, var name and var type
     N$ = LTRIM$(MID$(DS$, 5))
     V$ = N$
     N$ = RTRIM$(LEFT$(N$, INSTR(N$, "(") - 1)) 'Sub name
     
     Ty$ = MID$(V$, INSTR(V$, " AS ") + 4)
     IF LEFT$(Ty$, 4) = "WORD" THEN Ty$ = LEFT$(Ty$, 4) 'Var type

     V$ = RTRIM$(LEFT$(V$, INSTR(V$, " AS ") - 1))
     DO WHILE INSTR(V$, " ") <> 0: Replace V$, " ", "": LOOP
     IF INSTR(V$, "(") <> 0 THEN Replace V$, "(", ","
     FOR T% = LEN(V$) TO 1 STEP -1
      IF MID$(V$, T%, 1) = "," THEN V$ = MID$(V$, T% + 1): EXIT FOR
     NEXT T%
     
     'Remove type from sub name to prevent confusion later on
     Replace DS$, " AS " + Ty$, ""
     
     LVC% = LVC% + 1
     LargeVars$(LVC%, 1) = "DIM " + V$ + " AS " + Ty$
     LargeVars$(LVC%, 2) = N$
    LOOP

    SBC% = SBC% + 1
    SUBDATA$(SBC%, 1) = MID$(DS$, 5)
    SUBDATA$(SBC%, 2) = STR$(SDC% + 1)
    SUBDATA$(SBC%, 4) = STR$(RF%)
    GOTO LoadNextLine
   END IF

   IF LEFT$(DS$, 7) = "END SUB" THEN
    S% = 0
    SUBDATA$(SBC%, 3) = STR$(SDC%)
    GOTO LoadNextLine
   END IF

   'Decide if the line read is part of a data table or not
   IF LEFT$(DS$, 6) = "TABLE " THEN
    IF S% > 1 THEN
     ERC% = ERC% + 1: gcERR$(ERC%) = "Data table " + SUBDATA$(SBC%, 1) + " does not have a corresponding END TABLE"
    END IF
    S% = S% OR 2
    DTC% = DTC% + 1
    DTLC% = 0
    DataTables$(DTC%) = LTRIM$(RTRIM$(MID$(DS$, 7)))
    GOTO LoadNextLine
   END IF
   IF LEFT$(DS$, 9) = "END TABLE" THEN
    S% = S% AND 253
    DataTable%(DTC%, 0) = DTLC%
    GOTO LoadNextLine
   END IF
   
   'Automatic initialisation preparation
   IF LEFT$(DS$, 8) = "#STARTUP" THEN gcINC$(RF%, 2) = ";" + LTRIM$(RTRIM$(MID$(DS$, 9))): GOTO LoadNextLine
   IF LEFT$(DS$, 7) = "#DEFINE" THEN DS$ = DS$ + "':" + STR$(RF%)

   'Does the command need to be inserted into the main routine, regardless of sub/not sub?
   ForceMain% = 0
   T$ = ""
   IF LEFT$(DS$, 8) = "#DEFINE " THEN ForceMain% = 1
   IF LEFT$(DS$, 5) = "#OSC " THEN ForceMain% = 1
   IF LEFT$(DS$, 8) = "#CONFIG " THEN ForceMain% = 1
   IF LEFT$(DS$, 5) = "#MEM " THEN ForceMain% = 1
   IF LEFT$(DS$, 5) = "#RAM " THEN ForceMain% = 1
   IF LEFT$(DS$, 5) = "#INT " THEN ForceMain% = 1
   IF LEFT$(DS$, 6) = "#CHIP " THEN ForceMain% = 1
   
   IF INSTR(DS$, ":") <> 0 AND RIGHT$(DS$, 1) <> ":" AND LEFT$(DS$, 8) <> "#DEFINE " THEN
    IF INSTR(DS$, ":") > INSTR(DS$, CHR$(34)) AND INSTR(INSTR(DS$, ":"), DS$, CHR$(34)) <> 0 THEN GOTO DontSplitLoad
    T$ = DS$
    DS$ = RTRIM$(LEFT$(DS$, INSTR(DS$, ":") - 1))
    T$ = LTRIM$(MID$(T$, INSTR(T$, ":") + 1))
   END IF
DontSplitLoad:

   'Add tag to show origin of line
   'Except for directives, ASM and labels
   IF LEFT$(DS$, 1) <> "#" AND LEFT$(DS$, 1) <> " " AND RIGHT$(DS$, 1) <> ":" THEN DS$ = DS$ + " ;?F" + STR$(RF%) + "L" + STR$(LC%)+"?"

   IF S% = 0 OR ForceMain% = 1 THEN APC% = APC% + 1: PROG$(APC%) = DS$
   IF S% = 1 AND ForceMain% = 0 THEN SDC% = SDC% + 1: SUBCODE$(SDC%) = DS$
   IF S% = 2 THEN
    DTLC% = DTLC% + 1
    IF INSTR(DS$, ";") <> 0 Then DS$ = TRIM$(LEFT$(DS$, INSTR(DS$, ";") - 1))
    DataTable%(DTC%, DTLC%) = MakeDec(DS$)
   END IF

   IF T$ <> "" THEN DS$ = T$: GOTO MultiCommand
  END IF
LoadNextLine:
 LOOP
 
 IF S% = 1 THEN
  SUBDATA$(SBC%, 3) = STR$(SDC%)
  S% = 0
 END IF
 CLOSE
 IF VBS% = 1 THEN LOCATE , 60: PRINT STR$(INT(RF% / ICC% * 100)); "% ";

LoadNextFile:
 
NEXT RF%
IF VBS% = 1 THEN PRINT

'Find compiler directives, except SCRIPT, ENDSCRIPT, IFDEF and ENDIF
IF VBS% = 1 THEN
 PRINT SPC(5); Message$("CompDirs");
END IF
PD% = 0
RemoveCompDirs:
PD% = PD% + 1
 T$ = PROG$(PD%)
 IF LEFT$(T$, 1) = "#" AND INSTR(T$, "IFDEF") = 0 AND INSTR(T$, "ENDIF") = 0 AND INSTR(T$, "SCRIPT") = 0 AND INSTR(T$, "ENDSCRIPT") = 0 THEN
  DelLine PD%: PD% = PD% - 1

  IF LEFT$(T$, 7) = "#DEFINE" THEN
   N$ = MID$(T$, INSTR(T$, " ") + 1): N$ = LTRIM$(RTRIM$(N$))
   T$ = RTRIM$(LEFT$(N$, INSTR(N$, "'") - 1))

   IF INSTR(T$, " ") = 0 THEN
    N$ = T$
    F$ = ""
    V$ = ""
   END IF
   IF INSTR(T$, " ") <> 0 THEN
    V$ = MID$(N$, INSTR(N$, " ") + 1)
    F$ = LTRIM$(RTRIM$(MID$(V$, INSTR(V$, "':") + 2)))
    V$ = LTRIM$(RTRIM$(LEFT$(V$, INSTR(V$, "':") - 1)))
    N$ = LEFT$(N$, INSTR(N$, " ") - 1)
   END IF

   IF LEFT$(V$, 1) = "=" THEN
    V$ = MID$(V$, 2)
    Calculate V$
    V$ = LTRIM$(RTRIM$(V$))
   END IF
   T% = 0
   FOR CE% = 1 TO DFC%
    IF N$ = gcDEF$(CE%, 1) THEN T% = 1: EXIT FOR
   NEXT CE%
   IF T% = 0 THEN
    DFC% = DFC% + 1
    gcDEF$(DFC%, 1) = N$
    gcDEF$(DFC%, 2) = V$
    gcDEF$(DFC%, 3) = F$
   END IF
  END IF

  IF LEFT$(T$, 5) = "#CHIP" AND ChipName$ = "" THEN
   ChipName$ = MID$(T$, 6)
   ChipMhz = VAL(MID$(ChipName$, INSTR(ChipName$, ",") + 1))
   ChipName$ = LTRIM$(RTRIM$(LEFT$(ChipName$, INSTR(ChipName$, ",") - 1)))
   IF LEFT$(UCASE$(ChipName$), 3) = "PIC" THEN ChipName$ = MID$(ChipName$, 4)
   IF LEFT$(UCASE$(ChipName$), 1) = "P" THEN ChipName$ = MID$(ChipName$, 2)
'   'Find chip family (PIC12, PIC14 or PIC16)
'   ChipFamily% = 14
'   IF INSTR(ChipName$, "C5") <> 0 OR INSTR(ChipName$, "F5") OR INSTR(ChipName$, "10F") THEN ChipFamily% = 12
'   IF INSTR(ChipName$, "18F") <> 0 OR INSTR(ChipName$, "18C") <> 0 THEN ChipFamily% = 16
  END IF

  IF LEFT$(T$, 4) = "#OSC" AND OSC$ = "" THEN OSC$ = LTRIM$(RTRIM$(MID$(T$, 5)))

  IF LEFT$(T$, 4) = "#INT" AND Intrpt$ = "" THEN Intrpt$ = LTRIM$(RTRIM$(MID$(T$, 5)))
  
  IF LEFT$(T$, 7) = "#CONFIG" THEN
   T$ = LTRIM$(RTRIM$(MID$(T$, 8)))
   IF CONFIG$ <> "" THEN T$ = "," + T$
   CONFIG$ = CONFIG$ + T$
  END IF

  IF LEFT$(T$, 4) = "#RAM" AND ChipRam% = 0 THEN ChipRam% = VAL(LTRIM$(RTRIM$(MID$(T$, 5))))
  IF LEFT$(T$, 4) = "#MEM" AND ChipRam% = 0 THEN ChipRam% = VAL(LTRIM$(RTRIM$(MID$(T$, 5))))
  
 END IF
 IF VBS% = 1 THEN LOCATE , 60: PRINT STR$(INT(PD% / APC% * 100)); "%";
IF PD% < APC% THEN GOTO RemoveCompDirs

'    'Get chip data from chipdata.csv
'    'Read message list
'    #IFDEF __FB_LINUX__
'     M$ = ID$ + "/chipdata.csv"
'    #ELSE
'     M$ = ID$ + "\chipdata.csv"
'    #ENDIF
'    OPEN M$ For Input As #1
'    DO WHILE NOT EOF(1)
'     LINE INPUT #1, DS$
'     Do While INSTR(DS$, " ") <> 0: Replace DS$, " ", "": Loop
'     IF INSTR(UCASE$(DS$), UCASE$("PIC"+ChipName$+",")) <> 0 THEN
'      T$ = MID$(DS$, INSTR(DS$, ",") + 1)
'      T$ = MID$(T$, INSTR(T$, ",") + 1)
'      T$ = MID$(T$, INSTR(T$, ",") + 1)
'      If ChipRam% = 0 THEN ChipRam% = VAL(T$)
'      GOTO ChipDataFound
'     END IF
'    LOOP
'    ChipDataFound:
'    CLOSE
'

'Get chip data
IF VBS% = 1 THEN PRINT: PRINT SPC(5); Message$("ReadChipData")
ReadChipData

IF VBS% = 1 THEN
 PRINT
 PRINT SPC(10); Message$("ChipS")
 PRINT SPC(15); Message$("ChipM") + ChipName$
 PRINT SPC(15); Message$("ChipF") + "PIC" + STR$(ChipFamily%)
 PRINT SPC(15); Message$("ChipC") + STR$(ChipMhz)
 PRINT SPC(15); Message$("ChipR") + STR$(ChipRam%)
END IF
'Set chip config defines for #IFDEF and #SCRIPT use
DFC% = DFC% + 1: gcDEF$(DFC%, 1) = "CHIPNAME": gcDEF$(DFC%, 2) = ChipName$
DFC% = DFC% + 1: gcDEF$(DFC%, 1) = "CHIPMHZ": gcDEF$(DFC%, 2) = STR$(ChipMhz)
DFC% = DFC% + 1: gcDEF$(DFC%, 1) = "CHIPFAMILY": gcDEF$(DFC%, 2) = STR$(ChipFamily%)
DFC% = DFC% + 1: gcDEF$(DFC%, 1) = "OSC": gcDEF$(DFC%, 2) = OSC$

'Initialise built-in data, and prepare built-in subs
PrepareBuiltIn

'Find and run compiler scripts
IF VBS% = 1 THEN PRINT SPC(5); Message$("RunScripts")
RunScripts

'IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("FindFreeRAM")
'FindFreeRAM

IF VBS% = 1 THEN PRINT: PRINT SPC(5); Message$("BuildMemoryMap")
BuildMemoryMap

'Remove any #IFDEFs that do not apply to the program
IF VBS% = 1 THEN PRINT : PRINT SPC(5); Message$("RemIfDefs")
RemIfDefs

'Replace constants with their values
IF VBS% = 1 THEN PRINT SPC(5); Message$("RepDefs");
ReplaceConstants
IF VBS% = 1 THEN PRINT

'Break up lines with multiple commands
IF VBS% = 1 THEN PRINT SPC(5); Message$("SplitLines")
SplitLines

END SUB

SUB ProcessArrays

PD% = 0
ProcessArrayCalls:
PD% = PD% + 1

 DS$ = PROG$(PD%)
 Origin$ = ""
 IF INSTR(DS$, ";?F") <> 0 THEN 
  Origin$ = MID$(DS$, INSTR(DS$, ";?F"))
  DS$ = RTRIM$(LEFT$(DS$, INSTR(DS$, ";?F") - 1))
 END IF
 ATV% = 0 'Number of array temporary variables
 
 'Does the line contain an array?
CheckArrayAgain:
 AF% = 0
 FOR CD% = 1 TO ARC%
  N$ = ArrayData$(CD%, 1)
  
  T% = WholeINSTR(DS$, N$)
  
  IF T% = 2 THEN
   IF INSTR(DS$, "()") <> 0 THEN
    T$ = MID$(DS$, INSTR(DS$, N$) + LEN(N$))
    T$ = LEFT$(T$, 2)
    IF T$ = "()" THEN AF% = 0: EXIT FOR
   END IF
   AF% = CD%: EXIT FOR
  END IF

 NEXT CD%

 'Show error if array has not been found
 IF AF% = 0 AND INSTR(DS$, "(") <> 0 AND INSTR(DS$, "()") = 0 THEN
  DS$ = LEFT$(DS$, INSTR(DS$, "(") - 1)
  FOR SS% = LEN(DS$) TO 1 STEP -1
   IF MID$(DS$, SS%, 1) = " " THEN DS$ = MID$(DS$, SS% + 1)
  NEXT SS%
  IF DS$ <> "" THEN
   T% = ASC(UCASE$(RIGHT$(DS$, 1)))
   IF (T% > 64 AND T% < 91) OR (T% > 47 AND T% < 58) THEN
    ERC% = ERC% + 1
    T$ = Message$("ArrayNoDec")
    Replace T$, "%Name%", DS$
    gcERR$(ERC%) = Origin$ + T$
   END IF
  END IF
 END IF

 'Array has been found, so generate code to access it
 IF AF% <> 0 THEN
  
  'Is array being read or set?
  ArrayDir% = 0 '0 = read, 1 = set
  'These commands set variables:
  IF LEFT$(DS$, 4) = "SET " THEN ArrayDir% = 1
  IF INSTR(DS$, "=") > INSTR(DS$, N$) THEN ArrayDir% = 1
  IF LCASE$(LEFT$(LTRIM$(DS$), 6)) = "movwf " THEN ArrayDir% = 1
  'These commands read variables, but may appear to set them based on above
  IF LEFT$(DS$, 3) = "IF " THEN ArrayDir% = 0
  IF LEFT$(DS$, 3) = "DO " THEN ArrayDir% = 0
  IF LEFT$(DS$, 5) = "WAIT " THEN ArrayDir% = 0
  IF LEFT$(DS$, 10) = "READTABLE " Then
   T$ = DS$
   Replace T$, ",", ""
   IF INSTR(T$, ",") < INSTR(T$, N$) THEN ArrayDir% = 1
  END IF
  
  'Remove array from line, and replace with a temporary value
  T$ = MID$(DS$, INSTR(DS$, N$))
  L% = 1
  P% = INSTR(T$, "(")
FindEndBracket:
  P% = P% + 1
   IF MID$(T$, P%, 1) = "(" THEN L% = L% + 1
   IF MID$(T$, P%, 1) = ")" THEN L% = L% - 1
  IF L% <> 0 AND P% < LEN(T$) THEN GOTO FindEndBracket
  IF L% <> 0 THEN
   FOR AB% = 1 TO L%
    DS$ = DS$ + ")"
   NEXT AB%
   L% = 0
  END IF

  IF L% = 0 THEN
   T$ = LEFT$(T$, P%)

   'Alter line
   ATV% = ATV% + 1
   AV$ = "SysArrayTemp" + STR$(ATV%)
   VLC% = VLC% + 1: VARLIST$(VLC%) = AV$
   Replace DS$, T$, AV$
   PROG$(PD%) = DS$

   'Find array location
   ArrayPosition$ = MID$(T$, INSTR(T$, "(") + 1)
   ArrayPosition$ = LEFT$(ArrayPosition$, LEN(ArrayPosition$) - 1)
   
   'Find array type
   IF ArrayData$(AF%, 4) = "DIM" THEN
    T% = VAL(ArrayData$(AF%, 2))
    T% = T% AND 255
    ArrayHandler$ = STR$(T%)
    ArrayType% = 0
   END IF
   IF ArrayData$(AF%, 4) = "SUB" THEN
    ArrayHandler$ = "Sys" + N$ + "Handler"
    VLC% = VLC% + 1
    VARLIST$(VLC%) = ArrayHandler$
    ArrayType% = 1
   END IF

   'Add code to read/set array
   If ChipFamily% <> 16 THEN 
    AddLine "FSR = " + ArrayHandler$ + "+" + ArrayPosition$, PD% + ArrayDir%
    INDF$ = "INDF"
   END IF
   If ChipFamily% = 16 THEN 
    AddLine "FSR0L = " + ArrayHandler$ + "+" + ArrayPosition$, PD% + ArrayDir%
    INDF$ = "INDF0"
   END IF
   IF ArrayType% = 0 THEN
    IF ChipFamily% = 12 OR ChipFamily% = 14 THEN
     IF VAL(ArrayData$(AF%, 2)) < 256 THEN T$ = " bcf STATUS, IRP"
     IF VAL(ArrayData$(AF%, 2)) > 255 THEN T$ = " bsf STATUS, IRP"
    END IF
    IF ChipFamily% = 16 THEN T$ = "FSR0H = " + STR$(INT(VAL(ArrayData$(AF%, 2))/256))
    PD% = PD% + 1
    AddLine T$, PD%
   END IF
   IF ArrayType% = 1 THEN
    IF ChipFamily% <> 16 THEN
     AddLine " bcf STATUS, IRP", PD% + ArrayDir% + 1
     AddLine " btfsc " + ArrayHandler$ + "_H,0", PD% + ArrayDir% + 2
     AddLine " bsf STATUS, IRP", PD% + ArrayDir% + 3
     PD% = PD% + 3
    END IF
    IF ChipFamily% = 16 THEN
     AddLine "FSR0H = " + ArrayHandler$ + "_H", PD% + ArrayDir% + 1
     PD% = PD% + 1
    END IF
   END IF
   IF ArrayDir% = 0 THEN AddLine AV$ + " = " + INDF$, PD% + 1
   IF ArrayDir% = 1 THEN AddLine INDF$ + " = " + AV$, PD% + 2
  END IF
  PD% = PD% - 1
  GOTO CheckArrayAgain
 END IF

IF PD% < APC% THEN GOTO ProcessArrayCalls

END SUB

SUB ReadChipData

    'Get filename
#IFDEF __FB_LINUX__
    ChipDataFile$ = ID$ + "/chipdata/" + LCASE$(ChipName$) + ".dat"
#ELSE
    ChipDataFile$ = ID$ + "\chipdata\" + ChipName$ + ".dat"
#ENDIF
  
    IF VBS% = 1 THEN PRINT SPC(10); ChipDataFile$
    OPEN ChipDataFile$ for input as #1
    ReadDataMode$ = ""
    DO WHILE NOT EOF(1)
        LINE INPUT #1, DS$
        DS$ = TRIM$(LCASE$(DS$))
        IF DS$ = "" THEN goto ReadNextChipInfoLine
        IF LEFT$(DS$, 1) = "'" THEN goto ReadNextChipInfoLine
        
        IF LEFT$(DS$, 1) = "[" AND RIGHT$(DS$, 1) = "]" THEN
            ReadDataMode$ = DS$
            goto ReadNextChipInfoLine
        END IF
        
        'ChipData
        IF ReadDataMode$ = "[chipdata]" AND INSTR(DS$, "=") <> 0 THEN
            N$ = TRIM$(LEFT$(DS$, INSTR(DS$, "=") - 1))
            V$ = TRIM$(MID$(DS$, INSTR(DS$, "=") + 1))
            
            IF N$ = "configwords" then ConfWords% = VAL(V$)
            IF N$ = "ram" then ChipRam% = VAL(V$)
            IF N$ = "family" then ChipFamily% = VAL(V$)
            IF N$ = "psp" then PSP% = VAL(V$)
            IF N$ = "maxaddress" then MemSize% = VAL(V$)
        END IF
                
        'Registers
        If ReadDataMode$ = "[registers]" AND INSTR(DS$, ",") <> 0 THEN
            SVC% = SVC% + 1
            DS$ = UCASE$(DS$)
            SysVars$(SVC%, 1) = TRIM$(LEFT$(DS$, INSTR(DS$, ",") - 1))
            SysVars$(SVC%, 2) = TRIM$(MID$(DS$, INSTR(DS$, ",") + 1))
        END IF
        
        'Bits
        ' 1 = name 3 = parent
        ' In file: name, parent, location
        If ReadDataMode$ = "[bits]" AND INSTR(DS$, ",") <> 0 THEN
            SVBC% = SVBC% + 1
            DS$ = UCASE$(DS$)
            SysVarBits$(SVBC%, 1) = TRIM$(LEFT$(DS$, INSTR(DS$, ",") - 1)) 'Name
            T$ = TRIM$(MID$(DS$, INSTR(DS$, ",") + 1))
            SysVarBits$(SVBC%, 3) = TRIM$(LEFT$(T$, INSTR(T$, ",") - 1)) 'Location
            SysVarBits$(SVBC%, 2) = TRIM$(MID$(T$, INSTR(T$, ",") + 1)) 'Parent
        END IF
        
        'FreeRAM
        If ReadDataMode$ = "[freeram]" AND INSTR(DS$, ":") <> 0 THEN
            MRC% = MRC% + 1
            MemRanges$(MRC%) = DS$
        END IF
        
        'Config
        IF ReadDataMode$ = "[config]" THEN
            COC% = COC% + 1
            DS$ = UCASE$(DS$)
            IF INSTR(DS$, ":") <> 0 THEN DS$ = LEFT$(DS$, INSTR(DS$, ":") - 1)
            IF INSTR(DS$, ",") <> 0 THEN
                ConfigOp$(COC%) = TRIM$(LEFT$(DS$, INSTR(DS$, ",") - 1))
                ConfigLoc%(COC%) = VAL(MID$(DS$, INSTR(DS$, ",") + 1))
            END IF
            IF INSTR(DS$, ",") = 0 THEN
                ConfigOp$(COC%) = DS$
            END IF
        END IF
        
        ReadNextChipInfoLine:
    LOOP
    CLOSE
    
END SUB

SUB RemIfDefs
'Remove IFDEFs that are not applicable

RIDM% = 0
RemIfDefMode:
DS% = 0
IL% = 0
RemIfDef:
 DS% = DS% + 1
 IF RIDM% = 0 THEN T$ = PROG$(DS%)
 IF RIDM% = 1 THEN T$ = SUBCODE$(DS%)
 T% = 0

 IF LEFT$(UCASE$(T$), 6) = "#IFDEF" AND IL% = 0 THEN
  
  IF INSTR(T$, ";") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, ";") - 1)
  T% = 1
  SV% = DS%
  
  'Read real and test values
  C$ = LTRIM$(RTRIM$(MID$(T$, 8)))

  IF INSTR(C$, "BIT(") <> 0 THEN
   FV% = 0: IF INSTR(C$, "NOBIT(") <> 0 THEN FV% = 1

   T$ = MID$(C$, INSTR(C$, "(") + 1)
   T$ = LEFT$(T$, INSTR(T$, ")") - 1)

   ConstFound% = 0
   FOR FC% = 1 TO SVBC%
    IF SysVarBits$(FC%, 1) = T$ THEN ConstFound% = 1: EXIT FOR
   NEXT FC%
   T% = 1
   IF ConstFound% = 1 THEN T% = 2

   IF FV% = 1 THEN
    IF T% = 1 THEN T% = 3
    IF T% = 2 THEN T% = 1
    IF T% = 3 THEN T% = 2
   END IF
   
   GOTO IfDefProcessed  
  END IF

  IF INSTR(C$, "VAR(") <> 0 THEN
   FV% = 0: IF INSTR(C$, "NOVAR(") <> 0 THEN FV% = 1

   T$ = MID$(C$, INSTR(C$, "(") + 1)
   T$ = LEFT$(T$, INSTR(T$, ")") - 1)

   ConstFound% = 0
   FOR FC% = 1 TO SVC%
    IF SysVars$(FC%, 1) = T$ THEN ConstFound% = 1: EXIT FOR
   NEXT FC%
   T% = 1
   IF ConstFound% = 1 THEN T% = 2

   IF FV% = 1 THEN
    IF T% = 1 THEN T% = 3
    IF T% = 2 THEN T% = 1
    IF T% = 3 THEN T% = 2
   END IF
   
   GOTO IfDefProcessed
  END IF

  IF INSTR(C$, "ONEOF(") <> 0 OR INSTR(C$, "ALLOF(") <> 0 THEN

   FV% = 0: IF INSTR(C$, "ALLOF(") <> 0 THEN FV% = 1
   
   'Get list of defines to search for
   T$ = MID$(C$, INSTR(C$, "(") + 1)
   T$ = LEFT$(T$, INSTR(T$, ")") - 1)
   DC% = 0
   DO WHILE INSTR(T$, ",")
    DC% = DC% + 1
    TempData$(DC%) = TRIM$(LEFT$(T$, INSTR(T$, ",") - 1))
    T$ = MID$(T$, INSTR(T$, ",") + 1)
   LOOP
   DC% = DC% + 1
   TempData$(DC%) = TRIM$(T$)
   
   'Search
   VF% = 0
   FOR SD% = 1 to DC%
    FOR FC% = 1 TO DFC%
     IF gcDEF$(FC%, 1) = TempData$(SD%) THEN VF% = VF% + 1: EXIT FOR
    NEXT FC%
   NEXT
   
   'Decide outcome
   T% = 1
   IF FV% = 0 AND VF% > 0 THEN T% = 2
   IF FV% = 1 AND VF% = DC% THEN T% = 2
   GOTO IfDefProcessed
   
  END IF
  
  CheckValue% = 0
  IF INSTR(C$, " ") <> 0 THEN
   CheckValue% = 1
   V$ = MID$(C$, INSTR(C$, " ") + 1)
   C$ = LEFT$(C$, INSTR(C$, " ") - 1)
  END IF
  
  IF CheckValue% = 0 THEN
   ConstFound% = 0
   FOR FC% = 1 TO DFC%
    IF gcDEF$(FC%, 1) = C$ THEN ConstFound% = 1: EXIT FOR
   NEXT FC%
   T% = 1
   IF ConstFound% = 1 THEN T% = 2

   GOTO IfDefProcessed
  END IF

  VC% = 0
  DO WHILE INSTR(V$, ",") <> 0
   VC% = VC% + 1
   TempData$(VC%) = LEFT$(V$, INSTR(V$, ",") - 1)
   V$ = MID$(V$, INSTR(V$, ",") + 1)
   V$ = LTRIM$(V$)
  LOOP
  VC% = VC% + 1
  TempData$(VC%) = V$
  AD% = 0

  'Replace names of test constants with values
  FOR SD% = 1 TO VC%
   TV% = 0
   FOR FV% = 1 TO DFC%
    IF UCASE$(TempData$(SD%)) = UCASE$(gcDEF$(FV%, 1)) THEN TV% = FV%: EXIT FOR
   NEXT FV%
   IF TV% <> 0 THEN TempData$(SD%) = gcDEF$(FV%, 2)
  NEXT SD%
  T% = 1

  TV$ = C$
  ReplaceConstantsLine TV$
  IF INSTR(TV$, ";") <> 0 THEN TV$ = LEFT$(TV$, INSTR(TV$, ";") - 1)

  'Compare real and test values
  FOR SD% = 1 TO VC%
   IF UCASE$(TempData$(SD%)) = UCASE$(TV$) THEN T% = 2: EXIT FOR
  NEXT SD%

IfDefProcessed:
  'T% is 2 if true, 1 if false
  'Remove everything up to the #ENDIF
  IF T% = 1 THEN
   IF RIDM% = 0 THEN
    FOR CD% = SV% TO APC%
     IF UCASE$(LEFT$(PROG$(CD%), 6)) = "#IFDEF" THEN IL% = IL% + 1
     IF UCASE$(LEFT$(PROG$(CD%), 6)) = "#ENDIF" THEN IL% = IL% - 1
     IF IL% = 0 THEN EV% = CD%: EXIT FOR
    NEXT CD%
    FOR CD% = SV% TO EV%
     DelLine SV%
    NEXT CD%
   END IF
   IF RIDM% = 1 THEN
    FOR CD% = SV% TO SDC%
     IF UCASE$(LEFT$(SUBCODE$(CD%), 6)) = "#IFDEF" THEN IL% = IL% + 1
     IF UCASE$(LEFT$(SUBCODE$(CD%), 6)) = "#ENDIF" THEN IL% = IL% - 1
     IF IL% = 0 THEN EV% = CD%: EXIT FOR
    NEXT CD%
    FOR CD% = SV% TO EV%
     'DelSubLine SV%
     SUBCODE$(CD%) = ""
    NEXT CD%    
   END IF
   DS% = DS% - 1
  END IF
  
  'Remove the IFDEF and corresponding #ENDIF
  IF T% = 2 THEN
   IF RIDM% = 0 THEN
    FOR CD% = SV% TO APC%
     IF UCASE$(LEFT$(PROG$(CD%), 6)) = "#IFDEF" THEN IL% = IL% + 1
     IF UCASE$(LEFT$(PROG$(CD%), 6)) = "#ENDIF" THEN IL% = IL% - 1
     IF IL% = 0 THEN EV% = CD%: EXIT FOR
    NEXT CD%
    DelLine EV%
    DelLine SV%
    'PROG$(EV%) = ""
    'PROG$(SV%) = ""
   END IF
   IF RIDM% = 1 THEN
    FOR CD% = SV% TO SDC%
     IF UCASE$(LEFT$(SUBCODE$(CD%), 6)) = "#IFDEF" THEN IL% = IL% + 1
     IF UCASE$(LEFT$(SUBCODE$(CD%), 6)) = "#ENDIF" THEN IL% = IL% - 1
     IF IL% = 0 THEN EV% = CD%: EXIT FOR
    NEXT CD%
    'DelSubLine EV%
    'DelSubLine SV%
    SUBCODE$(EV%) = ""
    SUBCODE$(SV%) = ""
   END IF

   DS% = DS% - 1
  END IF

 END IF

IF RIDM% = 0 AND DS% < APC% THEN GOTO RemIfDef
IF RIDM% = 0 THEN RIDM% = 1: GOTO RemIfDefMode
IF RIDM% = 1 AND DS% < SDC% THEN GOTO RemIfDef

END SUB

SUB ReplaceConstants

'Replace constants with their values

'FOR DS% = 1 TO 2
 PDC% = APC%
' IF DS% = 2 THEN PDC% = SDC%

 FOR PD% = 1 TO PDC%
 ' IF DS% = 1 THEN DS$ = PROG$(PD%) ELSE DS$ = SUBCODE$(PD%)
 DS$ = PROG$(PD%)

  IF VBS% = 1 THEN
   LOCATE , 60: PRINT STR$(INT(PD% / APC% * 100)); "%";
  END IF

  T% = 0

ReplaceNestedConstants:
  T$ = DS$
  ReplaceConstantsLine DS$
  IF UCASE$(T$) <> UCASE$(DS$) THEN T% = 1: GOTO ReplaceNestedConstants
  IF T% = 1 THEN
   'IF DS% = 1 THEN PROG$(PD%) = DS$ ELSE SUBCODE$(PD%) = DS$
   PROG$(PD%) = DS$
  END IF
 NEXT PD%
'NEXT DS%

END SUB

SUB ReplaceConstantsLine (DS$)

 FOR CL% = 1 TO DFC%
  N$ = gcDEF$(CL%, 1)
  RC$ = gcDEF$(CL%, 2)
  SF$ = gcDEF$(CL%, 3)

REPCONST:
  IF INSTR(DS$, N$) <> 0 THEN
   T% = 0
   IF LEN(DS$) = LEN(N$) THEN T% = 2

   IF INSTR(DS$, N$) = 1 THEN T% = 1
   IF T% = 0 THEN
    T$ = MID$(DS$, INSTR(DS$, N$) - 1, 1)
    IF T$ = " " THEN T% = 1: GOTO LeftEndFound
    IF T$ = "(" THEN T% = 1: GOTO LeftEndFound
    IF T$ = ")" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "," THEN T% = 1: GOTO LeftEndFound
    IF T$ = "." THEN T% = 1: GOTO LeftEndFound
    IF T$ = ":" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "+" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "-" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "*" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "/" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "=" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "!" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "{" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "}" THEN T% = 1: GOTO LeftEndFound
    IF T$ = "<" THEN T% = 1: GOTO LeftEndFound
    IF T$ = ">" THEN T% = 1
LeftEndFound:
    T2$ = T$
   END IF
   
   IF INSTR(DS$, N$) + LEN(N$) - 1 = LEN(DS$) THEN T% = T% + 1
   IF T% < 2 THEN
    T$ = MID$(DS$, INSTR(DS$, N$) + LEN(N$), 1)
    IF T$ = " " THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "(" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = ")" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "," THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "." THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = ":" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = ";" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "+" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "-" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "*" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "/" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "=" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "!" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "{" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "}" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = "<" THEN T% = T% + 1: GOTO RightEndFound
    IF T$ = ">" THEN T% = T% + 1
RightEndFound:
   END IF

   IF T% = 2 THEN
    Replace DS$, N$, RC$
    IF SF$ <> "" AND INSTR(DS$, ";STARTUP" + gcDEF$(CL%, 3)) = 0 THEN DS$ = DS$ + ";STARTUP" + gcDEF$(CL%, 3)
    GOTO REPCONST
   END IF
   IF T% < 2 THEN
    Replace DS$, N$, "`"
   END IF
   GOTO REPCONST
  END IF

  DO WHILE INSTR(DS$, "`") <> 0: Replace DS$, "`", N$: LOOP
 NEXT CL%

END SUB

SUB RunScripts

'Read Scripts
'Read from main program
PD% = 0
ReadScriptMain:
 PD% = PD% + 1
 IF PROG$(PD%) = "#SCRIPT" THEN
  S% = PD%
  T% = 0
ReadScriptLinesMain:
  PD% = PD% + 1
  IF PROG$(PD%) <> "#ENDSCRIPT" THEN
   T% = T% + 1
   TempData$(T%) = PROG$(PD%)
   GOTO ReadScriptLinesMain
  END IF
  E% = PD%
  FOR CD% = S% TO E%
   DelLine S%
  NEXT CD%
  PD% = S% - 1
  GSReturn = 0
  GOTO ExecScript
ExecScriptDoneMain:
  
 END IF
IF PD% < APC% THEN GOTO ReadScriptMain

'Read from subs
PD% = 0
ReadScriptSubs:
 PD% = PD% + 1
 IF SUBCODE$(PD%) = "#SCRIPT" THEN
  S% = PD%
  T% = 0
ReadScriptLinesSubs:
  PD% = PD% + 1
  IF SUBCODE$(PD%) <> "#ENDSCRIPT" THEN
   T% = T% + 1
   TempData$(T%) = SUBCODE$(PD%)
   GOTO ReadScriptLinesSubs
  END IF
  E% = PD%
  FOR CD% = S% TO E%
   DelSubLine S%
  NEXT CD%
  PD% = S% - 1
  GSReturn = 1
  GOTO ExecScript
ExecScriptDoneSubs:
  
 END IF
IF PD% < SDC% THEN GOTO ReadScriptSubs

EXIT SUB

'Execute Script
ExecScript:
 SC% = 0
RunScript:
 SC% = SC% + 1
 CO$ = TempData$(SC%)
 IF INSTR(CO$, ";") <> 0 THEN CO$ = LEFT$(CO$, INSTR(CO$, ";") - 1)
 
'Prepare a version of the command with constants replaced by values
 COCR$ = CO$
 T% = 0
TokeniseQuotes:
 IF INSTR(COCR$, CHR$(34)) <> 0 THEN
  T% = T% + 1
  T$ = MID$(COCR$, INSTR(COCR$, CHR$(34)) + 1)
  T$ = LEFT$(T$, INSTR(T$, CHR$(34)) - 1)
  FILE$(T%) = T$
  Replace COCR$, CHR$(34) + T$ + CHR$(34), "$" + STR$(T%) + "$"
  GOTO TokeniseQuotes
 END IF

 ReplaceConstantsLine COCR$

 IF INSTR(COCR$, ";") <> 0 THEN COCR$ = LEFT$(COCR$, INSTR(COCR$, ";") - 1)

 DO WHILE INSTR(COCR$, "$") <> 0
  T% = VAL(MID$(COCR$, INSTR(COCR$, "$") + 1))
  T$ = "$" + STR$(T%) + "$"
  Replace COCR$, T$, FILE$(T%)
 LOOP
 
 'IF
 IF LEFT$(CO$, 3) = "IF " THEN
  C$ = MID$(COCR$, 4)
  IF INSTR(C$, "THEN") <> 0 THEN C$ = LEFT$(C$, INSTR(C$, "THEN") - 1)
  C$ = LTRIM$(RTRIM$(C$))
  IF INSTR(C$, "=") <> 0 THEN ACT$ = "="
  IF INSTR(C$, "!") <> 0 THEN ACT$ = "!"
  IF INSTR(C$, "<") <> 0 THEN ACT$ = "<"
  IF INSTR(C$, ">") <> 0 THEN ACT$ = ">"
  V1$ = RTRIM$(LEFT$(C$, INSTR(C$, ACT$) - 1))
  V2$ = LTRIM$(MID$(C$, INSTR(C$, ACT$) + 1))
  T% = VAL(V1$): T$ = LTRIM$(STR$(T%))
  IF T$ = LEFT$(V1$, LEN(T$)) THEN Calculate V1$
  T% = VAL(V2$): T$ = LTRIM$(STR$(T%))
  IF T$ = LEFT$(V2$, LEN(T$)) THEN Calculate V2$
  
  T% = 0
  IF ACT$ = "=" AND V1$ = V2$ THEN T% = 1
  IF ACT$ = "!" AND V1$ <> V2$ THEN T% = 1
  IF ACT$ = "<" AND VAL(V1$) < VAL(V2$) THEN T% = 1
  IF ACT$ = ">" AND VAL(V1$) > VAL(V2$) THEN T% = 1
  IF T% = 0 THEN
   TL% = 1
   DO WHILE TL% > 0
    SC% = SC% + 1
    T$ = TempData$(SC%)
    IF LEFT$(T$, 3) = "IF " THEN TL% = TL% + 1
    IF LEFT$(T$, 6) = "END IF" THEN TL% = TL% - 1
   LOOP
  END IF
 END IF

 'ERROR
 IF LEFT$(CO$, 6) = "ERROR " THEN
  T$ = MID$(COCR$, 7)
  IF INSTR(T$, "%STRING") <> 0 THEN
   DT$ = MID$(T$, INSTR(T$, "%") + 1)
   DT$ = "%" + LEFT$(DT$, INSTR(DT$, "%"))
   DS% = VAL(MID$(DT$, 8))
   PRINT T$, DT$, DS%
   Replace T$, DT$, StringStore$(DS%)
  END IF    
  IF LEFT$(T$, 1) = "%" AND RIGHT$(T$, 1) = "%" THEN
   Replace T$, "%", ""
   Replace T$, "%", ""
   T$ = Message$(T$)
  END IF
  ERC% = ERC% + 1
  gcERR$(ERC%) = T$
 END IF

 'CALCULATE
 IF INSTR(CO$, "=") <> 0 AND LEFT$(CO$, 3) <> "IF " THEN
  'Get data and output name
  O$ = LTRIM$(RTRIM$(LEFT$(CO$, INSTR(CO$, "=") - 1)))
  V$ = LTRIM$(RTRIM$(MID$(COCR$, INSTR(COCR$, "=") + 1)))
  
  'Check if the data is a sum, and calculate if it is
  DO WHILE INSTR(V$, "&") <> 0: Replace V$, "&", "AND": LOOP
  T$ = RTRIM$(LTRIM$(STR$(VAL(V$))))
  
  IF T$ = LEFT$(V$, LEN(T$)) OR INSTR(V$, "/") <> 0 OR INSTR(V$, "*") <> 0 OR INSTR(V$, "+") <> 0 OR INSTR(V$, "-") <> 0 OR INSTR(V$, "AND") <> 0 OR INSTR(V$, "OR") <> 0 OR INSTR(V$, "XOR") <> 0 THEN Calculate V$
  V$ = UCASE$(V$)

  'Write the data to the output
  T% = 0
  FOR CD% = 1 TO DFC%
   IF gcDEF$(CD%, 1) = O$ THEN T% = 1: EXIT FOR
  NEXT CD%
  V$ = LTRIM$(RTRIM$(V$))
  IF T% = 0 THEN
   DFC% = DFC% + 1
   gcDEF$(DFC%, 1) = O$
   gcDEF$(DFC%, 2) = V$
  END IF
  IF T% = 1 THEN gcDEF$(CD%, 2) = V$
 END IF

 IF SC% < (E% - S% - 1) THEN GOTO RunScript
 IF GSReturn = 0 THEN GOTO ExecScriptDoneMain
 IF GSReturn = 1 THEN GOTO ExecScriptDoneSubs

END SUB

SUB SortVarList

IF VLC% = 0 THEN EXIT SUB
IF VLC% = OldVLC% THEN EXIT SUB
IF VLC% = 1 THEN 
    If VARLIST$(1) = "" THEN VLC% = 0
    EXIT SUB
END IF

'Show var list
'FOR T% = 1 TO VLC%
'    PRINT VARLIST$(T%), VarType$(T%)
'NEXT

'Sort list of vars
SORTVARS:
T% = 0
FOR SC% = 1 TO VLC% - 1
 IF UCASE$(VARLIST$(SC%)) > UCASE$(VARLIST$(SC% + 1)) THEN
  'Swap name
  N$ = VARLIST$(SC% + 1)
  VARLIST$(SC% + 1) = VARLIST$(SC%)
  VARLIST$(SC%) = N$
  'Swap type
  N$ = VarType$(SC% + 1)
  VarType$(SC% + 1) = VarType$(SC%)
  VarType$(SC%) = N$
  T% = 1
 END IF
NEXT SC%
IF T% = 1 THEN GOTO SORTVARS

'Delete duplicate or blank entries in list
CD% = 0
DELDUPVARS:
 CD% = CD% + 1
 IF UCASE$(VARLIST$(CD%)) = UCASE$(VARLIST$(CD% + 1)) OR IsConst(VARLIST$(CD%)) OR TRIM$(VARLIST$(CD%)) = "" THEN
  IF VarType$(CD% + 1) = "" AND VarType$(CD%) <> "" THEN VarType$(CD% + 1) = VarType$(CD%)
  FOR DL% = CD% TO VLC%
   VARLIST$(DL%) = VARLIST$(DL% + 1)
   VarType$(DL%) = VarType$(DL% + 1)
  NEXT DL%
  VARLIST$(VLC%) = ""
  VarType$(VLC%) = ""
  VLC% = VLC% - 1
  CD% = CD% - 1
 END IF
IF CD% < VLC% - 1 THEN GOTO DELDUPVARS

'Clear out any remnants of deleted vars
For CD% = VLC% + 1 TO VLC% + 100
 VARLIST$(CD%) = ""
 VarType$(CD%) = ""
NEXT

OldVLC% = VLC%

END SUB

SUB SplitLines

'Search main program
PD% = 0
SplitMain:
 PD% = PD% + 1
 IF INSTR(PROG$(PD%), ":") <> 0 AND RIGHT$(PROG$(PD%), 1) <> ":" THEN
  T$ = PROG$(PD%)
  T2$ = LTRIM$(MID$(T$, INSTR(T$, ":") + 1))
  PROG$(PD%) = RTRIM$(LEFT$(T$, INSTR(T$, ":") - 1))
  AddLine T2$, PD% + 1
  PD% = PD% - 1
 END IF
IF PD% < APC% THEN GOTO SplitMain

EXIT SUB

'Search Subroutines
PD% = 0
SplitSubs:
 PD% = PD% + 1
 IF INSTR(SUBCODE$(PD%), ":") <> 0 AND RIGHT$(SUBCODE$(PD%), 1) <> ":" THEN
  T$ = SUBCODE$(PD%)
  T2$ = LTRIM$(RTRIM$(MID$(T$, INSTR(T$, ":") + 1)))
  SUBCODE$(PD%) = LTRIM$(RTRIM$(LEFT$(T$, INSTR(T$, ":") - 1)))
  AddSubLine T2$, PD% + 1
 END IF
IF PD% < SDC% THEN GOTO SplitSubs

END SUB

FUNCTION SubListLocation (SN$)

SubListLocation = 0
T$ = LTRIM$(RTRIM$(UCASE$(SN$)))

FOR T% = 1 TO SLC%
 IF LTRIM$(RTRIM$(UCASE$(SUBLIST$(T%, 1)))) = T$ THEN SubListLocation = T%: EXIT FUNCTION
NEXT

END FUNCTION

SUB SubParams
PD% = 0
REPSUBPARAM:
 PD% = PD% + 1
 T$ = PROG$(PD%)

 IF LEFT$(T$, 6) = " call " THEN
  Origin$ = ""
  IF INSTR(T$, ";?F") <> 0 THEN 
   Origin$ = MID$(T$, INSTR(T$, ";?F")) 
   PROG$(PD%) = RTRIM$(LEFT$(PROG$(PD%), INSTR(PROG$(PD%), ";?F") - 1))
   T$ = PROG$(PD%)   
  END IF
 END IF
 
 TP$ = MID$(T$, INSTR(T$, " call ") + 6)
 IF LEFT$(T$, 6) = " call " AND (INSTR(T$, "(") <> 0 OR INSTR(TP$, " ") <> 0) THEN
  
  FP% = INSTR(7, T$, " ") + 1
  IF INSTR(TP$, " ") <> 0 AND (INSTR(T$, "(") = 0 OR INSTR(T$, "(") > FP%) THEN
   SCT$ = LEFT$(T$, INSTR(7, T$, " "))
   Replace T$, SCT$, SCT$ + "("
   T$ = T$ + ")"
   PROG$(PD%) = T$
  END IF

  S$ = MID$(T$, 7): S$ = LEFT$(S$, INSTR(S$, "(") - 1)
  
  DO WHILE INSTR(S$, " ") <> 0: Replace S$, " ", "": LOOP
  FOR DS% = 1 TO SBC%
   DS$ = SUBDATA$(DS%, 1): T2$ = DS$
   IF INSTR(DS$, "(") <> 0 THEN DS$ = LEFT$(DS$, INSTR(DS$, "(") - 1)
   DS$ = LTRIM$(RTRIM$(DS$))
   IF DS$ = S$ THEN GOTO SubParamFound
  NEXT DS%
  IF PD% < APC% THEN GOTO REPSUBPARAM
  EXIT SUB

SubParamFound:
  'Check number of parameters is the same
  T% = 0
  IF INSTR(PROG$(PD%), "(") <> 0 THEN T% = T% + 1
  IF INSTR(T2$, "(") <> 0 THEN T% = T% - 1
  FOR CD% = 1 TO LEN(PROG$(PD%))
   IF MID$(PROG$(PD%), CD%, 1) = "," THEN T% = T% + 1
  NEXT CD%
  FOR CD% = 1 TO LEN(T2$)
   IF MID$(T2$, CD%, 1) = "," THEN T% = T% - 1
  NEXT CD%
  IF T% <> 0 THEN
   ERC% = ERC% + 1
   T$ = Message$("BadParam")
   Replace T$, "%sub%", PROG$(PD%)
   Replace T$, " call ", ""
   T3$ = T2$
   Replace T3$, "#NR", ""
   Replace T$, "%correct%", RTRIM$(T3$)
   gcERR$(ERC%) = Origin$ + T$
   'gcERR$(ERC%) = "Incorrect number of parameters: " + PROG$(PD%) + ". Correct syntax: " + T2$
   'ProgWillFail% = 1
   IF PD% < APC% THEN GOTO REPSUBPARAM
   EXIT SUB
  END IF

  T$ = MID$(T$, INSTR(T$, "(") + 1): T2$ = MID$(T2$, INSTR(T2$, "(") + 1)
  NR% = 0: IF INSTR(T2$, "#NR") <> 0 THEN NR% = 1
  IF INSTR(DS$, "FN_") <> 0 THEN NR% = 1
  IF T$ = ")" OR T2$ = ")" THEN GOTO NEXTSUBPARAM
  
  T$ = LTRIM$(RTRIM$(T$))
  IF RIGHT$(T$, 1) = ")" THEN T$ = LEFT$(T$, LEN(T$) - 1)
  'T$ = LEFT$(T$, INSTR(T$, ")") - 1)
  IF INSTR(T2$, "#NR") <> 0 THEN Replace T2$, "#NR", ""
  T2$ = LTRIM$(RTRIM$(T2$))
  IF RIGHT$(T2$, 1) = ")" THEN T2$ = LEFT$(T2$, LEN(T2$) - 1)
  'T2$ = LEFT$(T2$, INSTR(T2$, ")") - 1)

  T% = 0
  DO WHILE INSTR(T$, ",") <> 0
   T% = T% + 1
   FILE$(T%) = LEFT$(T$, INSTR(T$, ",") - 1)
   FILE$(T% + 25) = LEFT$(T2$, INSTR(T2$, ",") - 1)
   IF RIGHT$(FILE$(T%), 1) = " " THEN FILE$(T%) = LEFT$(FILE$(T%), LEN(FILE$(T%)) - 1)
   IF RIGHT$(FILE$(T% + 25), 1) = " " THEN FILE$(T% + 25) = LEFT$(FILE$(T% + 25), LEN(FILE$(T% + 25)) - 1)
   T$ = MID$(T$, INSTR(T$, ",") + 1)
   T2$ = MID$(T2$, INSTR(T2$, ",") + 1)
   IF LEFT$(T$, 1) = " " THEN T$ = MID$(T$, 2)
   IF LEFT$(T2$, 1) = " " THEN T2$ = MID$(T2$, 2)
  LOOP
  T% = T% + 1
  FILE$(T%) = T$
  FILE$(T% + 25) = T2$
  PROG$(PD%) = LEFT$(PROG$(PD%), INSTR(PROG$(PD%), "(") - 1)
  
  FOR CD% = 1 TO T%
   C% = 0
   IF IsConst(FILE$(CD%)) THEN C% = 1
   IF IsCalc(FILE$(CD%)) THEN C% = 2
   IF INSTR(FILE$(CD% + 25), "()") <> 0 THEN C% = 3
   IF INSTR(FILE$(CD%), "%STRING") <> 0 THEN C% = 4

   IF C% = 0 OR C% = 1 OR C% = 2 THEN

    AddLine FILE$(CD% + 25) + " = " + FILE$(CD%), PD%
    PD% = PD% + 1
    IF C% = 0 AND NR% = 0 THEN
     AddLine FILE$(CD%) + " = " + FILE$(CD% + 25), PD% + 1
    END IF
   END IF

   'Pass array using SYSTEMPARRAY
   IF C% = 4 THEN
    AddLine "SYSTEMPARRAY()=" + FILE$(CD%), PD%
    FILE$(CD%) = "SYSTEMPARRAY()"
    PD% = PD% + 1
    C% = 3
   END IF

   'Pass array by reference
   IF C% = 3 THEN
    SA$ = FILE$(CD%)
    SA$ = LEFT$(SA$, INSTR(SA$, "(") - 1)
    
    DA$ = FILE$(CD% + 25)
    DA$ = LEFT$(DA$, INSTR(DA$, "(") - 1)
    
    'Find source array location
    FOR SA% = 1 TO ARC%
     IF SA$ = ArrayData$(SA%, 1) THEN EXIT FOR
    NEXT SA%
    
    'Create destination array if necessary
    AF% = 0
    FOR DA% = 1 TO ARC%
     IF DA$ = ArrayData$(DA%, 1) THEN AF% = 1: EXIT FOR
    NEXT DA%
    IF AF% = 0 THEN
     ARC% = ARC% + 1
     ArrayData$(ARC%, 1) = DA$
     ArrayData$(ARC%, 4) = "SUB"
    END IF
    
    'Set handler
    ArrayHandler$ = "Sys" + DA$ + "Handler"
    VLC% = VLC% + 1: VARLIST$(VLC%) = ArrayHandler$
    VLC% = VLC% + 1: VARLIST$(VLC%) = ArrayHandler$ + "_H"
    ArrayLoc% = VAL(ArrayData$(SA%, 2)) AND 255

    AddLine " movlw " + STR$(ArrayLoc%), PD%
    AddLine " movwf " + ArrayHandler$, PD% + 1
    
    If ChipFamily% <> 16 THEN
     T% = 0
     IF VAL(ArrayData$(SA%, 2)) > 255 THEN T% = 1
     IF T% = 0 THEN AddLine " bcf " + ArrayHandler$ + "_H,0", PD% + 2
     IF T% = 1 THEN AddLine " bsf " + ArrayHandler$ + "_H,0", PD% + 2
    END IF
    IF ChipFamily% = 16 THEN AddLine ArrayHandler$ + "_H=" + GetByte$(ArrayData$(SA%, 2), 1), PD% + 2
           
    PD% = PD% + 3
   END IF
  NEXT CD%
 END IF
NEXTSUBPARAM:
IF PD% < APC% THEN GOTO REPSUBPARAM

END SUB

SUB TranslateFunctions

'Prepare list of functions
FC% = 0
FOR CD% = 1 TO SBC%
 IF INSTR(SUBDATA$(CD%, 1), "FN_") <> 0 THEN
  FC% = FC% + 1
  TempData$(FC%) = STR$(CD%)
  'PRINT SUBDATA$(CD%, 1)
 END IF
NEXT CD%

'Find functions
FOR DS% = 1 TO 2

 CD% = 0
FindFunctions:
  CD% = CD% + 1

FindFunctionsAgain:

  IF DS% = 1 THEN DS$ = PROG$(CD%)
  IF DS% = 2 THEN DS$ = SUBCODE$(CD%)

  'Run through list of functions
  FOR PD% = 1 TO FC%

   N$ = TRIM$(MID$(SUBDATA$(VAL(TempData$(PD%)), 1), 4))
   NO$ = N$
   IF INSTR(N$, "(") <> 0 THEN Replace N$, "(", " "
   IF INSTR(N$, " ") <> 0 THEN N$ = LEFT$(N$, INSTR(N$, " ") - 1)
   N$ = UCASE$(TRIM$(N$))
      
   'Check to see if a line contains a function
   SearchLineAgain:
   T% = WholeINSTR(UCASE$(DS$), N$)
   
   'Avoid calling functions from themselves
   IF DS% = 2 AND T% = 2 THEN
    S% = VAL(SUBDATA$(VAL(TempData$(PD%)), 2))
    E% = VAL(SUBDATA$(VAL(TempData$(PD%)), 3))
    IF CD% >= S% AND CD% <= E% THEN T% = 0
   END IF

   IF T% <> 2 AND INSTR(DS$, N$) <> 0 THEN 
       'PRINT DS$, N$
       Replace DS$, N$, "%N:" + STR$(PD%) + "%"
       Goto SearchLineAgain
   END IF

   'If it does, call a sub, and get the value after
   IF T% = 2 THEN

    'PRINT
    'PRINT "Original:", DS$

    'Replace the function in the line
    L$ = LEFT$(DS$, INSTR(DS$, N$) - 1)
    
    IF INSTR(NO$, "(") <> 0 THEN
     R$ = LTRIM$(RTRIM$(MID$(DS$, INSTR(DS$, N$) + LEN(N$))))
     BL% = 0
     FOR FB% = 1 TO LEN(R$)
      IF MID$(R$, FB%, 1) = "(" THEN BL% = BL% + 1
      IF MID$(R$, FB%, 1) = ")" THEN BL% = BL% - 1
      IF BL% = 0 THEN R$ = MID$(R$, FB% + 1): EXIT FOR
     NEXT FB%
    END IF

    IF INSTR(NO$, "(") = 0 THEN R$ = MID$(DS$, INSTR(DS$, N$) + LEN(N$))

    DSO$ = DS$
    Replace DSO$, L$, ""
    Replace DSO$, R$, ""
    DSO$ = TRIM$(DSO$)
    DSOP$ = DSO$
    T$ = MID$(SUBDATA$(VAL(TempData$(PD%)), 1), 4)
    'IF INSTR(DS$, "(") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, "(") - 1)
    'T$ = TRIM$(T$)
    IF INSTR(T$, "(") <> 0 THEN Replace T$, "(", " "
    IF INSTR(T$, " ") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, " ") - 1)
    T$ = UCASE$(TRIM$(T$))
     
    Replace DSOP$, T$, ""
    DS$ = L$ + "%F" + TempData$(PD%) + "F%" + R$

    'DO WHILE INSTR(DS$, "%~%") <> 0
    ' Replace DS$, "%~%", N$
    'LOOP

    DO WHILE INSTR(DS$, "%N:") <> 0
     T% = VAL(MID$(DS$, INSTR(DS$, "%N:") + 3))
     N$ = TRIM$(MID$(SUBDATA$(VAL(TempData$(T%)), 1), 4))
     IF INSTR(N$, "(") <> 0 THEN Replace N$, "(", " "
     IF INSTR(N$, " ") <> 0 THEN N$ = LEFT$(N$, INSTR(N$, " ") - 1)
     N$ = UCASE$(TRIM$(N$))
     
     Replace DS$, "%N:" + STR$(T%) + "%", N$
    LOOP
    
    DO WHILE INSTR(DSOP$, "%N:") <> 0
     T% = VAL(MID$(DSOP$, INSTR(DSOP$, "%N:") + 3))
     N$ = TRIM$(MID$(SUBDATA$(VAL(TempData$(T%)), 1), 4))
     IF INSTR(N$, "(") <> 0 THEN Replace N$, "(", " "
     IF INSTR(N$, " ") <> 0 THEN N$ = LEFT$(N$, INSTR(N$, " ") - 1)
     N$ = UCASE$(TRIM$(N$))
     
     Replace DSOP$, "%N:" + STR$(T%) + "%", N$
    LOOP

    'PRINT DS$, "FN_%F" + TempData$(PD%) + "F%" + DSOP$, L$, R$

    IF DS% = 1 THEN
     PROG$(CD%) = ";FNSTART," + "%F" + TempData$(PD%) + "F%"
     AddLine "FN_%F" + TempData$(PD%) + "F%" + DSOP$, CD% + 1
     AddLine DS$, CD% + 2
     CD% = CD% + 1 'was 1
     GOTO FindFunctionsAgain
    END IF

    IF DS% = 2 THEN
     'SUBCODE$(CD%) = DS$
     
     'SUBCODE$(CD%) = ";FNSTART," + "%F" + TempData$(PD%) + "F%"
     'AddSubLine "FN_%F" + TempData$(PD%) + "F%" + DSOP$, CD% + 1
     'AddSubLine DS$, CD% + 2
     
     AddSubLine DS$, CD%
     AddSubLine "FN_%F" + TempData$(PD%) + "F%" + DSOP$, CD%
     AddSubLine ";FNSTART," + "%F" + TempData$(PD%) + "F%", CD%
     SUBCODE$(CD% + 3) = ""
     
     'PRINT SUBCODE$(CD%), SUBCODE$(CD% + 1)
     'CD% = CD% + 1
     GOTO FindFunctionsAgain
    END IF
   
   END IF

   DO WHILE INSTR(DS$, "%N:") <> 0
    T% = VAL(MID$(DS$, INSTR(DS$, "%N:") + 3))
    N$ = TRIM$(MID$(SUBDATA$(VAL(TempData$(T%)), 1), 4))
    IF INSTR(N$, "(") <> 0 THEN Replace N$, "(", " "
    IF INSTR(N$, " ") <> 0 THEN N$ = LEFT$(N$, INSTR(N$, " ") - 1)
    N$ = UCASE$(TRIM$(N$))
    
    Replace DS$, "%N:" + STR$(T%) + "%", N$     
   LOOP

  NEXT PD%
  
 IF DS% = 1 AND CD% < APC% THEN GOTO FindFunctions
 IF DS% = 2 AND CD% < SDC% THEN GOTO FindFunctions
NEXT DS%

'Replace %F FN No. F% with function name
FOR DS% = 1 TO 2
 CD% = 0
CallFunctions:
  CD% = CD% + 1

  IF DS% = 1 THEN DS$ = PROG$(CD%)
  IF DS% = 2 THEN DS$ = SUBCODE$(CD%)
  IF INSTR(DS$, "%F") <> 0 AND INSTR(DS$, "F%") <> 0 THEN
   T% = VAL(MID$(DS$, INSTR(DS$, "%F") + 2))
   N$ = MID$(SUBDATA$(T%, 1), 4)
   IF INSTR(N$, "(") <> 0 THEN N$ = LEFT$(N$, INSTR(N$, "(") - 1)
   R$ = MID$(DS$, INSTR(DS$, "%F"))
   R$ = LEFT$(R$, INSTR(R$, "F%") + 1)
   Replace DS$, R$, N$
   IF DS% = 1 THEN PROG$(CD%) = DS$
   IF DS% = 2 THEN SUBCODE$(CD%) = DS$
   CD% = CD% - 1
  END IF
  
 IF DS% = 1 AND CD% < APC% THEN GOTO CallFunctions
 IF DS% = 2 AND CD% < SDC% THEN GOTO CallFunctions
NEXT DS%
 
END SUB

REM $STATIC
SUB CalcConfig

DO WHILE INSTR(CONFIG$, "&") <> 0: Replace CONFIG$, "&", ",": LOOP

If ChipFamily% = 16 then
 'Check for MCLR, LVP, WDT, OSC/FOSC
 WDT% = 0: LVP% = 0: MCLR% = 0: OSC% = 0
 FOR PD% = 1 TO COC%
  IF INSTR(ConfigOp$(PD%), "WDT") <> 0 THEN WDT% = 1
  IF INSTR(ConfigOp$(PD%), "LVP") <> 0 THEN LVP% = 1
  IF INSTR(ConfigOp$(PD%), "MCLR") <> 0 THEN MCLR% = 1
  IF INSTR(ConfigOp$(PD%), "MCLRE") <> 0 THEN MCLR% = 2
  IF INSTR(ConfigOp$(PD%), "OSC") <> 0 AND OSC% <> 2 THEN OSC% = 1
  IF INSTR(ConfigOp$(PD%), "FOSC") <> 0 THEN OSC% = 2
 NEXT PD%
 
 IF WDT% = 1 AND INSTR(CONFIG$, "WDT") = 0 THEN CONFIG$ = CONFIG$ + ", WDT=OFF"
 IF LVP% = 1 AND INSTR(CONFIG$, "LVP") = 0 THEN CONFIG$ = CONFIG$ + ", LVP=OFF"
 IF MCLR% = 1 AND INSTR(CONFIG$, "MCLR") = 0 THEN CONFIG$ = CONFIG$ + ", MCLR=OFF"
 IF MCLR% = 2 AND INSTR(CONFIG$, "MCLRE") = 0 THEN CONFIG$ = CONFIG$ + ", MCLRE=OFF"
 
 IF OSC% = 1 AND INSTR(CONFIG$, "OSC") = 0 THEN CONFIG$ = CONFIG$ + ", OSC=HS"
 IF OSC% = 1 AND INSTR(CONFIG$, "FOSC") <> 0 THEN Replace CONFIG$, "FOSC", "OSC"
 IF OSC% = 2 AND INSTR(CONFIG$, "FOSC") = 0 THEN CONFIG$ = CONFIG$ + ", FOSC=HS"
 IF OSC% = 2 AND WholeINSTR(CONFIG$, "OSC") = 2 THEN Replace CONFIG$, "OSC", "FOSC"
  
 IF LEFT$(CONFIG$, 1) = "," THEN CONFIG$ = TRIM$(MID$(CONFIG$, 2))
 exit sub
END IF

'Add #OSC to CONFIG
IF OSC$ <> "" THEN
 IF INSTR(OSC$, "OSC") = 0 AND INSTR(OSC$, "RC") = 0 THEN OSC$ = OSC$ + "_OSC"
 IF CONFIG$ <> "" THEN T$ = ","
 CONFIG$ = CONFIG$ + T$ + OSC$
END IF

'Check that an oscillator has been selected
IF INSTR(CONFIG$, "OSC") = 0 AND INSTR(CONFIG$, "EXTRC") = 0 AND INSTR(CONFIG$, "INTRC") = 0 THEN 
 'Check to see what options are valid
 HS% = 0
 XT% = 0
 IntRC% = 0
 IntOSC% = 0
 FOR PD% = 1 to COC%
  IF INSTR(UCASE$(ConfigOp$(PD%)), "HS_OSC") <> 0 THEN HS% = 1
  IF INSTR(UCASE$(ConfigOp$(PD%)), "XT_OSC") <> 0 THEN XT% = 1
  IF INSTR(UCASE$(ConfigOp$(PD%)), "INTRC_OSC") <> 0 THEN IntRC% = 1
  IF INSTR(UCASE$(ConfigOp$(PD%)), "INTRC_OSC_NOCLKOUT") <> 0 THEN IntOsc% = 1
 NEXT
 
 'Use options in order of availability
 T$ = ""
 IF HS% = 1 AND ChipMhz% > 4 THEN
  IF CONFIG$ <> "" THEN T$ = ","
  CONFIG$ = CONFIG$ + T$ + "HS_OSC"
  GOTO OscFound
 END IF
 IF XT% = 1 THEN
  IF CONFIG$ <> "" THEN T$ = ","
  CONFIG$ = CONFIG$ + T$ + "XT_OSC"
  GOTO OscFound
 END IF
 IF IntOsc% = 1 THEN 
  IF CONFIG$ <> "" THEN T$ = ","
  CONFIG$ = CONFIG$ + T$ + "INTRC_OSC_NOCLKOUT"
  GOTO OscFound
 END IF
 IF IntRC% = 1 THEN 
  IF CONFIG$ <> "" THEN T$ = ","
  CONFIG$ = CONFIG$ + T$ + "INTRC_OSC"
  GOTO OscFound
 END IF
 
 OscFound:
 
END IF

'Check to see if WDT, LVP or MCLR need to be configured
WDT% = 0: LVP% = 0: MCLR% = 0: MCLRE% = 0
FOR PD% = 1 TO COC%
 IF INSTR(ConfigOp$(PD%), "WDT_") <> 0 THEN WDT% = 1
 IF INSTR(ConfigOp$(PD%), "LVP_") <> 0 THEN LVP% = 1
 IF INSTR(ConfigOp$(PD%), "MCLR_") <> 0 THEN MCLR% = 1
 IF INSTR(ConfigOp$(PD%), "MCLRE_") <> 0 THEN MCLRE% = 1
NEXT PD%

'Set defaults for WDT, LVP and MCLR
IF INSTR(CONFIG$, "WDT") = 0 AND WDT% = 1 THEN
 IF CONFIG$ <> "" THEN T$ = ","
 CONFIG$ = CONFIG$ + T$ + "WDT_OFF"
END IF
T$ = LEFT$(ChipName$, 2)

IF INSTR(CONFIG$, "LVP") = 0 AND LVP% = 1 THEN
 IF CONFIG$ <> "" THEN T$ = ","
 CONFIG$ = CONFIG$ + T$ + "LVP_OFF"
END IF

IF INSTR(CONFIG$, "MCLR") = 0 AND MCLR% = 1 THEN
 IF CONFIG$ <> "" THEN T$ = ","
 CONFIG$ = CONFIG$ + T$ + "MCLR_OFF"
END IF

IF INSTR(CONFIG$, "MCLRE") = 0 AND MCLRE% = 1 THEN
 IF CONFIG$ <> "" THEN T$ = ","
 CONFIG$ = CONFIG$ + T$ + "MCLRE_OFF"
END IF

 'Split up CONFIG line
 DS$ = CONFIG$
 T% = 0
 DO WHILE INSTR(DS$, ",") <> 0
  T% = T% + 1
  TempData$(T%) = TRIM$(LEFT$(DS$, INSTR(DS$, ",") - 1))
  DS$ = MID$(DS$, INSTR(DS$, ",") + 1)
 LOOP
 T% = T% + 1
 TempData$(T%) = TRIM$(DS$)
 
 'Remove spaces
 FOR CD% = 1 to T%
  DO WHILE INSTR(TempData$(CD%), " ") <> 0: Replace TempData$(CD%), " ", "": LOOP
 NEXT
  
 'Change from x=y to X_Y style
 FOR CD% = 1 TO T%
  DS$ = TempData$(CD%)
  IF INSTR(DS$, "=") <> 0 THEN
   DO WHILE INSTR(DS$, " ") <> 0: Replace DS$, " ", "": LOOP
   N$ = UCASE$(TRIM$(LEFT$(DS$, INSTR(DS$, "=") - 1)))
   V$ = UCASE$(TRIM$(MID$(DS$, INSTR(DS$, "=") + 1)))
   FOR PD% = 1 TO COC%
    IF N$ <> "OSC" AND V$ <> "INT" AND INSTR(UCASE$(ConfigOp$(PD%)), N$) <> 0 AND INSTR(UCASE$(ConfigOp$(PD%)), V$) <> 0 THEN TempData$(CD%) = TRIM$(ConfigOp$(PD%)): EXIT FOR
   NEXT PD%
  END IF  
 NEXT
 
 'Check and combine
 CONFIG$ = ""
 CONFIG2$ = ""
 FOR CD% = 1 TO T%
  DS$ = TRIM$(TempData$(CD%))
  IF LEFT$(DS$, 1) = "_" THEN DS$ = MID$(DS$, 2)

  OV% = 0
  FOR CL% = 1 to COC%
   IF INSTR(UCASE$(ConfigOp$(CL%)), UCASE$(TRIM$(DS$))) <> 0 THEN
    CW% = ConfigLoc%(CL%)
    IF CW% = 1 THEN CONFIG$ = CONFIG$ + " & _" + TRIM$(ConfigOp$(CL%))
    IF CW% = 2 THEN CONFIG2$ = CONFIG2$ + " & _" + TRIM$(ConfigOp$(CL%))
    OV% = 1
    EXIT FOR
   END IF
  NEXT

  IF OV% = 0 AND INSTR(UCASE$(TempData$(CD%)), "OSC=") THEN
   
   DS$ = UCASE$(TempData$(CD%))
   DS$ = TRIM$(MID$(DS$, INSTR(DS$, "=") + 1))
   OT% = 0
   IF INSTR(DS$, "INT") <> 0 THEN OT% = 1
   IF INSTR(DS$, "EXT") <> 0 THEN OT% = 2
   
   FOR CL% = 1 to COC%
   
    SV% = 0
   
    IF OT% = 1 THEN
     DT$ = UCASE$(ConfigOp$(CL%))
     IF INSTR(DT$, "INT") <> 0 AND INSTR(DT$, "IO") <> 0 THEN SV% = 1
     IF INSTR(DT$, "INT") <> 0 AND INSTR(DT$, "NO") <> 0 THEN SV% = 1
    END IF
    IF OT% = 2 THEN
     DT$ = UCASE$(ConfigOp$(CL%))
     IF INSTR(DT$, "EXT") <> 0 AND INSTR(DT$, "IO") <> 0 THEN SV% = 1
     IF INSTR(DT$, "EXT") <> 0 AND INSTR(DT$, "NO") <> 0 THEN SV% = 1
    END IF
   
    IF SV% = 1 THEN
     CW% = ConfigLoc%(CL%)
     IF CW% = 1 THEN CONFIG$ = CONFIG$ + " & _" + TRIM$(ConfigOp$(CL%))
     IF CW% = 2 THEN CONFIG2$ = CONFIG2$ + " & _" + TRIM$(ConfigOp$(CL%))
     OV% = 1
     EXIT FOR
    END IF
   NEXT
   
  END IF
  
  IF OV% = 0 THEN 
   ERC% = ERC% + 1
   T$ = Message$("BadConfig")
   Replace T$, "%option%", TempData$(CD%)
   gcERR$(ERC%) = T$
  END IF
 NEXT
 
 IF CONFIG$ <> "" THEN CONFIG$ = MID$(CONFIG$, 4)
 IF CONFIG2$ <> "" THEN CONFIG2$ = MID$(CONFIG2$, 4)
 
' 'Recombine
' CONFIG$ = ""
' CONFIG2$ = ""
' FOR CD% = 1 TO T%
'  T$ = "_": IF CD% > 1 THEN T$ = " & _"
'  CONFIG$ = CONFIG$ + T$ + TempData$(CD%)
' NEXT

END SUB

SUB Calculate (SUM$)

SUM$ = UCASE$(SUM$)

REM Replace AND, OR, NOT, XOR with symbols
REPLOGIC:
IF INSTR(SUM$, "AND") <> 0 THEN Replace SUM$, "AND", "&": GOTO REPLOGIC
IF INSTR(SUM$, "OR") <> 0 THEN Replace SUM$, "OR", "|": GOTO REPLOGIC
IF INSTR(SUM$, "XOR") <> 0 THEN Replace SUM$, "XOR", "#": GOTO REPLOGIC

BEGINCALC:
REM Remove all Spaces
Replace SUM$, " ", ""
IF INSTR(SUM$, " ") <> 0 THEN GOTO BEGINCALC
SUM$ = " " + LCASE$(SUM$)
IF INSTR(SUM$, "%") <> 0 THEN GOTO PERCDIFF

REM Calculate Sine, Cosine, Tangent, Arctangent and Pi
PISWAP:
IF INSTR(SUM$, "pi") <> 0 THEN
 Replace SUM$, "pi", "3.14159"
 GOTO PISWAP
END IF

INSMULT:
IF INSTR(SUM$, ")(") <> 0 THEN
 Replace SUM$, ")(", ")*("
 GOTO INSMULT
END IF

FILE$(1) = "cos("
FILE$(2) = "sin("
FILE$(3) = "tan("
FILE$(4) = "atn("
FILE$(5) = "sqr("
FILE$(6) = "abs("
FILE$(7) = "sgn("
FILE$(8) = "int("
FILE$(9) = "log("

FOR CA% = 1 TO 9
CALCLOOP:
 IF INSTR(LCASE$(SUM$), FILE$(CA%)) <> 0 THEN

  CT$ = ""
  PD% = INSTR(LCASE$(SUM$), FILE$(CA%)) + 4
  T% = 1
GETFUNCTION:
   CT$ = CT$ + MID$(SUM$, PD%, 1)
   IF MID$(SUM$, PD%, 1) = "(" THEN T% = T% + 1
   IF MID$(SUM$, PD%, 1) = ")" THEN T% = T% - 1
   PD% = PD% + 1
  IF T% > 0 THEN GOTO GETFUNCTION
  CT$ = LEFT$(CT$, LEN(CT$) - 1)

  OSS$ = FILE$(CA%) + CT$ + ")"
  CLDO$ = CLD$
  CLD$ = "no"
  Calculate CT$
  CLD$ = CLDO$

  IF CA% = 1 THEN CT$ = STR$(COS(VAL(CT$)))
  IF CA% = 2 THEN CT$ = STR$(SIN(VAL(CT$)))
  IF CA% = 3 THEN CT$ = STR$(TAN(VAL(CT$)))
  IF CA% = 4 THEN CT$ = STR$(ATN(VAL(CT$)))
  IF CA% = 5 THEN CT$ = STR$(SQR(VAL(CT$)))
  IF CA% = 6 THEN CT$ = STR$(ABS(VAL(CT$)))
  IF CA% = 7 THEN CT$ = STR$(SGN(VAL(CT$)))
  IF CA% = 8 THEN CT$ = STR$(INT(VAL(CT$)))
  IF CA% = 9 THEN CT$ = STR$(LOG(VAL(CT$)))
  SCICONV CT$
  IF LEFT$(CT$, 1) = " " THEN CT$ = MID$(CT$, 2)
  Replace SUM$, OSS$, CT$
  'IF CLD$ = "" THEN PRINT SUM$
  GOTO CALCLOOP
 END IF
NEXT CA%

REM Solve Brackets using recursion
BRACKETS:
IF INSTR(SUM$, "(") = 0 AND INSTR(SUM$, ")") = 0 THEN GOTO ENDBRACKET
TS$ = ""
PD% = INSTR(SUM$, "(")
T% = 0
GETBRACKET:
 TS$ = TS$ + MID$(SUM$, PD%, 1)
 IF MID$(SUM$, PD%, 1) = "(" THEN T% = T% + 1
 IF MID$(SUM$, PD%, 1) = ")" THEN T% = T% - 1
 PD% = PD% + 1
IF T% > 0 THEN GOTO GETBRACKET
OSS$ = TS$
Replace TS$, "(", "": Replace TS$, ")", ""

CLDO$ = CLD$
CLD$ = "no"
Calculate TS$
SCICONV TS$
CLD$ = CLDO$
DO WHILE INSTR(TS$, " ") <> 0: Replace TS$, " ", "": LOOP
Replace SUM$, OSS$, TS$
GOTO BRACKETS
ENDBRACKET:

REM Find Exponentiation
EXPONENT:
IF INSTR(SUM$, "^") = 0 THEN GOTO ENDEXPONENT
FD% = 0
FINDEXP:
FD% = FD% + 1
T$ = MID$(SUM$, FD%, 1)
IF T$ <> "^" THEN GOTO FINDEXP
ACT$ = T$: AP = FD%

FOR FS% = AP - 1 TO 1 STEP -1
 T$ = MID$(SUM$, FS%, 1)
 IF (T$ <> RIGHT$(STR$(VAL(T$)), 1) AND T$ <> "." AND T$ <> "-" AND T$ <> "E") OR T$ = " " THEN FS% = FS% + 1: EXIT FOR
 M$ = MID$(SUM$, FS% - 1, 1)
 IF T$ = "-" AND (M$ <> "^" AND M$ <> "/" AND M$ <> "*" AND M$ <> "-" AND M$ <> "+" AND M$ <> "&" AND M$ <> "#" AND M$ <> "|" AND M$ <> "!") THEN EXIT FOR
NEXT FS%

N1$ = MID$(SUM$, FS%, AP - FS%)
N1 = VAL(N1$)
N2$ = MID$(SUM$, AP + 1)

IF INSTR(N2$, "^") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "^") - 1)
IF INSTR(N2$, "/") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "/") - 1)
IF INSTR(N2$, "*") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "*") - 1)
IF INSTR(2, N2$, "-") <> 0 THEN N2$ = LEFT$(N2$, INSTR(2, N2$, "-") - 1)
IF INSTR(N2$, "+") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "+") - 1)

N2 = VAL(N2$)
OS$ = N1$ + ACT$ + N2$
DO WHILE INSTR(OS$, " ") <> 0
Replace OS$, " ", ""
LOOP

IF N1 = 0 AND N2 = 0 THEN Replace SUM$, OS$, "0": GOTO EXPONENT

ANS$ = ""
'IF CLD$ = "" THEN PRINT SUM$

RES = N1 ^ N2

ANS$ = STR$(RES)
IF LEFT$(ANS$, 1) = " " THEN ANS$ = MID$(ANS$, 2)
SCICONV ANS$
IF LEFT$(ANS$, 1) = " " THEN ANS$ = MID$(ANS$, 2)
Replace SUM$, OS$, ANS$
GOTO EXPONENT
ENDEXPONENT:

REM Find Division and Multiplication
DIVMULT:
IF INSTR(SUM$, "/") = 0 AND INSTR(SUM$, "*") = 0 AND INSTR(SUM$, "x") = 0 THEN GOTO ENDDIVMULT
FD% = 0
FINDDIV:
FD% = FD% + 1
T$ = MID$(SUM$, FD%, 1)
IF T$ <> "/" AND T$ <> "*" AND T$ <> "x" THEN GOTO FINDDIV
ACT$ = T$: AP = FD%

FOR FS% = AP - 1 TO 1 STEP -1
 T$ = MID$(SUM$, FS%, 1)
 IF (T$ <> STR$(VAL(T$)) AND T$ <> ".") OR T$ = " " THEN FS% = FS% + 1: EXIT FOR
 M$ = MID$(SUM$, FS% - 1, 1)
 IF T$ = "-" AND (M$ <> LTRIM$(STR$(VAL(M$))) AND M$ <> "^" AND M$ <> "/" AND M$ <> "*" AND M$ <> "-" AND M$ <> "+" AND M$ <> "&" AND M$ <> "#" AND M$ <> "|" AND M$ <> "!") THEN FS% = FS% - 1: EXIT FOR
NEXT FS%

N1$ = MID$(SUM$, FS%, AP - FS%)
N1! = VAL(N1$)
N2$ = MID$(SUM$, AP + 1)

IF INSTR(N2$, "/") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "/") - 1)
IF INSTR(N2$, "*") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "*") - 1)
IF INSTR(2, N2$, "-") <> 0 THEN N2$ = LEFT$(N2$, INSTR(2, N2$, "-") - 1)
IF INSTR(N2$, "+") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "+") - 1)

N2! = VAL(N2$)
OS$ = N1$ + ACT$ + N2$
DO WHILE INSTR(OS$, " ") <> 0
Replace OS$, " ", ""
LOOP
'IF CLD$ = "" THEN PRINT SUM$

RES! = 0
IF ACT$ = "/" AND N2! <> 0 THEN RES! = N1! / N2!
IF ACT$ = "*" THEN RES! = N1! * N2!
ANS$ = STR$(RES!)
SCICONV ANS$
DO WHILE INSTR(ANS$, " ") <> 0: Replace ANS$, " ", "": LOOP
Replace SUM$, OS$, ANS$
GOTO DIVMULT
ENDDIVMULT:

REM Replace all occurances of "--" with "+"
REMMINUS:
IF INSTR(SUM$, "--") <> 0 THEN
 Replace SUM$, "--", "+"
 GOTO REMMINUS
END IF
IF INSTR(SUM$, "+-") <> 0 THEN
 Replace SUM$, "+-", "-"
 GOTO REMMINUS
END IF

REM Addition and Subtraction
FD% = 0
ADDSUB:
IF INSTR(SUM$, "+") = 0 AND INSTR(FD% + 1, SUM$, "-") = 0 THEN GOTO ENDADDSUB
FINDADD:
 FD% = FD% + 1
 T$ = MID$(SUM$, FD%, 1)
IF T$ <> "+" AND T$ <> "-" THEN GOTO FINDADD

ACT$ = T$: AP = FD%
FOR FS% = AP - 1 TO 1 STEP -1
 T$ = MID$(SUM$, FS%, 1)
 IF (T$ <> RIGHT$(STR$(VAL(T$)), 1) AND T$ <> "." AND T$ <> "-" AND T$ <> "E") OR T$ = " " THEN FS% = FS% + 1: EXIT FOR
 M$ = MID$(SUM$, FS% - 1, 1)
 IF T$ = "-" AND (M$ <> "^" AND M$ <> "/" AND M$ <> "*" AND M$ <> "-" AND M$ <> "+" AND M$ <> "&" AND M$ <> "#" AND M$ <> "|" AND M$ <> "!") THEN FS% = FS% - 1: EXIT FOR
NEXT FS%

N1$ = MID$(SUM$, FS%, AP - FS%)
IF N1$ = " " OR N1$ = "" THEN GOTO ADDSUB
N1 = VAL(N1$)
N2$ = MID$(SUM$, AP + 1)

IF INSTR(2, N2$, "-") <> 0 THEN N2$ = LEFT$(N2$, INSTR(2, N2$, "-") - 1)
IF INSTR(N2$, "+") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "+") - 1)
IF INSTR(N2$, "&") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "&") - 1)
IF INSTR(N2$, "|") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "|") - 1)
IF INSTR(N2$, "#") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "#") - 1)

N2 = VAL(N2$)
OS$ = N1$ + ACT$ + N2$
DO WHILE INSTR(OS$, " ") <> 0
Replace OS$, " ", ""
LOOP
'IF CLD$ = "" THEN PRINT SUM$

IF ACT$ = "+" THEN ANS$ = STR$(N1 + N2)
IF ACT$ = "-" THEN ANS$ = STR$(N1 - N2)
IF LEFT$(ANS$, 1) = " " THEN ANS$ = MID$(ANS$, 2)
IF N1 = 0 AND N2 <> 0 AND ACT$ = "-" THEN FD% = FD% + 1: GOTO ADDSUB
SCICONV ANS$
Replace SUM$, OS$, ANS$
FD% = FD% + LEN(OS$) - LEN(ANS$) + 1
FD% = 0

MINUS:
IF INSTR(SUM$, "--") <> 0 THEN
 Replace SUM$, "--", "+"
 GOTO MINUS
END IF
GOTO ADDSUB
ENDADDSUB:

REM Find Logical operation results (AND, OR, XOR)
LOGICALOP:
IF INSTR(SUM$, "&") = 0 AND INSTR(SUM$, "|") = 0 AND INSTR(SUM$, "#") = 0 THEN GOTO ENDLOGICALOP
FD% = 0
FINDLOGIC:
FD% = FD% + 1
T$ = MID$(SUM$, FD%, 1)
IF T$ <> "&" AND T$ <> "|" AND T$ <> "#" THEN GOTO FINDLOGIC
ACT$ = T$: AP = FD%

FOR FS% = AP - 1 TO 1 STEP -1
 T$ = MID$(SUM$, FS%, 1)
 IF (T$ <> STR$(VAL(T$)) AND T$ <> ".") OR T$ = " " THEN FS% = FS% + 1: EXIT FOR
 M$ = MID$(SUM$, FS% - 1, 1)
 IF T$ = "-" AND (M$ <> LTRIM$(STR$(VAL(M$))) AND M$ <> "^" AND M$ <> "/" AND M$ <> "*" AND M$ <> "-" AND M$ <> "+" AND M$ <> "&" AND M$ <> "#" AND M$ <> "|" AND M$ <> "!") THEN FS% = FS% - 1: EXIT FOR
NEXT FS%

N1$ = MID$(SUM$, FS%, AP - FS%)
N1 = VAL(N1$)
N2$ = MID$(SUM$, AP + 1)

IF INSTR(N2$, "/") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "/") - 1)
IF INSTR(N2$, "*") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "*") - 1)
IF INSTR(2, N2$, "-") <> 0 THEN N2$ = LEFT$(N2$, INSTR(2, N2$, "-") - 1)
IF INSTR(N2$, "+") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "+") - 1)
IF INSTR(N2$, "&") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "&") - 1)
IF INSTR(N2$, "|") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "|") - 1)
IF INSTR(N2$, "#") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "#") - 1)

N2 = VAL(N2$)
OS$ = N1$ + ACT$ + N2$
DO WHILE INSTR(OS$, " ") <> 0
Replace OS$, " ", ""
LOOP
'IF CLD$ = "" THEN PRINT SUM$

RES = 0
IF ACT$ = "&" AND N2 <> 0 THEN RES = N1 AND N2
IF ACT$ = "|" THEN RES = N1 OR N2
IF ACT$ = "#" THEN RES = N1 XOR N2
ANS$ = STR$(RES)
SCICONV ANS$
DO WHILE INSTR(ANS$, " ") <> 0: Replace ANS$, " ", "": LOOP
Replace SUM$, OS$, ANS$
GOTO LOGICALOP
ENDLOGICALOP:

REM Test Sum
TESTSUM:
IF INSTR(SUM$, "=") = 0 AND INSTR(SUM$, "<") = 0 AND INSTR(SUM$, ">") = 0 THEN GOTO ENDTESTSUM
FD% = 0
FINDTEST:
FD% = FD% + 1
T$ = MID$(SUM$, FD%, 1)
IF T$ <> "=" AND T$ <> "<" AND T$ <> ">" THEN GOTO FINDTEST
ACT$ = T$: AP = FD%

FOR FS% = AP - 1 TO 1 STEP -1
T$ = MID$(SUM$, FS%, 1)
IF (T$ <> STR$(VAL(T$)) AND T$ <> "." AND T$ <> "-" AND T$ <> "E") OR T$ = " " THEN FS% = FS% + 1: EXIT FOR
M$ = MID$(SUM$, FS% - 1, 1)
IF T$ = "-" AND (M$ <> "^" AND M$ <> "/" AND M$ <> "*" AND M$ <> "-" AND M$ <> "+") THEN FS% = FS% - 1: EXIT FOR
NEXT FS%

N1$ = MID$(SUM$, FS%, AP - FS%)
N1 = VAL(N1$)
N2$ = MID$(SUM$, AP + 1)

IF INSTR(N2$, "=") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "=") - 1)
IF INSTR(N2$, "<") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "<") - 1)
IF INSTR(N2$, ">") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, ">") - 1)
IF INSTR(2, N2$, "-") <> 0 THEN N2$ = LEFT$(N2$, INSTR(2, N2$, "-") - 1)
IF INSTR(N2$, "+") <> 0 THEN N2$ = LEFT$(N2$, INSTR(N2$, "+") - 1)

N2 = VAL(N2$)
OS$ = N1$ + ACT$ + N2$
DO WHILE INSTR(OS$, " ") <> 0
Replace OS$, " ", ""
LOOP
'IF CLD$ = "" THEN PRINT SUM$

IF ACT$ = "=" THEN RES = (N1 = N2) * -1
IF ACT$ = "<" THEN RES = (N1 < N2) * -1
IF ACT$ = ">" THEN RES = (N1 > N2) * -1
ANS$ = STR$(RES)
SCICONV ANS$
IF LEFT$(ANS$, 1) = " " THEN ANS$ = RIGHT$(ANS$, LEN(ANS$) - 1)
Replace SUM$, OS$, ANS$
GOTO TESTSUM
ENDTESTSUM:

REM End of normal calculate
SOL = VAL(SUM$)
EXIT SUB

REM Percentage Section
PERCDIFF:
FD% = 0
FINDSIGN:
FD% = FD% + 1
T$ = MID$(SUM$, FD%, 1)
IF T$ <> "+" AND T$ <> "." AND T$ <> "-" AND T$ <> "x" AND T$ <> "*" AND T$ <> "/" THEN GOTO FINDSIGN
ACT$ = T$: AP = FD%

FOR FS% = AP - 1 TO 1 STEP -1
T$ = MID$(SUM$, FS%, 1)
IF T$ <> RIGHT$(STR$(VAL(T$)), 1) OR T$ = " " AND T$ <> "." THEN EXIT FOR
NEXT FS%

N1 = VAL(MID$(SUM$, FS%, AP - FS%))
N2 = VAL(RIGHT$(SUM$, LEN(SUM$) - AP))
ACT2$ = RIGHT$(SUM$, 1)

IF ACT$ = "+" THEN SOL = (N2 + N1) / N2 * 100
IF ACT$ = "-" THEN SOL = (N2 - N1) / N2 * 100
IF ACT$ = "/" THEN SOL = N1 / N2 * 100
IF ACT$ = "x" OR ACT$ = "*" THEN
 SOL = N1 * (N2 / 100)
 IF ACT2$ = "+" THEN SOL = SOL + N1
 IF ACT2$ = "-" THEN SOL = SOL - N1
END IF
SOL = SOL * SGN(SOL)
SUM$ = STR$(SOL)
END SUB

FUNCTION CheckLine (DS$)

CheckLine = 0

'Check that the line is a known command, and that it has all necessary parameters

'Tidy assembly instructions
T% = 49
TempData$(1) = "ADDWF"
TempData$(2) = "ANDWF"
TempData$(3) = "CLRF"
TempData$(4) = "CLRW"
TempData$(5) = "COMF"
TempData$(6) = "DECF"
TempData$(7) = "DECFSZ"
TempData$(8) = "INCF"
TempData$(9) = "INCFSZ"
TempData$(10) = "IORWF"
TempData$(11) = "MOVF"
TempData$(12) = "MOVWF"
TempData$(13) = "NOP"
TempData$(14) = "RLF"
TempData$(15) = "RRF"
TempData$(16) = "SUBWF"
TempData$(17) = "SWAPF"
TempData$(18) = "XORWF"
TempData$(19) = "BCF"
TempData$(20) = "BSF"
TempData$(21) = "BTFSC"
TempData$(22) = "BTFSS"
TempData$(23) = "ADDLW"
TempData$(24) = "ANDLW"
TempData$(25) = "CALL"
TempData$(26) = "CLRWDT"
TempData$(27) = "IORLW"
TempData$(28) = "MOVLW"
TempData$(29) = "RETFIE"
TempData$(30) = "RETLW"
TempData$(31) = "RETURN"
TempData$(32) = "SLEEP"
TempData$(33) = "SUBLW"
TempData$(34) = "XORLW"
TempData$(35) = "MOVFW"
TempData$(36) = "TRIS"
TempData$(37) = "OPTION"
TempData$(38) = "MULWF"
TempData$(39) = "MULLW"
TempData$(40) = "MOVLB"
TempData$(41) = "MOVFF"
TempData$(42) = "RLCF"
TempData$(43) = "RRCF"
TempData$(44) = "RLNCF"
TempData$(45) = "RRNCF"
TempData$(46) = "CPFSEQ"
TempData$(47) = "CPFSGT"
TempData$(48) = "CPFSLT"
TempData$(49) = "SETF"

FOR PD% = 1 TO T%
 IF LEFT$(DS$, LEN(TempData$(PD%)) + 1) = TempData$(PD%) + " " THEN
  DS$ = LCASE$(LEFT$(DS$, LEN(TempData$(PD%)))) + MID$(DS$, LEN(TempData$(PD%)) + 1)
  DS$ = " " + DS$
 END IF
 IF DS$ = TempData$(PD%) THEN DS$ = " " + LCASE$(DS$)
NEXT


END FUNCTION

SUB CompileCalc (SUM$)

DO WHILE INSTR(SUM$, "--") <> 0: Replace SUM$, "--", "+": LOOP
DO WHILE INSTR(SUM$, "+-") <> 0: Replace SUM$, "+-", "-": LOOP

'Calculate brackets
CALCBRACKET:
 IF INSTR(SUM$, "(") <> 0 THEN

  T$ = MID$(SUM$, INSTR(SUM$, "("))
  BL% = 1
  FOR T% = 2 TO LEN(T$)
   IF MID$(T$, T%, 1) = "(" THEN BL% = BL% + 1
   IF MID$(T$, T%, 1) = ")" THEN BL% = BL% - 1
   IF BL% = 0 THEN T$ = LEFT$(T$, T%): EXIT FOR
  NEXT T%

  T2$ = MID$(T$, 2)
  T2$ = LEFT$(T2$, LEN(T2$) - 1)
  CompileCalc T2$
  Replace SUM$, T$, T2$
  GOTO CALCBRACKET
 END IF

 'Calculate *, /. Replace sum with variable containing answer
CALCMULTDIV:
 IF INSTR(SUM$, "*") <> 0 OR INSTR(SUM$, "/") <> 0 THEN
  'Read the sum
  FS% = 0
FINDMULT:
   FS% = FS% + 1
   ACT$ = MID$(SUM$, FS%, 1)
  IF ACT$ <> "*" AND ACT$ <> "/" THEN GOTO FINDMULT
  FV% = 0
  FOR T% = FS% - 1 TO 1 STEP -1
   T$ = MID$(SUM$, T%, 1)
   IF T$ = "{" OR T$ = "}" OR T$ = "=" OR T$ = "~" OR T$ = "<" OR T$ = ">" OR T$ = "*" OR T$ = "/" OR T$ = "+" OR T$ = "-" OR T$ = "&" OR T$ = "|" OR T$ = "#" OR T$ = "!" THEN FV% = T%: EXIT FOR
  NEXT T%
  V1$ = LEFT$(SUM$, INSTR(SUM$, ACT$) - 1): V1$ = MID$(V1$, FV% + 1)
  FOR T% = FS% + 1 TO LEN(SUM$)
   T$ = MID$(SUM$, T%, 1)
   IF T$ = "{" OR T$ = "}" OR T$ = "=" OR T$ = "~" OR T$ = "<" OR T$ = ">" OR T$ = "*" OR T$ = "/" OR T$ = "+" OR T$ = "-" OR T$ = "&" OR T$ = "|" OR T$ = "#" OR T$ = "!" THEN FV% = T%: EXIT FOR
   T$ = " "
  NEXT T%
  V2$ = MID$(SUM$, FS% + 1)
  IF INSTR(V2$, T$) < LEN(V2$) AND INSTR(V2$, T$) <> 0 THEN V2$ = LEFT$(V2$, INSTR(V2$, T$) - 1)
  V1O$ = V1$: V2O$ = V2$

 'Generate asm code for sum
  'Check if both are constants
  IF IsConst(V1$) AND IsConst(V2$) THEN
   IF ACT$ = "*" THEN AV$ = STR$(MakeDec(V1$) * MakeDec(V2$))
   IF ACT$ = "/" THEN AV$ = STR$(INT(MakeDec(V1$) / MakeDec(V2$)))
   GOTO MultDivAnswer
  END IF
  
  'Declare SysCalcTempA, SysCalcTempB and SysCalcTempX
  VLC% = VLC% + 1: VARLIST$(VLC%) = "SysCalcTempA"
  VLC% = VLC% + 1: VARLIST$(VLC%) = "SysCalcTempB"
  VLC% = VLC% + 1: VARLIST$(VLC%) = "SysCalcTempX"
  
  IF ACT$ = "/" THEN
   VLC% = VLC% + 1: VARLIST$(VLC%) = "SysDivLoop"
  END IF
  
  'Move V1 into SysCalcTempA
  IF ChipFamily% <> 16 OR IsConst$(V1$) THEN
   C$ = " movf " + GetByte$(V1$, 0) + ",W": IF IsConst(V1$) THEN C$ = " movlw " + GetByte$(V1$, 0)
   CSC% = CSC% + 1: TempData$(CSC%) = C$
   IF NOT IsConst(V1$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA"
  END IF
  IF ChipFamily% = 16 AND Not IsConst$(V1$) THEN
   VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
   CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V1$, 0) + ", SysCalcTempA"
  END IF

  'Move V2 into SysCalcTempB
  IF ChipFamily% <> 16 OR IsConst$(V2$) THEN
   C$ = " movf " + GetByte$(V2$, 0) + ",W": IF IsConst(V2$) THEN C$ = " movlw " + GetByte$(V2$, 0)
   CSC% = CSC% + 1: TempData$(CSC%) = C$
   IF NOT IsConst(V2$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB"
  END IF
  IF ChipFamily% = 16 AND Not IsConst$(V2$) THEN
   VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
   CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V2$, 0) + ", SysCalcTempA"
  END IF
 
  IF ThisCalc% = 1 THEN
   'Move V1 into SysCalcTempA
   'V1 is constant
   IF IsConst(V1$) THEN
    CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V1$, 1)
    CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA_H"
   END IF
   'V1 is word variable
   IF NOT IsConst(V1$) AND TypeOfVar$(V1$) = "WORD" THEN
    IF ChipFamily% <> 16 THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " movf " + GetByte$(V1$, 1) + ",W"
     VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
     CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA_H"
    END IF
    IF ChipFamily% = 16 THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V1$, 1) + ", SysCalcTempA_H"
     VLC% = VLC% + 1: VARLIST$(VLC%) = V1$     
    END IF
   END IF
   'V1 is byte variable
   IF NOT IsConst(V1$) AND TypeOfVar$(V1$) <> "WORD" THEN
    CSC% = CSC% + 1: TempData$(CSC%) = " clrf SysCalcTempA_H"
   END IF

   'Move V2 into SysCalcTempB
   'V2 is constant
   IF IsConst(V2$) THEN
    CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V2$, 1)
    CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB_H"
   END IF
   'V2 is word variable
   IF NOT IsConst(V2$) AND TypeOfVar$(V2$) = "WORD" THEN
    IF ChipFamily% <> 16 THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " movf " + GetByte$(V2$, 1) + ",W"
     VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
     CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB_H"
    END IF
    IF ChipFamily% = 16 THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V2$, 1) + ", SysCalcTempB_H"
     VLC% = VLC% + 1: VARLIST$(VLC%) = V1$     
    END IF    
    
   END IF
   'V2 is byte variable
   IF NOT IsConst(V2$) AND TypeOfVar$(V2$) <> "WORD" THEN
    CSC% = CSC% + 1: TempData$(CSC%) = " clrf SysCalcTempB_H"
   END IF

  END IF

  IF ACT$ = "*" THEN CSC% = CSC% + 1: TempData$(CSC%) = " call SysMultSub"
  IF ACT$ = "/" THEN CSC% = CSC% + 1: TempData$(CSC%) = " call SysDivSub"
  IF ThisCalc% = 1 THEN TempData$(CSC%) = TempData$(CSC%) + "16"

  'Write answer
  CV% = CV% + 1
  AV$ = "SysTemp" + STR$(CV%)
  IF ThisCalc% = 1 THEN
   IF ChipFamily% <> 16 THEN
    IF ACT$ = "*" THEN CSC% = CSC% + 1: TempData$(CSC%) = " movf SysCalcTempX_H,W"
    IF ACT$ = "/" THEN CSC% = CSC% + 1: TempData$(CSC%) = " movf SysCalcTempA_H,W"
    CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + AV$ + "_H"
   END IF
   IF ChipFamily% = 16 THEN
    IF ACT$ = "*" THEN CSC% = CSC% + 1: TempData$(CSC%) = " movff SysCalcTempX_H, " + AV$ + "_H"
    IF ACT$ = "/" THEN CSC% = CSC% + 1: TempData$(CSC%) = " movff SysCalcTempA_H, " + AV$ + "_H"
   END IF
   VLC% = VLC% + 1: VARLIST$(VLC%) = AV$ + "_H"
  END IF
    
  IF ChipFamily% <> 16 THEN
   IF ACT$ = "*" THEN CSC% = CSC% + 1: TempData$(CSC%) = " movf SysCalcTempX,W"
   IF ACT$ = "/" THEN CSC% = CSC% + 1: TempData$(CSC%) = " movf SysCalcTempA,W"
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + AV$
  END IF
  IF ChipFamily% = 16 THEN
   IF ACT$ = "*" THEN CSC% = CSC% + 1: TempData$(CSC%) = " movff SysCalcTempX, " + AV$
   IF ACT$ = "/" THEN CSC% = CSC% + 1: TempData$(CSC%) = " movff SysCalcTempA, " + AV$
  END IF
  VLC% = VLC% + 1: VARLIST$(VLC%) = AV$
  IF ThisCalc% = 1 THEN
   VarType$(VLC%) = "WORD"
   SortVarList
  END IF

  'Replace sum with answer variable
MultDivAnswer:
  Replace SUM$, V1O$ + ACT$ + V2O$, AV$
  GOTO CALCMULTDIV
 END IF

 'Calculate +, -. Replace sum with variable containing answer
CALCADDSUB:
 IF INSTR(SUM$, "+") <> 0 OR INSTR(SUM$, "-") <> 0 THEN
  'Read the sum
  FS% = 0
FINDADDSUB:
   FS% = FS% + 1
   ACT$ = MID$(SUM$, FS%, 1)
  IF ACT$ <> "+" AND ACT$ <> "-" THEN GOTO FINDADDSUB
  FV% = 0
  FOR T% = FS% - 1 TO 1 STEP -1
   T$ = MID$(SUM$, T%, 1)
   IF T$ = "{" OR T$ = "}" OR T$ = "=" OR T$ = "~" OR T$ = "<" OR T$ = ">" OR T$ = "+" OR T$ = "-" OR T$ = "&" OR T$ = "|" OR T$ = "#" OR T$ = "!" THEN FV% = T%: EXIT FOR
  NEXT T%
  V1$ = LEFT$(SUM$, INSTR(SUM$, ACT$) - 1): V1$ = MID$(V1$, FV% + 1)
  FOR T% = FS% + 1 TO LEN(SUM$)
   T$ = MID$(SUM$, T%, 1)
   IF T$ = "{" OR T$ = "}" OR T$ = "=" OR T$ = "~" OR T$ = "<" OR T$ = ">" OR T$ = "+" OR T$ = "-" OR T$ = "&" OR T$ = "|" OR T$ = "#" OR T$ = "!" THEN FV% = T%: EXIT FOR
   T$ = " "
  NEXT T%
  V2$ = MID$(SUM$, FS% + 1)
  IF INSTR(V2$, T$) < LEN(V2$) AND INSTR(V2$, T$) <> 0 THEN V2$ = LEFT$(V2$, INSTR(V2$, T$) - 1)
  V1O$ = V1$: V2O$ = V2$
  
  'Generate asm code for sum
  'Check if both are constants
  IF IsConst(V1$) AND IsConst(V2$) THEN
   IF ACT$ = "+" THEN AV$ = STR$(MakeDec(V1$) + MakeDec(V2$))
   IF ACT$ = "-" THEN AV$ = STR$(MakeDec(V1$) - MakeDec(V2$))
   IF LEFT$(AV$, 1) = "-" THEN
    T% = VAL(AV$) + 256
    AV$ = STR$(T%)
   END IF
   IF LEFT$(AV$, 1) = " " THEN AV$ = MID$(AV$, 2)
   GOTO AddSubAnswer
  END IF

  IF ThisCalc% = 1 THEN
   IF ACT$ = "+" THEN CSC% = CSC% + 1: TempData$(CSC%) = " bcf STATUS, C"
   IF ACT$ = "-" THEN CSC% = CSC% + 1: TempData$(CSC%) = " bsf STATUS, C"
   V1$ = GetByte$(V1$, 0)
   V2$ = GetByte$(V2$, 0)
  END IF

  'Check if V1 is constant, and put into variable if it is
  IF IsConst(V1$) THEN
   CV% = CV% + 1
   CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + V1$
   V1$ = "SysTemp" + STR$(CV%)
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + V1$
   VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
  END IF
  'Move V2 into W
  C$ = " movf " + V2$ + ",W": IF IsConst(V2$) THEN C$ = " movlw " + V2$
  CSC% = CSC% + 1: TempData$(CSC%) = C$
  IF NOT IsConst(V2$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
  'Add/Subtract V2 to/from W
  VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
  C$ = " addwf ": IF ACT$ = "-" THEN C$ = " subwf "
  CSC% = CSC% + 1: TempData$(CSC%) = C$ + V1$ + ",W"

  'Write W to answer
  CV% = CV% + 1
  AV$ = "SysTemp" + STR$(CV%)
  CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + AV$
  VLC% = VLC% + 1: VARLIST$(VLC%) = AV$

  'High Byte
  'Add:
  'Put V1 into var if necessary
  'movlw/movf ,W V2
  'btfsc STATUS, C
  'addlw 1
  'addwf V1, W
  'movwf SysTempN_H

  'Subtract:
  'Put V1 into var if necessary
  'movlw/movf ,W V2
  'btfss STATUS, C
  'addlw 1
  'subwf V1, W
  'movwf SysTempN_H

  IF ThisCalc% = 1 THEN
   VarType$(VLC%) = "WORD"
   V1$ = V1O$: V2$ = V2O$

   AVH$ = "SysTemp" + STR$(CV%) + "_H"
   VLC% = VLC% + 1: VARLIST$(VLC%) = AVH$

   IF IsConst(V1$) THEN
    CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V1$, 1)
    V1$ = "SysTemp" + STR$(CV%)
    VLC% = VLC% + 1: VARLIST$(VLC%) = V1$ + "_H"
    CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + GetByte$(V1$, 1)
   END IF

   T$ = " movf " + GetByte$(V2$, 1) + ",W"
   IF IsConst(V2$) THEN T$ = " movlw " + GetByte$(V2$, 1)
   IF NOT IsConst(V2$) and TypeOfVar$(V2$) = "BYTE" THEN T$ = " clrw"
   
   CSC% = CSC% + 1: TempData$(CSC%) = T$
   T$ = " btfsc ": IF ACT$ = "-" THEN T$ = " btfss "
   CSC% = CSC% + 1: TempData$(CSC%) = T$ + "STATUS,C"
   CSC% = CSC% + 1: TempData$(CSC%) = " addlw 1"
   IF TypeOfVar$(V1$) = "WORD" THEN
    IF ACT$ = "+" THEN CSC% = CSC% + 1: TempData$(CSC%) = " addwf " + GetByte$(V1$, 1) + ",W"
    IF ACT$ = "-" THEN CSC% = CSC% + 1: TempData$(CSC%) = " subwf " + GetByte$(V1$, 1) + ",W"
   END IF
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + AVH$

  END IF
  
  'Replace sum with answer variable
AddSubAnswer:
  Replace SUM$, V1O$ + ACT$ + V2O$, AV$
  GOTO CALCADDSUB
 END IF

'Calculate <,>,=,~. Replace sum with variable containing answer

'Unusual Tokens:
' ~ (not equal)
' } (equal or greater)
' { (less or equal)

CALCCONDITION:
 IF INSTR(SUM$, "{") <> 0 OR INSTR(SUM$, "}") <> 0 OR INSTR(SUM$, "=") <> 0 OR INSTR(SUM$, "~") <> 0 OR INSTR(SUM$, "<") <> 0 OR INSTR(SUM$, ">") <> 0 THEN
  'Read the sum
  FS% = 0
FINDCONDITION:
   FS% = FS% + 1
   ACT$ = MID$(SUM$, FS%, 1)
  IF ACT$ <> "=" AND ACT$ <> "~" AND ACT$ <> "<" AND ACT$ <> ">" AND ACT$ <> "{" AND ACT$ <> "}" THEN GOTO FINDCONDITION
  FV% = 0
  FOR T% = FS% - 1 TO 1 STEP -1
   T$ = MID$(SUM$, T%, 1)
   IF T$ = "{" OR T$ = "}" OR T$ = "=" OR T$ = "~" OR T$ = "<" OR T$ = ">" OR T$ = "*" OR T$ = "/" OR T$ = "+" OR T$ = "-" OR T$ = "&" OR T$ = "|" OR T$ = "#" OR T$ = "!" THEN FV% = T%: EXIT FOR
  NEXT T%
  V1$ = LEFT$(SUM$, INSTR(SUM$, ACT$) - 1): V1$ = MID$(V1$, FV% + 1)
  FOR T% = FS% + 1 TO LEN(SUM$)
   T$ = MID$(SUM$, T%, 1)
   IF T$ = "{" OR T$ = "}" OR T$ = "=" OR T$ = "~" OR T$ = "<" OR T$ = ">" OR T$ = "*" OR T$ = "/" OR T$ = "+" OR T$ = "-" OR T$ = "&" OR T$ = "|" OR T$ = "#" OR T$ = "!" THEN FV% = T%: EXIT FOR
   T$ = " "
  NEXT T%
  V2$ = MID$(SUM$, FS% + 1)
  IF INSTR(V2$, T$) < LEN(V2$) AND INSTR(V2$, T$) <> 0 THEN V2$ = LEFT$(V2$, INSTR(V2$, T$) - 1)
  V1O$ = V1$: V2O$ = V2$

 'Generate asm code for sum

  'Declare SysCalcTempA, SysCalcTempB and SysCalcTempX
  VLC% = VLC% + 1: VARLIST$(VLC%) = "SysCalcTempA"
  VLC% = VLC% + 1: VARLIST$(VLC%) = "SysCalcTempB"
  VLC% = VLC% + 1: VARLIST$(VLC%) = "SysCalcTempX"
  
  TCO% = ThisCalc%
  IF NOT IsWord(V1$ + ACT$ + V2$) THEN ThisCalc% = 0
  
  IF ThisCalc% = 0 OR ACT$ = "=" OR ACT$ = "~" THEN
   
   'Move V1 into SysCalcTempA
  IF ChipFamily% <> 16 OR IsConst$(V1$) THEN
   C$ = " movf " + GetByte$(V1$, 0) + ",W": IF IsConst(V1$) THEN C$ = " movlw " + GetByte$(V1$, 0)
   CSC% = CSC% + 1: TempData$(CSC%) = C$
   IF NOT IsConst(V1$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA"
  END IF
  IF ChipFamily% = 16 AND Not IsConst$(V1$) THEN
   VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
   CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V1$, 0) + ", SysCalcTempA"
  END IF

  'Move V2 into SysCalcTempB
  IF ChipFamily% <> 16 OR IsConst$(V2$) THEN
   C$ = " movf " + GetByte$(V2$, 0) + ",W": IF IsConst(V2$) THEN C$ = " movlw " + GetByte$(V2$, 0)
   CSC% = CSC% + 1: TempData$(CSC%) = C$
   IF NOT IsConst(V2$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB"
  END IF
  IF ChipFamily% = 16 AND Not IsConst$(V2$) THEN
   VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
   CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V2$, 0) + ", SysCalcTempA"
  END IF
   
  END IF

  IF ThisCalc% = 1 THEN

   'High Byte for =, ~
   IF ACT$ = "=" OR ACT$ = "~" THEN
   
    'Move V1 into SysCalcTempA
    'V1 is constant
    IF IsConst(V1$) THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V1$, 1)
     CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA_H"
    END IF
    'V1 is word variable
    IF NOT IsConst(V1$) AND TypeOfVar$(V1$) = "WORD" THEN
     If ChipFamily% <> 16 THEN   
      CSC% = CSC% + 1: TempData$(CSC%) = " movf " + GetByte$(V1$, 1) + ",W"
      VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
      CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA_H"
     END IF
     If ChipFamily% = 16 THEN   
      CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V1$, 1) + ", SysCalcTempA_H"
      VLC% = VLC% + 1: VARLIST$(VLC%) = V1$      
     END IF
    END IF
    'V1 is byte variable
    IF NOT IsConst(V1$) AND TypeOfVar$(V1$) <> "WORD" THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " clrf SysCalcTempA_H"
    END IF
    
    'Move V2 into SysCalcTempB
    'V2 is constant
    IF IsConst(V2$) THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V2$, 1)
     CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB_H"
    END IF
    'V2 is word variable
    IF NOT IsConst(V2$) AND TypeOfVar$(V2$) = "WORD" THEN
     VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
     IF ChipFamily% <> 16 THEN
      CSC% = CSC% + 1: TempData$(CSC%) = " movf " + GetByte$(V2$, 1) + ",W"
      CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB_H"
     END IF
     IF ChipFamily% = 16 THEN CSC% = CSC% + 1: TempData$(CSC%) = " movf " + GetByte$(V2$, 1) + ", SysCalcTempB_H"
    END IF
    'V2 is byte variable
    IF NOT IsConst(V2$) AND TypeOfVar$(V2$) <> "WORD" THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " clrf SysCalcTempB_H"
    END IF
    
   END IF

   IF ACT$ = "<" OR ACT$ = ">" OR ACT$ = "{" OR ACT$ = "}" THEN

    'Move whole word
    'Move V1 into SysCalcTempA
    'V1 is constant
    IF IsConst(V1$) THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V1$, 0)
     CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA_H"
     CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V1$, 1)
     CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA"
    END IF
    'V1 is word variable
    IF NOT IsConst(V1$) THEN
     IF ChipFamily% <> 16 THEN
      CSC% = CSC% + 1: TempData$(CSC%) = " movf " + GetByte$(V1$, 0) + ",W"
      VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
      CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA_H"
      IF TypeOfVar$(V1$) = "WORD" THEN
       CSC% = CSC% + 1: TempData$(CSC%) = " movf " + GetByte$(V1$, 1) + ",W"
       VLC% = VLC% + 1: VARLIST$(VLC%) = GetByte$(V1$, 1)
       CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempA"
      END IF
     END IF
     IF ChipFamily% = 16 THEN
      VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
      CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V1$, 0) + ", SysCalcTempA_H"
      IF TypeOfVar$(V1$) = "WORD" THEN
       VLC% = VLC% + 1: VARLIST$(VLC%) = GetByte$(V1$, 1)
       CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V1$, 1) + ", SysCalcTempA"
      END IF
     END IF
     IF TypeOfVar$(V1$) <> "WORD" THEN
      CSC% = CSC% + 1: TempData$(CSC%) = " clrf SysCalcTempA"
     END IF
    END IF
  
    'Move V2 into SysCalcTempB
    'V2 is constant
    IF IsConst(V2$) THEN
     CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V2$, 0)
     CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB_H"
     CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V2$, 1)
     CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB"
    END IF
    'V2 is word variable
    IF NOT IsConst(V2$) THEN
     IF ChipFamily% <> 16 THEN
      CSC% = CSC% + 1: TempData$(CSC%) = " movf " + GetByte$(V2$, 0) + ",W"
      VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
      CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB_H"
      IF TypeOfVar$(V2$) = "WORD" THEN
       CSC% = CSC% + 1: TempData$(CSC%) = " movf " + GetByte$(V2$, 1) + ",W"
       VLC% = VLC% + 1: VARLIST$(VLC%) = GetByte$(V2$, 1)
       CSC% = CSC% + 1: TempData$(CSC%) = " movwf SysCalcTempB"
      END IF
     END IF
     IF ChipFamily% = 16 THEN
      VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
      CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V2$, 0) + ", SysCalcTempB_H"
      IF TypeOfVar$(V2$) = "WORD" THEN
       VLC% = VLC% + 1: VARLIST$(VLC%) = GetByte$(V2$, 1)
       CSC% = CSC% + 1: TempData$(CSC%) = " movff " + GetByte$(V2$, 1) + ", SysCalcTempB"
      END IF
     END IF
     IF TypeOfVar$(V2$) <> "WORD" THEN
      CSC% = CSC% + 1: TempData$(CSC%) = " clrf SysCalcTempB"
     END IF
    END IF
   
   END IF

  END IF

  IF ACT$ = "=" THEN CSC% = CSC% + 1: TempData$(CSC%) = " call SysCompEqual"
  IF ACT$ = "~" THEN CSC% = CSC% + 1: TempData$(CSC%) = " call SysCompNotEqual"
  IF ACT$ = "<" THEN CSC% = CSC% + 1: TempData$(CSC%) = " call SysCompLessThan"
  IF ACT$ = ">" THEN CSC% = CSC% + 1: TempData$(CSC%) = " call SysCompMoreThan"
  IF ACT$ = "{" THEN CSC% = CSC% + 1: TempData$(CSC%) = " call SysCompLessOrEqual"
  IF ACT$ = "}" THEN CSC% = CSC% + 1: TempData$(CSC%) = " call SysCompMoreOrEqual"
  IF ThisCalc% = 1 THEN TempData$(CSC%) = TempData$(CSC%) + "16"

  'Write answer
  CV% = CV% + 1
  AV$ = "SysTemp" + STR$(CV%)
  IF ChipFamily% <> 16 THEN 
   CSC% = CSC% + 1: TempData$(CSC%) = " movf SysCalcTempX,W"
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + AV$
  END IF
  IF ChipFamily% = 16 THEN 
   CSC% = CSC% + 1: TempData$(CSC%) = " movff SysCalcTempX, " + AV$
  END IF
  
  'Replace sum with answer variable
  Replace SUM$, V1O$ + ACT$ + V2O$, AV$
  VLC% = VLC% + 1: VARLIST$(VLC%) = AV$

  'Restore ThisCalc%
  ThisCalc% = TCO%

  GOTO CALCCONDITION
 END IF

 'Calculate AND,OR,XOR,NOT
CALCLOGIC:
 IF INSTR(SUM$, "&") <> 0 OR INSTR(SUM$, "|") <> 0 OR INSTR(SUM$, "#") <> 0 OR INSTR(SUM$, "!") <> 0 THEN
  'Read the sum
  FS% = 0
FINDLOGICAL:
   FS% = FS% + 1
   ACT$ = MID$(SUM$, FS%, 1)
  IF ACT$ <> "&" AND ACT$ <> "|" AND ACT$ <> "#" AND ACT$ <> "!" THEN GOTO FINDLOGICAL
  IF ACT$ <> "!" THEN
   FV% = 0
   FOR T% = FS% - 1 TO 1 STEP -1
    T$ = MID$(SUM$, T%, 1)
    IF T$ = "&" OR T$ = "|" OR T$ = "#" OR T$ = "!" THEN FV% = T%: EXIT FOR
   NEXT T%
   V1$ = LEFT$(SUM$, INSTR(SUM$, ACT$) - 1): V1$ = MID$(V1$, FV% + 1)
  END IF

  FOR T% = FS% + 1 TO LEN(SUM$)
   T$ = MID$(SUM$, T%, 1)
   IF T$ = "&" OR T$ = "|" OR T$ = "#" OR T$ = "!" THEN FV% = T%: EXIT FOR
   T$ = " "
  NEXT T%
  V2$ = MID$(SUM$, FS% + 1)
  IF INSTR(V2$, T$) < LEN(V2$) AND INSTR(V2$, T$) <> 0 THEN V2$ = LEFT$(V2$, INSTR(V2$, T$) - 1)
  
  TCO% = ThisCalc%
  IF NOT IsWord(V1$ + ACT$ + V2$) THEN ThisCalc% = 0
  
  V1O$ = V1$: ACTO$ = ACT$: V2O$ = V2$
  
  'Generate asm code for sum

  'Check if V2 is a constant, and put it into variable if it is
  IF IsConst(V2$) THEN
   CV% = CV% + 1
   CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V2$, 0)
   V2$ = "SysTemp" + STR$(CV%)
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + V2$
   VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
  END IF
  IF ACT$ <> "!" THEN
   'Move V1 into W
   C$ = " movf " + GetByte$(V1$, 0) + ",W": IF IsConst(V1$) THEN C$ = " movlw " + GetByte$(V1$, 0)
   CSC% = CSC% + 1: TempData$(CSC%) = C$
   IF NOT IsConst(V1$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
  END IF

  'AND/OR/XOR/NOT W with V2
  IF ACT$ = "&" THEN C$ = " andwf "
  IF ACT$ = "|" THEN C$ = " iorwf "
  IF ACT$ = "#" THEN C$ = " xorwf "
  IF ACT$ = "!" THEN C$ = " comf "
  VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
  CSC% = CSC% + 1: TempData$(CSC%) = C$ + GetByte$(V2$, 0) + ",W"
  
  'Write W to answer
  CV% = CV% + 1
  AV$ = "SysTemp" + STR$(CV%)
  CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + AV$
  
  'Replace sum with answer variable
  Replace SUM$, V1O$ + ACTO$ + V2O$, AV$
  VLC% = VLC% + 1: VARLIST$(VLC%) = AV$

  'Repeat for high byte
  IF ThisCalc% = 1 THEN

   V1$ = V1O$: V2$ = V2O$

   'Check if V2 is a constant, and put it into variable if it is
   IF NOT IsConst(V2$) AND TypeOfVar$(V2$) <> "WORD" THEN V2$ = "0"
   IF IsConst(V2$) THEN
    CSC% = CSC% + 1: TempData$(CSC%) = " movlw " + GetByte$(V2$, 1)
    V2$ = "SysTemp" + STR$(CV% - 1)
    CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + V2$ + "_H"
    VLC% = VLC% + 1: VARLIST$(VLC%) = V2$
   END IF
   IF ACT$ <> "!" THEN
    'Move V1 into W
    C$ = " clrw"
    IF TypeOfVar$(V1$) = "WORD" THEN
     C$ = " movf " + GetByte$(V1$, 1) + ",W"
    END IF
    IF IsConst(V1$) THEN C$ = " movlw " + GetByte$(V1$, 1)
    CSC% = CSC% + 1: TempData$(CSC%) = C$
    IF NOT IsConst(V1$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = GetByte$(V1$, 1)
   END IF
 
   'AND/OR/XOR/NOT W with V2
   IF ACT$ = "&" THEN C$ = " andwf "
   IF ACT$ = "|" THEN C$ = " iorwf "
   IF ACT$ = "#" THEN C$ = " xorwf "
   IF ACT$ = "!" THEN C$ = " comf "
   VLC% = VLC% + 1: VARLIST$(VLC%) = GetByte$(V2$, 1)
   CSC% = CSC% + 1: TempData$(CSC%) = C$ + GetByte$(V2$, 1) + ",W"
 
   'Write W to answer
   AV$ = "SysTemp" + STR$(CV%) + "_H"
   CSC% = CSC% + 1: TempData$(CSC%) = " movwf " + AV$
 
   VLC% = VLC% + 1: VARLIST$(VLC%) = AV$
   
  END IF

  ThisCalc% = TCO%

  GOTO CALCLOGIC
 END IF

END SUB

SUB CompileConditions (Condition$, IfTrue$)

 'Initialise
 FOR PD% = 1 TO 100
  CheckTemp$(PD%) = ""
 NEXT PD%
 COSC% = 0: CV% = 0

 'Decide whether to compile inline or using CompileCalc
 Complex = 0
 IF CountOccur(Condition$, "';=~<>{}.") = 0 THEN Complex = -1
 IF CountOccur(Condition$, "';=~<>{}+-*/&|#!") >= 2 THEN Complex = 1
 IF IsWord(Condition$) AND INSTR(Condition$, ".") = 0 THEN Complex = 1
 
 'One condition, so compile inline
 IF Complex = 0 THEN
  OP$ = ""
  IF INSTR(Condition$, "=") <> 0 THEN OP$ = "="
  IF INSTR(Condition$, "~") <> 0 THEN OP$ = "~"
  IF INSTR(Condition$, "<") <> 0 THEN OP$ = "<"
  IF INSTR(Condition$, ">") <> 0 THEN OP$ = ">"
  IF INSTR(Condition$, "{") <> 0 THEN OP$ = "{"
  IF INSTR(Condition$, "}") <> 0 THEN OP$ = "}"
  IF INSTR(Condition$, ".") <> 0 THEN OP$ = "."

  'If the condition is not a bit test, then run this code:
  IF OP$ <> "." THEN
   'Read input variables
   V1$ = UCASE$(LEFT$(Condition$, INSTR(Condition$, OP$) - 1))
   DO WHILE INSTR(V1$, " ") <> 0: Replace V1$, " ", "": LOOP
   V2$ = UCASE$(MID$(Condition$, INSTR(Condition$, OP$) + 1))
   DO WHILE INSTR(V2$, " ") <> 0: Replace V2$, " ", "": LOOP
   
   'IF TypeOfVar$(V1$) <> "BYTE" OR TypeOfVar$(V2$) <> "BYTE" THEN
   ' Complex = 1
   ' GOTO ComplexCondition
   'END IF

   IF NOT IsConst(V1$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V1$
   IF NOT IsConst(V2$) THEN VLC% = VLC% + 1: VARLIST$(VLC%) = V2$

   'Code for equal/not equal
   IF OP$ = "=" OR OP$ = "~" THEN
    'V1 must always be a variable. V2 can be variable or constant.
    IF IsConst(V1$) AND NOT IsConst(V2$) THEN T$ = V1$: V1$ = V2$: V2$ = T$
    IF IsConst(V1$) AND IsConst(V2$) THEN
     OSC% = COSC% + 1: CheckTemp$(COSC%) = " movlw " + V1$
     COSC% = COSC% + 1: CheckTemp$(COSC%) = " movwf SysIFTemp"
     V1$ = "SysIFTemp": VLC% = VLC% + 1: VARLIST$(VLC%) = "SysIFTemp"
    END IF
    IF IsConst(V2$) THEN COSC% = COSC% + 1: CheckTemp$(COSC%) = " movlw " + V2$
    IF NOT IsConst(V2$) THEN COSC% = COSC% + 1: CheckTemp$(COSC%) = " movf " + V2$ + ",W"
    COSC% = COSC% + 1: CheckTemp$(COSC%) = " subwf " + V1$ + ",W"

    IF INSTR(UCASE$(IfTrue$), "TRUE") <> 0 THEN
     C$ = " btfss STATUS, Z"
     IF OP$ = "=" THEN C$ = " btfsc STATUS, Z"
    END IF
    IF INSTR(UCASE$(IfTrue$), "FALSE") <> 0 THEN
     C$ = " btfsc STATUS, Z"
     IF OP$ = "=" THEN C$ = " btfss STATUS, Z"
    END IF
    COSC% = COSC% + 1: CheckTemp$(COSC%) = C$
   END IF

   'Code for greater than/less than
   IF OP$ = "<" OR OP$ = ">" THEN
    'COSC% = COSC% + 1: CheckTemp$(COSC%) = " bsf STATUS, C"
    IF OP$ = ">" THEN T$ = V1$: V1$ = V2$: V2$ = T$
    'Take bigger from smaller. V2 should be bigger. Take V2 from V1
    'mov small into W
    'sub big from W
    IF IsConst(V1$) THEN
     COSC% = COSC% + 1: CheckTemp$(COSC%) = " movlw " + V1$
     COSC% = COSC% + 1: CheckTemp$(COSC%) = " movwf SysIFTemp"
     V1$ = "SysIFTemp": VLC% = VLC% + 1: VARLIST$(VLC%) = "SysIFTemp"
    END IF
    IF IsConst(V2$) THEN C$ = " movlw " + V2$
    IF NOT IsConst(V2$) THEN C$ = " movf " + V2$ + ",W"
    COSC% = COSC% + 1: CheckTemp$(COSC%) = C$
    COSC% = COSC% + 1: CheckTemp$(COSC%) = " subwf " + V1$ + ",W"
    IF INSTR(UCASE$(IfTrue$), "TRUE") <> 0 THEN C$ = " btfss STATUS, C"
    IF INSTR(UCASE$(IfTrue$), "FALSE") <> 0 THEN C$ = " btfsc STATUS, C"
    COSC% = COSC% + 1: CheckTemp$(COSC%) = C$
   END IF

   'Code for less or equal/more or equal
   IF OP$ = "{" OR OP$ = "}" THEN
    'COSC% = COSC% + 1: CheckTemp$(COSC%) = " bsf STATUS, C"
    IF OP$ = "{" THEN T$ = V1$: V1$ = V2$: V2$ = T$
    'Take smaller from bigger. V1 should be bigger or equal. Take V2 from V1
    'mov small into W
    'sub big from W
    IF IsConst(V1$) THEN
     COSC% = COSC% + 1: CheckTemp$(COSC%) = " movlw " + V1$
     COSC% = COSC% + 1: CheckTemp$(COSC%) = " movwf SysIFTemp"
     V1$ = "SysIFTemp": VLC% = VLC% + 1: VARLIST$(VLC%) = "SysIFTemp"
    END IF
    IF IsConst(V2$) THEN C$ = " movlw " + V2$
    IF NOT IsConst(V2$) THEN C$ = " movf " + V2$ + ",W"
    COSC% = COSC% + 1: CheckTemp$(COSC%) = C$
    COSC% = COSC% + 1: CheckTemp$(COSC%) = " subwf " + V1$ + ",W"
    IF INSTR(UCASE$(IfTrue$), "TRUE") <> 0 THEN C$ = " btfsc STATUS, C"
    IF INSTR(UCASE$(IfTrue$), "FALSE") <> 0 THEN C$ = " btfss STATUS, C"
    COSC% = COSC% + 1: CheckTemp$(COSC%) = C$
   END IF

  END IF

  'Code for a bit test
  IF OP$ = "." THEN
   IF INSTR(Condition$, "=") <> 0 THEN Replace Condition$, "=", ""
   T% = 1
   V$ = UCASE$(LEFT$(Condition$, INSTR(Condition$, ".") - 1))
   B$ = MID$(Condition$, INSTR(Condition$, ".") + 1): B$ = LEFT$(B$, INSTR(B$, " ") - 1)
   'S% = VAL(MID$(Condition$, INSTR(Condition$, " ") + 1))
   C$ = LEFT$(TRIM$(MID$(Condition$, INSTR(Condition$, " ") + 1)), 1)
   S% = 1
   IF C$ = "0" THEN S% = 0

   'Allow use with vars > 8 bit
   IF VAL(B$) > 7 THEN
    B$ = STR$(VAL(B$) - 8)
    V$ = V$ + "_H"
   END IF
   
   IF INSTR(UCASE$(IfTrue$), "TRUE") <> 0 THEN
    C$ = " btfss "
    IF S% = 1 THEN C$ = " btfsc "
   END IF
   IF INSTR(UCASE$(IfTrue$), "FALSE") <> 0 THEN
    C$ = " btfsc "
    IF S% = 1 THEN C$ = " btfss "
   END IF
   C$ = C$ + V$ + "," + B$
   COSC% = 1: CheckTemp$(COSC%) = C$
   VLC% = VLC% + 1: VARLIST$(VLC%) = V$
  END IF

 END IF

 'More than one condition, so compile as calculation
ComplexCondition:
 IF Complex = 1 THEN
  FOR T% = 1 TO 100
   TempData$(T%) = ""
  NEXT T%
  DO WHILE INSTR(Condition$, " ") <> 0: Replace Condition$, " ", "": LOOP
  T% = 0
  S$ = Condition$
  CSC% = 0
  ThisCalc% = 0
  IF IsWord(S$) THEN ThisCalc% = 1
  
  CompileCalc S$
  IF INSTR(TempData$(CSC%), " movff ") = 0 THEN TempData$(CSC%) = " movwf SysIFTemp"
  IF INSTR(TempData$(CSC%), " movff ") <> 0 THEN
   TempData$(CSC%) = LEFT$(TempData$(CSC%), INSTR(TempData$(CSC%), ",")) + "W"
   Replace TempData$(CSC%), " movff ", " movf "
  END IF
  FOR WD% = 1 TO CSC%
   CheckTemp$(WD%) = TempData$(WD%)
  NEXT WD%
  COSC% = CSC% + 1
  IF INSTR(UCASE$(IfTrue$), "TRUE") <> 0 THEN C$ = " btfss STATUS, Z"
  IF INSTR(UCASE$(IfTrue$), "FALSE") <> 0 THEN C$ = " btfsc STATUS, Z"
  CheckTemp$(COSC%) = C$
  VLC% = VLC% + 1: VARLIST$(VLC%) = "SysIFTemp"
 END IF

 'No conditions - check if variable is 0 or non-0
 IF Complex = -1 THEN
  COSC% = 2
  CheckTemp$(1) = "SysIFTemp=" + UCASE$(Condition$)
  IF IsConst(Condition$) THEN CheckTemp$(2) = " movf SysIFTemp, F": COSC% = COSC% + 1
  IF INSTR(UCASE$(IfTrue$), "TRUE") <> 0 THEN C$ = " btfss STATUS, Z"
  IF INSTR(UCASE$(IfTrue$), "FALSE") <> 0 THEN C$ = " btfsc STATUS, Z"
  CheckTemp$(COSC%) = C$
 END IF

END SUB

SUB CompileDir
CD% = 0
FoundCount% = 0

lbCompileDir:
 CD% = CD% + 1
 DS$ = PROG$(CD%)
 IF UCASE$(LEFT$(DS$, 4)) = "DIR " THEN
   
  Origin$ = ""
  IF INSTR(DS$, ";?F") <> 0 THEN 
   Origin$ = MID$(DS$, INSTR(DS$, ";?F"))
   DS$ = RTRIM$(LEFT$(DS$, INSTR(DS$, ";?F") - 1))
  END IF
  
  FoundCount% = FoundCount% + 1
  DS$ = LTRIM$(RTRIM$(MID$(DS$, 4)))
  D$ = LTRIM$(MID$(DS$, INSTR(DS$, " ") + 1))
  P$ = RTRIM$(LEFT$(DS$, INSTR(DS$, " ") - 1))
  IF INSTR(P$, "GPIO") <> 0 THEN Replace P$, "GPIO", "IO"
  IF INSTR(P$, "PORT") = 0 AND LEFT$(ChipName$, 2) = "16" AND LEN(P$) = 2 THEN P$ = "PORT" + LEFT$(P$, 1) + "." + MID$(P$, 2)
  Replace D$, "PUT", ""
  
  V$ = P$
  IF INSTR(V$, "PORT") = 0 THEN V$ = "TRIS" + V$: GOTO SetDir
  IF INSTR(V$, "PORT") <> 0 THEN Replace V$, "PORT", "TRIS"
  
SetDir:

  'Determine if DIR sets 1 bit or whole port
  DirPort% = 1
  IF INSTR(P$, ".") <> 0 THEN DirPort% = 0
  
  'Check for 12-bit compatible code
  IF DirPort% = 0 AND ChipFamily% = 12 THEN ERC% = ERC% + 1: gcERR$(ERC%) = Origin$ + Message$("NoBitSet")
  
  'Check that a pin is being set
  T$ = V$
  Replace T$, "TRIS", ""
  T2$ = T$
  IF INSTR(T$, ".") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, ".") - 1)
  IF LEN(T$) <> 1 AND T$ <> "GPIO" AND T$ <> "IO" THEN
   IF T2$ = "" AND (LEFT$(ChipName$, 2) = "10" OR LEFT$(ChipName$, 2) = "12") THEN V$ = "TRISIO"
   IF T2$ <> "" OR (LEFT$(ChipName$, 2) = "16" OR LEFT$(ChipName$, 2) = "18") THEN
    ERC% = ERC% + 1
    M$ = Message$("NotIO")
    Replace M$, "%var%", T2$
    gcERR$(ERC%) = Origin$ + M$
   END IF
  END IF
    
  'Add code
  IF DirPort% = 1 THEN
   PROG$(CD%) = ""
   IF D$ = "IN" THEN 
       IF PSP% = 0 OR V$ <> "TRISE" THEN PROG$(CD%) = V$ + "=255"
       IF PSP% = 1 AND V$ = "TRISE" THEN PROG$(CD%) = V$ + "=239"
   END IF
   IF D$ = "OUT" THEN 
    IF V$ <> "TRISIO" THEN PROG$(CD%) = " clrf " + V$
    IF V$ = "TRISIO" THEN
     PROG$(CD%) = " clrw"
     AddLine " movwf TRISIO", CD% + 1
     CD% = CD% + 1
    END IF
   END IF    
   IF PROG$(CD%) = "" THEN PROG$(CD%) = V$ + "=" + D$
  END IF

  IF DirPort% = 0 THEN
   Replace V$, ".", ","
   IF D$ = "IN" THEN PROG$(CD%) = " bsf " + V$
   IF D$ = "OUT" THEN PROG$(CD%) = " bcf " + V$
  END IF

 END IF
IF CD% < APC% THEN GOTO lbCompileDir

END SUB

SUB CompileDo
PD% = 0
DLC% = 0
DL% = 0
lbCompileDo:
PD% = PD% + 1
 DS$ = UCASE$(PROG$(PD%))
 IF LEFT$(DS$, 3) = "DO " OR DS$ = "DO" THEN
  
  Origin$ = ""
  IF INSTR(DS$, ";?F") <> 0 THEN 
   Origin$ = MID$(DS$, INSTR(DS$, ";?F"))
   DS$ = RTRIM$(LEFT$(DS$, INSTR(DS$, ";?F") - 1))
  END IF
    
  CP% = 0
  IF LEFT$(DS$, 3) = "DO " THEN CP% = 1
  DL% = 1
  DLC% = DLC% + 1
  PROG$(PD%) = "SysDoLoop_S" + STR$(DLC%)

  FOR CD% = PD% + 1 TO APC%
   T$ = UCASE$(PROG$(CD%))
   IF LEFT$(T$, 5) = "LOOP " OR T$ = "LOOP" THEN DL% = DL% - 1
   IF LEFT$(T$, 3) = "DO " OR T$ = "DO" THEN DL% = DL% + 1
   IF DL% = 0 THEN LL% = CD%: EXIT FOR
  NEXT CD%
  
  'Origin$ = ""
  IF INSTR(PROG$(LL%), ";?F") <> 0 THEN 
   'Origin$ = MID$(DS$, INSTR(DS$, ";?F"))
   PROG$(LL%) = RTRIM$(LEFT$(PROG$(LL%), INSTR(PROG$(LL%), ";?F") - 1))
  END IF
  
  IF LEFT$(PROG$(LL%), 5) = "LOOP " THEN CP% = 2

  IF CP% = 1 THEN
   V$ = LTRIM$(MID$(DS$, 4))
   U$ = LCASE$(MID$(V$, INSTR(V$, " ") + 1))
   V$ = UCASE$(LEFT$(V$, INSTR(V$, " ") - 1))
   C$ = U$
  END IF
  IF CP% = 2 THEN
   V$ = LTRIM$(MID$(PROG$(LL%), 6))
   U$ = LCASE$(MID$(V$, INSTR(V$, " ") + 1))
   V$ = UCASE$(LEFT$(V$, INSTR(V$, " ") - 1))
   C$ = U$
  END IF

  IF CP% = 1 THEN
   T$ = "TRUE": IF V$ = "WHILE" THEN T$ = "FALSE"
   CompileConditions C$, T$
   COSC% = COSC% + 1: CheckTemp$(COSC%) = " goto SysDoLoop_E" + STR$(DLC%)
   FOR WD% = 1 TO COSC%
    AddLine CheckTemp$(WD%), PD% + WD%
   NEXT WD%
   PD% = PD% + COSC%
   LL% = LL% + COSC%
  END IF
 
  IF CP% = 2 THEN
   T$ = "TRUE": IF V$ = "UNTIL" THEN T$ = "FALSE"
   CompileConditions C$, T$
   'COSC% = COSC% + 1: CheckTemp$(COSC%) = " goto SysDoLoop_E" + STR$(DLC%)
   FOR WD% = 1 TO COSC%
    AddLine CheckTemp$(WD%), LL% + WD% - 1
   NEXT WD%
   PD% = PD% + COSC%
   LL% = LL% + COSC%
  END IF
  
  PROG$(LL%) = " goto SysDoLoop_S" + STR$(DLC%)
  IF CP% <> 0 THEN AddLine "SysDoLoop_E" + STR$(DLC%), LL% + 1
 END IF

IF PD% < APC% THEN GOTO lbCompileDo

FoundCount% = DLC%
END SUB

SUB CompileFor
PD% = 0
lbCompileFor:
PD% = PD% + 1
 T$ = PROG$(PD%)
 FL% = 0

 IF LEFT$(T$, 4) = "FOR " THEN
 
  Origin$ = ""
  IF INSTR(T$, ";?F") <> 0 THEN 
   Origin$ = MID$(T$, INSTR(T$, ";?F"))
   T$ = RTRIM$(LEFT$(T$, INSTR(T$, ";?F") - 1))
  END IF
  
  FCO$ = T$
  Replace T$, " TO ", "~"
  IF INSTR(T$, " STEP ") <> 0 THEN Replace T$, " STEP ", "+"
  DO WHILE INSTR(T$, " ") <> 0: Replace T$, " ", "": LOOP
  V$ = MID$(T$, 4)
  V$ = LEFT$(V$, INSTR(V$, "=") - 1)
  SV$ = MID$(T$, INSTR(T$, "=") + 1)
  SV$ = LEFT$(SV$, INSTR(SV$, "~") - 1)
  EV$ = MID$(T$, INSTR(T$, "~") + 1)
  ST$ = "1"
  IF INSTR(T$, "+") <> 0 THEN
   EV$ = LEFT$(EV$, INSTR(EV$, "+") - 1)
   ST$ = MID$(T$, INSTR(T$, "+") + 1)
  END IF

  IF VAL(EV$) < VAL(SV$) AND IsConst(EV$) AND IsConst(SV$) AND INSTR(ST$, "-") = 0 THEN ST$ = "-" + ST$
  
  FLC% = FLC% + 1
  FL% = 1

  'Pseudo code:
  'V = SV - ST
  'SysForLoop(n):
  'V += ST
  '...
  'if V < EV then goto SysForLoop(n)
  
  'Starting code
  PROG$(PD%) = V$ + "=" + SV$ + "-" + ST$
  AddLine "SysForLoop" + STR$(FLC%), PD% + 1
  AddLine V$ + "=" + V$ + "+" + ST$, PD% + 2

  'End code
  LL% = 0
  FOR CD% = PD% TO APC%
   T$ = UCASE$(PROG$(CD%))
   IF LEFT$(T$, 4) = "FOR " THEN FL% = FL% + 1
   IF T$ = "NEXT" THEN FL% = FL% - 1
   IF LEN(T$) > 4 THEN
    IF LEFT$(T$, 5) = "NEXT " THEN FL% = FL% - 1
   END IF
   IF FL% = 0 THEN LL% = CD%: EXIT FOR
  NEXT CD%
  
  IF LL% = 0 THEN
   ERC% = ERC% + 1
   T$ = Message$("NoNext")
   'Replace T$, "%for%", CHR$(34) + FCO$ + CHR$(34)
   T$ = Origin$ + T$
   gcERR$(ERC%) = T$
  END IF
  IF LL% > 0 THEN
   IF INSTR(ST$, "-") = 0 THEN PROG$(LL%) = "IF " + V$ + " < " + EV$ + " THEN"
   IF INSTR(ST$, "-") <> 0 THEN PROG$(LL%) = "IF " + V$ + " > " + EV$ + " THEN"
   AddLine " goto SysForLoop" + STR$(FLC%), LL% + 1
   AddLine "END IF", LL% + 2
  END IF
 END IF
IF PD% < APC% THEN GOTO lbCompileFor

FoundCount% = FLC%

END SUB

SUB CompilePot

PD% = 0
lbPotCompileStart:
 PD% = PD% + 1

 DS$ = PROG$(PD%)
 IF LEFT$(DS$, 4) = "POT " THEN
  P$ = MID$(DS$, 5)
  
  Origin$ = ""
  IF INSTR(P$, ";?F") <> 0 THEN 
   Origin$ = MID$(P$, INSTR(P$, ";?F"))
   P$ = RTRIM$(LEFT$(P$, INSTR(P$, ";?F") - 1))
  END IF
  
  V$ = LTRIM$(MID$(P$, INSTR(P$, ",") + 1))
  P$ = LTRIM$(RTRIM$(LEFT$(P$, INSTR(P$, ",") - 1)))
  POC% = POC% + 1

  PROG$(PD%) = "DIR " + P$ + " OUT"
  AddLine "SET " + P$ + " 0", PD% + 1
  AddLine "WAIT 5 MS", PD% + 2
  AddLine "DIR " + P$ + " IN", PD% + 3
  AddLine V$ + "=0", PD% + 4
  AddLine "DO WHILE " + P$ + " OFF", PD% + 5
  AddLine V$ + "=" + V$ + "+1", PD% + 6
  AddLine "IF " + V$ + " = 255 THEN", PD% + 7
  AddLine "GOTO ENDPOT" + STR$(POC%), PD% + 8
  AddLine "END IF", PD% + 9
  AddLine "WAIT 10 10US", PD% + 10
  AddLine "LOOP", PD% + 11
  AddLine "ENDPOT" + STR$(POC%), PD% + 12
  PD% = PD% + 10
  FoundCount% = FoundCount% + 1

 END IF
IF PD% < APC% THEN GOTO lbPotCompileStart

END SUB

SUB CompilePulseOut

FoundCount% = 0

PD% = 0
lbCompilePulseOut:
PD% = PD% + 1

 DS$ = PROG$(PD%)
 IF LEFT$(DS$, 9) = "PULSEOUT " THEN

  P$ = LTRIM$(RTRIM$(MID$(DS$, 10)))
  D$ = LTRIM$(MID$(P$, INSTR(P$, ",") + 1))
  P$ = RTRIM$(LEFT$(P$, INSTR(P$, ",") - 1))

  PROG$(PD%) = "SET " + P$ + " 1"
  AddLine "WAIT " + D$, PD% + 1
  AddLine "SET " + P$ + " 0", PD% + 2
  PD% = PD% + 2
  
  FoundCount% = FoundCount% + 1
 END IF

IF PD% < APC% THEN GOTO lbCompilePulseOut

END SUB

SUB CompileRepeat
    
PD% = 0
FLC% = 0
RVN% = 0

lbCompileRepeat:
PD% = PD% + 1
 T$ = PROG$(PD%)
 RL% = 0

 IF LEFT$(T$, 7) = "REPEAT " THEN
 
  'Get origin
  Origin$ = ""
  IF INSTR(T$, ";?F") <> 0 THEN 
   Origin$ = MID$(T$, INSTR(T$, ";?F"))
   T$ = RTRIM$(LEFT$(T$, INSTR(T$, ";?F") - 1))
  END IF
  
  'Get parameter
  Replace T$, "(", " "
  Replace T$, ")", " "
  RV$ = TRIM$(MID$(T$, INSTR(T$, " ") + 1))
  RepWord% = 0
  IF Not IsConst(RV$) THEN
   IF IsWord(RV$) THEN RepWord% = 1
  END IF
  IF MakeDec(RV$) > 255 THEN RepWord% = 1
  
  FLC% = FLC% + 1
  RL% = 1
  RVN% = RVN% + 1

  VLC% = VLC% + 1: VARLIST$(VLC%) = "SysRepeatTemp" + STR$(RVN%): VarType$(VLC%) = ""
  If RepWord% = 1 THEN VLC% = VLC% + 1: VARLIST$(VLC%) = "SysRepeatTemp" + STR$(RVN%) + "_H": VarType$(VLC%) = ""

  'Pseudo code:
  'V = RV
  'SysRepeatLoop(n):
  '...
  'decfsz RV, F
  'goto SysRepeatLoop(n)
  
  'Starting code
  'PROG$(PD%) = "SysRepeatTemp" + STR$(RVN%) + "=" + RV$
  IF IsConst(RV$) THEN
   PROG$(PD%) = " movlw " + GetByte$(RV$, 0)
   AddLine " movwf SysRepeatTemp" + STR$(RVN%), PD% + 1
   PD% = PD% + 1
   If RepWord% = 1 THEN
   AddLine " movlw " + STR$(VAL(GetByte$(RV$, 1)) + 1), PD% + 1
   AddLine " movwf SysRepeatTemp" + STR$(RVN%) + "_H", PD% + 2
   PD% = PD% + 2
   END IF
  END IF    
  IF NOT IsConst(RV$) THEN
   If ChipFamily% <> 16 THEN
    PROG$(PD%) = " movf " + GetByte$(RV$, 0) + ",W"
    AddLine " movwf SysRepeatTemp" + STR$(RVN%), PD% + 1
    PD% = PD% + 1
   END IF
   IF ChipFamily% = 16 THEN
    PROG$(PD%) = " movff " + RV$ + ", SysRepeatTemp" + STR$(RVN%)
   END IF
   If RepWord% = 1 THEN
    IF ChipFamily% <> 16 THEN
     AddLine " movf " + GetByte$(RV$, 1) + ",W" , PD% + 1
     AddLine " movwf SysRepeatTemp" + STR$(RVN%) + "_H", PD% + 2
     PD% = PD% + 2
    END IF
    IF ChipFamily% = 16 THEN
     AddLine " movff " + GetByte$(RV$, 1) + ", SysRepeatTemp" + STR$(RVN%) + "_H", PD% + 1
     PD% = PD% + 1
    END IF
    AddLine " incf SysRepeatTemp" + STR$(RVN%) + "_H, F", PD% + 1
    PD% = PD% + 1
   END IF
  END IF
  AddLine "SysRepeatLoop" + STR$(FLC%), PD% + 1
  
  'Find End
  EV% = -1
  FOR FE% = PD% + 1 TO APC%
   IF LEFT$(PROG$(FE%), 7) = "REPEAT " THEN RL% = RL% + 1
   IF LEFT$(PROG$(FE%), 10) = "END REPEAT" THEN RL% = RL% - 1
   IF RL% = 0 THEN EV% = FE%: EXIT FOR
  NEXT
  'Match not found error
  IF EV% = -1 THEN
   ERC% = ERC% + 1
   gcERR$(ERC%) = Origin$ + Message$("NoEndRepeat")
   IF PD% < APC% THEN GOTO lbCompileRepeat
  END IF
 
  'Add end code
  PROG$(EV%) = " decfsz SysRepeatTemp" + STR$(RVN%) + ",F"
  AddLine " goto SysRepeatLoop" + STR$(FLC%), EV% + 1
  If RepWord% = 1 THEN
   AddLine " decfsz SysRepeatTemp" + STR$(RVN%) + "_H,F", EV% + 2
   AddLine " goto SysRepeatLoop" + STR$(FLC%), EV% + 3
  END IF 
 END IF

 IF INSTR(T$, " decfsz SysRepeatTemp") <> 0 AND INSTR(T$, "_H") = 0 Then RVN% = RVN% - 1

IF PD% < APC% THEN GOTO lbCompileRepeat

FoundCount% = FLC%

 
END SUB

SUB CompileRotate

FoundCount% = 0

PD% = 0
lbCompileRotate:
PD% = PD% + 1
 T$ = UCASE$(PROG$(PD%))
 IF LEFT$(T$, 7) = "ROTATE " THEN
  
  Origin$ = ""
  IF INSTR(T$, ";?F") <> 0 THEN 
   Origin$ = MID$(T$, INSTR(T$, ";?F"))
   T$ = RTRIM$(LEFT$(T$, INSTR(T$, ";?F") - 1))
  END IF
  
  V$ = MID$(T$, INSTR(T$, " ") + 1)
  D$ = MID$(V$, INSTR(V$, " ") + 1)
  V$ = LEFT$(V$, INSTR(V$, " ") - 1)
  VT$ = TypeOfVar$(V$)
  IF D$ = "LC" Then D$ = "LEFT"
  IF D$ = "RC" Then D$ = "RIGHT"
  IF D$ = "L" Then D$ = "LEFT SIMPLE"
  IF D$ = "R" Then D$ = "RIGHT SIMPLE"
  
  UseC% = 1
  IF INSTR(D$, "SIMPLE") <> 0 Then
   UseC% = 0
   Replace D$, "SIMPLE", ""
   D$ = TRIM(D$)
  End If
  
  If (ChipFamily% <> 16 OR VT$ = "WORD") And UseC% = 0 Then
   AddLine " bcf STATUS,C", PD%
   If D$ = "RIGHT" Then AddLine " btfsc " + V$ + ",0", PD% + 1
   If D$ = "LEFT" Then
    If VT$ <> "WORD" Then AddLine " btfsc " + V$ + ",7", PD% + 1
    If VT$ = "WORD" Then AddLine " btfsc " + V$ + "_H,7", PD% + 1
   End If
   AddLine " bsf STATUS,C", PD% + 2
   PD% = PD% + 3
  End If
  
  IF VT$ = "WORD" AND D$ = "RIGHT" THEN
   If ChipFamily% <> 16 THEN AddLine " rrf " + V$ + "_H,F", PD%
   If ChipFamily% = 16 THEN AddLine " rrcf " + V$ + "_H,F", PD%
   PD% = PD% + 1
  END IF

  IF ChipFamily% <> 16 THEN
   C$ = " rrf ": IF D$ = "LEFT" THEN C$ = " rlf "
  END IF
  IF ChipFamily% = 16 THEN
   If UseC% = 1 OR VT$ = "WORD" Then C$ = " rrcf ": IF D$ = "LEFT" THEN C$ = " rlcf "
   If UseC% = 0 AND VT$ <> "WORD" Then C$ = " rrncf ": IF D$ = "LEFT" THEN C$ = " rlncf "
  END IF
  
  PROG$(PD%) = C$ + V$ + ",F"
  IF VT$ <> "WORD" THEN
   VLC% = VLC% + 1: VARLIST$(VLC%) = V$
  END IF 

  IF VT$ = "WORD" AND D$ = "LEFT" THEN
   PD% = PD% + 1
   IF ChipFamily% <> 16 THEN AddLine " rlf " + V$ + "_H,F", PD%
   IF ChipFamily% = 16 THEN AddLine " rlcf " + V$ + "_H,F", PD%
  END IF
  
  FoundCount% = FoundCount% + 1
 END IF
IF PD% < APC% THEN GOTO lbCompileRotate

END SUB

SUB CompileSelect
FoundCount% = 0

PD% = 0
SCL% = 0
SCT% = 0
lbCompileSelect:
 PD% = PD% + 1
 DS$ = UCASE$(PROG$(PD%))
 
 IF LEFT$(DS$, 12) = "SELECT CASE " THEN
    
  Origin$ = ""
  IF INSTR(DS$, ";?F") <> 0 THEN 
   Origin$ = MID$(DS$, INSTR(DS$, ";?F")) 
   DS$ = RTRIM$(LEFT$(DS$, INSTR(DS$, ";?F") - 1))
  END IF 
 
  SV$ = TRIM$(MID$(DS$, 12))
  SCL% = 1
  CC% = 0
  'PROG$(PD%) = " goto SysSelect" + STR$(SCT%) + "Case" + STR$(CC%+1)
  DelLine PD%: PD% -= 1
  SCT% = SCT% + 1
  
  'For SCC% = PD% TO APC%
  SCC% = PD% - 1
  FindAndCompileSelect:
   SCC% += 1
  
   DS$ = PROG$(SCC%)
   
   IF LEFT$(DS$, 12) = "SELECT CASE " THEN SCL% = SCL% + 1
   
   IF SCL% = 1 AND LEFT$(DS$, 5) = "CASE " THEN
    CC% = CC% + 1
    
    IF CC% = 1 THEN
     PROG$(SCC%) = "SysSelect" + STR$(SCT%) + "Case" + STR$(CC%)
     SCC% -= 1
    END IF
    If CC% > 1 THEN
     PROG$(SCC%) = " goto SysSelectEnd" + STR$(SCT%)
     AddLine "SysSelect" + STR$(SCT%) + "Case" + STR$(CC%), SCC% + 1
    END IF
    
    C$ = TRIM$(MID$(DS$, 5))
    IF INSTR(C$, ";") <> 0 THEN C$ = TRIM$(LEFT$(C$, INSTR(C$, ";") - 1))
    
    IF C$ <> "ELSE" THEN
     IF CountOccur(C$, "'=~<>{}") = 0 THEN C$ = "=" + C$
     
     IF INSTR(C$, "=") <> 0 Then Replace C$, "=", ";"
     IF INSTR(C$, "~") <> 0 Then Replace C$, "~", "="
     IF INSTR(C$, ";") <> 0 Then Replace C$, ";", "~"
     
     IF INSTR(C$, "<") <> 0 Then Replace C$, "<", ";"
     IF INSTR(C$, ">") <> 0 Then Replace C$, ">", "<"
     IF INSTR(C$, ";") <> 0 Then Replace C$, ";", ">"
     
     IF INSTR(C$, "{") <> 0 Then Replace C$, "{", ";"
     IF INSTR(C$, "}") <> 0 Then Replace C$, "}", "{"
     IF INSTR(C$, ";") <> 0 Then Replace C$, ";", "}"
          
     AddLine "IF " + SV$ + C$ + "THEN", SCC% + 2
     AddLine " goto SysSelect" + STR$(SCT%) + "Case" + STR$(CC% + 1), SCC% + 3
     AddLine "END IF", SCC% + 4
    END IF

   END IF
    
   IF LEFT$(DS$, 10) = "END SELECT" THEN
    
    IF SCL% = 1 THEN
     PROG$(SCC%) = "SysSelect" + STR$(SCT%) + "Case" + STR$(CC%+1)
     AddLine "SysSelectEnd" + STR$(SCT%), SCC% + 1
    END IF
    SCL% = SCL% - 1
   END IF
   
 IF SCC% < APC% AND SCL% > 0 THEN GOTO FindAndCompileSelect
   
 END IF

IF PD% < APC% THEN GOTO lbCompileSelect 
 
END SUB

SUB CompileSet
FoundCount% = 0

FOR CD% = 1 TO APC%
 T$ = PROG$(CD%)
 IF UCASE$(LEFT$(T$, 4)) = "SET " THEN
 
  Origin$ = ""
  IF INSTR(T$, ";?F") <> 0 THEN 
   Origin$ = MID$(T$, INSTR(T$, ";?F"))
   T$ = RTRIM$(LEFT$(T$, INSTR(T$, ";?F") - 1))
  END IF
  
 IF INSTR(T$, ".") = 0 THEN
   ERC% = ERC% + 1: gcERR$(ERC%) = Origin$ + Message$("NoBit")
   PROG$(CD%) = ""
   GOTO BADSET
  END IF
  P$ = MID$(T$, INSTR(T$, " ") + 1): P$ = LEFT$(P$, INSTR(P$, ".") - 1)
  B$ = MID$(T$, INSTR(T$, ".") + 1): B$ = LEFT$(B$, INSTR(B$, " ") - 1)
  S$ = MID$(T$, INSTR(INSTR(T$, " ") + 1, T$, " ") + 1): S$ = LEFT$(S$, 1)
  C$ = " bsf ": IF S$ = "0" THEN C$ = " bcf "
  P$ = UCASE$(P$)

  'Allow use with vars > 8 bit
  IF VAL(B$) > 7 THEN
   B$ = STR$(VAL(B$) - 8)
   P$ = P$ + "_H"
  END IF

  PROG$(CD%) = C$ + P$ + "," + B$
  VLC% = VLC% + 1: VARLIST$(VLC%) = P$
  FoundCount% = FoundCount% + 1
 END IF
BADSET:
NEXT CD%
END SUB

SUB CompileWait

'Time Intervals: us, 10us, ms, 10ms, s, m, h

FoundCount% = 0
USDC% = 0

PD% = 0
lbCompileWaitLoop:
 PD% = PD% + 1
 T$ = UCASE$(PROG$(PD%))
 
 IF LEFT$(T$, 5) = "WAIT " THEN
    
  Origin$ = ""
  IF INSTR(T$, ";?F") <> 0 THEN 
   Origin$ = MID$(T$, INSTR(T$, ";?F")) 
   T$ = RTRIM$(LEFT$(T$, INSTR(T$, ";?F") - 1))
  END IF

  V$ = MID$(T$, 6)
  IF LEFT$(UCASE$(V$), 6) = "WHILE " OR LEFT$(UCASE$(V$), 6) = "UNTIL " THEN
   U$ = LCASE$(MID$(V$, INSTR(V$, " ") + 1))
   V$ = UCASE$(LEFT$(V$, INSTR(V$, " ") - 1))
   GOTO WaitDataFound
  END IF
  IF LEFT$(UCASE$(V$), 6) <> "WHILE " AND LEFT$(UCASE$(V$), 6) <> "UNTIL " THEN
   V$ = LTRIM$(RTRIM$(V$))
   FOR UP% = LEN(V$) TO 1 STEP -1
    IF MID$(V$, UP%, 1) = " " THEN
     U$ = LCASE$(LTRIM$(RTRIM$(MID$(V$, UP% + 1))))
     V$ = LTRIM$(RTRIM$(LEFT$(V$, UP% - 1)))
     EXIT FOR
    END IF
   NEXT UP%
  END IF

WaitDataFound:

  'Time-based wait
  IF V$ <> "UNTIL" AND V$ <> "WHILE" THEN
   IF INSTR(U$, "second") <> 0 THEN Replace U$, "second", "s"
   IF U$ = "us" OR U$ = "usec" THEN U$ = "Delay_US"
   IF U$ = "10us" OR U$ = "10usec" THEN U$ = "Delay_10US"
   IF U$ = "ms" OR U$ = "msec" THEN U$ = "Delay_MS"
   IF U$ = "10ms" OR U$ = "10msec" THEN U$ = "Delay_10MS"
   IF U$ = "s" OR U$ = "sec" THEN U$ = "Delay_S"
   IF U$ = "m" OR U$ = "min" THEN U$ = "Delay_M"
   IF U$ = "h" OR U$ = "hour" THEN U$ = "Delay_H"
   IF INSTR(U$, "Delay_") = 0 THEN ERC% = ERC% + 1: gcERR$(ERC%) = Origin$ + Message$("NoDelayUnits")

   T$ = U$: Replace T$, "Delay_", ""
   
   IF LTRIM$(RTRIM$(V$)) = "0" OR VAL(V$) < 0 THEN
    DelLine PD%: PD% = PD% - 1
    GOTO EndWaitCompile
   END IF

   'Compile longer delays as subs
   IF U$ <> "Delay_US" OR ChipMHz% >= 20 OR Not IsConst(V$) THEN
    PROG$(PD%) = "SysWaitTemp" + T$ + "=" + V$
    AddLine " call " + U$, PD% + 1
    
    'Generate error when using US delay on slow chips
    IF U$ = "Delay_US" AND ChipMHz% < 20 THEN
     ERC% = ERC% + 1
     gcERR$(ERC%) = Origin$ + Message$("InvalidDelayLength")
    END IF
    
   END IF
   
   'Compile microsecond delays as inline for slow chips
   IF U$ = "Delay_US" AND ChipMHz% < 20 And IsConst(V$) THEN
    T% = 0
    VLC% = VLC% + 1
    VARLIST$(VLC%) = "DELAYTEMP"
    VarType$(VLC%) = ""
    USDC% = USDC% + 1
    
    Cycles = MakeDec(V$)*ChipMHz%/4
    WasteMoreTime:
     
    If Cycles >= 5 Then
     DS% = (Cycles-2)\3
     IF DS% > 255 THEN DS% = 255
     Cycles = Cycles - (2 + DS% * 3)
     T% = T% + 1: TempData$(T%) = " movlw " + STR$(DS%)
     T% = T% + 1: TempData$(T%) = " movwf DELAYTEMP"
     T% = T% + 1: TempData$(T%) = "DelayUS" + STR$(USDC%)
     T% = T% + 1: TempData$(T%) = " decfsz DELAYTEMP,F"
     T% = T% + 1: TempData$(T%) = " goto DelayUS" + STR$(USDC%)
    END IF   
    If Cycles >= 2 AND ChipFamily% <> 16 THEN
     T% = T% + 1: TempData$(T%) = " goto $+1"
     Cycles = Cycles - 2
     GOTO WasteMoreTime
    END IF
    If Cycles >= 1 THEN
     T% = T% + 1: TempData$(T%) = " nop"
     Cycles = Cycles - 1
     GOTO WasteMoreTime
    END IF   
        
    PROG$(PD%) = TempData$(1)
    FOR AC% = 2 to T%
     AddLine TempData$(AC%), PD% + AC% - 1
    NEXT
    PD% = PD% + T% - 2
    
   DelayUSCompiled:
   END IF

   PD% = PD% + 1 'was + 2
   FoundCount% = FoundCount% + 1
  END IF

  'Condition-based Wait
  IF V$ = "WHILE" OR V$ = "UNTIL" THEN
   C$ = UCASE$(U$)
   OP$ = ""
   WSC% = WSC% + 1
   PROG$(PD%) = "SysWaitLoop" + STR$(WSC%)

   T$ = "TRUE": IF V$ = "UNTIL" THEN T$ = "FALSE"
   CompileConditions C$, T$
   COSC% = COSC% + 1: CheckTemp$(COSC%) = "goto SysWaitLoop" + STR$(WSC%)
   FOR WD% = 1 TO COSC%
    AddLine CheckTemp$(WD%), PD% + WD%
   NEXT WD%
   PD% = PD% + COSC%
   FoundCount% = FoundCount% + 1
  END IF

 END IF

EndWaitCompile:
IF PD% < APC% THEN GOTO lbCompileWaitLoop

END SUB

FUNCTION CountOccur (Source$, Search$)
 T% = 0

 IF LEFT$(Search$, 2) <> "';" THEN
  FOR CO% = 1 TO LEN(Source$) - (1 - LEN(Search$))
   IF MID$(UCASE$(Source$), CO%, LEN(Search$)) = UCASE$(Search$) THEN T% = T% + 1
  NEXT CO%
 END IF

 IF LEFT$(Search$, 2) = "';" THEN
  T$ = MID$(Search$, 3)
  FOR SC% = 1 TO LEN(T$)
   FOR CO% = 1 TO LEN(Source$)
    IF MID$(UCASE$(Source$), CO%, 1) = UCASE$(MID$(T$, SC%, 1)) THEN T% = T% + 1
   NEXT CO%
  NEXT SC%
 END IF

 CountOccur = T%
END FUNCTION

FUNCTION GetByte$ (DS$, BS%)

 'BS% is 0 for low, 1 for high

 'Check if variable
 IF NOT IsConst(DS$) THEN
  IF BS% = 0 THEN GetByte$ = DS$: EXIT FUNCTION
  IF BS% = 1 THEN GetByte$ = DS$ + "_H": EXIT FUNCTION
 END IF

 T! = 0

 'Convert to decimal
 IF DS$ = STR$(VAL(DS$)) THEN T! = VAL(DS$)

 IF INSTR(LCASE$(DS$), "b'") <> 0 THEN
  T$ = MID$(DS$, INSTR(LCASE$(DS$), "b'") + 2)
  Replace T$, "'", ""
  T! = 0
  FOR PD% = 1 TO LEN(T$)
   T! = T! + 2 ^ (LEN(T$) - PD%) * VAL(MID$(T$, PD%, 1))
  NEXT PD%
 END IF

 IF INSTR(LCASE$(DS$), "0x") <> 0 THEN
  T$ = DS$
  Replace T$, "0x", "&h"
  T! = VAL(T$)
 END IF

 IF BS% = 0 THEN GetByte$ = STR$(T! AND 255)
 IF BS% = 1 THEN GetByte$ = STR$((T! AND 65280) / 256)

END FUNCTION

SUB InitCompiler (C$)

'Replace - with / in parameters
'Allows better DOSBox compatibility
T$ = UCASE$(C$)
IF WholeINSTR(T$, "-O:") = 2 THEN Replace C$, "-O:", "/O:"
IF WholeINSTR(T$, "-A:") = 2 THEN Replace C$, "-A:", "/A:"
IF WholeINSTR(T$, "-D:") = 2 THEN Replace C$, "-D:", "/D:"
IF WholeINSTR(T$, "-V") = 2 THEN Replace C$, "-V", "/V"
IF WholeINSTR(T$, "-NC") = 2 THEN Replace C$, "-NC", "/NC"
IF WholeINSTR(T$, "-NP") = 2 THEN Replace C$, "-NP", "/NP"
IF WholeINSTR(T$, "-L") = 2 THEN Replace C$, "-L", "/L"
IF WholeINSTR(T$, "-P:") = 2 THEN Replace C$, "-P:", "/P:"

'Get output filename from COMMAND$
OFI$ = ""
IF INSTR(UCASE$(C$), "/O:") <> 0 THEN
 T$ = MID$(C$, INSTR(UCASE$(C$), "/O:"))
 IF INSTR(T$, " ") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, " ") - 1)
 Replace C$, T$, ""
 OFI$ = MID$(T$, 4)
END IF

'Get command to assemble program from COMMAND$
MakeASM$ = ""
IF INSTR(UCASE$(C$), "/A:") <> 0 THEN
 T$ = MID$(C$, INSTR(UCASE$(C$), "/A:"))
 IF INSTR(T$, " ") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, " ") - 1)
 Replace C$, T$, ""
 MakeASM$ = TRIM$(MID$(T$, 4))
END IF

SendToPIC$ = ""
IF INSTR(UCASE$(C$), "/P:") <> 0 THEN
 T$ = MID$(C$, INSTR(UCASE$(C$), "/P:"))
 IF INSTR(T$, " ") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, " ") - 1)
 Replace C$, T$, ""
 SendToPIC$ = ShortName$(MID$(T$, 4)) 
END IF

'Detect GCBASIC install directory
ID$ = CURDIR$
IF INSTR(UCASE$(C$), "/D:") <> 0 THEN
 T$ = MID$(C$, INSTR(UCASE$(C$), "/D:"))
 IF INSTR(T$, " ") <> 0 THEN T$ = LEFT$(T$, INSTR(T$, " ") - 1)
 Replace C$, T$, ""
 ID$ = ShortName$(MID$(T$, 4))
END IF

'Clear screen before compile?
IF WholeINSTR(UCASE$(C$), "/NC") <> 2 THEN CLS
IF WholeINSTR(UCASE$(C$), "/NC") = 2 THEN Replace C$, "/NC", ""

'Pause on errors?
PauseOnErr% = 0
IF WholeINSTR(UCASE$(C$), "/NP") <> 2 THEN PauseOnErr% = 1
IF WholeINSTR(UCASE$(C$), "/NP") = 2 THEN Replace C$, "/NP", ""

'Verbose Mode?
'VBS% = 0: IF INSTR(UCASE$(C$), "/V") <> 0 THEN VBS% = 1
'Replace C$, "/V", ""
VBS% = 0
IF WholeINSTR(UCASE$(C$), "/V") = 2 THEN VBS% = 1: Replace C$, "/V", ""

'Great Cow Graphical BASIC Mode?
GCGB% = 0
IF WholeINSTR(UCASE$(C$), "/GCGB") = 2 THEN GCGB% = 1: Replace C$, "/GCGB", ""

'Read message list
#IFDEF __FB_LINUX__
 M$ = ID$ + "/messages.dat"
#ELSE
 M$ = ID$ + "\messages.dat"
#ENDIF

IF DIR$(M$) = "" THEN
 PRINT "Cannot find " + M$ + "! Great Cow BASIC cannot operate"
 PRINT "without this file."
 PRINT
 PRINT "Press any key to continue"
 DO WHILE INKEY$ = "": SLEEP 10: LOOP
 END
END IF
OPEN M$ FOR INPUT AS #1
Block% = 0
DO WHILE NOT EOF(1)
 LINE INPUT #1, DS$
 DS$ = LTRIM$(RTRIM$(DS$))
 IF LEFT$(UCASE$(DS$), 6) = "BLOCK " THEN Block% = 1
 IF LEFT$(UCASE$(DS$), 9) = "END BLOCK" THEN Block% = 0
 IF LEFT$(DS$, 1) = "'" THEN DS$ = ""
 IF INSTR(DS$, "=") <> 0 THEN
  N$ = LEFT$(DS$, INSTR(DS$, "=") - 1)
  V$ = MID$(DS$, INSTR(DS$, "=") + 1)
  IF INSTR(V$, CHR$(34)) <> 0 THEN
   V$ = MID$(V$, INSTR(V$, CHR$(34)) + 1)
   V$ = LEFT$(V$, INSTR(V$, CHR$(34)) - 1)
  END IF
  MSGC% = MSGC% + 1
  Messages$(1, MSGC%) = LTRIM$(RTRIM$(UCASE$(N$)))
  Messages$(2, MSGC%) = V$
 END IF
LOOP
CLOSE #1

'Show Licence?
IF WholeINSTR(C$, "/L") = 2 THEN
 ShowBlock "License"
 END
END IF

FI$ = ShortName$(C$)
'DO WHILE INSTR(FI$, " ") <> 0: Replace FI$, " ", "": LOOP

'Message if no filename specified
IF FI$ = "" THEN
 ShowBlock "NoPrompt"
 T$ = INKEY$
 PRINT
 PRINT Message$("AnyKey")
 DO WHILE INKEY$ = "": SLEEP 10: LOOP
 END
END IF

'Decide name for output file if not specified
IF OFI$ = "" THEN
 T$ = MID$(FI$, INSTR(FI$, ".") + 1)
 OFI$ = FI$
 Replace OFI$, T$, "asm"
END IF

'Find directory of source file (used for relative include)
ProgDir$ = CURDIR$
IF INSTR(FI$, "\") <> 0 THEN
 FOR T% = LEN(FI$) TO 1 STEP -1
  IF MID$(FI$, T%, 1) = "\" THEN EXIT FOR
 NEXT T%
 ProgDir$ = LEFT$(FI$, T% - 1)
END IF
IF RIGHT$(ProgDir$, 1) = "\" THEN ProgDir$ = LEFT$(ProgDir$, LEN(ProgDir$) - 1)
IF RIGHT$(ProgDir$, 1) = "/" THEN ProgDir$ = LEFT$(ProgDir$, LEN(ProgDir$) - 1)

'Show version
IF GCGB% = 0 THEN
    PRINT "Great Cow BASIC (" + Version$ + ")"
    PRINT
END IF

'Start Compile
PRINT Message$("Compiling")

IF DIR$(FI$) = "" THEN
 PRINT
 PRINT Message$("NoFile")
 END
END IF

END SUB

FUNCTION IsCalc (DS$)

 IsCalc = 0
 IF INSTR(DS$, "+") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "-") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "*") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "/") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "AND") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "OR") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "NOT") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "XOR") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "&") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "|") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "!") <> 0 THEN IsCalc = -1
 IF INSTR(DS$, "#") <> 0 THEN IsCalc = -1

END FUNCTION

FUNCTION IsConst (DS$)

T$ = LTRIM$(RTRIM$(DS$))
IF LEFT$(T$, 1) = "-" THEN T$ = MID$(T$, 2)

IsConst = 0
IF T$ = STR$(VAL(T$)) THEN IsConst = -1
IF INSTR(LCASE$(T$), "b'") <> 0 THEN IsConst = -1
IF INSTR(LCASE$(T$), "0x") <> 0 THEN IsConst = -1

IF INSTR(T$, "+") <> 0 THEN IsConst = 0
IF INSTR(T$, "-") <> 0 THEN IsConst = 0
IF INSTR(T$, "*") <> 0 THEN IsConst = 0
IF INSTR(T$, "/") <> 0 THEN IsConst = 0
IF INSTR(T$, "&") <> 0 THEN IsConst = 0
IF INSTR(T$, "|") <> 0 THEN IsConst = 0
IF INSTR(T$, "!") <> 0 THEN IsConst = 0
IF INSTR(T$, "#") <> 0 THEN IsConst = 0


END FUNCTION

FUNCTION IsWord (T$)

IsWord = 0

'Word mode forced?
IF INSTR(UCASE$(T$), "[WORD]") <> 0 THEN IsWord = -1: EXIT FUNCTION

'Check for word var in calculation
FOR PD% = 1 TO VLC%
 IF WholeINSTR(UCASE$(T$), UCASE$(VARLIST$(PD%))) = 2 AND UCASE$(VarType$(PD%)) = "WORD" THEN IsWord = -1: EXIT FUNCTION
NEXT PD%

'Is a sub parameter that is a word being set?
DS$ = T$
IF INSTR(DS$, " call ") <> 0 THEN Replace DS$, " call ", ""
T% = LocationOfSub(DS$)
IF T% <> 0 THEN
 SP$ = TRIM$(SUBDATA$(T%, 1))
 SP$ = TRIM$(MID$(SP$, INSTR(SP$, " ") + 1))
 IF LEFT$(SP$, 1) <> ";" THEN
  FOR PD% = 1 TO VLC%
   IF WholeINSTR(UCASE$(SP$), UCASE$(VARLIST$(PD%))) = 2 AND UCASE$(VarType$(PD%)) = "WORD" THEN IsWord = -1: EXIT FUNCTION
  NEXT PD%
 END IF
END IF

END FUNCTION

FUNCTION MakeDec (T$)
 DS$ = UCASE$(T$)

 IF INSTR(DS$, "0X") <> 0 THEN
  DS$ = MID$(DS$, INSTR(DS$, "0X") + 2)
  MakeDec = VAL("&H" + DS$)
  EXIT FUNCTION
 END IF

 IF DS$ = STR$(VAL(DS$)) THEN
  MakeDec = VAL(DS$)
  EXIT FUNCTION
 END IF

 IF INSTR(DS$, "B'") <> 0 THEN
  DS$ = MID$(DS$, INSTR(DS$, "B'") + 2)
  DS$ = LEFT$(DS$, INSTR(DS$, "'") - 1)
  T% = 0
  FOR CB% = LEN(DS$) TO 1 STEP -1
   T% = T% + VAL(MID$(DS$, LEN(DS$) - CB% + 1, 1)) * 2 ^ (CB% - 1)
  NEXT CB%
  MakeDec = T%
 END IF

END FUNCTION

FUNCTION Message$ (DS$)

 'Find message
 DS$ = UCASE$(DS$)
 DS% = 0
 FOR T% = 1 TO MSGC%
  IF Messages$(1, T%) = DS$ THEN DS% = T%: EXIT FOR
 NEXT

 'Retrieve message
 IF DS% = 0 THEN Message$ = "Message " + DS$ + " not defined in messages.dat!": EXIT FUNCTION
 IF DS% <> 0 THEN M$ = Messages$(2, DS%)

 'Replace values
 T$ = UCASE$(M$)
 IF INSTR(T$, "%VERSION%") <> 0 THEN Replace M$, "%VERSION%", Version$
 IF INSTR(T$, "%FILENAME%") <> 0 THEN Replace M$, "%FILENAME%", FI$
 IF INSTR(T$, "%OUTPUTFILE%") <> 0 THEN Replace M$, "%OUTPUTFILE%", OFI$

 Message$ = M$

END FUNCTION

SUB Replace (VAR$, FIND$, REP$)

 VART$ = UCASE$(VAR$): FINDT$ = UCASE$(FIND$)
 IF INSTR(VART$, FINDT$) = 0 THEN VAR$ = VAR$ + REP$: EXIT SUB

 ND$ = LEFT$(VAR$, INSTR(VART$, FINDT$) - 1)
 ND$ = ND$ + REP$
 ND$ = ND$ + MID$(VAR$, INSTR(VART$, FINDT$) + LEN(FIND$))

 VAR$ = ND$
END SUB

SUB SCICONV (ST$)

DO WHILE INSTR(ST$, " ") <> 0: Replace ST$, " ", "": LOOP
ST$ = UCASE$(ST$)

IF INSTR(UCASE$(ST$), "E") <> 0 THEN
 T$ = "00000000000000000000"
 V$ = LEFT$(ST$, INSTR(ST$, "E") - 1)
 P$ = MID$(ST$, INSTR(ST$, "E") + 1)
 Replace V$, ".", ""
 P% = VAL(P$)
 S% = 1: IF INSTR(V$, "-") <> 0 THEN S% = -1: Replace V$, "-", ""
 IF SGN(P%) = -1 THEN ST$ = "0." + LEFT$(T$, P% * -1 - 1) + V$
 IF SGN(P%) = 1 THEN ST$ = V$ + LEFT$(T$, P%)
 IF S% = -1 THEN ST$ = "-" + ST$
END IF

IF INSTR(UCASE$(ST$), "D") <> 0 THEN
 T$ = "00000000000000000000"
 DV% = VAL(MID$(ST$, INSTR(ST$, "D") + 1))
 ST$ = LEFT$(ST$, INSTR(ST$, "D") - 1)
 DV% = DV% * SGN(DV%)
 IF DV% > 1 THEN DV% = DV% - 1
 DS$ = ST$
 Replace DS$, ".", ""
 Replace DS$, "-", ""
 Replace DS$, " ", ""
 M% = 0: IF INSTR(ST$, "-") <> 0 THEN M% = 1
 IF DV% > 7 THEN ST$ = ".0": EXIT SUB
 A% = 7 - DV%
 ST$ = "." + LEFT$(T$, DV%)
 ST$ = ST$ + LEFT$(DS$, A%)
 IF M% = 1 THEN ST$ = "-" + ST$
END IF

DO WHILE INSTR(ST$, " ") <> 0: Replace ST$, " ", "": LOOP

END SUB

FUNCTION ShortName$ (DS$)

'Get rid of "s
'DO WHILE INSTR(DS$, CHR$(34)) <> 0
' Replace DS$, CHR$(34), ""
'LOOP
IF INSTR(DS$, CHR$(34)) <> 0 THEN
 DS$ = MID$(DS$, INSTR(DS$, CHR$(34))+1)
 DS$ = LEFT$(DS$, INSTR(DS$, CHR$(34))-1)
END IF
DS$ = LTRIM$(RTRIM$(DS$))

'Linux filename conversion
#IFDEF __FB_LINUX__
 DO WHILE INSTR(DS$, "\") <> 0: Replace DS$, "\", "/": LOOP
#ENDIF

ShortName$ = DS$

END FUNCTION

SUB ShowBlock (DS$)

 DS$ = UCASE$(LTRIM$(RTRIM$(DS$)))

#IFDEF __FB_LINUX__
 OPEN ID$ + "/messages.dat" FOR INPUT AS #9
#ELSE
 OPEN ID$ + "\messages.dat" FOR INPUT AS #9
#ENDIF
 DO WHILE NOT EOF(9)
  LINE INPUT #9, L$
  L$ = UCASE$(LTRIM$(RTRIM$(L$)))
  IF L$ = "BLOCK " + DS$ THEN GOTO FoundBlock
 LOOP
 CLOSE #9
 EXIT SUB

FoundBlock:
 LINE INPUT #9, DS$
 IF UCASE$(LTRIM$(RTRIM$(DS$))) = "END BLOCK" THEN CLOSE #9: EXIT SUB

  IF INSTR(UCASE$(DS$), "%VERSION%") <> 0 THEN Replace DS$, "%VERSION%", Version$
  IF INSTR(UCASE$(DS$), "%FILENAME%") <> 0 THEN Replace DS$, "%FILENAME%", FI$
  IF INSTR(UCASE$(DS$), "%OUTPUTFILE%") <> 0 THEN Replace DS$, "%OUTPUTFILE%", OFI$

  PRINT DS$
 IF NOT EOF(9) THEN GOTO FoundBlock
 CLOSE #9

END SUB

FUNCTION TypeOfVar$ (VarName$)

TypeOfVar$ = "BYTE"

VarFound% = 0
T$ = TRIM$(UCASE$(VarName$))
FOR PD% = 1 TO VLC%
 IF TRIM$(UCASE$(VARLIST$(PD%))) = T$ THEN VarFound% = PD%: EXIT FOR
NEXT PD%

IF VarFound% <> 0 THEN
 T$ = VarType$(VarFound%)
 IF T$ <> "" THEN TypeOfVar$ = UCASE$(T$)
END IF

END FUNCTION

FUNCTION WholeINSTR (DS$, N$)
 IF INSTR(UCASE$(DS$), UCASE$(N$)) = 0 THEN WholeINSTR = 0: EXIT FUNCTION

 T% = 0
 IF LEN(DS$) = LEN(N$) THEN WholeINSTR = 2: EXIT FUNCTION

 IF INSTR(UCASE$(DS$), UCASE$(N$)) = 1 THEN T% = 1
 IF T% = 0 THEN
  T$ = MID$(DS$, INSTR(DS$, N$) - 1, 1)
  IF T$ = " " THEN T% = 1
  IF T$ = "(" THEN T% = 1
  IF T$ = ")" THEN T% = 1
  IF T$ = "," THEN T% = 1
  IF T$ = "." THEN T% = 1
  IF T$ = ":" THEN T% = 1
  IF T$ = ";" THEN T% = 1
  IF T$ = "+" THEN T% = 1
  IF T$ = "-" THEN T% = 1
  IF T$ = "*" THEN T% = 1
  IF T$ = "/" THEN T% = 1
  IF T$ = "=" THEN T% = 1
  IF T$ = "!" THEN T% = 1
  IF T$ = "<" THEN T% = 1
  IF T$ = ">" THEN T% = 1
  IF T$ = "{" THEN T% = 1
  IF T$ = "}" THEN T% = 1
  IF T$ = "~" THEN T% = 1
  IF T$ = "&" THEN T% = 1
  IF T$ = "|" THEN T% = 1
  IF T$ = "#" THEN T% = 1
  T2$ = T$
 END IF

 IF INSTR(UCASE$(DS$), UCASE$(N$)) + LEN(N$) - 1 = LEN(DS$) THEN T% = T% + 1
 IF T% < 2 THEN
  T$ = MID$(DS$, INSTR(UCASE$(DS$), UCASE$(N$)) + LEN(N$), 1)
  IF T$ = " " THEN T% = T% + 1
  IF T$ = "(" THEN T% = T% + 1
  IF T$ = ")" THEN T% = T% + 1
  IF T$ = "," THEN T% = T% + 1
  IF T$ = "." THEN T% = T% + 1
  IF T$ = ":" THEN T% = T% + 1
  IF T$ = ";" THEN T% = T% + 1
  IF T$ = "+" THEN T% = T% + 1
  IF T$ = "-" THEN T% = T% + 1
  IF T$ = "*" THEN T% = T% + 1
  IF T$ = "/" THEN T% = T% + 1
  IF T$ = "=" THEN T% = T% + 1
  IF T$ = "!" THEN T% = T% + 1
  IF T$ = "<" THEN T% = T% + 1
  IF T$ = ">" THEN T% = T% + 1
  IF T$ = "{" THEN T% = T% + 1
  IF T$ = "}" THEN T% = T% + 1
  IF T$ = "~" THEN T% = T% + 1
  IF T$ = "&" THEN T% = T% + 1
  IF T$ = "|" THEN T% = T% + 1
  IF T$ = "#" THEN T% = T% + 1

 END IF

WholeINSTR = T%

END FUNCTION
